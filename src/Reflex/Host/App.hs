{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Module supporting the implementation of frameworks. You should import this if you
-- want to build your own framework.
module Reflex.Host.App
  ( hostApp
  , newEventWithConstructor, newExternalEvent
  , performEventAndTrigger_, performEvent_, performEvent
  , switchAppHost, performAppHost, dynAppHost, holdAppHost, holdKeyAppHost
  , getPostBuild, performPostBuild, performEventAsync
  , MonadAppHost(..), AppHost()
  , AppInfo(..), infoPerform, infoQuit, infoFire, switchAppInfo
  ) where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.Trans
import Data.Dependent.Sum
import Data.IORef
import Data.Map.Strict (Map)
import Data.Maybe
import Data.Monoid
import Reflex.Class
import Reflex.Dynamic
import Reflex.Host.App.Internal
import Reflex.Host.Class

import qualified Data.Foldable as F
import qualified Data.Traversable as T

import Prelude -- Silence AMP warnings

-- | Create a new event and return a function that can be used to construct an event
-- trigger with an associated value. Note that this by itself will not fire the event.
-- To fire the event, you still need to use either 'performPostBuild_' or 'getFireAsync'
-- which can fire these event triggers with an associated value.
--
-- Note that in some cases (such as when there are no listeners), the returned function
-- does return 'Nothing' instead of an event trigger. This does not mean that it will
-- neccessarily return Nothing on the next call too though.
newEventWithConstructor
  :: (MonadAppHost t m, Applicative f) => m (Event t a, a -> IO (Maybe (DSum (EventTrigger t) f)))
newEventWithConstructor = do
  ref <- liftIO $ newIORef Nothing
  event <- newEventWithTrigger (\h -> writeIORef ref Nothing <$ writeIORef ref (Just h))
  return (event, \a -> fmap (:=> pure a) <$> liftIO (readIORef ref))

-- | Create a new event from an external event source. The returned function can be used
-- to fire the event.
newExternalEvent :: MonadAppHost t m => m (Event t a, a -> IO Bool)
newExternalEvent = do
  asyncFire <- getFireAsync
  (event, construct) <- newEventWithConstructor
  return (event, fmap isJust . T.traverse (asyncFire . pure) <=< construct)

-- | Run a monadic action after each frame in which the given event fires
-- and possibly fire other events as a result (fired in a new frame, together with all
-- other events to fire from this frame).
--
-- The events are fired synchronously, so they are processed before any other
-- external events.
performEventAndTrigger_ :: MonadAppHost t m => Event t (AppPerformAction t) -> m ()
performEventAndTrigger_ = performPostBuild_ . pure . infoPerform . pure

-- | Run a monadic action after each frame in which the event fires.
performEvent_ :: MonadAppHost t m => Event t (HostFrame t ()) -> m ()
performEvent_ = performEventAndTrigger_ . fmap (mempty <$)

-- | Run a monadic action after each frame in which the event fires, and return the result
-- in a new event which is fired immediately following the frame in which the original
-- event fired.
performEvent :: MonadAppHost t m => Event t (HostFrame t a) -> m (Event t a)
performEvent event = do
  (result, construct) <- newEventWithConstructor
  performEventAndTrigger_ $ (fmap (F.foldMap pure) . liftIO . construct =<<) <$> event
  return result

-- | Run some IO asynchronously in another thread starting after the frame in which the
-- input event fires and fire an event with the result of the IO action after it
-- completed.
performEventAsync :: MonadAppHost t m => Event t (IO a) -> m (Event t a)
performEventAsync event = do
  (result, fire) <- newExternalEvent
  performEvent_ $ void . liftIO . forkIO .  (void . fire =<<) <$> event
  return result

-- | Run a HostFrame action after application setup is complete and fire an event with the
-- result.
--
-- Typical use is sampling from Dynamics/Behaviors and providing the result in an Event
-- more convenient to use.
performPostBuild ::  (MonadAppHost t m) => HostFrame t a -> m (Event t a)
performPostBuild action = do
  (event, construct) <- newEventWithConstructor
  performPostBuild_ $ do
    a <- action
    pure $ infoFire $ liftIO (F.foldMap pure <$> construct a)
  return event

-- | Provide an event which is triggered directly after the initial setup of the
-- application is completed.
getPostBuild :: (MonadAppHost t m) =>  m (Event t ())
getPostBuild = performPostBuild (return ())

-- | Run an action in a 'MonadAppHost' monad, but do not register the 'AppInfo' for this
-- action nor its postBuild actions.
-- Instead, the 'AppInfo' for this action is collected and returned.
--
-- For example, all 'performEvent_' calls inside the passed action will not actually be
-- performed, as long as the returned 'AppInfo' is not registered manually.
runAppHost :: MonadAppHost t m => m a -> m (HostFrame t (AppInfo t), a)
runAppHost action = liftHostFrame . ($ action) =<< getRunAppHost

-- | Switch to a different host action after an event fires. Only the 'AppInfo' of the
-- currently active application is registered. For example, 'performEvent' calls are only
-- executed for the currently active application. As soon as it is switched out and
-- replaced by a different application, they are no longer executed.
--
-- The first argument specifies the postBuild action that is used initially, before the
-- event fires the first time.
--
-- Whenever a switch to a new host action happens, the returned event is fired in the
-- next frame with the result of running it.
switchAppHost :: MonadAppHost t m => HostFrame t (AppInfo t) -> Event t (m a) -> m (Event t a)
switchAppHost initial event = do
  run <- getRunAppHost
  let runWithPost = run >=> \(post, a) -> (,a) <$> post
  (infoEvent, valueEvent) <- fmap splitE . performEvent $ runWithPost <$> event
  performPostBuild_ $ flip switchAppInfo infoEvent =<< initial
  return valueEvent

-- | Like 'switchAppHost', but without an initial action.
performAppHost :: MonadAppHost t m => Event t (m a) -> m (Event t a)
performAppHost = switchAppHost (pure mempty)

-- | Like 'switchAppHost', but taking the initial action from a 'Dynamic'. The returned
-- event is also fired once in the frame directly after the call to this function, firing
-- with the result of the initial action.
dynAppHost :: MonadAppHost t m => Dynamic t (m a) -> m (Event t a)
dynAppHost dyn = do
  run <- getRunAppHost
  (initialEvent, initialConstruct) <- newEventWithConstructor
  updatedEvent <- flip switchAppHost (updated dyn) $ do
    (minfo, r) <- sample (current dyn) >>= run
    info <- minfo
    trigger <- liftIO $ initialConstruct r
    pure $ info <> F.foldMap (infoFire . pure . pure) trigger
  pure $ leftmost [updatedEvent, initialEvent]

-- | Like 'switchAppHost', but taking the initial postBuild action from another host
-- action.
holdAppHost :: MonadAppHost t m => m a -> Event t (m a) -> m (Dynamic t a)
holdAppHost mInit mChanged = do
  (postActions, aInit) <- runAppHost mInit
  aChanged <- switchAppHost postActions mChanged
  holdDyn aInit aChanged

-- | Helper to execute host action in one step (instead of usual 3 steps)
getRunWithPost :: MonadAppHost t m => m (m a -> HostFrame t (AppInfo t, a))
getRunWithPost = do
  run <- getRunAppHost
  return $ run >=> \(post, a) -> (,a) <$> post

-- | Mix in host action after an event fires. Only the 'AppInfo' of the
-- currently active key-value pairs of application is registered. For example, 'performEvent'
-- calls are only executed for the currently active application. As soon as it is
-- switched out and replaced by a different application, they are no longer executed.
--
-- The first argument specifies the application that is used initially, before the
-- event fires the first time.
--
-- Whenever a switch to a new host action happens, the returned dynamic is updated
-- with current active key-value pairs.
--
-- The function is helpful for implementation of dynamic collections where addition
-- or deletion of elements don't affect other parts of the collection. Each item
-- of the key-value pair is constructed once and hence post build event is fired
-- only once (or when you explicitly replaces an element with new one).
holdKeyAppHost :: forall t m a k . (MonadAppHost t m, Ord k)
  => Map k (m a) -- ^ Initial set of components
  -> Event t (Map k (Maybe (m a))) -- ^ 'Nothing' values indicates that the key should be deleted, 'Just' indicates that the key should be added/replaced
  -> m (Dynamic t (Map k a)) -- ^ Reflects current state of the collection.
holdKeyAppHost initialMap event = do
  -- describe how to execute partial updates
  run <- getRunAppHost
  postRun <- getRunWithPost
  let
    executedEvent :: Event t (Map k (Maybe (HostFrame t (AppInfo t, a))))
    executedEvent = fmap (fmap postRun) <$> event

    liftedEvent :: Event t (HostFrame t (Map k (Maybe (AppInfo t, a))))
    liftedEvent = sequence . fmap sequence <$> executedEvent

  -- execute partial updates to split values and app infos
  updMap :: Event t (Map k (Maybe (AppInfo t, a))) <- performEvent liftedEvent
  let
    updInfoMap :: Event t (Map k (Maybe (AppInfo t)))
    updInfoMap = fmap (fmap fst) <$> updMap

    updValueMap :: Event t (Map k (Maybe a))
    updValueMap = fmap (fmap snd) <$> updMap

  -- construct initial values
  initial :: Map k (HostFrame t (AppInfo t), a) <- liftHostFrame . sequence $ run <$> initialMap
  let
    initialInfo :: HostFrame t (Map k (AppInfo t))
    initialInfo = sequence . fmap fst $ initial

    initialValues :: Map k a
    initialValues = fmap snd initial

  -- register partial updates
  performPostBuild_ $ flip switchKeyAppInfo updInfoMap =<< initialInfo

  -- collect computed outputs
  foldDyn updateMap initialValues updValueMap
