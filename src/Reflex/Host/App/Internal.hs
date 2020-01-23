{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
-- | Module exposing the internal implementation of the host monad.
-- There is no guarrante about stability of this module.
-- If possible, use 'Reflex.Host.App' instead.
module Reflex.Host.App.Internal where

import Control.Applicative
import Control.Concurrent
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.RSS
import Control.Monad.Writer.Class
import Data.Dependent.Sum
import Data.Functor.Identity
import Data.Map.Strict            (Map)
import Data.Maybe
import Data.Monoid                hiding (Ap (..))
import Data.Semigroup.Applicative
import Prelude
import Reflex.Class               hiding (constant)
import Reflex.Dynamic
import Reflex.Host.Class

import qualified Data.DList       as DL
import qualified Data.Foldable    as F
import qualified Data.Map.Strict  as M
import qualified Data.Traversable as T
--------------------------------------------------------------------------------

-- | AppInputs are inputs to the application triggered by the external UI.
--   These are stored in a channel to be processed by the application.
type AppInputs t = [DSum (EventTrigger t) Identity]

-- | This is the environment in which the app host monad runs.
data AppEnv t = AppEnv
  { -- | This is the channel to which external events should push their triggers.
    --
    -- Because this is a channel, there is no guarrante that the event that was pushed
    -- is fired directly in the next frame, as there can already be other events waiting
    -- which will be fired first.
    envEventChan :: Chan (AppInputs t)
  }

-- | An action that is run after a frame. It may return event triggers to fire events.
-- For more information about this type, see the field 'eventsToPerform' of 'AppInfo'.
type AppPerformAction t = HostFrame t (DL.DList (DSum (EventTrigger t) Identity))

-- | Information required to set up the application. This also contains all reflex events
-- that the application wants to perform. An 'AppInfo' is called *registered* or *active*
-- if it is currently in use, meaning that the specified events are actually performed.
--
-- The 'AppInfo' represents the output side of a reflex FRP framework. It is used to
-- perform IO actions in response to FRP events, for example. This is called the *effect*
-- of the 'AppInfo'.
data AppInfo t = AppInfo
  { -- | Events that are performed after each frame.
    --
    -- Each event in this list will be checked after a frame. If it is firing with some
    -- 'AppPerformAction', then this action will be executed. The events will be fired
    -- before any other, currently waiting external events contained in the
    -- `envEventChan` are processed.
    --
    -- The event occurrences returned by each 'AppPerformAction' are collected in a list.
    -- If the list is non-empty, then a new frame will be created where all the collected
    -- occurrences are fired at the same time. This process continues until no events
    -- should be fired anymore. Only after this process has finished, external events
    -- contained in the `envEventChan` are processed again.
    --
    -- A common place where you need this is when you want to fire some event in response
    -- to another event, but you need to perform a monadic action such as IO to compute
    -- the value of the response. Using this field, you can perform the monadic action
    -- and then return a trigger to fire the event. This guarrantes that the event is
    -- fired immediately after the frame has finished, even if other, external events
    -- are waiting.
    eventsToPerform :: DL.DList (Event t (AppPerformAction t))

    -- | Events that, when fired, quit the application.
  , eventsToQuit    :: DL.DList (Event t ())

    -- | Delayed event triggers that will be fired immediately after the initial
    -- application setup has completed, before any external events are processed.
  , triggersToFire  :: Ap (HostFrame t) (DL.DList (DSum (EventTrigger t) Identity))
  }

-- | 'AppInfo' is a monoid. 'mappend' just merges the effects of both app infos.
-- 'mempty' is an 'AppInfo' that has no effect at all when registered.
instance Applicative (HostFrame t) => Monoid (AppInfo t) where
  mempty = AppInfo mempty mempty mempty
  mappend (AppInfo a b c) (AppInfo a' b' c') =
    AppInfo (mappend a a') (mappend b b') (mappend c c')

instance Applicative (HostFrame t) => Semigroup (AppInfo t) where
  (<>) (AppInfo a b c) (AppInfo a' b' c') =
    AppInfo ((<>) a a') ((<>) b b') ((<>) c c')

-- | Produce an 'AppInfo' which only contains 'eventsToPerform'. This is useful in a
-- monoid chain, like @infoToPerform toPerform <> infoToQuit toQuit@.
infoPerform :: Applicative (HostFrame t)
            => DL.DList (Event t (AppPerformAction t)) -> AppInfo t
infoPerform x = mempty { eventsToPerform = x }

-- | Produce an 'AppInfo' which only contains 'eventsToQuit'.
infoQuit :: Applicative (HostFrame t) => DL.DList (Event t ()) -> AppInfo t
infoQuit x = mempty { eventsToQuit = x }

-- | Produce an 'AppInfo' which only contains 'triggersToFire'.
infoFire :: Applicative (HostFrame t)
           => HostFrame t (DL.DList (DSum (EventTrigger t) Identity)) -> AppInfo t
infoFire x = mempty { triggersToFire = Ap x }

-- | Extract the 'eventsToPerform' and 'eventsToQuit' and merge each into a single event.
appInfoEvents :: (Reflex t, Applicative (HostFrame t))
              => AppInfo t -> (Event t (AppPerformAction t), Event t ())
appInfoEvents AppInfo{..} =
  ( mergeWith (liftA2 (<>)) $ DL.toList eventsToPerform
  , leftmost $ DL.toList eventsToQuit
  )

-- | Switch to a different 'AppInfo' whenever an 'Event' fires. Only the events of the
-- currently active application are performed.
--
-- This low-level primitive is used for implementing higher-level functions such as
-- 'switchAppHost', 'performAppHost' or 'dynAppHost'.
switchAppInfo :: (Reflex t, MonadHold t m, Applicative (HostFrame t))
              => AppInfo t -> Event t (AppInfo t) -> m (AppInfo t)
switchAppInfo initialInfo updatedInfo = do
  toPerform <- switch <$> hold initialToPerform updatedToPerform
  toQuit    <- switch <$> hold initialToQuit updatedToQuit
  pure AppInfo
    { eventsToPerform = pure toPerform <> pure (getAp . triggersToFire <$> updatedInfo)
    , eventsToQuit = pure toQuit
    , triggersToFire = triggersToFire initialInfo
    }
 where
  (updatedToPerform, updatedToQuit) = splitE $ fmap appInfoEvents updatedInfo
  (initialToPerform, initialToQuit) = appInfoEvents initialInfo

-- | Switch to a different 'AppInfo' whenever an 'Event' fires. Only the events of the
-- currently active application are performed. Unlike 'switchAppInfo' the primitive
-- allows partial update of FRP network, new added pieces of FRP network don't
-- force of recreation of other pieces under different keys. Post build event of each
-- key-value pair is called only once (or when explicitly recreated).
--
-- This low-level primitive is used for implementing higher-level functions such as
-- 'switchKeyAppHost'. Also this is key primitive of creation of dynamic collections of
-- components/widgets.
switchKeyAppInfo :: forall t m k .
    (Reflex t, MonadHold t m, MonadFix m, Applicative (HostFrame t), Ord k)
  => Map k (AppInfo t)                   -- ^ Initial FRP application
  -> Event t (Map k (Maybe (AppInfo t))) -- ^ Updates, 'Nothing' deletes specified key and 'Just' adds/overwrite given key
  -> m (AppInfo t)                       -- ^ Collected application info
switchKeyAppInfo initialMap updatedMap = do
  -- calculate eventsToPerform events
  let initialPerforms :: Map k (Event t (AppPerformAction t))
      initialPerforms = fst <$> initialEvents

      performsEvent :: Event t (Map k (Maybe (Event t (AppPerformAction t))))
      performsEvent = fmap (fmap fst) <$> updateEvents

  toPerformMap <- switch . current . fmap mergeMap <$> foldDyn updateMap initialPerforms performsEvent
  let toPerform = getAp . F.foldMap Ap <$> toPerformMap

  -- calculate eventsToQuit
  let initialToQuit :: Map k (Event t ())
      initialToQuit = snd <$> initialEvents

      toQuitEvent :: Event t (Map k (Maybe (Event t ())))
      toQuitEvent = fmap (fmap snd) <$> updateEvents

  toQuitMap <- switch . current . fmap mergeMap <$> foldDyn updateMap initialToQuit toQuitEvent
  let toQuit = F.foldMap id <$> toQuitMap

  pure AppInfo {
      eventsToPerform = pure toPerform <> pure updatedTriggers
    , eventsToQuit    = pure toQuit
    , triggersToFire  = F.foldMap id initialTriggers
    }
 where
  initialEvents = fmap appInfoEvents initialMap
  updateEvents = fmap (fmap appInfoEvents) <$> updatedMap
  initialTriggers = fmap triggersToFire initialMap
  updatedTriggers = getAp . F.foldMap triggersToFire . M.mapMaybe id <$> updatedMap

-- | Helper to merge update map with current state
updateMap :: Ord k => Map k (Maybe a) -> Map k a -> Map k a
updateMap updMap curMap = M.foldlWithKey' go curMap updMap
 where
  go m k Nothing  = M.delete k m
  go m k (Just v) = M.insert k v m
--------------------------------------------------------------------------------

-- | An implementation of the 'MonadAppHost' typeclass. You should not need to use this
-- type directly. Instead, use the methods provided by the 'MonadAppHost' typeclass and
-- then run your application using 'hostApp' to choose this implementation.
newtype AppHost t a = AppHost
  { unAppHost :: RSST (AppEnv t) (Ap (HostFrame t) (AppInfo t)) () (HostFrame t) a
  }
deriving instance ReflexHost t => Functor (AppHost t)
deriving instance ReflexHost t => Applicative (AppHost t)
deriving instance ReflexHost t => Monad (AppHost t)

-- | 'AppHost' supports hold
instance ReflexHost t => MonadHold t (AppHost t) where
  hold            a b = AppHost $ lift $ hold a b
  holdDyn         a b = AppHost $ lift $ holdDyn a b
  holdIncremental a b = AppHost $ lift $ holdIncremental a b
  buildDynamic    a b = AppHost $ lift $ buildDynamic a b
  headE           a   = AppHost $ lift $ headE a

-- | 'AppHost' supports sample
instance ReflexHost t => MonadSample t (AppHost t) where
  sample = AppHost . lift . sample

deriving instance (MonadIO (HostFrame t), ReflexHost t) => MonadIO (AppHost t)
deriving instance ReflexHost t => MonadFix (AppHost t)

-- | You can subscribe to events in 'AppHost'
instance ReflexHost t => MonadSubscribeEvent t (AppHost t) where
  subscribeEvent = AppHost . lift . subscribeEvent

-- | You can create new events in 'AppHost'
instance ReflexHost t => MonadReflexCreateTrigger t (AppHost t) where
  newEventWithTrigger = AppHost . lift . newEventWithTrigger
  newFanEventWithTrigger trigger = AppHost . lift $ newFanEventWithTrigger trigger

-- | Run the application host monad in a reflex host frame and return the produced
-- application info.
execAppHostFrame :: ReflexHost t => AppEnv t -> AppHost t () -> HostFrame t (AppInfo t)
execAppHostFrame env (AppHost m) = do
  ((), Ap minfo) <- execRSST m env ()
  minfo

-- | Run an application. The argument is an action in the application host monad,
-- where events can be set up (for example by using 'newExternalEvent').
--
-- This function will block until the application exits (when one of the 'eventsToQuit'
-- fires).
hostApp :: (MonadIO m, MonadReflexHost t m) => AppHost t () -> m ()
hostApp app = initHostApp app >>= F.mapM_ runStep where
  runStep (chan, step) = do
    nextInput <- liftIO (readChan chan)
    step nextInput >>= flip when (runStep (chan, step))

-- | Initialize the application using a 'AppHost' monad. This function enables use
-- of use an external control loop. It returns a step function to step the application
-- based on external inputs received through the channel.
-- The step function returns False when one of the 'eventsToQuit' is fired.
initHostApp :: (ReflexHost t, MonadIO m, MonadReflexHost t m)
            => AppHost t () -> m (Maybe (Chan (AppInputs t), AppInputs t -> m Bool))
initHostApp app = do
  chan <- liftIO newChan
  AppInfo{..} <- runHostFrame $ execAppHostFrame (AppEnv chan) app
  nextActionEvent <- subscribeEvent $ mergeWith (liftA2 (<>)) $ DL.toList eventsToPerform
  quitEvent <- subscribeEvent $ mergeWith mappend $ DL.toList eventsToQuit

  let
    go [] = return ()
    go triggers = do
      (nextAction, continue) <- lift $ fireEventsAndRead triggers $
        (,) <$> eventValue nextActionEvent <*> fmap isNothing (readEvent quitEvent)
      guard continue
      maybe (return mempty) (lift . runHostFrame) nextAction >>= go . DL.toList

    eventValue :: MonadReadEvent t m => EventHandle t a -> m (Maybe a)
    eventValue = readEvent >=> T.sequenceA

  runMaybeT $ do
    go =<< lift (runHostFrame (DL.toList <$> getAp triggersToFire))
    return (chan, fmap isJust . runMaybeT . go)
--------------------------------------------------------------------------------

-- | Class providing common functionality for implementing reflex frameworks.
--
-- The host monad is used to setup events from external sources (such as user input) and
-- execute actions in response to events (such as performing some IO). This class contains
-- the primitives required for such a monad, so that higher-level functions can be
-- implemented generically. An implementation is the 'AppHost' monad.
--
-- Much of the functionality of this class is also provided by its superclasses.
class (ReflexHost t, MonadSample t m, MonadHold t m, MonadReflexCreateTrigger t m,
       MonadIO m, MonadIO (HostFrame t), MonadFix m, MonadFix (HostFrame t))
      => MonadAppHost t m | m -> t where
  -- | Primitive function to create events from external sources.
  --
  -- In reflex, when you create an event (using 'newEventWithTrigger' for example),
  -- you get passed an 'EventTrigger t'. This action returns a function which, given
  -- a trigger and a value for an event, can fire the event. It takes a list of triggers
  -- with values, so you can also use it to fire multiple events in parallel.
  --
  -- Note that the events fired by this function are fired asynchronously. In particular,
  -- if a lot of events are fired, then it can happen that the event queue already
  -- contains other events. In that case, those events will be fired first.
  getFireAsync :: m ([DSum (EventTrigger t) Identity] -> IO ())

  -- | Get a function to run the host monad. Useful for implementing dynamic switching.
  --
  -- Running the host monad performs 3 steps:
  --
  -- 1. First, the events and behaviors (using hold) are created. This step does not read
  --    the value of any behavior, since that breaks MonadFix in some cases.
  -- 2. After all events and behaviors have been created, the initial value of behaviors
  --    can now be read (using for example 'sample')
  -- 3. This information is then used to create an 'AppInfo' which contains all the
  --    information about the actions to perform in response to the FRP events.
  --
  -- This is why the type of the @run@ function returned by this action is
  -- @m a -> HostFrame t (HostFrame t (AppInfo t), a)@.
  -- Executing outermost @HostFrame t@ will only perform step 1. The inner layer will
  -- then perform step 2, and the returned 'AppInfo' represents step 3.
  getRunAppHost :: m (m a -> HostFrame t (HostFrame t (AppInfo t), a))

  -- | Run an action after all other actions have been run and add information about the
  -- application. After the host monad's actions have been executed, actions registered
  -- with this function will be run and the returned 'AppInfo's will be merged to get the
  -- final 'AppInfo'.
  --
  -- One use case for this function is to sample the initial values of some behaviors.
  -- This cannot be done directly in the host monad, since that would break MonadFix in
  -- some cases, since it is not lazy enough. Using this function, the sampling can
  -- instead be done after the host monad has finished, so the behavior is not forced too
  -- early.
  performPostBuild_ :: HostFrame t (AppInfo t) -> m ()

  -- | Directly run a HostFrame action in the host app monad.
  liftHostFrame :: HostFrame t a -> m a

-- | 'AppHost' is an implementation of 'MonadAppHost'.
instance (ReflexHost t, MonadIO (HostFrame t)) => MonadAppHost t (AppHost t) where
  getFireAsync = AppHost $ fmap liftIO . writeChan . envEventChan <$> ask
  getRunAppHost = AppHost $ do
    env <- ask
    let rearrange (a, Ap m) = (m, a)
    pure $ \(AppHost m) -> rearrange <$> evalRSST m env ()
  performPostBuild_ mevent = AppHost . tell $ Ap mevent
  liftHostFrame = AppHost . lift
