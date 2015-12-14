-- import the reflex-host library and reflex itself
import Reflex.Host.App
import Reflex
import Control.Concurrent (forkIO)
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)

-- The application should be generic in the host monad that is used
app :: MonadAppHost t m => m ()
app = do
  (inputEvent, inputFire) <- newExternalEvent
  liftIO . forkIO . forever $ getLine >>= inputFire
  performEvent_ $ fmap (liftIO . putStrLn) inputEvent

main :: IO ()
main = runSpiderHost $ hostApp app
