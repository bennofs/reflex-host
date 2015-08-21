reflex-host [![Build Status](https://secure.travis-ci.org/bennofs/reflex-host.png?branch=master)](http://travis-ci.org/bennofs/reflex-host)
====================

## Introduction

The primitives that the reflex FRP library provides for framework implementations are very basic.
Much of the code for implementing a reflex framework is just building a higher-level abstraction on top of these basic primitives.
`reflex-host` greatly simplifies the implementation of such a framework by already providing much of this functionality.

Using the library, you don't need to build your own event loop. You can just start registering external events and performing actions in response to FRP events.

## Getting started

The latest release of the library is available via Hackage. After you've installed the libraary, you can use it like this:

```haskell
-- import the reflex-host library and reflex itself
import Reflex.Host.App 
import Reflex

-- The application should be generic in the host monad that is used
app :: MonadAppHost t m => m ()
app = do
  (inputEvent, inputFire) <- newExternalEvent
  liftIO . forkIO $ getLine >>= inputFire
  performEvent_ $ fmap (liftIO . putStrLn) inputEvent

main :: IO ()
main = hostApp app
  
```

## Contributing

Contributions are always welcome. If you have any ideas, improvements or bug reports,
send a pull request or open a issue on github. If you have questions, y
ou can also find me as bennofs on freenode in the #haskell and #reflex-frp IRC channels.
