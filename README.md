reflex-host [![Build Status](https://secure.travis-ci.org/bennofs/reflex-host.png?branch=master)](http://travis-ci.org/bennofs/reflex-host)
====================

An FRP framework needs to provide two things: it needs to supply the FRP network with input from the outside world (providing the "root" events on which the network is build)
and it needs to perform actions in response to FRP events (for example, updating the user interface when an event fires).  
However, the primitives that the reflex FRP library provides for this are very basic. 
This means that many FRP frameworks for reflex will end up building the same higher-level abstractions on top of these primitives again.
`reflex-host` greatly simplifies the implementation of such a framework by providing these higher-level abstractions so you don't need to implement them yourself.

Using the library, you don't need to build your own event loop. You can just start registering external events and performing actions in response to FRP events.

## Getting started

The library is not yet available. If you want to try it out, you'll have to install it directly from git.
After you've installed the library, you can use it like this:

```haskell
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
```

In the above example, we first implement the `reflex` library and the `reflex-host` library.
`reflex-host` provides its functionality through the `MonadAppHost t m` typeclass. Just as with
reflex, you should write your code to be polymorphic in the concrete types of `t` and `m`.

`app` is a generic action showing some of the features of `reflex-host`. First, we create
a new external event (an external event source is "external" in the sense that it has no
relation to other events in the FRP network). We then create a new thread that waits for
an input line from user and then fires the event with the line that it read.

That's the first half of `reflex-host`'s functionality, providing the application with events for external input. 
But, to actually *perform* something useful, our application also has to react to FRP events. 
That's what `performEvent_` does: it takes an `Event` containing a monadic action, and whenever the event fires, it'll perform that action.
In the example, we replace the line of text in `inputEvent` with an action that will print that line, and then use `performEvent_` to execute this action whenever the `inputEvent` fires.

## Current status / Future plans

The functionality of the library is mostly complete and it can already be used to implement your own FRP framework.
Compared to reflex-dom, there are no plans to include any DOM functionality in the library itself, it will remain
a building block that can be reused across frameworks for reflex.

There might still be some functions that turn out to be useful, but are not yet in the library. The library exposes
enough of the interface such that most higher-level functions should be implementable outside the library, but if
there are generic functions which are useful and missing, feel free to open an issue or send a pull request. 

Currently, there has been no real-world performance test of the library, so there might still be performance issues.
If you have code that performs very bad, please open an issue on the repository.

## Contributing

Contributions are always welcome. If you have any ideas, improvements or bug reports,
send a pull request or open a issue on github. If you have questions, you can also 
find me as bennofs on freenode in the #haskell and #reflex-frp IRC channels.
