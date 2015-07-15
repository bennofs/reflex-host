reflex-host [![Build Status](https://secure.travis-ci.org/bennofs/reflex-host.png?branch=master)](http://travis-ci.org/bennofs/reflex-host)
====================

## Introduction

The primitives that the reflex FRP library provides for framework implementations are very basic.
Much of the code for implementing a reflex framework is just building a higher-level abstraction on top of these basic primitives.
This library tries to make the process of implementing such a framework easier by already providing some of this functionality.

Using the library, you don't need to build your own event loop. You can just start registering external events and performing actions in response to FRP events.

## Contributing

Contributions are always welcome. If you have any ideas, improvements or bug reports,
send a pull request or open a issue on github. If you have questions, y
ou can also find me as bennofs on freenode in the #haskell and #reflex-frp IRC channels.
