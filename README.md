# Learn Reflex!

This will be my notes as I learn [Reflex](https://github.com/reflex-frp/reflex), intended to help other people take the plunge. Reflex is a suite of Haskell libraries and tools that enables a wonderful way to do functional reactive programming, or *FRP*, by applying the declarative nature of Haskell to user interfaces that run efficiently in the browser (with the GHCJS compiler) or as native applications (using GTK).

At the time I'm beginning to write this, I'm an intermediate-level Haskeller with no previous experience with either FRP or nontrivial JavaScript/web development. Let's see how far I get!

# Setting things up

First, if you haven't, install the [Reflex Platform](https://github.com/reflex-frp/reflex-platform/). Let's enter a Nix shell with access to all the things we'll need. 

From the `reflex-platform` directory, type

```
$ ./try-reflex
If you have any trouble with this script, please submit an issue at https://github.com/reflex-frp/reflex-platform/issues
Entering the reflex sandbox...
You are now in a shell with access to the Reflex functional reactive programming engine.

<snip>

Or to see a more complex GUI example (based on the source at https://github.com/reflex-frp/reflex-todomvc/blob/master/src/Main.hs), navigate your browser to file:///nix/store/mp8dpmfly8wxd03azp37sm5j2shwmsry-reflex-todomvc-0.1/bin/reflex-todomvc.jsexe/index.html
```

You should have output similar to mine.

For now, create a directory somewhere that you'll use to work on this tutorial. I recommend this:

```
$ cd ..
$ mkdir learn-reflex
$ cd learn-reflex
```


# Our first, trivial example

Let's build a little *cough* webapp that asks the user for their first and last names and displays a simple greeting to them. This is, to me, akin to a "Hello, world!" for functional reactive programming.

Put the following code in a file, which I'll name `greetings.hs`:

```haskell

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

import Reflex
import Reflex.Dom

main :: IO ()
main = mainWidget (el "div" ui)

ui :: (DomBuilderSpace m ~ GhcjsDomSpace, DomBuilder t m, PostBuild t m) => m ()
ui = do
  fname <- textInput def
  lname <- textInput def
  let name = mconcat [ constDyn "Hello, "
                     , _textInput_value fname
                     , constDyn " "
                     , _textInput_value lname
                     , constDyn "!"
                     ]
  dynText name
```

We'll use `ghcjs` to compile this. Run

```
[nix-shell:~/code/haskell/learn-reflex]$ ghcjs --make greetings.hs
[1 of 1] Compiling Main             ( greetings.hs, greetings.js_o )
Linking greetings.jsexe (Main)
```

GHCJS compiles the Haskell code to a JavaScript file. It also creates an `index.html` file that loads the generated JavaScript: open `greetings.jsexe/index.html` in a browser!

![greetings.hs screenshot](https://raw.githubusercontent.com/mrkgnao/learn-reflex/master/greetings.png)

There's ... still a lot of work to do design-wise, but let's first try to understand the big chunk of code you just pasted into some editor and built.

# The stuff that functional reactive programming is made of

Almost all incarnations of FRP are based on the observation that there are 

* *events* that fire once in a while: keypresses, doors opening, changes in device orientation

* *behaviors* or variables that always have some possibly changing value: mouse position, the current time, health points

and games or user interfaces can be described very succinctly (and, as it turns out, efficiently) in terms of these primitive notions. 

Events and behaviors can be manipulated in various ways. For example, one can create an event from a behavior `b` that fires whenever the value of `b` changes.

Another example is that of combining two behaviors `a` and `b` into a new behavior `c` with some "combining function" `f`. The new behavior `c`, at any time, has a value defined to be that obtained by combining the values of `a` and `b` at that time using `f`. This is akin to the following well-known Haskell function:
```haskell
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
```
For example, given behaviors `playerAPoints` and `playerBPoints` corresponding to the scores of players A and B in some game, one might write a function to find if player A is leading, or to find if the game is tied:
```haskell
isPlayerALeading = zipBehaviorsWith (>)  playerAPoints playerBPoints
isGameTied       = zipBehaviorsWith (==) playerAPoints playerBPoints
```

## A note about type signatures

The ugly type signature on `ui` can be inferred, but only if you turn off GHC's `-XMonomorphismRestriction` flag, which is enabled by default and prevents GHC from inferring polymorphic signatures for top-level bindings (hence a *monomorphism restriction*, one which forbids *polymorphism*). This means that a binding such as `x = 1` at the top level of a Haskell file will, by default, be inferred to have the type `x :: Int`, instead of `x :: Num a => a`.

To do so, we add a language pragma at the top of the file that turns that flag off. With that in place, the code compiles fine without the type signature:

```haskell

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

import Reflex
import Reflex.Dom

main :: IO ()
main = mainWidget $ el "div" ui

-- ui :: (DomBuilderSpace m ~ GhcjsDomSpace, DomBuilder t m, PostBuild t m) => m ()
ui = do
  fname <- textInput def
  lname <- textInput def
  let name = mconcat [ constDyn "Hello, "
                     , _textInput_value fname
                     , constDyn " "
                     , _textInput_value lname
                     , constDyn "!"
                     ]
  dynText name
```
