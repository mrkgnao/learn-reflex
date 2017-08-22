# Learn Reflex!

This will be my notes as I learn [Reflex](https://github.com/reflex-frp/reflex), intended to help other people take the plunge too. 

First, if you haven't, install the [Reflex Platform](https://github.com/reflex-frp/reflex-platform/). 

# Setting things up

Let's enter a Nix shell with access to all the things we'll need. From the `reflex-platform` directory, type

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

## A note about type signatures

The ugly type signature on `ui` can be inferred, but only if you turn off GHC's `-XNoMonomorphismRestriction` flag, which is enabled by default and prevents GHC from inferring polymorphic signatures for top-level bindings (hence a *monomorphism restriction*, one which forbids *polymorphism*). This means that a binding such as `x = 1` at the top level of a Haskell file will, by default, be inferred to have the type `x :: Int`, instead of `x :: Num a => a`.

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
