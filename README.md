# Learn Reflex!

This will be my notes as I learn [Reflex](https://github.com/reflex-frp/reflex), intended to help other people take the plunge too. 

First, if you haven't, install the [Reflex Platform](https://github.com/reflex-frp/reflex-platform/).

# Our first, trivial example

Let's build a little *cough* webapp that asks the user for their first and last names and displays a simple greeting to them. This is, to me, akin to a "Hello, world!" for functional reactive programming.

```haskell

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

import Reflex
import Reflex.Dom

main :: IO ()
main = mainWidget $ el "div" ui

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
