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

-- import qualified Data.Map as Map
-- import Safe      (readMay)
-- import Data.Text (pack, unpack, Text)
-- import Data.Monoid ((<>))
-- main2 = mainWidget $ el "div" $ do
--   nx <- numberInput
--   d <- dropdown "+" (constDyn ops) def
--   ny <- numberInput
--   let values = zipDyn nx ny
--       result = zipDynWith (\o (x,y) -> textToOp o <$> x <*> y) (_dropdown_value d) values
--       resultText = fmap (pack . show) result
--   text " = "
--   dynText resultText

-- numberInput :: (MonadWidget t m) => m (Dynamic t (Maybe Double))
-- numberInput = do
--   let errorState = "style" =: "border-color: red"
--       validState = "style" =: "border-color: green"
--   rec n <- textInput $ def & textInputConfig_inputType .~ "number"
--                            & textInputConfig_initialValue .~ "0"
--                            & textInputConfig_attributes .~ attrs
--       let result = (_textInput_value n) <&> (readMay . unpack)
--           attrs  = result <&> (maybe errorState (const validState))
--   return result

-- ops = Map.fromList [("+", "+"), ("-", "-"), ("*", "*"), ("/", "/")]

-- textToOp :: (Fractional a) => Text -> a -> a -> a
-- textToOp s = case s of
--                     "-" -> (-)
--                     "*" -> (*)
--                     "/" -> (/)
--                     _ -> (+)
