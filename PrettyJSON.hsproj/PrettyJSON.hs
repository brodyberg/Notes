module PrettyJSON 
    (
        renderJValue,
        compact,
        pretty,
        JValue(..)        
    ) where

import Numeric (showHex)
import Data.Char (ord)
import Data.Bits (shiftR, (.&.))

import SimpleJSON (JValue(..))
import Prettify (Doc, (<>), char, string, double, fsep, hcat, punctuate, text, compact, pretty, series)

renderJValue :: JValue -> Doc
renderJValue (JBool True)  = text "true"
renderJValue (JBool False) = text "false"
renderJValue JNull         = text "null"
renderJValue (JNumber num) = double num
renderJValue (JString str) = string str

renderJValue (JArray ary) = series '[' ']' renderJValue ary

renderJValue (JObject obj) = series '{' '}' field obj
    where field (name,value) = string name
                             <> text ": "
                             <> renderJValue value

