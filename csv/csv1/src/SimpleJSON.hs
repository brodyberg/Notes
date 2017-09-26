module SimpleJSON where

data JValue = JString String
            | JNumber Double
            | JBool Bool
            | JNull
            | JObject [(String, JValue)]
            | JArray [JValue]
              deriving (Eq, Ord, Show)

putJValue :: JValue -> IO ()
putJValue v = putStrLn (renderJValue v)

renderJValue :: JValue -> String
renderJValue (JString s) = s
renderJValue (JNumber d) = show d
renderJValue (JBool b) = show b
renderJValue JNull = "null"

-- renderJValue (JArray [a]) = undefined -- renderJValue [a] : renderJValue [as]
renderJValue (JArray [a:as]) = renderJValue [a] : renderJValue [as]
-- renderJValue (JArray [JValue a]) = undefined -- renderJValue [a] : renderJValue [as]
-- renderJValue (JArray [JString s]) = undefined -- renderJValue [a] : renderJValue [as]

--renderJValue (JObject o) =



  -- renderJValue (JNumber d) = "foo"
-- renderJValue _ _ = "unknown"
-- match value with 
--                    | JString s -> s
--                    | _ -> "unknown"