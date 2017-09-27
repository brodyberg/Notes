module SimpleJSON where

data JValue = JString String
            | JNumber Double
            | JBool Bool
            | JNull
            | JObject [(String, JValue)]
            | JArray [JValue]
              deriving (Eq, Ord, Show)

printItems :: Show a => [a] -> String
printItems [] = ""
printItems (x:xs) = (show x) ++ printItems xs

-- printItems (x:xs) = (show x) ++ printItems xs
-- printItems x = show x
-- *SimpleJSON> printItems [1]
-- "1[]"
-- *SimpleJSON> printItems [1,2]
-- "12[]"
-- *SimpleJSON> printItems [1,2,3]
-- "123[]"
-- *SimpleJSON> :t printItems
-- printItems :: Show a => [a] -> String
-- *SimpleJSON> printItems []
-- "[]"

-- putJValue :: JValue -> IO ()
-- putJValue v = putStrLn (renderJValue v)

-- renderJValue :: JValue -> String
-- renderJValue (JString s) = s
-- renderJValue (JNumber d) = show d
-- renderJValue (JBool b) = show b
-- renderJValue JNull = "null"

-- -- renderJValue (JArray [a]) = undefined -- renderJValue [a] : renderJValue [as]
-- renderJValue (JArray [a:as]) = renderJValue [a] : renderJValue [as]
-- renderJValue (JArray [JValue a]) = undefined -- renderJValue [a] : renderJValue [as]
-- renderJValue (JArray [JString s]) = undefined -- renderJValue [a] : renderJValue [as]

--renderJValue (JObject o) =



  -- renderJValue (JNumber d) = "foo"
-- renderJValue _ _ = "unknown"
-- match value with 
--                    | JString s -> s
--                    | _ -> "unknown"