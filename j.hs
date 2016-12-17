import System.Random

data JSON =
    JObject [(String, JSON)] |
    JArray [JSON] |
    JString String |
    JNum Float |
    JBool Bool |
    JNull
    deriving (Eq)

join :: [String] -> String
join [] = ""
join (x:[]) = x
join (x:xs) = x ++ "," ++ (join xs)

stringify :: JSON -> String
stringify JNull = "null"
stringify (JBool False) = "false"
stringify (JBool True) = "true"

stringify (JNum n) = show n

stringify (JString s) = ('"' : (escape s)) ++ "\"" where
    escape :: String -> String
    escape s = foldl esc "" s where
        esc acc '\\' = acc ++ "\\\\"    -- '\' -> '\\'
        esc acc '"' = acc ++ "\\\""       -- '"' -> '\"'
        esc acc item = acc ++ [item]

stringify (JArray values) = bracketize $ join $ map stringify values where
    bracketize :: String -> String
    bracketize s = "[" ++ s ++ "]"

stringify (JObject dict) = bracketize $ join $ map stringify' dict where
    stringify' :: (String, JSON) -> String
    stringify' (key, value) = "\"" ++ key ++ "\":" ++ (stringify value)
    bracketize :: String -> String
    bracketize s = "{" ++ s ++ "}"

instance Show JSON where
    show = stringify


-- find amount of nodes with maximal power
lab2 :: JSON -> Integer
lab2 tree = snd (lab2' tree) where
    theFold :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
    theFold (accPwr, accN) (itemPwr, itemN)
        | accPwr > itemPwr = (accPwr, accN)
        | accPwr == itemPwr = (accPwr, accN + itemN)
        | accPwr < itemPwr = (itemPwr, itemN)

    lab2' :: JSON -> (Integer, Integer)
    lab2' JNull = (0, 1)
    lab2' (JBool _) = (0, 1)
    lab2' (JNum _) = (0, 1)
    lab2' (JString _) = (0, 1)
    lab2' (JArray []) = (0, 1)
    lab2' (JObject []) = (0, 1)
    lab2' (JArray nodes)  = foldl theFold (toInteger $ length nodes, 1) $ map lab2' nodes
    lab2' (JObject nodes) = foldl theFold (toInteger $ length nodes, 1) $ map (lab2' . snd) nodes


randomTree :: StdGen -> Integer -> (JSON, StdGen)
randomTree g depthLeft = case rnd of 0 -> rString g'
                                     1 -> rNum g'
                                     2 -> rBool g'
                                     4 -> rObj g'
                                     5 -> rArr g'
                                     _ -> (JNull, g')
    where
        (rnd, g') = randomR (0, (if depthLeft == 0 then 3 else 5)) g :: (Integer, StdGen)

        rChar :: StdGen -> (Char, StdGen)
        rChar g = randomR ('a', 'z') g

        rString :: StdGen -> (JSON, StdGen)
        rString g = (JString chars, gen) where
            (n, g') = randomR (0, 16) g
            theFold :: ([Char], StdGen) -> Integer -> ([Char], StdGen)
            theFold (chs, gen) _ = (c:chs, gen') where
                (c, gen') = rChar gen
            (chars, gen) = foldl theFold ([], g') [1..n]

        rNum :: StdGen -> (JSON, StdGen)
        rNum g = (JNum n, g') where (n, g') = random g

        rBool :: StdGen -> (JSON, StdGen)
        rBool g = (JBool b, g') where (b, g') = random g

        rArr :: StdGen -> (JSON, StdGen)
        rArr g = (JArray subtrees, gen) where
            (n, g') = randomR (0, 16) g
            theFold :: ([JSON], StdGen) -> Integer -> ([JSON], StdGen)
            theFold (subs, gen) _ = (newTree:subs, gen') where
                (newTree, gen') = randomTree gen (depthLeft - 1)
            (subtrees, gen) = foldl theFold ([], g') [1..n]

        rProp :: StdGen -> (String, JSON, StdGen)
        rProp g = (key, value, g') where
            (JString key, g'') = rString g
            (value, g') = randomTree g'' (depthLeft - 1)

        rObj :: StdGen -> (JSON, StdGen)
        rObj g = (JObject properties, gen) where
            (n, g') = randomR (0, 16) g
            theFold :: ([(String, JSON)], StdGen) -> Integer -> ([(String, JSON)], StdGen)
            theFold (props, gen) _ = ((newPropName, newPropVal):props, gen') where
                (newPropName, newPropVal, gen') = rProp gen
            (properties, gen) = foldl theFold ([], g') [1..n]


type ImString = Maybe (String, String)
type ImResult = Maybe (String, JSON)
parse :: String -> (Maybe JSON)
parse "" = Nothing
parse s = case (parseValue $ removeWS s) of Nothing -> Nothing
                                            Just ("", json) -> Just json
                                            _ -> Nothing
    where
        readEscapedStr :: String -> ImString
        readEscapedStr input = let
            res :: ImString -> ImString
            res Nothing = Nothing
            res (Just (_, "")) = Nothing -- input ended unexpectedly
            res (Just (parsed, ('\\':'"':xs))) = res $ Just (parsed ++ "\"", xs) -- escaped qoute, unescaping
            res (Just (parsed, ('\\':'\\':xs))) = res $ Just (parsed ++ "\\", xs) -- escaped backslash, unescaping
            res (Just (parsed, ('"':xs))) = Just (parsed, xs) -- quote symbol met, string has ended
            res (Just (parsed, (x:xs))) = res $ Just (parsed ++ [x], xs)
            in res $ Just ("", input)

        parseValue :: String -> ImResult
        parseValue (x:xs)
            | x == '[' = parseArr (x:xs) []
            | x == '{' = parseObj (x:xs) []
            | x == '"' = parseStr (x:xs)
            | (x `elem` "-0123456789") = parseNum (x:xs)
            | otherwise = parseKeyword (x:xs)

        -- may be (rest, prop, value)
        parseProp :: String -> Maybe (String, String, JSON)
        parseProp "" = Nothing
        parseProp ('"':s) = case (rest, prop, value) of (_, Nothing, _) -> Nothing
                                                        (_, _, Nothing) -> Nothing
                                                        (Just a, Just b, Just c) -> Just (a, b, c)
            where
                propName = readEscapedStr s
                (prop, rest') = case propName of Nothing -> (Nothing, Nothing)
                                                 Just (parsed, rest'') -> (Just parsed, Just rest'')
                readPropValue :: Maybe String -> ImResult
                readPropValue (Just (':':xs)) = parseValue xs
                readPropValue _ = Nothing

                (value, rest) = case (readPropValue rest') of Nothing -> (Nothing, Nothing)
                                                              Just (rst, json) -> (Just json, Just rst)
        parseProp _ = Nothing

        parseObj :: String -> [(String, JSON)] -> ImResult
        parseObj "" entries = Nothing
        parseObj ('{':'}':str) _ = Just (str, JObject [])
        parseObj (s:str) entries
            | s == '{' = let
                r = parseProp str
                in if r == Nothing
                    then Nothing -- parse error
                    else let Just (rest, name, entry) = r
                        in parseObj rest (entries ++ [(name, entry)])
            | s == '}' = Just (str, JObject entries)
            | s == ',' = let
                r = parseProp str
                in if r == Nothing
                    then Nothing -- parse error
                    else let Just (rest, name, entry) = r
                        in parseObj rest (entries ++ [(name, entry)])
            | otherwise = Nothing

        parseArr :: String -> [JSON] -> ImResult
        parseArr "" entries = Nothing
        parseArr ('[':']':str) _ = Just (str, JArray [])
        parseArr (s:str) entries
            | s == '[' = let
                r = parseValue str
                in if r == Nothing
                    then Nothing -- parse error
                    else let Just (rest, entry) = r
                        in parseArr rest (entries ++ [entry])
            | s == ']' = Just (str, JArray entries)
            | s == ',' = let
                r = parseValue str
                in if r == Nothing
                    then Nothing -- parse error
                    else let Just (rest, entry) = r
                        in parseArr rest (entries ++ [entry])
            | otherwise = Nothing

        parseStr :: String -> ImResult
        parseStr ('"':str) = case (readEscapedStr str) of Nothing -> Nothing
                                                          Just (val, rest) -> Just (rest, JString val)
        parseStr _ = Nothing

        -- TODO: support exponents
        parseNum :: String -> ImResult
        parseNum str
            | null cut = Nothing
            | otherwise = Just (rest, JNum $ read cut)
            where (rest, cut) = cutMaxNum str ""

        cutMaxNum :: String -> String -> (String, String)
        cutMaxNum "" acc = ("", acc)
        cutMaxNum (x:xs) acc
            | (not $ null acc) && x == '-' = ((x:xs), acc)
            | (x == '.') = case xs of "" -> ((x:xs), acc)
                                      (y:ys) -> if (y `elem` "0123456789")
                                                then cutMaxNum ys (acc ++ (x:y:""))
                                                else ((x:xs), acc)
            | (not (x `elem` "-0123456789")) = ((x:xs), acc)
            | otherwise = cutMaxNum xs (acc ++ [x])


        parseKeyword :: String -> ImResult
        parseKeyword ('n':'u':'l':'l':xs) = Just (xs, JNull)
        parseKeyword ('t':'r':'u':'e':xs) = Just (xs, JBool True)
        parseKeyword ('f':'a':'l':'s':'e':xs) = Just (xs, JBool False)
        parseKeyword _ = Nothing

        removeWS :: String -> String
        removeWS from = remWS "" False False from where
            remWS :: String -> Bool -> Bool -> String -> String
            remWS clean _ _ "" = clean
            remWS clean inString escaped (f:fs)
                | not inString && (f `elem` " \t\n") = remWS clean False escaped fs
                | inString && f == '\\' = remWS (clean ++ [f]) inString (not escaped) fs
                | not inString && f == '"' = remWS (clean ++ [f]) True False fs
                | inString && f == '"' = remWS (clean ++ [f]) escaped escaped fs
                | otherwise = remWS (clean ++ [f]) inString escaped fs

-- endof parse where clause

example1 = JArray
    [
        JObject
            [
                ("name", JString "Bob"),
                ("surname", JNull),
                ("password", JNull)
            ],
        JObject
            [
                ("name", JString "Alice"),
                ("surname", JString "Cooper"),
                ("password", JString "123")
            ],
        JObject
            [
                ("name", JString "Eve"),
                ("surname", JString "Offline"),
                ("password", JNull),
                ("priveleges", JArray [
                    JString "root"
                ])
            ]
    ]

example2 = "{\"aliases\":[{\"name\":\"stat\",\"value\":\"status --branch --short\"}],\"statistics\":[20,-1,10.0,15.2,-1.0]}";

example3 = "{ \"aliases\": [{ \"name\": \"stat\", \"value\": \"status --branch --short\" }] }";
