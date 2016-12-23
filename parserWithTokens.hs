import System.Random

-- [ JSON ] --
data JSON
    = JObject [(String, JSON)]
    | JArray [JSON]
    | JString String
    | JNum Float
    | JBool Bool
    | JNull
    | JParseError String
    deriving (Eq)


-- [ TOKENIZING ] --
data Token
    = TOpenBracket
    | TCloseBracket
    | TOpenBrace
    | TCloseBrace
    | TComma
    | TColon
    | TNull
    | TBool Bool
    | TString String
    | TNumber Float
    | TError String
    deriving (Show, Eq)

tokenize :: String -> [Token]
tokenize "" = []
-- whitespace skipping
tokenize (' ':xs) = tokenize xs
tokenize ('\n':xs) = tokenize xs
tokenize ('\t':xs) = tokenize xs

tokenize ('[':xs) = TOpenBracket : (tokenize xs)
tokenize (']':xs) = TCloseBracket : (tokenize xs)
tokenize ('{':xs) = TOpenBrace : (tokenize xs)
tokenize ('}':xs) = TCloseBrace : (tokenize xs)
tokenize (',':xs) = TComma : (tokenize xs)
tokenize (':':xs) = TColon : (tokenize xs)
tokenize ('n':'u':'l':'l':xs) = TNull : (tokenize xs)
tokenize ('f':'a':'l':'s':'e':xs) = TBool False : (tokenize xs)
tokenize ('t':'r':'u':'e':xs) = TBool True : (tokenize xs)
tokenize ('"':xs) = case result of Just (str, rest) -> TString str : tokenize rest
                                   Nothing -> [TError "Unterminated string literal"]
    where
        result = parseEscapedString xs
        parseEscapedString :: String -> Maybe (String, String)
        parseEscapedString input = let
            res :: Maybe (String, String) -> Maybe (String, String)
            res Nothing = Nothing
            res (Just (_, "")) = Nothing -- input ended unexpectedly
            res (Just (parsed, ('\\':'"':xs))) = res $ Just (parsed ++ "\"", xs) -- escaped qoute, unescaping
            res (Just (parsed, ('\\':'\\':xs))) = res $ Just (parsed ++ "\\", xs) -- escaped backslash, unescaping
            res (Just (parsed, ('\\':'n':xs))) = res $ Just (parsed ++ "\n", xs) -- escaped entry, unescaping
            res (Just (parsed, ('\\':'r':xs))) = res $ Just (parsed ++ "\r", xs) -- escaped entry, unescaping
            res (Just (parsed, ('"':xs))) = Just (parsed, xs) -- quote symbol met, string has ended
            res (Just (parsed, (x:xs))) = res $ Just (parsed ++ [x], xs)
            in res $ Just ("", input)

tokenize str = tokenOrError : (tokenize xs) where
    pair = reads str :: [(Float, String)]
    (tokenOrError, xs) = case pair of [(n, str')] -> (TNumber n, str')
                                      _ -> (TError ("Unexpected symbol " ++ (take 1 str)), "")


-- [ PARSING ] --

parse :: String -> JSON
parse str = case (parseValue $ tokenize str) of (json, []) -> json
                                                _ -> JParseError "Invalid JSON value"
    where
        parseValue :: [Token] -> (JSON, [Token])
        parseValue [] = (JParseError "Unexpected end of file", [])
        parseValue ((TError msg):xs) = (JParseError msg, [])
        parseValue (TOpenBrace:xs) = parseObj (TOpenBrace:xs)
        parseValue (TCloseBrace:xs) = parseObj (TCloseBrace:xs)
        parseValue (TOpenBracket:xs) = parseArr (TOpenBracket:xs)
        parseValue (TCloseBracket:xs) = parseArr (TCloseBracket:xs)
        parseValue ((TString s):xs) = parseString ((TString s):xs)
        parseValue ((TNumber n):xs) = parseNumber ((TNumber n):xs)
        parseValue ((TBool b):xs) = parseBool ((TBool b):xs)

        parseValue (TNull:xs) = (JNull, xs)
        parseValue _ = (JParseError "Unexpected token met", [])

        -- {} --
        parseObj :: [Token] -> (JSON, [Token])
        parseObj (TOpenBrace:TCloseBrace:xs) = (JObject [], xs)
        parseObj (TOpenBrace:xs) = result where
            (prop, rest) = parseProp xs
            result = case (parseObj rest, prop)
                     of (_, (_, JParseError msg)) -> (JParseError msg, [])
                        ((JObject props, r), prop) -> (JObject (prop:props), r)
                        ((JParseError msg, r), _) -> (JParseError msg, r)
        parseObj (TComma:xs) = parseObj (TOpenBrace:xs) -- equally for comma and {, a property will go after
        parseObj (TCloseBrace:xs) = (JObject [], xs) -- recursion end
        parseObj [] = (JParseError "Unexpected eof while parsing an object", [])
        parseObj _ = (JParseError "Unexpected token while parsing an object", [])

        -- "<name>":<value>
        parseProp :: [Token] -> ((String, JSON), [Token]) -- a json property doesn't have a special constructor
        parseProp ((TString name):TColon:xs) = ((name, value), rest) where
            (value, rest) = parseValue xs
        parseProp [] = (("", JParseError "Unexpected eof while parsing a property"), [])
        parseProp ((TString name):[]) = parseProp []
        parseProp _ = (("", JParseError "Unexpected token while parsing a property"), [])

        -- []
        parseArr :: [Token] -> (JSON, [Token])
        parseArr (TOpenBracket:TCloseBracket:xs) = (JArray [], xs)
        parseArr (TOpenBracket:xs) = result where
            (val, rest) = parseValue xs
            result = case (parseArr rest, val)
                     of (_, JParseError msg) -> (JParseError msg, [])
                        ((JArray items, r), val) -> (JArray (val:items), r)
                        ((JParseError msg, _), _) -> (JParseError msg, [])
            -- (JArray vals, rest') = parseArr rest
        parseArr (TComma:xs) = parseArr (TOpenBracket:xs)
        parseArr (TCloseBracket:xs) = (JArray [], xs)
        parseArr [] = (JParseError "Unexpected eof while parsing an array", [])
        parseArr _ = (JParseError "Unexpected token while parsing an array", [])

        -- "<str>"
        parseString :: [Token] -> (JSON, [Token])
        parseString ((TString str):xs) = (JString str, xs)
        parseString _ = (JParseError "Unexpected token while awaited for a string", [])

        -- false|true
        parseBool :: [Token] -> (JSON, [Token])
        parseBool ((TBool b):xs) = (JBool b, xs)
        parseBool _ = (JParseError "Unexpected token while awaited for a bool", [])

        -- <n>
        parseNumber :: [Token] -> (JSON, [Token])
        parseNumber ((TNumber n):xs) = (JNum n, xs)
        parseNumber _ = (JParseError "Unexpected token while awaited for a number", [])

-- [ STRINGIFY ] --

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

stringify (JParseError msg) = "JSON parse error: " ++ msg

instance Show JSON where
    show = stringify

-- [ Lab2 Task ] --

lab2 :: JSON -> (Integer, Integer)
lab2 tree = (lab2' tree) where
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
    lab2' (JArray nodes)  = foldl theFold (toInteger $ length nodes, 1) $ map lab2' nodes
    lab2' (JObject nodes) = foldl theFold (toInteger $ length nodes, 1) $ map (lab2' . snd) nodes

-- [ Random Tree Generation ] --

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
