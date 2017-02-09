import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment


data Name = Name String deriving (Show, Eq)
data ReturnVal = ReturnVal String deriving (Show, Eq)
data Body = Bod Cpp | Flab String deriving (Show, Eq)
data Condition = Condition [Cpp] deriving (Show, Eq)
data Args = Args String deriving (Show, Eq)
data Cpp = If Condition Cpp
         | While Condition Cpp
         | GoTo
         | Return
         | Break
         | Function ReturnVal Name Args Cpp
         | ManyFun [Cpp]
         | UnknownCode String
         deriving (Show, Eq)



findFun :: Parser Cpp
findFun = do returnType <- many alphaNum
             _ <- space
             funName <- many alphaNum
             args <- between (char '(') (char ')') (many (noneOf ")"))
             _ <- space
             body <- parseFunCode
             eof
             return $ Function (ReturnVal returnType)
                               (Name funName)
                               (Args args)
                               (ManyFun body)

matchBrace :: Parser [Cpp]
matchBrace = between (char '{') (char '}') parseFunCode
matchParenthesis  :: Parser [Cpp]
matchParenthesis = between (char '(') (char ')') parseFunCode
matchBracket  :: Parser [Cpp]
matchBracket = between (char '[') (char ']') parseFunCode

parseFunCode :: Parser [Cpp]
parseFunCode = many1 grammar >>= (return.map mergeCpp)
  where grammar = choice [ matchBrace
                         , matchParenthesis
                         , matchBracket
                         , many1 findIf
                         , many1 findUnknown
                         ]

findIf :: Parser Cpp
findIf = do _ <- string "if"
            cond <- matchParenthesis
            body <- matchBrace
            return (If (Condition cond) (mergeCpp body))


mergeCpp :: [Cpp] -> Cpp
mergeCpp = foldl mergeCpp' (ManyFun [])

mergeCpp' :: Cpp -> Cpp -> Cpp
mergeCpp' (ManyFun x) (ManyFun y) = ManyFun $ x ++ y
mergeCpp' x (ManyFun y) = ManyFun $ x:y
mergeCpp' (ManyFun x) y = ManyFun $ x ++ y:[]
mergeCpp' x y = ManyFun $ x:y:[]


findUnknown :: Parser Cpp
findUnknown = many1 (noneOf "{([])}") >>= (return.UnknownCode)





--------------
-- Testing
--------------
program1 :: String
program1 = "string answerQuestion(string q) { string a; if (q == \"Please?\") { a = \"Okay!\"; } else { a = \"You forgot the magic word!\"; } return a; }"
{-
string answerQuestion(string q) { string a; if(q == "Please?") { a = "Okay!"; } else { a = "You forgot the magic word!"; } return a; }"

string answerQuestion(string q)
          {
            string a;
            if(q == "Please?") {
              a = "Okay!";
            }
            else {
              a = "You forgot the magic word!";
            }
            return a;
          }
-}
