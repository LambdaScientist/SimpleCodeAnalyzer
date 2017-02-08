import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment


data Name = Name String deriving (Show, Eq)
data ReturnVal = ReturnVal String deriving (Show, Eq)
data Body = Bod Cpp | Flab String deriving (Show, Eq)
data Condition = Condition Cpp deriving (Show, Eq)
data Args = Args String deriving (Show, Eq)
data Cpp = If Condition Cpp
         | While Condition Cpp
         | GoTo
         | Return
         | Break
         | Function ReturnVal Name Args Body
         deriving (Show, Eq)



findFun :: Parser Cpp
findFun = do returnType <- many alphaNum
             _ <- space
             funName    <- many alphaNum
             _ <- char '('
             args <- many (noneOf ")")
             _ <- char ')'
             _ <- space
             body <- many anyChar
             eof
             return $ Function (ReturnVal returnType)
                               (Name funName)
                               (Args args)
                               (Flab body)



--------------
-- Testing
--------------
program1 :: String
program1 = "string answerQuestion(string q) { string a; if(q == \"Please?\") { a = \"Okay!\"; } else { a = \"You forgot the magic word!\"; } return a; }"
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
