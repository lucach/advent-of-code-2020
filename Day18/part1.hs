import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language (emptyDef)
import Data.Either

lexer = makeTokenParser emptyDef 

expr = buildExpressionParser table term
     <?> "expression"

term = parens lexer expr 
     <|> natural lexer
     <?> "simple expression"

table = [[binary "+" (+) AssocLeft, binary "*" (*) AssocLeft]]

binary name fun = Infix (do{ reservedOp lexer name; return fun })

main = do
    content <- readFile "input.txt"
    let results = rights $ map (parse expr "") $ lines content
    print $ sum results