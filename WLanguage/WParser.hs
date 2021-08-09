module WParser ( parse,
                 wprogram ) where

    import Data.Char
    import W

    import Control.Applicative (Applicative(..))
    import Control.Monad (liftM, ap)


   -- Hope this can help y'all: I'd say first start with parsing literal number and variable name first, 
   -- meaning if given a string like "123" or "a" how would y'all extract the number and such in the 
   -- string to turn those into Val (VInt 123) and Var "a".

   -- Same for the comparison expressions. For example given "a < b", how would you parse it so that you can tell the interpreter to do eval (Less a b)
   -- The piece of code in the parsing guidance sort of like a template on how to implement +-*/ operations (hint: And Or logic expressions are in some way similar to plus and multiply)

  --  your parser turns something like
  --  var x = 10;
  --  x = 5;
  --  into
  --  [(Block [VarDecl "x" (Val (VInt 10)),Assign "x" (Val 5)],"")]


    -----------------------------
    -- This is the main parser --
    -----------------------------
    wprogram :: Parser WStmt
    wprogram = whitespace >> many stmt >>= \ss -> return (Block ss)

    stmt = varDeclStmt +++ assignStmt +++ ifStmt +++ whileStmt +++ 
           blockStmt +++ emptyStmt +++ printStmt

    --STATEMENT PARSERS
    emptyStmt = 
      symbol ";" >>
      return Empty                      --parse whileStmt        parse whileStmt "while (5) print \"hello\";"        parse whileStmt "while (5) ;"       parse whileStmt "while (5) x = \"True\";"
  
    printStmt =                         
      keyword "print" >>
      expr >>= \e ->
      symbol ";" >>
      return (Print e)

    varDeclStmt = 
        keyword "var" >>
        identifier >>= \e -> 
        symbol "=" >>
        expr >>= \c ->
        symbol ";" >>  
        return (VarDecl e c)

    assignStmt = 
        identifier >>= \e ->
        symbol "=" >>
        expr >>= \c ->
        symbol ";" >>  
        return (Assign e c)

    ifStmt =                     -- parse ifStmt "if (true) (do this); else (do this);"
        keyword "if" >>
        symbol "(" >>
        expr >>= \c ->
        symbol ")" >>
        stmt >>= \d -> 
        keyword "else" >>
        stmt >>= \e -> 
        return (If c d e)

    whileStmt = 
        keyword "while" >>
        symbol "(" >>
        expr >>= \c ->
        symbol ")" >>
        stmt >>= \d ->
        return (While c d)

    blockStmt = 
        symbol "{" >>
        (many stmt) >>= \a ->
        symbol "}" >>
        return(Block a)     


    --LITERALS
    boolLiteral = identifier >>= \a -> 
                   if(a == "True") then return (Val(VBool True))
                   else return (Val(VBool False)) 



    stringLiteral = char ('"') >>                           --parse stringLiteral "\"abc\""
                    many stringChar >>= \s ->
                    char ('"') >>
                    whitespace >>
                    return (Val (VString s))

    stringChar = (char '\\' >> char 'n' >> return '\n') 
                 +++ sat (/= '"')

    intLiteralPos = many digit >>= \d -> return (Val (VInt(read d)))  --parse intLiteral "45"

    intLiteralNeg = symbol "-" >>= \s ->
                    many1 digit >>= \d ->
                    return ((Val(VInt (-(read d)))))
    
    intLiteral = intLiteralNeg +++ intLiteralPos

    variableL = identifier >>= \a ->
                return (Var a)
    


-------------------------------- expression parsing
    expr =  (andOr >>= andOrSeq) +++ boolLiteral +++ variableL  +++ intLiteral  +++  stringLiteral  

    andOrSeq left = ( (symbol "&&" +++ symbol "||") >>= \s ->                                   ----And and OR && and ||
                 andOr >>= \right ->
                 andOrSeq ((toOp s) left right)
               ) +++ return left 

    andOr = arithmeticExpr >>= arithmeticExprSeq


    arithmeticExprSeq left = ( (symbol "==" +++ symbol "!=") >>= \s ->                          ---- equal or not equal
                 arithmeticExpr >>= \right ->
                 arithmeticExprSeq ((toOp s) left right)
               ) +++ return left

    arithmeticExpr = greaterThan >>= greaterThanSeq


    greaterThanSeq left = ( (symbol ">=" +++ symbol ">") >>= \s ->                              ----greater than or greater than or equal
                 greaterThan >>= \right ->
                 greaterThanSeq ((toOp s) left right)
               ) +++ return left 

    greaterThan = lessThan >>= lessThanSeq


    lessThanSeq left = ( (symbol "<=" +++ symbol "<") >>= \s ->                                 -----less than or less than or equal
                 lessThan >>= \right ->
                 lessThanSeq ((toOp s) left right)
               ) +++ return left 

    lessThan = term >>= termSeq

    termSeq left = ( (symbol "+" +++ symbol "-") >>= \s ->                                  ----- plus or minus
                 term >>= \right ->
                 termSeq ((toOp s) left right)
               ) +++ return left

    term = factor >>= factorSeq 

    factorSeq left = ( (symbol "*" +++ symbol "/") >>= \s ->                                    ------times or divide
                   factor >>= \right ->
                   factorSeq ((toOp s) left right)
                 ) +++ return left

    factor = ( nat >>= \i ->
           return (Val (VInt i))
         ) +++ variableL+++ boolLiteral  +++ stringLiteral +++ parens expr 

    toOp "+" = Plus
    toOp "-" = Minus
    toOp "*" = Multiplies
    toOp "/" = Divides
    toOp "==" = Equals
    toOp "!=" = NotEqual
    toOp "<" = Less
    toOp ">" = Greater
    toOp "<=" = LessOrEqual
    toOp ">=" = GreaterOrEqual
    toOp "&&" = And
    toOp "||" = Or

-------------------------------------------------------------------------------------------------


    ----------------------
    -- Parser utilities --
    ----------------------

    keywords = words "var if else while true false True False"
    isKeyword s = s `elem` keywords

    bools = words "true false True False"
    isbool s = s `elem` keywords

    keyword s =                                            -- parse (keyword "hello") "hello"
      identifier >>= \s' ->                                             --if keyword (s) is identifier then return s
      if s' == s then return s else failure     
       
    newtype Parser a = P (String -> [(a, String)])
    
    parse :: Parser a -> String -> [(a, String)]
    parse (P p) inp = p inp
    
    instance Functor Parser where
        fmap = liftM
     
    instance Applicative Parser where
        pure  = return
        (<*>) = ap
    
    instance Monad Parser where
        -- return :: a -> Parser a
        return v = P $ \inp -> [(v, inp)]
                 
        -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
        p >>= q = P $ \inp -> case parse p inp of 
                                [] -> []
                                [(v, inp')] -> let q' = q v in parse q' inp'
    
    failure :: Parser a
    failure = P $ \_ -> []
    
    item :: Parser Char                             --get first char, parse (item) "ididid" -> 'i' "didid"
    item = P $ \inp -> case inp of 
                         (x:xs) -> [(x, xs)]
                         [] -> []
    
    -- Parse with p or q
    (+++) :: Parser a -> Parser a -> Parser a
    p +++ q = P $ \inp -> case parse p inp of 
                              [] -> parse q inp
                              [(v, inp')] -> [(v, inp')]
    
    
    -- Simple helper parsers
    sat :: (Char -> Bool) -> Parser Char
    sat pred = item >>= \c ->
               if pred c then return c else failure
    
    digit, letter, alphanum :: Parser Char
    digit = sat isDigit
    letter = sat isAlpha
    alphanum = sat isAlphaNum
    
    char :: Char -> Parser Char
    char x = sat (== x)
    
    string = sequence . map char 
    
    many1 :: Parser a -> Parser [a]
    many1 p = p >>= \v ->
              many p >>= \vs ->
              return (v:vs)
    
    many :: Parser a -> Parser [a]
    many p = many1 p +++ return []
    
    -- Useful building blocks
    nat :: Parser Int
    nat = many1 digit >>= \s ->
          whitespace >>
          return (read s)
    
    identifier :: Parser String                            --- parse (identifier) "hi"   if start with letter bad
    identifier = letter >>= \s ->
                 many alphanum >>= \ss ->
                 whitespace >>
                 return (s:ss)
    
    whitespace :: Parser ()
    whitespace = many (sat isSpace) >> comment
    
    symbol s = 
        string s >>= \s' ->
        whitespace >>
        return s'    
    
    comment = ( string "//" >>
                many (sat (/= '\n')) >>
                whitespace ) +++ return ()
    
    parens p =          --parse (parens nat) "(20)"
        symbol "(" >> 
        p >>= \res ->
        symbol ")" >>
        return res