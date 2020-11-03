module Parser where
import AuxParser
import Type

{--
La grammatica data è:
t = n | var | t1 + t2 | t1 - t2 | t1 * t2 |
    if t0 then t1 else t2 | f_i(t1,...,ta_1)

Per rispettare le precedenze delle operazioni, può diventare:
expr = term + expr | term - minus | term
minus = term | term - minus  
term = factor (* term | e)
factor = n | var | func | (expr) | if expr then expr else expr

func = var(varn)
n = 0 | 1 | 2 | ...
varn = var (, varn | e)
var = "un qualsiasi carattere" varch
varch = "un qualsiasi carattere" | "una qualsiasi cifra" | _ | varch | e

(Es 1) L'input dell'interprete sarà del tipo:
f1(x1, x2) = x1, f2(x1) = x1 + 2, f3() = f3() + 1; 3 + f1(x1 + 2, y); x1 = 2, x2 = 3, y = undef;
L'output è 7. E' importante notare che "x1" nel termine si riferisce alla variabile
dichiarata successivamente, e non al parametro di f1, il cui scope è il corpo di f1.

(Es 2) Un altro termine potrebbe essere: ; 2 + 1; ;
Dove non sono definite funzioni o variabili

Dunque la grammatica per l'input è:
prog = funcn; expr; decn;
funcn = func (, funcn | e)
decn = dec (, decn | e)
dec = var = expr

L'interprete prima eseguirà un parser dell'input, e poi lo eseguirà. Nel caso di Es 1 il risultato del parser è:
([("f1", [(Evar "x1"), (Evar "x2")], (Evar "x1")), ("f2", [(Evar "x1")], (EOp (Evar "x1") PL (Enum (Just 2)))), ("f3", [], (EOp (EFunc "f3" []) P (ENum 1)))],
(EOp (ENum 3) P (EFunc "f1" [(EOp (EVar "x1") P (ENum 2)), (EVar "y")])), [("x1", (Just 2)), ("x2", (Just 3)), ("y", Nothing)]) 
--}

-- To handle errors
eval :: String -> Program
eval xs = case (parse parseProg xs) of
    [(n, [])] -> n
    [(_, out)] -> error ("Input non usato " ++ out)
    [] -> error ("Input on valido")

keyword :: String -> Bool
keyword key
    | key == "if" = True
    | key == "then" = True
    | key == "else" = True
    | key == "undef" = True
    | otherwise = False

-- Parse the entire program
parseProg :: Parser Program
parseProg = do
    fd <- many parseFuncDec
    symbol ";"
    e <- parseExpr
    symbol ";"
    vs <- many parseVarDec
    symbol ";"
    return (fd, e, vs)

-- Parse the function enviroment
parseFuncDec :: Parser FuncDec
parseFuncDec = do
    v <- parseVar
    symbol "("
    ps <- many parseParam
    symbol ")"
    symbol "="
    e <- parseExpr
    do
        symbol ","
        return (v, ps, e)
        <|> return (v, ps, e)

parseParam :: Parser Variable
parseParam = do
    v <- parseVar
    do
        symbol ","
        return v
        <|> return v

-- Parse the variable enviroment
parseVarDec :: Parser Def
parseVarDec = do
    v <- parseVar
    symbol "="
    n <- minteger
    do
        symbol ","
        return (v, n)
        <|> return (v, n)

parseVar :: Parser Variable
parseVar = do
    space
    var1 <- letter
    varn <- many alphanum
    space
    let var = var1:varn
        key = keyword var
        in if not(key)
            then return var
            else empty

-- Parse the term 
parseExpr :: Parser Expr 
parseExpr = do
    t <- parseTerm
    do
        symbol "+"
        e <- parseExpr 
        return (EOp t PL e)
        <|> do
            ts <- many (do
                symbol "-"
                parseTerm)
            return (foldl (\ acc x -> EOp acc MI x) t ts)

parseTerm :: Parser Expr
parseTerm = do
    f <- parseFactor
    symbol "*"
    t <- parseTerm
    return (EOp f ML t)
    <|> parseFactor

{-
E' importante che parseFunc sia eseguito prima di parseVar,
altrimenti una variabile potrebbe essere considerata una funzione.
-}
parseFactor :: Parser Expr
parseFactor =
    parseFunc
    <|> do
        v <- parseVar
        return (EVar v)
    <|> parseNum
    <|> do
        symbol "("
        e <- parseExpr
        symbol ")"
        return e
    <|> parseCond

parseNum :: Parser Expr
parseNum = do
    num <- integer
    return (ENum (Just num))
    <|> do
        symbol "undef"
        return (ENum Nothing)

parseCond :: Parser Expr
parseCond = do
    symbol "if"
    t0 <- parseExpr
    symbol "then"
    t1 <- parseExpr
    symbol "else"
    t2 <- parseExpr
    return (ECond t0 t1 t2)

parseFunc :: Parser Expr
parseFunc = do
    v <- parseVar
    symbol "("
    ps <- many parseParamExpr
    symbol ")"
    return (EFunc v ps)

parseParamExpr :: Parser Expr
parseParamExpr = do
    e <- parseExpr
    do
        symbol ","
        return e
        <|> return e