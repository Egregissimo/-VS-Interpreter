module Interpreter where
import Type
import AuxInterpreter

-- Get the parsed program and return the result of the term
interpreter :: Program -> Int
interpreter input = interpreter' 0 input

-- Use the tail recursive technique
interpreter' :: Int -> Program -> Int
interpreter' n input = let findF = findFix input
    in case (findF n) of
        Just n -> n
        Nothing -> interpreter' (n + 1) input

-- Find fix points of FEnv whether they are request
findFix :: Program -> Int -> Maybe Int
findFix (funcn, t, decn) = \k -> let fix = rho (functional funcn decn) k [bottom]
                                    in valueExpr (fix, t, decn)

--n indica il livello d'innesto delle chiamate di funzione
-- Use the tail recursive technique
rho :: (FEnv -> FEnv) -> Int -> (FEnv -> FEnv)
rho _ 0 = id
rho f k = \funcn -> rho f (k - 1) (f funcn)

functional :: [FuncDec] -> VEnv -> (FEnv -> FEnv)
functional [] _ = \_ -> []
functional ((name, params, exp):fs) venv = \fenv ->
    (name, (\inp -> valueExpr (fenv, exp, (replaceVars venv params inp)))) : (functional fs venv fenv)

-- EVALUATION

valueExpr :: ProgramParsed -> Maybe Int
valueExpr (funcn, expr, decn) = case expr of
    (EVar v)            -> valueVar decn v
    (ENum n)            -> n
    (EOp t1 op t2)      -> valueOp op (valueExpr (funcn, t1, decn)) (valueExpr (funcn, t2, decn))
    (ECond t0 t1 t2)    -> valueCond (valueExpr (funcn, t0, decn)) (valueExpr (funcn, t1, decn)) (valueExpr (funcn, t2, decn))
    (EFunc f params)    -> let (_, fun) = getFunc funcn f
                            in fun (valueParams funcn decn params)

valueVar :: [Def] -> Variable -> Maybe Int
valueVar [] _ = Nothing
valueVar ((dec, n):ds) var
    | dec == var    = n
    | otherwise     = valueVar ds var

valueOp :: Op -> Maybe Int -> Maybe Int -> Maybe Int
valueOp op x y = case op of
    PL -> case x of
        Just n1 -> case y of
            Just n2 -> Just (n1 + n2)
            _ -> Nothing
        _ -> Nothing
    MI -> case x of
        Just n1 -> case y of
            Just n2 -> Just (n1 - n2)
            _ -> Nothing
        _ -> Nothing
    ML -> case x of
        Just n1 -> case y of
            Just n2 -> Just (n1 * n2)
            _ -> Nothing
        _ -> Nothing

valueCond :: Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int
valueCond t0 t1 t2 = case t0 of
    Just f -> if f == 0
        then t1
        else t2
    _ -> Nothing

valueParams :: FEnv -> VEnv -> [Expr] -> [Maybe Int]
valueParams _ _ [] = []
valueParams funcn decn (p:ps) = (valueExpr (funcn, p, decn)) : valueParams funcn decn ps