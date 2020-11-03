module AuxInterpreter where
import Type

replaceVars :: VEnv -> [Variable] -> [Maybe Int] -> VEnv
replaceVars venv [] [] = venv
replaceVars venv (v:vs) (n:ns) = replaceVars (replaceVar venv v n) vs ns

replaceVar :: VEnv -> Variable -> Maybe Int -> VEnv
replaceVar [] v n = [(v, n)]
replaceVar ((name, val):decn) v n
    | name == v = (name, n) : decn 
    | otherwise = (name, val) : replaceVar decn v n

bottom :: Func
bottom = ("bottom", \_ -> Nothing)

getFunc :: FEnv -> Variable -> Func
getFunc [] _ = bottom
getFunc ((n, f):fs) name 
    | name == n = (n, f)
    | otherwise = getFunc fs name