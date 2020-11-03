module Type where

-- PARSER TYPES

type Variable = String

data Expr
    = EVar Variable
    | ENum (Maybe Int)
    | ECond
        Expr
        Expr
        Expr
    | EOp
        Expr
        Op
        Expr
    | EFunc
        Variable
        [Expr]
    deriving Show

data Op = PL | MI | ML
    deriving Show

type Def = (Variable, (Maybe Int))
type VEnv = [Def]

type FuncDec= (Variable, [Variable], Expr)

type Program = ([FuncDec], Expr, VEnv)

-- INTERPRETER TYPES

type ProgramParsed = (FEnv, Expr, VEnv)

type Func = (Variable, [Maybe Int] -> Maybe Int)
type FEnv = [Func] 