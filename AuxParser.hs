module AuxParser where
import Data.Char

newtype Parser a = P (String -> [(a,String)])

--MONADS

instance Functor Parser where
    -- fmap :: (a -> b) -> Parser a -> Parser b
    fmap g p = P (\inp -> case parse p inp of
        [] -> []
        [(v,out)] -> [(g v, out)])

instance Applicative Parser where
    -- pure :: a -> Parser a
        pure v = P (\inp -> [(v,inp)])
    
    -- <*> :: Parser (a -> b) -> Parser a -> Parser b
        pg <*> px = P (\inp -> case parse pg inp of
            [] -> []
            [(g,out)] -> parse (fmap g px) out)

instance Monad Parser where
    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    p >>= f = P (\inp -> case parse p inp of
        [] -> []
        [(v,out)] -> parse (f v) out)

--ALTERNATIVES

class Applicative f => Alternative f where
    -- f a = un'istanza di applicative, in questo caso Parser, di tipo a
    empty :: f a
    -- viene ritornato il primo argomento se questo ha successo,
    -- altrimenti il secondo
    (<|>) :: f a -> f a -> f a
    -- esegue il parser finché non fallisce. Permette zero o più applicazioni
    many :: f a -> f [a] 
    many x = some x <|> pure [] 
    -- esegue il parser finché non fallisce. Impone almeno un'applicazione
    some :: f a -> f [a] 
    some x = pure (:) <*> x <*> many x

instance Alternative Parser where
    -- empty :: Parser a
    empty = P (\_ -> [])
    
    -- (<|>) :: Parser a -> Parser a -> Parser a
    p <|> q = P (\inp -> case parse p inp of
        [] -> parse q inp
        [(v,out)] -> [(v,out)])

-- EXECUTE PARSER

parse :: Parser a -> String -> [(a,String)]
parse (P p) inp = p inp

--AUXILIAR PARSERS

item :: Parser Char
item = P (\inp -> case inp of
    [] -> []
    (x:xs) -> [(x,xs)])

-- Guarda se il primo elemento rispetta la funzione scelta
sat :: (Char -> Bool) -> Parser Char 
sat p = do
    x <- item 
    if p x then return x else empty

-- Simple parser

digit :: Parser Char 
digit = sat isDigit

char :: Char -> Parser Char 
char x = sat (== x)

letter :: Parser Char 
letter = sat isAlpha

alphanum :: Parser Char 
alphanum = sat isAlphaNum
    <|> char '_'

nat :: Parser Int 
nat = do
    xs <- some digit 
    return (read xs)

mnat :: Parser (Maybe Int) 
mnat = do
    xs <- some digit 
    return (Just (read xs))
    <|> do
        string "undef"
        return Nothing

int :: Parser Int
int = do
    char '-'
    n <- nat
    return (-n)
    <|> nat

mint :: Parser (Maybe Int)
mint = do
    char '-'
    n <- mnat
    case n of
        (Just n1) -> return (Just (-n1))
        Nothing -> return Nothing
    <|> mnat

string :: String -> Parser String 
string [] = return [] 
string (x:xs) = do
    char x 
    string xs
    return (x:xs)

-- Space parsers

space :: Parser () 
space = do
    many (sat isSpace) 
    return ()

token :: Parser a -> Parser a 
token p = do
    space 
    v <- p 
    space 
    return v

-- Recursive parsers with spaces

integer :: Parser Int 
integer = token int

symbol :: String -> Parser String 
symbol xs = token (string xs)

natural :: Parser Int
natural = token nat

minteger :: Parser (Maybe Int)
minteger = token mint