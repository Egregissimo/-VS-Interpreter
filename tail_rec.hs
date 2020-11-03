fac :: Int -> Int
fac 0 = 1
fac n = n * fac (n-1)

facAux :: Int -> Int -> Int
facAux 1 r = r
facAux n r = facAux (n - 1) ( r * n)

facTail :: Int -> Int
facTail n = facAux n 1

facTail2 :: Int -> Int
facTail2 n = facAux2 n 1
    where facAux2 c r
            | c == 1 = r
            | otherwise = facAux2 (c - 1) ( r * c)

facTail3 :: Int -> Int
facTail3 n = facAux3 1 1
    where facAux3 c r
            | c == n = ( r * c)
            | otherwise = facAux3 (c + 1) ( r * c)

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibTail :: Int -> Int
fibTail n = fibAux n 1 0
    where fibAux _ result previous
            | n == previous = (result + previous)
            | otherwise = fibAux (n-1) (previous + result) result