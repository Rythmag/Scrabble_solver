string2int p = read p :: Int

fib p = if p == 0 then 0 else if p == 1 then 1 else fib (p-1) + fib (p-2)
fibonacci = [fib x | x<-[1..]]

checkDiv x n
    | x == n = True
    | x `mod` n == 0 = False
    |otherwise = checkDiv x (n+1)
isPrime x = (checkDiv x 2)
primes =[x | x<-[2..], isPrime x]

length' xs = fromIntegral (length xs)

subs xs ys
    |(length' xs) > (length' ys) = False
    |(take (length' xs) ys ) == xs = True
    |otherwise = subs xs (tail ys)
substrings xs xss = [ys | ys<-xss , subs xs ys]

subsets xs
    | length xs == 0 = [[]]
    -- | otherwise = [subsets (tail xs)] ++ [(head xs):subsets (tail xs)]