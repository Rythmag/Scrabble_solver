dict1 x
    | x == 0 = ""
    | x == 1 = "one"
    | x == 2 = "two"
    | x == 3 = "three"
    | x == 4 = "four"
    | x == 5 = "five"
    | x == 6 = "six"
    | x == 7 = "seven"
    | x == 8 = "eight"
    | x == 9 = "nine"

dict2 x
    | x == 10 = "ten"
    | x == 11 = "eleven"
    | x == 12 = "tweleve"
    | x == 13 = "thirteen"
    | x == 14 = "fourteen"
    | x == 15 = "fifteen"
    | x == 16 = "sixteen"
    | x == 17 = "seventeen" 
    | x == 18 = "eighteen"
    | x == 19 = "nineteen"

dict3 x
    | x < 30 = "twenty"
    | x < 40 = "thirty"
    | x < 50 = "fourty"
    | x < 60 = "fifty"
    | x < 70 = "sixty"
    | x < 80 = "seventy"
    | x < 90 = "eighty"
    | x < 100 = "ninety"

word x
    | x == 0 = "zero"
    | x < 10 = dict1 x
    | x < 20 = dict2 x
    | x < 100 = dict3 x ++ " " ++ dict1 (mod x 10)
    | mod x 100 == 0 = word (div x 100) ++ " hundred"
    | x < 1000 = word (div x 100) ++ " hundred and " ++ word (mod x 100)
    | mod x 1000 == 0 = word (div x 1000) ++ " thousand"
    | x < 1000000 = word (div x 1000) ++ " thousand, " ++ word(mod x 1000)
    | mod x 1000000 == 0 = word (div x 1000000) ++ " million"
    | otherwise = word (div x 1000000) ++ " million, " ++ word( mod x 1000000)