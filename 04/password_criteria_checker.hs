main = do
    contents <- readFile "input.txt"
    let range = getRangeFromInput contents
    print (length (filterRange range))

filterRange::[Int] -> [Int]
filterRange []      = []
filterRange (x:xs)  =
    if meetsPwdCriteria x
        then x:filterRange xs
        else filterRange xs

meetsPwdCriteria:: Int -> Bool
meetsPwdCriteria x = any (\x -> x == 2) (map length (getRegions [] digits)) && isAscending digits
    where
        digits = getDigits x

hasTuple:: Eq a => [a] -> Bool
hasTuple (a:b:c:d:[])     = False
hasTuple (a:b:c:d:xs)   = a == b || hasTuple (b:xs)

getRegions:: Eq a => [a] -> [a] -> [[a]]
getRegions region (x:[]) = [x:region]
getRegions region (x:y:xs) = if x == y then getRegions (x:region) (y:xs) else (x:region):getRegions [] (y:xs)

isAscending:: [Int] -> Bool
isAscending (a:[])      = True
isAscending (a:b:xs)    = a <= b && isAscending (b:xs)

getDigits:: Int -> [Int]
getDigits = map (read . return) . show

getRangeFromInput:: String -> [Int]
getRangeFromInput input = [read (split !! 0)..read (split !! 1)]
    where split = splitString input '-' []

splitString::[Char] -> Char -> [Char] -> [String]
splitString [] _ []             = []
splitString [] _ x              = [reverse x]
splitString (x:xs) char temp =
    if x == char then (reverse temp):splitString xs char [] else splitString xs char (x:temp)