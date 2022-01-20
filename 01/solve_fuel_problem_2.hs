import System.IO

main = do
    input_strings <- do
        contents <- readFile "input.txt"
        let strings = splitString contents []
        return strings

    let input_integer = map read input_strings
    let fuel = sum [recGetFuelAmount x | x <- input_integer]
    print fuel

splitString::[Char] -> [Char] -> [String]
splitString [] []       = []
splitString [] x        = [(reverse x)]
splitString (x:xs) c    =
    if x == '\n' then (reverse c):splitString xs [] else splitString xs (x:c)

recGetFuelAmount:: Integer -> Integer
recGetFuelAmount mass = if fuel_amount <= 0 then 0 else fuel_amount + (recGetFuelAmount fuel_amount)
    where fuel_amount = (mass `div` 3) - 2