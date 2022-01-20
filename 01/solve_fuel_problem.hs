import System.IO
import System.Environment

main = do
    input_strings <- do
        args <- getArgs
        contents <- readFile (head args)
        let strings = splitString contents []
        return strings

    let input_integer = map read input_strings
    let fuel = sum [(x `div` 3) - 2 | x <- input_integer]
    let rec_fuel = sum [recGetFuelAmount x | x <- input_integer]
    putStr "solution day 1 part 1: "
    print fuel
    putStr "solution day 1 part 2: "
    print rec_fuel


splitString::[Char] -> [Char] -> [String]
splitString [] []       = []
splitString [] x        = [(reverse x)]
splitString (x:xs) c    =
    if x == '\n' then (reverse c):splitString xs [] else splitString xs (x:c)

recGetFuelAmount:: Integer -> Integer
recGetFuelAmount mass = if fuel_amount <= 0 then 0 else fuel_amount + (recGetFuelAmount fuel_amount)
    where fuel_amount = (mass `div` 3) - 2
