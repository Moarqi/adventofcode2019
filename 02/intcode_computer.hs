-- [1,9,10,3,2,3,11,0,99,30,40,50]

main = do
    input_strings <- do
        contents <- readFile "input.txt"
        let strings = splitString contents []
        return strings

    let input_state = (map read input_strings)::[Int]
    let input_state2 = setEntry input_state 1 12
    let input_state3 = setEntry input_state2 2 2
    print input_state3
    print (runIntCode 0 input_state3)

-- starting_index -> State
runIntCode:: Int -> [Int] -> [Int]
runIntCode index state
    | state == []       = []
    | instruction == 99 = state
    | instruction == 1  = runIntCode (index+4) (setEntry state result_index (first_arg + second_arg))  -- adding
    | instruction == 2  = runIntCode (index+4) (setEntry state result_index (first_arg * second_arg))  -- multiplication
        where
            instruction     = state !! index
            first_arg       = state !! (state !! (index+1))
            second_arg      = state !! (state !! (index+2))
            result_index    = state !! (index+3)

setEntry::[Int] -> Int -> Int -> [Int]
setEntry [] _ _             = []
setEntry state index value  = (take index state) ++ [value] ++ (drop (index+1) state)

splitString::[Char] -> [Char] -> [String]
splitString [] []       = []
splitString [] x        = [(reverse x)]
splitString (x:xs) c    =
    if x == ',' then (reverse c):splitString xs [] else splitString xs (x:c)