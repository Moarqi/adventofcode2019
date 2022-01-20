main = do
    input_strings <- do
        contents <- readFile "input.txt"
        let strings = splitString contents []
        return strings

    let input_state = (map read input_strings)::[Int]
    final_state <- runIntCode 0 input_state

    print final_state

data Instr = Mul | Add | In | Out | End | JTrue | JFalse | JLt | JEq deriving (Eq, Show)
type State = [Int]

runIntCode:: Int -> State-> IO [Int]
runIntCode index state
    | state == []           = return []
    | instruction == End    = return state
    | instruction == Add    = runIntCode (index+4) (setEntry state third_arg (first_arg + second_arg))
    | instruction == Mul    = runIntCode (index+4) (setEntry state third_arg (first_arg * second_arg))
    | instruction == JTrue  = runIntCode (if first_arg /= 0 then second_arg else (index+3)) state
    | instruction == JFalse = runIntCode (if first_arg == 0 then second_arg else (index+3)) state
    | instruction == JLt    = runIntCode (index+4) (setEntry state third_arg (if first_arg < second_arg then 1 else 0))
    | instruction == JEq    = runIntCode (index+4) (setEntry state third_arg (if first_arg == second_arg then 1 else 0))
    | instruction == In     = do
        user_arg <- do
            arg <- getLine
            return (read arg)
        runIntCode (index+2) (setEntry state first_arg user_arg)
    | instruction == Out    = do
        print first_arg
        runIntCode (index+2) state
            where
                (instruction, params)               = parseInstruction index state
                (first_arg:second_arg:third_arg:[]) = params

-- instruction code -> State -> (instruction, resulting parameters)
parseInstruction:: Int -> State -> (Instr, [Int])
parseInstruction index state = (instruction, params)
    where
        instr       = state !! index
        opcode     = instr `mod` 100
        instruction = getInstruction opcode
        param_codes = instr `div` 100
        params      = getParams state index instruction param_codes

getInstruction:: Int -> Instr
getInstruction opcode
        | opcode == 1     = Add
        | opcode == 2     = Mul
        | opcode == 3     = In
        | opcode == 4     = Out
        | opcode == 99    = End
        | opcode == 5     = JTrue
        | opcode == 6     = JFalse
        | opcode == 7     = JLt
        | opcode == 8     = JEq

getParams:: State -> Int -> Instr -> Int -> [Int]
getParams state index instr param_codes
    | instr == In   = [state !! (index+1), -1, -1]
    | instr == Out = [
        if first_mode == 1 then state !! (index+1) else state !! (state !! (index+1)),
        -1,
        -1
    ]
    | instr == JTrue || instr == JFalse = [
        if first_mode == 1 then state !! (index+1) else state !! (state !! (index+1)),
        if second_mode == 1 then state !! (index+2) else state !! (state !! (index+2)),
        -1
    ]
    | instr == Add
        || instr == JLt
        || instr == JEq
            = [
            if first_mode == 1 then state !! (index+1) else state !! (state !! (index+1)),
            if second_mode == 1 then state !! (index+2) else state !! (state !! (index+2)),
            state !! (index+3)
        ]
    | instr == Mul  = [
        if first_mode == 1 then state !! (index+1) else state !! (state !! (index+1)),
        if second_mode == 1 then state !! (index+2) else state !! (state !! (index+2)),
        state !! (index+3)
    ]
    where
        first_mode  = param_codes `mod` 10
        second_mode = param_codes `div` 10 `mod` 10
        third_mode  = param_codes `div` 100

setEntry::[Int] -> Int -> Int -> [Int]
setEntry [] _ _             = []
setEntry state index value  = (take index state) ++ [value] ++ (drop (index+1) state)

splitString::[Char] -> [Char] -> [String]
splitString [] []       = []
splitString [] x        = [(reverse x)]
splitString (x:xs) c    =
    if x == ',' then (reverse c):splitString xs [] else splitString xs (x:c)