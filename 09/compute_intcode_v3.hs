main = do
    input_strings <- do
        contents <- readFile "input.txt"
        let strings = splitString contents []
        return strings

    let input_state = (map read input_strings)::[Int]
    final_state <- runIntCode 0 0 input_state

    print final_state

data Instr = Mul | Add | In | Out | End | JTrue | JFalse | JLt | JEq | SetBase deriving (Eq, Show)
type State = [Int]

runIntCode:: Int -> Int -> State-> IO [Int]
runIntCode index relative_base state = do
    if state == []                  then return []
    else if instruction == End      then return state
    else if instruction == Add      then runIntCode (index+4) relative_base (setEntry state third_arg (first_arg + second_arg))
    else if instruction == Mul      then runIntCode (index+4) relative_base (setEntry state third_arg (first_arg * second_arg))
    else if instruction == JTrue    then runIntCode (if first_arg /= 0 then second_arg else (index+3)) relative_base state
    else if instruction == JFalse   then runIntCode (if first_arg == 0 then second_arg else (index+3)) relative_base state
    else if instruction == JLt      then runIntCode (index+4) relative_base (setEntry state third_arg (if first_arg < second_arg then 1 else 0))
    else if instruction == JEq      then runIntCode (index+4) relative_base (setEntry state third_arg (if first_arg == second_arg then 1 else 0))
    else if instruction == SetBase  then runIntCode (index+2) (relative_base+first_arg) state
    else if instruction == In       then do
        user_arg <- do
            arg <- getLine
            return (read arg)
        runIntCode (index+2) relative_base (setEntry state first_arg user_arg)
    else if instruction == Out      then do
        print first_arg
        runIntCode (index+2) relative_base state
    else do
        print "unknown instruction"
        return []
            where
                (instruction, params)               = parseInstruction index state relative_base
                (first_arg:second_arg:third_arg:[]) = params

-- instruction code -> State -> (instruction, resulting parameters)
parseInstruction:: Int -> State -> Int -> (Instr, [Int])
parseInstruction index state base = (instruction, params)
    where
        instr       = state !! index
        opcode      = instr `mod` 100
        instruction = getInstruction opcode
        param_codes = instr `div` 100
        params      = getParams state index instruction param_codes base

getInstruction:: Int -> Instr
getInstruction opcode
        | opcode == 1   = Add
        | opcode == 2   = Mul
        | opcode == 3   = In
        | opcode == 4   = Out
        | opcode == 99  = End
        | opcode == 5   = JTrue
        | opcode == 6   = JFalse
        | opcode == 7   = JLt
        | opcode == 8   = JEq
        | opcode == 9   = SetBase

getParams:: State -> Int -> Instr -> Int -> Int -> [Int]
getParams state index instr param_codes rel_base
    | instr == In                       = [
        getParameter state (index+1) first_mode rel_base False,
        -1,
        -1
    ]
    | instr == SetBase                  = [
        if first_mode == 2 then getValue state (rel_base + (getValue state (index+1)))
        else if first_mode == 1 then getValue state (index+1)
        else getValue state (getValue state (index+1)),
        -1,
        -1
    ]
    | instr == Out                      = [
        getParameter state (index+1) first_mode rel_base True,
        -1,
        -1
    ]
    | instr == JTrue || instr == JFalse = [
        getParameter state (index+1) first_mode rel_base True,
        getParameter state (index+2) second_mode rel_base True,
        -1
    ]
    | instr == Add                     = [
        getParameter state (index+1) first_mode rel_base True,
        getParameter state (index+2) second_mode rel_base True,
        getParameter state (index+3) third_mode rel_base False
    ]
    | instr == JLt || instr == JEq     = [
            getParameter state (index+1) first_mode rel_base True,
            getParameter state (index+2) second_mode rel_base True,
            getParameter state (index+3) third_mode rel_base False
        ]
    | instr == Mul                      = [
        getParameter state (index+1) first_mode rel_base True,
        getParameter state (index+2) second_mode rel_base True,
        getParameter state (index+3) third_mode rel_base False
    ]
    where
        first_mode  = param_codes `mod` 10
        second_mode = param_codes `div` 10 `mod` 10
        third_mode  = param_codes `div` 100

getParameter:: State -> Int -> Int -> Int -> Bool -> Int
getParameter state index mode base get_value =
    if mode == 1 then getValue state index
    else if mode == 2 then if get_value then getValue state (base + (getValue state index)) else base + (getValue state index)
    else if get_value then getValue state (getValue state index) else getValue state index

getValue:: State -> Int -> Int
getValue state index = if index >= length state then 0 else state !! index

setEntry::[Int] -> Int -> Int -> [Int]
setEntry [] _ _             = []
setEntry state index value
    | index >= (length state)   = (take index state) ++ (take (index-(length state)) (repeat 0)) ++ [value]
    | otherwise                 = (take index state) ++ [value] ++ (drop (index+1) state)

splitString::[Char] -> [Char] -> [String]
splitString [] []       = []
splitString [] x        = [(reverse x)]
splitString (x:xs) c    =
    if x == ',' then (reverse c):splitString xs [] else splitString xs (x:c)