main = do
    input_strings <- do
        contents <- readFile "input.txt"
        let wires = splitString contents '\n' []
        let instructions = [splitString xs ',' "" | xs <- wires]
        let instruction_tuples = map getInstructionTuples instructions
        return instruction_tuples

    let paths = map (getPath (0,0)) input_strings
    let intersections = getJustIntersections (getIntersections (paths !! 0) (paths !! 1))
    let path_length = map (flip getAllPathLengths paths) intersections
    print (minimum path_length)

getAllPathLengths:: (Int, Int) -> [[((Int, Int), (Int, Int))]] -> Int
getAllPathLengths goal paths = sum (map (getPathLength goal) paths)

getPathLength:: (Int, Int) -> [((Int, Int), (Int, Int))] -> Int
getPathLength _ []                = 0
getPathLength goal ((point, vec):xs)   =
    if goal `elem` range then steps_to_goal else steps + getPathLength goal xs
        where
            steps_to_goal   = (abs ((fst goal) - (fst point))) + (abs ((snd goal) - (snd point)))
            next_point      = point `addTuple` vec
            range           = [
                (x, y) |
                x <- [minimum [fst point, fst next_point]..maximum [fst point, fst next_point]],
                y <- [minimum [snd point, snd next_point]..maximum [snd point, snd next_point]]
                ]
            steps           = (abs (fst vec)) + (abs (snd vec))

getJustIntersections:: [Maybe (Int, Int)] -> [(Int, Int)]
getJustIntersections [] = []
getJustIntersections ((Nothing):xs) = getJustIntersections xs
getJustIntersections ((Just p): xs) = p:getJustIntersections xs

getDistances:: [Maybe (Int, Int)] -> [Int]
getDistances []                 = []
getDistances ((Nothing):xs)     = getDistances xs
getDistances ((Just (x, y)):xs) = ((abs x) + (abs y)):getDistances xs

getIntersections::[((Int, Int), (Int, Int))] -> [((Int, Int), (Int, Int))] -> [Maybe (Int, Int)]
getIntersections [] _           = []
getIntersections _ []           = []
getIntersections path_a path_b  = [intersection | a <- path_a, b <- path_b, let intersection = getIntersection a b, intersection /= Nothing]

-- getIntersection:: ((Int, Int), (Int, Int)) -> ((Int, Int), (Int, Int)) -> Maybe (Int, Int)
-- getIntersection (start_a, next_a) (start_b, next_b) =
--     if length intersections == 0 then Nothing else Just (intersections !! 0)
--         where intersections = [
--                         (x, y) |
--                         (a_x, a_y) <- getTupleRange start_a next_a,
--                         (b_x, b_y) <- getTupleRange start_b next_b,
--                         a_x == b_x && a_y == b_y,
--                         let x = a_x,
--                         let y = a_y,
--                         x /= 0 || y /= 0
--                         ] -- TODO solve this explicitly!

-- I do assume that only lines in x and y direction (or vice versa) can intersect
getIntersection:: ((Int, Int), (Int, Int)) -> ((Int, Int), (Int, Int)) -> Maybe (Int, Int)
getIntersection (start_a, vec_a) (start_b, vec_b) =
    if (fst vec_a == 0 && fst vec_b /= 0)
        then getIntersectionCaseOne (start_a, vec_a) (start_b, vec_b)
    else
        if (fst vec_a /= 0 && fst vec_b == 0)
            then getIntersectionCaseTwo (start_a, vec_a) (start_b, vec_b)
        else
            Nothing

getIntersectionCaseOne::((Int, Int), (Int, Int)) -> ((Int, Int), (Int, Int)) ->  Maybe (Int, Int)
getIntersectionCaseOne (start_a, vec_a) (start_b, vec_b) =
    if isValid then Just (x, y) else Nothing
        where
            x_a     = (snd start_b) - (snd start_a)
            x_b     = -((fst start_b) - (fst start_a))
            isValid = (abs x_a) `elem` [0..(abs (snd vec_a))]
                && (abs x_b) `elem` [0..(abs (fst vec_b))]
                && x_a * (snd vec_a) > 0 && x_b * (fst vec_b) > 0
            y       = (snd start_a) + x_a
            x       = fst start_a

getIntersectionCaseTwo::((Int, Int), (Int, Int)) -> ((Int, Int), (Int, Int)) ->  Maybe (Int, Int)
getIntersectionCaseTwo (start_a, vec_a) (start_b, vec_b) =
    if isValid then Just (x, y) else Nothing
        where
            x_a     = (fst start_b) - (fst start_a)
            x_b     = -((snd start_b) - (snd start_a))
            isValid = (abs x_a) `elem` [0..(abs (fst vec_a))]
                && (abs x_b) `elem` [0..(abs (snd vec_b))]
                && x_a * (fst vec_a) > 0 && x_b * (snd vec_b) > 0
            y       = snd start_a
            x       = (fst start_a) + x_a

-- getTupleRange:: (Int, Int) -> (Int, Int) -> [(Int, Int)]
-- getTupleRange (a, b) (c, d) = [
--                     (x, y) |
--                     x <- [x_min..x_max],
--                     y <- [y_min..y_max]
--                     ]
--                         where
--                             x_min = minimum [a, c]
--                             y_min = minimum [b, d]
--                             x_max = maximum [a, c]
--                             y_max = maximum [b, d]



getInstructionTuples::[String] -> [(Int, Int)]
getInstructionTuples [] = []
getInstructionTuples xs = [(x, y) | inst <- xs, let (x, y) = parseInstruction inst]

getPath::(Int, Int) -> [(Int, Int)] -> [((Int, Int), (Int, Int))]
getPath _ []            = []
getPath start (x:xs)    = (start, x):getPath next_start xs
        where next_start = start `addTuple` x

addTuple::(Int, Int) -> (Int, Int) -> (Int, Int)
addTuple (a, b) (c, d) = (a+c, b+d)

subTuple::(Int, Int) -> (Int, Int) -> (Int, Int)
subTuple (a, b) (c, d) = (a-c, b-d)

parseInstruction::String -> (Int, Int)
parseInstruction "" = (0,0)
parseInstruction (dir:xs)
        | dir == 'R' = (val, 0)
        | dir == 'L' = (-val, 0)
        | dir == 'U' = (0, val)
        | dir == 'D' = (0, -val)
            where val = read xs

splitString::[Char] -> Char -> [Char] -> [String]
splitString [] _ []             = []
splitString [] _ x              = [reverse x]
splitString (x:xs) char temp =
    if x == char then (reverse temp):splitString xs char [] else splitString xs char (x:temp)