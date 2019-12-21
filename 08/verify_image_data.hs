import System.IO

main = do
    contents <- readFile "input.txt"
    let layers          = getDataLayers contents 25 6 []
    -- let layer_data      = map getLayerData layers
    -- let (_, one, two)   = minimum layer_data
    let final = parseLayers layers

    outputLayer final

type Width  = Int
type Height = Int

getDataLayers:: String -> Width -> Height -> [Int] -> [[Int]]
getDataLayers [] _ _ _                  = []
getDataLayers (x:xs) width height temp  =
    if length layer >= width*height
        then (reverse layer):getDataLayers xs width height []
        else getDataLayers xs width height layer
        where
            layer       = (parsed_int:temp)
            parsed_int  = read [x]

parseLayers:: [[Int]] -> [Int]
parseLayers layers = [px_value index | index <- [0..(length (head layers) -1)]]
    where
        px_value index = getFirstNot (map (\xs -> if (xs !! index) /= 2 then (xs !! index) else (-1)) layers) (-1)

outputLayer:: [Int] -> IO ()
outputLayer [] = print '\n'
outputLayer xs = do
    do
        let chars = map (\x -> if x == 1 then '1' else ' ') (take 25 xs)
        mapM putChar chars
    print '\n'
    outputLayer (drop 25 xs)

getFirstNot:: [Int] -> Int -> Int
getFirstNot [] _ = -1
getFirstNot (x:xs) notThis
        | x == notThis  = getFirstNot xs notThis
        | otherwise     = x


getLayerData:: [Int] -> (Int, Int, Int)
getLayerData xs = (num_zero, num_one, num_two)
            where
                num_zero = sum (map (\x -> if x == 0 then 1 else 0) xs)
                num_one = sum (map (\x -> if x == 1 then 1 else 0) xs)
                num_two = sum (map (\x -> if x == 2 then 1 else 0) xs)