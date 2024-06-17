import Text.Read (readMaybe)

calculateVectorLength :: [Double] -> Double
calculateVectorLength vector = sqrt (sum (map (^2) vector))

populateVector :: [String] -> IO [String]
populateVector vector = do
    putStrLn "Enter a value. Type \"d\" when you are finished populating the vector."
    value <- getLine

    if value == "d"
        then return vector
    else case readMaybe value :: Maybe Double of
        Just _ -> populateVector (vector ++ [value])
        Nothing -> do
            putStrLn "Sorry, the input you provided is invalid. Please try again."
            populateVector vector

main :: IO ()
main = do
    vector <- populateVector []
    let convertedVector = map read vector :: [Double]

    let vectorLength = calculateVectorLength convertedVector
    putStrLn $ "The length of vector " ++ show vector ++ " is " ++ show vectorLength ++ "."
