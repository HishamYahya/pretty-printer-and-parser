module Test_Generic where

scoretag = "### " -- Change to unique ID before automated marking, to prevent student code trying to confuse score parser

printSuccess name score | score>0 = do putStrLn ("OK " ++ scoretag ++ show score ++ " " ++ name); return score
                        | otherwise = do putStrLn ("OK " ++ name); return 0

printFailure name score | score<0 = do putStrLn ("FAIL " ++ scoretag ++ show score ++ " " ++ name); return score
                        | otherwise = do putStrLn ("FAIL " ++ name); return 0

printResult name score True = printSuccess name score
printResult name score False = printFailure name score

printTotalScore score = putStrLn $ "TOTAL " ++ scoretag ++ show score


