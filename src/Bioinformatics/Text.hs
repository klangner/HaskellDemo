{--
    Bioinformatics. Text operations 
--}
module Bioinformatics.Text where


mostFreqWord :: String -> Int -> ([String], Int)
mostFreqWord xs l = ([], 0)


-- ----------------------------------------------------------------------------
-- Demo
-- ----------------------------------------------------------------------------

demo1 :: ([String], Int)
demo1 = mostFreqWord "ACAACTATGCATACTATCGGGAACTATCCT" 5
