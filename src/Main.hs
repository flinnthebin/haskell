import Data.List (subsequences)
import Data.Char (toLower)

-- Function to check if a character is a vowel
isVowel :: Char -> Bool
isVowel c = toLower c `elem` "aeiou"

-- Function to strip vowels and return the remaining consonants
stripVowels :: String -> [Char]
stripVowels = filter (not . isVowel)

-- Function to calculate the hash strength
hashStrength :: [Char] -> Int
hashStrength chars = length $ filter (not . null) $ subsequences chars

-- Main function to process the input string
processString :: String -> Int
processString input = hashStrength $ stripVowels input

-- Example usage
main :: IO ()
main = do
    let input1 = "baa"
    let input2 = "abc"
    print $ processString input1 -- Output: 1
    print $ processString input2 -- Output: 3
