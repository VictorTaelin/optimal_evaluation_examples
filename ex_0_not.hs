-- Applies a function N times
app :: Int -> (a -> a) -> a -> a
app 0 f x = x
app n f x = f (app (n - 1) f x)

-- Boolean type
data B = T | F

-- Stringifies a boolean
str :: B -> String
str T = "T"
str F = "F"

-- Negates a boolean
neg :: B -> B
neg T = F
neg F = T

-- Applies 'neg' 100m times to True
main :: IO ()
main = print $ app 100_000_000 neg T
