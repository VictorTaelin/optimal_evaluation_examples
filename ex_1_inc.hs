-- Bitstring type
data Bits = O Bits | I Bits | E

-- Stringifies a bitstring
str :: Bits -> String
str (O xs) = 'O' : str xs
str (I xs) = 'I' : str xs
str E      = "E"

-- Applies a function N times
rep :: Int -> (a -> a) -> a -> a
rep 0 f x = x
rep n f x = f (rep (n - 1) f x)

-- Increments a bitstring
inc :: Bits -> Bits
inc (O x) = (I x)
inc (I x) = (O (inc x))
inc E     = E

-- Bitstring with all 0's
zeros :: Int -> Bits
zeros 0 = E 
zeros n = O (zeros (n - 1))

-- Increments a bitstring 100m times
main :: IO ()
main = putStrLn $ str $ rep 100_000_000 inc (zeros 64)
