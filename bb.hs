import Control.Monad
import Data.Char
import Data.Bits

scale_ratios = [(1,1), (17,16), (9,8), (19,16), (5,4), (21,16), (11,16), (3,2), (25,16), (13,8), (27,16), (7,4), (15,8)]

scale t i = if (t < 13) then (top * i) `quot` bottom else 2 * scale (t-13) i
        where 
              tup = scale_ratios !! t
              top = fst tup
              bottom = snd tup

every t i e1 e2 = if testBit i t then e1 else e2 

repeat_fast i = every 9 i 0 6

g1 t = (2 * t) `xor` ((2 * t) + (t `quot` 128) .&. (t `quot` 4096)) 

g2 t = t `shiftR` (4 - (1  `xor` (7 .&. (t `shiftR` 19))))

g3 t = t `shiftR` 7

f :: Int -> Int
f i = (g1 i) .|. (g2 i) .|. (g3 i) 

pc i = do
    putChar $ chr $ (f i) `mod` 256
    pc (i+1) 

main = do
    pc 0 
