module Monads.Randoms where


type Seed = Int

nextInt :: Seed -> (Int, Seed)
nextInt s = (v, s2)
    where s2 = (s * 0x5DEECE66D + 0xB) & ((1 << 48) - 1)
          v = (s2 >> 16)
