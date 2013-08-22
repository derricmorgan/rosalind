type Location = Int

dna :: String -> String -> [Location]
dna ps xs = loop ps xs 1 where
  loop [] xs n = []
  loop (p:ps) xs n = case tryDNA (p:ps) xs of
    True -> n : loop ps xs (n+1)
    False -> loop ps xs (n+1)

tryDNA [] [] = True
tryDNA xs [] = True
tryDNA [] ps = False
tryDNA (p:ps) (x:xs) = p == x && tryDNA ps xs