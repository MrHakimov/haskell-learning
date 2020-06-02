import Data.Map as Map

dict =
	[("Python", "Guido van Rossum"),
	("C", "Dennis Ritchie"),
	("Haskell", "Haskell Curry")
	]

findKey :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey key [] = Nothing
findKey key ((k, v) : xs) = if key == k
	then Just v
	else findKey key xs

-- requires `import Data.Map`
-- Map.empty - empty map
-- Map.null mapa - checks `mapa` to emptiness
-- Map.size - size of the map
-- Map.insert key value mapa
-- Map.fromList - creates a map from a list

myMap = Map.fromList dict
-- Map.insert "php" "Rasmus Lerdorf" myMap
