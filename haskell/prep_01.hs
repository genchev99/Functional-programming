import Data.List

main::IO()
main = do
  print(containsWord t1 "acd")
  print(containsWord t1 "cd")
  print(containsWord t1 "ac")
  print(containsWord t1 "")
  print(genWords t1)
  print(genWords t2)
  print(allContain [t1,t2])
  print(times ["asd", "asd", "1"] "asd")



data BTree = Empty | Node Char BTree BTree
                                                --    a
t1 = Node 'a' (Node 'c' (Node 'f' Empty Empty)  --   / \
                      (Node 'd' Empty Empty))   --  c   b
                              (Node 'b' Empty   -- / \   \
                      (Node 'e' Empty Empty))   -- f  d   e

                                                --    a
t2 = Node 'a' (Node 'c' (Node 'f' Empty Empty)  --   / \
                      (Node 'z' Empty Empty))   --  c   b
                              (Node 'b' Empty   -- / \   \
                      (Node 'e' Empty Empty))   -- f  z   e

isLeaf :: BTree -> Bool
isLeaf (Node _ Empty Empty) = True
isLeaf _ = False

containsWord :: BTree -> String -> Bool
containsWord (Node c Empty Empty) [x] = c == x -- if the current node is leaf and the string contains only one char
containsWord (Node _ Empty _) (_:_) = False -- if the string contains only one char but the node is not a leaf
containsWord (Node _ _ Empty) (_:_) = False -- if the string contains only one char but the node is not a leaf
containsWord (Node _ _ _) [] = False -- if the string is empty but the current node is not a leaf
containsWord Empty [x] = False
containsWord (Node c left right) (x:xs) 
  | x == c = containsWord left xs || containsWord right xs -- if the current node char is equal to the head of the string
  | otherwise = containsWord left (x:xs) || containsWord right (x:xs) -- otherwise is calling the function with the left and right subtrees


genWords :: BTree -> [String]
genWords Empty = [] -- Edge case empty tree
genWords (Node c Empty Empty) = [[c]] -- Leaf -> returns the char as a word
genWords (Node c left right)
  = let subwords = genWords left ++ genWords right
        max = maximum $ [length wrd | wrd <- subwords]
    in nub $ (map (\str -> c:str) (filter (\str -> length str == max) subwords)) ++ subwords -- returns all words without duplicates


-- Todo instead of checking the number of occurences check if all of the trees contain 
times :: [String] -> String -> Int
times items word = length $ filter (==word) items

allContain :: [BTree] -> [String]
allContain trees 
  = let gen = map genWords trees 
        concated = concat $ gen
    in [word | word <- (filter (\w -> all (elem w) gen) (nub concated))] -- var 1
    -- var 2
    -- in [word | word <- (nub concated), times concated word == length trees] -- since there are no duplicates we can check the times of each word occurence is equal to the number of trees