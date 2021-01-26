import Data.Char (digitToInt, isDigit) 

decode :: String -> String
decode [] = []
decode encodedText = decodeFolder encodedText 0

decodeFolder :: String -> Int -> String
decodeFolder [] _ = []
decodeFolder (x:xs) 0 -- В случайте, когато все още не сме прочели ново число или вече сме срещнали буква, която сме репликирали
  | isDigit x = decodeFolder xs (digitToInt x) -- Ако прочетем число извикваме същата функция но с това чиско като аргумент и поради това ще попадне във 2рия патърн
  | otherwise = x:decodeFolder xs 0 -- Ако прочетем символ [различен от число] (и не сме прочели число преди това) само го запазваме
decodeFolder (x:xs) timesToReplicate -- В случайте, когато вече сме прочели едно числи и не сме прочели (все още) буква след него
  | isDigit x = decodeFolder xs (10 * timesToReplicate + digitToInt x) -- Ако отново прочетем число го 'стакваме'
  | otherwise = replicate timesToReplicate x ++ decodeFolder xs 0 -- Ако прочетем символ го репликираме нужните брой пъти


encode :: String -> String
encode [] = []
encode string = encodeFolder string 0

customShow :: Char -> Int -> String 
customShow _ 0 = ""
customShow x 1 = [x]
customShow x 2 = [x, x]
customShow x times = show times ++ [x]

encodeFolder :: String -> Int -> String 
encodeFolder [] _ = []
encodeFolder [x] times = customShow x (times + 1)
encodeFolder (x:y:xs) times 
  | x == y = encodeFolder (y:xs) (times + 1)
  | otherwise = customShow x (times + 1) ++ encodeFolder (y:xs) 0 


main = do 
  --  print(decode "bb10abb")    --calling a function 
  --  print(decode "12a3b")    --calling a function 
  --  print(decode "a3b")    --calling a function 
  --  print(decode "aa3b")    --calling a function  
   print(encode "Haskell")    --calling a function
   print(encode "aaabccdefff")    --calling a function
   print(encode "aaaaaaaaaaaabbb")    --calling a function
   print(encode "aabbb")    --calling a function