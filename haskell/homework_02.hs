import Data.List
import Data.Char

-- Задача 1. Напишете функция rf :: (Int -> Int) -> (Int -> Int) -> ([Int] ->
-- (Int -> Int) -> [Int]), която има два аргумента - едноместните целочислени
-- функции f и g. Функцията rf трябва да върне нова двуаргументна функция с първи
-- аргумент - списък от цели числа ns и втори аргумент - едноместна целочислена функция
-- h. Върнатата функция трябва да върне списък със всички числа h(n), за които е вярно,
-- че n e от ns и f(n) > g(n).

-- Пример:
-- (rf ((-) (- 4)) (* (- 2))) [1..10] (* 3) → [15,18,21,24,27,30]

rf :: (Int -> Int) -> (Int -> Int) -> ([Int] -> (Int -> Int) -> [Int])
rf f g
    = \ ns h ->
        map h (filter (\ x -> f(x) > g(x)) ns)

-- Задача 2. Напишете функция calcFrequencyTable :: String -> [(Char,Int)],
-- която получава низ cs, състоящ се от символите a-z. Функцията трябва да върне списък
-- от двойки. Първият елемент на двойката трябва да е символ и да съответства на символ
-- от cs, а вторият да е число и да съответства на броя на срещанията на символа в cs.
-- Върнатият списък трябва да съдържа всички символи на cs, без повторение. Върнатият
-- списък трябва да е сортиран в низходящ ред по броя на срещанията. Ако има символи с
-- еднакъв брой срещания, то първа в списъка трябва да е двойката, чийто символ е с помалък ASCII код.
-- Примери:
-- calcFrequencyTable “ababac” → [(‘a’,3),(’b’,2),(‘c’,1)]
-- calcFrequencyTable “aaabbbc” → [(‘a’,3),(’b’,3),(‘c’,1)]
-- calcFrequencyTable “ababacccc” → [(‘c’,4),(’a’,3),(‘b’,2)]

calcFrequencyTable :: String -> [(Char,Int)]
calcFrequencyTable cs
    = helper
        where
            countChars :: Char -> Int
            countChars c = length $ filter (== c) cs

            makePair :: Char -> (Char,Int)
            makePair c = (c, countChars(c))

            unique :: String -> String
            unique [] = []
            unique (x:xs) = x:unique (filter ((/=) x) xs)

            sortUniq :: String -> String
            sortUniq str = unique (sort cs)

            helper :: [(Char,Int)]
            helper = sortBy (\ (a1, b1) (a2, b2) -> flip compare b1 b2) (map makePair (sortUniq cs))

-- Задача 3. Напишете функция expandString :: String -> String, която получава
-- низ, състоящ се от символите: a-z, цифрите: 0-9 и скобите: (, ). Тази функция връща
-- разширения на подадения низ. Един низ се разширява по следния начин:
-- - Ако съдържа само символите a-z, то разширеният низ е идентичен с подадения;
-- - Ако съдържа цифри, то те образуват съответно число в десетична бройна
-- система. Образуваното число винаги е последвано от отваряща скоба - (, която
-- има съответна правилно поставена затваряща скоба - ). Числото пред скобите
-- обозначава колко пъти трябва да се повтори в разширения низ съответната
-- разширена стойност на низа вътре в скобите;
-- - Разширеният низ се състои само от символите a-z.
-- Възможно е да има влагане на скобите, като задължително всяка отваряща скоба има
-- съответна затваряща скоба.
-- Примери:
-- еxpandString “abcd” → “abcd”
-- еxpandString “2(abc)” → “abcabc”
-- еxpandString “2(3(a)bc)” → “aaabcaaabc”
-- еxpandString “2(3(a)2(bc))” → “aaabcbcaaabcbc”


expandString :: String -> String
expandString [] = []
expandString inp
    = helper inp ""
        where
            multiplyStr :: String -> Int -> String
            multiplyStr str times = concat $ replicate times str

            getSubstring :: String -> String -> Int -> (String, String)
            getSubstring (x:xs) res matches
                | x == '(' = getSubstring xs (concat ([res, [x]])) (matches + 1)
                | x == ')' = if matches == 1
                    then (res, xs)
                    else getSubstring xs (concat ([res, [x]])) (matches - 1)
                | otherwise = getSubstring xs (concat ([res, [x]])) (matches)

            helper :: String -> String -> String
            helper [] res = res
            helper (x:xs) res
                = if x >= '0' && x <= '9'
                    then helper (concat([res, concat([(multiplyStr (tail (fst (getSubstring xs "" 0))) (digitToInt x)), (snd (getSubstring xs "" 0))])])) ""
                    else helper xs (concat([res, [x]]))

main :: IO()
main = do
    print((rf ((-) (- 4)) (* (- 2))) [1..10] (* 3))
    print(calcFrequencyTable "ababac")
    print(calcFrequencyTable "aaabbbc")
    print(calcFrequencyTable "ababacccc")
    print(expandString "2(ab3(d))bc")
    print(expandString "abcd")
    print(expandString "2(abc)")
    print(expandString "2(3(a)bc)")
    print(expandString "2(3(a)2(bc))")
