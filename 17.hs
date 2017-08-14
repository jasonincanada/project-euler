-- project-euler.net problem 17
-- Jason Hooper

c :: Int -> String
c x
	| x >= 1000 = "onethousand" ++ c (x-1000)
	| x >= 901 = "ninehundredand" ++ c (x-900)
	| x >= 900 = "ninehundred"
	| x >= 801 = "eighthundredand" ++ c (x-800)
	| x >= 800 = "eighthundred"
	| x >= 701 = "sevenhundredand" ++ c (x-700)
	| x >= 700 = "sevenhundred"
	| x >= 601 = "sixhundredand" ++ c (x-600)
	| x >= 600 = "sixhundred"
	| x >= 501 = "fivehundredand" ++ c (x-500)
	| x >= 500 = "fivehundred"
	| x >= 401 = "fourhundredand" ++ c (x-400)
	| x >= 400 = "fourhundred"
	| x >= 301 = "threehundredand" ++ c (x-300)
	| x >= 300 = "threehundred"
	| x >= 201 = "twohundredand" ++ c (x-200)
	| x >= 200 = "twohundred"
	| x >= 101 = "onehundredand" ++ c (x-100)
	| x >= 100 = "onehundred"
	| x >= 90 = "ninety" ++ c (x-90)
	| x >= 80 = "eighty" ++ c (x-80)
	| x >= 70 = "seventy" ++ c (x-70)
	| x >= 60 = "sixty" ++ c (x-60)
	| x >= 50 = "fifty" ++ c (x-50)
	| x >= 40 = "forty" ++ c (x-40)
	| x >= 30 = "thirty" ++ c (x-30)
	| x >= 20 = "twenty" ++ c (x-20)
	| x >= 19 = "nineteen"
	| x >= 18 = "eighteen"
	| x >= 17 = "seventeen"
	| x >= 16 = "sixteen"
	| x >= 15 = "fifteen"
	| x >= 14 = "fourteen"
	| x >= 13 = "thirteen"
	| x >= 12 = "twelve"
	| x >= 11 = "eleven"
	| x >= 10 = "ten"
	| x >= 9 = "nine"
	| x >= 8 = "eight"
	| x >= 7 = "seven"
	| x >= 6 = "six"
	| x >= 5 = "five"
	| x >= 4 = "four"
	| x >= 3 = "three"
	| x >= 2 = "two"
	| x >= 1 = "one"
	| otherwise = ""

numbers :: [String]
numbers = [ c i | i <- [1..1000] ]

p17 :: Int
p17 = sum $ map length numbers

