-- projecteuler.net Problem 54 - Poker hands
--
-- So far detects: straight, flush, royal flush, 4 of a kind

{-
  Main> deck
  [2C,3C,4C,5C,6C,7C,8C,9C,TC,JC,QC,KC,AC,2D,3D,4D,5D,6D,7D,8D,9D,TD,JD,QD,KD,AD,2H,3H,4H,5H,6H,7H,8H,9H,TH,JH,QH,KH,AH,2S,3S,4S,5S,6S,7S,8S,9S,TS,JS,QS,KS,AS]
  
  Main> take 5 deck
  [2C,3C,4C,5C,6C]
  
  Main> isFlush $ take 5 deck
  True
  
  Main> isStraight $ take 5 deck
  True
  
  Main> take 5 $ drop 8 deck
  [TC,JC,QC,KC,AC]
  
  Main> isRoyalFlush $ take 5 $ drop 8 deck
  True
  
  Main> without deck 4
  [2C,3C,5C,6C,7C,8C,9C,TC,JC,QC,KC,AC,2D,3D,5D,6D,7D,8D,9D,TD,JD,QD,KD,AD,2H,3H,5H,6H,7H,8H,9H,TH,JH,QH,KH,AH,2S,3S,5S,6S,7S,8S,9S,TS,JS,QS,KS,AS]
  
  Main> byRank deck
  [(2,4),(3,4),(4,4),(5,4),(6,4),(7,4),(8,4),(9,4),(10,4),(11,4),(12,4),(13,4),(14,4)]
  
  Main> fourKind [(Card Clubs 3), (Card Diamonds 3), (Card Hearts 3), (Card Spades 3), (Card Clubs 2)]
  Just 3
-}

data Suit = Clubs | Diamonds | Hearts | Spades
  deriving (Eq)

type Rank = Int
data Card = Card Suit Rank 
type Hand = [Card]

instance Show Suit where
  show Clubs    = "C"
  show Diamonds = "D"
  show Hearts   = "H"
  show Spades   = "S"

instance Show Card where
  show (Card s r) = showRank r ++ show s

showRank :: Rank -> String
showRank r
  | r == 10   = "T"
  | r == 11   = "J"
  | r == 12   = "Q"
  | r == 13   = "K"
  | r == 14   = "A"
  | otherwise = show r

suit (Card s _) = s
rank (Card _ r) = r

-- Find the lowest rank in the hand
lowest :: Hand -> Rank
lowest cs = foldr min 14 $ map rank cs

deck :: [Card]
deck = [ Card s r | s <- [Clubs, Diamonds, Hearts, Spades], 
                    r <- [2..14] ]

-- All cards have the same suit
isFlush :: Hand -> Bool
isFlush h = all ((==s) . suit) h
  where s = suit (h !! 0)

-- Cards are in a run n, n+1, n+2, n+3, n+4
isStraight :: Hand -> Bool
isStraight cs = all (`elem` ranks) [low+1 .. low+4]
  where low = lowest cs
        ranks = map rank cs

isRoyalFlush :: Hand -> Bool
isRoyalFlush hand = all ($ hand) [isFlush, 
                                  isStraight, 
                                  (==10) . lowest]

-- Return the hand without a certain rank
without :: Hand -> Rank -> Hand
without cs r = filter ((/=r) . rank) cs

count :: Hand -> Rank -> Int
count cs r = length $ filter (==r) $ map rank cs

-- Count the number of cards of each rank
byRank :: Hand -> [(Rank, Int)]
byRank [] = []
byRank h@(c:cs) = let r = rank c
                  in (r, count h r) : byRank (without cs r)

fourKind :: Hand -> Maybe Rank
fourKind cs = let fours = filter (\(r, n) -> n == 4) $ byRank cs
              in if length fours > 0 
                 then Just $ fst (fours !! 0)
                 else Nothing

