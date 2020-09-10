-- Authors: Emil Logren & Daniel Ericsson, Group 46

module Blackjack where

import Cards
import RunGame
import Test.QuickCheck hiding (shuffle)

-- Task A1
hand2 :: Hand
hand2 = [Card (Numeric 2) Hearts, Card Jack Spades]

sizeSteps :: [Int]
sizeSteps = [ size hand2
            , size (Card (Numeric 2) Hearts : (Card Jack Spades : []))
            , 1 + (size (Card Jack Spades : []))
            , 1 + 1 + (size [])
            , 1 + 1 + 0
            , 2
            ]

-- Task A2
card1 :: Card
card1 = (Card (Numeric 2) Clubs)

card2 :: Card
card2 = (Card Ace Spades)

card3 :: Card
card3 = (Card (Numeric 3) Clubs)

card4 :: Card
card4 = (Card (Numeric 10) Diamonds)

hand :: Hand
hand = [card1, card2, card3, card4]

display :: Hand -> String
display [] = ""
display [c] = displayCard c
display (c:cs) = unlines ((displayCard c) : [(display cs)])

displayCard :: Card -> String
displayCard (Card (Numeric r) s) = (show r) ++ " of " ++ (show s)
displayCard (Card r s) = (show r) ++ " of " ++ (show s)

-- Task A3
value :: Hand -> Int
value hand = adjustAces (sumHand hand) hand

-- Get raw sum of hand
sumHand :: Hand -> Int
sumHand [] = 0
sumHand (c:cs) = (valueCard c) + (value cs) 

valueCard :: Card -> Int
valueCard c = valueRank (rank c)

-- Count aces as 11, subtract later if necessary
valueRank :: Rank -> Int
valueRank (Numeric r) = r
valueRank r = case r of
    Ace       -> 11
    otherwise -> 10

numberOfAces :: Hand -> Int
numberOfAces [] = 0
numberOfAces (c:cs) = case rank c of
    Ace       -> 1 + numberOfAces cs
    otherwise -> numberOfAces cs

-- Counts aces as ones if score exceeds 21
adjustAces :: Int -> Hand -> Int
adjustAces score hand = case compare score 21 of
    GT -> score - (10*(numberOfAces hand))
    EQ -> score
    LT -> score

--Task A4
gameOver :: Hand -> Bool
gameOver hand = case compare (value hand) 21 of
    GT -> True
    EQ -> False
    LT -> False

winner :: Hand -> Hand -> Player
winner guest bank
    | gameOver guest            = Bank
    | value guest == value bank = Bank
    | gameOver bank             = Guest
    | value guest > value bank  = Guest
    | otherwise                 = Bank

-- Task B1
suits :: [Suit]
suits = [Hearts, Spades, Diamonds, Clubs]

ranks :: [Rank]
ranks = [(Numeric 2), (Numeric 3), (Numeric 4), 
         (Numeric 5), (Numeric 6), (Numeric 7), 
         (Numeric 8), (Numeric 9), (Numeric 10), 
         Jack, Queen, King, Ace]

-- All combinations of ranks and suits
fullDeck :: Deck
fullDeck = [(Card r s) | r <- ranks, s <- suits]

prop_size_fullDeck :: Bool
prop_size_fullDeck = size fullDeck == 52

-- Task B2
draw :: Deck -> Hand -> (Deck, Hand)
draw [] _ = error "draw: The deck is empty."
draw (c:cs) hand = (cs, (c:hand))

-- Task B3
playBank :: Deck -> Hand
playBank deck = playBank' deck []

playBank' :: Deck -> Hand -> Hand
playBank' deck hand = case compare (value hand') 16 of
    LT -> playBank' deck' hand'
    EQ -> hand'
    GT -> hand'
    where
        (deck', hand') = draw deck hand

-- Task B4
shuffle :: [Double] -> Deck -> Deck
shuffle _ [] = []
shuffle _ [c] = c : []
shuffle (d:ds) cs = card : (shuffle ds deck)
    where
        (card, deck) = removeCard d cs

removeCard :: Double -> Deck -> (Card, Deck)
removeCard _ [c] = (c, [])
removeCard d cs =  (card, deck)
    where
        split = splitAt (fromIntegral (round (fromIntegral(length cs)*d))) cs
        (card, deck)
            | length (snd split) > 0 = (head (snd split), ((fst split) ++ (tail (snd split))))
            | otherwise = (last (fst split), ((init (fst split)) ++ (snd split)))

-- Task B5
belongsTo :: Card -> Deck -> Bool
c `belongsTo` []      = False
c `belongsTo` (c':cs) = c == c' || c `belongsTo` cs

prop_shuffle :: Card -> Deck -> Rand -> Bool
prop_shuffle card deck (Rand randomlist) =
    card `belongsTo` deck == card `belongsTo` shuffle randomlist deck

prop_size_shuffle :: Rand -> Deck -> Bool
prop_size_shuffle (Rand randomlist) deck = 
    length deck == length (shuffle randomlist deck)

-- Task B6
implementation = Interface
  {  iFullDeck  = fullDeck
  ,  iValue     = value
  ,  iDisplay   = display
  ,  iGameOver  = gameOver
  ,  iWinner    = winner
  ,  iDraw      = draw
  ,  iPlayBank  = playBank
  ,  iShuffle   = shuffle
  }
  
main :: IO ()
main = runGame implementation

