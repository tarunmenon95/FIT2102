-- | Tarun Menon 29739861 ----
--Highest Rank:17 Elo:1420~-----


--------Imports-------------
module Player where
import Parser.Parser -- This is the source for the parser from the course notes
import Parser.Instances
import Rummy.Types   -- Here you will find types used in the game of Rummy
import Cards         -- Finally, the generic card type(s)
import Rummy.Rules
import Data.List ( sortBy, maximumBy, (\\), nub, subsequences )
import Data.Ord ( comparing )
import Data.Char
----------------Show Instances------------------------

--Show instance to display suits
instance Show Suit where
  show Spade = "Spade"
  show Club = "Club"
  show Diamond = "Diamond"
  show Heart = "Heart"
--Show instance to display ranks
instance Show Rank where
  show Ace = "Ace"
  show Two = "Two" 
  show Three = "Three"
  show Four = "Four"
  show Five = "Five"
  show Six = "Six"
  show Seven = "Seven"
  show Eight = "Eight"
  show Nine = "Nine"
  show Ten = "Ten"
  show Jack = "Jack"
  show Queen = "Queen"
  show King = "King"
--Show instance to display melds
instance Show Meld where
     show (Deadwood c) = "Deadwood " ++ show c           
     show (Set3 x y z) = "Set3 " ++ show x ++ show y ++ show z  
     show (Set4 x y z zs) = "Set4 " ++ show x ++ show y ++ show z ++ show zs
     show (Straight3 x y z) = "Straight3 " ++ show x ++ show y ++ show z 
     show (Straight4 x y z zs) = "Straight4 " ++ show x ++ show y ++ show z ++ show zs
     show (Straight5 x y z zs zn) = "Straight5 " ++ show x ++ show y ++ show z ++ show zs ++ show zn 
--Show instance to display a card
instance Show Card where
  show (Card s r) = show s ++ " " ++ show r ++ " "
--Show instance to display a card 
instance Show Draw where
  show Stock = "Stock"
  show Discard = "Discard" 
---------------------------------------------------------

-------------Core Functions------------------------------

-- | The pickCard function creates melds using the current players hand with and without the drawn card,
-- | if a better meld is created using the visible discard it is chosen, else we utilise our analyzeBestDraw function
-- | which in turn uses statistics and memory to select the most optimal draw
-- | The pickCard function utilises traversables, folds, functors
pickCard :: ActionFunc
pickCard topOfDiscard score mem _ crds 
      | potentialScore <= currentScore = (Discard,memOut)     -- Draw from discard if potential score is lower (Eg: can make a set using discard pile card)
      | otherwise = (bestDraw,memOut)                         -- Draw from most effecient pile (Uses analyzeBestDraw to utilise memory for best draw)
      where curr = makeMelds score "" crds                            --Make melds with current cards
            potential = makeMelds score "" (crds ++ [topOfDiscard])   --Make melds with cards + visible card from discard
            currentScore =  foldl (+) 0 $ map cardPoints curr             --Add up scores with fold of melds from current cards
            potentialScore = foldl (+) 0 $ map cardPoints potential       --Add up scores with fold of melds current cards + discard   
            memOut = updateMemory score crds mem                          --Update the memory
            playerMem = getMem $ parse playerMemoryParser memOut                             --Create playerMemory object
            bestDraw = analyseBestDraw (cardsPlayed playerMem) topOfDiscard                  --Use memory/statistics to analyze best pile to draw from
            
-- | The playCard function optimally calls Gin, Knock or Drop based on the perceived value of the players hand and current round
-- | It creates melds using the drawn card and without to also select the most optimal card to discard based on value and need(ie: needed for meld)
-- | Finally it uses the hand without the discarded card to create melds and measure the value to decide on whether to Gin,Knock or Discard
-- | The pickCard function utilises folds and functors
playCard :: PlayFunc
playCard drawn score mem crds
      | foldl (+) 0 (map cardPoints finalMelds) == 0  && newRound meme == 1 = (Action Gin chosenDiscard,mem)     --Call Gin if possible, ensure its not the first turn
      | foldl (+) 0 (map cardPoints finalMelds) <= 10 && newRound meme == 1 = (Action Knock chosenDiscard,mem)   --Call Knock, ensure its not the first turn
      | otherwise = (Action Drop chosenDiscard,mem)                                                              --Otherwise Drop and continue
      where currentMelds = makeMelds score mem (crds ++ [drawn])                                                  --Create Melds from cards + drawn card
            withoutDrawMelds = makeMelds score mem (crds)                                                         --Create Melds from cards
            noDrawValue = maximumBy (\x y -> compare (cardPoints x) (cardPoints y)) withoutDrawMelds              --Get highest value meld from non drawn Melds
            highestValue = maximumBy (\x y -> compare (cardPoints x) (cardPoints y)) currentMelds                 --Get highest value meld from drawn Melds
            chosenDiscard = if (head (meldToCards highestValue) /= drawn)       --Select optimal discard (Eg: Not in meld, not drawn card, highest value deadwood)
                            then head $ meldToCards highestValue    
                            else head $ meldToCards noDrawValue
            finalHand = (crds ++ [drawn]) \\ [chosenDiscard]                    --Remove discard from hand
            finalMelds = makeMelds score mem finalHand                          --Make melds from final hand (Checking for Gin/Knock)
            meme = getMem $ parse playerMemoryParser mem                        --Create parse result to check whether its the first round
          

-- | The makeMelds function uses an input list of cards to and optimally selects the best possible meld from these cards, it then
-- | recursivly calls itself on the remainder of the hand excluding the cards used in the previous iteration until all possible melds are found.
-- | Once all sets/straights are found it simply Deadwoods the remainder of cards and returns a list containing all melds.
makeMelds :: MeldFunc
makeMelds score mem l
    |length nonZeroMelds > 0 = out                               --Creates melds if possible to do so
    |otherwise = Deadwood <$> l                                  --Otherwise Deadwood remainder of hand
    where sorted = sortBy (comparing getRank) l                  --Sorts hand by rank
          cardPermutations =  subsequences sorted                --Creates all permutations of hand
          meldPerms = nub $ filterPermutations cardPermutations  --Filter permutations containing melds, remove duplicates with nub
          possibleMelds = map createMeld $ meldPerms             --Map createMeld over all the meld perms
          nonZeroMelds = concat possibleMelds                    --Squash list of lists into single list of melds
          chosenMeld =  maximumBy (\x y -> compare (scoreHand $ head x) (scoreHand $ head y)) possibleMelds --Select highest value meld from possible melds
          remainingCards = l \\ (meldToCards $  head $ chosenMeld)    --Remove cards used in the best meld from hand
          out = chosenMeld ++ (makeMelds score mem remainingCards)    --Recursivly call makeMeld with the remaining cards whilst concatenating melds
                                                                      --to create hand of best possible melds

-------------------------------------------------------------------------
--The following parsers were obtained from my Week 10 - Week 12 lab work
-------------------------------------------------------------------------
----  Core parsers to build playerMemoryParser from   ----
list :: Parser a -> Parser [a]
list p = list1 p ||| pure []

list1 :: Parser a -> Parser [a]
list1 p = do
    a <- p
    b <- list p
    pure (a:b)

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = do
    a <- character
    if (f a) then pure a else unexpectedCharParser a

isNot :: Char -> Parser Char
isNot c = satisfy (\x -> x /= c)

parseUntil :: Char -> Parser [Char]
parseUntil c = do
    a <- list (isNot c)
    _ <- (is c)
    return a

space :: Parser Char
space = satisfy (\x -> x == ' ')

spaces :: Parser String
spaces = list space

digit :: Parser Char
digit = satisfy isDigit

string :: String -> Parser String
string s = traverse (is) s
-----------------------------------------------------------

------- Assignment 2 Specific Parsers----------------------

--Following set of parsers to parse a string into a Suit 
diamondParser :: Parser Suit
diamondParser = string "Diamond" >> pure Diamond
heartParser :: Parser Suit
heartParser = string "Heart" >> pure Heart
spadeParser :: Parser Suit
spadeParser = string "Spade" >> pure Spade
clubParser :: Parser Suit
clubParser = string "Club" >> pure Club

--Following set of parsers to parse a string into a Rank
aceParser :: Parser Rank
aceParser = string "Ace" >> pure Ace
twoParser :: Parser Rank
twoParser = string "Two" >> pure Two
threeParser :: Parser Rank
threeParser = string "Three" >> pure Three
fourParser :: Parser Rank
fourParser = string "Four" >> pure Four
fiveParser :: Parser Rank
fiveParser = string "Five" >> pure Five
sixParser :: Parser Rank
sixParser = string "Six" >> pure Six
sevenParser :: Parser Rank
sevenParser = string "Seven" >> pure Seven
eightParser :: Parser Rank
eightParser = string "Eight" >> pure Eight
nineParser :: Parser Rank
nineParser = string "Nine" >> pure Nine
tenParser :: Parser Rank
tenParser = string "Ten" >> pure Ten
jackParser :: Parser Rank
jackParser = string "Jack" >> pure Jack
queenParser :: Parser Rank
queenParser = string "Queen" >> pure Queen
kingParser :: Parser Rank
kingParser = string "King" >> pure King

--suitParser combines the previous individual suit parsers to create parser for all suits
suitParser :: Parser Suit
suitParser = diamondParser ||| heartParser ||| spadeParser ||| clubParser

--rankParser combines the previous individual rank parsers to create parser for all ranks
rankParser :: Parser Rank
rankParser = aceParser ||| twoParser ||| threeParser ||| fourParser ||| fiveParser ||| sixParser ||| sevenParser ||| eightParser ||| nineParser ||| tenParser ||| jackParser ||| queenParser ||| kingParser

--cardParser uses suitParser, rankParser and spaces to create a card based on input, eats spaces between suit and rank.
cardParser :: Parser Card
cardParser = do 
   a <- suitParser
   _ <- spaces
   b <- rankParser
   _ <- spaces
   return (Card a b)

--scoreParser reads a string (x,y) by eating away '(' ',' ')' and returning x and y as ints in a tuple.
--Uses numParser to ensure x and y are integers for score datatype
scoreParser :: Parser (Score,Score)
scoreParser = do 
  _ <- is '('
  a <- numParser
  _ <- is ','
  b <- numParser
  _ <- is ')'
  return  (a,b)

--numParser uses list and digit with fmap to convert any string of integers into a intger
numParser :: Parser Int
numParser = do
  x <- read <$> list digit
  return x

--datatype for our playerMemory which is used as the core memory throughout the assignment
--We store newRound as int (Flag for whether first round or not), the current score of the game and all cards seen so far in a list
data PlayerMemory = PlayerMemory {newRound :: Int, curScore :: (Score,Score), cardsPlayed :: [Card]} 
    deriving (Show)

--playerMemoryParser combines our previous parsers (cardParser,ScoreParser) to parse over a string to create our playerMemory datatype
playerMemoryParser :: Parser PlayerMemory
playerMemoryParser = do
    x <- read <$> list digit
    a <- scoreParser
    _ <-spaces
    _ <-is '['
    b <- list cardParser
    _ <- is ']'
    return (PlayerMemory x a b)

------------- Functions -----------------

--Combines setExists and straightExists to validate whether a list of cards contains a meld
meldExists :: [Card] -> Bool
meldExists crds =  setExists crds  || straightExists crds

--Simple function to convert a meld back into a list of cards
meldToCards :: Meld -> [Card]
meldToCards x = case x of Deadwood c -> [c]
                          Set3 c c3 c4 -> [c,c3,c4]
                          Set4 c c3 c4 c5-> [c,c3,c4,c5]
                          Straight3 c c3 c4 -> [c,c3,c4]
                          Straight4 c c3 c4 c5-> [c,c3,c4,c5]
                          Straight5 c c3 c4 c5 c6-> [c,c3,c4,c5,c6]


--Takes in a list of lists containing all permutations of cards held, based on each permutation check for existing meld, if so add to output 
--and continue the process, else dont add and continue 
filterPermutations :: [[Card]] -> [[Card]]
filterPermutations [] = []
filterPermutations(x:xs) = if (meldExists x) then [x] ++  filterPermutations xs else filterPermutations xs

--Based on input set of cards, check whether a set or straight exists, if so create the best corresponding meld possible
createMeld :: [Card] -> [Meld]
createMeld [x,y,z,a,b]
  |straightExists[x,y,z,a,b] = [Straight5 x y z a b]
createMeld [x,y,z,zs]
  |straightExists[x,y,z,zs] = [Straight4 x y z zs]
  |setExists[x,y,z,zs] = [Set4 x y z zs] 
createMeld [x,y,z]
  |straightExists [x,y,z] = [Straight3 x y z]
  |setExists [x,y,z] = [Set3 x y z]
createMeld _ = []

--Get the suit of a card
getSuit :: Card -> Suit
getSuit (Card s _)  = s

--Get the rank of a card
getRank :: Card -> Rank
getRank (Card _ r)  = r

--Input a Meld to return a Integer representing the value of cards in a meld (Not 0 like meld but value of each card)
scoreHand :: Meld-> Int
scoreHand (Set3 x y z) = toPoints x + toPoints y + toPoints z 
scoreHand (Set4 x y z zs) = toPoints x + toPoints y + toPoints z + toPoints zs
scoreHand (Straight3 x y z) = toPoints x + toPoints y + toPoints z
scoreHand (Straight4 x y z a) = toPoints x + toPoints y + toPoints z + toPoints a
scoreHand (Straight5 x y z a b) = toPoints x + toPoints y + toPoints z + toPoints a + toPoints b
scoreHand _ = 0

--Check whether a set exists in the list of cards being input by validating all the same rank
setExists :: [Card] -> Bool
setExists [] = False
setExists [_] = False
setExists [_,_] = False
setExists (x:xs) = all(\l -> getRank x == getRank l) xs

--Check whether a straight exists in the list of cards being input by checking all the same rank and then if they
--are in succeeding order (Not the most elegant solution...)
straightExists :: [Card] -> Bool
straightExists [] = False
straightExists [_] = False
straightExists [_,_] = False
straightExists [_,_,_,_,_,_] = False
straightExists l@[a,b,c] = all (\r -> getSuit r == getSuit a) l 
                && (getRank a /= maxBound) && (getRank b /= maxBound) 
                && getRank b == succ (getRank a) && getRank (c) == succ (getRank b)
straightExists l@[a,b,c,d] = all (\r -> getSuit r == getSuit a) l 
                && (getRank a /= maxBound) && (getRank b /= maxBound) && (getRank c /= maxBound) 
                && getRank b == succ (getRank a) && getRank (c) == succ (getRank b) && getRank (d) == succ (getRank c)
straightExists l@[a,b,c,d,e] = all (\r -> getSuit r == getSuit a) l 
                && (getRank a /= maxBound) && (getRank b /= maxBound) && (getRank c /= maxBound) && (getRank d /= maxBound) 
                && getRank b == succ (getRank a) && getRank (c) == succ (getRank b) && getRank (d) == succ (getRank c) && getRank (e) == succ (getRank d)
straightExists _ = False

--Function takes in game parameters such as score, cards, and Maybe memory(Not used) in order to create a base
--memory at the start of the game
createBaseMemory :: (Score,Score) -> [Card] -> Maybe String -> String
createBaseMemory sc crds _ = "0" ++ show sc ++ [ x | x <- show crds, not (x `elem` ",") ] 

--Function used to update an existing memory with new game parameters, if this isnt the start of the game
--will call function update to update the memory otherwise will create the base memory
updateMemory :: (Score,Score) -> [Card] -> Maybe String -> String
updateMemory s c existMemory = case existMemory of
              Nothing -> createBaseMemory s c (Nothing)     --If first round then create base
              (Just a)  -> update s c a                     --Else update

--Function to update current memory, creates flag to validate whether this is the first round or not, otherwise 
--contains current score and list of all cards seen so far for later statistic use for optimisation
update ::  (Score,Score) -> [Card] -> String -> String
update s c mem = flag ++ show s ++ [ x | x <- show totalCards, not (x `elem` ",") ]    --Flag + Score + All cards seen
          where pm = getMem $ parse playerMemoryParser mem
                flag = if curScore pm == s then "1" else "0"    --Update flag
                totalCards = if flag == "0" then c else sortBy (comparing getRank) $ nub $ c ++ cardsPlayed pm    --If new round reset cards seen

--Function to retrieve ParseResult from parsers
getMem :: ParseResult a -> a
getMem(Result _ v) =  v
getMem (Error _) = error "rip"  

--Simple function using functors and applicative to instantiate a full deck
createDeck :: [Card]
createDeck = Card <$> [Spade ..] <*> [Ace ..]

--analyseBestDraw utilizes the players memory to remove all seen cards from an imaginary deck
--It then averages the value of the remaning deck using fold and measures it against the value of the discard pile card
--We select which pile to draw from based on the lower value between the two
--analyzeBestDraw uses fold, functors aswell as memory and probablity
analyseBestDraw :: [Card] -> Card -> Draw
analyseBestDraw cards discard
              | averageValueOfRemainingDeck <= valueOfDiscard = Stock
              | otherwise = Discard
              where deck = createDeck
                    remainder = deck \\ cards      
                    valueOfRemainingDeck = foldl (+) 0 $ toPoints <$> remainder
                    averageValueOfRemainingDeck = valueOfRemainingDeck `div` length remainder
                    valueOfDiscard = toPoints discard