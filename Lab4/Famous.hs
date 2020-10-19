import Data.Char
import System.IO
import System.IO.Error

data QA = A String | 
          Q String QA QA -- Either an answer, or a question with yes/no subtrees
          deriving (Show, Read) 

-- | QA Decision tree that will be used when there is no QA file
standardTree :: QA
standardTree = Q "Is she from Europe?" 
              (Q "Is she a scientist?"  (A "Marie Curie") (A "Queen Elizabeth II"))
              (Q "Is she an actress?" (A "Marilyn Monroe") (A "Hillary Clinton") )

-- | Ask questions and go down either yes or no-subtree depending on user's
-- answer. Will either correctly reach an answer, or ask for a new question
-- that would separate the correct answer from the guessed one. Returns either
-- original tree or supplemented one, if it was incorrect.
play :: QA -> IO QA
play (A a) = do
    found <- yesNoQuestion $ "My guess: is it " ++ a ++"?"
    case found of 
        True -> do
            putStrLn "I am supreme. You are defeated."
            return (A a)
        False -> do 
            putStrLn "This game is beneath me."
            newA <- question $ "Not that it matters, but who was it?"
            newQ <- question $ 
                            "Boring. Give me a question to which the " ++
                            "answer is yes for " ++ newA ++ 
                            " but false for " ++ a
            return (Q newQ (A newA) (A a)) 
play (Q q y n ) = do
    a <- yesNoQuestion q
    case a of
        True  -> do
            y' <- play y
            return (Q q y' n)
        False -> do
            n' <- play n
            return (Q q y n')

-- | Writes out a question to terminal, retrievs the input from the user 
question :: String -> IO String
question q = do
    putStr $ q ++ "\n> "
    hFlush stdout
    a <- getLine
    return a

-- | Asks question until the user replies with yes or no (case-insensitive)
yesNoQuestion :: String -> IO Bool
yesNoQuestion q = do
    a <- question q
    case map toLower a of 
        "yes" -> return True
        "no"  -> return False
        _     -> do
            putStrLn "I know it's hard for you, but stick to yes/no answers"
            yesNoQuestion q

-- | Runs the guessing-game. Will either use decision-tree in file famous.qa, 
-- or standard. Will save resulting decision-tree in file famous.qa.
main :: IO ()
main = do
    try <- tryIOError $ readFile "famous.qa"
    case try of
        Left _  -> do
            new <- play standardTree
            writeFile "famous.qa" (show new)
        Right b -> do
            new <- play $ read b
            writeFile "famous.qa" (show new)
    replay <- yesNoQuestion "Play again?"
    case replay of 
        True  -> main
        False -> return ()