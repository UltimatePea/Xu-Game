module GameUx where

import Game
import NaiveQ


manualRun :: IO ()
manualRun = do
    putStrLn "Welcome to 波波攒"
    putStrLn "Enter Number of players"
    s <- getLine
    let n = read s :: Int
    let initialState = replicate n (Just 0)
    loop initialState


parseLevel :: Char -> Either String Level
parseLevel 's' = return Small
parseLevel 'm' = return Medium
parseLevel 'l' = return Large
parseLevel 'S' = return SuperLarge
parseLevel _ = Left "Unrecognized level, must be (s)mall (m)edium (l)arge (S)uperLarge"



parseAction :: String -> Either String Action
parseAction ('c':_) = return Xu
parseAction ('a':x:l:_) = do
    let n = read [x] :: Int
    l' <- parseLevel l
    return (Attack n l')
parseAction ('d':l:_) = do
    l' <- parseLevel l
    return (Defend l')
parseAction ('n':_) = return NoAction
parseAction _ = Left "Unrecognized action, must be (c)harge, (a)ttack[player][level], (d)efend[level]"

getAction :: IO Action
getAction = do 
    s <- getLine
    case parseAction s of
        Left str -> putStrLn str >> getAction
        Right a -> return a
    
showAdvising :: GameState ->  IO ()
showAdvising gs =
    if length gs /= 2 
        then putStrLn "No advising. Advising only for 2 players."
        else do
            case gs !! 0 of 
                Nothing -> putStrLn "Player 1 defeated, no advising."
                Just c -> do
                    putStrLn "Values estimate for actions: "
                    let actions = actionsForPlayerWithCharge 0 c
                    qmap <- simulated100000
                    flip mapM actions (\a -> 
                        putStrLn ((show a) ++ ": " ++ show (readQ (gs, a) qmap) ))
                    return ()

    
loop :: GameState ->  IO ()
loop gs = do
    putStrLn "Current Round, Charges:"
    putStrLn (show gs)
    -- show advising
    showAdvising gs
    -- get action for all player
    actions <- mapM (\idx -> do
        putStrLn ("Player " ++ show idx ++ ": Enter action:")
        getAction 
        ) [0..(length gs - 1)] 
    case runGameOneStepWithResults actions gs of 
        Left str -> do 
            putStrLn str
            loop gs
        Right res -> 
            let handleResult r = case r of 
                    Winner x -> putStrLn ("Player " ++ show x ++ " won the game!") >> return ()
                    Continue gs' -> loop gs'
                    PlayerDefeated x r' -> putStrLn ("Player " ++ show x ++ " lost.") >> handleResult r'
            in handleResult res



