module Game where 
import Data.Maybe

import Data.List

data Level = Small | Medium | Large | SuperLarge
type Player = Int
data Action = Xu
            | Attack Player Level
            | Defend Level
            | NoAction

type GameState = [Maybe Int] -- list of energies by player, None if player is defeated
data GameResult = Continue GameState
                | PlayerDefeated Player GameResult 
                | Winner Player


assert :: Bool -> String -> Either String ()
assert True _ = return ()
assert False s = Left s

attackCost :: Level -> Int
attackCost Small = 1
attackCost Medium = 2
attackCost Large = 3
attackCost SuperLarge = 4

defendCost :: Level -> Int
defendCost Small = 0
defendCost Medium = 1
defendCost Large = 2
defendCost SuperLarge = 3

levelLessThanOrEqualTo :: Level -> Level -> Maybe ()
levelLessThanOrEqualTo Small _ = return ()
levelLessThanOrEqualTo Medium Small = Nothing
levelLessThanOrEqualTo Medium _ = return ()
levelLessThanOrEqualTo Large Small = Nothing
levelLessThanOrEqualTo Large Medium = Nothing
levelLessThanOrEqualTo Large _ = return ()
levelLessThanOrEqualTo SuperLarge Small = Nothing
levelLessThanOrEqualTo SuperLarge Medium = Nothing
levelLessThanOrEqualTo SuperLarge Large = Nothing
levelLessThanOrEqualTo SuperLarge _ = return ()

maybeToEither :: Maybe a -> String -> Either String a
maybeToEither (Just x) _ = return x
maybeToEither Nothing str = Left str

applyEnergyChange :: Action -> Maybe Int -> Either String (Maybe Int)
applyEnergyChange Xu mx = do
    x <- maybeToEither mx "Only noaction is allowed on defeated player."
    return $ return (x+1)
applyEnergyChange (Attack _ l) mx = do
    x <- maybeToEither mx "Only noaction is allowed on defeated player."
    if x >= (attackCost l) 
        then return $ return (x-(attackCost l)) 
        else Left "Player does not have enough energy to attack"
applyEnergyChange (Defend l) mx = do
    x <- maybeToEither mx "Only noaction is allowed on defeated player."
    if x >= (defendCost l) 
        then return $ return (x-(defendCost l)) 
        else Left "Player does not have enough energy to defend"
applyEnergyChange (NoAction) mx = do
    if isJust mx 
        then Left "Undefeated player must pick some action"
        else return Nothing
    

checkActionLegal :: [Action] -> GameState -> Either String ()
checkActionLegal as gs = do
    flip mapM [0..(length as - 1)] (\idx -> 
        case (as !! idx) of 
            Attack p l -> do
                -- check p in index
                assert (0 <= p && p < length as) "Attacked index must be within range"
                -- check p is not self
                assert (idx /= p) "Cannot attack oneself"
                -- check attacked charge must be not none
                assert (isJust (gs !! p)) "Must attacked nondefeated player"
            _ -> return ()
            )
    return ()


runGameOneStep :: [Action] -> GameState -> Either String GameState
runGameOneStep as gs = do
    -- each player has an action
    assert (length as == length gs) "Internal Error: as /= gs"
    -- action is within spending budget
    nextState <- mapM (uncurry applyEnergyChange) (zip as gs)
    -- check action legal
    checkActionLegal as gs
    -- go through all actions and process them
    let nextState' = flip map [0..(length as-1)] (\idx -> 
        -- consider all other players actions
            flip mapM [0..(length as-1)] ( \idxOther -> 
                case (as !! idxOther) of 
                    -- if other player attacks me
                    Attack p l -> 
                        if p == idx 
                        then
                            case (as !! idx) of 
                                -- I lose if I play Xu
                                Xu -> Nothing
                                -- I survive if I attack him in return with same level
                                Attack p' l -> if p' == idxOther 
                                                then return ()
                                                else Nothing
                                -- I survive if I defend with at least same level
                                Defend l2 -> l `levelLessThanOrEqualTo` l2
                        else 
                            return ()
                    -- do nothing otherwise
                    _ -> return ()) >> 
            (nextState !! idx))
    return nextState'

runGameOneStepWithResults :: [Action] -> GameState -> Either String GameResult
runGameOneStepWithResults as gs = do
    nextState <- runGameOneStep as gs
    -- game has winner if there is only one surviving
    if length (filter (/= Nothing) nextState) == 1
        then case findIndex isJust nextState of
                Just x -> return (Winner x)
                Nothing -> error "Internal Error"
        else
            -- compare gs with as to determine if anyone dies
            let defeated = sequence $ filter (/= Nothing) 
                    (flip map (zip [0..] (zip gs nextState)) 
                    (\(idx, (prev, now)) -> 
                        if isJust prev && now == Nothing
                            then Just idx
                            else Nothing
                    ))
            in return $ foldr (\p gr -> PlayerDefeated p gr) (Continue nextState) (fromJust defeated)



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
    
    
loop :: GameState ->  IO ()
loop gs = do
    putStrLn "Current Round, Charges:"
    putStrLn (show gs)
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









    