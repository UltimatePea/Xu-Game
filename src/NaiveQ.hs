module NaiveQ where

-- This module optimizes player 0 and there are only two players

import Game
import Data.Map.Lazy as M hiding (map) -- lazy map has an advantage that we could compute values on the fly (no need to build entire game tree first but thunks)
import System.Random
import Data.List

reward :: GameState -> Double
reward [Nothing, _] = -1.0
reward [_, Nothing] = 1.0
reward _ = 0

actions0 = [Xu , Defend Small ]
actions1 p = [Attack p Small, Defend Medium]
actions2 p = [Attack p Medium, Defend Large]
actions3 p = [Attack p Large, Defend SuperLarge]
actions4 p = [Attack p SuperLarge]

actionsForPlayerWithCharge :: Player -> Int -> [Action]
actionsForPlayerWithCharge p 0 = actions0
actionsForPlayerWithCharge p 1 = actions0 ++ actions1 (1-p)
actionsForPlayerWithCharge p 2 = actions0 ++ actions1 (1-p) ++ actions2 (1-p)
actionsForPlayerWithCharge p 3 = actions0 ++ actions1 (1-p) ++ actions2 (1-p) ++ actions3 (1-p)
actionsForPlayerWithCharge p _ = actions0 ++ actions1 (1-p) ++ actions2 (1-p) ++ actions3 (1-p) ++ actions4 (1-p)


type QMapping = M.Map (GameState, Action) (Double, Int)

readQ :: (GameState, Action) -> QMapping -> Double
readQ (gs, a) map = case M.lookup (gs, a) map of 
        Nothing -> 0.0
        Just (x, y) -> x

writeQ :: (GameState, Action) -> Double -> QMapping -> QMapping
writeQ (gs, a) newValue map = case M.lookup (gs, a) map of 
    Nothing -> M.insert (gs, a) (newValue, 1) map
    Just (oldValue, n) -> M.insert (gs, a) (oldValue + 1.0 / (fromIntegral (n+1)) *(newValue - oldValue), n) map

pickPlayerActionWithEpsilon :: Player ->  GameState -> QMapping -> Double -> IO Action
pickPlayerActionWithEpsilon p gs map e = case gs !! p of 
    Nothing -> error "Asking for action on None Player"
    Just charge -> do
        let allActions = actionsForPlayerWithCharge p charge
        r <- randomIO :: IO Double
        if r < e
            then -- pick random
                do 
                    idx <- randomRIO (0, length allActions - 1) :: IO Int
                    return (allActions !! idx)
            else  -- pick best
                do 
                    return $ maximumBy (\a1 a2 -> compare (readQ (gs,a1) map) (readQ (gs,a2) map)) allActions

                
                

updateQForOneSimulation :: GameState -> QMapping -> IO QMapping
updateQForOneSimulation gs qmap = do
    if (gs !! 0 == Nothing) || (gs !! 1 == Nothing) 
        --then return $ writeQ (gs, action0) (reward gs) qmap
        then return qmap
        else do
            action0 <- pickPlayerActionWithEpsilon 0 gs qmap 0.4
            action1 <- pickPlayerActionWithEpsilon 1 gs qmap 0.4
            let gs' = case runGameOneStep [action0, action1] gs of
                        Left str -> error ("Internal Error: " ++ str)
                        Right g -> g
            -- calculate reward for gs
            let r0 = reward gs'
            let v1 = case (gs' !! 0) of 
                        Nothing -> 0
                        Just c -> 
                            let actions' = actionsForPlayerWithCharge 0 c 
                            in maximum (map (\a' -> readQ (gs',a') qmap) actions')
            let qmap' = writeQ (gs, action0) (0.95 * v1 + r0) qmap
            updateQForOneSimulation gs' qmap'

    
simulate ::  Int -> QMapping ->  IO QMapping
simulate n qmap = do
    putStrLn ("Iteration " ++ show n ++ ": " ++ show qmap)
    qmap' <- updateQForOneSimulation [Just 0, Just 0] qmap
    simulate (n+1) qmap'

keepSimulating :: IO QMapping
keepSimulating = simulate 0 M.empty

simulateNtimes :: Int -> IO QMapping
simulateNtimes n = 
    if n == 0
        then return M.empty
        else do 
            qm <- (simulateNtimes (n-1))
            updateQForOneSimulation [Just 0, Just 0] qm


simulated10000 :: IO QMapping
simulated10000 = simulateNtimes 10000

simulated100000 :: IO QMapping
simulated100000 = simulateNtimes 100000

simulated1000000 :: IO QMapping
simulated1000000 = simulateNtimes 1000000


