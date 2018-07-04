import Control.Concurrent
import System.Random

threadteamA :: Double -> MVar Double -> IO()
threadteamA force ropeCenter = do
   
    valueCenter <- takeMVar ropeCenter

    power <- randomRIO (0 :: Double, 1)
    let powerForce = power * force
    --print "Team A:"
    --print (powerForce - 10.0)

    let printStr = printRope 0 $ floor (valueCenter - powerForce)
    putStrLn printStr

    endCheck <- threadCheck (valueCenter - powerForce)

    if not endCheck then do
        putMVar ropeCenter (valueCenter - powerForce)
        threadDelay 1000000
        threadteamA force ropeCenter
    else
        putStrLn "TIME A VENCEU!"

threadteamB :: Double -> MVar Double -> IO()
threadteamB force ropeCenter = do

    valueCenter <- takeMVar ropeCenter

    power <- randomRIO (0 :: Double, 1)
    let powerForce = power * force
    --print "Team B:"
    --print (powerForce - 10.0)

    let printStr = printRope 0 $ floor (valueCenter + powerForce)
    putStrLn printStr
    
    endCheck <- threadCheck (valueCenter + powerForce)

    if not endCheck then do
        putMVar ropeCenter (valueCenter + powerForce)
        threadDelay 1000000
        threadteamB force ropeCenter
    else
        putStrLn "TIME B VENCEU!"


threadCheck :: Double -> IO Bool
threadCheck valueCenter = do
    if valueCenter >= 20.0 then do
        return True
    else if valueCenter <= 0.0 then do
        return True
    else do
        return False

printRope :: Int -> Int -> [Char]
printRope 0 0 = "A X|-" ++ printRope 1 0
printRope 0 center = "A X-" ++ printRope 1 center
printRope n center | n == center = "|" ++ printRope (n+1) center
                   | n <= 20 = "-" ++ printRope (n+1) center
                   | otherwise = "X B"   

createThreads :: [Char] -> Int -> (Double -> MVar Double -> IO()) -> MVar Double -> IO()
createThreads team n th mvar = do

    --A forca da thread varia entre 1 e 4
    mbDouble <- randomRIO (1 :: Double, 5)
    putStrLn (team ++ "Thread " ++ show n ++ " Força: " ++ show (floor mbDouble))

    forkIO(th ((fromIntegral (floor mbDouble)) :: Double) mvar)
    threadDelay 10000
    if n > 1 then
        createThreads team (n-1) th mvar
    else
        threadDelay 1

    

main :: IO()
main = do

    ropeCenter <- newEmptyMVar

    putStrLn "APERTE START!"
    level <- getLine
    createThreads "Time A " 3 threadteamA ropeCenter
    createThreads "Time B " 3 threadteamB ropeCenter

    putMVar ropeCenter 10.0

    threadDelay 1000



    

    
