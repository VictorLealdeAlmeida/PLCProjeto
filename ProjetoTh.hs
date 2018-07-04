import Control.Concurrent
import System.Random

threadteamA :: Double -> MVar Double -> IO()
threadteamA force ropeCenter = do
   
    valueCenter <- takeMVar ropeCenter

    let printStr = printRope 0 $ floor (valueCenter - force)
    putStrLn printStr

    endCheck <- threadCheck (valueCenter - force)

    if not endCheck then do
        putMVar ropeCenter (valueCenter - force)
        threadDelay 1000000
        threadteamA force ropeCenter
    else
        putStrLn "TIME A VENCEU!"

threadteamB :: Double -> MVar Double -> IO()
threadteamB force ropeCenter = do

    valueCenter <- takeMVar ropeCenter

    print "a"
    print force

    power <- randomRIO (0 :: Double, 1)
    let powerForce = power + force
    print powerForce

    let printStr = printRope 0 $ floor (valueCenter + force)
    putStrLn printStr
    
    endCheck <- threadCheck (valueCenter + force)

    if not endCheck then do
        putMVar ropeCenter (valueCenter + force)
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

    --A forca da thread varia entre 1 e 3
    mbDouble <- randomRIO (1 :: Double, 3)
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



    

    
