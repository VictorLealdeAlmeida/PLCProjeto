import Control.Concurrent
import System.Random

threadteamA :: Int -> MVar Int -> IO()
threadteamA force ropeCenter = do
   -- print "F1"
   -- print force
    valueCenter <- takeMVar ropeCenter

    endCheck <- threadCheck (valueCenter - force)

    if not endCheck then do
        putMVar ropeCenter (valueCenter - force)
        threadDelay 1000000
        threadteamA force ropeCenter
    else
        putStrLn "TIME A VENCEU!"

threadteamB :: Int -> MVar Int -> IO()
threadteamB force ropeCenter = do
   -- print "F2"
   -- print force
    valueCenter <- takeMVar ropeCenter

    let printStr = printRope 0 (valueCenter + force)
    putStrLn printStr
    
    endCheck <- threadCheck (valueCenter + force)

    if not endCheck then do
        putMVar ropeCenter (valueCenter + force)
        threadDelay 1000000
        threadteamB force ropeCenter
    else
        putStrLn "TIME B VENCEU!"


threadCheck :: Int -> IO Bool
threadCheck valueCenter = do
    if valueCenter >= 20 then do
        return True
    else if valueCenter <= 0 then do
        return True
    else do
        return False

printRope :: Int -> Int -> [Char]
printRope 0 0 = "A X|-" ++ printRope 1 0
printRope 0 center = "A X-" ++ printRope 1 center
printRope n center | n == center = "|" ++ printRope (n+1) center
                   | n <= 20 = "-" ++ printRope (n+1) center
                   | otherwise = "X B"   

createThreads :: [Char] -> Int -> (Int -> MVar Int -> IO()) -> MVar Int -> IO()
createThreads team n th mvar = do

    --A forca da thread varia entre 1 e 3
    mbInt <- randomRIO (1 :: Int, 3)
    putStrLn (team ++ "Thread " ++ show n ++ " Força: " ++ show mbInt)

    forkIO(th mbInt mvar)
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

    putMVar ropeCenter 10

    threadDelay 1000



    

    
