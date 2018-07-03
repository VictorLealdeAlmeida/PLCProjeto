import Control.Concurrent


threadAdv :: MVar Int -> IO()
threadAdv ropeCenter = do
    valueCenter <- takeMVar ropeCenter
    print "Adv"
    print(valueCenter)
    putMVar ropeCenter (valueCenter - 1)
    threadDelay 1000000
    

    threadAdv ropeCenter

threadPlayer :: MVar Int -> IO()
threadPlayer ropeCenter = do
    level <- getLine
    valueCenter <- takeMVar ropeCenter
    putMVar ropeCenter (valueCenter + 1)

    threadPlayer ropeCenter


createThreads :: Int -> (MVar Int -> IO()) -> MVar Int -> IO()
createThreads n th mvar = do
    forkIO(th mvar)

    if n > 1 then
        createThreads (n-1) th mvar
    else
        threadDelay 1
    
        


   

main :: IO()
main = do

    ropeCenter <- newMVar 0

    putStrLn "Escolha um nível - (1) (2) (3)"
    level <- getLine
    if level == "1" then do
        putStrLn "Nível Um Escolhido"
        createThreads 1 threadAdv ropeCenter
    else if level == "2" then do
        putStrLn "Nível Dois Escolhido"
        createThreads 2 threadAdv ropeCenter
    else if level == "3" then do
        putStrLn "Nível Tres Escolhido"
        createThreads 3 threadAdv ropeCenter
    else do
        putStrLn "Escolha entre (1) (2) (3)"
        main


    forkIO(threadPlayer ropeCenter)

    threadDelay 1000



    

    
