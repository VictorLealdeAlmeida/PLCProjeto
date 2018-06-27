import Control.Concurrent


threadAdv1 :: MVar Int -> IO()
threadAdv1 ropeCenter = do
    valueCenter <- takeMVar ropeCenter
    print "Adv1"
    print(valueCenter)
    putMVar ropeCenter (valueCenter - 1)
    threadDelay 1000000

    threadAdv1 ropeCenter

threadAdv2 :: MVar Int -> IO()
threadAdv2 ropeCenter = do
    valueCenter <- takeMVar ropeCenter
    print "Adv2"
    print(valueCenter)
    putMVar ropeCenter (valueCenter - 1)
   -- threadDelay 100000

    threadAdv1 ropeCenter

main :: IO()
main = do
    ropeCenter <- newMVar 0
    forkIO(threadAdv1 ropeCenter)
    forkIO(threadAdv2 ropeCenter)

    threadDelay 1000
