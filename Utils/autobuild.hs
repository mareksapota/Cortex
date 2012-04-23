import System.INotify
import Control.Concurrent
import Control.Monad
import System.Cmd
import System.IO

-- This tool will recompile Cortex if any Haskell file is changed in watched
-- directories.

toWatch :: [String]
toWatch =
    [ "Miranda"
    , "Miranda/test"
    , "Miranda/test/performance"
    , "Saffron"
    , "Saffron/Apps"
    , "G23"
    , "Ariel"
    , "Common"
    , "Utils"
    ]

wait :: Double -> IO ()
wait t = threadDelay $ round $ (10 ** 6) * t

main :: IO ()
main = do
    n <- initINotify
    modified <- newEmptyMVar
    main' n modified

main' :: INotify -> MVar () -> IO ()
main' n modified = do
    forM_ toWatch (\p -> addWatch n [Modify, Move, Create, Delete] p handle)
    forever $ do
        takeMVar modified
        -- Avoid being triggered two times by one operation like writing several
        -- files at once, Vim writing to a temporary file and then replacing the
        -- target file, etc.
        wait 0.5
        tryTakeMVar modified
        printLog "Auto build triggered"
        rawSystem "./build" ["--fast"]
        printLog "Auto build done"
    where
        handle :: Event -> IO ()
        handle e
            | (not $ isDirectory e) = handle' e
            | otherwise = return ()

        handle' :: Event -> IO ()
        handle' (Modified { maybeFilePath = Just f }) = handle'' f
        handle' (MovedIn { filePath = f }) = handle'' f
        handle' (MovedOut { filePath = f }) = handle'' f
        handle' (Created { filePath = f }) = handle'' f
        handle' (Deleted { filePath = f }) = handle'' f
        handle' _ = return ()

        handle'' :: String -> IO ()
        handle'' f
            | ".hs" == (reverse $ take 3 $ reverse f) = do
                tryPutMVar modified ()
                return ()
            | otherwise = return ()

printLog :: String -> IO ()
printLog s = do
    hPutStrLn stderr "#####"
    hPutStrLn stderr s
    hPutStrLn stderr "#####"
