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
    modified <- newEmptySampleVar
    main' n modified

main' :: INotify -> SampleVar () -> IO ()
main' n modified = do
    forM_ toWatch (\p -> addWatch n [Modify, Move, Create, Delete] p handle)
    forever $ do
        wait 0.5
        e <- isEmptySampleVar modified
        when (not e) $ do
            readSampleVar modified
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
            | ".hs" == (reverse $ take 3 $ reverse f) =
                writeSampleVar modified ()
            | otherwise = return ()

printLog :: String -> IO ()
printLog s = do
    hPutStrLn stderr "#####"
    hPutStrLn stderr s
    hPutStrLn stderr "#####"
