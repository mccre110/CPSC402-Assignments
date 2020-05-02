{-# LANGUAGE CPP, ScopedTypeVariables #-}

import AbsCPP
import LexCPP
import ParCPP
import ErrM
import Compiler

import Control.Monad ( liftM, mapM, when )
import System.FilePath ( joinPath, takeExtension, takeFileName, replaceExtension, (</>), (<.>) )
import System.Directory ( listDirectory, getCurrentDirectory, doesFileExist )
import System.Exit ( exitWith, ExitCode(..), exitFailure )
import System.Process ( ProcessHandle, runInteractiveCommand, waitForProcess )
import System.IO (hPutStr, hPutStrLn, hGetContents, hClose, stderr)
import Data.List ( sort, intercalate )
import Data.List.Split ( splitOn )
import Text.Read ( readMaybe )
import Control.Monad.State ( StateT(..), evalStateT )
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent (forkIO)

listCCFiles :: FilePath -> IO [FilePath]
listCCFiles dir =
    liftM (map (\f -> joinPath [dir,f]) . sort . filter ((==".cc") . takeExtension)) $ listDirectory dir


welcome :: IO ()
welcome = do putStrLn $ bold++"\n\nThis is the test program for Compiler Construction Assignment 5\n" ++ normal



--
-- * Various versions of runCommand
--

runCommandStr :: String -- ^ command
        -> String -- ^ stdin data
        -> IO (String,String,ProcessHandle) -- ^ stdout, stderr, process
runCommandStr c inStr = 
    do
    outVar <- newEmptyMVar
    errVar <- newEmptyMVar
    (pin,pout,perr,p) <- runInteractiveCommand c
    forkIO $ do 
                -- putStrLn "Writing input..."
                hPutStr pin inStr
                hClose pin
                -- putStrLn "Wrote input."
    forkIO $ do 
                -- putStrLn "Reading output..."
                s <- hGetContents pout
                putMVar outVar s
                -- putStrLn "Read output."
    forkIO $ do 
                -- putStrLn "Reading error..."
                s <- hGetContents perr
                putMVar errVar s
                -- putStrLn "Read error."
    out <- takeMVar outVar
    err <- takeMVar errVar
    return (out,err,p)


runCommandStrWait :: String -- ^ command
      -> String -- ^ stdin data
      -> IO (String,String,ExitCode) -- ^ stdout, stderr, process exit status
runCommandStrWait c inStr =
    do
    (out,err,p) <- runCommandStr c inStr
    s <- waitForProcess p
    -- putStrLn $ "Standard error:\n" ++ err
    return (out,err,s)


testProgramCompile :: FilePath -> IO Bool
testProgramCompile f = do
  (input, output) <- getIORaw f
  putStrLn $ "Compiling " ++ f ++ "... "
  s <- readFile f
  let ts = myLexer s
  let prog = pProgram ts
  case prog of
    Ok p -> do
      let out = compile p
      writeFile (replaceExtension f "wat") out
      
      let cmd = "node test/wat2wasm.js test/good/" ++ takeFileName (replaceExtension f "wat")
      putStrLn $ "Running " ++ cmd

      (out,err,s) <- runCommandStrWait cmd ""
      case s of
       ExitFailure x -> do
            putStrLn $ color red $ "Error: Converting to wasm failed " ++ takeFileName f
            putStrLn $ color red $ head $ tail $ splitOn "Error: validate failed:" err
            return False
       ExitSuccess -> do
        putStrLn $ color green $ "Successfully compiled to Wasm: " ++ takeFileName f
        let c = "node test/run.js " ++ (replaceExtension f "wasm")
        putStrLn $ c

        (out,err,s) <- runCommandStrWait c input
        -- putStrLn $ "Exit code: " ++ show s
        putStrLn $ "Running " ++ (replaceExtension f "wasm") ++ "... "
        if out == output then do
          putStrLn $ color green $ "Successfully run: " ++ takeFileName f
          return True
        else do
                putStrLn "Execution output:"
                putStrLn $ color red $ out
                putStrLn "Expected output:"
                putStrLn $ color blue $ output
                return False
    _ ->  do
      putStrLn $ "Error parsing " ++ f ++ "... "
      return False




getIORaw :: FilePath -> IO (String, String)
getIORaw f = do
    eIn <- doesFileExist $ f <.> "input"
    eOut <- doesFileExist $ f <.> "output"
    ins <- 
      if eIn then readFile $ f <.> "input"
      else return []
    outs <- 
      if eOut then readFile $ f <.> "output"
      else return []
    return (ins, outs)


runTests :: [FilePath] -> IO [Bool]
runTests goodProgs = do 
  good_compile <- mapM testProgramCompile goodProgs
  return good_compile


main :: IO ()
main = do
    welcome
    cDir <- getCurrentDirectory
    good <- listCCFiles (cDir </> "test" </> "good")
    gRes <- runTests good


    putStrLn ""
    putStrLn "------------------------------------------------------------"
    gBool <- report "Good programs: " gRes

    if gBool then
        exitWith ExitSuccess
    else
        exitWith (ExitFailure 1)



fgcol, bgcol :: Color -> String
fgcol col = "\ESC[0" ++ show (30+col) ++ "m"
bgcol col = "\ESC[0" ++ show (40+col) ++ "m"

highlight, bold, underline, normal :: String
highlight = "\ESC[7m"
bold      = "\ESC[1m"
underline = "\ESC[4m"
normal    = "\ESC[0m"

type Color = Int

color :: Color -> String -> String
#if defined(mingw32_HOST_OS)
color _ s = s
#else
color c s = fgcol c ++ s ++ normal
#endif

red, green, blue, black :: Color
black = 0
red = 1
green = 2
blue = 6

-- | Report how many tests passed.
report :: String -> [Bool] -> IO Bool
report n rs =
  do let (p,t) = (length (filter id rs), length rs)
         c = if p == t then green else red
     putStrLn $ color c $
              n ++ "passed " ++ show p ++ " of " ++ show t ++ " tests"
     return $ p == t
