{-# LANGUAGE CPP, ScopedTypeVariables #-}

import AbsCPP
import LexCPP
import ParCPP
import ErrM
import TypeChecker
--import Interpreter

import Control.Monad ( liftM, mapM, when )
import System.FilePath ( joinPath, takeExtension, (</>), (<.>) )
import System.Directory ( listDirectory, getCurrentDirectory, doesFileExist )
import System.Exit ( exitWith, ExitCode(..) )
import Data.List ( sort, intercalate )
import Text.Read ( readMaybe )
import Control.Monad.State ( StateT(..) )

listCCFiles :: FilePath -> IO [FilePath]
listCCFiles dir =
    liftM (map (\f -> joinPath [dir,f]) . sort . filter ((==".cc") . takeExtension)) $ listDirectory dir


welcome :: IO ()
welcome = do putStrLn $ highlight ++ "\n\nThis is the test program for Programming Languages Lab 2\n" ++ normal


runTest :: FilePath -> IO (Err ())
runTest f = do
  s <- readFile f
  return $ do
    let ts = myLexer s
    prog <- pProgram ts
    typecheck prog

testGoodProgram :: FilePath -> IO Bool
testGoodProgram f = do
  s <- readFile f
  let ts = myLexer s
  err <- return $ do
    prog <- pProgram ts
    err <- typecheck prog
    return err
  case err of
    Ok _ -> return True
    Bad err -> do
        putStrLn $ "Type-checking " ++ f ++ " failed"
        putStrLn $ fgcol red ++ err ++ normal
        return False

{-
showIO :: [Value] -> String
showIO xs = intercalate "\n" (map showAux xs)
  where
    showAux (VInteger i) = show i
    showAux (VDouble d) = show d
    showAux VVoid = "void"
    showAux VUndefined = "undefined"

getIO :: FilePath -> IO IIO
getIO f = do
    eIn <- doesFileExist $ f <.> "input"
    eOut <- doesFileExist $ f <.> "output"
    ins <- 
      if eIn then do
        s <- readFile $ f <.> "input"
        readIO (lines s)
      else return []
    outs <- 
      if eOut then do
        s <- readFile $ f <.> "output"
        readIO (lines s)
      else return []
    return $ IIO ins outs
  where
    readIO :: [String] -> IO [Value]
    readIO [] = return []
    readIO (x:xs) = case readMaybe x :: Maybe Integer of
      Nothing -> case readMaybe x :: Maybe Double of
        Nothing -> fail $ "Error parsing " ++ x
        Just d -> do
          vs <- readIO xs
          return $ (VDouble d):vs
      Just i -> do
          vs <- readIO xs
          return $ (VInteger i):vs
-}

testBadProgram :: FilePath -> IO Bool
testBadProgram f = do
  err <- runTest f
  case err of
    Ok _ -> do
      putStrLn $ "Type-checking " ++ f ++ " failed"
      putStrLn "Should not type-check"
      return False 
    Bad err -> do
      --putStrLn err
      return True

runTests :: ([FilePath],[FilePath]) -> IO ([Bool],[Bool])
runTests (goodProgs,badProgs) = do 
  good <- mapM testGoodProgram goodProgs
  bad  <- mapM testBadProgram badProgs
  return (good,bad)


main :: IO ()
main = do
    welcome
    cDir <- getCurrentDirectory
    good <- listCCFiles (cDir </> "test" </> "good")
    bad <- listCCFiles (cDir </> "test" </> "bad")
    (gRes,bRes) <- runTests (good, bad)

    putStrLn ""
    putStrLn "------------------------------------------------------------"
    gBool <- report "Good programs: " gRes
    bBool <- report "Bad programs:  " bRes

    if gBool && bBool then
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
