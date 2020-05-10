{-# LANGUAGE RecordWildCards, FlexibleInstances, FlexibleContexts, PatternSynonyms #-}

module Compiler where

import AbsCPP
import ErrM
import PrintCPP

import Data.Map ( Map )
import qualified Data.Map as M
import Data.Set ( Set )
import qualified Data.Set as S
import Data.List ( intercalate, intersperse, nub )
-- import Control.Monad ( foldM, foldM_, forM_, unless )
import Control.Monad.State ( MonadState, evalState, get, put, modify, foldM, liftIO, lift )

-- the data type of S-expressions
data SExp = Atom String | List [SExp] deriving Eq

-- docs on the wat language
-- https://webassembly.github.io/spec/core/exec/index.html
-- https://webassembly.github.io/spec/core/valid/instructions.html

-- link to the wasm typechecker
-- https://maurobringolf.ch/2018/04/learning-the-webassembly-type-system/

-- helper functions to generate S-expressions
-- Haskell has typeinference, so we don't need to write all type signatures:
-- s_i32 :: Maybe SExp
s_i32 = Just $ Atom "i32"
s_f64 = Just $ Atom "f64"
s_void = Nothing

s_var v = Atom ('$' : v)
s_module xs = List (Atom "module":xs)
s_block xs = List (Atom "block":xs)
s_loop xs = List (Atom "loop":xs)

s_br, s_br_if :: Int -> SExp 
s_br l = List [Atom "br", Atom $ show l]
s_br_if l = List [Atom "br_if", Atom $ show l]

s_if_then_else ty if_case else_case = 
    List $ [Atom "if"] ++ s_result ty ++
        [List (Atom "then":if_case),
        List (Atom "else":else_case)]

s_return = Atom "return"

s_result ty = maybe [] (\t -> [List [Atom "result", t]]) ty
s_import nm in_typ res_typ = 
    List ([
        Atom "import", 
        Atom "\"env\"" , 
        Atom ("\"" ++ nm ++ "\""),
        List (
            [Atom "func", s_var nm] ++
            map (\(Just p) -> List [Atom "param", p]) in_typ ++ 
            s_result res_typ)]) 

s_export nm = List [Atom "export", Atom $ "\"" ++ nm ++ "\"", List [Atom "func", s_var nm] ]

s_func :: String -> [(String, Maybe SExp)] -> Maybe SExp -> [SExp] -> SExp
s_func nm in_typ res_typ body = 
    List $ [
        Atom "func", 
        s_var nm] ++ 
        map (\(n, Just t) -> List [Atom "param", s_var n, t]) in_typ ++ 
        s_result res_typ ++ 
        body

s_local nm (Just ty) = List $ [Atom "local", s_var nm, ty]
s_local_get nm = List [Atom "local.get", s_var nm]
s_local_set nm = List [Atom "local.set", s_var nm]
s_local_tee nm = List [Atom "local.tee", s_var nm]
s_drop = Atom "drop"

s_call nm = List [Atom "call", s_var nm]

s_i32_const :: Integer -> SExp
s_i32_const i = List [Atom "i32.const", Atom (show i)]

s_i32_add = Atom "i32.add"
s_i32_sub = Atom "i32.sub"
s_i32_mul = Atom "i32.mul"
s_i32_div_s = Atom "i32.div_s"
s_i32_lt_s = Atom "i32.lt_s"
s_i32_le_s = Atom "i32.le_s"
s_i32_gt_s = Atom "i32.gt_s"
s_i32_ge_s = Atom "i32.ge_s"
s_i32_eq = Atom "i32.eq"
s_i32_ne = Atom "i32.ne"
s_i32_eqz = Atom "i32.eqz"

s_f64_const :: Double -> SExp
s_f64_const i = List [Atom "f64.const", Atom (show i)]

s_f64_add = Atom "f64.add"
s_f64_sub = Atom "f64.sub"
s_f64_mul = Atom "f64.mul"
s_f64_div = Atom "f64.div"
s_f64_lt = Atom "f64.lt"
s_f64_le = Atom "f64.le"
s_f64_gt = Atom "f64.gt"
s_f64_ge = Atom "f64.ge"
s_f64_eq = Atom "f64.eq"
s_f64_ne = Atom "f64.ne"


-- prints SExp's as lists of strings (= Wat programs)
pprint_aux :: SExp -> [String]
pprint_aux (Atom s) = [s]
pprint_aux (List []) = ["()"]
pprint_aux (List (x:xs)) = 
    (possiblyMerge 100 (pprint_aux x) (concat $ map pprint_aux xs))

concatHead :: String -> [String] -> [String]
concatHead s [] = [s]
concatHead s (x:xs) = (s++x):xs

concatTail :: String -> [String] -> [String]
concatTail s [] = [s]
concatTail s [x] = [x++s]
concatTail s (x:xs) = x:(concatTail s xs)

possiblyMerge l x xs = concatHead "(" $ 
    if length (concat (x++xs)) < l then 
        concatTail ")" $ [intercalate " " (x++xs)] 
    else 
        x ++ map (" "++) xs ++ [")"]

pprint sexp = intercalate "\n" $ pprint_aux sexp

-- compile C++ to WASM
compile :: Program -> String
compile (PDefs []) = ""
compile (PDefs defs) = pprint prog
    where
        prog = s_module $ [
                s_import "readInt" [] s_i32,
                s_import "readDouble" [] s_f64,
                s_import "printInt" [s_i32] s_void,
                s_import "printDouble" [s_f64] s_void
            ] ++ flip evalState 
                (M.fromList $ [
                        (Id "readInt", (Type_int, -1)),
                        (Id "readDouble", (Type_double, -1)),
                        (Id "printInt", (Type_void, -1)),
                        (Id "printDouble", (Type_void, -1))
                    ] 
              ++ map (\(DFun ty n _ _) -> (n, (ty, -1))) defs, 0) (mapM compileDef defs)
              ++ [s_export "main"]
        
-- example of an S-expressions written with the helper functions
-- run `stack ghci` and `putStrLn $ pprint test`
test = s_module [
        s_import "readInt" [] s_i32,
        s_import "readDouble" [] s_f64,
        s_import "printInt" [s_i32] s_void,
        s_import "printDouble" [s_f64] s_void,
        s_func "main" [] s_i32 [
            s_local "i" s_f64,
            s_call "readDouble",
            s_local_set "i",
            s_local_get "i",
            s_call "printDouble",
            s_i32_const 0
        ],
        s_export "main"
    ]

-- turns a type in an S-expression
compileType :: Type -> Maybe SExp
compileType Type_bool = s_i32
compileType Type_int = s_i32
compileType Type_double = s_f64
compileType Type_void = s_void
compileType Type_string = undefined

-- an Env maps a variable to its type and its scope and keeps track of the current scope
type Env = (Map Id (Type, Int), Int)

-- we use the Haskell run-time environment to build a stack of Env's
compileDef :: MonadState Env m => Def -> m SExp
compileDef (DFun ty (Id n) args stms) = do
    (m,c) <- get
    modify (\(m,c) -> (foldl (\m' (ADecl ty i) -> M.insert i (ty,c) m') m args, c))
    s_body <- compileStms stms
    modify (\_ -> (m,c)) -- restore the Env (m,c)
    return $ s_func 
        n 
        (map (\(ADecl ty (Id i)) -> (typePrefix ty i ++ "$0",compileType ty)) args)
        (compileType ty)
        s_body

compileStms :: MonadState Env m => [Stm] -> m [SExp]
compileStms stms = do
    -- first we declare all the variables in the function body
    let var_decls = (map (\(n,t) -> s_local n t) $ nub $ concat $ map (collectDecls 0) stms)
    -- then we compile all the statements
    s_body <- mapM compileStm stms
    return $ var_decls ++ concat s_body


-- in WASM all variables need to be declared at the beginning of the function:
-- we have to collect all the variables declared within a function body, 
-- including inside any blocks, such as ifs/while loops.
-- to avoid problems of shadowing, we keep a counter of scopes.
-- thus a variable i inside the main function body will be named i$0
collectDecls :: Int -> Stm -> [(String, Maybe SExp)]
collectDecls counter (SDecls ty ids) = map (\(Id i) -> (typePrefix ty i ++ "$" ++ show counter, compileType ty)) ids
collectDecls counter (SInit ty (Id i) _) = [(typePrefix ty i ++ "$" ++ show counter, compileType ty)]
collectDecls counter (SWhile _ s) = collectDecls (counter+1) s
collectDecls counter (SBlock ss) = concat $ map (collectDecls (counter+1)) ss
collectDecls counter (SIfElse _ s1 s2) = collectDecls (counter+1) s1 ++ collectDecls (counter+1) s2
collectDecls _ _ = []


-- make the type of a variable part of the name
typePrefix :: Type -> String -> String
typePrefix Type_int s = "i" ++ s
typePrefix Type_double s = "d" ++ s
typePrefix Type_bool s = "b" ++ s
typePrefix _ s = s

-- convert variable names from source to target language
getVarName :: MonadState Env m => Id -> m String
getVarName i@(Id s) = do
    (m,_) <- get
    case M.lookup i m of
        Just (ty,c) -> return $ typePrefix ty s ++ "$" ++ show c
        Nothing -> error $ "Error : " ++ s ++ " is not in the map " ++ show m 


-- similar to the pushPop of the interpreter
pushPop :: MonadState Env m => m a -> m a
pushPop f = do
    (m_old,c_old) <- get
    modify (\(m, c) -> (m,c+1))
    a <- f
    modify (\_ -> (m_old,c_old))
    return a


compileStm :: MonadState Env m => Stm -> m [SExp]
compileStm (SExp e) = compileExp TopLevel e
compileStm (SDecls ty ids) = do
    -- we have already declared all variables via collectDecls
    modify (\(m, c) -> (foldl (\m' i -> M.insert i (ty,c) m') m ids, c))
    return []
 
compileStm (SInit ty i e) = do
    modify (\(m, c) ->  (M.insert i (ty,c) m, c))
    s_e <- compileExp Nested e
    v <- getVarName i
    return $
        s_e ++
        [s_local_set v]

compileStm (SReturn e) = do
    s_e <- compileExp Nested e
    return $
      s_e++
      [s_return]

compileStm SReturnVoid = return []
-- compileStm (SWhile cond s) = do
    -- use `pushPop $ compileStm s`
    -- use `[ s_block ... ]` and `[ s_loop ]`
    -- proceed as in fibonacci.wat
-- compileStm (SBlock stms) = do
    -- use `mapM` to interate `compileStm` over the list `stms`
    -- you may want to use `concat :: [[a]] -> [a]` (hoogle it)
-- compileStm s@(SIfElse cond s1 s2) = do
    -- we have to specify the return type of the if/then/else block
-- delete the line below after implementing the above
compileStm _ = return []

-- computes the return type of the given statement.
-- if a return x statement occurs, getReturn returns the type of x
-- otherwise getReturn returns Type_void
getReturn :: MonadState Env m => Stm -> m Type
getReturn (SExp _) = return Type_void
getReturn (SDecls _ _) = return Type_void
getReturn (SInit _ _ _) = return Type_void
getReturn (SReturn e) = getType e
getReturn SReturnVoid = return Type_void
getReturn (SWhile _ s) = pushPop $ getReturn s
getReturn (SBlock xs) = do
    pushPop $ foldM (\t x -> case t of
        Type_void -> getReturn x
        _ -> return t) Type_void xs 
getReturn (SIfElse _ s1 s2) = do
    t1 <- pushPop $ getReturn s1
    t2 <- pushPop $ getReturn s2
    return $ if t1 == Type_void || t2 == Type_void then Type_void else t1
    
-- returns the type of the given expression. 
-- assumes that the expression is already well-typed
getType :: MonadState Env m => Exp -> m Type
getType ETrue = return $ Type_bool
getType EFalse = return $ Type_bool
getType (EInt i) = return $ Type_int
getType (EDouble i) = return $ Type_double
getType (EId id) = do
    (m,_) <- get
    let (ty,_) = m M.! id
    return ty
getType (EApp id _) = getType (EId id)
getType (EPIncr e) = getType e
getType (EPDecr e) = getType e
getType (EIncr e) = getType e
getType (EDecr e) = getType e
getType (ETimes e _) = getType e
getType (EDiv e _) = getType e
getType (EPlus e _) = getType e
getType (EMinus e _) = getType e
getType (ELt _ _) = return $ Type_bool
getType (EGt _ _) = return $ Type_bool
getType (ELtEq _ _) = return $ Type_bool
getType (EGtEq _ _) = return $ Type_bool
getType (EEq _ _) = return $ Type_bool
getType (ENEq _ _) = return $ Type_bool
getType (EAnd _ _) = return $ Type_bool
getType (EOr _ _) = return $ Type_bool
getType (EAss e _) = getType e
getType (ETyped e _) = getType e

-- WASM is based on a stack machine
-- toplevel expressions (such as "x" and "3") are not pushed on the stack
data Nesting = TopLevel | Nested deriving Eq

compileExp :: MonadState Env m => Nesting -> Exp -> m [SExp]
compileExp n ETrue = return $ if n == Nested then [s_i32_const 1] else []

compileExp n EFalse = return $ if n == Nested then [s_i32_const 0] else []

compileExp n (EInt i) = return $ if n == Nested then [s_i32_const i] else []

compileExp n (EDouble i) = return $ if n == Nested then [s_f64_const i] else []

compileExp n (EId i) = do
    v <- getVarName i
    return $ if n == Nested then [s_local_get v] else []

compileExp n x@(EApp (Id i) args) = do
    s_args <- mapM (compileExp Nested) args
    ty <- getType x
    return $
        concat s_args ++
        [s_call i] ++
        if n == TopLevel && ty /= Type_void then [s_drop] else []

compileExp n (EIncr id@(EId i)) = do
    t <- getType id
    v <- getVarName i
    if t == Type_double then return
            [s_local_get v, s_f64_const 1, s_f64_add, s_local_tee v]
        else return $
            [s_local_get v, s_i32_const 1, s_i32_add, s_local_tee v]
compileExp n (EPIncr id@(EId i)) = do
    t <- getType id
    v <- getVarName i
    if t == Type_double then return
            [s_local_get v, s_local_get v, s_f64_const 1,  s_f64_add, s_local_set v]
        else return
            [s_local_get v, s_local_get v, s_i32_const 1, s_i32_add, s_local_set v]
compileExp n (EDecr id@(EId i)) = do
    t <- getType id
    v <- getVarName i
    if t == Type_double then return
        [s_local_get v, s_f64_const 1, s_f64_sub, s_local_tee v] 
    else return
        [s_local_get v, s_i32_const 1, s_i32_sub, s_local_tee v]
compileExp n (EPDecr id@(EId i)) = do
    t <- getType id
    v <- getVarName i
    if t == Type_double then return
            [s_local_get v, s_local_get v, s_f64_const 1,  s_f64_sub, s_local_set v]
        else return
            [s_local_get v, s_local_get v, s_i32_const 1, s_i32_sub, s_local_set v]

compileExp n (ETimes e1 e2) = compileArith e1 e2 s_i32_mul s_f64_mul
compileExp n (EDiv e1 e2)   = compileArith e1 e2 s_i32_div_s s_f64_div
compileExp n (EPlus e1 e2)  = compileArith e1 e2 s_i32_add s_f64_add
compileExp n (EMinus e1 e2) = compileArith e1 e2 s_i32_sub s_f64_sub
compileExp n (ELt e1 e2)    = compileArith e1 e2 s_i32_lt_s s_f64_lt
compileExp n (EGt e1 e2)    = compileArith e1 e2 s_i32_gt_s s_f64_gt
compileExp n (ELtEq e1 e2)  = compileArith e1 e2 s_i32_le_s s_f64_le
compileExp n (EGtEq e1 e2)  = compileArith e1 e2 s_i32_ge_s s_f64_ge
compileExp n (EEq e1 e2)    = compileArith e1 e2 s_i32_eq s_f64_eq
compileExp n (ENEq e1 e2)   = compileArith e1 e2 s_i32_ne s_f64_ne

compileExp _ (EAnd e1 e2) = do
    s_e1 <- compileExp Nested e1
    s_e2 <- compileExp Nested e2
    if s_e1 == [s_i32_const 1] && s_e2 == [s_i32_const 1] then return $
        [s_i32_const 1] 
    else return $
        [s_i32_const 0]
compileExp _ (EOr e1 e2) = do
    s_e1 <- compileExp Nested e1
    s_e2 <- compileExp Nested e2
    if s_e1 == [s_i32_const 1] || s_e2 == [s_i32_const 1] then return $
        [s_i32_const 1] 
    else return $
        [s_i32_const 0]

compileExp n (EAss (EId i) e) = do
    s_e <- compileExp Nested e
    v <- getVarName i
    return $ if n == Nested then s_e ++ [s_local_tee v] else s_e ++ [s_local_set v]
        
compileExp n (ETyped e _) = compileExp n e
-- delete after implementing the above
compileExp _ _ = return []

compileArith e1 e2 intOp doubleOp = do
    s_e1 <- compileExp Nested e1
    s_e2 <- compileExp Nested e2
    t <- getType e1
    case t of 
        Type_double -> return $
            s_e1 ++
            s_e2 ++
            [doubleOp]
        _ -> return $
            s_e1 ++
            s_e2 ++
            [intOp]
