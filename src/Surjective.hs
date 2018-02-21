{-# LANGUAGE FlexibleContexts, TemplateHaskell, ViewPatterns #-}
module Surjective (Covers, surjective) where

import Control.Lens
import Control.Monad.Except
import Control.Monad.Writer
import Data.Data.Lens
import Language.Haskell.TH
import Language.Haskell.TH.Syntax hiding (lift)
import Text.Printf


-- |
-- When the function given to 'surjective' returns a value, it should call a
-- 'covers' function specifying a pattern. This pattern represents the subset of
-- values which are considered "covered" by this case, so we can then make sure
-- that all the values in the codomain are covered.
--
-- This 'covers' function needs to take a pattern and a value (the value being
-- returned), so we would like to write:
--
-- > covers {pattern} {value}
--
-- But unfortunately, patterns are not first-class in Haskell. So instead we
-- write:
--
-- > covers $ \{pattern} -> {value}
--
-- And we use TemplateHaskell to replace that call with @{value}@. So 'covers'
-- appears to have type @(a -> a) -> a@.
type Covers a = (a -> a) -> a

dollar :: Name
dollar = '($)

isApp :: Exp -> Maybe (Exp, Exp)
isApp (AppE e1 e2)                         = Just (e1, e2)
isApp (InfixE (Just e1)
              (VarE ((== dollar) -> True))
              (Just e2))                   = Just (e1, e2)
isApp _                                    = Nothing

isAppOf :: Name -> Exp -> Maybe Exp
isAppOf name (isApp -> Just ( VarE ((== name) -> True)
                            , e
                            ))                         = Just e
isAppOf _    _                                         = Nothing

replaceCovers :: (MonadError String m, MonadWriter [Pat] m)
              => Name -> Exp -> m Exp
replaceCovers covers (isAppOf covers -> Just e) = case e of
  LamE [coveredPattern] e' -> do
    tell [coveredPattern]
    pure e'
  _ -> do
    throwError $ printf "surjective: expected %s to be applied to a lambda, \
                        \found it applied to %s"
                        (show . ppr $ covers)
                        (show . ppr $ e)
replaceCovers _ e = pure e

detectUnreplacedCovers :: (MonadError String m)
                       => Name -> Exp -> m Exp
detectUnreplacedCovers covers (VarE ((== covers) -> True)) = do
  throwError $ printf "surjective: expected %s to be applied to a lambda, \
                      \found it used as a value"
                      (show . ppr $ covers)
detectUnreplacedCovers _ e = pure e

runExceptQ :: ExceptT String Q a -> Q a
runExceptQ body = do
  r <- runExceptT body
  case r of
    Right x -> pure x
    Left  e -> fail e

-- |
-- Assert that an expression of type 'b' covers all the values of type 'a'.
-- Typically, 'b' is a function of the form @... -> a@, so we are asserting that
-- the function is "surjective", meaning that for every value in its ouput type,
-- there is a combination of input values which will produce it. The idea is
-- that if you add a new constructor to a data type, the compiler should warn
-- you that your parser no longer cover all the outputs, in the same way that
-- @-Wincomplete-patterns@ warns you that your pretty-printer no longer cover
-- all the inputs.
--
-- Here is a parsing function which is missing a case:
--
-- > parseBool :: String -> Maybe Bool
-- > parseBool = \case
-- >   "true" -> Just True
-- >   _      -> Nothing
--
-- And here is how to annotate the function using 'surjective' so the compiler
-- warns us about that missing case:
--
-- > -- Warning: Pattern match(es) are non-exhaustive
-- > -- In a case alternative: Patterns not matched: (Just False)
-- > parseBool :: String -> Maybe Bool
-- > parseBool = $$(surjective
-- >   [||\covers -> \case
-- >     "true" -> covers $ \(Just True) -> Just True
-- >     _      -> covers $ \Nothing     -> Nothing
-- >   ||])
--
-- That is, to check that an expression is surjective:
--
-- 1. Pick a name, typically "'covers'", for the function which asserts that a
--    particular pattern is covered.
-- 2. For each output expression, choose the pattern which this output is
--    considered to cover.
-- 3. This is the weird part: wrap the output expression in a lambda and use the
--    pattern in the binding position. Don't worry about partiality, this is
--    just a syntactic trick to associate the pattern to the output expression.
--    This will be rewritten to just the output expression.
-- 4. Wrap your entire implementation in another lambda, using the name you
--    picked for 'covers' in the binding position. This is another syntactic
--    trick, that lambda will be eliminated as well.
-- 5. Wrap that lambda in a typed quotation @[|| ... ||]@. Make sure the
--    @TemplateHaskell@ language extension is enabled or you'll get a parse
--    error.
-- 6. Call 'surjective' on the typed quotation, and splice the result back into
--    your program using @$$( ... )@. The spliced-in code will contain an unused
--    case expression covering all the patterns given to 'covers', so we will
--    get a @-Wincomplete-patterns@ warning if we forgot a case.
--
-- Since the transformation is entirely syntactic, 'surjective' can be used to
-- check other kinds of coverage conditions, not just surjectivity. For example,
-- here we attempt to list all the values of type @Maybe Bool@, but we are
-- missing a case:
--
-- > listMaybeBools :: [Maybe Bool]
-- > listMaybeBools = [Just True, Nothing]
--
-- Here is how to annotate the list using 'surjective' so the compiler warns us
-- about that missing case:
--
-- > -- Warning: Pattern match(es) are non-exhaustive
-- > -- In a case alternative: Patterns not matched: (Just False)
-- > listMaybeBools :: [Maybe Bool]
-- > listMaybeBools = $$(surjective
-- >   [||\covers -> [ covers $ \(Just True) -> Just True
-- >                 , covers $ \Nothing     -> Nothing
-- >                 ]
-- >   ||])
surjective :: Q (TExp (Covers a -> b))
           -> Q (TExp b)
surjective qBody = do
  LamE [VarP covers] body <- unType <$> qBody
  (body', coveredPatterns) <- runExceptQ
                            . runWriterT
                            . transformMOf uniplate (replaceCovers covers)
                            $ body
  body'' <- runExceptQ
          . transformMOf uniplate (detectUnreplacedCovers covers)
          $ body'

  -- Strangely enough, incomplete-pattern warnings are emitted for generated
  -- case expressions but not for generated functions :(
  -- I would prefer to generate a function because the error message would
  -- contain the function name, which I can use to hint that the function was
  -- generated by 'surjective'.

  ---- a dummy function definition which will trigger a warning if
  ---- 'coveredPatterns' is missing a case:
  ----
  ---- > let covers {coveredPattern1} = ()
  ---- >     covers {coveredPattern2} = ()
  ---- >     covers {coveredPattern3} = ()
  ---- >     ...
  ---- > in {body''}
  --let functionName = mkName ('_' : show (ppr covers))
  --rhs <- [|()|]
  --let functionClause :: Pat -> Clause
  --    functionClause coveredPattern = Clause [coveredPattern] (NormalB rhs) []
  --let functionDef :: Dec
  --    functionDef = FunD functionName (map functionClause coveredPatterns)
  --let letExpr :: Exp
  --    letExpr = LetE [functionDef] body''
  --runIO $ print $ ppr $ letExpr
  --pure $ TExp letExpr

  -- a dummy case expression which will trigger a warning if 'coveredPatterns'
  -- is missing a case:
  --
  -- > let _ = case undefined of
  -- >     {coveredPattern1} -> ()
  -- >     {coveredPattern2} -> ()
  -- >     {coveredPattern3} -> ()
  -- >     ...
  -- > in {body''}
  scrutinee <- [|undefined|]
  rhs <- [|()|]
  let coveredMatch :: Pat -> Match
      coveredMatch coveredPattern = Match coveredPattern (NormalB rhs) []
      caseExp :: Exp
      caseExp = CaseE scrutinee (map coveredMatch coveredPatterns)
  TExp <$> [|let _ = $(pure caseExp) in $(pure body'')|]
