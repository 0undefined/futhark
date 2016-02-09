{-# LANGUAGE FlexibleContexts #-}
module Futhark.Analysis.Usage
       ( usageInBinding
       , usageInExp
       , usageInLambda

       , UsageInOp(..)
       )
       where

import Data.Monoid
import qualified Data.HashSet as HS

import Futhark.Representation.AST
import Futhark.Representation.AST.Attributes.Aliases
import qualified Futhark.Analysis.UsageTable as UT

usageInBinding :: (Attributes lore, Aliased lore, UsageInOp (Op lore)) =>
                  Binding lore -> UT.UsageTable
usageInBinding (Let pat lore e) =
  mconcat [usageInPat,
           usageInExpLore,
           usageInExp e,
           UT.usages (freeInExp e)]
  where usageInPat =
          UT.usages (mconcat (map freeIn $ patternElements pat)
                     `HS.difference`
                     HS.fromList (patternNames pat))
          <> mconcat (map consumptionInPatElem $ patternElements pat)
        usageInExpLore =
          UT.usages $ freeIn lore
        consumptionInPatElem (PatElem _ (BindInPlace _ src _) _) =
          UT.consumedUsage src
        consumptionInPatElem _ =
          mempty

usageInExp :: (Aliased lore, UsageInOp (Op lore)) => Exp lore -> UT.UsageTable
usageInExp (Apply _ args _) =
  mconcat [ mconcat $ map UT.consumedUsage $
            HS.toList $ subExpAliases arg
          | (arg,d) <- args, d == Consume ]
usageInExp (DoLoop _ merge _ _) =
  mconcat [ mconcat $ map UT.consumedUsage $
            HS.toList $ subExpAliases se
          | (v,se) <- merge, unique $ paramDeclType v ]
usageInExp (Op op) =
  usageInOp op
usageInExp _ = UT.empty

class UsageInOp op where
  usageInOp :: op -> UT.UsageTable

instance UsageInOp () where
  usageInOp () = mempty

usageInLambda :: (Aliased lore, UsageInOp (Op lore)) =>
                 Lambda lore -> [VName] -> UT.UsageTable
usageInLambda lam arrs =
  mconcat $
  map (UT.consumedUsage . snd) $
  filter ((`HS.member` consumed_in_body) . fst) $
  zip (map paramName arr_params) arrs
  where arr_params = snd $ splitAt n $ lambdaParams lam
        consumed_in_body = consumedInBody $ lambdaBody lam
        n = length arrs
