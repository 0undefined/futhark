{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
-- | This module defines a translation from imperative code with
-- kernels to imperative code with OpenGL calls.
module Futhark.CodeGen.ImpGen.Kernels.ToOpenGL
  ( kernelsToOpenGL
  )
  where

import Control.Monad.State
import Control.Monad.Identity
import Control.Monad.Writer
import Control.Monad.Reader
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Map.Strict as M

import qualified Language.C.Syntax as C
import qualified Language.C.Quote as C

import qualified Language.C.Quote.OpenCL as C

import Futhark.Error
import qualified Futhark.CodeGen.Backends.GenericC as GenericC
import Futhark.CodeGen.Backends.SimpleRepresentation
import Futhark.CodeGen.ImpCode.Kernels hiding (Program)
import qualified Futhark.CodeGen.ImpCode.Kernels as ImpKernels
import Futhark.CodeGen.ImpCode.OpenGL hiding (Program)
import qualified Futhark.CodeGen.ImpCode.OpenGL as ImpOpenGL
import Futhark.MonadFreshNames
import Futhark.Util (zEncodeString)

kernelsToOpenGL :: ImpKernels.Program
                -> Either InternalError ImpOpenGL.Program
kernelsToOpenGL = translateKernels

-------------------------------
--TODO: Accommodate OpenGL

-- | Translate a kernels-program to an OpenGL-program.
translateKernels :: ImpKernels.Program
                 -> Either InternalError ImpOpenGL.Program
translateKernels (ImpKernels.Functions funs) = do
  (prog', ToOpenGL shaders sizes) <-
    runWriterT $ fmap Functions $ forM funs $ \(fname, fun) ->
    (fname,) <$> runReaderT (traverse (onHostOp fname) fun) fname
  let shaders'       = M.map fst shaders
      opengl_code    = openGlCode $ map snd $ M.elems shaders
      opengl_prelude = pretty $ genOpenGlPrelude
  return $ ImpOpenGL.Program opengl_code opengl_prelude shaders'
    sizes prog'
  --return $ ImpOpenGL.Program undefined undefined undefined


type LocalMemoryUse = (VName, Count Bytes Exp)

newtype KernelRequirements =
  KernelRequirements { kernelLocalMemory :: [LocalMemoryUse] }

instance Semigroup KernelRequirements where
  KernelRequirements lm1 <> KernelRequirements lm2 =
    KernelRequirements (lm1<>lm2)

instance Monoid KernelRequirements where
  mempty = KernelRequirements mempty

newtype OpenGlRequirements =
  OpenGlRequirements { openglUsedTypes :: S.Set PrimType }

instance Semigroup OpenGlRequirements where
  OpenGlRequirements ts1 <> OpenGlRequirements ts2 =
    OpenGlRequirements (ts1 <> ts2)

instance Monoid OpenGlRequirements where
  mempty = OpenGlRequirements mempty

data ToOpenGL = ToOpenGL { glShaders :: M.Map ShaderName (Safety, C.Func)
                         , glSizes :: M.Map Name SizeClass
                         }

instance Semigroup ToOpenGL where
 ToOpenGL k1 sz1 <> ToOpenGL k2 sz2 =
   ToOpenGL (k1<>k2) (sz1<>sz2)

instance Monoid ToOpenGL where
 mempty = ToOpenGL mempty mempty

type OnKernelM = ReaderT Name (WriterT ToOpenGL (Either InternalError))

onHostOp :: kernelsToOpenGL -> HostOp -> OnKernelM OpenGL
--TODO: onHostOp target (CallKernel k) = onKernel target k
onHostOp _ (ImpKernels.GetSize v key size_class) = do
 tell mempty { glSizes = M.singleton key size_class }
 return $ ImpOpenGL.GetSize v key
onHostOp _ (ImpKernels.CmpSizeLe v key size_class x) = do
 tell mempty { glSizes = M.singleton key size_class }
 return $ ImpOpenGL.CmpSizeLe v key x
onHostOp _ (ImpKernels.GetSizeMax v size_class) =
 return $ ImpOpenGL.GetSizeMax v size_class


openGlCode :: [C.Func] -> String
openGlCode kernels =
 pretty [C.cunit|$edecls:funcs|]
 where funcs =
         [[C.cedecl|$func:kernel_func|] |
          kernel_func <- kernels ]

genOpenGlPrelude :: [C.Definition]
genOpenGlPrelude =
   [C.cunit|
 typedef char int8_t;
 typedef short int16_t;
 typedef int int32_t;
 typedef long int64_t;

 typedef uchar uint8_t;
 typedef ushort uint16_t;
 typedef uint uint32_t;
 typedef ulong uint64_t;
 |]
