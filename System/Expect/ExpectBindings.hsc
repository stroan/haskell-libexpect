{-# LANGUAGE ForeignFunctionInterface #-}
module System.Expect.ExpectBindings where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import IO

#include "expect_config.h"

#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

newtype ExpType = ExpType { unExpType :: CInt }

data ExpCase = ExpCase { pattern :: CString
     	       	       , regexp :: Ptr ()
		       , exptype :: ExpType
		       , value :: CInt }

instance Storable ExpCase where
  alignment _ = #{alignment exp_case}
  sizeOf _ = #{size exp_case}
  peek ptr = do
    p <- #{peek exp_case, pattern} ptr
    r <- #{peek exp_case, re} ptr
    t <- #{peek exp_case, type} ptr
    v <- #{peek exp_case, value} ptr
    return $ ExpCase p r (ExpType t) v
  poke ptr (ExpCase p r t v) = do
    (#poke exp_case, pattern) ptr p
    (#poke exp_case, re) ptr r
    (#poke exp_case, type) ptr (unExpType t)
    (#poke exp_case, value) ptr v

#{enum ExpType, ExpType
  , expExact = exp_exact
  , expRegexp = exp_regexp
  , expGlob = exp_glob
  , expNull = exp_null
  , expEnd = exp_end
 }

foreign import ccall "&" exp_loguser :: Ptr CInt

foreign import ccall exp_popen :: CString -> IO (Ptr CFile)
foreign import ccall exp_fexpectv :: Ptr CFile -> Ptr ExpCase -> IO CInt
