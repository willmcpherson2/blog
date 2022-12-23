module Markup.M (m) where

import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Markup.Compile.Exp (compile)

m :: QuasiQuoter
m =
  QuasiQuoter
    { quoteExp = compile,
      quotePat = undefined,
      quoteType = undefined,
      quoteDec = undefined
    }
