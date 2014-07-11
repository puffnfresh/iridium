module IR.Reader

import Effects

data Reader : Effect where
  Ask : { a } Reader a

instance Handler Reader m where
  handle st Ask k = k st st

READER : Type -> EFFECT
READER t = MkEff t Reader

ask : { [READER x] } Eff x
ask = call Ask
