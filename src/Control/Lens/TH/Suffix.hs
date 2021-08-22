module Control.Lens.TH.Suffix (suffixRules) where

import           Control.Lens    ((&), (.~))
import           Control.Lens.TH (LensRules, lensField, lensRules, mappingNamer)

suffixRules :: LensRules
suffixRules = lensRules & lensField .~ mappingNamer appendL where
  appendL = pure . (<> "L")
