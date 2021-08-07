module TH (suffixRules) where

import Control.Lens
import Control.Lens.TH

suffixRules :: LensRules
suffixRules = lensRules
            & lensField .~ mappingNamer (pure . (<> "L"))

