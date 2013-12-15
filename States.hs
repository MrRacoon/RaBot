module States where

import Types

checkState :: Bool -> C_State -> Bool
checkState _     Never   = False
checkState _     Always  = True
checkState True  Active  = True
checkState False Passive = True
checkState _    _        = False


