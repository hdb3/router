module RFC4271 where
import Data.Word
{-
         Name                       Value      Definition
         ------------               -----      ----------
         Message Header Error       1          Section 6.1
         OPEN Message Error         2          Section 6.2
         UPDATE Message Error       3          Section 6.3
         Hold Timer Expired         4          Section 6.5
         Finite State Machine Error 5          Section 6.6
         Cease                      6          Section 6.7
-}
_Notification_Message_Header_Error       = 1 :: Word8
_Notification_OPEN_Message_Error         = 2 :: Word8
_Notification_UPDATE_Message_Error       = 3 :: Word8
_Notification_Hold_Timer_Expired         = 4 :: Word8
_Notification_Finite_State_Machine_Error = 5 :: Word8
_Notification_Cease                      = 6 :: Word8
