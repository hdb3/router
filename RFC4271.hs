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

{-

 This document defines the following Message Header Error subcodes:

         Name                         Value        Definition
         --------------------         -----        ----------
         Connection Not Synchronized   1           See Section 6.1
         Bad Message Length            2           See Section 6.1
         Bad Message Type              3           See Section 6.1

   This document defines the following OPEN Message Error subcodes:

         Name                         Value        Definition
         --------------------         -----        ----------
         Unsupported Version Number     1          See Section 6.2
         Bad Peer AS                    2          See Section 6.2
         Bad BGP Identifier             3          See Section 6.2
         Unsupported Optional Parameter 4          See Section 6.2
         [Deprecated]                   5          See Appendix A
         Unacceptable Hold Time         6          See Section 6.2

    This document defines the following UPDATE Message Error subcodes:

         Name                             Value    Definition
         --------------------              ---     ----------
         Malformed Attribute List           1      See Section 6.3
         Unrecognized Well-known Attribute  2      See Section 6.3
         Missing Well-known Attribute       3      See Section 6.3
         Attribute Flags Error              4      See Section 6.3
         Attribute Length Error             5      See Section 6.3
         Invalid ORIGIN Attribute           6      See Section 6.3
         [Deprecated]                       7      See Appendix A
         Invalid NEXT_HOP Attribute         8      See Section 6.3
         Optional Attribute Error           9      See Section 6.3
         Invalid Network Field             10      See Section 6.3
         Malformed AS_PATH                 11      See Section 6.3

-}
_Notification_Header_Subcode_Connection_Not_Synchronized      = 1 :: Word8
_Notification_Header_Subcode_Bad_Message_Length               = 2 :: Word8
_Notification_Header_Subcode_Bad_Message_Type                 = 3 :: Word8

_Notification_OPEN_Subcode_Unsupported_Version_Number         = 1 :: Word8
_Notification_OPEN_Subcode_Bad_Peer_AS                        = 2 :: Word8
_Notification_OPEN_Subcode_Bad_BGP_Identifier                 = 3 :: Word8
_Notification_OPEN_Subcode_Unsupported_Optional_Parameter     = 4 :: Word8
-- _Notification_OPEN_Subcode_Deprecated                         = 5 :: Word8
_Notification_OPEN_Subcode_Unacceptable_Hold_Time             = 6 :: Word8
_Notification_OPEN_Subcode_Unsupported_Capability             = 7 :: Word8 -- from RFC5492

class Enum e => EnumWord8 e where
    decode :: Word8 -> e
    decode = toEnum . fromIntegral
    encode :: e -> Word8
    encode = fromIntegral . fromEnum

data EnumNotificationCode = UnsupportedVersionNumber | BadPeerAS | BadBGPIdentifier | UnsupportedOptionalParameter | UnacceptableHoldTime
                            deriving (Show,Eq)
instance Enum EnumNotificationCode where
    toEnum n | n == 1 = UnsupportedVersionNumber
             | n == 2 = BadPeerAS
             | n == 3 = BadBGPIdentifier
             | n == 4 = UnsupportedOptionalParameter
             | n == 6 = UnacceptableHoldTime

    fromEnum e | e == UnsupportedVersionNumber = 1
               | e == BadPeerAS = 2
               | e == BadBGPIdentifier = 3
               | e == UnsupportedOptionalParameter = 4
               | e == UnacceptableHoldTime = 6

