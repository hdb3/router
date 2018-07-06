module RFC4271 where
import Data.Word
class Enum e => EnumWord8 e where
    decode :: Word8 -> e
    decode = toEnum . fromIntegral
    encode :: e -> Word8
    encode = fromIntegral . fromEnum

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
data EnumNotificationCode = InvalidNotificationError |
                            NotificationMessageHeaderError |
                            NotificationOPENMessageError |
                            NotificationUPDATEMessageError |
                            NotificationHoldTimerExpired |
                            NotificationFiniteStateMachineError |
                            NotificationCease
                            deriving (Show,Eq)

instance Enum EnumNotificationCode where
    toEnum n | n == 0 = InvalidNotificationError
             | n == 1 = NotificationMessageHeaderError
             | n == 2 = NotificationOPENMessageError
             | n == 3 = NotificationUPDATEMessageError
             | n == 4 = NotificationHoldTimerExpired
             | n == 5 = NotificationFiniteStateMachineError
             | n == 6 = NotificationCease

    fromEnum e | e == InvalidNotificationError = 0
               | e == NotificationMessageHeaderError = 1
               | e == NotificationOPENMessageError = 2
               | e == NotificationUPDATEMessageError = 3
               | e == NotificationHoldTimerExpired = 4
               | e == NotificationFiniteStateMachineError = 5
               | e == NotificationCease = 6

instance EnumWord8 EnumNotificationCode where


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

data EnumNotificationOpenSubcode = InvalidOpenSubcode | UnsupportedVersionNumber | BadPeerAS | BadBGPIdentifier | UnsupportedOptionalParameter | UnacceptableHoldTime
                            deriving (Show,Eq)
instance Enum EnumNotificationOpenSubcode where
    toEnum n | n == 0 = InvalidOpenSubcode
             | n == 1 = UnsupportedVersionNumber
             | n == 2 = BadPeerAS
             | n == 3 = BadBGPIdentifier
             | n == 4 = UnsupportedOptionalParameter
             | n == 6 = UnacceptableHoldTime

    fromEnum e | e == InvalidOpenSubcode = 0
               | e == UnsupportedVersionNumber = 1
               | e == BadPeerAS = 2
               | e == BadBGPIdentifier = 3
               | e == UnsupportedOptionalParameter = 4
               | e == UnacceptableHoldTime = 6

instance EnumWord8 EnumNotificationOpenSubcode where
