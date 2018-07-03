module TLV where
import Data.Word
import RFC4271
-- import Data.ByteString.Builder
-- import Data.Monoid((<>))
--import Data.ByteString(ByteString)
--import qualified Data.ByteString as B
---- import Data.ByteString.Lazy()
--import qualified Data.ByteString.Lazy as L

-- import qualified Data.Attoparsec.ByteString as A

-- parse/deparse the Open message, especially the optional parametes//capabilities
-- the optional parameter field has a (8bit) length sub-field followed by 0 or more 'parameters
-- parameters are coded as TLVs with 8bit T and L
-- only one parameter is currently used - '2' == Capabilities Optional Parameter
-- (see RFC5492)
-- within the 'Capabilities Optional Parameter' is another TLV encoding,
-- although there is no length field to start
--
-- open processing consists of a function useable within the FSM to process and generate Open messages including optional capabilities
-- this includes the logic to handle incompatible capability objectives
-- it also includes processing of the Hold timer values, AS number and BGPID
--
-- from the 'application level' it accepts parameters and shows what has been negotiated
-- 
-- the application level builds an initial state object
-- this is called by the FSM when it receives Open or wants to send Open (either ordering)
-- the response after receiving Open can be +ve or -ve, which should result in either Keepalive or Notification message
-- the underlying logic uses a copy of the resepective 'offers'
-- once keepalive or notification has been confirmed from remote side then the result is confirmed
--
-- the semanitics of the capabilities etc is transparent to this mechanism - i.e.
-- the application layer should simply provide a list of tuples representing
-- the objectives
--
-- Structure of an 'offer'
-- Hold Time
-- My AS
-- My BGPID
-- an (opaque) list of type/value tuples - 'offer'
--
-- additionally the application level can/should specify requirements
-- remote AS
-- remote BGPID
-- required type/value tuples
-- excluded type/value tuples
--
-- these may all be null in which case any offer is accepted
--
-- once an Open has been received a response is available
-- the Open populates a 'remote' offer structure, of identical form to the 'local' one
--
-- getResponse calculates the compliance of the remote offer with the local requirement
-- the response includes both a text level message and a Notification
-- if the offer is acceptable then the response is an empty list
-- note that RFC4271 only allows a single error to be supplied in the Notification message
-- however that does not prevent this implementation from provding multiple reasons for rejection
-- the FSM can choose - probably by just taking the first i nthe list
--
-- getStatus provides the results of the exchange, including the agreed optional capabilities
--
data OpenStateMachine = OpenStateMachine {localOffer :: Offer , remoteOffer :: Maybe Offer, required :: Required}
type Offer = { myAS :: Word16, holdTime :: Word16, bgpID :: Word32, optionalCapabilities :: TLVS }
type Required { myAS :: Maybe Word16, Maybe holdTime :: Word16, Maybe bgpID :: Word32, requiredCapabilities :: TLVS}
type TLVS = [(Word8,ByteString)]
type NotifyMsgs = [(Word8,Word8,TLVS)]

makeOpenStateMachine :: Offer -> Required -> OpenStateMachine
makeOpenStateMachine offer required = OpenStateMachine offer Nothing required
updateOpenStateMachine :: OpenStateMachine -> Offer -> OpenStateMachine
updateOpenStateMachine osm offer = osm { required = offer }

getStatus :: OpenStateMachine -> Maybe Offer 
getStatus osm | isJust (remoteOffer osm) = Offer ( myAS $ remoteOffer osm)  negotiatedHoldTime ( bgpID $ remoteOffer osm) negotiatedOptionalCapabilities where
    negotiatedOptionalCapabilities = intersection (optionalCapabilities $ remoteOffer osm) (optionalCapabilities $ localOffer osm)
    negotiatedHoldTime = min ( holdTime $ remoteOffer osm) ( holdTime $ localOffer osm)

getResponse :: OpenStateMachine -> offer -> NotifyMsgs
getResponse osm | isJust (remoteOffer osm) = catMaybes [checkmyAS : checkBgpID : checkHoldTime : checkOptionalCapabilities] where
    remoteOffer = remoteOffer osm
    required    = required osm
    status = fromJust $ getStatus osm
    checkBgpID = maybe Nothing
                      (\requiredBGPID -> if (bgpID remoteOffer) == requiredBGPID then Nothing else Just (_Notification_OPEN_Message_Error,_Notification_OPEN_Subcode_Bad_BGP_Identifier,[])
                      (bgpID required)
    checkHoldTime = maybe Nothing
                      (\minimumHoldTime -> if (holdTime required) > (holdTime status)
                                               then Just (_Notification_OPEN_Message_Error,_Notification_OPEN_Subcode_Unacceptable_Hold_Time,[])
                                               else Nothing
                      (holdTime required)
    checkmyAS = maybe Nothing
                      (\requiredAS -> if (myAS remoteOffer) == requiredAS then Nothing else Just (_Notification_OPEN_Message_Error,_Notification_OPEN_Subcode_Bad_Peer_AS,[])
                      (myAS required)
    checkOptionalCapabilities = let missingCapabilies = optionalCapabilities required \\ optionalCapabilities remoteOffer in
                                if null missingCapabilies
                                    then Nothing
                                    else Just (_Notification_OPEN_Message_Error,_Notification_OPEN_Subcode_Unsupported_Capability,missingCapabilies)
