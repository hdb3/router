{-- LANGUAGE DisambiguateRecordFields #-} 
module Open where
import Data.Word
import Data.Maybe(isJust,fromJust,catMaybes,listToMaybe)
import Data.List(intersect,(\\))
import RFC4271
import Capabilities(Capability,eq_)
import BGPparse

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
data OpenStateMachine = OpenStateMachine {localOffer :: Offer , remoteOffer :: Maybe Offer, required :: Required} deriving Show
data Offer = Offer { myAS :: Word16, offeredHoldTime :: Word16, offeredBGPid :: Word32, optionalCapabilities :: [Capability] } deriving Show
data Required = Required { requiredAS :: Maybe Word16, requiredHoldTime :: Maybe Word16, requiredBgpID :: Maybe Word32, requiredCapabilities :: [Capability]} deriving Show

makeOpenStateMachine :: Offer -> Required -> OpenStateMachine
makeOpenStateMachine offer required = OpenStateMachine offer Nothing required

updateOpenStateMachine :: OpenStateMachine -> Offer -> OpenStateMachine
updateOpenStateMachine osm offer = osm { remoteOffer = Just offer }

getStatus :: OpenStateMachine -> Offer 
getStatus osm | isJust ( remoteOffer osm ) = Offer ( myAS offer)  negotiatedHoldTime ( offeredBGPid offer) negotiatedOptionalCapabilities where
                                                 offer = fromJust $ remoteOffer osm
                                                 negotiatedOptionalCapabilities = intersect (optionalCapabilities . fromJust $ remoteOffer osm) (optionalCapabilities $ localOffer osm)
                                                 negotiatedHoldTime = min ( offeredHoldTime . fromJust $ remoteOffer osm) ( offeredHoldTime $ localOffer osm)

-- getResponse should not be called before an OPEN message has been received
-- either a Keepalive is returned or the needed rejection message
getResponse :: OpenStateMachine -> BGPMessage
getResponse osm | isJust ( remoteOffer osm ) = firstMaybe [checkmyAS , checkBgpID , checkHoldTime , checkOptionalCapabilities, keepalive] where
        firstMaybe [] = undefined
        firstMaybe (Just m : mx) = m
        firstMaybe (Nothing : mx) = firstMaybe mx

        remoteOffer' = fromJust $ remoteOffer osm
        required' = required osm

        keepalive = Just BGPKeepalive

        checkBgpID :: Maybe BGPMessage
        checkBgpID =
            maybe Nothing
                  (\requirement -> if offeredBGPid remoteOffer' == requirement
                      then Nothing
                      else Just (BGPNotify NotificationOPENMessageError BadBGPIdentifier []))
                  (requiredBgpID required')

        checkHoldTime :: Maybe BGPMessage
        checkHoldTime = maybe Nothing
            (\requirement -> if requirement > offeredHoldTime ( getStatus osm )
                then Just (BGPNotify NotificationOPENMessageError UnacceptableHoldTime [])
                else Nothing)
            (requiredHoldTime required')

        checkmyAS :: Maybe BGPMessage
        checkmyAS = maybe Nothing
            (\requirement -> if myAS remoteOffer' == requirement
                                then Nothing
                                else Just (BGPNotify NotificationOPENMessageError BadPeerAS []))
            (requiredAS required')

-- a naive check looks for identical values in capabilities,
-- which is how the RFC is worded
-- However, in practice the requirement differs for each specific case, and in fact is not
-- clearly defined in some cases.  The minimal requirement appears to be a check for simple presence, with no comparison
-- of value.  This is clearly true for two common cases; AS4/32-bit ASNs, and Graceful Restart.
-- Note - there is no negotiation concept for Optional capabilities, and the responsibility for rejecting a peering lies with the prposer of a capability
-- which should arise when the peer has not advertised a capability which is required.
-- The present implementation consists simply of a check that the remote offer contains at least the capabilities in the required list.
--  
--  
-- this is the mentioned check for presecnce in remote offer of required parameters
-- return a list of capabilities required but not found in the offer
        checkOptionalCapabilities :: Maybe BGPMessage
        checkOptionalCapabilities = if null missingCapabilities then Nothing else Just (BGPNotify NotificationOPENMessageError UnsupportedOptionalParameter missingCapabilities) where
            required = requiredCapabilities required'
            offered  = optionalCapabilities remoteOffer'
            missingCapabilities = check required
            check [] = []
            check (c:cx) = if any (eq_ c) offered then check cx else c : check cx
