module RIB where
import qualified Data.Map as Map
import RIBData


class Rib where 
    update :: Rib -> Peer -> Prefix -> Route -> (Maybe (Peer,Route),(Peer,Route))
    -- withdraw :: Rib -> Peer -> Prefix -> Maybe (Peer,Route)
    -- removePeer  :: Rib -> Peer -> [Prefix, (Peer,Route)]
    -- lookup :: Rib -> Prefix -> Maybe (Peer,Route)
    -- mkRib :: ((Peer,Route) -> (Peer,Route) -> Ordering) -> Rib

data Rib = Rib { fSel :: (Peer,Route) -> (Peer,Route) -> Ordering,
                 locRib :: Map.Map Prefix (Peer,Route),
                 adjRibIn :: Map.Map Prefix (Map.Map Peer Route) }

instance Rib Rib where
    update rib peer prefix route = 

