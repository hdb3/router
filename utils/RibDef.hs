module RIB where
import qualified Data.Map.Strict as Map
import qualified Data.List
import Data.Maybe(fromMaybe)
import RIBData

class Rib rib where 
    update :: rib -> Prefix -> Peer -> Route -> (rib, ( Maybe (Peer,Route),Maybe (Peer,Route)))
    -- withdraw :: Rib -> Prefix -> Peer -> Maybe (Peer,Route)
    -- removePeer  :: Rib -> Peer -> [Prefix, (Peer,Route)]
    -- lookup :: Rib -> Prefix -> Maybe (Peer,Route)
    -- mkRib :: ((Peer,Route) -> (Peer,Route) -> Ordering) -> Rib

data MapRib = MapRib { fSel :: (Peer,Route) -> (Peer,Route) -> Ordering
                     ,  locRib :: Map.Map Prefix (Peer,Route)
                     ,  adjRibIn :: Map.Map Prefix (Map.Map Peer Route) }

instance Rib MapRib where
    update rib prefix peer route = (rib',result) where
        rib' = MapRib { fSel = fSel rib, locRib = locRib', adjRibIn = adjRibIn' }

        -- updating adjRibIn - 

        --    when the prefix is not present then the input target for the new peer value at this prefix is the empty map
        oldPrefixMap = fromMaybe Map.empty ( Map.lookup prefix (adjRibIn rib))

        -- insert function simply overwrites any existing entry - unless it was also the best route we don't care...
        --     because if it is/was best route it will be in locRib too, so no need to worry here
        newPrefixMap = Map.insert peer route oldPrefixMap

        -- similarly, we can simply replace the old one with the new one here....
        adjRibIn' = Map.insert prefix newPrefixMap ( adjRibIn rib)

        -- now check if the new best route has changed for this prefix
        -- first, calculate the new best route:
        newBestRoute = Data.List.maximumBy (fSel rib) (Map.toList newPrefixMap)
        -- there is a subtle point next, regarding comparing the new and old best routes....
        --     if the new best route is to a different peer then it is clearly a change, which must be disseminated
        --     but if the new and old calculated route is from the same peer we would need to know if it is different at the route level
        --     this would require an Eq instance for Route, which we have decalred we don't need...
        --     BUT.... there is an easy way round this - if the new best route is from the peer triggereing this update then 
        --     this is a change, otherwise if the best route is from another peer then it is not - so no need to compare routes, ever....
        -- TLDR summary - check the peer in the calculated new best route - only iff it is the peer given in the update request is it a change of route

        bestRouteChanged = (fst newBestRoute) == peer

        locRib' = if bestRouteChanged then Map.insert prefix newBestRoute (locRib rib) else (locRib rib) 

        -- we are now done building the update Rib values and need to construct the 'result' value which is a '(Maybe (Peer,Route),(Peer,Route))'
        result = if bestRouteChanged then (oldBestRoute,Just newBestRoute) else (Nothing,Nothing)
        -- note, we only need to know now what was the previous best route, so 'locRib' actually is never touched unless it needs to change
        -- also note, as an optimisation (***TODO measure the impact?), the locRib can be updated and queried simultaneously with 'insertLookupWithKey'
        --    (note - 'updateLookupWithKey' does not always return the old value, insertLookupWithKey does).

        -- this is the simple version.....
        oldBestRoute = Map.lookup prefix (locRib rib)

        -- optimised version: 
        -- require a simpler function than
        -- insertLookupWithKey :: Ord k => (k -> a -> a -> a) -> k -> a -> Map k a -> (Maybe a, Map k a)
        -- where the first function parameter 'f' is shown as f key new_value old_value
        --  - so insertLookupWithKey enables a function over a value 'a' to generate the actual new stored value,
        --    whereas we knwo that we just want to specify the new value as an input which will not be modified - so, 
        --    our function is insertLookup :: Ord k => k -> a -> Map k a -> (Maybe a, Map k a), derived from insertLookupWithKey with the function
        --    f key new_value old_value = new_value

        insertLookup :: Ord k => k -> a -> Map.Map k a -> (Maybe a, Map.Map k a)
        insertLookup = Map.insertLookupWithKey f where f _ new_value _ = new_value

        -- and the derived results are just:
        (_oldBestRoute,_locRib') = insertLookup prefix newBestRoute (locRib rib)





