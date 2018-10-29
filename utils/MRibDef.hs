{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module MRibDef where
import qualified Data.Map.Strict as Map
import qualified Data.List
import Data.Maybe(fromMaybe)
import Control.Monad

--import Prefix
import IP4Prefix
import RIBData

newtype RibT r a = RibT { runRib :: r -> (r,a) }

instance Functor (RibT r) where
  fmap = Control.Monad.liftM

instance Applicative (RibT r) where
  pure = return
  (<*>) = Control.Monad.ap

instance Monad (RibT r) where
  return a = RibT $ \r -> (r, a)

  RibT act >>= k = RibT $ \r ->
    let (r', a) = act r
    in runRib (k a) r'

liftR f r = RibT ( \rib -> f r)


adjustR a b c = liftR $ adjust a b c
updateR a b c = liftR $ update a b c
withdrawR a b = liftR $ withdraw a b 

removePeerR :: Rib r => Peer -> r -> RibT r [(Prefix, (Peer, Route))]
removePeerR a = liftR $ removePeer a 

dumpRibR :: Rib r => r -> RibT r ([(Prefix, (Peer,Route))],[(Prefix, [(Peer,Route)])])
dumpRibR = RibT (\rib -> (rib,dumpRib rib))

mkRibR :: Rib r => ((Peer, Route) -> (Peer, Route) -> Ordering) -> RibT r ()
mkRibR a = RibT (\_ -> (mkRib a,()))

type Prefix = IP4Prefix
class Rib rib where 
    adjust      :: Prefix -> Peer -> Maybe Route -> rib -> (rib, ( Maybe (Peer,Route),Maybe (Peer,Route)))
    update      :: Prefix -> Peer -> Route -> rib -> (rib, ( Maybe (Peer,Route),Maybe (Peer,Route)))
    withdraw    :: Prefix -> Peer -> rib -> (rib, Maybe (Peer,Route))
    removePeer  :: Peer -> rib -> (rib, [(Prefix, (Peer,Route))])
    lookup      :: Prefix -> rib -> Maybe (Peer,Route)
    mkRib       ::                    ((Peer,Route) -> (Peer,Route) -> Ordering) -> rib
    dumpRib     :: rib -> ([(Prefix, (Peer,Route))],[(Prefix, [(Peer,Route)])])

data MapRib = MapRib { fSel :: (Peer,Route) -> (Peer,Route) -> Ordering
                                               , locRib :: Map.Map Int (Peer,Route)
                                               , adjRibIn :: Map.Map Int (Map.Map Peer Route) }

instance Show MapRib where
    show mr = let (loc,adj) = dumpRib mr
              in "locRib:   { " ++ show loc ++ " } \n" ++
              "adjRibIn: { " ++ show adj ++ " }" 

instance Rib MapRib where

    mkRib cmp = MapRib { fSel = cmp , locRib = Map.empty , adjRibIn = Map.empty }

    lookup prefix rib = Map.lookup (toInt prefix) (locRib rib)

    dumpRib rib = (getLocRib rib,getAdjRibIn rib) where
        fi :: Int -> Prefix
        fi = fromInt
        --getLocRib rib = []
        getLocRib rib = map (\(k,v) -> (fi k,v)) $ Map.toAscList (locRib rib)
        getAdjRibIn rib = map (\(k,v) -> (fi k,f v)) $ Map.toAscList (adjRibIn rib) where f = Map.toAscList
        --getLocRib rib = map (\(k,v) -> (fromInt k,v)) $ Map.toAscList (locRib rib)

    removePeer peer rib = (newRib,results) where
        -- get the populated prefixes for this peer:
        prefixesForPeer = Map.foldrWithKey' f [] (adjRibIn rib) where f k v l = if Map.member peer v then k:l else l

        -- fold over the prefixes with withdraw, accumulating the results and updating the map
        (results,newRib) = Data.List.foldl' f ([],rib) prefixesForPeer where
            f :: ([(Prefix, (Peer,Route))],MapRib) -> Int -> ([(Prefix, (Peer,Route))],MapRib)
            f (l,r) i = let pfx = fromInt i -- note this single definition of pfx is needed to allow the following two lines to be unambiguous (and thus to compile)
                                            -- the function signature for 'f', above, though is just for documentation ;-)
                            (r',m) = withdraw pfx peer r
                            l' = maybe l (\x -> (pfx,x):l) m
                        in (l',r') 

    withdraw prefix peer rib = (\(r,(old,Nothing)) -> (r,old)) $ adjust prefix peer Nothing rib
    update prefix peer route rib = adjust prefix peer (Just route) rib

    adjust prefix' peer route rib = (rib',result) where
        prefix = toInt prefix'
        rib' = MapRib { fSel = fSel rib, locRib = newLocRib, adjRibIn = newAdjRibIn }

        oldLocRib = locRib rib
        oldAdjRibIn = adjRibIn rib

        -- updating adjRibIn - 

        --    when the prefix is not present then the input target for the new peer value at this prefix is the empty map
        oldPrefixMap = fromMaybe Map.empty ( Map.lookup prefix oldAdjRibIn )

        -- insert function simply overwrites any existing entry - unless it was also the best route we don't care...
        --     because if it is/was best route it will be in locRib too, so no need to worry here
        newPrefixMap = maybe (Map.delete peer oldPrefixMap)
                             (\x -> Map.insert peer x oldPrefixMap)
                             route

        -- similarly, we can simply replace the old one with the new one here....
        -- note (1) even if the operation was delete, this is still insert (replace, really)
        -- note (2) (corrolary of note (1)) if the prefix is now empty of routes, it will still remain in the AdjRibIn
        -- this is fine, as long as no-one uses adjribin for other purposes - it is NOT the same as locRib!!!
        newAdjRibIn = Map.insert prefix newPrefixMap oldAdjRibIn

        -- now check if the new best route has changed for this prefix
        -- first, calculate the new best route:
        adjRibList = Map.toList newPrefixMap
        newBestRoute | null adjRibList = Nothing
                     | otherwise = Just $ Data.List.maximumBy (fSel rib) adjRibList

        -- there is a subtle point next, regarding comparing the new and old best routes....
        --     if the new best route is to a different peer then it is clearly a change, which must be disseminated
        --     but if the new and old calculated route is from the same peer we would need to know if it is different at the route level
        --     this would require an Eq instance for Route, which we have decalred we don't need...
        --     BUT.... there is an easy way round this - if the new best route is from the peer triggereing this update then 
        --     this is a change, otherwise if the best route is from another peer then it is not - so no need to compare routes, ever....
        -- TLDR summary - check the peer in the calculated new best route - only iff it is the peer given in the update request is it a change of route

        -- in bestRouteChanged if the newBestRoute is empty then the route must have changed due to a withdraw
        -- unless the rib was already empty and this was an erroneous withdraw
        -- however this is still fine since the generated result will simply be (Nothing,Nothing) which is the same as if
        -- the withdraw took out a non selected routed...
        bestRouteChanged = maybe True
                                 ((peer ==) . fst )
                                 -- (\newRoute -> (fst newRoute) == peer)
                                 newBestRoute

        newLocRib = if bestRouteChanged then updatedLocRib else oldLocRib where
            updatedLocRib = maybe (Map.delete prefix oldLocRib) ( \newRoute -> Map.insert prefix newRoute oldLocRib) newBestRoute
        --locRib' = if bestRouteChanged then Map.insert prefix newBestRoute (locRib rib) else (locRib rib) 

        -- we are now done building the update Rib values and need to construct the 'result' value which is a '(Maybe (Peer,Route),(Peer,Route))'
        result = if bestRouteChanged then (oldBestRoute, newBestRoute) else (Nothing,Nothing)
        -- note, we only need to know now what was the previous best route, so 'locRib' actually is never touched unless it needs to change
        -- also note, as an optimisation (***TODO measure the impact?), the locRib can be updated and queried simultaneously with 'insertLookupWithKey'
        --    (note - 'updateLookupWithKey' does not always return the old value, insertLookupWithKey does).

        -- this is the simple version.....
        oldBestRoute = Map.lookup prefix oldLocRib

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
        -- *** !!!! (_oldBestRoute,_newLocRib) = insertLookup prefix newBestRoute oldLocRib

        -- but note may need adjustment to allow for the delete(withdraw) action
