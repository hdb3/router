module Route where
import Control.Monad.Extra(concatMapM)

import BGPlib
import BGPRib

lookupRoutes :: Rib -> PeerData -> [AdjRIBEntry] -> IO [BGPMessage]
lookupRoutes rib peer ares = do routes <- concatMapM (lookupRoute rib peer) ares
                                return $ map ungetUpdate routes

lookupRoute :: Rib -> PeerData -> AdjRIBEntry -> IO [ ParsedUpdate ]
lookupRoute _ _ (iprefixes, 0 ) = return [ originateWithdraw $ toPrefixes iprefixes ]
lookupRoute _ _ ([], _ ) = do
    putStrLn "empty prefix list in lookupRoute"
    return []

lookupRoute rib peer (iprefixes, _ ) = do
    maybeRoute <- queryRib rib (head iprefixes)
    maybe (do putStrLn "failed lookup in lookupRoute"
              return []
          )
          (\route -> let igpUpdate = makeUpdate (toPrefixes iprefixes)
                                                []
                                                ( sortPathAttributes $
                                                  setOrigin _BGP_ORIGIN_INCOMPLETE $
                                                  -- this is reflector/controller default, bur for a router next-hop-self is default:
                                                  -- setNextHop (nextHop route) $
                                                  setNextHop (localIPv4 $ peerData route) $ -- next hop self!
                                                  setLocalPref (localPref $ peerData route) $
                                                  pathAttributes route
                                                 )
                         egpUpdate = makeUpdate (toPrefixes iprefixes)
                                                []
                                                ( sortPathAttributes $
                                                  setOrigin _BGP_ORIGIN_INCOMPLETE $
                                                  -- setNextHop (nextHop route) $ -- reflector default
                                                  setNextHop (localIPv4 $ peerData route) $ -- next hop self!
                                                  prePendAS ( myAS $ globalData $ peerData route) $
                                                  pathAttributes route
                                                 )
              in return $ if isExternal peer then egpUpdate else igpUpdate
          )
          maybeRoute

lookupNextHop rib iprefix = do
    maybeRoute <- queryRib rib iprefix
    maybe (do putStrLn "failed lookup in lookupRoute"
              return Nothing
          )
          (\route -> return $ Just $ nextHop route
          )
          maybeRoute
