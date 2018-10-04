module Route where
import Control.Monad.Extra(concatMapM)

import BGPlib
import BGPData
import Update
import Rib
import AdjRIBOut

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
                                                  setNextHop (nextHop route) $
                                                  setLocalPref (localPref $ peerData route) $
                                                  pathAttributes route
                                                 )
                         egpUpdate = makeUpdate (toPrefixes iprefixes)
                                                []
                                                ( sortPathAttributes $
                                                  setOrigin _BGP_ORIGIN_INCOMPLETE $
                                                  setNextHop (nextHop route) $
                                                  prePendAS ( myAS $ globalData $ peerData route) $
                                                  pathAttributes route
                                                 )
              in return $ if isExternal peer then egpUpdate else igpUpdate
          )
          maybeRoute
