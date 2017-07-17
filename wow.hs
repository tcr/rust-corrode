module Test ()
where

promoteArg :: Result -> Rust.Expr
promoteArg r = alpha
    where
        reachableFrom = IntMap.unionWith IntSet.union (IntMap.fromSet IntSet.singleton entries) strictReachableFrom
        singlyReached = flipEdges $ IntMap.filter (\ r -> IntSet.size r == 1) $ IntMap.map (IntSet.intersection entries) reachableFrom
        handledEntries = IntMap.map (\ within -> blocks `restrictKeys` within) singlyReached
        unhandledEntries = entries `IntSet.difference` IntMap.keysSet handledEntries
        handledBlocks = IntMap.unions (IntMap.elems handledEntries)
        followBlocks = blocks `IntMap.difference` handledBlocks
        followEntries = unhandledEntries `IntSet.union` outEdges handledBlocks
        makeHandler entry blocks' = relooper (IntSet.singleton entry) blocks'
        allHandlers = IntMap.mapWithKey makeHandler handledEntries
        unhandled = if IntMap.keysSet allHandlers == entries
            then
                let (lastHandler, otherHandlers) = IntMap.deleteFindMax allHandlers
                in (snd lastHandler, otherHandlers)
            else ([], allHandlers)