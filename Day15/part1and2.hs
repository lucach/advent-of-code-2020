import qualified Data.Map as Map

insertAtPos historyMap (value,idx) = Map.insert value idx historyMap

doOneStep (historyMap, lastValue) lastPos = (Map.insert lastValue lastPos historyMap, value) where
    value = case Map.lookup lastValue historyMap of 
        Nothing -> 0
        Just previousPos -> lastPos - previousPos

main = do
    let initial = [0,3,1,6,7,5]
    let historyMap = foldl insertAtPos Map.empty $ zip initial [1..]
    let (_, val) = foldl doOneStep (historyMap, last initial) [(length initial)..30000000 - 1]
    print val