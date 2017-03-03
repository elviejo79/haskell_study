chainFilters :: [(a -> Bool)]->[a]->[a]
chainFilters [] ds = ds
chainFilters fs ds = filter (head fs) $ chainFilters (tail fs) ds
