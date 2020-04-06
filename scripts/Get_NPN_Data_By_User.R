# Getting all observations for specific people that are associated with The Morton Arboretum, but aren't observing on site
# 1. Get People in our Network 
#    -- Currently downloaded from Nature's Notebook:
#       1. My Observation Deck --> edit users --> 
#          downloads: group Roster
# 2. Get at home sites for our people: 
#    getAllStations (GetStations Port) -- station = site
#    key inputs: 
#      - state_code -- can filter to IL
#      - person_id -- get our users
#    key outputs:
#      - station_id; latitude; longitude
#
# 3. getObservations (use npn_get_obs.R)
#    key inputs:
#      - station_id
#    key outputs
#      - (whatever has been output in the past)