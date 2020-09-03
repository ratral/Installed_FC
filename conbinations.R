
  library(tidyverse)
  library(epanetReader)
  
  status <- tribble(
    ~ID, ~Typ, ~Status,
    "PUMP_01",   "pump",  "Closed",
    "PUMP_02",   "pump",  "Closed",
    "PUMP_03",   "pump",  "Closed",
    "RIKO_01",   "FCV",   "Closed",
    "RIKO_02",   "FCV",   "Closed",
    "BF_FN_01",  "BF_FN", "Closed",
    "BF_FN_02",  "BF_FN", "Closed",
    "BF_FN_03",  "BF_FN", "Closed",
    "BF_FN_04",  "BF_FN", "Closed",
    "BF_FN_05",  "BF_FN", "Closed",
    "BF_FN_06",  "BF_FN", "Closed",
    "BF_FN_07",  "BF_FN", "Closed",
    "BF_FN_08",  "BF_FN", "Closed",
    "BF_FV_01",  "BF_FV", "Closed",
    "BF_FV_02",  "BF_FV", "Closed",
    "BF_FV_03",  "BF_FV", "Closed",
    "BF_FV_04",  "BF_FV", "Closed",
    "BF_FV_05",  "BF_FV", "Closed",
    "BF_FV_06",  "BF_FV", "Closed",
    "BF_FV_07",  "BF_FV", "Closed",
    "BF_FV_08",  "BF_FV", "Closed",
    "BF_FV_09",  "BF_FV", "Closed",
    "BF_FV_10",  "BF_FV", "Closed",
    "BF_FV_11",  "BF_FV", "Closed",
    "BF_FV_12",  "BF_FV", "Closed",
    "BF_FV_13",  "BF_FV", "Closed",
    "BF_FV_14",  "BF_FV", "Closed",
    "BF_FV_15",  "BF_FV", "Closed",
    "BF_FV_16",  "BF_FV", "Closed",
    "BF_CONECT", "BF",    "Closed" )
  
  
  # escenario <-  status %>% 
  #   select(ID, Status) %>% 
  #   pivot_wider(names_from = ID, values_from = Status) %>% 
  #   mutate( escenario = 0,
  #           TAF_IL =5) %>% 
  #   relocate(escenario, .before = "PUMP_01") %>% 
  #   relocate(TAF_IL, .after = escenario) 

    
  old_filter <- status %>% 
    filter(Typ == "BF_FV") %>% 
    pull( ID)
  
  new_filter  <- status %>% 
    filter(Typ == "BF_FN") %>% 
    pull( ID)
  
  control_valves <- status %>% 
    filter(Typ == "FCV") %>% 
    pull( ID)
  
  pumps <- status %>% 
    filter(Typ == "pump") %>% 
    pull( ID)
  
  switch_valve <- status %>% 
    filter(Typ == "BF") %>% 
    pull( ID)
  
  
  # Direct supply (with RIKO_01 and RIKO_02 working) to each of the areas, 
  # Two filters at the same time one old one new.
  
  comb_01 <- tidyr::expand_grid( filter_01 = old_filter, 
                                 filter_02 = new_filter ) %>% 
    mutate( fcv_01  = control_valves[1],
            fcv_02  = control_valves[2], 
            switch  = NA,
            PUMP_01 = pumps[1],
            PUMP_02 = pumps[2],
            PUMP_03 = pumps[3] )
  
  # Direct supply only the old area, 
  # Two filters at the same time one in the old area.
  # with RIKO_01, RIKO_02 working and BF-CONECT open.
  
  comb_02 <- combn( old_filter, m = 2) %>%
    t() %>% as_tibble() %>%
    rename( filter_01 = V1, 
            filter_02 = V2) %>% 
    mutate( fcv_01 = control_valves[1],
            fcv_02 = control_valves[2], 
            switch = switch_valve,
            PUMP_01 = pumps[1],
            PUMP_02 = pumps[2],
            PUMP_03 = pumps[3] )

  # Direct supply only the new area, 
  # Two filters at the same time one in the old area.
  # with RIKO_01, RIKO_02 working and BF-CONECT open.
  
  comb_03 <- combn( new_filter, m = 2) %>%
    t() %>% as_tibble() %>%
    rename( filter_01 = V1, 
            filter_02 = V2) %>% 
    mutate( fcv_01 = control_valves[1],
            fcv_02 = control_valves[2],
            switch = switch_valve,
            PUMP_01 = pumps[1],
            PUMP_02 = pumps[2],
            PUMP_03 = pumps[3] )
  
  scenarios_01 <- bind_rows( comb_01, comb_02, comb_03) %>% 
    dplyr::select( PUMP_01, PUMP_02, PUMP_03,
                   fcv_01, fcv_02, switch, 
                   filter_01, filter_02 ) 
  
  scenarios_02 <- bind_rows( comb_01, comb_02, comb_03) %>% 
    dplyr::select( PUMP_01, PUMP_02, PUMP_03,
                   fcv_01, fcv_02, switch, 
                   filter_01, filter_02 ) %>% 
    mutate( PUMP_01 = NA, 
            PUMP_02 = NA, 
            PUMP_03 = NA)
  
  scenarios <- bind_rows(scenarios_01, scenarios_02)
  
  
  rm( comb_01, comb_02, comb_03 )
  rm( old_filter, new_filter, control_valves, pumps, switch_valve)
  rm( scenarios_01, scenarios_02 )
  
  #--------------------------------------------------  
  
  scenarios$active_status <- NA
  
  for(i in 1:length(scenarios$active_status)) {
    scenarios$active_status[i] <- scenarios %>% 
      slice(i) %>% 
      unlist(., use.names = FALSE) %>% 
      as.vector() %>% 
      discard(is.na) %>% 
      lst()
  }
  
  rm(i)
  
  scenarios <- scenarios %>% 
      mutate( taf_il =5 )
  
  scenarios <- cbind("scenarios_id" = sprintf("%04d", 1:nrow(scenarios)), scenarios)
  
  scenarios <- scenarios %>% 
    select("scenarios_id", "taf_il", "active_status")
  
  #---- END --------------------------------------------  
  