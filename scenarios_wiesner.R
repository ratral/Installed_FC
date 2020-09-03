
  # Libraries

  library(tidyverse)
  library(lubridate)
  library(wcontrolvalve)
  library(epanetReader)
  library(epanet2toolkit)
  
  
  # basic parameter
  
  params <- list ( model = "wiesner_rta_026", 
                   temperature = 15,
                   altitude = 2800)

  
  # definition of the Scenarios

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
  
  
  # write.csv(scenarios, "scenarios.csv")
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
      mutate( taf_il = 5, 
              flow = 2610,
              scenarios_id = NA)
  
  
  scenarios <- cbind("scenarios_id" = sprintf("%04d", 1:nrow(scenarios)), scenarios)
  
  scenarios <- scenarios %>% 
    select("scenarios_id", "taf_il", "flow", "active_status")
  
  #---- END of the definition of the Scenarios----------------------------------  
  #=============================================================================
  
  # Read network data from EPANET .inp ﬁle. 
  net <- here::here("wiesner", str_c(params$model,".inp")) %>% 
    read.inp()
  
  # Changing the setting value of the viscosity of the water in the H. model
  net$Options$VISCOSITY <- as.character(kinematic_viscosity(params$temperature))

  # Update Net Report with "Nodes All" and "Links All" 
  net$Report <- c("Status No", "Summary No", "Page 0", 
                  "Nodes All", "Links All") 
  
  status_ini <- net$Status
  
  
  #---- LOOP -------------------------------------------------------------------
  # LOOP =======================================================================

  for ( i in c(1:552)) {
    # Changing the total head of the reservoirs 
    net$Tanks <- net$Tanks %>% 
      mutate(InitLevel = case_when( ID == "TAF_01" ~ scenarios$taf_il[i],
                                    ID == "TAF_02" ~ scenarios$taf_il[i]))
    
    # Changing the setting of the FCVs valves
    net$Valves <- net$Valves %>% 
      mutate( Setting  = replace(Setting , str_detect(ID, "RIKO_") , scenarios$flow[i]))
    

    # Select  Scenario (Active elements)
    net$Status <- status_ini %>% 
      dplyr::filter(!ID %in% scenarios$active_status[[i]])
    
    # Writing the temp net
    net %>% 
      write.inp(here::here("wiesner", str_c(params$model,"_temp",".inp"))) 
    
    
    # Run EPANET toolkit to obtain the hydraulic solution
    ENepanet( here::here("wiesner", str_c(params$model,"_temp",".inp")),
              here::here("wiesner", str_c(params$model,"_temp",".rpt")))
    
    # Read Report
    net_report <- read.rpt(here::here("wiesner", str_c(params$model,"_temp",".rpt")))
    
    
    # Read data of the tanks (TAF)
    
    taf <- net_report$nodeResults %>% 
      filter(ID == "TAF_01") %>% 
      select( Timestamp, level = Pressure ) %>% 
      mutate( Timestamp = hms(Timestamp) )
    
    
    # Peak flow without valve
    results_01 <- valve_analyze( net = net, 
                                 report = net_report, 
                                 valve_name = "RIKO_01", 
                                 temperature = params$temperature, 
                                 masl = params$altitude) %>% 
      left_join(taf, by = "Timestamp") %>% 
      mutate( "scenarios_id" = scenarios$scenarios_id[i] )
    
    
    results_02 <- valve_analyze( net = net, 
                                 report = net_report, 
                                 valve_name = "RIKO_02", 
                                 temperature = params$temperature, 
                                 masl = params$altitude) %>%
      left_join(taf, by = "Timestamp") %>% 
      mutate( "scenarios_id" = scenarios$scenarios_id[i] )
    
    if (i == 1) {
      results <- bind_rows( results_01, results_02 )
    } else {
      results <- bind_rows(results, results_01, results_02)
    }
    print(i) 
  }    
  
   saveRDS( scenarios, "scenarios.rds")
   saveRDS( results,   "results.rds")
  # 
   write.csv( results,   "results.csv")
  
  
  
  
  