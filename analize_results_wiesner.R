
  library( "tidyverse" )
  library( "here" )
  
  results <- readRDS(here::here("results.rds"))
  
  summary(results)

  results %>% 
    filter(Headloss == min(Headloss))
  
  results %>% 
    filter(Headloss == max(Headloss))
  