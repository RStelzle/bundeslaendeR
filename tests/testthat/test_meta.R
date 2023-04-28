library(magrittr, quietly = TRUE)
library(tidyr, quietly = TRUE)
library(dplyr, quietly = TRUE)







test_that("Electoral ENP  always lower than number of parties", {
  
  expect_equal(ltw_elections_meta %>% 
                 select(state, election_date, number_parties, fragmentation_enep) %>% 
                 filter(number_parties < fragmentation_enep) %>% 
                 nrow(), 0)
})



test_that("Parliamentary ENP  always lower than number of parties in parliament", {
  
  expect_equal(ltw_elections_meta %>% 
                 select(state, election_date, number_parties_parliament, fragmentation_enpp) %>% 
                 filter(number_parties_parliament < fragmentation_enpp) %>% 
                 nrow(), 0)
})


