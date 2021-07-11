##  Parteinahmen checken
library(magrittr, quietly = TRUE)
library(tidyr, quietly = TRUE)
library(dplyr, quietly = TRUE)
library(readxl, quietly = TRUE)
library(here)



test_that("Did I select unique partyname_shorts?", {
  
  expect_equal(read_xlsx('C:/Users/rober/Git_Projekte/bundeslaendeR.Rcheck/tests/testthat/data-raw/Wahlleiter_Parteinamen.xlsx') %>%  
                 select(-1, -2, -3, -4) %>% 
                 rename(Parteikuerzel_Harmonisiert = 1) %>%
                 filter(!is.na(Parteikuerzel_Harmonisiert)) %>% 
                 group_by(Parteikuerzel_Harmonisiert) %>% 
                 unite("Check", Parteiname_Harmonisiert:`In Parteienhandbuch`, sep = "---") %>% 
                 summarise(N = n(),
                                  NDistinct = length(unique(Check))) %>% 
                 ungroup() %>% 
                 filter(NDistinct != 1) %>% nrow(), 0)
})


test_that("Did I select unique partynames?", {

  expect_equal(read_xlsx(here("data-raw", "Wahlleiter_Parteinamen.xlsx")) %>%
                 select(-1, -2, -3, -4) %>% 
                 rename(Parteikuerzel_Harmonisiert = 1) %>%
                 filter(!is.na(Parteikuerzel_Harmonisiert)) %>% 
                 relocate(Parteiname_Harmonisiert, 1) %>% 
                 group_by(Parteiname_Harmonisiert) %>% 
                 unite("Check", Parteikuerzel_Harmonisiert:`In Parteienhandbuch`, sep = "---") %>% 
                 summarise(N = n(),
                           NDistinct = length(unique(Check))) %>%
                 ungroup() %>% 
                 filter(NDistinct != 1) %>% nrow(), 0)
  })

test_that("Does every name have a abbreviation?", {
  expect_equal(read_xlsx(here("data-raw", "Wahlleiter_Parteinamen.xlsx")) %>%
                 rename(Parteikuerzel_Harmonisiert = 1) %>%
                 filter(!is.na(Parteikuerzel_Harmonisiert) & is.na(Parteikuerzel_Harmonisiert)) %>% nrow(),
               0)
})

test_that("Does every abbreviation have a name?", {
  expect_equal(read_xlsx(here("data-raw", "Wahlleiter_Parteinamen.xlsx")) %>%
                 rename(Parteikuerzel_Harmonisiert = 1) %>%
                 filter(is.na(Parteikuerzel_Harmonisiert) & !is.na(Parteikuerzel_Harmonisiert)) %>% nrow(),
               0)
})



