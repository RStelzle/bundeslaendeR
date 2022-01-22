##  Polidoc IDs
library(magrittr, quietly = TRUE)
library(tidyr, quietly = TRUE)
library(dplyr, quietly = TRUE)


test_that("Each manifesto ID only once?", {
  expect_equal(
  link_polidoc_parties %>%
    select(polidoc_filename, polidoc_filename_2) %>%
    pivot_longer(c("polidoc_filename", "polidoc_filename_2")) %>%
    select(value) %>%
    filter(!is.na(value)) %>%
    distinct() %>%
    nrow(),
  sum(length(link_polidoc_parties$polidoc_filename),
      length(link_polidoc_parties$polidoc_filename_2[!is.na(link_polidoc_parties$polidoc_filename_2)])))
  })

test_that("Each coalition agreement ID only once?", {

  expect_equal(link_polidoc_governments %>%
                 count(polidoc_filename) %>%
                 filter(n > 1) %>% nrow(), 0)
})




test_that("Number of matched parties equal to cases with at least one manifesto", {

  expect_equal(
    ltw_election_results %>%
      left_join(
        link_polidoc_parties,
        by = c("state", "election_date", "partyname_short")
        ) %>%
      filter(!is.na(polidoc_filename)) %>%
      select(state, election_date, partyname_short) %>%
      distinct() %>%
      nrow(),
    length(link_polidoc_parties$polidoc_filename))
})



test_that("Number of matched governments equal to cases with at least one coalition agreement", {
  expect_equal(
  ltw_election_results_and_gov %>%
    left_join(
      link_polidoc_governments,
      by = c("state", "election_date", "gov_id")
    ) %>%
    filter(!is.na(polidoc_filename)) %>%
    select(gov_id) %>%
    distinct() %>%
    nrow(),
  length(link_polidoc_governments$polidoc_filename[!is.na(link_polidoc_governments$polidoc_filename)])
  )
})




