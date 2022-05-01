##  Polidoc IDs
library(magrittr, quietly = TRUE)
library(tidyr, quietly = TRUE)
library(dplyr, quietly = TRUE)


test_that("Each manifesto ID only once?", {
  expect_equal(
  link_manifestos %>%
    select(polidoc_filename, polidoc_filename_2) %>%
    pivot_longer(c("polidoc_filename", "polidoc_filename_2")) %>%
    select(value) %>%
    filter(!is.na(value)) %>%
    distinct() %>%
    nrow(),
  sum(length(link_manifestos$polidoc_filename),
      length(link_manifestos$polidoc_filename_2[!is.na(link_manifestos$polidoc_filename_2)])))
  })

test_that("Each coalition agreement ID only once?", {

  expect_equal(link_coalitionagreements %>%
                 count(polidoc_filename) %>%
                 filter(n > 1) %>% nrow(), 0)
})




test_that("Number of matched parties equal to cases with at least one manifesto", {

  expect_equal(
    ltw_elections %>%
      left_join(
        link_manifestos,
        by = c("state", "election_date", "partyname_short")
        ) %>%
      filter(!is.na(polidoc_filename)) %>%
      select(state, election_date, partyname_short) %>%
      distinct() %>%
      nrow(),
    length(link_manifestos$polidoc_filename))
})



test_that("Number of matched governments equal to cases with at least one coalition agreement", {
  expect_equal(
  ltw_combined %>%
    left_join(
      link_coalitionagreements,
      by = c("state", "election_date", "gov_id")
    ) %>%
    filter(!is.na(polidoc_filename)) %>%
    select(gov_id) %>%
    distinct() %>%
    nrow(),
  length(link_coalitionagreements$polidoc_filename[!is.na(link_coalitionagreements$polidoc_filename)])
  )
})




