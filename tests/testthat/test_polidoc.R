##  Polidoc IDs
library(magrittr, quietly = TRUE)
library(tidyr, quietly = TRUE)
library(dplyr, quietly = TRUE)

### Polidoc Tests

test_that("Each polidoc manifesto ID only once?", {
  expect_equal(
  link_manifestos %>%
    filter(!is.na(polidoc_filename)) %>% 
    select(polidoc_filename, polidoc_filename_2) %>%
    pivot_longer(c("polidoc_filename", "polidoc_filename_2")) %>%
    select(value) %>%
    filter(!is.na(value)) %>%
    distinct() %>%
    nrow(),
  sum(length(link_manifestos$polidoc_filename[!is.na(link_manifestos$polidoc_filename)]),
      length(link_manifestos$polidoc_filename_2[!is.na(link_manifestos$polidoc_filename_2)])))
  })

test_that("Each coalition agreement ID only once?", {

  expect_equal(link_coalitionagreements %>%
                 count(polidoc_filename) %>%
                 filter(n > 1) %>% nrow(), 0)
})




test_that("Number of polidoc matched parties equal to cases with at least one manifesto", {

  expect_equal(
    ltw_elections %>%
      left_join(
        link_manifestos %>% 
          filter(!is.na(polidoc_filename)),
        by = c("state", "election_date", "partyname_short")
        ) %>%
      filter(!is.na(polidoc_filename)) %>%
      select(state, election_date, partyname_short) %>%
      nrow(),
    length(link_manifestos$polidoc_filename[!is.na(link_manifestos$polidoc_filename)]))
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

### AGWatch

test_that("All unique AGWatch URLS", {
  expect_equal(
  length(link_manifestos$agwatch_pdf_url[!is.na(link_manifestos$agwatch_pdf_url)]),
  length(unique(link_manifestos$agwatch_pdf_url[!is.na(link_manifestos$agwatch_pdf_url)]))
    )
})






test_that("Number of agwatch matched party equal to agwatch urls",{
  expect_equal(
    ltw_elections %>% 
      left_join(
        link_manifestos %>% 
          filter(!is.na(agwatch_pdf_url))
      ) %>% 
      filter(!is.na(agwatch_pdf_url)) %>% 
      nrow(),
  length(link_manifestos$agwatch_pdf_url[!is.na(link_manifestos$agwatch_pdf_url)])
    )
  })









