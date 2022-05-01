##  Wahlergebnisse
library(magrittr, quietly = TRUE)
library(tidyr, quietly = TRUE)
library(dplyr, quietly = TRUE)
library(readxl, quietly = TRUE)



test_that("Jeder Parteikuerzel bei jeder Wahl nur 1x", {
expect_equal(  
  ltw_elections %>% 
    group_by(state, state_election_term, partyname_short) %>% 
    summarise(N = n()) %>% 
    ungroup() %>% 
    filter(N != 1) %>% nrow(), 0  )
})


test_that("Jeder Parteiname bei jeder Wahl nur 1x", {
  expect_equal(  
    ltw_elections %>% 
      group_by(state, state_election_term, partyname) %>% 
      summarise(N = n()) %>% 
      ungroup() %>% 
      filter(N != 1) %>% nrow(), 0  )
})



test_that("Identische Totals innerhalb einer Wahl 1", {
  expect_equal(ltw_elections %>% 
                 select(state, state_election_term,
                        state_name_de, state_name_en, election_date, election_id_bundeswahlleiter,
                        election_remarks_bundeswahlleiter, electorate, number_of_voters, turnout,
                        valid_votes, total_seats_parliament, female_party_seats_available, total_female_mps_parliament,
                        gueltige_stimmzettel_hh_hb, gesamtstimmen_by, ausgefallene_stimmen_be,
                        abgegebene_stimmen_hh, ungueltige_stimmen_except_hh_hb, ungueltige_stimmen_except_hh_hb) %>% 
                 group_by(state, state_election_term) %>% 
                 unite("Check", state_name_de:ungueltige_stimmen_except_hh_hb, sep = "---") %>% 
                 summarise(N = n(),
                           NDistinct = length(unique(Check))) %>%
                 ungroup() %>% 
                 filter(NDistinct != 1) %>% nrow(), 0)
})



test_that("Identische Totals innerhalb einer Wahl 2", {
expect_equal(
ltw_elections %>%
  select(state, state_election_term,
         state_name_de, state_name_en, election_date, election_id_bundeswahlleiter,
         election_remarks_bundeswahlleiter, electorate, number_of_voters, turnout,
         valid_votes, total_seats_parliament, female_party_seats_available, total_female_mps_parliament,
         gueltige_stimmzettel_hh_hb, gesamtstimmen_by, ausgefallene_stimmen_be,
         abgegebene_stimmen_hh, ungueltige_stimmen_except_hh_hb, ungueltige_stimmen_except_hh_hb) %>%
  unite("Check", state_name_de:ungueltige_stimmen_except_hh_hb, sep = "---") %>%
  distinct() %>% nrow(),

ltw_elections %>% 
  select(state, state_election_term) %>% 
  distinct() %>% nrow()

)
})


test_that("Summe der Stimmen fuer Parteien entspricht der angegebenen valid votes", {
  expect_equal(ltw_elections %>%
                select(state, state_election_term, valid_votes, party_vote_count) %>% 
                group_by(state, state_election_term) %>% 
                summarise(valid_votes = mean(valid_votes),
                          party_vote_sum = sum(party_vote_count)) %>% 
                ungroup() %>% 
                mutate(check = valid_votes == party_vote_sum) %>% 
                filter(check == FALSE) %>% nrow(), 0)
})





test_that("Electorate groesser als Zahl der WaehlerInnen", {
  expect_equal(
    ltw_elections %>% 
      filter(electorate < number_of_voters) %>% 
      nrow(), 0
  )
})



test_that("Parteistimmen summieren sich zu valid votes auf", {
  expect_equal(
    ltw_elections %>% 
      group_by(state, state_election_term) %>% 
      summarise(valid_votes = mean(valid_votes),
                pvcsum = sum(party_vote_count)) %>% 
      ungroup() %>% 
      filter(valid_votes != pvcsum) %>% nrow(), 0
  )
})

test_that("Parteivshare summieren sich zu 1 auf", {
  expect_equal(
    ltw_elections %>% 
      group_by(state, state_election_term) %>% 
      summarise(pvssum = round(sum(party_vshare)), digits = 100) %>% 
      ungroup() %>% 
      filter(pvssum != 1) %>% nrow(), 0
  )
})






test_that("Parteisitze summieren sich zu total_seats_parliament auf", {
  expect_equal(
    ltw_elections %>% 
      group_by(state, state_election_term) %>% 
      summarise(total_seats_parliament = mean(total_seats_parliament),
                pscsum = sum(party_seat_count)) %>% 
      ungroup() %>% 
      filter(total_seats_parliament != pscsum) %>% nrow(), 0
  )
})





test_that("Parteisshare summieren sich zu 1 auf", {
  expect_equal(
    ltw_elections %>%
      group_by(state, state_election_term) %>%
      summarise(psssum = round(sum(party_sshare))) %>%
      ungroup() %>%
      filter(psssum != 1) %>% nrow(), 0
  )
})




test_that("Sind Party FSeats als available angegeben habe ich sie auch.", {
  expect_equal(
    ltw_elections %>%
      filter(female_party_seats_available == TRUE & party_seat_count > 0 & is.na(party_female_mps)) %>% 
      nrow(), 0
  )
})


## gleich oder weniger female mps als gesamt mps


test_that("Female seats -wo verfügbar- summieren sich zu total female mps parliament auf", {
  expect_equal(
    ltw_elections %>% 
      filter(female_party_seats_available == TRUE & party_seat_count > 0) %>% 
      group_by(state, state_election_term) %>% 
      summarise(total_female_mps_parliament = mean(total_female_mps_parliament),
                party_fseat_sum = sum(party_female_mps)) %>% 
      ungroup() %>% 
      mutate(Check = total_female_mps_parliament == party_fseat_sum) %>% 
      filter(Check == FALSE) %>% nrow(), 0
  )
})



test_that("Gleich oder weniger female MPs als Gesamt MPs", {
  expect_equal(
    ltw_elections %>% 
      filter(!is.na(total_female_mps_parliament)) %>% 
      filter(total_female_mps_parliament > total_seats_parliament) %>% 
      nrow(), 0
  )
})





test_that("hab ich fälle, wo ich party female mps aber nicht total female mps hab?", {
  expect_equal(
    ltw_elections %>% 
      filter(is.na(total_female_mps_parliament)) %>% 
      filter(!is.na(party_female_mps)) %>% 
      nrow(), 0
  )
})


