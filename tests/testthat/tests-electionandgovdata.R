##  Wahlergebnisse und Regierungen
library(magrittr, quietly = TRUE)
library(tidyr, quietly = TRUE)
library(dplyr, quietly = TRUE)
library(lubridate)
library(readxl, quietly = TRUE)
library(stringr)



###############################################################
##### Regierungszusammensetzung  Bissl Hacky hier im Test #####
###############################################################


## Note to self: Ich habe die Datei in  meinem Random Datensatz Ordner abgelegt, für falls sie offline geht!
linhartetal_full_meta_raw <- rio::import("https://www.tu-chemnitz.de/phil/politik/pspi/forschung/daten/PVS_Portfolioauft_dataset.xlsx", which = 1)

linhartetal_cabinetpos_raw <- rio::import("https://www.tu-chemnitz.de/phil/politik/pspi/forschung/daten/PVS_Portfolioauft_dataset.xlsx", which = 2)




linhartetal_full_meta <- linhartetal_full_meta_raw %>% 
  as_tibble() %>% 
  select(gov_id = ID, state = Land, state_election_term = Legperiode, election_date = Wahltermin, state_gov_number = Regnr,
         gov_start_date = Beginn, gov_end_date = Ende)

linhartetal_cabinet <- linhartetal_cabinetpos_raw %>%
  as_tibble() %>% 
  select(gov_id = ID, party = Partei, nmin_party = Ministeranzahl)



linhartetal <- linhartetal_cabinet %>% 
  left_join(linhartetal_full_meta %>% mutate(gov_id = as.numeric(gov_id))) %>% 
  mutate(gov_start_date = case_when(
    gov_id == 11110 ~ "01.12.1966",
    TRUE ~ gov_start_date
  )) %>% 
  mutate(gov_end_date = case_when(
    gov_id == 11109 ~ "01.12.1966",
    gov_id == 11110 ~ "08.12.1966",
    TRUE ~ gov_end_date
  ))




linhartetal_ready <- linhartetal %>% 
  mutate(across(c(election_date, gov_start_date, gov_end_date), ~as_date(., format = "%d.%m.%Y"))) %>% 
  select(gov_id, state, state_election_term, election_date, state_gov_number, gov_start_date, gov_end_date, party, nmin_party) %>% 
  mutate(state = case_when(
    state == "baden" ~ "BA",
    state == "bawue" ~ "BW",
    state == "bayern" ~ "BY",
    state == "berlin" ~ "BE",
    state == "brand" ~ "BB",
    state == "bremen" ~ "HB",
    state == "hessen" ~ "HE",
    state == "hh" ~ "HH",
    state == "meckpom" ~ "MV",
    state == "nieders" ~ "NI",
    state == "nrw" ~ "NW",
    state == "rheinpf" ~ "RP",
    state == "saar" ~ "SL",
    state == "sachsen" ~ "SN",
    state == "sachsena" ~ "ST",
    state == "schlewi" ~ "SH",
    state == "thuer" ~ "TH",
    state == "wueba" ~ "WB",
    state == "wueho" ~ "WH"
  )) %>% 
  mutate(party = case_when(
    state == "HE" & state_election_term == 5 & party == "GB/BHE" ~ "GDP",
    state == "HH" & state_election_term == 3 & party == "FDP" ~ "HamburgBlock/VBH",
    party == "CDU/CSU" & state == "BY" ~ "CSU",
    party == "CDU/CSU" & state != "BY" ~ "CDU",
    party == "B'90/Grüne" ~ "Grüne",
    party == "PDS" ~ "Linke",
    party == "KPD" ~ "KPD (1919)",
    party == "Schill-Partei" ~ "PRO (Schill)",
    TRUE ~ party
  )) %>% 
  rename(partyname_short = party) %>% 
  group_by(gov_id, state, state_election_term, election_date, state_gov_number, gov_start_date, gov_end_date, partyname_short) %>% 
  summarise(nmin_party = sum(nmin_party)) %>% 
  ungroup() %>% 
  select(-election_date) %>%   # Damit ist iwie nicht das Datum der LTW gemeint
  mutate(state_election_term = case_when(
    state == "HB" & gov_start_date >= as.Date("1967-11-28") ~ state_election_term + 1,
    TRUE ~ state_election_term # Da ist bei Linhart et al ein Fehler und es wird nicht eine Legislatur hochgezählt.
  )) %>% 
  mutate(source = "LinhartEtAl") %>% 
  filter(!(month(gov_end_date) == 12 & day(gov_end_date) == 31)) %>% 
  mutate(state_election_term = case_when(
    state == "HE" & gov_id == 10710 ~ 8, ## Fehler bei linhart et al
    TRUE ~ state_election_term
  ))


my_gov_data <- 
  system.file("extdata", "additional_govdata.xlsx", package = "bundeslaendeR") %>%  
  read_excel() %>% 
  mutate(gov_end_date = str_replace(gov_end_date, "2999-31-12", "2999-12-31")) %>% ## Ich bin ein Idiot :O
  mutate(across(c(gov_start_date, gov_end_date), as.Date))

gov_data <- 
  bind_rows(
    linhartetal_ready,
    my_gov_data 
  ) %>% 
  group_by(state) %>% 
  mutate(state_election_term = case_when(
    min(state_election_term) == 1 ~ state_election_term,
    min(state_election_term) < 1 ~ state_election_term + abs(min(state_election_term)) + 1
  )) %>% 
  mutate(state_gov_number = case_when(
    min(state_gov_number) == 1 ~ state_gov_number,
    min(state_gov_number) < 1 ~ state_gov_number + abs(min(state_gov_number)) + 1
  )) %>% ungroup() %>%
  mutate(partyname_short = case_when(
    state == "HH" &  state_election_term == 2 & state_gov_number == 3 & partyname_short == "FDP" ~ "HamburgBlock/VBH",
    state == "HH" &  state_election_term == 3 & state_gov_number == 5 & partyname_short == "CDU" ~ "HamburgBlock/VBH",
    state == "HH" &  state_election_term == 3 & state_gov_number == 5 & partyname_short == "DP" ~ "HamburgBlock/VBH",
    TRUE ~ partyname_short
  )) %>% 
  group_by(gov_id, state, state_election_term, state_gov_number, gov_start_date,
           gov_end_date, partyname_short, source) %>% 
  summarise(nmin_party = sum(nmin_party)) %>% 
  ungroup()




election_term_gocount <-
  gov_data %>% 
  select(state, state_election_term, state_gov_number) %>% 
  distinct() %>% 
  count(state, state_election_term) 








##################################################
##### Hier beginnen die echten Tests erst... #####
##################################################







## Check Idee: Habe ich für jeden Landtag mind. eine Regierung?
test_that("Für jeden Landtag mind. eine Regieung", {
  expect_equal(
    full_join(
      ltw_election_results %>% 
        select(state, state_election_term) %>% 
        distinct()
      # %>% bind_rows(tibble(state = "NI", state_election_term = 19)) # Testweise
      ,
      ltw_election_results_and_gov %>% 
        select(state, state_election_term, gov_id) %>% 
        distinct()
    ) %>% filter(is.na(gov_id)) %>% nrow(), 0
  )
})



## Check Idee: Hab ich überall mind. eine Partei als gov_party == TRUE (außer SL das indep cabinet elec term == 2. state_gov_number == 5)

test_that("In jeder Regierung (außer indep. cab. im SL) mind. eine Regierungspartei", {
  expect_equal(
    ltw_election_results_and_gov %>% 
      select(state, state_election_term, gov_id, gov_party) %>% 
      group_by(state, state_election_term, gov_id) %>% 
      summarise(n_gov_parties = sum(gov_party)) %>% 
      ungroup() %>% 
      filter(n_gov_parties == 0) %>% 
      filter(gov_id != 11318) %>% nrow(), 0# Das inde. Cabinet im Saarland
  )
  
})



## gleiche anzahl distincte gov ids zwischen nur gov ids und mit gov totals

test_that("gleiche anzahl distincte gov ids zwischen nur gov ids und mit gov totals", {
  expect_equal(
    right_join(
      gov_data %>% select(gov_id) %>% distinct(),
      
      
      ltw_election_results_and_gov %>% 
        select(state, state_election_term, gov_no_within_legterm, gov_id, state_gov_number,
               gov_start_date, gov_end_date, gov_source) %>% distinct()
      
    ) %>% count(gov_id) %>% filter(n != 1) %>% nrow(), 0
  )
})








## Check Idee: Identische Wahl und Gov Totals pro GovID



test_that("Identische Wahl und Gov Totals pro GovID", {
  expect_equal(
    ltw_election_results_and_gov %>% 
      select(state:total_female_mps_parliament,
             gueltige_stimmzettel_hh_hb:ungueltige_stimmzettel_hh_hb,gov_no_within_legterm, gov_id, state_gov_number,
             gov_start_date, gov_end_date, gov_source) %>% 
      distinct() %>% count(gov_id)%>% filter(n != 1) %>% nrow(), 0
  )
})


## Check Idee: Ist das gov_start_date innerhalb vom election_date der Wahl und dem election_date der nächsten Wahl?


test_that("Gov Start Date innerhalb Wahltag und nächstem Wahltag", {
  expect_equal(
    right_join(
      ltw_election_results_and_gov %>% 
        select(state, state_election_term, election_date) %>% 
        distinct() %>% 
        group_by(state) %>% 
        mutate(next_election_date = lead(election_date)) %>% 
        ungroup() %>% 
        mutate(next_election_date = case_when(
          is.na(next_election_date) & state %in% c("BA", "WB", "WH") ~ ltw_election_results_and_gov %>% 
            filter(state == "BW") %>% 
            pull(election_date) %>% min(),
          is.na(next_election_date) & !(state %in% c("BA", "WB", "WH")) ~ as.Date("3005-01-01"),
          TRUE ~ next_election_date
        )),
      
      ltw_election_results_and_gov %>% 
        select(state, state_election_term, gov_id, gov_start_date, gov_end_date, gov_source) %>% 
        distinct()
    ) %>% 
      select(-gov_end_date) %>% 
      filter(!(election_date <= gov_start_date & gov_start_date <= next_election_date)) %>% 
      nrow(), 0
  )
})





## Check Idee: Jede Kombination Partei::GovID nur einmal

test_that("Jede Kombination Partei::GovID nur einmal", {
  expect_equal(
    ltw_election_results_and_gov %>% 
      count(partyname_short, gov_id) %>% 
      filter(n != 1) %>% nrow(), 0
  )
})





## Check Idee: gleiche Anzahl Parteien je gov innerhalb leg per

test_that("gleiche Anzahl Parteien je gov innerhalb leg per über mehrere gov", {
  expect_equal(
    ltw_election_results_and_gov %>% 
      select(state, state_election_term, gov_id, partyname_short) %>% 
      group_by(state, state_election_term, gov_id) %>% 
      summarise(n_parties = length(unique(partyname_short))) %>% 
      group_by(state, state_election_term) %>% 
      summarise(check = n_parties == mean(n_parties)) %>% 
      ungroup() %>% 
      filter(check != TRUE) %>% nrow(), 0
  )
})







## Check Idee: Parteizusammensetzung paste partyname_short, wenn gov_party == TRUE
## identisch mit Zusammensetzung Linhart et al

test_that("gleiche Anzahl Parteien je gov innerhalb leg per über mehrere gov", {
  expect_equal(
    full_join(
      gov_data %>% 
        select(gov_id, partyname_short) %>% 
        group_by(gov_id) %>% 
        summarise(regzusammensetzung = paste0(sort(partyname_short), collapse = "---")),
      
      
      ltw_election_results_and_gov %>% 
        select(gov_id, partyname_short, gov_party) %>% 
        filter(gov_party == TRUE)%>% 
        group_by(gov_id) %>% 
        summarise(regzusammensetzung = paste0(sort(partyname_short), collapse = "---")),
      by = c("gov_id")
    ) %>% 
      mutate(check = regzusammensetzung.x == regzusammensetzung.y) %>% 
      filter(check != TRUE) %>% nrow(), 0
  )
})








test_that("Reihen LCGov identisch mit anzahl parteien je legper * anzahl govs je legper", {
  expect_equal(
    full_join(
      election_term_gocount,
      ltw_election_results %>% count(state, state_election_term, name = "n2")
    ) %>% mutate(product = n * n2) %>% pull(product) %>% sum(),
    
    ltw_election_results_and_gov %>% nrow()
    
  )
})











