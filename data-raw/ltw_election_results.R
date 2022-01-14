library(tidyverse)
library(readxl)
library(janitor)
library(lubridate)
library(here)


## NUTS


nutsdf <- 
tibble(
  state = c("BW", "BY", "BE", "BB", "HB", "HH", "HE", "MV", "NI", "NW", "RP", "SL", "SN", "ST", "SH", "TH", "WH", "BA", "WB"),
  nuts1 = c("DE1", "DE2", "DE3", "DE4", "DE5", "DE6", "DE7", "DE8", "DE9", "DEA", "DEB", "DEC", "DED", "DEE", "DEF", "DEG", NA, NA, NA)     
  )





##################################
##### Landtagswahlergebnisse #####
##################################


## Raw data of election results as supplied by Bundeswahlleiter
raw <- read_xlsx(here("inst", "extdata","ltw_erg_ab46_oF.xlsx"), sheet = 37, skip = 5)
## My Party Names and Codes Data
raw_partynames_wahlleiter <- read_xlsx(here("inst", "extdata","Wahlleiter_Parteinamen.xlsx"))



totals <-
  raw %>%
  select(Hilf,Land, Nr., Wahltag, Bemerkungen, Wahlberechtigte,
         Wähler, "Gültige Stimmen", "Sitze insgesamt", "Sitze Frauen",
         gesamtstimmen_by_ausgefallene_stimmen_be_abgegebene_stimmen_hh = 8,
         ungueltige_stimmen_ungueltige_stimmzettel_hh_hb = 9,
         gueltige_stimmzettel_hh_hb = 10) %>%
  clean_names() %>%
  rename(
    election_date = wahltag,
    election_remarks = bemerkungen,
    electorate = wahlberechtigte,
    total_votes = wahler,
    valid_votes = gultige_stimmen,
    total_seats_parliament = sitze_insgesamt,
    total_female_mps = sitze_frauen,
  ) %>% 
  mutate(total_female_mps = ifelse(total_female_mps == ".", NA, as.numeric(total_female_mps))) %>% 
  mutate(land = case_when(
    land == "BW" & nr == 18 ~ "WB",
    land == "BW" & nr == 19 ~ "BA",
    land == "BW" & nr == 20 ~ "WH",
    land == "BW" & nr == 21 ~ "WB",
    TRUE ~ land
  )) %>%
  group_by(land) %>%
  mutate(nr = case_when(
    !(land %in% c("WB", "BA", "WH")) ~ -nr + max(nr) + 1,
    land == "WB" & year(election_date) == 1950 ~ 2,
    land == "WB" & year(election_date) == 1946 ~ 1,
    TRUE ~ 1
  )) %>%
  ungroup() %>%
  mutate(election_id = case_when(
    nr < 10 ~ paste0(land, "0", nr),
    TRUE ~ paste0(land, nr)
  )) %>%
  relocate(election_id, 1) %>%
  mutate(election_date = as_date(election_date)) %>%
  mutate(state_name_de = case_when(
    land == "BB" ~ "Brandenburg",
    land == "BE" ~ "Berlin",
    land == "BW" ~ "Baden-Württemberg",
    land == "BY" ~ "Bayern",
    land == "HB" ~ "Bremen",
    land == "HE" ~ "Hessen",
    land == "HH" ~ "Hamburg",
    land == "MV" ~ "Mecklenburg-Vorpommern",
    land == "NI" ~ "Niedersachsen",
    land == "NW" ~ "Nordrhein-Westfalen",
    land == "RP" ~ "Rheinland-Pfalz",
    land == "SH" ~ "Schleswig-Holstein",
    land == "SL" ~ "Saarland",
    land == "SN" ~ "Sachsen",
    land == "ST" ~ "Sachsen-Anhalt",
    land == "TH" ~ "Thüringen",
    land == "WH" ~ "ehemaliges Land Württemberg-Hohenzollern",
    land == "BA" ~ "ehemaliges Land Baden",
    land == "WB" ~ "ehemaliges Land Württemberg-Baden"
  )) %>%
  mutate(state_name_en = case_when(
    land == "BB" ~ "Brandenburg",
    land == "BE" ~ "Berlin",
    land == "BW" ~ "Baden-Württemberg",
    land == "BY" ~ "Bavaria",
    land == "HB" ~ "Bremen",
    land == "HE" ~ "Hesse",
    land == "HH" ~ "Hamburg",
    land == "MV" ~ "Mecklenburg-Vorpommern",
    land == "NI" ~ "Lower-Saxony",
    land == "NW" ~ "North Rhine-Westphalia",
    land == "RP" ~ "Rhineland-Palatine",
    land == "SH" ~ "Schleswig-Holstein",
    land == "SL" ~ "Saarland",
    land == "SN" ~ "Saxony",
    land == "ST" ~ "Saxony-Anhalt",
    land == "TH" ~ "Thuringia",
    land == "WH" ~ "former state Württemberg-Hohenzollern",
    land == "BA" ~ "former state Baden",
    land == "WB" ~ "former state Württemberg-Baden"
  )) %>%
  rename(state_id = land) %>%
  relocate(state_name_de, state_name_en, .after = 3) %>%
  rename(state_election_no = nr) %>%
  mutate(election_remarks = case_when(
    election_remarks == "0" ~ NA_character_,
    TRUE ~ election_remarks
  )) %>% 
  relocate(hilf, 1) %>% 
  select(-election_id, election_id = hilf) %>% 
  mutate(election_remarks = case_when(
    election_remarks == "–" ~ NA_character_,
    TRUE ~ election_remarks
  ))





party_vote_counts <- 
  raw %>%
  clean_names() %>%
  select(hilf, starts_with("x")) %>%
  mutate(across(.cols = starts_with("x"), .fns = as.character)) %>%
  pivot_longer(cols = starts_with("x")) %>%
  mutate(type = case_when(
    str_detect(name, "name") ~ "party_name",
    str_detect(name, "ergeb") ~ "vote_count",
    str_detect(name, "sitz_g") ~ "total_seat_count",
    str_detect(name, "sitz_f") ~ "female_seat_count"
  )) %>%
  select(-name) %>%
  group_by(hilf) %>%
  mutate(id = paste0(hilf, c(rep(1:11, each = 4), rep(12:31, each = 2)))) %>% ## Für die ersten 10 Parteien sind 4 Werte, danach nur 2
  ungroup() %>%
  pivot_wider(
    id_cols = id,
    values_from = value,
    names_from = type
  ) %>%
  filter(!is.na(party_name)) %>%
  mutate(
    vote_count = as.numeric(vote_count),
    total_seat_count = case_when(
      is.na(total_seat_count) ~ 0,
      TRUE ~ as.numeric(total_seat_count)
    ),
    female_seat_count = case_when(
      female_seat_count == "." ~ "0",
      is.na(female_seat_count) ~ "0",
      TRUE ~ female_seat_count
    ) %>% as.numeric()
  ) %>%
  mutate(id = str_sub(id, start = 1, end = 4)) %>%
  group_by(id) %>%
  mutate(female_party_seats_available = max(female_seat_count) > 0) %>%
  mutate(female_seat_count = case_when(
    female_party_seats_available == FALSE ~ NA_real_,
    female_party_seats_available == TRUE & total_seat_count == 0 ~ NA_real_, # Parteien ohne Sitz IMMER Female Seat Count = NA
    female_party_seats_available == TRUE & total_seat_count > 0 & female_seat_count == 0 ~ 0,
    female_party_seats_available == TRUE & total_seat_count > 0 & female_seat_count > 0 ~ female_seat_count
  )) %>% 
  left_join(
    raw %>% 
      select(id = Hilf, election_date = Wahltag) %>% 
      mutate(election_date = as_date(election_date))
  ) %>% 
  mutate(land = str_sub(id, start = 1, end = 2),
         nr = str_sub(id, start = 3, end = 4) %>% as.numeric()) %>% 
  select(-id) %>% 
  mutate(land = case_when(
    land == "BW" & nr == 18 ~ "WB",
    land == "BW" & nr == 19 ~ "BA",
    land == "BW" & nr == 20 ~ "WH",
    land == "BW" & nr == 21 ~ "WB",
    TRUE ~ land
  )) %>%
  group_by(land) %>%
  mutate(nr = case_when(
    !(land %in% c("WB", "BA", "WH")) ~ -nr + max(nr) + 1,
    land == "WB" & year(election_date) == 1950 ~ 2,
    land == "WB" & year(election_date) == 1946 ~ 1,
    TRUE ~ 1
  )) %>%
  ungroup() %>%
  mutate(election_id = case_when(
    nr < 10 ~ paste0(land, "0", nr),
    TRUE ~ paste0(land, nr)
  )) %>% 
  select(-id, land, -nr) %>% 
  relocate(election_id, 1) %>% 
  relocate(election_date, .after = election_id) %>% 
  rename(partyname_short = party_name)  %>% 
  mutate(partyname_short = case_when( # Falsche Schreibweise der Parteikürzel im Ergebnis ggü. der Parteinamensliste
    land == "NW" & partyname_short == "Bewusstsein" ~ "Bewußtsein",
    land == "HE" & partyname_short == "Die Violetten" ~ "DIE VIOLETTEN",
    partyname_short == "FORUM" ~ "Forum",
    partyname_short == "für KINDER" ~ "Für Kinder",
    partyname_short == "Graue" ~ "GRAUE",
    partyname_short == "Grüne" ~ "GRÜNE",
    partyname_short == "ZENTRUM" ~ "Zentrum",
    partyname_short == "SCHILL" ~ "Schill",
    land %in% c("BE", "SH", "SL", "TH") & partyname_short == "ÖDP" ~ "ödp",
    land %in% c("BW", "BA", "WB", "WH") & partyname_short == "ödp" ~ "ÖDP",
    land == "NI" & partyname_short == "ödp" ~ "ÖDP",
    TRUE ~ partyname_short
  )) %>% 
  select(-land)






partynames_wahlleiter <- 
  raw_partynames_wahlleiter %>% 
  rename(partyname_short = 1,
         partyname = 2,
         states = 3,
         remarks = 4) %>% 
  mutate(states = str_remove_all(states, pattern = " ")) %>% 
  separate(states, sep = ",", into = paste0("state_", 1:16)) %>% 
  pivot_longer(starts_with("state")) %>% 
  filter(!is.na(value)) %>% 
  rename(state_today = value)




checkdaten <- party_vote_counts %>% 
  separate(election_id, into = c("state", "election_term"), sep = 2) %>% 
  mutate(state_today = case_when(
    state %in% c("WB", "WH", "BA") ~ "BW",
    TRUE ~ state
  )) %>%
  left_join(partynames_wahlleiter) %>%
  filter(!(state == "HE" & election_date == as_date("1999-02-07") & partyname == "Verband der freien, unabhängigen und überparteilichen Wählergruppen für das Land Hessen")) %>% ## Die (Verband der...) sind 1978 angetreten (Partycode FWG 2x vergeben)
  filter(!(state == "HE" & election_date == as_date("1978-10-08") & partyname == "Freie Wähler Gemeinschaft - Wählergruppe Hessen")) %>% 
  select(-name, -state_today) %>% 
  mutate(Parteikürzel_Harmonisiert = case_when(partyname_short == "BIG" & state == "HH" & year(election_date) == 1997 ~ paste(Parteikürzel_Harmonisiert, "(HH 1997)"), TRUE ~ Parteikürzel_Harmonisiert)) %>% 
  mutate(Parteiname_Harmonisiert = case_when(partyname_short == "BIG" & state == "HH" & year(election_date) == 1997 ~ paste(Parteiname_Harmonisiert , "(HH 1997)"), TRUE ~ Parteiname_Harmonisiert)) %>% 
  mutate(PartyfactsID = case_when(partyname_short == "BIG" & state == "HH" & year(election_date) == 1997 ~ NA_real_, TRUE ~ PartyfactsID)) %>% 
  mutate(Partyname_CHES = case_when(partyname_short == "BIG" & state == "HH" & year(election_date) == 1997 ~ NA_real_, TRUE ~ Partyname_CHES)) %>% 
  mutate(`MR Code` = case_when(partyname_short == "BIG" & state == "HH" & year(election_date) == 1997 ~ NA_real_, TRUE ~ `MR Code`)) %>% 
  mutate(PartyWikipediaParteienlexikon = case_when(partyname_short == "BIG" & state == "HH" & year(election_date) == 1997 ~ NA_character_, TRUE ~ PartyWikipediaParteienlexikon)) %>% 
  mutate(Remarks_Stelzle = case_when(partyname_short == "BIG" & state == "HH" & year(election_date) == 1997 ~ NA_character_, TRUE ~ Remarks_Stelzle)) %>% 
  mutate(`In Parteienhandbuch` = case_when(partyname_short == "BIG" & state == "HH" & year(election_date) == 1997 ~ NA_character_, TRUE ~ `In Parteienhandbuch`)) %>% 
  mutate(Parteiname_Harmonisiert = case_when(partyname_short == "CSU" & state == "SL" ~ "Christlich-Soziale Union im Saarland", TRUE ~ Parteiname_Harmonisiert)) %>% 
  mutate(Parteiname_Harmonisiert = case_when(partyname_short == "CSU" & state == "MV" ~ "Christlich-Soziale Union in Mecklenburg-Vorpommern", TRUE ~ Parteiname_Harmonisiert)) %>% 
  mutate(Parteikürzel_Harmonisiert = case_when(partyname_short == "CSU" & state == "SL" ~ "CSU-Saar", TRUE ~ Parteikürzel_Harmonisiert)) %>% 
  mutate(Parteikürzel_Harmonisiert = case_when(partyname_short == "CSU" & state == "MV" ~ "CSU-MV", TRUE ~ Parteikürzel_Harmonisiert)) %>% 
  mutate(PartyfactsID = case_when(partyname_short == "CSU" & state == "SL" ~ NA_real_, TRUE ~ PartyfactsID)) %>% 
  mutate(PartyfactsID = case_when(partyname_short == "CSU" & state == "MV" ~ NA_real_, TRUE ~ PartyfactsID)) %>% 
  mutate(Partyname_CHES = case_when(partyname_short == "CSU" & state == "SL" ~ NA_real_, TRUE ~ Partyname_CHES)) %>% 
  mutate(Partyname_CHES = case_when(partyname_short == "CSU" & state == "MV" ~ NA_real_, TRUE ~ Partyname_CHES)) %>% 
  mutate(`MR Code`  = case_when(partyname_short == "CSU" & state == "SL" ~ NA_real_, TRUE ~ `MR Code` )) %>% 
  mutate(`MR Code`  = case_when(partyname_short == "CSU" & state == "MV" ~ NA_real_, TRUE ~ `MR Code` )) %>% 
  mutate(PartyWikipediaParteienlexikon = case_when(partyname_short == "CSU" & state == "SL" ~ "https://de.wikipedia.org/wiki/Christlich-Soziale_Union_in_Bayern#Saarland", TRUE ~ PartyWikipediaParteienlexikon)) %>% 
  mutate(PartyWikipediaParteienlexikon = case_when(partyname_short == "CSU" & state == "MV" ~ "https://de.wikipedia.org/wiki/Christlich-Soziale_Union_in_Bayern#Mecklenburg-Vorpommern", TRUE ~ PartyWikipediaParteienlexikon)) %>% 
  mutate(Remarks_Stelzle = case_when(partyname_short == "CSU" & state == "SL" ~ NA_character_, TRUE ~ Remarks_Stelzle)) %>% 
  mutate(Remarks_Stelzle = case_when(partyname_short == "CSU" & state == "MV" ~ NA_character_, TRUE ~ Remarks_Stelzle)) %>% 
  mutate(`In Parteienhandbuch` = case_when(partyname_short == "CSU" & state == "SL" ~ NA_character_, TRUE ~ `In Parteienhandbuch`)) %>% 
  mutate(`In Parteienhandbuch` = case_when(partyname_short == "CSU" & state == "MV" ~ NA_character_, TRUE ~ `In Parteienhandbuch`))






ltw_election_results_bwl <-
  checkdaten %>% 
  mutate(election_term = as.numeric(election_term)) %>% 
  left_join(totals, by = c("state" = "state_id",
                           "election_term" = "state_election_no",
                           "election_date" ="election_date")) %>% 
  arrange(state, election_term, -vote_count) %>% 
  select(state, state_name_de, state_name_en, state_election_term = election_term, election_date, election_id_bundeswahlleiter = election_id,
         election_remarks_bundeswahlleiter = election_remarks,
         electorate = electorate, total_votes, valid_votes,
         total_seats_parliament, female_party_seats_available, total_female_mps_parliament = total_female_mps,
         Parteikürzel_Harmonisiert, Parteiname_Harmonisiert,
         partyname_short, partyname,
         party_vote_count = vote_count, party_seat_count = total_seat_count, party_female_mps = female_seat_count,
         WZB_Govelec_ID = `MR Code`, CHES_ID= Partyname_CHES,
         Partyfacts_ID = PartyfactsID,
         Decker_Neu =`In Parteienhandbuch`,
         URLInfo = PartyWikipediaParteienlexikon,
         Party_Remarks_Stelzle = Remarks_Stelzle,
         party_remarks_bundeswahlleiter = remarks,
         everything()) %>% 
  mutate(total_votes = case_when(
    state == "HB" & state_election_term == 1 ~ NA_character_, # Da stand was
    TRUE ~ total_votes
  )) %>% 
  mutate(ungueltige_stimmen_ungueltige_stimmzettel_hh_hb = case_when(
    state == "HB" & state_election_term == 1 ~ NA_character_, # Da stand was
    TRUE ~ ungueltige_stimmen_ungueltige_stimmzettel_hh_hb
  )) %>% 
  mutate(election_remarks_bundeswahlleiter = case_when(
    state == "HB" & state_election_term == 1 ~ paste(election_remarks_bundeswahlleiter, "Keine Angaben der Anzahl der Wähler und Ungültigen Stimmen möglich, da jeder Wähler 3 bis 5 Stimmen hatte und nur die Anzahl der gültigen Stimmzettel, nicht aber der Stimmen festgestellt werden konnte."), # Da stand was
    TRUE ~ election_remarks_bundeswahlleiter
  )) %>% 
  mutate(total_votes = as.numeric(total_votes),
         ungueltige_stimmen_ungueltige_stimmzettel_hh_hb = as.numeric(ungueltige_stimmen_ungueltige_stimmzettel_hh_hb)) %>% 
  rename(number_of_voters = total_votes) %>%
  mutate(gesamtstimmen_by = case_when(state == "BY" ~ gesamtstimmen_by_ausgefallene_stimmen_be_abgegebene_stimmen_hh, TRUE ~ NA_real_)) %>% 
  mutate(ausgefallene_stimmen_be = case_when(state == "BE" ~ gesamtstimmen_by_ausgefallene_stimmen_be_abgegebene_stimmen_hh, TRUE ~ NA_real_)) %>% 
  mutate(abgegebene_stimmen_hh = case_when(state == "HH" ~ gesamtstimmen_by_ausgefallene_stimmen_be_abgegebene_stimmen_hh, TRUE ~ NA_real_)) %>% 
  select(-gesamtstimmen_by_ausgefallene_stimmen_be_abgegebene_stimmen_hh) %>% 
  mutate(ungueltige_stimmen_except_hh_hb = case_when(!(state %in% c("HH", "HB")) ~ ungueltige_stimmen_ungueltige_stimmzettel_hh_hb, TRUE ~ NA_real_)) %>% 
  mutate(ungueltige_stimmzettel_hh_hb = case_when(state %in% c("HH", "HB") ~ ungueltige_stimmen_ungueltige_stimmzettel_hh_hb, TRUE ~ NA_real_)) %>% 
  select(-ungueltige_stimmen_ungueltige_stimmzettel_hh_hb) %>% 
  mutate(turnout = number_of_voters / electorate) %>% 
  relocate(turnout, .after = number_of_voters) %>% 
  mutate(party_vshare = party_vote_count / valid_votes) %>% 
  relocate(party_vshare, .after = party_vote_count) %>% 
  mutate(party_sshare = party_seat_count / total_seats_parliament) %>% 
  relocate(party_sshare, .after = party_seat_count) %>% 
  rename(partyname_bundeswahlleiter = partyname,
         partyname_short_bundeswahlleiter = partyname_short) %>% 
  rename(partyname_short = Parteikürzel_Harmonisiert,
         partyname = Parteiname_Harmonisiert) %>% 
  clean_names() %>% 
  mutate(partyname_short = case_when(partyname_short_bundeswahlleiter == "GRÜNE" & state == "BB" & state_election_term == 1 ~ "Grüne (BB 1990)", TRUE ~ partyname_short)) %>% 
  mutate(partyname = case_when(partyname_short_bundeswahlleiter == "GRÜNE" & state == "BB" & state_election_term == 1 ~ "Grüne (BB 1990)", TRUE ~ partyname))%>% 
  mutate(wzb_govelec_id = case_when(partyname_short_bundeswahlleiter == "GRÜNE" & state == "BB" & state_election_term == 1 ~ NA_real_, TRUE ~ wzb_govelec_id)) %>% 
  mutate(ches_id = case_when(partyname_short_bundeswahlleiter == "GRÜNE" & state == "BB" & state_election_term == 1 ~ NA_real_, TRUE ~ ches_id)) %>% 
  mutate(partyfacts_id = case_when(partyname_short_bundeswahlleiter == "GRÜNE" & state == "BB" & state_election_term == 1 ~ NA_real_, TRUE ~ partyfacts_id))
  

ltw_election_results_bwl <- 
  ltw_election_results_bwl %>% 
  left_join(nutsdf) %>% relocate(nuts1, .after = "state")




additional_elecdata_raw <- read_xlsx(here("inst", "extdata","additional_elecdata.xlsx"))
newparties_raw <- read_xlsx(here("inst", "extdata","newparties.xlsx"))

ltw_election_results <- 
ltw_election_results_bwl %>% 
  bind_rows(
    additional_elecdata_raw %>% 
      left_join(
        ltw_election_results_bwl %>% 
          select(state, state_name_de, state_name_en, nuts1) %>% 
          distinct()
        ) %>% 
      mutate(turnout = number_of_voters / electorate) %>% 
      mutate(party_vshare = party_vote_count / valid_votes) %>% 
      mutate(party_sshare = party_seat_count / total_seats_parliament) %>% 
      mutate(female_party_seats_available = FALSE) %>% 
      mutate(election_date = as.Date(election_date)) %>% 
      left_join(
        ltw_election_results_bwl %>% 
          select(partyname_short, partyname, wzb_govelec_id, ches_id, partyfacts_id,
                 decker_neu, url_info, party_remarks_stelzle) %>% 
          distinct() %>% 
          bind_rows(newparties_raw))
  )





usethis::use_data(ltw_election_results, overwrite = TRUE)






#####################################
##### Regierungszusammensetzung #####
#####################################


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


# %>% 
#   mutate(gov_end_date = case_when(
#     gov_id == 11109 ~ as.Date("1966-12-01"),
#     gov_id == 11110 ~ as.Date("1966-12-08"),
#     TRUE ~ gov_end_date
#   )) %>% 
#   mutate(gov_start_date = case_when(
#     gov_id == 11110 ~ as.Date("1966-12-08"),
#     TRUE ~ gov_start_date
#   ))
# 


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
  
  
## Bei meinen: Außer Baden, Württemberg Baden, Württemberg Hohenzollern:
## Linhart et al hören 31.12.2005 auf. Das ist auch als Enddate der je letzten Gov.
## eingetragen. Ich habe die neu nochmal kodiert.

## also erst (bis auf in 3 Ländern oben) die jeweils letzte Regierung aus Linhart
## rausfiltern, dann mit meinen Daten joinen

  
  
# 1. linhartetal letzte filtern (bis auf in 3 Ländern oben)
# 2. join_rows mit meinen
# 3. state_election_term und state_gov_number reskalieren, dass das mit 1 startet

# Irgendwie überlegen, was ich mit dem Parteilosen Kabinett im Saarland mache...

## Auch notable: Ich zähle bei der gov_id einfach hoch, auch wenn ich erst die
## früheren Landesregierungen vor Linhart et als bereich code.
## deshalb ist da die Zählung 1. nicht matchend mit state_gov_number und 
##                            2. nicht notwendigerweise mit der Zeit aufsteigend






my_gov_data <- 
  read_xlsx(here("inst", "extdata", "additional_govdata.xlsx")) %>% 
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





# # 
# %>%
#   mutate(partyname_short =case_when(
#     state == "HH", state_election_term == 2, state_gov_number == 3, partyname_short == "FDP", ~ "HamburgBlock/VBH",
#     TRUE ~ partyname_short
#   ))


election_term_gocount <-
gov_data %>% 
  select(state, state_election_term, state_gov_number) %>% 
  distinct() %>% 
  count(state, state_election_term) 
  


ltw_election_results_and_gov_expm <-
ltw_election_results %>%
  left_join(election_term_gocount) %>%
  rename(ngovs_state_election_term = n) %>% 
  filter(!is.na(ngovs_state_election_term)) %>% #### WICHTIG; DAS SIND DIE NEUEN WAHLEN NICHT IN LINHART ET AL und FRÜHE SAAR
  uncount(weights = ngovs_state_election_term) %>% 
  group_by(across(everything())) %>% 
  mutate(gov_no_within_legterm = row_number()) %>% 
  ungroup() %>% 
  full_join(
    gov_data %>% 
      group_by(state, state_election_term) %>% 
      mutate(gov_no_within_legterm = dense_rank(gov_start_date))
  ) %>% 
  mutate(gov_party = !is.na(nmin_party)) %>%
  relocate(gov_party, .before = nmin_party) %>% 
  group_by(state, state_election_term, gov_no_within_legterm) %>%
  fill(gov_id, state_gov_number, gov_start_date, gov_end_date, source, .direction = "downup") %>% 
  ungroup() %>% 
  mutate(gov_remarks_stelzle = case_when(
    state == "SL" & state_election_term == 2 & state_gov_number == 5 ~ "Fully Independent Cabinet",
    TRUE ~ NA_character_
  )) %>%  filter(partyname_short != "Independent_Cabinet") %>% 
  rename(gov_source = source)



ltw_election_results_and_gov_expm %>% 
  left_join(nutsdf) %>% relocate(nuts1, .after = "state")




pms_raw <- read_xlsx(here("inst", "extdata","primeministers.xlsx"))




ltw_election_results_and_gov <- 
ltw_election_results_and_gov_expm %>% 
  left_join(
    pms_raw %>% 
      select(gov_id, minister_president, mp_party)
    ) %>% 
  mutate(is_mp_party = partyname_short == mp_party)






usethis::use_data(ltw_election_results_and_gov, overwrite = TRUE)

## Notable: nmin_party ist NA, wenn die partei nicht an der Regierung ist.
## Um die Möglichkeit offen zu halten Regierungsparteien mit 0 Ministerposten zu haben.






