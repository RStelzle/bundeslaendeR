#'ID Links between bundeslaendeR data and data on state election results at the municipal level from GERDA - German Election Database
#'
#'Dataset providing a link between ltw_elections (or ltw_combined) and data on  state election results at the municipal level from GERDA, the German Election Database (Heddesheimer et al. 2025).
#'
#'
#'Heddesheimer, V., Hilbig, H., Sichart, F., & Wiedmann, A. (2025). GERDA: The German Election Database. Nature: Scientific Data, 12: 618.
#'
#'@docType data
#'
#'@usage data(link_gerda)
#'
#'
#'@format A tibble containing one row per party group in StateParl data.
#' \describe{
#'   \item{state_gerda}{chr State number in GERDA data.}
#'   \item{state}{chr State abbreviation in bundeslaendeR data.}
#'   \item{election_date}{date Election date.}
#'   \item{partyname_gerda}{chr Party name in GERDA data.}
#'   \item{partyname_short}{chr Party name in bundeslaendeR data.}
#' }
#' @examples
#' \dontrun{
#'
#'state_unharm %>% 
#'pivot_longer(
#'  cols = "50plus":442,
#'  names_to = "partyname_gerda",
#'  values_to = "vshare"
#') %>% 
#'  drop_na() %>%  
#'  filter(!str_detect(partyname_gerda, "flag_")) %>%
#'  filter(!str_detect(partyname_gerda, "cdu_csu")) %>% 
#'  select(state, election_date,ags, partyname_gerda, valid_votes, vshare) %>% 
#'  mutate(votes = vshare*valid_votes) %>% 
#'  group_by(state, election_date, partyname_gerda) %>% 
#'  summarise(votes = sum(votes)) %>% 
#'  group_by(state, election_date) %>% 
#'  mutate(aggregated_vshare_gerda = votes / sum(votes)) %>% 
#'  ungroup() %>% 
#'  rename(state_gerda = state) %>% 
#'  left_join(
#'    link_gerda,
#'    by = c("state_gerda", "election_date", "partyname_gerda")
#'  )
#' }
#' 
"link_gerda"








