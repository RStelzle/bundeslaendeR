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
#' 
#'link_stateparl %>%
#'  separate(
#'    col = election_date,
#'    into = c("election_date_1", "election_date_2"),
#'    sep = ";"
#'  ) %>%
#'  pivot_longer(
#'    cols = c(election_date_1, election_date_2),
#'    values_to = "election_date",
#'    names_to = "tmp"
#'  ) %>%
#'  filter(!is.na(election_date)) %>%
#'  select(-tmp) %>%
#'  mutate(election_date = as.Date(election_date)) %>%
#'  left_join(
#'    ltw_elections,
#'    by = c("state_bundeslaender" = "state", "election_date", "partyname_short")
#'  )
#' }
#' 
"link_gerda"








