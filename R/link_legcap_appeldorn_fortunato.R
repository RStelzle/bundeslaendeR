#' Links to state legislature capacity data from Appeldorn and Fortunato
#'
#' Dataset providing a link between bundeslaendeR data and data on Legislative Capacity in Germany's Parliaments (10.7910/DVN/BA8G7H) provided by Appeldorn and Fortunato,
#' consolidating minor differences in the spelling of state names and state abbreviations between the two datasets.
#' 
#' Note that Appeldorn and Fortunato provide yearly time-series data, while bundeslaendeR data are based on elections/governments.
#' 
#' Note that Appeldorn and Fortunato use the state-abbreviation "BE" twice, once for Berlin and once for the federal level.
#' 
#' Fortunato, David; Appeldorn, Niels H., 2021, "Replication Data for: Legislative Capacity in Germany's Parliaments", https://doi.org/10.7910/DVN/BA8G7H, Harvard Dataverse, V2.
#' 
#' Appeldorn, Niels H.; Fortunato, David, 2021, "Legislative Capacity in Germany's Parliaments", Legislative Studies Quarterly, 47: 309-328. https://doi.org/10.1111/lsq.12338.
#' 
#' 
#'@docType data
#'
#' @usage data(link_legcap_appeldorn_fortunato)
#'
#' 
#' @format A tibble containing one row per state in bundeslaendeR data.
#' \describe{
#'   \item{state}{chr ISO 3166-2:DE-code of the state}
#'   \item{state_name_en}{chr English name of state}
#'   \item{state_abb_appeldorn_fortunato}{chr State abbreviation in Appeldorn and Fortunato's data}
#'   \item{state_name_appeldorn_fortunato}{chr State name in Appeldorn and Fortunato's data}
#' }
#' @examples
#' \dontrun{
#' 
#' library(tidyverse)
#' library(bundeslaendeR)
#' library(dataverse)
#'
#' ltw_elections_meta %>% 
#'   mutate(election_year = lubridate::year(election_date)) %>%
#'     left_join(
#'       link_legcap_appeldorn_fortunato,
#'       by = c("state", "state_name_en")
#'     ) %>%
#'     left_join(
#'       get_dataframe_by_name(
#'         filename = "afScores.txt", 
#'         dataset = "10.7910/DVN/BA8G7H",
#'         server = "dataverse.harvard.edu",
#'         .f = read.table) %>% 
#'         left_join(
#'           get_dataframe_by_name(
#'             "replication.txt", 
#'             dataset = "10.7910/DVN/BA8G7H", 
#'             server = "dataverse.harvard.edu",
#'             .f = (\(x) read.delim(x, sep = "|"))
#'           ),
#'           by = c("land", "year")
#'         ) %>% 
#'         select(-election.y) %>% 
#'         rename(election = election.x) %>% 
#'         filter(election == 1),
#'       by = c(
#'         "election_year" = "year",
#'         "state_abb_appeldorn_fortunato" = "abbreviation",
#'         "state_name_appeldorn_fortunato" = "land"
#'       )
#'     )
#'
#'
#'
#' }
"link_legcap_appeldorn_fortunato"