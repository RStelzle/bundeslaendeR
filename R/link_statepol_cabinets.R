#' ID Links between bundeslaendeR data and cabinet data in StatePol database
#'
#' Dataset providing links between government data in ltw_governments and ltw_combined and cabinet data from StatePol database. In some cases one cabinet in the StatePol database is assigned two gov_ids in bundeslaendeR data as StatePol covers parties leaving a coalition using time variant cabinet membership of party ministers whereas bundeslaendeR creates a new government entry. In these cases there are two values in the gov_id column, separated by a semi-colon.
#' 
#' Koch, Elias, Daniel Kuhlen, Jochen Müller and Christian Stecker. StatePol - A Database on the Members of German State Cabinets and Parliaments. Polit Vierteljahresschr (2024).
#' 
#' 
#'@docType data
#'
#' @usage data(link_statepol_cabinets)
#'
#' 
#' @format A tibble containing one row per cabinet in StatePol data.
#' \describe{
#'   \item{state}{chr ISO 3166-2:DE-code of the state}
#'   \item{gov_id}{chr Unique ID of government. Taken from Linhart et al. However, this ID is not counting up within state by time. In cases where Governments were missing from Linhart et al. before the timeframe covered by Linhart et al. (eg. in Berlin) these earlier governments have an higher ID than later cabinets contained in Linhart et al. data.}
#'   \item{landtag_state_abb}{chr State name abbreviation in StatePol data.}
#'   \item{cabinet}{chr Name of cabinet in StatePol data.}
#' }
#' @examples
#' \dontrun{
#' 
#'   
#'   ltw_governments %>% 
#' left_join(
#'   link_statepol_cabinets %>% 
#'     separate(
#'       col = gov_id,
#'       into = c("gov_id_1", "gov_id_2"),
#'       sep = ";"
#'     ) %>% 
#'     pivot_longer(
#'       cols = c(gov_id_1, gov_id_2),
#'       names_to = NULL,
#'       values_to = "gov_id",
#'       values_drop_na = TRUE
#'     ) %>% 
#'     mutate(gov_id = as.numeric(gov_id))
#'   ) 
#'  
#' }
"link_statepol_cabinets"