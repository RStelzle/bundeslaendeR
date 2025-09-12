#' ID Links between bundeslaendeR data and parliamentary party groups in StatePol database
#'
#' Dataset providing a link between ltw_elections (or ltw_combined) and data on parliamentary party groups in the StatePol databsase (Koch et al. 2024). Use the ppg variable in the StatePol PPG dataset (not the ppg_id variable) to link StatePol and bundeslaendeR data. Party groups that are founded later in the legislative cycle cannot be linked to electoral outcome data. See link_notes for details on such cases. The Bündnis 90/Die Grünen (AL) / UFV party group links to the election results of both Bü90GrüUFV and Grüne in the 1990 Berlin state elections as MdLs running on both lists formed a joint party group immediately following the election.
#' 
#' Koch, Elias, Daniel Kuhlen, Jochen Müller and Christian Stecker. StatePol - A Database on the Members of German State Cabinets and Parliaments. Polit Vierteljahresschr (2024).
#' 
#' 
#'@docType data
#'
#' @usage data(link_statepol_ppg)
#'
#' 
#' @format A tibble containing one row per party group in StatePol data.
#' \describe{
#'   \item{state}{chr ISO 3166-2:DE-code of the state}
#'   \item{state_election_term}{dbl Election term in the state. Counts up from 1. Note that this count does not necessarily has to line up with the official way the respective legislature counts its legislative terms. For example, bundeslaendeR includes data from Berlin Stadtverordnetenversammlung elections in 1946 and 1948 prior to the enaction of the new Berlin state constitution in 1950 that established the Abgeordnetenhaus. The Abgeordnetenhaus starts counting its legislative terms in 1950 instead. Similarly, bundeslaendeR includes the election held in Berlin in 2021 as well as the 2023 repeat election and increments the state_election_term counter by one, whereas the Abgeordnetenhaus' official way of counting considers the legislative term to have started in 2021 even after the 2023 rerun.}
#'   \item{partyname_short}{chr Harmonized abbreviation of the party's name.}
#'   \item{landtag_state_abb}{chr State name abbreviation in StatePol data.}
#'   \item{electoralperiod}{dbl Election term in StatePol data.}
#'   \item{ppg}{chr Name of parliamentary party group in StatePol data.}
#'   \item{link_notes}{chr Notes on link. Primarily concerned with party groups that are founded later on in the electoral cycle and therefore cannot be matched to election data.}
#' }
#' @examples
#' \dontrun{
#' 
#' ltw_elections %>% 
#'   left_join(
#'     link_statepol_ppg,
#'     by = join_by(state, state_election_term, partyname_short)
#'   )
#'  
#' }
"link_statepol_ppg"