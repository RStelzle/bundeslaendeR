#' ID Links between bundeslaendeR data and state parties' policy positions
#'
#' Dataset providing a link between ltw_elections (or ltw_combined) and state parties' policy positions according to data from Bräuninger, Debus, Müller and Stecker's \emph{Parteienwettbewerb in den deutschen Bundesländern} (PWIB). The position data is available to download at \url{http://polidoc.net}. The measures of state parties' policy positions are based on automatic content analysis (WordScores. Reference texts: Manifestos of federal parties. Reference scores: Positions of federal parties based on expert surveys.). For more details see Debus et al. pp. 59ff.
#' 
#' Bräuninger, Thomas, Marc Debus, Jochen Müller, and Christian Stecker. 2020. Parteienwettbewerb in den deutschen Bundesländern. Wiesbaden: Springer Fachmedien Wiesbaden.
#' 
#' 
#'@docType data
#'
#' @usage data(link_positions_pwib)
#'
#' 
#' @format A tibble containing one row per party for which a position estimate is available.
#' \describe{
#'   \item{state}{chr ISO 3166-2:DE-code of the state}
#'   \item{election_date}{date Election date}
#'   \item{partyname_short}{chr Harmonized abbreviation of the party's name.}
#'   \item{party_pwib}{chr Party abbreviation according to PWIB data.}
#' }
#' @examples
#' \dontrun{
#' 
#' ltw_elections %>% 
#'   left_join(
#'     link_positions_pwib,
#'     by = c("state", "election_date", "partyname_short")
#'   )
#'  
#' }
"link_positions_pwib"