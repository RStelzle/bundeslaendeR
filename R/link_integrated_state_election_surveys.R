#' ID Links between bundeslaendeR data and vote choice in integrated state election survey data
#'
#' Dataset providing a link between ltw_elections (or ltw_combined) and party names in vote choice in the integrated dataset of state election surveys availabel on GESIS (ZAZA4182). 
#' 
#' Note that not all parties running in an election were necessarily asked about in the election surcey. Vice versa, not all parties that were asked about in the election survey did necessarily actually contested the election.
#' 
#' Note that for some state elections multiple surveys are available in the integrated survey dataset. See column za_nr1 for GESIS ID of original survey. The GESIS ID is the sole variable identifying a specific election in the integrated dataset.
#' 
#' Note that the integrated survey uses different variables for elections until 1970 (m7b) and after 1973 (m7).
#' 
#' 
#' Scheuch, Erwin K., Wildenmann, Rudolf, Baumert, Gerhard, Klingemann, Hans-Dieter, Kaase, Max, Adrian, Wolfgang, Berger, Manfred, INFRATEST, München, & FORSA, Berlin (2015). Landtagswahlen - Integrierter Datensatz 1964-2004. GESIS Datenarchiv, Köln. ZA4182 Datenfile Version 2.1.0, https://doi.org/10.4232/1.12389.

#' 
#' 
#'@docType data
#'
#' @usage data(link_integrated_state_election_surveys)
#'
#' 
#' @format A tibble containing one row per available party to answer to vote choice question in state surveys with. Restricted to parties actually contesting the election even when further parties were asked about.
#' \describe{
#'   \item{bland}{chr State name in integrated survey file}
#'   \item{m7}{chr Party names in vote choice variable in integrated survey file (after 1973)}
#'   \item{m7b}{chr Party names in vote choice variable in integrated survey file (before 1970)}
#'   \item{za_nr1}{chr GESIS ID of original state election survey}
#'   \item{state}{chr ISO 3166-2:DE-code of the state}
#'   \item{election_date}{date Election date}
#'   \item{partyname_short}{chr Harmonized abbreviation of the party's name.}
#' }
#' @examples
#' \dontrun{
#' 
#' ltw_elections %>% 
#'   left_join(
#'     link_integrated_state_election_surveys,
#'     by = join_by(state, election_date, partyname_short)
#'   )
#'  
#' }
"link_integrated_state_election_surveys"