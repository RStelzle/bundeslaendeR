#' ID Links between ltw_election_results and state parties' manifestos from polidoc.net- The Political Documents Archive
#'
#' Dataset providing a link between ltw_election_results and state parties' manifestos from polidoc.net- The Political Documents Archive. Note that polidoc.net provides a manifesto for the Neue Liberale in the HB 2015 election (41441.005.2015.1.1). Since the party withdrew it's candidacy before the election and is thus not included in the election results in ltw_election_results, the manifesto id is not included in link_polidoc_parties.
#'@docType data
#'
#' @usage data(link_polidoc_parties)
#'
#' 
#' @format A tibble containing one row per party for which a manifesto is available.
#' \describe{
#'   \item{state}{chr ISO 3166-2:DE-code of the state; including BA for the former state of Baden, WH for the former state of Württemberg-Hohenzollern and WB for the former state of Württemberg-Baden}
#'   \item{election_date}{date Election date}
#'   \item{partyname_short}{chr Harmonized abbreviation of the party's name.}
#'   \item{polidoc_filename}{chr Filename of the manifesto (excl. file type suffix) when downloaded from polidoc.net}
#'   \item{polidoc_filename_2}{chr For some parties in some elections a second manifesto is available. Filename of the second manifesto (excl. file type suffix) when downloaded from polidoc.net}
#' }
#' @examples
#' \dontrun{
#' merge(ltw_election_results, link_polidoc_parties)
#' 
#' ltw_election_results %>% 
#'  left_join(
#'    link_polidoc_parties,
#'    by = c("state", "election_date", "partyname_short")
#'      )
#' }
"link_polidoc_parties"