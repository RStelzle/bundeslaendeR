#' ID Links between bundeslaendeR data and state parties' manifestos from polidoc.net and abgeordnetenwatch.de
#'
#' Dataset providing a link between ltw_elections (or ltw_combined) and state parties' manifestos from polidoc.net- The Political Documents Archive and from abgeordnetenwatch.de . For polidoc.net, the filename of the manifesto in .txt format when downloaded is provided. For abgeordnetenwatch.de the URL to the manifesto in .pdf format is provided. Note that polidoc.net provides a manifesto for the Neue Liberale in the HB 2015 election (41441.005.2015.1.1). Since the party withdrew it's candidacy before the election and is thus not included in the election results in ltw_elections, the manifesto id is not included in link_polidoc_parties.
#' 
#'@docType data
#'
#' @usage data(link_manifestos)
#'
#' 
#' @format A tibble containing one row per party for which a manifesto is available.
#' \describe{
#'   \item{state}{chr ISO 3166-2:DE-code of the state; including BA for the former state of Baden, WH for the former state of Württemberg-Hohenzollern and WB for the former state of Württemberg-Baden}
#'   \item{election_date}{date Election date}
#'   \item{partyname_short}{chr Harmonized abbreviation of the party's name.}
#'   \item{polidoc_filename}{chr Filename of the manifesto (excl. file type suffix) when downloaded from polidoc.net}
#'   \item{polidoc_filename_2}{chr For some parties in some elections a second manifesto is available. Filename of the second manifesto (excl. file type suffix) when downloaded from polidoc.net}
#'   \item{agwatch_pdf_url}{chr URL of the manifesto in .pdf form on abgeordnetenwatch.de}
#'   \item{agwatch_election_manifesto}{lgl TRUE if the linked manifesto is an electoral manifesto. FALSE if it appears to be a more general manifesto of the party (Grundsatzprogramm) independent of any specific state election.}
#' }
#' @examples
#' \dontrun{
#' merge(ltw_elections, link_manifestos[!is.na(link_manifestos$polidoc_filename),1:5])
#' 
#' merge(ltw_elections, link_manifestos[!is.na(link_manifestos$agwatch_pdf_url),c(1:3, 6,7)])
#' 
#' ltw_elections %>% 
#'  left_join(
#'    link_manifestos %>% filter(!is.na(polidoc_filename)),
#'    by = c("state", "election_date", "partyname_short")
#'      )
#'  
#'  ltw_elections %>% 
#'   left_join(
#'    link_manifestos %>% filter(!is.na(agwatch_pdf_url)),
#'    by = c("state", "election_date", "partyname_short")
#'      )
#'  
#' }
"link_manifestos"