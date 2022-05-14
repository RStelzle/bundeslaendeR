#' Governments in the German states (Bundeslaender) since 1946.
#'
#' Dataset containing information on German state governments.
#'@docType data
#'
#' @usage data(ltw_governments)
#'
#' 
#' @format A tibble containing one row per state government.
#' \describe{
#'   \item{state}{chr ISO 3166-2:DE-code of the state; including BA for the former state of Baden, WH for the former state of Württemberg-Hohenzollern and WB for the former state of Württemberg-Baden}
#'   \item{nuts1}{chr NUTS1 code of state. NA for former states Baden, Württemberg-Baden, Württemberg-Hohenzollern.}
#'   \item{state_name_de}{chr German name of the state}
#'   \item{state_name_en}{chr English name of the state}
#'   \item{total_seats_parliament}{dbl Total number of members of the newly elected Landtag.} 
#'   \item{gov_no_within_legterm}{int Order of cabinets within the legislative session of the state.}
#'   \item{gov_id}{dbl Unique ID of government. Taken from Linhart et al. However, this ID is not counting up within state by time. In cases where Governments were missing from Linhart et al. before the timeframe covered by Linhart et al. (eg. in Berlin) these earlier governments have an higher ID than later cabinets contained in Linhart et al. data.}
#'   \item{state_gov_number}{int Order of cabinets within the state.}
#'   \item{gov_start_date}{int Start date of the government.}
#'   \item{gov_source}{chr Source of the information on the government. Either Linhart et al. or the URL of the German Wikipedia Page containing information on the cabinet.}
#'   \item{gov_remarks_stelzle}{chr My remarks on governments.}
#'   \item{minister_president}{chr Name of minister president}
#'   \item{mp_party}{chr Party of the minister president. partyname_short format used. Note: There is a single cabinet with an independent minister president: Heinrich Welsch's caretaker government in the Saarland (at the time not yet a member of the FRG) in 1955.}
#'   \item{gov_parties}{chr String containing the names (partyname_short format) of all government parties separated by " ~ ". The MP's party first, followed by other government parties in order of their seatshare.}
#'   \item{gov_vshare}{dbl Collective vote share of government parties.}
#'   \item{gov_seat_count}{dbl Collective seat count of government parties.}
#'   \item{gov_sshare}{dbl Collective seat share of government parties.}
#'   \item{gov_tog}{chr Type of Government.}

#' }
#' @source Election results data provided by the Bundeswahlleiter. A machine-readable version of the data in the pdf available here (\url{https://www.bundeswahlleiter.de/service/landtagswahlen.html}) was kindly provided to me. Election data outside the timeframe covered by Bundeswahlleiter's data provided to me was collected from the states' local election authorities' (Landeswahlleiter) websites. Further election data was collected from the respective Landeswahlleiters. More information on parties and the continuity of parties under different labels was collected by me. Information on Governments mainly taken from the replication data of Linhart, Eric, Franz U. Pappi und Ralf Schmitt (2008): Die proportionale Ministerienaufteilung in deutschen Koalitionsregierungen: Akzeptierte Norm oder das Ausnutzen strategischer Vorteile?, Politische Vierteljahresschrift 49(1): 46-67. To be found online here: \url{https://www.tu-chemnitz.de/phil/politik/pspi/forschung/daten.php}. Information outside the timeframe of Linhart et al. was collected by me, mainly from German Wikipedia.
"ltw_governments"