#' Meta information on elections in the German states (Bundeslaender) since 1946.
#'
#' Dataset containing meta information on elections in the German states (Bundeslaender) since 1946. For a discussion of the various measures quantifying party system properties see Niedermayer (2013). For descriptions of the various measures of electoral disproportionality see Karpov (2008).
#'@docType data
#'
#' @usage data(ltw_elections_meta)
#'
#' 
#' @format A tibble containing one row per contesting party per election.
#' \describe{
#'   \item{state}{chr ISO 3166-2:DE-code of the state; including BA for the former state of Baden, WH for the former state of Württemberg-Hohenzollern and WB for the former state of Württemberg-Baden}
#'   \item{nuts1}{chr NUTS1 code of state. NA for former states Baden, Württemberg-Baden, Württemberg-Hohenzollern.}
#'   \item{state_name_de}{chr German name of the state}
#'   \item{state_name_en}{chr English name of the state}
#'   \item{election_date}{date Election date}
#'   \item{electorate}{dbl Number of eligible voters. For more totals also see the last three columns.}
#'   \item{turnout}{dbl Turnout. Share of eligible voters turning out.}
#'   \item{total_seats_parliament}{dbl Total number of members of the newly elected Landtag.}
#'   \item{total_female_mps_parliament}{dbl Number of newly elected female MPs.}
#'   \item{number_parties}{dbl Number of parties contesting the election.}
#'   \item{number_parties_parliament}{dbl Number of parties gaining seats in the state parliament.}
#'   \item{fragmentation_enep}{dbl Effective number of parties in the electorate.}
#'   \item{fragmentation_enpp}{dbl Effective number of parties parliament.}
#'   \item{fragmentation_rae}{dbl Rae's index of fragmentation.}
#'   \item{volatility_pedersen}{dbl Pederson Index of electoral volatility. These figures probably slightly overestimate the real extent of electoral volatility, as party splits/mergers are not considered: If parties A (7% at t-1) and B (4% at t-1) contest election t-1 separately but merge before contesting election t and gaining 15% under the label of party A, they really only contribute |(7% + 4%) - 15%| = 4% to the calculation of the Pedersen Index. Here, they would contribute |7% - 15%| + |4% - 0%| = 12% to the calculation as the merger is not properly accounted for.}
#'   \item{disprop_max_deviation}{dbl Maximum deviation index of electoral disproportionality.}
#'   \item{disprop_rae}{dbl Rae's index of electoral disproportionality.}
#'   \item{disprop_loosmore_hanby}{dbl Loosmore-Hanby index of electoral disproportionality.}
#'   \item{disprop_grofman}{dbl Grofman index of electoral disproportionality.}
#'   \item{disprop_lijphart}{dbl Lijphart index of electoral disproportionality.}
#'   \item{disprop_gallagher}{dbl Gallagher index of electoral disproportionality.}
#'   \item{disprop_monroe}{dbl Monroe index of electoral disproportionality.}
#'   \item{disprop_gatev}{dbl Gatev index of electoral disproportionality.}
#'   \item{disprop_ryabtsev}{dbl Ryabtsev index of electoral disproportionality.}
#'   \item{disprop_szalai}{dbl Szalai index of electoral disproportionality.}
#'   \item{disprop_szalai_weighted}{dbl Weighted Szalai index of electoral disproportionality.}
#'   \item{disprop_aleskerov_platonov}{dbl Aleskerov-Platonov index of electoral disproportionality.}
#'   \item{disprop_dhondt}{dbl D'Hondt index of electoral disproportionality.}
#'   \item{disprop_sainte_lague}{dbl Sainte-Lague index of electoral disproportionality.}

#' }
#' @source Election results data provided by the Bundeswahlleiter. A machine-readable version of the data in the pdf available here (\url{https://www.bundeswahlleiter.de/service/landtagswahlen.html}) was kindly provided to me. Election data outside the timeframe covered by Bundeswahlleiter's data provided to me was collected from the states' local election authorities' (Landeswahlleiter) websites. Further election data was collected from the respective Landeswahlleiters. More information on parties and the continuity of parties under different labels was collected by me.
#' 
#' @references 
#' Niedermayer, Oskar. 2013. "Die Analyse von Parteiensystemen." In Handbuch Parteienforschung, ed. Oskar Niedermayer. Wiesbaden: Springer Fachmedien Wiesbaden, 83–117.
#' 
#' Karpov, Alexander. 2008. "Measurement of Disproportionality in Proportional Representation Systems." Mathematical and Computer Modelling 48(9–10): 1421–38.
"ltw_elections_meta"