#' Election Results of Landtagswahlen in the German states (Bundeslaender) since 1946.
#'
#' Dataset containing election results of Landtagswahlen in the German states (Bundeslaender) since 1946.
#'@docType data
#'
#' @usage data(ltw_election_results)
#'
#' 
#' @format A tibble containing one row per contesting party per election.
#' \describe{
#'   \item{state}{chr ISO 3166-2:DE-code of the state; including BA for the former state of Baden, WH for the former state of Württemberg-Hohenzollern and WB for the former state of Württemberg-Baden}
#'   \item{nuts1}{chr NUTS1 code of state. NA for former states Baden, Württemberg-Baden, Württemberg-Hohenzollern.}
#'   \item{state_name_de}{chr German name of the state}
#'   \item{state_name_en}{chr English name of the state}
#'   \item{state_election_term}{dbl Election term in the state}
#'   \item{election_date}{date Election date}
#'   \item{election_id_bundeswahlleiter}{chr Specific election_id as denoted by the Bundeswahlleiter. Note that BA, WH and WH are named as BW and the number counts down}
#'   \item{election_remarks_bundeswahlleiter}{chr Remarks on the election as given by the Bundeswahlleiter}
#'   \item{electorate}{dbl Number of eligible voters. For more totals also see the last three columns.}
#'   \item{number_of_voters}{dbl Number of voters turning out. For more totals also see the last three columns.}
#'   \item{turnout}{dbl Turnout. Share of eligible voters turning out.}
#'   \item{valid_votes}{dbl Number of valid votes. Must not be equal to the number of ballots cast, as sometimes a ballot contains multiple votes! For more totals also see the last three columns.}
#'   \item{total_seats_parliament}{dbl Total number of members of the newly elected Landtag.}
#'   \item{female_party_seats_available }{lgl Denotes whether information on the no. of female members of the Landtag per party is available. Note that for parties not elected to the new Landtag party_female_mps always is.na() == TRUE.}
#'   \item{total_female_mps_parliament}{dbl Number of newly elected female MPs.}
#'   \item{partyname_short}{chr Harmonized abbreviation of the party's name.}
#'   \item{partyname}{chr Harmonized name of the party.}
#'   \item{partyname_short_bundeswahlleiter}{chr Partyname abbreviation as documented by the Bundeswahlleiter.}
#'   \item{partyname_bundeswahlleiter}{chr Partyname as documented by the Bundeswahlleiter.}
#'   \item{party_vote_count}{dbl Number of votes recieved by the party.}
#'   \item{party_vshare}{dbl Share of votes recieved by the party.}
#'   \item{party_seat_count}{dbl Number of seats recieved by the party.}
#'   \item{party_sshare}{dbl Share of seats recieved by the party.}
#'   \item{party_female_mps}{dbl Number of female MPs elected for the party. Note that for parties not elected to the new Landtag party_female_mps always is.na() == TRUE.}
#'   \item{wzb_govelec_id}{dbl If available, MR-Code of the party in the internal govelec database of the WZB department Democracy and Democratization}
#'   \item{ches_id}{dbl If available, ID of the party in the Chapel-Hill Expert Survey.}
#'   \item{partyfacts_id}{dbl If available, ID of the party in the partyfacts database.}
#'   \item{decker_neu}{lgl Denotes, wether the Handbuch der deutschen Parteien (3. ed.) by Decker and Neu has a chapter on the party.}
#'   \item{url_info}{chr URL to informaton on the party on the web. Can contain multiple URLs!}
#'   \item{party_remarks_stelzle}{chr Remarks on the party by me.}
#'   \item{party_remarks_bundeswahlleiter}{chr Remarks on the party as listed by the Bundeswahlleiter.}
#'   \item{gueltige_stimmzettel_hh_hb}{chr Messy totals.}
#'   \item{gesamtstimmen_by}{chr Messy totals.}
#'   \item{ausgefallene_stimmen_be}{chr Messy totals.}
#'   \item{abgegebene_stimmen_hh}{chr Messy totals.}
#'   \item{ungueltige_stimmen_except_hh_hb}{chr Messy totals.}
#'   \item{ungueltige_stimmzettel_hh_hb}{chr Messy totals.}
#' }
#' @source Election results data provided by the Bundeswahlleiter. A machine-readable version of the data in the pdf available here (\url{https://www.bundeswahlleiter.de/service/landtagswahlen.html}) was kindly provided to me. Election data outside the timeframe covered by Bundeswahlleiter's data provided to me was collected from the states' local election authorities' (Landeswahlleiter) websites. More information on parties and the continuity of parties under different labels was collected by me.
"ltw_election_results"