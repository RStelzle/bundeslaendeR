#' ID Links between bundeslaendeR data and coalition agreements from polidoc.net - The Political Documents Archive
#'
#' Dataset providing a link between bundeslaendeR data and coalition agreements from polidoc.net- The Political Documents Archive. Note that polidoc.net provides a coalition agreement between the SPD and the Greens following the 2008 HE election (41001.006.2008.1.1). Since this potential coalition under leadership of SPD politician Andrea Ypsilanti never came to be due to several SPD MP's opposing the red-green minority cabinet being externally supported by Die Linke the coalition agreement can't be matched with a government in ltw_combined and ltw_governments and is thus not included.
#'@docType data
#'
#' @usage data(link_coalitionagreements)
#'
#' 
#' @format A tibble containing one row per government for which a coalition agreement is available.
#' \describe{
#'   \item{state}{chr ISO 3166-2:DE-code of the state; including BA for the former state of Baden, WH for the former state of Württemberg-Hohenzollern and WB for the former state of Württemberg-Baden}
#'   \item{election_date}{date Election date}
#'   \item{gov_id}{dbl Unique ID of government. Taken from Linhart et al. However, this ID is not counting up within state by time. In cases where Governments were missing from Linhart et al. before the timeframe covered by Linhart et al. (eg. in Berlin) these earlier governments have an higher ID than later cabinets contained in Linhart et al. data.}
#'   \item{polidoc_filename}{chr Filename of the coalition agreement (excl. file type suffix) when downloaded from polidoc.net}
#' }
#' @examples
#' \dontrun{
#' merge(ltw_combined, link_coalitionagreements)
#' 
#'ltw_combined %>% 
#'  left_join(link_coalitionagreements,
#'    by = c("state", "election_date", "gov_id")
#'     )
#'}
"link_coalitionagreements"