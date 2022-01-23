#' Geographical ordering of states for faceting using geofacet
#'
#' This function returns a dataframe of state IDs, state names and locations on a 4x4 grid to be used for faceting plot panels using geofacet.
#' @param linebreak Should longer names be broken up into two lines?
#' @export
#' 
#' 


de_states_geofacet_grid_4x4 <- function(linebreak = FALSE) {
  
int_grid


if (linebreak == TRUE) {

  int_grid$name <- gsub("-", "-\n", int_grid$name)
  int_grid$name_de <- gsub("-", "-\n", int_grid$name_de)

}

else {
  int_grid
}

return(int_grid)

}


