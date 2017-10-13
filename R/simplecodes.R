#' Simple color coding scheme generator
#'
#' Creates a simple color coding scheme used to mark and identify individual animals. Even if one marking is removed, the entire code can be reconstructed.
#'
#' @param total_length the number of unique positions to be marked on the animal. (This can be thought of as the total number of positions on which color bands or paint marks will be applied.)
#' @param alphabet an integer representing the 'alphabet size.' This is the number of unique markings (think different paint colors, symbols, or varieties of bands) at your disposal.
#' @param available.colors an optional list of strings that contains the names of the unique markings which compose the given 'alphabet' (e.g. "blue", "red", "yellow", etc.). If left blank, the mapping can be done at any later time using \code{\link{codes_to_colors}}. Additionally, the length of this list must match the 'alphabet size' given above.
#' @return a dataframe with all the possible unique identities included
#'
#' @author Karl W Broman, \email{kbroman@@biostat.wisc.edu}
#' @references \url{http://en.wikipedia.org/wiki/List_of_Crayola_crayon_colors}
#' @seealso \code{\link{brocolors}}
#' @keywords hplot
#'
#' @examples
#' plot_crayons()
#'
#' @export
#'


simple_code <- function(total_length, alphabet, available.colors = NULL){

  if (missing(alphabet)) {
    stop("Error: you need to enter an 'alphabet size,' e.g. the number of paint colors you have")
  }
  if (missing(total_length)) {
    stop("Error: you need to enter the total length of the ID, e.g. how many color bands or paint drops on each organism")
  }

  perms <- rep(list(seq_len(alphabet)),total_length - 1 )
  df <- as.matrix(expand.grid(perms)) - 1
  df <- cbind(df,apply(df, 1, function(x) alphabet - (sum(x) %% alphabet)))
  df[df == alphabet] <- 0
  df <- split(df, 1:nrow(df))
  names(df) <- NULL
  df <- codes_to_colors(df, available.colors)
  return(df)
}
