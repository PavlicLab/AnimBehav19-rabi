#' Converting numeric ID codes to listed color name codes
#'
#' This is a helper function
#'
#'
#' @param total_length the number of unique positions to be marked on your animal. (This can be thought of as the total number of positions on which color bands or paint marks will be applied.)
#' @param redundancy the number of erasures that can occur without disrupting unique identification. This determines how robust the scheme is.
#' @param alphabet an integer representing the 'alphabet size.' This is the number of unique marks (think different paint colors or varieties of bands) at your disposal. Note: unlike the Reed-Solomon inspired function, \code{\link{reed_solomon}}, this function can take non-prime values.
#' @param num.tries the number of iterations that will be run before choosing the best option. Increasing this number increases the running time.
#'
#' @return a list of unique color codes that fit the provided parameters.
#' @note HEY THIS IS A NOTE
#' @author Andrew Burchill, \email{andrew.burchill@asu.edu}
#' @references \url{http://DOI OF THE PAPER} for the orginal source
#' @seealso \code{\link{birdtag}} for an example extension based on leg-banding birds,
#' @keywords reed_solomon
#'
#' @examples
#' plot_crayons()
#'
#' @export
#' @importFrom stringdist seq_distmatrix



codes_to_colors <- function(codes, available.colors = NULL){
  if (!is.null(available.colors)) {
    original.nums <- unique(unlist(codes))
    if (length(available.colors) != length(original.nums) | length(available.colors) != length(unique(available.colors))) {
      warning(paste0("Error: the supplied list of color names must contain exactly ", length(original.nums), " unique elements. Returning the original numeric codes instead." ))
    } else {
      for (i in 1:length(available.colors)) {
        codes <- rapply(codes, function(x) ifelse(x == original.nums[i],available.colors[i], x), how = "replace")
      }
      mapping <- setNames(available.colors, original.nums)
      mapping <- mapping[order(unlist(mapping), decreasing = TRUE)]
      message("Note: The mapping (see below) that was used to assign color names to numeric values is not saved or assigned to a variable. The exact mapping may change with repeated function calls. Depending on your circumstances, you may want to record this now. \n\n")
      print.table(mapping)
    }
  }
  return(codes)
}
