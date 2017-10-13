#' Polynomial color coding scheme generator
#'
#' Creates a color coding scheme used to mark and identify individual animals using Reed-Solomon error-correction codes. The codes are robust to an arbitrary number of color-slot erasures.
#'
#' @param total_length the number of unique positions to be marked on the animal. (This can be thought of as the total number of positions on which color bands or paint marks will be applied.) Note: Reed-Solomon coding requires the total length of the ID to be less than or equal to the value of \code{alphabet}.
#' @param redundancy the number of erasures that can occur without disrupting surety of unique identification. This value determines how robust the scheme is to erasures.
#' @param alphabet an integer representing the 'alphabet size.' This is the number of unique markings (think different paint colors, symbols, or varieties of bands) at your disposal. Note: Reed-Solomon coding requires this value to be a prime number. If a non-prime is entered, the function will automatically adjust it to the nearest previous prime.
#' @param available.colors an optional list of strings that contains the names of the unique markings which compose the given 'alphabet' (e.g. "blue", "red", "yellow", etc.). If left blank, the mapping can be done at any later time using \code{\link{codes_to_colors}}. Additionally, the length of this list must match the 'alphabet size' given above.
#'
#' @return a list containing  with all the possible unique identities included
#'
#' @author Andrew Burchill, \email{andrew.burchill@asu.edu}
#' @references \url{http://DOI OF THE PAPER} for the orginal source
#' @seealso \code{\link{birdtag}} for an example extension based on leg-banding birds,
#' @keywords reed_solomon
#'
#' @examples
#' plot_crayons()
#'
#' @export
#' @importFrom polynom polynomial
#' @importFrom numbers isPrime previousPrime


reed_solomon <- function(total_length, redundancy, alphabet, available.colors = NULL) {

  #The annoying "are your inputs valid?" error checking---------------

  if (missing(alphabet)) {
    stop("Error: you need to enter an 'alphabet size,' e.g. the number of paint colors you have")
  }
  if (missing(total_length)) {
    stop("Error: you need to enter the total length of the ID, e.g. how many color bands or paint drops on each organism")
  }
  if (missing(redundancy)) {
    stop("Error: you need specify to how many erasure events the IDs should be robust. Note, an increase in robustness requires an increase in the total length of the ID. ")
  }
  if (redundancy >= total_length || redundancy == 0) {
    stop("Error: the code must be robust to at least one erasure. It also cannot be robust to a number of positions equal to or greater than the total length.")
  }
  if (!numbers::isPrime(alphabet)) {
    warning(paste0("NOTE: Reed-Solomon codes require the 'alphabet size' (e.g. the number of paint colors you have) to be a prime number. Automatically adjusting to use an alphabet size ",numbers::previousPrime(alphabet)," instead of the entered value of ", alphabet, "."))
    alphabet <- numbers::previousPrime(alphabet)
  }
  if (total_length > alphabet) {
    warning(paste0("NOTE: Reed-Solomon coding requires the total length of the ID to be less than or equal to the size of the 'alphabet' (e.g. the number of paint colors you have). 'total_length' being changed to ", alphabet, " instead."))
    total_length <- alphabet
  }


  #okay now let's get to the actual code!----------------------------------

  #creates a matrix full of all possible messages of right length
  message <- total_length - redundancy

  perms <- rep(list(seq_len(alphabet)), message )
  combos <- as.matrix(expand.grid(perms))
  # we want to go from 0 to (max - 1) because of modulo
  combos <- combos - 1
  #creates a matrix to fill with the codewords
  codes <- matrix(data=NA, nrow = dim(combos)[1], ncol = total_length)
  #dynamically creates a polynomial from the permutations such as a + b*x + c*x^2 + d*x^3 ...
  #then evaluates it at x = the column number minus one (0,1,2,3...)
  for (i in seq_len(total_length)) codes[, i] <-
    apply(combos, 1, function(x) predict(polynomial(x), i - 1) %% alphabet)

  codes <- split(codes, 1:nrow(codes))
  names(codes) <- NULL
  codes <- codes_to_colors(codes, available.colors)
  return(codes)
}

