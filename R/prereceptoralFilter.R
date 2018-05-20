#' @export
prereceptoralFilter <- function(coneFund, filter) {
  ConeFundNew <- coneFund * 10 ^ -filter
  return(ConeFundNew / max(ConeFundNew))
}
