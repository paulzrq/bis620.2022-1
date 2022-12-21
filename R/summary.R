
#' Summary the toxicity condition
#' This function summary the grade of toxicity.
#' @param x an object inherited from list. It is
#' assumed to have a `adae` list name along with
#'a `AESEVCD` data.frame
#' @return a summary form of grade of toxicity.
#' @importFrom gtsummary tbl_summary
#' @examples
#' data(ukb_accel)
#' summary_toxicity(ukb_accel)
#' @export
summary_toxicity <- function(x) {
  x$adae |> select(AESEVCD) |> tbl_summary()
}
