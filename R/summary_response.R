#' Plot summary of response
#'
#' This function summary the number of different types of
#' response conditioned on
#' "FOLFOX alone" and "Panitumumab + FOLFOX"
#' @param dl an object inherited from list. It is
#' assumed to have a `adae`, `adrsp`, `adsl` list name
#' @return a plot of number of different types of response.
#' @importFrom gtsummary tbl_summary
#' @importFrom purrr map
#' @importFrom ggplot2 ggplot aes
#' @importFrom dplyr n count group_nest mutate desc arrange
#' @examples
#' data(ukb_accel)
#' summary_response(ukb_accel)
#' @export
summary_response <- function(dl) {
  ae <- dl$adrsp
  find_best_response <- function(aep) {
    if ("Complete response" %in% aep$RSRESP) {
      return("Complete response")
    } else if ("Partial response" %in% aep$RSRESP) {
      return("Partial response")
    } else if ("Stable disease" %in% aep$RSRESP) {
      return("Stable disease")
    } else if ("Progressive disease" %in% aep$RSRESP) {
      return("Progressive disease")
    }
  }

  ae_nest <- ae |>
    group_by(SUBJID) |>
    group_nest(.key = "ae")
  dae <- inner_join(dl$adsl |> select(SUBJID, ATRT), ae_nest, by = "SUBJID")

  dae <- dae |> mutate(best_response = map(dae$ae, find_best_response))


  ae_counts <- dae |>
    group_by(ATRT) |>
    summarize(Complete_response = sum(best_response == "Complete response"),
              Partial_response = sum(best_response == "Partial response"),
              Stable_disease = sum(best_response == "Stable disease"),
              Progressive_disease = sum(
                best_response == "Progressive disease"), )


  ae_counts_tt <- ae_counts |>
    mutate(total = Complete_response + Partial_response +
             Stable_disease + Progressive_disease) |>
    arrange(desc(total)) |>
    select(-total) |>
    head(10) |>
    pivot_longer(-ATRT)

  ggplot(ae_counts_tt, aes(x = name, y = value)) +
    geom_col() +
    geom_text(aes(label = value), vjust = 1.5, color = "white") +
    theme_bw() +
    facet_wrap("ATRT") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

}
