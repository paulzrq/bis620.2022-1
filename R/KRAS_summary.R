#' Plot number of KRAS Status
#'
#' This function Create a plot of number of total mutant,
#'  wild-type, and unknown KRAS counts conditioned on
#' "FOLFOX alone" and "Panitumumab + FOLFOX"
#' @param dl an object inherited from list. It is
#' assumed to have a `biomark` and `adsl` list name
#' @return a plot of number of KRAS Status.
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 ggplot geom_col geom_text theme_bw
#' facet_wrap theme element_text xlab ylab
#' @importFrom dplyr inner_join group_by summarize
#' @examples
#' data(ukb_accel)
#' summary_response(ukb_accel)
#' @export



summary_kras <- function(dl) {
  comb <- dl$biomark |> select(SUBJID, BMMTNM1, BMMTR1,
        BMMTNM2, BMMTR2, BMMTNM3, BMMTR3, BMMTNM15, BMMTR15)
  comb <- inner_join(comb, dl$adsl |> select(ATRT, SUBJID),
                    by = "SUBJID")
  find_type <- function(aep) {
    if ("Mutant" %in% aep) {
      return("Mutant")
    }else {
      wild_num <- sum(aep == "Wild-type")
      unknown_num <- sum(aep == "Unknown")
      failure_num <- sum(aep == "Failure")
      if (wild_num > (failure_num + unknown_num)) {
        return("Wild-type")
      }else {
        return("Unkown")
      }
    }
  }

  type <- c()
  for (i in (seq_len(length(comb$BMMTR15)))) {
    type <- c(type, find_type(comb[i, ]))
  }
  comb$kras <- type
  summary_table <- comb |> group_by(ATRT) |> dplyr::summarize(
    Wild_type = sum(kras == "Wild-type"),
    Unknown = sum(kras == "Unkown"),
    Failure = sum(kras == "Failure"),
    Mutant = sum(kras == "Mutant"))
  plot_tibble <- summary_table |>
    pivot_longer(-ATRT)
  plot_tibble |> ggplot(aes(x = name, y = value)) +
    geom_col() +
    geom_text(aes(label = value), vjust = 1.5, color = "white") +
    theme_bw() +
    facet_wrap("ATRT") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    xlab("KRAS type") + ylab("count")
}
