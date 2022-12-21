#' Plot number of sex
#'
#' This function Create a plot of number of total females and males
#' @param dl an object inherited from list. It is
#' assumed to have a `adsl` list name
#' @return a plot of number of females and males.
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 ggplot geom_col geom_text theme_bw xlab ylab
#' facet_wrap theme element_text
#' @importFrom dplyr inner_join group_by summarize
#' @examples
#' data(ukb_accel)
#' summary_sex(ukb_accel)
#' @export

summary_sex <- function(dl) {
  comb <- dl$adsl |> select(SUBJID, ATRT, SEX)

  summary_table <- comb |> group_by(ATRT) |> summarize(
    Female = sum(SEX == "Female"),
    Male = sum(SEX == "Male"))

  plot_tibble <- summary_table |>
    pivot_longer(-ATRT)
  plot_tibble |> ggplot(aes(x = name, y = value)) +
    geom_col() +
    geom_text(aes(label = value), vjust = 1.5, color = "white") +
    theme_bw() +
    facet_wrap("ATRT") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    xlab("Sex") + ylab("count")
}
