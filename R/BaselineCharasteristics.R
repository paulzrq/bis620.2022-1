#' Plot summary of baseline characteristics
#'
#' This function summary the baseline characteristics
#' conditioned on
#' "FOLFOX alone" and "Panitumumab + FOLFOX"
#' @param dl an object inherited from list. It is
#' assumed to have a `adsl` list name
#' @return a data.frame of summary of baseline characteristics
#' including number of male, number female, mean of age,
#' std of age, mean of weight, std of weight, mean of height,
#' std of height, number of different race.
#' @importFrom plyr join_all
#' @importFrom dplyr summarize group_by
#' @examples
#' data(ukb_accel)
#' summary_response(ukb_accel)
#' @export

summary_baseline <- function(dl) {
  adsl <- dl$adsl
  male <- adsl |> group_by(ATRT) |>
    dplyr::summarize(male = sum(SEX == "Male"))
  female <- adsl |> group_by(ATRT) |>
    dplyr::summarize(female = sum(SEX == "Female"))
  age_summary <- adsl |> group_by(ATRT) |>
    dplyr::summarize(mean_age = mean(AGE), std_age = sd(AGE))
  weight_summary <- adsl |> group_by(ATRT) |>
    dplyr::summarize(mean_weight = mean(B_WEIGHT),
              std_weight = sd(B_WEIGHT))
  height_summary <- adsl |> group_by(ATRT) |>
    dplyr::summarize(mean_height = mean(B_HEIGHT, na.rm = TRUE),
              std_height = sd(B_HEIGHT,  na.rm = TRUE))

  race_summary <- adsl |> group_by(ATRT) |>
    dplyr::summarize(Asian = sum(RACE == "Asian"),
              Black_African = sum(RACE == "Black or African American"),
              Hispanic_Latino = sum(RACE == "Hispanic or Latino"),
              Other = sum(RACE == "Other"),
              White_Caucasian = sum(RACE == "White or Caucasian"), )
  overall_summary <- join_all(list(male, female, age_summary,
                      weight_summary, height_summary, race_summary),
                      by = "ATRT")
  overall_summary <-  t(data.frame(overall_summary, row.names = 1))
  round(overall_summary, 2)
}
