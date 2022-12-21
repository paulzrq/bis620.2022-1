#' Plot survival analysis
#'
#' This function returns a data summary by treatment(ATRT) in the Viewer,
#' provides the survival analysis result of
#' "FOLFOX alone" and "Panitumumab + FOLFOX" and visualize it.
#' @param dl an  object inherited from list. It is
#' assumed to have `adsl` and 'biomark' list name
#' @param detail a bool parameter, should be set as TRUE or FALSE
#' @return a visualization of survival analysis on the 'Plots' with
#' p-value, a gusummary on the 'Viewer'
#' and a table of survival analysis result on
#' "FOLFOX alone" and "Panitumumab + FOLFOX"
#' @importFrom dplyr left_join
#' @importFrom gtsummary tbl_summary
#' @importFrom survminer ggsurvplot
#' @importFrom survival Surv survfit survdiff
#' @examples
#' data(ukb_accel)
#' summary_response(ukb_accel)
#' @export
#'

survival_atrt <- function(dl, detail = FALSE) {
  demo <- left_join(dl$adsl, dl$biomark, by = "SUBJID")
  summary_table <- demo |>
    select(ATRT, DTH, AGE, SEX, B_WEIGHT, B_HEIGHT, RACE) |>
    tbl_summary(by = "ATRT")
  survive <- survfit(Surv(DTHDY, DTH) ~ ATRT, data = demo)

  print(summary_table)
  print(survive)
  ggsurvplot(survfit(Surv(DTHDY, DTH) ~ ATRT, data = demo),
             data = demo, pval = TRUE, conf.int = TRUE)
  if (detail) {
    print(summary(survive))
  }
  surv_diff <- survdiff(formula = Surv(DTHDY, DTH) ~ ATRT, data = demo)
  print(surv_diff)

}
