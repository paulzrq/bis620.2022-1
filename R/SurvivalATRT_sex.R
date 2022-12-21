#' Plot survival analysis
#'
#' This function returns a data summary by treatment(ATRT) in the Viewer,
#' provides the survival analysis result of
#' "FOLFOX alone" and "Panitumumab + FOLFOX" and see different treatments
#' with different sex and visualize it.
#' @param dl an object inherited from list. It is
#' assumed to have `adsl` and 'biomark' list name
#' @param detail is a bool parameter to decide whether to show the
#' detailed summary of survival analysis
#' @return a visualization of survival analysis on the 'Plots' with
#' p-value, a gusummary on the 'Viewer'
#' and a table of survival analysis results on
#' two different treatments "FOLFOX alone" "Panitumumab + FOLFOX" with
#' three distinct biomarks "Mutant","Unkown" and "Wild-Type"
#' @importFrom dplyr left_join
#' @importFrom gtsummary tbl_summary
#' @importFrom survminer ggsurvplot
#' @importFrom survival Surv survfit survdiff
#' @importFrom utils tail
#' @examples
#' data(ukb_accel)
#' survivalatrt_sex(ukb_accel)
#' @export

survivalatrt_sex <- function(dl, detail = FALSE) {
  comb <- dl$biomark |> select(SUBJID, BMMTNM1, BMMTR1, BMMTNM2, BMMTR2,
                              BMMTNM3, BMMTR3, BMMTNM15, BMMTR15)
  comb <- inner_join(comb, dl$adsl, by = "SUBJID")
  find_type <- function(aep) {
    if ("Mutant" %in% aep) {
      return("Mutant")
    } else {
      wild_num <- sum(aep == "Wild-type")
      unknown_num <- sum(aep == "Unknown")
      failure_num <- sum(aep == "Failure")
      if (wild_num > (failure_num + unknown_num)) {
        return("Wild-type")
      } else {
        return("Unkown")
      }
    }
  }

  type <- c()
  for (i in (1:tail(seq_along(comb$BMMTR15), 1))){
    type <- c(type, find_type(comb[i, 1:9]))
  }
  comb$kras <- type
  comb$sex_atrt <- paste(comb$SEX, comb$ATRT, sep = ",")
  summary_table <- comb |>
    select(ATRT, DTH, AGE, SEX, B_WEIGHT, B_HEIGHT, RACE, kras) |>
    tbl_summary(by = "ATRT")
  survive <- survfit(Surv(DTHDY, DTH) ~ sex_atrt, data = comb)

  print(summary_table)
  print(survive)
  myplot <- ggsurvplot(survive, data = comb)
  print(myplot)
  if (detail) {
    print(summary(survive))
  }
  surv_diff <- survdiff(formula = Surv(DTHDY, DTH) ~ sex_atrt, data = comb)
  print(surv_diff)

}
