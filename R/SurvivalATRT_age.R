#' Plot survival analysis
#'
#' This function returns a data summary by treatment(ATRT) in the Viewer,
#' provides the survival analysis result of
#' "FOLFOX alone" and "Panitumumab + FOLFOX" and see different treatments
#' with different age group and visualize it.
#' @param dl an object inherited from list. It is
#' assumed to have `adsl` and 'biomark' list name
#' @param detail an bool parameter to decide whether to show the
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
#' survivalatrt_age(ukb_accel)
#' @export
survivalatrt_age <- function(dl, detail = FALSE) {
  comb <- dl$biomark |> select(SUBJID, BMMTNM1, BMMTR1, BMMTNM2, BMMTR2,
                              BMMTNM3, BMMTR3, BMMTNM15, BMMTR15)
  comb <- inner_join(comb, dl$adsl, by = "SUBJID")
  find_type <- function(aep) {
    wild_num <- sum(aep == "Wild-type")
    unknown_num <- sum(aep == "Unknown")
    failure_num <- sum(aep == "Failure")
    if ("Mutant" %in% aep) {
      return("Mutant")
    } else if (wild_num > (failure_num + unknown_num)) {
      return("Wild-type")
    } else {
      return("Unknown")
    }
  }

  comb$kras <- apply(comb[, 1:9], 1, find_type)

  summary_table <- comb |>
    select(ATRT, DTH, AGE, SEX, B_WEIGHT, B_HEIGHT, RACE, kras) |>
    tbl_summary(by = "ATRT")
  comb$age_group <- ifelse(comb$AGE >= 27 & comb$AGE < 42, "age=[27, 42)",
            ifelse(comb$AGE >= 42 & comb$AGE < 57, "age=[42, 57)",
            ifelse(comb$AGE >= 57 & comb$AGE < 72, "age=[57, 72)",
            ifelse(comb$AGE >= 72 & comb$AGE < 85, "age=[72, 85)", NA))))
  comb$arm <- paste(comb$age_group, comb$ATRT, sep = ", ")
  survive <- survfit(Surv(DTHDY, DTH) ~ arm, data = comb)

  print(summary_table)
  print(survive)
  myplot <- ggsurvplot(survive, data = comb)
  print(myplot)
  if (detail) {
    print(summary(survive))
  }
  surv_diff <- survdiff(formula = Surv(DTHDY, DTH) ~ arm, data = comb)
  print(surv_diff)

}
