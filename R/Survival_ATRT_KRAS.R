#' Ruturn the result of survival analysis
#'
#' This function returns a result of survival analysis
#' to assess the effect of
#' treatment conditioned on different type of KRAS and visualize it.
#' @param dl an object inherited from list. It is
#' assumed to have `adsl` and 'biomark' list name
#' @param name a character, type of KRAS a user need to choose, including
#' "Wild-type", "Unknown", and "Mutant", its defalt value is "Mutant"
#' @param detailed a bool paramter, decide whether to show the
#' summary of survival analysis
#' @return a visualization of survival analysis on the 'Plots' with
#' p-value, two tibbles of survival analysis result on
#' "FOLFOX alone" and "Panitumumab + FOLFOX" conditioned on type
#' of KRAS you choose, and the summary of survival analysis
#' @importFrom dplyr left_join filter
#' @importFrom gtsummary tbl_summary
#' @importFrom survminer ggsurvplot
#' @importFrom survival Surv survfit survdiff
#' @importFrom utils tail
#' @examples
#' data(ukb_accel)
#' summary_response(ukb_accel)
#' @export
#'

survival_atrt_kras <- function(dl, name = "Mutant", detailed = FALSE) {
  kras_names <- c("Wild-type", "Unknown", "Mutant")
  if (!name %in% kras_names) {
    warning("There is no '", name, "' in the KRAS type, please type
             'Wild-type', 'Unknown', or 'Mutant', the defalt value
             for name is 'Mutant'")
    name <- "Mutant"
  }

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
        return("Unknown")
      }
    }
  }
  type <- c()
  comb <- dl$biomark |> select(SUBJID, BMMTNM1, BMMTR1,
                              BMMTNM2, BMMTR2, BMMTNM3, BMMTR3,
                              BMMTNM15, BMMTR15)
  comb <- inner_join(comb, dl$adsl |>
                      select(ATRT, SUBJID, DTHDY, DTH),
                    by = "SUBJID")
  for (i in (1:tail(seq_along(comb$BMMTR15), 1))){
    type <- c(type, find_type(comb[i, ]))
  }
  comb$kras <- type
  name |> assign(comb |> dplyr::filter(kras == name) |>
    select(ATRT, kras, DTHDY, DTH))
  survive <- survfit(Surv(DTHDY, DTH) ~ ATRT, data = get(name))
  surv_diff <- survdiff(formula = Surv(DTHDY, DTH) ~ ATRT,
                       data = get(name))
  print(survive)
  print(surv_diff)
  survive_plot <- ggsurvplot(survfit(Surv(DTHDY, DTH) ~ ATRT, data = get(name)),
             data = get(name), pval = TRUE, conf.int = TRUE)
  print(survive_plot)

  if (detailed) {
    print(summary(survive))
  }

}
