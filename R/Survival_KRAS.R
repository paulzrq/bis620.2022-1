#' survival analysis considering KRAS
#' @param x an object inherited from list.
#' @return survival analysis plot
#' @importFrom dplyr inner_join
#' @import survival
#' @import survminer
#' @export

survival_kras <- function(x) {
  survivaldata <- left_join(x$adsl, x$biomark, by = "SUBJID")
  survivaldata$arm <- paste(survivaldata$ATRT, survivaldata$BMMTR1,
                            sep = ",")
  ggsurvplot(survfit(Surv(DTHDY, DTH) ~ arm, data = survivaldata),
             data =  survivaldata)

}
