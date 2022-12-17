#' process the data
#' This function does some data processing.
#' @param x an object inherited from list.
#' @return data for linear models.
#' @importFrom dplyr inner_join
#' @importFrom nlme lme
#' @importFrom car avPlot
#' @import survival
#' @import survminer
#' @export
data_process <- function(x) {
  demo1 <- left_join(x$adsl, x$adae, by = "SUBJID")
  demo1$SEX <- as.numeric(factor(demo1$SEX, levels = unique(demo1$SEX),
                                 exclude = NULL))
  demo1$RACE <- as.numeric(factor(demo1$RACE, levels = unique(demo1$RACE),
                                  exclude = NULL))
  demo1$ATRT <- as.numeric(factor(demo1$ATRT, levels = unique(demo1$ATRT),
                                  exclude = NULL))
  demo2 <- left_join(demo1, x$biomark, by = "SUBJID")
  demo2$BMMTR1 <- as.numeric(factor(demo2$BMMTR1,
                          levels = unique(demo2$BMMTR1), exclude = NULL))
  dataset <- demo2|>
    group_by(SUBJID)|>
    summarize(y = DTH,
              x1_sex = SEX,
              x2_race = RACE,
              x3_atrt = ATRT,
              x4_biomark = BMMTR1,
              x5_age = AGE,
              x6_day = DTHDY)

  survivaldata <- left_join(x$adsl, x$biomark, by = "SUBJID")
  survivaldata$arm <- paste(survivaldata$ATRT, survivaldata$BMMTR1,
                            sep = ",")

  m2 <- lm(y ~ x1_sex + x2_race + x3_atrt + x4_biomark + x5_age,
           data = unique(dataset))
  par(mfrow = c(2, 3))
  avPlot(m2, variable = "x1_sex", ask = FALSE)
  avPlot(m2, variable = "x2_race", ask = FALSE)
  avPlot(m2, variable = "x3_atrt", ask = FALSE)
  avPlot(m2, variable = "x4_biomark", ask = FALSE)
  avPlot(m2, variable = "x5_age", ask = FALSE)
  ggsurvplot(survfit(Surv(DTHDY, DTH) ~ arm, data = survivaldata),
             data = survivaldata)

}
