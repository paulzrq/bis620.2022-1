#' process the data
#' This function builds linear models.
#' @param x an object inherited from list.
#' @return data for linear models.
#' @importFrom dplyr inner_join
#' @importFrom nlme lme
#' @importFrom car avPlot
#' @importFrom stats complete.cases na.exclude
#' @export
linear_regression <- function(x) {
  demo1 <- left_join(x$adsl, x$adae, by = "SUBJID")
  demo1$SEX <- as.numeric(factor(demo1$SEX,
                                 levels = unique(demo1$SEX), exclude = NULL))
  demo1$RACE <- as.numeric(factor(demo1$RACE,
                                  levels = unique(demo1$RACE), exclude = NULL))
  demo1$ATRT <- as.numeric(factor(demo1$ATRT,
                                  levels = unique(demo1$ATRT), exclude = NULL))
  demo2 <- left_join(demo1, x$biomark, by = "SUBJID")
  demo2$BMMTR1 <- as.numeric(factor(demo2$BMMTR1,
                                    levels = unique(demo2$BMMTR1),
                                    exclude = NULL))
  dataset <- demo2|>
    group_by(SUBJID)|>
    summarize(y = DTH,
              x1_sex = SEX,
              x2_race = RACE,
              x3_atrt = ATRT,
              x4_biomark = BMMTR1,
              x5_age = AGE,
              x6_weight = B_WEIGHT,
              x7_height = B_HEIGHT
              )
  dataset <- dataset[complete.cases(dataset), ]

  m1 <- lme(y ~ x1_sex + x2_race + x3_atrt + x5_age + x6_weight + x7_height,
            random = ~ 1 | x4_biomark, data = unique(dataset))
  print(summary(m1))
  m2 <- lm(y ~ x1_sex + x2_race + x3_atrt + x4_biomark +
             x5_age + x6_weight + x7_height, data = unique(dataset))
  print(summary(m2))

  avPlot(m2, variable = "x1_sex", ask = FALSE)
  avPlot(m2, variable = "x2_race", ask = FALSE)
  avPlot(m2, variable = "x3_atrt", ask = FALSE)
  avPlot(m2, variable = "x4_biomark", ask = FALSE)
  avPlot(m2, variable = "x5_age", ask = FALSE)
  avPlot(m2, variable = "x6_weight", ask = FALSE)
  avPlot(m2, variable = "x7_height", ask = FALSE)
}
