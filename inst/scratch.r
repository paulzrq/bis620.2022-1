library(devtools)
library(lubridate)
library(dplyr)
library(ggplot2)

document()

data(ukb_accel)


summary_baseline(ukb_accel)
summary_response(ukb_accel)
summary_kras(ukb_accel)
summary_sex(ukb_accel)
