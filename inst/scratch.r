library(devtools)
library(lubridate)
library(dplyr)
library(ggplot2)

document()

data(ukb_accel)

DataPlot(ukb_accel)
DataProcess(ukb_accel)
summary_baseline(ukb_accel)
summary_response(ukb_accel)
summary_KRAS(ukb_accel)
summary_sex(ukb_accel)

survival_ATRT(ukb_accel)
survival_ATRT_KRAS(ukb_accel)
