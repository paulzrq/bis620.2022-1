## code to prepare `DATASET` dataset goes here
library(haven)
library(purrr)
folfox_path <- file.path("data-raw", "AllProvidedFiles_309",
                        "PDS_DSA_20050203")
ff_files <- dir(folfox_path)
ff_names <- gsub("_pds2019.sas7bdat", "", ff_files)
dl <- map(file.path(folfox_path, ff_files), read_sas)
names(dl) <- ff_names
ukb_accel <- dl
usethis::use_data(ukb_accel, overwrite = TRUE)
