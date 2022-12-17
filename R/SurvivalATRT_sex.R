#' Plot survival analysis
#'
#' This function returns a data summary by treatment(ATRT) in the Viewer,
#' provides the survival analysis result of
#' "FOLFOX alone" and "Panitumumab + FOLFOX" and see different treatments with different sex and visualize it.
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
#' @examples
#' data(ukb_accel)
#' survivalATRT_sex(ukb_accel)
#' @export

survivalATRT_sex <- function(dl,detail=FALSE) {
  comb = dl$biomark |> select(SUBJID,BMMTNM1,BMMTR1,BMMTNM2,BMMTR2,BMMTNM3,BMMTR3,BMMTNM15,BMMTR15)
  comb = inner_join(comb, dl$adsl, by='SUBJID')
  find_type = function(aep){
    if ("Mutant" %in% aep){
      return("Mutant")
    }
    else{
      Wild_num = sum(aep == "Wild-type")
      Unknown_num = sum(aep == "Unknown")
      Failure_num = sum(aep == "Failure")
      if(Wild_num > (Failure_num + Unknown_num)){
        return("Wild-type")
      }
      else{
        return("Unkown")
      }
    }
  }

  type = c()
  for (i in (1:length(comb$BMMTR15))){
    type = c(type, find_type(comb[i,1:9]))
  }
  comb$KRAS = type
  comb$SEX_ATRT = paste(comb$SEX,comb$ATRT,sep = ',')
  summary_table <- comb |>
    select(ATRT, DTH, AGE, SEX, B_WEIGHT, B_HEIGHT, RACE, KRAS) |>
    tbl_summary(by = "ATRT")
  survive = survfit(Surv(DTHDY, DTH) ~ SEX_ATRT, data = comb)

  print(summary_table)
  print(survive)
  myplot <- ggsurvplot(survive, data = comb)
  print(myplot)
  if (detail){
    print(summary(survive))
  }
  surv_diff = survdiff(formula = Surv(DTHDY, DTH) ~ SEX_ATRT, data = comb)
  print(surv_diff)

}
