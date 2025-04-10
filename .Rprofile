.First <- function() {
  dir.create(paste0(getwd(), "/data-processed"), showWarnings = F)
  dir.create(paste0(getwd(), "/data-raw"), showWarnings = F)
  dir.create(paste0(getwd(), "/figures"), showWarnings = F)
  dir.create(paste0(getwd(), "/R"), showWarnings = F)
  dir.create(paste0(getwd(), "/reports"), showWarnings = F)
  
  if (!("renv" %in% list.files())) {
    renv::init()
  } else {
    source("renv/activate.R")
  }
  
  cat("\nWelcome to your R-Project:", basename(getwd()), "\n")
}
