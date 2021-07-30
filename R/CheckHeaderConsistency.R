

CheckHeaderConsistency <- function(){
  library(tidyverse)

  files = list.files(path = "/Users/jason/GitHub/IMOS_Toolbox/Plankton/RawData", ".csv", full.names = TRUE)

  nm <- character()
  for (f in 1:length(files)){

    out <- read_csv(files[f])
    nm <- c(nm, names(out))
    rm(out)
  }

  uni_nm <- sort(unique(nm))

  return(uni_nm)
}
