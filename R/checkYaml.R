#' Check if .yaml file contains correct stores for targets pipeline
#'
#' @author Hagen Tockhorn
#'
#' @importFrom yaml yaml.load_file
#' @importFrom stringr str_detect


checkYaml <- function() {

  # PARAMETERS------------------------------------------------------------------

  edgeFolder <- "EDGE-Buildings"
  filename   <- "_targets.yaml"

  subtargets <- c("files", "data", "input", "projections")


  # CHECKS----------------------------------------------------------------------

  # check working directory
  if (basename(getwd()) != "EDGE-Buildings") {
    tryCatch(
      {
        dirs <- list.dirs()

        matchingDirs <- grep(edgeFolder, dirs, value = TRUE)

        if (length(matchingDirs) > 0) {
          setwd(matchingDirs[1]) #nolint
          print(paste0("Switched working directory to: ", matchingDirs[1]))
        }
      },
      error = function(e) {
        print("Please make sure to set the correct working directory.")
      }
    )
  }


  # check whether file exists
  if (!file.exists(filename)) {
    return(FALSE)
  }


  # check if file contains correct stores
  data <- yaml.load_file(filename)

  if (all(subtargets %in% names(data))) {
    print("targets storages exist.")
    return(TRUE)
  } else {
    return(FALSE)
  }
}
