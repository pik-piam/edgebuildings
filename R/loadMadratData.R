#' Load input data from mredgebuildings
#'
#' Note: This function has been closely adapted from \code{loadMadratData} from BRICK
#'       by Robin Hasse.
#'
#' @author Hagen Tockhorn
#' @param inputdataRevision character, revision of the EDGE-B input data
#' @param forceDownload boolean, TRUE will force downloading the input data
#'
#' @return path to madrat input data directory
#'
#' @importFrom madrat getConfig toolGetMapping regionscode
#' @importFrom gms download_distribute
#' @importFrom piamutils getSystemFile
#' @export

loadMadratData <- function(inputdataRevision,
                           forceDownload = FALSE) {

  workingDir <- getSystemFile(package = "edgebuildings")
  inputDir <- file.path(workingDir, "input")


  # fixed regional resolution
  regionmap <- toolGetMapping("regionmappingISO-EDGE_EUR_ETP.csv", "regional", where = "mredgebuildings")

  # LOAD TGZ FILES--------------------------------------------------------------

  # Identify madrat tgz files
  sourceFiles <- file.path(inputDir, "source_files.log")

  if (file.exists(sourceFiles)) {
    madratOld <- readLines(sourceFiles)[1]
  } else {
    madratOld <- "noData"
  }

  # where to get new files from
  madratNew <- paste0("rev",
                      inputdataRevision, "_",
                      regionscode(regionmap), "_",
                      "edgebuildings.tgz")

  if (!setequal(madratNew, madratOld) || forceDownload) {
    message(
      if (forceDownload) {
        "You set 'forceDownload = TRUE'."
      } else {
        "Your input data are outdated or in a different regional resolution. "
      },
      "New input data are downloaded and distributed."
    )

    # delete all files but source_files.log
    file.remove(grep("source_files.log", list.files(inputDir, full.names = TRUE),
                     value = TRUE, invert = TRUE))

    # directory to look for the madrat tgz file
    repositories <- list(NULL)
    names(repositories) <- getConfig("outputfolder")

    # load tgz file and unpack it in input folder
    download_distribute(madratNew, repositories, modelfolder = workingDir, stopOnMissing = TRUE)

  } else {
    message("No input data downloaded and distributed. To enable that, ",
            "delete input/source_files.log or set forceDownload to TRUE.")
  }

  # OUTPUT----------------------------------------------------------------------

  # return path to data directory
  return(inputDir)

}
