#' Run EDGE-Buildings
#'
#' @param config scenario configuration. Can be a path to a config file or the
#'   name of an internal config file.
#' @param outputDir path to a directory to write files to
#' @param reporting reporting to apply
#' @param madratDir path to madrat root folder
#' @param inputdataRevision character, revision of the EDGE-B input data
#' @param forceDownload boolean, TRUE will force downloading the input data
#'
#' @author Falk Benke, Robin Hasse
#'
#' @importFrom piamutils getSystemFile
#' @importFrom targets tar_make
#' @importFrom pkgload is_dev_package
#' @export

runEdgeBuildings <- function(config = "configEDGEscens.csv",
                             outputDir = "./output",
                             reporting = NULL,
                             madratDir = NULL,
                             inputdataRevision = "0.5",
                             forceDownload = FALSE) {

  # TODO: relocate data_internal
  # TODO: fix all warnings and errors (lucode2::buildLibrary)



  # CONFIG ---------------------------------------------------------------------

  ## find config ====

  if (!file.exists(config)) {
    config <- getSystemFile("config", config,
                            package = "edgebuildings",
                            mustWork = TRUE)
    if (!file.exists(config)) {
      stop("You passed an invalid config: ", config,
           " is not a valid path and not a config file provided with the package.")
    }
  }
  message("using this config file: ", config)


  # copy config to internal start folder ====

  file.copy(config,
            file.path(getSystemFile("start", package = "edgebuildings"),
                      "config.csv"),
            overwrite = TRUE)



  # INPUT DATA -----------------------------------------------------------------

  # Madrat Input Directory
  if (is.null(madratDir)) {
    madratDir <- getOption("MADRAT_MAINFOLDER")
  }
  loadMadratData(inputdataRevision, forceDownload)



  # RUN TARGETS ----------------------------------------------------------------

  message("# starting targets")
  targetsDir <- getSystemFile("_targets", package = "edgebuildings")
  oldwd <- getwd()
  tryCatch(
    {
      Sys.setenv("edgebVer" = if (is_dev_package("edgebuildings")) {
        find.package("edgebuildings")
      } else {
        0
      })
      setwd(getSystemFile(package = "edgebuildings"))
      tar_make(store = targetsDir,
               envir = getNamespace("edgebuildings"))

    },
    error = function(e) {
      setwd(oldwd)
      stop(e)
    }
  )
  setwd(oldwd)



  # REPORTING ------------------------------------------------------------------

  # run folder ====

  # named after config file name
  runfolder <- paste0(sub("^([^\\.]+)\\..+$", "\\1", basename(config)),
                      format(Sys.time(), "_%Y-%m-%d_%H.%M.%S"))


  # copy projections ====

  runFolderDir <- file.path(outputDir, runfolder)
  dir.create(runFolderDir)
  scenarios <- rownames(readConfig(config = config, subtype = "scenario"))
  invisible(file.copy(
    file.path(getSystemFile(package = "edgebuildings"), "output",
              paste0("projections_", scenarios, ".csv")),
    runFolderDir)
  )


  if (!is.null(reporting)) {
    reportResults(runFolderDir, reporting)
  }
}
