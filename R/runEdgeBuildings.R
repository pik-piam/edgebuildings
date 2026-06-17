#' Run EDGE-Buildings
#'
#' @param config scenario configuration. Can be a path to a config file or the
#'   name of an internal config file.
#' @param outputDir path to a directory to write files to
#' @param reporting reporting to apply
#' @param madratDir path to madrat root folder
#' @param inputdataRevision character, revision of the EDGE-B input data
#' @param forceDownload boolean, TRUE will force downloading the input data
#' @param scenario character vector, subset of scenarios to run. If NULL (default),
#'   all scenarios in the config file will be run. Scenario names correspond to
#'   column names in the config file (e.g., c("SSP2", "SSP5")). Note: SSP2 is
#'   always required and will be automatically included if not specified, as it
#'   provides reference values and is used for carrier share extension between
#'   historic data and scenario start.
#' @param includeClimate logical, if TRUE, each scenario will be expanded with
#'   climate variants by creating additional columns with RCP suffixes (1.9, 2.6,
#'   4.5, 6.0, 7.0). The original scenario column is kept as the noCC case. For
#'   example, "SSP2" will be expanded to "SSP2", "SSP2-1.9", "SSP2-2.6", "SSP2-4.5",
#'   "SSP2-6.0", and "SSP2-7.0". Default is FALSE.
#'
#' @author Falk Benke, Robin Hasse
#'
#' @importFrom piamutils getSystemFile
#' @importFrom targets tar_make
#' @importFrom pkgload is_dev_package
#' @importFrom utils read.csv2 write.csv2
#' @export

runEdgeBuildings <- function(config = "config_remind.csv",
                             outputDir = "./output",
                             reporting = NULL,
                             madratDir = NULL,
                             inputdataRevision = "0.5.12",
                             forceDownload = FALSE,
                             scenario = NULL,
                             includeClimate = FALSE) {

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

  # Store original config path for naming purposes
  originalConfig <- config

  # filter scenarios if specified and copy config to internal start folder ====

  if (!is.null(scenario)) {
    # Read config to get column names
    configData <- read.csv2(config, stringsAsFactors = FALSE, check.names = FALSE)
    configCols <- names(configData)

    # Available scenarios are all columns except 'parameter' and 'valueComment'
    availableScenarios <- setdiff(configCols, c("parameter", "valueComment"))

    # Validate requested scenarios
    invalidScenarios <- setdiff(scenario, availableScenarios)
    if (length(invalidScenarios) > 0) {
      stop("Invalid scenario(s) requested: ", paste(invalidScenarios, collapse = ", "),
           "\nAvailable scenarios in config: ", paste(availableScenarios, collapse = ", "))
    }

    # SSP2 is always required (for refIncomeThresholdEC and carrier share extension)
    if (!"SSP2" %in% scenario) {
      if (!"SSP2" %in% availableScenarios) {
        stop("SSP2 scenario is required but not available in the config file.")
      }
      message("Note: SSP2 scenario has been automatically included.")
      scenario <- c(scenario, "SSP2")
    }

    # Filter to requested scenarios
    columnsToKeep <- c("parameter", "valueComment", scenario)
    filteredConfig <- configData[, columnsToKeep, drop = FALSE]

    # Expand with climate scenarios if requested
    if (includeClimate) {
      message("Expanding scenarios with climate variants (RCP: 1.9, 2.6, 4.5, 6.0, 7.0)")
      filteredConfig <- .expandClimateScenarios(filteredConfig)
    }

    # Write filtered/expanded config directly to start folder
    filteredConfigPath <- file.path(getSystemFile("start", package = "edgebuildings"),
                                    "config.csv")
    write.csv2(filteredConfig,
               filteredConfigPath,
               row.names = FALSE,
               quote = FALSE)

    # Update config to point to filtered version for later use
    config <- filteredConfigPath

    message("Filtering to scenario(s): ", paste(scenario, collapse = ", "))
  } else {
    # Read config
    configData <- read.csv2(config, stringsAsFactors = FALSE, check.names = FALSE)

    # Expand with climate scenarios if requested
    if (includeClimate) {
      message("Expanding scenarios with climate variants (RCP: 1.9, 2.6, 4.5, 6.0, 7.0)")
      configData <- .expandClimateScenarios(configData)
    }

    # Write config to internal start folder
    filteredConfigPath <- file.path(getSystemFile("start", package = "edgebuildings"),
                                    "config.csv")
    write.csv2(configData,
               filteredConfigPath,
               row.names = FALSE,
               quote = FALSE)

    # Update config to point to version in start folder for later use
    config <- filteredConfigPath
  }



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

  # name after config file name and create the folder
  runfolder <- paste0(sub("^([^\\.]+)\\..+$", "\\1", basename(originalConfig)),
                      format(Sys.time(), "_%Y-%m-%d_%H.%M.%S"))
  runFolderDir <- file.path(outputDir, runfolder)
  dir.create(runFolderDir)


  # copy config ====
  # Copy the filtered config that was actually used, not the original
  invisible(file.copy(config, runFolderDir))
  cfgText <- c(paste("Edgebuildings version:", utils::packageVersion("edgebuildings")),
               paste("Input revision:", inputdataRevision),
               paste("Creation date:", format(Sys.time(), "%A %B %d %Y %H:%M:%S")))
  write(cfgText, file = file.path(runFolderDir, "cfg.txt"))

  # copy projections ====

  scenarios <- rownames(readConfig(config = config, subtype = "scenario"))
  invisible(file.copy(
    file.path(getSystemFile(package = "edgebuildings"), "output",
              paste0("projections_", scenarios, ".csv")),
    runFolderDir
  ))


  if (!is.null(reporting)) {
    reportResults(runFolderDir, reporting, cfgText)
  }
}



#' Expand config with climate scenarios
#'
#' Internal helper function to expand scenario columns with climate variants.
#' For each scenario column, creates 5 additional columns with RCP suffixes
#' (1.9, 2.6, 4.5, 6.0, 7.0) while keeping the original column as the noCC case.
#'
#' @param configData data frame, the config data to expand
#' @return data frame with expanded scenario columns
#' @keywords internal
.expandClimateScenarios <- function(configData) {

  # Get scenario column names (exclude 'parameter' and 'valueComment')
  scenarioCols <- setdiff(names(configData), c("parameter", "valueComment"))

  # Climate RCP scenarios to add
  rcpScenarios <- c("1.9", "2.6", "4.5", "6.0", "7.0")

  # Create expanded config starting with base columns
  expandedConfig <- configData[, c("parameter", "valueComment"), drop = FALSE]

  # For each scenario, create climate variants
  for (scen in scenarioCols) {
    # Add original scenario (noCC case) first
    expandedConfig[[scen]] <- configData[[scen]]

    # Add climate variants
    for (rcp in rcpScenarios) {
      newScenName <- paste0(scen, "-", rcp)
      # Copy all values from original scenario
      expandedConfig[[newScenName]] <- configData[[scen]]
      # Update rcpScen parameter for this scenario
      expandedConfig["rcpScen", newScenName] <- rcp
    }
  }

  return(expandedConfig)
}
