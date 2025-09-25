#' Pre-process degree day data for EDGE-B
#'
#' Degree day data is filtered and processed according to specifications made
#' in the \code{config} file. Scenarios assume a gradual shift in limit temperatures
#' for both heating and cooling degree days relative to historic thresholds, i.e.
#' heating and cooling behavior changes w.r.t to time and assumed scenario.
#'
#' Hence, degree day data converges from historic thresholds \code{tlimHist} to
#' assumed target threshold \code{tlimTarget} in a time period from \code{endOfHistory}
#' to \code{yearTarget}. The convergence is done region- and variable-wise.
#'
#' @note
#' Since the current data only delivers historic data until 2014, but EDGE-B defines
#' historic data until endOfHistory, projected degree day data from SSP2 between 2015-endOfHistory
#' is used to fill this gap. The degree day data is then linearly converged in the
#' same time period towards the respective scenario values.
#'
#'
#' @param hddcdd data.frame with degree days per year
#' @param config data.frame with configuration parameters for specific scenario
#' @param regionmap region mapping
#'
#' @return converged degree day data
#'
#' @author Hagen Tockhorn
#'
#' @importFrom dplyr rename mutate left_join filter rename
#' @importFrom tidyr separate unite pivot_wider
#' @importFrom quitte getRegs


prepHDDCDD <- function(hddcdd, config, regionmap) {

  # PARAMETERS -----------------------------------------------------------------

  # scenario to fill missing historic data
  scenFillHist <- "SSP2"

  # scenarios to fill
  scensToFill <- c("SSP1", "SSP3", "SSP4", "SSP5")

  # end of historic degree day data
  endOfDDHistory <- 2014


  # READ-IN DATA ---------------------------------------------------------------

  #--- extract parameters from config file

  scenConfig <- row.names(config) %>% unique()

  # upper temporal boundary of historic data
  endOfHistory <- config[["endOfHistory"]] %>% unlist()


  # scenarios
  ssp <- config[["hddcddScen"]] %>% unlist()
  rcp <- config[["rcpScen"]] %>% unlist()

  scen <- paste(ssp, rcp, sep = "_")


  # historic limit/set point temperature
  tlimHist <- c("HDD" = config[["tlimHist_HDD"]] %>% unlist(),
                "CDD" = config[["tlimHist_CDD"]] %>% unlist())


  # target limit temperatures as region-wise data.frames
  tlimTargetHDD <- config[["tlimTarget_HDD"]] %>%
    buildScenInput(subtype = "mapping",
                   regionmap = regionmap) %>%
    mutate(variable = "HDD")

  tlimTargetCDD <-  config[["tlimTarget_CDD"]] %>%
    buildScenInput(subtype = "mapping",
                   regionmap = regionmap) %>%
    mutate(variable = "CDD")


  # temporal convergence targets
  yearTargetHDD <- config[["speed_HDD"]] %>%
    buildScenInput(subtype = "mapping",
                   regionmap = regionmap) %>%
    rename("fullconv" = "value") %>%
    mutate(scenario = scen)

  yearTargetCDD <- config[["speed_CDD"]] %>%
    buildScenInput(subtype = "mapping",
                   regionmap = regionmap) %>%
    rename("fullconv" = "value") %>%
    mutate(scenario = scen)


  # PROCESS DATA ---------------------------------------------------------------

  # combine scenarios to fill historic data with rcp scenarios
  scensToFill <- paste(scensToFill, rcp, sep = "_")

  # calculate convergence shares
  lambda <- rbind(
    # hdd
    compLambdaScen(yearTargetHDD, endOfHistory, startYearVector = 1960) %>%
      mutate(variable = "HDD"),

    # cdd
    compLambdaScen(yearTargetCDD, endOfHistory, startYearVector = 1960) %>%
      mutate(variable = "CDD")
  )


  # bind limit temperature data.frames
  tlimTarget <- rbind(tlimTargetHDD, tlimTargetCDD)

  # split variables
  hddcdd <- hddcdd %>%
    separate(col = "variable", into = c("variable", "tlim"), sep = "_") %>%
    mutate(tlim = as.numeric(.data[["tlim"]]))


  # converge data
  data <- do.call(
    "rbind",
    lapply(
      c("HDD", "CDD"),
      function(typeDD) {
        do.call(
          "rbind",
          lapply(
            getRegs(hddcdd),
            function(reg) {
              # regional limit temperature
              tlimTargetReg <- tlimTarget %>%
                filter(.data[["region"]] == reg,
                       .data[["variable"]] == typeDD) %>%
                pull("value")

              # define historic and target tlim columns
              colHist   <- paste(typeDD, tlimHist[[typeDD]], sep = "_")
              colTarget <- paste(typeDD, tlimTargetReg,      sep = "_")


              tmp <- hddcdd %>%
                # filter data
                filter(.data[["variable"]] == typeDD,
                       .data[["scenario"]] == scen,
                       .data[["region"]] == reg)

              # fill historic data with SSP2 data if necessary
              # if (scen %in% scensToFill) {
              #   tmp <- tmp %>%
              #     filter(!(.data[["period"]] %in% seq(endOfDDHistory, endOfHistory))) %>%
              #     rbind(hddcdd %>%
              #             filter(.data[["scenario"]] == scenFillHist,
              #                    .data[["period"]] %in% seq(endOfDDHistory, endOfHistory)) %>%
              #             mutate(scenario = scen))
              # }

              tmp <- tmp %>%
                # set temperature boundaries
                filter(.data[["tlim"]] %in% c(tlimHist, tlimTargetReg)) %>%

                # unite "tlim" and "variable" column
                unite(col = "variable", "variable", "tlim", sep = "_") %>%

                # allocate separate columns for tlimHist and tlimTarget
                pivot_wider(names_from = "variable", values_from = "value") %>%

                # join convergence shares
                left_join(lambda %>%
                            filter(.data[["variable"]] == typeDD),
                          by = c("region", "period", "scenario")) %>%

                # transition data from tlimHist to tlimTarget
                mutate(value = .data[[colTarget]] * .data[["fullconv"]] +
                         .data[[colHist]] * (1 - .data[["fullconv"]])) %>%

                # filter non-relevant periods
                filter(!is.na(.data[["value"]])) %>%

                # prepare for output
                mutate(variable = typeDD,
                       scenario = scenConfig) %>%
                dplyr::select("region", "period", "variable", "scenario", "value")

              return(tmp)
            }
          )
        )
      }
    )
  )

  return(data)
}
