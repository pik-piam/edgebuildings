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
#'
#' @param hddcdd data.frame with degree days per year
#' @param config data.frame with configuration parameters for specific scenario
#' @param regionmap region mapping
#'
#' @return converged degree day data
#'
#' @author Hagen Tockhorn
#'
#' @importFrom dplyr rename mutate left_join filter rename select
#' @importFrom tidyr separate unite pivot_wider
#' @importFrom quitte getRegs


prepHDDCDD <- function(hddcdd, config, regionmap) {


  # READ-IN DATA ---------------------------------------------------------------

  #--- extract parameters from config file

  scenConfig <- row.names(config) %>% unique()

  # upper temporal boundary of historic data
  endOfHistory <- config[["endOfHistory"]] %>% unlist()


  # scenarios
  socioScen <- config[["hddcddScen"]] %>% unlist()
  climScen  <- config[["rcpScen"]] %>% unlist()

  socioClimScen <- paste(socioScen, climScen, sep = "_")


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
    mutate(scenario = socioClimScen)

  yearTargetCDD <- config[["speed_CDD"]] %>%
    buildScenInput(subtype = "mapping",
                   regionmap = regionmap) %>%
    rename("fullconv" = "value") %>%
    mutate(scenario = socioClimScen)


  # PROCESS DATA ---------------------------------------------------------------

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


  # process regional data subsets
  hddcdd <- do.call(rbind, lapply(c("HDD", "CDD"), function(typeDD) {
    do.call(rbind, lapply(getRegs(hddcdd), function(reg) {

      # regional limit temperature
      tlimTargetReg <- tlimTarget %>%
        filter(.data[["region"]] == reg,
               .data[["variable"]] == typeDD) %>%
        pull("value")

      # historic and target tlim columns <variable_tlim>
      colHist   <- paste(typeDD, tlimHist[[typeDD]], sep = "_")
      colTarget <- paste(typeDD, tlimTargetReg,      sep = "_")


      hddcdd %>%
        # filter relevant subset
        filter(.data$region   ==   reg,
               .data$ssp      ==   socioScen,
               .data$variable ==   typeDD,
               .data$rcp      %in% c("historical", climScen),
               .data$tlim     %in% c(tlimHist, tlimTargetReg),
               !is.na(.data$value)) %>%

        # unite "historical" and ...
        mutate(rcp = climScen) %>%

        # unite scenario and variable cols
        unite(col = "scenario", c("ssp", "rcp"), sep = "_") %>%
        unite(col = "variable", c("variable", "tlim"), sep = "_") %>%
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
        select("region", "period", "variable", "scenario", "value")
    }))
  }))

  return(hddcdd)
}
