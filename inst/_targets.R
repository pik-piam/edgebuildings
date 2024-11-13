#' full EDGE-B processing pipeline
#' @author Hagen Tockhorn, Robin Hasse

library(targets)

# load EDGE-B either from installation or development version
# TODO: there has to be a better way of doing this
edgebVer <- Sys.getenv("edgebVer")
if (edgebVer == "0") {
  library(edgebuildings)
} else {
  devtools::load_all(edgebVer)
}

tar_option_set(envir = getNamespace("edgebuildings"))
workingDir <- piamutils::getSystemFile(package = "edgebuildings")


mrData <- piamutils::getSystemFile("input", package = "edgebuildings")
output <- piamutils::getSystemFile("output", package = "edgebuildings")


list(
  #--- FILES -------------------------------------------------------------------

  # Config----------------------------------------------------------------------

  tar_target(
    config.csv,
    piamutils::getSystemFile("start", "config.csv", package = "edgebuildings"),
    format = "file"
  ),

  tar_target(
    default.csv,
    piamutils::getSystemFile("config/default.csv", package = "edgebuildings"),
    format = "file"
  ),

  tar_target(
    region_groups.csv,
    piamutils::getSystemFile("config/region_groups.csv", package = "edgebuildings"),
    format = "file"
  ),

  # Mappings--------------------------------------------------------------------

  # iea mapping
  tar_target(
    regionmappingIEA.csv,
    piamutils::getSystemFile("data_internal/mappings/regionmapping_iea_floor.csv", package = "edgebuildings"),
    format = "file"
  ),

  # fe -> ue efficiency corrections
  tar_target(
    correct_efficiencies.csv,
    piamutils::getSystemFile("data_internal/mappings/correct_efficiencies.csv", package = "edgebuildings"),
    format = "file"
  ),

  # mredgebuildings-----------------------------

  # surface area
  tar_target(
    surface.cs4r,
    file.path(mrData, "f_surface.cs4r"),
    format = "file"
  ),

  # floor
  tar_target(
    floorspacePast.cs4r,
    file.path(mrData, "f_floorspace.cs4r"),
    format = "file"
  ),

  # pfu
  tar_target(
    pfu.cs4r,
    file.path(mrData, "f_feue.cs4r"),
    format = "file"
  ),

  # hddcdd
  tar_target(
    hddcdd.cs4r,
    file.path(mrData, "f_hddcdd.cs4r"),
    format = "file"
  ),

  # fe->ue efficiencies
  tar_target(
    feueEff.cs4r,
    file.path(mrData, "f_feue_efficiencies.cs4r"),
    format = "file"
  ),

  # fe->ue efficiency regression parameters
  tar_target(
    feueEffPars.cs4r,
    file.path(mrData, "f_feue_efficiencyPars.cs4r"),
    format = "file"
  ),


  # mrdrivers------------------------------

  # population
  tar_target(
    pop.cs4r,
    file.path(mrData, "f_pop.cs4r"),
    format = "file"
  ),

  # gdp
  tar_target(
    gdp.cs4r,
    file.path(mrData, "f_gdp.cs4r"),
    format = "file"
  ),

  # urban share
  tar_target(
    urbanshare.cs4r,
    file.path(mrData, "f_urban.cs4r"),
    format = "file"
  ),

  # uvalues
  tar_target(
    uvaluesResCom.cs4r,
    file.path(mrData, "f_uvalues_rescom.cs4r"),
    format = "file"
  ),

  tar_target(
    uvaluesETSAP.cs4r,
    file.path(mrData, "f_uvalues_etsap.cs4r"),
    format = "file"
  ),

  tar_target(
    floor0.cs4r,
    file.path(mrData, "f_floorspace_tcep.cs4r"),
    format = "file"
  ),

  #--- LOAD DATA ---------------------------------------------------------------

  # Config----------------------------------------------------------------------

  tar_target(
    config,
    {
      readConfig(config = config.csv,
                 default = default.csv,
                 regiongroups = region_groups.csv,
                 subtype = "scenario")
    }
  ),

  # Mappings--------------------------------------------------------------------

  # general edge mappings
  tar_target(
    regionmap,
    {
      buildRegionalMapping()
    }
  ),

  # iea mapping
  tar_target(
    regionmapIEA,
    read.csv2(regionmappingIEA.csv, stringsAsFactors = FALSE)
  ),

  # Scenario Assumptions--------------------------------------------------------

  # scenario assumptions
  tar_target(
    scenAssump,
    {
      buildScenInput(config,
                     subtype = "scen_assump",
                     regionmap = regionmap)
    },
    pattern = map(config),
    iteration = "vector"
  ),

  # scenario regression end
  tar_target(
    scenAssumpSpeed,
    {
      buildScenInput(config,
                     subtype = "scen_assump_speed",
                     regionmap = regionmap,
                     regionalTargetDimension = "EDGE_EUR_ETP")
    },
    pattern = map(config),
    iteration = "vector"
  ),

  # fe share scenario assumptions
  tar_target(
    scenAssumpFEshares,
    {
      buildScenInput(config,
                     subtype = "fe_shares",
                     regionmap = regionmap,
                     regionalTargetDimension = "EDGE_EUR_ETP")
    },
    pattern = map(config),
    iteration = "vector"
  ),

  # efficiency corrections
  tar_target(
    scenAssumpCorrect,
    read.csv(correct_efficiencies.csv, stringsAsFactors = F)
  ),


  # Process Files---------------------------------------------------------------

  # mredgebuildings-----------------------------

  # surface area
  tar_target(
    surface,
    {
      cols <- c("period", "region", "variable", "value")
      file <- surface.cs4r

      read.csv2(file, skip = skiprow(file), sep = ",", col.names = cols, header = F) %>%
        mutate(value = as.numeric(.data[["value"]])) %>%
        dplyr::select(-"period") %>%
        mutate(variable = "surface")
    }),

  # floor
  tar_target(
    floorspacePast,
    {
      cols <- c("period", "region", "value")
      file <- floorspacePast.cs4r

      read.csv2(file, skip = skiprow(file), sep = ",", col.names = cols, header = F) %>%
        mutate(value = as.numeric(.data[["value"]]),
               variable = "m2cap",
               scenario = "history")
    }
  ),

  # pfu
  tar_target(
    pfu,
    {
      cols <- c("period", "region", "scenario", "unit", "enduse", "carrier", "value")
      file <- pfu.cs4r

      read.csv2(file, skip = skiprow(file), sep = ",", col.names = cols, header = F) %>%
        mutate(value = as.numeric(.data[["value"]]),
               scenario = "history") %>%
        as.quitte()
    }
  ),

  # fe->ue efficiencies
  tar_target(
    feueEffHist,
    {
      cols <- c("period", "region", "scenario" ,"carrier", "enduse", "value")
      file <- feueEff.cs4r

      read.csv(file, header = FALSE, comment.char = "*", col.names = cols) %>%
        as.quitte()
    }
  ),

  # fe->ue efficiency regression parameters
  tar_target(
    feueEffPars,
    {
      cols <- c("carrier", "enduse", "variable", "value")
      file <- feueEffPars.cs4r

      read.csv2(file, skip = skiprow(file), sep = ",", header = F, col.names = cols) %>%
        mutate(value = as.numeric(.data[["value"]])) %>%
        pivot_wider(names_from  = "variable",
                    values_from = "value")
    }
  ),

  # heating/cooling degree days
  tar_target(
    hddcdd,
    {
      read.csv(hddcdd.cs4r, header = FALSE, comment.char = "*",
               col.names = c("period", "region", "scenario", "variable", "value")) %>%

        # converge limit temperatures
        prepHDDCDD(config, regionmap)
    },
    pattern = map(config),
    iteration = "vector"
  ),

  # uvalues
  tar_target(
    uvaluesResCom,
    {
      cols <- c("period", "region", "variable", "value")
      file <- uvaluesResCom.cs4r
      read.csv2(file, skip = skiprow(file), sep = ",", col.names = cols, header = F) %>%
        mutate(value = as.numeric(.data[["value"]]))
    }
  ),

  tar_target(
    uvaluesETSAP,
    {
      cols <- c("region", "variable", "value")
      file <- uvaluesETSAP.cs4r
      read.csv2(file, skip = skiprow(file), sep = ",", col.names = cols, header = F) %>%
        mutate(value = as.numeric(.data[["value"]]))
    }
  ),

  # floor0
  tar_target(
    floor0,
    read.csv(floor0.cs4r, header = FALSE, comment.char = "*",
             col.names = c("period", "region", "variable", "unit", "value"))
  ),


  # mrdrivers------------------------------

  # population
  tar_target(
    pop,
    {
      cols <- c("period", "region", "variable", "value")
      file <- pop.cs4r

      read.csv2(file, skip = skiprow(file), sep = ",", col.names = cols) %>%
        mutate(value = as.numeric(.data[["value"]]))
    }),

  # gdp
  tar_target(
    gdpInput,
    {
      cols <- c("period", "region", "variable", "value")
      file <- gdp.cs4r

      read.csv2(file, skip = skiprow(file), sep = ",", col.names = cols) %>%
        mutate(value = as.numeric(.data[["value"]]))
    }),

  # population density
  tar_target(
    density,
    {

      getDensity(pop = pop %>%
                   filter(variable == config[, "popScen"]) %>%
                   sepVarScen(),
                 surface = surface)
    },
    pattern = map(config),
    iteration = "vector"
  ),

  # urban share
  tar_target(
    urbanshare,
    {
      cols <- c("period", "region", "variable", "value")
      file <- urbanshare.cs4r

      read.csv2(file, skip = skiprow(file), sep = ",", col.names = cols) %>%
        mutate(value = as.numeric(.data[["value"]]))
    }
  ),

  # EDGE-B--------------------------------

  # gdp
  tar_target(
    gdp,
    {
      getGDP(config = config,
             gdp = gdpInput,
             regionmap = regionmap)
    },
    pattern = map(config),
    iteration = "vector"
  ),

  # gdppop
  tar_target(
    gdppop,{
      getGDPpop(pop = pop %>%
                  filter(variable == config[, "popScen"]),
                gdp = gdp %>%
                  filter(variable == config[, "gdpScen"]))
    },
    pattern = map(config),
    iteration = "vector"
  ),


  #--- EDGE-B PROCESSING -------------------------------------------------------

  # Floorspace------------------------------

  # share of commercial buildings (EDGE)
  tar_target(
    shareFloorCommercialEDGE,
    {
      getShareFloorCommercial(config = config,
                              subtype = "EDGE",
                              gdppop = gdppop,
                              pop = pop,
                              floor0 = floor0,
                              regionalmap = regionmap)},
    pattern = map(config),
    iteration = "vector"
  ),


  # share of commercial buildings (IEA)
  tar_target(
    shareFloorCommercialIEA,
    {
      getShareFloorCommercial(config = config,
                              subtype = "IEA",
                              gdppop = gdppop,
                              pop = pop,
                              floor0 = floor0,
                              regionalmap = regionmap)},
    pattern = map(config),
    iteration = "vector"
  ),


  # absolute and per capita floorspace of residential buildings
  tar_target(
    floorspaceResidential,
    {
      getFloorspaceResidential(config = config,
                               floorspacePast = floorspacePast,
                               gdppop = gdppop,
                               density = density,
                               pop = pop,
                               surface = surface,
                               regionmap = regionmap,
                               scenAssump = scenAssump,
                               scenAssumpSpeed = scenAssumpSpeed)
    },
    pattern = map(config),
    iteration = "vector"
  ),

  # absolute and per capita floorspace
  tar_target(
    floorspaceBuild,
    {
      getFloorspaceBuild(config = config,
                         resid = floorspaceResidential,
                         comShares = shareFloorCommercialEDGE,
                         comSharesIEA = shareFloorCommercialIEA,
                         regionmappingIEA = regionmapIEA,
                         regionmapping = regionmap,
                         scenAssumpSpeed = scenAssumpSpeed)
    },
    pattern = map(config),
    iteration = "vector"
  ),


  # EC Share Projections--------------------

  # projected energy carrier shares
  tar_target(
    feSharesEC,
    {
      getShareECprojections(config = config,
                            pfu = pfu,
                            gdp = gdp,
                            gdppop = gdppop,
                            scenAssumpFEShares = scenAssumpFEshares,
                            regionalmap = regionmap)
    },
    pattern = map(config),
    iteration = "vector"
  ),


  # U Values--------------------------------

  tar_target(
    uvalue,
    {
      getUvalues(config = config,
                 uvaluesResCom = uvaluesResCom,
                 uvaluesETSAP = uvaluesETSAP,
                 gdppop = gdppop,
                 hddcdd = hddcdd,
                 pop = pop,
                 regionalmap = regionmap,
                 scenAssumpSpeed = scenAssumpSpeed)
    },
    pattern = map(config),
    iteration = "vector"
  ),


  # FE-EU-Efficiencies----------------------

  # TODO: handle missing parameter "endOfHistory"
  tar_target(
    feueEfficiencies,
    {
      getFEUEefficiencies(config = config,
                          eff = feueEffHist,
                          gdppop = gdppop,
                          scenAssump = scenAssump,
                          scenAssumpSpeed = scenAssumpSpeed,
                          regPars = feueEffPars)
    },
    pattern = map(config),
    iteration = "vector"
  ),


  #--- PROJECTIONS -------------------------------------------------------------

  tar_target(
    projections,
    {
      buildingsProjections(config = config,
                           floor = floorspaceBuild,
                           hddcdd = hddcdd,
                           pop = pop,
                           gdppop = gdppop,
                           uvalue = uvalue,
                           pfu = pfu,
                           feueEff = feueEfficiencies,
                           feSharesEC = feSharesEC,
                           regionmap = regionmap,
                           scenAssump = scenAssump,
                           scenAssumpSpeed = scenAssumpSpeed,
                           scenAssumpCorrect = scenAssumpCorrect,
                           outputDir = output
      )
    },
    pattern = map(config),
    iteration = "vector"
  )
)
