#' Build full EDGE-B regional mapping
#'
#' @importFrom utils read.csv2
#' @importFrom dplyr left_join

buildRegionalMapping <- function() {
  # mapping paths

  edgeMap    <- piamutils::getSystemFile("data_internal/mappings/regionmappingEDGE.csv", package = "edgebuildings")
  ieaCommMap <- piamutils::getSystemFile("data_internal/mappings/commercial_floorspace_regmap.csv", package = "edgebuildings")
  pfuMap     <- piamutils::getSystemFile("data_internal/mappings/pfu_regionmapping.csv", package = "edgebuildings")

  # read-in maps
  mappingEDGE    <- read.csv2(edgeMap, stringsAsFactors = FALSE)
  mappingIEAcomm <- read.csv2(ieaCommMap, stringsAsFactors = FALSE)
  mappingPFU     <- read.csv2(pfuMap, stringsAsFactors = FALSE)

  names(mappingEDGE) <- c("countryname", "iso", "EDGE_all", "EDGE_allEUR", "EDGE_EUR_ETP")

  # join maps
  mapping <- mappingEDGE %>%
    left_join(mappingIEAcomm, by = "EDGE_all") %>%
    left_join(mappingPFU, by = "iso")

  return(mapping)
}
