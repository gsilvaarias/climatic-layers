#' Download CHELSA V_2.1 BIO dataset
#'
#' This function downloads CHELSA V_2.1 BIO climate layers (Karger et al., 2018) into a specified folder.
#' Adapted from Simon Kapitza's chelsa_get.R https://rdrr.io/github/kapitzas/WorldClimTiles/src/R/chelsa_get.R

#' @importFrom utils download.file

#' @param target_path Download folder on local machine. Defaults to working directory
#' @param vars Character string containing the CHELSA variables to be downloaded, can be one of: "bio", "clt", "fcf", "gd", "gs", "hurs", "kg", "ngd", "npp", "pet", "rsds", "snow", "sfcWind", "vpd". Default "all".
#'
#' @author Gustavo A. Silva-Arias \email{gasilvaa@unal.edu.co}
#' @export
#' @examples

#' vars <- c("bio")
#' target_path <- "~/chelsa_bio"
#' chelsa_get(target_path, vars)
#'
#' @references
#' Karger D.N., Conrad, O., Böhner, J., Kawohl, T., Kreft, H., Soria-Auza, R.W., Zimmermann, N.E, Linder, H.P., Kessler, M. (2018): Data from: Climatologies at high resolution for the earth’s land surface areas. EnviDat. https://doi.org/10.16904/envidat.228.v2.1

download_chelsa = function(target_path=getwd(), vars="") 
{
  timeout_old <- getOption("timeout")
  options(timeout = 1000)
  ## variables sets
  if(vars=="bio") {
    vars=c("bio10",
           "bio11",
           "bio12",
           "bio13",
           "bio14",
           "bio15",
           "bio16",
           "bio17",
           "bio18",
           "bio19",
           "bio1",
           "bio2",
           "bio3",
           "bio4",
           "bio5",
           "bio6",
           "bio7",
           "bio8",
           "bio9")
  } else if(vars=="clt") {
    vars==c("clt_max",
            "clt_mean",
            "clt_min",
            "clt_range",
            "cmi_max",
            "cmi_mean",
            "cmi_min",
            "cmi_range")
  } else if(vars=="fcf") {
    vars==c("fcf")
  } else if(vars=="gd") {
    vars=c("gdd0",
           "gdd10",
           "gdd5",
           "gddlgd0",
           "gddlgd10",
           "gddlgd5",
           "gdgfgd0",
           "gdgfgd10",
           "gdgfgd5")
  } else if(vars=="gs") {
    vars=c("fgd",
           "gsl",
           "gsp",
           "gst",
           "lgd")
  } else if(vars=="hurs") {
    vars=c("hurs_max",
           "hurs_mean",
           "hurs_min",
           "hurs_range")
  } else if(vars=="kg") {
    vars=c("kg0",
           "kg1",
           "kg2",
           "kg3",
           "kg4",
           "kg5")
  } else if(vars=="ngd") {
    vars=c("ngd0",
           "ngd10",
           "ngd5")
  } else if(vars=="npp") {
    vars=c("npp")
  } else if(vars=="pet") {
    vars=c("pet_penman_max",
           "pet_penman_mean",
           "pet_penman_min",
           "pet_penman_range")
  } else if(vars=="rsds") {
    vars=c("rsds_1981-2010_max",
           "rsds_1981-2010_mean",
           "rsds_1981-2010_min",
           "rsds_1981-2010_range")
  } else if(vars=="snow") {
    vars=c("scd",
           "swe")
  } else if(vars=="sfcWind") {
    vars=c("sfcWind_max",
           "sfcWind_mean",
           "sfcWind_min",
           "sfcWind_range")
  } else if(vars=="vpd") {
    vars=c("vpd_max",
           "vpd_mean",
           "vpd_min",
           "vpd_range")
  } else {
    vars=c("bio10",
           "bio11",
           "bio12",
           "bio13",
           "bio14",
           "bio15",
           "bio16",
           "bio17",
           "bio18",
           "bio19",
           "bio1",
           "bio2",
           "bio3",
           "bio4",
           "bio5",
           "bio6",
           "bio7",
           "bio8",
           "bio9",
           "clt_max",
           "clt_mean",
           "clt_min",
           "clt_range",
           "cmi_max",
           "cmi_mean",
           "cmi_min",
           "cmi_range",
           "fcf",
           "fgd",
           "gdd0",
           "gdd10",
           "gdd5",
           "gddlgd0",
           "gddlgd10",
           "gddlgd5",
           "gdgfgd0",
           "gdgfgd10",
           "gdgfgd5",
           "gsl",
           "gsp",
           "gst",
           "hurs_max",
           "hurs_mean",
           "hurs_min",
           "hurs_range",
           "kg0",
           "kg1",
           "kg2",
           "kg3",
           "kg4",
           "kg5",
           "lgd",
           "ngd0",
           "ngd10",
           "ngd5",
           "npp",
           "pet_penman_max",
           "pet_penman_mean",
           "pet_penman_min",
           "pet_penman_range",
           "rsds_1981-2010_max",
           "rsds_1981-2010_mean",
           "rsds_1981-2010_min",
           "rsds_1981-2010_range",
           "scd",
           "sfcWind_max",
           "sfcWind_mean",
           "sfcWind_min",
           "sfcWind_range",
           "swe",
           "vpd_max",
           "vpd_mean",
           "vpd_min",
           "vpd_range")
  }

  for (var in vars) {
    ## fix name issue for rsds variables
    if(grepl("rsds", var)) {
      name <- paste(c("CHELSA", var, "V.2.1.tif"), collapse = "_")
    } else {
      name <- paste(c("CHELSA", var, "1981-2010", "V.2.1.tif"), collapse = "_")
    }
    source_url <- file.path("https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/1981-2010/bio", 
                            name)
    destination <- file.path(target_path, name)
    if (!file.exists(destination)) {
      out <- NULL
      out <- tryCatch(download.file(source_url, destination), 
                      error = function(e) {
                        return(NA)
                      })
      if (is.na(out)) {
        next
      }
    }
    else {
      message(paste0(destination, " already downloaded, skipping to next"))
    }
  }
  options(timeout = timeout_old)
}
