#' Crop raster variables.
#'
#' This function load bioclim variables from CHELSA 2.1 and crops them to a provided extent.
#'
#' @param input_dir Folder on local machine containing the raster layers downloaded from CHELSA. Required
#' @param output_dir Folder on local machine to store the croped raster layers. Default to same input folder
#' @param ext vector c(xmin, xmax, ymin, ymax). Required
#' @param file_format File extension. Default ".tif"
#' @param suffix Suffix to file name. Default "_crop"
#'
#' @author Gustavo A. Silva-Arias \email{gasilvaa@unal.edu.co}
# #' @export

#' @examples

#' chelsa_layers <- "~/bio_CHELSA"
#' chelsa_layers_cropped <- "~/bio_CHELSA_crop"
#' ext <- c(-80, -70, -4, 1)
#' crop_chelsa(chelsa_layers, chelsa_layers_cropped, ext)

crop_chelsa = function(input_dir, output_dir=input_dir, ext, file_format=".tif", suffix="_crop")
{
  layers_list <- list.files(input_dir, file_format, full.names = F)
  for(i in 1:length(layers_list)){
    temp <- raster(paste0(input_dir, layers_list[[i]]))
    temp <- crop(temp,ext)
    name <- str_replace_all(layers_list[[i]], c("CHELSA_" = "", "_1981-2010" = "", "_V.2.1.tif" = ""))
    writeRaster(temp, filename=paste0(output_dir, name, suffix, file_format), overwrite=T)
    print(paste0(name,' DONE'))
    rm(temp)
  }
}

require(raster)
require(stringr)
