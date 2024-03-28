library(raster)
library(rgdal)
library(dismo)
library(biomod2)
library(ecospat)
library(fuzzySim)

# env
b <- list.files("/data/home/users/g.silvaarias/clim_vars","tif",full.names = T)
b <- stack(b)

### species occurrences
egla <- read.table("eglandulata.txt", h=T)
coordinates(egla) <- c("long","lat")
proj4string(egla) <- CRS("+proj=longlat +datum=WGS84")

# ###### eliminate duplicates #############
egla_dedup <- gridSample(egla, b[[1]], n=1)
summary(egla_dedup)

###### set geographical extent based on altitude to sample pseudo-abcesnces
elevation <- raster("/data/home/users/g.silvaarias/rasters_srtm/srtm.tif")
plot(elevation)
points(egla_dedup)
dev.off()

#### pseudo-absence points withn a radius of presence points #######
egla_coords <- BIOMOD_FormatingData(resp.var=as.numeric(rep(1,nrow(egla_dedup))),
                                    expl.var=b[[1]],
                                    resp.xy=as.matrix(egla_dedup),
                                    resp.name='egla',
                                    PA.nb.rep=1,
                                    PA.nb.absences=10000,
                                    PA.strategy='disk',
                                    PA.dist.min=5000, PA.dist.max=100000,
                                    na.rm=TRUE)

pdf("egla_presence-pseudoabsence.pdf")
plot(elevation, main='C. eglandulata')#, xlim=c(min(egla_dedup[,1])-2, max(egla_dedup[,1])+2), ylim=c(min(egla_dedup[,2])-2, max(egla_dedup[,2])+2))
points(egla_coords@coord[-c(1:nrow(egla_dedup)),], cex=0.3)
points(egla_coords@coord[1:nrow(egla_dedup),], pch=19, col='red', cex=0.5)
dev.off()

######## extract climatic values 
bio_egla <- extract(b, egla_coords@coord)
bio_egla[rowSums(is.na(bio_egla)) > 0,] ## check for missing data
which(is.na(bio_egla), arr.ind=TRUE) ## check for missing data 

pres <- c(rep(1,nrow(egla_dedup)),rep(0,10000))
bio_egla <- cbind(pres,bio_egla)
colnames(bio_egla)
bio_egla <- as.data.frame(bio_egla)

 ############### variable selection ########################
pdf("variable_correlation.pdf", width = 30, height = 30)
ecospat.cor.plot(bio_egla)
dev.off()

bio_egla <- as.data.frame(bio_egla)
paste(names(bio_egla), collapse = " + ")
selected <- corSelect(bio_egla, sp.cols = 1, var.cols = c(2:ncol(bio_egla)), cor.thresh = 0.75)  
selected
paste(selected$selected.vars, collapse = ",")
paste(selected$selected.var.cols, collapse = ",")
####
pdf("variable_correlation_selected.pdf", width = 30, height = 30)
ecospat.cor.plot(bio_egla[,c(1,selected$selected.var.cols)])
dev.off()

## BIOMOD
b <- b[[selected$selected.var.cols-1]]
print("############### selected variables ###############")
b
print("##################################################")

eglaBiomodData <- BIOMOD_FormatingData(resp.var = as.numeric(rep(1,nrow(egla_dedup))),
                                      expl.var = b,
                                      resp.xy = as.matrix(egla_dedup),
                                      resp.name = 'egla',
                                      PA.nb.rep=1,
                                      PA.nb.absences=10000, 
                                      PA.strategy='disk',
                                      PA.dist.min=5000, PA.dist.max=100000,
                                      na.rm=TRUE)

eglaBiomodOption <- Print_Default_ModelingOptions()

eglaBiomodOption@GLM$test = 'none'
eglaBiomodOption@GLM$maxit = 100

eglaBiomodOption@MAXENT.Phillips$betamultiplier = 2.5
eglaBiomodOption@MAXENT.Phillips$product = F
eglaBiomodOption@MAXENT.Phillips$threshold = F
eglaBiomodOption@MAXENT.Phillips$hinge = F
eglaBiomodOption@MAXENT.Phillips$path_to_maxent.jar = '/data/home/users/g.silvaarias/local/bin'

### Calibration of simple bivariate models
egla.ESM <- ecospat.ESM.Modeling( data=eglaBiomodData,
                                models=c('GLM','CTA','ANN','GAM','MAXENT.Phillips'),
                                models.options=eglaBiomodOption,
                                NbRunEval=10,
                                DataSplit=70,
                                weighting.score=c("TSS"),
                                parallel=T,
                                Prevalence=0.5)
save.image()

### Evaluation and average of simple bivariate models to ESMs
egla.ESM_EF <- ecospat.ESM.EnsembleModeling(egla.ESM, weighting.score=c("TSS"), threshold=0.7)

## write the model performance of ESMs to file
write.table(egla.ESM_EF$ESM.evaluations, "egla_ESM_EF_evaluations.txt", sep='\t', col.names=T, row.names=F, quote=F)
## get the weights of the single bivariate models used to build the ESMs
print('############ Weights of the single bivariate models used to build the ESMs ############')
egla.ESM_EF$weights
print('#######################################################################################'); print('')
## get the variable contributions of ESMs
print('############################### Variable contributions of ESMs ########################')
ecospat.ESM.VarContrib(egla.ESM, egla.ESM_EF, scaling = "01")
print('#######################################################################################'); print('')

# ### Projection of simple bivariate models into new space
egla.ESM_proj_current <- ecospat.ESM.Projection(ESM.modeling.output=egla.ESM, new.env=b, parallel=T)
save.image()

### Projection of calibrated ESMs into new space
egla.ESM_EFproj_current <- ecospat.ESM.EnsembleProjection(ESM.prediction.output=egla.ESM_proj_current, ESM.EnsembleModeling.output=egla.ESM_EF)
writeRaster(egla.ESM_EFproj_current, 'egla_ESM_current.tif', bylayer=T, suffix=c('GLM','CTA','ANN','GAM','MAXENT.Phillips','EF'))
egla.ESM_EFproj_current

thres <- mean(subset(egla.ESM_EF$ESM.evaluations, egla.ESM_EF$ESM.evaluations$technique=='EF')$threshold)
print('threshold')
thres

save.image()



#######################################################
############## LAST GLACIAL MAXIMUM ###################
print('##project to lgm_MPI_ESM')
variables_lgm <- list.files(path="/data/home/users/g.silvaarias/clim_vars/mpi_esm_lgm", pattern='tif', full.names=TRUE)
variables_lgm <- stack(variables_lgm)
variables_lgm <- variables_lgm[[selected$selected.var.cols-1]]
egla.ESM_proj_mpi_LGM <- ecospat.ESM.Projection(ESM.modeling.output=egla.ESM, new.env=variables_lgm)
egla.ESM_EFproj_mpi_LGM <- ecospat.ESM.EnsembleProjection(ESM.prediction.output=egla.ESM_proj_mpi_LGM, ESM.EnsembleModeling.output=egla.ESM_EF)
writeRaster(egla.ESM_EFproj_mpi_LGM, 'egla_ESM_mpi_LGM.tif', bylayer=T, suffix=c('GLM','CTA','ANN','GAM','MAXENT.Phillips','EF'))
egla_ESM_mpi_LGM_bin <- writeRaster(BinaryTransformation(egla.ESM_EFproj_mpi_LGM$EF, thres), "egla_ESM_mpi_LGM_bin.tif")
rm(variables_lgm); save.image()

print('##project to lgm_MIROC')
variables_lgm <- list.files(path="/data/home/users/g.silvaarias/clim_vars/miroc_lgm", pattern='tif', full.names=TRUE)
variables_lgm <- stack(variables_lgm)
variables_lgm <- variables_lgm[[selected$selected.var.cols-1]]
egla.ESM_proj_miroc_LGM <- ecospat.ESM.Projection(ESM.modeling.output=egla.ESM, new.env=variables_lgm)
egla.ESM_EFproj_miroc_LGM <- ecospat.ESM.EnsembleProjection(ESM.prediction.output=egla.ESM_proj_miroc_LGM, ESM.EnsembleModeling.output=egla.ESM_EF)
writeRaster(egla.ESM_EFproj_miroc_LGM, 'egla_ESM_miroc_LGM.tif', bylayer=T, suffix=c('GLM','CTA','ANN','GAM','MAXENT.Phillips','EF'))
egla_ESM_miroc_LGM_bin <- writeRaster(BinaryTransformation(egla.ESM_EFproj_miroc_LGM$EF, thres), "egla_ESM_miroc_LGM_bin.tif")
rm(variables_lgm); save.image()

print('##project to lgm_CCSM4')
variables_lgm <- list.files(path="/data/home/users/g.silvaarias/clim_vars/ccsm4_lgm", pattern='tif', full.names=TRUE)
variables_lgm <- stack(variables_lgm)
variables_lgm <- variables_lgm[[selected$selected.var.cols-1]]
egla.ESM_proj_ccsm4_LGM <- ecospat.ESM.Projection(ESM.modeling.output=egla.ESM, new.env=variables_lgm)
egla.ESM_EFproj_ccsm4_LGM <- ecospat.ESM.EnsembleProjection(ESM.prediction.output=egla.ESM_proj_ccsm4_LGM, ESM.EnsembleModeling.output=egla.ESM_EF)
writeRaster(egla.ESM_EFproj_ccsm4_LGM, 'egla_ESM_ccsm4_LGM.tif', bylayer=T, suffix=c('GLM','CTA','ANN','GAM','MAXENT.Phillips','EF'))
egla_ESM_ccsm4_LGM_bin <- writeRaster(BinaryTransformation(egla.ESM_EFproj_ccsm4_LGM$EF, thres), "egla_ESM_ccsm4_LGM_bin.tif")
rm(variables_lgm); save.image()



######################################
###### plot model projections ########

pdf("egla_model_projections.pdf")
##LGM
plot(egla_ESM_ccsm4_LGM_bin + egla_ESM_miroc_LGM_bin + egla_ESM_mpi_LGM_bin, main='P. eglandulata - LGM projection',
  xlim=c(min(egla_dedup[,1])-2, max(egla_dedup[,1])+2), ylim=c(min(egla_dedup[,2])-2, max(egla_dedup[,2])+2), xlab = "Longitude", ylab = "Latitude")
dev.off()
