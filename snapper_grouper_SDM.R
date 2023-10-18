
library(ggplot2)
library(sf)
library(dplyr)
library(raster)
library(cowplot)

#theme_set(theme_bw())
world <- map_data('world')

#data <- read.csv("C:\\Users\\jcao22\\Google Drive\\Research_NCSU\\Snapper_grouper_SDM\\AllSpeciesSumcounts2011-2018.csv")
#data <- read.csv("/Users/jcao22/Google Drive/Research_NCSU/Snapper_grouper_SDM/AllSpeciesSumcounts2011-2018.csv")
#spp.all.names <- colnames(data)[47:132]

#Den_spp <- cbind(data[1:46],stack(data[47:132]))

### updated dataset
data <- read.csv("/Users/jcao22/Google Drive/Research_NCSU/Snapper_grouper_SDM/video.dataset.2011.2021.subset.csv")
data <- read.csv("/Users/jcao22/My Drive/Research_NCSU/Snapper_grouper_SDM/video.dataset.2011.2021.subset.csv")

spp.all.names <- colnames(data)[47:136]

Den_spp <- cbind(data[1:46],stack(data[47:136]))

#Proportion of zeros for each species/year
yrs <- sort(unique(Den_spp$Year))
spp <- sort(unique(Den_spp$ind))
PropZeros <- matrix(NA,nrow=length(yrs),ncol=length(spp))
for(y in 1:length(yrs)){
  for(s in 1:length(spp)){
    tmp <- dplyr::filter(Den_spp,Year==yrs[y],ind==spp[s])
    PropZeros[y,s] <- nrow(tmp[tmp$values == 0,])/length(tmp$values)
  }
}
PropZeros <- as.data.frame(PropZeros)
colnames(PropZeros) <- spp
PropZeros$Year <- yrs
x <- reshape2::melt(PropZeros, id = "Year")
x$col <- ifelse(x$value == 0 | x$value == 1, "all zeros or none", "OK")
# pdf('zeros.pdf',width=9, height=9)
# print(ggplot(x, aes(x = Year, y = variable)) + geom_point(aes(size = value,col = factor(col))) +
#         theme_bw() + theme(axis.text.x = element_text(angle = -90)))
# dev.off()
spp.no.zero <- spp[-which(spp %in% unique(subset(x,value==1)$variable))]
spp.99 <- spp[which(spp %in% unique(subset(x,value<=0.99)$variable))]

spp.SP.names <- c("Balistes.capriscus","Caulolatilus.microps","Centropristis.striata","Cephalopholis.cruentata",
                  "Epinephelus.adscensionis","Epinephelus.drummondhayi","Epinephelus.itajara","Epinephelus.morio",
                  "Epinephelus.niveatus","Haemulon.plumierii","Lachnolaimus.maximus","Lutjanus.analis",
                  "Lutjanus.campechanus","Lutjanus.griseus","Lutjanus.vivanus","Malacanthus.plumieri",
                  "Mycteroperca.interstitialis","Mycteroperca.microlepis","Mycteroperca.phenax","Ocyurus.chrysurus",
                  "Pagrus.pagrus","Rhomboplites.aurorubens","Seriola.dumerili","Seriola.zonata")

spp.SP.names2 <- spp.SP.names[which(spp.SP.names %in% spp.99)]

#common.names <- c('Gray triggerfish','Blueline tilefish','Black sea bass','Graysby','Rock hind','Red grouper','Snowy grouper',
#                  'White grunt','Hogfish','Mutton snapper','Red snapper','Gray snapper','Sand tilefish','Yellowmouth grouper',
#                  'Gag','Scamp','Yellowtail snapper','Red porgy','Vermilion snapper','Greater Amberjack','Banded rudderfish')

common.names <- c('Gray triggerfish','Blueline tilefish','Black sea bass','Graysby','Rock hind','Goliath grouper','Red grouper','Snowy grouper',
                  'White grunt','Hogfish','Mutton snapper','Red snapper','Gray snapper','Sand tilefish','Yellowmouth grouper',
                  'Gag','Scamp','Yellowtail snapper','Red porgy','Vermilion snapper','Greater Amberjack')

# species <- c('Centropristis.striata','Seriola.dumerili','Rhomboplites.aurorubens',
#              'Mycteroperca.microlepis','Haemulon.plumierii','Balistes.capriscus',
#              'Mycteroperca.phenax','Lutjanus.campechanus','Pagrus.pagrus')
# 
# species_cn <- c('Black Sea Bass','Greater Amberjack','Vermilion Snapper','Gag','White Grunt',
#                 'Grey Triggerfish','Scamp','Red Snapper','Red Porgy')

# Black Sea Bass (Centropristis striata)
# Greater Amberjack (Seriola dumerili)
# Vermilion Snapper (Rhomboplites aurorubens)
# Gag (Mycteroperca microlepis)
# White Grunt (Haemulon plumierii)
# Grey Triggerfish (Balistes capriscus)
# Scamp (Mycteroperca phenax)
# Red Snapper (Lutjanus campechanus)
# Red Porgy (Pagrus pagrus)

data_sub <- cbind(data[,1:46],data[,spp.SP.names2])
# write.csv(data_sub,file='data_used.csv')
Den_spp <- cbind(data_sub[1:46],stack(data_sub[47:67]))
stations <- Den_spp[!duplicated(paste(Den_spp$Year,Den_spp$Start_Latitude,Den_spp$Start_Longitude)),]
aggregate(values ~ ind, Den_spp, sum)

setwd('/Users/jcao22/Google Drive/Research_NCSU/Snapper_grouper_SDM/VAST_2021')
setwd('/Users/jcao22/My Drive/Research_NCSU/Snapper_grouper_SDM/VAST_2021')
setwd('/Users/jcao22/Library/CloudStorage/GoogleDrive-jcao22@ncsu.edu/My Drive/Research_NCSU/Snapper_grouper_SDM/VAST_2021')

pdf('Survey locations by year.pdf',width=12, height=8)
print(ggplot() + geom_polygon(data=world,aes(x=long,y=lat,group=group),colour='black',fill='gray')+
  coord_fixed(xlim = c(-82.5, -75), ylim = c(27, 36), ratio = 1.2)+
  geom_point(data=stations, aes(x=Start_Longitude,y=Start_Latitude),shape="+")+
  facet_wrap(~Year,ncol=4)+ggtitle("Survey locations by year")+theme_bw())
dev.off()

table(stations$Year)

for (s in 1:length(spp.SP.names2)){
  pdf(paste('spatial catches of ',spp.SP.names2[s],'.pdf',sep=''),width=12, height=8)
  plotDF <- Den_spp[Den_spp$ind == spp.SP.names2[s],]
  print(ggplot() + geom_polygon(data=world,aes(x=long,y=lat,group=group),colour='black',fill='gray')+
          coord_fixed(xlim = c(-82.5, -75), ylim = c(27, 36), ratio = 1.2)+
          geom_point(data=plotDF[plotDF$values != 0,],aes(x=Start_Longitude,y=Start_Latitude,size=values),colour='blue',alpha=0.5)+
          scale_size_continuous(limits=range(sqrt(Den_spp$values)))+
          geom_point(data=plotDF[plotDF$values==0,], aes(x=Start_Longitude,y=Start_Latitude),colour='red',shape='+')+
          facet_wrap(~Year,ncol=4)+theme_bw()+ggtitle(paste('spatial catches of ',spp.SP.names2[s],sep='')))
  dev.off()
}

den_mean <- Den_spp %>%
  group_by(ind, Year) %>%
  summarise(Mean = mean(values))

ggplot(Den_spp, aes(x = values,  fill = as.factor(Year))) +
  geom_histogram() + xlim(c(0,70)) + ylim(c(0,100)) +
  geom_vline(data = den_mean, aes(xintercept = Mean), linetype = "dashed") +
  facet_grid(Year ~ ind, scales = "free") + theme_bw()

#####################################   VAST  ###################################################
# Load packages
library(VAST)
#read in extrapolation grids
user_region = readRDS("/Users/jcao22/Google Drive/Research_NCSU/Snapper_grouper_SDM/VAST/user_region.rds")

############### covariate data #############
#The user must supply a data frame covariate_data of covariate values, with columns named Lat, Lon, and Year, as well as values for all covariates as additional named columns. This data frame is then used as a "look-up table", and is matched against variables listed in formula.
#Specifically, for every observation i at location Lat_i[i] and Lon_i[i] in year t_i[t], the nearest Lat-Lon observation in that year is identified in covariate_data, and covariate values in that row of covariate_data are assigned to observation i. Similarly, for every extrapolation-grid cell g at location spatial_list$latlon_g[i,] in each year, the nearest row of covariate_data in that year is used to assign covariate values. make_covariates then formats these covariate values appropriately and returns them.

covariate_data = data.frame(Lat=Den_spp$Start_Latitude,Lon=Den_spp$Start_Longitude,Year=Den_spp$Year,
                            Depth=Den_spp$Start_Depth,Substrate=Den_spp$Substrate)
# cannot have NA in the covariates, so replace NAs (here with mean, which is not good)
covariate_data$Depth[which(is.na(covariate_data$Depth))] = mean(covariate_data$Depth,na.rm=TRUE)
covariate_data$Substrate[which(is.na(covariate_data$Substrate))] =  mean(covariate_data$Substrate,na.rm=TRUE)

############################################
# Make settings
settings = make_settings( n_x = 600, 
                          Region = "user", 
                          purpose = "ordination",
                          use_anisotropy = TRUE,
                          strata.limits = data.frame(STRATA = "All_areas"), 
                          n_categories = 3,
                          fine_scale = FALSE,
                          bias.correct = TRUE)

# Change settings from defaults
settings$RhoConfig[c('Beta1','Beta2')] = c(3,3)
settings$RhoConfig[c('Epsilon1','Epsilon2')] = c(2,5)
settings$ObsModel=c(2,1)
#settings$ObsModel=c(7,0)
settings$Options['Calculate_Range'] = TRUE
settings$Options['Calculate_effective_area'] = TRUE
#settings$FieldConfig[2,1] = 0

X1_formula = ~ Depth + Substrate
X2_formula = ~ Depth + Substrate
 
# Run model
fit = fit_model( settings = settings, 
                 Lat_i = Den_spp[,'Start_Latitude'], 
                 Lon_i = Den_spp[,'Start_Longitude'],
                 t_i = Den_spp[,'Year'], 
                 c_i = as.numeric(Den_spp[,"ind"])-1,
                 b_i = Den_spp[,'values']/41, 
#                 b_i = Den_spp[,'values'], 
                 a_i = rep(1,length(Den_spp[,'values'])),
#                 X1_formula = X1_formula,
#                 X2_formula = X2_formula,
#                 covariate_data = covariate_data,
#                 observations_LL=covariate_data[,c('Lat','Lon')],
#                 grid_dim_km = c(0.5,0.5)
                 input_grid=user_region
                 )

# Plot results
results = plot(fit)

save(results,file = 'results.rdata')
save(fit,file="fit.rdata")

#load("/Users/jcao22/Google Drive/Research_NCSU/Snapper_grouper_SDM/VAST/results.RData")
#load("/Users/jcao22/Google Drive/Research_NCSU/Snapper_grouper_SDM/VAST/fit.RData")

load("/Users/jcao22/Google Drive/Research_NCSU/Snapper_grouper_SDM/VAST_2021/results.RData")
load("/Users/jcao22/Google Drive/Research_NCSU/Snapper_grouper_SDM/VAST_2021/fit.RData")

load("/Users/jcao22/My Drive/Research_NCSU/Snapper_grouper_SDM/VAST_2021/version1-results/results.RData")
load("/Users/jcao22/My Drive/Research_NCSU/Snapper_grouper_SDM/VAST_2021/version1-results/fit.RData")


#################### producing customized figures ##############
shoreline <- ggplot() + geom_polygon(data=world,aes(x=long,y=lat,group=group),colour='black',fill='gray')+
             coord_fixed(xlim = c(-82.5, -75), ylim = c(27, 36))

##################################################################

factors_rotate = data.frame(species = common.names, Epsilon1_factor_1 = results[["Factors"]][["Rotated_loadings"]]$Epsilon1[,1],
                            Epsilon1_factor_2 = results[["Factors"]][["Rotated_loadings"]]$Epsilon1[,2],
                            Epsilon2_factor_1 = results[["Factors"]][["Rotated_loadings"]]$Epsilon2[,1],
                            Epsilon2_factor_2 = results[["Factors"]][["Rotated_loadings"]]$Epsilon2[,2],
                            Omega1_factor_1 = results[["Factors"]][["Rotated_loadings"]][["Omega1"]][,1],
                            Omega1_factor_2 = results[["Factors"]][["Rotated_loadings"]][["Omega1"]][,2],
                            Omega2_factor_1 = results[["Factors"]][["Rotated_loadings"]][["Omega2"]][,1],
                            Omega2_factor_2 = results[["Factors"]][["Rotated_loadings"]][["Omega2"]][,2]
                            )
require("ggrepel")

pdf('Factor_loading_omega1.pdf')
p <- ggplot(factors_rotate, aes(Omega1_factor_1, Omega1_factor_2)) +
  geom_point(color = 'red') +
  theme_classic(base_size = 12) +
  geom_hline(yintercept=0, linetype="dashed", color = "gray") + geom_vline(xintercept=0, linetype="dashed", color = "gray")
p + geom_text_repel(aes(label = species),size = 3.5) + ggtitle("Factor loadings for spatial encounter probability") +
  xlab("Factor 1 (89.7% variance explained)") + ylab("Factor 2 (8.3% variance explained)") + theme(
    plot.title = element_text(color="black", size=16, face="bold"),
    axis.title.x = element_text(color="black", size=14, face="bold"),
    axis.title.y = element_text(color="black", size=14, face="bold"))
dev.off()

pdf('Factor_loading_omega2.pdf')
p <- ggplot(factors_rotate, aes(Omega2_factor_1, Omega2_factor_2)) +
  geom_point(color = 'red') +
  theme_classic(base_size = 12)+
  geom_hline(yintercept=0, linetype="dashed", color = "gray") + geom_vline(xintercept=0, linetype="dashed", color = "gray")
p + geom_text_repel(aes(label = species),size = 3.5) + ggtitle("Factor loadings for spatial catch rates") +
  xlab("Factor 1 (86.4% variance explained)") + ylab("Factor 2 (7.2% variance explained)") + theme(
    plot.title = element_text(color="black", size=16, face="bold"),
    axis.title.x = element_text(color="black", size=14, face="bold"),
    axis.title.y = element_text(color="black", size=14, face="bold"))
dev.off()

pdf('Factor_loading_epsilon1.pdf')
p <- ggplot(factors_rotate, aes(Epsilon1_factor_1, Epsilon1_factor_2)) +
  geom_point(color = 'red') +
  theme_classic(base_size = 12)+
  geom_hline(yintercept=0, linetype="dashed", color = "gray") + geom_vline(xintercept=0, linetype="dashed", color = "gray")
p + geom_text_repel(aes(label = species),size = 3.5) + ggtitle("Factor loadings for spatiotemporal encounter probability") +
  xlab("Factor 1 (73.4% variance explained)") + ylab("Factor 2 (16.1% variance explained)") + theme(
  plot.title = element_text(color="black", size=16, face="bold"),
  axis.title.x = element_text(color="black", size=14, face="bold"),
  axis.title.y = element_text(color="black", size=14, face="bold"))
dev.off()

pdf('Factor_loading_epsilon2.pdf')
p <- ggplot(factors_rotate, aes(Epsilon2_factor_1, Epsilon2_factor_2)) +
  geom_point(color = 'red') +
  theme_classic(base_size = 12)+
  geom_hline(yintercept=0, linetype="dashed", color = "gray") + geom_vline(xintercept=0, linetype="dashed", color = "gray")
p + geom_text_repel(aes(label = species),size = 3.5) + ggtitle("Factor loadings for spatiotemporal catch rates") +
  xlab("Factor 1 (57.7% variance explained)") + ylab("Factor 2 (23% variance explained)") + theme(
    plot.title = element_text(color="black", size=16, face="bold"),
    axis.title.x = element_text(color="black", size=14, face="bold"),
    axis.title.y = element_text(color="black", size=14, face="bold"))
dev.off()


# covaraince matrix (omega1,omega2,epsilon1,epsilon2)
library(corrplot);library(ggcorrplot)
##############################
Cov_omega1 = fit$Report$L_omega1_cf %*% t(fit$Report$L_omega1_cf)
Cor_omega1 = cov2cor(Cov_omega1)
colnames(Cor_omega1)=common.names
rownames(Cor_omega1)=common.names

range(Cor_omega1[lower.tri(Cor_omega1, diag = FALSE)])
median(Cor_omega1[lower.tri(Cor_omega1, diag = FALSE)])

#corrplot( Cor_omega1, method="pie", type="lower")
pdf('corrplot_omega1.pdf')
corrplot_omega1 <- ggcorrplot(Cor_omega1, hc.order = TRUE, lab = TRUE, 
                              lab_size = 2.0, type = 'lower', tl.cex = 9,
                              outline.color = 'white', title='A (encounter probability )')
corrplot_omega1
dev.off()

dis_omega1 = matrix(NA,nrow=21,ncol=21)
for(i in 1:21){
  for(j in 1:21){
    if(i==j){
      dis_omega1[i,j]=Cov_omega1[i,j]
    }else{
      dis_omega1[i,j]=sqrt(Cov_omega1[i,i]+Cov_omega1[j,j]-2*Cov_omega1[i,j])
    }
  }
}
colnames(dis_omega1)=common.names
rownames(dis_omega1)=common.names

##############################
Cov_omega2 = fit$Report$L_omega2_cf %*% t(fit$Report$L_omega2_cf)
Cor_omega2 = cov2cor(Cov_omega2)
colnames(Cor_omega2)=common.names
rownames(Cor_omega2)=common.names

range(Cor_omega2[lower.tri(Cor_omega2, diag = FALSE)])
median(Cor_omega2[lower.tri(Cor_omega2, diag = FALSE)])

pdf('corrplot_omega2.pdf')
corrplot_omega2 <- ggcorrplot(Cor_omega2, hc.order = TRUE, lab = TRUE, 
                              lab_size = 2.0, type = 'lower', tl.cex = 9,
                              outline.color = 'white', title='B (positive density)')
corrplot_omega2
dev.off()

dis_omega2 = matrix(NA,nrow=21,ncol=21)
for(i in 1:21){
  for(j in 1:21){
    if(i==j){
      dis_omega2[i,j]=Cov_omega2[i,j]
    }else{
      dis_omega2[i,j]=sqrt(Cov_omega2[i,i]+Cov_omega2[j,j]-2*Cov_omega2[i,j])
    }
  }
}
colnames(dis_omega2)=common.names
rownames(dis_omega2)=common.names

##############################
Cov_epsilon1 = fit$Report$L_epsilon1_cf %*% t(fit$Report$L_epsilon1_cf)
Cor_epsilon1 = cov2cor(Cov_epsilon1)
colnames(Cor_epsilon1)=common.names
rownames(Cor_epsilon1)=common.names

range(Cor_epsilon1[lower.tri(Cor_epsilon1, diag = FALSE)])
median(Cor_epsilon1[lower.tri(Cor_epsilon1, diag = FALSE)])

pdf('corrplot_epsilon1.pdf')
corrplot_epsilon1 <- ggcorrplot(Cor_epsilon1, hc.order = TRUE, lab = TRUE, 
                                lab_size = 2.0, type = 'lower', tl.cex = 9,
                                outline.color = 'white', title='A (encounter probability )')
corrplot_epsilon1
dev.off()

dis_epsilon1 = matrix(NA,nrow=21,ncol=21)
for(i in 1:21){
  for(j in 1:21){
    if(i==j){
      dis_epsilon1[i,j]=Cov_epsilon1[i,j]
    }else{
      dis_epsilon1[i,j]=sqrt(Cov_epsilon1[i,i]+Cov_epsilon1[j,j]-2*Cov_epsilon1[i,j])
    }
  }
}
colnames(dis_epsilon1)=common.names
rownames(dis_epsilon1)=common.names

##############################
Cov_epsilon2 = fit$Report$L_epsilon2_cf %*% t(fit$Report$L_epsilon2_cf)
Cor_epsilon2 = cov2cor(Cov_epsilon2)
colnames(Cor_epsilon2)=common.names
rownames(Cor_epsilon2)=common.names

range(Cor_epsilon2[lower.tri(Cor_epsilon2, diag = FALSE)])
median(Cor_epsilon2[lower.tri(Cor_epsilon2, diag = FALSE)])


pdf('corrplot_epsilon2.pdf')
corrplot_epsilon2 <- ggcorrplot(Cor_epsilon2, hc.order = TRUE, lab = TRUE, 
                                lab_size = 2.0, type = 'lower', tl.cex = 9,
                                outline.color = 'white', title='B (positive density)')
corrplot_epsilon2
dev.off()

dis_epsilon2 = matrix(NA,nrow=21,ncol=21)
for(i in 1:21){
  for(j in 1:21){
    if(i==j){
      dis_epsilon2[i,j]=Cov_epsilon2[i,j]
    }else{
      dis_epsilon2[i,j]=sqrt(Cov_epsilon2[i,i]+Cov_epsilon2[j,j]-2*Cov_epsilon2[i,j])
    }
  }
}
colnames(dis_epsilon2)=common.names
rownames(dis_epsilon2)=common.names

##############################
pdf('corrplot_omega.pdf',height=9,width=19)
plot_grid(corrplot_omega1, corrplot_omega2, labels = c('Encounter probability', 'Density'), 
          nrow = 1, rel_widths=c(1,1), scale =c(1,1))
dev.off()

pdf('corrplot_epsilon.pdf',height=9,width=19)
plot_grid(corrplot_epsilon1, corrplot_epsilon2, labels = c('Encounter probability', 'Density'), 
          nrow = 1, rel_widths=c(1,1), scale =c(1,1))
dev.off()

#Cor_omega2
#Cor_epsilon2

##############################
# Hierarchical clustering using Complete Linkage
library('factoextra')
fviz_nbclust(dis_omega1, FUN = hcut, method = "silhouette")
fviz_nbclust(dis_omega2, FUN = hcut, method = "silhouette")
fviz_nbclust(dis_epsilon1, FUN = hcut, method = "silhouette")
fviz_nbclust(dis_epsilon2, FUN = hcut, method = "silhouette")
n.omega1 = 3; n.omega2 = 2; n.epsilon1 = 2; n.epsilon2 = 3

hc.omega1 <- hclust(as.dist(dis_omega1), method = "ward.D" )
#plot(hc.omega1, axes = FALSE, xlab='', ylab='', main='Encounter probability',sub="",rotate=TRUE)
#rect.hclust(hc.omega1, k = 2, border = c('red','green'))

hc.omega2 <- hclust(as.dist(dis_omega2), method = "ward.D" )
#plot(hc.omega2, axes = FALSE, xlab='', ylab='', main='Density',sub="")
#rect.hclust(hc.omega2, k = 3, border = c('red','green','yellow'))

hc.epsilon1 <- hclust(as.dist(dis_epsilon1), method = "ward.D" )
#plot(hc.epsilon1)

hc.epsilon2 <- hclust(as.dist(dis_epsilon2), method = "ward.D" )
#plot(hc.epsilon2)

##############################
#rm(list=ls())
#load("/Users/jcao22/Google Drive/Research_NCSU/Snapper_grouper_SDM/VAST/parameter_estimates.RData")
load("/Users/jcao22/Google Drive/Research_NCSU/Snapper_grouper_SDM/VAST_2021/parameter_estimates.RData")
load('/Users/jcao22/My Drive/Research_NCSU/Snapper_grouper_SDM/VAST_2021/version1-results/parameter_estimates.RData')

#### average density for each species ####
#library(stars)

for (s in 1:21){
  average_den.temp = (fit$Report$Omega1_gc + fit$Report$Omega2_gc)[,s] +
                     (fit$Report$beta1_tc + fit$Report$beta2_tc)[1,s]
  plot_average_den.temp = data.frame(species = rep(common.names[s],nrow(results$map_list$PlotDF)),
                                     Longitude = results$map_list$PlotDF$Lon,
                                     Latitude = results$map_list$PlotDF$Lat,
                                     average_density = average_den.temp[results$map_list$PlotDF$x2i])
  if(s==1){
    plot_average_den = plot_average_den.temp
  }else{
    plot_average_den = rbind(plot_average_den,plot_average_den.temp)
  }
}

# plot_average_den = data.frame(lon = results$map_list$PlotDF$Lon, 
#                                    lat = results$map_list$PlotDF$Lat)
# for (s in 1:21){
#   average_den.temp = (fit$Report$Omega1_gc + fit$Report$Omega2_gc)[,s] +
#                       (fit$Report$beta1_tc + fit$Report$beta2_tc)[1,s]
#   average_density = average_den.temp[results$map_list$PlotDF$x2i]
#   plot_average_den = cbind(plot_average_den,average_density)
# }
# names(plot_average_den) = c('Lon','Lat',paste0(spp.SP.names2))

# library(ggmap)
# 
# domain = c(-82.5,27,-75,36)
# map2 = get_map(location = domain, source = "stamen", maptype = "watercolor", color="bw")

pdf('Average_density2.pdf',height=8,width=13)
print(
  shoreline + 
  geom_tile(data = plot_average_den, aes(x = Longitude, y = Latitude, fill = average_density, height=0.05, width=0.05)) +
  facet_wrap(species ~., ncol=7) + xlab("Longitude °E") + ylab("Latitude °N") +
  #coord_equal() +
  scale_fill_viridis_c(values=c(0, 0.6, 0.7, 0.8, 0.9, 1)) +
   theme_bw() +
  #theme_void() +
    guides(fill = guide_colourbar(barwidth = 15,barheight = 1,title = "Relative Density"))+
  theme(legend.position = "top",
  axis.line = element_line(color='black'),
  plot.background = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
    axis.title=element_text(size=14,face="bold"),
  strip.text.x = element_text(size = 10,face="bold"))
  )
dev.off()

# stratify by depth and latitude
red_snapper_average_den<-subset(plot_average_den,species==c('Red snapper'))

write.csv(red_snapper_average_den,'RS_log_average_den.csv')

bathy.dat<-read.table('/Users/jcao22/Google Drive/Research_NCSU/Snapper_grouper_SDM/749.xyz', sep='')
names(bathy.dat)<-c('lon','lat','depth')
bathy.dat$depth[bathy.dat$depth>0]<-NA #Avoid points above water
bathy.mat<-as.matrix(bathy.dat)
e<-extent(bathy.mat[,1:2])

# ncol = 20/0.02 = 1000
# nrow = 24/0.02 = 1200
r <- raster(e, ncol=1000, nrow=1200)
bathy.raster <- rasterize(bathy.mat[, 1:2], r, bathy.mat[,3], fun=mean)
plot(bathy.raster)
points(red_snapper_average_den$Longitude,red_snapper_average_den$Latitude, pch=0, cex = 0.2 )

depth<-extract(bathy.raster,red_snapper_average_den[,2:3])
red_snapper_average_den$depth<-depth

lat_strata <- c(27,28,29,30,31,32,33,34,36)
depth_strata <- c(-1003,-45,-20,0)

red_snapper_props<-matrix(NA,nrow=8,ncol=3)

for (l in 1:8){
  for (d in 1:3){
    temp1<-subset(red_snapper_average_den,Latitude>=lat_strata[l]&Latitude<lat_strata[l+1])
    temp2<-subset(temp1,depth>=depth_strata[d]&depth<depth_strata[d+1])
    red_snapper_props[l,d]=sum(exp(temp2$average_density))
  }
}
red_snapper_props <- 100*(red_snapper_props/sum(red_snapper_props))

red_snapper_average_den_coord<-as.matrix(red_snapper_average_den[,2:3])
e1<-extent(red_snapper_average_den_coord)

den.r <- raster(e1, ncol=180, nrow=120)
red.den.raster <- rasterize(red_snapper_average_den[, 2:3], den.r, red_snapper_average_den[,4], fun=mean)

 # library(tidyr)
 # rasdf<-as.data.frame(red.den.raster,xy=TRUE)%>%drop_na()
 # ggplot()+
 #   geom_polygon(data=world,aes(x=long,y=lat,group=group),colour='black',fill='gray')+
 #   geom_sf(fill='transparent')+
 #   #coord_fixed(xlim = c(-82.5, -75), ylim = c(27, 36))+
 #   coord_sf(xlim = c(-82.5, -75), ylim = c(27, 36), expand = FALSE)+
 #   geom_raster(aes(x=x,y=y,fill=log(layer)),data=rasdf)+
 #   scale_fill_viridis_c(name='Log(relative density)',direction=1)+
 #   labs(x='Longitude',y='Latitude',title='Red Snapper average density')+
 #   cowplot::theme_cowplot()+
 #   theme(panel.grid.major = element_line(color='gray',linetype='dashed',size=.5),
 #         panel.grid.minor = element_blank(),
 #         panel.ontop = TRUE,
 #         panel.background = element_rect(fill=NA,color='black'))


####### plot factors #######
############ Omega ###############
factor_omega1 = results[["Factors"]][["Rotated_factors"]][["Omega1"]]

omega1_factor1 = data.frame(Factor=rep('Factor 1',nrow(results$map_list$PlotDF)),Latitude=results$map_list$PlotDF$Lat,
                                   Longitude=results$map_list$PlotDF$Lon, Factor_value=factor_omega1[1:600,1,][results$map_list$PlotDF$x2i])
omega1_factor2 = data.frame(Factor=rep('Factor 2',nrow(results$map_list$PlotDF)),Latitude=results$map_list$PlotDF$Lat,
                                   Longitude=results$map_list$PlotDF$Lon, Factor_value=factor_omega1[1:600,2,][results$map_list$PlotDF$x2i])
omega1_factor3 = data.frame(Factor=rep('Factor 3',nrow(results$map_list$PlotDF)),Latitude=results$map_list$PlotDF$Lat,
                                   Longitude=results$map_list$PlotDF$Lon, Factor_value=factor_omega1[1:600,3,][results$map_list$PlotDF$x2i])

factor_omega1_plot = rbind(omega1_factor1,omega1_factor2)

factor_omega2 = results[["Factors"]][["Rotated_factors"]][["Omega2"]]

omega2_factor1 = data.frame(Factor=rep('Factor 1',nrow(results$map_list$PlotDF)),Latitude=results$map_list$PlotDF$Lat,
                            Longitude=results$map_list$PlotDF$Lon, Factor_value=factor_omega2[1:600,1,][results$map_list$PlotDF$x2i])
omega2_factor2 = data.frame(Factor=rep('Factor 2',nrow(results$map_list$PlotDF)),Latitude=results$map_list$PlotDF$Lat,
                            Longitude=results$map_list$PlotDF$Lon, Factor_value=factor_omega2[1:600,2,][results$map_list$PlotDF$x2i])
omega2_factor3 = data.frame(Factor=rep('Factor 3',nrow(results$map_list$PlotDF)),Latitude=results$map_list$PlotDF$Lat,
                            Longitude=results$map_list$PlotDF$Lon, Factor_value=factor_omega2[1:600,3,][results$map_list$PlotDF$x2i])

factor_omega2_plot = rbind(omega2_factor1,omega2_factor2)


library(cowplot);library(ggcorrplot);library(ggdendro)
pdf('factor_omega1.pdf',width = 12, height = 12)
A = #ggplot() + 
  shoreline +
  #geom_polygon(data=world,aes(x=long,y=lat,group=group),colour='black',fill='gray') +
  geom_tile(data = factor_omega1_plot, aes(x = Longitude, y = Latitude, 
                                        fill = Factor_value, height=0.05, width=0.05),inherit.aes = F) +
  facet_wrap(Factor ~., nrow=2) +
  #coord_equal() +
  scale_fill_viridis_c() +
  theme_bw() +  xlab("Longitude °E") + ylab("Latitude °N") +
  #theme_void() +
  guides(fill = guide_colourbar(barwidth = 1,barheight = 10,title = ""))+
  theme(legend.position = c(0.9,0.7),plot.margin = margin(0, 0, 0, 0),
        axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.title=element_text(size=14,face="bold"),
        strip.text.x = element_text(size = 10,face="bold"))

B = ggplot(factors_rotate, aes(Omega1_factor_1, Omega1_factor_2)) +
  geom_point(color = 'red') +
  theme_classic(base_size = 12) +
  geom_hline(yintercept=0, linetype="dashed", color = "gray") + geom_vline(xintercept=0, linetype="dashed", color = "gray") +
  geom_text_repel(aes(label = species),size = 3.5) + 
  xlab("Factor 1 (84.9% variance explained)") + ylab("Factor 2 (13.0% variance explained)") + theme(
    plot.title = element_text(color="black", size=16, face="bold"),
    axis.title.x = element_text(color="black", size=14, face="bold"),
    axis.title.y = element_text(color="black", size=14, face="bold"))

dendr    <- dendro_data(hc.omega1, type="rectangle") # convert for ggplot
clust    <- cutree(hc.omega1,k=3)                    # find 2 clusters
clust.df <- data.frame(label=names(clust), cluster=factor(clust))
# dendr[["labels"]] has the labels, merge with clust.df based on 
dendr[["labels"]] <- merge(dendr[["labels"]],clust.df, by="label")
# plot the dendrogram; note use of color=cluster in geom_text(...)

hcdata <- dendro_data_k(hc.omega1, 3)

E <- plot_ggdendro(hcdata,
                   direction   = "lr",
                   expand.y    = 0.2)+
  theme(#axis.text.x      = element_text(color = "#ffffff"),
        panel.background = element_rect(fill  = "#ffffff"),
        #axis.ticks       = element_blank(),
        axis.line.x = element_line(color = "black", size=0.5, lineend = 'square')) +  
  scale_color_brewer(palette = "Set1")+
  xlab(NULL) +
  ylab(NULL) +
  annotate("text", x=21, y=18, label= "Ward's hierarchical clustering dendrogram",size = 4,fontface = "bold") +
  annotate("text", x=20, y=18, label= "(number of clusters = 3)",size = 4) 


C = ggplot() + 
  geom_segment(data=segment(dendr), aes(x=x, y=y, xend=xend, 
                                        yend=yend)) + 
  geom_text(data=label(dendr), aes(x, y, label=label, hjust=1, color=cluster), 
            size=4) + scale_colour_discrete(l = 20) +
  coord_flip() + 
  #scale_y_reverse(expand=c(0.25, 0),limits=c(35,0)) + 
  expand_limits(y = -11) +
  annotate("text", x=21, y=18, label= "Ward's hierarchical clustering dendrogram",size = 4,fontface = "bold") +
  annotate("text", x=20, y=18, label= "(number of clusters = 3)",size = 4) +
  theme_void() + theme(legend.position="top")

D=plot_grid(B, E, labels = c('B', 'C'), nrow = 2, rel_widths=c(1,1), scale =c(1,1))
plot_grid(A, D, labels = c('A', ''), nrow = 1, rel_widths=c(1,1), scale =c(1,1))
#plot_grid(A, B, E, labels = c('A', 'B', 'C'), nrow = 3, rel_widths=c(1.4,0.7,0.8), scale =c(1.0,0.85,0.9))
dev.off()

#######################################
pdf('factor_omega2.pdf',width = 12, height = 12)
A = #ggplot() + 
  shoreline +
  geom_tile(data = factor_omega2_plot, aes(x = Longitude, y = Latitude, fill = Factor_value, height=0.05, width=0.05)) +
  facet_wrap(Factor ~., nrow=2) +
  #coord_equal() +
  scale_fill_viridis_c() +
  theme_bw() + xlab("Longitude °E") + ylab("Latitude °N") +
  #theme_void() +
  guides(fill = guide_colourbar(barwidth = 1,barheight = 10,title = ""))+
  theme(legend.position = c(0.9,0.7),plot.margin = margin(0, 0, 0, 0),
        axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.title=element_text(size=14,face="bold"),
        strip.text.x = element_text(size = 10,face="bold"))

B = ggplot(factors_rotate, aes(Omega2_factor_1, Omega2_factor_2)) +
  geom_point(color = 'red') +
  theme_classic(base_size = 12) +
  geom_hline(yintercept=0, linetype="dashed", color = "gray") + geom_vline(xintercept=0, linetype="dashed", color = "gray") +
  geom_text_repel(aes(label = species),size = 3.5) + 
  xlab("Factor 1 (84.3% variance explained)") + ylab("Factor 2 (12.3% variance explained)") + theme(
    plot.title = element_text(color="black", size=16, face="bold"),
    axis.title.x = element_text(color="black", size=14, face="bold"),
    axis.title.y = element_text(color="black", size=14, face="bold"))

dendr    <- dendro_data(hc.omega2, type="rectangle") # convert for ggplot
clust    <- cutree(hc.omega2,k=2)                    # find 3 clusters
clust.df <- data.frame(label=names(clust), cluster=factor(clust))
# dendr[["labels"]] has the labels, merge with clust.df based on 
dendr[["labels"]] <- merge(dendr[["labels"]],clust.df, by="label")
# plot the dendrogram; note use of color=cluster in geom_text(...)
C = ggplot() + 
  geom_segment(data=segment(dendr), aes(x=x, y=y, xend=xend, 
                                        yend=yend)) + 
  geom_text(data=label(dendr), aes(x, y, label=label, hjust=1.0, color=cluster), 
            size=4) + scale_colour_discrete(l = 20) +
  coord_flip() + 
  #scale_y_reverse(expand=c(0.3, 0),limits=c(5.8,0)) + 
  expand_limits(y = -1.5) +
  annotate("text", x=21, y=3.2, label= "Ward's hierarchical clustering dendrogram",size = 4,fontface = "bold") +
  annotate("text", x=20, y=3.5, label= "(number of clusters = 2)",size = 4) +
  theme_void() + theme(legend.position="none")

hcdata <- dendro_data_k(hc.omega2, 2)

E <- plot_ggdendro(hcdata,
                   direction   = "lr",
                   expand.y    = 0.2)+
  theme(#axis.text.x      = element_text(color = "#ffffff"),
        panel.background = element_rect(fill  = "#ffffff"),
        #axis.ticks       = element_blank(),
        axis.line.x = element_line(color = "black", size=0.5, lineend = 'square')) +  
  scale_color_brewer(palette = "Set1")+
  xlab(NULL) +
  ylab(NULL) +
  annotate("text", x=21, y=3, label= "Ward's hierarchical clustering dendrogram",size = 4,fontface = "bold") +
  annotate("text", x=20, y=3, label= "(number of clusters = 2)",size = 4) 


D =plot_grid(B, E, labels = c('B', 'C'), nrow = 2, rel_widths=c(1,1), scale =c(0.96,0.95))
plot_grid(A, D, labels = c('A', ''), nrow = 1, rel_heights=c(1,1), scale =c(1,1))

#plot_grid(A, B, labels = c('', ''), nrow = 1, rel_widths=c(1.6,1), scale =c(1,0.8))
dev.off()

############ epsilon ###############
factor_epsilon1 = results[["Factors"]][["Rotated_factors"]][["Epsilon1"]]

for(y in 1:11){
  epsilon1_factor1.temp = data.frame(Year=rep(2010+y,nrow(results$map_list$PlotDF)),
                                Factor=rep('Factor 1',nrow(results$map_list$PlotDF)),
                                Latitude=results$map_list$PlotDF$Lat,
                                Longitude=results$map_list$PlotDF$Lon, 
                                Factor_value=factor_epsilon1[1:600,1,y][results$map_list$PlotDF$x2i])
  if(y==1){
    epsilon1_factor1 = epsilon1_factor1.temp
  }
  if(y>1){
    epsilon1_factor1 = rbind(epsilon1_factor1,epsilon1_factor1.temp)
  }
}

for(y in 1:11){
  epsilon1_factor2.temp = data.frame(Year=rep(2010+y,nrow(results$map_list$PlotDF)),
                                     Factor=rep('Factor 1',nrow(results$map_list$PlotDF)),
                                     Latitude=results$map_list$PlotDF$Lat,
                                     Longitude=results$map_list$PlotDF$Lon, 
                                     Factor_value=factor_epsilon1[1:600,2,y][results$map_list$PlotDF$x2i])
  if(y==1){
    epsilon1_factor2 = epsilon1_factor2.temp
  }
  if(y>1){
    epsilon1_factor2 = rbind(epsilon1_factor2,epsilon1_factor2.temp)
  }
}

factor_epsilon2 = results[["Factors"]][["Rotated_factors"]][["Epsilon2"]]

for(y in 1:11){
  epsilon2_factor1.temp = data.frame(Year=rep(2010+y,nrow(results$map_list$PlotDF)),
                                     Factor=rep('Factor 1',nrow(results$map_list$PlotDF)),
                                     Latitude=results$map_list$PlotDF$Lat,
                                     Longitude=results$map_list$PlotDF$Lon, 
                                     Factor_value=factor_epsilon2[1:600,1,y][results$map_list$PlotDF$x2i])
  if(y==1){
    epsilon2_factor1 = epsilon2_factor1.temp
  }
  if(y>1){
    epsilon2_factor1 = rbind(epsilon2_factor1,epsilon2_factor1.temp)
  }
}

for(y in 1:11){
  epsilon2_factor2.temp = data.frame(Year=rep(2010+y,nrow(results$map_list$PlotDF)),
                                     Factor=rep('Factor 1',nrow(results$map_list$PlotDF)),
                                     Latitude=results$map_list$PlotDF$Lat,
                                     Longitude=results$map_list$PlotDF$Lon, 
                                     Factor_value=factor_epsilon2[1:600,2,y][results$map_list$PlotDF$x2i])
  if(y==1){
    epsilon2_factor2 = epsilon2_factor2.temp
  }
  if(y>1){
    epsilon2_factor2 = rbind(epsilon2_factor2,epsilon2_factor2.temp)
  }
}

################################################################################
pdf('factor_epsilon1.pdf',width = 16, height = 14)
A = shoreline + 
  geom_tile(data = epsilon1_factor1, aes(x = Longitude, y = Latitude, fill = Factor_value, height=0.05, width=0.05)) +
  facet_wrap(Year ~., ncol=4) + xlab("") + ylab("Latitude °N") +
  #coord_equal() +
  scale_fill_viridis_c() +
  theme_bw() +
  #theme_void() +
  guides(fill = guide_colourbar(barwidth = 1,barheight = 15,title = ""))+
  theme(legend.position = "right",plot.margin = margin(0, 0, 0, 0),
        axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.title=element_text(size=14,face="bold"),
        strip.text.x = element_text(size = 10,face="bold"))

B = shoreline + 
  geom_tile(data = epsilon1_factor2, aes(x = Longitude, y = Latitude, fill = Factor_value, height=0.05, width=0.05)) +
  facet_wrap(Year ~., ncol=4) + xlab("Longitude °E") + ylab("Latitude °N") +
  #coord_equal() +
  scale_fill_viridis_c() +
  theme_bw() +
  #theme_void() +
  guides(fill = guide_colourbar(barwidth = 1,barheight = 15,title = ""))+
  theme(legend.position = "right",plot.margin = margin(0, 0, 0, 0),
        axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.title=element_text(size=14,face="bold"),
        strip.text.x = element_text(size = 10,face="bold"))

C = plot_grid(A, B, labels = c('Factor 1', 'Factor 2'), nrow = 2, rel_widths=c(1,1), scale =c(1,1))

D = ggplot(factors_rotate, aes(Epsilon1_factor_1, Epsilon1_factor_2)) +
  geom_point(color = 'red') +
  theme_classic(base_size = 12) +
  geom_hline(yintercept=0, linetype="dashed", color = "gray") + geom_vline(xintercept=0, linetype="dashed", color = "gray") +
  geom_text_repel(aes(label = species),size = 3.5) + 
  xlab("Factor 1 (65.7% variance explained)") + ylab("Factor 2 (17.8% variance explained)") + theme(
    plot.title = element_text(color="black", size=16, face="bold"),
    axis.title.x = element_text(color="black", size=14, face="bold"),
    axis.title.y = element_text(color="black", size=14, face="bold"))

dendr    <- dendro_data(hc.epsilon1, type="rectangle") # convert for ggplot
clust    <- cutree(hc.epsilon1,k=2)                    # find 2 clusters
clust.df <- data.frame(label=names(clust), cluster=factor(clust))
# dendr[["labels"]] has the labels, merge with clust.df based on 
dendr[["labels"]] <- merge(dendr[["labels"]],clust.df, by="label")
# plot the dendrogram; note use of color=cluster in geom_text(...)
E = ggplot() + 
  geom_segment(data=segment(dendr), aes(x=x, y=y, xend=xend, 
                                        yend=yend)) + 
  geom_text(data=label(dendr), aes(x, y, label=label, hjust=1, color=cluster), 
            size=4) + scale_colour_discrete(l = 20) +
  coord_flip() + 
  #scale_y_reverse(expand=c(0.25, 0),limits=c(35,0)) + 
  expand_limits(y = -1.5) +
  annotate("text", x=12, y=1.8, label= "Ward's hierarchical clustering dendrogram",size = 4,fontface = "bold") +
  annotate("text", x=11, y=2.0, label= "(number of clusters = 2)",size = 4) +
  theme_void() + theme(legend.position="none")

hcdata <- dendro_data_k(hc.epsilon1, 2)

K <- plot_ggdendro(hcdata,
                   direction   = "lr",
                   expand.y    = 0.2)+
  theme(#axis.text.x      = element_text(color = "#ffffff"),
        panel.background = element_rect(fill  = "#ffffff"),
        #axis.ticks       = element_blank(),
        axis.line.x = element_line(color = "black", size=0.5, lineend = 'square')) +
  scale_color_brewer(palette = "Set1")+
  xlab(NULL) +
  ylab(NULL) +
  annotate("text", x=21.5, y=2, label= "Ward's hierarchical clustering dendrogram",size = 4,fontface = "bold") +
  annotate("text", x=20.5, y=2, label= "(number of clusters = 2)",size = 4) 


H = plot_grid(D, K, labels = c('B', 'C'), nrow = 2, rel_heights=c(1,1), scale =c(0.98,0.98))
plot_grid(C, H, labels = c('A', ''), nrow = 1, rel_widths=c(1.5,1.2), scale =c(1,0.99))

dev.off()

################################################################################
pdf('factor_epsilon2.pdf',width = 16, height = 14)
A = shoreline + 
  geom_tile(data = epsilon2_factor1, aes(x = Longitude, y = Latitude, fill = Factor_value, height=0.05, width=0.05)) +
  facet_wrap(Year ~., ncol=4) + xlab("") + ylab("Latitude °N") +
  #coord_equal() +
  scale_fill_viridis_c() +
  theme_bw() +
  #theme_void() +
  guides(fill = guide_colourbar(barwidth = 1,barheight = 15,title = ""))+
  theme(legend.position = "right",plot.margin = margin(0, 0, 0, 0),
        axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.title=element_text(size=14,face="bold"),
        strip.text.x = element_text(size = 10,face="bold"))

B = shoreline + 
  geom_tile(data = epsilon2_factor2, aes(x = Longitude, y = Latitude, fill = Factor_value, height=0.05, width=0.05)) +
  facet_wrap(Year ~., ncol=4) + xlab("Longitude °E") + ylab("Latitude °N") +
  #coord_equal() +
  scale_fill_viridis_c() +
  theme_bw() +
  #theme_void() +
  guides(fill = guide_colourbar(barwidth = 1,barheight = 15,title = ""))+
  theme(legend.position = "right",plot.margin = margin(0, 0, 0, 0),
        axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.title=element_text(size=14,face="bold"),
        strip.text.x = element_text(size = 10,face="bold"))

C = plot_grid(A, B, labels = c('Factor 1', 'Factor 2'), nrow = 2, rel_widths=c(1,1), scale =c(1,1))

D = ggplot(factors_rotate, aes(Epsilon2_factor_1, Epsilon2_factor_2)) +
  geom_point(color = 'red') +
  theme_classic(base_size = 12) +
  geom_hline(yintercept=0, linetype="dashed", color = "gray") + geom_vline(xintercept=0, linetype="dashed", color = "gray") +
  geom_text_repel(aes(label = species),size = 3.5) + 
  xlab("Factor 1 (59.9% variance explained)") + ylab("Factor 2 (22.3% variance explained)") + theme(
    plot.title = element_text(color="black", size=16, face="bold"),
    axis.title.x = element_text(color="black", size=14, face="bold"),
    axis.title.y = element_text(color="black", size=14, face="bold"))

dendr    <- dendro_data(hc.epsilon2, type="rectangle") # convert for ggplot
clust    <- cutree(hc.epsilon2,k=3)                    # find 2 clusters
clust.df <- data.frame(label=names(clust), cluster=factor(clust))
# dendr[["labels"]] has the labels, merge with clust.df based on 
dendr[["labels"]] <- merge(dendr[["labels"]],clust.df, by="label")
# plot the dendrogram; note use of color=cluster in geom_text(...)
E = ggplot() + 
  geom_segment(data=segment(dendr), aes(x=x, y=y, xend=xend, 
                                        yend=yend)) + 
  geom_text(data=label(dendr), aes(x, y, label=label, hjust=1, color=cluster), 
            size=4) + scale_colour_discrete(l = 20) +
  coord_flip() + 
  #scale_y_reverse(expand=c(0.25, 0),limits=c(35,0)) + 
  expand_limits(y = -0.6) +
  annotate("text", x=21, y=1.5, label= "Ward's hierarchical clustering dendrogram",size = 4,fontface = "bold") +
  annotate("text", x=20, y=1.5, label= "(number of clusters = 3)",size = 4) +
  theme_void() + theme(legend.position="none")

hcdata <- dendro_data_k(hc.epsilon2, 3)

K <- plot_ggdendro(hcdata,
                   direction   = "lr",
                   expand.y    = 0.2)+
  theme(#axis.text.x      = element_text(color = "#ffffff"),
        panel.background = element_rect(fill  = "#ffffff"),
        #axis.ticks       = element_blank(),
        axis.line.x = element_line(color = "black", size=0.5, lineend = 'square')) + 
  scale_color_brewer(palette = "Set1")+
  xlab(NULL) +
  ylab(NULL) +
  annotate("text", x=21, y=1.5, label= "Ward's hierarchical clustering dendrogram",size = 4,fontface = "bold") +
  annotate("text", x=20, y=1.5, label= "(number of clusters = 3)",size = 4) 

H = plot_grid(D, K, labels = c('B', 'C'), nrow = 2, rel_heights=c(1,1), scale =c(0.98,0.98))
plot_grid(C, H, labels = c('A', ''), nrow = 1, rel_widths=c(1.5,1.2), scale =c(1,0.98))

dev.off()
###############################################################
#### abundance indices ####

indices = data.frame(Species=rep(common.names,11),Year=rep(seq(2011,2021,1),each=21),
                     Index=c(results$Index$log_Index_ctl[,,1,][,,1]), 
                     SD=c(results$Index$log_Index_ctl[,,1,][,,2])*1.96)
indices$CI_lower = indices$Index-1.96*indices$SD
indices$CI_upper = indices$Index+1.96*indices$SD

pdf('indices.pdf',width = 8, height = 11)
#write.csv(indices,'indices.csv')
ggplot(indices, aes(x = Year, y = Index)) +
  facet_wrap(Species ~., ncol=3, scales='free') +
  geom_ribbon(aes(ymin = Index - SD, ymax = Index + SD), alpha = 0.2) +
  theme_bw() + ylab('Log index') +
  geom_line() +
  geom_point() +
  scale_y_continuous(limits = c(-5, 5))+
  scale_x_continuous(breaks = c(2011:2021),labels=c('2011','','','2014','','','2017','','','2020',''))+
  #geom_line(aes(y = Index - SD), colour = "grey50", linetype = "dotted")+
  #geom_line(aes(y = Index + SD), colour = "grey50", linetype = "dotted")+
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.title=element_text(size=14,face="bold"),
        strip.text.x = element_text(size = 10,face="bold"))

dev.off()

color = c("#cea46e",
          "#58c3fd",
          "#cdc67a",
          "#b1c2ff",
          "#ddf5a9",
          "#c99cd5",
          "#a1d897",
          "#d6c3ff",
          "#72c899",
          "#d49abd",
          "#4ce7fa",
          "#ea9685",
          "#9efdff",
          "#ffb9d0",
          "#c3ffe1",
          "#89aedd",
          "#ffeeb5",
          "#72b4d0",
          "#dbffd3",
          "#8db68e",
          "#9fbdaa")

pdf('indices_1.pdf',width = 8, height = 10)

ggplot(indices, aes(x=Year, y=Index, color = Species)) +
  geom_line(size=1.3) + 
  scale_color_manual(values=color) +
  theme_bw() +
  ylab('Relative abundance index (log)') +
  guides(col = guide_legend(ncol = 1))+
  theme(axis.line = element_line(color='black'),
      plot.background = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      axis.title=element_text(size=14,face="bold"),
      strip.text.x = element_text(size = 12,face="bold"))
dev.off()
  #geom_ribbon(aes(ymin=CI_lower,ymax=CI_upper,fill=Species),color="grey70",alpha=0.4)


###############################################################
#### Center of Gravity ####
library(ggpmisc)

COG = results$Range$COG_Table
northing = data.frame(subset(COG,COG[,2]==2))
easting = data.frame(subset(COG,COG[,2]==1))
Species=rep(common.names,each=11)
northing$Category = Species
easting$Category = Species
northing$m = 'Northing'
easting$m = 'Easting'

COG.plot = rbind(northing,easting)

COG.plot$CI_lower = COG.plot$COG_hat-1.96*COG.plot$SE
COG.plot$CI_upper = COG.plot$COG_hat+1.96*COG.plot$SE

COG_n = subset(COG.plot,m=='Northing')
COG_e = subset(COG.plot,m=='Easting')

pdf('COG.pdf',width = 13, height = 14)
formula <- y ~ x    
ggplot(COG.plot, aes(x = Year, y = COG_hat)) +
  facet_wrap(~Category+m, ncol=6, scales='free') +
  geom_ribbon(aes(ymin = COG_hat - 1.96*SE, ymax = COG_hat + 1.96*SE), alpha = 0.2) +
  theme_bw() + ylab('') + 
  geom_line() + 
  geom_point() +
  scale_x_continuous(breaks = c(2011:2021),labels=c('2011','','','2014','','','2017','','','2020',''))+
  #geom_smooth(method = "lm", se = TRUE) +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.title=element_text(size=14,face="bold"),
        strip.text.x = element_text(size = 8,face="bold"))+
  geom_smooth(method = "lm", formula = formula, se = F, linetype="dashed", lwd = 1) +
  
  #stat_correlation(mapping = use_label(c("P")))+
  #stat_correlation(mapping = aes(label = ..r.label.., color = ifelse(after_stat(p.value) < 0.05,
  #                                              "red", "black"))) 
  #scale_color_identity()
  stat_poly_eq(aes(label = paste(..eq.label..),color = ifelse(after_stat(p.value) < 0.05,"red", "black")), 
              label.x.npc = "right", label.y.npc = 0.05,
               formula = formula, parse = TRUE, size = 2.5)+
  scale_color_identity()
  # stat_fit_glance(method = 'lm',
  #                 method.args = list(formula = formula),
  #                 geom = 'text',
  #                 aes(label = paste("P-value = ", signif(..p.value.., digits = 3), sep = "")),
  #                 label.x = "center", label.y = "top", size = 2.6)

dev.off()

#write.csv(COG.plot,'COG.csv')

COG.northing = subset(COG.plot,m=='Northing')

ggplot(COG.northing, aes(x=Year, y=COG_hat, color = Category)) +
  geom_line(aes(x=Year, y=COG_hat, color=Category)) +
  geom_ribbon(aes(ymin=CI_lower,ymax=CI_upper,fill=Category),color="grey70",alpha=0.4)


# extracting densities 
Den_gct = results$plot_maps_args$Report$D_gct
extr_grid_coords = results$plot_maps_args$PlotDF

year.out = seq(2011,2021,1)
for (s in 1:21){
  for (y in 1:11){
    extr.den.temp = data.frame(species = rep(common.names[s],nrow(extr_grid_coords)),
                               year = rep(year.out[y],nrow(extr_grid_coords)),
                               Longitude = extr_grid_coords$Lon,
                               Latitude = extr_grid_coords$Lat,
                               density = Den_gct[extr_grid_coords$x2i,s,y])
    if(y==1){
      Extra.den.y = extr.den.temp
    }else{
      Extra.den.y = rbind(Extra.den.y,extr.den.temp)
    }
  }
  write.csv(Extra.den.y,paste0(common.names[s],'.den.csv',sep=''))
  # if(s==1){
  #   Extra.den = Extra.den.y
  # }else{
  #   Extra.den = rbind(Extra.den,Extra.den.y)
  # }
}

















