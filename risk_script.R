#============================================================
# clear memory
#============================================================
rm(list = ls())

#============================================================
# load libraries
#============================================================
library(plyr)
library(ggplot2)
library(scales)
library(lubridate)
library(zoo)
library(fields)
library(raster)
library(maptools)
library(spatstat) 
library(raster)
library(ncdf)
library(RNetCDF)
library(ncdf.tools)
library(fields)
library(colorRamps) 
library(animation)
library(RcppRoll)
library(rworldmap)

#============================================================
# read data
#============================================================

#==========
# set directory
setwd("C:/Users/RenkeLuehken/Google Drive/projects/reserach_projects/mosquito_ecology/risk_maps_albopictus")

#==========
# read temperature dataset  "min_temperature_germany_2006_2016" "Germany_Temperature_2006_2016"
suborder<- "min/"
suborder<- "mean/"

temp_data_raster <- stack("D:/NeuAll/projects/reserach_projects/general_data/climate/min_temperature_germany_2006_2016.grd")
temp_data_raster <- stack("D:/NeuAll/projects/reserach_projects/general_data/climate/Germany_Temperature_2006_2016.grd")
temp_data_raster <- stack("C:/Users/RenkeLuehken/Google Drive/projects/reserach_projects/general_data/climate/Germany_Temperature_2006_2016.grd")

#==========
# read shape file
state.map <- readShapeSpatial("D:/NeuAll/projects/reserach_projects/general_data/country_shapes/Germany/DEU_adm1")
state.map <- readShapeSpatial("C:/Users/RenkeLuehken/Google Drive/projects/reserach_projects/general_data/country_shapes/Germany/DEU_adm1")

#==========
# gps coordinates

# St. Georgen, black forest
gps_schwarzwald <- matrix(nrow = 1, ncol = 2)
gps_schwarzwald[1, 1] <- 8.334848
gps_schwarzwald[1, 2] <- 48.128923

# Freiburg
gps_freiburg <- matrix(nrow = 1, ncol = 2)
gps_freiburg[1, 1] <- 7.842104
gps_freiburg[1, 2] <- 47.999008

# Heidelberg
gps_heidelberg <- matrix(nrow = 1, ncol = 2)
gps_heidelberg[1, 1] <- 8.672434
gps_heidelberg[1, 2] <- 49.398752

#////////////////////////////////////////////////////////////
# read data

# convert temperature raster to a matrix (col = timesteps, row = cell)
temp_data_matrix <- getValues(temp_data_raster)

# time
A1 <- paste(2006, "-01-01", sep = "")
A2 <- paste(2016, "-12-31", sep = "")
time.s <- as.POSIXct(A1, tz = 'UTC')
time.e <- as.POSIXct(A2, tz = 'UTC')
tseq = seq(time.s, time.e, by = '24 hours')

temp_blackforest <- extract(temp_data_raster, gps_schwarzwald)
temp_heidelberg <- extract(temp_data_raster, gps_heidelberg)
temp_freiburg <- extract(temp_data_raster, gps_freiburg)

data <- data.frame(date = tseq[3561:3743], 
                   temp = c(as.numeric(temp_blackforest[3561:3712]),
                            as.numeric(temp_heidelberg[3713:3743])),
                   site = c(rep("St. Georgen", length(as.numeric(temp_blackforest[3561:3712]))),
                            rep("Heidelberg", length(as.numeric(temp_blackforest[3713:3743])))),
                   group = "field experiment")
data2 <- data.frame(date = tseq[3561:3743], 
                    temp = as.numeric(temp_heidelberg[3561:3743]),
                    site = "Heidelberg",
                    group = "Aedes albopictus populations")
data3 <- data.frame(date = tseq[3561:3743], 
                    temp = as.numeric(temp_freiburg[3561:3743]),
                    site = "Freiburg",
                    group = "Aedes albopictus populations")
datafull <- rbind(data, data2, data3)

datafull$site <- factor(datafull$site, levels=c("St. Georgen", "Heidelberg", "Freiburg"))

png(file = "figs/min_temps.jpg",width = 10, height=5, units = 'in', res = 500)
ggplot(datafull, aes(x = date, y = temp, group = site)) +
  geom_point(aes(colour = site)) +
  geom_line(aes(colour = site))+
  xlab("date")+
  ylab("temperature [?C]") +
  geom_hline(yintercept = 0, linetype = 2) +
  facet_wrap(~group, ncol = 2) +
  theme_bw()
dev.off()


##
datafull_2 <- data
##
png(file = "figs/min_temps_2.jpg",width = 10, height=5, units = 'in', res = 500)
ggplot(datafull_2, aes(x = date, y = temp)) +
  geom_rect(xmin = as.numeric(datafull_2$date[52]), 
            xmax = as.numeric(datafull_2$date[59]), ymin = -Inf,
            ymax = Inf, alpha = 0.5,fill = "lightgray") +
  geom_rect(xmin = as.numeric(datafull_2$date[105]), 
            xmax = as.numeric(datafull_2$date[114]), ymin = -Inf,
            ymax = Inf, alpha = 0.5,fill = "lightgray") +
  geom_rect(xmin = as.numeric(datafull_2$date[127]), 
            xmax = as.numeric(datafull_2$date[128]), ymin = -Inf,
            ymax = Inf, alpha = 0.5,fill = "lightgray") +
  geom_rect(xmin = as.numeric(datafull_2$date[133]), 
            xmax = as.numeric(datafull_2$date[135]), ymin = -Inf,
            ymax = Inf, alpha = 0.5,fill = "lightgray") +
  geom_rect(xmin = as.numeric(datafull_2$date[138]), 
            xmax = as.numeric(datafull_2$date[140]), ymin = -Inf,
            ymax = Inf, alpha = 0.5,fill = "lightgray") +
  geom_rect(xmin = as.numeric(datafull_2$date[142]), 
            xmax = as.numeric(datafull_2$date[143]), ymin = -Inf,
            ymax = Inf, alpha = 0.5,fill = "lightgray") +
  geom_rect(xmin = as.numeric(datafull_2$date[147]), 
            xmax = as.numeric(datafull_2$date[150]), ymin = -Inf,
            ymax = Inf, alpha = 0.5,fill = "lightgray") +
  geom_rect(xmin = as.numeric(datafull_2$date[152]), 
            xmax = as.numeric(datafull_2$date[153]), ymin = -Inf,
            ymax = Inf, alpha = 0.5,fill = "lightgray") +
  geom_line(aes(colour = site))+
  geom_ribbon(data=subset(datafull_2, 
                          date >= as.numeric(datafull_2$date[105]) & date <=as.numeric(datafull_2$date[114])),
              aes(x=date,ymax=temp),ymin=0,fill="red")+
  geom_point(colour = "black") +
  geom_point(data=subset(datafull_2, temp <0), 
             aes(x=date, y=temp), colour="blue")+
  geom_point(data=subset(datafull_2, temp == min(temp)), 
             aes(x=date, y=temp), colour="blue", size = 5)+
  #geom_ribbon(data=subset(datafull_2, temp<0),
  #            aes(x=date,ymax=temp),ymin=0,fill="blue", alpha=0.5)
  #geom_area(data=subset(datafull_2, temp>=0), fill="pink") +
  #geom_area(data=subset(datafull_2, temp<0), fill="lightblue")+
  xlab("date")+
  ylab("temperature [?C]") +
  #scale_color_manual(guide=FALSE, values=c("red", "black")) + #turn off the legend, define the colors
  geom_hline(yintercept = 0, linetype = 2) +
  geom_vline(xintercept= c(as.numeric(datafull_2$date[93]), 
                           as.numeric(datafull_2$date[123])),
             colour = "green", size = 2)+
  theme_bw()
dev.off()

source("R/mean_specific_month.R")
source("R/matrix_to_raster.R")

# Schwarzwald - beginning October 2015 - end February 2016 + 
# Heidelberg - from beginning of March, first larvae April 2016
temp_experimental <- c(temp_blackforest[,1:3712], temp_heidelberg[,3713:4018])

experimental_mean_jan <- mean_specific_month(temp_experimental, tseq, 1, 1, "mean")[10]
experimental_min <- mean_specific_month(temp_experimental, tseq, 10, 3, "min")[10]
experimental_n_min_values <- mean_specific_month(temp_experimental, tseq, 10, 3, "n_min_values")[10]
experimental_variability <- mean_specific_month(temp_experimental, tseq, 10, 3, "variability")[10]
experimental_min_in_row <- mean_specific_month(temp_experimental, tseq, 10, 3, "min_in_row")[10]
experimental_sum_mins <- mean_specific_month(temp_experimental, tseq, 10, 3, "sum_mins")[10]

#============================================================
# calculations for temperature timeseries
#============================================================

#==========
# read temperature dataset
res_mean <- apply(temp_data_matrix, 1, function(x) 
  mean_specific_month(x, tseq, 1, 1, "mean"))
res_min <- apply(temp_data_matrix, 1, function(x) 
  mean_specific_month(x, tseq, 10, 3, "min"))
res_variability<-apply(temp_data_matrix, 1, function(x) 
  mean_specific_month(x, tseq, 10, 3, "variability"))
res_count_min_in_row<-apply(temp_data_matrix, 1, function(x) 
  mean_specific_month(x, tseq, 10, 3, "min_in_row"))
res_n_min_values <- apply(temp_data_matrix, 1, function(x) 
  mean_specific_month(x, tseq, 10, 3, "n_min_values"))
res_sum_mins <- apply(temp_data_matrix, 1, function(x) 
  mean_specific_month(x, tseq, 10, 3, "sum_mins"))


#==========
# convert matrix to raster
res_mean_2 <- matrix_to_raster(res_mean, temp_data_raster[[1]])
res_min_2 <- matrix_to_raster(res_min, temp_data_raster[[1]])
res_variability_2 <- matrix_to_raster(res_variability, temp_data_raster[[1]])
res_count_min_in_row_2 <- matrix_to_raster(res_count_min_in_row, temp_data_raster[[1]])
res_n_min_values_2 <- matrix_to_raster(res_n_min_values, temp_data_raster[[1]])
res_sum_mins_2 <- matrix_to_raster(res_sum_mins, temp_data_raster[[1]])


#x <- seq(1,24,1)
#plot(x, 0.75*log(x)-11.18, ylim=c(-12, 0))
#0.75*log(24)-11.18
#////////////////////////////////////////////////////////////
# calculations for temperature timeseries


#==================================================
# plotting risk maps
#==================================================
# comparison x and x
# colour legend
brks<-seq(0,10)
A <- blue2green2red(11)

png(file = paste("figs/", suborder, "comparison_Becker_vs_Bayreuth.jpg", sep = ""),
    width = 5, height=10, units = 'in', res = 500)
par(mfrow=c(2,1))
plot(mask(sum(res_min_2 >= experimental_min, na.rm=T), state.map), 
     main = paste("Becker: minimum temperature >=", round(experimental_min, 2), "°C"),
     col = A,breaks=brks)
points(gps_freiburg, col = "red", cex = 2, lwd = 3)
points(gps_heidelberg, col = "red", cex = 2, lwd = 3)
plot(state.map,add=T)


plot(mask(sum(res_min_2 >= ifelse(suborder == "min/", -11.18, -8.8), na.rm=T), state.map), 
     main = paste("Bayreuth: minimum temperature >=", 
                  ifelse(suborder == "min/", -11.18, -8.8), "°C"),
     col = A, breaks=brks)
points(gps_freiburg, col = "red", cex = 2, lwd = 3)
points(gps_heidelberg, col = "red", cex = 2, lwd = 3)
plot(state.map,add=T)
dev.off()


#==========
# risk of overwintering for timeseries for each of the six variables

# colour legend
brks<-seq(0,10)
A <- blue2green2red(11)

# plot
png(file = paste("figs/", suborder, "rsk_2007_2016.jpg", sep = ""),
    width = 7, height=10, units = 'in', res = 500)
par(mfrow=c(3,2))
plot(mask(sum(res_mean_2 >= experimental_mean_jan, na.rm=T), state.map), 
     main = paste("mean January temperature >=", round(experimental_mean_jan, 2), "°C"),
     col = A,breaks=brks)
points(gps_freiburg, col = "red", cex = 2, lwd = 3)
points(gps_heidelberg, col = "red", cex = 2, lwd = 3)
plot(state.map,add=T)
plot(mask(sum(res_min_2 >= experimental_min, na.rm=T), state.map), 
     main = paste("minimum temperature >=", round(experimental_min, 2), "°C"),
     col = A,breaks=brks)
points(gps_freiburg, col = "red", cex = 2, lwd = 3)
points(gps_heidelberg, col = "red", cex = 2, lwd = 3)
plot(state.map,add=T)
plot(mask(sum(res_n_min_values_2 <= experimental_n_min_values, na.rm=T), state.map), 
     main = paste("days below zero <=", round(experimental_n_min_values), "days"),
     col = A,breaks=brks)
points(gps_freiburg, col = "red", cex = 2, lwd = 3)
points(gps_heidelberg, col = "red", cex = 2, lwd = 3)
plot(state.map,add=T)
plot(mask(sum(res_count_min_in_row_2 <= experimental_min_in_row, na.rm=T), state.map), 
     main = paste("consecutive days below zero <=", round(experimental_min_in_row), "days"),
     col = A,breaks=brks)
points(gps_freiburg, col = "red", cex = 2, lwd = 3)
points(gps_heidelberg, col = "red", cex = 2, lwd = 3)
plot(state.map,add=T)
plot(mask(sum(res_sum_mins_2 >= experimental_sum_mins, na.rm=T), state.map), 
     main = paste("minimum sum of temperatures >=", round(experimental_sum_mins)),
     col = A,breaks=brks)
points(gps_freiburg, col = "red", cex = 2, lwd = 3)
points(gps_heidelberg, col = "red", cex = 2, lwd = 3)
plot(state.map,add=T)
plot(mask(sum(res_variability_2 <= experimental_variability, na.rm=T), state.map), 
     main = paste("temperature variability <=", round(experimental_variability), "periods"),
     col = A,breaks=brks)
points(gps_freiburg, col = "red", cex = 2, lwd = 3)
points(gps_heidelberg, col = "red", cex = 2, lwd = 3)
plot(state.map,add=T)
dev.off()

#==========
# mean risk of overwintering over the timeseries

# calculate mean
gft_full <- mean((res_mean_2 >= experimental_mean_jan)+
                   (res_min_2 >= experimental_min)+
                   (res_n_min_values_2 <= experimental_n_min_values)+
                   (res_count_min_in_row_2 <= experimental_min_in_row)+
                   (res_sum_mins_2 >= experimental_sum_mins)+
                   (res_variability_2 <= experimental_variability), na.rm = T)

# colour legend
brks2<-seq(0,6)
A2 <- blue2green2red(length(brks2))

# plot
png(file = paste("figs/", suborder, "sum_rsk_2007_2016.jpg", sep = ""),
    width = 5, height=5, units = 'in', res = 500)
plot(mask(gft_full, state.map), 
     col = A2,
     breaks = brks2)
points(gps_freiburg, col = "red", cex = 2, lwd = 3)
points(gps_heidelberg, col = "red", cex = 2, lwd = 3)
plot(state.map,add=T)
dev.off()

#==========
# risk of overwintering for specific year for each of the six variables
png(file = paste("figs/", suborder, "min_rsk_2016.jpg", sep = ""),
    width = 7, height=10, units = 'in', res = 500)
par(mfrow=c(3,2))
plot(mask(res_mean_2[[10]], state.map) >= experimental_mean_jan, 
     main = paste("mean January temperature >=", round(experimental_mean_jan, 2), "°C"),
     col = c("white",blue2green2red(11)[11]),
     legend = F)
points(gps_freiburg, cex = 2, lwd = 3)
points(gps_heidelberg, cex = 2, lwd = 3)
plot(state.map,add=T)
plot(mask(res_min_2[[10]], state.map) >= experimental_min, 
     main = paste("minimum temperature >=", round(experimental_min, 2), "°C"),
     col = c("white",blue2green2red(11)[11]),
     legend = F)
points(gps_freiburg, cex = 2, lwd = 3)
points(gps_heidelberg, cex = 2, lwd = 3)
plot(state.map,add=T)
plot(mask(res_n_min_values_2[[10]], state.map) <= experimental_n_min_values, 
     main = paste("days below zero <=", round(experimental_n_min_values), "days"),
     col = c("white",blue2green2red(11)[11]),
     legend = F)
points(gps_freiburg, cex = 2, lwd = 3)
points(gps_heidelberg, cex = 2, lwd = 3)
plot(state.map,add=T)
plot(mask(res_count_min_in_row_2[[10]], state.map) <= experimental_min_in_row, 
     main = paste("consecutive days below zero <=", round(experimental_min_in_row), "days"),
     col = c("white",blue2green2red(11)[11]),
     legend = F)
points(gps_freiburg, cex = 2, lwd = 3)
points(gps_heidelberg, cex = 2, lwd = 3)
plot(state.map,add=T)
plot(mask(res_sum_mins_2[[10]], state.map) >= experimental_sum_mins, 
     main = paste("minimum sum of temperatures >=", round(experimental_sum_mins)),
     col = c("white",blue2green2red(11)[11]),
     legend = F)
points(gps_freiburg, cex = 2, lwd = 3)
points(gps_heidelberg, cex = 2, lwd = 3)
plot(state.map,add=T)
plot(mask(res_variability_2[[10]], state.map) <= experimental_variability, 
     main = paste("temperature variability <=", round(experimental_variability), "periods"),
     col = c("white",blue2green2red(11)[11]),
     legend = F)
points(gps_freiburg, cex = 2, lwd = 3)
points(gps_heidelberg, cex = 2, lwd = 3)
plot(state.map,add=T)
dev.off()

#==========
# sum risk of overwintering over the risk variables
sum_risk_variables <- (res_mean_2[[10]] >= experimental_mean_jan)+
  (res_min_2[[10]] >= experimental_min)+
  (res_n_min_values_2[[10]] <= experimental_n_min_values)+
  (res_count_min_in_row_2[[10]] <= experimental_min_in_row)+
  (res_sum_mins_2[[10]] >= experimental_sum_mins)+
  (res_variability_2[[10]] <= experimental_variability)

plot(res_n_min_values_2)

# colour legend
brks2<-seq(0,6)
A2 <- blue2green2red(7)

# plot
png(file = paste("figs/", suborder, "sum_rsk_2016.jpg", sep = ""),
    width = 5, height=5, units = 'in', res = 500)
plot(mask(sum_risk_variables, state.map),
     col = A2,
     breaks = brks2)
points(gps_freiburg, cex = 2, lwd = 3)
points(gps_heidelberg, cex = 2, lwd = 3)
plot(state.map,add=T)
dev.off()

#////////////////////////////////////////////////////////////
# plotting risk maps