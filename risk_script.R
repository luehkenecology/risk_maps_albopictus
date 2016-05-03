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
require(lubridate)
library(zoo)
require(fields)
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
# read temperature dataset
brick_all_tg1 <- stack("D:/NeuAll/projects/reserach_projects/general_data/climate/Germany_Temperature_2006_2016.grd")

#==========
# gps coordinates

# Schwarzwald - beginning October 2015 - end February 2016
gps_schwarzwald <-matrix(nrow=1,ncol=2)
gps_schwarzwald[1,1]<-8.334848
gps_schwarzwald[1,2]<-48.128923

# Freibuerg - from beginning of March, first larvae April 2016
gps_freiburg <-matrix(nrow=1,ncol=2)
gps_freiburg[1,1]<-7.842104
gps_freiburg[1,2]<-47.999008

gps_heidelberg <-matrix(nrow=1,ncol=2)
gps_heidelberg[1,1]<-8.672434
gps_heidelberg[1,2]<-49.398752

#////////////////////////////////////////////////////////////
# read data

dfdfdf1 <- getValues(brick_all_tg1)



# Time
A1 <- paste(2006, "-01-01", sep = "")
A2 <- paste(2016, "-12-31", sep = "")
time.s <- as.POSIXct(A1, tz = 'UTC')
time.e <- as.POSIXct(A2, tz = 'UTC')
tseq = seq(time.s, time.e, by = '24 hours')

dfdf <- extract(brick_all_tg1, gps_schwarzwald)
dfdf_2 <- extract(brick_all_tg1, gps_heidelberg)
dfdf_3 <- extract(brick_all_tg1, gps_freiburg)

data <- data.frame(date = tseq[3561:3743], 
                   temp = c(as.numeric(dfdf[3561:3712]),
                            as.numeric(dfdf_2[3713:3743])),
                   site = c(rep("St. Georgen", length(as.numeric(dfdf[3561:3712]))),
                            rep("Heidelberg", length(as.numeric(dfdf[3713:3743])))),
                   group = "field experiment")
data2 <- data.frame(date = tseq[3561:3743], 
                    temp = as.numeric(dfdf_2[3561:3743]),
                    site = "Heidelberg",
                    group = "Aedes albopictus populations")
data3 <- data.frame(date = tseq[3561:3743], 
                    temp = as.numeric(dfdf_3[3561:3743]),
                    site = "Freiburg",
                    group = "Aedes albopictus populations")
datafull <- rbind(data, data2, data3)

#dtt$model <- factor(dtt$model, levels=c("mb", "ma", "mc"), labels=c("MBB", "MAA", "MCC"))
datafull$site <- factor(datafull$site, levels=c("St. Georgen", "Heidelberg", "Freiburg"))

png(file = "figs/temps.jpg",width = 10, height=5, units = 'in', res = 500)
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
png(file = "figs/temps_2.jpg",width = 10, height=5, units = 'in', res = 500)
ggplot(datafull_2, aes(x = date, y = temp)) +
  #annotate("rect", xmin = as.numeric(datafull_2$date[93]), 
  #         xmax = as.numeric(datafull_2$date[112]), ymin = -Inf,
  #        ymax = Inf,
  #      fill = "blue", alpha = .5, color = NA)+
  #geom_rect(xmin = as.numeric(datafull_2$date[93]), 
  #          xmax = as.numeric(datafull_2$date[123]), ymin = -Inf,
  #          ymax = Inf, alpha = 0.5,fill = "green") +
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

field_t<-c(as.numeric(dfdf[3561:3712]),
           as.numeric(dfdf_2[3713:3743]))

# average mean
mean(as.numeric(dfdf[3653:3683]))
mean(as.numeric(dfdf_2[3653:3683]))
mean(as.numeric(dfdf_3[3653:3683]))

# minimum temperature
min(as.numeric(dfdf[3653:3683]))
min(as.numeric(dfdf_2[3653:3683]))
min(as.numeric(dfdf_3[3653:3683]))

# number of days below zero
sum(ifelse(field_t<0, 1, 0))
sum(ifelse(as.numeric(dfdf_2[3561:3743])<0, 1, 0))
sum(ifelse(as.numeric(dfdf_3[3561:3743])<0, 1, 0))

# number of days below zero in a row
maxi <- max(rle(field_t < 0)$length[rle(field_t < 0)$values])
maxi <- max(rle(as.numeric(dfdf_2[3561:3743]) < 0)$length[rle(as.numeric(dfdf_2[3561:3683]) < 0)$values])
maxi <- max(rle(as.numeric(dfdf_3[3561:3743]) < 0)$length[rle(as.numeric(dfdf_3[3561:3683]) < 0)$values])

# variability
length(rle(field_t < 0)$length)
length(rle(as.numeric(dfdf_2[3561:3743]) < 0)$length)
length(rle(as.numeric(dfdf_3[3561:3743]) < 0)$length)

#
gtunc <- function(x){
  #Your rle code
  run <- rle(x < 0)
  maxi <- max(rle(x < 0)$length[rle(x < 0)$values])
  
  #Add columns for the length of the run and the value of that run
  dat1 <- rep(run$lengths, run$lengths)
  dat2 <- rep(run$values, run$lengths)
  
  dat <- cbind(x, dat1, dat2)
  
  #Subset the data based on the length and value
  sum(subset(dat, dat1 ==maxi & dat2)[,1])
}

gtunc(as.numeric(dfdf_3[3561:3743]))

# October-Apri=0, May-September=1


january<-ifelse(month(tseq)<2,1,0)
#SUBS2[3653:3683]


#==================================================
# calculation
#==================================================
aedes_min <- function(x, time_start, time_end){
  temp <- as.numeric(x)
  
  testfull <- data.frame(year = year(tseq), tempen=temp)
  
  # mean january temperature
  testfull_2 <-data.frame(testfull, time_start, time_end)
  
  testfull_2_sub<-subset(testfull_2, time_start > 0 | time_end < 0)
  testfull_2_sub$new_year <- testfull_2_sub$year + testfull_2_sub$smaller_march
  
  testfull3_b<-subset(testfull_2_sub,new_year>2005 & new_year<2016)
  
  einsd<-ddply(testfull3_b,.(new_year),summarize,sf=min(tempen, na.rm=T))
  
  einsd$sf
}

aedes_count_min <- function(x){
  temp<-as.numeric(x)
  
  testfull<-data.frame(year=year(tseq),tempen=temp)
  
  # mean january temperature
  testfull_2 <-data.frame(testfull, larger_september, smaller_march)
  
  testfull_2_sub<-subset(testfull_2, larger_september>0 |smaller_march<0)
  testfull_2_sub$new_year <- testfull_2_sub$year + testfull_2_sub$smaller_march
  
  testfull3_b<-subset(testfull_2_sub,new_year>2005 & new_year<2016)
  
  einsd<-ddply(testfull3_b,.(new_year),summarize,sf=sum(ifelse(tempen<0, 1,0)))
  
  einsd$sf
}

count_min_in_row <- function(x){
  temp<-as.numeric(x)
  
  testfull<-data.frame(year=year(tseq),tempen=temp)
  
  # mean january temperature
  testfull_2 <-data.frame(testfull, larger_september, smaller_march)
  
  testfull_2_sub<-subset(testfull_2, larger_september>0 |smaller_march<0)
  testfull_2_sub$new_year <- testfull_2_sub$year + testfull_2_sub$smaller_march
  
  testfull3_b<-subset(testfull_2_sub,new_year>2005 & new_year<2016)
  
  einsd<-ddply(testfull3_b,.(new_year),
               summarize,
               sf=max(rle(tempen < 0)$length[rle(tempen < 0)$values]))
  
  einsd$sf
}

variability <- function(x){
  temp<-as.numeric(x)
  
  testfull<-data.frame(year=year(tseq),tempen=temp)
  
  # mean january temperature
  testfull_2 <-data.frame(testfull, larger_september, smaller_march)
  
  testfull_2_sub<-subset(testfull_2, larger_september>0 |smaller_march<0)
  testfull_2_sub$new_year <- testfull_2_sub$year + testfull_2_sub$smaller_march
  
  testfull3_b<-subset(testfull_2_sub,new_year>2005 & new_year<2016)
  
  einsd<-ddply(testfull3_b,.(new_year),
               summarize,
               sf=length(rle(tempen < 0)$length))
  
  einsd$sf
}

min_sum <- function(x){
  temp<-as.numeric(x)
  
  testfull<-data.frame(year=year(tseq),tempen=temp)
  
  # mean january temperature
  testfull_2 <-data.frame(testfull, larger_september, smaller_march)
  
  testfull_2_sub<-subset(testfull_2, larger_september>0 |smaller_march<0)
  testfull_2_sub$new_year <- testfull_2_sub$year + testfull_2_sub$smaller_march
  
  testfull3_b<-subset(testfull_2_sub,new_year>2005 & new_year<2016)
  
  einsd<-ddply(testfull3_b,.(new_year),
               summarize,
               sf=gtunc(tempen))
  
  einsd$sf
}



aedes_mean <- function(x){
  temp<-as.numeric(x)
  testfull<-data.frame(year=year(tseq),tempen=temp)
  
  # mean january temperature
  testfull2<-data.frame(testfull,january)
  testfull3<-subset(testfull2, testfull2[,3]>0)
  testfull3_b<-subset(testfull3,testfull3[,1]>2006)
  einsd<-ddply(testfull3_b,.(year),summarize,sf=mean(tempen, na.rm=T))
  
  einsd$sf
}

source("R/minimum_time_row.R")
source("R/mean_specific_month.R")


res_mean <- apply(dfdfdf1, 1, function(x) mean_specific_month(x, tseq, 12, 2, "mean"))
res_min <- apply(dfdfdf1, 1, function(x) mean_specific_month(x, tseq, 9, 4, "min"))
res_variability<-apply(dfdfdf1, 1, function(x) 
  mean_specific_month(x, tseq, 9, 4, "variability"))
res_count_min_in_row<-apply(dfdfdf1, 1, function(x) 
  mean_specific_month(x, tseq, 9, 4, "max_in_row"))
res_n_min_values <- apply(dfdfdf1, 1, function(x) 
  mean_specific_month(x, tseq, 9, 4, "n_min_values"))

res_n_min_values_2 <- t(res_n_min_values)
res_n_min_values_3 <- lapply(1:ncol(res_n_min_values_2), function(x) 
  setValues(brick_all_tg1[[1]],res_n_min_values_2[,x]))
res_n_min_values_4 <- brick(unlist(res_n_min_values_3))

res_mean_2<-t(res_mean)
res_min_2<-t(res_min)
res_count_min_2<-t(res_count_min)
res_count_min_in_row_2<-t(res_count_min_in_row)
res_variability_2<-t(res_variability)

res_mean_3<-lapply(1:ncol(res_mean_2), function(x) 
  setValues(brick_all_tg1[[1]],res_mean_2[,x]))
res_min_3<-lapply(1:ncol(res_min_2), function(x) 
  setValues(brick_all_tg1[[1]],res_min_2[,x]))
res_count_min_3<-lapply(1:ncol(res_count_min_2), function(x) 
  setValues(brick_all_tg1[[1]],res_count_min_2[,x]))
res_count_min_in_row_3<-lapply(1:ncol(res_count_min_in_row_2), function(x) 
  setValues(brick_all_tg1[[1]],res_count_min_in_row_2[,x]))
res_variability_3<-lapply(1:ncol(res_variability_2), function(x) 
  setValues(brick_all_tg1[[1]],res_variability_2[,x]))
res_res_min_sum_3<-lapply(1:ncol(res_res_min_sum_2), function(x) 
  setValues(brick_all_tg1[[1]],res_res_min_sum_2[,x]))

res_mean_4<-brick(unlist(res_mean_3))
res_min_4<-brick(unlist(res_min_3))
res_count_min_4<-brick(unlist(res_count_min_3))
res_count_min_in_row_4<-brick(unlist(res_count_min_in_row_3))
res_variability_4<-brick(unlist(res_variability_3))
res_res_min_sum_4<-brick(unlist(res_res_min_sum_3))

#==================================================
# save raster
#==================================================
# transform to raster
plot(res_mean_4 >= 0.2519355)
plot(res_min_4 >= -10.5)
plot(res_min_4 >= -10.5)

plot(res_count_min_4 <=31)
plot(res_count_min_in_row_4<=10)
plot(res_variability_4<=17)
plot(res_res_min_sum_4>=-45.4)

state.map <- readShapeSpatial("D:/NeuAll/projects/reserach_projects/general_data/country_shapes/Germany/DEU_adm1")

brks<-seq(0,10)
A <- blue2green2red(11)

png(file = "figs/rsk_2007_2016.jpg",width = 7, height=10, units = 'in', res = 500)
par(mfrow=c(3,2))
plot(mask(sum(res_mean_4 >= 0.2519355, na.rm=T), state.map), 
     main = "mean January temperature >= 0.25?C",
     col = A,breaks=brks)
points(gps_freiburg, col = "red", cex = 2, lwd = 3)
points(gps_heidelberg, col = "red", cex = 2, lwd = 3)
plot(state.map,add=T)
plot(mask(sum(res_min_4 >= -10.5, na.rm=T), state.map), 
     main = "minimum temperature >= -10.5?C",col = A,breaks=brks)
points(gps_freiburg, col = "red", cex = 2, lwd = 3)
points(gps_heidelberg, col = "red", cex = 2, lwd = 3)
plot(state.map,add=T)
plot(mask(sum(res_mean_4 <= 31, na.rm=T), state.map), 
     main = "days below zero <= 31 days",col = A,breaks=brks)
points(gps_freiburg, col = "red", cex = 2, lwd = 3)
points(gps_heidelberg, col = "red", cex = 2, lwd = 3)
plot(state.map,add=T)
plot(mask(sum(res_count_min_in_row_4 <= 10, na.rm=T), state.map), 
     main = "consecutive days below zero <= 10 days",col = A,breaks=brks)
points(gps_freiburg, col = "red", cex = 2, lwd = 3)
points(gps_heidelberg, col = "red", cex = 2, lwd = 3)
plot(state.map,add=T)
plot(mask(sum(res_res_min_sum_4 >= -45.4, na.rm=T), state.map), 
     main = "minimum sum of temperatures >= -45.4",col = A,breaks=brks)
points(gps_freiburg, col = "red", cex = 2, lwd = 3)
points(gps_heidelberg, col = "red", cex = 2, lwd = 3)
plot(state.map,add=T)
plot(mask(sum(res_variability_4 <= 17, na.rm=T), state.map), 
     main = "temperature variability <= 17",col = A,breaks=brks)
points(gps_freiburg, col = "red", cex = 2, lwd = 3)
points(gps_heidelberg, col = "red", cex = 2, lwd = 3)
plot(state.map,add=T)
dev.off()

gft_full <- (res_mean_4 >= 0.2519355)+
  (res_min_4 >= -10.5)+
  (res_count_min_4 <= 31)+
  (res_count_min_in_row_4 <= 10)+
  (res_res_min_sum_4 >= -45.4)+
  (res_variability_4 <= 17)
plot(mean(gft_full))

brks2<-seq(0,6)
A2 <- blue2green2red(length(brks2))

png(file = "figs/sum_rsk_2007_2016.jpg",
    width = 5, height=5, units = 'in', res = 500)
plot(mask(mean(gft_full), state.map), 
     col = A2,
     breaks = brks2)
points(gps_freiburg, col = "red", cex = 2, lwd = 3)
points(gps_heidelberg, col = "red", cex = 2, lwd = 3)
plot(state.map,add=T)
dev.off()

######
png(file = "figs/rsk_2016.jpg",width = 7, height=10, units = 'in', res = 500)
par(mfrow=c(3,2))
plot(mask(res_mean_4[[10]], state.map) >= 0.2519355, 
     main = "mean January temperature >= 0.25?C",
     col = c("white",blue2green2red(11)[11]),
     legend = F)
points(gps_freiburg, cex = 2, lwd = 3)
points(gps_heidelberg, cex = 2, lwd = 3)
plot(state.map,add=T)
plot(mask(res_min_4[[10]], state.map) >= -10.5, 
     main = "minimum temperature >= -10.5?C",
     col = c("white",blue2green2red(11)[11]),
     legend = F)
points(gps_freiburg, cex = 2, lwd = 3)
points(gps_heidelberg, cex = 2, lwd = 3)
plot(state.map,add=T)
plot(mask(res_count_min_4[[10]], state.map) <= 31, 
     main = "days below zero <= 31 days",
     col = c("white",blue2green2red(11)[11]),
     legend = F)
points(gps_freiburg, cex = 2, lwd = 3)
points(gps_heidelberg, cex = 2, lwd = 3)
plot(state.map,add=T)
plot(mask(res_count_min_in_row_4[[10]], state.map) <= 10, 
     main = "consecutive days below zero <= 10 days",
     col = c("white",blue2green2red(11)[11]),
     legend = F)
points(gps_freiburg, cex = 2, lwd = 3)
points(gps_heidelberg, cex = 2, lwd = 3)
plot(state.map,add=T)
plot(mask(res_res_min_sum_4[[10]], state.map) >= -45.4, 
     main = "minimum sum of temperatures >= -45.4",
     col = c("white",blue2green2red(11)[11]),
     legend = F)
points(gps_freiburg, cex = 2, lwd = 3)
points(gps_heidelberg, cex = 2, lwd = 3)
plot(state.map,add=T)
plot(mask(res_variability_4[[10]], state.map) <= 17, 
     main = "temperature variability <= 17",
     col = c("white",blue2green2red(11)[11]),
     legend = F)
points(gps_freiburg, cex = 2, lwd = 3)
points(gps_heidelberg, cex = 2, lwd = 3)
plot(state.map,add=T)
dev.off()

gft <- (res_mean_4[[10]] >= 0.2519355)+
  (res_min_4[[10]] >= -10.5)+
  (res_count_min_4[[10]] <= 31)+
  (res_count_min_in_row_4[[10]] <= 10)+
  (res_res_min_sum_4[[10]] >= -45.4)+
  (res_variability_4[[10]] <= 16)

brks2<-seq(0,6)
A2 <- blue2green2red(7)

png(file = "figs/sum_rsk_2016_2.jpg",
    width = 5, height=5, units = 'in', res = 500)
plot(mask(gft, state.map),
     col = A2,
     breaks = brks2)
points(gps_freiburg, cex = 2, lwd = 3)
points(gps_heidelberg, cex = 2, lwd = 3)
plot(state.map,add=T)
dev.off()





















plot(D)
A <- 
  B <- (sum(res_min_4 >= -10.5, na.rm = T))
C <- (sum(res_count_min_4 <=18, na.rm =T))
D <- (sum(res_count_min_in_row_4<=10, na.rm=T))
E <- (sum(res_variability_4<=16, na.rm=T))

t <- (res_mean_4 >= 0.2519355)+
  (res_min_4 >= -10.5)+
  (res_count_min_4 <=18)+
  (res_count_min_in_row_4<=10)
plot(t)

plot((res_mean_4 >= 0.2519355) + (res_min_4 >= -10.5))
plot(sum((res_mean_4 >= 0.2519355) + (res_min_4 >= -10.5)))

plot(mean(res_mean_4, na.rm = T) >= 0.2519355)
plot(mean(res_min_4, na.rm = T) >= -10.5)
plot((mean(res_mean_4, na.rm=T) >= 0.2519355) + (mean(res_min_4, na.rm=T) >= -10.5))

plot(sum(FinalPopulation >= -10.5))
plot(FinalPopulation[[4]])
plot((FinalPopulation[[11]])>= -10.5)
plot(mean(FinalPopulation, na.rm = T)>= -10.5)

-10.5
writeRaster(FinalPopulation,paste("figs/rr",y,".grd",sep=""),overwrite=T)
rm(FinalPopulation,res,res2)

#==================================================
# print progress
#==================================================
print(y)


#==================================================
# calculation
#==================================================
ZIKA28<-function(x){
  DDU<-as.numeric(x)
  DDU_130_days<-roll_sum(DDU,n=14,fill=0,align="right", na.rm = T)
  dfdf<-ifelse(DDU_130_days>=392,1,0)
  testfull<-data.frame(year=year(tseq),DDU=dfdf)
  einsd<-ddply(testfull,.(year),summarize,sf=sum(DDU, na.rm=T))
  einsd$sf
}

TemperaturCellsAll <- getValues(brick_all_tg1)

#df<-ZIKA(as.numeric(TemperaturCellsAll[10000,]))
res<-apply(TemperaturCellsAll, 1, function(x) ZIKA28(x))

res2<-t(res)

#==================================================
# save raster
#==================================================
# transform to raster
testst<-lapply(1:ncol(res2), function(x) setValues(brick_all_tg1[[1]],res2[,x]))
FinalPopulation<-brick(unlist(testst))
writeRaster(mean(FinalPopulation, na.rm=T), "figs/ZIKA_28.grd", overwrite=T)

ZIKA25<-function(x){
  DDU<-as.numeric(x)
  DDU_130_days<-roll_sum(DDU,n=14,fill=0,align="right", na.rm = T)
  dfdf<-ifelse(DDU_130_days>=350,1,0)
  testfull<-data.frame(year=year(tseq),DDU=dfdf)
  einsd<-ddply(testfull,.(year),summarize,sf=sum(DDU, na.rm=T))
  einsd$sf
}

TemperaturCellsAll <- getValues(brick_all_tg1)

#df<-ZIKA(as.numeric(TemperaturCellsAll[10000,]))
res<-apply(TemperaturCellsAll, 1, function(x) ZIKA25(x))

res2<-t(res)

#==================================================
# save raster
#==================================================
# transform to raster
testst<-lapply(1:ncol(res2), function(x) setValues(brick_all_tg1[[1]],res2[,x]))
FinalPopulation<-brick(unlist(testst))
writeRaster(mean(FinalPopulation, na.rm=T), "figs/ZIKA_25.grd", overwrite=T)

tet<-raster("figs/ZIKA_28.grd")
tet2 <- crop(tet, extent(-11, 30, 36, 50))
plot(tet2)
colfunc2<-colorRampPalette(c("deepskyblue4", 
                             "darkorange1",
                             "firebrick2"))
brks = c(0, seq(1,111.7))
A<-c("white",colfunc2(length(brks)))

plot(tet2,ylim=c(35, 50), xlim=c(-11, 35),col = A,breaks=brks,
     legend = F)
#plot(menno>0,ylim=c(35, 50), xlim=c(-11, 35))
newmap <- getMap(resolution = "low")
plot(newmap,add=T)



EINS<-mean(raster("figs/rr1.grd"),na.rm=T)
ZWEI<-mean(raster("figs/rr2.grd"),na.rm=T)
DREI<-mean(raster("figs/rr3.grd"),na.rm=T)
VIER<-mean(raster("figs/rr4.grd"),na.rm=T)
FUNF<-mean(raster("figs/rr5.grd"),na.rm=T)
SECH<-mean(raster("figs/rr6.grd"),na.rm=T)


plot(newmap,add=T)

png(file = "EuropeDI.jpg",width = 5.63, height=9, units = 'in', res = 500)
par(mfrow=c(3,2))
plot(EINS,ylim=c(33, 65), xlim=c(-11, 35))
plot(newmap, ylim=c(33, 65), xlim=c(-11, 35),add=T)
plot(ZWEI,ylim=c(33, 65), xlim=c(-11, 35))
plot(newmap, ylim=c(33, 65), xlim=c(-11, 35),add=T)
plot(DREI,ylim=c(33, 65), xlim=c(-11, 35))
plot(newmap, ylim=c(33, 65), xlim=c(-11, 35),add=T)
plot(VIER,ylim=c(33, 65), xlim=c(-11, 35))
plot(newmap, ylim=c(33, 65), xlim=c(-11, 35),add=T)
plot(FUNF,ylim=c(33, 65), xlim=c(-11, 35))
plot(newmap, ylim=c(33, 65), xlim=c(-11, 35),add=T)
plot(SECH,ylim=c(33, 65), xlim=c(-11, 35))
plot(newmap, ylim=c(33, 65), xlim=c(-11, 35),add=T)
dev.off()









#============================================================
# save the figures in a gif
#============================================================
#state.map <- readShapeSpatial("data/DEU_adm1.shp")

#rr <- mask(FinalPopulation, state.map)
rr1 <- raster("figs/rr1.grd")
rr2 <- raster("figs/rr2.grd")
rr3 <- raster("figs/rr3.grd")
rr4 <- raster("figs/rr4.grd")
rr5 <- raster("figs/rr5.grd")
rr6 <- raster("figs/rr6.grd")

rrStack<-stack(rr1,rr2,rr3,rr4,rr5,rr6)

NESFS<-read.csv("MyData.csv")

brks <- as.vector(NESFS$werts)
brks[length(brks)]<-200
brks[1]<-0
A<-as.vector(NESFS$col)

#brks <- c(0,40,75,200)
#A<-matlab.like2(length(brks))

summus<-c("1956-1965","1966-1975","1976-1985","1986-1995","1996-2005","2006-2014")
#summus<-seq(1,11,1)

#colfunc1<-colorRampPalette(c("white","red"))
#colfunc2<-colorRampPalette(c("blue","white","red"))

#brks<-c(0,10,35,40,70,200)
#A<-c(colfunc1(length(brks)))

rclmat <- data.frame(eins=brks[-c(length(brks))],zwei=brks[-c(1)],drei=NESFS[-c(1),2])

brks <- as.vector(NESFS[,2])


rc1 <- reclassify(rrStack[[1]], rclmat)
rc2 <- reclassify(rrStack[[2]], rclmat)
rc3 <- reclassify(rrStack[[3]], rclmat)
rc4 <- reclassify(rrStack[[4]], rclmat)
rc5 <- reclassify(rrStack[[5]], rclmat)
rc6 <- reclassify(rrStack[[6]], rclmat)


png(file = "figs/world2.jpg",width = 7, height=10, units = 'in', res = 500)
par(mfrow=c(3,2))
plot(rc1,col = A,breaks=brks,main=paste(summus[1]),legend=F)
plot(state.map,add=T)
plot(rc2,col = A,breaks=brks,main=paste(summus[2]),legend=F)
plot(state.map,add=T)
plot(rc3,col = A,breaks=brks,main=paste(summus[3]),legend=F)
plot(state.map,add=T)
plot(rc4,col = A,breaks=brks,main=paste(summus[4]),legend=F)
plot(state.map,add=T)
plot(rc5,col = A,breaks=brks,main=paste(summus[5]),legend=F)
plot(state.map,add=T)
plot(rc6,col = A,breaks=brks,main=paste(summus[6]),legend=F)
plot(state.map,add=T)
dev.off()












#
teststWIRKLICH<-lapply(1:ncol(vierzig), function(x) setValues(brick_all_tg1[[1]],vierzig[,x]))
FinalPopulationWIRKLICH<-brick(unlist(teststWIRKLICH))











###
years<-seq(1956,2014,1)

GanzDeutschland<-data.frame(year=as.numeric(years),mean=colMeans(res2,na.rm=T))

png(file = "figs/Deutschland.jpg",width = 5, height=5, units = 'in', res = 500)
ggplot(GanzDeutschland,aes(x=as.numeric(year),y=as.numeric(mean)))+
  geom_line()+
  stat_smooth(method = "lm")+
  xlab("year (1956:2014)")+
  ylab("total of days")+
  theme_bw()
dev.off()

#
allows<-ifelse(res2>0,1,0)
allows2<-na.omit(allows)
GanzDeutschlanddf<-data.frame(year=as.numeric(years),mean=apply(allows2,2,function(x) sum(x)*100/length(x)))

png(file = "figs/Deutschlandallows.jpg",width = 5, height=5, units = 'in', res = 500)
ggplot(GanzDeutschlanddf,aes(x=as.numeric(year),y=as.numeric(mean)))+
  geom_line()+
  stat_smooth(method = "lm")+
  xlab("year (1956:2014)")+
  ylab("percentage of the area allowing development")+
  theme_bw()
dev.off()



state.map <- readShapeSpatial("data/DEU_adm1.shp")

FinalPopulation2<-lapply(1:16,function(x) crop(FinalPopulation,state.map[c(x),]))
FinalPopulation4<-lapply(FinalPopulation2,function(x) getValues(x))

FinalPopulation5<-lapply(1:16,function(x) data.frame(FinalPopulation4[[x]],Name=state.map$NAME_1[x]))

FinalPopulation6<-do.call(rbind,FinalPopulation5)

FinalPopulation6$Name

dimnames(FinalPopulation6)[[2]]<-c(as.character(years),"name")


days<-as.numeric()
for(i in 1:49){
  days<-c(days,FinalPopulation6[,i])
}

year<-as.character()
for(i in 1:49){
  year<-c(year,rep(colnames(FinalPopulation6)[i],nrow(FinalPopulation6)))
}

Sample.ID<-rep(FinalPopulation6$name,49)

FULL<-data.frame(Sample.ID,year,days)

FULL2<-FULL
FULL2$minDays<-ifelse(FULL$days>0,1,0)
percentageAllowSubmission<-ddply(FULL2,.(Sample.ID,year),summarize,perc=sum(minDays,na.rm=T)*100/length(minDays))

png(file = "figs/percentageAllowSubmission.jpg",width = 7, height=7, units = 'in', res =500)
ggplot(percentageAllowSubmission,aes(x=as.numeric(year),y=as.numeric(perc)))+
  geom_line()+
  stat_smooth(method = "lm")+
  xlab("year (1956:2014)")+
  ylab("percentage of the area allowing development")+
  scale_y_continuous(limits = c(0, 100))+
  facet_wrap(~Sample.ID,scale="free_y")+
  theme_bw()
dev.off()


FULLmean<-ddply(FULL,.(Sample.ID,year),summarize,mean=mean(days,na.rm=T))

mods = dlply(FULLmean, .(Sample.ID), lm, formula = mean~as.numeric(year))
ldply(mods, anova)
ldply(mods, coef)



png(file = "figs/Bundeslnder.jpg",width = 7, height=7, units = 'in', res = 500)
ggplot(FULLmean,aes(x=as.numeric(year),y=as.numeric(mean)))+
  geom_line()+
  stat_smooth(method = "lm")+
  xlab("year (1956:2014)")+
  ylab("total of days")+
  facet_wrap(~Sample.ID,scale="free_y")+
  theme_bw()
dev.off()


#============================================================
# save the figures in a gif
#============================================================
state.map <- readShapeSpatial("data/DEU_adm1.shp")

rr <- mask(FinalPopulation, state.map)
rr1 <- mean(mask(FinalPopulation[[1:10]], state.map),na.rm=T)
rr2 <- mean(mask(FinalPopulation[[11:20]], state.map),na.rm=T)
rr3 <- mean(mask(FinalPopulation[[21:30]], state.map),na.rm=T)
rr4 <- mean(mask(FinalPopulation[[31:40]], state.map),na.rm=T)
rr5 <- mean(mask(FinalPopulation[[41:50]], state.map),na.rm=T)
rr6 <- mean(mask(FinalPopulation[[51:59]], state.map),na.rm=T)


rrStack<-stack(rr1,rr2,rr3,rr4,rr5,rr6)

NESFS<-read.csv("MyData.csv")

brks <- as.vector(NESFS$werts)
brks[length(brks)]<-200
brks[1]<-0
A<-as.vector(NESFS$col)

#brks <- c(0,40,75,200)
#A<-matlab.like2(length(brks))

summus<-c("1956-1965","1966-1975","1976-1985","1986-1995","1996-2005","2006-2014")
#summus<-seq(1,11,1)

#colfunc1<-colorRampPalette(c("white","red"))
#colfunc2<-colorRampPalette(c("blue","white","red"))

#brks<-c(0,10,35,40,70,200)
#A<-c(colfunc1(length(brks)))


?reclassify
m <- c(0, 0.25, 1,  0.25, 0.5, 2,  0.5, 1, 3)
rclmat <- data.frame(eins=brks[-c(length(brks))],zwei=brks[-c(1)],drei=NESFS[-c(1),2])

brks <- as.vector(NESFS[,2])


rc1 <- reclassify(rrStack[[1]], rclmat)
rc2 <- reclassify(rrStack[[2]], rclmat)
rc3 <- reclassify(rrStack[[3]], rclmat)
rc4 <- reclassify(rrStack[[4]], rclmat)
rc5 <- reclassify(rrStack[[5]], rclmat)
rc6 <- reclassify(rrStack[[6]], rclmat)

##
library(rworldmap)
newmap <- getMap(resolution = "low")

plot(newmap,
     xlim = range(europe.limits$lon),
     ylim = range(europe.limits$lat),
     asp = 1
)

##
newmap2<-crop(newmap,c(-20, 59,30, 71))
rc3<-crop(rc,c(-20, 59,30, 71))

png(file = "figs/world2.jpg",width = 7, height=10, units = 'in', res = 500)
par(mfrow=c(3,2))
plot(rc1,col = A,breaks=brks,main=paste(summus[1]),legend=F)
plot(state.map,add=T)
plot(rc2,col = A,breaks=brks,main=paste(summus[2]),legend=F)
plot(state.map,add=T)
plot(rc3,col = A,breaks=brks,main=paste(summus[3]),legend=F)
plot(state.map,add=T)
plot(rc4,col = A,breaks=brks,main=paste(summus[4]),legend=F)
plot(state.map,add=T)
plot(rc5,col = A,breaks=brks,main=paste(summus[5]),legend=F)
plot(state.map,add=T)
plot(rc6,col = A,breaks=brks,main=paste(summus[6]),legend=F)
plot(state.map,add=T)
dev.off()




#============================================================
# impact of years with possibility of transmission
#============================================================
state.map <- readShapeSpatial("data/DEU_adm1.shp")

NESFS<-read.csv("MyData.csv")

table(brks)
brks <- as.vector(NESFS$val)
brks[length(brks)]<-0
brks[1]<-200
A<-as.vector(NESFS$col)

Re1 <- mean(mask(FinalPopulationWIRKLICH[[1:10]], state.map),na.rm=T)
Re2 <- mean(mask(FinalPopulationWIRKLICH[[11:20]], state.map),na.rm=T)
Re3 <- mean(mask(FinalPopulationWIRKLICH[[21:30]], state.map),na.rm=T)
Re4 <- mean(mask(FinalPopulationWIRKLICH[[31:40]], state.map),na.rm=T)
Re5 <- mean(mask(FinalPopulationWIRKLICH[[41:50]], state.map),na.rm=T)
Re6 <- mean(mask(FinalPopulationWIRKLICH[[51:59]], state.map),na.rm=T)

png(file = "figs/combine.jpg",width = 7, height=10, units = 'in', res = 500)
par(mfrow=c(3,2))
plot(mean(rc1,Re1,na.rm=T),col = A,breaks=brks,main=paste(summus[1]),legend=F)
plot(state.map,add=T)
plot(mean(rc2,Re2,na.rm=T),col = A,breaks=brks,main=paste(summus[2]),legend=F)
plot(state.map,add=T)
plot(mean(rc3,Re3,na.rm=T),col = A,breaks=brks,main=paste(summus[3]),legend=F)
plot(state.map,add=T)
plot(mean(rc4,Re4,na.rm=T),col = A,breaks=brks,main=paste(summus[4]),legend=F)
plot(state.map,add=T)
plot(mean(rc5,Re5,na.rm=T),col = A,breaks=brks,main=paste(summus[5]),legend=F)
plot(state.map,add=T)
plot(mean(rc6,Re6,na.rm=T),col = A,breaks=brks,main=paste(summus[6]),legend=F)
plot(state.map,add=T)
dev.off()

brks <- as.vector(NESFS[,2])
brks[length(brks)]<-1
brks[1]<-0

g1<-plot(mean(rc6,FinalPopulationWIRKLICH[[6]],na.rm=T))
plot(g1,col = A,breaks=brks,main=paste(summus[6]),legend=F)

mean(0.7,0.5)

=
  
  
  
  
  
  # interval of gif
  ATR<-rep(0.5,ncol(res2)) # 0.00000001 secs between each picture
ATR[length(ATR)]<-2 # last picture stays for 2 secs


saveGIF({
  for( ii in seq(1,ncol(res2),1)) {
    par(mar = c(3, 3, 3,1), mfrow = c(1,1),cex=0.9) 
    plot(rr[[ii]],col = A,breaks=round(brks),main=as.character(years[ii]))
    plot(state.map,add=T)
  }},
  movie.name = "Diro.gif", interval = ATR, ani.width = 600, 
  ani.height = 600
)












ldply(TemperaturCellsAll[1000:1002,], function(x) DDU130(x))
plot(DDU130(TemperaturCellsAll[1000,]))

?vapply
TemperaturCells <- vapply(unique(colnames(TemperaturCells)), function(x) 
  rowMeans(TemperaturCells[,colnames(TemperaturCells)== x,drop=FALSE], na.rm=TRUE),
  numeric(nrow(TemperaturCells)))



sum(SUBS[1:365])

dayVar<-yday(tseq)




library(RcppRoll)
?roll_sum


ddd<-roll_mean(as.numeric(TemperaturCells[300,]),n=30,fill=0)
ddd2<-ddd-14
ddd<-roll_sum(ddd2,n=30,fill=0)
dfdf<-ifelse(ddd>130,1,0)
testfull<-data.frame(year=year(tseq),DDU=dfdf)
ddply(testfull,.(year),summarize,sf=sum(DDU))



RRCells<-getValues(brick_all_rr)

colnames(TemperaturCells)<-dayVar
colnames(RRCells)<-dayVar

TemperaturCells <- vapply(unique(colnames(TemperaturCells)), function(x) 
  rowMeans(TemperaturCells[,colnames(TemperaturCells)== x,drop=FALSE], na.rm=TRUE),
  numeric(nrow(TemperaturCells)))
RRCells <- vapply(unique(colnames(RRCells)), function(x) 
  rowMeans(RRCells[,colnames(RRCells)== x,drop=FALSE], na.rm=TRUE),
  numeric(nrow(RRCells)) )

Emean<-matrix(nrow=nrow(TemperaturCells),ncol=ncol(TemperaturCells))
Lmean<-matrix(nrow=nrow(TemperaturCells),ncol=ncol(TemperaturCells))
Pmean<-matrix(nrow=nrow(TemperaturCells),ncol=ncol(TemperaturCells))
Aemmean<-matrix(nrow=nrow(TemperaturCells),ncol=ncol(TemperaturCells))
A1hmean<-matrix(nrow=nrow(TemperaturCells),ncol=ncol(TemperaturCells))
A1gmean<-matrix(nrow=nrow(TemperaturCells),ncol=ncol(TemperaturCells))
A10mean<-matrix(nrow=nrow(TemperaturCells),ncol=ncol(TemperaturCells))
A2hmean<-matrix(nrow=nrow(TemperaturCells),ncol=ncol(TemperaturCells))
A2gmean<-matrix(nrow=nrow(TemperaturCells),ncol=ncol(TemperaturCells))
A20mean<-matrix(nrow=nrow(TemperaturCells),ncol=ncol(TemperaturCells))


SUBS<-ifelse(month(tseq)>9,0,(ifelse(month(tseq)<4,0,1)))

for(g in 1:nrow(TemperaturCells)){
  Temperatur<-as.numeric(TemperaturCells[g,])
  Temperatur[is.na(Temperatur)] <- 0
  NIEDERSCHLAGSHOEHE<-as.numeric(RRCells[g,])
  NIEDERSCHLAGSHOEHE[is.na(NIEDERSCHLAGSHOEHE)] <- 0
  NIEDERSCHLAGSHOEHE2<-rollapply(NIEDERSCHLAGSHOEHE, 14, sum)
  
  beta1 <- 95 #Number of eggs laid by ovipositing nulliparous females (per female)  
  beta2 <- 75 #Number of eggs laid by ovipositing parous females (per female)		
  KL <- 250000 #Standard environment carrying capacity for larvae (larvae ha???1)	
  KP <- 250000 #Standard environment carrying capacity for pupae (pupae ha???1)
  delta <- 0.5 #Sex-ratio at the emergence	
  #muE <- 0.05 #Egg mortality rate (day???1)
  muE <- ifelse(Temperatur>0,0.05,ifelse(Temperatur>(-5),0.5,ifelse(Temperatur>(-10),0.75,1))) #Egg mortality rate (day???1)
  muL <- 0.08 #Minimum larva mortality rate (day???1)
  muP <- 0.03 #Minimum pupa mortality rate (day???1)  
  muem <- 0.1 #Mortality rate during adult emergence (day???1)	
  muA <- 0.02 # Minimum adult mortality rate (day???1)	
  mur  <- 0.08 #Adult mortality rate related to seeking behavior (day???1)	
  TE <- 10.4 # Minimal temperature needed for egg development (?C)		
  TDDE <-110# Total number of degree-day necessary for egg development (?C)	
  gammaAem <- 0.4 # Development rate of emerging adults (day???1)	
  gammaAh <-0.2# Transition rate from host-seeking to engorged adults (day???1)
  gammaAo <-0.2 #Transition rate from oviposition site-seeking to host-seeking adults (day???1)	
  TAg <-10# Minimal temperature needed for egg maturation (?C)	
  TDDAg <-77 #Total number of degree-days necessary for egg maturation (?C)	
  tstart <-1003#  Start of the favorable season	
  tend <-3009  # End of the favorable season	
  
  fE <- ifelse(Temperatur>TE,(Temperatur-TE)/TDDE,0)
  fL <- -0.0007*Temperatur^2 + 0.0392*Temperatur - 0.3911  #Transition function from larva to pupa	
  fP <-  0.0008*Temperatur^2 - 0.0051*Temperatur + 0.0319
  fAg <- ifelse(Temperatur>TAg,(Temperatur-TE)/TDDAg,0)
  
  mLt = exp(-Temperatur/2) + muL
  mPt = exp(-Temperatur/2) + muP
  mAt = ifelse(0.04417 + 0.00217*Temperatur<=muA,muA,0.04417 + 0.00217*Temperatur)
  
  newvalue= (NIEDERSCHLAGSHOEHE2-min(NIEDERSCHLAGSHOEHE2))/
    (max(NIEDERSCHLAGSHOEHE2)-min(NIEDERSCHLAGSHOEHE2))
  KLt = KL * (newvalue+1)
  KPt = KP * (newvalue+1)
  
  A10<-A20<-E<-L<-P<-Aem<-A1h<-A1g<-A10<-A2h<-A2g<-rep(0,length(Temperatur))
  
  E<-10^6
  
  for(i in 2:length(Temperatur)){
    z<-SUBS[i]
    E[i]   <- ifelse(E[i-1]+gammaAo*(beta1*A10[i]+beta2*A20[i])-(muE+z*fE[i])*E[i-1]>=1,
                     E[i-1]+gammaAo*(beta1*A10[i-1]+beta2*A20[i-1])-(muE+z*fE[i-1])*E[i-1],0)
    L[i]   <- ifelse(L[i-1]+ z*fE[i]*E[i]-(mLt[i]*(1+L[i-1]/KL)+fL[i])*L[i-1]>=1,
                     L[i-1]+ z * fE[i]*E[i]-(mLt[i]*(1+L[i-1]/KL)+fL[i])*L[i-1],0)
    P[i]   <- ifelse(P[i-1]+fL[i]*L[i-1]-(mPt[i]+fP[i])*P[i-1]>=1,
                     P[i-1]+fL[i]*L[i-1]-(mPt[i]+fP[i])*P[i-1],0)
    Aem[i] <- ifelse(Aem[i-1]+fP[i]*P[i]*delta*exp(-muem*(1+P[i]/KP))-(mAt[i]*gammaAem)*Aem[i-1]>=1,
                     Aem[i-1]+fP[i]*P[i]*delta*exp(-muem*(1+P[i]/KP))-(mAt[i]*gammaAem)*Aem[i-1],0)
    A1h[i] <- ifelse(A1h[i-1]+gammaAem*Aem[i]-(mAt[i]+mur+gammaAh)*A1h[i-1]>=1,
                     A1h[i-1]+gammaAem*Aem[i]-(mAt[i]+mur+gammaAh)*A1h[i-1],0)
    A1g[i] <- ifelse(A1g[i-1]+gammaAh*A1h[i]-(mAt[i]+fAg[i])*A1g[i-1]>=1,
                     A1g[i-1]+gammaAh*A1h[i]-(mAt[i]+fAg[i])*A1g[i-1],0)
    A10[i] <- ifelse(A10[i-1]+fAg[i]*A1g[i]-(mAt[i]+mur+gammaAo)*A10[i-1]>=1,
                     A10[i-1]+fAg[i]*A1g[i]-(mAt[i]+mur+gammaAo)*A10[i-1],0)
    A2h[i] <- ifelse(A2h[i-1]+gammaAo*(A10[i]+A20[i-1])-(mAt[i]+mur+gammaAh)*A2h[i-1]>=1,
                     A2h[i-1]+gammaAo*(A10[i]+A20[i-1])-(mAt[i]+mur+gammaAh)*A2h[i-1],0)
    A2g[i] <- ifelse(A2g[i-1]+gammaAh* A2h[i]-(mAt[i]+fAg[i])*A2g[i-1]>=1,
                     A2g[i-1]+gammaAh* A2h[i]-(mAt[i]+fAg[i])*A2g[i-1],0)
    A20[i] <- ifelse(A20[i-1]+fAg[i]*  A2g[i]-(mAt[i]+mur+gammaAo)*A20[i-1]>=1,
                     A20[i-1]+fAg[i]*  A2g[i]-(mAt[i]+mur+gammaAo)*A20[i-1],0)
    
    #print(c(E[i], L[i],P[i],Aem[i],A1h[i],A1g[i], A10[i],A2h[i],A2g[i],A20[i]))
  }
  
  Emean[g,]<- E
  Lmean[g,]<- L
  Pmean[g,]<- P
  Aemmean[g,]<- Aem
  A1hmean[g,]<- A1h
  A1gmean[g,]<- A1g
  A10mean[g,]<- A10
  A2hmean[g,]<- A2h
  A2gmean[g,]<- A2g
  A20mean[g,]<- A20
}

#============================================================
# years of interest: Rainfall- and temperature-driven abundance model for Aedes albopictus
#============================================================
brick_all_tg<-stack("figs/Italy_Temperature_2012_2015.grd")
brick_all_rr<-stack("figs/Italy_Rainfall_2012_2015.grd")

# Time
A1<-paste(2012, "-01-01", sep = "")
A2<-paste(2015, "-07-31", sep = "")
time.s=as.POSIXct(A1,tz='UTC')
time.e=as.POSIXct(A2,tz='UTC')
tseq=seq(time.s, time.e, by='24 hours')

TemperaturCells<-getValues(brick_all_tg)
RRCells<-getValues(brick_all_rr)

SumFrame<-matrix(nrow=nrow(TemperaturCells),ncol=ncol(TemperaturCells))
HostSeek<-matrix(nrow=nrow(TemperaturCells),ncol=ncol(TemperaturCells))
SumEGG<-matrix(nrow=nrow(TemperaturCells),ncol=ncol(TemperaturCells))

SUBS1<-ifelse(month(tseq)>9,0,(ifelse(month(tseq)<4,0,1)))
SUB2<-ifelse(month(tseq)>9,0,(ifelse(month(tseq)<4,0,1)))
SUBS<-c(SUBS1,SUB2)

for(g in 1:nrow(TemperaturCells)){
  Temperatur<-as.numeric(TemperaturCells[g,])
  Temperatur[is.na(Temperatur)] <- 0
  NIEDERSCHLAGSHOEHE<-as.numeric(RRCells[g,])
  NIEDERSCHLAGSHOEHE[is.na(NIEDERSCHLAGSHOEHE)] <- 0
  NIEDERSCHLAGSHOEHE2<-rollapply(NIEDERSCHLAGSHOEHE, 14, sum)
  
  beta1 <- 95 #Number of eggs laid by ovipositing nulliparous females (per female)  
  beta2 <- 75 #Number of eggs laid by ovipositing parous females (per female)		
  KL <- 250000 #Standard environment carrying capacity for larvae (larvae ha???1)	
  KP <- 250000 #Standard environment carrying capacity for pupae (pupae ha???1)
  delta <- 0.5 #Sex-ratio at the emergence	
  #muE <- 0.05 #Egg mortality rate (day???1)
  muE <- ifelse(Temperatur>0,0.05,ifelse(Temperatur>(-5),0.5,ifelse(Temperatur>(-10),0.75,1))) #Egg mortality rate (day???1)
  muL <- 0.08 #Minimum larva mortality rate (day???1)
  muP <- 0.03 #Minimum pupa mortality rate (day???1)  
  muem <- 0.1 #Mortality rate during adult emergence (day???1)	
  muA <- 0.02 # Minimum adult mortality rate (day???1)	
  mur  <- 0.08 #Adult mortality rate related to seeking behavior (day???1)	
  TE <- 10.4 # Minimal temperature needed for egg development (?C)		
  TDDE <-110# Total number of degree-day necessary for egg development (?C)	
  gammaAem <- 0.4 # Development rate of emerging adults (day???1)	
  gammaAh <-0.2# Transition rate from host-seeking to engorged adults (day???1)
  gammaAo <-0.2 #Transition rate from oviposition site-seeking to host-seeking adults (day???1)	
  TAg <-10# Minimal temperature needed for egg maturation (?C)	
  TDDAg <-77 #Total number of degree-days necessary for egg maturation (?C)	
  tstart <-1003#  Start of the favorable season	
  tend <-3009  # End of the favorable season	
  
  fE <- ifelse(Temperatur>TE,(Temperatur-TE)/TDDE,0)
  fL <- -0.0007*Temperatur^2 + 0.0392*Temperatur - 0.3911  #Transition function from larva to pupa	
  fP <-  0.0008*Temperatur^2 - 0.0051*Temperatur + 0.0319
  fAg <- ifelse(Temperatur>TAg,(Temperatur-TE)/TDDAg,0)
  
  mLt = exp(-Temperatur/2) + muL
  mPt = exp(-Temperatur/2) + muP
  mAt = ifelse(0.04417 + 0.00217*Temperatur<=muA,muA,0.04417 + 0.00217*Temperatur)
  
  newvalue= (NIEDERSCHLAGSHOEHE2-min(NIEDERSCHLAGSHOEHE2))/
    (max(NIEDERSCHLAGSHOEHE2)-min(NIEDERSCHLAGSHOEHE2))
  KLt = KL * (newvalue+1)
  KPt = KP * (newvalue+1)
  
  A10<-A20<-E<-L<-P<-Aem<-A1h<-A1g<-A10<-A2h<-A2g<-rep(0,length(Temperatur))
  
  E[1]<-Emean[g,366]
  L[1]<-Lmean[g,366]
  P[1]<-Pmean[g,366]
  Aem[1]<-Aemmean[g,366]
  A1h[1]<-A1hmean[g,366]
  A1g[1]<-A1gmean[g,366]
  A10[1]<-A10mean[g,366]
  A2h[1]<-A2hmean[g,366]
  A2g[1]<-A2gmean[g,366]
  A20[1]<-A20mean[g,366]
  
  
  for(i in 2:length(Temperatur)){
    z<-SUBS[i]
    E[i]   <- ifelse(E[i-1]+gammaAo*(beta1*A10[i]+beta2*A20[i])-(muE+z*fE[i])*E[i-1]>=1,
                     E[i-1]+gammaAo*(beta1*A10[i-1]+beta2*A20[i-1])-(muE+z*fE[i-1])*E[i-1],0)
    L[i]   <- ifelse(L[i-1]+ z*fE[i]*E[i]-(mLt[i]*(1+L[i-1]/KL)+fL[i])*L[i-1]>=1,
                     L[i-1]+ z * fE[i]*E[i]-(mLt[i]*(1+L[i-1]/KL)+fL[i])*L[i-1],0)
    P[i]   <- ifelse(P[i-1]+fL[i]*L[i-1]-(mPt[i]+fP[i])*P[i-1]>=1,
                     P[i-1]+fL[i]*L[i-1]-(mPt[i]+fP[i])*P[i-1],0)
    Aem[i] <- ifelse(Aem[i-1]+fP[i]*P[i]*delta*exp(-muem*(1+P[i]/KP))-(mAt[i]*gammaAem)*Aem[i-1]>=1,
                     Aem[i-1]+fP[i]*P[i]*delta*exp(-muem*(1+P[i]/KP))-(mAt[i]*gammaAem)*Aem[i-1],0)
    A1h[i] <- ifelse(A1h[i-1]+gammaAem*Aem[i]-(mAt[i]+mur+gammaAh)*A1h[i-1]>=1,
                     A1h[i-1]+gammaAem*Aem[i]-(mAt[i]+mur+gammaAh)*A1h[i-1],0)
    A1g[i] <- ifelse(A1g[i-1]+gammaAh*A1h[i]-(mAt[i]+fAg[i])*A1g[i-1]>=1,
                     A1g[i-1]+gammaAh*A1h[i]-(mAt[i]+fAg[i])*A1g[i-1],0)
    A10[i] <- ifelse(A10[i-1]+fAg[i]*A1g[i]-(mAt[i]+mur+gammaAo)*A10[i-1]>=1,
                     A10[i-1]+fAg[i]*A1g[i]-(mAt[i]+mur+gammaAo)*A10[i-1],0)
    A2h[i] <- ifelse(A2h[i-1]+gammaAo*(A10[i]+A20[i-1])-(mAt[i]+mur+gammaAh)*A2h[i-1]>=1,
                     A2h[i-1]+gammaAo*(A10[i]+A20[i-1])-(mAt[i]+mur+gammaAh)*A2h[i-1],0)
    A2g[i] <- ifelse(A2g[i-1]+gammaAh* A2h[i]-(mAt[i]+fAg[i])*A2g[i-1]>=1,
                     A2g[i-1]+gammaAh* A2h[i]-(mAt[i]+fAg[i])*A2g[i-1],0)
    A20[i] <- ifelse(A20[i-1]+fAg[i]*  A2g[i]-(mAt[i]+mur+gammaAo)*A20[i-1]>=1,
                     A20[i-1]+fAg[i]*  A2g[i]-(mAt[i]+mur+gammaAo)*A20[i-1],0)
    
    #print(c(E[i], L[i],P[i],Aem[i],A1h[i],A1g[i], A10[i],A2h[i],A2g[i],A20[i]))
  }
  
  sdfasfd<-A1h+A1g+A10+A2h+A2g+A20
  SumFrame[g,]<-sdfasfd
  HostSeek[g,]<-A20
  SumEGG[g,]<-E
}

#============================================================
#  weighting mechanistic model with correlative distribution model
#============================================================
#
dfdf<-raster("data/AedesItalyPA.grd")
y <- aggregate(dfdf, floor(res(brick_all_rr) / res(dfdf)), fun=mean)
# then resample
z <- resample(dfdf, brick_all_rr, method='bilinear')
plot(z)

z2<-getValues(z)

z2[is.na(z2)]<-0
HostSeek[is.na(HostSeek)]<-0

VALUIS<-(apply(HostSeek, 2, function(x) x*z2))
VALUISMean<-colMeans(VALUIS)

plot(VALUISMean,type="l")



testst<-lapply(1:ncol(TemperaturCells), function(x) setValues(brick_all_tg[[1]],SumFrame[,x]))
FinalPopulation<-brick(unlist(testst))

teststEGG<-lapply(1:ncol(TemperaturCells), function(x) setValues(brick_all_tg[[1]],SumEGG[,x]))
FinalPopulationEGG<-brick(unlist(teststEGG))

#============================================================
#  Rom
#============================================================
tzer<-matrix(nrow=1,ncol=2)
tzer[1,1]<-12.496366
tzer[1,2]<-41.902783

#============================================================
#  Freiburg
#============================================================
#tzer<-matrix(nrow=1,ncol=2)
#tzer[1,1]<-7.842104
#tzer[1,2]<-47.999008

dfdf<-extract(FinalPopulation,tzer)
dfdfEGG<-extract(FinalPopulationEGG,tzer)

TempFrei<-extract(brick_all_tg,tzer)
RainFrei<-extract(brick_all_rr,tzer)

timesNew<-c(timesfull,timesfull2015)

RAIN<-data.frame(date=tseq,value=as.numeric(RainFrei))
TEMP<-data.frame(date=tseq,value=as.numeric(TempFrei))
POP<-data.frame(date=tseq,value=as.numeric(dfdf))
POPEGG<-data.frame(date=tseq,value=as.numeric(dfdfEGG))
VALUISMean2<-data.frame(date=tseq,value=as.numeric(VALUISMean))
#plot(as.numeric(dfdfEGG)[1:300])

pT <- ggplot()+theme_bw()+
  geom_bar(data=RAIN, aes(x=date, y=value,colour="red"),stat="identity")+
  geom_line(data=TEMP, aes(x=date, y=value,colour="blue"))+
  theme_bw()+xlab("")+ ylab("temperature and rainfall")+
  guides(colour=FALSE)
pPOP <- ggplot(VALUISMean2, aes(x=date, y=value))+
  geom_line()+theme_bw()+ ylab("adults")

gp1<- ggplot_gtable(ggplot_build(pT))
gp2<- ggplot_gtable(ggplot_build(pPOP))
maxWidth = unit.pmax(gp1$widths[2:3], gp2$widths[2:3])
gp1$widths[2:3] <- maxWidth
gp2$widths[2:3] <- maxWidth


library(gridExtra)
library(grid)
library(ggplot2)
library(gridBase)

grid.arrange(gp2, gp1)

png(file = "PopulationDynamicMeanItaly.png",width = 6, height=10, units = 'in', res = 1000)
grid.arrange(gp2, gp1)
dev.off()


#============================================================
# save the figures in a gif
#============================================================
state.map <- readShapeSpatial("data/ITA_adm1")


brks <- seq(1,max(as.matrix(FinalPopulation)),by=max(as.matrix(FinalPopulation))/100)
brks <- seq(0,max(as.matrix(FinalPopulation)),by=max(as.matrix(FinalPopulation))/10)
brks <- seq(0,max(as.matrix(FinalPopulation)),by=max(as.matrix(FinalPopulation))/10)

A<-matlab.like(length(brks))
A[1]<-"white"

rr <- mask(FinalPopulation, state.map)

# interval of gif
ATR<-rep(0.00000001,length(seq(1,ncol(TemperaturCells),10))) # 0.00000001 secs between each picture
ATR[length(ATR)]<-2 # last picture stays for 2 secs

saveGIF({
  for( ii in seq(1,ncol(TemperaturCells),10)) {
    par(mar = c(3, 3, 3,1), mfrow = c(1,1),cex=0.9) 
    plot(rr[[ii]],col = A,breaks=round(brks),main=as.character(tseq[ii]))
    plot(state.map,add=T)
  }},
  movie.name = "AsianTigerMosquioDynamicItaly.gif", interval = ATR, ani.width = 600, 
  ani.height = 600
)

lowerx <- as.POSIXct(strftime(min(RAIN$date),"%Y-%m-%d"))
upperx <- as.POSIXct(strftime(max(RAIN$date),"%Y-%m-%d"))
limitsx<-c(lowerx,upperx)

saveGIF({
  for( ii in seq(1,ncol(TemperaturCells),10)) {
    pT <- ggplot()+theme_bw()+
      geom_bar(data=RAIN[1:ii,], aes(x=as.POSIXct(date), y=value,colour="red"),stat="identity")+
      geom_line(data=TEMP[1:ii,], aes(x=as.POSIXct(date), y=value,colour="blue"))+
      theme_bw()+xlab("")+ ylab("temperature and rainfall")+
      scale_y_continuous(limits=c(ifelse(min(TEMP$value,na.rm=T)>0.00,0.00,min(TEMP$value,na.rm=T)),max(RAIN$value,na.rm=T)))+
      guides(colour=FALSE)+     
      scale_x_datetime(limits=c(limitsx))
    pPOP <- ggplot(VALUISMean2[1:ii,], aes(x=as.POSIXct(date), y=value))+
      geom_line()+theme_bw()+ xlab("")+ylab("adults")+
      scale_x_datetime(limits=limitsx)+
      scale_y_continuous(limits=c(min(VALUISMean2$value), max(VALUISMean2$value)))
    
    gp1<- ggplot_gtable(ggplot_build(pT))
    gp2<- ggplot_gtable(ggplot_build(pPOP))
    maxWidth = unit.pmax(gp1$widths[2:3], gp2$widths[2:3])
    gp1$widths[2:3] <- maxWidth
    gp2$widths[2:3] <- maxWidth
    
    grid.arrange(gp2, gp1)
  }},
  movie.name = "PopulationDynamicEntireItaly.gif", interval = ATR, ani.width = 600, 
  ani.height = 600
)

#============================================================
# pops
#============================================================
VALUISMean3<-data.frame(VALUISMean2[366:1461,],
                        KW=isoweek(VALUISMean2[366:1461,1]),
                        year=year(VALUISMean2[366:1461,1]))
VALUISMean4<-data.frame(VALUISMean3,
                        KW_year=paste(VALUISMean3$year,VALUISMean3$KW,sep="_"))
VALUISMean5<-ddply(VALUISMean4,.(KW),summarize,mean=mean(value))

Tiger <- read.table(file = "data/TigerSurveillance.csv",row.names=1,header=TRUE,sep=",") # read xy-values
Tiger2<-data.frame(KW=Tiger$KW,mean=Tiger$sum,group="Tiger",KW_year=paste(Tiger$year,Tiger$KW,sep="_"))
Tiger3<-ddply(Tiger2,.(KW),summarize,mean=sum(mean),.drop=F)

KwADD1<-c(26,28,43,seq(1,23),seq(45,53))
KwADD2<-rep(0,length(KwADD1))
Kwadd<-data.frame(KW=KwADD1,mean=KwADD2)
full<-rbind(Tiger3,Kwadd)

neuM<-merge(VALUISMean5,full,by="KW")
plot(neuM$mean.x,neuM$mean.y,
     xlab="mean number of parous females seeking for oviposition sites [2012-2014]",
     ylab="Ae. albopictus detections in Germany")

lm.out3 = lm(neuM$mean.y ~ neuM$mean.x + I(neuM$mean.x ^2))
anova(lm.out3)
test <- function(x) {summary(lm.out3)$coef[1]+x*summary(lm.out3)$coef[2]+summary(lm.out3)$coef[3]*x^2}


lm.out3 = lm(log(mean.y+1) ~ mean.x,data=neuM)
anova(lm.out3)
test <- function(x) {exp(summary(lm.out3)$coef[1]+x*summary(lm.out3)$coef[2])}
testpos <- function(x) {exp(summary(lm.out3)$coef[1]+x*summary(lm.out3)$coef[2])+exp(summary(lm.out3)$coef[3]+x*summary(lm.out3)$coef[4])}
testneg <- function(x) {exp(summary(lm.out3)$coef[1]+x*summary(lm.out3)$coef[2])-exp(summary(lm.out3)$coef[3]+x*summary(lm.out3)$coef[4])}

predicts<-data.frame(x=timevalues,y=Counts.exponential2)

tre<-ggplot(data=neuM, aes(x=mean.x, y=mean.y))+theme_bw()+
  geom_point()+
  stat_function(fun=test, colour = "red")+
  stat_function(fun=testpos)+
  stat_function(fun=testneg)+
  xlab("Ae. albopictus population in Italy")+
  ylab("Ae. albopictus detections in Germany")+
  theme_bw()

png(file = "MosquitoItalyDetections1.png",width = 7, height=7, units = 'in', res = 1000)
tre
dev.off()

EINS<-ggplot()+theme_bw()+
  geom_bar(data=full, aes(x=KW, y=mean),stat="identity")+
  theme_bw()+xlab("KW")+ ylab("Ae. albopictus detections in Germany")+
  guides(colour=FALSE)+    
  # geom_smooth(data=full,aes(x=KW,y=mean), method="loess") +
  scale_x_continuous(limits=c(0, 53))

ZWEI<-ggplot()+theme_bw()+
  geom_line(data=VALUISMean5, aes(x=KW, y=mean))+
  guides(colour=FALSE)+ylab("Ae. albopictus population in Italy")+xlab("")+
  scale_x_continuous(limits=c(0, 53))



#============================================================
# cars
#============================================================

#read shape
state.map1 <- readShapeSpatial("data/Rastis")
state.map2 <- readShapeSpatial("data/NEU")

#
datse <- read.table(file = "data/MosquitoData.txt",row.names=1,header=TRUE,sep="\t") # read xy-values

plot(state.map1,col = 'blue')
plot(state.map2,add=T,col = 'red')

###################################################
### distance to the next point
###################################################
distMatrix<-crossdist(coordinates(state.map2)[,1], coordinates(state.map2)[,2], coordinates(state.map1)[,1],  coordinates(state.map1)[,2]) 

min.d <- apply(distMatrix, 1, function(x) which(x == min(x)))

KIFF<-state.map1[min.d,]$DZ_Nr

d2<-lapply(KIFF, function(x) subset(d,d$Zst==x))
d2NEU<-do.call("rbind", d2)
d2NEU$Datum<-as.Date(as.character(d2NEU$Datum),"%y%m%d") 
d2NEU$KW<-isoweek(d2NEU$Datum)
d2NEU$SUMME<-d2NEU$KFZ_R1+d2NEU$Lkw_R1
d3<-ddply(d2NEU,.(KW),summarize,mean=mean(SUMME))
plot(d3$mean)

sum(d3$mean/(sum(d3$mean)))

neuMCAR<-merge(d3,full,by="KW")

lm.out3 = lm(neuMCAR$mean.y ~ neuMCAR$mean.x + I(neuMCAR$mean.x ^2))
anova(lm.out3)
test <- function(x) {summary(lm.out3)$coef[1]+x*summary(lm.out3)$coef[2]+summary(lm.out3)$coef[3]*x^2}

5000/(35000/100)


plot(neuMCAR$mean.x)

NeuNeu<-merge(neuM,neuMCAR,by="KW")

NeuNeu$drei<-ifelse(NeuNeu$mean.x.y>33000,1,0)
lm.out3 = lm(log(NeuNeu$mean.y.x+1) ~ NeuNeu$mean.x.x+NeuNeu$mean.x.y)
summary(lm.out3)
test <- function(x) {exp(summary(lm.out3)$coef[1]+x*summary(lm.out3)$coef[2])}

lm.out3 = lm(log(neuMCAR$mean.y+1) ~ neuMCAR$mean.x)
anova(lm.out3)
test <- function(x) {exp(summary(lm.out3)$coef[1]+x*summary(lm.out3)$coef[2])}

ggplot(data=neuMCAR, aes(x=mean.x, y=mean.y))+theme_bw()+
  geom_point()+
  stat_function(fun=test, colour = "red")+
  xlab("mean number of cars/lkws [2012-2014]")+
  ylab("number of detections [2012-2014]")+
  theme_bw()

DREI<-ggplot()+theme_bw()+
  geom_line(data=d3, aes(x=KW, y=mean))+
  guides(colour=FALSE)+ylab("number of cars/lkw")+xlab("")+
  scale_x_continuous(limits=c(0, 53))


gp1<- ggplot_gtable(ggplot_build(EINS))
gp2<- ggplot_gtable(ggplot_build(ZWEI))
gp3<- ggplot_gtable(ggplot_build(DREI))
maxWidth = unit.pmax(gp1$widths[2:3], gp2$widths[2:3], gp2$widths[3])
gp1$widths[2:3] <- maxWidth
gp2$widths[2:3] <- maxWidth
gp3$widths[2:3] <- maxWidth

library(gridExtra)

png(file = "MosquitoItalyDetections2.png",width = 7, height=10, units = 'in', res = 1000)
grid.arrange(gp1, gp2,gp3)
dev.off()












plot(d3$KW,d3$mean,type="l")

as.Date(as.character(d2[[1]]$Datum),"%y%m%d") 
colnames(d2[[1]])
unique(datse$AutobahnID)
unique(datse$year)

###
AedesPop<-data.frame(cbind(year(Daten$DATE),sdfasfd,isoweek(Daten$DATE)))
dimnames(AedesPop)[[2]] <- c("year","pop","week")

nodata <- data.frame(x= numeric(0), y= integer(0), z = character(0))
KEKS1<-data.frame(Traffic= numeric(0) ,Popu= numeric(0),
                  PA= numeric(0))

for(i in 1:length(unique(datse$year))){
  datseyear<-subset(datse,datse$year==unique(datse$year)[i])
  
  
  for(j in 1:length(unique(datse$AutobahnID))){
    d4 <- d2[[unique(datse$AutobahnID)[j]]]
    d3<-subset(d4,d4$year==unique(d4$year)[i])
    AedesPop10 <-subset(AedesPop,AedesPop$year==unique(d4$year)[i])
    
    
    datums<-subset(datseyear,datseyear$AutobahnID==unique(datse$AutobahnID)[j])$date2
    b<-as.Date(as.character(datums),"%y%m%d")
    b2<-isoweek(b)
    print(b2)
    
    if(length(b2>0)){
      
      # isoweeks
      d3$Datum<-as.Date(as.character(d3$Datum),"%y%m%d")
      
      d3$Datum<-sapply(d3$Datum, isoweek) 
      
      d3_2<-subset(d3,d3$Datum>4  & d3$Datum<44)
      AedesPop101<-subset(AedesPop10,AedesPop10$week>4  & AedesPop10$week<44)
      
      
      rbi <- ddply(d3_2, .(Zst,Datum), summarise,
                   KFZ = sum(KFZ_R1),LKW = sum(Lkw_R1),all=sum(KFZ_R1)+sum(Lkw_R1))
      
      rbi2 <- ddply(AedesPop101, .(week), summarise,
                    POPU = mean(pop))
      
      p <- ggplot()
      p2 <- p + geom_line()+
        #stat_smooth()+
        geom_vline(xintercept = b2)+
        geom_line(aes(Datum, (all-min(all))/
                        (max(all)-min(all)), colour="#008B00"), rbi)+
        geom_line(aes(week, (POPU-min(POPU))/
                        (max(POPU)-min(POPU)), colour="red"), rbi2)+
        ylab("traffic")+
        xlab("week")
      
      
      KEKS3<-rep(0,nrow(rbi))
      KEKS3[(min(b2)-4):(max(b2)-4)]<-1
      #KEKS3[b2-4]<-1
      #KEKS<-cbind((rbi$all-min(rbi$all))/(max(rbi$all)-min(rbi$all)),
      #            (rbi2$POPU-min(rbi2$POPU))/(max(rbi2$POPU)-min(rbi2$POPU)),KEKS3)
      KEKS<-cbind(rbi$all,
                  rbi2$POPU,KEKS3)
      print(KEKS3)
      KEKS1<-rbind(KEKS1,KEKS)
      
      png(file = paste("figs/","A_",unique(datse$AutobahnID)[j],"_","year","_",unique(datse$year)[i],".png",sep=""),width = 6, height=6, units = 'in', res = 200)
      plot(p2)
      dev.off()
    }
  }
}






KEKS1_pos<-subset(KEKS1,KEKS1$KEKS3>0)
KEKS1_neg<-subset(KEKS1,KEKS1$KEKS3==0)

library(dplyr)
KILF<-sample_n(KEKS1, 10000,replace=T)

hist(KILF[,2])

library(randomForest)
KEKS1<-na.omit(KEKS1)
fit <- randomForest(as.factor(KEKS3) ~ V2,   data=KEKS1)
print(fit) # view results
importance(fit) # importance of each predictor 

dfdfdfdf<-glm(KEKS3 ~ V2,   data=KEKS1)
plot(KEKS1$V2,predict(dfdfdfdf))

sunflowerplot(predict(dfdfdfdf),KEKS1$KEKS3)

predict(dfdfdfdf,type="prob",newdata=V2)

V1=seq(0,max(KEKS1$V1),max(KEKS1$V1)/1000)
V2=seq(0,max(KEKS1$V2),max(KEKS1$V2)/1000)
DATZ<- expand.grid(V1,V2)
dimnames(DATZ)[[2]]<-c("V1","V2")
DFDF<- as.numeric(predict(fit,type="prob",newdata=V2)[,-1])
sunflowerplot(V1,DFDF)

colnames(DATZ)





REPEATES=100

PRESENCE <- subset(KEKS1,KEKS1$KEKS3>0)
ABSENCE  <- subset(KEKS1,KEKS1$KEKS3<1)

TUTssiPresence <- lapply(1:REPEATES, function(x) PRESENCE[sample(nrow(PRESENCE), round(0.1*nrow(PRESENCE))), ])
TUTssiAbsence  <- lapply(1:REPEATES, function(x) ABSENCE[sample(nrow(ABSENCE), round(0.1*nrow(ABSENCE))), ])
TUTssiTEST     <- lapply(1:REPEATES, function(x) rbind(TUTssiPresence[[x]],TUTssiAbsence[[x]]))
index         <- lapply(1:REPEATES, function(x) row.names(KEKS1)  %in% row.names(TUTssiTEST[[x]]))
TUTssiTRAIN    <- lapply(1:REPEATES, function(x) subset(KEKS1, index[[x]] ==F))

rf.model<- lapply(1:REPEATES, 
                  function(x) randomForest(as.factor(TUTssiTRAIN[[x]][,3])~.,data=TUTssiTRAIN[[x]][,-3], ntree=2000, mtry=round((ncol(KEKS1)-1)/2)))

###
###
AdultRfPr<- lapply(1:REPEATES, 
                   function(x) as.numeric(predict(rf.model[[x]],type="prob",newdata=TUTssiTEST[[x]][,-3])[,-1]))

PA1 = lapply(1:REPEATES, 
             function(x) as.numeric(predict(rf.model[[x]],newdata=TUTssiTEST[[x]][,-3],type="class")))

PA <- lapply(1:REPEATES, 
             function(x) ifelse(PA1[[x]]==2,1,0))

DOKO<-lapply(1:REPEATES, 
             function(x)  ifelse(TUTssiTEST[[x]][,3]==PA[[x]],0,1))

# positive predictions
VAR.IMP.GLM.ALL[(9*j-(9-1))+y-1,6] <- (1-(sum(unlist(DOKO))/length(unlist(DOKO))))



library(ROCR)
x<-1
RfModelPred<-lapply(1:REPEATES, 
                    function(x)  prediction(AdultRfPr[[x]],TUTssiTEST[[x]][,3]))

AdultRfPerf<-lapply(1:REPEATES, 
                    function(x)  performance(RfModelPred[[x]],"tpr","fpr"))

# AUC
auc <- lapply(1:REPEATES, 
              function(x)  performance(RfModelPred[[x]],"auc"))
auc2 <- lapply(1:REPEATES, 
               function(x)  unlist(slot(auc[[x]], "y.values"))) 

VAR.IMP.GLM.ALL[(9*j-(9-1))+y-1,1]<- mean(unlist(auc2))  


# Sensitivity and specificity
perf <- lapply(1:REPEATES, 
               function(x)  performance(RfModelPred[[x]], 'sens', 'spec')) 
ATI<- lapply(1:REPEATES, 
             function(x)  which(abs(perf[[x]]@y.values[[1]]-perf[[x]]@x.values[[1]])==min(abs(perf[[x]]@y.values[[1]]-perf[[x]]@x.values[[1]])))) 
SENS <-lapply(1:REPEATES, 
              function(x)        max(perf[[x]]@x.values[[1]][ATI[[x]]])) 
VAR.IMP.GLM.ALL[(9*j-(9-1))+y-1,2]<- mean(unlist(SENS))
SPEC <-lapply(1:REPEATES, 
              function(x)        max(perf[[x]]@y.values[[1]][ATI[[x]]])) 
VAR.IMP.GLM.ALL[(9*j-(9-1))+y-1,3]<- mean(unlist(SPEC))

# acc
perfi <-lapply(1:REPEATES, 
               function(x)        performance(RfModelPred[[x]], 'acc')) 
MaxPerfi <-lapply(1:REPEATES, 
                  function(x)        max(perfi[[x]]@y.values[[1]][ATI[[x]]])) 
VAR.IMP.GLM.ALL[(9*j-(9-1))+y-1,4]<-mean(unlist(MaxPerfi))

# kappa
KAPPAS <- lapply(1:REPEATES, 
                 function(x) kappa.eval(TUTssiTEST[[x]]$Resp,as.numeric(AdultRfPr[[x]]),max(perf[[x]]@alpha.values[[1]][ATI[[x]]])))
VAR.IMP.GLM.ALL[(9*j-(9-1))+y-1,5]<-mean(unlist(KAPPAS))

VAR.IMP.GLM.ALL[(9*j-(9-1))+y-1,7]<-j
VAR.IMP.GLM.ALL[(9*j-(9-1))+y-1,8]<-y




























colnames(KEKS1)
glm_results<-glm(KEKS3~V2,data=KEKS1)
summary(glm_results)

x1weight <- seq(0, 1, 0.01)
x2weight <- seq(0, 1, 0.01)
yweight <- predict(glm_results, list(V2 = x2weight),type="response")

plot(KEKS1$V2, KEKS1$KEKS3, pch = 16, xlab = "WEIGHT (g)", ylab = "VS")
lines(x1weight, yweight)

dev.new()
boxplot(KEKS1[,2]~KEKS1$KEKS3)

wilcox.test(KEKS1[,2],KEKS1$KEKS3)



















####
GOIN = read.table("data/GOIN.csv", 
                  sep=",",header=T)

GOIN2012<-subset(GOIN,GOIN$X2012>0)
GOIN2013<-subset(GOIN,GOIN$X2013>0)

rbi12 <- ddply(d1, .(Zst), summarise,
               KFZ = sum(KFZ_R1),LKW = sum(Lkw_R1),all=sum(KFZ_R1)+sum(Lkw_R1))
rbi13 <- ddply(d2, .(Zst), summarise,
               KFZ = sum(KFZ_R1),LKW = sum(Lkw_R1),all=sum(KFZ_R1)+sum(Lkw_R1))

d2012<-lapply(KIFF, function(x) subset(d1,d$Zst==x))
d2013<-lapply(KIFF, function(x) subset(d2,d$Zst==x))


GOIN2012_pos<-subset(GOIN2012,GOIN2012$X2012==2)
df<-lapply(GOIN2012_pos$Stand.ort.Nr,function(x) d2012[[x]])
dfd<-lapply(df,function(x) sum(x$KFZ_R1))

GOIN2012_neg<-subset(GOIN2012,GOIN2012$X2013==1)
df_neg<-lapply(GOIN2012_neg$Stand.ort.Nr,function(x) d2012[[x]])
dfd_neg<-lapply(df_neg,function(x) sum(x$KFZ_R1))


GOIN2013_pos<-subset(GOIN2013,GOIN2013$X2013==2)
df<-lapply(GOIN2013_pos$Stand.ort.Nr,function(x) d2013[[x]])
dfd13<-lapply(df,function(x) sum(x$KFZ_R1))


GOIN2013_neg<-subset(GOIN2013,GOIN2013$X2013==1)
df_neg<-lapply(GOIN2013_neg$Stand.ort.Nr,function(x) d2013[[x]])
dfd_neg13<-lapply(df_neg,function(x) sum(x$KFZ_R1))

A<-cbind(unlist(dfd),1)
B<-cbind(unlist(dfd_neg),0)
A2<-cbind(unlist(dfd13),1)
B2<-cbind(unlist(dfd_neg13),0)
C<-rbind(A,B,A2,B2)
plot(C)
mean(A)
mean(B)

boxplot(A,B)

wilcox.test(A,B)


rbi_pos <- subset(rbi,rbi$Zst==c(GOIN2012_pos$Stand.ort.Nr))





B<- c("130619",
      "130718",
      "130731",
      "130731",
      "130814",
      "130814",
      "130822",
      "130828",
      "130904",
      "130909",
      "130909")

B<-c("20130718","130814","130909")

B<-c("130822","130904")

B<-c("130814",
     "130828",
     "130929")

B<-c("130828", "130929")
















d$Datum<-as.Date(as.character(d$Datum),"%y%m%d")

d$Datum<-sapply(d$Datum, isoweek) 

d2<-subset(d,d$Datum>2  & d$Datum<50)

rbi <- ddply(d2, .(Zst,Datum), summarise,
             mean_rbi = sum(PLZ_R1))
#rbi$Datum<-as.Date(as.character(rbi$Datum),"%y%m%d")


#week = format(rbi$Datum, format="%Y-%U")

unique(rbi$Zst)

rbi2<-subset(rbi,Zst==9191)

plot(rollmean(rbi2$mean_rbi,k=1))


p <- ggplot(rbi2, aes(x=Datum, y=mean_rbi, group=Zst))
p + geom_line()+geom_vline(xintercept = c(23,27,29,31,32,33,34,35,36))+geom_hline(yintercept = c(mean(rbi2$mean_rbi)))
abline(37)

?rollmean





set.seed(1)
dates <- seq(as.Date('2011-01-01'),as.Date('2011-12-31'),by='days')

set1 <- data.frame(lat=40+runif(10000),
                   lon=-70+runif(10000),date=sample(dates,10000,replace=TRUE))

set2 <- data.frame(lat=40+runif(100),
                   lon=-70+runif(100),date=sample(dates,100,replace=TRUE))
set1[1,]


