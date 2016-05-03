minimum_time_row <- function(x, time_start, time_end){
  temp <- as.numeric(x)
  
  testfull <- data.frame(year = year(tseq), tempen=temp)
  
  testfull_2 <-data.frame(testfull, time_start, time_end)
  
  testfull_2_sub<-subset(testfull_2, time_start > 0 | time_end < 0)
  testfull_2_sub$new_year <- testfull_2_sub$year + testfull_2_sub$smaller_march
  
  testfull3_b<-subset(testfull_2_sub,new_year>2005 & new_year<2016)
  
  einsd<-ddply(testfull3_b,.(new_year),summarize,sf=min(tempen, na.rm=T))
  
  einsd$sf
}