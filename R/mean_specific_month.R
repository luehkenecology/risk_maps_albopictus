mean_specific_month <- function(x, t_seq, time_start, time_end, func_sel){

  time_start <- ifelse(time_start == 1, 12, time_start-1)
  time_end <- ifelse(time_end == 12, 1, time_end+1)
  
  time_start_pa <- ifelse(month(t_seq) > time_start, 1, 0)
  time_end_pa <- ifelse(month(t_seq) < time_end, -1, 0)
  
  values <- as.numeric(x)
  
  dataset <- data.frame(year = year(t_seq), values = values,
                        time_start_pa, time_end_pa)
  
  dataset_sub <- subset(dataset, time_start_pa > 0 | time_end_pa < 0)

  # new year variable 
  dataset_sub$new_year <- dataset_sub$year + dataset_sub$time_end_pa
  
  dataset_sub_2 <- subset(dataset_sub, new_year > min(new_year) & 
                            new_year < max(year))
  
  # calculation
  # mean temperature
  if(func_sel == "mean"){
    calculation <- ddply(dataset_sub_2, .(new_year), summarize, 
                         result = mean(values, na.rm=T))}
  
  # minimum temperature
  if(func_sel == "min"){
    calculation <- ddply(dataset_sub_2, .(new_year), summarize, 
                         result = min(values, na.rm=T))}
  
  # days below zero
  if(func_sel == "n_min_values"){
    calculation <- ddply(dataset_sub_2, .(new_year), summarize, 
                         result =sum(ifelse(values<0, 1,0)), na.rm = T)}
  
  # variability: phases of consecutive days with T above/below zero
  if(func_sel == "variability"){
    calculation <- ddply(dataset_sub_2, .(new_year), summarize, 
                         result =length(rle(values < 0)$length))}
  
  # min_in_row
  if(func_sel == "min_in_row"){
    calculation <- ddply(dataset_sub_2, .(new_year), summarize, 
                         result = max(rle(values < 0)$length[rle(values < 0)$values], na.rm = T))
    }
  # sum of ne
  if(func_sel == "sum_mins"){
    
    calculation <- ddply(dataset_sub_2, .(new_year), function(dataset_sub_2_sub){
                           #Your rle code
                           run <- rle(dataset_sub_2_sub$values < 0)
                           maxi <- max(rle(dataset_sub_2_sub$values < 0)$length[rle(dataset_sub_2_sub$values < 0)$values], na.rm = T)
                           
                           #Add columns for the length of the run and the value of that run
                           dat1 <- rep(run$lengths, run$lengths)
                           dat2 <- rep(run$values, run$lengths)
                           
                           dat <- cbind(dataset_sub_2_sub$values, dat1, dat2)
                           
                           #Subset the data based on the length and value
                           sum(subset(dat, dat1 ==maxi & dat2)[,1], na.rm = T)
                         })
    
    colnames(calculation)[2] <- "result"
  }
  
  ifelse(is.infinite(calculation$result), NA, calculation$result)
}