mean_specific_month <- function(x, t_seq, time_start, time_end, func_sel){

  time_start_pa <- ifelse(month(t_seq) > time_start, 1, 0)
  time_end_pa <-ifelse(month(t_seq) < time_end, -1, 0)
  
  values <- as.numeric(x)
  
  dataset <- data.frame(year = year(t_seq), values = values,
                        time_start_pa, time_end_pa)
  
  dataset_sub <- subset(dataset, time_start_pa > 0 | time_end_pa < 0)

  # new year variable 
  dataset_sub$new_year <- dataset_sub$year + dataset_sub$time_end_pa
  
  dataset_sub_2 <- subset(dataset_sub, new_year > min(new_year) & 
                            new_year < max(year))
  
  # calculation
  if(func_sel == "mean"){
    calculation <- ddply(dataset_sub_2, .(new_year), summarize, 
                         result = mean(values, na.rm=T))}
  
  if(func_sel == "min"){
    calculation <- ddply(dataset_sub_2, .(new_year), summarize, 
                         result = min(values, na.rm=T))}
  
  if(func_sel == "variability"){
    calculation <- ddply(dataset_sub_2, .(new_year), summarize, 
                         result = max(rle(values < 0)$length[rle(values < 0)$values]))}
  
  calculation$result
}