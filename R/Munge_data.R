#============================#
#====    Data Munging    ====#
#============================#

#-----    munge_data    ----

munge_data <- function(data){
  data <- data[data$age_year<12.30,]
  data <- subset(data, data$fas !="NA")

  data <- data %>%
    mutate_at(c("externalizing_sum", "internalizing_sum"), round, 0)

  return(data)
}


#-----




