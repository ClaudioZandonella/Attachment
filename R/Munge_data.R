#============================#
#====    Data Munging    ====#
#============================#

#-----    munge_data    ----

munge_data <- function(data){

  # TODO ask Tatiana
  # data <- data[data$age_year<12.30,]
  # data <- subset(data, data$fas !="NA")

  data <- data %>%
    rename("gender" = "genere") %>%
    mutate_at(c("externalizing_sum", "internalizing_sum"), round, 0) %>%
    select(c("ID","externalizing_sum", "internalizing_sum", "gender",
             starts_with(c("o_ecr", "Av", "Anx"))))

  return(data)
}


#-----



