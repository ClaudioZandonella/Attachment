#============================#
#====    Data Munging    ====#
#============================#

#-----    munge_data    ----

#' Munge Raw Data
#'
#' Filter raw including only middle childhood age. 7 children are removed as
#' they are older (failed in school). Select columns of interest: children and
#' class id; children information (gender, and age); internalizing and
#' externalizing scores;  ECR items to conduct cluster analysis; ECR scores on
#' Anxiety and Avoidance (both parents).
#'
#' @param data the row dataset
#'
#' @return data used in the analysis
#'
#' @examples
#' targets::tar_load(data_raw)
#' data <- data_raw
#' munge_data(data)
#'

munge_data <- function(data){

  data <- data %>%
    rename("gender" = "genere",
           "ID_class" = "id_classe") %>%
    dplyr::filter(age_year < 12.30) %>%  # only middle-childhood - remove children who failed in school (7 subjs)
    mutate_at(c("externalizing_sum", "internalizing_sum"), round, 0) %>%  # round values caused by imputation missing data
    select(c("ID", "ID_class", "externalizing_sum", "internalizing_sum", "gender", "age_year",
             starts_with(c("o_ecr", "Av", "Anx"))))

  return(data)
}

#=============
