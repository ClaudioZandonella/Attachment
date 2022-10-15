#=====================#
#====    Values   ====#
#=====================#

#----    perc_females    ----

# targets::tar_load(data_cluster)

perc_females <- function(perc = TRUE, digits = 2){
  freq <- table(data_cluster$gender)

  if(isTRUE(perc)){
    res <- round(freq[1]*100/nrow(data_cluster), digits = digits)
  } else {
    res <- freq[1]
  }
  return(res)
}

#----    perc_rare_condition    ----

# targets::tar_load(data_cluster)

perc_rare_condition <- function(perc = TRUE, digits = 1){
  test <- with(data_cluster,
               (mother == "Secure" & father == "Fearful") |
                 (mother == "Fearful" & father == "Secure"))

  if(isTRUE(perc)){
    res <- round(mean(test)*100, digits = digits)
  } else {
    res <- sum(test)
  }
  return(res)
}

#----    tot_subj    ----

# targets::tar_load(data_raw)

tot_subj <- function(){
  nrow(data_raw)
}

#----    tot_subj_older    ----

# targets::tar_load(data_raw)

tot_subj_older <- function(){
  data_raw %>%
    dplyr::filter(age_year >= 12.3) %>%
    count()
}

#----    tot_subj_NA    ----

# targets::tar_load(data_raw)

tot_subj_NA <- function(){
  data_raw %>%
    dplyr::filter(is.na(age_year)) %>%
    count()
}

#----    tot_subj_included    ----

# targets::tar_load(data_raw)

tot_subj_included <- function(){
  nrow(data_cluster)
}

#----    var_info    ----

# targets::tar_load(data_cluster)

var_info <- function(var){

  sd_age <- sd(data_cluster[, var]) %>%
    my_round()
  mean_age <- mean(data_cluster[, var]) %>%
    my_round()

  paste0(mean_age, " (SD = ", sd_age, ")")
}

#----    var_summary    ----

# targets::tar_load(data_cluster)

var_summary <- function(var){

  var_name <- switch(var,
                     "age_year" = "Age",
                     "externalizing_sum" = "Externalizing problems",
                     "internalizing_sum" = "Internalizing problems")

  comment <- sprintf("%s summary statistics", var_name)
  my_comment(comment)
  summary(data_cluster[,var])
}

#----    parent_mclust    ----

# targets::tar_load(mclust_mother, mclust_father)

parent_mclust <- function(parent = c("mother", "father")){
  parent <- match.arg(parent)
  comment <- sprintf("Best model-based clustering for %s attachment", parent)

  my_comment(comment)
  if(parent == "mother"){
    summary(mclust_mother)
  } else {
    summary(mclust_father)
  }

}


#-
#----    perc_model_ext    ----

# targets::tar_load(AIC_weights_ext, BIC_weights_ext)

perc_model_ext <- function(model, ic = c("AIC", "BIC")){
  ic <- match.arg(ic)

  if(ic == "AIC"){
    res <- AIC_weights_ext[AIC_weights_ext$names == model, "weights"]
  } else {
    res <- BIC_weights_ext[BIC_weights_ext$names == model, "weights"]
  }

  return(my_perc(res, 0))
}
#----    ext_mean    ----

# targets::tar_load(data_cluster)

ext_mean <- function(){
  my_round(mean(data_cluster$externalizing_sum))
}

#----    perc_model_int    ----

# targets::tar_load(AIC_weights_int, BIC_weights_int)

perc_model_int <- function(model, ic = c("AIC", "BIC")){
  ic <- match.arg(ic)

  if(ic == "AIC"){
    res <- AIC_weights_int[AIC_weights_int$names == model, "weights"]
  } else {
    res <- BIC_weights_int[BIC_weights_int$names == model, "weights"]
  }

  return(my_perc(res, 0))
}
#=============








