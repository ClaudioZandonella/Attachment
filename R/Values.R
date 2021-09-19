#=====================#
#====    Values   ====#
#=====================#

#----    perc_females    ----

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

tot_subj <- function(){
  nrow(data_raw)
}

#----    tot_subj_older    ----

tot_subj_older <- function(){
  data_raw %>%
    dplyr::filter(age_year >= 12.3) %>%
    count()
}

#----    tot_subj_NA    ----

tot_subj_NA <- function(){
  data_raw %>%
    dplyr::filter(is.na(age_year)) %>%
    count()
}

#----    tot_subj_included    ----

tot_subj_included <- function(){
  nrow(data_cluster)
}

#----    var_info    ----

var_info <- function(var){

  sd_age <- sd(data_cluster[, var]) %>%
    my_round()
  mean_age <- mean(data_cluster[, var]) %>%
    my_round()

  paste0(mean_age, " (SD = ", sd_age, ")")
}

#----    var_summary    ----

var_summary <- function(var){

  var_name <- switch(var,
                     "age_year" = "Age",
                     "externalizing_sum" = "Externalizing problems")
  comment <- sprintf("%s summary statistics", var_name)
  my_comment(comment)
  summary(data_cluster[,var])
}

#----    freq_gender    ----

freq_gender <- function(){
  my_comment("Gender frequencies")
  table(data_cluster$gender)
}

#----    freq_grade    ----

freq_grade <- function(){
  my_comment("Grade frequencies")
  data_raw %>%
    dplyr::filter(age_year < 12.30) %>%
    select(classe) %>%
    table()
}

#----    freq_grade    ----

freq_grade <- function(){
  my_comment("Grade frequencies")
  data_raw %>%
    dplyr::filter(age_year < 12.30) %>%
    select(classe) %>%
    table()
}

#----    parent_mclust    ----

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


#-----

#-------







