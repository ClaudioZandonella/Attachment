#=====================#
#====    Tables   ====#
#=====================#

#----    get_table_cluster    ----

get_table_cluster <- function(perc = TRUE, digits = 2, format = "latex"){

  data_cluster %>%
    select(father, mother) %>%
    group_by(father, mother) %>%
    count() %>%
    pivot_wider(names_from = father, values_from = n) %>%
    bind_cols(Total = rowSums(.[, 2:5])) %>%
    ungroup() %>%
    bind_rows(tibble(mother = "Total",
                     summarise_at(., vars(Secure:Total), sum))) %>%
    kable(., format = format, booktabs = TRUE, align = c("r", rep("c", 5)),
          col.names = c("Mother Attachemnt", "Secure", "Anxious", "Avoidant", "Fearful", "Total"),
          caption = "Attachment styles frequencies ($n_{subj} = 847$).") %>%
    add_header_above(c(" ", "Father Attachment" = 4, " "), bold = TRUE) %>%
    row_spec(0, bold = TRUE) %>%
    kable_styling(latex_options = c("hold_position"))
}

#----    get_table_cluster_ext    ----

get_table_cluster_ext <- function(){
  data_cluster %>%
    group_by(father, mother) %>%
    summarize(mean = my_round(mean(externalizing_sum), 2),
              median = my_round(median(externalizing_sum), 1),
              sd =  my_round(sd(externalizing_sum), 2),
              summary = paste0(mean, " (",
                               sd, ")")) %>%
    select(father, mother, summary, median) %>%
    pivot_wider(names_from = father, values_from = c(summary, median)) %>%
    select(mother, dplyr::ends_with("Secure"), dplyr::ends_with("Anxious"),
           dplyr::ends_with("Avoidant"),  dplyr::ends_with("Fearful")) %>%
    kable(.,format = "latex", booktabs = TRUE, align = c("r", rep("c", 8)),
          col.names = c("Mother Attachemnt", rep(c("Mean (SD)", "Median"), 4)),
          caption = "Externalizing problems according to attachment styles ($n_{subj} = 847$).") %>%
    add_header_above(c(" ", "Secure" = 2, "Anxious" = 2, "Avoidant" = 2, "Fearful" = 2), bold = TRUE) %>%
    add_header_above(c(" ", "Father Attachment" = 8), bold = TRUE) %>%
    row_spec(0, bold = TRUE) %>%
    kable_styling(latex_options = c("hold_position", "scale_down"))
}

#----    get_table_prior_predict    ----

get_table_prior_predict <- function(data = data_prior_predict){
  data %>%
    dplyr::select(- prior_sd) %>%
    mutate_if(is.numeric, round, 1) %>%
    kable(., booktabs = TRUE, align = c("r", rep("c", 5)), escape = FALSE,
          col.names = c("Prior", "- 1 SD", "- .5 SD", "+ 0 SD", "+ .5 SD", "+ 1 SD"),
          caption = "Prior prediction acording to different prior settings assuming $exp(1)$ as intercept value.") %>%
    add_header_above(c(" " = 1, "Predicted Problems" = 5)) %>%
    kable_styling(latex_options = c("hold_position"))
}
#----    get_table_bf    ----

get_table_bf <- function(bf_result = BF_weights_ext,
                         path_img = "Documents/Paper/figure/"){

  names_plot <- paste("ball", c("null", "monotropy", "hierarchy",
                                "independence", "integration"), sep = "_")
  images <- paste0(path_img, names_plot, ".png")

  for(i in 1:5){
    get_ggplot_balls(data = bf_result[i,],
                     filename = images[i])
  }

  # centerText <- function(text){
  #   paste0("\\multirow{1}{*}[0pt]{", text, "}")
  # }



  bf_result %>%
    bind_cols(img = "") %>%
    mutate(hypothesis = c("Null", "Monotropy", "Hierarchy",
                          "Independence", "Integration"),
           bf = format(bf, digits = 2),
           weights = round(weights,2)) %>%
    select(hypothesis, bf, weights, img) %>%
    # mutate_at(c("hypothesis", "bf","weights"), centerText) %>%
    kable(., booktabs = TRUE, align = c("r", rep("c", 3)), escape = FALSE,
          col.names = c("Hypothesis", "Bayes Factor", "Posterior Probability", " "),
          caption = "Bayes Factor encompassing model and hypothesis posteriro probabilities  ($n_{subj} = 847$).") %>%
    column_spec(4, image = spec_image(images, 100, 100),
                width = "1cm", latex_valign = "m") %>%
    row_spec(0, bold = TRUE) %>%
    kable_styling(latex_options = c("hold_position"))
}



#----    get_table_sens_analysis    ----

get_table_sens_analysis <- function(summary_sensitivity){
  summary_sensitivity %>%
    mutate(bf = format(bf, digits = 2),
           weights = round(weights,2),
           names = gsub("(_ext$|_int$)", "", names)) %>%
    pivot_wider(names_from = case, values_from = c(bf, weights)) %>%
    select(1, 2, 7, 3, 8, 4, 9, 5, 10, 6, 11) %>%
    mutate(names = c("Null", "Monotropy", "Hierarchy",
                     "Independence", "Integration")) %>%
    kable(., booktabs = T, align = c("r", rep("c", 3)), escape = FALSE,
          col.names = c("Hypothesis", rep(c("BF", "PP"), 5)),
          caption = "Bayes Factor encompassing model v and hypothesis posterior probabilities (PP) under different prior settings  ($n_{subj} = 847$).") %>%
    add_header_above(c(" ", "$\\\\bm{\\\\mathcal{N}(0, .5)}$" = 2,
                       "$\\\\bm{\\\\mathcal{N}(0, 1)}$" = 2,
                       "$\\\\bm{\\\\mathcal{N}(0, 3)}$" = 2,
                       "$\\\\bm{\\\\mathcal{N}(0, 5)}$" = 2,
                       "$\\\\bm{\\\\mathcal{N}(0, 10)}$" = 2),
                     escape = FALSE, bold = TRUE) %>%
    row_spec(0, bold = TRUE) %>%
    column_spec(1, bold = TRUE) %>%
    kable_styling(latex_options = c("hold_position", "scale_down"))

}

#----    table_grade  ----

#' Get Table School Grade
#'
#' Get table school grade from data_raw selcting subjeects accroding to age
#'
#' @param data_raw the row dataset
#'
#' @return a contingency table
#'
#' @examples
#' drake::loadd(data_raw)
#' table_grade(data_raw)
#'

table_grade <- function(data_raw){
  data_raw %>%
    dplyr::filter(age_year < 12.30) %>%
    select(classe) %>%
    table()
}

#====    Bookdown    ====

#----    table_gender_grade    ----

table_gender_grade <- function(format = "latex"){
  data_raw %>%
    dplyr::filter(age_year < 12.30) %>%
    group_by(genere, classe) %>%
    count() %>%
    pivot_wider(names_from = genere, values_from = n) %>%
    ungroup() %>%
    mutate(total = `F` + `M`,
           classe = c("3rd", "4th", "5th", "6th")) %>%
    bind_rows(., data.frame(classe = "Total", `F` = sum(.$`F`),
                            `M` = sum(.$`M`), total = sum(.$total))) %>%
    kable(.,format = format, booktabs = TRUE, align = c("r", rep("c", 3)), escape = FALSE,
          col.names = c("Grade", "Females", "Males", "Total"),
          caption = "Participants frequancies by grade and gender ($n_{subj} = 847$).") %>%
    add_header_above(c(" ", "Gender" = 2, " "),
                     escape = FALSE, bold = TRUE) %>%
    row_spec(0, bold = TRUE) %>%
    column_spec(1, bold = TRUE) %>%
    kable_styling(latex_options = c("hold_position"),
                  bootstrap_options = c("striped", "hover"), full_width = FALSE)
}

#----    table_parent_clusters    ----

table_parent_clusters <- function(parent = c("mother", "father"), format = "latex"){
  parent <- match.arg(parent)
  parent_sym <- sym(parent)
  parent_lab <- if_else(parent == "mother", true = "Mother", false = "Father")

  data_cluster %>%
    group_by(!!parent_sym, gender) %>%
    summarize(n = n(),
              perc = n/nrow(data_cluster)) %>%
    pivot_wider(names_from = !!parent_sym, values_from = c(n, perc)) %>%
    ungroup() %>%
    bind_rows(., data.frame(gender = "Total",
                           summarise(., across(where(is.numeric), ~ sum(.))))) %>%
    mutate(Gender = c("Females", "Males", "Total"),
           Secure = paste0(n_Secure, " (", my_perc(perc_Secure, 0), "\\%)"),
           Anxious = paste0(n_Anxious, " (", my_perc(perc_Anxious, 0), "\\%)"),
           Avoidant = paste0(n_Avoidant, " (", my_perc(perc_Avoidant, 0), "\\%)"),
           Fearful = paste0(n_Fearful, " (", my_perc(perc_Fearful, 0), "\\%)"),) %>%
    select(Gender, Secure, Anxious, Avoidant, Fearful) %>%
    kable(., format = format, booktabs = TRUE, align = c("r", rep("c", 4)), escape = FALSE,
          caption = paste(parent_lab, "attachment styles by gender ($n_{subj} = 847$).")) %>%
    add_header_above(c(" ", "Attachment Style" = 4),
                     escape = FALSE, bold = TRUE) %>%
    row_spec(0, bold = TRUE) %>%
    column_spec(1, bold = TRUE) %>%
    kable_styling(latex_options = c("hold_position"),
                  bootstrap_options = c("striped", "hover"), full_width = FALSE)
}

#----    table_mother_father_clusters    ----

table_mother_father_clusters <- function(format = "html"){

  data_cluster %>%
    group_by(mother, father) %>%
    summarize(n = n(),
              perc = n/nrow(data_cluster)) %>%
    pivot_wider(names_from = father, values_from = c(n, perc)) %>%
    ungroup() %>%
    mutate(Secure = paste0(n_Secure, " (", my_perc(perc_Secure, 0), "\\%)"),
           Anxious = paste0(n_Anxious, " (", my_perc(perc_Anxious, 0), "\\%)"),
           Avoidant = paste0(n_Avoidant, " (", my_perc(perc_Avoidant, 0), "\\%)"),
           Fearful = paste0(n_Fearful, " (", my_perc(perc_Fearful, 0), "\\%)")) %>%
    select(mother, Secure, Anxious, Avoidant, Fearful) %>%
    kable(., format = format, booktabs = TRUE, align = c("r", rep("c", 4)), escape = FALSE,
          col.names = c("Mother Attachment", "Secure", "Anxious", "Avoidant", "Fearful"),
          caption = "Mother attachment and father attachemtn styles ($n_{subj} = 847$).") %>%
    add_header_above(c(" ", "Father Attachment" = 4),
                     escape = FALSE, bold = TRUE) %>%
    row_spec(0, bold = TRUE) %>%
    column_spec(1, bold = TRUE) %>%
    kable_styling(latex_options = c("hold_position"),
                  bootstrap_options = c("striped", "hover"), full_width = FALSE)
}

#----    get_table_cluster_prob    ----

get_table_cluster_prob <- function(prob = c("ext", "int"), format = "html"){
  prob <- match.arg(prob)

  if(prob == "ext"){
    prob_sym <- sym("externalizing_sum")
    my_fill <-  "firebrick"
    title <- "Externalizing"
  } else {
    prob_sym <- sym("internalizing_sum")
    my_fill <-  "#46ACC8"
    title <- "Internalizing"
  }

  if(format == "latex"){
    font_size <-  NULL
  } else {
    font_size <-  12
  }


  data_cluster %>%
    group_by(father, mother) %>%
    summarize(mean = my_round(mean(!!prob_sym), 2),
              median = my_round(median(!!prob_sym), 1),
              sd =  my_round(sd(!!prob_sym), 2),
              summary = paste0(mean, " (",
                               sd, ")")) %>%
    select(father, mother, summary, median) %>%
    pivot_wider(names_from = father, values_from = c(summary, median)) %>%
    select(mother, dplyr::ends_with("Secure"), dplyr::ends_with("Anxious"),
           dplyr::ends_with("Avoidant"),  dplyr::ends_with("Fearful")) %>%
    kable(.,format = format, booktabs = TRUE, align = c("r", rep("c", 8)),
          col.names = c("Mother Attachemnt", rep(c("Mean (SD)", "Median"), 4)),
          caption = sprintf("%s problems according to attachment styles ($n_{subj} = 847$).", title)) %>%
    add_header_above(c(" ", "Secure" = 2, "Anxious" = 2, "Avoidant" = 2, "Fearful" = 2), bold = TRUE) %>%
    add_header_above(c(" ", "Father Attachment" = 8), bold = TRUE) %>%
    row_spec(0, bold = TRUE) %>%
    column_spec(1, bold = TRUE) %>%
    kable_styling(latex_options = c("hold_position", "scale_down"),
                  bootstrap_options = c("striped", "hover"),
                  full_width = FALSE, font_size = font_size)
}

#----


















