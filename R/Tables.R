#=====================#
#====    Tables   ====#
#=====================#

#----    *get_table_cluster    ----

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

#----    *get_table_cluster_ext    ----

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

#----    *get_table_prior_predict    ----

# drake::loadd(data_prior_predict)

get_table_prior_predict <- function(data = data_prior_predict, format = c("latex", "html")){
  format <- match.arg(format)

  data %>%
    dplyr::select(- prior_sd) %>%
    mutate_if(is.numeric, round, 1) %>%
    kable(., format = format, booktabs = TRUE, align = c("r", rep("c", 5)), escape = FALSE,
          col.names = c("Prior", "$-1$ SD", "$-.5$ SD", "$+0$ SD", "$+ .5$ SD", "$+ 1$ SD"),
          caption = "Prior prediction acording to different prior settings assuming $exp(1)$ as intercept value.") %>%
    add_header_above(c(" " = 1, "Predicted Problems" = 5)) %>%
    kable_styling(latex_options = c("hold_position"),
                  bootstrap_options = c("hover", "striped"),
                  full_width = FALSE)
}
#----    *get_table_bf    ----

get_table_bf <- function(bf_result = BF_weights_ext,
                         path_img = "Documents/Paper/figure/",
                         problem = c("ext", "int"),
                         format = c("latex", "html")){
  format <- match.arg(format)
  problem <- match.arg(problem)

  names_plot <- paste("ball_BF",problem, c("null", "monotropy", "hierarchy",
                                "independence", "integration"), sep = "_")
  images <- paste0(path_img, names_plot, ".png")

  for(i in 1:5){
    get_ggplot_balls(data = bf_result[i,],
                     filename = images[i])
  }

  if(format == "latex"){
    col_width <- "1cm"
    image_size <- 100
  } else {
    col_width <- "50pt"
    image_size <- 125
  }

  bf_result %>%
    bind_cols(img = "") %>%
    mutate(hypothesis = c("Null", "Monotropy", "Hierarchy",
                          "Independence", "Integration"),
           bf = format(bf, digits = 2),
           weights = round(weights,2)) %>%
    select(hypothesis, bf, weights, img) %>%
    # mutate_at(c("hypothesis", "bf","weights"), centerText) %>%
    kable(., format = format, booktabs = TRUE, align = c("r", rep("c", 3)), escape = FALSE,
          col.names = c("Hypothesis", "Bayes Factor", "Posterior Probability", " "),
          caption = "Bayes Factor encompassing model and hypothesis posteriro probabilities  ($n_{subj} = 847$).") %>%
    column_spec(4, image = spec_image(images, image_size, image_size),
                extra_css = "vertical-align:middle;padding-bottom: 0px;",
                width = col_width, latex_valign = "m") %>%
    row_spec(0, bold = TRUE) %>%
    kable_styling(latex_options = c("hold_position"),
                  bootstrap_options = c("hover"),
                  full_width = FALSE)
}



#----    *get_table_sens_analysis    ----

get_table_sens_analysis <- function(summary_sensitivity, format = c("latex", "html"),
                                    bookdown = FALSE){
  format <- match.arg(format)
  font_size <- NULL

  table <- summary_sensitivity %>%
    mutate(bf = format(bf, digits = 2),
           weights = round(weights,2),
           names = gsub("(_ext$|_int$)", "", names)) %>%
    pivot_wider(names_from = case, values_from = c(bf, weights)) %>%
    select(1, 2, 7, 3, 8, 4, 9, 5, 10, 6, 11) %>%
    mutate(names = c("Null", "Monotropy", "Hierarchy",
                     "Independence", "Integration")) %>%
    kable(., format = format, booktabs = T, align = c("r", rep("c", 3)), escape = FALSE,
          col.names = c("Hypothesis", rep(c("BF", "PP"), 5)),
          caption = "Bayes Factor encompassing model v and hypothesis posterior probabilities (PP) under different prior settings  ($n_{subj} = 847$).")

  if(format == "html"){
    table <- table %>%
      add_header_above(c(" ", "$\\mathcal{N}(0, .5)$" = 2,
                         "$\\mathcal{N}(0, 1)$" = 2,
                         "$\\mathcal{N}(0, 3)$" = 2,
                         "$\\mathcal{N}(0, 5)$" = 2,
                         "$\\mathcal{N}(0, 10)$" = 2),
                       escape = FALSE, bold = TRUE)
    font_size <-  12
  } else if(isTRUE(bookdown)){
    table <- table %>%
      add_header_above(c(" ", "$\\\\mathcal{N}(0, .5)$" = 2,
                         "$\\\\mathcal{N}(0, 1)$" = 2,
                         "$\\\\mathcal{N}(0, 3)$" = 2,
                         "$\\\\mathcal{N}(0, 5)$" = 2,
                         "$\\\\mathcal{N}(0, 10)$" = 2),
                       escape = FALSE, bold = TRUE)
  } else {
    table <- table %>%
      add_header_above(c(" ", "$\\\\bm{\\\\mathcal{N}(0, .5)}$" = 2,
                         "$\\\\bm{\\\\mathcal{N}(0, 1)}$" = 2,
                         "$\\\\bm{\\\\mathcal{N}(0, 3)}$" = 2,
                         "$\\\\bm{\\\\mathcal{N}(0, 5)}$" = 2,
                         "$\\\\bm{\\\\mathcal{N}(0, 10)}$" = 2),
                       escape = FALSE, bold = TRUE)
  }
  table %>%
    row_spec(0, bold = TRUE) %>%
    column_spec(1, bold = TRUE) %>%
    kable_styling(latex_options = c("hold_position", "scale_down"),
                  bootstrap_options = c("striped", "hover"),
                  full_width = FALSE, font_size = font_size)

}

#====    Bookdown    ====

#----    *table_gender_grade    ----

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

#----    *table_parent_clusters    ----

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

#----    *table_mother_father_clusters    ----

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

#----    *get_table_cluster_prob    ----

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

#----    *get_table_AIC_BIC    ----

# drake::loadd(AIC_weights_ext, BIC_weights_ext)
get_table_AIC_BIC <- function(AIC_weights = AIC_weights_ext,
                              BIC_weights = BIC_weights_ext,
                              problem = c("ext", "int"),
                              format = c("latex", "html"),
                              path_img = "Documents/Bookdown/images/"){

  problem <- match.arg(problem)
  format <- match.arg(format)

  names_plot_AIC <- paste("ball_AIC", problem, c("zero", "mother", "additive",
                                "inter"), sep = "_")
  names_plot_BIC <- paste("ball_BIC", problem, c("zero", "mother", "additive",
                                                 "inter"), sep = "_")
  images_AIC <- paste0(path_img, names_plot_AIC, ".png")
  images_BIC <- paste0(path_img, names_plot_BIC, ".png")

  for(i in seq_along(images_AIC)){
    get_ggplot_balls(data = AIC_weights[i,],
                     filename = images_AIC[i])
    get_ggplot_balls(data = BIC_weights[i, ],
                     filename = images_BIC[i])
  }

  data_table <- AIC_weights %>%
    left_join(BIC_weights, by = "names", suffix = c("_AIC", "_BIC")) %>%
    mutate(img_AIC = "",
           img_BIC = "") %>%
    mutate_at(vars(starts_with("weights")), round, 2) %>%
    mutate_at(vars(starts_with("ic")), round, 1) %>%
    select(names, df_AIC, ic_AIC, weights_AIC, img_AIC,
           ic_BIC, weights_BIC, img_BIC)


  if(format == "latex"){
    data_table <- data_table %>%
      mutate(names = gsub("_", "-", names))
    col_width <- "1cm"
    image_size <- 100
  } else {
    col_width <- "50pt"
    image_size <- 125
  }

  problem_title <- if_else(problem == "ext", "externalizing", "internalizing")

  kable(data_table, format = format, booktabs = TRUE, align = c("r", rep("c", 7)), escape = FALSE,
          col.names = c("Model", "Df", "AIC", "AIC$_{weights}$", " ",
                        "BIC", "BIC$_{weights}$", " "),
          caption = sprintf("Model comparison %s problems ($n_{subj} = 847$).", problem_title)) %>%
    column_spec(5, image = spec_image(images_AIC, image_size, image_size),
                extra_css = "vertical-align:middle;padding-bottom: 0px;",
                width = col_width, latex_valign = "m") %>%
    column_spec(8, image = spec_image(images_BIC, image_size, image_size),
                extra_css = "vertical-align:middle;padding-bottom: 0px;",
                width = col_width, latex_valign = "m") %>%
    row_spec(0, bold = TRUE) %>%
    kable_styling(latex_options = c("hold_position", "scale_down"),
                  bootstrap_options = c("hover"),
                  full_width = FALSE)
}



#----



















