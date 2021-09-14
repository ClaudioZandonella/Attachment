#=============================#
#====    Plots & Tables   ====#
#=============================#

#----    get_plot_zinb    ----

#' Plot of the ZINB Model
#'
#' Plot of the marginal effects of the Zero inflated Negative Binomial model (ZINB)
#'
#' @param model the ZINB modle
#'
#' @return a ggplot object
#'
#' @examples
#' drake::loadd(fit_int_zinb)
#' get_plot_zinb(model = fit_int_zinb)
#'

get_plot_zinb <- function(model){

  marginal_effects_gender <- emmeans::emmeans(model, specs = ~ gender)

  p_gender <- ggplot(as.data.frame(marginal_effects_gender),
         aes(x = gender, y = emmean, colour = gender)) +
    geom_point(show.legend = FALSE) +
    geom_errorbar(aes(ymin=asymp.LCL, ymax = asymp.UCL, colour = gender),
                  size = 1, width = .2, show.legend = FALSE) +
    labs(x = "Gender",
         y = "Problems") +
    coord_cartesian(ylim = c(0, 8)) +
    theme_bw()

  marginal_effects_att <- emmeans::emmeans(model, specs = ~ mother*father)

  p_attachemnt <- ggplot(as.data.frame(marginal_effects_att),
         aes(x = mother, y = emmean, colour = father, group = father)) +
    geom_point(show.legend = FALSE) +
    geom_line(show.legend = FALSE) +
    geom_errorbar(aes(ymin=asymp.LCL, ymax = asymp.UCL, colour = father),
                  width = .5, show.legend = FALSE) +
    facet_grid(.~father) +
    labs(x = "Mother Attachment",
         y = "Problems") +
    coord_cartesian(ylim = c(0, 8)) +
    theme_bw()

  gridExtra::grid.arrange(p_gender, p_attachemnt, layout_matrix = matrix(c(1,2,2,2,2), nrow = 1))
}

#----    plot_prior_adj    ----

plot_prior_adj <- function(k = 1){

  ggplot(data.frame(x = c(-3,3))) +
    geom_area(stat = "function", fun = dnorm, col = "gray40", fill = "#F98400",
              xlim = c(-3, k), size = 1, alpha = .8) +
    geom_area(stat = "function", fun = dnorm, col = "gray40", fill = "#5BBCD6",
              xlim = c(k, 3), size = 1, alpha = .8) +
    geom_segment(x = k, xend = k , y = 0, yend = .35, size = 1.5, col = "gray20") +
    annotate(geom = "text", x = k, y = .37, label = "\\textit{k}") +
    xlim(-3, 3) +
    labs(x = "$\\theta_i$") +
    theme_classic() +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title.y = element_blank(),
          axis.line.y.left = element_blank())
}

#----    plot_bounded_par    ----

plot_bounded_par <- function(){

  ggplot(data.frame(x = c(-3,3))) +
    geom_area(stat = "function", fun = dnorm, col = "gray40", fill = "#F98400",
              args = list(sd = .75), size = 1, alpha = .7) +
    geom_area(stat = "function", fun = dunif, args = list(min = -1), col = "gray40", fill = "#5BBCD6", size = 1, alpha = .7) +
    geom_segment(x = 0, xend = 0 , y = 0, yend = .55, size = 1.5, col = "gray20") +
    xlim(-2.2, 2.2) +
    labs(x = "$\\rho_i$") +
    theme_classic() +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.y = element_blank(),
          axis.line.y.left = element_blank())
}

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

#----    plot_externalizing_dist    ----

plot_externalizing_dist <- function(){

  ggplot(data_cluster) +
    geom_bar(aes(x = externalizing_sum), fill = "firebrick",
             col = "gray20", alpha = .8) +
    scale_x_continuous(limits = c(-0.5,20)) +
    labs(x = "Externalizing Problems",
         y = "Frequency") +
    theme_classic()
}
#----    plot_attachment_marginal    ----

plot_attachment_marginal <- function(mother_score = c(0.05, 0.5, 0.5, 1),
                                     father_score = c( 0.05, 0.25, 0.25, .5),
                                     lab_y = "Externalizing Problems"){
  data_plot <- data.frame(
    Parent = factor(rep(c("Mother", "Father"), each = 4), levels = c("Mother", "Father")),
    Attachment = factor(rep(c("Secure", "Anxious", "Avoidant", "Fearful"), times = 2),
                        levels = c("Secure", "Anxious", "Avoidant", "Fearful")),
    Score = c(mother_score, father_score))

  ggplot(data_plot) +
    geom_bar(aes(x = Attachment, y = Score, fill = Parent),
             stat = "identity", show.legend = FALSE, alpha = .8, col = "gray20") +
    ylim(0, 1) +
    facet_grid(Parent ~ .) +
    labs(x = "Attachemnt Style",
         y = lab_y) +
    theme_classic() +
    theme(axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          panel.border = element_rect(fill = "#FF000000"))
}

#----    plot_attachment_tile    ----

plot_attachment_tile <- function(problems_score,
                                 lab_legend = "Externalizing Problems"){
  data_plot <- expand.grid(
    Mother = factor(c("Secure", "Anxious", "Avoidant", "Fearful"),
                    levels = c("Secure", "Anxious", "Avoidant", "Fearful")),
    Father = factor(c("Secure", "Anxious", "Avoidant", "Fearful"),
                    levels = c("Secure", "Anxious", "Avoidant", "Fearful")))

  data_plot$Problems <- problems_score
  range_score <- c(0,max(data_plot$Problems, 1))


  ggplot(data_plot) +
    geom_tile(aes(x = Father, y = Mother, fill = Problems), col = "gray10") +
    scale_fill_gradient(low = "white", high = "firebrick", limits = range_score,
                        breaks= range_score, labels = c("Low", "High")) +
    labs(fill = "Problems",
         y = "Mother Attachment",
         x = "Father Attachment")+
    guides(fill = guide_colourbar(barheight = .75, label = FALSE)) +
    theme_classic() +
    theme(plot.margin = margin(t = 30, r = 10, b = 10, l = 10, unit = "pt"),
          legend.direction = "horizontal",
          legend.position = c(.5,1.075),
          legend.background = element_rect(fill = "#FF000000"))
}

#----    plot_hypothesis    ----

plot_hypothesis <- function(hypothesis = c("null", "monotropy",
                                           "hierarchy", "independence",
                                           "integration"),
                            out_var = "Externalizing Problems"){
  hypothesis <- match.arg(hypothesis)

  if(hypothesis == "null"){
    mother_score <- .05
    father_score <- .05
  } else if (hypothesis == "monotropy"){
    mother_score <- c(.05, .5, .5, 1)
    father_score <- rep(.05, 4)
  } else if (hypothesis == "hierarchy"){
    mother_score <- c(.05, .5, .5, 1)
    father_score <- c(.05, .25, .25, .5)
  } else if (hypothesis == "independence"){
    mother_score <- c(.05, .5, .5, 1)
    father_score <- c(.05, .25, .75, 1)
  }


  if(hypothesis == "integration"){
    score <- c(.05, .15, .15, NA,
               .10, .50, .50, .90,
               .10, .50, .50, .90,
               NA, .90, .90, 1.0)
    plot_attachment_tile(problems_score = score,
                         lab_legend = out_var)
  } else {
    plot_1 <- plot_attachment_marginal(mother_score = mother_score,
                                       father_score = father_score,
                                       lab_y = out_var)

    score <- rep(mother_score, 4) + rep(father_score, each = 4)
    plot_2 <- plot_attachment_tile(problems_score = score,
                                   lab_legend = out_var)
    gridExtra::grid.arrange(plot_1, plot_2, ncol = 2,
                            layout_matrix = rbind(c(1,1,1,1,1,2,2,2,2,2,2)))
  }
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
#----    get_ggplot_balls    ----

get_ggplot_balls <- function(data, filename = "Documents/Paper/figure/Prova.png"){
  plot <- data %>%
    ggplot() +
    geom_point(aes(x = 0, y = 0, size = weights, fill = weights),
               shape = 21, stroke = 2, col = "gray20") +
    scale_size(limits = c(0,1), range = c(15,150),
               breaks = seq(0,1, length.out =10)) +
    scale_fill_gradient(limit=c(0,1), high = "#046C9A", low = "white") +
    theme_classic() +
    theme(legend.position = "none",
          axis.line = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank())

  ggsave(plot = plot, filename = filename, width =4.67, height =4.67)
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

#----    normal_approximation    ----

# Extra function not used at the moment

normal_approximation <- function(){

  drake::loadd(encompassing_model_int)
  par_post <- brms::fixef(encompassing_model_int, summary = FALSE) %>%
    as.data.frame()

  mu <- apply(par_post[, c("motherAvoidant",
                           "motherAvoidant:fatherAvoidant")],2, mean)
  cov <- cov(par_post[, c("motherAvoidant",
                          "motherAvoidant:fatherAvoidant")])

  data_grid <- expand.grid(s_1 = seq(-.6, .5, length.out=100), s_2 = seq(-.5, .75, length.out=100))
  q_samp <- cbind(data_grid, prob = mvtnorm::dmvnorm(data_grid, mean = mu, sigma = cov))

  ggplot(par_post) +
    geom_density_2d(aes(x=motherAvoidant, y=`motherAvoidant:fatherAvoidant`)) +
    geom_contour(data = q_samp, aes(x=s_1, y=s_2, z=prob), col = "black")+
    theme_bw()


}

#----    get_plot_sensitivity    ----

get_plot_sensitivity <- function(encompassing_model = encompassing_model_ext){
  par_post <- brms::fixef(encompassing_model, summary = FALSE) %>%
    as.data.frame()

  cols <- c("$\\mathcal{N}(0, .5)$" = "#F8766D",
            "$\\mathcal{N}(0, 3)$" = "#00BA38",
            "$\\mathcal{N}(0, 10)$" = "#619CFF",
            "Parameter Posterior" = "black")

  ggplot(par_post[seq(1,nrow(par_post), by = 5), ]) +
    # geom_density(aes(x = motherAnxious)) +
    geom_density(aes(x = motherAvoidant), adjust=1.5) +
    geom_density(aes(x = motherFearful), adjust=1.5) +
    # geom_density(aes(x = fatherAnxious)) +
    # geom_density(aes(x = fatherAvoidant)) +
    # geom_density(aes(x = fatherFearful)) +
    geom_density(aes(x = `motherAnxious:fatherAnxious`), adjust=1.5) +
    # geom_density(aes(x = `motherAvoidant:fatherAvoidant`),
    #              aes(color = "black")) +
    geom_segment(aes(x = 0, y = 0, xend = 0, yend = 1.55), linetype = "dashed") +
    stat_function(fun = dnorm, args = list(mean = 0, sd = .5),
                  aes(color = "$\\mathcal{N}(0, .5)$",), size = 1.5) +
    stat_function(fun = dnorm, args = list(mean = 0, sd = 3),
                  aes(color = "$\\mathcal{N}(0, 3)$",), size = 1.5) +
    stat_function(fun = dnorm, args = list(mean = 0, sd = 10),
                  aes(color = "$\\mathcal{N}(0, 10)$",), size = 1.5)  +
    scale_colour_manual(values = cols)+
    coord_cartesian(xlim = c(-3, 3), ylim = c(0,1.65)) +
    xlim(-3, 3) +
    theme_classic() +
    theme(axis.title = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          axis.line.y = element_blank(),
          legend.title = element_blank(),
          legend.direction = "horizontal",
          legend.position = c(.5,.95))

}

#----

