#=====================#
#====    Plots    ====#
#=====================#

#----    get_plot_zinb    ----

#' Plot of the ZINB Model
#'
#' Plot of the marginal effects of the Zero inflated Negative Binomial model
#' (ZINB)
#'
#' @param model the ZINB modle
#' @param attachment character value ("interaction" to plot interaction effect
#'   between mother and father attachment; "mother" to plot only mother effect)
#' @param gender logical value indicating whether to plot gender effect
#'
#' @return a ggplot object
#'
#' @examples
#' drake::loadd(fit_ext_zinb)
#' get_plot_zinb(model = fit_ext_zinb, attachment = "mother")
#'

get_plot_zinb <- function(model, attachment = c("interaction", "mother"), gender = TRUE){
  attachment <- match.arg(attachment)

  if(attachment == "interaction"){
    marginal_effects_att <- emmeans::emmeans(model, specs = ~ mother*father) %>%
      as.data.frame() %>%
      mutate_at(vars(emmean, lower.CL, upper.CL), exp)

    p_attachemnt <- ggplot(marginal_effects_att,
                           aes(x = mother, y = emmean, colour = father, group = father)) +
      geom_point(show.legend = FALSE) +
      geom_line(show.legend = FALSE) +
      geom_errorbar(aes(ymin=lower.CL, ymax = upper.CL, colour = father),
                    width = .5, show.legend = FALSE) +
      facet_grid(.~father) +
      labs(x = "Mother Attachment",
           y = "Problems") +
      coord_cartesian(ylim = c(0, 8)) +
      theme_bw()

    layout <- matrix(c(1,2,2,2,2), nrow = 1)
  } else {
    marginal_effects_att <- emmeans::emmeans(model, specs = ~ mother) %>%
      as.data.frame() %>%
      mutate_at(vars(emmean, lower.CL, upper.CL), exp)

    p_attachemnt <- ggplot(marginal_effects_att,
                           aes(x = mother, y = emmean, colour = mother)) +
      geom_point(show.legend = FALSE, size = 3) +
      geom_errorbar(aes(ymin=lower.CL, ymax = upper.CL, colour = mother),
                    width = .5, show.legend = FALSE, size = 1) +
      labs(x = "Mother Attachment",
           y = "Problems") +
      coord_cartesian(ylim = c(0, 7.5)) +
      scale_color_manual(values=c("#3B9AB2", "#EBCC2A", "#E1AF00", "#F21A00")) +
      theme_bw()

    layout <- matrix(c(1,1,2,2,2), nrow = 1)
  }

  if(isTRUE(gender)){
    marginal_effects_gender <- emmeans::emmeans(model, specs = ~ gender) %>%
      as.data.frame() %>%
      mutate_at(vars(emmean, lower.CL, upper.CL), exp) %>%
      mutate(gender = recode_factor(gender, "F" = "Female", "M" = "Male"))

    p_gender <- ggplot(marginal_effects_gender,
                       aes(x = gender, y = emmean, colour = gender)) +
      geom_point(show.legend = FALSE, size = 3) +
      geom_errorbar(aes(ymin=lower.CL, ymax = upper.CL, colour = gender),
                    size = 1, width = .2, show.legend = FALSE) +
      labs(x = "Gender",
           y = "Problems") +
      coord_cartesian(ylim = c(0, 7.5)) +
      theme_bw()

    gridExtra::grid.arrange(p_gender, p_attachemnt, layout_matrix = layout)
  } else {
    p_attachemnt
  }


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
    labs(x = "Attachment Style",
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
          panel.background = element_rect(fill = "transparent"),
          plot.background = element_rect(fill = "transparent", color = NA),
          axis.line = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank())

  ggsave(plot = plot, filename = filename, width =4.67, height =4.67)
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


#====    bookdown    ====

#----    plot_age_dist    ----

plot_age_dist <- function(){
  data_cluster %>%
    ggplot() +
    geom_histogram(aes(x = age_year), bins = 17,
                   col = "gray40", fill = "#3B9AB2", alpha = .8) +
    labs(x = "Years",
         y = "Frequency")
}


#----    plot_scores_cluster    ----

plot_scores_cluster <- function(parent = c("mother", "father")){
  parent <- match.arg(parent)
  parent_sym <- sym(parent)

  if(parent == "mother"){
    parent_lab <- "Mother"
    Anx_sym <- sym("Anxm")
    Av_sym <- sym("Avm")
  } else {
    parent_lab <- "Father"
    Anx_sym <- sym("Anxp")
    Av_sym <- sym("Avp")
  }


  data_plot <- data_cluster %>%
    select(!!parent_sym, !!Anx_sym, !!Av_sym) %>%
    pivot_longer(c(!!Anx_sym, !!Av_sym), names_to = "scale")

  avg <- data_plot %>%
    group_by(scale) %>%
    summarise(mean = mean(value)) %>%
    pivot_wider(names_from = scale, values_from = mean)

  names(avg) <- c("Anx", "Av")

  data_plot %>%
    mutate(scale = factor(scale, labels = c("Anx", "Av"))) %>%
    ggplot() +
    geom_boxplot(aes(x = scale, y = value, fill = scale),
                 show.legend = FALSE, color = "gray40", alpha = .8) +
    geom_segment(aes(x = .5, xend = 1.5, y = avg$Anx, yend = avg$Anx),
                 col = "gray20", linetype = "dashed", size = .7) +
    geom_segment(aes(x = 1.5, xend = 2.5, y = avg$Av, yend = avg$Av),
                 col = "gray20", linetype = "dashed", size = .7) +
    facet_grid(cols = vars(!!parent_sym)) +
    labs(y = "Value") +
    theme(axis.title.x = element_blank())
}

#----    plot_problems_dist    ----

plot_problems_dist <- function(prob = c("ext", "int")){

  prob <- match.arg(prob)

  if(prob == "ext"){
    prob_sym <- sym("externalizing_sum")
    my_fill <-  "firebrick"
    label_x <- "Externalizing Problems"
  } else {
    prob_sym <- sym("internalizing_sum")
    my_fill <-  "#46ACC8"
    label_x <- "Internalizing Problems"
  }

  ggplot(data_cluster) +
    geom_bar(aes(x = !!prob_sym), fill = my_fill,
             col = "gray20", alpha = .8) +
    scale_x_continuous(limits = c(-0.5,20)) +
    labs(x = label_x,
         y = "Frequency")
}
#----    plot_post_pred    ----

# drake::loadd(post_pred_ext)
plot_post_pred <-  function(post_pred = post_pred_ext, problem = c("Externalizing", "Internalizing")){
  problem <- match.arg(problem)

  data <- post_pred %>%
    mutate_all(exp) %>%
    mutate(Female = (F_Secure + F_Anxious + F_Avoidant + F_Fearful)/4,
           Male = (M_Secure + M_Anxious + M_Avoidant + M_Fearful)/4,
           Secure = (F_Secure + M_Secure)/2,
           Anxious = (F_Anxious + M_Anxious)/2,
           Avoidant = (F_Avoidant + M_Avoidant)/2,
           Fearful = (F_Fearful + M_Fearful)/2) %>%
    select(Female:Fearful)

  p_gender <- data %>%
    select(Female, Male) %>%
    pivot_longer(c(Female, Male), names_to = "Gender", values_to = "Problems") %>%
    ggplot() +
    ggridges::stat_density_ridges(aes(x = Problems, y = Gender, fill = factor(stat(quantile))),
                                  geom = "density_ridges_gradient", show.legend = FALSE,
                                  calc_ecdf = TRUE, quantiles = c(0.025, 0.975), color = "gray30") +
    ggridges::stat_density_ridges(aes(x = Problems, y = Gender, fill = Gender), show.legend = FALSE,
                                  quantile_lines = TRUE, quantiles = c(0.025,.5, 0.975), color = "gray30", alpha = 0.7) +
    scale_fill_manual(values = c("gray10", "#ffffff00", "gray10", "#F8766D", "#00BFC4")) +
    scale_y_discrete(expand = expansion(add = c(.3, 1.5))) +
    labs(x = sprintf("%s Problems", problem))

  p_attachemnt <- data %>%
    select(Secure, Anxious, Avoidant, Fearful) %>%
    pivot_longer(c(Secure, Anxious, Avoidant, Fearful), names_to = "Mother", values_to = "Problems") %>%
    mutate(Mother = factor(Mother, levels = c("Secure", "Anxious", "Avoidant", "Fearful"))) %>%
    ggplot() +
    ggridges::stat_density_ridges(aes(x = Problems, y = Mother, fill = factor(stat(quantile))),
                                  geom = "density_ridges_gradient", show.legend = FALSE,
                                  calc_ecdf = TRUE, quantiles = c(0.025, 0.975), color = "gray30") +
    ggridges::stat_density_ridges(aes(x = Problems, y = Mother, fill = Mother), show.legend = FALSE,
                                  quantile_lines = TRUE, quantiles = c(0.025,.5, 0.975), color = "gray30", alpha = 0.7) +
    scale_fill_manual( values = c("gray10", "#ffffff00", "gray10",
                                  "#EBCC2A", "#E1AF00", "#F21A00","#3B9AB2")) +
    scale_y_discrete(expand = expansion(add = c(.3, 1.1))) +
    labs(x = sprintf("%s Problems", problem),
         y = "Mother Attachment")

  gridExtra::grid.arrange(p_gender, p_attachemnt, layout_matrix = matrix(c(1,1,2,2), nrow = 1))
}
#----    plot_post_diff    ----

# drake::loadd(post_pred_ext)
plot_post_diff <-  function(post_pred = post_pred_ext, problem = c("Externalizing", "Internalizing")){
  problem <- match.arg(problem)

  if (problem == "Extrnalizing"){
    labs_diff <- c("Avoidance - Anxious", "Anxious - Secure",
                   "Fearful - Avoidant", "Avoidant - Secure",
                   "Fearful - Anxious", "Fearful - Secure")
    colors <- c("#5F808E", "#3B9AB2", "#A84D47", "#84666A", "#CD3323", "#F21A00")
  } else {
    labs_diff <- c("Avoidance - Anxious", "Avoidant - Secure",
                   "Fearful - Anxious", "Anxious - Secure",
                   "Fearful - Avoidant", "Fearful - Secure")
    colors <- c( "#A84D47", "#3B9AB2", "#5F808E", "#CD3323", "#84666A", "#F21A00")
  }

  data <- post_pred %>%
    mutate_all(exp) %>%
    mutate(Secure = (F_Secure + M_Secure)/2,
           Anxious = (F_Anxious + M_Anxious)/2,
           Avoidant = (F_Avoidant + M_Avoidant)/2,
           Fearful = (F_Fearful + M_Fearful)/2) %>%
    mutate(diff_fear_sec = Fearful - Secure,
           diff_avo_sec = Avoidant - Secure,
           diff_anx_sec = Anxious - Secure,

           diff_fear_anx = Fearful - Anxious,
           diff_avo_anx = Avoidant - Anxious,

           diff_far_avo = Fearful - Avoidant) %>%
    select(dplyr::starts_with("diff_")) %>%
    pivot_longer(dplyr::starts_with("diff_"), names_to = "Difference", values_to = "Problems")

  data %>%
    mutate(Difference = fct_reorder(Difference, Problems, median)) %>%
    ggplot() +
    ggridges::stat_density_ridges(aes(x = Problems, y = Difference, fill = factor(stat(quantile))),
                                  geom = "density_ridges_gradient", show.legend = FALSE,
                                  calc_ecdf = TRUE, quantiles = c(0.025, 0.975), color = "gray30") +
    ggridges::stat_density_ridges(aes(x = Problems, y = Difference, fill = Difference), show.legend = FALSE,
                                  quantile_lines = TRUE, quantiles = c(0.025,.5, 0.975), color = "gray30", alpha = 0.7) +
    geom_vline(xintercept = 0, linetype = "longdash", size = .6, col = "gray30") +
    scale_fill_manual(values = c("gray10", "#ffffff00", "gray10", colors)) +
    scale_y_discrete(labels = labs_diff, expand = expansion(add = c(.3, 1.2))) +
    labs(x = sprintf("%s Problems", problem)) +
    theme(axis.title.y = element_blank())
  }

#----    my_pp_check    ----

# drake::loadd(brm_selected_ext)

my_pp_check <- function(brm_fit = brm_selected_ext,  problem = c("Externalizing", "Internalizing")){
  problem <- match.arg(problem)

  plot <- brms::pp_check(brm_fit, nsamples = 30)

  plot +
    theme_bw() +
    labs(x = sprintf("%s Problems", problem)) +
    coord_cartesian(xlim = c(-.5, 45)) +
    theme(legend.position = c(.8,.8),
          legend.background = element_rect(fill = "transparent"),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank())
}

#=============

