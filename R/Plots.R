#=====================#
#====    Plots    ====#
#=====================#

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
#' get_plot_zinb(model = fit_int_zinb, attachment = "mother")
#'

get_plot_zinb <- function(model, attachment = c("interaction", "mother")){
  attachment <- match.arg(attachment)

  marginal_effects_gender <- emmeans::emmeans(model, specs = ~ gender) %>%
    as.data.frame() %>%
    mutate_at(vars(emmean, lower.CL, upper.CL), exp)

  p_gender <- ggplot(as.data.frame(marginal_effects_gender),
         aes(x = gender, y = emmean, colour = gender)) +
    geom_point(show.legend = FALSE, size = 3) +
    geom_errorbar(aes(ymin=lower.CL, ymax = upper.CL, colour = gender),
                  size = 1, width = .2, show.legend = FALSE) +
    labs(x = "Gender",
         y = "Problems") +
    coord_cartesian(ylim = c(0, 6)) +
    theme_bw()

  if(attachment == "interaction"){
    marginal_effects_att <- emmeans::emmeans(model, specs = ~ mother*father) %>%
      as.data.frame() %>%
      mutate_at(vars(emmean, lower.CL, upper.CL), exp)

    p_attachemnt <- ggplot(as.data.frame(marginal_effects_att),
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

    p_attachemnt <- ggplot(as.data.frame(marginal_effects_att),
                           aes(x = mother, y = emmean, colour = mother)) +
      geom_point(show.legend = FALSE, size = 3) +
      geom_errorbar(aes(ymin=lower.CL, ymax = upper.CL, colour = mother),
                    width = .5, show.legend = FALSE, size = 1) +
      labs(x = "Mother Attachment",
           y = "Problems") +
      coord_cartesian(ylim = c(0, 6)) +
      scale_color_manual(values=c("#3B9AB2", "#EBCC2A", "#E1AF00", "#F21A00")) +
      theme_bw()

    layout <- matrix(c(1,1,2,2,2), nrow = 1)
  }


  gridExtra::grid.arrange(p_gender, p_attachemnt, layout_matrix = layout)
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
#----

