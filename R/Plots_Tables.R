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

get_table_cluster <- function(perc = TRUE, digits = 2){

  data_cluster %>%
    select(father, mother) %>%
    group_by(father, mother) %>%
    count() %>%
    pivot_wider(names_from = father, values_from = n) %>%
    bind_cols(Total = rowSums(.[, 2:5])) %>%
    ungroup() %>%
    bind_rows(tibble(mother = "Total",
                     summarise_at(., vars(Secure:Total), sum))) %>%
    kable(., booktabs = T, align = c("r", rep("c", 5)),
          col.names = c("Mother Attachemnt", "Secure", "Anxious", "Avoidant", "Fearful", "Total"),
          caption = "Attachment styles frequencies ($n_{subj} = 847$)") %>%
    add_header_above(c(" ", "Father Attachment" = 4, " ")) %>%
    kable_styling(latex_options = c("hold_position"))
}

#----    plot_externalizing_dist    ----

plot_externalizing_dist <- function(){

  ggplot(data_cluster) +
    geom_bar(aes(x = externalizing_sum), fill = "firebrick",
             col = "gray20", alpha = .8)+
    labs(x = "Externalizing Problems",
         y = "Frequency") +
    theme_classic()
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

sensitivity_plot <- function(){
  drake::loadd(encompassing_model_int)
  par_post <- brms::fixef(encompassing_model_int, summary = FALSE) %>%
    as.data.frame()

  ggplot(par_post) +
    geom_density(aes(x = motherAnxious)) +
    geom_density(aes(x = motherAvoidant)) +
    geom_density(aes(x = motherFearful)) +
    # geom_density(aes(x = fatherAnxious)) +
    # geom_density(aes(x = fatherAvoidant)) +
    # geom_density(aes(x = fatherFearful)) +
    geom_density(aes(x = `motherAnxious:fatherAnxious`)) +
    geom_density(aes(x = `motherAvoidant:fatherAvoidant`)) +
    stat_function(fun = dnorm, args = list(mean = 0, sd = .5), col = "red") +
    stat_function(fun = dnorm, args = list(mean = 0, sd = 3), col = "blue") +
    stat_function(fun = dnorm, args = list(mean = 0, sd = 10), col = "green") +
    xlim(-3, 3)
}

#----

