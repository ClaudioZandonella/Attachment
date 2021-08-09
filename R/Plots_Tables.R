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


#----

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

