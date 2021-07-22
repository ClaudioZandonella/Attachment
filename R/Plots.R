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

