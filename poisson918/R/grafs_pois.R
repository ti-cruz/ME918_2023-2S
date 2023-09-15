#' @importFrom magrittr '%>%'
#' @importFrom dplyr mutate
#' @import ggplot2
#' @export

graf_pois <- function(input){

  experiment_pois(input) %>%
    mutate(lambda = as.factor(lambda),
           n = as.factor(n)) %>%
    ggplot(aes(x = emv))+
    geom_density(aes(col = n), linewidth = 1)+
    facet_wrap(vars(lambda),
               scales = "free_x",
               labeller = labeller(lambda = ~ paste("Lambda:", .x)))+
    theme_bw()+
    labs(title = "Densidade do EMV da distribuição de Poisson.",
         x = "EMV", y = "Densidade", col = "Amostra")+
    scale_color_brewer(palette="Set2")

}



