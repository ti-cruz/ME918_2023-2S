#' @importFrom dplyr mutate
#' @importFrom ggplot2 ggplot geom_density facet_wrap theme_bw labs
#' scale_color_brewer aes vars labeller
#' @export
#'
#' @title Gráficos de densidade do EMV da distribuição de Poisson.
#'
#' @param input: Resultado obtidos pela função `experiment_pois(lista)`
#'
#' @returns Gráfico da distribuição do EMV (empírica obtida através de
#' simulações) para diferentes valores de \eqn{\lambda} e tamanhos de
#' amostra `n`.
#'
#' @examples
#' configs <- list(
#'   list(n = 10, n_replicas = 200, lambda = 5),
#'   list(n = 50, n_replicas = 200, lambda = 5),
#'   list(n = 100, n_replicas = 200, lambda = 5),
#'   list(n = 10, n_replicas = 200, lambda = 20),
#'   list(n = 50, n_replicas = 200, lambda = 20),
#'   list(n = 100, n_replicas = 200, lambda = 20))
#' input <- experiment_pois(configs)
#' graf_pois(input)

graf_pois <- function(input){

  input |>
    mutate(lambda = as.factor(lambda),
           n = as.factor(n)) |>
    ggplot(aes(x = emv))+
    geom_density(aes(col = n), linewidth = 1)+
    facet_wrap(vars(lambda),
               scales = "free",
               labeller = labeller(lambda = ~ paste("Lambda:", .x)))+
    theme_bw()+
    labs(title = "Densidade do EMV da distribuição de Poisson.",
         x = "EMV", y = "Densidade", col = "Amostra")+
    scale_color_brewer(palette="Set2")

}



