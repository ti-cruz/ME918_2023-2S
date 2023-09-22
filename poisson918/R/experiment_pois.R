#' @importFrom dplyr bind_rows mutate
#' @importFrom purrr map
#' @export
#'
#' @title Resultados de amostras do EMV.
#'
#' @param list: Lista ou um conjunto de lista com
#' amostra desejavél para simular.
#' @param n: Tamanho das amostras
#' @param n_replicas: Número de réplicas da simulação.
#' @param lambda:  Parâmetro utilizado na simulação.
#'
#' @returns resultados de amostras do Estimador de Máxima Verossimilhança
#' para um par de `n` e \eqn{\lambda}.
#'
#' @examples
#' configs <- list(
#'   list(n = 10, n_replicas = 2, lambda = 3.14),
#'   list(n = 50, n_replicas = 3, lambda = 3.14),
#'   list(n = 50, n_replicas = 2, lambda = 20))
#' experiment_pois(configs)

experiment_pois <- function(listas){

  map(listas, \(x) emv_df(list_rpois(n = x$n,
                                     n_replicas = x$n_replicas,
                                     lambda = x$lambda)) |>
        mutate(n = x$n,
               lambda = x$lambda)) |>
    bind_rows(.id = "expert")

}




