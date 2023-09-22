#' @importFrom purrr map
#' @export
#'
#' @title Amostra da distribuição Poisson
#'
#' @description
#' A função deve retornar uma lista com `n_replicas` elementos, sendo cada
#' elemento uma amostra de tamanho `n`(vetor) da distribuição Poisson
#' com parâmetro lambda.
#'
#' @param n: Tamanho das amostras
#' @param n_replicas: Número de réplicas da simulação.
#' @param lambda:  Parâmetro utilizado na simulação.
#'
#' @returns listas de replicações da distribuição de Poisson.
#'
#' @examples
#' list_rpois(n = 10, n_replicas = 2, lambda = 4)
#'

list_rpois <- function(n, n_replicas, lambda){
  map(rep(n, n_replicas), \(x) rpois(x, lambda))
}



