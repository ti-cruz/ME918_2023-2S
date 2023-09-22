#' @importFrom tidyr expand_grid
#' @importFrom purrr transpose
#' @export
#'
#' @title Combinações de `n` amostra, \eqn{\lambda} e `replicas`.
#'
#' @param n_p: vetor com valores de `n` amostra.
#' @param lambda_p vetor com valores de \eqn{\lambda}.
#' @param n_replicas_p Único valor de replicas.
#'
#' @returns Lista com todas as combinações de `n` amostra,
#' \eqn{\lambda} e `replicas`.
#'
#' @examples
#' pois_extras2(n_p=c(10, 50, 100),
#'              lambda_p=c(5, 20),
#'              n_replicas_p=2)

# Parte extra (2 opção)


pois_extras2 <- function(n_p, lambda_p, n_replicas_p){

  expand_grid(n = n_p, lambda = lambda_p,
              n_replicas = n_replicas_p) |>
    as.list() |>
    transpose()

}

