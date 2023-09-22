#' @importFrom purrr cross
#' @export

# Parte extra

pois_extras <- function(n_p, lambda_p, n_replicas_p){

  cross(list(n=n_p,
             lambda=lambda_p,
             n_replicas=n_replicas_p))

}


