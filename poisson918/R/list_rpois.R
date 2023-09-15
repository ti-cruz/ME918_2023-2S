#' @importFrom purrr map
#' @export

list_rpois <- function(n, n_replicas, lambda){
  map(rep(n, n_replicas), \(x) rpois(x, lambda))
}

