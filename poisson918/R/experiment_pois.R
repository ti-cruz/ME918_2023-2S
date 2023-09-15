#' @importFrom dplyr bind_rows mutate
#' @importFrom magrittr '%>%'
#' @importFrom purrr map
#' @export

experiment_pois <- function(listas){

  map(listas, \(x) emv_df(list_rpois(n = x$n,
                                     n_replicas = x$n_replicas,
                                     lambda = x$lambda)) %>%
        mutate(n = x$n,
               lambda = x$lambda)) %>%
    bind_rows(.id = "expert")

}




