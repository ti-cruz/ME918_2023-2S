#' @importFrom purrr map
#' @importFrom dplyr bind_rows mutate select n
#' @importFrom magrittr '%>%'

emv_df <- function(lista){
  map(lista,
      \(x) data.frame(emv=sum(x)/length(x))) %>%
    bind_rows() %>%
    mutate(i = 1:n()) %>%
    select(i, emv)
}
