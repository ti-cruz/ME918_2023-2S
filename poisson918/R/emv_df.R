#' @importFrom purrr map
#' @importFrom dplyr bind_rows mutate select n

emv_df <- function(lista){
  map(lista,
      \(x) data.frame(emv=mean(x))) |>
    bind_rows() |>
    mutate(i = 1:n()) |>
    select(i, emv)
}
