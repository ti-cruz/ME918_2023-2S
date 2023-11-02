#' @importFrom ggplot2 ggplot aes geom_density
#' geom_histogram theme_classic labs
#'
#' @export

Graf_den_hist <- function(df, variavel, tipo){

  if(!tipo %in% c("densidade", "histograma")){
    stop(paste("O gráfico", tipo,
                       "não está disponivel na função"))
    }

  if(!variavel %in% colnames(df)){
    stop(paste("A variavél", variavel,
                  "não existe no banco de dados"))}

  if(!class(df[,variavel])
     %in% c("numeric", "integer")){
   stop(paste("A variavél",
                 variavel,
                 "não é numerica"))
    }

  if (tipo %in% "densidade") {
    return(df |>
      ggplot(aes(x = .data[[variavel]])) +
      geom_density() +
      theme_classic()#+
      #labs(title = paste0("Densidade de ", {{variavel}}))
      )
    warning("dasfa")
  }
  if (tipo %in% "histograma") {
    return(df |>
      ggplot(aes(x = .data[[variavel]])) +
      geom_histogram() +
      theme_classic()#+
        #labs(title = paste0("Histograma de ",
        #                    deparse(substitute({{variavel}}))
                            #)
             #)
      )
  }
}
