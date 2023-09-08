library(jsonlite)
library(tidyverse)
library(yaml)

## Gabarito = Realizado em sala de aula.

# Parte 0

fromJSON(paste0("dados/relatorio1.json"), 
         simplifyDataFrame = FALSE)

# Parte 2

rl <- read_yaml("configuracoes.yaml")

lidos <- purrr::map(rl$arquivos, function(x) {
  read_json(file.path("dados", x))
})

rlt <- list()
for (i in 1:length(rl$arquivos)) { 
  rlt <- append(rlt, lidos[[i]])
}

## Gabarito 

### configuracoes <- yaml::read_yaml("configuracoes.yaml")

### purrr::map(configuracoes$arquivos, function(x) {
###   read_json(file.path("dados", x))
### }) -> lidos

### listao <- do.call(c, args = lidos)

# Parte 3

library(purrr)

obj <- map(rlt, data.frame)
df <- list_rbind(obj)

df <- df %>%
  mutate(horario = as.POSIXct(horario)) %>%
  as_tibble()

## Gabarito

### library(purrr)

### map(listao, as.data.frame) -> listao_df
### list_rbind(listao_df) -> tabelao

### library(tidyverse)
### tabelao <- tibble(tabelao)
### tabelao$horario <-  lubridate::as_datetime(tabelao$horario)

### tabelao %>%
###  filter(evento == "recalibragem") %>%
###  select(horario, evento) %>%
###  arrange(horario) %>%
###  mutate(total = 1:n()) -> tabela_r

# Parte 4

library(ggplot2)

df %>%
  filter(evento %in% "recalibragem") %>%
  select(horario, evento) %>%
  arrange(horario) %>%
  mutate(total = 1:n()) %>%
  ggplot(aes(x = horario, y = total)) +
  geom_line() +
  labs(title = "Quantidade de recalibragens acumuladas 
  a cada instante de tempo",
       x = "Horário",
       y = "Total")+
  theme_bw()

## Resposta: De acordo com o gráfico, podemos reparar que existe dois 
## periodos que foram causado dano no sistema de recalibagem. 

horario_vali <- df %>%
  filter(evento == "recalibragem") %>%
  select(horario) %>%
  mutate(anterior = lag(horario)) %>%
  mutate(diferenca = horario - anterior) %>%
  filter(diferenca > 8*60*60)

df_gg <- df %>%
  filter(horario < horario_vali$anterior[1] |
           horario > horario_vali$horario[1]) %>%
  filter(horario < horario_vali$anterior[2] |
           horario > horario_vali$horario[2])

## Duvida: Como transformaria os "filter()" em laço "for(){}" ?
## pois, se tiver "n" intevalos de 8 horas, fica impossibilitado de
## fazer a quantidade de "filter()" na mão.

## Gabarito

### ggplot(tabela_r, aes(x = horario, y = total)) +
### geom_line()

### tabelao %>%
###   filter(evento == "recalibragem") %>%
###   select(horario) %>%
###   mutate(anterior = lag(horario)) %>%
###   mutate(diferenca = horario - anterior) %>%
###   filter(diferenca > 8*60*60) -> horario_validos

### tabelao %>%
###   filter(horario < horario_validos$anterior[1] |
###            horario > horario_validos$horario[1]) %>%
###   filter(horario < horario_validos$anterior[2] |
###            horario > horario_validos$horario[2]) -> tabelao_bom

# Parte 5

combin <- expand.grid(v1 = c(T,F), v2 = c(T,F), v3 = c(T,F), v4 = c(T,F))
l <- unlist(apply(combin, 1, list), recursive = FALSE)
Nomes_comb <- lapply(l, function(x) names(x)[x])

lms_intensidade <- function(Nomes_variaveis){
  
  list_lms <- list()
  
  for (i in 1:length(Nomes_variaveis)) {
    df_lms <- df_gg %>%
      select(intensidade,{{Nomes_variaveis}}[[i]])
    
    list_lms <- append(list_lms, list(lm(intensidade~., data = df_lms)))
  }
  return(list_lms)
}

## lms_intensidade(Nomes_comb)

## Gabarito

### combin <- expand.grid(v1 = c(T,F), v2 = c(T,F), v3 = c(T,F), v4 = c(T,F))
### l <- unlist(apply(combin, 1, list), recursive = FALSE)
### nomes <- lapply(l, function(x) names(x)[x])

### f <- function(n){
###   aux <- tabelao_bom[, c("intensidade", n)]
  ###   lm(intensidade ~ ., data = aux)
### }

### map(nomes, f)
  