library(jsonlite)
library(tidyverse)
library(yaml)

# Parte 0

fromJSON(paste0("dados/relatorio1.json"), 
         simplifyDataFrame = FALSE)

# Parte 2

rl <- read_yaml("configuracoes.yaml")
rl2 <- rl[[1]]

rlt <- list()
for (i in 1:length(rl$arquivos)) { 
  rlt <- append(rlt, read_json(paste0("dados/",rl2[i])))
}

# Parte 3

library(purrr)

obj <- map(rlt, data.frame)
df <- list_rbind(obj)

df <- df %>%
  mutate(horario = as.POSIXct(horario)) 

# Parte 4

library(ggplot2)

df %>%
  filter(evento %in% "recalibragem") %>%
  select(horario) %>%
  mutate(`Horário acumulado` =  as.integer(format(horario, "%H")),
         `Dia da Semana` = str_to_title(format(horario, "%A"))) %>%
  ggplot(aes(x = `Dia da Semana`, y = `Horário acumulado`)) +
  geom_point(alpha = 0.16, size = 4)+
  labs(title = "Dispersão dos horários acumulados de recalibragem 
no período dos registros.")+
  scale_x_discrete(labels = c("Domingo", "Segunda-Feira", 
                              "Terça-Feira", "Quarta-Feira", 
                              "Quinta-Feira", "Sexta-Feira",
                              "Sábado"))+
  theme_minimal()

## Resposta: De acordo com o gráfico, quanto mais a coloração das observações 
## são escuras, mais a quantidade de registro foi obtida no intervalo do tempo
## no dia da semana. Dessa maneira, vale relatar que existe dados ausente 
## no horario das 5:00:00 às 5:59:59 na quinta-feira.

l = 1
df_rem_8 <- df %>% arrange(horario)
for (i in 1:nrow(df)) {
  
  if (df[i,]$evento %in% "recalibragem") {
    
    if (abs(df[i,]$horario - df[l,]$horario) > 8) {
      df_rem_8  <- df_rem_8[-c(l:i-1),]
    }
    
    l=i+1
    }
}

# Parte 5

combin <- expand.grid(v1 = c(T,F), v2 = c(T,F), v3 = c(T,F), v4 = c(T,F))
l <- unlist(apply(combin, 1, list), recursive = FALSE)
Nomes_comb <- lapply(l, function(x) names(x)[x])

lms_intensidade <- function(Nomes_variaveis){
  
  list_lms <- list()
  
  for (i in 1:length(Nomes_variaveis)) {
    df_lms <- df_rem_8 %>%
      filter(evento %in% "medida") %>% 
      select(intensidade,{{Nomes_variaveis}}[[i]])
    
    list_lms <- append(list_lms, list(lm(intensidade~., data = df_lms)))
  }
  return(list_lms)
}

lms_intensidade(Nomes_comb)
  