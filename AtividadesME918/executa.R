source("simulador.R")
#Simul_Dist(list(distribution = "normal", mu = 45, sigma2 = 1.0 , obs = 60))

devtools::source_url("https://ime.unicamp.br/~ra137784/ME918/2023s2/lab01_ig.R")
planos <- input_lab01()

purrr::map(planos, Simul_Dist)
