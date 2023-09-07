source("executa.R")

dir <- getwd()

for (i in 1:length(planos)) {
  
  distribution <- planos[[i]]$distribution
  obs <- as.integer(planos[[i]]$obs)
  result <- i
  
  if(distribution %in% "normal"){
    
    mu <- planos[[i]]$mu
    sigma2 <- planos[[i]]$sigma2
  
  
  save(distr_plano = distribution,
       obs_plano = obs,
       mu_plano = mu,
       sigma2_plano = sigma2,
       result_plano = result,
       file = paste0(dir, "/resultados/", "simulacao", 
                            ifelse(i < 10, 
                                   as.character(paste0("0",i)),
                                   as.character(i))
                            ,".RData"))
  }
  
  if(distribution %in% "poisson"){
    
    lambda <- planos[[i]]$lambda
    
    save(distr_plano = distribution,
         obs_plano = obs,
         lambda_plano = lambda,
         result_plano = result,
         file = paste0(dir, "/resultados/", "simulacao", 
                       ifelse(i < 10, 
                              as.character(paste0("0",i)),
                              as.character(i))
                       ,".RData"))
  }
  
  if(distribution %in% "bernoulli"){
    
    p <- planos[[i]]$p
    
    save(distr_plano = distribution,
         obs_plano = obs,
         p_plano = p,
         result_plano = result,
         file = paste0(dir, "/resultados/", "simulacao", 
                       ifelse(i < 10, 
                              as.character(paste0("0",i)),
                              as.character(i))
                       ,".RData"))
  }
}

# 01: bernoulli
# 04: normal
# 93: poisson