library(purrr)
library(devtools)
library(usethis)

# list(distribution = "poisson", lambda = 2.0, obs = 20)

# list(distribution = "normal", mu = 12, sigma2 = 1.0 , obs = 20)

# list(distribution = "bernoulli", p = 0.3, obs = 30)


Simul_Dist <- function(Amostra_distri){
  if (Amostra_distri$distribution %in% "poisson") {
    return(rpois(Amostra_distri$obs, lambda = Amostra_distri$lambda))
    
  }
  
  if (Amostra_distri$distribution %in% "normal") {
    return(rnorm(Amostra_distri$obs, mean = Amostra_distri$mu, 
          sd = sqrt(Amostra_distri$sigma2)))
  }
  
  if (Amostra_distri$distribution %in% "bernoulli") {
    return(rbinom(Amostra_distri$obs, size = 1, prob = Amostra_distri$p))
  }
  
}

Simul_Dist(list(distribution = "bernoulli", p = 0.3, obs = 30))



