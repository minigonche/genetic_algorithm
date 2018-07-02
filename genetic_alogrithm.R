
#Script for trying out evolutionary (or genetic?) algorithms.
#The idea is to get to a certiain image (black and white) by means of evolution

library('imager')
library('reproducible')

#reads the target image

target <- load.image('target.jpeg')
df_target = as.data.frame(target)
plot(target)



get_kids = function(mom, dad, eps = 0.01)
{
  kids = list()
  
  n = nrow(mom)
  
  for(i in 1:(sample(1:max_kids, 1)))
  {
    kid = Copy(mom)
    
    kid$value = (mom$value + dad$value)/2
    
    #Random noise
    noise_ind = sample(1:n, as.integer(n*runif(1, 0.3, 1)))
    kid$value[noise_ind] = kid$value[noise_ind] + runif(length(noise_ind), -1*eps, eps)
    
      #Random assign dad
     dad_ind = sample(1:n, as.integer(n*runif(1, 0.3, 1)))
    kid$value[dad_ind] = dad$value[dad_ind]
    
    
    
      #Random assign mom
      mom_ind = sample(1:n, as.integer(n*runif(1, 0.3, 1)))
      kid$value[mom_ind] = mom$value[mom_ind]
    
    
    kid$value[kid$value > 1] = 1
    kid$value[kid$value < 0] = 0
    
    kids[[i]] = Copy(kid)
    
  }
  
  return(kids)
  
}

next_gen = function(survivors)
{
  
  next_gen = list()
  for(i in 1:(length(survivors)-1))
  {
    for(j in (i+1):length(survivors)-1)
    {
      kids = get_kids(survivors[[i]],survivors[[j]])
      for(k in 1:length(kids))
      {
        next_gen[[length(next_gen) + 1]] = Copy(kids[[k]])
      }
      
    }
  }
  
  return(next_gen)
  
}

get_fitness = function(surviviors)
{
  
  result = c()
  for(i in 1:length(surviviors))
  {
    result = c(result, sum(abs(df_target$value - surviviors[[i]]$value)))
  }
  
  return(result)
}




max_gen = 15
survivors = list()
gens = 50
max_kids = 15



for(i in 1:max_gen)
{
  adan = Copy(as.data.frame(target))
  adan$value =  runif(nrow(adan), 0, 1)
  survivors[[i]] = Copy(adan)
}


while(TRUE)
{
 
  for(i in 1:gens)
  {
    survivors = next_gen(survivors)
    fit = get_fitness(survivors)
    sel = sort(fit, index.return = TRUE)$ix[1:max_gen]
    survivors = survivors[sel]
    print(fit[sel[1]])
  }

  plot(as.cimg(survivors[[1]]))
  
}


