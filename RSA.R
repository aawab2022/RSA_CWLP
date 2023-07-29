RSA <- function(func, lb, ub, d, popSize, MaxItr){
  best_f <- Inf
  fnEval <- c()
  fn <- generateFunc(func)
  population <- matrix(runif(d*popSize, lb,ub), ncol = d, nrow = popSize)
  plottingVector <- c()
  Alpha <- 0.2
  Beta <- 0.005
  for(i in 1:popSize){
    fni <- fn(population[i,])
    fnEval <- append(fnEval, fni)
    if(fni <= best_f){
      best_f <- fni
      best_p <- population[i,]
    }
  }
  for(i in 1:MaxItr){
    ES <- 2 * runif(1) * (1 - (i/MaxItr))
    for(k in popSize){
      xnew = c()
      for(j in 1:d){
        R <- best_p[j] - population[sample(1:popSize, 1), j]/(best_p[j]+2^-52)
        P <- Alpha + (population[k, j] - mean(population[k,]))/(best_p[j]*(ub-lb)+2^-52)
        Eta <- best_p[j] * P
        if(i < MaxItr/4){
          xnew <- append(xnew, best_p[j]-Eta*Beta-R*runif(1))
        }else if(i<MaxItr/2 & i>=MaxItr/4){
          xnew <- append(xnew, best_p[j]*population[sample(1:popSize,1),j] * ES * runif(1))
        }else if(i<3 * MaxItr/4 & i >= MaxItr/2){
          xnew <- append(xnew, best_p[j] * P * runif(1))
        }else{
          xnew <- append(xnew, best_p[j]-Eta * 2^-52 - R * runif(1))
        }
      }
      xnew[xnew >ub] <- ub
      xnew[xnew <lb] <- lb
      fnNew <- fn(xnew)
      if(fnNew < fnEval[k]){
        population[k,] <- xnew
        fnEval[k] <- fnNew
      }
      if(fnNew < best_f){
        best_p <- xnew
        best_f <- fnNew
      }
      plottingVector <- append(plottingVector, best_f)
    }
  }
  return(list(best_f, best_p, plottingVector))
}

generateFunc <- function(func){
  if(func == "Sphere"){
    fn <- (function(x) sum(x^2))
  }else if(func == "ackley"){
    fn <- (function(x)ackley(x))
  }else if (func == "bukin"){
    fn <- (function(x)bukin(x))
  }else if(func == "crossit"){
    fn <- (function(x)crossit(x))
  }else if(func == "drop_wave"){
    fn <- (function(x)drop_wave(x))
  }else if(func == "EGGHOLDER"){
    fn <- (function(x)EGGHOLDER(x))
  }else if(func == "GRAMACY_LEE"){
    fn <- (function(x)GRAMACY_LEE(x))
  }else if(func == "griewank"){
    fn <- (function(x)griewank(x))
  }else if(func == "HOLDER_TABLE"){
    fn <- (function(x)HOLDER_TABLE(x))
  }
}




