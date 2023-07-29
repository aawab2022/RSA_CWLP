RSA_WLP <- function(Alpha, Beta, lb, ub, capacityFixedCost, Demand, transportationCost, popSize, MaxItr){
  num_warehouses = nrow(capacityFixedCost)
  num_customers = length(Demand)
  # Alpha <- 0.1
  # Beta <- 0.005
  best_f <- Inf
  fnEval <- c()
  plottingVector <- c()
  population <- matrix(runif(num_warehouses*popSize, lb,ub), ncol = num_warehouses, nrow = popSize)
  
  for(i in 1:popSize){
    fni <- WLP_solution(num_warehouses, num_customers, transportationCost, capacityFixedCost, Demand, population[i,])[[1]]
    fnEval <- append(fnEval, fni)
    if(fni <= best_f){
      best_f <- fni
      best_p <- population[i,]
    }
  }
  for(i in 1:MaxItr){
    # print(i)
    
    ES <- 2 * runif(1) * (1 - (i/MaxItr))
    for(k in 1:popSize){
      
      xnew = c()
      for(j in 1:num_warehouses){
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
      # xnew[xnew >ub] <- ub
      # xnew[xnew <lb] <- lb
      fnNew <- WLP_solution(num_warehouses, num_customers, transportationCost, capacityFixedCost, Demand, xnew)[[1]]
      if(fnNew < fnEval[k]){
        population[k,] <- xnew
        fnEval[k] <- fnNew
      }
      if(fnNew < best_f){
        best_p <- xnew
        best_f <- fnNew
      }
      
    }
    plottingVector <- append(plottingVector, best_f)
  }
  return(list(best_f, best_p, plottingVector))
}
##################################################################################
WLP_solution <- function(num_warehouses, num_customers, transCosts, capacityFixedCost, Demand, priortyStruct){
  
  #print(priortyStruct)
  demand <- Demand
  # Generate random capacity for each warehouse
  capacity <- capacityFixedCost$Capacity
  FixedCost <- capacityFixedCost$FixedCost
  # Generate random allocation of customers to warehouses
  # The allocation matrix is the proportional matrix related to the transportation cost
  allocation <- matrix(0, nrow = num_customers, ncol = num_warehouses)
  # The allocationAssign matrix is related to the warehouse fixed costs
  allocationAssign <- matrix(0, nrow = num_customers, ncol = num_warehouses)
  for (i in 1:num_customers) {
    while(demand[i] > 0){
      #browser()
      possible_warehouses <- which(capacity >= demand[i])
      
      if (length(possible_warehouses) > 0) {
        warehouse <- possible_warehouses[which(priortyStruct[possible_warehouses] == max(priortyStruct[possible_warehouses]))][1]
        # warehouse <- sample(possible_warehouses, 1)

        
        allocation[i, warehouse] <- demand[i]/Demand[i]
        allocationAssign[i, warehouse] <- 1
        capacity[warehouse] <- capacity[warehouse] - demand[i]
        demand[i] <- 0
      }else{
        possible_warehouses <- which(capacity > 0)

        if(length(possible_warehouses) > 0){
          warehouse <- possible_warehouses[which(priortyStruct[possible_warehouses] == max(priortyStruct[possible_warehouses]))][1]

          # Calculate the proportion that is used to calculate the transportation cost
          allocation[i, warehouse] <- capacity[warehouse]/Demand[i]
          allocationAssign[i, warehouse] <- 1
          demand[i] <- demand[i] - capacity[warehouse]
          capacity[warehouse] <- 0
        }
      }
    }
    
  }

  transportationCost <- sum(allocation * transCosts)
  FixedCost <- sum(apply(allocationAssign, 2, function(i)if(sum(i)){1}else{0}) * FixedCost)
  totalCost <- transportationCost + FixedCost
  #totalCost <- transportationCost 
  openedWarehouses <- length(which(apply(allocation,2, sum)>1))
  
  return(list(totalCost, allocationAssign, allocation, openedWarehouses)) 
}

calculated_fixedCosts <- function(allocationAssign, FixedCost){
  totalFixedCosts <- 0
  for(i in 1:nrow(allocationAssign)){
    totalFixedCosts <- totalFixedCosts + sum(allocationAssign[i, ] * FixedCost)
  }
  return(totalFixedCosts)
}