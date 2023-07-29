num_warehouses = nrow(capacityFixedCost)
num_customers = length(Demand)
result <- RSA_WLP(0.1, 0.005, -1, 1, capacityFixedCost, Demand, transCosts, 100, 100)
result[[1]]
temp1 <- WLP_solution(num_warehouses, num_customers, transCosts, capacityFixedCost, Demand, result[[2]])
temp <- WLP_solution(num_warehouses, num_customers, transCosts, capacityFixedCost, Demand, result[[2]])[[3]]
apply(temp1[[3]],2,function(col)sum(col * Demand))
rowsum(temp[[3]])