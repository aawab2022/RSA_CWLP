# Reptile Search Algorithm for Solving The Capacitated Warehouse Location Problem
The files in this repository are coded using R programming and they are used to solve the capacitated warehouse location problem (CWLP) using a population based metaheuristic, namely the reptile searhc algorithm (RSA). 
The (import data.R) is used to import the benchmark problem data from one of the popular operations research benchmarks sites called OR-library. It returns two dataframes that are related to the fixed costs of the warehouses and the transportation costs. In addition, it returns the demand data as a numeric vector.
The (RSA_WLP.R) is the code of the proposed RSA for solving CWLP. It contains two functions, which are RSA_WLP and WLP_solution. RSA_WLP function uses WLP_solution heuristic function in order to find the optimized solution using the RSA algorithm.
The (Testing.R) is used to test the implementation of the proposed RSA code for solving CWLP. 
