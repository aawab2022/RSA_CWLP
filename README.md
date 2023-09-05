# Reptile Search Algorithm for Solving The Capacitated Warehouse Location Problem
This GitHub repository contains R code to tackle the Capacitated Warehouse Location Problem (CWLP) utilizing the power of the Reptile Search Algorithm (RSA), a population-based metaheuristic. The repository offers a seamless solution to import problem data from the renowned Operations Research benchmark repository, OR-library, through the import_data.R script. It conveniently returns two essential dataframes for fixed warehouse costs and transportation costs, alongside the demand data as a numeric vector.
Repository Contents

    import_data.R: This script allows you to effortlessly fetch benchmark problem data from OR-library, providing dataframes for fixed warehouse costs, transportation costs, and demand data.

    RSA_WLP.R: Here, you'll find the core RSA algorithm for solving CWLP. This script comprises two functions, RSA_WLP and WLP_solution. The RSA_WLP function seamlessly integrates the WLP_solution heuristic function to optimize solutions using the RSA algorithm.

    Testing.R: This script serves as a comprehensive testing suite for validating the implementation of the RSA code when solving the CWLP.

We hope this repository simplifies your CWLP solving needs with the robust RSA algorithm. Please feel free to explore, contribute, and enhance the codebase as needed.
