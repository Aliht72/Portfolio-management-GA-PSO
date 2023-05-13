Modern Optimization Project
This project explores the use of two modern optimization methods, Genetic Algorithm and Particle Swarm, to find the best shape ratio for 10 valuable S&P500 assets.

Installation
To run the scripts, you will need to have R installed on your machine. The following packages are also required:

GA
PSO
quantmod
xts
tidyverse
You can install these packages by running the following code in R:

install.packages(c("GA", "PSO", "quantmod", "xts", "tidyverse"))

Usage
To use this code, clone the repository and open optimisation.R in RStudio. Then, run the script to execute the optimization methods.

The code takes historical data for 10 S&P500 assets and finds the optimal shape ratio that maximizes the Sharpe Ratio for a given time period. The results of the optimization are printed to the console and a plot is generated to show the performance of the optimized portfolios.

Contributing
Pull requests are welcome. For major changes, please open an issue first to discuss what you would like to change.

License
This project is licensed under the MIT License - see the LICENSE file for details.

Acknowledgments
The GA package for R: https://cran.r-project.org/web/packages/GA/index.html
The PSO package for R: https://cran.r-project.org/web/packages/PSO/index.html
Yahoo Finance for providing historical stock data: https://finance.yahoo.com/
Feel free to customize this text to fit your project's specific details. Good luck with your optimization project!
