library(parallel)
library(doParallel)
library("GA")
library(iterators)
library(foreach)

f = NULL
files = c("AAPL.csv","AMZN.csv","BRK-B.csv","FB2A.F.csv","GOOG.csv","JNJ.csv" ,"MSFT.csv" , "NVDA.csv" , "TSLA.csv" , "UEEC.csv" )
for (i in 1:length(files)) {
  csv = read.csv(files[i])
  csv = csv[,c("Date","Close")]
  names(csv) = c("Date",files[i])
  if (i == 1) f = csv
  else f = merge(f,csv)
}

for (i in 2:ncol(f)) {
  # Price time series of the i-th asset
  prices = f[,i] 
  
  # Price lagged by 1
  prices_prev = c(NA,prices[1:(length(prices)-1)]) 
  
  # Returns time series
  returns = (prices-prices_prev)/prices_prev 
  
  # Replace the i-th column with returns
  f[,i] = returns 
}
# Remove the first row with NAs and the Date column
asset_return = f[2:nrow(f),2:ncol(f)]


sharpe = function(x) {
  port.returns = portfolio_returns(x)
  
  return ((mean(port.returns)-(4*0.0078))/sqrt(var(port.returns)))
  
}

constraint = function(x) {
  boundary_constr = (sum(x)-1)**2   # "sum x = 1" constraint
  
  for (i in 1:length(x)) {
    boundary_constr = boundary_constr + 
      max(c(0,x[i]-1))**2 +  # "x <= 1" constraint
      max(c(0,-x[i]))**2     # "x >= 0" constraint
  }
  
  return (boundary_constr)
}

portfolio_returns = function(x) {
  port.returns = 0
  
  # Multiplication of the i-th asset by the i-th weight in "x"
  for (i in 1:length(x)) {
    port.returns = port.returns + asset_return[,i] * x[i]
  }
  
  return (port.returns)
}

sharpe = function(x) {
  port.returns = portfolio_returns(x)
  
  return ((mean(port.returns)-(4*0.0078))/sqrt(var(port.returns)))
  
}
constraint = function(x) {
  boundary_constr = (sum(x)-1)**2   # "sum x = 1" constraint
  
  for (i in 1:length(x)) {
    boundary_constr = boundary_constr + 
      max(c(0,x[i]-1))**2 +  # "x <= 1" constraint
      max(c(0,-x[i]))**2     # "x >= 0" constraint
  }
  
  return (boundary_constr)
}

obj = function(x) {
  # We want the maximum Sharpe ratio, so we multiply it by
  # -1 to fit an optimization problem
  
  return (-sharpe(x)+100*constraint(x))
}

std = function(x){
  port.returns = portfolio_returns(x)
  
  return (var(port.returns))
}
Mean = function(x) {
  port.returns = portfolio_returns(x)
  
  return (mean(port.returns))
}


Weight_Single = c(0.0092 , 0.0001 , 0.0015 , 0.0526 , 0.0027 , 0.0018, 0.0021 ,0.0003 , 0.5440 , 0.3853)
Weight_Blend = c(0.0159 , 0.0045 , 0.0154 , 0.0505 , 0.0023 ,0.0409 , 0.0478 , 0.0165 , 0.4221 , 0.3861)
Weight_Arithmatic = c(0.0436 , 0.0190 , 0.0598 , 0.0386 , 0.0709 , 0.0881 , 0.0868 , 0.0310 , 0.2537 , 0.3110)
