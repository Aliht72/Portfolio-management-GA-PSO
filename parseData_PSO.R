parseData <- function(data, firstcolumn, noRuns){
    col <- firstcolumn
    
    allstats <- (ncol(data)-1)/noRuns   #how many stats were collected. Omit the first column (Generations)
    cols <- seq(col,noRuns*allstats, by=allstats)
    subdata <- data[,cols]
    noGens <- nrow(data)
    pdata <- matrix(nrow = noGens, ncol = 3)
    for (i in 1:noGens){
      pdata[i,1] = i
      pdata[i,2] = mean(subdata$X.PSO1.stats.error[i] ,subdata$X.PSO2.stats.error[i] , subdata$X.PSO3.stats.error[i],subdata$X.PSO4.stats.error[i],subdata$X.PSO5.stats.error[i],subdata$X.PSO6.stats.error[i],subdata$X.PSO7.stats.error[i] , subdata$X.PSO8.stats.error[i] ,subdata$X.PSO9.stats.error[i] , subdata$X.PSO10.stats.error[i] ,subdata$X.PSO11.stats.error[i] , subdata$X.PSO12.stats.error[i], subdata$X.PSO13.stats.error[i] , subdata$X.PSO14.stats.error[i] , subdata$X.PSO15.stats.error[i],subdata$X.PSO16.stats.error[i] , subdata$X.PSO17.stats.error[i], subdata$X.PSO18.stats.error[i] , subdata$X.PSO19.stats.error[i] , subdata$X.PSO20.stats.error[i] , subdata$X.PSO21.stats.error[i] , subdata$X.PSO22.stats.error[i] , subdata$X.PSO23.stats.error[i], subdata$X.PSO24.stats.error[i] , subdata$X.PSO25.stats.error[i],subdata$X.PSO26.stats.error[i] ,subdata$X.PSO27.stats.error[i] , subdata$X.PSO28.stats.error[i] , subdata$X.PSO29.stats.error[i] , subdata$X.PSO30.stats.error[i])
      pdata[i,3] = 1.96*sd((subdata[i,]))/sqrt(noRuns)   #compute the length of error bar. 
    }
  
    return (pdata)
}

#p1 <-parseData(result_arith_crossover , 2, 30)
#p2 <- parseData(result_blend_crossover , 2 , 30)
#p3 <- parseData(result_single_crossover , 2 , 30)
