source("myplothelpers.R")

## How to use this script:
##


## The latest folder is:
## 

difficulties <- c(34, 36, 38, 40, 42, 44, 46, 48)
thresholds <- c(240000, 220000, 200000, 180000, 160000, 140000, 120000, 100000, 80000, 60000)
num.byz <- 0:12
style <- "blockchain"
nodes <- 0:1

do.experiment1 <- TRUE

dates.bc.exp1 <- c("12-03-2018")
dates.bc.exp2 <- c("14-03-2018")

dates.bc.exp3 <- c("21-03-2018") ## non-safe

dates.bc.exp3 <- c("29-03-2018", "30-03-2018") ## safe

dates.exp1 <- dates.bc.exp1
dates.exp2 <- dates.bc.exp2
dates.exp3 <- dates.bc.exp3

data.base <- "~/localestimation/"

report.dir <- "~/Dropbox/mypapers/ANTS2018/llncs-ants/img/"
N = 20

dec2node <- function(dec) {
    if (dec == 1)
        return(0)
    if (dec == 2)
        return(2)
    if (dec == 3)
        return(5)
}

# Experiment 1 (Increasing difficulty)
create.df.exp1 <- function(max.trials=50) {
    df <- data.frame()
        for (dat in dates.exp1) {
            for (i in 1:max.trials) {
                for (d in difficulties) {
                    for (node in nodes){

                        trials.name <- sprintf("%s/experiment1-node%d-%s/num20_black%d_byz0_run%d-blockchain.RUN1", data.base, node, dat, d, i)

                        #print(trials.name)
                        if (file.exists(trials.name)) {
                            X <- tryCatch(read.table(trials.name, header=T), error=function(e) NULL)
                            if (nrow(X) != 0 && !is.null(X)){

                                ## extract last row
                                X <- X[nrow(X), ]
                                
                                X$difficulty = round(d / (100 - d), 2)
                                X$actual = d / 100
                                X$predicted = 1 - X$mean / 10^7
                             if (nrow(df) == 0) {
                                 df <- X
                             } else  {
                                 df <- rbind(df, X)
                             }
                         }
                        }           
                    }
                }
            }
        }
    return(df)    
}

# Experiment 2 (Varying threshold)
create.df.exp2 <- function(max.trials=30) {
    d <- 40
    df <- data.frame()
        for (dat in dates.exp2) {
            for (i in 1:max.trials) {
                for (t in thresholds) {
                    for (node in nodes){

                        trials.name <- sprintf("%s/experiment1_decision2-node%d-%s/%d/num20_black%d_byz0_run%d-blockchain.RUN1", data.base, node, dat, t, d, i)
                        if (file.exists(trials.name)) {
                            X <- tryCatch(read.table(trials.name, header=T), error=function(e) NULL)
                            if (nrow(X) != 0 && !is.null(X)){

                                ## extract last row
                                X <- X[nrow(X), ]

                                X$threshold <- t / 10^7
                                X$difficulty = round(d / (100 - d), 2)
                                X$actual = d / 100
                                X$predicted = 1 - X$mean / 10^7
                             if (nrow(df) == 0) {
                                 df <- X
                             } else  {
                                 df <- rbind(df, X)
                             }
                            }
                        }           
                    }
                }
            }
        }
    return(df)    
}


## Experiment 3 (Increasing the number of Byzantine robots)
create.df.exp3 <- function(max.trials=30, safe=T) {
    d <- 40
    df <- data.frame()
    t <- 140000
        for (dat in dates.exp3) {
            for (i in 1:max.trials) {
                for (b in num.byz) {
                    for (node in nodes){

                        if (safe) {
                            trials.name <- sprintf("%s/experiment3-safe-node%d-%s/%d/num20_black%d_byz%d_run%d-blockchain.RUN1", data.base, node, dat, t, d, b, i)
                        } else {
                            trials.name <- sprintf("%s/experiment3-node%d-%s/%d/num20_black%d_byz%d_run%d-blockchain.RUN1", data.base, node, dat, t, d, b, i)
                            }
                        if (file.exists(trials.name)) {
                            X <- tryCatch(read.table(trials.name, header=T), error=function(e) NULL)
                            if (nrow(X) != 0 && !is.null(X)){

                                ## extract last row
                                X <- X[nrow(X), ]
                                X$byz <- b
                                X$threshold <- t / 10^7
                                X$difficulty = round(d / (100 - d), 2)
                                X$actual = d / 100
                                X$predicted = 1 - X$mean / 10^7
                             if (nrow(df) == 0) {
                                 df <- X
                             } else  {
                                 df <- rbind(df, X)
                             }
                            }
                        }
                        }
                }
            }
        }
    return(df)    
}



data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
 return(data_sum)
}

df <- create.df.exp1() ## Iterate over runs and create big df
df <- df[df$clock < 1000, ]

df$error <- df$actual - df$predicted

df2 <- create.df.exp2() ## Iterate over runs and create big df

df3.safe <- create.df.exp3(safe=T) ## Iterate over runs and create big df
df3.safe$error <- df3.safe$actual - df3.safe$predicted
df3.safe$absError <- abs(df3.safe$error)
df3.safe$squaredError <- df3.safe$error * df3.safe$error
df3.safe$consWhite <- df3.safe$predicted < 0.5


df3.nonsafe <- create.df.exp3(safe=F) ## Iterate over runs and create big df
df3.nonsafe$error <- df3.nonsafe$actual - df3.nonsafe$predicted
df3.nonsafe$absError <- abs(df3.nonsafe$error)
df3.nonsafe$squaredError <- df3.nonsafe$error * df3.nonsafe$error
df3.nonsafe$consWhite <- df3.nonsafe$predicted < 0.5



df$consWhite <- df$predicted < 0.5


df2$error <- df2$actual - df2$predicted



df$absError <- abs(df$error)

df2$absError <- abs(df2$error)
df2$squaredError <- df2$error * df2$error

df$consWhite <- df$predicted < 0.5

df.agg <- data_summary(df, varname="consWhite", groupnames = c("actual"))
df.agg2 <- data_summary(df, varname=c("absError"), groupnames = c("actual"))
df.agg3 <- data_summary(df, varname=c("blockchain_size_kB"), groupnames = c("actual"))

write.csv(df, sprintf("experiment1_%s.csv", style), row.names = FALSE, quote=FALSE)


source("myplothelpers.R")
plot.error.gg(df,
              xlab=expression("Actual ("* rho['b']*")"),
              ylab=expression("Predicted ("* hat(rho)['b']*")"),
              sprintf("exp1_error.pdf"),
              report.dir)  

source("myplothelpers.R")
plot.cons.gg(df,
              xlab=expression("Actual ("* rho['b']*")"),
              ylab=expression("Consensus Time"),
              sprintf("constime.pdf"),
              report.dir)  


source("myplothelpers.R")
plot.exit.prob.gg1(df.agg,
                  xlab=expression("Actual ("* rho['b']*")"),
                  ylab=expression("Exit probability"),
                  sprintf("exitprob.pdf"),
                  report.dir)  


source("myplothelpers.R")
plot.abs.error.gg(df,
                  xlab=expression("Actual ("* rho['b']*")"),
                  ylab=expression("Mean Absolute Error"),
                  sprintf("absMeanError.pdf"),
                  report.dir)  


source("myplothelpers.R")
plot.blockchain.size.gg(df,
                  xlab=expression("Actual ("* rho['b']*")"),
                  ylab=expression("Blockchain size (MB)"),
                  sprintf("blockchain_size.pdf"),
                  report.dir)  

source("myplothelpers.R")
plot.blockchain.size.by.tau.gg(df2,
                  xlab=expression("Threshold ("*tau*")"),
                  ylab=expression("Blockchain size (MB)"),
                  sprintf("blockchain_size_tau.pdf"),
                  report.dir)  



source("myplothelpers.R")
plot.error.by.tau.gg(df2,
                  xlab=expression("Threshold ("*tau*")"),
                  ylab=expression("Error"),
                  sprintf("error_tau.pdf"),
                  report.dir)  

source("myplothelpers.R")
plot.MAE.by.tau.gg(df2,
                  xlab=expression("Threshold ("*tau*")"),
                  ylab=expression("Mean absolute error"),
                  sprintf("MAE_tau.pdf"),
                  report.dir)  

source("myplothelpers.R")
plot.MSE.by.tau.gg(df2,
                  xlab=expression("Threshold ("*tau*")"),
                  ylab=expression("Mean squared error"),
                  sprintf("MSE_tau.pdf"),
                  report.dir)  


## Experiment 3 non-safe
source("myplothelpers.R")
plot.error.by.byz.gg(df3.nonsafe,
                  xlab=expression("Number of Byzantines"),
                  ylab=expression("Error"),
                  sprintf("error_byz-nonsafe.pdf"),
                  report.dir) 


## Experiment 3 safe
source("myplothelpers.R")
plot.error.by.byz.gg(df3.safe,
                  xlab=expression("Number of Byzantines"),
                  ylab=expression("Error"),
                  sprintf("error_byz-safe.pdf"),
                  report.dir) 
