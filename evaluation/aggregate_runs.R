source("myplothelpers.R")

## April 2018
## Volker Strobel
## This script creates the plots for the ANTS 2018 paper

difficulties <- seq(34, 48, 2)
thresholds <- seq(24 * 10^4, 6 * 10 ^ 4, - 2 * 10^4)
num.byz <- 0:12
style <- "blockchain"
nodes <- 0:1

do.experiment1 <- TRUE

dates.bc.exp1 <- c("12-03-2018")
dates.bc.exp2 <- c("14-03-2018")


dates.exp1 <- dates.bc.exp1
dates.exp2 <- dates.bc.exp2
dates.exp3 <- dates.bc.exp3

data.base <- "~/localestimation/"

report.dir <- "~/Dropbox/mypapers/ANTS2018/llncs-ants/img/"
N = 20


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
create.df.exp3 <- function(dates, max.trials=30, safe=T) {
    d <- 40
    df <- data.frame()
    t <- 140000
        for (dat in dates) {
            for (i in 1:max.trials) {
                for (b in num.byz) {
                    for (node in nodes){

                        if (safe) {
                            trials.name <- sprintf("%s/experiment3-safe-newSC-all-node%d-%s/%d/num20_black%d_byz%d_run%d-blockchain.RUN1", data.base, node, dat, t, d, b, i)
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


df2$error <- df2$actual - df2$predicted
df2$absError <- abs(df2$error)
df2$squaredError <- df2$error * df2$error


dates.bc.exp3.nonsecure <- c("21-03-2018") ## non-secure
dates.bc.exp3.secure <- c("30-03-2018") ## secure

df3.safe <- create.df.exp3(dates=dates.bc.exp3.secure, safe=T) ## Iterate over runs and create big df
df3.safe <- df3.safe[df3.safe$byz < 10, ]
df3.safe$error <- df3.safe$actual - df3.safe$predicted
df3.safe$absError <- abs(df3.safe$error)
df3.safe$squaredError <- df3.safe$error * df3.safe$error
df3.safe$consWhite <- df3.safe$predicted < 0.5


df3.nonsafe <- create.df.exp3(dates=dates.bc.exp3.nonsecure, safe=F) ## Iterate over runs and create big df
df3.nonsafe <- df3.nonsafe[df3.nonsafe$byz < 10, ]
df3.nonsafe$error <- df3.nonsafe$actual - df3.nonsafe$predicted
df3.nonsafe$absError <- abs(df3.nonsafe$error)
df3.nonsafe$squaredError <- df3.nonsafe$error * df3.nonsafe$error
df3.nonsafe$consWhite <- df3.nonsafe$predicted < 0.5



df$consWhite <- df$predicted < 0.5
df$absError <- abs(df$error)


df$consWhite <- df$predicted < 0.5

df.agg <- data_summary(df, varname="consWhite", groupnames = c("actual"))
df.agg2 <- data_summary(df, varname=c("absError"), groupnames = c("actual"))
df.agg3 <- data_summary(df, varname=c("blockchain_size_kB"), groupnames = c("actual"))

write.csv(df, "experiment1_ants2018.csv", row.names = FALSE, quote=FALSE)
write.csv(df2, "experiment2_ants2018.csv", row.names = FALSE, quote=FALSE)
write.csv(df3.nonsafe, "experiment3_nonsafe_ants2018.csv", row.names = FALSE, quote=FALSE)
write.csv(df3.safe, "experiment3_safe_ants2018.csv", row.names = FALSE, quote=FALSE)



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


## Experiment 2 
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

source("myplothelpers.R")
plot.cons.by.tau.gg(df2,
                    xlab=expression("Threshold ("*tau*")"),
                    ylab=expression("Consensus Time"),
                    sprintf("exp2-constime.pdf"),
                    report.dir)  


## Experiment 3 non-safe
source("myplothelpers.R")
plot.MAE.by.byz.gg(df3.nonsafe,
                  xlab=expression("Number of Byzantine robots (k)"),
                  ylab=expression("Mean absolute error"),
                  sprintf("MAE_byz-nonsecure.pdf"),
                  report.dir) 


## Experiment 3 safe
source("myplothelpers.R")
plot.MAE.by.byz.gg(df3.safe,
                  xlab=expression("Number of Byzantine robots (k)"),
                  ylab=expression("Mean absolute error"),
                  sprintf("MAE_byz-secure.pdf"),
                  report.dir) 
