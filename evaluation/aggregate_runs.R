source("myplothelpers.R")

## How to use this script:
##


## The latest folder is:
## 

difficulties <- c(34, 36, 38, 40, 42, 44, 46, 48)
style <- "blockchain"
nodes <- 0:1

do.experiment1 <- TRUE

dates.bc.exp1 <- c("12-03-2018")

dates.exp1 <- dates.bc.exp1

data.base <- "~/localestimation/"

report.dir <- "~/Dropbox/mypapers/ANTS2018/img/"
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
                            X <- tryCatch(read.table(trials.name, skip = 1), error=function(e) NULL)
                            if (nrow(X) != 0 && !is.null(X)){

                                ## extract last row
                                X <- X[nrow(X), ]
                                
                                X$difficulty = round(d / (100 - d), 2)
                                X$actual = d / 100
                                X$predicted = 1 - X$V3 / 10^7
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

df$error <- df$actual - df$predicted

df$consWhite <- df$predicted < 0.5

df.agg <- data_summary(df, varname="consWhite", groupnames = c("actual"))

write.csv(df, sprintf("experiment1_%s.csv", style), row.names = FALSE, quote=FALSE)


source("myplothelpers.R")
plot.error.gg(df,
              xlab=expression("Actual ("* rho['w']*")"),
              ylab=expression("Predicted ("* hat(rho)['w']*")"),
              sprintf("exp1_error.pdf"),
              report.dir)  

source("myplothelpers.R")
plot.cons.gg(df,
              xlab=expression("Actual ("* rho['w']*")"),
              ylab=expression("Consensus Time"),
              sprintf("constime.pdf"),
              report.dir)  



# TODO: Continuous value supplied to discrete scale
source("myplothelpers.R")
plot.exit.prob.gg1(df.agg,
                  xlab=expression("Actual ("* rho['w']*")"),
                  ylab=expression("Exit probability"),
                  sprintf("exitprob.pdf"),
                  report.dir)  
