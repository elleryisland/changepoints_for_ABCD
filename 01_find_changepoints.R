
# LIBRARIES 
if(!require(devtools)){
  install.packages("devtools")}

if(!require(mvtnorm)){
  install.packages("mvtnorm")}

if(!require(mixedcpt)){
  devtools::install_github("mfiecas/mixedcpt")}

# load and clean data 
# create variable that identifies groups (denoted 'group' below)
# prewhiten each time series 
# ensure time series format is correct: each time series matrix (denoted 'X' below) is formatted such that the rows index time and the columns index the region

# parameters for changepoints
P <- dim(X)[2]
LENGTH <- dim(X)[1] # time = rows
minseglen <- 2*P # minimum segment length
cluster.size <- nlevels(group)  # number of groups
pen <- (choose(cluster.size + 1, 2) + cluster.size)*log(LENGTH) # penalty 
      
# changepoints
cpts <- PELT.lmec(data = X,
                  group.id = group,
                  pen = pen,
                  minseglen = minseglen,
                  equal.variance=TRUE)
      

