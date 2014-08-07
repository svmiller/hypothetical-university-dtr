

## Simulate democratic thermometer ratings at a hypothetical 20,000-student university.
## Pollock III (2012, Chp. 6) conjectured about a 20,000-student university whose known population thermometer rating toward Democrats was 58. 
## Population standard deviation is assumed to be 24.8
## We will simulate that here with this code. However, we have to bound the ratings between 0 and 100.
## Let's write a bootstrap function toward that end.
###############################################################################################################################################


set.seed(8675309) # for reproducibility
getwd() # get workign directory
setwd("~/Dropbox/teaching/posc3410/hypothetical-university") # set working directory

## After some consideration, a beta distribution does the job best.
## http://en.wikipedia.org/wiki/Beta_distribution

GoGoGadgetBootstrap <- function(n,mean,sd,lowerbound,upperbound){
range <- upperbound - lowerbound
m <- (mean - lowerbound)/range
s <- sd/range
a <- (m^2 - m^3 - m*s^2)/s^2 # calculate alpha for rbeta
b <- (m-2*m^2+m^3-s^2+m*s^2)/s^2 # calculate beta for rbeta
data <- rbeta(n,a,b) # generate the data
data <- lowerbound + data * range # squeeze it within the bounds.
return(data)
}

DTR <- GoGoGadgetBootstrap(20000,58,24.8,0,100) # Create the university, assign it to DTR
DTR <- round(DTR) # Round thermometer ratings to integers, since students probably don't think in decimals toward the Dems.

