# Here's a (simulated) experiment, with a single subject and 500 categorization trials.
all.data <- read.csv('experiment-data.csv')
source('memory-limited-exemplar-model.R')
rm(sample.data.set)
rm(sample.training.data)

# Use optim() to fit the model to this data.
# Note: In optim() you can tell it to display updates as it goes with:
# optim( ... , control=list(trace=4))

exemplar.wrapper <- function(params){
  return(exemplar.memory.log.likelihood(all.data,params[1],params[2]))
}

optim.results <- optim(c(0.5,0.5),exemplar.wrapper,method="Nelder-Mead",control=list(trace=4))

# Now try fitting a restricted version of the model, where we assume there is no decay.
# Fix the decay.rate parameter to 1, and use optim to fit the sensitivity parameter.
# Note that you will need to use method="Brent" in optim() instead of Nelder-Mead. 
# The brent method also requires an upper and lower boundary:
# optim( ..., upper=100, lower=0, method="Brent")

exemplar.wrapper.decay <- function(params){
  return(exemplar.memory.log.likelihood(all.data,params[1],1))
}

optim.decay.results <- optim(c(0.5),exemplar.wrapper.decay,
                             upper=100,lower=0,method="Brent")

# What's the log likelihood of both models? (see the $value in the result of optiom(),
# remember this is the negative log likeihood, so multiply by -1.

#regular examplar: -251.1787

#fixed decay exemplar: -251.7536

# What's the AIC and BIC for both models? Which model should we prefer?

first.aic <- (2*2) - (2*log())
print(first.aic)

n <- as.numeric(max(rownames(all.data)))
first.bic <- ((-2)*log(10610.31)) + (2*log(n))
print(first.bic)

second.aic <- 2 - (2*log(10610.31))
print(second.aic)

second.bic <- ((-2)*log(10610.31)) + (log(n))
print(second.bic)

#Mine gets an x error

#### BONUS...
# If you complete this part I'll refund you a late day. You do not need to do this.

# Use parametric bootstrapping to estimate the uncertainty on the decay.rate parameter.
# Unfortunately the model takes too long to fit to generate a large bootstrapped sample in
# a reasonable time, so use a small sample size of 10-100 depending on how long you are
# willing to let your computer crunch the numbers.

# Steps for parametric bootstrapping:
# Use the best fitting parameters above to generate a new data set (in this case, that means
# a new set of values in the correct column for all.data).
# Fit the model to this new data, record the MLE for decay.rate.
# Repeat many times to get a distribution of decay.rate values.
# Usually you would then summarize with a 95% CI, but for our purposes you can just plot a
# histogram of the distribution.

