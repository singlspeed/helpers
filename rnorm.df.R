# function that creates data.frame with random normal distibuted data given the mean
# function provides adjustable between- and within-subject variance

rnorm.df <- function(factor.names, no.obs, mean, sd = 1, min = 1, max = 4) {
  no.factors <- length(factor.names)
  df <- t(replicate(no.obs, round(rnorm(no.factors*2, rnorm(1, mean, .5), sd), 0))) # here we generate random numbers, that are somehow equal for each participant and different between participants
  new_df <- matrix(ncol = no.factors, nrow = no.obs)
  for (i in 1:nrow(df)) {
    new_df[i,] <- df[i, (df[i,] <= max & df[i,] >= min)][1:no.factors]
  }
  df <- new_df
  df <- data.frame(df)
  colnames(df) <- factor.names
  return(df)
}

