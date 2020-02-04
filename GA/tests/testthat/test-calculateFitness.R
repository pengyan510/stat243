context("calculateFitness Tests")

test_that('calculateFitness handles inputs correctly',{

  ##handles realistic values well
  ##setup for realistic values taken from 'select' function

  regressType=gaussian
  fitnessFunc=AIC
  P=NULL

  fit = glm(Sepal.Length~Sepal.Width+Petal.Length+Petal.Width,data=iris,
            x = TRUE, y = TRUE)

  X <- fit$x[, -1]
  colnames(X) <- NULL
  # The response variable
  Y <- matrix(fit$y, ncol = 1)

  # length of the chromosome
  C <- ncol(X)

  if (is.null(P)) {
    P <- C * 2
  }

  # initialize the P chromosomes
  generations <- initChromosomes(P, C)



  expect_type(calculateFitness(X,Y,regressType,fitnessFunc,generations),'double')



  ###throws an error for unrealistic values

  expect_error(calculateFitness(X,Y,regressType=not.a.thing,fitnessFunc,generations))
  expect_error(calculateFitness(X,Y,regressType,fitnessFunc=not.a.thing,generations))
  expect_error(calculateFitness(X,Y,regressType,fitnessFunc,generations=NA))

})
