context("crossover Tests")

test_that('crossover handles inputs correctly',{

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


  P = dim(generations)[1]

  prob = calculateFitness(X,Y,regressType,fitnessFunc,generations)


  parents <- generations[sample(1:P, P + 2,
                                prob = prob,
                                replace = TRUE),]



  expect_type(crossover(parents),'double')

  ###throws an error for uneven number of rows

  expect_error(crossover(rbind(c(1,2,3),c(1,2,3),c(1,2,3))))

  ###throws an error for unrealistic values

  expect_error(crossover(not.a.thing))

})
