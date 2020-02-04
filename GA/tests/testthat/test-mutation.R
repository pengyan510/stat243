context("mutations Tests")

test_that('mutation handles inputs correctly',{

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

  offsprings = crossover(parents)

  expect_type(mutation(parents,offsprings,prob=.1),'double')

  ###throws an error for unrealistic values

  expect_error(mutation(parents=not.a.thing,offsprings,prob=.1))
  expect_error(mutation(parents,offsprings=not.a.thing,prob=.1))
  expect_error(mutation(parents,offsprings,prob=not.a.thing))

})



test_that('mutation handles weird probability inputs well', {

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

  offsprings = crossover(parents)


  ##Throws an error for probabilities that don't make sense

  expect_error(mutation(parents,offsprings,prob=-.1))
  expect_error(mutation(parents,offsprings,prob=1.1))
  expect_error(mutation(parents,offsprings,prob=NA))

})
