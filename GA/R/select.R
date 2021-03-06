# select is used to carry out the variable selection
# Arguments:
# formula - a symbolic description of the model to
#           be fitted.
# data - a data frame, list or environment containing
#        the variables in the model
# P - number of organisms at each generation
#     (further may enable it to decay over iteration)
# regressType - type of regression, default is gaussian,
#               which just corresponds to running lm()
# fitnessFunc - what finess function to use
#               default is AIC, user can provide
#               any other function
# rankOption - whether to use fitness ranks, default
#            - is TRUE
# geneticOperator - what genetic operator to use,
#                   default is the one described in
#                   Givens and Hoeting, user can
#                   provide any other operator
# mutationProb - the probability of mutation occurs
# maxIter - max number of iterations, default is 100
select <- function(formula, data, P = NULL,
                   regressType = gaussian,
                   fitnessFunc = AIC,
                   rankOption = TRUE,
                   geneticOperator = NULL,
                   mutationProb = 0.1,
                   maxIter = 100) {
  # Check whether maxIter is valid
  if ((maxIter < 0) | ! all.equal(maxIter, as.integer(maxIter))) {
    stop("Invalid maximum iterations given!")
  }
  # By default, regressType is gaussian. Thus glm()
  # runs the same as lm()
  originLm <- glm(formula, data = data, family = regressType,
                  x = TRUE, y = TRUE)
  # The matrix of variables for selection
  X <- originLm$x[, -1]
  colnames(X) <- NULL
  # The response variable
  Y <- matrix(originLm$y, ncol = 1)
  
  # length of the chromosome
  C <- ncol(X)
  # Set the number of organisms per generation to
  # 2C if not given by the user
  if (is.null(P)) {
    P <- C * 2
  }
  
  # initialize the P chromosomes
  generations <- initChromosomes(P, C)
  # Count the number of iterations
  numIter <- 0
  # Record how the fitness changes over iterations
  fits <- rep(NA, maxIter)
  while (numIter < maxIter) {
    numIter <- numIter + 1
    P <- dim(generations)[1]
    # Calcualte the fitness of each chromosome
    # in the generations matrix
    # Store the results in prob
    prob <- calculateFitness(X, Y, regressType,
                               fitnessFunc,
                               generations)
    # Record the fitess of the current iteration
    fits[numIter] <- mean(prob)
    # For the AIC, the lower it is, the better the fitness
    # Thus we take the addditive inverse of it
    if (identical(fitnessFunc, AIC)) {
      prob <- - prob
    }
    # Check whether to use rank fitness
    if (rankOption) {
      # Calculate the \phi() function specified
      # in Givens and Hoeting
      # Store the result in prob for convenience
      phi <- 2 * rank(prob) / (P * (P + 1))
      prob <- phi
    }
    
    # Sample P + 4 parents based on the prob vector
    # Use P + 4 because some offsprings would be duplicated
    # So the number of offsprings should be slightly
    # larger than P to keep the size of the new generation
    # relatively stable
    parents <- generations[sample(1:P, P + 4,
                                  prob = prob,
                                  replace = TRUE),]
    if (is.null(geneticOperator)) {
      # Use the genetic operators described in
      # Givens and Hoeting on the parents matrix
      # I wrap the crossover() and mutation() in a function
      # called defaultOperator()
      generations <- defaultOperator(parents, mutationProb)
      # Remove duplicate offsprings
      generations <- unique(generations)
      # Keep the number of offsprings at an even number
      if (dim(generations)[1] %% 2 == 1) {
        # If only one offspring remains, simply return it
        if (dim(generations)[1] == 1) {
          break
        }
        generations <- generations[-1,]
      }
    } else {
      # Use genetic operators provided by the user
      generations <- geneticOperator(parents)
    }
  }
  
  # The final results is determined by the mode in
  # each column
  result <- (colMeans(generations) > 0.5) * 1
  return(list(selection = result, fitness = fits))
}

# initChromosomes is used to initialize the chromosomes
# Arguments:
# P - number of chromosomes
# C - length of each chromosome
initChromosomes <- function(P, C) {
  # Generate P numbers from 0 to 2^C - 1 randomly
  nums <- sample(0:(2^C - 1), P, replace = FALSE)
  
  # converToBinary is used to conver a number to
  # its binary form
  # Argument:
  # x - the number to be converted
  convertToBinary <- function(x) {
    binary <- rev(intToBits(x))
    return(as.numeric(binary[(33 - C):32]))
  }
  
  # Convert all the number in nums to binary form
  # and store them as a P * C matrix
  chromosomes <- t(sapply(nums, convertToBinary))
  return(chromosomes)
}

# calculateFitness is used to calculate the fitness
# of each chromosome in the generation
# Arguments:
# X - the matrix of the independent variables
# Y - the vector of the response variable
# regressType - the type of regression
# fitnessFunc - the fitness function to use
# generations - the P * C matrix of chromosomes
calculateFitness <- function(X, Y, regressType,
                             fitnessFunc, generations) {
  P <- dim(generations)[1]
  # fits store the fitness of the P chromosomes
  fits <- rep(0, P)
  for (i in 1:P) {
    # Get the subset of X being selected according
    # to the i-th chromosome
    subsetX <- X[, generations[i,] == 1]
    # Create a temporary data.frame of Y and subsetX
    tempDF <- as.data.frame(cbind(Y, subsetX))
    # Calculate the fitness using the provided
    # fitness function
    fits[i] <- fitnessFunc(glm(V1 ~ ., family = regressType,
                                       data = tempDF))
  }
  return(fits)
}

# defaultOperator a wrapper over crossover() and mutation()
# Arguments:
# parents - matrix that stores the parents
# prob - the mutation probability
defaultOperator <- function(parents, prob) {
  # First do crossover()
  offsprings <- crossover(parents)
  # Then do mutation()
  return(mutation(parents, offsprings, prob))
}

# mutation is used to mutate the offsprings
# Arguments:
# parents - matrix that stores the parents
# parents - matrix that stores the offsprings
# prob - the mutation probability
mutation <- function(parents, offsprings, prob) {
  # Check whether mutation probability is valid
  if ((prob < 0) | (prob > 1)) {
    stop("Invalid mutation probability given!")
  }
  P <- dim(parents)[1]
  i <- 1
  while (i < P) {
    # Get the parents of the current offspring
    parent_1 <- parents[i,]
    if (i %% 2 == 1) {
      parent_2 <- parents[i + 1,]
    } else{
      parent_2 <- parents[i - 1,]
    }
    
    # Get the positions where parent_1 is the same as parent_2
    samePos <- which(parent_1 == parent_2)
    # For each same position, check whether we need to
    # flip it over
    for (k in seq_len(length(samePos))) {
      if (runif(1) < prob) {
        offsprings[i, samePos[k]] <- 1 -
          offsprings[i, samePos[k]]
      }
    }
    i <- i + 1
  }
  
  return(offsprings)
}

# mutation is used to crossover the parents to get offsprings
# Arguments:
# parents - matrix that stores the parents
crossover <- function(parents){
  P <- dim(parents)[1]
  # Check whether the number of parents is even
  if (P %% 2 == 1) {
    stop("Invalid number of parents!")
  }
  C <- dim(parents)[2]
  offsprings <- matrix(rep(NA, P * C), c(P, C))
  i <- 1
  while (i < P) {
    # Get the two parents
    parent_1 <- parents[i,]
    parent_2 <- parents[i + 1,]
    # Randomly pick the spliting point
    pos <- sample(1:(C - 1), 1)
    # Formulate the two offsprings
    offsprings[i, 1:pos] <- parents[i, 1:pos]
    offsprings[i, (pos + 1):C] <- parents[i + 1, (pos + 1):C]
    offsprings[i + 1, 1:pos] <- parents[i + 1, 1:pos]
    offsprings[i + 1, (pos + 1):C] <- parents[i, (pos + 1):C]
    i <- i + 2
  }
  
  return(offsprings)
}