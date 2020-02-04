\name{select}
\alias{select}
\title{Variable Selection for Regression}
\usage{
select(formula, data, P = NULL, regressType = gaussian,
fitnessFunc = AIC, rankOption = TRUE, geneticOperator = NULL,
mutationProb = 0.1, maxIter = 100)
}
\description{
\code{select} is used to perform variable selection for regression problems, including both linear regression and GLMs. It is implemented using a genetic algorithm, which is based on Section 3.4 of the Givens and Hoeting book on Computational Statistics. However, users are also allowed to provide their own objective criterion/fitness function and additional genetic operators.
}
\arguments{
  \item{formula}{a symbolic description of the model to be fitted.}
  \item{data}{a data frame, list or environment containing the variables in the model.}
  \item{P}{number of organisms at each generation.}
  \item{regressType}{type of regression, default is gaussian, which just corresponds to running lm().}
  \item{fitnessFunc}{what finess function to use, default is AIC. User can provide any other function.}
  \item{rankOption}{whether to use fitness ranks, default is \code{TRUE}.}
  \item{geneticOperator}{what genetic operator to use, default is the one described in Givens and Hoeting. User can provide any other operator.}
  \item{mutationProb}{the probability of mutation occurs.}
  \item{maxIter}{max number of iterations, default is 100.}
}
\value{
\item{select}{returns a list of the selection result and a vector of the fitness over iteration.}
\item{selection}{a 0-1 vector indicating the final selection result. Its length is the same as the number of terms specified in the formula (not counting the dependent variable).}
\item{fitness}{a numeric vector recording the fitness over iteration. Its length is the same as the number of iterations.}
}
\references{
Givens, G.H. and Hoeting, J.A. (2012). Computational Statistics. John Wiley & Sons.
}
\examples{
\code{library(mlbench)

data(BostonHousing)

select(medv~.-medv,data=BostonHousing)

data(HouseVotes84)

select(Class~.-Class,data=HouseVotes84,regressType=binomial)}}