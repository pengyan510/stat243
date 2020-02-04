context("select Tests")

test_that('select works with standard input',{

  ###Test standard data with 'gaussian' family

  expect_type(select(Employed~GNP.deflator+GNP+Unemployed+Armed.Forces+Population,data=longley),'list')

  ###Test standard data with 'binomial' family

  iris$Virginica = ifelse(iris$Species=='Virginica',1,0)

  expect_type(select(Virginica~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width,data=iris,regressType=binomial),'list')

  ###Test interaction data with 'gaussian' family

  expect_type(select(Employed~GNP.deflator*GNP*Unemployed*Armed.Forces*Population,data=longley),'list')

  ###Test interaction data with 'binomial' family

  expect_type(select(Virginica~Sepal.Length*Sepal.Width*Petal.Length*Petal.Width,data=iris,regressType=binomial),'list')
})





test_that('select works with missing values',{

  ###Test missing data in covariates

  longley$Armed.Forces.NA = ifelse(longley$Armed.Forces<200,NA,longley$Armed.Forces)

  expect_type(select(Employed~GNP.deflator+GNP+Unemployed+Armed.Forces.NA+Population,data=longley),'list')

  ###Test missing data in covariates w/ interactions

  expect_type(select(Employed~GNP.deflator*GNP*Unemployed*Armed.Forces.NA*Population,data=longley),'list')

  ###Test missing data in dependent

  iris$Virginica.NA = ifelse(iris$Species=='Virginica',1,
                             ifelse(iris$Species=='setosa',NA,0))

  expect_type(select(Virginica.NA~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width,data=iris,regressType=binomial),'list')
})






test_that('select fails with unrealistic parameters specified',{

  ###Non-existant dataset specified

  expect_error(select(y~x,data=not.a.thing))

  ###regressType incorrect

  expect_error(select(Employed~GNP.deflator*GNP*Unemployed*Armed.Forces*Population,data=longley,regressType=not.a.thing))

  ###fitnessFunc incorrect

  expect_error(select(Employed~GNP.deflator*GNP*Unemployed*Armed.Forces*Population,data=longley,fitnessFunc=not.a.thing))

  ###geneticOperator incorrect

  expect_error(select(Employed~GNP.deflator*GNP*Unemployed*Armed.Forces*Population,data=longley,geneticOperator=not.a.thing))

  ###maxIter negative

  expect_error(select(Employed~GNP.deflator*GNP*Unemployed*Armed.Forces*Population,data=longley,maxIter = -1))
})
