context("initChromosomes Tests")

test_that('initChromosomes handles inputs correctly',{

  ###handles realistic values well

  expect_type(initChromosomes(5,10),'double')
  expect_type(initChromosomes(32,5),'double')
  expect_equal(length(initChromosomes(5,10)),5*10)

  ###throws an error for unrealistic values

  expect_error(initChromosomes(33,5))
  expect_error(initChromosomes('a','b'))
  expect_error(initChromosomes(10,NA))

})
