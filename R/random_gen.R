random_gen <- function(x, y, mean = 10, sd = 2, df = 2, df1 = 1,
                       df2 = 10, size = 100, prob = 0.5){
  if(x == "normal"){
    rnorm(y, mean, sd)
  } else if(x == "t"){
    rt(y, df)
  } else if(x == "F"){
    rf(y, df1, df2)
  } else if(x == "binomial"){
    rbinom(y, size, prob)
  }
}

random_gen("normal", 3)
random_gen("t", 3)
random_gen("t", 3)
random_gen("binomial", 3)

random_gen("normal", 3, mean = 500, sd = 0.2)
random_gen("t", 3, df = 100)
random_gen("t", 3, df1 = 100, df2 = 10000)
random_gen("binomial", 5, size = 501, prob = 0.3)

