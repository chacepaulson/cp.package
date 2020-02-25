#' Distribution Based Random Number Generator
#'
#' @param x Name of distribution type ("normal", "t", "F", "binomial")
#' @param n Number of observations
#'
#' @param seed Logical operator, TRUE if setting seed (default to FALSE)
#' @param seed_n number for set seed (default to 1)
#'
#' @param mean For normal distribution, mean value (default to 10)
#' @param sd For normal distribution, standard deviation value (default to 2)
#' @param df For t distribution, degrees of freedom (default to 2)
#' @param df1 For F distribution, numerator degrees of freedom (default to 1)
#' @param df2 For F distribution, denominator degrees of freedom (default to 10)
#' @param size For binomial distribution, number of trials (default to 100)
#' @param prob For binomial distribution, probability of success of each trial (default to 0.5)
#'
#' @importFrom stats rbinom
#' @importFrom stats rf
#' @importFrom stats rnorm
#' @importFrom stats rt
#'
#' @return a vector of random numbers
#' @export
#'
#' @examples random_gen(x = "binomial", n = 5, size = 501, prob = 0.3, seed = TRUE, seed_n = 1)
random_gen <- function(x, n = 1, seed = FALSE, seed_n = 1, mean = 10,
                       sd = 2, df = 2,
                       df1 = 1, df2 = 10, size = 100, prob = 0.5){
  if(x != "binomial" & x != "normal" & x != "t" & x != "F"){
    print("warning: incorrect distribution name given")
  } else if(is.numeric(n) == FALSE | is.numeric(seed_n) == FALSE |
            is.numeric(mean) == FALSE | is.numeric(sd) == FALSE |
            is.numeric(df) == FALSE | is.numeric(df1) == FALSE |
            is.numeric(df2) == FALSE | is.numeric(size) == FALSE |
            is.numeric(prob) == FALSE){
      print("warning: value must be numeric")
    } else if(prob > 1 | prob < 0){
        print("warning: prob value must be between 0 and 1")
    } else if(is.logical(seed) == FALSE){
        print("warning: seed must be logical operator")
    } else{
          if(seed == TRUE){
            set.seed(seed_n)
            if(x == "normal"){
              rnorm(n = n, mean = mean, sd = sd)
            } else if(x == "t"){
              rt(n = n, df = df)
            } else if(x == "F"){
              rf(n = n, df1 = df1, df2 = df2)
            } else if(x == "binomial"){
              rbinom(n = n, size = size, prob = prob)
            }
          } else{
            if(x == "normal"){
              rnorm(n = n, mean = mean, sd = sd)
            } else if(x == "t"){
              rt(n = n, df = df)
            } else if(x == "F"){
              rf(n = n, df1 = df1, df2 = df2)
            } else if(x == "binomial"){
              rbinom(n = n, size = size, prob = prob)
            }
          }
        }
      }


