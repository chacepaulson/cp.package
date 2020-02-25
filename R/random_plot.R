library(ggplot2)
library(stringr)
#' Plot Generator for Random Distributions
#'
#' @param x Name of distribution type
#' @param n Number of observations
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
#' @param bins binwidth of graph (default to 30)
#'
#' @importFrom stats rbinom
#' @importFrom stats rf
#' @importFrom stats rnorm
#' @importFrom stats rt
#' @importFrom ggplot2 qplot
#' @importFrom ggplot2 theme_bw
#' @importFrom ggplot2 labs
#' @importFrom stringr str_c
#'
#' @return a plot from randomly generated numbers
#' @export
#'
#' @examples random_plot("normal", n = 1000, mean = 10, sd = 1, bins = 20)
random_plot <- function(x, n = 1, seed = FALSE, seed_n = 1, mean = 10,
                       sd = 2, df = 2,
                       df1 = 1, df2 = 10, size = 100, prob = 0.5,
                       bins = 30){
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
  } else if(is.numeric(bins) == FALSE){
    print("wanring: value must be numeric")
  } else{
    x.name <- str_c(n, "Samples in", bins, "Bins", sep = " ")
    if(seed == TRUE){
      set.seed(seed_n)
      if(x == "normal"){
        title.name <- str_c(x, " (", mean, ", ", sd, ")")
        qplot(rnorm(n = n, mean = mean, sd = sd), bins = bins) +
          theme_bw() +
          labs(x = x.name, title = title.name, y = "count")
      } else if(x == "t"){
        title.name <- str_c(x, " (", df, ")")
        qplot(rt(n = n, df = df), bins = bins) +
          theme_bw() +
          labs(x = x.name, title = title.name, y = "count")
      } else if(x == "F"){
        title.name <- str_c(x, " (", df1, ", ", df2, ")")
        qplot(rf(n = n, df1 = df1, df2 = df2), bins = bins) +
          theme_bw() +
          labs(x = x.name, title = title.name, y = "count")
      } else if(x == "binomial"){
        title.name <- str_c(x, " (", size, ", ", prob, ")")
        qplot(rbinom(n = n, size = size, prob = prob), bins = bins) +
          theme_bw() +
          labs(x = x.name, title = title.name, y = "count")
      }
    } else{
      x.name <- str_c(n, "Samples in", bins, "Bins", sep = " ")
      if(x == "normal"){
        title.name <- str_c(x, " (", mean, ", ", sd, ")")
        qplot(rnorm(n = n, mean = mean, sd = sd), bins = bins) +
          theme_bw() +
          labs(x = x.name, title = title.name, y = "count")
      } else if(x == "t"){
        title.name <- str_c(x, " (", df, ")")
        qplot(rt(n = n, df = df), bins = bins) +
          theme_bw() +
          labs(x = x.name, title = title.name, y = "count")
      } else if(x == "F"){
        title.name <- str_c(x, " (", df1, ", ", df2, ")")
        qplot(rf(n = n, df1 = df1, df2 = df2), bins = bins) +
          theme_bw() +
          labs(x = x.name, title = title.name, y = "count")
      } else if(x == "binomial"){
        title.name <- str_c(x, " (", size, ", ", prob, ")")
        qplot(rbinom(n = n, size = size, prob = prob), bins = bins) +
          theme_bw() +
          labs(x = x.name, title = title.name, y = "count")
      }
    }
  }
}




