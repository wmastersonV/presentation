generateRN <- function(product, tempDiff,SystemAge, BuildingAge,System,
                       sdNonLinear, nonRandomFlag = FALSE) {
  out = NA
  # input a single row
  if(nonRandomFlag){
    if (product == "A") {
      out = tempDiff
    } else if (product == "B") {
      out = SystemAge
    }else if (product == "C") {
      out = BuildingAge
    }else if (product == "D") {
      out = BuildingAge * SystemAge
    }else if (product == "E") {
      if (System < 5){
        out = 1
      } else if( System < 10){
        out = 2
      }  else if( System < 15){
        out = 3
      }  else if( System <= 20){
        out = 4
      }
    }
    
    print(out)
    
  } else {
    if (product == "A") {
      out = rnorm(1, mean(rnorm(1, mean = .1, sd = sdNonLinear/2)), sd = sdNonLinear) 
    } else if (product == "B") {
      out = rnorm(1, mean(rnorm(1, mean =.2, sd = sdNonLinear/2)), sd = sdNonLinear) 
    }else if (product == "C") {
      out = rnorm(1, mean(rnorm(1, mean = .3, sd = sdNonLinear/2)), sd =sdNonLinear) 
    }else if (product == "D") {
      out = rnorm(1, mean(rnorm(1, mean = .4, sd = sdNonLinear/2)), sd = sdNonLinear) 
    }else if (product == "E") {
      out = rnorm(1, mean(rnorm(1, mean = .5, sd = sdNonLinear/2)), sd = sdNonLinear) 
    }
  }
  return(out)
}
generateRNVec <- Vectorize(generateRN, c("product", "tempDiff","SystemAge", "BuildingAge","System",
                                         "sdNonLinear"))