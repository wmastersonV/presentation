f.initialize <- function(sdSystemAge = 7, sdBuilding = 7, 
                         sdTemp = 7, sdNonLinear = 1, sampleFlag = FALSE, 
                         downSampleTime = 10, fold = 1, type = "linearModel", transformFlag = TRUE ) {
  
  #check install packages
  pkgs = c("survival","pec","prodlim","randomForestSRC","ggRandomForests","rpart","partykit","rpart.plot","data.table")
  isPkgInstal = pkgs %in% rownames(installed.packages())
  for(k in 1 : length(isPkgInstal) ){
    if(!isPkgInstal[k]){
      install.packages(pkgs[k], dependencies = TRUE, repos="http://cran.rstudio.com/")
    }
  }
  library(survival)
  library(pec)
  library(MASS)
  library(prodlim)
  library(randomForestSRC)
  library(ggRandomForests)
  library(rpart)
  library(rpart.plot)
  library(partykit)
  library(data.table)
  # to do: install packages
  
  # features to add:
  # geographic location, client, price of electricity, 
  dat1 <- data.table(read.csv("SensorFiles/dat1.csv"))
  dat2 <- data.table(read.csv("SensorFiles/dat2.csv"))
  
  setkey(dat1, BuildingID)
  setkey(dat2, BuildingID)
  
  dat <- merge(dat1, dat2, by.x = BuildingID)
  levels(dat$product) <- c("A", "B", "C", "D", "E")
  table(dat$BuildingID)
  table(dat$product)
  
  dat$tempDiff <- dat$TargetTemp - dat$ActualTemp
  dat$System <- as.numeric(dat$System)
  
  # Z transform variables
  if(transformFlag){
    dat$SystemAge <- (dat$SystemAge - mean(dat$SystemAge) )/ sd(dat$SystemAge)
    dat$SystemAge <- (dat$SystemAge - mean(dat$SystemAge) )/ sd(dat$SystemAge)
    dat$tempDiff <- (dat$tempDiff - mean(dat$tempDiff) )/ sd(dat$tempDiff)
  }
  
  # define a "failure" as 1.5 standard deviations away from historical averages + system age Affect +
  # higher order interaction of building age and system age
  if(type == "linearModel"){
    # build a linear phenomena
    dat$isDeath <-  (dat$BuildingAge + dat$SystemAge + dat$tempDiff)  > 
      rnorm(length(dat$tempDiff), mean(((dat$SystemAge)), sd = sdSystemAge)) + 
      rnorm(length(dat$tempDiff), mean(((dat$BuildingAge))), sd = sdBuilding) + 
      rnorm(length(dat$tempDiff), mean((abs( dat$tempDiff)), sd = sdTemp)) 
  }
  else if (type == "nonLinearModel"){
    dat$isDeath <-  generateRNVec(product = as.character(dat$product), tempDiff = dat$tempDiff, SystemAge = dat$SystemAge, 
                                 BuildingAge = dat$BuildingAge, System = dat$System, 
                                 sdNonLinear = sdNonLinear, nonRandomFlag = TRUE) >
      generateRNVec(product = as.character(dat$product), tempDiff = dat$tempDiff, SystemAge = dat$SystemAge, 
                    BuildingAge = dat$BuildingAge, System = dat$System, 
                    sdNonLinear = sdNonLinear, nonRandomFlag = FALSE)
    dat$time2 <- generateRNVec(product = as.character(dat$product), tempDiff = dat$tempDiff, SystemAge = dat$SystemAge, 
                              BuildingAge = dat$BuildingAge, System = dat$System, 
                              sdNonLinear = sdNonLinear, nonRandomFlag = FALSE) 
    dat$time2 <- dat$time2 - min(dat$time2)
  }
  
  
  # convert dates to times
  dateTransform <- as.Date(as.character(dat$Date), "%m/%d/%y")
  dat$time <- as.POSIXct(paste0(as.character(dateTransform)," ", as.character(dat$Time)),  format = "%Y-%m-%d %H:%M:%S", tz = "EST")
  dat$timeDays <- as.numeric(difftime(Sys.time(), dat$time, units = "days"))
  dat$timeDaysTransformed <- dat$timeDays - min(dat$timeDays)
  dat$time <- dat$timeDaysTransformed
  # hist(dat$time )
  
  # remove unecessary columns, transform variables
  dat$Date <- NULL
  dat$Time <- NULL
  dat$timeDaysTransformed <- NULL
  dat$timeDays <- NULL
  dat$BuildingID <- as.factor(dat$BuildingID)
  # dat$System <- as.factor(dat$System)
  dat$ActualTemp <- NULL
  dat$TargetTemp <- NULL
  str(dat)
  
  #dummy transform dataset
  if(sampleFlag) {
    indx <- createFolds(dat$BuildingID, downSampleTime)
  }
  
  return(dat[indx[[fold]],])
}