transformDummy <- function(datInput, featureInit){
  # this transforms a datatable's features to dummy variables. Features are selected by featureInit
  
  # input:
  # featureInit: names of features to transform to dummy vars
  
  # output:
  # new datatable with original column names 'featureInit' removed, with their transformed versions in place
  
  datInputDummy <- datInput[, names(datInput) %in% c(featureInit), with = FALSE]
  
  if(length(featureInit) == 0) {
    returnDatFlag = TRUE
    } else {
      returnDatFlag = Reduce(function(u, v) u | v, featureInit %in% c("isTerm","tenure"))
    }
  
  if(dim(datInputDummy)[1] == 0 | returnDatFlag){
      return(datInput)
  } else {
      indxNumeric <- unlist(lapply(datInputDummy, is.numeric))
      datInputDummyNumeric <- datInputDummy[,indxNumeric, with = FALSE]
      
      require(dummy)
      datInputDummyTransform <- dummy(datInputDummy)
      if( dim(datInputDummyNumeric)[1] > 0) {
        datInputDummyOut <- cbind(datInputDummyTransform, datInputDummyNumeric)
      } else {
        datInputDummyOut <- datInputDummyTransform
      }
      datInputDummyOut <- data.table(datInputDummyOut)
      
      # remove dummy columns
      set(datInput, j = which(colnames(datInput) %in% featureInit), value = NULL)
      
      datInputDummyOut = cbind(datInput, datInputDummyOut)
      
      return(datInputDummyOut)
  }
}