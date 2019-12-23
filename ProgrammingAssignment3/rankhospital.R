  rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  ##read the file
  keymatch <- list("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
  nummatch <- list(a="best", b="worst")
  datatable <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  bestRow <- NULL
  
  ## Check that state and outcome are valid
  ##check the validity of state
  allStates <- datatable$State
  if (!state %in% unique(allStates)) {
    stop('invalid state')
  }
  
  ##check the validity of outcome
  if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
    stop('invalid outcome')
  }
  
  ##check the validity of num
  rankchar <- NULL
  ranknum <- NULL
  ##check <- all.equal(num, as.character(num))
  if (num %in% nummatch) {
    rankchar <- num
  }
  else {
    ranknum <- as.integer(num)
  }
  
  ##fetch state specific data
  temp <- datatable[which(datatable$State==state),]
  
  ## Return hospital name in that state with the given rank
  truetemp <- temp[complete.cases(temp),]
  if (rankchar == "best") {
    bestRow <- truetemp$Hospital.Name[truetemp[,as.numeric(keymatch[[outcome]])] == min(truetemp[,as.numeric(keymatch[[outcome]])])]
  } else if (rankchar == "worst") {
    bestRow <- truetemp$Hospital.Name[truetemp[,as.numeric(keymatch[[outcome]])] == max(truetemp[,as.numeric(keymatch[[outcome]])])]
  } else {
    truetemporder <- truetemp[order(truetemp[,as.numeric(keymatch[[outcome]])]),]
    bestRow <- truetemporder[ranknum,2]
  }
  ## 30-day death rate
  if (length(bestRow) > 1) {
    print(sort(bestRow)[1])
  }
  else {
    print(bestRow)
  }
}
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  