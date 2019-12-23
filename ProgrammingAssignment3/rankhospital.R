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
  if (!state %in% unique(allStates)) {stop('invalid state')}
  
  ##check the validity of outcome
  if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {stop('invalid outcome')}
  
  ##check the validity of num
  ##check <- all.equal(num, as.character(num))
  if (!num %in% nummatch && !num%%1==0) {stop("Seems like a faulty argument!!")}
  
  ##fetch state specific data
  temp <- datatable[datatable$State==state & datatable[as.numeric(keymatch[[outcome]])] != "Not Available",]
  
  ## Return hospital name in that state with the given rank
  ##truetemp <- temp[complete.cases(temp[,11]),]
  if (num == "best") {
    bestRow <- temp$Hospital.Name[temp[,as.numeric(keymatch[[outcome]])] == min(temp[,as.numeric(keymatch[[outcome]])])]
  } else if (num == "worst") {
    bestRow <- temp$Hospital.Name[temp[,as.numeric(keymatch[[outcome]])] == max(temp[,as.numeric(keymatch[[outcome]])])]
  } else {
    temporder <- temp[order(temp[,as.numeric(keymatch[[outcome]])], decreasing = FALSE),]
    bestRow <- temporder[as.integer(num),2]
  }
  ## 30-day death rate
  if (length(bestRow) > 1) {
    print(sort(bestRow)[1])
  }
  else {
    print(bestRow)
  }
}
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  