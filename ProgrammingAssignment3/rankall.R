rankall <- function(outcome, num = "best") {
  ## Read outcome data
  ##read the file
  
  datatable <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  out <- data.frame()
  
  
  ranktemp <- function(df, outcome, num="best") {
    keymatch <- list("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
    nummatch <- list(a="best", b="worst")
    bestRow <- NULL
    temp <- df[df$State==state & df[as.numeric(keymatch[[outcome]])] != "Not Available",]
    state <- temp$State[1]
    out <- data.frame()
    ##check the validity of outcome
    if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {stop('invalid outcome')}
    
    ##check the validity of num
    ##check <- all.equal(num, as.character(num))
    if (!num %in% nummatch && !num%%1==0) {stop("Seems like a faulty argument!!")}
    
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
      out <- data.frame(Hospital=sort(bestRow)[1], State=state)
      return(out)
    }
    else {
      out <- data.frame(Hospital=bestRow, State=state)
      return(out)
    }
  }
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
}








