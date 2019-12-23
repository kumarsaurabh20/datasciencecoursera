best <- function(state, outcome) {
        ##read the file
        keymatch <- list("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
        datatable <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        bestRow <- NULL
        
        ##check the validity of state
        allStates <- datatable$State
        if (!state %in% unique(allStates)) {
          stop('invalid state')
        }
        
        ##check the validity of outcome
        if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
          stop('invalid outcome')
        }
        
        ##fetch state specific data & remove the missing values
        temp <- datatable[datatable$State==state & datatable[as.numeric(keymatch[[outcome]])] != "Not Available",]
        bestRow <- temp$Hospital.Name[temp[,as.numeric(keymatch[[outcome]])] == min(temp[,as.numeric(keymatch[[outcome]])])]
        
        if (length(bestRow) > 1) {
          print(sort(bestRow)[1])
        }
        else {
          print(bestRow)
        }
}
##newdata <- mydata[ which(mydata$gender=='F' & mydata$age > 65), ]
