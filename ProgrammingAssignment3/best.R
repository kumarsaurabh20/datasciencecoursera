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
        
        ##fetch state specific data
        temp <- datatable[which(datatable$State==state),]
        ##remove the missing values
        truetemp <- temp[complete.cases(temp),]
        bestRow <- truetemp$Hospital.Name[truetemp[,as.numeric(keymatch[[outcome]])] == min(truetemp[,as.numeric(keymatch[[outcome]])])]
        
        if (length(bestRow) > 1) {
          print(sort(bestRow)[1])
        }
        else {
          print(bestRow)
        }
}
##newdata <- mydata[ which(mydata$gender=='F' & mydata$age > 65), ]
