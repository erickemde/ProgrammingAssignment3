rankhospital <- function(state, outcome, num = "best") {
        ## Read outcome data
        outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        ## Check that state and outcome are valid
        
        ## Check state
        outcomeForState <- subset(outcomeData, State == state, )
        if(nrow(outcomeForState)==0){stop("invalid state")}
        
        ## Check outcome
        outcomeCol <- switch(outcome,
                             "heart attack" = 11, ## "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",            
                             "heart failure" = 17, ##"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
                             "pneumonia" = 23, ## "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia",
                             stop("invalid outcome")
        )
        
        ## Remove rows that have "Not Available" in the outcome column
        outcomeForState<-subset(outcomeForState, outcomeForState[,outcomeCol] !="Not Available",)
        
        ## check num
        ranking <- if(is.numeric(num)){
                num
        }else{
                switch(num,
                       "best" = min(as.numeric(outcomeForState[,outcomeCol])),
                       "worst" = max(as.numeric(outcomeForState[,outcomeCol])),
                       stop("invalid outcome")                                                             
                )
        }

        rankedHospital <- outcomeForState[which(as.numeric(outcomeForState[,outcomeCol]) == ranking),]
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        if(nrow(rankedHospital)>1){
                order.state <- order(rankedHospital[,2])
                rankedHospital <- rankedHospital[order.state,]
                rankedHospital[,2]
        }else{
                rankedHospital[,2]
        }
}