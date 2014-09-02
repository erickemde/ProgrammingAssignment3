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
        outcomeForState<-subset(outcomeForState, outcomeForState[,outcomeCol] !="Not Available",select=c(2, outcomeCol))

        ## print(head(outcomeForState))
        ## check num
        ranking <- if(is.numeric(num)){
                num
        }else{
                switch(num,
                       "best" = 1,
                       "worst" = nrow(outcomeForState),
                       stop("invalid outcome")                                                             
                )
        }
        
        order.outcomes <- order(as.numeric(outcomeForState[,2]))
        outcomeForState <- outcomeForState[order.outcomes,]
        ## print(head(outcomeForState))

        rankedHospitalNum <- outcomeForState[ranking,]
        
        rankedHospital <- outcomeForState[which(as.numeric(outcomeForState[,2]) == as.numeric(rankedHospitalNum[,2])),]
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        if(nrow(rankedHospital)>1){
                order.state <- order(rankedHospital[,2])
                rankedHospital <- rankedHospital[order.state,]
                rankedHospital[1,1]
        }else{
                rankedHospital[1,1]
        }
}