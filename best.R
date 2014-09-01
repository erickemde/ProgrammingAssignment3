

best <- function(state, outcome) {
        
        ## Read outcome data
        outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        ## Check that state and outcome are valid
        outcomeForState <- subset(outcomeData, State == state, )
        if(nrow(outcomeForState)==0){stop("invalid state")}
        outcomeCol <- switch(outcome,
                             "heart attack" = 11, ## "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",            
                             "heart failure" = 17, ##"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
                             "pneumonia" = 23, ## "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia",
                             stop("invalid outcome")
                             )
        outcomeForState<-subset(outcomeForState, outcomeForState[,outcomeCol] !="Not Available",)

        minForState <- min(as.numeric(outcomeForState[,outcomeCol]))
        
        topHospital <- outcomeForState[which(as.numeric(outcomeForState[,outcomeCol]) == minForState),]

        ## Return hospital name in that state with lowest 30-day death
        ## rate
        if(nrow(topHospital)>1){
                order.state <- order(topHospital[,2])
                topHospital <- topHospital[order.state,]
                topHospital[,2]
        }else{
                topHospital[,2]
        }
}