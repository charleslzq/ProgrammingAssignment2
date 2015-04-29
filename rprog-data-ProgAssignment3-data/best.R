best <- function(state, outcome){
  ## Read outcome data
  
  ## Check taht state and outcome are valid
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character");
  
  if(outcome != "heart attack"){
    if(outcome != "heart failure"){
      if(outcome != "pneumonia"){
        stop("invalid outcome");
      }
    }
  }
  
  if(is.na(match(state, data[,7]))){
    stop("invalid state");
  }
  
  m <- data[data[,7] == state, 2];
  if(outcome == "heart attack"){
    k <- as.numeric(data[data[,7] == state, 11]);
  }
  else if(outcome == "heart failure"){
    k <- as.numeric(data[data[,7] == state, 17]);
  }
  else if(outcome == "pneumonia"){
    k <- as.numeric(data[data[,7] == state, 23]);
  }
  
  min <- min(k, na.rm = TRUE);
  n <- m[k == min];
  sort(n);
}