rankhospital <- function(state, outcome, num = "best"){
  ## Read outcome data
  
  ## Check that state and outcome are valid
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  
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
  
  if(num == "best"){
    n = 1;
  }
  else{
    n = num;
  }
  
  if(outcome == "heart attack"){
    k <- data[data[,7] == state, c(2,11)];
  }
  else if(outcome == "heart failure"){
    k <- data[data[,7] == state, c(2,17)];
  }
  else if(outcome == "pneumonia"){
    k <- data[data[,7] == state, c(2,23)];
  }
  
  k[,2] <- as.numeric(k[,2]);
  k <- k[!is.na(k[,2]),];
  k <- k[order(k[,1]),];
  k <- k[order(k[,2]),];
  if(num == "worst"){
    n = nrow(k);  
  }
  k[n,1];
}