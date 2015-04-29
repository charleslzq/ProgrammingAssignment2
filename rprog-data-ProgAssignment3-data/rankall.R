rankall <- function(outcome, num = "best"){
  ## Read outcome data
  
  ## Check that state and outcome are valid
  
  ## For each state, find the hospital of the given rank
  
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character");
  
  if(outcome != "heart attack"){
    if(outcome != "heart failure"){
      if(outcome != "pneumonia"){
        stop("invalid outcome");
      }
    }
  }
  
  states <- unique(data[,7]);
  result <- data.frame(matrix(NA,0,2));
  for(state in states){
    if(outcome == "heart attack"){
      k <- data[data[,7] == state, c(2,11,7)];
    }
    else if(outcome == "heart failure"){
      k <- data[data[,7] == state, c(2,17,7)];
    }
    else if(outcome == "pneumonia"){
      k <- data[data[,7] == state, c(2,23,7)];
    }
    
    k[,2] <- as.numeric(k[,2]);
    k <- k[!is.na(k[,2]),];
    k <- k[order(k[,1]),];
    k <- k[order(k[,2]),];
    if(num == "best"){
      n = 1;
    }
    else if( num == "worst"){
      n = nrow(k); 
    }
    else{
      n = num;
    }
    if(!is.na(k[n,c(1,3)])){
      result <- rbind(result, k[n,c(1,3)]);
    }
    else{
      result <- rbind(result, list(NA, k[1,3]));
    }
  }
  
  
  result <- result[order(result[,2]),];
  names(result) <- list("hospital","state");
  
  result;
}