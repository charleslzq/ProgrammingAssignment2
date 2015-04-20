corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  cplt <- complete(directory);
  cr <- numeric();
  for(i in 1:332){
    if(cplt[i,2] > threshold){
      if(i < 10){
        prefix <- "00";
      }
      else if(i < 100){
        prefix <- "0";
      }
      else{
        prefix <- "";
      }
      name <- paste(directory, "/", prefix, i, ".csv", sep="");
      table<-read.csv(name);
      ct<- complete.cases(table)
      cr <- c(cr, cor(table[ct,2], table[ct,3]));
    }
  }
  cr;
}