concatenate_speed <- function(x){
  x[x == 0] <- NA
  x <- rank(x,na =  "keep")
  x <- sign(diff(x))
  x <- gsub("-1","D",paste(x))
  x <- gsub("1","A",paste(x))
  x <- gsub("0","C",paste(x))
  x <- gsub("NA","S",paste(x))
  x <- paste(x,collapse = "")
  x <- gsub("(A|C|D|S)\\1{2,}","\\1",x)
  x <-  gsub("^S|S$","",x)
  return(x)
}


lomb_scargle <- function(x){
  butter_filter =  butter(3,0.8,type = "low")
  lsp_x <- lsp(x = as.matrix(data.frame(x["second"],filtfilt(butter_filter,runmed(unlist(x["acceleration_x"]),17)))),plot = F)
  lsp_y <- lsp(x = as.matrix(data.frame(x["second"],filtfilt(butter_filter,runmed(unlist(x["acceleration_y"]),17)))),plot = F)
  lsp_z <- lsp(x = as.matrix(data.frame(x["second"],filtfilt(butter_filter,runmed(unlist(x["acceleration_z"]),17)))),plot = F)
  
  x[,"a_x_peak"] = lsp_x$peak
  x[,"a_y_peak"] = lsp_y$peak
  x[,"a_z_peak"] = lsp_y$peak
  
  x_freq <- sort(lsp_x$scanned)[1:10]
  y_freq <- sort(lsp_y$scanned)[1:10]
  z_freq <- sort(lsp_z$scanned)[1:10]  
  
  i <- 1
  while(i<=10) {
    x[,paste0("a_x_freq_",i)] = x_freq[i]
    x[,paste0("a_y_freq_",i)] = y_freq[i]
    x[,paste0("a_z_freq_",i)] = z_freq[i]
    i = i+1
  } 
  
  
  return(x)
  
}

