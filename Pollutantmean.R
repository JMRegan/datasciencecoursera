pollutantmean <- function(directory, pollutant, id  = 1:332) {
  names <- list.files(directory) [id]
  ##list the files in the named directory
  ##[] tell the function what subset of files to look at, aka index on them 
  read <- lapply(paste(directory,"/", names, sep = ""), read.csv)
  ##applies the paste function (concatenate into a single element)
  ## to the directory files, which are the "names" function.
  ## read.csv: read csv files in
  return(mean(unlist(lapply(read, function(x) {x[,pollutant]})), na.rm=T))
  ## Mean: taking the average
  ## unlist: converts a list to a single vector
  ## lapply is taking the list from "read" and applying 
  ## function x which combines tables based on pollutant specified
}
