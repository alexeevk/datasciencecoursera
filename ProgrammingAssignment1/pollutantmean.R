pollutantmean <- function(directory, polutant, id = 1:332){
    if(polutant != "sulfate" && polutant != "nitrate")
        return(paste("Invalid polutant", polutant))
    
    mm <- NULL
    data <- NULL
    fileNames <- file.path(directory, paste(sprintf("%03d", id), ".csv", sep=""))
    for(n in fileNames){
        d <- read.csv(n)[polutant]
        data <- c(data, d[!is.na(d)]);
    } 
    
    mean(data)
}