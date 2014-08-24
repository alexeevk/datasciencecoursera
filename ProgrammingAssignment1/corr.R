corr <- function(directory, threshold = 0) {
    correlation <- NULL
    fileNames <- NULL
    for(fileName in list.files(directory, pattern="*.csv", full.names=TRUE)) {
        data <- read.csv(fileName)
        completecases <- data[complete.cases(data),]
        if(nrow(completecases) > threshold)
        {
            sulfate = completecases$sulfate
            nitrate = completecases$nitrate
            fileNames <- c(fileNames, fileName)
            correlation <- c(correlation, cor(sulfate, nitrate))
        }
    }
    correlation
}