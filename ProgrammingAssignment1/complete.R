complete <- function(directory, id = 1:332) {
    cc <- NULL
    ids <- NULL
    for(i in id) {
        data <- read.csv(file.path(directory, paste(sprintf("%03d", i), ".csv", sep="")))
        completecases <- data[complete.cases(data),]
        cc <- c(cc, nrow(completecases))
        ids <- c(ids, i)
    }
    data.frame(id = ids, nobs = cc)
}