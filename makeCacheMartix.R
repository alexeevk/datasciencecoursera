#######################################################
# makeCacheMatrix()
# Description: 
#    The method creates an object with inversed matrix
#    calculation support. The calculated value is 
#    cacheed once calculated.
# Parameters:
#    x - matrix
# Example:
#    x <- makeCacheMatrix(matrix(1:4, 2, 2))
#    x$getInversed()
# Comments:
#    Having one object that handels inversed value as 
#    well as its cache represents much more robust 
#    solution. It makes much more sense to manage cached
#    value in the same object where data is managed vs.
#    having this functioanlity spread across two methods.
#
#    I could not resist to create better code.
#######################################################
makeCacheMatrix <- function(x = matrix()) {
    
    inversedMatrixData <- NULL #invalidate cache
    matrixData <- x #save data
    
    #set() allows to reset the object with new data
    set <- function(value) {
        matrixData <<- value #save data
        inversedMatrixData <<- NULL #invalidate cache
    }
    
    get <- function() matrixData #returns object data
    
    #either gets cached value or calculates inversed value and saves it in the cache 
    getInversed <- function() {
        if(is.null(inversedMatrixData)){
            inversedMatrixData <<- solve(matrixData)
            print("Calculate inversed value")
        }
        else
            print("Cached value is used")
        
        return(inversedMatrixData)
    }
    
    list(set = set, get = get,
         getInversed = getInversed)
}

#######################################################
# cacheSolve()
# Parameters:
#     x - object created with makeCacheValue() method
# Examples:
#     a) cacheSolve(makeCacheMatrix(matrix(1:4, 2, 2))
#
#     b) x <- makeCacheMatrix(matrix(1:4, 2, 2))
#        cacheSolve(x)     #calculating
#        cacheSolve(x)     #using cache
# Summary:
#     Method encapsulates makeCachedMatrix() interface
#     reusing its functionality. 
#     cacheSolve() was created to comply with programming 
#     assigment task definition ONLY and is redundant.
#######################################################
cacheSolve <- function(x, ...) {
    m <- x$getInversed()
    m
}