## Description:
## makeCacheMatrix and cacheSolve are used to create a special object that
## stores a numberic matrix and caches its inverse matrix

## Assumption:
## The matrix supplied is always invertible

## Instructions:
## To run these two functions, you may follow the instructions as below:
##      1. create a numberic matrix
##      2. make a cache matrix:
##      3. get the inverse matrix:
## Example:
##    testM <- matrix(c(4,2,7,6),nrow = 2,ncol = 2)
##    cacheM <- makeCacheMatrix(testM)
##    cacheSolve(cacheM)

## -----------------------------------------------------------------------

## makeCacheMatrix creates a special "matrix", which contains a function to:
##      1. set the value of the matrix
##      2. get the value of the matrix
##      3. set the value of the inverse matrix
##      4. get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function(){
        x
    }
    setInverseMatrix <- function(inverseMatrix){
        m <<- inverseMatrix
    }
    getInverseMatrix <- function(){
        m
    }
    list(set = set, 
         get = get,
         setInverseMatrix = setInverseMatrix,
         getInverseMatrix = getInverseMatrix)

}


## cacheSolve calculates the inverse matrix of the special "matrix" created 
## by makeCacheMatrix function.
## If the inverse matrix has been calculated before, it gets the inverse 
## matrix from the cache and skips the computation;
## Otherwise, it calculates the inverse matrix and caches it.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverseMatrix()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverseMatrix(m)
    m
}
