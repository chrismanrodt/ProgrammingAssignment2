## This is Chris Manrodt's submission for the Coursera Course R Programming
## This is programming assignment 2, for course week 3
## These functions take advantage of R's lexical scoping to solve 
## for a square matrix inversion when needed, but delivering a cached solution
## if this has been previously computed

## this function returns a list of functions, passing get and set for the 
## input matrix and an inversion, if previously solved by cacheSolve

makeCacheMatrix <- function(neo = matrix()) {
    invneo <- NULL
    set <- function(y) {
        neo <<- y
        invneo <<- NULL
    }
    get <- function() neo
    setinv <- function(inv) invneo <<- inv
    getinv <- function() invneo
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve returns a matrix inversion, using the solve() function, by 
## calling the functions within list neo to check if the matrix has been
## inverted previously, and returning the previous calc to save on bandwidth

cacheSolve <- function(neo, ...) {
    invneo <- neo$getinv()
    if(!is.null(invneo)) {
        message("getting cached inverse matrix")
        return(invneo)
    }
    data <- neo$get()
    invneo <- solve(data, ...)
    neo$setinv(invneo)
    invneo
        ## Return a matrix that is the inverse of 'neo'
}
