## These functions "cache the inverse of a matrix."  I based the code for these
## two functions entirely on the example codes from the "Programing 
## Assignment 2: Lexical Scoping" instructions.
##
## To test it I used the command line:
## my_matrix2<-cacheSolve(makeCacheMatrix(my_matrix))
##
## I tested it with these two square invertible matrices
##> my_matrix
##     [,1] [,2]
##[1,]    4    3
##[2,]    3    2
##
## and
##
##> my_matrix
##     [,1] [,2] [,3]
##[1,]    1    2    3
##[2,]    0    4    5
##[3,]    1    0    6
##
## and it returned answers identical to solve(my_matrix)
##
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setSolve <- function(solve) s <<- solve
        getSolve <- function() s
        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then cacheSolve should retrieve the inverse from the
## cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getSolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setSolve(s)
        s
}
