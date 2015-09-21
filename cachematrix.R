## Example
## > source("cachematrix.R")
## > m1 <- matrix(1:10,2,2)
## > mc <- makeCacheMatrix(m1)
## > mcmc$get$get()
##      [,1] [,2]
##[1,]    1    3
##[2,]    2    4

## First run
## > cacheSolve(mc)
##      [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5

##  Second Run (now, cache was used)
## > cacheSolve(mc)
##getting cached data
##      [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5



# makeCacheMatrix: This function creates a special "matrix" object that
##    can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y)
        {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) m <<- solve(x)
        getInverse <- function() m
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


# cacheSolve: Compute the inverse of the matrix. If the result is already calculated,
# it returns the cached result.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m))
        {
                message("getting cached data")
                return(m)
        }
        m <- solve(x$get())
        x$setInverse(m)
        m        
}
