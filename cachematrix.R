## Put comments here that give an overall description of what your
## functions do
# Part of the R Programming assignments on Coursera.org


## Write a short comment describing this function
# A vector used to store a matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
            x <<- y
            inverse <<- NULL
        }
        get <- function() x
        setInv <- function(inv) inverse <<- inv
        getInv <- function() inverse
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}



## Write a short comment describing this function
# This function calculates the inverse of a matrix by looking at the cached data first.
# If there is a cached data, the function returns the cache data without computing the matrix inverse.
# If no cache data, it will compute the invese of the matrix and return the result.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getInv()
        if(!is.null(inverse)) {
            message("getting cached data")
            return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setInv(inverse)
        inverse
}
