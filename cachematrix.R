## These are a pair of functions that cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
                i <- NULL
                set <- function(y){
                        x <<- y
                        i <<- NULL
                }
                get <- function () x
                setinv <- function (inv) i<<-inv
                getinv <- function () i
                list (set = set, get = get, setinv = setinv, getinv = getinv)
        
}


## This function computes the inverse of the matrix returned by makeCacheMatrix function.
## If the inverse of the matrix has been calculated, then the cacheSolve should return the inverse from the cache.


cacheSolve <- function(x, ...) {        ## Is "..." here required?
        i <- x$getinv()                 ##i here is newly defined, x refers to mymatrix where (mymatrix <- makeCacheMatrix(x))
        if (!is.null(i)){
                message("getting cached data")
                return(i)               ##(i) here refers to cache data i. is this right?
        }
        data <- x$get()
        i <- sort(data,...)              ## Is "..." here required?
        x$setinv(i)
        i
}