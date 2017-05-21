## These functions help construct a square matrix object with its inverse  
## with getters/setters, calculating the inverse only if it hasn't been calculated yet.
## Else, cached data is retrieved.

## makeCacheMatrix has a square matrix object 'x' and its inverse 'i'
## Getter/Setter functions help to get/set the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(invers) i <<- invers
        getinv <- function() i
        list(set = set, get = get, getinv = getinv, setinv = setinv)
}


## cacheSolve retries the square matrix x's inverse if calculated earlier already.
## Else, it calculates it and then stores it.

cacheSolve <- function(x, ...) {
        i <- x$getinv()
        if(!is.null(i)){
                message("Getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data,...)
        x$setinv(i)
        i
}
