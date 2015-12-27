## To run the function the values of a squared matrix should be informed
## as a vector c(...). Also number of rows and columns must be informed.

## This funcion set the value of a matrix, gets its value,
## sets and gets its inverse

makeMatrix <- function(dados,nrow,ncol,...) {
    x <- matrix(dados,nrow,ncol,...)
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## This function inverts the matrix created in function above.
## It checks the cache for the availability of the inverse of the matrix.
## If the inverse is available it is read from chache, otherwise it is calculated.

cacheinverse <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
