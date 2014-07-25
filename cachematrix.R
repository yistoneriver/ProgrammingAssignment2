## Two functions, makeCacheMatrix and cacheSolve, save costly computation,
## utilizing a cache mechanism.

## makeCacheMatrix(x = matrix())
##    x must be a squre matrix, otherwise the function raises an error.
##
##    returns: a "cache matrix" object for a given matrix.
##    The cache matrix is a list including
##    the following four functions:
##      1) setVal(x) --  store the matrix
##      2) getVal()  --  retrieve the matrix
##      3) setInverse(m) -- cache the inverse of the matrix
##      4) getInverse() -- retrieve the cached inverse.

makeCacheMatrix <- function(x = matrix()) {
    if (nrow(x) != ncol(x))
      stop("matrix must be squre")
    
    m <- NULL
    
    setVal <- function(y) {
      if (nrow(x) != ncol(x))
        stop("matrix must be squre")
      
      if (identical(x,y)) 
        return
      x <<- y
      m <<- NULL
    }
    
    getVal <- function() x
    setInverse <- function(inv) m <<- inv
    getInverse <- function() m
    list(setVal = setVal, getVal = getVal,
         setInverse = setInverse,
         getInverse = getInverse) 
}


## cacheSolve(x, ...)
##    x: a cached object returned by makeCacheMatrix.
##    ...:  optional parameters passed to solve().
##
##    returns the inverse of a matrix stored in a given cached object x
##    if the matrix is inversible.
##    The inverse has already been calculated, cacheSolve just returns the 
##    cached value.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      m <- x$getInverse()
      if(!is.null(m)) {
        message("getting cached data")
        return(m)
      }
      data <- x$getVal()
      m <- solve(data, ...)
      x$setInverse(m)
      m
}
