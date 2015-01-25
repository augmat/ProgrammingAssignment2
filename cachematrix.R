## --------------------------------------------------------------------
## Course       : R Programming
## Description  : Programming Assignment 2 - Functions that cache the
##                inverse of a matrix.
## Author       : Mathieu Auger-Perreault
## Date         : January 25, 2015
## --------------------------------------------------------------------

## --------------------------------------------------------------------
## Function     : makeCacheMatrix
## Description  : Creates a matrix that can cache its inverse.
## --------------------------------------------------------------------
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      ## Variables x and m, in the containing environment, are updated to y and NULL
      x <<- y
      m <<- NULL
    }
    ## return x from the parent environment
    get <- function() x
    ## set and get the inverse in the cache
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## --------------------------------------------------------------------
## Function     : cacheSolve
## Description  : Computes the inverse of a matrix. Retrieves the cache
##                value if it has already been computed.
## --------------------------------------------------------------------
cacheSolve <- function(x, ...) {
  ## Returns a matrix that is the inverse of 'x'
  m <- x$getinverse()
  
  ## If the inverse already has been computed
  if(!is.null(m)) {
    ## Return the value from the cache
    message("getting cached data")
    return(m)
  }
  
  ## Assign the matrix to the data variable
  data <- x$get()  
  ## Compute the inverse of the matrix and assigns the results to x
  m <- solve(data, ...)  
  ## Set the inverse so that it will be reused for future calls using the same data
  x$setinverse(m)
  m
}
