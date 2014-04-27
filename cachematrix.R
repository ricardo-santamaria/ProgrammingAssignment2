## Caching the Inverse of a Matrix
## R Programming (Coursera) -- Peer Assignment

## Functions that cache the inverse of a matrix.
## USE:
## specialMatrix <- makeCacheMatrix(matrix) 
## cacheSolve(specialMatrix)

## Creates a "special matrix" object that can cache its inverse
## WARNING: assume that the matrix supplied is always invertible

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Computes the inverse of the "special matrix"
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m  
}
