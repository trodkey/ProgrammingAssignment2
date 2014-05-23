## Put comments here that give an overall description of what your
## functions do

## Creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {  ##create a function that creates empty matrix "X" to invert as input
    m <- NULL  ## set m = NULL within makeCacheMatrix, 
    set <- function(y) {
      x <<- y  ## assign x the value of the function argument (y) within the "set" function
      
      m <<- NULL  ## assign "m" NULL inside the "set" function so don't overwrite in parent makeCacheMatrix environment
    
    }
    
    get <- function() x  ## assign variable "get" the empty matrix "x"
    setSolve <- function(solve) m <<- solve 
    getSolve <- function() m
    list(set = set, get = get,  ## create a named list of functions
         setSolve = setSolve,
         getSolve = getSolve)
  }

## Computes the inverse of the "matrix" returned by makeCacheMatrix 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
   m <- x$getSolve()  ## run getSolve over matrix named "x"
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setSolve(m)
    m
  }
