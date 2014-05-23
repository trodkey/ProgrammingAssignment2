## Put comments here that give an overall description of what your
## functions do

## Creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {  ##create a function that creates empty matrix named "x" to invert as input
 
  m <- NULL  ## set the cache named m to NULL within makeCacheMatrix 
    
    set <- function(y) {  ## create the function "set" that takes argument "y"
      
      x <<- y  ## assign the empty matrix "x" the value of the function argument (y) & use superoperator <<-
                ## to scope x throughout all of makeCacheMatrix, not just within function "set"
      
      m <<- NULL  ## assign the cache "m" NULL & scope throughout makeCacheMatrix using <<- to override previously
                  ## assigned cache "m" values within makeCacheMatrix
    
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
