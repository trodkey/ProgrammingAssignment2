## Put comments here that give an overall description of what your
## functions do

## Creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {  ##create a function to cache the matrix to be inverted
                                             ## instantiate the empty matrix x - if no argument is passed to cacheMatrix x is an empty matrix
                                              ## return 4 functions created by this function to carry out the inversion
  m <- NULL  ## set the cache named m to NULL within the scope of makeCacheMatrix 
            ## if makeCacheMatrix is called without reference to any of the 4 functions inside it, set cache m to NULL
    
    set <- function(y) {  ## create the function "set" that takes argument "y"
      
      x <<- y  ## takes the matrix passed to it as " y" and assigns it to variable "x".  Also use superoperator <<-
                ## to scope x throughout all of makeCacheMatrix, not just within function "set"
              
      m <<- NULL  ## Clears the cache -  make cache "m" NULL & scope throughout makeCacheMatrix using <<- to override previously
                  ## assigned cache "m" values within makeCacheMatrix
    
    }
    
    get <- function() x  ## the function "get" returns whatever is stored in the matrix "x"
    setSolve <- function(solve) m <<- solve ## setSolve takes the matrix passed to it and stores it in the cache "m"
                                            ## use the superoperator to allow this behavior within all of makeCacheMatrix
    getSolve <- function() m  ## function to return the cached matrix
    list(set = set, get = get,  ## create a named list of the 4 functions
         setSolve = setSolve,
         getSolve = getSolve)
  }

## Computes the inverse of the "matrix" returned by makeCacheMatrix 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
   m <- x$getSolve()  ## call getSolve to return the contents of the cache and assign it to local variable m scoped to cacheSolve
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }  ## if the cache contains data return it 
    data <- x$get()  ## if cache is empty take the matrix and assign it to local variable data
    m <- solve(data, ...)  ## invert the matrix and assign it to local variable m, overwriting m
    x$setSolve(m)  ## store the inverted matrix in the cache
    m  ## return the inverted matrix to the calling function
