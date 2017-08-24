## makeCacheMatrix : caches a matrix and its inverse
## cacheSolve      : tries to use a previously cached inverse matrix data, if none is
##                   found computes one

## makeCacheMatrix : function optional argument is a matrix.
##                   Returns a list of functions to create and manipulate a 
##                   matrix and its inverse. 
## functions made available are
## set : caches a matrix data. Invoking this function with an argument blanks 
##       out inverse matrix data.
## get : returnS cached matrix data
## setminv : computes and caches inverse data of cached matrix
## getminv ; returns cached inverse matrix data
## variables
## x : matrix data
## m : inverse matrix data
makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      
      ## cache matrix data and
      ## blank out any cached inverse matrix data
      set <- function(y=NULL) {
            if(is.null(y)) {
                  message("provide a matrix")
                  return (NULL)
            }
            x <<- y
            m <<- NULL
      }
      
      ## return cached matrix
      get <- function() {
            if (is.null(x)) {
                  message("no matrix data found")
                  return(NULL)
            }
            x
      }
      
      ## compute inverse matrix and cache result
      setminv <- function(){
            if (is.null(x)) {
                  message("no matrix data found")
                  return(NULL)
            }
            message("inverse matrix is computed")
            
            m <<- solve(x)
      }
      
      ## return cached inverse matrix data
      getminv <- function() {
            if (is.null(m)) {
                  message("no inverse matrix data found")
                  return(NULL)
            }
            m
      }
      
      ## returns list of functions to manipulate matrix
      list(set = set, get = get, setminv = setminv, getminv = getminv)
}

## cacheSolve : function argument is list returned by makeCacheMatrix
##              returns inverse matrix data of matrix associated with argument
##              computes inverse matrix when not found in cache
## variables
## x : list returned by makeCacheMatrix
## m : cached inverse matrix data
cacheSolve <- function(x, ...) {
      
      ## Look for cached inverse matrix data location
      m <- x$getminv()
      
      ## return cached inverse matrix data
      if (!is.null(m)) {
            message("getting cached inverse matrix")
            return(m)
      }
      
      ## no cached inverse matrix data was found
      
      ## compute and cache inverse matrix
      x$setminv()
      
      ## return cache inverse matrix
      x$getminv()
}