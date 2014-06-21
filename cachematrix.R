## RProgamming Assignment 2
## 2014_06_22
## 
## The lexical scoping rules of the R language can be manipulated to preserve
## the state of a variable in an R object.  This can speed up code executuion
## where the initial result of a time consuming calculation can be cached to a variable
## which is looked up by subsequent loops instead of being recomputed.
## 
## Example: 
##    
##    The inversion of a matrix (usually a costly computation) 
##
##    Using the following pair of functions
##
##    1.  `makeCacheMatrix` which creates a special "matrix" object
##         that can cache its inverse.
##
##    2.  `cacheSolve` which computes the inverse of the special
##        "matrix" returned by `makeCacheMatrix` above. 
##         If the inverse has already been calculated (and the matrix has not changed), 
##         then `cacheSolve` retrieves the inverse from the cache.
##  
##    The functions assume that the supplied matrix is always invertible
##
##    The `<<-` operator assigns a value to an object in an environment 
##    different from the current environment. 


  ###  1.   The 'makeCacheMatrix' function creates a special "matrix"
  ###       which is a list of functions that
  ###        a.  sets the value of the matrix
  ###        b.  gets the value of the matrix
  ###        c.  inverts the matrix
  ###        d.  gets the value of the matrix inversion

  makeCacheMatrix <- function(x = matrix()) {
  
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
  


  ###  2.   The 'cacheSolve' function returns a matrix that is the inverse of 'x'
  ###       created in makeCacheMatrix.  If the inversion has already been computed
  ###       then the cached result is used, otherwise the matrix is inverted and
  ###       & its value is stored in the cache via the 'setinverse' function.
  
  cacheSolve <- function(x, ...) {
      
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
  

