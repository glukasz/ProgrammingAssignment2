## The goal of this excersise is to create matrix with cached inverse  capability.
## To achive that I prepared three functions:
## makeCacheMatrix - this function creates matrix object with cached invers
## cacheSolve - function which does the inverse of matrix returning as a result regular matrix
## cacheSolveCachedResult - function which does the invers of matrix returning as a result a matrix
##                          which itself has cahced inverse capability


## function creates a matrix with cached inverse capability
## in reality it's a list of multiple functions which allow to operate
## on the matrix
makeCacheMatrix <- function(x = matrix()) {
  # inversed (regular) matrix
  inversed <- NULL
  # inversed matrix with cached inverse capability
  cachedInversed <- NULL
  
  get <- function() {
    x 
  }
  
  set <- function( m ) {
    x <<- m
    inversed <<- NULL 
    cachedInversed <<- NULL
  }
  
  getinv <- function() {
    inversed
  }
  
  setinv <- function( inv ) {
    inversed <<- inv
    cachedInversed <<- makeCacheMatrix( inv )
  }
  
  getcachedinv <- function() {
    cachedInversed
  }
  

  list(get = get,
       set = set,
       getinv = getinv,
       setinv = setinv,
       getcachedinv = getcachedinv)
}


## Funciton does the inverse of matrix with cached inverse capability.
## If the inverse was already done and the matrix wasn't changed the iverse
## is retrieved from the cached.
## The correctness of the inverse can be checked (assuming m is a matrix and inv
## is an inverse) running:
## m$get() %*% inv -> it should return a singular matrix
cacheSolve <- function(x, ...) {
  inversed <- x$getinv()
  if( !is.null(inversed) ) {
    message( "getting cached data" )
    return( inversed )
  }
  
  m <- x$get()
  inversed <- solve( m, ... )
  x$setinv( inversed )
  inversed
}


## Funciton does the inverse of matrix with cached inverse capability.
## If the inverse was already done and the matrix wasn't changed the iverse
## is retrieved from the cached.
## The returned matrix (being an inverse) is itself a matrix with cached inverse
## capability (description of the exercise didn't state clearly what type of matrix
## should be the inverse so I implemented both types).
## The correctness of the inverse can be checked (assuming m is a matrix and inv
## is an inverse) running:
## m$get() %*% inv$get() -> it should return a singular matrix
cacheSolveCachedResult <- function(x, ...) {
  if( !is.null(x$getcachedinv()) ) {
    message( "getting cached data" )
    return( x$getcachedinv() )
  }
  
  m <- x$get()
  inversed <- solve( m, ... )
  x$setinv( inversed )
  x$getcachedinv()
}