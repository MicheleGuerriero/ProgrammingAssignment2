## The following two functions allow to cache and manage a matrix and its inverse.
## The first one create a special object allowing to managet (using getters and setters)
## a matrix and its inverse. The second one implements the caching.

## This function creates and returns a special object which is a list of functions managing 
## a square matrix and its inverse, that the function receive as the first argument. 
## Through the returned object we can in fact get/set both the original matrix
## and its inverse. 
## The returned object can be seen as a java class exposing some
## basic getters and setters for its attributes. In this case the two attribute
## are the original matrix received as argument and the internal field "inv".


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(invers) inv <<- invers
  getinv <- function() inv
  matrix<-list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  matrix
}


## This function receives an object like the one returned from the "makeCacheMatrix" function
## and it just updates the "inv" field of such object using the provided setter (the setinv function).
## After the update, calling the "getinv" function will return the updated inverse matrix.
## The greate advantage of using this function is the combined with the object returned by the "makeCacheMatrix"
## is that such object is able to cache the inverse of a matrix and the matrix itself
## while the cacheSolve matrix is able to know in advance if the received object has already the inverse matrix cached
## and in this case, it avoids the time consuming computation of the matrix. Finally it just return the inverse matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}
