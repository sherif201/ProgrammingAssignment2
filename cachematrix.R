## These functions allows to compute the inverse of a matrix while storing its
## value inside their enviorment, allowing to call them at anytime without 
## the need to compute the matrix again, making use of the lexcial scoping
## feature of R


## This function takes the value of x the formal of a matrix, the assumption here
## it's invertible, it sets the 4 getters and assigns it a list for the other
## function (cacheSolve) to read.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function will have 2 options, if it finds a previous stored inverse in
## the makeCacheMatrix then it will get it, if not then it will compute it and
## cache it there and return the value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
