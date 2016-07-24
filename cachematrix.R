## These two functiones are meant to emulate the behavior of the functions given as an example.
## But with the hability to catche the inverese of a matrix

##This function will eventually allow the inverse, but is actually used later on.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInv <- function(I) m <<- I
  getInv <- function() m
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}

## this function catches the inverse of the matrix, if the inverse has not been set, then it sets it, else it calls it.

cacheSolve <- function(x, ...) {
  m <- x$getInv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setInv(m)
  m}