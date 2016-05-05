## Create a special matrix boject that stores a matrix and caches its inverse.

## Create a matrix object

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setm <- function(solvem) m <<- solvem
  getm <- function() m
  list(set=set, get=get, setm=setm, getm=getm)
}


## Check if inverse has been calculated; if not, compute the inverse of the matrix using solve(x).

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getm()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  newmat <- x$get()
  m <- solve(newmat, ...)
  x$setm(m)
  return(m)
}
