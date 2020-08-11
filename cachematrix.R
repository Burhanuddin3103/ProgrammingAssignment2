## code to find and cache the inverse a matrix

## function creates a matrix 

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function() x
  setmat <- function() inv <<- solve(x)
  getmat <- function() inv
  list(set = set, get = get, setmat = setmat, getmat = getmat)
}


## function checks if inverse matrix is avaliable 
## If not then function creates a inverse matrix and caches it

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getmat()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matx <- x$get()
  inv <- solve(matx, ...)
  x$setmat(inv)
  inv
}