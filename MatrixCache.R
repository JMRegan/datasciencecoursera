##Caches the inverse of a matrix to save on computational power usage
##will only work on invertable matrices
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
    ##defining terms
  }
  get <- function() x
  ## define "get" as function x
  setinverse <- function(inverse) i <<- inverse
  ##if i is not available (NULL), take the inverse of i
  ##also searches through the parent environmment for a definition of i,
  ##if found, will use the previously defined definition
  getinverse <- function() i
  #anonymous function
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  ##defines these terms so the function is the same in different environments
}


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  ##extracts the getinverse subset of x
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
    ##if i is not null, then i is returned
  }
  data <- x$get()
  ##assigns the "get" subset of x to "data"
  i <- solve(data, ...)
  ##solves data for the inverse of "data" and assigns that to i
  x$setinverse(i)
  ##takes the "setinverse" subset of x 
  return(i)
  ##return i
}