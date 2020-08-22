# Write the following functions:

#  makeCacheMatrix: 
#  This function creates a special "matrix" object that can 
#  cache its inverse.


## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  b <- NULL
  set <- function(y) {
    x <<- y
    b <<- NULL
  }
  get <- function() x
  setmean <- function(avar) b <<- avar
  getmean <- function() b
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
  
}



#  cacheSolve: 
#  This function computes the inverse of the special 
#  "matrix" returned by makeCacheMatrix above. If the inverse 
#  has already been calculated (and the matrix has not changed), 
#  then the cachesolve should retrieve the inverse from the cache.


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached inverse")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmean(m)
  m
}

