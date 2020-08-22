#  cachematrix.R
#
#  Author:  Lawrence Brown
#  Date:    22 Aug 2020
#
#  Fucnction:  
#
#  makeCacheMatrix 
#  
#  This function creates a special "matrix" object that can 
#  cache its inverse.
#
#  This function assumes the input matrix is square and has
#  an inverse.  No error checking is made on these two 
#  stated properties. 
#
#  Input:  x of type matrix
#  
#  Returns:  A special matrix object (list) that can store the 
#            inverse of the matrix. 
#
#  Details:
# 
#  The returned object is a list containing four named 
#  elements that are functions.  Each list name and functions
#  are summarized below
#
#  set(x)  Store the matrix x
# 
#  get()   Return the stored matrix
#
#  setsolve(x)  Sets the inverse of the matrix x
#
#  getsolve()   Returns the stored inverse matrix
#

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  
  # Assign value y into parent environment variable x
  # and create parent environment variable y will value NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  
  # Return the stored matrix
  get <- function() x
  
  # Stored the argument of setsolve (inverse of matrix) 
  # into the parent environment variable s
  setsolve <- function(i) s <<- i
  
  # Return the inverse matrix stored in the parent environment 
  getsolve <- function() s
  
  # Return a list contain for named functions above
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
  
}



#  cacheSolve: 
#  This function computes the inverse of the special 
#  "matrix" returned by makeCacheMatrix above. If the inverse 
#  has already been calculated (and the matrix has not changed), 
#  then the cachesolve should retrieve the inverse from the cache.


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached inverse")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}

