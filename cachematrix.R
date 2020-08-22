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
#  Input:   x of type matrix
#  
#  Returns:  A special matrix object (list) that can store the 
#            inverse of the matrix. 
#
#  Details:
# 
#  The returned object is a list containing four named 
#  elements that are functions.  Each list element and 
#  corresponding function is summarized below
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


#  cacheSolve
#
#  This function computes the inverse of the special 
#  "matrix" returned by makeCacheMatrix above. If the inverse 
#  has already been calculated (and the matrix has not changed), 
#  then the cacheSolve should retrieve the inverse from the cache.
#
#  No error checking is performed on the matrix.  Assumption is that 
#  the matrix is square and invertable. 
# 
#  Input:   x of type makeCacheMatrix
#  
#  Returns:  Inverse of x
#
#  Details:  Uses the solve function 
#

cacheSolve <- function(x, ...) {
  # Get inverse matrix
  s <- x$getsolve()
  
  # Check if inverse matrix is cached, and, if true, 
  # return the matrix
  if(!is.null(s)) {
    message("getting cached inverse")
    return(s)
  }
  
  # In no inverse matrix found, calculate inverse and cache 
  # value. 
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}


# Code to unit test makeCacheMatrix and cacheSovle 

unitTest <- function() {
  
  # Create 3 x 3 matrix and test makeCacheMatrix and cacheSolve
  
  a <- matrix(c(3, 5, 2, 5, 0, -1, 4, 9, -3), nrow = 3, ncol = 3)
  message("Matrix a:")
  print(a)
  
  aCacheMatrix <- makeCacheMatrix()
  aCacheMatrix$set(a)
  cacheSolve(aCacheMatrix)
  message("Inverse Matrix a:")
  print(aCacheMatrix$getsolve())
  cacheSolve(aCacheMatrix)
  
  # Create 4 x 4 matrix and test makeCacheMatrix and cacheSolve
  
  b <- matrix( c(1, 2, 5, -1,
                 1, 0, -4, 4,
                 4, 7,  2, 1,
                 4, 1, -2, 5), nrow = 4, ncol = 4)
  message("Matrix b:")
  print(b) 
  bCacheMatrix <- makeCacheMatrix(b)
  cacheSolve(bCacheMatrix)
  message("Inverse Matrix b:")
  print(bCacheMatrix$getsolve())
  cacheSolve(bCacheMatrix)  
}

