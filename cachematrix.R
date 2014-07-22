## SOurce: https://class.coursera.org/rprog-005/human_grading/view/courses/972576/assessments/3/submissions
## Modified sample functions makevector() and cachemean() to compute and cache the inversion of a matrix.
##
## How to use:
##
## 1. > my.matrix <- matrix(rnorm(9), nrow = 3)            # create or assign an invertible matrix to the variable name of your choice
## 2. > cache.my.matrix <- makeCacheMatrix(my.matrix)      # create a list containing all the objetcs needed to store the cached matrix
## opt.  > cache.my.matrix$get()                           # optionally check if my.matrix was stored and can be accessed by the method get()
## 3. > cacheSolve(my.matrix)                              # compute and return the inverse of my.matrix
## opt.  > cacheSolve(my.matrix)                           # optionally Call function cachesolve() again to see that it returns the cached inverted matrix



# makeCacheMatrix: returns a list of functions to:
# 1. Set the value of a matrix
# 2. Get the value of a matrix
# 3. Set the value of the inverted matrix
# 4. Get the value of the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
  # assign NULL to variable matrix
  matrix <- NULL
  #declare set function for the purpose to change the matrix stored in an instance of makeCacheMAtrix()
  set <- function(y) {
    x <<- y
    matrix <<- NULL
  }
  #declare get function to return x
  get <- function() x
  
  #store the setinversion function to store the inversion of x
  setinversion <- function(inverse) matrix <<- inverse
  #store the getinversion function to retrun the inversion of x
  getinversion <- function() matrix
  # the list that actually caches the stuff
  list(set = set, get = get,
       setinversion = setinversion,
       getinversion = getinversion)

}


# cacheSolve() Computes the inverse of the matrix in case
# it is not cached yet and returns the inversion

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinversion() #x$getinversion() looks if x already has the cached inversion
    if(!is.null(inverse)) {
      # let us know if the inversion was returned from cache
      message("getting cached data")
      return(inverse)
    }
    ## read the matrix out of x's list  
    matrix <- x$get()
    ## now do the math
    inverse <- solve(matrix, ...)
    ## and store the result while using x's setinversion method
    x$setinversion(inverse)
    ##and finally display the result
    inverse
}
