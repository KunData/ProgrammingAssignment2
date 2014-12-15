## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a special 'matrix' object that can cache 
## its inverse. cacheSolve computes the inverse of the special 'matrix'
## returned by makeCacheMatrix. If the inverse has already been calculated
## for the same matrix, the cacheSolve will retrieve the inverse from the cache.

## Usage Example:
## 1. source cachematrix.R to the working directory in R Console.
## 2. generate a test matrix with 
##	mTest = matrix(c(1, 5, 10, 8, 19, 20, 16, 7, 14), nrow = 3, ncol = 3, byrow = TRUE)
## 3. create a special 'matrix' object with x <- makeCacheMatrix(mTest)
## 4. call cacheSolve(x) to return the inverse of x (actually mTest) as:
##		   [,1]          [,2]        [,3]
##	[1,] -0.09589041 -6.938894e-18  0.06849315
##	[2,] -0.15829528  1.111111e-01 -0.04566210
##	[3,]  0.18873668 -5.555556e-02  0.01598174

## Note:
## if matrix 'x' is NOT invertible, install Package "MASS" from CRAN 
## call ginv(x, tol = sqrt(.Machine$double.eps))
## to calculate the Moore-Penrose generalized inverse of 'x'.   
 

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	## Create a special 'matrix' object that can cache its inverse
  matInv <- matrix()
  set <- function(y) {
    x <<- y
    matInv <<- matrix()
  }
  get <- function() x
  setInverse <- function(inverse) matInv <<- inverse
  getInverse <- function() matInv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
  matInv <- x$getInverse()
  if(!is.na(matInv)) {
    message("getting cached inverse")
    return(matInv)
  }
  mat <- x$get()
  matInv <- solve(mat, ...)
  x$setInverse(matInv)
  matInv
}

