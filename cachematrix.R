## Daniel Krastev
## October 15, 2016
## I certify that this submission is my own original work in accordance with the Coursera Honor Code.

## ------------------------------------------------------------------------
## makeCacheMatrix: This function creates a special "matrix" object 
## that can cache its inverse.
## 
## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve
## retrieves the inverse from the cache.
## -----------------------------------------------------------------------

## The first function, makeCacheMatrix creates a special "matrix", which is 
## a list containing a function to:

## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
        x <<- y
        inv <<- NULL
      }
      get <- function() {x}
      setinv <- function(inverse) {inv <<- inverse}
      getinv <- function() {inv}
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}


## The following function calculates the inverse of the special "matrix" created with the above 
## function. It first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates
## the inverse of the data and sets its value in the cache via the setinv function.

cacheSolve <- function(x, ...) {
      inv <- x$getinv()
      if(!is.null(inv)) {
        message("getting cached data...")
        return(inv)
      }
      data <- x$get()
      inv <- solve(data)
      x$setinv(inv)
      inv
}

## Test Case:
## > x = rbind(c(1, -0.5), c(-0.5, 1))
## > m = makeCacheMatrix(x)
## > m$get()
##      [,1] [,2]
##  [1,]  1.0 -0.5
##  [2,] -0.5  1.0

## > cacheSolve(m)
##           [,1]      [,2]
##  [1,] 1.3333333 0.6666667
##  [2,] 0.6666667 1.3333333

## > cacheSolve(m)
## getting cached data...
##            [,1]      [,2]
##  [1,] 1.3333333 0.6666667
##  [2,] 0.6666667 1.3333333
