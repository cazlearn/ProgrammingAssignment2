## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## The first function, makeCacheMatrix creates a special "matrix", 
## which is really a list containing four functions to 
##
## 1.	set the value of the matrix
## 2.	get the value of the matrix
## 3.	set the value of the inverse matrix
## 4.	get the value of the inverse matrix

makeCacheMatrix <- function(mx = matrix()) {
    inv <- NULL
    f_set <- function(nx) {
        mx <<- nx
        inv <<- NULL
    }
    f_get <- function() mx
    f_setinverse <- function(inverse) inv <<- inverse
    f_getinverse <- function() inv
    list(  set = f_set
         , get = f_get
         , setinverse = f_setinverse
         , getinverse = f_getinverse)
}

## The following function finds the inverse matrix of the invertible
## special "matrix" created with the above function. 
##
## However, it first checks to see if the matrix inverse has already been calculated. 
## If so, it gets the inverse matrix from the cache and skips the computation. 
## Otherwise, it finds the inverse of the matrix data and 
## sets the inverse matrix in the cache via the f_setinverse function.

cacheSolve <- function(mx, ...) {
    ## Return a matrix that is the inverse of 'mx'
    ## Assuption:  Matrix 'mx' is a square matrix and is invertible.
    inv <- mx$getinverse()
    if(!is.null(inv)) {
        message("getting cached matrix data")
        return(inv)
    }
    data <- mx$get()
    inv <- solve(data, ...)
    mx$setinverse(inv)
    inv
}