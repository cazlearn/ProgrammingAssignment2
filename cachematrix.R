## The two functions makeCacheMatrix and cacheSolve work together to use caching
## to find the inverse of a special matrix only if the inverse has not already 
## been calculated. 
## NOTE:  These functions assume the original matrix is an invertible square matrix.  
## If this is not the case, an error message from the R function "solve" is displayed
## that the "system is singular".

## The first function, makeCacheMatrix creates a special "matrix", 
## which is really a list containing four functions for the specified matrix to 
##
## 1.	set the value of the matrix                    (f_set(mx=matrix())
## 2.	get the value of the matrix                    (f_get())          
## 3.	set the value of the inverse matrix in cache   (f_setinverse(inverse=matrix()))
## 4.	get the value of the inverse matrix from cache (f_get_inverse())

## The second function, cacheSolve, determines the inverse of the special matrix
## using the R function "source".  If the matrix for which the inverse is to be found
## has not been modified since the last determination of the inverse, the existing 
## inverse is retrieved from the cache and is not recalculated.

## To test the functions, for a square, invertible matrix "a", run:
##              a$get() %*% a$getinverse()
## and you should get the identity matrix returned.
##--------------------------------------------------------------------

## The following function creates the cached matrix and inverse matrix (empty) and
## returns the list of four functions to access those values.
##
## Note: The makeCacheMatrix function does not calculate the matrix inverse.
##
## Useage of makeCacheMatrix and the returned functions for the special matrix, mx:
## (uses the names in the function list)

##     a <- makeCacheMatrix( square_matrix )
##       where "square_matrix" is an invertible, square matrix.
##       "a" is the special matrix with a cached value and the four associated functions.

##     mx$set(nx):  Sets the new value of the special matrix to "nx"
##     mx$get():    Returns the value of the special matrix from the cache.
##     mx$setinverse(inverse):  Sets a new value of the cached inverse matrix to "inverse".  
##     mx$getinverse():  Returns the value of the matrix stored in the inverse matrix cache.

makeCacheMatrix <- function(mx = matrix()) {
    ## Define the four functions (get, set, getinverse, setinverse) 
    ## for the original matrix, mx. 
    
    inv <- NULL   ## create and initialize an empty inverse matrix variable. 
    
    f_set <- function(nx) {
        ## Set the cached matrix, mx, to the new passed matrix value, nx.
        mx <<- nx    
        
        ## Reset the cached inverse matrix to empty as the matrix has changed.
        inv <<- NULL 
    }
    
    f_get <- function() {
        ## Retrieve the cached matrix, mx, and return it to the calling process.
        mx
    }
    
    f_setinverse <- function(inverse) {
        ## Set the cached inverse matrix, inv, to the passed in value, inverse.
        
        ## Note: This function does NOT calculate the inverse matrix and
        ## may create a condition where the cached values of the original matrix
        ## and the inverse matrix are inconsistent.
        inv <<- inverse
    }
    
    f_getinverse <- function() {
        ## Retrieve the inverse matrix value, inv, from the cache. 
        ## Note: This function does NOT calculate the inverse matrix.
        inv
    }    
        
    ## Return a list of the four functions named "set", "get", "setinverse", and
    ## "getinverse" to the calling process for matrix.
    list(  set = f_set
         , get = f_get
         , setinverse = f_setinverse
         , getinverse = f_getinverse)
}


## The following function finds the inverse matrix of the invertible
## special "matrix" created with the makeCacheMatrix function. 
##
## However, cacheSolve first checks to see if the matrix inverse has already been calculated. 
## If so, the inverse matrix is retrieved from the cache and no additional computation
## is performed. Otherwise, cacheSolve finds the inverse of the matrix data and 
## sets the inverse matrix in the cache via the makeCacheMatrix["setinverse"] function.

## Useage of cacheSolve, for the special matrix a:
## (requires that "mx" be first defined with makeCacheMatrix)

##     a_inv <- cacheSolve( a )
##       where "a" is a special maxtrix established with makeCacheMatrix.
##       "b" is the cached inverse matrix for "a"

cacheSolve <- function(mx, ...) {
    ## Return a matrix that is the inverse of 'mx'
    ## Assuption:  Matrix 'mx' is a square matrix and is invertible.
    
    ## Retrieve the existing inverse matrix value from cache.
    inv <- mx$getinverse()  
    
    ## Check if the retrieved inverse matrix is empty.  If not, then just use
    ## the retrieved value and do not recompute the inverse.
    if(!is.null(inv)) {
        message("getting cached matrix data")
        return(inv)
    }
    
    ## If the retrieved inverse matrix IS empty (implied "elsif"), then calculate
    ## the new inverse matrix value.
    
    ## First, get the original matrix, mx, from the cache and put in a local variable, data.
    data <- mx$get()
    
    ## Use the R function "solve" to calculate the inverse matrix.  
    ## "solve(a, b, ...)" calculates the matrix product "a * x = b" where "a" and "b" are 
    ## both matrix values passed in and returns the matrix, x, as the function value.
    ## If "b" is not supplied by the calling process, "b" is assumed to be the 
    ## identity matrix, I and the calculation is therefore the inverse matrix.
    
    inv <- solve(data, ...)
    
    ## Set the inverse matrix value in the cache using the function from makeCacheMatrix.
    mx$setinverse(inv)
    
    ## Return the inverse matrix.
    inv
}