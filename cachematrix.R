## Assignment 2 concepts:
##
## R uses lexical scoping, meaning it uses the variables values where the 
## function was defined.
##

#####################
## makeCacheMatrix ##
#####################
##
## Purpose: This function creates a special "matrix" object that can 
## cache its inverse. It's special because the matrix is created as object with 
## 2 attributes and 4 methods (just like an object oreinted language constructs):
## 
##  Matrix Attributes (object attributes):
##  * x, a variable of type matrix, used to store the matrix that was passed or 
##  created when the makeCacheMatrix was created/called.
##
##  * inverse_matrix, a variable of type matrix, used to store the value of 
##  the inverse of the "x" matrix that was passed or called.  
##
##  Methodes:
##  * get() - To get the value of the cached matrix      
##  * set(<cachematrix>) - To set the value of the cached matrix
##  * getinverse() - To get the inverse matrix value
##  * setinverse(<inverse of cachematrix>) - To set the inverse matrix value
##
##  Parameters:
##  This function takes a square (and assumed invertible) matrix as parameter
##  If user omits passing a matrix when the function is called, We initialize
##  an empty matrix.
##
## Usage: 
## Where testmatrix = matrix(c(1,1,-1,2), nrow=2, ncol=2, byrow=TRUE)
##
## a <- makeCacheMatrix(testmatrix) to create and initialize the cache matrix "a"
##
## a$get() - To get the value of the cached matrix
## [,1] [,2]
## [1,]    1    1
## [2,]   -1    2
##
## a$set(testmatrix) - To set the value of the cached matrix
##
## a$setinverse(solve(a$get())) - To set the value of the inverse matrix
##
## a$getinverse() - To return the value of the inverse matrix
## [,1]       [,2]
## [1,] 0.6666667 -0.3333333
## [2,] 0.3333333  0.3333333
##
##
###############################################################################

makeCacheMatrix <- function(x = matrix(, nrow = 0, ncol = 0)) {
    
    ## intializing a Variable used to store reverse of matrix
    inverse_matrix <- NULL  
    
    ## set funtion is being defined here so it will use the values of the 
    ## x and inverse_matrix as defined in the scope of makeCacheMatrix function.
    set <- function(y) {
        
        ## setting the value of the x matrix (makeCacheMatrix's scope)
        x <<- y
        
        ## emptying inverse_matrix variable (makeCacheMatrix's scope)
        inverse_matrix <<- NULL 
    }
    
    ## The get function returns the current value of 
    ## the x matrix (makeCacheMatrix's scope)
    get <- function() x
    
    ## The setinverse function will assign inverse_matrix (makeCacheMatrix's scope)
    ## to the value of "inverse_value" (setinverse scope)
    setinverse <- function(inverse_value) inverse_matrix <<- inverse_value
    
    ## The getinverse method fetches the current value of inverse_matrix 
    ## (makeCacheMatrix's scope)
    getinverse <- function() inverse_matrix
    
    ## The makeCahceMatrix returns a list of methodes that can be called.
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

##################
## cacheSolve  ###
##################

## Purpose: This function checks if the cacheMatrix list object is passed 
## as a parameter has a value.  If is doesn't it will calculate the inverse of 
## the cacheMatrix and store the value in the cacheMatrix object.
##
## Function Parameters:
##  * x, a cacheMatrix object (i.e. variable of type list), used to store 
##  the cacheMatrix object that was passed as a parameter. 
##
##  * CacheMatrixInverse, variable to store value of the inverse matrix.
##    (cacheSolve scope)
##
##  Returns:
##  * cacheMatrixInverse
##
##
##   Usage:
##
##   Where testmatrix = matrix(c(1,1,-1,2), nrow=2, ncol=2, byrow=TRUE)
##   and 
##   a <- makeCacheMatrix(testmatrix) to create and initialize the cache matrix "a"
##
##   # No inverse value in the cache
##   --------------------------------
##
##   >cacheSolve(a)  
##   [,1]       [,2]
##   [1,] 0.6666667 -0.3333333
##   [2,] 0.3333333  0.3333333
##  
##   # Inverse value IS in the cache
##   --------------------------------
##    Where a$setinverse(NULL)  - 
##
##   > cacheSolve(a)
##   getting cached data
##   [,1]       [,2]
##   [1,] 0.6666667 -0.3333333
##   [2,] 0.3333333  0.3333333
##
###############################################################################


cacheSolve <- function(x, ...) {
    
    # Calling the getinverse function from cacheMatrix "x" object (CacheMatrix Scope)
    # and storing it in CacheMatrixInverse (cacheSolve scope) 
    CacheMatrixInverse <- x$getinverse()
    
    # If there's already a value stored as the Inverse Matrix in the cacheMatrix,
    # let's notify that we are going to use and return that value.
    
    if(!is.null(CacheMatrixInverse)) {
        message("getting cached data")
        
        return(CacheMatrixInverse)
    }
    
    # Otherwise, we are going to calculate the inverse of the cacheMatrix using 
    # solve function.
    data <- x$get()
    
    CacheMatrixInverse <- solve(data, ...)
    
    # Storing the calculated Inverse back into the cacheMatrix object.
    x$setinverse(CacheMatrixInverse) 
    
    ## The cacheSolve returns the value of the inverse matrix.
    CacheMatrixInverse
}