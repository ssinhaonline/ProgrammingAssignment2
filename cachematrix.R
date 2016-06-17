## Put comments here that give an overall description of what your
## functions do

# makeCacheMatrix() defines required functions
# to construct the cache.

# cacheSolve() solves the matrix arguement,
# only if it is not present in the cache

## Write a short comment describing this function
# Makes a list of functions described below:

makeCacheMatrix <- function(x = matrix()) {
    # Returns a list of functions that:
    #   1. sets the value of the matrix
    #   2. gets the value of the matrix
    #   3. sets the value of the inverse
    #   4. sets the value of the inverse
  
    my_inv <- NULL
  
    set_matrix <- function(mat)
    {
        x <<- mat
        my_inv <<- NULL
    }
  
    get_matrix <- function()
    {
        x
    }
  
    set_inverse <- function(inverse)
    {
        my_inv <<- inverse
    }
    
    get_inverse <- function()
    {
        my_inv
    }
    
    list(set_matrix = set_matrix,
         get_matrix = get_matrix, 
         set_inverse = set_inverse, 
         get_inverse = get_inverse)
  
}


## Write a short comment describing this function
# Returns the inverse of the matrix x.
# 'Solves' x only if it is not present in the cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    my_inv <- x$get_inverse()
    
    if(!is.null(my_inv))
    {
        message('Updating inverse from cache..')
        return(my_inv)
    }
    
    data_matrix <- x$get_matrix()
    my_inv <- solve(data_matrix, ...)
    x$set_inverse(my_inv)
    my_inv
}
