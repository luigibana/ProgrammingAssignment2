## makeCacheMatrix allows to save and retrieve a matrix and its inverse 
## cacheSolve allows to solve for the inverse of a matrix exploiting the cache

## makeChaceMatrix: generates a list with four actions on matrix x: 
##    set, get the matrix, set_inverse and get_inverse
makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      set_inverse <- function(inve) inv <<- inve
      get_inverse <- function() inv
      list(set = set, get = get, 
           set_inverse = set_inverse, get_inverse = get_inverse)
} 


## cacheSolve retrieves the inverse of the matrix x from the cache if possible
## otherwise it retrieves x itself and computes its inverse via the function solve
## and saves it to the cache
cacheSolve <- function(x, ...) {
       ## Return a matrix that is the inverse of 'x'
      x_inv <- x$get_inverse()
      if(!is.null(x_inv)){
            message("getting cached data")
            return(x_inv)
      }
      mat <- x$get()
      inv_compute <- solve(mat);
      x$set_inverse(inv_compute);
      inv_compute
}
