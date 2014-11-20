## This function creates a list containing a functions to
## 1. set matrix. When this happens inverse matrix set to NULL
## because previous value was for previous matrix
## 2. get matrix
## 3. set inverse matrix
## 4. get inverse matrix

makeCacheMatrix <- function(x = matrix()) {
       inverse_matrix <- NULL
       set <- function(y) {
              x <<- y
              m <<- NULL
       }
       get <- function() x
       set_inverse <- function(inv) inverse_matrix <<- inv
       get_inverse <- function() inverse_matrix
       list(set = set,
            get = get,
            set_inverse = set_inverse,
            get_inverse = get_inverse)
}


## This function calculates the inverse matrix.
## If there is cached value function reports
## about it by message and return this cached
## value.
## Otherwise, function calculate inverse matrix for x
## (by using solve() function), set it as cached
## inverse for x and return inverse matrix.

cacheSolve <- function(x, ...) {
        inverse_matrix <- x$get_inverse()
        if (!is.null(inverse_matrix)) {
               message("getting cached data")
               return(inverse_matrix)
        }
        data <- x$get()
        inverse_matrix <- solve(data, ...)
        x$set_inverse(inverse_matrix)
        inverse_matrix
}