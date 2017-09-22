## The two functions below allow to create an object (a cached matrix) and to
## calculate its inverse. 

## makeCacheMatrix is a closure function that creates the following object: 
## an empty matrix with 4 functions attached to it: set content of matrix, 
## get content of matrix, set content of inverse matrix, get content of 
## inverse matrix. Note that the "set" child function resets the value of the 
## inverse matrix when called: this will force the cacheSolve function to
## recalculate the inverse matrix whenever the original one is changed.

makeCacheMatrix <- function(x = matrix()) { #Constructor makes empty matrix.
        inv <- NULL 
        set <- function(y) { 
                x <<- y 
                inv <<- NULL # Resets the inverse when the matrix is changed.
        }
        get <- function() x 
        set_inv <- function(inverse) inv <<- inverse 
        get_inv <- function() inv
        list(set = set, get = get,
             set_inv = set_inv,
             get_inv = get_inv)
}


## cacheSolve takes an object made using makeCacheMatrix (i.e. a matrix with 
## 4 functions attached) and calculates the inverse of the matrix. If the
## inverse has already been calculated, the function retrieves the cached value
## rather than recalculating it.

cacheSolve <- function(x,...) {     
        inv <- x$get_inv()
        
        if(!is.null(inv)) { # Checks if there is a cached inverse matrix.
                # if there is a cached inverse, it returns it.
                message("getting cached data")
                return(inv)
        }
        else { # If there is no cached inverse matrix, it calculates it.
                data <- x$get()
                inv <- solve(data,...)
                x$set_inv(inv)
                inv
        }
        
}
