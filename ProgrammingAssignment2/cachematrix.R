## These functions create an object returned by makeCacheMatrix 
## which "knows" how to get and set the inverse of a matrix
## For now assuming that the matrix is invertible 


makeCacheMatrix <- function(x = matrix()) {
        my_inverse <- NULL
        set <- function(y) {
                x <<- y
                my_inverse <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse_matrix) my_inverse <<- inverse_matrix
        getInverse <- function() my_inverse
        list(set = set, 
        	get = get,
	     	setInverse = setInverse,
     		getInverse = getInverse)
}


## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        my_inverse <- x$getInverse()
        if(!is.null(my_inverse)) {
                message("getting cached inverse for your matrix")
                return(my_inverse)
        }
        ## else it will compute the inverse and store it	
        data <- x$get()
        my_inverse <- solve(data, ...)
        x$setInverse(my_inverse)
        my_inverse
}



