## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	# The inverse of the matrix is stored        
	inv <- NULL
        
	#set will alter the matrix, thus invalidating the cache
	set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
	#get will return the raw matrix
	get <- function() x
	#setInverse will set the inverse variable and will be used only by cacheSolve
        setInverse <- function(inverse) inv <<- inverse
        #getInverse will get the cached inverse
	getInverse <- function() inv
        #returns the special matrix
	list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        # Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()

        if (!is.null(inv)) {
		# if the inverse is cached, will return it
                message("getting cached data")
                return(inv)
        }

	# if the inverse is not cached, it will calculate it and cache it
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}


## Example

# mat <- makeCacheMatrix(matrix(1:4, 2, 2))
# cacheSolve(mat) ##gets cached inverse
#mat$set(matrix(10:13, 2, 2))
# cacheSolve(mat) ##gets cached inverse
