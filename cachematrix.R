## Both functions "makeCacheMatrix" & "cacheSolve" work together in order to create 
## a caching mechanism for matrices. "makeCacheMatrix" creates a special list with 
## functionality to access the matrix's data (and its' inverse) and "cacheSolve" manages
## the caching / calculating procedure. (Please refer to the individual function's comments
## for more information).

## Creates a "special" matrix-like object (technically, it's a list).
## This list contains the functionality to set or get a matrix as well as
## to set or get the matrix's inverse (and that acts as the cache).

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve: Calculates the inverse of the "special" matrix
## (checks if already cached - if so, gets the cached version, otherwise, calculated and sets).

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
	
	  print("data not cached, calculated on the fly")
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}
