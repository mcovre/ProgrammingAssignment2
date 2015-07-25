# The purpose of the following two functions is to inverse a matrix and cache the inverse matrix, 
# so inversing the matrix is only required if the matrix changes (new matrix) or the inverse matrix 
# has never been cached

# Function 'makeCacheMatrix' does the following:
# 1. accepts a square invertible matrix (n X n) as the input
# 2. caches the input matrix 
# 3. caches and initialises (NULL) variable ms, which will store the inverse matrix
# 4. creates function 'set' to cache the input matrix
# 5. creates function 'get' to retrieve the cached matrix x
# 6. creates function 'setmsolve' to cache the inverse matrix
# 7. creates function 'getmsolve' to retrieve the cached inverse matrix
# 8. returns a List object containing the four functions

makeCacheMatrix <- function(x = matrix()) {
        
        ms <- NULL
        set <- function(y) {
                x <<- y
                ms <<- NULL
        }
        get <- function() x
        setmsolve <- function(msolve) ms <<- msolve
        getmsolve <- function() ms
        list(set = set, get = get,
             setmsolve = setmsolve,
             getmsolve = getmsolve)        
}


# Function 'cacheSolve' does the following:
# 1. accepts the list of functions created in 'makeCacheMatrix' as input
# 2. retrieves the cached inverse matrix 
# 3. retrieves the matrix to be inversed
# 4. if there is a cached inverse matrix and the matrix to be inversed has not changed, returns the inverse matrix
# 5. if there is no cached inverse matrix or the matrix to be inversed has changed, generates the new inverse matrix
# 6. caches the new inverse matrix
# 7. returns the inverse matrix 


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ms <- x$getmsolve()
        mx <- x$get()
        if(!is.null(ms) && ms %*% mx==diag(nrow(mx))) {
                message("getting cached data")
                return(ms)
        }
        ms <- solve(mx, ...)
        x$setmsolve(ms)
        ms
        
}
