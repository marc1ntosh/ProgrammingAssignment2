# Programming Assignment 2
# Marc Brehme, 20 March - 22 March 2015

# OVERALL DESCRIPTION
# Creating two functions that create and solve the cached inverse of an invertable matrix, respectively
# FUNCTION 1 = "makeCacheMatrix"
# FUNCTION 2 = "cacheSolve"

# FUNCTION 1
# "makeCacheMatrix" function:
# This function creates a special "matrix" object that can cache its inverse (from the provided readme)

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}

# An examplary invertible square matrix:
example <- makeCacheMatrix(matrix(c(1,2,11,12,21,22,31,32,41), nrow = 3, ncol = 3))


# FUNCTION 2:
# This function calculates the inverse of the special "matrix"
# returned by the `makeCacheMatrix` function above (FUNCTION 1).
# If the inverse has already been calculated (and the matrix has not changed),
# then `cacheSolve` (FUNCTION 2) will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        inverse <- x$getinv()
        if(!is.null(inverse)) {
                message("Fetching cached data...")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data)
        x$setinv(inverse)
        inverse
}

cacheSolve(example)

# The second time R will retrieve the inverse of the matrix from the cache:
cacheSolve(example)
