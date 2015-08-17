##Assignment: Caching the Inverse of a Matrix

# Matrix inversion is usually a costly computation and their may be some benefit 
# to caching the inverse of a matrix rather than compute it repeatedly (there are
# also alternatives to matrix inversion that we will not discuss here). 
# Your assignment is to write a pair of functions that cache the inverse of a matrix.

# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

# makeCacheMatrix: return a list of functions to:
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of the inverse
# 4. Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
# inv will store the cached inverse matrix, set to NULL
        inv <- NULL
# Matrix setter
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
# Matrix getter
        get <- function() x
# inv setter
        setinverse <- function(inverse) inv <<- inverse
# inv getter        
        getinverse <- function() inv
# return the matrix
        list(set=set, get=get, 
             setinverse=setinverse, 
             getinverse=getinverse)
}

# cacheSolve: This function computes the inverse of the special "matrix" returned by 
#             makeCacheMatrix above. If the inverse has already been calculated 
#             and the matrix has not changed), then the cachesolve should retrieve 
#             the inverse from the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
# if inv is calculated, return it       
         if(!is.null(inv)) {
                message("getting cached data.")
                return(inv)
        }
# if inv is not jet calculated, calculate it       
         dat <- x$get()
        inv <- solve(dat,...)
# inv cache       
         x$setinverse(inv)
# inv return
        inv
}