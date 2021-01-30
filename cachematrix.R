#makeCacheMatrix creates a set of methods which cache matrix (x) and the inverse of that matrix (i) and enable other programs to call them so that
#cache data gets updated
#cacheSolve looks for the cache data calling makeCacheMatrix method getinverse() to see if has already been calculated, and if it has not it will calculate it, store it in cache ny calling method setinverse() and output it to console

## Write a short comment describing this function
#This function is enables both matrix and its inverse to be stored in cache for later use, with the help of four different methods: one to change values to matrix (set), another to get the value of the matrix(get), and the other two to get or set the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Write a short comment describing this function
#THis function calls the makecachematrix by inputing the cached inverse matrix, and if there is not one, it will calculate it, store it in cache and output it to console
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}

init <- makeCacheMatrix(matrix( c(5, 1, 0,
                                  3,-1, 2,
                                  4, 0,-1), nrow=3, byrow=TRUE))

cacheSolve(init)

init$getinverse()

cacheSolve(init)
