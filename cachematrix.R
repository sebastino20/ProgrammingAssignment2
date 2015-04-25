##There are two functions that are used to create a special
#object that stores a numeric vector and cache's its mean.

##The first function, makeVector creates a special "vector", 
#which is really a list containing a function to
#set the value of the vector
#get the value of the vector
#set the value of the mean
#get the value of the mean

##Matrix inversion is usually a costly computation and there may be some benefit
#to caching the inverse of a matrix rather than compute it repeatedly.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y)
        {
            x <<- y
            m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
    
    }
    #The following function cacheSolve calculates the inverse of the special "matrix" 
    #created with the above function it first checks to see if the inverse of the matrix 
    #has already been calculated. If so, it gets the inverse from the cache
    #and skips the computation. Otherwise, it calculates the inverse of the data 
    #and sets the inverse matrix in the cache via the setinverse function.
    
    cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
            message("getting cached data")
            return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
    }
x = rbind(c(3, 4), c(4, 3))
m = makeCacheMatrix(x)
m$get()
cacheSolve(m)

