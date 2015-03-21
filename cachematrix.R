## The next two functions calculate the inverse of a matrix and saves it
## to the cache.The next time the user attempts to calculate the
## matrix inverse, the previously saved value is returned instead 
## of calculating it again.


## The first function creates a special "matrix" object, which is really a list 
## containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) { ## create a matrix object x 
        ## and some associated sub-functions
        
        m <- NULL ## define the cache m
        set <- function(y) {
                x <<- y ## assign the input matrix y to the variable x in the
                ## parent environment
                m <<- NULL ## re-initialize m in the parent environment to null
        }
        get <- function() x ## return the matrix x
        setinverse <- function(inverse) m <<- inverse ## set the cache m equal
        ## to the inverse of the matrix x
        getinverse <- function() m ## return the cached inverse of x
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## The second function calculates the inverse of the special "matrix" created
## with the above function. First it checks to see if the inverse
## has already been caclulated. In this case it gets the inverse from the cache
## and skips the computation. Otherwise, it calculates the matrix inverse
## and sets the value of the inverse in the cache with the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of x
        
        m <- x$getinverse()
        if(!is.null(m)) { ## checks to see if the inverse
                ## has already been calculated 
                message("getting cached data") ##  notifying by message
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...) ## calculates the matrix inverse
        ## and sets the value of the inverse in the cache
        x$setinverse(m)
        m
}