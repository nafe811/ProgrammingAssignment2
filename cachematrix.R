## two functions that use to cache the inverse matrix

## function1: cache its matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        #set matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        #get matrix
        get <- function() x
        #set matrix inverse
        set_inver <- function(inver) m <<- inver
        #get matrix inverse
        get_inver <- function() m
        #return list
        list(set = set, get = get,
             set_inver = set_inver,
             get_inver = get_inver)

}


## function2: use 'makeCacheMatrix' function  to compute inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$get_inver()
        #get alread cached inverse
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        #get matrix
        data <- x$get()
        #compute inverse
        m <- solve(data)
        #set inverse
        x$set_inver(m)
        m
}
