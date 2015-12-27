## These functions save time during lenghty computations by looking for 
##cached computations

## This creates a matrix object to cache the inverse

makeCacheMatrix <- function(x = matrix()) {
        ## return: a list containing functions to
        ##               set  and get the matrix
        ##              set and get the inverse
        
        inv = NULL
        set = function(y) {
                # use `<<-` to assign a value to an object in an environment 
                x <<- y
                inv <<- NULL
        }
        get = function() x
        setinv = function(inverse) inv <<- inverse 
        getinv = function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}





## This computes the inverse of the special matrix and if already created
## retrieves the inverse of the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## @x: output makeCacheMatrix
        ## return: inverse of the original matrix 
        
        inv = x$getinv()
        
        # if the inverse has already been calculated
        if (!is.null(inv)){
                # gets from the cache and skips computation. 
                message("getting cached data")
                return(inv)
        }
        
        # if not cached, calculate the inverse 
        mat.data = x$get()
        inv = solve(mat.data, ...)
        
        x$setinv(inv)
        
        return(inv)
        
}
