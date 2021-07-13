## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        a <- NULL
        set <- function(y) {
        x <<- y
        a <<- NULL
        }
        get <- function() x
        setInv <- function(Inv)
                a <<- Inv
        getInv <- function() a
        list(set = set, get = get, setInv = setInv,
             getInv = getInv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        a = x$getInv()
        if(!is.null(a)) {
                message("Cashed data is being fetched!")
                return(a)
        }
        ## Return a matrix that is the inverse of 'x'
        data = x$get()
        a = Inv(data, ...)
        x$setInv(a)
        a
}
