## makeCacheMatrix is a function that acts as a constructor for a cached matrix from a given matrix
makeCacheMatrix <- function(x = matrix()) {
        invVal <- NULL

        ## (Setter) Function that sets the value of x and m that affects the parent
        set <- function(y) {
                x <<- y
                invVal <<- NULL
        }

        ## (Getter) Function that gets the value of x
        get <- function() {
                x
        }

        ## (Setter) Function that sets invVal to the computed inverted data
        setInverse <- function(inverse) {
                invVal <<- mean
        }

        ## (Getter) Function that returns the inverted data
        getInverse <- function() {
                invVal
        }

        ## Defines the functions available in makeCacheMatrix
        list(
                set = set, 
                get = get,
                setInverse = setInverse,
                getInverse = getInverse
        )
}

## cacheSolve is a function that computes and returns the cached and inverted input data
cacheSolve <- function(x, ...) {
        
        ## Sets the value of val from the getter getInverse function from makeCacheMatrix
        val <- x$getInverse()

        ## Checks if the cached data is alread inverted. Returns the data from the cache
        if(!is.null(val)) {
                message("getting cached data")
                return(val)
        }

        ## If data is not yet cached, logic below will execute to obtain and computed inverted data and cache it
        data <- x$get()
        val <- solve(data, ...)
        x$setInverse(val)
        val
}
