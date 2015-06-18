# These functions create a container that stores both a square-matrix and
# (where applicable) its inverse.

# makeCacheMatrix creates a container that holds both a square-matrix x,
# and its inverse m. The container also defines four functions get, set,
# get_inverse and set_inverse. Note that x and m are made available to these
# functions via the superassignment operator (<<-). Note that I have added a
# conditional statement that will return an error if the argument x is not a
# square matrix.

makeCacheMatrix <- function(x = matrix())
    
{
    if (nrow(x) != ncol(x))
    {
        return("Matrix is not square.")
    }
    else
    {
        m <- NULL
        set <- function(y)
        {
            x <<- y
            m <<- NULL
        }
        get <- function() x
        set_inverse <- function(inverse) m <<- inverse
        get_inverse <- function() m
        list(set = set, get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)
    }
}


# cacheSolve takes as an argument the container created in makeCacheMatrix, and
# either calculates the inverse if m is NULL (first iteration via set_inverse),
# or retrieves the stored inverse (consecutive iterations via get_inverse) and
# prints the result to the console.

cacheSolve <- function(x, ...)

{
    m <- x$get_inverse()
    if(!is.null(m)) {
        message("Fetching cached data...")
        return(m)
    }
    data <- x$get()
    m    <- solve(data, ...)
    x$set_inverse(m)
    m
}
