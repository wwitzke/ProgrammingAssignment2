## The functions in this file implement matrix objects that cache their
## solutions.

## This function creates a matrix object that can cache its inverse.
makeCacheMatrix <- function(x = matrix())
{
    s <- NULL;
    set <- function(y)
    {
	x <<- y;
	s <<- NULL;
    }
    get <- function() x;
    setsolved <- function(solved) s <<- solved;
    getsolved <- function() s;
    list (  set = set, get = get,
	    setsolved = setsolved,
	    getsolved = getsolved
    );
}

## This function can solve a cached matrix object or, if the matrix is already
## solved, return the cached solution.
cacheSolve <- function(x, ...)
{
    ## Return a matrix that is the inverse of 'x'
    s <- x$getsolved();
    if ( !is.null(s) )
    {
	message( "getting cached inverse" );
	return(s);
    }
    data <- x$get();
    s <- solve(data, ...);
    x$setsolved(s);
    s;
}
