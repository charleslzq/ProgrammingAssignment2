## These two function will calculate the inverse of a matrix.

## This function will create a list of functions that can be used to cache calculated inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL;
	set <- function(y) {
		x <<- y
		m <<- NULL
	};
	get <- function() x;
	setinverse <- function(inverse) m <<- inverse;
	getinverse <- function() m;
	list(set = set, get = get,
		 setinverse = setinverse,
		 getinverse = getinverse);
}


## This function will calculate the inverse of a matrix. If the inverse of this matrix has been calculated before and thus is cached, then the result will be returned immediately.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		m <- x$getinverse();
		## If the result is already there, just return it.
		if(!is.null(m)) {
			message("getting cached data");
			return(m);
		}
		data <- x$get();
		m <- solve(data, ...);
		x$setinverse(m);
		m;
}
