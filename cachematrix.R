#The list containing matrix  functions is passed as argument
#if the inverse has already been created, that will be returned, 
#else inverse will be computed stored in the cache & returned
cacheSolve <- function(m_cache, ...) {
        m_inverse <- m_cache$getinverse()
        if(!is.null(m_inverse)) {
                message("getting cached data")
                return(m_inverse)
        }
        data <- m_cache$get()
        m_inverse <- solve(data,...)
        m_cache$setinverse(m_inverse)
        m_inverse
}

#This creates the list containing caching functions on the matrix
makeCacheMatrix <- function(m = matrix()) {
        inv <- NULL
        set <- function(y) {
                m <<- y
                inv <<- NULL
        }
        get <- function() m
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

#code to test the cache
m1=matrix(1:4,2,2)
m1

solve(m1)

cache_m1 = makeCacheMatrix(m1)
cache_m1
cacheSolve(cache_m1)


