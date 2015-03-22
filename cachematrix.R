#The list containing matrix  functions is passed as argument
#if the inverse has already been created, that will be returned, 
#else inverse will be computed stored in the cache & returned
cacheSolve <- function(m_cache, ...) {
        #Get the inverse stored in the cache
        m_inverse <- m_cache$getinverse()

        #If there is an inverse stored in the cache then return the cached value
        if(!is.null(m_inverse)) {
                message("getting cached data")
                return(m_inverse)
        }

        #If no inverse in cache, then get the matrix, compute its inverse, 
        #store in the cache and return the value
        data <- m_cache$get()
        m_inverse <- solve(data,...)
        m_cache$setinverse(m_inverse)
        m_inverse
}

#This creates the list containing caching functions on the matrix
makeCacheMatrix <- function(m = matrix()) {
        inv <- NULL
        
        #Set function to initialize the entire operation for a matrix
        #Sets the matrix to a specified matrix and its inverse to null
        set <- function(y) {
                m <<- y
                inv <<- NULL
        }

        #Get function to return the matrix
        get <- function() m

        #SetInverse function to set the inverse to a particular value
        setinverse <- function(inverse) inv <<- inverse

        #GetInverse function to return the inverse value stored in the object
        getinverse <- function() inv

        #Returns list of all the special functions
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


