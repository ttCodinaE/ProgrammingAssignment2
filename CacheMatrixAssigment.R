#PROGRAMMING ASSIGNMENT 2, Week 3: caching the inverse of a Matrix

#The assignment consists of caching the inverse of a Matrix
#Write an R function able to cache potentially time-consuming computations.
#2 Functions are required:
#   1. One that creates a "special" matrix object that can cache its inverse
#   2. Another one that computes the inverse of the special "matrix"

# The first function returns a list consisting of:
# 1. Setting the matrix
# 2. Getting the matrix
# 3. Setting the inverse matrix
# 4. Getting the inverse matrix

makeCacheMatrix <- function(z = matrix()){     #call this function with object "z"
  inv <- NULL            # initialize object. Set to NULL to be used later in the function environment
  set <- function(y) {   # define the data retrievers - getters - and setters
    z <<- y              # <<- operator assigns z to an object in the parent environment
    inv <<- NULL         # "set" assigns the input argument to z in the parent environment
  }                      # and the value of NULL to "inv" in the parent env. and dependant on the value of z
  get <- function() z
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list ( set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)   #getters and setters defined and named
  
}

# The function above has created a list.
# In order to access the functions within, the argument (z) and
# the variable "inv" (set to NULL) we need to create a
# an object environment. 

mCC.object <- makeCacheMatrix(z)
mCC.object

# The above will work, but to get the most out of lexical scoping
# we can activate the "set" function we created, and run it easily for a new matrix

mCC.object$set(z)   # where z is substituted by the name of the new matrix to inverse


# The second function will execute the actual computation

cacheSolve <- function(z, ...) {
  inv <- z$getinverse()
  if (!is.null(inv)) {
    message ("computing cache matrix")
    return(inv)
  }
  matrix.data = z$get()
  inv <- solve(matrix.data,...)
  z$setinverse(inv)
  return (inv)
}

# To see the result, call this last function to the object created above

cacheSolve(mCC.object)

# I will now test the functions above on simple 2x2 matrices named "m" and "m1"

y <- c(10, 25, 200, 3)
m <- matrix (y, nrow=2, ncol=2)
m

m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow=2, ncol=2)
m1

makeCacheMatrix(m)                 # this is the longer version
mCC.object <- makeCacheMatrix(m)   # having to define makeCacheMatrix & object each time
cacheSolve(mCC.object)

mCC.object$set(m)         # more efficient, using the "set" function created applaying to a new matrix
cacheSolve(mCC.object)      # this argument does not need changing

mCC.object$set(m1)       # example with the m1 matrix just 2 steps are required
cacheSolve(mCC.object)    

# One last check. I create matrix n1, which is m1's inverse
# running the cache functions should result in matrix m1. Let's try

n1 <- matrix(c(6,2,8,4), nrow=2, ncol=2)
n1

mCC.object$set(n1)
cacheSolve(mCC.object)

# It worked! I invite you to try it yourself - thanks for your feedback!
