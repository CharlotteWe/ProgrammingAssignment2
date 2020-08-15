#ProgrammingAssignment2/Alcancelexico.md

## makeCacheMatrix:
## Esta función crea un objeto especial de "matriz" que puede almacenar en caché su inverso.

makeCacheMatrix <- function (x = matrix ()) {
m <- NULL
set <- function (y) {
x << - y
m << - NULL
}
get <- function () x
setresolver <- function (solve) m << - resolver
getresolver <- function () m
list (set = set,
get = get,
setresolver = setresolver,
getresolver = getresolver)
}

## cacheSolve:
## Esta función calcula la inversa de la “matriz” especial devuelta por makeCacheMatrix arriba.
## Si la inversa ya se ha calculado (y la matriz no ha cambiado),
## entonces cacheSolve debería recuperar la inversa de la caché.
cacheSolve <- function (x, ...) {
m <- x $ getresolver ()
if (! is.null (m)) {
message ("obtener datos en caché")
return (m)
}
data <- x $ get ()
m <- solve (datos, ...)
x $ setresolver (m)
m
}
