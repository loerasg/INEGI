\name{prueba}
\alias{prueba}
\alias{Libro1}
\alias{Libros}
\docType{data}
\encoding{UTF-8}
\title{Datos de ejemplo}
\description{
  \code{data(prueba)} Ejemplos de Códigos CIE-10: \code{Libro1} ejemplo con 13 obs., \code{Libros} ejemplo con 10 obs.}
\usage{data(prueba)}
\format{
  El \code{Libro1} Ejemplos de Códigos CIE-10:
  \describe{
    \item{\code{Clave}}{Clave de los registros}
    \item{\code{CAUSADEF}}{código automático}
    \item{\code{CODSEL}}{Código revisado}
  }
  El \code{Libros} data frame contains the following columns:
  \describe{
    \item{\code{Claves}}{Clave de los registros}
    \item{\code{CAUSADEFs}}{código automático}
    \item{\code{CODSELs}}{Código revisado}
  }
}
\source{
  Registros administrativos
}
\references{
  http://es.wikipedia.org/wiki/CIE-10
}
\examples{
data(prueba)
## maybe str(prueba) ; plot(prueba) ...
}
\keyword{datasets}