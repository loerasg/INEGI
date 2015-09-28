\name{DatPru}
\alias{DatPru}
\alias{DatCap}
\alias{InfCap}
\docType{data}
\encoding{UTF-8}
\title{Datos de ejemplo}
\description{
  \code{data(DatPru)} Ejemplos de Códigos CIE-10 e información para generar muestras: \code{DatCap} ejemplo de Códigos CIE-10 con 167 obs., \code{InfCap} ejemplo de información para la muestra (Cap,Es,ps,n)}
\usage{data(DatPru)}
\format{
  El \code{DatCap} Ejemplos de Códigos CIE-10:
  \describe{
    \item{\code{ID}}{Identificador de el código}
    \item{\code{CausaD}}{código automático CIE-10}
  }
  El \code{InfCap} información para la muestra (Cap,Es,ps,n):
  \describe{
    \item{\code{Cap}}{Solo con fines de orden para que el usuario visualice los errores o tamaños de muestra}
    \item{\code{Es}}{Error que se prevé cometer. Por ejemplo, para un error del 10 por ciento, introduciremos en la función el valor 0.1}
    \item{\code{ps}}{Vector con la proporción esperada (o antecedentes) en las poblaciones o en los estratos}
    \item{\code{n}}{Tamaño de la muestra}
  }
}
\source{
  Registros administrativos
}
\references{
  http://es.wikipedia.org/wiki/CIE-10
}
\examples{
data(DatPru)
}
\keyword{datasets}