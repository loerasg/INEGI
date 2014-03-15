\name{PonerDefCap}
\alias{PonerDefCap}
\encoding{UTF-8}
\title{Junto los capitulos numericos con la tabla}
\description{A una tabla dada le añade el capitulo numérico, la cual debe contener las columnas CAUSADEF y COD_SEL.}
\usage{PonerDefCap(Clave,CAUSADEF,COD_SEL)}
\arguments{
  \item{Clave}{Lleva el  control de los registros. Para vincular con otros datos. Tabla con la causa de defunción (CAUSADEF) y código final (COD_SEL)}
  \item{CAUSADEF}{Registro con la causa de defunción (CAUSADEF)}
  \item{COD_SEL}{Registro con código final (COD_SEL)}
}
\details{Tener en cuenta no mover su orden, pues se vincula con la base}
\value{
\item{Tabla}{Tabla con columna de capítulo numérico}
}
\references{Registros administrativos}
\author{Daniel}
\note{Esta es solo una prueba}
\seealso{\code{\link{DefCap}}}
\examples{
##Aquí vemos como usar la función PonerDefCap.
data(prueba)
PonerDefCap(Libro1$Clave,Libro1$CAUSADEF,Libro1$CODSEL)
}
\keyword{Registros}