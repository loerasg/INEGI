\name{INEGI-package}
\alias{INEGI-package}
\alias{INEGI}
\encoding{UTF-8}
\docType{package}
\title{Soporte para registros administrativos}
\description{
Este paquete tiene la finalidad de ayudar a la dirección de registros administrativos, con varios procesos, con la finalidad de llevar las estadísticas de los registros.
}
\details{
\tabular{ll}{
Package: \tab INEGI\cr
Type: \tab Package\cr
Version: \tab 0.3\cr
Date: \tab 2014-02-26\cr
License: \tab GPL (>= 3)\cr
}
Aquí se usan pocas funciones como es DefCap y PonerDefCap, otras en desarrollo.
}
\author{
Daniel Loera <jose.loera@inegi.org.mx>
}
\references{Datos de registros administrativos}
\keyword{package}
\keyword{Registros}
\keyword{Procesos}
\keyword{Estadisticas}
\examples{
##Aquí vemos como usar la función DefCap, ejemplo general.
Capis<-c("P456","C682","M613","F629","X392")
DefCap(Capis)
data(prueba)
PonerDefCap(Libro1$Clave,Libro1$CAUSADEF,Libro1$CODSEL)
}
