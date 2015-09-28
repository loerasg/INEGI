\name{INEGI-package}
\alias{INEGI-package}
\alias{INEGI}
\encoding{UTF-8}
\docType{package}
\title{Soporte para registros administrativos}
\description{
Este paquete tiene la finalidad de ayudar a la dirección de registros administrativos, con varios procesos, con el fin de llevar las estadísticas de los registros administrativos
}
\details{
\tabular{ll}{
Package: \tab INEGI\cr
Type: \tab Package\cr
Version: \tab 0.0.37\cr
Date: \tab 2015-09-25\cr
License: \tab GPL (>= 3)\cr
}
Aquí se usan pocas funciones como es DefCap y Etapa01, otras en desarrollo.
}
\author{
José D. Loera <jose.loera@inegi.org.mx>
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

data(DatPru)
DatCap
InfCap

##Cuando ya tenemos el tamaño de la muestra usamos la función así.
Etapa01(IDm=DatCap[,1], CausaD=DatCap[,2], n=InfCap$n)

# Función para evaluar códigos de  defunción.
if (interactive()) {
  Defunciones(1)
}

# Función para evaluar códigos de  defunción por el experto.
if (interactive()) {
  Defunciones(2)
}


}
