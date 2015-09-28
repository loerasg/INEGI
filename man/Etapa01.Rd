\name{Etapa01}
\alias{Etapa01}
\encoding{UTF-8}
\title{
Crea la Muestra
}
\description{
Crea la Muestra, así como también el tamaño de la muestra si no es especificada, para esto como mínimo se tiene que introducir un ID y los códigos de defunción (CIE) los que se clasificaran por sus capítulos.
}
\usage{
Etapa01(IDm,CausaD,Cap,Es,ps,n,Grande)
}
\arguments{
  \item{IDm}{Es un ID que identifica cada código de defunción con la base de datos principal}
  \item{CausaD}{Son los códigos de la causa de defunción en su Clasificación internacional de enfermedades (CIE)}
  \item{Cap}{Solo con fines de orden para que el usuario visualice los errores o tamaños de muestra}
  \item{Es}{Error que se prevé cometer. Por ejemplo, para un error del 10 por ciento, introduciremos en la función el valor 0.1}
  \item{ps}{Vector con la proporción esperada (o antecedentes) en las poblaciones o en los estratos}
  \item{n}{Tamaño de la muestra}
  \item{Grande}{Si  se desea tener la muestra con todos los registros, dar un valor a Grande cualquiera sirve, como Grande=1}
}
\details{
Para más detalles ver los ejemplos que se describirán abajo
}
\value{
\item{Muestra}{Datos con los códigos que entraran en la muestra, marcados con 1 en variable EnMuestra}
}
\references{Registros administrativos}
\author{José D. Loera}
\seealso{\code{\link{DefCap}}}
\examples{
##Aquí vemos como usar la función Etapa01

data(DatPru)
DatCap
InfCap

##Cuando ya tenemos el tamaño de la muestra usamos la función así
Etapa01(IDm=DatCap[,1], CausaD=DatCap[,2], n=InfCap$n)

##Si solo se tiene el vector con la proporción esperada 
##(o antecedentes), los valores de “Es” se predeterminan en 0.03
Etapa01(IDm=DatCap[,1], CausaD=DatCap[,2], ps=InfCap$ps)
}
\keyword{Registros}
