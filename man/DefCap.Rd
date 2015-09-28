\name{DefCap}
\alias{DefCap}
\encoding{UTF-8}
\title{Da  capitulo numerico a los códigos CIE-10}
\description{Esta función da a cada defunción su capítulo con valor numérico}
\usage{DefCap(cau)}
\arguments{
  \item{cau}{Vector de códigos de las defunciones}
}
\details{Tener en cuenta no mover su orden, pues se vincula con la base.}
\value{
\item{Capitulos}{Son los capítulos ya en valor numérico}
\item{Errores}{Es el número de defunciones cuyo código no tiene capítulo.}
}
\references{Registros administrativos}
\author{SUAREZ CAMPOS MIGUEL ANGEL}
\note{Esta es solo una prueba}
\seealso{\code{\link{Etapa01}}}
\examples{
##Aquí vemos como usar la función DefCap
Capis<-c("A456","C682","M613","F629","Z729","G365","Y823")
DefCap(Capis)
}
\keyword{Registros}
