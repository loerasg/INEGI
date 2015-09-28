\name{Defunciones}
\alias{Defunciones}
\encoding{UTF-8}
\title{
Evaluación de los códigos de registros de defunción
}
\description{
Interfaz web para facilitar la evaluación de los códigos de defunción facilitando así el orden de los procesos.
}
\usage{
Defunciones(Nivel)
}
\arguments{
  \item{Nivel}{
  Para los parámetros
  \tabular{ll}{
1 = \tab Menú para las etapas de evaluación de los códigos de defunción\cr
2 = \tab Solo la etapa para el codificador experto.\cr
}
}
}
\details{
Mas detalles acerca de su uso se encuentran dentro de la interfaz web, ver en la pestaña de ayuda.
}
\value{
Las salidas están definidas por cada etapa.
}
\references{
Registros administrativos
}
\author{
José D. Loera
}
\examples{
# Función para evaluar códigos de  defunción.
if (interactive()){
  Defunciones(1)
}

# Función para evaluar códigos de  defunción por el experto.
if (interactive()){
  Defunciones(2)
}
}
\keyword{ Registros }
