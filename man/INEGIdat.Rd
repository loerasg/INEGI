\name{INEGIdat}
\alias{INEGIdat}
\alias{Accidentes.colision}
\alias{Accidentes.fatales}
\alias{Accidentes.transito}
\alias{Comercio.Automoviles}
\alias{Economia.Acervo}
\alias{Economia.Activos}
\alias{Transportes.Automoviles}
\docType{data}
\encoding{UTF-8}
\title{Datos generales de Estados Unidos Mexicanos}
\description{
  \code{data(INEGIdat)} Algunos temas económicos y de transporte: \code{Accidentes.colision} Accidentes de tránsito terrestres por colisión con objeto fijo en zonas urbanas y suburbanas, \code{Accidentes.fatales} Accidentes de tránsito terrestre fatales, \code{Accidentes.transito} Accidentes de tránsito terrestres en zonas urbanas y suburbanas, \code{Comercio.Automoviles} Automóviles nuevos vendidos al público, \code{Economia.Acervo} Acervo total de activos fijos, \code{Economia.Activos} Activos Fijos. Sector privado y paraestatal (estructura porcentual), \code{Transportes.Automoviles} Automóviles registrados en circulación}
\usage{data(INEGIdat)}
\format{
  \code{Accidentes.colision} Accidentes de tránsito terrestres por colisión con objeto fijo en zonas urbanas y suburbanas:
  \describe{
    \item{\code{Clave.de.la.entidad.federativa}}{Clave de la entidad federativa}
    \item{\code{Entidad.federativa}}{Entidad federativa}
    \item{\code{Tema}}{Seguridad vial}
    \item{\code{Id_Indicador}}{1006000042}
    \item{\code{Indicador}}{Accidentes de tránsito terrestres por colisión con objeto fijo en zonas urbanas y suburbanas}
    \item{\code{X1997}}{Año de observación 1997, etc.}
  }
  Fecha de consulta: 02/09/2013 12:40:19
  
  \code{Accidentes.fatales} Accidentes de tránsito terrestre fatales:
  \describe{
    \item{\code{Clave.de.la.entidad.federativa}}{Clave de la entidad federativa}
    \item{\code{Entidad.federativa}}{Entidad federativa}
    \item{\code{Tema}}{Seguridad vial}
    \item{\code{Id_Indicador}}{3101006001}
    \item{\code{Indicador}}{Accidentes de tránsito terrestre fatales}
    \item{\code{X1997}}{Año de observación 1997, etc.}
  }
  Fecha de consulta: 02/09/2013 10:17:01
  
  \code{Accidentes.transito} Accidentes de tránsito terrestres en zonas urbanas y suburbanas:
  \describe{
    \item{\code{Clave.de.la.entidad.federativa}}{Clave de la entidad federativa}
    \item{\code{Entidad.federativa}}{Entidad federativa}
    \item{\code{Tema}}{Seguridad vial}
    \item{\code{Id_Indicador}}{1006000039}
    \item{\code{Indicador}}{Accidentes de tránsito terrestres en zonas urbanas y suburbanas}
    \item{\code{X1997}}{Año de observación 1997, etc.}
  }
  Fecha de consulta: 02/09/2013 10:21:57

  \code{Comercio.Automoviles} Automóviles nuevos vendidos al público:
  \describe{
    \item{\code{Clave.de.la.entidad.federativa}}{Clave de la entidad federativa}
    \item{\code{Entidad.federativa}}{Entidad federativa}
    \item{\code{Tema}}{Comercio}
    \item{\code{Id_Indicador}}{1011000038}
    \item{\code{Indicador}}{Automóviles nuevos vendidos al público}
    \item{\code{X1994}}{Año de observación 1994, etc.}
  }
  Fecha de consulta: 02/09/2013 15:28:10
  
  \code{Economia.Acervo} Acervo total de activos fijos:
  \describe{
    \item{\code{Clave.de.la.entidad.federativa}}{Clave de la entidad federativa}
    \item{\code{Entidad.federativa}}{Entidad federativa}
    \item{\code{Tema}}{Economía}
    \item{\code{Id_Indicador}}{5300000041}
    \item{\code{Indicador}}{Acervo total de activos fijos}
    \item{\code{X1998}}{Año de observación 1998, etc.}
  }
  Fecha de consulta: 02/09/2013 14:05:14
  
  \code{Economia.Activos} Activos Fijos. Sector privado y paraestatal (estructura porcentual):
  \describe{
    \item{\code{Clave.de.la.entidad.federativa}}{Clave de la entidad federativa}
    \item{\code{Entidad.federativa}}{Entidad federativa}
    \item{\code{Tema}}{Economía}
    \item{\code{Id_Indicador}}{5000000022}
    \item{\code{Indicador}}{Activos Fijos. Sector privado y paraestatal (estructura porcentual)}
    \item{\code{X1998}}{Año de observación 1998, etc.}
  }
  Fecha de consulta: 02/09/2013 14:41:57
  
  \code{Transportes.Automoviles} Automóviles registrados en circulación:
  \describe{
    \item{\code{Clave.de.la.entidad.federativa}}{Clave de la entidad federativa}
    \item{\code{Entidad.federativa}}{Entidad federativa}
    \item{\code{Tema}}{Transportes, correos y almacenamiento}
    \item{\code{Id_Indicador}}{1011000057}
    \item{\code{Indicador}}{Automóviles registrados en circulación}
    \item{\code{Unidad.de.medida}}{Automoviles}
    \item{\code{X1980}}{Año de observación 1980, etc.}
  }
  Fecha de consulta: 04/09/2013 15:57:02
}
\source{INEGI}
\note{Datos un poco generales de Estados Unidos Mexicanos, la finalidad de estos es tener ejemplos de México,  para realizar ejercicios estadísticos, así  para cualquier sugerencia, comentario o ejemplo estadístico que desee que se agregue comunicarse a <jose.loera@inegi.gob.mx>}
\references{
  Instituto Nacional de Estadística y Geografía 
  Banco de Información INEGI  
  Fecha de consulta: 04/09/2013  
  http://www3.inegi.org.mx/sistemas/biinegi/default.aspx 
}
\examples{
data(INEGIdat)

}
\keyword{datasets}