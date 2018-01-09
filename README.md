# Sofi

[![Build Status](https://travis-ci.org/loerasg/Sofi.svg?branch=master)](https://travis-ci.org/loerasg/Sofi) [![CRAN version](http://www.r-pkg.org/badges/version/Sofi)](http://cran.rstudio.com/web/packages/Sofi/index.html)

----------------------------------------------------------------------

Aprender con R,
Interfaz (web) interactiva con fines didácticos

Este paquete tiene la finalidad de ayudar a aprender diversos temas académicos de una forma interactiva teniendo ejemplos y la posibilidad de resolver nuevos al mismo tiempo. Apuntes de clase interactivos.

Para mayores detalles visitar la página del paquete ([aquí](http://www.sofi.uno/)).

## ¿Cómo Instalar?
Afortunadamente el paquete se  encuentra en CRAN así que es muy sencillo instalarlo, corre lo siguiente en la consola de R:

```r
install.packages("Sofi")
```

O para estar al día con la última versión subida a GitHub, poner lo siguiente:

```r
if (!require("devtools"))
  install.packages("devtools")
devtools::install_github("loerasg/Sofi")
```

Ya instalado el paquete lo cargamos con library("Sofi"). Por lo pronto la función que mejor representa los fines de este paquete es Estadistica con el parámetro 1, la usamos así:

```r
library("Sofi")
Estadistica(1)
```

Para reportar errores, puedes hacerlo en [GitHub](https://github.com/loerasg/Sofi/issues). Y para sugerencias de cómo llevar este proyecto escribir a loeras@gmail.com.
