
Defunciones <- function(Nivel){
  if (Nivel==1) {shinyAppDir(system.file("INEGI/Defun", package="INEGI"))}
  else if (Nivel==2) {shinyAppDir(system.file("INEGI/Pack_Experto", package="INEGI"))}
}

#.FdeDatos<-function(){tabPanel("Datos",
#                              h4("Tabla de c\u00f3digos"),
#                              dataTableOutput('tabla1'),
#                              tags$hr(),
#                              h4("Tabla de Datos"),
#                              tableOutput('tabla2')
#)}
