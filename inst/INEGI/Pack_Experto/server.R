
pkg <- c("foreign","shinythemes")#, "ggplot2", "sampling","shinyjs"
new.pkg <- pkg[!(pkg %in% installed.packages())]
if (length(new.pkg)) {
  install.packages(new.pkg)
}

library(shiny)
#library(shinyjs)
#library(foreign)
#library(ggplot2)
#library(sampling)
#library(Sofi)

source("Func.R")
#load("Historia.RData")
#load("Datos.RData")
#E3Dat<-readRDS("Dat_Def.rds")
E3Dat<-readRDS("Rev2.rds")
#E3Dat<-readRDS("Rev10.rds")
#E3Dat<-E3Dat[E3Dat[,"REV"]==1,]
E3Codigos<-readRDS("Codigos2.rds")

#Dat_Def<-as.data.frame(Dat_Def,stringsAsFactors = FALSE)
Tamaño<-dim(E3Dat)


options(shiny.maxRequestSize=1300*1024^2,shiny.deprecation.messages=FALSE)#,shiny.reactlog=TRUE
#options(shiny.reactlog=TRUE)
#options(shiny.deprecation.messages=FALSE)

shinyServer(function(input, output, session) {
  #Etapa 
  ####
  #_______________________________________________________________
  #_______________________________________________________________
  #Etapa 3
  #_______________________________________________________________
  #_______________________________________________________________
  ####
  values <- reactiveValues()
  
  observe({
    #E3Dat<-CargarDatos()
    #E3Dat[,"Cap_Tex"]<-as.character(E3Dat[,"Cap_Tex"])
    values$d<-E3Dat[,"COD_SEL"]
    values$e<-E3Dat[,"Cap_Tex"]#E3Dat[,"Cap_Tex"]
    values$Num_Reg<-1
    #E3_tem<-E3Dat[,"COD_SEL"]
    #names(E3_tem)<-1:Tamaño[1]
    #Val_Ini<<-as.integer(names(E3_tem[is.na(E3_tem)])[1])
  })
 
  observeEvent(input$E3Bot_Buscar, {
    E3_tem<-values$d
    names(E3_tem)<-1:Tamaño[1]
    Val_Bus<-as.integer(names(E3_tem[is.na(E3_tem)])[1])
    if(is.na(Val_Bus)){
      values$Num_Reg<-1
      updateTextInput(session, "Et3_Reg_Tex", value = 1)
    } else {
      values$Num_Reg<-Val_Bus
      updateTextInput(session, "Et3_Reg_Tex", value = Val_Bus)
    }
    #updateNumericInput(session, "values$Num_Reg", value = Val_Bus)
    
  })
 
  observeEvent(input$E3_Buscar_Reg, {
    if(is.na(as.integer(input$Et3_Reg_Tex))){
      values$Num_Reg<-1
      updateTextInput(session, "Et3_Reg_Tex", value = 1)
    } else {
      if(is.integer(as.integer(input$Et3_Reg_Tex))){
        Numero<-as.integer(input$Et3_Reg_Tex)
        if(Numero>Tamaño[1]){
          values$Num_Reg<-Tamaño[1]
          updateTextInput(session, "Et3_Reg_Tex", value = Tamaño[1])
        } else if(Numero<1) {
          values$Num_Reg<-1
          updateTextInput(session, "Et3_Reg_Tex", value = 1)
        } else {
          values$Num_Reg<-as.integer(input$Et3_Reg_Tex)
          updateTextInput(session, "Et3_Reg_Tex", value = values$Num_Reg)
        }
      }
      else {
        values$Num_Reg<-1
        updateTextInput(session, "Et3_Reg_Tex", value = 1)
      }
    }
  })
  
  observeEvent(input$Bot_Guar, {
    SalvarDatos(E3Dat,values$d,values$e)#values$Num_Reg,input$Et3_inText
  })
  
  observeEvent(input$Bot_RAM, {
    if (!is.null(input$Cod_Cor) & input$Et3_inText==""){
      values$d[values$Num_Reg]<-input$Cod_Cor
      values$e[values$Num_Reg]<-CapDef(input$Cod_Cor)[[2]]
      updateTextInput(session, "Et3_inText",  "Código:", value = "")
      if (values$Num_Reg<Tamaño[1]){
        #updateNumericInput(session, "values$Num_Reg", value = values$Num_Reg+1)
        values$Num_Reg<-values$Num_Reg+1
        updateTextInput(session, "Et3_Reg_Tex", value = values$Num_Reg)
        }
    }
    else if (input$Et3_inText!="" & is.null(input$Cod_Cor)){
      #if(nchar(input$Et3_inText)==4 & !is.na(as.integer(substr(input$Et3_inText,2,4)))){
      E3_Tex<-gsub("\\b(\\w)","\\U\\1",input$Et3_inText, perl=TRUE)
      #Bandera<-condiciones(E3Codigos,E3_Tex)#,E3Dat[values$Num_Reg,"SEXO"]
      
      E3_Descr2<-E3Codigos[E3Codigos[,1]==E3_Tex,2]
      #E3_Descr<-E3_Val[2]
      #cat("Valor de E3_Val",E3_Descr)
      #Val_Cap<-CapDef(E3_Tex)
      #Val_Cap1<-Val_Cap[[1]]
      if(length(E3_Descr2)!=0){
        E3_Descr<-E3Codigos[E3Codigos[,1]==E3_Tex,c(3,4)]
        #cat("Sexo edad ",E3_Descr)
        E3_Descr3<-E3_Descr[1]
        if(is.na(E3_Descr3) | E3Dat[values$Num_Reg,"SEXO"]==E3_Descr3){
          E3_Descr4<-E3_Descr[2]#as.character(E3_Descr[2])
          Pasa<-1
          if(!is.na(E3_Descr4)){
            Pasa<-condiciones(E3_Descr4,as.character(E3Dat[values$Num_Reg,"EDAD"]),as.character(E3Dat[values$Num_Reg,"UNIEDAD"]))
          }
          if(Pasa==1) {
            values$d[values$Num_Reg]<-E3_Tex
            #values$e[values$Num_Reg]<-E3_Descr2
            values$e[values$Num_Reg]<-CapDef(E3_Tex)[[2]]
            updateTextInput(session, "Et3_inText",  "Código:", value = "")
            if (values$Num_Reg<Tamaño[1]){
              #updateNumericInput(session, "values$Num_Reg", value = values$Num_Reg+1)
              values$Num_Reg<-values$Num_Reg+1
              updateTextInput(session, "Et3_Reg_Tex", value = values$Num_Reg)
              }
          } else {session$sendCustomMessage(type = 'testmessage', message = "Edad fuera de rango")}#{info("Edad fuera de rango")}
        } else {session$sendCustomMessage(type = 'testmessage', message = "El sexo no concuerda")}#{info("El sexo no concuerda")}
      } else {session$sendCustomMessage(type = 'testmessage', message = "Código no valido")}#{info("Código no valido")}
      #}
      
    }
    
    
    #Dat_Def<-Etapa3Data()
    #Dat_Def[values$Num_Reg,"COD_SEL"]<-input$Et3_inText
    #SalvarDatos(Etapa3Data())
  })
  
  #observeEvent(input$Bot_Guar, {
  #  output$E3COD<-isolate(renderText({Etapa3Data()[values$Num_Reg,"COD_SEL"]}))
  #})
  
  
  
  observe({
    #if (length(values$Num_Reg)!=0){
    #  if (values$Num_Reg<1){updateNumericInput(session, "values$Num_Reg", value = 1)}
    #}
    #input$Bot_Guar
    output$E3DatIde<-renderText({
      paste0( "Año de registro:  ",     E3Dat[values$Num_Reg,"ANIOREG"], " \n",
              "Entidad de registro:  ", E3Dat[values$Num_Reg,"ENTREG"], " \n",
              "Mes de registro:  ",     E3Dat[values$Num_Reg,"MESREG"], " \n"
      )
    })
    output$E3Folea1<-renderText({
      paste0("Foliocer:  ", E3Dat[values$Num_Reg,"FOLIOCER"], " \n",
             "Sexo:  ",     E3Dat[values$Num_Reg,"SEXO"], " \n",
             "Edad:  ", paste0(E3Dat[values$Num_Reg,"UNIEDAD"],E3Dat[values$Num_Reg,"EDAD"]), " \n"
      )
    })
    output$E3Folea2<-renderText({
      paste0("Fue un presunto:  ",     E3Dat[values$Num_Reg,"PRESUNTO"], " \n",
             #"Motivo de la lesión:",E3Dat[values$Num_Reg,"SITUACION"], " \n",
             "Lugar de lesión:  ",E3Dat[values$Num_Reg,"LUGLESION"], " \n"
             #"Parentesco del agresor:",E3Dat[values$Num_Reg,"VIOLENCIA"], " \n"
      )
    })
    
    #output$E3NORE<-renderText({E3Dat[values$Num_Reg,"NOREG1"]})
    #output$E3Foli<-renderText({E3Dat[values$Num_Reg,"FOLIOCER"]})
    #output$E3Sexo<-renderText({E3Dat[values$Num_Reg,"SEXO"]})
    #output$E3Edad<-renderText({E3Dat[values$Num_Reg,"EDAD"]})
    output$E3Desc1<-renderText({E3Dat[values$Num_Reg,"DESCR_LIN1"]})#
    output$E3t_CoA<-renderText({E3Dat[values$Num_Reg,"TXT_CODIA"]})#
    output$E3Desc2<-renderText({E3Dat[values$Num_Reg,"DESCR_LIN2"]})#
    output$E3t_CoB<-renderText({E3Dat[values$Num_Reg,"TXT_CODIB"]})#
    output$E3Desc3<-renderText({E3Dat[values$Num_Reg,"DESCR_LIN3"]})#
    output$E3t_CoC<-renderText({E3Dat[values$Num_Reg,"TXT_CODIC"]})#
    output$E3Desc4<-renderText({E3Dat[values$Num_Reg,"DESCR_LIN4"]})#
    output$E3t_CoD<-renderText({E3Dat[values$Num_Reg,"TXT_CODID"]})#
    output$E3Desc5<-renderText({E3Dat[values$Num_Reg,"DESCR_LIN5"]})#
    output$E3t_CoI<-renderText({E3Dat[values$Num_Reg,"TXT_CODII"]})#
    output$E3DURATION1<-renderText({E3Dat[values$Num_Reg,"DURATION1"]})#
    output$E3CAUS<-renderText({E3Dat[values$Num_Reg,"CAUSADEF"]})
    output$E3RECO<-renderText({E3Dat[values$Num_Reg,"RECODCBD"]})
    output$E3RECO2<-renderText({E3Dat[values$Num_Reg,"RECODCBD2"]})
    
    
    #output$E3ENTREG<-renderText({E3Dat[values$Num_Reg,"ENTREG"]})
    #output$E3ANIOREG<-renderText({E3Dat[values$Num_Reg,"ANIOREG"]})
    #output$E3MESREG<-renderText({E3Dat[values$Num_Reg,"MESREG"]})
    #output$E3UNIEDAD<-renderText({E3Dat[values$Num_Reg,"UNIEDAD"]})
    #output$E3OCURRIO<-renderText({E3Dat[values$Num_Reg,"OCURRIO"]})
    #output$E3ASISTENCIA<-renderText({E3Dat[values$Num_Reg,"ASISTENCIA"]})
    
    
    #output$E3CONDIEMB<-renderText({E3Dat[values$Num_Reg,"CONDIEMB"]})
    #output$E3PRESUNTO<-renderText({E3Dat[values$Num_Reg,"PRESUNTO"]})
    #output$E3SITUACION<-renderText({E3Dat[values$Num_Reg,"SITUACION"]})
    #output$E3LUGLESION<-renderText({E3Dat[values$Num_Reg,"LUGLESION"]})
    #output$E3VIOLENCIA<-renderText({E3Dat[values$Num_Reg,"VIOLENCIA"]})
    output$E3DURATION2<-renderText({E3Dat[values$Num_Reg,"DURATION2"]})#
    output$E3DURATION3<-renderText({E3Dat[values$Num_Reg,"DURATION3"]})#
    output$E3DURATION4<-renderText({E3Dat[values$Num_Reg,"DURATION4"]})#
    output$E3DURATION5<-renderText({E3Dat[values$Num_Reg,"DURATION5"]})#
    #output$E3OMITIDO<-renderText({E3Dat[values$Num_Reg,"OMITIDO"]})
    output$E3CODER<-renderText({E3Dat[values$Num_Reg,"CODER"]})
    #output$E3RECODIA<-renderText({E3Dat[values$Num_Reg,"RECODIA"]})
    #output$E3RECODIB<-renderText({E3Dat[values$Num_Reg,"RECODIB"]})
    #output$E3RECODIC<-renderText({E3Dat[values$Num_Reg,"RECODIC"]})
    #output$E3RECODID<-renderText({E3Dat[values$Num_Reg,"RECODID"]})
    #output$E3RECODII<-renderText({E3Dat[values$Num_Reg,"RECODII"]})
    output$E3CODER2<-renderText({E3Dat[values$Num_Reg,"CODER2"]})
    output$E3REV<-renderText({E3Dat[values$Num_Reg,"REV"]})
    output$E3NEWFLD<-renderText({E3Dat[values$Num_Reg,"NEWFLD"]})
    
    
    
    output$E3COD<-renderText({values$d[values$Num_Reg]})
    #output$E3Tam<-renderText({Tamaño[1]})
    output$E3Nul<-renderText({
      Nulos <- values$d[is.na(values$d)]
      Cuan_Nul<-length(Nulos)
      Test<-paste0("Registro ",values$Num_Reg," de ",Tamaño[1], ",   Pendientes:  ",Cuan_Nul)
      return(Test)})
    
    
    #output$E3Link<-renderText({
    #  Text<-paste0("https://eciemaps.mspsi.es/ecieMaps/browser/index_10_2008.html#search=",as.character(substr(values$d[values$Num_Reg],1,3)))
    #cat("Text",Text)
    #  return(Text)
    #  })
    output$E3CapTex<-renderText({values$e[values$Num_Reg]})
  })
  
  observe({
    #showReactLog()
    #if (values$Num_Reg<1){updateNumericInput(session, "values$Num_Reg", value = 1)}
    #if (length(values$Num_Reg)!=0) { #| is.null(values$Num_Reg))
    #  if (values$Num_Reg<1 | values$Num_Reg==""){updateNumericInput(session, "values$Num_Reg", value = 1)}
    #  else if (Tamaño[1]<values$Num_Reg){updateNumericInput(session, "values$Num_Reg", value = Tamaño[1])}
    #  else {updateNumericInput(session, "values$Num_Reg", value = 1)}
    #  }
    r_options <- list()
    r_options[[paste(E3Dat[values$Num_Reg,"CAUSADEF"], "Sistema")]] <-paste0(as.character(E3Dat[values$Num_Reg,"CAUSADEF"]),"")
    r_options[[paste(E3Dat[values$Num_Reg,"RECODCBD"], "Codificador 1")]] <-paste0(as.character(E3Dat[values$Num_Reg,"RECODCBD"]),"")
    r_options[[paste(E3Dat[values$Num_Reg,"RECODCBD2"], "Codificador 2")]] <-paste0(as.character(E3Dat[values$Num_Reg,"RECODCBD2"]),"")
    #r_options[[paste(Etapa3Data()[values$Num_Reg,"COD_SEL"], "Experto")]] <-paste0(as.character(Etapa3Data()[values$Num_Reg,"COD_SEL"]),"")
    
    # Set the label, choices, and selected item
    updateCheckboxGroupInput(session, "Cod_Cor",
                             label = "¿Cuál  es el código correcto?",
                             choices = r_options
                             #selected = ""
                             #selected = NULL #paste0(as.character(Etapa3Data()[values$Num_Reg,"CAUSADEF"]),"")
    )
    #values$Num_Reg<-values$Num_Reg
  })
  
  #observe({
  #Cod_fin<<-input$Cod_Cor
  #  updateTextInput(session, "Et3_inText",
  #                  label = paste("Código definitivo"),
  #                  value = paste(input$Cod_Cor)
  #  )
  #})
  
  
  #observeEvent(input$Bot_RAM, {
  #  if (values$Num_Reg<Tamaño[1]){updateNumericInput(session, "values$Num_Reg", value = values$Num_Reg+1)}
  #})
  
  observeEvent(input$Bot_Ant, {
    if (1<values$Num_Reg){
      #updateNumericInput(session, "values$Num_Reg", value = values$Num_Reg-1)
      values$Num_Reg<-values$Num_Reg-1
      updateTextInput(session, "Et3_inText", value = "")
      updateTextInput(session, "Et3_Reg_Tex", value = values$Num_Reg)
      }
  })
  
  observeEvent(input$Bot_Sig, {
    if (values$Num_Reg<Tamaño[1]){
      #updateNumericInput(session, "values$Num_Reg", value = values$Num_Reg+1)
      values$Num_Reg<-values$Num_Reg+1
      updateTextInput(session, "Et3_inText", value = "")
      updateTextInput(session, "Et3_Reg_Tex", value = values$Num_Reg)
    }
  })
  
  output$Etapa3Tabla1 <- renderDataTable({
    E3Dat
  },options = list(lengthMenu = c(5, 10, 25, 50), 
                   pageLength = 5))
  
  output$Etapa3Tabla2 <- renderDataTable({
    E3Codigos
  },options = list(lengthMenu = c(5, 10, 25, 50), 
                   pageLength = 10))
  
  observeEvent(input$E3_sal, {
    SalvarDatos(E3Dat,values$d,values$e)
    stopApp()
  })
  ####
  #_______________________________________________________________
})

