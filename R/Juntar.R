PonerDefCap <- function(Clave,CAUSADEF,COD_SEL)
{
  dimn<-length(CAUSADEF)
  dimn
  Fin3<-rep(0,dimn)
  Fin4<-rep(0,dimn)
  Caus<-as.character(CAUSADEF)
  CodFin<-as.character(COD_SEL)
  
  for (i in 1:dimn)
  {
    Ca<-substr(Caus[i],1,3)
    CF<-substr(CodFin[i],1,3)
    if (Ca==CF) {Fin3[i]<-1} 
    else {Fin3[i]<-0}
  }
  
  for (i in 1:dimn)
  {
    Ca<-substr(Caus[i],1,4)
    if (substr(Caus[i],4,4)=="X") {Ca<-substr(Caus[i],1,3)}
    CF<-substr(CodFin[i],1,4)
    if (substr(CodFin[i],4,4)=="X") {CF<-substr(CodFin[i],1,3)}
    if (Ca==CF) {Fin4[i]<-1} 
    else {Fin4[i]<-0}
  }
  
  Fin3<-as.integer(Fin3)
  Fin4<-as.integer(Fin4)
  
  cau<-as.character(CAUSADEF)
  Cap<-DefCap(cau)
  CapCau<-as.integer(Cap[[1]])
  
  cau<-as.character(COD_SEL)
  Cap<-DefCap(cau)
  CapFin<-as.integer(Cap[[1]])
  
  EtapaCC<-data.frame(Clave,CAUSADEF,COD_SEL,CapCau,CapFin,Fin3,Fin4)
  
  remove(CapFin,Fin3,Fin4)
  remove(CF,Ca,Cap,CapCau,Caus,CodFin,cau,dimn,i)
  list(Tabla=EtapaCC)
}