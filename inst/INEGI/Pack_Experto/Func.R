
condiciones <- function(E3_Descr4="character", E3_Edad="character", E3_Unidad="character") {#, SEXO
  bandera<-0
  
  Let_inf<-substr(E3_Descr4, 4, 4)
  if (Let_inf=="H"){infer<-0} else if (Let_inf=="D") {infer<-1} else if (Let_inf=="A") {infer<-2}
  E3_Lim_inf<-as.integer(paste0(infer,substr(E3_Descr4, 1, 3)))
  
  Let_sup<-substr(E3_Descr4, 9, 9)
  if (Let_sup=="H") {super<-0} else if (Let_sup=="D") {super<-1} else if (Let_sup=="A") {super<-2}
  E3_Lim_sup<-as.integer(paste0(super,substr(E3_Descr4, 6, 8)))
  #cat("Limites",E3_Lim_inf,E3_Lim_sup)
  
  if (E3_Unidad=="H"){infer<-0} else if (E3_Unidad=="D") {infer<-1} else if (E3_Unidad=="A") {infer<-2}
  E3_Edad_Reg<-as.integer(paste0(infer,E3_Edad))
  
  if (E3_Lim_inf<=E3_Edad_Reg & E3_Edad_Reg<=E3_Lim_sup) {bandera<-1}
  
  return(bandera)
}




SalvarDatos <- function(Dat_Def,Cod_fin,Cod_Tex) {#
  Dat_Def[,"COD_SEL"]<-Cod_fin
  Dat_Def[,"Cap_Tex"]<-Cod_Tex
  saveRDS(Dat_Def, "Rev2.rds")
  }

CargarDatos <- function() {
  Dat<-readRDS("Rev2.rds")
  Dat
}

CapDef <- function(cau="character")
{
    ca<-substr(cau,1,3)
    if ("A00"<=ca & ca<="B99") {Capitulo<-1
    Cau_Def_Tex<-"Cap\u00edtulo I Enfermedades infecciosas y parasitarias"}
    else if ("C00"<=ca & ca<="D48") {Capitulo<-2
    Cau_Def_Tex<-"Cap\u00edtulo II Neoplasias"}
    else if ("D50"<=ca & ca<="D89") {Capitulo<-3
    Cau_Def_Tex<-"Cap\u00edtulo III Enfermedades de la sangre y de los \u00f3rganos hematopoy\u00e9ticos"}
    else if ("E00"<=ca & ca<="E90") {Capitulo<-4
    Cau_Def_Tex<-"Cap\u00edtulo IV Enfermedades endocrinas, nutricionales y metab\u00f3licas."}
    else if ("F00"<=ca & ca<="F99") {Capitulo<-5
    Cau_Def_Tex<-"Cap\u00edtulo V Transtornos mentales y del comportamiento."}
    else if ("G00"<=ca & ca<="G99") {Capitulo<-6
    Cau_Def_Tex<-"Cap\u00edtulo VI Enfermedades del sistema nervioso."}
    else if ("H00"<=ca & ca<="H59") {Capitulo<-7
    Cau_Def_Tex<-"Cap\u00edtulo VII Enfermedades del ojo y sus anexos."}
    else if ("H60"<=ca & ca<="H95") {Capitulo<-8
    Cau_Def_Tex<-"Cap\u00edtulo VIII Enfermedades del o\u00eddo y de la ap\u00f3fisis mastoides"}
    else if ("I00"<=ca & ca<="I99") {Capitulo<-9
    Cau_Def_Tex<-"Cap\u00edtulo IX Enfermedades del sistema cardiocirculatorio"}
    else if ("J00"<=ca & ca<="J99") {Capitulo<-10
    Cau_Def_Tex<-"Cap\u00edtulo X Enfermedades del sistema respiratorio"
    }
    else if ("K00"<=ca & ca<="K93") {Capitulo<-11
    Cau_Def_Tex<-"Cap\u00edtulo XI Enfermedades del sistema digestivo"
    }
    else if ("L00"<=ca & ca<="L99") {Capitulo<-12
    Cau_Def_Tex<-"Cap\u00edtulo XII Enfermedades de la piel y tejido subcut\u00e1neo"
    }
    else if ("M00"<=ca & ca<="M99") {Capitulo<-13
    Cau_Def_Tex<-"Cap\u00edtulo XIII Enfermedades del sistema osteomuscular y del tejido conjuntivo."
    }
    else if ("N00"<=ca & ca<="N99") {Capitulo<-14
    Cau_Def_Tex<-"Cap\u00edtulo XIV Enfermedades del sistema genitourinario"
    }
    else if ("O00"<=ca & ca<="O99") {Capitulo<-15
    Cau_Def_Tex<-"Cap\u00edtulo XV Enfermedades del embarazo, parto y puerperio"
    }
    else if ("P00"<=ca & ca<="P96") {Capitulo<-16
    Cau_Def_Tex<-"Cap\u00edtulo XVI Enfermedades del feto y reci\u00e9n nacido"
    }
    else if ("Q00"<=ca & ca<="Q99") {Capitulo<-17
    Cau_Def_Tex<-"Cap\u00edtulo XVII Enfermedades cong\u00e9nitas, malformaciones y alteraciones cromos\u00f3micas"
    }
    else if ("R00"<=ca & ca<="R99") {Capitulo<-18
    Cau_Def_Tex<-"Cap\u00edtulo XVIII S\u00edntomas y observaciones cl\u00ednicas o de laboratorio anormales no clasificados en otras parte"
    }
    else if ("S00"<=ca & ca<="T98") {Capitulo<-19
    Cau_Def_Tex<-"Cap\u00edtulo XIX Lesiones, heridas, intoxicaciones y otros factores externos"
    }
    else if ("V01"<=ca & ca<="Y98") {Capitulo<-20
    Cau_Def_Tex<-"Cap\u00edtulo XX Causas externas de mortalidad y morbilidad"
    }
    else if ("Z00"<=ca & ca<="Z99") {Capitulo<-21
    Cau_Def_Tex<-"Cap\u00edtulo XXI Factores que influyen en el estado de salud y contacto con los servicios de salud."
    }
    else if ("U00"<=ca & ca<="U99") {Capitulo<-22
    Cau_Def_Tex<-"Cap\u00edtulo XXII C\u00f3digos para prop\u00f3sitos especiales"
    }
    else Capitulo<-23
    return(list(Capitulo,Cau_Def_Tex))
#return(Capitulo,Cau_Def_Tex)
}

