DefCap <- function(cau)
	{
	d<-length(cau)
	Capitulo<-rep(0,d)
		for (i in 1:d) 
		{
		ca<-substr(cau[i],1,3)
		if ("A00"<=ca & ca<="B99") {Capitulo[i]<-1}
		else if ("C00"<=ca & ca<="D48") {Capitulo[i]<-2}
		else if ("D50"<=ca & ca<="D89") {Capitulo[i]<-3}
		else if ("E00"<=ca & ca<="E90") {Capitulo[i]<-4}
		else if ("F00"<=ca & ca<="F99") {Capitulo[i]<-5}
		else if ("G00"<=ca & ca<="G99") {Capitulo[i]<-6}
		else if ("H00"<=ca & ca<="H59") {Capitulo[i]<-7}
		else if ("H60"<=ca & ca<="H95") {Capitulo[i]<-8}
		else if ("I00"<=ca & ca<="I99") {Capitulo[i]<-9}
		else if ("J00"<=ca & ca<="J99") {Capitulo[i]<-10}
		else if ("K00"<=ca & ca<="K93") {Capitulo[i]<-11}
		else if ("L00"<=ca & ca<="L99") {Capitulo[i]<-12}
		else if ("M00"<=ca & ca<="M99") {Capitulo[i]<-13}
		else if ("N00"<=ca & ca<="N99") {Capitulo[i]<-14}
		else if ("O00"<=ca & ca<="O99") {Capitulo[i]<-15}
		else if ("P00"<=ca & ca<="P96") {Capitulo[i]<-16}
		else if ("Q00"<=ca & ca<="Q99") {Capitulo[i]<-17}
		else if ("R00"<=ca & ca<="R99") {Capitulo[i]<-18}
		else if ("S01"<=ca & ca<="T99") {Capitulo[i]<-19}
		else if ("V01"<=ca & ca<="Y98") {Capitulo[i]<-20}
		else Capitulo[i]<-99
		}
	Error<-sum(as.integer(Capitulo==99))
	list(Capitulos=Capitulo,Errores=Error)
	}
