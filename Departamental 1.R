install.packages("readxl")
library(readxl)

############Cargar datos####################
Natalidad 2007-2018 <-read_excel(Natalidad 2007-2018.xlsx)
######Como no corrio lo anterior, voy a importar el excel de otra forma#####
Natalidad<-Natalidad_2007_2018
Natalidad
head(Natalidad)
View(Natalidad)
names(Natalidad)
attach(Natalidad)
ls(Natalidad)
str(Natalidad)
summary(`Nacidos vivos observados`)
############Crear variable región

Region<-select(Natalidad,`REGIÓN DE RESIDENCIA`)%>%unique
view(Region)

#########Para ver el promedio de nacidos vivos observados por  region
#para hacer una tabla 
tab_region<-table(Natalidad$`Nacidos vivos observados`)
tab_region<-table(Natalidad$`REGIÓN DE RESIDENCIA`)

#promedio de nacidos vivos por región
mean(tab_region)
z<-mean(tab_region)
##### Para ver el promedio de las defunciones generales por región
#para hacer tabla

tab_defunciones<-(Natalidad$`REGIÓN DE RESIDENCIA`)
tab_defunciones<-(Natalidad$`Defunciones generales`)

###promedio de defunciones generales por región
mean(tab_defunciones)
x<-mean(tab_defunciones)
x

Natalidad%>%group_by(`REGIÓN DE RESIDENCIA`)%>%summarise(mean=mean(`Nacidos vivos observados`), variance=var(`Nacidos vivos observados`))

tab_año<-table(Natalidad$`AÑOS DE LAS REGIONES`)

#Promedio segun la población por localidad

Natalidad%>% group_by(`REGIÓN DE RESIDENCIA`)%>%summarise(mean=mean(Población), variance=var(Población))

######Agrupar por año de nacidos vivos

Natalidad_2007_2018%>%
  group_by(`AÑOS DE LAS REGIONES`)%>%summarise(mean=mean(`Nacidos vivos observados`), variance=var(`Nacidos vivos observados`), sd=sd(`Nacidos vivos observados`), length=length(`Nacidos vivos observados`), max=max(`Nacidos vivos observados`), min=min(`Nacidos vivos observados`))

####Agrupar por años de defunciones

Natalidad_2007_2018%>%
  group_by(`AÑOS DE LAS REGIONES`)%>%summarise(mean=mean(`Defunciones generales`), variance=var(`Defunciones generales`), sd=sd(`Defunciones generales`), length=length(`Defunciones generales`), max=max(`Defunciones generales`), min=min(`Defunciones generales`))

##### Agrupar por años de matrimonios  

Natalidad_2007_2018%>%
  group_by(`AÑOS DE LAS REGIONES`)%>%summarise(mean=mean(Matrimonios), variance=var(Matrimonios), sd=sd(Matrimonios), length=length(Matrimonios), max=max(Matrimonios), min=min(Matrimonios))

#####Agrupar por años de población

Natalidad_2007_2018%>%
  group_by(`AÑOS DE LAS REGIONES`)%>%summarise(mean=mean(Población), variance=var(Población), sd=sd(Población), length=length(Población), max=max(Población), min=min(Población))



####Condicionales

Natalidad_2007_2018<-function(`Defunciones generales`)
  if(`Defunciones generales`>=50 &&`Defunciones generales`<=100){ 
    print("pais con muchas defunciones")
  }else if(`Defunciones generales`>=1 && `Defunciones generales`<=49){
    print("pais con pocas defunciones")
  }else
    print("no esta determinado")



