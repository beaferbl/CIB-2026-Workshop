# En este script se inluyen los bloques de código del pdf numerados de la misma forma.

##### 2.3. Instalación de paquetes #####
install.packages(c("dplyr","ggplot2","heplots","RColorBrewer",
                   "readxl","swirl"))
library(dplyr)
library(ggplot2)
library(heplots)
library(RColorBrewer)
library(readxl)
library(swirl)

##### 3. R como calculadora #####
3+2 # suma
3-2 # resta
3*2 # multiplicación
3/2 # división con decimales
3%/%2 # división entera
3**2 # potencia
3^2 # potencia

##### 4.1. Concepto de variable #####
a <- 3+2
a=3+2 
a < -3 # ¡OJO!

# Las variables no tienen por qué ser números, también pueden ser palabras.
b <- "Hola"
c <- "Adios"

# Las variables se pueden sobreescribir y reasignarles otro valor
c<-4

##### 4.2. Concepto de objeto #####
# Funciones
sqrt(2)
class(sqrt)
help(sqrt)

# Vectores
edad <- c(21,18,20,21)
carrera<-c("psicología","medicina", "medicina", "biologia")
edad.carrera<-c(21,18,"psicología","medicina")
class(edad.carrera)
edad[2] # acceder a un elemento de un vector
edad[c(3,1,2)] # acceder a varios elementos de un vector
sort(edad) # ordenar un vector
sort(edad, decreasing = TRUE)
order(edad)
order(edad, decreasing = TRUE)
sort(carrera)
order(carrera)

# Conjuntos de datos
df<-data.frame(meses=c("enero","febrero","marzo","abril","mayo","junio",
                       "julio","agosto","septiembre","octubre",
                       "noviembre","diciembre"),
               dias=c(31,28,31,30,31,30,31,31,30,31,30,31))

df$meses # acceder a los elementos de un conjunto de datos
df$dias
df[,] # nos devuelve todas las filas y todas las columnas
df[,1] # nos devuelve todas las filas y la primera columna
df[1,] # nos devuelve la primera fila y todas las columnas
df[,"dias"] # nos devuelve todas las filas y la columna "dias"

##### 5. Rutas y directorios #####
getwd() # obtener el directorio en el que estoy
list.files() # lista los archivos que hay en el directorio
list.files(full.names = TRUE) # con rutas absolutas

##### 6. Operaciones con conjuntos de datos #####
## 6.1. Importar un conjunto de datos
bca<-read_xlsx("BCA_assay.xlsx",sheet=1,skip = 1)

tmb<-read.delim("tmb_mskcc_2018_clinical_data.tsv")
tmb<-read.table("tmb_mskcc_2018_clinical_data.tsv",header=TRUE,sep="\t") 

## 6.2. Conocer la estructura de un conjunto de datos
str(tmb)
summary(tmb)
dim(tmb)
nrow(tmb)
ncol(tmb)
rownames(tmb)
colnames(tmb)
head(tmb)
tail(tmb)

# Acceder a las columnas:
tmb$Patient.ID
tmb[,"Patient.ID"]

# Acceder a la primera fila
tmb[1,]

# Acceder a un valor específico:
tmb[3,2]


## 6.3. Filtrar un conjunto de datos
tmb2<-tmb[1:10,]
tmb2<-tmb[tmb$Sex=="Female",]
tmb2<-tmb[tmb$TMB..nonsynonymous.>5,]
tmb2<-tmb[tmb$Cancer.Type!="Glioma",]
tmb_mama_metastasis<-tmb[tmb$Cancer.Type == "Breast Cancer" &
                           tmb$Sample.Type == "Metastasis",]

## 6.4. Manejar valores perdidos (NA)
mean(tmb$Mutation.Count)
mean(tmb$Mutation.Count, na.rm = TRUE)
colSums(is.na(tmb)) # Nulos en la columna de concentracion
tmb.noNA <- na.omit(tmb) # Elimina todas las filas con algún NA

## 6.5. El paquete dplyr
tmb %>% 
  group_by(Sex) %>%
  count() # Similar a la función table

# Aparece un NA
tmb %>% 
  group_by(Cancer.Type) %>%
  summarise(Promedio_tiempo = mean(Age.at.Which.Sequencing.was.Reported..Days.))

tmb %>% 
  group_by(Cancer.Type) %>%
  summarise(Promedio_tiempo = mean(Age.at.Which.Sequencing.was.Reported..Days.,
                                   na.rm = TRUE))

tmb %>% 
  filter(Cancer.Type == "Melanoma") %>%
  group_by(Sex) %>%
  summarise(Promedio_tiempo = mean(Age.at.Which.Sequencing.was.Reported..Days.,
                                   na.rm = TRUE))

##### 7. Breve análisis estadístico #####
tmb<-read.delim("tmb_mskcc_2018_clinical_data.tsv")
str(tmb)

# Ver si hay valores duplicados
id.duplicados<-duplicated(tmb$Patient.ID)
sum(id.duplicados)

# Comprobar si hay valores perdidos
na.count<-rowSums(is.na(tmb))
tmb.complete<-tmb[na.count==0,]

## 7.1. Estadística descriptiva
# 1. Variables de tipo numérico.
summary(tmb.complete)

mean(tmb$Mutation.Count)
mean(tmb$Mutation.Count, na.rm = TRUE)
mean(tmb.complete$Mutation.Count)

min(tmb.complete$Mutation.Count)
max(tmb.complete$Mutation.Count)

# 2. Variables de tipo categórico.
table(tmb.complete$Cancer.Type)

# 3. Gráficos
# Gráfico de dispersión
plot(tmb.complete$Mutation.Count,
     tmb.complete$Age.at.Which.Sequencing.was.Reported..Days.,
     xlab="número de mutaciones",
     ylab = "edad (días)")

# Gráfico de cajas y bigotes
boxplot(tmb.complete$Mutation.Count)

boxplot(tmb.complete$Mutation.Count ~tmb.complete$Cancer.Type,
        xlab="Tipo de cáncer",
        ylab="Número de mutaciones")
# Gráfico de barras
age.freq<-table(tmb.complete$Age.Group.at.Diagnosis.in.Years)[c(1,3,4,5,2)]
age.bar<-barplot(age.freq,
                 col=brewer.pal(5, "Set3"),
                 xlab="Edad al diagnóstico (años)",
                 ylab="Número de pacientes",
                 ylim = c(0,max(age.freq)+30),
                 cex.names=0.6
)

## 7.2. Estadística inferencial
# 1. Test de normalidad
shapiro.test(tmb.complete$Mutation.Count)

qqnorm(tmb.complete$Mutation.Count)
qqline(tmb.complete$Mutation.Count)

# 2. Test de igualdad de varianzas
leveneTests(tmb.complete[,"Mutation.Count", drop=FALSE],
            tmb.complete$Sex)
# 3. Comparación de medias para distribuciones normales (pruebas paramétricas)
plot(extra ~ group, data = sleep)

leveneTests(sleep[,1, drop=FALSE], sleep$group)
t.test(extra ~ group, data = sleep, var.equal=TRUE)

anova(lm(Mutation.Count ~ Cancer.Type, data = tmb.complete))

# 4. Comparación de medias para distribuciones no normales (pruebas no paramétricas)
# Comparación de dos grupos: test de Wilcoxon.
wilcox.test(tmb.complete$Mutation.Count[tmb.complete$Sex=="Female"],
            tmb.complete$Mutation.Count[tmb.complete$Sex=="Male"])
# Comparación de más de dos grupos: test de Kruskal Wallis.
kruskal.test(Mutation.Count ~ Cancer.Type, data = tmb.complete)
# 5. Correlación entre variables.
plot(tmb.complete$Age.at.Which.Sequencing.was.Reported..Days.,
     tmb.complete$Mutation.Count,
     xlab="Edad (días)",
     ylab = "Número de mutaciones")
cor(tmb.complete$Age.at.Which.Sequencing.was.Reported..Days.,
    tmb.complete$Mutation.Count)

pairs(tmb.complete[,c("Age.at.Which.Sequencing.was.Reported..Days.",
                      "Mutation.Count",
                      "Sample.coverage",
                      "TMB..nonsynonymous.")]
)

# 6. Test para variables categóricas
table(tmb.complete$Cancer.Type,tmb.complete$Sex)
chisq.test(table(tmb.complete$Sex[tmb.complete$Cancer.Type=="Bladder Cancer"]))

## 7.3. Creación de gráficos con el paquete ggplot2
display.brewer.all(colorblindFriendly = TRUE)
# Gráfico de cajas y bigotes
ggplot(data = tmb, aes(x = Overall.Survival.Status, y = Overall.Survival..Months., fill = factor(Overall.Survival.Status))) + 
  geom_boxplot() +
  scale_fill_manual(values = c("0:LIVING" = "lightblue", "1:DECEASED" = "grey")) +
  labs(title = "Overall Survival by Status", 
       x = "Survival Status", 
       y = "Overall Survival (Months)")

# Gráfico de barras.
age.freq<-table(tmb$Age.Group.at.Diagnosis.in.Years)[c(1,3,4,5,2)]
age.bar<-barplot(age.freq,
                 col=brewer.pal(5, "Blues"),
                 xlab="Edad al diagnóstico (años)",
                 ylab="Número de pacientes",
                 ylim = c(0,max(age.freq)+30),
                 cex.names=0.6
)

# Gráfico de dispersión:
ggplot(data=tmb[1:20,], 
       aes(y=Sample.coverage, x=Tumor.Purity, color=Sample.Type)) +
  geom_point(size=3) + # capa de puntos tamaño 3
  theme(text=element_text(size=10))+
  labs(y="Cobertura",x="Pureza Tumoral")

p <- ggplot(data=tmb[1:20,], 
            aes(y=Sample.coverage, x=Tumor.Purity, color=Sample.Type)) +
  geom_point(size=3) + # capa de puntos tamaño 3
  theme(text=element_text(size=10))+
  labs(y="Cobertura",x="Pureza Tumoral")
