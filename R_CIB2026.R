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

var(tmb.complete$Mutation.Count)
sd(tmb.complete$Mutation.Count)
Q1_25 = quantile(tmb.complete$Mutation.Count, 0.25)
Q3_75 = quantile(tmb.complete$Mutation.Count, 0.75)
IQR = IQR(tmb.complete$Mutation.Count)
      
fivenum(tmb.complete$Mutation.Count)



# 2. Variables de tipo categórico.
table(tmb.complete$Cancer.Type)

# 3. Gráficos
# Cheat sheet: http://rstudio.github.io/cheatsheets/data-visualization.pdf
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

## Grafico de barras con ggplot2:

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

# Guardar los graficos:
ggsave("lastplot_name.png",# This code only works for ggplot-made figures
       width = 10,   
       height = 6,   
       units = "in", # Units (in, cm, mm)
       dpi = 300)    # Resolution (300 is the standard for publications)  

## EJERCICIOS

# CbioPortal for Cancer Genomics  https://www.cbioportal.org/
# https://www.cbioportal.org/study/clinicalData?id=mel_ucla_2016
# Metastatic Melanoma (UCLA, Cell 2016)
# Whole-exome sequencing of 38 pretreated (pembrolizumab, nivolumab) melanoma tumor-normal pairs.PubMed
# https://pubmed.ncbi.nlm.nih.gov/26997480/

# Ejercicio 1: Dataset Cancer Melanoma (Universidad de California)
# 1.1 Carga al entorno de trabajo el conjunto de datos en excel: complete_mel_ucla_2016_clinical_data_excel.xlsx
# 1.2 Genera un Diagrama de barras que muestre los conteos de las columnas A) "M Stage", B) "Biopsy Time"
# 1.3 Puedes probar a cambiar la posición de la leyenda? theme(legend.position = "bottom"), try "top" or "right"
# 1.4 Guarda el gráfico generado en png o jpg
# 1.5 Genera un grafico de dispersión que represente la relación entre las variables "Mutation Count" y "Neoantigen Load"

# Ejercicio 2: Montes y Niveles de Oxigeno
# 2.1 Carga el conjunto de datos en csv: HeightsOxygen.csv
# 2.2 Describe el conjunto de datos (nº de cols, nº filas). 
# 2.3 Para las variables numericas, puedes calcular la media, la mediana y la dispersion (min, max, Q1, Q3, IQR)?
# 2.4 Selecciona todas las observaciones tomadas en el Monte Everest. Obten la varianza y la desviación estandar de los niveles de oxigeno de ese monte.
# 2.5 Genera un diagrama de cajas que represente los niveles de oxigeno medidos en cada uno de los montes.


### EXTRA DIAGRAMAS LINEALES (TIEMPO)
# Representan evolución temporal de un fenomeno. Revelan Tendencias, patrones temporales y ciclos.

# Scenario: Bacterial population growth (OD600) over 12 hours.
growth_data <- read_csv("bacterial_growth.csv")
colnames(growth_data)
summary(growth_data)

ggplot(growth_data, aes(x = Time_h, y = OD600)) +
  geom_line(color = "blue", size = 1) +
  geom_point() +
  labs(y = "Optical Density (OD600)", x = "Time (hours)", title = "Bacterial Growth Curve")


## Scenario: Presas y Depredadores en un 
eco_data <- read.csv("Lynx_vs_Hare_populations.csv")
head(eco_data) # Time sequence: 50 years, measuring every 6 months (0.5)

ggplot(eco_data, aes(x = Year)) +
  geom_line(aes(y = Hare, color = "Hare"), size = 1) 

ggplot(eco_data, aes(x = Year)) +
  geom_line(aes(y = Lynx, color = "Lynx"), size = 1, linetype = "dashed")

ggplot(eco_data, aes(x = Year)) +
  geom_line(aes(y = Hare, color = "Hare"), size = 1) +
  geom_line(aes(y = Lynx, color = "Lynx"), size = 1, linetype = "dashed") +
  scale_color_manual(values = c("Hare" = "forestgreen", "Lynx" = "darkblue")) +
  labs(title = "Predator-Prey Population Dynamics",
       subtitle = "Lynx population follows Hare population cycles",
       y = "Population (in thousands)", 
       x = "Year",
       color = "Species") +
  theme_bw()

plot_preypredator <- ggplot(eco_data, aes(x = Year)) +
  geom_line(aes(y = Hare, color = "Hare"), size = 1) +
  geom_line(aes(y = Lynx, color = "Lynx"), size = 1, linetype = "dashed") +
  scale_color_manual(values = c("Hare" = "forestgreen", "Lynx" = "darkblue")) +
  labs(title = "Predator-Prey Population Dynamics",
       subtitle = "Lynx population follows Hare population cycles",
       y = "Population (in thousands)", 
       x = "Year",
       color = "Species") +
  theme_bw()

ggsave("population_cycle_high_res.png",# This code only works for ggplot-made figures
       width = 10,   
       height = 6,   
       units = "in", # Units (in, cm, mm)
       dpi = 300)    # Resolution (300 is the standard for publications)



### EXTRA HISTOGRAMAS
# Ejercicio 3:
# Escenario: Prescripciones de analgésicos opioides. Los analgésicos opioides se prescriben 
# en Estados Unidos con una frecuencia mayor que en cualquier otra nación, 
# a pesar de que el abuso de estos medicamentos puede derivar en adicción y sobredosis fatales.
# Los CDC examinaron las prescripciones de analgésicos opioides en cada estado para determinar
# cuán variables son las tasas de prescripción en todo el país. Aquí están las tasas 
# de prescripción estatales de 2012, en número de recetas por cada 100 personas, 
# listadas en orden ascendente:

Prescription_rates <- c(
  52.0, 57.0, 59.5, 61.6, 62.9, 65.1, 66.5, 67.4, 67.9, 69.6,
  70.8, 71.2, 71.7, 72.4, 72.7, 72.8, 73.8, 74.3, 74.3, 74.7,
  76.1, 77.3, 77.5, 79.4, 82.0, 82.4, 85.1, 85.6, 85.8, 88.2,
  89.2, 89.6, 90.7, 90.8, 93.8, 94.1, 94.8, 96.6, 100.1, 101.8,
  107.0, 109.1, 115.8, 118.0, 120.3, 127.8, 128.4, 137.6, 142.8, 142.9
)

# 3.1 Crea un data.frame a partir de los datos.
# 3.2 (Busca Recursos) Genera un histograma con ggplot de las tasas de prescripción de opioides por estado
# utilizando intervalos (bins) de ancho 10, comenzando en 50.0 prescripciones por cada 100 personas.
# Construye el gráfico con la función: 
# geom_histogram(binwidth = 10, 
#    boundary = 50, 
#    fill = "steelblue", 
#    color = "white")
# 3.3. Personaliza el histograma añadiendo un subtítulo y etiquetas para los ejes x e y, modifica los colores
# 3.4. Guarda el histograma en formato png.


### EXTRA DIAGRAMAS DE TARTA (PIE CHARTS)
# Escenario: Cancer Melanoma 
data_excel <- read_excel("complete_mel_ucla_2016_clinical_data_excel2.xlsx")
ggplot(data_excel, aes(x = "", fill = `M Stage`)) +
  geom_bar(width = 1) +
  coord_polar("y", start = 0) +
  theme_void() + # Removes background grid/axes for a cleaner look
  labs(title = "M Stage Distribution")
