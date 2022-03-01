###Librerias
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(readr)
library(DT)
library(tidyverse)
library(lubridate)
library(plotly)
library(agricolae)
library(haven) 
library(corrplot)
library(corrr) 
library(highcharter)
library(ggplot2)
library(grid)
library(gridExtra)
library(GPArotation)
library(psych)
library(nFactors)
library(knitr)
library(reshape2)
library(foreign)
library(ca)
library(stringr)
library(FactoMineR)
library(bs4Dash)
library(plotrix)
library(modeest)
library(dplyr)
library(caret)
library(ade4)
library(factoextra)
library(FactoClass)
library(missMDA)
library(readxl)
library(devtools)
library(usethis)
library(ggord)
library(MASS)
library(CGPfunctions)
library(vcd)
library(DescTools)
library(statsr)
library(ca)
library(ggcorrplot)
library(plotly)
library(flextable)
library(broom)
library(rgl)
library(scatterplot3d)
###
img = tags$a(href="https://www.espol.edu.ec/",
             tags$img(src="https://i.ibb.co/GcCXysm/FCNM.png",
                      height="75", width="250"))
encuesta = tags$a(href="https://www.ecuadorencifras.gob.ec/encuesta-nacional-multiproposito-de-hogares-2018/",
                  tags$img(src="https://i.ibb.co/V2pxCsh/banner.png",
                           height="450", width="1000"))
#data
multip2018 <- read_sav("201812_multibdd_personas.sav")
multipxlsx <- read_excel("201812_multibdd_personas.xlsx")
base_logistica <- read_excel("Base Logistica.xlsx")
df1 <- read_excel("acm_bayesiana.xlsx")
#amc
df1 <- read_excel("acm_bayesiana.xlsx")
df1$Edad= factor(df1$Edad)
df1$`Estado civil`= factor(df1$`Estado civil`)
df1$`Nivel de instrucción`= factor(df1$`Nivel de instrucción`)
df1$`Frecuencia de utilizar internet`= factor(df1$`Frecuencia de utilizar internet`)
df1$`Horas dedicadas a internet`= factor(df1$`Horas dedicadas a internet`)
df2= data.frame(df1$Edad,df1$`Nivel de instrucción`,df1$`Horas dedicadas a internet`,df1$`Estado civil`)
names(df2)[1]="Edad"
names(df2)[2]="Educacion"
names(df2)[3]="Horas internet"
names(df2)[4]="Estado civil"
df2
acm <- dudi.acm(df2, nf = 10, scannf = FALSE)
graf.acm= fviz_mca_var(acm,axes = c(1,2), repel = T) + labs(title="Nube de Categorias en el primer plano")
###
datos.biplot <- read_excel("201812_multibdd_personas.xlsx")
datos.biplot[is.na(datos.biplot)] <- 0

(datos.biplot$educación <- cut(datos.biplot$educación , breaks = 4
                             , labels = c("Ninguno","Educación Básica",
                                          "Educación Secundaría","Educación Superior")))
edad <- datos.biplot$edad
estado.civil <- datos.biplot$`estado civil`
horas.de.internert <- datos.biplot$s7p5
ingresos <- datos.biplot$ingresos
idioma <- datos.biplot$idioma
educacion <- datos.biplot$educación
datos <- data.frame(edad,horas.de.internert,educacion)

set.seed(555)
ind <- sample(2,nrow(datos),
              replace = TRUE,
              prob = c(0.8,0.2))
entrenamiento <- datos[ind==1,]
prueba <- datos[ind==2,]
modelo <- lda(educacion  ~.,entrenamiento)
biPlot.1 <- ggord(modelo, entrenamiento$educacion)
#####
datos.acm <- read_sav("201812_multibdd_personas.sav")
(datos.acm$s1p12a  <- cut(datos.acm $s1p12a , breaks = 4, labels = c("Ninguno",
                                                                      "Educación Básica",
                                                                      "Educación Secundaría",
                                                                      "Educación Superior")))

datos.acm$s3p5 <- factor(datos.acm$s3p5, labels = c("obrero de gobierno",
                                                      "obrero privado",
                                                      "obrero tercerizado",
                                                      "jornalero",
                                                      "patrono",
                                                      "cuenta propia",
                                                      "trabajador del hogarno remunerado",
                                                      "trabajador no remunerado en otro hogar",
                                                      "ayudante no remunerado de asalariado",
                                                      "empleado(a) doméstico(a)"))

datos.acm$s1p2  <- factor(datos.acm$s1p2 , labels = c("Masculino","Femenino"))

datos.acm$s1p6  <- factor(datos.acm$s1p6 , labels = c("casado(a)"
                                                        ,"separado(a)"
                                                        ,"divorciado(a)"
                                                        ,"viudo(a)"
                                                        ,"union libre"
                                                        ,"soltero(a)"))

educacion <- datos.acm$s1p12a 
sexo <- datos.acm$s1p2
estado.civil <- datos.acm$s1p6 
ocupacion <- datos.acm$s3p5

cuadro.1 <- table(educacion,estado.civil)

####Arbol de decision
Estado <- c("casado(a)"
             ,"separado(a)"
             ,"divorciado(a)"
             ,"viudo(a)"
             ,"union libre"
             ,"soltero(a)")
Hombres <- c(11733,886,619,860,6864,15440)
Mujeres <- c(11908,1585,1173,2255,6919,14078)
datos.barras <- data.frame(Hombres, Mujeres)
barras <- plot_ly(datos.barras , x = ~Estado, y = ~Hombres, type = 'bar', name = 'Hombres')
barras <- barras %>% add_trace(y = ~Mujeres, name = 'Mujeres')
barras <- barras %>% layout(yaxis = list(title = 'Conteo'), barmode = 'stack')
# Test Chi
statistic <- 4.0166
p_value <- 0.0
parameter <- 15
method <- "Pearson's Chi-squared test"
tabla.Chi <- data.frame(statistic ,p_value , parameter, method)

###acm 1  y  2
datos.prueba.1 <- data.frame(educacion,estado.civil)
prueba.1 <- dudi.acm(datos.prueba.1, nf = 10, scannf = FALSE)
acm.1 <- fviz_mca_var(prueba.1,axes = c(1,2), repel = T) + labs(title="Nube de Categorias en el primer plano")

datos.prueba.2 <- data.frame(ocupacion,estado.civil)
prueba.2 <- dudi.acm(datos.prueba.2, nf = 10, scannf = FALSE)
acm.2 <- fviz_mca_var(prueba.2,axes = c(1,2), repel = T) + labs(title="Nube de Categorias en el primer plano")
###Regresion
Variables <- c("Intercepto","Edad","Educación")
Estimado <- c(0.520,-0.003,0.307)
Standar.Error <- c(0.049,0.001,0.007)
t.value <- c(10.606,-3.366,42.527)
Pr <- c("0.000***","0.0008***","0.000***")
modelo.1 <- data.frame(Variables,Estimado,Standar.Error,t.value,Pr)
###Regresion Logistica
Analfabestismo <- base_logistica$Analfabestismo
Age <- base_logistica$Edad
Ocupación <- base_logistica$Ocupación
datos.glm <- data.frame(Analfabestismo,Ocupación,Age)
modelo_glm <- glm(Analfabestismo~Age, family = "binomial")
datos.glm$Age <- as.numeric(as.character(datos.glm$Age))


modelo.2 <- ggplot(data = datos.glm , aes(x = Age, y=Analfabestismo )) + 
            geom_point(aes(color=as.factor(Analfabestismo)), shape = 1) + 
            stat_function(fun = function(x){predict(modelo_glm ,
                                          newdata = data.frame(Age = x),
                                          type = "response")}) +
            theme_bw() +
            labs(title = "Regresión logística",
            y = "Probabilidad Analfabestismo") +
            theme(legend.position = "none")

variables <- c("Intercepto","Edad")
standar.Error <- c(0.0247167,0.0005133)
z.value <- c(-96.00 ,28.09)
pr <- c("<2e-16 ***","<2e-16 ***")
tabla.2 <- data.frame(Variables,Estimado,Standar.Error,t.value,Pr)
  
#Datos Encuesta
datos.encuesta <- multipxlsx

###
# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title =img),
  dashboardSidebar(status = "purple",
                   sidebarMenu(
                     menuItem("Inicio",tabName = "inicio",icon = icon("home")),
                     menuItem("Integrantes",tabName = "integrantes",icon=icon("user")),
                     menuItem("Introduccion",tabName = "intro",icon = icon("book-open")),
                     menuItem("Base de datos",tabName = "database",icon=icon("database")),
                     menuItem("Analisis exploratorio",tabName = "exploratorio",icon = icon("globe"),
                              startExpanded = TRUE,
                              menuSubItem("Descriptivo",tabName = "descriptivo"),
                              menuSubItem("Bivariante",tabName = "bivariante"),
                              menuSubItem("Inferencial",tabName = "inferencial"),
                              menuSubItem("Multivariante",tabName = "multivariante")),
                     menuItem("Analisis Bayesiano",tabName = "bayesiano",icon = icon("chart-bar"))
                   )),
  dashboardBody(tabItems(
    tabItem("inicio",
            fluidRow(
              bs4Jumbotron(encuesta,
                           title = "Encuesta Nacional Multiproposito de Hogares (Seguimiento al Plan Nacional de Desarrollo), Diciembre 2018",
                           lead = "La Encuesta Multiproposito constituye la fuente oficial de informacion estadistica que permite hacer seguimiento a los indicadores de las metas planteadas en el Plan Nacional de Desarrollo, y demas agendas de desarrollo nacional e internacional.
                           En este espacio, el INEC pone a disposicion de la ciudadania, la documentacion tecnica relacionada y los principales resultados de la encuesta, en una primera entrega.",
                           status = "info",
                           btnName = "Mas informacion",
                           href = "https://www.ecuadorencifras.gob.ec/encuesta-nacional-multiproposito-de-hogares-2018/"
              )
            )
    ),
    tabItem("intro",
            fluidPage(
              fluidRow(column(12,
                              h1("Introduccion"),
                              h5("La Encuesta Nacional Multiproposito de Hogares es la operacion estadistica utilizada para obtener informacion estadistica que permita hacer seguimiento a los indicadores de las metas del Plan Nacional de Desarrollo, y demas agendas de desarrollo nacional e internacional.El objetivo de este proyecto sera hacer una interpretacion estadistica completa de los datos, tanto de un punto de vista Bayesiano como desde la estadistica Clasica. Se pretende investigar el porcentaje de utilizacion de internet, edad media, sexo e inferencias bayesianas de la poblacion encuestada , asi como otras variables de interes general acerca de la muestra de 12060 viviendas."),
                              fluidRow(column(6,
                                              h1("Objetivo General:"),
                                              h5("Realizar un estudio estadistico de la Encuesta Nacional Multiproposito de Hogares por medio del enfoque clasico y bayesiano.")),
                                       column(6,
                                              h1("Objetivos especificos:"),
                                              h5("+ Presentar las variables cuantitativas y cualitativas que se utilizaran en el estudio"),
                                              h5("+ Realizar un analisis descriptivo de las variables, utilizando graficos para mejorar la visualizacion de los datos.")))))
            )
    ),
    tabItem("integrantes",
            fluidRow(
              userBox(status="purple",
                      width=12,
                      gradient=TRUE,
                      background = "gray",
                      boxToolSize = "sm",
                      "Estudiante de Ing. Estadistica",
                      footer = "sfpoveda@espol.edu.ec",
                      title = userDescription(
                        title="Sebastian Poveda",
                        backgroundImage = "https://i.ibb.co/LQvx5ZT/ESPOL.jpg",
                        type = "2",
                        image = "https://i.ibb.co/m9qmf1B/Sebastian-foto-carnet.jpg")),
              userBox(status="purple",
                      width=12,
                      gradient=TRUE,
                      background = "gray",
                      boxToolSize = "sm",
                      "Estudiante de Ing. Estadistica",
                      footer = "pedjgoya@espol.edu.ec",
                      title = userDescription(
                        title="Pedro Goya",
                        backgroundImage = "https://i.ibb.co/LQvx5ZT/ESPOL.jpg",
                        type = "2",
                        image = "https://i.ibb.co/m06N1XM/pedro.jpg")),
              userBox(status="purple",
                      width=12,
                      gradient=TRUE,
                      background = "gray",
                      boxToolSize = "sm",
                      "Estudiante de Ing. Estadistica",
                      footer = "jhchirib@espol.edu.ec",
                      title = userDescription(
                        title="Josias Chiriboga",
                        backgroundImage = "https://i.ibb.co/LQvx5ZT/ESPOL.jpg",
                        type = "2",
                        image = "https://i.ibb.co/8rc8qNg/josias.jpg")),
            )
    ),
    tabItem("database",
            fluidPage(
              h1("Encuesta multiproposito"), dataTableOutput("datos.encuesta")
            )
    ),
    tabItem(tabName = "descriptivo",
            fluidRow(valueBoxOutput("vbox_unicode"),valueBoxOutput("vbox1"),valueBoxOutput("vbox2"),valueBoxOutput("vbox3")),
            fluidRow(
              tabBox(width=12,id="explo1",title = "",
                     tabPanel("Edad",highchartOutput("edad",height = 400)),
                     tabPanel("Sexo",highchartOutput("sexo",height = 400)),
                     tabPanel("Horas de internet",highchartOutput("horasint",height = 400),
                              fluidRow(highchartOutput("cajahoras",height = 400))),
                     tabPanel("Sexo segun el Estado Civil", plotlyOutput("barras",height = 540))
              )
            )
    ),
    tabItem(tabName = "multivariante",
            fluidRow(
            tabBox(width=12,id="explo2",title = "",
                   tabPanel("Análisis de Coordenadas Multiples",plotOutput("graf.acm",height = 500)),
                   tabPanel("Análisis de Correspondencia Bivariante para las variables Educación y Estado Civil",plotOutput("acm.1",height = 500)),
                   tabPanel("Análisis de Correspondencia Bivariante para las variables Educación y Ocupación",plotOutput("acm.2",height = 500))
            )
            )
            ),

    tabItem("bivariante",
            fluidRow(
              tabBox(width = 12, id = "bi3" , title = "",
                     tabPanel("Prueba Chi Cuadrado para la categoría Educación y Estado civil", plotOutput("mosaico",height = 500)),
                     fluidRow(column(6,tableOutput("cuadro.1")),
                              column(6,tableOutput("tabla.Chi"))))
            )
    ),
    tabItem("inferencial",
            fluidRow(
              tabBox(width=12,id="explo4",title = "",
                     tabPanel("Dispersión en el Espacio tridiminesional para el modelo multivariante aditivo",plotOutput("nube.puntos",height = 500),
                     fluidRow(column(12,tableOutput("modelo.1")))),
                              
                     tabPanel("Modelo Logístico",plotOutput("modelo.2"),
                     fluidRow(column(12,tableOutput("tabla.2"))))
                
              )
            )
      
    ),
    
    tabItem("bayesiano",
            fluidRow(
              tabBox(width=12,id="bayes",title = "",
                     tabPanel("Análisis Discriminante Lineal",plotOutput("biPlot.1",height = 540)),
                     
                     tabPanel("Intervalo de credibilidad",plotOutput("bayesci",height = 500),
                              fluidRow(column(6,tableOutput("tablaci")),
                                       column(6,valueBoxOutput("vboxbayes")))),
                     
                     tabPanel("Inferencia para 2 medias",plotOutput("bayesmedias",height = 500),
                              fluidRow(column(6,plotOutput("cajasbayes")),
                                       column(6,tableOutput("cajabayes"))))
                     
                     
                     
                     )
            )
    )
  ))
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$edad=renderHighchart({
    edad=as.numeric(multip2018$s1p3)
    hist1=hist(edad,breaks=10,plot=FALSE)
    hchart(hist1,showInLegend=FALSE)%>%
      hc_title(text="Histograma de Edades")%>%
      hc_xAxis(title=list(text="Edad en anos"))%>%
      hc_yAxis(title=list(text="Frecuencia absoluta"))%>%
      hc_subtitle(text="ENEMDU-Rango de edades de personas encuestas en el ano 2018")%>%
      hc_credits(text="INEC-Instituto Ecuatoriano de Estadistica y Censo")%>% 
      hc_add_theme(hc_theme_elementary())
  })
  output$sexo=renderHighchart({
    sexo=as.factor(multip2018$s1p2)
    hchart(sexo,"column",showInLegend=FALSE)%>%
      hc_title(text="Diagrama de barras de sexo del encuestado")%>%
      hc_xAxis(title=list(text="Sexo"))%>%
      hc_yAxis(title=list(text="Frecuencia absoluta"))%>%hc_add_theme(hc_theme_elementary())
  })
  output$horasint= renderHighchart({
    inthours=as.numeric(multip2018$s7p5)
    hist2=hist(inthours,breaks=50,plot=FALSE)
    hchart(hist2,showInLegend=FALSE)%>%
      hc_title(text="Histograma de horas de internet totales dedicadas")%>%
      hc_xAxis(title=list(text="Tiempo en horas"))%>%
      hc_yAxis(title=list(text="Frecuencia absoluta"))%>%
      hc_subtitle(text="ENEMDU-Total de horas dedicadas a internet por encuestados en el 2018")%>%
      hc_credits(text="INEC-Instituto Ecuatoriano de Estadistica y Censo")%>% 
      hc_add_theme(hc_theme_elementary())
  })
  output$cajahoras=renderHighchart({
    inthours=as.numeric(multip2018$s7p5)
    hcboxplot(x = inthours,
              name = "Length", color = "#2980b9") %>%
      hc_add_theme(hc_theme_economist())
  })
  
  output$barras = renderPlotly(barras)
  output$graf.acm = renderPlot(graf.acm)
  
  output$acm.1 = renderPlot(acm.1)
  output$acm.2 = renderPlot(acm.2)
  
  output$datos.encuesta <- renderDataTable(datos.encuesta)
  output$biPlot.1 = renderPlot(biPlot.1)
  output$tabla = renderImage({img(src = "tabla.png")})
  output$bayesmedias= renderPlot({
    bayesinf= bayes_inference(y = s7p5, x = s4p13, data = multip2018, statistic = "mean",type = "ci", mu_0 = 0)
    bayesinf$plot
  })
  output$cajasbayes= renderPlot({
    boxplot(multip2018$s7p5~multip2018$s4p13,horizontal = TRUE,xlab="Horas dedicadas a internet",ylab = NULL)
  })
  output$cajabayes= renderTable({
    bayesinf= bayes_inference(y = s7p5, x = s4p13, data = multip2018, statistic = "mean",type = "ci", mu_0 = 0)
    bayesinf$summary
  })
  output$vboxbayes= renderValueBox({
    valueBox(
      value = h2("2.3764, 2.4385 horas"),
      subtitle = tagList("Promedio de horas de internet utilizadas por los encuestados"),
      icon=icon("user-clock"),
      color="purple",
      href = NULL,
      width = 12
    )
  })
  output$bayesci=renderPlot({
    bayesmedia= bayes_inference(y=s7p5,data=multip2018,statistic="mean",type="ci",cred_level = 0.99 ,mu_0 = 0)
    bayesmedia$plot
  })
  output$tablaci=renderTable({
    bayesmedia= bayes_inference(y=s7p5,data=multip2018,statistic="mean",type="ci",cred_level = 0.99 ,mu_0 = 0)
    bayesmedia$summary
  })
  output$bayesmedias= renderPlot({
    bayesinf= bayes_inference(y = s7p5, x = s4p13, data = multip2018, statistic = "mean",type = "ci", mu_0 = 0)
    bayesinf$plot
  })
  output$cajasbayes= renderPlot({
    boxplot(multip2018$s7p5~multip2018$s4p13,horizontal = TRUE,xlab="Horas dedicadas a internet",ylab = NULL)
  })
  output$cajabayes= renderTable({
    bayesinf= bayes_inference(y = s7p5, x = s4p13, data = multip2018, statistic = "mean",type = "ci", mu_0 = 0)
    bayesinf$summary
  })
  output$mosaico = renderPlot({
    Test.Chi <- chisq.test(cuadro.1 , correct = FALSE)
    
    corrplot::corrplot(Test.Chi$residuals,is.corr = FALSE,method = "color",addgrid.col = F, tl.col = "black")
  })
  output$cuadro.1=renderTable(cuadro.1)
  output$tabla.Chi = renderTable(tabla.Chi)
  output$nube.puntos = renderPlot({
    horas.de.internet <- multip2018$s7p5
    Sexo <- multip2018$s1p2
    Edad <- multip2018$s1p3
    Educacion <- multip2018$s1p12a
    
    datos.modelo <- data.frame(Sexo,Edad,Educacion,horas.de.internet)
   
    x <- Edad
    y <- Educacion
    z <- horas.de.internet
    scatterplot3d(x,y,z, pch = 18 ,color = "blue")
  })
  output$modelo.1=renderTable(modelo.1)
  output$modelo.2=renderPlot(modelo.2)
  output$tabla.2=renderTable(tabla.2)
}

# Run the application 
shinyApp(ui = ui, server = server)

