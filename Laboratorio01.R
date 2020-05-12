#############################################################################

#######################################################
# Espacio Multivariante :  Datos
#######################################################

# Cargar el archivo CSV loan_prediction-II.CSV

library(data.table)
library(dplyr)
library(ggplot2)
library()

loanPR <-fread("loan_prediction-II.csv",
                sep = ";",
                header=T, 
                verbose =FALSE, 
                stringsAsFactors=TRUE,
                showProgress =TRUE)

# Revisar el archivo

str(loanPR)

# Definir como n.X y p.X el numero de estados y el numero de variables.
dim(loanPR)

n.X <- nrow(loanPR) # Filas
n.X
p.X <- ncol(loanPR) # Columnas
p.X

loanPR <- mutate(loanPR, loan_granted = ifelse(loanPR$Loan_Status == "Y", 1 , 0))
loanPR1 <- as.data.frame(loanPR[,c(7:10)])
loanPR2 <- as.data.frame(loanPR[,c(1:15)])

#===============================================================
# ----------- VARIABLES CUALITATIVAS (BARRAS) ------------------

#windows()
#par(mfrow=c(1,2))
# sapply(seq(2,3,1),function(j)  -- loanPR2[,j]

# - Genero
ggplot(loanPR2, aes(x=Gender,fill=Loan_Status ) ) +
  geom_bar(position=position_fill()) +
  theme_bw() +
  labs(title = "Aceptacion de prestamo", 
       x = "Genero",
       y = "Proporcion") + 
  scale_fill_manual(values=c("darkolivegreen3", "firebrick2"))

# -- Casado
ggplot(loanPR2, aes(x=Married,fill=Loan_Status ) ) +
  geom_bar(position=position_fill()) +
  theme_bw() +
  labs(title = "Aceptacion de prestamo", 
       x = "Casado",
       y = "Proporcion") + 
  scale_fill_manual(values=c("darkolivegreen3", "firebrick2")) 

# -- Cantidad de dependientes
ggplot(loanPR2, aes(x=Dependents,fill=Loan_Status ) ) +
  geom_bar(position=position_fill()) +
  theme_bw() +
  labs(title = "Aceptacion de prestamo", 
       x = "Cantidad de dependientes",
       y = "Proporcion") + 
  scale_fill_manual(values=c("darkolivegreen3", "firebrick2")) 

# -- Education
ggplot(loanPR2, aes(x=Education,fill=Loan_Status ) ) +
  geom_bar(position=position_fill()) +
  theme_bw() +
  labs(title = "Aceptacion de prestamo", 
       x = "Educacion",
       y = "Proporcion") + 
  scale_fill_manual(values=c("darkolivegreen3", "firebrick2")) 

# -- Tiene Empleo
ggplot(loanPR2, aes(x=Self_Employed,fill=Loan_Status ) ) +
  geom_bar(position=position_fill()) +
  theme_bw() +
  labs(title = "Aceptacion de prestamo", 
       x = "Tiene Empleo",
       y = "Proporcion") + 
  scale_fill_manual(values=c("darkolivegreen3", "firebrick2")) 

# -- Historia de credito
ggplot(loanPR2, aes(x=Credit_History,fill=Loan_Status ) ) +
  geom_bar(position=position_fill()) +
  theme_bw() +
  labs(title = "Aceptacion de prestamo", 
       x = "Historia de credito",
       y = "Proporcion") + 
  scale_fill_manual(values=c("darkolivegreen3", "firebrick2")) 

# -- Ubicacion residencia
ggplot(loanPR2, aes(x=Property_Area,fill=Loan_Status ) ) +
  geom_bar(position=position_fill()) +
  theme_bw() +
  labs(title = "Aceptacion de prestamo", 
       x = "Ubicacion residencia",
       y = "Proporcion") + 
  scale_fill_manual(values=c("darkolivegreen3", "firebrick2")) 

# -- Nacionalidad
ggplot(loanPR2, aes(x=Nacionality,fill=Loan_Status ) ) +
  geom_bar(position=position_fill()) +
  theme_bw() +
  labs(title = "Aceptacion de prestamo", 
       x = "Nacionalidad",
       y = "Proporcion") + 
  scale_fill_manual(values=c("darkolivegreen3", "firebrick2")) 

# ----------- VARIABLES CUALITATIVAS (PIE) ------------------

Credit_History_1 <- table(loanPR2$Credit_History)
Nacionality_1 <- table(loanPR2$Nacionality)

pct1 <- round(prop.table(table(loanPR2$Credit_History))*100)
pct2 <- prop.table(table(loanPR2$Nacionality))*100

etiq1  <- c("NO","SI")
etiq2  <- c("Foraneo","No Foreneo")
etiq12 <- paste(etiq1, " ", pct1, "%")   # Concatena cadenas
etiq22 <- paste(etiq2, " ", pct2, "%")   # Concatena cadenas

pie(Credit_History_1, 
    labels=etiq12, 
    col=rainbow(length(etiq1)), 
    main="Historia de Credito")

pie(Nacionality_1, 
    labels=etiq22, 
    col=rainbow(length(etiq2)), 
    main="Nacionalidad")



#=================================================================
# --------------- VARIABLES CUANTITATIVAS (DISPERSION) -----------

library(VIM)
library(ggplot2)

#---Identificacion de missing -----
which(colSums(is.na(loanPR2))!=0)

#---Eliminando missing -----
loanPR3=na.omit(loanPR2)

#---Graficando histograma con proporciones ----------

#---Ingreso Dependiente ----
ggplot(loanPR3, aes(x=ApplicantIncome)) +
  geom_histogram(bins=20, aes(fill=Loan_Status), position="fill", alpha=0.4) +
  labs(x= "Ingreso Dependientes", y="Solicitantes", fill="Prestamo") +  # títulos de ejes y leyenda
  scale_fill_discrete(labels=c("NO","SI"))     # títulos claves leyenda

#---Ingreso Independientes ----
ggplot(loanPR3, aes(x=CoapplicantIncome)) +
  geom_histogram(bins=20, aes(fill=Loan_Status), position="fill", alpha=0.4) +
  labs(x= "Ingreso Independientes", y="Solicitantes", fill="Prestamo") +  # títulos de ejes y leyenda
  scale_fill_discrete(labels=c("NO","SI"))     # títulos claves leyenda

#---Monto Prestamo ----
ggplot(loanPR3, aes(x=LoanAmount)) +
  geom_histogram(bins=20, aes(fill=Loan_Status), position="fill", alpha=0.4) +
  labs(x= "Monto Prestamo", y="Solicitantes", fill="Prestamo") +  # títulos de ejes y leyenda
  scale_fill_discrete(labels=c("NO","SI"))     # títulos claves leyenda

#---Termino Prestamo ----
ggplot(loanPR3, aes(x=Loan_Amount_Term)) +
  geom_histogram(bins=20, aes(fill=Loan_Status), position="fill", alpha=0.4) +
  labs(x= "Termino Prestamo", y="Solicitantes", fill="Prestamo") +  # títulos de ejes y leyenda
  scale_fill_discrete(labels=c("NO","SI"))     # títulos claves leyenda

#--Relacion de todas las variables

par(mfrow=c(1,1))
pairs(loanPR3,pch=19,col="blue")

#---Zoom de relacion monto prestamo con Ingreso ---

# loanPR4 <- as.data.frame(filter(loanPR3, ApplicantIncome > 1 & Loan_Status == "y"))

windows()
attach(loanPR3)
par(mfrow=c(1,1)) # Volver a definir una ventana normal
plot(LoanAmount,ApplicantIncome ,pch=19,col="blue",xlab="Monto Prestamo",ylab="Ingreso Dependientes")






#--- PRUEBAS -------------

Married_2 <- table(loanPR2$Loan_Status, loanPR2$Married)
barplot(Married_2)


ejemplo2 <- table(loanPR2$Gender, mean(loanPR2$loan_granted))
barplot(ejemplo2)

Gender_1 <- select(loanPR2, Gender,loan_granted) %>% 
  group_by(Gender) %>% 
  summarise(media = mean(loan_granted), total = n())

ejemplo3 <- table(Gender_1$Gender, Gender_1$media)

barplot(ejemplo3)

plot (Gender_1$Gender, Gender_1$media)

ggplot(loanPR2, aes(Gender)) + geom_bar(aes(loan_granted), stat = "summary", fun.y = "mean") + coord_flip()

ggplot(data=loanPR2, aes(x=Gender))
ggplot(data=loanPR2, aes(x=loan_granted)) + geom_bar(stat="count")
ggplot(datos, aes(Gender)) + geom_bar()   # es equivalente
ggplot(datos) + geom_bar(aes(loan_granted))     # es equivalente


factorx1 <- factor(cut(loanPR2$ApplicantIncome, breaks=nclass.Sturges(loanPR2$ApplicantIncome),right=TRUE))
xout <- as.data.frame(table(factorx1))
colnames(xout)<-c("Media")
xout <- transform(xout, 
                  ME = mean_se(loanPR2$loan_granted)
)
xout