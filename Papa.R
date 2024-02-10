setwd("C:/Users/david/OneDrive/Escritorio/Personal/R/Data_science")

library(readxl)
mccain_savia <- read_excel("C:/Users/david/OneDrive/Escritorio/Personal/R/mccain_savia.xlsx")
head(mccain_savia)

names(mccain_savia)

library(ggplot2)

tema <-   theme(legend.position="bottom",panel.background = element_rect(fill = NA),panel.grid.major.y = element_line(colour = "grey90"),legend.key = element_rect(fill = "white"),axis.line = element_line(colour = "grey90"),legend.title = element_text(colour = "white"))

library(dplyr)

crear_grafico <- function(datos, variedad, variable) {
  nombre_variable <- as.character(substitute(variable))  # Obtener el nombre de la variable
  
  datos %>%
    filter(Variedad == variedad) %>%
    group_by(DDS, Tratamiento) %>%
    summarise(Variable = mean({{variable}}, na.rm = TRUE),
              DDS = DDS,
              TRATAMIENTO = Tratamiento,
              SE = sd({{variable}}, na.rm = TRUE) / sqrt(sum({{variable}}))) %>%
    ggplot(aes(x = DDS, y = Variable, group = TRATAMIENTO, colour = TRATAMIENTO, shape = TRATAMIENTO)) +
    geom_errorbar(aes(ymin = Variable - 2 * SE, ymax = Variable + 2 * SE), width = 1) +
    geom_line(size = 0.5) +
    scale_y_continuous(breaks = seq(0, 10000, 500)) +
    scale_x_continuous(breaks = seq(0, 150, 15)) +
    labs(title = paste(nombre_variable, "en savia - Variedad", variedad),
         subtitle = paste("Representación gráfica de medias para la variedad", variedad),
         caption = paste("Variedad", variedad),
         x = "DDS", y = paste("Contenido de", nombre_variable, "(ppm)")) +
    geom_point(size = 2, fill = "white") +
    scale_color_manual(values = c("seagreen2", "cadetblue3", "cadetblue4", "cadetblue2")) +
    tema
}

grafico_variedad_1_NO3 <- crear_grafico(mccain_savia, "CIP 1", NO3)
grafico_variedad_1_K <- crear_grafico(mccain_savia, "CIP 1", K)
grafico_variedad_1_Ca <- crear_grafico(mccain_savia, "CIP 1", Ca)
grafico_variedad_39_NO3 <- crear_grafico(mccain_savia, "CIP 39", NO3)
grafico_variedad_39_K <- crear_grafico(mccain_savia, "CIP 39", K)
grafico_variedad_39_Ca <- crear_grafico(mccain_savia, "CIP 39", Ca)
grafico_variedad_102_NO3 <- crear_grafico(mccain_savia, "CIP 102", NO3)
grafico_variedad_102_K <- crear_grafico(mccain_savia, "CIP 102", K)
grafico_variedad_102_Ca <- crear_grafico(mccain_savia, "CIP 102", Ca)

grafico_variedad_1_NO3
grafico_variedad_1_K
grafico_variedad_1_Ca
grafico_variedad_39_NO3
grafico_variedad_39_K
grafico_variedad_39_Ca
grafico_variedad_102_NO3
grafico_variedad_102_K
grafico_variedad_102_Ca

library(readxl)
datos_masa_seca <- read_excel("C:/Users/david/OneDrive/Escritorio/Personal/R/masa_seca.xlsx")
head(datos_masa_seca)

names(datos_masa_seca)

library(dplyr)

masa_seca_1 <- datos_masa_seca %>% 
  filter(VARIEDAD=="1")%>%
  group_by(DDS,TRATAMIENTO.)%>%
  summarise(PESO_TOTAL=mean(PESO_TOTAL,na.rm = T),
            DDS=DDS,
            TRATAMIENTO=TRATAMIENTO.)


ggplot() + geom_point(data=masa_seca_1, aes(x=DDS, y = PESO_TOTAL, group = TRATAMIENTO., colour = TRATAMIENTO.)) +
  xlab("DDS") + 
  ylab("PESO TOTAL") + 
  ggtitle("") +
  tema

Gompertz <- function(x, y0, ymax, k, lag){
  result<- y0 + (ymax)*exp(-exp(k*(lag-x)/(ymax-y0) + 1))
  return(result)
}

masa_seca_100_1 <- datos_masa_seca %>%
  filter(VARIEDAD =="1",TRATAMIENTO.=="100% Fertilización")%>%
  group_by(DDS, TRATAMIENTO.)%>%
  summarise(PESO_TOTAL_100_1=PESO_TOTAL,
            DDS_100_1=DDS,
            TRATAMIENTO=TRATAMIENTO.)

Gomp_100_1 <- nls(PESO_TOTAL_100_1 ~ Gompertz(DDS_100_1, y0, ymax, k, lag),
                  data=masa_seca_100_1,
                  start = list(y0=15, ymax=2500, k=10, lag=1))

masa_seca_66_1 <- datos_masa_seca %>%
  filter(VARIEDAD =="1",TRATAMIENTO.=="66% Fertilización")%>%
  group_by(DDS, TRATAMIENTO.)%>%
  summarise(PESO_TOTAL_66_1=PESO_TOTAL,
            DDS_66_1=DDS,
            TRATAMIENTO=TRATAMIENTO.)

Gomp_66_1 <- nls(PESO_TOTAL_66_1 ~ Gompertz(DDS_66_1, y0, ymax, k, lag),
                 data=masa_seca_66_1,
                 start = list(y0=31, ymax=2100, k=15, lag=1))

masa_seca_33_1 <- datos_masa_seca %>%
  filter(VARIEDAD =="1",TRATAMIENTO.=="33% Fertilización")%>%
  group_by(DDS, TRATAMIENTO.)%>%
  summarise(PESO_TOTAL_33_1=PESO_TOTAL,
            DDS_33_1=DDS,
            TRATAMIENTO=TRATAMIENTO.)

Gomp_33_1 <- nls(PESO_TOTAL_33_1 ~ Gompertz(DDS_33_1, y0, ymax, k, lag),
                 data=masa_seca_33_1,
                 start = list(y0=21, ymax=2200, k=15, lag=1))

masa_seca_0_1 <- datos_masa_seca %>%
  filter(VARIEDAD =="1",TRATAMIENTO.=="0% Fertilización")%>%
  group_by(DDS, TRATAMIENTO.)%>%
  summarise(PESO_TOTAL_0_1=PESO_TOTAL,
            DDS_0_1=DDS,
            TRATAMIENTO=TRATAMIENTO.)

Gomp_0_1 <- nls(PESO_TOTAL_0_1 ~ Gompertz(DDS_0_1, y0, ymax, k, lag),
                data=masa_seca_0_1,
                start = list(y0=-7, ymax=1700, k=10, lag=1))

coefs_100_1 <- coef(Gomp_100_1)
y0_100_1 = coefs_100_1[1]
ymax_100_1=coefs_100_1[2]
k_100_1=coefs_100_1[3]
lag_100_1=coefs_100_1[4]

coefs_100_1

coefs_66_1 <- coef(Gomp_66_1)
y0_66_1 = coefs_66_1[1]
ymax_66_1=coefs_66_1[2]
k_66_1=coefs_66_1[3]
lag_66_1=coefs_66_1[4]

coefs_66_1

coefs_33_1 <- coef(Gomp_33_1)
y0_33_1 = coefs_33_1[1]
ymax_33_1=coefs_33_1[2]
k_33_1=coefs_33_1[3]
lag_33_1=coefs_33_1[4]

coefs_33_1

coefs_0_1 <- coef(Gomp_0_1)
y0_0_1 = coefs_0_1[1]
ymax_0_1= coefs_0_1[2]
k_0_1 = coefs_0_1[3]
lag_0_1 = coefs_0_1[4]

coefs_0_1

summary(Gomp_100_1)
summary(Gomp_66_1)
summary(Gomp_33_1)
summary(Gomp_0_1)

DDS_LEVELS <- seq(0,150,by=1)

par(mfrow=c(2,2))

pred <- predict(Gomp_100_1, list(x=DDS_LEVELS))
plot(PESO_TOTAL_100_1~DDS_100_1, data=masa_seca_100_1, xlim=c(0,150), ylim=c(0,2600),
     xlab="DDS", ylab="Peso total (g)")
lines(DDS_LEVELS, Gompertz(DDS_LEVELS, y0_100_1, ymax_100_1, k_100_1, lag_100_1),
      lty=1, col="cadetblue3", lwd = 2)

pred <- predict(Gomp_66_1, list(x=DDS_LEVELS))
plot(PESO_TOTAL_66_1~DDS_66_1, data=masa_seca_66_1, xlim=c(0,150), ylim=c(0,2600),
     xlab="DDS", ylab="Peso total (g)")
lines(DDS_LEVELS, Gompertz(DDS_LEVELS, y0_66_1, ymax_66_1, k_66_1, lag_66_1),
      lty=1, col="cadetblue2", lwd = 2)

pred <- predict(Gomp_33_1, list(x=DDS_LEVELS))
plot(PESO_TOTAL_33_1~DDS_33_1, data=masa_seca_33_1, xlim=c(0,150), ylim=c(0,2600),
     xlab="DDS", ylab="Peso total (g)")
lines(DDS_LEVELS, Gompertz(DDS_LEVELS, y0_33_1, ymax_33_1, k_33_1, lag_33_1),
      lty=1, col="cadetblue4", lwd = 2)

pred <- predict(Gomp_0_1, list(x=DDS_LEVELS))
plot(PESO_TOTAL_0_1~DDS_0_1, data=masa_seca_0_1, xlim=c(0,150), ylim=c(0,2600),
     xlab="DDS", ylab="Peso total (g)")
lines(DDS_LEVELS, Gompertz(DDS_LEVELS, y0_0_1, ymax_0_1, k_0_1, lag_0_1),
      lty=1, col="seagreen2", lwd = 2)

par(mfrow=c(1,1))

plot(PESO_TOTAL~DDS, data=masa_seca_1, xlim=c(0,150), ylim=c(0,2600),
     xlab="DDS", ylab="Peso total (g)")
lines(DDS_LEVELS, Gompertz(DDS_LEVELS, y0_100_1, ymax_100_1, k_100_1, lag_100_1),
      lty=1, col="cadetblue3", lwd = 2)
lines(DDS_LEVELS, Gompertz(DDS_LEVELS, y0_66_1, ymax_66_1, k_66_1, lag_66_1),
      lty=1, col="cadetblue2", lwd = 2)
lines(DDS_LEVELS, Gompertz(DDS_LEVELS, y0_33_1, ymax_33_1, k_33_1, lag_33_1),
      lty=1, col="cadetblue4", lwd = 2)
lines(DDS_LEVELS, Gompertz(DDS_LEVELS, y0_0_1, ymax_0_1, k_0_1, lag_0_1),
      lty=1, col="seagreen2", lwd = 2)
legend("topleft",legend=c("100% Fertilización", "66% Fertilización", "33% Fertilización", "0% Fertilización"), lwd = 3, col = c("cadetblue3","cadetblue2","cadetblue4","seagreen2"))

pesos_savia_100_1 <- Gompertz(c(36,45,53,61,70,78,84,91,98,105,115),y0_100_1,ymax_100_1,k_100_1,lag_100_1)
DDS_100_1 <- c(36,45,53,61,70,78,84,91,98,105,115)
TRATAMIENTO_100_1 <- rep("100% Fertilización",11)
Tabla_pesos_savia_100_1 <- data.frame(pesos_savia_100_1,DDS_100_1,TRATAMIENTO_100_1)
Tabla_pesos_savia_100_1

pesos_savia_66_1 <- Gompertz(c(36,45,53,61,70,78,84,91,98,105,115),y0_66_1,ymax_66_1,k_66_1,lag_66_1)
DDS_66_1 <- c(36,45,53,61,70,78,84,91,98,105,115)
TRATAMIENTO_66_1 <- rep("66% Fertilización",11)
Tabla_pesos_savia_66_1 <- data.frame(pesos_savia_66_1,DDS_66_1,TRATAMIENTO_66_1)
Tabla_pesos_savia_66_1

pesos_savia_33_1 <- Gompertz(c(36,45,53,61,70,78,84,91,98,105,115),y0_33_1,ymax_33_1,k_33_1,lag_33_1)
DDS_33_1 <- c(36,45,53,61,70,78,84,91,98,105,115)
TRATAMIENTO_33_1 <- rep("33% Fertilización",11)
Tabla_pesos_savia_33_1 <- data.frame(pesos_savia_33_1,DDS_33_1,TRATAMIENTO_33_1)
Tabla_pesos_savia_33_1

pesos_savia_0_1 <- Gompertz(c(36,45,53,61,70,78,84,91,98,105,115),y0_0_1,ymax_0_1,k_0_1,lag_0_1)
DDS_0_1 <- c(36,45,53,61,70,78,84,91,98,105,115)
TRATAMIENTO_0_1 <- rep("0% Fertilización",11)
Tabla_pesos_savia_0_1 <- data.frame(pesos_savia_0_1,DDS_0_1,TRATAMIENTO_0_1)
Tabla_pesos_savia_0_1

mccain_savia_0_1 <- mccain_savia %>%
  filter(Variedad =="CIP 1", Tratamiento =="0% Fertilización")%>%
  group_by(DDS,Variedad,Tratamiento)%>%
  summarise(NO3 = mean(NO3, na.rm=T))

Tabla_0_1 <- cbind(mccain_savia_0_1$Variedad,Tabla_pesos_savia_0_1, mccain_savia_0_1$NO3)
colnames(Tabla_0_1) <- c('Variedad','Peso','DDS','Tratamiento','NO3')

mccain_savia_33_1 <- mccain_savia %>%
  filter(Variedad =="CIP 1", Tratamiento =="33% Fertilización")%>%
  group_by(DDS,Variedad,Tratamiento)%>%
  summarise(NO3 = mean(NO3, na.rm=T))

Tabla_33_1 <- cbind(mccain_savia_33_1$Variedad,Tabla_pesos_savia_33_1, mccain_savia_33_1$NO3)
colnames(Tabla_33_1) <- c('Variedad','Peso','DDS','Tratamiento','NO3')

mccain_savia_66_1 <- mccain_savia %>%
  filter(Variedad =="CIP 1", Tratamiento =="66% Fertilización")%>%
  group_by(DDS,Variedad,Tratamiento)%>%
  summarise(NO3 = mean(NO3, na.rm=T))

Tabla_66_1 <- cbind(mccain_savia_66_1$Variedad,Tabla_pesos_savia_66_1, mccain_savia_66_1$NO3)
colnames(Tabla_66_1) <- c('Variedad','Peso','DDS','Tratamiento','NO3')

mccain_savia_100_1 <- mccain_savia %>%
  filter(Variedad =="CIP 1", Tratamiento =="100% Fertilización")%>%
  group_by(DDS,Variedad,Tratamiento)%>%
  summarise(NO3 = mean(NO3, na.rm=T))

Tabla_100_1 <- cbind(mccain_savia_100_1$Variedad,Tabla_pesos_savia_100_1, mccain_savia_100_1$NO3)
colnames(Tabla_100_1) <- c('Variedad','Peso','DDS','Tratamiento','NO3')

Tabla_1 <- rbind(Tabla_0_1,Tabla_33_1, Tabla_66_1, Tabla_100_1)

head(Tabla_1)

mccain_savia_0_1 <- mccain_savia %>%
  filter(Variedad =="CIP 1", Tratamiento =="0% Fertilización")%>%
  group_by(DDS,Variedad,Tratamiento)%>%
  summarise(Ca = mean(Ca, na.rm=T))

Tabla_0_1 <- cbind(mccain_savia_0_1$Variedad,Tabla_pesos_savia_0_1, mccain_savia_0_1$Ca)
colnames(Tabla_0_1) <- c('Variedad','Peso','DDS','Tratamiento','Ca')

mccain_savia_33_1 <- mccain_savia %>%
  filter(Variedad =="CIP 1", Tratamiento =="33% Fertilización")%>%
  group_by(DDS,Variedad,Tratamiento)%>%
  summarise(Ca = mean(Ca, na.rm=T))

Tabla_33_1 <- cbind(mccain_savia_33_1$Variedad,Tabla_pesos_savia_33_1, mccain_savia_33_1$Ca)
colnames(Tabla_33_1) <- c('Variedad','Peso','DDS','Tratamiento','Ca')

mccain_savia_66_1 <- mccain_savia %>%
  filter(Variedad =="CIP 1", Tratamiento =="66% Fertilización")%>%
  group_by(DDS,Variedad,Tratamiento)%>%
  summarise(Ca = mean(Ca, na.rm=T))

Tabla_66_1 <- cbind(mccain_savia_66_1$Variedad,Tabla_pesos_savia_66_1, mccain_savia_66_1$Ca)
colnames(Tabla_66_1) <- c('Variedad','Peso','DDS','Tratamiento','Ca')

mccain_savia_100_1 <- mccain_savia %>%
  filter(Variedad =="CIP 1", Tratamiento =="100% Fertilización")%>%
  group_by(DDS,Variedad,Tratamiento)%>%
  summarise(Ca = mean(Ca, na.rm=T))

Tabla_100_1 <- cbind(mccain_savia_100_1$Variedad,Tabla_pesos_savia_100_1, mccain_savia_100_1$Ca)
colnames(Tabla_100_1) <- c('Variedad','Peso','DDS','Tratamiento','Ca')

Tabla_1 <- rbind(Tabla_0_1,Tabla_33_1, Tabla_66_1, Tabla_100_1)

head(Tabla_1)

mccain_savia_0_1 <- mccain_savia %>%
  filter(Variedad =="CIP 1", Tratamiento =="0% Fertilización")%>%
  group_by(DDS,Variedad,Tratamiento)%>%
  summarise(K = mean(K, na.rm=T))

Tabla_0_1 <- cbind(mccain_savia_0_1$Variedad,Tabla_pesos_savia_0_1, mccain_savia_0_1$K)
colnames(Tabla_0_1) <- c('Variedad','Peso','DDS','Tratamiento','K')

mccain_savia_33_1 <- mccain_savia %>%
  filter(Variedad =="CIP 1", Tratamiento =="33% Fertilización")%>%
  group_by(DDS,Variedad,Tratamiento)%>%
  summarise(K = mean(K, na.rm=T))

Tabla_33_1 <- cbind(mccain_savia_33_1$Variedad,Tabla_pesos_savia_33_1, mccain_savia_33_1$K)
colnames(Tabla_33_1) <- c('Variedad','Peso','DDS','Tratamiento','K')

mccain_savia_66_1 <- mccain_savia %>%
  filter(Variedad =="CIP 1", Tratamiento =="66% Fertilización")%>%
  group_by(DDS,Variedad,Tratamiento)%>%
  summarise(K = mean(K, na.rm=T))

Tabla_66_1 <- cbind(mccain_savia_66_1$Variedad,Tabla_pesos_savia_66_1, mccain_savia_66_1$K)
colnames(Tabla_66_1) <- c('Variedad','Peso','DDS','Tratamiento','K')

mccain_savia_100_1 <- mccain_savia %>%
  filter(Variedad =="CIP 1", Tratamiento =="100% Fertilización")%>%
  group_by(DDS,Variedad,Tratamiento)%>%
  summarise(K = mean(K, na.rm=T))

Tabla_100_1 <- cbind(mccain_savia_100_1$Variedad,Tabla_pesos_savia_100_1, mccain_savia_100_1$K)
colnames(Tabla_100_1) <- c('Variedad','Peso','DDS','Tratamiento','K')

Tabla_1 <- rbind(Tabla_0_1,Tabla_33_1, Tabla_66_1, Tabla_100_1)

head(Tabla_1)

library(dplyr)

masa_seca_1 <- datos_masa_seca %>% 
  filter(VARIEDAD=="1")%>%
  group_by(DDS,TRATAMIENTO.)%>%
  summarise(REND_TUB_TON_HA=mean(REND_TUB_TON_HA,na.rm = T),
            DDS=DDS,
            TRATAMIENTO=TRATAMIENTO.)


ggplot() + geom_point(data=masa_seca_1, aes(x=DDS, y = REND_TUB_TON_HA, group = TRATAMIENTO., colour = TRATAMIENTO.)) + 
  xlab("DDS") + 
  ylab("RENDIMIENTO TOTAL") + 
  ggtitle("") +
  tema

Gompertz <- function(x, y0, ymax, k, lag){
  result<- y0 + ymax*exp(-exp(k*(lag-x)/(ymax-y0) + 1))
  return(result)
}

masa_seca_100_1 <- datos_masa_seca %>%
  filter(VARIEDAD =="1",TRATAMIENTO.=="100% Fertilización")%>%
  group_by(DDS, TRATAMIENTO.)%>%
  summarise(REND_TUB_TON_HA_100_1=REND_TUB_TON_HA,
            DDS_100_1=DDS,
            TRATAMIENTO=TRATAMIENTO.)

Gomp_100_1 <- nls(REND_TUB_TON_HA_100_1 ~ Gompertz(DDS_100_1, y0, ymax, k, lag),
                  data=masa_seca_100_1,
                  start = list(y0=1, ymax=30, k=0.1, lag=1))

masa_seca_66_1 <- datos_masa_seca %>%
  filter(VARIEDAD =="1",TRATAMIENTO.=="66% Fertilización")%>%
  group_by(DDS, TRATAMIENTO.)%>%
  summarise(REND_TUB_TON_HA_66_1=REND_TUB_TON_HA,
            DDS_66_1=DDS,
            TRATAMIENTO=TRATAMIENTO.)

Gomp_66_1 <- nls(REND_TUB_TON_HA_66_1 ~ Gompertz(DDS_66_1, y0, ymax, k, lag),
                 data=masa_seca_66_1,
                 start = list(y0=1, ymax=30, k=0.1, lag=1))

masa_seca_33_1 <- datos_masa_seca %>%
  filter(VARIEDAD =="1",TRATAMIENTO.=="33% Fertilización")%>%
  group_by(DDS, TRATAMIENTO.)%>%
  summarise(REND_TUB_TON_HA_33_1=REND_TUB_TON_HA,
            DDS_33_1=DDS,
            TRATAMIENTO=TRATAMIENTO.)

Gomp_33_1 <- nls(REND_TUB_TON_HA_33_1 ~ Gompertz(DDS_33_1, y0, ymax, k, lag),
                 data=masa_seca_33_1,
                 start = list(y0=1, ymax=30, k=0.1, lag=1))

masa_seca_0_1 <- datos_masa_seca %>%
  filter(VARIEDAD =="1",TRATAMIENTO.=="0% Fertilización")%>%
  group_by(DDS, TRATAMIENTO.)%>%
  summarise(REND_TUB_TON_HA_0_1=REND_TUB_TON_HA,
            DDS_0_1=DDS,
            TRATAMIENTO=TRATAMIENTO.)

Gomp_0_1 <- nls(REND_TUB_TON_HA_0_1 ~ Gompertz(DDS_0_1, y0, ymax, k, lag),
                data=masa_seca_0_1,
                start = list(y0=1, ymax=30, k=0.1, lag=1))

coefs_100_1 <- coef(Gomp_100_1)
y0_100_1 = coefs_100_1[1]
ymax_100_1=coefs_100_1[2]
k_100_1=coefs_100_1[3]
lag_100_1=coefs_100_1[4]

coefs_100_1

coefs_66_1 <- coef(Gomp_66_1)
y0_66_1 = coefs_66_1[1]
ymax_66_1=coefs_66_1[2]
k_66_1=coefs_66_1[3]
lag_66_1=coefs_66_1[4]

coefs_66_1

coefs_33_1 <- coef(Gomp_33_1)
y0_33_1 = coefs_33_1[1]
ymax_33_1=coefs_33_1[2]
k_33_1=coefs_33_1[3]
lag_33_1=coefs_33_1[4]

coefs_33_1

coefs_0_1 <- coef(Gomp_0_1)
y0_0_1 = coefs_0_1[1]
ymax_0_1= coefs_0_1[2]
k_0_1 = coefs_0_1[3]
lag_0_1 = coefs_0_1[4]

coefs_0_1

summary(Gomp_100_1)
summary(Gomp_66_1)
summary(Gomp_33_1)
summary(Gomp_0_1)

DDS_LEVELS <- seq(0,150,by=1)

par(mfrow=c(2,2))

pred <- predict(Gomp_100_1, list(x=DDS_LEVELS))
plot(REND_TUB_TON_HA_100_1~DDS_100_1, data=masa_seca_100_1, xlim=c(0,150), ylim=c(0,25),
     xlab="DDS", ylab="RENDIMIENTO total")
lines(DDS_LEVELS, Gompertz(DDS_LEVELS, y0_100_1, ymax_100_1, k_100_1, lag_100_1),
      lty=1, col="cadetblue3", lwd = 2)

pred <- predict(Gomp_66_1, list(x=DDS_LEVELS))
plot(REND_TUB_TON_HA_66_1~DDS_66_1, data=masa_seca_66_1, xlim=c(0,150), ylim=c(0,25),
     xlab="DDS", ylab="RENDIMIENTO total")
lines(DDS_LEVELS, Gompertz(DDS_LEVELS, y0_66_1, ymax_66_1, k_66_1, lag_66_1),
      lty=1, col="cadetblue2", lwd=2)

pred <- predict(Gomp_33_1, list(x=DDS_LEVELS))
plot(REND_TUB_TON_HA_33_1~DDS_33_1, data=masa_seca_33_1, xlim=c(0,150), ylim=c(0,25),
     xlab="DDS", ylab="RENDIMIENTO total")
lines(DDS_LEVELS, Gompertz(DDS_LEVELS, y0_33_1, ymax_33_1, k_33_1, lag_33_1),
      lty=1, col="cadetblue4", lwd=2)

pred <- predict(Gomp_0_1, list(x=DDS_LEVELS))
plot(REND_TUB_TON_HA_0_1~DDS_0_1, data=masa_seca_0_1, xlim=c(0,150), ylim=c(0,25),
     xlab="DDS", ylab="RENDIMIENTO total")
lines(DDS_LEVELS, Gompertz(DDS_LEVELS, y0_0_1, ymax_0_1, k_0_1, lag_0_1),
      lty=1, col="seagreen2", lwd=2)

par(mfrow=c(1,1))

plot(REND_TUB_TON_HA~DDS, data=masa_seca_1, xlim=c(0,150), ylim=c(0,25),
     xlab="DDS", ylab="Rendimiento total (ton/ha)")
lines(DDS_LEVELS, Gompertz(DDS_LEVELS, y0_66_1, ymax_66_1, k_66_1, lag_66_1),
      lty=1, col="cadetblue2", lwd=2)
lines(DDS_LEVELS, Gompertz(DDS_LEVELS, y0_100_1, ymax_100_1, k_100_1, lag_100_1),
      lty=1, col="cadetblue3", lwd=2)
lines(DDS_LEVELS, Gompertz(DDS_LEVELS, y0_33_1, ymax_33_1, k_33_1, lag_33_1),
      lty=1, col="cadetblue4", lwd=2)
lines(DDS_LEVELS, Gompertz(DDS_LEVELS, y0_0_1, ymax_0_1, k_0_1, lag_0_1),
      lty=1, col="seagreen2", lwd=2)
legend("topleft", legend=c("100% Fertilización", "66% Fertilización", "33% Fertilización", "0% Fertilización"), lwd = 3, col = c("cadetblue3","cadetblue2","cadetblue4", "seagreen2"))

RENDIMIENTOs_savia_100_1 <- Gompertz(c(36,45,53,61,70,78,84,91,98,105,115),y0_100_1,ymax_100_1,k_100_1,lag_100_1)
DDS_100_1 <- c(36,45,53,61,70,78,84,91,98,105,115)
TRATAMIENTO_100_1 <- rep("100% Fertilización",11)
Tabla_RENDIMIENTOs_savia_100_1 <- data.frame(RENDIMIENTOs_savia_100_1,DDS_100_1,TRATAMIENTO_100_1)
Tabla_RENDIMIENTOs_savia_100_1

RENDIMIENTOs_savia_66_1 <- Gompertz(c(36,45,53,61,70,78,84,91,98,105,115),y0_66_1,ymax_66_1,k_66_1,lag_66_1)
DDS_66_1 <- c(36,45,53,61,70,78,84,91,98,105,115)
TRATAMIENTO_66_1 <- rep("66% Fertilización",11)
Tabla_RENDIMIENTOs_savia_66_1 <- data.frame(RENDIMIENTOs_savia_66_1,DDS_66_1,TRATAMIENTO_66_1)
Tabla_RENDIMIENTOs_savia_66_1

RENDIMIENTOs_savia_33_1 <- Gompertz(c(36,45,53,61,70,78,84,91,98,105,115),y0_33_1,ymax_33_1,k_33_1,lag_33_1)
DDS_33_1 <- c(36,45,53,61,70,78,84,91,98,105,115)
TRATAMIENTO_33_1 <- rep("33% Fertilización",11)
Tabla_RENDIMIENTOs_savia_33_1 <- data.frame(RENDIMIENTOs_savia_33_1,DDS_33_1,TRATAMIENTO_33_1)
Tabla_RENDIMIENTOs_savia_33_1

RENDIMIENTOs_savia_0_1 <- Gompertz(c(36,45,53,61,70,78,84,91,98,105,115),y0_0_1,ymax_0_1,k_0_1,lag_0_1)
DDS_0_1 <- c(36,45,53,61,70,78,84,91,98,105,115)
TRATAMIENTO_0_1 <- rep("0% Fertilización",11)
Tabla_RENDIMIENTOs_savia_0_1 <- data.frame(RENDIMIENTOs_savia_0_1,DDS_0_1,TRATAMIENTO_0_1)
Tabla_RENDIMIENTOs_savia_0_1

library(dplyr)

mccain_savia_0_1 <- mccain_savia %>%
  filter(Variedad =="CIP 1", Tratamiento =="100% Fertilización")%>%
  group_by(DDS,Variedad,Tratamiento)%>%
  summarise(NO3 = mean(NO3, na.rm=T))

Tabla_0_1 <- cbind(mccain_savia_0_1$Variedad,Tabla_RENDIMIENTOs_savia_0_1, mccain_savia_0_1$NO3)
colnames(Tabla_0_1) <- c('Variedad','RENDIMIENTO','DDS','Tratamiento','NO3')

mccain_savia_33_1 <- mccain_savia %>%
  filter(Variedad =="CIP 1", Tratamiento =="33% Fertilización")%>%
  group_by(DDS,Variedad,Tratamiento)%>%
  summarise(NO3 = mean(NO3, na.rm=T))

Tabla_33_1 <- cbind(mccain_savia_33_1$Variedad,Tabla_RENDIMIENTOs_savia_33_1, mccain_savia_33_1$NO3)
colnames(Tabla_33_1) <- c('Variedad','RENDIMIENTO','DDS','Tratamiento','NO3')

mccain_savia_66_1 <- mccain_savia %>%
  filter(Variedad =="CIP 1", Tratamiento =="66% Fertilización")%>%
  group_by(DDS,Variedad,Tratamiento)%>%
  summarise(NO3 = mean(NO3, na.rm=T))

Tabla_66_1 <- cbind(mccain_savia_66_1$Variedad,Tabla_RENDIMIENTOs_savia_66_1, mccain_savia_66_1$NO3)
colnames(Tabla_66_1) <- c('Variedad','RENDIMIENTO','DDS','Tratamiento','NO3')

mccain_savia_100_1 <- mccain_savia %>%
  filter(Variedad =="CIP 1", Tratamiento =="100% Fertilización")%>%
  group_by(DDS,Variedad,Tratamiento)%>%
  summarise(NO3 = mean(NO3, na.rm=T))

Tabla_100_1 <- cbind(mccain_savia_100_1$Variedad,Tabla_RENDIMIENTOs_savia_100_1, mccain_savia_100_1$NO3)
colnames(Tabla_100_1) <- c('Variedad','RENDIMIENTO','DDS','Tratamiento','NO3')

Tabla_1 <- rbind(Tabla_0_1,Tabla_33_1, Tabla_66_1, Tabla_100_1)

head(Tabla_1)

mccain_savia_0_1 <- mccain_savia %>%
  filter(Variedad =="CIP 1", Tratamiento =="0% Fertilización")%>%
  group_by(DDS,Variedad,Tratamiento)%>%
  summarise(Ca = mean(Ca, na.rm=T))

Tabla_0_1 <- cbind(mccain_savia_0_1$Variedad,Tabla_RENDIMIENTOs_savia_0_1, mccain_savia_0_1$Ca)
colnames(Tabla_0_1) <- c('Variedad','RENDIMIENTO','DDS','Tratamiento','Ca')

mccain_savia_33_1 <- mccain_savia %>%
  filter(Variedad =="CIP 1", Tratamiento =="33% Fertilización")%>%
  group_by(DDS,Variedad,Tratamiento)%>%
  summarise(Ca = mean(Ca, na.rm=T))

Tabla_33_1 <- cbind(mccain_savia_33_1$Variedad,Tabla_RENDIMIENTOs_savia_33_1, mccain_savia_33_1$Ca)
colnames(Tabla_33_1) <- c('Variedad','RENDIMIENTO','DDS','Tratamiento','Ca')

mccain_savia_66_1 <- mccain_savia %>%
  filter(Variedad =="CIP 1", Tratamiento =="66% Fertilización")%>%
  group_by(DDS,Variedad,Tratamiento)%>%
  summarise(Ca = mean(Ca, na.rm=T))

Tabla_66_1 <- cbind(mccain_savia_66_1$Variedad,Tabla_RENDIMIENTOs_savia_66_1, mccain_savia_66_1$Ca)
colnames(Tabla_66_1) <- c('Variedad','RENDIMIENTO','DDS','Tratamiento','Ca')

mccain_savia_100_1 <- mccain_savia %>%
  filter(Variedad =="CIP 1", Tratamiento =="100% Fertilización")%>%
  group_by(DDS,Variedad,Tratamiento)%>%
  summarise(Ca = mean(Ca, na.rm=T))

Tabla_100_1 <- cbind(mccain_savia_100_1$Variedad,Tabla_RENDIMIENTOs_savia_100_1, mccain_savia_100_1$Ca)
colnames(Tabla_100_1) <- c('Variedad','RENDIMIENTO','DDS','Tratamiento','Ca')

Tabla_1 <- rbind(Tabla_0_1,Tabla_33_1, Tabla_66_1, Tabla_100_1)

head(Tabla_1)

mccain_savia_0_1 <- mccain_savia %>%
  filter(Variedad =="CIP 1", Tratamiento =="0% Fertilización")%>%
  group_by(DDS,Variedad,Tratamiento)%>%
  summarise(K = mean(K, na.rm=T))

Tabla_0_1 <- cbind(mccain_savia_0_1$Variedad,Tabla_RENDIMIENTOs_savia_0_1, mccain_savia_0_1$K)
colnames(Tabla_0_1) <- c('Variedad','RENDIMIENTO','DDS','Tratamiento','K')

mccain_savia_33_1 <- mccain_savia %>%
  filter(Variedad =="CIP 1", Tratamiento =="33% Fertilización")%>%
  group_by(DDS,Variedad,Tratamiento)%>%
  summarise(K = mean(K, na.rm=T))

Tabla_33_1 <- cbind(mccain_savia_33_1$Variedad,Tabla_RENDIMIENTOs_savia_33_1, mccain_savia_33_1$K)
colnames(Tabla_33_1) <- c('Variedad','RENDIMIENTO','DDS','Tratamiento','K')

mccain_savia_66_1 <- mccain_savia %>%
  filter(Variedad =="CIP 1", Tratamiento =="66% Fertilización")%>%
  group_by(DDS,Variedad,Tratamiento)%>%
  summarise(K = mean(K, na.rm=T))

Tabla_66_1 <- cbind(mccain_savia_66_1$Variedad,Tabla_RENDIMIENTOs_savia_66_1, mccain_savia_66_1$K)
colnames(Tabla_66_1) <- c('Variedad','RENDIMIENTO','DDS','Tratamiento','K')

mccain_savia_100_1 <- mccain_savia %>%
  filter(Variedad =="CIP 1", Tratamiento =="100% Fertilización")%>%
  group_by(DDS,Variedad,Tratamiento)%>%
  summarise(K = mean(K, na.rm=T))

Tabla_100_1 <- cbind(mccain_savia_100_1$Variedad,Tabla_RENDIMIENTOs_savia_100_1, mccain_savia_100_1$K)
colnames(Tabla_100_1) <- c('Variedad','RENDIMIENTO','DDS','Tratamiento','K')

Tabla_1 <- rbind(Tabla_0_1,Tabla_33_1, Tabla_66_1, Tabla_100_1)

head(Tabla_1)

