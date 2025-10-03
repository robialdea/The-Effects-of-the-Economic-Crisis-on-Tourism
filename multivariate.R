setwd("C:/Users/Robi/Desktop/Serii de timp")
# Încarcă pachetul necesar
library(tseries)
library(readxl)
library(vars)
multi <- read_excel("multivariat_oficial.xlsx")

# Declaram variabilele de tip ts

pib <- ts(multi$PIB, start = c(2008,1), frequency = 4)
somaj <- ts(multi$Rata_somaj, start = c(2008,1), frequency = 4)
sosiri <- ts(multi$Sosiri, start=c(2008,1), frequency = 4)
pondere <- ts(multi$Pondere_turism, start=c(2008,1), frequency = 4)
valutar <- ts(multi$Schimb_val, start=c(2008,1), frequency = 4)
inflatie <- ts(multi$Inflatie, start=c(2008,1), frequency = 4)

# Pachete necesare
library(tsibble)
library(fabletools)
library(ggplot2)
library(dplyr)
library(lubridate)

# Convertim dataframe-ul în tsibble
multi_tsibble <- multi %>%
  mutate(Trimestru = yearquarter(Trimestru)) %>%
  as_tsibble(index = Trimestru)

# Nume variabile (excludem coloana Trimestru)
var_names <- names(multi_tsibble)[-1]

# Loop pentru generarea graficelor
library(forecast)

ggplot(multi) +
  geom_line(aes(x=Trimestru, y=Schimb_val, color="Curs valutar", group=1))

ggplot(multi) +
  geom_line(aes(x = Trimestru, y = Sosiri, color = "Sosiri", group=1)) +
  geom_line(aes(x = Trimestru, y = Rata_somaj, color = "Somaj", group=1)) +
  geom_line(aes(x = Trimestru, y = PIB, color = "PIB", group=1)) +
  geom_line(aes(x = Trimestru, y = Pondere_turism, color = "Ponderea turismului din PIB", group=1)) +
  geom_line(aes(x = Trimestru, y = Inflatie, color = "Inflatie", group=1)) +
  geom_line(aes(x = Trimestru, y = Schimb_val, color = "Curs valutar", group=1)) +
  labs(title = "Serii", y = "Valori", x = "Timp") +
  theme_bw() +
  scale_color_manual(values = c("Sosiri" = "blue", "Somaj" = "green", "PIB" = "red", "Ponderea turismului din PIB" = "chocolate", "Inflatia" = "bisque", "Curs valutar" = "lightblue2"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_x_discrete(breaks = multi$Trimestru[seq(1, nrow(multi), by = 4)])

# Standardizare (Z-score) pentru toate seriile relevante
multi_z <- multi %>%
  mutate(
    Sosiri_z = scale(Sosiri),
    Somaj_z = scale(Rata_somaj),
    PIB_z = scale(PIB),
    Pondere_z = scale(Pondere_turism),
    Curs_z = scale(Schimb_val),
    Inflatie_z = scale(Inflatie)
  )
library(dplyr)
install.packages("tidyr")
library(tidyverse)
multi_long <- multi_z %>%
  dplyr::select(Trimestru, Sosiri_z, Somaj_z, PIB_z, Pondere_z, Curs_z, Inflatie_z) %>%
  pivot_longer(-Trimestru, names_to = "Variabila", values_to = "Valoare")
# Etichete prietenoase pentru legendă
etichete <- c(
  Sosiri_z = "Sosiri",
  Somaj_z = "Șomaj",
  PIB_z = "PIB",
  Pondere_z = "Pondere turism",
  Curs_z = "Curs valutar",
  Inflatie_z = "Inflație"
)

# Culoare pentru fiecare serie
culori <- c(
  "Sosiri" = "blue",
  "Șomaj" = "green",
  "PIB" = "red",
  "Pondere turism" = "chocolate",
  "Curs valutar" = "gray50",
  "Inflație" = "orange"
)
multi_long$Variabila <- factor(
  multi_long$Variabila,
  levels = c("Sosiri_z", "Somaj_z", "PIB_z", "Pondere_z", "Curs_z", "Inflatie_z"),
  labels = c("Sosiri", "Șomaj", "PIB", "Pondere turism", "Curs valutar", "Inflație")
)
# Grafic standardizat
ggplot(multi_long, aes(x = Trimestru, y = Valoare, color = Variabila, group = Variabila)) +
  geom_line() +
  labs(
    title = "Serii standardizate",
    x = "Timp", y = "Valori standardizate"
  ) +
  scale_color_manual(values = c(
    "Sosiri" = "blue",
    "Șomaj" = "green",
    "PIB" = "red",
    "Pondere turism" = "chocolate",
    "Curs valutar" = "gray50",
    "Inflație" = "orange"
  )) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(breaks = multi$Trimestru[seq(1, nrow(multi), by = 4)])

for (var in var_names) {
  ts_data <- ts(multi[[var]], start = c(2008, 1), frequency = 4)
  
  p <- ggsubseriesplot(ts_data) +
    ylab(var) +
    ggtitle(paste("Subseries plot - Sezonalitate pentru", var)) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))
  
  print(p)
  Sys.sleep(1)
}

model_pib <- stl(pib, s.window = "periodic")
pib_desez <- seasadj(model_pib)
ts.plot(pib, pib_desez, col = c("red", "blue"), lty = 1:2,
        main = "PIB: original vs. fără sezonalitate", ylab = "PIB")
legend("topleft", legend = c("Original", "Fără sezonalitate"),
       col = c("red", "blue"), lty = 1:2)

model_sosiri <- stl(sosiri, s.window = "periodic")
sosiri_desez <- seasadj(model_sosiri)
ts.plot(sosiri, sosiri_desez, col = c("red", "blue"), lty = 1:2,
        main = "Sosiri turiști: original vs. fără sezonalitate", ylab = "Număr sosiri")
legend("topleft", legend = c("Original", "Fără sezonalitate"),
       col = c("red", "blue"), lty = 1:2)

model_inflatie <- stl(inflatie, s.window = "periodic")
inflatie_desez <- seasadj(model_inflatie)
ts.plot(inflatie, inflatie_desez, col = c("red", "blue"), lty = 1:2,
        main = "Inflație: original vs. fără sezonalitate", ylab = "%")
legend("topleft", legend = c("Original", "Fără sezonalitate"),
       col = c("red", "blue"), lty = 1:2)

model_pondere <- stl(pondere, s.window = "periodic")
pondere_desez <- seasadj(model_pondere)
ts.plot(pondere, pondere_desez, col = c("red", "blue"), lty = 1:2,
        main = "Pondere turism în economie: original vs. fără sezonalitate", ylab = "% PIB")
legend("topleft", legend = c("Original", "Fără sezonalitate"),
       col = c("red", "blue"), lty = 1:2)

model_somaj <- stl(somaj, s.window = "periodic")
somaj_desez <- seasadj(model_somaj)
ts.plot(somaj, somaj_desez, col = c("red", "blue"), lty = 1:2,
        main = "Rata șomaj: original vs. fără sezonalitate", ylab = "%")
legend("topleft", legend = c("Original", "Fără sezonalitate"),
       col = c("red", "blue"), lty = 1:2)


# Modele VAR, cauzalitate Granger 
multi$Sosiri <- sosiri_desez

# Librariile folosite

library(urca)
library(vars)
install.packages("mFilter")
library(mFilter)
library(tseries)
library(forecast)
install.packages("tidyverse")
library(tidyverse)
install.packages("stargazer")
library(stargazer)
install.packages("Metrics")
library(Metrics)

# Scatterplot
install.packages("gridExtra")
library(gridExtra)
p1 <- ggplot(data = multi) + 
  geom_point(mapping = aes(x = Schimb_val, y = Sosiri)) +
  xlab('Curs valutar') +
  ylab('Sosiri') +  
  ggtitle('Norul de puncte dintre cursul valutar si sosiri turistice')+
  theme_bw()

p2<- ggplot(data = multi) + 
  geom_point(mapping = aes(x = Inflatie, y = Sosiri)) +
  xlab('Inflatie') +
  ylab('Sosiri') +  
  ggtitle('Norul de puncte dintre inflatie si sosiri turistice')+
  theme_bw()

p3<-ggplot(data = multi) + 
  geom_point(mapping = aes(x = Rata_somaj, y = Sosiri)) +
  xlab('Rata Somajului') +
  ylab('Sosiri') +  
  ggtitle('Norul de puncte dintre rata somajului si sosiri turistice')+
  theme_bw()

p4<-ggplot(data = multi) + 
  geom_point(mapping = aes(x = PIB, y = Sosiri)) +
  xlab('PIB') +
  ylab('Sosiri') +  
  ggtitle('Norul de puncte dintre PIB si sosiri turistice')+
  theme_bw()

p5<-ggplot(data = multi) + 
  geom_point(mapping = aes(x = Pondere_turism, y = Sosiri)) +
  xlab('Ponderea turismului in PIB') +
  ylab('Sosiri') +  
  ggtitle('Norul de puncte dintre ponderea turismului in PIB si sosiri turistice')+
  theme_bw()

grid.arrange(p1, p2, p3, p4, p5, ncol = 3)

# Determinarea persistentei modelului
p11<-ggtsdisplay(pib, main="Persistenta modelului PIB")
p12<-ggtsdisplay(somaj, main="Persistenta modelului Somaj")
p13<-ggtsdisplay(sosiri_desez, main="Persistenta modelului Sosiri")
p14<-ggtsdisplay(pondere, main="Persistenta modelului Pondere turism din PIB")
p15<-ggtsdisplay(inflatie, main="Persistenta modelului Inflatie")
p16<-ggtsdisplay(valutar, main="Persistenta modelului Curs valutar")
# Testarea stationaritatii seriilor (am ales varianta cea mai complexa a ADF)
adf.somaj.trend <- ur.df(somaj, type = "trend", selectlags = "AIC")
summary(adf.somaj.trend)
adf.somaj.intercept <- ur.df(somaj, type = "drift", selectlags = "AIC")
summary(adf.somaj.intercept) #
adf.somaj.none<- ur.df(somaj, type="none", selectlags="AIC")
summary(adf.somaj.none)
somaj_diff <-diff(somaj)

adf.pib.trend <- ur.df(pib, type = "trend", selectlags = "AIC")
summary(adf.pib.trend) #
adf.pib.intercept <- ur.df(pib, type = "drift", selectlags = "AIC")
summary(adf.pib.intercept) #
adf.pib.none<- ur.df(pib, type="none", selectlags="AIC")
summary(adf.pib.none)
pib_diff<-diff(pib)

adf.sosiri.trend <- ur.df(sosiri_desez, type = "trend", selectlags = "AIC")
summary(adf.sosiri.trend) #
adf.sosiri.intercept <- ur.df(sosiri_desez, type = "drift", selectlags = "AIC")
summary(adf.sosiri.intercept) #
adf.sosiri.none<- ur.df(sosiri_desez, type="none", selectlags="AIC")
summary(adf.sosiri.none)
sosiri_diff<-diff(sosiri_desez)

adf.pondere.trend <- ur.df(pondere, type = "trend", selectlags = "AIC")
summary(adf.pondere.trend) #
adf.pondere.intercept <- ur.df(pondere, type = "drift", selectlags = "AIC")
summary(adf.pondere.intercept) #
adf.pondere.none<- ur.df(pondere, type="none", selectlags="AIC")
summary(adf.pondere.none)
pondere_diff<-diff(pondere)

adf.inflatie.trend <- ur.df(inflatie, type = "trend", selectlags = "AIC")
summary(adf.inflatie.trend) #
adf.inflatie.intercept <- ur.df(inflatie, type = "drift", selectlags = "AIC")
summary(adf.inflatie.intercept) #
adf.inflatie.none<- ur.df(inflatie, type="none", selectlags="AIC")
summary(adf.inflatie.none)
inflatie_diff <- diff(inflatie)

adf.cval.trend <- ur.df(valutar, type = "trend", selectlags = "AIC")
summary(adf.cval.trend) #
adf.cval.intercept <- ur.df(valutar, type = "drift", selectlags = "AIC")
summary(adf.cval.intercept) #
adf.cval.none<- ur.df(valutar, type="none", selectlags="AIC")
summary(adf.cval.none)
valutar_diff<-diff(valutar)

adf.somaj.diff <- ur.df(somaj_diff, type = "drift", selectlags = "AIC")
summary(adf.somaj.diff)

adf.pib.diff <- ur.df(pib_diff, type = "drift", selectlags = "AIC")
summary(adf.pib.diff)

adf.pondere.diff <- ur.df(pondere_diff, type = "drift", selectlags = "AIC")
summary(adf.pondere.diff)

adf.inflatie.diff <- ur.df(inflatie_diff, type = "drift", selectlags = "AIC")
summary(adf.inflatie.diff)

final<-cbind(sosiri_diff, inflatie_diff, valutar_diff, pib_diff, pondere_diff, somaj_diff)
final
final_final<-cbind(sosiri_desez, inflatie, valutar, pib, pondere, somaj)
tsdisplay(sosiri_diff)
tsdisplay(inflatie_diff)
tsdisplay(valutar_diff)
tsdisplay(pib_diff)
tsdisplay(pondere_diff)
tsdisplay(somaj_diff)

# Identificarea lagurilor optime

lagselect <- VARselect(final_final,lag.max = 4, type = 'const')
lagselect
lagselect$selection # 8 laguri conform AIC, HQ

# Testul de cointegrare Johansen
johansen_test <- ca.jo(final_final, type = "trace", ecdet = "const", K = 2)
summary(johansen_test)
#r=3 3 realtii de cointegrare

# VECM
install.packages("tsDyn")
library(tsDyn)
Model1 <- VECM(final_final,
               lag = 1, 
               r=1, 
               LRinclude = 'const')
summary(Model1)
#pt licenta de testat mai multe laguri
# trebuie sa transformam modelul intr un var
Model1VAR <- vec2var(johansen_test, r=1)
#autocorelare
Serial1 <- serial.test(Model1VAR, lags.pt = 8, type = 'PT.asymptotic')
Serial1 # nu avem autocorelare in reziduuri
# Heteroschedascitate
Arch1 <- vars::arch.test(Model1VAR, lags.multi = 15, multivariate.only = TRUE)
Arch1 #reziduuri homoschedastice
# Normalitate
Norm1 <- normality.test(Model1VAR, multivariate.only = TRUE)
Norm1 # reziduurile nu sunt normal distribuite

# Functia de raspuns la impuls
# soc în PIB, efect asupra sosirilor
irf_pib_sosiri <- irf(Model1VAR, impulse = "pib", response = "sosiri_desez", 
                      n.ahead = 8, boot = TRUE)
irf1 <- plot(irf_pib_sosiri, ylab = "sosiri", main = "Soc PIB -> Sosiri turisti")

irf_somaj_sosiri <- irf(Model1VAR, impulse = "somaj", response = "sosiri_desez", 
                        n.ahead = 8, boot = TRUE)
irf2 <- plot(irf_somaj_sosiri, ylab = "sosiri", main = "Soc Somaj -> Sosiri turisti")

irf_inflatie_sosiri <- irf(Model1VAR, impulse = "inflatie", response="sosiri_desez", n.ahead=8, boot=TRUE)
irf3 <- plot(irf_inflatie_sosiri, ylab="sosiri", main = "Soc Inflatie -> Sosiri turisti")

irf_sosiri_pturism <- irf(Model1VAR, impulse = "sosiri_desez", response = "pondere", 
                      n.ahead = 8, boot = TRUE)
plot(irf_sosiri_pturism, ylab = "pondere turism", main = "Soc Sosiri turisti -> Pondere turism")

irf_curs_pturism <- irf(Model1VAR, impulse = "valutar", response = "pondere", 
                          n.ahead = 8, boot = TRUE)
plot(irf_curs_pturism, ylab = "pondere turism", main = "Soc curs valutar -> Pondere turism")

irf_inflatie_pturism <- irf(Model1VAR, impulse = "inflatie", response = "pondere", 
                        n.ahead = 8, boot = TRUE)
plot(irf_inflatie_pturism, ylab = "pondere turism", main = "Soc inflatie -> Pondere turism")
# Descompunerea variantei
FEVD1 <- fevd(Model1VAR,n.ahead=4)
plot(FEVD1) 
FEVD1
forecast <- predict(Model1VAR, n.ahead = 4, ci = 0.90) # prognoza pe 4 trimestre
par(mfrow=c(2,3))
f1<-plot(forecast, name = 'pib')
f2<-plot(forecast, name = 'pondere')
f3<-plot(forecast, name = 'somaj')
f4<-plot(forecast, name = 'sosiri_desez')
f5<-plot(forecast, name = 'valutar')
f6<-plot(forecast, name = 'inflatie')

fanchart(forecast, name = 'pib')
fanchart(forecast, name = 'pondere')
fanchart(forecast, name = 'somaj')
fanchart(forecast, name = 'sosiri_desez')
fanchart(forecast, name = 'valutar')
fanchart(forecast, name = 'inflatie')

# Cauzalitate Granger 
model_var <-VAR(final_final, p=1, type="const")
Granger_pib <- causality(model_var, cause = "pib")
Granger_pib
granger_somaj <- causality(model_var, cause="somaj")
granger_somaj
granger_inflatie <- causality(model_var, cause="inflatie")
granger_inflatie
granger_valutar <- causality(model_var, cause="valutar")
granger_valutar

