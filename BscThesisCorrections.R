acorrected <- "/Users/baumannmike/Desktop/Datenbeispiele/Datenbeispiele/2024_04_16_MKN_datachain_test_AE31_1hr_corrected.csv"
corrae31data <- read.csv(acorrected)

araw <- "/Users/baumannmike/Desktop/Datenbeispiele/Datenbeispiele/2024_04_16_MKN_datachain_test_AE31_1hr_raw.csv"
rawae31data <- read.csv(araw)

ncorrected <- "/Users/baumannmike/Desktop/Datenbeispiele/Datenbeispiele/2024_04_16_MKN_datachain_test_neph_1hr_corrected.csv"
corrnephdata <- read.csv(ncorrected)

nraw <- "/Users/baumannmike/Desktop/Datenbeispiele/Datenbeispiele/2024_04_16_MKN_datachain_test_neph__1hr_raw.csv"
rawnephdata <- read.csv(nraw)

time <- as.POSIXct(rawae31data$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")


# Aurora 3000
#----------------------------------------------------------------------------------------------------
# total scattering coefficient blue, green, red
#----------------------------------------------------------------------------------------------------
plot(time, corrnephdata$BsB0_S11, type = "l", col = "blue",xlab = "time [h]" ,ylab = "BsB0_S11 [Mm-1]")
lines(time, rawnephdata$BsB0_S11, col = "red", lty = 1)  
legend("topright", legend = c("corrected", "raw"), col = c("blue", "red"), lty = c(1, 2), bty = "n")
title(main = "Aerosol total light scattering coefficient, blue (PM10)", col.main = "black", font.main = 2)

plot(time, corrnephdata$BsG0_S11, type = "l", col = "blue",xlab = "time [h]" ,ylab = "BsG0_S11 [Mm-1]")
lines(time, rawnephdata$BsG0_S11, col = "red", lty = 1)  
legend("topright", legend = c("corrected", "raw"), col = c("blue", "red"), lty = c(1, 2), bty = "n")
title(main = "Aerosol total light scattering coefficient, green (PM10)", col.main = "black", font.main = 2)

plot(time, corrnephdata$BsR0_S11, type = "l", col = "blue",xlab = "time [h]" ,ylab = "BsR0_S11 [Mm-1]")
lines(time, rawnephdata$BsR0_S11, col = "red", lty = 1)  
legend("topright", legend = c("corrected", "raw"), col = c("blue", "red"), lty = c(1, 2), bty = "n")
title(main = "Aerosol total light scattering coefficient, red (PM10)", col.main = "black", font.main = 2)

#----------------------------------------------------------------------------------------------------
# backwards-hemispheric scattering coefficient blue, green, red
#----------------------------------------------------------------------------------------------------
plot(time, corrnephdata$BbsB0_S11, type = "l", col = "blue",xlab = "time [h]" ,ylab = "BbsB0_S11 [Mm-1]")
lines(time, rawnephdata$BbsB0_S11, col = "red", lty = 1)  
legend("topright", legend = c("corrected", "raw"), col = c("blue", "red"), lty = c(1, 2), bty = "n")
title(main = "Aerosol backwards-hemispheric light scattering coefficient, blue (PM10)", col.main = "black", font.main = 2)

plot(time, corrnephdata$BbsG0_S11, type = "l", col = "blue",xlab = "time [h]" ,ylab = "BbsG0_S11 [Mm-1]")
lines(time, rawnephdata$BbsG0_S11, col = "red", lty = 1)  
legend("topright", legend = c("corrected", "raw"), col = c("blue", "red"), lty = c(1, 2), bty = "n")
title(main = "Aerosol backwards-hemispheric light scattering coefficient, green (PM10)", col.main = "black", font.main = 2)

plot(time, corrnephdata$BbsR0_S11, type = "l", col = "blue",xlab = "time [h]" ,ylab = "BbsR0_S11 [Mm-1]")
lines(time, rawnephdata$BbsR0_S11, col = "red", lty = 1)  
legend("topright", legend = c("corrected", "raw"), col = c("blue", "red"), lty = c(1, 2), bty = "n")
title(main = "Aerosol backwards-hemispheric light scattering coefficient, red (PM10)", col.main = "black", font.main = 2)

#----------------------------------------------------------------------------------------------------
# Verhältnis Total Scattering
#----------------------------------------------------------------------------------------------------
# Blau

rawnephdata$BsB0_S11 <- as.numeric(as.character(rawnephdata$BsB0_S11))
corrnephdata$BsB0_S11 <- as.numeric(as.character(corrnephdata$BsB0_S11))

#Bei dieser Zeile hat es einen NA Wert
corrnephdata$BsB0_S11[266] <- 0

ratiob <- rawnephdata$BsB0_S11/corrnephdata$BsB0_S11

# Plot des Verhältnisses
plot(time, ratiob, type = "l", col = "blue", xlab = "time", ylab = "")

# Titel setzen
title(main = "Ratio Total Scattering, blue", col.main = "black", font.main = 2)

#----------------------------------------------------------------------------------------------------
# Grün

rawnephdata$BsG0_S11 <- as.numeric(as.character(rawnephdata$BsG0_S11))
corrnephdata$BsG0_S11 <- as.numeric(as.character(corrnephdata$BsG0_S11))

#Bei dieser Zeile hat es einen NA Wert
corrnephdata$BsG0_S11[266] <- 0

ratiog <- rawnephdata$BsG0_S11/corrnephdata$BsG0_S11

# Plot des Verhältnisses
plot(time, ratiog, type = "l", col = "green", xlab = "time", ylab = "")

# Titel setzen
title(main = "Ratio Total Scattering, green", col.main = "black", font.main = 2)

#----------------------------------------------------------------------------------------------------
# Rot

rawnephdata$BsR0_S11 <- as.numeric(as.character(rawnephdata$BsR0_S11))
corrnephdata$BsR0_S11 <- as.numeric(as.character(corrnephdata$BsR0_S11))

#Bei dieser Zeile hat es einen NA Wert
corrnephdata$BsR0_S11[266] <- 0

ratior <- rawnephdata$BsR0_S11/corrnephdata$BsR0_S11

# Plot des Verhältnisses
plot(time, ratior, type = "l", col = "red", xlab = "time", ylab = "")

# Titel setzen
title(main = "Ratio Total Scattering, red", col.main = "black", font.main = 2)
#----------------------------------------------------------------------------------------------------
# Verhältnis backwards scattering
#----------------------------------------------------------------------------------------------------
# Blue
rawnephdata$BbsB0_S11 <- as.numeric(as.character(rawnephdata$BsB0_S11))
corrnephdata$BbsB0_S11 <- as.numeric(as.character(corrnephdata$BsB0_S11))

ratiobb <- rawnephdata$BbsB0_S11/corrnephdata$BbsB0_S11

# Plot des Verhältnisses
plot(time, ratiobb, type = "l", col = "blue", xlab = "time", ylab = "")

# Titel setzen
title(main = "Ratio Backscattering, blue", col.main = "black", font.main = 2)
#----------------------------------------------------------------------------------------------------
# Green

rawnephdata$BbsG0_S11 <- as.numeric(as.character(rawnephdata$BsG0_S11))
corrnephdata$BbsG0_S11 <- as.numeric(as.character(corrnephdata$BsG0_S11))

ratiobg <- rawnephdata$BbsG0_S11/corrnephdata$BbsG0_S11

# Plot des Verhältnisses
plot(time, ratiobg, type = "l", col = "green", xlab = "time", ylab = "")

# Titel setzen
title(main = "Ratio Backscattering, green", col.main = "black", font.main = 2)

#----------------------------------------------------------------------------------------------------
# Red

rawnephdata$BbsR0_S11 <- as.numeric(as.character(rawnephdata$BsR0_S11))
corrnephdata$BbsR0_S11 <- as.numeric(as.character(corrnephdata$BsR0_S11))

ratiobr <- rawnephdata$BbsR0_S11/corrnephdata$BbsR0_S11

# Plot des Verhältnisses
plot(time, ratiobr, type = "l", col = "red", xlab = "time", ylab = "")

# Titel setzen
title(main = "Ratio Backscattering, red", col.main = "black", font.main = 2)


#----------------------------------------------------------------------------------------------------
# STP correction
#----------------------------------------------------------------------------------------------------

rawnephdata$BsB0_S11 <- as.numeric(rawnephdata$BsB0_S11)
rawnephdata$P0_S11 <- as.numeric(rawnephdata$P0_S11)
rawnephdata$T10_S11 <- as.numeric(rawnephdata$T10_S11)

Ninstr <- rawnephdata$BsB0_S11
Pinstr <- mean(rawnephdata$P0_S11, na.rm = TRUE)
Tinstr <- rawnephdata$T10_S11 + 273.15
Pstp <- 1013.25
Tstp <- 273.15

# Mittelwert für Instrumentendruck
# streuungsplots 
# github code übernhemen clone repository, github

STP <- (Pstp * Tinstr)/(Pinstr * Tstp)

# Plot mit Daten aus einem DataFrame erstellen
plot(time, STP, type = "l", xlab = "time", ylab = "correction factor", main = "STP correction factor")

#----------------------------------------------------------------------------------------------------
# Weingartner korrektur
#----------------------------------------------------------------------------------------------------

rawae31data$X1c0_A11 <- as.numeric(rawae31data$X1c0_A11)
rawae31data$X2c0_A11 <- as.numeric(rawae31data$X2c0_A11)
rawae31data$X3c0_A11 <- as.numeric(rawae31data$X3c0_A11)
rawae31data$X4c0_A11 <- as.numeric(rawae31data$X4c0_A11)
rawae31data$X5c0_A11 <- as.numeric(rawae31data$X5c0_A11)
rawae31data$X6c0_A11 <- as.numeric(rawae31data$X6c0_A11)
rawae31data$X7c0_A11 <- as.numeric(rawae31data$X7c0_A11)


MAC = c(39.5, 31.1, 28.1, 24.8, 22.2, 16.6, 15.4);


batn1 = 1e-3 * rawae31data$X1c0_A11* MAC[1]
batn2 = 1e-3 * rawae31data$X2c0_A11* MAC[2]
batn3 = 1e-3 * rawae31data$X3c0_A11* MAC[3]
batn4 = 1e-3 * rawae31data$X4c0_A11* MAC[4]
batn5 = 1e-3 * rawae31data$X5c0_A11* MAC[5]
batn6 = 1e-3 * rawae31data$X6c0_A11* MAC[6]
batn7 = 1e-3 * rawae31data$X7c0_A11* MAC[7]

cref <- 3.5

WG1 = batn1/cref
WG2 = batn2/cref
WG3 = batn3/cref
WG4 = batn4/cref
WG5 = batn5/cref
WG6 = batn6/cref
WG7 = batn7/cref


plot(time, WG1, col = "blue", type = "l", xlab = "time", ylab = "Mm-1", main = "Aerosol absorption coefficient")
lines(time, WG2, col = "red", lty = 1)  
lines(time, WG3, col = "green", lty = 1)
lines(time, WG4, col = "yellow", lty = 1) 
lines(time, WG5, col = "purple", lty = 1) 
lines(time, WG6, col = "orange", lty = 1) 
lines(time, WG7, col = "black", lty = 1) 

#----------------------------------------------------------------------------------------------------
# Loading Effect Correction
#----------------------------------------------------------------------------------------------------

f = c(1.155, 1.137, 1.128, 1.116, 1.103, 1.064, 1.051)

rawae31data$ZIr10_A11 <- as.numeric(rawae31data$ZIr10_A11)
rawae31data$ZIr20_A11 <- as.numeric(rawae31data$ZIr20_A11)
rawae31data$ZIr30_A11 <- as.numeric(rawae31data$ZIr30_A11)
rawae31data$ZIr40_A11 <- as.numeric(rawae31data$ZIr40_A11)
rawae31data$ZIr50_A11 <- as.numeric(rawae31data$ZIr50_A11)
rawae31data$ZIr60_A11 <- as.numeric(rawae31data$ZIr60_A11)
rawae31data$ZIr70_A11 <- as.numeric(rawae31data$ZIr70_A11)

data.b10_370 = ((1/f[1] - 1) * ((log(rawae31data$ZIr10_A11) - log(10)) / (log(50) - log(10))) + 1)
data.b10_470 = ((1/f[1] - 1) * ((log(rawae31data$ZIr20_A11) - log(10)) / (log(50) - log(10))) + 1)
data.b10_520 = ((1/f[1] - 1) * ((log(rawae31data$ZIr30_A11) - log(10)) / (log(50) - log(10))) + 1)
data.b10_590 = ((1/f[1] - 1) * ((log(rawae31data$ZIr40_A11) - log(10)) / (log(50) - log(10))) + 1)
data.b10_660 = ((1/f[1] - 1) * ((log(rawae31data$ZIr50_A11) - log(10)) / (log(50) - log(10))) + 1)
data.b10_880 = ((1/f[1] - 1) * ((log(rawae31data$ZIr60_A11) - log(10)) / (log(50) - log(10))) + 1)
data.b10_950 = ((1/f[1] - 1) * ((log(rawae31data$ZIr70_A11) - log(10)) / (log(50) - log(10))) + 1)

plot(time, data.b10_370, col = "blue", type = "l", xlab = "time", ylab = "correction factor", main = "loading effect correction")
lines(time, data.b10_470, col = "red", lty = 1)  
lines(time, data.b10_520, col = "green", lty = 1)
lines(time, data.b10_590, col = "yellow", lty = 1) 
lines(time, data.b10_660, col = "purple", lty = 1) 
lines(time, data.b10_880, col = "orange", lty = 1) 
lines(time, data.b10_950, col = "black", lty = 1) 

legend("topright", 
       legend = c("370 nm", "470 nm", "520 nm", "590 nm", "660 nm", "880 nm", "950 nm"),
       col = c("blue", "red", "green", "yellow", "purple", "orange", "black"),
       lty = 1,
       title = "Wellenlänge",
       cex = 0.8)

#----------------------------------------------------------------------------------------------------
# Ttruncation correction (Müller)
#----------------------------------------------------------------------------------------------------

# calculation of Angstrom exponent

rawnephdata$BsB0_S11 <- as.numeric(rawnephdata$BsB0_S11)
rawnephdata$BsG0_S11 <- as.numeric(rawnephdata$BsG0_S11)
rawnephdata$BsR0_S11 <- as.numeric(rawnephdata$BsR0_S11)

ang_bg = -1*(log(rawnephdata$BsB0_S11/rawnephdata$BsG0_S11)/(log(450/525)))
ang_br = -1*(log(rawnephdata$BsB0_S11/rawnephdata$BsR0_S11)/(log(450/635)))
ang_gr = -1*(log(rawnephdata$BsG0_S11)/rawnephdata$BsR0_S11)/(log(525/635))

#Müller constants

a_blue = 1.455
b_blue = -0.189
a_green = 1.434
b_green = -0.176
a_red = 1.403
b_red = -0.156

tr_blue = a_blue + ang_bg * b_blue   
tr_green = a_green + ang_br * b_green   
tr_red = a_red + ang_gr * b_red

plot(time, tr_blue, col = "blue", type = "l", xlab = "time", ylab = "correction factor", main = "truncation correction factors")
lines(time, tr_green, col = "green", lty = 1)  
lines(time, tr_red, col = "red", lty = 1)

legend("topright", 
       legend = c("blue", "green", "red"),
       col = c("blue", "green", "red"),
       lty = 1,
       cex = 0.8)

#----------------------------------------------------------------------------------------------------
# Boxplot Diagramme
#----------------------------------------------------------------------------------------------------




library(ggplot2)

corrnephdata$BbsB0_S11 <- as.numeric(corrnephdata$BbsB0_S11)

# Datenrahmen erstellen
data <- na.omit(data.frame(date = as.Date(time), factor = corrnephdata$BbsB0_S11))

# Gruppierung der Daten nach Tag
data$date_group <- format(data$date, "%Y-%m-%d")

# Erstelle einen Boxplot für jeden Tag in einem einzigen Diagramm
ggplot(data, aes(x = date_group, y = factor )) +
  geom_boxplot() +
  coord_cartesian(ylim = c(min(data$factor), max(data$factor))) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  # Drehung der X-Achsenbeschriftungen um 90 Grad nach links
  labs(x = "date", y = "back scattering coefficient [Mm-1]") +
  ggtitle("aerosol backwards-hemispheric light scattering coefficient, green")


library(ggplot2)

corrnephdata$BsB0_S11 <- as.numeric(corrnephdata$BsB0_S11)

# Datenrahmen erstellen
data <- na.omit(data.frame(date = as.Date(time), factor = corrnephdata$BsB0_S11))

# Gruppierung der Daten nach Tag
data$date_group <- format(data$date, "%Y-%m-%d")

# Erstelle einen Boxplot für jeden Tag in einem einzigen Diagramm
ggplot(data, aes(x = date_group, y = factor )) +
  geom_boxplot() +
  coord_cartesian(ylim = c(min(data$factor), max(data$factor))) + # setzen der y-Achsenbegrenzung auf den Bereich aller Daten
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  # Drehung der X-Achsenbeschriftungen um 90 Grad nach links
  labs(x = "date", y = "total light scattering coefficient [Mm-1]") +
  ggtitle("aerosol total light scattering coefficient, green")



library(ggplot2)

corrae31data$BaB0_ <- as.numeric(corrae31data$BaB0_)

# Datenrahmen erstellen
data <- na.omit(data.frame(date = as.Date(time), factor = corrae31data$BaB0_))

# Gruppierung der Daten nach Tag
data$date_group <- format(data$date, "%Y-%m-%d")

# Erstelle einen Boxplot für jeden Tag in einem einzigen Diagramm
ggplot(data, aes(x = date_group, y = factor )) +
  geom_boxplot() +
  coord_cartesian(ylim = c(min(data$factor), max(data$factor))) + # setzen der y-Achsenbegrenzung auf den Bereich aller Daten
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  # Drehung der X-Achsenbeschriftungen um 90 Grad nach links
  labs(x = "date", y = "absorption coefficient [Mm-1]") +
  ggtitle("aerosol absorption coefficient, 470 nm")









