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


Pinstr <- mean(rawnephdata$P0_S11, na.rm = TRUE)
Tinstr <- rawnephdata$T10_S11 + 273.15
Pstp <- 1013.25
Tstp <- 273.15

# Mittelwert für Instrumentendruck

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


plot(time, WG1, col = "blue", type = "l", xlab = "time", ylab = "Mm-1", main = "aerosol absorption coefficient with Weingartner correction")
lines(time, WG2, col = "red", lty = 1)  
lines(time, WG3, col = "green", lty = 1)
lines(time, WG4, col = "yellow", lty = 1) 
lines(time, WG5, col = "purple", lty = 1) 
lines(time, WG6, col = "orange", lty = 1) 
lines(time, WG7, col = "black", lty = 1) 

legend("topright", 
       legend = c("370 nm", "470 nm", "520 nm", "590 nm", "660 nm", "880 nm", "950 nm"), 
       col = c("blue", "red", "green", "yellow", "purple", "orange", "black"), 
       lty = 1)




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
# Flow Correction
#----------------------------------------------------------------------------------------------------


rawae31data$X1c0_A11 <- as.numeric(rawae31data$X1c0_A11)
rawae31data$Q0_A11 <- as.numeric(rawae31data$Q0_A11 )
flow_buck = 3.74; 


data.bc370 = (rawae31data$X1c0_A11 * rawae31data$Q0_A11) / (flow_buck * 1000)

plot(time, data.bc370, col = "blue", type = "l", xlab = "time", ylab = "Mm-1", main = "Aurora3000")
plot(time, corrae31data$X1c0_A11, col = "red", type = "l", xlab = "time", ylab = "Mm-1", main = "Aurora3000")


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




# Tagesgang time of day 
# intensive properties berechnung single scattering albedo SSA & Angströmexponent
# intrinsisch 

# auf Switch drive Daten
# meine korrektur und corrdaten

#----------------------------------------------------------------------------------------------------
# Korrekturen angewendet AE31
#----------------------------------------------------------------------------------------------------

rawnephdata$BsB0_S11 <- as.numeric(rawnephdata$BsB0_S11)
rawnephdata$P0_S11 <- as.numeric(rawnephdata$P0_S11)
rawnephdata$T10_S11 <- as.numeric(rawnephdata$T10_S11)
rawae31data$X1c0_A11 <- as.numeric(rawae31data$X1c0_A11)


# STP
Pinstr <- mean(rawnephdata$P0_S11, na.rm = TRUE)
Tinstr <- rawnephdata$T10_S11 + 273.15
Pstp <- 1013.25
Tstp <- 273.15

STP <- (Pstp * Tinstr)/(Pinstr * Tstp)


# Calculation batn
data.batn370  <- 1e-3 * rawae31data$X1c0_A11*31.1

#Apply Weingartner and STP
babs <- (data.batn370/3.5)*STP


plot(time, babs, col = "blue", type = "l", xlab = "time", ylab = "Mm-1", main = "Aurora3000")
plot(time, corrae31data$BaB0_, col = "red", type = "l", xlab = "time", ylab = "Mm-1", main = "Aurora3000")

plot(babs, corrae31data$BaB0_, main = "Correlationplot AE31", 
     xlab = "corrected values", ylab = "CPD output", pch = 19, col = "blue")

#----------------------------------------------------------------------------------------------------
# Korrelationsplot Aurora3000
#----------------------------------------------------------------------------------------------------

#Müller

#STP 

rawnephdata$BsB0_S11 <- as.numeric(rawnephdata$BsB0_S11)
rawnephdata$P0_S11 <- as.numeric(rawnephdata$P0_S11)
rawnephdata$T10_S11 <- as.numeric(rawnephdata$T10_S11)


Pinstr <- mean(rawnephdata$P0_S11, na.rm = TRUE)
Tinstr <- rawnephdata$T10_S11 + 273.15
Pstp <- 1013.25
Tstp <- 273.15

# Mittelwert für Instrumentendruck

STP <- (Pstp * Tinstr)/(Pinstr * Tstp)

# calculation of Angstrom exponent

rawnephdata$BsB0_S11 <- as.numeric(rawnephdata$BsB0_S11)
rawnephdata$BsG0_S11 <- as.numeric(rawnephdata$BsG0_S11)
rawnephdata$BsR0_S11 <- as.numeric(rawnephdata$BsR0_S11)

ang_bg = -1*(log(rawnephdata$BsB0_S11/rawnephdata$BsG0_S11)/(log(450/525)))


#Müller constants

a_blue = 1.455
b_blue = -0.189
a_green = 1.434
b_green = -0.176
a_red = 1.403
b_red = -0.156

tr_blue = a_blue + ang_bg * b_blue   


x <- rawnephdata$BsB0_S11*STP*tr_blue

plot(time, corrnephdata$BsB0_S11, col = "red", type = "l", xlab = "time", ylab = "Mm-1", main = "Aurora3000")

plot(time, x, col = "blue", type = "l", xlab = "time", ylab = "Mm-1", main = "Aurora3000")

plot(x, corrnephdata$BsB0_S11, main = "Correlationplot Aurora 3000", 
     xlab = "corrected values", ylab = "CPD output", pch = 19, col = "blue")


#----------------------------------------------------------------------------------------------------
# Tagesgang AE31
#----------------------------------------------------------------------------------------------------

# Zuerst füge eine Spalte für die Stunde hinzu
corrae31data$hour <- as.numeric(format(time, "%H"))
corrae31data$Ba10_A11 <- as.numeric(corrae31data$Ba10_A11)
corrae31data$Ba20_A11 <- as.numeric(corrae31data$Ba20_A11)
corrae31data$Ba30_A11 <- as.numeric(corrae31data$Ba30_A11)
corrae31data$Ba40_A11 <- as.numeric(corrae31data$Ba40_A11)
corrae31data$Ba50_A11 <- as.numeric(corrae31data$Ba50_A11)
corrae31data$Ba60_A11 <- as.numeric(corrae31data$Ba60_A11)
corrae31data$Ba70_A11 <- as.numeric(corrae31data$Ba70_A11)

# Dann gruppieren nach Stunde und den Durchschnitt berechnen
hourly_avg1 <- aggregate(corrae31data$Ba10_A11, by = list(hour = corrae31data$hour), FUN = function(x) mean(x, na.rm = TRUE))
hourly_avg2 <- aggregate(corrae31data$Ba20_A11, by = list(hour = corrae31data$hour), FUN = function(x) mean(x, na.rm = TRUE))
hourly_avg3 <- aggregate(corrae31data$Ba30_A11, by = list(hour = corrae31data$hour), FUN = function(x) mean(x, na.rm = TRUE))
hourly_avg4 <- aggregate(corrae31data$Ba40_A11, by = list(hour = corrae31data$hour), FUN = function(x) mean(x, na.rm = TRUE))
hourly_avg5 <- aggregate(corrae31data$Ba50_A11, by = list(hour = corrae31data$hour), FUN = function(x) mean(x, na.rm = TRUE))
hourly_avg6 <- aggregate(corrae31data$Ba60_A11, by = list(hour = corrae31data$hour), FUN = function(x) mean(x, na.rm = TRUE))
hourly_avg7 <- aggregate(corrae31data$Ba70_A11, by = list(hour = corrae31data$hour), FUN = function(x) mean(x, na.rm = TRUE))




plot(hourly_avg1$hour, hourly_avg1$x, type = "l", col = "red", 
     xlab = "hour", ylab = expression("Mm"^{-1}), 
     ylim = c(0, max(hourly_avg1$x, na.rm = TRUE) * 1),
     xaxt = "n")

# Weitere Linien hinzufügen
lines(hourly_avg2$hour, hourly_avg2$x, type = "l", col = "blue")
lines(hourly_avg3$hour, hourly_avg3$x, type = "l", col = "green")
lines(hourly_avg4$hour, hourly_avg4$x, type = "l", col = "purple")
lines(hourly_avg5$hour, hourly_avg5$x, type = "l", col = "orange")
lines(hourly_avg6$hour, hourly_avg6$x, type = "l", col = "brown")
lines(hourly_avg7$hour, hourly_avg7$x, type = "l", col = "black")

# Legende hinzufügen
legend("topright", legend = c("370 nm", "470 nm", "521 nm", "590 nm", "660 nm", "880 nm", "950 nm"), 
       col = c("red", "blue", "green", "purple", "orange", "brown", "black"), lty = 1)
title("hourly averaged absorption coefficient")

# X-Achsenbeschriftung einstellen
axis(1, at = NA)

axis(side = 1, at = hourly_avg1$hour, labels = paste0(hourly_avg1$hour, ":00"), las = 2, cex.axis = 0.9)


#----------------------------------------------------------------------------------------------------
# Tagesgang Aurora3000
#----------------------------------------------------------------------------------------------------

# Zuerst füge eine Spalte für die Stunde hinzu
corrae31data$hour <- as.numeric(format(time, "%H"))
corrnephdata$BsB0_S11 <- as.numeric(corrnephdata$BsB0_S11)
corrnephdata$BsG0_S11 <- as.numeric(corrnephdata$BsG0_S11)
corrnephdata$BsR0_S11 <- as.numeric(corrnephdata$BsR0_S11)

corrnephdata$BbsB0_S11 <- as.numeric(corrnephdata$BbsB0_S11)
corrnephdata$BbsG0_S11 <- as.numeric(corrnephdata$BbsG0_S11)
corrnephdata$BbsR0_S11 <- as.numeric(corrnephdata$BbsR0_S11)



# Dann gruppieren nach Stunde und den Durchschnitt berechnen
hourly_avgtb <- aggregate(corrnephdata$BsB0_S11, by = list(hour = corrae31data$hour), FUN = function(x) mean(x, na.rm = TRUE))
hourly_avgtg <- aggregate(corrnephdata$BsG0_S11, by = list(hour = corrae31data$hour), FUN = function(x) mean(x, na.rm = TRUE))
hourly_avgtr <- aggregate(corrnephdata$BsR0_S11, by = list(hour = corrae31data$hour), FUN = function(x) mean(x, na.rm = TRUE))

hourly_avgbb <- aggregate(corrnephdata$BbsB0_S11, by = list(hour = corrae31data$hour), FUN = function(x) mean(x, na.rm = TRUE))
hourly_avgbg <- aggregate(corrnephdata$BbsG0_S11, by = list(hour = corrae31data$hour), FUN = function(x) mean(x, na.rm = TRUE))
hourly_avgbr <- aggregate(corrnephdata$BbsR0_S11, by = list(hour = corrae31data$hour), FUN = function(x) mean(x, na.rm = TRUE))

# Jetzt hast du einen Dataframe mit zwei Spalten: 'hour' und 'x' (der Durchschnittswert für jede Stunde)

# Plot erstellen mit angepasster y-Achse
plot(hourly_avgtb$hour, hourly_avgtb$x, col = "blue", type = "l", xlab = "hour", ylab = expression("Mm"^{-1}), main = "hourly averaged scattering coefficient", xaxt = "n", ylim = c(min(hourly_avgtb$x, hourly_avgtg$x, hourly_avgtr$x, hourly_avgbb$x, hourly_avgbg$x, hourly_avgbr$x), max(hourly_avgtb$x, hourly_avgtg$x, hourly_avgtr$x, hourly_avgbb$x, hourly_avgbg$x, hourly_avgbr$x)))
lines(hourly_avgtg$hour, hourly_avgtg$x, col = "green")
lines(hourly_avgtr$hour, hourly_avgtr$x, col = "red")

lines(hourly_avgbb$hour, hourly_avgbb$x, col = "blue")
lines(hourly_avgbg$hour, hourly_avgbg$x, col = "green")
lines(hourly_avgbr$hour, hourly_avgbr$x, col = "red")

# x-Achse beschriften
axis(side = 1, at = hourly_avgtb$hour, labels = paste0(hourly_avgtb$hour, ":00"), las = 2, cex.axis = 0.9)

# Legende hinzufügen
legend("topright", legend = c("BsB0_S11", "BsG0_S11", "BsR0_S11", "BbsB0_S11", "BbsG0_S11", "BbsR0_S11"), 
       col = c("blue", "green", "red", "blue", "green", "red"), lty = 1, title.col = "black", cex = 0.8)
#----------------------------------------------------------------------------------------------------
# SSA | Vietnam
#----------------------------------------------------------------------------------------------------

#SSA dry

corrae31data$Ba10_A11 <- as.numeric(corrae31data$Ba10_A11)
corrae31data$Ba20_A11 <- as.numeric(corrae31data$Ba20_A11)
corrae31data$Ba30_A11 <- as.numeric(corrae31data$Ba30_A11)
corrae31data$Ba40_A11 <- as.numeric(corrae31data$Ba40_A11)
corrae31data$Ba50_A11 <- as.numeric(corrae31data$Ba50_A11)
corrae31data$Ba60_A11 <- as.numeric(corrae31data$Ba60_A11)
corrae31data$Ba70_A11 <- as.numeric(corrae31data$Ba70_A11)

corrnephdata$BsB0_S11<- as.numeric(corrnephdata$BsB0_S11)
corrnephdata$BsG0_S11<- as.numeric(corrnephdata$BsG0_S11)
corrnephdata$BsR0_S11<- as.numeric(corrnephdata$BsR0_S11)

corrae31data$hour <- as.numeric(format(time, "%H"))

# Berechnung von AAE ohne NA-Werte
AAE1 <- -log(corrae31data$Ba10_A11/corrae31data$Ba20_A11)/log(370/470)
AAE2 <- -log(corrae31data$Ba20_A11/corrae31data$Ba30_A11)/log(470/521)
AAE3 <- -log(corrae31data$Ba30_A11/corrae31data$Ba40_A11)/log(521/590)
AAE4 <- -log(corrae31data$Ba40_A11/corrae31data$Ba50_A11)/log(590/660)
AAE5 <- -log(corrae31data$Ba50_A11/corrae31data$Ba60_A11)/log(660/880)
AAE6 <- -log(corrae31data$Ba60_A11/corrae31data$Ba70_A11)/log(880/950)


Abs_450 <- corrae31data$Ba10_A11*(450/470)^(-1 * AAE1)
Abs_525 <- corrae31data$Ba40_A11*(525/590)^(-1 * AAE3)
Abs_635 <- corrae31data$Ba50_A11*(635/660)^(-1 * AAE4)


bextb <- Abs_450 + corrnephdata$BsB0_S11
bextg <- Abs_525 + corrnephdata$BsG0_S11
bextr <- Abs_635 + corrnephdata$BsR0_S11

SSAdryb <- corrnephdata$BsB0_S11/bextb
SSAdryg <- corrnephdata$BsG0_S11/bextg
SSAdryr <- corrnephdata$BsR0_S11/bextr

# Zuerst füge eine Spalte für die Stunde hinzu
corrae31data$hour <- as.numeric(format(time, "%H"))

# Dann gruppieren nach Stunde und den Durchschnitt berechnen
hourly_avgb <- aggregate(SSAdryb, by = list(hour = corrae31data$hour), FUN = function(x) mean(x, na.rm = TRUE))
hourly_avgg <- aggregate(SSAdryg, by = list(hour = corrae31data$hour), FUN = function(x) mean(x, na.rm = TRUE))
hourly_avgr <- aggregate(SSAdryr, by = list(hour = corrae31data$hour), FUN = function(x) mean(x, na.rm = TRUE))

# Jetzt hast du einen Dataframe mit zwei Spalten: 'hour' und 'x' (der Durchschnittswert für jede Stunde)

# Plot erstellen mit angepasster y-Achse
plot(hourly_avgb$hour, hourly_avgb$x, col = "blue", type = "l", xlab = "hour", ylab = "SSA dry", main = "hourly averaged SSA dry values", xaxt = "n", ylim = c(min(hourly_avgb$x, hourly_avgg$x, hourly_avgr$x), max(hourly_avgb$x, hourly_avgg$x, hourly_avgr$x)))
lines(hourly_avgg$hour, hourly_avgg$x, col = "green")
lines(hourly_avgr$hour, hourly_avgr$x, col = "red")

# x-Achse beschriften
axis(side = 1, at = hourly_avgb$hour, labels = paste0(hourly_avgb$hour, ":00"), las = 2, cex.axis = 0.9)




#----------------------------------------------------------------------------------------------------
# AAE 
#----------------------------------------------------------------------------------------------------

# Konvertieren der Spalten in numerische Werte

# Ersetzen von NA-Werten durch geeignete Werte (z.B. 0)
corrae31data$Ba10_A11 <- as.numeric(corrae31data$Ba10_A11)
corrae31data$Ba20_A11 <- as.numeric(corrae31data$Ba20_A11)
corrae31data$Ba30_A11 <- as.numeric(corrae31data$Ba30_A11)
corrae31data$Ba40_A11 <- as.numeric(corrae31data$Ba40_A11)
corrae31data$Ba50_A11 <- as.numeric(corrae31data$Ba50_A11)
corrae31data$Ba60_A11 <- as.numeric(corrae31data$Ba60_A11)
corrae31data$Ba70_A11 <- as.numeric(corrae31data$Ba70_A11)


# Berechnung von AAE ohne NA-Werte
AAE1 <- -log(corrae31data$Ba10_A11/corrae31data$Ba20_A11)/log(370/470)
AAE2 <- -log(corrae31data$Ba20_A11/corrae31data$Ba30_A11)/log(470/521)
AAE3 <- -log(corrae31data$Ba30_A11/corrae31data$Ba40_A11)/log(521/590)
AAE4 <- -log(corrae31data$Ba40_A11/corrae31data$Ba50_A11)/log(590/660)
AAE5 <- -log(corrae31data$Ba50_A11/corrae31data$Ba60_A11)/log(660/880)
AAE6 <- -log(corrae31data$Ba60_A11/corrae31data$Ba70_A11)/log(880/950)



# Zuerst füge eine Spalte für die Stunde hinzu
corrae31data$hour <- as.numeric(format(time, "%H"))

# Dann gruppieren nach Stunde und den Durchschnitt berechnen
hourly_avg1 <- aggregate(AAE1, by = list(hour = corrae31data$hour), FUN = function(x) mean(x, na.rm = TRUE))
hourly_avg2 <- aggregate(AAE2, by = list(hour = corrae31data$hour), FUN = function(x) mean(x, na.rm = TRUE))
hourly_avg3 <- aggregate(AAE3, by = list(hour = corrae31data$hour), FUN = function(x) mean(x, na.rm = TRUE))
hourly_avg4 <- aggregate(AAE4, by = list(hour = corrae31data$hour), FUN = function(x) mean(x, na.rm = TRUE))
hourly_avg5 <- aggregate(AAE5, by = list(hour = corrae31data$hour), FUN = function(x) mean(x, na.rm = TRUE))
hourly_avg6 <- aggregate(AAE6, by = list(hour = corrae31data$hour), FUN = function(x) mean(x, na.rm = TRUE))

# Plot der durchschnittlichen AAE-Werte nach Stunden
# Plot der durchschnittlichen AAE-Werte nach Stunden
# Plot erstellen
plot(hourly_avg1$hour, hourly_avg1$x, type = "l", col = "red", 
     xlab = "hour", ylab = "AAE", 
     ylim = c(0, max(hourly_avg1$x, na.rm = TRUE) * 3),
     xaxt = "n")

# Weitere Linien hinzufügen
lines(hourly_avg2$hour, hourly_avg2$x, type = "l", col = "blue")
lines(hourly_avg3$hour, hourly_avg3$x, type = "l", col = "green")
lines(hourly_avg4$hour, hourly_avg4$x, type = "l", col = "purple")
lines(hourly_avg5$hour, hourly_avg5$x, type = "l", col = "orange")
lines(hourly_avg6$hour, hourly_avg6$x, type = "l", col = "brown")

# Legende hinzufügen
legend("topright", legend = c("370-470 nm", "470-521 nm", "521-590 nm", "590-660 nm", "660-880 nm", "880-950 nm"), 
       col = c("red", "blue", "green", "purple", "orange", "brown"), lty = 1)
title("hourly averaged AAE values")

# X-Achsenbeschriftung einstellen
axis(1, at = NA)

axis(side = 1, at = hourly_avg1$hour, labels = paste0(hourly_avg1$hour, ":00"), las = 2, cex.axis = 0.9)


#----------------------------------------------------------------------------------------------------
# backscatter fraction
#----------------------------------------------------------------------------------------------------

#Total Scattering
corrnephdata$BsB0_S11 <- as.numeric(corrnephdata$BsB0_S11)
corrnephdata$BsG0_S11 <- as.numeric(corrnephdata$BsG0_S11)
corrnephdata$BsR0_S11 <- as.numeric(corrnephdata$BsR0_S11)

#Backscattering
corrnephdata$BbsB0_S11 <- as.numeric(corrnephdata$BbsB0_S11)
corrnephdata$BbsG0_S11 <- as.numeric(corrnephdata$BbsG0_S11)
corrnephdata$BbsR0_S11 <- as.numeric(corrnephdata$BbsR0_S11)

corrae31data$hour <- as.numeric(format(time, "%H"))


blue <- corrnephdata$BbsB0_S11/corrnephdata$BsB0_S11
green <- corrnephdata$BbsG0_S11/corrnephdata$BsG0_S11
red <- corrnephdata$BbsR0_S11/corrnephdata$BsR0_S11

hourly_avg1 <- aggregate(blue, by = list(hour = corrae31data$hour), FUN = function(x) mean(x, na.rm = TRUE))
hourly_avg2 <- aggregate(green, by = list(hour = corrae31data$hour), FUN = function(x) mean(x, na.rm = TRUE))
hourly_avg3 <- aggregate(red, by = list(hour = corrae31data$hour), FUN = function(x) mean(x, na.rm = TRUE))

plot(hourly_avg1$hour, hourly_avg1$x, type = "l", col = "red", 
     xlab = "hour", ylab = "b", 
     ylim = c(0, max(hourly_avg1$x, na.rm = TRUE) * 1.5),
     xaxt = "n")
# Weitere Linien hinzufügen
lines(hourly_avg2$hour, hourly_avg2$x, type = "l", col = "blue")
lines(hourly_avg3$hour, hourly_avg3$x, type = "l", col = "green")


# Legende hinzufügen
legend("topright", legend = c("blue", "green", "red"), 
       col = c("blue", "green", "red"), lty = 1)
title("hourly averaged backscatter fraction")

# X-Achsenbeschriftung einstellen
axis(1, at = NA)

axis(side = 1, at = hourly_avg1$hour, labels = paste0(hourly_avg1$hour, ":00"), las = 2, cex.axis = 0.9)


#----------------------------------------------------------------------------------------------------
# SAE
#----------------------------------------------------------------------------------------------------

# Ersetzen von NA-Werten durch geeignete Werte (z.B. 0)

corrnephdata$BsB0_S11 <- as.numeric(corrnephdata$BsB0_S11)
corrnephdata$BsG0_S11 <- as.numeric(corrnephdata$BsG0_S11)
corrnephdata$BsR0_S11 <- as.numeric(corrnephdata$BsR0_S11)

# Berechnung von AAE ohne NA-Werte
AAE1 <- -log(corrnephdata$BsB0_S11/corrnephdata$BsG0_S11)/log(450/525)
AAE2 <- -log(corrnephdata$BsG0_S11/corrnephdata$BsR0_S11)/log(525/635)


# Zuerst füge eine Spalte für die Stunde hinzu
corrae31data$hour <- as.numeric(format(time, "%H"))

# Dann gruppieren nach Stunde und den Durchschnitt berechnen
hourly_avg1 <- aggregate(AAE1, by = list(hour = corrae31data$hour), FUN = function(x) mean(x, na.rm = TRUE))
hourly_avg2 <- aggregate(AAE2, by = list(hour = corrae31data$hour), FUN = function(x) mean(x, na.rm = TRUE))


# Plot der durchschnittlichen AAE-Werte nach Stunden
# Plot der durchschnittlichen AAE-Werte nach Stunden
# Plot erstellen
plot(hourly_avg1$hour, hourly_avg1$x, type = "l", col = "red", 
     xlab = "hour", ylab = "SAE", 
     ylim = c(0, max(hourly_avg1$x, na.rm = TRUE) * 1.5), xaxt = "n")

# Weitere Linien hinzufügen
lines(hourly_avg2$hour, hourly_avg2$x, type = "l", col = "blue")


# Legende hinzufügen
legend("topright", legend = c("450-525 nm", "525-635 nm"), 
       col = c("red", "blue"), lty = 1)
title("hourly averaged SAE values")

# X-Achsenbeschriftung einstellen
axis(side = 1, at = hourly_avg1$hour, labels = paste0(hourly_avg1$hour, ":00"), las = 2, cex.axis = 0.9)


