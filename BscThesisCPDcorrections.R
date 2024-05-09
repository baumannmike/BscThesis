#################################################################################################################################
# Input
#################################################################################################################################

acorrected <- "/Users/baumannmike/Desktop/Datenbeispiele/Datenbeispiele/2019_03_01_5y_MKN_AE31_1hr_corrected_fixed_STP.csv"
corrae31data <- read.csv(acorrected)


ncorrected <- "/Users/baumannmike/Desktop/Datenbeispiele/Datenbeispiele/2019_03_01_5y_MKN_neph_1hr_corrected.csv"
corrnephdata <- read.csv(ncorrected)

time <- as.POSIXct(corrae31data$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

#################################################################################################################################
# 
#################################################################################################################################

corrae31data$hour <- as.numeric(format(time, "%m"))
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
     xlab = "", ylab = expression("Mm"^{-1}), 
     ylim = c(0, max(hourly_avg1$x, na.rm = TRUE) * 1),
     xaxt = "n")

# Weitere Linien hinzufügen
lines(hourly_avg2$hour, hourly_avg2$x, type = "l", col = "orange")
lines(hourly_avg3$hour, hourly_avg3$x, type = "l", col = "yellow")
lines(hourly_avg4$hour, hourly_avg4$x, type = "l", col = "green")
lines(hourly_avg5$hour, hourly_avg5$x, type = "l", col = "blue")
lines(hourly_avg6$hour, hourly_avg6$x, type = "l", col = "purple")
lines(hourly_avg7$hour, hourly_avg7$x, type = "l", col = "brown")

# Legende hinzufügen
legend("topright", legend = c("370 nm", "470 nm", "521 nm", "590 nm", "660 nm", "880 nm", "950 nm"), 
       col = c("red", "orange", "yellow", "green", "blue", "purple", "brown"), lty = 1)
title("monthly averaged absorption coefficient")

# X-Achsenbeschriftung einstellen
axis(1, at = NA)

month_to_name <- function(month_num) {
  month_names <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
  return(month_names[month_num])
}

axis(side = 1, at = unique(corrae31data$hour), labels = month_to_name(unique(corrae31data$hour)), las = 2, cex.axis = 0.9)

#################################################################################################################################
# Daily Absorption
#################################################################################################################################

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
     xlab = "hour", ylab = expression("Absorption Coefficent [Mm"^-1*"]"), 
     ylim = c(0, max(hourly_avg1$x, na.rm = TRUE) * 1),
     xaxt = "n")

# Weitere Linien hinzufügen
lines(hourly_avg2$hour, hourly_avg2$x, type = "l", col = "orange")
lines(hourly_avg3$hour, hourly_avg3$x, type = "l", col = "yellow")
lines(hourly_avg4$hour, hourly_avg4$x, type = "l", col = "green")
lines(hourly_avg5$hour, hourly_avg5$x, type = "l", col = "blue")
lines(hourly_avg6$hour, hourly_avg6$x, type = "l", col = "purple")
lines(hourly_avg7$hour, hourly_avg7$x, type = "l", col = "brown")

# Legende hinzufügen
legend("topright", legend = c("370 nm", "470 nm", "521 nm", "590 nm", "660 nm", "880 nm", "950 nm"), 
       col = c("red", "orange", "yellow", "green", "blue", "purple", "brown"), lty = 1)
title("hourly averaged absorption coefficient")

# X-Achsenbeschriftung einstellen
axis(1, at = NA)

axis(side = 1, at = hourly_avg1$hour, labels = paste0(hourly_avg1$hour, ":00"), las = 2, cex.axis = 0.9)

#################################################################################################################################
# V2 Boxplot
#################################################################################################################################

corrae31data$hour <- as.numeric(format(time, "%H"))
corrae31data$Ba10_A11 <- as.numeric(corrae31data$Ba10_A11)
corrae31data$Ba20_A11 <- as.numeric(corrae31data$Ba20_A11)



# Boxplot erstellen
boxplot(corrae31data$Ba10_A11 ~ corrae31data$hour,
        xlab = "Monat",
        ylab = "Absorptionswert",
        main = "Boxplot der Absorptionswerte nach Monat",
       )










# Erstelle den Boxplot ohne Ausreißer

ggplot(corrae31data, aes(x = factor(hour), y = Ba10_A11)) +
  geom_boxplot() +
  labs(x = "Monat", y = "Absorptionswert") +
  ggtitle("Boxplot der Absorptionswerte nach Monat")

# Berechne den Median und die Standardabweichung für jeden Monat
medians <- aggregate(Ba20_A11 ~ hour, data = corrae31data, FUN = median)
std_devs <- aggregate(Ba20_A11 ~ hour, data = corrae31data, FUN = sd)

# Erstelle ein Balkendiagramm für den Median und vertikale Linien für die Standardabweichung
ggplot() +
  geom_point(data = medians, aes(x = factor(hour), y = Ba20_A11), color = "red", size = 3) +
  geom_errorbar(data = std_devs, aes(x = factor(hour), ymin = Ba20_A11 - Ba20_A11, ymax = Ba20_A11 + Ba20_A11), width = 0.2, color = "red", size = 1) +
  geom_line(data = medians, aes(x = factor(hour), y = Ba20_A11, group = 1), color = "red", size = 1) +  # Verbinde Medianwerte mit Linien
  labs(x = "Monat", y = "Absorptionswert", title = "Median und Standardabweichung der Absorptionswerte nach Monat")



corrnephdata$P0_S11 <- as.numeric(corrnephdata$P0_S11)
Pmedian <- mean(corrnephdata$P0_S11, na.rm = TRUE)

corrnephdata$T10_S11 <- as.numeric(corrnephdata$T10_S11)
Tinstr <- mean((corrnephdata$T10_S11 + 273.15), na.rm=TRUE)


#################################################################################################################################
# Daily Aurora3000
#################################################################################################################################


par(oma = c(0, 0, 0, 0))

# Teilt das Grafikfenster in 2 Reihen und 1 Spalte auf
par(mfrow = c(2, 1), mar = c(3, 5, 2, 2) + 0.1)

# Erster Plot mit den ersten drei Linien
plot(hourly_avgtb$hour, hourly_avgtb$x, col = "blue", type = "l", 
     xlab = "", ylab = expression(paste(sigma[TS], " [Mm"^-1*"]")), 
     main = "hourly averaged scattering coefficient", 
     xaxt = "n", 
     ylim = c(min(hourly_avgtb$x, hourly_avgtg$x, hourly_avgtr$x), 
              max(hourly_avgtb$x, hourly_avgtg$x, hourly_avgtr$x)))
lines(hourly_avgtg$hour, hourly_avgtg$x, col = "green")
lines(hourly_avgtr$hour, hourly_avgtr$x, col = "red")


# Zweiter Plot mit den letzten drei Linien
plot(hourly_avgbb$hour, hourly_avgbb$x, col = "blue", type = "l", 
     xlab = "hour", ylab = expression(paste(sigma[BS], " [Mm"^-1*"]")) ,  
     xaxt = "n", 
     ylim = c(min(hourly_avgbb$x, hourly_avgbg$x, hourly_avgbr$x), 
              max(hourly_avgbb$x, hourly_avgbg$x, hourly_avgbr$x)))
lines(hourly_avgbg$hour, hourly_avgbg$x, col = "green")
lines(hourly_avgbr$hour, hourly_avgbr$x, col = "red")
legend("topright", legend = c("450 nm", "525 nm", "635 nm"), col = c("blue", "green", "red"), lty = 1)

axis(side = 1, at = hourly_avgtb$hour, labels = paste0(hourly_avgtb$hour, ":00"), las = 2, cex.axis = 0.9)

#################################################################################################################################
# AAE
#################################################################################################################################


# Konvertieren der Spalten in numerische Werte

# Ersetzen von NA-Werten durch geeignete Werte (z.B. 0)
corrae31data$Ba10_A11 <- as.numeric(corrae31data$Ba10_A11)
corrae31data$Ba20_A11 <- as.numeric(corrae31data$Ba20_A11)
corrae31data$Ba30_A11 <- as.numeric(corrae31data$Ba30_A11)
corrae31data$Ba40_A11 <- as.numeric(corrae31data$Ba40_A11)
corrae31data$Ba50_A11 <- as.numeric(corrae31data$Ba50_A11)
corrae31data$Ba60_A11 <- as.numeric(corrae31data$Ba60_A11)
corrae31data$Ba70_A11 <- as.numeric(corrae31data$Ba70_A11)

# Zuerst füge eine Spalte für die Stunde hinzu
corrae31data$hour <- as.numeric(format(time, "%H"))

# Berechnung von AAE ohne NA-Werte

AAE1 <- -log(corrae31data$Ba20_A11/corrae31data$Ba60_A11)/log(470/880)



# Dann gruppieren nach Stunde und den Durchschnitt berechnen

hourly_avg1 <- aggregate(AAE1, 
                           by = list(hour = corrae31data$hour), 
                           FUN = median , na.rm = TRUE)



# Plot der durchschnittlichen AAE-Werte nach Stunden
# Plot erstellen
plot(hourly_avg1$hour, hourly_avg1$x, type = "l", col = "black", 
     xlab = "hour", ylab = "AAE", 
     ylim = c(0, max(hourly_avg1$x, na.rm = TRUE) * 1.5),
     xaxt = "n")


# Legende hinzufügen
title("hourly median AAE value [470-880 nm]")

# X-Achsenbeschriftung einstellen
axis(1, at = NA)

axis(side = 1, at = hourly_avg1$hour, labels = paste0(hourly_avg1$hour, ":00"), las = 2, cex.axis = 0.9)

#################################################################################################################################
# SAE
#################################################################################################################################

corrnephdata$BsB0_S11 <- as.numeric(corrnephdata$BsB0_S11)
corrnephdata$BsG0_S11 <- as.numeric(corrnephdata$BsG0_S11)
corrnephdata$BsR0_S11 <- as.numeric(corrnephdata$BsR0_S11)
time <- as.POSIXct(corrnephdata$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
corrnephdata$hour <- as.numeric(format(time, "%H"))

# Berechnung von AAE ohne NA-Werte
AAE1 <- -log(corrnephdata$BsB0_S11/corrnephdata$BsR0_S11)/log(450/635)

# Zuerst füge eine Spalte für die Stunde hinzu


# Dann gruppieren nach Stunde und den Durchschnitt berechnen
hourly_avg1 <- aggregate(AAE1, 
                         by = list(hour = corrnephdata$hour), 
                         FUN = median , na.rm = TRUE)


# Plot erstellen
plot(hourly_avg1$hour, hourly_avg1$x, type = "l", col = "black", 
     xlab = "hour", ylab = "SAE", 
     ylim = c(0, max(hourly_avg1$x, na.rm = TRUE) * 1.5), xaxt = "n")



# Legende hinzufügen
title("hourly median SAE values [450-635 nm]")

# X-Achsenbeschriftung einstellen
axis(side = 1, at = hourly_avg1$hour, labels = paste0(hourly_avg1$hour, ":00"), las = 2, cex.axis = 0.9)

#################################################################################################################################
# backsacatter fraction
#################################################################################################################################

#Total Scattering
corrnephdata$BsB0_S11 <- as.numeric(corrnephdata$BsB0_S11)
corrnephdata$BsG0_S11 <- as.numeric(corrnephdata$BsG0_S11)
corrnephdata$BsR0_S11 <- as.numeric(corrnephdata$BsR0_S11)

#Backscattering
corrnephdata$BbsB0_S11 <- as.numeric(corrnephdata$BbsB0_S11)
corrnephdata$BbsG0_S11 <- as.numeric(corrnephdata$BbsG0_S11)
corrnephdata$BbsR0_S11 <- as.numeric(corrnephdata$BbsR0_S11)
time <- as.POSIXct(corrnephdata$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
corrnephdata$hour <- as.numeric(format(time, "%H"))


blue <- corrnephdata$BbsB0_S11/corrnephdata$BsB0_S11
green <- corrnephdata$BbsG0_S11/corrnephdata$BsG0_S11
red <- corrnephdata$BbsR0_S11/corrnephdata$BsR0_S11

hourly_avg1 <- aggregate(blue, by = list(hour = corrnephdata$hour), FUN = function(x) median(x, na.rm = TRUE))
hourly_avg2 <- aggregate(green, by = list(hour = corrnephdata$hour), FUN = function(x) median(x, na.rm = TRUE))
hourly_avg3 <- aggregate(red, by = list(hour = corrnephdata$hour), FUN = function(x) median(x, na.rm = TRUE))

plot(hourly_avg1$hour, hourly_avg1$x, type = "l", col = "red", 
     xlab = "hour", ylab = "b", 
     ylim = c(0, max(hourly_avg1$x, na.rm = TRUE) * 1.5),
     xaxt = "n")
# Weitere Linien hinzufügen
lines(hourly_avg2$hour, hourly_avg2$x, type = "l", col = "blue")
lines(hourly_avg3$hour, hourly_avg3$x, type = "l", col = "green")


# Legende hinzufügen
legend("topright", legend = c("450 nm", "525 nm", "635 nm"), 
       col = c("blue", "green", "red"), lty = 1)
title("hourly median backscatter fraction ")

# X-Achsenbeschriftung einstellen
axis(1, at = NA)

axis(side = 1, at = hourly_avg1$hour, labels = paste0(hourly_avg1$hour, ":00"), las = 2, cex.axis = 0.9)

#################################################################################################################################
# SSA dry
#################################################################################################################################

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

time <- as.POSIXct(corrae31data$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
corrae31data$hour <- as.numeric(format(time, "%H"))

# Berechnung von AAE ohne NA-Werte

AAE1 <- -log(corrae31data$Ba20_A11/corrae31data$Ba60_A11)/log(470/880)

Abs_450 <- corrae31data$Ba10_A11*(450/470)^(-1 * AAE1)
Abs_525 <- corrae31data$Ba40_A11*(525/590)^(-1 * AAE1)
Abs_635 <- corrae31data$Ba50_A11*(635/660)^(-1 * AAE1)


time <- as.POSIXct(corrae31data$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
time2 <- as.POSIXct(corrnephdata$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

time2_not_in_time <- time2[!time2 %in% time]

merged_data <- merge(corrae31data, corrnephdata, by = "DateTimeUTC", all.x = TRUE)

bextb <- Abs_450 + merged_data$BsB0_S11
bextg <- Abs_525 + merged_data$BsG0_S11
bextr <- Abs_635 + merged_data$BsR0_S11

SSAdryb <- merged_data$BsB0_S11/bextb
SSAdryg <- merged_data$BsG0_S11/bextg
SSAdryr <- merged_data$BsR0_S11/bextr

# Zuerst füge eine Spalte für die Stunde hinzu
merged_data$hour <- as.numeric(format(time, "%H"))

# Dann gruppieren nach Stunde und den Durchschnitt berechnen
hourly_avgb <- aggregate(SSAdryb, by = list(hour = merged_data$hour), FUN = function(x) mean(x, na.rm = TRUE))
hourly_avgg <- aggregate(SSAdryg, by = list(hour = merged_data$hour), FUN = function(x) mean(x, na.rm = TRUE))
hourly_avgr <- aggregate(SSAdryr, by = list(hour = merged_data$hour), FUN = function(x) mean(x, na.rm = TRUE))

# Jetzt hast du einen Dataframe mit zwei Spalten: 'hour' und 'x' (der Durchschnittswert für jede Stunde)

# Plot erstellen mit angepasster y-Achse
plot(hourly_avgb$hour, hourly_avgb$x, col = "blue", type = "l", xlab = "hour", ylab = "SSA dry", main = "hourly averaged SSA dry values", xaxt = "n", ylim = c(min(hourly_avgb$x, hourly_avgg$x, hourly_avgr$x), max(hourly_avgb$x, hourly_avgg$x, hourly_avgr$x)))
lines(hourly_avgg$hour, hourly_avgg$x, col = "green")
lines(hourly_avgr$hour, hourly_avgr$x, col = "red")

# x-Achse beschriften
axis(side = 1, at = hourly_avgb$hour, labels = paste0(hourly_avgb$hour, ":00"), las = 2, cex.axis = 0.9)




