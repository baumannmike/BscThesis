#################################################################################################################################
# Input
#################################################################################################################################

acorrected <- "/Users/baumannmike/Desktop/Datenbeispiele/Datenbeispiele/2019_03_01_5y_MKN_AE31_1hr_corrected.csv"
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
title("mothly averaged absorption coefficient")

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
     xlab = "hour", ylab = expression("Mm"^{-1}), 
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

time <- as.POSIXct(corrnephdata$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
corrnephdata$hour <- as.numeric(format(time, "%H"))
corrnephdata$BsB0_S11 <- as.numeric(corrnephdata$BsB0_S11)
corrnephdata$BsG0_S11 <- as.numeric(corrnephdata$BsG0_S11)
corrnephdata$BsR0_S11 <- as.numeric(corrnephdata$BsR0_S11)

corrnephdata$BbsB0_S11 <- as.numeric(corrnephdata$BbsB0_S11)
corrnephdata$BbsG0_S11 <- as.numeric(corrnephdata$BbsG0_S11)
corrnephdata$BbsR0_S11 <- as.numeric(corrnephdata$BbsR0_S11)




# Dann gruppieren nach Stunde und den Durchschnitt berechnen
hourly_avgtb <- aggregate(corrnephdata$BsB0_S11, by = list(hour = corrnephdata$hour), FUN = function(x) mean(x, na.rm = TRUE))
hourly_avgtg <- aggregate(corrnephdata$BsG0_S11, by = list(hour = corrnephdata$hour), FUN = function(x) mean(x, na.rm = TRUE))
hourly_avgtr <- aggregate(corrnephdata$BsR0_S11, by = list(hour = corrnephdata$hour), FUN = function(x) mean(x, na.rm = TRUE))

hourly_avgbb <- aggregate(corrnephdata$BbsB0_S11, by = list(hour = corrnephdata$hour), FUN = function(x) mean(x, na.rm = TRUE))
hourly_avgbg <- aggregate(corrnephdata$BbsG0_S11, by = list(hour = corrnephdata$hour), FUN = function(x) mean(x, na.rm = TRUE))
hourly_avgbr <- aggregate(corrnephdata$BbsR0_S11, by = list(hour = corrnephdata$hour), FUN = function(x) mean(x, na.rm = TRUE))

# Jetzt hast du einen Dataframe mit zwei Spalten: 'hour' und 'x' (der Durchschnittswert für jede Stunde)

# Plot erstellen mit angepasster y-Achse
plot(hourly_avgtb$hour, hourly_avgtb$x, col = "blue", type = "l", xlab = "hour", ylab = expression("Mm"^{-1}), main = "hourly averaged scattering coefficient", xaxt = "n", ylim = c(min(hourly_avgtb$x, hourly_avgtg$x, hourly_avgtr$x, hourly_avgbb$x, hourly_avgbg$x, hourly_avgbr$x), max(hourly_avgtb$x, hourly_avgtg$x, hourly_avgtr$x, hourly_avgbb$x, hourly_avgbg$x, hourly_avgbr$x)))
lines(hourly_avgtg$hour, hourly_avgtg$x, col = "green")
lines(hourly_avgtr$hour, hourly_avgtr$x, col = "red")

lines(hourly_avgbb$hour, hourly_avgbb$x, col = "blue")
lines(hourly_avgbg$hour, hourly_avgbg$x, col = "green")
lines(hourly_avgbr$hour, hourly_avgbr$x, col = "red")

# x-Achse beschriften

# Legende hinzufügen



par(oma = c(0, 0, 0, 0))

# Teilt das Grafikfenster in 2 Reihen und 1 Spalte auf
par(mfrow = c(2, 1), mar = c(3, 5, 2, 2) + 0.1)

# Erster Plot mit den ersten drei Linien
plot(hourly_avgtb$hour, hourly_avgtb$x, col = "blue", type = "l", 
     xlab = "", ylab = expression("Mm"^{-1}), 
     main = "hourly averaged scattering coefficient", 
     xaxt = "n", 
     ylim = c(min(hourly_avgtb$x, hourly_avgtg$x, hourly_avgtr$x), 
              max(hourly_avgtb$x, hourly_avgtg$x, hourly_avgtr$x)))
lines(hourly_avgtg$hour, hourly_avgtg$x, col = "green")
lines(hourly_avgtr$hour, hourly_avgtr$x, col = "red")


# Zweiter Plot mit den letzten drei Linien
plot(hourly_avgbb$hour, hourly_avgbb$x, col = "blue", type = "l", 
     xlab = "hour", ylab = expression(paste(σ )) ,  
     xaxt = "n", 
     ylim = c(min(hourly_avgbb$x, hourly_avgbg$x, hourly_avgbr$x), 
              max(hourly_avgbb$x, hourly_avgbg$x, hourly_avgbr$x)))
lines(hourly_avgbg$hour, hourly_avgbg$x, col = "green")
lines(hourly_avgbr$hour, hourly_avgbr$x, col = "red")
legend("topright", legend = c("450 nm", "525 nm", "635 nm"), col = c("blue", "green", "red"), lty = 1)

axis(side = 1, at = hourly_avgtb$hour, labels = paste0(hourly_avgtb$hour, ":00"), las = 2, cex.axis = 0.9)


