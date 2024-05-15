#################################################################################################################################
# Input
#################################################################################################################################

acorrected <- "/Users/baumannmike/Desktop/Datenbeispiele/Datenbeispiele/2019_03_01_5y_MKN_AE31_1hr_corrected_fixed_STP2.csv"
corrae31data <- read.csv(acorrected)


ncorrected <- "/Users/baumannmike/Desktop/Datenbeispiele/Datenbeispiele/2019_03_01_5y_MKN_neph_1hr_corrected.csv"
corrnephdata <- read.csv(ncorrected)

time <- as.POSIXct(corrae31data$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

#################################################################################################################################
# Monthly AE31 for every year Box
#################################################################################################################################

time <- as.POSIXct(corrae31data$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
year <- format(time, "%Y")
data_2019 <- corrae31data[year == "2019", ]
data_2020 <- corrae31data[year == "2020", ]
data_2021 <- corrae31data[year == "2021", ]
data_2022 <- corrae31data[year == "2022", ]
data_2023 <- corrae31data[year == "2023", ]
data_2024 <- corrae31data[year == "2024", ]

data_2019$Ba30_A11 <- as.numeric(data_2019$Ba30_A11)
data_2020$Ba30_A11 <- as.numeric(data_2020$Ba30_A11)
data_2021$Ba30_A11 <- as.numeric(data_2021$Ba30_A11)
data_2022$Ba30_A11 <- as.numeric(data_2022$Ba30_A11)
data_2023$Ba30_A11 <- as.numeric(data_2023$Ba30_A11)
data_2024$Ba30_A11 <- as.numeric(data_2024$Ba30_A11)

data_2019$month <- format(as.POSIXct(data_2019$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"), "%m")
data_2020$month <- format(as.POSIXct(data_2020$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"), "%m")
data_2021$month <- format(as.POSIXct(data_2021$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"), "%m")
data_2022$month <- format(as.POSIXct(data_2022$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"), "%m")
data_2023$month <- format(as.POSIXct(data_2023$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"), "%m")
data_2024$month <- format(as.POSIXct(data_2024$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"), "%m")


library(gglpot)


# Berechne den Median und die Standardabweichung für jeden Monat
medians <- aggregate(Ba30_A11 ~ month, data = data_2019, FUN = median)
std_devs <- aggregate(Ba30_A11 ~ month, data = data_2019, FUN = sd)

medians2 <- aggregate(Ba30_A11 ~ month, data = data_2020, FUN = median)
std_devs2 <- aggregate(Ba30_A11 ~ month, data = data_2020, FUN = sd)

medians3 <- aggregate(Ba30_A11 ~ month, data = data_2021, FUN = median)
std_devs3 <- aggregate(Ba30_A11 ~ month, data = data_2021, FUN = sd)

medians4 <- aggregate(Ba30_A11 ~ month, data = data_2022, FUN = median)
std_devs4 <- aggregate(Ba30_A11 ~ month, data = data_2022, FUN = sd)

medians5 <- aggregate(Ba30_A11 ~ month, data = data_2023, FUN = median)
std_devs5 <- aggregate(Ba30_A11 ~ month, data = data_2023, FUN = sd)

medians6 <- aggregate(Ba30_A11 ~ month, data = data_2024, FUN = median)
std_devs6 <- aggregate(Ba30_A11 ~ month, data = data_2024, FUN = sd)

data <- merge(medians, std_devs, by = "month")
data2 <- merge(medians2, std_devs2, by = "month")
data3 <- merge(medians3, std_devs3, by = "month")
data4 <- merge(medians4, std_devs4, by = "month")
data5 <- merge(medians5, std_devs5, by = "month")
data6 <- merge(medians6, std_devs6, by = "month")

months_labels <- c("January", "February", "March", "April", "May", "June", 
                   "July", "August", "September", "October", "November", "December")


# ggplot erstellen
ggplot() +
  geom_point(data = data, aes(x = month, y = Ba30_A11.x, color = "data"), shape = 4, size = 3) +
  geom_errorbar(data = data, aes(x = month, ymin = Ba30_A11.x - Ba30_A11.y, ymax = Ba30_A11.x + Ba30_A11.y, color = "data"), 
                width = 0.2) +
  geom_line(data = data, aes(x = month, y = Ba30_A11.x, group = 1, color = "data"), linetype = "dashed") +
  geom_point(data = data2, aes(x = month, y = Ba30_A11.x, color = "data2"), shape = 4, size = 3) +
  geom_errorbar(data = data2, aes(x = month, ymin = Ba30_A11.x - Ba30_A11.y, ymax = Ba30_A11.x + Ba30_A11.y, color = "data2"), 
                width = 0.2) +
  geom_line(data = data2, aes(x = month, y = Ba30_A11.x, group = 1, color = "data2"), linetype = "dashed") +
  geom_point(data = data3, aes(x = month, y = Ba30_A11.x, color = "data3"), shape = 4, size = 3) +
  geom_errorbar(data = data3, aes(x = month, ymin = Ba30_A11.x - Ba30_A11.y, ymax = Ba30_A11.x + Ba30_A11.y, color = "data3"), 
                width = 0.2) +
  geom_line(data = data3, aes(x = month, y = Ba30_A11.x, group = 1, color = "data3"), linetype = "dashed") +
  geom_point(data = data4, aes(x = month, y = Ba30_A11.x, color = "data4"), shape = 4, size = 3) +
  geom_errorbar(data = data4, aes(x = month, ymin = Ba30_A11.x - Ba30_A11.y, ymax = Ba30_A11.x + Ba30_A11.y, color = "data4"), 
                width = 0.2) +
  geom_line(data = data4, aes(x = month, y = Ba30_A11.x, group = 1, color = "data4"), linetype = "dashed") +
  geom_point(data = data5, aes(x = month, y = Ba30_A11.x, color = "data5"), shape = 4, size = 3) +
  geom_errorbar(data = data5, aes(x = month, ymin = Ba30_A11.x - Ba30_A11.y, ymax = Ba30_A11.x + Ba30_A11.y, color = "data5"), 
                width = 0.2) +
  geom_line(data = data5, aes(x = month, y = Ba30_A11.x, group = 1, color = "data5"), linetype = "dashed") +
  geom_point(data = data6, aes(x = month, y = Ba30_A11.x, color = "data6"), shape = 4, size = 3) +
  geom_errorbar(data = data6, aes(x = month, ymin = Ba30_A11.x - Ba30_A11.y, ymax = Ba30_A11.x + Ba30_A11.y, color = "data6"), 
                width = 0.2) +
  geom_line(data = data6, aes(x = month, y = Ba30_A11.x, group = 1, color = "data6"), linetype = "dashed") +
  labs(title = "Monthly Absorption Coefficient [521nm]",
       x = "",
       y = expression("Absorption Coefficient [Mm"^-1*"]")) +
  scale_x_discrete(labels = months_labels) +
  scale_color_manual(name = "", 
                     labels = c("2019", "2020", "2021", "2022", "2023", "2024"),
                     values = c("purple", "blue", "green", "yellow", "orange", "brown")) +
  theme_minimal()+
theme(
  plot.title = element_text(face = "bold", hjust = 0.5) # Hier wird der Haupttitel formatiert
)


#################################################################################################################################
# daily AE31 for every hour Box
#################################################################################################################################

corrae31data$hour <- as.numeric(format(time, "%H"))
corrae31data$Ba30_A11 <- as.numeric(corrae31data$Ba30_A11)


# Dann gruppieren nach Stunde und den Durchschnitt berechnen

hourly_avg3 <- aggregate(corrae31data$Ba30_A11, by = list(hour = corrae31data$hour), FUN = function(x) mean(x, na.rm = TRUE))



library(gglpot)


# Berechne den Median und die Standardabweichung für jeden Monat
medians <- aggregate(Ba30_A11 ~ hour, data = corrae31data, FUN = median)
std_devs <- aggregate(Ba30_A11 ~ hour, data = corrae31data, FUN = sd)

data <- merge(medians, std_devs, by = "hour")



# ggplot erstellen
ggplot() +
  geom_point(data = data, aes(x = hour, y = Ba30_A11.x), shape = 4, size = 3, color = "red", show.legend = FALSE) +
  geom_errorbar(data = data, aes(x = hour, ymin = Ba30_A11.x - Ba30_A11.y, ymax = Ba30_A11.x + Ba30_A11.y), 
                width = 0.2, color = "red", show.legend = FALSE) +
  geom_line(data = data, aes(x = hour, y = Ba30_A11.x, group = 1), linetype = "dashed", color = "red", show.legend = FALSE) +
  labs(title = "Daily Absorption Coefficient [521nm]",
       x = "",
       y = expression("Absorption Coefficient [Mm"^-1*"]")) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5), # Hier wird der Haupttitel formatiert
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1) # Hier werden die x-Achsentexte um 90 Grad gedreht
  ) +
  scale_x_continuous(breaks = seq(0, 23, by = 1), labels = paste0(seq(0, 23), ":00")) +
  scale_y_continuous(breaks = seq(-10, 10, by = 1))






#################################################################################################################################
# Monthly AE31
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
     xlab = "", ylab = expression("Absorption Coefficent [Mm"^-1*"]"), 
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
title("monthly averaged absorption coefficent")

# X-Achsenbeschriftung einstellen
axis(1, at = NA)

month_to_name <- function(month_num) {
  month_names <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
  return(month_names[month_num])
}

axis(side = 1, at = unique(corrae31data$hour), labels = month_to_name(unique(corrae31data$hour)), las = 2, cex.axis = 0.9)


#################################################################################################################################
# Yearly Absorption
#################################################################################################################################

# Zuerst füge eine Spalte für die Stunde hinzu
corrae31data$hour <- as.numeric(format(time, "%Y"))

time <- as.POSIXct(corrae31data$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
corrae31data$Ba10_A11 <- as.numeric(corrae31data$Ba10_A11)
corrae31data$Ba20_A11 <- as.numeric(corrae31data$Ba20_A11)
corrae31data$Ba30_A11 <- as.numeric(corrae31data$Ba30_A11)
corrae31data$Ba40_A11 <- as.numeric(corrae31data$Ba40_A11)
corrae31data$Ba50_A11 <- as.numeric(corrae31data$Ba50_A11)
corrae31data$Ba60_A11 <- as.numeric(corrae31data$Ba60_A11)
corrae31data$Ba70_A11 <- as.numeric(corrae31data$Ba70_A11)




plot(time, corrae31data$Ba10_A11, type = "l", col = "purple", 
     xlab = "time", ylab = expression("Absorption Coefficient [Mm"^-1*"]")) 
     


# Weitere Linien hinzufügen
lines(time, corrae31data$Ba20_A11, type = "l", col = "blue")
lines(time, corrae31data$Ba30_A11, type = "l", col = "green")
lines(time, corrae31data$Ba40_A11, type = "l", col = "yellow")
lines(time, corrae31data$Ba50_A11, type = "l", col = "orange")
lines(time, corrae31data$Ba60_A11, type = "l", col = "red")
lines(time, corrae31data$Ba70_A11, type = "l", col = "brown")

# Legende hinzufügen
legend("topright", legend = c("370 nm", "470 nm", "521 nm", "590 nm", "660 nm", "880 nm", "950 nm"), 
       col = c("purple","blue", "green", "yellow", "orange", "red", "brown"), lty = 1)
title("Absorption Coefficient")



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




plot(hourly_avg1$hour, hourly_avg1$x, type = "l", col = "purple", 
     xlab = "hour", ylab = expression("Absorption Coefficient [Mm"^-1*"]"), 
     ylim = c(0, max(hourly_avg1$x, na.rm = TRUE) * 1),
     xaxt = "n")

# Weitere Linien hinzufügen
lines(hourly_avg2$hour, hourly_avg2$x, type = "l", col = "blue")
lines(hourly_avg3$hour, hourly_avg3$x, type = "l", col = "green")
lines(hourly_avg4$hour, hourly_avg4$x, type = "l", col = "yellow")
lines(hourly_avg5$hour, hourly_avg5$x, type = "l", col = "orange")
lines(hourly_avg6$hour, hourly_avg6$x, type = "l", col = "red")
lines(hourly_avg7$hour, hourly_avg7$x, type = "l", col = "brown")

# Legende hinzufügen
legend("topright", legend = c("370 nm", "470 nm", "521 nm", "590 nm", "660 nm", "880 nm", "950 nm"), 
       col = c("purple","blue", "green", "yellow", "orange", "red", "brown"), lty = 1)
title("Hourly Averaged Absorption Coefficient")

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








library(gglpot)

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
corrae31data$hour <- as.numeric(format(time, "%H"))
corrnephdata$BsB0_S11 <- as.numeric(corrnephdata$BsB0_S11)
corrnephdata$BsG0_S11 <- as.numeric(corrnephdata$BsG0_S11)
corrnephdata$BsR0_S11 <- as.numeric(corrnephdata$BsR0_S11)

corrnephdata$BbsB0_S11 <- as.numeric(corrnephdata$BbsB0_S11)
corrnephdata$BbsG0_S11 <- as.numeric(corrnephdata$BbsG0_S11)
corrnephdata$BbsR0_S11 <- as.numeric(corrnephdata$BbsR0_S11)
time <- as.POSIXct(corrnephdata$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
corrnephdata$hour <- as.numeric(format(time, "%H"))

hourly_avgtb <- aggregate(corrnephdata$BsB0_S11, by = list(hour = corrnephdata$hour), FUN = function(x) mean(x, na.rm = TRUE))
hourly_avgtg <- aggregate(corrnephdata$BsG0_S11, by = list(hour = corrnephdata$hour), FUN = function(x) mean(x, na.rm = TRUE))
hourly_avgtr <- aggregate(corrnephdata$BsR0_S11, by = list(hour = corrnephdata$hour), FUN = function(x) mean(x, na.rm = TRUE))

hourly_avgbb <- aggregate(corrnephdata$BbsB0_S11, by = list(hour = corrnephdata$hour), FUN = function(x) mean(x, na.rm = TRUE))
hourly_avgbg <- aggregate(corrnephdata$BbsG0_S11, by = list(hour = corrnephdata$hour), FUN = function(x) mean(x, na.rm = TRUE))
hourly_avgbr <- aggregate(corrnephdata$BbsR0_S11, by = list(hour = corrnephdata$hour), FUN = function(x) mean(x, na.rm = TRUE))


par(oma = c(0, 0, 0, 0))

# Teilt das Grafikfenster in 2 Reihen und 1 Spalte auf
par(mfrow = c(2, 1), mar = c(3, 5, 2, 2) + 0.1)

# Erster Plot mit den ersten drei Linien
plot(hourly_avgtb$hour, hourly_avgtb$x, col = "blue", type = "l", 
     xlab = "", ylab = expression(paste(sigma[TS], " [Mm"^-1*"]")), 
     main = "Hourly Averaged Scattering Coefficient", 
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
legend("topright", legend = c("450 nm", "525 nm", "635 nm"), col = c("red", "green", "blue"), lty = 1)

axis(side = 1, at = hourly_avgtb$hour, labels = paste0(hourly_avgtb$hour, ":00"), las = 2, cex.axis = 0.9)

#################################################################################################################################
# Aurora3000 month
#################################################################################################################################


corrnephdata$BsB0_S11 <- as.numeric(corrnephdata$BsB0_S11)
corrnephdata$BsG0_S11 <- as.numeric(corrnephdata$BsG0_S11)
corrnephdata$BsR0_S11 <- as.numeric(corrnephdata$BsR0_S11)

corrnephdata$BbsB0_S11 <- as.numeric(corrnephdata$BbsB0_S11)
corrnephdata$BbsG0_S11 <- as.numeric(corrnephdata$BbsG0_S11)
corrnephdata$BbsR0_S11 <- as.numeric(corrnephdata$BbsR0_S11)
time <- as.POSIXct(corrnephdata$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
corrnephdata$hour <- as.numeric(format(time, "%m"))

hourly_avgtb <- aggregate(corrnephdata$BsB0_S11, by = list(hour = corrnephdata$hour), FUN = function(x) mean(x, na.rm = TRUE))
hourly_avgtg <- aggregate(corrnephdata$BsG0_S11, by = list(hour = corrnephdata$hour), FUN = function(x) mean(x, na.rm = TRUE))
hourly_avgtr <- aggregate(corrnephdata$BsR0_S11, by = list(hour = corrnephdata$hour), FUN = function(x) mean(x, na.rm = TRUE))

hourly_avgbb <- aggregate(corrnephdata$BbsB0_S11, by = list(hour = corrnephdata$hour), FUN = function(x) mean(x, na.rm = TRUE))
hourly_avgbg <- aggregate(corrnephdata$BbsG0_S11, by = list(hour = corrnephdata$hour), FUN = function(x) mean(x, na.rm = TRUE))
hourly_avgbr <- aggregate(corrnephdata$BbsR0_S11, by = list(hour = corrnephdata$hour), FUN = function(x) mean(x, na.rm = TRUE))


# Teilt das Grafikfenster in 2 Reihen und 1 Spalte auf
par(mfrow = c(2, 1))

pin <- c(5, 2)  # Breite und Höhe des inneren Plots in Zoll
plt <- c(0.5, 4, 0.5, 4) 

# Erster Plot mit den ersten drei Linien
plot(hourly_avgtb$hour, hourly_avgtb$x, col = "blue", type = "l", 
     xlab = "", ylab = expression(paste(sigma[TS], " [Mm"^-1*"]")), 
     main = "Monthly Averaged Scattering Coefficient", 
     xaxt = "n", 
     ylim = c(min(hourly_avgtb$x, hourly_avgtg$x, hourly_avgtr$x), 
              max(hourly_avgtb$x, hourly_avgtg$x, hourly_avgtr$x)))
lines(hourly_avgtg$hour, hourly_avgtg$x, col = "green")
lines(hourly_avgtr$hour, hourly_avgtr$x, col = "red")


plot(hourly_avgbb$hour, hourly_avgbb$x, col = "blue", type = "l", 
     xlab = "", ylab = expression(paste(sigma[BS], " [Mm"^-1*"]")) ,  
     xaxt = "n", 
     ylim = c(min(hourly_avgbb$x, hourly_avgbg$x, hourly_avgbr$x), 
              max(hourly_avgbb$x, hourly_avgbg$x, hourly_avgbr$x)))
lines(hourly_avgbg$hour, hourly_avgbg$x, col = "green")
lines(hourly_avgbr$hour, hourly_avgbr$x, col = "red")
legend("topright", legend = c("450 nm", "525 nm", "635 nm"), col = c("red", "green", "blue"), lty = 1)

month_to_name <- function(month_num) {
  month_names <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
  return(month_names[month_num])
}

# Hier setzen wir die x-Achsenbeschriftung für den ersten Plot
axis(side = 1, at = unique(hourly_avgtb$hour), labels = month_to_name(unique(hourly_avgtb$hour)), las = 2, cex.axis = 0.9)


#################################################################################################################################
# Aurora3000 monthly Box year Total Scattering
#################################################################################################################################


time <- as.POSIXct(corrnephdata$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
year <- format(time, "%Y")
data_2019 <- corrnephdata[year == "2019", ]
data_2020 <- corrnephdata[year == "2020", ]
data_2021 <- corrnephdata[year == "2021", ]
data_2022 <- corrnephdata[year == "2022", ]
data_2023 <- corrnephdata[year == "2023", ]
data_2024 <- corrnephdata[year == "2024", ]

data_2019$BsG0_S11 <- as.numeric(data_2019$BsG0_S11)
data_2020$BsG0_S11 <- as.numeric(data_2020$BsG0_S11)
data_2021$BsG0_S11 <- as.numeric(data_2021$BsG0_S11)
data_2022$BsG0_S11 <- as.numeric(data_2022$BsG0_S11)
data_2023$BsG0_S11 <- as.numeric(data_2023$BsG0_S11)
data_2024$BsG0_S11 <- as.numeric(data_2024$BsG0_S11)

data_2019$month <- format(as.POSIXct(data_2019$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"), "%m")
data_2020$month <- format(as.POSIXct(data_2020$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"), "%m")
data_2021$month <- format(as.POSIXct(data_2021$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"), "%m")
data_2022$month <- format(as.POSIXct(data_2022$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"), "%m")
data_2023$month <- format(as.POSIXct(data_2023$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"), "%m")
data_2024$month <- format(as.POSIXct(data_2024$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"), "%m")


library(ggplot2)

# Berechne den Median und die Standardabweichung für jeden Monat
medians <- aggregate(BsG0_S11 ~ month, data = data_2019, FUN = median ,na.rm = TRUE)
std_devs <- aggregate(BsG0_S11 ~ month, data = data_2019, FUN = sd ,na.rm = TRUE)

medians2 <- aggregate(BsG0_S11 ~ month, data = data_2020, FUN = median ,na.rm = TRUE)
std_devs2 <- aggregate(BsG0_S11 ~ month, data = data_2020, FUN = sd ,na.rm = TRUE)

medians3 <- aggregate(BsG0_S11 ~ month, data = data_2021, FUN = median ,na.rm = TRUE)
std_devs3 <- aggregate(BsG0_S11 ~ month, data = data_2021, FUN = sd ,na.rm = TRUE)

medians4 <- aggregate(BsG0_S11 ~ month, data = data_2022, FUN = median ,na.rm = TRUE)
std_devs4 <- aggregate(BsG0_S11 ~ month, data = data_2022, FUN = sd ,na.rm = TRUE)

medians5 <- aggregate(BsG0_S11 ~ month, data = data_2023, FUN = median ,na.rm = TRUE)
std_devs5 <- aggregate(BsG0_S11 ~ month, data = data_2023, FUN = sd ,na.rm = TRUE)

medians6 <- aggregate(BsG0_S11 ~ month, data = data_2024, FUN = median ,na.rm = TRUE)
std_devs6 <- aggregate(BsG0_S11 ~ month, data = data_2024, FUN = sd ,na.rm = TRUE)

data <- merge(medians, std_devs, by = "month")
data2 <- merge(medians2, std_devs2, by = "month")
data3 <- merge(medians3, std_devs3, by = "month")
data4 <- merge(medians4, std_devs4, by = "month")
data5 <- merge(medians5, std_devs5, by = "month")
data6 <- merge(medians6, std_devs6, by = "month")

months_labels <- c("January", "February", "March", "April", "May", "June", 
                   "July", "August", "September", "October", "November", "December")

ggplot() +   
  geom_point(data = data, aes(x = month, y = BsG0_S11.x, color = "data"), shape = 4, size = 3) +
  geom_errorbar(data = data, aes(x = month, ymin = BsG0_S11.x - BsG0_S11.y, ymax = BsG0_S11.x + BsG0_S11.y, color = "data"), 
                width = 0.2) +
  geom_line(data = data, aes(x = month, y = BsG0_S11.x, group = 1, color = "data"), linetype = "dashed") +
  geom_point(data = data2, aes(x = month, y = BsG0_S11.x, color = "data2"), shape = 4, size = 3) +
  geom_errorbar(data = data2, aes(x = month, ymin = BsG0_S11.x - BsG0_S11.y, ymax = BsG0_S11.x + BsG0_S11.y, color = "data2"), 
                width = 0.2) +
  geom_line(data = data2, aes(x = month, y = BsG0_S11.x, group = 1, color = "data2"), linetype = "dashed") +
  geom_point(data = data3, aes(x = month, y = BsG0_S11.x, color = "data3"), shape = 4, size = 3) +
  geom_errorbar(data = data3, aes(x = month, ymin = BsG0_S11.x - BsG0_S11.y, ymax = BsG0_S11.x + BsG0_S11.y, color = "data3"), 
                width = 0.2) +
  geom_line(data = data3, aes(x = month, y = BsG0_S11.x, group = 1, color = "data3"), linetype = "dashed") +
  geom_point(data = data4, aes(x = month, y = BsG0_S11.x, color = "data4"), shape = 4, size = 3) +
  geom_errorbar(data = data4, aes(x = month, ymin = BsG0_S11.x - BsG0_S11.y, ymax = BsG0_S11.x + BsG0_S11.y, color = "data4"), 
                width = 0.2) +
  geom_line(data = data4, aes(x = month, y = BsG0_S11.x, group = 1, color = "data4"), linetype = "dashed") +
  geom_point(data = data5, aes(x = month, y = BsG0_S11.x, color = "data5"), shape = 4, size = 3) +
  geom_errorbar(data = data5, aes(x = month, ymin = BsG0_S11.x - BsG0_S11.y, ymax = BsG0_S11.x + BsG0_S11.y, color = "data5"), 
                width = 0.2) +
  geom_line(data = data5, aes(x = month, y = BsG0_S11.x, group = 1, color = "data5"), linetype = "dashed") +
  geom_point(data = data6, aes(x = month, y = BsG0_S11.x, color = "data6"), shape = 4, size = 3) +
  geom_errorbar(data = data6, aes(x = month, ymin = BsG0_S11.x - BsG0_S11.y, ymax = BsG0_S11.x + BsG0_S11.y, color = "data6"), 
                width = 0.2) +
  geom_line(data = data6, aes(x = month, y = BsG0_S11.x, group = 1, color = "data6"), linetype = "dashed") +
  labs(title = "Monthly Total Scattering Coefficient [525nm]",
       x = "Month",
       y = expression(paste(sigma[TS], " [Mm"^-1*"]"))) +
  scale_x_discrete(labels = months_labels) +
  scale_color_manual(name = "", 
                     labels = c("2019", "2020", "2021", "2022", "2023", "2024"),
                     values = c("purple", "blue", "green", "yellow", "orange", "brown")) +
  theme_minimal()+
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5) # Hier wird der Haupttitel formatiert
  )

#################################################################################################################################
# Aurora3000 monthly Box year Back Scattering
#################################################################################################################################


time <- as.POSIXct(corrnephdata$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
year <- format(time, "%Y")
data_2019 <- corrnephdata[year == "2019", ]
data_2020 <- corrnephdata[year == "2020", ]
data_2021 <- corrnephdata[year == "2021", ]
data_2022 <- corrnephdata[year == "2022", ]
data_2023 <- corrnephdata[year == "2023", ]
data_2024 <- corrnephdata[year == "2024", ]

data_2019$BbsG0_S11 <- as.numeric(data_2019$BbsG0_S11)
data_2020$BbsG0_S11 <- as.numeric(data_2020$BbsG0_S11)
data_2021$BbsG0_S11 <- as.numeric(data_2021$BbsG0_S11)
data_2022$BbsG0_S11 <- as.numeric(data_2022$BbsG0_S11)
data_2023$BbsG0_S11 <- as.numeric(data_2023$BbsG0_S11)
data_2024$BbsG0_S11 <- as.numeric(data_2024$BbsG0_S11)

data_2019$month <- format(as.POSIXct(data_2019$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"), "%m")
data_2020$month <- format(as.POSIXct(data_2020$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"), "%m")
data_2021$month <- format(as.POSIXct(data_2021$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"), "%m")
data_2022$month <- format(as.POSIXct(data_2022$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"), "%m")
data_2023$month <- format(as.POSIXct(data_2023$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"), "%m")
data_2024$month <- format(as.POSIXct(data_2024$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"), "%m")


library(ggplot2)

# Berechne den Median und die Standardabweichung für jeden Monat
medians <- aggregate(BbsG0_S11 ~ month, data = data_2019, FUN = median ,na.rm = TRUE)
std_devs <- aggregate(BbsG0_S11 ~ month, data = data_2019, FUN = sd ,na.rm = TRUE)

medians2 <- aggregate(BbsG0_S11 ~ month, data = data_2020, FUN = median ,na.rm = TRUE)
std_devs2 <- aggregate(BbsG0_S11 ~ month, data = data_2020, FUN = sd ,na.rm = TRUE)

medians3 <- aggregate(BbsG0_S11 ~ month, data = data_2021, FUN = median ,na.rm = TRUE)
std_devs3 <- aggregate(BbsG0_S11 ~ month, data = data_2021, FUN = sd ,na.rm = TRUE)

medians4 <- aggregate(BbsG0_S11 ~ month, data = data_2022, FUN = median ,na.rm = TRUE)
std_devs4 <- aggregate(BbsG0_S11 ~ month, data = data_2022, FUN = sd ,na.rm = TRUE)

medians5 <- aggregate(BbsG0_S11 ~ month, data = data_2023, FUN = median ,na.rm = TRUE)
std_devs5 <- aggregate(BbsG0_S11 ~ month, data = data_2023, FUN = sd ,na.rm = TRUE)

medians6 <- aggregate(BbsG0_S11 ~ month, data = data_2024, FUN = median ,na.rm = TRUE)
std_devs6 <- aggregate(BbsG0_S11 ~ month, data = data_2024, FUN = sd ,na.rm = TRUE)

data <- merge(medians, std_devs, by = "month")
data2 <- merge(medians2, std_devs2, by = "month")
data3 <- merge(medians3, std_devs3, by = "month")
data4 <- merge(medians4, std_devs4, by = "month")
data5 <- merge(medians5, std_devs5, by = "month")
data6 <- merge(medians6, std_devs6, by = "month")

months_labels <- c("January", "February", "March", "April", "May", "June", 
                   "July", "August", "September", "October", "November", "December")

ggplot() +   
  geom_point(data = data, aes(x = month, y = BbsG0_S11.x, color = "data"), shape = 4, size = 3) +
  geom_errorbar(data = data, aes(x = month, ymin = BbsG0_S11.x - BbsG0_S11.y, ymax = BbsG0_S11.x + BbsG0_S11.y, color = "data"), 
                width = 0.2) +
  geom_line(data = data, aes(x = month, y = BbsG0_S11.x, group = 1, color = "data"), linetype = "dashed") +
  geom_point(data = data2, aes(x = month, y = BbsG0_S11.x, color = "data2"), shape = 4, size = 3) +
  geom_errorbar(data = data2, aes(x = month, ymin = BbsG0_S11.x - BbsG0_S11.y, ymax = BbsG0_S11.x + BbsG0_S11.y, color = "data2"), 
                width = 0.2) +
  geom_line(data = data2, aes(x = month, y = BbsG0_S11.x, group = 1, color = "data2"), linetype = "dashed") +
  geom_point(data = data3, aes(x = month, y = BbsG0_S11.x, color = "data3"), shape = 4, size = 3) +
  geom_errorbar(data = data3, aes(x = month, ymin = BbsG0_S11.x - BbsG0_S11.y, ymax = BbsG0_S11.x + BbsG0_S11.y, color = "data3"), 
                width = 0.2) +
  geom_line(data = data3, aes(x = month, y = BbsG0_S11.x, group = 1, color = "data3"), linetype = "dashed") +
  geom_point(data = data4, aes(x = month, y = BbsG0_S11.x, color = "data4"), shape = 4, size = 3) +
  geom_errorbar(data = data4, aes(x = month, ymin = BbsG0_S11.x - BbsG0_S11.y, ymax = BbsG0_S11.x + BbsG0_S11.y, color = "data4"), 
                width = 0.2) +
  geom_line(data = data4, aes(x = month, y = BbsG0_S11.x, group = 1, color = "data4"), linetype = "dashed") +
  geom_point(data = data5, aes(x = month, y = BbsG0_S11.x, color = "data5"), shape = 4, size = 3) +
  geom_errorbar(data = data5, aes(x = month, ymin = BbsG0_S11.x - BbsG0_S11.y, ymax = BbsG0_S11.x + BbsG0_S11.y, color = "data5"), 
                width = 0.2) +
  geom_line(data = data5, aes(x = month, y = BbsG0_S11.x, group = 1, color = "data5"), linetype = "dashed") +
  geom_point(data = data6, aes(x = month, y = BbsG0_S11.x, color = "data6"), shape = 4, size = 3) +
  geom_errorbar(data = data6, aes(x = month, ymin = BbsG0_S11.x - BbsG0_S11.y, ymax = BbsG0_S11.x + BbsG0_S11.y, color = "data6"), 
                width = 0.2) +
  geom_line(data = data6, aes(x = month, y = BbsG0_S11.x, group = 1, color = "data6"), linetype = "dashed") +
  labs(title = "Monthly Backcattering Coefficient [525nm]",
       x = "Month",
       y = expression(paste(sigma[BS], " [Mm"^-1*"]"))) +
  scale_x_discrete(labels = months_labels) +
  scale_color_manual(name = "", 
                     labels = c("2019", "2020", "2021", "2022", "2023", "2024"),
                     values = c("purple", "blue", "green", "yellow", "orange", "brown")) +
  theme_minimal()+
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5) # Hier wird der Haupttitel formatiert
  )




#################################################################################################################################
# Aurora3000 hourly Box year Scattering
#################################################################################################################################

ncorrected <- "/Users/baumannmike/Desktop/Datenbeispiele/Datenbeispiele/2019_03_01_5y_MKN_neph_1hr_corrected.csv"
corrnephdata <- read.csv(ncorrected)

time <- as.POSIXct(corrnephdata$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
corrnephdata$hour <- as.numeric(format(time, "%H"))
corrnephdata$BsG0_S11 <- as.numeric(corrnephdata$BsG0_S11)
corrnephdata$BbsG0_S11 <- as.numeric(corrnephdata$BbsG0_S11)

library(gglpot)


# Berechne den Median und die Standardabweichung für jeden Monat
medians <- aggregate(BsG0_S11 ~ hour, data = corrnephdata, FUN = median)
std_devs <- aggregate(BsG0_S11 ~ hour, data = corrnephdata, FUN = sd)

medians2 <- aggregate(BbsG0_S11 ~ hour, data = corrnephdata, FUN = median)
std_devs2 <- aggregate(BbsG0_S11 ~ hour, data = corrnephdata, FUN = sd)

data <- merge(medians, std_devs, by = "hour")

data2 <- merge(medians2, std_devs2, by = "hour")


# ggplot erstellen
ggplot() +
  geom_point(data = data, aes(x = hour, y = BsG0_S11.x, color = "Total Scattering"), shape = 4, size = 3, show.legend = TRUE) +
  geom_errorbar(data = data, aes(x = hour, ymin = BsG0_S11.x - BsG0_S11.y, ymax = BsG0_S11.x + BsG0_S11.y, color = "Total Scattering"), 
                width = 0.2, show.legend = FALSE) +
  geom_line(data = data, aes(x = hour, y = BsG0_S11.x, group = 1, color = "Total Scattering"), linetype = "dashed", show.legend = FALSE) +
  geom_point(data = data2, aes(x = hour, y = BbsG0_S11.x, color = "Backscattering"), shape = 4, size = 3, show.legend = TRUE) +
  geom_errorbar(data = data2, aes(x = hour, ymin = BbsG0_S11.x - BbsG0_S11.y, ymax = BbsG0_S11.x + BbsG0_S11.y, color = "Backscattering"), 
                width = 0.2, show.legend = FALSE) +
  geom_line(data = data2, aes(x = hour, y = BbsG0_S11.x, group = 1, color = "Backscattering"), linetype = "dashed", show.legend = FALSE) +
  labs(title = "Daily Absorption Coefficient [525nm]",
       x = "",
       y = expression(paste(sigma, " [Mm"^-1*"]"))) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5), # Hier wird der Haupttitel formatiert
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1) # Hier werden die x-Achsentexte um 90 Grad gedreht
  ) +
  scale_x_continuous(breaks = seq(0, 23, by = 1), labels = paste0(seq(0, 23), ":00")) +
  scale_color_manual(values = c("Total Scattering" = "red", "Backscattering" = "blue"), 
                     labels = c("Total Scattering", "Backscattering"), 
                     name = "Scattering Type",
                     guide = "legend")





#################################################################################################################################
# Aurora3000 year
#################################################################################################################################


corrnephdata$BsB0_S11 <- as.numeric(corrnephdata$BsB0_S11)
corrnephdata$BsG0_S11 <- as.numeric(corrnephdata$BsG0_S11)
corrnephdata$BsR0_S11 <- as.numeric(corrnephdata$BsR0_S11)

corrnephdata$BbsB0_S11 <- as.numeric(corrnephdata$BbsB0_S11)
corrnephdata$BbsG0_S11 <- as.numeric(corrnephdata$BbsG0_S11)
corrnephdata$BbsR0_S11 <- as.numeric(corrnephdata$BbsR0_S11)
time <- as.POSIXct(corrnephdata$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")




par(oma = c(0, 0, 0, 0))

# Teilt das Grafikfenster in 2 Reihen und 1 Spalte auf
par(mfrow = c(2, 1), mar = c(3, 5, 2, 2) + 0.1)

# Erster Plot mit den ersten drei Linien
plot(time, corrnephdata$BsB0_S11, col = "blue", type = "l", 
     xlab = "", ylab = expression(paste(sigma[TS], " [Mm"^-1*"]")), 
     main = "Scattering Coefficient")
lines(time, corrnephdata$BsG0_S11, col = "green")
lines(time, corrnephdata$BsR0_S11, col = "red")


# Zweiter Plot mit den letzten drei Linien
plot(time, corrnephdata$BbsB0_S11, col = "blue", type = "l", 
     xlab = "hour", ylab = expression(paste(sigma[BS], " [Mm"^-1*"]")))
lines(time, corrnephdata$BbsG0_S11, col = "green")
lines(time, corrnephdata$BbsR0_S11, col = "red")
legend("topright", legend = c("450 nm", "525 nm", "635 nm"), col = c("red", "green", "blue"), lty = 1)



#################################################################################################################################
# AAE hourly box
#################################################################################################################################
corrae31data$hour <- as.numeric(format(time, "%H"))
corrae31data$Ba30_A11 <- as.numeric(corrae31data$Ba30_A11)


corrae31data$Ba20_A11 <- as.numeric(corrae31data$Ba20_A11)
corrae31data$Ba60_A11 <- as.numeric(corrae31data$Ba60_A11)


# Zuerst füge eine Spalte für die Stunde hinzu
corrae31data$hour <- as.numeric(format(time, "%H"))

# Berechnung von AAE ohne NA-Werte

AAE1 <- -log(corrae31data$Ba20_A11/corrae31data$Ba60_A11)/log(470/880)
AAE1[is.infinite(AAE1)] <- NA

medians <- aggregate(AAE1, 
                         by = list(hour = corrae31data$hour), 
                         FUN = median , na.rm = TRUE)
std_devs <- aggregate(AAE1, 
                     by = list(hour = corrae31data$hour), 
                     FUN = sd, na.rm = TRUE)
library(gglpot)


data <- merge(medians, std_devs, by = "hour")





# ggplot erstellen

ggplot() +
  geom_point(data = data, aes(x = hour, y = x.x), shape = 4, size = 3, color = "red", show.legend = FALSE) +
  geom_errorbar(data = data, aes(x = hour, ymin = x.x - x.y, ymax = x.x + x.y), 
                width = 0.2, color = "red", show.legend = FALSE) +
  geom_line(data = data, aes(x = hour, y = x.x, group = 1), linetype = "dashed", color = "red", show.legend = FALSE) +
  labs(title = "Daily Median AAE [470-880nm]",
       x = "hour",
       y = "AAE") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5), # Hier wird der Haupttitel formatiert
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1) # Hier werden die x-Achsentexte um 90 Grad gedreht
  ) +
  scale_x_continuous(breaks = seq(0, 23, by = 1), labels = paste0(seq(0, 23), ":00"))


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
title("Hourly Median AAE [470-880 nm]")

# X-Achsenbeschriftung einstellen
axis(1, at = NA)

axis(side = 1, at = hourly_avg1$hour, labels = paste0(hourly_avg1$hour, ":00"), las = 2, cex.axis = 0.9)

#################################################################################################################################
# AAE monthly box
#################################################################################################################################


time <- as.POSIXct(corrae31data$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
year <- format(time, "%Y")
data_2019 <- corrae31data[year == "2019", ]
data_2020 <- corrae31data[year == "2020", ]
data_2021 <- corrae31data[year == "2021", ]
data_2022 <- corrae31data[year == "2022", ]
data_2023 <- corrae31data[year == "2023", ]
data_2024 <- corrae31data[year == "2024", ]

data_2019$Ba20_A11 <- as.numeric(data_2019$Ba20_A11)
data_2020$Ba20_A11 <- as.numeric(data_2020$Ba20_A11)
data_2021$Ba20_A11 <- as.numeric(data_2021$Ba20_A11)
data_2022$Ba20_A11 <- as.numeric(data_2022$Ba20_A11)
data_2023$Ba20_A11 <- as.numeric(data_2023$Ba20_A11)
data_2024$Ba20_A11 <- as.numeric(data_2024$Ba20_A11)

data_2019$Ba60_A11 <- as.numeric(data_2019$Ba60_A11)
data_2020$Ba60_A11 <- as.numeric(data_2020$Ba60_A11)
data_2021$Ba60_A11 <- as.numeric(data_2021$Ba60_A11)
data_2022$Ba60_A11 <- as.numeric(data_2022$Ba60_A11)
data_2023$Ba60_A11 <- as.numeric(data_2023$Ba60_A11)
data_2024$Ba60_A11 <- as.numeric(data_2024$Ba60_A11)

data_2019$month <- format(as.POSIXct(data_2019$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"), "%m")
data_2020$month <- format(as.POSIXct(data_2020$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"), "%m")
data_2021$month <- format(as.POSIXct(data_2021$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"), "%m")
data_2022$month <- format(as.POSIXct(data_2022$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"), "%m")
data_2023$month <- format(as.POSIXct(data_2023$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"), "%m")
data_2024$month <- format(as.POSIXct(data_2024$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"), "%m")



# Berechnung von AAE ohne NA-Werte

AAE1 <- -log(data_2019$Ba20_A11/data_2019$Ba60_A11)/log(470/880)
AAE1[is.infinite(AAE1)] <- NA

AAE2 <- -log(data_2020$Ba20_A11/data_2020$Ba60_A11)/log(470/880)
AAE2[is.infinite(AAE2)] <- NA

AAE3 <- -log(data_2021$Ba20_A11/data_2021$Ba60_A11)/log(470/880)
AAE3[is.infinite(AAE3)] <- NA

AAE4 <- -log(data_2022$Ba20_A11/data_2022$Ba60_A11)/log(470/880)
AAE4[is.infinite(AAE4)] <- NA

AAE5 <- -log(data_2023$Ba20_A11/data_2023$Ba60_A11)/log(470/880)
AAE5[is.infinite(AAE5)] <- NA

AAE6 <- -log(data_2024$Ba20_A11/data_2024$Ba60_A11)/log(470/880)
AAE6[is.infinite(AAE6)] <- NA

# Dann gruppieren nach Stunde und den Durchschnitt berechnen

medians <- aggregate(AAE1, by = list(month = data_2019$month), FUN = median , na.rm = TRUE)
std_devs <- aggregate(AAE1, by = list(month = data_2019$month), FUN = sd , na.rm = TRUE)

medians2 <- aggregate(AAE2, by = list(month = data_2020$month), FUN = median , na.rm = TRUE)
std_devs2 <- aggregate(AAE2, by = list(month = data_2020$month), FUN = sd , na.rm = TRUE)

medians3 <- aggregate(AAE3, by = list(month = data_2021$month), FUN = median , na.rm = TRUE)
std_devs3 <- aggregate(AAE3, by = list(month = data_2021$month), FUN = sd , na.rm = TRUE)

medians4 <- aggregate(AAE4, by = list(month = data_2022$month), FUN = median , na.rm = TRUE)
std_devs4 <- aggregate(AAE4, by = list(month = data_2022$month), FUN = sd , na.rm = TRUE)

medians5 <- aggregate(AAE5, by = list(month = data_2023$month), FUN = median , na.rm = TRUE)
std_devs5 <- aggregate(AAE5, by = list(month = data_2023$month), FUN = sd , na.rm = TRUE)

medians6 <- aggregate(AAE6, by = list(month = data_2024$month), FUN = median , na.rm = TRUE)
std_devs6 <- aggregate(AAE6, by = list(month = data_2024$month), FUN = sd , na.rm = TRUE)

library(gglpot)


data <- merge(medians, std_devs, by = "month")
data2 <- merge(medians2, std_devs2, by = "month")
data3 <- merge(medians3, std_devs3, by = "month")
data4 <- merge(medians4, std_devs4, by = "month")
data5 <- merge(medians5, std_devs5, by = "month")
data6 <- merge(medians6, std_devs6, by = "month")

months_labels <- c("January", "February", "March", "April", "May", "June", 
                   "July", "August", "September", "October", "November", "December")


# ggplot erstellen
ggplot() +
  geom_point(data = data, aes(x = month, y = x.x, color = "data"), shape = 4, size = 3) +
  geom_errorbar(data = data, aes(x = month, ymin = x.x - x.y, ymax = x.x + x.y, color = "data"), 
                width = 0.2) +
  geom_line(data = data, aes(x = month, y = x.x, group = 1, color = "data"), linetype = "dashed") +
  geom_point(data = data2, aes(x = month, y = x.x, color = "data2"), shape = 4, size = 3) +
  geom_errorbar(data = data2, aes(x = month, ymin = x.x - x.y, ymax = x.x + x.y, color = "data2"), 
                width = 0.2) +
  geom_line(data = data2, aes(x = month, y = x.x, group = 1, color = "data2"), linetype = "dashed") +
  geom_point(data = data3, aes(x = month, y = x.x, color = "data3"), shape = 4, size = 3) +
  geom_errorbar(data = data3, aes(x = month, ymin = x.x - x.y, ymax = x.x + x.y, color = "data3"), 
                width = 0.2) +
  geom_line(data = data3, aes(x = month, y = x.x, group = 1, color = "data3"), linetype = "dashed") +
  geom_point(data = data4, aes(x = month, y = x.x, color = "data4"), shape = 4, size = 3) +
  geom_errorbar(data = data4, aes(x = month, ymin = x.x - x.y, ymax = x.x + x.y, color = "data4"), 
                width = 0.2) +
  geom_line(data = data4, aes(x = month, y = x.x, group = 1, color = "data4"), linetype = "dashed") +
  geom_point(data = data5, aes(x = month, y = x.x, color = "data5"), shape = 4, size = 3) +
  geom_errorbar(data = data5, aes(x = month, ymin = x.x - x.y, ymax = x.x + x.y, color = "data5"), 
                width = 0.2) +
  geom_line(data = data5, aes(x = month, y = x.x, group = 1, color = "data5"), linetype = "dashed") +
  geom_point(data = data6, aes(x = month, y = x.x, color = "data6"), shape = 4, size = 3) +
  geom_errorbar(data = data6, aes(x = month, ymin = x.x - x.y, ymax = x.x + x.y, color = "data6"), 
                width = 0.2) +
  geom_line(data = data6, aes(x = month, y = x.x, group = 1, color = "data6"), linetype = "dashed") +
  labs(title = "Monthly AAE",
       x = "",
       y = "AAE") +
  scale_x_discrete(labels = months_labels) +
  scale_color_manual(name = "", 
                     labels = c("2019", "2020", "2021", "2022", "2023", "2024"),
                     values = c("purple", "blue", "green", "yellow", "orange", "brown")) +
  theme_minimal()+
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5) # Hier wird der Haupttitel formatiert
  )


#################################################################################################################################
# AAE monthly
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
corrae31data$hour <- as.numeric(format(time, "%m"))

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
title("Monthly Median AAE [470-880 nm]")

month_to_name <- function(month_num) {
  month_names <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
  return(month_names[month_num])
}

axis(side = 1, at = unique(hourly_avg1$hour), labels = month_to_name(unique(hourly_avg1$hour)), las = 2, cex.axis = 0.9)

#################################################################################################################################
# AAE yearly
#################################################################################################################################


# Konvertieren der Spalten in numerische Werte

time <- as.POSIXct(corrae31data$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
corrae31data$Ba10_A11 <- as.numeric(corrae31data$Ba10_A11)
corrae31data$Ba20_A11 <- as.numeric(corrae31data$Ba20_A11)
corrae31data$Ba30_A11 <- as.numeric(corrae31data$Ba30_A11)
corrae31data$Ba40_A11 <- as.numeric(corrae31data$Ba40_A11)
corrae31data$Ba50_A11 <- as.numeric(corrae31data$Ba50_A11)
corrae31data$Ba60_A11 <- as.numeric(corrae31data$Ba60_A11)
corrae31data$Ba70_A11 <- as.numeric(corrae31data$Ba70_A11)


# Berechnung von AAE ohne NA-Werte

AAE1 <- -log(corrae31data$Ba20_A11/corrae31data$Ba60_A11)/log(470/880)



# Plot der durchschnittlichen AAE-Werte nach Stunden
# Plot erstellen
plot(time, AAE1, type = "l", col = "black", 
     xlab = "time", ylab = "AAE")


# Legende hinzufügen
title("AAE [470-880 nm]")

# X-Achsenbeschriftung einstellen



#################################################################################################################################
# SAE
#################################################################################################################################

corrnephdata$BsB0_S11 <- as.numeric(corrnephdata$BsB0_S11)
corrnephdata$BsG0_S11 <- as.numeric(corrnephdata$BsG0_S11)
corrnephdata$BsR0_S11 <- as.numeric(corrnephdata$BsR0_S11)
time <- as.POSIXct(corrnephdata$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")


# Berechnung von AAE ohne NA-Werte
AAE1 <- -log(corrnephdata$BsB0_S11/corrnephdata$BsR0_S11)/log(450/635)

# Zuerst füge eine Spalte für die Stunde hinzu



# Plot erstellen
plot(time, AAE1, type = "l", col = "black", 
     xlab = "time", ylab = "SAE")



# Legende hinzufügen
title("SAE [450-635 nm]")

#################################################################################################################################
# SAE hourly box
#################################################################################################################################
time <- as.POSIXct(corrnephdata$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

corrnephdata$BsB0_S11 <- as.numeric(corrnephdata$BsB0_S11)
corrnephdata$BsG0_S11 <- as.numeric(corrnephdata$BsG0_S11)
corrnephdata$BsR0_S11 <- as.numeric(corrnephdata$BsR0_S11)

# Berechnung von AAE ohne NA-Werte
AAE1 <- -log(corrnephdata$BsB0_S11/corrnephdata$BsR0_S11)/log(450/635)
AAE1[is.infinite(AAE1)] <- NA

# Zuerst füge eine Spalte für die Stunde hinzu
corrnephdata$hour <- as.numeric(format(time, "%H"))

# Berechnung von AAE ohne NA-Werte




medians <- aggregate(AAE1, 
                     by = list(hour = corrnephdata$hour), 
                     FUN = median , na.rm = TRUE)
std_devs <- aggregate(AAE1, 
                      by = list(hour = corrnephdata$hour), 
                      FUN = sd, na.rm = TRUE)
library(gglpot)


data <- merge(medians, std_devs, by = "hour")





# ggplot erstellen

ggplot() +
  geom_point(data = data, aes(x = hour, y = x.x), shape = 4, size = 3, color = "red", show.legend = FALSE) +
  geom_errorbar(data = data, aes(x = hour, ymin = x.x - x.y, ymax = x.x + x.y), 
                width = 0.2, color = "red", show.legend = FALSE) +
  geom_line(data = data, aes(x = hour, y = x.x, group = 1), linetype = "dashed", color = "red", show.legend = FALSE) +
  labs(title = "Daily Median SAE [470-880nm]",
       x = "hour",
       y = "SAE") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5), # Hier wird der Haupttitel formatiert
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1) # Hier werden die x-Achsentexte um 90 Grad gedreht
  ) +
  scale_x_continuous(breaks = seq(0, 23, by = 1), labels = paste0(seq(0, 23), ":00"))


#################################################################################################################################
# SAE daily
#################################################################################################################################

time <- as.POSIXct(corrnephdata$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

corrnephdata$BsB0_S11 <- as.numeric(corrnephdata$BsB0_S11)
corrnephdata$BsG0_S11 <- as.numeric(corrnephdata$BsG0_S11)
corrnephdata$BsR0_S11 <- as.numeric(corrnephdata$BsR0_S11)

# Berechnung von AAE ohne NA-Werte
AAE1 <- -log(corrnephdata$BsB0_S11/corrnephdata$BsR0_S11)/log(450/635)


# Zuerst füge eine Spalte für die Stunde hinzu
corrnephdata$hour <- as.numeric(format(time, "%H"))

# Dann gruppieren nach Stunde und den Durchschnitt berechnen
hourly_avg1 <- aggregate(AAE1, by = list(hour = corrnephdata$hour), FUN = function(x) median(x, na.rm = TRUE))



# Plot der durchschnittlichen AAE-Werte nach Stunden
# Plot der durchschnittlichen AAE-Werte nach Stunden
# Plot erstellen
plot(hourly_avg1$hour, hourly_avg1$x, type = "l", col = "black", 
     xlab = "hour", ylab = "SAE", 
     ylim = c(0, max(hourly_avg1$x, na.rm = TRUE) * 1.5), xaxt = "n")



# Legende hinzufügen

title("Hourly Median SAE [450-635 nm]")

# X-Achsenbeschriftung einstellen
axis(side = 1, at = hourly_avg1$hour, labels = paste0(hourly_avg1$hour, ":00"), las = 2, cex.axis = 0.9)

#################################################################################################################################
# SAE monthly
#################################################################################################################################

time <- as.POSIXct(corrnephdata$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

corrnephdata$BsB0_S11 <- as.numeric(corrnephdata$BsB0_S11)
corrnephdata$BsG0_S11 <- as.numeric(corrnephdata$BsG0_S11)
corrnephdata$BsR0_S11 <- as.numeric(corrnephdata$BsR0_S11)

# Berechnung von AAE ohne NA-Werte
AAE1 <- -log(corrnephdata$BsB0_S11/corrnephdata$BsR0_S11)/log(450/635)


# Zuerst füge eine Spalte für die Stunde hinzu
corrnephdata$hour <- as.numeric(format(time, "%m"))

# Dann gruppieren nach Stunde und den Durchschnitt berechnen
hourly_avg1 <- aggregate(AAE1, by = list(hour = corrnephdata$hour), FUN = function(x) median(x, na.rm = TRUE))



# Plot der durchschnittlichen AAE-Werte nach Stunden
# Plot der durchschnittlichen AAE-Werte nach Stunden
# Plot erstellen
plot(hourly_avg1$hour, hourly_avg1$x, type = "l", col = "black", 
     xlab = "", ylab = "SAE", 
     ylim = c(0, max(hourly_avg1$x, na.rm = TRUE) * 1.5), xaxt = "n")



# Legende hinzufügen

title("Monthly Median SAE [450-635 nm]")

# X-Achsenbeschriftung einstellen
month_to_name <- function(month_num) {
  month_names <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
  return(month_names[month_num])
}

axis(side = 1, at = unique(hourly_avg1$hour), labels = month_to_name(unique(hourly_avg1$hour)), las = 2, cex.axis = 0.9)

#################################################################################################################################
# SAE monthly box
#################################################################################################################################

time <- as.POSIXct(corrnephdata$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
year <- format(time, "%Y")
data_2019 <- corrnephdata[year == "2019", ]
data_2020 <- corrnephdata[year == "2020", ]
data_2021 <- corrnephdata[year == "2021", ]
data_2022 <- corrnephdata[year == "2022", ]
data_2023 <- corrnephdata[year == "2023", ]
data_2024 <- corrnephdata[year == "2024", ]

data_2019$BsB0_S11 <- as.numeric(data_2019$BsB0_S11)
data_2020$BsB0_S11 <- as.numeric(data_2020$BsB0_S11)
data_2021$BsB0_S11 <- as.numeric(data_2021$BsB0_S11)
data_2022$BsB0_S11 <- as.numeric(data_2022$BsB0_S11)
data_2023$BsB0_S11 <- as.numeric(data_2023$BsB0_S11)
data_2024$BsB0_S11 <- as.numeric(data_2024$BsB0_S11)

data_2019$BsR0_S11 <- as.numeric(data_2019$BsR0_S11)
data_2020$BsR0_S11 <- as.numeric(data_2020$BsR0_S11)
data_2021$BsR0_S11 <- as.numeric(data_2021$BsR0_S11)
data_2022$BsR0_S11 <- as.numeric(data_2022$BsR0_S11)
data_2023$BsR0_S11 <- as.numeric(data_2023$BsR0_S11)
data_2024$BsR0_S11 <- as.numeric(data_2024$BsR0_S11)


data_2019$month <- format(as.POSIXct(data_2019$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"), "%m")
data_2020$month <- format(as.POSIXct(data_2020$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"), "%m")
data_2021$month <- format(as.POSIXct(data_2021$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"), "%m")
data_2022$month <- format(as.POSIXct(data_2022$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"), "%m")
data_2023$month <- format(as.POSIXct(data_2023$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"), "%m")
data_2024$month <- format(as.POSIXct(data_2024$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"), "%m")

# Berechnung von AAE ohne NA-Werte

AAE1 <- -log(data_2019$BsB0_S111/data_2019$BsR0_S11)/log(470/880)
AAE1[is.infinite(AAE1)] <- NA

AAE2 <- -log(data_2020$BsB0_S11/data_2020$BsR0_S11)/log(470/880)
AAE2[is.infinite(AAE2)] <- NA

AAE3 <- -log(data_2021$BsB0_S11/data_2021$BsR0_S11)/log(470/880)
AAE3[is.infinite(AAE3)] <- NA

AAE4 <- -log(data_2022$BsB0_S11/data_2022$BsR0_S11)/log(470/880)
AAE4[is.infinite(AAE4)] <- NA

AAE5 <- -log(data_2023$BsB0_S11/data_2023$BsR0_S11)/log(470/880)
AAE5[is.infinite(AAE5)] <- NA

AAE6 <- -log(data_2024$BsB0_S11/data_2024$BsR0_S11)/log(470/880)
AAE6[is.infinite(AAE6)] <- NA

# Dann gruppieren nach Stunde und den Durchschnitt berechnen

medians <- aggregate(AAE1, by = list(month = data_2019$month), FUN = median , na.rm = TRUE)
std_devs <- aggregate(AAE1, by = list(month = data_2019$month), FUN = sd , na.rm = TRUE)

medians2 <- aggregate(AAE2, by = list(month = data_2020$month), FUN = median , na.rm = TRUE)
std_devs2 <- aggregate(AAE2, by = list(month = data_2020$month), FUN = sd , na.rm = TRUE)

medians3 <- aggregate(AAE3, by = list(month = data_2021$month), FUN = median , na.rm = TRUE)
std_devs3 <- aggregate(AAE3, by = list(month = data_2021$month), FUN = sd , na.rm = TRUE)

medians4 <- aggregate(AAE4, by = list(month = data_2022$month), FUN = median , na.rm = TRUE)
std_devs4 <- aggregate(AAE4, by = list(month = data_2022$month), FUN = sd , na.rm = TRUE)

medians5 <- aggregate(AAE5, by = list(month = data_2023$month), FUN = median , na.rm = TRUE)
std_devs5 <- aggregate(AAE5, by = list(month = data_2023$month), FUN = sd , na.rm = TRUE)

medians6 <- aggregate(AAE6, by = list(month = data_2024$month), FUN = median , na.rm = TRUE)
std_devs6 <- aggregate(AAE6, by = list(month = data_2024$month), FUN = sd , na.rm = TRUE)

library(gglpot)


data <- merge(medians, std_devs, by = "month")
data2 <- merge(medians2, std_devs2, by = "month")
data3 <- merge(medians3, std_devs3, by = "month")
data4 <- merge(medians4, std_devs4, by = "month")
data5 <- merge(medians5, std_devs5, by = "month")
data6 <- merge(medians6, std_devs6, by = "month")

months_labels <- c("January", "February", "March", "April", "May", "June", 
                   "July", "August", "September", "October", "November", "December")


# ggplot erstellen
ggplot() +
  geom_point(data = data, aes(x = month, y = x.x, color = "data"), shape = 4, size = 3) +
  geom_errorbar(data = data, aes(x = month, ymin = x.x - x.y, ymax = x.x + x.y, color = "data"), 
                width = 0.2) +
  geom_line(data = data, aes(x = month, y = x.x, group = 1, color = "data"), linetype = "dashed") +
  geom_point(data = data2, aes(x = month, y = x.x, color = "data2"), shape = 4, size = 3) +
  geom_errorbar(data = data2, aes(x = month, ymin = x.x - x.y, ymax = x.x + x.y, color = "data2"), 
                width = 0.2) +
  geom_line(data = data2, aes(x = month, y = x.x, group = 1, color = "data2"), linetype = "dashed") +
  geom_point(data = data3, aes(x = month, y = x.x, color = "data3"), shape = 4, size = 3) +
  geom_errorbar(data = data3, aes(x = month, ymin = x.x - x.y, ymax = x.x + x.y, color = "data3"), 
                width = 0.2) +
  geom_line(data = data3, aes(x = month, y = x.x, group = 1, color = "data3"), linetype = "dashed") +
  geom_point(data = data4, aes(x = month, y = x.x, color = "data4"), shape = 4, size = 3) +
  geom_errorbar(data = data4, aes(x = month, ymin = x.x - x.y, ymax = x.x + x.y, color = "data4"), 
                width = 0.2) +
  geom_line(data = data4, aes(x = month, y = x.x, group = 1, color = "data4"), linetype = "dashed") +
  geom_point(data = data5, aes(x = month, y = x.x, color = "data5"), shape = 4, size = 3) +
  geom_errorbar(data = data5, aes(x = month, ymin = x.x - x.y, ymax = x.x + x.y, color = "data5"), 
                width = 0.2) +
  geom_line(data = data5, aes(x = month, y = x.x, group = 1, color = "data5"), linetype = "dashed") +
  geom_point(data = data6, aes(x = month, y = x.x, color = "data6"), shape = 4, size = 3) +
  geom_errorbar(data = data6, aes(x = month, ymin = x.x - x.y, ymax = x.x + x.y, color = "data6"), 
                width = 0.2) +
  geom_line(data = data6, aes(x = month, y = x.x, group = 1, color = "data6"), linetype = "dashed") +
  labs(title = "Monthly SAE",
       x = "",
       y = "SAE") +
  scale_x_discrete(labels = months_labels) +
  scale_color_manual(name = "", 
                     labels = c("2019", "2020", "2021", "2022", "2023", "2024"),
                     values = c("purple", "blue", "green", "yellow", "orange", "brown")) +
  theme_minimal()+
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5) # Hier wird der Haupttitel formatiert
  )


#################################################################################################################################
# backsacatter fraction daily box
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

medians <- aggregate(blue, by = list(hour = corrnephdata$hour), FUN = function(x) median(x, na.rm = TRUE))
medians2 <- aggregate(green, by = list(hour = corrnephdata$hour), FUN = function(x) median(x, na.rm = TRUE))
medians3 <- aggregate(red, by = list(hour = corrnephdata$hour), FUN = function(x) median(x, na.rm = TRUE))

std_devs <- aggregate(blue, by = list(hour = corrnephdata$hour), FUN = function(x) sd(x, na.rm = TRUE))
std_devs2 <- aggregate(green, by = list(hour = corrnephdata$hour), FUN = function(x) sd(x, na.rm = TRUE))
std_devs3 <- aggregate(red, by = list(hour = corrnephdata$hour), FUN = function(x) sd(x, na.rm = TRUE))

data <- merge(medians, std_devs, by = "hour")
data2 <- merge(medians2, std_devs2, by = "hour")
data3 <- merge(medians3, std_devs3, by = "hour")


ggplot() +
  geom_point(data = data, aes(x = hour, y = x.x, color = "635nm"), shape = 4, size = 3) +
  geom_errorbar(data = data, aes(x = hour, ymin = x.x - x.y, ymax = x.x + x.y, color = "635nm"), 
                width = 0.2) +
  geom_line(data = data2, aes(x = hour, y = x.x, group = 1, color = "525nm"), linetype = "dashed") +
  geom_point(data = data2, aes(x = hour, y = x.x, color = "525nm"), shape = 4, size = 3) +
  geom_errorbar(data = data2, aes(x = hour, ymin = x.x - x.y, ymax = x.x + x.y, color = "525nm"), 
                width = 0.2) +
  geom_line(data = data3, aes(x = hour, y = x.x, group = 1, color = "450nm"), linetype = "dashed") +
  geom_point(data = data3, aes(x = hour, y = x.x, color = "450nm"), shape = 4, size = 3) +
  geom_errorbar(data = data3, aes(x = hour, ymin = x.x - x.y, ymax = x.x + x.y, color = "450nm"), 
                width = 0.2) +
  labs(title = "Daily Median backscatter fraction",
       x = "hour",
       y = "b") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5), 
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  ) +
  scale_x_continuous(breaks = seq(0, 23, by = 1), labels = paste0(seq(0, 23), ":00")) +
  scale_color_manual(values = c("635nm" = "blue", "525nm" = "green", "450nm" = "red"), 
                     labels = c("635nm", "525nm", "450nm"), 
                     name = "Wavelength",
                     guide = "legend")





#################################################################################################################################
# backsacatter fraction daily
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

plot(hourly_avg1$hour, hourly_avg1$x, type = "l", col = "blue", 
     xlab = "hour", ylab = "b", 
     ylim = c(0, max(hourly_avg1$x, na.rm = TRUE) * 1.5),
     xaxt = "n")
# Weitere Linien hinzufügen
lines(hourly_avg2$hour, hourly_avg2$x, type = "l", col = "green")
lines(hourly_avg3$hour, hourly_avg3$x, type = "l", col = "red")


# Legende hinzufügen
legend("topright", legend = c("450 nm", "525 nm", "635 nm"), 
       col = c("red", "green", "blue"), lty = 1)
title("Hourly Median Backscatter Fraction ")

# X-Achsenbeschriftung einstellen
axis(1, at = NA)

axis(side = 1, at = hourly_avg1$hour, labels = paste0(hourly_avg1$hour, ":00"), las = 2, cex.axis = 0.9)

#################################################################################################################################
# backsacatter fraction monthly
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
corrnephdata$hour <- as.numeric(format(time, "%m"))


blue <- corrnephdata$BbsB0_S11/corrnephdata$BsB0_S11
green <- corrnephdata$BbsG0_S11/corrnephdata$BsG0_S11
red <- corrnephdata$BbsR0_S11/corrnephdata$BsR0_S11

hourly_avg1 <- aggregate(blue, by = list(hour = corrnephdata$hour), FUN = function(x) median(x, na.rm = TRUE))
hourly_avg2 <- aggregate(green, by = list(hour = corrnephdata$hour), FUN = function(x) median(x, na.rm = TRUE))
hourly_avg3 <- aggregate(red, by = list(hour = corrnephdata$hour), FUN = function(x) median(x, na.rm = TRUE))

plot(hourly_avg1$hour, hourly_avg1$x, type = "l", col = "blue", 
     xlab = "", ylab = "b", 
     ylim = c(0, max(hourly_avg1$x, na.rm = TRUE) * 1.5),
     xaxt = "n")
# Weitere Linien hinzufügen
lines(hourly_avg2$hour, hourly_avg2$x, type = "l", col = "green")
lines(hourly_avg3$hour, hourly_avg3$x, type = "l", col = "red")


# Legende hinzufügen
legend("topright", legend = c("450 nm", "525 nm", "635 nm"), 
       col = c("red", "green", "blue"), lty = 1)
title("Monthly Median Backscatter Fraction ")

# X-Achsenbeschriftung einstellen
month_to_name <- function(month_num) {
  month_names <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
  return(month_names[month_num])
}

axis(side = 1, at = unique(hourly_avg1$hour), labels = month_to_name(unique(hourly_avg1$hour)), las = 2, cex.axis = 0.9)


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


blue <- corrnephdata$BbsB0_S11/corrnephdata$BsB0_S11
green <- corrnephdata$BbsG0_S11/corrnephdata$BsG0_S11
red <- corrnephdata$BbsR0_S11/corrnephdata$BsR0_S11

plot(time, blue, type = "l", col = "blue", 
     xlab = "time", ylab = "b", ylim = c(-5, 5))
# Weitere Linien hinzufügen
lines(time, green, type = "l", col = "green")
lines(time, red, type = "l", col = "red")


# Legende hinzufügen
legend("topright", legend = c("450 nm", "525 nm", "635 nm"), 
       col = c("red", "green", "blue"), lty = 1)
title("Backscatter Fraction ")


#################################################################################################################################
# SSA dry hourly box
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
medians <- aggregate(SSAdryb, by = list(hour = merged_data$hour), FUN = function(x) median(x, na.rm = TRUE))
medians2 <- aggregate(SSAdryg, by = list(hour = merged_data$hour), FUN = function(x) median(x, na.rm = TRUE))
medians3 <- aggregate(SSAdryr, by = list(hour = merged_data$hour), FUN = function(x) median(x, na.rm = TRUE))

std_devs <- aggregate(SSAdryb, by = list(hour = merged_data$hour), FUN = function(x) sd(x, na.rm = TRUE))
std_devs2 <- aggregate(SSAdryg, by = list(hour = merged_data$hour), FUN = function(x) sd(x, na.rm = TRUE))
std_devs3 <- aggregate(SSAdryr, by = list(hour = merged_data$hour), FUN = function(x) sd(x, na.rm = TRUE))


data <- merge(medians, std_devs, by = "hour")
data2 <- merge(medians2, std_devs2, by = "hour")
data3 <- merge(medians3, std_devs3, by = "hour")


ggplot() +
  geom_point(data = data, aes(x = hour, y = x.x, color = "635nm"), shape = 4, size = 3) +
  geom_errorbar(data = data, aes(x = hour, ymin = x.x - x.y, ymax = x.x + x.y, color = "635nm"), 
                width = 0.2) +
  geom_line(data = data2, aes(x = hour, y = x.x, group = 1, color = "525nm"), linetype = "dashed") +
  geom_point(data = data2, aes(x = hour, y = x.x, color = "525nm"), shape = 4, size = 3) +
  geom_errorbar(data = data2, aes(x = hour, ymin = x.x - x.y, ymax = x.x + x.y, color = "525nm"), 
                width = 0.2) +
  geom_line(data = data3, aes(x = hour, y = x.x, group = 1, color = "450nm"), linetype = "dashed") +
  geom_point(data = data3, aes(x = hour, y = x.x, color = "450nm"), shape = 4, size = 3) +
  geom_errorbar(data = data3, aes(x = hour, ymin = x.x - x.y, ymax = x.x + x.y, color = "450nm"), 
                width = 0.2) +
  labs(title = "Daily Median SSA dry",
       x = "hour",
       y = "SSA dry") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5), 
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  ) +
  scale_x_continuous(breaks = seq(0, 23, by = 1), labels = paste0(seq(0, 23), ":00")) +
  scale_color_manual(values = c("635nm" = "blue", "525nm" = "green", "450nm" = "red"), 
                     labels = c("635nm", "525nm", "450nm"), 
                     name = "Wavelength",
                     guide = "legend")




#################################################################################################################################
# SSA dry hourly
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
hourly_avgb <- aggregate(SSAdryb, by = list(hour = merged_data$hour), FUN = function(x) median(x, na.rm = TRUE))
hourly_avgg <- aggregate(SSAdryg, by = list(hour = merged_data$hour), FUN = function(x) median(x, na.rm = TRUE))
hourly_avgr <- aggregate(SSAdryr, by = list(hour = merged_data$hour), FUN = function(x) median(x, na.rm = TRUE))

# Jetzt hast du einen Dataframe mit zwei Spalten: 'hour' und 'x' (der Durchschnittswert für jede Stunde)

# Plot erstellen mit angepasster y-Achse
plot(hourly_avgb$hour, hourly_avgb$x, col = "blue", type = "l", xlab = "hour", ylab = "SSA dry", main = "Hourly Median SSA dry", xaxt = "n", ylim = c(min(hourly_avgb$x, hourly_avgg$x, hourly_avgr$x), max(hourly_avgb$x, hourly_avgg$x, hourly_avgr$x)))
lines(hourly_avgg$hour, hourly_avgg$x, col = "green")
lines(hourly_avgr$hour, hourly_avgr$x, col = "red")

# x-Achse beschriften
axis(side = 1, at = hourly_avgb$hour, labels = paste0(hourly_avgb$hour, ":00"), las = 2, cex.axis = 0.9)
legend("bottomright", legend = c("450 nm", "525 nm", "635 nm"), 
       col = c("red", "green", "blue"), lty = 1)

#################################################################################################################################
# SSA dry monthly
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
corrae31data$hour <- as.numeric(format(time, "%m"))

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
merged_data$hour <- as.numeric(format(time, "%m"))

# Dann gruppieren nach Stunde und den Durchschnitt berechnen
hourly_avgb <- aggregate(SSAdryb, by = list(hour = merged_data$hour), FUN = function(x) median(x, na.rm = TRUE))
hourly_avgg <- aggregate(SSAdryg, by = list(hour = merged_data$hour), FUN = function(x) median(x, na.rm = TRUE))
hourly_avgr <- aggregate(SSAdryr, by = list(hour = merged_data$hour), FUN = function(x) median(x, na.rm = TRUE))

# Jetzt hast du einen Dataframe mit zwei Spalten: 'hour' und 'x' (der Durchschnittswert für jede Stunde)

# Plot erstellen mit angepasster y-Achse
plot(hourly_avgb$hour, hourly_avgb$x, col = "blue", type = "l", xlab = "", ylab = "SSA dry", main = "Monthly Median SSA dry", xaxt = "n", ylim = c(min(hourly_avgb$x, hourly_avgg$x, hourly_avgr$x), max(hourly_avgb$x, hourly_avgg$x, hourly_avgr$x)))
lines(hourly_avgg$hour, hourly_avgg$x, col = "green")
lines(hourly_avgr$hour, hourly_avgr$x, col = "red")

# x-Achse beschriften
legend("topright", legend = c("450 nm", "525 nm", "635 nm"), 
       col = c("red", "green", "blue"), lty = 1)

month_to_name <- function(month_num) {
  month_names <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
  return(month_names[month_num])
}

axis(side = 1, at = unique(hourly_avgb$hour), labels = month_to_name(unique(hourly_avgb$hour)), las = 2, cex.axis = 0.9)

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

# Plot erstellen mit angepasster y-Achse
plot(time, SSAdryb, col = "blue", type = "l", xlab = "time", ylab = "SSA dry", main = "SSA dry", ylim = c(-5,5))
lines(time, SSAdryg, col = "green")
lines(time, SSAdryr, col = "red")

# x-Achse beschriften

legend("topright", legend = c("450 nm", "525 nm", "635 nm"), 
       col = c("red", "green", "blue"), lty = 1)




