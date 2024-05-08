acorrected <- "/Users/baumannmike/Desktop/Datenbeispiele/Datenbeispiele/2024_04_16_MKN_datachain_test_AE31_1hr_corrected.csv"
corrae31data <- read.csv(acorrected)

araw <- "/Users/baumannmike/Desktop/Datenbeispiele/Datenbeispiele/2024_04_16_MKN_datachain_test_AE31_1hr_raw.csv"
rawae31data <- read.csv(araw)

ncorrected <- "/Users/baumannmike/Desktop/Datenbeispiele/Datenbeispiele/2024_04_16_MKN_datachain_test_neph_1hr_corrected.csv"
corrnephdata <- read.csv(ncorrected)

nraw <- "/Users/baumannmike/Desktop/Datenbeispiele/Datenbeispiele/2024_04_16_MKN_datachain_test_neph__1hr_raw.csv"
rawnephdata <- read.csv(nraw)

time <- as.POSIXct(rawae31data$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")


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
# Korrekturen angewendet auf eine Wellenlänge
#----------------------------------------------------------------------------------------------------

rawnephdata$BsB0_S11 <- as.numeric(rawnephdata$BsB0_S11)
rawnephdata$P0_S11 <- as.numeric(rawnephdata$P0_S11)
rawnephdata$T10_S11 <- as.numeric(rawnephdata$T10_S11)
rawae31data$X1c0_A11 <- as.numeric(rawae31data$X1c0_A11)
corrae31data$BaB0_ <- as.numeric(corrae31data$BaB0_)

# STP correction
Pinstr <- 650 #mean(rawnephdata$P0_S11, na.rm = TRUE)
Tinstr <- 20 + 273.15#rawnephdata$T10_S11 + 273.15
Pstp <- 1013.25
Tstp <- 273.15

STP <- (Pstp * Tinstr)/(Pinstr * Tstp)


# Calculation batn 
data.batn370  <- 1e-3 * rawae31data$X1c0_A11 * 39.5

#Apply Weingartner and STP
babs <- (data.batn370/3.5)

# Plot korrigierte Werte
plot(time, WG1, col = "blue", type = "l", xlab = "time", ylab = "Mm-1")

# Plot CPD output
plot(time, corrae31data$BaB0_, col = "red", type = "l", xlab = "time", ylab = "Mm-1")

# Correlationplot
# Plot erstellen
plot(babs, corrae31data$BaB0_, main = "Correlationplot AE31", 
     xlab = "corrected Values", ylab = "CPD Output", pch = 19, col = "blue")

# Regressionsgerade hinzufügen
abline(lm(corrae31data$BaB0_ ~ babs), col = "red")

fit <- lm(corrae31data$BaB0_ ~ babs)

# Koeffizienten abrufen
intercept <- coef(fit)[1]
slope <- coef(fit)[2]

legend_text <- paste("y =", round(intercept, 2), "+", round(slope, 2), "x")

# Legende hinzufügen
legend("topright", legend = legend_text, col = "red", lty = 1, cex = 0.8)

