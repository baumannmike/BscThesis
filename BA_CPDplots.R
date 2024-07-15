acorrected <- "/Users/baumannmike/Desktop/Datenbeispiele/Datenbeispiele/2019_03_01_5y_MKN_AE31_1hr_cleaned0523.csv"
corrae31data <- read.csv(acorrected)


ncorrected <- "/Users/baumannmike/Desktop/Datenbeispiele/Datenbeispiele/2019_03_01_5y_MKN_neph_1hr_cleaned0523.csv"
corrnephdata <- read.csv(ncorrected)



###################################
#AE31 monthly
###################################


# Konvertiere die DateTimeUTC-Spalte in POSIXct-Format
time <- as.POSIXct(corrae31data$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

# Extrahiere das Jahr und filtere die Daten für 2019
year <- format(time, "%Y")
data_2019 <- corrae31data[year == "2019", ]
data_2020 <- corrae31data[year == "2020", ]
data_2021 <- corrae31data[year == "2021", ]
data_2022 <- corrae31data[year == "2022", ]
data_2023 <- corrae31data[year == "2023", ]
data_2024 <- corrae31data[year == "2024", ]

# Laden der benötigten Bibliotheken



# Annahme: Die Daten für das zusätzliche Jahr befinden sich im DataFrame 'data_2020'

# Konvertiere die DateTimeUTC-Spalte in POSIXct-Format
data_2019$DateTimeUTC <- as.POSIXct(data_2019$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
data_2020$DateTimeUTC <- as.POSIXct(data_2020$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
data_2021$DateTimeUTC <- as.POSIXct(data_2021$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
data_2022$DateTimeUTC <- as.POSIXct(data_2022$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
data_2023$DateTimeUTC <- as.POSIXct(data_2023$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
data_2024$DateTimeUTC <- as.POSIXct(data_2024$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

# Extrahiere das Jahr und füge die Monatsspalte hinzu
data_2019$month <- format(data_2019$DateTimeUTC, "%m")
data_2020$month <- format(data_2020$DateTimeUTC, "%m")
data_2021$month <- format(data_2021$DateTimeUTC, "%m")
data_2022$month <- format(data_2022$DateTimeUTC, "%m")
data_2023$month <- format(data_2023$DateTimeUTC, "%m")
data_2024$month <- format(data_2024$DateTimeUTC, "%m")


# Wandle die Ba30_A11-Spalte in numerische Werte um
data_2019$Ba30_A11 <- as.numeric(data_2019$Ba30_A11, na.rm = TRUE)
data_2020$Ba30_A11 <- as.numeric(data_2020$Ba30_A11, na.rm = TRUE)
data_2021$Ba30_A11 <- as.numeric(data_2021$Ba30_A11, na.rm = TRUE)
data_2022$Ba30_A11 <- as.numeric(data_2022$Ba30_A11, na.rm = TRUE)
data_2023$Ba30_A11 <- as.numeric(data_2023$Ba30_A11, na.rm = TRUE)
data_2024$Ba30_A11 <- as.numeric(data_2024$Ba30_A11, na.rm = TRUE)

# Kombiniere Daten für beide Jahre
combined_data <- rbind(data_2019, data_2020, data_2021, data_2022, data_2023, data_2024)
combined_data <- na.omit(combined_data)

library(ggplot2)
library(dplyr)
library(lubridate)

light_colors <- c("#FFCCCC", "#FFE5CC", "#FFFFCC", "#E5FFCC", "#CCFFE5", "#CCE5FF")
# Plot erstellen und Ausreißer am Rand platzieren
# Plot erstellen und Ausreißer am Rand platzieren
ggplot(combined_data, aes(x = month, y = Ba30_A11, fill = factor(year(DateTimeUTC)))) +
  geom_boxplot(varwidth = TRUE, alpha = 1) +
  geom_point(data = combined_data %>% filter(Ba30_A11 < 0 | Ba30_A11 > 15), 
             aes(x = month, y = ifelse(Ba30_A11 < 0, 0, ifelse(Ba30_A11 > 15, 15, Ba30_A11))), 
             color = "red", alpha = 1) +
  scale_fill_manual(values = light_colors)+
  labs(title = "Monthly absorption coefficient [521nm]",
       x = "",
       y = expression(paste(sigma[a], " [Mm"^-1*"]")),
       fill = "Years") +
  scale_x_discrete(labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
                   breaks = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")) +
  scale_y_continuous(limits = c(0, 15), breaks = seq(0, 15, by = 2.5)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        panel.grid.major = element_line(color = "grey", size = 0.5), # Große Gitterlinien
        panel.grid.minor = element_line(color = "lightgrey", size = 0.25), # Kleine Gitterlinien
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        legend.title = element_text(face = "bold")) + # Schwarzer Rahmen um den Plot
  guides(fill = guide_legend(override.aes = list(shape = NA)))


##########################
##absortpion daily
##########################


corrae31data$DateTimeUTC <- as.POSIXct(corrae31data$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

# Extrahieren der Stunde aus dem Datum
corrae31data$hour <- as.numeric(format(corrae31data$DateTimeUTC, "%H"))

# Konvertieren der relevanten Spalten in numerische Werte
corrae31data$Ba30_A11 <- as.numeric(corrae31data$Ba30_A11)
corrae31data <- na.omit(corrae31data)

# Daten umformen
library(tidyr)
library(dplyr)
library(ggplot2)

long_data <- corrae31data %>%
  gather(key = "variable", value = "value", Ba30_A11)

# Berechnung der Anzahl der Werte pro Stunde
count_data <- long_data %>%
  group_by(hour) %>%
  summarize(count = n())

# Boxplot für Ba30_A11
ggplot(long_data, aes(x = factor(hour), y = value, fill = variable)) +
  geom_boxplot(varwidth = TRUE, width = 0.4) + # Anpassung der Boxplot-Breite an die Anzahl der Werte
  geom_point(data = long_data %>% filter(variable == "Ba30_A11", (value < 0 | value > 7)),
             aes(x = factor(hour), y = ifelse(value < 0, 0, ifelse(value > 7, 7, value))),
             color = "red", alpha = 1, size = 2) + # Punkte für Wert1 hinzufügen
  labs(
    title = "Daily absorption coefficient [521nm]",
    x = "UTC",
    y = expression(paste(sigma[a], " [Mm"^-1*"]")),
    fill = "Scattering type" # Ändert die Beschriftung der Legende
  ) +
  scale_fill_manual(
    values = c("Ba30_A11" = "#FFCCCC"),
    labels = c("Ba30_A11" = "total scattering") # Ändert die Namen der Variablen in der Legende
  ) +
  scale_y_continuous(limits = c(0, 7), breaks = seq(0, 7, by = 0.5)) +
  scale_x_discrete(labels = function(x) paste0(x, ":00")) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 90, hjust = 1, color = "black"),
    axis.text.y = element_text(color = "black"),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "grey", size = 0.5), # Große Gitterlinien
    panel.grid.minor = element_line(color = "lightgrey", size = 0.25), # Kleine Gitterlinien
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    legend.title = element_text(face = "bold"),
    legend.position = "none"
  ) +
  guides(
    fill = guide_legend(override.aes = list(shape = NA))
  )



###################################
#Aurora3000 monthly Total
###################################



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


library(ggplot2)


# Annahme: Die Daten für das zusätzliche Jahr befinden sich im DataFrame 'data_2020'

# Konvertiere die DateTimeUTC-Spalte in POSIXct-Format
data_2019$DateTimeUTC <- as.POSIXct(data_2019$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
data_2020$DateTimeUTC <- as.POSIXct(data_2020$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
data_2021$DateTimeUTC <- as.POSIXct(data_2021$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
data_2022$DateTimeUTC <- as.POSIXct(data_2022$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
data_2023$DateTimeUTC <- as.POSIXct(data_2023$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
data_2024$DateTimeUTC <- as.POSIXct(data_2024$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

# Extrahiere das Jahr und füge die Monatsspalte hinzu
data_2019$month <- format(data_2019$DateTimeUTC, "%m")
data_2020$month <- format(data_2020$DateTimeUTC, "%m")
data_2021$month <- format(data_2021$DateTimeUTC, "%m")
data_2022$month <- format(data_2022$DateTimeUTC, "%m")
data_2023$month <- format(data_2023$DateTimeUTC, "%m")
data_2024$month <- format(data_2024$DateTimeUTC, "%m")


# Wandle die Ba30_A11-Spalte in numerische Werte um
data_2019$BsG0_S11 <- as.numeric(data_2019$BsG0_S11, na.rm = TRUE)
data_2020$BsG0_S11 <- as.numeric(data_2020$BsG0_S11, na.rm = TRUE)
data_2021$BsG0_S11 <- as.numeric(data_2021$BsG0_S11, na.rm = TRUE)
data_2022$BsG0_S11 <- as.numeric(data_2022$BsG0_S11, na.rm = TRUE)
data_2023$BsG0_S11 <- as.numeric(data_2023$BsG0_S11, na.rm = TRUE)
data_2024$BsG0_S11 <- as.numeric(data_2024$BsG0_S11, na.rm = TRUE)

# Kombiniere Daten für beide Jahre
combined_data <- rbind(data_2019, data_2020, data_2021, data_2022, data_2023, data_2024)
combined_data <- na.omit(combined_data)

# Plot erstellen und Ausreißer am Rand platzieren
# Plot erstellen und Ausreißer am Rand platzieren
light_colors <- c("#FFCCCC", "#FFE5CC", "#FFFFCC", "#E5FFCC", "#CCFFE5", "#CCE5FF")
# Plot erstellen und Ausreißer am Rand platzieren
# Plot erstellen und Ausreißer am Rand platzieren
ggplot(combined_data, aes(x = month, y = BsG0_S11, fill = factor(year(DateTimeUTC)))) +
  geom_boxplot(varwidth = TRUE, alpha = 1) +
  geom_point(data = combined_data %>% filter(BsG0_S11 < 0 | BsG0_S11 > 200), 
             aes(x = month, y = ifelse(BsG0_S11 < 0, 0, ifelse(BsG0_S11 > 200, 200, BsG0_S11))), 
             color = "red", alpha = 1) +
  scale_fill_manual(values = light_colors)+
  labs(title = "Monthly total scattering coefficient [525nm]",
       x = "",
       y = expression(paste(sigma[s], " [Mm"^-1*"]")),
       fill = "Years") +
  scale_x_discrete(labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
                   breaks = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")) +
  scale_y_continuous(limits = c(0, 200), breaks = seq(0, 200, by = 25)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        panel.grid.major = element_line(color = "grey", size = 0.5), # Große Gitterlinien
        panel.grid.minor = element_line(color = "lightgrey", size = 0.25), # Kleine Gitterlinien
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        legend.title = element_text(face = "bold")) + # Schwarzer Rahmen um den Plot
  guides(fill = guide_legend(override.aes = list(shape = NA)))

###################################
#Aurora3000 monthly backscattering
###################################



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


library(ggplot2)


# Annahme: Die Daten für das zusätzliche Jahr befinden sich im DataFrame 'data_2020'

# Konvertiere die DateTimeUTC-Spalte in POSIXct-Format
data_2019$DateTimeUTC <- as.POSIXct(data_2019$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
data_2020$DateTimeUTC <- as.POSIXct(data_2020$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
data_2021$DateTimeUTC <- as.POSIXct(data_2021$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
data_2022$DateTimeUTC <- as.POSIXct(data_2022$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
data_2023$DateTimeUTC <- as.POSIXct(data_2023$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
data_2024$DateTimeUTC <- as.POSIXct(data_2024$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

# Extrahiere das Jahr und füge die Monatsspalte hinzu
data_2019$month <- format(data_2019$DateTimeUTC, "%m")
data_2020$month <- format(data_2020$DateTimeUTC, "%m")
data_2021$month <- format(data_2021$DateTimeUTC, "%m")
data_2022$month <- format(data_2022$DateTimeUTC, "%m")
data_2023$month <- format(data_2023$DateTimeUTC, "%m")
data_2024$month <- format(data_2024$DateTimeUTC, "%m")


# Wandle die Ba30_A11-Spalte in numerische Werte um
data_2019$BbsG0_S11 <- as.numeric(data_2019$BbsG0_S11, na.rm = TRUE)
data_2020$BbsG0_S11 <- as.numeric(data_2020$BbsG0_S11, na.rm = TRUE)
data_2021$BbsG0_S11 <- as.numeric(data_2021$BbsG0_S11, na.rm = TRUE)
data_2022$BbsG0_S11 <- as.numeric(data_2022$BbsG0_S11, na.rm = TRUE)
data_2023$BbsG0_S11 <- as.numeric(data_2023$BbsG0_S11, na.rm = TRUE)
data_2024$BbsG0_S11 <- as.numeric(data_2024$BbsG0_S11, na.rm = TRUE)

# Kombiniere Daten für beide Jahre
combined_data <- rbind(data_2019, data_2020, data_2021, data_2022, data_2023, data_2024)
combined_data <- na.omit(combined_data)

# Plot erstellen und Ausreißer am Rand platzieren
# Plot erstellen und Ausreißer am Rand platzieren
light_colors <- c("#FFCCCC", "#FFE5CC", "#FFFFCC", "#E5FFCC", "#CCFFE5", "#CCE5FF")
# Plot erstellen und Ausreißer am Rand platzieren
# Plot erstellen und Ausreißer am Rand platzieren
ggplot(combined_data, aes(x = month, y = BbsG0_S11, fill = factor(year(DateTimeUTC)))) +
  geom_boxplot(varwidth = TRUE, alpha = 1) +
  geom_point(data = combined_data %>% filter(BbsG0_S11 < 0 | BbsG0_S11 > 35), 
             aes(x = month, y = ifelse(BbsG0_S11 < 0, 0, ifelse(BbsG0_S11 > 35, 35, BbsG0_S11))), 
             color = "red", alpha = 1) +
  scale_fill_manual(values = light_colors)+
  labs(title = "Monthly backscattering coefficient [525nm]",
       x = "",
       y = expression(paste(sigma[b], " [Mm"^-1*"]")),
       fill = "Years") +
  scale_x_discrete(labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
                   breaks = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")) +
  scale_y_continuous(limits = c(0, 35), breaks = seq(0, 35, by = 5)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        panel.grid.major = element_line(color = "grey", size = 0.5), # Große Gitterlinien
        panel.grid.minor = element_line(color = "lightgrey", size = 0.25), # Kleine Gitterlinien
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        legend.title = element_text(face = "bold")) + # Schwarzer Rahmen um den Plot
  guides(fill = guide_legend(override.aes = list(shape = NA)))


##########################
##Scattering daily
##########################

# Daten einlesen (falls nötig)
# data <- read.csv("your_data.csv")

# Annahme: corrnephdata ist bereits in Ihrem Workspace geladen

# Konvertieren der DateTimeUTC-Spalte in ein POSIXct-Objekt
corrnephdata$DateTimeUTC <- as.POSIXct(corrnephdata$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

# Extrahieren der Stunde aus dem Datum
corrnephdata$hour <- as.numeric(format(corrnephdata$DateTimeUTC, "%H"))

# Konvertieren der relevanten Spalten in numerische Werte
corrnephdata$BsG0_S11 <- as.numeric(corrnephdata$BsG0_S11)
corrnephdata$BbsG0_S11 <- as.numeric(corrnephdata$BbsG0_S11)

# Entfernen von NA-Werten
corrnephdata <- na.omit(corrnephdata)

# Daten umformen
library(tidyr)

long_data <- corrnephdata %>%
  gather(key = "variable", value = "value", BsG0_S11, BbsG0_S11)

# Laden des ggplot2 Pakets
library(ggplot2)

# Boxplot für BsG0_S11 und BbsG0_S11
ggplot(long_data, aes(x = factor(hour), y = value, fill = variable)) +
  geom_boxplot() +
  geom_point(data = long_data %>% filter(variable == "BsG0_S11", (value < 0 | value > 120)),
             aes(x = factor(hour), y = ifelse(value < 0, 0, ifelse(value > 120, 120, value))),
             color = "red", alpha = 1, size = 2) + # Punkte für Wert1 hinzufügen
  geom_point(data = long_data %>% filter(variable == "BbsG0_S11", (value < 0 | value > 120)),
             aes(x = factor(hour), y = ifelse(value < 0, 0, ifelse(value > 120, 120, value))),
             color = "blue", alpha = 1, size = 2) + # 
  labs(
    title = "Daily scattering coefficient [525nm]",
    x = "UTC",
    y = expression(paste(" [Mm"^-1*"]")),
    fill = "Scattering type" # Ändert die Beschriftung der Legende
  ) +
  scale_fill_manual(
    values = c("BsG0_S11" = "#FFCCCC", "BbsG0_S11" = "#CCE5FF"),
    labels = c("BsG0_S11" = "total scattering", "BbsG0_S11" = "backscattering") # Ändert die Namen der Variablen in der Legende
  ) +
  scale_y_continuous(limits = c(0, 120), breaks = seq(0 ,120, by = 20)) +
  scale_x_discrete(labels = function(x) paste0(x, ":00")) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 90, hjust = 1, color = "black"),
    axis.text.y = element_text(color = "black"),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "grey", size = 0.5), # Große Gitterlinien
    panel.grid.minor = element_line(color = "lightgrey", size = 0.25), # Kleine Gitterlinien
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    legend.title = element_text(face = "bold")
  ) +
  guides(
    fill = guide_legend(override.aes = list(shape = NA)))

###################################
#AAE monthly
###################################

time <- as.POSIXct(corrae31data$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

# Extrahiere das Jahr und filtere die Daten für 2019
year <- format(time, "%Y")
data_2019 <- corrae31data[year == "2019", ]
data_2020 <- corrae31data[year == "2020", ]
data_2021 <- corrae31data[year == "2021", ]
data_2022 <- corrae31data[year == "2022", ]
data_2023 <- corrae31data[year == "2023", ]
data_2024 <- corrae31data[year == "2024", ]



# Konvertiere die DateTimeUTC-Spalte in POSIXct-Format
data_2019$DateTimeUTC <- as.POSIXct(data_2019$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
data_2020$DateTimeUTC <- as.POSIXct(data_2020$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
data_2021$DateTimeUTC <- as.POSIXct(data_2021$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
data_2022$DateTimeUTC <- as.POSIXct(data_2022$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
data_2023$DateTimeUTC <- as.POSIXct(data_2023$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
data_2024$DateTimeUTC <- as.POSIXct(data_2024$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

# Extrahiere das Jahr und füge die Monatsspalte hinzu
data_2019$month <- format(data_2019$DateTimeUTC, "%m")
data_2020$month <- format(data_2020$DateTimeUTC, "%m")
data_2021$month <- format(data_2021$DateTimeUTC, "%m")
data_2022$month <- format(data_2022$DateTimeUTC, "%m")
data_2023$month <- format(data_2023$DateTimeUTC, "%m")
data_2024$month <- format(data_2024$DateTimeUTC, "%m")

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


# Berechne AAE-Werte und füge sie den jeweiligen DataFrames hinzu
data_2019$AAE <- -log(data_2019$Ba20_A11 / data_2019$Ba60_A11) / log(470 / 880)
data_2019$AAE[is.infinite(data_2019$AAE)] <- NA

data_2020$AAE <- -log(data_2020$Ba20_A11 / data_2020$Ba60_A11) / log(470 / 880)
data_2020$AAE[is.infinite(data_2020$AAE)] <- NA

data_2021$AAE <- -log(data_2021$Ba20_A11 / data_2021$Ba60_A11) / log(470 / 880)
data_2021$AAE[is.infinite(data_2021$AAE)] <- NA

data_2022$AAE <- -log(data_2022$Ba20_A11 / data_2022$Ba60_A11) / log(470 / 880)
data_2022$AAE[is.infinite(data_2022$AAE)] <- NA

data_2023$AAE <- -log(data_2023$Ba20_A11 / data_2023$Ba60_A11) / log(470 / 880)
data_2023$AAE[is.infinite(data_2023$AAE)] <- NA

data_2024$AAE <- -log(data_2024$Ba20_A11 / data_2024$Ba60_A11) / log(470 / 880)
data_2024$AAE[is.infinite(data_2024$AAE)] <- NA



# Kombiniere Daten für beide Jahre
combined_data <- rbind(data_2019, data_2020, data_2021, data_2022, data_2023, data_2024)
combined_data <- na.omit(combined_data)


light_colors <- c("#FFCCCC", "#FFE5CC", "#FFFFCC", "#E5FFCC", "#CCFFE5", "#CCE5FF")
# Plot erstellen und Ausreißer am Rand platzieren
# Plot erstellen und Ausreißer am Rand platzieren
ggplot(combined_data, aes(x = month, y = AAE, fill = factor(year(DateTimeUTC)))) +
  geom_boxplot(varwidth = TRUE, alpha = 1) +
  geom_point(data = combined_data %>% filter(AAE < 0.5 | AAE > 1.75), 
             aes(x = month, y = ifelse(AAE < 0.5, 0.5, ifelse(AAE > 1.75, 1.75, AAE))), 
             color = "red", alpha = 1) +
  scale_fill_manual(values = light_colors)+
  labs(title = "Monthly AAE [470-880nm]",
       x = "",
       y = "AAE",
       fill = "Years") +
  scale_x_discrete(labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
                   breaks = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")) +
  scale_y_continuous(limits = c(0.5, 1.75), breaks = seq(0,1.75, by = 0.25)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        panel.grid.major = element_line(color = "grey", size = 0.5), # Große Gitterlinien
        panel.grid.minor = element_line(color = "lightgrey", size = 0.25), # Kleine Gitterlinien
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        legend.title = element_text(face = "bold")) + # Schwarzer Rahmen um den Plot
  guides(fill = guide_legend(override.aes = list(shape = NA)))

##########################
##AAE daily
##########################


corrae31data$DateTimeUTC <- as.POSIXct(corrae31data$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

# Extrahieren der Stunde aus dem Datum
corrae31data$hour <- as.numeric(format(corrae31data$DateTimeUTC, "%H"))

# Konvertieren der relevanten Spalten in numerische Werte
corrae31data$Ba10_A11 <- as.numeric(corrae31data$Ba10_A11)
corrae31data$Ba30_A11 <- as.numeric(corrae31data$Ba30_A11)

corrae31data$AAE <- -log(corrae31data$Ba10_A11/corrae31data$Ba30_A11)/log(370/521)
corrae31data$AAE[is.infinite(corrae31data$AAE)] <- NA
corrae31data <- na.omit(corrae31data)

# Daten umformen
library(tidyr)
library(dplyr)
library(ggplot2)

long_data <- corrae31data %>%
  gather(key = "variable", value = "value", AAE)

# Berechnung der Anzahl der Werte pro Stunde
count_data <- long_data %>%
  group_by(hour) %>%
  summarize(count = n())

# Boxplot für Ba30_A11
ggplot(long_data, aes(x = factor(hour), y = value, fill = variable)) +
  geom_boxplot(varwidth = TRUE, width = 0.4) + # Anpassung der Boxplot-Breite an die Anzahl der Werte
  geom_point(data = long_data %>% filter(variable == "AAE", (value < 0 | value > 2)),
             aes(x = factor(hour), y = ifelse(value < 0, 0, ifelse(value > 2, 2, value))),
             color = "red", alpha = 1, size = 2) + # Punkte für Wert1 hinzufügen
  labs(
    title = "Daily AAE [370-521nm]",
    x = "UTC",
    y = "AAE",
    fill = "Scattering type" # Ändert die Beschriftung der Legende
  ) +
  scale_fill_manual(
    values = c("AAE" = "#FFCCCC"),
    labels = c("AAE" = "total scattering") # Ändert die Namen der Variablen in der Legende
  ) +
  scale_y_continuous(limits = c(0, 2), breaks = seq(0, 2, by = 0.5)) +
  scale_x_discrete(labels = function(x) paste0(x, ":00")) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 90, hjust = 1, color = "black"),
    axis.text.y = element_text(color = "black"),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "grey", size = 0.5), # Große Gitterlinien
    panel.grid.minor = element_line(color = "lightgrey", size = 0.25), # Kleine Gitterlinien
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    legend.title = element_text(face = "bold"),
    legend.position = "none"
  ) +
  guides(
    fill = guide_legend(override.aes = list(shape = NA))
  )



##########################
##AAE daily 470 - 880
##########################


corrae31data$DateTimeUTC <- as.POSIXct(corrae31data$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

# Extrahieren der Stunde aus dem Datum
corrae31data$hour <- as.numeric(format(corrae31data$DateTimeUTC, "%H"))

# Konvertieren der relevanten Spalten in numerische Werte
corrae31data$Ba20_A11 <- as.numeric(corrae31data$Ba20_A11)
corrae31data$Ba60_A11 <- as.numeric(corrae31data$Ba60_A11)

corrae31data$AAE <- -log(corrae31data$Ba20_A11/corrae31data$Ba60_A11)/log(470/880)
corrae31data$AAE[is.infinite(corrae31data$AAE)] <- NA
corrae31data <- na.omit(corrae31data)

# Daten umformen
library(tidyr)
library(dplyr)
library(ggplot2)

long_data <- corrae31data %>%
  gather(key = "variable", value = "value", AAE)

# Berechnung der Anzahl der Werte pro Stunde
count_data <- long_data %>%
  group_by(hour) %>%
  summarize(count = n())

# Boxplot für Ba30_A11
ggplot(long_data, aes(x = factor(hour), y = value, fill = variable)) +
  geom_boxplot(varwidth = TRUE, width = 0.4) + # Anpassung der Boxplot-Breite an die Anzahl der Werte
  geom_point(data = long_data %>% filter(variable == "AAE", (value < 0 | value > 2)),
             aes(x = factor(hour), y = ifelse(value < 0, 0, ifelse(value > 2, 2, value))),
             color = "red", alpha = 1, size = 2) + # Punkte für Wert1 hinzufügen
  labs(
    title = "Daily AAE [470-880nm]",
    x = "UTC",
    y = "AAE",
    fill = "Scattering type" # Ändert die Beschriftung der Legende
  ) +
  scale_fill_manual(
    values = c("AAE" = "#FFCCCC"),
    labels = c("AAE" = "total scattering") # Ändert die Namen der Variablen in der Legende
  ) +
  scale_y_continuous(limits = c(0, 2), breaks = seq(0, 2, by = 0.5)) +
  scale_x_discrete(labels = function(x) paste0(x, ":00")) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 90, hjust = 1, color = "black"),
    axis.text.y = element_text(color = "black"),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "grey", size = 0.5), # Große Gitterlinien
    panel.grid.minor = element_line(color = "lightgrey", size = 0.25), # Kleine Gitterlinien
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    legend.title = element_text(face = "bold"),
    legend.position = "none"
  ) +
  guides(
    fill = guide_legend(override.aes = list(shape = NA))
  )



###################################
#SAE monthly
###################################

time <- as.POSIXct(corrnephdata$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
year <- format(time, "%Y")
data_2019 <- corrnephdata[year == "2019", ]
data_2020 <- corrnephdata[year == "2020", ]
data_2021 <- corrnephdata[year == "2021", ]
data_2022 <- corrnephdata[year == "2022", ]
data_2023 <- corrnephdata[year == "2023", ]
data_2024 <- corrnephdata[year == "2024", ]

data_2019$DateTimeUTC <- as.POSIXct(data_2019$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
data_2020$DateTimeUTC <- as.POSIXct(data_2020$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
data_2021$DateTimeUTC <- as.POSIXct(data_2021$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
data_2022$DateTimeUTC <- as.POSIXct(data_2022$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
data_2023$DateTimeUTC <- as.POSIXct(data_2023$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
data_2024$DateTimeUTC <- as.POSIXct(data_2024$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

# Extrahiere das Jahr und füge die Monatsspalte hinzu
data_2019$month <- format(data_2019$DateTimeUTC, "%m")
data_2020$month <- format(data_2020$DateTimeUTC, "%m")
data_2021$month <- format(data_2021$DateTimeUTC, "%m")
data_2022$month <- format(data_2022$DateTimeUTC, "%m")
data_2023$month <- format(data_2023$DateTimeUTC, "%m")
data_2024$month <- format(data_2024$DateTimeUTC, "%m")

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



data_2019$SAE <- -log(data_2019$BsB0_S11 / data_2019$BsR0_S11) / log(450/635)
data_2019$SAE[is.infinite(data_2019$SAE)] <- NA

data_2020$SAE <- -log(data_2020$BsB0_S11 / data_2020$BsR0_S11) / log(450/635)
data_2020$SAE[is.infinite(data_2020$SAE)] <- NA

data_2021$SAE <- -log(data_2021$BsB0_S11 / data_2021$BsR0_S11) / log(450/635)
data_2021$SAE[is.infinite(data_2021$SAE)] <- NA

data_2022$SAE <- -log(data_2022$BsB0_S11 / data_2022$BsR0_S11) / log(450/635)
data_2022$SAE[is.infinite(data_2022$SAE)] <- NA

data_2023$SAE <- -log(data_2023$BsB0_S11 / data_2023$BsR0_S11) / log(450/635)
data_2023$SAE[is.infinite(data_2023$SAE)] <- NA

data_2024$SAE <- -log(data_2024$BsB0_S11 / data_2024$BsR0_S11) / log(450/635)
data_2024$SAE[is.infinite(data_2024$SAE)] <- NA


combined_data <- rbind(data_2019, data_2020, data_2021, data_2022, data_2023, data_2024)
combined_data <- na.omit(combined_data)


light_colors <- c("#FFCCCC", "#FFE5CC", "#FFFFCC", "#E5FFCC", "#CCFFE5", "#CCE5FF")
# Plot erstellen und Ausreißer am Rand platzieren
# Plot erstellen und Ausreißer am Rand platzieren
ggplot(combined_data, aes(x = month, y = SAE, fill = factor(year(DateTimeUTC)))) +
  geom_boxplot(varwidth = TRUE, alpha = 1) +
  geom_point(data = combined_data %>% filter(SAE < 0 | SAE > 2.25), 
             aes(x = month, y = ifelse(SAE < 0, 0, ifelse(SAE > 2.25, 2.25, SAE))), 
             color = "red", alpha = 1) +
  scale_fill_manual(values = light_colors)+
  labs(title = "Monthly SAE [450-635nm]",
       x = "",
       y = "SAE",
       fill = "Years") +
  scale_x_discrete(labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
                   breaks = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")) +
  scale_y_continuous(limits = c(0, 2.25), breaks = seq(0,2.25, by = 0.25)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        panel.grid.major = element_line(color = "grey", size = 0.5), # Große Gitterlinien
        panel.grid.minor = element_line(color = "lightgrey", size = 0.25), # Kleine Gitterlinien
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        legend.title = element_text(face = "bold")) + # Schwarzer Rahmen um den Plot
  guides(fill = guide_legend(override.aes = list(shape = NA)))

##########################
##SAE daily
##########################


corrnephdata$DateTimeUTC <- as.POSIXct(corrnephdata$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

# Extrahieren der Stunde aus dem Datum
corrnephdata$hour <- as.numeric(format(corrnephdata$DateTimeUTC, "%H"))

# Konvertieren der relevanten Spalten in numerische Werte
corrnephdata$BsB0_S11 <- as.numeric(corrnephdata$BsB0_S11)
corrnephdata$BsR0_S11 <- as.numeric(corrnephdata$BsR0_S11)

corrnephdata$SAE <- -log(corrnephdata$BsB0_S11/corrnephdata$BsR0_S11)/log(450/635)
corrnephdata$SAE[is.infinite(corrnephdata$SAE)] <- NA
corrnephdata <- na.omit(corrnephdata)

# Daten umformen
library(tidyr)
library(dplyr)
library(ggplot2)

long_data <- corrnephdata %>%
  gather(key = "variable", value = "value", SAE)

# Berechnung der Anzahl der Werte pro Stunde
count_data <- long_data %>%
  group_by(hour) %>%
  summarize(count = n())

# Boxplot für Ba30_A11
ggplot(long_data, aes(x = factor(hour), y = value, fill = variable)) +
  geom_boxplot(varwidth = TRUE, width = 0.4) + # Anpassung der Boxplot-Breite an die Anzahl der Werte
  geom_point(data = long_data %>% filter(variable == "SAE", (value < 0 | value > 2)),
             aes(x = factor(hour), y = ifelse(value < 0, 0, ifelse(value > 2, 2, value))),
             color = "red", alpha = 1, size = 2) + # Punkte für Wert1 hinzufügen
  labs(
    title = "Daily SAE [450-635nm]",
    x = "UTC",
    y = "SAE",
    fill = "Scattering type" # Ändert die Beschriftung der Legende
  ) +
  scale_fill_manual(
    values = c("SAE" = "#FFCCCC"),
    labels = c("SAE" = "total scattering") # Ändert die Namen der Variablen in der Legende
  ) +
  scale_y_continuous(limits = c(0, 2), breaks = seq(0, 2, by = 0.25)) +
  scale_x_discrete(labels = function(x) paste0(x, ":00")) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 90, hjust = 1, color = "black"),
    axis.text.y = element_text(color = "black"),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "grey", size = 0.5), # Große Gitterlinien
    panel.grid.minor = element_line(color = "lightgrey", size = 0.25), # Kleine Gitterlinien
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    legend.title = element_text(face = "bold"),
    legend.position = "none"
  ) +
  guides(
    fill = guide_legend(override.aes = list(shape = NA))
  )



###################################
#backscatter fraction Monthly
###################################

time <- as.POSIXct(corrnephdata$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
year <- format(time, "%Y")
data_2019 <- corrnephdata[year == "2019", ]
data_2020 <- corrnephdata[year == "2020", ]
data_2021 <- corrnephdata[year == "2021", ]
data_2022 <- corrnephdata[year == "2022", ]
data_2023 <- corrnephdata[year == "2023", ]
data_2024 <- corrnephdata[year == "2024", ]

data_2019$DateTimeUTC <- as.POSIXct(data_2019$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
data_2020$DateTimeUTC <- as.POSIXct(data_2020$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
data_2021$DateTimeUTC <- as.POSIXct(data_2021$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
data_2022$DateTimeUTC <- as.POSIXct(data_2022$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
data_2023$DateTimeUTC <- as.POSIXct(data_2023$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
data_2024$DateTimeUTC <- as.POSIXct(data_2024$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

# Extrahiere das Jahr und füge die Monatsspalte hinzu
data_2019$month <- format(data_2019$DateTimeUTC, "%m")
data_2020$month <- format(data_2020$DateTimeUTC, "%m")
data_2021$month <- format(data_2021$DateTimeUTC, "%m")
data_2022$month <- format(data_2022$DateTimeUTC, "%m")
data_2023$month <- format(data_2023$DateTimeUTC, "%m")
data_2024$month <- format(data_2024$DateTimeUTC, "%m")

data_2019$BsG0_S11 <- as.numeric(data_2019$BsG0_S11)
data_2020$BsG0_S11 <- as.numeric(data_2020$BsG0_S11)
data_2021$BsG0_S11 <- as.numeric(data_2021$BsG0_S11)
data_2022$BsG0_S11 <- as.numeric(data_2022$BsG0_S11)
data_2023$BsG0_S11 <- as.numeric(data_2023$BsG0_S11)
data_2024$BsG0_S11 <- as.numeric(data_2024$BsG0_S11)

data_2019$BbsG0_S11 <- as.numeric(data_2019$BbsG0_S11)
data_2020$BbsG0_S11 <- as.numeric(data_2020$BbsG0_S11)
data_2021$BbsG0_S11 <- as.numeric(data_2021$BbsG0_S11)
data_2022$BbsG0_S11 <- as.numeric(data_2022$BbsG0_S11)
data_2023$BbsG0_S11 <- as.numeric(data_2023$BbsG0_S11)
data_2024$BbsG0_S11 <- as.numeric(data_2024$BbsG0_S11)



data_2019$green <- data_2019$BbsG0_S11/data_2019$BsG0_S11
data_2020$green <- data_2020$BbsG0_S11/data_2020$BsG0_S11
data_2021$green <- data_2021$BbsG0_S11/data_2021$BsG0_S11
data_2022$green <- data_2022$BbsG0_S11/data_2022$BsG0_S11
data_2023$green <- data_2023$BbsG0_S11/data_2023$BsG0_S11
data_2024$green <- data_2024$BbsG0_S11/data_2024$BsG0_S11

combined_data <- rbind(data_2019, data_2020, data_2021, data_2022, data_2023, data_2024)
combined_data <- na.omit(combined_data)

light_colors <- c("#FFCCCC", "#FFE5CC", "#FFFFCC", "#E5FFCC", "#CCFFE5", "#CCE5FF")
# Plot erstellen und Ausreißer am Rand platzieren
# Plot erstellen und Ausreißer am Rand platzieren
ggplot(combined_data, aes(x = month, y = green, fill = factor(year(DateTimeUTC)))) +
  geom_boxplot(varwidth = TRUE, alpha = 1) +
  geom_point(data = combined_data %>% filter(green < 0 | green > 0.25), 
             aes(x = month, y = ifelse(green < 0, 0, ifelse(green > 0.25, 0.25, green))), 
             color = "red", alpha = 1) +
  scale_fill_manual(values = light_colors)+
  labs(title = "Monthly backscatter fraction [525nm]",
       x = "",
       y = "b",
       fill = "Years") +
  scale_x_discrete(labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
                   breaks = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")) +
  scale_y_continuous(limits = c(0, 0.25), breaks = seq(0,0.25, by = 0.05)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        panel.grid.major = element_line(color = "grey", size = 0.5), # Große Gitterlinien
        panel.grid.minor = element_line(color = "lightgrey", size = 0.25), # Kleine Gitterlinien
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        legend.title = element_text(face = "bold")) + # Schwarzer Rahmen um den Plot
  guides(fill = guide_legend(override.aes = list(shape = NA)))

##########################
##backscatter fraction daily
##########################


corrnephdata$DateTimeUTC <- as.POSIXct(corrnephdata$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

# Extrahieren der Stunde aus dem Datum
corrnephdata$hour <- as.numeric(format(corrnephdata$DateTimeUTC, "%H"))

# Konvertieren der relevanten Spalten in numerische Werte
corrnephdata$BsG0_S11 <- as.numeric(corrnephdata$BsG0_S11)
corrnephdata$BbsG0_S11 <- as.numeric(corrnephdata$BbsG0_S11)

corrnephdata$b <- corrnephdata$BbsG0_S11/corrnephdata$BsG0_S11
corrnephdata$b[is.infinite(corrnephdata$b)] <- NA
corrnephdata <- na.omit(corrnephdata)

# Daten umformen
library(tidyr)
library(dplyr)
library(ggplot2)

long_data <- corrnephdata %>%
  gather(key = "variable", value = "value", b)

# Berechnung der Anzahl der Werte pro Stunde
count_data <- long_data %>%
  group_by(hour) %>%
  summarize(count = n())

# Boxplot für Ba30_A11
ggplot(long_data, aes(x = factor(hour), y = value, fill = variable)) +
  geom_boxplot(varwidth = TRUE, width = 0.4) + # Anpassung der Boxplot-Breite an die Anzahl der Werte
  geom_point(data = long_data %>% filter(variable == "b", (value < 0 | value > 0.25)),
             aes(x = factor(hour), y = ifelse(value < 0, 0, ifelse(value > 0.25, 0.25, value))),
             color = "red", alpha = 1, size = 2) + # Punkte für Wert1 hinzufügen
  labs(
    title = "Daily backscatter fraction [525nm]",
    x = "UTC",
    y = "b",
    fill = "Scattering type" # Ändert die Beschriftung der Legende
  ) +
  scale_fill_manual(
    values = c("b" = "#FFCCCC"),
    labels = c("b" = "total scattering") # Ändert die Namen der Variablen in der Legende
  ) +
  scale_y_continuous(limits = c(0, 0.25), breaks = seq(0, 0.25, by = 0.05)) +
  scale_x_discrete(labels = function(x) paste0(x, ":00")) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 90, hjust = 1, color = "black"),
    axis.text.y = element_text(color = "black"),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "grey", size = 0.5), # Große Gitterlinien
    panel.grid.minor = element_line(color = "lightgrey", size = 0.25), # Kleine Gitterlinien
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    legend.title = element_text(face = "bold"),
    legend.position = "none"
  ) +
  guides(
    fill = guide_legend(override.aes = list(shape = NA))
  )



###################################
#SSAdry Monthly
###################################



time2 <- as.POSIXct(corrnephdata$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
year2 <- format(time2, "%Y")
data_2019x <- corrnephdata[year2 == "2019", ]
data_2020x <- corrnephdata[year2 == "2020", ]
data_2021x <- corrnephdata[year2 == "2021", ]
data_2022x <- corrnephdata[year2 == "2022", ]
data_2023x <- corrnephdata[year2 == "2023", ]
data_2024x <- corrnephdata[year2 == "2024", ]

time <- as.POSIXct(corrae31data$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

# Extrahiere das Jahr und filtere die Daten für 2019
year <- format(time, "%Y")
data_2019 <- corrae31data[year == "2019", ]
data_2020 <- corrae31data[year == "2020", ]
data_2021 <- corrae31data[year == "2021", ]
data_2022 <- corrae31data[year == "2022", ]
data_2023 <- corrae31data[year == "2023", ]
data_2024 <- corrae31data[year == "2024", ]

data_2019$DateTimeUTC <- as.POSIXct(data_2019$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
data_2020$DateTimeUTC <- as.POSIXct(data_2020$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
data_2021$DateTimeUTC <- as.POSIXct(data_2021$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
data_2022$DateTimeUTC <- as.POSIXct(data_2022$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
data_2023$DateTimeUTC <- as.POSIXct(data_2023$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
data_2024$DateTimeUTC <- as.POSIXct(data_2024$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

# Extrahiere das Jahr und füge die Monatsspalte hinzu
data_2019$month <- format(data_2019$DateTimeUTC, "%m")
data_2020$month <- format(data_2020$DateTimeUTC, "%m")
data_2021$month <- format(data_2021$DateTimeUTC, "%m")
data_2022$month <- format(data_2022$DateTimeUTC, "%m")
data_2023$month <- format(data_2023$DateTimeUTC, "%m")
data_2024$month <- format(data_2024$DateTimeUTC, "%m")

data_2019x$DateTimeUTC <- as.POSIXct(data_2019x$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
data_2020x$DateTimeUTC <- as.POSIXct(data_2020x$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
data_2021x$DateTimeUTC <- as.POSIXct(data_2021x$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
data_2022x$DateTimeUTC <- as.POSIXct(data_2022x$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
data_2023x$DateTimeUTC <- as.POSIXct(data_2023x$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
data_2024x$DateTimeUTC <- as.POSIXct(data_2024x$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

# Extrahiere das Jahr und füge die Monatsspalte hinzu
data_2019x$month <- format(data_2019x$DateTimeUTC, "%m")
data_2020x$month <- format(data_2020x$DateTimeUTC, "%m")
data_2021x$month <- format(data_2021x$DateTimeUTC, "%m")
data_2022x$month <- format(data_2022x$DateTimeUTC, "%m")
data_2023x$month <- format(data_2023x$DateTimeUTC, "%m")
data_2024x$month <- format(data_2024x$DateTimeUTC, "%m")

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

data_2019x$BsG0_S11 <- as.numeric(data_2019x$BsG0_S11)
data_2020x$BsG0_S11 <- as.numeric(data_2020x$BsG0_S11)
data_2021x$BsG0_S11 <- as.numeric(data_2021x$BsG0_S11)
data_2022x$BsG0_S11 <- as.numeric(data_2022x$BsG0_S11)
data_2023x$BsG0_S11 <- as.numeric(data_2023x$BsG0_S11)
data_2024x$BsG0_S11 <- as.numeric(data_2024x$BsG0_S11)


merged_data2019 <- merge(data_2019, data_2019x, by = "DateTimeUTC", all.x = TRUE)
merged_data2020 <- merge(data_2020, data_2020x, by = "DateTimeUTC", all.x = TRUE)
merged_data2021 <- merge(data_2021, data_2021x, by = "DateTimeUTC", all.x = TRUE)
merged_data2022 <- merge(data_2022, data_2022x, by = "DateTimeUTC", all.x = TRUE)
merged_data2023 <- merge(data_2023, data_2023x, by = "DateTimeUTC", all.x = TRUE)
merged_data2024 <- merge(data_2024, data_2024x, by = "DateTimeUTC", all.x = TRUE)

merged_data2019$month <- format(merged_data2019$DateTimeUTC, "%m")
merged_data2020$month <- format(merged_data2020$DateTimeUTC, "%m")
merged_data2021$month <- format(merged_data2021$DateTimeUTC, "%m")
merged_data2022$month <- format(merged_data2022$DateTimeUTC, "%m")
merged_data2023$month <- format(merged_data2023$DateTimeUTC, "%m")
merged_data2024$month <- format(merged_data2024$DateTimeUTC, "%m")

AAE1 <- -log(merged_data2019 $Ba20_A11/merged_data2019 $Ba60_A11)/log(470/880)
AAE1[is.infinite(AAE1)] <- NA

AAE2 <- -log(merged_data2020$Ba20_A11/merged_data2020$Ba60_A11)/log(470/880)
AAE2[is.infinite(AAE2)] <- NA

AAE3 <- -log(merged_data2021$Ba20_A11/merged_data2021$Ba60_A11)/log(470/880)
AAE3[is.infinite(AAE3)] <- NA

AAE4 <- -log(merged_data2022$Ba20_A11/merged_data2022$Ba60_A11)/log(470/880)
AAE4[is.infinite(AAE4)] <- NA

AAE5 <- -log(merged_data2023$Ba20_A11/merged_data2023$Ba60_A11)/log(470/880)
AAE5[is.infinite(AAE5)] <- NA

AAE6 <- -log(merged_data2024$Ba20_A11/merged_data2024$Ba60_A11)/log(470/880)
AAE6[is.infinite(AAE6)] <- NA

merged_data2019$Ba40_A11 <- as.numeric(merged_data2019$Ba40_A11)
merged_data2020$Ba40_A11 <- as.numeric(merged_data2020$Ba40_A11)
merged_data2021$Ba40_A11 <- as.numeric(merged_data2021$Ba40_A11)
merged_data2022$Ba40_A11 <- as.numeric(merged_data2022$Ba40_A11)
merged_data2023$Ba40_A11 <- as.numeric(merged_data2023$Ba40_A11)
merged_data2024$Ba40_A11 <- as.numeric(merged_data2024$Ba40_A11)

Abs_525_2019 <- merged_data2019$Ba40_A11*(525/590)^(-1 * AAE1)
Abs_525_2020 <- merged_data2020$Ba40_A11*(525/590)^(-1 * AAE2)
Abs_525_2021 <- merged_data2021$Ba40_A11*(525/590)^(-1 * AAE3)
Abs_525_2022 <- merged_data2022$Ba40_A11*(525/590)^(-1 * AAE4)
Abs_525_2023 <- merged_data2023$Ba40_A11*(525/590)^(-1 * AAE5)
Abs_525_2024 <- merged_data2024$Ba40_A11*(525/590)^(-1 * AAE6)



merged_data2019$BsG0_S11 <- as.numeric(merged_data2019$BsG0_S11)
merged_data2020$BsG0_S11 <- as.numeric(merged_data2020$BsG0_S11)
merged_data2021$BsG0_S11 <- as.numeric(merged_data2021$BsG0_S11)
merged_data2022$BsG0_S11 <- as.numeric(merged_data2022$BsG0_S11)
merged_data2023$BsG0_S11 <- as.numeric(merged_data2023$BsG0_S11)
merged_data2024$BsG0_S11 <- as.numeric(merged_data2024$BsG0_S11)


bextg19 <- Abs_525_2019 + merged_data2019$BsG0_S11
bextg20 <- Abs_525_2020 + merged_data2020$BsG0_S11
bextg21 <- Abs_525_2021 + merged_data2021$BsG0_S11
bextg22 <- Abs_525_2022 + merged_data2022$BsG0_S11
bextg23<- Abs_525_2023 + merged_data2023$BsG0_S11
bextg24 <- Abs_525_2024 + merged_data2024$BsG0_S11

SSAdryg19 <- merged_data2019$BsG0_S11/bextg19
SSAdryg20 <- merged_data2020$BsG0_S11/bextg20
SSAdryg21 <- merged_data2021$BsG0_S11/bextg21
SSAdryg22 <- merged_data2022$BsG0_S11/bextg22
SSAdryg23 <- merged_data2023$BsG0_S11/bextg23
SSAdryg24 <- merged_data2024$BsG0_S11/bextg24



merged_data2019$SSAdry <-  merged_data2019$BsG0_S11/bextg19
merged_data2019$SSAdry[is.infinite(merged_data2019$SSAdry)] <- NA

merged_data2020$SSAdry <-  merged_data2020$BsG0_S11/bextg20
merged_data2020$SSAdry[is.infinite(merged_data2020$SSAdry)] <- NA

merged_data2021$SSAdry <-  merged_data2021$BsG0_S11/bextg21
merged_data2021$SSAdry[is.infinite(merged_data2021$SSAdry)] <- NA

merged_data2022$SSAdry <-  merged_data2022$BsG0_S11/bextg22
merged_data2022$SSAdry[is.infinite(merged_data2022$SSAdry)] <- NA

merged_data2023$SSAdry <-  merged_data2023$BsG0_S11/bextg23
merged_data2023$SSAdry[is.infinite(merged_data2023$SSAdry)] <- NA

merged_data2024$SSAdry <-  merged_data2024$BsG0_S11/bextg24
merged_data2024$SSAdry[is.infinite(merged_data2024$SSAdry)] <- NA

combined_data <- rbind(merged_data2019, merged_data2020, merged_data2021, merged_data2022, merged_data2023, merged_data2024)
combined_data <- na.omit(combined_data)

light_colors <- c("#FFCCCC", "#FFE5CC", "#FFFFCC", "#E5FFCC", "#CCFFE5", "#CCE5FF")
# Plot erstellen und Ausreißer am Rand platzieren
# Plot erstellen und Ausreißer am Rand platzieren
ggplot(combined_data, aes(x = month, y = SSAdry, fill = factor(year(DateTimeUTC)))) +
  geom_boxplot(varwidth = TRUE, alpha = 1) +
  geom_point(data = combined_data %>% filter(SSAdry < 0.88 | SSAdry > 1), 
             aes(x = month, y = ifelse(SSAdry < 0.88, 0.88, ifelse(SSAdry > 1, 1, SSAdry))), 
             color = "red", alpha = 1) +
  scale_fill_manual(values = light_colors)+
  labs(title = "Monthly SSA [525nm]",
       x = "",
       y = "SSA",
       fill = "Years") +
  scale_x_discrete(labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
                   breaks = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")) +
  scale_y_continuous(limits = c(0.88, 1), breaks = seq(0.88 ,1, by = 0.025)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        panel.grid.major = element_line(color = "grey", size = 0.5), # Große Gitterlinien
        panel.grid.minor = element_line(color = "lightgrey", size = 0.25), # Kleine Gitterlinien
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        legend.title = element_text(face = "bold")) + # Schwarzer Rahmen um den Plot
  guides(fill = guide_legend(override.aes = list(shape = NA)))

###################################
#SSAdry daily
###################################

# Laden der erforderlichen Bibliotheken
library(ggplot2)
library(tidyr)
library(dplyr)

# Sicherstellen, dass die Spalten numerisch sind
corrae31data$Ba20_A11 <- as.numeric(corrae31data$Ba20_A11)
corrae31data$Ba60_A11 <- as.numeric(corrae31data$Ba60_A11)
corrae31data$Ba40_A11 <- as.numeric(corrae31data$Ba40_A11)

# Berechnung von AAE ohne NA-Werte
AAE1 <- -log(corrae31data$Ba20_A11 / corrae31data$Ba60_A11) / log(470 / 880)

# Berechnung von Abs_525
Abs_525 <- corrae31data$Ba40_A11 * (525 / 590)^(-1 * AAE1)

# Konvertieren der Zeitspalten in POSIXct-Format
time <- as.POSIXct(corrae31data$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
time2 <- as.POSIXct(corrnephdata$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

# Daten zusammenführen
merged_data <- merge(corrae31data, corrnephdata, by = "DateTimeUTC", all.x = TRUE)

# Sicherstellen, dass die BsG0_S11-Spalte numerisch ist
merged_data$BsG0_S11 <- as.numeric(merged_data$BsG0_S11)

# Berechnung von bextg und SSA
bextg <- Abs_525 + merged_data$BsG0_S11
merged_data$SSA <- merged_data$BsG0_S11 / bextg

# Extrahieren der Stunde aus der Zeitspalte
merged_data$hour <- as.numeric(format(time, "%H"))

# Daten in ein langes Format bringen
long_data <- merged_data %>%
  gather(key = "variable", value = "value", SSA)

# Filtern der NA-Werte
long_data <- long_data[!is.na(long_data$hour), ]

# Berechnung der Anzahl der Werte pro Stunde
count_data <- long_data %>%
  group_by(hour) %>%
  summarize(count = n())

# Erstellen des Boxplots
ggplot(long_data, aes(x = factor(hour), y = value, fill = variable)) +
  geom_boxplot(varwidth = TRUE, width = 0.4) +  # Anpassung der Boxplot-Breite an die Anzahl der Werte
  geom_point(data = long_data %>% filter(variable == "SSA", (value < 0.88 | value > 1.00)),
             aes(x = factor(hour), y = ifelse(value < 0.88, 0.88, ifelse(value > 1.00, 1.00, value))),
             color = "red", alpha = 1, size = 2) +  # Punkte für Wert1 hinzufügen
  labs(
    title = "Daily SSA [525nm]",
    x = "UTC",
    y = "SSA",
    fill = "Scattering type"  # Ändert die Beschriftung der Legende
  ) +
  scale_fill_manual(
    values = c("SSA" = "#FFCCCC"),
    labels = c("SSA" = "total scattering")  # Ändert die Namen der Variablen in der Legende
  ) +
  scale_y_continuous(limits = c(0.88, 1.00), breaks = seq(0.88, 1.00, by = 0.02)) +
  scale_x_discrete(labels = function(x) paste0(x, ":00")) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 90, hjust = 1, color = "black"),
    axis.text.y = element_text(color = "black"),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "grey", size = 0.5),  # Große Gitterlinien
    panel.grid.minor = element_line(color = "lightgrey", size = 0.25),  # Kleine Gitterlinien
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    legend.title = element_text(face = "bold"),
    legend.position = "none"
  ) +
  guides(
    fill = guide_legend(override.aes = list(shape = NA))
  )



###################################
#BC monthly
###################################

# Konvertiere die DateTimeUTC-Spalte in POSIXct-Format
time <- as.POSIXct(corrae31data$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

# Extrahiere das Jahr und filtere die Daten für 2019
year <- format(time, "%Y")
data_2019 <- corrae31data[year == "2019", ]
data_2020 <- corrae31data[year == "2020", ]
data_2021 <- corrae31data[year == "2021", ]
data_2022 <- corrae31data[year == "2022", ]
data_2023 <- corrae31data[year == "2023", ]
data_2024 <- corrae31data[year == "2024", ]

# Annahme: Die Daten für das zusätzliche Jahr befinden sich im DataFrame 'data_2020'

# Konvertiere die DateTimeUTC-Spalte in POSIXct-Format
data_2019$DateTimeUTC <- as.POSIXct(data_2019$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
data_2020$DateTimeUTC <- as.POSIXct(data_2020$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
data_2021$DateTimeUTC <- as.POSIXct(data_2021$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
data_2022$DateTimeUTC <- as.POSIXct(data_2022$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
data_2023$DateTimeUTC <- as.POSIXct(data_2023$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
data_2024$DateTimeUTC <- as.POSIXct(data_2024$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

# Extrahiere das Jahr und füge die Monatsspalte hinzu
data_2019$month <- format(data_2019$DateTimeUTC, "%m")
data_2020$month <- format(data_2020$DateTimeUTC, "%m")
data_2021$month <- format(data_2021$DateTimeUTC, "%m")
data_2022$month <- format(data_2022$DateTimeUTC, "%m")
data_2023$month <- format(data_2023$DateTimeUTC, "%m")
data_2024$month <- format(data_2024$DateTimeUTC, "%m")


# Wandle die Ba30_A11-Spalte in numerische Werte um
data_2019$X6c0_A11 <- as.numeric(data_2019$X6c0_A11, na.rm = TRUE)
data_2020$X6c0_A11 <- as.numeric(data_2020$X6c0_A11, na.rm = TRUE)
data_2021$X6c0_A11 <- as.numeric(data_2021$X6c0_A11, na.rm = TRUE)
data_2022$X6c0_A11 <- as.numeric(data_2022$X6c0_A11, na.rm = TRUE)
data_2023$X6c0_A11 <- as.numeric(data_2023$X6c0_A11, na.rm = TRUE)
data_2024$X6c0_A11 <- as.numeric(data_2024$X6c0_A11, na.rm = TRUE)

# Kombiniere Daten für beide Jahre
combined_data <- rbind(data_2019, data_2020, data_2021, data_2022, data_2023, data_2024)
combined_data <- na.omit(combined_data)

library(ggplot2)
library(dplyr)
library(lubridate)

light_colors <- c("#FFCCCC", "#FFE5CC", "#FFFFCC", "#E5FFCC", "#CCFFE5", "#CCE5FF")
# Plot erstellen und Ausreißer am Rand platzieren
# Plot erstellen und Ausreißer am Rand platzieren
ggplot(combined_data, aes(x = month, y = X6c0_A11, fill = factor(year(DateTimeUTC)))) +
  geom_boxplot(varwidth = TRUE, alpha = 1) +
  geom_point(data = combined_data %>% filter(X6c0_A11 < 0 | X6c0_A11 > 0.5), 
             aes(x = month, y = ifelse(X6c0_A11 < 0, 0, ifelse(X6c0_A11 > 0.5, 0.5, X6c0_A11))), 
             color = "red", alpha = 1) +
  scale_fill_manual(values = light_colors)+
  labs(title = "Monthly equivalent black carbon concentration [880nm]",
       x = "",
       y = expression("\u03BCg/m"^3),
       fill = "Years") +
  scale_x_discrete(labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
                   breaks = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")) +
  scale_y_continuous(limits = c(0, 0.5), breaks = seq(0, 0.5, by = 0.05)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        panel.grid.major = element_line(color = "grey", size = 0.5), # Große Gitterlinien
        panel.grid.minor = element_line(color = "lightgrey", size = 0.25), # Kleine Gitterlinien
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        legend.title = element_text(face = "bold")) + # Schwarzer Rahmen um den Plot
  guides(fill = guide_legend(override.aes = list(shape = NA)))

###################################
#BC daily
###################################

corrae31data$DateTimeUTC <- as.POSIXct(corrae31data$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

# Extrahieren der Stunde aus dem Datum
corrae31data$hour <- as.numeric(format(corrae31data$DateTimeUTC, "%H"))

# Konvertieren der relevanten Spalten in numerische Werte
corrae31data$X6c0_A11 <- as.numeric(corrae31data$X6c0_A11)
corrae31data <- na.omit(corrae31data)

# Daten umformen
library(tidyr)
library(dplyr)
library(ggplot2)

long_data <- corrae31data %>%
  gather(key = "variable", value = "value", X6c0_A11)

# Berechnung der Anzahl der Werte pro Stunde
count_data <- long_data %>%
  group_by(hour) %>%
  summarize(count = n())

# Boxplot für Ba30_A11
ggplot(long_data, aes(x = factor(hour), y = value, fill = variable)) +
  geom_boxplot(varwidth = TRUE, width = 0.4) + # Anpassung der Boxplot-Breite an die Anzahl der Werte
  geom_point(data = long_data %>% filter(variable == "X6c0_A11", (value < 0 | value > 0.45)),
             aes(x = factor(hour), y = ifelse(value < 0, 0, ifelse(value > 0.45, 0.45, value))),
             color = "red", alpha = 1, size = 2) + # Punkte für Wert1 hinzufügen
  labs(
    title = "Daily equivalent black carbon concentration [880nm]",
    x = "UTC",
    y = expression("\u03BCg/m"^3),
    fill = "Scattering type" # Ändert die Beschriftung der Legende
  ) +
  scale_fill_manual(
    values = c("X6c0_A11" = "#FFCCCC"),
    labels = c("X6c0_A11" = "total scattering") # Ändert die Namen der Variablen in der Legende
  ) +
  scale_y_continuous(limits = c(0, 0.45), breaks = seq(0, 0.45, by = 0.05)) +
  scale_x_discrete(labels = function(x) paste0(x, ":00")) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 90, hjust = 1, color = "black"),
    axis.text.y = element_text(color = "black"),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "grey", size = 0.5), # Große Gitterlinien
    panel.grid.minor = element_line(color = "lightgrey", size = 0.25), # Kleine Gitterlinien
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    legend.title = element_text(face = "bold"),
    legend.position = "none"
  ) +
  guides(
    fill = guide_legend(override.aes = list(shape = NA))
  )

###################################
#monthly temperature
###################################



time <- as.POSIXct(corrnephdata$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
year <- format(time, "%Y")
data_2019 <- corrnephdata[year == "2019", ]
data_2020 <- corrnephdata[year == "2020", ]
data_2021 <- corrnephdata[year == "2021", ]
data_2022 <- corrnephdata[year == "2022", ]
data_2023 <- corrnephdata[year == "2023", ]
data_2024 <- corrnephdata[year == "2024", ]


library(ggplot2)


# Annahme: Die Daten für das zusätzliche Jahr befinden sich im DataFrame 'data_2020'

# Konvertiere die DateTimeUTC-Spalte in POSIXct-Format
data_2019$DateTimeUTC <- as.POSIXct(data_2019$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
data_2020$DateTimeUTC <- as.POSIXct(data_2020$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
data_2021$DateTimeUTC <- as.POSIXct(data_2021$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
data_2022$DateTimeUTC <- as.POSIXct(data_2022$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
data_2023$DateTimeUTC <- as.POSIXct(data_2023$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
data_2024$DateTimeUTC <- as.POSIXct(data_2024$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

# Extrahiere das Jahr und füge die Monatsspalte hinzu
data_2019$month <- format(data_2019$DateTimeUTC, "%m")
data_2020$month <- format(data_2020$DateTimeUTC, "%m")
data_2021$month <- format(data_2021$DateTimeUTC, "%m")
data_2022$month <- format(data_2022$DateTimeUTC, "%m")
data_2023$month <- format(data_2023$DateTimeUTC, "%m")
data_2024$month <- format(data_2024$DateTimeUTC, "%m")


# Wandle die Ba30_A11-Spalte in numerische Werte um
data_2019$T10_S11 <- as.numeric(data_2019$T10_S11, na.rm = TRUE)
data_2020$T10_S11 <- as.numeric(data_2020$T10_S11, na.rm = TRUE)
data_2021$T10_S11 <- as.numeric(data_2021$T10_S11, na.rm = TRUE)
data_2022$T10_S11 <- as.numeric(data_2022$T10_S11, na.rm = TRUE)
data_2023$T10_S11 <- as.numeric(data_2023$T10_S11, na.rm = TRUE)
data_2024$T10_S11 <- as.numeric(data_2024$T10_S11, na.rm = TRUE)

# Kombiniere Daten für beide Jahre
combined_data <- rbind(data_2019, data_2020, data_2021, data_2022, data_2023, data_2024)
combined_data <- na.omit(combined_data)

# Plot erstellen und Ausreißer am Rand platzieren
# Plot erstellen und Ausreißer am Rand platzieren
light_colors <- c("#FFCCCC", "#FFE5CC", "#FFFFCC", "#E5FFCC", "#CCFFE5", "#CCE5FF")
# Plot erstellen und Ausreißer am Rand platzieren
# Plot erstellen und Ausreißer am Rand platzieren
ggplot(combined_data, aes(x = month, y = T10_S11, fill = factor(year(DateTimeUTC)))) +
  geom_boxplot(varwidth = TRUE, alpha = 1) +
  geom_point(data = combined_data %>% filter(T10_S11 < 0 | T10_S11 > 40), 
             aes(x = month, y = ifelse(T10_S11 < 0, 0, ifelse(T10_S11 > 40, 40, T10_S11))), 
             color = "red", alpha = 1) +
  scale_fill_manual(values = light_colors)+
  labs(title = "Temperature inside nephelometer",
       x = "",
       y = "°C",
       fill = "Years") +
  scale_x_discrete(labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
                   breaks = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")) +
  scale_y_continuous(limits = c(0, 40), breaks = seq(0, 40, by = 5)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        panel.grid.major = element_line(color = "grey", size = 0.5), # Große Gitterlinien
        panel.grid.minor = element_line(color = "lightgrey", size = 0.25), # Kleine Gitterlinien
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        legend.title = element_text(face = "bold")) + # Schwarzer Rahmen um den Plot
  guides(fill = guide_legend(override.aes = list(shape = NA)))


###################################
#Temperature daily
###################################
acorrected <- "/Users/baumannmike/Desktop/Datenbeispiele/Datenbeispiele/2019_03_01_5y_MKN_AE31_1hr_cleaned0523.csv"
corrae31data <- read.csv(acorrected)


ncorrected <- "/Users/baumannmike/Desktop/Datenbeispiele/Datenbeispiele/2019_03_01_5y_MKN_neph_1hr_cleaned0523.csv"
corrnephdata <- read.csv(ncorrected)


corrnephdata$DateTimeUTC <- as.POSIXct(corrnephdata$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

# Extrahieren der Stunde aus dem Datum
corrnephdata$hour <- as.numeric(format(corrnephdata$DateTimeUTC, "%H"))

# Konvertieren der relevanten Spalten in numerische Werte
corrnephdata$T10_S11 <- as.numeric(corrnephdata$T10_S11)
corrnephdata <- na.omit(corrnephdata)

# Daten umformen
library(tidyr)
library(dplyr)
library(ggplot2)

long_data <- corrnephdata %>%
  gather(key = "variable", value = "value", T10_S11)

# Berechnung der Anzahl der Werte pro Stunde
count_data <- long_data %>%
  group_by(hour) %>%
  summarize(count = n())

# Boxplot für Ba30_A11
ggplot(long_data, aes(x = factor(hour), y = value, fill = variable)) +
  geom_boxplot(varwidth = TRUE, width = 0.4) + # Anpassung der Boxplot-Breite an die Anzahl der Werte
  geom_point(data = long_data %>% filter(variable == "T10_S11", (value < 0 | value > 40)),
             aes(x = factor(hour), y = ifelse(value < 0, 0, ifelse(value > 40, 40, value))),
             color = "red", alpha = 1, size = 2) + # Punkte für Wert1 hinzufügen
  labs(
    title = "Temperature inside nephelometer",
    x = "UTC",
    y = "°C",
    fill = "Scattering type" # Ändert die Beschriftung der Legende
  ) +
  scale_fill_manual(
    values = c("T10_S11" = "#FFCCCC"),
    labels = c("T10_S11" = "total scattering") # Ändert die Namen der Variablen in der Legende
  ) +
  scale_y_continuous(limits = c(0, 40), breaks = seq(0, 40, by = 5)) +
  scale_x_discrete(labels = function(x) paste0(x, ":00")) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 90, hjust = 1, color = "black"),
    axis.text.y = element_text(color = "black"),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "grey", size = 0.5), # Große Gitterlinien
    panel.grid.minor = element_line(color = "lightgrey", size = 0.25), # Kleine Gitterlinien
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    legend.title = element_text(face = "bold"),
    legend.position = "none"
  ) +
  guides(
    fill = guide_legend(override.aes = list(shape = NA))
  )


