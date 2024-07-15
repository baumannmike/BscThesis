acorrected <- "/Users/baumannmike/Desktop/Datenbeispiele/Datenbeispiele/2019_03_01_5y_MKN_AE31_1hr_cleaned0523.csv"
corrae31data <- read.csv(acorrected)


ncorrected <- "/Users/baumannmike/Desktop/Datenbeispiele/Datenbeispiele/2019_03_01_5y_MKN_neph_1hr_cleaned0523.csv"
corrnephdata <- read.csv(ncorrected)

# Notwendige Bibliotheken laden


corrae31data$Ba30_A11 <- as.numeric(corrae31data$Ba30_A11)

time <- as.POSIXct(corrae31data$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

# Extrahiere das Jahr und filtere die Daten für 2019
year <- format(time, "%Y")
data_2019 <- corrae31data[year == "2019", ]
data_2020 <- corrae31data[year == "2020", ]
data_2021 <- corrae31data[year == "2021", ]
data_2022 <- corrae31data[year == "2022", ]
data_2023 <- corrae31data[year == "2023", ]
data_2024 <- corrae31data[year == "2024", ]

library(ggplot2)
library(dplyr)

werte <- data_2019$Ba30_A11
werte <- na.omit(data_2019$Ba30_A11)

werte2 <- data_2020$Ba30_A11
werte2 <- na.omit(data_2020$Ba30_A11)

werte3 <- data_2021$Ba30_A11
werte3 <- na.omit(data_2021$Ba30_A11)

werte4 <- data_2022$Ba30_A11
werte4 <- na.omit(data_2022$Ba30_A11)

werte5 <- data_2023$Ba30_A11
werte5 <- na.omit(data_2023$Ba30_A11)

werte6 <- data_2024$Ba30_A11
werte6 <- na.omit(data_2024$Ba30_A11)

n <- length(werte)
iqr <- IQR(werte)
bin_width <- 2 * iqr / (n^(1/3))
num_bins <- diff(range(werte)) / bin_width

n <- length(werte2)
iqr <- IQR(werte2)
bin_width <- 2 * iqr / (n^(1/3))
num_bins2 <- diff(range(werte2)) / bin_width

n <- length(werte3)
iqr <- IQR(werte3)
bin_width <- 2 * iqr / (n^(1/3))
num_bins3 <- diff(range(werte2)) / bin_width

n <- length(werte4)
iqr <- IQR(werte4)
bin_width <- 2 * iqr / (n^(1/3))
num_bins4 <- diff(range(werte2)) / bin_width

n <- length(werte5)
iqr <- IQR(werte5)
bin_width <- 2 * iqr / (n^(1/3))
num_bins5 <- diff(range(werte2)) / bin_width

n <- length(werte6)
iqr <- IQR(werte6)
bin_width <- 2 * iqr / (n^(1/3))
num_bins6 <- diff(range(werte2)) / bin_width




# Verteilung der Werte plotten
percentile_95 <- quantile(werte, 0.95)

# Erstellen des Diagramms mit Markierung des 95%-Quartils
ggplot(data.frame(Values = werte), aes(x = Values)) +
  geom_histogram(bins = 72, fill = "#87CEEB", alpha = 1, color = "black") +
  geom_vline(xintercept = percentile_95, linetype = "dashed", color = "red") +
  ggtitle("2019") +
  xlab(expression(paste(sigma[a], " [Mm"^-1*"]"))) +
  ylab("Count") +
  theme(panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major = element_line(color = "lightgray"),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        plot.title = element_text(face = "bold", hjust = 0.5)) +
  scale_x_continuous(breaks = seq(0, 40, by = 2)) # Fetter Titel

ggplot(data.frame(Values = werte2), aes(x = Values)) +
  geom_histogram(bins = 62, fill = "#87CEEB", alpha = 1, color = "black") +
  ggtitle("2020") +
  xlab(expression(paste(sigma[a], " [Mm"^-1*"]"))) +
  ylab("Count") +
  theme(panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major = element_line(color = "lightgray"),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        plot.title = element_text(face = "bold", hjust = 0.5)) +
scale_x_continuous(breaks = seq(0, 20, by = 2))

ggplot(data.frame(Values = werte3), aes(x = Values)) +
  geom_histogram(bins = 55, fill = "#87CEEB", alpha = 1, color = "black") +
  ggtitle("2021") +
  xlab(expression(paste(sigma[a], " [Mm"^-1*"]"))) +
  ylab("Count") +
  theme(panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major = element_line(color = "lightgray"),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        plot.title = element_text(face = "bold", hjust = 0.5))  +
  scale_x_continuous(breaks = seq(0, 25, by = 5))# Fetter Titel

percentile_95 <- quantile(werte4, 0.95)

# Erstellen des Diagramms mit Markierung des 95%-Quartils
ggplot(data.frame(Values = werte4), aes(x = Values)) +
  geom_histogram(bins = 55, fill = "#87CEEB", alpha = 1, color = "black") +
  geom_vline(xintercept = percentile_95, linetype = "dashed", color = "red") +
  ggtitle("2022") +
  xlab(expression(paste(sigma[a], " [Mm"^-1*"]"))) +
  ylab("Count") +
  theme(panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major = element_line(color = "lightgray"),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        plot.title = element_text(face = "bold", hjust = 0.5)) +
  scale_x_continuous(breaks = seq(0, 90, by = 10))



ggplot(data.frame(Values = werte5), aes(x = Values)) +
  geom_histogram(bins = 70, fill = "#87CEEB", alpha = 1, color = "black") +
  ggtitle("2023") +
  xlab(expression(paste(sigma[a], " [Mm"^-1*"]"))) +
  ylab("Count") +
  theme(panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major = element_line(color = "lightgray"),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        plot.title = element_text(face = "bold", hjust = 0.5)) + # Fetter Titel
scale_x_continuous(breaks = seq(0, 12, by = 2))# Fetter Titel

ggplot(data.frame(Values = werte6), aes(x = Values)) +
  geom_histogram(bins = 54, fill = "#87CEEB", alpha = 1, color = "black") +
  ggtitle("2024") +
  xlab(expression(paste(sigma[a], " [Mm"^-1*"]"))) +
  ylab("Count") +
  theme(panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major = element_line(color = "lightgray"),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        plot.title = element_text(face = "bold", hjust = 0.5)) +
  scale_x_continuous(breaks = seq(0, 12, by = 2))# Fetter Titel


###############################################################################


corrnephdata$BbsG0_S11 <- as.numeric(corrnephdata$BbsG0_S11)

time <- as.POSIXct(corrnephdata$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

# Extrahiere das Jahr und filtere die Daten für 2019
year <- format(time, "%Y")
data_2019 <- corrnephdata[year == "2019", ]
data_2020 <- corrnephdata[year == "2020", ]
data_2021 <- corrnephdata[year == "2021", ]
data_2022 <- corrnephdata[year == "2022", ]
data_2023 <- corrnephdata[year == "2023", ]
data_2024 <- corrnephdata[year == "2024", ]

library(ggplot2)
library(dplyr)

werte <- data_2019$BbsG0_S11
werte <- na.omit(data_2019$BbsG0_S11)

werte2 <- data_2020$BbsG0_S11
werte2 <- na.omit(data_2020$BbsG0_S11)

werte3 <- data_2020$BbsG0_S11
werte3 <- na.omit(data_2021$BbsG0_S11)

werte4 <- data_2020$BbsG0_S11
werte4 <- na.omit(data_2022$BbsG0_S11)

werte5 <- data_2020$BbsG0_S11
werte5 <- na.omit(data_2023$BbsG0_S11)

werte6 <- data_2020$BbsG0_S11
werte6 <- na.omit(data_2024$BbsG0_S11)

n <- length(werte)
iqr <- IQR(werte)
bin_width <- 2 * iqr / (n^(1/3))
num_bins <- diff(range(werte)) / bin_width

n <- length(werte2)
iqr <- IQR(werte2)
bin_width <- 2 * iqr / (n^(1/3))
num_bins2 <- diff(range(werte2)) / bin_width

n <- length(werte3)
iqr <- IQR(werte3)
bin_width <- 2 * iqr / (n^(1/3))
num_bins3 <- diff(range(werte2)) / bin_width

n <- length(werte4)
iqr <- IQR(werte4)
bin_width <- 2 * iqr / (n^(1/3))
num_bins4 <- diff(range(werte2)) / bin_width

n <- length(werte5)
iqr <- IQR(werte5)
bin_width <- 2 * iqr / (n^(1/3))
num_bins5 <- diff(range(werte2)) / bin_width

n <- length(werte6)
iqr <- IQR(werte6)
bin_width <- 2 * iqr / (n^(1/3))
num_bins6 <- diff(range(werte2)) / bin_width




# Verteilung der Werte plotten
percentile_95 <- quantile(werte, 0.95)

# Erstellen des Diagramms mit Markierung des 95%-Quartils
ggplot(data.frame(Values = werte), aes(x = Values)) +
  geom_histogram(bins = 30, fill = "#87CEEB", alpha = 1, color = "black") +
  geom_vline(xintercept = percentile_95, linetype = "dashed", color = "red") +
  ggtitle("2019") +
  xlab(expression(paste(sigma[b], " [Mm"^-1*"]"))) +
  ylab("Count") +
  theme(panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major = element_line(color = "lightgray"),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        plot.title = element_text(face = "bold", hjust = 0.5)) +
  scale_x_continuous(breaks = seq(0, 50, by = 2)) # Fetter Titel

ggplot(data.frame(Values = werte2), aes(x = Values)) +
  geom_histogram(bins = 77, fill = "#87CEEB", alpha = 1, color = "black") +
  ggtitle("2020") +
  xlab(expression(paste(sigma[b], " [Mm"^-1*"]"))) +
  ylab("Count") +
  theme(panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major = element_line(color = "lightgray"),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        plot.title = element_text(face = "bold", hjust = 0.5)) +
  scale_x_continuous(breaks = seq(0, 50, by = 2))

ggplot(data.frame(Values = werte3), aes(x = Values)) +
  geom_histogram(bins = 62, fill = "#87CEEB", alpha = 1, color = "black") +
  ggtitle("2021") +
  xlab(expression(paste(sigma[b], " [Mm"^-1*"]"))) +
  ylab("Count") +
  theme(panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major = element_line(color = "lightgray"),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        plot.title = element_text(face = "bold", hjust = 0.5))  +
  scale_x_continuous(breaks = seq(0, 50, by = 2))# Fetter Titel

ggplot(data.frame(Values = werte4), aes(x = Values)) +
  geom_histogram(bins = 63, fill = "#87CEEB", alpha = 1, color = "black") +
  ggtitle("2022") +
  xlab(expression(paste(sigma[b], " [Mm"^-1*"]"))) +
  ylab("Count") +
  theme(panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major = element_line(color = "lightgray"),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        plot.title = element_text(face = "bold", hjust = 0.5))  +
  scale_x_continuous(breaks = seq(0, 300, by = 20))# Fetter Titel

ggplot(data.frame(Values = werte5), aes(x = Values)) +
  geom_histogram(bins = 64, fill = "#87CEEB", alpha = 1, color = "black") +
  ggtitle("2023") +
  xlab(expression(paste(sigma[b], " [Mm"^-1*"]"))) +
  ylab("Count") +
  theme(panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major = element_line(color = "lightgray"),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        plot.title = element_text(face = "bold", hjust = 0.5)) + # Fetter Titel
  scale_x_continuous(breaks = seq(0, 30, by = 2))# Fetter Titel

ggplot(data.frame(Values = werte6), aes(x = Values)) +
  geom_histogram(bins = 61, fill = "#87CEEB", alpha = 1, color = "black") +
  ggtitle("2024") +
  xlab(expression(paste(sigma[b], " [Mm"^-1*"]"))) +
  ylab("Count") +
  theme(panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major = element_line(color = "lightgray"),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        plot.title = element_text(face = "bold", hjust = 0.5)) +
  scale_x_continuous(breaks = seq(0, 20, by = 2))# Fetter Titel


################################################################################

corrnephdata$BsG0_S11 <- as.numeric(corrnephdata$BsG0_S11)

time <- as.POSIXct(corrnephdata$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

# Extrahiere das Jahr und filtere die Daten für 2019
year <- format(time, "%Y")
data_2019 <- corrnephdata[year == "2019", ]
data_2020 <- corrnephdata[year == "2020", ]
data_2021 <- corrnephdata[year == "2021", ]
data_2022 <- corrnephdata[year == "2022", ]
data_2023 <- corrnephdata[year == "2023", ]
data_2024 <- corrnephdata[year == "2024", ]

library(ggplot2)
library(dplyr)

werte <- data_2019$BsG0_S11
werte <- na.omit(data_2019$BsG0_S11)

werte2 <- data_2020$BsG0_S11
werte2 <- na.omit(data_2020$BsG0_S11)

werte3 <- data_2020$BsG0_S11
werte3 <- na.omit(data_2021$BsG0_S11)

werte4 <- data_2020$BsG0_S11
werte4 <- na.omit(data_2022$BsG0_S11)

werte5 <- data_2020$BsG0_S11
werte5 <- na.omit(data_2023$BsG0_S11)

werte6 <- data_2020$BsG0_S11
werte6 <- na.omit(data_2024$BsG0_S11)

n <- length(werte)
iqr <- IQR(werte)
bin_width <- 2 * iqr / (n^(1/3))
num_bins <- diff(range(werte)) / bin_width

n <- length(werte2)
iqr <- IQR(werte2)
bin_width <- 2 * iqr / (n^(1/3))
num_bins2 <- diff(range(werte2)) / bin_width

n <- length(werte3)
iqr <- IQR(werte3)
bin_width <- 2 * iqr / (n^(1/3))
num_bins3 <- diff(range(werte2)) / bin_width

n <- length(werte4)
iqr <- IQR(werte4)
bin_width <- 2 * iqr / (n^(1/3))
num_bins4 <- diff(range(werte2)) / bin_width

n <- length(werte5)
iqr <- IQR(werte5)
bin_width <- 2 * iqr / (n^(1/3))
num_bins5 <- diff(range(werte2)) / bin_width

n <- length(werte6)
iqr <- IQR(werte6)
bin_width <- 2 * iqr / (n^(1/3))
num_bins6 <- diff(range(werte2)) / bin_width




# Verteilung der Werte plotten
percentile_95 <- quantile(werte, 0.95)

# Erstellen des Diagramms mit Markierung des 95%-Quartils
ggplot(data.frame(Values = werte), aes(x = Values)) +
  geom_histogram(bins = 30, fill = "#87CEEB", alpha = 1, color = "black") +
  geom_vline(xintercept = percentile_95, linetype = "dashed", color = "red") +
  ggtitle("2019") +
  xlab(expression(paste(sigma[s], " [Mm"^-1*"]"))) +
  ylab("Count") +
  theme(panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major = element_line(color = "lightgray"),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        plot.title = element_text(face = "bold", hjust = 0.5)) +
  scale_x_continuous(breaks = seq(0, 350, by = 20)) # Fetter Titel

ggplot(data.frame(Values = werte2), aes(x = Values)) +
  geom_histogram(bins = 77, fill = "#87CEEB", alpha = 1, color = "black") +
  ggtitle("2020") +
  xlab(expression(paste(sigma[s], " [Mm"^-1*"]"))) +
  ylab("Count") +
  theme(panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major = element_line(color = "lightgray"),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        plot.title = element_text(face = "bold", hjust = 0.5)) +
  scale_x_continuous(breaks = seq(0, 500, by = 20))

ggplot(data.frame(Values = werte3), aes(x = Values)) +
  geom_histogram(bins = 62, fill = "#87CEEB", alpha = 1, color = "black") +
  ggtitle("2021") +
  xlab(expression(paste(sigma[s], " [Mm"^-1*"]"))) +
  ylab("Count") +
  theme(panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major = element_line(color = "lightgray"),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        plot.title = element_text(face = "bold", hjust = 0.5))  +
  scale_x_continuous(breaks = seq(0, 500, by = 20))# Fetter Titel

percentile_95 <- quantile(werte4, 0.95)

# Erstellen des Diagramms mit Markierung des 95%-Quartils
ggplot(data.frame(Values = werte4), aes(x = Values)) +
  geom_histogram(bins = 63, fill = "#87CEEB", alpha = 1, color = "black") +
  geom_vline(xintercept = percentile_95, linetype = "dashed", color = "red") +
  ggtitle("2022") +
  xlab(expression(paste(sigma[s], " [Mm"^-1*"]"))) +
  ylab("Count") +
  theme(panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major = element_line(color = "lightgray"),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        plot.title = element_text(face = "bold", hjust = 0.5)) +
  scale_x_continuous(breaks = seq(0, 2000, by = 150)) # Fetter Titel

ggplot(data.frame(Values = werte5), aes(x = Values)) +
  geom_histogram(bins = 64, fill = "#87CEEB", alpha = 1, color = "black") +
  ggtitle("2023") +
  xlab(expression(paste(sigma[s], " [Mm"^-1*"]"))) +
  ylab("Count") +
  theme(panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major = element_line(color = "lightgray"),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        plot.title = element_text(face = "bold", hjust = 0.5)) + # Fetter Titel
  scale_x_continuous(breaks = seq(0, 500, by = 20))# Fetter Titel

ggplot(data.frame(Values = werte6), aes(x = Values)) +
  geom_histogram(bins = 61, fill = "#87CEEB", alpha = 1, color = "black") +
  ggtitle("2024") +
  xlab(expression(paste(sigma[s], " [Mm"^-1*"]"))) +
  ylab("Count") +
  theme(panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major = element_line(color = "lightgray"),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        plot.title = element_text(face = "bold", hjust = 0.5)) +
  scale_x_continuous(breaks = seq(0, 500, by = 10))# Fetter Titel



###################################
#SSAdry 2019
###################################

library(tidyr)
library(dplyr)
library(ggplot2)

acorrected <- "/Users/baumannmike/Desktop/Datenbeispiele/Datenbeispiele/2019_03_01_5y_MKN_AE31_1hr_cleaned0523.csv"
corrae31data <- read.csv(acorrected)


ncorrected <- "/Users/baumannmike/Desktop/Datenbeispiele/Datenbeispiele/2019_03_01_5y_MKN_neph_1hr_cleaned0523.csv"
corrnephdata <- read.csv(ncorrected)

time <- as.POSIXct(corrae31data$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
year <- format(time, "%Y")

time2 <- as.POSIXct(corrnephdata$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
year <- format(time2, "%Y")

data_2019 <- corrae31data[year == "2019", ]

data_2019x <- corrnephdata[year == "2019", ]

data_2019$DateTimeUTC <- as.POSIXct(data_2019$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
data_2019x$DateTimeUTC <- as.POSIXct(data_2019x$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

data_2019 <- data_2019 %>%
  filter(format(DateTimeUTC, "%m") %in% c("06", "07"))

data_2019x <- data_2019x %>%
  filter(format(DateTimeUTC, "%m") %in% c("06", "07"))

merged_data <- merge(data_2019, data_2019x, by = "DateTimeUTC", all.x = TRUE)

merged_data$Ba10_A11 <- as.numeric(merged_data$Ba10_A11)
merged_data$Ba20_A11 <- as.numeric(merged_data$Ba20_A11)
merged_data$Ba30_A11 <- as.numeric(merged_data$Ba30_A11)
merged_data$Ba40_A11 <- as.numeric(merged_data$Ba40_A11)
merged_data$Ba50_A11 <- as.numeric(merged_data$Ba50_A11)
merged_data$Ba60_A11 <- as.numeric(merged_data$Ba60_A11)
merged_data$Ba70_A11 <- as.numeric(merged_data$Ba70_A11)

merged_data$BsB0_S11<- as.numeric(merged_data$BsB0_S11)
merged_data$BsG0_S11<- as.numeric(merged_data$BsG0_S11)
merged_data$BsR0_S11<- as.numeric(merged_data$BsR0_S11)



# Berechnung von AAE ohne NA-Werte

AAE1 <- -log(merged_data$Ba20_A11/merged_data$Ba60_A11)/log(470/880)

Abs_450 <- merged_data$Ba10_A11*(450/470)^(-1 * AAE1)
Abs_525 <- merged_data$Ba40_A11*(525/590)^(-1 * AAE1)
Abs_635 <- merged_data$Ba50_A11*(635/660)^(-1 * AAE1)


bextb <- Abs_450 + merged_data$BsB0_S11
bextg <- Abs_525 + merged_data$BsG0_S11
bextr <- Abs_635 + merged_data$BsR0_S11

SSAdryb <- merged_data$BsB0_S11/bextb
SSAdryg <- merged_data$BsG0_S11/bextg
SSAdryr <- merged_data$BsR0_S11/bextr

# Zuerst füge eine Spalte für die Stunde hinzu



# Plot erstellen mit angepasster y-Achse
plot(merged_data$DateTimeUTC , SSAdryb, col = "blue", type = "l", xlab = "", ylab = "SSA", main = "SSA (June - July 2019)", ylim = c(0.85,1))
lines(merged_data$DateTimeUTC , SSAdryg, col = "green")
lines(merged_data$DateTimeUTC , SSAdryr, col = "red")


# x-Achse beschriften

grid(col = "black")

legend("topright", legend = c("450 nm", "525 nm", "635 nm"), 
       col = c("red", "green", "blue"), lty = 1)


##################
#SAE 2019

library(tidyr)
library(dplyr)
library(ggplot2)


library(dplyr)

# Beispiel-Daten erstellen (falls nicht vorhanden)
#corrnephdata <- data.frame(
#  DateTimeUTC = seq(as.POSIXct("2019-06-01 00:00:00", tz = "UTC"), 
#                    as.POSIXct("2019-07-31 23:59:59", tz = "UTC"), by = "hour"),
#  BsB0_S11 = runif(1464, 0.1, 1),
#  BsG0_S11 = runif(1464, 0.1, 1),
#  BsR0_S11 = runif(1464, 0.1, 1)
#)

# Zeitzoneneinstellungen und Filtern nach 2019
time <- as.POSIXct(corrnephdata$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
year <- format(time, "%Y")

data_2019 <- corrnephdata[year == "2019", ]

data_2019$DateTimeUTC <- as.POSIXct(data_2019$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

# Filtern nach Juni und Juli
data_june_july <- data_2019 %>%
  filter(format(DateTimeUTC, "%m") %in% c("06", "07"))

data_june_july$BsB0_S11 <- as.numeric(data_june_july$BsB0_S11)
data_june_july$BsG0_S11 <- as.numeric(data_june_july$BsG0_S11)
data_june_july$BsR0_S11 <- as.numeric(data_june_july$BsR0_S11)

# Berechnung von AAE ohne NA-Werte
AAE1 <- -log(data_june_july$BsB0_S11/data_june_july$BsR0_S11)/log(450/635)

# Plot erstellen
plot(data_june_july$DateTimeUTC, AAE1, type = "l", col = "black", 
     xlab = "date", ylab = "SAE", xaxt = "n")

# Achsenbeschriftungen hinzufügen
axis.POSIXct(1, at = seq(min(data_june_july$DateTimeUTC), max(data_june_july$DateTimeUTC), by = "weeks"), 
             format = "%m-%d", las = 2)

title("SAE [450-635 nm] (2019)")

#


########################
##backscatter fraction

time <- as.POSIXct(corrnephdata$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
year <- format(time, "%Y")

data_2019 <- corrnephdata[year == "2019", ]

data_2019$DateTimeUTC <- as.POSIXct(data_2019$DateTimeUTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

data_june_july <- data_2019 %>%
  filter(format(DateTimeUTC, "%m") %in% c("06", "07"))


#Total Scattering
data_june_july$BsB0_S11 <- as.numeric(data_june_july$BsB0_S11)
data_june_july$BsG0_S11 <- as.numeric(data_june_july$BsG0_S11)
data_june_july$BsR0_S11 <- as.numeric(data_june_july$BsR0_S11)

#Backscattering
data_june_july$BbsB0_S11 <- as.numeric(data_june_july$BbsB0_S11)
data_june_july$BbsG0_S11 <- as.numeric(data_june_july$BbsG0_S11)
data_june_july$BbsR0_S11 <- as.numeric(data_june_july$BbsR0_S11)


blue <- data_june_july$BbsB0_S11/data_june_july$BsB0_S11
green <- data_june_july$BbsG0_S11/data_june_july$BsG0_S11
red <- data_june_july$BbsR0_S11/data_june_july$BsR0_S11

plot(data_june_july$DateTimeUTC, green, type = "l", col = "green", 
     xlab = "", ylab = "b", ylim = c(0, 0.5))
lines(data_june_july$DateTimeUTC, blue, type = "l", col = "blue")
lines(data_june_july$DateTimeUTC, red, type = "l", col = "red")


# Legende hinzufügen
legend("topleft", legend = c("450 nm", "525 nm", "635 nm"), 
       col = c("red", "green", "blue"), lty = 1)
title("Backscatter fraction ")


#########################################
#Boxplot ganze grössen

acorrected <- "/Users/baumannmike/Desktop/Datenbeispiele/Datenbeispiele/2019_03_01_5y_MKN_AE31_1hr_cleaned0523.csv"
corrae31data <- read.csv(acorrected)


ncorrected <- "/Users/baumannmike/Desktop/Datenbeispiele/Datenbeispiele/2019_03_01_5y_MKN_neph_1hr_cleaned0523.csv"
corrnephdata <- read.csv(ncorrected)

library(ggplot2)
library(gridExtra)

corrae31data$Ba30_A11 <- as.numeric(corrae31data$Ba30_A11)
corrnephdata$BsG0_S11 <- as.numeric(corrnephdata$BsG0_S11)

x <- corrae31data$Ba30_A11
y <- corrnephdata$BsG0_S11

# Beispiel Daten
# 2.28

# Filtere alle negativen Werte
filtered_x <- x[x > 0]

filtered_x <- x[x > 0 & !is.na(x)]

filtered_y <- y[y > 0]

filtered_y <- y[y > 0 & !is.na(y)]

par(mfrow = c(2, 1))

boxplot(x, main = "April 2019 - February 2024 [521 nm]", xlab = expression(paste(sigma[a], " [Mm"^-1*"]")), horizontal = TRUE, outline = FALSE)

boxplot(y, main = "April 2019 - February 2024 [525 nm]", xlab = expression(paste(sigma[s], " [Mm"^-1*"]")), horizontal = TRUE, outline = FALSE)

par(mfrow = c(1, 1))


boxplot(filtered_x, 
        main = "absorption", 
        xlab = expression(paste(sigma[a], " [Mm"^"-1", "]")),
        horizontal = TRUE, 
        outline = FALSE,
        log = "x",  # Setzt die x-Achse auf logarithmisch (Basis 10)
        axes = FALSE  # Deaktiviert automatische Achsenbeschriftungen
)
box()
# Manuell Achsenbeschriftungen setzen
axis(1, at = 10^seq(floor(log10(min(filtered_x))), ceiling(log10(max(filtered_x))), by = 1), 
     labels = 10^seq(floor(log10(min(filtered_x))), ceiling(log10(max(filtered_x))), by = 1))


# Zeigt den Boxplot mit den manuell gesetzten Achsenbeschriftungen


boxplot(filtered_y, 
        main = "Scattering", 
        xlab = expression(paste(sigma[s], " [Mm"^"-1", "]")),
        horizontal = TRUE, 
        outline = FALSE,
        log = "y",  # Setzt die x-Achse auf logarithmisch (Basis 10)
        axes = FALSE  # Deaktiviert automatische Achsenbeschriftungen
)
box()
axis(1, at = 10^seq(floor(log10(min(filtered_y))), ceiling(log10(max(filtered_y))), by = 1), 
     labels = 10^seq(floor(log10(min(filtered_y))), ceiling(log10(max(filtered_y))), by = 1))


boxplot_stats <- boxplot.stats(filtered_y)

# Minimum, unteres Quartil, Median, oberes Quartil und Maximum extrahieren
minimum <- boxplot_stats$stats[1]
unteres_quartil <- boxplot_stats$stats[2]
median <- boxplot_stats$stats[3]
oberes_quartil <- boxplot_stats$stats[4]
maximum <- boxplot_stats$stats[5]

# Ausreißer extrahieren
ausreisser <- boxplot_stats$out

# Ergebnisse anzeigen
cat("Minimum:", minimum, "\n")
cat("Unteres Quartil:", unteres_quartil, "\n")
cat("Median:", median, "\n")
cat("Oberes Quartil:", oberes_quartil, "\n")
cat("Maximum:", maximum, "\n")
cat("Ausreißer:", ausreisser, "\n")


################3
###SSA

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


SSAdryg <- merged_data$BsG0_S11/bextg

filtered_SSA <- SSAdryg[SSAdryg > 0]

filtered_SSA <- SSAdryg[SSAdryg > 0 & !is.na(SSAdryg)]


boxplot(filtered_SSA, 
        main = "SSA", 
        xlab = expression(paste(sigma[a], " [Mm"^"-1", "]")),
        horizontal = TRUE, 
        outline = FALSE,
        log = "x"  # Setzt die x-Achse auf logarithmisch (Basis 10)  # Deaktiviert automatische Achsenbeschriftungen
)
box()
# Manuell Achsenbeschriftungen setzen



Q1 <- quantile(filtered_x, 0.25)
Q3 <- quantile(filtered_x, 0.75)
IQR <- Q3 - Q1

# Berechnung des unteren Whiskers
lower_whisker <- max(min(filtered_x), Q1 - 1.5 * IQR)



