
#####VISUALIZATIONS SENTIMENT ANALYSIS#####
-------------------------------------------------------------------
  #Graph combining all positive sentiments 
library(ggplot2)
library(dplyr)
library(lubridate)

#Upload Data:
library(readr)
Green <- read_csv("/Users/emma/Desktop/Green_done.csv")

AFD <- read_csv("/Users/emma/Desktop/AFD_done.csv") 

CDU <- read_csv("/Users/emma/Desktop/CDU_done.csv")

SPD <- read_csv("/Users/emma/Desktop/SPD_done.csv")

FDP <- read_csv("/Users/emma/Desktop/FDP_done.csv") 

#Delete all missing rows
Green <- na.omit(Green)
AFD <- na.omit(AFD)
CDU <- na.omit(CDU)
SPD <- na.omit(SPD)
FDP <- na.omit(FDP)

library(ggplot2)
library(scales)  # For percentage scales

# Calculate the percentage distribution of sentiments, Green Party
Green$sentiment <- factor(Green$sentiment, levels = c(0, 1, 2))
sentiment_counts_G <- as.data.frame(table(Green$sentiment))
sentiment_counts_G$percentage <- (sentiment_counts_G$Freq / sum(sentiment_counts$Freq)) * 100
View(sentiment_counts_G)

# Calculate the percentage distribution of sentiments, AfD Party
AFD$sentiment <- factor(AFD$sentiment, levels = c(0, 1, 2))
sentiment_counts_AFD <- as.data.frame(table(AFD$sentiment))
sentiment_counts_AFD$percentage <- (sentiment_counts_AFD$Freq / sum(sentiment_counts_AFD$Freq)) * 100
View(sentiment_counts_AFD)

# Calculate the percentage distribution of sentiments, SPD Party
SPD$sentiment <- factor(SPD$sentiment, levels = c(0, 1, 2))
sentiment_counts_SPD <- as.data.frame(table(SPD$sentiment))
sentiment_counts_SPD$percentage <- (sentiment_counts_SPD$Freq / sum(sentiment_counts_SPD$Freq)) * 100
View(sentiment_counts_SPD)

# Calculate the percentage distribution of sentiments, FDP Party
FDP$sentiment <- factor(FDP$sentiment, levels = c(0, 1, 2))
sentiment_counts_FDP <- as.data.frame(table(FDP$sentiment))
sentiment_counts_FDP$percentage <- (sentiment_counts_FDP$Freq / sum(sentiment_counts_FDP$Freq)) * 100
View(sentiment_counts_FDP)

# Calculate the percentage distribution of sentiments, CDU Party
CDU$sentiment <- factor(CDU$sentiment, levels = c(0, 1, 2))
sentiment_counts_CDU <- as.data.frame(table(CDU$sentiment))
sentiment_counts_CDU$percentage <- (sentiment_counts_CDU$Freq / sum(sentiment_counts_CDU$Freq)) * 100
View(sentiment_counts_CDU)
---------------------------------------------------------------------------------------------------------------
# Plot with ggplot2, Greens:

G_plot <- ggplot(Green, aes(x = sentiment)) +
  geom_bar(aes(y = ..count../sum(..count..)), fill = "#6B8E23", width = 0.5) +
  geom_text(data = sentiment_counts_G, aes(x = Var1, y = Freq / sum(Freq), label = sprintf("%.1f%%", percentage)), 
            vjust = -0.5, size = 4, color = "black") +
  labs(x = "Sentiment", 
       y = "Percentage", 
       title = "Bündnis 90/Die Grünen") +
  scale_y_continuous(labels = percent_format()) +
  scale_x_discrete(labels = c("Neutral", "Positive", "Negative")) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black")
  )
)

print(G_plot)

# Plot with ggplot2, AfD:

AFD_plot <- ggplot(AFD, aes(x = sentiment)) +
  geom_bar(aes(y = ..count../sum(..count..)), fill = "#1f78b4", width = 0.5) +
  geom_text(data = sentiment_counts_AFD, aes(x = Var1, y = Freq / sum(Freq), label = sprintf("%.1f%%", percentage)), 
            vjust = -0.5, size = 4, color = "black") +
  labs(x = "Sentiment", 
       y = "Percentage", 
       title = "AFD") +
  scale_y_continuous(labels = percent_format()) +
  scale_x_discrete(labels = c("Neutral", "Positive", "Negative")) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black")
  )
)

print(AFD_plot)

# Plot with ggplot2, SPD:
SPD_plot <- ggplot(SPD, aes(x = sentiment)) +
  geom_bar(aes(y = ..count../sum(..count..)), fill = "#FF0000", width = 0.5) +
  geom_text(data = sentiment_counts_SPD, aes(x = Var1, y = Freq / sum(Freq), label = sprintf("%.1f%%", percentage)), 
            vjust = -0.5, size = 4, color = "black") +
  labs(x = "Sentiment", 
       y = "Percentage", 
       title = "SPD") +
  scale_y_continuous(labels = percent_format()) +
  scale_x_discrete(labels = c("Neutral", "Positive", "Negative")) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black")
  )
)

print(SPD_plot)

# Plot with ggplot2, FDP:
FDP_plot <- ggplot(FDP, aes(x = sentiment)) +
  geom_bar(aes(y = ..count../sum(..count..)), fill = "#FFA500", width = 0.5) +
  geom_text(data = sentiment_counts_FDP, aes(x = Var1, y = Freq / sum(Freq), label = sprintf("%.1f%%", percentage)), 
            vjust = -0.5, size = 4, color = "black") +
  labs(x = "Sentiment", 
       y = "Percentage", 
       title = "FDP") +
  scale_y_continuous(labels = percent_format()) +
  scale_x_discrete(labels = c("Neutral", "Positive", "Negative")) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black")
  )
)

print(FDP_plot)

# Plot with ggplot2, CDU:
CDU_plot <- ggplot(CDU, aes(x = sentiment)) +
  geom_bar(aes(y = ..count../sum(..count..)), fill = "#FFFF00",, width = 0.5) +
  geom_text(data = sentiment_counts_CDU, aes(x = Var1, y = Freq / sum(Freq), label = sprintf("%.1f%%", percentage)), 
            vjust = -0.5, size = 4, color = "black") +
  labs(x = "Sentiment", 
       y = "Percentage", 
       title = "CDU") +
  scale_y_continuous(labels = percent_format()) +
  scale_x_discrete(labels = c("Neutral", "Positive", "Negative")) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black")
  )
)

print(CDU_plot)

-----------------------------
  
library(ggplot2)
library(patchwork)


# Combine plots with specified layout
combined_plot <- ((G_plot + SPD_plot) /
                    (AFD_plot + FDP_plot)) +



# Display the combined plot
print(combined_plot)
