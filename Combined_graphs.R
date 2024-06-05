#Graph combining all positive sentiments 
library(ggplot2)
library(dplyr)
library(lubridate)

#Upload Data:
library(readr)
Green <- read_csv("/Users/emma/Desktop/Green_done.csv")

Green <- na.omit(Green)
AFD <- read_csv("/Users/emma/Desktop/AFD_done.csv") 

install.packages("patchwork")
library(patchwork)

#Sentiment Counts:
# Calculate the percentage distribution of sentiments
AFD$sentiment <- factor(AFD$sentiment, levels = c(0, 1, 2))
sentiment_counts <- as.data.frame(table(AFD$sentiment))
sentiment_counts$percentage <- (sentiment_counts$Freq / sum(sentiment_counts$Freq)) * 100

# Calculate the percentage distribution of sentiments
Green$sentiment <- factor(Green$sentiment, levels = c(0, 1, 2))
sentiment_counts <- as.data.frame(table(Green$sentiment))
sentiment_counts$percentage <- (sentiment_counts$Freq / sum(sentiment_counts$Freq)) * 100

# Plot for Alternative für Deutschland (AfD)
plot_afd <- ggplot(AFD, aes(x = sentiment)) +
  geom_bar(aes(y = ..count../sum(..count..)), fill = "#1f78b4", width = 0.5) +
  geom_text(data = sentiment_counts, aes(x = Var1, y = Freq / sum(Freq), label = sprintf("%.1f%%", percentage)), 
            vjust = -0.5, size = 4, color = "black") +
  labs(x = "Sentiment", 
       y = "Percentage", 
       title = "Alternative für Deutschland (AfD)") +
  scale_y_continuous(labels = percent_format()) +
  scale_x_discrete(labels = c("Neutral", "Positive", "Negative")) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black")
  )

# Plot for Bündnis 90/Die Grünen
plot_green <- ggplot(Green, aes(x = sentiment)) +
  geom_bar(aes(y = ..count../sum(..count..)), fill = "#6B8E23", width = 0.5) +
  geom_text(data = sentiment_counts, aes(x = Var1, y = Freq / sum(Freq), label = sprintf("%.1f%%", percentage)), 
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

# Combine plots
combined_plot <- plot_afd + plot_green + plot_layout(ncol = 2)

# Display the combined plot
combined_plot

-------------------------------------------------------------------
install.packages("extrafont")  
  # Load the extrafont library
  library(extrafont)

# Load the Times New Roman font
font_import(pattern = "times new roman")

# Set the font family to Times New Roman
theme_set(theme_light(base_family = "Times New Roman"))

# Adjusting the font size
font_size <- 12

# Plot for Alternative für Deutschland (AfD)
plot_afd <- ggplot(AFD, aes(x = sentiment)) +
  geom_bar(aes(y = ..count../sum(..count..)), fill = "#1f78b4", width = 0.5) +
  geom_text(data = sentiment_counts, aes(x = Var1, y = Freq / sum(Freq), label = sprintf("%.1f%%", percentage)), 
            vjust = -0.5, size = font_size * 0.8, color = "black", fontface = "bold") +
  labs(x = "Sentiment", 
       y = "Percentage", 
       title = "Alternative für Deutschland (AfD)") +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  scale_x_discrete(labels = c("Neutral", "Positive", "Negative")) +
  theme_minimal(base_size = font_size) +
  theme(
    plot.title = element_text(hjust = 0.5, size = font_size * 1.2, face = "bold"),
    axis.text = element_text(color = "black", size = font_size),
    axis.title = element_text(color = "black", size = font_size)
  )

# Plot for Bündnis 90/Die Grünen
plot_green <- ggplot(Green, aes(x = sentiment)) +
  geom_bar(aes(y = ..count../sum(..count..)), fill = "#6B8E23", width = 0.5) +
  geom_text(data = sentiment_counts, aes(x = Var1, y = Freq / sum(Freq), label = sprintf("%.1f%%", percentage)), 
            vjust = -0.5, size = font_size * 0.8, color = "black", fontface = "bold") +
  labs(x = "Sentiment", 
       y = "Percentage", 
       title = "Bündnis 90/Die Grünen") +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  scale_x_discrete(labels = c("Neutral", "Positive", "Negative")) +
  theme_minimal(base_size = font_size) +
  theme(
    plot.title = element_text(hjust = 0.5, size = font_size * 1.2, face = "bold"),
    axis.text = element_text(color = "black", size = font_size),
    axis.title = element_text(color = "black", size = font_size)
  )

# Combine plots
combined_plot <- plot_afd + plot_green + plot_layout(ncol = 2)

# Display the combined plot
combined_plot

-----------------------------------------------------------------------
  # Adjusting the font size for percentage indications
  percentage_font_size <- 3

# Plot for Alternative für Deutschland (AfD)
plot_afd <- ggplot(AFD, aes(x = sentiment)) +
  geom_bar(aes(y = ..count../sum(..count..)), fill = "#1f78b4", width = 0.5) +
  geom_text(data = sentiment_counts, aes(x = Var1, y = Freq / sum(Freq), label = sprintf("%.1f%%", percentage)), 
            vjust = -0.5, size = percentage_font_size, color = "black", fontface = "bold") +
  labs(x = "Sentiment", 
       y = "Percentage", 
       title = "Alternative für Deutschland (AfD)") +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  scale_x_discrete(labels = c("Neutral", "Positive", "Negative")) +
  theme_minimal(base_size = font_size) +
  theme(
    plot.title = element_text(hjust = 0.5, size = font_size * 1.2, face = "bold"),
    axis.text = element_text(color = "black", size = font_size),
    axis.title = element_text(color = "black", size = font_size)
  )

# Plot for Bündnis 90/Die Grünen
plot_green <- ggplot(Green, aes(x = sentiment)) +
  geom_bar(aes(y = ..count../sum(..count..)), fill = "#6B8E23", width = 0.5) +
  geom_text(data = sentiment_counts, aes(x = Var1, y = Freq / sum(Freq), label = sprintf("%.1f%%", percentage)), 
            vjust = -0.5, size = percentage_font_size, color = "black", fontface = "bold") +
  labs(x = "Sentiment", 
       y = "Percentage", 
       title = "Bündnis 90/Die Grünen") +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  scale_x_discrete(labels = c("Neutral", "Positive", "Negative")) +
  theme_minimal(base_size = font_size) +
  theme(
    plot.title = element_text(hjust = 0.5, size = font_size * 1.2, face = "bold"),
    axis.text = element_text(color = "black", size = font_size),
    axis.title = element_text(color = "black", size = font_size)
  )

# Combine plots
combined_plot <- plot_afd + plot_green + plot_layout(ncol = 2)

# Display the combined plot
combined_plot
--------------------------------------------------------------------
  
  
  # Calculate sentiment counts and percentages for Alternative für Deutschland (AfD)
  AFD_counts <- AFD %>%
  count(sentiment) %>%
  mutate(percentage = n / sum(n) * 100)

# Calculate sentiment counts and percentages for Bündnis 90/Die Grünen
Green_counts <- Green %>%
  count(sentiment) %>%
  mutate(percentage = n / sum(n) * 100)

# Plot for Alternative für Deutschland (AfD)
plot_afd <- ggplot(AFD, aes(x = sentiment)) +
  geom_bar(aes(y = ..count../sum(..count..)), fill = "#1f78b4", width = 0.5) +
  geom_text(data = AFD_counts, aes(x = sentiment, y = n / sum(n), label = sprintf("%.1f%%", percentage)), 
            vjust = -0.5, size = percentage_font_size, color = "black", fontface = "bold") +
  labs(x = "Sentiment", 
       y = "Percentage", 
       title = "Alternative für Deutschland (AfD)") +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  scale_x_discrete(labels = c("Neutral", "Positive", "Negative")) +
  theme_minimal(base_size = font_size) +
  theme(
    plot.title = element_text(hjust = 0.5, size = font_size * 1.2, face = "bold"),
    axis.text = element_text(color = "black", size = font_size),
    axis.title = element_text(color = "black", size = font_size)
  )

# Plot for Bündnis 90/Die Grünen
plot_green <- ggplot(Green, aes(x = sentiment)) +
  geom_bar(aes(y = ..count../sum(..count..)), fill = "#6B8E23", width = 0.5) +
  geom_text(data = Green_counts, aes(x = sentiment, y = n / sum(n), label = sprintf("%.1f%%", percentage)), 
            vjust = -0.5, size = percentage_font_size, color = "black", fontface = "bold") +
  labs(x = "Sentiment", 
       y = "Percentage", 
       title = "Bündnis 90/Die Grünen") +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  scale_x_discrete(labels = c("Neutral", "Positive", "Negative")) +
  theme_minimal(base_size = font_size) +
  theme(
    plot.title = element_text(hjust = 0.5, size = font_size * 1.2, face = "bold"),
    axis.text = element_text(color = "black", size = font_size),
    axis.title = element_text(color = "black", size = font_size)
  )

# Combine plots
combined_plot <- plot_afd + plot_green + plot_layout(ncol = 2)

# Display the combined plot
combined_plot
