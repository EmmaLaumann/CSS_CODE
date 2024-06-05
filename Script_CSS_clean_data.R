
### AFD DATASET:
#Load Data:
library(readr)
AFD_comments <- read_csv("Desktop/AFD_comments.csv")
View(AFD_comments) #N = 21954

#Delete Cases AfD that are not relevant:
# Remove rows where 'text' column contains NA
AFD_comments_clean <- na.omit(AFD_comments)
View(AFD_comments_clean) #N = 11917

##SAVE THE DATA that is cleaned:
# Define the file path for the CSV file
file_path <- "/Users/emma/Desktop/cleaned_data.csv"

# Export the cleaned data frame to a CSV file
write.csv(AFD_comments_clean, file = file_path, row.names = FALSE)

# Print confirmation message
print(paste("Data frame exported to", file_path))

####CONDUCT SENTIMENT ANALYSIS####


### Grüne DATASET:
#Load Data:
library(readr)
G_comments <- read_csv("Desktop/Gruene_comments.csv")
View(G_comments) #N = 6430

#Delete Cases AfD that are not relevant:
# Remove rows where 'text' column contains NA
G_comments_clean <- na.omit(G_comments)
View(G_comments_clean) #N = 5578

##SAVE THE DATA that is cleaned:
# Define the file path for the CSV file
file_path <- "/Users/emma/Desktop/G_cleaned_data.csv"

# Export the cleaned data frame to a CSV file
write.csv(G_comments_clean, file = file_path, row.names = FALSE)

# Print confirmation message
print(paste("Data frame exported to", file_path))

####CONDUCT SENTIMENT ANALYSIS####

