#Import data
setwd("C:/Users/17985/Desktop/intro dataset") # Set the working directory to the folder where the data file is located

football_data <- read.csv("results.csv", stringsAsFactors = FALSE) # Read the 'results.csv' file into R as a dataframe and prevent converting strings to factors

head(football_data) # Display the first 6 rows of the data to verify that the file was loaded correctly

str(football_data) # Display the structure of the dataframe, showing column names and data types

summary(football_data) # Provide a summary of the dataframe, such as the min, max, and average values for numeric columns

#Filter England matches
england_matches <- subset(football_data, home_team == "England" | away_team == "England") # Filter all matches where England is either the home team or the away team
head(england_matches) # Display the first few rows of the filtered data
dim(england_matches) # Check the dimensions of the filtered data (number of rows and columns)

#England's performance in different types of competitions (friendly matches, World Cup, European Championship, etc.)
england_matches$result <- ifelse(
  (england_matches$home_team == "England" & england_matches$home_score > england_matches$away_score) |
    (england_matches$away_team == "England" & england_matches$away_score > england_matches$home_score),
  "Win",
  ifelse(england_matches$home_score == england_matches$away_score, "Draw", "Loss")
) # Create a new column to store the match result: Win, Draw, or Loss

result_by_tournament <- table(england_matches$tournament, england_matches$result) # Group the data by tournament type and calculate the count of Win, Draw, Loss

print(result_by_tournament) # Print the result



# Check the game year
england_matches$year <- as.numeric(format(as.Date(england_matches$date), "%Y"))  # Convert date to year as numeric

time_distribution <- table(england_matches$year)  # Create a table summarizing match counts by year

time_distribution_df <- as.data.frame(time_distribution)  # Transform the table into a data frame
colnames(time_distribution_df) <- c("Year", "Match_Count")  # Rename columns for clarity

horizontal_table <- t(time_distribution_df$Match_Count)  # Transpose the Match_Count column
colnames(horizontal_table) <- time_distribution_df$Year  # Set year as column names

print(horizontal_table)  # Print the horizontal table to the console








library(dplyr)
library(tidyr)
library(ggplot2)# Load necessary libraries

setwd("C:/Users/17985/Desktop/intro dataset")  # Set the working directory
football_data <- read.csv("results.csv", stringsAsFactors = FALSE)  # Read data

england_matches <- subset(football_data, home_team == "England" | away_team == "England")  # Filter England matches

england_matches$result <- ifelse(
  (england_matches$home_team == "England" & england_matches$home_score > england_matches$away_score) |
    (england_matches$away_team == "England" & england_matches$away_score > england_matches$home_score),
  "Win",
  ifelse(england_matches$home_score == england_matches$away_score, "Draw", "Loss")
) # Create the result column (Win, Draw, Loss)

if (!"year" %in% names(england_matches)) {
  england_matches$year <- as.numeric(format(as.Date(england_matches$date), "%Y"))
} # Extract the year from the date column if it's not already a year

england_matches$time_group <- ifelse(england_matches$year >= 1872 & england_matches$year <= 1884, 
                                     "1872-1884",  # First 13 years grouped as 1872-1884
                                     paste(floor((england_matches$year - 1885) / 10) * 10 + 1885, 
                                           "-", 
                                           floor((england_matches$year - 1885) / 10) * 10 + 1894, sep = "")) # Create a new column for 10-year intervals

england_performance_by_time_group <- england_matches %>%
  group_by(time_group, result) %>%
  summarise(count = n(), .groups = 'drop') # Group by time_group and result, then count occurrences

england_performance_by_time_group <- england_performance_by_time_group %>%
  pivot_wider(names_from = result, values_from = count, values_fill = list(count = 0)) # Spread the results (Win, Draw, Loss) into separate columns

england_performance_by_time_group$total_matches <- england_performance_by_time_group$Win + england_performance_by_time_group$Draw + england_performance_by_time_group$Loss
# Calculate the total number of matches per time group

england_performance_by_time_group$Win_Percentage <- (england_performance_by_time_group$Win / england_performance_by_time_group$total_matches) * 100
england_performance_by_time_group$Draw_Percentage <- (england_performance_by_time_group$Draw / england_performance_by_time_group$total_matches) * 100
england_performance_by_time_group$Loss_Percentage <- (england_performance_by_time_group$Loss / england_performance_by_time_group$total_matches) * 100
ggplot(england_performance_by_time_group, aes(x = time_group)) + 
  geom_line(aes(y = Win_Percentage, color = "Win"), size = 1.2, group = 1) +
  geom_line(aes(y = Draw_Percentage, color = "Draw"), size = 1.2, group = 1) +
  geom_line(aes(y = Loss_Percentage, color = "Loss"), size = 1.2, group = 1) +
  labs(title = "England's Football Performance by Time Group (1872-2024)",
       x = "Time Period (Group)",
       y = "Percentage (%)",
       color = "Result") +
  theme_minimal() +
  scale_color_manual(values = c("Win" = "green", "Draw" = "yellow", "Loss" = "red")) +
  scale_y_continuous(breaks = seq(0, 100, by = 5)) +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Create a plot to show the percentage of Wins, Draws, and Losses by time group









library(dplyr)
library(ggplot2)# Load necessary libraries

setwd("C:/Users/17985/Desktop/intro dataset") # Set the working directory to the folder where the data file is located

football_data <- read.csv("results.csv", stringsAsFactors = FALSE) # Read the 'results.csv' file into R as a dataframe

target_teams <- c("France", "Germany", "Italy", "Spain") # Define the list of teams to filter against

england_vs_teams <- football_data %>%
  filter((home_team == "England" & away_team %in% target_teams) | 
           (away_team == "England" & home_team %in% target_teams)) 

competitive_tournaments <- c("British Home Championship", "FIFA World Cup", 
                             "FIFA World Cup qualification", "UEFA Euro", 
                             "UEFA Euro qualification", "UEFA Nations League", 
                             "King Hassan II Tournament", "Rous Cup", 
                             "Tournoi de France", "USA Cup")  # Define match type: Competitive for certain tournaments, otherwise Friendly

england_vs_teams <- england_vs_teams %>%
  mutate(match_type = ifelse(tournament %in% competitive_tournaments, 
                             "Competitive", "Friendly"),
         result = case_when(
           (home_team == "England" & home_score > away_score) | 
             (away_team == "England" & away_score > home_score) ~ "Win",
           home_score == away_score ~ "Draw",
           TRUE ~ "Loss"
         ))  # Assign match type: Competitive for tournaments except "Friendly"

performance_by_type <- england_vs_teams %>%
  group_by(match_type, result) %>%
  summarise(count = n(), .groups = 'drop')  # Group by match type and result, then count occurrences

performance_by_type <- performance_by_type %>%
  group_by(match_type) %>%
  mutate(total_matches = sum(count)) %>%
  ungroup() # Calculate the total number of matches per match type

performance_by_type$percentage <- (performance_by_type$count / performance_by_type$total_matches) * 100  # Calculate the percentage of each result type
performance_by_type$result <- factor(performance_by_type$result, levels = c("Win", "Draw", "Loss")) # Order 'result' factor to ensure 'Win' comes first, then 'Draw', then 'Loss'

ggplot(performance_by_type, aes(x = match_type, y = percentage, fill = result)) + 
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  labs(title = "England's Performance Against Germany, France, Italy, and Spain (Competitive vs Friendly)",
       x = "Match Type",
       y = "Percentage (%)",
       fill = "Result") +
  scale_fill_manual(values = c("Win" = "green", "Draw" = "yellow", "Loss" = "red")) +
  theme_minimal() +
  scale_y_continuous(breaks = seq(0, 100, by = 5)) +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Create a plot to show the percentage of Wins, Draws, and Losses by match type

















result_by_tournament <- result_by_tournament[c("British Home Championship", "FIFA World Cup", 
                                               "FIFA World Cup qualification", "Friendly", 
                                               "UEFA Euro", "UEFA Euro qualification"), ] # Remove unwanted tournaments (e.g., King Hassan II Tournament, Rous Cup, etc.)

print(result_by_tournament) # Print the new result

tournaments <- c("British Home Championship", "FIFA World Cup", "FIFA World Cup qualification", 
                 "Friendly", "UEFA Euro", "UEFA Euro qualification")  # Prepare the data (for simplicity, we'll manually set the counts for each tournament)

counts_matrix <- matrix(c(155, 56, 49,   # British Home Championship
                          32, 22, 20,   # FIFA World Cup
                          84, 27, 11,   # FIFA World Cup qualification
                          234, 99, 92,  # Friendly
                          18, 16, 11,   # UEFA Euro
                          75, 25, 10),  # UEFA Euro qualification
                        nrow = 6, byrow = TRUE)  # Create a matrix with the counts for each tournament (Win, Draw, Loss)

par(mfrow = c(2, 3))  # Set up the plotting area to have 2 rows and 3 columns for the pie charts

for (i in 1:length(tournaments)) {
  # Create the pie chart for each tournament
  result_counts <- counts_matrix[i, ]  # Get the counts for the current tournament
  result_labels <- c("Win", "Draw", "Loss")  # Loop through each tournament to create the pie charts
  
  pie(result_counts, 
      labels = paste(result_labels, round(result_counts / sum(result_counts) * 100, 1), "%"),
      col = c("green", "yellow", "red"), 
      main = tournaments[i], 
      radius = 1, 
      cex = 1.2)  # Create the pie chart
}

par(mfrow = c(1, 1))  # Reset to default layout





