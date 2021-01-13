# Load necessary packages
library(tidyverse)
install.packages("corrplot", repos = "http://cran.us.r-project.org")
library(corrplot)
install.packages("reshape2", repos = "http://cran.us.r-project.org")
library(reshape2)

## Importing the data file used in the analysis ##
# Data files exploratory research
# Import the dataset 
music <- read_csv("data_w_genres-1.csv")
# Generate a tibble from the data frame
music_tbl <- as_tibble(music)

# Import csv files to portray background research to build on our hypothesis. 
revenue_split <- read_csv("Revenue_Split.csv")
music_properties <- read_csv("Music_Properties.csv")
monthly_active_users <- read_csv("Monthly_Active_Users.csv")
genre_popularity <- read_csv("Genre_Popularity_.csv")

## Nomenclature standards ##
# Create a data frame with only the top 5 genres
music_top_5 <- music %>%
  filter(Genre %in% c("HipHop/Rap", "Pop", "Electro", "Rock", "Country"))
# Create a data frame with the top 5 genres and the different attributes of music composition
new_music_top_5 <- music_top_5 %>% 
  select(acousticness, danceability, energy, instrumentalness, liveness, speechiness, valence, popularity) %>% 
  # Renaming the columns for visualization purposes
  rename(Acoust= acousticness, Dance = danceability, Ener = energy, Inst = instrumentalness, Liv = liveness, Spee = speechiness, Val = valence)

## Function ##
# Function for plotting 
func_visual <- function(data,x,y,Legend,chart_type) {
  plot<- ggplot(data,mapping=aes(x,y,fill = Legend))
  if (chart_type == "col"){
    plot +
      geom_col()
  }
  else if (chart_type == "line"){
    plot +
      geom_line()
  }
  else if (chart_type == "tile"){
    plot +
      geom_tile()
  }
}

### FRAMING THE PROBLEM ###
## Research Guide ##

# Evolution of genre trends over the years
genre_popularity %>% 
  group_by(Genre, Popularity) %>% 
  ggplot(., aes(Year, Popularity, color=Genre))+
  geom_point()+
  geom_line() +
  theme_classic()+
  ggtitle("Changes in Popularity of Genres over the Years")+
  theme(plot.title = element_text(hjust = 0.5))

# Change in music composition attributes over the years
music_properties2 <- music_properties %>% 
  # Reducing the number of columns and add rows to manage the visualization
  pivot_longer(c(Energy, Organicness, Mechanism, Bounciness, Acousticness, Loudness, Tempo), names_to = "stat", values_to = "values")
# Plotting the result
func_visual(music_properties2, music_properties2$Year, music_properties2$values, music_properties2$stat,"col")+
  theme_classic()+
  ggtitle("Changes in Music Composition Attributes over the Years")+
  xlab("Year")+
  ylab("Value")+
  theme(plot.title = element_text(hjust = 0.5))

# Changes in music revenues over the years
revenue2 <- revenue_split %>% 
  # Reducing the number of columns and add rows to manage the visualization
  pivot_longer(c(Syncronisation, Performance_Rights, Downloads, Total_Streaming, Total_Physical), names_to = "stat", values_to = "values")
# Plotting the result
func_visual(revenue2, revenue2$Year, revenue2$values, revenue2$stat,"col")+
  theme_classic()+
  ggtitle("Evolution of Music Revenues over the Years")+
  xlab("Year")+
  ylab("Music Revenue ($ Billion)")+
  theme(plot.title = element_text(hjust = 0.5))

# Spotify active users in million
func_visual(monthly_active_users, monthly_active_users$Year_Quarter, monthly_active_users$MAU_Millions, monthly_active_users$MAU_Millions,"col")+
  theme_classic()+
  ggtitle("Active Users on Spotify (in million)")+
  xlab("Quarter of Year")+
  ylab("Active Users (millions)")+
  theme(plot.title = element_text(hjust = 0.5))

## Testable Hypothesis ##

# Plotting relationship between Genre and popularity 
ggplot(music_top_5, aes(x = Genre, y = popularity, fill = Genre))+
  geom_col(position = "dodge")+
  theme_classic()+
  ggtitle("Popularity by Genre")+
  ylab("Popularity")+
  theme(plot.title = element_text(hjust = 0.5))

# Estimate the linear model
lm_genp <- lm(formula = popularity ~ Genre, data = music_top_5)
lm_genp

#Creation of correlation matrix between every music composition attributes
corr_matrix <- round(cor(new_music_top_5), 2)
# Building the correlation matrix heatmap
melted_corr <- melt(corr_matrix)
func_visual(melted_corr, melted_corr$Var1, melted_corr$Var2, melted_corr$value, "tile")+
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 2) +
  scale_fill_gradient2(low = "red", high = "green", mid = "white", midpoint = 0, limit = c(-1,1),
                       name="Pearson Correlation\nPopularity &\nMusic Composition Attributes") +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank())

### SOLVING THE PROBLEM ###



## Average Music Composition Attributes per Genre ##
# Select the df, group by genre, summarize the average of each variable. Then, create a bar plot to visualize the average attribute levels per genre.
av_music_top_5 <- music_top_5 %>% 
  group_by(Genre) %>% 
  summarise(across(c(danceability, energy,speechiness), mean, na.rm = TRUE), .groups = "keep") %>% 
  # Renaming the columns for visualization purposes
  rename(Dance = danceability, Ener = energy, Spe = speechiness) %>% 
  pivot_longer(., cols = c(Dance, Ener, Spe), names_to = "Var", values_to = "Val")

# Visualization of the average attributes by genre
func_visual(av_music_top_5, av_music_top_5$Var, av_music_top_5$Val, av_music_top_5$Var, "col")+
  facet_wrap(~Genre, nrow = 1)+
  theme_classic()+
  ggtitle("Average Level of Music Composition Attributes by Genre (Data Analysis 4/4)")+
  xlab("Music Composition Attributes Averages")+
  ylab("Values")+
  theme(plot.title = element_text(hjust = 0.5))

# New data frame with only the top 3 attributes, popularity and Genre
music_top_5_attr <- music_top_5 %>% 
  select(Genre, popularity, danceability, energy, speechiness) %>% 
  filter(popularity >= 70)

# Visualization of count of tracks from the dataset with a popularity over 70
func_visual(music_top_5_attr, music_top_5_attr$Genre, music_top_5_attr$popularity, music_top_5_attr$Genre,"col")+
  theme_classic()+
  ggtitle("Tracks with a Popularity over 70 by Genre")+
  xlab("Genre")+
  ylab("Value")+
  theme(plot.title = element_text(hjust = 0.5))

### MODELLING & COMMUNICATION ###

## Prediction Model
# Building a data frame with the popularity > 70 and the top 3 genres in popularity (HipHop/Rap, Pop and Electro)
top_popularity <- music_top_5 %>%
  filter(popularity > 70, Genre %in% c("HipHop/Rap", "Pop", "Electro"))

# Popularity Prediction Model where popularity is a function of danceability, speechiness and energy
predict_model <- lm(formula = popularity ~ danceability + speechiness + energy, data = top_popularity)

# Visualization of the model using broom::tidy
broom::tidy(predict_model)

## PredictedPopularity = 70.8682629 + (6.7116851*danceability) - (2.8268618*speechiness) - (0.3012474*energy)

## ELECTRO Genre Model
# Building a data frame keeping only the genre Electro 
electro <- music_top_5 %>%
 filter(popularity > 70, Genre %in% c("Electro")) 
  
# Popularity Prediction Model where popularity is a function of danceability, speechiness and energy
electro_model <- lm(formula = popularity ~ danceability + speechiness + energy, data = electro)
  
# Visualization of the model using broom::tidy
broom::tidy(electro_model)

## ElectroPredictedPopularity = 65.646833 + (9.682327*danceability) + (3.663718*speechiness) + (3.813377*energy)

## POP Genre Model
# Building a data frame keeping only the genre Pop 
pop <- music_top_5 %>%
  filter(popularity > 70, Genre %in% c("Pop")) 

# Popularity Prediction Model where popularity is a function of danceability, speechiness and energy
pop_model <- lm(formula = popularity ~ danceability + speechiness + energy, data = pop)

# Visualization of the model using broom::tidy
broom::tidy(pop_model)

## PopPredictedPopularity = 70.6154946 + (7.3641178*danceability) + (0.4995130*speechiness) - (0.9867791*energy)

## HIPHOP/RAP Genre Model
# Building a data frame keeping only the genre HipHop/Rap 
rap <- music_top_5 %>%
  filter(popularity > 70, Genre %in% c("HipHop/Rap")) 

# Popularity Prediction Model where popularity is a function of danceability, speechiness and energy
rap_model <- lm(formula = popularity ~ danceability + speechiness + energy, data = rap)

# Visualization of the model using broom::tidy
broom::tidy(rap_model)

## RapPredictedPopularity = 71.1713882 + (5.5411240*danceability) - (4.7735104*speechiness) + (0.9993428*energy)
