library(tidyverse)
library(leaflet)
library(lintr)

lint(filename = "analysis.R")

# map data from https://simplemaps.com/data/us-counties
# map data being used to create map in section 6
map_data <- read.csv("C:/Users/Grayson/Documents/info201/data/uscounties.csv")

map_data <- rename(map_data, fips = county_fips)

# The functions might be useful for A4
source("~/info201/assignments/a4-Graydwarf77/source/a4-helpers.R")
incarceration_df <- get_data()

## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return ("Hello world")
 }

# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}

## Section 2  ---- 
#----------------------------------------------------------------------------#
# Your functions and variables might go here ... <todo: update comment>
#----------------------------------------------------------------------------#

# Filters counties by years so more specific analysis can be done
all_counties_2016 <- incarceration_df %>% 
  filter(year == 2016)

all_counties_2016 <- left_join(all_counties_2016, map_data, by = "fips")

all_counties_1970 <- incarceration_df %>% 
  filter(year == 1970)

# calculating summary values for section 2

avg_prison_rate_1970 <- round(mean(all_counties_1970$total_prison_pop_rate, na.rm = TRUE), 2)

avg_prison_rate_2016 <- round(mean(all_counties_2016$total_prison_pop_rate, na.rm = TRUE), 2)

avg_black_prison_rate_2016 <- round(mean(all_counties_2016$black_prison_pop_rate, na.rm = TRUE), 2)

avg_white_prison_rate_2016 <- round(mean(all_counties_2016$white_prison_pop_rate, na.rm = TRUE), 2)

highest_female_prison_rate_1970 <- all_counties_1970 %>% 
  filter(female_prison_pop_rate == max(female_prison_pop_rate, na.rm = TRUE)) %>% 
  pull(female_prison_pop_rate)

highest_female_prison_rate_2016 <- all_counties_2016 %>% 
  filter(female_prison_pop_rate == max(female_prison_pop_rate, na.rm = TRUE)) %>% 
  pull(female_prison_pop_rate)
  

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
#----------------------------------------------------------------------------#

# This function gets the total jail population of 
# the US for every year in the dataset


get_year_jail_pop <- function() {
  summary_df <- incarceration_df %>% 
    group_by(year) %>% 
    summarize(country_jail_pop = sum(total_jail_pop, na.rm = TRUE))
  
  return(summary_df)

}

# This function creates a bar chart that displays data from the previous function
plot_jail_pop_for_us <- function()  {
  pop_chart_jail <- ggplot(data = get_year_jail_pop(), a
                           es(x = year, y = country_jail_pop)) +
    geom_bar(stat = "identity") +
    ylim(0, 800000) +
    xlab("Year") +
    ylab("Total Jail Population") +
    ggtitle("Increase of Jail Population in U.S. (1970-2018)") +
    scale_y_continuous(labels = function(x) format(x, scientific = FALSE))
    
  return(pop_chart_jail)   
} 

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
# See Canvas
#----------------------------------------------------------------------------#

# This function filters the data by the inputted states and 
# calculates their total jail population for each state by year.
get_jail_pop_by_states <- function(states) {
  selected_states_df <- incarceration_df %>% 
    filter(str_detect(state, states)) %>% 
    group_by(state, year) %>% 
    summarize(state_jail_pop = sum(total_jail_pop, na.rm = TRUE)) %>% 

  return(selected_states_df)
}

# This function creates a line chart that displays data from the previous function 
# once again based on the states inputted.
plot_jail_pop_by_states <-function(states) {
  state_jail_chart <- ggplot(data = get_jail_pop_by_states(states), 
                             aes(x = year, y = state_jail_pop, group = state)) +
    geom_line(aes(color = state), linewidth = .9) +
    ggtitle("Growth of U.S. Jail Population By State (1970-2018)") +
    xlab("Year") +
    ylab("Total Jail Population")
  
  return(state_jail_chart)
}


## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# See Canvas
#----------------------------------------------------------------------------#

# This function retrieves female rates of incarceration 
# grouped by geographical division
get_female_rate_by_division <- function() {
  female_rate_df <- incarceration_df %>% 
    group_by(year, division) %>% 
    summarize(female_rate = mean(female_prison_pop_rate, na.rm = TRUE))
  
  return(female_rate_df)
}

# This function produces a line chart using the data retrieved 
# from the previous function
plot_female_rate_by_division <- function() {
  region_prison_chart <- ggplot(data = get_female_rate_by_division(), 
                                aes(x = year, y = female_rate, group = division)) +
  geom_line(aes(color = division), linewidth = 1.2) +
  xlim(1970, 2016) +
  ggtitle("Female Prison Rate By Division (1970-2016)") +
  xlab("Year") +
  ylab("Female Prison Rate")
  
  return(region_prison_chart)
}



## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# See Canvas
#----------------------------------------------------------------------------#

# This function creates a data frame that is wrangled to be used effectively
# in a map
get_latinx_jail_rate_2016 <- function() {
  latinx_jail_map_df <- all_counties_2016 %>% 
    select(fips, state, county_name, latinx_jail_pop_rate, lat, lng) %>% 
    mutate(radius = latinx_jail_pop_rate / mean(latinx_jail_pop_rate, na.rm = TRUE))
  
  return(latinx_jail_map_df)
}

# This function uses the data frame produced by the previous function to 
# create an interactive map
map_latinx_jail_rate_2016 <- function() {
  latinx_jail_map <- leaflet(get_latinx_jail_rate_2016()) %>% 
    addTiles() %>% 
    addCircleMarkers(
      lat = ~lat,
      lng = ~lng,
      popup = ~county_name,
      stroke = FALSE,
      radius = ~radius,
      fillOpacity = 0.2
    )
  return(latinx_jail_map)
}

## Load data frame ---- 

