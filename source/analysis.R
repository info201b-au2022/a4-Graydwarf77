library(tidyverse)


# The functions might be useful for A4
source("~/info201/assignments/a4-Graydwarf77/source/a4-helpers.R")
incarceration_df <- get_data()

View(incarceration_df)

get_basic_info(incarceration_df)

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
all_counties_2016 <- incarceration_df %>% 
  filter(year == 2016)

all_counties_1970 <- incarceration_df %>% 
  filter(year == 1970)

avg_prison_rate_1970 <- round(mean(all_counties_1970$total_prison_pop_rate, na.rm = TRUE), 2)

avg_prison_rate_2016 <- round(mean(all_counties_2016$total_prison_pop_rate, na.rm = TRUE), 2)

highest_black_prison_rate_2016 <- all_counties_2016 %>%  
  filter(black_prison_pop_rate == max(black_prison_pop_rate, na.rm = TRUE)) %>% 
  pull(black_prison_pop_rate)

highest_black_prison_rate_state_2016 <- all_counties_2016 %>%  
  filter(black_prison_pop_rate == max(black_prison_pop_rate, na.rm = TRUE)) %>% 
  pull(county_name)

highest_white_prison_rate_2016 <- all_counties_2016 %>% 
  filter(white_prison_pop_rate == max(white_prison_pop_rate, na.rm = TRUE)) %>% 
  pull(white_prison_pop_rate)

avg_female_prison_rate_1970 <- all_counties_1970 %>% 
  filter(female_prison_pop_rate == max(female_prison_pop_rate, na.rm = TRUE)) %>% 
  pull(female_prison_pop_rate)

avg_female_prison_rate_2016 <- all_counties_2016 %>% 
  filter(female_prison_pop_rate == max(female_prison_pop_rate, na.rm = TRUE)) %>% 
  pull(female_prison_pop_rate)
  

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# Your functions might go here ... <todo:  update comment>
#----------------------------------------------------------------------------#

# This function ... <todo:  update comment>


get_year_jail_pop <- function() {
  summary_df <- incarceration_df %>% 
    group_by(year) %>% 
    summarize(country_jail_pop = sum(total_jail_pop, na.rm = TRUE))
  
  return(summary_df)

}



# This function ... <todo:  update comment>
plot_jail_pop_for_us <- function()  {
  jail_pop_chart <- ggplot(data = get_year_jail_pop(), aes(x = year, y = country_jail_pop)) +
    geom_bar(stat = "identity") +
    ylim(0, 800000) +
    xlab("Year") +
    ylab("Total Jail Population") +
    ggtitle("Increase of Jail Population in U.S. (1970-2018)") +
    
  return(jail_pop_chart)   
} 

plot_jail_pop_for_us()

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#
get_jail_pop_by_states <- function(states) {
  selected_states_df <- incarceration_df %>% 
    filter(str_detect(state, states)) %>% 
    group_by(state, year) %>% 
    summarize(state_jail_pop = sum(total_jail_pop, na.rm = TRUE))

  return(selected_states_df)
}

df <- get_jail_pop_by_states(c("AL", "AK"))

plot_jail_pop_by_states <-function(states) {
  
  return()
}

## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

## Load data frame ---- 


