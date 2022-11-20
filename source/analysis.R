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
  pop_chart_jail <- ggplot(data = get_year_jail_pop(), aes(x = year, y = country_jail_pop)) +
    geom_bar(stat = "identity") +
    ylim(0, 800000) +
    xlab("Year") +
    ylab("Total Jail Population") +
    ggtitle("Increase of Jail Population in U.S. (1970-2018)") 
    
  return(pop_chart_jail)   
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
    summarize(state_jail_pop = sum(total_jail_pop, na.rm = TRUE)) %>% 

  return(selected_states_df)
}


plot_jail_pop_by_states <-function(states) {
  state_jail_chart <- ggplot(data = get_jail_pop_by_states(states), 
                             aes(x = year, y = state_jail_pop, group = state)) +
    geom_line(aes(color = state), linewidth = 1.5) +
    ggtitle("Growth of U.S. Jail Population By State (1970-2018)") +
    xlab("Year") +
    ylab("Total Jail Population")
  
  return(state_jail_chart)
}

plot_jail_pop_by_states(c("WA", "CA", "FL", "NY", "UT", "SD", "TN"))

## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#
get_female_rate_by_division <- function() {
  female_rate_df <- incarceration_df %>% 
    group_by(year, division) %>% 
    summarize(female_rate = mean(female_prison_pop_rate, na.rm = TRUE))
  
  return(female_rate_df)
}

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

plot_female_rate_by_division()

## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#



## Load data frame ---- 


