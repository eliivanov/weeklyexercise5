# weeklyexercise5---
title: 'Weekly Exercises #5'
author: "Eli Ivanov"
output: 
  html_document:
    keep_md: TRUE
    toc: TRUE
    toc_float: TRUE
    df_print: paged
    code_download: true
---


```{r setup, include=FALSE}
#knitr::opts_chunk$set(echo = TRUE, error=TRUE, message=FALSE, warning=FALSE)
```

```{r libraries}
library(tidyverse)     # for data cleaning and plotting
library(gardenR)       # for Lisa's garden data
library(lubridate)     # for date manipulation
library(openintro)     # for the abbr2state() function
library(palmerpenguins)# for Palmer penguin data
library(maps)          # for map data
library(ggmap)         # for mapping points on maps
library(gplots)        # for col2hex() function
library(RColorBrewer)  # for color palettes
library(sf)            # for working with spatial data
library(leaflet)       # for highly customizable mapping
library(ggthemes)      # for more themes (including theme_map())
library(plotly)        # for the ggplotly() - basic interactivity
library(gganimate)     # for adding animation layers to ggplots
library(transformr)    # for "tweening" (gganimate)
library(gifski)        # need the library for creating gifs but don't need to load each time
library(shiny)         # for creating interactive apps
theme_set(theme_minimal())
```

```{r data}
# SNCF Train data
small_trains <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-26/small_trains.csv") 

# Lisa's garden data
data("garden_harvest")

# Lisa's Mallorca cycling data
mallorca_bike_day7 <- read_csv("https://www.dropbox.com/s/zc6jan4ltmjtvy0/mallorca_bike_day7.csv?dl=1") %>% 
  select(1:4, speed)

# Heather Lendway's Ironman 70.3 Pan Am championships Panama data
panama_swim <- read_csv("https://raw.githubusercontent.com/llendway/gps-data/master/data/panama_swim_20160131.csv")

panama_bike <- read_csv("https://raw.githubusercontent.com/llendway/gps-data/master/data/panama_bike_20160131.csv")

panama_run <- read_csv("https://raw.githubusercontent.com/llendway/gps-data/master/data/panama_run_20160131.csv")

#COVID-19 data from the New York Times
covid19 <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")

```

## Put your homework on GitHub!

Go [here](https://github.com/llendway/github_for_collaboration/blob/master/github_for_collaboration.md) or to previous homework to remind yourself how to get set up. 

Once your repository is created, you should always open your **project** rather than just opening an .Rmd file. You can do that by either clicking on the .Rproj file in your repository folder on your computer. Or, by going to the upper right hand corner in R Studio and clicking the arrow next to where it says Project: (None). You should see your project come up in that list if you've used it recently. You could also go to File --> Open Project and navigate to your .Rproj file. 

## Instructions

* Put your name at the top of the document. 

* **For ALL graphs, you should include appropriate labels.** 

* Feel free to change the default theme, which I currently have set to `theme_minimal()`. 

* Use good coding practice. Read the short sections on good code with [pipes](https://style.tidyverse.org/pipes.html) and [ggplot2](https://style.tidyverse.org/ggplot2.html). **This is part of your grade!**

* **NEW!!** With animated graphs, add `eval=FALSE` to the code chunk that creates the animation and saves it using `anim_save()`. Add another code chunk to reread the gif back into the file. See the [tutorial](https://animation-and-interactivity-in-r.netlify.app/) for help. 

* When you are finished with ALL the exercises, uncomment the options at the top so your document looks nicer. Don't do it before then, or else you might miss some important warnings and messages.

## Warm-up exercises from tutorial

  1. Choose 2 graphs you have created for ANY assignment in this class and add interactivity using the `ggplotly()` function.
```{r,eval=FALSE}
beans_variety_daily <- garden_harvest %>% 
  filter(vegetable %in% c('beans', 'peas')) %>% 
  group_by(date, variety) %>% 
  summarize(daily_wt_kg = .01 * sum(weight))

graph1 <- beans_variety_daily %>%  
  ggplot(aes(x = date, 
             y = daily_wt_kg, 
             color = variety)) +
    labs(title = "Daily Bean and Pea Harvest in Kilograms", 
         x = " ", 
         y = " ") +
    geom_point() +
    geom_line() +
    theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) +
    scale_color_manual(values= c("dodgerblue4",
                                 "darkolivegreen4",
                                 "darkorchid3",
                                 "goldenrod1",
                                 "darkred")) +
   theme(legend.position="bottom") +
    guides(color=guide_legend(nrow=2,byrow=TRUE))
ggplotly(graph1, tooltip = c("text", "y"))
anim_save("Q1.gif")
```
```{r}
knitr::include_graphics("Q1.gif")
```
```{r,eval=FALSE}
washingtondc <- get_stamenmap(
    bbox = c(left = -77.3, bottom = 38.79, right = -76.8, top = 39.15), 
    maptype = "terrain",
    zoom = 11)
trips_per_station <- Trips %>%
  group_by(sstation) %>% 
  count() %>%
  inner_join(Stations, by = c("sstation" = "name"))
# Plot the points on the map
graph2 = ggmap(washingtondc) + # creates the map "background"
  geom_point(data = trips_per_station, 
             aes(x = long, y = lat, color = n), 
             alpha = .8, 
             size = .8)+
  theme_map() +
  theme(legend.background = element_blank())+
  scale_color_viridis_c(option = "D")
ggplotly(graph2, tooltip = "text")
anim_save("Q1.1.gif")

```

```{r}
knitr::include_graphics("Q1.1.gif")
```
  
  2. Use animation to tell an interesting story with the `small_trains` dataset that contains data from the SNCF (National Society of French Railways). These are Tidy Tuesday data! Read more about it [here](https://github.com/rfordatascience/tidytuesday/tree/master/data/2019/2019-02-26).

```{r,eval=FALSE}
small_trains %>%
  group_by(year, month) %>% 
  summarize(
    delay_rate =   sum(num_late_at_departure)/sum(total_num_trips)) %>% ggplot(aes(x = month, y = delay_rate))+
  geom_line()+
  facet_wrap(~year, scales = "free_y")+
  labs(title = "Delay rate in differnt years",
       x = "month",
       y = "delay rate")+
  transition_reveal(month)
anim_save("Q2.gif")
```
```{r}
knitr::include_graphics("Q2.gif")
```
## Garden data

  3. In this exercise, you will create a stacked area plot that reveals itself over time (see the `geom_area()` examples [here](https://ggplot2.tidyverse.org/reference/position_stack.html)). You will look at cumulative harvest of tomato varieties over time. You should do the following:
  * From the `garden_harvest` data, filter the data to the tomatoes and find the *daily* harvest in pounds for each variety.  
  * Then, for each variety, find the cumulative harvest in pounds.  
  * Use the data you just made to create a static cumulative harvest area plot, with the areas filled with different colors for each vegetable and arranged (HINT: `fct_reorder()`) from most to least harvested (most on the bottom).  
  * Add animation to reveal the plot over date. 


```{r,eval=FALSE}
garden_harvest %>% 
  filter(vegetable == "tomatoes") %>% 
  group_by(date, variety) %>% 
  summarize(daily_harvest_lb = sum(weight)*0.00220462) %>% 
  ungroup() %>% 
  complete(variety, date, fill = list(daily_harvest_lb = 0)) %>% 
  group_by(variety) %>% 
  summarize(variety = variety, 
            date = date, 
            daily_harvest_lb = daily_harvest_lb,
            cumulative_harvest = cumsum(daily_harvest_lb)) %>% 
  ggplot(aes(x = date, 
             y = cumulative_harvest, 
             group = fct_reorder(variety, cumulative_harvest)))+
  geom_area(aes(fill = fct_reorder(variety, cumulative_harvest)))+
  labs(title = "Cumulative tomato harvest",
       x = "date",
       y = "cumulative harvest weight(pounds)")+
  transition_reveal(date)
anim_save("Q3.gif")
```

```{r}
knitr::include_graphics("Q3.gif")
```
## Maps, animation, and movement!

  4. Map my `mallorca_bike_day7` bike ride using animation! 
  Requirements:
  * Plot on a map using `ggmap`.  
  * Show "current" location with a red point. 
  * Show path up until the current point.  
  * Color the path according to elevation.  
  * Show the time in the subtitle.  
  * CHALLENGE: use the `ggimage` package and `geom_image` to add a bike image instead of a red point. You can use [this](https://raw.githubusercontent.com/llendway/animation_and_interactivity/master/bike.png) image. See [here](https://goodekat.github.io/presentations/2019-isugg-gganimate-spooky/slides.html#35) for an example. 
  * Add something of your own! And comment on if you prefer this to the static map and why or why not.
  
```{r,eval=FALSE}
census_pop_est_2018 <- read_csv("https://www.dropbox.com/s/6txwv3b4ng7pepe/us_census_2018_state_pop_est.csv?dl=1") %>% 
  separate(state, into = c("dot","state"), extra = "merge") %>% 
  select(-dot) %>% 
  mutate(state = str_to_lower(state))
states_map <- map_data("state")
covid19_Fri <-covid19 %>% 
   mutate(day_of_week = wday(date,
                             label = TRUE),
          state = str_to_lower(state)) %>% 
   filter(day_of_week == "Fri")
covidFridayMap <-
covid19_Fri %>% 
  left_join(census_pop_est_2018,by = "state") %>%
  mutate(cases_per_10000 = 10000*cases/est_pop_2018) %>% 
  ggplot() +
  geom_map(map = states_map,
           aes(map_id = state,
               fill = cases_per_10000,
               group = date)) +
  expand_limits(x = states_map$long, y = states_map$lat) + 
  labs(title = "Cumulative COVID-19 cases per 10,000 residents",
       subtitle = "Date:{current_frame}") +
  theme_map() +
  theme(legend.background = element_blank())+
  transition_manual(date)
animate(covidFridayMap, nframe = 200, 
        end_pause = 10)
anim_save("Q4.gif")
```
```{r}
knitr::include_graphics("Q4.gif")
```


I prefer the static map, but the animated map does provide additional information. 

  5. In this exercise, you get to meet my sister, Heather! She is a proud Mac grad, currently works as a Data Scientist at 3M where she uses R everyday, and for a few years (while still holding a full-time job) she was a pro triathlete. You are going to map one of her races. The data from each discipline of the Ironman 70.3 Pan Am championships, Panama is in a separate file - `panama_swim`, `panama_bike`, and `panama_run`. Create a similar map to the one you created with my cycling data. You will need to make some small changes: 1. combine the files (HINT: `bind_rows()`, 2. make the leading dot a different color depending on the event (for an extra challenge, make it a different image using `geom_image()!), 3. CHALLENGE (optional): color by speed, which you will need to compute on your own from the data. You can read Heather's race report [here](https://heatherlendway.com/2016/02/10/ironman-70-3-pan-american-championships-panama-race-report/). She is also in the Macalester Athletics [Hall of Fame](https://athletics.macalester.edu/honors/hall-of-fame/heather-lendway/184) and still has records at the pool. 
  
```{r,eval=FALSE}
panama <-
  bind_rows(panama_swim, panama_bike, panama_run)
panamaMap <- get_stamenmap(
    bbox = c(left = min(panama$lon)-0.02, 
             bottom = min(panama$lat)-0.02, 
             right = max(panama$lon)+0.02, 
             top = max(panama$lat)+0.02), 
    maptype = "terrain",
    zoom = 12)
ggmap(panamaMap)+
  geom_path(data = panama,
            aes(x =lon,
                y = lat,
                col = event))+
  geom_point(data = panama,
             aes(x = lon,
                 y = lat))+
  scale_color_manual(values = c("Swim" = "darkblue",
                                "Bike" = "red", 
                                "Run" = "green"))+
  labs(title = "Heather's triathlon in Panama",
       subtitle = "Date: {frame_along}",
       x = "longitude",
       y = "latitude")+
  transition_reveal(time)
anim_save("Q5.gif")
```
```{r}
knitr::include_graphics("Q5.gif")
```

  
## COVID-19 data

  6. In this exercise, you are going to replicate many of the features in [this](https://aatishb.com/covidtrends/?region=US) visualization by Aitish Bhatia but include all US states. Requirements:
 * Create a new variable that computes the number of new cases in the past week (HINT: use the `lag()` function you've used in a previous set of exercises). Replace missing values with 0's using `replace_na()`.  
  * Filter the data to omit rows where the cumulative case counts are less than 20.  
  * Create a static plot with cumulative cases on the x-axis and new cases in the past 7 days on the y-axis. Connect the points for each state over time. HINTS: use `geom_path()` and add a `group` aesthetic.  Put the x and y axis on the log scale and make the tick labels look nice - `scales::comma` is one option. This plot will look pretty ugly as is.
  * Animate the plot to reveal the pattern by date. Display the date as the subtitle. Add a leading point to each state's line (`geom_point()`) and add the state name as a label (`geom_text()` - you should look at the `check_overlap` argument).  
  * Use the `animate()` function to have 200 frames in your animation and make it 30 seconds long. 
  * Comment on what you observe.
  
```{r,eval=FALSE}
covid19_trajectory <-
  covid19 %>% 
  group_by(state) %>% 
  mutate(seven_days_lag = replace_na(lag(cases,7),0),
         new_cases_7 = cases-seven_days_lag) %>% 
  filter(cases>=20) %>% 
  ggplot(aes(x = cases,
             y = new_cases_7,
             group = state,
             label = state))+
  geom_path()+
  scale_x_log10(labels = scales::comma)+
  scale_y_log10(labels = scales::comma)+
  geom_point()+
  geom_text(check_overlap = FALSE)+
  labs(title = "Trajectory of US Covid cases",
       subtitle = "Date: {frame_along}",
       x = "cumulative cases",
       y = "new cases in 7 days")+
  transition_reveal(date)
animate(covid19_trajectory, 
        nframe = 200,
        duration = 30,
        height = 500,
        width = 1200)
anim_save("Q6.gif")
```
```{r}
knitr::include_graphics("Q6.gif")
```


The new cases in the last 7 days first increasing and then starts decreasing as time goes on. This could mean that the spread of the virus slowing.

  7. In this exercise you will animate a map of the US, showing how cumulative COVID-19 cases per 10,000 residents has changed over time. This is similar to exercises 11 & 12 from the previous exercises, with the added animation! So, in the end, you should have something like the static map you made there, but animated over all the days. The code below gives the population estimates for each state and loads the `states_map` data. Here is a list of details you should include in the plot:
  
  * Put date in the subtitle.   
  * Because there are so many dates, you are going to only do the animation for all Fridays. So, use `wday()` to create a day of week variable and filter to all the Fridays.   
  * Use the `animate()` function to make the animation 200 frames instead of the default 100 and to pause for 10 frames on the end frame.   
  * Use `group = date` in `aes()`.   
  * Comment on what you see.  
```{r,eval=FALSE}
census_pop_est_2018 <- read_csv("https://www.dropbox.com/s/6txwv3b4ng7pepe/us_census_2018_state_pop_est.csv?dl=1") %>% 
  separate(state, into = c("dot","state"), extra = "merge") %>% 
  select(-dot) %>% 
  mutate(state = str_to_lower(state))
states_map <- map_data("state")
covid19_Fri <-
 covid19 %>% 
   mutate(day_of_week = wday(date,
                             label = TRUE),
          state = str_to_lower(state)) %>% 
   filter(day_of_week == "Fri")
covidFridayMap <-
covid19_Fri %>% 
  left_join(census_pop_est_2018,by = "state") %>%
  mutate(cases_per_10000 = 10000*cases/est_pop_2018) %>% 
  ggplot() +
  geom_map(map = states_map,
           aes(map_id = state,
               fill = cases_per_10000,
               group = date)) +
  expand_limits(x = states_map$long, y = states_map$lat) + 
  labs(title = "Cumulative COVID-19 cases per 10,000 residents",
       subtitle = "Date:{current_frame}") +
  theme_map() +
  theme(legend.background = element_blank())+
  transition_manual(date)
animate(covidFridayMap, nframe = 200, 
        end_pause = 10)
anim_save("Q7.gif")
```
 
```{r}
knitr::include_graphics("Q7.gif")
```

The cases per 10,000 residents shows that the spread of the virus was a lot more uniform across the country than it appeared in the previous animation.

## Your first `shiny` app (for next week!)

NOT DUE THIS WEEK! If any of you want to work ahead, this will be on next week's exercises.

  8. This app will also use the COVID data. Make sure you load that data and all the libraries you need in the `app.R` file you create. Below, you will post a link to the app that you publish on shinyapps.io. You will create an app to compare states' cumulative number of COVID cases over time. The x-axis will be number of days since 20+ cases and the y-axis will be cumulative cases on the log scale (`scale_y_log10()`). We use number of days since 20+ cases on the x-axis so we can make better comparisons of the curve trajectories. You will have an input box where the user can choose which states to compare (`selectInput()`) and have a submit button to click once the user has chosen all states they're interested in comparing. The graph should display a different line for each state, with labels either on the graph or in a legend. Color can be used if needed. 
  
## GitHub link

  9. Below, provide a link to your GitHub page with this set of Weekly Exercises. Specifically, if the name of the file is 05_exercises.Rmd, provide a link to the 05_exercises.md file, which is the one that will be most readable on GitHub. If that file isn't very readable, then provide a link to your main GitHub page.
  
https://github.com/eliivanov/weeklyexercise5.git

**DID YOU REMEMBER TO UNCOMMENT THE OPTIONS AT THE TOP?**
