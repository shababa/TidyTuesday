library(dplyr)
library(usmap)
library(ggplot2)
library(gganimate)
library(srvyr)
library(transformr)


beer_awards <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-10-20/beer_awards.csv')


# required for plot_usmap
beer_awards$fips<-fips(beer_awards$state)

# Count number of awards per state
beer_group <- beer_awards %>%
  group_by(year,state) %>% summarise(awards=n())


#Create the plot
beer_plot<-plot_usmap(data=beer_group, color = "black", size=.3, values="awards")+
  theme_void()+
  scale_fill_continuous(
    low = "white", high = "red", name = "# of awards", label = scales::comma
  ) +
  labs(title = "Beer Awards Won in {closest_state}")+ 
  theme(legend.position="bottom", legend.justification=c(.8,0),
        plot.title=element_text(face="bold", size=14, color="#262626",hjust=.5),
        plot.subtitle=element_text(hjust=.5))+
  transition_states(year, transition_length = 1, state_length = 1) 

# Create animation
anim<-animate(beer_plot, nframes=49, fps=1, ,renderer=gifski_renderer("beer_map.gif"))

