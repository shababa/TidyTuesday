library(srvyr)
library(tidyverse)
library(cowplot)
library(transformr)
library(gganimate)


# read data 
wind_turbine <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-10-27/wind-turbine.csv')

# The commissioning_date column contains multiple years so we'll take 
# only the first one for simplicity 
wind_turbine_transf <- wind_turbine %>%
  mutate(year = parse_number(commissioning_date)) 


# Get the total wind capacity per project.
cap_by_project <- wind_turbine_transf %>%
          group_by(project_name) %>%
          summarise(project_capacity = max(total_project_capacity_mw),
                    region = first(province_territory),
                    year = first(year)) %>%
          ungroup()

# The total capacity by province/territory per year 
region_cap_year_tot <- cap_by_project %>%
                       group_by(region,year) %>%
                       summarise(year_cap = sum(project_capacity))


# There are missing values in the data which will result in gaps in the racing
# bar graph. For example Alberta has entries for 1993 and 1997, and none inbetween. 


#Build a reference table with all province and year combinations
all_years <- 1993:2019
all_regions <- unique(region_cap_year_tot$region)
d1 <- expand.grid(year = all_years, region = all_regions)


# Merge data together. missing years will have NA for capacity  
datN <- merge(d1, region_cap_year_tot, all = TRUE)

# If capacity is NA is set to zero This means that in that year 
# the region did not add new turbines, and thus no new increase 
# in capacity
datN[is.na(datN)] <- 0

# Calculate the cumulative capacity for each year 
capicty_cumsum <- datN %>%
                  arrange(region,year) %>%
                  group_by(region) %>%
                  mutate(cum_cap = cumsum(year_cap))

#Add rank for the racing bar graph. The graph will only have the top 5 
# provinces and territories 
# https://stackoverflow.com/questions/53162821/animated-sorted-bar-chart-with-bars-overtaking-each-other

rank_df <- capicty_cumsum %>%
  group_by(year) %>%
  mutate(Rank = rank(-cum_cap),
         Value_rel = cum_cap/cum_cap[Rank==1])  %>%
  group_by(region) %>%
  filter(Rank <= 5) 

# Generate plot  
p <- ggplot(rank_df, aes(-Rank,cum_cap, fill = region)) +
  geom_col(width = 0.8, position = "dodge") +
  coord_flip(clip = "off") + 
  geom_text(aes(-Rank,y=0,label = region,hjust=1)) +       #country label
  geom_text(aes(-Rank,y=cum_cap,label = cum_cap, hjust=-0.5),position = position_nudge(y = -2)) + # value label
  theme_void()+
  theme(plot.margin=unit(c(1,1,1,3),"cm"))+
  theme(legend.position = "none",axis.title = element_blank()) +
  labs(title = "Total Wind Generation Capacity (mW) in {closest_state}")+ 
  # animate along Year
  transition_states(year,1,1)

# Generate gif 
animate(p,duration = 30, start_pause = 1, end_pause = 1, 
        height = 400, width = 900, renderer=gifski_renderer("wind.gif"))

