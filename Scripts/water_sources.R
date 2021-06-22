library(glitr)
library(glamr)
library(gisr)
library(Wavelength)
library(ICPIutilities)
library(tidyverse)
library(scales)
library(sf)
library(extrafont)
library(tidytext)
library(ggtext)
library(here)
library(readxl)
library(ggplot2)

#hmis package- describe compactly describes every variable in a dataset, also can use 
#skimr


install.packages("tidytuesdayR")

tuesdata <- tidytuesdayR::tt_load('2021-05-04')
tuesdata <- tidytuesdayR::tt_load(2021, week = 19)
water <- tuesdata$water

water2 <- water %>%
  drop_na(water_source, install_year) %>% 
  mutate(count=1,
         install_year=as.numeric(install_year)) %>% 
  group_by(country_name, install_year, water_source) %>% 
  summarise(across(c(count), sum, na.rm=TRUE)) %>%
  #count(country_name)
  View()

`%notin%` <- Negate(`%in%`)

water2 %>%
  filter(country_name=="Uganda", #make variable, wrap into a function, use map or walk
         install_year>1970) %>%
  mutate(
    water_source = replace(water_source, water_source %notin% c("Borehole", "Rainwater Harvesting", "Undefined Shallow Well", "Protected Spring"), "Other"),
    water_source=factor(water_source, levels=c("Borehole", "Rainwater Harvesting", "Protected Spring", "Undefined Shallow Well", "Other")),
    water_color=case_when(
      water_source=="Borehole" ~denim,
      water_source=="Rainwater Harvesting" ~golden_sand,
      water_source=="Protected Spring" ~genoa,
      water_source=="Undefined Shallow Well" ~moody_blue,
      TRUE ~ grey40k)) %>%
  group_by(water_source) %>% 
  mutate(max_value=case_when(count==max(count)~count)) %>% 
  ungroup() %>% 
  ggplot(aes(x=install_year, y=count, group=water_color, color=water_color))+
  geom_line(size=1)+
  geom_area(alpha=.2, aes(fill=water_color))+
  geom_text(aes(label=comma(max_value), color=grey70k, vjust=-.5),na.rm=TRUE)+
  geom_vline(xintercept=2000, color=grey80k, linetype="dotted")+
  geom_vline(xintercept=2010, color=grey80k, linetype="dotted")+
  si_style_ygrid()+
  facet_wrap(~water_source, nrow=2)+
  theme(legend.position="none") +
  scale_y_continuous(limits=c(NA, 2500), label=comma)+
  labs(x = NULL, y = NULL, color = NULL,
       title = "WATER SOURCE MONITORING FREQUENCY - UGANDA", family="Source Sans Pro")+
  scale_color_identity()+
  scale_fill_identity()



ggsave("uganda_water_sources.png",
       height = 8,
       width = 14)
