# Loading Libraries and Code Config
library(tidyr)
library(forcats)
library(ggplot2)
library(gt)
library(lubridate)
library(markdown)
library(readr)
library(dplyr)
library(stringr)
library(finalfit)
library(gtools)
library(patchwork)
library(png)
library(webshot)
library(readr)
library(sp)
library(raster)
library(sf)
library(broom)

## Helpful Web pages
#https://lakens.github.io/statistical_inferences/index.html
#https://www.burns-stat.com/pages/Tutor/R_inferno.pdf
#https://argoshare.is.ed.ac.uk/healthyr_book/
#https://www.opendata.nhs.scot/dataset/unintentional-injuries

## Loading Raw Data 
#Health Board Codes (HB)
HB <- read_csv("hb_lookup.csv")
#Unintentional accidents admissions (UIA_raw)
UIA_raw <- read_csv("ui_admissions_2022.csv")
# Unintentional accidents deaths (UID_raw)
UID_raw <- read_csv("ui_deaths_2022.csv")


# Exploring admissions data

unique_age <- unique(UIA_raw["AgeGroup"])
unique_location <- unique(UIA_raw["InjuryLocation"])
unique_injury <- unique(UID_clean["InjuryType"])
unique_hb <- unique(UIA_raw["HBR"])
unique_date <- unique(UID_clean["Year"])

##Cleaning UIA and UID

UIA_clean <- UIA_raw %>% 
# Removing aggregate values
  filter(AgeGroup != 'All') %>%
  filter(AgeGroup != 'under75 years') %>%
  filter(Sex != 'All') %>% 
  filter(HBR != 'S92000003') %>% 
  filter(InjuryType != 'All Diagnoses') %>% 
# Joining HB names
  left_join(HB,HDA, by = c("HBR" = "HB"))  

UID_clean <- UID_raw %>% # Removing aggregate values
  filter(AgeGroup != 'All') %>%
  filter(AgeGroup != 'under75 years') %>%
  filter(Sex != 'All') %>% 
  filter(HBR != 'S92000003') %>% 
  filter(InjuryType != 'All') %>% 
  # Joining HB names
  left_join(HB,HDA, by = c("HBR" = "HB"))  

# Dropping QF cols
drops <- c('HBRQF','CAQF','AgeGroupQF',
           'SexQF','InjuryLocationQF','InjuryTypeQF',
           'HBDateArchived','Country','HBDateEnacted')

UIA_clean <- UIA_clean[ , !(names(UIA_clean) %in% drops)]
UID_clean <- UID_clean[ , !(names(UID_clean) %in% drops)]

# Aggregating Age groups into 3 age categories

age_groups <- c(    "0-4 years"    = "Children",
                    "5-9 years"    = "Children",
                    "10-14 years"  = "Children",
                    "15-24 years"  = "Adults",
                    "25-44 years"  = "Adults",
                    "45-64 years"  = "Adults",
                    "65-74 years"  = "Elderly",
                    "75plus years" = "Elderly")
#For UIA
UIA_clean$AgeGroup <- UIA_clean$AgeGroup %>% 
  str_replace_all(age_groups)

#For UID
UID_clean$AgeGroup <- UID_clean$AgeGroup %>% 
  str_replace_all(age_groups)

# Changing "Land transport accidents" in deaths data set to "RTA" for
# better continuity between Figures

UID_clean$InjuryType <- UID_clean$InjuryType %>% 
  str_replace_all(c("Land transport accidents" = "RTA"))


# Formatting Year col to Date
UID_clean$Year <- as.Date.character(UID_clean$Year, format = "%Y")
UID_clean$Year <- year(UID_clean$Year)

# To order facet wrap later
#For UIA
UIA_clean$AgeGroup <- factor(UIA_clean$AgeGroup, levels = c("Children",
                                                            "Adults",
                                                            "Elderly"))
#For UID
UID_clean$AgeGroup <- factor(UID_clean$AgeGroup, levels = c("Children",
                                                            "Adults",
                                                            "Elderly"))
### UIA Figure
## Line plot of injuries over time per age group
fig1a_data <- UIA_clean %>% 
  group_by(FinancialYear,AgeGroup,InjuryType) %>% 
  summarise(NumberOfAdmissions = sum(NumberOfAdmissions))

# Distinct colours for fig1a
colours <- c("#1F77B4", "#AEC7E8", "#FF7F0E", "#FFBB78",
             "#2CA02C", "#98DF8A", "#D62728", "#FF9896")

# Fig1a settings
fig1a <- fig1a_data %>% 
  ggplot(aes(x= FinancialYear,
             y= NumberOfAdmissions,
             group = InjuryType,
             colour = InjuryType)) +
  geom_line(size = 1, aes(col = InjuryType)) +
# log^10 scale to separate lines
  scale_y_log10()+
  facet_wrap(~AgeGroup) +
  labs(x = "Financial Year",
       y = "Number of Admissions (Log^10)",
       title = "Admissions over time",
       subtitle = "") +
  theme_bw()+
# Adjusting text elements to centre titles and offset x axis values
  theme(axis.text.x = element_text(angle = 70, hjust = 1,vjust = 1),
        plot.caption.position = "panel",
        plot.title = element_text(hjust = .5),
        plot.subtitle = element_text(hjust = .5),
        legend.position = "bottom")+
# Colour setting
  scale_color_manual(values =  colours,
                     guide = guide_legend(nrow = 1))

## Table to show fold change between 2011 and 2020
# 2011 and 2021 data frame

UIA_2011 <- UIA_clean %>% 
  filter(FinancialYear == "2011/12") %>% 
  select('FinancialYear','AgeGroup','InjuryType','NumberOfAdmissions') %>% 
  group_by(FinancialYear,AgeGroup,InjuryType) %>% 
  summarise(NumberOfAdmissions = sum(NumberOfAdmissions))

UIA_2020 <- UIA_clean %>% 
  filter(FinancialYear == "2020/21") %>% 
  select('FinancialYear','AgeGroup','InjuryType','NumberOfAdmissions') %>% 
  group_by(FinancialYear,AgeGroup,InjuryType) %>% 
  summarise(NumberOfAdmissions = sum(NumberOfAdmissions))

# Gluing both tables together
fig1b_data <- bind_rows(UIA_2011,UIA_2020)

## GT table of counts and fold change
# Separating dates to own columns for table
fig1b_data <-  fig1b_data %>% 
  pivot_wider(names_from = FinancialYear, values_from = NumberOfAdmissions) 
  
# Adding fold change column
fig1b_data$Foldchange <- ((fig1b_data$`2020/21`/fig1b_data$`2011/12`)-1)

# GT table
fig1b <- fig1b_data%>%              
  gt(rowname_col = "InjuryType", groupname_col = "AgeGroup") %>% 
  tab_spanner(label = "Number of Admissions",
              columns = c("2011/12","2020/21")) %>% 
  
# Fold change to percentage
  fmt_percent(columns = Foldchange,
             decimals = 1) %>% 
  
# Formatting table
  cols_label(Foldchange = "Fold Change (%)") %>% 
  tab_stubhead(label = "Injury Type per Age Group") %>% 
  cols_align(align = "center", columns = everything()) %>% 
  tab_header(title = "Counts Table with Fold change  (%)",
             subtitle = "2011/12 and 2020/21 snapshot") %>% 
  tab_style(style = cell_text(align = "left"),
            locations = cells_stub(rows = TRUE))

# Saving Gt table as PNG to work with patchwork 
#Solution by JohannesNE(https://github.com/rstudio/gt/issues/420)

tmp <- tempfile(fileext = '.png') #generate path to temp .png file
gtsave(fig1b, tmp, path = NULL) #save gt table as png
fig1b_png <- png::readPNG(tmp, native = TRUE) # read tmp png file


# Patchwork fig1 a and b together
fig1a + fig1b_png + 
  plot_annotation(tag_levels = "a",
                  tag_suffix = ")",
                  title = "Fig 1. NHS Scotland admissions of Unintentional injuries",
                  subtitle = "By Age Group from 2011 to 2021",
                  caption = "Data aggregated from all of NHS Scotlands' health boards",
  theme = theme(plot.title = element_text(hjust = 0.5),
                plot.subtitle = element_text(hjust = 0.5),
                plot.caption = element_text(hjust = 0)))
        
### UID Figure
## Line plot of injuries over time per age group
fig2a_data <- UID_clean %>% 
  group_by(Year,AgeGroup,InjuryType) %>% 
  summarise(NumberofDeaths = sum(NumberofDeaths))

# Fig2a settings
fig2a <- fig2a_data %>% 
  ggplot(aes(x= Year,
             y= NumberofDeaths,
             group = InjuryType,
             colour = InjuryType)) +
  geom_line(size = 1, aes(col = InjuryType)) +
  # log^10 scale to separate lines
  scale_y_log10()+
  scale_x_continuous(breaks = c(2011:2020))+
  facet_wrap(~AgeGroup) +
  labs(x = "Year",
       y = "Number of Deaths (Log^10)",
       title = "Deaths over time",
       subtitle = "") +
  theme_bw()+
  # Adjusting text elements to center titles and offset x axis values
  theme(axis.text.x = element_text(angle = 70, hjust = 1,vjust = 1),
        plot.caption.position = "panel",
        plot.title = element_text(hjust = .5),
        plot.subtitle = element_text(hjust = .5),
        legend.position = "bottom")+
  # Colour setting same as fig1
  scale_color_manual(values =  colours,
                     guide = guide_legend(nrow = 1))

## Table to show fold change between 2011 and 2020
# 2011 and 2021 data frame

UID_2011 <- UID_clean %>% 
  filter(Year == "2011") %>% 
  select('Year','AgeGroup','InjuryType','NumberofDeaths') %>% 
  group_by(Year,AgeGroup,InjuryType) %>% 
  summarise(NumberofDeaths = sum(NumberofDeaths))

UID_2020 <- UID_clean %>% 
  filter(Year == "2020") %>% 
  select('Year','AgeGroup','InjuryType','NumberofDeaths') %>% 
  group_by(Year,AgeGroup,InjuryType) %>% 
  summarise(NumberofDeaths = sum(NumberofDeaths))

# Gluing both tables together
fig2b_data <- bind_rows(UID_2011,UID_2020)

## GT table of counts and fold change
# Separating dates to own columns for table
fig2b_data <-  fig2b_data %>% 
  pivot_wider(names_from = Year, values_from = NumberofDeaths) 

# Adding fold change column
fig2b_data$Foldchange <- ((fig2b_data$`2020`/fig2b_data$`2011`)-1)

# GT table
fig2b <- fig2b_data%>%              
  gt(rowname_col = "InjuryType", groupname_col = "AgeGroup") %>% 
  tab_spanner(label = "Number of Deaths",
              columns = c("2011","2020")) %>% 
  # Substituting NAs with -
  sub_missing(columns = Foldchange,
              missing_text = "-") %>% 
  
  # Fold change to percentage
  fmt_percent(columns = Foldchange,
              decimals = 1) %>% 
  
  # Formatting table
  cols_label(Foldchange = "Fold Change (%)") %>% 
  tab_stubhead(label = "Injury Type per Age Group") %>% 
  cols_align(align = "center", columns = everything()) %>% 
  tab_header(title = "Counts Table with Fold change  (%)",
             subtitle = "2011/12 and 2020/21 snapshot") %>% 
  tab_style(style = cell_text(align = "left"),
            locations = cells_stub(rows = TRUE))

# Saving Gt table as PNG to work with patchwork 
#Solution by JohannesNE(https://github.com/rstudio/gt/issues/420)

tmp <- tempfile(fileext = '.png') #generate path to temp .png file
gtsave(fig2b, tmp, path = NULL) #save gt table as png
fig2b_png <- png::readPNG(tmp, native = TRUE) # read tmp png file


# Patchwork fig2 a and b together
fig2a + fig2b_png + 
  plot_annotation(tag_levels = "a",
                  tag_suffix = ")",
                  title = "Fig 2. NHS Scotland deaths from Unintentional injuries",
                  subtitle = "By Age Group from 2011 to 2021",
                  caption = "Data aggregated from all of NHS Scotlands' health boards",
                  theme = theme(plot.title = element_text(hjust = 0.5),
                                plot.subtitle = element_text(hjust = 0.5),
                                plot.caption = element_text(hjust = 0)))

## Fig 3
# Focus on Adult Poisonings dataframes 2011 + 2020

AP2020_A <- UIA_clean %>% 
  select(c(HBName,FinancialYear,AgeGroup,InjuryLocation,InjuryType,NumberOfAdmissions)) %>% 
  filter(AgeGroup == "Adults") %>% 
  filter(InjuryType == "Poisoning") %>% 
  filter(FinancialYear == "2020/21") %>% 
  group_by(HBName) %>% 
  summarise('2020 Admissions' = sum(NumberOfAdmissions))

AP2011_A <- UIA_clean %>% 
  select(c(HBName,FinancialYear,AgeGroup,InjuryLocation,InjuryType,NumberOfAdmissions)) %>% 
  filter(AgeGroup == "Adults") %>% 
  filter(InjuryType == "Poisoning") %>% 
  filter(FinancialYear == "2011/12") %>% 
  group_by(HBName) %>% 
  summarise( '2011 Admissions' = sum(NumberOfAdmissions))

AP_admissions <- full_join(AP2011_A, AP2020_A, by = "HBName")

AP_admissions$Foldchange <- ((AP2020_A$`2020 Admissions`/ AP2011_A$`2011 Admissions`)-1)

AP2020_D <- UID_clean %>% 
  select(c(HBName,Year,AgeGroup,InjuryLocation,InjuryType,NumberofDeaths)) %>% 
  filter(AgeGroup == "Adults") %>% 
  filter(InjuryType == "Poisoning") %>% 
  filter(Year == "2020") %>% 
  group_by(HBName) %>% 
  summarise('2020 Deaths' = sum(NumberofDeaths))

AP2011_D <- UID_clean %>% 
  select(c(HBName,Year,AgeGroup,InjuryLocation,InjuryType,NumberofDeaths)) %>% 
  filter(AgeGroup == "Adults") %>% 
  filter(InjuryType == "Poisoning") %>% 
  filter(Year == "2011") %>% 
  group_by(HBName) %>% 
  summarise( '2011 Deaths' = sum(NumberofDeaths))

AP_deaths <- full_join(AP2011_D,AP2020_D)

AP_deaths$Foldchange <- ((AP2020_D$`2020 Deaths`/ AP2011_D$`2011 Deaths`)-1)

AP <- full_join(AP_admissions,AP_deaths, by = "HBName")
# NHS Scotland spatial data 
# https://spatialdata.gov.scot/geonetwork/srv/eng/catalog.search#/metadata/f12c3826-4b4b-40e6-bf4f-77b9ed01dc14


# GT table of Adult poisonings by HB 

AP %>% 
  gt() %>% 
  fmt_percent(columns = c(Foldchange.x,Foldchange.y),
              decimals = 1) %>% 
  tab_spanner(label = "Admissions",
              columns = c("2011 Admissions","2020 Admissions",Foldchange.x)) %>% 
  tab_spanner(label = "Deaths",
              columns = c("2011 Deaths","2020 Deaths",Foldchange.y)) %>% 
  cols_label("2011 Admissions" = "2011",
             "2020 Admissions" = "2020",
             "2011 Deaths" = "2011",
             "2020 Deaths" = "2020",
             HBName = "HealthBoards",
             Foldchange.x = "Fold change",
             Foldchange.y = "Fold change") %>%
  tab_header(title = "Admissions and Deaths of Adult Poisonings in NHS Scotland ",
             subtitle = "Counts of 2011 and 2020")


# Specail data frames for figure analysis and discussion
  
UIA_struck <- UIA_clean %>% 
  filter(InjuryType == 'Struck by, against') %>% 
  filter(AgeGroup == 'Adults') %>% 
  group_by(FinancialYear) %>% 
  summarise(NumberOfAdmissions= sum(NumberOfAdmissions))

UID_RTA <- UID_clean %>% 
  filter(InjuryType == "RTA") %>% 
  filter(AgeGroup == "Children") %>% 
  group_by(Year) %>% 
  summarise(NumberofDeaths = sum(NumberofDeaths))

UID_AE <- UID_clean %>% 
  filter(InjuryType == "Accidental exposure") %>% 
  filter(AgeGroup == "Elderly") %>% 
  group_by(Year) %>% 
  summarise(NumberofDeaths = sum(NumberofDeaths))
