library(dplyr)
library(tidyverse)
library(ggplot2)
library(gt)

# load in files for bar charts

path <- file.path("~", "Desktop", "Police_Use_of_Force.csv")
invisible(Force <- read.csv(path, stringsAsFactors = TRUE))

# consolidate the domestic abuse levels 
levels(Force$Problem)[levels(Force$Problem) == 'Domestic Abuse-In Progress '] <- "Domestic Abuse"
levels(Force$Problem)[levels(Force$Problem) == 'Domestic '] <- "Domestic Abuse"
levels(Force$Problem)[levels(Force$Problem) == 'Domestic Abuse Report Only '] <- "Domestic Abuse"
levels(Force$Problem)[levels(Force$Problem) == 'Domestic with Weapons '] <- "Domestic Abuse"

# consolidate the suspicious person/vehicle levels
levels(Force$Problem)[levels(Force$Problem) == 'Suspicious Person '] <- "Suspicious Person/Vehicle"
levels(Force$Problem)[levels(Force$Problem) == 'Suspcious Vehicle '] <- "Suspicious Person/Vehicle"
levels(Force$Problem)[levels(Force$Problem) == 'Unwanted Person '] <- "Suspicious Person/Vehicle"
levels(Force$Problem)[levels(Force$Problem) == 'Suspected Prostitute '] <- "Suspicious Person/Vehicle"
levels(Force$Problem)[levels(Force$Problem) == 'Person with a Gun '] <- "Suspicious Person/Vehicle"
levels(Force$Problem)[levels(Force$Problem) == 'Person with a Weapon '] <- "Suspicious Person/Vehicle"

# consolidate the assault levels
levels(Force$Problem)[levels(Force$Problem) == 'Assault in Progress '] <- "Assault"
levels(Force$Problem)[levels(Force$Problem) == 'Assault Report Only '] <- "Assault"
levels(Force$Problem)[levels(Force$Problem) == 'Fight '] <- "Assault"

# consolidate the disturbance levels
levels(Force$Problem)[levels(Force$Problem) == 'Disturbance '] <- "Disturbance"
levels(Force$Problem)[levels(Force$Problem) == 'Loud Party '] <- "Disturbance"
levels(Force$Problem)[levels(Force$Problem) == 'Music-Loud '] <- "Disturbance"
levels(Force$Problem)[levels(Force$Problem) == 'Drunk/Intoxicated Person '] <- "Disturbance"
levels(Force$Problem)[levels(Force$Problem) == 'Firecrackers '] <- "Disturbance"

# consolidate the traffic law levels
levels(Force$Problem)[levels(Force$Problem) == 'Traffic Law Enforcement '] <- "Traffic Violations"

# consolidate the Unknown level
levels(Force$Problem)[levels(Force$Problem) == 'Unknown Trouble '] <- "Unspecified Trouble"

# setting all missing (blank) entries to NA
Force[Force==" " | Force == ""] <- NA

# setting zero values to NA
Force[Force == 0] <- NA

# omit the rows with NA values
Force <- na.omit(Force)

## PROBLEMS

# Prep data frame
allDF <- Force %>%
    select(Problem, Race) %>%
    arrange(Race) %>%
    group_by(Race, Problem) %>%
    summarize(Counts = n()) %>%
    pivot_wider(names_from = Problem, values_from = Counts)

# select the top 5 problems
allDF <- allDF %>%
    select('Suspicious Person/Vehicle', 'Assault', 'Domestic Abuse', 'Disturbance', 'Traffic Violations')

# specify which races to include
finalDF <- allDF %>%
    filter(Race == 'White' | 
               Race == 'Black' |
               Race == 'Asian' |
               Race == 'Native American') %>%
    arrange(desc(`Suspicious Person/Vehicle`))

# create table
finalDF %>%
    ungroup(Race) %>%
    gt() %>%
    cols_label(
        Race = "Race"
    ) %>%
    tab_header(
        title = "Reasons for Police Response by Race"
    ) %>%
    tab_spanner(
        label = "Problems",
        columns = vars('Suspicious Person/Vehicle', 'Assault', 'Domestic Abuse', 'Disturbance', 'Traffic Violations')
    ) %>%
    fmt_number(
        columns = vars('Suspicious Person/Vehicle', 'Assault', 'Domestic Abuse', 'Disturbance', 'Traffic Violations'),
        sep = ',',
        decimals = 0
    )
    
# plots by race

#black offenses
invisible(BlackDF <- Force %>%
              filter(Race == 'Black'))

# prep data frame
blackOffense <- BlackDF %>%
    select(Problem) %>%
    count(Problem) %>%
    arrange(desc(n)) %>%
    top_n(5, n) #getting the top five
boDF <- as.data.frame(blackOffense)

# manually reorder the levels in decreasing order
boDF$Problem <- factor(boDF$Problem, levels = c('Suspicious Person/Vehicle', 'Assault', 'Domestic Abuse', 'Disturbance', 'Traffic Violations'))

# change the n column into count column
colnames(boDF)[colnames(boDF) == "n"] <- "Counts"

# plot the bar graph
ggplot(boDF, aes(x=Problem, y=Counts)) + 
    geom_bar(stat = 'identity', fill = 'steelblue') +
    geom_text(aes(label=Counts), vjust=1.6, color="white", size=3.5) +
    ggtitle("Black Use of Force Cases") +
    theme(
        plot.title = element_text(face = 'bold', size = 18, hjust = .5),
        axis.title.x = element_text(face = 'bold', size = 14),
        axis.title.y = element_text(face = 'bold', size = 14))
#white offenses
invisible(WhiteDf <- Force %>%
              filter(Race == 'White'))

# Prep the data frame

whiteOffense <- WhiteDf %>%
    select(Problem) %>%
    count(Problem) %>%
    arrange(desc(n)) %>%
    top_n(5, n) # getting the top five

woDF <- as.data.frame(whiteOffense)

# manually reorder the levels in decreasing order
woDF$Problem <- factor(woDF$Problem, levels = c('Suspicious Person/Vehicle', 'Assault', 'Domestic Abuse', 'Disturbance', 'Unspecified Trouble'))

# change the n column into count column
colnames(woDF)[colnames(woDF) == "n"] <- "Counts"

# plot the bar graph
ggplot(woDF, aes(x=Problem, y=Counts)) + 
    geom_bar(stat = 'identity', fill = 'firebrick') +
    geom_text(aes(label=Counts), vjust=1.6, color="white", size=3.5) +
    ggtitle("White Use of Force Cases") +
    theme(
        plot.title = element_text(face = 'bold', size = 18, hjust = .5),
        axis.title.x = element_text(face = 'bold', size = 14),
        axis.title.y = element_text(face = 'bold', size = 14))

## INJURY

# prep the data frame
InjuryDF <- Force %>%
    select(SubjectInjury, Race) %>%
    arrange(Race) %>%
    group_by(Race, SubjectInjury) %>%
    summarize(Counts = n()) %>%
    pivot_wider(names_from = SubjectInjury, values_from = Counts)

# Specify races
InjuryDF <- InjuryDF %>%
    filter(Race == 'White' | 
               Race == 'Black' |
               Race == 'Asian' |
               Race == 'Native American') %>%
    arrange(desc(Yes))

# create table
InjuryDF %>%
    ungroup(Race) %>%
    gt() %>%
    cols_label(
        Race = "Race"
    ) %>%
    tab_header(
        title = "Subject Injury by Race"
    ) %>%
    tab_spanner(
        label = "Was the Subject Injured from Police Force?",
        columns = vars('Yes', 'No')
    ) %>%
    fmt_number(
        columns = vars('Yes', 'No'),
        sep = ',',
        decimals = 0
    )