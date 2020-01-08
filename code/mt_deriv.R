# Derivation of the mean temperature over france

# Loading packages 
require(tidyverse)
require(openxlsx)
require(janitor)

# Loading data 
data = read.xlsx("./data/raw_data.xlsx")
population = read.xlsx("./data/population.xlsx")

# Renaming data
colnames(data) = c("date",
    "tmax", "tmin", "station")
colnames(population) = c("dep",
    "ndep", "population")

# NAs verification
## tmax
x = as.numeric(data$tmax)
length(which(is.na(x) == T))
## tmin
x = as.numeric(data$tmin)
length(which(is.na(x) == T))

# Converting date formats 
data = data %>% 
    mutate(# date = excel_numeric_to_date(date),
        tmax = as.numeric(tmax),
        tmin = as.numeric(tmin))
# Averaging by weak
## 01/01/2012 - sunday
## 26/09/2016, 27/09/2016 - monday and tuesday
data = data %>% 
    filter(date != 40909,
        date != 42639,
        date != 42640)
# Summraise by weak
weak = data.frame(date = unique(data$date),
    weak = rep(1:(length(unique(data$date))/7), each = 7))
datax = left_join(data, weak, by = "date")
# Departments
unique(datax$station) %>%
    View()
# Regroup cities by department
length(unique(datax$station))
comm = read.delim("./data/communes.txt")
comm = comm %>% 
    select(nreg = REG, ndep = DEP, ville = NCC)
comm %>% 
    arrange(ville) %>%
    View()
# Station names verif
datax$station2 = str_replace(datax$station, " / [[:graph:]]*", "")
datax$station2 = str_replace(datax$station2, "/[[:graph:]]*", "")
datax$station2 = str_replace(datax$station2, " - [[:graph:]]*", "")
datax$station2 = str_replace(datax$station2, "LA ", "")
datax$station2 = str_replace(datax$station2, "LE ", "")
datax$station2 = str_replace(datax$station2, "LES ", "")
datax$station2 = str_replace(datax$station2, " ", "-")
unique(datax$station2) %>%
    View() # Insufficient
# Join
datay = left_join(datax, comm, by = c("station2" = "ville"))
# Verify
summary(datay$ndep) # Nearly half NAs
# Join population
head(population)
population[1:9, 2] = c("01", "02", "03",
    "04", "05", "06", "07", "08", "09")
dataX = left_join(datay, population, by = "ndep")
View(dataX)
# Means by dep
# Clear uneq
dataX = dataX %>% 
    filter(is.na(ndep) == FALSE)
totalpop = population %>% 
    filter(ndep %in% unique(dataX$ndep)) %>% 
    summarise(total = sum(population))
totalpop[1,1]
## totalpop = population[97,3]
# Means
dataS = dataX %>% 
    group_by(ndep, date) %>% 
    summarise(tmax = mean(tmax, na.rm = T),
        tmin = mean(tmin, na.rm = T),
        weak = first(weak),
        share_pop = mean(population, na.rm = T)/totalpop) %>% 
    ungroup() %>% 
    group_by(weak) %>% 
    summarise(tmax = mean(tmax*share_pop, na.rm = T),
        tmin = mean(tmin*share_pop, na.rm = T))
View(dataS)
# Mean temperature
dataXS = dataS %>% 
    mutate()