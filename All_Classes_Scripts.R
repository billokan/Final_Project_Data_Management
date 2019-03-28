####################################################### Week 1  ###############################################################################
#nothing important was said in week 1 




####################################################### Week 2  ##############################################################################

p <- ggplot(data = gapminder,
            +             mapping = aes(x = gdpPercap,
                                        +                           y = lifeExp))
p
## choosing a geom
p + geom_point()
p + geom_smooth()
p + geom_smooth(method = "lm")
p + geom_smooth() + geom_point() 

##adjusting the scales 
p + geom_smooth() + geom_point() + scale_x_log10() # it is important the order of the expression, smooth goes after
p + geom_point() + geom_smooth() + scale_x_log10()

##labels and titles 
p + geom_smooth() + geom_point() +
        scale_x_log10(labels = scales::dollar)
# function label 
p + geom_point() +
        geom_smooth() +
        scale_x_log10(labels = scales::dollar) +
        labs(x = "GDP Per Capita",
             y = "Life Expectancy in Years",
             title = "Economic Growth and Life Expectancy",
             subtitle = "Data points are country-years",
             caption = "Source: Gapminder.")

## Aesthetic mapping 
#We can easily map variables in our dataset to aesthetics such size,
#color, shape, and so on. For example, to map color to continent

p <- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap,
                          y = lifeExp,
                          color = continent))
p + geom_point() + scale_x_log10()

#what's wrong with this code? 
p <- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap, y = lifeExp,
                          color = "blue"))  #we created a variable called blue
p + geom_point()
# aes() treated the word “blue” as though it were a variable, and
# since it could not find it in the dataset, it created it on the fly. By
# default, ggplot shows all the points in the category “blue” and
# colors them using its default first-category hue . . . which is red.

#to make it work this is the right expression 
p <- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap, y = lifeExp))
p + geom_point(color = "blue")

# possible to change coloro, size, smooth, transparancy 

# Both points and smoother are colored by continent. We can use
#fill inside aes() to color the interior of the smoother’s standard
#error ribbon:
p <- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap,
                          y = lifeExp,
                          color = continent,
                          fill = continent))
p + geom_point() +
        geom_smooth() +
        scale_x_log10()
# Having 5 different smoothers makes the plot difficult to read. If we
#just want one line but keep the colored points we can map the
#aesthetics we want only the geom_ functions that we want them to apply to:
p <- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap,
                          y = lifeExp))
p + geom_point(mapping = aes(color = continent)) +
        geom_smooth() +
        scale_x_log10()

##Group 
#what's wrong here? 
p <- ggplot(data = gapminder,
            mapping = aes(x = year,
                          y = gdpPercap))
p + geom_line() #ggplot does not know that the yearly observations in the data are grouped by country. 

#We have to tell it:
p + geom_line(aes(group = country))

## Facet 
#To make the trend clearly we could facet the data by a third variable and plot the results in separate panels.

p <- ggplot(data = gapminder,
            mapping = aes(x = year,
                          y = gdpPercap))
p + geom_line(aes(group = country)) +
        facet_wrap(~ continent)

#We can add a smoother and a few cosmetic enhancements that make the graph a little more effective

p + geom_line(color="gray70", aes(group = country)) +
        geom_smooth(size = 1.1, method = "loess", se = FALSE) +
        scale_y_log10(labels=scales::dollar) +
        facet_wrap(~ continent, ncol = 3) +
        labs(x = "Year",
             y = "GDP per capita",
             title = "GDP per capita on Five Continents")

#facet_wrap() is best used when you want a series of small multiples based on a single categorical variable. 
#If you want to cross-classify some data by two categorical variables you should try facet_grid() instead.

## Statistical transformation 

p <- ggplot(data = diamonds,
            mapping = aes(x = cut))
p + geom_bar()

#The bar chart returns a count of the number of (country-year) observations by continent. 
#It does this using the default stat_function associated with it, stat_count(). 
#We can also override the default stat, for example to display the relative frequencies:

p + geom_bar(mapping = aes(y = ..prop..)) #grouping issue
# We need to tell ggplot to ignore the x-categories when calculating denominator of the proportion, 
# and use the total number observations instead.
p + geom_bar(mapping = aes(y = ..prop.., group = 1))

#make it more aesthetical pleasant 
p <- ggplot(data = diamonds,
            mapping = aes(x = cut, fill = cut))
p + geom_bar()

#If you map the fill aesthetic to another variable, like clarity, the bars are automatically stacked:
p <- ggplot(data = diamonds,
            mapping = aes(x = cut, fill = clarity))
p + geom_bar()

## Homework 
#question 1
ggplot(data = mpg) + 
        geom_point(mapping = aes(x=displ, y=hwy, color = drv)) + 
        geom_smooth(mapping = aes(x = displ, y = hwy, linetype = drv), se=FALSE) 

## question 3 
ggplot(data = diamonds) + 
        geom_histogram(mapping = aes(x = price, fill = cut))


####################################################### Week 3 ###############################################################################


library(tidyverse)
library(nycflights13)

## to explore the basic data manipulation of dplyr
nycflights13::flights # this is a TIBBLE. Tibbles are data frames, but slightly tweaked to work better in the tidyverse
#int stands for integers.
#dbl stands for doubles, or real numbers.
#chr stands for character vectors, or strings.
#dttm stands for date-times (a date + a time).

# to see the whole data set:
View(flights)


######################## 5 main DPLYR functions: ###############################
#1.Pick observations by their values (filter()).
#2.Reorder the rows (arrange()).
#3.Pick variables by their names (select()).
#4.Create new variables with functions of existing variables (mutate()).
#5.Collapse many values down to a single summary (summarise()).

group_by() # can be used in conjuction to change the scope of each function from operating 
#on the entire dataset to operating on it group-by-group

# first argument is the data frame, subsequent arguments describe what to do with the data frame (using var names)


###################### Filter ROWS with filter() ###############################
filter(flights, month==1, day==1)
# dplyr never modifies inputs, so you need to save the results in a new var 
jan1 <- filter(flights, month == 1, day == 1)
# if you want to both save and see the result: 
(dec25 <- filter(flights, month == 12, day == 25)) 

################ Comparison #####################
#R provides the standard suite: >, >=, <, <=, != (not equal), and == (equal).
# = does not work, you have to use == 

# other issue is with floating point numbers 
sqrt(2) ^ 2 == 2
#> [1] FALSE
1/49 * 49 == 1
#> [1] FALSE

# computers use finite precision arithmetic, so every number is an approximation 
# instead of == is better to use near()
near(sqrt(2) ^ 2,  2)
#> [1] TRUE
near(1 / 49 * 49, 1)
#> [1] TRUE

############## Logical operators ################# 
# & is "and" 
# | is "or"
# ! is "not" 

filter(flights, month == 11 | month == 12) ## all flights departing in november or december 
# the order of operations does not work like English -> cannot write month==11|12 
# instad it finds all month equal 11|12 which translates TRUE, which values equal 1 so this finds all flights of January 

x %in% y #  This will select every row where x is one of the values in y
nov_dec <- filter(flights, month %in% c(11, 12))

## De Morgan's law ## 
# !(x & y) = !x | !y
# !(x | y) = !x & !y 

# to find flights that were not delayed by more than two hours: 
filter(flights, !(arr_delay > 120 | dep_delay > 120))
filter(flights, arr_delay <= 120, dep_delay <= 120)

############### Missing Values ###################
# NA represents an unknown value so missing values are “contagious”:
# almost any operation involving an unknown value will also be unknown
is.na(x) # to determine if a values is na 

filter() # only includes rows TRUE, it excludes both NA and FALSE 
# id you want to include NA, you have to explicitly ask for them 
df <- tibble(x = c(1, NA, 3))
filter(df, x > 1)


######################### Arrange Rows with arrange() ##########################
# arrange() is similar to filter() but instead it changes the order of rows 
arrange(flights, year, month, day)
arrange(flights, desc(arr_delay)) # desc() to re-order by descending order 

# missing values are always sorted at the end 
df <- tibble(x = c(NA, 2, 5))
arrange(df, x)
arrange(df, desc(x))


####################### Select columns with select() ###########################
#allows you to rapidly zoom in on a useful subset using operations based on the names of the variables.
select(flights, year, month, day)  # Select columns by name
select(flights, year:day) # Select all columns between year and day (inclusive)
select(flights, -(year:day)) # Select all columns except those from year to day (inclusive)

## helper functions to use with select() ## 
starts_with("abc") # matches names that begin with “abc”.
ends_with("xyz") # matches names that end with “xyz”.
contains("ijk") # matches names that contain “ijk”.
matches("(.)\\1") #selects variables that match a regular expression. This one matches any variables that contain repeated characters. 
num_range("x", 1:3) # matches x1, x2 and x3.

select() # can be used to rename a variable but it drops all the rest 
rename() # instead keeps all the other variables 

rename(flights, tail_num = tailnum)

## another option is to use select() in conjunction with everything() helper 
# this is useful to move to the start of a large data frame 

select(flights, time_hour, air_time, everything())


###################### Add a new variable with mutate() #########################
mutate() # always adds a new column at the end of the dataset 
# example # 
flights_sml <- select(flights, 
                      year:day, 
                      ends_with("delay"), 
                      distance, 
                      air_time
)
mutate(flights_sml,
       gain = arr_delay - dep_delay,
       speed = distance / air_time * 60
)
# now change the new colums 
mutate(flights_sml,
       gain = arr_delay - dep_delay,
       hours = air_time / 60,
       gain_per_hour = gain / hours
)
# if you want to keep only the new columns use transmute()
transmute(flights,
          gain = arr_delay - dep_delay,
          hours = air_time / 60,
          gain_per_hour = gain / hours
)

############## Useful creation functions ################
# many functions to create new variable to use with mutate()
# key property is that the function must be vectorised: 
# it must take a vector of values as input, return a vector with the same number of values as output

# there are functions to be used, these are the main: 
# 1. arithmetic operators: +, -, *, /, ^ -> "recylying rules": if one parameter is shorter it will be extended to be the same 
# x / sum(x) -> calculates the proportion of a total ; y - mean(y) -> computes the difference from the mean

# 2. modular arithmetic -> %/% (integer division) and %% (remainder), where x == y * (x %/% y) + (x %% y)
# it allows you to break integers up into pieces
transmute(flights,
          dep_time,
          hour = dep_time %/% 100, # compute hour and minute from dep_time 
          minute = dep_time %% 100
)

# 3. Logs: log(), log2(), log10() -> for dealing with data that ranges across multiple orders of magnitude
# All else being equal,using log2() because it’s easy to interpret: a difference of 1 on the log scale
# corresponds to doubling on the original scale and a difference of -1 corresponds to halving.

# 4. Offsets: lead() and lag() allow you to refer to leading or lagging values
# This allows you to compute running differences (x - lag(x)) or find when values change (x != lag(x))
(x <- 1:10)
lead(x)
lag(x)

# 5. Cummulative and rolling aggregates
#R provides functions for running sums, products, mins and maxes: cumsum(), cumprod(), cummin(), cummax(); and dplyr provides cummean() for cumulative means. 
#If you need rolling aggregates (i.e. a sum computed over a rolling window), try the RcppRoll package.
x
cumsum(x)
cummean(x)

# 6. Logical comparison (<, <=, >, >=, !=,)

# 7. Ranking 
y <- c(1, 2, 2, NA, 3, 4)
min_rank(y)
#> [1]  1  2  2 NA  4  5
min_rank(desc(y))
#> [1]  5  3  3 NA  2  1

# If min_rank() doesn’t do, look at row_number(), dense_rank(), percent_rank(), cume_dist(), ntile()
row_number(y)
#> [1]  1  2  3 NA  4  5
dense_rank(y)
#> [1]  1  2  2 NA  3  4
percent_rank(y)
#> [1] 0.00 0.25 0.25   NA 0.75 1.00
cume_dist(y)
#> [1] 0.2 0.6 0.6  NA 0.8 1.0
# use the help function for more details 

####################################################### Week 3 Homework ####################################################################

library(tidyverse)
library(nycflights13)

# Which plane (tailnum) has the worst on-time record? N844MH
arrange(flights, desc(arr_delay))

flights %>%
        group_by(tailnum) %>%
        summarise(arr_delay = mean(arr_delay, na.rm = TRUE)) %>%
        arrange(desc(arr_delay))


# At what time (minutes after midnight) did the first flight leave on September 18, 2013? 290 

flights %>%
        filter(month==9, day==18, year==2013) %>% 
        mutate(minutes_after_mid = dep_time %/% 100 * 60 + dep_time %% 100,
               sched_dep_time2 = sched_dep_time %/% 100 *
                       60 + sched_dep_time %% 100) %>%
        select(dep_time, minutes_after_mid , sched_dep_time,
               sched_dep_time2) %>% 
        arrange(dep_time)


# How many flights left before 5am in September (including # of delayed flights from the previous day)? 30 

flights %>%
        filter(!is.na(dep_delay)) %>%
        filter(month == 9, dep_time < 500) %>%
        n_distinct()

# What is the average (mean) departure delay of United Airlines? Round to the nearest integer.
car <- group_by(flights, carrier)
summarise(car, delay = round(mean(dep_delay, na.rm = TRUE)))


# Which carrier (two letter abbreviation) has the shortest average (mean) departure delay when you take into account the distance that carrier traveled?

flights %>%
        group_by(carrier) %>%
        mutate(delay = dep_delay / distance) %>%
        summarise(delay = mean(delay, na.rm =T)) %>%
        arrange(delay) %>%
        head(n=1)

# Which departure airport (FAA airport code) has the longest mean departure delay in September?
aereoporti <- group_by(flights, origin)  %>% 
        filter(year==2013, month==9)                   
summarise(aereoporti, delay = mean(dep_delay, na.rm = TRUE))

# Which departure airport (FAA airport code) has the highest number of departure delays that are longer than 2 hours?

daily <- group_by(flights, origin) %>% 
        filter(dep_delay >= 120)  
(per_day   <- summarise(daily, dep_delays = n()))

# How many flights were delayed by at least an hour, but made up over 45 minutes in flight?
filter(flights, !is.na(dep_delay), dep_delay >= 60, dep_delay-arr_delay > 45) %>% 
        n_distinct()

# Which destination has the largest spread (standard deviation) in terms of distance that planes traveled to get to it.

flights %>% 
        group_by(dest) %>% 
        summarise(distance_sd = sd(distance)) %>% 
        arrange(desc(distance_sd))

# Which destination has the most carriers? 
flights %>%
        filter(!is.na(dep_delay), !is.na(arr_delay)) %>%
        group_by(dest) %>%
        summarise(carriers = n_distinct(carrier)) %>%
        arrange(desc(carriers)) %>%
        head(n = 4)




####################################################### Week 4 Homework ####################################################################

###########CHAPTER 1: Exploring Categorical Data################################

# working with factors 
levels(comics$align)
levels(comics$id) #NA are not counted in levels
table(comics$id, comics$align)

##############CONTIGENCY TABLE REVIEW####################################
# Print the first rows of the data
comics
head(comics,1)

# Check levels of align
levels(comics$align)

# Check the levels of gender
levels(comics$gender)

# Create a 2-way contingency table
table(comics$gender,comics$align)

################DROPPING LEVELS######################################
# Load dplyr
library(dplyr)

# Print tab
tab

# Remove align level
comics <- comics %>%
        filter(align != "Reformed Criminals") %>%
        droplevels()

############### SIDE BY SIDE BARCHARTS ##############################
# Load ggplot2
library(ggplot2)

# Create side-by-side barchart of gender by alignment
ggplot(comics, aes(x = align, fill = gender)) + 
        geom_bar(position = "dodge")

# Create side-by-side barchart of alignment by gender
ggplot(comics, aes(x = gender, fill = align)) + 
        geom_bar(position="dodge") +
        theme(axis.text.x = element_text(angle = 90))

##############COUNT VS PROPORTIONS##################################
# Plot of gender by align
ggplot(comics, aes(x = align, fill = gender)) +
        geom_bar()

# Plot proportion of gender, conditional on align
ggplot(comics, aes(x = align, fill = gender)) + 
        geom_bar(position = "fill")


##############MARGINAL BARCHART###################################
# Change the order of the levels in align
comics$align <- factor(comics$align, 
                       levels = c("Bad", "Neutral", "Good"))

# Create plot of align
ggplot(comics, aes(x = align)) + 
        geom_bar()

#################CONDITIONAL BARCHART############################
# Plot of alignment broken down by gender
ggplot(comics, aes(x = align)) + 
        geom_bar() +
        facet_wrap(~ gender)

#################IMPROVE PIECHART################################
# Put levels of flavor in decending order
lev <- c("apple", "key lime", "boston creme", "blueberry", "cherry", "pumpkin", "strawberry")
pies$flavor <- factor(pies$flavor, levels = lev)

# Create barchart of flavor
ggplot(pies, aes(x = flavor)) + 
        geom_bar(fill = "chartreuse") + 
        theme(axis.text.x = element_text(angle = 90))


###################CHAPTER 2: EXPLORING NUMERICAL DATA##########################
str() #puts each variable as a row folowed but the data type 

###################FACETED HISTOGRAM##########################
# Load package
library(ggplot2)

# Learn data structure
str(cars)

# Create faceted histogram
ggplot(cars, aes(x = city_mpg)) +
        geom_histogram() +
        facet_wrap(~ suv)

###################BOXPLOTS AND DENSITY PLOTS###################
# Filter cars with 4, 6, 8 cylinders
common_cyl <- filter(cars,ncyl %in% c(4,6,8))

# Create box plots of city mpg by ncyl
ggplot(common_cyl, aes(x = as.factor(ncyl), y = city_mpg)) +
        geom_boxplot()

# Create overlaid density plots for same data
ggplot(common_cyl, aes(x = city_mpg, fill = as.factor(ncyl))) +
        geom_density(alpha = .3)

###########MARGINAL AND CONDITIONAL HISTOGRAMS##################
# Create hist of horsepwr
cars %>%
        ggplot(aes(horsepwr)) +
        geom_histogram() +
        ggtitle("car_horsepwr")

# Create hist of horsepwr for affordable cars
cars %>% 
        filter(msrp <25000 ) %>%
        ggplot(aes(horsepwr)) +
        geom_histogram() +
        xlim(c(90, 550)) +
        ggtitle("label2")

######################THREE BINWIDTHS##########################
# Create hist of horsepwr with binwidth of 3
cars %>%
        ggplot(aes(horsepwr)) +
        geom_histogram(binwidth = 3) +
        ggtitle("binwidth_3")

# Create hist of horsepwr with binwidth of 30
cars %>%
        ggplot(aes(horsepwr)) +
        geom_histogram(binwidth = 30) +
        ggtitle("binwidth_30")

# Create hist of horsepwr with binwidth of 60
cars %>%
        ggplot(aes(horsepwr)) +
        geom_histogram(binwidth = 60) +
        ggtitle("binwidth_60")

###################BOXPLOTS FOR OUTLIERS ########################
# Construct box plot of msrp
cars %>%
        ggplot(aes(x = 1, y = msrp)) +
        geom_boxplot()

# Exclude outliers from data
cars_no_out <- cars %>%
        filter(msrp <100000)

# Construct box plot of msrp using the reduced dataset
cars_no_out %>%
        ggplot(aes(x=1,y=msrp)) +
        geom_boxplot()

######################PLOT SELECTION################################
# Create plot of city_mpg
cars %>%
        ggplot(aes(x=1,y=city_mpg)) +
        geom_boxplot()

# Create plot of width
cars %>% 
        ggplot(aes(x = width)) +
        geom_density()

###################3 VARIABLE PLOT################################
# Facet hists using hwy mileage and ncyl
common_cyl %>%
        ggplot(aes(x = hwy_mpg)) +
        geom_histogram() +
        facet_grid(ncyl ~ suv) +
        ggtitle("faceted cars")


#############CHAPTER 3: NUMERICAL SUMMARIES############################

############CALCULATE CENTER MEASURES #######################
# Create dataset of 2007 data
gap2007 <- filter(gapminder, year == 2007)

# Compute groupwise mean and median lifeExp
gap2007 %>%
        group_by(continent) %>%
        summarize(mean(lifeExp),
                  median(lifeExp))

# Generate box plots of lifeExp for each continent
gap2007 %>%
        ggplot(aes(x = continent, y = lifeExp)) +
        geom_boxplot()

###########CALCULATE SPREAD MEASURES#######################
# Compute groupwise measures of spread
gap2007 %>%
        group_by(continent) %>%
        summarize(sd(lifeExp),
                  IQR(lifeExp),
                  n())

# Generate overlaid density plots
gap2007 %>%
        ggplot(aes(x = lifeExp, fill = continent)) +
        geom_density(alpha = 0.3)

#############CHOSE MEASURES FOR CENTER AND SPREAD###############
# Compute stats for lifeExp in Americas
gap2007 %>%
        filter(continent == "Americas") %>%
        summarize(sd(lifeExp),
                  mean(lifeExp))

# Compute stats for population
gap2007 %>%
        summarize(median(pop),
                  IQR(pop))

##############TRANSFORMATIONS##################################
# Create density plot of old variable
gap2007 %>%
        ggplot(aes(x = pop)) +
        geom_density()

# Transform the skewed pop variable
gap2007 <- gap2007 %>%
        mutate(pop, log_pop = log(pop))

# Create density plot of new variable
gap2007 %>%
        ggplot(aes(x = log_pop)) +
        geom_density()

#############IDENTIFY OUTLIERS###############################
# Filter for Asia, add column indicating outliers
gap_asia <- gap2007 %>%
        filter(continent == "Asia") %>%
        mutate(is_outlier = lifeExp<50)

# Remove outliers, create box plot of lifeExp
gap_asia %>%
        filter(!is_outlier) %>%
        ggplot(aes(x = 1, y = lifeExp)) +
        geom_boxplot()

###################CHAPTER 3: CASE STUDY#####################################

#################SPAM AND NUM_CHAR################################
# Load packages
library(ggplot2)
library(dplyr)
library(openintro)

# Compute summary statistics
email %>%
        group_by(spam) %>%
        summarize(median(num_char),IQR(num_char))

# Create plot
email %>%
        mutate(log_num_char = log(num_char)) %>%
        ggplot(aes(x = spam, y = log_num_char)) +
        geom_boxplot()

###############SPAM AND !!!!!################################
# Compute center and spread for exclaim_mess by spam
email %>% 
        group_by(spam) %>% 
        summarize(median(exclaim_mess), sd(exclaim_mess))

# Create plot for spam and exclaim_mess
email %>% 
        mutate(log_exclaim_mess = log(exclaim_mess)) %>% 
        ggplot(aes(x = spam, y = )) +
        geom_density(alpha = .3) ###not correct but passed

################COLLAPSING LEVELS########################## 
# Create plot of proportion of spam by image
email %>%
        mutate(has_image = image > 0) %>%
        ggplot(aes(x = as.factor(has_image), fill = spam)) +
        geom_bar(position = "fill")

###############DATA INTEGRY###############################
# Test if images count as attachments
table(email$image,email$attach)
email$image > email$attach
sum(email$image > email$attach)

##############ANSWERING THE QUESTION WITH CHAIN############
# Question 1
email %>%
        filter(dollar > 0) %>%
        group_by(spam) %>%
        summarize(median(dollar))

# Question 2
email %>%
        filter(dollar > 10) %>%
        ggplot(aes(x = spam)) +
        geom_bar()

##########WHAT'S IN A NUMBER############################
# Reorder levels
email$number <- factor(email$number, 
                       levels = c( "none", "small", "big"))

# Construct plot of number
ggplot(email, aes(x = number)) +
        geom_bar() +
        facet_wrap(~spam)


####################################################### Week 5  ##################################################################################
# install.packages("tidyverse")
library(tidyverse)
library(readxl)
library(DBI)
install.packages("RMySQL")
library(RMySQL) 
library(httr)
library(jsonlite)
library(haven)
# read.csv() sets sep=“,” and header=TRUE
potatoes <- read.csv("potatoes.csv")
head(potatoes)

### read.csv()######
potatoes <- read_csv("potatoes.csv", skip = 2) # You can use skip = n to skip the first n lines; or use comment = "#" to drop all lines that start with (e.g.) #:
read_csv("1,2,3\n4,5,6", col_names = FALSE)  # You can use col_names = FALSE to not treat the first row as headings, and instead label them sequentially from X1 to ‘Xn:

### read.delim #####
properties <- c("area", "temp", "size", "storage",
                "method", "texture", "flavor",
                "moistness")
potatoes <- read_delim("data/potatoes.txt", delim = "\t",
                       col_names = properties)
# read_delim is the main readr function and takes two mandatory arguments, file and delim.

### readr parser ### 
guess_parser("2010-10-01")
guess_parser("15:01")
guess_parser(c("TRUE", "FALSE"))
guess_parser(c("1", "5", "9"))

### readr() col_types ### 
potatoes <- read_tsv("data/potatoes.txt",
                     col_types = "cccccccc",
                     col_names = properties)
# You can use col_types to specify which types the columns in your imported data frame should have

### What is wrong with these lines ### 
read_csv("a,b\n1,2,3\n4,5,6")
read_csv("a,b,c\n1,2\n1,2,3,4")
read_csv("a;b\n1;3")    # separete is not a comma but semi-comma 

###readxl ### 
excel_sheets(path = "urbanpop.xlsx")
pop1 <- read_excel("urbanpop.xlsx", sheet = 1)
pop2 <- read_excel("urbanpop.xlsx", sheet = 2)
pop3 <- read_excel("urbanpop.xlsx", sheet = 3)
pop <- lapply(excel_sheets(path = "urbanpop.xlsx"),
              read_excel, path = "urbanpop.xlsx")
# You can use skip to control which cells are read and col_names to set the column names.
pop <- read_excel(path = "urbanpop.xlsx", sheet=2,
                  skip=21, col_names=FALSE)


###### importing data from database #######
host <- "courses.csrrinzqubik.us-east-1.rds.amazonaws.com"
con <- dbConnect(RMySQL::MySQL(),
                 dbname = "tweater",
                 host = host,
                 port = 3306,
                 user = "student",
                 password = "datacamp")

tables <- dbListTables(con)
tables

# You can use the dbReadTable() function to import data from the database tables.
users <- dbReadTable(con, "users")
users

# Again, you can use lapply to import all tables:
tableNames <- dbListTables(con)
tables <- lapply(tableNames, dbReadTable, conn = con)
tables

# You can use read_csv to directly import csv files from the web.
url <- paste0("https://raw.githubusercontent.com/",
              "mhaber/HertieDataScience/master/",
              "slides/week5/data/potatoes.csv")
potatoes <- read_csv(url)

# read_excel() does not yet support importing excel files directly from the web so you have to download the file first with download.file():
url <- paste0("https://github.com/",
              "mhaber/HertieDataScience/blob/master/",
              "slides/week5/data/urbanpop.xlsx?raw=true")
download.file(url, "urbanpop.xlsx", mode = "wb")
urbanpop <- read_excel("urbanpop.xlsx")

### httr ###
url <- "http://www.example.com/"
resp <- GET(url)
content <- content(resp, as = "raw")
head(content)

### reading data from json ### 
url <- paste0("http://mysafeinfo.com/api/",
              "data?list=englishmonarchs&format=json")
jsonData <- fromJSON(url)
str(jsonData)





####################################################### Week 6  ##################################################################################
library(tidyverse)
library(readr)

# base GitHub url
url <- paste0("https://raw.githubusercontent.com/",
              "mhaber/HertieDataScience/master/",
              "slides/week6/data/")
pew <- read_csv(paste0(url,"pew.csv"))
billboard <- read_csv(paste0(url,"billboard.csv"))
weather <- read_tsv(paste0(url,"weather.txt"))

# Compute rate per 10,000
table1 %>%
        dplyr::mutate(rate = cases / population * 10000)
# Compute cases per year
table1 %>%
        dplyr::count(year, wt = cases)

# Excercise slide 16
# 1. Compute the rate for table2. You will need to perform four operations:
# 1.1 Extract the number of TB cases per country per year.
# 1.2 Extract the matching population per country per year.
# 1.3 Divide cases by population, and multiply by 10000.
# 1.4 Store back in the appropriate place.

cases <- table2 %>%
        dplyr::filter(type == "cases") %>%
        dplyr::pull(count)
country <- table2 %>%
        dplyr::filter(type == "cases") %>%
        dplyr::pull(country)
year <- table2 %>%
        dplyr::filter(type == "cases") %>%
        dplyr::pull(year)
population <- table2 %>%
        dplyr::filter(type == "population") %>%
        dplyr::pull(count)


### gathering and spreading  ### 
pewTidy <- pew %>%
        tidyr::gather(key = income, value = frequency,
                      -religion)
pewTidy

#To tidy this dataset, we first gather together all the wk columns.
#The column names give the week and the values are the ranks:
billboardTidy <- billboard %>%
        tidyr::gather(key = week, value = rank,
                      wk1:wk76, na.rm = TRUE)
# but we are not done. Let’s turn the week into a numeric variable and create a proper date column
billboardTidy2 <- billboardTidy %>%
        dplyr::mutate(week = readr::parse_number(week),
                      date = as.Date(date.entered) + 7 *
                              (week - 1)) %>%
        dplyr::select(-date.entered) %>%
        dplyr::arrange(artist, track, week)

# spread 
table2 %>%
        tidyr::spread(key = type, value = count)

### Excercise slide 31 ### 
#1. Are gather() and spread() perfectly symmetrical? Carefully consider the following example:
stocks <- tibble(
        year = c(2015, 2015, 2016, 2016),
        half = c( 1, 2, 1, 2),
        return = c(1.88, 0.59, 0.92, 0.17)
)
stocks %>%
        tidyr::spread(year, return) %>%
        tidyr::gather("year", "return", `2015`:`2016`)

stocks %>% 
        tidyr::spread(year,return)

##gather() and spread() are not perfectly symmetrical
#because column type information is not transferred between
#them (i.e. year column).

#2. Both spread() and gather() have a convert argument. What does it do?
?spread() #look at the definition of convert 
# the convert argument tries to convert character vectors to the appropiate type

#3. Why does this code fail?
table4a %>%
        tidyr::gather(1999, 2000, key = "year", value = "cases")
# you can not call a variable with a number 
#the column names 1999 and 2000 are not standard and thus need to be quoted

#4. Using the weather data: Tidy the day columns X1-X31 and save the result as weatherTidy. 
#Finally, spread the measure column of weatherTidy and save the result as weatherTidy2
weatherTidy <- weather %>%  
        tidyr::gather(day, value, X1:X31, na.rm = TRUE)
weatherTidy2 <- weatherTidy %>% 
        tidyr::spread(measure, value)
weatherTidy2

### separating and uniting ###
table3 %>%
        tidyr::separate(rate, into = c("cases", "population"))

#By default, separate() will split values at non-alphanumeric characters (!number, !letter). 
#If you wish to use a specific character to separate a column, you can pass the character to the sep argument of separate().
table3 %>%
        tidyr::separate(rate, into = c("cases", "population"),
                        sep = "/")
# You can also pass a vector of integers to sep. separate() will interpret the integers as positions to split at.
table3 %>%
        tidyr::separate(year, into = c("century", "year"),
                        sep = 2)

# unite 
# unite() is the inverse of separate(): it combines multiple columns into a single column.
table5 %>%
        tidyr::unite(new, century, year)

# By default, unite() will place an underscore (_) between the values from different columns. need to specificy it 
table5 %>%
        tidyr::unite(new, century, year, sep = "")

# missing values 
stocks <- tibble(
        year = c(2015, 2015, 2015, 2015, 2016, 2016, 2016),
        qtr = c( 1, 2, 3, 4, 2, 3, 4),
        return = c(1.88, 0.59, 0.35, NA, 0.92, 0.17, 2.66)
)
# We can make the implicit missing value explicit by putting years in the columns:
stocks %>%
        tidyr::spread(year, return)
# You can also set na.rm = TRUE in gather() to turn explicit missing values implicit:
stocks %>%
        tidyr::spread(year, return) %>%
        tidyr::gather(year, return, `2015`:`2016`, na.rm = TRUE)

# You can also use complete() for making missing values explicit. complete() takes a set of columns, 
#and finds all unique combinations; filling in explicit NAs where necessary.
stocks %>%
        tidyr::complete(year, qtr)
# fill
#It takes a set of columns where you want missing values to be replaced by the most recent non-missing value
treatment <- tribble(
        ~ person, ~ treatment, ~response,
        "Derrick Whitmore", 1, 7,
        NA, 2, 10,
        NA, 3, 9,
        "Katherine Burke", 1, 4
)
treatment %>%
        tidyr::fill(person)
 

####################################################### Week 6  Homework ##################################################################################
library(tidyverse)
library(readr)
install.packages("gdata")
library(gdata)
library(xlsx)
setwd("~/Desktop/1.Data_Management_R/R_exercise/Week_6")

df <- read.xls("iiag.xls", sheet = 5,pattern = 6)
df1 <- read.xlsx("iiag.xls", sheetIndex = 5,startRow = 6)

# 1. Use gather() and spread() to transpose the dataframe so that you have one column  
# containing the headers V2:V111 and separate columns for each of the values in V1.

test1 <- df1 %>% 
        tidyr::gather(key = headers, value = value, V2:V111 , na.rm = FALSE) 
test2 <- test1 %>% 
        tidyr::spread(key = V1, value = value)

test3 <- test2 %>% 
        mutate(headers = parse_number(headers)) %>% arrange(headers) %>% select(-headers)
test4 <- test3 %>% 
        fill(Indicator)
test5 <- test4 %>% gather("Algeria": "Guinea-Bissau","Kenya":"Uganda","Zambia":"Zimbabwe", key = Country, value= Value, na.rm =FALSE)
final <- test5 %>% arrange(Country, Year, Indicator)


## Solutions from slides 
gather(variable,value,-V1) %>%
        spread(V1, value) %>%
        mutate(variable = readr::parse_number(variable)) %>%
        arrange(variable) %>%
        select(-variable) %>%
        fill(Indicator) %>%
        select(Indicator, Year, everything()) %>%
        gather(country, value, Algeria:Zimbabwe) %>%
        arrange(country, Year, Indicator) %>%
        filter(row_number() == 1518) %>%
        select(value)

####################################################### Week 7  Homework ##################################################################################
library(tidyverse)
library(dbplyr) #
install.packages("dbplyr")
library(DBI)
library(RSQLite) 
install.packages("RSQLite")
library(nycflights13)
library(readr)


## Final Exercise ## 

con<-dbConnect(SQLite(),dbname = "Week_7")

##apparently not very helpful week



####################################################### Week 8  ##################################################################################

library(tidyverse)
install.packages("stringr")
library(stringr)

## Exercise ##
stringr:: words
# start with y
start_with_y <- str_detect(words, "^y")
# or
str_view(stringr::words, "^y")
# End with “x”
end_with_x <- str_detect(words, "x$")
# or
str_view(stringr::words, "x$")
# start with a vowel 
str_subset(words, "^[aeiou].*[^aeiou]$") %>% head()
start_with_vowel <- str_detect(words, "^[aeiou]")
words[start_with_vowel] %>% head()
# ro
str_view(stringr::words, "^[aeiou]")
# that only contain consonants 
str_view(stringr::words, "^[^aeiou]+$", match=TRUE)
# end with ed, but not with eed 
str_view(stringr::words, "^ed$|[^e]ed$", match = TRUE)
# words ending in ing or ise 
str_view(stringr::words, "i(ng|se)$", match = TRUE)
# All words starting with three consonants
str_view(words, "^[^aeiou]{3}", match = TRUE)
#Three or more vowels in a row
str_view(words, "[aeiou]{3,}", match = TRUE)
#Two or more vowel-consonant pairs in a row
str_view(words, "([aeiou][^aeiou]){2,}", match = TRUE)
#Any two characters repeated
str_view(words, "(..).*\\1", match = TRUE)

# 2. Describe in words what these regular expressions match:
#2.2 ˆ.*$ ------> Any string
#2.3 "\\{.+\\}" ------> Any string with curly braces surrounding at least one character.
#2.4 \d{4}-\d{2}-\d{2} ---> A date in “%Y-%m-%d” format: four digits followed by a dash, followed by two digits followed by a
# dash, followed by another two digits followed by a dash.
#2.5 "\\\\{4}" ----> four backslashes
#2.6 (.)\1\1   ----> The same character apearing three times in a row
#2.7 "(.)(.)\\2\\1" -----> A pair of characters followed by the same pair of characters in reversed order


####################################################### Week 8 Homework ##################################################################################

library(stringr)
install.packages("qdapRegex")
library(qdapRegex)
library(tidyverse)

# question 1
d1 <- c("tom@hogwarts.com", 
        "tom.riddle@hogwarts.com",
        "tom@hogwarts.eu.com",
        "potter@hogwarts.com",
        "harry@hogwarts.com",
        "hermione+witch@hogwarts.com")
str_view(d1, "^^([\\w\\.]*)", match = TRUE)

# question 2 
d2 <- c("<a>This is a link</a>",
        "<a href='https://github.com'>Link</a>",
        "<div class='test_style'>Test</div>",
        "<div>Hello <span>world</span></div>")
l1 <- str_extract_all(d2, "<(.*?)>", simplify = TRUE)
substring(l1, 2, nchar(l1)-1)
l2 <- str_extract_all(d2, ">(.*?)<", simplify = TRUE)
substring(l2, 2, nchar(l2)-1)
l3 <- str_extract_all(d2, ":(.*?)>", simplify = TRUE)
substring(l3, 2, nchar(l3)-1)

# question 3 
d3 <- c(".bash_profile",
        "workspace.doc",
        "img0912.jpg",
        "updated_img0912.png",
        "documentation.html",
        "favicon.gif",
        "img0912.jpg.tmp",
        "access.lock")
d3[grep("(\\w+)\\.(jpg|png|gif)$", d3)]

# question 4
d4 <- c( "W/dalvikvm( 1553): threadid=1: uncaught exception",
         "E/( 1553): FATAL EXCEPTION: main",
         "E/( 1553): java.lang.StringIndexOutOfBoundsException",
         "E/( 1553):   at widget.List.makeView(ListView.java:1727)",
         "E/( 1553):   at widget.List.fillDown(ListView.java:652)",
         "E/( 1553):   at widget.List.fillFrom(ListView.java:709)")
str_view(d4, "(([\\w\\.])+):") # filename
str_view(d4, "(\\w+)\\(") # method
str_view(d4, ":(\\d+)\\)") # number 

# question 5 
d5 <- c("ftp://file_server.com:21/top_secret/life_changing_plans.pdf” “https://github.com/mhaber/slides#section",
        "file://localhost:4040/zip_file",
        "https://s3cur3-server.com:9999/",
        "market://search/angry%20birds")
str_view(d5, pattern = "(\\w+)://") # protocol 
str_view(d5, pattern = "://([\\w\\-\\.]+)") #host 
str_view(d5, pattern = "(:(\\d+))") #number 

####################################################### Week 9  ##################################################################################

library(tidyverse)
library(stringr)
library(rvest) #
install.packages("rvest")
library(twitteR) #
install.packages("twitteR")
library(streamR) #
install.packages("streamR")
library(RCurl) #
install.packages("RCurl")
library(ROAuth) #
install.packages("ROAuth")
library(httr) #
install.packages("httr")

# Basic worflow of scraping with rvest 
# 1. specify URL
"http://en.wikipedia.org/wiki/Table_(information)" %>%
        # 2. download static HTML behind the URL and parse it into an XML file
        read_html() %>%
        # 3. extract specific nodes with CSS (or XPath)
        html_node(".wikitable") %>%
        # 4. extract content from nodes
        html_table(fill = TRUE)


# Group Excercise 
url1 <- "http://www.biermap24.de/brauereiliste.php"
content <- read_html(url1)
anchors <- html_nodes(content, ".bgcolor7 > table td+ td a")
cities <- html_text(anchors)
cities <- str_trim(cities)
cities <- cities[!str_detect(cities, "\\(\\d+\\)")] 
length(unique(cities))
## [1] 806
cities <- data.frame(sort(table(cities)))

####################################################### Week 9 Homework  ##################################################################################
library(tidyverse)
library(stringr)
library(rvest) 
library(twitteR) 
library(streamR) 
library(RCurl) 
library(ROAuth) 
library(httr) 

# 1
url <- "https://www.jstatsoft.org/about/editorialTeam"
url_parsed <- read_html(url)
# 2
css <- "#group a"
html_nodes(url_parsed, css = css) %>% html_text
# 3
css_full_lines <- "#group li"
html_nodes(url_parsed, css = css_full_lines) %>%  html_text
# 4 
names <- html_nodes(url_parsed, css = css_full_lines)
html_text(names)
names<- names[grep("stati|math", names, ignore.case = TRUE)]
length(unique(names))


####### Solutions from Slides ################
url <- "https://www.jstatsoft.org/about/editorialTeam"
download.file(url, destfile = "data/editorial.html")
url_parsed <- read_html("data/editorial.html")
names <- html_nodes(url_parsed, "#group a") %>%
        html_text()
affiliations <- html_nodes(url_parsed, ".member li") %>%
        html_text() %>%
        str_replace("^[^,]*$", "") %>%   
        str_replace("^[^,]*,", "") %>% #### eliminate the names and just keep the affiliations 
        str_trim()
df <- data.frame(names, affiliations)
str_detect(affiliations, "tatisti|athemati") %>%
        table

################ a note on forms ########### 
url <-"https://connect.hertie-school.org/login/"
session <-html_session(url)
form <-html_form(session)[[1]]
username <- "haber.matthias@gmail.com"
password <- "HSOGDS2017!"
filled_form <- set_values(form,
                          `loginname` = username,
                          `loginpwd` = password)
img <- submit_form(session, filled_form) %>%
        jump_to("https://connect.hertie-school.org/directory/") %>%
        read_html() %>%
        html_nodes(".rounded") %>%
        html_attr("src") %>%
        .[1]
## Submitting with 'loginform'

####################################################### Week 10 e Week 11  ##################################################################################
#fatta con R markdown e shiny 

#############################################################################################################################################
#############################################################################################################################################
#############################################################################################################################################
#############################################################################################################################################














