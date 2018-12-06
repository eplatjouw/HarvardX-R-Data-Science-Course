# Get working Directory
getwd()

# Change Working Directory
setwd(dir=)
# Unless a full path is given RStudo will look into the working directory

# After downloading/installing the dslabs package, files will be in
# the external data, extdata directory
path <-system.file("extdata", package= "dslabs")
path
# See the files in that directory
list.files(path)

# Move file in your working directory using file.copy()
# Define variable with full path using full.path() function
filename <- "murders.csv"
fullpath <- file.path(path, filename)
fullpath
file.copy(fullpath, getwd())
file.exists(filename)

#To know which kind of file you want to import, read first few lines in R
library(readr)
library(readxl)
read_lines("murders.csv", n_max=3)

dat <-read_csv(filename)
#or
dat <- read_csv(fullpath)

head(dat) #tibble with the content of the file

# R:BASE has similar function, we show the difference here (. instead of _)
dat2 <- read.csv(filename)
class(dat2) #data.frame
class(dat) #tiblle
class(dat2$region)#factor
class(dat$region)#character
#Avoided by adding argument
dat3 <- read.csv(filename, stringsAsFactors = FALSE)
class(dat3$abb)

#Read data from the internet
url <- "https://raw.githubusercontent.com/rafalab/dslabs/master/inst/extdata/murders.csv"
dat_url <- read_csv(url)

#If you want a local copy of the file
download.file(url, "murders.csv")

#Two functions usefull downloading data from the internet
  #tempdir() creates directory with name very unlikely not be unique
  #tempfile() creates character string, not a file, likely to be a unique file name
tempfile()

tmp_filename <- tempfile()
download.file(url, tmp_filename)
dat <- read_csv(tmp_filename)
file.remove(tmp_filename)
head(dat)


###Define tidy data
library(tidyverse)
library(dslabs)
data("gapminder")
tidy_data <- gapminder %>%
  filter(country %in% c("South Korea", "Germany")) %>%
  select(country, year, fertility)
head(tidy_data)

tidy_data %>%
  ggplot(aes(year, fertility, color = country)) +
  geom_point()

#This works seamlessly becaus the data is tidy
#Each row represents on observation and columns represent different variables that we have data on for observations
#Original gapminder data not so tidy

path <- system.file("extdata", package="dslabs")
filename <- file.path(path, "fertility-two-countries-example.csv")
wide_data <- read_csv(filename)

# contain the same data as tidy_dat but in a wide format
select(wide_data, country, '1960':'1967')

  #Wide format each row includes several information, one of the variable (year) is stored in the header
  #Previous code won't work anymore on this dat -> Wrangle data

#tidyr package includes several functions usefull for tidying data
  #gather() function converts y data into tidy data
    #First argument sets the name of the column that will hold the variable that is currently kept in the wide data column name
    #Second argument sets the column name for the column that will hold the values in the column cells
    #Third argument specifies the columns that will be gathered (default al columns)

new_tidy_data <- wide_data %>%
  gather(year, fertility, '1960':'2015')

head(new_tidy_data)

    # Note that First column is not gathered because we asked for all the other ones to be gathered
    # Quicker way is to specify which columns not to gather rather that what will be gathered
new_tidy_data <- wide_data %>%
  gather(year, fertility, -country)

head(new_tidy_data)

class(tidy_data$year)
class(new_tidy_data$year)
    #gather() function assumes column names are characters so more wrangling
    #Convert column as numbers
new_tidy_data <- wide_data %>%
  gather(year, fertility, -country, convert = TRUE)

class(new_tidy_data$year)

new_tidy_data %>%
  ggplot(aes(year, fertility, color = country)) +
  geom_point()

  #spread() function is basically the inverse of gather
    #First Argument: which variable will be used as the column names
    #Second Argument: which variable to use to fill out cells
    
new_wide_data <- new_tidy_data %>% spread(year, fertility)
select(new_wide_data, country, '1960':'1967')

#More realistic example
path <- system.file("extdata", package ="dslabs")
filename <- file.path(path,"life-expectancy-and-fertility-two-countries-example.csv")

raw_dat <- read_csv(filename)
select(raw_dat, 1:5)

dat <- raw_dat %>% gather(key, value, -country)
head(dat)

  #We want value from two variables as two seperate columns
  #First split the key column (they use an underscore)
dat$key[1:5]
    #Use function seperate() from readr package
      #First argument: name of the column to be seperated
      #Second argument: names to be used for the new columns
      #Third argument: character that seperates the variables

dat %>% separate(key, c("year", "variable_name"), "_")
  #or this as _ is default seperator
dat %>% separate(key, c("year" , "variable_name"))

  #PROBLEM: Warning message with too many ieces that are discarded
  #PROBLEM: life_expectancy truncated to life
    # One way is to create a seperate column and fill with missing values
dat %>% separate(key, c("year", "first_variable_name", "second_variable_name"), fill ="right")
    #Better approach: merge to last variables with an extra seperation
dat %>% separate(key, c("year", "variable_name"), sep ="_", extra = "merge")

  #Now we need to create a column for each variable using spread()
dat %>% separate(key, c("year", "variable_name"), sep ="_", extra = "merge") %>%
  spread(variable_name, value)

  #Building further on the first approach we can also next unite two columns into one using 
  #the unite() function and 
  # then spread() the column
  # then rename the fertility column
dat %>% separate(key, c("year", "first_variable_name", "second_variable_name"), fill ="right") %>%
  unite(variable_name, c("first_variable_name", "second_variable_name"), sep="_") %>%
  spread(variable_name, value) %>%
  rename(fertility = fertility_NA)

###Combining tables
  #Previously used left_join()
  
#Suppose we explore relationship between population size for US states
data(murders)
head(murders)
# and electoral votes
data("polls_us_election_2016")
head(results_us_election_2016)
  #we can not just join as order of states are not identical
identical(results_us_election_2016$state, murders$state)

  # join() function in dplyr, based on SQL joins, make sure that the tables are combined so that matching rows are together.
  #general idea is to identify one or more colimns that contain the info needed to match the two tables
tab <- left_join(murders, results_us_election_2016, by="state")
head(tab)
  #data succesfully joined and now make plot
library(ggrepel)
tab %>% ggplot(aes(population/10^6, electoral_votes, label = abb))+
  geom_point() +
  geom_text_repel()+
  scale_x_continuous(trans = "log2") +
  scale_y_continuous(trans = "log2") +
  geom_smooth(method = "lm", se = FALSE)

#In practice not always each row in one table matching row in the other
  #Illustration, using subset of our two tables
tab1 <- slice(murders, 1:6) %>% select(state, population)
tab1

tab2 <- slice(results_us_election_2016, c(1:3, 5, 7:8)) %>%
  select(state, electoral_votes)
tab2
    #different states
    #Explore different join functions
left_join(tab1,tab2)
tab1 %>% left_join(tab2) #Both the same and added NA were state differ

tab1 %>% right_join(tab2) # Rows of tab1 have NAs now

inner_join(tab1, tab2) #Keeps only rowns with info

full_join(tab1, tab2) # Keeps all the info with NAs

semi_join(tab1, tab2) #!Lets you keep part of the first table that has info in the second but does not add columns!

anti_join(tab1, tab2) #Opposite of semi_join(), keeps were no info available in second table


###Binding
  #Binding combines the data sets without trying to match a variable
  #Have to be same dimension

    #dplyr bind_cols() function binds two object by putting columns of each together in a tibble
bind_cols(a= 1:3, b = 4:6)
      #R:Based function cbind() does the same but creates objects other than tibbles
    #bind_cols() also binds data frames
tab1 <- tab[, 1:3]
tab2 <- tab[, 4:6]
tab3 <- tab[, 7:9]
new_tab <- bind_cols(tab1, tab2, tab3)
head(new_tab)

    #bind_rows() similar but binds rows instead of columns
tab1 <- tab[1:2,]
tab2 <- tab[3:4,]
bind_rows(tab1, tab2)
      #R:Based function rbind()

###Set Operators
  #The R:Based functions used on vectors but if the tidyverse, dplyr more specifically, is loaded also on data frames

    #intersect() function
intersect(1:10, 6:15) #numerical
intersect(c("a", "b", "c"), c("b", "c", "d")) #character
      #With dplyr, also for tables
      #Take intersection of rows for tables having same column names
tab1 <- tab[1:5,]
tab2 <- tab[3:7,]
intersect(tab1, tab2)

    # union() function similarly takes the union
union(1:10, 6:15) #numerical
union(c("a", "b", "c"), c("b", "c", "d")) #character

tab1 <- tab[1:5,]
tab2 <- tab[3:7,]
union(tab1, tab2)

    # setdiff() function sets differences
      #unlike intersect and union the function is not symmetric
setdiff(1:10, 6:15)
setdiff(6:15, 1:10)

tab1 <- tab[1:5,]
tab2 <- tab[3:7,]
setdiff(tab1, tab2)

    # setequal() function tells if 2 sets are the same regardless of order
setequal(1:5, 1:6)
setequal(1:5, 5:1)

      # provides usefull info in why tables are not equal in dplyr
setequal(tab1, tab2)


### Web Scraping
  ##Extracting Data from a Website
  ##Information used by browser to render web pages is received as text from server
  ##in HyperText Markup Language or HTML
  ## In Chrome, view source code with View Page Source button
  ## rvest package from tidyverse

library(rvest)
url <- "https://en.wikipedia.org/wiki/Murder_in_the_United_States_by_state"
h <- read_html(url)
class(h) #XML is general markup language

h #not much to see
  # Info to extract is in HTML table as seen in a specific line in the source code
  # messages between two symbols <> in html. <> are nodes
  # rvest package has functions to extract nodes from HTML documents
    
    # html_nodes() function extract all nodes of a given type
    # html_node() function extract just the first node of that type
tab <- h %>% html_nodes("table")
tab <- tab[[2]]
tab
    # html_table() function converts HTML tables into data frames
tab <- tab %>% html_table()
class(tab)
tab

    #Change names columns
tab <- tab %>% setNames(c("state", "population", "total", "murders", "gun_murders", "gun_ownership", "total_rate", "murder_rate" , "gun_murder_rate"))
head(tab)
 
  #Web Scraping example on guacamole recipe using SelectorGadget tool

h <- read_html("http://www.foodnetwork.com/recipes/alton-brown/guacamole-recipe-1940609")

recipe <- h %>% 
  html_node(".o-AssetTitle__a-HeadlineText") %>% 
  html_text()

prep_time <- h %>% 
  html_node(".m-RecipeInfo__a-Description--Total") %>% 
  html_text()

ingredients <- h %>% 
  html_nodes(".o-Ingredients__a-Ingredient") %>% 
  html_text()


guacamole <- list(recipe, prep_time, ingredients)

guacamole

#Since recipe pages from this website follow this general layout, we can use this code to create a function that extracts this information:
get_recipe <- function(url){
  h <- read_html(url)
  recipe <- h %>% html_node(".o-AssetTitle__a-HeadlineText") %>% html_text()
  prep_time <- h %>% html_node(".o-RecipeInfo__a-Description--Total") %>% html_text()
  ingredients <- h %>% html_nodes(".o-Ingredients__a-ListItemText") %>% html_text()
  return(list(recipe = recipe, prep_time = prep_time, ingredients = ingredients))
}

#and then use it on any of their webpages:
  
  get_recipe("http://www.foodnetwork.com/recipes/food-network-kitchen/pancakes-recipe-1913844")
  
  

### String Processing
  ## String Parsing
library(rvest)
url <- "https://en.wikipedia.org/wiki/Murder_in_the_United_States_by_state"
murders_raw <- read_html(url) %>%
  html_nodes("table") %>%
  html_table()
murders_raw <- murders_raw[[2]] %>%
  setNames(c("state", "population", "total", "murders", "gun_murders", "gun_ownership", "total_rate", "murder_rate", "gun_murder_rate"))
head(murders_raw)
      #two of the columns contain characters instead of numbers
class(murders_raw$population)
class(murders_raw$total)
      # parse_number() function converts numbers written with comma into numbers written without

  ## Defining strings in R: with " " and ' ' but not with ` `
s <- "Hello!"
s <- 'Hello!'
s <- ´Hello!´
s

  # What happens if you want to use " within a string?
  # use single quotes 
s <- '10"'
cat(s) # to make sure it works and see how it actually looks like
  #Same other way around
s <-"5'"
cat(s)

  # What if we want to write 5'10", neither " " nor ' ' will work
  # We need to escape the quotes using /
s <- '5\'10"'
  #or
s <- "5'10\""
cat(s)

  ## stringr Package
  #In General, string processing involves a string and a pattern
    #After web scraping, population column has a character vector
library(rvest)
url <- "https://en.wikipedia.org/wiki/Murder_in_the_United_States_by_state"
murders_raw <- read_html(url) %>%
  html_nodes("table") %>%
  html_table()
murders_raw <- murders_raw[[2]] %>%
  setNames(c("state", "population", "total", "murders", "gun_murders", "gun_ownership", "total_rate", "murder_rate", "gun_murder_rate"))
head(murders_raw)

class(murders_raw$population)

murders_raw$population[1:3]
as.numeric(murders_raw$population[1:3]) # usual coercien doesn't work because of the commas
    #remove commas
    #Generally, processing tasks devided in detecting, locating, extracting or replacing patterns in strings
      # Here locate commas and replace with empty
      #Base:R has functions but not unifying convention
      # stringr package repackages this in more consistent approach
library(stringr)

    #Detect commas with str_detect() functions
commas <- function(x) any(str_detect(x, ","))
murders_raw %>% summarize_all(funs(commas))

    # Remove them with str_replace_all() function to remove them
test_1 <- str_replace_all(murders_raw$population, ",", "")
class(test_1)
test_1 <-as.numeric(test_1)
    #Use mutate_all() to apply this operation to all columns as it won't affect columns without commas
    #Operation so common that readr  includes function 
    # parse_number() meant to remove non-numeric characters before coercing
test_2 <- parse_number(murders_raw$population)
class(test_2)
identical(test_1, test_2)

    # Obtain desired table using following code
murders_new <- murders_raw %>% mutate_at(2:3, parse_number)
murders_new %>% head


  ##Case Study 2: Reported Heights
library(tidyverse)
library(dslabs)
data("reported_heights") # raw data, obtained from web form filled in by students

class(reported_heights$height) #Student could enter anything but were asked height in inches

x <- as.numeric(reported_heights$height) # warning NAs introduced
head(x)
sum(is.na(x)) #81 NAs

reported_heights %>% mutate(new_height = as.numeric(height)) %>%
  filter(is.na(new_height)) %>% #check out some unsuccesfull entries
  head(n=10)

    # Identify large groups/patterns that can be rectified with a single piece of code
    # To keep an overview write function to view only the problematic entries
    # or outside of acceptable heights range
    #surpressWarnings() function to surpress warnings the as.numeric() function will give us
not_inches <- function(x, smallest = 50, tallest = 84){
  inches <- suppressWarnings(as.numeric(x))
  ind <- is.na(inches) | inches < smallest | inches > tallest
  ind
}
  
    #Apply function
problems <- reported_heights %>%
  filter(not_inches(height)) %>%
  .$height

length(problems)
problems

    #Identify 3 main patterns 
    # 1: x' y or x' y"" or x' y\""
pattern <- "^\\d\\s*'\\s*\\d{1,2}\\.*\\d*'*\"*$"
str_subset(problems, pattern) %>% head(n=10) %>% cat    
    # 2: x.y or x,y
pattern <- "^[4-6]\\s*[\\.|,]\\s*([0-9]|???10|11)$"
str_subset(problems, pattern) %>% head(n=10) %>% cat   
    # 3: x in cm
ind <- which(between(suppressWarnings(as.numeric(problems))/2.54, 54, 81))
ind <- ind[!is.na(ind)]
problems[ind] %>% head(n=10) %>% cat


##Regular expressions or regex
  # Make a script that makes web-based data collection methods robust to the most common user mistakes
  # Use technique that enables us to detect patterns and extract parts we want
  #   =regex. Regex is a way to descrive specific patterns of a character of ttext that can
  #           be used to determine if a given string matches the pattern
    #Set of rules have been defined for this or also cheatsheets
    # Technically any string is a regex
    # stringr package
library(stringr)
pattern <- ","
str_detect(murders_raw$total, pattern) # simple example search with regex

str_subset(reported_heights$height, "cm")

    #More Complicated example. Yes = define as satisfy pattern, otherwise no
yes <- c("180 cm", "70 inches")
no <- c("180", "70''")
s <- c(yes, no)

str_detect(s, "cm") | str_detect(s, "inches") #contain cm or inches?

  # regex language distinguish from plain strings because we can use special characters with meaning
  # | means or
str_detect(s, "cm|inches")
  # \d means any digits (0-9)
    # in R we have to escape the backslash so \\d instead
yes <- c("5", "6", "5'10", "5 feet", "4'11")
no <- c("", ".", "Five", "six")
s <- c(yes,no)
pattern <- "\\d"
str_detect(s, pattern)

  # str_view() function, helpful for troubleshooting
    # Shows first match for each string
install.packages("htmlwidgets")
library("htmlwidgets")

str_view(s, pattern)

  # str_view_all() function shows us all the matches
str_view_all(s, pattern)


### Character Classes, Anchors and Qualifiers
  ## Character classes are used to define a series of characters that can be matched
  ## Character Classes use []
str_view(s, "[56]") #only detects 5 and 6's

yes <- as.character(4:7)
no <- as.character(1:3)
s <- c(yes, no)
str_detect(s, "[4-7]") # character range
str_view(s, "[4-7]")
    # NOTE there are NO NUMBERS but CHARACTERS
    # [1-20] means 1 to 2 and then the character 0; so 0, 1, 2
yes <- as.character(1:2)
no <- as.character(3:20)
s <- c(yes, no)
str_detect(s, "[1-20]")
str_view_all(s,"[1-20]")

    #They still have the order 
    #Same  for letters [a-z] is alphabet but lowercase
    #[A-Z] alphabet but uppercase


  # Pattern to match when exactly one digit?

  ## Anchors define patterns that must start or end at specific places
  ## Most common ^ and $
  ## ^ beginning and $ end of a string
    # Example ^\\d$ string with 1 digit
pattern <- "^\\d$"
yes <- c("1", "5", "9")
no <- c("12", "123", " 1", "a4", "b")
s <- c(yes, no)
str_view(s, pattern)


  # Pattern on or two digits?
  
  ## Quantifiers say number of times the previous entry repeats 
  ## using {}
    # example ^\\d{1,2}$ one or two digits
pattern <- "^\\d{1,2}$"
yes <- c("1", "5", "9", "12")
no <- c("123", "a4", "b")
str_view(c(yes, no), pattern)

    # to look for one ' and " patterns just add the symbols
pattern <- "^[4-7]'\\d{1,2}\"$"

    #Test
yes <- c("5'7\"", "6'2\"", "5'12\"")
no <- c("6,2\"", "6.2\"", "I am 5'11\"", "3'2\"", "64")
str_detect(yes, pattern)
str_detect(no, pattern)


### Search and Replace with regex
pattern <- "^[4-7]'\\d{1,2}\"$"
sum(str_detect(problems, pattern)) # only 14 match the pattern defined

problems[c(2, 10, 11, 12, 15)] %>% str_view(pattern) # some wrote out the feet and inches

str_subset(problems, "inches")
str_subset(problems, "''")
    # To solve this we can replace the different ways of representing
      # We will us single quote ' for feet and nothing for inches
      # 5'y so change pattern accordingly
pattern <- "^[4-7]'\\d{1,2}$"

problems %>%
  str_replace("feet|ft|foot", "'") %>% # str_replace() to replace
  str_replace("inches|in|''|\"", "") %>%
  str_detect(pattern) %>%
  sum

      # other proble are spaces
      # spaces are characters and R does not ignore them
      identical("Hi", "Hi ")
      # \s is space so \\s to escape backslash
pattern_2 <- "^[4-7]'\\s\\d{1,2}\"$"
str_subset(problems, pattern_2)

      #Do we need more than one regex pattern?
      # No we can use quantifiers
      # * means 0 or more instances of the previous character
yes <- c("AB", "A1B", "A11B", "A111B", "A1111B")
no <- c("A2B", "A21B")
s <-c(yes, no)
str_detect(s, "A1*B")
      # use * after \\s

    # ? means none or once
    # + means one ore more
data.frame(string =c("AB", "A1B", "A11B", "A111B", "A1111B"),
           none_or_more =str_detect(yes, "A1*B"),
           none_or_once =str_detect(yes, "A1?B"),
           once_or_more =str_detect(yes, "A1+B"))

    #Add that stuff
pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
problems %>%
  str_replace("feet|ft|foot", "'") %>% # replace feet, ft, foot with '
  str_replace("inches|in|''|\"", "") %>% # remove all inches symbols
  str_detect(pattern) %>%
  sum


### Groups with Regex
  ## Next common mistake is x.y or x,y, and x y
  ## change into x'y
  ## But not 70.5 into 70'5 so look for very specific patterns
  
  ## Groups pwerfull aspect of regex that permit extraction of values
  ## Groups are defined by parentheses ()
    #First digit between 4-7: [4-7]
    #Second be none or more digits \\d*
pattern_without_groups <- "^[4-7],\\d*$"
    # we want to extract digits to form new version with single quote
    # So we encapsulate
pattern_with_groups <- "^([4-7]),(\\d*)$"

  ## Adding groups does not affect detection
pattern_with_groups <- "^([4-7]),(\\d*)$"
yes <- c("5,9", "5,11", "6,", "6,1")
no <- c("5'9", ",", "2,8", "6.1.1")
s <- c(yes, no)
str_detect(s, pattern_without_groups)
str_detect(s, pattern_with_groups)

  ## str_match() function extracts values these groups define
str_match(s, pattern_with_groups)

  ## Difference between str_extract() and str_match()
  ## str_extract() only extracts strings that match a pattern, not values defined by the groups
str_extract(s, pattern_with_groups)

  ## You can refer to the extracted value in regex when searching and replacing
  ## using \\i as in the i-th group
    ## \\1 is the value from the first group and \\2 the value from the second group
pattern_with_groups <- "^([4-7]),(\\d*)$"
yes <- c("5,9", "5,11", "6,", "6,1")
no <- c("5'9", ",", "2,8", "6.1.1")
s <- c(yes, no)
str_replace(s, pattern_with_groups, "\\1'\\2") #Will replace comma by a period but only when between two digits

  ## New pattern that is more flexible and emcompasses all the new groups
pattern_with_groups <- "^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$"

str_subset(problems, pattern_with_groups) %>% head


str_subset(problems, pattern_with_groups) %>% 
  str_replace(pattern_with_groups, "\\1'\\2") %>% head # Still the problem of the 25


### Testing and Improving

not_inches_or_cm <- function(x, smallest = 50, tallest = 84) {
  inches <- suppressWarnings(as.numeric(x))
  ind <- !is.na(inches) &
    ((inches >= smallest & inches <= tallest) |
       (inches/2.54 >= smallest & inches/2.54 <= tallest))
  !ind
}


problems <- reported_heights %>%
  filter(not_inches_or_cm(height)) %>%
  .$height
length(problems)

converted <- problems %>%
  str_replace("feet|foot|ft", "'") %>% #convert feet symbols to '
  str_replace("inches|in|''|\"", "") %>% # remove inches symbols
  str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2") ## change format

pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
index <- str_detect(converted, pattern)
mean(index)

converted[!index] #examine remaining cases


  ## Replacing no inches with '0
yes <- c("5", "6", "5")
no <- c("5'", "5''", "5'4")
s <- c(yes, no)
str_replace(s, "^([4-7])$", "\\1'0")

str_replace(s, "^([4-7])'?$", "\\1'0") # include ' with ?=none or one special character

pattern <- "^[4-7]\\s*'\\s*(\\d+\\.?\\d*)$" #allow the second group to include decimals and not just digits.

yes <- c("1,7", "1, 8", "2, " ) # meters using commas into x.y or x'y
no <- c("5,8", "5,3,2", "1.7")
s <- c(yes, no)
str_replace(s, "^([12])\\s*,\\s*(\\d*)$", "\\1\\.\\2")

  ## Trimming: remove spaces in front and end of strings
  ## str_trim()  
s <- "Hi "
cat(s)
identical(s, "Hi")
identical(str_trim(s), "Hi")
str_trim("5 ' 9 ")

  ## To upper and lower case
s <- c("Five feet eight inches")
str_to_lower(s)

  ## Putting into a function
convert_format <- function(s){
  s %>%
    str_replace("feet|foot|ft", "'") %>% #convert feet symbols to '
    str_replace_all("inches|in|''|\"|cm|and", "") %>%  #remove inches and other symbols
    str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2") %>% #change x.y, x,y x y
    str_replace("^([56])'?$", "\\1'0") %>% #add 0 when to 5 or 6
    str_replace("^([12])\\s*,\\s*(\\d*)$", "\\1\\.\\2") %>% #change european decimal
    str_trim() #remove extra space
}

  ## Also write function that converts words into numbers
words_to_numbers <- function(s){
  str_to_lower(s) %>%  
    str_replace_all("zero", "0") %>%
    str_replace_all("one", "1") %>%
    str_replace_all("two", "2") %>%
    str_replace_all("three", "3") %>%
    str_replace_all("four", "4") %>%
    str_replace_all("five", "5") %>%
    str_replace_all("six", "6") %>%
    str_replace_all("seven", "7") %>%
    str_replace_all("eight", "8") %>%
    str_replace_all("nine", "9") %>%
    str_replace_all("ten", "10") %>%
    str_replace_all("eleven", "11")
}

  ## Which problematic entries remain?
converted <- problems %>% words_to_numbers %>% convert_format
remaining_problems <- converted[not_inches_or_cm(converted)]
pattern <- "^[4-7]\\s*'\\s*\\d+\\.?\\d*$"
index <- str_detect(remaining_problems, pattern)
remaining_problems[!index]


### Separate with Regex
## Extract and save the Feet and Number value to convert them in inches appropriatemy

s <- c("5'10", "6'1")
tab <- data.frame(x = s)
tab
tab %>% separate(x, c("feet", "inches"), sep ="'")
  # same as extract() in tidyr package
tab %>% extract(x, c("feet", "inches"), regex = "(\\d)'(\\d{1,2})")
  # extract is more flexible
s <- c("5'10", "6'1\"", "5'8inches")
tab <- data.frame(x = s)
tab %>% separate(x, c("feet", "inches"), sep ="'", fill = "right") #Fails
tab %>% extract(x, c("feet", "inches"), regex = "(\\d)'(\\d{1,2})")


  ##### All together
pattern <- "^([4-7])\\s*'\\s*(\\d+\\.?\\d*)$"

smallest <- 50
tallest <- 84
new_heights <- reported_heights %>% 
  mutate(original = height, 
         height = words_to_numbers(height) %>% convert_format()) %>%
  extract(height, c("feet", "inches"), regex = pattern, remove = FALSE) %>% 
  mutate_at(c("height", "feet", "inches"), as.numeric) %>%
  mutate(guess = 12*feet + inches) %>%
  mutate(height = case_when(
    !is.na(height) & between(height, smallest, tallest) ~ height, #inches 
    !is.na(height) & between(height/2.54, smallest, tallest) ~ height/2.54, #centimeters
    !is.na(height) & between(height*100/2.54, smallest, tallest) ~ height*100/2.54, #meters
    !is.na(guess) & inches < 12 & between(guess, smallest, tallest) ~ guess, #feet'inches
    TRUE ~ as.numeric(NA))) %>%
  select(-guess)

  # We can check all the entries we converted using the following code:
  
  new_heights %>%
  filter(not_inches(original)) %>%
  select(original, height) %>% 
  arrange(height) %>%
  View()
  
  # Let's take a look at the shortest students in our dataset using the following code:

new_heights %>% arrange(height) %>% head(n=7)


### String Splitting
  ## Read suppose read csv with base R function readLines()
filename <- system.file("extdata/murders.csv", package ="dslabs")
lines <- readLines(filename)

lines %>% head()

  # Extract values separated by commas
  # str_split() function
x <- str_split(lines, ",")
x %>% head()

  #First entry has column name
col_names <-x[[1]]
x <- x[-1]

  ## Convert list into data frame
  ## map() function in purrr package
    # Applies same function to each element in a list
library(purrr)
map(x, function(y) y[1]) %>% head()

    # Common task, so shortcut
    # If second argument instead of function receives integer, assumes we want that
map(x,1) %>% head()

    # Force map to return character vector instead of a list
    # map_chr()
    # Similarly map_int returns integers

  ## Create Data frame
dat <- data.frame(map_chr(x, 1),
                  map_chr(x, 2),
                  map_chr(x, 3),
                  map_chr(x, 4),
                  map_chr(x, 5)) %>%
  mutate_all(parse_guess) %>%
  setNames(col_names)

  # Same ut more efficient using purrr package
dat <- x %>%
  transpose() %>%
  map( ~ parse_guess(unlist(.))) %>%
  setNames(col_names) %>%
  as.data.frame()
dat %>% head

  # Even avoid all this with str_split() function as it has argument called simplify = TRUE
x <- str_split(lines, ",", simplify = TRUE)
col_names <- x[1,]
x <- x[-1,]
x %>% as_data_frame() %>%
  setNames(col_names) %>%
  mutate_all(parse_guess)


### Case Study: Extracting Table from PDF
library(dslabs)
data("research_funding_rates")
research_funding_rates  #From pdf url = http://www.pnas.org/content/112/40/12349.abstract

  # Downloading data
install.packages("pdftools")
library("pdftools")
temp_file <- tempfile()
url <- "http://www.pnas.org/content/suppl/2015/09/16/1510159112.DCSupplemental/pnas.201510159SI.pdf"
download.file(url, temp_file)
txt <- pdf_text(temp_file)
file.remove(temp_file)

  # If we examine the object text we notice that it is a character vector with an entry for each page. 
  # So we keep the page we want using the following code:
raw_data_research_funding_rates <- txt[2]

  # Steps can be skipped as raw data included in dslabs
data("raw_data_research_funding_rates")

  # Examine
raw_data_research_funding_rates %>% head

  # Long String with each line, including table rowsn seperated by symbol \n for newline
tab <- str_split(raw_data_research_funding_rates, "\n")

  #start off with just one element in the string, we end up with a list with just one entry:
tab <- tab[[1]]
tab %>% head

  # Info column names third and forth entries
the_names_1 <- tab[3]
the_names_2 <- tab[4]

  # Extract table data
the_names_1

    #We want to remove the leading space and everything following the comma. 
    #We can use regex for the latter. 
    #obtain the elements by splitting using the space. 
    #We want to split only when there are 2 or more spaces to avoid splitting success rate. 
the_names_1 <- the_names_1 %>%
  str_trim() %>%
  str_replace_all(",\\s.", "") %>%
  str_split("\\s{2,}", simplify = TRUE)
the_names_1
    
    #Now let's look at the second line:

the_names_2
    #Here we want to trim the leading space and then split by space as we did for the first line:

the_names_2 <- the_names_2 %>%
  str_trim() %>%
  str_split("\\s+", simplify = TRUE)
the_names_2

  #Now we can join these to generate one name for each column:

tmp_names <- str_c(rep(the_names_1, each = 3), the_names_2[-1], sep = "_")
the_names <- c(the_names_2[1], tmp_names) %>%
  str_to_lower() %>%
  str_replace_all("\\s", "_")
the_names

  # Now we are ready to get the actual data. 
  # By examining the tab object, we notice that the information is in lines 6 through 14.
  # We can use str_split again to achieve our goal:

new_research_funding_rates <- tab[6:14] %>%
  str_trim %>%
  str_split("\\s{2,}", simplify = TRUE) %>%
  data.frame(stringsAsFactors = FALSE) %>%
  setNames(the_names) %>%
  mutate_at(-1, parse_number)
new_research_funding_rates %>% head()

  #We can see that the objects are identical:

identical(research_funding_rates, new_research_funding_rates)


### Recoding
  ## Recoding long names into smaller ones for plots and stuff
  ## Using case_when()
  ## Tidyverse specific designed function recode()
library(dslabs)
data("gapminder")

gapminder %>%
  filter(region == "Caribbean") %>%
  ggplot(aes(year, life_expectancy, color = country)) +
  geom_line()
    # Too much plot space for long country names
gapminder %>%
  filter(region == "Caribbean") %>%
  filter(str_length(country) >= 12) %>%
  distinct(country)

  # Change it for every year in the whole dataset
gapminder %>% filter(region == "Caribbean") %>%
  mutate(country = recode(country,
                          'Antigua and Barbuda' ="Barbuda",
                          'Dominican Republic' ="DR",
                          'St. Vincent and the Grenadines' = "St. Vincent",
                          'Trinidad and Tobago' = "Trinidad")) %>%
  ggplot(aes(year, life_expectancy, color = country)) +
  geom_line()

    # Similar functions in the forcats function in tidyverse
    # recode_factor() and fct_recoder()
