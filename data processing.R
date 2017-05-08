#### package.require #####
#### @param: packages install and load package ####
package.require <- function(packages){
  package.local <- as.data.frame(installed.packages()[,c("Package","Version")])
  package.need.install <- setdiff(packages,
                                  as.character(package.local$Package))
  if(length(package.need.install) != 0){
    install.packages(package.need.install)
  }
  for(package in packages){
    require(package,character.only = TRUE)
  }
}

package.require(c("dplyr", "foreign", "readr", "data.table", "lubridate"))


#### get all the file directory in fire ####
path <- paste0(getwd(),"/",unlist(lapply(list.dirs("Fire"), function(x) paste0(x,"/",list.files(x)))))

#######                     fire.read                   #########
####### @param: path.dir the file path list or vector   #########
####### return: the list of data.table                  #########
fire.read <- function(path.dir){
  df.list <- list()
  file.n <- length(path.dir)
  year <-  sapply(path.dir, function(x) unlist(strsplit(x,"/"))[4])
  for(i in 1:file.n){
    cat(paste0("File in ", path.dir[i]," is reading"),"\n")
    # if the file extension is dbf, use the read.dbf to read file
    if(grepl("dbf", tolower(path.dir[i])))
      ds <- data.table(read.dbf(path.dir[i], as.is=TRUE))
    else
      # else use the readr to read file
      ds <- read_delim(path.dir[i], delim = "^", col_names = TRUE)
    colnames(ds) <- tolower(colnames(ds))
    df.list[[i]] <- ds
  }
  # name list using year
  names(df.list) <- year
  gc()
  return(df.list)
}

#### read all the codelookup, and add variable year  ####
codelookup <- fire.read(path[grepl("codelookup", path)])
codelookup <- lapply(2006:2015, function(x)
  codelookup[[x-2005]] %>% mutate(year = x)) %>%
  rbindlist

####                 key.transoform                      #####
#### THe function to make the primary key unified format ####
key.transform <- function(df){
  df %>%
    mutate(fdid = as.numeric(fdid)) %>%
    mutate(inc_date = as.numeric(inc_date)) %>%
    mutate(inc_no = as.numeric(inc_no)) %>%
    mutate(exp_no = as.numeric(exp_no))
}
####                 inc_date.transfrom                    ####
#### To derive the year, month, day, weekday from inc_date ####
inc_date.transfrom <- function(df){
  df %>%
    mutate(inc_date = mdy(ifelse(nchar(inc_date) == 7, paste0(0,inc_date),as.character(inc_date)))) %>%
    mutate(year = as.integer(year(inc_date))) %>%
    mutate(month = month(inc_date,label = TRUE, abbr = TRUE)) %>%
    mutate(day = day(inc_date)) %>%
    mutate(weekday = wday(inc_date, label = TRUE, abbr = FALSE))
}
####              basicincident.stat                   #####
####       calculate summarized statistics             #####
basicincident.stat <- function(df) {
  df %>%
    mutate(hh = as.integer(as.numeric(alarm)%%10^4/100)) %>%
    group_by(state, year, month, weekday, day, hh, inc_type) %>%
    summarise(n = n(),
              ff_death = sum(ff_death, na.rm = TRUE),
              oth_death = sum(oth_death, na.rm = TRUE),
              ff_inj = sum(ff_inj, na.rm = TRUE),
              oth_inj = sum(oth_inj, na.rm = TRUE))
}


basicinciden.list <- fire.read(path[grepl("basicincident", path)]) %>%
  lapply(function(df) df %>% 
           select(state, fdid, inc_date, inc_no, exp_no, inc_type, alarm, ff_death, oth_death, ff_inj, oth_inj) %>% key.transform %>% inc_date.transfrom
           )




#### get the description of inc_type in the codelookup #### 
inc_type.convert.df <- codelookup %>%
  filter(fieldid == "INC_TYPE") %>%
  mutate(inc_type = as.numeric(code_value)) %>%
  select(-fieldid, -code_value)

load("fifty_states.RData")
  
state.position <- fifty_states %>%
  dplyr::group_by(id) %>%
  summarise(latitude = mean(lat), longitude  = median(long))

state_code <- codelookup %>%
  filter(fieldid == "STATE" & !is.na(code_value)) %>%
  mutate(state = code_value)  %>%
  select(state, code_descr) %>%
  unique

state.df <- Fire.Stat %>%
  group_by(state, year) %>%
  summarise(sum = sum(n)) %>%
  inner_join(state_code) %>%
  ungroup %>%
  mutate(id = tolower(code_descr), 
         state = code_descr) %>%
  inner_join(state.position)

save(state.df, file = "state.df")
####       calculate the  number of fires with year     #####
Fire.Stat <- basicinciden.list %>%
  lapply(function(df) df %>% basicincident.stat) %>%
  rbindlist %>%
  mutate(inc_type = as.numeric(inc_type)) %>%
  inner_join(inc_type.convert.df, by = c("inc_type", "year"))
  
####       Calculate the number of occurrences of different fires per year     #####
Fire.year.stat <- Fire.Stat %>%
  group_by(year, inc_type, code_descr) %>%
  summarise(n = n())
#### save the result into .RData  ####
save(Fire.year.stat, file = "Fire.year.stat.RData")

####    Calculate the number of occurrences of different fires per hour     #####
Fire.time.stat <- Fire.Stat %>%
  group_by(year, hh, inc_type, code_descr) %>%
  summarise(n = n())
#### save the result into .RData  ####
save(Fire.time.stat, file = "Fire.time.stat.RData")

#### read teh hazmat information ####
hazmat <- fire.read(path[grepl("hazmat", path) & !grepl("hazmatequipinvolved", path)]) %>%
  lapply(function(df) df %>% key.transform %>% inc_date.transfrom)

  
for(i in 1:10){
  hazmat[[i]] <- hazmat[[i]] %>%
    inner_join(basicinciden.list[[i]] %>% 
                 select(state, fdid, inc_date, inc_no, exp_no, inc_type) %>%
                 mutate(inc_type = as.numeric(inc_type))) %>% 
    inner_join(inc_type.convert.df, by = c("inc_type", "year"))
}

####    Calculate the number of occurrences of person death in different types of fires per hour     #####
death.month.week.stat <- hazmat %>%
  rbindlist %>%
  group_by(month, weekday, inc_type, code_descr) %>%
  summarise(death.num = sum(haz_death, na.rm = TRUE))

death.month.week.stat <- within(
  death.month.week.stat,{
    month <- factor(as.character(month), c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
    weekday <- factor(as.character(weekday), c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
  }
)

save(death.month.week.stat, file = "death.month.week.stat.RData")

death.fire <- hazmat %>%
  rbindlist %>% 
  group_by(year, inc_type, code_descr) %>%
  summarise(death.num = sum(haz_death, na.rm = TRUE)) %>%
  filter(death.num > 0)


save(death.fire, file = "death.fire.RData")
gc()
save.image()
