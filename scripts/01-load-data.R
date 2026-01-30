library(here)
library(tidyverse)
library(lubridate)


sqf_2006 <- read_csv(here("data", "raw", "2006.csv"), col_types = cols(.default = col_character()))
sqf_2007 <- read_csv(here("data", "raw", "2007.csv"), col_types = cols(.default = col_character()))
sqf_2008 <- read_csv(here("data", "raw", "2008.csv"), col_types = cols(.default = col_character()))
sqf_2009 <- read_csv(here("data", "raw", "2009.csv"), col_types = cols(.default = col_character()))
sqf_2010 <- read_csv(here("data", "raw", "2010.csv"), col_types = cols(.default = col_character()))
sqf_2011 <- read_csv(here("data", "raw", "2011.csv"), col_types = cols(.default = col_character()))
sqf_2012 <- read_csv(here("data", "raw", "2012.csv"), col_types = cols(.default = col_character()))

