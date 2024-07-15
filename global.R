
library(shiny)
library(shinydashboard)
library (shinyjs)
library(shinycssloaders)
library(DT)
library(corrplot)
library(tidymodels)
library(caret)
library(ggrepel)  
library(ggplot2)
library(psych)
library(GGally)
library(Hmisc)
library(MASS)
library(tabplot)
library(reshape2)
library(dplyr)

# for geom_label_repel()
#set page length globally
# options(DT.options = list(pageLength = 20, language = list(search = 'Filter:')))
# 
# list.of.packages <- c("ggplot2",
#                       "DT",
#                       "GGally",
#                       "psych",
#                       "Hmisc",
#                       "MASS",
#                       "tabplot",
#                       "reshape2",
#                       "tidyverse")
# 
# new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# if(length(new.packages)) install.packages(new.packages)
# library(devtools)
# install_github("mtennekes/tabplot")

#
# # load all these
# lapply(list.of.packages, require, character.only = TRUE)