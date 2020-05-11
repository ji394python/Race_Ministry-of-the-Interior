rm(list=ls())
library(shinydashboard)
library(shinydashboardPlus)
library(magrittr)
library(reshape2)
library(shiny)
library(tidyverse)
library(psych) #pairs.panels
library(ggpubr) #as_ggplot
library(gridExtra) #grid.arrange()
library(rvest)
library(stringr)
library(jiebaR)
library(tmcn)
library(car)
library(leaps)
library(randomForest)
library(rpart)
library(rpart.plot)
library(leaflet)
library(shinyWidgets)

# Cairo包的PNG设备似乎无法显示中文字符，强制使用R自身的png()设备
options(shiny.usecairo = FALSE)

# 请忽略以下代码，它只是为了解决ShinyApps上没有中文字体的问题
font_home <- function(path = '') file.path('~', '.fonts', path)
if (Sys.info()[['sysname']] == 'Linux' &&
    system('locate msjh.ttc') != 0 &&
    !file.exists(font_home('msjh.ttc'))) {
  if (!file.exists('msjh.ttc'))
    shiny:::download(
      'https://github.com/JohnHsiao/a200dv/blob/master/html/css/msjh.ttc?raw=true',
      'msjh.ttc'
    )
  print("OK")
  dir.create(font_home())
  file.copy('msjh.ttc', font_home())
  system2('fc-cache', paste('-f', font_home()))
}
rm(font_home)


