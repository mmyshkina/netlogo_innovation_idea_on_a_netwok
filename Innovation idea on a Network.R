# почта для связи: momyshkina@edu.hse.ru

library(ggplot2)
library(plotly)
library(dplyr)
library(here)

# путь в текущую папку ----
script.dir <- paste(here(), gsub(".*Documents/", "\\1", dirname(rstudioapi::getSourceEditorContext()$path)), sep = "/")
script.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(script.dir)
getwd()

# НАЧАЛЬНЫЕ УСЛОВИЯ ------
#  Number of nodes ---------------
data <- read.csv2("Innovation on a Network number-of-nodes 5 5 300-table.csv", sep = ",", skip = 6) %>% 
  dplyr::mutate(success = ifelse(`count.turtles.with..infected..` == 0, 0, ifelse(`X.step.` == 3000, 0, 1))) 
# colnames(data)
d_plot <- data %>% dplyr::select("success", "number.of.nodes") %>% dplyr::mutate(count = c(1)) %>% 
  dplyr::group_by(`number.of.nodes`) %>% summarise_all(,.funs = sum) %>% 
  dplyr::mutate(prob = success/count*100)

fig <- plot_ly(d_plot, 
               x = ~`number.of.nodes`,
               y = ~prob,
               type = "bar",
               text = ~prob #textposition = 'bottom center'
) %>% layout(xaxis = list(title = 'Number of nodes / 
                          Численность сотрудников организации',dtick = 10),
             yaxis = list(title = 'Доля успешных кейсов, %' )
)
fig

#  average-node-degree ---------------
data <- read.csv2("Innovation on a Network average-node-degree 1 1 49-table.csv", sep = ",", skip = 6) %>% 
  dplyr::mutate(success = ifelse(`count.turtles.with..infected..` == 0, 0, ifelse(`X.step.` == 3000, 0, 1))) 
# colnames(data)
d_plot <- data %>% dplyr::select("success", "average.node.degree") %>% dplyr::mutate(count = c(1)) %>% 
  dplyr::group_by(`average.node.degree`) %>% summarise_all(,.funs = sum) %>% 
  dplyr::mutate(prob = success/count*100)

fig <- plot_ly(d_plot, 
               x = ~`average.node.degree`,
               y = ~prob,
               type = "bar",
               # width = 600, height = 350,
               text = ~prob #textposition = 'bottom center'
) %>% layout(xaxis = list(title = 'Average node degree /
                          Среднее число устойчивых связей у сотрудников организации',dtick = 1),
             yaxis = list(title = 'Доля успешных кейсов, %' )
)
fig


#  initial-outbreak-size ---------------
data <- read.csv2("Innovation on a Network initial-outbreak-size 1 1 50-table.csv", sep = ",", skip = 6) %>% 
  dplyr::mutate(success = ifelse(`count.turtles.with..infected..` == 0, 0, ifelse(`X.step.` == 3000, 0, 1))) 
# colnames(data)
d_plot <- data %>% dplyr::select("success", "initial.outbreak.size") %>% dplyr::mutate(count = c(1)) %>% 
  dplyr::group_by(`initial.outbreak.size`) %>% summarise_all(,.funs = sum) %>% 
  dplyr::mutate(prob = success/count*100)

fig <- plot_ly(d_plot, 
               x = ~`initial.outbreak.size`,
               y = ~prob,
               type = "bar",
               text = ~prob #textposition = 'bottom center'
) %>% layout(xaxis = list(title = 'Initial outbreak size /
                          Количество носителей инновационной идеи на старте',dtick = 1),
             yaxis = list(title = 'Доля успешных кейсов, %' )
)
fig

# ХАРАКТЕРИСТИКИ ЭЛЕМЕНТОВ МОДЕЛИ -----
# virus-spread-chance -------------
data <- read.csv2("Innovation on a Network virus-spread-chance 0-10-table.csv", sep = ",", skip = 6) %>% 
  dplyr::mutate(success = ifelse(`count.turtles.with..infected..` == 0, 0, ifelse(`X.step.` == 3000, 0, 1))) 

d_plot <- data %>% dplyr::select("success", "virus.spread.chance") %>% dplyr::mutate(count = c(1)) %>% 
  dplyr::group_by(`virus.spread.chance`) %>% summarise_all(,.funs = sum) %>% 
  dplyr::mutate(prob = success/count*100)


fig <- plot_ly(d_plot, 
  x = ~`virus.spread.chance`,
  y = ~prob,
  type = "bar",
  text = ~prob #textposition = 'bottom center'
) %>% layout(xaxis = list(title = 'Virus spread chance / 
                          Вероятность передачи инновационной идеи соседу по сети, %',dtick = 1),
             yaxis = list(title = 'Доля успешных кейсов, %' )
)

fig

#  virus-check-frequency ---------------
data <- read.csv2("Innovation on a Network virus-check-frequency 0-20-table.csv", sep = ",", skip = 6) %>% 
  dplyr::mutate(success = ifelse(`count.turtles.with..infected..` == 0, 0, ifelse(`X.step.` == 3000, 0, 1))) 

d_plot <- data %>% dplyr::select("success", "virus.check.frequency") %>% dplyr::mutate(count = c(1)) %>% 
  dplyr::group_by(`virus.check.frequency`) %>% summarise_all(,.funs = sum) %>% 
  dplyr::mutate(prob = success/count*100)

fig <- plot_ly(d_plot, 
               x = ~`virus.check.frequency`,
               y = ~prob,
               type = "bar",
               text = ~prob #textposition = 'bottom center'
) %>% layout(xaxis = list(title = 'Virus check frequency / 
                          Периодичность возникновения сомнений (1 раз в N периодов)',dtick = 1),
             yaxis = list(title = 'Доля успешных кейсов, %' )
)

fig



#  recovery-chance ---------------
data <- read.csv2("Innovation on a Network recovery-chance 0 1 10-table.csv", sep = ",", skip = 6) %>% 
  dplyr::mutate(success = ifelse(`count.turtles.with..infected..` == 0, 0, ifelse(`X.step.` == 3000, 0, 1))) 
# colnames(data)
d_plot <- data %>% dplyr::select("success", "recovery.chance") %>% dplyr::mutate(count = c(1)) %>% 
  dplyr::group_by(`recovery.chance`) %>% summarise_all(,.funs = sum) %>% 
  dplyr::mutate(prob = success/count*100)

fig <- plot_ly(d_plot, 
               x = ~`recovery.chance`,
               y = ~prob,
               type = "bar",
               text = ~prob #textposition = 'bottom center'
) %>% layout(xaxis = list(title = 'Recovery chance / 
                          Вероятность возникновения сопротивления, %',dtick = 1),
             yaxis = list(title = 'Доля успешных кейсов, %' )
)
fig


#  gain.resistance.chance  ---------------
data <- read.csv2("Innovation on a Network gain-resistance-chance 0 5 100-table.csv", sep = ",", skip = 6) %>% 
  dplyr::mutate(success = ifelse(`count.turtles.with..infected..` == 0, 0, ifelse(`X.step.` == 3000, 0, 1))) 
# colnames(data)
d_plot <- data %>% dplyr::select("success", "gain.resistance.chance") %>% dplyr::mutate(count = c(1)) %>% 
  dplyr::group_by(`gain.resistance.chance`) %>% summarise_all(,.funs = sum) %>% 
  dplyr::mutate(prob = success/count*100)

fig <- plot_ly(d_plot, 
               x = ~`gain.resistance.chance`,
               y = ~prob,
               type = "bar",
               text = ~prob #textposition = 'bottom center'
) %>% layout(xaxis = list(title = 'Gain resistance chance /
                          Вероятность возникновения полного недоверия к инновационной идее, %',dtick = 5),
             yaxis = list(title = 'Доля успешных кейсов, %' )
)

fig
