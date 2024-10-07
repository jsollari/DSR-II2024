#autor:      Joao Sollari Lopes
#local:      INE, Lisboa
#Rversion:   4.3.1
#criado:     05.07.2023
#modificado: 07.10.2024

pack0 = c(
  "performance",    #package of "easystats" for diangosting models
  "rpart",          #package for decision tree models
  "tidyverse"       #collection of packages for "tidyverse"
)

pack1 = c(
  "nycflights13",   #dataset nycflights13
  "skimr"           #package for descriptive statistics
)
pack2 = c(
  "ggbeeswarm",     #package for "bee swarm" plots
  "gridExtra",      #package for extra features of ggplot2
  "hexbin",         #package for "hexagonal binning" plots
  "lvplot",         #package for "letter value" plots
  "tidymodels"      #collection of of packages for "tidymodels"
)
pack3 = c(
  "dotwhisker",     #package for "dot whisker" plots
  "GGally",         #package for quick data visualization
  "glmnet",         #package for GLMs with penalized maxL
  "nnet",           #package for neural network models
  "NeuralNetTools", #package for tools to neural network models
  "ranger",         #package for random forest models
  "vip"             #package for "variable importance" plots for ML models
)

install.packages(c(pack1,pack2,pack3), repos = "http://mirror.ibcp.fr/pub/CRAN/")
