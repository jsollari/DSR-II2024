#autor:      Joao Sollari Lopes
#local:      INE, Lisboa
#Rversion:   4.3.1
#criado:     05.07.2023
#modificado: 23.08.2023

pack1 = c(
  "tidyverse",   #collection of packages for "tidyverse"
  "nycflights13" #dataset nycflights13
)
pack2 = c(
  "hexbin",      #package for "hexagonal binning" plots
  "tidymodels",  #collection of of packages for "tidymodels"
  "lvplot",      #package for "letter value" plots
  "ggbeeswarm",  #package for "bee swarm" plots
  "gridExtra"    #package for extra features of ggplot2
)
pack3 = c(
  "dotwhisker",  #package for "dot whisker" plots
  "see",         #package of "easystats" for visualization
  "performance", #package of "easystats" for diangosting models
  "glmnet",      #package for GLMs with penalized maxL
  "ranger",      #package for random forest models
  "vip"          #package for "variable importance" plots for ML models
)

install.packages(c(pack1,pack2,pack3),repos="http://mirror.ibcp.fr/pub/CRAN/")
