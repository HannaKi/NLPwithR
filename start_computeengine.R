#https://cloudyr.github.io/googleComputeEngineR/index.html
#install.packages("googleComputeEngineR")

## saved the json in Renviron
readRenviron('.Renviron') 
library(googleComputeEngineR)

#gce_list_machinetype()

## see gce_list_machinetype() for options of predefined_type
vm <- gce_vm(template = "rstudio-shiny",
             name = "rstudio-team",
             username = "mark", 
             password = "mark1234",
             predefined_type = "n1-standard-1")

##remember to stop the instance
gce_vm_stop(vm)

# in theory you should be able to start the instance again with this
gce_vm_start(vm)
