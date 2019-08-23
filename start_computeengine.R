#https://cloudyr.github.io/googleComputeEngineR/index.html
library(googleComputeEngineR)
#readRenviron('.Renviron') 

vm_nlp <- gce_vm("virtualmachine", 
                  predefined_type = "n1-standard-1", 
                  template = "rstudio", 
                  username = "eicaa", 
                  password = Sys.getenv("PASSWORDCLOUD") ,
                  dynamic_image = "gcr.io/gcer-public/persistent-rstudio")

gce_vm_stop(vm_nlp)

gce_vm_start(vm_nlp)