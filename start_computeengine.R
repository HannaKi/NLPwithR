#https://cloudyr.github.io/googleComputeEngineR/index.html

readRenviron('.Renviron') 
library(googleComputeEngineR)

vm_nlp <- gce_vm("virtualmachine", 
                  predefined_type = "n1-standard-1", 
                  template = "rstudio-shiny", 
                  username = "eicaa", 
                  password = Sys.getenv("PASSWORDCLOUD") 
                  #,dynamic_image = "gcr.io/gcer-public/persistent-rstudio"
                 )

gce_vm_stop(vm_nlp)

gce_vm_start(vm_nlp)

gce_pull_registry()