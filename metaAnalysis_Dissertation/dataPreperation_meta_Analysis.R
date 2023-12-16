# sets the directory of location of this script as the current directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

rm(list=ls(all=TRUE))
graphics.off()

########################################
# load packages
########################################
library(tidyverse)
library(rjson)
library(igraph)
library(xlsx)


########################################
# load functions
########################################
# print(getwd())
setwd("functionsCAM")
for(i in 1:length(dir())){
  # print(dir()[i])
  source(dir()[i], encoding = "utf-8")
}
rm(i)
setwd("..")


#############################################################
# load files
# Sendtner, C. (2021). Kostbare Kisten: Gründe für Fehleinschätzungen der Kosten des eigenen Autos und deren Auswirkungen auf die Bewertung des ÖPNV -Masterarbeit [University of Freiburg]. https://doi.org/10.13140/RG.2.2.32640.56325
#############################################################
setwd("rawData")
dir()
setwd("Motivation of Car vs. Public Transport Use 2021")


files_links <- list.files(path = getwd(), pattern = "*_links.csv", full.names = FALSE)
files_blocks <- list.files(path = getwd(), pattern = "*_blocks.csv", full.names = FALSE)
blocks <- list()
links <- list()

h=1
for(i in 1:length(files_links)){
  tmp_links <- read.csv(file = files_links[i], sep = ",", encoding = "UTF-8")
  if(nrow(tmp_links) > 0){
    blocks[[h]] <- read.csv(file = files_blocks[i], sep = ",", encoding = "UTF-8")
    blocks[[h]]$participantCAM <- str_extract(string = files_blocks[i], pattern = ".*(?=_blocks)")

    links[[h]] <- tmp_links
    links[[h]]$participantCAM <- str_extract(string = files_links[i], pattern = ".*(?=_links)")
    h=h+1
  }else{
    print(files_links[i])
  }
}
setwd("../..")


########################################
# pre-processing
########################################
## create CAM files
CAMfiles <- create_ValenceFiles(datBlocks = blocks, datLinks = links, verbose = FALSE)

## fix Valence data
tmp_fixed <- fix_ValenceData(dat_nodes = CAMfiles[[1]],
                             dat_connectors = CAMfiles[[2]],
                             dat_merged = CAMfiles[[3]], verbose = FALSE)

CAMfiles[[1]] <- tmp_fixed[[1]]
CAMfiles[[3]] <- tmp_fixed[[3]]

## draw CAMs
CAMdrawn <- draw_CAM(dat_merged = CAMfiles[[3]],
                     dat_nodes = CAMfiles[[1]],ids_CAMs = "all", plot_CAM = FALSE,
                     relvertexsize = 3,
                     reledgesize = 1)



## check number of components
for(i in 1:length(x = CAMdrawn)){
  tmp_com <- components(graph = CAMdrawn[[i]])
  if(tmp_com$no != 1){
    print(names(CAMdrawn)[i])
  }
}


# Save an object to a file
saveRDS(CAMdrawn, file = "outputs/CAMdrawn_Sendtner (2021).rds")


CAMindicators <- compute_indicatorsCAM(drawn_CAM = CAMdrawn)
head(CAMindicators)

write.xlsx2(x = CAMindicators, file = "outputs/CAMindicator_Sendtner (2021).xlsx")



#############################################################
# load files
# Reuter, L., Fenn, J., Bilo, T. A., Schulz, M., Weyland, A. L., Kiesel, A., & Thomaschke, R. (2021). Leisure walks modulate the cognitive and affective representation of the corona pandemic: Employing Cognitive-Affective Maps within a randomized experimental design. Applied Psychology: Health and Well-Being, 13(4), 952–967. https://doi.org/10.1111/aphw.12283################################################################################
#############################################################
setwd("rawData")
dir()
setwd("Intervetion study Leisure Walks 2020")

files_links <- list.files(path = getwd(), pattern = "*_links.csv", full.names = FALSE)
files_blocks <- list.files(path = getwd(), pattern = "*_blocks.csv", full.names = FALSE)
blocks <- list()
links <- list()

h=1
for(i in 1:length(files_links)){
  tmp_links <- read.csv(file = files_links[i], sep = ",", encoding = "UTF-8")
  if(nrow(tmp_links) > 0){
    blocks[[h]] <- read.csv(file = files_blocks[i], sep = ",", encoding = "UTF-8")
    blocks[[h]]$participantCAM <- str_extract(string = files_blocks[i], pattern = ".*(?=_blocks)")

    links[[h]] <- tmp_links
    links[[h]]$participantCAM <- str_extract(string = files_links[i], pattern = ".*(?=_links)")
    h=h+1
    }else{
    print(files_links[i])
  }
}
setwd("../..")

# plot(CAMdrawn[["612"]])


########################################
# pre-processing
########################################
## create CAM files
CAMfiles <- create_ValenceFiles(datBlocks = blocks, datLinks = links, verbose = FALSE)

## fix Valence data
tmp_fixed <- fix_ValenceData(dat_nodes = CAMfiles[[1]],
                dat_connectors = CAMfiles[[2]],
                dat_merged = CAMfiles[[3]], verbose = FALSE)

CAMfiles[[1]] <- tmp_fixed[[1]]
CAMfiles[[3]] <- tmp_fixed[[3]]

## draw CAMs
CAMdrawn <- draw_CAM(dat_merged = CAMfiles[[3]],
                     dat_nodes = CAMfiles[[1]],ids_CAMs = "all", plot_CAM = FALSE,
                     relvertexsize = 3,
                     reledgesize = 1)

## check number of components

for(i in 1:length(x = CAMdrawn)){
  tmp_com <- components(graph = CAMdrawn[[i]])
  if(tmp_com$no != 1){
    print(names(CAMdrawn)[i])
  }
}

saveRDS(CAMdrawn, file = "outputs/CAMdrawn_Reuter et al. (2021).rds")


CAMindicators <- compute_indicatorsCAM(drawn_CAM = CAMdrawn)
head(CAMindicators)

write.xlsx2(x = CAMindicators, file = "outputs/CAMindicator_Reuter et al. (2021).xlsx")



#############################################################
# load files
# Fenn, J., Helm, J. F., Höfele, P., Kulbe, L., Ernst, A., & Kiesel, A. (2023). Identifying key-psychological factors influencing the acceptance of yet emerging technologies–A multi-method-approach to inform climate policy. PLOS Climate, 2(6), 1–25. https://doi.org/10.1371/journal.pclm.0000207
#############################################################
setwd("rawData")
dir()
setwd("Stratospheric Aerosol Injection Multi Method 2022")


read_file("jatos_results_20230131145704.txt") %>%
  # ... split it into lines ...
  str_split('\n') %>% first() %>%
  # ... filter empty rows ...
  discard(function(x) x == '') -> dat_CAM


raw_CAM <- list()
for(i in 1:length(dat_CAM)){
  raw_CAM[[i]] <- jsonlite::fromJSON(txt = dat_CAM[[i]])
}
rm(i)


setwd("../..")



########################################
# pre-processing
########################################
## create CAM files
### create CAM single files (nodes, connectors, merged)
CAMfiles <- create_CAMfiles(datCAM = raw_CAM, reDeleted = TRUE)


## draw CAMs
CAMdrawn <- draw_CAM(dat_merged = CAMfiles[[3]],
                     dat_nodes = CAMfiles[[1]],ids_CAMs = "all", plot_CAM = FALSE,
                     relvertexsize = 3,
                     reledgesize = 1)
# plot(CAMdrawn[[4]])

## check number of components
for(i in 1:length(x = CAMdrawn)){
  tmp_com <- components(graph = CAMdrawn[[i]])
  if(tmp_com$no != 1){
    print(names(CAMdrawn)[i])
  }
}

saveRDS(CAMdrawn, file = "outputs/CAMdrawn_Fenn et al. (2023).rds")



### CAM indicators
CAMindicators <- compute_indicatorsCAM(drawn_CAM = CAMdrawn)
head(CAMindicators)



write.xlsx2(x = CAMindicators, file = "outputs/CAMindicator_Fenn et al. (2023).xlsx")




#############################################################
# load files
# Mansell, J., Reuter, L., Rhea, C., & Kiesel, A. (2021). A Novel Network Approach to Capture Cognition and Affect: COVID-19 Experiences in Canada and Germany. Frontiers in Psychology, 12, 1–14. https://doi.org/10.3389/fpsyg.2021.663627
#############################################################
################ > Canada
setwd("rawData")
dir()
setwd("Network Approach CAMs 2021")
### tp1
setwd("canada")

files_links <- list.files(path = getwd(), pattern = "*_links.csv", full.names = FALSE)
files_blocks <- list.files(path = getwd(), pattern = "*_blocks.csv", full.names = FALSE)
blocks <- list()
links <- list()

h=1
for(i in 1:length(files_links)){
  tmp_links <- read.csv(file = files_links[i], sep = ",", encoding = "UTF-8")
  if(nrow(tmp_links) > 0){
    blocks[[h]] <- read.csv(file = files_blocks[i], sep = ",", encoding = "UTF-8")
    blocks[[h]]$participantCAM <- str_extract(string = files_blocks[i], pattern = ".*(?=_blocks)")

    links[[h]] <- tmp_links
    links[[h]]$participantCAM <- str_extract(string = files_links[i], pattern = ".*(?=_links)")
    h=h+1
  }else{
    print(files_links[i])
  }
}
setwd("../../..")



########################################
# pre-processing
########################################
## create CAM files
CAMfiles <- create_ValenceFiles(datBlocks = blocks, datLinks = links, verbose = FALSE)

## fix Valence data
tmp_fixed <- fix_ValenceData(dat_nodes = CAMfiles[[1]],
                             dat_connectors = CAMfiles[[2]],
                             dat_merged = CAMfiles[[3]])

CAMfiles[[1]] <- tmp_fixed[[1]]
CAMfiles[[3]] <- tmp_fixed[[3]]

## draw CAMs
CAMdrawn <- draw_CAM(dat_merged = CAMfiles[[3]],
                     dat_nodes = CAMfiles[[1]],ids_CAMs = "all", plot_CAM = FALSE,
                     relvertexsize = 3,
                     reledgesize = 1)

## check number of components

for(i in 1:length(x = CAMdrawn)){
  tmp_com <- components(graph = CAMdrawn[[i]])
  if(tmp_com$no != 1){
    print(names(CAMdrawn)[i])
  }
}

saveRDS(CAMdrawn, file = "outputs/CAMdrawn_Mansell, Reuter et al. (2021) Canada.rds")


### CAM indicators
CAMindicators <- compute_indicatorsCAM(drawn_CAM = CAMdrawn)
head(CAMindicators)



write.xlsx2(x = CAMindicators, file = "outputs/CAMindicator_Mansell, Reuter et al. (2021) Canada.xlsx")


################ > Germany
setwd("rawData")
dir()
setwd("Network Approach CAMs 2021")
### tp1
setwd("germany")

files_links <- list.files(path = getwd(), pattern = "*_links.csv", full.names = FALSE)
files_blocks <- list.files(path = getwd(), pattern = "*_blocks.csv", full.names = FALSE)
blocks <- list()
links <- list()

h=1
for(i in 1:length(files_links)){
  tmp_links <- read.csv(file = files_links[i], sep = ",", encoding = "UTF-8")
  if(nrow(tmp_links) > 0){
    blocks[[h]] <- read.csv(file = files_blocks[i], sep = ",", encoding = "UTF-8")
    blocks[[h]]$participantCAM <- str_extract(string = files_blocks[i], pattern = ".*(?=_blocks)")

    links[[h]] <- tmp_links
    links[[h]]$participantCAM <- str_extract(string = files_links[i], pattern = ".*(?=_links)")
    h=h+1
  }else{
    print(files_links[i])
  }
}
setwd("../../..")



########################################
# pre-processing
########################################
## create CAM files
CAMfiles <- create_ValenceFiles(datBlocks = blocks, datLinks = links, verbose = FALSE)

## fix Valence data
tmp_fixed <- fix_ValenceData(dat_nodes = CAMfiles[[1]],
                             dat_connectors = CAMfiles[[2]],
                             dat_merged = CAMfiles[[3]])

CAMfiles[[1]] <- tmp_fixed[[1]]
CAMfiles[[3]] <- tmp_fixed[[3]]

## draw CAMs
CAMdrawn <- draw_CAM(dat_merged = CAMfiles[[3]],
                     dat_nodes = CAMfiles[[1]],ids_CAMs = "all", plot_CAM = FALSE,
                     relvertexsize = 3,
                     reledgesize = 1)

## check number of components

for(i in 1:length(x = CAMdrawn)){
  tmp_com <- components(graph = CAMdrawn[[i]])
  if(tmp_com$no != 1){
    print(names(CAMdrawn)[i])
  }
}

saveRDS(CAMdrawn, file = "outputs/CAMdrawn_Mansell, Reuter et al. (2021) Germany.rds")


### CAM indicators
CAMindicators <- compute_indicatorsCAM(drawn_CAM = CAMdrawn)
head(CAMindicators)



write.xlsx2(x = CAMindicators, file = "outputs/CAMindicator_Mansell, Reuter et al. (2021) Germany.xlsx")
