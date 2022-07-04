## https://labjs.readthedocs.io/en/latest/learn/deploy/3c-jatos.html
## https://labjs.readthedocs.io/en/latest/learn/deploy/3-third-party.html


# sets the directory of location of this script as the current directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

rm(list=ls(all=TRUE))
graphics.off()

############################################################################
# Pakete, Daten laden
############################################################################
################
# Pakete
################
# This code relies on the pacman, tidyverse and jsonlite packages
require(pacman)
p_load('tidyverse', 'jsonlite', 'magrittr', 'xlsx',
       'stargazer', 'psych', 'igraph', 'Cairo', 'visNetwork', 'tm')
# rjson pracma performance lme4 lattice



################
# Daten
################
# Read the text file from JATOS ...
dir()
read_file('01_jatos_results_20220108121121.txt') %>%
  # ... split it into lines ...
  str_split('\n') %>% first() %>%
  # ... filter empty rows ...
  discard(function(x) x == '') %>%
  # ... parse JSON into a data.frame
  map_dfr(fromJSON, flatten=TRUE) -> dat1


### read CAM
read_file('02_jatos_results_20220108121141.txt') %>%
  # ... split it into lines ...
  str_split('\n') %>% first() %>%
  # ... filter empty rows ...
  discard(function(x) x == '') -> dat2


# dat2 <- as.list(vroom::vroom(file = "02_jatos_results_20211220080317.txt",
#                           delim = "\n", show_col_types = FALSE, col_names = FALSE)$X1)


# paste0(str_sub(string = a$X1[1], start = 1, end = 100), ".....")
raw_CAM <- list()
for(i in 1:length(dat2)){
  raw_CAM[[i]] <- jsonlite::fromJSON(txt = dat2[[i]])
}


read_file('03_jatos_results_20220108121156.txt') %>%
  # ... split it into lines ...
  str_split('\n') %>% first() %>%
  # ... filter empty rows ...
  discard(function(x) x == '') %>%
  # ... parse JSON into a data.frame
  map_dfr(fromJSON, flatten=TRUE) -> dat3

head(dat1); dim(dat1)
length(raw_CAM)
head(dat3); dim(dat3)

rm(dat2); rm(i)

### desc. stats:
length(dat1$url.srid[!is.na(dat1$url.srid)])
length(raw_CAM)
raw_CAM_final <- list(); h=1
for(i in 1:length(raw_CAM)){
  if(nrow(raw_CAM[[i]]$nodes) >= 20){
    raw_CAM_final[[h]] <- raw_CAM[[i]]
    h=h+1
  }
}
length(raw_CAM_final)
length(dat3$url.srid[!is.na(dat3$url.srid)])

###

################
# sub functions
################
setwd("functions_CAM")
source("create_CAMfiles.R", encoding="utf-8")
source("compute_indicatorsCAM.R", encoding="utf-8")
source("draw_CAM.R", encoding="utf-8")
source("create_wordlist.R", encoding="utf-8")
source("summaryFunctions.R", encoding="utf-8")
source("aggregate_CAMs.R", encoding="utf-8")

source("helperFunctions.R", encoding="utf-8")
setwd("..")



############################################################################
############################################################################
# run code -> CAMs
############################################################################
### create CAM single files (nodes, connectors, merged)
CAMfiles <- create_CAMfiles(datCAM = raw_CAM_final, reDeleted = TRUE)

## Datenfehler ??!!
length(unique(CAMfiles[[1]]$CAM))
sort(table(CAMfiles[[1]]$text))
table(str_subset(string = CAMfiles[[1]]$text, pattern = "(l|L)eistungsdruck"))
table(str_subset(string = CAMfiles[[1]]$text, pattern = "(U|u)nterstützung"))
# CAMfiles[[1]]$text[str_detect(string = CAMfiles[[1]]$text,
#                               pattern = "Leistungsdruck")] <- "empfundener Leistungsdruck"

## identical words within CAMs
for(i in 1:length(unique(CAMfiles[[1]]$CAM))){
  tmp <- CAMfiles[[1]]$text[CAMfiles[[1]]$CAM == unique(CAMfiles[[1]]$CAM)[i]]
  if(any(table(tmp) > 2)){
    cat("CAM:",  unique(CAMfiles[[1]]$CAM)[i], "contains multiple words:\n")
    print(names(table(tmp))[table(tmp) > 2]
    )
  }
}

### draw CAMs
CAMdrawn <- draw_CAM(dat_merged = CAMfiles[[3]],
                     dat_nodes = CAMfiles[[1]],ids_CAMs = "all", plot_CAM = FALSE,
                     relvertexsize = 5,
                     reledgesize = 1)

# CAMdrawn <- draw_CAM(dat_merged = CAMfiles[[3]],
#                      dat_nodes = CAMfiles[[1]],ids_CAMs = names(CAMdrawn)[1:10], plot_CAM = FALSE,
#                      relvertexsize = 5,
#                      reledgesize = 1)

plot(CAMdrawn[["78497bac-6769-4a42-9d08-62175063b5ad"]])
  plot(CAMdrawn[[3]], edge.arrow.size = .7,
       layout=layout_nicely, vertex.frame.color="black", asp = .5, margin = -0.1,
       vertex.size = 12, vertex.label.cex = .9)
# for(i in 1:length(CAMdrawn)){
#   print(is_simple(CAMdrawn[[i]]))
# }

setwd("CAMs in R")
ids_CAMs <- unique(CAMfiles[[3]]$CAM.x); length(ids_CAMs)
for(i in 1:length(ids_CAMs)){
  save_graphic(filename = paste0(ids_CAMs[i]))
  CAM_igraph <- CAMdrawn[[c(1:length(CAMdrawn))[
    names(CAMdrawn) == paste0("CAM_", unique(CAMfiles[[3]]$CAM.x)[i])]]]
  plot(CAM_igraph, edge.arrow.size = .7,
       layout=layout_nicely, vertex.frame.color="black", asp = .5, margin = -0.1,
       vertex.size = 12, vertex.label.cex = .9)
  dev.off()
}
setwd("..")

### create CAM indicators
CAMindicators <- compute_indicatorsCAM(drawn_CAM = CAMdrawn,
                                       micro_degree = c("Psychologie", "empfundener Leistungsdruck", "empfundene Unterstützung", "allgemeines Lehrklima"),
                                       micro_valence = c("Psychologie", "empfundener Leistungsdruck", "empfundene Unterstützung", "allgemeines Lehrklima"),
                                       micro_centr_clo = NULL)
any(colSums(is.na(CAMindicators)) == nrow(CAMindicators))

write.xlsx2(x = CAMindicators,
            file = "CAMindicators.xlsx")

dev.off()
# correlation plot over all macro indicators
psych::cor.plot(r = cor(CAMindicators[, str_detect(string = colnames(CAMindicators), pattern = "_macro")],
                        use = "pairwise.complete.obs"), upper = FALSE,
                xlas = 2, cex = .75, cex.axis = .55)
dev.off()

## show significant correlatons
sigCorr <- des_sigCorr(indicatos_CAM = CAMindicators,
                       vars = c("mean_valence_macro", "assortativity_valence_macro"))

## create descreptive APA data table of network indicatos
summaryStats <- des_summaryStats(indicatos_CAM = CAMindicators, createHTML = TRUE)

## create list with most central terms (closeness)
centralTerms <- des_centralTerms(drawn_CAM = CAMdrawn, createHTML = TRUE)



### > add ID vector from variable participantCAM:
tmp <- unique(cbind(CAMfiles[[1]]$CAM, CAMfiles[[1]]$participantCAM))
CAMindicators$IDparticipant  <- tmp[,2][CAMindicators$CAM_ID %in% tmp[,1]]
rm(tmp)

head(CAMindicators)
cbind(CAMindicators$CAM_ID, CAMindicators$participantCAM)

### create CAM word lists
CAMwordlist <- create_wordlist(dat_nodes = CAMfiles[[1]],
                               dat_merged = CAMfiles[[3]],
                               order = "frequency",
                               comments = TRUE)
head(CAMwordlist)
head(CAMwordlist[,1:7])

write.xlsx2(x = CAMwordlist,
            file = "CAMwordlist.xlsx")

### save all comments

extractedComments <- des_extractComments(dat_nodes = CAMfiles[[1]])
write.xlsx(x = extractedComments, file = "extractedComments.xlsx", row.names = FALSE)



## save CAM to drawn them using java script:
setwd("CAMs as JSON format")
for(i in 1:length(raw_CAM_final)){
  write(toJSON(raw_CAM_final[[i]], encoding = "UTF-8"),
        paste0(raw_CAM_final[[i]]$idCAM, ".json"))
}


# str <- c("äöü", "ÄÖÜ")
# tmp <- stringi::stri_replace_all_fixed(
#   raw_CAM_final[[1]],
#   c("ä", "ö", "ü", "Ä", "Ö", "Ü"),
#   c("ae", "oe", "ue", "Ae", "Oe", "Ue"),
#   vectorize_all = FALSE
# )


### rename identical terms
# CAMfiles[[1]] <- rename_identicalTerms(dat_nodes = CAMfiles[[1]], drawn_CAM = CAMdrawn)
# table(str_subset(string = CAMfiles[[1]]$text, pattern = "_"))

### aggregate CAM
num_cams <- 6
# sel_ids <- sample(x = unique(CAMfiles[[1]]$CAM), size = num_cams, replace = FALSE) # 5 randomly
sel_ids <- CAMindicators$CAM_ID[order(CAMindicators$mean_valence_macro,
                                      decreasing = TRUE)]
sel_ids <- sel_ids[1:6]
plot(CAMdrawn[[sel_ids[1]]], edge.arrow.size = .7,
     layout=layout_nicely, vertex.frame.color="black", asp = .5, margin = -0.1,
     vertex.size = 12, vertex.label.cex = .9)

CAMaggregated <- aggregate_CAMs(dat_merged = CAMfiles[[3]], dat_nodes = CAMfiles[[1]],
                                ids_CAMs = sel_ids)
plot(CAMaggregated[[2]], vertex.size=diag(CAMaggregated[[1]]) / max(diag(CAMaggregated[[1]]))*20, edge.arrow.size=0.01)
plot(CAMaggregated[[2]], vertex.size=(abs(V(CAMaggregated[[2]])$value)+1)*5, edge.arrow.size=0.01)


sel_nodes <- CAMfiles[[1]][CAMfiles[[1]]$CAM %in% sel_ids, ]
sel_merged <- CAMfiles[[3]][CAMfiles[[3]]$CAM.x %in% sel_ids, ]
tmpWordlist <- create_wordlist(dat_nodes = sel_nodes,
                               dat_merged = sel_merged,
                               order = "frequency",
                               comments = TRUE)
write.xlsx2(x = tmpWordlist,
            file = "mostpositive_aggCAM_wordlist.xlsx")
rm(sel_ids); rm(sel_nodes); rm(sel_merged); rm(tmpWordlist)

# > using visNetwork
CAMaggregated[[2]] <- as.undirected(CAMaggregated[[2]])
nodes <- data.frame(id = str_replace_all(string = V(CAMaggregated[[2]])$name, pattern = " ", replacement = "_"),
                    label = V(CAMaggregated[[2]])$name,
                    color  = V(CAMaggregated[[2]])$color,
                    value = diag(CAMaggregated[[1]]) / max(diag(CAMaggregated[[1]])),
                    title = diag(CAMaggregated[[1]]) / num_cams)
edges <- data.frame(from = str_replace_all(string = as_edgelist(CAMaggregated[[2]])[,1], pattern = " ", replacement = "_"),
                    to = str_replace_all(string = as_edgelist(CAMaggregated[[2]])[,2], pattern = " ", replacement = "_"))
# sort(table(nodes$id))
visNetwork(nodes, edges)





############################################################################
### applying approximate string matching
# > https://deanattali.com/blog/shinyalert-package/
# > https://mastering-shiny.org/
# > https://shiny.rstudio.com/reference/shiny/1.6.0/modalDialog.html
# > https://mastering-shiny.org/action-feedback.html?q=modalDialog#feedback-modal
# user.input <- dlgInput("Enter a number", Sys.info()["user"])$res

library(stringdist)
nodes_text <- unique(CAMfiles[[1]]$text)
nodes_text <- unique(stringr::str_trim(nodes_text, side = "both"))
h = 1
list_nodesMatching <- list()

for(i in 1:length(nodes_text)){
  tmp_allother <- nodes_text[nodes_text[i] !=  nodes_text]
  tmp_optimalmatching <- stringdist(nodes_text[i], tmp_allother, method = "osa")
  if(any(tmp_optimalmatching < 4)){
    # cat("\n word:", nodes_text[i], "num:", h, "\n")
    # print(tmp_allother[tmp_optimalmatching < 4])
    list_nodesMatching[[h]] <- sort(c(nodes_text[i],
                                      tmp_allother[tmp_optimalmatching < 4]))
    h = h+1
  }
}
list_nodesMatching <- unique(list_nodesMatching)
head(list_nodesMatching); length(list_nodesMatching)
nodes_text[nodes_text %in% list_nodesMatching[[1]]] <- "Unsicherheit"
length(unique(nodes_text))



### Tokenization of words
str_split(string = nodes_text, pattern = " ", simplify = TRUE)


### word 2 vec
library(udpipe)
library(word2vec)
setwd("word2vec_Models")
dir()
model <- read.word2vec(file = "german.model", normalize = FALSE)

# https://github.com/mukul13/rword2vec
# https://github.com/devmount/GermanWordEmbeddings/blob/master/training.py
library(rword2vec)
dist=distance(file_name = "englishModel_2013.bin",search_word = "king",num = 10)
# rword2vec::bin_to_txt(bin_file = "englishModel_2013.bin", txt_file = "englishModel_2013.txt")
model_english <- read.word2vec(file = "englishModel_2013.gz", normalize = FALSE)

tmp_nodes_dat <- CAMfiles[[1]]
tmp_nodes_boolean <- table(tmp_nodes_dat$text) < 20
tmp_nodes <- names(table(tmp_nodes_dat$text))[tmp_nodes_boolean]
rm(tmp_nodes_boolean)

### create search vector
search_vec <- c(); d=1
# length(tmp_titles)
for(i in 1:40){

  tmp <- str_trim(tmp_nodes[i], side = "both")
  tmp <- str_remove_all(string = tmp, pattern = "\"")
  tmp <- str_split(string = tmp, pattern = " ", simplify = TRUE)

  if(length(tmp) == 1){
    cat("single word: ", tmp[1], "\n")


    tmp_predict <- NULL
    tryCatch({
      tmp_predict <- predict(model, tmp[1], type = "nearest", top_n = 20)
    }, error=function(e){cat("catched ERROR :",conditionMessage(e), "\n")})

    if(!is.null(tmp_predict)){
      search_vec[d] <- c(tmp[1])
      d=d+1
    }
  }
}

search_vec
tmp_predict_final <- predict(model, search_vec, type = "nearest", top_n = 25)
tmp_predict <- tmp_predict_final
### filter results
for(i in 1:length(tmp_predict)){
  # if lower case letter -> filter for upper case letter
  if(str_detect(string = tmp_predict[[i]]$term1[1], pattern = "^[:lower:]")){
    tmp_boolean <- str_detect(string = tmp_predict[[i]]$term2,
                              pattern = "_|%|^[:upper:]", negate = TRUE)
    tmp_predict[[i]] <- tmp_predict[[i]][tmp_boolean, ]
  }else{
    # if upper case letter -> filter for lower case letter
    tmp_boolean <- str_detect(string = tmp_predict[[i]]$term2,
                              pattern = "_|%|^[:lower:]", negate = TRUE)
    tmp_predict[[i]] <- tmp_predict[[i]][tmp_boolean, ]
  }
}

## create data set
tmp_predict_dat <- data.frame(term1 = tmp_predict[[1]]$term1, term2 = tmp_predict[[1]]$term2,
                              similarity = tmp_predict[[1]]$similarity, rank = tmp_predict[[1]]$rank)
for(i in 2:length(tmp_predict)){
  if(tmp_predict[[i]]$term1[1] == tmp_predict[[i]]$term2[1] && nrow(tmp_predict[[i]]) > 1){
    tmp_predict_dat <- rbind(tmp_predict_dat,
                             data.frame(term1 = tmp_predict[[i]]$term1, term2 = tmp_predict[[i]]$term2,
                                        similarity = tmp_predict[[i]]$similarity, rank = tmp_predict[[i]]$rank))
  }

}

sort(table(tmp_predict_dat$term2))

## recursively replace words
for(i in 1:length(unique(tmp_predict_dat$term1))){
  tmp_subterms <- tmp_predict_dat$term2[tmp_predict_dat$term1 == unique(tmp_predict_dat$term1)[i]]

  print(tmp_nodes_dat$text[tmp_nodes_dat$text %in% tmp_subterms])
  tmp_nodes_dat$text[tmp_nodes_dat$text %in% tmp_subterms] <- unique(tmp_predict_dat$term1)[i]
}

length(unique(tmp_nodes_dat$text))
length(unique(CAMfiles[[1]]$text))











############################################################################
# run code -> data files dat1, dat3
############################################################################
################
# ID Variable erstellen dat1
################
### Deskreptiv N
## N:
sum(!is.na(dat1$url.srid)); sum(dat1$sender_id == "0", na.rm = TRUE)

### ID variable dat 1
dat1$ID <- NA
tmp_IDcounter <- 0
for(i in 1:nrow(dat1)){
  if(!is.na(dat1$url.srid[i])){
    # tmp <- dat$prolific_pid[i]
    tmp_IDcounter = tmp_IDcounter + 1
  }
  dat1$ID[i] <- tmp_IDcounter
}

rm(i); rm(tmp_IDcounter)
table(dat1$ID)

################
# ID Variable erstellen dat3
################
### Deskreptiv N
## N:
sum(!is.na(dat3$url.srid)); sum(dat3$sender_id == "0", na.rm = TRUE)

### ID variable dat 1
dat3$ID <- NA
tmp_IDcounter <- 0
for(i in 1:nrow(dat3)){
  if(!is.na(dat3$url.srid[i])){
    # tmp <- dat$prolific_pid[i]
    tmp_IDcounter = tmp_IDcounter + 1
  }
  dat3$ID[i] <- tmp_IDcounter
}

rm(i); rm(tmp_IDcounter)
table(dat3$ID)


## remove missing > X
# sel_per <- 70
# tmp_sel <- data.frame(ID = unique(dat$ID),
#                       solved_perc = as.numeric(table(dat$ID) / max(table(dat$ID)) * 100))
# tmp_sel[tmp_sel$solved_perc <= sel_per, ]
# tmp_sel <- tmp_sel[tmp_sel$solved_perc > sel_per, ]
# dat <- dat[dat$ID %in% tmp_sel$ID,]
# rm(sel_per); rm(tmp_sel)


glimpse(dat1)
glimpse(dat3)


################
# save single variables as excel files
################
### save feedback as Excel file
write.xlsx2(x = data.frame(ID = dat3[, "ID"][!is.na(dat3[, "feedback_critic"]) & dat3[, "feedback_critic"] != ""],
                           feedback = dat3[, "feedback_critic"][!is.na(dat3[, "feedback_critic"]) & dat3[, "feedback_critic"] != ""]),
            file = "feedback_critic.xlsx")

write.xlsx2(x = data.frame(ID = dat3[, "ID"][!is.na(dat3[, "feedCAM_technicalprobsText"]) & dat3[, "feedCAM_technicalprobsText"] != ""],
                           feedback = dat3[, "feedCAM_technicalprobsText"][!is.na(dat3[, "feedCAM_technicalprobsText"]) & dat3[, "feedCAM_technicalprobsText"] != ""]),
            file = "feedCAM_technicalprobsText.xlsx")



################
# save single variables as excel files
################
##############
# Fragen ohne Loop (Fragebogen + demographische Fragen)
##############
ques_mixed <- c("IDparticipant",
                str_subset(string = colnames(dat3), pattern = "^sociodemo"),
                str_subset(string = colnames(dat3), pattern = "^psc"),
                str_subset(string = colnames(dat3), pattern = "^studienanf"),
                str_subset(string = str_subset(string = colnames(dat3), pattern = "^feedCAM"),
                           pattern = "Text$", negate = TRUE))




# dataset = dat3
# datasetques = questionnaire
# listvars = ques_mixed
vec_notNumeric = c("IDparticipant", "sociodemo_gender", "sociodemo_education", "sociodemo_occupation")
questionnairetype <- function(dataset,
                              listvars = ques_mixed,
                              notNumeric = vec_notNumeric){

  datasetques <- data.frame(ID = unique(dataset$ID))

  for(c in 1:length(listvars)){
    if(any(colnames(dataset) == listvars[c])){

      ## tmp IDs
      tmpid <- dataset$ID[!is.na(dataset[, listvars[c]])]
      ## tmp value variable
      tmpvalue <- dataset[, listvars[c]][!is.na(dataset[, listvars[c]])]
      datasetques[listvars[c]]  <- NA

      # prolific PID
      # if(vec_vars[c] == "prolific_pid"){
      #   datasetques[vec_vars[c]] <- unique(tmpvalue)
      #
      #   # if multiple answers are stored within JATOS
      # }else if(sum(datasetques$ID %in% tmpid) !=  length(tmpvalue)){
      #   cat("multiple times answered: ", vec_vars[c], "by ID: \n")
      #   tmptab <- table(tmpid, tmpvalue)
      #   tmptab[table(tmpid, tmpvalue) == 0] <- NA
      #   tmptab[tmptab != 1 & !is.na(tmptab)] <- 1
      #   print(rownames(tmptab)[rowSums(tmptab, na.rm = TRUE) > 1])
      #   ## multiple idcodefinal in this dataset!
      #   if(vec_vars[c] != "idcodefinal"){
      #     for(j in 1:ncol(tmptab)){
      #       tmptab[,j] <-  as.numeric(colnames(tmptab))[j] * tmptab[,j]
      #     }
      #     tmpvalue <- round(x = rowMeans(tmptab, na.rm = TRUE), digits = 0)
      #     datasetques[datasetques$ID %in% tmpid, vec_vars[c]] <- as.numeric(tmpvalue)
      #   }else{
      #     for(k in 1:nrow(tmptab)){
      #       # print(names(tmptab[k,][tmptab[k,] == 1 & !is.na(tmptab[k,])]))
      #       datasetques[k, vec_vars[c]] <- paste0(names(tmptab[k,][tmptab[k,] == 1 & !is.na(tmptab[k,])]), collapse = ", ")
      #     }
      #   }
      # }else{
      #   datasetques[datasetques$ID %in% tmpid, vec_vars[c]] <- tmpvalue
      # }

      if(listvars[c] %in% notNumeric){
        datasetques[datasetques$ID %in% tmpid, listvars[c]] <- tmpvalue

      }else{
        datasetques[datasetques$ID %in% tmpid, listvars[c]] <- as.numeric(tmpvalue)
      }
    }
  }
  return(datasetques)
}


questionnaire <- questionnairetype(dataset = dat3, listvars = ques_mixed)
head(questionnaire)




##############
# create rowmeans
##############
tmp_mean <- questionnaire %>%
  select(matches("^psc.+h$")) %>%
  rowMeans()
tmp_sd <- questionnaire %>%
  select(matches("^psc.+h$")) %>%
  t() %>% as.data.frame() %>%
  summarise_all(sd) %>%
  t()

questionnaire$pscH_mean <- tmp_mean
questionnaire$pscH_sd <- tmp_sd[,1]


tmp_mean <- questionnaire %>%
  select(matches("^psc.+s$")) %>%
  rowMeans()
tmp_sd <- questionnaire %>%
  select(matches("^psc.+s$")) %>%
  t() %>% as.data.frame() %>%
  summarise_all(sd) %>%
  t()

questionnaire$pscS_mean <- tmp_mean
questionnaire$pscS_sd <- tmp_sd[,1]

##########################################
tmp_mean <- questionnaire %>%
  select(matches("^studienanf-KK")) %>%
  rowMeans()
tmp_sd <- questionnaire %>%
  select(matches("^studienanf-KK"))  %>%
  t() %>% as.data.frame() %>%
  summarise_all(sd) %>%
  t()

questionnaire$studienanfKK_mean <- tmp_mean
questionnaire$studienanfKK_sd <- tmp_sd[,1]


colnames(questionnaire %>%
           select(matches("^studienanf")))

tmp_mean <- questionnaire %>%
  select(matches("^studienanf-SO")) %>%
  rowMeans()
tmp_sd <- questionnaire %>%
  select(matches("^studienanf-SO"))  %>%
  t() %>% as.data.frame() %>%
  summarise_all(sd) %>%
  t()

questionnaire$studienanfSO_mean <- tmp_mean
questionnaire$studienanfSO_sd <- tmp_sd[,1]




tmp_mean <- questionnaire %>%
  select(matches("^studienanf-LD")) %>%
  rowMeans()
tmp_sd <- questionnaire %>%
  select(matches("^studienanf-LD"))  %>%
  t() %>% as.data.frame() %>%
  summarise_all(sd) %>%
  t()

questionnaire$studienanfLD_mean <- tmp_mean
questionnaire$studienanfLD_sd <- tmp_sd[,1]




tmp_mean <- questionnaire %>%
  select(matches("^studienanf-LA")) %>%
  rowMeans()
tmp_sd <- questionnaire %>%
  select(matches("^studienanf-LA"))  %>%
  t() %>% as.data.frame() %>%
  summarise_all(sd) %>%
  t()

questionnaire$studienanfLA_mean <- tmp_mean
questionnaire$studienanfLA_sd <- tmp_sd[,1]



tmp_mean <- questionnaire %>%
  select(matches("^studienanf-WM")) %>%
  rowMeans()
tmp_sd <- questionnaire %>%
  select(matches("^studienanf-WM"))  %>%
  t() %>% as.data.frame() %>%
  summarise_all(sd) %>%
  t()

questionnaire$studienanfWM_mean <- tmp_mean
questionnaire$studienanfWM_sd <- tmp_sd[,1]



tmp_mean <- questionnaire %>%
  select(matches("^studienanf-StA")) %>%
  rowMeans()
tmp_sd <- questionnaire %>%
  select(matches("^studienanf-StA"))  %>%
  t() %>% as.data.frame() %>%
  summarise_all(sd) %>%
  t()

questionnaire$studienanfStA_mean <- tmp_mean
questionnaire$studienanfStA_sd <- tmp_sd[,1]
##############
# match data
##############
questionnaire$IDparticipant <- str_extract(string = questionnaire$IDparticipant, pattern = "[[:digit:]]+$")
CAMindicators$IDparticipant <- str_extract(string = CAMindicators$IDparticipant, pattern = "[[:digit:]]+$")



# questionnaire$learn_litdataba <- as.factor(x = questionnaire$learn_litdataba)
# levels(questionnaire$learn_litdataba) <- c("No", "Yes", "Not sure")

# questionnaire_para <- questionnairetype_para(dataset = dat, datasetques = questionnaire_para)
# questionnaire_para

# write.xlsx(questionnaire_para, file = "questionnaire_para_tp1_2.xlsx", row.names = FALSE)

dat_merged <- left_join(x = questionnaire, y = CAMindicators)
dim(dat_merged); dim(questionnaire); dim(CAMindicators)
glimpse(dat_merged)


write.xlsx(dat_merged, file = "questionnaire_CAMs.xlsx", row.names = FALSE)
write.csv(x = dat_merged, file = "questionnaire_CAMs.csv", row.names = FALSE)

