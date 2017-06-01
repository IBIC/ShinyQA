library(shiny); library(digest)

setwd(getwd())

subjects <- read.csv("all_subjects")

fieldsMandatory <- c("userid", "parrecQA", "rawmoviesQA", "tsnrQA", "meicaQA", "motionQA", "regQA", "Comments")
fieldsAll <- c("userid", "parrecQA", "rawmoviesQA", "tsnrQA", "meicaQA", "motionQA", "regQA", "Comments")
responsesDir <- file.path("/mnt/home/natkoh/ShinyQA/output")
epochTime <- function(){as.integer(Sys.time())}
humanTime <- function(){format(Sys.time(), "%Y%m%d-%H%M%OS")}

saveData <- function(data) {
  fileName <- sprintf("%s_%s.csv", humanTime(), digest::digest(data))
  write.csv(x = data, file = file.path(responsesDir, fileName),
            row.names = FALSE, quote = FALSE)
}

loadData <- function() {
  files <- list.files(file.path(responsesDir), full.names = TRUE)
  data <- lapply(files, read.csv, stringsAsFactors = FALSE)
  #data <- dplyr::rbind_all(data)
  data <- do.call(rbind, data)
  data
}

