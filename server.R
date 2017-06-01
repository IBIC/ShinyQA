library(shiny); library(DT); library(shinyjs)
library(dplyr); library(digest); library(knitr)

setwd(getwd())

PROJECTDIR <- file.path("/mnt/panuc/udallp2")

fieldsMandatory <- c("subid", "taskid", "sessionid", "userid", "parrecQA", 
                     "rawmoviesQA", "tsnrQA", "meicaQA", "motionQA", "regQA", "Comments")
fieldsAll <- c("subid", "taskid", "sessionid", "userid", "parrecQA", 
               "rawmoviesQA", "tsnrQA", "meicaQA", "motionQA", "regQA", "Comments")
responsesDir <- "output"
epochTime <- function(){as.integer(Sys.time())}
humanTime <- function(){format(Sys.time(), "%Y%m%d-%H%M")}

saveData <- function(data) {
  fileName <- sprintf("%s_%s.csv", humanTime(), digest::digest(data))
  write.csv(x = data, file = file.path(responsesDir, fileName),
            row.names = FALSE, quote = FALSE)
}

loadData <- function() {
  files <- list.files(file.path(responsesDir), full.names = TRUE)
  data <- lapply(files, read.csv, stringsAsFactors = FALSE)
  data <- dplyr::rbind_all(data)
  data
}

function(input, output, session) {
  
  #subid
  subject <- reactive({ input$subid })
  output$subject <- renderText({ input$subid })
  output$subviewtext <- renderText({ paste("You are now viewing subject ", 
                                           input$subid, "for", input$taskid, 
                                           ":", input$sessionid) })
  
  # change task list based on PD vs. control
  observe({
    subj <- input$subid
    file_path <- paste(PROJECTDIR, "/subjects/", subj, "/session1/0_group", sep="")
    subj_type <- readLines(file(file_path))
    if (subj_type == "PATIENT") {
      subj <- list("rest_on", "rest_off", "axcpt_on", "axcpt_off")
    } else {
      subj <- list("rest_on", "axcpt_on")
    }
    updateSelectInput(session, "taskid", choices = subj)
  })
  
  # generate output for flagged subjects tab
  output$most_warnings <- renderUI({
    # WRITE -> viewing flagged subjects for "session1"
    #file_path <- "/mnt/panuc/udallp2/all_subjects"
    #conn <- file(file_path)
    #subjects <- readLines(conn)
    #on.exit(close(conn))  
    # QA_stats dataframe 
    filename <- paste("/mnt/panuc/udallp2/subjects/", input$subid, "/", input$sessionid,
                      "/QA/", input$subid, "_QA_stats.csv", sep = "")
    QA_stats <- read.csv(filename, header = TRUE)
    subjects <- c("100023", "100044", "100054", "100089", "100157", "100165", "110213")
    str <- c()
    i <- 1
    for (subject in subjects) {
      flagged_indices <- which(QA_stats$flag == TRUE)
      warnings <- paste(QA_stats[flagged_indices, "measure"], collapse = ", ")
      if (warnings != "") {
        str[[i]] <- paste("<strong>", subject, "</strong>", ": ", warnings, sep = "")
        i <- i + 1
      }
    }
    HTML(paste(str, collapse = "<br><br>"))
  })
  
  output$warnings <- renderUI({
    filename <- paste("/mnt/panuc/udallp2/subjects/", input$subid, "/", input$sessionid,
                      "/QA/", input$subid, "_QA_stats.csv", sep = "")
    QA_stats <- read.csv(filename, header = TRUE)
    tasks <- c("rest_on", "rest_off", "axcpt_on", "axcpt_off")
    tasks <- tasks[tasks != input$taskid]
    flagged_indices <- which(QA_stats$flag == TRUE)
    flagged_indices <- flagged_indices[! flagged_indices %in% flagged_indices[which(grepl(tasks[1], QA_stats[flagged_indices, "measure"]))]]
    flagged_indices <- flagged_indices[! flagged_indices %in% flagged_indices[which(grepl(tasks[2], QA_stats[flagged_indices, "measure"]))]]
    flagged_indices <- flagged_indices[! flagged_indices %in% flagged_indices[which(grepl(tasks[3], QA_stats[flagged_indices, "measure"]))]]
    warnings <- paste(QA_stats[flagged_indices, "measure"], collapse = ", ")
    HTML(warnings)
  })
  
  #acquisition parameters
  output$acqpar <- renderUI({
    # QA_stats dataframe 
    filename <- paste("/mnt/panuc/udallp2/subjects/", input$subid, "/", input$sessionid,
                      "/QA/", input$subid, "_QA_stats.csv", sep = "")
    QA_stats <- read.csv(filename, header = TRUE)
    rownames(QA_stats) <- QA_stats[,"measure"]
    on_or_off <- ifelse(grepl("on",input$taskid), "on", "off")
    direction <- ifelse(QA_stats[paste("Task_", on_or_off, "_slice_orientation",sep=""),"data"]==1, "transverse", "INCORRECT")
    content <- paste("<pre>Subject ID                 :  ", input$subid, "<br>",sep="")
    content <- paste(content, "No. of Volumes             :  ", QA_stats[paste("Task_", on_or_off, "_dyn_scans",sep=""), "data"],"<br>",sep="")
    content <- paste(content, "Repetition Time            :  ", QA_stats[paste("Task_", on_or_off, "_TR",sep=""),"data"],"<br>",sep="")
    content <- paste(content, "Echo Time                  :  ", QA_stats[paste("Task_", on_or_off, "_TE",sep=""),"data"],"<br>",sep="")
    content <- paste(content, "Preparation Direction      :  ", direction,"<br>",sep="")
    content <- paste(content, "FOV (RL)                   :  ", QA_stats[paste("Task_", on_or_off, "_FOV_RL",sep=""),"data"],"<br>",sep="")
    content <- paste(content, "FOV (AP)                   :  ", QA_stats[paste("Task_", on_or_off, "_FOV_AP",sep=""),"data"],"<br>",sep="")
    content <- paste(content, "FOV (FH)                   :  ", QA_stats[paste("Task_", on_or_off, "_FOV_FH",sep=""),"data"],"<br>",sep="")
    content <- paste(content, "Voxel Size                 :  ", QA_stats[paste("Task_", on_or_off, "_voxel_size",sep=""),"data"],"<br>",sep="")
    HTML(content)
    })
  
  #quantitative measures
  output$motionmetrics <- renderUI({
    outlier_volumes_e002_file_path <- paste("/mnt/panuc/udallp2/subjects/", input$subid, "/", input$sessionid,
                                            "/", input$taskid, "/", input$taskid, "_e002_outliers_volumes.txt", sep="")
    conn <- file(outlier_file_path)
    outlier_volumes_e002 <- readLines(conn)
    on.exit(close(conn))
    #read fd outliers
    fd_e002_spikes_file_path <- paste("/mnt/panuc/udallp2/subjects/", input$subid, "/", input$sessionid,
                                            "/", input$taskid, "/", input$taskid, "_e002_fd_spike_vols", sep="")
    conn <- file(fd_e002_spikes_file_path)
    fd_e002_spikes <- readLines(conn)
    on.exit(close(conn))
    fd_tsoc_spikes_file_path <- paste("/mnt/panuc/udallp2/subjects/", input$subid, "/", input$sessionid,
                                      "/", input$taskid, "/", input$taskid, "_e00213_tsoc_fd_spike_vols", sep="")
    conn <- file(fd_tsoc_spikes_file_path)
    fd_tsoc_spikes <- readLines(conn)
    on.exit(close(conn))
    fd_medn_spikes_file_path <- paste("/mnt/panuc/udallp2/subjects/", input$subid, "/", input$sessionid,
                                      "/", input$taskid, "/", input$taskid, "_e00213_medn_fd_spike_vols", sep="")
    conn <- file(fd_medn_spikes_file_path)
    fd_medn_spikes <- readLines(conn)
    on.exit(close(conn))
    # read dvars outliers
    dvars_e002_spikes_file_path <- paste("/mnt/panuc/udallp2/subjects/", input$subid, "/", input$sessionid,
                                      "/", input$taskid, "/", input$taskid, "_e002_dvars_spike_vols", sep="")
    conn <- file(dvars_e002_spikes_file_path)
    dvars_e002_spikes <- readLines(conn)
    on.exit(close(conn))
    dvars_tsoc_spikes_file_path <- paste("/mnt/panuc/udallp2/subjects/", input$subid, "/", input$sessionid,
                                      "/", input$taskid, "/", input$taskid, "_e00213_tsoc_dvars_spike_vols", sep="")
    conn <- file(dvars_tsoc_spikes_file_path)
    dvars_tsoc_spikes <- readLines(conn)
    on.exit(close(conn))
    dvars_medn_spikes_file_path <- paste("/mnt/panuc/udallp2/subjects/", input$subid, "/", input$sessionid,
                                      "/", input$taskid, "/", input$taskid, "_e00213_medn_dvars_spike_vols", sep="")
    conn <- file(dvars_medn_spikes_file_path)
    dvars_medn_spikes <- readLines(conn)
    on.exit(close(conn))
    
    filename <- paste("/mnt/panuc/udallp2/subjects/", input$subid, "/", input$sessionid,
                      "/QA/", input$subid, "_QA_stats.csv", sep = "")
    QA_stats <- read.csv(filename, header = TRUE)
    rownames(QA_stats) <- QA_stats[,"measure"]
    content <- paste("<h5><b>Absolute & Relative Displacement</b></h5>", 
                     "<pre>Mean Absolute Displacement : ", 
                     QA_stats[paste(input$taskid, "_abs_mean_displacement",sep=""), "data"]," mm<br>",
                     "Mean Relative Displacement : ", 
                     QA_stats[paste(input$taskid, "_rel_mean_displacement",sep=""), "data"]," mm</pre>",sep="")
    content <- paste(content,"<h5><b>List of All Outlier Volumes (e002)</b></h5>", 
                     "<pre>", paste(outlier_volumes_e002, collapse = " "), "</pre>",sep="")
    content <- paste(content, "<h5><b>Percentage of Motion (FD, DVARS & SN) Outlier Volumes</b></h5>", 
                     "<pre>", QA_stats[paste(input$taskid, "_percent_outliers",sep=""),"data"], " %</pre>", sep="")
    content <- paste(content, "<h5><b>FD Volume Outliers for e002</b></h5>", 
                     "<pre>", paste(fd_e002_spikes, collapse = " "), "</pre>", sep="")
    content <- paste(content, "<h5><b>FD Volume Outliers for Optimally Combined Time Series</b></h5>", 
                     "<pre>", paste(fd_tsoc_spikes, collapse = " "), "</pre>", sep="")
    content <- paste(content, "<h5><b>FD Volume outliers for Denoised Time Series</b></h5>", 
                     "<pre>", paste(fd_medn_spikes, collapse = " "), "</pre>", sep="")
    content <- paste(content, "<h5><b>DVARS Volume Outliers for e002</b></h5>", 
                     "<pre>", paste(dvars_e002_spikes, collapse = " "), "</pre>", sep="")
    content <- paste(content, "<h5><b>DVARS Volume Outliers for Optimally Combined Time Series</b></h5>", 
                     "<pre>", paste(dvars_tsoc_spikes, collapse = " "), "</pre>", sep="")
    content <- paste(content, "<h5><b>DVARS Volume outliers for Denoised Time Series</b></h5>", 
                     "<pre>", paste(dvars_medn_spikes, collapse = " "), "</pre><br><br/>", sep="")
    HTML(content)
  })
  
  #paths to all QA images
  output$rawe001x <- renderImage({ 
    filename <- normalizePath(file.path(PROJECTDIR, "subjects", 
                                        paste(input$subid, "/", input$sessionid, 
                                              "/QA/images/", input$taskid, 
                                              "_e001_x_animation.gif", sep = ""))) 
    list(src = filename)}, deleteFile = FALSE)
  output$rawe001y <- renderImage({ 
    filename <- normalizePath(file.path(PROJECTDIR, "subjects", 
                                        paste(input$subid, "/", input$sessionid, 
                                              "/QA/images/", input$taskid, 
                                              "_e001_y_animation.gif", sep = "")))
    list(src = filename)}, deleteFile = FALSE)
  output$rawe001z <- renderImage({ 
   filename <- normalizePath(file.path(PROJECTDIR, "subjects", 
                                       paste(input$subid, "/", input$sessionid, 
                                             "/QA/images/", input$taskid,
                                       "_e001_z_animation.gif", sep = "")))
  list(src = filename)}, deleteFile = FALSE)
  output$rawe002x <- renderImage({ 
    filename <- normalizePath(file.path(PROJECTDIR, "subjects", 
                                        paste(input$subid, "/", input$sessionid, 
                                              "/QA/images/", input$taskid,
                                              "_e002_x_animation.gif", sep = "")))
    list(src = filename)}, deleteFile = FALSE)
  output$rawe002y <- renderImage({ 
    filename <- normalizePath(file.path(PROJECTDIR, "subjects", 
                                        paste(input$subid, "/", input$sessionid, 
                                              "/QA/images/", input$taskid,
                                              "_e002_y_animation.gif", sep = "")))
    list(src = filename)}, deleteFile = FALSE)
  output$rawe002z <- renderImage({ 
    filename <- normalizePath(file.path(PROJECTDIR, "subjects", 
                                        paste(input$subid, "/", input$sessionid, 
                                              "/QA/images/", input$taskid,
                                              "_e002_z_animation.gif", sep = "")))
    list(src = filename)}, deleteFile = FALSE)
  output$rawe003x <- renderImage({ 
    filename <- normalizePath(file.path(PROJECTDIR, "subjects", 
                                        paste(input$subid, "/", input$sessionid, 
                                              "/QA/images/", input$taskid,
                                              "_e003_x_animation.gif", sep = "")))
    list(src = filename)}, deleteFile = FALSE)
  output$rawe003y <- renderImage({ 
    filename <- normalizePath(file.path(PROJECTDIR, "subjects", 
                                        paste(input$subid, "/", input$sessionid, 
                                              "/QA/images/", input$taskid,
                                              "_e003_y_animation.gif", sep = "")))
    list(src = filename)}, deleteFile = FALSE)
  output$rawe003z <- renderImage({ 
    filename <- normalizePath(file.path(PROJECTDIR, "subjects", 
                                        paste(input$subid, "/", input$sessionid, 
                                              "/QA/images/", input$taskid,
                                              "_e003_z_animation.gif", sep = "")))
    list(src = filename)}, deleteFile = FALSE)
  output$tsnr.e001x <- renderImage({ 
    filename <- normalizePath(file.path(PROJECTDIR, "subjects", 
                                        paste(input$subid, "/", input$sessionid, 
                                              "/QA/images/", input$taskid, 
                                              "_e001_tsnr_mean_x.gif", sep = "")))
    list(src = filename)}, deleteFile = FALSE)
  output$tsnr.e001y <- renderImage({ 
    filename <- normalizePath(file.path(PROJECTDIR, "subjects", 
                                        paste(input$subid, "/", input$sessionid, 
                                              "/QA/images/", input$taskid, 
                                              "_e001_tsnr_mean_y.gif", sep = "")))
    list(src = filename)}, deleteFile = FALSE)
  output$tsnr.e001z <- renderImage({ 
    filename <- normalizePath(file.path(PROJECTDIR, "subjects", 
                                        paste(input$subid, "/", input$sessionid, 
                                              "/QA/images/", input$taskid, 
                                              "_e001_tsnr_mean_z.gif", sep = "")))
    list(src = filename)}, deleteFile = FALSE)
  output$tsnr.e002x <- renderImage({ 
    filename <- normalizePath(file.path(PROJECTDIR, "subjects", 
                                        paste(input$subid, "/", input$sessionid, 
                                              "/QA/images/", input$taskid, 
                                              "_e002_tsnr_mean_x.gif", sep = "")))
    list(src = filename)}, deleteFile = FALSE)
  output$tsnr.e002y <- renderImage({ 
    filename <- normalizePath(file.path(PROJECTDIR, "subjects", 
                                        paste(input$subid, "/", input$sessionid, 
                                              "/QA/images/", input$taskid, 
                                              "_e002_tsnr_mean_y.gif", sep = "")))
    list(src = filename)}, deleteFile = FALSE)
  output$tsnr.e002z <- renderImage({ 
    filename <- normalizePath(file.path(PROJECTDIR, "subjects", 
                                        paste(input$subid, "/", input$sessionid, 
                                              "/QA/images/", input$taskid, 
                                              "_e002_tsnr_mean_z.gif", sep = "")))
    list(src = filename)}, deleteFile = FALSE)
  output$tsnr.e003x <- renderImage({ 
    filename <- normalizePath(file.path(PROJECTDIR, "subjects", 
                                        paste(input$subid, "/", input$sessionid, 
                                              "/QA/images/", input$taskid, 
                                              "_e003_tsnr_mean_x.gif", sep = "")))
    list(src = filename)}, deleteFile = FALSE)
  output$tsnr.e003y <- renderImage({ 
    filename <- normalizePath(file.path(PROJECTDIR, "subjects", 
                                        paste(input$subid, "/", input$sessionid, 
                                              "/QA/images/", input$taskid, 
                                              "_e003_tsnr_mean_y.gif", sep = "")))
    list(src = filename)}, deleteFile = FALSE)
  output$tsnr.e003z <- renderImage({ 
    filename <- normalizePath(file.path(PROJECTDIR, "subjects", 
                                        paste(input$subid, "/", input$sessionid, 
                                              "/QA/images/", input$taskid, 
                                              "_e003_tsnr_mean_z.gif", sep = "")))
    list(src = filename)}, deleteFile = FALSE)
  
  output$meica.tsocx <- renderImage({ 
    filename <- normalizePath(file.path(PROJECTDIR, "subjects", 
                                        paste(input$subid, "/", input$sessionid, 
                                              "/QA/images/", input$taskid,
                                              "_e00213_tsoc_x_animation.gif", sep = "")))
    list(src = filename)}, deleteFile = FALSE)
  output$rmeica.tsocy <- renderImage({ 
    filename <- normalizePath(file.path(PROJECTDIR, "subjects", 
                                        paste(input$subid, "/", input$sessionid, 
                                              "/QA/images/", input$taskid,
                                              "rest-on_e00213_tsoc_y_animation.gif", sep = "")))
    list(src = filename)}, deleteFile = FALSE)
  output$meica.tsocz <- renderImage({ 
    filename <- normalizePath(file.path(PROJECTDIR, "subjects", 
                                        paste(input$subid, "/", input$sessionid, 
                                              "/QA/images/", input$taskid,
                                              "_e00213_tsoc_z_animation.gif", sep = "")))
    list(src = filename)}, deleteFile = FALSE)
  output$meica.mednx <- renderImage({ 
    filename <- normalizePath(file.path(PROJECTDIR, "subjects", 
                                        paste(input$subid, "/", input$sessionid, 
                                              "/QA/images/", input$taskid,
                                              "_e00213_medn_x_animation.gif", sep = "")))
    list(src = filename)}, deleteFile = FALSE)
  output$meica.medny <- renderImage({ 
    filename <- normalizePath(file.path(PROJECTDIR, "subjects", 
                                        paste(input$subid, "/", input$sessionid, 
                                              "/QA/images/", input$taskid,
                                              "_e00213_medn_y_animation.gif", sep = "")))
    list(src = filename)}, deleteFile = FALSE)
  output$meica.mednz <- renderImage({ 
    filename <- normalizePath(file.path(PROJECTDIR, "subjects", 
                                        paste(input$subid, "/", input$sessionid, 
                                              "/QA/images/", input$taskid,
                                              "_e00213_medn_z_animation.gif", sep = "")))
    list(src = filename)}, deleteFile = FALSE)
  output$meica.mefcx <- renderImage({ 
    filename <- normalizePath(file.path(PROJECTDIR, "subjects", 
                                        paste(input$subid, "/", input$sessionid, 
                                              "/QA/images/", input$taskid,
                                              "_e00213_mefc_x_animation.gif", sep = "")))
    list(src = filename)}, deleteFile = FALSE)
  output$meica.mefcy <- renderImage({ 
    filename <- normalizePath(file.path(PROJECTDIR, "subjects", 
                                        paste(input$subid, "/", input$sessionid, 
                                              "/QA/images/", input$taskid,
                                              "_e00213_mefc_y_animation.gif", sep = "")))
    list(src = filename)}, deleteFile = FALSE)
  output$meica.mefcz <- renderImage({ 
    filename <- normalizePath(file.path(PROJECTDIR, "subjects", 
                                        paste(input$subid, "/", input$sessionid, 
                                              "/QA/images/", input$taskid,
                                              "_e00213_mefc_z_animation.gif", sep = "")))
    list(src = filename)}, deleteFile = FALSE)
  
  output$motion.rot <- renderImage({ 
    filename <- normalizePath(file.path(PROJECTDIR, "subjects", 
                                        paste(input$subid, "/", input$sessionid, 
                                              "/QA/images/", input$taskid, 
                                              "_e002_MotionGraphRotations.gif", sep = "")))
    list(src = filename)}, deleteFile = FALSE)
  output$motion.trans <- renderImage({ 
    filename <- normalizePath(file.path(PROJECTDIR, "subjects", 
                                        paste(input$subid, "/", input$sessionid, 
                                              "/QA/images/", input$taskid, 
                                              "_e002_MotionGraphTranslations.gif", sep = "")))
    list(src = filename)}, deleteFile = FALSE)
  output$motion.fd <- renderImage({ 
    filename <- normalizePath(file.path(PROJECTDIR, "subjects", 
                                        paste(input$subid, "/", input$sessionid, 
                                              "/QA/images/", input$taskid, 
                                              "_e002_FramewiseDisplacement.gif", sep = "")))
    list(src = filename)}, deleteFile = FALSE)
  output$motion.dvars02 <- renderImage({ 
    filename <- normalizePath(file.path(PROJECTDIR, "subjects", 
                                        paste(input$subid, "/", input$sessionid, 
                                              "/QA/images/", input$taskid, 
                                              "_e002_DVARS_raw.gif", sep = "")))
    list(src = filename)}, deleteFile = FALSE)
  output$motion.dvars.tsocmedn <- renderImage({ 
    filename <- normalizePath(file.path(PROJECTDIR, "subjects", 
                                        paste(input$subid, "/", input$sessionid, 
                                              "/QA/images/", input$taskid, 
                                              "_e002_DVARS_dn-oc.gif", sep = "")))
    list(src = filename)}, deleteFile = FALSE)
  
  output$tsocT1 <- renderImage({ 
    filename <- normalizePath(file.path(PROJECTDIR, "subjects", 
                                        paste(input$subid, "/", input$sessionid, 
                                              "/QA/images/", input$taskid,
                                              "_e00213_tsoc_reoriented_to_T1.gif", sep = "")))
    list(src = filename)}, deleteFile = FALSE)
  output$tsocCT <- renderImage({ 
    filename <- normalizePath(file.path(PROJECTDIR, "subjects", 
                                        paste(input$subid, "/", input$sessionid, 
                                              "/QA/images/", input$taskid,
                                              "_e00213_tsoc_to_CT_epireg_ants.gif", sep = "")))
    list(src = filename)}, deleteFile = FALSE)
  output$tsocMNI <- renderImage({
    filename <- normalizePath(file.path(PROJECTDIR, "subjects", 
                                        paste(input$subid, "/", input$sessionid, 
                                              "/QA/images/", input$taskid,
                                              "_e00213_tsoc_reoriented_to_mni_epireg_ants.gif", sep = "")))
    list(src = filename)}, deleteFile = FALSE)
  
  #form
  observe({
    mandatoryFilled <- vapply(fieldsMandatory, function(x) {
      !is.null(input[[x]]) && input[[x]] != ""},
      logical(1))
    mandatoryFilled <- all(mandatoryFilled)
    
    shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
  })
  
  formData <- reactive({
    data <- sapply(fieldsAll, function(x) input[[x]])
    data <- c(data, timestamp = humanTime())
    data <- t(data)
    data
  })
  
  observeEvent(input$submit, {
    shinyjs::disable("submit")
    shinyjs::show("submit_msg")
    shinyjs::hide("error")
    
    tryCatch({
      saveData(formData())
      shinyjs::reset("form")
      shinyjs::hide("form")
      shinyjs::show("submitted_msg")
    }, error = function(err) {
      shinyjs::html("error_msg", err$message)
      shinyjs::show(id = "error", anim = TRUE)
    }, finally = {
      shinyjs::enable("submit")
      shinyjs::hide("submit_msg")
    })
    
    })
  
  observeEvent(input$submit_another, {
    shinyjs::show("form")
    shinyjs::hide("submitted_msg")
  })
  
  #logsheet table display
  output$responsesTable <- DT::renderDataTable(
    loadData(),
    rownames = FALSE,
    options = list(searching = FALSE, lengthChange = FALSE)
  )
  
  #download data
  output$downloadData <- downloadHandler(
  function() {
    filename = sprintf("rest-on_QALogSheet_%s.csv", humanTime())
  },
  content = function(file) {
    write.csv(loadData(), file, row.names = FALSE)
  })
  
}