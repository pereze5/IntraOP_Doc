# app.R
library(htmltools)
library(shiny)
library(shinyjs)
library(bslib)
library(thematic)
library(ggplot2)
library(tidyr)
library(dplyr)
library(RColorBrewer)
library(data.table)
library(DT)
library(shinyWidgets)
library(rmarkdown)
library(tools)
library(glue)
library(shinyTime)


# Define UI
ui <- fluidPage(
  useShinyjs(),
  titlePanel("Intraoperative Results Reporting"),
  
  HTML("<p><b>Instructions:</b><br>
       • Use the format <code>IOPX</code> for <b>SampleID</b>, where <code>X</code> is the running number.<br>
       • Use the format <code>YYYY_MM_DD_IOPX</code> for <b>ExperimentID</b>.<br>
       • All time fields accept both date and time input.<br>
       • Attach the corresponding nanoDx PDF report when available.<br><br></p>"),
  
  sidebarLayout(
    sidebarPanel(
  div(
    id = "input_form",
    
    textInput("sample_id", "Sample ID (IOPX):"),
    textInput("experiment_id", "Experiment ID (YYYY_MM_DD_IOPX):"),
    dateInput("experiment_date", "Experiment Date:", value = Sys.Date()),
    textInput("suspected_diagnosis", "Suspected Diagnosis:"),

    textInput("surgery_time", "Surgery Time (HH:MM):", placeholder = "e.g. 13:15"),
    textInput("tissue_acquisition_time", "Tissue Acquisition Time (HH:MM):", placeholder = "e.g. 13:45"),
    textInput("dna_extraction_time", "DNA Extraction Start (HH:MM):", placeholder = "e.g. 14:10"),
    textInput("library_prep_time", "Library Prep Time (HH:MM):", placeholder = "e.g. 16:00"),
    textInput("sequencing_time", "Sequencing Start (HH:MM):", placeholder = "e.g. 18:30"),
    textInput("nanodx_report_time", "nanoDx Report Time (HH:MM):", placeholder = "e.g. 21:00"),
    
    textInput("nanodx_classification", "nanoDx Classification:"),
    numericInput("nanodx_score", "nanoDx Score:", value = NA, min = 0, max = 1, step = 0.01),
    fileInput("pdf_report", "Attach nanoDx PDF report:", accept = ".pdf"),
    actionButton("save", "Save Entry", class = "btn-primary")
  ),
  width = 4
),
    
    mainPanel(
  h4("Download Results"),
  downloadButton("download_zip", "Download ZIP File", class = "btn-success"),
  br(), br(),
  h4("Recorded Entries for Current Session"),
  DTOutput("records"),
  width = 8
)
  )
)


# Define server logic
server <- function(input, output, session) {
  useShinyjs()
  records_rv <- reactiveVal(data.table())

  # Helper to validate HH:MM time format
  valid_hhmm <- function(x) grepl("^([01]?\\d|2[0-3]):[0-5]\\d$", x %||% "")
  `%||%` <- function(a, b) if (is.null(a) || is.na(a)) b else a

  # ---- Reactive trigger to prepare download ----
  observeEvent(input$save, {
    req(input$experiment_id, input$sample_id)

    # Validate time fields
    if (!all(
      valid_hhmm(input$surgery_time),
      valid_hhmm(input$tissue_acquisition_time),
      valid_hhmm(input$dna_extraction_time),
      valid_hhmm(input$library_prep_time),
      valid_hhmm(input$sequencing_time),
      valid_hhmm(input$nanodx_report_time)
    )) {
      showNotification("Please enter all times as HH:MM (24h).", type = "error")
      return()
    }

    # Create temporary experiment folder
    exp_dir <- file.path(tempdir(), input$experiment_id)
    dir.create(exp_dir, recursive = TRUE, showWarnings = FALSE)

    # Handle uploaded PDF
    uploaded_pdf_name <- NULL
    if (!is.null(input$pdf_report)) {
      uploaded_pdf_name <- paste0(input$sample_id, "_uploaded_nanoDx_report.pdf")
      file.copy(input$pdf_report$datapath,
                file.path(exp_dir, uploaded_pdf_name),
                overwrite = TRUE)
    }

    # Combine date + times
    exp_date <- input$experiment_date
    surgery_start_time        <- as.POSIXct(paste(exp_date, input$surgery_time))
    tissue_acquisition_time   <- as.POSIXct(paste(exp_date, input$tissue_acquisition_time))
    dna_extraction_start_time <- as.POSIXct(paste(exp_date, input$dna_extraction_time))
    library_prep_time         <- as.POSIXct(paste(exp_date, input$library_prep_time))
    sequencing_start_time     <- as.POSIXct(paste(exp_date, input$sequencing_time))
    nanodx_pdf_report_time    <- as.POSIXct(paste(exp_date, input$nanodx_report_time))

    # Create record
    new_entry <- data.table(
      SampleID                  = input$sample_id,
      ExperimentID              = input$experiment_id,
      Suspected_diagnosis       = input$suspected_diagnosis,
      Surgery_start_time        = as.character(surgery_start_time),
      Tissue_acquisition_time   = as.character(tissue_acquisition_time),
      DNA_extraction_start_time = as.character(dna_extraction_start_time),
      Library_prep_time         = as.character(library_prep_time),
      Sequencing_start_time     = as.character(sequencing_start_time),
      nanoDx_pdf_report_time    = as.character(nanodx_pdf_report_time),
      nanoDx_classification     = input$nanodx_classification,
      nanoDx_score              = input$nanodx_score,
      Timestamp                 = as.character(Sys.time())
    )

    # Save CSV
    csv_file <- file.path(exp_dir, paste0(input$experiment_id, "_intraop_results_reporting.csv"))
    fwrite(new_entry, csv_file)

    # Generate summary report (always HTML to avoid LaTeX dependency)
    rmd_content <- glue("
---
title: 'Intraoperative Report Summary'
output: html_document
---

**Sample ID:** {input$sample_id}  
**Experiment ID:** {input$experiment_id}  
**Suspected Diagnosis:** {input$suspected_diagnosis}  

| Step | Time |
|------|------|
| Surgery Start | {surgery_start_time} |
| Tissue Acquisition | {tissue_acquisition_time} |
| DNA Extraction Start | {dna_extraction_start_time} |
| Library Prep | {library_prep_time} |
| Sequencing Start | {sequencing_start_time} |
| nanoDx Report | {nanodx_pdf_report_time} |

**nanoDx Classification:** {input$nanodx_classification}  
**nanoDx Score:** {input$nanodx_score}  

Generated on: {Sys.time()}
")
    tmp_rmd <- tempfile(fileext = ".Rmd")
    writeLines(rmd_content, tmp_rmd)
    summary_file <- file.path(exp_dir, paste0(input$sample_id, "_summary.html"))
    rmarkdown::render(tmp_rmd, output_file = summary_file, quiet = TRUE)

    # Create zip archive for download
    zip_path <- tempfile(fileext = ".zip")
    zip::zipr(zip_path, files = list.files(exp_dir, full.names = TRUE))

    # Store path so downloadHandler can use it
    records_rv(zip_path)

    # Reset form
    reset("input_form")
    showNotification("Entry saved — ZIP ready for download below.", type = "message")
  })

  # ---- Download ZIP ----
  output$download_zip <- downloadHandler(
    filename = function() {
      paste0(input$experiment_id, "_IntraOp_Report.zip")
    },
    content = function(file) {
      req(records_rv())
      file.copy(records_rv(), file)
    },
    contentType = "application/zip"
  )

  # ---- Show table ----
  output$records <- renderDT({
    if (!is.null(records_rv())) {
      datatable(data.table(File = basename(records_rv())),
                options = list(dom = "t"),
                rownames = FALSE)
    } else {
      datatable(data.table(Message = "No saved entries yet."),
                options = list(dom = "t"),
                rownames = FALSE)
    }
  })
}


# Run app
shinyApp(ui = ui, server = server)

