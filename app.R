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

# --- Define base data directory on Charité network drive ---
# Try to use Charité network path, fall back to local storage
base_dir <- "\\\\Charite.de\\Centren\\AG\\AG-Euskirchen\\Daten\\IntraOP_Berlin"

if (!dir.exists(base_dir)) {
  message("⚠️ Network path not available, using local 'data' folder instead.")
  base_dir <- "data"
}

if (!dir.exists(base_dir)) dir.create(base_dir, recursive = TRUE, showWarnings = FALSE)
data_dir <- file.path(base_dir)


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
      h4("Recorded Entries for Current Experiment"),
      DTOutput("records"),
      width = 8
    )
  )
)


# Define server logic
server <- function(input, output, session) {
  
  records_rv <- reactiveVal(data.table())
  
  observeEvent(input$save, {
    req(input$experiment_id, input$sample_id)
    
    # Create experiment-specific directory
    exp_dir <- file.path(data_dir, input$experiment_id)
    if (!dir.exists(exp_dir)) dir.create(exp_dir, recursive = TRUE)
    
    # ---- Handle uploaded PDF ----
    uploaded_pdf_name <- NA
    if (!is.null(input$pdf_report)) {
      uploaded_pdf_name <- paste0(input$sample_id, "_uploaded_nanoDx_report.pdf")
      uploaded_pdf_path <- file.path(exp_dir, uploaded_pdf_name)
      file.copy(input$pdf_report$datapath, uploaded_pdf_path, overwrite = TRUE)
    }
    
    # ---- Combine date and time inputs ----
    exp_date <- input$experiment_date
    surgery_start_time <- as.POSIXct(paste(exp_date, input$surgery_time))
    tissue_acquisition_time <- as.POSIXct(paste(exp_date, input$tissue_acquisition_time))
    dna_extraction_start_time <- as.POSIXct(paste(exp_date, input$dna_extraction_time))
    library_prep_time <- as.POSIXct(paste(exp_date, input$library_prep_time))
    sequencing_start_time <- as.POSIXct(paste(exp_date, input$sequencing_time))
    nanodx_pdf_report_time <- as.POSIXct(paste(exp_date, input$nanodx_report_time))
    
    # ---- Create new record ----
    new_entry <- data.table(
      SampleID = input$sample_id,
      ExperimentID = input$experiment_id,
      Suspected_diagnosis = input$suspected_diagnosis,
      Surgery_start_time = as.character(surgery_start_time),
      Tissue_acquisition_time = as.character(tissue_acquisition_time),
      DNA_extraction_start_time = as.character(dna_extraction_start_time),
      Library_prep_time = as.character(library_prep_time),
      Sequencing_start_time = as.character(sequencing_start_time),
      nanoDx_pdf_report_time = as.character(nanodx_pdf_report_time),
      nanoDx_classification = input$nanodx_classification,
      nanoDx_score = input$nanodx_score,
      Uploaded_PDF = ifelse(!is.na(uploaded_pdf_name),
                            paste0("<a href='", file.path(exp_dir, uploaded_pdf_name), "' target='_blank'>", uploaded_pdf_name, "</a>"),
                            ""),
      Timestamp = as.character(Sys.time())
    )
    
    # ---- Save to CSV ----
    csv_file <- file.path(exp_dir, paste0(input$experiment_id, "_intraop_results_reporting.csv"))
    if (file.exists(csv_file)) {
      existing <- fread(csv_file)
      updated <- rbind(existing, new_entry, fill = TRUE)
    } else {
      updated <- new_entry
    }
    fwrite(updated, csv_file)
    
    # ---- Generate automatic PDF summary ----
    pdf_summary_name <- paste0(input$sample_id, "_summary_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf")
    pdf_summary_path <- normalizePath(file.path(exp_dir, pdf_summary_name), mustWork = FALSE)
    
    if (!dir.exists(dirname(pdf_summary_path))) dir.create(dirname(pdf_summary_path), recursive = TRUE)
    
    rmd_content <- glue("
---
title: 'Intraoperative Report Summary'
output: pdf_document
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
    
    rmarkdown::render(
      tmp_rmd,
      output_file = basename(pdf_summary_path),
      output_dir = exp_dir,
      quiet = TRUE
    )
    
    # Add PDF link
    new_entry[, Auto_PDF := paste0("<a href='", pdf_summary_path, "' target='_blank'>Summary PDF</a>")]
    
    # Update reactive table
    records_display <- updated
    records_display$Auto_PDF <- paste0("<a href='", pdf_summary_path, "' target='_blank'>Summary PDF</a>")
    records_rv(records_display)
    
    # Reset inputs
    reset("input_form")
    showNotification("Entry saved and form reset.", type = "message")
  })
  
  
  # ---- Display current experiment's records ----
  output$records <- renderDT({
    req(input$experiment_id)
    csv_file <- file.path(data_dir, input$experiment_id, paste0(input$experiment_id, "_intraop_results_reporting.csv"))
    if (file.exists(csv_file)) {
      dat <- fread(csv_file)
      datatable(dat, escape = FALSE, options = list(pageLength = 10, scrollX = TRUE))
    } else {
      datatable(data.table(Message = "No records yet for this experiment."))
    }
  })
}

# Run app
shinyApp(ui = ui, server = server)

