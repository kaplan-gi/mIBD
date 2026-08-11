# Title: mIBD Global Systematic Review Shiny Application, data explorer tab
# Contributor: Lindsay Hracs, Julia Gorospe
# Created: 2026-06-17
# Updated: 2026-07-30
# R version 4.5.0 (2025-04-11)
# Platform: aarch64-apple-darwin20 (64-bit)
# Running under: macOS Sequoia 15.6.1




#--- UI ----------------------------------------------------------------------------#
dataUI <- function(id) {
  ns <- NS(id)
  
  card(style = "margin-top: 10px", height = "77vh",
       
       layout_sidebar(
         
         sidebar = sidebar(width = "30%",
                           
                           # buttons
                           layout_columns(
                             actionButton(inputId = ns("defs"),
                                          label = "Definitions",
                                          icon = icon("book")),
                             actionButton(inputId = ns("help"),
                                          label = "Help",
                                          icon = icon("circle-question"))
                           ),
                           
                           hr(style = "border-top: 2px solid #363538; margin: 0;"),
                           
                           
                           HTML("<span style = 'font-size: 100%; color: #316673'><i>Make selections to subset the data:</i></span>"),
                           
                           
                           
                           # inputs
                           pickerInput(
                             inputId = ns("select_disease"),
                             label = HTML("<span style = 'font-size: 110%;'><b>IBD Phenotype:</b></span>"),
                             choices = unique(data$IBD_subtype),
                             selected = unique(data$IBD_subtype),
                             #selected = NULL,
                             options = pickerOptions(
                               actionsBox = TRUE,
                               noneSelectedText = "All phenotypes",
                               selectedTextFormat = "count > 3"
                             ),
                             multiple = TRUE
                           ),

                           pickerInput(
                             inputId = ns("select_sex"),
                             label = HTML("<span style = 'font-size: 110%;'><b>Sex:</b></span>"),
                             choices = unique(data$sex),
                             selected = unique(data$sex),
                             #selected = NULL,
                             options = pickerOptions(
                               actionsBox = TRUE,
                               noneSelectedText = "All sexes",
                             ),
                             multiple = TRUE
                           ),
                           
                           pickerInput(
                             inputId = ns("select_gene"),
                             label = HTML("<span style = 'font-size: 110%;'><b>HUGO Gene:</b></span>"),
                             choices = unique(data$gene_name),
                             selected = unique(data$gene_name),
                             #selected = NULL,
                             options = pickerOptions(
                               actionsBox = TRUE,
                               noneSelectedText = "All genes",
                               selectedTextFormat = "count > 3"
                             ),
                             multiple = TRUE
                           ),

                           pickerInput(
                             inputId = ns("select_country"),
                             label = HTML("<span style = 'font-size: 110%;'><b>Geographic Region:</b></span>"),
                             choices = unique(data$country),
                             selected = unique(data$country),
                             #selected = NULL,
                             options = pickerOptions(
                               actionsBox = TRUE,
                               noneSelectedText = "All regions",
                               selectedTextFormat = "count > 3"
                             ),
                             multiple = TRUE
                           ),
                           
                           hr(style = "border-top: 2px solid #363538; margin: 0;"),
                           
                           # download button
                           layout_columns(
                             downloadButton(ns("download"),
                                            icon = shiny::icon("download"),
                                            label = " Selection .CSV"),
                             downloadButton(ns("download_all"),
                                            icon = shiny::icon("download"),
                                            label = " All Data .CSV")
                           ),
                           
                           
         ), # sidebar
         dataTableOutput(ns("table"), height = "58vh", width = "100%") %>% withSpinner()
       ) # layout sidebar
    ) # card
} # fluidPage




#--- Server ----------------------------------------------------------------------------#
dataServer <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    
    observeEvent(input$help, {
      if (input$help > 0)  {
        showModal(modalDialog(
          title = "How to Use the Data Explorer",
          HTML("<p style = 'font-size: 150%;'><p>
            Make selections in the sidebar to view specific subsets of the data. The data may also be searched by key words using the box at the top right of the table. Records may be highlighted with a click.<br><br>
            Use the download buttons at the bottom of the sidebar to export the data as a .csv file. The <i>Selection</i> button will download the data visible in the table while the <i>All Data</i> button will download the complete dataset.<br>
               </p>"),
          size = "l",
          footer = modalButton("Close"),
          easyClose = TRUE
        ))
      } else{}


    })


    observeEvent(input$defs, {
      if (input$defs > 0)  {
        showModal(modalDialog(
          title = "Useful Definitions",
          HTML(definitions),
          size = "l",
          footer = modalButton("Close"),
          easyClose = TRUE
        ))
      } else{}


    })
    
    ## Data table -----------------------------------------
    
    # # list out the columns that can be filtered
    # filter_map <- list(
    #   select_disease = "IBD_subtype",
    #   select_sex     = "sex",
    #   select_gene    = "gene_name",
    #   select_country = "country"
    # )
    # 
    # # filter dataset
    # data_react <- reactive({
    #   df <- data
    #   
    #   for (id in names(filter_map)) {
    #     vals <- input[[id]]
    #     
    #     if (length(vals) > 0) {
    #       df <- df[df[[filter_map[[id]]]] %in% vals, , drop = FALSE]
    #     }
    #   }
    #   
    #   df
    #   
    # })
    # 
    # observe({
    #   
    #   for (id in names(filter_map)) {
    #     
    #     ## Always start from the original data
    #     df <- data
    #     
    #     ## Apply every OTHER filter
    #     for (other in setdiff(names(filter_map), id)) {
    #       
    #       vals <- input[[other]]
    #       
    #       if (length(vals) > 0) {
    #         df <- df[df[[filter_map[[other]]]] %in% vals, , drop = FALSE]
    #       }
    #     }
    #     
    #     choices <- sort(unique(df[[filter_map[[id]]]]))
    #     
    #     updatePickerInput(
    #       session = session,
    #       inputId = id,
    #       choices = choices,
    #       selected = intersect(isolate(input[[id]]), choices)
    #     )
    #   }
    #   
    # })
    
    
    # create dataset based on user input for table
    data_react <- reactive({
      data %>%
        filter(IBD_subtype %in% input$select_disease,
               sex %in% input$select_sex,
               gene_name %in% input$select_gene,
               country %in% input$select_country)
    })
  
    
    # build table
    output$table <- renderDataTable({
      data_react() %>% 
        mutate(link = paste0("<a href='https://doi.org/", DOI,"' target='_blank'><i class='fa fa-external-link' style='font-size:18px; color: #408697'></i></a>")) %>%
        mutate(`Protein Change` = ifelse(!is.na(aa_change_2), paste0(aa_change, ", ", aa_change_2), aa_change)) %>% 
        select(Link = link, Gene = gene_name, `Protein Change`, Country = country, `IBD Type` = IBD_subtype, Sex = sex, EIC = eic) %>% 
        datatable(., 
                  rownames = FALSE,  escape = FALSE, class = "hover nowrap cell-border stripe",
                  style = "bootstrap", extensions = "Scroller",
                  options = list(dom = "frti",
                                 scrollX = TRUE,
                                 scrollY = "60vh",
                                 scroller = TRUE)
                  ) # to shorten long text
    })
    
    
    # download buttons
    output$download <- downloadHandler(
      filename = function(){
        paste("Nambu-2022_mIBD-SLR-Selection_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file){
        write.csv(isolate(data_react()), file, row.names = FALSE)
      }
    )
    
    output$download_all <- downloadHandler(
      filename = function(){
        paste("Nambu-2022_mIBD-SLR_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file){
        write.csv(data, file, row.names = FALSE)
      }
    )
    
  }) # server
  
    
}
