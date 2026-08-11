# Title: mIBD Global Systematic Review Shiny Application, map overview tab
# Contributor: Lindsay Hracs, Julia Gorospe
# Created: 2026-01-27
# Updated: 2026-08-11
# R version 4.5.0 (2025-04-11)
# Platform: aarch64-apple-darwin20 (64-bit)
# Running under: macOS Sequoia 15.6.1




#--- UI ----------------------------------------------------------------------------#
mapUI <- function(id) {
  ns <- NS(id)
    
    card(
      style = "margin-top: 10px;", height = "77vh",
      
      layout_sidebar(
        
        sidebar = sidebar(width = "30%",
                          
                          layout_columns(
                            actionButton(inputId = ns("defs"),
                                         label = "Definitions",
                                         icon = icon("book")),
                            actionButton(inputId = ns("help"),
                                         label = "Help",
                                         icon = icon("circle-question"))
                          ),
                          
                          hr(style = "border-top: 2px solid #363538; margin: 0;"),
                          
                          HTML("<span style = 'font-size: 100%; color: #316673'><i>Click on a coloured region to update the summary:</i></span>"),
                          
                          card(style = "background-color: #D4DADC;",
                               card_body(
                                 
                                 div(class = "card",
                                     style = "background-color: #F6F6F6; padding: 15px; line-height: 125%",
                                     span(textOutput(ns("summary")),
                                          style = "font-size: 110%; font-weight: bold; color: #363538"), #display: inline-block;
                                     span(HTML("&nbsp;"), shiny::icon("users", class = "fa-fw"), 
                                          HTML("&nbsp;"), textOutput(ns("sum_cases"), inline = TRUE),
                                          style = "margin-top: 10px;"),
                                     span(HTML("&nbsp;"), shiny::icon("dna", class = "fa-fw"),
                                          HTML("&nbsp;&nbsp;"), textOutput(ns("sum_genes"), inline = TRUE),
                                          style = "margin-top: 6px;")
                                 ),
                                 
                                 card(style = "background-color: #F6F6F6;",
                                      card_body(
                                        plotlyOutput(ns("plot"), height = "28vh", width = "100%") 
                                      )
                                 )
                               )
                          ),
                          
        ), # sidebar
        
        leafletOutput(ns("map"), height = "77vh") %>% withSpinner(),
        class = "p-0",
      ) # layout sidebar
    ) # card
  
} # fluidPage




### Server ----------------------------------------------------------------------------#

mapServer <- function(id) {
  
  moduleServer(id, function(input, output, session) {

    ## Buttons -----------------------------------------
    
    observeEvent(input$help, {
      if (input$help > 0)  {
        showModal(modalDialog(
          title = "How to Use the Interactive Maps",
          HTML("<p style = 'font-size: 150%;'><p>
              The map is interactive! Scroll to zoom in or out. Click and drag to change the visible window. Hover over a coloured country to get a brief summary of mIBD cases and the number of genes reported in a region. Clicking on a coloured country will update the summary in the sidebar to reflect available reports of mIBD in that region.<br><br></p>"),
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
    
    ## Data prep -------------------------------------
    
    country_data <- geo %>% 
      group_by(name) %>% 
      summarize(cases = n(),
                genes = length(unique(gene_name))) %>% 
      ungroup()
  
    
    ## Map -----------------------------------------
    
    # Tips for recolouring polygons: https://github.com/rstudio/leaflet/issues/496
    # Idea for instant update: https://stackoverflow.com/questions/69033403/how-to-refresh-sliderinput-in-shiny-in-real-time-not-only-when-the-sliding-en
    
    labels <- paste0("<span style = 'font-size: 125%;'><b>", country_data$name,"</b>", 
                           "<br>Cases: ", country_data$cases,
                           "<br>Genes: ", country_data$genes) %>% lapply(htmltools::HTML)
    
    # build map
    output$map <- renderLeaflet({
      country_data %>% 
      leaflet(options = leafletOptions(worldCopyJump = TRUE, minZoom = 1, maxZoom = 4, zoomControl = FALSE)) %>%
        addProviderTiles("CartoDB.Positron") %>%
        setView(lng = 30, lat = 20, zoom = 2) %>%
        addPolygons(fillColor = ~map_pal(cases), 
                    fillOpacity = 0.8,
                    color = "#363538",
                    weight = 2,
                    stroke = TRUE,
                    layerId = ~name, # layers have to have unique ids for click_data to work
                    label = ~labels,
                    popup = ~labels) %>%
        addLegend(position = "bottomleft",
                  title = "mIBD Cases",
                  pal = map_pal,
                  values = ~cases,
                  bins = seq(floor(min(country_data$cases, na.rm = TRUE)), ceiling(max(country_data$cases, na.rm = TRUE)), by = 2),
                  labFormat = labelFormat(digits = 0),
                  opacity = 0.65) %>% 
        #addControl("stuff about geo definition",position = "topright",)
        addControl(position = "topright",
          HTML("<div class='collapsible-control'>
                  <strong>Location Definition</strong>
                  <div class='control-content'>
                  <span>Case location determined by:</span>
                    <ol>
                      <li>Birth location</li>
                      <li>Location proband living when diagnosis made</li>
                      <li>Location of hospital managing proband</li>
                      <li>Senior author location</li>
                      <li>Corresponding author location</li>
                    </ol>
                  </div>
                </div>"
          )
        )
    })
    
    
    ## Sidebar ---------------------------------------
    
    # default summary text and plot in side bar
    output$summary <- renderText({"Summary for all regions:"})
    output$sum_cases <- renderText({paste0("  Cases reported: ", nrow(data))})
    output$sum_genes <- renderText({paste0("  Unique genes: ", length(unique(data$gene_name)))})
    
    output$plot <- renderPlotly({
      data %>%
        bar_genes(.) %>%
        layout(plot_bgcolor = "#F6F6F6", paper_bgcolor = "#F6F6F6")})
    
    # update plot when a marker is clicked
    observeEvent(input$map_shape_click, {
      
      # print("Clicked")
      # str(input$map_shape_click)
      
      # subset
      click_data <- data %>%
        filter(data$country == input$map_shape_click$id)
      
      output$summary <- renderText({paste0("Summary for ", unique(click_data$country), ":")})
      output$sum_cases <- renderText({paste0("  Cases reported: ", nrow(click_data))})
      output$sum_genes <- renderText({paste0("  Unique genes: ", length(unique(click_data$gene_name)))})
      
      output$plot <- renderPlotly({
        click_data %>% 
          bar_genes(.) %>% 
          layout(plot_bgcolor = "#F6F6F6", paper_bgcolor = "#F6F6F6")})
      
    })
    
    # Reset
    observeEvent(input$map_click, {
      
      output$summary <- renderText({"Summary for all regions:"})
      output$sum_cases <- renderText({paste0("  Cases reported: ", nrow(data))})
      output$sum_genes <- renderText({paste0("  Unique genes: ", length(unique(data$gene_name)))})
      
      output$plot <- renderPlotly({
        data %>%
          bar_genes(.) %>%
          layout(plot_bgcolor = "#F6F6F6", paper_bgcolor = "#F6F6F6")})
      
    })
    
  }) # server
  
    
}
