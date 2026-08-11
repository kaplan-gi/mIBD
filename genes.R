# Title: mIBD Global Systematic Review Shiny Application, gene-specific plots
# Contributor: Lindsay Hracs, Julia Gorospe
# Created: 2026-01-27
# Updated: 2026-08-11
# R version 4.5.0 (2025-04-11)
# Platform: aarch64-apple-darwin20 (64-bit)
# Running under: macOS Sequoia 15.6.1


# UI -------------------------------------------------------------------------------------------
genesUI <- function(id) {
    ns <- NS(id)
    
    #page_fillable(
    div(
      
        layout_columns(
            style = "padding-top: 10px",
            col_widths = c(3, 6, 3), 
            height = "28vh",
            fillable = TRUE,
            
            layout_columns(
                col_widths = c(12, 12), # 2 rows
                gap = 0,
                
                # gene selection
                div(
                  class = "primary-selectize",
                  selectizeInput(
                    inputId = ns("gene"), 
                    width = "100%",
                    label = HTML("<span style = 'font-size: 115%; color: #363538'><i>Enter a gene:</i></span>"),
                    choices = unique(data$gene_name),
                    options = list(
                      create = TRUE,      # allow new values
                      placeholder = "Type or select..."
                    )
                  )
                ),
                
                # summary card
                card(
                  div(
                    div(
                      style = "display: flex; align-items: center; gap: 10px; margin-left: 10px;",
                      shiny::icon("users", class = "fa-fw", style = "line-height:1;"),
                      textOutput(ns("sum_cases"), inline = TRUE)
                    ),
                    
                    div(
                      style = "display: flex; align-items: center; gap: 10px; margin-left: 10px;",
                      shiny::icon("dna", class = "fa-fw"),
                      textOutput(ns("sum_vars"), inline = TRUE)
                    ),
                    
                    div(
                      style = "display: flex; align-items: center; gap: 10px; margin-left: 10px;",
                      shiny::icon("book-open", class = "fa-fw"),
                      textOutput(ns("sum_articles"), inline = TRUE)
                    )
                  )
                )
            ),
            
            
            # A) GENE MAP
            card(
              #full_screen = TRUE,
              card_body(
                g3LollipopOutput(ns("lollipop")),
                as_fill_carrier()
              )
            ),
            
            # B) PHENOTYPE
            card(
              full_screen = TRUE,
              card_header("IBD Phenotype"),
              card_body(plotlyOutput(ns("pie"), width = "100%"))
            )
        ),
        
        layout_columns(
          col_widths = c(4, 4, 4), 
          height = "48vh",
          fillable = TRUE,
            
          # C) GEOGRAPHIC MAP
          card(
            full_screen = TRUE,
            card_header("Geographic Distribution"),
            card_body(
              leafletOutput(ns("map")),
              as_fill_carrier(),
              class = "p-0"
            )
          ),
          
          # D) THERAPIES
          navset_card_tab(
            nav_panel(shiny::icon("circle-info"), p("The medication/therapeutic information presented in this application is based on published reports identified through a systematic review. It is provided for informational and research purposes only and should not be interpreted as a treatment recommendation, prescribing guidance, or evidence that a medication is safe or effective for a particular patient or condition.")),
            nav_spacer(),
            nav_panel(
              title = "Therapeutic Attempts",

              div(
                dataTableOutput(ns("table")),
                
                div(
                  style = "margin-top: 1rem; line-height: 1.4;",
                  HTML('
        <div style="color:forestgreen;">
          <i class="fa fa-check-double" style="width:1.2em;"></i>
          Therapy reported to be effective
        </div>
        <div style="color:darkred;">
          <i class="fa fa-xmark" style="width:1.2em;"></i>
          Therapy reported to be ineffective
        </div>
      ')
                )
              )
            )
          ),
          
          # E) SYMPTOMS
          card(
            full_screen = TRUE,
            card_header("Extraintestinal Comorbidities"),
            card_body(
              plotlyOutput(ns("bar")),
              as_fill_carrier()
            )
          )

      )
          
  ) #div
} 


# Server ----------------------------------------------------------------------------------------
genesServer <- function(id) {
    
    moduleServer(id, function(input, output, session) {
      
      # subset data by gene
      gene_data <- reactive({
        data %>% 
          filter(gene_name == input$gene)
      })
      
      data_eic_react <- reactive({
        data_eic %>% 
          filter(gene_name == input$gene)
      })
      
      data_tr_react <- reactive({
        data_tr %>% 
          filter(gene_name == input$gene)
      })

      # summary box
      output$sum_cases <- renderText({paste0("Cases reported: ", length(unique(gene_data()$record_number)))})
      output$sum_vars <- renderText({paste0("Unique variants: ", length(unique(na.omit(gene_data()$aa_change)))+length(unique(na.omit(gene_data()$aa_change_2))))})
      output$sum_articles <- renderText({paste0("Publications: ", length(unique(gene_data()$DOI)))})
  
      
      # A) GENE MAP, build lollipop chart
      output$lollipop <- renderG3Lollipop({
        #mutation.dat <- getMutationsFromCbioportal("msk_impact_2017", input$gene)
        
        mutation.dat <- readMAF("mIBD_data.csv",
                                gene.symbol.col = "gene_name",
                                variant.class.col = "variant_type",
                                protein.change.col = "aa_change",
                                sep = ",")  # column-separator of csv file
        
        options <- g3Lollipop.theme(theme.name = "cbioportal",
                                          title.text = input$gene,
                                          y.axis.label = "# of Variants")
        
        options[['chartWidth']] <- 600
        options[['lollipopTrackHeight']] <- 120
        options[['yAxisLabelPadding']] <- 30
        options[['legendMargin']] = list(top = 0, right = 0, bottom = 5, left = 20)
        
        g3Lollipop(mutation.dat,
                   gene.symbol = input$gene,
                   gene.symbol.col = "gene_name",
                   protein.change.col = "aa_change",
                   btn.style = "#408697",
                   save.png.btn = FALSE,  # Hides the PNG download button
                   save.svg.btn = FALSE,   # Hides the SVG download button
                   plot.options = options)
      })
      
      
      # B) PHENOTYPE, build  pie chart
      output$pie <- renderPlotly({
        gene_data() %>% 
          group_by(IBD_subtype) %>% 
          summarize(count = length(unique(record_number))) %>% 
          #arrange(factor(IBD_subtype, levels = c())) %>% 
          plot_ly(
            labels = ~IBD_subtype,
            values = ~count,
            marker = list(colors = c("#7570b3", "#33305a", "#d5d4e8", "#555555")),
            texttemplate = "%{label}",
            textposition = "outside",
            customdata = ~count, 
            hovertemplate = paste("<b>%{label}</b>: %{percent:.1%}",
                                  "<br>N = %{customdata}",
                                  "<extra></extra>")) %>% 
          add_pie(hole = 0.4) %>% 
          layout(showlegend = FALSE,
                 xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                 yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                 margin = list(l = 15, r = 15, t = 15, b = 15),
                 font = list(family = "Ubuntu")) %>% 
          config(displayModeBar = FALSE)
        
      })
    
       
      # C) GEOGRAPHIC MAP, build map
      geo_data <- reactive({geo %>% 
          filter(gene_name == input$gene) %>% 
          group_by(name) %>% 
          summarize(cases = n(),
                    variants = length(unique(aa_change))) %>% 
          ungroup()
      })
      
      output$map <- renderLeaflet({
        
        labels <- paste0("<span style = 'font-size: 125%;'><b>", geo_data()$name,"</b>", 
                         "<br>Cases: ", geo_data()$cases,
                         "<br>Variants: ", geo_data()$variants) %>% lapply(htmltools::HTML)
        
        leaflet(data = geo_data(), options = leafletOptions(worldCopyJump = TRUE, minZoom = 1, maxZoom = 4, zoomControl = FALSE)) %>%
          addProviderTiles("CartoDB.Positron") %>%
          setView(lng = 30, lat = 20, zoom = 1) %>%
          addPolygons(fillColor = ~map_pal(cases), 
                      fillOpacity = 0.8,
                      color = "#363538",
                      weight = 2,
                      stroke = TRUE,
                      label = ~labels) %>% 
          addLegend(
            position = "bottomleft",
            pal = map_pal,
            values = seq(1,7),
            title = "mIBD Cases",
            labFormat = labelFormat(digits = 0),
            bins = seq(1,10, by = 2),
            opacity = 0.65
          ) %>%
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
      
      # D) THERAPIES, build table
      output$table <- renderDataTable({
        data_tr_react() %>%
          #mutate(link = paste0("<a href='", DOI,"' target='_blank'><i class='fa fa-external-link' style='font-size:18px; color: #408697'></i></a>")) %>%
          datatable(., 
                    colnames = c('Gene', 'Therapy', 'Reports', 
                                 '<i class="fa fa-check-double" style="color:forestgreen;"></i>', 
                                 '<i class="fa fa-xmark" style="color:darkred;"></i>'),
                    rownames = FALSE,  escape = FALSE, class = "hover nowrap cell-border stripe",
                    style = "bootstrap",
                    options = list(dom = "rti",
                                   scrollX = TRUE,
                                   columnDefs = list(list(visible = FALSE, targets = c("gene_name")))
                    )
                                   
                    
          ) %>%
          formatStyle(4,
            color = "forestgreen"
          )%>%
          formatStyle(5,
                      color = "darkred"
          )
      })
      
      # E) EICs
      output$bar <- renderPlotly({
        data_eic_react() %>%
          group_by(eic_type) %>% 
          summarize(count = sum(eic_bin)) %>% 
          plot_ly() %>% 
          add_trace(x = ~count, y = ~eic_type,
                    type = "bar",
                    orientation = "h",
                    marker = list(color = "#D95F02"),
                    #text = ~eic_type,
                    #textposition = "outside",
                    hoverinfo = "skip",
                    showlegend = FALSE) %>% 
          layout(xaxis = list(title = "Case Count"),
                 yaxis = list(title = "",
                              #showticklabels = FALSE,
                              autorange = "reversed"
                              ),
                 font = list(family = "Ubuntu"),
                 margin = list(l = 0, r = 5, t = 10, b = 10)) %>% 
          config(displayModeBar = FALSE)
      })

    }) #moduleServer    
}





