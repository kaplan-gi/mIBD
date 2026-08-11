# Title: mIBD Global Systematic Review Shiny Application
# Contributor: Lindsay Hracs, Julia Gorospe
# Created: 2026-01-27
# Updated: 2026-08-11
# R version 4.5.0 (2025-04-11)
# Platform: aarch64-apple-darwin20 (64-bit)
# Running under: macOS Sequoia 15.6.1


# link:

# version notes:


source("global.R", local = TRUE)$value
source("map.R", local = TRUE)$value
source("genes.R", local = TRUE)$value
source("data.R", local = TRUE)$value


#--- UI ----------------------------------------------------------------------------#
ui <- page_fillable(
  
  title = "mIBD",
  
  useShinyjs(),
  
  theme = bs_theme(bootswatch = "united",
                   primary = "#408697",
                   base_font = "Ubuntu"),
  
  # format link style
  tags$head(tags$style(HTML("a {color: #408697}"))),
  # format action button
  tags$head(tags$style(HTML(".btn {color:rgb(255,255,255); border-color: #408697; background-color: #408697;}"))),
  # format logo placement
  tags$head(tags$style(HTML(".logo {margin-left: 10px; margin-right: 10px; margin-bottom: 20px; vertical-align: middle;}"))),
  # format Leaflet popup font
  tags$head(tags$style(HTML(".leaflet-pane {font-family: Ubuntu;}
                               .leaflet .info {font-family: Ubuntu;}"))),
  # remove Leaflet control which interfers with sidebar
  # tags$head(tags$style(HTML(".leaflet-left .leaflet-control{margin-left: 25px; margin-bottom: 35px;}"))), #visibility: hidden; background-color: transparent;
  # leaflet control collapsible
  tags$head(tags$style(HTML(
    ".collapsible-control {
    padding: 6px 8px;
    font: 14px/16px Arial, Helvetica, sans-serif;
    box-shadow: 0 0 15px rgba(0,0,0,.2);
    border-radius: 5px;
  }
  
  .collapsible-control .control-content {
    display: none;
    margin-top: 5px;
  }
  
  .collapsible-control:hover .control-content {
    display: block;
  }"
  ))),
  
  # See for issues with z-index for leaflet-containers: https://github.com/rstudio/bslib/issues/955
  tags$head(tags$style(HTML('.leaflet-container {z-index: 0;}'))),
  # Sidebar transparency for mobile
  tags$head(tags$style(HTML('.bslib-gap-spacing .sidebar {background: rgba(212, 218, 220, 0.7)}'))),
  # change sidebar collapse arrow to make more visible
  assignInNamespace(
    "collapse_icon", 
    function() {
      bsicons::bs_icon(
        "chevron-double-left", class = "collapse-icon", size = NULL
      ) 
    },
    ns = "bslib"
  ),
  tags$head(tags$style(HTML('.bslib-sidebar-layout .collapse-toggle .collapse-icon {fill: #000000 !important;}'))),
  tags$head(
    tags$style(HTML("
      .resizable-panel {
        position: relative;
        resize: both;
        overflow: auto;
        min-width: 300px;
        min-height: 300px;
        max-width: 600px;
        max-height: 600px;
      }
    "))
  ),
  
  # selectizeInput styling to match pickerInput/shinyWidgets
  tags$style(HTML(".primary-selectize .selectize-dropdown .active {
       background-color: #02D1F6;
       color: var(--bs-white);
    }
    .primary-selectize .selectize-dropdown .option.selected,
     .primary-selectize .selectize-dropdown .option.active.selected {
      background-color: var(--bs-primary);
       color: var(--bs-white);
     }           
    .primary-selectize .selectize-input {
       background-color: var(--bs-primary);
       border-color: var(--bs-primary);
       color: #F6F6F6;
     }")),
  
  # DT highlighting
  tags$head(
    tags$style(HTML("
    table.table-striped tbody tr:hover td {
      background-color: #02D1F6 !important;
      color: #ffffff !important;
    }
    table.table tbody tr.active td {
      background-color: #408697 !important;
      color: #F6F6F6 !important;
    }
  "))
  ),

  div(
    class = "d-flex flex-column h-100", # header is always fixed
  
    # Header
    div(
      class = "flex-shrink-0",
      style = "color: #F6F6F6; background-color: #363538; margin: -20px -20px 0px -20px; padding: 25px 20px 5px 20px;",
        layout_columns(
          col_widths = c(8, 4),
          tags$h2("##mIBD DEMO SITE. NO EXTERAL USE."),
          #tags$h2("Monogenic IBD: A global map"),
  
            div(style = "display: flex; justify-content: flex-end; gap: 10px;",
   
                tags$a(
                  actionButton(
                    width = "125px",
                    inputId = "paper_share",
                    label = "Paper",
                    icon = icon("link")),
                  href = "https://doi.org/10.1016/j.cgh.2021.03.021", #ADD ON PUBLICATION
                  target = "_blank"
                ), 
  
                tags$a(
                  actionButton(
                    width = "125px",
                    inputId = "contact us",
                    label = "Contact",
                    icon = icon("envelope")),
                  href = "mailto:aleixo.muise@sickkids.ca"
                )
            )
        )
    ),
    
      
    # Tabs
    div(
      class = "flex-grow-1 overflow-hidden pt-2", # tab content can scroll
      
      navset_tab(
        
        nav_panel(tags$header(style = "text-align:center; font-weight: bold; font-size:125% ;", "Map"),
                  mapUI("map")   
        ),
        
        nav_panel(tags$header(style = "text-align:center; font-weight: bold; font-size:125% ;", "Genes"),
                  genesUI("genes")
        ),
        
        nav_panel(tags$header(style = "text-align:center; font-weight: bold; font-size:125% ;", "Data"),
                  dataUI("data")
        ),
        
        nav_spacer(),
        
        nav_panel(tags$header(style = "text-align:center; padding-right:10px; padding-left:10px; font-size: 125%; font-weight:bold;",
                              shiny::icon("circle-info")),
                  div(
                    style = "height: calc(100vh - 140px); overflow-y: auto; padding-right: 10px;",
                    
                    HTML(
                      "<p style='margin: 20px 40px; font-size: 115%;'>
                        <b>Monogenic Inflammatory Bowel Disease (mIBD)</b><br>
                        mIBD is a rare disease presenting as IBD but traceable to a genetic variant at a single locus. Cases typically onset early in life and are characterized by severe, refractory disease. Around 100 genes have been associated with mIBD to date.<br><br>
                      
                        <b>Global Systematic Review</b><br>
                        The data presented in this web application are from a systematic review of mIBD case reports published in 2022, with updates ongoing. Articles linking any of the 75 mIBD-associated genes with IBD were retrieved from PubMed. A total of 750 unique cases were found, resulting in the compilation of a rich dataset.<br><br>
                      
                        <b>Using and Citing this Work</b><br>
                        The medication/therapeutic information presented in this application is based on published reports identified through a systematic review. It is provided for informational and research purposes only and should not be interpreted as a treatment recommendation, prescribing guidance, or evidence that a medication is safe or effective for a particular patient or condition. Data and figures may be downloaded. If you would like to reuse material, please cite the following:
                    </p>
    
                    <ul style='margin: -10px 40px 20px 70px; font-size: 100%;'>
                        <li><i>Nambu R, Warner N, Mulder D, et al. A Systematic Review of Monogenic Inflammatory Bowel Disease. Clin Gastroenterol Hepatol. 2022 Apr;20(4):e653–e663. <a href='https://doi.org/10.1016/j.cgh.2021.03.021' target='_blank'>doi:10.1016/j.cgh.2021.03.021</a>.</i></li>
                    </ul>
    
                    <p style='margin: 20px 40px; font-size: 115%;'>
                          <b>Collaboration and Support</b><br>
                          Research and data was prepared by members of the Muise Lab at SickKids in Toronto, Canada with support from the Canadian Institute for Health Research and the International Organization for IBD <br></p>"
                    ),
                    
                    div(style = "display: flex; align-items: center; justify-content: center; gap: 50px; margin: 20px 40px;",
                        tags$a(
                          href = "https://www.sickkids.ca/en/care-services/centres/inflammatory-bowel-disease-centre/",
                          target = "_blank",
                          img(src="https://raw.githubusercontent.com/kaplan-gi/Images/main/SickKids_logo_2col.png",
                              style = "height: 50px; width: auto; object-fit: contain;")
                        ),
                        tags$a(
                          href = "https://ioibd.org/",
                          target = "_blank",
                          img(src="https://raw.githubusercontent.com/kaplan-gi/Images/main/IOIBD_logo.png",
                              style = "height: 80px; width: auto; object-fit: contain;")
                        ),
                        tags$a(
                          href = "https://wpsites.ucalgary.ca/gilkaplan/about-us/",
                          target = "_blank",
                          img(src="https://raw.githubusercontent.com/kaplan-gi/Images/main/Lab_Logo_PowerPoint_Transparent.png",
                              style = "height: 90px; width: auto; object-fit: contain;")
                        )
                    ),
                    
                    HTML("<p style = 'margin: 20px 40px; font-size: 115%;'><br>
                         <b>Application Development</b><br>
                         This web application was developed by the Kaplan Global Epidemiology Lab at the University of Calgary. It was constructed using the <i>Shiny</i> framework with dependancies on the R-compatible <i>leaflet</i>, <i>plotly</i>, <i>g3viz</i>, and <i>DT</i> modules.<br><br>
    
                      </p>")
                    
                  )
          )
        
        ) # navset_tab
      
    ) # div (flex-grow)
    
  ) # div (full-height)
  
) # page_fillable



#--- Server ----------------------------------------------------------------------------#
server <- function(input, output, session) {
  
  mapServer("map")
  genesServer("genes")
  dataServer("data")
  
  
  showModal(modalDialog(
    title = "Welcome! ##UNDER DEVELOPMENT. Do not copy, share, publish, or report any data from this site.",
    HTML("<p style = 'font-size: 100%;'>Thank you for visiting our mIBD data repository. Information about published cases of mIBD from around the world can be stratified and summarized visually.<br><br>
                    <b>Disclaimer:</b><br>
                    The medication/therapeutic information presented in this application is based on published reports identified through a systematic review. It is provided for informational and research purposes only and should not be interpreted as a treatment recommendation, prescribing guidance, or evidence that a medication is safe or effective for a particular patient or condition.<br><br>
                    <b>To cite:</b><br>
                    <i>Nambu R, Warner N, Mulder D, et al. A Systematic Review of Monogenic Inflammatory Bowel Disease. Clin Gastroenterol Hepatol. 2022 Apr;20(4):e653–e663. <a href='https://doi.org/10.1016/j.cgh.2021.03.021' target='_blank'>doi:10.1016/j.cgh.2021.03.021</a>.</i><br><br>
                    </p>"),
    size = "l",
    footer = modalButton("Close"),
    easyClose = TRUE)
  )
  
}


shinyApp(ui, server)
