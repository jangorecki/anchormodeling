library(shiny)
library(shinydashboard)
library(DT)
library(rpivotTable)
library(data.table)
library(anchormodeling)

AM <- function() getOption("am.share")
if(!anchormodeling:::is.AM(AM())){
    warning("You should use `dashboard` method against your anchor model instance. Loading application with dummy data.",  immediate. = TRUE, call. = FALSE)
    am <- anchormodeling:::dashboard_dummy_data()
    options("am.share" = am)
}

# ui ----------------------------------------------------------------------

ui <- dashboardPage(
    dashboardHeader(title = "Anchor Modeling"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Anchor Model", tabName = "am", icon = icon("dashboard")),
            menuItem("3NF views", tabName = "data", icon = icon("database")),
            menuItem("Cube", tabName = "cube", icon = icon("cube")),
            menuItem("ETL", tabName = "etl", icon = icon("download"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "am",
                    fluidRow(DT::dataTableOutput("am")),
                    fluidRow(downloadLink("export_xml", "Model XML")),
                    fluidRow(downloadLink("export_csv_3nf", "Model 3NF data csv")),
                    fluidRow(downloadLink("export_csv_6nf", "Model 6NF data csv")),
                    fluidRow(downloadLink("export_am", "AM instance binary"))
            ),
            tabItem(tabName = "data",
                    fluidRow(
                        column(width = 6L,
                               selectInput("view",
                                           label = "3NF views",
                                           choices = list(Anchors = AM()$read(class="anchor")[, setNames(code, name)],
                                                          Ties = AM()$read(class="tie")[, setNames(code, name)]))),
                        column(width = 6L,
                               radioButtons("view_type", "type", choices = c("current","latest","timepoint","difference"), selected = "current", inline=TRUE),
                               conditionalPanel("input.view_type == 'timepoint'",
                                                dateInput("view_timepoint", label = "Point in time")),
                               conditionalPanel("input.view_type == 'difference'",
                                                dateRangeInput("view_timepoints", label = "Range timepoints")),
                               checkboxInput("view_data_only", label = "Hide ID and metadata", value = FALSE))
                    ),
                    fluidRow(DT::dataTableOutput("view"))
            ),
            tabItem(tabName = "cube",
                    fluidRow(
                        column(width = 6L,
                               selectInput("cube_tbls",
                                           label = "Cube views",
                                           choices = list(Anchors = AM()$read(class="anchor")[, setNames(code, name)],
                                                          Ties = AM()$read(class="tie")[, setNames(code, name)]),
                                           multiple = FALSE)),
                        column(width = 6L,
                               radioButtons("cube_type", "type", choices = c("current","latest","timepoint","difference"), selected = "current", inline=TRUE),
                               conditionalPanel("input.cube_type == 'timepoint'",
                                                dateInput("cube_timepoint", label = "Point in time")),
                               conditionalPanel("input.cube_type == 'difference'",
                                                dateRangeInput("cube_timepoints", label = "Range timepoints")),
                               checkboxInput("cube_data_only", label = "Hide ID and metadata", value = TRUE))
                    ),
                    fluidRow(
                        rpivotTable::rpivotTableOutput("cube")
                    )),
            tabItem(tabName = "etl",
                    fluidRow(
                        DT::dataTableOutput("etl")
                    ))
        )
    )
)

# server ------------------------------------------------------------------

server <- function(input, output) {

    # sidebar elements

    output$am <- DT::renderDataTable(DT::datatable(AM()$process(pretty=TRUE), rownames=FALSE))

    view <- reactive({
        validate(need(is.character(input$view), message = "Invalid view name"))
        validate(need(input$view %in% AM()$read(class=c("anchor","tie"))$code, message = "Provided view name does not exists in the model"))
        validate(need(nrow(AM()$read(input$view)$obj[[1L]]$data) > 0L, message = paste("No data loaded for", input$view)))
        validate(need(input$view_type %chin% c("latest","timepoint","current","difference"), message = "Select view type"))
        time <- NULL
        if(input$view_type=="timepoint"){
            validate(need(input$view_timepoint, message = "Timepoint view requires to provide point in time."))
            time <- input$view_timepoint
        } else if(input$view_type=="difference"){
            validate(need(input$view_timepoints, message = "Difference view requires to provide date range."))
            time <- input$view_timepoints
        }
        AM()$view(input$view, type = input$view_type, time = time)
    })
    output$view <- DT::renderDataTable(DT::datatable({
        if(input$view_data_only) technical_filter(view()) else view()
    }, rownames=FALSE, options = list(scrollX = TRUE)))

    cube <- reactive({
        validate(need(is.character(input$cube_tbls), message = "Select tables 3NF tables to join into cube"))
        validate(need(all(input$cube_tbls %in% AM()$read(class=c("anchor","tie"))$code), message = "Provided cube tables name does not exists in the model"))
        validate(need(requireNamespace("rpivotTable", quietly = TRUE), message = "Install nice pivot js library as rpivotTable package"))
        validate(need(input$cube_type %chin% c("latest","timepoint","current","difference"), message = "Select cube view type"))
        time <- NULL
        if(input$cube_type=="timepoint"){
            validate(need(input$cube_timepoint, message = "Timepoint view requires to provide point in time."))
            time <- input$cube_timepoint
        } else if(input$cube_type=="difference"){
            validate(need(input$cube_timepoints, message = "Difference view requires to provide date range."))
            time <- input$cube_timepoints
        }
        AM()$view(input$cube_tbls[[1L]], type = input$cube_type, time = time)
        # TO DO batch join 3NF tables, currently only first table
    })
    output$cube <- rpivotTable::renderRpivotTable(rpivotTable({
        if(input$cube_data_only) technical_filter(cube()) else cube()
    }))

    output$etl <- DT::renderDataTable(DT::datatable({
        AM()$etl[nchar(src) > 20L, src := paste0(substr(src,1,20),"...")]
    }, rownames=FALSE, options = list(scrollX = TRUE)))

    # download helpers

    output$export_xml <- downloadHandler(
        filename = function(){
            format(Sys.time(),"AM_%Y%m%d_%H%M%S.xml")
        },
        content = function(file){
            AM()$xml(file)
        },
        contentType = "text/xml"
    )
    output$export_csv_3nf <- downloadHandler(
        filename = function(){
            format(Sys.time(),"AM_csv_3NF_%Y%m%d_%H%M%S.tar")
        },
        content = function(file){
            am <- AM()
            csv.paths <- am$csv(dir = tempdir(), nf = 3L)
            tar(file, files = csv.paths, compression="gzip", tar="tar")
            file.remove(csv.paths)
        },
        contentType = "application/x-tar"
    )
    output$export_csv_6nf <- downloadHandler(
        filename = function(){
            format(Sys.time(),"AM_csv_6NF_%Y%m%d_%H%M%S.tar")
        },
        content = function(file){
            am <- AM()
            csv.paths <- am$csv(dir = tempdir(), nf = 6L)
            tar(file, files = csv.paths, compression="gzip", tar="tar")
            file.remove(csv.paths)
        },
        contentType = "application/x-tar"
    )
    output$export_am <- downloadHandler(
        filename = function(){
            format(Sys.time(),"AM_%Y%m%d_%H%M%S.RData")
        },
        content = function(file){
            am <- AM()
            am$stop()
            save(am, file = file)
            am$run()
        },
        contentType = "application/octet-stream"
    )

}

# shinyApp ----------------------------------------------------------------

shinyApp(ui, server)
