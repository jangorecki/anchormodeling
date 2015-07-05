library(shinydashboard)

AM <- function() getOption("am.share")
if(!anchormodeling::is.AM(AM())) stop("You must use `dashboard` method against your anchor model instance to start dashboard shiny application.")

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
                    fluidRow(downloadLink("export_csv", "Model data"))
            ),
            tabItem(tabName = "data",
                    fluidRow(
                        selectInput("view",
                                    label = "3NF views",
                                    choices = list(Anchors = am$read(class="anchor")[, setNames(code, name)],
                                                   Ties = am$read(class="tie")[, setNames(code, name)])),
                        DT::dataTableOutput("data")
                    )
            ),
            tabItem(tabName = "cube",
                    fluidRow(
                        selectInput("cube_tbls",
                                    label = "Cube views",
                                    choices = list(Anchors = am$read(class="anchor")[, setNames(code, name)],
                                                   Ties = am$read(class="tie")[, setNames(code, name)]),
                                    multiple = TRUE),
                        DT::dataTableOutput("cube")
                    )),
            tabItem(tabName = "etl",
                    fluidRow(
                        DT::dataTableOutput("etl")
                    ))
        )
    )
)

server <- function(input, output) {

    output$am <- DT::renderDataTable(DT::datatable(AM()$process(pretty=TRUE), rownames=FALSE))

    output$export_xml <- downloadHandler(
        filename = function(){
            format(Sys.time(),"AM_%Y%m%d_%H%M%S.xml")
        },
        content = function(file){
            am$xml(file)
        },
        contentType = "text/xml"
    )
    output$export_csv <- downloadHandler(
        filename = function(){
            format(Sys.time(),"AM_%Y%m%d_%H%M%S.tar")
        },
        content = function(file){
            #csv.files <- AM()$csv() # TO DO
            csv.files <- character()
            tar(file, files = csv.files, compression='gzip')
        },
        contentType = "application/x-tar"
    )

    view <- reactive({
        validate(need(is.character(input$view), message = "Invalid view name"))
        validate(need(input$view %in% am$read(class=c("anchor","tie"))$code, message = "Provided view name does not exists in the model"))
        validate(need(nrow(AM()$read(input$view)$obj[[1L]]$data) > 0L, message = paste("No data loaded for", input$view)))
        AM()$view(input$view)
    })

    output$data <- DT::renderDataTable(DT::datatable(view(), rownames=FALSE))

    output$cube <- DT::renderDataTable(DT::datatable(data.table(to_do = "to do"), rownames=FALSE))

    output$etl <- DT::renderDataTable(DT::datatable(AM()$etl[nchar(src) > 20L, src := paste0(substr(src,1,20),"...")], rownames=FALSE))

}

shinyApp(ui, server)
