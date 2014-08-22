#' Interactive UI For Exploring Benchmarks
#' 
#' Creates an interactive Shiny environment for users to browse through the 
#' \code{`Rprof`} results in tree structure.
#' 
#' @note ideally this would be implemented as an S3 method to the \code{`shine`}
#'   generic, but Shiny does not currenlty define such a method so we're stuck
#'   with this.
#' 
#' @import shiny 
#' @param x a treeprof object
#' @param id.start integer(1L) what node to start displaying from
#' @param depth integer(1L) how deep down the tree to start off showing
#' @export
#' @examples
#' \dontrun{
#' x <- treeprof(data.frame(a=1:10, b=letters[1:10]))
#' shinify(x)
#' } 

shinyfy <- function(x, id.start=1L, depth=10L) {

  # = STATIC ===================================================================  

  x.norm <- normalize(x, "auto")
  tips <- c(
    Function="Name of Function",
    Total="Total time within function, including sub-calls",
    Self="Time within function proper",
    Inst="How many times `fun.name` was called",
    `Inst Nest`="How many of the instances were nested calls"
  )
  # = SERVER ===================================================================

  server <- function(input, output, session) {
    curr.id <- id.start
    update_table <- function(y, depth, disp.thresh) {
      message("Updating", y, " to depth ", depth)
      if(missing(depth) || is.null(depth) || any(is.na(depth))) depth <- 0
      if(missing(disp.thresh) || is.null(disp.thresh) || any(is.na(disp.thresh))) 
        disp.thresh <- 0        
      session$sendCustomMessage(
        type = "myCallbackHandler", 
        paste0(
          as.character(
            x, id.start=y, depth=depth, mode="HTML", disp.thresh=disp.thresh
          ),
          collapse="\n"
      ) )
      curr.id <<- y
    }
    navigate <- function(ref) {
      update_table(ref, input$depth, input$hideunder * 10)
    }
    observe({
      if (is.null(input$navigate)) return()
      isolate(update_table(input$navigate, input$depth, input$hideunder * 10))
    })
    by.fun <- by_fun(x.norm)
    setnames(by.fun, names(tips))
    output$table <- renderDataTable(
      by.fun,
      options=list(
        iDisplayLength=25,
        aoColumns=list(
          list(sClass="char", asSorting=list("asc", "desc")),
          list(sClass="num", asSorting=list("desc", "asc")),
          list(sClass="num", asSorting=list("desc", "asc")),
          list(sClass="num", asSorting=list("desc", "asc")),
          list(sClass="num", asSorting=list("desc", "asc"))
    ) ) )
    observe(update_table(curr.id, input$depth, input$hideunder * 10))
  }
  # = UI =======================================================================

  ui <- fluidPage(
    tags$head(tags$style("
      div#tree_input_bar label { display: inline; vertical-align: middle; margin: 5px;}
      div#tree_input_bar input { margin: 5px; width: 30px;}
      input#hideunder { width: 40px;}
      div#treeprof_table div#table table { line-height: auto;}
      div#treeprof_table div#table table td { padding: 3px;}
      div#treeprof_table div#table table td.num {text-align: right;}
      div#treeprof_table div#table table th {text-align: center;}
      div#treeprof_table {width: 650px;}
      div#treeprof_table_tu {margin: 10px 0px;}
      div#treeprof_col_desc {}
      "
    ) ),
    tags$h1("Tree Prof"),
    tabsetPanel(type = "tabs", 
      
      # - Tree ---------------------------------------------------------------

      tabPanel(
        "Tree", 
        tags$div(id="tree_input_bar",
          numericInput("depth", "Depth: ", value=10, min=1, max=Inf, step=1),
          tags$span(
            numericInput("hideunder", "Hide: ", value=1, min=0, max=100, step=1),
            title="Don't expand branches that account for less than this percent of total time"
          ),
          span("%")
        ),
        HTML(paste0(as.character(x, id.start=1L, mode="HTML"), collapse="\n")),
        HTML(
          paste0(
            "<br />", 
            as.character(summarize(x.norm), mode="HTML"), 
            collapse="\n"
      ) ) ), 
      # - Table --------------------------------------------------------------

      tabPanel(
        "Table",
        div(id="treeprof_table", dataTableOutput("table")),
        div(id="treeprof_table_tu", paste0("Time Units: ", attr(x.norm, "time.unit"))), 
        div(id="treeprof_col_desc", 
          "Column Descriptions:", 
          tags$ul(lapply(paste0(names(tips), ": ", tips), tags$li))
    ) ) ),
    tags$script('
      Shiny.addCustomMessageHandler(
        "myCallbackHandler",
        function(x) {
          document.getElementById("treeproftree").innerHTML = x;
        } );'
  ) )
  # - RUN --------------------------------------------------------------------

  runApp(
    list(ui = ui, server = server), 
    launch.browser = getOption("viewer", utils::browseURL)
  )
}