# Data entry application

library(shiny)
library(dplyr)
library(purrr)
library(tidyr)
library(tidyselect)
library(stringr)
library(magrittr)
library(DT)
library(rio)
library(here)

i_am("app.R")

# functions
isValid = function(x){
  if(try(isTRUE(is.null(x)),silent=T)) r = F else
    if(try(isTRUE(is.na(x)),silent=T)) r = F else
      if(try(isTRUE(length(x) < 1),silent=T)) r = F else
        if(try(isTRUE(x == ""),silent=T)) r = F else r = T
        return(r)
}

splitVals = function(x,sep){
  if(is.na(x)){
    r = list("")
  } else {
    if(!is.na(sep))r = str_split(x,pattern = sep) else r = list(x)
  }
  return(r)
}

options = import(here("InputOptions.xlsx"),
                 setclass = 'tibble',range = "A1:A1000")

indx = which(options$Settings == "Input")

settings = import(here("InputOptions.xlsx"),
                  setclass = 'tibble',range = glue::glue("A2:B{indx - 1}")) %>%
  filter(!is.na(field))

selectMultiple = settings %>%
  filter(field == 'multiple') %>%
  pull(parameter) %>%
  as.logical()

inputOptions = import(here("InputOptions.xlsx"),
                      setclass = 'tibble',skip = indx + 1)

inputOptions %<>%
  rowwise() %>%
  mutate(values = splitVals(values,sep)) %>%
  ungroup() %>%
  select(-sep)

lookups = rio::import(here("lookups.xlsx"))

jscode = '$(document).keyup(function(e) {
    if (e.key == "Enter") {
    $("#add").click();
}});
Shiny.addCustomMessageHandler("refocus",
                                  function(NULL) {
                                    document.getElementById("variable").focus();});'


# load from tmp
fntmp = here("tmp/current.Rds")
if(file.exists(fntmp)){
  prior = readRDS(fntmp)
} else {
  prior = tibble()
}

# ui ----
ui <- fluidPage(

  # Application title
  titlePanel(tryCatch(settings %>% filter(field == 'name') %>% pull(parameter),error = function(e)return("Data Entry"))),

  # javascript
  tags$head(tags$script(HTML(jscode))),
  shinyjs::useShinyjs(),


  sidebarLayout(
    sidebarPanel(
      h2("Add data"),
      textInput('ID','ID',
                placeholder = "catalog-number"),
      selectizeInput("variable","variable",
                     inputOptions$key %>%
                       setNames(inputOptions$inputName)),
      uiOutput('valueUI'),
      checkboxInput('check','Check if observation is uncertain'),
      fixedRow(column(2,tagAppendAttributes(
        actionButton('add','Add'),
        `data-proxy-click` = "add"
      )),column(1),column(2,actionButton('skip','skip'))
      )
    ),
    mainPanel(
      actionButton('deleteAll','delete all'),
      actionButton('deleteRow','delete row'),
      actionButton('save','save'),
      DT::DTOutput('table1')
    )
  )

)

# Server ----
server <- function(input, output, session) {
  input <<- input
  rvals <<- reactiveValues(analysis = prior)


  # table output ----
  prntTbl = function(){
    output$table1 = DT::renderDT({
      DT::datatable(rvals$analysis,rownames = F, editable = "cell")
    })
  }
  prntTbl()

  proxyDT = dataTableProxy('table1')

  observeEvent(input$table1_cell_edit, {
    info = input$table1_cell_edit
    rvals$analysis <<- editData(rvals$analysis, info,rownames = F)
    replaceData(proxyDT, rvals$analysis, resetPaging = FALSE)
  })

  # Define ui inputs ----
  output$valueUI = renderUI({
    req(input$variable)
    type = inputOptions %>%
      dplyr::filter(key == input$variable) %>%
      dplyr::pull(type)
    rvals$type = type
    if(type == 'list'){
      qs =
        inputOptions %>%
        filter(key == input$variable) %>%
        pull(values) %>%
        unlist()
    } else if(type %in% c("lookup","autolookup")){
      qs = tryCatch({qstmp =
        lookups %>%
        filter(outputField == input$variable)
      lookupVal = rvals$analysis %>%
        filter(ID == input$ID) %>%
        filter(variable == qstmp$lookupField[1]) %>%
        pull(value)
      qstmp = qstmp %>%
        filter(lookup == lookupVal) %>%
        filter(!is.na(value)) %>%
        pull(value)
      },error = function(e)return(""))
    }
    if(type == "text"){
      r = shiny::textInput('value','value')
    } else if(type == "numeric"){
      r = shiny::numericInput('value','value',0)
    } else if(type == "list"){
      r = shiny::selectizeInput('value','value',qs, multiple = selectMultiple)
    } else if(type == "autoID"){
      r = shinyjs::disabled(shiny::textInput('value','value',placeholder = input$ID))
      updateTextInput(session = session,inputId = 'value', value = input$ID)
    } else if(type == "autolookup"){
      rvals$lookup =
        r = shinyjs::disabled(shiny::selectInput('value','value',qs,multiple = F, selected = qs))
    } else if(type == "lookup"){
      r = shiny::selectInput('value','value',qs,multiple = selectMultiple)
    } else {
      r = shiny::textInput('value','value',placeholder = "error determining type")
    }
    r
  })

  # disable ----
  observeEvent({
    input$ID
    input$value
  },{
    if(isValid(input$ID) && isValid(input$value)){
      shinyjs::enable("add")
    } else {
      shinyjs::disable("add")
    }
  })

  # buttons ----
  observeEvent(input$add,{
    req(input$variable)
    if(input$check == T){
      value = paste0(input$value,'?')
      # reset checkbox
      updateCheckboxInput(session = session,
                          inputId = 'check',
                          label =  'Check if observation is uncertain',
                          value = F)
    } else value = input$value
    value = paste(value, collapse = "; ")
    if(rvals$type == "autoID") value = input$ID

    new = tibble(
      ID = input$ID,
      variable = input$variable,
      value = value
    )
    # print(new)
    new = bind_rows(new,rvals$analysis) %>% distinct_all() %>%
      distinct(ID,variable,.keep_all = T)
    rvals$analysis = new
    prntTbl()

    # save results
    rvals$analysis %>% saveRDS(here("tmp/current.Rds"))

    # reset select input
    nextKey = tryCatch({
      indx = which(inputOptions$key == input$variable)
      inputOptions$key[indx + 1]
    },error = function(e)return(""))
    updateSelectizeInput(session = session,
                         inputId =  "variable",
                         choices = inputOptions$key %>%
                           setNames(inputOptions$inputName),
                         selected = nextKey)

    # refocus cursor
    session$sendCustomMessage(type="refocus",message=list(NULL))
  })

  observeEvent(input$skip,{
    nextKey = tryCatch({
      indx = which(inputOptions$key == input$variable)
      inputOptions$key[indx + 1]
    },error = function(e)return(""))
    updateSelectizeInput(session = session,
                         inputId =  "variable",
                         choices = inputOptions$key %>%
                           setNames(inputOptions$inputName),
                         selected = nextKey)
  })

  # delete everything -- including prior saved data
  observeEvent(input$deleteAll,{
    showModal(modalDialog(
      title = h1("Warning!", style = "color: red;"),
      "This will delete all data including everything saved in prior sessions!",
      footer = tagList(
        actionButton('deleteAllMod','delete', style = "color: red;"),
        actionButton('cancel','cancel')
      )
    ))
  })

  observeEvent(input$cancel,removeModal())
  observeEvent(input$deleteAllMod,{
    print('working?')
    analysis <<- reactiveVal(tibble())
    prntTbl()
    # save results
    rvals$analysis %>% saveRDS(here("tmp/current.Rds"))
    removeModal()
  })

  observeEvent(input$deleteRow,{

    indx = input$table1_rows_selected

    # save deleted just in case
    removed = rvals$analysis %>%
      slice(indx)
    fntmp = here("tmp/deleted.Rds")
    if(file.exists(fntmp)){
      old = readRDS(fntmp)
    } else {
      old = tibble()
    }
    removed = bind_rows(old, removed) %>%
      distinct_all()
    saveRDS(removed,fntmp)

    new = rvals$analysis %>%
      slice(-indx)

    analysis <<- reactiveVal(new)
    prntTbl()

    # save results
    rvals$analysis %>% saveRDS(here("tmp/current.Rds"))
  })

  # save ----
  observeEvent(input$save,{
    print("saving")
    fn = here(glue::glue(
      "output-{as.Date(Sys.time())}.xlsx"
    ))
    i = 0
    while(isTRUE(file.exists(fn))){
      i = i + 1
      fn %<>%
        gsub("v\\d+.xlsx|.xlsx",paste0("v",i,".xlsx"),.)
      if(i > 99) stop("limited to 99 files in one day. Why so many files?")
    }
    rio::export(rvals$analysis %>%
                  mutate(variable = factor(variable, levels = inputOptions$key)) %>%
                  arrange(variable) %>%
                  pivot_wider(names_from = variable,values_from = value),fn)
    browseURL(fn)
  })
}


# Run the application
shinyApp(ui = ui, server = server)
