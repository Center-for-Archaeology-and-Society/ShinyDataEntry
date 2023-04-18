# Data entry application

library(shiny)
library(shinyjs)
library(dplyr)
library(purrr)
library(tidyr)
library(tidyselect)
library(stringr)
library(magrittr)
library(DT)
library(rio)
library(here)
library(janitor)
library(shinyauthr)
library(shinythemes)

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

exportAnalysis = function(analysis,inputOptions){
  exclude = inputOptions %>%
    dplyr::filter(include == F) %>%
    dplyr::pull(key)
  vars = inputOptions %>%
    dplyr::filter(!key %in% exclude) %>%
    dplyr::select(inputName,variable = key)
  analysis %<>%
    dplyr::right_join(vars, by = "variable") %>%
    select(-variable) %>%
    tidyr::pivot_wider(names_from = inputName,values_from = value) %>%
    select(vars$inputName) %>%
    janitor::remove_empty("rows")
  return(analysis)
}

options = import(here("InputOptions.xlsx"),
                 setclass = 'tibble',range = "A1:A1000")

indx = which(options$Settings == "Input")

settings = import(here("InputOptions.xlsx"),
                  setclass = 'tibble',range = glue::glue("A2:B{indx - 1}")) %>%
  filter(!is.na(field))

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

# change folder permissions on server
if (Sys.info()["sysname"] == "Linux") {
  system("sudo chmod a+rw tmp")
}

# ui ----
ui <- navbarPage(
    id = "main",

  # Application title
  title =  tryCatch(settings %>% filter(field == 'name') %>% pull(parameter),error = function(e)return("Data Entry")),


  theme = shinytheme("flatly"),

  # javascript
  tags$head(tags$script(HTML(jscode))),
  shinyjs::useShinyjs(),
  tabPanel(
    "home",
    # add logout button UI
    div(class = "pull-right", shinyauthr::logoutUI(id = "logout")),
    div(HTML("<h3>Forgot password? Email <a href=\"mailto:rbischoff@asu.edu\">rbischoff@asu.edu</a></h3>
"),id = "forgotPassword"),
    actionButton("create_user", "Create user"),
    # add login panel UI function
    shinyauthr::loginUI(id = "login"),
    uiOutput("welcomeUI")
  ),
  tabPanel(
    title = "Data entry",
    value = "dataEntry",
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
        downloadButton('download','download'),
        DT::DTOutput('table1')
      )
    )
  )
)

# Server ----
server <- function(input, output, session) {

  database = reactiveVal(readRDS("database.Rds"))

  observeEvent(credentials()$user_auth,{
    shinyjs::toggle("create_user",condition = isFALSE(credentials()$user_auth))
    shinyjs::toggle("forgotPassword",condition = isFALSE(credentials()$user_auth))
  })

  observeEvent(input$create_user, {
    showModal(modalDialog(
      title = "New user information",
      textInput("new_username", "Username:"),
      textInput("new_name", "Name:"),
      passwordInput("new_password1", "Password:"),
      passwordInput("new_password2", "Confirm password:"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("save_user", "Save",
                     class = "btn-primary")
      ),
      easyClose = TRUE,
      size = "m",
      closeOnEscape = TRUE,
      closeOnClickOutside = TRUE
    ))
  })

  observeEvent(input$save_user, {
    # Get the values of the input fields
    username <- input$new_username
    password1 <- input$new_password1
    password2 <- input$new_password2
    name <- input$new_name

    # Validate the passwords match
    if(username %in% database()$user){
      showNotification("Username already exists. Please try again.",
                       type = "error")
      return()
    }

    if (password1 != password2) {
      showNotification("Passwords don't match. Please try again.",
                       type = "error")
      return()
    }

    new = tibble(user = username, password = sodium::password_store(password1), permissions = "standard",name = name)

    database = reactiveVal(bind_rows(database(),new))
    saveRDS(database(),"database.Rds")

    showNotification(sprintf("User '%s' was created successfully!\n", new$user))
    removeModal()

  })

  # login ----

  # call login module supplying data frame,
  # user and password cols and reactive trigger
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = database(),
    user_col = user,
    pwd_col = password,
    sodium_hashed = T,
    log_out = reactive(logout_init())
  )

  # call the logout module with reactive trigger to hide/show
  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(credentials()$user_auth)
  )

  output$welcomeUI = renderUI({
    req(credentials()$user_auth)
    renderText(paste("Welcome",credentials()$info$name))
  })

  observeEvent(credentials()$user_auth,{
    if(credentials()$user_auth) {
      showTab("main","dataEntry")
    } else {
      hideTab("main","dataEntry")
    }
  })

  # reactive values ----
  rvals <- reactiveValues(ignore = c())

  # load from tmp
  observeEvent(credentials()$user_auth,{
    req(credentials()$user_auth)
    print("loading prior values")
    if(!dir.exists(here("tmp"))) dir.create(here("tmp"))
    rvals$dirpath = here(paste0("tmp/",credentials()$info$user))
    if(!dir.exists(rvals$dirpath)) dir.create(rvals$dirpath)
    rvals$filepath = file.path(rvals$dirpath,"current.Rds")

    fntmp = rvals$filepath
    if(file.exists(fntmp)){
      print("prior values exist")
      rvals$analysis = readRDS(fntmp)
    } else {
      print("prior values do not exist")
      rvals$analysis = tibble()
    }
  })

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
    rvals$analysis <- editData(rvals$analysis, info,rownames = F)
    replaceData(proxyDT, rvals$analysis, resetPaging = FALSE)
  })

  # Define ui inputs ----
  output$valueUI = renderUI({
    req(credentials()$user_auth)
    req(input$variable)
    print("defining UI")
    row = inputOptions %>%
      dplyr::filter(key == input$variable)
    type = row %>%
      dplyr::pull(type)
    selectMultiple = row %>%
      dplyr::pull(multiple)
    if(!isTRUE(selectMultiple)) selectMultiple = F
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
    } else if(type == "group"){
      qs = c("Yes","No")
    }
    if(type == "text"){
      r = shiny::textInput('value','value')
    } else if(type == "numeric"){
      r = shiny::numericInput('value','value',0)
    } else if(type == "list"){
      r = shiny::selectizeInput('value','value',qs, multiple = selectMultiple)
    } else if(type == "autoID"){
      r = shiny::textInput('value','value',placeholder = input$ID)
      updateTextInput(session = session,inputId = 'value', value = input$ID)
    } else if(type == "autolookup"){
      rvals$lookup =
        r = shinyjs::disabled(shiny::selectInput('value','value',qs,multiple = F, selected = qs))
    } else if(type == "lookup"){
      r = shiny::selectInput('value','value',qs,multiple = selectMultiple)
    } else if(type == "function"){
      rvals$eval = T
      r = shiny::textInput('value','value')
    } else if(type == "group"){
      r = shiny::selectInput('value',paste("include",input$variable,"variables?"), choices = qs, selected = "yes")
    } else {
      r = shiny::textInput('value','value',placeholder = "error determining type")
    }
    print("UI definition completed")
    r
  })

  # function update ----
  observeEvent(rvals$eval,{
    req(rvals$eval)
    if(rvals$eval){
      f = inputOptions %>%
        filter(key == input$variable) %>%
        pull(`function`)
      val = tryCatch(eval(parse(text = f)),error = function(e)return(""))
      updateTextInput(session = session, inputId = "value",value = val)
      rvals$eval = F
    }
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
    if(rvals$type != "group"){
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
      rvals$analysis %>% saveRDS(rvals$filepath)
    }
    # reset select input
    nextKey = tryCatch({
      indx = which(inputOptions$key == input$variable)
      if(inputOptions$type[indx] == "group" && input$value == "No"){
        rvals$ignore = c(rvals$ignore,which(inputOptions$parent == inputOptions$key[indx]))
      }
      indx = indx + 1
      while(indx %in% rvals$ignore){
        indx = indx + 1
      }
      inputOptions$key[indx]
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
    rvals$analysis = tibble()
    prntTbl()
    # save results
    rvals$analysis %>% saveRDS(rvals$filepath)
    removeModal()
  })

  observeEvent(input$deleteRow,{

    indx = input$table1_rows_selected

    # save deleted just in case
    removed = rvals$analysis %>%
      slice(indx)
    fntmp = here(rvals$dirpath,"deleted.Rds")
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

    rvals$analysis <- new
    prntTbl()

    # save results
    rvals$analysis %>% saveRDS(rvals$filepath)
  })

  # download ----


  output$download = downloadHandler(
    filename = function() {
      glue::glue(
        "output-{as.Date(Sys.time())}.xlsx"
      )
    },
    content = function(file) {
      rio::export({
        exportAnalysis(rvals$analysis,inputOptions)
      },file)
    }
  )
}


# Run the application
shinyApp(ui = ui, server = server)
