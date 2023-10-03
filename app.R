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
library(sodium)
library(shinythemes)

i_am("app.R")

# functions

safeSaveRDS = function(object, file) {
  try(saveRDS(object, file))
}

safeImport = function(file, ...) {
  if (file.access(file, mode = 4) == 0) {
    object = tryCatch(
      rio::import(file, setclass = 'tibble', ...),
      error =  function(e)
        return(NULL)
    )
  } else {
    if (Sys.info()["sysname"] == "Linux") {
      system(glue::glue("chown shiny '{file}'"))
    }
    object = tryCatch(
      rio::import(file, setclass = 'tibble' , ...),
      error =  function(e)
        return(NULL)
    )
  }
  return(object)
}

key = sha256(charToRaw(Sys.getenv("key")))

saveDatabase = function(database){
  safeSaveRDS(database %>%
                dplyr::mutate_at(vars(1,3,4),~ purrr::map(.x,function(x){
                  data_encrypt(charToRaw(x),key)
                })
                ),'database.Rds')
}

readDatabase = function(){
  safeImport("database.Rds") %>%
    dplyr::mutate_at(vars(1,3,4),~ purrr::map_chr(.x, function(x){
      unlist(x) %>% data_decrypt(key = key) %>% rawToChar()
    })
    ) %>%
    ungroup()
}

isValid = function(x) {
  if (try(isTRUE(is.null(x)), silent = T)
  ) r = F
  else
    if (try(isTRUE(is.na(x)), silent  =  T)
    ) r = F
    else
      if (try(isTRUE(length(x) < 1), silent   =   T)
      ) r = F
      else
        if (try(isTRUE(x == ""), silent    =    T)
        ) r = F
        else
          r = T
        return(r)
}

splitVals = function(x, sep) {
  if (is.na(x)) {
    r = list("")
  } else {
    if (!is.na(sep))
      r = str_split(x, pattern = sep)
    else
      r = list(x)
  }
  return(r)
}

exportAnalysis = function(analysis, inputOptions) {
  exclude = inputOptions %>%
    dplyr::filter(include == F) %>%
    dplyr::pull(key)
  vars = inputOptions %>%
    dplyr::filter(!key %in% exclude) %>%
    dplyr::select(inputName, variable = key)
  analysis %<>%
    dplyr::right_join(vars, by = "variable") %>%
    select(-variable) %>%
    tidyr::pivot_wider(names_from = inputName, values_from = value) %>%
    select(vars$inputName) %>%
    janitor::remove_empty("rows")
  return(analysis)
}


jscode = '$(document).keyup(function(e) {
    if (e.key == "Enter") {
    $("#add").click();
}});
Shiny.addCustomMessageHandler("refocus",
                                  function(NULL) {
                                    document.getElementById("variable").focus();});
'

templateOptions = list.dirs(here("public")) %>%  basename() %>%
  .[which(. != "public")]

# ui ----
ui <- navbarPage(
  id = "main",

  # Application title
  title = "Shiny Data Entry",

  theme = shinytheme("flatly"),

  # javascript
  tags$head(tags$script(HTML(jscode))),
  shinyjs::useShinyjs(),
  tabPanel(
    "home",
    # add logout button UI
    div(class = "pull-right", shinyauthr::logoutUI(id = "logout")),
    div(
      HTML(
        "<h3>Forgot password? Email <a href=\"mailto:rbischoff@asu.edu\">rbischoff@asu.edu</a></h3>
"
      ),
id = "forgotPassword"
    ),
actionButton("create_user", "Create user"),
# add login panel UI function
shinyauthr::loginUI(id = "login"),
uiOutput("welcomeUI"),
br(),
br(),
br(),
div(id = "templates",
    fluidRow(wellPanel(
      h3("Select from existing data entry options"),
      selectInput(
        "chooseTemplate",
        "Choose template",
        choices = templateOptions,
        width = "400px"
      )
    )),
    fluidRow(wellPanel(
      h3("Upload new template (coming soon)"),
      fileInput(
        "files",
        "choose file(s) to import",
        multiple = T,
        width = "400px"
      )
    )))
  ),
tabPanel(
  title = "Data entry",
  value = "dataEntry",
  sidebarLayout(
    sidebarPanel(
      uiOutput("header"),
      textInput('ID', 'ID',
                placeholder = "catalog-number"),
      selectInput("variable", "variable",
                  ""),
      uiOutput('valueUI'),
      checkboxInput('check', 'Check if observation is uncertain'),
      fixedRow(
        column(
          2,
          tagAppendAttributes(actionButton('add', 'Add'),
                              `data-proxy-click` = "add")
        ),
        column(1),
        column(2, actionButton('skip', 'skip'))
      )
    ),
    mainPanel(
      actionButton('deleteAll', 'delete all'),
      actionButton('deleteRow', 'delete row'),
      downloadButton('download', 'download'),
      DT::DTOutput('table1')
    )
  )
)
)

# Server ----
server <- function(input, output, session) {


  database = readDatabase()

  observeEvent(credentials()$user_auth, {
    try({
      shinyjs::toggle("create_user", condition = isFALSE(credentials()$user_auth))
      shinyjs::toggle("forgotPassword", condition = isFALSE(credentials()$user_auth))
      shinyjs::toggle("templates", condition = isTRUE(credentials()$user_auth))
    })
  })

  # disable file upload until function is ready
  observe({
    try(disable("files"))
  })

  observeEvent(input$create_user, {
    showModal(
      modalDialog(
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
      )
    )
  })

  observeEvent(input$save_user, {
    # Get the values of the input fields
    username <- input$new_username
    password1 <- input$new_password1
    password2 <- input$new_password2
    name <- input$new_name

    # Validate the passwords match
    if (username %in% database$user) {
      showNotification("Username already exists. Please try again.",
                       type = "error")
      return()
    }

    if (password1 != password2) {
      showNotification("Passwords don't match. Please try again.",
                       type = "error")
      return()
    }

    new = tibble(
      user = username,
      password = sodium::password_store(password1),
      permissions = "standard",
      name = name
    )

    databaseNew = bind_rows(database, new)
    saveDatabase(databaseNew)

    showNotification(sprintf("User '%s' was created successfully!\n", new$user))
    removeModal()
    session$reload()

  })

  # login ----

  # call login module supplying data frame,
  # user and password cols and reactive trigger
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = database,
    user_col = user,
    pwd_col = password,
    sodium_hashed = T,
    log_out = reactive(logout_init())
  )

  # call the logout module with reactive trigger to hide/show
  logout_init <- shinyauthr::logoutServer(id = "logout",
                                          active = reactive(credentials()$user_auth))

  output$welcomeUI = renderUI({
    req(credentials()$user_auth)
    renderText(paste("Welcome", credentials()$info$name))
  })

  observeEvent(credentials()$user_auth, {
    print("showing data entry tab")
    try({
      if (credentials()$user_auth) {
        showTab("main", "dataEntry")
      } else {
        hideTab("main", "dataEntry")
      }
    })
  })

  observeEvent({
    credentials()$info
  },{
    print("admin tab")
    print(credentials()$info)
    try({
      if(credentials()$info$permissions == "admin"){
        insertTab(inputId = 'main',
                  tab = tabPanel('admin',
                                 actionButton('saveadmin','save'),
                                 DT::DTOutput('databaseDT')
                  ))
      } else {
        removeTab(inputId = 'main',target = 'admin', session = session)
      }
    })
  })

    # Observe cell edits and update the password column using sodium
  observeEvent(input$databaseDT_cell_edit, {
    print("editing admin table")
    try({
      info <- input$databaseDT_cell_edit
      str(info)  # Print cell edit information for debugging
      col = which(names(database) == "password")
      if(info$col == col){
        info$value = password_store(info$value)
      }
      database[[info$col]][info$row] <<- info$value
    })
  })

  observeEvent(input$saveadmin,{
    saveDatabase(database)
    database = readDatabase()
  })

  output$databaseDT = DT::renderDT({
    DT::datatable(database,editable = 'cell', selection = 'single')
  })

  # reactive values ----
  rvals <- reactiveValues(ignore = c())

  # load template ----

  observeEvent({
    credentials()$user_auth
    input$chooseTemplate
  }, {
    print("loading templates")
    try({
      req(credentials()$user_auth)
      req(input$chooseTemplate)
      fp = here("public", input$chooseTemplate, "InputOptions.xlsx")
      if (!file.exists(fp)) {
        fp = here(
          "public",
          credentials()$info$user,
          input$chooseTemplate,
          "InputOptions.xlsx"
        )
        if (!file.exists(fp)) {
          showNotification("Template does not exist", type = "error")
          return(NULL)
        }
      }
      options = safeImport(fp, range = "A1:A1000")

      indx = which(options$Settings == "Input")

      settings = safeImport(fp, range = glue::glue("A2:B{indx - 1}")) %>%
        filter(!is.na(field))

      # update application title
      rvals$title = tryCatch(
        settings %>% filter(field == 'name') %>% pull(parameter),
        error = function(e)
          return("Shiny Data Entry")
      )

      inputOptions = safeImport(fp, skip = indx + 1)

      inputOptions %<>%
        rowwise() %>%
        mutate(values = splitVals(values, sep)) %>%
        ungroup() %>%
        select(-sep)

      fpLook = str_replace(fp, "InputOptions.xlsx", "lookups.xlsx")
      lookups = safeImport(fpLook)
      rvals$inputOptions = inputOptions
      rvals$lookups = lookups
      updateSelectInput(
        session = session,
        inputId = "variable",
        choices = rvals$inputOptions$key %>%
          setNames(rvals$inputOptions$inputName)
      )
    })
  })


  # load saved input from tmp ----
  observeEvent({
    credentials()$user_auth
    input$chooseTemplate
  }, {
    req(credentials()$user_auth)
    print("loading prior values")
    try({
      if (!dir.exists(here("tmp")))
        dir.create(here("tmp"))
      rvals$dirpath = here("tmp", credentials()$info$user, input$chooseTemplate)
      if (Sys.info()["sysname"] == "Linux") {
        system(glue::glue("mkdir -p '{rvals$dirpath}'"))
      } else {
        if (!dir.exists(rvals$dirpath))
          dir.create(rvals$dirpath, recursive = T)
      }
      rvals$filepath = file.path(rvals$dirpath, "current.Rds")

      fntmp = rvals$filepath
      if (file.exists(fntmp)) {
        print("prior values exist")
        rvals$analysis = safeImport(fntmp)
      } else {
        warning("prior values do not exist")
        rvals$analysis = tibble()
      }
    })
  })

  # table output ----
  prntTbl = function() {
    output$table1 = DT::renderDT({
      DT::datatable(rvals$analysis,
                    rownames = F,
                    editable = "cell")
    })
  }
  prntTbl()

  proxyDT = dataTableProxy('table1')

  observeEvent(input$table1_cell_edit, {
    info = input$table1_cell_edit
    rvals$analysis <- editData(rvals$analysis, info, rownames = F)
    replaceData(proxyDT, rvals$analysis, resetPaging = FALSE)
  })

  # Define ui inputs ----

  output$header = renderUI(h2(rvals$title))

  output$valueUI = renderUI({
    req(credentials()$user_auth)
    req(input$variable)
    try({
      print("defining UI")
      row = rvals$inputOptions %>%
        dplyr::filter(key == input$variable)
      type = row %>%
        dplyr::pull(type)
      selectMultiple = row %>%
        dplyr::pull(multiple)
      if (!isTRUE(selectMultiple))
        selectMultiple = F
      rvals$type = type
      if (type == 'list') {
        qs =
          rvals$inputOptions %>%
          filter(key == input$variable) %>%
          pull(values) %>%
          unlist()
      } else if (type %in% c("lookup", "autolookup")) {
        qs = tryCatch({
          qstmp =
            rvals$lookups %>%
            filter(outputField == input$variable)
          lookupVal = rvals$analysis %>%
            filter(ID == input$ID) %>%
            filter(variable == qstmp$lookupField[1]) %>%
            pull(value)
          qstmp = qstmp %>%
            filter(lookup == lookupVal) %>%
            filter(!is.na(value)) %>%
            pull(value)
        }, error = function(e)
          return(""))
      } else if (type == "group") {
        qs = c("Yes", "No")
      }
      if (type == "text") {
        r = shiny::textInput('value', 'value')
      } else if (type == "numeric") {
        r = shiny::numericInput('value', 'value', 0)
      } else if (type == "list") {
        r = shiny::selectizeInput('value', 'value', qs, multiple = selectMultiple)
      } else if (type == "autoID") {
        r = shiny::textInput('value', 'value', placeholder = input$ID)
        updateTextInput(session = session,
                        inputId = 'value',
                        value = input$ID)
      } else if (type == "autolookup") {
        rvals$lookup =
          r = shinyjs::disabled(shiny::selectInput(
            'value',
            'value',
            qs,
            multiple = F,
            selected = qs
          ))
      } else if (type == "lookup") {
        r = shiny::selectInput('value', 'value', qs, multiple = selectMultiple)
      } else if (type == "function") {
        rvals$eval = T
        r = shiny::textInput('value', 'value')
      } else if (type == "group") {
        r = shiny::selectInput(
          'value',
          paste("include", input$variable, "variables?"),
          choices = qs,
          selected = "yes"
        )
      } else if (type == "user") {
        r = shiny::textInput('value', 'value')
        updateTextInput(
          session = session,
          inputId = 'value',
          value = credentials()$info$name
        )
      } else {
        r = shiny::textInput('value', 'value', placeholder = "error determining type")
      }
      print("UI definition completed")
      r
    })
  })

  # function update ----
  observeEvent(rvals$eval, {
    req(rvals$eval)
    try({
      if (rvals$eval) {
        f = rvals$inputOptions %>%
          filter(key == input$variable) %>%
          pull(`function`)
        val = tryCatch(
          eval(parse(text = f)),
          error = function(e)
            return("")
        )
        updateTextInput(session = session,
                        inputId = "value",
                        value = val)
        rvals$eval = F
      }
    })
  })

  # disable ----
  observeEvent({
    input$ID
    input$value
  }, {
    try({
      if (isValid(input$ID) && isValid(input$value)) {
        shinyjs::enable("add")
      } else {
        shinyjs::disable("add")
      }
    })
  })

  # buttons ----
  observeEvent(input$add, {
    req(input$variable)
    try({
      if (rvals$type != "group") {
        if (input$check == T) {
          value = paste0(input$value, '?')
          # reset checkbox
          updateCheckboxInput(
            session = session,
            inputId = 'check',
            label =  'Check if observation is uncertain',
            value = F
          )
        } else
          value = input$value
        value = paste(value, collapse = "; ")
        if (rvals$type == "autoID")
          value = input$ID

        new = tibble(ID = input$ID,
                     variable = input$variable,
                     value = value)
        # print(new)
        new = bind_rows(new, rvals$analysis) %>% distinct_all() %>%
          distinct(ID, variable, .keep_all = T)
        rvals$analysis = new
        prntTbl()

        # save results
        rvals$analysis %>% safeSaveRDS(rvals$filepath)
      }
      # reset select input
      nextKey = tryCatch({
        indx = which(rvals$inputOptions$key == input$variable)
        if (rvals$inputOptions$type[indx] == "group" &&
            input$value == "No") {
          rvals$ignore = c(rvals$ignore,
                           which(rvals$inputOptions$parent == rvals$inputOptions$key[indx]))
        }
        indx = indx + 1
        while (indx %in% rvals$ignore) {
          indx = indx + 1
        }
        rvals$inputOptions$key[indx]
      }, error = function(e)
        return(""))
      updateSelectizeInput(
        session = session,
        inputId =  "variable",
        choices = rvals$inputOptions$key %>%
          setNames(rvals$inputOptions$inputName),
        selected = nextKey
      )

      # refocus cursor
      session$sendCustomMessage(type = "refocus", message = list(NULL))
    })
  })

  observeEvent(input$skip, {
    try({
      nextKey = tryCatch({
        indx = which(rvals$inputOptions$key == input$variable)
        rvals$inputOptions$key[indx + 1]
      }, error = function(e)
        return(""))
      updateSelectizeInput(
        session = session,
        inputId =  "variable",
        choices = rvals$inputOptions$key %>%
          setNames(rvals$inputOptions$inputName),
        selected = nextKey
      )
    })
  })

  # delete everything -- including prior saved data
  observeEvent(input$deleteAll, {
    showModal(
      modalDialog(
        title = h1("Warning!", style = "color: red;"),
        "This will delete all data including everything saved in prior sessions!",
        footer = tagList(
          actionButton('deleteAllMod', 'delete', style = "color: red;"),
          actionButton('cancel', 'cancel')
        )
      )
    )
  })

  observeEvent(input$cancel, removeModal())

  observeEvent(input$deleteAllMod, {
    print('working?')
    rvals$analysis = tibble()
    prntTbl()
    # save results
    rvals$analysis %>% safeSaveRDS(rvals$filepath)
    removeModal()
  })

  observeEvent(input$deleteRow, {
    try({
      indx = input$table1_rows_selected

      # save deleted just in case
      removed = rvals$analysis %>%
        slice(indx)
      fntmp = here(rvals$dirpath, "deleted.Rds")
      if (file.exists(fntmp)) {
        old = safeImport(fntmp)
      } else {
        old = tibble()
      }
      removed = bind_rows(old, removed) %>%
        distinct_all()
      safeSaveRDS(removed, fntmp)

      new = rvals$analysis %>%
        slice(-indx)

      rvals$analysis <- new
      prntTbl()

      # save results
      rvals$analysis %>% safeSaveRDS(rvals$filepath)
    })
  })

  # download ----


  output$download = downloadHandler(
    filename = function() {
      glue::glue("output-{as.Date(Sys.time())}.xlsx")
    },
    content = function(file) {
      rio::export({
        exportAnalysis(rvals$analysis, rvals$inputOptions)
      }, file)
    }
  )

  session$allowReconnect(TRUE)
}


# Run the application
shinyApp(ui = ui, server = server)
