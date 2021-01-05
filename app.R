library(shiny)
library(shinyjs)
library(tidyverse)
library(jsonlite)

# testing = TRUE
testing = FALSE

source('setup.R')

prices = file.path('data', 'prices') %>%
    file.path(list.files(.)) %>%
    map_df(function(path) {
        file.path(path, list.files(path)) %>%
            map_df(function(file) {
                prices = file %>%
                    fromJSON()
                data.frame(
                    date = prices$date,
                    prices$data
                )
            })
    }) %>%
    arrange(desc(date)) %>%
    
    # get most recent data for each item
    group_by(itemId) %>%
    filter(row_number() == max(row_number())) %>%
    ungroup() %>%
    
    # convert price string to numeric
    mutate(price = parse_price(price_chr)) %>%
    
    # unstrung jewellery has the same name as finished jewellerey
    mutate(item = ifelse(grepl(pattern = 'needs a string', x = description), paste(item, '(unstrung)'), item)) %>%
    
    # modify item name for other duplicates
    group_by(item) %>%
    mutate(item = ifelse(row_number() > 1, paste0(item, ' (alt', row_number(), ')'), item)) %>%
    
    ungroup() %>%
    arrange(desc(date), itemId)


recipes = readRDS('recipes')
# profit = map_recipe(recipes, prices)
choices = unique(prices$item)


# UI ----------------------------------------------------------------------------------------------------------------

ui <- fluidPage(
    
    useShinyjs(),
    includeCSS('ge.css'),
    
    div(verbatimTextOutput(outputId = 'test')),
    
    # Content 
    
    div(
        
        # Recipe comparison
        
        div(
            div(
                selectInput(inputId = 'recipe_compare', label = 'Compare recipes', choices = names(recipes), multiple = TRUE, width = '100%')
            ),
            div(
                uiOutput(outputId = 'ui_recipe_compare'),
                class = 'mb'
            ),
            style = ''
        ),
        
        
        # Saved recipes
        
        div(
            
            div(
                selectInput(inputId = 'recipes', label = 'Saved recipes', choices = "Custom", width = '100%'),
                style = 'width: 300px; margin-right: 15px;'
            ),
            div(
                disabled(actionButton(inputId = 'save_recipe', label = 'Save recipe')),
                style = 'margin-right: 15px'
            ),
            div(
                id = 'div_delete_recipe',
                disabled(actionButton(inputId = 'delete_recipe', label = 'Delete recipe')),
                style = 'margin-right: 15px'
            ),
            style = 'display: flex; justify-content: center'
            
        ),
        
        # Custom recipe
        
        div(
            div(
                div(
                    selectInput(inputId = "finished", label = "Finished product", choices = choices, multiple = TRUE, width = "100%")
                ),
                div(
                    uiOutput(outputId = 'ui_finished_qty')
                ),
                div(
                    uiOutput(outputId = 'ui_finished_cost')
                ),
                style = 'width: 42.5%'
            ),
            div(
                div(
                    selectInput(inputId = "raw", label = "Raw material", choices = choices, multiple = TRUE, width = "100%"),
                ),
                div(
                    uiOutput(outputId = 'ui_raw_qty')
                ),
                div(
                    uiOutput(outputId = 'ui_raw_cost')
                ),
                style = 'width: 42.5%'
                
            ),
            style = 'display: flex; justify-content: space-around'
        ),
        
        style = 'margin: 15px'
        
    )
    
)



# Server -----------------------------------------------------------------------------------------------------------

server <- function(input, output, session) {
    
    output$test = renderPrint({
        # cost_raw()
    })
    
    rv = reactiveValues(
        recipes = readRDS(file = 'recipes')
    )
    
    
    # Initialize recipe comparison data
    
    observeEvent(rv$recipes, {
        rv$profit = map_recipe(rv$recipes, prices)
    })
    
    
    # Initialize recipe selection
    
    observeEvent(rv$recipes, {
        updateSelectInput(session = session, inputId = 'recipes', choices = c('Custom', names(rv$recipes)))
    })
    
    
    # Prevent duplicate selections
    
    observeEvent(input$finished, {
        rv$raw <- input$raw
        if (any(input$finished %in% input$raw)) {
            choices_raw = choices[!choices %in% input$finished]
            updateSelectInput(session = session, inputId = 'raw', choices = choices_raw, selected = rv$raw)
        }
    })
    
    
    # Finished product -----------------------------------------------------------
    
    # Generate quantity inputs for each finished material
    
    output$ui_finished_qty = renderUI({
        req(input$finished)
        qty_inputs = 1:length(input$finished) %>%
            map(function(i) {
                div(
                    numericInput(
                        inputId = paste0('finished_qty', i), 
                        label = paste('Qty', input$finished[i]), 
                        value = 1, 
                        min = 1,
                        width = '100%'
                    ),
                    style = paste0('width: ', 100/length(input$finished) - 1, '%')
                )
            })
        div(qty_inputs, style = 'display: flex; justify-content: space-around')
    })
    
    
    # Get price and cost
    
    cost_finished = reactive({
        req(input$finished)
        tryCatch(expr = {
            1:length(input$finished) %>%
                map(function(i) {
                    qty = parse(text = paste0('input$finished_qty', i)) %>% eval()
                    prices %>%
                        filter(item == input$finished[i]) %>%
                        mutate(qty = qty, cost = price * qty) %>%
                        as.list()
                })
        }, error = function(e) return())
    })
    
    
    # Generate outputs
    
    output$ui_finished_cost = renderUI({
        req(cost_finished())
        
        costs = tryCatch(expr = {
            
            raw = cost_raw() %>%
                bind_rows() %>%
                summarise(cost = sum(cost))
            
            cost_finished() %>%
                map(function(item) {
                    div(
                        div(div('Price:'), div(item$price_chr), class = 'card-pair'),
                        div(div('Cost:'), div(rs_price(item$cost)), class = 'card-pair', style = 'margin-bottom: 10px'),
                        div(div('Profit:'), div(rs_price(item$cost - raw$cost)), class = 'card-pair'),
                        div(div('Return:'), div(round((item$cost - raw$cost) / raw$cost * 100, 1) %>% paste0('%')), class = 'card-pair'),
                        style = paste0('width: ', 100/length(cost_finished()) - 1, '%'),
                        class = 'card'
                    )
                })
            
        }, error = function(e) {
            
            cost_finished() %>%
                map(function(item) {
                    div(
                        div(div('Price:'), div(item$price_chr), class = 'card-pair'),
                        div(div('Cost:'), div(rs_price(item$cost)), class = 'card-pair', style = 'margin-bottom: 10px'),
                        div(div('Profit:'), div('-'), class = 'card-pair'),
                        div(div('Return:'), div('-'), class = 'card-pair'),
                        style = paste0('width: ', 100/length(cost_finished()) - 1, '%'),
                        class = 'card'
                    )
                })
            
        })
        
        div(costs, style = 'display: flex; justify-content: space-around')
    })
    
    
    
    # Raw material --------------------------------------------------------------
    
    # Generate quantity inputs for each raw material
    
    output$ui_raw_qty = renderUI({
        req(input$raw)
        qty_inputs = 1:length(input$raw) %>%
            map(function(i) {
                div(
                    numericInput(
                        inputId = paste0('raw_qty', i), 
                        label = paste('Qty', input$raw[i]), 
                        value = 1, 
                        min = 1,
                        width = '100%'
                    ),
                    style = paste0('width: ', 100/length(input$raw) - 1, '%')
                )
            })
        div(qty_inputs, style = 'display: flex; justify-content: space-around')
    })
    
    
    # Get price and cost for each raw material
    
    cost_raw = reactive({
        req(input$raw)
        tryCatch(expr = {
            1:length(input$raw) %>%
                map(function(i) {
                    qty = parse(text = paste0('input$raw_qty', i)) %>% eval()
                    prices %>%
                        filter(item == input$raw[i]) %>%
                        mutate(qty = qty, cost = price * qty) %>%
                        as.list()
                })
        }, error = function(e) return())
    })
    
    
    # Generate outputs for raw material cost
    
    output$ui_raw_cost = renderUI({
        req(cost_raw())
        costs = cost_raw() %>%
            map(function(item) {
                div(
                    div(div('Price:'), div(item$price_chr), class = 'card-pair'),
                    div(div('Cost:'), div(rs_price(item$cost)), class = 'card-pair'),
                    style = paste0('width: ', 100/length(cost_raw()) - 1, '%'),
                    class = 'card'
                )
            })
        div(costs, style = 'display: flex; justify-content: space-around')
    })
    
    
    
    # Recipe Comparison ----------------------------------------------------------------------
    
    output$ui_recipe_compare = renderUI({
        req(input$recipe_compare)
        returns = input$recipe_compare %>%
            map(function(recipe) {
                x = rv$profit[[recipe]]
                items = x$ingredients$item %>%
                    map(function(item) {
                        div(div(item), div(x$ingredients$price_chr[x$ingredients$item == item]), class = 'card-pair')
                    })
                div(
                    div(tags$strong(recipe), style = 'text-align: center; margin-bottom: 10px'),
                    div(div('Return:'), div(x$return), class = 'card-pair'),
                    div(div('Profit:'), div(rs_price(x$profit)), class = 'card-pair'),
                    div(div('Revenue:'), div(rs_price(x$revenue)), class = 'card-pair', style = 'margin-bottom: 10px'),
                    div(
                        div(div('Cost:'), div(rs_price(x$cost)), class = 'card-pair'),
                        div(items),
                        style = 'color: gray'
                    ),
                    class = 'card',
                    style = 'min-width: 200px; margin-right: 10px'
                )
            })
        div(returns, style = 'display: flex; overflow-x: auto')
    })
    
    
    
    # Recipes --------------------------------------------------------------------------------
    
    observeEvent(c(input$finished, input$raw), {
        if (length(input$finished) < 1 | length(input$raw) < 1) {
            disable(id = 'save_recipe')
        } else {
            enable(id = 'save_recipe')
        }
    })
    
    observeEvent(input$save_recipe, {
        if (any(input$finished %in% names(rv$recipes))) {
            existing_recipes = input$finished[input$finished %in% names(rv$recipes)] %>%
                paste0(collapse = ', ')
            showModal(modalDialog(
                div(
                    div(
                        div('The following items already have saved recipes - do you want to overwrite them?'),
                        div(tags$strong(existing_recipes)),
                        style = 'text-align: center'
                    ),
                    tags$br(),
                    div(
                        div(
                            actionButton(inputId = 'recipe_check_yes', label = 'Yes', width = '100%'),
                            style = 'width: 100px; margin-right: 5px'
                        ),
                        div(
                            actionButton(inputId = 'recipe_check_no', label = 'No', width = '100%'),
                            style = 'width: 100px; margin-left: 5px'
                        ),
                        style = 'display: flex; justify-content: center'
                    )
                ),
                footer = NULL, 
                size = 'm'
            ))
        } else {
            recipe_update = rv$recipes
            for (product in input$finished) {
                recipe_update[[product]] = input$raw
                if (!testing) {
                    saveRDS(object = recipe_update, file = 'recipes')
                    rv$recipes = readRDS(file = 'recipes')
                } else {
                    rv$recipes = recipe_update
                }
            }
            updateSelectInput(session = session, inputId = 'recipes', selected = input$finished[1])
            updateSelectInput(session = session, inputId = 'recipe_compare', choices = names(rv$recipes))
            insertUI(selector = '#div_delete_recipe', where = 'afterEnd', ui = div(id = 'msg_save', icon('check-circle'), style = 'align-self: center'))
            shinyjs::delay(ms = 1500, expr = removeUI(selector = '#msg_save'))
        }
    })
    
    observeEvent(input$recipe_check_yes, {
        recipe_update = rv$recipes
        for (product in input$finished) {
            recipe_update[[product]] = input$raw
            if (!testing) {
                saveRDS(object = recipe_update, file = 'recipes')
                rv$recipes = readRDS(file = 'recipes')
            } else {
                rv$recipes = recipe_update
            }
        }
        updateSelectInput(session = session, inputId = 'recipes', selected = input$finished[1])
        updateSelectInput(session = session, inputId = 'recipe_compare', choices = names(rv$recipes))
        removeModal()
        insertUI(selector = '#div_delete_recipe', where = 'afterEnd', ui = div(id = 'msg_save', icon('check-circle'), style = 'align-self: center'))
        shinyjs::delay(ms = 1500, expr = removeUI(selector = '#msg_save'))
    })
    
    observeEvent(input$recipe_check_no, removeModal())
    
    
    
    # Recipe selection
    
    observeEvent(input$recipes, {
        if (input$recipes != 'Custom') {
            updateSelectInput(session = session, inputId = 'finished', selected = input$recipes)
            selected = rv$recipes[[input$recipes]]
            updateSelectInput(session = session, inputId = 'raw', selected = selected)
        }
    })
    
    
    # Delete recipe 
    
    observeEvent(input$recipes, {
        if (input$recipes == 'Custom') {
            disable(id = 'delete_recipe')
        } else {
            enable(id = 'delete_recipe')
        }
    })
    
    observeEvent(input$delete_recipe, {
        if (input$recipes != 'Custom') {
            rv$recipes = rv$recipes[!names(rv$recipes) %in% input$recipes]
            if (!testing) saveRDS(object = rv$recipes, file = 'recipes')
            updateSelectInput(session = session, inputId = 'recipe_compare', choices = names(rv$recipes))
        }
    })
    
    
    
}

shinyApp(ui, server)