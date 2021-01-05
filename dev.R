library(tidyverse)
library(reticulate)
library(jsonlite)

categories = read_csv(file = 'categories.csv')

prices = file.path('data', 'prices') %>%
    file.path(list.files(.)) %>%
    map_df(function(path) {
        prices = file.path(path, list.files(path)) %>%
            fromJSON()
        data.frame(
            date = prices$date,
            prices$data
        )
    })




# Convert string prices from API into numeric values

parse_price = function(text) {
    unit = ifelse(
        test = grepl(pattern = 'k', x = text), 
        yes = 10^3, 
        no = ifelse(
            test = grepl(pattern = 'm', x = text), 
            yes = 10^6, 
            no = ifelse(
                test = grepl(pattern = 'b', x = text), 
                yes = 10^9, 
                no = 1
            )
        ))
    price = as.numeric(gsub(pattern = ',|k|m|b', replacement = '', x = text)) * unit
    return(price)
}


# Convert numeric values into string format used by API

rs_price = function(price) {
    p = floor(log10(price))
    price_adj = case_when(
        p >= 9 ~ paste0(price / 10^9, 'b'),
        p >= 6 & p < 9 ~ paste0(price / 10^6, 'm'),
        p >= 4 & p < 6 ~ paste0(price / 10^3, 'k'),
        TRUE ~ format(price, big.mark = ',', justify = 'right')
    )
    price_formatted = str_trim(price_adj)
    return(price_formatted)
}



prices %>%
    mutate(price = parse_price(price_chr))

