
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


# Map recipes to price data

map_recipe = function(recipe_list, price_table) {
    names(recipe_list) %>%
        map(function(recipe) {
            revenue = price_table %>%
                filter(item == recipe)
            cost = price_table %>%
                filter(item %in% recipe_list[[recipe]]) %>%
                arrange(desc(price))
            list(
                recipe = recipe,
                revenue = revenue$price,
                cost = sum(cost$price),
                profit = revenue$price - sum(cost$price),
                return = round((revenue$price - sum(cost$price)) / sum(cost$price) * 100, 1) %>% paste0('%'),
                ingredients = cost
            )
        }) %>%
        set_names(names(recipe_list))
}


