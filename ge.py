import requests, json, os
from math import ceil
from datetime import date


# Item category

def get_category(categoryId):
    url = "https://services.runescape.com/m=itemdb_rs/api/catalogue/category.json?category=" + str(categoryId)
    response = requests.get(url)
    categories = response.json()
    categories = categories['alpha']
    items = {}
    for item in categories:
        if item['items'] > 0:
            for page in range(1, ceil(item['items'] / 12) + 1):
                alphaId = item['letter']
                url = 'https://services.runescape.com/m=itemdb_rs/api/catalogue/items.json?category=' + str(categoryId) + '&alpha=' + alphaId + '&page=' + str(page)
                response = requests.get(url)
                itemGroup = response.json()
                for itemDetail in itemGroup['items']:
                    items[itemDetail['id']] = itemDetail
    return items

# test_category = get_category(8)


# Item details

def get_items(category):
    prices = {}
    for itemId in category:
        url = 'https://services.runescape.com/m=itemdb_rs/api/catalogue/detail.json?item=' + str(itemId)
        response = requests.get(url)
        itemDetail = response.json()
        prices[itemId] = itemDetail['item']
    return prices

# test_items = get_items(test_category)


# Item prices

def get_prices(items):
    prices = {
        'date': date.today().strftime('%Y-%m-%d'), 
        'data': {'itemId': [], 'type': [], 'item': [], 'price_chr': [], 'description': []}
    }
    for item in items:
        itemData = items.get(item)
        prices['data']['itemId'].append(item)
        prices['data']['type'].append(itemData['type'])
        prices['data']['item'].append(itemData['name'])
        prices['data']['price_chr'].append(str(itemData['current']['price']))
        prices['data']['description'].append(itemData['description'])
    return prices

# test_prices = extract_prices(test_items)



# Cache data
# error with 5

categoryId = 21
category = get_category(categoryId)
items = get_items(category)
prices = get_prices(items)

today = date.today().strftime('%Y-%m-%d')
directory = 'data/prices/' + today

if not os.path.exists(directory):
    os.makedirs(directory)

file = directory + '/' + str(categoryId) + '.json'
with open(file, 'w') as outfile:
    json.dump(prices, outfile)



# Full item list
# Category 5 (Construction Materials) does not work for some reason

categoryIds = [8, 16, 25, 32, 19, 20]
# categoryIds = range(8, 11)
for categoryId in categoryIds:
    category = get_category(categoryId)
    items = get_items(category)
    prices = get_prices(items)
    today = date.today().strftime('%Y-%m-%d')
    directory = 'data/prices/' + today
    if not os.path.exists(directory):
        os.makedirs(directory)
    file = directory + '/' + str(categoryId) + '.json'
    with open(file, 'w') as outfile:
        json.dump(prices, outfile)

