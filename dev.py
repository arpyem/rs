import requests
from math import ceil
from time import time
from pprint import pprint

startTime = time()


# Get category list

baseURL = "https://services.runescape.com/m=itemdb_rs/api/catalogue/category.json?category="
categoryID = "8"
URL = baseURL + categoryID

response = requests.get(url = URL)
categories = response.json()
categories = categories['alpha']

pprint(categories)



# Get item info from category list
# items endpoint returns first 12 items using the category id, first letter of the item, and the page number

items = {}
for item in categories:
    if item['items'] > 0:
        for page in range(1, math.ceil(item['items'] / 12) + 1):
            alphaID = item['letter']
            itemURL = 'https://services.runescape.com/m=itemdb_rs/api/catalogue/items.json?category=' + categoryID + '&alpha=' + alphaID + '&page=' + str(page)
            response = requests.get(url = itemURL)
            itemGroup = response.json()
            for itemDetail in itemGroup['items']:
                items[itemDetail['id']] = itemDetail

pprint(items)



# Get current prices for each item

prices = {}
for itemId in items:
    URL = 'https://services.runescape.com/m=itemdb_rs/api/catalogue/detail.json?item=' + str(itemId)
    response = requests.get(url = URL)
    itemDetail = response.json()
    prices[itemId] = itemDetail['item']

pprint(prices)



endTime = time()
endTime - startTime


dfPrices = {'item': [], 'price': []}
for item in prices.values():
    dfPrices['item'].append(item['name'])
    dfPrices['price_chr'].append(str(item['current']['price']))

pprint(dfPrices)





