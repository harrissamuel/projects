#%%
## Samuel Harris 
## Web Scraper
## Web Scrape  FrankHarris.com

# %%
from selenium import webdriver
from selenium.webdriver.chrome.service import Service as ChromeService
# from webdriver_manager.chrome import ChromeDriverManager
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.common.by import By
from time import sleep
# import csv
import pandas as pd
from bs4 import BeautifulSoup
import numpy as np

driver = webdriver.Chrome()
url = 'https://frankharris.com/Artist.asp?ArtistID=26299&Akey=P7FJPT2G&ajx=1#!pf200572'
driver.get(url)
driver.quit()
print('test sucessfull')

# %%
def get_paintings():
    start = 0
    end = 32
    title_l = []
    price_l = []
    width_l = []
    height_l = []
    ########
    driver = webdriver.Chrome()
    url = 'https://frankharris.com/Artist.asp?ArtistID=26299&Akey=P7FJPT2G&ajx=1#!pf200572'
    driver.get(url)
    sleep(1)
    # Click image
    driver.find_element(By.CSS_SELECTOR, f'#pf200572_im{start}').click()
    #####################
    # Loop Through Painitings
    #####################
    for i in range(start, end):
        sleep(1)
        try:
            #Click see more
            driver.find_element(By.CSS_SELECTOR, f'#priceSpaceBg{i} > div.BtnPlace > a.PriceBtn.bigPrice.btn.btn-default').click()
            sleep(1.3)
            soup = BeautifulSoup(driver.page_source, 'html5lib')
            selectprice = soup.select(f'#priceSpaceBg{i} > div.priceInfoHere > div > div.PriceVal.col-xs-8.padTopBtm5')
            selecttitle = soup.select(f'#priceSpaceBg{i} > div.priceInfoHere > div > div:nth-child(1)')
            select_width = soup.select(f'#priceSpaceBg{i} > div.priceInfoHere > div > div:nth-child(4) > div.col-xs-4.powidth')
            select_h = soup.select(f'#priceSpaceBg{i} > div.priceInfoHere > div > div:nth-child(4) > div.col-xs-4.poheight')
            # Append
            title_l.append(selecttitle[0].text)
            price_l.append(selectprice[0].text)
            height_l.append(select_h[0].text)
            width_l.append(select_width[0].text)
        except: 
            soup = BeautifulSoup(driver.page_source, 'html5lib')
            selecttitle = soup.select(f'#InfoHolder{i} > div')
            if selecttitle == [] :
                title_l.append('No Title')
                price_l.append('Price on Request')
                height_l.append('none')
                width_l.append('none')
            elif selecttitle[0].text == '' :
                title_l.append('No Title')
                price_l.append('Price on Request')
                height_l.append('none')
                width_l.append('none')
            else:
                title_l.append(selecttitle[0].text)
                price_l.append('Price on Request')
                height_l.append('none')
                width_l.append('none')
            # Check if it is the last image, otherwise, click next
        if i == (end-1):
            driver.quit()
        else:   
            driver.find_element(By.CSS_SELECTOR, '#nxtArrow > i').click()
    
    d = {'title': title_l, 'price': price_l,
         'height': height_l, 'width': width_l}
    d = pd.DataFrame(d)
    return d
# %%
d  = get_paintings()
d['price'] = d['price'].str.replace(' USD', '')
d['height'] = d['height'].str.replace('Height: ', '')
d['width'] = d['width'].str.replace('Width:', '')

print(d)

# %%
d.index.name = 'painting'
print(d)
#d.to_csv('paintings.csv')

# %%
