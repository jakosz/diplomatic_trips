"""
Simple script that processess data from http://en.wikipedia.org/wiki/List_of_diplomatic_visits_to_the_Philippines into a csv file
"""
from bs4 import BeautifulSoup

import pandas as pd
from collections import defaultdict
import ipdb
html_doc = open('C:/Users/mikol_000/Documents/Diplomatic trips/trips.htm',"r")
soup = BeautifulSoup(html_doc)

tables = soup.find_all("table", "wikitable")


presidents_html = soup.find_all('span', id=lambda x: x and x.startswith('President'))
"""
from each table extract: start of trip , end of trip, country, cities and reason 
for the trip. Store data in a dictionary
"""
results = list()
for t_num, table in enumerate(tables):
    president_name =  presidents_html[t_num].get_text().encode('utf8')
    # dont include data before 1945
    if t_num == 13:
        break
    colnames = list()
    for i,row in enumerate(table.findAll('tr')):
        #ipdb.set_trace()
        if i == 0:
            colnames = [ name.get_text() for name in row.findAll('th') ]
            continue
        try:
            aux = row.findAll('td')
            thisrow = list()
            for j, cell in enumerate(aux):
                if j ==0:
                    continue
                if cell.text != None:
                    
                    thisrow.append( cell.get_text().encode('utf8').replace('\n','') )
                else:
                    thisrow.append( 'N/A')
                    
            results.append(thisrow[-5:] + [president_name])
        except IndexError:
            pass


#ipdb.set_trace()

presidents_trips_df = pd.DataFrame(results)


presidents_trips_df.to_csv('C:/Users/mikol_000/Documents/Diplomatic trips/presidents_trips_1945_2014.csv')





