# -*- coding: utf-8 -*-
"""
Created on Wed Oct 23 20:53:06 2019

@author: Margot

python 3.5.2
pandas 0.21.0

"""

import pandas as pd
from datetime import timedelta

#Read in wifi data and drop unnecessary columns
df = pd.read_csv('wifi.csv')
df = df.drop(columns=['src_ip','Department','Display_Department','MBU','Registrar_School','uvaPersonIAMAffiliation'])

#select library and filter data for that library
library = "harrison"
df = df[df['location'] == library]

#initialize spells container
spells = []

#convert the time column to datetimes so we can work with them more easily
df['time']= pd.to_datetime(df['time']) 

#get a list of unique users to iterate through
uniques = list(pd.unique(df['user']))

#iterate through each user and calculate spells
for i,patron in enumerate(uniques):
    
    #prints the index so we can keep track of how far it's gotten
    print(i)
    
    #get all the times that the user was in the library
    patronstay = df['time'][df['user'] == patron]
    
    #diff all the times
    diffs = [patronstay.iloc[x+1]-patronstay.iloc[x] for x in range(len(patronstay)-1)]
    
    #get the indices for the times when there was at least a half hour gap 
    breaks = [i for i,dif in enumerate(diffs) if dif > timedelta(0,1800)]
    
    #if the user was there continuously, just report the entire time duration
    if len(breaks) == 0:
        spells.extend([(patron,patronstay.iloc[-1]-patronstay.iloc[0],patronstay.iloc[0])])
    #otherwise get the stay lengths from the times between half hour breaks
    else:
        breaks = [-1] + breaks + [len(patronstay)-1]
        userspells = [patronstay.iloc[breaks[i+1]]-patronstay.iloc[b+1] for i,b in enumerate(breaks) if i < len(breaks)-1]
        spells.extend([(patron,spell,patronstay.iloc[breaks[i+1]]) for i,spell in enumerate(userspells)])

#convert the list into a data frame and write out
spellsdf = pd.DataFrame(spells, columns = ["user","stay","time"])
spellsdf.to_csv(library+'-spells.csv')
