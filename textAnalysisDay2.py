#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Nov 23 09:20:55 2017

@author: peerchristensen
"""

#core
import io, re,os

#string management
from unidecode import unidecode
from pandas import DataFrame

path = '/Users/peerchristensen/Desktop/ling-evid/ling-evid-master/DATA/KJV'
os.listdir(path)

texts=[]
for i in os.listdir(path):
    dpath=os.path.join(path,i)
    for j in os.listdir(dpath):
        texts.append(j)
        

def read_dir(path, SPLITCHAR = '\n', NORM = False):
    """ get paragraphs from unicode documents in subdirectories of root directory on path (walk to subsub directories)
    - SPLITCHAR: escape sequences for segment (default paragraph)
    - NORM: normalization optional (remove anything but alphabetic characters and decode unicode as ascii)
    """
    paragraphs_ls, filenames_ls = [], []
    for (root, dirnames, filenames) in os.walk(path):
        for filename in filenames:
            filepath = os.path.join(root,filename)
            with io.open(filepath, 'r') as f:
                text = f.read()
                paragraphs = text.split(SPLITCHAR)
                del paragraphs[0]
                i = 0
                for paragraph in paragraphs:
                    paragraph = paragraph.rstrip()
                    if paragraph:
                        if NORM:
                            paragraph = re.sub(r'\W+',' ', paragraph)
                            paragraph = re.sub(r'\d','',paragraph)
                            paragraph = re.sub(r'  +',' ', paragraph)
                            paragraph = unidecode(paragraph.lower())
                        paragraphs_ls.append(paragraph)
                        filenames_ls.append(filename+'_'+ str(i))
                        i += 1
    return filenames_ls, paragraphs_ls

def make_df(path, classification):
    """ export directory walk to dataframe with CLASS INFORMATION filename as index
    """
    filenames, paragraphs = read_dir(path, NORM = True)
    rows = []
    idx = []
    i = 0
    for paragraph in paragraphs:
        rows.append({'text': paragraph, 'class': classification})
        idx.append(filenames[i])
        i += 1
    df = DataFrame(rows, index = idx)
    return df

## CLASS LABELS
NT = 'new_testament'
OT = 'old_testament'
### map CLASS to PATH
SRCS = [("DATA/KJV/OT", OT),("DATA/KJV/NT", NT)]

## Build dataframe
DATA = DataFrame({'text': [], 'class': []})
for path, classification in SRCS:
    DATA = DATA.append(make_df(path, classification))

### inspect
print DATA.shape
DATA.head()
DATA.tail()
print DATA.text.iloc[0]

## export
DATA.to_csv("DATA/CLASS_DATA.csv")