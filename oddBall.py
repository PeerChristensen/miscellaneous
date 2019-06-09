#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Sep 20 08:31:38 2017

@author: peerchristensen
"""
import numpy as np

''' UNUSED
stim=list(range(1,21))
odd=list(np.random.choice(2,21))
odd=[x+3 for x in odd]
'''

a=[0,0,1]
b=[0,0,0,1]

odds=list(np.random.choice(2,10))
stims=[]
for i in odds:
    if i == 0:
        stims.append(a)
    else:
        stims.append(b)
        
stims=[x for l in stims for x in l]
