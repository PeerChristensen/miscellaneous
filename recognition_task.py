#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Sep 21 08:25:14 2018

@author: peerchristensen
"""

import random, os, csv
import numpy as np
import itertools as it

image_path_A = "/Users/peerchristensen/Desktop/Khanyiso/Recognition/NC3/"
image_path_B = "/Users/peerchristensen/Desktop/Khanyiso/Recognition/NC7/"
image_path_C = "/Users/peerchristensen/Desktop/Khanyiso/Recognition/NC9/"
image_path_D = "/Users/peerchristensen/Desktop/Khanyiso/Recognition/NC11/"
    
images_A     = os.listdir(image_path_A)
images_B     = os.listdir(image_path_B)
images_C     = os.listdir(image_path_C)
images_D     = os.listdir(image_path_D) # contains only one item

n_items=4
combinations_A = list(it.permutations(images_A,n_items))
combinations_B = list(it.permutations(images_B,n_items))
combinations_C = list(it.permutations(images_C,n_items))
#combinations_D = list(it.permutations(images_D,n_items))

random.shuffle(combinations_A)
random.shuffle(combinations_B)
random.shuffle(combinations_C)

sets_selected = np.random.randint(1,4,10) # sets A,B,C - 10 sets

congruent_displays = []

congruent_displays = []

for set in sets_selected:
    if set == 1:
        congruent_displays.append(combinations_A[0])
        combinations_A.pop(0)
    elif set == 2:
        congruent_displays.append(combinations_B[0])
        combinations_B.pop(0)
    else:
        congruent_displays.append(combinations_C[0])
        combinations_C.pop(0)
        
incongruent_displays = []

for i in range(1,11):
    set = []
    random.shuffle(images_A)
    random.shuffle(images_B)
    random.shuffle(images_C)
    random.shuffle(images_D)
    set.append(images_A[0])
    set.append(images_B[0])
    set.append(images_C[0])
    set.append(images_D[0])
    random.shuffle(set)
    incongruent_displays.append(set)

displays_mem = [congruent_displays[0:5],incongruent_displays[0:5]]

#flatten list of lists
displays_mem = [item for sublist in displays_mem for item in sublist] 
random.shuffle(displays_mem)

test_displays = [congruent_displays,incongruent_displays]
test_displays = [item for sublist in test_displays for item in sublist] 
random.shuffle(test_displays)

    
    
    