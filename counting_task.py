#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Sep 26 15:30:35 2018

@author: peerchristensen
"""

from psychopy import visual, event, core, gui
import random
import numpy as np
import itertools as it
import os, random

#load images
path  = "/Users/peerchristensen/Desktop/Khanyiso/Recognition/NC3/"

image_path_A = "/Users/peerchristensen/Desktop/Khanyiso/Recognition/NC3/"
image_path_B = "/Users/peerchristensen/Desktop/Khanyiso/Recognition/NC7/"
image_path_C = "/Users/peerchristensen/Desktop/Khanyiso/Recognition/NC9/"
image_path_D = "/Users/peerchristensen/Desktop/Khanyiso/Recognition/NC11/"
    
images_A     = [image_path_A + image for image in os.listdir(image_path_A)]
images_B     = [image_path_B + image for image in os.listdir(image_path_B)]
images_C     = [image_path_C + image for image in os.listdir(image_path_C)]
images_D     = [image_path_D + image for image in os.listdir(image_path_D)] # contains only one item

random.shuffle(images_A)
random.shuffle(images_B)
random.shuffle(images_C)

# create target-distractor pairs within noun classes
list_A1 = list(it.permutations(images_A,2))
list_B1 = list(it.permutations(images_B,2))
list_C1 = list(it.permutations(images_C,2))


# create pairs between noun classes
# A is target class
BC_list = images_B+images_C
BC_list = np.random.choice(BC_list,len(list_A1))

list_A2 = []
for i in range(0,len(list_A1)):
    targ  = list_A1[i][0]
    distr = BC_list[i]
    item=[targ,distr]
    list_A2.append(item)
    
# B is target class
AC_list = images_A+images_C
AC_list = np.random.choice(AC_list,len(list_B1))

list_B2 = []
for i in range(0,len(list_B1)):
    targ  = list_B1[i][0]
    distr = AC_list[i]
    item=[targ,distr]
    list_B2.append(item)
    
# C is target class
AB_list = images_A+images_B
AB_list = np.random.choice(AB_list,len(list_C1))

list_C2 = []
for i in range(0,len(list_C1)):
    targ  = list_C1[i][0]
    distr = AB_list[i]
    item=[targ,distr]
    list_C2.append(item)

# reduce n displays to 60 in total
n_sets = 30
n_displays = 60

stims = [list_A1,list_B1,list_C1,list_A2,list_B2,list_C2]
#[random.shuffle(sublist) for sublist in stims]
stims = [x[:int((n_sets/3))] for x in stims] # subset to limit stims

within_sets  = [i for sublist in stims[:3] for i in sublist]
# convert tuples to lists
within_sets = [list(elem) for elem in within_sets]
between_sets = [i for sublist in stims[3:] for i in sublist]

displays = within_sets, between_sets
displays = [i for sublist in displays for i in sublist]
random.shuffle(displays)

# determine whether target n should match
k = 0
for i in displays:
    if  k < 15 or k >= 30 and k < 45:
        i.append(1)
    else:
        i.append(0)
    k += 1

# add n targets and distractors
for i in displays:
    i.append(np.random.choice(range(8,12)))
    i.append(np.random.choice(range(8,12)))

random.shuffle(displays)

# make a grid
screen_size = [1000,800]
size= [100,100]
units = "pix"

#win   = visual.Window(size=screen_size, monitor="testMonitor",color=(1,1,1))

# set positions
widths    = list(np.arange((-screen_size[0]/2)+150,(screen_size[0]/2)-50,150))
heights   = list(np.arange((-screen_size[1]/2)+150,(screen_size[1]/2)-50,150))
positions = list(it.product(widths, heights))
positions = [list(elem) for elem in positions]

#add some randomness to positions
for pos in positions:
    pos[0] = pos[0] + random.randint(-25,26)
    pos[1] = pos[1] + random.randint(-25,26)

# experiment
trial  = 1

positions2 = positions
for i in displays:
    random.shuffle(positions2)
    stims = []
    for j in range(1,i[3]+1):
        #targ  = visual.ImageStim(win, image=i[0], size=size, units=units, pos=positions2[j])
        targ = j
        stims.append(targ)
        print(positions2[j])
        positions2.pop(0)
        print(len(positions2))
    for k in range(1,i[4]+1):
        #distr  = visual.ImageStim(win, image=i[1], size=size, units=units, pos=positions2[k])
        distr = k
        stims.append(distr)
        print(positions2[j])
    #for stim in stims:
    #    stim.draw()
    positions2 = positions


for i in range(0,len(displays)):
    random.shuffle(positions)
    stims=[]
    for j in range(1,displays[i][3]+1):
        #targ  = visual.ImageStim(win, image=i[0], size=size, units=units, pos=positions2[j])
        targ = j
        stims.append(targ)
        print(len(stims))
        print(positions[j])




