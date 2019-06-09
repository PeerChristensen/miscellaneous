#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sat Sep 23 19:45:58 2017

@author: peerchristensen
"""

from psychopy import core, visual, gui, monitors, sound, event
import random, os
from random import sample
import numpy as np
core.wait(0.5)

win = visual.Window([1000,800], monitor="testMonitor",color=(-1,-1,-1))
mes = visual.TextStim(win, text="?")

def runExp():

    stims=list(np.arange(1,5.1,0.5))
    random.shuffle(stims)
    win.flip()
    core.wait(0.8)
    event.waitKeys(keyList=['return'])
    for i in range(1,(stims+1)):
        trialNum=visual.TextStim(win=win,text=str(i))
        trialNum.draw()
        print("Trial num: " + str(i))
        print(stims[i])
        win.flip()
        #image
        core.wait(1)
        #image draw time = stims[i]
        win.flip()
        core.wait(1)
        mes.draw()
        win.flip()
        event.waitKeys(keyList=['return'])
        #start time
        event.waitKeys(keyList=['return'])
        #get time
        #time= get time - start time
        diff = stims[i] - time
        print("diff: " + str(diff))
runExp()
core.quit