#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Dec  7 09:40:17 2018

@author: peerchristensen
"""

import sys
if sys.version_info[0] < 3:
    import got
else:
    import got3 as got
 
def main():
 
    def printTweet(descr, t):
        print(descr)
        print("Username: %s" % t.username)
        print("Retweets: %d" % t.retweets)
        print("Text: %s" % t.text)
        print("Mentions: %s" % t.mentions)
        print("Hashtags: %s\n" % t.hashtags)
 '''
    # Example 1 - Get tweets by username
    tweetCriteria = got.manager.TweetCriteria().setUsername('barackobama').setMaxTweets(1)
    tweet = got.manager.TweetManager.getTweets(tweetCriteria)[0]
 
    printTweet("### Example 1 - Get tweets by username [barackobama]", tweet)
 '''
    # Example 2 - Get tweets by query search
    tweetCriteria = got.manager.TweetCriteria().setQuerySearch('vitaepro').setSince("2012-05-01").setUntil("2018-12-07").setMaxTweets(100)
    tweet = got.manager.TweetManager.getTweets(tweetCriteria)[0]
 
    printTweet("### Example 2 - Get tweets by query search [europe refugees]", tweet)
 '''
    # Example 3 - Get tweets by username and bound dates
    tweetCriteria = got.manager.TweetCriteria().setUsername("barackobama").setSince("2015-09-10").setUntil("2015-09-12").setMaxTweets(1)
    tweet = got.manager.TweetManager.getTweets(tweetCriteria)[0]
 
    printTweet("### Example 3 - Get tweets by username and bound dates [barackobama, '2015-09-10', '2015-09-12']", tweet)
 '''
if __name__ == '__main__':
    main()