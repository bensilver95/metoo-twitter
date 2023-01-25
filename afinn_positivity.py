#!/usr/bin/env python3

import pandas as pd
from afinn import Afinn
from progressbar import ProgressBar

afinn = Afinn()
pbar = ProgressBar()

homedir = "/home/bms2202/newmetoo/"

d = pd.read_csv(homedir + 'data/ALL_TWEETS.csv', low_memory = False)
d['tweet'] = d['tweet'].astype(str)

sentiment = []
for tweet in pbar(d['tweet']):
    try:
        sentiment.append(afinn.score(tweet)/len(tweet.split(' ')))
    except TypeError:
    	print('error, but its okay')
    	sentiment.append(0)

afinn_scores = pd.DataFrame({'date':d['date'],'PublicFigure':d['PublicFigure'],
                             'tweet_period':d['tweet_period'],'datediff':d['datediff'],
                             'liking_afinn':sentiment})
afinn_scores.to_csv(homedir + 'data/afinn_scores.csv', index = False)