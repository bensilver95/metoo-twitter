#!/usr/bin/env python3

import pandas as pd
from nltk import word_tokenize
from collections import Counter
from progressbar import ProgressBar

homedir = "/home/bms2202/newmetoo/"

MFDdict = pd.read_csv(homedir + 'MFD.txt', sep = '\t', names = ['word','domain'])
MFDkey = {1: "care.virtue", 2: "care.vice", 3: "fairness.virtue", 4: "fairness.vice",
          5: "loyalty.virtue", 6: "loyalty.vice", 7: "authority.virtue", 8: "authority.vice",
          9: "sanctity.virtue", 10: "sanctity.vice"} 
MFDdict = MFDdict.replace(MFDkey)

tweets = pd.read_csv(homedir + 'data/ALL_TWEETS.csv')

MFDpercs = pd.DataFrame(columns = ['care.virtue','care.vice','fairness.virtue','fairness.vice',
                        'loyalty.virtue','loyalty.vice','authority.virtue','authority.vice',
                        'sanctity.virtue','sanctity.vice'])

pbar = ProgressBar()
tweet_tokens = [word_tokenize(t.lower()) for t in pbar(tweets['tweet'])]

    
pbar2 = ProgressBar()        
for block in pbar2(tweet_tokens):
    cts = Counter(block)
    overlap = set(cts) & set(MFDdict['word'])
    subcts = dict((k, cts[k]) for k in overlap)

    cts_df = pd.DataFrame(columns = ['domain','count'])
    for word in subcts:
        wordcat = MFDdict[MFDdict['word'] == word]['domain']
        for w in wordcat:
            tmp = pd.DataFrame({'domain':w,'count':subcts[word]}, index = [0])
            cts_df = pd.concat([cts_df,tmp]).reset_index(drop = True)
    
    if len(overlap) == 0: # stupid bug fix
        cts_df = pd.DataFrame({'domain':'fairness.virtue','count':0}, index = [0])
        
    sum_cts_df = cts_df.groupby(by = 'domain').sum(numeric_only = False)
    sum_cts_df['percent'] = (sum_cts_df['count']/len(block))*100

    sum_cts_df = sum_cts_df.drop(columns = ['count']).transpose()

    MFDpercs = pd.concat([MFDpercs,sum_cts_df])

MFDpercs = MFDpercs.reset_index(drop = True)
MFDpercs = MFDpercs.fillna(0)
tweets = pd.concat([tweets[['date','PublicFigure']], MFDpercs], axis = 1)

tweets.to_csv(homedir + 'data/MFD_scores.csv', index = False)