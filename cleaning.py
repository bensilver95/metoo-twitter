#!/usr/bin/env python3

import re
import sys
import string
import pandas as pd 
from glob import glob
from datetime import timedelta
from datetime import datetime as dt
from progressbar import ProgressBar

import nltk
from nltk import pos_tag
from nltk.stem.wordnet import WordNetLemmatizer
from nltk.stem.snowball import SnowballStemmer
from nltk.tokenize import TweetTokenizer
from nltk.tokenize import word_tokenize
from nltk.corpus import wordnet, stopwords

# cleaning functions token_postag to tweet_process adapted from William Brady
# word_tokenize as defined in nltk library
def token_postag(text):
    tokens = word_tokenize(text)
    return pos_tag(tokens)

# function to simplify POS
# exclusively used for lemmatization using WORDNET
def get_wordnet_pos(treebank_tag):
    if treebank_tag.startswith('J'):
        return wordnet.ADJ
    elif treebank_tag.startswith('V'):
        return wordnet.VERB
    elif treebank_tag.startswith('N'):
        return wordnet.NOUN
    elif treebank_tag.startswith('R'):
        return wordnet.ADV
    else:
        return None

# tokenize and then stemmize the string
def token_stem_lemmatize(text):
    tokens_pos = token_postag(text)
    result_string = ''
    for word, tag in tokens_pos:
        wntag = get_wordnet_pos(tag)
        # not return tag in case of None
        if wntag is None:
            result_string += lemmatizer.lemmatize(word.lower())
        else:
            result_string += lemmatizer.lemmatize(word.lower(), pos=wntag)
        result_string += ' '
    return result_string

# remove stop words and short words
def stop_short_process(text):
    text = ' '.join([word for word in text.split() if word not in cachedStopWordsPunctuation])
    text = re.sub("[^a-zA-Z ]+", '', text) # remove apostrophe now
    text = ' '.join(word for word in text.split() if len(word)>2)
    return text

def tweet_process(tweet):
    tweet = re.sub('//t.co\S+', ' ', tweet) # remove link
    tweet = re.sub('http\S+\s*', ' ', tweet)  # remove URLs
    tweet = re.sub('@\S+', ' ', tweet)  # remove mentions
    tweet = re.sub('&amp', ' ', tweet)  # remove mentions
    tweet = re.sub('RT@|RT @', ' ', tweet)  # remove RT
    tweet = re.sub('#\S+', ' ', tweet)  # remove hashtags
    tweet = re.sub('[%s]' % re.escape("""!"#$%&()*+,-./:;<=>?@[\]^_`{|}~"""), ' ', tweet)  # remove punctuations, leave behind apostrophe"'"
    tweet = re.sub('\s+', ' ', tweet)  # remove extra whitespace
    tweet = token_stem_lemmatize(tweet)
    tweet = stop_short_process(tweet)
    return tweet

def small_clean(df):
    df = df[df['language'] == "en"]
    nolinks = []
    for tweet in df["tweet"]:
        nohttp = re.sub(r"http\S+", "", tweet)
        nolinks.append(nohttp)
    df = df.assign(tweet = nolinks)
    return df.drop_duplicates(subset = 'tweet', keep = "first").reset_index(drop = True)

def analysis_prep(df,filename):
    df["date"] = pd.to_datetime(df["date"]).dt.date
    df["datediff"] = (df['date'] - min(df['date'])).dt.days
    df['weekdiff'] = 2
    df.loc[df['datediff'] > 13,'weekdiff'] = 3
    df.loc[df['datediff'] < 7,'weekdiff'] = 1

    if "pre.csv" in filename:
        df["tweet_period"] = "pre"
        df["PublicFigure"] = filename.split("_pre")[0].replace("_"," ")
    elif "post.csv" in filename:
        df["tweet_period"] = "post"
        df["PublicFigure"] = filename.split("_post")[0].replace("_"," ")
    elif "oneyear.csv" in filename:
        df["tweet_period"] = "oneyear"
        df["PublicFigure"] = filename.split("_oneyear")[0].replace("_"," ")

    return df[["id","date","time","user_id","username","tweet","retweets_count","likes_count","hashtags",
    "datediff","weekdiff","tweet_period","PublicFigure"]]

# from William Brady
top_emojis = ['😂','🤣','😡','🖕','😹','🙏','👎','🌊','🙄','🤔']
lemmatizer = WordNetLemmatizer()
cachedStopWordsPunctuation = set(stopwords.words("english")
                                 + [x for x in list(string.punctuation) if x not in ['!','?']]
                                 + ['',' ','  '])

homedir = '/home/bms2202/newmetoo/'
ALL_TWEETS = pd.DataFrame()

pbar = ProgressBar()
for file in pbar(glob(homedir + "tweets/raw/*.csv")):
    filename = file.split("/")[-1]
    df = pd.read_csv(file)
    df = small_clean(df)
    df.to_csv(homedir + "tweets/clean/" + filename)

    dfprep = analysis_prep(df,filename)   
    ALL_TWEETS = pd.concat([ALL_TWEETS,dfprep]).reset_index(drop = True)


## post processing for all tweets
# combine tweets about same people
ALL_TWEETS = ALL_TWEETS.replace({'PublicFigure':{'Louis C.K.':'Louis CK','T.J. Miller':'TJ Miller',
	'Leslie Moonves':'Les Moonves', 'George H.W. Bush':'George HW Bush'}})
pbar3 = ProgressBar()
ALL_TWEETS['tweet_clean'] = [tweet_process(text) for text in pbar3(ALL_TWEETS['tweet'])]
ALL_TWEETS = ALL_TWEETS.dropna(subset = ['tweet_clean'])

print("saving file")
ALL_TWEETS.to_csv(homedir + 'data/ALL_TWEETS_indiv.csv', index = 'False')
# issue with na tweets not being removed
ALL_TWEETS = pd.read_csv(homedir + 'data/ALL_TWEETS_indiv.csv', lineterminator='\n', low_memory = False)
ALL_TWEETS = ALL_TWEETS.dropna(subset = ['tweet_clean'])
ALL_TWEETS.to_csv(homedir + 'data/ALL_TWEETS_indiv.csv', index = 'False')

print('concatenating ALL_TWEETS')
concat_df = pd.DataFrame(columns = ["date","tweet","datediff","weekdiff","tweet_period","PublicFigure","count"])

pbar2 = ProgressBar()
for person in pbar2(ALL_TWEETS['PublicFigure'].unique()):
    mini_df = ALL_TWEETS[ALL_TWEETS['PublicFigure'] == person]
    for timepoint in mini_df['tweet_period'].unique():  
        minier_df = mini_df[mini_df['tweet_period'] == timepoint]
        for d in minier_df['date'].unique():
            miniest_df = minier_df[minier_df['date'] == d]
            tmp = pd.DataFrame({'PublicFigure':person,'date':d,
                                                            'tweet':' '.join(text for text in miniest_df['tweet']),
                                                           'tweet_period':timepoint, 
                                                            'datediff': miniest_df['datediff'].unique()[0],
                                                            'weekdiff': miniest_df['weekdiff'].unique()[0],
                                                            'count': len(miniest_df)}, index = [0])
            concat_df = pd.concat([concat_df,tmp]).reset_index(drop = True)
             
print("saving concat file")
concat_df.to_csv(homedir + "data/ALL_TWEETS.csv", index = False)