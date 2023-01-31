#!/usr/bin/env python3

import re
import sys
import string
import pandas as pd 
from datetime import datetime as dt
from progressbar import ProgressBar
from sklearn.model_selection import train_test_split
from simpletransformers.classification import ClassificationModel, ClassificationArgs

import logging
logging.basicConfig(level=logging.INFO)
transformers_logger = logging.getLogger("transformers")
transformers_logger.setLevel(logging.WARNING)

import nltk
from nltk import pos_tag
from nltk.stem.wordnet import WordNetLemmatizer
from nltk.stem.snowball import SnowballStemmer
from nltk.tokenize import TweetTokenizer
from nltk.tokenize import word_tokenize
from nltk.corpus import wordnet, stopwords

import os
os.environ["TOKENIZERS_PARALLELISM"] = "false"

# functions from William Brady
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

output_dir = str(dt.date(dt.now())) + '_outputs_positivity'
homedir = "/home/bms2202/newmetoo/"

# from William Brady
top_emojis = ['😂','🤣','😡','🖕','😹','🙏','👎','🌊','🙄','🤔']
lemmatizer = WordNetLemmatizer()
cachedStopWordsPunctuation = set(stopwords.words("english")
                                 + [x for x in list(string.punctuation) if x not in ['!','?']]
                                 + ['',' ','  '])

# set up train and test data
if len(sys.argv) == 1:
	file = pd.read_csv(homedir + 'data/st_training_data.csv', 
		header = None, encoding='latin-1')
	file[0] = file[0].replace({4:1})
	file  = pd.concat([file[file[0] == 0].sample(frac = .1, random_state = 1),
		file[file[0] == 1].sample(frac = .1, random_state = 1)]).reset_index(drop = True)
	#file = file.loc[0:1000,:] # testing purposes

	pbar = ProgressBar()
	file_new = pd.DataFrame({'text':file[5],
	                        'labels':file[0]})

#	file1 = pd.read_csv(homedir + 'simpletransformers/train_test_sets/TrainingData.csv')
#	file2 = pd.read_csv(homedir + 'simpletransformers/train_test_sets/TestingData.csv')
#	file = pd.concat([file1,file2]).reset_index(drop = True)
#	file_new = pd.DataFrame({'text':file['tweet'],
#		'labels':file['label'] + 1})

#	file_new = file_new.groupby('labels')
#	file_new = pd.DataFrame(file_new.apply(lambda x: x.sample(file_new.size().min()))).reset_index(drop=True)

#	pbar = ProgressBar()
	file_new['text'] = [tweet_process(text) for text in pbar(file_new['text'])] # testing purposes

	train,test = train_test_split(file_new,test_size = .2)
    
	model_args = ClassificationArgs(output_dir = homedir + 'simpletransformers/' + output_dir,
                                    overwrite_output_dir = True,
                                    num_train_epochs = 3,
                                    use_multiprocessing=False,
                                    use_multiprocessing_for_evaluation=False)

	model = ClassificationModel(model_type = "distilbert",
	                            model_name = "distilbert-base-uncased",
	                           use_cuda = False,
	                            num_labels = 2,
	                           args = model_args)

	# train model
	model.train_model(train)

	# evaluate model performance
	result, model_outputs, wrong_predictions = model.eval_model(test, verbose = True)
else:
	model = ClassificationModel(model_type = "distilbert",
	                            model_name = homedir + "simpletransformers/" + str(sysargv[1]),
	                           use_cuda = False,
	                            num_labels = 2)


# predict on new data
predict_df = pd.read_csv(homedir + 'data/ALL_TWEETS_indiv.csv', lineterminator='\n', low_memory = False)
predict_df = predict_df.dropna(subset = ['tweet_clean'])

predictions, raw_outputs = model.predict(predict_df['tweet_clean'].tolist())


predict_df['liking_st'] = predictions
predict_df.to_csv(homedir + 'data/ALL_TWEETS_indiv.csv', index = False)