## Using Twitter to understand psychological responses to #MeToo

This repository is the analysis pipeline for a project looking at how the perceived morality of public figures changes after being accused of sexual misconduct. All cleaned/prepared data ready for analysis, AND raw data, can be found on the [OSF page](https://osf.io/z6c45/). All scripts run in Python 3.7.6.

### Analysis procedures

1. Run `cleaning.py` to clean and concatenate all of the tweets in the `tweets` folder on OSF.
2. Run `MFD.py` to analyze the moral content of tweets. 
3. Run `afinn_positivity.py` and `st_positivity.py` to calculate how well-liked public figures were, using both a dictionary-based method (AFINN) and a machine learning method (BERT, via simple transformers). Note that simple transformers works best in a conda environment. Set one up using the instructions [here](https://simpletransformers.ai/docs/installation/).
4. Run the "clean" section `analyses.R` to load other associated data and combine it into a single dataframe.
5. You can recreate analyses by running the rest of `analyses.R`, or you may load all models by opening `models.RData`
