
#written by Albert Ki/Swift

import fasttext as ft
import numpy as np
from scipy.spatial.distance import cosine
from sklearn.metrics import pairwise_distances

# ----------------- lid176 -----------------
def fasttext_predict(model,sentences): 
    predictions = model.predict(sentences)
    lang = predictions[0][0]
    confidence = predictions[1][0]
    return((lang,confidence))

def hello_world(binpath):
    sentence = ['Bonjour le monde']
    print(sentence[0])
    model = load_model(binpath)
    predictions = model.predict(sentence)
    lang = predictions[0][0][0]
    confidence = predictions[1][0][0]
    return(lang,confidence)

# Load and predict a the language of a string
def predict2(binpath, sentences):
    model = ft.load_model(binpath)
    print(type(model))
    predictions = model.predict(sentences)
    lang = predictions[0][0][0]
    confidence = predictions[1][0][0]
    return((lang,confidence))

# ----------------- fasttext models -----------------
# For binary models.  For textual models use xp_faiss

model_dict = {}

def load_model(binpath,name):  
    print('Loading training model',binpath)
    model = ft.load_model(binpath)
    print(type(model))
    model_dict[name] = model
    return True

def get_word_vec(Name,X):
    model = model_dict[Name]
    Vec = model.get_word_vector(X)
    return Vec.tolist()

#mymod = ft.load_model('/home/tswift/fasttext/cc.en.300.bin')

def cosine_similarity(name,wordlist):
    mymod = model_dict[name]
    veclist = []
    for elt in wordlist:
        wordvec = mymod.get_word_vector(elt)
        veclist.append(wordvec)
    np_veclist = np.array(veclist)
    distMat = 1-pairwise_distances(np_veclist, metric="cosine")
    return distMat.tolist()

def get_nearest_neighbors(name,word):
    mymod = model_dict[name]
    return mymod.get_nearest_neighbors(word)
