import spacy
                                                                                
#nlp=  spacy.load("en_core_web_lg")
#doc = nlp('')                     

doc_dict = dict()

def load_model(Model):
    nlp = spacy.load(Model)
    doc_dict[Model] = nlp

def add_pipe(Model,Pipe):
    nlp = doc_dict[Model]
    nlp.add_pipe(Pipe)

def proc_string(Model,String):
    try:
        nlp = doc_dict[Model]
        return(nlp(String))
    except:
        try:
            load_model(Model)
            nlp = doc_dict[Model]
            return(nlp(String))
        except:
            raise NameError(Model)

def doc_from_file(Model,File):
    try:
        nlp = doc_dict[Model]
        with open(File,"r") as fp: 
            file_text = fp.read()                                                   
            return(nlp(file_text))                                                  
    except:
        try:
            load_model(Model)
            nlp = doc_dict[Model]
            with open(File,"r") as fp: 
                file_text = fp.read()                                                   
                return(nlp(file_text))                                                  
        except:
            raise NameError(Model)
                                                                                
def get_nps(Doc):                                                               
    return([chunk.text for chunk in Doc.noun_chunks])                           
def get_ents(Doc):                                                              
    return([(ent.text,ent.label_,ent.start_char,ent.end_char) for ent in Doc.ents])
def get_token_info(Doc):
    return([(token.idx,token.text,token.lemma_,token.pos_,token.tag_,token.dep_, \
             token.ent_type_,[token.idx for token in list(token.children)]) \
            for token in Doc])

def print_token_info(Doc):
    for elt in get_token_info(Doc):
        print(elt)

def make_doc(text):                                                             
    global doc                                                                  
    doc = nlp(text)                                                             
    return(doc)                                                                 
                 
def show_ents(doc):
    spacy.displacy.serve(doc,style='ent')

def show_sents(doc):
    sentence_spans = list(doc.sents)
    spacy.displacy.serve(sentence_spans, style="dep")
