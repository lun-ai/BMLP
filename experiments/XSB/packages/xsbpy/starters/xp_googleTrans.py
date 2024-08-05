
# Written by Albert Ki.

from googletrans import Translator

LIMIT_EXCEEDED = False
translator = Translator(raise_exception=True)

def hello_world():
    text = 'Bonjour le monde.'
    print(text)
    translation = translate(text,'fr')
    return(translation)

def hello_world2():
    text = 'Bonjour le monde.'
    print(text)
    lang, confidence = detect(text)
    return(lang,confidence)

def translate(text,source,dest='en'):  
    global LIMIT_EXCEEDED
    if not LIMIT_EXCEEDED:
        try:
            translation = translator.translate(text, src=source,dest=dest)
            return(translation.text)
        except Exception as err:
            print('Exceeded continuous googletrans request limit, pausing googletrans...')
            LIMIT_EXCEEDED = True
            return
    else:
        return
        
def detect(text):
    language = translator.detect(text)
    return(language.lang, language.confidence)
