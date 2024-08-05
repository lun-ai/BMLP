
import unicodedata

def unicode_lower(Upper):
    return Upper.lower()

def unicode_upper(Lower):
    return Lower.upper()

def unicode_category(Char):
    return unicodedata.category(Char)
    
