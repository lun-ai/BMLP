
import xmltodict

def get_as_dict(File):
    print(File)
    with open(File,'r') as fileptr:
        mystr = fileptr.read()
        return xmltodict.parse(mystr)
