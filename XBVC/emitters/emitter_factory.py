from XBVC.emitters._emitter_registry import emitter_registry as er

class EmitterFactory(object):
    def __init__(self):
        pass

    def get_emitter(self, lang):
        """
        Returns an Emitter instance for the language specified
        @param string lang target programming language
        """
        if lang.lower() in er.keys():
            return er[lang.lower()]()
        else:
            raise TypeError()

    @property
    def supported_languages(self):
        return er.keys()
        
