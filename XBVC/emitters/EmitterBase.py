import os.path
from jinja2 import Environment, FileSystemLoader
import os
import os.path

class SourceFile:
    def __init__(self, name, content):
        self.content = content
        self.name = name

    def save_to_disk(self, path):
        with open(os.path.join(path, self.name), 'w') as f:
            f.write(self.content)

    def __str__(self):
        return self.content

# Base emitter class, doesn't do much, just provides stubs for child
# classes and provides a convenient way to expand a template
class EmitterBase:
    def __init__(self, language):
        self.language = language

    def generate_source(self, commspec, targets):
        pass

    def expand_template(self, template_name, template_vars={}):
        base_path = os.path.dirname(__file__)
        tmpl_path = os.path.abspath(os.path.join(base_path, 'templates'))
        env = Environment(loader=FileSystemLoader(tmpl_path))

        env.trim_blocks = True
        env.lstrip_blocks = True

        tmpl = env.get_template(template_name)
        return tmpl.render(template_vars)

