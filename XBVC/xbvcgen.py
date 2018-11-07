#!/usr/bin/python

from XBVC.objects import CommSpec

import argparse
import os
import shutil
import sys
import pkgutil

_dir_path = os.path.dirname(os.path.realpath(__file__))
EMITTER_DIR = os.path.join(_dir_path, 'emitters')


# Adapted from https://stackoverflow.com/questions/1057431
def _load_emitters():
    result = {}
    for importer, package_name, _ in pkgutil.iter_modules([EMITTER_DIR]):
        if package_name not in sys.modules:
            module = (importer.find_module(package_name)
                      .load_module(package_name))
            if hasattr(module, 'EMITTER_NAME'):
                print("Loading emitter: {}".format(module.__name__))
                em_name = getattr(module, 'EMITTER_NAME')
                result[em_name] = module
    return result

EMITTER_MODULES = _load_emitters()


def _handle_args():
    parser = argparse.ArgumentParser()
    parser.add_argument("--input", "-i",
                        required=True,
                        help="XBVC Definition file")
    parser.add_argument("--output", "-o",
                        help="Output Directory, or stdout to print to screen")
    parser.add_argument("--targets", '-t', action='append', dest='targets',
                        required=True,
                        default=[], help="Autogen targets")
    parser.add_argument("--lang", '-l', action='append', dest='languages',
                        default=[], help='Target Languages')
    parser.add_argument("--clean", '-c', action='store_true', default=False,
                        help="Removes output directory before writing")
    return parser.parse_args()


def main():
    args = _handle_args()

    comspec = CommSpec(args.input)

    emitters = []
    if not args.languages:
        print("No languages specified\n"
              "The following languages are supported:")
        for lang in EMITTER_MODULES:
            print('-{}'.format(lang))
        exit(0)

    emitters = []
    for lang in args.languages:
        if lang.lower() not in EMITTER_MODULES:
            print("No emitter found for language: {}!".format(lang))
            return
        emitters.append(EMITTER_MODULES[lang.lower()].Emitter())

    if not args.output:
        exit(0)

    if args.output == 'stdout':
        for emt in emitters:
            for fil in emt.generate_source(comspec, args.targets):
                print(fil)
    else:
        if args.clean and os.path.isdir(args.output):
            shutil.rmtree(args.output)

        try:
            os.makedirs(args.output)
        except:
            pass

        for emt in emitters:
            src_path = os.path.join(args.output, emt.language)
            try:
                os.makedirs(src_path)
            except:
                pass
            for fil in emt.generate_source(comspec, args.targets):
                fil.save_to_disk(src_path)

if __name__ == '__main__':
    main()
