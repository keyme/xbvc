#!/usr/bin/python

from XBVC.objects import CommSpec
from XBVC.emitters import emitter_factory
import argparse
import os
import shutil


def main():
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
    args = parser.parse_args()

    comspec = CommSpec(args.input)

    ef = emitter_factory.EmitterFactory()

    emitters = []
    if not args.languages:
        print("No languages specified")
        print("The following languages are supported:")
        for lang in ef.supported_languages:
            print('-{}'.format(lang))
        exit(0)

    for lang in args.languages:
        try:
            emitters.append(ef.get_emitter(lang))
        except:
            print("No emitter found for language: {}".format(lang))

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
