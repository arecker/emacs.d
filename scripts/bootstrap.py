#!./venv/bin/python
import argparse
import logging
import pathlib
import sys

parser = argparse.ArgumentParser()
parser.add_argument('--verbose', action='store_true', default=False)
parser.add_argument('--mode', required=True, type=str, choices=[
    'prod',
    'dev',
])


def read_gitignore(root: pathlib.Path) -> list[pathlib.Path]:
    with (root / '.gitignore').open('r') as f:
        for line in f.readlines():
            yield (root / line.strip()).absolute()


def main(args):
    root = pathlib.Path('.').parent
    makefile = (root / 'Makefile')
    assert makefile.is_file(), f'cannot find Makefile in {makefile.parent}, are you running in project root?'
    
    gitignore_paths = [p for p in read_gitignore(root)]
    logging.debug('gitignore_paths = %s', gitignore_paths)

if __name__ == '__main__':
    args = parser.parse_args()
    if args.verbose:
        level = logging.DEBUG
    else:
        level = logging.INFO
    logging.basicConfig(stream=sys.stderr, format='bootstrap.py: %(message)s', level=level)
    logging.debug('parsed args = %s', args)
    main(args=args)
