
from setuptools import setup, find_packages
setup(
    name = 'asterix-renderer',
    version = open('VERSION.txt').read().strip(),
    packages = find_packages(),
    scripts = ['bin/render']
)

