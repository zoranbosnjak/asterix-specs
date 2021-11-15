
from setuptools import setup, find_packages
setup(
    name = 'json-to-rst',
    version = open('VERSION.txt').read().strip(),
    packages = find_packages(),
    scripts = ['json-to-rst']
)

