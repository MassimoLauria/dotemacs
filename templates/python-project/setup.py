#!/usr/bin/env python
"""
Setup script for the __PROJECT-NAME__ package
"""

from setuptools import setup

setup(name='__PROJECT-NAME__',
      version='0.0.0.1',
      description='__DESCRIPTION__',
      author='__USER-NAME__',
      author_email='__USER-MAIL-ADDRESS__',
      # url='',
      packages=['__PROJECT-NAME__'],
      license='__LICENSE-FILE-NAME__',
      install_requires=[],
      # make some tests
      test_suite='nose.collector',
      tests_require=['nose']
)
