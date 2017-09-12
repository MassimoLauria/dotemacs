#!/usr/bin/env python
"""
Setup script for the __PROJECT-NAME__ package
"""

from setuptools import setup, find_packages

import __PROJECT-NAME__.prjdata as p

setup(
    name         = p.__project_name__,
    version      = p.__version__,
    description  = p.__project_description__,
    author       = p.__author__,
    author_email = p.__author_email__,
    url          = p.__url__,
    license      = p.__license__,
    packages     = find_packages(".", exclude=["tests"]),
    entry_points={
        'console_scripts': [
        ],
    },
    install_requires=[],
    # make some tests
    test_suite='nose.collector',
    tests_require=['nose']
)
