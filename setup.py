#!/usr/bin/env python

from setuptools import setup
from proto_cmd import build_proto, clean_proto
import re

# Get the riak_pb app version
appfile = open('src/riak_pb.app.src').read()
version = re.search(r"vsn.*\"([^\"]+)\"", appfile).group(1)

setup(name='riak_pb',
      version = version,
      description='Riak Protocol Buffers Messages',
      packages=['riak_pb'],
      requires=['protobuf(==2.4.1)'],
      license='Apache 2',
      platforms='Platform Independent',
      author='Basho Technologies',
      author_email='clients@basho.com',
      url='https://github.com/basho/riak_pb',
      zip_safe=True,
      classifiers = ['License :: OSI Approved :: Apache Software License',
                     'Intended Audience :: Developers',
                     'Operating System :: OS Independent',
                     'Topic :: Database'],
      cmdclass = { 'build_proto': build_proto,
                   'clean': clean_proto }
      )
