#!/usr/bin/env python

from setuptools import setup
import version
from msgcodegen import build_messages, clean_messages

setup(name='riak_pb',
      version=version.get_version(),
      description='Riak Protocol Buffers Messages',
      packages=['riak_pb'],
      requires=['protobuf(>=2.4.1,<2.6.0)'],
      install_requires=['protobuf >=2.4.1, <2.6.0'],
      options={'easy_install': {'allow_hosts': 'pypi.python.org'}},
      license='Apache 2',
      platforms='Platform Independent',
      author='Basho Technologies',
      author_email='clients@basho.com',
      url='https://github.com/basho/riak_pb',
      zip_safe=True,
      cmdclass={'build_messages': build_messages,
                'clean_messages': clean_messages},
      classifiers=['License :: OSI Approved :: Apache Software License',
                   'Intended Audience :: Developers',
                   'Operating System :: OS Independent',
                   'Topic :: Database']
      )
