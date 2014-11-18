#!/usr/bin/env python

from setuptools import setup
import version
import platform
from msgcodegen import build_messages, clean_messages
install_requires = ["six >= 1.8.0"]
requires = ["six(>=1.8.0)"]
if platform.python_version() < '3.0':
    name = 'riak_pb'
    requires.append('protobuf(>=2.4.1,<2.7.0)')
    install_requires.append('protobuf >=2.4.1, <2.7.0')
else:
    name = 'python3_riak_pb'
    requires.append('python3_protobuf(>=2.4.1,<2.6.0)')
    install_requires.append('python3_protobuf >=2.4.1, <2.6.0')

setup(name=name,
      version=version.get_version(),
      description='Riak Protocol Buffers Messages',
      packages=['riak_pb'],
      requires=requires,
      install_requires=install_requires,
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
                   'Programming Language :: Python :: 2.6',
                   'Programming Language :: Python :: 2.7',
                   'Programming Language :: Python :: 3.3',
                   'Programming Language :: Python :: 3.4',
                   'Topic :: Database']
      )
