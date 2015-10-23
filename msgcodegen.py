# Copyright 2014 Basho Technologies, Inc.
#
# This file is provided to you under the Apache License,
# Version 2.0 (the "License"); you may not use this file
# except in compliance with the License.  You may obtain
# a copy of the License at
#
#  http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied.  See the License for the
# specific language governing permissions and limitations
# under the License.
#
# NOTE: TO RUN THIS SUCCESSFULLY, YOU NEED TO HAVE THESE
#       PACKAGES INSTALLED:
#       protobuf or python3_protobuf
#       six
#
#       Run the following command to install them:
#       python setup.py install
#
# TO DEBUG: Set DISTUTILS_DEBUG=1 in the environment or run as 'python setup.py -vv build_messages'
#

"""
distutils commands for generating protocol message-code mappings.
"""

__all__ = ['build_messages', 'clean_messages']

import re
import csv
import os
from os.path import isfile
from distutils import log
from distutils.core import Command
from distutils.file_util import write_file
from datetime import date

LICENSE = """# Copyright {0} Basho Technologies, Inc.
#
# This file is provided to you under the Apache License,
# Version 2.0 (the "License"); you may not use this file
# except in compliance with the License.  You may obtain
# a copy of the License at
#
#  http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied.  See the License for the
# specific language governing permissions and limitations
# under the License.
""".format(date.today().year)


class ComparableMixin(object):
    def _compare(self, other, method):
        try:
            return method(self._cmpkey(), other._cmpkey())
        except (AttributeError, TypeError):
            # _cmpkey not implemented, or return different type,
            # so I can't compare with "other".
            return NotImplemented

    def __lt__(self, other):
        return self._compare(other, lambda s, o: s < o)

    def __le__(self, other):
        return self._compare(other, lambda s, o: s <= o)

    def __eq__(self, other):
        return self._compare(other, lambda s, o: s == o)

    def __ge__(self, other):
        return self._compare(other, lambda s, o: s >= o)

    def __gt__(self, other):
        return self._compare(other, lambda s, o: s > o)

    def __ne__(self, other):
        return self._compare(other, lambda s, o: s != o)


class MessageCodeMapping(ComparableMixin):
    def __init__(self, code, message, proto):
        self.code = int(code)
        self.message = message
        self.proto = proto
        self.message_code_name = self._message_code_name()
        self.module_name = 'riak_pb.{0}_pb2'.format(self.proto)
        self.message_class = self._message_class()

    def _cmpkey(self):
        return self.code

    def __hash__(self):
        return self.code

    def _message_code_name(self):
        strip_rpb = re.sub(r"^Rpb", "", self.message)
        word = re.sub(r"([A-Z]+)([A-Z][a-z])", r'\1_\2', strip_rpb)
        word = re.sub(r"([a-z\d])([A-Z])", r'\1_\2', word)
        word = word.replace("-", "_")
        return "MSG_CODE_" + word.upper()

    def _message_class(self):
        try:
            pbmod = __import__(self.module_name, globals(), locals(),
                               [self.message])
            klass = pbmod.__dict__[self.message]
            return klass
        except KeyError:
            log.warn("Did not find '%s' message class in module '%s'",
                      self.message, self.module_name)
        except ImportError as e:
            log.error("Could not import module '%s', exception: %s", self.module_name, e)
            raise
        return None


class clean_messages(Command):
    """
    Cleans generated message code mappings. Add to the build process
    using::

        setup(cmd_class={'clean_messages': clean_messages})
    """

    description = "clean generated protocol message code mappings"

    user_options = [
        ('destination=', None, 'destination Python source file')
    ]

    def initialize_options(self):
        self.destination = None

    def finalize_options(self):
        self.set_undefined_options('build_messages',
                                   ('destination', 'destination'))

    def run(self):
        if isfile(self.destination):
            self.execute(os.remove, [self.destination],
                         msg="removing {0}".format(self.destination))


class build_messages(Command):
    """
    Generates message code mappings. Add to the build process using::

        setup(cmd_class={'build_messages': build_messages})
    """

    description = "generate protocol message code mappings"

    user_options = [
        ('source=', None, 'source CSV file containing message code mappings'),
        ('destination=', None, 'destination Python source file')
    ]

    # Used in loading and generating
    _pb_imports = set()
    _messages = set()
    _linesep = os.linesep
    _indented_item_sep = ',{0}    '.format(_linesep)

    _docstring = [
        ''
        '# This is a generated file. DO NOT EDIT.',
        '',
        '"""',
        'Constants and mappings between Riak protocol codes and messages.',
        '"""',
        ''
    ]

    def initialize_options(self):
        self.source = None
        self.destination = None
        self.update_import = None

    def finalize_options(self):
        if self.source is None:
            self.source = 'src/riak_pb_messages.csv'
        if self.destination is None:
            self.destination = 'riak_pb/messages.py'

    def run(self):
        self.make_file(self.source, self.destination,
                       self._load_and_generate, [])

    def _load_and_generate(self):
        self._format_python2_or_3()
        self._load()
        self._generate()

    def _load(self):
        with open(self.source, 'r', buffering=1) as csvfile:
            reader = csv.reader(csvfile)
            for row in reader:
                message = MessageCodeMapping(*row)
                self._messages.add(message)
                self._pb_imports.add(message.module_name)

    def _generate(self):
        self._contents = []
        self._generate_doc()
        self._generate_imports()
        self._generate_codes()
        self._generate_classes()
        write_file(self.destination, self._contents)

    def _generate_doc(self):
        # Write the license and docstring header
        self._contents.append(LICENSE)
        self._contents.extend(self._docstring)

    def _generate_imports(self):
        # Write imports
        for im in sorted(self._pb_imports):
            self._contents.append("import {0}".format(im))

    def _generate_codes(self):
        # Write protocol code constants
        self._contents.extend(['', "# Protocol codes"])
        for message in sorted(self._messages):
            self._contents.append("{0} = {1}".format(message.message_code_name,
                                                     message.code))

    def _generate_classes(self):
        # Write message classes
        classes = [self._generate_mapping(message)
                   for message in sorted(self._messages)]

        classes = self._indented_item_sep.join(classes)
        self._contents.extend(['',
                               "# Mapping from code to protobuf class",
                               'MESSAGE_CLASSES = {',
                               '    ' + classes,
                               '}'])

    def _generate_mapping(self, m):
        if m.message_class is not None:
            klass = "{0}.{1}".format(m.module_name,
                                     m.message_class.__name__)
        else:
            klass = "None"
        pair = "{0}: {1}".format(m.message_code_name, klass)
        if len(pair) > 76:
            # Try to satisfy PEP8, lulz
            pair = (self._linesep + '    ').join(pair.split(' '))
        return pair

    def _format_python2_or_3(self):
        """
        Change the PB files to use full pathnames for Python 3.x
        and modify the metaclasses to be version agnostic
        """
        pb_files = set()
        with open(self.source, 'r', buffering=1) as csvfile:
            reader = csv.reader(csvfile)
            for row in reader:
                _, _, proto = row
                pb_files.add('riak_pb/{0}_pb2.py'.format(proto))

        for im in sorted(pb_files):
            with open(im, 'r', buffering=1) as pbfile:
                contents = 'from six import *\n' + pbfile.read()
                contents = re.sub(r'riak_pb2',
                                  r'riak_pb.riak_pb2',
                                  contents)
            # Look for this pattern in the protoc-generated file:
            #
            # class RpbCounterGetResp(_message.Message):
            #    __metaclass__ = _reflection.GeneratedProtocolMessageType
            #
            # and convert it to:
            #
            # @add_metaclass(_reflection.GeneratedProtocolMessageType)
            # class RpbCounterGetResp(_message.Message):
            contents = re.sub(
                r'class\s+(\S+)\((\S+)\):\s*\n'
                '\s+__metaclass__\s+=\s+(\S+)\s*\n',
                r'@add_metaclass(\3)\nclass \1(\2):\n', contents)

            with open(im, 'w', buffering=1) as pbfile:
                pbfile.write(contents)
