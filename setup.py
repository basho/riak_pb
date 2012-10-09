#!/usr/bin/env python

from setuptools import setup
from distutils.spawn import find_executable
from distutils.command.build import build
from distutils.command.clean import clean
from distutils.cmd import Command
from distutils import log
import os


class clean_proto(clean):
    def run(self):
        clean.run(self)
        for package in self.distribution.packages:
            for root, dirs, files in os.walk(package):
                for filename in files:
                    path = os.path.join(root, filename)
                    if self.is_pb2_file(filename):
                        log.info("removing generated file '%s'" % path)
                        os.remove(path)

    def is_pb2_file(self, filename):
        base, u, rest = filename.rpartition('_')
        return rest == 'pb2.py'


class build_proto(Command):
    description = "build .proto files into Python modules"
    user_options = [
        ('protoc=', None, 'location of the protoc compiler'),
        ('indir=', 'i', 'where to find the *.proto files to compile'),
        ('outdir=', 'o', 'where to output the generated Python code'),
        ('force', 'f', 'forcibly build everything (ignore file timestamps)')]

    boolean_options = ['force']

    def initialize_options(self):
        self.indir = None
        self.outdir = None
        self.protoc = None
        self.force = None

    def finalize_options(self):
        self.set_undefined_options('build', ('force', 'force'))

        if self.indir is None:
            self.indir = os.path.join('.', 'src')
        if self.outdir is None:
            self.outdir = os.path.join('.', self.distribution.packages[0])

        if self.protoc is None:
            self.protoc = find_executable('protoc')
        if self.protoc is None:
            raise RuntimeError, "No protoc compiler was found!"

    def run(self):
        for protofile in self.get_proto_files():
            outputfile = self.pb2_filename(protofile)
            outputdir = os.path.dirname(outputfile)
            self.make_file(protofile, outputfile, self.generate_proto, [protofile, outputdir])
        self.reinitialize_command('build_py')

    def get_proto_files(self):
        protos = []
        for root, dirs, files in os.walk(self.indir):
            for filename in files:
                if self.is_proto_file(filename):
                    self.debug_print("Found protobuffs definition: %s" % filename)
                    protos.append(os.path.join(root, filename))
        return protos

    def is_proto_file(self, filename):
        base, dot, ext = filename.rpartition(".")
        return (ext == "proto")

    def pb2_filename(self, source):
        return source.replace(".proto", "_pb2.py").replace(self.indir, self.outdir)

      # Lifted and modified from the google project
    def generate_proto(self, source, outputdir):
        """Invokes the Protocol Compiler to generate a _pb2.py from the given
        .proto file.."""

        protoc_command = [self.protoc,
                           "-I%s" % self.indir,
                           "--python_out=%s" % outputdir,
                           source]
        result = self.spawn(protoc_command, 0)
        if result is not None and result[1] is not 0:
            raise SystemError, "protoc command failed: '%s'" % protoc_command.join(' ')

# Inject our .proto compiler into the front of the build commands
build.sub_commands.insert(0, ('build_proto', None))


# Get the riak_pb app version
#appfile = open('src/riak_pb.app.src').read()
#version = re.search(r"vsn.*\"([^\"]+)\"", appfile).group(1)
version = "1.2.0"

setup(name='riak_pb',
      version=version,
      description='Riak Protocol Buffers Messages',
      packages=['riak_pb'],
      requires=['protobuf(==2.4.1)'],
      license='Apache 2',
      platforms='Platform Independent',
      author='Basho Technologies',
      author_email='clients@basho.com',
      url='https://github.com/basho/riak_pb',
      zip_safe=True,
      classifiers=['License :: OSI Approved :: Apache Software License',
                   'Intended Audience :: Developers',
                   'Operating System :: OS Independent',
                   'Topic :: Database'],
      cmdclass={'build_proto': build_proto,
                'clean': clean_proto}
      )
