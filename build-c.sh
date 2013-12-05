#!/bin/sh
# Build Protocol Buffer C headers and source from proto files
#
# Copyright (c) 2007-2013 Basho Technologies, Inc.  All Rights Reserved.
#
# This file is provided to you under the Apache License,
# Version 2.0 (the "License"); you may not use this file
# except in compliance with the License.  You may obtain
# a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied.  See the License for the
# specific language governing permissions and limitations
# under the License.

CMD=protoc-c
# Test for protoc-c
which ${CMD} > /dev/null 2>&1
[ $? -eq 0 ] || { echo "Required command ${CMD} not found"; exit 1; }

# Make each .c and .h file
mkdir -p c
for p in `ls src/*.proto`
do
    echo "Building $p"
    ${CMD} -Isrc $p --c_out=c
done
