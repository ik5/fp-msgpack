Introduction
============
[MessagePack](http://msgpack.org/) saves type-information to the serialized data. Thus each data is
stored in **type-data** or **type-length-data** style.

MessagePack supports following types:

   * Fixed length types
       - Integers
       - nil
       - boolean
       - Floating point
   * Variable length types
       - Raw bytes
   * Container types
       - Arrays
       - Maps

Each type has one or more serialize format:

   * Fixed length types
       - Integers
           + positive fixnum
           + negative fixnum
           + uint 8
           + uint 16
           + uint 32
           + uint 64
           + int 8
           + int 16
           + int 32
           + int 64
       - Nil
           + nil
       - Boolean
           + true
           + false
       - Floating point
           + float
           + double
   * Variable length types
       - Raw bytes
           + fix raw
           + raw 16
           + raw 32
   * Container types
       - Arrays
           + fix array
           + array 16
           + array 32
       - Maps
           + fix map
           + map 16
           + map 32

Current Implementation
======================
The following code provides pure Object Pascal implementation of msgpack
according to [Format Specification](http://wiki.msgpack.org/display/MSGPACK/Format+specification) from Jun 06 2011.

Files
-----
 * src/
   - msgpack.pas - basic implementation for packing and unpacking data

 * tests/
   - convert.lpr - program that uses fpcunit
   - convert\_testcase.pas - unit test for msgpack.pas

 * examples/

 * docs/

 * license - The following library is using MIT license
 * README.md - this file
