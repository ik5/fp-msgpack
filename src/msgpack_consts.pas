{ Basic constant values of msgpack

  Copyright (c) 2012 Ido Kanner <idokan@gmail.com>

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to
  deal in the Software without restriction, including without limitation the
  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
  sell copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
  IN THE SOFTWARE.
}
unit MsgPack_Consts;

{$mode objfpc}{$H+}

interface

// Notations for information
const
  notPosIntMin   = $00; // Starting range of Positive Integer from 0
  notPosIntMax   = $7f; // Ending range of positive Integer     to 127
  notFixMapMin   = $80; // Starting range of FixMap
  notFixMapMax   = $8f; // Ending range of FixMap
  notFixArrayMin = $90; // Starting range of FixArray
  notFixArrayMax = $9f; // Ending range of FixArray
  notFixRawMin   = $a0; // Starting range of FixRaw
  notFixRawMax   = $bf; // Ending range of FixRaw
  notNil         = $c0; // Nil notation
  notFalse       = $c2; // False notation
  notTrue        = $c3; // True notation
  notFloat       = $ca; // Floting point notation
  notDouble      = $cb; // Double precision notation
  notUInt8       = $cc; // Unsgined integer  8 bit
  notUInt16      = $cd; // Unsigned integer 16 bit
  notUInt32      = $ce; // Unsigned integer 32 bit
  notUInt64      = $cf; // Unsigned integer 64 bit
  notInt8        = $d0; // Signed integer  8 bit
  notInt16       = $d1; // Signed integer 16 bit
  notInt32       = $d2; // Signed integer 32 bit
  notInt64       = $d3; // Signed integer 64 bit
  notRaw16       = $da; // Raw bytes 16 bit
  notRaw32       = $db; // Raw bytes 32 bit
  notArray16     = $dc; // Array length of 16 bit
  notArray32     = $dd; // Array length of 32 bit
  notMap16       = $de; // Map 16 bit
  notMap32       = $df; // Map 32 bit
  notNegIntMin   = $e0; // Starting range of Negative Integer   to -32
  notNegIntMax   = $ff; // Ending range of Negative Integer   from -1

type
  // Enum that explains what is the given data type
  TMsgPackDataTypes = (
                mpdtNil,     // Contain nil data
                mpdtBoolean, // Contain true or false data
                mpdtNumber,  // Number type (Integer or floating point)
                mpdtRaw,     // Contain Non numeric char
                mpdtArray,   // Contain array
                mpdtMap      // Contain map of key value
               );

  // Enum for the sub type of the data
  TMsgPackSubTypes = (
    mpstInt8,       // Signed integer  8 bit
    mpstInt16,      // Signed integer 16 bit
    mpstInt32,      // Signed integer 32 bit
    mpstInt64,      // Signed integer 64 bit
    mpstUInt8,      // Unsgined integer  8 bit
    mpstUInt16,     // Unsigned integer 16 bit
    mpstUInt32,     // Unsigned integer 32 bit
    mpstUInt64,     // Unsigned integer 64 bit
    mpstFloat,      // Single floating point
    mpstDouble,     // Double floating point
    mpstFixedRaw,   // Chars from 0..31
    mpstRaw16,      // Raw bytes 16 bit - String
    mpstRaw32,      // Raw bytes 32 bit
    mpstArrayFixed, // Array Fixed Length
    mpstArray16,    // Array 16 bit length
    mpstArray32,    // Array 32 bit length
    mpstMap16,      // Map 16 bit
    mpstMap32,      // Map 32 bit
    mpstTrue,       // Boolean True
    mpstFalse,      // Boolean False
    mpstNil,        // Nil value
    mpstUnknown     // Unknow type
  );

implementation

end.

