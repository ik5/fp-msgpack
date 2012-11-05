{ Basic msgpack unit test for converter unit

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
unit convert_testcase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, msgpack;

type

  { TConvertTest }

  TConvertTest = class(TTestCase)
  protected
    MsgPackType : TMsgPackType;
    procedure SetUp; override;
  published
    procedure TestNil;
    procedure TestBoolean;
    procedure TestBytes;
    procedure TestWord;
    procedure TestLongWord;
    procedure TestQWord;
    procedure TestShortInt;
    procedure TestSmallInt;
    procedure TestLongInt;
    procedure TestInt64;
    procedure TestSingle;
    procedure TestDouble;
    procedure TestFixedRawByte;
  end;

implementation
uses MsgPack_Consts, TypInfo;

resourcestring
  IsNilError        = 'IsNil function contain wrong value';
  RawDataLenError   = 'RawData does not contain the proper length';
  RawDataWrongType  = 'RawData[0] contain wrong data type';
  BooleanWrongValue = 'Boolean contain wrong value';
  ByteLength        = 'Input of %d have length of %d';
  ByteOutput        = 'Input of %d must be equal to the value with not change';
  BytePrefix        = 'Number Prefix: %2x';
  FloatLength       = 'Input of %f have length of %d';
  FloatOutput       = 'Input of %f must be equal %f';
  WrongDataType     = 'Wrong Data type was given %s';
  WrongSubDataType  = 'Wrong sub Data type was given %s';
  WrongRawLength    = 'Wrong Raw Length. Expected %d, got %d';
  WrongRawValueInt  = 'Wrong Raw Value. Expected %d, got %d';
  WrongRawValueStr  = 'Wrong Raw Value. Expected "%s", got "%s"';

procedure TConvertTest.SetUp;
begin

end;

function DataTypesToString(AType : TMsgPackDataTypes) : String;
begin
  Result := GetEnumName(TypeInfo(TMsgPackDataTypes), Byte(AType));
end;

function SubDataTypeToString(AType : TMsgPackSubTypes) : String;
begin
  Result := GetEnumName(TypeInfo(TMsgPackSubTypes), Byte(AType));
end;

procedure TConvertTest.TestNil;
begin
  MsgPackType := TMsgPackNil.create;

  CheckEquals(True, MsgPackType.IsNil, IsNilError);
  CheckEquals(1, Length(MsgPackType.RawData), RawDataLenError);
  CheckEquals(notNil, MsgPackType.RawData[0], RawDataWrongType);
  AssertEquals(Format(WrongDataType, [DataTypesToString(MsgPackType.MsgType)]),
               Ord(mpdtNil), Ord(MsgPackType.MsgType));
  AssertEquals(Format(WrongSubDataType, [SubDataTypeToString(MsgPackType.SubType)]),
               Ord(mpstNil), Ord(MsgPackType.SubType));

  MsgPackType.Free;
end;

procedure TConvertTest.TestBoolean;
begin
  MsgPackType := TMsgPackBoolean.Create;
  CheckEquals(False, TMsgPackBoolean(MsgPackType).Value, BooleanWrongValue);
  CheckEquals(1, Length(MsgPackType.RawData), RawDataLenError);
  CheckEquals(notFalse, MsgPackType.RawData[0], RawDataWrongType);
  AssertEquals(Format(WrongDataType, [DataTypesToString(MsgPackType.MsgType)]),
               Ord(mpdtBoolean), Ord(MsgPackType.MsgType));
  AssertEquals(Format(WrongSubDataType, [SubDataTypeToString(MsgPackType.SubType)]),
               Ord(mpstFalse), Ord(MsgPackType.SubType));

  TMsgPackBoolean(MsgPackType).Value := True;
  CheckEquals(True, TMsgPackBoolean(MsgPackType).Value, BooleanWrongValue);
  CheckEquals(True, MsgPackType.AsBoolean, BooleanWrongValue);
  CheckEquals(1, Length(MsgPackType.RawData), RawDataLenError);
  CheckEquals(notTrue, MsgPackType.RawData[0], RawDataWrongType);
  AssertEquals(Format(WrongDataType, [DataTypesToString(MsgPackType.MsgType)]),
               Ord(mpdtBoolean), Ord(MsgPackType.MsgType));
  AssertEquals(Format(WrongSubDataType, [SubDataTypeToString(MsgPackType.SubType)]),
               Ord(mpstTrue), Ord(MsgPackType.SubType));

  MsgPackType.Free;
end;

procedure TConvertTest.TestBytes;
var n : Byte;
begin
  MsgPackType := TMsgPackNumber.Create;

  n := 1; // Almost start
  TMsgPackNumber(MsgPackType).Value(n);
  CheckEquals(1, Length(MsgPackType.RawData), Format(ByteLength, [n, 1]));
  CheckEquals(n, MsgPackType.AsByte, Format(ByteOutput, [n]));
  AssertEquals(Format(WrongDataType, [DataTypesToString(MsgPackType.MsgType)]),
               Ord(mpdtNumber), Ord(MsgPackType.MsgType));
  AssertEquals(Format(WrongSubDataType, [SubDataTypeToString(MsgPackType.SubType)]),
               Ord(mpstUInt8), Ord(MsgPackType.SubType));

  n := 127; // Last low byte
  TMsgPackNumber(MsgPackType).Value(n);
  CheckEquals(1, Length(MsgPackType.RawData), Format(ByteLength, [n, 1]));
  CheckEquals(n, MsgPackType.AsByte, Format(ByteOutput, [n]));
  AssertEquals(Format(WrongDataType, [DataTypesToString(MsgPackType.MsgType)]),
               Ord(mpdtNumber), Ord(MsgPackType.MsgType));
  AssertEquals(Format(WrongSubDataType, [SubDataTypeToString(MsgPackType.SubType)]),
               Ord(mpstUInt8), Ord(MsgPackType.SubType));

  n := 128; // Start high byte
  TMsgPackNumber(MsgPackType).Value(n);
  CheckEquals(2, Length(MsgPackType.RawData), Format(ByteLength, [n, 2]));
  CheckEquals(notUInt8, MsgPackType.RawData[0],
              Format(BytePrefix, [MsgPackType.RawData[0]]));
  CheckEquals(n, MsgPackType.AsByte, Format(ByteOutput, [n]));
  AssertEquals(Format(WrongDataType, [DataTypesToString(MsgPackType.MsgType)]),
               Ord(mpdtNumber), Ord(MsgPackType.MsgType));
  AssertEquals(Format(WrongSubDataType, [SubDataTypeToString(MsgPackType.SubType)]),
               Ord(mpstUInt8), Ord(MsgPackType.SubType));

  n := 255; // Last high byte
  TMsgPackNumber(MsgPackType).Value(n);
  CheckEquals(2, Length(MsgPackType.RawData), Format(ByteLength, [n, 2]));
  CheckEquals(notUInt8, MsgPackType.RawData[0],
              Format(BytePrefix, [MsgPackType.RawData[0]]));
  CheckEquals(n, MsgPackType.AsByte, Format(ByteOutput, [n]));
  AssertEquals(Format(WrongDataType, [DataTypesToString(MsgPackType.MsgType)]),
               Ord(mpdtNumber), Ord(MsgPackType.MsgType));
  AssertEquals(Format(WrongSubDataType, [SubDataTypeToString(MsgPackType.SubType)]),
               Ord(mpstUInt8), Ord(MsgPackType.SubType));

  MsgPackType.Free;
end;

procedure TConvertTest.TestWord;
var n : Word;
begin
  MsgPackType := TMsgPackNumber.Create;

  n := 256; // Minimal Value
  TMsgPackNumber(MsgPackType).Value(n);
  CheckEquals(3, Length(MsgPackType.RawData), Format(ByteLength, [n, 3]));
  CheckEquals(notUInt16, MsgPackType.RawData[0],
              Format(BytePrefix, [MsgPackType.RawData[0]]));
  CheckEquals(n, MsgPackType.AsWord, Format(ByteOutput, [n]));
  AssertEquals(Format(WrongDataType, [DataTypesToString(MsgPackType.MsgType)]),
               Ord(mpdtNumber), Ord(MsgPackType.MsgType));
  AssertEquals(Format(WrongSubDataType, [SubDataTypeToString(MsgPackType.SubType)]),
               Ord(mpstUInt16), Ord(MsgPackType.SubType));

  n := 32767; // middle range
  TMsgPackNumber(MsgPackType).Value(n);
  CheckEquals(3, Length(MsgPackType.RawData), Format(ByteLength, [n, 3]));
  CheckEquals(notUInt16, MsgPackType.RawData[0],
              Format(BytePrefix, [MsgPackType.RawData[0]]));
  CheckEquals(n, MsgPackType.AsWord, Format(ByteOutput, [n]));
  AssertEquals(Format(WrongDataType, [DataTypesToString(MsgPackType.MsgType)]),
               Ord(mpdtNumber), Ord(MsgPackType.MsgType));
  AssertEquals(Format(WrongSubDataType, [SubDataTypeToString(MsgPackType.SubType)]),
               Ord(mpstUInt16), Ord(MsgPackType.SubType));

  n := High(Word); // maximal range (65535)
  TMsgPackNumber(MsgPackType).Value(n);
  CheckEquals(3, Length(MsgPackType.RawData), Format(ByteLength, [n, 3]));
  CheckEquals(notUInt16, MsgPackType.RawData[0],
              Format(BytePrefix, [MsgPackType.RawData[0]]));
  CheckEquals(n, MsgPackType.AsWord, Format(ByteOutput, [n]));
  AssertEquals(Format(WrongDataType, [DataTypesToString(MsgPackType.MsgType)]),
               Ord(mpdtNumber), Ord(MsgPackType.MsgType));
  AssertEquals(Format(WrongSubDataType, [SubDataTypeToString(MsgPackType.SubType)]),
               Ord(mpstUInt16), Ord(MsgPackType.SubType));

  MsgPackType.Free;
end;

procedure TConvertTest.TestLongWord;
var n : LongWord;
begin
  MsgPackType := TMsgPackNumber.Create;

  n := 65536; // Minimal Value
  TMsgPackNumber(MsgPackType).Value(n);
  CheckEquals(5, Length(MsgPackType.RawData), Format(ByteLength, [n, 5]));
  CheckEquals(notUInt32, MsgPackType.RawData[0],
              Format(BytePrefix, [MsgPackType.RawData[0]]));
  CheckEquals(n, MsgPackType.AsLongWord, Format(ByteOutput, [n]));
  AssertEquals(Format(WrongDataType, [DataTypesToString(MsgPackType.MsgType)]),
               Ord(mpdtNumber), Ord(MsgPackType.MsgType));
  AssertEquals(Format(WrongSubDataType, [SubDataTypeToString(MsgPackType.SubType)]),
               Ord(mpstUInt32), Ord(MsgPackType.SubType));

  n := 2147483647; // middle range
  TMsgPackNumber(MsgPackType).Value(n);
  CheckEquals(5, Length(MsgPackType.RawData), Format(ByteLength, [n, 5]));
  CheckEquals(notUInt32, MsgPackType.RawData[0],
              Format(BytePrefix, [MsgPackType.RawData[0]]));
  CheckEquals(n, MsgPackType.AsLongWord, Format(ByteOutput, [n]));
  AssertEquals(Format(WrongDataType, [DataTypesToString(MsgPackType.MsgType)]),
               Ord(mpdtNumber), Ord(MsgPackType.MsgType));
  AssertEquals(Format(WrongSubDataType, [SubDataTypeToString(MsgPackType.SubType)]),
               Ord(mpstUInt32), Ord(MsgPackType.SubType));

  n := High(LongWord); // maximal range (4294967295)
  TMsgPackNumber(MsgPackType).Value(n);
  CheckEquals(5, Length(MsgPackType.RawData), Format(ByteLength, [n, 5]));
  CheckEquals(notUInt32, MsgPackType.RawData[0],
              Format(BytePrefix, [MsgPackType.RawData[0]]));
  CheckEquals(n, MsgPackType.AsLongWord, Format(ByteOutput, [n]));
  AssertEquals(Format(WrongDataType, [DataTypesToString(MsgPackType.MsgType)]),
               Ord(mpdtNumber), Ord(MsgPackType.MsgType));
  AssertEquals(Format(WrongSubDataType, [SubDataTypeToString(MsgPackType.SubType)]),
               Ord(mpstUInt32), Ord(MsgPackType.SubType));

  MsgPackType.Free;
end;

procedure TConvertTest.TestQWord;
var n : QWord;
begin
  MsgPackType := TMsgPackNumber.Create;

  n := 4294967296; // Minimal Value
  TMsgPackNumber(MsgPackType).Value(n);
  CheckEquals(9, Length(MsgPackType.RawData), Format(ByteLength, [n, 9]));
  CheckEquals(notUInt64, MsgPackType.RawData[0],
              Format(BytePrefix, [MsgPackType.RawData[0]]));
  CheckEquals(n, MsgPackType.AsQWord, Format(ByteOutput, [n]));
  AssertEquals(Format(WrongDataType, [DataTypesToString(MsgPackType.MsgType)]),
               Ord(mpdtNumber), Ord(MsgPackType.MsgType));
  AssertEquals(Format(WrongSubDataType, [SubDataTypeToString(MsgPackType.SubType)]),
               Ord(mpstUInt64), Ord(MsgPackType.SubType));

  n := 9223372036854775807; // middle range
  TMsgPackNumber(MsgPackType).Value(n);
  CheckEquals(9, Length(MsgPackType.RawData), Format(ByteLength, [n, 9]));
  CheckEquals(notUInt64, MsgPackType.RawData[0],
              Format(BytePrefix, [MsgPackType.RawData[0]]));
  CheckEquals(n, MsgPackType.AsQWord, Format(ByteOutput, [n]));
  AssertEquals(Format(WrongDataType, [DataTypesToString(MsgPackType.MsgType)]),
               Ord(mpdtNumber), Ord(MsgPackType.MsgType));
  AssertEquals(Format(WrongSubDataType, [SubDataTypeToString(MsgPackType.SubType)]),
               Ord(mpstUInt64), Ord(MsgPackType.SubType));

  n := High(QWord); // maximal range (18446744073709551615)
  TMsgPackNumber(MsgPackType).Value(n);
  CheckEquals(9, Length(MsgPackType.RawData), Format(ByteLength, [n, 9]));
  CheckEquals(notUInt64, MsgPackType.RawData[0],
              Format(BytePrefix, [MsgPackType.RawData[0]]));
  CheckEquals(n, MsgPackType.AsQWord, Format(ByteOutput, [n]));
  AssertEquals(Format(WrongDataType, [DataTypesToString(MsgPackType.MsgType)]),
               Ord(mpdtNumber), Ord(MsgPackType.MsgType));
  AssertEquals(Format(WrongSubDataType, [SubDataTypeToString(MsgPackType.SubType)]),
               Ord(mpstUInt64), Ord(MsgPackType.SubType));

  MsgPackType.Free;
end;

procedure TConvertTest.TestShortInt;
var n : ShortInt;
begin
  MsgPackType := TMsgPackNumber.Create;

  n := -128; // Minimal Value
  TMsgPackNumber(MsgPackType).Value(n);
  CheckEquals(2, Length(MsgPackType.RawData), Format(ByteLength, [n, 2]));
  CheckEquals(notInt8, MsgPackType.RawData[0],
              Format(BytePrefix, [MsgPackType.RawData[0]]));
  CheckEquals(n, MsgPackType.AsShortInt, Format(ByteOutput, [n]));
  AssertEquals(Format(WrongDataType, [DataTypesToString(MsgPackType.MsgType)]),
               Ord(mpdtNumber), Ord(MsgPackType.MsgType));
  AssertEquals(Format(WrongSubDataType, [SubDataTypeToString(MsgPackType.SubType)]),
               Ord(mpstInt8), Ord(MsgPackType.SubType));

  n := -64; // Middle value
  TMsgPackNumber(MsgPackType).Value(n);
  CheckEquals(2, Length(MsgPackType.RawData), Format(ByteLength, [n, 2]));
  CheckEquals(notInt8, MsgPackType.RawData[0],
              Format(BytePrefix, [MsgPackType.RawData[0]]));
  CheckEquals(n, MsgPackType.AsShortInt, Format(ByteOutput, [n]));
  AssertEquals(Format(WrongDataType, [DataTypesToString(MsgPackType.MsgType)]),
               Ord(mpdtNumber), Ord(MsgPackType.MsgType));
  AssertEquals(Format(WrongSubDataType, [SubDataTypeToString(MsgPackType.SubType)]),
               Ord(mpstInt8), Ord(MsgPackType.SubType));

  n := -32; // Middle value Single Values
  TMsgPackNumber(MsgPackType).Value(n);
  CheckEquals(1, Length(MsgPackType.RawData), Format(ByteLength, [n, 1]));
  CheckEquals(Byte(n), MsgPackType.RawData[0],
              Format(BytePrefix, [MsgPackType.RawData[0]]));
  CheckEquals(n, MsgPackType.AsShortInt, Format(ByteOutput, [n]));
  AssertEquals(Format(WrongDataType, [DataTypesToString(MsgPackType.MsgType)]),
               Ord(mpdtNumber), Ord(MsgPackType.MsgType));
  AssertEquals(Format(WrongSubDataType, [SubDataTypeToString(MsgPackType.SubType)]),
               Ord(mpstInt8), Ord(MsgPackType.SubType));


  n := -1; // maximal range
  TMsgPackNumber(MsgPackType).Value(n);
  CheckEquals(1, Length(MsgPackType.RawData), Format(ByteLength, [n, 1]));
  CheckEquals(Byte(n), MsgPackType.RawData[0],
              Format(BytePrefix, [MsgPackType.RawData[0]]));
  CheckEquals(n, MsgPackType.AsShortInt, Format(ByteOutput, [n]));
  AssertEquals(Format(WrongDataType, [DataTypesToString(MsgPackType.MsgType)]),
               Ord(mpdtNumber), Ord(MsgPackType.MsgType));
  AssertEquals(Format(WrongSubDataType, [SubDataTypeToString(MsgPackType.SubType)]),
               Ord(mpstInt8), Ord(MsgPackType.SubType));

  MsgPackType.Free;
end;

procedure TConvertTest.TestSmallInt;
var n : SmallInt;
begin
  MsgPackType := TMsgPackNumber.Create;

  n := -32768; // Minimal Value
  TMsgPackNumber(MsgPackType).Value(n);
  CheckEquals(3, Length(MsgPackType.RawData), Format(ByteLength, [n, 3]));
  CheckEquals(notInt16, MsgPackType.RawData[0],
              Format(BytePrefix, [MsgPackType.RawData[0]]));
  CheckEquals(n, MsgPackType.AsSmallInt, Format(ByteOutput, [n]));
  AssertEquals(Format(WrongDataType, [DataTypesToString(MsgPackType.MsgType)]),
               Ord(mpdtNumber), Ord(MsgPackType.MsgType));
  AssertEquals(Format(WrongSubDataType, [SubDataTypeToString(MsgPackType.SubType)]),
               Ord(mpstInt16), Ord(MsgPackType.SubType));

  n := -16384; // Middle value
  TMsgPackNumber(MsgPackType).Value(n);
  CheckEquals(3, Length(MsgPackType.RawData), Format(ByteLength, [n, 3]));
  CheckEquals(notInt16, MsgPackType.RawData[0],
              Format(BytePrefix, [MsgPackType.RawData[0]]));
  CheckEquals(n, MsgPackType.AsSmallInt, Format(ByteOutput, [n]));
  AssertEquals(Format(WrongDataType, [DataTypesToString(MsgPackType.MsgType)]),
               Ord(mpdtNumber), Ord(MsgPackType.MsgType));
  AssertEquals(Format(WrongSubDataType, [SubDataTypeToString(MsgPackType.SubType)]),
               Ord(mpstInt16), Ord(MsgPackType.SubType));

  n := -129; // maximal value
  TMsgPackNumber(MsgPackType).Value(n);
  CheckEquals(3, Length(MsgPackType.RawData), Format(ByteLength, [n, 3]));
  CheckEquals(notInt16, MsgPackType.RawData[0],
              Format(BytePrefix, [MsgPackType.RawData[0]]));
  CheckEquals(n, MsgPackType.AsSmallInt, Format(ByteOutput, [n]));
  AssertEquals(Format(WrongDataType, [DataTypesToString(MsgPackType.MsgType)]),
               Ord(mpdtNumber), Ord(MsgPackType.MsgType));
  AssertEquals(Format(WrongSubDataType, [SubDataTypeToString(MsgPackType.SubType)]),
               Ord(mpstInt16), Ord(MsgPackType.SubType));

  MsgPackType.Free;
end;

procedure TConvertTest.TestLongInt;
var n : LongInt;
begin
  MsgPackType := TMsgPackNumber.Create;

  n := -2147483648; // Minimal Value
  TMsgPackNumber(MsgPackType).Value(n);
  CheckEquals(5, Length(MsgPackType.RawData), Format(ByteLength, [n, 5]));
  CheckEquals(notInt32, MsgPackType.RawData[0],
              Format(BytePrefix, [MsgPackType.RawData[0]]));
  CheckEquals(n, MsgPackType.AsLongInt, Format(ByteOutput, [n]));
  AssertEquals(Format(WrongDataType, [DataTypesToString(MsgPackType.MsgType)]),
               Ord(mpdtNumber), Ord(MsgPackType.MsgType));
  AssertEquals(Format(WrongSubDataType, [SubDataTypeToString(MsgPackType.SubType)]),
               Ord(mpstInt32), Ord(MsgPackType.SubType));

  n := -1073741824; // middle Value
  TMsgPackNumber(MsgPackType).Value(n);
  CheckEquals(5, Length(MsgPackType.RawData), Format(ByteLength, [n, 5]));
  CheckEquals(notInt32, MsgPackType.RawData[0],
              Format(BytePrefix, [MsgPackType.RawData[0]]));
  CheckEquals(n, MsgPackType.AsLongInt, Format(ByteOutput, [n]));
  AssertEquals(Format(WrongDataType, [DataTypesToString(MsgPackType.MsgType)]),
               Ord(mpdtNumber), Ord(MsgPackType.MsgType));
  AssertEquals(Format(WrongSubDataType, [SubDataTypeToString(MsgPackType.SubType)]),
               Ord(mpstInt32), Ord(MsgPackType.SubType));

  n := -32769; // maximal Value
  TMsgPackNumber(MsgPackType).Value(n);
  CheckEquals(5, Length(MsgPackType.RawData), Format(ByteLength, [n, 5]));
  CheckEquals(notInt32, MsgPackType.RawData[0],
              Format(BytePrefix, [MsgPackType.RawData[0]]));
  CheckEquals(n, MsgPackType.AsLongInt, Format(ByteOutput, [n]));
  AssertEquals(Format(WrongDataType, [DataTypesToString(MsgPackType.MsgType)]),
               Ord(mpdtNumber), Ord(MsgPackType.MsgType));
  AssertEquals(Format(WrongSubDataType, [SubDataTypeToString(MsgPackType.SubType)]),
               Ord(mpstInt32), Ord(MsgPackType.SubType));

  MsgPackType.Free;
end;

procedure TConvertTest.TestInt64;
var n : Int64;
begin
  MsgPackType := TMsgPackNumber.Create;

  n := -9223372036854775808; // Minimal Value
  TMsgPackNumber(MsgPackType).Value(n);
  CheckEquals(9, Length(MsgPackType.RawData), Format(ByteLength, [n, 9]));
  CheckEquals(notInt64, MsgPackType.RawData[0],
              Format(BytePrefix, [MsgPackType.RawData[0]]));
  CheckEquals(n, MsgPackType.AsInt64, Format(ByteOutput, [n]));
  AssertEquals(Format(WrongDataType, [DataTypesToString(MsgPackType.MsgType)]),
               Ord(mpdtNumber), Ord(MsgPackType.MsgType));
  AssertEquals(Format(WrongSubDataType, [SubDataTypeToString(MsgPackType.SubType)]),
               Ord(mpstInt64), Ord(MsgPackType.SubType));

  n := -4611686018427387904; // middle Value
  TMsgPackNumber(MsgPackType).Value(n);
  CheckEquals(9, Length(MsgPackType.RawData), Format(ByteLength, [n, 9]));
  CheckEquals(notInt64, MsgPackType.RawData[0],
              Format(BytePrefix, [MsgPackType.RawData[0]]));
  CheckEquals(n, MsgPackType.AsInt64, Format(ByteOutput, [n]));
  AssertEquals(Format(WrongDataType, [DataTypesToString(MsgPackType.MsgType)]),
               Ord(mpdtNumber), Ord(MsgPackType.MsgType));
  AssertEquals(Format(WrongSubDataType, [SubDataTypeToString(MsgPackType.SubType)]),
               Ord(mpstInt64), Ord(MsgPackType.SubType));

  n := -2147483649; // maximal Value
  TMsgPackNumber(MsgPackType).Value(n);
  CheckEquals(9, Length(MsgPackType.RawData), Format(ByteLength, [n, 9]));
  CheckEquals(notInt64, MsgPackType.RawData[0],
              Format(BytePrefix, [MsgPackType.RawData[0]]));
  CheckEquals(n, MsgPackType.AsInt64, Format(ByteOutput, [n]));
  AssertEquals(Format(WrongDataType, [DataTypesToString(MsgPackType.MsgType)]),
               Ord(mpdtNumber), Ord(MsgPackType.MsgType));
  AssertEquals(Format(WrongSubDataType, [SubDataTypeToString(MsgPackType.SubType)]),
               Ord(mpstInt64), Ord(MsgPackType.SubType));

  MsgPackType.Free;
end;

procedure TConvertTest.TestSingle;
var n, a : Single;
begin
  MsgPackType := TMsgPackNumber.Create;

  n := 1.10; // Minimal Value +-
  TMsgPackNumber(MsgPackType).Value(n);
  CheckEquals(5, Length(MsgPackType.RawData), Format(FloatLength, [n, 5]));
  CheckEquals(notFloat, MsgPackType.RawData[0],
              Format(BytePrefix, [MsgPackType.RawData[0]]));
  a := MsgPackType.AsSingle;
  AssertEquals(Format(FloatOutput, [n, a]), n, a, 0.1);
  AssertEquals(Format(WrongDataType, [DataTypesToString(MsgPackType.MsgType)]),
               Ord(mpdtNumber), Ord(MsgPackType.MsgType));
  AssertEquals(Format(WrongSubDataType, [SubDataTypeToString(MsgPackType.SubType)]),
               Ord(mpstFloat), Ord(MsgPackType.SubType));

  n := 2.5; // middle value +-
  TMsgPackNumber(MsgPackType).Value(n);
  CheckEquals(5, Length(MsgPackType.RawData), Format(FloatLength, [n, 5]));
  CheckEquals(notFloat, MsgPackType.RawData[0],
              Format(BytePrefix, [MsgPackType.RawData[0]]));
  a := MsgPackType.AsSingle;
  AssertEquals(Format(FloatOutput, [n, a]), n, a, 0.1);
  AssertEquals(Format(WrongDataType, [DataTypesToString(MsgPackType.MsgType)]),
               Ord(mpdtNumber), Ord(MsgPackType.MsgType));
  AssertEquals(Format(WrongSubDataType, [SubDataTypeToString(MsgPackType.SubType)]),
               Ord(mpstFloat), Ord(MsgPackType.SubType));

  n := 3.4E38; // maximal value +-
  TMsgPackNumber(MsgPackType).Value(n);
  CheckEquals(5, Length(MsgPackType.RawData), Format(FloatLength, [n, 5]));
  CheckEquals(notFloat, MsgPackType.RawData[0],
              Format(BytePrefix, [MsgPackType.RawData[0]]));
  a := MsgPackType.AsSingle;
  AssertEquals(Format(FloatOutput, [n, a]), n, a, 0.1);
  AssertEquals(Format(WrongDataType, [DataTypesToString(MsgPackType.MsgType)]),
               Ord(mpdtNumber), Ord(MsgPackType.MsgType));
  AssertEquals(Format(WrongSubDataType, [SubDataTypeToString(MsgPackType.SubType)]),
               Ord(mpstFloat), Ord(MsgPackType.SubType));

  MsgPackType.Free;
end;

procedure TConvertTest.TestDouble;
var n,a : Double;
begin
  MsgPackType := TMsgPackNumber.Create;

  n := 3.4E38 * 1.0000000000000001; // minimal Value +-
  TMsgPackNumber(MsgPackType).Value(n);
  CheckEquals(9, Length(MsgPackType.RawData), Format(FloatLength, [n, 9]));
  CheckEquals(notDouble, MsgPackType.RawData[0],
              Format(BytePrefix, [MsgPackType.RawData[0]]));
  a := MsgPackType.AsDouble;
  AssertEquals(Format(FloatOutput, [n, a]), n, a, 0.1);
  AssertEquals(Format(WrongDataType, [DataTypesToString(MsgPackType.MsgType)]),
               Ord(mpdtNumber), Ord(MsgPackType.MsgType));
  AssertEquals(Format(WrongSubDataType, [SubDataTypeToString(MsgPackType.SubType)]),
               Ord(mpstDouble), Ord(MsgPackType.SubType));

  n := 1.7E308 / 2; // middle Value +-
  TMsgPackNumber(MsgPackType).Value(n);
  CheckEquals(9, Length(MsgPackType.RawData), Format(FloatLength, [n, 9]));
  CheckEquals(notDouble, MsgPackType.RawData[0],
              Format(BytePrefix, [MsgPackType.RawData[0]]));
  a := MsgPackType.AsDouble;
  AssertEquals(Format(FloatOutput, [n, a]), n, a, 0.1);
  AssertEquals(Format(WrongDataType, [DataTypesToString(MsgPackType.MsgType)]),
               Ord(mpdtNumber), Ord(MsgPackType.MsgType));
  AssertEquals(Format(WrongSubDataType, [SubDataTypeToString(MsgPackType.SubType)]),
               Ord(mpstDouble), Ord(MsgPackType.SubType));

  n := 1.7E308; // maximal Value +-
  TMsgPackNumber(MsgPackType).Value(n);
  CheckEquals(9, Length(MsgPackType.RawData), Format(FloatLength, [n, 9]));
  CheckEquals(notDouble, MsgPackType.RawData[0],
              Format(BytePrefix, [MsgPackType.RawData[0]]));
  a := MsgPackType.AsDouble;
  AssertEquals(Format(FloatOutput, [n, a]), n, a, 0.1);
  AssertEquals(Format(WrongDataType, [DataTypesToString(MsgPackType.MsgType)]),
               Ord(mpdtNumber), Ord(MsgPackType.MsgType));
  AssertEquals(Format(WrongSubDataType, [SubDataTypeToString(MsgPackType.SubType)]),
               Ord(mpstDouble), Ord(MsgPackType.SubType));

  MsgPackType.Free;
end;

procedure TConvertTest.TestFixedRawByte;
var Ch : Byte;
begin
  MsgPackType := TMsgPackRaw.Create;

  Ch := 97; // letter a
  TMsgPackRaw(MsgPackType).Value(Ch);
  AssertEquals(Format(WrongRawLength, [notFixRawMin + 1, MsgPackType.RawData[0]]),
               notFixRawMin + 1, MsgPackType.RawData[0]);
  AssertEquals(Format(WrongRawValueInt, [ch, MsgPackType.AsByte]),
               ch, MsgPackType.AsByte);
  AssertEquals(Format(WrongDataType, [DataTypesToString(MsgPackType.MsgType)]),
               Ord(mpdtRaw), Ord(MsgPackType.MsgType));
  AssertEquals(Format(WrongSubDataType, [SubDataTypeToString(MsgPackType.SubType)]),
               Ord(mpstFixedRaw), Ord(MsgPackType.SubType));

  Ch := 32; // letter space
  TMsgPackRaw(MsgPackType).Value(Ch);
  AssertEquals(Format(WrongRawLength, [notFixRawMin + 1, MsgPackType.RawData[0]]),
               notFixRawMin + 1, MsgPackType.RawData[0]);
  AssertEquals(Format(WrongRawValueInt, [ch, MsgPackType.AsByte]),
               ch, MsgPackType.AsByte);
  AssertEquals(Format(WrongDataType, [DataTypesToString(MsgPackType.MsgType)]),
               Ord(mpdtRaw), Ord(MsgPackType.MsgType));
  AssertEquals(Format(WrongSubDataType, [SubDataTypeToString(MsgPackType.SubType)]),
               Ord(mpstFixedRaw), Ord(MsgPackType.SubType));

  Ch := 219; // █ ( Block )
  TMsgPackRaw(MsgPackType).Value(Ch);
  AssertEquals(Format(WrongRawLength, [notFixRawMin + 1, MsgPackType.RawData[0]]),
               notFixRawMin + 1, MsgPackType.RawData[0]);
  AssertEquals(Format(WrongRawValueInt, [ch, MsgPackType.AsByte]),
               ch, MsgPackType.AsByte);
  AssertEquals(Format(WrongDataType, [DataTypesToString(MsgPackType.MsgType)]),
               Ord(mpdtRaw), Ord(MsgPackType.MsgType));
  AssertEquals(Format(WrongSubDataType, [SubDataTypeToString(MsgPackType.SubType)]),
               Ord(mpstFixedRaw), Ord(MsgPackType.SubType));

  MsgPackType.Free;
end;

initialization
  RegisterTest(TConvertTest);
end.

