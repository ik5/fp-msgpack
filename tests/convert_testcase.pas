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
    procedure TestCardinal;
    procedure TestQWord;
    procedure TestShortInt;
    procedure TestSmallInt;
  end;

implementation
uses MsgPack_Consts;

resourcestring
  IsNilError        = 'IsNil function contain wrong value';
  RawDataLenError   = 'RawData.Len does not contain the proper length';
  RawDataWrongType  = 'RawData.RawBytes[0] contain wrong data type';
  BooleanWrongValue = 'Boolean contain wrong value';
  ByteLength        = 'Input of %d have length of %d';
  ByteOutput        = 'Input of %d must be equal to the value with not change';
  BytePrefix        = 'Number Prefix: %2x';

procedure TConvertTest.SetUp;
begin

end;

procedure TConvertTest.TestNil;
begin
  MsgPackType := TMsgPackNil.create;

  CheckEquals(True, MsgPackType.IsNil, IsNilError);
  CheckEquals(1, MsgPackType.RawData.Len, RawDataLenError);
  CheckEquals(notNil, MsgPackType.RawData.RawBytes[0], RawDataWrongType);

  MsgPackType.Free;
end;

procedure TConvertTest.TestBoolean;
begin
  MsgPackType := TMsgPackBoolean.Create;
  CheckEquals(False, TMsgPackBoolean(MsgPackType).Value, BooleanWrongValue);
  CheckEquals(1, MsgPackType.RawData.Len, RawDataLenError);
  CheckEquals(notFalse, MsgPackType.RawData.RawBytes[0], RawDataWrongType);

  TMsgPackBoolean(MsgPackType).Value := True;
  CheckEquals(True, TMsgPackBoolean(MsgPackType).Value, BooleanWrongValue);
  CheckEquals(True, MsgPackType.AsBoolean, BooleanWrongValue);
  CheckEquals(1, MsgPackType.RawData.Len, RawDataLenError);
  CheckEquals(notTrue, MsgPackType.RawData.RawBytes[0], RawDataWrongType);

  MsgPackType.Free;
end;

procedure TConvertTest.TestBytes;
var n : Byte;
begin
  MsgPackType := TMsgPackNumber.Create;
  n := 1; // Almost start

  TMsgPackNumber(MsgPackType).Value(n);
  CheckEquals(1, MsgPackType.RawData.Len, Format(ByteLength, [n, 1]));
  CheckEquals(n, MsgPackType.AsByte, Format(ByteOutput, [n]));

  n := 127; // Last low byte
  TMsgPackNumber(MsgPackType).Value(n);
  CheckEquals(1, MsgPackType.RawData.Len, Format(ByteLength, [n, 1]));
  CheckEquals(n, MsgPackType.AsByte, Format(ByteOutput, [n]));

  n := 128; // Start high byte
  TMsgPackNumber(MsgPackType).Value(n);
  CheckEquals(2, MsgPackType.RawData.Len, Format(ByteLength, [n, 2]));
  CheckEquals(notUInt8, MsgPackType.RawData.RawBytes[0],
              Format(BytePrefix, [MsgPackType.RawData.RawBytes[0]]));
  CheckEquals(n, MsgPackType.AsByte, Format(ByteOutput, [n]));

  n := 255; // Last high byte
  TMsgPackNumber(MsgPackType).Value(n);
  CheckEquals(2, MsgPackType.RawData.Len, Format(ByteLength, [n, 2]));
  CheckEquals(notUInt8, MsgPackType.RawData.RawBytes[0],
              Format(BytePrefix, [MsgPackType.RawData.RawBytes[0]]));
  CheckEquals(n, MsgPackType.AsByte, Format(ByteOutput, [n]));

  MsgPackType.Free;
end;

procedure TConvertTest.TestWord;
var n : Word;
begin
  MsgPackType := TMsgPackNumber.Create;

  n := 256; // Minimal Value
  TMsgPackNumber(MsgPackType).Value(n);
  CheckEquals(3, MsgPackType.RawData.Len, Format(ByteLength, [n, 3]));
  CheckEquals(notUInt16, MsgPackType.RawData.RawBytes[0],
              Format(BytePrefix, [MsgPackType.RawData.RawBytes[0]]));
  CheckEquals(n, MsgPackType.AsWord, Format(ByteOutput, [n]));

  n := 32767; // middle range
  TMsgPackNumber(MsgPackType).Value(n);
  CheckEquals(3, MsgPackType.RawData.Len, Format(ByteLength, [n, 3]));
  CheckEquals(notUInt16, MsgPackType.RawData.RawBytes[0],
              Format(BytePrefix, [MsgPackType.RawData.RawBytes[0]]));
  CheckEquals(n, MsgPackType.AsWord, Format(ByteOutput, [n]));

  n := High(Word); // maximal range (65535)
  TMsgPackNumber(MsgPackType).Value(n);
  CheckEquals(3, MsgPackType.RawData.Len, Format(ByteLength, [n, 3]));
  CheckEquals(notUInt16, MsgPackType.RawData.RawBytes[0],
              Format(BytePrefix, [MsgPackType.RawData.RawBytes[0]]));
  CheckEquals(n, MsgPackType.AsWord, Format(ByteOutput, [n]));

  MsgPackType.Free;
end;

procedure TConvertTest.TestCardinal;
var n : Cardinal;
begin
  MsgPackType := TMsgPackNumber.Create;

  n := 65536; // Minimal Value
  TMsgPackNumber(MsgPackType).Value(n);
  CheckEquals(5, MsgPackType.RawData.Len, Format(ByteLength, [n, 5]));
  CheckEquals(notUInt32, MsgPackType.RawData.RawBytes[0],
              Format(BytePrefix, [MsgPackType.RawData.RawBytes[0]]));
  CheckEquals(n, MsgPackType.AsCardinal, Format(ByteOutput, [n]));

  n := 2147483647; // middle range
  TMsgPackNumber(MsgPackType).Value(n);
  CheckEquals(5, MsgPackType.RawData.Len, Format(ByteLength, [n, 5]));
  CheckEquals(notUInt32, MsgPackType.RawData.RawBytes[0],
              Format(BytePrefix, [MsgPackType.RawData.RawBytes[0]]));
  CheckEquals(n, MsgPackType.AsCardinal, Format(ByteOutput, [n]));

  n := High(Cardinal); // maximal range (4294967295)
  TMsgPackNumber(MsgPackType).Value(n);
  CheckEquals(5, MsgPackType.RawData.Len, Format(ByteLength, [n, 5]));
  CheckEquals(notUInt32, MsgPackType.RawData.RawBytes[0],
              Format(BytePrefix, [MsgPackType.RawData.RawBytes[0]]));
  CheckEquals(n, MsgPackType.AsCardinal, Format(ByteOutput, [n]));

  MsgPackType.Free;
end;

procedure TConvertTest.TestQWord;
var n : QWord;
begin
  MsgPackType := TMsgPackNumber.Create;

  n := 4294967296; // Minimal Value
  TMsgPackNumber(MsgPackType).Value(n);
  CheckEquals(9, MsgPackType.RawData.Len, Format(ByteLength, [n, 9]));
  CheckEquals(notUInt64, MsgPackType.RawData.RawBytes[0],
              Format(BytePrefix, [MsgPackType.RawData.RawBytes[0]]));
  CheckEquals(n, MsgPackType.AsQWord, Format(ByteOutput, [n]));

  n := 9223372036854775807; // middle range
  TMsgPackNumber(MsgPackType).Value(n);
  CheckEquals(9, MsgPackType.RawData.Len, Format(ByteLength, [n, 9]));
  CheckEquals(notUInt64, MsgPackType.RawData.RawBytes[0],
              Format(BytePrefix, [MsgPackType.RawData.RawBytes[0]]));
  CheckEquals(n, MsgPackType.AsQWord, Format(ByteOutput, [n]));

  n := High(QWord); // maximal range (18446744073709551615)
  TMsgPackNumber(MsgPackType).Value(n);
  CheckEquals(9, MsgPackType.RawData.Len, Format(ByteLength, [n, 9]));
  CheckEquals(notUInt64, MsgPackType.RawData.RawBytes[0],
              Format(BytePrefix, [MsgPackType.RawData.RawBytes[0]]));
  CheckEquals(n, MsgPackType.AsQWord, Format(ByteOutput, [n]));

  MsgPackType.Free;
end;

procedure TConvertTest.TestShortInt;
var n : ShortInt;
begin
  MsgPackType := TMsgPackNumber.Create;

  n := -128; // Minimal Value
  TMsgPackNumber(MsgPackType).Value(n);
  CheckEquals(2, MsgPackType.RawData.Len, Format(ByteLength, [n, 2]));
  CheckEquals(notInt8, MsgPackType.RawData.RawBytes[0],
              Format(BytePrefix, [MsgPackType.RawData.RawBytes[0]]));
  CheckEquals(n, MsgPackType.AsShortInt, Format(ByteOutput, [n]));

  n := -64; // Middle value
  TMsgPackNumber(MsgPackType).Value(n);
  CheckEquals(2, MsgPackType.RawData.Len, Format(ByteLength, [n, 2]));
  CheckEquals(notInt8, MsgPackType.RawData.RawBytes[0],
              Format(BytePrefix, [MsgPackType.RawData.RawBytes[0]]));
  CheckEquals(n, MsgPackType.AsShortInt, Format(ByteOutput, [n]));


  n := -32; // Middle value Single Values
  TMsgPackNumber(MsgPackType).Value(n);
  CheckEquals(1, MsgPackType.RawData.Len, Format(ByteLength, [n, 1]));
  CheckEquals(Byte(n), MsgPackType.RawData.RawBytes[0],
              Format(BytePrefix, [MsgPackType.RawData.RawBytes[0]]));
  CheckEquals(n, MsgPackType.AsShortInt, Format(ByteOutput, [n]));


  n := -1; // maximal range
  TMsgPackNumber(MsgPackType).Value(n);
  CheckEquals(1, MsgPackType.RawData.Len, Format(ByteLength, [n, 1]));
  CheckEquals(Byte(n), MsgPackType.RawData.RawBytes[0],
              Format(BytePrefix, [MsgPackType.RawData.RawBytes[0]]));
  CheckEquals(n, MsgPackType.AsShortInt, Format(ByteOutput, [n]));

  MsgPackType.Free;
end;

procedure TConvertTest.TestSmallInt;
var n : SmallInt;
begin
  MsgPackType := TMsgPackNumber.Create;

  n := -32768; // Minimal Value
  TMsgPackNumber(MsgPackType).Value(n);
  CheckEquals(3, MsgPackType.RawData.Len, Format(ByteLength, [n, 3]));
  CheckEquals(notInt16, MsgPackType.RawData.RawBytes[0],
              Format(BytePrefix, [MsgPackType.RawData.RawBytes[0]]));
  CheckEquals(n, MsgPackType.AsSmallInt, Format(ByteOutput, [n]));

  MsgPackType.Free;
end;


initialization

  RegisterTest(TConvertTest);
end.

