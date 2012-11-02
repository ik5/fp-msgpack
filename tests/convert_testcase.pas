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
  Classes, SysUtils, fpcunit, testutils, testregistry;

type

  { TConvertTest }

  TConvertTest = class(TTestCase)
  protected
    procedure SetUp; override;
  published
    procedure TestPackedBytes;
    procedure TestNil;
    procedure TestBoolean;
    procedure TestWord;
    procedure TestCardinal;
  end;

implementation
uses msgpack, MsgPack_Consts;

resourcestring
  IsNilError        = 'IsNil function contain wrong value';
  RawDataLenError   = 'RawData.Len does not contain the proper length';
  RawDataWrongType  = 'RawData.RawBytes[0] contain wrong data type';
  BooleanWrongValue = 'Boolean contain wrong value';
  ByteLength        = 'Input of %d have length of %d';
  ByteOutput        = 'Input of %d must be equal to the value with not change';
  BytePrefix        = 'Byte Prefix: %2x';

procedure TConvertTest.SetUp;
begin

end;

procedure TConvertTest.TestPackedBytes;
var Input  : Byte;
    Number : TMsgPackNumber;
begin
  Number := TMsgPackNumber.Create;
  Input := 1; // Almost start

  Number.Value(Input);
  CheckEquals(1, Number.RawData.Len, Format(ByteLength, [Input, 1]));
  CheckEquals(Input, Number.AsByte, Format(ByteOutput, [Input]));

  Input := 127; // Last low byte
  Number.Value(Input);
  CheckEquals(1, Number.RawData.Len, Format(ByteLength, [Input, 1]));
  CheckEquals(Input, Number.AsByte, Format(ByteOutput, [Input]));

  Input := 128; // Start high byte
  Number.Value(Input);
  CheckEquals(2, Number.RawData.Len, Format(ByteLength, [Input, 2]));
  CheckEquals(notUInt8, Number.RawData.RawBytes[0],
              Format(BytePrefix, [Number.RawData.RawBytes[0]]));
  CheckEquals(Input, Number.AsByte, Format(ByteOutput, [Input]));

  Input := 255; // Last high byte
  Number.Value(Input);
  CheckEquals(2, Number.RawData.Len, Format(ByteLength, [Input, 2]));
  CheckEquals(notUInt8, Number.RawData.RawBytes[0],
              Format(BytePrefix, [Number.RawData.RawBytes[0]]));
  CheckEquals(Input, Number.AsByte, Format(ByteOutput, [Input]));

  Number.Free;
end;

procedure TConvertTest.TestNil;
var NilClass : TMsgPackNil;
begin
  NilClass := TMsgPackNil.create;
  CheckEquals(True, NilClass.IsNil, IsNilError);
  CheckEquals(1, NilClass.RawData.Len, RawDataLenError);
  CheckEquals(notNil, NilClass.RawData.RawBytes[0], RawDataWrongType);
  NilClass.Free;
end;

procedure TConvertTest.TestBoolean;
var BooleanClass : TMsgPackBoolean;
begin
  BooleanClass := TMsgPackBoolean.Create;
  CheckEquals(False, BooleanClass.Value, BooleanWrongValue);
  CheckEquals(1, BooleanClass.RawData.Len, RawDataLenError);
  CheckEquals(notFalse, BooleanClass.RawData.RawBytes[0], RawDataWrongType);
  BooleanClass.Value := True;
  CheckEquals(True, BooleanClass.Value, BooleanWrongValue);
  CheckEquals(True, BooleanClass.AsBoolean, BooleanWrongValue);
  CheckEquals(1, BooleanClass.RawData.Len, RawDataLenError);
  CheckEquals(notTrue, BooleanClass.RawData.RawBytes[0], RawDataWrongType);
  BooleanClass.Free;
end;

procedure TConvertTest.TestWord;
var Number : TMsgPackNumber;
    n      : Word;
begin
  Number := TMsgPackNumber.Create;

  n := 256; // Minimal Value
  Number.Value(n);
  CheckEquals(3, Number.RawData.Len, Format(ByteLength, [n, 3]));
  CheckEquals(notUInt16, Number.RawData.RawBytes[0],
              Format(BytePrefix, [Number.RawData.RawBytes[0]]));
  CheckEquals(n, Number.AsWord, Format(ByteOutput, [n]));

  n := 32767; // middle range
  Number.Value(n);
  CheckEquals(3, Number.RawData.Len, Format(ByteLength, [n, 3]));
  CheckEquals(notUInt16, Number.RawData.RawBytes[0],
              Format(BytePrefix, [Number.RawData.RawBytes[0]]));
  CheckEquals(n, Number.AsWord, Format(ByteOutput, [n]));

  n := High(Word); // maximal range (65535)
  Number.Value(n);
  CheckEquals(3, Number.RawData.Len, Format(ByteLength, [n, 3]));
  CheckEquals(notUInt16, Number.RawData.RawBytes[0],
              Format(BytePrefix, [Number.RawData.RawBytes[0]]));
  CheckEquals(n, Number.AsWord, Format(ByteOutput, [n]));

  Number.Free;
end;

procedure TConvertTest.TestCardinal;
var Number : TMsgPackNumber;
    n      : Cardinal;
begin
  Number := TMsgPackNumber.Create;

  n := 65536; // Minimal Value
  Number.Value(n);
  CheckEquals(5, Number.RawData.Len, Format(ByteLength, [n, 5]));
  CheckEquals(notUInt32, Number.RawData.RawBytes[0],
              Format(BytePrefix, [Number.RawData.RawBytes[0]]));
  CheckEquals(n, Number.AsCardinal, Format(ByteOutput, [n]));

  n := 2147483647; // middle range
  Number.Value(n);
  CheckEquals(5, Number.RawData.Len, Format(ByteLength, [n, 5]));
  CheckEquals(notUInt32, Number.RawData.RawBytes[0],
              Format(BytePrefix, [Number.RawData.RawBytes[0]]));
  CheckEquals(n, Number.AsCardinal, Format(ByteOutput, [n]));

  n := High(Cardinal); // maximal range (4294967295)
  Number.Value(n);
  CheckEquals(5, Number.RawData.Len, Format(ByteLength, [n, 5]));
  CheckEquals(notUInt32, Number.RawData.RawBytes[0],
              Format(BytePrefix, [Number.RawData.RawBytes[0]]));
  CheckEquals(n, Number.AsCardinal, Format(ByteOutput, [n]));

  Number.Free;
end;


initialization

  RegisterTest(TConvertTest);
end.

