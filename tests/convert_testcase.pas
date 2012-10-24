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

  TConvertTest= class(TTestCase)
  protected
    procedure SetUp; override;
  published
    procedure TestPackedBytes;
  end;

implementation
uses msgpack;

procedure TConvertTest.SetUp;
begin

end;

procedure TConvertTest.TestPackedBytes;
(*const
  ByteLength = 'Input of %d have length of %d';
  ByteOutput = 'Input of %d must be equal to the value with not change';
  BytePrefix = 'Byte Prefix: %2x';

var Input  : Byte;
    //Output : TByteList;
  *)
begin
  (*
  Input := 1; // Almost start
  //pack(Input, Output);
  CheckEquals(1, Length(Output), Format(ByteLength, [Input, 1]));
  CheckEquals(Input, Input, Format(ByteOutput, [Input]));

  Input := 127; // Last low byte
  //pack(Input, Output);
  CheckEquals(1, Length(Output), Format(ByteLength, [Input, 1]));
  CheckEquals(Input, Output[0], Format(ByteOutput, [Input]));

  Input := 128; // Start high byte
  //pack(Input, Output);
  CheckEquals(2, Length(Output), Format(ByteLength, [Input, 2]));
  CheckEquals(notUInt8, Output[0], Format(BytePrefix, [Output[0]]));
  CheckEquals(Input, Output[1], Format(ByteOutput, [Input]));

  Input := 255; // Last high byte
  //pack(Input, Output);
  CheckEquals(2, Length(Output), Format(ByteLength, [Input, 2]));
  CheckEquals(notUInt8, Output[0], Format(BytePrefix, [Output[0]]));
  CheckEquals(Input, Output[1], Format(ByteOutput, [Input]));
  *)
end;


initialization

  RegisterTest(TConvertTest);
end.

