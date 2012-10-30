{ Bit manipulation unit
  Perform bit tasks without official Pascal function for them

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
unit msgpack_bits;

{$mode objfpc}

interface

function OneToTwoComplement(AValue : Byte)     : Byte;
function OneToTwoComplement(AValue : Word)     : Word;
function OneToTwoComplement(AValue : Cardinal) : Cardinal;
function OneToTwoComplement(AValue : QWord)    : QWord;
function OneToTwoComplement(AValue : ShortInt) : ShortInt;
function OneToTwoComplement(AValue : SmallInt) : SmallInt;
function OneToTwoComplement(AValue : LongInt)  : LongInt;
function OneToTwoComplement(AValue : Int64)    : Int64;

function TwoToOneComplement(AValue : Byte)     : Byte;
function TwoToOneComplement(AValue : Word)     : Word;
function TwoToOneComplement(AValue : Cardinal) : Cardinal;
function TwoToOneComplement(AValue : QWord)    : QWord;
function TwoToOneComplement(AValue : ShortInt) : ShortInt;
function TwoToOneComplement(AValue : SmallInt) : SmallInt;
function TwoToOneComplement(AValue : LongInt)  : LongInt;
function TwoToOneComplement(AValue : Int64)    : Int64;


implementation
//uses math;

function OneToTwoComplement(AValue: Byte): Byte;
begin

end;

function OneToTwoComplement(AValue: Word): Word;
begin

end;

function OneToTwoComplement(AValue: Cardinal): Cardinal;
begin

end;

function OneToTwoComplement(AValue: QWord): QWord;
begin

end;

function OneToTwoComplement(AValue: ShortInt): ShortInt;
begin

end;

function OneToTwoComplement(AValue: SmallInt): SmallInt;
begin

end;

function OneToTwoComplement(AValue: LongInt): LongInt;
begin

end;

function OneToTwoComplement(AValue: Int64): Int64;
begin

end;

function TwoToOneComplement(AValue: Byte): Byte;
begin

end;

function TwoToOneComplement(AValue: Word): Word;
begin

end;

function TwoToOneComplement(AValue: Cardinal): Cardinal;
begin

end;

function TwoToOneComplement(AValue: QWord): QWord;
begin

end;

function TwoToOneComplement(AValue: ShortInt): ShortInt;
begin

end;

function TwoToOneComplement(AValue: SmallInt): SmallInt;
begin

end;

function TwoToOneComplement(AValue: LongInt): LongInt;
begin

end;

function TwoToOneComplement(AValue: Int64): Int64;
begin

end;


end.

