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

function OneToTwoComplement(AValue : Byte)     : Byte;     {$ifdef SYSTEMINLINE} inline; {$ENDIF}
function OneToTwoComplement(AValue : Word)     : Word;     {$ifdef SYSTEMINLINE} inline; {$ENDIF}
function OneToTwoComplement(AValue : Cardinal) : Cardinal; {$ifdef SYSTEMINLINE} inline; {$ENDIF}
function OneToTwoComplement(AValue : QWord)    : QWord;    {$ifdef SYSTEMINLINE} inline; {$ENDIF}
function OneToTwoComplement(AValue : ShortInt) : ShortInt; {$ifdef SYSTEMINLINE} inline; {$ENDIF}
function OneToTwoComplement(AValue : SmallInt) : SmallInt; {$ifdef SYSTEMINLINE} inline; {$ENDIF}
function OneToTwoComplement(AValue : LongInt)  : LongInt;  {$ifdef SYSTEMINLINE} inline; {$ENDIF}
function OneToTwoComplement(AValue : Int64)    : Int64;    {$ifdef SYSTEMINLINE} inline; {$ENDIF}

function TwoToOneComplement(AValue : Byte)     : Byte;     {$ifdef SYSTEMINLINE} inline; {$ENDIF}
function TwoToOneComplement(AValue : Word)     : Word;     {$ifdef SYSTEMINLINE} inline; {$ENDIF}
function TwoToOneComplement(AValue : Cardinal) : Cardinal; {$ifdef SYSTEMINLINE} inline; {$ENDIF}
function TwoToOneComplement(AValue : QWord)    : QWord;    {$ifdef SYSTEMINLINE} inline; {$ENDIF}
function TwoToOneComplement(AValue : ShortInt) : ShortInt; {$ifdef SYSTEMINLINE} inline; {$ENDIF}
function TwoToOneComplement(AValue : SmallInt) : SmallInt; {$ifdef SYSTEMINLINE} inline; {$ENDIF}
function TwoToOneComplement(AValue : LongInt)  : LongInt;  {$ifdef SYSTEMINLINE} inline; {$ENDIF}
function TwoToOneComplement(AValue : Int64)    : Int64;    {$ifdef SYSTEMINLINE} inline; {$ENDIF}


implementation
//uses math;

function OneToTwoComplement(AValue: Byte): Byte;
begin
  Result := (AValue xor $FF) + 1;
end;

function OneToTwoComplement(AValue: Word): Word;
begin
  Result := (AValue xor $FFFF) + 1;
end;

function OneToTwoComplement(AValue: Cardinal): Cardinal;
begin
  Result := (AValue xor $FFFFFFFF) + 1;
end;

function OneToTwoComplement(AValue: QWord): QWord;
begin
  Result := (AValue xor $FFFFFFFFFFFFFFFF) + 1;
end;

function OneToTwoComplement(AValue: ShortInt): ShortInt;
begin
  Result := (Abs(AValue) xor $FF) + 1;
end;

function OneToTwoComplement(AValue: SmallInt): SmallInt;
begin
  Result := (Abs(AValue) xor $FFFF) + 1;
end;

function OneToTwoComplement(AValue: LongInt): LongInt;
begin
  Result := (Abs(AValue) xor $FFFFFFFF) + 1;
end;

function OneToTwoComplement(AValue: Int64): Int64;
begin
  Result := (Abs(AValue) xor $FFFFFFFFFFFFFFFF) + 1;
end;

function TwoToOneComplement(AValue: Byte): Byte;
begin
  Result := AValue xor $FF;
end;

function TwoToOneComplement(AValue: Word): Word;
begin
  Result := AValue xor $FFFF;
end;

function TwoToOneComplement(AValue: Cardinal): Cardinal;
begin
  Result := AValue xor $FFFFFFFF;
end;

function TwoToOneComplement(AValue: QWord): QWord;
begin
  Result := AValue xor $FFFFFFFFFFFFFFFF;
end;

function TwoToOneComplement(AValue: ShortInt): ShortInt;
begin
  Result := Abs(AValue) xor $FF;
end;

function TwoToOneComplement(AValue: SmallInt): SmallInt;
begin
  Result := Abs(AValue) xor $FFFF;
end;

function TwoToOneComplement(AValue: LongInt): LongInt;
begin
  Result := Abs(AValue) xor $FFFFFFFF;
end;

function TwoToOneComplement(AValue: Int64): Int64;
begin
  Result := Abs(AValue) xor $FFFFFFFFFFFFFFFF;
end;


end.

