{ Basic msgpack converter unit

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

unit msgpack;

{$mode objfpc}{$H+}

interface
uses SysUtils,       // for exceptions
     MsgPack_Consts; // Constants for msgpack

type
  EMsgPack          = class(Exception);
  EMsgPackWrongType = class(EMsgPack);

  TRawData = array of Byte;

  TMsgPackArray = class;
  TMsgPackMap   = class;
  TMsgPackRaw   = class;

  { TMsgPackType }

  TMsgPackType = class(TObject)
  protected
     FRawData : TRawData;
  public
    class function MsgType : TMsgPackDataTypes; virtual; abstract;
    function SubType : TMsgPackSubTypes;  virtual; abstract;

    constructor Create; virtual;

    function AsByte     : Byte;     virtual;
    function AsWord     : Word;     virtual;
    function AsLongWord : LongWord; virtual;
    function AsQWord    : QWord;    virtual;

    function AsShortInt : ShortInt; virtual;
    function AsSmallInt : SmallInt; virtual;
    function AsLongInt  : LongInt;  virtual;
    function AsInt64    : Int64;    virtual;

    function AsSingle : Single; virtual;
    function AsDouble : Double; virtual;

    function AsRaw    : TMsgPackRaw;   virtual;
    function AsArray  : TMsgPackArray; virtual;
    function AsMap    : TMsgPackMap;   virtual;

    function AsBoolean : Boolean; virtual;

    function IsNil : Boolean; virtual;

    property RawData : TRawData read FRawData write FRawData;
  end;

  { TMsgPackNil }

  TMsgPackNil = class(TMsgPackType)
  public
    class function MsgType : TMsgPackDataTypes; override;
    function SubType : TMsgPackSubTypes;  override;

    constructor Create; override;

    function IsNil : Boolean; override;
  end;

  { TMsgPackBoolean }

  TMsgPackBoolean = class(TMsgPackType)
  protected
    function GetValue : Boolean;
    procedure SetValue(AValue : Boolean);
  public
    class function MsgType : TMsgPackDataTypes; override;
    function SubType : TMsgPackSubTypes; override;

    constructor Create; Override;

    function IsNil     : Boolean; override;
    function AsBoolean : Boolean; override;

    property Value : Boolean read GetValue write SetValue;
  end;

  { TMsgPackNumber }

  TMsgPackNumber = class(TMsgPackType)
  public
    class function MsgType : TMsgPackDataTypes; override;
    function SubType : TMsgPackSubTypes;  override;

    constructor Create; override;

    function AsByte     : Byte;     override;
    function AsWord     : Word;     override;
    function AsLongWord : LongWord; override;
    function AsQWord    : QWord;    override;

    function AsShortInt : ShortInt; override;
    function AsSmallInt : SmallInt; override;
    function AsLongInt  : LongInt;  override;
    function AsInt64    : Int64;    override;

    function AsSingle : Single; override;
    function AsDouble : Double; override;

    function AsBoolean : Boolean; override;

    function IsNil : Boolean; override;

    procedure Value(AValue : Byte);     virtual;
    procedure Value(AValue : Word);     virtual;
    procedure Value(AValue : LongWord); virtual;
    procedure Value(AValue : QWord);    virtual;
    procedure Value(AValue : ShortInt); virtual;
    procedure Value(AValue : SmallInt); virtual;
    procedure Value(AValue : LongInt);  virtual;
    procedure Value(AValue : Int64);    virtual;
    procedure Value(AValue : Single);   virtual;
    procedure Value(AValue : Double);   virtual;
  end;

  { TMsgPackRaw }

  TMsgPackRaw = class(TMsgPackType)
  public
    class function MsgType : TMsgPackDataTypes; override;
    function SubType : TMsgPackSubTypes;  override;

    constructor Create; override;

    function AsByte          : Byte;          override;
    function AsWord          : Word;          override;
    function AsAnsiChar      : AnsiChar;      virtual;
    function AsWideChar      : WideChar;      virtual;
    function AsUCS2Char      : UCS2Char;      virtual;
    function AsUCS4Char      : UCS4Char;      virtual;
    function AsShortString   : ShortString;   virtual;
    function AsAnsiString    : AnsiString;    virtual;
    function AsUTF8String    : UTF8String;    virtual;
    function AsWideString    : WideString;    virtual;
    function AsUnicodeString : UnicodeString; virtual;
    function AsUCS4String    : UCS4String;    virtual;

    function IsNil : Boolean;                 override;

    procedure Value;                          virtual; // Adding empty raw
    procedure Value(AValue : Byte);           virtual;
    procedure Value(AValue : Word);           virtual;
    procedure Value(AValue : AnsiChar);       virtual;
    procedure Value(AValue : WideChar);       virtual;
    procedure Value(AValue : UCS4Char);       virtual;
    procedure Value(AValue : ShortString);    virtual;
    procedure Value(AValue : AnsiString);     virtual;
    procedure Value(AValue : UTF8String);     virtual;
    procedure Value(AValue : WideString);     virtual;
    procedure Value(AValue : UnicodeString);  virtual;
    procedure Value(AValue : UCS4String);     virtual;
  end;

  { TMsgPackArray }

  TMsgPackArray = class(TMsgPackType)

  end;

  { TMsgPackMap }

  TMsgPackMap  = class(TMsgPackType)

  end;

implementation
uses msgpack_errors;

{ TMsgPackRaw }

class function TMsgPackRaw.MsgType: TMsgPackDataTypes;
begin
  Result := mpdtRaw;
end;

function TMsgPackRaw.SubType: TMsgPackSubTypes;
begin
 case FRawData[0] of
   notFixRawMin..notFixRawMax : Result := mpstFixedRaw;
 else
   Raise EMsgPackWrongType.Create(errInvalidDataType);
 end;
end;

constructor TMsgPackRaw.Create;
begin
  inherited Create;
  SetLength(FRawData, 1);
  FRawData[0] := notFixRawMin; // Empty Raw
end;

function TMsgPackRaw.AsByte : Byte;
begin
  if FRawData[0] = notFixRawMin+1 then
    Result := FRawData[1]
  else raise EMsgPackWrongType.Create(errRawSizeTooBig);
end;

function TMsgPackRaw.AsWord : Word;
begin
 case FRawData[0] of
   notFixRawMin + 1 : Result := Self.AsByte;
   notFixRawMin + 2 : Move(FRawData[1], Result, SizeOf(Result));
 else raise EMsgPackWrongType.Create(errRawSizeTooBig);
 end;
end;

function TMsgPackRaw.AsAnsiChar: AnsiChar;
begin
  Result := Chr(Self.AsByte);
end;

function TMsgPackRaw.AsWideChar: WideChar;
begin

end;

function TMsgPackRaw.AsUCS2Char: UCS2Char;
begin

end;

function TMsgPackRaw.AsUCS4Char: UCS4Char;
begin

end;

function TMsgPackRaw.AsShortString: ShortString;
begin

end;

function TMsgPackRaw.AsAnsiString: AnsiString;
begin

end;

function TMsgPackRaw.AsUTF8String: UTF8String;
begin

end;

function TMsgPackRaw.AsWideString: WideString;
begin

end;

function TMsgPackRaw.AsUnicodeString: UnicodeString;
begin

end;

function TMsgPackRaw.AsUCS4String: UCS4String;
begin

end;

function TMsgPackRaw.IsNil : Boolean;
begin
  Result := False;
end;

procedure TMsgPackRaw.Value;
begin
  SetLength(FRawData, 1);
  FRawData[0] := notFixRawMin;
end;

procedure TMsgPackRaw.Value(AValue: Byte);
begin
  SetLength(FRawData, 2);
  FRawData[0] := notFixRawMin + 1; // Length of one char
  FRawData[1] := AValue;
end;

procedure TMsgPackRaw.Value(AValue: Word);
begin
 if AValue <= $FF then Self.Value(Byte(AValue))
 else begin
  SetLength(FRawData, 2);
  FRawData[0] := notFixRawMin +2; //Length of two chars
  Move(AValue, FRawData[1], SizeOf(AValue));
 end;
end;

procedure TMsgPackRaw.Value(AValue: AnsiChar);
begin
 Self.Value(Byte(Ord(AValue)));
end;

procedure TMsgPackRaw.Value(AValue: WideChar);
begin

end;

procedure TMsgPackRaw.Value(AValue: UCS4Char);
begin

end;

procedure TMsgPackRaw.Value(AValue: ShortString);
begin

end;

procedure TMsgPackRaw.Value(AValue: AnsiString);
begin

end;

procedure TMsgPackRaw.Value(AValue: UTF8String);
begin

end;

procedure TMsgPackRaw.Value(AValue: WideString);
begin

end;

procedure TMsgPackRaw.Value(AValue: UnicodeString);
begin

end;

procedure TMsgPackRaw.Value(AValue: UCS4String);
begin

end;

{ TMsgPackType }

constructor TMsgPackType.Create;
begin
  SetLength(FRawData, 1);
  FRawData[0] := notNil; // Default value is nil.
end;

function TMsgPackType.AsByte: Byte;
begin
  Result := 0;
end;

function TMsgPackType.AsWord: Word;
begin
  Result := 0;
end;

function TMsgPackType.AsLongWord: LongWord;
begin
  Result := 0;
end;

function TMsgPackType.AsQWord: QWord;
begin
  Result := 0;
end;

function TMsgPackType.AsShortInt: ShortInt;
begin
  Result := 0;
end;

function TMsgPackType.AsSmallInt: SmallInt;
begin
  Result := 0;
end;

function TMsgPackType.AsLongInt: LongInt;
begin
  Result := 0;
end;

function TMsgPackType.AsInt64: Int64;
begin
  Result := 0;
end;

function TMsgPackType.AsSingle: Single;
begin
  Result := 0.0;
end;

function TMsgPackType.AsDouble: Double;
begin
  Result := 0.0;
end;

function TMsgPackType.AsRaw: TMsgPackRaw;
begin
  Result := Nil;
end;

function TMsgPackType.AsArray: TMsgPackArray;
begin
  Result := Nil;
end;

function TMsgPackType.AsMap: TMsgPackMap;
begin
  Result := nil;
end;

function TMsgPackType.AsBoolean: Boolean;
begin
  Result := False;
end;

function TMsgPackType.IsNil: Boolean;
begin
  Result := True;
end;

{ TMsgPackNumber }

class function TMsgPackNumber.MsgType: TMsgPackDataTypes;
begin
  Result := mpdtNumber;
end;

function TMsgPackNumber.SubType: TMsgPackSubTypes;
begin
  case FRawData[0] of
    0..127,   notUInt8  : Result := mpstUInt8;
              notUInt16 : Result := mpstUInt16;
              notUInt32 : Result := mpstUInt32;
              notUInt64 : Result := mpstUInt64;
    224..255, notInt8   : Result := mpstInt8;
              notInt16  : Result := mpstInt16;
              notInt32  : Result := mpstInt32;
              notInt64  : Result := mpstInt64;
              notFloat  : Result := mpstFloat;
              notDouble : Result := mpstDouble;
  else
    raise EMsgPackWrongType.Create(errInvalidDataType);
  end;
end;

constructor TMsgPackNumber.Create;
begin
  inherited Create;
  FRawData[0] := 0;
end;

function TMsgPackNumber.AsByte: Byte;
begin
  case Length(FRawData) of
     1 : begin
           if FRawData[0] in [0..127] then
             Result := FRawData[0]
           else raise EMsgPackWrongType.Create(errInvalidDataType);
          end;
     2 : begin
          if FRawData[0] = notUInt8 then
           Result := FRawData[1]
          else raise EMsgPackWrongType.Create(errInvalidDataType);
         end;
     else raise EMsgPackWrongType.Create(errInvalidDataType);
    end;
end;

function TMsgPackNumber.AsWord: Word;
var Data : Word;
begin
  case Length(FRawData) of
     1..2 : Result := self.AsByte;
        3 : begin
              if FRawData[0] = notUInt16 then
               begin
                 Data := 0;
                 Move(FRawData[1], Data, SizeOf(Word));
                 Result := BEtoN(Data); // Move Big Endian to Native ...
               end
              else raise EMsgPackWrongType.Create(errInvalidDataType);
            end;
    else raise EMsgPackWrongType.Create(errInvalidDataType);
  end;
end;

function TMsgPackNumber.AsLongWord: LongWord;
var Data : LongWord;
begin
  case Length(FRawData) of
    1..3 : Result := self.AsWord;
       5 : begin
             if FRawData[0] = notUInt32 then
              begin
               Data := 0;
               Move(FRawData[1], Data, SizeOf(LongWord));
               Result := BEtoN(Data); // Move Big Endian to Native ...
              end
             else raise EMsgPackWrongType.Create(errInvalidDataType);
           end;
    else raise EMsgPackWrongType.Create(errInvalidDataType);
  end;
end;

function TMsgPackNumber.AsQWord: QWord;
var Data : QWord;
begin
  case Length(FRawData) of
    1..5 : Result := self.AsLongWord;
       9 : begin
             if FRawData[0] = notUInt64 then
              begin
               Data := 0;
               Move(FRawData[1], Data, SizeOf(QWord));
               Result := BEtoN(Data); // Move Big Endian to Native ...
              end
             else raise EMsgPackWrongType.Create(errInvalidDataType);
           end;
    else raise EMsgPackWrongType.Create(errInvalidDataType);
  end;
end;

function TMsgPackNumber.AsShortInt: ShortInt;
begin
  case Length(FRawData) of
       1 : begin
             if FRawData[0] in [224..255] then
               Result := FRawData[0] - 256
             else raise EMsgPackWrongType.Create(errInvalidDataType);
            end;
       2 : begin
            if FRawData[0] = notInt8 then
             Result := FRawData[1] - 256
            else raise EMsgPackWrongType.Create(errInvalidDataType);
           end;
       else raise EMsgPackWrongType.Create(errInvalidDataType);
  end;
end;

function TMsgPackNumber.AsSmallInt: SmallInt;
var Data : SmallInt;
begin
  case Length(FRawData) of
    1..2 : Result := Self.AsShortInt;
       3 : begin
            if FRawData[0] = notInt16 then
              begin
               Data := 0;
               Move(FRawData[1], Data, SizeOf(Data));
               Result := BEtoN(Data); // Move Big Endian to Native ...
              end
             else raise EMsgPackWrongType.Create(errInvalidDataType);
           end;
    else raise EMsgPackWrongType.Create(errInvalidDataType);
  end;
end;

function TMsgPackNumber.AsLongInt: LongInt;
var Data : LongInt;
begin
  case Length(FRawData) of
     1..3 : Result := Self.AsSmallInt;
        5 : begin
              if FRawData[0] = notInt32 then
               begin
                 Data := 0;
                 Move(FRawData[1], Data, SizeOf(Data));
                 Result := BEtoN(Data); // Move Big Endian to Native ...
               end
              else raise EMsgPackWrongType.Create(errInvalidDataType);
            end;
    else raise EMsgPackWrongType.Create(errInvalidDataType);
  end;
end;

function TMsgPackNumber.AsInt64: Int64;
var Data : Int64;
begin
  case Length(FRawData) of
    1..5 : Result := Self.AsLongInt;
       9 : begin
             if FRawData[0] = notInt64 then
              begin
               Data := 0;
               Move(FRawData[1], Data, SizeOf(Data));
               Result := BEtoN(Data); // Move Big Endian to Native ...
              end
             else raise EMsgPackWrongType.Create(errInvalidDataType);
           end;
    else raise EMsgPackWrongType.Create(errInvalidDataType);
  end;
end;

function TMsgPackNumber.AsSingle: Single;
var Data : LongWord;
begin
  if Length(FRawData) <> 5 then
   raise EMsgPackWrongType.Create(errInvalidDataType);

  if FRawData[0] = notFloat then
   begin
     Data := 0;
     Move(FRawData[1], Data, SizeOf(Data));
     Data   := BEtoN(Data);
     Result := 0;
     Move(Data, Result, SizeOf(Data));
   end
  else raise EMsgPackWrongType.Create(errInvalidDataType);
end;

function TMsgPackNumber.AsDouble: Double;
var Data : QWord;
begin
  case Length(FRawData) of
   5 : Result := Self.AsSingle;
   9 : begin
        if FRawData[0] = notDouble then
         begin
           Data := 0;
           Move(FRawData[1], Data, SizeOf(Data));
           Data   := BEtoN(Data);
           Result := 0;
           Move(Data, Result, SizeOf(Data));
         end
        else raise EMsgPackWrongType.Create(errInvalidDataType);
       end;
   else raise EMsgPackWrongType.Create(errInvalidDataType);
  end;
end;

function TMsgPackNumber.AsBoolean: Boolean;
begin
  Result := ((self.SubType <> mpstUInt8) or (Self.AsByte <> 0));
end;

function TMsgPackNumber.IsNil: Boolean;
begin
  Result := False;
end;

procedure TMsgPackNumber.Value(AValue: Byte);
begin
  if AValue > 127 then
  begin
    SetLength(FRawData, 2);
    FRawData[0] := notUInt8;
    FRawData[1] := AValue;
  end
 else begin
   SetLength(FRawData, 1);
   FRawData[0] := AValue;
 end;
end;

procedure TMsgPackNumber.Value(AValue: Word);
var ConvertedValue : Word;
begin
  if AValue <= High(Byte) then self.Value(Byte(AValue))
  else begin
    SetLength(FRawData, 3);
    ConvertedValue := NtoBE(AValue); // Convert our native Endian to Big Endian ...
    FRawData[0]    := notUInt16;
    Move(ConvertedValue, FRawData[1], SizeOf(Word));
  end;
end;

procedure TMsgPackNumber.Value(AValue: LongWord);
var ConvertedValue : LongWord;
begin
  if AValue <= High(Word) then self.Value(Word(AValue))
  else begin
    SetLength(FRawData, 5);
    ConvertedValue  := NtoBE(AValue); // Convert our native Endian to Big Endian ...
    FRawData[0]     := notUInt32;
    Move(ConvertedValue, FRawData[1], SizeOf(LongWord));
  end;
end;

procedure TMsgPackNumber.Value(AValue: QWord);
var ConvertedValue : QWord;
begin
  if AValue <= High(LongWord) then self.Value(LongWord(AValue))
  else begin
    SetLength(FRawData, 9);
    ConvertedValue := NtoBE(AValue); // Convert our native Endian to Big Endian ...
    FRawData[0]    := notUInt64;
    Move(ConvertedValue, FRawData[1], SizeOf(QWord));
  end;
end;

procedure TMsgPackNumber.Value(AValue: ShortInt);
begin
  if AValue > 0 then self.Value(Byte(AValue))
  else begin
         if AValue >= -32 then
           begin
            SetLength(FRawData, 1);
            FRawData[0] := Byte(AValue);
           end
         else begin
           SetLength(FRawData, 2);
           FRawData[0] := notInt8;
           FRawData[1] := Byte(AValue);
         end;
       end;
end;

procedure TMsgPackNumber.Value(AValue: SmallInt);
var ConvertedValue : SmallInt;
begin
  if AValue >= -128 then self.Value(ShortInt(AValue))
  else begin
    SetLength(FRawData, 3);
    ConvertedValue := NtoBE(AValue);
    FRawData[0]    := notInt16;
    Move(ConvertedValue, FRawData[1], SizeOf(ConvertedValue));
  end;
end;

procedure TMsgPackNumber.Value(AValue: LongInt);
var ConvertedValue : LongInt;
begin
  if AValue >= -32768 then Self.Value(SmallInt(AValue))
  else begin
    SetLength(FRawData, 5);
    ConvertedValue := NtoBE(AValue);
    FRawData[0]    := notInt32;
    Move(ConvertedValue, FRawData[1], SizeOf(ConvertedValue));
  end;
end;

procedure TMsgPackNumber.Value(AValue: Int64);
var ConvertedValue : Int64;
begin
  if AValue >= -2147483648 then Self.Value(LongInt(AValue))
  else begin
    SetLength(FRawData, 9);
    ConvertedValue := NtoBE(AValue);
    FRawData[0]    := notInt64;
    Move(ConvertedValue, FRawData[1], SizeOf(ConvertedValue));
  end;
end;

procedure TMsgPackNumber.Value(AValue: Single);
var ConvertedValue : LongWord;
begin
  SetLength(FRawData, 5);
  ConvertedValue := NtoBE(LongWord(AValue));
  FRawData[0]    := notFloat;
  Move(ConvertedValue, FRawData[1], SizeOf(ConvertedValue));
end;

procedure TMsgPackNumber.Value(AValue: Double);
var ConvertedValue : QWord;
begin
  if AValue <= 3.4E38 then
   Self.Value(Single(AValue))
  else begin
    SetLength(FRawData, 9);
    ConvertedValue := NtoBE(QWord(AValue));
    FRawData[0]    := notDouble;
    Move(ConvertedValue, FRawData[1], SizeOf(ConvertedValue));
  end;
end;

{ TMsgPackBoolean }

function TMsgPackBoolean.GetValue: Boolean;
begin
  case FRawData[0] of
    notFalse : Result := False;
    notTrue  : Result := True;
  else
    raise EMsgPackWrongType.Create(errInvalidDataType);
  end;
end;

procedure TMsgPackBoolean.SetValue(AValue: Boolean);
const Values : array[Boolean] of Byte = (notFalse, notTrue);
begin
  SetLength(FRawData, 1);
  FRawData[0] := Values[AValue];
end;

class function TMsgPackBoolean.MsgType: TMsgPackDataTypes;
begin
  Result := mpdtBoolean;
end;

function TMsgPackBoolean.SubType: TMsgPackSubTypes;
begin
  case FRawData[0] of
    notFalse : Result := mpstFalse;
    notTrue  : Result := mpstTrue;
  else
    raise EMsgPackWrongType.Create(errInvalidDataType);
  end;
end;

constructor TMsgPackBoolean.Create;
begin
  inherited;
  SetLength(FRawData, 1);
  FRawData[0] := notFalse;
end;

function TMsgPackBoolean.IsNil: Boolean;
begin
  Result := False;
end;

function TMsgPackBoolean.AsBoolean: Boolean;
begin
  Result := self.GetValue;
end;

{ TMsgPackNil }

class function TMsgPackNil.MsgType: TMsgPackDataTypes;
begin
  Result := mpdtNil;
end;

function TMsgPackNil.SubType: TMsgPackSubTypes;
begin
  Result := mpstNil;
end;

constructor TMsgPackNil.Create;
begin
  Inherited;
end;

function TMsgPackNil.IsNil: Boolean;
begin
  Result := True;
end;

end.

