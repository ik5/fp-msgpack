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
uses SysUtils // for exceptions
;

type
  EMsgPack          = class(Exception);
  EMsgPackWrongType = class(EMsgPack);

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
  notMap16       = $de; // Map 16 bit
  notMap32       = $df; // Map 32 bit
  notNegIntMin   = $e0; // Starting range of Negative Integer   to -32
  notNegIntMax   = $ff; // Ending range of Negative Integer   from -1

type
  // Enum that explains what is the given data type
  TMsgPackDataTypes = (
                mpdtNil,     // Contain nil data
                mpdtBoolean, // Contain true or false data
                mpdtPosInt,  // Contain Positive integer
                mpdtNegInt,  // Contain Negative integer
                mpdtFloat,   // Contain floating point
                mpdtDouble,  // Contain floating point double precision IEEE 754
                mpdtRaw,     // Contain Non numeric char
                mpdtArray,   // Contain array
                mpdtMap      // Contain map of key value
               );

  // Enum for the sub type of the data
  TMsgPackSubTypes = (
    mpstInt8,   // Signed integer  8 bit
    mpstInt16,  // Signed integer 16 bit
    mpstInt32,  // Signed integer 32 bit
    mpstInt64,  // Signed integer 64 bit
    mpstUInt8,  // Unsgined integer  8 bit
    mpstUInt16, // Unsigned integer 16 bit
    mpstUInt32, // Unsigned integer 32 bit
    mpstUInt64, // Unsigned integer 64 bit
    mpstFloat,  // Single floating point
    mpstDouble, // Double floating point
    mpstRaw16,  // Raw bytes 16 bit - String
    mpstRaw32,  // Raw bytes 32 bit
    mpstMap16,  // Map 16 bit
    mpstMap32,  // Map 32 bit
    mpstTrue,   // Boolean True
    mpstFalse,  // Boolean False
    mpstNil,    // Nil value
    mpstUnknown // Unknow type
  );

  TMsgPackArray = class;
  TMsgPackMap   = class;

  { TMsgPackType }

  TMsgPackType = class(TObject)
  protected
    type
     TCharArray = Array[0..32767] of Char;
     TRawData = record
       Len : Word;
       case Boolean of
          False : (RawBytes : TByteArray);
          True  : (RawChars : TCharArray);
       end;
    var
     RawData : TRawData;
  public
    class function MsgType : TMsgPackDataTypes; virtual; abstract;
    class function SubType : TMsgPackSubTypes;  virtual; abstract;

    function AsByte     : Byte;     virtual; abstract;
    function AsWord     : Word;     virtual; abstract;
    function AsCardinal : Cardinal; virtual; abstract;
    function AsQWord    : QWord;    virtual; abstract;

    function AsShortInt : ShortInt; virtual; abstract;
    function AsSmallInt : SmallInt; virtual; abstract;
    function AsLongInt  : LongInt;  virtual; abstract;
    function AsInt4     : Int64;    virtual; abstract;

    function AsSingle : Single; virtual; abstract;
    function AsDouble : Double; virtual; abstract;

    function AsArray  : TMsgPackArray; virtual; abstract;
    function AsMap    : TMsgPackMap;   virtual; abstract;

    function AsBoolean : Boolean; virtual; abstract;

    function IsNil : Boolean; virtual; abstract;
  end;

  { TMsgPackNumber }

  TMsgPackNumber = class(TMsgPackType)

  end;

  { TMsgPackNil }

  TMsgPackNil = class(TMsgPackType)
  public
    class function MsgType : TMsgPackDataTypes; override;
    class function SubType : TMsgPackSubTypes;  override;

    constructor Create; virtual;
    function IsNil : Boolean; override;

    property Value : TRawData read RawData;
  end;

  { TMsgPackBoolean }

  TMsgPackBoolean = class(TMsgPackType)
  protected
    function GetValue : Boolean;
    procedure SetValue(AValue : Boolean);
  public
    class function MsgType : TMsgPackDataTypes; override;
    function SubType : TMsgPackSubTypes;

    constructor Create; virtual;

    function IsNil     : Boolean; override;
    function AsBoolean : Boolean; override;

    property Value : Boolean read GetValue write SetValue;
  end;

  { TMsgPackArray }

  TMsgPackArray = class(TMsgPackType)

  end;

  { TMsgPackMap }

  TMsgPackMap  = class(TMsgPackType)

  end;

{procedure pack(AData : Byte; out APacked : TByteList); overload;
procedure pack(AData : Shortint; out APacked : TByteList); overload;

procedure unpack(APacked : TByteList; out AData : Byte); overload;
}
implementation
uses msgpack_errors;

{ TMsgPackBoolean }

function TMsgPackBoolean.GetValue: Boolean;
begin
  case RawData.RawBytes[0] of
    notFalse : Result := False;
    notTrue  : Result := True;
  end;
end;

procedure TMsgPackBoolean.SetValue(AValue: Boolean);
const Values : array[Boolean] of Byte = (notFalse, notTrue);
begin
  RawData.Len         := 1;
  RawData.RawBytes[0] := Values[AValue];
end;

class function TMsgPackBoolean.MsgType: TMsgPackDataTypes;
begin
  Result := mpdtBoolean;
end;

function TMsgPackBoolean.SubType: TMsgPackSubTypes;
begin
  case RawData.RawBytes[0] of
    notFalse : Result := mpstFalse;
    notTrue  : Result := mpstTrue;
  else
    Result := mpstUnknown;
  end;
end;

constructor TMsgPackBoolean.Create;
begin
  RawData.Len         := 1;
  RawData.RawBytes[0] := notFalse;
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

class function TMsgPackNil.SubType: TMsgPackSubTypes;
begin
  Result := mpstNil;
end;

constructor TMsgPackNil.Create;
begin
  RawData.Len         := 1;
  RawData.RawBytes[0] := notNil;
end;

function TMsgPackNil.IsNil: Boolean;
begin
  Result := True;
end;

{procedure pack(AData: Byte; out APacked: TByteList);
begin
 if AData > 127 then
  begin
    SetLength(APacked, 2);
    APacked[0] := notUInt8;
    APacked[1] := AData;
  end
 else begin
   SetLength(APacked,1);
   APacked[0] := AData;
 end;
end;

procedure pack(AData: Shortint; out APacked: TByteList);
begin
  if AData >= 0 then
   begin
    pack(Byte(AData, APacked);
   end
  else begin
     if AData >= -32 then
      begin
        SetLength(APacked, 1);
        APacked[0] := notNegIntMax + AData +1;
      end
     else begin
       SetLength(APacked, 2);
       APacked[0] := notInt8;
       APacked[1] := AData;
     end;
  end;
end;

procedure unpack(APacked: TByteList; out AData: Byte);
begin
  case Length(APacked) of
   1 : begin
         if APacked[0] in [0..127] then
           AData := APacked[0]
         else raise EMsgPackWrongType.Create(errInvalidDataType);
        end;
   2 : begin
        if APacked[0] = notUInt8 then
         AData := APacked[1]
        else raise EMsgPackWrongType.Create(errInvalidDataType);
       end;
   else raise EMsgPackWrongType.Create(errInvalidDataType);
  end;
end;
}
end.

