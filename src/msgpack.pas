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

  TMsgPackArray = class;
  TMsgPackMap   = class;
  TMsgPackRaw   = class;

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
    function SubType : TMsgPackSubTypes;  virtual; abstract;

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

    function AsRaw    : TMsgPackRaw;   virtual; abstract;
    function AsArray  : TMsgPackArray; virtual; abstract;
    function AsMap    : TMsgPackMap;   virtual; abstract;

    function AsBoolean : Boolean; virtual; abstract;

    function IsNil : Boolean; virtual; abstract;
  end;

  { TMsgPackNil }

  TMsgPackNil = class(TMsgPackType)
  public
    class function MsgType : TMsgPackDataTypes; override;
    function SubType : TMsgPackSubTypes;  override;

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
    function SubType : TMsgPackSubTypes; override;

    constructor Create; virtual;

    function IsNil     : Boolean; override;
    function AsBoolean : Boolean; override;

    property Value : Boolean read GetValue write SetValue;
  end;

  { TMsgPackNumber }

  TMsgPackNumber = class(TMsgPackType)
  public
    class function MsgType : TMsgPackDataTypes; override;
    function SubType : TMsgPackSubTypes;  override;

    function AsByte     : Byte;     override;
    function AsWord     : Word;     override;
    function AsCardinal : Cardinal; override;
    function AsQWord    : QWord;    override;

    function AsShortInt : ShortInt; override;
    function AsSmallInt : SmallInt; override;
    function AsLongInt  : LongInt;  override;
    function AsInt4     : Int64;    override;

    function AsSingle : Single; override;
    function AsDouble : Double; override;

    function AsBoolean : Boolean; override;

    function IsNil : Boolean; override;

    procedure Value(AValue : Byte);     virtual;
    procedure Value(AValue : Word);     virtual;
    procedure Value(AValue : Cardinal); virtual;
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

{ TMsgPackNumber }

class function TMsgPackNumber.MsgType: TMsgPackDataTypes;
begin
  Result := mpdtNumber;
end;

function TMsgPackNumber.SubType: TMsgPackSubTypes;
begin

end;

function TMsgPackNumber.AsByte: Byte;
begin

end;

function TMsgPackNumber.AsWord: Word;
begin

end;

function TMsgPackNumber.AsCardinal: Cardinal;
begin

end;

function TMsgPackNumber.AsQWord: QWord;
begin

end;

function TMsgPackNumber.AsShortInt: ShortInt;
begin

end;

function TMsgPackNumber.AsSmallInt: SmallInt;
begin

end;

function TMsgPackNumber.AsLongInt: LongInt;
begin

end;

function TMsgPackNumber.AsInt4: Int64;
begin

end;

function TMsgPackNumber.AsSingle: Single;
begin

end;

function TMsgPackNumber.AsDouble: Double;
begin

end;

function TMsgPackNumber.AsBoolean: Boolean;
begin

end;

function TMsgPackNumber.IsNil: Boolean;
begin

end;

procedure TMsgPackNumber.Value(AValue: Byte);
begin

end;

procedure TMsgPackNumber.Value(AValue: Word);
begin

end;

procedure TMsgPackNumber.Value(AValue: Cardinal);
begin

end;

procedure TMsgPackNumber.Value(AValue: QWord);
begin

end;

procedure TMsgPackNumber.Value(AValue: ShortInt);
begin

end;

procedure TMsgPackNumber.Value(AValue: SmallInt);
begin

end;

procedure TMsgPackNumber.Value(AValue: LongInt);
begin

end;

procedure TMsgPackNumber.Value(AValue: Int64);
begin

end;

procedure TMsgPackNumber.Value(AValue: Single);
begin

end;

procedure TMsgPackNumber.Value(AValue: Double);
begin

end;

//uses msgpack_errors;

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

function TMsgPackNil.SubType: TMsgPackSubTypes;
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

