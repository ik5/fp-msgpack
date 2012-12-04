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

{$mode objfpc}{$H+}{$R+}

interface
uses SysUtils,       // for exceptions
     Classes,        // for TList
     MsgPack_Consts; // Constants for msgpack

type
  EMsgPack          = class(Exception);
  EMsgPackWrongType = class(EMsgPack);
  EMsgPackLength    = class(EMsgPack);

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
    function AsBoolean : Boolean; virtual;

    property Value : Boolean read GetValue write SetValue;
  end;

  { TMsgPackNumber }

  TMsgPackNumber = class(TMsgPackType)
  public
    class function MsgType : TMsgPackDataTypes; override;
    function SubType : TMsgPackSubTypes;  override;

    constructor Create; override;

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

    function AsBoolean : Boolean; virtual;

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

    function AsByte          : Byte;          virtual;
    function AsWord          : Word;          virtual;
    function AsAnsiChar      : AnsiChar;      virtual;
    function AsWideChar      : WideChar;      virtual;
    function AsUCS2Char      : UCS2Char;      virtual;
    function AsUCS4Char      : UCS4Char;      virtual;
    function AsShortString   : ShortString;   virtual;
    function AsAnsiString    : AnsiString;    virtual;
    function AsUTF8String    : UTF8String;    virtual;
    function AsWideString    : WideString;    virtual;
    function AsUnicodeString : UnicodeString; virtual;

    function AsBoolean       : Boolean;       virtual;

    function IsNil   : Boolean;               override;
    function IsEmpty : Boolean;               virtual;

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
  end;

  { TMsgPackArray }

  TMsgPackArray = class(TMsgPackType)
  protected
    FList : TList;

    procedure ToRawData; virtual;
  public
    class function MsgType : TMsgPackDataTypes; override;
    function SubType : TMsgPackSubTypes;  override;

    constructor Create; override;
    destructor Destroy; override;


  end;

  { TMsgPackMap }

  TMsgPackMap  = class(TMsgPackType)

  end;

implementation
uses msgpack_errors;

const
  wr_length  = High(Word);
  wr2_length = wr_length +1;
  lw_length = High(LongWord);

{ TMsgPackArray }

procedure TMsgPackArray.ToRawData;
var i, l, start : longword;
    t, idx      : byte;

begin
  if FList.Count > lw_length then
   raise EMsgPackLength.Create(errArraySizeTooBig) at get_caller_addr(get_frame),
                                                      get_caller_frame(get_frame);

  l := FList.Count;
  case l of
             0..15        : begin
                              FRawData[0] := (notFixArrayMax + l);
                              idx         := 1;
                              SetLength(FRawData, 2);
                            end;
            16..wr_length : begin
                              FRawData[0] := notArray16;
                              idx         := 1 + SizeOf(Word);
                              SetLength(FRawData, idx);
                              ;
                            end;
    wr2_length..lw_length : begin
                              FRawData[0] := notArray32;
                              idx         := 1 + SizeOf(longword);
                              SetLength(FRawData, idx);

                            end;
  // better safe then sorry
  else raise EMsgPackLength.Create(errArraySizeTooBig) at get_caller_addr(get_frame),
                                                          get_caller_frame(get_frame);
  end;

  start := 1;

  for i := 0 to l-1 do
    begin
      FRawData[start] := ;
    end;
end;

class function TMsgPackArray.MsgType: TMsgPackDataTypes;
begin
  Result := mpdtArray;
end;

function TMsgPackArray.SubType: TMsgPackSubTypes;
begin
  case FRawData[0] of
    (notFixArrayMin)..(notFixArrayMax) : Result := mpstArrayFixed;
    notArray16                         : Result := mpstArray16;
    notArray32                         : Result := mpstArray32;
  else raise EMsgPackWrongType.Create(errInvalidDataType);
  end;
end;

constructor TMsgPackArray.Create;
begin
  inherited Create;
  FList := TList.Create;
end;

destructor TMsgPackArray.Destroy;
var i : longword;
begin
  for i := 0 to FList.Count do
    begin
      TMsgPackType(FList.Items[i]).Free;
    end;

  FList.Free;
  inherited Destroy;
end;

{ TMsgPackRaw }

class function TMsgPackRaw.MsgType: TMsgPackDataTypes;
begin
  Result := mpdtRaw;
end;

function TMsgPackRaw.SubType: TMsgPackSubTypes;
begin
 case FRawData[0] of
   notFixRawMin..notFixRawMax : Result := mpstFixedRaw;
                 notRaw16     : Result := mpstRaw16;
                 notRaw32     : Result := mpstRaw32;
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
  Result := WideChar(Self.AsWord);
end;

function TMsgPackRaw.AsUCS2Char: UCS2Char;
begin
  Result := Self.AsWideChar;
end;

function TMsgPackRaw.AsUCS4Char: UCS4Char;
begin
  raise EMsgPack.Create(errNotImplemented);
(*  if FRawData[0] = notRaw32 then
    begin
      Move(FRawData[2], Result, SizeOf(Result));
      Result := BEtoN(Result);
    end
  else raise EMsgPackWrongType.Create(errRawSizeTooBig);
*)
end;

function TMsgPackRaw.AsShortString: ShortString;
var l : Word;
begin
  case FRawData[0] of
                      notFixRawMin : Result := '';
    (notFixRawMin+1)..notFixRawMax : begin
                                   l := FRawData[0] - notFixRawMin;
                                   SetLength(Result, l);
                                   Move(FRawData[1], Result[1],l);
                                 end;
                      notRaw16 : begin
                                  if ((Length(FRawData) -3) <= 255) then
                                    begin
                                      //l := FRawData[1];
                                      Move(FRawData[1], l, SizeOf(l));
                                      l := BEtoN(l);
                                      SetLength(Result, l);
                                      Move(FRawData[3], Result[1], l);
                                    end
                                  else raise EMsgPackLength.Create(errRawSizeTooBig);
                                 end;

    else raise EMsgPackWrongType.Create(errInvalidDataType);
  end;
end;

function TMsgPackRaw.AsAnsiString: AnsiString;
var be_16 : word;
    be_32 : longword;
begin
  case FRawData[0] of
    notFixRawMin..notFixRawMax : Result := Self.AsShortString;
                      notRaw16 : begin
                                   Move(FRawData[1], be_16, SizeOf(be_16));
                                   be_16 := BEtoN(be_16);
                                   SetLength(Result, be_16);
                                   Move(FRawData[1+SizeOf(be_16)], Result[1], be_16);
                                 end;
                      notRaw32 : begin
                                   Move(FRawData[1], be_32, SizeOf(be_32));
                                   be_32 := BEtoN(be_32);
                                   SetLength(Result, be_32);
                                   Move(FRawData[1+SizeOf(be_32)], Result[1], be_32);
                                 end;
    else raise EMsgPackWrongType.Create(errInvalidDataType);
  end;
end;

function TMsgPackRaw.AsUTF8String: UTF8String;
begin
 Result := Self.AsAnsiString;
end;

function TMsgPackRaw.AsWideString: WideString;
begin
  Result := Self.AsAnsiString;
end;

function TMsgPackRaw.AsUnicodeString: UnicodeString;
begin
 Result := Self.AsAnsiString;
end;

function TMsgPackRaw.AsBoolean: Boolean;
begin
  Result := (FRawData[0] in [(notFixRawMin+1)..notFixRawMax, notRaw16, notRaw32]);
end;

function TMsgPackRaw.IsNil : Boolean;
begin
  Result := FRawData[0] = notFixRawMin;
end;

function TMsgPackRaw.IsEmpty: Boolean;
begin
  Result := FRawData[0] = notFixRawMin;
end;

procedure TMsgPackRaw.Value;
begin
  SetLength(FRawData, 1);
  FRawData[0] := notFixRawMin;
end;

procedure TMsgPackRaw.Value(AValue: Byte);
begin
  SetLength(FRawData, SizeOf(AValue) + 1);
  FRawData[0] := notFixRawMin + 1; // Length of one char
  FRawData[1] := AValue;
end;

procedure TMsgPackRaw.Value(AValue: Word);
begin
 if AValue <= $FF then Self.Value(Byte(AValue))
 else begin
  SetLength(FRawData, SizeOf(AValue) + 1);
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
 Self.Value(Word(AValue));
end;

procedure TMsgPackRaw.Value(AValue: UCS4Char);
var ConvertedValue : UCS4Char;
begin
  raise EMsgPack.Create(errNotImplemented);
(*  SetLength(FRawData, SizeOf(AValue) +2);
  FRawData[0]    := notRaw32;
  FRawData[1]    := SizeOf(AValue);
  ConvertedValue := NtoBE(AValue);
  Move(ConvertedValue, FRawData[2], SizeOf(AValue));
*)
end;

procedure TMsgPackRaw.Value(AValue: ShortString);
var l    : Byte;
    be_l : word;
begin
  l := Length(Avalue);
  case l of
        0 : self.Value; // Add it as empty string
    1..31 : begin
              SetLength(FRawData, l+1);
              FRawData[0] := notFixRawMin + l;
              Move(AValue[1], FRawData[1], l);
            end;
    else begin
          SetLength(FRawData, l+SizeOf(word)+1);
          FRawData[0] := notRaw16;
          be_l := NtoBE(l);
          Move(be_l, FRawData[1], SizeOf(be_l));
          Move(AValue[1], FRawData[3], l);
         end;
  end;
end;

procedure TMsgPackRaw.Value(AValue: AnsiString);
var l     : qword;
    be_16 : word;
    be_32 : longword;
begin
  l := Length(AValue);
  case l of
      0..255       : Value(ShortString(AValue));
    256..wr_length : begin
                       SetLength(FRawData, l + SizeOf(be_16) + 1);
                       FRawData[0] := notRaw16;
                       be_16       := l;
                       be_16       := NtoBE(be_16);
                       Move(be_16, FRawData[1], SizeOf(be_16));
                       Move(AValue[1], FRawData[3], l);
                     end;

    wr2_length..lw_length :
                     begin
                       SetLength(FRawData, l + SizeOf(be_32) +1);
                       FRawData[0] := notRaw32;
                       be_32       := l;
                       be_32       := NtoBE(be_32);
                       Move(be_32, FRawData[1], SizeOf(be_32));
                       Move(AValue[1], FRawData[1+SizeOf(be_32)], l);
                     end;
  else begin
         raise EMsgPackLength.Create(errRawSizeTooBig);
       end;
  end;
end;

procedure TMsgPackRaw.Value(AValue: UTF8String);
begin
  self.Value(AnsiString(AValue));
end;

procedure TMsgPackRaw.Value(AValue: WideString);
begin
  Self.Value(AnsiString(AValue));
end;

procedure TMsgPackRaw.Value(AValue: UnicodeString);
begin
  Self.Value(AnsiString(AValue));
end;

{ TMsgPackType }

constructor TMsgPackType.Create;
begin
  SetLength(FRawData, 1);
  FRawData[0] := notNil; // Default value is nil.
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

