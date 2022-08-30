//Based on unit by Albert de Weerd

{*******************************************************}
{               MiTeC Common Routines                   }
{                    IP Types                           }
{                                                       }
{                                                       }
{         Copyright (c) 1997-2019 Michal Mutl           }
{                                                       }
{*******************************************************}

{$INCLUDE Compilers.inc}

unit MiTeC_IPTypes;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils, System.Classes, System.Variants, System.TypInfo,
     System.StrUtils, WinAPI.ShellAPI, System.Math, WinApi.Winsock,
     {$ELSE}
     Windows, SysUtils, Classes, Variants, TypInfo,  StrUtils, ShellAPI, Math, Winsock,
     {$ENDIF}
     MiTeC_Ws2_32;

const
  IPv4BitSize = SizeOf(Byte) * 4 * 8;
  IPv6BitSize = SizeOf(Word) * 8 * 8;
  DefPortNumber = 80;
  DefProtocol = 'http';

type
  T4 = 0..3;
  T8 = 0..7;
  TIPv4ByteArray = array[T4] of Byte;
  TIPv6WordArray = array[T8] of Word;

  TIPv4 = packed record
    case Integer of
      0: (D, C, B, A: Byte);
      1: (Groups: TIPv4ByteArray);
      2: (Value: Cardinal);
  end;

  TIPv6 = packed record
    case Integer of
      0: (H, G, F, E, D, C, B, A: Word);
      1: (Groups: TIPv6WordArray);
  end;

  TCharCase = (ccUpperCase, ccLowerCase);

  EIPv4Error = class(EVariantError);
  EIPv6Error = class(EVariantError);

function varIPv4: TVarType;
function varIPv6: TVarType;

procedure VarIPv6Create(var Dest: Variant; const AIPv6: TIPv6); overload;
function VarIPv6Create: Variant; overload;
function VarIPv6Create(const A, B, C, D, E, F, G, H: Word): Variant; overload;
function VarIPv6Create(const AIPv6: TIPv6): Variant; overload;
function VarIPv6Create(const Groups: TIPv6WordArray): Variant; overload;
function VarIPv6Create(const S: String): Variant; overload;

function CardinalToIPv4(AValue: Cardinal): TIPv4;
function DoubleToIPv4(const AValue: Double): TIPv4;
function ExtendedToIPv4(const AValue: Extended): TIPv4;
function SingleToIPv4(const AValue: Single): TIPv4;
function StrToIPv4(const S: String): TIPv4;
function TryVarToIPv4(const AValue: Variant; out AIPv4: TIPv4): Boolean;
function VarIsIPv4(const AValue: Variant): Boolean;
function VarToIPv4(const AValue: Variant): TIPv4;
function ReverseIPv4(AValue: TIPv4): TIPv4;
function IPv4ToInAddr(AValue: TIPv4): in_addr;
function InAddrToIPv4(AValue: in_addr): TIPv4;
function IsIPv4(AValue: string): Boolean;

function IPv4Compare(const AIPv41, AIPv42: TIPv4): Integer;
function SameIPv4(const AIPv41, AIPv42: TIPv4): Boolean;
procedure IPv4ToBits(const AIPv4: TIPv4; ABits: TBits);
function IPv4ToCardinal(const AIPv4: TIPv4): Cardinal;
function IPv4ToDouble(const AIPv4: TIPv4): Double;
function IPv4ToExtended(const AIPv4: TIPv4): Extended;
function IPv4ToIPv6(const AIPv4: TIPv4): TIPv6;
function IPv4ToSingle(const AIPv4: TIPv4): Single;
function IPv4ToStr(const AIPv4: TIPv4): String;
function IPv4ToStrHex(const AIPv4: TIPv4): String;
function IPv4ToStrOutwr(const AIPv4: TIPv4): String;
function IPv4ToURL(const AIPv4: TIPv4; const Protocol: String = DefProtocol;
  const PortNumber: Word = DefPortNumber): String;
function IPv4ToVar(const AIPv4: TIPv4): Variant;

function DoubleToIPv6(const AValue: Double): TIPv6;
function ExtendedToIPv6(const AValue: Extended): TIPv6;
function SingleToIPv6(const AValue: Single): TIPv6;
function StrToIPv6(const S: String): TIPv6;
function TryVarToIPv6(const AValue: Variant; out AIPv6: TIPv6): Boolean;
function VarIsIPv6(const AValue: Variant): Boolean;
function VarToIPv6(const AValue: Variant): TIPv6;
function IsIPv6(AValue: string): Boolean;

function IPv6Compare(const AIPv61, AIPv62: TIPv6): Integer;
function SameIPv6(const AIPv61, AIPv62: TIPv6): Boolean;
procedure IPv6ToBits(const AIPv6: TIPv6; ABits: TBits);
function IPv6ToDouble(const AIPv6: TIPv6): Double;
function IPv6ToExtended(const AIPv6: TIPv6): Extended;
function IPv6ToIPv4(const AIPv6: TIPv6): TIPv4;
function IPv6ToSingle(const AIPv6: TIPv6): Single;
function IPv6ToStr(const AIPv6: TIPv6): String;
function IPv6ToStrCompr(const AIPv6: TIPv6): String;
function IPv6ToStrOutwr(const AIPv6: TIPv6): String;
function IPv6ToURL(const AIPv6: TIPv6; const Protocol: String = DefProtocol;
  const PortNumber: Word = DefPortNumber): String;
function IPv6ToVar(const AIPv6: TIPv6): Variant;
function IPv6IsNull(const AIPv6: TIPv6): Boolean;
function IPv6ToInAddr(AValue: TIPv6): in6_addr;
function InAddrToIPv6(AValue: in6_addr): TIPv6;

function IPv6AddOp(const Left, Right: TIPv6): TIPv6;
function IPv6AndOp(const Left, Right: TIPv6): TIPv6;
function IPv6OrOp(const Left, Right: TIPv6): TIPv6;
function IPv6SubtractOp(const Left, Right: TIPv6): TIPv6;
function IPv6XorOp(const Left, Right: TIPv6): TIPv6;

const
  ZeroIPv4: TIPv4 = (D: 0; C: 0; B: 0; A: 0);
  ZeroIPv6: TIPv6 = (H: 0; G: 0; F: 0; E: 0; D: 0; C: 0; B: 0; A: 0);

var
  IPCharCase: TCharCase = ccUpperCase;

implementation

const
  SInvalidIPv4Value = '''%s'' is not a valid IPv4 address';
  SInvalidIPv4FormatType = 'Invalid format type for IPv4';
  SInvalidIPv6Value = '''%s'' is not a valid IPv6 address';
  SInvalidIPv6FormatType = 'Invalid format type for IPv6';

type
  TIPv4VariantType = class(TInvokeableVariantType)
  public
    procedure Cast(var Dest: TVarData; const Source: TVarData); override;
    procedure CastTo(var Dest: TVarData; const Source: TVarData;
      const AVarType: Word); override;
    procedure Clear(var V: TVarData); override;
    procedure Compare(const Left, Right: TVarData;
      var Relationship: TVarCompareResult); override;
    procedure Copy(var Dest: TVarData; const Source: TVarData;
      const Indirect: Boolean); override;
    function DoProcedure(const V: TVarData; const Name: String;
      const Arguments: TVarDataArray): Boolean; override;
    function GetProperty(var Dest: TVarData; const V: TVarData;
      const Name: String): Boolean; override;
  end;

  TIPv6VariantType = class(TInvokeableVariantType)
  protected
    function LeftPromotion(const V: TVarData; const Operator: TVarOp;
      out RequiredVarType: Word): Boolean; override;
    function RightPromotion(const V: TVarData; const Operator: TVarOp;
      out RequiredVarType: Word): Boolean; override;
  public
    procedure BinaryOp(var Left: TVarData; const Right: TVarData;
      const Operator: TVarOp); override;
    procedure Cast(var Dest: TVarData; const Source: TVarData); override;
    procedure CastTo(var Dest: TVarData; const Source: TVarData;
      const AVarType: Word); override;
    procedure Clear(var V: TVarData); override;
    procedure Compare(const Left, Right: TVarData;
      var Relationship: TVarCompareResult); override;
    procedure Copy(var Dest: TVarData; const Source: TVarData;
      const Indirect: Boolean); override;
    function DoFunction(var Dest: TVarData; const V: TVarData;
      const Name: String; const Arguments: TVarDataArray): Boolean; override;
    function DoProcedure(const V: TVarData; const Name: String;
      const Arguments: TVarDataArray): Boolean; override;
    function GetProperty(var Dest: TVarData; const V: TVarData;
      const Name: String): Boolean; override;
    function SetProperty({$IFDEF FPC}var{$ELSE}const{$ENDIF} V: TVarData; const Name: String;
      const Value: TVarData): Boolean; override;
  end;

var
  IPv4VariantType: TIPv4VariantType = nil;
  IPv6VariantType: TIPv6VariantType = nil;
  IPv6MaxOrdValue: Double = 0;

type
  TIPv6Object = class(TPersistent)
  private
    FIPv6: TIPv6;
    function GetAsDouble: Double;
    function GetAsExtended: Extended;
    function GetAsSingle: Single;
    function GetAsString: String;
    function GetAsStringCompressed: String;
    function GetAsStringOutwritten: String;
    function GetGroups(Index: T8): Word;
    procedure SetAsExtended(const Value: Extended);
    procedure SetAsDouble(const Value: Double);
    procedure SetAsSingle(const Value: Single);
    procedure SetAsString(const Value: String);
    procedure SetGroups(Index: T8; Value: Word);
  public
    function AsURL: String;
    function Compare(Value: TIPv6Object): TVarCompareResult;
    constructor Create(const AIPv6: TIPv6); overload;
    constructor Create(AIPv6Object: TIPv6Object); overload;
    constructor Create(const S: String); overload;
    function Equals(const AValue: Variant): Boolean; overload;
    function Equals(const AIPv6: TIPv6): Boolean; overload;
    function Group(Index: T8): Word;
    procedure Follow;
    function IsZero: Boolean;
    property Groups[Index: T8]: Word read GetGroups write SetGroups;
    property IPv6: TIPv6 read FIPv6 write FIPv6;
  published
    property AsDouble: Double read GetAsDouble write SetAsDouble;
    property AsExtended: Extended read GetAsExtended write SetAsExtended;
    property AsSingle: Single read GetAsSingle write SetAsSingle;
    property AsString: String read GetAsString write SetAsString;
    property AsStringCompressed: String read GetAsStringCompressed
      write SetAsString;
    property AsStringOutwritten: String read GetAsStringOutwritten
      write SetAsString;
  end;

  TIPv4VarData = packed record
    VType: TVarType;
    Reserved1, Reserved2, Reserved3: Word;
    VIPv4: TIPv4;
    Reserved4: LongWord;
    {$IFDEF WIN64}
    Reserved5: LongWord;
    Reserved6: LongWord;
    {$ENDIF}
  end;

  TIPv6VarData = packed record
    VType: TVarType;
    Reserved1, Reserved2, Reserved3: Word;
    VIPv6: TIPv6Object;
    Reserved4: LongWord;
    {$IFDEF WIN64}
    Reserved5: LongWord;
    {$ENDIF}
  end;

function varIPv4: TVarType;
begin
  Result:=IPv4VariantType.VarType;
end;

function varIPv6: TVarType;
begin
  Result:=IPv6VariantType.VarType;
end;

procedure IPv4Error(const Message: String);
begin
  raise EIPv4Error.Create(Message);
end;

procedure IPv4ErrorFmt(const Message, IPv4AsString: String);
begin
  raise EIPv4Error.Create(Format(Message, [IPv4AsString]));
end;

procedure IPv6Error(const Message: String);
begin
  raise EIPv6Error.Create(Message);
end;

procedure IPv6ErrorFmt(const Message, IPv6AsString: String);
begin
  raise EIPv6Error.Create(Format(Message, [IPv6AsString]));
end;

{ TIPv6Object }

function TIPv6Object.AsURL: String;
begin
  Result:=IPv6ToURL(FIPv6);
end;

function TIPv6Object.Compare(Value: TIPv6Object): TVarCompareResult;
begin
  Result:=TVarCompareResult(IPv6Compare(Self.FIPv6, Value.FIPv6) + 1);
end;

constructor TIPv6Object.Create(const AIPv6: TIPv6);
begin
  inherited Create;
  Move(AIPv6, FIPv6, SizeOf(TIPv6));
end;

constructor TIPv6Object.Create(AIPv6Object: TIPv6Object);
begin
  Create(AIPv6Object.FIPv6);
end;

constructor TIPv6Object.Create(const S: String);
begin
  Create(StrToIPv6(S));
end;

function TIPv6Object.Equals(const AValue: Variant): Boolean;
begin
  if VarIsIPv6(AValue) then
    Result:=Equals(TIPv6VarData(AValue).VIPv6.IPv6)
  else if not VarIsNull(AValue) and not VarIsEmpty(AValue) then
  begin
    Result:=Equals(StrToIPv6(AValue))
  end
  else
    Result:=False;
end;

function TIPv6Object.Equals(const AIPv6: TIPv6): Boolean;
begin
  Result:=CompareMem(@FIPv6, @AIPv6, SizeOf(TIPv6));
end;

procedure TIPv6Object.Follow;
begin
  ShellExecute(0, 'Open', PChar(IPv6ToURL(FIPv6)), nil, nil, SW_SHOWNORMAL);
end;

function TIPv6Object.GetAsDouble: Double;
begin
  Result:=IPv6ToDouble(FIPv6);
end;

function TIPv6Object.GetAsExtended: Extended;
begin
  Result:=IPv6ToExtended(FIPv6);
end;

function TIPv6Object.GetAsSingle: Single;
begin
  Result:=IPv6ToSingle(FIPv6);
end;

function TIPv6Object.GetAsString: String;
begin
  Result:=IPv6ToStr(FIPv6);
end;

function TIPv6Object.GetAsStringCompressed: String;
begin
  Result:=IPv6ToStrCompr(FIPv6);
end;

function TIPv6Object.GetAsStringOutwritten: String;
begin
  Result:=IPv6ToStrOutwr(FIPv6);
end;

function TIPv6Object.GetGroups(Index: T8): Word;
begin
  Result:=FIPv6.Groups[High(T8) - Index];
end;

function TIPv6Object.Group(Index: T8): Word;
begin
  Result:=Groups[Index];
end;

function TIPv6Object.IsZero: Boolean;
begin
  Result:=Equals(ZeroIpv6);
end;

procedure TIPv6Object.SetAsDouble(const Value: Double);
begin
  FIPv6:=DoubleToIPv6(Value);
end;

procedure TIPv6Object.SetAsExtended(const Value: Extended);
begin
  FIPv6:=ExtendedToIPv6(Value);
end;

procedure TIPv6Object.SetAsSingle(const Value: Single);
begin
  FIPv6:=SingleToIPv6(Value);
end;

procedure TIPv6Object.SetAsString(const Value: String);
begin
  FIPv6:=StrToIPv6(Value);
end;

procedure TIPv6Object.SetGroups(Index: T8; Value: Word);
begin
  if FIPv6.Groups[High(T8) - Index] <> Value then
    FIPv6.Groups[High(T8) - Index]:=Value;
end;

{ TIPv4VariantType }

procedure TIPv4VariantType.Cast(var Dest: TVarData; const Source: TVarData);
var
  LSource: TVarData;
begin
  VarDataInit(LSource);
  try
    VarDataClear(Dest);
    VarDataCopyNoInd(LSource, Source);
    TIPv4VarData(Dest).VIPv4:=VarToIPv4(Variant(LSource));
    Dest.VType:=VarType;
  finally
    VarDataClear(LSource);
  end;
end;

procedure TIPv4VariantType.CastTo(var Dest: TVarData; const Source: TVarData;
  const AVarType: Word);
var
  LTemp: TVarData;
begin
  if Source.VType = VarType then
    case AVarType of
      varDouble:
        begin
          VarDataClear(Dest);
          Dest.VType:=varDouble;
          Dest.VDouble:=IPv4ToDouble(TIPv4VarData(Source).VIPv4);
        end;
      varInt64:
        begin
          VarDataClear(Dest);
          Dest.VType:=varInt64;
          Dest.VInt64:=TIPv4VarData(Source).VIPv4.Value;
        end;
      varLongWord:
        begin
          VarDataClear(Dest);
          Dest.VType:=varLongWord;
          Dest.VLongWord:=TIPv4VarData(Source).VIPv4.Value;
        end;
      varOleStr:
        VarDataFromOleStr(Dest, IPv4ToStr(TIPv4VarData(Source).VIPv4));
      varSingle:
        begin
          VarDataClear(Dest);
          Dest.VType:=varSingle;
          Dest.VSingle:=IPv4ToSingle(TIPv4VarData(Source).VIPv4);
        end;
      varString:
        VarDataFromStr(Dest, IPv4ToStr(TIPv4VarData(Source).VIPv4));
    else
      VarDataInit(LTemp);
      try
        LTemp.VType:=varLongWord;
        LTemp.VLongWord:=TIPv4VarData(Source).VIPv4.Value;
        VarDataCastTo(Dest, LTemp, AVarType);
      finally
        VarDataClear(LTemp);
      end;
    end
  else
    inherited CastTo(Dest, Source, AVarType);
end;

procedure TIPv4VariantType.Clear(var V: TVarData);
begin
  V.VType:=varEmpty;
  TIPv4VarData(V).VIPv4:=ZeroIPv4;
end;

procedure TIPv4VariantType.Compare(const Left, Right: TVarData;
  var Relationship: TVarCompareResult);
begin
  RelationShip:=TVarCompareResult(IPv4Compare(TIPv4VarData(Left).VIPv4,
     TIPv4VarData(Right).VIPv4) + 1);
end;

procedure TIPv4VariantType.Copy(var Dest: TVarData; const Source: TVarData;
  const Indirect: Boolean);
begin
  if Indirect and VarDataIsByRef(Source) then
    VarDataCopyNoInd(Dest, Source)
  else
    with TIPv4VarData(Dest) do
    begin
      VType:=VarType;
      VIPv4:=TIPv4VarData(Source).VIPv4;
    end;
end;

function TIPv4VariantType.DoProcedure(const V: TVarData; const Name: String;
  const Arguments: TVarDataArray): Boolean;
begin
  Result:=True;
  if Name = 'FOLLOW' then
    ShellExecute(0, 'Open', PChar(IPv4ToURL(TIPv4VarData(V).VIPv4)), nil, nil,
      SW_SHOWNORMAL)
  else
    Result:=inherited DoProcedure(V, Name, Arguments);
end;

function TIPv4VariantType.GetProperty(var Dest: TVarData; const V: TVarData;
  const Name: String): Boolean;
begin
  Result:=True;
  if Name = 'ASCARDINAL' then
    Variant(Dest):=IPv4ToCardinal(TIPv4VarData(V).VIPv4)
  else if Name = 'ASEXTENDED' then
    Variant(Dest):=IPv4ToExtended(TIPv4VarData(V).VIPv4)
  else if Name = 'ASDOUBLE' then
    Variant(Dest):=IPv4ToDouble(TIPv4VarData(V).VIPv4)
  else if Name = 'ASSINGLE' then
    Variant(Dest):=IPv4ToSingle(TIPv4VarData(V).VIPv4)
  else if Name = 'ASSTRING' then
    Variant(Dest):=IPv4ToStr(TIPv4VarData(V).VIPv4)
  else if Name = 'ASSTRINGOUTWRITTEN' then
    Variant(Dest):=IPv4ToStrOutwr(TIPv4VarData(V).VIPv4)
  else
    Result:=inherited GetProperty(Dest, V, Name);
end;

{ TIPv6VariantType }

procedure TIPv6VariantType.BinaryOp(var Left: TVarData; const Right: TVarData;
  const Operator: TVarOp);
var
  L: TIPv6Object;
  R: TIPv6Object;
begin
  if (Left.VType = VarType) and (Right.VType = VarType) then
  begin
    L:=TIPv6VarData(Left).VIPv6;
    R:=TIPv6VarData(Right).VIPv6;
    case Operator of
      opAdd:
        L.IPv6:=IPv6AddOp(L.IPv6, R.IPv6);
      opAnd:
        L.IPv6:=IPv6AndOp(L.IPv6, R.IPv6);
      opOr:
        L.IPv6:=IPv6OrOp(L.IPv6, R.IPv6);
      opSubtract:
        L.IPv6:=IPv6SubtractOp(L.IPv6, R.IPv6);
      opXor:
        L.IPv6:=IPv6XorOp(L.IPv6, R.IPv6);
    else
      inherited BinaryOp(Left, Right, Operator);
    end;
  end
  else if (Left.VType = VarType) and (Operator = opAdd) and
    VarDataIsStr(Right) then
  begin
    Left.VType:=varString;
    String(Left.VString):=IPv6ToStr(TIPv6VarData(Left).VIPv6.IPv6) +
      VarDataToStr(Right);
  end
  else if (Right.VType = VarType) and (Operator = opAdd) and
    VarDataIsStr(Left) then
  begin
    VarDataFromStr(Left,
      VarDataToStr(Left) + IPv6ToStr(TIPv6VarData(Right).VIPv6.IPv6));
  end
  else
    inherited BinaryOp(Left, Right, Operator);
end;

procedure TIPv6VariantType.Cast(var Dest: TVarData; const Source: TVarData);
var
  LSource: TVarData;
  LTemp: TVarData;
begin
  VarDataInit(LSource);
  try
    VarDataCopyNoInd(LSource, Source);
    if VarDataIsStr(LSource) then
      TIPv6VarData(Dest).VIPv6:=TIPv6Object.Create(VarDataToStr(LSource))
    else
    begin
      VarDataInit(LTemp);
      try
        VarDataCastTo(LTemp, LSource, varString);
        TIPv6VarData(Dest).VIPv6:=TIPv6Object.Create(String(LTemp.VString));
      finally
        VarDataClear(LTemp);
      end;
    end;
    Dest.VType:=VarType;
  finally
    VarDataClear(LSource);
  end;
end;

procedure TIPv6VariantType.CastTo(var Dest: TVarData; const Source: TVarData;
  const AVarType: Word);
var
  LTemp: TVarData;
begin
  if Source.VType = VarType then
    case AVarType of
      varOleStr:
        VarDataFromOleStr(Dest, TIPv6VarData(Source).VIPv6.AsString);
      varString:
        VarDataFromStr(Dest, TIPv6VarData(Source).VIPv6.AsString);
    else
      VarDataInit(LTemp);
      try
        LTemp.VType:=varString;
        String(LTemp.VString):=IPv6ToStr(TIPv6VarData(Source).VIPv6.IPv6);
        VarDataCastTo(Dest, LTemp, AVarType);
      finally
        VarDataClear(LTemp);
      end;
    end
  else
    inherited CastTo(Dest, Source, AVarType);
end;

procedure TIPv6VariantType.Clear(var V: TVarData);
begin
  V.VType:=varEmpty;
  FreeAndNil(TIPv6VarData(V).VIPv6);
end;

procedure TIPv6VariantType.Compare(const Left, Right: TVarData;
  var Relationship: TVarCompareResult);
begin
  Relationship:=TIPv6VarData(Left).VIPv6.Compare(TIPv6VarData(Right).VIPv6)
end;

procedure TIPv6VariantType.Copy(var Dest: TVarData; const Source: TVarData;
  const Indirect: Boolean);
begin
  if Indirect and VarDataIsByRef(Source) then
    VarDataCopyNoInd(Dest, Source)
  else
    with TIPv6VarData(Dest) do
    begin
      VType:=VarType;
      VIPv6:=TIPv6Object.Create(TIPv6VarData(Source).VIPv6);
    end;
end;

function TIPv6VariantType.DoFunction(var Dest: TVarData; const V: TVarData;
  const Name: String; const Arguments: TVarDataArray): Boolean;
begin
  Result:=True;
  if Name = 'ASURL' then
    Variant(Dest):=TIPv6VarData(V).VIPv6.AsURL
  else if Name = 'EQUALS' then
    Variant(Dest):=TIPv6VarData(V).VIPv6.Equals(Variant(Arguments[0]))
  else if Name = 'GROUP' then
    Variant(Dest):=TIPv6VarData(V).VIPv6.Group(Variant(Arguments[0]))
  else if Name = 'ISZERO' then
    Variant(Dest):=TIPv6VarData(V).VIPv6.IsZero
  else
    Result:=inherited DoFunction(Dest, V, Name, Arguments);
end;

function TIPv6VariantType.DoProcedure(const V: TVarData; const Name: String;
  const Arguments: TVarDataArray): Boolean;
begin
  Result:=True;
  if Name = 'FOLLOW' then
    TIPv6VarData(V).VIPv6.Follow
  else
    Result:=inherited DoProcedure(V, Name, Arguments);
end;

function TIPv6VariantType.GetProperty(var Dest: TVarData; const V: TVarData;
  const Name: String): Boolean;
begin
  Result:=True;
  if Name = 'ASEXTENDED' then
    Variant(Dest):=TIPv6VarData(V).VIPv6.AsExtended
  else if Name = 'ASDOUBLE' then
    Variant(Dest):=TIPv6VarData(V).VIPv6.AsDouble
  else if Name = 'ASSINGLE' then
    Variant(Dest):=TIPv6VarData(V).VIPv6.AsSingle
  else if Name = 'ASSTRING' then
    Variant(Dest):=TIPv6VarData(V).VIPv6.AsString
  else if Name = 'ASSTRINGCOMPRESSED' then
    Variant(Dest):=TIPv6VarData(V).VIPv6.AsStringCompressed
  else if Name = 'ASSTRINGOUTWRITTEN' then
    Variant(Dest):=TIPv6VarData(V).VIPv6.AsStringOutwritten
  else
    Result:=inherited GetProperty(Dest, V, Name);
end;

function TIPv6VariantType.LeftPromotion(const V: TVarData;
  const Operator: TVarOp; out RequiredVarType: Word): Boolean;
begin
  if (Operator = opAdd) and VarDataIsStr(V) then
  begin
    RequiredVarType:=V.VType;
    Result:=True;
  end
  else
    Result:=inherited LeftPromotion(V, Operator, RequiredVarType);
end;

function TIPv6VariantType.RightPromotion(const V: TVarData;
  const Operator: TVarOp; out RequiredVarType: Word): Boolean;
begin
  if (Operator = opAdd) and VarDataIsStr(V) then
  begin
    RequiredVarType:=V.VType;
    Result:=True;
  end
  else
    Result:=inherited RightPromotion(V, Operator, RequiredVarType);
end;

function TIPv6VariantType.SetProperty({$IFDEF FPC}var{$ELSE}const{$ENDIF} V: TVarData; const Name: String;
  const Value: TVarData): Boolean;
begin
  Result:=True;
  if Name = 'ASEXTENDED' then
    TIPv6VarData(V).VIPv6.AsExtended:=Variant(Value)
  else if Name = 'ASDOUBLE' then
    TIPv6VarData(V).VIPv6.AsDouble:=Variant(Value)
  else if Name = 'ASSINGLE' then
    TIPv6VarData(V).VIPv6.AsSingle:=Variant(Value)
  else if Name = 'ASSTRING' then
    TIPv6VarData(V).VIPv6.AsString:=Variant(Value)
  else if Name = 'ASSTRINGCOMPRESSED' then
    TIPv6VarData(V).VIPv6.AsStringCompressed:=Variant(Value)
  else if Name = 'ASSTRINGOUTWRITTEN' then
    TIPv6VarData(V).VIPv6.AsStringOutWritten:=Variant(Value)
  else
    Result:=inherited SetProperty(V, Name, Value);
end;

{ IPv6 variant create routines }

procedure VarIPv6Create(var Dest: Variant; const AIPv6: TIPv6); overload;
begin
  VarClear(Dest);
  TIPv6VarData(Dest).VType:=IPv6VariantType.VarType;
  TIPv6VarData(Dest).VIPv6:=TIPv6Object.Create(AIPv6);
end;

function VarIPv6Create: Variant; overload;
begin
  VarIPv6Create(Result, ZeroIPv6);
end;

function VarIPv6Create(const A, B, C, D, E, F, G, H: Word): Variant; overload;
var
  IPv6: TIPv6;
begin
  IPv6.A:=A;
  IPv6.B:=B;
  IPv6.C:=C;
  IPv6.D:=D;
  IPv6.E:=E;
  IPv6.F:=F;
  IPv6.G:=G;
  IPv6.H:=H;
  VarIPv6Create(Result, IPv6);
end;

function VarIPv6Create(const AIPv6: TIPv6): Variant; overload;
begin
  VarIPv6Create(Result, AIPv6);
end;

function VarIPv6Create(const Groups: TIPv6WordArray): Variant; overload;
var
  IPv6: TIPv6;
begin
  IPv6.Groups:=Groups;
  VarIPv6Create(Result, IPv6);
end;

function VarIPv6Create(const S: String): Variant; overload;
begin
  VarIPv6Create(Result, StrToIPv6(S));
end;

{ Utility routines }

procedure CheckCase(var S: String);
begin
  if IPCharCase = ccLowerCase then
    S:=LowerCase(S);
end;

function ReverseIPv4(AValue: TIPv4): TIPv4;
var
  c: Cardinal;
begin
  c:=IPv4ToCardinal(AValue);
  c:=Swap(c shr 16) or (Cardinal(Swap(c and $FFFF)) shl 16);
  Result:=CardinalToIPv4(c);
end;

function IPv4ToInAddr(AValue: TIPv4): in_addr;
begin
  Result.S_un_b.s_b1:=AnsiChar(Chr(AValue.A));
  Result.S_un_b.s_b2:=AnsiChar(Chr(AValue.B));
  Result.S_un_b.s_b3:=AnsiChar(Chr(AValue.C));
  Result.S_un_b.s_b4:=AnsiChar(Chr(AValue.D));
end;

function InAddrToIPv4(AValue: in_addr): TIPv4;
begin
  Result.A:=Ord(AValue.S_un_b.s_b1);
  Result.B:=Ord(AValue.S_un_b.s_b2);
  Result.C:=Ord(AValue.S_un_b.s_b3);
  Result.D:=Ord(AValue.S_un_b.s_b4);
end;

function IsIPv4(AValue: string): Boolean;
var
  ip: TIPv4;
begin
  Result:=TryVarToIPv4(AValue,ip) and (ip.Value<>0);
end;

function CardinalToIPv4(AValue: Cardinal): TIPv4;
begin
  Result.Value:=AValue;
end;

function DoubleToIPv4(const AValue: Double): TIPv4;
begin
  Result.Value:=Round(AValue * High(Cardinal));
end;

function ExtendedToIPv4(const AValue: Extended): TIPv4;
begin
  Result.Value:=Round(AValue * High(Cardinal));
end;

function SingleToIPv4(const AValue: Single): TIPv4;
begin
  Result.Value:=Round(AValue * High(Cardinal));
end;

function StrToIPv4(const S: String): TIPv4;
var
  SIP: String;
  Start: Integer;
  I: T4;
  Index: Integer;
  Count: Integer;
  SGroup: String;
  G: Integer;
begin
  SIP:=S + '.';
  Start:=1;
  for I:=High(T4) downto Low(T4) do
  begin
    Index:=PosEx('.', SIP, Start);
    if Index = 0 then
      IPv4ErrorFmt(SInvalidIPv4Value, S);
    Count:=Index - Start + 1;
    SGroup:=Copy(SIP, Start, Count - 1);
    if TryStrToInt(SGroup, G) and (G >= Low(Word)) and (G <= High(Word)) then
        Result.Groups[I]:=G
      else
        Result.Groups[I]:=0;
    Inc(Start, Count);
  end;
end;

function TryVarToIPv4(const AValue: Variant; out AIPv4: TIPv4): Boolean;
begin
  try
    AIPv4:=VarToIPv4(AValue);
    Result:=True;
  except
    Result:=False;
  end;
end;

function VarIsIPv4(const AValue: Variant): Boolean;
begin
  Result:=VarType(AValue)=IPv4VariantType.VarType;
end;

function VarToIPv4(const AValue: Variant): TIPv4;
begin
  if VarIsIPv4(AValue) then
    Result:=TIPv4VarData(AValue).VIPv4
  else
    case VarType(AValue) of
      varByte:
        Result.Value:=TVarData(AValue).VByte;
      varDouble:
        Result:=DoubleToIPv4(TVarData(AValue).VDouble);
      varLongWord:
        Result.Value:=TVarData(AValue).VLongWord;
      varSingle:
        Result:=SingleToIPv4(TVarData(AValue).VSingle);
      varString,
      {$IFDEF UNICODE}varUString,{$ENDIF}
      varOleStr:
        Result:=StrToIPv4(AValue);
      varWord:
        Result.Value:=TVarData(AValue).VWord;
    else
      IPv4Error(SInvalidIPv4FormatType);
    end;
end;

function IPv4Compare(const AIPv41, AIPv42: TIPv4): Integer;
begin
  if AIPv41.Value = AIPv42.Value then
    Result:=0
  else if AIPv41.Value < AIPv42.Value then
    Result:=-1
  else
    Result:=1;
end;

function SameIPv4(const AIPv41, AIPv42: TIPv4): Boolean;
begin
  Result:=IPV4Compare(AIPv41,AIPv42)=0;
end;

procedure IPv4ToBits(const AIPv4: TIPv4; ABits: TBits);
var
  I: Integer;
begin
  if ABits <> nil then
  begin
    ABits.Size:=IPv4BitSize;
    for I:=0 to IPv4BitSize - 1 do
      ABits[IPv4BitSize - I - 1]:=AIPv4.Value and (1 shl I) <> 0;
  end;
end;

function IPv4ToCardinal(const AIPv4: TIPv4): Cardinal;
begin
  Result:=AIPv4.Value;
end;

function IPv4ToDouble(const AIPv4: TIPv4): Double;
begin
  Result:=AIPv4.Value / High(Cardinal);
end;

function IPv4ToExtended(const AIPv4: TIPv4): Extended;
begin
  Result:=AIPv4.Value / High(Cardinal);
end;

function IPv4ToIPv6(const AIPv4: TIPv4): TIPv6;
begin
  FillChar(Result.E, 5 * SizeOf(Word), 0);
  Result.F:=$FFFF;
  Result.G:=AIPv4.A shl 8 + AIPv4.B;
  Result.H:=AIPv4.C shl 8 + AIPv4.D;
end;

function IPv4ToSingle(const AIPv4: TIPv4): Single;
begin
  Result:=AIPv4.Value / High(Cardinal);
end;

function IPv4ToStr(const AIPv4: TIPv4): String;
begin
  with AIPv4 do
    Result:=Format('%d.%d.%d.%d', [A, B, C, D]);
end;

function IPv4ToStrHex(const AIPv4: TIPv4): String;
begin
  with AIPv4 do
    Result:=Format('%.2x.%.2x.%.2x.%.2x', [A, B, C, D]);
  CheckCase(Result);
end;

function IPv4ToStrOutwr(const AIPv4: TIPv4): String;
begin
  with AIPv4 do
    Result:=Format('%.3d.%.3d.%.3d.%.3d', [A, B, C, D]);
end;

function IPv4ToURL(const AIPv4: TIPv4; const Protocol: String = DefProtocol;
  const PortNumber: Word = DefPortNumber): String;
begin
  Result:=IPv4ToStr(AIPv4);
  if (PortNumber <> DefPortNumber) or (Protocol <> DefProtocol) then
    Result:=Protocol + '://' + Result + ':' + IntToStr(PortNumber) + '/'
  else
    Result:=Protocol + '://' + Result + '/';
end;

function IPv4ToVar(const AIPv4: TIPv4): Variant;
begin
  Result.VType:=IPv4VariantType.VarType;
  TIPv4VarData(Result).VIPv4:=AIPv4;
end;

function DoubleToIPv6(const AValue: Double): TIPv6;
begin
  Result:=ExtendedToIPv6(AValue);
end;

function ExtendedToIPv6(const AValue: Extended): TIPv6;
var
  I: T8;
  OrdValue: Extended;
  G: Extended;
  D: Extended;
begin
  OrdValue:=AValue * IPv6MaxOrdValue;
  D:=Power(High(Word) + 1, High(T8));
  for I:=High(T8) downto Low(T8) do
  begin
    G:=OrdValue / D;
    Result.Groups[I]:=Trunc(G);
    OrdValue:=Frac(G) * D;
    D:=Power(High(Word) + 1, I - 1);
  end;
end;

function SingleToIPv6(const AValue: Single): TIPv6;
begin
  Result:=ExtendedToIPv6(AValue);
end;

function StrToIPv6(const S: String): TIPv6;
{ Valid examples for S:
  2001:0db8:85a3:0000:0000:8a2e:0370:7334
  2001:db8:85a3:0:0:8a2e:370:7334
  2001:db8:85a3::8a2e:370:7334
  ::8a2e:370:7334
  2001:db8:85a3::
  ::1
  ::
  ::ffff:c000:280
  ::ffff:192.0.2.128 }
var
  ZeroPos: Integer;
  DotPos: Integer;
  SIP: String;
  Start: Integer;
  Index: Integer;
  Count: Integer;
  SGroup: String;
  G: Integer;

  procedure NormalNotation;
  var
    I: T8;
  begin
    SIP:=S + ':';
    Start:=1;
    for I:=High(T8) downto Low(T8) do
    begin
      Index:=PosEx(':', SIP, Start);
      if Index = 0 then
        IPv6ErrorFmt(SInvalidIPv6Value, S);
      Count:=Index - Start + 1;
      SGroup:='$' + Copy(SIP, Start, Count - 1);
      if not TryStrToInt(SGroup, G) or (G > High(Word)) or (G < 0) then
        IPv6ErrorFmt(SInvalidIPv6Value, S);
      Result.Groups[I]:=G;
      Inc(Start, Count);
    end;
  end;

  procedure CompressedNotation;
  var
    I: T8;
    A: array of Word;
  begin
    SIP:=S + ':';
    Start:=1;
    I:=High(T8);
    while Start < ZeroPos do
    begin
      Index:=PosEx(':', SIP, Start);
      if Index = 0 then
        IPv6ErrorFmt(SInvalidIPv6Value, S);
      Count:=Index - Start + 1;
      SGroup:='$' + Copy(SIP, Start, Count - 1);
      if not TryStrToInt(SGroup, G) or (G > High(Word)) or (G < 0) then
        IPv6ErrorFmt(SInvalidIPv6Value, S);
      Result.Groups[I]:=G;
      Inc(Start, Count);
      Dec(I);
    end;
    FillChar(Result.H, (I + 1) * SizeOf(Word), 0);
    if ZeroPos < (Length(S) - 1) then
    begin
      SetLength(A, I + 1);
      Start:=ZeroPos + 2;
      repeat
        Index:=PosEx(':', SIP, Start);
        if Index > 0 then
        begin
          Count:=Index - Start + 1;
          SGroup:='$' + Copy(SIP, Start, Count - 1);
          if not TryStrToInt(SGroup, G) or (G > High(Word)) or (G < 0) then
            IPv6ErrorFmt(SInvalidIPv6Value, S);
          A[I]:=G;
          Inc(Start, Count);
          Dec(I);
        end;
      until Index = 0;
      Inc(I);
      Count:=Length(A) - I;
      Move(A[I], Result.H, Count * SizeOf(Word));
    end;
  end;

  procedure DottedQuadNotation;
  var
    I: T4;
  begin
    if UpperCase(Copy(S, ZeroPos + 2, 4)) <> 'FFFF' then
      IPv6ErrorFmt(SInvalidIPv6Value, S);
    FillChar(Result.E, 5 * SizeOf(Word), 0);
    Result.F:=$FFFF;
    SIP:=S + '.';
    Start:=ZeroPos + 7;
    for I:=Low(T4) to High(T4) do
    begin
      Index:=PosEx('.', SIP, Start);
      if Index = 0 then
        IPv6ErrorFmt(SInvalidIPv6Value, S);
      Count:=Index - Start + 1;
      SGroup:=Copy(SIP, Start, Count - 1);
      if not TryStrToInt(SGroup, G) or (G > High(Byte)) or (G < 0) then
        IPv6ErrorFmt(SInvalidIPv6Value, S);
      case I of
        0: Result.G:=G shl 8;
        1: Inc(Result.G, G);
        2: Result.H:=G shl 8;
        3: Inc(Result.H, G);
      end;
      Inc(Start, Count);
    end;
  end;

begin
  ZeroPos:=Pos('::', S);
  if ZeroPos = 0 then
    NormalNotation
  else
  begin
    DotPos:=Pos('.', S);
    if DotPos = 0 then
      CompressedNotation
    else
      DottedQuadNotation;
  end;
end;

function TryVarToIPv6(const AValue: Variant; out AIPv6: TIPv6): Boolean;
begin
  try
    AIPv6:=VarToIPv6(AValue);
    Result:=True;
  except
    Result:=False;
  end;
end;

function VarIsIPv6(const AValue: Variant): Boolean;
begin
  Result:=VarType(AValue) = IPv6VariantType.VarType;
end;

function VarToIPv6(const AValue: Variant): TIPv6;
begin
  if VarIsIPv6(AValue) then
    Result:=TIPv6VarData(AValue).VIPv6.IPv6
  else
    case VarType(AValue) of
      varDouble:
        Result:=DoubleToIPv6(TVarData(AValue).VDouble);
      varOleStr:
        Result:=StrToIPv6(AValue);
      varSingle:
        Result:=SingleToIPv6(TVarData(AValue).VSingle);
      {$IFDEF UNICODE}varUString,{$ENDIF}
      varString:
        Result:=StrToIPv6(String(TVarData(AValue).VString));
    else
      IPv6Error(SInvalidIPv6FormatType);
    end;
end;

function IsIPv6(AValue: string): Boolean;
var
  ip: TIPv6;
begin
  Result:=TryVarToIPv6(AValue,ip) and (ip.H+ip.G+ip.F+ip.E+ip.D+ip.C+ip.B+ip.A<>0);
end;

function IPv6Compare(const AIPv61, AIPv62: TIPv6): Integer;
var
  I: T8;
begin
  Result:=0;
  for I:=High(T8) downto Low(T8) do
    if AIPv61.Groups[I] <> AIPv62.Groups[I] then
    begin
      if AIPv61.Groups[I] < AIPv62.Groups[I] then
        Result:=-1
      else
        Result:=1;
      Break;
    end;
end;

function SameIPv6(const AIPv61, AIPv62: TIPv6): Boolean;
begin
  Result:=IPv6Compare(AIPv61,AIPv62)=0;
end;

procedure IPv6ToBits(const AIPv6: TIPv6; ABits: TBits);
var
  I: Integer;
  GroupBitSize: Integer;
begin
  if ABits <> nil then
  begin
    ABits.Size:=IPv6BitSize;
    GroupBitSize:=IPv6BitSize div Length(AIPv6.Groups);
    for I:=0 to IPv6BitSize - 1 do
      ABits[IPv6BitSize - I - 1] :=
        AIPv6.Groups[I div GroupBitSize] and (1 shl (I mod GroupBitSize)) <> 0;
  end;
end;

function IPv6ToDouble(const AIPv6: TIPv6): Double;
begin
  Result:=IPv6ToExtended(AIPv6);
end;

function IPv6ToExtended(const AIPv6: TIPv6): Extended;
var
  I: T8;
begin
  Result:=0;
  for I:=High(T8) downto Low(T8) do
    Result:=Result * (High(Word) + 1) + AIPv6.Groups[I];
  Result:=Result / IPv6MaxOrdValue;
end;

function IPv6ToIPv4(const AIPv6: TIPv6): TIPv4;
begin
  with AIPv6 do
    if (A > 0) or (B > 0) or (C > 0) or (D > 0) or (E > 0) or (F < $FFFF) then
      IPv6Error(SInvalidIPv6FormatType);
  Move(AIPv6.G, Result, 2 * SizeOf(Word));
end;

function IPv6ToSingle(const AIPv6: TIPv6): Single;
begin
  Result:=IPv6ToExtended(AIPv6);
end;

function IPv6ToStr(const AIPv6: TIPv6): String;
begin
  with AIPv6 do
    Result:=Format('%x:%x:%x:%x:%x:%x:%x:%x', [A, B, C, D, E, F, G, H]);
  CheckCase(Result);
end;

function IPv6ToStrCompr(const AIPv6: TIPv6): String;
var
  Zeroed: Boolean;
  I: T8;
begin
  Result:='';
  Zeroed:=False;
  for I:=High(T8) downto Low(T8) do
  begin
    if AIPv6.Groups[I] = 0 then
    begin
      if (I = Low(T8)) then
        Result:=Result + ':';
      if not Zeroed then
      begin
        Result:=Result + ':';
        Zeroed:=True;
      end;
    end
    else
      if (I = High(T8)) and not Zeroed then
        Result:=Result + Format('%x', [AIPv6.Groups[I]])
      else
        Result:=Result + Format(':%x', [AIPv6.Groups[I]]);
  end;
  CheckCase(Result);
end;

function IPv6ToStrOutwr(const AIPv6: TIPv6): String;
begin
  with AIPv6 do
    Result:=Format('%.4x:%.4x:%.4x:%.4x:%.4x:%.4x:%.4x:%.4x', [A, B, C, D,
      E, F, G, H]);
  CheckCase(Result);
end;

function IPv6ToURL(const AIPv6: TIPv6; const Protocol: String = DefProtocol;
  const PortNumber: Word = DefPortNumber): String;
begin
  Result:=IPv6ToStr(AIPv6);
  CheckCase(Result);
  if (PortNumber <> DefPortNumber) or (Protocol <> DefProtocol) then
    Result:=Protocol + '://[' + Result + ']:' + IntToStr(PortNumber) + '/'
  else
    Result:=Protocol + '://[' + Result + ']/';
end;

function IPv6ToVar(const AIPv6: TIPv6): Variant;
begin
  Result:=VarIPv6Create(AIPv6);
end;

function IPv6IsNull(const AIPv6: TIPv6): Boolean;
begin
  Result:=(AIPv6.H+AIPv6.G+AIPv6.F+AIPv6.E+AIPv6.D+AIPv6.C+AIPv6.B+AIPv6.A=0);
end;

function IPv6ToInAddr(AValue: TIPv6): in6_addr;
begin
end;

function InAddrToIPv6(AValue: in6_addr): TIPv6;
begin
  Result.A:=Swap(AValue.Word[0]);
  Result.B:=Swap(AValue.Word[1]);
  Result.C:=Swap(AValue.Word[2]);
  Result.D:=Swap(AValue.Word[3]);
  Result.E:=Swap(AValue.Word[4]);
  Result.F:=Swap(AValue.Word[5]);
  Result.G:=Swap(AValue.Word[6]);
  Result.H:=Swap(AValue.Word[7]);
end;

{ IPv6 variant binary operation routines }

function IPv6AddOp(const Left, Right: TIPv6): TIPv6;
var
  I: T8;
  Sum: Integer;
  Remain: Word;
begin
  Remain:= 0;
  for I:=Low(T8) to High(T8) do
  begin
    Sum:=Remain + Left.Groups[I] + Right.Groups[I];
    Result.Groups[I]:=Sum mod (High(Word) + 1);
    Remain:=Sum div (High(Word) + 1);
  end;
end;

function IPv6AndOp(const Left, Right: TIPv6): TIPv6;
var
  I: T8;
begin
  for I:=Low(T8) to High(T8) do
    Result.Groups[I]:=Left.Groups[I] and Right.Groups[I];
end;

function IPv6OrOp(const Left, Right: TIPv6): TIPv6;
var
  I: T8;
begin
  for I:=Low(T8) to High(T8) do
    Result.Groups[I]:=Left.Groups[I] or Right.Groups[I];
end;

function IPv6SubtractOp(const Left, Right: TIPv6): TIPv6;
var
  I: T8;
  Sum: Integer;
  Lost: Word;
begin
  Lost:=0;
  for I:=Low(T8) to High(T8) do
  begin
    Sum:=Left.Groups[I] - Right.Groups[I] - Lost;
    if Sum < 0 then
    begin
      Inc(Sum, High(Word) + 1);
      Lost:=1;
    end
    else
      Lost:=0;
    Result.Groups[I]:=Sum;
  end;
  if Lost > 0 then
    Result:=ZeroIPv6;
end;

function IPv6XorOp(const Left, Right: TIPv6): TIPv6;
var
  I: T8;
begin
  for I:=Low(T8) to High(T8) do
    Result.Groups[I]:=Left.Groups[I] xor Right.Groups[I];
end;

initialization
  IPv4VariantType:=TIPv4VariantType.Create;
  IPv6VariantType:=TIPv6VariantType.Create;
  IPv6MaxOrdValue:={$IFDEF WIN64}3.40323907514263E38;{$ELSE}Power(High(Word) + 1, High(T8) + 1) - 1{$ENDIF};

finalization
  FreeAndNil(IPv4VariantType);
  FreeAndNil(IPv6VariantType);
end.
