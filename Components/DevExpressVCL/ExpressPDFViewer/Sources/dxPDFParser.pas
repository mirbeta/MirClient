{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressPDFViewer                                         }
{                                                                    }
{           Copyright (c) 2015-2019 Developer Express Inc.           }
{           ALL RIGHTS RESERVED                                      }
{                                                                    }
{   The entire contents of this file is protected by U.S. and        }
{   International Copyright Laws. Unauthorized reproduction,         }
{   reverse-engineering, and distribution of all or any portion of   }
{   the code contained in this file is strictly prohibited and may   }
{   result in severe civil and criminal penalties and will be        }
{   prosecuted to the maximum extent possible under the law.         }
{                                                                    }
{   RESTRICTIONS                                                     }
{                                                                    }
{   THIS SOURCE CODE AND ALL RESULTING INTERMEDIATE FILES            }
{   (DCU, OBJ, DLL, ETC.) ARE CONFIDENTIAL AND PROPRIETARY TRADE     }
{   SECRETS OF DEVELOPER EXPRESS INC. THE REGISTERED DEVELOPER IS    }
{   LICENSED TO DISTRIBUTE THE EXPRESSPDFVIEWER AND ALL              }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM       }
{   ONLY.                                                            }
{                                                                    }
{   THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED       }
{   FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE         }
{   COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE        }
{   AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT   }
{   AND PERMISSION FROM DEVELOPER EXPRESS INC.                       }
{                                                                    }
{   CONSULT THE END USER LICENSE AGREEMENT FOR INFORMATION ON        }
{   ADDITIONAL RESTRICTIONS.                                         }
{                                                                    }
{********************************************************************}

unit dxPDFParser;

{$I cxVer.inc}

interface

uses
  SysUtils, Classes, Generics.Defaults, Generics.Collections, dxHashUtils, dxPDFStreamFilter, dxPDFBase, dxPDFTypes,
  dxPDFUtils;

type
  { TdxPDFBaseParser }

  TdxPDFBaseParser = class(TdxPDFReferencedObject)
  strict private
    FCurrent: Byte;
    FCurrentPosition: Int64;

    FData: TBytes;
    FDataLength: Integer;
    FStoredCurrentPosition: Int64;

    FBooleanTrueToken: TdxPDFToken;
    FBooleanFalseToken: TdxPDFToken;
    FNullToken: TdxPDFToken;

    procedure SetCurrentPosition(const AValue: Int64);
    procedure SetData(const AValue: TBytes);

    function CreateBooleanObject(AToken: TdxPDFToken; AValue: Boolean): TdxPDFBoolean;
    function GetName: string;
    function DoReadArray: TdxPDFArray; inline;
    function DoReadBooleanObject(AToken: TdxPDFToken; ADefaultValue: Boolean;
      AIsHexStrSeparatedByWhiteSpaces: Boolean): TdxPDFBase; inline;
    function DoReadComment: TdxPDFComment;
    function DoReadName: TdxPDFName; inline;
    function DoReadNames: TdxPDFArray; inline;
    function DoReadNumericObject(AIsHexStrSeparatedByWhiteSpaces: Boolean): TdxPDFBase; inline;
    procedure InitializeData(const AData: TBytes; APosition: Int64);
    procedure PopulateEscapeStringSequence(var AData: TBytes);
    procedure ReadNumberSign(out AIsNegative, AHasSign: Boolean);
    procedure SkipLineFeed;
  protected
    function CreateDefaultCompositeObject: TdxPDFBase; virtual;
    function CreateDictionary: TdxPDFDictionary; virtual;
    function CreateStream(ADictionary: TdxPDFDictionary): TdxPDFStream; virtual;
    function DecryptString(const AData: TBytes): string; virtual;
    function DoReadString: TdxPDFString; virtual;
    function DoSkipSpaces: Boolean; virtual;
    function ReadCompositeObject(AIsHexStrSeparatedByWhiteSpaces: Boolean): TdxPDFBase; virtual;
    function ReadNumericObject: TdxPDFBase; virtual;
    procedure Initialize(const AData: TBytes; APosition: Int64); virtual;
    procedure InitializeTokens; virtual;
    procedure Finalize; virtual;
    procedure FinalizeTokens; virtual;

    function CanContinueReading: Boolean; virtual;
    function CanContinueTokenReading: Boolean; virtual;
    function CanReadObject: Boolean;
    function DoReadDictionary: TdxPDFDictionary;
    function DoReadNumber: TdxPDFBase; inline;
    function DoReadObject(AIsHexStrSeparatedByWhiteSpaces: Boolean = False): TdxPDFBase;
    function DoReadToken: string;
    function GetHexDigit: Byte; inline;
    function IsDigit: Boolean;
    function IsHexDigit: Boolean;
    function IsWhiteSpace: Boolean;
    function NeedReadObjectAfterComment: Boolean; virtual;
    function ReadArray(const AData: TBytes): TdxPDFArray; inline;
    function ReadNames(const AData: TBytes): TdxPDFArray; inline;
    function ReadNext: Boolean; inline;
    function ReadNumber(const AData: TBytes): TdxPDFNumericObject; inline;
    function ReadPrev: Boolean; inline;
    function ReadToken(const AToken: TBytes): Boolean; inline;
    procedure SaveCurrentPosition;
    function SkipSpaces(AIgnoreComment: Boolean = True): Boolean; inline;
    procedure RestoreCurrentPosition;

    property Current: Byte read FCurrent;
    property CurrentPosition: Int64 read FCurrentPosition write SetCurrentPosition;
    property Data: TBytes read FData write SetData;
    property DataLength: Integer read FDataLength;
  public
    constructor Create; overload; virtual;
    constructor Create(const AData: TBytes; APosition: Int64); overload;

    destructor Destroy; override;

    function ReadDictionary(const AData: TBytes): TdxPDFDictionary;
    function ReadObject(const AData: TBytes; APosition: Integer = 0): TdxPDFBase;
  end;

  { TdxPDFStructureParser }

  TdxPDFStructureParser = class(TdxPDFBaseParser)
  strict private
    FEndStreamToken: TdxPDFToken;
    FStreamToken: TdxPDFToken;

    procedure SetRepository(const AValue: TdxPDFCustomRepository);

    function DoReadStream(ADictionary: TdxPDFDictionary): TdxPDFStream; overload;
    function DoReadStream(ALength: Integer): TdxPDFStream; overload;
    function DoReadStreamToken: Boolean;
    procedure MoveToStreamDataStartPosition;
    procedure PopulateStreamData(AStream: TdxPDFStream; ALength: Integer);
  private
    FRepository: TdxPDFCustomRepository;
  protected
    function CreateDictionary: TdxPDFDictionary; override;
    function CreateStream(ADictionary: TdxPDFDictionary): TdxPDFStream; override;
    function ReadCompositeObject(AIsHexStrSeparatedByWhiteSpaces: Boolean): TdxPDFBase; override;
    function ReadNumericObject: TdxPDFBase; override;
    procedure InitializeTokens; override;
    procedure FinalizeTokens; override;

    function GetEOF: Boolean; virtual;
    function ReadHexadecimalString(AIsHexStrSeparatedByWhiteSpaces: Boolean = False): TdxPDFString; virtual;

    function DoReadData(AToken: TdxPDFToken; AIgnoreWhiteSpace: Boolean = False; ASupportBracket: Boolean = False): TBytes; overload;
    function DoReadData(ALength: Integer; AToken: TdxPDFToken): TBytes; overload;
    function DoReadDictionaryOrStream: TdxPDFBase;
    function ReadStream(const AData: TBytes; ALength: Integer): TdxPDFStream; overload;
    function ReadStream(ADictionary: TdxPDFDictionary): TdxPDFStream; overload;

    property Repository: TdxPDFCustomRepository read FRepository write SetRepository;
  public
    constructor Create(ARepository: TdxPDFCustomRepository); virtual;
    property IsEOF: Boolean read GetEOF;
  end;

  { TdxPDFEncryptedStructureParser }

  TdxPDFEncryptedStructureParser = class(TdxPDFStructureParser)
  strict private
    FNumber: Integer;
  protected
    function DecryptString(const AData: TBytes): string; override;
  public
    constructor Create(ARepository: TdxPDFCustomRepository; ANumber: Integer); reintroduce;
  end;

  { TdxPDFObjectParser }

  TdxPDFObjectParser = class(TdxPDFStructureParser)
  private
    FStream: TStream;
    FCheckObjToken: TdxPDFToken;

  protected
    function GetEOF: Boolean; override;
    procedure InitializeTokens; override;
    procedure FinalizeTokens; override;
  public
    constructor Create(ARepository: TdxPDFCustomRepository; AStream: TStream); reintroduce;

    function CanReadNumber(AOffset: Int64): Boolean;
    function ReadToken(AToken: TdxPDFToken): Boolean;
    function ReadByte: Byte; inline;
    function ReadObject(AOffset: Int64): TdxPDFBase; overload;
    function ReadIndirectObject(AOffset: Int64): TdxPDFIndirectObject;
    function ReadIndirectObjectNumber(AOffset: Int64): Integer;
    function ReadNumber: Integer;
    function ReadObjectData(AOffset: Int64; out ANumber, AGeneration: Integer): TBytes;
    function ReadString: string;
    procedure SkipSpaces(out ASymbol: Byte); overload;
    function IsObjectValid(AReference: TdxPDFReference): Boolean;
  end;

  { TdxPDFDocumentParser }

  TdxPDFDocumentParser = class(TdxPDFObjectParser)
  type
    TdxPDFCrossReferenceIndexDescription = record
      StartNumber: Integer;
      Count: Integer;
    end;
  strict private const
    StartXRefOffset = 30;
    RequiredStartXRefSpace = 50;
  strict private
    FPositionOffset: Int64;
    FStartPosition: Int64;
    FStartXRefObjectToken: TdxPDFToken;

    function ReadValue(const AData: TBytes; AWeight: Integer; var APosition: Int64): Integer;
    procedure Decode(const AData: TBytes; AIndexes: TList<TdxPDFCrossReferenceIndexDescription>;
      ATypeSize, AOffsetSize, AGenerationSize: Integer);

    function ReadNumber(ADigitCount: Integer): Int64; overload;
    procedure ReadCrossReferenceRecords(AStartNumber, ACount: Integer);

    function DoReadStartXRefObject: Integer;
    function GetStartXRefObjectOffset: Int64;
    function ReadCorruptedStartXRefObjectOffset: Int64;
    function ReadCrossReferences(AOffset: Integer): Boolean;
    function ReadObjectSlot(AOffset: Int64): TdxPDFReference;
    function ReadStartXRefObject(const AData: TBytes): Integer;
    procedure ReadTrailerStream(AOffset: Int64; out ATrailer: TdxPDFDictionary);

    procedure PopulateIndexes(ATrailer: TdxPDFDictionary; AIndexes: TList<TdxPDFCrossReferenceIndexDescription>);
  protected
    procedure InitializeTokens; override;
    procedure FinalizeTokens; override;
  public
    function ReadCrossReferencesOffset: Int64;

    function FindToken(const ATokenDescription: string): Boolean; overload;
    function FindToken(AToken: TdxPDFToken): Boolean; overload;
    function ParseVersion(const AHeader: string): TdxPDFVersion;
    function ReadTrailerData: TBytes;
    procedure CalculateHeaderPosition;
    procedure FindObjects;
    procedure ReadVersion(out AVersion: TdxPDFVersion);
    procedure ReadTrailer(out ATrailer: TdxPDFDictionary); overload;
    procedure ReadTrailer(AOffset: Int64; out ATrailer: TdxPDFDictionary); overload;
    procedure SaveStartPosition;

    property Repository;
    property StartPosition: Int64 read FStartPosition write FStartPosition;
  end;

implementation

uses
  Variants, Math, Character, StrUtils, dxCore, dxPDFCore;

type
  TdxPDFStreamAccess = class(TdxPDFStream);

{ TdxPDFBaseParser }

constructor TdxPDFBaseParser.Create;
begin
  inherited Create;
  Initialize(FData, 0);
end;

constructor TdxPDFBaseParser.Create(const AData: TBytes; APosition: Int64);
begin
  inherited Create;
  Initialize(AData, APosition);
end;

destructor TdxPDFBaseParser.Destroy;
begin
  Finalize;
  SetLength(FData, 0);
  inherited Destroy;
end;

function TdxPDFBaseParser.ReadDictionary(const AData: TBytes): TdxPDFDictionary;
begin
  Data := AData;
  Result := DoReadDictionary;
end;

function TdxPDFBaseParser.ReadObject(const AData: TBytes; APosition: Integer = 0): TdxPDFBase;
begin
  Data := AData;
  CurrentPosition := APosition;
  Result := DoReadObject;
end;

function TdxPDFBaseParser.CreateDefaultCompositeObject: TdxPDFBase;
begin
  Result := TdxPDFString.Create(DoReadToken);
end;

function TdxPDFBaseParser.CreateDictionary: TdxPDFDictionary;
begin
  Result := TdxPDFDictionary.Create;
end;

function TdxPDFBaseParser.CreateStream(ADictionary: TdxPDFDictionary): TdxPDFStream;
begin
  Result := TdxPDFStream.Create(ADictionary);
end;

function TdxPDFBaseParser.DecryptString(const AData: TBytes): string;
begin
  Result := TdxPDFUtils.BytesToStr(AData);
end;

function TdxPDFBaseParser.DoReadString: TdxPDFString;
var
  AEntryCount: Integer;
  AData: TBytes;
begin
  Result := nil;
  if Current <> TdxPDFDefinedSymbols.StartString then
    TdxPDFUtils.Abort;
  AEntryCount := 1;
  while ReadNext do
    case Current of
      TdxPDFDefinedSymbols.Backslash:
        PopulateEscapeStringSequence(AData);
      TdxPDFDefinedSymbols.CarriageReturn:
        begin
          TdxPDFUtils.AddByte(TdxPDFDefinedSymbols.LineFeed, AData);
          SkipLineFeed;
        end;
      TdxPDFDefinedSymbols.EndString:
        begin
          Dec(AEntryCount);
          if AEntryCount = 0 then
          begin
            ReadNext;
            Result := TdxPDFString.Create(DecryptString(AData));
            Break;
          end
          else
            TdxPDFUtils.AddByte(Current, AData);
        end;
      TdxPDFDefinedSymbols.StartString:
        begin
          Inc(AEntryCount);
          TdxPDFUtils.AddByte(Current, AData);
        end;
      else
        TdxPDFUtils.AddByte(Current, AData);
    end;
  SetLength(AData, 0);
  if Result = nil then
    Result := TdxPDFString.Create('');
end;

function TdxPDFBaseParser.DoSkipSpaces: Boolean;
begin
  Result := SkipSpaces;
end;

function TdxPDFBaseParser.ReadCompositeObject(AIsHexStrSeparatedByWhiteSpaces: Boolean): TdxPDFBase;
begin
  Result := CreateDefaultCompositeObject;
end;

function TdxPDFBaseParser.ReadNumericObject: TdxPDFBase;
begin
  Result := DoReadNumber;
end;

procedure TdxPDFBaseParser.Initialize(const AData: TBytes; APosition: Int64);
begin
  InitializeData(AData, APosition);
  InitializeTokens;
end;

procedure TdxPDFBaseParser.InitializeTokens;
begin
  FBooleanTrueToken := TdxPDFToken.Create(TdxPDFUtils.StrToByteArray('true'));
  FBooleanFalseToken := TdxPDFToken.Create(TdxPDFUtils.StrToByteArray('false'));
  FNullToken := TdxPDFToken.Create(TdxPDFUtils.StrToByteArray('null'));
end;

procedure TdxPDFBaseParser.Finalize;
begin
  FinalizeTokens;
end;

procedure TdxPDFBaseParser.FinalizeTokens;
begin
  FreeAndNil(FNullToken);
  FreeAndNil(FBooleanFalseToken);
  FreeAndNil(FBooleanTrueToken);
end;

function TdxPDFBaseParser.CanContinueReading: Boolean;
begin
  Result := not IsWhiteSpace and not (Current in [TdxPDFDefinedSymbols.Comment,
    TdxPDFDefinedSymbols.NameIdentifier, TdxPDFDefinedSymbols.StartString,
    TdxPDFDefinedSymbols.EndString, TdxPDFDefinedSymbols.StartArray, TdxPDFDefinedSymbols.EndArray]);
end;

function TdxPDFBaseParser.CanContinueTokenReading: Boolean;
begin
  Result := CanContinueReading;
end;

function TdxPDFBaseParser.CanReadObject: Boolean;
begin
  Result := CurrentPosition < FDataLength;
  if Result then
    while IsWhiteSpace do
    begin
      Result := not ReadNext;
      if Result then
        Break;
    end;
end;

function TdxPDFBaseParser.DoReadDictionary: TdxPDFDictionary;
var
  AName: TdxPDFName;
  ANamedObject: TdxPDFBase;
begin
  Result := nil;
  SkipSpaces;
  if (Current = TdxPDFDefinedSymbols.StartObject) and ReadNext and (Current = TdxPDFDefinedSymbols.StartObject) and ReadNext then
  begin
    Result := CreateDictionary;
    while SkipSpaces do
      case Current of
        TdxPDFDefinedSymbols.NameIdentifier:
          begin
            AName := DoReadName;
            if not SkipSpaces then
            begin
              dxPDFFreeObject(AName);
              TdxPDFUtils.Abort;
            end
            else
            begin
              ANamedObject := DoReadObject(AName.Value = 'Panose');
              if ANamedObject <> nil then
                Result.Add(AName.Value, ANamedObject);
            end;
            dxPDFFreeObject(AName);
          end;
        TdxPDFDefinedSymbols.EndObject:
          begin
            if ReadNext or (Current = TdxPDFDefinedSymbols.EndObject) then
              ReadNext
            else
              TdxPDFUtils.Abort;
            Break;
          end;
      else
        Break;
      end;
  end;
end;

function TdxPDFBaseParser.DoReadNumber: TdxPDFBase;
var
  AIsNegative, AHasSign: Boolean;
  AFraction: Double;
  AValue, APosition, APow: Integer;
  AExponent: TdxPDFBase;
begin
  ReadNumberSign(AIsNegative, AHasSign);

  AValue := 0;
  if IsDigit then
    for APosition := CurrentPosition to FDataLength - 1 do
    begin
      AValue := AValue * 10 + GetHexDigit;
      if not ReadNext or not IsDigit then
        Break;
    end;

  if AIsNegative then
    AValue := -AValue;
  if not (Current in [TdxPDFDefinedSymbols.Period, Byte(',')]) then
    Exit(TdxPDFInteger.Create(AValue));

  if not ReadNext then
    Exit(TdxPDFDouble.Create(AValue));

  if (Current = TdxPDFDefinedSymbols.Minus) and not AHasSign then
    if ReadNext and IsDigit then
    begin
      AIsNegative := True;
      AValue := -AValue;
    end
    else
    begin
      ReadPrev;
      Exit(TdxPDFDouble.Create(AValue));
    end;
  SaveCurrentPosition;
  AFraction := 0;
  APow := 10;
  while IsDigit do
  begin
    AFraction := AFraction +  GetHexDigit / APow;
    APow := APow * 10;
    if not ReadNext then
      Break;
  end;
  if AIsNegative then
    AFraction := -AFraction;
  if Current <> Byte('E') then
    Exit(TdxPDFDouble.Create(AValue + AFraction));
  if not ReadNext then
    TdxPDFUtils.Abort;
  Result := nil;
  AExponent := DoReadNumber;
  if (AExponent = nil) or (AExponent <> nil) and (AExponent.ObjectType <> otInteger) then
    TdxPDFUtils.Abort
  else
    try
      Result := TdxPDFDouble.Create(AValue + AFraction / Power(10, TdxPDFInteger(AExponent).Value));
    finally
      dxPDFFreeObject(AExponent);
    end;
end;

function TdxPDFBaseParser.DoReadObject(AIsHexStrSeparatedByWhiteSpaces: Boolean = False): TdxPDFBase;
var
  AStringBuilder: TStringBuilder;
begin
  Result := nil;

  if Current = TdxPDFDefinedSymbols.Comment then
  begin
    AStringBuilder := TStringBuilder.Create;
    try
      while ReadNext and not (Current in [TdxPDFDefinedSymbols.CarriageReturn, TdxPDFDefinedSymbols.LineFeed]) do
        AStringBuilder.Append(Char(Current));
      Result := TdxPDFComment.Create(AStringBuilder.ToString);
    finally
      AStringBuilder.Free;
    end;
    ReadNext;
  end;

  if (Result = nil) or NeedReadObjectAfterComment then
  begin
    DoSkipSpaces;
    if CanReadObject then
    begin
      dxPDFFreeObject(Result);
      if IsDigit then
        Result := DoReadNumericObject(AIsHexStrSeparatedByWhiteSpaces)
      else
        case Current of
          TdxPDFDefinedSymbols.NameIdentifier:
            Result := DoReadName;
          TdxPDFDefinedSymbols.Comment:
            Result := DoReadComment;
          TdxPDFDefinedSymbols.Plus, TdxPDFDefinedSymbols.Minus, TdxPDFDefinedSymbols.Period:
            Result := DoReadNumericObject(AIsHexStrSeparatedByWhiteSpaces);
          TdxPDFDefinedSymbols.StartArray:
            Result := DoReadArray;
          TdxPDFDefinedSymbols.StartString:
            Result := DoReadString;
          Byte('t'):
            Result := DoReadBooleanObject(FBooleanTrueToken, True, AIsHexStrSeparatedByWhiteSpaces);
          Byte('f'):
            Result := DoReadBooleanObject(FBooleanFalseToken, False, AIsHexStrSeparatedByWhiteSpaces);
          Byte('n'):
            begin
              if ReadToken(FNullToken.Sequence) then
                Exit(TdxPDFNull.Create)
              else
                ReadPrev;
              Result := ReadCompositeObject(AIsHexStrSeparatedByWhiteSpaces);
            end;
        else
          Result := ReadCompositeObject(AIsHexStrSeparatedByWhiteSpaces);
        end;
    end;
  end;
end;

function TdxPDFBaseParser.DoReadToken: string;
var
  ABuilder: TStringBuilder;
begin
  ABuilder := TStringBuilder.Create;
  try
    repeat
      if not CanContinueTokenReading then
        Break;
      if Current = TdxPDFDefinedSymbols.EndObject then
      begin
        ABuilder.Append(Char(Current));
        ReadNext;
        Break;
      end;
      ABuilder.Append(Char(Current));
    until not ReadNext;
    Result := ABuilder.ToString;
  finally
    ABuilder.Free;
  end;
end;

function TdxPDFBaseParser.GetHexDigit: Byte;
begin
  Result := TdxPDFUtils.ByteToHexDigit(Current);
end;

function TdxPDFBaseParser.IsWhiteSpace: Boolean;
begin
  Result := TdxPDFUtils.IsWhiteSpace(FCurrent);
end;

function TdxPDFBaseParser.NeedReadObjectAfterComment: Boolean;
begin
  Result := True;
end;

function TdxPDFBaseParser.IsDigit: Boolean;
begin
  Result := TdxPDFUtils.IsDigit(FCurrent);
end;

function TdxPDFBaseParser.IsHexDigit: Boolean;
begin
  Result := TdxPDFUtils.IsHexDigit(FCurrent);
end;

function TdxPDFBaseParser.ReadArray(const AData: TBytes): TdxPDFArray;
begin
  Data := AData;
  Result := DoReadArray;
end;

function TdxPDFBaseParser.ReadNames(const AData: TBytes): TdxPDFArray;
begin
  Data := AData;
  Result := DoReadNames;
end;

function TdxPDFBaseParser.ReadNext: Boolean;
begin
  Inc(FCurrentPosition);
  Result := CurrentPosition < FDataLength;
  if Result then
    FCurrent := FData[CurrentPosition];
end;

function TdxPDFBaseParser.ReadNumber(const AData: TBytes): TdxPDFNumericObject;
begin
  Data := AData;
  Result := DoReadNumber as TdxPDFNumericObject;
end;

function TdxPDFBaseParser.ReadPrev: Boolean;
begin
  Result := CurrentPosition > 0;
  if Result then
  begin
    Dec(FCurrentPosition);
    FCurrent := Data[CurrentPosition];
  end;
end;

function TdxPDFBaseParser.ReadToken(const AToken: TBytes): Boolean;
var
  ASymbol: Byte;
begin
  if not SkipSpaces then
    Exit(False);
  Result := False;
  for ASymbol in AToken do
  begin
    if Result or (Current <> ASymbol) then
      Exit(False);
    Result := not ReadNext;
  end;
  Result := True;
end;

procedure TdxPDFBaseParser.SaveCurrentPosition;
begin
  FStoredCurrentPosition := CurrentPosition;
end;

function TdxPDFBaseParser.SkipSpaces(AIgnoreComment: Boolean = True): Boolean;
begin
  Result := False;
  if CurrentPosition < FDataLength then
    while True do
      if not IsWhiteSpace then
      begin
        if AIgnoreComment and(Current = TdxPDFDefinedSymbols.Comment) then
          repeat
            if not ReadNext then
              Exit(False)
          until Current in [TdxPDFDefinedSymbols.CarriageReturn, TdxPDFDefinedSymbols.LineFeed]
        else
          Exit(True);
      end
      else
        if not ReadNext then
          Exit(False);
end;

procedure TdxPDFBaseParser.RestoreCurrentPosition;
begin
  CurrentPosition := FStoredCurrentPosition;
end;

procedure TdxPDFBaseParser.SetCurrentPosition(const AValue: Int64);
begin
  FCurrentPosition := AValue;
  if (FDataLength > 0) and (FCurrentPosition < FDataLength) then
    FCurrent := FData[FCurrentPosition];
end;

procedure TdxPDFBaseParser.SetData(const AValue: TBytes);
begin
  InitializeData(AValue, 0);
end;

function TdxPDFBaseParser.CreateBooleanObject(AToken: TdxPDFToken; AValue: Boolean): TdxPDFBoolean;
begin
  Result := nil;
  if ReadToken(AToken.Sequence) then
    Result := TdxPDFBoolean.Create(AValue)
  else
    ReadPrev;
end;

function TdxPDFBaseParser.GetName: string;
var
  ABuilder: TStringBuilder;
  ALowByte, AHighByte: Byte;
  AFinished: Boolean;
begin
  AFinished := False;
  ABuilder := TStringBuilder.Create;
  try
    while not AFinished and CanContinueReading and (Current <> TdxPDFDefinedSymbols.EndObject) and
      (Current <> TdxPDFDefinedSymbols.StartObject) do
    begin
      if Current = TdxPDFDefinedSymbols.NumberSign then
        if not (ReadNext and IsHexDigit) then
          ABuilder.Append(Char(TdxPDFDefinedSymbols.NumberSign))
        else
        begin
          ALowByte := Current;
          AHighByte := Byte(GetHexDigit * 16);
          if not (ReadNext and IsHexDigit) then
          begin
            ABuilder.Append(Char(TdxPDFDefinedSymbols.NumberSign));
            ABuilder.Append(Char(ALowByte));
          end
          else
            ABuilder.Append(Char(AHighByte + GetHexDigit));
        end
      else
        ABuilder.Append(Char(Current));
      if not ReadNext then
        AFinished := True;
    end;
    Result := ABuilder.ToString;
  finally
    ABuilder.Free;
  end;
end;

function TdxPDFBaseParser.DoReadArray: TdxPDFArray;
var
  ACurrentObject: TdxPDFBase;
begin
  Result := nil;
  if (Current = TdxPDFDefinedSymbols.StartArray) and ReadNext then
  begin
    Result := TdxPDFArray.Create;
    while SkipSpaces do
    begin
      if Current = TdxPDFDefinedSymbols.EndArray then
      begin
        ReadNext;
        Break;
      end;
      ACurrentObject := DoReadObject;
      if ACurrentObject <> nil then
        Result.Add(ACurrentObject)
      else
        Break;
    end
  end
  else
    TdxPDFUtils.Abort;
end;

function TdxPDFBaseParser.DoReadBooleanObject(AToken: TdxPDFToken; ADefaultValue: Boolean;
  AIsHexStrSeparatedByWhiteSpaces: Boolean): TdxPDFBase;
begin
  Result := CreateBooleanObject(AToken, ADefaultValue);
  if Result = nil then
    Result := ReadCompositeObject(AIsHexStrSeparatedByWhiteSpaces);
end;

function TdxPDFBaseParser.DoReadComment: TdxPDFComment;
var
  ABuilder: TStringBuilder;
begin
  ABuilder := TStringBuilder.Create;
  try
    while ReadNext and not (Current in [TdxPDFDefinedSymbols.CarriageReturn, TdxPDFDefinedSymbols.LineFeed]) do
      ABuilder.Append(Char(FCurrent));
    Result := TdxPDFComment.Create(ABuilder.ToString);
  finally
    ABuilder.Free;
  end;
end;

function TdxPDFBaseParser.DoReadName: TdxPDFName;
begin
  Result := nil;
  if (Current = TdxPDFDefinedSymbols.NameIdentifier) and ReadNext then
    Result := TdxPDFName.Create(GetName)
  else
    TdxPDFUtils.Abort;
end;

function TdxPDFBaseParser.DoReadNames: TdxPDFArray;
begin
  Result := TdxPDFArray.Create;
  while SkipSpaces do
    Result.Add(DoReadName);
end;

function TdxPDFBaseParser.DoReadNumericObject(AIsHexStrSeparatedByWhiteSpaces: Boolean): TdxPDFBase;
begin
  Result := ReadNumericObject;
  if Result = nil then
    Result := ReadCompositeObject(AIsHexStrSeparatedByWhiteSpaces);
end;

procedure TdxPDFBaseParser.InitializeData(const AData: TBytes; APosition: Int64);
begin
  SetLength(FData, 0);
  FData := AData;
  FDataLength := Length(FData);
  SetCurrentPosition(APosition);
end;

procedure TdxPDFBaseParser.PopulateEscapeStringSequence(var AData: TBytes);
var
  I: Integer;
  ACharacterCode: Byte;
begin
  if not ReadNext then
    TdxPDFUtils.Abort;
  if IsDigit then
  begin
    ACharacterCode := GetHexDigit;
    for I := 0 to 1 do
    begin
      if not ReadNext then
        TdxPDFUtils.Abort;
      if IsDigit then
        ACharacterCode := (ACharacterCode shl 3 + GetHexDigit) and $FF
      else
        ReadPrev;
    end;
    TdxPDFUtils.AddByte(ACharacterCode, AData);
  end
  else
    case Current of
      Byte('n'):
        TdxPDFUtils.AddByte(TdxPDFDefinedSymbols.LineFeed, AData);
      Byte('r'):
        TdxPDFUtils.AddByte(TdxPDFDefinedSymbols.CarriageReturn, AData);
      Byte('t'):
        TdxPDFUtils.AddByte(TdxPDFDefinedSymbols.HorizontalTab, AData);
      Byte('b'):
        TdxPDFUtils.AddByte(TdxPDFDefinedSymbols.Backspace, AData);
      Byte('f'):
        TdxPDFUtils.AddByte(TdxPDFDefinedSymbols.FormFeed, AData);
      TdxPDFDefinedSymbols.CarriageReturn:
        SkipLineFeed;
      TdxPDFDefinedSymbols.LineFeed:
    else
      TdxPDFUtils.AddByte(Current, AData);
    end;
end;

procedure TdxPDFBaseParser.ReadNumberSign(out AIsNegative, AHasSign: Boolean);
begin
  case Current of
    TdxPDFDefinedSymbols.Minus:
      begin
        AIsNegative := True;
        AHasSign := True;
        if not ReadNext or (Current = TdxPDFDefinedSymbols.Minus) and not ReadNext then
          TdxPDFUtils.Abort;
      end;
    TdxPDFDefinedSymbols.Plus:
      begin
        AIsNegative := False;
        AHasSign := True;
        if not ReadNext then
          TdxPDFUtils.Abort;
      end;
    else
      AIsNegative := False;
      AHasSign := False;
  end;
end;

procedure TdxPDFBaseParser.SkipLineFeed;
begin
  if not ReadNext then
    TdxPDFUtils.Abort;
  if Current <> TdxPDFDefinedSymbols.LineFeed then
    ReadPrev;
end;

{ TdxPDFStructureParser }

constructor TdxPDFStructureParser.Create(ARepository: TdxPDFCustomRepository);
begin
  inherited Create;
  FRepository := ARepository;
end;

function TdxPDFStructureParser.CreateDictionary: TdxPDFDictionary;
begin
  Result := TdxPDFReaderDictionary.Create(Repository as TdxPDFDocumentRepository);
end;

function TdxPDFStructureParser.CreateStream(ADictionary: TdxPDFDictionary): TdxPDFStream;
var
  AEncryptionInfo: IdxPDFEncryptionInfo;
begin
  Result := inherited CreateStream(ADictionary);
  if Supports((Repository as TdxPDFDocumentRepository).EncryptionInfo, IdxPDFEncryptionInfo, AEncryptionInfo) then
    TdxPDFStreamAccess(Result).EncryptionInfo := AEncryptionInfo;
end;

function TdxPDFStructureParser.ReadCompositeObject(AIsHexStrSeparatedByWhiteSpaces: Boolean): TdxPDFBase;
var
  AIsReadingDictionary: Boolean;
begin
  Result := nil;
  if (Current = TdxPDFDefinedSymbols.StartObject) and ReadNext then
  begin
    AIsReadingDictionary := Current = TdxPDFDefinedSymbols.StartObject;
    ReadPrev;
    if AIsReadingDictionary then
      Result := DoReadDictionaryOrStream
    else
      Result := ReadHexadecimalString(AIsHexStrSeparatedByWhiteSpaces);
  end;
end;

function TdxPDFStructureParser.ReadNumericObject: TdxPDFBase;

  function IsIndirectReference(ACurrentObject: TdxPDFNumericObject): Boolean;
  begin
    Result := (ACurrentObject.ObjectType = otInteger) and SkipSpaces and (Current = Byte('R')) and
      not (ReadNext and CanContinueReading and not (Current in [TdxPDFDefinedSymbols.StartObject, TdxPDFDefinedSymbols.EndObject]));
  end;

var
  ANumber: Integer;
  ATempNumeric: TdxPDFNumericObject;
  APosition: Int64;
begin
  ATempNumeric := DoReadNumber as TdxPDFNumericObject;
  if ATempNumeric.ObjectType = otInteger then
  begin
    ANumber := TdxPDFInteger(ATempNumeric).Value;
    if SkipSpaces and IsDigit then
    begin
      ATempNumeric.Free;
      APosition := CurrentPosition;
      ATempNumeric := DoReadNumber as TdxPDFNumericObject;
      if not IsIndirectReference(ATempNumeric) then
      begin
        CurrentPosition := APosition;
        Result := TdxPDFInteger.Create(ANumber);
      end
      else
        Result := TdxPDFReference.Create(ANumber, TdxPDFInteger(ATempNumeric).Value);
      ATempNumeric.Free;
    end
    else
      Result := ATempNumeric;
  end
  else
    Result := ATempNumeric;
end;

function TdxPDFStructureParser.ReadStream(const AData: TBytes; ALength: Integer): TdxPDFStream;
begin
  Data := AData;
  Result := DoReadStream(ALength);
end;

function TdxPDFStructureParser.ReadStream(ADictionary: TdxPDFDictionary): TdxPDFStream;
begin
  Result := DoReadStream(ADictionary);
end;

procedure TdxPDFStructureParser.InitializeTokens;
begin
  inherited InitializeTokens;
  FStreamToken := TdxPDFToken.Create(TdxPDFUtils.StrToByteArray('stream'));
  FEndStreamToken := TdxPDFToken.Create(TdxPDFUtils.StrToByteArray('endstream'));
end;

procedure TdxPDFStructureParser.FinalizeTokens;
begin
  FreeAndNil(FEndStreamToken);
  FreeAndNil(FStreamToken);
  inherited FinalizeTokens;
end;

function TdxPDFStructureParser.GetEOF: Boolean;
begin
  Result := CurrentPosition >= Length(Data);
end;

function TdxPDFStructureParser.ReadHexadecimalString(AIsHexStrSeparatedByWhiteSpaces: Boolean): TdxPDFString;
var
  AResult: TBytes;
  ASymbol: Byte;
begin
  if (Current <> TdxPDFDefinedSymbols.StartObject) or (not ReadNext) then
    TdxPDFUtils.Abort;
  while SkipSpaces do
  begin
    if Current = TdxPDFDefinedSymbols.EndObject then
    begin
      ReadNext;
      Exit(TdxPDFString.Create(DecryptString(AResult)));
    end;
    ASymbol := GetHexDigit;
    if not ReadNext then
      TdxPDFUtils.Abort;
    if AIsHexStrSeparatedByWhiteSpaces then
    begin
      if IsWhiteSpace then
      begin
        TdxPDFUtils.AddByte(ASymbol, AResult);
        if not ReadNext then
          TdxPDFUtils.Abort;
        Continue;
      end
      else
        ASymbol := ASymbol * 16;
    end
    else
    begin
      if not SkipSpaces then
        TdxPDFUtils.Abort;
      ASymbol := ASymbol * 16;
    end;
    if Current = TdxPDFDefinedSymbols.EndObject then
      TdxPDFUtils.AddByte(ASymbol, AResult)
    else
    begin
      TdxPDFUtils.AddByte(ASymbol + GetHexDigit, AResult);
      if not ReadNext then
        TdxPDFUtils.Abort;
    end;
  end;
  Result := nil;
end;

function TdxPDFStructureParser.DoReadData(AToken: TdxPDFToken; AIgnoreWhiteSpace: Boolean = False;
  ASupportBracket: Boolean = False): TBytes;
var
  I, AEndMarkerPosition, ATokenLength, ALength: Integer;
  ACurrent, APrevious: Byte;
begin
  AEndMarkerPosition := 0;
  SetLength(Result, 0);
  ATokenLength := Length(AToken.Sequence);
  for I := -ATokenLength to MaxInt do
  begin
    TdxPDFUtils.AddByte(Current, Result);
    if (AEndMarkerPosition < ATokenLength) and (Current = AToken.Sequence[AEndMarkerPosition]) then
      Inc(AEndMarkerPosition)
    else
    begin
      if (AEndMarkerPosition = ATokenLength) and IsWhiteSpace then
      begin
        if AIgnoreWhiteSpace then
          Break
        else
        begin
          APrevious := Result[Length(Result) - 1 - ATokenLength - 1];
          if (APrevious in [TdxPDFDefinedSymbols.Space, TdxPDFDefinedSymbols.LineFeed, TdxPDFDefinedSymbols.CarriageReturn]) or
            (APrevious = $3E) and ASupportBracket then
          begin
            if (Current <> TdxPDFDefinedSymbols.LineFeed) or not ReadNext then
              Break;
            ACurrent := Current;
            ReadPrev;
            if (ACurrent in [TdxPDFDefinedSymbols.CarriageReturn, TdxPDFDefinedSymbols.Space]) or
           {$IFDEF DELPHIXE4}Char(ACurrent).IsLetterOrDigit{$ELSE}TCharacter.IsLetterOrDigit(Char(ACurrent)){$ENDIF} then
              Break;
          end;
        end;
      end;
    AEndMarkerPosition := 0;
    end;
    if not ReadNext then
      if AEndMarkerPosition = ATokenLength then
      begin
        TdxPDFUtils.AddByte(Current, Result);
        Break;
      end
  end;
  ALength := Length(Result) - ATokenLength - 2;
  SetLength(Result, Max(ALength, 0));
end;

function TdxPDFStructureParser.DoReadData(ALength: Integer; AToken: TdxPDFToken): TBytes;
begin
  if (ALength = 0) or not TdxPDFUtils.IsIntegerValid(ALength) then
  begin
    SkipSpaces;
    Result := DoReadData(AToken);
  end
  else
  begin
    if not ReadNext then
    begin
      if ALength = 0 then
        SetLength(Result, 0);
      TdxPDFUtils.Abort
    end;
    Result := Copy(Data, CurrentPosition, ALength);
    CurrentPosition := CurrentPosition + ALength;
  end;
end;

function TdxPDFStructureParser.DoReadDictionaryOrStream: TdxPDFBase;
var
  AIsDictionary: Boolean;
  ADictionary: TdxPDFDictionary;
begin
  Result := nil;
  if (Current = TdxPDFDefinedSymbols.StartObject) and ReadNext then
  begin
    AIsDictionary := Current = TdxPDFDefinedSymbols.StartObject;
    ReadPrev;
    if AIsDictionary then
    begin
      ADictionary := DoReadDictionary;
      if DoReadStreamToken then
        Result := ReadStream(ADictionary)
      else
        Result := ADictionary;
    end
    else
      Result := ReadHexadecimalString;
  end;
end;

procedure TdxPDFStructureParser.SetRepository(const AValue: TdxPDFCustomRepository);
begin
  if FRepository <> AValue then
  begin
    FreeAndNil(FRepository);
    FRepository := AValue;
  end;
end;

function TdxPDFStructureParser.DoReadStream(ADictionary: TdxPDFDictionary): TdxPDFStream;
var
  ALength: Integer;
  ALengthObject: TdxPDFBase;
begin
  if ADictionary <> nil then
  begin
    Result := CreateStream(ADictionary);
    ALengthObject := ADictionary.GetObject(TdxPDFKeywords.Length);
    if ALengthObject <> nil then
      case ALengthObject.ObjectType of
        otInteger:
          ALength := TdxPDFInteger(ALengthObject).Value;
        otIndirectReference:
          begin
            ALengthObject := Repository.GetObject(ALengthObject.Number) as TdxPDFBase;
            ALength := TdxPDFInteger(ALengthObject).Value;
          end;
      else
        ALength := dxPDFInvalidValue;
      end
    else
      ALength := dxPDFInvalidValue;
    PopulateStreamData(Result, ALength);
  end
  else
    Result := nil;
end;

function TdxPDFStructureParser.DoReadStream(ALength: Integer): TdxPDFStream;
begin
  Result := CreateStream(nil);
  PopulateStreamData(Result, ALength);
end;

function TdxPDFStructureParser.DoReadStreamToken: Boolean;
begin
  SaveCurrentPosition;
  Result := SkipSpaces and ReadToken(FStreamToken.Sequence);
  if not Result then
    RestoreCurrentPosition;
end;

procedure TdxPDFStructureParser.MoveToStreamDataStartPosition;
begin
  while Current = TdxPDFDefinedSymbols.Space do
    if not ReadNext then
      TdxPDFUtils.Abort;
  if not (Current in [TdxPDFDefinedSymbols.CarriageReturn, TdxPDFDefinedSymbols.LineFeed]) then
    TdxPDFUtils.Abort
  else
    if Current = TdxPDFDefinedSymbols.CarriageReturn then
    begin
      if not ReadNext then
        TdxPDFUtils.Abort;
      if Current <> TdxPDFDefinedSymbols.LineFeed then
        ReadPrev;
    end;
end;

procedure TdxPDFStructureParser.PopulateStreamData(AStream: TdxPDFStream; ALength: Integer);

  procedure ReadToEndStreamToken;
  var
    L: Int64;
    AData: TBytes;
  begin
    L := 0;
    try
      AData := DoReadData(FEndStreamToken, True, False);
      L := Length(AData);
    finally
      AStream.RawData := Copy(AData, 0, L);
      SetLength(AData, 0);
    end;
  end;

  procedure ReadData;
  var
    ASavedCurrentPosition: Int64;
  begin
    if not ReadNext then
      if ALength = 0 then
        TdxPDFUtils.RaiseTestException;
    SaveCurrentPosition;
    try
      AStream.RawData := Copy(Data, CurrentPosition, ALength);
      CurrentPosition := CurrentPosition + ALength;
      ASavedCurrentPosition := CurrentPosition;
      if not ReadToken(FEndStreamToken.Sequence) then
      begin
        CurrentPosition := ASavedCurrentPosition - ALength;
        ReadToEndStreamToken;
      end;
    finally
      RestoreCurrentPosition;
    end;
  end;

begin
  MoveToStreamDataStartPosition;
  if (ALength = 0) or not TdxPDFUtils.IsIntegerValid(ALength) then
  begin
    SkipSpaces;
    ReadToEndStreamToken;
  end
  else
    ReadData;
  TdxPDFStreamAccess(AStream).PopulateFilters;
end;

{ TdxPDFEncryptedStructureParser }

constructor TdxPDFEncryptedStructureParser.Create(ARepository: TdxPDFCustomRepository; ANumber: Integer);
begin
  inherited Create(ARepository);
  FNumber := ANumber;
end;

function TdxPDFEncryptedStructureParser.DecryptString(const AData: TBytes): string;
begin
  Result := inherited DecryptString((Repository as TdxPDFDocumentRepository).EncryptionInfo.Decrypt(AData, FNumber));
end;

{ TdxPDFDocumentParser }

function TdxPDFObjectParser.CanReadNumber(AOffset: Int64): Boolean;
var
  ASymbol: Byte;
  APosition: Int64;
begin
  APosition := FStream.Position;
  FStream.Position := AOffset;
  SkipSpaces(ASymbol);
  Result := TdxPDFUtils.IsHexDigit(ASymbol);
  FStream.Position := APosition;
end;

function TdxPDFObjectParser.IsObjectValid(AReference: TdxPDFReference): Boolean;
var
  AReadedNumber: Integer;
begin
  if CanReadNumber(AReference.Offset) then
  begin
    AReadedNumber := ReadIndirectObjectNumber(AReference.Offset);
    Result := AReadedNumber = AReference.Number;
  end
  else
    Result := False;
end;

function TdxPDFDocumentParser.ReadCrossReferences(AOffset: Integer): Boolean;

  procedure CalculateStartPositionOffset;
  var
    APosition, ADelta: Int64;
    ACrossReferencesToken: TdxPDFToken;
  begin
    ACrossReferencesToken := TdxPDFToken.Create(TdxPDFUtils.StrToByteArray('xref'));
    try
      ADelta := 0;
      if AOffset - StartXRefOffset > 0 then
        while not Result and (FStream.Position > AOffset - StartXRefOffset) do
        begin
          FStream.Position := FStream.Position - 1;
          APosition := FStream.Position;
          Result := ReadToken(ACrossReferencesToken);
          if not Result then
            FStream.Position := APosition
          else
            ADelta := AOffset - FStream.Position + Length(ACrossReferencesToken.Sequence);
        end;
      FPositionOffset := FPositionOffset + ADelta;
    finally
      ACrossReferencesToken.Free;
    end;
  end;

var
  ANumber, ACount: Integer;
  ACrossReferencesToken: TdxPDFToken;
  ASymbol: Byte;
  APosition: Int64;
begin
  FStream.Position := AOffset;
  ACrossReferencesToken := TdxPDFToken.Create('xref');
  try
    Result := ReadToken(ACrossReferencesToken);
    CalculateStartPositionOffset;
    while Result do
    begin
      Result := True;
      APosition := FStream.Position;
      SkipSpaces(ASymbol);
      if IsEOF or not TdxPDFUtils.IsDigit(ASymbol) then
      begin
        FStream.Position := APosition;
        Break;
      end;
      FStream.Position := FStream.Position - 1;
      try
        ANumber := ReadNumber;
        ACount := ReadNumber;
        ReadCrossReferenceRecords(ANumber, ACount);
      except
        Result := False;
      end;
    end;
  finally
    ACrossReferencesToken.Free;
  end;
end;

function TdxPDFObjectParser.ReadIndirectObject(AOffset: Int64): TdxPDFIndirectObject;
var
  AData: TBytes;
  ANumber, AGeneration: Integer;
begin
  AData := ReadObjectData(AOffset, ANumber, AGeneration);
  Result := TdxPDFIndirectObject.Create(ANumber, AGeneration, AData);
end;

function TdxPDFObjectParser.ReadIndirectObjectNumber(AOffset: Int64): Integer;
begin
  FStream.Position := AOffset;
  Result := ReadNumber;
end;

function TdxPDFObjectParser.ReadNumber: Integer;
var
  AIsFraction: Boolean;
  ASymbol: Byte;
begin
  SkipSpaces(ASymbol);
  AIsFraction := False;
  Result := TdxPDFUtils.ByteToHexDigit(ASymbol);
  while not IsEOF do
  begin
    ASymbol := ReadByte;
    if TdxPDFUtils.IsWhiteSpace(ASymbol) then
      Break;
    if ASymbol = TdxPDFDefinedSymbols.Period then
      AIsFraction := True
    else
      if AIsFraction then
      begin
        if ASymbol <> TdxPDFDefinedSymbols.DigitStart then
          TdxPDFUtils.Abort
      end
      else
        Result := Result * 10 + TdxPDFUtils.ByteToHexDigit(ASymbol);
  end;
end;

function TdxPDFObjectParser.ReadString: string;
var
  ABuilder: TStringBuilder;
  ASymbol: Byte;
begin
  ABuilder := TStringBuilder.Create;
  try
    while not IsEOF do
    begin
      ASymbol := ReadByte;
      if ASymbol in [TdxPDFDefinedSymbols.LineFeed, TdxPDFDefinedSymbols.CarriageReturn] then
      begin
        Result := ABuilder.ToString;
        Break;
      end
      else
        ABuilder.Append(Char(ASymbol));
    end;
  finally
    ABuilder.Free;
  end;
end;

function TdxPDFDocumentParser.ReadCrossReferencesOffset: Int64;
var
  AData: TBytes;
  ARequiredSpace, AStartXRefObjectOffset: Int64;
begin
  Result := -1;
  SetLength(AData, 0);
  AStartXRefObjectOffset := GetStartXRefObjectOffset;
  if AStartXRefObjectOffset >= 0 then
  begin
    FStream.Position := AStartXRefObjectOffset;
    ARequiredSpace := RequiredStartXRefSpace;
    if FStream.Position + ARequiredSpace > FStream.Size then
      ARequiredSpace := FStream.Size - FStream.Position;
    SetLength(AData, ARequiredSpace);
    FStream.ReadBuffer(AData[0], ARequiredSpace);
    Result := ReadStartXRefObject(AData);
    SetLength(AData, 0);
  end;
end;

function TdxPDFDocumentParser.FindToken(const ATokenDescription: string): Boolean;
var
  AToken: TdxPDFToken;
begin
  AToken := TdxPDFToken.Create(ATokenDescription);
  try
    Result := FindToken(AToken);
  finally
    AToken.Free;
  end;
end;

function TdxPDFDocumentParser.FindToken(AToken: TdxPDFToken): Boolean;
begin
  Result := False;
  while not IsEOF do
  begin
    Result := AToken.Compare(ReadByte);
    if Result then
      Break;
  end;
end;

function TdxPDFDocumentParser.ParseVersion(const AHeader: string): TdxPDFVersion;

  function CreateFormatVersionDictionary: TDictionary<string, TdxPDFVersion>;
  begin
    Result := TDictionary<string, TdxPDFVersion>.Create;
    Result.Add('1.7', TdxPDFVersion.v1_7);
    Result.Add('1.6', TdxPDFVersion.v1_6);
    Result.Add('1.5', TdxPDFVersion.v1_5);
    Result.Add('1.4', TdxPDFVersion.v1_4);
    Result.Add('1.3', TdxPDFVersion.v1_3);
    Result.Add('1.2', TdxPDFVersion.v1_2);
    Result.Add('1.1', TdxPDFVersion.v1_1);
    Result.Add('1.0', TdxPDFVersion.v1_0);
  end;

var
  APair: TPair<string, TdxPDFVersion>;
  AFormatVersionDictionary: TDictionary<string, TdxPDFVersion>;
begin
  Result := TdxPDFVersion.v1_7;
  AFormatVersionDictionary := CreateFormatVersionDictionary;
  try
    for APair in AFormatVersionDictionary do
      if StartsStr(APair.Key, RightStr(AHeader, Length(AHeader) - Length(TdxPDFKeywords.VersionSignature))) then
      begin
        Result := APair.Value;
        Break;
      end;
  finally
    AFormatVersionDictionary.Free;
  end;
end;

function TdxPDFDocumentParser.ReadTrailerData: TBytes;
var
  AData: TBytes;
  ASymbol: Byte;
  ACrossReferencesStartToken, ATrailerToken: TdxPDFToken;
begin
  SetLength(Result, 0);
  ATrailerToken := TdxPDFToken.Create(TdxPDFKeywords.Trailer);
  try
    begin
      ACrossReferencesStartToken := TdxPDFToken.Create('startxref');
      try
        while not IsEOF do
        begin
          ASymbol := ReadByte;
          if not IsEOF then
          begin
            TdxPDFUtils.AddByte(ASymbol, Result);
            if ACrossReferencesStartToken.Compare(ASymbol) then
            begin
              TdxPDFUtils.AddData(Result, AData);
              SetLength(Result, Length(Result) - Length(ACrossReferencesStartToken.Sequence));
              TdxPDFUtils.CopyData(AData, 0, Result, 0, Length(Result));
              Break;
            end;
          end
        end;
      finally
        ACrossReferencesStartToken.Free;
      end;
    end;
  finally
    ATrailerToken.Free;
  end;
end;

procedure TdxPDFDocumentParser.CalculateHeaderPosition;
var
  I, AHeaderSignatureLength: Integer;
  B: Byte;
  APosition: Integer;
begin
  I := 0;
  APosition := FStream.Position;
  AHeaderSignatureLength := Length(TdxPDFKeywords.VersionSignature);
  while not IsEOF do
  begin
    B := ReadByte;
    I := IfThen(Chr(B) = TdxPDFKeywords.VersionSignature[I + 1], I + 1);
    if I = AHeaderSignatureLength then
    begin
      FStream.Position := FStream.Position - AHeaderSignatureLength;
      FPositionOffset := APosition - FStream.Position;
      Break;
    end;
  end;
end;

procedure TdxPDFDocumentParser.FindObjects;
var
  AData: Byte;
  AStartXRefPosition, APosition: Int64;
  AEOFToken, AEndObjToken, AObjToken, ACrossReferencesToken: TdxPDFToken;
  ASlot: TdxPDFBase;
begin
  (Repository as TdxPDFDocumentRepository).RemoveCorruptedObjects;
  FStream.Position := 0;

  ACrossReferencesToken := TdxPDFToken.Create(TdxPDFUtils.StrToByteArray('xref'));
  AObjToken := TdxPDFToken.Create(TdxPDFUtils.StrToByteArray('obj'));
  AEndObjToken := TdxPDFToken.Create(TdxPDFUtils.StrToByteArray('endobj'));
  AEOFToken := TdxPDFToken.Create(TdxPDFUtils.StrToByteArray('%%EOF'));

  AStartXRefPosition := -1;
  try
    while not IsEOF do
    begin
      APosition := FStream.Position;
      AData := ReadByte;
      if not TdxPDFUtils.IsWhiteSpace(AData) then
      begin
        if AData <> TdxPDFDefinedSymbols.Comment then
        begin
          FStream.Position := APosition;
          if ReadToken(ACrossReferencesToken) then
          begin
            AStartXRefPosition := FStream.Position;
              if not ReadToken(AEOFToken) then
              Break
            else
              Continue;
          end
          else
          begin
            FStream.Position := APosition;
            if ReadToken(FStartXRefObjectToken) then
            begin
              if (ReadNumber <> 0) and ReadToken(AEOFToken) then
                AStartXRefPosition := APosition;
              Continue;
            end
            else
            begin
              try
                ASlot := ReadObjectSlot(APosition);
                Repository.Add(ASlot.Number, ASlot, True);
              except
                on E: EdxPDFAbortException do
                begin
                  if AStartXRefPosition >= 0 then
                    Break
                  else
                    Continue;
                end
                else
                  raise;
              end;
            end;
          end;
        end
        else
          ReadString;
      end
    end;
  finally
    AEOFToken.Free;
    ACrossReferencesToken.Free;
    AObjToken.Free;
    AEndObjToken.Free;
  end;
  FStream.Position := Max(0, AStartXRefPosition);
end;

procedure TdxPDFDocumentParser.ReadVersion(out AVersion: TdxPDFVersion);
var
  AHeader: string;
begin
  CalculateHeaderPosition;
  AHeader := ReadString;
  while not StartsStr(TdxPDFKeywords.VersionSignature, AHeader) do
    if not ((Length(AHeader) = 0) or (Byte(AHeader[1]) <> TdxPDFDefinedSymbols.Comment)) then
      AHeader := ReadString
    else
      TdxPDFUtils.Abort;
  AVersion := ParseVersion(AHeader);
end;

procedure TdxPDFDocumentParser.ReadTrailer(out ATrailer: TdxPDFDictionary);
begin
  ReadTrailer(FStream.Position, ATrailer);
end;

procedure TdxPDFDocumentParser.ReadTrailer(AOffset: Int64; out ATrailer: TdxPDFDictionary);
var
  ATrailerData: TBytes;
  ATrailerToken: TdxPDFToken;
begin
  if ReadCrossReferences(AOffset - FPositionOffset) then
  begin
    SetLength(ATrailerData, 0);
    ATrailerToken := TdxPDFToken.Create(TdxPDFKeywords.Trailer);
    try
      if FindToken(ATrailerToken) then
        ATrailerData := ReadTrailerData;
    finally
      ATrailerToken.Free;
    end;
    ATrailer := ReadDictionary(ATrailerData);
  end
  else
    ReadTrailerStream(AOffset - FPositionOffset, ATrailer);
end;

procedure TdxPDFDocumentParser.SaveStartPosition;
begin
  FStartPosition := FStream.Position;
end;

function TdxPDFObjectParser.GetEOF: Boolean;
begin
  Result := FStream.Position >= FStream.Size;
end;

procedure TdxPDFObjectParser.InitializeTokens;
begin
  inherited InitializeTokens;
  FCheckObjToken := TdxPDFToken.Create(TdxPDFUtils.StrToByteArray(' obj'));
end;

procedure TdxPDFObjectParser.FinalizeTokens;
begin
  FreeAndNil(FCheckObjToken);
  inherited FinalizeTokens;
end;

procedure TdxPDFDocumentParser.InitializeTokens;
begin
  inherited InitializeTokens;
  FStartXRefObjectToken := TdxPDFToken.Create(TdxPDFUtils.StrToByteArray('startxref'));
end;

procedure TdxPDFDocumentParser.FinalizeTokens;
begin
  FreeAndNil(FStartXRefObjectToken);
  inherited FinalizeTokens;
end;

function TdxPDFObjectParser.ReadByte: Byte;
begin
  FStream.ReadBuffer(Result, SizeOf(Result));
end;

function TdxPDFDocumentParser.ReadNumber(ADigitCount: Integer): Int64;
var
  I: Integer;
  ASymbol: Byte;
begin
  SkipSpaces(ASymbol);
  Result := TdxPDFUtils.ByteToHexDigit(ASymbol);
  for I := 1 to ADigitCount - 1 do
  begin
    ASymbol := ReadByte;
    if not IsEOF then
      Result := Result * 10 + TdxPDFUtils.ByteToHexDigit(ASymbol)
    else
      TdxPDFUtils.Abort;
  end;
end;

constructor TdxPDFObjectParser.Create(ARepository: TdxPDFCustomRepository; AStream: TStream);
begin
  inherited Create(ARepository);
  FStream := AStream;
end;

function TdxPDFObjectParser.ReadObject(AOffset: Int64): TdxPDFBase;
var
  AIndirectObject: TdxPDFIndirectObject;
begin
  AIndirectObject := ReadIndirectObject(AOffset);
  try
    Result := ReadObject(AIndirectObject.Data);
    Result.Number := AIndirectObject.Number;
    Result.Generation := AIndirectObject.Generation;
  finally
    AIndirectObject.Free;
  end;
end;

function TdxPDFObjectParser.ReadObjectData(AOffset: Int64; out ANumber, AGeneration: Integer): TBytes;
var
  ANext, ATokenLength: Integer;
  ABeginToken, AEndToken, AEndObjToken: TdxPDFToken;
  ANested: Boolean;
  ASymbol: Byte;
  AObjToken: TdxPDFToken;
begin
  AObjToken := TdxPDFToken.Create(TdxPDFUtils.StrToByteArray('obj'));
  AEndObjToken := TdxPDFToken.Create(TdxPDFUtils.StrToByteArray('endobj'));
  try
    FStream.Position := AOffset;
    ANumber := ReadNumber;
    AGeneration := ReadNumber;
    if ReadToken(AObjToken) then
    begin
      SetLength(Result, 0);
      ABeginToken := TdxPDFToken.BeginCompare(FCheckObjToken);
      AEndToken := TdxPDFToken.BeginCompare(AEndObjToken);
      try
        ANested := False;
        while not IsEOF do
        begin
          ANext := ReadByte;
          ASymbol := Byte(ANext);
          TdxPDFUtils.AddByte(ASymbol, Result);
          if AEndToken.Compare(ASymbol) then
            if ANested then
            begin
              AEndToken.Free;
              AEndToken := TdxPDFToken.BeginCompare(AEndObjToken);
              ANested := False;
            end
            else
            begin
              ATokenLength := Length(AEndToken.Sequence);
              TdxPDFUtils.DeleteData(Length(Result) - ATokenLength, ATokenLength, Result);
              Break;
            end;
          if ABeginToken.Compare(ASymbol) then
          begin
            if TdxPDFUtils.IsDigit(Result[Length(Result) - Length(FCheckObjToken.Sequence) - 1]) then
              ANested := True;
            ABeginToken.Free;
            ABeginToken := TdxPDFToken.BeginCompare(FCheckObjToken);
          end;
        end;
      finally
        AEndToken.Free;
        ABeginToken.Free;
      end;
    end
    else
      TdxPDFUtils.Abort;
  finally
    AEndObjToken.Free;
    AObjToken.Free;
  end;
end;

function TdxPDFObjectParser.ReadToken(AToken: TdxPDFToken): Boolean;
var
  I: Integer;
  ASymbol: Byte;
begin
  Result := False;
  if AToken.IsStartWithComment then
    ASymbol := ReadByte
  else
    SkipSpaces(ASymbol);
  if not IsEOF then
  begin
    Result := AToken.Compare(ASymbol);
    for I := 1 to Length(AToken.Sequence) - 1 do
      if not IsEOF then
      begin
        Result := AToken.Compare(ReadByte);
        if Result then
          Break;
      end;
  end;
end;

procedure TdxPDFDocumentParser.ReadCrossReferenceRecords(AStartNumber, ACount: Integer);

  function ReadObjectParameter(ADimension: Integer): Integer;
  begin
    Result := ReadNumber(ADimension);
    if ReadByte <> TdxPDFDefinedSymbols.Space then
      TdxPDFUtils.Abort;
  end;

const
  dxcXReferenceObjectOffsetDimension = 10;
  dxcXReferenceObjectGenerationDimension = 5;
var
  I: Integer;
  ANumber, AGeneration, AOffset: Int64;
  ANext: Byte;
begin
  ANumber := AStartNumber;
  for I := 0 to ACount - 1 do
  begin
    AOffset := ReadObjectParameter(dxcXReferenceObjectOffsetDimension) - FPositionOffset;
    AGeneration := ReadObjectParameter(dxcXReferenceObjectGenerationDimension);
    case ReadByte of
      Byte('n'):
        Repository.Add(ANumber, TdxPDFReference.Create(ANumber, AGeneration, AOffset, False, True), False);
      Byte('f'):
        Repository.Add(ANumber, TdxPDFReference.Create(ANumber, AGeneration, AOffset, True), False);
    end;
    ANext := ReadByte;
    case ANext of
      TdxPDFDefinedSymbols.Space, TdxPDFDefinedSymbols.CarriageReturn:
        begin
          ANext := ReadByte;
          if (ANext <> TdxPDFDefinedSymbols.LineFeed) and (ANext <> TdxPDFDefinedSymbols.CarriageReturn) then
            TdxPDFUtils.Abort;
        end;
    end;
    Inc(ANumber);
  end;
end;

function TdxPDFDocumentParser.DoReadStartXRefObject: Integer;
var
  APosition: TdxPDFInteger;
  AToken: TdxPDFToken;
begin
  Result := dxPDFInvalidValue;
  AToken := TdxPDFToken.BeginCompare(FStartXRefObjectToken);
  try
    repeat
      if AToken.Compare(Current) then
        if ReadNext and SkipSpaces then
        begin
          APosition := DoReadNumber as TdxPDFInteger;
          if APosition <> nil then
          begin
            Result := APosition.Value;
            APosition.Free;
            Break;
          end;
        end
        else
          Break;
    until not ReadNext;
  finally
    AToken.Free;
  end;
end;

function TdxPDFDocumentParser.GetStartXRefObjectOffset: Int64;
var
  APosition, AOffset: Int64;
  AEOFToken: TdxPDFToken;
  ARequiredSpace: Int64;
begin
  ARequiredSpace := RequiredStartXRefSpace;
  APosition := FStream.Position;
  AEOFToken := TdxPDFToken.Create(TdxPDFUtils.StrToByteArray('%%EOF'));
  try
    FStream.Position := IfThen(FStream.Size > StartXRefOffset, FStream.Size - StartXRefOffset, FStream.Size);
    AOffset := -1;
    if FindToken(AEOFToken) then
      AOffset := FStream.Position
    else
    begin
      FStream.Position := APosition;
      while FindToken(AEOFToken) do
        AOffset := FStream.Position;
      if AOffset = -1 then
      begin
        FStream.Position := APosition;
        AOffset := ReadCorruptedStartXRefObjectOffset;
        if AOffset = -1 then
          TdxPDFUtils.Abort;
        ARequiredSpace := 0;
      end
    end;
    Result := AOffset - ARequiredSpace;
  finally
    AEOFToken.Free;
  end;
end;

function TdxPDFDocumentParser.ReadCorruptedStartXRefObjectOffset: Int64;
var
  AData: Byte;
  AEOFToken: TdxPDFToken;
  APosition: Int64;
begin
  Result := -1;
  APosition := FStream.Position;
  AEOFToken := TdxPDFToken.Create(TdxPDFUtils.StrToByteArray('%%EOF'));
  try
    while not IsEOF do
    begin
      AData := ReadByte;
      if not TdxPDFUtils.IsSpace(AData) then
      begin
        if AData <> TdxPDFDefinedSymbols.Comment then
        begin
          FStream.Position := APosition;
          if ReadToken(FStartXRefObjectToken) then
          begin
            Result := FStream.Position - Length(FStartXRefObjectToken.Sequence);
            if not ReadToken(AEOFToken) then
              Break
            else
              Continue;
          end;
        end
        else
          ReadString;
      end;
      APosition := FStream.Position;
    end;
  finally
    AEOFToken.Free;
  end;
end;

function TdxPDFDocumentParser.ReadObjectSlot(AOffset: Int64): TdxPDFReference;

  function IsSingleObject(AStart, AEnd: Int64): Boolean;
  var
    APosition: Int64;
  begin
    Result := True;
    APosition := FStream.Position;
    try
      FStream.Position := AStart;
      if FindToken(' obj') then
        Result := FStream.Position >= AEnd
    finally
      FStream.Position := APosition;
    end;
  end;

var
  ANumber, AGeneration: Integer;
  AStartPosition: Int64;
  AIsObjectFound, AIsObjectEndFound: Boolean;
begin
  FStream.Position := AOffset;
  ANumber := ReadNumber;
  AGeneration := ReadNumber;
  AIsObjectFound := FindToken('obj');
  AStartPosition := FStream.Position;
  AIsObjectEndFound := FindToken('endobj');
  if not IsSingleObject(AStartPosition, FStream.Position) then
  begin
    FStream.Position := AStartPosition;
    AIsObjectEndFound := FindToken('endstream') and IsSingleObject(AStartPosition, FStream.Position);
  end;
  if not (AIsObjectFound and AIsObjectEndFound) then
    TdxPDFUtils.Abort;
  Result := TdxPDFReference.Create(ANumber, AGeneration, AOffset, False, True);
end;

function TdxPDFDocumentParser.ReadStartXRefObject(const AData: TBytes): Integer;
begin
  Data := AData;
  Result := DoReadStartXRefObject;
end;

procedure TdxPDFDocumentParser.ReadTrailerStream(AOffset: Int64; out ATrailer: TdxPDFDictionary);

    function GetDecodeParameters(ATrailer: TdxPDFDictionary; out ATypeSize, AOffsetSize, AGenerationSize: Integer): Boolean;
    var
      ASizes: TdxPDFArray;
    begin
      ASizes := ATrailer.GetArray(TdxPDFKeywords.ShortWidths);
      Result := (ASizes <> nil) and (ASizes.Count = 3) and (ATrailer.GetString(TdxPDFKeywords.TypeKey) = 'XRef');
      if  Result then
      begin
        ATypeSize := TdxPDFInteger(ASizes[0]).Value;
        AOffsetSize := TdxPDFInteger(ASizes[1]).Value;
        AGenerationSize := TdxPDFInteger(ASizes[2]).Value;
      end;
    end;

var
  ATypeSize, AOffsetSize, AGenerationSize: Integer;
  AIndexes: TList<TdxPDFCrossReferenceIndexDescription>;
  AObject: TdxPDFBase;
  AStream: TdxPDFStream;
begin
  AObject := ReadObject(AOffset);
  if AObject is TdxPDFStream then
  begin
    AStream := TdxPDFStream(AObject);
    try
      ATrailer := AStream.Dictionary;
      ATrailer.Reference;
      if GetDecodeParameters(ATrailer, ATypeSize, AOffsetSize, AGenerationSize) then
      begin
        AIndexes := TList<TdxPDFCrossReferenceIndexDescription>.Create;
        try
          PopulateIndexes(ATrailer, AIndexes);
          Decode(AStream.UncompressedData, AIndexes, ATypeSize, AOffsetSize, AGenerationSize);
          Repository.TrimExcess;
        finally
          AIndexes.Free;
        end;
      end
    finally
      AStream.Free;
    end;
  end
  else
  begin
    ATrailer := nil;
    dxPDFFreeObject(AObject);
  end;
end;

function TdxPDFDocumentParser.ReadValue(const AData: TBytes; AWeight: Integer; var APosition: Int64): Integer;
var
  I: Integer;
begin
  if AWeight > 0 then
  begin
    if APosition + AWeight <= Length(AData) then
    begin
      Result := AData[APosition];
      Inc(APosition);
      for I := 1 to AWeight - 1 do
      begin
        Result := Result shl 8 + AData[APosition];
        Inc(APosition);
      end;
    end
    else
      raise Exception.Create('Error Message');
  end
  else
    Result := 0;
end;

procedure TdxPDFDocumentParser.Decode(const AData: TBytes; AIndexes: TList<TdxPDFCrossReferenceIndexDescription>;
  ATypeSize, AOffsetSize, AGenerationSize: Integer);
var
  APosition: Int64;
  I, ANumber, AObjectType, V1, V2: Integer;
  ADescription: TdxPDFCrossReferenceIndexDescription;
begin
  APosition := 0;
  for ADescription in AIndexes do
  begin
    ANumber := ADescription.StartNumber;
    for I := 0 to ADescription.Count - 1 do
    begin
      if ATypeSize = 0 then
        AObjectType := 1
      else
        AObjectType := ReadValue(AData, ATypeSize, APosition);
      V1 := ReadValue(AData, AOffsetSize, APosition);
      V2 := ReadValue(AData, AGenerationSize, APosition);
      case AObjectType of
        0:
          Repository.Add(ANumber, TdxPDFReference.Create(ANumber, V2, V1, True), False);
        1:
          Repository.Add(ANumber, TdxPDFReference.Create(ANumber, V2, V1, False, True), False);
        2:
          (Repository as TdxPDFDocumentRepository).AddStreamElement(ANumber, TdxPDFStreamElement.Create(V1, V2));
      else
        TdxPDFUtils.RaiseTestException('TdxPDFDocumentParser.Decode');
      end;
      Inc(ANumber);
    end;
  end;
end;

procedure TdxPDFDocumentParser.PopulateIndexes(ATrailer: TdxPDFDictionary;
  AIndexes: TList<TdxPDFCrossReferenceIndexDescription>);

  function GetIndexDescription(ANumber, ACount: Integer): TdxPDFCrossReferenceIndexDescription;
  begin
    Result.StartNumber := ANumber;
    Result.Count := ACount;
  end;

var
  AArray: TdxPDFArray;
  I, AIndex, AIndexCount, AStartValue, AValuesCount, ASizeValue: Integer;
begin
  AArray := ATrailer.GetArray('Index');
  if AArray = nil then
  begin
    ASizeValue := ATrailer.GetInteger(TdxPDFKeywords.Size);
    if TdxPDFUtils.IsIntegerValid(ASizeValue) and (ASizeValue >= 1) then
      AIndexes.Add(GetIndexDescription(0, ASizeValue))
    else
      TdxPDFUtils.Abort;
  end
  else
  begin
    AIndexCount := AArray.Count mod 2;
    if AIndexCount <> 1 then
    begin
      AIndex := 0;
      AIndexCount := AArray.Count div 2;
      for I := 0 to AIndexCount - 1 do
      begin
        AStartValue := TdxPDFInteger(AArray[AIndex]).Value;
        Inc(AIndex);
        AValuesCount := TdxPDFInteger(AArray[AIndex]).Value;
        Inc(AIndex);
        AIndexes.Add(GetIndexDescription(AStartValue, AValuesCount));
      end;
    end;
  end;
end;

procedure TdxPDFObjectParser.SkipSpaces(out ASymbol: Byte);
begin
  while not IsEOF do
  begin
    ASymbol := ReadByte;
    if ASymbol = TdxPDFDefinedSymbols.Comment then
      ReadString
    else
      if not TdxPDFUtils.IsWhiteSpace(ASymbol) then
        Break;
  end;
end;

end.



