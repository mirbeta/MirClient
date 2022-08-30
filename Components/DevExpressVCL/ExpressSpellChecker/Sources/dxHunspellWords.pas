{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressSpellChecker                                      }
{                                                                    }
{           Copyright (c) 1998-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSSPELLCHECKER AND ALL           }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY. }
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

unit dxHunspellWords;

{$I cxVer.inc}

interface

uses
  SysUtils, Classes, cxClasses, dxHunspellTypes, dxSpellCheckerUtils, dxCore;

const
  dxHunspellMaxDictionaryCount  = 20;

type
  TdxHunspellWordStemManager = class;
  TdxHunspellWordStem = class;
  TdxHunspellFlags = class;

  TdxAffixFlagMode = (afmCharacter, afmTwoCharacters, afmNumber, afmUnicode);

  TdxWordStemOption = (wsoMorphologyDescription, wsoMorphologyAliases,
    wsoMorphologyPhone, wsoReverse, wsoUpcaseOnly, wsoForbidden, wsoNoSuggest,
    wsoCompound, wsoCompoundBegin, wsoCompoundMiddle, wsoCompoundEnd,
    wsoCompoundRoot, wsoInCompoundOnly, wsoNeedAffix, wsoKeepCase);
  TdxWordStemOptions = set of TdxWordStemOption;

  PdxWordStemTable = ^TdxWordStemTable;
  TdxWordStemTable = array[0..0] of TdxHunspellWordStem;

  { TdxHunspellFlags }

  TdxHunspellFlags = class
  private
    FAffixFlagMode: TdxAffixFlagMode;
    FData: PdxAffixFlagsData;
    FDataSize: Word;
    FLength: Word;
    procedure DisposeData;
    function GetItem(Index: Integer): Word; inline;
    procedure SetLength(ANewLength: Word);
  public
    constructor Create(AAffixFlagMode: TdxAffixFlagMode);
    constructor CreateCapitalized(AFlags: TdxHunspellFlags; AAffixFlagMode: TdxAffixFlagMode);
    destructor Destroy; override;
    procedure Assign(ASource: TdxHunspellFlags);
    procedure AssignCapitalized(ASource: TdxHunspellFlags);
    function ContainsFlag(AFlag: Word): Boolean;
    function Decode(AWordStemManager: TdxHunspellWordStemManager; AFlags: PWideChar): Boolean;
    function InitializeByAlias(AWordStemManager: TdxHunspellWordStemManager; AAlias: PWideChar): Boolean;
    procedure Sort;

    property Data: PdxAffixFlagsData read FData write FData;
    property Items[Index: Integer]: Word read GetItem; default;
    property Length: Word read FLength write SetLength;
  end;

  { TdxHunspellFlagsList }

  TdxHunspellFlagsList = class(TcxObjectList)
  private
    function GetItem(Index: Integer): TdxHunspellFlags; inline;
  public
    property Items[Index: Integer]: TdxHunspellFlags read GetItem; default;
  end;

  { TdxHunspellWordStem }

  TdxHunspellWordStem = class
  private
    FAffixFlags: TdxHunspellFlags;
    FNext: TdxHunspellWordStem;
    FNextHomonym: TdxHunspellWordStem;
    FMorphologicalDescription: PWideChar;
    FOptions: TdxWordStemOptions;
    FWordStem: PWideChar;
    FWordStemLength: Byte;
    function GetCompound: Boolean; inline;
    function GetCompoundBegin: Boolean; inline;
    function GetCompoundEnd: Boolean; inline;
    function GetCompoundMiddle: Boolean; inline;
    function GetCompoundRoot: Boolean; inline;
    function GetForbidden: Boolean; inline;
    function GetInCompoundOnly: Boolean; inline;
    function GetKeepCase: Boolean; inline;
    function GetMorphologyAliases: Boolean; inline;
    function GetMorphologyPhone: Boolean; inline;
    function GetNeedAffix: Boolean; inline;
    function GetNoSuggest: Boolean; inline;
    function GetUpcaseOnly: Boolean; inline;
    procedure SetAffixFlags(const Value: TdxHunspellFlags);
    procedure UpdateOptionsByMorphology;
  public
    constructor Create(const AWord: PWideChar; AWordLength: Integer; const AIgnoreChars: PWideChar; AReverse: Boolean);
    destructor Destroy; override;
    function IsCompatibleWithFlag(AFlag: Word; CompatibleIfNull: Boolean = False): Boolean; inline;
    procedure ProcessFlags(AWordStemManager: TdxHunspellWordStemManager);
    procedure SetMorphology(const AMorphologocalDescription: PWideChar);
    procedure SetMorphologyByAlias(const AMorphologocalDescription: PWideChar);
    procedure UpdateOptions(const AOptions: TdxWordStemOptions);

    property AffixFlags: TdxHunspellFlags read FAffixFlags write SetAffixFlags;

    //options
    property Compound: Boolean read GetCompound;
    property CompoundBegin: Boolean read GetCompoundBegin;
    property CompoundEnd: Boolean read GetCompoundEnd;
    property CompoundMiddle: Boolean read GetCompoundMiddle;
    property CompoundRoot: Boolean read GetCompoundRoot;
    property Forbidden: Boolean read GetForbidden;
    property InCompoundOnly: Boolean read GetInCompoundOnly;
    property KeepCase: Boolean read GetKeepCase;
    property MorphologyAliases: Boolean read GetMorphologyAliases;
    property MorphologyPhone: Boolean read GetMorphologyPhone;
    property NeedAffix: Boolean read GetNeedAffix;
    property NoSuggest: Boolean read GetNoSuggest;
    property UpcaseOnly: Boolean read GetUpcaseOnly;

    property MorphologicalDescription: PWideChar read FMorphologicalDescription;
    property Next: TdxHunspellWordStem read FNext write FNext;
    property NextHomonym: TdxHunspellWordStem read FNextHomonym write FNextHomonym;
    property Options: TdxWordStemOptions read FOptions;
    property WordStem: PWideChar read FWordStem;
    property WordStemLength: Byte read FWordStemLength;
  end;

  { TdxHunspellReader }

  TdxHunspellReader = class
  private
    FData: TdxSpellCheckerStrings;
    FLineIndex: Integer;
    procedure LoadFromStream(AStream: TStream);
  public
    constructor Create(AStream: TStream; ACodePage: Integer);
    destructor Destroy; override;
    function GetNextLine(var ABuffer; ABufferSize: Integer): Boolean;
    procedure Reset;

    property LineIndex: Integer read FLineIndex;
  end;

  { TdxHunspellWordStemManager }

  TdxHunspellWordStemManager = class
  private
    FAffixFlagAliases: TdxHunspellFlagsList;
    FAffixFlagMode: TdxAffixFlagMode;
    FAffixMorphologicAliases: PdxPWideCharArray;
    FAffixMorphologicAliasesCount: Integer;
    FCodePage: Cardinal;
    FComplexPrefixes: Boolean;
    FForbiddenWordFlag: Word;
    FHashTable: PdxWordStemTable;
    FHashTableSize: Integer;
    FIgnoredChars: PWideChar;
    FLanguage: Integer;
    //hash
    function AllocateHashTable(S: PWideChar): Boolean;
    procedure FreeHashTable;
    function Hash(S: PWideChar): Cardinal;
    //
    procedure AddWord(const AWord: PWideChar; AWordLength: Integer; AAffixFlags: TdxHunspellFlags;
      const AMorphologocalDescription: PWideChar; AOnlyUpperCase: Boolean);
    procedure CheckAddCapitalizedWord(AWord: PWideChar; AWordLength: Integer;
      AAffixFlags: TdxHunspellFlags; ADescription: PWideChar);
    function CreateWordStem(const AWord: PWideChar; AWordLength: Integer;
      const AMorphologyDescription: PWideChar): TdxHunspellWordStem;
    procedure FreeAffixMorphologicAliases;
    function GetAffixesDescription(ALine: PWideChar): PWideChar;
    function GetMorphologyDescription(ALine: PWideChar): PWideChar;
    function InitializeAffixFlags(AAffixFlags: TdxHunspellFlags; ADescription: PWideChar): Boolean;
    function LoadWordStems(AStream: TStream): Boolean;
    procedure ParseWordStem(ALine: PWideChar);
    procedure SetupMorphology(var AWordStem: TdxHunspellWordStem;
      const AMorphologyDescription: PWideChar);
  public
    constructor Create(ALanguage: Integer);
    destructor Destroy; override;
    procedure AllocateAffixMorphologicAliases(ASize: Integer);
    function GetAffixMorphologyByAlias(AAlias: Integer): PWideChar;
    function GetFirstItem(out ATableIndex: Integer): TdxHunspellWordStem;
    function GetNextItem(var ATableIndex: Integer; AWordStem: TdxHunspellWordStem): TdxHunspellWordStem;
    function HasAffixFlagAliases: Boolean;
    function HasAffixMorphologicAliases: Boolean;
    function Load(ADictionaryStream: TStream): Boolean;
    function Lookup(const AWord: PWideChar): TdxHunspellWordStem;

    property AffixFlagAliases: TdxHunspellFlagsList read FAffixFlagAliases;
    property AffixFlagMode: TdxAffixFlagMode read FAffixFlagMode write FAffixFlagMode;
    property AffixMorphologicAliases: PdxPWideCharArray read FAffixMorphologicAliases;
    property AffixMorphologicAliasesCount: Integer read FAffixMorphologicAliasesCount;
    property CodePage: Cardinal read FCodePage write FCodePage;
    property ComplexPrefixes: Boolean read FComplexPrefixes write FComplexPrefixes;
    property ForbiddenWordFlag: Word read FForbiddenWordFlag write FForbiddenWordFlag;
    property IgnoredChars: PWideChar read FIgnoredChars write FIgnoredChars;
    property Language: Integer read FLanguage write FLanguage;
  end;

implementation

uses
  dxHunspellUtils, dxHash, dxHashUtils;

{ TdxHunspellFlags }

constructor TdxHunspellFlags.Create(AAffixFlagMode: TdxAffixFlagMode);
begin
  inherited Create;
  FAffixFlagMode := AAffixFlagMode;
end;

constructor TdxHunspellFlags.CreateCapitalized(AFlags: TdxHunspellFlags;
  AAffixFlagMode: TdxAffixFlagMode);
begin
  Create(AAffixFlagMode);
  Length := AFlags.Length + 1;
  Move(AFlags.Data^, Data^, AFlags.Length * SizeOf(Word));
  FData[Length - 1] := dxOnlyUpcaseFlag;
end;

destructor TdxHunspellFlags.Destroy;
begin
  DisposeData;
  inherited Destroy;
end;

procedure TdxHunspellFlags.Assign(ASource: TdxHunspellFlags);
begin
  Length := ASource.Length;
  Move(ASource.Data^, Data^, ASource.Length * SizeOf(Word));
end;

procedure TdxHunspellFlags.AssignCapitalized(ASource: TdxHunspellFlags);
begin
  Length := ASource.Length + 1;
  Move(ASource.Data^, Data^, ASource.Length * SizeOf(Word));
  FData[Length - 1] := dxOnlyUpcaseFlag;
end;

procedure TdxHunspellFlags.DisposeData;
begin
  if FDataSize = 0 then Exit;
  FreeMem(FData, FDataSize);
  FData := nil;
  FDataSize := 0;
end;

function TdxHunspellFlags.InitializeByAlias(
  AWordStemManager: TdxHunspellWordStemManager; AAlias: PWideChar): Boolean;
var
  AAliasNum: Integer;
begin
  AAliasNum := StrInt(AAlias);
  Result := (AAliasNum > 0) and (AAliasNum <= AWordStemManager.AffixFlagAliases.Count);
  DisposeData;
  if Result then
  begin
    FLength := AWordStemManager.AffixFlagAliases[AAliasNum - 1].Length;
    FData := AWordStemManager.AffixFlagAliases[AAliasNum - 1].Data;
  end;
end;

function TdxHunspellFlags.ContainsFlag(AFlag: Word): Boolean;
begin
  Result := (Length > 0) and dxHunspellUtils.ContainsFlag(FData, Length, AFlag);
end;

function TdxHunspellFlags.Decode(AWordStemManager: TdxHunspellWordStemManager;
  AFlags: PWideChar): Boolean;
var
  I, ALength: Integer;
  AFlag, ATempPointer: PWideChar;
  ADecodedFlag: PWord;
begin
  Result := True;
  case FAffixFlagMode of
    afmTwoCharacters:
      begin
        Length := StrLen(AFlags) div 2;
        for I := 0 to Length - 1 do
          Data[I] := Ord((AFlags + I * 2)^) shl 8 +
            Ord((AFlags + I * 2 + 1)^);
      end;
    afmNumber:
      begin
        ALength := 1;
        AFlag := AFlags;
        ATempPointer := AFlags;
        while ATempPointer^ <> #0 do
        begin
          if ATempPointer^ = ',' then
            Inc(ALength);
          Inc(ATempPointer);
        end;
        Length := ALength;
        ADecodedFlag := @FData[0];
        ATempPointer := AFlags;
        while ATempPointer^ <> #0 do
        begin
          if ATempPointer^ = ',' then
          begin
            I := StrInt(AFlag);
            ADecodedFlag^ := I;
            AFlag := ATempPointer + 1;
            Inc(ADecodedFlag);
          end;
          Inc(ATempPointer);
        end;
        I := StrInt(AFlag);
        ADecodedFlag^ := I;
      end;
    afmUnicode:
      begin
        Length := StrLen(AFlags);
        Move(AFlags^, Data^, Length * SizeOf(Word));
      end;
  else
    Length := StrLen(AFlags);
    ADecodedFlag := @FData[0];
    ATempPointer := AFlags;
    while ATempPointer^ <> #0 do
    begin
      ADecodedFlag^ := Ord(ATempPointer^);
      Inc(ADecodedFlag);
      Inc(ATempPointer);
    end;
  end;
end;

procedure TdxHunspellFlags.Sort;
begin
  SortFlags(Data^, 0, Length);
end;

procedure TdxHunspellFlags.SetLength(ANewLength: Word);
begin
  if ANewLength <> Length then
  begin
    FLength := ANewLength;
    if ANewLength * SizeOf(Word) > FDataSize then
    begin
      FDataSize := ANewLength * SizeOf(Word);
      ReallocMem(FData, FDataSize);
    end;
  end;
end;

function TdxHunspellFlags.GetItem(Index: Integer): Word;
begin
  Result := FData[Index];
end;

{ TdxHunspellFlagsList }

function TdxHunspellFlagsList.GetItem(Index: Integer): TdxHunspellFlags;
begin
  Result := TdxHunspellFlags(inherited Items[Index]);
end;

{ TdxHunspellWordStem }

constructor TdxHunspellWordStem.Create(const AWord: PWideChar; AWordLength: Integer;
  const AIgnoreChars: PWideChar; AReverse: Boolean);
begin
  inherited Create;
  FWordStem := StrNew(AWord);
  FWordStemLength := AWordLength;
  RemoveIgnoredChars(FWordStem, AIgnoreChars);
  if AReverse then
  begin
    Include(FOptions, wsoReverse);
    StrReverse(FWordStem);
  end;
end;

destructor TdxHunspellWordStem.Destroy;
begin
  StrDispose(FWordStem);
  if (FMorphologicalDescription <> nil) and not MorphologyAliases then
    StrDispose(FMorphologicalDescription);
  FreeAndNil(FAffixFlags);
  inherited Destroy;
end;

function TdxHunspellWordStem.IsCompatibleWithFlag(AFlag: Word;
  CompatibleIfNull: Boolean = False): Boolean;
begin
  if AFlag <> dxNullFlag then
    Result := (FAffixFlags <> nil) and FAffixFlags.ContainsFlag(AFlag)
  else
    Result := CompatibleIfNull;
end;

procedure TdxHunspellWordStem.ProcessFlags(AWordStemManager: TdxHunspellWordStemManager);
begin
  if IsCompatibleWithFlag(dxOnlyUpcaseFlag) then
    Include(FOptions, wsoUpcaseOnly)
  else
    Exclude(FOptions, wsoUpcaseOnly);
  if IsCompatibleWithFlag(AWordStemManager.FForbiddenWordFlag) then
    Include(FOptions, wsoForbidden)
  else
    Exclude(FOptions, wsoForbidden);
end;

procedure TdxHunspellWordStem.SetAffixFlags(const Value: TdxHunspellFlags);
begin
  FreeAndNil(FAffixFlags);
  FAffixFlags := Value;
end;

procedure TdxHunspellWordStem.SetMorphology(
  const AMorphologocalDescription: PWideChar);
begin
  FMorphologicalDescription := StrNew(AMorphologocalDescription);
  if wsoReverse in Options then
    StrReverse(FMorphologicalDescription);
  UpdateOptionsByMorphology;
end;

procedure TdxHunspellWordStem.SetMorphologyByAlias(
  const AMorphologocalDescription: PWideChar);
begin
  Include(FOptions, wsoMorphologyAliases);
  FMorphologicalDescription := AMorphologocalDescription;
  UpdateOptionsByMorphology;
end;

procedure TdxHunspellWordStem.UpdateOptions(const AOptions: TdxWordStemOptions);
begin
  FOptions := FOptions + AOptions;
end;

function TdxHunspellWordStem.GetCompound: Boolean;
begin
  Result := wsoCompound in FOptions;
end;

function TdxHunspellWordStem.GetCompoundBegin: Boolean;
begin
  Result := wsoCompoundBegin in FOptions;
end;

function TdxHunspellWordStem.GetCompoundEnd: Boolean;
begin
  Result := wsoCompoundEnd in FOptions;
end;

function TdxHunspellWordStem.GetCompoundMiddle: Boolean;
begin
  Result := wsoCompoundMiddle in FOptions;
end;

function TdxHunspellWordStem.GetCompoundRoot: Boolean;
begin
  Result := wsoCompoundRoot in FOptions;
end;

function TdxHunspellWordStem.GetForbidden: Boolean;
begin
  Result := wsoForbidden in FOptions;
end;

function TdxHunspellWordStem.GetInCompoundOnly: Boolean;
begin
  Result := wsoInCompoundOnly in FOptions;
end;

function TdxHunspellWordStem.GetKeepCase: Boolean;
begin
  Result := wsoKeepCase in FOptions;
end;

function TdxHunspellWordStem.GetMorphologyAliases: Boolean;
begin
  Result := wsoMorphologyAliases in FOptions;
end;

function TdxHunspellWordStem.GetMorphologyPhone: Boolean;
begin
  Result := wsoMorphologyPhone in FOptions;
end;

function TdxHunspellWordStem.GetNeedAffix: Boolean;
begin
  Result := wsoNeedAffix in FOptions;
end;

function TdxHunspellWordStem.GetNoSuggest: Boolean;
begin
  Result := wsoNoSuggest in FOptions;
end;

function TdxHunspellWordStem.GetUpcaseOnly: Boolean;
begin
  Result := wsoUpcaseOnly in FOptions;
end;

procedure TdxHunspellWordStem.UpdateOptionsByMorphology;
begin
  Include(FOptions, wsoMorphologyDescription);
  if StrPos(FMorphologicalDescription, mlPhon) <> nil then
    Include(FOptions, wsoMorphologyPhone);
end;

{ TdxHunspellReader }

constructor TdxHunspellReader.Create(AStream: TStream; ACodePage: Integer);
begin
  inherited Create;
  FData := TdxSpellCheckerStrings.Create(ACodePage);
  LoadFromStream(AStream);
end;

destructor TdxHunspellReader.Destroy;
begin
  FData.Free;
  inherited Destroy;
end;

function TdxHunspellReader.GetNextLine(var ABuffer; ABufferSize: Integer): Boolean;
var
  L: Integer;
begin
  Result := (FData <> nil) and (FLineIndex < FData.Count);
  if Result then
  begin
    L := FData.Lengths[FLineIndex];
    if L * SizeOf(WideChar) > ABufferSize then
     FillChar(ABuffer, SizeOf(WideChar), 0)
    else
    begin
      Move(FData[FLineIndex]^, ABuffer, L * SizeOf(WideChar));
      (PWideChar(@ABuffer) + L)^ := #0;
    end;
  end;
  Inc(FLineIndex);
end;

procedure TdxHunspellReader.Reset;
begin
  FLineIndex := 0;
end;

procedure TdxHunspellReader.LoadFromStream(AStream: TStream);
begin
  AStream.Position := 0;
  FData.Capacity := 16384;
  FData.LoadFromStream(AStream);
end;

{ TdxHunspellWordStemManager }

constructor TdxHunspellWordStemManager.Create(ALanguage: Integer);
begin
  inherited Create;
  FComplexPrefixes := False;
  FLanguage := ALanguage;
  FForbiddenWordFlag := dxForbiddenWordFlag;
  FAffixFlagAliases := TdxHunspellFlagsList.Create;
end;

destructor TdxHunspellWordStemManager.Destroy;
begin
  FreeHashTable;
  FreeAndNil(FAffixFlagAliases);
  FreeAffixMorphologicAliases;
  StrDispose(FIgnoredChars);
  inherited Destroy;
end;

procedure TdxHunspellWordStemManager.AllocateAffixMorphologicAliases(ASize: Integer);
begin
  FAffixMorphologicAliases := AllocMem(ASize * SizeOf(PWideChar));
  FAffixMorphologicAliasesCount := ASize;
end;

function TdxHunspellWordStemManager.Load(
  ADictionaryStream: TStream): Boolean;
begin
  Result := LoadWordStems(ADictionaryStream);
  if not Result then
    FreeHashTable;
end;

function TdxHunspellWordStemManager.Lookup(const AWord: PWideChar): TdxHunspellWordStem;
begin
  Result := FHashTable[Hash(AWord)];
  while (Result <> nil) and not StrEquals(AWord, Result.WordStem) do
    Result := Result.Next;
end;

function TdxHunspellWordStemManager.GetFirstItem(out ATableIndex: Integer): TdxHunspellWordStem;
begin
  ATableIndex := -1;
  Result := GetNextItem(ATableIndex, nil);
end;

function TdxHunspellWordStemManager.GetNextItem(var ATableIndex: Integer;
  AWordStem: TdxHunspellWordStem): TdxHunspellWordStem;
var
  I: Integer;
begin
  if (AWordStem <> nil) and (AWordStem.Next <> nil) then
  begin
    Result := AWordStem.Next;
    Exit;
  end;
  for I := ATableIndex + 1 to FHashTableSize - 1 do
  begin
    Result := FHashTable[I];
    if Result <> nil then
    begin
      ATableIndex := I;
      Exit;
    end;
  end;
  ATableIndex := -1;
  Result := nil;
end;

procedure TdxHunspellWordStemManager.AddWord(const AWord: PWideChar;
  AWordLength: Integer; AAffixFlags: TdxHunspellFlags;
  const AMorphologocalDescription: PWideChar; AOnlyUpperCase: Boolean);

  procedure AssignFlags(AWordStem: TdxHunspellWordStem);
  begin
    if AOnlyUpperCase then
      AWordStem.AffixFlags.AssignCapitalized(AAffixFlags)
    else
      AWordStem.AffixFlags.Assign(AAffixFlags);
    AWordStem.ProcessFlags(Self);
  end;

  function InitializeWordStem(AWordStem: TdxHunspellWordStem): TdxHunspellWordStem;
  var
    AFlags: TdxHunspellFlags;
  begin
    if AOnlyUpperCase then
      AFlags := TdxHunspellFlags.CreateCapitalized(AAffixFlags, FAffixFlagMode)
    else
    begin
      AFlags := TdxHunspellFlags.Create(FAffixFlagMode);
      AFlags.Assign(AAffixFlags);
    end;
    AWordStem.AffixFlags := AFlags;
    AWordStem.ProcessFlags(Self);
    Result := AWordStem;
  end;

  function ProcessItems(var ANewWordStem: TdxHunspellWordStem;
    const AExistingWordStem: TdxHunspellWordStem;
    var AHasUpperCaseHomonym: Boolean; ACheckNextHomonym: Boolean): Boolean;
  begin
    Result := True;
    if (not ACheckNextHomonym or (AExistingWordStem.NextHomonym = nil)) and
      StrEquals(ANewWordStem.WordStem, AExistingWordStem.WordStem) then
    begin
      if not AOnlyUpperCase then
      begin
        if AExistingWordStem.UpcaseOnly then
        begin
          FreeAndNil(ANewWordStem);
          AssignFlags(AExistingWordStem);
          Result := False;
        end
        else
          AExistingWordStem.NextHomonym := InitializeWordStem(ANewWordStem);
      end
      else
        AHasUpperCaseHomonym := True;
    end;
  end;

var
  AHasUpperCaseHomonym: Boolean;
  ATableIndex: Integer;
  ANewWordStem, AExistingWordStem: TdxHunspellWordStem;
begin
  ANewWordStem := CreateWordStem(AWord, AWordLength, AMorphologocalDescription);
  ATableIndex := Hash(ANewWordStem.WordStem);
  AExistingWordStem := FHashTable[ATableIndex];
  if AExistingWordStem = nil then
  begin
    FHashTable[ATableIndex] := InitializeWordStem(ANewWordStem);
    Exit;
  end;
  AHasUpperCaseHomonym := False;
  while AExistingWordStem.Next <> nil do
  begin
    if not ProcessItems(ANewWordStem, AExistingWordStem, AHasUpperCaseHomonym, True) then
      Exit;
    AExistingWordStem := AExistingWordStem.Next;
  end;
  if not ProcessItems(ANewWordStem, AExistingWordStem, AHasUpperCaseHomonym, False) then
    Exit;
  if not AHasUpperCaseHomonym then
    AExistingWordStem.Next := InitializeWordStem(ANewWordStem)
  else
    FreeAndNil(ANewWordStem);
end;

function TdxHunspellWordStemManager.AllocateHashTable(S: PWideChar): Boolean;
const
  dxHashDelta = 1005;
var
  AStemCount: Integer;
begin
  RemoveCRLF(S);
  AStemCount := StrInt(S);
  Result := AStemCount > 0;
  if Result then
  begin
    FHashTableSize := (AStemCount + dxHashDelta) or $00000001;
    FHashTable := AllocMem(FHashTableSize * SizeOf(TdxHunspellWordStem));
  end;
end;

procedure TdxHunspellWordStemManager.CheckAddCapitalizedWord(AWord: PWideChar;
  AWordLength: Integer; AAffixFlags: TdxHunspellFlags; ADescription: PWideChar);
var
  ACapitalizationType: TdxCapitalizationType;
begin
  ACapitalizationType := GetWordCapitalizationType(AWord, AWordLength, Language);
  if (ACapitalizationType in [ctMixed, ctMixedCapitalized, ctUpper]) and
    not AAffixFlags.ContainsFlag(FForbiddenWordFlag) then
  begin
    MakeAllSmall(AWord, Language, AWordLength);
    MakeCapitalized(AWord, Language);
    AddWord(AWord, AWordLength, AAffixFlags, ADescription, True);
  end;
end;

procedure TdxHunspellWordStemManager.FreeAffixMorphologicAliases;
var
  I: Integer;
begin
  if FAffixMorphologicAliases = nil then Exit;
  for I := 0 to FAffixMorphologicAliasesCount - 1 do
    StrDispose(FAffixMorphologicAliases[I]);
  FreeMem(FAffixMorphologicAliases, FAffixMorphologicAliasesCount * SizeOf(PWideChar));
  FAffixMorphologicAliases := nil;
  FAffixMorphologicAliasesCount := 0;
end;

procedure TdxHunspellWordStemManager.FreeHashTable;
var
  I: Integer;
  AWordStem, ANextWordStem: TdxHunspellWordStem;
begin
  if FHashTable = nil then Exit;
  for I := 0 to FHashTableSize - 1 do
  begin
    AWordStem := FHashTable[I];
    while AWordStem <> nil  do
    begin
      ANextWordStem := AWordStem.Next;
      FreeAndNil(AWordStem);
      AWordStem := ANextWordStem;
    end;
  end;
  FreeMem(FHashTable, FHashTableSize * SizeOf(TdxHunspellWordStem));
  FHashTable := nil;
  FHashTableSize := 0;
end;

function TdxHunspellWordStemManager.Hash(S: PWideChar): Cardinal;
begin
  Result := Cardinal(dxElfHash(S, 0, nil, 0)) mod Cardinal(FHashTableSize);
end;

function TdxHunspellWordStemManager.LoadWordStems(AStream: TStream): Boolean;
var
  ALineCursor: PWideChar;
  AReader: TdxHunspellReader;
  ALineBuffer: TdxLineBuffer;
begin
  Result := False;
  AReader := TdxHunspellReader.Create(AStream, CodePage);
  try
    ALineCursor := @ALineBuffer;
    if not AReader.GetNextLine(ALineBuffer, SizeOf(ALineBuffer)) then
      Exit;
    if not AllocateHashTable(ALineCursor) then
      Exit;
    while AReader.GetNextLine(ALineBuffer, SizeOf(ALineBuffer)) do
      ParseWordStem(ALineCursor);
  finally
    AReader.Free;
  end;
  Result := not dxHunspellWarningFlag;
end;

procedure TdxHunspellWordStemManager.ParseWordStem(ALine: PWideChar);
var
  ALength: Integer;
  AAffixes, AMorphology: PWideChar;
  AAffixFlags: TdxHunspellFlags;
begin
  RemoveCRLF(ALine);
  AMorphology := GetMorphologyDescription(ALine);
  AAffixes := GetAffixesDescription(ALine);
  AAffixFlags := TdxHunspellFlags.Create(FAffixFlagMode);
  try
    if not InitializeAffixFlags(AAffixFlags, AAffixes) then
      Exit;
    ALength := StrLen(ALine);
    AddWord(ALine, ALength, AAffixFlags, AMorphology, False);
    CheckAddCapitalizedWord(ALine, ALength, AAffixFlags, AMorphology);
  finally
    FreeAndNil(AAffixFlags);
  end;
end;

function TdxHunspellWordStemManager.HasAffixFlagAliases: Boolean;
begin
  Result := FAffixFlagAliases.Count > 0;
end;

function TdxHunspellWordStemManager.HasAffixMorphologicAliases: Boolean;
begin
  Result := FAffixMorphologicAliases <> nil;
end;

function TdxHunspellWordStemManager.InitializeAffixFlags(
  AAffixFlags: TdxHunspellFlags; ADescription: PWideChar): Boolean;
begin
  Result := True;
  if ADescription = nil then Exit;
  ADescription^ := #0;
  if FAffixFlagAliases.Count > 0 then
  begin
    Result := AAffixFlags.InitializeByAlias(Self, ADescription + 1);
    if not Result then
      ADescription^ := #0;
  end
  else
  begin
    Result := AAffixFlags.Decode(Self, ADescription + 1);
    if Result then
      AAffixFlags.Sort;
  end;
  if not Result then
    AAffixFlags.Length := 0;
end;

function TdxHunspellWordStemManager.GetAffixesDescription(
  ALine: PWideChar): PWideChar;
var
  ATemp: PWideChar;
begin
  Result := StrScan(ALine, '/');
  while (Result <> nil) do
  begin
    if Result = ALine then
    begin
      Inc(Result);
      Continue;
    end
    else
      if (Result - 1)^ <> '\' then
        Break;
    ATemp := Result - 1;
    while ATemp^ <> #0 do
    begin
      ATemp^ := (ATemp + 1)^;
      Inc(ATemp);
    end;
    Result := StrScan(Result, '/');
  end;
end;

function TdxHunspellWordStemManager.GetAffixMorphologyByAlias(AAlias: Integer): PWideChar;
begin
  if (AAlias > 0) and (AAlias <= FAffixMorphologicAliasesCount) then
    Result := FAffixMorphologicAliases[AAlias - 1]
  else
    Result := nil;
end;

function TdxHunspellWordStemManager.GetMorphologyDescription(
  ALine: PWideChar): PWideChar;
var
  ATabPosition: PWideChar;
begin
  Result := StrScan(ALine, ':');
  while Result <> nil do
  begin
    if (Result > ALine + 3) and (((Result - 3)^ = ' ') or ((Result - 3)^ = #9)) then
    begin
      Dec(Result, 4);
      while (Result >= ALine) and ((Result^ = ' ') or (Result^ = #9)) do
        Dec(Result);
      if Result < ALine then
        Result := nil
      else
      begin
        (Result + 1)^ := #0;
        Inc(Result, 2);
      end;
      Break;
    end;
    Inc(Result);
    Result := StrScan(Result, ':');
  end;
  ATabPosition := StrScan(ALine, #9);
  if (ATabPosition <> nil) and ((Result = nil) or (ATabPosition < Result)) then
  begin
    ATabPosition^ := #0;
    Result := ATabPosition + 1;
  end;
end;

function TdxHunspellWordStemManager.CreateWordStem(const AWord: PWideChar; AWordLength: Integer;
  const AMorphologyDescription: PWideChar): TdxHunspellWordStem;
begin
  Result := TdxHunspellWordStem.Create(AWord, AWordLength, FIgnoredChars, FComplexPrefixes);
  SetupMorphology(Result, AMorphologyDescription);
end;

procedure TdxHunspellWordStemManager.SetupMorphology(var AWordStem: TdxHunspellWordStem;
  const AMorphologyDescription: PWideChar);
begin
  if AMorphologyDescription = nil then Exit;
  if HasAffixMorphologicAliases then
    AWordStem.SetMorphologyByAlias(GetAffixMorphologyByAlias(StrInt(AMorphologyDescription)))
  else
    AWordStem.SetMorphology(AMorphologyDescription);
end;

end.
