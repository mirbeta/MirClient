{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressRichEditControl                                   }
{                                                                    }
{           Copyright (c) 2000-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSRICHEDITCONTROL AND ALL        }
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

unit dxRichEdit.Control.Hyphenations;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Windows, Classes, Types, SysUtils, Graphics, Generics.Defaults, Generics.Collections,
  dxCore, cxGeometry, dxCoreClasses,
  dxRichEdit.Utils.Types,
  dxGenerics;

type

  { IdxHyphenationService }

  IdxHyphenationService = interface
  ['{545C6D4B-1624-4592-944B-F633FE62D693}']
    function Hyphenate(const AOriginalWord: string): TdxIntegerList;
  end;

  { TdxHyphenator }

  TdxHyphenator = class
  strict private
    FPatternTable: TDictionary<string, TArray<Integer>>;
    FExceptionTable: TdxNamedObjectDictionary<TdxIntegerList>;
    FMaxPatternLength: Integer;
    FLeftHyphenMin: Integer;
    FRightHyphenMin: Integer;
    FExtendedWord: string;
    FExtendedWordLength: Integer;
    FPatternCharsValues: TArray<Integer>;
  protected
    procedure AddException(const AException: string);
    class function GetPatternStringLength(const APattern: string): Integer; static;
    procedure ApplySubPatterns(const APattern: string; var APatternArray: TArray<Integer>);
    procedure AddPattern(const APattern: string);
    class function CreatePatternString(const APattern: string; var APatternArray: TArray<Integer>): string; static;
    class procedure ApplyPattern(var AValues: TArray<Integer>; const APattern: TArray<Integer>; APosition: Integer); static;
    class function IsOdd(ANumber: Integer): Boolean; static;
    procedure ApplyPatternToSubwords(AMaxSubwordLength, ASubwordsStartIndex: Integer);
    function GetHyphenPositions(const AWord: string; AOffset: Integer): TdxIntegerList;
    function HyphenatePart(const AOriginalWord: string; AOffset, ALength: Integer): TdxIntegerList;
    function HyphenateCore(const AOriginalWord: string; AOffset: Integer): TdxIntegerList;
  public
    constructor Create(const APatternFileName: string); overload;
    constructor Create(const APatternsFileName, AExceptionsFileName: string); overload;
    constructor Create(APatternStream: TStream); overload;
    constructor Create(APatternStream, AExceptionsStream: TStream); overload;
    destructor Destroy; override;
    procedure LoadPatterns(const AFileName: string); overload;
    procedure LoadExceptions(const AFileName: string); overload;
    procedure LoadPatterns(AStream: TStream); overload;
    procedure LoadExceptions(AStream: TStream); overload;
    function Hyphenate(const AOriginalWord: string): TdxIntegerList; virtual;

    property LeftHyphenMin: Integer read FLeftHyphenMin write FLeftHyphenMin;
    property RightHyphenMin: Integer read FRightHyphenMin write FRightHyphenMin;
  end;

  { TdxEmptyHyphenator }

  TdxEmptyHyphenator = class(TdxHyphenator)
  public
    constructor Create;
    function Hyphenate(const AOriginalWord: string): TdxIntegerList; override;
  end;

  { TdxHyphenationService }

  TdxHyphenationService = class(TInterfacedObject, IdxHyphenationService)
  strict private
    FHyphenator: TdxHyphenator;
  public
    constructor Create(AHyphenator: TdxHyphenator);
    destructor Destroy; override;
    function Hyphenate(const AOriginalWord: string): TdxIntegerList;
  end;

implementation

uses
  Character, Math,
  dxRichEdit.DocumentModel.Core;

{ TdxHyphenator }

constructor TdxHyphenator.Create(const APatternsFileName, AExceptionsFileName: string);
begin
  inherited Create;
  FPatternTable := TDictionary<string, TArray<Integer>>.Create;
  FExceptionTable := TdxNamedObjectDictionary<TdxIntegerList>.Create(True);
  FLeftHyphenMin := 2;
  FRightHyphenMin := 2;
  LoadPatterns(APatternsFileName);
  LoadExceptions(AExceptionsFileName);
end;

constructor TdxHyphenator.Create(APatternStream, AExceptionsStream: TStream);
begin
  inherited Create;
  FPatternTable := TDictionary<string, TArray<Integer>>.Create;
  FExceptionTable := TdxNamedObjectDictionary<TdxIntegerList>.Create(True);
  FLeftHyphenMin := 2;
  FRightHyphenMin := 2;
  LoadPatterns(APatternStream);
  LoadExceptions(AExceptionsStream);
end;

constructor TdxHyphenator.Create(APatternStream: TStream);
begin
  Create(APatternStream, nil);
end;

constructor TdxHyphenator.Create(const APatternFileName: string);
begin
  Create(APatternFileName, '');
end;

destructor TdxHyphenator.Destroy;
begin
  FreeAndNil(FPatternTable);
  FreeAndNil(FExceptionTable);
  inherited Destroy;
end;

procedure TdxHyphenator.LoadPatterns(const AFileName: string);
var
  AStream: TdxMemoryStream;
begin
  if not FileExists(AFileName) then
    Exit;

  AStream := TdxMemoryStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadPatterns(AStream);
  finally
    AStream.Free;
  end;
end;

procedure TdxHyphenator.LoadExceptions(const AFileName: string);
var
  AStream: TdxMemoryStream;
begin
  if not FileExists(AFileName) then
    Exit;

  AStream := TdxMemoryStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
  try
    LoadExceptions(AStream);
  finally
    AStream.Free;
  end;
end;

procedure TdxHyphenator.LoadPatterns(AStream: TStream);
var
  AStrings: TStringList;
  I: Integer;
begin
  if AStream = nil then
    Exit;

  AStrings := TStringList.Create;
  try
    AStrings.LoadFromStream(AStream, TEncoding.Unicode);
    for I := 0 to AStrings.Count - 1 do
      AddPattern(AStrings[I]);
  finally
    AStrings.Free;
  end;
end;

procedure TdxHyphenator.LoadExceptions(AStream: TStream);
var
  AStrings: TStringList;
  I: Integer;
begin
  if AStream = nil then
    Exit;

  AStrings := TStringList.Create;
  try
    AStrings.LoadFromStream(AStream, TEncoding.Unicode);
    for I := 0 to AStrings.Count - 1 do
      AddException(AStrings[I]);
  finally
    AStrings.Free;
  end;
end;

procedure TdxHyphenator.AddException(const AException: string);
var
  ALength, I: Integer;
  APositions: TdxIntegerList;
  ARealWord: string;
begin
  ALength := Length(AException);
  if ALength = 0 then
    Exit;

  APositions := TdxIntegerList.Create;
  for I := 1 to ALength do
  begin
    if AException[I] = '-' then
      APositions.Add(I - APositions.Count);
  end;

  ARealWord := StringReplace(AException, '-', '', [rfReplaceAll]);
  FExceptionTable.Add(ARealWord, APositions);
end;

class function TdxHyphenator.GetPatternStringLength(const APattern: string): Integer;
var
  ACount, I: Integer;
begin
  Result := 0;
  ACount := Length(APattern);
  for I := 1 to ACount do
  begin
    if not {$IFDEF DELPHIXE4}APattern[I].IsDigit{$ELSE}TCharacter.IsDigit(APattern[I]){$ENDIF} then
      Inc(Result);
  end;
end;

procedure TdxHyphenator.ApplySubPatterns(const APattern: string; var APatternArray: TArray<Integer>);
var
  ASubPatternArray: TArray<Integer>;
  APatternString: string;
begin
  APatternString := APattern;
  while Length(APatternString) > 0 do
  begin
    Delete(APatternString, Length(APatternString), 1);
    if FPatternTable.TryGetValue(APatternString, ASubPatternArray) and (ASubPatternArray <> nil) then
      ApplyPattern(APatternArray, ASubPatternArray, 0);
  end;
end;

procedure TdxHyphenator.AddPattern(const APattern: string);
var
  APatternLength: Integer;
  APatternArray: TArray<Integer>;
  APatternString: string;
begin
  if Length(APattern) = 0 then
    Exit;
  APatternLength := GetPatternStringLength(APattern);
  SetLength(APatternArray, APatternLength + 1);
  ZeroMemory(@APatternArray[0], (APatternLength + 1) * SizeOf(Integer));
  FMaxPatternLength := Max(APatternLength, FMaxPatternLength);
  APatternString := CreatePatternString(APattern, APatternArray);
  ApplySubPatterns(APatternString, APatternArray);
  FPatternTable.Add(APatternString, APatternArray);
end;

class function TdxHyphenator.CreatePatternString(const APattern: string; var APatternArray: TArray<Integer>): string;
var
  ACount, APatternArrayIndex, I, ANumericValue: Integer;
  ABuilder: TStringBuilder;
begin
  ACount := Length(APattern);
  ABuilder := TStringBuilder.Create(ACount);
  try
    APatternArrayIndex := 0;
    for I := 1 to ACount do
    begin
      ANumericValue := Trunc({$IFDEF DELPHIXE4}APattern[I].GetNumericValue{$ELSE}TCharacter.GetNumericValue(APattern[I]){$ENDIF});
      if ANumericValue >= 0 then
        APatternArray[APatternArrayIndex] := ANumericValue
      else
      begin
        Inc(APatternArrayIndex);
        ABuilder.Append(APattern[I]);
      end;
    end;
    Result := ABuilder.ToString;
  finally
    ABuilder.Free;
  end;
end;

class procedure TdxHyphenator.ApplyPattern(var AValues: TArray<Integer>; const APattern: TArray<Integer>; APosition: Integer);
var
  ALength, I: Integer;
begin
  ALength := Length(APattern);
  for I := 0 to ALength - 1 do
    AValues[I + APosition] := Max(AValues[I + APosition], APattern[I]);
end;

class function TdxHyphenator.IsOdd(ANumber: Integer): Boolean;
begin
  Result := (ANumber and 1) = 1;
end;

procedure TdxHyphenator.ApplyPatternToSubwords(AMaxSubwordLength, ASubwordsStartIndex: Integer);
var
  ASubStrLength: Integer;
  ASubword: string;
  APattern: TArray<Integer>;
begin
  AMaxSubwordLength := Min(AMaxSubwordLength, FExtendedWordLength - (ASubwordsStartIndex - 1));
  for ASubStrLength := AMaxSubwordLength downto 1 do
  begin
    ASubword := Copy(FExtendedWord, ASubwordsStartIndex, ASubStrLength);
    if FPatternTable.TryGetValue(ASubword, APattern) and (APattern <> nil) then
    begin
      ApplyPattern(FPatternCharsValues, APattern, ASubwordsStartIndex - 1);
      Exit;
    end;
  end;
end;

function TdxHyphenator.GetHyphenPositions(const AWord: string; AOffset: Integer): TdxIntegerList;
var
  AWordLength, AMaxHyphenPosition, I: Integer;
  ACanAddHyphen: Boolean;
begin
  Result := TdxIntegerList.Create;
  AWordLength := Length(AWord);
  AMaxHyphenPosition := AWordLength - RightHyphenMin - 1;
  for I := 0 to AWordLength - 1 do
  begin
    ACanAddHyphen := (IsOdd(FPatternCharsValues[I + 1]) and (I >= LeftHyphenMin)) and (I <= AMaxHyphenPosition);
    if ACanAddHyphen then
      Result.Add(I + AOffset + 1);
  end;
end;

function TdxHyphenator.Hyphenate(const AOriginalWord: string): TdxIntegerList;
var
  ALength, AStartOfWord, I, APrevWordLength: Integer;
  AWord: string;
  APart: TdxIntegerList;
begin
  Result := TdxIntegerList.Create;
  AWord := {$IFDEF DELPHIXE4}AOriginalWord.ToLower{$ELSE}TCharacter.ToLower(AOriginalWord){$ENDIF};
  ALength := Length(AWord);
  AStartOfWord := 1;
  for I := 1 to ALength do
  begin
    if {$IFDEF DELPHIXE4}AWord[I].IsLetter{$ELSE}TCharacter.IsLetter(AWord[I]){$ENDIF} then
      Continue;

    APrevWordLength := I - AStartOfWord;
    if APrevWordLength > 0 then
    begin
      APart := HyphenatePart(AWord, AStartOfWord, APrevWordLength);
      try
        Result.AddRange(APart);
      finally
        APart.Free;
      end;
    end;
    AStartOfWord := I + 1;
  end;
  if AStartOfWord = 1 then
  begin
    Result.Free;
    Exit(HyphenateCore(AWord, 0));
  end;

  if AStartOfWord < ALength then
  begin
    APart := HyphenatePart(AWord, AStartOfWord, ALength - AStartOfWord + 1);
    try
      Result.AddRange(APart);
    finally
      APart.Free;
    end;
  end;
end;

function TdxHyphenator.HyphenatePart(const AOriginalWord: string; AOffset, ALength: Integer): TdxIntegerList;
begin
  Result := HyphenateCore(Copy(AOriginalWord, AOffset, ALength), AOffset - 1);
end;

function TdxHyphenator.HyphenateCore(const AOriginalWord: string; AOffset: Integer): TdxIntegerList;
var
  AException: TdxIntegerList;
  AMaxSubwordLength, I: Integer;
begin
  if FExceptionTable.TryGetValue(AOriginalWord, AException) then
  begin
    Result := TdxIntegerList.Create;
    Result.AddRange(AException);
    Exit;
  end;

  FExtendedWord := '.' + AOriginalWord + '.';
  FExtendedWordLength := Length(FExtendedWord);
  SetLength(FPatternCharsValues, FExtendedWordLength + 1);
  ZeroMemory(@FPatternCharsValues[0], (FExtendedWordLength + 1) * SizeOf(Integer));
  AMaxSubwordLength := Min(FExtendedWordLength, FMaxPatternLength);
  for I := 1 to FExtendedWordLength do
    ApplyPatternToSubwords(AMaxSubwordLength, I);
  Result := GetHyphenPositions(AOriginalWord, AOffset);
end;

{ TdxEmptyHyphenator }

constructor TdxEmptyHyphenator.Create;
begin
  inherited Create('', '');
end;

function TdxEmptyHyphenator.Hyphenate(const AOriginalWord: string): TdxIntegerList;
begin
  Result := TdxIntegerList.Create;
end;

{ TdxHyphenationService }

constructor TdxHyphenationService.Create(AHyphenator: TdxHyphenator);
begin
  inherited Create;
  FHyphenator := AHyphenator;
end;

destructor TdxHyphenationService.Destroy;
begin
  FreeAndNil(FHyphenator);
  inherited Destroy;
end;

function TdxHyphenationService.Hyphenate(const AOriginalWord: string): TdxIntegerList;
begin
  Result := FHyphenator.Hyphenate(AOriginalWord);
end;

end.
