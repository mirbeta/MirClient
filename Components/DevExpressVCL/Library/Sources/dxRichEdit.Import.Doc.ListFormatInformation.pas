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
unit dxRichEdit.Import.Doc.ListFormatInformation;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, Classes, Generics.Defaults, Generics.Collections, dxCore, dxCoreClasses,
  dxCoreGraphics, dxGenerics,

  dxRichEdit.DocumentModel.NumberingFormatting,
  dxRichEdit.Doc.Utils;

type
  { TdxDocListLevelFormatFlags }

  TdxDocListLevelFormatFlags = class
  public const
    ConvertPreviousLevelNumberingToDecimal = $04;
    NoRestart                              = $08;
    RemoveIndent                           = $10;
    Converted                              = $20;
    Legacy                                 = $40;
    Tentative                              = $80;
  end;

  { TdxDocListFormatting }

  TdxDocListFormatting = class
  public const
    MaxLevelCount = 9;
  strict private
    FListIdentifier: Integer;
    FTemplateCode: Integer;
    FLevelStyleIdentifiers: TArray<SmallInt>;
    FSimpleList: Boolean;
    FHybrid: Boolean;
    FAutonumbered: Boolean;
    FHtmlCompatibilitiesFlags: Byte;
  protected
    procedure Read(AReader: TBinaryReader);
  public
    constructor Create;
    class function FromStream(AReader: TBinaryReader): TdxDocListFormatting; static;
    procedure Write(AWriter: TBinaryWriter);

    property ListIdentifier: Integer read FListIdentifier write FListIdentifier;
    property TemplateCode: Integer read FTemplateCode write FTemplateCode;
    property LevelStyleIdentifiers: TArray<SmallInt> read FLevelStyleIdentifiers write FLevelStyleIdentifiers;
    property SimpleList: Boolean read FSimpleList write FSimpleList;
    property Autonumbered: Boolean read FAutonumbered write FAutonumbered;
    property Hybrid: Boolean read FHybrid write FHybrid;
  end;

  { TdxDocListLevelProperties }

  TdxDocListLevelProperties = class
  public const
    BulletFormatCode = $17;
    Nothing          = #0;
    Space            = ' ';
  strict private
    FStart: Integer;
    FNumberingFormat: TdxNumberingFormat;
    FBulletedList: Boolean;
    FConvertPreviousLevelNumberingToDecimal: Boolean;
    FLegacy: Boolean;
    FRestartAfterLevelLimit: Boolean;
    FAlignment: TdxListNumberAlignment;
    FLevelFormatFlags: Byte;
    FSeparator: Char;
    FLegacySpace: Integer;
    FLegacyIndent: Integer;
    FCharacterUPXsize: Byte;
    FParagraphUPXsize: Byte;
    FRelativeRestartLevel: Byte;
    FHtmlCompatibilitiesFlags: Byte;
  protected
    FPlaceholderIndices: TBytes;
    procedure Read(AReader: TBinaryReader);
  public
    class function FromStream(AReader: TBinaryReader): TdxDocListLevelProperties; static;
    procedure Write(AWriter: TBinaryWriter);
    function CalcSeparator(ASeparatorCode: Byte): Char;
    function CalcSeparatorCode: Byte;

    property Start: Integer read FStart write FStart;
    property NumberingFormat: TdxNumberingFormat read FNumberingFormat write FNumberingFormat;
    property BulletedList: Boolean read FBulletedList;
    property ConvertPreviousLevelNumberingToDecimal: Boolean read FConvertPreviousLevelNumberingToDecimal write FConvertPreviousLevelNumberingToDecimal;
    property Legacy: Boolean read FLegacy write FLegacy;
    property RestartAfterLevelLimit: Boolean read FRestartAfterLevelLimit write FRestartAfterLevelLimit;
    property Alignment: TdxListNumberAlignment read FAlignment write FAlignment;
    property PlaceholderIndices: TBytes read FPlaceholderIndices write FPlaceholderIndices;
    property Separator: Char read FSeparator write FSeparator;
    property LegacySpace: Integer read FLegacySpace write FLegacySpace;
    property LegacyIndent: Integer read FLegacyIndent write FLegacyIndent;
    property CharacterUPXSize: Byte read FCharacterUPXsize write FCharacterUPXSize;
    property ParagraphUPXSize: Byte read FParagraphUPXsize write FParagraphUPXSize;
    property RestartLevelLimit: Byte read FRelativeRestartLevel write FRelativeRestartLevel;
  end;

  { TdxDocListLevel }

  TdxDocListLevel = class
  strict private
    FListLevelProperties: TdxDocListLevelProperties;
    FCharacterUPX: TBytes;
    FParagraphUPX: TBytes;
    FDisplayTextPlaceHolder: string;
    FCurrentLevel: Integer;
  protected
    class function IsPercent(ACh: Char): Boolean; static;
    class function IsDoublePercent(const ADisplayFormatString: string; I: Integer): Boolean; static;
    procedure Read(AReader: TBinaryReader);
    function GetDisplayTextPlaceHolder(AReader: TBinaryReader): string;
  public
    destructor Destroy; override;
    class function FromStream(AReader: TBinaryReader): TdxDocListLevel; static;
    procedure Write(AWriter: TBinaryWriter);
    function GetDisplayFormatString: string;
    function GetPlaceHolderIndices: TdxIntegerList;
    procedure SetDisplayFormatString(const ADisplayFormatString: string);
    function AddLevelNumber(const ADisplayFormatString: string; AIndex: Integer): Integer;
    function AddLevelNumberCore(const ADisplayFormatString: string; AIndex: Integer): Integer;

    property ListLevelProperties: TdxDocListLevelProperties read FListLevelProperties write FListLevelProperties;
    property CharacterUPX: TBytes read FCharacterUPX write FCharacterUPX;
    property ParagraphUPX: TBytes read FParagraphUPX write FParagraphUPX;
    property DisplayTextPlaceHolder: string read FDisplayTextPlaceHolder write FDisplayTextPlaceHolder;
  end;

  { TdxDocListData }

  TdxDocListData = class
  strict private
    FListFormatting: TdxDocListFormatting;
    FLevelsFormatting: TdxObjectList<TdxDocListLevel>;
  public
    constructor Create;
    destructor Destroy; override;
    class function FromStream(AReader: TBinaryReader): TdxDocListData; static;

    property ListFormatting: TdxDocListFormatting read FListFormatting write FListFormatting;
    property LevelsFormatting: TdxObjectList<TdxDocListLevel> read FLevelsFormatting;
  end;

  { TdxDocListFormatInformation }

  TdxDocListFormatInformation = class
  strict private
    FListData: TdxObjectList<TdxDocListData>;
  protected
    procedure Read(AReader: TBinaryReader; AOffset: Integer; ASize: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    class function FromStream(AReader: TBinaryReader; AOffset: Integer; ASize: Integer): TdxDocListFormatInformation; static;
    procedure Write(AWriter: TBinaryWriter);

    property ListData: TdxObjectList<TdxDocListData> read FListData;
  end;

implementation

uses
  Math, Contnrs, dxCharacters,
  dxRichEdit.Import.Rtf,
  dxStringHelper, dxEncoding;

{ TdxDocListFormatting }

constructor TdxDocListFormatting.Create;
begin
  SetLength(FLevelStyleIdentifiers, MaxLevelCount);
end;

class function TdxDocListFormatting.FromStream(AReader: TBinaryReader): TdxDocListFormatting;
begin
  Result := TdxDocListFormatting.Create;
  Result.Read(AReader);
end;

procedure TdxDocListFormatting.Read(AReader: TBinaryReader);
var
  ALevelIndex: Integer;
  AFlags: Byte;
begin
  Assert(AReader <> nil, 'reader');
  FListIdentifier := AReader.ReadInt32;
  FTemplateCode := AReader.ReadInt32;

  for ALevelIndex := 0 to MaxLevelCount - 1 do
    FLevelStyleIdentifiers[ALevelIndex] := AReader.ReadSmallInt;

  AFlags := AReader.ReadByte;
  FSimpleList := (AFlags and $01) <> 0;
  FAutonumbered := (AFlags and $04) <> 0;
  FHybrid := (AFlags and $10) <> 0;
  FHtmlCompatibilitiesFlags := AReader.ReadByte;
end;

procedure TdxDocListFormatting.Write(AWriter: TBinaryWriter);
var
  ALevelIndex: Integer;
  AFlags: Byte;
begin
  Assert(AWriter <> nil, 'writer');
  AWriter.Write(FListIdentifier);
  AWriter.Write(FTemplateCode);
  for ALevelIndex := 0 to MaxLevelCount - 1 do
    AWriter.Write(FLevelStyleIdentifiers[ALevelIndex]);

  if SimpleList then
    AFlags := $01
  else
    AFlags := $00;
  AWriter.Write(AFlags);
  AWriter.Write(FHtmlCompatibilitiesFlags);
end;

{ TdxDocListLevelProperties }

class function TdxDocListLevelProperties.FromStream(AReader: TBinaryReader): TdxDocListLevelProperties;
begin
  Result := TdxDocListLevelProperties.Create;
  Result.Read(AReader);
end;

procedure TdxDocListLevelProperties.Read(AReader: TBinaryReader);
var
  ANumberFormatCode, AFlags: Byte;
begin
  Assert(AReader <> nil, 'reader');
  FStart := Max(0, AReader.ReadInt32);
  ANumberFormatCode := AReader.ReadByte;
  if ANumberFormatCode = BulletFormatCode then
    FBulletedList := True;
  FNumberingFormat := TdxNumberingFormatCalculator.CalcNumberingFormat(ANumberFormatCode);
  AFlags := AReader.ReadByte;
  FAlignment := TdxAlignmentCalculator.CalcListNumberAlignment(Byte((AFlags and $03)));
  FLevelFormatFlags := AFlags and $fc;
  FConvertPreviousLevelNumberingToDecimal := (FLevelFormatFlags and TdxDocListLevelFormatFlags.ConvertPreviousLevelNumberingToDecimal) <> 0;
  FRestartAfterLevelLimit := (FLevelFormatFlags and TdxDocListLevelFormatFlags.NoRestart) <> 0;
  FLegacy := (FLevelFormatFlags and TdxDocListLevelFormatFlags.Legacy) <> 0;
  FPlaceholderIndices := AReader.ReadBytes(TdxDocListFormatting.MaxLevelCount);
  FSeparator := CalcSeparator(AReader.ReadByte);
  FLegacySpace := AReader.ReadInt32;
  FLegacyIndent := AReader.ReadInt32;
  FCharacterUPXsize := AReader.ReadByte;
  FParagraphUPXsize := AReader.ReadByte;
  FRelativeRestartLevel := AReader.ReadByte;
  FHtmlCompatibilitiesFlags := AReader.ReadByte;
end;

procedure TdxDocListLevelProperties.Write(AWriter: TBinaryWriter);
var
  ANumberFormatCode, AFlags: Byte;
begin
  Assert(AWriter <> nil, 'writer');
  FStart := Math.Max(0, FStart);
  if FStart = $ffff then
    FStart := 0;
  AWriter.Write(FStart);
  ANumberFormatCode := TdxNumberingFormatCalculator.CalcNumberingFormatCode(FNumberingFormat);
  AWriter.Write(ANumberFormatCode);
  AFlags := TdxAlignmentCalculator.CalcListNumberAlignmentCode(FAlignment);
  if FConvertPreviousLevelNumberingToDecimal then
    FLevelFormatFlags := FLevelFormatFlags or TdxDocListLevelFormatFlags.ConvertPreviousLevelNumberingToDecimal;
  if FLegacy then
    FLevelFormatFlags := FLevelFormatFlags or TdxDocListLevelFormatFlags.Legacy;
  if FRestartAfterLevelLimit then
    FLevelFormatFlags := FLevelFormatFlags or TdxDocListLevelFormatFlags.NoRestart;
  AFlags := AFlags or Byte(FLevelFormatFlags);
  AWriter.Write(AFlags);
  AWriter.Write(FPlaceholderIndices);
  AWriter.Write(CalcSeparatorCode);
  AWriter.Write(FLegacySpace);
  AWriter.Write(FLegacyIndent);
  AWriter.Write(FCharacterUPXsize);
  AWriter.Write(FParagraphUPXsize);
  AWriter.Write(FRelativeRestartLevel);
  AWriter.Write(FHtmlCompatibilitiesFlags);
end;

function TdxDocListLevelProperties.CalcSeparator(ASeparatorCode: Byte): Char;
begin
  case ASeparatorCode of
    0: Result := TdxCharacters.TabMark;
    1: Result := TdxCharacters.Space;
    else
      Result := TdxCharacters.Nothing;
  end;
end;

function TdxDocListLevelProperties.CalcSeparatorCode: Byte;
begin
  case FSeparator of
    TdxCharacters.TabMark:
      Result := $0;
    TdxCharacters.Space:
      Result := $1;
    else
      Result := $2;
  end;
end;

{ TdxDocListLevel }

destructor TdxDocListLevel.Destroy;
begin
  FListLevelProperties.Free;
  inherited Destroy;
end;

class function TdxDocListLevel.IsPercent(ACh: Char): Boolean;
begin
  Result := ACh = '%';
end;

class function TdxDocListLevel.IsDoublePercent(const ADisplayFormatString: string; I: Integer): Boolean;
begin
  if I > Length(ADisplayFormatString) then
    Exit(False);
  Result := (ADisplayFormatString[I] = '%') and (ADisplayFormatString[I + 1] = '%');
end;

class function TdxDocListLevel.FromStream(AReader: TBinaryReader): TdxDocListLevel;
begin
  Result := TdxDocListLevel.Create;
  Result.Read(AReader);
end;

procedure TdxDocListLevel.Read(AReader: TBinaryReader);
begin
  Assert(AReader <> nil, 'result');
  ListLevelProperties := TdxDocListLevelProperties.FromStream(AReader);
  FParagraphUPX := AReader.ReadBytes(ListLevelProperties.ParagraphUPXSize);
  FCharacterUPX := AReader.ReadBytes(ListLevelProperties.CharacterUPXSize);
  DisplayTextPlaceHolder := GetDisplayTextPlaceHolder(AReader);
end;

function TdxDocListLevel.GetDisplayTextPlaceHolder(AReader: TBinaryReader): string;
var
  ATextPlaceholderSize: Integer;
  ABuffer: TBytes;
begin
  ATextPlaceholderSize := AReader.ReadUInt16 * 2;
  if ATextPlaceholderSize >= AReader.BaseStream.Size - AReader.BaseStream.Position then
    Exit('');
  ABuffer := AReader.ReadBytes(ATextPlaceholderSize);
  Result := ' ' + TdxEncoding.Unicode.GetString(ABuffer, 0, Length(ABuffer));
end;

procedure TdxDocListLevel.Write(AWriter: TBinaryWriter);
begin
  Assert(AWriter <> nil, 'writer');
  FListLevelProperties.Write(AWriter);
  AWriter.Write(FParagraphUPX);
  AWriter.Write(FCharacterUPX);
  AWriter.Write(SmallInt(Length(DisplayTextPlaceHolder)));
  AWriter.Write(TdxEncoding.Unicode.GetBytes(DisplayTextPlaceHolder));
end;

function TdxDocListLevel.GetDisplayFormatString: string;
var
  APlaceHolderIndices: TdxIntegerList;
  ADisplayString: string;
begin
  APlaceHolderIndices := GetPlaceHolderIndices;
  try
    ADisplayString := TdxListLevelDisplayTextHelper.CreateDisplayFormatStringCore(APlaceHolderIndices, DisplayTextPlaceHolder);
    Result := TdxStringHelper.RemoveSpecialSymbols(ADisplayString);
  finally
    APlaceHolderIndices.Free;
  end;
end;

function TdxDocListLevel.GetPlaceHolderIndices: TdxIntegerList;
var
  APlaceHolderIndex: Integer;
begin
  Result := TdxIntegerList.Create;
  Result.Add(0);
  for APlaceHolderIndex := 0 to TdxDocListFormatting.MaxLevelCount - 1 do
  begin
    if FListLevelProperties.PlaceholderIndices[APlaceHolderIndex] = 0 then
      Break;
    Result.Add(FListLevelProperties.PlaceholderIndices[APlaceHolderIndex]);
  end;
  Result.Add(Length(DisplayTextPlaceHolder));
end;

procedure TdxDocListLevel.SetDisplayFormatString(const ADisplayFormatString: string);
var
  ACount, ACurrentIndex: Integer;
  ACh: Char;
begin
  DisplayTextPlaceHolder := '';
  SetLength(FListLevelProperties.FPlaceholderIndices, TdxDocListFormatting.MaxLevelCount);
  FCurrentLevel := 0;
  ACount := Length(ADisplayFormatString);
  ACurrentIndex := 1;
  while ACurrentIndex <= ACount do
  begin
    ACh := ADisplayFormatString[ACurrentIndex];
    if not IsPercent(ACh) then
    begin
      DisplayTextPlaceHolder := DisplayTextPlaceHolder + ACh;
      Inc(ACurrentIndex);
    end
    else
      Inc(ACurrentIndex, AddLevelNumber(ADisplayFormatString, ACurrentIndex));
  end;
end;

function TdxDocListLevel.AddLevelNumber(const ADisplayFormatString: string; AIndex: Integer): Integer;
begin
  if IsDoublePercent(ADisplayFormatString, AIndex) then
  begin
    DisplayTextPlaceHolder := DisplayTextPlaceHolder + ADisplayFormatString[AIndex];
    Exit(AIndex + 2);
  end;
  Result := AddLevelNumberCore(ADisplayFormatString, AIndex);
end;

function TdxDocListLevel.AddLevelNumberCore(const ADisplayFormatString: string; AIndex: Integer): Integer;
var
  AValue: string;
  AStartIndex, ALength: Integer;
begin
  Inc(AIndex);
  AStartIndex := AIndex;
  ALength := 0;
  while AIndex <= Length(ADisplayFormatString) do
  begin
    if ADisplayFormatString[AIndex] = ':' then
    begin
      AValue := Copy(ADisplayFormatString, AStartIndex, ALength);
      Break;
    end;
    Inc(ALength);
    Inc(AIndex);
  end;
  Assert(AIndex <= Length(ADisplayFormatString));

  DisplayTextPlaceHolder := DisplayTextPlaceHolder + Chr(StrToIntDef(AValue, 1));
  FListLevelProperties.PlaceholderIndices[FCurrentLevel] := Byte(Length(DisplayTextPlaceHolder));
  Inc(FCurrentLevel);
  Result := Length(AValue) + 3;
end;

{ TdxDocListData }

constructor TdxDocListData.Create;
begin
  FLevelsFormatting := TdxObjectList<TdxDocListLevel>.Create;
end;

destructor TdxDocListData.Destroy;
begin
  FListFormatting.Free;
  FLevelsFormatting.Free;
  inherited Destroy;
end;

class function TdxDocListData.FromStream(AReader: TBinaryReader): TdxDocListData;
begin
  Result := TdxDocListData.Create;
  Result.FListFormatting := TdxDocListFormatting.FromStream(AReader);
end;

{ TdxDocListFormatInformation }

constructor TdxDocListFormatInformation.Create;
begin
  FListData := TdxObjectList<TdxDocListData>.Create;
end;

destructor TdxDocListFormatInformation.Destroy;
begin
  FListData.Free;
  inherited Destroy;
end;

class function TdxDocListFormatInformation.FromStream(AReader: TBinaryReader; AOffset: Integer; ASize: Integer): TdxDocListFormatInformation;
begin
  Result := TdxDocListFormatInformation.Create;
  Result.Read(AReader, AOffset, ASize);
end;

procedure TdxDocListFormatInformation.Read(AReader: TBinaryReader; AOffset: Integer; ASize: Integer);
var
  AListsCount: SmallInt;
  I, AListIndex, ALevelIndex: Integer;
  AItem: TdxDocListData;
begin
  Assert(AReader <> nil, 'reader');
  if ASize = 0 then
    Exit;
  AReader.BaseStream.Seek(AOffset, TSeekOrigin.soBeginning);
  AListsCount := AReader.ReadSmallInt;
  for I := 0 to AListsCount - 1 do
  begin
    AItem := TdxDocListData.FromStream(AReader);
    FListData.Add(AItem);
  end;
  for AListIndex := 0 to AListsCount - 1 do
  begin
    if FListData[AListIndex].ListFormatting.SimpleList then
      FListData[AListIndex].LevelsFormatting.Add(TdxDocListLevel.FromStream(AReader))
    else
      for ALevelIndex := 0 to TdxDocListFormatting.MaxLevelCount - 1 do
        FListData[AListIndex].LevelsFormatting.Add(TdxDocListLevel.FromStream(AReader));
  end;
end;

procedure TdxDocListFormatInformation.Write(AWriter: TBinaryWriter);
var
  AListCount: SmallInt;
  I, AListIndex, ALevelCount, ALevelIndex: Integer;
begin
  Assert(AWriter <> nil, 'writer');
  AListCount := SmallInt(FListData.Count);
  if AListCount = 0 then
    Exit;

  AWriter.Write(AListCount);
  for I := 0 to AListCount - 1 do
    FListData[I].ListFormatting.Write(AWriter);

  for AListIndex := 0 to AListCount - 1 do
  begin
    ALevelCount := FListData[AListIndex].LevelsFormatting.Count;
    for ALevelIndex := 0 to ALevelCount - 1 do
      FListData[AListIndex].LevelsFormatting[ALevelIndex].Write(AWriter);
  end;
end;

end.
