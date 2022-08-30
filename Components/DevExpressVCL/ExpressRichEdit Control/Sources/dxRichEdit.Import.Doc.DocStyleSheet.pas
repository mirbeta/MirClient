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
unit dxRichEdit.Import.Doc.DocStyleSheet;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, Classes, Generics.Defaults, Generics.Collections, dxCore, dxCoreGraphics,
  dxGenerics,
  dxRichEdit.DocumentModel.Styles.Core;

type
  { TdxStyleSheetInformation }

  TdxStyleSheetInformation = class
  public const
    DefaultFixedIndexStylesCount             = Integer($0f);
    FtcBi                                    = SmallInt($0000);
    LatentStyleDescriptionSize               = SmallInt($0004);
    StiNormalLatentStyleInformation          = Cardinal($00000008);
    StiHeading1LatentStyleInformation        = Cardinal($00000098);
    StiHeadingsDefaultLatentStyleInformation = Cardinal($00000636);
    StiTocLatenStyleInformation              = Cardinal($00000276);
    StiListBulletLatentStyleInformation      = Cardinal($00000008);
    StiListBullet2LatentStyleInformation     = Cardinal($00000016);
    StiSubtitlelLatentStyleInformation       = Cardinal($000000b8);
    StiStrongLatentStyleInformation          = Cardinal($00000168);
    StiEmphasisLatentLatentStyleInformation  = Cardinal($00000148);
    DefaultFlags                             = Integer($0001);
    MaxStyleIdentifierWhenSaved              = Integer($005b);
    BuiltInStyleNamesVersionWhenSaved        = Integer($0000);
    WithoutStdfPost2000                      = Integer($000a);
    WithStdfPost2000                         = Integer($0012);
  strict private
    FStylesCount: SmallInt;
    FBaseStyleDescriptionSize: SmallInt;
    FFlags: SmallInt;
    FMaxStyleIdentifier: SmallInt;
    FFixedIndexStylesCount: SmallInt;
    FBuiltInStyleNamesVersion: SmallInt;
    FDefaultASCIIFont: SmallInt;
    FDefaultEastAsianFont: SmallInt;
    FDefaultOthersFont: SmallInt;
    function GetIsBuiltInStyleNamesStored: Boolean;
    function GetContainsStdfPost2000: Boolean;
  protected
    procedure Read(AReader: TBinaryReader; AOffset: Integer);
  public
    constructor Create;
    class function FromStream(AReader: TBinaryReader; AOffset: Integer): TdxStyleSheetInformation; static;
    class function CreateDefault: TdxStyleSheetInformation; static;
    procedure Write(AWriter: TBinaryWriter);

    property StylesCount: SmallInt read FStylesCount write FStylesCount;
    property BaseStyleDescriptionSize: SmallInt read FBaseStyleDescriptionSize;
    property IsBuiltInStyleNamesStored: Boolean read GetIsBuiltInStyleNamesStored;
    property MaxStyleIdentifier: SmallInt read FMaxStyleIdentifier;
    property FixedIndexStylesCount: SmallInt read FFixedIndexStylesCount write FFixedIndexStylesCount;
    property BuiltInStyleNameVersion: SmallInt read FBuiltInStyleNamesVersion;
    property DefaultASCIIFont: SmallInt read FDefaultASCIIFont write FDefaultASCIIFont;
    property DefaultEastAsianFont: SmallInt read FDefaultEastAsianFont;
    property DefaultOthersFont: SmallInt read FDefaultOthersFont;
    property ContainsStdfPost2000: Boolean read GetContainsStdfPost2000;
  end;

  { TdxStyleDescriptionBase }

  TdxStyleDescriptionBase = class abstract
  public const
    EmptyStyleIdentifier       = SmallInt($0fff);
    UserDefinedStyleIdentifier = SmallInt($0ffe);
    DefaultFlags               = SmallInt($2000);
    SizeOfBytesCount           = Integer(2);
    UnusedFlagsSize            = Integer(2);
    RsidSize                   = Integer(4);
    StyleSizeOffset            = Integer(6);
  strict private
    FStart: Int64;
    FEnd: Int64;
    FBaseSize: SmallInt;
    FStyleType: TdxStyleType;
    FUpxCount: SmallInt;
    FStyleIdentifier: SmallInt;
    FHidden: Boolean;
    FQFormat: Boolean;
    FStyleIndex: Integer;
    FPriority: SmallInt;
    FBaseStyleIndex: SmallInt;
    FNextStyleIndex: SmallInt;
    FLinkedStyleIndex: SmallInt;
    FStyleName: string;
  protected
    procedure Read(AReader: TBinaryReader; ABaseSize: SmallInt; AIsExtended: Boolean);
    procedure ReadUPX(AReader: TBinaryReader); virtual; abstract;
    procedure WriteUPX(AWriter: TBinaryWriter); virtual; abstract;
    function ReadParagraphUPX(AReader: TBinaryReader; AParagraphUPXLength: Integer): TArray<Byte>; virtual;
    procedure AlignOffset(AReader: TBinaryReader); overload;
    procedure AlignOffset(AWriter: TBinaryWriter); overload;

    property Start: Int64 read FStart;
    property BaseSize: SmallInt read FBaseSize;
  public
    constructor Create(AStyleType: TdxStyleType; AUpxCount: SmallInt);
    procedure Write(AWriter: TBinaryWriter; AIsExtended: Boolean);

    property StyleIdentifier: SmallInt read FStyleIdentifier write FStyleIdentifier;
    property StyleType: TdxStyleType read FStyleType;
    property Hidden: Boolean read FHidden write FHidden;
    property QFormat: Boolean read FQFormat write FQFormat;
    property StyleIndex: Integer read FStyleIndex write FStyleIndex;
    property Priority: SmallInt read FPriority write FPriority;
    property BaseStyleIndex: SmallInt read FBaseStyleIndex write FBaseStyleIndex;
    property NextStyleIndex: SmallInt read FNextStyleIndex write FNextStyleIndex;
    property LinkedStyleIndex: SmallInt read FLinkedStyleIndex write FLinkedStyleIndex;
    property StyleName: string read FStyleName write FStyleName;
  end;

  TdxStyleDescriptionCollection = TdxObjectList<TdxStyleDescriptionBase>;

  { TdxDocStyleSheet }

  TdxDocStyleSheet = class
  public const
    ExtendedStyleDescriptionBaseInFile = Integer($0012);
  strict private
    FStyleSheetInformation: TdxStyleSheetInformation;
    FStyles: TdxStyleDescriptionCollection;
  protected
    procedure Read(AReader: TBinaryReader; AOffset: Integer; ASize: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    class function FromStream(AReader: TBinaryReader; AOffset: Integer; ASize: Integer): TdxDocStyleSheet; static;
    class function CreateDefault: TdxDocStyleSheet; static;
    procedure Write(AWriter: TBinaryWriter);
    function GetStyles(AReader: TBinaryReader; AOffset: Integer; ASize: Integer): TdxList<TdxStyleDescriptionBase>;
    function GetStyleType(AReader: TBinaryReader): TdxStyleType;

    property StylesInformation: TdxStyleSheetInformation read FStyleSheetInformation write FStyleSheetInformation;
    property Styles: TdxStyleDescriptionCollection read FStyles;
  end;

  { TdxStyleDescriptionFactory }

  TdxStyleDescriptionFactory = class
  public
    class function CreateStyleDescription(AReader: TBinaryReader; AType: TdxStyleType; AInfo: TdxStyleSheetInformation): TdxStyleDescriptionBase; static;
  end;

implementation

uses
  Math, Contnrs,
  dxEncoding,
  dxStringHelper,
  dxRichEdit.Import.Doc.DocStyles,
  dxRichEdit.Import.Doc.DocImporter;

{ TdxStyleSheetInformation }

constructor TdxStyleSheetInformation.Create;
begin
  FBaseStyleDescriptionSize := TdxDocStyleSheet.ExtendedStyleDescriptionBaseInFile;
  FFlags := DefaultFlags;
  FMaxStyleIdentifier := MaxStyleIdentifierWhenSaved;
  FBuiltInStyleNamesVersion := BuiltInStyleNamesVersionWhenSaved;
  FDefaultASCIIFont := $0000;
  FDefaultEastAsianFont := $0000;
  FDefaultOthersFont := $0000;
end;

class function TdxStyleSheetInformation.FromStream(AReader: TBinaryReader; AOffset: Integer): TdxStyleSheetInformation;
begin
  Result := TdxStyleSheetInformation.Create;
  Result.Read(AReader, AOffset);
end;

class function TdxStyleSheetInformation.CreateDefault: TdxStyleSheetInformation;
begin
  Result := TdxStyleSheetInformation.Create;
  Result.FixedIndexStylesCount := DefaultFixedIndexStylesCount;
end;

function TdxStyleSheetInformation.GetIsBuiltInStyleNamesStored: Boolean;
begin
  Result := (FFlags and $01) = 1;
end;

function TdxStyleSheetInformation.GetContainsStdfPost2000: Boolean;
begin
  Result := FBaseStyleDescriptionSize = WithStdfPost2000;
end;

procedure TdxStyleSheetInformation.Read(AReader: TBinaryReader; AOffset: Integer);
begin
  AReader.BaseStream.Seek(AOffset, TSeekOrigin.soBeginning);
  FStylesCount := SmallInt(AReader.ReadUInt16);
  FBaseStyleDescriptionSize := SmallInt(AReader.ReadUInt16);

  FFlags := AReader.ReadSmallInt;
  FMaxStyleIdentifier := SmallInt(AReader.ReadUInt16);
  FFixedIndexStylesCount := SmallInt(AReader.ReadUInt16);
  FBuiltInStyleNamesVersion := SmallInt(AReader.ReadUInt16);
  FDefaultASCIIFont := SmallInt(AReader.ReadUInt16);
  FDefaultEastAsianFont := SmallInt(AReader.ReadUInt16);
  FDefaultOthersFont := SmallInt(AReader.ReadUInt16);
end;

procedure TdxStyleSheetInformation.Write(AWriter: TBinaryWriter);
var
  I: Integer;
begin
  AWriter.Write(FStylesCount);
  AWriter.Write(FBaseStyleDescriptionSize);
  AWriter.Write(FFlags);
  AWriter.Write(FMaxStyleIdentifier);
  AWriter.Write(FFixedIndexStylesCount);
  AWriter.Write(FBuiltInStyleNamesVersion);
  AWriter.Write(FDefaultASCIIFont);
  AWriter.Write(FDefaultEastAsianFont);
  AWriter.Write(FDefaultOthersFont);
  AWriter.Write(Word(FtcBi));
  AWriter.Write(Word(LatentStyleDescriptionSize));
  AWriter.Write(Cardinal(StiNormalLatentStyleInformation));
  AWriter.Write(Cardinal(StiHeading1LatentStyleInformation));
  for I := 0 to 8 - 1 do
    AWriter.Write(Cardinal($0000009e));
  for I := 0 to 9 - 1 do
    AWriter.Write(Cardinal(StiHeadingsDefaultLatentStyleInformation));
  for I := 0 to 9 - 1 do
    AWriter.Write(Cardinal(StiTocLatenStyleInformation));
  for I := 0 to 34 - 1 do
    AWriter.Write(Cardinal(StiHeading1LatentStyleInformation));
  AWriter.Write(Cardinal(StiListBulletLatentStyleInformation));
  AWriter.Write(Cardinal(StiHeadingsDefaultLatentStyleInformation));
  AWriter.Write(Cardinal(StiHeadingsDefaultLatentStyleInformation));
  AWriter.Write(Cardinal(StiListBullet2LatentStyleInformation));
  for I := 0 to 8 - 1 do
    AWriter.Write(Cardinal(StiHeadingsDefaultLatentStyleInformation));
  AWriter.Write(Cardinal(StiSubtitlelLatentStyleInformation));
  for I := 0 to 12 - 1 do
    AWriter.Write(Cardinal(StiHeadingsDefaultLatentStyleInformation));
  AWriter.Write(Cardinal(StiStrongLatentStyleInformation));
  AWriter.Write(Cardinal(StiEmphasisLatentLatentStyleInformation));
  AWriter.Write(Cardinal(StiHeadingsDefaultLatentStyleInformation));
  AWriter.Write(Cardinal(StiHeadingsDefaultLatentStyleInformation));
end;

{ TdxStyleDescriptionBase }

constructor TdxStyleDescriptionBase.Create(AStyleType: TdxStyleType; AUpxCount: SmallInt);
begin
  FStyleType := AStyleType;
  FUpxCount := AUpxCount;
  StyleIdentifier := UserDefinedStyleIdentifier;
  FBaseSize := TdxDocStyleSheet.ExtendedStyleDescriptionBaseInFile;
  BaseStyleIndex := EmptyStyleIdentifier;
end;

procedure TdxStyleDescriptionBase.Read(AReader: TBinaryReader; ABaseSize: SmallInt; AIsExtended: Boolean);
var
  AStyleKindAndBaseStyleIdentifier: Word;
  AGeneralStyleProperties: Integer;
  AStyleNameLength: Byte;
  ABuffer: TBytes;
  AStyleName: string;
begin
  FStart := AReader.BaseStream.Position;
  FBaseSize := ABaseSize;
  StyleIdentifier := AReader.ReadUInt16 and $0fff;
  AStyleKindAndBaseStyleIdentifier := AReader.ReadUInt16;
  FStyleType := TdxStyleType((AStyleKindAndBaseStyleIdentifier and $000f) - 1);

  BaseStyleIndex := AStyleKindAndBaseStyleIdentifier shr 4;
  NextStyleIndex := AReader.ReadUInt16 shr 4;
  AReader.BaseStream.Seek(UnusedFlagsSize, TSeekOrigin.soCurrent);
  AGeneralStyleProperties := AReader.ReadSmallInt;
  Hidden := (AGeneralStyleProperties and $0002) <> 0;
  QFormat := (AGeneralStyleProperties and $1000) <> 0;

  if AIsExtended then
  begin
    LinkedStyleIndex := AReader.ReadUInt16 and $0fff;
    AReader.BaseStream.Seek(RsidSize, TSeekOrigin.soCurrent);
    Priority := AReader.ReadUInt16 shr 4;
  end;
  AReader.BaseStream.Seek(Start + BaseSize, TSeekOrigin.soBeginning);

  AStyleNameLength := AReader.ReadByte;
  if AReader.ReadByte = $00 then
  begin
    ABuffer := AReader.ReadBytes(AStyleNameLength * 2);
    AStyleName := TdxEncoding.Unicode.GetString(ABuffer, 0, Length(ABuffer));
    StyleName := TdxStringHelper.RemoveSpecialSymbols(AStyleName);
    AReader.ReadSmallInt;
  end
  else
  begin
    AReader.BaseStream.Seek(-1, TSeekOrigin.soCurrent);
    ABuffer := AReader.ReadBytes(AStyleNameLength);
    AStyleName := TdxEncoding.ASCII.GetString(ABuffer, 0, Length(ABuffer));
    StyleName := TdxStringHelper.RemoveSpecialSymbols(AStyleName);
    AReader.ReadByte;
  end;

  AlignOffset(AReader);
  ReadUPX(AReader);
end;

procedure TdxStyleDescriptionBase.Write(AWriter: TBinaryWriter; AIsExtended: Boolean);
var
  AStyleDescriptionSize: SmallInt;
begin
  FStart := AWriter.BaseStream.Position;
  AWriter.BaseStream.Seek(SizeOfBytesCount, TSeekOrigin.soCurrent);
  AWriter.Write(Word(DefaultFlags or StyleIdentifier));
  AWriter.Write(Word((BaseStyleIndex shl 4) or (Ord(FStyleType) + 1)));
  AWriter.Write(Word((NextStyleIndex shl 4) or (Word(FUpxCount))));
  AWriter.BaseStream.Seek(UnusedFlagsSize, TSeekOrigin.soCurrent);
  AWriter.Write(Word((Byte(Hidden) shl 1) + (Byte(QFormat) shl 12)));
  if AIsExtended then
  begin
    LinkedStyleIndex := Max(0, LinkedStyleIndex);
    AWriter.Write(Word(LinkedStyleIndex));
    AWriter.Write(Integer(0));
    AWriter.Write(Word(Priority shl 4));
  end;
  AWriter.BaseStream.Seek(Start + BaseSize + SizeOfBytesCount, TSeekOrigin.soBeginning);
  AWriter.Write(Word(Length(StyleName)));
  AWriter.Write(TdxEncoding.Unicode.GetBytes(StyleName));
  AWriter.BaseStream.Seek(2, TSeekOrigin.soCurrent);
  AlignOffset(AWriter);
  WriteUPX(AWriter);
  FEnd := AWriter.BaseStream.Position;
  AWriter.BaseStream.Seek(FStart, TSeekOrigin.soBeginning);
  AStyleDescriptionSize := FEnd - FStart - SizeOfBytesCount;
  AWriter.Write(AStyleDescriptionSize);
  AWriter.Seek(StyleSizeOffset, TSeekOrigin.soCurrent);
  AWriter.Write(AStyleDescriptionSize);

  AWriter.BaseStream.Seek(FEnd, TSeekOrigin.soBeginning);
end;

function TdxStyleDescriptionBase.ReadParagraphUPX(AReader: TBinaryReader; AParagraphUPXLength: Integer): TBytes;
begin
  if AParagraphUPXLength = 0 then
    Result := TBytes.Create()
  else
    Result := AReader.ReadBytes(AParagraphUPXLength - 2);
end;

procedure TdxStyleDescriptionBase.AlignOffset(AReader: TBinaryReader);
begin
  if (AReader.BaseStream.Position - FStart) mod 2 <> 0 then
    AReader.BaseStream.Seek(1, TSeekOrigin.soCurrent);
end;

procedure TdxStyleDescriptionBase.AlignOffset(AWriter: TBinaryWriter);
begin
  if (AWriter.BaseStream.Position - FStart) mod 2 <> 0 then
    AWriter.BaseStream.Seek(1, TSeekOrigin.soCurrent);
end;

{ TdxDocStyleSheet }

constructor TdxDocStyleSheet.Create;
begin
  FStyles := TdxStyleDescriptionCollection.Create;
end;

destructor TdxDocStyleSheet.Destroy;
begin
  FStyleSheetInformation.Free;
  FStyles.Free;
  inherited Destroy;
end;

class function TdxDocStyleSheet.FromStream(AReader: TBinaryReader; AOffset: Integer; ASize: Integer): TdxDocStyleSheet;
begin
  Result := TdxDocStyleSheet.Create;
  Result.Read(AReader, AOffset, ASize);
end;

class function TdxDocStyleSheet.CreateDefault: TdxDocStyleSheet;
begin
  Result := TdxDocStyleSheet.Create;
  Result.FStyleSheetInformation := TdxStyleSheetInformation.CreateDefault;
end;

procedure TdxDocStyleSheet.Read(AReader: TBinaryReader; AOffset: Integer; ASize: Integer);
var
  AStyleSheetInformationOffset: Integer;
  AStyleSheetInformationSize: SmallInt;
  AStyles: TdxList<TdxStyleDescriptionBase>;
begin
  AStyleSheetInformationOffset := AOffset + 2;
  AReader.BaseStream.Seek(AOffset, TSeekOrigin.soBeginning);
  AStyleSheetInformationSize := AReader.ReadSmallInt;
  FStyleSheetInformation.Free;
  FStyleSheetInformation := TdxStyleSheetInformation.FromStream(AReader, AStyleSheetInformationOffset);
  AReader.BaseStream.Seek(AStyleSheetInformationOffset + AStyleSheetInformationSize, TSeekOrigin.soBeginning);
  AStyles := GetStyles(AReader, AOffset, ASize);
  try
    FStyles.AddRange(AStyles);
  finally
    AStyles.Free;
  end;
end;

procedure TdxDocStyleSheet.Write(AWriter: TBinaryWriter);
var
  ACount, I: Integer;
begin
  AWriter.Write(Word($0182));
  StylesInformation.Write(AWriter);
  ACount := Styles.Count;
  for I := 0 to ACount - 1 do
  begin
    if Styles[I] <> nil then
      Styles[I].Write(AWriter, StylesInformation.ContainsStdfPost2000)
    else
      AWriter.Write(SmallInt(0));
  end;
end;

function TdxDocStyleSheet.GetStyles(AReader: TBinaryReader; AOffset: Integer; ASize: Integer): TdxList<TdxStyleDescriptionBase>;
var
  AStyleDescriptionLength: SmallInt;
  ACurrentPosition: Int64;
  AStyleDescription: TdxStyleDescriptionBase;
begin
  Result := TdxList<TdxStyleDescriptionBase>.Create;
  while AReader.BaseStream.Position - AOffset < ASize do
  begin
    AStyleDescriptionLength := AReader.ReadSmallInt;
    if AStyleDescriptionLength > 0 then
    begin
      ACurrentPosition := AReader.BaseStream.Position;
      AStyleDescription := TdxStyleDescriptionFactory.CreateStyleDescription(AReader, GetStyleType(AReader), StylesInformation);
      AStyleDescription.StyleIndex := Result.Count;
      Result.Add(AStyleDescription);
      AReader.BaseStream.Seek(ACurrentPosition + AStyleDescriptionLength, TSeekOrigin.soBeginning);
    end
    else
      Result.Add(nil);
  end;
end;

function TdxDocStyleSheet.GetStyleType(AReader: TBinaryReader): TdxStyleType;
begin
  AReader.BaseStream.Seek(2, TSeekOrigin.soCurrent);
  Result := TdxStyleType((AReader.ReadSmallInt and $000f) - 1);
  AReader.BaseStream.Seek(-4, TSeekOrigin.soCurrent);
end;

{ TdxStyleDescriptionFactory }

class function TdxStyleDescriptionFactory.CreateStyleDescription(AReader: TBinaryReader; AType: TdxStyleType; AInfo: TdxStyleSheetInformation): TdxStyleDescriptionBase;
var
  ASize: SmallInt;
  AIsExtended: Boolean;
begin
  ASize := AInfo.BaseStyleDescriptionSize;
  AIsExtended := AInfo.ContainsStdfPost2000;
  case AType of
    TdxStyleType.ParagraphStyle:
      Result := TdxParagraphStyleDescription.FromStream(AReader, ASize, AIsExtended);
    TdxStyleType.CharacterStyle:
      Result := TdxCharacterStyleDescription.FromStream(AReader, ASize, AIsExtended);
    TdxStyleType.TableStyle:
      Result := TdxTableStyleDescription.FromStream(AReader, ASize, AIsExtended);
    TdxStyleType.NumberingListStyle:
      Result := TdxListStyleDescription.FromStream(AReader, ASize, AIsExtended);
    else
      TdxDocImporter.ThrowInvalidDocFile;
      Result := nil;
  end;
end;

end.
