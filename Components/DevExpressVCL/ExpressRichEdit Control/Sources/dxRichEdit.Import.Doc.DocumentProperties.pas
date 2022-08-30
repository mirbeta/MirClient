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
unit dxRichEdit.Import.Doc.DocumentProperties;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, SysUtils, Classes, Generics.Defaults, Generics.Collections, dxCore, dxCoreGraphics,
  dxRichEdit.Utils.Types,
  dxRichEdit.DocumentModel.UnitConverter,
  dxRichEdit.Platform.Font,
  dxRichEdit.DocumentModel.CharacterFormatting,
  dxRichEdit.DocumentModel.ParagraphFormatting,
  dxRichEdit.DocumentModel.TabFormatting,
  dxRichEdit.DocumentModel.TableFormatting,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.Section,
  dxRichEdit.DocumentModel.SectionFormatting,
  dxRichEdit.DocumentModel.ProtectionFormatting,
  dxRichEdit.Doc.Utils,
  dxRichEdit.Import.Doc.DocCharacterFormattingInfo,
  dxRichEdit.Import.Doc.DocPictureBulletInformation;

type

  { TdxDocDocumentProperties }

  TdxDocumentViewKind = (
    NormalView,
    OutlineView,
    PageView);

  TdxZoomType = (
    None = $0000,
    FullPage = $1000,
    PageWidth = $2000);

  TdxGutterPosition = (
    Side = $0000,
    Top = $8000);

  TdxAutoformatDocumentType = (
    Normal,
    Letter,
    Email);

  { TdxDocumentTypographyInfo }

  TdxDocumentTypographyInfo = class
  strict private
    FFlags: SmallInt;
    FFollowingPunctsLength: SmallInt;
    FLeadingPunctsLength: SmallInt;
    FFollowingPuncts: TArray<Char>;
    FLeadingPuncts: TArray<Char>;
  protected
    procedure Read(AReader: TBinaryReader);

    property FollowingPunctsLength: SmallInt read FFollowingPunctsLength;
    property LeadingPunctsLength: SmallInt read FLeadingPunctsLength;
  public
    constructor Create;
    class function FromStream(AReader: TBinaryReader): TdxDocumentTypographyInfo; static;
    procedure Write(AWriter: TBinaryWriter);
  end;

  { TdxDocDocumentProperties }

  TdxDocDocumentProperties = class
  public const
    DefaultCompatibilities60 = Word($f000);
    DefaultCompatibilities80 = Cardinal($0010f000);
    PasswordHashPosition = Integer(78);
    ProtectionPosition = Integer(598);
    BackgroundPosition = Integer(598);
  strict private
    FPasswordHash: TBytes;
    FProtectionType: TdxDocumentProtectionType;
    FDefaultTabWidth: SmallInt;
    FDifferentOddAndEvenPage: Byte;
    FDisplayBackgroundShape: Byte;
    FWidowControl: Byte;
    FFootNotePosition: TdxFootNotePosition;
    FFootNoteNumberingRestartType: TdxLineNumberingRestart;
    FFootnoteInitialNumber: Integer;
    FOutlineDirtySave: Word;
    FOnlyMacPics: Word;
    FOnlyWinPics: Word;
    FLabelDoc: Word;
    FHyphCapitals: Word;
    FBackup: Word;
    FExactWordsCount: Word;
    FDisplayHiddenContent: Word;
    FDisplayFieldResults: Word;
    FLockAnnotations: Word;
    FMirrorMargins: Word;
    FTrueTypeFontsByDefault: Word;
    FAutoHyphen: Word;
    FFormNoFields: Word;
    FLinkStyles: Word;
    FRevMarking: Word;
    FProtectedFromEditing: Word;
    FDisplayFormFieldsSelection: Word;
    FShowRevisionMarkings: Word;
    FPrintRevisionMarkings: Word;
    FLockRevisionMarkings: Word;
    FContainsEmbeddedFonts: Word;
    FEnforceProtection: Word;
    FHyphenationHotZone: SmallInt;
    FHyphenationConsecutiveLimit: SmallInt;
    FDateCreated: TDateTime;
    FDateRevised: TDateTime;
    FDateLastPrinted: TDateTime;
    FRevisionNumber: SmallInt;
    FTimeEdited: Integer;
    FCharactersCount: Integer;
    FWordsCount: Integer;
    FParagraphsCount: Integer;
    FPagesCount: SmallInt;
    FEndNoteNumberingRestartType: TdxLineNumberingRestart;
    FEndnoteInitialNumber: Integer;
    FEndNotePosition: TdxFootNotePosition;
    FShadeFormFields: Word;
    FIncludeFootnotesAndEndNotesInWordsCount: Word;
    FLinesCount: Integer;
    FWordsCountInNotes: Integer;
    FCharactersCountInNotes: Integer;
    FPagesCountInNotes: SmallInt;
    FParagraphsCountInNotes: Integer;
    FLinesCountInNotes: Integer;
    FDocumentProtectionPasswordKey: Integer;
    FViewKind: TdxDocumentViewKind;
    FZoomPercentage: SmallInt;
    FZoomType: TdxZoomType;
    FGutterPosition: TdxGutterPosition;
    FDocumentType: TdxAutoformatDocumentType;
    FDoptypography: TdxDocumentTypographyInfo;
    function GetDifferentOddAndEvenPages: Boolean;
    procedure SetDifferentOddAndEvenPages(const AValue: Boolean);
    function GetDisplayBackgroundShape: Boolean;
    procedure SetDisplayBackgroundShape(const AValue: Boolean);
    function GetOnlyMacPics: Boolean;
    function GetOnlyWinPics: Boolean;
    function GetLabelDoc: Boolean;
    function GetHyphCapitals: Boolean;
    function GetAutoHyphen: Boolean;
    function GetFormNoFields: Boolean;
    function GetLinkStyles: Boolean;
    function GetRevisionMarking: Boolean;
    function GetBackup: Boolean;
    function GetExactWordsCount: Boolean;
    function GetDisplayHiddenContent: Boolean;
    function GetDisplayFieldResults: Boolean;
    function GetLockAnnotations: Boolean;
    function GetMirrorMargins: Boolean;
    function GetTrueTypeFontsByDefault: Boolean;
    function GetProtectedFromEditing: Boolean;
    function GetDisplayFormFieldsSelection: Boolean;
    function GetShowRevisionMarkings: Boolean;
    function GetPrintRevisionMarkings: Boolean;
    function GetLockRevisionMarkings: Boolean;
    function GetContainsEmbeddedFonts: Boolean;
    function GetEnforceProtection: Boolean;
    procedure SetEnforceProtection(const AValue: Boolean);
    procedure SetEndnoteInitialNumber(const AValue: Integer);
    procedure SetEndNotePosition(const AValue: TdxFootNotePosition);
    function GetIncludeFootnotesAndEndnotesInWordCount: Boolean;
    procedure SetGutterPosition(const AValue: TdxGutterPosition);
    procedure SetDocumentType(const AValue: TdxAutoformatDocumentType);
    procedure SetTypography(const AValue: TdxDocumentTypographyInfo);
  protected
    procedure Read(AReader: TBinaryReader; AOffset: Integer);
    procedure ReadOddEvenPageSettingsAndFootNoteSettings(AReader: TBinaryReader);
    procedure ReadDocCreatedInfo(AReader: TBinaryReader);
    procedure ReadDisplaySettings(AReader: TBinaryReader);
    procedure ReadDefaultTabWidth(AReader: TBinaryReader);
    procedure ReadHyphenationSettings(AReader: TBinaryReader);
    procedure ReadDocStatistics(AReader: TBinaryReader);
    procedure ReadEndNoteSettings(AReader: TBinaryReader);
    procedure ReadDocStatistics2(AReader: TBinaryReader);
    procedure ReadDocZoomAndPositionInfo(AReader: TBinaryReader);
    procedure ReadDocumentProtection(AReader: TBinaryReader; AOffset: Integer);
    procedure ReadPageBackground(AReader: TBinaryReader; AOffset: Integer);

    property WidowControl: Byte read FWidowControl;
  public
    destructor Destroy; override;
    class function FromStream(AReader: TBinaryReader; AOffset: Integer): TdxDocDocumentProperties; static;
    class function CreateDefault: TdxDocDocumentProperties; static;
    procedure Write(AWriter: TBinaryWriter);
    procedure WriteDefaultFlags(AWriter: TBinaryWriter);
    procedure WriteDefaultTabWidth(AWriter: TBinaryWriter);
    procedure WriteHyphenationSettings(AWriter: TBinaryWriter);
    procedure WritePageBackgroundAndDocumentProtection(AWriter: TBinaryWriter; AOffset: Integer);
    procedure WriteDocStatistics(AWriter: TBinaryWriter);
    procedure WriteEndNoteSettings(AWriter: TBinaryWriter);
    procedure WriteDocStatistics2(AWriter: TBinaryWriter);
    procedure WriteDocZoomAndPosiitonInfo(AWriter: TBinaryWriter);
    function ReadDTTM(ADttm: Cardinal): TDateTime;
    procedure WriteDTTM(AWriter: TBinaryWriter; ADateTime: TDateTime);

    property DifferentOddAndEvenPages: Boolean read GetDifferentOddAndEvenPages write SetDifferentOddAndEvenPages;
    property DisplayBackgroundShape: Boolean read GetDisplayBackgroundShape write SetDisplayBackgroundShape;
    property FootNotePosition: TdxFootNotePosition read FFootNotePosition write FFootNotePosition;
    property FootNoteNumberingRestartType: TdxLineNumberingRestart read FFootNoteNumberingRestartType write FFootNoteNumberingRestartType;
    property FootNoteInitialNumber: Integer read FFootnoteInitialNumber write FFootnoteInitialNumber;
    property OnlyMacPics: Boolean read GetOnlyMacPics;
    property OnlyWinPics: Boolean read GetOnlyWinPics;
    property LabelDoc: Boolean read GetLabelDoc;
    property HyphCapitals: Boolean read GetHyphCapitals;
    property AutoHyphen: Boolean read GetAutoHyphen;
    property FormNoFields: Boolean read GetFormNoFields;
    property LinkStyles: Boolean read GetLinkStyles;
    property RevisionMarking: Boolean read GetRevisionMarking;
    property Backup: Boolean read GetBackup;
    property ExactWordsCount: Boolean read GetExactWordsCount;
    property DisplayHiddenContent: Boolean read GetDisplayHiddenContent;
    property DisplayFieldResults: Boolean read GetDisplayFieldResults;
    property LockAnnotations: Boolean read GetLockAnnotations;
    property MirrorMargins: Boolean read GetMirrorMargins;
    property TrueTypeFontsByDefault: Boolean read GetTrueTypeFontsByDefault;
    property ProtectedFromEditing: Boolean read GetProtectedFromEditing;
    property DisplayFormFieldsSelection: Boolean read GetDisplayFormFieldsSelection;
    property ShowRevisionMarkings: Boolean read GetShowRevisionMarkings;
    property PrintRevisionMarkings: Boolean read GetPrintRevisionMarkings;
    property LockRevisionMarkings: Boolean read GetLockRevisionMarkings;
    property ContainsEmbeddedFonts: Boolean read GetContainsEmbeddedFonts;
    property EnforceProtection: Boolean read GetEnforceProtection write SetEnforceProtection;
    property PasswordHash: TBytes read FPasswordHash write FPasswordHash;
    property ProtectionType: TdxDocumentProtectionType read FProtectionType write FProtectionType;
    property DefaultTabWidth: SmallInt read FDefaultTabWidth write FDefaultTabWidth;
    property HyphenationHotZone: SmallInt read FHyphenationHotZone write FHyphenationHotZone;
    property HyphenationConsecutiveLimit: SmallInt read FHyphenationConsecutiveLimit write FHyphenationConsecutiveLimit;
    property DateCreated: TDateTime read FDateCreated write FDateCreated;
    property DateRevised: TDateTime read FDateRevised write FDateRevised;
    property DateLastPrinter: TDateTime read FDateLastPrinted write FDateLastPrinted;
    property RevisionNumber: SmallInt read FRevisionNumber write FRevisionNumber;
    property CharactersCount: Integer read FCharactersCount write FCharactersCount;
    property WordsCount: Integer read FWordsCount write FWordsCount;
    property ParagraphsCount: Integer read FParagraphsCount write FParagraphsCount;
    property PagesCount: SmallInt read FPagesCount write FPagesCount;
    property EndNoteNumberingRestartType: TdxLineNumberingRestart read FEndNoteNumberingRestartType write FEndNoteNumberingRestartType;
    property EndnoteInitialNumber: Integer read FEndnoteInitialNumber write SetEndnoteInitialNumber;
    property EndNotePosition: TdxFootNotePosition read FEndNotePosition write SetEndNotePosition;
    property IncludeFootnotesAndEndnotesInWordCount: Boolean read GetIncludeFootnotesAndEndnotesInWordCount;
    property GutterPosition: TdxGutterPosition read FGutterPosition write SetGutterPosition;
    property DocumentType: TdxAutoformatDocumentType read FDocumentType write SetDocumentType;
    property Typography: TdxDocumentTypographyInfo read FDoptypography write SetTypography;
  end;

implementation

uses
  Math, Windows, dxEncoding, dxTypeHelpers, DateUtils;


{ TdxDocumentTypographyInfo }

constructor TdxDocumentTypographyInfo.Create;
begin
  SetLength(FFollowingPuncts, 101);
  SetLength(FLeadingPuncts, 51);
end;

class function TdxDocumentTypographyInfo.FromStream(AReader: TBinaryReader): TdxDocumentTypographyInfo;
begin
  Result := TdxDocumentTypographyInfo.Create;
  Result.Read(AReader);
end;

procedure TdxDocumentTypographyInfo.Read(AReader: TBinaryReader);
var
  ABuffer: TArray<Char>;
  I: Integer;
begin
  FFlags := AReader.ReadInt16;
  FFollowingPunctsLength := AReader.ReadInt16;
  FLeadingPunctsLength := AReader.ReadInt16;
  ABuffer := TdxEncoding.Unicode.GetChars(AReader.ReadBytes(202));
  for I := 0 to 101 - 1 do
    FFollowingPuncts[I] := ABuffer[I];

  ABuffer := TdxEncoding.Unicode.GetChars(AReader.ReadBytes(102));
  for I := 0 to 51 - 1 do
    FLeadingPuncts[I] := ABuffer[I];
end;

procedure TdxDocumentTypographyInfo.Write(AWriter: TBinaryWriter);
var
  I: Integer;
begin
  AWriter.Write(FFlags);
  AWriter.Write(FFollowingPunctsLength);
  AWriter.Write(FLeadingPunctsLength);
  for I := 0 to 101 - 1 do
    AWriter.Write(TdxEncoding.Unicode.GetBytes(TArray<Char>.Create(FFollowingPuncts[I])));

  for I := 0 to 51 - 1 do
    AWriter.Write(TdxEncoding.Unicode.GetBytes(TArray<Char>.Create(FFollowingPuncts[I])));
end;

{ TdxDocDocumentProperties }

destructor TdxDocDocumentProperties.Destroy;
begin
  FDoptypography.Free;
  inherited Destroy;
end;

class function TdxDocDocumentProperties.FromStream(AReader: TBinaryReader; AOffset: Integer): TdxDocDocumentProperties;
begin
  Result := TdxDocDocumentProperties.Create;
  Result.Read(AReader, AOffset);
end;

class function TdxDocDocumentProperties.CreateDefault: TdxDocDocumentProperties;
begin
  Result := TdxDocDocumentProperties.Create;
  Result.FWidowControl := $02;
  Result.FFootNotePosition := TdxFootNotePosition.BelowText;
  Result.FFootNoteNumberingRestartType := TdxLineNumberingRestart.Continuous;
  Result.FFootnoteInitialNumber := 1;
  Result.FOutlineDirtySave := $0001;
  Result.FHyphCapitals := $0800;
  Result.FDisplayFieldResults := $0008;
  Result.FTrueTypeFontsByDefault := $0080;
  Result.FDisplayFieldResults := $0008;
  Result.FTrueTypeFontsByDefault := $0080;
  Result.FShowRevisionMarkings := $0800;
  Result.FPrintRevisionMarkings := $1000;
  Result.FHyphenationHotZone := $0168;
  Result.FDateCreated := Now;
  Result.FDateRevised := Now;
  Result.FDateLastPrinted := EncodeDate(1601, 1, 1);
  Result.FPagesCount := 1;
  Result.FParagraphsCount := 1;
  Result.FEndNotePosition := TdxFootNotePosition.EndOfSection;
  Result.FShadeFormFields := $1000;
  Result.FIncludeFootnotesAndEndNotesInWordsCount := $8000;
  Result.FViewKind := TdxDocumentViewKind.OutlineView;
  Result.FZoomPercentage := 100;
  Result.FZoomType := TdxZoomType.None;
  Result.FGutterPosition := TdxGutterPosition.Side;
  Result.FDoptypography := TdxDocumentTypographyInfo.Create;
  Result.EnforceProtection := False;
  Result.ProtectionType := TdxDocumentProtectionType.ReadOnly;
  Result.FPasswordHash := TBytes.Create(0);
end;

function TdxDocDocumentProperties.GetDifferentOddAndEvenPages: Boolean;
begin
  Result := FDifferentOddAndEvenPage <> 0;
end;

procedure TdxDocDocumentProperties.SetDifferentOddAndEvenPages(const AValue: Boolean);
begin
  if AValue then
    FDifferentOddAndEvenPage := 1
  else
    FDifferentOddAndEvenPage := 0;
end;

function TdxDocDocumentProperties.GetDisplayBackgroundShape: Boolean;
begin
  Result := FDisplayBackgroundShape <> 0;
end;

procedure TdxDocDocumentProperties.SetDisplayBackgroundShape(const AValue: Boolean);
begin
  if AValue then
    FDisplayBackgroundShape := 1
  else
    FDisplayBackgroundShape := 0;
end;

function TdxDocDocumentProperties.GetOnlyMacPics: Boolean;
begin
  Result := FOnlyMacPics <> 0;
end;

function TdxDocDocumentProperties.GetOnlyWinPics: Boolean;
begin
  Result := FOnlyWinPics <> 0;
end;

function TdxDocDocumentProperties.GetLabelDoc: Boolean;
begin
  Result := FLabelDoc <> 0;
end;

function TdxDocDocumentProperties.GetHyphCapitals: Boolean;
begin
  Result := FHyphCapitals <> 0;
end;

function TdxDocDocumentProperties.GetAutoHyphen: Boolean;
begin
  Result := FAutoHyphen <> 0;
end;

function TdxDocDocumentProperties.GetFormNoFields: Boolean;
begin
  Result := FFormNoFields <> 0;
end;

function TdxDocDocumentProperties.GetLinkStyles: Boolean;
begin
  Result := FLinkStyles <> 0;
end;

function TdxDocDocumentProperties.GetRevisionMarking: Boolean;
begin
  Result := FRevMarking <> 0;
end;

function TdxDocDocumentProperties.GetBackup: Boolean;
begin
  Result := FBackup <> 0;
end;

function TdxDocDocumentProperties.GetExactWordsCount: Boolean;
begin
  Result := FExactWordsCount <> 0;
end;

function TdxDocDocumentProperties.GetDisplayHiddenContent: Boolean;
begin
  Result := FDisplayHiddenContent <> 0;
end;

function TdxDocDocumentProperties.GetDisplayFieldResults: Boolean;
begin
  Result := FDisplayFieldResults <> 0;
end;

function TdxDocDocumentProperties.GetLockAnnotations: Boolean;
begin
  Result := FLockAnnotations <> 0;
end;

function TdxDocDocumentProperties.GetMirrorMargins: Boolean;
begin
  Result := FMirrorMargins <> 0;
end;

function TdxDocDocumentProperties.GetTrueTypeFontsByDefault: Boolean;
begin
  Result := FTrueTypeFontsByDefault <> 0;
end;

function TdxDocDocumentProperties.GetProtectedFromEditing: Boolean;
begin
  Result := FProtectedFromEditing <> 0;
end;

function TdxDocDocumentProperties.GetDisplayFormFieldsSelection: Boolean;
begin
  Result := FDisplayFormFieldsSelection <> 0;
end;

function TdxDocDocumentProperties.GetShowRevisionMarkings: Boolean;
begin
  Result := FShowRevisionMarkings <> 0;
end;

function TdxDocDocumentProperties.GetPrintRevisionMarkings: Boolean;
begin
  Result := FPrintRevisionMarkings <> 0;
end;

function TdxDocDocumentProperties.GetLockRevisionMarkings: Boolean;
begin
  Result := FLockRevisionMarkings <> 0;
end;

function TdxDocDocumentProperties.GetContainsEmbeddedFonts: Boolean;
begin
  Result := FContainsEmbeddedFonts <> 0;
end;

function TdxDocDocumentProperties.GetEnforceProtection: Boolean;
begin
  Result := FEnforceProtection <> 0;
end;

procedure TdxDocDocumentProperties.SetEnforceProtection(const AValue: Boolean);
begin
  if AValue then
    FEnforceProtection := 1
  else
    FEnforceProtection := 0;
end;

procedure TdxDocDocumentProperties.SetEndnoteInitialNumber(const AValue: Integer);
begin
  FEndnoteInitialNumber := AValue;
end;

procedure TdxDocDocumentProperties.SetEndNotePosition(const AValue: TdxFootNotePosition);
begin
  FEndNotePosition := AValue;
end;

function TdxDocDocumentProperties.GetIncludeFootnotesAndEndnotesInWordCount: Boolean;
begin
  Result := FIncludeFootnotesAndEndNotesInWordsCount <> 0;
end;

procedure TdxDocDocumentProperties.SetGutterPosition(const AValue: TdxGutterPosition);
begin
  FGutterPosition := AValue;
end;

procedure TdxDocDocumentProperties.SetDocumentType(const AValue: TdxAutoformatDocumentType);
begin
  FDocumentType := AValue;
end;

procedure TdxDocDocumentProperties.SetTypography(const AValue: TdxDocumentTypographyInfo);
begin
  FDoptypography.Free;
  FDoptypography := AValue;
end;

procedure TdxDocDocumentProperties.Read(AReader: TBinaryReader; AOffset: Integer);
begin
  AReader.BaseStream.Seek(AOffset, TSeekOrigin.soBeginning);
  ReadOddEvenPageSettingsAndFootNoteSettings(AReader);
  ReadDocCreatedInfo(AReader);
  ReadDisplaySettings(AReader);
  ReadDefaultTabWidth(AReader);
  ReadHyphenationSettings(AReader);
  ReadDocStatistics(AReader);
  ReadEndNoteSettings(AReader);
  ReadDocStatistics2(AReader);
  ReadDocZoomAndPositionInfo(AReader);
  ReadDocumentProtection(AReader, AOffset);
  ReadPageBackground(AReader, AOffset);
end;

procedure TdxDocDocumentProperties.ReadOddEvenPageSettingsAndFootNoteSettings(AReader: TBinaryReader);
var
  AByteFlags: Byte;
  AShortFlags: Word;
begin
  AByteFlags := AReader.ReadByte;
  FDifferentOddAndEvenPage := AByteFlags and $01;
  FWidowControl := AByteFlags and $02;
  FFootNotePosition := TdxFootNotePositionCalculator.CalcFootNotePosition(AByteFlags and $60);
  AReader.BaseStream.Seek(1, TSeekOrigin.soCurrent);
  AShortFlags := AReader.ReadUInt16;
  FFootNoteNumberingRestartType := TdxFootNoteNumberingRestartCalculator.CalcFootNoteNumberingRestart(AShortFlags and $0003);
  FFootnoteInitialNumber := AShortFlags shr 2;
end;

procedure TdxDocDocumentProperties.ReadDocCreatedInfo(AReader: TBinaryReader);
var
  AShortFlags: Word;
begin
  AShortFlags := AReader.ReadUInt16;
  FOutlineDirtySave := AShortFlags and $0001;
  FOnlyMacPics := AShortFlags and $0100;
  FOnlyWinPics := AShortFlags and $0200;
  FLabelDoc := AShortFlags and $0400;
  FHyphCapitals := AShortFlags and $0800;
  FAutoHyphen := AShortFlags and $1000;
  FFormNoFields := AShortFlags and $2000;
  FLinkStyles := AShortFlags and $4000;
  FRevMarking := AShortFlags and $8000;
end;

procedure TdxDocDocumentProperties.ReadDisplaySettings(AReader: TBinaryReader);
var
  AShortFlags: Word;
begin
  AShortFlags := AReader.ReadUInt16;
  FBackup := AShortFlags and $0001;
  FExactWordsCount := AShortFlags and $0002;
  FDisplayHiddenContent := AShortFlags and $0004;
  FDisplayFieldResults := AShortFlags and $0008;
  FLockAnnotations := AShortFlags and $0010;
  FMirrorMargins := AShortFlags and $0020;
  FTrueTypeFontsByDefault := AShortFlags and $0080;
  FProtectedFromEditing := AShortFlags and $0200;
  FDisplayFormFieldsSelection := AShortFlags and $0400;
  FShowRevisionMarkings := AShortFlags and $0800;
  FPrintRevisionMarkings := AShortFlags and $1000;
  FLockRevisionMarkings := AShortFlags and $4000;
  FContainsEmbeddedFonts := AShortFlags and $8000;
  AReader.BaseStream.Seek(2, TSeekOrigin.soCurrent);
end;

procedure TdxDocDocumentProperties.ReadDefaultTabWidth(AReader: TBinaryReader);
begin
  DefaultTabWidth := AReader.ReadSmallInt;
  AReader.BaseStream.Seek(2, TSeekOrigin.soCurrent);
end;

procedure TdxDocDocumentProperties.ReadHyphenationSettings(AReader: TBinaryReader);
begin
  FHyphenationHotZone := AReader.ReadSmallInt;
  FHyphenationConsecutiveLimit := AReader.ReadSmallInt;
  AReader.BaseStream.Seek(2, TSeekOrigin.soCurrent);
end;

procedure TdxDocDocumentProperties.ReadDocStatistics(AReader: TBinaryReader);
begin
  FDateCreated := ReadDTTM(AReader.ReadCardinal);
  FDateRevised := ReadDTTM(AReader.ReadCardinal);
  FDateLastPrinted := ReadDTTM(AReader.ReadCardinal);
  FRevisionNumber := AReader.ReadSmallInt;
  FTimeEdited := AReader.ReadInteger;
  FWordsCount := AReader.ReadInteger;
  FCharactersCount := AReader.ReadInteger;
  FPagesCount := AReader.ReadSmallInt;
  FParagraphsCount := AReader.ReadInteger;
end;

procedure TdxDocDocumentProperties.ReadEndNoteSettings(AReader: TBinaryReader);
var
  AShortFlags: Word;
begin
  AShortFlags := AReader.ReadWord;
  FEndNoteNumberingRestartType := TdxFootNoteNumberingRestartCalculator.CalcFootNoteNumberingRestart(AShortFlags and $0003);
  FEndnoteInitialNumber := AShortFlags shr 2;
  AShortFlags := AReader.ReadWord;
  FEndNotePosition := TdxFootNotePositionCalculator.CalcFootNotePosition(AShortFlags and $0003);
  FShadeFormFields := AShortFlags and $1000;
  FIncludeFootnotesAndEndNotesInWordsCount := AShortFlags and $8000;
end;

procedure TdxDocDocumentProperties.ReadDocStatistics2(AReader: TBinaryReader);
begin
  FLinesCount := AReader.ReadInt32;
  FWordsCountInNotes := AReader.ReadInt32;
  FCharactersCountInNotes := AReader.ReadInt32;
  FPagesCountInNotes := AReader.ReadSmallInt;
  FParagraphsCountInNotes := AReader.ReadInt32;
  FLinesCountInNotes := AReader.ReadInt32;
end;

procedure TdxDocDocumentProperties.ReadDocZoomAndPositionInfo(AReader: TBinaryReader);
var
  AShortFlags: Word;
begin
  FDocumentProtectionPasswordKey := AReader.ReadInt32;
  AShortFlags := AReader.ReadWord;
  FViewKind := TdxDocumentViewKind(AShortFlags and $0007);
  FZoomPercentage := SmallInt(AShortFlags shr 3);
  FZoomType := TdxZoomType(AShortFlags and $3000);
  FGutterPosition := TdxGutterPosition(AShortFlags and $8000);
  AReader.BaseStream.Seek(2, TSeekOrigin.soCurrent);
  FDocumentType := TdxAutoformatDocumentType(AReader.ReadSmallInt);
  FDoptypography := TdxDocumentTypographyInfo.FromStream(AReader);
end;

procedure TdxDocDocumentProperties.ReadDocumentProtection(AReader: TBinaryReader; AOffset: Integer);
var
  AProtectionOffset, AHash: Integer;
  AShortFlags: Word;
begin
  AProtectionOffset := AOffset + ProtectionPosition;
  if AProtectionOffset >= AReader.BaseStream.Size then
    Exit;

  AReader.BaseStream.Seek(AOffset + PasswordHashPosition, TSeekOrigin.soBeginning);
  AHash := AReader.ReadInt32;
  if AHash <> 0 then
  begin
    PasswordHash := TdxByteArrayHelper.From<Integer>(AHash);
    TArray.Reverse<Byte>(FPasswordHash);
  end;
  AReader.BaseStream.Seek(AProtectionOffset, TSeekOrigin.soBeginning);
  AShortFlags := AReader.ReadUInt16;
  FEnforceProtection := AShortFlags and $0008;
  ProtectionType := TdxDocumentProtectionTypeCalculator.CalcDocumentProtectionType(SmallInt(AShortFlags and $0070) shr 4);
end;

procedure TdxDocDocumentProperties.ReadPageBackground(AReader: TBinaryReader; AOffset: Integer);
var
  ABackgroundOffset: Integer;
  AByteFlags: Byte;
begin
  ABackgroundOffset := AOffset + BackgroundPosition;
  if ABackgroundOffset >= AReader.BaseStream.Size then
    Exit;

  AReader.BaseStream.Seek(ABackgroundOffset, TSeekOrigin.soBeginning);
  AByteFlags := AReader.ReadByte;
  FDisplayBackgroundShape := AByteFlags and $80;
end;

procedure TdxDocDocumentProperties.Write(AWriter: TBinaryWriter);
var
  AOffset: Integer;
begin
  AOffset := Integer(AWriter.BaseStream.Position);
  WriteDefaultFlags(AWriter);          //8 bytes, offset = 0
  WriteDefaultTabWidth(AWriter);       //6 bytes, offset = 8
  WriteHyphenationSettings(AWriter);   //6 bytes, offset = 14
  WriteDocStatistics(AWriter);         //32 bytes, offset = 20
  WriteEndNoteSettings(AWriter);       //4 bytes, offset = 52
  WriteDocStatistics2(AWriter);        //22 bytes, offset = 56
  WriteDocZoomAndPosiitonInfo(AWriter);//538 bytes, offset = 78
  WritePageBackgroundAndDocumentProtection(AWriter, AOffset);
end;

procedure TdxDocDocumentProperties.WriteDefaultFlags(AWriter: TBinaryWriter);
var
  AByteFlags: Byte;
  AShortFlags: Word;
begin
  AByteFlags :=
    FDifferentOddAndEvenPage or (FWidowControl shl 1) or
    (TdxFootNotePositionCalculator.CalcFootNotePositionTypeCodeForDocumentProperties(FootNotePosition) shl 5);
  AWriter.Write(AByteFlags);
  AWriter.BaseStream.Seek(1, TSeekOrigin.soCurrent);
  AShortFlags := Word(FFootnoteInitialNumber shl 2) or Word(FFootNoteNumberingRestartType);
  AWriter.Write(AShortFlags);
  AShortFlags :=
    FOutlineDirtySave or FOnlyMacPics or FOnlyWinPics or FLabelDoc or
    FHyphCapitals or FAutoHyphen or FFormNoFields or FLinkStyles or FRevMarking;
  AWriter.Write(AShortFlags);

  if EnforceProtection then
    FLockAnnotations := $0010;
  AShortFlags :=
    FBackup or FExactWordsCount or FDisplayHiddenContent or FDisplayFieldResults or FLockAnnotations or
    FMirrorMargins or FTrueTypeFontsByDefault or FProtectedFromEditing or FDisplayFormFieldsSelection or
    FShowRevisionMarkings or FPrintRevisionMarkings or FLockRevisionMarkings or FContainsEmbeddedFonts;
  AWriter.Write(AShortFlags);
end;

procedure TdxDocDocumentProperties.WriteDefaultTabWidth(AWriter: TBinaryWriter);
begin
  AWriter.Write(DefaultCompatibilities60);
  AWriter.Write(DefaultTabWidth);
  AWriter.BaseStream.Seek(2, TSeekOrigin.soCurrent);
end;

procedure TdxDocDocumentProperties.WriteHyphenationSettings(AWriter: TBinaryWriter);
begin
  AWriter.Write(FHyphenationHotZone);
  AWriter.Write(FHyphenationConsecutiveLimit);
  AWriter.BaseStream.Seek(2, TSeekOrigin.soCurrent);
end;

procedure TdxDocDocumentProperties.WritePageBackgroundAndDocumentProtection(AWriter: TBinaryWriter; AOffset: Integer);
var
  AOriginalOffset: Int64;
  AFlags: Word;
begin
  AOriginalOffset := AWriter.BaseStream.Position;
  AWriter.BaseStream.Seek(AOffset + PasswordHashPosition, TSeekOrigin.soBeginning);
  AWriter.Write(PasswordHash);
  AOriginalOffset := Max(AOriginalOffset, AWriter.BaseStream.Position);
  AFlags := 0;
  AFlags := AFlags or Word(FEnforceProtection shl 3);
  AFlags := AFlags or Word(TdxDocumentProtectionTypeCalculator.CalcDocumentProtectionTypeCode(ProtectionType) shl 4);
  AFlags := AFlags or Word(FDisplayBackgroundShape shl 7);
  AWriter.BaseStream.Seek(AOffset + BackgroundPosition, TSeekOrigin.soBeginning);
  AWriter.Write(AFlags);
  AOriginalOffset := Max(AOriginalOffset, AWriter.BaseStream.Position);
  AWriter.BaseStream.Seek(AOriginalOffset, TSeekOrigin.soBeginning);
end;

procedure TdxDocDocumentProperties.WriteDocStatistics(AWriter: TBinaryWriter);
begin
  WriteDTTM(AWriter, FDateCreated);
  WriteDTTM(AWriter, FDateRevised);
  WriteDTTM(AWriter, FDateLastPrinted);
  AWriter.Write(FRevisionNumber);
  AWriter.Write(FTimeEdited);
  AWriter.Write(FWordsCount);
  AWriter.Write(FCharactersCount);
  AWriter.Write(FPagesCount);
  AWriter.Write(FParagraphsCount);
end;

procedure TdxDocDocumentProperties.WriteEndNoteSettings(AWriter: TBinaryWriter);
var
  AShortFlags: Word;
begin
  AShortFlags := Word(FEndnoteInitialNumber shl 2) or Byte(EndNoteNumberingRestartType <> TdxLineNumberingRestart.Continuous);
  AWriter.Write(AShortFlags);
  AShortFlags :=
    Word(TdxFootNotePositionCalculator.CalcFootNotePositionTypeCode(EndNotePosition)) or
    FShadeFormFields or FIncludeFootnotesAndEndNotesInWordsCount;
  AWriter.Write(AShortFlags);
end;

procedure TdxDocDocumentProperties.WriteDocStatistics2(AWriter: TBinaryWriter);
begin
  AWriter.Write(FLinesCount);
  AWriter.Write(FWordsCountInNotes);
  AWriter.Write(FCharactersCountInNotes);
  AWriter.Write(FPagesCountInNotes);
  AWriter.Write(FParagraphsCountInNotes);
  AWriter.Write(FLinesCountInNotes);
end;

procedure TdxDocDocumentProperties.WriteDocZoomAndPosiitonInfo(AWriter: TBinaryWriter);
var
  AShortFlags: Word;
  I: Integer;
begin
  AWriter.Write(FDocumentProtectionPasswordKey);
  AShortFlags := Word(FViewKind) or Word(FZoomPercentage shl 3) or Word(FZoomType) or Word(FGutterPosition);
  AWriter.Write(AShortFlags);
  AWriter.Write(DefaultCompatibilities80);
  AWriter.Write(SmallInt(FDocumentType));
  FDoptypography.Write(AWriter);

  for I := 0 to 54 - 1 do
    AWriter.Write(SmallInt(0));

  AWriter.Write(DefaultCompatibilities80);

  for I := 0 to 52 - 1 do
    AWriter.Write(SmallInt(0));
end;

function TdxDocDocumentProperties.ReadDTTM(ADttm: Cardinal): TDateTime;
var
  AMinutes, AHours, ADaysOfMonth, AMonths, AYears: Integer;
begin
  if ADttm = 0 then
    Exit(EncodeDate(1601, 1, 1));
  AMinutes := Integer(ADttm and $003f);
  AHours := Integer((ADttm and $07C0) shr 6);
  ADaysOfMonth := Integer((ADttm and $f800) shr 11);
  AMonths := Integer((ADttm and $000f0000) shr 16);
  AYears := Integer((ADttm and $1ff00000) shr 20) + 1900;
  if (AMonths = 0) or (ADaysOfMonth = 0) then
    Exit(EncodeDate(1601, 1, 1));
  Result := EncodeDate(AYears, AMonths, ADaysOfMonth) + EncodeTime(AHours, AMinutes, 0, 0);
end;

procedure TdxDocDocumentProperties.WriteDTTM(AWriter: TBinaryWriter; ADateTime: TDateTime);
var
  AMinutes, AHours, ADayOfMonth, AMonth, AYear: Integer;
  Y, M, D: Word;
begin
  if ADateTime = EncodeDate(1601, 1, 1) then
  begin
    AWriter.Write(Cardinal(0));
    Exit;
  end;

  AMinutes := MinuteOf(ADateTime);
  AHours := HourOf(ADateTime) shl 6;

  DecodeDate(ADateTime, Y, M, D);
  ADayOfMonth := D shl 11;
  AMonth := M shl 16;
  AYear := (Y - 1900) shl 20;
  AWriter.Write(Cardinal(AMinutes or AHours or ADayOfMonth or AMonth or AYear));
end;

end.
