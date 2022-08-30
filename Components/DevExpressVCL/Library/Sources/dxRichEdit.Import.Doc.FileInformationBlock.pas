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
unit dxRichEdit.Import.Doc.FileInformationBlock;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, Classes, Generics.Defaults, Generics.Collections, dxCore, dxCoreGraphics,
  dxRichEdit.Utils.Types,
  dxRichEdit.DocumentModel.UnitConverter,
  dxRichEdit.DocumentModel.Borders,
  dxRichEdit.Doc.Utils;

type

  TdxFileInformationFlag = (
    TemplateDocument,
    GlossaryDocument,
    ComplexFormat,
    HasPictures,
    QuickSaves,
    Encryped,
    TableStreamType,
    ReadOnlyRecommended,
    WritePreservation,
    ExtendedCharset,
    LoadOverride,
    FarEast,
    Obfuscated);
  TdxFileInformationFlags = set of TdxFileInformationFlag;

  TdxFileInformationFlagValues = class
  private const
    TemplateDocument    = $0001;
    GlossaryDocument    = $0002;
    ComplexFormat       = $0004;
    HasPictures         = $0008;
    QuickSaves          = $00f0;
    Encryped            = $0100;
    TableStreamType     = $0200;
    ReadOnlyRecommended = $0400;
    WritePreservation   = $0800;
    ExtendedCharset     = $1000;
    LoadOverride        = $2000;
    FarEast             = $4000;
    Obfuscated          = $8000;
  public
    class function ToFlags(AValue: Word): TdxFileInformationFlags; static;
    class function ToValue(AFlags: TdxFileInformationFlags): Word; static;
  end;

  { TdxFileInformationBlock }

  TdxFileInformationBlock = class
  public const
    FIBSize = Integer($800);
    VersionPosition = Integer($0002);
    FlagsPosition = Integer($000a);
    FibBackCompatiblePosition = Integer($000c);
    Word97SavedFlafPosition = Integer($0013);
    FirstCharacterFileOffsetPosition = Integer($0018);
    LastCharacterFileOffsetPosition = Integer($001c);
    CswPosition = Integer($0020);
    MagicCreatedPosition = Integer($0022);
    LidFEposition = Integer($003c);
    LastByteFileOffsetPosition = Integer($0040);
    MainDocumentLengthPosition = Integer($004c);
    InsufficientMemoryCHPXPageNumberPosition = Integer($006c);
    FirstCHPXFormattedDiskPageNumberPosition = Integer($0070);
    InsufficientMemoryPAPXPageNumberPosition = Integer($0078);
    FirstPAPXFormattedDiskPageNumberPosition = Integer($007c);
    InsufficientMemoryLVCPageNumberPosition = Integer($0084);
    CbRgFcLcbPosition = Integer($0098);
    StyleSheetOffsetOriginalPosition = Integer($009a);
    StyleSheetOffsetPosition = Integer($00a2);
    FootnotesReferenceOffsetPosition = Integer($00aa);
    SectionTableOffsetPosition = Integer($00ca);
    ParagraphHeightsPosition = Integer($00da);
    HeadersFootersPositionsOffsetPosition = Integer($00f2);
    CharacterTableOffsetPosition = Integer($00fa);
    ParagraphTableOffsetPosition = Integer($0102);
    FontTableOffsetPosition = Integer($0112);
    MainDocumentFieldTablePosition = Integer($011a);
    EndNotesFieldTablePosition = Integer($021a);
    MainDocumentTextBoxesFieldTablePosition = Integer($0262);
    HeaderTextBoxesFieldTablePosition = Integer($0272);
    MainDocumentTextBoxesTextOffsetPosition = Integer($025a);
    HeaderTextBoxesTextBoxesTextOffsetPosition = Integer($026a);
    BookmarkInfoPosition = Integer($0142);
    DocumentPropertiesOffsetPosition = Integer($0192);
    ComplexFileInformationOffsetPosition = Integer($01a2);
    MainDocumentFileShapeAddressesOffsetPosition = Integer($01da);
    HeadersDocumentFileShapeAddressesOffsetPosition = Integer($01e2);
    CommentsReferenceOffsetAuthor = Integer($01ba);
    CommentsReferenceOffsetPosition = Integer($01ea);
    EndnotesReferenceOffsetPosition = Integer($020a);
    DrawingObjectDataOffsetPosition = Integer($022a);
    DocumentVariablesOffsetPosition = Integer($027a);
    ListFormatInformationOffsetPosition = Integer($02e2);
    ListFormatOverrideInformationOffsetPosition = Integer($02ea);
    RangeEditPermissionsInformationOffsetPosition = Integer($0502);
    CswNewPosition = Integer($05ba);
    RmdThreadingPosition = Integer($038a);
    DocumentFileRecordsPosition = Integer($03b2);
    FibVersion97 = Word($00c1);
    FibVersion2003 = Word($010c);
    Product = Word($6027);
    Lid = Word($0419);
    LidFE = Word($0409);
    FibVersionBackCompatible = Word($00bf);
    Csw = Word($000e);
    Cslw = Word($0016);
    CswNew = Word($0002);
    CbRgFcLcb = Word($00a4);
    MagicCreated = Word($6a62);
    MagicCreatedPrivate = Word($554c);
    InsufficientMemoryPageNumber = Integer($000fffff);
    RevisedDate = Integer($00023f2e);
    Word97SavedFlag = Byte($10);
    WordBinaryFileSignature = SmallInt($a5ec);
  strict private
    FVersion: Integer;
    FFlags: TdxFileInformationFlags;
    FFirstCharacterFileOffset: Integer;
    FLastCharacterFileOffset: Integer;
    FLastByteFileOffset: Integer;
    FFirstCHPXFormattedDiskPageNumber: Integer;
    FCharactersFormattedDiskPageCount: Integer;
    FFirstPAPXFormattedDiskPageNumber: Integer;
    FParagraphsFormattedDiskPageCount: Integer;
    FMainDocumentLength: Integer;
    FFootNotesLength: Integer;
    FHeadersFootersLength: Integer;
    FMacroLength: Integer;
    FCommentsLength: Integer;
    FEndNotesLength: Integer;
    FMainDocumentTextBoxesLength: Integer;
    FHeaderTextBoxesLength: Integer;
    FComplexFileInformationOffset: Integer;
    FComplexFileInformationSize: Integer;
    FHeadersFootersPositionsOffset: Integer;
    FHeadersFootersPositionsSize: Integer;
    FCharacterTableOffset: Integer;
    FCharacterTableSize: Integer;
    FParagraphTableOffset: Integer;
    FParagraphTableSize: Integer;
    FSectionTableOffset: Integer;
    FSectionTableSize: Integer;
    FParagraphHeightsOffset: Integer;
    FParagraphHeightsSize: Integer;
    FStyleSheetOffset: Integer;
    FStyleSheetSize: Integer;
    FFootNotesReferenceOffset: Integer;
    FFootNotesReferenceSize: Integer;
    FFootNotesTextOffset: Integer;
    FFootNotesTextSize: Integer;
    FCommentsReferenceOffset: Integer;
    FCommentsReferenceSize: Integer;
    FCommentsTextOffset: Integer;
    FCommentsTextSize: Integer;
    FCommentsNameTableOffset: Integer;
    FCommentsNameTableSize: Integer;
    FCommentsFirstTableOffset: Integer;
    FCommentsFirstTableSize: Integer;
    FCommentsLimTableOffset: Integer;
    FCommentsLimTableSize: Integer;
    FCommentsAuthorTableOffset: Integer;
    FCommentsAuthorTableSize: Integer;
    FMainDocumentTextBoxesTextOffset: Integer;
    FMainDocumentTextBoxesTextSize: Integer;
    FHeaderTextBoxesTextOffset: Integer;
    FHeaderTextBoxesTextSize: Integer;
    FFontTableOffset: Integer;
    FFontTableSize: Integer;
    FMainDocumentFieldTableOffset: Integer;
    FMainDocumentFieldTableSize: Integer;
    FFootNotesFieldTableOffset: Integer;
    FFootNotesFieldTableSize: Integer;
    FHeadersFootersFieldTableOffset: Integer;
    FHeadersFootersFieldTableSize: Integer;
    FCommentsFieldTableOffset: Integer;
    FCommentsFieldTableSize: Integer;
    FEndNotesFieldTableOffset: Integer;
    FEndNotesFieldTableSize: Integer;
    FMainDocumentTextBoxesFieldTableOffset: Integer;
    FMainDocumentTextBoxesFieldTableSize: Integer;
    FHeaderTextBoxesFieldTableOffset: Integer;
    FHeaderTextBoxesFieldTableSize: Integer;
    FBookmarkNamesTableOffset: Integer;
    FBookmarkNamesTableSize: Integer;
    FBookmarkStartInfoOffset: Integer;
    FBookmarkStartInfoSize: Integer;
    FBookmarkEndInfoOffset: Integer;
    FBookmarkEndInfoSize: Integer;
    FMainDocumentFileShapeTableOffset: Integer;
    FMainDocumentFileShapeTableSize: Integer;
    FEndNotesReferenceOffset: Integer;
    FEndNotesReferenceSize: Integer;
    FEndnotesTextOffset: Integer;
    FEndnotesTextSize: Integer;
    FHeadersFootersFileShapeTableOffset: Integer;
    FHeadersFootersFileShapeTableSize: Integer;
    FDrawingObjectTableOffset: Integer;
    FDrawingObjectTableSize: Integer;
    FDocumentVariablesOffset: Integer;
    FDocumentVariablesSize: Integer;
    FListFormatInformationOffset: Integer;
    FListFormatInformationSize: Integer;
    FRangeEditPermissionsInformationOffset: Integer;
    FRangeEditPermissionsInformationSize: Integer;
    FRangeEditPermissionsStartInfoOffset: Integer;
    FRangeEditPermissionsStartInfoSize: Integer;
    FRangeEditPermissionsEndInfoOffset: Integer;
    FRangeEditPermissionsEndInfoSize: Integer;
    FRangeEditPermissionsUsernamesOffset: Integer;
    FRangeEditPermissionsUsernamesSize: Integer;
    FListFormatOverrideInformationOffset: Integer;
    FListFormatOverrideInformationSize: Integer;
    FMainTextBoxBreakTableOffset: Integer;
    FMainTextBoxBreakTableSize: Integer;
    FHeadersFootersTextBoxBreakTableOffset: Integer;
    FHeadersFootersTextBoxBreakTableSize: Integer;
    FDocumentPropertiesOffset: Integer;
    FDocumentPropertiesSize: Integer;
    FRmdThreadingOffset: Integer;
    FRmdThreadingSize: Integer;
    FDocumentFileRecordsOffset: Integer;
    FDocumentFileRecordsSize: Integer;
    function GetHeadersFootersStart: Integer;
    function GetMacroStart: Integer;
    function GetCommentsStart: Integer;
    function GetEndnotesStart: Integer;
    function GetMainDocumentTextBoxesStart: Integer;
    function GetHeaderTextBoxesStart: Integer;
  public
    procedure Read(AReader: TBinaryReader);
    procedure ReadFormatSpecificInfo(AReader: TBinaryReader);
    procedure ReadFormattedDiskPagesInfo(AReader: TBinaryReader);
    procedure ReadComplexFileInfo(AReader: TBinaryReader);
    procedure ReadDocumentStructureInfo(AReader: TBinaryReader);
    procedure ReadReferenceInfo(AReader: TBinaryReader);
    procedure ReadPieceTablesInfo(AReader: TBinaryReader);
    procedure ReadHeadersFootersInfo(AReader: TBinaryReader);
    procedure ReadFontsInfo(AReader: TBinaryReader);
    procedure ReadStyleSheetInfo(AReader: TBinaryReader);
    procedure ReadFieldsInfo(AReader: TBinaryReader);
    procedure ReadListsInfo(AReader: TBinaryReader);
    procedure ReadBookmarksInfo(AReader: TBinaryReader);
    procedure ReadShapesAndDrawingObjectsInfo(AReader: TBinaryReader);
    procedure ReadDocumentVariablesInfo(AReader: TBinaryReader);
    procedure ReadDocumentPropertiesInfo(AReader: TBinaryReader);
    procedure ReadRangeEditPermissionsInfo(AReader: TBinaryReader);
    procedure ReadRmdThreading(AReader: TBinaryReader);
    procedure ReadDocumentFileRecords(AReader: TBinaryReader);
    procedure Write(AWriter: TBinaryWriter);
    procedure WriteFormatSpecificInfo(AWriter: TBinaryWriter);
    procedure WriteReferenceInfo(AWriter: TBinaryWriter);
    procedure WriteHeadersFootersInfo(AWriter: TBinaryWriter);
    procedure WriteFormattedDiskPagesInfo(AWriter: TBinaryWriter);
    procedure WritePieceTableInfo(AWriter: TBinaryWriter);
    procedure WriteDocumentStructureInfo(AWriter: TBinaryWriter);
    procedure WriteInsufficientMemoryInfo(AWriter: TBinaryWriter);
    procedure WriteStyleSheetInfo(AWriter: TBinaryWriter);
    procedure WriteFieldsInfo(AWriter: TBinaryWriter);
    procedure WriteFontsInfo(AWriter: TBinaryWriter);
    procedure WriteBookmarksInfo(AWriter: TBinaryWriter);
    procedure WriteComplexFileInfo(AWriter: TBinaryWriter);
    procedure WriteShapesAndDrawingObjectsInfo(AWriter: TBinaryWriter);
    procedure WriteListInfo(AWriter: TBinaryWriter);
    procedure WriteDocumentVariablesInfo(AWriter: TBinaryWriter);
    procedure WriteDocumentPropertiesInfo(AWriter: TBinaryWriter);
    procedure WriteRangeEditPermissionsInfo(AWriter: TBinaryWriter);
    procedure WriteRmdThreading(AWriter: TBinaryWriter);

    property Version: Integer read FVersion;
    property Flags: TdxFileInformationFlags read FFlags write FFlags;
    property FirstCharacterFileOffset: Integer read FFirstCharacterFileOffset write FFirstCharacterFileOffset;
    property LastCharacterFileOffset: Integer read FLastCharacterFileOffset write FLastCharacterFileOffset;
    property LastByteFileOffset: Integer read FLastByteFileOffset write FLastByteFileOffset;
    property FirstCHPXFormattedDiskPageNumber: Integer read FFirstCHPXFormattedDiskPageNumber write FFirstCHPXFormattedDiskPageNumber;
    property CharactersFormattedDiskPageCount: Integer read FCharactersFormattedDiskPageCount write FCharactersFormattedDiskPageCount;
    property FirstPAPXFormattedDiskPageNumber: Integer read FFirstPAPXFormattedDiskPageNumber write FFirstPAPXFormattedDiskPageNumber;
    property ParagraphsFormattedDiskPageCount: Integer read FParagraphsFormattedDiskPageCount write FParagraphsFormattedDiskPageCount;
    property MainDocumentLength: Integer read FMainDocumentLength write FMainDocumentLength;
    property FootNotesLength: Integer read FFootNotesLength write FFootNotesLength;
    property HeadersFootersLength: Integer read FHeadersFootersLength write FHeadersFootersLength;
    property MacroLength: Integer read FMacroLength write FMacroLength;
    property CommentsLength: Integer read FCommentsLength write FCommentsLength;
    property EndNotesLength: Integer read FEndNotesLength write FEndNotesLength;
    property MainDocumentTextBoxesLength: Integer read FMainDocumentTextBoxesLength write FMainDocumentTextBoxesLength;
    property HeaderTextBoxesLength: Integer read FHeaderTextBoxesLength write FHeaderTextBoxesLength;
    property ComplexFileInformationOffset: Integer read FComplexFileInformationOffset write FComplexFileInformationOffset;
    property ComplexFileInformationSize: Integer read FComplexFileInformationSize write FComplexFileInformationSize;
    property HeadersFootersPositionsOffset: Integer read FHeadersFootersPositionsOffset write FHeadersFootersPositionsOffset;
    property HeadersFootersPositionsSize: Integer read FHeadersFootersPositionsSize write FHeadersFootersPositionsSize;
    property CharacterTableOffset: Integer read FCharacterTableOffset write FCharacterTableOffset;
    property CharacterTableSize: Integer read FCharacterTableSize write FCharacterTableSize;
    property ParagraphTableOffset: Integer read FParagraphTableOffset write FParagraphTableOffset;
    property ParagraphTableSize: Integer read FParagraphTableSize write FParagraphTableSize;
    property SectionTableOffset: Integer read FSectionTableOffset write FSectionTableOffset;
    property SectionTableSize: Integer read FSectionTableSize write FSectionTableSize;
    property ParagraphHeightsOffset: Integer read FParagraphHeightsOffset write FParagraphHeightsOffset;
    property ParagraphHeightsSize: Integer read FParagraphHeightsSize write FParagraphHeightsSize;
    property StyleSheetOffset: Integer read FStyleSheetOffset write FStyleSheetOffset;
    property StyleSheetSize: Integer read FStyleSheetSize write FStyleSheetSize;
    property FootNotesReferenceOffset: Integer read FFootNotesReferenceOffset write FFootNotesReferenceOffset;
    property FootNotesReferenceSize: Integer read FFootNotesReferenceSize write FFootNotesReferenceSize;
    property FootNotesTextOffset: Integer read FFootNotesTextOffset write FFootNotesTextOffset;
    property FootNotesTextSize: Integer read FFootNotesTextSize write FFootNotesTextSize;
    property CommentsReferenceOffset: Integer read FCommentsReferenceOffset write FCommentsReferenceOffset;
    property CommentsReferenceSize: Integer read FCommentsReferenceSize write FCommentsReferenceSize;
    property CommentsTextOffset: Integer read FCommentsTextOffset write FCommentsTextOffset;
    property CommentsTextSize: Integer read FCommentsTextSize write FCommentsTextSize;
    property CommentsNameTableOffset: Integer read FCommentsNameTableOffset write FCommentsNameTableOffset;
    property CommentsNameTableSize: Integer read FCommentsNameTableSize write FCommentsNameTableSize;
    property CommentsFirstTableOffset: Integer read FCommentsFirstTableOffset write FCommentsFirstTableOffset;
    property CommentsFirstTableSize: Integer read FCommentsFirstTableSize write FCommentsFirstTableSize;
    property CommentsLimTableOffset: Integer read FCommentsLimTableOffset write FCommentsLimTableOffset;
    property CommentsLimTableSize: Integer read FCommentsLimTableSize write FCommentsLimTableSize;
    property CommentsAuthorTableOffset: Integer read FCommentsAuthorTableOffset write FCommentsAuthorTableOffset;
    property CommentsAuthorTableSize: Integer read FCommentsAuthorTableSize write FCommentsAuthorTableSize;
    property MainDocumentTextBoxesTextOffset: Integer read FMainDocumentTextBoxesTextOffset write FMainDocumentTextBoxesTextOffset;
    property MainDocumentTextBoxesTextSize: Integer read FMainDocumentTextBoxesTextSize write FMainDocumentTextBoxesTextSize;
    property HeaderTextBoxesTextOffset: Integer read FHeaderTextBoxesTextOffset write FHeaderTextBoxesTextOffset;
    property HeaderTextBoxesTextSize: Integer read FHeaderTextBoxesTextSize write FHeaderTextBoxesTextSize;
    property FontTableOffset: Integer read FFontTableOffset write FFontTableOffset;
    property FontTableSize: Integer read FFontTableSize write FFontTableSize;
    property MainDocumentFieldTableOffset: Integer read FMainDocumentFieldTableOffset write FMainDocumentFieldTableOffset;
    property MainDocumentFieldTableSize: Integer read FMainDocumentFieldTableSize write FMainDocumentFieldTableSize;
    property FootNotesFieldTableOffset: Integer read FFootNotesFieldTableOffset write FFootNotesFieldTableOffset;
    property FootNotesFieldTableSize: Integer read FFootNotesFieldTableSize write FFootNotesFieldTableSize;
    property HeadersFootersFieldTableOffset: Integer read FHeadersFootersFieldTableOffset write FHeadersFootersFieldTableOffset;
    property HeadersFootersFieldTableSize: Integer read FHeadersFootersFieldTableSize write FHeadersFootersFieldTableSize;
    property CommentsFieldTableOffset: Integer read FCommentsFieldTableOffset write FCommentsFieldTableOffset;
    property CommentsFieldTableSize: Integer read FCommentsFieldTableSize write FCommentsFieldTableSize;
    property EndNotesFieldTableOffset: Integer read FEndNotesFieldTableOffset write FEndNotesFieldTableOffset;
    property EndNotesFieldTableSize: Integer read FEndNotesFieldTableSize write FEndNotesFieldTableSize;
    property MainDocumentTextBoxesFieldTableOffset: Integer read FMainDocumentTextBoxesFieldTableOffset write FMainDocumentTextBoxesFieldTableOffset;
    property MainDocumentTextBoxesFieldTableSize: Integer read FMainDocumentTextBoxesFieldTableSize write FMainDocumentTextBoxesFieldTableSize;
    property HeaderTextBoxesFieldTableOffset: Integer read FHeaderTextBoxesFieldTableOffset write FHeaderTextBoxesFieldTableOffset;
    property HeaderTextBoxesFieldTableSize: Integer read FHeaderTextBoxesFieldTableSize write FHeaderTextBoxesFieldTableSize;
    property BookmarkNamesTableOffset: Integer read FBookmarkNamesTableOffset write FBookmarkNamesTableOffset;
    property BookmarkNamesTableSize: Integer read FBookmarkNamesTableSize write FBookmarkNamesTableSize;
    property BookmarkStartInfoOffset: Integer read FBookmarkStartInfoOffset write FBookmarkStartInfoOffset;
    property BookmarkStartInfoSize: Integer read FBookmarkStartInfoSize write FBookmarkStartInfoSize;
    property BookmarkEndInfoOffset: Integer read FBookmarkEndInfoOffset write FBookmarkEndInfoOffset;
    property BookmarkEndInfoSize: Integer read FBookmarkEndInfoSize write FBookmarkEndInfoSize;
    property MainDocumentFileShapeTableOffset: Integer read FMainDocumentFileShapeTableOffset write FMainDocumentFileShapeTableOffset;
    property MainDocumentFileShapeTableSize: Integer read FMainDocumentFileShapeTableSize write FMainDocumentFileShapeTableSize;
    property EndNotesReferenceOffset: Integer read FEndNotesReferenceOffset write FEndNotesReferenceOffset;
    property EndNotesReferenceSize: Integer read FEndNotesReferenceSize write FEndNotesReferenceSize;
    property EndnotesTextOffset: Integer read FEndnotesTextOffset write FEndnotesTextOffset;
    property EndnotesTextSize: Integer read FEndnotesTextSize write FEndnotesTextSize;
    property HeadersFootersFileShapeTableOffset: Integer read FHeadersFootersFileShapeTableOffset write FHeadersFootersFileShapeTableOffset;
    property HeadersFootersFileShapeTableSize: Integer read FHeadersFootersFileShapeTableSize write FHeadersFootersFileShapeTableSize;
    property DrawingObjectTableOffset: Integer read FDrawingObjectTableOffset write FDrawingObjectTableOffset;
    property DrawingObjectTableSize: Integer read FDrawingObjectTableSize write FDrawingObjectTableSize;
    property DocumentVariablesOffset: Integer read FDocumentVariablesOffset write FDocumentVariablesOffset;
    property DocumentVariablesSize: Integer read FDocumentVariablesSize write FDocumentVariablesSize;
    property ListFormatInformationOffset: Integer read FListFormatInformationOffset write FListFormatInformationOffset;
    property ListFormatInformationSize: Integer read FListFormatInformationSize write FListFormatInformationSize;
    property RangeEditPermissionsInformationOffset: Integer read FRangeEditPermissionsInformationOffset write FRangeEditPermissionsInformationOffset;
    property RangeEditPermissionsInformationSize: Integer read FRangeEditPermissionsInformationSize write FRangeEditPermissionsInformationSize;
    property RangeEditPermissionsStartInfoOffset: Integer read FRangeEditPermissionsStartInfoOffset write FRangeEditPermissionsStartInfoOffset;
    property RangeEditPermissionsStartInfoSize: Integer read FRangeEditPermissionsStartInfoSize write FRangeEditPermissionsStartInfoSize;
    property RangeEditPermissionsEndInfoOffset: Integer read FRangeEditPermissionsEndInfoOffset write FRangeEditPermissionsEndInfoOffset;
    property RangeEditPermissionsEndInfoSize: Integer read FRangeEditPermissionsEndInfoSize write FRangeEditPermissionsEndInfoSize;
    property RangeEditPermissionsUsernamesOffset: Integer read FRangeEditPermissionsUsernamesOffset write FRangeEditPermissionsUsernamesOffset;
    property RangeEditPermissionsUsernamesSize: Integer read FRangeEditPermissionsUsernamesSize write FRangeEditPermissionsUsernamesSize;
    property ListFormatOverrideInformationOffset: Integer read FListFormatOverrideInformationOffset write FListFormatOverrideInformationOffset;
    property ListFormatOverrideInformationSize: Integer read FListFormatOverrideInformationSize write FListFormatOverrideInformationSize;
    property MainTextBoxBreakTableOffset: Integer read FMainTextBoxBreakTableOffset write FMainTextBoxBreakTableOffset;
    property MainTextBoxBreakTableSize: Integer read FMainTextBoxBreakTableSize write FMainTextBoxBreakTableSize;
    property HeadersFootersTextBoxBreakTableOffset: Integer read FHeadersFootersTextBoxBreakTableOffset write FHeadersFootersTextBoxBreakTableOffset;
    property HeadersFootersTextBoxBreakTableSize: Integer read FHeadersFootersTextBoxBreakTableSize write FHeadersFootersTextBoxBreakTableSize;
    property DocumentPropertiesOffset: Integer read FDocumentPropertiesOffset write FDocumentPropertiesOffset;
    property DocumentPropertiesSize: Integer read FDocumentPropertiesSize write FDocumentPropertiesSize;
    property RmdThreadingOffset: Integer read FRmdThreadingOffset write FRmdThreadingOffset;
    property RmdThreadingSize: Integer read FRmdThreadingSize write FRmdThreadingSize;
    property DocumentFileRecordsOffset: Integer read FDocumentFileRecordsOffset write FDocumentFileRecordsOffset;
    property DocumentFileRecordsSize: Integer read FDocumentFileRecordsSize write FDocumentFileRecordsSize;
    property CommentsStart: Integer read GetCommentsStart;
    property HeadersFootersStart: Integer read GetHeadersFootersStart;
    property HeaderTextBoxesStart: Integer read GetHeaderTextBoxesStart;
    property EndnotesStart: Integer read GetEndnotesStart;
    property FootNotesStart: Integer read FMainDocumentLength;
    property MacroStart: Integer read GetMacroStart;
    property MainDocumentTextBoxesStart: Integer read GetMainDocumentTextBoxesStart;
  end;

implementation

{ TdxFileInformationFlagValues }

class function TdxFileInformationFlagValues.ToFlags(
  AValue: Word): TdxFileInformationFlags;
begin
  Result := [];
  if TemplateDocument and AValue <> 0 then
    Include(Result, TdxFileInformationFlag.TemplateDocument);
  if GlossaryDocument and AValue <> 0 then
    Include(Result, TdxFileInformationFlag.GlossaryDocument);
  if ComplexFormat and AValue <> 0 then
    Include(Result, TdxFileInformationFlag.ComplexFormat);
  if HasPictures and AValue <> 0 then
    Include(Result, TdxFileInformationFlag.HasPictures);
  if QuickSaves and AValue <> 0 then
    Include(Result, TdxFileInformationFlag.QuickSaves);
  if Encryped and AValue <> 0 then
    Include(Result, TdxFileInformationFlag.Encryped);
  if TableStreamType and AValue <> 0 then
    Include(Result, TdxFileInformationFlag.TableStreamType);
  if ReadOnlyRecommended and AValue <> 0 then
    Include(Result, TdxFileInformationFlag.ReadOnlyRecommended);
  if WritePreservation and AValue <> 0 then
    Include(Result, TdxFileInformationFlag.WritePreservation);
  if ExtendedCharset and AValue <> 0 then
    Include(Result, TdxFileInformationFlag.ExtendedCharset);
  if LoadOverride and AValue <> 0 then
    Include(Result, TdxFileInformationFlag.LoadOverride);
  if FarEast and AValue <> 0 then
    Include(Result, TdxFileInformationFlag.FarEast);
  if Obfuscated and AValue <> 0 then
    Include(Result, TdxFileInformationFlag.Obfuscated);
end;

class function TdxFileInformationFlagValues.ToValue(
  AFlags: TdxFileInformationFlags): Word;
begin
  Result := 0;
  if TdxFileInformationFlag.TemplateDocument in AFlags then
    Result := Result or TemplateDocument;
  if TdxFileInformationFlag.GlossaryDocument in AFlags then
    Result := Result or GlossaryDocument;
  if TdxFileInformationFlag.ComplexFormat in AFlags then
    Result := Result or ComplexFormat;
  if TdxFileInformationFlag.HasPictures in AFlags then
    Result := Result or HasPictures;
  if TdxFileInformationFlag.QuickSaves in AFlags then
    Result := Result or QuickSaves;
  if TdxFileInformationFlag.Encryped in AFlags then
    Result := Result or Encryped;
  if TdxFileInformationFlag.TableStreamType in AFlags then
    Result := Result or TableStreamType;
  if TdxFileInformationFlag.ReadOnlyRecommended in AFlags then
    Result := Result or ReadOnlyRecommended;
  if TdxFileInformationFlag.WritePreservation in AFlags then
    Result := Result or WritePreservation;
  if TdxFileInformationFlag.ExtendedCharset in AFlags then
    Result := Result or ExtendedCharset;
  if TdxFileInformationFlag.LoadOverride in AFlags then
    Result := Result or LoadOverride;
  if TdxFileInformationFlag.FarEast in AFlags then
    Result := Result or FarEast;
  if TdxFileInformationFlag.Obfuscated in AFlags then
    Result := Result or Obfuscated;
end;

{ TdxFileInformationBlock }

function TdxFileInformationBlock.GetHeadersFootersStart: Integer;
begin
  Result := FootNotesStart + FootNotesLength;
end;

function TdxFileInformationBlock.GetMacroStart: Integer;
begin
  Result := HeadersFootersStart + HeadersFootersLength;
end;

function TdxFileInformationBlock.GetCommentsStart: Integer;
begin
  Result := MacroStart + MacroLength;
end;

function TdxFileInformationBlock.GetEndnotesStart: Integer;
begin
  Result := CommentsStart + CommentsLength;
end;

function TdxFileInformationBlock.GetMainDocumentTextBoxesStart: Integer;
begin
  Result := EndnotesStart + EndNotesLength;
end;

function TdxFileInformationBlock.GetHeaderTextBoxesStart: Integer;
begin
  Result := MainDocumentTextBoxesStart + MainDocumentTextBoxesLength;
end;

procedure TdxFileInformationBlock.Read(AReader: TBinaryReader);
begin
  Assert(AReader <> nil, 'reader');
  ReadFormatSpecificInfo(AReader);
  ReadFormattedDiskPagesInfo(AReader);
  ReadComplexFileInfo(AReader);
  ReadDocumentStructureInfo(AReader);

  ReadReferenceInfo(AReader);
  ReadPieceTablesInfo(AReader);
  ReadHeadersFootersInfo(AReader);

  ReadFontsInfo(AReader);
  ReadStyleSheetInfo(AReader);
  ReadFieldsInfo(AReader);
  ReadListsInfo(AReader);
  ReadBookmarksInfo(AReader);
  ReadShapesAndDrawingObjectsInfo(AReader);

  ReadDocumentVariablesInfo(AReader);
  ReadDocumentPropertiesInfo(AReader);
  ReadRangeEditPermissionsInfo(AReader);
  ReadRmdThreading(AReader);
  ReadDocumentFileRecords(AReader);
end;

procedure TdxFileInformationBlock.ReadFormatSpecificInfo(AReader: TBinaryReader);
begin
  AReader.BaseStream.Seek(VersionPosition, TSeekOrigin.soBeginning);
  FVersion := AReader.ReadUInt16;
  AReader.BaseStream.Seek(FlagsPosition, TSeekOrigin.soBeginning);
  Flags := TdxFileInformationFlagValues.ToFlags(AReader.ReadUInt16);
end;

procedure TdxFileInformationBlock.ReadFormattedDiskPagesInfo(AReader: TBinaryReader);
begin
  AReader.BaseStream.Seek(FirstCharacterFileOffsetPosition, TSeekOrigin.soBeginning);
  FirstCharacterFileOffset := AReader.ReadInt32;
  LastCharacterFileOffset := AReader.ReadInt32;
  AReader.BaseStream.Seek(LastByteFileOffsetPosition, TSeekOrigin.soBeginning);
  LastByteFileOffset := AReader.ReadInt32;

  AReader.BaseStream.Seek(FirstCHPXFormattedDiskPageNumberPosition, TSeekOrigin.soBeginning);
  FirstCHPXFormattedDiskPageNumber := AReader.ReadInt32;
  CharactersFormattedDiskPageCount := AReader.ReadInt32;
  AReader.BaseStream.Seek(FirstPAPXFormattedDiskPageNumberPosition, TSeekOrigin.soBeginning);
  FirstPAPXFormattedDiskPageNumber := AReader.ReadInt32;
  ParagraphsFormattedDiskPageCount := AReader.ReadInt32;
end;

procedure TdxFileInformationBlock.ReadComplexFileInfo(AReader: TBinaryReader);
begin
  AReader.BaseStream.Seek(ComplexFileInformationOffsetPosition, TSeekOrigin.soBeginning);
  ComplexFileInformationOffset := AReader.ReadInt32;
  ComplexFileInformationSize := AReader.ReadInt32;
end;

procedure TdxFileInformationBlock.ReadDocumentStructureInfo(AReader: TBinaryReader);
begin
  AReader.BaseStream.Seek(MainDocumentLengthPosition, TSeekOrigin.soBeginning);
  MainDocumentLength := AReader.ReadInt32;
  FootNotesLength := AReader.ReadInt32;
  HeadersFootersLength := AReader.ReadInt32;
  MacroLength := AReader.ReadInt32;
  CommentsLength := AReader.ReadInt32;
  EndNotesLength := AReader.ReadInt32;
  MainDocumentTextBoxesLength := AReader.ReadInt32;
  HeaderTextBoxesLength := AReader.ReadInt32;
end;

procedure TdxFileInformationBlock.ReadReferenceInfo(AReader: TBinaryReader);
begin
  AReader.BaseStream.Seek(FootnotesReferenceOffsetPosition, TSeekOrigin.soBeginning);
  FootNotesReferenceOffset := AReader.ReadInt32;
  FootNotesReferenceSize := AReader.ReadInt32;
  FootNotesTextOffset := AReader.ReadInt32;
  FootNotesTextSize := AReader.ReadInt32;
  CommentsReferenceOffset := AReader.ReadInt32;
  CommentsReferenceSize := AReader.ReadInt32;
  CommentsTextOffset := AReader.ReadInt32;
  CommentsTextSize := AReader.ReadInt32;
  AReader.BaseStream.Seek(CommentsReferenceOffsetAuthor, TSeekOrigin.soBeginning);
  CommentsAuthorTableOffset := AReader.ReadInt32;
  CommentsAuthorTableSize := AReader.ReadInt32;
  CommentsNameTableOffset := AReader.ReadInt32;
  CommentsNameTableSize := AReader.ReadInt32;
  AReader.BaseStream.Seek(CommentsReferenceOffsetPosition, TSeekOrigin.soBeginning);
  CommentsFirstTableOffset := AReader.ReadInt32;
  CommentsFirstTableSize := AReader.ReadInt32;
  CommentsLimTableOffset := AReader.ReadInt32;
  CommentsLimTableSize := AReader.ReadInt32;
  AReader.BaseStream.Seek(EndnotesReferenceOffsetPosition, TSeekOrigin.soBeginning);
  EndNotesReferenceOffset := AReader.ReadInt32;
  EndNotesReferenceSize := AReader.ReadInt32;
  EndnotesTextOffset := AReader.ReadInt32;
  EndnotesTextSize := AReader.ReadInt32;
  AReader.BaseStream.Seek(MainDocumentTextBoxesTextOffsetPosition, TSeekOrigin.soBeginning);
  MainDocumentTextBoxesTextOffset := AReader.ReadInt32;
  MainDocumentTextBoxesTextSize := AReader.ReadInt32;
  AReader.BaseStream.Seek(HeaderTextBoxesTextBoxesTextOffsetPosition, TSeekOrigin.soBeginning);
  HeaderTextBoxesTextOffset := AReader.ReadInt32;
  HeaderTextBoxesTextSize := AReader.ReadInt32;
end;

procedure TdxFileInformationBlock.ReadPieceTablesInfo(AReader: TBinaryReader);
begin
  AReader.BaseStream.Seek(CharacterTableOffsetPosition, TSeekOrigin.soBeginning);
  CharacterTableOffset := AReader.ReadInt32;
  CharacterTableSize := AReader.ReadInt32;
  AReader.BaseStream.Seek(ParagraphTableOffsetPosition, TSeekOrigin.soBeginning);
  ParagraphTableOffset := AReader.ReadInt32;
  ParagraphTableSize := AReader.ReadInt32;
  AReader.BaseStream.Seek(SectionTableOffsetPosition, TSeekOrigin.soBeginning);
  SectionTableOffset := AReader.ReadInt32;
  SectionTableSize := AReader.ReadInt32;
  AReader.BaseStream.Seek(ParagraphHeightsPosition, TSeekOrigin.soBeginning);
  ParagraphHeightsOffset := AReader.ReadInt32;
  ParagraphHeightsSize := AReader.ReadInt32;
end;

procedure TdxFileInformationBlock.ReadHeadersFootersInfo(AReader: TBinaryReader);
begin
  AReader.BaseStream.Seek(HeadersFootersPositionsOffsetPosition, TSeekOrigin.soBeginning);
  HeadersFootersPositionsOffset := AReader.ReadInt32;
  HeadersFootersPositionsSize := AReader.ReadInt32;
end;

procedure TdxFileInformationBlock.ReadFontsInfo(AReader: TBinaryReader);
begin
  AReader.BaseStream.Seek(FontTableOffsetPosition, TSeekOrigin.soBeginning);
  FontTableOffset := AReader.ReadInt32;
  FontTableSize := AReader.ReadInt32;
end;

procedure TdxFileInformationBlock.ReadStyleSheetInfo(AReader: TBinaryReader);
begin
  AReader.BaseStream.Seek(StyleSheetOffsetPosition, TSeekOrigin.soBeginning);
  StyleSheetOffset := AReader.ReadInt32;
  StyleSheetSize := AReader.ReadInt32;
end;

procedure TdxFileInformationBlock.ReadFieldsInfo(AReader: TBinaryReader);
begin
  AReader.BaseStream.Seek(MainDocumentFieldTablePosition, TSeekOrigin.soBeginning);
  MainDocumentFieldTableOffset := AReader.ReadInt32;
  MainDocumentFieldTableSize := AReader.ReadInt32;
  HeadersFootersFieldTableOffset := AReader.ReadInt32;
  HeadersFootersFieldTableSize := AReader.ReadInt32;
  FootNotesFieldTableOffset := AReader.ReadInt32;
  FootNotesFieldTableSize := AReader.ReadInt32;
  CommentsFieldTableOffset := AReader.ReadInt32;
  CommentsFieldTableSize := AReader.ReadInt32;
  AReader.BaseStream.Seek(EndNotesFieldTablePosition, TSeekOrigin.soBeginning);
  EndNotesFieldTableOffset := AReader.ReadInt32;
  EndNotesFieldTableSize := AReader.ReadInt32;
  AReader.BaseStream.Seek(MainDocumentTextBoxesFieldTablePosition, TSeekOrigin.soBeginning);
  MainDocumentTextBoxesFieldTableOffset := AReader.ReadInt32;
  MainDocumentTextBoxesFieldTableSize := AReader.ReadInt32;
  AReader.BaseStream.Seek(HeaderTextBoxesFieldTablePosition, TSeekOrigin.soBeginning);
  HeaderTextBoxesFieldTableOffset := AReader.ReadInt32;
  HeaderTextBoxesFieldTableSize := AReader.ReadInt32;
end;

procedure TdxFileInformationBlock.ReadListsInfo(AReader: TBinaryReader);
begin
  AReader.BaseStream.Seek(ListFormatInformationOffsetPosition, TSeekOrigin.soBeginning);
  ListFormatInformationOffset := AReader.ReadInt32;
  ListFormatInformationSize := AReader.ReadInt32;

  AReader.BaseStream.Seek(ListFormatOverrideInformationOffsetPosition, TSeekOrigin.soBeginning);
  ListFormatOverrideInformationOffset := AReader.ReadInt32;
  ListFormatOverrideInformationSize := AReader.ReadInt32;
  MainTextBoxBreakTableOffset := AReader.ReadInt32;
  MainTextBoxBreakTableSize := AReader.ReadInt32;
  HeadersFootersTextBoxBreakTableOffset := AReader.ReadInt32;
  HeadersFootersTextBoxBreakTableSize := AReader.ReadInt32;
end;

procedure TdxFileInformationBlock.ReadBookmarksInfo(AReader: TBinaryReader);
begin
  AReader.BaseStream.Seek(BookmarkInfoPosition, TSeekOrigin.soBeginning);
  BookmarkNamesTableOffset := AReader.ReadInt32;
  BookmarkNamesTableSize := AReader.ReadInt32;
  BookmarkStartInfoOffset := AReader.ReadInt32;
  BookmarkStartInfoSize := AReader.ReadInt32;
  BookmarkEndInfoOffset := AReader.ReadInt32;
  BookmarkEndInfoSize := AReader.ReadInt32;
end;

procedure TdxFileInformationBlock.ReadShapesAndDrawingObjectsInfo(AReader: TBinaryReader);
begin
  AReader.BaseStream.Seek(MainDocumentFileShapeAddressesOffsetPosition, TSeekOrigin.soBeginning);
  MainDocumentFileShapeTableOffset := AReader.ReadInt32;
  MainDocumentFileShapeTableSize := AReader.ReadInt32;

  AReader.BaseStream.Seek(HeadersDocumentFileShapeAddressesOffsetPosition, TSeekOrigin.soBeginning);
  HeadersFootersFileShapeTableOffset := AReader.ReadInt32;
  HeadersFootersFileShapeTableSize := AReader.ReadInt32;

  AReader.BaseStream.Seek(DrawingObjectDataOffsetPosition, TSeekOrigin.soBeginning);
  DrawingObjectTableOffset := AReader.ReadInt32;
  DrawingObjectTableSize := AReader.ReadInt32;
end;

procedure TdxFileInformationBlock.ReadDocumentVariablesInfo(AReader: TBinaryReader);
begin
  AReader.BaseStream.Seek(DocumentVariablesOffsetPosition, TSeekOrigin.soBeginning);
  DocumentVariablesOffset := AReader.ReadInt32;
  DocumentVariablesSize := AReader.ReadInt32;
end;

procedure TdxFileInformationBlock.ReadDocumentPropertiesInfo(AReader: TBinaryReader);
begin
  AReader.BaseStream.Seek(DocumentPropertiesOffsetPosition, TSeekOrigin.soBeginning);
  DocumentPropertiesOffset := AReader.ReadInt32;
  DocumentPropertiesSize := AReader.ReadInt32;
end;

procedure TdxFileInformationBlock.ReadRangeEditPermissionsInfo(AReader: TBinaryReader);
begin
  AReader.BaseStream.Seek(RangeEditPermissionsInformationOffsetPosition, TSeekOrigin.soBeginning);
  RangeEditPermissionsInformationOffset := AReader.ReadInt32;
  RangeEditPermissionsInformationSize := AReader.ReadInt32;
  RangeEditPermissionsStartInfoOffset := AReader.ReadInt32;
  RangeEditPermissionsStartInfoSize := AReader.ReadInt32;
  RangeEditPermissionsEndInfoOffset := AReader.ReadInt32;
  RangeEditPermissionsEndInfoSize := AReader.ReadInt32;
  RangeEditPermissionsUsernamesOffset := AReader.ReadInt32;
  RangeEditPermissionsUsernamesSize := AReader.ReadInt32;
end;

procedure TdxFileInformationBlock.ReadRmdThreading(AReader: TBinaryReader);
begin
  AReader.BaseStream.Seek(RmdThreadingPosition, TSeekOrigin.soBeginning);
  RmdThreadingOffset := AReader.ReadInt32;
  RmdThreadingSize := AReader.ReadInt32;
end;

procedure TdxFileInformationBlock.ReadDocumentFileRecords(AReader: TBinaryReader);
begin
  AReader.BaseStream.Seek(DocumentFileRecordsPosition, TSeekOrigin.soBeginning);
  DocumentFileRecordsOffset := AReader.ReadInt32;
  DocumentFileRecordsSize := AReader.ReadInt32;
end;

procedure TdxFileInformationBlock.Write(AWriter: TBinaryWriter);
begin
  Assert(AWriter <> nil, 'writer');
  WriteFormatSpecificInfo(AWriter);
  WriteFormattedDiskPagesInfo(AWriter);
  WriteComplexFileInfo(AWriter);
  WriteDocumentStructureInfo(AWriter);
  WriteInsufficientMemoryInfo(AWriter);

  WriteReferenceInfo(AWriter);
  WritePieceTableInfo(AWriter);
  WriteHeadersFootersInfo(AWriter);

  WriteFontsInfo(AWriter);
  WriteStyleSheetInfo(AWriter);
  WriteFieldsInfo(AWriter);
  WriteListInfo(AWriter);
  WriteBookmarksInfo(AWriter);
  WriteShapesAndDrawingObjectsInfo(AWriter);

  WriteDocumentVariablesInfo(AWriter);
  WriteDocumentPropertiesInfo(AWriter);
  WriteRangeEditPermissionsInfo(AWriter);
  WriteRmdThreading(AWriter);
end;

procedure TdxFileInformationBlock.WriteFormatSpecificInfo(AWriter: TBinaryWriter);
begin
  AWriter.BaseStream.Seek(0, TSeekOrigin.soBeginning);
  AWriter.Write(WordBinaryFileSignature);
  AWriter.Write(FibVersion97);
  AWriter.Write(Product);
  AWriter.Write(Lid);

  AWriter.BaseStream.Seek(FlagsPosition, TSeekOrigin.soBeginning);
  AWriter.Write(TdxFileInformationFlagValues.ToValue(Flags));
  AWriter.Write(FibVersionBackCompatible);

  AWriter.BaseStream.Seek(Word97SavedFlafPosition, TSeekOrigin.soBeginning);
  AWriter.Write(Word97SavedFlag);

  AWriter.BaseStream.Seek(CswPosition, TSeekOrigin.soBeginning);
  AWriter.Write(Csw);
  AWriter.BaseStream.Seek(LidFEposition, TSeekOrigin.soBeginning);
  AWriter.Write(LidFE);
  AWriter.Write(Cslw);
  AWriter.BaseStream.Seek(CbRgFcLcbPosition, TSeekOrigin.soBeginning);
  AWriter.Write(CbRgFcLcb);

  AWriter.BaseStream.Seek(MagicCreatedPosition, TSeekOrigin.soBeginning);
  AWriter.Write(MagicCreated);
  AWriter.Write(MagicCreated);
  AWriter.Write(MagicCreatedPrivate);
  AWriter.Write(MagicCreatedPrivate);

  AWriter.BaseStream.Seek(CswNewPosition, TSeekOrigin.soBeginning);
  AWriter.Write(CswNew);
  AWriter.Write(FibVersion2003);
  AWriter.Write(Word(0));
end;

procedure TdxFileInformationBlock.WriteReferenceInfo(AWriter: TBinaryWriter);
begin
  AWriter.BaseStream.Seek(FootnotesReferenceOffsetPosition, TSeekOrigin.soBeginning);
  AWriter.Write(FootNotesReferenceOffset);
  AWriter.Write(Cardinal(FootNotesReferenceSize));
  AWriter.Write(FootNotesTextOffset);
  AWriter.Write(Cardinal(FootNotesTextSize));
  AWriter.Write(CommentsReferenceOffset);
  AWriter.Write(Cardinal(CommentsReferenceSize));
  AWriter.Write(CommentsTextOffset);
  AWriter.Write(Cardinal(CommentsTextSize));
  AWriter.BaseStream.Seek(CommentsReferenceOffsetAuthor, TSeekOrigin.soBeginning);
  AWriter.Write(CommentsAuthorTableOffset);
  AWriter.Write(Cardinal(CommentsAuthorTableSize));
  AWriter.Write(CommentsNameTableOffset);
  AWriter.Write(Cardinal(CommentsNameTableSize));
  AWriter.BaseStream.Seek(CommentsReferenceOffsetPosition, TSeekOrigin.soBeginning);
  AWriter.Write(CommentsFirstTableOffset);
  AWriter.Write(Cardinal(CommentsFirstTableSize));
  AWriter.Write(CommentsLimTableOffset);
  AWriter.Write(Cardinal(CommentsLimTableSize));
  AWriter.BaseStream.Seek(EndnotesReferenceOffsetPosition, TSeekOrigin.soBeginning);
  AWriter.Write(EndNotesReferenceOffset);
  AWriter.Write(Cardinal(EndNotesReferenceSize));
  AWriter.Write(EndnotesTextOffset);
  AWriter.Write(Cardinal(EndnotesTextSize));
  AWriter.BaseStream.Seek(MainDocumentTextBoxesTextOffsetPosition, TSeekOrigin.soBeginning);
  AWriter.Write(MainDocumentTextBoxesTextOffset);
  AWriter.Write(Cardinal(MainDocumentTextBoxesTextSize));
  AWriter.BaseStream.Seek(HeaderTextBoxesTextBoxesTextOffsetPosition, TSeekOrigin.soBeginning);
  AWriter.Write(HeaderTextBoxesTextOffset);
  AWriter.Write(Cardinal(HeaderTextBoxesTextSize));
end;

procedure TdxFileInformationBlock.WriteHeadersFootersInfo(AWriter: TBinaryWriter);
begin
  AWriter.BaseStream.Seek(HeadersFootersPositionsOffsetPosition, TSeekOrigin.soBeginning);
  AWriter.Write(HeadersFootersPositionsOffset);
  AWriter.Write(Cardinal(HeadersFootersPositionsSize));
end;

procedure TdxFileInformationBlock.WriteFormattedDiskPagesInfo(AWriter: TBinaryWriter);
begin
  AWriter.BaseStream.Seek(FirstCharacterFileOffsetPosition, TSeekOrigin.soBeginning);
  AWriter.Write(FirstCharacterFileOffset);
  AWriter.Write(LastCharacterFileOffset);
  AWriter.BaseStream.Seek(LastByteFileOffsetPosition, TSeekOrigin.soBeginning);
  AWriter.Write(LastByteFileOffset);
  AWriter.Write(RevisedDate);
  AWriter.Write(RevisedDate);

  AWriter.BaseStream.Seek(FirstCHPXFormattedDiskPageNumberPosition, TSeekOrigin.soBeginning);
  AWriter.Write(FirstCHPXFormattedDiskPageNumber);
  AWriter.Write(CharactersFormattedDiskPageCount);
  AWriter.BaseStream.Seek(FirstPAPXFormattedDiskPageNumberPosition, TSeekOrigin.soBeginning);
  AWriter.Write(FirstPAPXFormattedDiskPageNumber);
  AWriter.Write(ParagraphsFormattedDiskPageCount);
end;

procedure TdxFileInformationBlock.WritePieceTableInfo(AWriter: TBinaryWriter);
begin
  AWriter.BaseStream.Seek(CharacterTableOffsetPosition, TSeekOrigin.soBeginning);
  AWriter.Write(CharacterTableOffset);
  AWriter.Write(Cardinal(CharacterTableSize));
  AWriter.BaseStream.Seek(ParagraphTableOffsetPosition, TSeekOrigin.soBeginning);
  AWriter.Write(ParagraphTableOffset);
  AWriter.Write(Cardinal(ParagraphTableSize));
  AWriter.BaseStream.Seek(SectionTableOffsetPosition, TSeekOrigin.soBeginning);
  AWriter.Write(SectionTableOffset);
  AWriter.Write(Cardinal(SectionTableSize));
  AWriter.BaseStream.Seek(ParagraphHeightsPosition, TSeekOrigin.soBeginning);
  AWriter.Write(ParagraphHeightsOffset);
  AWriter.Write(Cardinal(ParagraphHeightsSize));
end;

procedure TdxFileInformationBlock.WriteDocumentStructureInfo(AWriter: TBinaryWriter);
begin
  AWriter.BaseStream.Seek(MainDocumentLengthPosition, TSeekOrigin.soBeginning);
  AWriter.Write(MainDocumentLength);
  AWriter.Write(FootNotesLength);
  AWriter.Write(HeadersFootersLength);
  AWriter.Write(MacroLength);
  AWriter.Write(CommentsLength);
  AWriter.Write(EndNotesLength);
  AWriter.Write(MainDocumentTextBoxesLength);
  AWriter.Write(HeaderTextBoxesLength);
end;

procedure TdxFileInformationBlock.WriteInsufficientMemoryInfo(AWriter: TBinaryWriter);
begin
  AWriter.BaseStream.Seek(InsufficientMemoryCHPXPageNumberPosition, TSeekOrigin.soBeginning);
  AWriter.Write(InsufficientMemoryPageNumber);
  AWriter.BaseStream.Seek(InsufficientMemoryPAPXPageNumberPosition, TSeekOrigin.soBeginning);
  AWriter.Write(InsufficientMemoryPageNumber);
  AWriter.BaseStream.Seek(InsufficientMemoryLVCPageNumberPosition, TSeekOrigin.soBeginning);
  AWriter.Write(InsufficientMemoryPageNumber);
end;

procedure TdxFileInformationBlock.WriteStyleSheetInfo(AWriter: TBinaryWriter);
begin
  AWriter.BaseStream.Seek(StyleSheetOffsetOriginalPosition, TSeekOrigin.soBeginning);
  AWriter.Write(StyleSheetOffset);
  AWriter.Write(Cardinal(StyleSheetSize));
  AWriter.Write(StyleSheetOffset);
  AWriter.Write(Cardinal(StyleSheetSize));
end;

procedure TdxFileInformationBlock.WriteFieldsInfo(AWriter: TBinaryWriter);
begin
  AWriter.BaseStream.Seek(MainDocumentFieldTablePosition, TSeekOrigin.soBeginning);
  AWriter.Write(MainDocumentFieldTableOffset);
  AWriter.Write(Cardinal(MainDocumentFieldTableSize));
  AWriter.Write(HeadersFootersFieldTableOffset);
  AWriter.Write(Cardinal(HeadersFootersFieldTableSize));
  AWriter.Write(FootNotesFieldTableOffset);
  AWriter.Write(Cardinal(FootNotesFieldTableSize));
  AWriter.Write(CommentsFieldTableOffset);
  AWriter.Write(Cardinal(CommentsFieldTableSize));
  AWriter.BaseStream.Seek(EndNotesFieldTablePosition, TSeekOrigin.soBeginning);
  AWriter.Write(EndNotesFieldTableOffset);
  AWriter.Write(Cardinal(EndNotesFieldTableSize));
  AWriter.BaseStream.Seek(MainDocumentTextBoxesFieldTablePosition, TSeekOrigin.soBeginning);
  AWriter.Write(MainDocumentTextBoxesFieldTableOffset);
  AWriter.Write(Cardinal(MainDocumentTextBoxesFieldTableSize));
  AWriter.BaseStream.Seek(HeaderTextBoxesFieldTablePosition, TSeekOrigin.soBeginning);
  AWriter.Write(HeaderTextBoxesFieldTableOffset);
  AWriter.Write(Cardinal(HeaderTextBoxesFieldTableSize));
end;

procedure TdxFileInformationBlock.WriteFontsInfo(AWriter: TBinaryWriter);
begin
  AWriter.BaseStream.Seek(FontTableOffsetPosition, TSeekOrigin.soBeginning);
  AWriter.Write(FontTableOffset);
  AWriter.Write(Cardinal(FontTableSize));
end;

procedure TdxFileInformationBlock.WriteBookmarksInfo(AWriter: TBinaryWriter);
begin
  AWriter.BaseStream.Seek(BookmarkInfoPosition, TSeekOrigin.soBeginning);
  AWriter.Write(BookmarkNamesTableOffset);
  AWriter.Write(Cardinal(BookmarkNamesTableSize));
  AWriter.Write(BookmarkStartInfoOffset);
  AWriter.Write(Cardinal(BookmarkStartInfoSize));
  AWriter.Write(BookmarkEndInfoOffset);
  AWriter.Write(Cardinal(BookmarkEndInfoSize));
end;

procedure TdxFileInformationBlock.WriteComplexFileInfo(AWriter: TBinaryWriter);
begin
  AWriter.BaseStream.Seek(ComplexFileInformationOffsetPosition, TSeekOrigin.soBeginning);
  AWriter.Write(ComplexFileInformationOffset);
  AWriter.Write(Cardinal(ComplexFileInformationSize));
end;

procedure TdxFileInformationBlock.WriteShapesAndDrawingObjectsInfo(AWriter: TBinaryWriter);
begin
  AWriter.BaseStream.Seek(MainDocumentFileShapeAddressesOffsetPosition, TSeekOrigin.soBeginning);
  AWriter.Write(MainDocumentFileShapeTableOffset);
  AWriter.Write(Cardinal(MainDocumentFileShapeTableSize));
  AWriter.BaseStream.Seek(HeadersDocumentFileShapeAddressesOffsetPosition, TSeekOrigin.soBeginning);
  AWriter.Write(HeadersFootersFileShapeTableOffset);
  AWriter.Write(Cardinal(HeadersFootersFileShapeTableSize));

  AWriter.BaseStream.Seek(DrawingObjectDataOffsetPosition, TSeekOrigin.soBeginning);
  AWriter.Write(DrawingObjectTableOffset);
  AWriter.Write(Cardinal(DrawingObjectTableSize));
end;

procedure TdxFileInformationBlock.WriteListInfo(AWriter: TBinaryWriter);
begin
  if DocumentFileRecordsSize <> 0 then
  begin
    AWriter.BaseStream.Seek(DocumentFileRecordsPosition, TSeekOrigin.soBeginning);
    AWriter.Write(DocumentFileRecordsOffset);
    AWriter.Write(Cardinal(DocumentFileRecordsSize));
  end;
  AWriter.BaseStream.Seek(ListFormatInformationOffsetPosition, TSeekOrigin.soBeginning);
  AWriter.Write(ListFormatInformationOffset);
  AWriter.Write(Cardinal(ListFormatInformationSize));
  AWriter.BaseStream.Seek(ListFormatOverrideInformationOffsetPosition, TSeekOrigin.soBeginning);
  AWriter.Write(ListFormatOverrideInformationOffset);
  AWriter.Write(Cardinal(ListFormatOverrideInformationSize));
  AWriter.Write(MainTextBoxBreakTableOffset);
  AWriter.Write(Cardinal(MainTextBoxBreakTableSize));
  AWriter.Write(HeadersFootersTextBoxBreakTableOffset);
  AWriter.Write(Cardinal(HeadersFootersTextBoxBreakTableSize));
end;

procedure TdxFileInformationBlock.WriteDocumentVariablesInfo(AWriter: TBinaryWriter);
begin
  AWriter.BaseStream.Seek(DocumentVariablesOffsetPosition, TSeekOrigin.soBeginning);
  AWriter.Write(DocumentVariablesOffset);
  AWriter.Write(DocumentVariablesSize);
end;

procedure TdxFileInformationBlock.WriteDocumentPropertiesInfo(AWriter: TBinaryWriter);
begin
  AWriter.BaseStream.Seek(DocumentPropertiesOffsetPosition, TSeekOrigin.soBeginning);
  AWriter.Write(DocumentPropertiesOffset);
  AWriter.Write(Cardinal(DocumentPropertiesSize));
end;

procedure TdxFileInformationBlock.WriteRangeEditPermissionsInfo(AWriter: TBinaryWriter);
begin
  AWriter.BaseStream.Seek(RangeEditPermissionsInformationOffsetPosition, TSeekOrigin.soBeginning);
  AWriter.Write(RangeEditPermissionsInformationOffset);
  AWriter.Write(Cardinal(RangeEditPermissionsInformationSize));
  AWriter.Write(RangeEditPermissionsStartInfoOffset);
  AWriter.Write(Cardinal(RangeEditPermissionsStartInfoSize));
  AWriter.Write(RangeEditPermissionsEndInfoOffset);
  AWriter.Write(Cardinal(RangeEditPermissionsEndInfoSize));
  AWriter.Write(RangeEditPermissionsUsernamesOffset);
  AWriter.Write(Cardinal(RangeEditPermissionsUsernamesSize));
end;

procedure TdxFileInformationBlock.WriteRmdThreading(AWriter: TBinaryWriter);
begin
  AWriter.BaseStream.Seek(RmdThreadingPosition, TSeekOrigin.soBeginning);
  AWriter.Write(RmdThreadingOffset);
  AWriter.Write(Cardinal(RmdThreadingSize));
end;

end.
