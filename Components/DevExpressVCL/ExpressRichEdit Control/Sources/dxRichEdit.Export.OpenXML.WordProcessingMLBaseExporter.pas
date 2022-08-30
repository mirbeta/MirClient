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

unit dxRichEdit.Export.OpenXML.WordProcessingMLBaseExporter;

{$I cxVer.inc}
{$I dxRichEditControl.inc}


interface

uses
  Types, Classes, SysUtils, Graphics, Generics.Defaults, Generics.Collections,
  dxCore, dxCoreClasses, dxCoreGraphics, dxZIPUtils,

  dxRichEdit.NativeApi,
  dxRichEdit.Utils.Types,
  dxGenerics,
  dxRichEdit.Utils.OfficeImage,
  dxXMLWriter,
  dxRichEdit.Platform.Font,
  dxRichEdit.Export.Core,
  dxRichEdit.Options,
  dxRichEdit.DocumentModel.UnitConverter,
  dxRichEdit.DocumentModel.Borders,
  dxRichEdit.DocumentModel.Bookmarks,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Exporter,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.Numbering,
  dxRichEdit.DocumentModel.NumberingFormatting,
  dxRichEdit.DocumentModel.Notes,
  dxRichEdit.DocumentModel.NotesRange,
  dxRichEdit.DocumentModel.Section,
  dxRichEdit.DocumentModel.SectionFormatting,
  dxRichEdit.DocumentModel.FrameFormatting,
  dxRichEdit.DocumentModel.Styles.Core,
  dxRichEdit.DocumentModel.Styles,
  dxRichEdit.DocumentModel.CharacterFormatting,
  dxRichEdit.DocumentModel.ParagraphFormatting,
  dxRichEdit.DocumentModel.TabFormatting,
  dxRichEdit.DocumentModel.Tables,
  dxRichEdit.DocumentModel.TableFormatting,
  dxRichEdit.DocumentModel.TableStyles,
  dxRichEdit.DocumentModel.TextRange,
  dxRichEdit.DocumentModel.InlineObjectRange,
  dxRichEdit.DocumentModel.FloatingObjectRange,
  dxRichEdit.DocumentModel.FloatingObjectFormatting,
  dxRichEdit.DocumentModel.ShapeFormatting,
  dxRichEdit.DocumentModel.FieldRange,
  dxRichEdit.DocumentModel.DocumentProperties,
  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.Import,
  dxRichEdit.DocumentFormats.DocumentFormatUtils;

type

  TdxInternalZipArchive = class(TdxZIPStreamWriter);

  { TdxXmlBasedDocumentModelExporter }

  TdxXMLWriteAction = reference to procedure (AWriter: TdxXmlWriter);

  TdxXmlBasedDocumentModelExporter = class abstract(TdxDocumentModelExporter)
  strict private
    FNow: TDateTime;
  protected
    function GetPackage: TdxInternalZipArchive; virtual; abstract;
    function CreateXmlWriterSettings: TdxXmlWriterSettings; virtual;
    procedure BeforeCreateXmlContent(AWriter: TdxXmlWriter); virtual;
    function CreateXmlContent(const AAction: TdxXMLWriteAction): TStream; virtual;
    function CreateCompressedXmlContent(const AAction: TdxXMLWriteAction): TdxCompressedStream; overload; virtual;
    function CreateCompressedXmlContent(const AAction: TdxXMLWriteAction; AEncoding: TEncoding): TdxCompressedStream; overload; virtual;
    procedure AddPackageContent(const AFileName: string; AContent: TStream); overload; virtual;
    procedure AddPackageContent(const AFileName: string; const AContent: TArray<Byte>); overload; virtual;
    procedure AddCompressedPackageContent(const AFileName: string; AContent: TdxCompressedStream); virtual;
    procedure AddPackageImage(const AFileName: string; AImage: TdxOfficeImage); virtual;
    function GetImageExtension(AImage: TdxOfficeImage): string; virtual;
    function GetImageBytesStream(AImage: TdxOfficeImage): TStream; virtual;
    function GetImageBytes(AImage: TdxOfficeImage): TArray<Byte>; virtual;

    property Package: TdxInternalZipArchive read GetPackage;
    property Now: TDateTime read FNow;
  public
    constructor Create(ADocumentModel: TdxCustomDocumentModel; const AOptions: IdxExporterOptions); override;
  end;

  { TdxWordProcessingMLValue }

  TdxWordProcessingMLValue = record
  strict private
    FOpenXmlValue: string;
    FValuesAreEqual: Boolean;
    FWordMLValue: string;
    class function ConvertToWordML(const AOpenXmlValue: string): string; static;
  public
    constructor Create(const AOpenXmlValue: string); overload;
    constructor Create(const AOpenXmlValue: string; const AWordMLValue: string); overload;

    property OpenXmlValue: string read FOpenXmlValue;
    property ValuesAreEqual: Boolean read FValuesAreEqual;
    property WordMLValue: string read FWordMLValue;
  end;

  TdxMLDictionary<TEnumeration> = class(TdxEnumeratedDictionary<TEnumeration, TdxWordProcessingMLValue>)
  public
    procedure AddValue(AKey: TEnumeration; const AValue: string);
    function GetKeyByStringDef(const AValue: string; ADefaultKey: TEnumeration): TEnumeration;
    function GetKeyByOpenXmlStringDef(const AValue: string; ADefaultKey: TEnumeration): TEnumeration;
    function GetKeyByWordMLStringDef(const AValue: string; ADefaultKey: TEnumeration): TEnumeration;
    function TryGetKeyByOpenXmlString(const AValue: string; out AKey: TEnumeration): Boolean;
    function TryGetKeyByWordMLString(const AValue: string; out AKey: TEnumeration): Boolean;
  end;


  TdxWordProcessingMLBaseExporter = class abstract (TdxXmlBasedDocumentModelExporter)
  public const
    OfficeNamespaceConst = 'urn:schemas-microsoft-com:office:office';
    W10MLNamespace = 'urn:schemas-microsoft-com:office:word';
    W10MLPrefix = 'w10';
    OPrefix = 'o';
    TransparentClr = 16777215;
    VMLPrefix = 'v';
    VMLNamespace = 'urn:schemas-microsoft-com:vml';
  strict private class var
    FPredefinedBackgroundColors: TDictionary<TdxAlphaColor, TdxWordProcessingMLValue>;
    FPresetColors: TdxStringColorDictionary;
    FUnderlineTable: TdxMLDictionary<TdxUnderlineType>;
    FRunBreaksTable: TDictionary<Char, TdxWordProcessingMLValue>;
    FParagraphAlignmentTable: TdxMLDictionary<TdxParagraphAlignment>;
    FLineSpacingTable: TdxMLDictionary<TdxParagraphLineSpacing>;
    FTabAlignmentTable: TdxMLDictionary<TdxTabAlignmentType>;
    FTabLeaderTable: TdxMLDictionary<TdxTabLeaderType>;
    FTextDirectionTable: TdxMLDictionary<TdxTextDirection>;
    FShadingPatternTable: TdxMLDictionary<TdxShadingPattern>;
    FVerticalAlignmentTable: TdxMLDictionary<TdxVerticalAlignment>;
    FSectionStartTypeTable: TdxMLDictionary<TdxSectionStartType>;
    FLineNumberingRestartTable: TdxMLDictionary<TdxLineNumberingRestart>;
    FPageNumberingFormatTable: TdxMLDictionary<TdxNumberingFormat>;
    FChapterSeparatorsTable: TDictionary<Char, TdxWordProcessingMLValue>;
    FNumberingListTypeTable: TdxMLDictionary<TdxNumberingType>;
    FListNumberAlignmentTable: TdxMLDictionary<TdxListNumberAlignment>;
    FListNumberSeparatorTable: TDictionary<Char, TdxWordProcessingMLValue>;
    FContentTypeTable: TdxStringsDictionary;
    FWidthUnitTypesTable: TdxMLDictionary<TdxWidthUnitType>;
    FTableLayoutTypeTable: TdxMLDictionary<TdxTableLayoutType>;
    FBorderLineStyleTable: TdxMLDictionary<TdxBorderLineStyle>;
    FHorizontalAlignModeTable: TdxMLDictionary<TdxHorizontalAlignMode>;
    FHorizontalAnchorTypesTable: TdxMLDictionary<TdxHorizontalAnchorTypes>;
    FVerticalAlignModeTable: TdxMLDictionary<TdxVerticalAlignMode>;
    FVerticalAnchorTypesTable: TdxMLDictionary<TdxVerticalAnchorTypes>;
    FHeightUnitTypeTable: TdxMLDictionary<TdxHeightUnitType>;
    FMergingStateTable: TdxMLDictionary<TdxMergingState>;
    FTableRowAlignmentTable: TdxMLDictionary<TdxTableRowAlignment>;
    FFootNotePlacementTable: TdxMLDictionary<TdxFootNotePosition>;
    FFloatingObjectHorizontalPositionAlignmentTable: TdxMLDictionary<TdxFloatingObjectHorizontalPositionAlignment>;
    FFloatingObjectVerticalPositionAlignmentTable: TdxMLDictionary<TdxFloatingObjectVerticalPositionAlignment>;
    FFloatingObjectHorizontalPositionTypeTable: TdxMLDictionary<TdxFloatingObjectHorizontalPositionType>;
    FFloatingObjectCssRelativeFromHorizontalTable: TdxMLDictionary<TdxFloatingObjectRelativeFromHorizontal>;
    FFloatingObjectCssRelativeFromVerticalTable: TdxMLDictionary<TdxFloatingObjectRelativeFromVertical>;
    FFloatingObjectRelativeFromHorizontalTable: TdxMLDictionary<TdxFloatingObjectRelativeFromHorizontal>;
    FFloatingObjectRelativeFromVerticalTable: TdxMLDictionary<TdxFloatingObjectRelativeFromVertical>;
    FFloatingObjectVerticalPositionTypeTable: TdxMLDictionary<TdxFloatingObjectVerticalPositionType>;
    FFloatingObjectTextWrapSideTable: TdxMLDictionary<TdxFloatingObjectTextWrapSide>;
    FFloatingObjectTextWrapTypeTable: TdxMLDictionary<TdxFloatingObjectTextWrapType>;
    FTextBoxVerticalAlignmentTable: TdxMLDictionary<TdxVerticalAlignment>;
    FConditionalTableStyleFormattingTypesTable: TdxMLDictionary<TdxConditionalTableStyleFormattingType>;
    FPredefinedGroupNames: TdxStringsDictionary;
  strict private
    class constructor Initialize;
    class destructor Finalize;
  strict private
    FOptions: TdxDocumentExporterOptions;
    FDocumentContentWriter: TdxXmlWriter;
    FCurrentSection: TdxSection;
    FImageCounter: Integer;
    FForbidExecuteBaseExportTableCell: Boolean;
    function GetUnitConverter: TdxDocumentModelUnitConverter;
  protected const
    OfficeNamespace = 'urn:schemas-microsoft-com:office:office';
    OfficePrefix = 'o';
  protected
    FDrawingElementId: Integer;
    FFieldCodeDepth: Integer;
    FRangePermissionCounter: Integer;
    FRangePermissionIdMap: TDictionary<TdxRangePermission, string>;
    function AllowExportTableCellProperties(ACell: TdxTableCell): Boolean; virtual;
    function ConvertAlignment(AAlignment: TdxParagraphAlignment): string; virtual;
    function ConvertBackColorToString(AValue: TdxAlphaColor): string; virtual;
    function ConvertBoolToString(AValue: Boolean): string; virtual; abstract;
    function ConvertBoolValueToString(AValue: Boolean): string;
    function ConvertBorderLineStyle(AValue: TdxBorderLineStyle): string;
    function ConvertChapterSeparator(AValue: Char): string;
    function ConvertColorToString(AValue: TdxAlphaColor): string; virtual;
    function ConvertConditionalStyleType(AStyleType: TdxConditionalTableStyleFormattingType): string;
    function ConvertFootNotePlacement(AValue: TdxFootNotePosition): string;
    function ConvertFormatString(const AValue: string): string;
    function ConvertHeightUnitType(AValue: TdxHeightUnitType): string;
    function ConvertHorizontalAlignMode(AValue: TdxHorizontalAlignMode): string;
    function ConvertHorizontalAnchorTypes(AValue: TdxHorizontalAnchorTypes): string;
    function ConvertHorizontalPositionAlignment(AHorizontalPositionAlignment: TdxFloatingObjectHorizontalPositionAlignment): string; overload; virtual;
    function ConvertHorizontalPositionAlignment(AHorizontalPositionAlignment: TdxParagraphFrameHorizontalPositionAlignment): string; overload; virtual;
    function ConvertHorizontalPositionType(AHorizontalPositionType: TdxFloatingObjectHorizontalPositionType): string; overload; virtual;
    function ConvertHorizontalPositionType(AHorizontalPositionType: TdxParagraphFrameHorizontalPositionType): string; overload; virtual;
    function ConvertHorizontalRule(AHorizontalRule: TdxParagraphFrameHorizontalRule): string; virtual;
    function ConvertLineNumberingRestartType(AValue: TdxLineNumberingRestart): string;
    function ConvertLineSpacing(ALineSpacing: TdxParagraphLineSpacing): string; virtual;
    function ConvertLineSpacingValue(ALineSpacing: TdxParagraphLineSpacing; AValue: Single): Integer; virtual;
    function ConvertListNumberAlignment(AValue: TdxListNumberAlignment): string;
    function ConvertMergingState(AValue: TdxMergingState): string;
    function ConvertNumberFormat(AValue: TdxNumberingFormat): string;
    function ConvertNumberingListType(AValue: TdxNumberingType): string;
    function ConvertNumberingSeparator(AValue: Char): string;
    function ConvertScript(AScript: TdxCharacterFormattingScript): string; virtual;
    function ConvertSectionStartType(AValue: TdxSectionStartType): string;
    function ConvertShadingPattern(AValue: TdxShadingPattern): string;
    function ConvertTabAlignment(AAlignment: TdxTabAlignmentType): string; virtual;
    function ConvertTabLeader(ALeaderType: TdxTabLeaderType): string; virtual;
    function ConvertTableLayoutType(AValue: TdxTableLayoutType): string;
    function ConvertTableRowAlignment(AValue: TdxTableRowAlignment): string;
    function ConvertTextDirection(AValue: TdxTextDirection): string;
    function ConvertToHexBinary(ANumber: Integer): string;
    function ConvertToHexString(AValue: Integer): string; virtual;
    function ConvertUnderlineType(AUnderline: TdxUnderlineType): string; virtual;
    function ConvertVerticalAlignment(AValue: TdxVerticalAlignment): string;
    function ConvertVerticalAlignMode(AValue: TdxVerticalAlignMode): string;
    function ConvertVerticalAnchorTypes(AValue: TdxVerticalAnchorTypes): string;
    function ConvertVerticalPositionAlignment(AVerticalPositionAlignment: TdxFloatingObjectVerticalPositionAlignment): string; overload; virtual;
    function ConvertVerticalPositionAlignment(AVerticalPositionAlignment: TdxParagraphFrameVerticalPositionAlignment): string; overload; virtual;
    function ConvertVerticalPositionType(AVerticalPositionType: TdxFloatingObjectVerticalPositionType): string; overload; virtual;
    function ConvertVerticalPositionType(AVerticalPositionType: TdxParagraphFrameVerticalPositionType): string; overload; virtual;
    function ConvertWidthUnitTypes(AValue: TdxWidthUnitType): string;
    function ConvertWrapType(AWrapType: TdxFloatingObjectTextWrapType): string; overload; virtual;
    function ConvertWrapType(AWrapType: TdxParagraphFrameTextWrapType): string; overload; virtual;
    function EncodeVariableValue(const AValue: string): string;
    function ExportBinData(AImage: TdxOfficeImage): string; virtual;
    function ExportImageStyle(AFloatingObjectProperties: TdxFloatingObjectProperties; ATextBoxContent: TdxTextBoxFloatingObjectContent; AShape: TdxShape): string;
    function ExportParagraph(AParagraph: TdxParagraph): TdxParagraphIndex; override;
    function ExportTable(ATableInfo: TdxTableInfo): TdxParagraphIndex; override;
    function ForbidExportWidthUnit(AWidthUnit: TdxWidthUnit): Boolean; virtual;
    function GenerateFloatingObjectName(const AName: string; const ADefaultNamePrefix: string; AId: Integer): string;
    function GenerateImageId: string; virtual;
    function GenerateImageRelationId(const AImageId: string): string; virtual;
    function GetBoolValueAsString(AValue: Boolean): string;
    function GetCharacterStyleId(AStyleIndex: Integer): string;
    function GetExportedImageTable: TDictionary<TdxOfficeImage, string>; virtual; abstract;
    function GetGroupName(const AGroupName: string): string;
    function GetHorizontalPositionAlignment(AFloatingObjectHorizontalPositionAlignment: TdxFloatingObjectHorizontalPositionAlignment): string;
    function GetHorizontalPositionType(AFloatingObjectHorizontalPositionType: TdxFloatingObjectHorizontalPositionType): string;
    function GetHorizontalPositionTypeAttributeTable: TdxMLDictionary<TdxFloatingObjectHorizontalPositionType>; virtual; abstract;
    function GetImagePath(AImage: TdxOfficeImage): string; overload; virtual;
    function GetNumberingListIndexForExport(ANumberingListIndex: TdxNumberingListIndex): Integer;
    function GetNumberingStyleId(AStyleIndex: Integer): string;
    function GetPackage: TdxInternalZipArchive; override;
    function GetParagraphStyleId(AStyleIndex: Integer): string;
    function GetRelativeHeight(const ARelativeHeight: TdxFloatingObjectRelativeHeight): string;
    function GetRelativeSizeFromHorizontal(const ARelativeWidth: TdxFloatingObjectRelativeWidth): string;
    function GetRelativeSizeFromVertical(const ARelativeHeight: TdxFloatingObjectRelativeHeight): string;
    function GetRelativeWidth(const ARelativeWidth: TdxFloatingObjectRelativeWidth): string;
    function GetTableCellStyleId(AStyleIndex: Integer): string;
    function GetTableStyleId(AStyleIndex: Integer): string;
    function GetVerticalAlignment(AVerticalAlignment: TdxVerticalAlignment): string;
    function GetVerticalPositionAlignment(AFloatingObjectVerticalPositionAlignment: TdxFloatingObjectVerticalPositionAlignment): string;
    function GetVerticalPositionType(AFloatingObjectVerticalPositionType: TdxFloatingObjectVerticalPositionType): string;
    function GetVerticalPositionTypeAttributeTable: TdxMLDictionary<TdxFloatingObjectVerticalPositionType>; virtual; abstract;
    function GetWordProcessingMLValue(AValue: TdxWordProcessingMLValue): string; overload; virtual; abstract;
    function GetWordProcessingMLValue(const AOpenXmlValue: string): string; overload; virtual;
    function GetWordProcessingNamespace: string; virtual; abstract;
    function GetWordProcessingPrefix: string; virtual; abstract;
    function GetWrapText(AWrapText: Boolean): string;
    function PrepareFontName(const AName: string): string; virtual;
    function ShouldExportCellMargins(ACellMargins: TdxCellMargins): Boolean; virtual;
    function ShouldExportFloatingPosition(AFloatingPosition: TdxTableFloatingPosition): Boolean; virtual;
    function ShouldExportLineNumbering(ANumbering: TdxSectionLineNumbering): Boolean; virtual;
    function ShouldExportPageNumbering(ANumbering: TdxSectionPageNumbering): Boolean; virtual;
    function ShouldExportPaperSource(ASettings: TdxSectionGeneralSettings): Boolean; virtual;
    function ShouldExportParagraphBorder(ADefaultParagraphProperties: Boolean; ABorderInfo: TdxBorderInfo): Boolean;
    function ShouldExportParagraphNumbering(ANumberingListIndex: TdxNumberingListIndex): Boolean; virtual;
    function ShouldExportParagraphProperties(AParagraph: TdxParagraph): Boolean; virtual;
    function ShouldExportRunProperties(ARun: TdxTextRunBase): Boolean; virtual;
    function ShouldExportSectionEndNote(ANote: TdxSectionFootNote): Boolean; virtual;
    function ShouldExportSectionFootNote(ANote: TdxSectionFootNote): Boolean; virtual;
    function ShouldExportSectionMargins(AMargins: TdxSectionMargins): Boolean; virtual;
    function ShouldExportSectionPage(APage: TdxSectionPage): Boolean; virtual;
    function ShouldExportSectionProperties(AParagraph: TdxParagraph): Boolean; virtual;
    function ShouldExportTableBorders(ABorders: TdxTableBorders): Boolean; virtual;
    function ShouldExportTabProperties(ATabs: TdxTabFormattingInfo): Boolean; virtual;
    function ShouldPreserveSpace(const AText: string): Boolean; virtual;
    function WriteFloatingObjectShapeType: Integer;
    procedure ExportAbstractLevelProperties(ALevel: TdxListLevel);
    procedure ExportAbstractNumberingList(AList: TdxAbstractNumberingList; AId: Integer); virtual; abstract;
    procedure ExportAbstractNumberingLists; virtual;
    procedure ExportBreak(const AValue: string); virtual;
    procedure ExportCell(ACell: TdxTableCell; ATableInfo: TdxTableInfo); override;
    procedure ExportCellMargin(const ATag: string; AMargin: TdxMarginUnitBase); overload;
    procedure ExportCellMargins(ATableProperties: TdxTableProperties); overload;
    procedure ExportCellMargins(const ATag: string; ACellMargins: TdxCellMargins); overload; virtual;
    procedure ExportCharacterStyle(AStyleIndex: Integer); virtual;
    procedure ExportCharacterStyles; virtual;
    procedure ExportColumn(AColumn: TdxColumnInfo); virtual;
    procedure ExportConditionalFormatting(AConditionalFormatting: TdxConditionalTableStyleFormattingTypes); virtual;
    procedure ExportDocumentCharacterDefaults; virtual; abstract;
    procedure ExportDocumentDefaults; virtual; abstract;
    procedure ExportDocumentParagraphDefaults; virtual; abstract;
    procedure ExportDocumentProtectionSettings; virtual;
    procedure ExportDocumentProtectionSettingsCore; virtual; abstract;
    procedure ExportDocumentVariablesSettings; virtual;
    procedure ExportDoubleFontSize(ACharacterProperties: TdxCharacterProperties);
    procedure ExportEndNoteCore(AWriter: TdxXmlWriter; AEndNote: TdxFootNoteBase; AId: Integer); virtual;
    procedure ExportEndNoteReference(ARun: TdxEndNoteRun); virtual;
    procedure ExportEndNoteRun(ARun: TdxEndNoteRun); override;
    procedure ExportEndNoteSelfReference(ARun: TdxEndNoteRun); virtual;
    procedure ExportEqualWidthColumns(AColumns: TdxSectionColumns); virtual;
    procedure ExportFieldChar(ARun: TdxTextRun; const AFieldCharType: string; AWriteDisableUpdate: Boolean); virtual;
    procedure ExportFieldCodeEndRun(ARun: TdxFieldCodeEndRun); override;
    procedure ExportFieldCodeStartRun(ARun: TdxFieldCodeStartRun); override;
    procedure ExportFieldResultEndRun(ARun: TdxFieldResultEndRun); override;
    procedure ExportFloatingObjectAnchorRun(ARun: TdxFloatingObjectAnchorRun); override;
    procedure ExportFloatingPosition(AFloatingPosition: TdxTableFloatingPosition); virtual;
    procedure ExportFootNoteCore(AWriter: TdxXmlWriter; AFootNote: TdxFootNoteBase; AId: Integer); overload; virtual;
    procedure ExportFootNoteCore(AWriter: TdxXmlWriter; ANote: TdxFootNoteBase; AId: Integer; const ATagName: string); overload; virtual;
    procedure ExportFootNoteReference(ARun: TdxFootNoteRun); virtual;
    procedure ExportFootNoteRun(ARun: TdxFootNoteRun); override;
    procedure ExportFootNoteSelfReference(ARun: TdxFootNoteRun); virtual;
    procedure ExportHeightUnit(AHeightUnit: TdxHeightUnit); virtual;
    procedure ExportImageData(const AImagePath: string); overload; virtual;
    procedure ExportInlinePictureImageReference(ARun: TdxInlinePictureRun); virtual;
    procedure ExportInlinePictureRun(ARun: TdxInlinePictureRun); overload; override;
    procedure ExportIsTableOverlap(AIsTableOverlap: Boolean); virtual;
    procedure ExportLevel(const ALevel: IdxListLevel; ALevelIndex: Integer);
    procedure ExportLevelCharacterProperties(AProperties: TdxCharacterProperties);
    procedure ExportLevelOverride(ALevel: TdxAbstractListLevel; ALevelIndex: Integer);
    procedure ExportLevelParagraphProperties(AProperties: TdxParagraphProperties; ATabs: TdxTabProperties);
    procedure ExportLevelProperties(const ALevel: IdxListLevel; ALevelIndex: Integer); virtual;
    procedure ExportLevels(ALevels: TdxListLevelCollection);
    procedure ExportLinkedCharacterStyle(AStyle: TdxCharacterStyle);
    procedure ExportLinkedParagraphStyle(AStyle: TdxParagraphStyle);
    procedure ExportLockAspectRatio(AFloatingObjectProperties: TdxFloatingObjectProperties);
    procedure ExportNextParagraphStyle(AStyle: TdxParagraphStyle);
    procedure ExportNonUniformColumns(AColumns: TdxSectionColumns); virtual;
    procedure ExportNumberFormatValue(AProperties: TdxListLevelProperties); virtual; abstract;
    procedure ExportNumberingCore;
    procedure ExportNumberingList(AList: TdxNumberingList; AId: Integer); virtual; abstract;
    procedure ExportNumberingLists; virtual;
    procedure ExportNumberingListStyle(AStyleIndex: Integer); virtual;
    procedure ExportNumberingListStyles; virtual;
    procedure ExportOverrideLevels(ALevels: TdxListLevelCollection);
    procedure ExportParagraphBackground(ABackground: TdxAlphaColor); virtual;
    procedure ExportParagraphBorder(const ATag: string; ABorder: TdxBorderInfo); virtual;
    procedure ExportParagraphBorders(AProperties: TdxParagraphProperties; ADefaultParagraphProperties: Boolean); virtual;
    procedure ExportParagraphFrame(AProperties: TdxFrameProperties); virtual;
    procedure ExportParagraphIndentation(AProperties: TdxParagraphProperties); virtual;
    procedure ExportParagraphListReference(AParagraph: TdxParagraph); virtual; abstract;
    procedure ExportParagraphOutlineLevel(AProperties: TdxParagraphProperties); virtual;
    procedure ExportParagraphProperties(AParagraph: TdxParagraph); virtual;
    procedure ExportParagraphPropertiesCore(AParagraph: TdxParagraph; const AParagraphNumberingExporter: TdxAction); overload; virtual;
    procedure ExportParagraphPropertiesCore(AProperties: TdxParagraphProperties; ADefaultParagraphProperties: Boolean;
      const AFramePropertiesExporter, AParagraphNumberingExporter, ATabsExporter: TdxAction); overload; virtual;
    procedure ExportParagraphSpacing(AProperties: TdxParagraphProperties); virtual;
    procedure ExportParagraphStyle(AStyleIndex: Integer); virtual;
    procedure ExportParagraphStyleListReference(ANumberingListIndex: TdxNumberingListIndex; AListLevelIndex: Integer); virtual; abstract;
    procedure ExportParagraphStyles; virtual;
    procedure ExportParentCharacterStyle(AStyle: TdxCharacterStyle);
    procedure ExportParentParagraphStyle(AStyle: TdxParagraphStyle);
    procedure ExportParentTableCellStyle(AStyle: TdxTableCellStyle);
    procedure ExportParentTableStyle(AStyle: TdxTableStyle);
    procedure ExportRangePermissionEnd(ARangePermission: TdxRangePermission); override;
    procedure ExportRangePermissionStart(ARangePermission: TdxRangePermission); override;
    procedure ExportRow(ARow: TdxTableRow; ATableInfo: TdxTableInfo); override;
    procedure ExportRunAllCaps(ACharacterProperties: TdxCharacterProperties);
    procedure ExportRunBackColor(ACharacterProperties: TdxCharacterProperties);
    procedure ExportRunFontBold(ACharacterProperties: TdxCharacterProperties);
    procedure ExportRunFontItalic(ACharacterProperties: TdxCharacterProperties);
    procedure ExportRunFontName(ACharacterProperties: TdxCharacterProperties); virtual;
    procedure ExportRunFontScript(ACharacterProperties: TdxCharacterProperties); virtual;
    procedure ExportRunFontStrikeout(ACharacterProperties: TdxCharacterProperties); virtual;
    procedure ExportRunFontUnderline(ACharacterProperties: TdxCharacterProperties); virtual;
    procedure ExportRunForeColor(ACharacterProperties: TdxCharacterProperties);
    procedure ExportRunHidden(ACharacterProperties: TdxCharacterProperties);
    procedure ExportRunLangInfo(ACharacterProperties: TdxCharacterProperties); virtual;
    procedure ExportRunNoProof(ACharacterProperties: TdxCharacterProperties); virtual;
    procedure ExportRunProperties(ARun: TdxTextRunBase); virtual;
    procedure ExportRunPropertiesCore(ACharacterProperties: TdxCharacterProperties); virtual;
    procedure ExportSection(const ASection: TdxSection); override;
    procedure ExportSectionColumns(AColumns: TdxSectionColumns); virtual;
    procedure ExportSectionEndNote(ANote: TdxSectionFootNote); virtual;
    procedure ExportSectionFootNote(ANote: TdxSectionFootNote); virtual;
    procedure ExportSectionFootNoteCore(const ATagName: string; ANote: TdxSectionFootNote; ADefaultInfo: TdxFootNoteInfo); virtual;
    procedure ExportSectionGeneralSettings(ASettings: TdxSectionGeneralSettings); virtual;
    procedure ExportSectionHeadersFooters(ASection: TdxSection); override;
    procedure ExportSectionLineNumbering(ANumbering: TdxSectionLineNumbering); virtual;
    procedure ExportSectionMargins(AMargins: TdxSectionMargins); virtual;
    procedure ExportSectionPage(APage: TdxSectionPage); virtual;
    procedure ExportSectionPageNumbering(ANumbering: TdxSectionPageNumbering); virtual;
    procedure ExportSectionProperties(ASection: TdxSection); virtual;
    procedure ExportSectionPropertiesCore(ASection: TdxSection); virtual;
    procedure ExportStartOverride(ANewStart: Integer);
    procedure ExportStyleCharacterProperties(ACharacterProperties: TdxCharacterProperties); virtual;
    procedure ExportStyleName(const AStyle: IdxStyle); virtual;
    procedure ExportStyleParagraphProperties(AParagraphProperties: TdxParagraphProperties; ATabInfo: TdxTabFormattingInfo;
      AOwnNumberingListIndex: TdxNumberingListIndex; AListLevelIndex: Integer; AMergedStyleListIndex: TdxNumberingListIndex); virtual;
    procedure ExportStylesCore; virtual;
    procedure ExportTab(const ATab: TdxTabInfo); virtual;
    procedure ExportTableBackground(ABackground: TdxAlphaColor); virtual;
    procedure ExportTableBorder(const ATag: string; ABorder: TdxBorderBase); virtual;
    procedure ExportTableBorderCore(ABorder: TdxBorderBase; AExportAutoColor: Boolean);
    procedure ExportTableBorders(ABorders: TdxTableBorders); virtual;
    procedure ExportTableCellBackgroundColor(ACellProperties: TdxTableCellProperties); virtual;
    procedure ExportTableCellBorder(const ATag: string; ABorder: TdxBorderBase); virtual;
    procedure ExportTableCellBorders(ABorders: TdxTableCellBorders); virtual;
    procedure ExportTableCellProperties(ACell: TdxTableCell); overload; virtual;
    procedure ExportTableCellProperties(ACellProperties: TdxTableCellProperties); overload; virtual;
    procedure ExportTableCellProperties(ACellProperties: TdxTableCellProperties; AExportBorders: Boolean); overload; virtual;
    procedure ExportTableCellPropertiesCore(ACellProperties: TdxTableCellProperties); overload;
    procedure ExportTableCellPropertiesCore(ACellProperties: TdxTableCellProperties; AExportBorders: Boolean); overload;
    procedure ExportTableCellPropertiesVerticalMerging(AVerticalMerging: TdxMergingState); virtual;
    procedure ExportTableCellStyle(AStyleIndex: Integer); virtual;
    procedure ExportTableCellStyles; virtual;
    procedure ExportTableConditionalStyle(AStyleType: TdxConditionalTableStyleFormattingType; AStyle: TdxTableConditionalStyle); virtual;
    procedure ExportTableLayout(ATableLayout: TdxTableLayoutType); virtual;
    procedure ExportTableLook(ATableLook: TdxTableLookTypes); virtual;
    procedure ExportTableProperties(ATable: TdxTable); virtual;
    procedure ExportTablePropertiesCore(ATableProperties: TdxTableProperties); overload; virtual;
    procedure ExportTablePropertiesCore(ATableProperties: TdxTableProperties; AExportTableLayout: Boolean); overload;
    procedure ExportTablePropertiesException(ATableProperties: TdxTableProperties); virtual;
    procedure ExportTableRowHeight(AHeight: TdxHeightUnit); virtual;
    procedure ExportTableRowProperties(ARowProperties: TdxTableRowProperties); overload; virtual;
    procedure ExportTableRowProperties(ARowProperties: TdxTableRowProperties; AIsStyle: Boolean); overload; virtual;
    procedure ExportTableStyle(AStyleIndex: Integer); virtual;
    procedure ExportTableStyleConditionalProperties(ATableConditionalStyleProperties: TdxTableConditionalStyleProperties); virtual;
    procedure ExportTableStyles; virtual;
    procedure ExportTabProperties(ATabs: TdxTabFormattingInfo); virtual;
    procedure ExportTextCore(const ARunText: string); overload; virtual;
    procedure ExportTextCore(const ARunText: string; AFrom: Integer; ALength: Integer); overload; virtual;
    procedure ExportTextRun(ARun: TdxTextRun); override;
    procedure ExportTextRunCore(ARun: TdxTextRun; const ARunText: string); virtual;
    procedure ExportTextShading(ABackground: TdxAlphaColor);
    procedure ExportWidthUnit(AWidthUnit: TdxWidthUnit); virtual;
    procedure ExportWidthUnitValue(const ATag: string; AValue: TdxWidthUnit); virtual;
    procedure IncrementDrawingElementId;
    procedure WriteBoolValue(const ATag: string; AValue: Boolean); virtual;
    procedure WriteFloatingObjectAnchorLock(ALocked: Boolean);
    procedure WriteFloatingObjectPict(AFloatingObjectProperties: TdxFloatingObjectProperties;
      ATextBoxContent: TdxTextBoxFloatingObjectContent; APictureContent: TdxPictureFloatingObjectContent;
      AShape: TdxShape; const AName: string);
    procedure WriteFloatingObjectShape(AFloatingObjectProperties: TdxFloatingObjectProperties;
      ATextBoxContent: TdxTextBoxFloatingObjectContent; APictureContent: TdxPictureFloatingObjectContent;
      AShape: TdxShape; AShapeTypeId: Integer; const AName: string);
    procedure WriteFloatingObjectShapeAllColorsAndOutlineWeight(AShape: TdxShape);
    procedure WriteFloatingObjectTextBox(ATextBoxContent: TdxTextBoxFloatingObjectContent);
    procedure WriteFloatingObjectTxbxContent(AContent: TdxTextBoxFloatingObjectContent);
    procedure WriteIntValue(const ATag: string; AValue: Integer); virtual;
    procedure WriteSettingsCore;
    procedure WriteStringAttr(const APrefix: string; const AAttr: string; const ANs: string; const AValue: string); virtual;
    procedure WriteStringValue(const APrefix: string; const ATag: string; const AValue: string); overload; virtual;
    procedure WriteStringValue(const ATag: string; const AValue: string); overload; virtual;
    procedure WriteTableCellStyle(ACell: TdxTableCell); virtual;
    procedure WriteWpBoolAttr(const AAttr: string; AValue: Boolean); virtual;
    procedure WriteWpBoolValue(const ATag: string; AValue: Boolean); virtual;
    procedure WriteWpBoolValueAsTag(const ATag: string; AValue: Boolean); virtual;
    procedure WriteWpEmptyElement(const ATag: string); virtual;
    procedure WriteWpEmptyOrFalseValue(const ATag: string; AValue: Boolean); virtual;
    procedure WriteWpEndElement; virtual;
    procedure WriteWpIntAttr(const AAttr: string; AValue: Integer); virtual;
    procedure WriteWpIntValue(const ATag: string; AValue: Integer); virtual;
    procedure WriteWpStartElement(const ATag: string); virtual;
    procedure WriteWpStringAttr(const AAttr: string; const AValue: string); virtual;
    procedure WriteWpStringValue(const ATag: string; const AValue: string); virtual;
    procedure WriteWrap(AFloatingObjectProperties: TdxFloatingObjectProperties);

    property Options: TdxDocumentExporterOptions read FOptions;
    property CurrentSection: TdxSection read FCurrentSection write FCurrentSection;
    property DocumentContentWriter: TdxXmlWriter read FDocumentContentWriter write FDocumentContentWriter;
    property WordProcessingNamespace: string read GetWordProcessingNamespace;
    property WordProcessingPrefix: string read GetWordProcessingPrefix;
    property ImageCounter: Integer read FImageCounter write FImageCounter;
    property UnitConverter: TdxDocumentModelUnitConverter read GetUnitConverter;
    property DrawingElementId: Integer read FDrawingElementId;
    property ForbidExecuteBaseExportTableCell: Boolean read FForbidExecuteBaseExportTableCell write FForbidExecuteBaseExportTableCell;
    property ExportedImageTable: TDictionary<TdxOfficeImage, string> read GetExportedImageTable;
    property HorizontalPositionTypeAttributeTable: TdxMLDictionary<TdxFloatingObjectHorizontalPositionType> read GetHorizontalPositionTypeAttributeTable;
    property VerticalPositionTypeAttributeTable: TdxMLDictionary<TdxFloatingObjectVerticalPositionType> read GetVerticalPositionTypeAttributeTable;
  public
    constructor Create(ADocumentModel: TdxCustomDocumentModel; const AOptions: IdxExporterOptions); override;
    destructor Destroy; override;

    class property PredefinedGroupNames: TdxStringsDictionary read FPredefinedGroupNames;
    class property PredefinedBackgroundColors: TDictionary<TdxAlphaColor, TdxWordProcessingMLValue> read FPredefinedBackgroundColors;
    class property PresetColors: TdxStringColorDictionary read FPresetColors;
    class property UnderlineTable: TdxMLDictionary<TdxUnderlineType> read FUnderlineTable;
    class property RunBreaksTable: TDictionary<Char, TdxWordProcessingMLValue> read FRunBreaksTable;
    class property ParagraphAlignmentTable: TdxMLDictionary<TdxParagraphAlignment> read FParagraphAlignmentTable;
    class property LineSpacingTable: TdxMLDictionary<TdxParagraphLineSpacing> read FLineSpacingTable;
    class property TabAlignmentTable: TdxMLDictionary<TdxTabAlignmentType> read FTabAlignmentTable;
    class property TabLeaderTable: TdxMLDictionary<TdxTabLeaderType> read FTabLeaderTable;
    class property TextDirectionTable: TdxMLDictionary<TdxTextDirection> read FTextDirectionTable;
    class property ShadingPatternTable: TdxMLDictionary<TdxShadingPattern> read FShadingPatternTable;
    class property VerticalAlignmentTable: TdxMLDictionary<TdxVerticalAlignment> read FVerticalAlignmentTable;
    class property SectionStartTypeTable: TdxMLDictionary<TdxSectionStartType> read FSectionStartTypeTable;
    class property LineNumberingRestartTable: TdxMLDictionary<TdxLineNumberingRestart> read FLineNumberingRestartTable;
    class property PageNumberingFormatTable: TdxMLDictionary<TdxNumberingFormat> read FPageNumberingFormatTable;
    class property ChapterSeparatorsTable: TDictionary<Char, TdxWordProcessingMLValue> read FChapterSeparatorsTable;
    class property NumberingListTypeTable: TdxMLDictionary<TdxNumberingType> read FNumberingListTypeTable;
    class property ListNumberAlignmentTable: TdxMLDictionary<TdxListNumberAlignment> read FListNumberAlignmentTable;
    class property ListNumberSeparatorTable: TDictionary<Char, TdxWordProcessingMLValue> read FListNumberSeparatorTable;
    class property ContentTypeTable: TdxStringsDictionary read FContentTypeTable;
    class property WidthUnitTypesTable: TdxMLDictionary<TdxWidthUnitType> read FWidthUnitTypesTable;
    class property TableLayoutTypeTable: TdxMLDictionary<TdxTableLayoutType> read FTableLayoutTypeTable;
    class property BorderLineStyleTable: TdxMLDictionary<TdxBorderLineStyle> read FBorderLineStyleTable;
    class property HorizontalAlignModeTable: TdxMLDictionary<TdxHorizontalAlignMode> read FHorizontalAlignModeTable;
    class property HorizontalAnchorTypesTable: TdxMLDictionary<TdxHorizontalAnchorTypes> read FHorizontalAnchorTypesTable;
    class property VerticalAlignModeTable: TdxMLDictionary<TdxVerticalAlignMode> read FVerticalAlignModeTable;
    class property VerticalAnchorTypesTable: TdxMLDictionary<TdxVerticalAnchorTypes> read FVerticalAnchorTypesTable;
    class property HeightUnitTypeTable: TdxMLDictionary<TdxHeightUnitType> read FHeightUnitTypeTable;
    class property MergingStateTable: TdxMLDictionary<TdxMergingState> read FMergingStateTable;
    class property TableRowAlignmentTable: TdxMLDictionary<TdxTableRowAlignment> read FTableRowAlignmentTable;
    class property FootNotePlacementTable: TdxMLDictionary<TdxFootNotePosition> read FFootNotePlacementTable;
    class property FloatingObjectHorizontalPositionAlignmentTable: TdxMLDictionary<TdxFloatingObjectHorizontalPositionAlignment> read FFloatingObjectHorizontalPositionAlignmentTable;
    class property FloatingObjectVerticalPositionAlignmentTable: TdxMLDictionary<TdxFloatingObjectVerticalPositionAlignment> read FFloatingObjectVerticalPositionAlignmentTable;
    class property FloatingObjectHorizontalPositionTypeTable: TdxMLDictionary<TdxFloatingObjectHorizontalPositionType> read FFloatingObjectHorizontalPositionTypeTable;
    class property FloatingObjectCssRelativeFromHorizontalTable: TdxMLDictionary<TdxFloatingObjectRelativeFromHorizontal> read FFloatingObjectCssRelativeFromHorizontalTable;
    class property FloatingObjectCssRelativeFromVerticalTable: TdxMLDictionary<TdxFloatingObjectRelativeFromVertical> read FFloatingObjectCssRelativeFromVerticalTable;
    class property FloatingObjectRelativeFromHorizontalTable: TdxMLDictionary<TdxFloatingObjectRelativeFromHorizontal> read FFloatingObjectRelativeFromHorizontalTable;
    class property FloatingObjectRelativeFromVerticalTable: TdxMLDictionary<TdxFloatingObjectRelativeFromVertical> read FFloatingObjectRelativeFromVerticalTable;
    class property FloatingObjectVerticalPositionTypeTable: TdxMLDictionary<TdxFloatingObjectVerticalPositionType> read FFloatingObjectVerticalPositionTypeTable;
    class property FloatingObjectTextWrapSideTable: TdxMLDictionary<TdxFloatingObjectTextWrapSide> read FFloatingObjectTextWrapSideTable;
    class property FloatingObjectTextWrapTypeTable: TdxMLDictionary<TdxFloatingObjectTextWrapType> read FFloatingObjectTextWrapTypeTable;
    class property TextBoxVerticalAlignmentTable: TdxMLDictionary<TdxVerticalAlignment> read FTextBoxVerticalAlignmentTable;
    class property ConditionalTableStyleFormattingTypesTable: TdxMLDictionary<TdxConditionalTableStyleFormattingType> read FConditionalTableStyleFormattingTypesTable;
  end;

implementation

uses
  RTLConsts, Math,
  Character, dxTypeHelpers,
  dxRichEdit.Utils.Exceptions,
  dxCharacters,
  dxStringHelper,
  dxXMLClasses,
  dxRichEdit.DocumentModel.Fields,
  dxRichEdit.DocumentModel.ProtectionFormatting;

{ TdxXmlBasedDocumentModelExporter }

constructor TdxXmlBasedDocumentModelExporter.Create(ADocumentModel: TdxCustomDocumentModel; const AOptions: IdxExporterOptions);
begin
  inherited Create(ADocumentModel, AOptions);
  FNow := SysUtils.Now;
end;

function TdxXmlBasedDocumentModelExporter.CreateXmlWriterSettings: TdxXmlWriterSettings;
begin
  Result := TdxXmlWriterSettings.Create;
  Result.Indent := False;
  Result.Encoding := TEncoding.UTF8;
  Result.CheckCharacters := True;
  Result.OmitXmlDeclaration := False;
end;

procedure TdxXmlBasedDocumentModelExporter.BeforeCreateXmlContent(AWriter: TdxXmlWriter);
begin
end;

function TdxXmlBasedDocumentModelExporter.CreateXmlContent(const AAction: TdxXMLWriteAction): TStream;
var
  AWriter: TdxXmlWriter;
begin
  Result := TMemoryStream.Create;
  try
    AWriter := TdxXmlWriter.Create(Result, CreateXmlWriterSettings);
    try
      BeforeCreateXmlContent(AWriter);
      AAction(AWriter);
      AWriter.Flush;
    finally
      AWriter.Free;
    end;
  except
    Result.Free;
    raise;
  end;
end;

function TdxXmlBasedDocumentModelExporter.CreateCompressedXmlContent(const AAction: TdxXMLWriteAction): TdxCompressedStream;
begin
  Result := CreateCompressedXmlContent(AAction, TEncoding.UTF8);
end;

function TdxXmlBasedDocumentModelExporter.CreateCompressedXmlContent(const AAction: TdxXMLWriteAction;
  AEncoding: TEncoding): TdxCompressedStream;
var
  AWriter: TdxXmlWriter;
begin
  if Package = nil then
    TdxRichEditExceptions.ThrowInternalException;

  Result := TdxCompressedStream.Create;
  try
    AWriter := TdxXmlWriter.Create(Result, CreateXmlWriterSettings);
    try
      BeforeCreateXmlContent(AWriter);
      AAction(AWriter);
    finally
      AWriter.Free;
    end;
  except
    Result.Free;
    raise;
  end;
end;

procedure TdxXmlBasedDocumentModelExporter.AddPackageContent(const AFileName: string; AContent: TStream);
begin
  Package.AddFile(AnsiString(AFileName), AContent, True);
end;

procedure TdxXmlBasedDocumentModelExporter.AddPackageContent(const AFileName: string; const AContent: TArray<Byte>);
begin
  Package.AddFile(AnsiString(AFileName), PByte(AContent), Length(AContent));
end;

procedure TdxXmlBasedDocumentModelExporter.AddCompressedPackageContent(const AFileName: string; AContent: TdxCompressedStream);
begin
  Package.AddFile(AnsiString(AFileName), AContent, True);
end;

procedure TdxXmlBasedDocumentModelExporter.AddPackageImage(const AFileName: string; AImage: TdxOfficeImage);
var
  AImageBytes: TArray<Byte>;
begin
  AImageBytes := GetImageBytes(AImage);
  Package.AddFile(AnsiString(AFileName), PByte(AImageBytes), Length(AImageBytes));
end;

function TdxXmlBasedDocumentModelExporter.GetImageExtension(AImage: TdxOfficeImage): string;
begin
  Result := TdxOfficeImage.GetExtension(AImage.RawFormat);
  if Result = '' then
    Result := 'png';
end;

function TdxXmlBasedDocumentModelExporter.GetImageBytesStream(AImage: TdxOfficeImage): TStream;
begin
  Result := AImage.GetImageBytesStreamSafe(AImage.RawFormat);
end;

function TdxXmlBasedDocumentModelExporter.GetImageBytes(AImage: TdxOfficeImage): TArray<Byte>;
begin
  Result := AImage.GetImageBytesSafe(AImage.RawFormat);
end;

{ TdxMLDictionary<TEnumeration> }

procedure TdxMLDictionary<TEnumeration>.AddValue(AKey: TEnumeration; const AValue: string);
begin
  Add(AKey, TdxWordProcessingMLValue.Create(AValue));
end;

function TdxMLDictionary<TEnumeration>.GetKeyByStringDef(const AValue: string; ADefaultKey: TEnumeration): TEnumeration;
var
  AIndex, AOpenXmlOrdKey, AWordMLOrdKey: Integer;
begin
  Result := ADefaultKey;
  AOpenXmlOrdKey := MinInt;
  AWordMLOrdKey  := MinInt;
  for AIndex := MinValue to MaxValue do
    if Map.ContainsKey(AIndex) then
    begin
      if AValue = Map.ValueTable[AIndex].OpenXmlValue then
        if Map.ValueTable[AIndex].ValuesAreEqual then
        begin
          TdxOrdinalHelper.SetValue(KeySize, AIndex, @Result);
          Exit;
        end
        else
          AOpenXmlOrdKey := AIndex
     else
       if AValue = Map.ValueTable[AIndex].WordMLValue then
         AWordMLOrdKey := AIndex;
    end;
  if AOpenXmlOrdKey <> MinInt then
    TdxOrdinalHelper.SetValue(KeySize, AOpenXmlOrdKey, @Result)
  else
    if AWordMLOrdKey <> MinInt then
      TdxOrdinalHelper.SetValue(KeySize, AWordMLOrdKey, @Result);
end;

function TdxMLDictionary<TEnumeration>.GetKeyByOpenXmlStringDef(const AValue: string; ADefaultKey: TEnumeration): TEnumeration;
var
  AIndex: Integer;
begin
  Result := ADefaultKey;
  for AIndex := MinValue to MaxValue do
    if Map.ContainsKey(AIndex) then
    begin
      if AValue = Map.ValueTable[AIndex].OpenXmlValue then
      begin
        TdxOrdinalHelper.SetValue(KeySize, AIndex, @Result);
        Exit;
      end;
    end;
end;

function TdxMLDictionary<TEnumeration>.GetKeyByWordMLStringDef(const AValue: string; ADefaultKey: TEnumeration): TEnumeration;
var
  AIndex: Integer;
begin
  Result := ADefaultKey;
  for AIndex := MinValue to MaxValue do
    if Map.ContainsKey(AIndex) then
    begin
      if AValue = Map.ValueTable[AIndex].WordMLValue then
      begin
        TdxOrdinalHelper.SetValue(KeySize, AIndex, @Result);
        Exit;
      end;
    end;
end;

function TdxMLDictionary<TEnumeration>.TryGetKeyByOpenXmlString(const AValue: string; out AKey: TEnumeration): Boolean;
var
  AIndex: Integer;
begin
  for AIndex := MaxValue downto MinValue do
    if Map.ContainsKey(AIndex) then
    begin
      if AValue = Map.ValueTable[AIndex].OpenXmlValue then
      begin
        TdxOrdinalHelper.SetValue(KeySize, AIndex, @AKey);
        Exit(True);
      end;
    end;
  Result := False;
end;

function TdxMLDictionary<TEnumeration>.TryGetKeyByWordMLString(const AValue: string; out AKey: TEnumeration): Boolean;
var
  AIndex: Integer;
begin
  for AIndex := MaxValue downto MinValue do
    if Map.ContainsKey(AIndex) then
    begin
      if AValue = Map.ValueTable[AIndex].WordMLValue then
      begin
        TdxOrdinalHelper.SetValue(KeySize, AIndex, @AKey);
        Exit(True);
      end;
    end;
  Result := False;
end;

{ TdxWordProcessingMLValue }

constructor TdxWordProcessingMLValue.Create(const AOpenXmlValue: string; const AWordMLValue: string);
begin
  FOpenXmlValue := AOpenXmlValue;
  FWordMLValue := AWordMLValue;
  FValuesAreEqual := FOpenXmlValue = FWordMLValue;
end;

constructor TdxWordProcessingMLValue.Create(const AOpenXmlValue: string);
begin
  FOpenXmlValue := AOpenXmlValue;
  FWordMLValue := ConvertToWordML(AOpenXmlValue);
  FValuesAreEqual := FOpenXmlValue = FWordMLValue;
end;

class function TdxWordProcessingMLValue.ConvertToWordML(const AOpenXmlValue: string): string;
var
  ABuilder: TStringBuilder;
  ACurrentChar: Char;
begin
  ABuilder := TStringBuilder.Create(Length(AOpenXmlValue));
  try
    for ACurrentChar in AOpenXmlValue do
    begin
    {$IFDEF DELPHIXE4}
      if ACurrentChar.IsUpper then
      begin
        ABuilder.Append('-');
        ABuilder.Append(ACurrentChar.ToLower);
      end
      else
        ABuilder.Append(ACurrentChar);
    {$ELSE}
      if TCharacter.IsUpper(ACurrentChar) then
      begin
        ABuilder.Append('-');
        ABuilder.Append(TCharacter.ToLower(ACurrentChar));
      end
      else
        ABuilder.Append(ACurrentChar);
    {$ENDIF}
    end;
    Result := ABuilder.ToString;
  finally
    ABuilder.Free;
  end;
end;

{ TdxWordProcessingMLBaseExporter }

constructor TdxWordProcessingMLBaseExporter.Create(ADocumentModel: TdxCustomDocumentModel; const AOptions: IdxExporterOptions);
begin
  inherited Create(ADocumentModel, AOptions);
  FDrawingElementId := 1;
  FOptions := TdxDocumentExporterOptions(AOptions);
  FRangePermissionIdMap := TDictionary<TdxRangePermission, string>.Create;
end;

destructor TdxWordProcessingMLBaseExporter.Destroy;
begin
  FRangePermissionIdMap.Free;
  inherited Destroy;
end;

function CreateBorderLineStyleTable: TdxMLDictionary<TdxBorderLineStyle>;
begin
  Result := TdxMLDictionary<TdxBorderLineStyle>.Create;
  Result.AddValue(TdxBorderLineStyle.&Nil, 'nil');
  Result.AddValue(TdxBorderLineStyle.DashDotStroked, 'dashDotStroked');
  Result.AddValue(TdxBorderLineStyle.Dashed, 'dashed');
  Result.AddValue(TdxBorderLineStyle.DashSmallGap, 'dashSmallGap');
  Result.AddValue(TdxBorderLineStyle.DotDash, 'dotDash');
  Result.AddValue(TdxBorderLineStyle.DotDotDash, 'dotDotDash');
  Result.AddValue(TdxBorderLineStyle.Dotted, 'dotted');
  Result.AddValue(TdxBorderLineStyle.Double, 'double');
  Result.AddValue(TdxBorderLineStyle.DoubleWave, 'doubleWave');
  Result.AddValue(TdxBorderLineStyle.Inset, 'inset');
  Result.AddValue(TdxBorderLineStyle.Disabled, 'disabled');
  Result.AddValue(TdxBorderLineStyle.None, 'none');
  Result.AddValue(TdxBorderLineStyle.Outset, 'outset');
  Result.AddValue(TdxBorderLineStyle.Single, 'single');
  Result.AddValue(TdxBorderLineStyle.Thick, 'thick');
  Result.AddValue(TdxBorderLineStyle.ThickThinLargeGap, 'thickThinLargeGap');
  Result.AddValue(TdxBorderLineStyle.ThickThinMediumGap, 'thickThinMediumGap');
  Result.AddValue(TdxBorderLineStyle.ThickThinSmallGap, 'thickThinSmallGap');
  Result.AddValue(TdxBorderLineStyle.ThinThickLargeGap, 'thinThickLargeGap');
  Result.AddValue(TdxBorderLineStyle.ThinThickMediumGap, 'thinThickMediumGap');
  Result.AddValue(TdxBorderLineStyle.ThinThickSmallGap, 'thinThickSmallGap');
  Result.AddValue(TdxBorderLineStyle.ThinThickThinLargeGap, 'thinThickThinLargeGap');
  Result.AddValue(TdxBorderLineStyle.ThinThickThinMediumGap, 'thinThickThinMediumGap');
  Result.AddValue(TdxBorderLineStyle.ThinThickThinSmallGap, 'thinThickThinSmallGap');
  Result.AddValue(TdxBorderLineStyle.ThreeDEmboss, 'threeDEmboss');
  Result.AddValue(TdxBorderLineStyle.ThreeDEngrave, 'threeDEngrave');
  Result.AddValue(TdxBorderLineStyle.Triple, 'triple');
  Result.AddValue(TdxBorderLineStyle.Wave, 'wave');

  Result.AddValue(TdxBorderLineStyle.Apples, 'apples');
  Result.AddValue(TdxBorderLineStyle.ArchedScallops, 'archedScallops');
  Result.AddValue(TdxBorderLineStyle.BabyPacifier, 'babyPacifier');
  Result.AddValue(TdxBorderLineStyle.BabyRattle, 'babyRattle');
  Result.AddValue(TdxBorderLineStyle.Balloons3Colors, 'balloons3Colors');
  Result.AddValue(TdxBorderLineStyle.BalloonsHotAir, 'balloonsHotAir');
  Result.AddValue(TdxBorderLineStyle.BasicBlackDashes, 'basicBlackDashes');
  Result.AddValue(TdxBorderLineStyle.BasicBlackDots, 'basicBlackDots');
  Result.AddValue(TdxBorderLineStyle.BasicBlackSquares, 'basicBlackSquares');
  Result.AddValue(TdxBorderLineStyle.BasicThinLines, 'basicThinLines');
  Result.AddValue(TdxBorderLineStyle.BasicWhiteDashes, 'basicWhiteDashes');
  Result.AddValue(TdxBorderLineStyle.BasicWhiteDots, 'basicWhiteDots');
  Result.AddValue(TdxBorderLineStyle.BasicWhiteSquares, 'basicWhiteSquares');
  Result.AddValue(TdxBorderLineStyle.BasicWideInline, 'basicWideInline');
  Result.AddValue(TdxBorderLineStyle.BasicWideMidline, 'basicWideMidline');
  Result.AddValue(TdxBorderLineStyle.BasicWideOutline, 'basicWideOutline');
  Result.AddValue(TdxBorderLineStyle.Bats, 'bats');
  Result.AddValue(TdxBorderLineStyle.Birds, 'birds');
  Result.AddValue(TdxBorderLineStyle.BirdsFlight, 'birdsFlight');
  Result.AddValue(TdxBorderLineStyle.Cabins, 'cabins');
  Result.AddValue(TdxBorderLineStyle.CakeSlice, 'cakeSlice');
  Result.AddValue(TdxBorderLineStyle.CandyCorn, 'candyCorn');
  Result.AddValue(TdxBorderLineStyle.CelticKnotwork, 'celticKnotwork');
  Result.AddValue(TdxBorderLineStyle.CertificateBanner, 'certificateBanner');
  Result.AddValue(TdxBorderLineStyle.ChainLink, 'chainLink');
  Result.AddValue(TdxBorderLineStyle.ChampagneBottle, 'champagneBottle');
  Result.AddValue(TdxBorderLineStyle.CheckedBarBlack, 'checkedBarBlack');
  Result.AddValue(TdxBorderLineStyle.CheckedBarColor, 'checkedBarColor');
  Result.AddValue(TdxBorderLineStyle.Checkered, 'checkered');
  Result.AddValue(TdxBorderLineStyle.ChristmasTree, 'christmasTree');
  Result.AddValue(TdxBorderLineStyle.CirclesLines, 'circlesLines');
  Result.AddValue(TdxBorderLineStyle.CirclesRectangles, 'circlesRectangles');
  Result.AddValue(TdxBorderLineStyle.ClassicalWave, 'classicalWave');
  Result.AddValue(TdxBorderLineStyle.Clocks, 'clocks');
  Result.AddValue(TdxBorderLineStyle.Compass, 'compass');
  Result.AddValue(TdxBorderLineStyle.Confetti, 'confetti');
  Result.AddValue(TdxBorderLineStyle.ConfettiGrays, 'confettiGrays');
  Result.AddValue(TdxBorderLineStyle.ConfettiOutline, 'confettiOutline');
  Result.AddValue(TdxBorderLineStyle.ConfettiStreamers, 'confettiStreamers');
  Result.AddValue(TdxBorderLineStyle.ConfettiWhite, 'confettiWhite');
  Result.AddValue(TdxBorderLineStyle.CornerTriangles, 'cornerTriangles');
  Result.AddValue(TdxBorderLineStyle.CouponCutoutDashes, 'couponCutoutDashes');
  Result.AddValue(TdxBorderLineStyle.CouponCutoutDots, 'couponCutoutDots');
  Result.AddValue(TdxBorderLineStyle.CrazyMaze, 'crazyMaze');
  Result.AddValue(TdxBorderLineStyle.CreaturesButterfly, 'creaturesButterfly');
  Result.AddValue(TdxBorderLineStyle.CreaturesFish, 'creaturesFish');
  Result.AddValue(TdxBorderLineStyle.CreaturesInsects, 'creaturesInsects');
  Result.AddValue(TdxBorderLineStyle.CreaturesLadyBug, 'creaturesLadyBug');
  Result.AddValue(TdxBorderLineStyle.CrossStitch, 'crossStitch');
  Result.AddValue(TdxBorderLineStyle.Cup, 'cup');
  Result.AddValue(TdxBorderLineStyle.DecoArch, 'decoArch');
  Result.AddValue(TdxBorderLineStyle.DecoArchColor, 'decoArchColor');
  Result.AddValue(TdxBorderLineStyle.DecoBlocks, 'decoBlocks');
  Result.AddValue(TdxBorderLineStyle.DiamondsGray, 'diamondsGray');
  Result.AddValue(TdxBorderLineStyle.DoubleD, 'doubleD');
  Result.AddValue(TdxBorderLineStyle.DoubleDiamonds, 'doubleDiamonds');
  Result.AddValue(TdxBorderLineStyle.Earth1, 'earth1');
  Result.AddValue(TdxBorderLineStyle.Earth2, 'earth2');
  Result.AddValue(TdxBorderLineStyle.EclipsingSquares1, 'eclipsingSquares1');
  Result.AddValue(TdxBorderLineStyle.EclipsingSquares2, 'eclipsingSquares2');
  Result.AddValue(TdxBorderLineStyle.EggsBlack, 'eggsBlack');
  Result.AddValue(TdxBorderLineStyle.Fans, 'fans');
  Result.AddValue(TdxBorderLineStyle.Film, 'film');
  Result.AddValue(TdxBorderLineStyle.Firecrackers, 'firecrackers');
  Result.AddValue(TdxBorderLineStyle.FlowersBlockPrint, 'flowersBlockPrint');
  Result.AddValue(TdxBorderLineStyle.FlowersDaisies, 'flowersDaisies');
  Result.AddValue(TdxBorderLineStyle.FlowersModern1, 'flowersModern1');
  Result.AddValue(TdxBorderLineStyle.FlowersModern2, 'flowersModern2');
  Result.AddValue(TdxBorderLineStyle.FlowersPansy, 'flowersPansy');
  Result.AddValue(TdxBorderLineStyle.FlowersRedRose, 'flowersRedRose');
  Result.AddValue(TdxBorderLineStyle.FlowersRoses, 'flowersRoses');
  Result.AddValue(TdxBorderLineStyle.FlowersTeacup, 'flowersTeacup');
  Result.AddValue(TdxBorderLineStyle.FlowersTiny, 'flowersTiny');
  Result.AddValue(TdxBorderLineStyle.Gems, 'gems');
  Result.AddValue(TdxBorderLineStyle.GingerbreadMan, 'gingerbreadMan');
  Result.AddValue(TdxBorderLineStyle.Gradient, 'gradient');
  Result.AddValue(TdxBorderLineStyle.Handmade1, 'handmade1');
  Result.AddValue(TdxBorderLineStyle.Handmade2, 'handmade2');
  Result.AddValue(TdxBorderLineStyle.HeartBalloon, 'heartBalloon');
  Result.AddValue(TdxBorderLineStyle.HeartGray, 'heartGray');
  Result.AddValue(TdxBorderLineStyle.Hearts, 'hearts');
  Result.AddValue(TdxBorderLineStyle.HeebieJeebies, 'heebieJeebies');
  Result.AddValue(TdxBorderLineStyle.Holly, 'holly');
  Result.AddValue(TdxBorderLineStyle.HouseFunky, 'houseFunky');
  Result.AddValue(TdxBorderLineStyle.Hypnotic, 'hypnotic');
  Result.AddValue(TdxBorderLineStyle.IceCreamCones, 'iceCreamCones');
  Result.AddValue(TdxBorderLineStyle.LightBulb, 'lightBulb');
  Result.AddValue(TdxBorderLineStyle.Lightning1, 'lightning1');
  Result.AddValue(TdxBorderLineStyle.Lightning2, 'lightning2');
  Result.AddValue(TdxBorderLineStyle.MapleLeaf, 'mapleLeaf');
  Result.AddValue(TdxBorderLineStyle.MapleMuffins, 'mapleMuffins');
  Result.AddValue(TdxBorderLineStyle.MapPins, 'mapPins');
  Result.AddValue(TdxBorderLineStyle.Marquee, 'marquee');
  Result.AddValue(TdxBorderLineStyle.MarqueeToothed, 'marqueeToothed');
  Result.AddValue(TdxBorderLineStyle.Moons, 'moons');
  Result.AddValue(TdxBorderLineStyle.Mosaic, 'mosaic');
  Result.AddValue(TdxBorderLineStyle.MusicNotes, 'musicNotes');
  Result.AddValue(TdxBorderLineStyle.Northwest, 'northwest');
  Result.AddValue(TdxBorderLineStyle.Ovals, 'ovals');
  Result.AddValue(TdxBorderLineStyle.Packages, 'packages');
  Result.AddValue(TdxBorderLineStyle.PalmsBlack, 'palmsBlack');
  Result.AddValue(TdxBorderLineStyle.PalmsColor, 'palmsColor');
  Result.AddValue(TdxBorderLineStyle.PaperClips, 'paperClips');
  Result.AddValue(TdxBorderLineStyle.Papyrus, 'papyrus');
  Result.AddValue(TdxBorderLineStyle.PartyFavor, 'partyFavor');
  Result.AddValue(TdxBorderLineStyle.PartyGlass, 'partyGlass');
  Result.AddValue(TdxBorderLineStyle.Pencils, 'pencils');
  Result.AddValue(TdxBorderLineStyle.People, 'people');
  Result.AddValue(TdxBorderLineStyle.PeopleHats, 'peopleHats');
  Result.AddValue(TdxBorderLineStyle.PeopleWaving, 'peopleWaving');
  Result.AddValue(TdxBorderLineStyle.Poinsettias, 'poinsettias');
  Result.AddValue(TdxBorderLineStyle.PostageStamp, 'postageStamp');
  Result.AddValue(TdxBorderLineStyle.Pumpkin1, 'pumpkin1');
  Result.AddValue(TdxBorderLineStyle.PushPinNote1, 'pushPinNote1');
  Result.AddValue(TdxBorderLineStyle.PushPinNote2, 'pushPinNote2');
  Result.AddValue(TdxBorderLineStyle.Pyramids, 'pyramids');
  Result.AddValue(TdxBorderLineStyle.PyramidsAbove, 'pyramidsAbove');
  Result.AddValue(TdxBorderLineStyle.Quadrants, 'quadrants');
  Result.AddValue(TdxBorderLineStyle.Rings, 'rings');
  Result.AddValue(TdxBorderLineStyle.Safari, 'safari');
  Result.AddValue(TdxBorderLineStyle.Sawtooth, 'sawtooth');
  Result.AddValue(TdxBorderLineStyle.SawtoothGray, 'sawtoothGray');
  Result.AddValue(TdxBorderLineStyle.ScaredCat, 'scaredCat');
  Result.AddValue(TdxBorderLineStyle.Seattle, 'seattle');
  Result.AddValue(TdxBorderLineStyle.ShadowedSquares, 'shadowedSquares');
  Result.AddValue(TdxBorderLineStyle.SharksTeeth, 'sharksTeeth');
  Result.AddValue(TdxBorderLineStyle.ShorebirdTracks, 'shorebirdTracks');
  Result.AddValue(TdxBorderLineStyle.Skyrocket, 'skyrocket');
  Result.AddValue(TdxBorderLineStyle.SnowflakeFancy, 'snowflakeFancy');
  Result.AddValue(TdxBorderLineStyle.Snowflakes, 'snowflakes');
  Result.AddValue(TdxBorderLineStyle.Sombrero, 'sombrero');
  Result.AddValue(TdxBorderLineStyle.Southwest, 'southwest');
  Result.AddValue(TdxBorderLineStyle.Stars, 'stars');
  Result.AddValue(TdxBorderLineStyle.Stars3d, 'stars3d');
  Result.AddValue(TdxBorderLineStyle.StarsBlack, 'starsBlack');
  Result.AddValue(TdxBorderLineStyle.StarsShadowed, 'starsShadowed');
  Result.AddValue(TdxBorderLineStyle.StarsTop, 'starsTop');
  Result.AddValue(TdxBorderLineStyle.Sun, 'sun');
  Result.AddValue(TdxBorderLineStyle.Swirligig, 'swirligig');
  Result.AddValue(TdxBorderLineStyle.TornPaper, 'tornPaper');
  Result.AddValue(TdxBorderLineStyle.TornPaperBlack, 'tornPaperBlack');
  Result.AddValue(TdxBorderLineStyle.Trees, 'trees');
  Result.AddValue(TdxBorderLineStyle.TriangleParty, 'triangleParty');
  Result.AddValue(TdxBorderLineStyle.Triangles, 'triangles');
  Result.AddValue(TdxBorderLineStyle.Tribal1, 'tribal1');
  Result.AddValue(TdxBorderLineStyle.Tribal2, 'tribal2');
  Result.AddValue(TdxBorderLineStyle.Tribal3, 'tribal3');
  Result.AddValue(TdxBorderLineStyle.Tribal4, 'tribal4');
  Result.AddValue(TdxBorderLineStyle.Tribal5, 'tribal5');
  Result.AddValue(TdxBorderLineStyle.Tribal6, 'tribal6');
  Result.AddValue(TdxBorderLineStyle.TwistedLines1, 'twistedLines1');
  Result.AddValue(TdxBorderLineStyle.TwistedLines2, 'twistedLines2');
  Result.AddValue(TdxBorderLineStyle.Vine, 'vine');
  Result.AddValue(TdxBorderLineStyle.Waveline, 'waveline');
  Result.AddValue(TdxBorderLineStyle.WeavingAngles, 'weavingAngles');
  Result.AddValue(TdxBorderLineStyle.WeavingBraid, 'weavingBraid');
  Result.AddValue(TdxBorderLineStyle.WeavingRibbon, 'weavingRibbon');
  Result.AddValue(TdxBorderLineStyle.WeavingStrips, 'weavingStrips');
  Result.AddValue(TdxBorderLineStyle.WhiteFlowers, 'whiteFlowers');
  Result.AddValue(TdxBorderLineStyle.Woodwork, 'woodwork');
  Result.AddValue(TdxBorderLineStyle.XIllusions, 'xIllusions');
  Result.AddValue(TdxBorderLineStyle.ZanyTriangles, 'zanyTriangles');
  Result.AddValue(TdxBorderLineStyle.ZigZag, 'zigZag');
  Result.AddValue(TdxBorderLineStyle.ZigZagStitch, 'zigZagStitch');
end;

function CreateChapterSeparatorsTable: TDictionary<Char, TdxWordProcessingMLValue>;
begin
  Result := TDictionary<Char, TdxWordProcessingMLValue>.Create;
  Result.Add(':', TdxWordProcessingMLValue.Create('colon'));
  Result.Add(TdxCharacters.EmDash, TdxWordProcessingMLValue.Create('emDash'));
  Result.Add(TdxCharacters.EnDash, TdxWordProcessingMLValue.Create('enDash'));
  Result.Add(TdxCharacters.Hyphen, TdxWordProcessingMLValue.Create('hyphen'));
  Result.Add('.', TdxWordProcessingMLValue.Create('period'));
end;

function CreateConditionalTableStyleFormattingTypesTable: TdxMLDictionary<TdxConditionalTableStyleFormattingType>;
begin
  Result := TdxMLDictionary<TdxConditionalTableStyleFormattingType>.Create;
  Result.Add(TdxConditionalTableStyleFormattingType.BottomLeftCell, TdxWordProcessingMLValue.Create('swCell', 'swCell'));
  Result.Add(TdxConditionalTableStyleFormattingType.BottomRightCell, TdxWordProcessingMLValue.Create('seCell', 'seCell'));
  Result.Add(TdxConditionalTableStyleFormattingType.EvenColumnBanding, TdxWordProcessingMLValue.Create('band2Vert', 'band2Vert'));
  Result.Add(TdxConditionalTableStyleFormattingType.EvenRowBanding, TdxWordProcessingMLValue.Create('band2Horz', 'band2Horz'));
  Result.Add(TdxConditionalTableStyleFormattingType.FirstColumn, TdxWordProcessingMLValue.Create('firstCol', 'firstCol'));
  Result.Add(TdxConditionalTableStyleFormattingType.FirstRow, TdxWordProcessingMLValue.Create('firstRow', 'firstRow'));
  Result.Add(TdxConditionalTableStyleFormattingType.LastColumn, TdxWordProcessingMLValue.Create('lastCol', 'lastCol'));
  Result.Add(TdxConditionalTableStyleFormattingType.LastRow, TdxWordProcessingMLValue.Create('lastRow', 'lastRow'));
  Result.Add(TdxConditionalTableStyleFormattingType.OddColumnBanding, TdxWordProcessingMLValue.Create('band1Vert', 'band1Vert'));
  Result.Add(TdxConditionalTableStyleFormattingType.OddRowBanding, TdxWordProcessingMLValue.Create('band1Horz', 'band1Horz'));
  Result.Add(TdxConditionalTableStyleFormattingType.TopLeftCell, TdxWordProcessingMLValue.Create('nwCell', 'nwCell'));
  Result.Add(TdxConditionalTableStyleFormattingType.TopRightCell, TdxWordProcessingMLValue.Create('neCell', 'neCell'));
  Result.Add(TdxConditionalTableStyleFormattingType.WholeTable, TdxWordProcessingMLValue.Create('wholeTable', 'wholeTable'));
end;

function CreateContentTypeTable: TdxStringsDictionary;
begin
  Result := TdxStringsDictionary.Create;
  Result.Add('jpg', 'image/jpeg');
  Result.Add('png', 'image/png');
  Result.Add('bmp', 'image/bitmap');
  Result.Add('tif', 'image/tiff');
  Result.Add('gif', 'image/gif');
  Result.Add('ico', 'image/x-icon');
  Result.Add('wmf', 'application/x-msmetafile');
  Result.Add('emf', 'application/x-msmetafile');
end;

function CreateFloatingObjectCssRelativeFromHorizontalTable: TdxMLDictionary<TdxFloatingObjectRelativeFromHorizontal>;
begin
  Result := TdxMLDictionary<TdxFloatingObjectRelativeFromHorizontal>.Create;
  Result.AddValue(TdxFloatingObjectRelativeFromHorizontal.Margin, 'margin');
  Result.AddValue(TdxFloatingObjectRelativeFromHorizontal.Page, 'page');
  Result.AddValue(TdxFloatingObjectRelativeFromHorizontal.LeftMargin, 'left-margin-area');
  Result.AddValue(TdxFloatingObjectRelativeFromHorizontal.RightMargin, 'right-margin-area');
  Result.AddValue(TdxFloatingObjectRelativeFromHorizontal.OutsideMargin, 'outer-margin-area');
  Result.AddValue(TdxFloatingObjectRelativeFromHorizontal.InsideMargin, 'inner-margin-area');
end;

function CreateFloatingObjectCssRelativeFromVerticalTable: TdxMLDictionary<TdxFloatingObjectRelativeFromVertical>;
begin
  Result := TdxMLDictionary<TdxFloatingObjectRelativeFromVertical>.Create;
  Result.AddValue(TdxFloatingObjectRelativeFromVertical.Margin, 'margin');
  Result.AddValue(TdxFloatingObjectRelativeFromVertical.Page, 'page');
  Result.AddValue(TdxFloatingObjectRelativeFromVertical.TopMargin, 'top-margin-area');
  Result.AddValue(TdxFloatingObjectRelativeFromVertical.BottomMargin, 'bottom-margin-area');
  Result.AddValue(TdxFloatingObjectRelativeFromVertical.OutsideMargin, 'outer-margin-area');
  Result.AddValue(TdxFloatingObjectRelativeFromVertical.InsideMargin, 'inner-margin-area');
end;

function CreateFloatingObjectHorizontalPositionAlignmentTable: TdxMLDictionary<TdxFloatingObjectHorizontalPositionAlignment>;
begin
  Result := TdxMLDictionary<TdxFloatingObjectHorizontalPositionAlignment>.Create;
  Result.AddValue(TdxFloatingObjectHorizontalPositionAlignment.Left, 'left');
  Result.AddValue(TdxFloatingObjectHorizontalPositionAlignment.Center, 'center');
  Result.AddValue(TdxFloatingObjectHorizontalPositionAlignment.Right, 'right');
  Result.AddValue(TdxFloatingObjectHorizontalPositionAlignment.Inside, 'inside');
  Result.AddValue(TdxFloatingObjectHorizontalPositionAlignment.Outside, 'outside');
end;

function CreateFloatingObjectHorizontalPositionTypeTable: TdxMLDictionary<TdxFloatingObjectHorizontalPositionType>;
begin
  Result := TdxMLDictionary<TdxFloatingObjectHorizontalPositionType>.Create;
  Result.Add(TdxFloatingObjectHorizontalPositionType.Column, TdxWordProcessingMLValue.Create('column', 'text'));
  Result.AddValue(TdxFloatingObjectHorizontalPositionType.Margin, 'margin');
  Result.AddValue(TdxFloatingObjectHorizontalPositionType.Page, 'page');
  Result.Add(TdxFloatingObjectHorizontalPositionType.Character, TdxWordProcessingMLValue.Create('character', 'char'));
  Result.Add(TdxFloatingObjectHorizontalPositionType.LeftMargin, TdxWordProcessingMLValue.Create('leftMargin', 'page'));
  Result.Add(TdxFloatingObjectHorizontalPositionType.RightMargin, TdxWordProcessingMLValue.Create('rightMargin', 'page'));
  Result.Add(TdxFloatingObjectHorizontalPositionType.InsideMargin, TdxWordProcessingMLValue.Create('insideMargin', 'page'));
  Result.Add(TdxFloatingObjectHorizontalPositionType.OutsideMargin, TdxWordProcessingMLValue.Create('outsideMargin', 'page'));
end;

function CreateFloatingObjectRelativeFromHorizontalTable: TdxMLDictionary<TdxFloatingObjectRelativeFromHorizontal>;
begin
  Result := TdxMLDictionary<TdxFloatingObjectRelativeFromHorizontal>.Create;
  Result.AddValue(TdxFloatingObjectRelativeFromHorizontal.Margin, 'margin');
  Result.AddValue(TdxFloatingObjectRelativeFromHorizontal.Page, 'page');
  Result.AddValue(TdxFloatingObjectRelativeFromHorizontal.LeftMargin, 'leftMargin');
  Result.AddValue(TdxFloatingObjectRelativeFromHorizontal.RightMargin, 'rightMargin');
  Result.AddValue(TdxFloatingObjectRelativeFromHorizontal.OutsideMargin, 'outsideMargin');
  Result.AddValue(TdxFloatingObjectRelativeFromHorizontal.InsideMargin, 'insideMargin');
end;

function CreateFloatingObjectRelativeFromVerticalTable: TdxMLDictionary<TdxFloatingObjectRelativeFromVertical>;
begin
  Result := TdxMLDictionary<TdxFloatingObjectRelativeFromVertical>.Create;
  Result.AddValue(TdxFloatingObjectRelativeFromVertical.Margin, 'margin');
  Result.AddValue(TdxFloatingObjectRelativeFromVertical.Page, 'page');
  Result.AddValue(TdxFloatingObjectRelativeFromVertical.TopMargin, 'topMargin');
  Result.AddValue(TdxFloatingObjectRelativeFromVertical.BottomMargin, 'bottomMargin');
  Result.AddValue(TdxFloatingObjectRelativeFromVertical.OutsideMargin, 'outsideMargin');
  Result.AddValue(TdxFloatingObjectRelativeFromVertical.InsideMargin, 'insideMargin');
end;

function CreateFloatingObjectTextWrapSideTable: TdxMLDictionary<TdxFloatingObjectTextWrapSide>;
begin
  Result := TdxMLDictionary<TdxFloatingObjectTextWrapSide>.Create;
  Result.Add(TdxFloatingObjectTextWrapSide.Both, TdxWordProcessingMLValue.Create('bothSides', 'both-sides'));
  Result.AddValue(TdxFloatingObjectTextWrapSide.Left, 'left');
  Result.AddValue(TdxFloatingObjectTextWrapSide.Right, 'right');
  Result.AddValue(TdxFloatingObjectTextWrapSide.Largest, 'largest');
end;

function CreateFloatingObjectTextWrapTypeTable: TdxMLDictionary<TdxFloatingObjectTextWrapType>;
begin
  Result := TdxMLDictionary<TdxFloatingObjectTextWrapType>.Create;
  Result.Add(TdxFloatingObjectTextWrapType.TopAndBottom, TdxWordProcessingMLValue.Create('wrapTopAndBottom', 'topAndBottom'));
  Result.Add(TdxFloatingObjectTextWrapType.Square, TdxWordProcessingMLValue.Create('wrapSquare', 'square'));
  Result.Add(TdxFloatingObjectTextWrapType.Through, TdxWordProcessingMLValue.Create('wrapThrough', 'through'));
  Result.Add(TdxFloatingObjectTextWrapType.Tight, TdxWordProcessingMLValue.Create('wrapTight', 'tight'));
  Result.Add(TdxFloatingObjectTextWrapType.None, TdxWordProcessingMLValue.Create('wrapNone', 'none'));
end;

function CreateFloatingObjectVerticalPositionAlignmentTable: TdxMLDictionary<TdxFloatingObjectVerticalPositionAlignment>;
begin
  Result := TdxMLDictionary<TdxFloatingObjectVerticalPositionAlignment>.Create;
  Result.AddValue(TdxFloatingObjectVerticalPositionAlignment.Top, 'top');
  Result.AddValue(TdxFloatingObjectVerticalPositionAlignment.Center, 'center');
  Result.AddValue(TdxFloatingObjectVerticalPositionAlignment.Bottom, 'bottom');
  Result.AddValue(TdxFloatingObjectVerticalPositionAlignment.Inside, 'inside');
  Result.AddValue(TdxFloatingObjectVerticalPositionAlignment.Outside, 'outside');
end;

function CreateFloatingObjectVerticalPositionTypeTable: TdxMLDictionary<TdxFloatingObjectVerticalPositionType>;
begin
  Result := TdxMLDictionary<TdxFloatingObjectVerticalPositionType>.Create;
  Result.AddValue(TdxFloatingObjectVerticalPositionType.Margin, 'margin');
  Result.Add(TdxFloatingObjectVerticalPositionType.Page, TdxWordProcessingMLValue.Create('page', 'margin'));
  Result.AddValue(TdxFloatingObjectVerticalPositionType.Line, 'line');
  Result.Add(TdxFloatingObjectVerticalPositionType.Paragraph, TdxWordProcessingMLValue.Create('paragraph', 'text'));
  Result.Add(TdxFloatingObjectVerticalPositionType.TopMargin, TdxWordProcessingMLValue.Create('topMargin', 'page'));
  Result.Add(TdxFloatingObjectVerticalPositionType.BottomMargin, TdxWordProcessingMLValue.Create('bottomMargin', 'page'));
  Result.Add(TdxFloatingObjectVerticalPositionType.InsideMargin, TdxWordProcessingMLValue.Create('insideMargin', 'page'));
  Result.Add(TdxFloatingObjectVerticalPositionType.OutsideMargin, TdxWordProcessingMLValue.Create('outsideMargin', 'page'));
end;

function CreateFootNotePlacementTable: TdxMLDictionary<TdxFootNotePosition>;
begin
  Result := TdxMLDictionary<TdxFootNotePosition>.Create;
  Result.AddValue(TdxFootNotePosition.BelowText, 'beneathText');
  Result.AddValue(TdxFootNotePosition.BottomOfPage, 'pageBottom');
  Result.AddValue(TdxFootNotePosition.EndOfDocument, 'docEnd');
  Result.AddValue(TdxFootNotePosition.EndOfSection, 'sectEnd');
end;

function CreateHeightUnitTypeTable: TdxMLDictionary<TdxHeightUnitType>;
begin
  Result := TdxMLDictionary<TdxHeightUnitType>.Create;
  Result.AddValue(TdxHeightUnitType.Auto, 'auto');
  Result.AddValue(TdxHeightUnitType.Exact, 'exact');
  Result.AddValue(TdxHeightUnitType.Minimum, 'atLeast');
end;

function CreateHorizontalAlignModeTable: TdxMLDictionary<TdxHorizontalAlignMode>;
begin
  Result := TdxMLDictionary<TdxHorizontalAlignMode>.Create;
  Result.AddValue(TdxHorizontalAlignMode.Center, 'center');
  Result.AddValue(TdxHorizontalAlignMode.Inside, 'inside');
  Result.AddValue(TdxHorizontalAlignMode.Left, 'left');
  Result.AddValue(TdxHorizontalAlignMode.Outside, 'outside');
  Result.AddValue(TdxHorizontalAlignMode.Right, 'right');
end;

function CreateHorizontalAnchorTypesTable: TdxMLDictionary<TdxHorizontalAnchorTypes>;
begin
  Result := TdxMLDictionary<TdxHorizontalAnchorTypes>.Create;
  Result.AddValue(TdxHorizontalAnchorTypes.Column, 'text');
  Result.AddValue(TdxHorizontalAnchorTypes.Margin, 'margin');
  Result.AddValue(TdxHorizontalAnchorTypes.Page, 'page');
end;

function CreateLineNumberingRestartTable: TdxMLDictionary<TdxLineNumberingRestart>;
begin
  Result := TdxMLDictionary<TdxLineNumberingRestart>.Create;
  Result.AddValue(TdxLineNumberingRestart.Continuous, 'continuous');
  Result.AddValue(TdxLineNumberingRestart.NewPage, 'newPage');
  Result.AddValue(TdxLineNumberingRestart.NewSection, 'newSection');
end;

function CreateListNumberAlignmentTable: TdxMLDictionary<TdxListNumberAlignment>;
begin
  Result := TdxMLDictionary<TdxListNumberAlignment>.Create;
  Result.AddValue(TdxListNumberAlignment.Left, 'left');
  Result.AddValue(TdxListNumberAlignment.Right, 'right');
  Result.AddValue(TdxListNumberAlignment.Center, 'center');
end;

function CreateListNumberSeparatorTable: TDictionary<Char, TdxWordProcessingMLValue>;
begin
  Result := TDictionary<Char, TdxWordProcessingMLValue>.Create;
  Result.Add(#0, TdxWordProcessingMLValue.Create('nothing'));
  Result.Add(' ', TdxWordProcessingMLValue.Create('space'));
  Result.Add(TdxCharacters.TabMark, TdxWordProcessingMLValue.Create('tab'));
end;

function CreateMergingStateTable: TdxMLDictionary<TdxMergingState>;
begin
  Result := TdxMLDictionary<TdxMergingState>.Create;
  Result.AddValue(TdxMergingState.Restart, 'restart');
  Result.AddValue(TdxMergingState.Continue, 'continue');
end;

function CreateNumberingListTypeTable: TdxMLDictionary<TdxNumberingType>;
begin
  Result := TdxMLDictionary<TdxNumberingType>.Create;
  Result.Add(TdxNumberingType.Bullet, TdxWordProcessingMLValue.Create('hybridMultilevel', 'HybridMultilevel'));
  Result.Add(TdxNumberingType.Simple, TdxWordProcessingMLValue.Create('hybridMultilevel', 'HybridMultilevel'));
  Result.Add(TdxNumberingType.MultiLevel, TdxWordProcessingMLValue.Create('multilevel', 'Multilevel'));
end;

function CreatePageNumberingFormatTable: TdxMLDictionary<TdxNumberingFormat>;
begin
  Result := TdxMLDictionary<TdxNumberingFormat>.Create;
  Result.AddValue(TdxNumberingFormat.None, 'none');
  Result.AddValue(TdxNumberingFormat.Decimal, 'decimal');

  Result.AddValue(TdxNumberingFormat.AIUEOFullWidthHiragana, 'aiueoFullWidth');
  Result.AddValue(TdxNumberingFormat.AIUEOHiragana, 'aiueo');
  Result.AddValue(TdxNumberingFormat.ArabicAbjad, 'arabicAbjad');
  Result.AddValue(TdxNumberingFormat.ArabicAlpha, 'arabicAlpha');
  Result.AddValue(TdxNumberingFormat.Bullet, 'bullet');
  Result.AddValue(TdxNumberingFormat.CardinalText, 'cardinalText');
  Result.AddValue(TdxNumberingFormat.Chicago, 'chicago');
  Result.AddValue(TdxNumberingFormat.ChineseCounting, 'chineseCounting');
  Result.AddValue(TdxNumberingFormat.ChineseCountingThousand, 'chineseCountingThousand');
  Result.AddValue(TdxNumberingFormat.ChineseLegalSimplified, 'chineseLegalSimplified');
  Result.AddValue(TdxNumberingFormat.Chosung, 'chosung');
  Result.AddValue(TdxNumberingFormat.DecimalEnclosedCircle, 'decimalEnclosedCircle');
  Result.AddValue(TdxNumberingFormat.DecimalEnclosedCircleChinese, 'decimalEnclosedCircleChinese');
  Result.AddValue(TdxNumberingFormat.DecimalEnclosedFullstop, 'decimalEnclosedFullstop');
  Result.AddValue(TdxNumberingFormat.DecimalEnclosedParentheses, 'decimalEnclosedParen');
  Result.AddValue(TdxNumberingFormat.DecimalFullWidth, 'decimalFullWidth');
  Result.AddValue(TdxNumberingFormat.DecimalFullWidth2, 'decimalFullWidth2');
  Result.AddValue(TdxNumberingFormat.DecimalHalfWidth, 'decimalHalfWidth');
  Result.AddValue(TdxNumberingFormat.DecimalZero, 'decimalZero');
  Result.AddValue(TdxNumberingFormat.Ganada, 'ganada');
  Result.AddValue(TdxNumberingFormat.Hebrew1, 'hebrew1');
  Result.AddValue(TdxNumberingFormat.Hebrew2, 'hebrew2');
  Result.AddValue(TdxNumberingFormat.Hex, 'hex');
  Result.AddValue(TdxNumberingFormat.HindiConsonants, 'hindiConsonants');
  Result.AddValue(TdxNumberingFormat.HindiDescriptive, 'hindiCounting');
  Result.AddValue(TdxNumberingFormat.HindiNumbers, 'hindiNumbers');
  Result.AddValue(TdxNumberingFormat.HindiVowels, 'hindiVowels');
  Result.AddValue(TdxNumberingFormat.IdeographDigital, 'ideographDigital');
  Result.AddValue(TdxNumberingFormat.IdeographEnclosedCircle, 'ideographEnclosedCircle');
  Result.AddValue(TdxNumberingFormat.IdeographLegalTraditional, 'ideographLegalTraditional');
  Result.AddValue(TdxNumberingFormat.IdeographTraditional, 'ideographTraditional');
  Result.AddValue(TdxNumberingFormat.IdeographZodiac, 'ideographZodiac');
  Result.AddValue(TdxNumberingFormat.IdeographZodiacTraditional, 'ideographZodiacTraditional');
  Result.AddValue(TdxNumberingFormat.Iroha, 'iroha');
  Result.AddValue(TdxNumberingFormat.IrohaFullWidth, 'irohaFullWidth');
  Result.AddValue(TdxNumberingFormat.JapaneseCounting, 'japaneseCounting');
  Result.AddValue(TdxNumberingFormat.JapaneseDigitalTenThousand, 'japaneseDigitalTenThousand');
  Result.AddValue(TdxNumberingFormat.JapaneseLegal, 'japaneseLegal');
  Result.AddValue(TdxNumberingFormat.KoreanCounting, 'koreanCounting');
  Result.AddValue(TdxNumberingFormat.KoreanDigital, 'koreanDigital');
  Result.AddValue(TdxNumberingFormat.KoreanDigital2, 'koreanDigital2');
  Result.AddValue(TdxNumberingFormat.KoreanLegal, 'koreanLegal');
  Result.AddValue(TdxNumberingFormat.LowerLetter, 'lowerLetter');
  Result.AddValue(TdxNumberingFormat.LowerRoman, 'lowerRoman');
  Result.AddValue(TdxNumberingFormat.NumberInDash, 'numberInDash');
  Result.AddValue(TdxNumberingFormat.Ordinal, 'ordinal');
  Result.AddValue(TdxNumberingFormat.OrdinalText, 'ordinalText');
  Result.AddValue(TdxNumberingFormat.RussianLower, 'russianLower');
  Result.AddValue(TdxNumberingFormat.RussianUpper, 'russianUpper');
  Result.AddValue(TdxNumberingFormat.TaiwaneseCounting, 'taiwaneseCounting');
  Result.AddValue(TdxNumberingFormat.TaiwaneseCountingThousand, 'taiwaneseCountingThousand');
  Result.AddValue(TdxNumberingFormat.TaiwaneseDigital, 'taiwaneseDigital');
  Result.AddValue(TdxNumberingFormat.ThaiDescriptive, 'thaiCounting');
  Result.AddValue(TdxNumberingFormat.ThaiLetters, 'thaiLetters');
  Result.AddValue(TdxNumberingFormat.ThaiNumbers, 'thaiNumbers');
  Result.AddValue(TdxNumberingFormat.UpperLetter, 'upperLetter');
  Result.AddValue(TdxNumberingFormat.UpperRoman, 'upperRoman');
  Result.AddValue(TdxNumberingFormat.VietnameseDescriptive, 'vietnameseCounting');
end;

function CreateParagraphAlignmentTable: TdxMLDictionary<TdxParagraphAlignment>;
begin
  Result := TdxMLDictionary<TdxParagraphAlignment>.Create;
  Result.AddValue(TdxParagraphAlignment.Left, 'left');
  Result.AddValue(TdxParagraphAlignment.Right, 'right');
  Result.AddValue(TdxParagraphAlignment.Center, 'center');
  Result.AddValue(TdxParagraphAlignment.Justify, 'both');
end;

function CreateParagraphLineSpacingTable: TdxMLDictionary<TdxParagraphLineSpacing>;
begin
  Result := TdxMLDictionary<TdxParagraphLineSpacing>.Create;
  Result.AddValue(TdxParagraphLineSpacing.Single, 'auto');
  Result.AddValue(TdxParagraphLineSpacing.Double, 'auto');
  Result.AddValue(TdxParagraphLineSpacing.Sesquialteral, 'auto');
  Result.AddValue(TdxParagraphLineSpacing.Multiple, 'auto');
  Result.AddValue(TdxParagraphLineSpacing.Exactly, 'exact');
  Result.AddValue(TdxParagraphLineSpacing.AtLeast, 'atLeast');
end;

function CreatePredefinedBackgroundColors: TDictionary<TdxAlphaColor, TdxWordProcessingMLValue>;
begin
  Result := TDictionary<TdxAlphaColor, TdxWordProcessingMLValue>.Create;
  Result.Add(TdxAlphaColors.Empty, TdxWordProcessingMLValue.Create('none'));
  Result.Add(TdxAlphaColors.Black, TdxWordProcessingMLValue.Create('black'));
  Result.Add(TdxAlphaColors.FromArgb($00, $00, $FF), TdxWordProcessingMLValue.Create('blue'));
  Result.Add(TdxAlphaColors.FromArgb($00, $FF, $FF), TdxWordProcessingMLValue.Create('cyan'));
  Result.Add(TdxAlphaColors.FromArgb($00, $00, $80), TdxWordProcessingMLValue.Create('darkBlue'));
  Result.Add(TdxAlphaColors.FromArgb($00, $80, $80), TdxWordProcessingMLValue.Create('darkCyan'));
  Result.Add(TdxAlphaColors.FromArgb($80, $80, $80), TdxWordProcessingMLValue.Create('darkGray'));
  Result.Add(TdxAlphaColors.FromArgb($00, $80, $00), TdxWordProcessingMLValue.Create('darkGreen'));
  Result.Add(TdxAlphaColors.FromArgb($80, $00, $80), TdxWordProcessingMLValue.Create('darkMagenta'));
  Result.Add(TdxAlphaColors.FromArgb($80, $00, $00), TdxWordProcessingMLValue.Create('darkRed'));
  Result.Add(TdxAlphaColors.FromArgb($80, $80, $00), TdxWordProcessingMLValue.Create('darkYellow'));
  Result.Add(TdxAlphaColors.FromArgb($00, $FF, $00), TdxWordProcessingMLValue.Create('green'));
  Result.Add(TdxAlphaColors.FromArgb($C0, $C0, $C0), TdxWordProcessingMLValue.Create('lightGray'));
  Result.Add(TdxAlphaColors.FromArgb($FF, $00, $FF), TdxWordProcessingMLValue.Create('magenta'));
  Result.Add(TdxAlphaColors.FromArgb($FF, $00, $00), TdxWordProcessingMLValue.Create('red'));
  Result.Add(TdxAlphaColors.FromArgb($FF, $FF, $FF), TdxWordProcessingMLValue.Create('white'));
  Result.Add(TdxAlphaColors.FromArgb($FF, $FF, $00), TdxWordProcessingMLValue.Create('yellow'));
end;

function CreatePresetColors: TdxStringColorDictionary;
begin
  Result := TdxStringColorDictionary.Create;
  Result.Add('aliceBlue', TdxAlphaColors.FromArgb(240, 248, 255));
  Result.Add('antiqueWhite', TdxAlphaColors.FromArgb(250, 235, 215));
  Result.Add('aqua', TdxAlphaColors.FromArgb(0, 255, 255));
  Result.Add('aquamarine', TdxAlphaColors.FromArgb(127, 255, 212));
  Result.Add('azure', TdxAlphaColors.FromArgb(240, 255, 255));
  Result.Add('beige', TdxAlphaColors.FromArgb(245, 245, 220));
  Result.Add('bisque', TdxAlphaColors.FromArgb(255, 228, 196));
  Result.Add('black', TdxAlphaColors.FromArgb(0, 0, 0));
  Result.Add('blanchedAlmond', TdxAlphaColors.FromArgb(255, 235, 205));
  Result.Add('blue', TdxAlphaColors.FromArgb(0, 0, 255));
  Result.Add('blueViolet', TdxAlphaColors.FromArgb(138, 43, 226));
  Result.Add('brown', TdxAlphaColors.FromArgb(165, 42, 42));
  Result.Add('burlyWood', TdxAlphaColors.FromArgb(222, 184, 135));
  Result.Add('cadetBlue', TdxAlphaColors.FromArgb(95, 158, 160));
  Result.Add('chartreuse', TdxAlphaColors.FromArgb(127, 255, 0));
  Result.Add('chocolate', TdxAlphaColors.FromArgb(210, 105, 30));
  Result.Add('coral', TdxAlphaColors.FromArgb(255, 127, 80));
  Result.Add('cornflowerBlue', TdxAlphaColors.FromArgb(100, 149, 237));
  Result.Add('cornsilk', TdxAlphaColors.FromArgb(255, 248, 220));
  Result.Add('crimson', TdxAlphaColors.FromArgb(220, 20, 60));
  Result.Add('cyan', TdxAlphaColors.FromArgb(0, 255, 255));
  Result.Add('deepPink', TdxAlphaColors.FromArgb(255, 20, 147));
  Result.Add('deepSkyBlue', TdxAlphaColors.FromArgb(0, 191, 255));
  Result.Add('dimGray', TdxAlphaColors.FromArgb(105, 105, 105));
  Result.Add('dkBlue', TdxAlphaColors.FromArgb(0, 0, 139));
  Result.Add('dkCyan', TdxAlphaColors.FromArgb(0, 139, 139));
  Result.Add('dkGoldenrod', TdxAlphaColors.FromArgb(184, 134, 11));
  Result.Add('dkGray', TdxAlphaColors.FromArgb(169, 169, 169));
  Result.Add('dkGreen', TdxAlphaColors.FromArgb(0, 100, 0));
  Result.Add('dkKhaki', TdxAlphaColors.FromArgb(189, 183, 107));
  Result.Add('dkMagenta', TdxAlphaColors.FromArgb(139, 0, 139));
  Result.Add('dkOliveGreen', TdxAlphaColors.FromArgb(85, 107, 47));
  Result.Add('dkOrange', TdxAlphaColors.FromArgb(255, 140, 0));
  Result.Add('dkOrchid', TdxAlphaColors.FromArgb(153, 50, 204));
  Result.Add('dkRed', TdxAlphaColors.FromArgb(139, 0, 0));
  Result.Add('dkSalmon', TdxAlphaColors.FromArgb(233, 150, 122));
  Result.Add('dkSeaGreen', TdxAlphaColors.FromArgb(143, 188, 139));
  Result.Add('dkSlateBlue', TdxAlphaColors.FromArgb(72, 61, 139));
  Result.Add('dkSlateGray', TdxAlphaColors.FromArgb(47, 79, 79));
  Result.Add('dkTurquoise', TdxAlphaColors.FromArgb(0, 206, 209));
  Result.Add('dkViolet', TdxAlphaColors.FromArgb(148, 0, 211));
  Result.Add('dodgerBlue', TdxAlphaColors.FromArgb(30, 144, 255));
  Result.Add('firebrick', TdxAlphaColors.FromArgb(178, 34, 34));
  Result.Add('floralWhite', TdxAlphaColors.FromArgb(255, 250, 240));
  Result.Add('forestGreen', TdxAlphaColors.FromArgb(34, 139, 34));
  Result.Add('fuchsia', TdxAlphaColors.FromArgb(255, 0, 255));
  Result.Add('gainsboro', TdxAlphaColors.FromArgb(220, 220, 220));
  Result.Add('ghostWhite', TdxAlphaColors.FromArgb(248, 248, 255));
  Result.Add('gold', TdxAlphaColors.FromArgb(255, 215, 0));
  Result.Add('goldenrod', TdxAlphaColors.FromArgb(218, 165, 32));
  Result.Add('gray', TdxAlphaColors.FromArgb(128, 128, 128));
  Result.Add('green', TdxAlphaColors.FromArgb(0, 128, 0));
  Result.Add('greenYellow', TdxAlphaColors.FromArgb(173, 255, 47));
  Result.Add('honeydew', TdxAlphaColors.FromArgb(240, 255, 240));
  Result.Add('hotPink', TdxAlphaColors.FromArgb(255, 105, 180));
  Result.Add('indianRed', TdxAlphaColors.FromArgb(205, 92, 92));
  Result.Add('indigo', TdxAlphaColors.FromArgb(75, 0, 130));
  Result.Add('ivory', TdxAlphaColors.FromArgb(255, 255, 240));
  Result.Add('khaki', TdxAlphaColors.FromArgb(240, 230, 140));
  Result.Add('lavender', TdxAlphaColors.FromArgb(230, 230, 250));
  Result.Add('lavenderBlush', TdxAlphaColors.FromArgb(255, 240, 245));
  Result.Add('lawnGreen', TdxAlphaColors.FromArgb(124, 252, 0));
  Result.Add('lemonChiffon', TdxAlphaColors.FromArgb(255, 250, 205));
  Result.Add('lime', TdxAlphaColors.FromArgb(0, 255, 0));
  Result.Add('limeGreen', TdxAlphaColors.FromArgb(50, 205, 50));
  Result.Add('linen', TdxAlphaColors.FromArgb(250, 240, 230));
  Result.Add('ltBlue', TdxAlphaColors.FromArgb(173, 216, 230));
  Result.Add('ltCoral', TdxAlphaColors.FromArgb(240, 128, 128));
  Result.Add('ltCyan', TdxAlphaColors.FromArgb(224, 255, 255));
  Result.Add('ltGoldenrodYellow', TdxAlphaColors.FromArgb(250, 250, 120));
  Result.Add('ltGray', TdxAlphaColors.FromArgb(211, 211, 211));
  Result.Add('ltGreen', TdxAlphaColors.FromArgb(144, 238, 144));
  Result.Add('ltPink', TdxAlphaColors.FromArgb(255, 182, 193));
  Result.Add('ltSalmon', TdxAlphaColors.FromArgb(255, 160, 122));
  Result.Add('ltSeaGreen', TdxAlphaColors.FromArgb(32, 178, 170));
  Result.Add('ltSkyBlue', TdxAlphaColors.FromArgb(135, 206, 250));
  Result.Add('ltSlateGray', TdxAlphaColors.FromArgb(119, 136, 153));
  Result.Add('ltSteelBlue', TdxAlphaColors.FromArgb(176, 196, 222));
  Result.Add('ltYellow', TdxAlphaColors.FromArgb(255, 255, 224));
  Result.Add('magenta', TdxAlphaColors.FromArgb(255, 0, 255));
  Result.Add('maroon', TdxAlphaColors.FromArgb(128, 0, 0));
  Result.Add('medAquamarine', TdxAlphaColors.FromArgb(102, 205, 170));
  Result.Add('medBlue', TdxAlphaColors.FromArgb(0, 0, 205));
  Result.Add('medOrchid', TdxAlphaColors.FromArgb(186, 85, 211));
  Result.Add('medPurple', TdxAlphaColors.FromArgb(147, 112, 219));
  Result.Add('medSeaGreen', TdxAlphaColors.FromArgb(60, 179, 113));
  Result.Add('medSlateBlue', TdxAlphaColors.FromArgb(123, 104, 238));
  Result.Add('medSpringGreen', TdxAlphaColors.FromArgb(0, 250, 154));
  Result.Add('medTurquoise', TdxAlphaColors.FromArgb(72, 209, 204));
  Result.Add('medVioletRed', TdxAlphaColors.FromArgb(199, 21, 133));
  Result.Add('midnightBlue', TdxAlphaColors.FromArgb(25, 25, 112));
  Result.Add('mintCream', TdxAlphaColors.FromArgb(245, 255, 250));
  Result.Add('mistyRose', TdxAlphaColors.FromArgb(255, 228, 225));
  Result.Add('moccasin', TdxAlphaColors.FromArgb(255, 228, 181));
  Result.Add('navajoWhite', TdxAlphaColors.FromArgb(255, 222, 173));
  Result.Add('navy', TdxAlphaColors.FromArgb(0, 0, 128));
  Result.Add('oldLace', TdxAlphaColors.FromArgb(253, 245, 230));
  Result.Add('olive', TdxAlphaColors.FromArgb(128, 128, 0));
  Result.Add('oliveDrab', TdxAlphaColors.FromArgb(107, 142, 35));
  Result.Add('orange', TdxAlphaColors.FromArgb(255, 165, 0));
  Result.Add('orangeRed', TdxAlphaColors.FromArgb(255, 69, 0));
  Result.Add('orchid', TdxAlphaColors.FromArgb(218, 112, 214));
  Result.Add('paleGoldenrod', TdxAlphaColors.FromArgb(238, 232, 170));
  Result.Add('paleGreen', TdxAlphaColors.FromArgb(152, 251, 152));
  Result.Add('paleTurquoise', TdxAlphaColors.FromArgb(175, 238, 238));
  Result.Add('paleVioletRed', TdxAlphaColors.FromArgb(219, 112, 147));
  Result.Add('papayaWhip', TdxAlphaColors.FromArgb(255, 239, 213));
  Result.Add('peachPuff', TdxAlphaColors.FromArgb(255, 218, 185));
  Result.Add('peru', TdxAlphaColors.FromArgb(205, 133, 63));
  Result.Add('pink', TdxAlphaColors.FromArgb(255, 192, 203));
  Result.Add('plum', TdxAlphaColors.FromArgb(221, 160, 221));
  Result.Add('powderBlue', TdxAlphaColors.FromArgb(176, 224, 230));
  Result.Add('purple', TdxAlphaColors.FromArgb(128, 0, 128));
  Result.Add('red', TdxAlphaColors.FromArgb(255, 0, 0));
  Result.Add('rosyBrown', TdxAlphaColors.FromArgb(188, 143, 143));
  Result.Add('royalBlue', TdxAlphaColors.FromArgb(65, 105, 225));
  Result.Add('saddleBrown', TdxAlphaColors.FromArgb(139, 69, 19));
  Result.Add('salmon', TdxAlphaColors.FromArgb(250, 128, 114));
  Result.Add('sandyBrown', TdxAlphaColors.FromArgb(244, 164, 96));
  Result.Add('seaGreen', TdxAlphaColors.FromArgb(46, 139, 87));
  Result.Add('seaShell', TdxAlphaColors.FromArgb(255, 245, 238));
  Result.Add('sienna', TdxAlphaColors.FromArgb(160, 82, 45));
  Result.Add('silver', TdxAlphaColors.FromArgb(192, 192, 192));
  Result.Add('skyBlue', TdxAlphaColors.FromArgb(135, 206, 235));
  Result.Add('slateBlue', TdxAlphaColors.FromArgb(106, 90, 205));
  Result.Add('slateGray', TdxAlphaColors.FromArgb(112, 128, 144));
  Result.Add('snow', TdxAlphaColors.FromArgb(255, 250, 250));
  Result.Add('springGreen', TdxAlphaColors.FromArgb(0, 255, 127));
  Result.Add('steelBlue', TdxAlphaColors.FromArgb(70, 130, 180));
  Result.Add('tan', TdxAlphaColors.FromArgb(210, 180, 140));
  Result.Add('teal', TdxAlphaColors.FromArgb(0, 128, 128));
  Result.Add('thistle', TdxAlphaColors.FromArgb(216, 191, 216));
  Result.Add('tomato', TdxAlphaColors.FromArgb(255, 99, 71));
  Result.Add('turquoise', TdxAlphaColors.FromArgb(64, 224, 208));
  Result.Add('violet', TdxAlphaColors.FromArgb(238, 130, 238));
  Result.Add('wheat', TdxAlphaColors.FromArgb(245, 222, 179));
  Result.Add('white', TdxAlphaColors.FromArgb(255, 255, 255));
  Result.Add('whiteSmoke', TdxAlphaColors.FromArgb(245, 245, 245));
  Result.Add('yellow', TdxAlphaColors.FromArgb(255, 255, 0));
  Result.Add('yellowGreen', TdxAlphaColors.FromArgb(154, 205, 50));
end;

function CreateRunBreaksTable: TDictionary<Char, TdxWordProcessingMLValue>;
begin
  Result := TDictionary<Char, TdxWordProcessingMLValue>.Create;
  Result.Add(TdxCharacters.LineBreak, TdxWordProcessingMLValue.Create('textWrapping'));
  Result.Add(TdxCharacters.PageBreak, TdxWordProcessingMLValue.Create('page'));
  Result.Add(TdxCharacters.ColumnBreak, TdxWordProcessingMLValue.Create('column'));
end;

function CreateSectionStartTypeTable: TdxMLDictionary<TdxSectionStartType>;
begin
  Result := TdxMLDictionary<TdxSectionStartType>.Create;
  Result.AddValue(TdxSectionStartType.NextPage, 'nextPage');
  Result.AddValue(TdxSectionStartType.OddPage, 'oddPage');
  Result.AddValue(TdxSectionStartType.EvenPage, 'evenPage');
  Result.AddValue(TdxSectionStartType.Column, 'nextColumn');
  Result.AddValue(TdxSectionStartType.Continuous, 'continuous');
end;

function CreateShadingPatternTable: TdxMLDictionary<TdxShadingPattern>;
begin
  Result := TdxMLDictionary<TdxShadingPattern>.Create;
  Result.AddValue(TdxShadingPattern.Clear, 'clear');
  Result.AddValue(TdxShadingPattern.DiagCross, 'diagCross');
  Result.AddValue(TdxShadingPattern.DiagStripe, 'diagStripe');
  Result.AddValue(TdxShadingPattern.HorzCross, 'horzCross');
  Result.AddValue(TdxShadingPattern.HorzStripe, 'horzStripe');
  Result.AddValue(TdxShadingPattern.Nil, 'nil');
  Result.AddValue(TdxShadingPattern.Pct10, 'pct10');
  Result.AddValue(TdxShadingPattern.Pct12, 'pct12');
  Result.AddValue(TdxShadingPattern.Pct15, 'pct15');
  Result.AddValue(TdxShadingPattern.Pct20, 'pct20');
  Result.AddValue(TdxShadingPattern.Pct25, 'pct25');
  Result.AddValue(TdxShadingPattern.Pct30, 'pct30');
  Result.AddValue(TdxShadingPattern.Pct35, 'pct35');
  Result.AddValue(TdxShadingPattern.Pct37, 'pct37');
  Result.AddValue(TdxShadingPattern.Pct40, 'pct40');
  Result.AddValue(TdxShadingPattern.Pct45, 'pct45');
  Result.AddValue(TdxShadingPattern.Pct5, 'pct5');
  Result.AddValue(TdxShadingPattern.Pct50, 'pct50');
  Result.AddValue(TdxShadingPattern.Pct55, 'pct55');
  Result.AddValue(TdxShadingPattern.Pct60, 'pct60');
  Result.AddValue(TdxShadingPattern.Pct62, 'pct62');
  Result.AddValue(TdxShadingPattern.Pct65, 'pct65');
  Result.AddValue(TdxShadingPattern.Pct70, 'pct70');
  Result.AddValue(TdxShadingPattern.Pct75, 'pct75');
  Result.AddValue(TdxShadingPattern.Pct80, 'pct80');
  Result.AddValue(TdxShadingPattern.Pct85, 'pct85');
  Result.AddValue(TdxShadingPattern.Pct87, 'pct87');
  Result.AddValue(TdxShadingPattern.Pct90, 'pct90');
  Result.AddValue(TdxShadingPattern.Pct95, 'pct95');
  Result.AddValue(TdxShadingPattern.ReverseDiagStripe, 'reverseDiagStripe');
  Result.AddValue(TdxShadingPattern.Solid, 'solid');
  Result.AddValue(TdxShadingPattern.ThinDiagCross, 'thinDiagCross');
  Result.AddValue(TdxShadingPattern.ThinDiagStripe, 'ThinDiagStripe');
  Result.AddValue(TdxShadingPattern.ThinHorzCross, 'thinHorzCross');
  Result.AddValue(TdxShadingPattern.ThinHorzStripe, 'thinHorzStripe');
  Result.AddValue(TdxShadingPattern.ThinReverseDiagStripe, 'thinReverseDiagStripe');
  Result.AddValue(TdxShadingPattern.ThinVertStripe, 'thinVertStripe');
  Result.AddValue(TdxShadingPattern.VertStripe, 'vertStripe');
end;

function CreateTabAlignmentTable: TdxMLDictionary<TdxTabAlignmentType>;
begin
  Result := TdxMLDictionary<TdxTabAlignmentType>.Create;
  Result.AddValue(TdxTabAlignmentType.Left, 'left');
  Result.AddValue(TdxTabAlignmentType.Right, 'right');
  Result.AddValue(TdxTabAlignmentType.Center, 'center');
  Result.AddValue(TdxTabAlignmentType.Decimal, 'decimal');
end;

function CreateTabLeaderTable: TdxMLDictionary<TdxTabLeaderType>;
begin
  Result := TdxMLDictionary<TdxTabLeaderType>.Create;
  Result.AddValue(TdxTabLeaderType.None, 'none');
  Result.AddValue(TdxTabLeaderType.Dots, 'dot');
  Result.AddValue(TdxTabLeaderType.Hyphens, 'hyphen');
  Result.AddValue(TdxTabLeaderType.EqualSign, 'hyphen');
  Result.AddValue(TdxTabLeaderType.MiddleDots, 'middleDot');
  Result.AddValue(TdxTabLeaderType.ThickLine, 'heavy');
  Result.AddValue(TdxTabLeaderType.Underline, 'underscore');
end;

function CreateTableLayoutTypeTable: TdxMLDictionary<TdxTableLayoutType>;
begin
  Result := TdxMLDictionary<TdxTableLayoutType>.Create;
  Result.AddValue(TdxTableLayoutType.Autofit, 'autofit');
  Result.AddValue(TdxTableLayoutType.Fixed, 'fixed');
end;

function CreateTableRowAlignmentTable: TdxMLDictionary<TdxTableRowAlignment>;
begin
  Result := TdxMLDictionary<TdxTableRowAlignment>.Create;
  Result.AddValue(TdxTableRowAlignment.Both, 'both');
  Result.AddValue(TdxTableRowAlignment.Center, 'center');
  Result.AddValue(TdxTableRowAlignment.Distribute, 'distribute');
  Result.AddValue(TdxTableRowAlignment.Left, 'left');
  Result.AddValue(TdxTableRowAlignment.NumTab, 'numTab');
  Result.AddValue(TdxTableRowAlignment.Right, 'right');
end;

function CreateTextBoxVerticalAlignmentTable: TdxMLDictionary<TdxVerticalAlignment>;
begin
  Result := TdxMLDictionary<TdxVerticalAlignment>.Create;
  Result.Add(TdxVerticalAlignment.Top, TdxWordProcessingMLValue.Create('t', 'top'));
  Result.Add(TdxVerticalAlignment.Center, TdxWordProcessingMLValue.Create('ctr', 'middle'));
  Result.Add(TdxVerticalAlignment.Both, TdxWordProcessingMLValue.Create('just'));
  Result.Add(TdxVerticalAlignment.Bottom, TdxWordProcessingMLValue.Create('b', 'bottom'));
end;

function CreateTextDirectionTable: TdxMLDictionary<TdxTextDirection>;
begin
  Result := TdxMLDictionary<TdxTextDirection>.Create;
  Result.AddValue(TdxTextDirection.LeftToRightTopToBottom, 'lrTb');
  Result.AddValue(TdxTextDirection.LeftToRightTopToBottomRotated, 'lrTbV');
  Result.AddValue(TdxTextDirection.BottomToTopLeftToRight, 'btLr');
  Result.AddValue(TdxTextDirection.TopToBottomLeftToRightRotated, 'tbLrV');
  Result.AddValue(TdxTextDirection.TopToBottomRightToLeft, 'tbRl');
  Result.AddValue(TdxTextDirection.TopToBottomRightToLeftRotated, 'tbRlV');
end;

function CreateUnderlineTable: TdxMLDictionary<TdxUnderlineType>;
begin
  Result := TdxMLDictionary<TdxUnderlineType>.Create;
  Result.AddValue(TdxUnderlineType.None, 'none');
  Result.AddValue(TdxUnderlineType.Single, 'single');
  Result.AddValue(TdxUnderlineType.Double, 'double');
  Result.AddValue(TdxUnderlineType.Dotted, 'dotted');
  Result.AddValue(TdxUnderlineType.Dashed, 'dash');
  Result.AddValue(TdxUnderlineType.LongDashed, 'dashLong');
  Result.AddValue(TdxUnderlineType.DashDotted, 'dotDash');
  Result.AddValue(TdxUnderlineType.DashDotDotted, 'dotDotDash');
  Result.AddValue(TdxUnderlineType.DoubleWave, 'wavyDouble');
  Result.AddValue(TdxUnderlineType.HeavyWave, 'wavyHeavy');
  Result.AddValue(TdxUnderlineType.ThickDashDotDotted, 'dashDotDotHeavy');
  Result.AddValue(TdxUnderlineType.ThickDashDotted, 'dashDotHeavy');
  Result.AddValue(TdxUnderlineType.ThickDashed, 'dashedHeavy');
  Result.AddValue(TdxUnderlineType.ThickDotted, 'dottedHeavy');
  Result.AddValue(TdxUnderlineType.ThickLongDashed, 'dashLongHeavy');
  Result.AddValue(TdxUnderlineType.ThickSingle, 'thick');
  Result.AddValue(TdxUnderlineType.Wave, 'wave');
end;

function CreateVerticalAlignmentTable: TdxMLDictionary<TdxVerticalAlignment>;
begin
  Result := TdxMLDictionary<TdxVerticalAlignment>.Create;
  Result.AddValue(TdxVerticalAlignment.Top, 'top');
  Result.AddValue(TdxVerticalAlignment.Center, 'center');
  Result.AddValue(TdxVerticalAlignment.Bottom, 'bottom');
  Result.AddValue(TdxVerticalAlignment.Both, 'both');
end;

function CreateVerticalAlignModeTable: TdxMLDictionary<TdxVerticalAlignMode>;
begin
  Result := TdxMLDictionary<TdxVerticalAlignMode>.Create;
  Result.AddValue(TdxVerticalAlignMode.Bottom, 'bottom');
  Result.AddValue(TdxVerticalAlignMode.Center, 'center');
  Result.AddValue(TdxVerticalAlignMode.Inline, 'inline');
  Result.AddValue(TdxVerticalAlignMode.Inside, 'inside');
  Result.AddValue(TdxVerticalAlignMode.Outside, 'outside');
  Result.AddValue(TdxVerticalAlignMode.Top, 'top');
end;

function CreateVerticalAnchorTypesTable: TdxMLDictionary<TdxVerticalAnchorTypes>;
begin
  Result := TdxMLDictionary<TdxVerticalAnchorTypes>.Create;
  Result.AddValue(TdxVerticalAnchorTypes.Paragraph, 'text');
  Result.AddValue(TdxVerticalAnchorTypes.Margin, 'margin');
  Result.AddValue(TdxVerticalAnchorTypes.Page, 'page');
end;

function CreateWidthUnitTypesTable: TdxMLDictionary<TdxWidthUnitType>;
begin
  Result := TdxMLDictionary<TdxWidthUnitType>.Create;
  Result.AddValue(TdxWidthUnitType.Auto, 'auto');
  Result.AddValue(TdxWidthUnitType.FiftiethsOfPercent, 'pct');
  Result.AddValue(TdxWidthUnitType.ModelUnits, 'dxa');
  Result.AddValue(TdxWidthUnitType.&Nil, 'nil');
end;

function CreatePredefinedGroupNames: TdxStringsDictionary;
begin
  Result := TdxStringsDictionary.Create;
  Result.Add('Everyone', 'everyone');
  Result.Add('Current User', 'current');
  Result.Add('Editors', 'editors');
  Result.Add('Owners', 'owners');
  Result.Add('Contributors', 'contributors');
  Result.Add('Administrators', 'administrators');
end;

class constructor TdxWordProcessingMLBaseExporter.Initialize;
begin
  FHorizontalAnchorTypesTable := CreateHorizontalAnchorTypesTable;
  FBorderLineStyleTable := CreateBorderLineStyleTable;
  FChapterSeparatorsTable := CreateChapterSeparatorsTable;
  FConditionalTableStyleFormattingTypesTable := CreateConditionalTableStyleFormattingTypesTable;
  FContentTypeTable := CreateContentTypeTable;
  FFloatingObjectCssRelativeFromHorizontalTable := CreateFloatingObjectCssRelativeFromHorizontalTable;
  FFloatingObjectCssRelativeFromVerticalTable := CreateFloatingObjectCssRelativeFromVerticalTable;
  FFloatingObjectHorizontalPositionAlignmentTable := CreateFloatingObjectHorizontalPositionAlignmentTable;
  FFloatingObjectHorizontalPositionTypeTable := CreateFloatingObjectHorizontalPositionTypeTable;
  FFloatingObjectRelativeFromHorizontalTable := CreateFloatingObjectRelativeFromHorizontalTable;
  FFloatingObjectRelativeFromVerticalTable := CreateFloatingObjectRelativeFromVerticalTable;
  FFloatingObjectTextWrapSideTable := CreateFloatingObjectTextWrapSideTable;
  FFloatingObjectTextWrapTypeTable := CreateFloatingObjectTextWrapTypeTable;
  FFloatingObjectVerticalPositionAlignmentTable := CreateFloatingObjectVerticalPositionAlignmentTable;
  FFloatingObjectVerticalPositionTypeTable := CreateFloatingObjectVerticalPositionTypeTable;
  FFootNotePlacementTable := CreateFootNotePlacementTable;
  FHeightUnitTypeTable := CreateHeightUnitTypeTable;
  FHorizontalAlignModeTable := CreateHorizontalAlignModeTable;
  FLineNumberingRestartTable := CreateLineNumberingRestartTable;
  FLineSpacingTable := CreateParagraphLineSpacingTable;
  FListNumberAlignmentTable := CreateListNumberAlignmentTable;
  FListNumberSeparatorTable := CreateListNumberSeparatorTable;
  FMergingStateTable := CreateMergingStateTable;
  FNumberingListTypeTable := CreateNumberingListTypeTable;
  FPageNumberingFormatTable := CreatePageNumberingFormatTable;
  FParagraphAlignmentTable := CreateParagraphAlignmentTable;
  FPredefinedBackgroundColors := CreatePredefinedBackgroundColors;
  FPredefinedGroupNames := CreatePredefinedGroupNames;
  FPresetColors := CreatePresetColors;
  FRunBreaksTable := CreateRunBreaksTable;
  FSectionStartTypeTable := CreateSectionStartTypeTable;
  FShadingPatternTable := CreateShadingPatternTable;
  FTabAlignmentTable := CreateTabAlignmentTable;
  FTabLeaderTable := CreateTabLeaderTable;
  FTableLayoutTypeTable := CreateTableLayoutTypeTable;
  FTableRowAlignmentTable := CreateTableRowAlignmentTable;
  FTextBoxVerticalAlignmentTable := CreateTextBoxVerticalAlignmentTable;
  FTextDirectionTable := CreateTextDirectionTable;
  FUnderlineTable := CreateUnderlineTable;
  FVerticalAlignmentTable := CreateVerticalAlignmentTable;
  FVerticalAlignModeTable := CreateVerticalAlignModeTable;
  FVerticalAnchorTypesTable := CreateVerticalAnchorTypesTable;
  FWidthUnitTypesTable := CreateWidthUnitTypesTable;
end;

class destructor TdxWordProcessingMLBaseExporter.Finalize;
begin
  FreeAndNil(FPredefinedBackgroundColors);
  FreeAndNil(FPresetColors);
  FreeAndNil(FUnderlineTable);
  FreeAndNil(FRunBreaksTable);
  FreeAndNil(FParagraphAlignmentTable);
  FreeAndNil(FLineSpacingTable);
  FreeAndNil(FTabAlignmentTable);
  FreeAndNil(FTabLeaderTable);
  FreeAndNil(FTextDirectionTable);
  FreeAndNil(FShadingPatternTable);
  FreeAndNil(FVerticalAlignmentTable);
  FreeAndNil(FSectionStartTypeTable);
  FreeAndNil(FLineNumberingRestartTable);
  FreeAndNil(FPageNumberingFormatTable);
  FreeAndNil(FChapterSeparatorsTable);
  FreeAndNil(FNumberingListTypeTable);
  FreeAndNil(FListNumberAlignmentTable);
  FreeAndNil(FListNumberSeparatorTable);
  FreeAndNil(FContentTypeTable);
  FreeAndNil(FWidthUnitTypesTable);
  FreeAndNil(FTableLayoutTypeTable);
  FreeAndNil(FBorderLineStyleTable);
  FreeAndNil(FHorizontalAlignModeTable);
  FreeAndNil(FHorizontalAnchorTypesTable);
  FreeAndNil(FVerticalAlignModeTable);
  FreeAndNil(FVerticalAnchorTypesTable);
  FreeAndNil(FHeightUnitTypeTable);
  FreeAndNil(FMergingStateTable);
  FreeAndNil(FTableRowAlignmentTable);
  FreeAndNil(FFootNotePlacementTable);
  FreeAndNil(FFloatingObjectHorizontalPositionAlignmentTable);
  FreeAndNil(FFloatingObjectVerticalPositionAlignmentTable);
  FreeAndNil(FFloatingObjectHorizontalPositionTypeTable);
  FreeAndNil(FFloatingObjectCssRelativeFromHorizontalTable);
  FreeAndNil(FFloatingObjectCssRelativeFromVerticalTable);
  FreeAndNil(FFloatingObjectRelativeFromHorizontalTable);
  FreeAndNil(FFloatingObjectRelativeFromVerticalTable);
  FreeAndNil(FFloatingObjectVerticalPositionTypeTable);
  FreeAndNil(FFloatingObjectTextWrapSideTable);
  FreeAndNil(FFloatingObjectTextWrapTypeTable);
  FreeAndNil(FTextBoxVerticalAlignmentTable);
  FreeAndNil(FConditionalTableStyleFormattingTypesTable);
  FreeAndNil(FPredefinedGroupNames);
end;


function TdxWordProcessingMLBaseExporter.GetUnitConverter: TdxDocumentModelUnitConverter;
begin
  Result := DocumentModel.UnitConverter;
end;

function TdxWordProcessingMLBaseExporter.GetPackage: TdxInternalZipArchive;
begin
  Result := nil;
end;

procedure TdxWordProcessingMLBaseExporter.WriteWpBoolValue(const ATag: string; AValue: Boolean);
begin
  WriteWpStringValue(ATag, ConvertBoolToString(AValue));
end;

procedure TdxWordProcessingMLBaseExporter.WriteWpBoolValueAsTag(const ATag: string; AValue: Boolean);
begin
  if AValue then
  begin
    WriteWpStartElement(ATag);
    WriteWpEndElement;
  end;
end;

procedure TdxWordProcessingMLBaseExporter.WriteBoolValue(const ATag: string; AValue: Boolean);
begin
  WriteStringAttr('', ATag, '', ConvertBoolToString(AValue));
end;

procedure TdxWordProcessingMLBaseExporter.WriteWpIntValue(const ATag: string; AValue: Integer);
begin
  WriteWpStringValue(ATag, IntToStr(AValue));
end;

procedure TdxWordProcessingMLBaseExporter.WriteIntValue(const ATag: string; AValue: Integer);
begin
  WriteStringAttr('', ATag, '', IntToStr(AValue));
end;

procedure TdxWordProcessingMLBaseExporter.WriteWpStringValue(const ATag: string; const AValue: string);
begin
  WriteStringValue(WordProcessingPrefix, ATag, AValue);
end;

procedure TdxWordProcessingMLBaseExporter.WriteStringValue(const ATag: string; const AValue: string);
begin
  WriteStringAttr('', ATag, '', AValue);
end;

procedure TdxWordProcessingMLBaseExporter.WriteWpBoolAttr(const AAttr: string; AValue: Boolean);
begin
  WriteWpStringAttr(AAttr, ConvertBoolToString(AValue));
end;

procedure TdxWordProcessingMLBaseExporter.WriteWpIntAttr(const AAttr: string; AValue: Integer);
begin
  WriteWpStringAttr(AAttr, IntToStr(AValue));
end;

procedure TdxWordProcessingMLBaseExporter.WriteWpStringAttr(const AAttr: string; const AValue: string);
begin
  FDocumentContentWriter.WriteAttributeString(WordProcessingPrefix, AAttr, WordProcessingNamespace, AValue);
end;

procedure TdxWordProcessingMLBaseExporter.WriteStringAttr(const APrefix: string; const AAttr: string; const ANs: string; const AValue: string);
begin
  FDocumentContentWriter.WriteAttributeString(APrefix, AAttr, ANs, AValue);
end;

procedure TdxWordProcessingMLBaseExporter.WriteWpStartElement(const ATag: string);
begin
  FDocumentContentWriter.WriteStartElement(WordProcessingPrefix, ATag, WordProcessingNamespace);
end;

procedure TdxWordProcessingMLBaseExporter.WriteWpEndElement;
begin
  FDocumentContentWriter.WriteEndElement;
end;

procedure TdxWordProcessingMLBaseExporter.WriteStringValue(const APrefix: string; const ATag: string; const AValue: string);
begin
  FDocumentContentWriter.WriteStartElement(APrefix, ATag, WordProcessingNamespace);
  try
    FDocumentContentWriter.WriteAttributeString(APrefix, 'val', WordProcessingNamespace, AValue);
  finally
    FDocumentContentWriter.WriteEndElement;
  end;
end;

procedure TdxWordProcessingMLBaseExporter.WriteWpEmptyElement(const ATag: string);
begin
  WriteWpStartElement(ATag);
  WriteWpEndElement;
end;

procedure TdxWordProcessingMLBaseExporter.WriteWpEmptyOrFalseValue(const ATag: string; AValue: Boolean);
begin
  if AValue then
    WriteWpEmptyElement(ATag)
  else
    WriteWpBoolValue(ATag, False);
end;

procedure TdxWordProcessingMLBaseExporter.ExportTextRun(ARun: TdxTextRun);
var
  ARunText: string;
begin
  ARunText := ARun.GetPlainText(PieceTable.TextBuffer);
  WriteWpStartElement('r');
  try
    ExportRunProperties(ARun);
    ExportTextRunCore(ARun, ARunText);
  finally
    WriteWpEndElement;
  end;
end;

function TdxWordProcessingMLBaseExporter.ShouldPreserveSpace(const AText: string): Boolean;
begin
  Result := (Length(AText) > 0) and
    ((AText[1] = ' ') or (AText[Length(AText)] = ' ') or (Pos('  ', AText) > 0));
end;

function TdxWordProcessingMLBaseExporter.ShouldExportRunProperties(ARun: TdxTextRunBase): Boolean;
var
  ACharacterProperties: TdxCharacterProperties;
begin
  ACharacterProperties := ARun.CharacterProperties;
  Result :=
    ACharacterProperties.UseFontName or
    ACharacterProperties.UseDoubleFontSize or
    ACharacterProperties.UseFontBold or
    ACharacterProperties.UseFontItalic or
    ACharacterProperties.UseFontUnderlineType or
    (ACharacterProperties.UseUnderlineColor and not TdxAlphaColors.IsTransparentOrEmpty(ACharacterProperties.UnderlineColor)) or
    ACharacterProperties.UseUnderlineWordsOnly or
    ACharacterProperties.UseFontStrikeoutType or
    ACharacterProperties.UseAllCaps or
    ACharacterProperties.UseForeColor or
    ACharacterProperties.UseScript or
    ACharacterProperties.UseBackColor or
    ACharacterProperties.UseHidden or
    ACharacterProperties.UseNoProof or
    (ARun.CharacterStyleIndex > 0);
end;

procedure TdxWordProcessingMLBaseExporter.ExportTextRunCore(ARun: TdxTextRun; const ARunText: string);
var
  AFrom, ACount, I: Integer;
  ACharacter: Char;
  ARunBreakValue: TdxWordProcessingMLValue;
begin
  AFrom := 1;
  ACount := Length(ARunText);
  for I := 1 to ACount do
  begin
    ACharacter := ARunText[I];
    if FRunBreaksTable.TryGetValue(ACharacter, ARunBreakValue) then
    begin
      ExportTextCore(ARunText, AFrom, I - AFrom);
      AFrom := I + 1;
      ExportBreak(GetWordProcessingMLValue(ARunBreakValue));
    end
    else
    begin
      if ACharacter = TdxCharacters.TabMark then
      begin
        ExportTextCore(ARunText, AFrom, I - AFrom);
        AFrom := I + 1;
        WriteWpStartElement('tab');
        WriteWpEndElement;
      end;
    end;
  end;
  ExportTextCore(ARunText, AFrom, ACount - AFrom + 1);
end;

procedure TdxWordProcessingMLBaseExporter.ExportTextCore(const ARunText: string; AFrom: Integer; ALength: Integer);
begin
  if ALength = 0 then
    Exit;
  if (AFrom = 1) and (ALength = Length(ARunText)) then
    ExportTextCore(ARunText)
  else
    ExportTextCore(Copy(ARunText, AFrom, ALength));
end;

procedure TdxWordProcessingMLBaseExporter.ExportBreak(const AValue: string);
begin

  DocumentContentWriter.WriteStartElement(WordProcessingPrefix, 'br', WordProcessingNamespace);
  try
    DocumentContentWriter.WriteAttributeString(WordProcessingPrefix, 'type', WordProcessingNamespace, AValue);
  finally
    DocumentContentWriter.WriteEndElement;
  end;
end;

procedure TdxWordProcessingMLBaseExporter.ExportTextCore(const ARunText: string);
var
  AText: string;
begin
  if ARunText = '' then
    Exit;

  AText := TdxXmlTextHelper.DeleteIllegalXmlCharacters(ARunText);

  if AText = '' then
    Exit;

  if FFieldCodeDepth = 0 then
    WriteWpStartElement('t')
  else
    WriteWpStartElement('instrText');
  try
    if ShouldPreserveSpace(AText) then
      FDocumentContentWriter.WriteAttributeString('xml', 'space', '', 'preserve');
    FDocumentContentWriter.WriteString(AText);
  finally
    WriteWpEndElement;
  end;
end;

procedure TdxWordProcessingMLBaseExporter.ExportRunProperties(ARun: TdxTextRunBase);
begin
  if not ShouldExportRunProperties(ARun) then
    Exit;

  WriteWpStartElement('rPr');
  try
    if ARun.CharacterStyleIndex > 0 then
      WriteWpStringValue('rStyle', GetCharacterStyleId(ARun.CharacterStyleIndex));
    ExportRunPropertiesCore(ARun.CharacterProperties);
  finally
    WriteWpEndElement;
  end;
end;

procedure TdxWordProcessingMLBaseExporter.ExportRunPropertiesCore(ACharacterProperties: TdxCharacterProperties);
begin
  ExportRunFontName(ACharacterProperties);
  ExportRunFontBold(ACharacterProperties);
  ExportRunFontItalic(ACharacterProperties);
  ExportRunAllCaps(ACharacterProperties);
  ExportRunFontStrikeout(ACharacterProperties);
  ExportRunNoProof(ACharacterProperties);
  ExportRunHidden(ACharacterProperties);
  ExportRunForeColor(ACharacterProperties);
  ExportDoubleFontSize(ACharacterProperties);
  ExportRunFontUnderline(ACharacterProperties);
  ExportRunBackColor(ACharacterProperties);
  ExportRunFontScript(ACharacterProperties);
  ExportRunLangInfo(ACharacterProperties);
end;

procedure TdxWordProcessingMLBaseExporter.ExportRunFontName(ACharacterProperties: TdxCharacterProperties);
var
  AFontName: string;
begin
  if not ACharacterProperties.UseFontName then
    Exit;

  WriteWpStartElement('rFonts');
  try
    AFontName := PrepareFontName(ACharacterProperties.FontName);
    WriteWpStringAttr('ascii', AFontName);
    WriteWpStringAttr(GetWordProcessingMLValue('hAnsi'), AFontName);
  finally
    WriteWpEndElement;
  end;
end;

function TdxWordProcessingMLBaseExporter.PrepareFontName(const AName: string): string;
begin
  Result := AName;
end;

procedure TdxWordProcessingMLBaseExporter.ExportRunFontBold(ACharacterProperties: TdxCharacterProperties);
begin
  if ACharacterProperties.UseFontBold then
    WriteWpBoolValue('b', ACharacterProperties.FontBold);
end;

procedure TdxWordProcessingMLBaseExporter.ExportRunFontItalic(ACharacterProperties: TdxCharacterProperties);
begin
  if ACharacterProperties.UseFontItalic then
    WriteWpBoolValue('i', ACharacterProperties.FontItalic);
end;

procedure TdxWordProcessingMLBaseExporter.ExportRunAllCaps(ACharacterProperties: TdxCharacterProperties);
begin
  if ACharacterProperties.UseAllCaps then
    WriteWpBoolValue('caps', ACharacterProperties.AllCaps);
end;

procedure TdxWordProcessingMLBaseExporter.ExportRunHidden(ACharacterProperties: TdxCharacterProperties);
begin
  if ACharacterProperties.UseHidden then
    WriteWpBoolValue('vanish', ACharacterProperties.Hidden);
end;

procedure TdxWordProcessingMLBaseExporter.ExportRunForeColor(ACharacterProperties: TdxCharacterProperties);
var
  AColor: TdxAlphaColor;
begin
  if ACharacterProperties.UseForeColor then
  begin
    AColor := ACharacterProperties.ForeColor;
    if not TdxAlphaColors.IsTransparentOrEmpty(AColor) then
      WriteWpStringValue('color', ConvertColorToString(AColor))
    else
      WriteWpStringValue('color', 'auto');
  end;
end;

procedure TdxWordProcessingMLBaseExporter.ExportDoubleFontSize(ACharacterProperties: TdxCharacterProperties);
begin
  if ACharacterProperties.UseDoubleFontSize then
    WriteWpIntValue('sz', ACharacterProperties.DoubleFontSize);
end;

procedure TdxWordProcessingMLBaseExporter.ExportRunFontUnderline(ACharacterProperties: TdxCharacterProperties);
begin
  if (not ACharacterProperties.UseFontUnderlineType and
     (not ACharacterProperties.UseUnderlineColor or (TdxAlphaColors.IsTransparentOrEmpty(ACharacterProperties.UnderlineColor)))) and
      not ACharacterProperties.UseUnderlineWordsOnly then
    Exit;

  WriteWpStartElement('u');
  try
    if ACharacterProperties.UseUnderlineWordsOnly and ACharacterProperties.UnderlineWordsOnly and
       ACharacterProperties.UseFontUnderlineType and (ACharacterProperties.FontUnderlineType = TdxUnderlineType.Single) then
      WriteWpStringAttr('val', 'words')
    else
      if ACharacterProperties.UseFontUnderlineType then
        WriteWpStringAttr('val', ConvertUnderlineType(ACharacterProperties.FontUnderlineType));

    if ACharacterProperties.UseUnderlineColor and not TdxAlphaColors.IsTransparentOrEmpty(ACharacterProperties.UnderlineColor) then
      WriteWpStringAttr('color', ConvertColorToString(ACharacterProperties.UnderlineColor));
  finally
    WriteWpEndElement;
  end;
end;

procedure TdxWordProcessingMLBaseExporter.ExportRunFontStrikeout(ACharacterProperties: TdxCharacterProperties);
begin
  if not ACharacterProperties.UseFontStrikeoutType then
    Exit;

  if ACharacterProperties.FontStrikeoutType = TdxStrikeoutType.Double then
    WriteWpBoolValue('dstrike', True)
  else
    if ACharacterProperties.FontStrikeoutType = TdxStrikeoutType.None then
      WriteWpBoolValue('strike', False)
    else
      WriteWpBoolValue('strike', True);
end;

procedure TdxWordProcessingMLBaseExporter.ExportRunFontScript(ACharacterProperties: TdxCharacterProperties);
begin
  if ACharacterProperties.UseScript then
    WriteWpStringValue('vertAlign', ConvertScript(ACharacterProperties.Script));
end;

procedure TdxWordProcessingMLBaseExporter.ExportRunLangInfo(ACharacterProperties: TdxCharacterProperties);
begin
end;

procedure TdxWordProcessingMLBaseExporter.ExportRunNoProof(ACharacterProperties: TdxCharacterProperties);
begin
  if not ACharacterProperties.UseNoProof then
    Exit;
  WriteWpStartElement('noProof');
  WriteWpBoolAttr('val', ACharacterProperties.NoProof);
  WriteWpEndElement;
end;

function TdxWordProcessingMLBaseExporter.ExportParagraph(AParagraph: TdxParagraph): TdxParagraphIndex;
begin
  WriteWpStartElement('p');
  try
    ExportParagraphProperties(AParagraph);
    Exit(inherited ExportParagraph(AParagraph));
  finally
    WriteWpEndElement;
  end;
end;

procedure TdxWordProcessingMLBaseExporter.ExportParagraphProperties(AParagraph: TdxParagraph);
var
  AParagraphNumberingExporter: TdxAction;
begin
  if not ShouldExportParagraphProperties(AParagraph) then
    Exit;
  WriteWpStartElement('pPr');
  try
    AParagraphNumberingExporter := nil;
    if ShouldExportParagraphNumbering(AParagraph.GetOwnNumberingListIndex) and AParagraph.ShouldExportNumbering then
      AParagraphNumberingExporter :=
        procedure ()
        begin
          ExportParagraphListReference(AParagraph);
        end;
    ExportParagraphPropertiesCore(AParagraph, AParagraphNumberingExporter);
    if ShouldExportSectionProperties(AParagraph) then
      ExportSectionProperties(CurrentSection);
  finally
    WriteWpEndElement;
  end;
end;

function TdxWordProcessingMLBaseExporter.ShouldExportParagraphNumbering(ANumberingListIndex: TdxNumberingListIndex): Boolean;
begin
  Result := (ANumberingListIndex = NumberingListIndexNoNumberingList) or (ANumberingListIndex >= NumberingListIndexMinValue);
end;

procedure TdxWordProcessingMLBaseExporter.ExportParagraphPropertiesCore(AParagraph: TdxParagraph;
  const AParagraphNumberingExporter: TdxAction);
var
  AFrameExporter, ATabsExporter: TdxAction;
  AWebSettings: TdxWebSettings;
begin
  if AParagraph.ParagraphStyleIndex > 0 then
    WriteWpStringValue('pStyle', GetParagraphStyleId(AParagraph.ParagraphStyleIndex));
  AFrameExporter := nil;
  if AParagraph.FrameProperties <> nil then
    AFrameExporter :=
      procedure ()
      begin
        ExportParagraphFrame(AParagraph.FrameProperties);
      end;
  ATabsExporter :=
    procedure()
    var
      ATabs: TdxTabFormattingInfo;
    begin
      ATabs := AParagraph.GetOwnTabs;
      try
        ExportTabProperties(ATabs);
      finally
        ATabs.Free;
      end;
    end;
  ExportParagraphPropertiesCore(AParagraph.ParagraphProperties, False, AFrameExporter, AParagraphNumberingExporter, ATabsExporter);

  AWebSettings := DocumentModel.WebSettings;
  if AWebSettings.IsBodyMarginsSet then
    WriteWpIntValue('divId', AWebSettings.GetHashCode);

  ExportRunProperties(PieceTable.Runs[AParagraph.LastRunIndex]);
end;

procedure TdxWordProcessingMLBaseExporter.ExportParagraphPropertiesCore(AProperties: TdxParagraphProperties;
  ADefaultParagraphProperties: Boolean; const AFramePropertiesExporter, AParagraphNumberingExporter, ATabsExporter: TdxAction);
begin
  if AProperties.UseKeepWithNext then
    WriteWpBoolValue('keepNext', AProperties.KeepWithNext);
  if AProperties.UseKeepLinesTogether then
    WriteWpBoolValue('keepLines', AProperties.KeepLinesTogether);
  if AProperties.UsePageBreakBefore and AProperties.PageBreakBefore then
    WriteWpBoolValue('pageBreakBefore', AProperties.PageBreakBefore);
  if Assigned(AFramePropertiesExporter) then
    AFramePropertiesExporter;
  if AProperties.UseWidowOrphanControl then
    WriteWpBoolValue('widowControl', AProperties.WidowOrphanControl);
  if Assigned(AParagraphNumberingExporter) then
    AParagraphNumberingExporter;
  if AProperties.UseSuppressLineNumbers then
    WriteWpBoolValue(GetWordProcessingMLValue(TdxWordProcessingMLValue.Create('suppressLineNumbers', 'supressLineNumbers')), AProperties.SuppressLineNumbers);
  if (AProperties.UseLeftBorder) or (AProperties.UseRightBorder) or (AProperties.UseTopBorder) or (AProperties.UseBottomBorder) then
    ExportParagraphBorders(AProperties, ADefaultParagraphProperties);
  if AProperties.UseBackColor then
    ExportParagraphBackground(AProperties.BackColor);
  if Assigned(ATabsExporter) then
    ATabsExporter;
  if AProperties.UseSuppressHyphenation then
    WriteWpBoolValue('suppressAutoHyphens', AProperties.SuppressHyphenation);
  if (AProperties.UseSpacingBefore) or (AProperties.UseSpacingAfter) or (AProperties.UseLineSpacingType) or (AProperties.UseLineSpacing) or (AProperties.UseBeforeAutoSpacing) or (AProperties.UseAfterAutoSpacing) then
    ExportParagraphSpacing(AProperties);
  if (AProperties.UseFirstLineIndentType) or (AProperties.UseFirstLineIndent) or (AProperties.UseLeftIndent) or (AProperties.UseRightIndent) then
    ExportParagraphIndentation(AProperties);
  if AProperties.UseContextualSpacing then
    WriteWpBoolValue('contextualSpacing', AProperties.ContextualSpacing);
  if AProperties.UseAlignment then
    WriteWpStringValue('jc', ConvertAlignment(AProperties.Alignment));
  if AProperties.UseOutlineLevel then
    ExportParagraphOutlineLevel(AProperties);
end;

function TdxWordProcessingMLBaseExporter.GetNumberingListIndexForExport(ANumberingListIndex: TdxNumberingListIndex): Integer;
begin
  if ANumberingListIndex = NumberingListIndexNoNumberingList then
    Result := 0
  else
    Result := ANumberingListIndex + 1;
end;

procedure TdxWordProcessingMLBaseExporter.ExportTabProperties(ATabs: TdxTabFormattingInfo);
var
  ACount, I: Integer;
begin
  if not ShouldExportTabProperties(ATabs) then
    Exit;

  WriteWpStartElement('tabs');
  try
    ACount := ATabs.Count;
    for I := 0 to ACount - 1 do
      if not ATabs[I].IsDefault then
        ExportTab(ATabs[I]);
  finally
    WriteWpEndElement;
  end;
end;

procedure TdxWordProcessingMLBaseExporter.ExportTab(const ATab: TdxTabInfo);
var
  APos: Integer;
begin
  WriteWpStartElement('tab');
  try
    if ATab.Deleted then
      WriteWpStringAttr('val', 'clear')
    else
      WriteWpStringAttr('val', ConvertTabAlignment(ATab.Alignment));
    APos := Math.Max(UnitConverter.ModelUnitsToTwips(ATab.Position), -31680);
    APos := Math.Min(APos, 31680);
    WriteWpIntAttr('pos', APos);
    WriteWpStringAttr('leader', ConvertTabLeader(ATab.Leader));
  finally
    WriteWpEndElement;
  end;
end;

procedure TdxWordProcessingMLBaseExporter.ExportParagraphFrame(AProperties: TdxFrameProperties);
begin
  WriteWpStartElement('framePr');
  try
    if AProperties.UseWidth then
      WriteWpIntAttr('w', UnitConverter.ModelUnitsToTwips(AProperties.Width));
    if AProperties.UseHeight then
      WriteWpIntAttr('h', UnitConverter.ModelUnitsToTwips(AProperties.Height));
    if AProperties.UseHorizontalRule then
      WriteWpStringAttr(GetWordProcessingMLValue('hRule'), ConvertHorizontalRule(AProperties.HorizontalRule));
    if AProperties.UseTextWrapType then
      WriteWpStringAttr(GetWordProcessingMLValue('wrap'), ConvertWrapType(AProperties.TextWrapType));

    if AProperties.UseVerticalPositionType then
      WriteWpStringAttr(GetWordProcessingMLValue('vAnchor'), ConvertVerticalPositionType(AProperties.VerticalPositionType));
    if AProperties.UseHorizontalPositionType then
      WriteWpStringAttr(GetWordProcessingMLValue('hAnchor'), ConvertHorizontalPositionType(AProperties.HorizontalPositionType));

    if AProperties.UseX then
      WriteWpIntAttr('x', UnitConverter.ModelUnitsToTwips(AProperties.X));
    if AProperties.HorizontalPositionAlignment <> TdxParagraphFrameHorizontalPositionAlignment.None then
      WriteWpStringAttr(GetWordProcessingMLValue('xAlign'), ConvertHorizontalPositionAlignment(AProperties.HorizontalPositionAlignment));

    if AProperties.UseY then
      WriteWpIntAttr('y', UnitConverter.ModelUnitsToTwips(AProperties.Y));
    if AProperties.VerticalPositionAlignment <> TdxParagraphFrameVerticalPositionAlignment.None then
      WriteWpStringAttr(GetWordProcessingMLValue('yAlign'), ConvertVerticalPositionAlignment(AProperties.VerticalPositionAlignment));
  finally
    WriteWpEndElement;
  end;
end;

function TdxWordProcessingMLBaseExporter.ConvertHorizontalRule(AHorizontalRule: TdxParagraphFrameHorizontalRule): string;
begin
  case AHorizontalRule of
    TdxParagraphFrameHorizontalRule.AtLeast:
      Result := 'atLeast';
    TdxParagraphFrameHorizontalRule.Exact:
      Result := 'exact';
    else
      Result := 'auto';
  end;
end;

function TdxWordProcessingMLBaseExporter.ConvertWrapType(AWrapType: TdxFloatingObjectTextWrapType): string;
begin
  case AWrapType of
    TdxFloatingObjectTextWrapType.Square:
      Result := 'around';
    TdxFloatingObjectTextWrapType.None:
      Result := 'none';
    TdxFloatingObjectTextWrapType.TopAndBottom:
      Result := 'notBeside';
    TdxFloatingObjectTextWrapType.Through:
      Result := 'through';
    TdxFloatingObjectTextWrapType.Tight:
      Result := 'tight';
    else
      Result := 'auto';
  end;
end;

function TdxWordProcessingMLBaseExporter.ConvertVerticalPositionType(AVerticalPositionType: TdxFloatingObjectVerticalPositionType): string;
begin
  case AVerticalPositionType of
    TdxFloatingObjectVerticalPositionType.Page:
      Result := 'page';
    TdxFloatingObjectVerticalPositionType.Margin:
      Result := 'margin';
    else
      Result := 'text';
  end;
end;

function TdxWordProcessingMLBaseExporter.ConvertHorizontalPositionType(AHorizontalPositionType: TdxFloatingObjectHorizontalPositionType): string;
begin
  case AHorizontalPositionType of
    TdxFloatingObjectHorizontalPositionType.Margin:
      Result := 'margin';
    TdxFloatingObjectHorizontalPositionType.Page:
      Result := 'page';
    else
      Result := 'text';
  end;
end;

function TdxWordProcessingMLBaseExporter.ConvertHorizontalPositionAlignment(AHorizontalPositionAlignment: TdxFloatingObjectHorizontalPositionAlignment): string;
begin
  case AHorizontalPositionAlignment of
    TdxFloatingObjectHorizontalPositionAlignment.Center:
      Result := 'center';
    TdxFloatingObjectHorizontalPositionAlignment.Inside:
      Result := 'inside';
    TdxFloatingObjectHorizontalPositionAlignment.Left:
      Result := 'left';
    TdxFloatingObjectHorizontalPositionAlignment.Outside:
      Result := 'outside';
    TdxFloatingObjectHorizontalPositionAlignment.Right:
      Result := 'right';
    else
      Result := '';
  end;
end;

function TdxWordProcessingMLBaseExporter.ConvertVerticalPositionAlignment(AVerticalPositionAlignment: TdxFloatingObjectVerticalPositionAlignment): string;
begin
  case AVerticalPositionAlignment of
    TdxFloatingObjectVerticalPositionAlignment.Bottom:
      Result := 'bottom';
    TdxFloatingObjectVerticalPositionAlignment.Center:
      Result := 'center';
    TdxFloatingObjectVerticalPositionAlignment.Inside:
      Result := 'inside';
    TdxFloatingObjectVerticalPositionAlignment.Outside:
      Result := 'outside';
    TdxFloatingObjectVerticalPositionAlignment.Top:
      Result := 'top';
    else
      Result := '';
  end;
end;

function TdxWordProcessingMLBaseExporter.ConvertWrapType(AWrapType: TdxParagraphFrameTextWrapType): string;
begin
  case AWrapType of
    TdxParagraphFrameTextWrapType.Around:
      Result := 'around';
    TdxParagraphFrameTextWrapType.None:
      Result := 'none';
    TdxParagraphFrameTextWrapType.NotBeside:
      Result := 'notBeside';
    TdxParagraphFrameTextWrapType.Through:
      Result := 'through';
    TdxParagraphFrameTextWrapType.Tight:
      Result := 'tight';
    else
      Result := 'auto';
  end;
end;

function TdxWordProcessingMLBaseExporter.ConvertVerticalPositionType(AVerticalPositionType: TdxParagraphFrameVerticalPositionType): string;
begin
  case AVerticalPositionType of
    TdxParagraphFrameVerticalPositionType.Margin:
      Result := 'margin';
    TdxParagraphFrameVerticalPositionType.Page:
      Result := 'page';
    else
      Result := 'text';
  end;
end;

function TdxWordProcessingMLBaseExporter.ConvertHorizontalPositionType(AHorizontalPositionType: TdxParagraphFrameHorizontalPositionType): string;
begin
  case AHorizontalPositionType of
    TdxParagraphFrameHorizontalPositionType.Margin:
      Result := 'margin';
    TdxParagraphFrameHorizontalPositionType.Page:
      Result := 'page';
    else
      Result := 'text';
  end;
end;

function TdxWordProcessingMLBaseExporter.ConvertHorizontalPositionAlignment(AHorizontalPositionAlignment: TdxParagraphFrameHorizontalPositionAlignment): string;
begin
  case AHorizontalPositionAlignment of
    TdxParagraphFrameHorizontalPositionAlignment.Center:
      Result := 'center';
    TdxParagraphFrameHorizontalPositionAlignment.Inside:
      Result := 'inside';
    TdxParagraphFrameHorizontalPositionAlignment.Left:
      Result := 'left';
    TdxParagraphFrameHorizontalPositionAlignment.Outside:
      Result := 'outside';
    TdxParagraphFrameHorizontalPositionAlignment.Right:
      Result := 'right';
    else
      Result := '';
  end;
end;

function TdxWordProcessingMLBaseExporter.ConvertVerticalPositionAlignment(AVerticalPositionAlignment: TdxParagraphFrameVerticalPositionAlignment): string;
begin
  case AVerticalPositionAlignment of
    TdxParagraphFrameVerticalPositionAlignment.Bottom:
      Result := 'bottom';
    TdxParagraphFrameVerticalPositionAlignment.Center:
      Result := 'center';
    TdxParagraphFrameVerticalPositionAlignment.Inline:
      Result := 'inline';
    TdxParagraphFrameVerticalPositionAlignment.Inside:
      Result := 'inside';
    TdxParagraphFrameVerticalPositionAlignment.Outside:
      Result := 'outside';
    TdxParagraphFrameVerticalPositionAlignment.Top:
      Result := 'top';
    else
      Result := '';
  end;
end;

procedure TdxWordProcessingMLBaseExporter.ExportParagraphSpacing(AProperties: TdxParagraphProperties);
begin
  WriteWpStartElement('spacing');
  try
    if AProperties.UseLineSpacingType then
      WriteWpStringAttr(GetWordProcessingMLValue('lineRule'), ConvertLineSpacing(AProperties.LineSpacingType));
    if (AProperties.UseLineSpacing) or (AProperties.UseLineSpacingType) then
      WriteWpIntAttr('line', ConvertLineSpacingValue(AProperties.LineSpacingType, AProperties.LineSpacing));
    if AProperties.UseSpacingBefore then
      WriteWpIntAttr('before', UnitConverter.ModelUnitsToTwips(AProperties.SpacingBefore));
    if AProperties.UseSpacingAfter then
      WriteWpIntAttr('after', UnitConverter.ModelUnitsToTwips(AProperties.SpacingAfter));
    if AProperties.UseBeforeAutoSpacing then
      WriteWpBoolAttr(GetWordProcessingMLValue(TdxWordProcessingMLValue.Create('beforeAutospacing', 'before-autospacing')), AProperties.BeforeAutoSpacing);
    if AProperties.UseAfterAutoSpacing then
      WriteWpBoolAttr(GetWordProcessingMLValue(TdxWordProcessingMLValue.Create('afterAutospacing', 'after-autospacing')), AProperties.AfterAutoSpacing);
  finally
    WriteWpEndElement;
  end;
end;

procedure TdxWordProcessingMLBaseExporter.ExportParagraphOutlineLevel(AProperties: TdxParagraphProperties);
var
  ALevel: Integer;
begin
  ALevel := AProperties.OutlineLevel;
  if (ALevel <= 0) or (ALevel >= 10) then
    Exit;
  Dec(ALevel);
  WriteWpIntValue('outlineLvl', ALevel);
end;

procedure TdxWordProcessingMLBaseExporter.ExportParagraphBackground(ABackground: TdxAlphaColor);
begin
  WriteWpStartElement('shd');
  try
    if TdxAlphaColors.IsTransparentOrEmpty(ABackground) then
    begin
      WriteWpStringAttr('val', ConvertShadingPattern(TdxShadingPattern.Clear));
      WriteWpStringAttr('fill', 'auto');
    end
    else
    begin
      WriteWpStringAttr('val', ConvertShadingPattern(TdxShadingPattern.Clear));
      WriteWpStringAttr('fill', ConvertColorToString(ABackground));
    end;
  finally
    WriteWpEndElement;
  end;
end;

procedure TdxWordProcessingMLBaseExporter.ExportParagraphIndentation(AProperties: TdxParagraphProperties);
begin
  WriteWpStartElement('ind');
  try
    if AProperties.UseFirstLineIndentType then
    begin
      case AProperties.FirstLineIndentType of
        TdxParagraphFirstLineIndent.Hanging:
          begin
            WriteWpIntAttr('hanging', UnitConverter.ModelUnitsToTwips(AProperties.FirstLineIndent));
            if AProperties.UseLeftIndent then
              WriteWpIntAttr('left', UnitConverter.ModelUnitsToTwips(AProperties.LeftIndent));
          end;
        TdxParagraphFirstLineIndent.Indented:
          begin
            WriteWpIntAttr(GetWordProcessingMLValue('firstLine'), UnitConverter.ModelUnitsToTwips(AProperties.FirstLineIndent));
            if AProperties.UseLeftIndent then
              WriteWpIntAttr('left', UnitConverter.ModelUnitsToTwips(AProperties.LeftIndent));
          end;
        TdxParagraphFirstLineIndent.None:
          begin
            WriteWpIntAttr(GetWordProcessingMLValue('firstLine'), 0);
            if AProperties.UseLeftIndent then
              WriteWpIntAttr('left', UnitConverter.ModelUnitsToTwips(AProperties.LeftIndent));
          end;
      end;
    end
    else
      if AProperties.UseLeftIndent then
        WriteWpIntAttr('left', UnitConverter.ModelUnitsToTwips(AProperties.LeftIndent));
    if AProperties.UseRightIndent then
      WriteWpIntAttr('right', UnitConverter.ModelUnitsToTwips(AProperties.RightIndent));
  finally
    WriteWpEndElement;
  end;
end;

procedure TdxWordProcessingMLBaseExporter.ExportParagraphBorders(AProperties: TdxParagraphProperties; ADefaultParagraphProperties: Boolean);
var
  AShouldExportBottomBorder, AShouldExportLeftBorder, AShouldExportRightBorder, AShouldExportTopBorder, AShouldExportBorder: Boolean;
begin

  AShouldExportBottomBorder := AProperties.UseBottomBorder and ShouldExportParagraphBorder(ADefaultParagraphProperties, AProperties.BottomBorder);
  AShouldExportLeftBorder := AProperties.UseLeftBorder and ShouldExportParagraphBorder(ADefaultParagraphProperties, AProperties.LeftBorder);
  AShouldExportRightBorder := AProperties.UseRightBorder and ShouldExportParagraphBorder(ADefaultParagraphProperties, AProperties.RightBorder);
  AShouldExportTopBorder := AProperties.UseTopBorder and ShouldExportParagraphBorder(ADefaultParagraphProperties, AProperties.TopBorder);
  AShouldExportBorder := (AShouldExportBottomBorder) or (AShouldExportLeftBorder) or (AShouldExportRightBorder) or (AShouldExportTopBorder);
  if not AShouldExportBorder then
    Exit;
  WriteWpStartElement('pBdr');
  try
    if AShouldExportTopBorder then
      ExportParagraphBorder('top', AProperties.TopBorder);
    if AShouldExportLeftBorder then
      ExportParagraphBorder('left', AProperties.LeftBorder);
    if AShouldExportBottomBorder then
      ExportParagraphBorder('bottom', AProperties.BottomBorder);
    if AShouldExportRightBorder then
      ExportParagraphBorder('right', AProperties.RightBorder);
  finally
    WriteWpEndElement;
  end;
end;

function TdxWordProcessingMLBaseExporter.ShouldExportParagraphBorder(ADefaultParagraphProperties: Boolean; ABorderInfo: TdxBorderInfo): Boolean;
begin
  if ADefaultParagraphProperties then
    Result := (ABorderInfo.Style <> TdxBorderLineStyle.None) or (ABorderInfo.Width <> 0)
  else
    Result := True;
end;

procedure TdxWordProcessingMLBaseExporter.ExportParagraphBorder(const ATag: string; ABorder: TdxBorderInfo);
begin
  WriteWpStartElement(ATag);
  try
    WriteWpStringAttr('val', ConvertBorderLineStyle(ABorder.Style));
    WriteWpIntAttr('sz', Trunc(UnitConverter.ModelUnitsToPointsF(ABorder.Width * 8.0)));
    WriteWpIntAttr('space', Trunc(UnitConverter.ModelUnitsToPointsF(ABorder.Offset)));
    WriteWpBoolAttr('shadow', ABorder.Shadow);
    WriteWpBoolAttr('frame', ABorder.Frame);
    WriteWpStringAttr('color', ConvertColorToString(ABorder.Color));
  finally
    WriteWpEndElement;
  end;
end;

function TdxWordProcessingMLBaseExporter.ConvertLineSpacingValue(ALineSpacing: TdxParagraphLineSpacing; AValue: Single): Integer;
begin
  if (ALineSpacing = TdxParagraphLineSpacing.AtLeast) or (ALineSpacing = TdxParagraphLineSpacing.Exactly) then
    Result := Round(UnitConverter.ModelUnitsToTwipsF(AValue))
  else
  begin
    if ALineSpacing = TdxParagraphLineSpacing.Single then
      AValue := 1
    else
      if ALineSpacing = TdxParagraphLineSpacing.Double then
        AValue := 2
      else
        if ALineSpacing = TdxParagraphLineSpacing.Sesquialteral then
          AValue := 1.5;
    Result := Round(AValue * 240);
  end;
end;

function TdxWordProcessingMLBaseExporter.ConvertTabAlignment(AAlignment: TdxTabAlignmentType): string;
var
  AResult: TdxWordProcessingMLValue;
begin
  if not FTabAlignmentTable.TryGetValue(AAlignment, AResult) then
    AResult := FTabAlignmentTable[TdxTabAlignmentType.Left];
  Result := GetWordProcessingMLValue(AResult);
end;

function TdxWordProcessingMLBaseExporter.ConvertTabLeader(ALeaderType: TdxTabLeaderType): string;
var
  AResult: TdxWordProcessingMLValue;
begin
  if not FTabLeaderTable.TryGetValue(ALeaderType, AResult) then
    AResult := FTabLeaderTable[TdxTabLeaderType.None];
  Result := GetWordProcessingMLValue(AResult);
end;

function TdxWordProcessingMLBaseExporter.ConvertAlignment(AAlignment: TdxParagraphAlignment): string;
var
  AResult: TdxWordProcessingMLValue;
begin
  if not FParagraphAlignmentTable.TryGetValue(AAlignment, AResult) then
    AResult := FParagraphAlignmentTable[TdxParagraphAlignment.Left];
  Result := GetWordProcessingMLValue(AResult);
end;

function TdxWordProcessingMLBaseExporter.ConvertLineSpacing(ALineSpacing: TdxParagraphLineSpacing): string;
var
  AResult: TdxWordProcessingMLValue;
begin
  if not FLineSpacingTable.TryGetValue(ALineSpacing, AResult) then
    AResult := FLineSpacingTable[TdxParagraphLineSpacing.Single];
  Result := GetWordProcessingMLValue(AResult);
end;

function TdxWordProcessingMLBaseExporter.ConvertTextDirection(AValue: TdxTextDirection): string;
var
  AResult: TdxWordProcessingMLValue;
begin
  if FTextDirectionTable.TryGetValue(AValue, AResult) then
    Result := GetWordProcessingMLValue(AResult)
  else
    Result := GetWordProcessingMLValue(FTextDirectionTable[TdxTextDirection.LeftToRightTopToBottom]);
end;

function TdxWordProcessingMLBaseExporter.ConvertShadingPattern(AValue: TdxShadingPattern): string;
var
  AResult: TdxWordProcessingMLValue;
begin
  if FShadingPatternTable.TryGetValue(AValue, AResult) then
    Result := GetWordProcessingMLValue(AResult)
  else
    Result := GetWordProcessingMLValue(FShadingPatternTable[TdxShadingPattern.Clear]);
end;

function TdxWordProcessingMLBaseExporter.ConvertVerticalAlignment(AValue: TdxVerticalAlignment): string;
var
  AResult: TdxWordProcessingMLValue;
begin
  if FVerticalAlignmentTable.TryGetValue(AValue, AResult) then
    Result := GetWordProcessingMLValue(AResult)
  else
    Result := GetWordProcessingMLValue(FVerticalAlignmentTable[TdxVerticalAlignment.Top]);
end;

function TdxWordProcessingMLBaseExporter.ConvertWidthUnitTypes(AValue: TdxWidthUnitType): string;
var
  AResult: TdxWordProcessingMLValue;
begin
  if FWidthUnitTypesTable.TryGetValue(AValue, AResult) then
    Result := GetWordProcessingMLValue(AResult)
  else
    Result := GetWordProcessingMLValue(FWidthUnitTypesTable[TdxWidthUnitType.ModelUnits]);
end;

function TdxWordProcessingMLBaseExporter.ConvertTableLayoutType(AValue: TdxTableLayoutType): string;
var
  AResult: TdxWordProcessingMLValue;
begin
  if FTableLayoutTypeTable.TryGetValue(AValue, AResult) then
    Result := GetWordProcessingMLValue(AResult)
  else
    Result := GetWordProcessingMLValue(FTableLayoutTypeTable[TdxTableLayoutType.Autofit]);
end;

function TdxWordProcessingMLBaseExporter.ConvertBorderLineStyle(AValue: TdxBorderLineStyle): string;
var
  AResult: TdxWordProcessingMLValue;
begin
  if FBorderLineStyleTable.TryGetValue(AValue, AResult) then
    Result := GetWordProcessingMLValue(AResult)
  else
    Result := GetWordProcessingMLValue(FBorderLineStyleTable[TdxBorderLineStyle.None]);
end;

function TdxWordProcessingMLBaseExporter.ConvertHorizontalAlignMode(AValue: TdxHorizontalAlignMode): string;
var
  AResult: TdxWordProcessingMLValue;
begin
  if FHorizontalAlignModeTable.TryGetValue(AValue, AResult) then
    Result := GetWordProcessingMLValue(AResult)
  else
    Result := GetWordProcessingMLValue(FHorizontalAlignModeTable[TdxHorizontalAlignMode.Left]);
end;

function TdxWordProcessingMLBaseExporter.ConvertHorizontalAnchorTypes(AValue: TdxHorizontalAnchorTypes): string;
var
  AResult: TdxWordProcessingMLValue;
begin
  if FHorizontalAnchorTypesTable.TryGetValue(AValue, AResult) then
    Result := GetWordProcessingMLValue(AResult)
  else
    Result := GetWordProcessingMLValue(FHorizontalAnchorTypesTable[TdxHorizontalAnchorTypes.Column]);
end;

function TdxWordProcessingMLBaseExporter.ConvertConditionalStyleType(AStyleType: TdxConditionalTableStyleFormattingType): string;
var
  AResult: TdxWordProcessingMLValue;
begin
  if FConditionalTableStyleFormattingTypesTable.TryGetValue(AStyleType, AResult) then
    Result := GetWordProcessingMLValue(AResult)
  else
    Result := GetWordProcessingMLValue(FConditionalTableStyleFormattingTypesTable[TdxConditionalTableStyleFormattingType.WholeTable]);
end;

function TdxWordProcessingMLBaseExporter.ConvertVerticalAlignMode(AValue: TdxVerticalAlignMode): string;
var
  AResult: TdxWordProcessingMLValue;
begin
  if FVerticalAlignModeTable.TryGetValue(AValue, AResult) then
    Result := GetWordProcessingMLValue(AResult)
  else
    Result := GetWordProcessingMLValue(FVerticalAlignModeTable[TdxVerticalAlignMode.Bottom]);
end;

function TdxWordProcessingMLBaseExporter.ConvertVerticalAnchorTypes(AValue: TdxVerticalAnchorTypes): string;
var
  AResult: TdxWordProcessingMLValue;
begin
  if FVerticalAnchorTypesTable.TryGetValue(AValue, AResult) then
    Result := GetWordProcessingMLValue(AResult)
  else
    Result := GetWordProcessingMLValue(FVerticalAnchorTypesTable[TdxVerticalAnchorTypes.Margin]);
end;

function TdxWordProcessingMLBaseExporter.ConvertHeightUnitType(AValue: TdxHeightUnitType): string;
var
  AResult: TdxWordProcessingMLValue;
begin
  if FHeightUnitTypeTable.TryGetValue(AValue, AResult) then
    Result := GetWordProcessingMLValue(AResult)
  else
    Result := GetWordProcessingMLValue(FHeightUnitTypeTable[TdxHeightUnitType.Auto]);
end;

function TdxWordProcessingMLBaseExporter.ConvertMergingState(AValue: TdxMergingState): string;
var
  AResult: TdxWordProcessingMLValue;
begin
  if FMergingStateTable.TryGetValue(AValue, AResult) then
    Result := GetWordProcessingMLValue(AResult)
  else
    Result := GetWordProcessingMLValue(FMergingStateTable[TdxMergingState.Restart]);
end;

function TdxWordProcessingMLBaseExporter.ConvertTableRowAlignment(AValue: TdxTableRowAlignment): string;
var
  AResult: TdxWordProcessingMLValue;
begin
  if AValue = TdxTableRowAlignment.Both then
    Exit(GetWordProcessingMLValue(FTableRowAlignmentTable[TdxTableRowAlignment.Center]));

  if FTableRowAlignmentTable.TryGetValue(AValue, AResult) then
    Result := GetWordProcessingMLValue(AResult)
  else
    Result := GetWordProcessingMLValue(FTableRowAlignmentTable[TdxTableRowAlignment.Left]);
end;

function TdxWordProcessingMLBaseExporter.ShouldExportParagraphProperties(AParagraph: TdxParagraph): Boolean;
var
  AProperties: TdxParagraphProperties;
  ATabs: TdxTabFormattingInfo;
begin
  AProperties := AParagraph.ParagraphProperties;
  Result :=
    AProperties.UseAlignment or
    AProperties.UseFirstLineIndent or
    AProperties.UseFirstLineIndentType or
    AProperties.UseLeftIndent or
    AProperties.UseRightIndent or
    AProperties.UseLineSpacing or
    AProperties.UseLineSpacingType or
    AProperties.UseSpacingAfter or
    AProperties.UseSpacingBefore or
    AProperties.UseSuppressHyphenation or
    AProperties.UseSuppressLineNumbers or
    AProperties.UseContextualSpacing or
    AProperties.UsePageBreakBefore or
    AProperties.UseBeforeAutoSpacing or
    AProperties.UseAfterAutoSpacing or
    AProperties.UseKeepWithNext or
    AProperties.UseKeepLinesTogether or
    AProperties.UseWidowOrphanControl or
    AProperties.UseOutlineLevel or
    AProperties.UseBackColor or
    (AParagraph.ParagraphStyleIndex > 0) or
    ShouldExportRunProperties(PieceTable.Runs[AParagraph.LastRunIndex]) or
    AParagraph.IsInList or
    ShouldExportSectionProperties(AParagraph) or
    (AParagraph.FrameProperties <> nil);
  if not Result then
  begin
    ATabs := AParagraph.GetOwnTabs;
    try
      Result := ShouldExportTabProperties(ATabs);
    finally
      ATabs.Free;
    end;
  end;
end;

function TdxWordProcessingMLBaseExporter.ShouldExportSectionProperties(AParagraph: TdxParagraph): Boolean;
begin
  Result := PieceTable.IsMain and (AParagraph.Index = CurrentSection.LastParagraphIndex) and
    (AParagraph.Index <> PieceTable.Paragraphs.Count - 1) and DocumentModel.DocumentCapabilities.SectionsAllowed;
end;

function TdxWordProcessingMLBaseExporter.ShouldExportTabProperties(ATabs: TdxTabFormattingInfo): Boolean;
var
  ANonDefaultTabCount, ACount, I: Integer;
begin
  ANonDefaultTabCount := 0;
  ACount := ATabs.Count;
  for I := 0 to ACount - 1 do
    if not ATabs[I].IsDefault then
      Inc(ANonDefaultTabCount);

  Result := ANonDefaultTabCount > 0;
end;

procedure TdxWordProcessingMLBaseExporter.ExportSection(const ASection: TdxSection);
begin
  FCurrentSection := ASection;
  inherited ExportSection(ASection);
end;

procedure TdxWordProcessingMLBaseExporter.ExportSectionHeadersFooters(ASection: TdxSection);
begin
end;

procedure TdxWordProcessingMLBaseExporter.ExportSectionProperties(ASection: TdxSection);
begin
  WriteWpStartElement('sectPr');
  try
    ExportSectionHeadersFootersCore(ASection);
    ExportSectionPropertiesCore(ASection);
  finally
    WriteWpEndElement;
  end;
end;

procedure TdxWordProcessingMLBaseExporter.ExportSectionPropertiesCore(ASection: TdxSection);
begin
  ExportSectionFootNote(ASection.FootNote);
  ExportSectionEndNote(ASection.EndNote);
  WriteWpStringValue('type', ConvertSectionStartType(ASection.GeneralSettings.StartType));
  ExportSectionPage(ASection.Page);
  ExportSectionMargins(ASection.Margins);
  ExportSectionLineNumbering(ASection.LineNumbering);
  ExportSectionPageNumbering(ASection.PageNumbering);
  ExportSectionColumns(ASection.Columns);
  ExportSectionGeneralSettings(ASection.GeneralSettings);
end;

procedure TdxWordProcessingMLBaseExporter.ExportSectionGeneralSettings(ASettings: TdxSectionGeneralSettings);
var
  ADefaultSettings: TdxGeneralSectionInfo;
  ATag: TdxWordProcessingMLValue;
begin
  ADefaultSettings := DocumentModel.Cache.GeneralSectionInfoCache.DefaultItem;
  if ASettings.VerticalTextAlignment <> ADefaultSettings.VerticalTextAlignment then
    WriteWpStringValue('vAlign', ConvertVerticalAlignment(ASettings.VerticalTextAlignment));
  if ASettings.DifferentFirstPage <> ADefaultSettings.DifferentFirstPage then
    WriteWpBoolValue('titlePg', ASettings.DifferentFirstPage);
  if ASettings.TextDirection <> ADefaultSettings.TextDirection then
  begin
    ATag := TdxWordProcessingMLValue.Create('textDirection', 'textFlow');
    WriteWpStringValue(GetWordProcessingMLValue(ATag), ConvertTextDirection(ASettings.TextDirection));
  end;
  if ShouldExportPaperSource(ASettings) then
  begin
    WriteWpStartElement('paperSrc');
    try
      if ASettings.FirstPagePaperSource <> ADefaultSettings.FirstPagePaperSource then
        WriteWpIntAttr('first', ASettings.FirstPagePaperSource);
      if ASettings.OtherPagePaperSource <> ADefaultSettings.OtherPagePaperSource then
        WriteWpIntAttr('other', ASettings.OtherPagePaperSource);
    finally
      WriteWpEndElement;
    end;
  end;
end;

function TdxWordProcessingMLBaseExporter.ShouldExportPaperSource(ASettings: TdxSectionGeneralSettings): Boolean;
var
  ADefaultSettings: TdxGeneralSectionInfo;
begin
  ADefaultSettings := DocumentModel.Cache.GeneralSectionInfoCache.DefaultItem;

  Result := (ASettings.FirstPagePaperSource <> ADefaultSettings.FirstPagePaperSource) or (ASettings.OtherPagePaperSource <> ADefaultSettings.OtherPagePaperSource);
end;

procedure TdxWordProcessingMLBaseExporter.ExportSectionPage(APage: TdxSectionPage);
var
  ADefaultPage: TdxPageInfo;
begin
  if not ShouldExportSectionPage(APage) then
    Exit;
  ADefaultPage := DocumentModel.Cache.PageInfoCache.DefaultItem;
  WriteWpStartElement('pgSz');
  try
    if APage.Width <> ADefaultPage.Width then
      WriteWpIntAttr('w', UnitConverter.ModelUnitsToTwips(APage.Width));
    if APage.Height <> ADefaultPage.Height then
      WriteWpIntAttr('h', UnitConverter.ModelUnitsToTwips(APage.Height));
    if APage.PaperKind <> ADefaultPage.PaperKind then
      WriteWpIntAttr('code', Integer(APage.PaperKind));
    if APage.Landscape then
      WriteWpStringAttr('orient', 'landscape');
  finally
    WriteWpEndElement;
  end;
end;

function TdxWordProcessingMLBaseExporter.ShouldExportSectionPage(APage: TdxSectionPage): Boolean;
var
  ADefaultPage: TdxPageInfo;
begin
  ADefaultPage := DocumentModel.Cache.PageInfoCache.DefaultItem;
  Result := (APage.Width <> ADefaultPage.Width) or (APage.Height <> ADefaultPage.Height) or (APage.PaperKind <> ADefaultPage.PaperKind) or (APage.Landscape);
end;

procedure TdxWordProcessingMLBaseExporter.ExportSectionMargins(AMargins: TdxSectionMargins);
begin
  WriteWpStartElement('pgMar');
  try
    WriteWpIntAttr('left', UnitConverter.ModelUnitsToTwips(AMargins.Left));
    WriteWpIntAttr('right', UnitConverter.ModelUnitsToTwips(AMargins.Right));
    WriteWpIntAttr('top', UnitConverter.ModelUnitsToTwips(AMargins.Top));
    WriteWpIntAttr('bottom', UnitConverter.ModelUnitsToTwips(AMargins.Bottom));
    WriteWpIntAttr('header', UnitConverter.ModelUnitsToTwips(AMargins.HeaderOffset));
    WriteWpIntAttr('footer', UnitConverter.ModelUnitsToTwips(AMargins.FooterOffset));
    WriteWpIntAttr('gutter', UnitConverter.ModelUnitsToTwips(AMargins.Gutter));
  finally
    WriteWpEndElement;
  end;
end;

function TdxWordProcessingMLBaseExporter.ShouldExportSectionMargins(AMargins: TdxSectionMargins): Boolean;
var
  ADefaultMargins: TdxMarginsInfo;
begin
  ADefaultMargins := DocumentModel.Cache.MarginsInfoCache.DefaultItem;
  Result :=
    (AMargins.Left <> ADefaultMargins.Left) or
    (AMargins.Right <> ADefaultMargins.Right) or
    (AMargins.Top <> ADefaultMargins.Top) or
    (AMargins.Bottom <> ADefaultMargins.Bottom) or
    (AMargins.HeaderOffset <> ADefaultMargins.HeaderOffset) or
    (AMargins.FooterOffset <> ADefaultMargins.FooterOffset) or
    (AMargins.Gutter <> ADefaultMargins.Gutter);
end;

procedure TdxWordProcessingMLBaseExporter.ExportSectionColumns(AColumns: TdxSectionColumns);
begin
  if not TdxDocumentFormatsHelper.ShouldExportSectionColumns(AColumns, DocumentModel) then
    Exit;

  WriteWpStartElement('cols');
  try
    WriteWpBoolAttr('equalWidth', AColumns.EqualWidthColumns);
    if AColumns.EqualWidthColumns then
      ExportEqualWidthColumns(AColumns)
    else
      ExportNonUniformColumns(AColumns);
  finally
    WriteWpEndElement;
  end;
end;

procedure TdxWordProcessingMLBaseExporter.ExportEqualWidthColumns(AColumns: TdxSectionColumns);
var
  ADefaultColumns: TdxColumnsInfo;
begin
  ADefaultColumns := DocumentModel.Cache.ColumnsInfoCache.DefaultItem;
  if AColumns.ColumnCount <> ADefaultColumns.ColumnCount then
    WriteWpIntAttr('num', AColumns.ColumnCount);
  if AColumns.Space <> ADefaultColumns.Space then
    WriteWpIntAttr('space', UnitConverter.ModelUnitsToTwips(AColumns.Space));
  if AColumns.DrawVerticalSeparator <> ADefaultColumns.DrawVerticalSeparator then
    WriteWpBoolAttr('sep', AColumns.DrawVerticalSeparator);
end;

procedure TdxWordProcessingMLBaseExporter.ExportNonUniformColumns(AColumns: TdxSectionColumns);
var
  ACollection: TdxColumnInfoCollection;
  ACount, I: Integer;
  ADefaultColumns: TdxColumnsInfo;
begin
  ACollection := AColumns.GetColumns;
  try
    ACount := ACollection.Count;
    ADefaultColumns := DocumentModel.Cache.ColumnsInfoCache.DefaultItem;
    if ACount <> ADefaultColumns.ColumnCount then
      WriteWpIntAttr('num', ACount);
    for I := 0 to ACount - 1 do
      ExportColumn(ACollection[I]);
  finally
    ACollection.Free;
  end;
end;

procedure TdxWordProcessingMLBaseExporter.ExportColumn(AColumn: TdxColumnInfo);
begin
  WriteWpStartElement('col');
  try
    WriteWpIntAttr('w', UnitConverter.ModelUnitsToTwips(AColumn.Width));
    WriteWpIntAttr('space', UnitConverter.ModelUnitsToTwips(AColumn.Space));
  finally
    WriteWpEndElement;
  end;
end;

procedure TdxWordProcessingMLBaseExporter.ExportSectionPageNumbering(ANumbering: TdxSectionPageNumbering);
var
  ADefaultPageNumbering: TdxPageNumberingInfo;
begin
  if not ShouldExportPageNumbering(ANumbering) then
    Exit;

  ADefaultPageNumbering := DocumentModel.Cache.PageNumberingInfoCache.DefaultItem;

  WriteWpStartElement('pgNumType');
  try
    if not ANumbering.ContinueNumbering then
      WriteWpIntAttr('start', ANumbering.FirstPageNumber);
    if ANumbering.NumberingFormat <> ADefaultPageNumbering.NumberingFormat then
      WriteWpStringAttr('fmt', ConvertNumberFormat(ANumbering.NumberingFormat));
    if ANumbering.ChapterSeparator <> ADefaultPageNumbering.ChapterSeparator then
      WriteWpStringAttr(GetWordProcessingMLValue('chapSep'), ConvertChapterSeparator(ANumbering.ChapterSeparator));
  finally
    WriteWpEndElement;
  end;
end;

function TdxWordProcessingMLBaseExporter.ShouldExportPageNumbering(ANumbering: TdxSectionPageNumbering): Boolean;
var
  ADefaultPageNumbering: TdxPageNumberingInfo;
begin
  ADefaultPageNumbering := DocumentModel.Cache.PageNumberingInfoCache.DefaultItem;
  Result := not ANumbering.ContinueNumbering or (ANumbering.NumberingFormat <> ADefaultPageNumbering.NumberingFormat) or
    (ANumbering.ChapterSeparator <> ADefaultPageNumbering.ChapterSeparator);
end;

procedure TdxWordProcessingMLBaseExporter.ExportSectionLineNumbering(ANumbering: TdxSectionLineNumbering);
var
  ADefaultLineNumbering: TdxLineNumberingInfo;
begin
  if not ShouldExportLineNumbering(ANumbering) then
    Exit;
  ADefaultLineNumbering := DocumentModel.Cache.LineNumberingInfoCache.DefaultItem;
  WriteWpStartElement('lnNumType');
  try
    if ANumbering.StartingLineNumber <> ADefaultLineNumbering.StartingLineNumber then
      WriteWpIntAttr('start', ANumbering.StartingLineNumber);
    if ANumbering.Step <> ADefaultLineNumbering.Step then
      WriteWpIntAttr(GetWordProcessingMLValue('countBy'), ANumbering.Step);
    if ANumbering.Distance <> ADefaultLineNumbering.Distance then
      WriteWpIntAttr('distance', UnitConverter.ModelUnitsToTwips(ANumbering.Distance));
    if ANumbering.NumberingRestartType <> ADefaultLineNumbering.NumberingRestartType then
      WriteWpStringAttr('restart', ConvertLineNumberingRestartType(ANumbering.NumberingRestartType));
  finally
    WriteWpEndElement;
  end;
end;

function TdxWordProcessingMLBaseExporter.ShouldExportLineNumbering(ANumbering: TdxSectionLineNumbering): Boolean;
var
  ADefaultLineNumbering: TdxLineNumberingInfo;
begin
  ADefaultLineNumbering := DocumentModel.Cache.LineNumberingInfoCache.DefaultItem;
  Result := ANumbering.Step <> ADefaultLineNumbering.Step;
end;

procedure TdxWordProcessingMLBaseExporter.ExportSectionFootNote(ANote: TdxSectionFootNote);
begin
  if not ShouldExportSectionFootNote(ANote) then
    Exit;

  ExportSectionFootNoteCore('footnotePr', ANote, DocumentModel.Cache.FootNoteInfoCache[TdxFootNoteInfoCache.DefaultFootNoteItemIndex]);
end;

function TdxWordProcessingMLBaseExporter.ShouldExportSectionFootNote(ANote: TdxSectionFootNote): Boolean;
begin
  Result := DocumentModel.DocumentCapabilities.FootNotesAllowed and (ANote.Index <> TdxFootNoteInfoCache.DefaultFootNoteItemIndex);
end;

procedure TdxWordProcessingMLBaseExporter.ExportSectionEndNote(ANote: TdxSectionFootNote);
begin
  if not ShouldExportSectionEndNote(ANote) then
    Exit;

  ExportSectionFootNoteCore('endnotePr', ANote, DocumentModel.Cache.FootNoteInfoCache[TdxFootNoteInfoCache.DefaultEndNoteItemIndex]);
end;

function TdxWordProcessingMLBaseExporter.ShouldExportSectionEndNote(ANote: TdxSectionFootNote): Boolean;
begin
  Result := DocumentModel.DocumentCapabilities.EndNotesAllowed and (ANote.Index <> TdxFootNoteInfoCache.DefaultEndNoteItemIndex);
end;

procedure TdxWordProcessingMLBaseExporter.ExportSectionFootNoteCore(const ATagName: string; ANote: TdxSectionFootNote; ADefaultInfo: TdxFootNoteInfo);
begin
  WriteWpStartElement(ATagName);
  try
    if ANote.Position <> ADefaultInfo.Position then
      WriteWpStringValue('pos', ConvertFootNotePlacement(ANote.Position));
    if ANote.StartingNumber <> ADefaultInfo.StartingNumber then
      WriteWpIntValue('numStart', ANote.StartingNumber);
    if ANote.NumberingFormat <> ADefaultInfo.NumberingFormat then
      WriteWpStringValue('numFmt', ConvertNumberFormat(ANote.NumberingFormat));
    if ANote.NumberingRestartType <> ADefaultInfo.NumberingRestartType then
      WriteWpStringValue('numRestart', ConvertLineNumberingRestartType(ANote.NumberingRestartType));
  finally
    WriteWpEndElement;
  end;
end;

function TdxWordProcessingMLBaseExporter.ConvertSectionStartType(AValue: TdxSectionStartType): string;
var
  AResult: TdxWordProcessingMLValue;
begin
  if FSectionStartTypeTable.TryGetValue(AValue, AResult) then
    Result := GetWordProcessingMLValue(AResult)
  else
    Result := GetWordProcessingMLValue(FSectionStartTypeTable[TdxSectionStartType.NextPage]);
end;

function TdxWordProcessingMLBaseExporter.ConvertLineNumberingRestartType(AValue: TdxLineNumberingRestart): string;
var
  AResult: TdxWordProcessingMLValue;
begin
  if FLineNumberingRestartTable.TryGetValue(AValue, AResult) then
    Result := GetWordProcessingMLValue(AResult)
  else
    Result := GetWordProcessingMLValue(FLineNumberingRestartTable[TdxLineNumberingRestart.NewPage]);
end;

function TdxWordProcessingMLBaseExporter.ConvertFootNotePlacement(AValue: TdxFootNotePosition): string;
var
  AResult: TdxWordProcessingMLValue;
begin
  if FFootNotePlacementTable.TryGetValue(AValue, AResult) then
    Result := GetWordProcessingMLValue(AResult)
  else
    Result := GetWordProcessingMLValue(FFootNotePlacementTable[TdxFootNotePosition.BottomOfPage]);
end;

function TdxWordProcessingMLBaseExporter.ConvertNumberFormat(AValue: TdxNumberingFormat): string;
var
  AResult: TdxWordProcessingMLValue;
begin
  if FPageNumberingFormatTable.TryGetValue(AValue, AResult) then
    Result := GetWordProcessingMLValue(AResult)
  else
    Result := GetWordProcessingMLValue(FPageNumberingFormatTable[TdxNumberingFormat.Decimal]);
end;

function TdxWordProcessingMLBaseExporter.ConvertChapterSeparator(AValue: Char): string;
var
  AResult: TdxWordProcessingMLValue;
begin
  if FChapterSeparatorsTable.TryGetValue(AValue, AResult) then
    Result := GetWordProcessingMLValue(AResult)
  else
    Result := GetWordProcessingMLValue(FChapterSeparatorsTable['.']);
end;

function TdxWordProcessingMLBaseExporter.ExportTable(ATableInfo: TdxTableInfo): TdxParagraphIndex;
var
  ATable: TdxTable;
begin
  WriteWpStartElement('tbl');
  try
    ATable := ATableInfo.Table;
    if (ATable.TableProperties.Info.Value <> TdxTablePropertiesOptions.MaskUseNone) or (ATable.StyleIndex > 0) then
      ExportTableProperties(ATable)
    else
      WriteWpEmptyElement('tblPr');
    WriteWpEmptyElement('tblGrid');

    Result := inherited ExportTable(ATableInfo);
  finally
    WriteWpEndElement;
  end;
end;

procedure TdxWordProcessingMLBaseExporter.ExportTableProperties(ATable: TdxTable);
begin
  WriteWpStartElement('tblPr');
  try
    if ATable.StyleIndex > 0 then
      WriteWpStringValue('tblStyle', GetTableStyleId(ATable.StyleIndex));
    ExportTablePropertiesCore(ATable.TableProperties);
  finally
    WriteWpEndElement;
  end;
end;

procedure TdxWordProcessingMLBaseExporter.ExportTablePropertiesCore(ATableProperties: TdxTableProperties);
begin
  ExportTablePropertiesCore(ATableProperties, True);
end;

procedure TdxWordProcessingMLBaseExporter.ExportTablePropertiesCore(ATableProperties: TdxTableProperties; AExportTableLayout: Boolean);
begin
  if ShouldExportFloatingPosition(ATableProperties.FloatingPosition) then
    ExportFloatingPosition(ATableProperties.FloatingPosition);
  if ATableProperties.IsTableOverlap <> True then
    ExportIsTableOverlap(ATableProperties.IsTableOverlap);
  if ATableProperties.UseTableStyleRowBandSize then
    WriteWpIntValue('tblStyleRowBandSize', Max(ATableProperties.TableStyleRowBandSize, 1));
  if ATableProperties.UseTableStyleColBandSize then
    WriteWpIntValue('tblStyleColBandSize', Max(ATableProperties.TableStyleColBandSize, 1));
  if ATableProperties.UsePreferredWidth then
    ExportWidthUnitValue('tblW', ATableProperties.PreferredWidth);
  if ATableProperties.UseTableAlignment then
    WriteWpStringValue('jc', ConvertTableRowAlignment(ATableProperties.TableAlignment));
  if ATableProperties.UseCellSpacing then
    ExportWidthUnitValue('tblCellSpacing', ATableProperties.CellSpacing);
  if ATableProperties.UseTableIndent then
    ExportWidthUnitValue('tblInd', ATableProperties.TableIndent);
  ExportTableBorders(ATableProperties.Borders);
  if ATableProperties.BackgroundColor <> TdxAlphaColors.Empty then
    ExportTableBackground(ATableProperties.BackgroundColor);
  if ATableProperties.UseTableLayout and AExportTableLayout then
    ExportTableLayout(ATableProperties.TableLayout);
  ExportCellMargins(ATableProperties);
  if ATableProperties.TableLook <> [] then
    ExportTableLook(ATableProperties.TableLook);
end;

procedure TdxWordProcessingMLBaseExporter.ExportCellMargins(ATableProperties: TdxTableProperties);
var
  ACellMargins: TdxCellMargins;
begin

  if ((not ATableProperties.UseLeftMargin and not ATableProperties.UseTopMargin) and not ATableProperties.UseRightMargin) and not ATableProperties.UseBottomMargin then
    Exit;
  ACellMargins := ATableProperties.CellMargins;
  WriteWpStartElement('tblCellMar');
  try
    if ATableProperties.UseTopMargin then
      ExportCellMargin('top', ACellMargins.Top);
    if ATableProperties.UseLeftMargin then
      ExportCellMargin('left', ACellMargins.Left);
    if ATableProperties.UseBottomMargin then
      ExportCellMargin('bottom', ACellMargins.Bottom);
    if ATableProperties.UseRightMargin then
      ExportCellMargin('right', ACellMargins.Right);
  finally
    WriteWpEndElement;
  end;
end;

procedure TdxWordProcessingMLBaseExporter.ExportCellMargin(const ATag: string; AMargin: TdxMarginUnitBase);
var
  AValue: Integer;
  AType: TdxWidthUnitType;
begin
  WriteWpStartElement(ATag);
  try
    AValue := AMargin.Value;
    AType := AMargin.&Type;
    case AMargin.&Type of
      TdxWidthUnitType.ModelUnits:
        AValue := DocumentModel.UnitConverter.ModelUnitsToTwips(AMargin.Value);
      TdxWidthUnitType.Nil:
        if AValue = 0 then
          AType := TdxWidthUnitType.ModelUnits;
      else
    end;
    WriteWpIntAttr('w', AValue);
    WriteWpStringAttr('type', ConvertWidthUnitTypes(AType));
  finally
    WriteWpEndElement;
  end;
end;

procedure TdxWordProcessingMLBaseExporter.ExportTableBorders(ABorders: TdxTableBorders);
begin
  if not ShouldExportTableBorders(ABorders) then
    Exit;
  WriteWpStartElement('tblBorders');
  try
    if ABorders.UseTopBorder then
      ExportTableBorder('top', ABorders.TopBorder);
    if ABorders.UseLeftBorder then
      ExportTableBorder('left', ABorders.LeftBorder);
    if ABorders.UseBottomBorder then
      ExportTableBorder('bottom', ABorders.BottomBorder);
    if ABorders.UseRightBorder then
      ExportTableBorder('right', ABorders.RightBorder);
    if ABorders.UseInsideHorizontalBorder then
      ExportTableBorder('insideH', ABorders.InsideHorizontalBorder);
    if ABorders.UseInsideVerticalBorder then
      ExportTableBorder('insideV', ABorders.InsideVerticalBorder);
  finally
    WriteWpEndElement;
  end;
end;

function TdxWordProcessingMLBaseExporter.ShouldExportTableBorders(ABorders: TdxTableBorders): Boolean;
begin
  Result :=
    (ABorders.UseBottomBorder and (ABorders.BottomBorder.Style <> TdxBorderLineStyle.&Nil)) or
    (ABorders.UseLeftBorder and (ABorders.LeftBorder.Style <> TdxBorderLineStyle.&Nil)) or
    (ABorders.UseRightBorder and (ABorders.RightBorder.Style <> TdxBorderLineStyle.&Nil)) or
    (ABorders.UseTopBorder and (ABorders.TopBorder.Style <> TdxBorderLineStyle.&Nil)) or
    (ABorders.UseInsideHorizontalBorder and (ABorders.InsideHorizontalBorder.Style <> TdxBorderLineStyle.&Nil)) or
    (ABorders.UseInsideVerticalBorder and (ABorders.InsideVerticalBorder.Style <> TdxBorderLineStyle.&Nil));
end;

procedure TdxWordProcessingMLBaseExporter.ExportTableBorder(const ATag: string; ABorder: TdxBorderBase);
begin
  if ABorder.Style = TdxBorderLineStyle.&Nil then
    Exit;

  WriteWpStartElement(ATag);
  try
    ExportTableBorderCore(ABorder, True);
  finally
    WriteWpEndElement;
  end;
end;

procedure TdxWordProcessingMLBaseExporter.ExportTableCellBorder(const ATag: string; ABorder: TdxBorderBase);
begin
  if ABorder.Style = TdxBorderLineStyle.&Nil then
    Exit;

  WriteWpStartElement(ATag);
  try
    if ABorder.Style = TdxBorderLineStyle.&None then
      WriteWpStringAttr('val', ConvertBorderLineStyle(TdxBorderLineStyle.&Nil))
    else
      ExportTableBorderCore(ABorder, False);
  finally
    WriteWpEndElement;
  end;
end;

procedure TdxWordProcessingMLBaseExporter.ExportTableBorderCore(ABorder: TdxBorderBase; AExportAutoColor: Boolean);
begin
  WriteWpStringAttr('val', ConvertBorderLineStyle(ABorder.Style));
  WriteWpIntAttr('sz', Trunc(UnitConverter.ModelUnitsToPointsF(ABorder.Width * 8.0)));
  WriteWpIntAttr('space', Trunc(UnitConverter.ModelUnitsToPointsF(ABorder.Offset)));
  WriteWpBoolAttr('shadow', ABorder.Shadow);
  WriteWpBoolAttr('frame', ABorder.Frame);
  WriteWpStringAttr('color', ConvertColorToString(ABorder.Color));
end;

function TdxWordProcessingMLBaseExporter.ShouldExportCellMargins(ACellMargins: TdxCellMargins): Boolean;
begin
  Result :=
    not ForbidExportWidthUnit(ACellMargins.Bottom) or
    not ForbidExportWidthUnit(ACellMargins.Left) or
    not ForbidExportWidthUnit(ACellMargins.Top) or
    not ForbidExportWidthUnit(ACellMargins.Right);
end;

procedure TdxWordProcessingMLBaseExporter.ExportCellMargins(const ATag: string; ACellMargins: TdxCellMargins);
begin
  WriteWpStartElement(ATag);
  try
    ExportWidthUnitValue('top', ACellMargins.Top);
    ExportWidthUnitValue('left', ACellMargins.Left);
    ExportWidthUnitValue('bottom', ACellMargins.Bottom);
    ExportWidthUnitValue('right', ACellMargins.Right);
  finally
    WriteWpEndElement;
  end;
end;

procedure TdxWordProcessingMLBaseExporter.ExportWidthUnitValue(const ATag: string; AValue: TdxWidthUnit);
begin
  if ForbidExportWidthUnit(AValue) then
    Exit;
  WriteWpStartElement(ATag);
  try
    ExportWidthUnit(AValue);
  finally
    WriteWpEndElement;
  end;
end;

function TdxWordProcessingMLBaseExporter.ForbidExportWidthUnit(AWidthUnit: TdxWidthUnit): Boolean;
begin
  Result := (AWidthUnit.&Type = TdxWidthUnitType.Nil) and (AWidthUnit.Value = 0);
end;

procedure TdxWordProcessingMLBaseExporter.ExportWidthUnit(AWidthUnit: TdxWidthUnit);
var
  AValue: Integer;
begin
  if AWidthUnit.&Type = TdxWidthUnitType.ModelUnits then
    AValue := DocumentModel.UnitConverter.ModelUnitsToTwips(AWidthUnit.Value)
  else
    AValue := AWidthUnit.Value;
  WriteWpIntAttr('w', AValue);
  WriteWpStringAttr('type', ConvertWidthUnitTypes(AWidthUnit.&Type));
end;

procedure TdxWordProcessingMLBaseExporter.ExportTableLook(ATableLook: TdxTableLookTypes);
var
  B: Byte absolute ATableLook;
  AIntValue: Integer;
begin
  AIntValue := B shl 5;
  WriteWpStringValue('tblLook', IntToHex(AIntValue, 4));
end;

procedure TdxWordProcessingMLBaseExporter.ExportTableLayout(ATableLayout: TdxTableLayoutType);
begin
  WriteWpStartElement('tblLayout');
  try
    WriteWpStringAttr('type', ConvertTableLayoutType(ATableLayout));
  finally
    WriteWpEndElement;
  end;
end;

procedure TdxWordProcessingMLBaseExporter.ExportTableBackground(ABackground: TdxAlphaColor);
begin
  ExportTextShading(ABackground);
end;

procedure TdxWordProcessingMLBaseExporter.ExportRunBackColor(ACharacterProperties: TdxCharacterProperties);
begin
  if ACharacterProperties.UseBackColor then
    if ACharacterProperties.BackColor <> TransparentClr then
      ExportTextShading(ACharacterProperties.BackColor);
end;

procedure TdxWordProcessingMLBaseExporter.ExportTextShading(ABackground: TdxAlphaColor);
begin
  WriteWpStartElement('shd');
  try
    if TdxAlphaColors.IsTransparentOrEmpty(ABackground) then
    begin
      WriteWpStringAttr('val', ConvertShadingPattern(TdxShadingPattern.&Nil));
      WriteWpStringAttr('fill', 'auto');
    end
    else
    begin
      WriteWpStringAttr('val', ConvertShadingPattern(TdxShadingPattern.Clear));
      WriteWpStringAttr('fill', ConvertColorToString(ABackground));
    end;
  finally
    WriteWpEndElement;
  end;
end;

procedure TdxWordProcessingMLBaseExporter.ExportIsTableOverlap(AIsTableOverlap: Boolean);
begin
  if AIsTableOverlap then
    WriteWpStringValue('tblOverlap', 'overlap')
  else
    WriteWpStringValue('tblOverlap', 'never');
end;

function TdxWordProcessingMLBaseExporter.ShouldExportFloatingPosition(AFloatingPosition: TdxTableFloatingPosition): Boolean;
begin

  Result := AFloatingPosition.TextWrapping <> TdxTextWrapping.Never;
end;

procedure TdxWordProcessingMLBaseExporter.ExportFloatingPosition(AFloatingPosition: TdxTableFloatingPosition);
begin
  WriteWpStartElement('tblpPr');
  try
    if AFloatingPosition.BottomFromText <> 0 then
      WriteWpIntAttr('bottomFromText', UnitConverter.ModelUnitsToTwips(AFloatingPosition.BottomFromText));
    if AFloatingPosition.LeftFromText <> 0 then
      WriteWpIntAttr('leftFromText', UnitConverter.ModelUnitsToTwips(AFloatingPosition.LeftFromText));
    if AFloatingPosition.RightFromText <> 0 then
      WriteWpIntAttr('rightFromText', UnitConverter.ModelUnitsToTwips(AFloatingPosition.RightFromText));
    if AFloatingPosition.TopFromText <> 0 then
      WriteWpIntAttr('topFromText', UnitConverter.ModelUnitsToTwips(AFloatingPosition.TopFromText));
    if AFloatingPosition.TableHorizontalPosition <> 0 then
      WriteWpIntAttr('tblpX', UnitConverter.ModelUnitsToTwips(AFloatingPosition.TableHorizontalPosition))
    else
      WriteWpIntAttr('tblpX', 1);
    if AFloatingPosition.TableVerticalPosition <> 0 then
      WriteWpIntAttr('tblpY', UnitConverter.ModelUnitsToTwips(AFloatingPosition.TableVerticalPosition))
    else
      WriteWpIntAttr('tblpY', 1);
    if AFloatingPosition.HorizontalAnchor <> TdxHorizontalAnchorTypes.Column then
      WriteWpStringAttr('horzAnchor', ConvertHorizontalAnchorTypes(AFloatingPosition.HorizontalAnchor));
    if AFloatingPosition.VerticalAnchor <> TdxVerticalAnchorTypes.Margin then
      WriteWpStringAttr('vertAnchor', ConvertVerticalAnchorTypes(AFloatingPosition.VerticalAnchor));
    if AFloatingPosition.IsHorizontalRelativePositionUse then
      WriteWpStringAttr('tblpXSpec', ConvertHorizontalAlignMode(AFloatingPosition.HorizontalAlign));
    if AFloatingPosition.IsVerticalRelativePositionUse then
      WriteWpStringAttr('tblpYSpec', ConvertVerticalAlignMode(AFloatingPosition.VerticalAlign));
  finally
    WriteWpEndElement;
  end;
end;

procedure TdxWordProcessingMLBaseExporter.ExportRow(ARow: TdxTableRow; ATableInfo: TdxTableInfo);
begin
  WriteWpStartElement('tr');
  try
    ExportTablePropertiesException(ARow.TablePropertiesException);
    if ARow.Properties.Info.Value <> TdxTableRowPropertiesOptions.MaskUseNone then
      ExportTableRowProperties(ARow.Properties);
    inherited ExportRow(ARow, ATableInfo);
  finally
    WriteWpEndElement;
  end;
end;

procedure TdxWordProcessingMLBaseExporter.ExportTablePropertiesException(ATableProperties: TdxTableProperties);
var
  AShouldExport: Boolean;
begin
  AShouldExport :=
    ATableProperties.UsePreferredWidth or
    ATableProperties.UseTableAlignment or
    ATableProperties.UseCellSpacing or
    ATableProperties.UseTableIndent or
    ShouldExportTableBorders(ATableProperties.Borders) or
    ATableProperties.UseBackgroundColor or
    ATableProperties.UseTableLayout or
    ShouldExportCellMargins(ATableProperties.CellMargins) or
    (ATableProperties.TableLook <> []);

  if not AShouldExport then
    Exit;

  WriteWpStartElement('tblPrEx');
  try
    if ATableProperties.UsePreferredWidth then
      ExportWidthUnitValue('tblW', ATableProperties.PreferredWidth);
    if ATableProperties.UseTableAlignment then
      WriteWpStringValue('jc', ConvertTableRowAlignment(ATableProperties.TableAlignment));
    if ATableProperties.UseCellSpacing then
      ExportWidthUnitValue('tblCellSpacing', ATableProperties.CellSpacing);
    if ATableProperties.UseTableIndent then
      ExportWidthUnitValue('tblInd', ATableProperties.TableIndent);
    ExportTableBorders(ATableProperties.Borders);
    if ATableProperties.UseBackgroundColor then
      ExportTableBackground(ATableProperties.BackgroundColor);
    if ATableProperties.UseTableLayout then
      ExportTableLayout(ATableProperties.TableLayout);
    if ShouldExportCellMargins(ATableProperties.CellMargins) then
      ExportCellMargins('tblCellMar', ATableProperties.CellMargins);
    if ATableProperties.TableLook <> [] then
      ExportTableLook(ATableProperties.TableLook);
  finally
    WriteWpEndElement;
  end;
end;

procedure TdxWordProcessingMLBaseExporter.ExportTableRowProperties(ARowProperties: TdxTableRowProperties);
begin
  ExportTableRowProperties(ARowProperties, False);
end;

procedure TdxWordProcessingMLBaseExporter.ExportTableRowProperties(ARowProperties: TdxTableRowProperties; AIsStyle: Boolean);
begin
  WriteWpStartElement('trPr');
  try
    if ARowProperties.UseGridBefore then
      WriteWpIntValue('gridBefore', ARowProperties.GridBefore);
    if ARowProperties.UseGridAfter then
      WriteWpIntValue('gridAfter', ARowProperties.GridAfter);
    if ARowProperties.UseWidthBefore and not AIsStyle then
      ExportWidthUnitValue('wBefore', ARowProperties.WidthBefore);
    if ARowProperties.UseWidthAfter and not AIsStyle then
      ExportWidthUnitValue('wAfter', ARowProperties.WidthAfter);
    if ARowProperties.CantSplit then
      WriteWpEmptyElement('cantSplit');
    if ARowProperties.Height.Value <> 0 then
      ExportTableRowHeight(ARowProperties.Height);
    if ARowProperties.Header then
      WriteWpEmptyElement('tblHeader');
    if ARowProperties.UseCellSpacing then
      ExportWidthUnitValue('tblCellSpacing', ARowProperties.CellSpacing);
    if ARowProperties.TableRowAlignment <> TdxTableRowAlignment.Left then
      WriteWpStringValue('jc', ConvertTableRowAlignment(ARowProperties.TableRowAlignment));
    if ARowProperties.HideCellMark then
      WriteWpBoolValue('hidden', ARowProperties.HideCellMark);
  finally
    WriteWpEndElement;
  end;
end;

procedure TdxWordProcessingMLBaseExporter.ExportTableRowHeight(AHeight: TdxHeightUnit);
begin
  WriteWpStartElement('trHeight');
  try
    ExportHeightUnit(AHeight);
  finally
    WriteWpEndElement;
  end;
end;

procedure TdxWordProcessingMLBaseExporter.ExportHeightUnit(AHeightUnit: TdxHeightUnit);
begin
  if AHeightUnit.&Type <> TdxHeightUnitType.Auto then
    WriteWpStringAttr('hRule', ConvertHeightUnitType(AHeightUnit.&Type));
  WriteWpIntAttr('val', DocumentModel.UnitConverter.ModelUnitsToTwips(AHeightUnit.Value));
end;

procedure TdxWordProcessingMLBaseExporter.ExportCell(ACell: TdxTableCell; ATableInfo: TdxTableInfo);
begin
  WriteWpStartElement('tc');
  try
    if AllowExportTableCellProperties(ACell) then
      ExportTableCellProperties(ACell);
    if not ForbidExecuteBaseExportTableCell then
      inherited ExportCell(ACell, ATableInfo);
  finally
    WriteWpEndElement;
  end;
end;

function TdxWordProcessingMLBaseExporter.AllowExportTableCellProperties(ACell: TdxTableCell): Boolean;
var
  ACellProperties: TdxTableCellProperties;
begin
  ACellProperties := ACell.Properties;
  Result :=
    (ACellProperties.Info.Value <> TdxTableCellPropertiesOptions.MaskUseNone) or
    (ACellProperties.ColumnSpan > 1) or
    (ACellProperties.VerticalMerging <> TdxMergingState.None);
end;

procedure TdxWordProcessingMLBaseExporter.ExportTableCellPropertiesCore(ACellProperties: TdxTableCellProperties);
begin
  ExportTableCellPropertiesCore(ACellProperties, True);
end;

procedure TdxWordProcessingMLBaseExporter.ExportTableCellPropertiesCore(ACellProperties: TdxTableCellProperties; AExportBorders: Boolean);
begin
  if ACellProperties.UseCellConditionalFormatting then
    ExportConditionalFormatting(ACellProperties.CellConditionalFormatting);
  if ACellProperties.UsePreferredWidth then
    ExportWidthUnitValue('tcW', ACellProperties.PreferredWidth);
  if ACellProperties.ColumnSpan > 1 then
    WriteWpIntValue('gridSpan', ACellProperties.ColumnSpan);
  if ACellProperties.VerticalMerging <> TdxMergingState.None then
    ExportTableCellPropertiesVerticalMerging(ACellProperties.VerticalMerging);
  if AExportBorders then
    ExportTableCellBorders(ACellProperties.Borders);
  ExportTableCellBackgroundColor(ACellProperties);
  if ACellProperties.UseNoWrap then
    WriteWpEmptyOrFalseValue('noWrap', ACellProperties.NoWrap);
  if ShouldExportCellMargins(ACellProperties.CellMargins) then
    ExportCellMargins('tcMar', ACellProperties.CellMargins);
  if ACellProperties.UseTextDirection then
    WriteWpStringValue('textDirection', ConvertTextDirection(ACellProperties.TextDirection));
  if ACellProperties.UseFitText then
    WriteWpEmptyOrFalseValue('tcFitText', ACellProperties.FitText);
  if ACellProperties.UseVerticalAlignment then
    WriteWpStringValue('vAlign', ConvertVerticalAlignment(ACellProperties.VerticalAlignment));
  if ACellProperties.UseHideCellMark then
    WriteWpEmptyOrFalseValue('hideMark', ACellProperties.HideCellMark);
end;

procedure TdxWordProcessingMLBaseExporter.ExportTableCellProperties(ACellProperties: TdxTableCellProperties);
begin
  ExportTableCellProperties(ACellProperties, True);
end;

procedure TdxWordProcessingMLBaseExporter.ExportTableCellProperties(ACellProperties: TdxTableCellProperties; AExportBorders: Boolean);
begin
  WriteWpStartElement('tcPr');
  try
    ExportTableCellPropertiesCore(ACellProperties, AExportBorders);
  finally
    WriteWpEndElement;
  end;
end;

procedure TdxWordProcessingMLBaseExporter.WriteTableCellStyle(ACell: TdxTableCell);
begin
end;

procedure TdxWordProcessingMLBaseExporter.ExportTableCellProperties(ACell: TdxTableCell);
begin
  WriteWpStartElement('tcPr');
  try
    WriteTableCellStyle(ACell);
    ExportTableCellPropertiesCore(ACell.Properties);
  finally
    WriteWpEndElement;
  end;
end;

procedure TdxWordProcessingMLBaseExporter.ExportTableCellPropertiesVerticalMerging(AVerticalMerging: TdxMergingState);
begin
  WriteWpStringValue('vMerge', ConvertMergingState(AVerticalMerging));
end;

procedure TdxWordProcessingMLBaseExporter.ExportTableCellBackgroundColor(ACellProperties: TdxTableCellProperties);
var
  AColor: TdxAlphaColor;
begin
  if not ACellProperties.UseBackgroundColor and not ACellProperties.UseForegroundColor then
    Exit;
  FDocumentContentWriter.WriteStartElement(WordProcessingPrefix, 'shd', WordProcessingNamespace);
  try
    if ACellProperties.UseShading then
      FDocumentContentWriter.WriteAttributeString(WordProcessingPrefix, 'val', WordProcessingNamespace, ConvertShadingPattern(ACellProperties.Shading))
    else
      FDocumentContentWriter.WriteAttributeString(WordProcessingPrefix, 'val', WordProcessingNamespace, 'clear');
    if ACellProperties.UseForegroundColor and not TdxAlphaColors.IsEmpty(ACellProperties.ForegroundColor) then
      FDocumentContentWriter.WriteAttributeString(WordProcessingPrefix, 'color', WordProcessingNamespace, ConvertColorToString(ACellProperties.ForegroundColor))
    else
      FDocumentContentWriter.WriteAttributeString(WordProcessingPrefix, 'color', WordProcessingNamespace, 'auto');
    AColor := ACellProperties.BackgroundColor;
    if ACellProperties.UseBackgroundColor then
    begin
      if TdxAlphaColors.IsEmpty(AColor) then
        FDocumentContentWriter.WriteAttributeString(WordProcessingPrefix, 'fill', WordProcessingNamespace, 'FFFFFF')
      else
        FDocumentContentWriter.WriteAttributeString(WordProcessingPrefix, 'fill', WordProcessingNamespace, ConvertColorToString(AColor));
    end
    else
      FDocumentContentWriter.WriteAttributeString(WordProcessingPrefix, 'fill', WordProcessingNamespace, 'FFFFFF');
  finally
    FDocumentContentWriter.WriteEndElement;
  end;
end;

procedure TdxWordProcessingMLBaseExporter.ExportTableCellBorders(ABorders: TdxTableCellBorders);
var
  AHasBorders: Boolean;
begin
  AHasBorders :=
    (ABorders.UseBottomBorder and (ABorders.BottomBorder.Style <> TdxBorderLineStyle.&Nil)) or
    (ABorders.UseLeftBorder and (ABorders.LeftBorder.Style <> TdxBorderLineStyle.&Nil)) or
    (ABorders.UseRightBorder and (ABorders.RightBorder.Style <> TdxBorderLineStyle.&Nil)) or
    (ABorders.UseTopBorder and (ABorders.TopBorder.Style <> TdxBorderLineStyle.&Nil)) or
    (ABorders.UseInsideHorizontalBorder and (ABorders.InsideHorizontalBorder.&Style <> TdxBorderLineStyle.Nil)) or
    (ABorders.UseInsideVerticalBorder and (ABorders.InsideVerticalBorder.Style <> TdxBorderLineStyle.&Nil)) or
    (ABorders.UseTopLeftDiagonalBorder and (ABorders.TopLeftDiagonalBorder.Style <> TdxBorderLineStyle.&Nil)) or
    (ABorders.UseTopRightDiagonalBorder and (ABorders.TopRightDiagonalBorder.Style <> TdxBorderLineStyle.&Nil));

  if not AHasBorders then
    Exit;
  WriteWpStartElement('tcBorders');
  try
    if ABorders.UseTopBorder then
      ExportTableCellBorder('top', ABorders.TopBorder);
    if ABorders.UseLeftBorder then
      ExportTableCellBorder('left', ABorders.LeftBorder);
    if ABorders.UseBottomBorder then
      ExportTableCellBorder('bottom', ABorders.BottomBorder);
    if ABorders.UseRightBorder then
      ExportTableCellBorder('right', ABorders.RightBorder);
    if ABorders.UseInsideHorizontalBorder then
      ExportTableCellBorder('insideH', ABorders.InsideHorizontalBorder);
    if ABorders.UseInsideVerticalBorder then
      ExportTableCellBorder('insideV', ABorders.InsideVerticalBorder);
    if ABorders.UseTopLeftDiagonalBorder then
      ExportTableCellBorder('tl2br', ABorders.TopLeftDiagonalBorder);
    if ABorders.UseTopRightDiagonalBorder then
      ExportTableCellBorder('tr2bl', ABorders.TopRightDiagonalBorder);
  finally
    WriteWpEndElement;
  end;
end;

procedure TdxWordProcessingMLBaseExporter.ExportConditionalFormatting(AConditionalFormatting: TdxConditionalTableStyleFormattingTypes);
var
  AIntValue: Word absolute AConditionalFormatting;
  AStrValue: string;
begin
  AStrValue := TdxStringHelper.ToBinary(AIntValue, 12);
  WriteWpStringValue('cnfStyle', AStrValue);
end;

procedure TdxWordProcessingMLBaseExporter.ExportStylesCore;
begin
  WriteWpStartElement('styles');
  try
    ExportDocumentDefaults;
    ExportParagraphStyles;
    ExportCharacterStyles;
    ExportTableStyles;
    ExportTableCellStyles;
    ExportNumberingListStyles;
  finally
    WriteWpEndElement;
  end;
end;

procedure TdxWordProcessingMLBaseExporter.ExportParagraphStyles;
var
  AStyles: TdxParagraphStyleCollection;
  ACount, I: Integer;
begin
  AStyles := DocumentModel.ParagraphStyles;
  ACount := AStyles.Count;
  for I := 0 to ACount - 1 do
    ExportParagraphStyle(I);
end;

procedure TdxWordProcessingMLBaseExporter.ExportCharacterStyles;
var
  AStyles: TdxCharacterStyleCollection;
  ACount, I: Integer;
begin
  AStyles := DocumentModel.CharacterStyles;
  ACount := AStyles.Count;
  for I := 0 to ACount - 1 do
    ExportCharacterStyle(I);
end;

procedure TdxWordProcessingMLBaseExporter.ExportTableStyles;
var
  AStyles: TdxTableStyleCollection;
  ACount, I: Integer;
begin
  AStyles := DocumentModel.TableStyles;
  ACount := AStyles.Count;
  for I := 0 to ACount - 1 do
    ExportTableStyle(I);
end;

procedure TdxWordProcessingMLBaseExporter.ExportNumberingListStyles;
var
  AStyles: TdxNumberingListStyleCollection;
  ACount, I: Integer;
begin
  AStyles := DocumentModel.NumberingListStyles;
  ACount := AStyles.Count;
  for I := 0 to ACount - 1 do
    ExportNumberingListStyle(I);
end;

procedure TdxWordProcessingMLBaseExporter.ExportTableCellStyles;
begin
end;

procedure TdxWordProcessingMLBaseExporter.ExportStyleName(const AStyle: IdxStyle);
begin
  WriteWpStringValue('name', AStyle.StyleName);
end;

procedure TdxWordProcessingMLBaseExporter.ExportCharacterStyle(AStyleIndex: Integer);
var
  AStyle: TdxCharacterStyle;
begin
  AStyle := DocumentModel.CharacterStyles[AStyleIndex];
  if AStyle.Deleted then
    Exit;

  WriteWpStartElement('style');
  try
    WriteWpStringAttr('type', 'character');
    WriteWpStringAttr('styleId', GetCharacterStyleId(AStyleIndex));
    if AStyleIndex = 0 then
      WriteWpBoolAttr('default', True);

    ExportStyleName(AStyle);
    ExportParentCharacterStyle(AStyle);
    ExportLinkedCharacterStyle(AStyle);

    if AStyle.Hidden then
      WriteWpEmptyElement('hidden');
    if AStyle.Semihidden then
      WriteWpEmptyElement('semiHidden');
    WriteWpBoolValueAsTag('qFormat', AStyle.Primary);
    ExportStyleCharacterProperties(AStyle.CharacterProperties);
  finally
    WriteWpEndElement;
  end;
end;

procedure TdxWordProcessingMLBaseExporter.ExportNumberingListStyle(AStyleIndex: Integer);
var
  AStyle: TdxNumberingListStyle;
begin
  AStyle := DocumentModel.NumberingListStyles[AStyleIndex];
  if AStyle.Deleted then
    Exit;

  WriteWpStartElement('style');
  try
    WriteWpStringAttr('type', 'numbering');
    WriteWpStringAttr('styleId', GetNumberingStyleId(AStyleIndex));

    ExportStyleName(AStyle);
    if AStyle.Hidden then
      WriteWpEmptyElement('hidden');
    if AStyle.Semihidden then
      WriteWpEmptyElement('semiHidden');
    WriteWpBoolValueAsTag('qFormat', AStyle.Primary);
    if AStyle.NumberingListIndex >= NumberingListIndexMinValue then
    begin
      WriteWpStartElement('pPr');
      WriteWpStartElement('numPr');
      WriteWpStartElement('numId');
      WriteWpIntAttr('val', GetNumberingListIndexForExport(AStyle.NumberingListIndex));
      WriteWpEndElement;
      WriteWpEndElement;
      WriteWpEndElement;
    end;
  finally
    WriteWpEndElement;
  end;
end;

procedure TdxWordProcessingMLBaseExporter.ExportParentCharacterStyle(AStyle: TdxCharacterStyle);
var
  AParentStyleIndex: Integer;
begin
  if AStyle.Parent <> nil then
  begin
    AParentStyleIndex := DocumentModel.CharacterStyles.IndexOf(AStyle.Parent);
    if AParentStyleIndex >= 0 then
      WriteWpStringValue('basedOn', GetCharacterStyleId(AParentStyleIndex));
  end;
end;

procedure TdxWordProcessingMLBaseExporter.ExportLinkedCharacterStyle(AStyle: TdxCharacterStyle);
var
  AParagraphStyles: TdxParagraphStyleCollection;
  ALinkedStyleIndex: Integer;
begin
  if AStyle.HasLinkedStyle then
  begin
    AParagraphStyles := DocumentModel.ParagraphStyles;
    ALinkedStyleIndex := AParagraphStyles.IndexOf(AStyle.LinkedStyle);
    if ALinkedStyleIndex >= 0 then
      WriteWpStringValue('link', GetParagraphStyleId(ALinkedStyleIndex));
  end;
end;

procedure TdxWordProcessingMLBaseExporter.ExportParagraphStyle(AStyleIndex: Integer);
var
  AStyle: TdxParagraphStyle;
begin
  AStyle := DocumentModel.ParagraphStyles[AStyleIndex];
  if AStyle.Deleted then
    Exit;

  WriteWpStartElement('style');
  try
    WriteWpStringAttr('type', 'paragraph');
    WriteWpStringAttr('styleId', GetParagraphStyleId(AStyleIndex));
    if AStyleIndex = 0 then
      WriteWpBoolAttr('default', True);

    ExportStyleName(AStyle);
    ExportParentParagraphStyle(AStyle);
    ExportNextParagraphStyle(AStyle);
    ExportLinkedParagraphStyle(AStyle);
    if AStyle.AutoUpdate then
      WriteWpBoolValue('autoRedefine', AStyle.AutoUpdate);
    if AStyle.Hidden then
      WriteWpEmptyElement('hidden');
    if AStyle.Semihidden then
      WriteWpEmptyElement('semiHidden');
    WriteWpBoolValueAsTag('qFormat', AStyle.Primary);
    ExportStyleParagraphProperties(AStyle.ParagraphProperties, AStyle.Tabs.Info, AStyle.GetOwnNumberingListIndex,
      AStyle.GetOwnListLevelIndex, AStyle.GetNumberingListIndex);
    ExportStyleCharacterProperties(AStyle.CharacterProperties);
  finally
    WriteWpEndElement;
  end;
end;

procedure TdxWordProcessingMLBaseExporter.ExportParentParagraphStyle(AStyle: TdxParagraphStyle);
var
  AParentStyleIndex: Integer;
begin
  if AStyle.Parent <> nil then
  begin
    AParentStyleIndex := DocumentModel.ParagraphStyles.IndexOf(AStyle.Parent);
    if AParentStyleIndex >= 0 then
      WriteWpStringValue('basedOn', GetParagraphStyleId(AParentStyleIndex));
  end;
end;

procedure TdxWordProcessingMLBaseExporter.ExportNextParagraphStyle(AStyle: TdxParagraphStyle);
var
  ANextStyleIndex: Integer;
begin
  if AStyle.NextParagraphStyle <> nil then
  begin
    ANextStyleIndex := DocumentModel.ParagraphStyles.IndexOf(AStyle.NextParagraphStyle);
    if ANextStyleIndex >= 0 then
      WriteWpStringValue('next', GetParagraphStyleId(ANextStyleIndex));
  end;
end;

procedure TdxWordProcessingMLBaseExporter.ExportLinkedParagraphStyle(AStyle: TdxParagraphStyle);
var
  ACharacterStyles: TdxCharacterStyleCollection;
  ALinkedStyleIndex: Integer;
begin
  if AStyle.HasLinkedStyle then
  begin
    ACharacterStyles := DocumentModel.CharacterStyles;
    ALinkedStyleIndex := ACharacterStyles.IndexOf(AStyle.LinkedStyle);
    if ALinkedStyleIndex >= 0 then
      WriteWpStringValue('link', GetCharacterStyleId(ALinkedStyleIndex));
  end;
end;

procedure TdxWordProcessingMLBaseExporter.ExportTableStyle(AStyleIndex: Integer);
var
  AStyle: TdxTableStyle;
begin
  AStyle := DocumentModel.TableStyles[AStyleIndex];
  if AStyle.Deleted then
    Exit;

  WriteWpStartElement('style');
  try
    WriteWpStringAttr('type', 'table');
    WriteWpStringAttr('styleId', GetTableStyleId(AStyleIndex));
    if AStyleIndex = 0 then
      WriteWpBoolAttr('default', True);

    ExportStyleName(AStyle);
    ExportParentTableStyle(AStyle);
    if AStyle.Hidden then
      WriteWpEmptyElement('hidden');
    if AStyle.Semihidden then
      WriteWpEmptyElement('semiHidden');
    WriteWpBoolValueAsTag('qFormat', AStyle.Primary);
    if AStyle.ParagraphProperties.Info.Options.Value <> TdxParagraphFormattingOptions.MaskUseNone then
      ExportStyleParagraphProperties(AStyle.ParagraphProperties, AStyle.Tabs.Info, NumberingListIndexListIndexNotSetted, 0, NumberingListIndexListIndexNotSetted);

    if AStyle.CharacterProperties.Info.Options.Value <> TdxCharacterFormattingOptions.MaskUseNone then
      ExportStyleCharacterProperties(AStyle.CharacterProperties);

    WriteWpStartElement('tblPr');
    try
      ExportTablePropertiesCore(AStyle.TableProperties, False);
    finally
      WriteWpEndElement;
    end;
    ExportTableRowProperties(AStyle.TableRowProperties, True);
    ExportTableCellProperties(AStyle.TableCellProperties, False);
    if AStyle.HasConditionalStyleProperties then
      ExportTableStyleConditionalProperties(AStyle.ConditionalStyleProperties);
  finally
    WriteWpEndElement;
  end;
end;

procedure TdxWordProcessingMLBaseExporter.ExportParentTableStyle(AStyle: TdxTableStyle);
var
  AParentStyleIndex: Integer;
begin
  if AStyle.Parent <> nil then
  begin
    AParentStyleIndex := DocumentModel.TableStyles.IndexOf(AStyle.Parent);
    if AParentStyleIndex >= 0 then
      WriteWpStringValue('basedOn', GetTableStyleId(AParentStyleIndex));
  end;
end;

procedure TdxWordProcessingMLBaseExporter.ExportTableCellStyle(AStyleIndex: Integer);
var
  AStyle: TdxTableCellStyle;
begin
  AStyle := DocumentModel.TableCellStyles[AStyleIndex];
  if AStyle.Deleted then
    Exit;

  WriteWpStartElement('style');
  try
    WriteWpStringAttr('type', 'tableCell');
    WriteWpStringAttr('styleId', GetTableCellStyleId(AStyleIndex));
    if AStyleIndex = 0 then
      WriteWpBoolAttr('default', True);
    ExportStyleName(AStyle);
    ExportParentTableCellStyle(AStyle);
    if AStyle.Hidden then
      WriteWpEmptyElement('hidden');
    if AStyle.Semihidden then
      WriteWpEmptyElement('semiHidden');
    WriteWpBoolValueAsTag('qFormat', AStyle.Primary);
    if AStyle.ParagraphProperties.Info.Options.Value <> TdxParagraphFormattingOptions.MaskUseNone then
      ExportStyleParagraphProperties(AStyle.ParagraphProperties, AStyle.Tabs.Info, NumberingListIndexListIndexNotSetted, 0, NumberingListIndexListIndexNotSetted);

    if AStyle.CharacterProperties.Info.Options.Value <> TdxCharacterFormattingOptions.MaskUseNone then
      ExportStyleCharacterProperties(AStyle.CharacterProperties);

    ExportTableCellProperties(AStyle.TableCellProperties);
  finally
    WriteWpEndElement;
  end;
end;

procedure TdxWordProcessingMLBaseExporter.ExportParentTableCellStyle(AStyle: TdxTableCellStyle);
var
  AParentStyleIndex: Integer;
begin
  if AStyle.Parent <> nil then
  begin
    AParentStyleIndex := DocumentModel.TableCellStyles.IndexOf(AStyle.Parent);
    if AParentStyleIndex >= 0 then
      WriteWpStringValue('basedOn', GetTableCellStyleId(AParentStyleIndex));
  end;
end;

procedure TdxWordProcessingMLBaseExporter.ExportTableStyleConditionalProperties(ATableConditionalStyleProperties: TdxTableConditionalStyleProperties);
var
  AStyleType: TdxConditionalTableStyleFormattingType;
  AConditionalStyle: TdxTableConditionalStyle;
begin
  for AStyleType in ATableConditionalStyleProperties.StyleTypes do
  begin
    AConditionalStyle := ATableConditionalStyleProperties[AStyleType];
    if AConditionalStyle <> nil then
      ExportTableConditionalStyle(AStyleType, AConditionalStyle);
  end;
end;

procedure TdxWordProcessingMLBaseExporter.ExportTableConditionalStyle(AStyleType: TdxConditionalTableStyleFormattingType; AStyle: TdxTableConditionalStyle);
begin
  WriteWpStartElement('tblStylePr');
  try
    WriteWpStringAttr('type', ConvertConditionalStyleType(AStyleType));
    if AStyle.ParagraphProperties.Info.Options.Value <> TdxParagraphFormattingOptions.MaskUseNone then
      ExportStyleParagraphProperties(AStyle.ParagraphProperties, AStyle.Tabs.Info, NumberingListIndexListIndexNotSetted, 0, NumberingListIndexListIndexNotSetted);

    if AStyle.CharacterProperties.Info.Options.Value <> TdxCharacterFormattingOptions.MaskUseNone then
      ExportStyleCharacterProperties(AStyle.CharacterProperties);
    WriteWpStartElement('tblPr');
    try
      ExportTablePropertiesCore(AStyle.TableProperties);
    finally
      WriteWpEndElement;
    end;
    ExportTableRowProperties(AStyle.TableRowProperties);
    ExportTableCellProperties(AStyle.TableCellProperties);
  finally
    WriteWpEndElement;
  end;
end;

procedure TdxWordProcessingMLBaseExporter.ExportStyleParagraphProperties(AParagraphProperties: TdxParagraphProperties;
  ATabInfo: TdxTabFormattingInfo; AOwnNumberingListIndex: TdxNumberingListIndex; AListLevelIndex: Integer;
  AMergedStyleListIndex: TdxNumberingListIndex);
var
  AParagraphNumberingExporter, ATabsExporter: TdxAction;
begin
  WriteWpStartElement('pPr');
  try
    AParagraphNumberingExporter := nil;
    if (ShouldExportParagraphNumbering(AOwnNumberingListIndex)) or (ShouldExportParagraphNumbering(AMergedStyleListIndex)) then
      AParagraphNumberingExporter :=
        procedure ()
        begin
          ExportParagraphStyleListReference(AOwnNumberingListIndex, AListLevelIndex);
        end;
    ATabsExporter :=
      procedure ()
      begin
        ExportTabProperties(ATabInfo);
      end;
    ExportParagraphPropertiesCore(AParagraphProperties, False, nil, AParagraphNumberingExporter, ATabsExporter);
  finally
    WriteWpEndElement;
  end;
end;

procedure TdxWordProcessingMLBaseExporter.ExportStyleCharacterProperties(ACharacterProperties: TdxCharacterProperties);
begin
  WriteWpStartElement('rPr');
  try
    ExportRunPropertiesCore(ACharacterProperties);
  finally
    WriteWpEndElement;
  end;
end;

procedure TdxWordProcessingMLBaseExporter.WriteSettingsCore;
var
  AProperties: TdxDocumentProperties;
begin
  AProperties := DocumentModel.DocumentProperties;
  WriteWpBoolValue('displayBackgroundShape', AProperties.DisplayBackgroundShape);
  ExportDocumentProtectionSettings;
  WriteWpIntValue('defaultTabStop', UnitConverter.ModelUnitsToTwips(AProperties.DefaultTabWidth));
  WriteWpBoolValue('autoHyphenation', AProperties.HyphenateDocument);
  WriteWpBoolValue('evenAndOddHeaders', AProperties.DifferentOddAndEvenPages);
  ExportDocumentVariablesSettings;
  WriteWpEmptyElement('clrSchemeMapping');
end;

procedure TdxWordProcessingMLBaseExporter.ExportDocumentProtectionSettings;
var
  AProperties: TdxDocumentProtectionProperties;
  AVal: TdxWordProcessingMLValue;
begin
  AProperties := DocumentModel.ProtectionProperties;
  if not AProperties.EnforceProtection then
    Exit;
  WriteWpStartElement('documentProtection');
  try
    WriteWpBoolAttr('enforcement', AProperties.EnforceProtection);
    if AProperties.ProtectionType = TdxDocumentProtectionType.ReadOnly then
    begin
      AVal := TdxWordProcessingMLValue.Create('readOnly', 'read-only');
      WriteWpStringAttr('edit', GetWordProcessingMLValue(AVal));
    end;
    ExportDocumentProtectionSettingsCore;
  finally
    WriteWpEndElement;
  end;
end;

procedure TdxWordProcessingMLBaseExporter.ExportDocumentVariablesSettings;
var
  AVariables: TdxDocumentVariableCollection;
  AName: string;
begin
  AVariables := DocumentModel.Variables;
  if AVariables.Count = 0 then
    Exit;

  WriteWpStartElement('docVars');
  try
    for AName in AVariables.GetVariableNames do
    begin
      WriteWpStartElement('docVar');
      try
        WriteWpStringAttr('name', AName);
        WriteWpStringAttr('val', EncodeVariableValue(AVariables[AName].ToString));
      finally
        WriteWpEndElement;
      end;
    end;
  finally
    WriteWpEndElement;
  end;
end;

function TdxWordProcessingMLBaseExporter.EncodeVariableValue(const AValue: string): string;
begin
  Result := TdxXmlBasedExporterUtils.EncodeXmlChars(AValue);
end;

procedure TdxWordProcessingMLBaseExporter.ExportNumberingCore;
var
  ATag: TdxWordProcessingMLValue;
begin
  ATag := TdxWordProcessingMLValue.Create('numbering', 'lists');
  WriteWpStartElement(GetWordProcessingMLValue(ATag));
  try
    ExportAbstractNumberingLists;
    ExportNumberingLists;
  finally
    WriteWpEndElement;
  end;
end;

procedure TdxWordProcessingMLBaseExporter.ExportAbstractNumberingLists;
var
  ALists: TdxAbstractNumberingListCollection;
  ACount, I: Integer;
  AIndex: TdxAbstractNumberingListIndex;
begin
  ALists := DocumentModel.AbstractNumberingLists;
  ACount := ALists.Count;
  for I := 0 to ACount - 1 do
  begin
    AIndex := I;
    ExportAbstractNumberingList(ALists[AIndex], I);
  end;
end;

procedure TdxWordProcessingMLBaseExporter.ExportNumberingLists;
var
  ALists: TdxNumberingListCollection;
  ACount, I: Integer;
begin
  ALists := DocumentModel.NumberingLists;
  ACount := ALists.Count;
  for I := 0 to ACount - 1 do
    ExportNumberingList(ALists[I], I);
end;

procedure TdxWordProcessingMLBaseExporter.ExportOverrideLevels(ALevels: TdxListLevelCollection);
var
  ACount, I: Integer;
begin
  ACount := ALevels.Count;
  for I := 0 to ACount - 1 do
  begin
    if ALevels[I].OverrideStart or (ALevels[I] is TdxOverrideListLevel) then
      ExportLevelOverride(ALevels[I], I);
  end;
end;

procedure TdxWordProcessingMLBaseExporter.ExportLevelOverride(ALevel: TdxAbstractListLevel; ALevelIndex: Integer);
begin
  WriteWpStartElement('lvlOverride');
  try
    WriteWpIntAttr('ilvl', ALevelIndex);
    if ALevel.OverrideStart then
      ExportStartOverride(ALevel.NewStart);
    if ALevel is TdxOverrideListLevel then
      ExportLevel(ALevel, ALevelIndex);
  finally
    WriteWpEndElement;
  end;
end;

procedure TdxWordProcessingMLBaseExporter.ExportStartOverride(ANewStart: Integer);
begin
  WriteWpIntValue('startOverride', ANewStart);
end;

procedure TdxWordProcessingMLBaseExporter.ExportLevel(const ALevel: IdxListLevel; ALevelIndex: Integer);
begin
  WriteWpStartElement('lvl');
  try
    WriteWpIntAttr('ilvl', ALevelIndex);
    if ALevel.ListLevelProperties.TemplateCode <> 0 then
      WriteWpStringAttr('tplc', ConvertToHexBinary(ALevel.ListLevelProperties.TemplateCode));
    ExportLevelProperties(ALevel, ALevelIndex);
    ExportLevelParagraphProperties(ALevel.ParagraphProperties, ALevel.Tabs);
    ExportLevelCharacterProperties(ALevel.CharacterProperties);
  finally
    WriteWpEndElement;
  end;
end;

procedure TdxWordProcessingMLBaseExporter.ExportAbstractLevelProperties(ALevel: TdxListLevel);
var
  AParagraphStyleIndex: Integer;
begin
  AParagraphStyleIndex := ALevel.ParagraphStyleIndex;
  if (AParagraphStyleIndex < 0) or (AParagraphStyleIndex >= DocumentModel.ParagraphStyles.Count) then
    Exit;
  WriteWpStringValue('pStyle', GetParagraphStyleId(AParagraphStyleIndex));
end;

procedure TdxWordProcessingMLBaseExporter.ExportLevelParagraphProperties(AProperties: TdxParagraphProperties; ATabs: TdxTabProperties);
begin
  WriteWpStartElement('pPr');
  try
    ExportParagraphPropertiesCore(AProperties, False, nil, nil, nil);
    ExportTabProperties(ATabs.Info);
  finally
    WriteWpEndElement;
  end;
end;

procedure TdxWordProcessingMLBaseExporter.ExportLevelCharacterProperties(AProperties: TdxCharacterProperties);
begin
  WriteWpStartElement('rPr');
  try
    ExportRunPropertiesCore(AProperties);
  finally
    WriteWpEndElement;
  end;
end;

procedure TdxWordProcessingMLBaseExporter.ExportLevels(ALevels: TdxListLevelCollection);
var
  ACount, I: Integer;
begin
  ACount := ALevels.Count;
  for I := 0 to ACount - 1 do
    ExportLevel(ALevels[I], I);
end;

procedure TdxWordProcessingMLBaseExporter.ExportLevelProperties(const ALevel: IdxListLevel; ALevelIndex: Integer);
var
  AProperties: TdxListLevelProperties;
  AAbstractLevel: TdxListLevel;
begin
  AProperties := ALevel.ListLevelProperties;
  WriteWpIntValue('start', AProperties.Start);
  ExportNumberFormatValue(AProperties);
  if AProperties.SuppressRestart then
    WriteWpIntValue('lvlRestart', 0)
  else
    if AProperties.RelativeRestartLevel <> 0 then
      WriteWpIntValue('lvlRestart', ALevelIndex - AProperties.RelativeRestartLevel);
  AAbstractLevel := TdxListLevel(ALevel);
  if AAbstractLevel <> nil then
    ExportAbstractLevelProperties(AAbstractLevel);
  if AProperties.ConvertPreviousLevelNumberingToDecimal then
    WriteWpBoolValue('isLgl', AProperties.ConvertPreviousLevelNumberingToDecimal);
  WriteWpStringValue('suff', ConvertNumberingSeparator(AProperties.Separator));
  WriteWpStringValue('lvlText', ConvertFormatString(AProperties.DisplayFormatString));
  if AProperties.Legacy then
  begin
    WriteWpStartElement('legacy');
    try
      WriteWpIntAttr('legacy', 1);
      WriteWpIntAttr('legacyIndent', UnitConverter.ModelUnitsToTwips(AProperties.LegacyIndent));
      WriteWpIntAttr('legacySpace', UnitConverter.ModelUnitsToTwips(AProperties.LegacySpace));
    finally
      WriteWpEndElement;
    end;
  end;
  WriteWpStringValue('lvlJc', ConvertListNumberAlignment(AProperties.Alignment));
end;

procedure TdxWordProcessingMLBaseExporter.ExportFloatingObjectAnchorRun(ARun: TdxFloatingObjectAnchorRun);
begin
  WriteWpStartElement('r');
  try
    ExportImageReference(ARun);
  finally
    WriteWpEndElement;
  end;
end;

procedure TdxWordProcessingMLBaseExporter.ExportInlinePictureRun(ARun: TdxInlinePictureRun);
var
  AInfo: TdxCharacterFormattingBase;
begin
  WriteWpStartElement('r');
  try
    AInfo := ARun.CharacterProperties.Info;
    if TdxInlinePictureRun.ShouldExportInlinePictureRunCharacterProperties(AInfo.Info, AInfo.Options) then
      ExportRunProperties(ARun);
    ExportInlinePictureImageReference(ARun);
  finally
    WriteWpEndElement;
  end;
end;

procedure TdxWordProcessingMLBaseExporter.ExportInlinePictureImageReference(ARun: TdxInlinePictureRun);
begin
  ExportImageReference(ARun);
end;

function TdxWordProcessingMLBaseExporter.GenerateImageId: string;
begin
  Inc(FImageCounter);
  Result := 'image' + IntToStr(ImageCounter);
end;

function TdxWordProcessingMLBaseExporter.GenerateImageRelationId(const AImageId: string): string;
begin
  Result := 'Rel' + AImageId;
end;

function TdxWordProcessingMLBaseExporter.ConvertColorToString(AValue: TdxAlphaColor): string;
begin
  if TdxAlphaColors.IsTransparentOrEmpty(AValue) then
    Result := 'auto'
  else
    Result := Format('%.2x%.2x%.2x', [TdxAlphaColors.R(AValue), TdxAlphaColors.G(AValue), TdxAlphaColors.B(AValue)]);
end;

function TdxWordProcessingMLBaseExporter.ConvertToHexString(AValue: Integer): string;
begin
  Result := Format('%x', [AValue]);
end;

function TdxWordProcessingMLBaseExporter.ConvertToHexBinary(ANumber: Integer): string;
begin
  Result := Format('%.8x',[ANumber]);
end;

function TdxWordProcessingMLBaseExporter.ConvertBackColorToString(AValue: TdxAlphaColor): string;
var
  AResult: TdxWordProcessingMLValue;
  ABestMatchColor, ACurrentTableBackgroundColor: TdxAlphaColor;
begin
  if TdxAlphaColors.IsTransparentOrEmpty(AValue) then
    Exit(GetWordProcessingMLValue(FPredefinedBackgroundColors[TdxAlphaColors.Empty]));

  if FPredefinedBackgroundColors.TryGetValue(AValue, AResult) then
    Exit(GetWordProcessingMLValue(AResult));

  ABestMatchColor := TdxAlphaColors.CalculateNearestColor(FPredefinedBackgroundColors.Keys.ToArray, AValue);

  ACurrentTableBackgroundColor := CurrentTableBackgroundColor;
  if not TdxAlphaColors.IsTransparentOrEmpty(ACurrentTableBackgroundColor) and (ACurrentTableBackgroundColor = AValue) and
    (ABestMatchColor <> AValue) then
    Exit('');

  Result := GetWordProcessingMLValue(FPredefinedBackgroundColors[ABestMatchColor]);
end;

function TdxWordProcessingMLBaseExporter.ConvertScript(AScript: TdxCharacterFormattingScript): string;
begin
  case AScript of
    TdxCharacterFormattingScript.Subscript:
      Result := 'subscript';
    TdxCharacterFormattingScript.Superscript:
      Result := 'superscript';
    else
      Result := 'baseline';
  end;
end;

function TdxWordProcessingMLBaseExporter.ConvertUnderlineType(AUnderline: TdxUnderlineType): string;
var
  AValue: TdxWordProcessingMLValue;
begin
  if FUnderlineTable.TryGetValue(AUnderline, AValue) then
    Result := GetWordProcessingMLValue(AValue)
  else
    Result := GetWordProcessingMLValue(FUnderlineTable[TdxUnderlineType.Single]);
end;

function TdxWordProcessingMLBaseExporter.ConvertFormatString(const AValue: string): string;
begin
  Result := Format(AValue, ['%1', '%2', '%3', '%4', '%5', '%6', '%7', '%8', '%9']);
end;

function TdxWordProcessingMLBaseExporter.ConvertListNumberAlignment(AValue: TdxListNumberAlignment): string;
var
  AResult: TdxWordProcessingMLValue;
begin
  if FListNumberAlignmentTable.TryGetValue(AValue, AResult) then
    Result := GetWordProcessingMLValue(AResult)
  else
    Result := GetWordProcessingMLValue(FListNumberAlignmentTable[TdxListNumberAlignment.Left]);
end;

function TdxWordProcessingMLBaseExporter.ConvertNumberingSeparator(AValue: Char): string;
var
  AResult: TdxWordProcessingMLValue;
begin
  if FListNumberSeparatorTable.TryGetValue(AValue, AResult) then
    Result := GetWordProcessingMLValue(AResult)
  else
    Result := 'nothing';
end;

function TdxWordProcessingMLBaseExporter.ConvertNumberingListType(AValue: TdxNumberingType): string;
var
  AResult: TdxWordProcessingMLValue;
begin
  if FNumberingListTypeTable.TryGetValue(AValue, AResult) then
    Result := GetWordProcessingMLValue(AResult)
  else
    Result := GetWordProcessingMLValue(FNumberingListTypeTable[TdxNumberingType.Bullet]);
end;

procedure TdxWordProcessingMLBaseExporter.ExportFieldCodeStartRun(ARun: TdxFieldCodeStartRun);
begin
  ExportFieldChar(ARun, 'begin', True);
  Inc(FFieldCodeDepth);
end;

procedure TdxWordProcessingMLBaseExporter.ExportFieldCodeEndRun(ARun: TdxFieldCodeEndRun);
begin
  ExportFieldChar(ARun, 'separate', False);
  Dec(FFieldCodeDepth);
end;

procedure TdxWordProcessingMLBaseExporter.ExportFieldResultEndRun(ARun: TdxFieldResultEndRun);
begin
  ExportFieldChar(ARun, 'end', False);
end;

procedure TdxWordProcessingMLBaseExporter.ExportFieldChar(ARun: TdxTextRun; const AFieldCharType: string; AWriteDisableUpdate: Boolean);
begin
  WriteWpStartElement('r');
  try
    ExportRunProperties(ARun);
    WriteWpStartElement('fldChar');
    try
      WriteWpStringAttr('fldCharType', AFieldCharType);
      if AWriteDisableUpdate and PieceTable.FindFieldByRunIndex(ARun.GetRunIndex).DisableUpdate then
        WriteWpBoolAttr('disableUpdate', True);
    finally
      WriteWpEndElement;
    end;
  finally
    WriteWpEndElement;
  end;
end;

procedure TdxWordProcessingMLBaseExporter.ExportRangePermissionStart(ARangePermission: TdxRangePermission);
 var
   AId: string;
begin
   WriteWpStartElement('permStart');
   try
     AId := IntToStr(FRangePermissionCounter);
     Inc(FRangePermissionCounter);
     FRangePermissionIdMap.Add(ARangePermission, AId);

    WriteWpStringAttr('id', AId);
    if ARangePermission.UserName <> '' then
      WriteWpStringAttr('ed', ARangePermission.UserName);
    if ARangePermission.Group <> '' then
      WriteWpStringAttr('edGrp', GetGroupName(ARangePermission.Group));
   finally
     WriteWpEndElement;
   end;
end;

function TdxWordProcessingMLBaseExporter.GetGroupName(const AGroupName: string): string;
var
  AResult: string;
begin
  if FPredefinedGroupNames.TryGetValue(AGroupName, AResult) then
    Exit(AResult);
  Result := AGroupName;
end;

procedure TdxWordProcessingMLBaseExporter.ExportRangePermissionEnd(ARangePermission: TdxRangePermission);
var
  AId: string;
begin
  if not FRangePermissionIdMap.TryGetValue(ARangePermission, AId) then
    Exit;
  FRangePermissionIdMap.Remove(ARangePermission);
  WriteWpStartElement('permEnd');
  try
    WriteWpStringAttr('id', AId);
  finally
    WriteWpEndElement;
  end;
end;

procedure TdxWordProcessingMLBaseExporter.ExportFootNoteRun(ARun: TdxFootNoteRun);
begin

  if not DocumentModel.DocumentCapabilities.FootNotesAllowed then
    Exit;

  WriteWpStartElement('r');
  try
    ExportRunProperties(ARun);
    if PieceTable.IsFootNote then
      ExportFootNoteSelfReference(ARun)
    else
      ExportFootNoteReference(ARun);
  finally
    WriteWpEndElement;
  end;
end;

procedure TdxWordProcessingMLBaseExporter.ExportFootNoteReference(ARun: TdxFootNoteRun);
begin
  ExportFootNoteCore(DocumentContentWriter, ARun.Note, -1);
end;

procedure TdxWordProcessingMLBaseExporter.ExportFootNoteSelfReference(ARun: TdxFootNoteRun);
begin
  WriteWpStartElement('footnoteRef');
  WriteWpEndElement;
end;

procedure TdxWordProcessingMLBaseExporter.ExportFootNoteCore(AWriter: TdxXmlWriter; AFootNote: TdxFootNoteBase; AId: Integer);
begin
  ExportFootNoteCore(AWriter, AFootNote, AId, 'footnote');
end;

procedure TdxWordProcessingMLBaseExporter.ExportFootNoteCore(AWriter: TdxXmlWriter; ANote: TdxFootNoteBase; AId: Integer; const ATagName: string);
var
  AOldWriter: TdxXmlWriter;
begin
  AOldWriter := DocumentContentWriter;
  try
    DocumentContentWriter := AWriter;
    WriteWpStartElement(ATagName);
    try
      if AId >= 0 then
        WriteWpIntAttr('id', AId);
      PerformExportPieceTable(TdxPieceTable(ANote.PieceTable), ExportPieceTable);
    finally
      WriteWpEndElement;
    end;
  finally
    DocumentContentWriter := AOldWriter;
  end;
end;

procedure TdxWordProcessingMLBaseExporter.ExportEndNoteRun(ARun: TdxEndNoteRun);
begin

  if not DocumentModel.DocumentCapabilities.EndNotesAllowed then
    Exit;

  WriteWpStartElement('r');
  try
    ExportRunProperties(ARun);
    if PieceTable.IsEndNote then
      ExportEndNoteSelfReference(ARun)
    else
      ExportEndNoteReference(ARun);
  finally
    WriteWpEndElement;
  end;
end;

procedure TdxWordProcessingMLBaseExporter.ExportEndNoteReference(ARun: TdxEndNoteRun);
begin
  ExportEndNoteCore(DocumentContentWriter, ARun.Note, -1);
end;

procedure TdxWordProcessingMLBaseExporter.ExportEndNoteSelfReference(ARun: TdxEndNoteRun);
begin
  WriteWpStartElement('endnoteRef');
  WriteWpEndElement;
end;

procedure TdxWordProcessingMLBaseExporter.ExportEndNoteCore(AWriter: TdxXmlWriter; AEndNote: TdxFootNoteBase; AId: Integer);
begin
  ExportFootNoteCore(AWriter, AEndNote, AId, 'endnote');
end;

function TdxWordProcessingMLBaseExporter.GetParagraphStyleId(AStyleIndex: Integer): string;
begin
  Result := 'P' + IntToStr(AStyleIndex);
end;

function TdxWordProcessingMLBaseExporter.GetCharacterStyleId(AStyleIndex: Integer): string;
begin
  Result := 'C' + IntToStr(AStyleIndex);
end;

function TdxWordProcessingMLBaseExporter.GetTableStyleId(AStyleIndex: Integer): string;
begin
  Result := 'T' + IntToStr(AStyleIndex);
end;

function TdxWordProcessingMLBaseExporter.GetTableCellStyleId(AStyleIndex: Integer): string;
begin
  Result := 'TC' + IntToStr(AStyleIndex);
end;

function TdxWordProcessingMLBaseExporter.GetNumberingStyleId(AStyleIndex: Integer): string;
begin
  Result := 'N' + IntToStr(AStyleIndex);
end;

function TdxWordProcessingMLBaseExporter.GetWordProcessingMLValue(const AOpenXmlValue: string): string;
begin
  Result := GetWordProcessingMLValue(TdxWordProcessingMLValue.Create(AOpenXmlValue));
end;

function TdxWordProcessingMLBaseExporter.ExportImageStyle(AFloatingObjectProperties: TdxFloatingObjectProperties;
  ATextBoxContent: TdxTextBoxFloatingObjectContent; AShape: TdxShape): string;
var
  AFinalWidth, AFinalHeight, ATopDistance, ABottomDistance, ALeftDistance, ARightDistance, AOffsetX, AOffsetY: Single;
  AZOrder: Integer;
  AHPositionType, AVPositionType, AHPositionAlignment, AVPositionAlignment, ARelativeWidth, ARelativeHeight: string;
  ATextBoxProperties: TdxTextBoxProperties;
begin
  AFinalWidth := UnitConverter.ModelUnitsToPointsF(AFloatingObjectProperties.ActualSize.Width);
  AFinalHeight := UnitConverter.ModelUnitsToPointsF(AFloatingObjectProperties.ActualSize.Height);
  AZOrder := AFloatingObjectProperties.ZOrder;
  ATopDistance := UnitConverter.ModelUnitsToPointsF(AFloatingObjectProperties.TopDistance);
  ABottomDistance := UnitConverter.ModelUnitsToPointsF(AFloatingObjectProperties.BottomDistance);
  ALeftDistance := UnitConverter.ModelUnitsToPointsF(AFloatingObjectProperties.LeftDistance);
  ARightDistance := UnitConverter.ModelUnitsToPointsF(AFloatingObjectProperties.RightDistance);
  AOffsetX := UnitConverter.ModelUnitsToPointsF(AFloatingObjectProperties.Offset.X);
  AOffsetY := UnitConverter.ModelUnitsToPointsF(AFloatingObjectProperties.Offset.Y);
  AHPositionType := GetHorizontalPositionType(AFloatingObjectProperties.HorizontalPositionType);
  AVPositionType := GetVerticalPositionType(AFloatingObjectProperties.VerticalPositionType);
  AHPositionAlignment := GetHorizontalPositionAlignment(AFloatingObjectProperties.HorizontalPositionAlignment);
  AVPositionAlignment := GetVerticalPositionAlignment(AFloatingObjectProperties.VerticalPositionAlignment);
  if AFloatingObjectProperties.UseRelativeWidth then
    ARelativeWidth := GetRelativeWidth(AFloatingObjectProperties.RelativeWidth)
  else
    ARelativeWidth := '';
  if AFloatingObjectProperties.UseRelativeHeight then
    ARelativeHeight := GetRelativeHeight(AFloatingObjectProperties.RelativeHeight)
  else
    ARelativeHeight := '';
  Result := TdxStringHelper.Format('position:absolute;width:%gpt;height:%gpt;z-index:%d;mso-wrap-distance-left:%gpt;' +
    'mso-wrap-distance-top:%gpt;mso-wrap-distance-right:%gpt;mso-wrap-distance-bottom:%gpt;margin-left:%gpt;margin-top:%gpt;' +
    'mso-position-horizontal:%s;mso-position-horizontal-relative:%s;mso-position-vertical:%s;mso-position-vertical-relative:%s',
    [AFinalWidth, AFinalHeight, AZOrder, ALeftDistance,
    ATopDistance, ARightDistance, ABottomDistance, AOffsetX, AOffsetY,
    AHPositionAlignment, AHPositionType, AVPositionAlignment, AVPositionType]);
  if ARelativeWidth <> '' then
    Result := Result + ';' + ARelativeWidth;
  if ARelativeHeight <> '' then
    Result := Result + ';' + ARelativeHeight;
  if AFloatingObjectProperties.UsePercentOffset then
  begin
    if AFloatingObjectProperties.PercentOffsetX <> 0 then
      Result := Result + ';' +
        TdxStringHelper.Format('mso-left-percent:%g', [AFloatingObjectProperties.PercentOffsetX / 100]);
    if AFloatingObjectProperties.PercentOffsetY <> 0 then
      Result := Result + ';' +
        TdxStringHelper.Format('mso-top-percent:%g', [AFloatingObjectProperties.PercentOffsetY / 100]);
  end;

  if ATextBoxContent <> nil then
  begin
    ATextBoxProperties := ATextBoxContent.TextBoxProperties;
    if ATextBoxProperties.UseWrapText then
      Result := Result + ';mso-wrap-style:' + GetWrapText(ATextBoxProperties.WrapText);
    if ATextBoxProperties.UseVerticalAlignment then
      Result := Result + ';v-text-anchor:' + GetVerticalAlignment(ATextBoxProperties.VerticalAlignment);
  end;
  if (AShape <> nil) and AShape.UseRotation then
  begin
    if AShape.Rotation mod 60000 = 0 then
      Result := Result + ';rotation:' + IntToStr(UnitConverter.ModelUnitsToDegree(AShape.Rotation))
    else
      Result := Result + ';rotation:' + IntToStr(UnitConverter.ModelUnitsToFD(AShape.Rotation)) + 'fd';
  end;
end;

function TdxWordProcessingMLBaseExporter.GetVerticalAlignment(AVerticalAlignment: TdxVerticalAlignment): string;
var
  AVAlignment: TdxWordProcessingMLValue;
begin
  if FTextBoxVerticalAlignmentTable.TryGetValue(AVerticalAlignment, AVAlignment) then
  begin
    if AVAlignment.WordMLValue = 'just' then
      Result := 'top'
    else
      Result := AVAlignment.WordMLValue;
  end
  else
    Result := 'top';
end;

function TdxWordProcessingMLBaseExporter.GetWrapText(AWrapText: Boolean): string;
begin
  if AWrapText then
    Result := 'square'
  else
    Result := 'none';
end;

function TdxWordProcessingMLBaseExporter.GetRelativeWidth(const ARelativeWidth: TdxFloatingObjectRelativeWidth): string;
begin
  if ARelativeWidth.From <> TdxFloatingObjectRelativeFromHorizontal.Page then
    Result := Format('mso-width-relative:%s;mso-width-percent:%d', [GetRelativeSizeFromHorizontal(ARelativeWidth), ARelativeWidth.Width div 100])
  else
    Result := Format('mso-width-percent:%d', [ARelativeWidth.Width div 100]);
end;

function TdxWordProcessingMLBaseExporter.GetRelativeHeight(const ARelativeHeight: TdxFloatingObjectRelativeHeight): string;
begin
  if ARelativeHeight.From <> TdxFloatingObjectRelativeFromVertical.Page then
    Result := Format('mso-height-relative:%s;mso-height-percent:%d', [GetRelativeSizeFromVertical(ARelativeHeight), ARelativeHeight.Height div 100])
  else
    Result := Format('mso-height-percent:%d', [ARelativeHeight.Height div 100]);
end;

function TdxWordProcessingMLBaseExporter.GetRelativeSizeFromHorizontal(const ARelativeWidth: TdxFloatingObjectRelativeWidth): string;
var
  ARelativeFrom: TdxWordProcessingMLValue;
begin
  if FFloatingObjectCssRelativeFromHorizontalTable.TryGetValue(ARelativeWidth.From, ARelativeFrom) then
    Result := ARelativeFrom.WordMLValue
  else
    Result := FFloatingObjectCssRelativeFromHorizontalTable[TdxFloatingObjectRelativeFromHorizontal.Margin].WordMLValue;
end;

function TdxWordProcessingMLBaseExporter.GetRelativeSizeFromVertical(const ARelativeHeight: TdxFloatingObjectRelativeHeight): string;
var
  ARelativeFrom: TdxWordProcessingMLValue;
begin
  if FFloatingObjectCssRelativeFromVerticalTable.TryGetValue(ARelativeHeight.From, ARelativeFrom) then
    Result := ARelativeFrom.WordMLValue
  else
    Result := FFloatingObjectCssRelativeFromVerticalTable[TdxFloatingObjectRelativeFromVertical.Margin].WordMLValue;
end;

function TdxWordProcessingMLBaseExporter.GetVerticalPositionAlignment(
  AFloatingObjectVerticalPositionAlignment: TdxFloatingObjectVerticalPositionAlignment): string;
var
  AHPositionAlignment: TdxWordProcessingMLValue;
begin
  if FFloatingObjectVerticalPositionAlignmentTable.TryGetValue(AFloatingObjectVerticalPositionAlignment, AHPositionAlignment) then
    Result := AHPositionAlignment.WordMLValue
  else
    Result := 'absolute';
end;

function TdxWordProcessingMLBaseExporter.GetHorizontalPositionAlignment(
  AFloatingObjectHorizontalPositionAlignment: TdxFloatingObjectHorizontalPositionAlignment): string;
var
  AVPositionAlignment: TdxWordProcessingMLValue;
begin
  if FFloatingObjectHorizontalPositionAlignmentTable.TryGetValue(AFloatingObjectHorizontalPositionAlignment, AVPositionAlignment) then
    Result := AVPositionAlignment.WordMLValue
  else
    Result := 'absolute';
end;

function TdxWordProcessingMLBaseExporter.GetVerticalPositionType(AFloatingObjectVerticalPositionType: TdxFloatingObjectVerticalPositionType): string;
var
  AVPositionType: TdxWordProcessingMLValue;
begin
  if VerticalPositionTypeAttributeTable.TryGetValue(AFloatingObjectVerticalPositionType, AVPositionType) then
    Result := AVPositionType.OpenXmlValue
  else
    Result := 'margin';
end;

function TdxWordProcessingMLBaseExporter.GetHorizontalPositionType(AFloatingObjectHorizontalPositionType: TdxFloatingObjectHorizontalPositionType): string;
var
  AHPositionType: TdxWordProcessingMLValue;
begin
  if HorizontalPositionTypeAttributeTable.TryGetValue(AFloatingObjectHorizontalPositionType, AHPositionType) then
    Result := AHPositionType.OpenXmlValue
  else
    Result := 'page';
end;

procedure TdxWordProcessingMLBaseExporter.ExportLockAspectRatio(AFloatingObjectProperties: TdxFloatingObjectProperties);
begin
  if not AFloatingObjectProperties.UseLockAspectRatio then
    Exit;
  DocumentContentWriter.WriteStartElement(OPrefix, 'lock', OfficeNamespaceConst);
  try
    DocumentContentWriter.WriteAttributeString('aspectratio', ConvertBoolValueToString(AFloatingObjectProperties.LockAspectRatio));
  finally
    DocumentContentWriter.WriteEndElement;
  end;
end;

function TdxWordProcessingMLBaseExporter.ConvertBoolValueToString(AValue: Boolean): string;
begin
  if AValue then
    Result := 't'
  else
    Result := 'f';
end;

procedure TdxWordProcessingMLBaseExporter.WriteWrap(AFloatingObjectProperties: TdxFloatingObjectProperties);
var
  AWrapType, AWrapSide: TdxWordProcessingMLValue;
  ATextWrapType, ATextWrapSide: string;
begin
  ATextWrapType := '';

  if FFloatingObjectTextWrapTypeTable.TryGetValue(AFloatingObjectProperties.TextWrapType, AWrapType) then
  begin
    ATextWrapType := AWrapType.WordMLValue;
    if ATextWrapType = 'none' then
      Exit;
  end;

  DocumentContentWriter.WriteStartElement(W10MLPrefix, 'wrap', W10MLNamespace);
  try
    DocumentContentWriter.WriteAttributeString('type', ATextWrapType);
    case AFloatingObjectProperties.TextWrapType of
      TdxFloatingObjectTextWrapType.Through,
      TdxFloatingObjectTextWrapType.Tight:
        DocumentContentWriter.WriteAttributeString('anchory', 'line');
    end;
    if AFloatingObjectProperties.UseTextWrapSide then
    begin
      ATextWrapSide := '';
      if FFloatingObjectTextWrapSideTable.TryGetValue(AFloatingObjectProperties.TextWrapSide, AWrapSide) then
        ATextWrapSide := AWrapSide.WordMLValue;
      if (ATextWrapSide <> '') and (ATextWrapSide <> 'both-sides') then
        DocumentContentWriter.WriteAttributeString('side', ATextWrapSide);
    end;
  finally
    DocumentContentWriter.WriteEndElement;
  end;
end;

procedure TdxWordProcessingMLBaseExporter.WriteFloatingObjectTextBox(ATextBoxContent: TdxTextBoxFloatingObjectContent);
var
  ATextBoxProperties: TdxTextBoxProperties;
begin
  DocumentContentWriter.WriteStartElement(VMLPrefix, 'textbox', VMLNamespace);
  try
    ATextBoxProperties := ATextBoxContent.TextBoxProperties;
    if ATextBoxProperties.UseResizeShapeToFitText then
      DocumentContentWriter.WriteAttributeString('style', 'mso-fit-shape-to-text:' + ConvertBoolValueToString(ATextBoxProperties.ResizeShapeToFitText));
    if ATextBoxProperties.UseLeftMargin or ATextBoxProperties.UseRightMargin or ATextBoxProperties.UseTopMargin or ATextBoxProperties.UseBottomMargin then
      DocumentContentWriter.WriteAttributeString('inset',
        Format('%dmm,%dmm,%dmm,%dmm', [
          Round(UnitConverter.ModelUnitsToMillimetersF(ATextBoxProperties.LeftMargin)),
          Round(UnitConverter.ModelUnitsToMillimetersF(ATextBoxProperties.TopMargin)),
          Round(UnitConverter.ModelUnitsToMillimetersF(ATextBoxProperties.RightMargin)),
          Round(UnitConverter.ModelUnitsToMillimetersF(ATextBoxProperties.BottomMargin))]));
    WriteFloatingObjectTxbxContent(ATextBoxContent);
  finally
    DocumentContentWriter.WriteEndElement;
  end;
end;

procedure TdxWordProcessingMLBaseExporter.WriteFloatingObjectTxbxContent(AContent: TdxTextBoxFloatingObjectContent);
begin
  DocumentContentWriter.WriteStartElement(WordProcessingPrefix, 'txbxContent', WordProcessingNamespace);
  try
    PerformExportPieceTable(TdxPieceTable(AContent.TextBox.PieceTable), ExportPieceTable);
  finally
    DocumentContentWriter.WriteEndElement;
  end;
end;

procedure TdxWordProcessingMLBaseExporter.WriteFloatingObjectAnchorLock(ALocked: Boolean);
begin
  if ALocked then
  begin
    DocumentContentWriter.WriteStartElement(W10MLPrefix, 'anchorlock', W10MLNamespace);
    DocumentContentWriter.WriteEndElement;
  end;
end;

function TdxWordProcessingMLBaseExporter.GenerateFloatingObjectName(const AName: string; const ADefaultNamePrefix: string; AId: Integer): string;
begin
  if AName <> '' then
    Result := AName
  else
    Result := ADefaultNamePrefix + ' ' + IntToStr(AId);
end;

procedure TdxWordProcessingMLBaseExporter.WriteFloatingObjectPict(AFloatingObjectProperties: TdxFloatingObjectProperties; ATextBoxContent: TdxTextBoxFloatingObjectContent; APictureContent: TdxPictureFloatingObjectContent; AShape: TdxShape; const AName: string);
var
  AShapeTypeId: Integer;
begin
  DocumentContentWriter.WriteStartElement(WordProcessingPrefix, 'pict', WordProcessingNamespace);
  try
    AShapeTypeId := WriteFloatingObjectShapeType;
    WriteFloatingObjectShape(AFloatingObjectProperties, ATextBoxContent, APictureContent, AShape, AShapeTypeId, AName);
  finally
    DocumentContentWriter.WriteEndElement;
  end;
end;

function TdxWordProcessingMLBaseExporter.WriteFloatingObjectShapeType: Integer;
var
  AId: Integer;
begin
  DocumentContentWriter.WriteStartElement(VMLPrefix, 'shapetype', VMLNamespace);
  AId := FDrawingElementId;
  try
    WriteIntValue('id', AId);
    DocumentContentWriter.WriteAttributeString('path', 'm,l,21600r21600,l21600,xe');
    IncrementDrawingElementId;
  finally
    DocumentContentWriter.WriteEndElement;
  end;
  Result := AId;
end;

procedure TdxWordProcessingMLBaseExporter.IncrementDrawingElementId;
begin
  Inc(FDrawingElementId);
end;

procedure TdxWordProcessingMLBaseExporter.WriteFloatingObjectShape(AFloatingObjectProperties: TdxFloatingObjectProperties; ATextBoxContent: TdxTextBoxFloatingObjectContent; APictureContent: TdxPictureFloatingObjectContent; AShape: TdxShape; AShapeTypeId: Integer; const AName: string);
var
  AImagePath, AImageStyle: string;
begin
  AImagePath := '';
  if APictureContent <> nil then
    AImagePath := ExportBinData(APictureContent.Image.Image);
  DocumentContentWriter.WriteStartElement(VMLPrefix, 'shape', VMLNamespace);
  try
    AImageStyle := ExportImageStyle(AFloatingObjectProperties, ATextBoxContent, AShape);
    DocumentContentWriter.WriteAttributeString('type', '#' + IntToStr(AShapeTypeId));
    DocumentContentWriter.WriteAttributeString('id', AName);
    DocumentContentWriter.WriteAttributeString('style', AImageStyle);

    if ATextBoxContent <> nil then
      WriteFloatingObjectShapeAllColorsAndOutlineWeight(AShape);
    if AFloatingObjectProperties.UseLayoutInTableCell then
      WriteStringAttr(OPrefix, 'allowincell', OfficeNamespaceConst, GetBoolValueAsString(AFloatingObjectProperties.LayoutInTableCell));
    if AFloatingObjectProperties.UseAllowOverlap then
      WriteStringAttr(OPrefix, 'allowoverlap', OfficeNamespaceConst, GetBoolValueAsString(AFloatingObjectProperties.AllowOverlap));

    if APictureContent <> nil then
      ExportImageData(AImagePath)
    else
      if ATextBoxContent <> nil then
        WriteFloatingObjectTextBox(ATextBoxContent);

    WriteFloatingObjectAnchorLock(AFloatingObjectProperties.Locked);
    ExportLockAspectRatio(AFloatingObjectProperties);
    WriteWrap(AFloatingObjectProperties);
  finally
    DocumentContentWriter.WriteEndElement;
  end;
end;

procedure TdxWordProcessingMLBaseExporter.WriteFloatingObjectShapeAllColorsAndOutlineWeight(AShape: TdxShape);
begin
  if AShape.UseFillColor then
    DocumentContentWriter.WriteAttributeString('fillcolor', '#' + ConvertColorToString(AShape.FillColor));
  if AShape.UseOutlineColor then
    DocumentContentWriter.WriteAttributeString('strokecolor', '#' + ConvertColorToString(AShape.OutlineColor));
  if AShape.UseOutlineWidth then
    DocumentContentWriter.WriteAttributeString('strokeweight', IntToStr(Trunc(UnitConverter.ModelUnitsToPointsF(AShape.OutlineWidth))) + 'pt');
  if TdxAlphaColors.IsTransparentOrEmpty(AShape.OutlineColor) then
    DocumentContentWriter.WriteAttributeString('stroked', 'f');
end;

function TdxWordProcessingMLBaseExporter.GetBoolValueAsString(AValue: Boolean): string;
begin
  if AValue then
    Result := 't'
  else
    Result := 'f';
end;

procedure TdxWordProcessingMLBaseExporter.ExportImageData(const AImagePath: string);
begin
  DocumentContentWriter.WriteStartElement(VMLPrefix, 'imagedata', VMLNamespace);
  try
    DocumentContentWriter.WriteAttributeString('src', AImagePath);
    DocumentContentWriter.WriteAttributeString(OfficePrefix, 'title', OfficeNamespace, '');
  finally
    DocumentContentWriter.WriteEndElement;
  end;
end;

function TdxWordProcessingMLBaseExporter.ExportBinData(AImage: TdxOfficeImage): string;
var
  AImagePath: string;
  AImageStream: TStream;
  ABuffer: TArray<Byte>;
  ABytesRead: Integer;
begin
  if ExportedImageTable.TryGetValue(AImage, AImagePath) then
    Exit(AImagePath);

  AImagePath := GetImagePath(AImage);
  WriteWpStartElement('binData');
  try
    WriteWpStringAttr('name', AImagePath);
    AImageStream := GetImageBytesStream(AImage);
    try
      SetLength(ABuffer, 8192);
      while True do
      begin
        ABytesRead := AImageStream.Read(ABuffer, Length(ABuffer));
        DocumentContentWriter.WriteBase64(ABuffer, 0, ABytesRead);
        if ABytesRead < Length(ABuffer) then
          Break;
      end;
    finally
      AImageStream.Free;
    end;
  finally
    WriteWpEndElement;
  end;
  ExportedImageTable.Add(AImage, AImagePath);
  Result := AImagePath;
end;

function TdxWordProcessingMLBaseExporter.GetImagePath(AImage: TdxOfficeImage): string;
var
  AExtension: string;
begin
  AExtension := GetImageExtension(AImage);
  Result := 'wordml://' + GenerateImageId + '.' + AExtension;
end;

end.

