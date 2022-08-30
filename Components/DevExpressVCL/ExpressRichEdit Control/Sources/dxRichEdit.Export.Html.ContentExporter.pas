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

unit dxRichEdit.Export.Html.ContentExporter;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
{$IFDEF DELPHIXE2}
  System.UITypes,
{$ENDIF}
  Types, SysUtils, Classes, Graphics, Generics.Defaults, Generics.Collections, Rtti,
  dxCore, dxCoreClasses, dxCoreGraphics, dxGDIPlusAPI, dxGDIPlusClasses,
  dxXMLClasses,
  dxRichEdit.Utils.Types,
  dxGenerics,
  dxRichEdit.Utils.ChunkedStringBuilder,
  dxRichEdit.NativeApi,
  dxRichEdit.Export.Html.Types,
  dxRichEdit.DocumentModel.UnitConverter,
  dxRichEdit.Platform.Font,
  dxRichEdit.Utils.OfficeImage,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.Import.CSSParser,
  dxRichEdit.Utils.Graphics,
  dxRichEdit.Export.Html.Utils,
  dxRichEdit.Options,
  dxRichEdit.DocumentModel.IndexBasedObject,
  dxRichEdit.DocumentModel.CharacterFormatting,
  dxRichEdit.DocumentModel.ParagraphFormatting,
  dxRichEdit.DocumentModel.Bookmarks,
  dxRichEdit.DocumentModel.Numbering,
  dxRichEdit.DocumentModel.Notes,
  dxRichEdit.DocumentModel.Section,
  dxRichEdit.DocumentModel.Borders,
  dxRichEdit.DocumentModel.Boxes.Simple,
  dxRichEdit.DocumentModel.Fields,
  dxRichEdit.DocumentModel.Hyperlink,
  dxRichEdit.DocumentModel.TabFormatting,
  dxRichEdit.DocumentModel.TableFormatting,
  dxRichEdit.DocumentModel.Tables,
  dxRichEdit.DocumentModel.Exporter,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.TextRange,
  dxRichEdit.DocumentModel.InlineObjectRange,
  dxRichEdit.DocumentModel.FloatingObjectRange,
  dxRichEdit.DocumentModel.NotesRange,
  dxRichEdit.DocumentModel.FieldRange,
  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.DocumentFormats.DocumentFormatUtils,
  dxRichEdit.Export.Html.Classes;

type
  TdxHtmlExportHelper = class;
  TdxHtmlStyleHelper = class;

  TdxHtmlLiteralControl = class;
  TdxEmptyWebControl = class;

  TdxWebImageAlign = (
    NotSet,
    Left,
    Right,
    Baseline,
    Top,
    Middle,
    Bottom,
    AbsBottom,
    AbsMiddle,
    TextTop);

  { TdxHtmlContainerControl }

  TdxHtmlContainerControl = class abstract(TdxHtmlControl)
  strict private
    FLiteralControl: TdxHtmlLiteralControl;
    function GetHasChildren: Boolean;
  protected
    procedure AddAttributesToRender(AWriter: TdxHtmlTextWriter); override;
    function CreateControlCollection: TdxWebControlCollection; override;
    function GetInnerHtml: string; virtual;
    function GetInnerText: string; virtual;
    procedure Render(AWriter: TdxHtmlTextWriter); override;
    procedure RenderContents(AWriter: TdxHtmlTextWriter); virtual;
    procedure RenderEndTag(AWriter: TdxHtmlTextWriter); virtual;
    procedure SetInnerHtml(const AValue: string); virtual;
    procedure SetInnerText(const AValue: string); virtual;
  public
    destructor Destroy; override;
    property InnerHtml: string read GetInnerHtml write SetInnerHtml;
    property InnerText: string read GetInnerText write SetInnerText;
    property HasChildren: Boolean read GetHasChildren;
  end;

  { TdxHtmlAnchor }

  TdxHtmlAnchor = class(TdxHtmlContainerControl)
  strict private
    function GetHRef: string;
    function GetName: string;
    function GetTarget: string;
    function GetTitle: string;
    procedure SetHRef(const AValue: string);
    procedure SetName(const AValue: string);
    procedure SetTarget(const AValue: string);
    procedure SetTitle(const AValue: string);
  protected
    function GetCausesValidation: Boolean; virtual;
    function GetValidationGroup: string; virtual;
    procedure SetCausesValidation(const AValue: Boolean); virtual;
    procedure SetValidationGroup(const AValue: string); virtual;
  public
    constructor Create; reintroduce;

    property CausesValidation: Boolean read GetCausesValidation write SetCausesValidation;
    property HRef: string read GetHRef write SetHRef;
    property Name: string read GetName write SetName;
    property Target: string read GetTarget write SetTarget;
    property Title: string read GetTitle write SetTitle;
    property ValidationGroup: string read GetValidationGroup write SetValidationGroup;
  end;

  { TdxHtmlGenericControl }

  TdxHtmlGenericControl = class(TdxHtmlContainerControl)
  strict private
    FDataControl: TdxHtmlGenericControl;
  public
    constructor Create(AHtmlTextWriterTag: TdxHtmlTextWriterTag = TdxHtmlTextWriterTag.Span; ADataControl: TdxHtmlGenericControl = nil); reintroduce;
    destructor Destroy; override;
  end;

  { TdxNumberingListWebControl }

  TdxNumberingListWebControl = class(TdxHtmlGenericControl)
  strict private
    FAbstractListIndex: TdxAbstractNumberingListIndex;
    FCurrentLevelIndex: Integer;
  public
    constructor Create(AHtmlTextWriterTag: TdxHtmlTextWriterTag;
      AAbstractListIndex: TdxAbstractNumberingListIndex; ACurrentLevelIndex: Integer); reintroduce;

    property AbstractListIndex: TdxAbstractNumberingListIndex read FAbstractListIndex;
    property CurrentLevelIndex: Integer read FCurrentLevelIndex;
  end;

  { TdxWebImageControl }

  TdxWebImageControl = class(TdxHtmlGenericControl)
  strict private
    FUrlResolved: Boolean;
  protected
    function GetAlignValueString: string;
    function GetAlternateText: string; virtual;
    function GetDescriptionUrl: string; virtual;
    function GetGenerateEmptyAlternateText: Boolean; virtual;
    function GetImageAlign: TdxWebImageAlign; virtual;
    function GetImageUrl: string; virtual;
    procedure AddAttributesToRender(AWriter: TdxHtmlTextWriter); override;
    procedure SetAlternateText(const AValue: string); virtual;
    procedure SetDescriptionUrl(const AValue: string); virtual;
    procedure SetGenerateEmptyAlternateText(const AValue: Boolean); virtual;
    procedure SetImageAlign(const AValue: TdxWebImageAlign); virtual;
    procedure SetImageUrl(const AValue: string); virtual;
  public
    constructor Create; reintroduce;

    property AlternateText: string read GetAlternateText write SetAlternateText;
    property DescriptionUrl: string read GetDescriptionUrl write SetDescriptionUrl;
    property GenerateEmptyAlternateText: Boolean read GetGenerateEmptyAlternateText write SetGenerateEmptyAlternateText;
    property ImageAlign: TdxWebImageAlign read GetImageAlign write SetImageAlign;
    property ImageUrl: string read GetImageUrl write SetImageUrl;
    property UrlResolved: Boolean read FUrlResolved write FUrlResolved;
  end;

  { TdxHtmlLiteralControl }

  TdxHtmlLiteralControl = class(TdxWebControlBase)
  strict private
    FText: string;
  protected
    function CreateControlCollection: TdxWebControlCollection; override;
    procedure Render(AOutput: TdxHtmlTextWriter); override;
    function GetText: string; virtual;
    procedure SetText(const AValue: string); virtual;
    function IsLiteral: Boolean; override;
  public
    constructor Create(const AText: string = '');
    procedure InitRecursive(ANamingContainer: TdxWebControlBase); override;
    procedure LoadRecursive; override;
    procedure PreRenderRecursiveInternal; override;
    procedure UnloadRecursive(ADispose: Boolean); override;

    property Text: string read GetText write SetText;
  end;

  { TdxEmptyWebControl }

  TdxEmptyWebControl = class(TdxHtmlGenericControl)
  protected
    procedure RenderBeginTag(AWriter: TdxHtmlTextWriter); override;
    procedure RenderEndTag(AWriter: TdxHtmlTextWriter); override;
  end;

  { TdxStyleWebControl }

  TdxStyleWebControl = class(TdxHtmlGenericControl, IdxScriptContainer)
  strict private
    FStyles: TdxStringsDictionary;
    FTagStyles: TdxStringsDictionary;
  protected
    procedure Render(AWriter: TdxHtmlTextWriter); override;
    procedure RenderTagStyles(AWriter: TTextWriter); virtual;
    function GetClassName(const AStyle: string): string;
  public
    constructor Create;
    destructor Destroy; override;
    function IsClientScriptBlockRegistered(const AKey: string): Boolean;
    procedure RenderStyles(AWriter: TTextWriter); virtual;
    procedure RegisterClientScriptBlock(const AKey, AScript: string);
    function RegisterCssClass(const AStyle: string): string;
    procedure RegisterCommonCssStyle(const AStyle, ATagName: string);
    procedure AddStyle(const AStyle, AName: string);
    procedure AddTagStyle(const AStyle, AName: string);

    property Styles: TdxStringsDictionary read FStyles;
    property TagStyles: TdxStringsDictionary read FTagStyles;
  end;

  { TdxOfficeHtmlImageHelper }

  TdxOfficeHtmlImageHelper = class
  strict private
    FDocumentModel: TdxDocumentModel;
    FUnitConverter: TdxDocumentModelUnitConverter;
    FHorizontalResolution: Single;
    FVerticalResolution: Single;
  public
    constructor Create(const ADocumentModel: TdxDocumentModel; AUnitConverter: TdxDocumentModelUnitConverter);
    function ShouldConvertImage(AImage: TdxOfficeImage): Boolean;
    procedure ApplyImageProperties(AImageControl: TdxWebImageControl; AImage: TdxOfficeImage; const AActualSize: TSize;
      const AImageRepository: IdxOfficeImageRepository; ADisposeConvertedImagesImmediately: Boolean); overload;
    procedure ApplyImageProperties(AImageControl: TdxWebImageControl; AImage: TdxOfficeImage; const AActualSize: TSize;
      const AImageRepository: IdxOfficeImageRepository; ADisposeConvertedImagesImmediately, ACorrectWidth: Boolean); overload;
    procedure ApplyImageProperties(AImageControl: TdxWebImageControl; AImage: TdxOfficeImage; const AActualSize: TSize;
      const AImageRepository: IdxOfficeImageRepository; ADisposeConvertedImagesImmediately, ACorrectWidth, AAlwaysWriteImageSize: Boolean); overload;
    procedure ApplyImageProperties(AImageControl: TdxWebImageControl; AImage: TdxOfficeImage; const AActualSize: TSize;
      const AImageRepository: IdxOfficeImageRepository; ADisposeConvertedImagesImmediately, ACorrectWidth, AAlwaysWriteImageSize, AKeepImageSize: Boolean); overload;
    function GetHtmlImage(AImage: TdxOfficeImage; const AActualSize: TSize; ACorrectWidth: Boolean): TdxOfficeImageReference;
    function CreateBitmapFromSourceImage(AImage: TdxOfficeImage; AWidth, AHeight: Integer; ACorrectWidth: Boolean): TdxOfficeImageReference;
    procedure CalculateDefaultImageResolution;

    property HorizontalResolution: Single read FHorizontalResolution write FHorizontalResolution;
    property VerticalResolution: Single read FVerticalResolution write FVerticalResolution;
  end;

  { TdxHtmlExporterTextProperties }

  TdxHtmlExporterTextProperties = class
  strict private
    FFontCacheIndex: Integer;
    FForeColor: TdxAlphaColor;
    FBackColor: TdxAlphaColor;
    FUnderline: Boolean;
    FStrikeout: Boolean;
    FAllCaps: Boolean;
    FScript: TdxCharacterFormattingScript;
    FDoubleFontSize: Integer;
  public
    constructor Create(AFontCacheIndex: Integer; AForeColor, ABackColor: TdxAlphaColor; AUnderline, AStrikeout: Boolean;
      AScript: TdxCharacterFormattingScript; AAllCaps: Boolean; ADoubleFontSize: Integer);
    function Equals(AObj: TObject): Boolean; override;
    function GetHashCode: Integer; override;

    property FontCacheIndex: Integer read FFontCacheIndex;
    property ForeColor: TdxAlphaColor read FForeColor;
    property BackColor: TdxAlphaColor read FBackColor;
    property Underline: Boolean read FUnderline;
    property Strikeout: Boolean read FStrikeout;
    property AllCaps: Boolean read FAllCaps;
    property Script: TdxCharacterFormattingScript read FScript;
    property DoubleFontSize: Integer read FDoubleFontSize;
  end;

  { TdxVisitableDocumentIntervalBoundaryProxy }

  TdxVisitableDocumentIntervalBoundaryProxy = class(TdxVisitableDocumentIntervalBoundary)
  strict private
    FBoundary: TdxVisitableDocumentIntervalBoundary;
    FPosition: TdxDocumentLogPosition;
    FModelPosition: TdxDocumentModelPosition;
  protected
    function GetPosition: PdxDocumentModelPosition; override;
    function GetOrder: TdxBookmarkBoundaryOrder; override;
  public
    constructor Create(ABoundary: TdxVisitableDocumentIntervalBoundary);
    destructor Destroy; override;
    procedure Export(const AExporter: IdxDocumentModelExporter); override;
    procedure ChangePosition(AValue: TdxDocumentLogPosition);
    function CreateBox: TdxVisitableDocumentIntervalBox; override;

    property LogPosition: TdxDocumentLogPosition read FPosition;
  end;

  { TdxHtmlVisitableDocumentIntervalBasedObjectBoundaryIterator }

  TdxHtmlVisitableDocumentIntervalBasedObjectBoundaryIterator = class(TdxVisitableDocumentIntervalBasedObjectBoundaryIterator)
  protected
    procedure PopulateBookmarksCore(ABookmarks: TdxBookmarkList); override;
  end;

  { TdxHtmlFastCharacterMultiReplacement }

  TdxHtmlFastCharacterMultiReplacement = class(TdxFastCharacterMultiReplacement)
  public
    function CreateReplacementInfo(const AText: string; AReplaceTable: TdxCharStringDictionary; const ATabMarker: string): TdxReplacementInfo; overload;
    function PerformReplacements(const AText: string; AReplaceTable: TdxCharStringDictionary; const ATabMarker: string): string; overload;
  end;

  { TdxHtmlParagraphStyleHelper }

  TdxHtmlParagraphStyleHelper = class
  strict private const
{$REGION 'AlignHT'}
    AlignHT: array[TdxParagraphAlignment] of string = (
      'left',
      'right',
      'center',
      'justify');
{$ENDREGION}
  strict private
    FUnitConverter: TdxDocumentModelUnitConverter;
    FStyleHelper: TdxHtmlStyleHelper;
  protected
    function ShouldWriteNumberingListTextIndent(AParagraph: TdxParagraph): Boolean;
    function CalculateParagraphLeftIndent(AParagraph: TdxParagraph; AListControlNestingLevel, AParentLevelOffset: Integer): Integer;
  public
    constructor Create(AUnitConverter: TdxDocumentModelUnitConverter; AStyleHelper: TdxHtmlStyleHelper);

    function GetHtmlLineHeight(ASpacingType: TdxParagraphLineSpacing; ASpacing, AFontSizeInPoints: Single): string;
    function GetHtmlAlignment(AAlign: TdxParagraphAlignment): string; inline;
    function GetHtmlListLevelType(const AListLevel: IdxListLevel): string;
    function GetBulletType(const AFontName: string; const ADisplayFormat: string): string;
    function IsNotChangedDisplayFormat(const ADisplayFormat: string): Boolean;
    function GetHtmlParagraphStyle(AParagraph: TdxParagraph; AListControlNestingLevel, AParentLevelOffset: Integer): string; overload;
    procedure GetHtmlParagraphStyle(AParagraph: TdxParagraph; AListControlNestingLevel, AParentLevelOffset: Integer;
      AStyle: TdxCssStyleCollection); overload;
    procedure GetHtmlParagraphStyleCore(AParagraph: TdxParagraph; AListControlNestingLevel, AParentLevelOffset: Integer;
      AStyle: TdxCssStyleCollection);
    function GetHtmlParagraphInListStyle(AParagraph: TdxParagraph; const AListLevel: IdxListLevel;
      AListControlNestingLevel, AParentLevelOffset: Integer): string; overload;
    procedure GetHtmlParagraphInListStyle(AParagraph: TdxParagraph; const AListLevel: IdxListLevel;
      AListControlNestingLevel, AParentLevelOffset: Integer; AStyle: TdxCssStyleCollection; ADefaultCharacterPropertiesExportToCss: Boolean); overload;
    procedure GetHtmlParagraphInListStyle(AParagraph: TdxParagraph; const AListLevel: IdxListLevel;
      AListControlNestingLevel, AParentLevelOffset: Integer; AStyle: TdxCssStyleCollection); overload;
    function GetLineSpacingValue(ASpacingType: TdxParagraphLineSpacing; ASpacing: Single; AFontSizeInPoints: Single): string;
    function GetLeftPaddingValue(ALeftIndent: Integer; AFirstLineIndentType: TdxParagraphFirstLineIndent; AFirstLineIndent: Integer): Integer;
    function GetTextIndentValue(AFirstLineIndentType: TdxParagraphFirstLineIndent; AFirstLineIndent: Integer): string;

    property UnitConverter: TdxDocumentModelUnitConverter read FUnitConverter;
    property HtmlStyleHelper: TdxHtmlStyleHelper read FStyleHelper;
  end;

  { TdxHtmlParagraphTabsHelper }

  TdxHtmlParagraphTabsHelper = class
  strict private
    FUnitConverter: TdxDocumentModelUnitConverter;
    FStyleHelper: TdxHtmlStyleHelper;
  public
    constructor Create(AUnitConverter: TdxDocumentModelUnitConverter; AStyleHelper: TdxHtmlStyleHelper);
    function CreateTabPlaceholder(AWidth: Integer; ASpaceWidth: Integer;
      AFillCharacterWidth: Integer; AFillCharacter: Char): string;
    function CreateTabStops(AParagraph: TdxParagraph): string;
    function CreateTabStopAttribute(const ATab: TdxTabInfo): string;
    function CreateMsoTabStopAttributeValue(ATabCount: Integer; ALeader: TdxTabLeaderType): string;
    function GetHtmlTabInfoAlign(AAlign: TdxTabAlignmentType): string;
    function GetHtmlTabInfoLeader(ALeader: TdxTabLeaderType): string;
    function GetFillCharacter(ALeader: TdxTabLeaderType): Char;
    function GetFillCharacterWidth(AFontInfo: TdxFontInfo; ALeader: TdxTabLeaderType): Integer;

    property UnitConverter: TdxDocumentModelUnitConverter read FUnitConverter;
    property HtmlStyleHelper: TdxHtmlStyleHelper read FStyleHelper;
  end;

  { TdxHtmlStyleHelper }

  TdxHtmlStyleHelper = class
  strict private
    class var
      FSpecialCharactersReplaceTable: TdxCharStringDictionary;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateSpecialCharacterReplaceTable: TdxCharStringDictionary; static;
  strict private
    FCreateCssStyleStringCollectionStringBuilder: TdxChunkedStringBuilder;
    FCreateCssStyleStyleCollectionStringBuilder: TdxChunkedStringBuilder;
    FPreProcessHtmlContentTextStringBuilder: TStringBuilder;
    FReplacement: TdxHtmlFastCharacterMultiReplacement;
  public
    constructor Create;
    destructor Destroy; override;
    class function GetHtmlSizeInPoints(ASize: Single): string;
    class function GetHtmlSizeInPercents(ASize: Integer): string;
    function PreprocessHtmlContentText(const AText: string; const ATabMarker: string): string;
    function CreateCssStyle(AAttributes: TStringList; ASeparator: Char): string; overload;
    function CreateCssStyle(AAttributes: TdxCssStyleCollection; ASeparator: Char): string; overload;
  end;

  { TdxHtmlContentExporter }

  TdxHtmlContentExporter = class(TdxDocumentModelExporter)
  strict private
    FControlStack: TdxObjectStack<TdxWebControlBase>;
    FCurrentHyperlinkInfo: TdxHyperlinkInfo;
    FCurrentListLeftIndent: Integer;
    FExportedBookmarks: TDictionary<TdxBookmark, TdxHtmlGenericControl>;
    FExportHelper: TdxHtmlExportHelper;
    FIsHyperlinkContentExporting: Boolean;
    FKeepLeadingWhitespace: Boolean;
    FLastCreatedTextControl: TdxWebControlBase;
    FLastExportedHyperLinkTextProperties: TdxHtmlExporterTextProperties;
    FLastExportedTextProperties: TdxHtmlExporterTextProperties;
    FListControlNestingLevel: Integer;
    FListLeftIndents: TStack<Integer>;
    FObjectsToDelete: TdxFastObjectList;
    FOptions: TdxHtmlDocumentExporterOptions;
    FParentListLeftIndent: Integer;
    function GetCurrentParent: TdxWebControlBase;
  protected
    function GetExportHelper: TdxHtmlExportHelper; virtual;
    function CreateHtmlExportHelper(const AScriptContainer: IdxScriptContainer;
      const AImageRepository: IdxOfficeImageRepository; AOptions: TdxHtmlDocumentExporterOptions): TdxHtmlExportHelper; virtual;
    function GetAllowSkipParagraphInCell: Boolean; override;
    function CreateVisitableDocumentIntervalBoundaryIterator: TdxVisitableDocumentIntervalBasedObjectBoundaryIterator; override;
    procedure PushControl(AParent: TdxWebControlBase); virtual;
    function PopControl: TdxWebControlBase; virtual;
    procedure ExportDocument; override;
    procedure ExportFootEndNotes(ANotes: TList<TdxFootNoteExportInfo>); virtual;
    procedure AddControlToChild(AParent: TdxWebControlBase; AControl: TdxWebControlBase); virtual;
    procedure ExportCore(ATextProperties: TdxHtmlExporterTextProperties; const AText: string); virtual;
    procedure ExportFirstPageHeader(ASectionHeader: TdxSectionHeader; ALinkedToPrevious: Boolean); override;
    procedure ExportOddPageHeader(ASectionHeader: TdxSectionHeader; ALinkedToPrevious: Boolean); override;
    procedure ExportEvenPageHeader(ASectionHeader: TdxSectionHeader; ALinkedToPrevious: Boolean); override;
    procedure ExportFirstPageFooter(ASectionFooter: TdxSectionFooter; ALinkedToPrevious: Boolean); override;
    procedure ExportOddPageFooter(ASectionFooter: TdxSectionFooter; ALinkedToPrevious: Boolean); override;
    procedure ExportEvenPageFooter(ASectionFooter: TdxSectionFooter; ALinkedToPrevious: Boolean); override;
    function ExportTable(ATableInfo: TdxTableInfo): TdxParagraphIndex; override;
    procedure ExportRow(ARow: TdxTableRow; ATableInfo: TdxTableInfo); override;
    procedure ExportCell(ACell: TdxTableCell; ATableInfo: TdxTableInfo); override;
    function ExportParagraph(AParagraph: TdxParagraph): TdxParagraphIndex; override;
    procedure CloseNumberingLists;
    procedure ClosePreviousNumberingList; virtual;
    function ExportParagraphInList(AParagraph: TdxParagraph): TdxParagraphIndex; virtual;
    procedure ExportParagraphNumeration(AParagraph: TdxParagraph; const ACounters: TIntegerDynArray);
    function ExportParagraphInListCore(AParagraph: TdxParagraph; AParagraphControl: TdxWebControlBase;
      const ACounters: TIntegerDynArray): TdxParagraphIndex; virtual;
    function ExportParagraphCore(AParagraph: TdxParagraph; AParagraphControl: TdxWebControlBase): TdxParagraphIndex;
    function ExportParagraphContent(AParagraph: TdxParagraph): TdxParagraphIndex;
    function IsEmptyParagraph(AParagraph: TdxParagraph): Boolean;
    procedure ExportBookmarkStart(ABookmark: TdxBookmark); override;
    procedure ExportBookmarkEnd(ABookmark: TdxBookmark); override;
    procedure ExportFieldCodeStartRun(ARun: TdxFieldCodeStartRun); override;
    procedure ExportFieldCodeEndRun(ARun: TdxFieldCodeEndRun); override;
    function CreateHtmlExporterTextProperties(AFontCacheIndex: Integer; AForeColor, ABackColor: TdxAlphaColor;
      AUnderline, AStrikeout: Boolean; AScript: TdxCharacterFormattingScript; AAllCaps: Boolean;
      ADoubleFontSize: Integer): TdxHtmlExporterTextProperties; virtual;
    procedure StartHyperlinkExport(AHyperlinkInfo: TdxHyperlinkInfo); overload; virtual;
    procedure StartHyperlinkExport(AHyperlinkInfo: TdxHyperlinkInfo; ATextProperties: TdxHtmlExporterTextProperties); overload; virtual;
    procedure StartHyperlinkExportCore(AControl: TdxWebControlBase; AHyperlinkInfo: TdxHyperlinkInfo);
    function GetHyperlinkInfo(ARun: TdxTextRun): TdxHyperlinkInfo;
    function CreateHyperlinkControl(AHyperlinkInfo: TdxHyperlinkInfo): TdxHtmlAnchor; overload; virtual;
    function CreateHyperlinkControl(AHyperlinkInfo: TdxHyperlinkInfo; ATextProperties: TdxHtmlExporterTextProperties): TdxHtmlAnchor; overload; virtual;
    function CreateHyperlinkControlCore(AHyperlinkInfo: TdxHyperlinkInfo): TdxHtmlAnchor;
    procedure ExportFieldResultEndRun(ARun: TdxFieldResultEndRun); override;
    procedure FinishHyperlinkExport; virtual;
    function CreateParagraphControl(AParagraph: TdxParagraph): TdxWebControlBase;
    function CreateParagraphInListControl(AParagraph: TdxParagraph; const ACounters: TIntegerDynArray): TdxWebControlBase;
    procedure DecreaseListControlNestingLevel; virtual;
    procedure IncreaseListControlNestingLevel; virtual;
    function GetListLevelNestingLevel: Integer; virtual;
    procedure EnsureNumberingListControl(AParagraph: TdxParagraph; ANumberingList: TdxNumberingList; const ACounters: TIntegerDynArray); virtual;
    function IsRootFirstParagraphInListHasNonZeroLevel(AParagraph: TdxParagraph): Boolean;
    procedure CreateSeveralParentNumberingListToMakeNewNumberingListNested(AParagraph: TdxParagraph;
      ANumberingList: TdxNumberingList; const ACounters: TIntegerDynArray);
    procedure CloseListsWithSmallerLevelIndex(AListLevelIndex: Integer);
    procedure CreateNumberingListControl(AParagraph: TdxParagraph; ANumberingList: TdxNumberingList; ACounter: Integer); virtual;
    function CreateNumberingListControlCore(AParagraph: TdxParagraph; ANumberingList: TdxNumberingList; ACounter: Integer): TdxWebControlBase;
    procedure ExportTextRun(ARun: TdxTextRun); override;
    function ProcessRunText(ARun: TdxTextRun; const AText: string): string; virtual;
    procedure ExportTextRunCore(ARun: TdxTextRun; const AText: string); virtual;
    procedure ExportTextCore(AFontCacheIndex: Integer; const AText: string; AForeColor, ABackColor: TdxAlphaColor;
      AUnderlineType: TdxUnderlineType; AStrikeoutType: TdxStrikeoutType; AScript: TdxCharacterFormattingScript;
        AAllCaps: Boolean; ADoubleFontSize: Integer);
    procedure ExportInlinePictureRun(ARun: TdxInlinePictureRun); override;
    function AreCharacterPropertiesDefault(ARun: TdxInlinePictureRun): Boolean;
    function ShouldExportInlinePicture(ARun: TdxInlinePictureRun): Boolean; override;
    procedure ExportFootNoteRun(ARun: TdxFootNoteRun); override;
    procedure ExportEndNoteRun(ARun: TdxEndNoteRun); override;
    procedure ExportFootNoteRunReference(const AInfo: TdxFootNoteExportInfo;
      ARun: TdxFootNoteRunBase; const AFormat, APrefix: string);
    procedure ExportFloatingObjectAnchorRun(ARun: TdxFloatingObjectAnchorRun); override;
    procedure ExportFloatingObjectFrame(ARun: TdxFloatingObjectAnchorRun; AControl: TdxHtmlGenericControl); virtual;
    procedure ExportFloatingObjectContent(ARun: TdxFloatingObjectAnchorRun); virtual;
    procedure ExportFloatingObjectTextBoxContent(AContent: TdxTextBoxFloatingObjectContent; ARun: TdxFloatingObjectAnchorRun); virtual;
    procedure ExportFloatingObjectPictureContent(AContent: TdxPictureFloatingObjectContent; ARun: TdxFloatingObjectAnchorRun); virtual;
    function CreateImageControl(AContent: TdxPictureFloatingObjectContent; ARun: TdxFloatingObjectAnchorRun): TdxWebControlBase; virtual;
    function GetCssFloat(ARun: TdxFloatingObjectAnchorRun): TdxHtmlCssFloat;

    property ExportHelper: TdxHtmlExportHelper read GetExportHelper;
    property CurrentParent: TdxWebControlBase read GetCurrentParent;
    property ControlStack: TdxObjectStack<TdxWebControlBase> read FControlStack;
  public
    constructor Create(ADocumentModel: TdxDocumentModel;
      const AScriptContainer: IdxScriptContainer;
      const AImageRepository: IdxOfficeImageRepository;
      AOptions: TdxHtmlDocumentExporterOptions); reintroduce; virtual;
    destructor Destroy; override;
    procedure Export(AOutputStream: TStream); overload; override;
    procedure Export(AParent: TdxWebControlBase); overload;
    procedure Export; overload; override;
    function ExportSaveMemory: TdxChunkedStringBuilder; override;
    function CreateListLevelControl(AParagraph: TdxParagraph; ANumberingList: TdxNumberingList; ACounter: Integer): TdxWebControlBase;
    procedure ExportEmptyParagraph(ARun: TdxTextRunBase);
  end;

  { TdxHtmlTableStyleHelper }

  TdxHtmlTableStyleHelper = class
  strict private
    FUnitConverter: TdxDocumentModelUnitConverter;
    FStyleHelper: TdxHtmlStyleHelper;
  protected
    function GetHtmlTableCellStyle(ACell: TdxTableCell; AActualBottomBorderCell: TdxBorderBase): string; overload;
    function ConvertWidthUnitToPixels(AWidthUnit: TdxWidthUnit): string;
    function ConvertWidthUnitToPoints(AWidthUnit: TdxWidthUnit): string;
    function ConvertWidthUnitToPointsF(AWidthUnit: TdxWidthUnit): Single;
    function GetHtmlVerticalAlignment(AVAlignment: TdxVerticalAlignment): string;
    function GetHtmlTableAlignment(ATableAlignment: TdxTableRowAlignment): string; virtual;
  public
    constructor Create(AUnitConverter: TdxDocumentModelUnitConverter; AStyleHelper: TdxHtmlStyleHelper);
    function GetHtmlTableStyle(ATable: TdxTable): string; overload;
    procedure GetHtmlTableStyle(ATable: TdxTable; AStyle: TdxCssStyleCollection); overload;
    function GetHtmlBorder(ABorder: TdxBorderBase): string;
    function IsBorderNil(ABorder: TdxBorderBase): Boolean;
    function GetHtmlBorderStyle(ABorderStyle: TdxBorderLineStyle): string;
    procedure GetHtmlTableCellStyle(ACell: TdxTableCell; AStyle: TdxCssStyleCollection; AActualBottomBorderCell: TdxBorderBase); overload;

    property UnitConverter: TdxDocumentModelUnitConverter read FUnitConverter;
    property HtmlStyleHelper: TdxHtmlStyleHelper read FStyleHelper;
  end;

  { TdxHtmlExportHelper }

  TdxHtmlExportHelper = class
  strict private
    FDocumentModel: TdxDocumentModel;
    FScriptContainer: IdxScriptContainer;
    FImageRepository: IdxOfficeImageRepository;
    FHtmlStyleHelper: TdxHtmlStyleHelper;
    FParagraphStyleHelper: TdxHtmlParagraphStyleHelper;
    FTableStyleHelper: TdxHtmlTableStyleHelper;
    FParagraphTabsHelper: TdxHtmlParagraphTabsHelper;
    FImageHelper: TdxOfficeHtmlImageHelper;
    FObjectsToDelete: TdxFastObjectList;
    FOptions: TdxHtmlDocumentExporterOptions;
    FText: TStringBuilder;
    function GetUnitConverter: TdxDocumentModelUnitConverter;
    function GetIsExportInlineStyle: Boolean;
  protected
    function GetDisposeConvertedImagesImmediately: Boolean; virtual;
    function ReplaceWhiteSpaceWithNonBreakingSpace(const ARawText: string): string;
    function ProcessSpace(ACh: Char; AIsPrevWhiteSpace: Boolean): Boolean;
    function CalculateParagraphTag(AParagraph: TdxParagraph; AIgnoreOutlineLevel: Boolean): TdxHtmlTextWriterTag; virtual;

    property DisposeConvertedImagesImmediately: Boolean read GetDisposeConvertedImagesImmediately;
    property DocumentModel: TdxDocumentModel read FDocumentModel;
    property HtmlStyleHelper: TdxHtmlStyleHelper read FHtmlStyleHelper;
    property IsExportInlineStyle: Boolean read GetIsExportInlineStyle;
    property UnitConverter: TdxDocumentModelUnitConverter read GetUnitConverter;
  public
    constructor Create(ADocumentModel: TdxDocumentModel; const AScriptContainer: IdxScriptContainer;
      const AImageRepository: IdxOfficeImageRepository; AOptions: TdxHtmlDocumentExporterOptions);
    destructor Destroy; override;

    class function CreateDefaultCharacterProperties(ADocumentModel: TdxDocumentModel): TdxCharacterFormattingInfo; static;
    class procedure RemoveDefaultProperties(ADocumentModel: TdxDocumentModel; ACharacterProperties: TdxCharacterFormattingInfo;
      const AFontName: string; AFontBold, AFontItalic: Boolean; ADoubleFontSize: Integer; ABackColor, AForeColor: TdxAlphaColor;
      AStyle: TdxCssStyleCollection); overload; static;
    class procedure TryRemoveKey(ACssStyleCollection: TdxCssStyleCollection; const AKey: string); static;

    function ConvertWidthUnitToPixels(AWidthUnit: TdxWidthUnit): string;
    function CreateImageControl(AImage: TdxOfficeImage; const AActualSize: TSize; ACssFloat: TdxHtmlCssFloat): TdxWebControlBase; overload;
    function CreateImageControl(ARun: TdxInlinePictureRun): TdxWebControlBase; overload;
    function CreateImageControlInternal(AImage: TdxOfficeImage; const AActualSize: TSize; ACssFloat: TdxHtmlCssFloat): TdxWebImageControl;
    function CreateListLevelControl(AParagraph: TdxParagraph; ANumberingList: TdxNumberingList;
      AListControlNestingLevel, AParentLevelOffset, ACounter: Integer): TdxWebControlBase; virtual;
    function CreateNumberingListControl(AParagraph: TdxParagraph; ANumberingList: TdxNumberingList; ACounter: Integer): TdxWebControlBase;
    function CreateParagraphControl(AParagraph: TdxParagraph; AIgnoreOutlineLevel: Boolean;
      AListControlNestingLevel, AParentLevelOffset: Integer): TdxWebControlBase; virtual;
    function CreateTableCellControl(ACell: TdxTableCell; AMergedCellProperties: TdxVerticalMergeCellProperties): TdxWebControlBase;
    function CreateTableControl(ATable: TdxTable): TdxWebControlBase;
    function CreateTableRowControl(ARow: TdxTableRow): TdxWebControlBase;
    function CreateTextControl(const AText: string; ATextProperties: TdxHtmlExporterTextProperties): TdxWebControlBase;
    function GetMostNestedCounter(const ACounters: TIntegerDynArray): Integer;
    function ShouldAddNumberingListStartAttribute(AParagraph: TdxParagraph; ANumberingList: TdxNumberingList): Boolean;
    procedure AppendText(AParent: TdxWebControlBase; const AText: string);
    procedure RemoveDefaultProperties(ATextProperties: TdxHtmlExporterTextProperties; AFontInfo: TdxFontInfo; ACssStyleCollection: TdxCssStyleCollection); overload;
    procedure SetTextProperties(AControl: TdxHtmlControl; ATextProperties: TdxHtmlExporterTextProperties; AForceExportCharacterProperties: Boolean);
  end;

implementation

uses
  Contnrs, Math, RTLConsts, StrUtils,
  dxTypeHelpers, dxHash, dxCultureInfo,

  dxRichEdit.DocumentModel.Intervals,
  dxRichEdit.DocumentModel.NumberingFormatting,
  dxRichEdit.DocumentModel.FloatingObjectFormatting,
  dxRichEdit.DocumentModel.ShapeFormatting,
  dxRichEdit.DocumentModel.Fields.Core,
  dxRichEdit.Import.Html.HTMLParser,
  dxRichEdit.Import.Html,
  dxCharacters,
  dxStringHelper;

function FormatFloatInvariantCulture(Value: Single): string;
begin
  Result := FormatFloat('0.##', Value, TdxCultureInfo.InvariantCulture.FormatSettings);
end;

{ TdxHtmlContainerControl }

destructor TdxHtmlContainerControl.Destroy;
begin
  FLiteralControl.Free;
  inherited Destroy;
end;

function TdxHtmlContainerControl.GetInnerHtml: string;
begin
  if IsLiteralContent then
    Exit((TdxHtmlLiteralControl(Controls[0])).Text);
  if Controls.Count <> 0 then
    raise EInvalidOpException.Create('Inner_Content_not_literal');
  Result := '';
end;

procedure TdxHtmlContainerControl.SetInnerHtml(const AValue: string);
begin
  Controls.Clear;
  FLiteralControl.Free;
  FLiteralControl := TdxHtmlLiteralControl.Create(AValue);
  Controls.Add(FLiteralControl);
  ViewState['innerhtml'] := AValue;
end;

function TdxHtmlContainerControl.GetInnerText: string;
begin
  Result := TdxHttpUtility.HtmlDecode(InnerHtml);
end;

procedure TdxHtmlContainerControl.SetInnerText(const AValue: string);
begin
  InnerHtml := TdxHttpUtility.HtmlEncode(AValue);
end;

function TdxHtmlContainerControl.GetHasChildren: Boolean;
begin
  Result := Controls.Count = 0;
end;

function TdxHtmlContainerControl.CreateControlCollection: TdxWebControlCollection;
begin
  Result := TdxWebControlCollection.Create(Self);
end;

procedure TdxHtmlContainerControl.Render(AWriter: TdxHtmlTextWriter);
begin
  RenderBeginTag(AWriter);
  RenderChildren(AWriter);
  RenderEndTag(TdxHtmlTextWriter(AWriter));
end;

procedure TdxHtmlContainerControl.RenderContents(AWriter: TdxHtmlTextWriter);
begin
  RenderChildren(AWriter);
end;

procedure TdxHtmlContainerControl.AddAttributesToRender(AWriter: TdxHtmlTextWriter);
begin
  ViewState.Remove('innerhtml');
  inherited AddAttributesToRender(AWriter);
end;

procedure TdxHtmlContainerControl.RenderEndTag(AWriter: TdxHtmlTextWriter);
begin
  AWriter.RenderEndTag;
end;

{ TdxHtmlAnchor }

constructor TdxHtmlAnchor.Create;
begin
  inherited Create(TdxHtmlTextWriterTag.A);
end;

function TdxHtmlAnchor.GetCausesValidation: Boolean;
var
  AValue: TValue;
begin
  AValue := ViewState['CausesValidation'];
  if not AValue.IsEmpty then
    Result := AValue.AsBoolean
  else
    Result := True;
end;

procedure TdxHtmlAnchor.SetCausesValidation(const AValue: Boolean);
begin
  ViewState['CausesValidation'] := AValue;
end;

function TdxHtmlAnchor.GetHRef: string;
begin
  Result := Attributes['href'];
end;

procedure TdxHtmlAnchor.SetHRef(const AValue: string);
begin
  Attributes['href'] := TdxHtmlControl.MapStringAttributeToString(AValue);
end;

function TdxHtmlAnchor.GetName: string;
begin
  Result := Attributes['name'];
end;

procedure TdxHtmlAnchor.SetName(const AValue: string);
begin
  Attributes['name'] := TdxHtmlControl.MapStringAttributeToString(AValue);
end;

function TdxHtmlAnchor.GetTarget: string;
begin
  Result := Attributes['target'];
end;

procedure TdxHtmlAnchor.SetTarget(const AValue: string);
begin
  Attributes['target'] := TdxHtmlControl.MapStringAttributeToString(AValue);
end;

function TdxHtmlAnchor.GetTitle: string;
begin
  Result := Attributes['title'];
end;

procedure TdxHtmlAnchor.SetTitle(const AValue: string);
begin
  Attributes['title'] := TdxHtmlControl.MapStringAttributeToString(AValue);
end;

function TdxHtmlAnchor.GetValidationGroup: string;
var
  AItem: TValue;
begin
  AItem := ViewState['ValidationGroup'];
  if not AItem.IsEmpty then
    Result := AItem.AsString
  else
    Result := '';
end;

procedure TdxHtmlAnchor.SetValidationGroup(const AValue: string);
begin
  ViewState['ValidationGroup'] := AValue;
end;

{ TdxHtmlGenericControl }

constructor TdxHtmlGenericControl.Create(AHtmlTextWriterTag: TdxHtmlTextWriterTag = TdxHtmlTextWriterTag.Span;
  ADataControl: TdxHtmlGenericControl = nil);
begin
  inherited Create(AHtmlTextWriterTag);
  FDataControl := ADataControl;
end;

destructor TdxHtmlGenericControl.Destroy;
begin
  FDataControl.Free;
  inherited Destroy;
end;

{ TdxNumberingListWebControl }

constructor TdxNumberingListWebControl.Create(AHtmlTextWriterTag: TdxHtmlTextWriterTag;
  AAbstractListIndex: TdxAbstractNumberingListIndex; ACurrentLevelIndex: Integer);
begin
  inherited Create(AHtmlTextWriterTag);
  FAbstractListIndex := AAbstractListIndex;
  FCurrentLevelIndex := ACurrentLevelIndex;
end;

{ TdxWebImageControl }

constructor TdxWebImageControl.Create;
begin
  inherited Create(TdxHtmlTextWriterTag.Img);
end;

function TdxWebImageControl.GetAlternateText: string;
begin
  Result := ViewState['alt'].AsString;
end;

procedure TdxWebImageControl.SetAlternateText(const AValue: string);
begin
  ViewState['alt'] := AValue;
end;

function TdxWebImageControl.GetDescriptionUrl: string;
begin
  Result := ViewState['longdesc'].AsString;
end;

procedure TdxWebImageControl.SetDescriptionUrl(const AValue: string);
begin
  ViewState['longdesc'] := AValue;
end;

function TdxWebImageControl.GetGenerateEmptyAlternateText: Boolean;
var
  AItem: TValue;
begin
  AItem := ViewState['alt'];
  Result := not AItem.IsEmpty and AItem.IsString;
end;

procedure TdxWebImageControl.SetGenerateEmptyAlternateText(const AValue: Boolean);
begin
  if AValue then
  begin
    if ViewState['alt'].IsEmpty then
      ViewState['alt'] := '';
  end
  else
    ViewState.Remove('alt');
end;

function TdxWebImageControl.GetImageAlign: TdxWebImageAlign;
var
  AItem: TValue;
begin
  AItem := ViewState['align'];
  if AItem.IsType<TdxWebImageAlign> then
    Result := AItem.AsType<TdxWebImageAlign>
  else
    Result := TdxWebImageAlign.NotSet;
end;

procedure TdxWebImageControl.SetImageAlign(const AValue: TdxWebImageAlign);
begin
  if (AValue < TdxWebImageAlign.NotSet) or (AValue > TdxWebImageAlign.TextTop) then
    raise EArgumentOutOfRangeException.Create('AValue');
  ViewState['align'] := TValue.From(AValue);
end;

function TdxWebImageControl.GetImageUrl: string;
var
  AItem: TValue;
begin
  AItem := ViewState['src'];
  Result := AItem.AsString;
end;

procedure TdxWebImageControl.SetImageUrl(const AValue: string);
begin
  ViewState['src'] := AValue;
end;

procedure TdxWebImageControl.AddAttributesToRender(AWriter: TdxHtmlTextWriter);
var
  AImageUrl, AAlignString: string;
begin
  inherited AddAttributesToRender(AWriter);

  AImageUrl := ImageUrl;
  if not UrlResolved then
    AImageUrl := inherited ResolveClientUrl(AImageUrl);

  if AImageUrl <> '' then
  begin
    if TdxStringHelper.StartsWith(AImageUrl, 'file://') then
      AWriter.AddAttribute('src', TdxNullableString.Create(True, AImageUrl), False)
    else
      AWriter.AddAttribute(TdxHtmlTextWriterAttribute.Src, AImageUrl);
  end;

  AImageUrl := DescriptionUrl;
  if Length(AImageUrl) > 0 then
    AWriter.AddAttribute(TdxHtmlTextWriterAttribute.Longdesc, inherited ResolveClientUrl(AImageUrl));

  AImageUrl := AlternateText;
  if (Length(AImageUrl) > 0) or GenerateEmptyAlternateText then
    AWriter.AddAttribute(TdxHtmlTextWriterAttribute.Alt, AImageUrl);

  AAlignString := GetAlignValueString;
  if AAlignString <> '' then
    AWriter.AddAttribute(TdxHtmlTextWriterAttribute.Align, AAlignString);

  if BorderWidth.IsEmpty then
    AWriter.AddStyleAttribute(TdxHtmlTextWriterStyle.BorderWidth, '0px');
end;

function TdxWebImageControl.GetAlignValueString: string;
begin
  case ImageAlign of
    TdxWebImageAlign.Left:
      Result := 'left';
    TdxWebImageAlign.Right:
      Result := 'right';
    TdxWebImageAlign.Baseline:
      Result := 'baseline';
    TdxWebImageAlign.Top:
      Result := 'top';
    TdxWebImageAlign.Middle:
      Result := 'middle';
    TdxWebImageAlign.Bottom:
      Result := 'bottom';
    TdxWebImageAlign.AbsBottom:
      Result := 'absbottom';
    TdxWebImageAlign.AbsMiddle:
      Result := 'absmiddle';
    TdxWebImageAlign.NotSet:
      Result := '';
    else
      Result := 'texttop';
  end;
end;

{ TdxHtmlLiteralControl }

constructor TdxHtmlLiteralControl.Create(const AText: string = '');
begin
  inherited Create;
  PreventAutoID;
  FText := AText;
end;

function TdxHtmlLiteralControl.CreateControlCollection: TdxWebControlCollection;
begin
  Result := TdxWebEmptyControlCollection.Create(Self);
end;

procedure TdxHtmlLiteralControl.InitRecursive(ANamingContainer: TdxWebControlBase);
begin
  OnInit(TdxEventArgs.Empty);
end;

procedure TdxHtmlLiteralControl.LoadRecursive;
begin
  OnLoad(TdxEventArgs.Empty);
end;

procedure TdxHtmlLiteralControl.PreRenderRecursiveInternal;
begin
  OnPreRender(TdxEventArgs.Empty);
end;

procedure TdxHtmlLiteralControl.Render(AOutput: TdxHtmlTextWriter);
begin
  AOutput.Write(FText);
end;

procedure TdxHtmlLiteralControl.UnloadRecursive(ADispose: Boolean);
begin
  OnUnload(TdxEventArgs.Empty);
  if ADispose then
    Free;
end;

function TdxHtmlLiteralControl.GetText: string;
begin
  Result := FText;
end;

procedure TdxHtmlLiteralControl.SetText(const AValue: string);
begin
  FText := AValue;
end;

function TdxHtmlLiteralControl.IsLiteral: Boolean;
begin
  Result := True;
end;

{ TdxEmptyWebControl }

procedure TdxEmptyWebControl.RenderBeginTag(AWriter: TdxHtmlTextWriter);
begin
end;

procedure TdxEmptyWebControl.RenderEndTag(AWriter: TdxHtmlTextWriter);
begin
end;

{ TdxStyleWebControl }

constructor TdxStyleWebControl.Create;
begin
  inherited Create(TdxHtmlTextWriterTag.Style);
  FStyles := TdxStringsDictionary.Create(TdxIStringComparer.Ordinal);
  FTagStyles := TdxStringsDictionary.Create(TdxIStringComparer.Ordinal);
  Attributes.Add('type', 'text/css');
end;

destructor TdxStyleWebControl.Destroy;
begin
  FStyles.Free;
  FTagStyles.Free;
  inherited Destroy;
end;

procedure TdxStyleWebControl.Render(AWriter: TdxHtmlTextWriter);
begin
  RenderBeginTag(AWriter);
  RenderTagStyles(AWriter);
  RenderStyles(AWriter);
  RenderEndTag(AWriter);
end;

procedure TdxStyleWebControl.RenderStyles(AWriter: TTextWriter);
var
  AStyle, AName: string;
begin
  for AStyle in FStyles.Keys do
  begin
    AName := FStyles[AStyle];
    AWriter.WriteLine(Format('.%s{%s}', [AName, AStyle]));
  end;
end;

procedure TdxStyleWebControl.RenderTagStyles(AWriter: TTextWriter);
var
  AStyle, AName: string;
begin
  for AStyle in FTagStyles.Keys do
  begin
    AName := FTagStyles[AStyle];
    AWriter.WriteLine(Format('%s {%s}', [AName, AStyle]));
  end;
end;

function TdxStyleWebControl.IsClientScriptBlockRegistered(const AKey: string): Boolean;
begin
  Result := True;
end;

procedure TdxStyleWebControl.RegisterClientScriptBlock(const AKey, AScript: string);
begin
  raise ENotSupportedException.Create('RegisterClientScriptBlock');
end;

function TdxStyleWebControl.RegisterCssClass(const AStyle: string): string;
begin
  AddStyle(AStyle, GetClassName(AStyle));
  Result := FStyles[AStyle];
end;

procedure TdxStyleWebControl.RegisterCommonCssStyle(const AStyle, ATagName: string);
begin
  AddTagStyle(AStyle, ATagName);
end;

procedure TdxStyleWebControl.AddStyle(const AStyle, AName: string);
begin
  if not FStyles.ContainsKey(AStyle) then
    FStyles.Add(AStyle, AName);
end;

procedure TdxStyleWebControl.AddTagStyle(const AStyle, AName: string);
begin
  if not FTagStyles.ContainsKey(AStyle) then
    FTagStyles.Add(AStyle, AName);
end;

function TdxStyleWebControl.GetClassName(const AStyle: string): string;
begin
  Result := 'cs' + TdxStringHelper.TrimStart(IntToHex(dxElfHash(AStyle), 8), ['0']);
end;

{ TdxOfficeHtmlImageHelper }

constructor TdxOfficeHtmlImageHelper.Create(const ADocumentModel: TdxDocumentModel; AUnitConverter: TdxDocumentModelUnitConverter);
begin
  inherited Create;
  FDocumentModel := ADocumentModel;
  FUnitConverter := AUnitConverter;
  CalculateDefaultImageResolution;
end;

function TdxOfficeHtmlImageHelper.ShouldConvertImage(AImage: TdxOfficeImage): Boolean;
begin
  case AImage.RawFormat of
    TdxOfficeImageFormat.Wmf,
    TdxOfficeImageFormat.Emf,
    TdxOfficeImageFormat.Exif,
    TdxOfficeImageFormat.Icon,
    TdxOfficeImageFormat.MemoryBmp,
    TdxOfficeImageFormat.None:
      Result := True;
    else
      Result := False;
  end;
end;

procedure TdxOfficeHtmlImageHelper.ApplyImageProperties(AImageControl: TdxWebImageControl; AImage: TdxOfficeImage;
  const AActualSize: TSize; const AImageRepository: IdxOfficeImageRepository; ADisposeConvertedImagesImmediately: Boolean);
begin
  ApplyImageProperties(AImageControl, AImage, AActualSize, AImageRepository, ADisposeConvertedImagesImmediately, True);
end;

procedure TdxOfficeHtmlImageHelper.ApplyImageProperties(AImageControl: TdxWebImageControl; AImage: TdxOfficeImage;
  const AActualSize: TSize; const AImageRepository: IdxOfficeImageRepository; ADisposeConvertedImagesImmediately, ACorrectWidth: Boolean);
begin
  ApplyImageProperties(AImageControl, AImage, AActualSize, AImageRepository, ADisposeConvertedImagesImmediately, ACorrectWidth, False);
end;

procedure TdxOfficeHtmlImageHelper.ApplyImageProperties(AImageControl: TdxWebImageControl; AImage: TdxOfficeImage;
  const AActualSize: TSize; const AImageRepository: IdxOfficeImageRepository;
  ADisposeConvertedImagesImmediately, ACorrectWidth, AAlwaysWriteImageSize: Boolean);
begin
  ApplyImageProperties(AImageControl, AImage, AActualSize, AImageRepository, ADisposeConvertedImagesImmediately,
    ACorrectWidth, AAlwaysWriteImageSize, False);
end;

procedure TdxOfficeHtmlImageHelper.ApplyImageProperties(AImageControl: TdxWebImageControl;
  AImage: TdxOfficeImage; const AActualSize: TSize; const AImageRepository: IdxOfficeImageRepository;
  ADisposeConvertedImagesImmediately, ACorrectWidth, AAlwaysWriteImageSize, AKeepImageSize: Boolean);
var
  AForceWriteImageSize, AUriImageWithoutSize: Boolean;
  AReference: TdxOfficeImageReference;
  AUriOfficeImage: TdxUriOfficeImage;
begin
  AForceWriteImageSize := False;
  AUriOfficeImage := Safe<TdxUriOfficeImage>.Cast(AImage);
  if ShouldConvertImage(AImage) then
  begin
    if AKeepImageSize then
    begin
      AReference := FDocumentModel.CreateImage(AImage);
      AForceWriteImageSize := True;
    end
    else
      AReference := GetHtmlImage(AImage, AActualSize, ACorrectWidth);
    try
      AImageControl.ImageUrl := AImageRepository.GetImageSource(AReference);
    finally
      AReference.Free;
    end;
  end
  else
  begin
    AReference := FDocumentModel.CreateImage(AImage);
    try
      AImageControl.ImageUrl := AImageRepository.GetImageSource(AReference);
    finally
      AReference.Free;
    end;
    AUriImageWithoutSize := (AUriOfficeImage <> nil) and ((AUriOfficeImage.PixelTargetWidth = 0) and (AUriOfficeImage.PixelTargetHeight = 0));
    if not AUriImageWithoutSize then
      AForceWriteImageSize := True;
  end;
  if AAlwaysWriteImageSize or AForceWriteImageSize then
  begin
    AImageControl.Attributes.Add('width', IntToStr(FUnitConverter.ModelUnitsToPixels(AActualSize.Width, HorizontalResolution)));
    AImageControl.Attributes.Add('height', IntToStr(FUnitConverter.ModelUnitsToPixels(AActualSize.Height, VerticalResolution)));
  end
  else
  begin
    if AUriOfficeImage <> nil then
    begin
      if AUriOfficeImage.PixelTargetWidth > 0 then
        AImageControl.Attributes.Add('width', IntToStr(FUnitConverter.ModelUnitsToPixels(AActualSize.Width, HorizontalResolution)));
      if AUriOfficeImage.PixelTargetHeight > 0 then
        AImageControl.Attributes.Add('height', IntToStr(FUnitConverter.ModelUnitsToPixels(AActualSize.Height, VerticalResolution)));
    end;
  end;
end;

function TdxOfficeHtmlImageHelper.GetHtmlImage(AImage: TdxOfficeImage; const AActualSize: TSize; ACorrectWidth: Boolean): TdxOfficeImageReference;
var
  AFinalWidth, AFinalHeight: Integer;
begin
  AFinalWidth := FUnitConverter.ModelUnitsToPixels(AActualSize.cx, HorizontalResolution);
  AFinalHeight := FUnitConverter.ModelUnitsToPixels(AActualSize.cy, VerticalResolution);
  Result := CreateBitmapFromSourceImage(AImage, AFinalWidth, AFinalHeight, ACorrectWidth);
  Result.Uri := AImage.Uri;
end;

function TdxOfficeHtmlImageHelper.CreateBitmapFromSourceImage(AImage: TdxOfficeImage;
  AWidth, AHeight: Integer; ACorrectWidth: Boolean): TdxOfficeImageReference;
var
  AGdiPlusIssueWidthCorrectionValue: Integer;
  ABitmap: TdxOfficeImage;
  ACanvas: TdxGPCanvas;
begin
  Result := nil;
  AGdiPlusIssueWidthCorrectionValue := 1;
  ABitmap := TdxOfficeImage.CreateSize(Max(1, IfThen(ACorrectWidth, AWidth - AGdiPlusIssueWidthCorrectionValue, AWidth)), Max(1, AHeight));
  try
    ACanvas := ABitmap.CreateCanvas;
    try
      ACanvas.Clear(TdxAlphaColors.Transparent);
      ACanvas.Draw(AImage, TRect.Create(0, 0, AWidth, AHeight), AImage.ClientRect);
    finally
      ACanvas.Free;
    end;
    Result := FDocumentModel.CreateImage(ABitmap);
  finally
    if Result.Image <> ABitmap then
      ABitmap.Free;
  end;
end;

procedure TdxOfficeHtmlImageHelper.CalculateDefaultImageResolution;
var
  AImage: TdxOfficeImage;
begin
  AImage := TdxOfficeImage.CreateSize(1, 1);
  try
    HorizontalResolution := AImage.HorizontalResolution;
    VerticalResolution := AImage.VerticalResolution;
  finally
    AImage.Free;
  end;
end;

{ TdxHtmlExporterTextProperties }

constructor TdxHtmlExporterTextProperties.Create(AFontCacheIndex: Integer; AForeColor, ABackColor: TdxAlphaColor;
  AUnderline, AStrikeout: Boolean; AScript: TdxCharacterFormattingScript; AAllCaps: Boolean; ADoubleFontSize: Integer);
begin
  inherited Create;
  FFontCacheIndex := AFontCacheIndex;
  FForeColor := AForeColor;
  FBackColor := ABackColor;
  FUnderline := AUnderline;
  FStrikeout := AStrikeout;
  FAllCaps := AAllCaps;
  FScript := AScript;
  FDoubleFontSize := ADoubleFontSize;
end;

function TdxHtmlExporterTextProperties.Equals(AObj: TObject): Boolean;
var
  AOther: TdxHtmlExporterTextProperties;
begin
  AOther := Safe<TdxHtmlExporterTextProperties>.Cast(AObj);
  if AOther = nil then
    Exit(False);
  Result :=
    (FontCacheIndex = AOther.FontCacheIndex) and
    (ForeColor = AOther.ForeColor) and
    (BackColor = AOther.BackColor) and
    (Underline = AOther.Underline) and
    (Strikeout = AOther.Strikeout) and
    (AllCaps = AOther.AllCaps) and
    (DoubleFontSize = AOther.DoubleFontSize);
end;

function TdxHtmlExporterTextProperties.GetHashCode: Integer;
begin
  Result :=
    FontCacheIndex xor
    Integer(ForeColor) xor
    Integer(BackColor) xor
    Ord(Script) xor
    DoubleFontSize;
  if AllCaps then
    Result := Result xor Integer($80000000);
  if Underline then
    Result := Result xor Integer($40000000);
  if Strikeout then
    Result := Result xor Integer($20000000);
end;

{ TdxVisitableDocumentIntervalBoundaryProxy }

constructor TdxVisitableDocumentIntervalBoundaryProxy.Create(ABoundary: TdxVisitableDocumentIntervalBoundary);
begin
  inherited Create(ABoundary.VisitableInterval);
  FBoundary := ABoundary;
  FPosition := ABoundary.Position.LogPosition;
  FModelPosition := TdxPositionConverter.ToDocumentModelPosition(FBoundary.VisitableInterval.PieceTable, FPosition);
end;

destructor TdxVisitableDocumentIntervalBoundaryProxy.Destroy;
begin
  FBoundary.Free;
  inherited Destroy;
end;

function TdxVisitableDocumentIntervalBoundaryProxy.GetPosition: PdxDocumentModelPosition;
begin
  FModelPosition := TdxPositionConverter.ToDocumentModelPosition(FBoundary.VisitableInterval.PieceTable, FPosition);
  Result := @FModelPosition;
end;

function TdxVisitableDocumentIntervalBoundaryProxy.GetOrder: TdxBookmarkBoundaryOrder;
begin
  Result := FBoundary.Order;
end;

procedure TdxVisitableDocumentIntervalBoundaryProxy.Export(const AExporter: IdxDocumentModelExporter);
begin
  FBoundary.Export(AExporter);
end;

function TdxVisitableDocumentIntervalBoundaryProxy.CreateBox: TdxVisitableDocumentIntervalBox;
begin
  Result := FBoundary.CreateBox;
end;

procedure TdxVisitableDocumentIntervalBoundaryProxy.ChangePosition(AValue: TdxDocumentLogPosition);
begin
  FPosition := AValue;
end;

{ TdxHtmlVisitableDocumentIntervalBasedObjectBoundaryIterator }

procedure TdxHtmlVisitableDocumentIntervalBasedObjectBoundaryIterator.PopulateBookmarksCore(ABookmarks: TdxBookmarkList);
var
  AStartBoundaryFactory: TdxVisitableDocumentIntervalStartBoundaryFactory;
  AEndBoundaryFactory: TdxVisitableDocumentIntervalEndBoundaryFactory;
  ACount, I: Integer;
  APrevEndBoundary, AStartBoundary, AEndBoundary: TdxVisitableDocumentIntervalBoundaryProxy;
  AInterval: TdxVisitableDocumentInterval;
  AHyperlink: TdxField;
begin
  AStartBoundaryFactory := CreateVisitableDocumentIntervalStartBoundaryFactory;
  try
    AEndBoundaryFactory := CreateVisitableDocumentIntervalEndBoundaryFactory;
    try
      ACount := ABookmarks.Count;
      APrevEndBoundary := nil;
      for I := 0 to ACount - 1 do
      begin
        AInterval := ABookmarks[I];
        if not IsVisibleInterval(AInterval) then
          Continue;

        AInterval.Visit(AStartBoundaryFactory);
        AStartBoundary := TdxVisitableDocumentIntervalBoundaryProxy.Create(AStartBoundaryFactory.Boundary);
        AStartBoundary.IntervalIndex := I;
        if (APrevEndBoundary <> nil) and (AStartBoundary.Position^ < APrevEndBoundary.Position^) then
          APrevEndBoundary.ChangePosition(AStartBoundary.Position.LogPosition);
        AHyperlink := PieceTable.GetHyperlinkField(AStartBoundary.Position.RunIndex);
        if AHyperlink <> nil then
          AStartBoundary.ChangePosition(TdxDocumentModelPosition.FromRunEnd(PieceTable, AHyperlink.LastRunIndex).LogPosition);
        Boundaries.Add(AStartBoundary);

        AInterval.Visit(AEndBoundaryFactory);
        AEndBoundary := TdxVisitableDocumentIntervalBoundaryProxy.Create(AEndBoundaryFactory.Boundary);
        if AEndBoundary.LogPosition > PieceTable.DocumentEndLogPosition then
          AEndBoundary.ChangePosition(PieceTable.DocumentEndLogPosition);
        AEndBoundary.IntervalIndex := I;
        if AEndBoundary.Position^ < AStartBoundary.Position^ then
          AEndBoundary.ChangePosition(AStartBoundary.Position.LogPosition)
        else
          if AEndBoundary.Position^ > AStartBoundary.Position^ then
          begin
            AHyperlink := PieceTable.GetHyperlinkField(AEndBoundary.Position.RunIndex);
            if AHyperlink <> nil then
              AEndBoundary.ChangePosition(TdxDocumentModelPosition.FromRunStart(PieceTable, AHyperlink.FirstRunIndex).LogPosition);
          end;
        Boundaries.Add(AEndBoundary);
        APrevEndBoundary := AEndBoundary;
      end;
    finally
      AEndBoundaryFactory.Free;
    end;
  finally
    AStartBoundaryFactory.Free;
  end;
end;

{ TdxHtmlFastCharacterMultiReplacement }

function TdxHtmlFastCharacterMultiReplacement.CreateReplacementInfo(const AText: string;
  AReplaceTable: TdxCharStringDictionary; const ATabMarker: string): TdxReplacementInfo;
var
  I: Integer;
  AReplaceWith: string;
begin
  if ATabMarker = #9 then
    Exit(CreateReplacementInfo(AText, AReplaceTable));

  Result := nil;
  for I := Length(AText) downto 1 do
  begin
    if AReplaceTable.TryGetValue(AText[I], AReplaceWith) then
    begin
      if Result = nil then
        Result := TdxReplacementInfo.Create;
      Result.Add(I - 1, AReplaceWith);
    end
    else
      if AText[I] = #9 then
      begin
        if Result = nil then
          Result := TdxReplacementInfo.Create;
        Result.Add(I - 1, ATabMarker);
      end;
  end;
end;

function TdxHtmlFastCharacterMultiReplacement.PerformReplacements(const AText: string; AReplaceTable: TdxCharStringDictionary; const ATabMarker: string): string;
var
  AReplacementInfo: TdxReplacementInfo;
begin
  AReplacementInfo := CreateReplacementInfo(AText, AReplaceTable, ATabMarker);
  try
    Result := PerformReplacements(AText, AReplacementInfo);
  finally
    AReplacementInfo.Free;
  end;
end;

{ TdxHtmlParagraphStyleHelper }

constructor TdxHtmlParagraphStyleHelper.Create(AUnitConverter: TdxDocumentModelUnitConverter; AStyleHelper: TdxHtmlStyleHelper);
begin
  inherited Create;
  FUnitConverter := AUnitConverter;
  FStyleHelper := AStyleHelper;
end;

function TdxHtmlParagraphStyleHelper.GetHtmlLineHeight(ASpacingType: TdxParagraphLineSpacing; ASpacing, AFontSizeInPoints: Single): string;
var
  AValue: Single;
begin
  case ASpacingType of
    TdxParagraphLineSpacing.Sesquialteral:
      Result := FormatFloatInvariantCulture(1.5);
    TdxParagraphLineSpacing.Double:
      Result := FormatFloatInvariantCulture(2.0);
    TdxParagraphLineSpacing.Multiple:
      Result := FormatFloatInvariantCulture(ASpacing);
    TdxParagraphLineSpacing.Exactly:
      Result := HtmlStyleHelper.GetHtmlSizeInPoints(UnitConverter.ModelUnitsToPointsF(ASpacing));
    TdxParagraphLineSpacing.AtLeast:
      begin
        AValue := UnitConverter.ModelUnitsToPointsF(ASpacing);
        if AValue > AFontSizeInPoints then
          Result := HtmlStyleHelper.GetHtmlSizeInPoints(AValue)
        else
          Result := FormatFloatInvariantCulture(1.0);
      end;
    else
      Result := FormatFloatInvariantCulture(1.0);
  end;
end;

function TdxHtmlParagraphStyleHelper.GetHtmlAlignment(AAlign: TdxParagraphAlignment): string;
begin
  Result := AlignHT[AAlign];
end;

function TdxHtmlParagraphStyleHelper.GetHtmlListLevelType(const AListLevel: IdxListLevel): string;
var
  AFormat: TdxNumberingFormat;
  ADisplayFormat: string;
begin
  AFormat := AListLevel.ListLevelProperties.Format;
  ADisplayFormat := AListLevel.ListLevelProperties.DisplayFormatString;
  if IsNotChangedDisplayFormat(ADisplayFormat) and (AFormat <> TdxNumberingFormat.Bullet) then
    Exit('disc');
  Result := '';
  case AFormat of
    TdxNumberingFormat.Bullet:
      Result := GetBulletType(AListLevel.CharacterProperties.FontName, ADisplayFormat);
    TdxNumberingFormat.Decimal:
      Result := 'decimal';
    TdxNumberingFormat.DecimalZero:
      Result := 'decimal-leading-zero';
    TdxNumberingFormat.UpperLetter:
      Result := 'upper-latin';
    TdxNumberingFormat.LowerLetter:
      Result := 'lower-latin';
    TdxNumberingFormat.UpperRoman:
      Result := 'upper-roman';
    TdxNumberingFormat.LowerRoman:
      Result := 'lower-roman';
    TdxNumberingFormat.Hebrew1:
      Result := 'hebrew';
    TdxNumberingFormat.Hebrew2:
      Result := 'hebrew';
    TdxNumberingFormat.AIUEOHiragana:
      Result := 'hiragana';
    TdxNumberingFormat.AIUEOFullWidthHiragana:
      Result := 'katakana';
    TdxNumberingFormat.Iroha:
      Result := 'hiragana-iroha';
    TdxNumberingFormat.IrohaFullWidth:
      Result := 'katakana-iroha';
  end;
end;

function TdxHtmlParagraphStyleHelper.GetBulletType(const AFontName: string;
  const ADisplayFormat: string): string;
var
  AFirstChar: Char;
begin
  if AFontName = 'Courier New' then
    Exit('circle')
  else
    if AFontName = 'Wingdings' then
      Exit('square')
    else
    begin
      if ADisplayFormat = '' then
        Exit('disc');
      AFirstChar := ADisplayFormat[1];
      if AFirstChar = 'o' then
        Result := 'circle'
      else
        Result := 'disc';
    end;
end;

function TdxHtmlParagraphStyleHelper.IsNotChangedDisplayFormat(const ADisplayFormat: string): Boolean;
begin
  Result := Format(ADisplayFormat, ['1', '1', '1', '1', '1', '1', '1', '1', '1']) = ADisplayFormat;
end;

function TdxHtmlParagraphStyleHelper.GetHtmlParagraphStyle(AParagraph: TdxParagraph;
  AListControlNestingLevel, AParentLevelOffset: Integer): string;
var
  AStyle: TdxCssStyleCollection;
begin
  AStyle := TdxCssStyleCollection.Create;
  try
    GetHtmlParagraphStyle(AParagraph, AListControlNestingLevel, AParentLevelOffset, AStyle);
    Result := HtmlStyleHelper.CreateCssStyle(AStyle, ';');
  finally
    AStyle.Free;
  end;
end;

procedure TdxHtmlParagraphStyleHelper.GetHtmlParagraphStyle(AParagraph: TdxParagraph;
  AListControlNestingLevel, AParentLevelOffset: Integer; AStyle: TdxCssStyleCollection);
begin
  GetHtmlParagraphStyleCore(AParagraph, AListControlNestingLevel, AParentLevelOffset, AStyle);
end;

procedure TdxHtmlParagraphStyleHelper.GetHtmlParagraphStyleCore(AParagraph: TdxParagraph;
  AListControlNestingLevel, AParentLevelOffset: Integer; AStyle: TdxCssStyleCollection);
var
  ALeftIndent: Integer;
  ASpacingBefore, ASpacingAfter: Single;
  AProperties: TdxMergedCharacterProperties;
begin
  AStyle.Add('text-align', GetHtmlAlignment(AParagraph.Alignment));
  if not AParagraph.IsInList or ShouldWriteNumberingListTextIndent(AParagraph) then
    AStyle.Add('text-indent', GetTextIndentValue(AParagraph.FirstLineIndentType, AParagraph.FirstLineIndent));

  ALeftIndent := CalculateParagraphLeftIndent(AParagraph, AListControlNestingLevel, AParentLevelOffset);
  if AParagraph.ContextualSpacing then
    ASpacingBefore := 0
  else
    ASpacingBefore := UnitConverter.ModelUnitsToPointsFRound(AParagraph.SpacingBefore);
  if AParagraph.ContextualSpacing then
    ASpacingAfter := 0
  else
    ASpacingAfter := UnitConverter.ModelUnitsToPointsFRound(AParagraph.SpacingAfter);
  AStyle.Add('margin', HtmlStyleHelper.GetHtmlSizeInPoints(ASpacingBefore) + ' ' +
    HtmlStyleHelper.GetHtmlSizeInPoints(UnitConverter.ModelUnitsToPointsFRound(AParagraph.RightIndent)) + ' ' +
    HtmlStyleHelper.GetHtmlSizeInPoints(ASpacingAfter) + ' ' +
    HtmlStyleHelper.GetHtmlSizeInPoints(
      UnitConverter.ModelUnitsToPointsFRound(GetLeftPaddingValue(ALeftIndent, AParagraph.FirstLineIndentType, AParagraph.FirstLineIndent))));

  if AParagraph.LineSpacingType <> TdxParagraphLineSpacing.Single then
  begin
    AProperties := AParagraph.GetMergedCharacterProperties;
    try
      AStyle.Add('line-height', GetLineSpacingValue(AParagraph.LineSpacingType, AParagraph.LineSpacing, AProperties.Info.DoubleFontSize / 2));
    finally
      AProperties.Free;
    end;
  end;

  if AParagraph.PageBreakBefore then
    AStyle.Add('page-break-before', 'always');
  if AParagraph.BeforeAutoSpacing then
    AStyle.Add('mso-margin-top-alt', 'auto');
  if AParagraph.AfterAutoSpacing then
    AStyle.Add('mso-margin-bottom-alt', 'auto');
  if AParagraph.KeepWithNext then
    AStyle.Add('page-break-after', 'avoid');
  if AParagraph.KeepLinesTogether then
    AStyle.Add('page-break-inside', 'avoid');

  if not TdxAlphaColors.IsTransparentOrEmpty(AParagraph.BackColor) then
    AStyle.Add('background', TdxHtmlConvert.ToHtml(AParagraph.BackColor));
end;

function TdxHtmlParagraphStyleHelper.ShouldWriteNumberingListTextIndent(AParagraph: TdxParagraph): Boolean;
var
  AFirstLineIndentType: TdxParagraphFirstLineIndent;
begin
  AFirstLineIndentType := AParagraph.FirstLineIndentType;
  if (AFirstLineIndentType = TdxParagraphFirstLineIndent.Hanging) or (AFirstLineIndentType = TdxParagraphFirstLineIndent.None) then
    Result := False
  else
    Result := AParagraph.FirstLineIndent > 0;
end;

function TdxHtmlParagraphStyleHelper.CalculateParagraphLeftIndent(AParagraph: TdxParagraph;
  AListControlNestingLevel, AParentLevelOffset: Integer): Integer;
begin
  if AParagraph.IsInList then
  begin
    if AListControlNestingLevel = 1 then
      Result := Max(0, AParagraph.LeftIndent - UnitConverter.DocumentsToModelUnits(150))
    else
      Result := AParagraph.LeftIndent - AParentLevelOffset;
  end
  else
    Result := AParagraph.LeftIndent;
end;

function TdxHtmlParagraphStyleHelper.GetHtmlParagraphInListStyle(AParagraph: TdxParagraph; const AListLevel: IdxListLevel;
  AListControlNestingLevel: Integer; AParentLevelOffset: Integer): string;
var
  AStyle: TdxCssStyleCollection;
begin
  AStyle := TdxCssStyleCollection.Create;
  try
    GetHtmlParagraphInListStyle(AParagraph, AListLevel, AListControlNestingLevel, AParentLevelOffset, AStyle);
    Result := HtmlStyleHelper.CreateCssStyle(AStyle, ';');
  finally
    AStyle.Free;
  end;
end;

procedure TdxHtmlParagraphStyleHelper.GetHtmlParagraphInListStyle(AParagraph: TdxParagraph; const AListLevel: IdxListLevel;
  AListControlNestingLevel: Integer; AParentLevelOffset: Integer; AStyle: TdxCssStyleCollection;
  ADefaultCharacterPropertiesExportToCss: Boolean);
var
  ACharacterProperties: TdxMergedCharacterProperties;
  AInfo: TdxCharacterFormattingInfo;
  AForeColor, ABackColor: TdxAlphaColor;
  AFontName: string;
begin
  GetHtmlParagraphStyleCore(AParagraph, AListControlNestingLevel, AParentLevelOffset, AStyle);
  AStyle.Add('list-style-type', GetHtmlListLevelType(AListLevel));

  ACharacterProperties := AParagraph.GetNumerationCharacterProperties;
  try
    AInfo := ACharacterProperties.Info;

    AForeColor := AInfo.ForeColor;
    ABackColor := AInfo.BackColor;

    if AForeColor = TdxAlphaColors.Empty then
      AForeColor := TdxAlphaColors.Black;

    if ABackColor = TdxAlphaColors.Empty then
      ABackColor := TdxAlphaColors.Transparent;

    AFontName := AInfo.FontName;
    if AFontName = 'Symbol' then
      AFontName := 'Arial';
    TdxHtmlStyleRender.GetHtmlStyle(AFontName, AInfo.DoubleFontSize / 2, TdxGraphicUnit.guPoint,
      AInfo.FontBold, AInfo.FontItalic, False, False, AForeColor, ABackColor, AStyle);
    if not ADefaultCharacterPropertiesExportToCss then
      TdxHtmlExportHelper.RemoveDefaultProperties(AParagraph.DocumentModel, AInfo,
        AInfo.FontName, AInfo.FontBold, AInfo.FontItalic,
        AInfo.DoubleFontSize, ABackColor, AForeColor, AStyle);
  finally
    ACharacterProperties.Free;
  end;
end;

procedure TdxHtmlParagraphStyleHelper.GetHtmlParagraphInListStyle(AParagraph: TdxParagraph; const AListLevel: IdxListLevel;
  AListControlNestingLevel: Integer; AParentLevelOffset: Integer; AStyle: TdxCssStyleCollection);
begin
  GetHtmlParagraphInListStyle(AParagraph, AListLevel, AListControlNestingLevel, AParentLevelOffset, AStyle, True);
end;

function TdxHtmlParagraphStyleHelper.GetLineSpacingValue(ASpacingType: TdxParagraphLineSpacing; ASpacing: Single;
  AFontSizeInPoints: Single): string;
begin
  Result := GetHtmlLineHeight(ASpacingType, ASpacing, AFontSizeInPoints);
  if ASpacingType = TdxParagraphLineSpacing.Exactly then
    Result := Result + ';mso-line-height-rule:exactly';
end;

function TdxHtmlParagraphStyleHelper.GetLeftPaddingValue(ALeftIndent: Integer;
  AFirstLineIndentType: TdxParagraphFirstLineIndent; AFirstLineIndent: Integer): Integer;
begin
  Result := ALeftIndent;
end;

function TdxHtmlParagraphStyleHelper.GetTextIndentValue(AFirstLineIndentType: TdxParagraphFirstLineIndent;
  AFirstLineIndent: Integer): string;
begin
  case AFirstLineIndentType of
    TdxParagraphFirstLineIndent.Hanging:
      Result := HtmlStyleHelper.GetHtmlSizeInPoints(UnitConverter.ModelUnitsToPointsFRound(-AFirstLineIndent));
    TdxParagraphFirstLineIndent.Indented:
      Result := HtmlStyleHelper.GetHtmlSizeInPoints(UnitConverter.ModelUnitsToPointsFRound(AFirstLineIndent));
    else
      Result := '0pt';
  end;
end;

{ TdxHtmlParagraphTabsHelper }

constructor TdxHtmlParagraphTabsHelper.Create(AUnitConverter: TdxDocumentModelUnitConverter; AStyleHelper: TdxHtmlStyleHelper);
begin
  inherited Create;
  FUnitConverter := AUnitConverter;
  FStyleHelper := AStyleHelper;
end;

function TdxHtmlParagraphTabsHelper.CreateTabPlaceholder(AWidth: Integer; ASpaceWidth: Integer; AFillCharacterWidth: Integer; AFillCharacter: Char): string;
var
  AFillCharacterCount: Integer;
begin
  AFillCharacterCount := 0;
  Dec(AWidth, ASpaceWidth);
  if AWidth > 0 then
    AFillCharacterCount := AWidth div AFillCharacterWidth;
  Result := StringOfChar(AFillCharacter, AFillCharacterCount) + ' ';
end;

function TdxHtmlParagraphTabsHelper.CreateTabStops(AParagraph: TdxParagraph): string;
var
  ATabs: TdxTabFormattingInfo;
  ATabAttributes: TStringList;
  ACount, I: Integer;
begin
  ATabs := AParagraph.GetTabs;
  try
    ATabAttributes := TStringList.Create;
    try
      ACount := ATabs.Count;
      for I := 0 to ACount - 1 do
        ATabAttributes.Add(CreateTabStopAttribute(ATabs[I]));
      Result := HtmlStyleHelper.CreateCssStyle(ATabAttributes, ' ');
    finally
      ATabAttributes.Free;
    end;
  finally
    ATabs.Free;
  end;
end;

function TdxHtmlParagraphTabsHelper.CreateTabStopAttribute(const ATab: TdxTabInfo): string;
var
  AAttributes: TStringList;
begin
  if ATab.Deleted then
    Exit('');

  AAttributes := TStringList.Create;
  try
    AAttributes.Add(GetHtmlTabInfoAlign(ATab.Alignment));
    AAttributes.Add(GetHtmlTabInfoLeader(ATab.Leader));
    AAttributes.Add(HtmlStyleHelper.GetHtmlSizeInPoints(UnitConverter.ModelUnitsToPointsF(ATab.Position)));
    Result := HtmlStyleHelper.CreateCssStyle(AAttributes, ' ');
  finally
    AAttributes.Free;
  end;
end;

function TdxHtmlParagraphTabsHelper.CreateMsoTabStopAttributeValue(ATabCount: Integer; ALeader: TdxTabLeaderType): string;
var
  AAttributes: TStringList;
begin
  AAttributes := TStringList.Create;
  try
    AAttributes.Add(IntToStr(ATabCount));
    AAttributes.Add(GetHtmlTabInfoLeader(ALeader));
    Result := HtmlStyleHelper.CreateCssStyle(AAttributes, ' ');
  finally
    AAttributes.Free;
  end;
end;

function TdxHtmlParagraphTabsHelper.GetHtmlTabInfoAlign(AAlign: TdxTabAlignmentType): string;
begin
  case AAlign of
    TdxTabAlignmentType.Right:
      Result := 'right';
    TdxTabAlignmentType.Center:
      Result := 'center';
    TdxTabAlignmentType.Decimal:
      Result := 'decimal';
    else
      Result := 'left';
  end;
end;

function TdxHtmlParagraphTabsHelper.GetHtmlTabInfoLeader(ALeader: TdxTabLeaderType): string;
begin
  case ALeader of
    TdxTabLeaderType.Dots:
      Result := 'dotted';
    TdxTabLeaderType.Underline:
      Result := 'lined';
    TdxTabLeaderType.Hyphens:
      Result := 'dashed';
    TdxTabLeaderType.MiddleDots:
      Result := 'middot';
    TdxTabLeaderType.ThickLine:
      Result := 'heavy';
    else
      Result := '';
  end;
end;

function TdxHtmlParagraphTabsHelper.GetFillCharacter(ALeader: TdxTabLeaderType): Char;
begin
  case ALeader of
    TdxTabLeaderType.Dots:
      Result := TdxCharacters.Dot;
    TdxTabLeaderType.Underline:
      Result := TdxCharacters.Underscore;
    TdxTabLeaderType.Hyphens:
      Result := TdxCharacters.Dash;
    TdxTabLeaderType.MiddleDots:
      Result := TdxCharacters.MiddleDot;
    TdxTabLeaderType.ThickLine:
      Result := TdxCharacters.Underscore;
    else
      Result := TdxCharacters.NonBreakingSpace;
  end;
end;

function TdxHtmlParagraphTabsHelper.GetFillCharacterWidth(AFontInfo: TdxFontInfo; ALeader: TdxTabLeaderType): Integer;
begin
  case ALeader of
    TdxTabLeaderType.Dots:
      Result := AFontInfo.DotWidth;
    TdxTabLeaderType.Underline:
      Result := AFontInfo.UnderscoreWidth;
    TdxTabLeaderType.Hyphens:
      Result := AFontInfo.DashWidth;
    TdxTabLeaderType.MiddleDots:
      Result := AFontInfo.MiddleDotWidth;
    TdxTabLeaderType.ThickLine:
      Result := AFontInfo.UnderscoreWidth;
    else
      Result := AFontInfo.NonBreakingSpaceWidth;
  end;
end;

{ TdxHtmlStyleHelper }

class constructor TdxHtmlStyleHelper.Initialize;
begin
  FSpecialCharactersReplaceTable := CreateSpecialCharacterReplaceTable;
end;

class destructor TdxHtmlStyleHelper.Finalize;
begin
  FreeAndNil(FSpecialCharactersReplaceTable);
end;

constructor TdxHtmlStyleHelper.Create;
begin
  inherited Create;
  FPreProcessHtmlContentTextStringBuilder := TStringBuilder.Create;
  FReplacement := TdxHtmlFastCharacterMultiReplacement.Create(FPreProcessHtmlContentTextStringBuilder);
  FCreateCssStyleStringCollectionStringBuilder := TdxChunkedStringBuilder.Create;
  FCreateCssStyleStyleCollectionStringBuilder := TdxChunkedStringBuilder.Create;
end;

destructor TdxHtmlStyleHelper.Destroy;
begin
  FreeAndNil(FPreProcessHtmlContentTextStringBuilder);
  FreeAndNil(FReplacement);
  FreeAndNil(FCreateCssStyleStringCollectionStringBuilder);
  FreeAndNil(FCreateCssStyleStyleCollectionStringBuilder);
  inherited Destroy;
end;

class function TdxHtmlStyleHelper.CreateSpecialCharacterReplaceTable: TdxCharStringDictionary;
var
  ATable: TdxHtmlSpecialSymbolTable;
  AKey: string;
begin
  Result := TdxCharStringDictionary.Create;
  Result.Add('&', '&amp;');
  Result.Add(TdxCharacters.Hyphen, '&shy;');

  ATable := TdxHtmlImporter.CreateHtmlSpecialSymbolTable;
  try
    for AKey in ATable.Keys do
    begin
      if AKey = 'amp' then
        Continue;

      if AKey = 'apos' then
        Result.Add(ATable[AKey], '&#39;')
      else
        if AKey = 'thetasy' then
          Result.Add(ATable[AKey], '&#977;')
        else
          Result.Add(ATable[AKey], '&' + AKey + ';');
    end;
  finally
    ATable.Free;
  end;

  Result.Add(TdxCharacters.LineBreak, '<br/>');
  Result.Add(TdxCharacters.PageBreak, ' ');
  Result.Add(TdxCharacters.ColumnBreak, ' ');
end;

class function TdxHtmlStyleHelper.GetHtmlSizeInPoints(ASize: Single): string;
begin
  Result := FormatFloatInvariantCulture(ASize) + 'pt';
end;

class function TdxHtmlStyleHelper.GetHtmlSizeInPercents(ASize: Integer): string;
begin
  Result := Format('%d%%', [ASize]);
end;

function TdxHtmlStyleHelper.PreprocessHtmlContentText(const AText: string; const ATabMarker: string): string;
begin
  Result := FReplacement.PerformReplacements(AText, FSpecialCharactersReplaceTable, ATabMarker);
end;

function TdxHtmlStyleHelper.CreateCssStyle(AAttributes: TStringList; ASeparator: Char): string;
var
  ACount, AAttributesWritten, I: Integer;
  AAttribute: string;
begin
  ACount := AAttributes.Count;
  AAttributesWritten := 0;
  for I := 0 to ACount - 1 do
  begin
    AAttribute := Trim(AAttributes[I]);
    if AAttribute <> '' then
    begin
      if AAttributesWritten > 0 then
        FCreateCssStyleStringCollectionStringBuilder.Append(ASeparator);
      FCreateCssStyleStringCollectionStringBuilder.Append(AAttribute);
      Inc(AAttributesWritten);
    end;
  end;
  Result := FCreateCssStyleStringCollectionStringBuilder.ToString;
  FCreateCssStyleStringCollectionStringBuilder.Clear;
end;

function TdxHtmlStyleHelper.CreateCssStyle(AAttributes: TdxCssStyleCollection; ASeparator: Char): string;
var
  AAttributesWritten: Integer;
  AKey, AAttribute: string;
begin
  AAttributesWritten := 0;
  for AKey in AAttributes.Keys do
  begin
    AAttribute := Trim(AAttributes[AKey]);
    if AAttribute <> '' then
    begin
      if AAttributesWritten > 0 then
        FCreateCssStyleStyleCollectionStringBuilder.Append(ASeparator);
      FCreateCssStyleStyleCollectionStringBuilder.Append(AKey);
      FCreateCssStyleStyleCollectionStringBuilder.Append(':');
      FCreateCssStyleStyleCollectionStringBuilder.Append(AAttribute);
      Inc(AAttributesWritten);
    end;
  end;
  Result := FCreateCssStyleStyleCollectionStringBuilder.ToString;
  FCreateCssStyleStyleCollectionStringBuilder.Clear;
end;

{ TdxHtmlContentExporter }

constructor TdxHtmlContentExporter.Create(
  ADocumentModel: TdxDocumentModel;
  const AScriptContainer: IdxScriptContainer;
  const AImageRepository: IdxOfficeImageRepository;
  AOptions: TdxHtmlDocumentExporterOptions);
begin
  inherited Create(ADocumentModel, AOptions);
  FExportedBookmarks := TDictionary<TdxBookmark, TdxHtmlGenericControl>.Create;
  FOptions := AOptions;
  FObjectsToDelete := TdxFastObjectList.Create;
  FControlStack := TdxObjectStack<TdxWebControlBase>.Create(False);
  FListLeftIndents := TStack<Integer>.Create;
  FExportHelper := CreateHtmlExportHelper(AScriptContainer, AImageRepository, AOptions);
end;

destructor TdxHtmlContentExporter.Destroy;
begin
  FreeAndNil(FControlStack);
  FreeAndNil(FListLeftIndents);
  FreeAndNil(FExportHelper);
  FreeAndNil(FExportedBookmarks);
  FreeAndNil(FObjectsToDelete);
  inherited Destroy;
end;

function TdxHtmlContentExporter.GetExportHelper: TdxHtmlExportHelper;
begin
  Result := FExportHelper;
end;

function TdxHtmlContentExporter.CreateHtmlExportHelper(const AScriptContainer: IdxScriptContainer;
  const AImageRepository: IdxOfficeImageRepository; AOptions: TdxHtmlDocumentExporterOptions): TdxHtmlExportHelper;
begin
  Result := TdxHtmlExportHelper.Create(DocumentModel, AScriptContainer, AImageRepository, AOptions);
end;

function TdxHtmlContentExporter.GetCurrentParent: TdxWebControlBase;
begin
  Result := FControlStack.Peek;
end;

function TdxHtmlContentExporter.GetAllowSkipParagraphInCell: Boolean;
begin
  Result := True;
end;

function TdxHtmlContentExporter.CreateVisitableDocumentIntervalBoundaryIterator: TdxVisitableDocumentIntervalBasedObjectBoundaryIterator;
begin
  Result := TdxHtmlVisitableDocumentIntervalBasedObjectBoundaryIterator.Create(PieceTable);
end;

procedure TdxHtmlContentExporter.PushControl(AParent: TdxWebControlBase);
begin
  FControlStack.Push(AParent);
end;

function TdxHtmlContentExporter.PopControl: TdxWebControlBase;
begin
  Result := FControlStack.Extract;
end;

procedure TdxHtmlContentExporter.Export(AOutputStream: TStream);
begin
end;

procedure TdxHtmlContentExporter.Export(AParent: TdxWebControlBase);
begin
  PushControl(AParent);
  try
    Export;
  finally
    PopControl;
  end;
end;

procedure TdxHtmlContentExporter.Export;
begin
  inherited Export;
end;

procedure TdxHtmlContentExporter.ExportDocument;
begin
  inherited ExportDocument;
  FKeepLeadingWhitespace := True;
  ExportFootEndNotes(FootNoteExportInfos);
  ExportFootEndNotes(EndNoteExportInfos);
end;

procedure TdxHtmlContentExporter.ExportFootEndNotes(ANotes: TList<TdxFootNoteExportInfo>);
var
  ACount, I: Integer;
begin
  ACount := ANotes.Count;
  if ACount <= 0 then
    Exit;

  for I := 0 to ACount - 1 do
    PerformExportPieceTable(ANotes[I].Note, ExportPieceTable);
end;

procedure TdxHtmlContentExporter.AddControlToChild(AParent: TdxWebControlBase; AControl: TdxWebControlBase);
begin
  AParent.Controls.Add(AControl);
end;

procedure TdxHtmlContentExporter.ExportCore(ATextProperties: TdxHtmlExporterTextProperties; const AText: string);
var
  AScript: TdxCharacterFormattingScript;
  AParentControls: TdxWebControlCollection;
  APeek, AControl, AStyle: TdxHtmlGenericControl;
  AForceExportCharacterProperties: Boolean;
  AParentControlsCount: Integer;
  APrevControl: TdxWebControlBase;
  ATextControl: TdxHtmlControl;
  ATag: TdxHtmlTextWriterTag;
begin
  AScript := ATextProperties.Script;
  AParentControls := CurrentParent.Controls;
  APeek := Safe<TdxHtmlGenericControl>.Cast(CurrentParent);
  AForceExportCharacterProperties := (APeek <> nil) and (APeek.TagKey = TdxHtmlTextWriterTag.Li);
  if AScript = TdxCharacterFormattingScript.Normal then
  begin
    AParentControlsCount := AParentControls.Count;
    if AParentControlsCount > 0 then
      APrevControl := AParentControls[AParentControlsCount - 1]
    else
      APrevControl := nil;
    if (APrevControl <> nil) and (FLastCreatedTextControl = APrevControl) and ATextProperties.Equals(FLastExportedTextProperties) then
      FExportHelper.AppendText(APrevControl, AText)
    else
    begin
      ATextControl := TdxHtmlControl(FExportHelper.CreateTextControl(AText, ATextProperties));
      AddControlToChild(CurrentParent, ATextControl);
      if not (CurrentParent is TdxHtmlAnchor) or not ATextProperties.Equals(FLastExportedHyperLinkTextProperties) then
        FExportHelper.SetTextProperties(ATextControl, ATextProperties, AForceExportCharacterProperties);
      FLastCreatedTextControl := ATextControl;
      FLastExportedTextProperties := ATextProperties;
    end;
  end
  else
  begin
    ATag := TdxHtmlTextWriterTag.Unknown;
    if AScript = TdxCharacterFormattingScript.Subscript then
      ATag := TdxHtmlTextWriterTag.Sub;
    if AScript = TdxCharacterFormattingScript.Superscript then
      ATag := TdxHtmlTextWriterTag.Sup;
    AControl := TdxHtmlGenericControl.Create(ATag);
    try
      FExportHelper.AppendText(AControl, AText);

      AStyle := TdxHtmlGenericControl.Create(TdxHtmlTextWriterTag.Span);
      FObjectsToDelete.Add(AStyle);

      FExportHelper.SetTextProperties(AStyle, ATextProperties, AForceExportCharacterProperties);

      AddControlToChild(AStyle, AControl);
      AddControlToChild(CurrentParent, AStyle);

      FLastCreatedTextControl := nil;
      FLastExportedTextProperties := nil;
    finally
      FObjectsToDelete.Add(AControl);
    end;
  end;
end;

procedure TdxHtmlContentExporter.ExportFirstPageHeader(ASectionHeader: TdxSectionHeader; ALinkedToPrevious: Boolean);
begin
end;

procedure TdxHtmlContentExporter.ExportOddPageHeader(ASectionHeader: TdxSectionHeader; ALinkedToPrevious: Boolean);
begin
end;

procedure TdxHtmlContentExporter.ExportEvenPageHeader(ASectionHeader: TdxSectionHeader; ALinkedToPrevious: Boolean);
begin
end;

procedure TdxHtmlContentExporter.ExportFirstPageFooter(ASectionFooter: TdxSectionFooter; ALinkedToPrevious: Boolean);
begin
end;

procedure TdxHtmlContentExporter.ExportOddPageFooter(ASectionFooter: TdxSectionFooter; ALinkedToPrevious: Boolean);
begin
end;

procedure TdxHtmlContentExporter.ExportEvenPageFooter(ASectionFooter: TdxSectionFooter; ALinkedToPrevious: Boolean);
begin
end;

function TdxHtmlContentExporter.ExportTable(ATableInfo: TdxTableInfo): TdxParagraphIndex;
var
  AFirstParagraph: TdxParagraph;
  ATable: TdxWebControlBase;
begin
  AFirstParagraph := TdxParagraph(ATableInfo.Table.PieceTable.Paragraphs[ATableInfo.Table.Rows[0].Cells[0].StartParagraphIndex]);
  if (not AFirstParagraph.IsInList) or (not AFirstParagraph.ShouldExportNumbering) then
    if CurrentParent is TdxNumberingListWebControl then
      CloseListsWithSmallerLevelIndex(AFirstParagraph.GetListLevelIndex);

  ATable := FExportHelper.CreateTableControl(ATableInfo.Table);
  try
    AddControlToChild(CurrentParent, ATable);
    PushControl(ATable);
    Result := inherited ExportTable(ATableInfo);
  finally
    PopControl;
  end;
end;

procedure TdxHtmlContentExporter.ExportRow(ARow: TdxTableRow; ATableInfo: TdxTableInfo);
var
  ATableRow: TdxWebControlBase;
begin
  ATableRow := FExportHelper.CreateTableRowControl(ARow);
  try
    AddControlToChild(CurrentParent, ATableRow);
    PushControl(ATableRow);
    inherited ExportRow(ARow, ATableInfo);
  finally
    PopControl;
  end;
end;

function TdxHtmlContentExporter.ExportSaveMemory: TdxChunkedStringBuilder;
begin
  Result := nil;
end;

procedure TdxHtmlContentExporter.ExportCell(ACell: TdxTableCell; ATableInfo: TdxTableInfo);
var
  AMergedCellProperties: TdxVerticalMergeCellProperties;
  ARowSpan: Integer;
  ATableCell: TdxWebControlBase;
begin
  AMergedCellProperties := ATableInfo.GetMergedCellProperties(ACell);
  ARowSpan := AMergedCellProperties.RowSpan;
  if ARowSpan = 0 then
    Exit;
  ATableCell := FExportHelper.CreateTableCellControl(ACell, AMergedCellProperties);
  try
    AddControlToChild(CurrentParent, ATableCell);
    PushControl(ATableCell);
    inherited ExportCell(ACell, ATableInfo);
  finally
    while PopControl <> ATableCell do ;
  end;
end;

function TdxHtmlContentExporter.ExportParagraph(AParagraph: TdxParagraph): TdxParagraphIndex;
var
  AParagraphIndex: TdxParagraphIndex;
  AParagraphControl: TdxWebControlBase;
begin
  FKeepLeadingWhitespace := True;
  if AParagraph.IsInList and AParagraph.ShouldExportNumbering then
  begin
    AParagraphIndex := ExportParagraphInList(AParagraph);
    if AParagraph.IsLast then
      CloseNumberingLists;

    Result := AParagraphIndex;
  end
  else
  begin
    if CurrentParent is TdxNumberingListWebControl then
      CloseNumberingLists;

    AParagraphControl := CreateParagraphControl(AParagraph);
    FCurrentListLeftIndent := AParagraph.LeftIndent;
    try
      Result := ExportParagraphCore(AParagraph, AParagraphControl);
    finally
      FCurrentListLeftIndent := 0;
    end;
  end;
end;

procedure TdxHtmlContentExporter.CloseNumberingLists;
begin
  if CurrentParent is TdxNumberingListWebControl then
    CloseListsWithSmallerLevelIndex(0);
  if CurrentParent is TdxNumberingListWebControl then
    PopControl;
end;

procedure TdxHtmlContentExporter.ClosePreviousNumberingList;
var
  APeek: TdxHtmlGenericControl;
begin
  PopControl;
  DecreaseListControlNestingLevel;
  APeek := Safe<TdxHtmlGenericControl>.Cast(CurrentParent);
  if (APeek <> nil) and (APeek.TagKey = TdxHtmlTextWriterTag.Li) then
    PopControl;
end;

function TdxHtmlContentExporter.ExportParagraphInList(AParagraph: TdxParagraph): TdxParagraphIndex;
var
  ACounters: TIntegerDynArray;
  AParagraphControl, AParagraphInListControl: TdxWebControlBase;
begin
  ACounters := PieceTableNumberingListCounters.CalculateNextCounters(AParagraph);
  if FOptions.HtmlNumberingListExportFormat = TdxHtmlNumberingListExportFormat.PlainTextFormat then
  begin
    AParagraphControl := CreateParagraphControl(AParagraph);
    FCurrentListLeftIndent := AParagraph.LeftIndent;
    try
      Exit(ExportParagraphInListCore(AParagraph, AParagraphControl, ACounters));
    finally
      FCurrentListLeftIndent := 0;
    end;
  end;
  AParagraphInListControl := CreateParagraphInListControl(AParagraph, ACounters);
  Result := ExportParagraphCore(AParagraph, AParagraphInListControl);
end;

procedure TdxHtmlContentExporter.ExportParagraphNumeration(AParagraph: TdxParagraph;
  const ACounters: TIntegerDynArray);
var
  ALevel: IdxListLevel;
  AFontCacheIndex: Integer;
  AText: string;
  AProperties: TdxMergedCharacterProperties;
  AInfo: TdxCharacterFormattingInfo;
begin
  ALevel := DocumentModel.NumberingLists[AParagraph.GetNumberingListIndex].Levels[AParagraph.GetListLevelIndex];
  AFontCacheIndex := AParagraph.GetNumerationFontCacheIndex;
  AText := AParagraph.GetNumberingListText(ACounters) + ALevel.ListLevelProperties.Separator;
  AProperties := AParagraph.GetNumerationCharacterProperties;
  try
    AInfo := AProperties.Info;

    ExportTextCore(AFontCacheIndex, AText, AInfo.ForeColor, AInfo.BackColor, AInfo.FontUnderlineType, AInfo.FontStrikeoutType,
      AInfo.Script, AInfo.AllCaps, AInfo.DoubleFontSize);
  finally
    AProperties.Free;
  end;
end;

function TdxHtmlContentExporter.ExportParagraphInListCore(AParagraph: TdxParagraph; AParagraphControl: TdxWebControlBase;
  const ACounters: TIntegerDynArray): TdxParagraphIndex;
begin
  AddControlToChild(CurrentParent, AParagraphControl);
  PushControl(AParagraphControl);
  ExportParagraphNumeration(AParagraph, ACounters);
  if FIsHyperlinkContentExporting then
    StartHyperlinkExport(FCurrentHyperlinkInfo);
  Result := ExportParagraphContent(AParagraph);
end;

function TdxHtmlContentExporter.ExportParagraphCore(AParagraph: TdxParagraph; AParagraphControl: TdxWebControlBase): TdxParagraphIndex;
begin
  AddControlToChild(CurrentParent, AParagraphControl);
  PushControl(AParagraphControl);
  if FIsHyperlinkContentExporting then
    StartHyperlinkExport(FCurrentHyperlinkInfo);
  Result := ExportParagraphContent(AParagraph);
end;

function TdxHtmlContentExporter.ExportParagraphContent(AParagraph: TdxParagraph): TdxParagraphIndex;
var
  AControlsCountBefore: Integer;
begin
  AControlsCountBefore := ControlStack.Count;
  try
    if IsEmptyParagraph(AParagraph) then
    begin
      TryToExportBookmarks(AParagraph.FirstRunIndex, 0);
      ExportEmptyParagraph(PieceTable.Runs[AParagraph.FirstRunIndex]);
      TryToExportBookmarks(AParagraph.LastRunIndex, 0);
      Exit(AParagraph.Index);
    end
    else
      Exit(inherited ExportParagraph(AParagraph));
  finally
    if FIsHyperlinkContentExporting then
      PopControl;
    while ControlStack.Count >= AControlsCountBefore do
      PopControl;
  end;
end;

function TdxHtmlContentExporter.IsEmptyParagraph(AParagraph: TdxParagraph): Boolean;
var
  ARunCount: Integer;
begin
  ARunCount := AParagraph.LastRunIndex - AParagraph.FirstRunIndex;
  if (ARunCount = 0) or (((ARunCount = 1) and (AParagraph.PieceTable.Runs[AParagraph.FirstRunIndex].GetPlainText(PieceTable.TextBuffer) = ' '))) then
    Exit(True);
  Result := False;
end;

procedure TdxHtmlContentExporter.ExportBookmarkStart(ABookmark: TdxBookmark);
var
  AControl: TdxHtmlGenericControl;
begin
  AControl := TdxHtmlGenericControl.Create(TdxHtmlTextWriterTag.A);
  try
    if FOptions.UseHtml5 then
      AControl.Attributes.Add('id', ABookmark.Name)
    else
      AControl.Attributes.Add('name', ABookmark.Name);
    AddControlToChild(CurrentParent, AControl);
    PushControl(AControl);
    FExportedBookmarks.AddOrSetValue(ABookmark, AControl);
  finally
    FObjectsToDelete.Add(AControl);
  end;
end;

procedure TdxHtmlContentExporter.ExportBookmarkEnd(ABookmark: TdxBookmark);
var
  AControl: TdxHtmlGenericControl;
begin
  if not FExportedBookmarks.TryGetValue(ABookmark, AControl) then
    Exit;
  FExportedBookmarks.Remove(ABookmark);
  if (FControlStack.Count > 0) and (AControl <> FControlStack.Peek) then
    Exit;

  PopControl;
end;

procedure TdxHtmlContentExporter.ExportFieldCodeStartRun(ARun: TdxFieldCodeStartRun);
begin
  inherited ExportFieldCodeStartRun(ARun);
  if GetHyperlinkInfo(ARun) = nil then
    Exit;

  FinishHyperlinkExport;
  ExportTextRunCore(ARun, '');
end;

procedure TdxHtmlContentExporter.ExportFieldCodeEndRun(ARun: TdxFieldCodeEndRun);
var
  AHyperlinkInfo: TdxHyperlinkInfo;
  AForeColor, ABackColor: TdxAlphaColor;
  AUnderline, AStrikeout: Boolean;
  AParentTocField: TdxField;
begin
  inherited ExportFieldCodeEndRun(ARun);
  AHyperlinkInfo := GetHyperlinkInfo(ARun);
  if AHyperlinkInfo = nil then
    Exit;

  AForeColor := ARun.ForeColor;
  ABackColor := ARun.BackColor;

  if AForeColor = TdxAlphaColors.Empty then
    AForeColor := TdxAlphaColors.Black;
  if ABackColor = TdxAlphaColors.Empty then
    ABackColor := TdxAlphaColors.Transparent;
  AUnderline := ARun.FontUnderlineType <> TdxUnderlineType.None;
  if FOptions.UnderlineTocHyperlinks then
  begin
    AParentTocField := PieceTable.FindFieldByRunIndex(ARun.GetRunIndex).Parent;
    if (AParentTocField <> nil) and PieceTable.IsTocField(AParentTocField) then
      AUnderline := True;
  end;
  AStrikeout := ARun.FontStrikeoutType <> TdxStrikeoutType.None;

  StartHyperlinkExport(AHyperlinkInfo, CreateHtmlExporterTextProperties(ARun.FontCacheIndex, AForeColor, ABackColor,
    AUnderline, AStrikeout, ARun.Script, ARun.AllCaps, ARun.DoubleFontSize));
end;

function TdxHtmlContentExporter.CreateHtmlExporterTextProperties(AFontCacheIndex: Integer;
  AForeColor, ABackColor: TdxAlphaColor; AUnderline, AStrikeout: Boolean;
  AScript: TdxCharacterFormattingScript; AAllCaps: Boolean; ADoubleFontSize: Integer): TdxHtmlExporterTextProperties;
begin
  Result := TdxHtmlExporterTextProperties.Create(AFontCacheIndex, AForeColor, ABackColor, AUnderline, AStrikeout, AScript, AAllCaps, ADoubleFontSize);
  FObjectsToDelete.Add(Result);
end;

procedure TdxHtmlContentExporter.StartHyperlinkExport(AHyperlinkInfo: TdxHyperlinkInfo);
var
  AControl: TdxWebControlBase;
begin
  AControl := CreateHyperlinkControl(AHyperlinkInfo);
  StartHyperlinkExportCore(AControl, AHyperlinkInfo);
end;

procedure TdxHtmlContentExporter.StartHyperlinkExport(AHyperlinkInfo: TdxHyperlinkInfo; ATextProperties: TdxHtmlExporterTextProperties);
var
  AControl: TdxWebControlBase;
begin
  AControl := CreateHyperlinkControl(AHyperlinkInfo, ATextProperties);
  StartHyperlinkExportCore(AControl, AHyperlinkInfo);
end;

procedure TdxHtmlContentExporter.StartHyperlinkExportCore(AControl: TdxWebControlBase; AHyperlinkInfo: TdxHyperlinkInfo);
var
  AChildrenCount: Integer;
  ALastChild: TdxWebControlBase;
begin
  AChildrenCount := CurrentParent.Controls.Count;
  if AChildrenCount > 0 then
    ALastChild := CurrentParent.Controls[AChildrenCount - 1]
  else
    ALastChild := nil;
  if (ALastChild <> nil) and (FLastCreatedTextControl = ALastChild) then
    AddControlToChild(FLastCreatedTextControl, AControl)
  else
    AddControlToChild(CurrentParent, AControl);
  PushControl(AControl);
  FIsHyperlinkContentExporting := True;
  FCurrentHyperlinkInfo := AHyperlinkInfo;
end;

function TdxHtmlContentExporter.GetHyperlinkInfo(ARun: TdxTextRun): TdxHyperlinkInfo;
var
  ARunIndex: TdxRunIndex;
  AField: TdxField;
begin
  ARunIndex := ARun.GetRunIndex;
  AField := PieceTable.FindFieldByRunIndex(ARunIndex);
  if not PieceTable.HyperlinkInfos.TryGetHyperlinkInfo(AField.Index, Result) then
    Result := nil;
end;

function TdxHtmlContentExporter.CreateHyperlinkControl(AHyperlinkInfo: TdxHyperlinkInfo): TdxHtmlAnchor;
begin
  Result := CreateHyperlinkControlCore(AHyperlinkInfo);
end;

function TdxHtmlContentExporter.CreateHyperlinkControl(AHyperlinkInfo: TdxHyperlinkInfo;
  ATextProperties: TdxHtmlExporterTextProperties): TdxHtmlAnchor;
begin
  Result := CreateHyperlinkControlCore(AHyperlinkInfo);
  FExportHelper.SetTextProperties(Result, ATextProperties, False);
  FLastExportedHyperLinkTextProperties := ATextProperties;
end;

function TdxHtmlContentExporter.CreateHyperlinkControlCore(AHyperlinkInfo: TdxHyperlinkInfo): TdxHtmlAnchor;
begin
  Result := TdxHtmlAnchor.Create;
  FObjectsToDelete.Add(Result);

  if AHyperlinkInfo.NavigateUri <> '' then
  begin
    if AHyperlinkInfo.Anchor = '' then
      Result.HRef := TdxHyperlinkUriHelper.ConvertToUrl(AHyperlinkInfo.NavigateUri)
    else
      Result.HRef := TdxHyperlinkUriHelper.ConvertToUrl(AHyperlinkInfo.NavigateUri + '#' + AHyperlinkInfo.Anchor);
  end
  else
    if AHyperlinkInfo.Anchor <> '' then
      Result.HRef := '#' + AHyperlinkInfo.Anchor;
  if AHyperlinkInfo.Target <> '' then
    Result.Target := AHyperlinkInfo.Target;
  if AHyperlinkInfo.ToolTip <> '' then
    Result.Title := AHyperlinkInfo.ToolTip;
end;

procedure TdxHtmlContentExporter.ExportFieldResultEndRun(ARun: TdxFieldResultEndRun);
begin
  if GetHyperlinkInfo(ARun) = nil then
    Exit;
  FinishHyperlinkExport;
end;

procedure TdxHtmlContentExporter.FinishHyperlinkExport;
begin
  if (FControlStack.Count > 0) and FIsHyperlinkContentExporting then
  begin
    PopControl;
    FIsHyperlinkContentExporting := False;
    FCurrentHyperlinkInfo := nil;
  end;
end;

function TdxHtmlContentExporter.CreateParagraphControl(AParagraph: TdxParagraph): TdxWebControlBase;
begin
  Result := FExportHelper.CreateParagraphControl(AParagraph, FOptions.IgnoreParagraphOutlineLevel,
    GetListLevelNestingLevel, FParentListLeftIndent);
end;

function TdxHtmlContentExporter.CreateParagraphInListControl(AParagraph: TdxParagraph; const ACounters: TIntegerDynArray): TdxWebControlBase;
var
  ANumberingList: TdxNumberingList;
begin
  ANumberingList := DocumentModel.NumberingLists[AParagraph.GetNumberingListIndex];
  EnsureNumberingListControl(AParagraph, ANumberingList, ACounters);
  Result := CreateListLevelControl(AParagraph, ANumberingList, FExportHelper.GetMostNestedCounter(ACounters));
end;

procedure TdxHtmlContentExporter.DecreaseListControlNestingLevel;
begin
  Dec(FListControlNestingLevel);
  FCurrentListLeftIndent := FListLeftIndents.Pop;
  if FListLeftIndents.Count > 0 then
    FParentListLeftIndent := FListLeftIndents.Peek + DocumentModel.UnitConverter.DocumentsToModelUnits(150)
  else
    FParentListLeftIndent := 0;
end;

procedure TdxHtmlContentExporter.IncreaseListControlNestingLevel;
begin
  Inc(FListControlNestingLevel);
  FListLeftIndents.Push(FCurrentListLeftIndent);
  FParentListLeftIndent := FCurrentListLeftIndent + DocumentModel.UnitConverter.DocumentsToModelUnits(150);
end;

function TdxHtmlContentExporter.GetListLevelNestingLevel: Integer;
begin
  Result := FListControlNestingLevel;
end;

procedure TdxHtmlContentExporter.EnsureNumberingListControl(AParagraph: TdxParagraph; ANumberingList: TdxNumberingList;
  const ACounters: TIntegerDynArray);
var
  ANumberingListWebControl: TdxNumberingListWebControl;
  AListLevelIndex: Integer;
begin
  ANumberingListWebControl := Safe<TdxNumberingListWebControl>.Cast(CurrentParent);
  if ANumberingListWebControl = nil then
  begin
    if IsRootFirstParagraphInListHasNonZeroLevel(AParagraph) then
      CreateSeveralParentNumberingListToMakeNewNumberingListNested(AParagraph, ANumberingList, ACounters);
    CreateNumberingListControl(AParagraph, ANumberingList, FExportHelper.GetMostNestedCounter(ACounters));
    Exit;
  end;
  AListLevelIndex := AParagraph.GetListLevelIndex;
  if ANumberingListWebControl.AbstractListIndex <> ANumberingList.AbstractNumberingListIndex then
  begin
    ClosePreviousNumberingList;

    if AListLevelIndex < ANumberingListWebControl.CurrentLevelIndex then
      CloseListsWithSmallerLevelIndex(AListLevelIndex);

    if IsRootFirstParagraphInListHasNonZeroLevel(AParagraph) then
      CreateSeveralParentNumberingListToMakeNewNumberingListNested(AParagraph, ANumberingList, ACounters);

    CreateNumberingListControl(AParagraph, ANumberingList, FExportHelper.GetMostNestedCounter(ACounters));
    Exit;
  end;
  if AListLevelIndex > ANumberingListWebControl.CurrentLevelIndex then
    CreateNumberingListControl(AParagraph, ANumberingList, FExportHelper.GetMostNestedCounter(ACounters));

  if AListLevelIndex < ANumberingListWebControl.CurrentLevelIndex then
    CloseListsWithSmallerLevelIndex(AListLevelIndex);
end;

function TdxHtmlContentExporter.IsRootFirstParagraphInListHasNonZeroLevel(AParagraph: TdxParagraph): Boolean;
begin
  Result := (Safe<TdxNumberingListWebControl>.Cast(CurrentParent) = nil) and (AParagraph.GetListLevelIndex <> 0);
end;

procedure TdxHtmlContentExporter.CreateSeveralParentNumberingListToMakeNewNumberingListNested(AParagraph: TdxParagraph;
  ANumberingList: TdxNumberingList; const ACounters: TIntegerDynArray);
var
  ACount, I: Integer;
begin
  ACount := AParagraph.GetListLevelIndex;
  for I := 0 to ACount - 1 do
    CreateNumberingListControl(AParagraph, ANumberingList, ACounters[Min(I, Length(ACounters) - 1)]);
end;

procedure TdxHtmlContentExporter.CloseListsWithSmallerLevelIndex(AListLevelIndex: Integer);
var
  ANumberingListWebControl: TdxNumberingListWebControl;
begin
  ANumberingListWebControl := Safe<TdxNumberingListWebControl>.Cast(CurrentParent);
  if ANumberingListWebControl = nil then
    Exit;
  repeat
    ClosePreviousNumberingList;
    ANumberingListWebControl := Safe<TdxNumberingListWebControl>.Cast(CurrentParent);
    if ANumberingListWebControl = nil then
      Break;
  until AListLevelIndex >= ANumberingListWebControl.CurrentLevelIndex;
end;

procedure TdxHtmlContentExporter.CreateNumberingListControl(AParagraph: TdxParagraph; ANumberingList: TdxNumberingList; ACounter: Integer);
var
  AParent: TdxNumberingListWebControl;
  ALastItem: TdxHtmlGenericControl;
  ANumberingListControl: TdxWebControlBase;
begin
  AParent := Safe<TdxNumberingListWebControl>.Cast(CurrentParent);
  if (AParent <> nil) and (AParent.Controls.Count > 0) then
  begin
    ALastItem := Safe<TdxHtmlGenericControl>.Cast(AParent.Controls[AParent.Controls.Count - 1]);
    if (ALastItem <> nil) and (ALastItem.TagKey = TdxHtmlTextWriterTag.Li) then
      PushControl(ALastItem);
  end;
  ANumberingListControl := CreateNumberingListControlCore(AParagraph, ANumberingList, ACounter);
  AddControlToChild(CurrentParent, ANumberingListControl);
  PushControl(ANumberingListControl);
  IncreaseListControlNestingLevel;
end;

function TdxHtmlContentExporter.CreateNumberingListControlCore(AParagraph: TdxParagraph; ANumberingList: TdxNumberingList;
  ACounter: Integer): TdxWebControlBase;
begin
  Result := FExportHelper.CreateNumberingListControl(AParagraph, ANumberingList, ACounter);
end;

function TdxHtmlContentExporter.CreateListLevelControl(AParagraph: TdxParagraph; ANumberingList: TdxNumberingList;
  ACounter: Integer): TdxWebControlBase;
begin
  FCurrentListLeftIndent := AParagraph.LeftIndent;
  Result := FExportHelper.CreateListLevelControl(AParagraph, ANumberingList, GetListLevelNestingLevel, FParentListLeftIndent, ACounter);
end;

procedure TdxHtmlContentExporter.ExportEmptyParagraph(ARun: TdxTextRunBase);
var
  AForeColor, ABackColor: TdxAlphaColor;
begin
  AForeColor := ARun.ForeColor;
  ABackColor := ARun.BackColor;

  if AForeColor = TdxAlphaColors.Empty then
    AForeColor := TdxAlphaColors.Black;
  if ABackColor = TdxAlphaColors.Empty then
    ABackColor := TdxAlphaColors.Transparent;
  ExportCore(CreateHtmlExporterTextProperties(ARun.FontCacheIndex, AForeColor, ABackColor, False, False,
    TdxCharacterFormattingScript.Normal, ARun.AllCaps, ARun.DoubleFontSize), '&nbsp;');
end;

procedure TdxHtmlContentExporter.ExportTextRun(ARun: TdxTextRun);
var
  AText: string;
begin
  AText := ARun.GetPlainText(PieceTable.TextBuffer);
  if FKeepLeadingWhitespace then
  begin
    if TdxStringHelper.StartsWith(AText, ' ') then
      AText := TdxCharacters.NonBreakingSpace + TdxStringHelper.Substring(AText, 1);
    FKeepLeadingWhitespace := False;
  end;
  AText := ProcessRunText(ARun, AText);

  if AText <> '' then
    ExportTextRunCore(ARun, AText);
end;

function TdxHtmlContentExporter.ProcessRunText(ARun: TdxTextRun; const AText: string): string;
begin
  Result := ExportHelper.HtmlStyleHelper.PreprocessHtmlContentText(AText, FOptions.TabMarker);
end;

procedure TdxHtmlContentExporter.ExportTextRunCore(ARun: TdxTextRun; const AText: string);
begin
  ExportTextCore(ARun.FontCacheIndex, AText, ARun.ForeColor, ARun.BackColor, ARun.FontUnderlineType,
    ARun.FontStrikeoutType, ARun.Script, ARun.AllCaps, ARun.DoubleFontSize);
end;

procedure TdxHtmlContentExporter.ExportTextCore(AFontCacheIndex: Integer; const AText: string;
  AForeColor, ABackColor: TdxAlphaColor; AUnderlineType: TdxUnderlineType; AStrikeoutType: TdxStrikeoutType;
  AScript: TdxCharacterFormattingScript; AAllCaps: Boolean; ADoubleFontSize: Integer);
var
  AUnderline, AStrikeout: Boolean;
begin
  if AForeColor = TdxAlphaColors.Empty then
    AForeColor := TdxAlphaColors.Black;
  if ABackColor = TdxAlphaColors.Empty then
    ABackColor := TdxAlphaColors.Transparent;
  AUnderline := AUnderlineType <> TdxUnderlineType.None;
  AStrikeout := AStrikeoutType <> TdxStrikeoutType.None;
  ExportCore(CreateHtmlExporterTextProperties(AFontCacheIndex, AForeColor, ABackColor, AUnderline, AStrikeout,
    AScript, AAllCaps, ADoubleFontSize), AText);
end;

procedure TdxHtmlContentExporter.ExportInlinePictureRun(ARun: TdxInlinePictureRun);
var
  AForeColor, ABackColor: TdxAlphaColor;
  AUnderline, AStrikeout: Boolean;
  ATextProperties: TdxHtmlExporterTextProperties;
  AParentControls: TdxWebControlCollection;
  AParentControlsCount: Integer;
  APrevControl, AImageControl: TdxWebControlBase;
  AWebImageControl: TdxWebImageControl;
  ATextControl: TdxHtmlControl;
begin
  if ARun.ForeColor <> TdxAlphaColors.Empty then
    AForeColor := ARun.ForeColor
  else
    AForeColor := TdxAlphaColors.Black;
  if ARun.BackColor <> TdxAlphaColors.Empty then
    ABackColor := ARun.BackColor
  else
    ABackColor := TdxAlphaColors.Transparent;

  AUnderline := ARun.FontUnderlineType <> TdxUnderlineType.None;
  AStrikeout := ARun.FontStrikeoutType <> TdxStrikeoutType.None;

  ATextProperties := CreateHtmlExporterTextProperties(ARun.FontCacheIndex, AForeColor, ABackColor, AUnderline, AStrikeout, ARun.Script, ARun.AllCaps, ARun.DoubleFontSize);
  AParentControls := CurrentParent.Controls;
  AParentControlsCount := AParentControls.Count;
  if AParentControlsCount > 0 then
    APrevControl := AParentControls[AParentControlsCount - 1]
  else
    APrevControl := nil;
  if (APrevControl <> nil) and (FLastCreatedTextControl = APrevControl) and ATextProperties.Equals(FLastExportedTextProperties) then
  begin
    AWebImageControl := FExportHelper.CreateImageControlInternal(ARun.Image.Image, ARun.ActualSize, TdxHtmlCssFloat.NotSet);
    if AWebImageControl.ImageUrl <> '' then
      AddControlToChild(APrevControl, AWebImageControl);
  end
  else
    if (AreCharacterPropertiesDefault(ARun)) or ((CurrentParent is TdxHtmlAnchor) and ATextProperties.Equals(FLastExportedHyperLinkTextProperties)) then
    begin
      AImageControl := FExportHelper.CreateImageControl(ARun);
      AddControlToChild(CurrentParent, AImageControl);
    end
    else
    begin
      ATextControl := TdxHtmlControl(FExportHelper.CreateTextControl('', ATextProperties));
      AddControlToChild(CurrentParent, ATextControl);
      AWebImageControl := FExportHelper.CreateImageControlInternal(ARun.Image.Image, ARun.ActualSize, TdxHtmlCssFloat.NotSet);
      if AWebImageControl.ImageUrl <> '' then
        AddControlToChild(ATextControl, AWebImageControl);
      FExportHelper.SetTextProperties(ATextControl, ATextProperties, False);
      FLastCreatedTextControl := ATextControl;
      FLastExportedTextProperties := ATextProperties;
    end;
end;

function TdxHtmlContentExporter.AreCharacterPropertiesDefault(ARun: TdxInlinePictureRun): Boolean;
var
  ACharacterProperties: TdxCharacterFormattingInfo;
begin
  ACharacterProperties := TdxHtmlExportHelper.CreateDefaultCharacterProperties(DocumentModel);
  try
    Result := (ARun.DoubleFontSize = ACharacterProperties.DoubleFontSize) and
      (ARun.FontName = ACharacterProperties.FontName) and
      (ARun.FontBold = ACharacterProperties.FontBold) and
      (ARun.FontItalic = ACharacterProperties.FontItalic) and
      (ARun.BackColor = ACharacterProperties.BackColor) and
      (ARun.ForeColor = ACharacterProperties.ForeColor);
  finally
    ACharacterProperties.Free;
  end;
end;

function TdxHtmlContentExporter.ShouldExportInlinePicture(ARun: TdxInlinePictureRun): Boolean;
begin
  Result := ShouldExportRun(ARun);
end;

procedure TdxHtmlContentExporter.ExportFootNoteRun(ARun: TdxFootNoteRun);
var
  AInfo: TdxFootNoteExportInfo;
begin
  if not DocumentModel.DocumentCapabilities.FootNotesAllowed then
    Exit;

  if PieceTable.IsMain then
  begin
    inherited ExportFootNoteRun(ARun);
    AInfo := CreateFootNoteExportInfo(ARun);
    AInfo.Id := FootNoteExportInfos.Count;
    FootNoteExportInfos.Add(AInfo);
    ExportFootNoteRunReference(AInfo, ARun,
      FOptions.ActualFootNoteNumberStringFormat, FOptions.ActualFootNoteNamePrefix);
  end
  else
  begin
    if FindFootNoteExportInfoByNote(FootNoteExportInfos, PieceTable, AInfo) then
      ExportFootNoteRunReference(AInfo, ARun,
        FOptions.ActualFootNoteNumberStringFormat, FOptions.ActualFootNoteNamePrefix);
  end;
end;

procedure TdxHtmlContentExporter.ExportEndNoteRun(ARun: TdxEndNoteRun);
var
  AInfo: TdxFootNoteExportInfo;
begin
  if not DocumentModel.DocumentCapabilities.EndNotesAllowed then
    Exit;

  if PieceTable.IsMain then
  begin
    inherited ExportEndNoteRun(ARun);
    AInfo := CreateFootNoteExportInfo(ARun);
    AInfo.Id := EndNoteExportInfos.Count;
    EndNoteExportInfos.Add(AInfo);
    ExportFootNoteRunReference(AInfo, ARun,
      FOptions.ActualEndNoteNumberStringFormat, FOptions.ActualEndNoteNamePrefix);
  end
  else
  begin
    if FindFootNoteExportInfoByNote(EndNoteExportInfos, PieceTable, AInfo) then
      ExportFootNoteRunReference(AInfo, ARun,
        FOptions.ActualEndNoteNumberStringFormat, FOptions.ActualEndNoteNamePrefix);
  end;
end;

procedure TdxHtmlContentExporter.ExportFootNoteRunReference(const AInfo: TdxFootNoteExportInfo;
  ARun: TdxFootNoteRunBase; const AFormat, APrefix: string);
var
  AControl: TdxHtmlAnchor;
  AHref, AAnchor, AText: string;
begin
  AControl := TdxHtmlAnchor.Create;
  try
    if PieceTable.IsMain then
      AHref := APrefix
    else
      AHref := APrefix + '_GoBack';
    if PieceTable.IsMain then
      AAnchor := APrefix + '_GoBack'
    else
      AAnchor := APrefix;
    AHref := AHref + IntToStr(AInfo.Id);
    AAnchor := AAnchor + IntToStr(AInfo.Id);
    AControl.HRef := '#' + AHref;
    AControl.Name := AAnchor;
    try
      AddControlToChild(CurrentParent, AControl);
      PushControl(AControl);

      AText := Format(AFormat, [AInfo.NumberText, AInfo.Number, AInfo.Id]);
      ExportTextRunCore(ARun, AText);
    finally
      PopControl;
    end;
  finally
    AControl.Free;
  end;
end;

procedure TdxHtmlContentExporter.ExportFloatingObjectAnchorRun(ARun: TdxFloatingObjectAnchorRun);
var
  AControl: TdxHtmlGenericControl;
begin
  AControl := TdxHtmlGenericControl.Create(TdxHtmlTextWriterTag.Div);
  try
    AddControlToChild(CurrentParent, AControl);
    PushControl(AControl);
    try
      ExportFloatingObjectFrame(ARun, AControl);
      ExportFloatingObjectContent(ARun);
    finally
      PopControl;
    end;
  finally
    FObjectsToDelete.Add(AControl);
  end;
end;

procedure TdxHtmlContentExporter.ExportFloatingObjectFrame(ARun: TdxFloatingObjectAnchorRun; AControl: TdxHtmlGenericControl);
var
  AUnitConverter: TdxDocumentModelUnitConverter;
  AStyle: TdxCssStyleCollection;
  AShape: TdxShape;
  AFloatingObjectProperties: TdxFloatingObjectProperties;
  AWidth, AHeight: Integer;
  ATextBoxContent: TdxTextBoxFloatingObjectContent;
  AProperties: TdxTextBoxProperties;
  ACssFloat: TdxHtmlCssFloat;
begin
  AUnitConverter := DocumentModel.UnitConverter;
  AStyle := AControl.Style;
  AShape := ARun.Shape;
  if (AShape.OutlineWidth > 0) and not TdxAlphaColors.IsTransparentOrEmpty(AShape.OutlineColor) then
  begin
    AStyle.Add(TdxHtmlTextWriterStyle.BorderWidth,
      TdxStringHelper.Format(TdxCultureInfo.InvariantCulture.FormatSettings, '%gpt', [AUnitConverter.ModelUnitsToPointsF(AShape.OutlineWidth)]));
    AStyle.Add(TdxHtmlTextWriterStyle.BorderStyle, 'solid');
    AStyle.Add(TdxHtmlTextWriterStyle.BorderColor, TdxAlphaColors.ToHtml(AShape.OutlineColor));
  end;
  if not TdxAlphaColors.IsTransparentOrEmpty(AShape.FillColor) then
    AStyle.Add(TdxHtmlTextWriterStyle.BackgroundColor, TdxAlphaColors.ToHtml(AShape.FillColor));

  AFloatingObjectProperties := ARun.FloatingObjectProperties;
  if (AFloatingObjectProperties.LeftDistance > 0) or (AFloatingObjectProperties.RightDistance > 0) or
    (AFloatingObjectProperties.TopDistance > 0) or (AFloatingObjectProperties.BottomDistance > 0) then
  begin
    AStyle.Add(TdxHtmlTextWriterStyle.Margin,
      TdxStringHelper.Format(TdxCultureInfo.InvariantCulture.FormatSettings, '%gpt %gpt %gpt %gpt',
        [AUnitConverter.ModelUnitsToPointsF(AFloatingObjectProperties.TopDistance),
         AUnitConverter.ModelUnitsToPointsF(AFloatingObjectProperties.RightDistance),
         AUnitConverter.ModelUnitsToPointsF(AFloatingObjectProperties.BottomDistance),
         AUnitConverter.ModelUnitsToPointsF(AFloatingObjectProperties.LeftDistance)]));
  end;

  AWidth := AFloatingObjectProperties.ActualSize.Width;
  AHeight := AFloatingObjectProperties.ActualSize.Height;

  ATextBoxContent := Safe<TdxTextBoxFloatingObjectContent>.Cast(ARun.Content);
  if ATextBoxContent <> nil then
  begin
    AProperties := ATextBoxContent.TextBoxProperties;
    if (AProperties.LeftMargin > 0) or (AProperties.RightMargin > 0) or (AProperties.TopMargin > 0) or (AProperties.BottomMargin > 0) then
      AStyle.Add(TdxHtmlTextWriterStyle.Padding, TdxStringHelper.Format(TdxCultureInfo.InvariantCulture.FormatSettings, '%gpt %gpt %gpt %gpt', [
        AUnitConverter.ModelUnitsToPointsF(AProperties.TopMargin),
        AUnitConverter.ModelUnitsToPointsF(AProperties.RightMargin),
        AUnitConverter.ModelUnitsToPointsF(AProperties.BottomMargin),
        AUnitConverter.ModelUnitsToPointsF(AProperties.LeftMargin)]));
    Dec(AWidth, 2 * AShape.OutlineWidth + AProperties.LeftMargin + AProperties.RightMargin);
    Dec(AHeight, AShape.OutlineWidth + AProperties.TopMargin + AProperties.BottomMargin);
  end;
  AStyle.Add(TdxHtmlTextWriterStyle.Width, TdxStringHelper.Format(TdxCultureInfo.InvariantCulture.FormatSettings, '%gpt', [AUnitConverter.ModelUnitsToPointsF(AWidth)]));
  AStyle.Add(TdxHtmlTextWriterStyle.Height, TdxStringHelper.Format(TdxCultureInfo.InvariantCulture.FormatSettings, '%gpt', [AUnitConverter.ModelUnitsToPointsF(AHeight)]));
  ACssFloat := GetCssFloat(ARun);
  if ACssFloat <> TdxHtmlCssFloat.NotSet then
  begin
    if ACssFloat = TdxHtmlCssFloat.Left then
      AControl.Style.Add('float', 'left')
    else
      if ACssFloat = TdxHtmlCssFloat.Right then
        AControl.Style.Add('float', 'right');
  end;
end;

procedure TdxHtmlContentExporter.ExportFloatingObjectContent(ARun: TdxFloatingObjectAnchorRun);
var
  ATextBoxContent: TdxTextBoxFloatingObjectContent;
  APictureContent: TdxPictureFloatingObjectContent;
begin
  ATextBoxContent := Safe<TdxTextBoxFloatingObjectContent>.Cast(ARun.Content);
  if ATextBoxContent <> nil then
  begin
    ExportFloatingObjectTextBoxContent(ATextBoxContent, ARun);
    Exit;
  end;
  APictureContent := Safe<TdxPictureFloatingObjectContent>.Cast(ARun.Content);
  if APictureContent <> nil then
    ExportFloatingObjectPictureContent(APictureContent, ARun);
end;

procedure TdxHtmlContentExporter.ExportFloatingObjectTextBoxContent(AContent: TdxTextBoxFloatingObjectContent; ARun: TdxFloatingObjectAnchorRun);
begin
  PerformExportPieceTable(TdxPieceTable(AContent.TextBox.PieceTable), ExportPieceTable);
end;

procedure TdxHtmlContentExporter.ExportFloatingObjectPictureContent(AContent: TdxPictureFloatingObjectContent; ARun: TdxFloatingObjectAnchorRun);
begin
  AddControlToChild(CurrentParent, CreateImageControl(AContent, ARun));
end;

function TdxHtmlContentExporter.CreateImageControl(AContent: TdxPictureFloatingObjectContent; ARun: TdxFloatingObjectAnchorRun): TdxWebControlBase;
begin
  Result := FExportHelper.CreateImageControl(AContent.Image.Image, ARun.FloatingObjectProperties.ActualSize, TdxHtmlCssFloat.NotSet);
end;

function TdxHtmlContentExporter.GetCssFloat(ARun: TdxFloatingObjectAnchorRun): TdxHtmlCssFloat;
var
  AProperties: TdxFloatingObjectProperties;
begin
  AProperties := ARun.FloatingObjectProperties;
  if (AProperties.TextWrapType <> TdxFloatingObjectTextWrapType.Square) and (AProperties.TextWrapType = TdxFloatingObjectTextWrapType.Tight) then
    Exit(TdxHtmlCssFloat.NotSet);
  if AProperties.HorizontalPositionAlignment = TdxFloatingObjectHorizontalPositionAlignment.Left then
    Exit(TdxHtmlCssFloat.Left)
  else
    if AProperties.HorizontalPositionAlignment = TdxFloatingObjectHorizontalPositionAlignment.Right then
      Exit(TdxHtmlCssFloat.Right);
  Result := TdxHtmlCssFloat.NotSet;
end;

{ TdxHtmlTableStyleHelper }

constructor TdxHtmlTableStyleHelper.Create(AUnitConverter: TdxDocumentModelUnitConverter; AStyleHelper: TdxHtmlStyleHelper);
begin
  inherited Create;
  FUnitConverter := AUnitConverter;
  FStyleHelper := AStyleHelper;
end;

function TdxHtmlTableStyleHelper.GetHtmlTableStyle(ATable: TdxTable): string;
var
  AStyle: TdxCssStyleCollection;
begin
  AStyle := TdxCssStyleCollection.Create;
  try
    GetHtmlTableStyle(ATable, AStyle);
    Result := HtmlStyleHelper.CreateCssStyle(AStyle, ';');
  finally
    AStyle.Free;
  end;
end;

procedure TdxHtmlTableStyleHelper.GetHtmlTableStyle(ATable: TdxTable; AStyle: TdxCssStyleCollection);
var
  ATableIndent: Single;
begin
  Assert(ATable <> nil, 'table');
  ATableIndent := ConvertWidthUnitToPointsF(ATable.TableIndent);
  if ATableIndent <> 0 then
    AStyle.Add('margin-left', TdxHtmlStyleHelper.GetHtmlSizeInPoints(ATableIndent));

  if ATable.TableProperties.Info.UseBackgroundColor then
    AStyle.Add('background-color', TdxHtmlConvert.ToHtml(ATable.BackgroundColor));
end;

function TdxHtmlTableStyleHelper.GetHtmlBorder(ABorder: TdxBorderBase): string;
var
  AHtmlColor, ABorderWidth, AHtmlBorderStyle: string;
  AWidth: Double;
begin
  if IsBorderNil(ABorder) then
    Exit('none');
  if TdxAlphaColors.IsEmpty(ABorder.Color) then
    AHtmlColor := 'windowtext'
  else
    AHtmlColor := TdxHtmlConvert.ToHtml(ABorder.Color);

  AWidth := Max(1.0, SimpleRoundTo(UnitConverter.ModelUnitsToPointsF(ABorder.Width), -1));
  ABorderWidth := FormatFloatInvariantCulture(AWidth) + 'pt';
  AHtmlBorderStyle := GetHtmlBorderStyle(ABorder.Style);
  Result := ABorderWidth + ' ' + AHtmlColor + ' ' + AHtmlBorderStyle;
end;

function TdxHtmlTableStyleHelper.IsBorderNil(ABorder: TdxBorderBase): Boolean;
begin
  Result := ABorder.Style in [TdxBorderLineStyle.None, TdxBorderLineStyle.Nil, TdxBorderLineStyle.Disabled];
end;

function TdxHtmlTableStyleHelper.GetHtmlBorderStyle(ABorderStyle: TdxBorderLineStyle): string;
begin
  case ABorderStyle of
    TdxBorderLineStyle.Dotted:
      Result := 'dotted';
    TdxBorderLineStyle.Dashed:
      Result := 'dashed';
    TdxBorderLineStyle.Double:
      Result := 'double';
    TdxBorderLineStyle.Inset:
      Result := 'inset';
    TdxBorderLineStyle.Outset:
      Result := 'outset';
    else
      Result := 'solid';
  end;
end;

function TdxHtmlTableStyleHelper.GetHtmlTableCellStyle(ACell: TdxTableCell; AActualBottomBorderCell: TdxBorderBase): string;
var
  AStyle: TdxCssStyleCollection;
begin
  AStyle := TdxCssStyleCollection.Create;
  try
    GetHtmlTableCellStyle(ACell, AStyle, AActualBottomBorderCell);
    Result := HtmlStyleHelper.CreateCssStyle(AStyle, ';');
  finally
    AStyle.Free;
  end;
end;

procedure TdxHtmlTableStyleHelper.GetHtmlTableCellStyle(ACell: TdxTableCell; AStyle: TdxCssStyleCollection; AActualBottomBorderCell: TdxBorderBase);
var
  AWidthContent: string;
begin
  if ACell.Properties.UsePreferredWidth then
  begin
    AWidthContent := ConvertWidthUnitToPoints(ACell.PreferredWidth);
    if AWidthContent <> '' then
      AStyle.Add('width', AWidthContent);
  end;

  AStyle.Add('padding', HtmlStyleHelper.GetHtmlSizeInPoints(UnitConverter.ModelUnitsToPointsF(ACell.GetActualTopMargin.Value)) + ' ' +
    HtmlStyleHelper.GetHtmlSizeInPoints(UnitConverter.ModelUnitsToPointsF(ACell.GetActualRightMargin.Value)) + ' ' +
    HtmlStyleHelper.GetHtmlSizeInPoints(UnitConverter.ModelUnitsToPointsF(ACell.GetActualBottomMargin.Value)) + ' ' +
    HtmlStyleHelper.GetHtmlSizeInPoints(UnitConverter.ModelUnitsToPointsF(ACell.GetActualLeftMargin.Value)));

  if ACell.Properties.Info.UseBackgroundColor then
    AStyle.Add('background-color', TdxHtmlConvert.ToHtml(ACell.BackgroundColor));

  AStyle.Add('border-top', GetHtmlBorder(ACell.GetActualTopCellBorder));
  AStyle.Add('border-right', GetHtmlBorder(ACell.GetActualRightCellBorder));
  if AActualBottomBorderCell = nil then
    AActualBottomBorderCell := ACell.GetActualBottomCellBorder;
  AStyle.Add('border-bottom', GetHtmlBorder(AActualBottomBorderCell));
  AStyle.Add('border-left', GetHtmlBorder(ACell.GetActualLeftCellBorder));
end;

function TdxHtmlTableStyleHelper.ConvertWidthUnitToPixels(AWidthUnit: TdxWidthUnit): string;
begin
  if AWidthUnit.&Type = TdxWidthUnitType.ModelUnits then
    Result := IntToStr(UnitConverter.ModelUnitsToPixels(AWidthUnit.Value, UnitConverter.ScreenDpi))
  else
    if AWidthUnit.&Type = TdxWidthUnitType.FiftiethsOfPercent then
      Result := HtmlStyleHelper.GetHtmlSizeInPercents(AWidthUnit.Value div 50)
    else
      Result := '0';
end;

function TdxHtmlTableStyleHelper.ConvertWidthUnitToPoints(AWidthUnit: TdxWidthUnit): string;
begin
  if AWidthUnit.&Type = TdxWidthUnitType.ModelUnits then
    Result := HtmlStyleHelper.GetHtmlSizeInPoints(UnitConverter.ModelUnitsToPointsF(AWidthUnit.Value))
  else
    if AWidthUnit.&Type = TdxWidthUnitType.FiftiethsOfPercent then
      Result := HtmlStyleHelper.GetHtmlSizeInPercents(AWidthUnit.Value div 50)
    else
      Result := '';
end;

function TdxHtmlTableStyleHelper.ConvertWidthUnitToPointsF(AWidthUnit: TdxWidthUnit): Single;
begin
  if AWidthUnit.&Type = TdxWidthUnitType.ModelUnits then
    Result := UnitConverter.ModelUnitsToPointsF(AWidthUnit.Value)
  else
    if AWidthUnit.&Type = TdxWidthUnitType.FiftiethsOfPercent then
      Result := AWidthUnit.Value / 50
    else
      Result := 0;
end;

function TdxHtmlTableStyleHelper.GetHtmlVerticalAlignment(AVAlignment: TdxVerticalAlignment): string;
begin
  case AVAlignment of
    TdxVerticalAlignment.Top:
      Result := 'top';
    TdxVerticalAlignment.Bottom:
      Result := 'bottom';
    else
      Result := 'middle';
  end;
end;

function TdxHtmlTableStyleHelper.GetHtmlTableAlignment(ATableAlignment: TdxTableRowAlignment): string;
begin
  case ATableAlignment of
    TdxTableRowAlignment.Center:
      Result := 'center';
    TdxTableRowAlignment.Right:
      Result := 'right';
    else
      Result := 'left';
  end;
end;

{ TdxHtmlExportHelper }

constructor TdxHtmlExportHelper.Create(ADocumentModel: TdxDocumentModel; const AScriptContainer: IdxScriptContainer; const AImageRepository: IdxOfficeImageRepository;
  AOptions: TdxHtmlDocumentExporterOptions);
begin
  inherited Create;
  FDocumentModel := ADocumentModel;
  FScriptContainer := AScriptContainer;
  FImageRepository := AImageRepository;
  FOptions := AOptions;
  FObjectsToDelete := TdxFastObjectList.Create;
  FText := TStringBuilder.Create;
  FHtmlStyleHelper := TdxHtmlStyleHelper.Create;
  FTableStyleHelper := TdxHtmlTableStyleHelper.Create(ADocumentModel.UnitConverter, FHtmlStyleHelper);
  FParagraphStyleHelper := TdxHtmlParagraphStyleHelper.Create(ADocumentModel.UnitConverter, FHtmlStyleHelper);
  FParagraphTabsHelper := TdxHtmlParagraphTabsHelper.Create(ADocumentModel.UnitConverter, FHtmlStyleHelper);
  FImageHelper := TdxOfficeHtmlImageHelper.Create(ADocumentModel, ADocumentModel.UnitConverter);
  if FOptions.OverrideImageResolution <> TdxHtmlDocumentExporterOptions.DefaultOverrideImageResolution then
  begin
    FImageHelper.HorizontalResolution := FOptions.OverrideImageResolution;
    FImageHelper.VerticalResolution := FOptions.OverrideImageResolution;
  end;
end;

destructor TdxHtmlExportHelper.Destroy;
begin
  FreeAndNil(FText);
  FreeAndNil(FHtmlStyleHelper);
  FreeAndNil(FTableStyleHelper);
  FreeAndNil(FParagraphStyleHelper);
  FreeAndNil(FParagraphTabsHelper);
  FreeAndNil(FImageHelper);
  FreeAndNil(FObjectsToDelete);
  inherited Destroy;
end;

function TdxHtmlExportHelper.GetUnitConverter: TdxDocumentModelUnitConverter;
begin
  Result := FDocumentModel.UnitConverter;
end;

function TdxHtmlExportHelper.GetIsExportInlineStyle: Boolean;
begin
  Result := FOptions.CssPropertiesExportType = TdxCssPropertiesExportType.Inline;
end;

function TdxHtmlExportHelper.GetDisposeConvertedImagesImmediately: Boolean;
begin
  Result := FOptions.DisposeConvertedImagesImmediately;
end;

function TdxHtmlExportHelper.CreateTextControl(const AText: string; ATextProperties: TdxHtmlExporterTextProperties): TdxWebControlBase;
var
  AControl: TdxHtmlGenericControl;
begin
  Assert(ATextProperties.FontCacheIndex >= 0, 'fontCacheIndex');

  AControl := TdxHtmlGenericControl.Create(TdxHtmlTextWriterTag.Span);
  FObjectsToDelete.Add(AControl);

  AppendText(AControl, AText);
  Result := AControl;
end;

procedure TdxHtmlExportHelper.SetTextProperties(AControl: TdxHtmlControl; ATextProperties: TdxHtmlExporterTextProperties; AForceExportCharacterProperties: Boolean);
var
  AFontInfo: TdxFontInfo;
  AUseFontSizeInPixels, AIsHtmlAnchor: Boolean;
  AStyleCollection: TdxCssStyleCollection;
  AStyle, AStyleName: string;
begin
  AFontInfo := FDocumentModel.FontCache[ATextProperties.FontCacheIndex];

  AUseFontSizeInPixels := FOptions.FontUnit = TdxHtmlFontUnit.Pixel;
  AIsHtmlAnchor := AControl is TdxHtmlAnchor;
  if IsExportInlineStyle then
  begin
    TdxHtmlStyleRender.GetHtmlStyle(AFontInfo.Name, ATextProperties.DoubleFontSize / 2, TdxGraphicsUnit.guPoint,
      AFontInfo.Bold, AFontInfo.Italic, ATextProperties.Strikeout, ATextProperties.Underline, ATextProperties.ForeColor,
      ATextProperties.BackColor, AUseFontSizeInPixels, AControl.Style);
    if not FOptions.DefaultCharacterPropertiesExportToCss and not AForceExportCharacterProperties then
      RemoveDefaultProperties(ATextProperties, AFontInfo, AControl.Style);

    if ATextProperties.AllCaps then
      AControl.Style.Add('text-transform', 'uppercase');
    if (AIsHtmlAnchor and not ATextProperties.Underline) and not ATextProperties.Strikeout then
      AControl.Style.Add('text-decoration', 'none');
  end
  else
  begin
    AStyleCollection := TdxCssStyleCollection.Create;
    try
      TdxHtmlStyleRender.GetHtmlStyle(AFontInfo.Name, ATextProperties.DoubleFontSize / 2, TdxGraphicsUnit.guPoint,
        AFontInfo.Bold, AFontInfo.Italic, ATextProperties.Strikeout, ATextProperties.Underline, ATextProperties.ForeColor,
        ATextProperties.BackColor, AUseFontSizeInPixels, AStyleCollection);
      if not FOptions.DefaultCharacterPropertiesExportToCss and not AForceExportCharacterProperties then
        RemoveDefaultProperties(ATextProperties, AFontInfo, AStyleCollection);
      AStyle := AStyleCollection.Value;
      if ATextProperties.AllCaps then
        AStyle := AStyle + 'text-transform:uppercase;';
      if (AIsHtmlAnchor and not ATextProperties.Underline) and not ATextProperties.Strikeout then
        AStyle := AStyle + 'text-decoration: none;';

      if AStyle <> '' then
      begin
        AStyleName := FScriptContainer.RegisterCssClass(AStyle);
        AControl.CssClass := AStyleName;
      end;
    finally
      AStyleCollection.Free;
    end;
  end;
end;

procedure TdxHtmlExportHelper.RemoveDefaultProperties(ATextProperties: TdxHtmlExporterTextProperties;
  AFontInfo: TdxFontInfo; ACssStyleCollection: TdxCssStyleCollection);
var
  ADefaultCharacterProperties: TdxCharacterFormattingInfo;
begin
  ADefaultCharacterProperties := CreateDefaultCharacterProperties(FDocumentModel);
  try
    RemoveDefaultProperties(FDocumentModel, ADefaultCharacterProperties, AFontInfo.Name, AFontInfo.Bold, AFontInfo.Italic,
      ATextProperties.DoubleFontSize, ATextProperties.BackColor, ATextProperties.ForeColor, ACssStyleCollection);
  finally
    ADefaultCharacterProperties.Free;
  end;
end;

class function TdxHtmlExportHelper.CreateDefaultCharacterProperties(ADocumentModel: TdxDocumentModel): TdxCharacterFormattingInfo;
var
  AMergedProperties: TdxMergedCharacterProperties;
begin
  if ADocumentModel.CharacterStyles.Count > 0 then
  begin
    AMergedProperties := TdxMergedCharacterProperties.Create(ADocumentModel.CharacterStyles[0].CharacterProperties);
    try
      AMergedProperties.Merge(ADocumentModel.DefaultCharacterProperties);
      Result := AMergedProperties.Info.Clone;
    finally
      AMergedProperties.Free;
    end;
  end
  else
    Result := ADocumentModel.DefaultCharacterProperties.Info.Info.Clone;
end;

class procedure TdxHtmlExportHelper.TryRemoveKey(ACssStyleCollection: TdxCssStyleCollection; const AKey: string);
begin
  if ACssStyleCollection[AKey] <> '' then
    ACssStyleCollection.Remove(AKey);
end;

class procedure TdxHtmlExportHelper.RemoveDefaultProperties(ADocumentModel: TdxDocumentModel;
  ACharacterProperties: TdxCharacterFormattingInfo; const AFontName: string;
  AFontBold, AFontItalic: Boolean; ADoubleFontSize: Integer;
  ABackColor, AForeColor: TdxAlphaColor; AStyle: TdxCssStyleCollection);
begin
  if ABackColor = ACharacterProperties.BackColor then
    TryRemoveKey(AStyle, 'background-color');
  if ADoubleFontSize = ACharacterProperties.DoubleFontSize then
    TryRemoveKey(AStyle, 'font-size');
  if AFontName = ACharacterProperties.FontName then
    TryRemoveKey(AStyle, 'font-family');
  if AFontBold = ACharacterProperties.FontBold then
    TryRemoveKey(AStyle, 'font-weight');
  if AFontItalic = ACharacterProperties.FontItalic then
    TryRemoveKey(AStyle, 'font-style');
  if AForeColor = ACharacterProperties.ForeColor then
    TryRemoveKey(AStyle, 'color');
end;

procedure TdxHtmlExportHelper.AppendText(AParent: TdxWebControlBase; const AText: string);
var
  ALiteralControl: TdxHtmlLiteralControl;
begin
  ALiteralControl := TdxHtmlLiteralControl.Create(ReplaceWhiteSpaceWithNonBreakingSpace(AText));
  FObjectsToDelete.Add(ALiteralControl);

  AParent.Controls.Add(ALiteralControl);
end;

function TdxHtmlExportHelper.ReplaceWhiteSpaceWithNonBreakingSpace(const ARawText: string): string;
var
  ADoubleSpaceIndex, ACount, I: Integer;
  AIsPrevWhiteSpace: Boolean;
begin
  ADoubleSpaceIndex := TdxStringHelper.IndexOf(ARawText, '  ');
  if ADoubleSpaceIndex < 0 then
    Exit(ARawText);

  AIsPrevWhiteSpace := False;
  ACount := Length(ARawText);

  FText.Capacity := Max(FText.Capacity, ACount + Max(64, ACount div 20));
  if ADoubleSpaceIndex > 0 then
    FText.Append(TdxStringHelper.Substring(ARawText, 0, ADoubleSpaceIndex));
  for I := ADoubleSpaceIndex + 1 to ACount do
  begin
    if ARawText[I] = ' ' then
      AIsPrevWhiteSpace := ProcessSpace(ARawText[I], AIsPrevWhiteSpace)
    else
    begin
      if AIsPrevWhiteSpace then
        AIsPrevWhiteSpace := False;
      FText.Append(ARawText[I]);
    end;
  end;
  Result := FText.ToString;
  FText.Length := 0;
end;

function TdxHtmlExportHelper.ProcessSpace(ACh: Char; AIsPrevWhiteSpace: Boolean): Boolean;
begin
  if AIsPrevWhiteSpace then
    FText.Append('&nbsp;')
  else
  begin
    AIsPrevWhiteSpace := True;
    FText.Append(ACh);
  end;
  Result := AIsPrevWhiteSpace;
end;

function TdxHtmlExportHelper.CreateTableControl(ATable: TdxTable): TdxWebControlBase;
var
  ATableControl: TdxHtmlGenericControl;
  AStyle: string;
begin
  Assert(ATable <> nil, 'table');

  ATableControl := TdxHtmlGenericControl.Create(TdxHtmlTextWriterTag.Table);
  FObjectsToDelete.Add(ATableControl);

  ATableControl.Attributes.Add('border', '0');
  ATableControl.Attributes.Add('cellspacing', ConvertWidthUnitToPixels(ATable.CellSpacing));
  ATableControl.Attributes.Add('cellpadding', '0');

  if ATable.PreferredWidth.Value <> 0 then
    ATableControl.Attributes.Add('width', ConvertWidthUnitToPixels(ATable.PreferredWidth));

  if ATable.CellSpacing.Value = 0 then
    ATableControl.Style.Add('border-collapse', 'collapse');

  if ATable.TableAlignment <> TdxTableRowAlignment.Left then
    ATableControl.Attributes.Add('align', FTableStyleHelper.GetHtmlTableAlignment(ATable.TableAlignment));

  if IsExportInlineStyle then
    FTableStyleHelper.GetHtmlTableStyle(ATable, ATableControl.Style)
  else
  begin
    AStyle := FTableStyleHelper.GetHtmlTableStyle(ATable);
    ATableControl.CssClass := FScriptContainer.RegisterCssClass(AStyle);
  end;
  Result := ATableControl;
end;

function TdxHtmlExportHelper.CreateTableRowControl(ARow: TdxTableRow): TdxWebControlBase;
var
  ATableRowControl: TdxHtmlGenericControl;
  ARowHeight: string;
begin
  Assert(ARow <> nil, 'tableRow');

  ATableRowControl := TdxHtmlGenericControl.Create(TdxHtmlTextWriterTag.Tr);
  FObjectsToDelete.Add(ATableRowControl);

  if ARow.Properties.UseHeight then
  begin
    ARowHeight := HtmlStyleHelper.GetHtmlSizeInPoints(UnitConverter.ModelUnitsToPointsF(ARow.Height.Value));
    ATableRowControl.Style.Add('height', ARowHeight);
  end;
  Result := ATableRowControl;
end;

function TdxHtmlExportHelper.CreateTableCellControl(ACell: TdxTableCell; AMergedCellProperties: TdxVerticalMergeCellProperties): TdxWebControlBase;
var
  ATableCellControl: TdxHtmlGenericControl;
  ARowSpan: Integer;
  AStyle: string;
begin
  Assert(ACell <> nil, 'tableCell');

  ATableCellControl := TdxHtmlGenericControl.Create(TdxHtmlTextWriterTag.Td);
  FObjectsToDelete.Add(ATableCellControl);

  if ACell.ColumnSpan > 1 then
    ATableCellControl.Attributes.Add('colspan', IntToStr(ACell.ColumnSpan));

  ARowSpan := AMergedCellProperties.RowSpan;
  if ARowSpan > 1 then
    ATableCellControl.Attributes.Add('rowspan', IntToStr(ARowSpan));

  if ACell.VerticalAlignment <> TdxVerticalAlignment.Center then
    ATableCellControl.Attributes.Add('valign', FTableStyleHelper.GetHtmlVerticalAlignment(ACell.VerticalAlignment));

  if ACell.PreferredWidth.Value <> 0 then
    ATableCellControl.Attributes.Add('width', ConvertWidthUnitToPixels(ACell.PreferredWidth));

  if ACell.NoWrap then
    ATableCellControl.Attributes.Add('nowrap', '');

  if IsExportInlineStyle then
    FTableStyleHelper.GetHtmlTableCellStyle(ACell, ATableCellControl.Style, AMergedCellProperties.ActualBottomBorder)
  else
  begin
    AStyle := FTableStyleHelper.GetHtmlTableCellStyle(ACell, AMergedCellProperties.ActualBottomBorder);
    ATableCellControl.CssClass := FScriptContainer.RegisterCssClass(AStyle);
  end;
  Result := ATableCellControl;
end;

function TdxHtmlExportHelper.ConvertWidthUnitToPixels(AWidthUnit: TdxWidthUnit): string;
begin
  Result := FTableStyleHelper.ConvertWidthUnitToPixels(AWidthUnit);
end;

function TdxHtmlExportHelper.CreateParagraphControl(AParagraph: TdxParagraph; AIgnoreOutlineLevel: Boolean;
  AListControlNestingLevel, AParentLevelOffset: Integer): TdxWebControlBase;
var
  AParagraphControl: TdxHtmlGenericControl;
  ATabStops, AStyle: string;
begin
  Assert(AParagraph <> nil, 'paragraph');

  AParagraphControl := TdxHtmlGenericControl.Create(CalculateParagraphTag(AParagraph, AIgnoreOutlineLevel));
  FObjectsToDelete.Add(AParagraphControl);

  ATabStops := FParagraphTabsHelper.CreateTabStops(AParagraph);
  if ATabStops <> '' then
    AParagraphControl.Style.Add('tab-stops', ATabStops);
  if IsExportInlineStyle then
    FParagraphStyleHelper.GetHtmlParagraphStyle(AParagraph, AListControlNestingLevel, AParentLevelOffset, AParagraphControl.Style)
  else
  begin
    AStyle := FParagraphStyleHelper.GetHtmlParagraphStyle(AParagraph, AListControlNestingLevel, AParentLevelOffset);
    AParagraphControl.CssClass := FScriptContainer.RegisterCssClass(AStyle);
  end;
  Result := AParagraphControl;
end;

function TdxHtmlExportHelper.CalculateParagraphTag(AParagraph: TdxParagraph; AIgnoreOutlineLevel: Boolean): TdxHtmlTextWriterTag;
begin
  if AIgnoreOutlineLevel then
    Exit(TdxHtmlTextWriterTag.P);
  case AParagraph.OutlineLevel of
    1:
      Result := TdxHtmlTextWriterTag.H1;
    2:
      Result := TdxHtmlTextWriterTag.H2;
    3:
      Result := TdxHtmlTextWriterTag.H3;
    4:
      Result := TdxHtmlTextWriterTag.H4;
    5:
      Result := TdxHtmlTextWriterTag.H5;
    6:
      Result := TdxHtmlTextWriterTag.H6;
    else
      Result := TdxHtmlTextWriterTag.P;
  end;
end;

function TdxHtmlExportHelper.CreateNumberingListControl(AParagraph: TdxParagraph; ANumberingList: TdxNumberingList; ACounter: Integer): TdxWebControlBase;
var
  AHtmlTextWriterTag: TdxHtmlTextWriterTag;
  AControl: TdxNumberingListWebControl;
begin
  Assert(AParagraph <> nil, 'paragraph');
  if TdxNumberingListHelper.GetListType(ANumberingList.AbstractNumberingList) = TdxNumberingType.Bullet then
    AHtmlTextWriterTag := TdxHtmlTextWriterTag.Ul
  else
    AHtmlTextWriterTag := TdxHtmlTextWriterTag.Ol;

  AControl := TdxNumberingListWebControl.Create(AHtmlTextWriterTag, ANumberingList.AbstractNumberingListIndex, AParagraph.GetListLevelIndex);
  FObjectsToDelete.Add(AControl);

  AControl.Style.Add(TdxHtmlTextWriterStyle.MarginTop, '0');
  AControl.Style.Add(TdxHtmlTextWriterStyle.MarginBottom, '0');

  if ShouldAddNumberingListStartAttribute(AParagraph, ANumberingList) then
    AControl.Attributes.Add('start', IntToStr(ACounter));
  Result := AControl;
end;

function TdxHtmlExportHelper.ShouldAddNumberingListStartAttribute(AParagraph: TdxParagraph; ANumberingList: TdxNumberingList): Boolean;
var
  AIsFirstParagraph, ADifferNumberingLists: Boolean;
  ACurParagraphLevel, APrevParagraphLevel: Integer;
  APreviousParagraph: TdxParagraph;
begin
  AIsFirstParagraph := AParagraph.Index = 0;
  ACurParagraphLevel := AParagraph.GetListLevelIndex;

  if AIsFirstParagraph then
    Exit(AParagraph.IsInList and (ANumberingList.AbstractNumberingList.Levels[ACurParagraphLevel].ListLevelProperties.Start <> 1));
  APreviousParagraph := AParagraph.PieceTable.Paragraphs[AParagraph.Index - 1];
  APrevParagraphLevel := APreviousParagraph.GetListLevelIndex;
  ADifferNumberingLists := APreviousParagraph.GetNumberingListIndex <> AParagraph.GetNumberingListIndex;
  if APreviousParagraph.IsInList and (APrevParagraphLevel > ACurParagraphLevel) and ADifferNumberingLists then
    Exit(True);
  Result := (APrevParagraphLevel = ACurParagraphLevel) and ADifferNumberingLists
end;

function TdxHtmlExportHelper.CreateListLevelControl(AParagraph: TdxParagraph; ANumberingList: TdxNumberingList;
  AListControlNestingLevel, AParentLevelOffset, ACounter: Integer): TdxWebControlBase;
var
  ALevelControl: TdxHtmlGenericControl;
  AStyle: string;
  AListLevelIndex: Integer;
  AListLevel: IdxListLevel;
begin
  ALevelControl := TdxHtmlGenericControl.Create(TdxHtmlTextWriterTag.Li);
  FObjectsToDelete.Add(ALevelControl);

  AListLevelIndex := AParagraph.GetListLevelIndex;
  AListLevel := ANumberingList.Levels[AListLevelIndex];
  if IsExportInlineStyle then
    FParagraphStyleHelper.GetHtmlParagraphInListStyle(AParagraph, AListLevel, AListControlNestingLevel, AParentLevelOffset, ALevelControl.Style, FOptions.DefaultCharacterPropertiesExportToCss)
  else
  begin
    AStyle := FParagraphStyleHelper.GetHtmlParagraphInListStyle(AParagraph, AListLevel, AListControlNestingLevel, AParentLevelOffset);
    ALevelControl.CssClass := FScriptContainer.RegisterCssClass(AStyle);
  end;

  if (AListLevel.ListLevelProperties.Start <> 1) and (AListLevel.ListLevelProperties.Start = ACounter) then
    ALevelControl.Attributes.Add('value', IntToStr(AListLevel.ListLevelProperties.Start));
  Result := ALevelControl;
end;

function TdxHtmlExportHelper.GetMostNestedCounter(const ACounters: TIntegerDynArray): Integer;
begin
  Result := ACounters[Length(ACounters) - 1];
end;

function TdxHtmlExportHelper.CreateImageControl(AImage: TdxOfficeImage; const AActualSize: TSize; ACssFloat: TdxHtmlCssFloat): TdxWebControlBase;
var
  AImageControl: TdxWebImageControl;
  AControl: TdxHtmlGenericControl;
begin
  Assert(AImage <> nil, 'image');
  AImageControl := CreateImageControlInternal(AImage, AActualSize, ACssFloat);

  AControl := TdxHtmlGenericControl.Create(TdxHtmlTextWriterTag.Span, nil);
  FObjectsToDelete.Add(AControl);

  if AImageControl.ImageUrl <> '' then
    AControl.Controls.Add(AImageControl);
  Result := AControl;
end;

function TdxHtmlExportHelper.CreateImageControlInternal(AImage: TdxOfficeImage; const AActualSize: TSize; ACssFloat: TdxHtmlCssFloat): TdxWebImageControl;
var
  AImageControl: TdxWebImageControl;
begin
  AImageControl := TdxWebImageControl.Create;
  FObjectsToDelete.Add(AImageControl);

  FImageHelper.ApplyImageProperties(AImageControl, AImage, AActualSize, FImageRepository, DisposeConvertedImagesImmediately,
    True, FOptions.ExportImageSize = TdxExportImageSize.Always, FOptions.KeepExternalImageSize);

  if ACssFloat <> TdxHtmlCssFloat.NotSet then
  begin
    if ACssFloat = TdxHtmlCssFloat.Left then
      AImageControl.Style.Add('float', 'left')
    else
      if ACssFloat = TdxHtmlCssFloat.Right then
        AImageControl.Style.Add('float', 'right');
  end;

  AImageControl.GenerateEmptyAlternateText := True;
  Result := AImageControl;
end;

function TdxHtmlExportHelper.CreateImageControl(ARun: TdxInlinePictureRun): TdxWebControlBase;
begin
  Result := CreateImageControl(ARun.Image.Image, ARun.ActualSize, TdxHtmlCssFloat.NotSet);
end;

end.
