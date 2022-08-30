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

unit dxRichEdit.Import.Html;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface
uses
  Types, SysUtils, Classes, Generics.Defaults, Generics.Collections, dxCore, dxCoreClasses,

  dxGenerics,
  dxRichEdit.Utils.Types,
  dxRichEdit.Utils.DataObject,
  dxRichEdit.Utils.OfficeImage,
  dxRichEdit.Utils.UriStreamService,
  dxRichEdit.NativeApi,
  dxRichEdit.Options.Core,
  dxRichEdit.Options.Simple,
  dxRichEdit.Options,
  dxRichEdit.DocumentModel.History.IndexChangedHistoryItem,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.UnitConverter,
  dxRichEdit.DocumentModel.ParagraphFormatting,
  dxRichEdit.DocumentModel.TableFormatting,
  dxRichEdit.DocumentModel.TabFormatting,
  dxRichEdit.DocumentModel.Fields.Core,
  dxRichEdit.DocumentModel.Borders,
  dxRichEdit.DocumentModel.Bookmarks,
  dxRichEdit.DocumentModel.Tables,
  dxRichEdit.DocumentModel.Hyperlink,
  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.CopyManager,
  dxRichEdit.DocumentFormats.DocumentFormatUtils,
  dxRichEdit.DocumentModel.Commands,
  dxRichEdit.Commands.CopyAndPaste,
  dxRichEdit.Import,
  dxRichEdit.Import.Core,
  dxRichEdit.Import.Formats,
  dxRichEdit.Import.CSSParser,
  dxRichEdit.Import.Html.HtmlParser,
  dxRichEdit.Import.Html.TagBase,
  dxRichEdit.Import.Html.HtmlTags,
  dxRichEdit.Import.Html.DocumentImporter;

type
  TdxHtmlImporter = class;

  { TdxFontSizeInfo }

  TdxFontSizeInfo = record
  strict private
    FDoubleFontSize: Integer;
    FUseFontSize: Boolean;
  public
    constructor Create(ADoubleFontSize: Integer; AUseFontSize: Boolean);

    property DoubleFontSize: Integer read FDoubleFontSize;
    property UseFontSize: Boolean read FUseFontSize;
  end;

  { TdxHtmlImportedTableInfo }

  TdxHtmlImportedTableInfo = class(TdxImportedTableInfo)
  strict private
    FTableCaption: TdxList<TdxTableCell>;
    FCaptionColSpan: Integer;
    procedure SetCaptionColSpan(const AValue: Integer);
  public
    constructor Create(ATable: TdxTable);
    destructor Destroy; override;

    property TableCaption: TdxList<TdxTableCell> read FTableCaption;
    property CaptionColSpan: Integer read FCaptionColSpan write SetCaptionColSpan;
  end;

  { TdxHtmlTablesImportHelper }

  TdxHtmlTablesImportHelper = class(TdxTablesImportHelper)
  strict private
    FImporter: TdxHtmlImporter;
    function GetTableInfo: TdxHtmlImportedTableInfo;
  protected
    function CreateTableInfo(ANewTable: TdxTable): TdxImportedTableInfo; override;
    function CreateCoveredCellWithEmptyParagraph(ARow: TdxTableRow): TdxTableCell; override;
    procedure InsertEmptyParagraph;
  public
    constructor Create(APieceTable: TdxPieceTable; AImporter: TdxHtmlImporter);
    procedure FixBrokenCells(ACurrentTable: TdxTable); override;

    property Importer: TdxHtmlImporter read FImporter;
    property TableInfo: TdxHtmlImportedTableInfo read GetTableInfo;
  end;

  TdxHtmlTranslateKeywordHandler = function (AImporter: TdxHtmlImporter): TdxTagBase;

  TdxHtmlKeywordTranslatorTable = class(TdxEnumeratedDictionary<TdxHtmlTagNameID, TdxHtmlTranslateKeywordHandler>);

  { TdxHtmlImporter }

  TdxHtmlImporter = class(TdxCustomHtmlImporter{, IdxHtmlImporter})
  strict private
    class var
      FHtmlKeywordTable: TdxHtmlKeywordTranslatorTable;
      FSpecialSymbolTable: TdxHtmlSpecialSymbolTable;
    class constructor Initialize;
    class destructor Finalize;
  strict private
    FTagsStack: TdxList<TdxOpenHtmlTag>;
    FHasPreformattedTagInStack: Boolean;
    FPosition: TdxHtmlInputPosition;
    FElement: TdxReferencedObjectContainer<TdxHtmlElement>;
    FTag: TdxTagBase;
    FHtmlFontSize: TdxHtmlFontSize;
    FIsEmptyParagraph: Boolean;
    FIsEmptyLine: Boolean;
    FIsEmptyListItem: Boolean;
    FCanInsertSpace: Boolean;
    FStyleTagIsOpen: Boolean;
    FStyleTagCollection: TdxCssElementCollection;
    FBaseUri: string;
    FEncoding: TEncoding;
    FSuppressEncoding: Boolean;
    FProcessHyperlink: TdxImportFieldInfo;
    FProcessBookmarks: TdxObjectStack<TdxHtmlBookmarkInfo>;
    FEmptyHyperlinks: TdxFieldList;
    FTablesImportHelper: TdxHtmlTablesImportHelper;
    FUsedNumberingLists: TdxSortedList<TdxNumberingListIndex>;
    FParser: TdxHtmlParser;
    FMarkTable: TdxNamedOrdinalDictionary<TdxBookmark>;
    FTagCollection: TdxObjectList<TdxTagBase>;
    procedure SetIsEmptyParagraph(const AValue: Boolean);
    procedure SetBaseUri(const AValue: string);
    function GetCodePage: Integer;
    procedure SetCodePage(const AValue: Integer);
    function GetOptions: TdxHtmlDocumentImporterOptions;
    function GetIgnoredTagIsOpen: Boolean;
    function WhiteSpaceAtStartCanRemove(const AContentText: string): Boolean;
  private
    FLastOpenParagraphTagIndex: Integer;
    FLastOpenAnchorTagIndex: Integer;
    FAbsoluteBaseUri: string;
    FRootDoubleFontSize: Integer;
    procedure SetProcessHyperlink(const Value: TdxImportFieldInfo);
    function GetElement: TdxHtmlElement; inline;
    procedure SetElement(const Value: TdxHtmlElement);
    function GetDefaultEncoding(AStream: TStream): TEncoding;
  protected
    FUniqueUriBasedImages: TdxNamedObjectDictionary<TdxUriOfficeImage>;
    class function CreateHtmlKeywordTable: TdxHtmlKeywordTranslatorTable; static;
    class function AbbrKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase; static;
    class function AcronymKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase; static;
    class function AddressKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase; static;
    class function AreaKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase; static;
    class function BaseFontKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase; static;
    class function BdoKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase; static;
    class function BgsoundKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase; static;
    class function ButtonKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase; static;
    class function CiteKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase; static;
    class function DdKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase; static;
    class function DelKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase; static;
    class function DfnKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase; static;
    class function DlKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase; static;
    class function DtKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase; static;
    class function EmbedKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase; static;
    class function FieldsetKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase; static;
    class function FormKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase; static;
    class function FrameKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase; static;
    class function FrameSetKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase; static;
    class function HrKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase; static;
    class function IframeKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase; static;
    class function InputKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase; static;
    class function InsKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase; static;
    class function KbdKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase; static;
    class function LabelKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase; static;
    class function LegendKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase; static;
    class function MapKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase; static;
    class function MarqueeKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase; static;
    class function NobrKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase; static;
    class function NoembedKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase; static;
    class function NoFramesKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase; static;
    class function NoScriptKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase; static;
    class function ObjectKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase; static;
    class function OptGroupKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase; static;
    class function OptionKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase; static;
    class function ParamKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase; static;
    class function QKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase; static;
    class function SampKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase; static;
    class function SelectKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase; static;
    class function TableKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase; static;
    class function TdKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase; static;
    class function TextAreaKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase; static;
    class function ThKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase; static;
    class function TFootKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase; static;
    class function CaptionKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase; static;
    class function TBodyKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase; static;
    class function THeadKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase; static;
    class function TrKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase; static;
    class function TtKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase; static;
    class function VarKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase; static;
    class function WbrKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase; static;
    class function XmpKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase; static;
    class function HtmlKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase; static;
    class function HeadKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase; static;
    class function BaseKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase; static;
    class function MetaKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase; static;
    class function BodyKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase; static;
    class function TitleKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase; static;
    class function LinkKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase; static;
    class function AnchorKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase; static;
    class function BoldKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase; static;
    class function ItalicKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase; static;
    class function UnderlineKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase; static;
    class function ParagraphKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase; static;
    class function StrongKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase; static;
    class function BigKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase; static;
    class function SmallKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase; static;
    class function PreformattedKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase; static;
    class function FontKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase; static;
    class function LineBreakKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase; static;
    class function EmphasizedKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase; static;
    class function ImageKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase; static;
    class function SubScriptKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase; static;
    class function SuperScriptKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase; static;
    class function CenterKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase; static;
    class function StrikeoutKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase; static;
    class function Heading1KeywordTag(AImporter: TdxHtmlImporter): TdxTagBase; static;
    class function Heading2KeywordTag(AImporter: TdxHtmlImporter): TdxTagBase; static;
    class function Heading3KeywordTag(AImporter: TdxHtmlImporter): TdxTagBase; static;
    class function Heading4KeywordTag(AImporter: TdxHtmlImporter): TdxTagBase; static;
    class function Heading5KeywordTag(AImporter: TdxHtmlImporter): TdxTagBase; static;
    class function Heading6KeywordTag(AImporter: TdxHtmlImporter): TdxTagBase; static;
    class function NumberingListKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase; static;
    class function BulletListKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase; static;
    class function LevelKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase; static;
    class function CodeKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase; static;
    class function SpanKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase; static;
    class function DivisionKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase; static;
    class function ScriptKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase; static;
    class function BlockquoteKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase; static;

    function GetHtmlKeywordTable: TdxHtmlKeywordTranslatorTable; virtual;
    function GetSpecialSymbolTable: TdxHtmlSpecialSymbolTable; virtual;
    procedure SortSelectors; virtual;
    procedure ParseHtmlContent(AHtmlElements: TdxHtmlElementList; AStreamReader: TStreamReader); virtual;
    procedure FixBordersAndParagraphBetweenTables(APieceTable: TdxPieceTable); virtual;
    procedure FixBordersAndParagraphBetweenTablesCore(ATables: TdxList<TdxTable>); virtual;
    procedure ResolveBorderConflicts(ATable: TdxTable); virtual;
    function ShouldResetTableCellBorder(AUseTableBorder: Boolean; AUseCellBorder: Boolean;
      ATableBorder: TdxBorderBase; ACellBorder: TdxBorderBase): Boolean; virtual;
    procedure InsertParagraphBeforeTable(ATable: TdxTable);
    procedure ShiftBookmarks(ALogPosition: TdxDocumentLogPosition);
    class function TableComparer(const ALeft, ARight: TdxTable): Integer; static;
    function GetTablesByLevel(APieceTable: TdxPieceTable; ANestedLevel: Integer): TdxList<TdxTable>; virtual;
    procedure ProcessRemainTags; virtual;
    procedure ProcessUnclosedTableTags(AIndex: Integer);
    procedure ProcessAnchorTag(AHtmlTag: TdxTagBase);
    procedure ClearTagStack;
    function AddTagToStack(ATag: TdxOpenHtmlTag): Boolean;
    procedure RemoveTagFromStack(AIndex: Integer);
    procedure StyleTagIsNotOpen(AStreamReader: TStreamReader; AHtmlElements: TdxHtmlElementList);
    function StyleTagIsOpen(const ARawText: string): string;
    procedure ElementsImport(AHtmlElements: TdxHtmlElementList); virtual;
    procedure ElementsImportCore; virtual;
    procedure FixLastParagraph; virtual;
    procedure FindKeywordInTagTable; virtual;
    procedure ProcessContentText; virtual;
    function GetContentText: string; virtual;
    function UseRawContent: Boolean; virtual;
    function ReplaceSpecialSymbols(const AContentText: string): string;
    function ReplaceSpecialSymbolCore(const ARawText: string): string;
    function GetSpecialValue(const ASpecialSymbol: string; AIsNumeric: Boolean): Char; virtual;
    function GetUnicodeSymbol(const ASpecialSymbol: string): Char;
    function TryParseSymbol(const ASpecialSymbol: string; out ACode: Integer): Boolean;
    function IsSeparator(ACh: Char): Boolean;
    procedure ProcessTag(ATag: TdxTagBase); virtual;
    procedure DeleteOpenTagFromStack(AIndex: Integer);
    procedure ApplyProperties(AIndex: Integer); virtual;
    procedure EmptyProcess(ATag: TdxTagBase); virtual;
    procedure ApplyPreviousCharacterProperties(AIndex: TdxRunIndex); virtual;
    function CreateUriBasedRichEditImageCore(const AUri: string; APixelTargetWidth: Integer;
      APixelTargetHeight: Integer): TdxUriOfficeImage;
    function ApplyPositionScriptFontSize: TdxFontSizeInfo; virtual;
    procedure RestorePositionScriptFontSize(const AFontSizeInfo: TdxFontSizeInfo); virtual;
    function CreateHtmlParser: TdxHtmlParser; virtual;
    procedure ValidateHyperlinkInfo(AHyperlinkInfo: TdxHyperlinkInfo); virtual;
    function ValidateBookmarkName(const AAnchorName: string): string; virtual;
    procedure CreateBookmark(ABookmarkInfo: TdxHtmlBookmarkInfo);
    procedure SetDefaultDocumentProperties; virtual;

    property SuppressEncoding: Boolean read FSuppressEncoding;
    property IgnoredTagIsOpen: Boolean read GetIgnoredTagIsOpen;
  public
    constructor Create(ADocumentModel: TdxDocumentModel; AOptions: TdxHtmlDocumentImporterOptions); reintroduce;
    destructor Destroy; override;

    class function CreateHtmlSpecialSymbolTable: TdxHtmlSpecialSymbolTable; static;
    procedure AppendInlineImage(AImage: TdxOfficeImageReference; AScaleX, AScaleY: Single;
      const ADesiredSize: TSize); virtual;
    procedure AppendParagraph; virtual;
    procedure AppendPlainText(const AText: string); virtual;
    procedure AppendText(const AText: string); overload; virtual;
    procedure AppendText(APieceTable: TdxPieceTable; const AText: string); overload; virtual;
    function CreateUriBasedRichEditImage(const AUri: string; APixelTargetWidth: Integer;
      APixelTargetHeight: Integer): TdxUriOfficeImage;
    procedure CloseUnClosedTag(ATag: TdxTagBase; AIndex: Integer); virtual;
    function DecodeStringContent(const AContentText: string): string;
    function GetAbsoluteBaseUri(const ABaseUri: string): string; virtual;
    function GetMarkPosition(const AMark: string): TdxDocumentLogPosition;
    function GetTagNameID(const AName: string): TdxHtmlTagNameID; virtual;
    procedure ImportCore(AStream: TStream; APos: TdxHtmlInputPosition); reintroduce;
    procedure Import(AStream: TStream); overload; override;
    procedure Import(AStream: TStream; ALeaveOpen: Boolean); overload;
    function OpenTagIsFoundAndRemoved(ATag: TdxTagBase): Boolean;
    procedure ParseCssElementCollection(const ARawText: string); overload; virtual;
    procedure ParseCssElementCollection(AReader: TTextReader); overload;
    procedure RemoveEmptyHyperlinks;
    procedure RegisterCommentMarksToCollectPositions(const AMarks: array of string);
    procedure SetAppendObjectProperty;

    procedure CloseProcess(ATag: TdxTagBase); virtual;
    procedure OpenProcess(ATag: TdxTagBase); virtual;
    procedure ProcessHyperlinkStart(AHyperlinkInfo: TdxHyperlinkInfo); virtual;
    procedure ProcessHyperlinkEnd; virtual;
    procedure ProcessBookmarkStart(const AAnchorName: string); virtual;
    procedure ProcessBookmarkEnd; virtual;

    class procedure ThrowInvalidFile; override;

    property HtmlKeywordTable: TdxHtmlKeywordTranslatorTable read GetHtmlKeywordTable;
    property SpecialSymbolTable: TdxHtmlSpecialSymbolTable read GetSpecialSymbolTable;
    property TagsStack: TdxList<TdxOpenHtmlTag> read FTagsStack;
    property Position: TdxHtmlInputPosition read FPosition;
    property Element: TdxHtmlElement read GetElement write SetElement;
    property IsEmptyParagraph: Boolean read FIsEmptyParagraph write SetIsEmptyParagraph;
    property IsEmptyLine: Boolean read FIsEmptyLine write FIsEmptyLine;
    property IsEmptyListItem: Boolean read FIsEmptyListItem write FIsEmptyListItem;
    property StyleTagCollection: TdxCssElementCollection read FStyleTagCollection;
    property HtmlFontSize: TdxHtmlFontSize read FHtmlFontSize;
    property RootDoubleFontSize: Integer read FRootDoubleFontSize write FRootDoubleFontSize;
    property BaseUri: string read FBaseUri write SetBaseUri;
    property CodePage: Integer read GetCodePage write SetCodePage;
    property Encoding: TEncoding read FEncoding write FEncoding;
    property Options: TdxHtmlDocumentImporterOptions read GetOptions;
    property LastOpenParagraphTagIndex: Integer read FLastOpenParagraphTagIndex write FLastOpenParagraphTagIndex;
    property LastOpenAnchorTagIndex: Integer read FLastOpenAnchorTagIndex write FLastOpenAnchorTagIndex;
    property ProcessHyperlink: TdxImportFieldInfo read FProcessHyperlink write SetProcessHyperlink;
    property ProcessBookmarks: TdxObjectStack<TdxHtmlBookmarkInfo> read FProcessBookmarks;
    property TablesImportHelper: TdxHtmlTablesImportHelper read FTablesImportHelper;
    property UsedNumberingLists: TdxSortedList<TdxNumberingListIndex> read FUsedNumberingLists;
    property Parser: TdxHtmlParser read FParser;
    property AbsoluteBaseUri: string read FAbsoluteBaseUri write FAbsoluteBaseUri;
  end;

  { TdxImportHtmlFormat }

  TdxImportHtmlFormat = class(TdxImportFileFormat)
  public
    class function GetDocumentFormat: TdxRichEditDocumentFormat; override;
    function GetImporter(ADocumentModel: TdxCustomDocumentModel;
      const AOptions: IdxImporterOptions): TdxDocumentModelImporter; override;
    function GetDocumentImporter: IdxImporter<TdxRichEditDocumentFormat, Boolean>; override;
  end;

  { TdxPieceTablePasteHtmlTextCommand }

  TdxPieceTablePasteHtmlTextCommand = class(TdxPieceTablePasteTextContentConvertedToDocumentModelCommandBase)
  protected
    function GetFormat: TdxRichEditDocumentFormat; override;
    function GetContent: TdxClipboardStringContent; override;
    procedure PopulateDocumentModelFromContentStringCore(ADocumentModel: TdxDocumentModel;
      AContent: TdxClipboardStringContent; const ASizeCollection: string); override;
    function PrepareInputStream(const AStr: string): TStream;
  public
    function IsDataAvailable: Boolean; override;
  end;

  { TdxHtmlClipboardStringContent }

  TdxHtmlClipboardStringContent = class(TdxClipboardStringContent)
  strict private
    FSourceUri: string;
    FStartFragment: Integer;
    FEndFragment: Integer;
  public
    constructor Create(const ASourseUri, AContent: string; AFragmentStart, AFragmentEnd: Integer);

    property SourceUri: string read FSourceUri write FSourceUri;
    property StartFragment: Integer read FStartFragment write FStartFragment;
    property EndFragment: Integer read FEndFragment write FEndFragment;
  end;

  { TdxHtmlClipboardData }

  TdxHtmlClipboardData = class
  strict private
    FHtmlContent: string;
    FSourceUri: string;
    FStartFragment: Integer;
    FEndFragment: Integer;
  protected
    procedure Parse(const AContent: TArray<Byte>); virtual;
    function CalculateFragmentStart(ADescription: TdxStringsDictionary): Integer;
    function CalculateFragmentEnd(ADescription: TdxStringsDictionary): Integer;
    function CreateContentDescription(const AContent: TArray<Byte>): TdxStringsDictionary;
    function CalculateHtmlContentOffset(ADescription: TdxStringsDictionary): Integer; virtual;
    function GetIntegerValue(ADescription: TdxStringsDictionary; const AKey: string): Integer;
    function CalculateSourceUri(ADescription: TdxStringsDictionary): string; virtual;
  public
    constructor Create(const AContent: TArray<Byte>);

    property HtmlContent: string read FHtmlContent;
    property SourceUri: string read FSourceUri;
    property StartFragment: Integer read FStartFragment;
    property EndFragment: Integer read FEndFragment;
  end;

  { TdxPieceTablePasteHtmlClipboardDataCommand }

  TdxPieceTablePasteHtmlClipboardDataCommand = class(TdxPieceTablePasteHtmlTextCommand)
  strict protected const
    StartFragmentCommentText = 'StartFragment';
    EndFragmentCommentText = 'EndFragment';
    StartFragmentComment = '<!--StartFragment-->';
    EndFragmentComment = '<!--EndFragment-->';
  protected
    function GetContent: TdxClipboardStringContent; override;
    procedure PopulateDocumentModelFromContentStringCore(ADocumentModel: TdxDocumentModel;
      AContent: TdxClipboardStringContent; const ASizeCollection: string); override;
    procedure CopyFragment(ASourceModel, ATargetModel: TdxDocumentModel; AStartFragmentPos, AEndFragmentPos: TdxDocumentLogPosition);
    function ClearHTMLContent(const AHtmlContent: string; AStartFragent, AEndFragment: Integer): string;
    function RemoveFragmentMarks(const AContent: string): string;
    function SuppressCopySectionProperties(ASource: TdxDocumentModel): Boolean; override;
  public
    function IsDataAvailable: Boolean; override;
  end;

  { TdxPasteHtmlTextCommand }

  TdxPasteHtmlTextCommand = class(TdxPasteContentConvertedToDocumentModelCommandBase)
  protected
    function CreateInnerCommandCore: TdxPieceTablePasteContentConvertedToDocumentModelCommandBase; override;
  public
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

var
  CF_HTML: Word;

implementation

uses
  Windows, Contnrs, Math, Character, RTLConsts, IOUtils, Clipbrd,

  dxRichEdit.DocumentModel.CharacterFormatting,
  dxRichEdit.Import.Html.CSSParser,
  dxRichEdit.Strs,
  dxRichEdit.Commands.Strs,
  dxRichEdit.DocumentModel.Section,
  dxEncoding,
  dxRichEdit.Utils.Exceptions,
  dxUriRecord,
  dxCharacters,
  dxStringHelper,
  dxRichEdit.Utils.NumberParser,
  dxRichEdit.Import.Html.TextTags,
  dxRichEdit.Import.Html.FrameTags,
  dxRichEdit.Import.Html.TableTags,
  dxRichEdit.Import.Html.DocumentTags,
  dxRichEdit.Import.Html.ObjectTags,
  dxRichEdit.Import.Html.FormTags,
  dxRichEdit.Import.Html.FormattingTags,
  dxRichEdit.Import.Html.NumberingListTags,
  dxRichEdit.Import.Html.ImageTags;

type
  TdxHtmlCorrector = class;

  { TdxHtmlCodePageDecoder }

  TdxHtmlCodePageDecoder = class
  public
    class function ApplyEncoding(const AContent: string; AEncoding: TEncoding): string; static;
  end;

  { TdxHtmlCorrectorStateBase }

  TdxHtmlCorrectorStateBase = class abstract
  strict private
    FCorrector: TdxHtmlCorrector;
    FOutput: TdxHtmlElementList;
    FCloseTag: TdxReferencedObjectContainer<TdxTag>;
  protected
    procedure ProcessTableOpenTag(AElement: TdxHtmlElement); virtual; abstract;
    procedure ProcessTableCloseTag(AElement: TdxHtmlElement); virtual; abstract;
    procedure ProcessTableRowOpenTag(AElement: TdxHtmlElement); virtual; abstract;
    procedure ProcessTableRowCloseTag(AElement: TdxHtmlElement); virtual; abstract;
    procedure ProcessTableCellOpenTag(AElement: TdxHtmlElement); virtual; abstract;
    procedure ProcessTableCellCloseTag(AElement: TdxHtmlElement); virtual; abstract;
    procedure ProcessNonTableElement(AElement: TdxHtmlElement); virtual; abstract;
    procedure ProcessCaptionOpenTag(AElement: TdxHtmlElement); virtual; abstract;
    procedure ProcessCaptionCloseTag(AElement: TdxHtmlElement); virtual; abstract;
    procedure ProcessTableHeaderCellOpenTag(AElement: TdxHtmlElement); virtual;
    procedure ProcessTableHeaderCellCloseTag(AElement: TdxHtmlElement); virtual;
    procedure ProcessTheadOpenTag(AElement: TdxHtmlElement); virtual;
    procedure ProcessTheadCloseTag(AElement: TdxHtmlElement); virtual;
    procedure ProcessTfootOpenTag(AElement: TdxHtmlElement); virtual;
    procedure ProcessTfootCloseTag(AElement: TdxHtmlElement); virtual;
    procedure ProcessTbodyOpenTag(AElement: TdxHtmlElement); virtual;
    procedure ProcessTbodyCloseTag(AElement: TdxHtmlElement); virtual;
    procedure ProcessColOpenTag(AElement: TdxHtmlElement); virtual;
    procedure ProcessColCloseTag(AElement: TdxHtmlElement); virtual;
    procedure ProcessColgroupOpenTag(AElement: TdxHtmlElement); virtual;
    procedure ProcessColgroupCloseTag(AElement: TdxHtmlElement); virtual;
    procedure ProcessWhiteSpaceElement(AElement: TdxHtmlElement); virtual;
    function ProcessTableTag(ATag: TdxTag): Boolean; virtual;
    function GetOpenTag(ATag: TdxTag): TdxHtmlElement;
    function GetCloseTag(ATag: TdxTag): TdxHtmlElement;
  public
    constructor Create(ACorrector: TdxHtmlCorrector; AOutput: TdxHtmlElementList);
    destructor Destroy; override;

    procedure ProcessEnd; virtual; abstract;
    procedure Process(AHtmlElement: TdxHtmlElement); virtual;
    class function IsWhiteSpaceElement(AElement: TdxHtmlElement): Boolean; static;

    property Corrector: TdxHtmlCorrector read FCorrector;
    property CurrentOutput: TdxHtmlElementList read FOutput;
  end;

  { TdxHtmlCorrectorDefaultState }

  TdxHtmlCorrectorDefaultState = class(TdxHtmlCorrectorStateBase)
  protected
    procedure ProcessTableOpenTag(AElement: TdxHtmlElement); override;
    procedure ProcessTableCloseTag(AElement: TdxHtmlElement); override;
    procedure ProcessNonTableElement(AElement: TdxHtmlElement); override;
    procedure ProcessTableRowOpenTag(AElement: TdxHtmlElement); override;
    procedure ProcessTableRowCloseTag(AElement: TdxHtmlElement); override;
    procedure ProcessTableCellOpenTag(AElement: TdxHtmlElement); override;
    procedure ProcessTableCellCloseTag(AElement: TdxHtmlElement); override;
    procedure ProcessCaptionOpenTag(AElement: TdxHtmlElement); override;
    procedure ProcessCaptionCloseTag(AElement: TdxHtmlElement); override;
  public
    procedure ProcessEnd; override;
  end;

  { TdxHtmlCorrectorTableStateBase }

  TdxHtmlCorrectorTableStateBase = class abstract(TdxHtmlCorrectorStateBase)
  strict private
    FDeferredCellContent: TdxHtmlElementList;
    FInnerOutput: TdxHtmlElementList;
    FLastCellOutputPosition: Integer;
    FHasRows: Boolean;
  protected
    procedure EnsureTableHasRows; virtual;
    procedure ProcessTableOpenTag(AElement: TdxHtmlElement); override;
    procedure ProcessNonTableElement(AElement: TdxHtmlElement); override;
    procedure AddInnerElementsToCurrentOutput; virtual;
    procedure ProcessTableRowOpenTag(AElement: TdxHtmlElement); override;
    procedure ProcessTableRowCloseTag(AElement: TdxHtmlElement); override;
    procedure ProcessTableCellOpenTag(AElement: TdxHtmlElement); override;
    procedure ProcessTableCellCloseTag(AElement: TdxHtmlElement); override;

    property HasRows: Boolean read FHasRows write FHasRows;
    property InnerOutput: TdxHtmlElementList read FInnerOutput;
  public
    constructor Create(ACorrector: TdxHtmlCorrector; AOutput: TdxHtmlElementList);
    destructor Destroy; override;
    procedure AddMisplacedCellContent(AElement: TdxHtmlElement); overload;
    procedure AddMisplacedCellContent(AElements: TdxHtmlElementList); overload;
    procedure OnStartCell(AElements: TdxHtmlElementList);
    procedure OnEndCell(AOutputPosition: Integer);
    procedure ProcessEnd; override;
  end;

  { TdxHtmlCorrectorTableState }

  TdxHtmlCorrectorTableState = class(TdxHtmlCorrectorTableStateBase)
  strict private
    FHeaderOutput: TdxHtmlElementList;
    FFooterOutput: TdxHtmlElementList;
    FCaptionOutput: TdxHtmlElementList;
  protected
    procedure ProcessTableCloseTag(AElement: TdxHtmlElement); override;
    procedure ProcessCaptionOpenTag(AElement: TdxHtmlElement); override;
    procedure ProcessCaptionCloseTag(AElement: TdxHtmlElement); override;
    procedure ProcessTbodyOpenTag(AElement: TdxHtmlElement); override;
    procedure ProcessTheadOpenTag(AElement: TdxHtmlElement); override;
    procedure ProcessTfootOpenTag(AElement: TdxHtmlElement); override;
  public
    constructor Create(ACorrector: TdxHtmlCorrector; AOutput: TdxHtmlElementList);
    destructor Destroy; override;
  end;

  { TdxHtmlCorrectorTableRowState }

  TdxHtmlCorrectorTableRowState = class(TdxHtmlCorrectorStateBase)
  strict private
    FFirstRow: Boolean;
    FHasCells: Boolean;
    FOutput: TdxHtmlElementList;
  protected
    procedure ProcessTableOpenTag(AElement: TdxHtmlElement); override;
    procedure ProcessNonTableElement(AElement: TdxHtmlElement); override;
    procedure ProcessTableCloseTag(AElement: TdxHtmlElement); override;
    procedure ProcessTableRowOpenTag(AElement: TdxHtmlElement); override;
    procedure ProcessTableRowCloseTag(AElement: TdxHtmlElement); override;
    procedure ProcessTableCellOpenTag(AElement: TdxHtmlElement); override;
    procedure ProcessTableCellCloseTag(AElement: TdxHtmlElement); override;
    procedure ProcessCaptionOpenTag(AElement: TdxHtmlElement); override;
    procedure ProcessCaptionCloseTag(AElement: TdxHtmlElement); override;
  public
    constructor Create(ACorrector: TdxHtmlCorrector; AOutput: TdxHtmlElementList; AFirstRow: Boolean);
    destructor Destroy; override;
    procedure ProcessEnd; override;
  end;

  { TdxHtmlCorrectorTableCellState }

  TdxHtmlCorrectorTableCellState = class(TdxHtmlCorrectorStateBase)
  protected
    procedure ProcessNonTableElement(AElement: TdxHtmlElement); override;
    procedure ProcessTableOpenTag(AElement: TdxHtmlElement); override;
    procedure ProcessTableCloseTag(AElement: TdxHtmlElement); override;
    procedure ProcessTableRowOpenTag(AElement: TdxHtmlElement); override;
    procedure ProcessTableRowCloseTag(AElement: TdxHtmlElement); override;
    procedure ProcessTableCellOpenTag(AElement: TdxHtmlElement); override;
    procedure ProcessTableCellCloseTag(AElement: TdxHtmlElement); override;
    procedure ProcessCaptionOpenTag(AElement: TdxHtmlElement); override;
    procedure ProcessCaptionCloseTag(AElement: TdxHtmlElement); override;
  public
    procedure ProcessEnd; override;
  end;

  { TdxHtmlCorrectorTableCaptionState }

  TdxHtmlCorrectorTableCaptionState = class(TdxHtmlCorrectorStateBase)
  strict private
    FEmpty: Boolean;
    FParentOutput: TdxHtmlElementList;
  protected
    procedure ProcessNonTableElement(AElement: TdxHtmlElement); override;
    procedure ProcessTableOpenTag(AElement: TdxHtmlElement); override;
    procedure ProcessTableCloseTag(AElement: TdxHtmlElement); override;
    procedure ProcessTableRowOpenTag(AElement: TdxHtmlElement); override;
    procedure ProcessTableRowCloseTag(AElement: TdxHtmlElement); override;
    procedure ProcessTableCellOpenTag(AElement: TdxHtmlElement); override;
    procedure ProcessTableCellCloseTag(AElement: TdxHtmlElement); override;
    procedure ProcessCaptionOpenTag(AElement: TdxHtmlElement); override;
    procedure ProcessCaptionCloseTag(AElement: TdxHtmlElement); override;
  public
    constructor Create(ACorrector: TdxHtmlCorrector; AOutput: TdxHtmlElementList);
    destructor Destroy; override;
    procedure ProcessEnd; override;
  end;

  { TdxHtmlCorrectorTableBodyState }

  TdxHtmlCorrectorTableBodyState = class(TdxHtmlCorrectorTableStateBase)
  protected
    procedure ProcessTbodyCloseTag(AElement: TdxHtmlElement); override;
    procedure ProcessTbodyOpenTag(AElement: TdxHtmlElement); override;
    procedure ProcessTableCloseTag(AElement: TdxHtmlElement); override;
    procedure ProcessTfootOpenTag(AElement: TdxHtmlElement); override;
    procedure ProcessTheadOpenTag(AElement: TdxHtmlElement); override;
    procedure ProcessCaptionOpenTag(AElement: TdxHtmlElement); override;
    procedure ProcessCaptionCloseTag(AElement: TdxHtmlElement); override;
  public
    procedure ProcessEnd; override;
  end;

  { TdxHtmlCorrectorTableHeaderState }

  TdxHtmlCorrectorTableHeaderState = class(TdxHtmlCorrectorTableStateBase)
  protected
    procedure ProcessTheadOpenTag(AElement: TdxHtmlElement); override;
    procedure ProcessTheadCloseTag(AElement: TdxHtmlElement); override;
    procedure ProcessTbodyOpenTag(AElement: TdxHtmlElement); override;
    procedure ProcessTableCloseTag(AElement: TdxHtmlElement); override;
    procedure ProcessCaptionOpenTag(AElement: TdxHtmlElement); override;
    procedure ProcessCaptionCloseTag(AElement: TdxHtmlElement); override;
  public
    procedure ProcessEnd; override;
  end;

  { TdxHtmlCorrectorTableFooterState }

  TdxHtmlCorrectorTableFooterState = class(TdxHtmlCorrectorTableStateBase)
  protected
    procedure ProcessTbodyOpenTag(AElement: TdxHtmlElement); override;
    procedure ProcessTfootOpenTag(AElement: TdxHtmlElement); override;
    procedure ProcessTfootCloseTag(AElement: TdxHtmlElement); override;
    procedure ProcessTableCloseTag(AElement: TdxHtmlElement); override;
    procedure ProcessCaptionOpenTag(AElement: TdxHtmlElement); override;
    procedure ProcessCaptionCloseTag(AElement: TdxHtmlElement); override;
  public
    procedure ProcessEnd; override;
  end;

  { TdxHtmlCorrector }

  TdxHtmlCorrector = class
  strict private
    FStates: TdxList<TdxHtmlCorrectorStateBase>;//TdxObjectList<TdxHtmlCorrectorStateBase>;
    FStatesStorage: TdxObjectList<TdxHtmlCorrectorStateBase>;
    FLastOpenedTableState: TdxHtmlCorrectorTableStateBase;
    function GetState: TdxHtmlCorrectorStateBase;
  protected
    procedure AddState(AState: TdxHtmlCorrectorStateBase);
    procedure Process(AElement: TdxHtmlElement); virtual;
    procedure ForceOpenTag(ANameID: TdxHtmlTagNameID); virtual;
    procedure ForceCloseTag(ANameID: TdxHtmlTagNameID); virtual;
    procedure ForceCloseCaption;

    property State: TdxHtmlCorrectorStateBase read GetState;
  public
    constructor Create;
    destructor Destroy; override;
    class function GetText(AElements: TdxHtmlElementList): string; static;
    function GetCorrectedHtmlElements(AHtmlElements: TdxHtmlElementList; out AHtmlTagPresent: Boolean): TdxHtmlElementList;
    function CalculateHtmlElementBeforeComment(AComment: TdxComment; AList: TdxHtmlElementList; AIndex: Integer): TdxHtmlElement;
    function CalculateHtmlElementAfterComment(AComment: TdxComment; AList: TdxHtmlElementList; AIndex: Integer): TdxHtmlElement;
    procedure InsertStartComment(ANewComment: TdxComment; AOutput: TdxHtmlElementList; ABeforeComment: TdxHtmlElement);
    procedure InsertEndComment(ANewComment: TdxComment; AOutput: TdxHtmlElementList; AAfterComment: TdxHtmlElement);
    procedure StartNewTable(AOutput: TdxHtmlElementList);
    procedure StartNewRow(AOutput: TdxHtmlElementList; AFirstRow: Boolean);
    procedure StartNewCell(AOutput: TdxHtmlElementList);
    procedure StartCaption(AOutput: TdxHtmlElementList);
    procedure StartTbody(AOutput: TdxHtmlElementList);
    procedure StartThead(AOutput: TdxHtmlElementList);
    procedure StartTfoot(AOutput: TdxHtmlElementList);
    procedure ProcessEnd;
    procedure AddMisplacedCellContent(AElements: TdxHtmlElementList); overload;
    procedure AddMisplacedCellContent(AElement: TdxHtmlElement); overload;
    procedure ForceOpenTableCell;
    procedure ForceCloseTableCell;
    procedure ForceOpenTableRow;
    procedure ForceCloseTableRow;
    procedure ForceCloseTable;
    procedure ForceOpenCaption;
    procedure ReturnToPrevStateFromCellState(ALastCellPosition: Integer);
    procedure ReturnToPrevState;
  end;


{ TdxHtmlCodePageDecoder }

class function TdxHtmlCodePageDecoder.ApplyEncoding(const AContent: string; AEncoding: TEncoding): string;
var
  I, ACount: Integer;
  ABytes: TArray<Byte>;
begin
  ACount := Length(AContent);
  SetLength(ABytes, ACount);
  for I := 0 to ACount - 1 do
    ABytes[I] := Byte(Ord(AContent[I + 1]));

  if AEncoding.GetCharCount(ABytes, 0, ACount) > 0 then
    Result := AEncoding.GetString(ABytes, 0, ACount)
  else
  begin
    AEncoding := TdxEncoding.DetectEncoding(ABytes, 0, ACount);
    if (AEncoding <> nil) and (AEncoding.GetCharCount(ABytes, 0, ACount) > 0) then
      Result := AEncoding.GetString(ABytes, 0, ACount)
    else
      Result := AContent;
  end;
end;

{ TdxHtmlCorrectorStateBase }

constructor TdxHtmlCorrectorStateBase.Create(ACorrector: TdxHtmlCorrector; AOutput: TdxHtmlElementList);
begin
  FCorrector := ACorrector;
  FOutput := AOutput;
end;

procedure TdxHtmlCorrectorStateBase.ProcessTableHeaderCellOpenTag(AElement: TdxHtmlElement);
begin
  ProcessTableCellOpenTag(AElement);
end;

procedure TdxHtmlCorrectorStateBase.ProcessTableHeaderCellCloseTag(AElement: TdxHtmlElement);
begin
  ProcessTableCellCloseTag(AElement);
end;

procedure TdxHtmlCorrectorStateBase.ProcessTheadOpenTag(AElement: TdxHtmlElement);
begin
end;

procedure TdxHtmlCorrectorStateBase.ProcessTheadCloseTag(AElement: TdxHtmlElement);
begin
end;

procedure TdxHtmlCorrectorStateBase.ProcessTfootOpenTag(AElement: TdxHtmlElement);
begin
end;

procedure TdxHtmlCorrectorStateBase.ProcessTfootCloseTag(AElement: TdxHtmlElement);
begin
end;

procedure TdxHtmlCorrectorStateBase.ProcessTbodyOpenTag(AElement: TdxHtmlElement);
begin
end;

procedure TdxHtmlCorrectorStateBase.ProcessTbodyCloseTag(AElement: TdxHtmlElement);
begin
end;

procedure TdxHtmlCorrectorStateBase.ProcessColOpenTag(AElement: TdxHtmlElement);
begin
end;

procedure TdxHtmlCorrectorStateBase.ProcessColCloseTag(AElement: TdxHtmlElement);
begin
end;

procedure TdxHtmlCorrectorStateBase.ProcessColgroupOpenTag(AElement: TdxHtmlElement);
begin
end;

procedure TdxHtmlCorrectorStateBase.ProcessColgroupCloseTag(AElement: TdxHtmlElement);
begin
end;

procedure TdxHtmlCorrectorStateBase.ProcessWhiteSpaceElement(AElement: TdxHtmlElement);
begin
  CurrentOutput.Add(AElement);
end;

procedure TdxHtmlCorrectorStateBase.Process(AHtmlElement: TdxHtmlElement);
var
  ATag: TdxTag;
begin
  ATag := Safe<TdxTag>.Cast(AHtmlElement);
  if (ATag <> nil) and ProcessTableTag(ATag) then
    Exit
  else
    if IsWhiteSpaceElement(AHtmlElement) then
      ProcessWhiteSpaceElement(AHtmlElement)
    else
      ProcessNonTableElement(AHtmlElement);
end;

function TdxHtmlCorrectorStateBase.ProcessTableTag(ATag: TdxTag): Boolean;
var
  AElementType: TdxHtmlElementType;
  AIsEmptyTag, AIsOpenTag, AIsCloseTag: Boolean;
begin
  AElementType := ATag.ElementType;
  AIsEmptyTag := AElementType = TdxHtmlElementType.EmptyTag;
  AIsOpenTag := (AElementType = TdxHtmlElementType.OpenTag) or AIsEmptyTag;
  AIsCloseTag := (AElementType = TdxHtmlElementType.CloseTag) or AIsEmptyTag;
  if ATag.NameID = TdxHtmlTagNameID.Table then
  begin
    if AIsOpenTag then
      ProcessTableOpenTag(GetOpenTag(ATag));
    if AIsCloseTag then
      ProcessTableCloseTag(GetCloseTag(ATag));
    Exit(True);
  end;
  if ATag.NameID = TdxHtmlTagNameID.TR then
  begin
    if AIsOpenTag then
      ProcessTableRowOpenTag(GetOpenTag(ATag));
    if AIsCloseTag then
      ProcessTableRowCloseTag(GetCloseTag(ATag));
    Exit(True);
  end;
  if ATag.NameID = TdxHtmlTagNameID.TD then
  begin
    if AIsOpenTag then
      ProcessTableCellOpenTag(GetOpenTag(ATag));
    if AIsCloseTag then
      ProcessTableCellCloseTag(GetCloseTag(ATag));
    Exit(True);
  end;
  if ATag.NameID = TdxHtmlTagNameID.TH then
  begin
    if AIsOpenTag then
      ProcessTableHeaderCellOpenTag(GetOpenTag(ATag));
    if AIsCloseTag then
      ProcessTableHeaderCellCloseTag(GetCloseTag(ATag));
    Exit(True);
  end;
  if ATag.NameID = TdxHtmlTagNameID.Caption then
  begin
    if AIsOpenTag then
      ProcessCaptionOpenTag(GetOpenTag(ATag));
    if AIsCloseTag then
      ProcessCaptionCloseTag(GetCloseTag(ATag));
    Exit(True);
  end;
  if ATag.NameID = TdxHtmlTagNameID.Thead then
  begin
    if AIsOpenTag then
      ProcessTheadOpenTag(GetOpenTag(ATag));
    if AIsCloseTag then
      ProcessTheadCloseTag(GetCloseTag(ATag));
    Exit(True);
  end;
  if ATag.NameID = TdxHtmlTagNameID.Tfoot then
  begin
    if AIsOpenTag then
      ProcessTfootOpenTag(GetOpenTag(ATag));
    if AIsCloseTag then
      ProcessTfootCloseTag(GetCloseTag(ATag));
    Exit(True);
  end;
  if ATag.NameID = TdxHtmlTagNameID.Tbody then
  begin
    if AIsOpenTag then
      ProcessTbodyOpenTag(GetOpenTag(ATag));
    if AIsCloseTag then
      ProcessTbodyCloseTag(GetCloseTag(ATag));
    Exit(True);
  end;
  if ATag.NameID = TdxHtmlTagNameID.Col then
  begin
    if AIsOpenTag then
      ProcessColOpenTag(GetOpenTag(ATag));
    if AIsCloseTag then
      ProcessColCloseTag(GetCloseTag(ATag));
    Exit(True);
  end;
  if ATag.NameID = TdxHtmlTagNameID.ColGroup then
  begin
    if AIsOpenTag then
      ProcessColgroupOpenTag(GetOpenTag(ATag));
    if AIsCloseTag then
      ProcessColgroupCloseTag(GetCloseTag(ATag));
    Exit(True);
  end;

  Result := False;
end;

function TdxHtmlCorrectorStateBase.GetOpenTag(ATag: TdxTag): TdxHtmlElement;
begin
  if ATag.ElementType = TdxHtmlElementType.EmptyTag then
  begin
    Result := TdxTag.Create(TdxHtmlElementType.OpenTag);
    TdxTag(Result).CopyFrom(ATag);
  end
  else
    Result := ATag;
end;

destructor TdxHtmlCorrectorStateBase.Destroy;
begin
  FCloseTag.Value := nil;
  inherited Destroy;
end;

function TdxHtmlCorrectorStateBase.GetCloseTag(ATag: TdxTag): TdxHtmlElement;
begin
  if ATag.ElementType = TdxHtmlElementType.EmptyTag then
  begin
    Result := TdxTag.Create(TdxHtmlElementType.CloseTag);
    TdxTag(Result).CopyFrom(ATag);
    FCloseTag.Value := TdxTag(Result);
  end
  else
    Result := ATag;
end;

class function TdxHtmlCorrectorStateBase.IsWhiteSpaceElement(AElement: TdxHtmlElement): Boolean;
var
  ARawText: string;
  I: Integer;
begin
  if AElement.ElementType = TdxHtmlElementType.Comment then
    Exit(True);
  if AElement.ElementType <> TdxHtmlElementType.Content then
    Exit(False);
  ARawText := AElement.RawText;
  for I := 1 to Length(ARawText) do
   if {$IFDEF DELPHIXE4}not ARawText[I].IsWhiteSpace{$ELSE}not TCharacter.IsWhiteSpace(ARawText, I){$ENDIF} then
      Exit(False);
  Result := True;
end;

{ TdxHtmlCorrectorDefaultState }

procedure TdxHtmlCorrectorDefaultState.ProcessTableOpenTag(AElement: TdxHtmlElement);
begin
  CurrentOutput.Add(AElement);
  Corrector.StartNewTable(CurrentOutput);
end;

procedure TdxHtmlCorrectorDefaultState.ProcessTableCloseTag(AElement: TdxHtmlElement);
begin
end;

procedure TdxHtmlCorrectorDefaultState.ProcessNonTableElement(AElement: TdxHtmlElement);
begin
  CurrentOutput.Add(AElement);
end;

procedure TdxHtmlCorrectorDefaultState.ProcessTableRowOpenTag(AElement: TdxHtmlElement);
begin
end;

procedure TdxHtmlCorrectorDefaultState.ProcessTableRowCloseTag(AElement: TdxHtmlElement);
begin
end;

procedure TdxHtmlCorrectorDefaultState.ProcessTableCellOpenTag(AElement: TdxHtmlElement);
begin
end;

procedure TdxHtmlCorrectorDefaultState.ProcessTableCellCloseTag(AElement: TdxHtmlElement);
begin
end;

procedure TdxHtmlCorrectorDefaultState.ProcessCaptionOpenTag(AElement: TdxHtmlElement);
begin
end;

procedure TdxHtmlCorrectorDefaultState.ProcessCaptionCloseTag(AElement: TdxHtmlElement);
begin
end;

procedure TdxHtmlCorrectorDefaultState.ProcessEnd;
begin
end;

{ TdxHtmlCorrectorTableStateBase }

constructor TdxHtmlCorrectorTableStateBase.Create(ACorrector: TdxHtmlCorrector; AOutput: TdxHtmlElementList);
begin
  inherited Create(ACorrector, AOutput);
  FLastCellOutputPosition := -1;
  FInnerOutput := TdxHtmlElementList.Create;
end;

destructor TdxHtmlCorrectorTableStateBase.Destroy;
begin
  FDeferredCellContent.Free;
  FInnerOutput.Free;
  inherited Destroy;
end;

procedure TdxHtmlCorrectorTableStateBase.EnsureTableHasRows;
begin
  if not HasRows then
  begin
    Corrector.ForceOpenTableRow;
    Corrector.ForceCloseTableRow;
  end;
end;

procedure TdxHtmlCorrectorTableStateBase.ProcessTableOpenTag(AElement: TdxHtmlElement);
begin
  Corrector.ForceCloseTable;

  CurrentOutput.Add(AElement);
  Corrector.StartNewTable(CurrentOutput);
end;

procedure TdxHtmlCorrectorTableStateBase.ProcessNonTableElement(AElement: TdxHtmlElement);
begin
  AddMisplacedCellContent(AElement);
end;

procedure TdxHtmlCorrectorTableStateBase.AddInnerElementsToCurrentOutput;
begin
  CurrentOutput.AddRange(InnerOutput);
end;

procedure TdxHtmlCorrectorTableStateBase.ProcessTableRowOpenTag(AElement: TdxHtmlElement);
begin
  FInnerOutput.Add(AElement);
  Corrector.StartNewRow(FInnerOutput, not HasRows);
  FHasRows := True;
end;

procedure TdxHtmlCorrectorTableStateBase.ProcessTableRowCloseTag(AElement: TdxHtmlElement);
begin
  if HasRows then
    Exit;
  Corrector.ForceOpenTableRow;
  Corrector.Process(AElement);
end;

procedure TdxHtmlCorrectorTableStateBase.ProcessTableCellOpenTag(AElement: TdxHtmlElement);
begin
  Corrector.ForceOpenTableRow;
  Corrector.Process(AElement);
end;

procedure TdxHtmlCorrectorTableStateBase.ProcessTableCellCloseTag(AElement: TdxHtmlElement);
begin
  if HasRows then
    Exit;
  Corrector.ForceOpenTableRow;
  Corrector.Process(AElement);
end;

procedure TdxHtmlCorrectorTableStateBase.AddMisplacedCellContent(AElement: TdxHtmlElement);
begin
  if FLastCellOutputPosition = -1 then
  begin
    if FDeferredCellContent = nil then
      FDeferredCellContent := TdxHtmlElementList.Create;
    FDeferredCellContent.Add(AElement);
  end
  else
  begin
    FInnerOutput.Insert(FLastCellOutputPosition, AElement);
    Inc(FLastCellOutputPosition);
  end;
end;

procedure TdxHtmlCorrectorTableStateBase.AddMisplacedCellContent(AElements: TdxHtmlElementList);
var
  ACount, I: Integer;
begin
  ACount := AElements.Count;
  for I := 0 to ACount - 1 do
    AddMisplacedCellContent(AElements[I]);
end;

procedure TdxHtmlCorrectorTableStateBase.OnStartCell(AElements: TdxHtmlElementList);
var
  AOutputPosition, I: Integer;
begin
  AOutputPosition := 0;
  if FDeferredCellContent <> nil then
  begin
    for I := 0 to FDeferredCellContent.Count - 1 do
    begin
      AElements.Add(FDeferredCellContent[I]);
      Inc(AOutputPosition);
    end;
    FreeAndNil(FDeferredCellContent);
  end;
  FLastCellOutputPosition := AOutputPosition;
end;

procedure TdxHtmlCorrectorTableStateBase.OnEndCell(AOutputPosition: Integer);
begin
  FLastCellOutputPosition := AOutputPosition;
end;

procedure TdxHtmlCorrectorTableStateBase.ProcessEnd;
begin
  Corrector.ForceCloseTable;
  Corrector.ProcessEnd;
end;

{ TdxHtmlCorrectorTableState }

constructor TdxHtmlCorrectorTableState.Create(ACorrector: TdxHtmlCorrector; AOutput: TdxHtmlElementList);
begin
  inherited Create(ACorrector, AOutput);
  FCaptionOutput := TdxHtmlElementList.Create;
  FHeaderOutput := TdxHtmlElementList.Create;
  FFooterOutput := TdxHtmlElementList.Create;
end;

destructor TdxHtmlCorrectorTableState.Destroy;
begin
  FCaptionOutput.Free;
  FHeaderOutput.Free;
  FFooterOutput.Free;
  inherited Destroy;
end;

procedure TdxHtmlCorrectorTableState.ProcessTableCloseTag(AElement: TdxHtmlElement);
begin
  EnsureTableHasRows;
  CurrentOutput.AddRange(FCaptionOutput);
  CurrentOutput.AddRange(FHeaderOutput);
  CurrentOutput.AddRange(InnerOutput);
  CurrentOutput.AddRange(FFooterOutput);
  CurrentOutput.Add(AElement);
  Corrector.ReturnToPrevState;
  if Corrector.State.CurrentOutput <> CurrentOutput then
    Corrector.AddMisplacedCellContent(CurrentOutput);
end;

procedure TdxHtmlCorrectorTableState.ProcessCaptionOpenTag(AElement: TdxHtmlElement);
begin
  FCaptionOutput.Add(AElement);
  Corrector.StartCaption(FCaptionOutput);
  HasRows := True;
end;

procedure TdxHtmlCorrectorTableState.ProcessCaptionCloseTag(AElement: TdxHtmlElement);
begin
  Corrector.ForceOpenCaption;
  Corrector.Process(AElement);
end;

procedure TdxHtmlCorrectorTableState.ProcessTbodyOpenTag(AElement: TdxHtmlElement);
begin
  InnerOutput.Add(AElement);
  Corrector.StartTbody(InnerOutput);
  HasRows := True;
end;

procedure TdxHtmlCorrectorTableState.ProcessTheadOpenTag(AElement: TdxHtmlElement);
begin
  FHeaderOutput.Add(AElement);
  Corrector.StartThead(FHeaderOutput);
  HasRows := True;
end;

procedure TdxHtmlCorrectorTableState.ProcessTfootOpenTag(AElement: TdxHtmlElement);
begin
  FFooterOutput.Add(AElement);
  Corrector.StartTfoot(FFooterOutput);
  HasRows := True;
end;

{ TdxHtmlCorrectorTableRowState }

constructor TdxHtmlCorrectorTableRowState.Create(ACorrector: TdxHtmlCorrector; AOutput: TdxHtmlElementList; AFirstRow: Boolean);
begin
  inherited Create(ACorrector, AOutput);
  FFirstRow := AFirstRow;
end;

destructor TdxHtmlCorrectorTableRowState.Destroy;
begin
  FreeAndNil(FOutput);
  inherited Destroy;
end;

procedure TdxHtmlCorrectorTableRowState.ProcessTableOpenTag(AElement: TdxHtmlElement);
begin
  Assert(FOutput = nil);
  FOutput := TdxHtmlElementList.Create;
  FOutput.Add(AElement);
  Corrector.StartNewTable(FOutput);
end;

procedure TdxHtmlCorrectorTableRowState.ProcessNonTableElement(AElement: TdxHtmlElement);
begin
  Corrector.AddMisplacedCellContent(AElement);
end;

procedure TdxHtmlCorrectorTableRowState.ProcessTableCloseTag(AElement: TdxHtmlElement);
begin
  Corrector.ForceCloseTableRow;
  Corrector.Process(AElement);
end;

procedure TdxHtmlCorrectorTableRowState.ProcessTableRowOpenTag(AElement: TdxHtmlElement);
begin
  Corrector.ForceCloseTableRow;
  Corrector.Process(AElement);
end;

procedure TdxHtmlCorrectorTableRowState.ProcessTableRowCloseTag(AElement: TdxHtmlElement);
begin
  if not FHasCells and FFirstRow then
  begin
    Corrector.ForceOpenTableCell;
    Corrector.ForceCloseTableCell;
  end;
  CurrentOutput.Add(AElement);
  Corrector.ReturnToPrevState;
end;

procedure TdxHtmlCorrectorTableRowState.ProcessTableCellOpenTag(AElement: TdxHtmlElement);
begin
  FHasCells := True;
  CurrentOutput.Add(AElement);
  Corrector.StartNewCell(CurrentOutput);
end;

procedure TdxHtmlCorrectorTableRowState.ProcessTableCellCloseTag(AElement: TdxHtmlElement);
begin
  if FHasCells then
    Exit;
  Corrector.ForceOpenTableCell;
  Corrector.Process(AElement);
end;

procedure TdxHtmlCorrectorTableRowState.ProcessCaptionOpenTag(AElement: TdxHtmlElement);
begin
  Corrector.ForceCloseTableRow;
  Corrector.Process(AElement);
end;

procedure TdxHtmlCorrectorTableRowState.ProcessCaptionCloseTag(AElement: TdxHtmlElement);
begin
  Corrector.ForceCloseTableRow;
  Corrector.Process(AElement);
end;

procedure TdxHtmlCorrectorTableRowState.ProcessEnd;
begin
  Corrector.ForceCloseTableRow;
  Corrector.ProcessEnd;
end;

{ TdxHtmlCorrectorTableCellState }

procedure TdxHtmlCorrectorTableCellState.ProcessNonTableElement(AElement: TdxHtmlElement);
begin
  CurrentOutput.Add(AElement);
end;

procedure TdxHtmlCorrectorTableCellState.ProcessTableOpenTag(AElement: TdxHtmlElement);
begin
  CurrentOutput.Add(AElement);
  Corrector.StartNewTable(CurrentOutput);
end;

procedure TdxHtmlCorrectorTableCellState.ProcessTableCloseTag(AElement: TdxHtmlElement);
begin
  Corrector.ForceCloseTableCell;
  Corrector.Process(AElement);
end;

procedure TdxHtmlCorrectorTableCellState.ProcessTableRowOpenTag(AElement: TdxHtmlElement);
begin
  Corrector.ForceCloseTableCell;
  Corrector.Process(AElement);
end;

procedure TdxHtmlCorrectorTableCellState.ProcessTableRowCloseTag(AElement: TdxHtmlElement);
begin
  Corrector.ForceCloseTableCell;
  Corrector.Process(AElement);
end;

procedure TdxHtmlCorrectorTableCellState.ProcessTableCellOpenTag(AElement: TdxHtmlElement);
begin
  Corrector.ForceCloseTableCell;
  Corrector.Process(AElement);
end;

procedure TdxHtmlCorrectorTableCellState.ProcessTableCellCloseTag(AElement: TdxHtmlElement);
begin
  Corrector.ReturnToPrevStateFromCellState(CurrentOutput.Count);
  CurrentOutput.Add(AElement);
end;

procedure TdxHtmlCorrectorTableCellState.ProcessCaptionOpenTag(AElement: TdxHtmlElement);
begin
  Corrector.ForceCloseTableCell;
  Corrector.Process(AElement);
end;

procedure TdxHtmlCorrectorTableCellState.ProcessCaptionCloseTag(AElement: TdxHtmlElement);
begin
  Corrector.ForceCloseTableCell;
  Corrector.Process(AElement);
end;

procedure TdxHtmlCorrectorTableCellState.ProcessEnd;
begin
  Corrector.ForceCloseTableCell;
  Corrector.ProcessEnd;
end;

{ TdxHtmlCorrectorTableCaptionState }

constructor TdxHtmlCorrectorTableCaptionState.Create(ACorrector: TdxHtmlCorrector; AOutput: TdxHtmlElementList);
begin
  inherited Create(ACorrector, TdxHtmlElementList.Create);
  FParentOutput := AOutput;
  FEmpty := True;
end;

destructor TdxHtmlCorrectorTableCaptionState.Destroy;
begin
  CurrentOutput.Free;
  inherited Destroy;
end;

procedure TdxHtmlCorrectorTableCaptionState.ProcessNonTableElement(AElement: TdxHtmlElement);
begin
  CurrentOutput.Add(AElement);
  FEmpty := False;
end;

procedure TdxHtmlCorrectorTableCaptionState.ProcessTableOpenTag(AElement: TdxHtmlElement);
begin
  CurrentOutput.Add(AElement);
  Corrector.StartNewTable(CurrentOutput);
end;

procedure TdxHtmlCorrectorTableCaptionState.ProcessTableCloseTag(AElement: TdxHtmlElement);
begin
  Corrector.ForceCloseCaption;
  Corrector.Process(AElement);
end;

procedure TdxHtmlCorrectorTableCaptionState.ProcessTableRowOpenTag(AElement: TdxHtmlElement);
begin
  Corrector.ForceCloseCaption;
  Corrector.Process(AElement);
end;

procedure TdxHtmlCorrectorTableCaptionState.ProcessTableRowCloseTag(AElement: TdxHtmlElement);
begin
  Corrector.ForceCloseCaption;
  Corrector.Process(AElement);
end;

procedure TdxHtmlCorrectorTableCaptionState.ProcessTableCellOpenTag(AElement: TdxHtmlElement);
begin
  Corrector.ForceCloseCaption;
  Corrector.Process(AElement);
end;

procedure TdxHtmlCorrectorTableCaptionState.ProcessTableCellCloseTag(AElement: TdxHtmlElement);
begin
  Corrector.ForceCloseCaption;
  CurrentOutput.Add(AElement);
end;

procedure TdxHtmlCorrectorTableCaptionState.ProcessCaptionOpenTag(AElement: TdxHtmlElement);
begin
end;

procedure TdxHtmlCorrectorTableCaptionState.ProcessCaptionCloseTag(AElement: TdxHtmlElement);
begin
  if not FEmpty then
  begin
    CurrentOutput.Add(AElement);
    FParentOutput.AddRange(CurrentOutput);
  end
  else
    FParentOutput.Delete(FParentOutput.Count - 1);
  Corrector.ReturnToPrevState;
end;

procedure TdxHtmlCorrectorTableCaptionState.ProcessEnd;
begin
  Corrector.ForceCloseCaption;
  Corrector.ProcessEnd;
end;

{ TdxHtmlCorrectorTableBodyState }

procedure TdxHtmlCorrectorTableBodyState.ProcessTbodyCloseTag(AElement: TdxHtmlElement);
begin
  EnsureTableHasRows;
  CurrentOutput.AddRange(InnerOutput);
  CurrentOutput.Add(AElement);
  Corrector.ReturnToPrevState;
end;

procedure TdxHtmlCorrectorTableBodyState.ProcessTbodyOpenTag(AElement: TdxHtmlElement);
begin
end;

procedure TdxHtmlCorrectorTableBodyState.ProcessTableCloseTag(AElement: TdxHtmlElement);
begin
  Corrector.ForceCloseTag(TdxHtmlTagNameID.Tbody);
  Corrector.Process(AElement);
end;

procedure TdxHtmlCorrectorTableBodyState.ProcessEnd;
begin
  Corrector.ForceCloseTag(TdxHtmlTagNameID.Tbody);
  inherited ProcessEnd;
end;

procedure TdxHtmlCorrectorTableBodyState.ProcessTfootOpenTag(AElement: TdxHtmlElement);
begin
  Corrector.ForceCloseTag(TdxHtmlTagNameID.Tbody);
  Corrector.Process(AElement);
end;

procedure TdxHtmlCorrectorTableBodyState.ProcessTheadOpenTag(AElement: TdxHtmlElement);
begin
  Corrector.ForceCloseTag(TdxHtmlTagNameID.Tbody);
  Corrector.Process(AElement);
end;

procedure TdxHtmlCorrectorTableBodyState.ProcessCaptionOpenTag(AElement: TdxHtmlElement);
begin
  Corrector.ForceCloseTag(TdxHtmlTagNameID.Tbody);
  Corrector.Process(AElement);
end;

procedure TdxHtmlCorrectorTableBodyState.ProcessCaptionCloseTag(AElement: TdxHtmlElement);
begin
end;

{ TdxHtmlCorrectorTableHeaderState }

procedure TdxHtmlCorrectorTableHeaderState.ProcessTheadOpenTag(AElement: TdxHtmlElement);
begin
end;

procedure TdxHtmlCorrectorTableHeaderState.ProcessTheadCloseTag(AElement: TdxHtmlElement);
begin
  EnsureTableHasRows;
  CurrentOutput.AddRange(InnerOutput);
  CurrentOutput.Add(AElement);
  Corrector.ReturnToPrevState;
end;

procedure TdxHtmlCorrectorTableHeaderState.ProcessTbodyOpenTag(AElement: TdxHtmlElement);
begin
  Corrector.ForceCloseTag(TdxHtmlTagNameID.Thead);
  Corrector.Process(AElement);
end;

procedure TdxHtmlCorrectorTableHeaderState.ProcessTableCloseTag(AElement: TdxHtmlElement);
begin
  Corrector.ForceCloseTag(TdxHtmlTagNameID.Thead);
  Corrector.Process(AElement);
end;

procedure TdxHtmlCorrectorTableHeaderState.ProcessEnd;
begin
  Corrector.ForceCloseTag(TdxHtmlTagNameID.Thead);
  inherited ProcessEnd;
end;

procedure TdxHtmlCorrectorTableHeaderState.ProcessCaptionOpenTag(AElement: TdxHtmlElement);
begin
  Corrector.ForceCloseTag(TdxHtmlTagNameID.Thead);
  Corrector.Process(AElement);
end;

procedure TdxHtmlCorrectorTableHeaderState.ProcessCaptionCloseTag(AElement: TdxHtmlElement);
begin
end;

{ TdxHtmlCorrectorTableFooterState }

procedure TdxHtmlCorrectorTableFooterState.ProcessTbodyOpenTag(AElement: TdxHtmlElement);
begin
  Corrector.ForceCloseTag(TdxHtmlTagNameID.Tfoot);
  Corrector.Process(AElement);
end;

procedure TdxHtmlCorrectorTableFooterState.ProcessTfootOpenTag(AElement: TdxHtmlElement);
begin
end;

procedure TdxHtmlCorrectorTableFooterState.ProcessTfootCloseTag(AElement: TdxHtmlElement);
begin
  EnsureTableHasRows;
  CurrentOutput.AddRange(InnerOutput);
  CurrentOutput.Add(AElement);
  Corrector.ReturnToPrevState;
end;

procedure TdxHtmlCorrectorTableFooterState.ProcessTableCloseTag(AElement: TdxHtmlElement);
begin
  Corrector.ForceCloseTag(TdxHtmlTagNameID.Tfoot);
  Corrector.Process(AElement);
end;

procedure TdxHtmlCorrectorTableFooterState.ProcessEnd;
begin
  Corrector.ForceCloseTag(TdxHtmlTagNameID.Tfoot);
  inherited ProcessEnd;
end;

procedure TdxHtmlCorrectorTableFooterState.ProcessCaptionOpenTag(AElement: TdxHtmlElement);
begin
  Corrector.ForceCloseTag(TdxHtmlTagNameID.Tfoot);
  Corrector.Process(AElement);
end;

procedure TdxHtmlCorrectorTableFooterState.ProcessCaptionCloseTag(AElement: TdxHtmlElement);
begin
end;

{ TdxHtmlCorrector }

constructor TdxHtmlCorrector.Create;
begin
  inherited Create;
  FStatesStorage := TdxObjectList<TdxHtmlCorrectorStateBase>.Create;
end;

destructor TdxHtmlCorrector.Destroy;
begin
  FStates.Free;
  FStatesStorage.Free;
  inherited Destroy;
end;

class function TdxHtmlCorrector.GetText(AElements: TdxHtmlElementList): string;
var
  AResult: TStringBuilder;
  AElement: TdxHtmlElement;
  I: Integer;
begin
  AResult := TStringBuilder.Create;
  try
    for I := 0 to AElements.Count - 1 do
    begin
      AElement := AElements[I];
      AResult.Append(Trim(AElement.RawText));
    end;
    Result := AResult.ToString;
  finally
    AResult.Free;
  end;
end;

function TdxHtmlCorrector.GetState: TdxHtmlCorrectorStateBase;
begin
  Result := FStates[FStates.Count - 1];
end;

function TdxHtmlCorrector.GetCorrectedHtmlElements(AHtmlElements: TdxHtmlElementList; out AHtmlTagPresent: Boolean): TdxHtmlElementList;
var
  AStartComment, AEndComment, AComment: TdxComment;
  ABeforeStartComment, AAfterEndComment, AElement: TdxHtmlElement;
  ACount, I: Integer;
  ATag: TdxTag;
begin
  FStates := TdxList<TdxHtmlCorrectorStateBase>.Create;

  Result := TdxHtmlElementList.Create;
  Result.Capacity := AHtmlElements.Count;

  AddState(TdxHtmlCorrectorDefaultState.Create(Self, Result));
  AStartComment := nil;
  AEndComment := nil;
  ABeforeStartComment := nil;
  AAfterEndComment := nil;
  ACount := AHtmlElements.Count;
  AHtmlTagPresent := False;
  for I := 0 to ACount - 1 do
  begin
    AElement := AHtmlElements[I];
    AComment := Safe<TdxComment>.Cast(AElement);
    if AComment <> nil then
    begin
      if AComment.CommentText = 'StartFragment' then
      begin
        AStartComment := AComment;
        ABeforeStartComment := CalculateHtmlElementBeforeComment(AComment, AHtmlElements, I);
      end;
      if AComment.CommentText = 'EndFragment' then
      begin
        AEndComment := AComment;
        AAfterEndComment := CalculateHtmlElementAfterComment(AComment, AHtmlElements, I);
      end;
    end
    else
    begin
      if not AHtmlTagPresent and (AElement.ElementType = TdxHtmlElementType.OpenTag) then
      begin
        ATag := Safe<TdxTag>.Cast(AElement);
        if (ATag <> nil) and (ATag.NameID = TdxHtmlTagNameID.Html) then
          AHtmlTagPresent := True;
      end;
      Process(AElement);
    end;
  end;
  ProcessEnd;
  if AStartComment <> nil then
    InsertStartComment(AStartComment, Result, ABeforeStartComment);
  if AEndComment <> nil then
    InsertEndComment(AEndComment, Result, AAfterEndComment);
end;

function TdxHtmlCorrector.CalculateHtmlElementBeforeComment(AComment: TdxComment; AList: TdxHtmlElementList; AIndex: Integer): TdxHtmlElement;
var
  I: Integer;
begin
  for I := AIndex - 1 downto 0 do
    if not TdxHtmlCorrectorStateBase.IsWhiteSpaceElement(AList[I]) then
      Exit(AList[I]);
  Result := nil;
end;

function TdxHtmlCorrector.CalculateHtmlElementAfterComment(AComment: TdxComment; AList: TdxHtmlElementList; AIndex: Integer): TdxHtmlElement;
var
  ACount, I: Integer;
begin
  ACount := AList.Count;
  for I := AIndex + 1 to ACount - 1 do
    if not TdxHtmlCorrectorStateBase.IsWhiteSpaceElement(AList[I]) then
      Exit(AList[I]);
  Result := nil;
end;

procedure TdxHtmlCorrector.InsertStartComment(ANewComment: TdxComment; AOutput: TdxHtmlElementList; ABeforeComment: TdxHtmlElement);
begin
  if (ABeforeComment <> nil) then
  begin
    AOutput.Insert(AOutput.IndexOf(ABeforeComment) + 1, ANewComment);
  end
  else
    AOutput.Insert(0, ANewComment);
end;

procedure TdxHtmlCorrector.InsertEndComment(ANewComment: TdxComment; AOutput: TdxHtmlElementList; AAfterComment: TdxHtmlElement);
begin
  if (AAfterComment <> nil) then
  begin
    AOutput.Insert(AOutput.IndexOf(AAfterComment), ANewComment);
  end
  else
    AOutput.Add(ANewComment);
end;

procedure TdxHtmlCorrector.StartNewTable(AOutput: TdxHtmlElementList);
var
  ANewState: TdxHtmlCorrectorTableState;
begin
  ANewState := TdxHtmlCorrectorTableState.Create(Self, AOutput);
  AddState(ANewState);
  FLastOpenedTableState := ANewState;
end;

procedure TdxHtmlCorrector.StartNewRow(AOutput: TdxHtmlElementList; AFirstRow: Boolean);
var
  ANewState: TdxHtmlCorrectorTableRowState;
begin
  ANewState := TdxHtmlCorrectorTableRowState.Create(Self, AOutput, AFirstRow);
  AddState(ANewState);
end;

procedure TdxHtmlCorrector.StartNewCell(AOutput: TdxHtmlElementList);
var
  ANewState: TdxHtmlCorrectorTableCellState;
begin
  ANewState := TdxHtmlCorrectorTableCellState.Create(Self, AOutput);
  AddState(ANewState);
  FLastOpenedTableState.OnStartCell(AOutput);
end;

procedure TdxHtmlCorrector.StartCaption(AOutput: TdxHtmlElementList);
var
  ANewState: TdxHtmlCorrectorTableCaptionState;
begin
  ANewState := TdxHtmlCorrectorTableCaptionState.Create(Self, AOutput);
  AddState(ANewState);
end;

procedure TdxHtmlCorrector.StartTbody(AOutput: TdxHtmlElementList);
var
  ANewState: TdxHtmlCorrectorTableBodyState;
begin
  ANewState := TdxHtmlCorrectorTableBodyState.Create(Self, AOutput);
  AddState(ANewState);
  FLastOpenedTableState := ANewState;
end;

procedure TdxHtmlCorrector.StartThead(AOutput: TdxHtmlElementList);
var
  ANewState: TdxHtmlCorrectorTableHeaderState;
begin
  ANewState := TdxHtmlCorrectorTableHeaderState.Create(Self, AOutput);
  AddState(ANewState);
  FLastOpenedTableState := ANewState;
end;

procedure TdxHtmlCorrector.StartTfoot(AOutput: TdxHtmlElementList);
var
  ANewState: TdxHtmlCorrectorTableFooterState;
begin
  ANewState := TdxHtmlCorrectorTableFooterState.Create(Self, AOutput);
  AddState(ANewState);
  FLastOpenedTableState := ANewState;
end;

procedure TdxHtmlCorrector.Process(AElement: TdxHtmlElement);
begin
  State.Process(AElement);
end;

procedure TdxHtmlCorrector.ProcessEnd;
begin
  State.ProcessEnd;
end;

procedure TdxHtmlCorrector.AddMisplacedCellContent(AElements: TdxHtmlElementList);
begin
  FLastOpenedTableState.AddMisplacedCellContent(AElements);
end;

procedure TdxHtmlCorrector.AddMisplacedCellContent(AElement: TdxHtmlElement);
begin
  FLastOpenedTableState.AddMisplacedCellContent(AElement);
end;

procedure TdxHtmlCorrector.AddState(AState: TdxHtmlCorrectorStateBase);
begin
  FStates.Add(AState);
  FStatesStorage.Add(AState);
end;

procedure TdxHtmlCorrector.ForceOpenTableCell;
var
  ATag: TdxTag;
begin
  ATag := TdxTag.Create(TdxHtmlElementType.OpenTag);
  ATag.NameID := TdxHtmlTagNameID.TD;
  Process(ATag);
end;

procedure TdxHtmlCorrector.ForceCloseTableCell;
var
  ATag: TdxTag;
begin
  ATag := TdxTag.Create(TdxHtmlElementType.CloseTag);
  ATag.NameID := TdxHtmlTagNameID.TD;
  Process(ATag);
end;

procedure TdxHtmlCorrector.ForceOpenTableRow;
var
  ATag: TdxTag;
begin
  ATag := TdxTag.Create(TdxHtmlElementType.OpenTag);
  ATag.NameID := TdxHtmlTagNameID.TR;
  Process(ATag);
end;

procedure TdxHtmlCorrector.ForceCloseTableRow;
var
  ATag: TdxTag;
begin
  ATag := TdxTag.Create(TdxHtmlElementType.CloseTag);
  ATag.NameID := TdxHtmlTagNameID.TR;
  Process(ATag);
end;

procedure TdxHtmlCorrector.ForceCloseTable;
var
  ATag: TdxTag;
begin
  ATag := TdxTag.Create(TdxHtmlElementType.CloseTag);
  ATag.NameID := TdxHtmlTagNameID.Table;
  Process(ATag);
end;

procedure TdxHtmlCorrector.ForceOpenCaption;
var
  ATag: TdxTag;
begin
  ATag := TdxTag.Create(TdxHtmlElementType.OpenTag);
  try
    ATag.NameID := TdxHtmlTagNameID.Caption;
    Process(ATag);
  finally
    ATag.Free;
  end;
end;

procedure TdxHtmlCorrector.ForceOpenTag(ANameID: TdxHtmlTagNameID);
var
  ATag: TdxTag;
begin
  ATag := TdxTag.Create(TdxHtmlElementType.OpenTag);
  ATag.NameID := ANameID;
  Process(ATag);
end;

procedure TdxHtmlCorrector.ForceCloseTag(ANameID: TdxHtmlTagNameID);
var
  ATag: TdxTag;
begin
  ATag := TdxTag.Create(TdxHtmlElementType.CloseTag);
  ATag.NameID := ANameID;
  Process(ATag);
end;

procedure TdxHtmlCorrector.ForceCloseCaption;
var
  ATag: TdxTag;
begin
  ATag := TdxTag.Create(TdxHtmlElementType.CloseTag);
  ATag.NameID := TdxHtmlTagNameID.Caption;
  Process(ATag);
end;

procedure TdxHtmlCorrector.ReturnToPrevStateFromCellState(ALastCellPosition: Integer);
begin
  Assert(State is TdxHtmlCorrectorTableCellState);
  FLastOpenedTableState.OnEndCell(ALastCellPosition);
  ReturnToPrevState;
end;

procedure TdxHtmlCorrector.ReturnToPrevState;
begin
  FStates.Delete(FStates.Count - 1);
  if FStates.Count = 1 then
    FLastOpenedTableState := nil
  else
    if State is TdxHtmlCorrectorTableCellState then
    begin
      Assert((FStates.Count > 3) and (FStates[FStates.Count - 3] is TdxHtmlCorrectorTableStateBase));
      FLastOpenedTableState := TdxHtmlCorrectorTableStateBase(FStates[FStates.Count - 3]);
    end
    else
      if State is TdxHtmlCorrectorTableRowState then
      begin
        Assert((FStates.Count > 2) and (FStates[FStates.Count - 2] is TdxHtmlCorrectorTableStateBase));
        FLastOpenedTableState := TdxHtmlCorrectorTableStateBase(FStates[FStates.Count - 2]);
      end
      else
        if State is TdxHtmlCorrectorTableState then
        begin
          Assert((FStates.Count > 1) and (FStates[FStates.Count - 1] is TdxHtmlCorrectorTableStateBase));
          FLastOpenedTableState := TdxHtmlCorrectorTableStateBase(FStates[FStates.Count - 1]);
        end;
end;

{ TdxFontSizeInfo }

constructor TdxFontSizeInfo.Create(ADoubleFontSize: Integer; AUseFontSize: Boolean);
begin
  FDoubleFontSize := ADoubleFontSize;
  FUseFontSize := AUseFontSize;
end;

{ TdxHtmlImportedTableInfo }

constructor TdxHtmlImportedTableInfo.Create(ATable: TdxTable);
begin
  inherited Create(ATable);
  FTableCaption := TdxList<TdxTableCell>.Create;
  FCaptionColSpan := 0;
end;

destructor TdxHtmlImportedTableInfo.Destroy;
begin
  FTableCaption.Free;
  inherited Destroy;
end;

procedure TdxHtmlImportedTableInfo.SetCaptionColSpan(const AValue: Integer);
var
  ACell: TdxTableCell;
  I: Integer;
begin
  if FCaptionColSpan = AValue then
    Exit;
  if (FCaptionColSpan = 0) and (AValue = 1) then
  begin
    FCaptionColSpan := AValue;
    Exit;
  end;
  FCaptionColSpan := AValue;
  for I := 0 to TableCaption.Count - 1 do
  begin
    ACell := TableCaption[I];
    ACell.ColumnSpan := AValue;
  end;
end;

{ TdxHtmlTablesImportHelper }

constructor TdxHtmlTablesImportHelper.Create(APieceTable: TdxPieceTable; AImporter: TdxHtmlImporter);
begin
  inherited Create(APieceTable);
  FImporter := AImporter;
end;

function TdxHtmlTablesImportHelper.GetTableInfo: TdxHtmlImportedTableInfo;
begin
  if (inherited TableInfo = nil) then
    Result := nil
  else
    Result := TdxHtmlImportedTableInfo((inherited TableInfo));
end;

function TdxHtmlTablesImportHelper.CreateTableInfo(ANewTable: TdxTable): TdxImportedTableInfo;
begin
  Result := TdxHtmlImportedTableInfo.Create(ANewTable);
end;

function TdxHtmlTablesImportHelper.CreateCoveredCellWithEmptyParagraph(ARow: TdxTableRow): TdxTableCell;
var
  ACoveredCell: TdxTableCell;
  AStart, AEnd: TdxParagraphIndex;
begin
  ACoveredCell := TdxTableCell.Create(ARow);
  ACoveredCell.Row.Cells.AddInternal(ACoveredCell);

  InsertEmptyParagraph;
  AStart := Importer.Position.ParagraphIndex;
  AEnd := Importer.Position.ParagraphIndex;

  Assert(ARow = Table.LastRow);
  InitializeTableCell(ACoveredCell, AStart, AEnd);
  Result := ACoveredCell;
end;

procedure TdxHtmlTablesImportHelper.InsertEmptyParagraph;
begin
  if Importer.IsEmptyParagraph then
  begin
    Importer.IsEmptyParagraph := False;
  end
  else
  begin
    Importer.TagsStack[0].Tag.ParagraphFunctionalProcess;
    Importer.IsEmptyParagraph := False;
  end;
end;

procedure TdxHtmlTablesImportHelper.FixBrokenCells(ACurrentTable: TdxTable);
var
  AOldEnd, ACurrentLogPosition: TdxDocumentLogPosition;
  AOldParagraphsCount, ACurrentParagraphsCount, ADiffLogPosition, ADiffParagraphIndex: Integer;
begin
  AOldEnd := Importer.PieceTable.Paragraphs.Last.EndLogPosition;
  AOldParagraphsCount := Importer.PieceTable.Paragraphs.Count;
  inherited FixBrokenCells(ACurrentTable);
  ACurrentTable.NormalizeRows;
  ACurrentTable.NormalizeCellColumnSpans;

  ACurrentLogPosition := FImporter.PieceTable.Paragraphs.Last.EndLogPosition;
  ACurrentParagraphsCount := Importer.PieceTable.Paragraphs.Count;
  ADiffLogPosition := Max(0, AOldEnd - ACurrentLogPosition);
  ADiffParagraphIndex := Max(0, AOldParagraphsCount - ACurrentParagraphsCount);
  Importer.Position.LogPosition := Importer.Position.LogPosition - ADiffLogPosition;
  Importer.Position.ParagraphIndex := Importer.Position.ParagraphIndex - ADiffParagraphIndex;
end;

{ TdxHtmlImporter }

constructor TdxHtmlImporter.Create(ADocumentModel: TdxDocumentModel; AOptions: TdxHtmlDocumentImporterOptions);
begin
  inherited Create(ADocumentModel, AOptions);
  FTagsStack := TdxObjectList<TdxOpenHtmlTag>.Create;
  FHtmlFontSize := TdxHtmlFontSize.Create;
  FUniqueUriBasedImages := TdxNamedObjectDictionary<TdxUriOfficeImage>.Create(True);

  FStyleTagCollection := TdxObjectList<TdxCssElement>.Create;
  FIsEmptyParagraph := True;
  FIsEmptyLine := False;
  FCanInsertSpace := True;

  LastOpenParagraphTagIndex := -1;
  LastOpenAnchorTagIndex := -1;
  BaseUri := AOptions.SourceUri;
  AbsoluteBaseUri := GetAbsoluteBaseUri(AOptions.SourceUri);
  FEncoding := AOptions.ActualEncoding;
  FProcessBookmarks := TdxObjectStack<TdxHtmlBookmarkInfo>.Create(False);
  FTablesImportHelper := TdxHtmlTablesImportHelper.Create(PieceTable, Self);
  FUsedNumberingLists := TdxSortedList<TdxNumberingListIndex>.Create;
  FParser := CreateHtmlParser;
  FMarkTable := TdxNamedOrdinalDictionary<TdxBookmark>.Create;
  RootDoubleFontSize := 24;
  FTagCollection := TdxObjectList<TdxTagBase>.Create;
end;

destructor TdxHtmlImporter.Destroy;
begin
  FEmptyHyperlinks.Free;
  FMarkTable.Free;
  FParser.Free;
  FUsedNumberingLists.Free;
  FProcessBookmarks.Free;
  FTablesImportHelper.Free;
  FStyleTagCollection.Free;
  FTagsStack.Free;
  FUniqueUriBasedImages.Free;
  FTagCollection.Free;
  FProcessHyperlink.Free;
  Element := nil;
  inherited Destroy;
end;

class constructor TdxHtmlImporter.Initialize;
begin
  FHtmlKeywordTable := CreateHtmlKeywordTable;
  FSpecialSymbolTable := CreateHtmlSpecialSymbolTable;
end;

class destructor TdxHtmlImporter.Finalize;
begin
  FHtmlKeywordTable.Free;
  FSpecialSymbolTable.Free;
end;

class function TdxHtmlImporter.CreateHtmlSpecialSymbolTable: TdxHtmlSpecialSymbolTable;
begin
  Result := TdxHtmlSpecialSymbolTable.Create;
  Result.Add('quot',    '"');
  Result.Add('amp',     '&');
  Result.Add('apos',    #$27);
  Result.Add('lt',      '<');
  Result.Add('gt',      '>');
  Result.Add('nbsp',    TdxCharacters.NonBreakingSpace);
  Result.Add('iexcl',   #$00A1);
  Result.Add('cent',    #$00A2);
  Result.Add('pound',   #$00A3);
  Result.Add('curren',  #$00A4);
  Result.Add('yen',     #$00A5);
  Result.Add('brvbar',  #$00A6);
  Result.Add('sect',    #$00A7);
  Result.Add('uml',     #$00A8);
  Result.Add('copy',    #$00A9);
  Result.Add('ordf',    #$00AA);
  Result.Add('laquo',   #$00AB);
  Result.Add('not',     #$00AC);
  Result.Add('shy',     #$00AD);
  Result.Add('reg',     #$00AE);
  Result.Add('macr',    #$00AF);
  Result.Add('deg',     #$00B0);
  Result.Add('plusmn',  #$00B1);
  Result.Add('sup2',    #$00B2);
  Result.Add('sup3',    #$00B3);
  Result.Add('acute',   #$00B4);
  Result.Add('micro',   #$00B5);
  Result.Add('para',    #$00B6);
  Result.Add('middot',  #$00B7);
  Result.Add('cedil',   #$00B8);
  Result.Add('sup1',    #$00B9);
  Result.Add('ordm',    #$00BA);
  Result.Add('raquo',   #$00BB);
  Result.Add('frac14',  #$00BC);
  Result.Add('frac12',  #$00BD);
  Result.Add('frac34',  #$00BE);
  Result.Add('iquest',  #$00BF);
  Result.Add('Agrave',  #$00C0);
  Result.Add('Aacute',  #$00C1);
  Result.Add('Acirc',   #$00C2);
  Result.Add('Atilde',  #$00C3);
  Result.Add('Auml',    #$00C4);
  Result.Add('Aring',   #$00C5);
  Result.Add('AElig',   #$00C6);
  Result.Add('Ccedil',  #$00C7);
  Result.Add('Egrave',  #$00C8);
  Result.Add('Eacute',  #$00C9);
  Result.Add('Ecirc',   #$00CA);
  Result.Add('Euml',    #$00CB);
  Result.Add('Igrave',  #$00CC);
  Result.Add('Iacute',  #$00CD);
  Result.Add('Icirc',   #$00CE);
  Result.Add('Iuml',    #$00CF);
  Result.Add('ETH',     #$00D0);
  Result.Add('Ntilde',  #$00D1);
  Result.Add('Ograve',  #$00D2);
  Result.Add('Oacute',  #$00D3);
  Result.Add('Ocirc',   #$00D4);
  Result.Add('Otilde',  #$00D5);
  Result.Add('Ouml',    #$00D6);
  Result.Add('times',   #$00D7);
  Result.Add('Oslash',  #$00D8);
  Result.Add('Ugrave',  #$00D9);
  Result.Add('Uacute',  #$00DA);
  Result.Add('Ucirc',   #$00DB);
  Result.Add('Uuml',    #$00DC);
  Result.Add('Yacute',  #$00DD);
  Result.Add('Yuml',    #$0178);
  Result.Add('THORN',   #$00DE);
  Result.Add('szlig',   #$00DF);
  Result.Add('agrave',  #$00E0);
  Result.Add('aacute',  #$00E1);
  Result.Add('acirc',   #$00E2);
  Result.Add('atilde',  #$00E3);
  Result.Add('auml',    #$00E4);
  Result.Add('aring',   #$00E5);
  Result.Add('aelig',   #$00E6);
  Result.Add('ccedil',  #$00E7);
  Result.Add('egrave',  #$00E8);
  Result.Add('eacute',  #$00E9);
  Result.Add('ecirc',   #$00EA);
  Result.Add('euml',    #$00EB);
  Result.Add('igrave',  #$00EC);
  Result.Add('iacute',  #$00ED);
  Result.Add('icirc',   #$00EE);
  Result.Add('iuml',    #$00EF);
  Result.Add('eth',     #$00F0);
  Result.Add('ntilde',  #$00F1);
  Result.Add('ograve',  #$00F2);
  Result.Add('oacute',  #$00F3);
  Result.Add('ocirc',   #$00F4);
  Result.Add('otilde',  #$00F5);
  Result.Add('ouml',    #$00F6);
  Result.Add('divide',  #$00F7);
  Result.Add('oslash',  #$00F8);
  Result.Add('ugrave',  #$00F9);
  Result.Add('uacute',  #$00FA);
  Result.Add('ucirc',   #$00FB);
  Result.Add('uuml',    #$00FC);
  Result.Add('yacute',  #$00FD);
  Result.Add('thorn',   #$00FE);
  Result.Add('yuml',    #$00FF);
  Result.Add('oelig',   #$0153);
  Result.Add('OElig',   #$0152);
  Result.Add('Scaron',  #$0160);
  Result.Add('scaron',  #$0161);
  Result.Add('fnof',    #$0192);
  Result.Add('circ',    #$02C6);
  Result.Add('tilde',   #$02DC);

  Result.Add('trade',   #$2122);
  Result.Add('hellip',  #$2026);
  Result.Add('prime',   #$2032);
  Result.Add('Prime',   #$2033);
  Result.Add('oline',   #$203E);
  Result.Add('frasl',   #$2044);
  Result.Add('infin',   #$221E);
  Result.Add('asymp',   #$2248);
  Result.Add('ne',      #$2260);
  Result.Add('le',      #$2264);
  Result.Add('ge',      #$2265);
  Result.Add('lsquo',   #$2018);
  Result.Add('rsquo',   #$2019);
  Result.Add('ldquo',   #$201C);
  Result.Add('rdquo',   #$201D);
  Result.Add('bdquo',   #$201E);
  Result.Add('dagger',  #$2020);
  Result.Add('Dagger',  #$2021);
  Result.Add('permil',  #$2030);
  Result.Add('lsaquo',  #$2039);
  Result.Add('rsaquo',  #$203A);
  Result.Add('euro',    #$20AC);
  Result.Add('ndash',   #$2013);
  Result.Add('mdash',   #$2014);
  Result.Add('emsp',    TdxCharacters.EmSpace);
  Result.Add('ensp',    TdxCharacters.EnSpace);
  Result.Add('thinsp',  TdxCharacters.QmSpace);

  Result.Add('image',   #$2111);
  Result.Add('weierp',  #$2118);
  Result.Add('real',    #$211C);
  Result.Add('alefsym', #$2135);
  Result.Add('larr',    #$2190);
  Result.Add('uarr',    #$2191);
  Result.Add('rarr',    #$2192);
  Result.Add('darr',    #$2193);
  Result.Add('harr',    #$2194);
  Result.Add('crarr',   #$21B5);
  Result.Add('lArr',    #$21D0);
  Result.Add('uArr',    #$21D1);
  Result.Add('rArr',    #$21D2);
  Result.Add('dArr',    #$21D3);
  Result.Add('hArr',    #$21D4);
  Result.Add('forall',  #$2200);
  Result.Add('part',    #$2202);
  Result.Add('exist',   #$2203);
  Result.Add('empty',   #$2205);
  Result.Add('nabla',   #$2207);
  Result.Add('isin',    #$2208);
  Result.Add('notin',   #$2209);
  Result.Add('ni',      #$220B);
  Result.Add('prod',    #$220F);
  Result.Add('sum',     #$2211);
  Result.Add('minus',   #$2212);
  Result.Add('lowast',  #$2217);
  Result.Add('radic',   #$221A);
  Result.Add('prop',    #$221D);

  Result.Add('ang',     #$2220);
  Result.Add('and',     #$2227);
  Result.Add('or',      #$2228);
  Result.Add('cap',     #$2229);
  Result.Add('cup',     #$222A);
  Result.Add('int',     #$222B);
  Result.Add('there4',  #$2234);
  Result.Add('sim',     #$223C);
  Result.Add('cong',    #$2245);

  Result.Add('equiv',   #$2261);

  Result.Add('sub',     #$2282);
  Result.Add('sup',     #$2283);
  Result.Add('nsub',    #$2284);
  Result.Add('sube',    #$2286);
  Result.Add('supe',    #$2287);
  Result.Add('oplus',   #$2295);
  Result.Add('otimes',  #$2297);
  Result.Add('perp',    #$22A5);
  Result.Add('sdot',    #$22C5);
  Result.Add('lceil',   #$2308);
  Result.Add('rceil',   #$2309);
  Result.Add('lfloor',  #$230A);
  Result.Add('rfloor',  #$230B);
  Result.Add('lang',    #$2329);
  Result.Add('rang',    #$232A);
  Result.Add('loz',     #$25CA);
  Result.Add('spades',  #$2660);
  Result.Add('clubs',   #$2663);
  Result.Add('hearts',  #$2665);
  Result.Add('diams',   #$2666);

  Result.Add('Alpha',   #$0391);
  Result.Add('Beta',    #$0392);
  Result.Add('Gamma',   #$0393);
  Result.Add('Delta',   #$0394);
  Result.Add('Epsilon', #$0395);
  Result.Add('Zeta',    #$0396);
  Result.Add('Eta',     #$0397);
  Result.Add('Theta',   #$0398);
  Result.Add('Iota',    #$0399);
  Result.Add('Kappa',   #$039A);
  Result.Add('Lambda',  #$039B);
  Result.Add('Mu',      #$039C);
  Result.Add('Nu',      #$039D);
  Result.Add('Xi',      #$039E);
  Result.Add('Omicron', #$039F);
  Result.Add('Pi',      #$03A0);
  Result.Add('Rho',     #$03A1);
  Result.Add('Sigma',   #$03A3);
  Result.Add('Tau',     #$03A4);
  Result.Add('Upsilon', #$03A5);
  Result.Add('Phi',     #$03A6);
  Result.Add('Chi',     #$03A7);
  Result.Add('Psi',     #$03A8);
  Result.Add('Omega',   #$03A9);

  Result.Add('alpha',   #$03B1);
  Result.Add('beta',    #$03B2);
  Result.Add('gamma',   #$03B3);
  Result.Add('delta',   #$03B4);
  Result.Add('epsilon', #$03B5);
  Result.Add('zeta',    #$03B6);
  Result.Add('eta',     #$03B7);
  Result.Add('theta',   #$03B8);
  Result.Add('iota',    #$03B9);
  Result.Add('kappa',   #$03BA);
  Result.Add('lambda',  #$03BB);
  Result.Add('mu',      #$03BC);
  Result.Add('nu',      #$03BD);
  Result.Add('xi',      #$03BE);
  Result.Add('omicron', #$03BF);
  Result.Add('pi',      #$03C0);
  Result.Add('rho',     #$03C1);
  Result.Add('sigmaf',  #$03C2);
  Result.Add('sigma',   #$03C3);
  Result.Add('tau',     #$03C4);
  Result.Add('upsilon', #$03C5);
  Result.Add('phi',     #$03C6);
  Result.Add('chi',     #$03C7);
  Result.Add('psi',     #$03C8);
  Result.Add('omega',   #$03C9);

  Result.Add('thetasy', #$03D1);
  Result.Add('upsih',   #$03D2);
  Result.Add('piv',     #$03D6);

  Result.Add('bull',    #$2022);
  Result.Add('zwnj',    #$200C);
  Result.Add('zwj',     #$200D);
end;

class function TdxHtmlImporter.CreateHtmlKeywordTable: TdxHtmlKeywordTranslatorTable;
begin
  Result := TdxHtmlKeywordTranslatorTable.Create;
  Result.Add(TdxHtmlTagNameID.Abbr,          AbbrKeywordTag);
  Result.Add(TdxHtmlTagNameID.Acronym,       AcronymKeywordTag);
  Result.Add(TdxHtmlTagNameID.Address,       AddressKeywordTag);
  Result.Add(TdxHtmlTagNameID.Area,          AreaKeywordTag);
  Result.Add(TdxHtmlTagNameID.BaseFont,      BaseFontKeywordTag);
  Result.Add(TdxHtmlTagNameID.Bdo,           BdoKeywordTag);
  Result.Add(TdxHtmlTagNameID.BgSound,       BgsoundKeywordTag);
  Result.Add(TdxHtmlTagNameID.Button,        ButtonKeywordTag);
  Result.Add(TdxHtmlTagNameID.Cite,          CiteKeywordTag);

  Result.Add(TdxHtmlTagNameID.Dd,            DdKeywordTag);
  Result.Add(TdxHtmlTagNameID.Del,           DelKeywordTag);
  Result.Add(TdxHtmlTagNameID.Dfn,           DfnKeywordTag);
  Result.Add(TdxHtmlTagNameID.Dl,            DlKeywordTag);
  Result.Add(TdxHtmlTagNameID.Dt,            DtKeywordTag);
  Result.Add(TdxHtmlTagNameID.Embed,         EmbedKeywordTag);
  Result.Add(TdxHtmlTagNameID.Fieldset,      FieldsetKeywordTag);
  Result.Add(TdxHtmlTagNameID.Form,          FormKeywordTag);
  Result.Add(TdxHtmlTagNameID.Frame,         FrameKeywordTag);
  Result.Add(TdxHtmlTagNameID.FrameSet,      FrameSetKeywordTag);
  Result.Add(TdxHtmlTagNameID.Hr,            HrKeywordTag);
  Result.Add(TdxHtmlTagNameID.Iframe,        IframeKeywordTag);
  Result.Add(TdxHtmlTagNameID.Input,         InputKeywordTag);
  Result.Add(TdxHtmlTagNameID.Ins,           InsKeywordTag);
  Result.Add(TdxHtmlTagNameID.Kbd,           KbdKeywordTag);
  Result.Add(TdxHtmlTagNameID.Label,         LabelKeywordTag);
  Result.Add(TdxHtmlTagNameID.Legend,        LegendKeywordTag);
  Result.Add(TdxHtmlTagNameID.Map,           MapKeywordTag);
  Result.Add(TdxHtmlTagNameID.Nobr,          NobrKeywordTag);
  Result.Add(TdxHtmlTagNameID.Noembed,       NoembedKeywordTag);
  Result.Add(TdxHtmlTagNameID.NoFrames,      NoFramesKeywordTag);
  Result.Add(TdxHtmlTagNameID.NoScript,      NoScriptKeywordTag);
  Result.Add(TdxHtmlTagNameID.Object,        ObjectKeywordTag);
  Result.Add(TdxHtmlTagNameID.OptGroup,      OptGroupKeywordTag);
  Result.Add(TdxHtmlTagNameID.Option,        OptionKeywordTag);
  Result.Add(TdxHtmlTagNameID.Param,         ParamKeywordTag);
  Result.Add(TdxHtmlTagNameID.Q,             QKeywordTag);
  Result.Add(TdxHtmlTagNameID.Samp,          SampKeywordTag);
  Result.Add(TdxHtmlTagNameID.Select,        SelectKeywordTag);
  Result.Add(TdxHtmlTagNameID.TextArea,      TextAreaKeywordTag);
  Result.Add(TdxHtmlTagNameID.TT,            TtKeywordTag);
  Result.Add(TdxHtmlTagNameID.Var,           VarKeywordTag);
  Result.Add(TdxHtmlTagNameID.Wbr,           WbrKeywordTag);
  Result.Add(TdxHtmlTagNameID.Xmp,           XmpKeywordTag);

  Result.Add(TdxHtmlTagNameID.Html,          HtmlKeywordTag);
  Result.Add(TdxHtmlTagNameID.Head,          HeadKeywordTag);
  Result.Add(TdxHtmlTagNameID.Base,          BaseKeywordTag);
  Result.Add(TdxHtmlTagNameID.Meta,          MetaKeywordTag);
  Result.Add(TdxHtmlTagNameID.Title,         TitleKeywordTag);
  Result.Add(TdxHtmlTagNameID.Link,          LinkKeywordTag);
  Result.Add(TdxHtmlTagNameID.Anchor,        AnchorKeywordTag);
  Result.Add(TdxHtmlTagNameID.Body,          BodyKeywordTag);
  Result.Add(TdxHtmlTagNameID.Bold,          BoldKeywordTag);
  Result.Add(TdxHtmlTagNameID.Italic,        ItalicKeywordTag);
  Result.Add(TdxHtmlTagNameID.Underline,     UnderlineKeywordTag);
  Result.Add(TdxHtmlTagNameID.Paragraph,     ParagraphKeywordTag);
  Result.Add(TdxHtmlTagNameID.Strong,        StrongKeywordTag);
  Result.Add(TdxHtmlTagNameID.Big,           BigKeywordTag);
  Result.Add(TdxHtmlTagNameID.Small,         SmallKeywordTag);
  Result.Add(TdxHtmlTagNameID.Preformatted,  PreformattedKeywordTag);
  Result.Add(TdxHtmlTagNameID.Font,          FontKeywordTag);
  Result.Add(TdxHtmlTagNameID.LineBreak,     LineBreakKeywordTag);
  Result.Add(TdxHtmlTagNameID.Emphasized,    EmphasizedKeywordTag);
  Result.Add(TdxHtmlTagNameID.Img,           ImageKeywordTag);
  Result.Add(TdxHtmlTagNameID.Heading1,      Heading1KeywordTag);
  Result.Add(TdxHtmlTagNameID.Heading2,      Heading2KeywordTag);
  Result.Add(TdxHtmlTagNameID.Heading3,      Heading3KeywordTag);
  Result.Add(TdxHtmlTagNameID.Heading4,      Heading4KeywordTag);
  Result.Add(TdxHtmlTagNameID.Heading5,      Heading5KeywordTag);
  Result.Add(TdxHtmlTagNameID.Heading6,      Heading6KeywordTag);
  Result.Add(TdxHtmlTagNameID.SuperScript,   SuperScriptKeywordTag);
  Result.Add(TdxHtmlTagNameID.SubScript,     SubScriptKeywordTag);
  Result.Add(TdxHtmlTagNameID.Center,        CenterKeywordTag);
  Result.Add(TdxHtmlTagNameID.Table,         TableKeywordTag);
  Result.Add(TdxHtmlTagNameID.Tbody,         TBodyKeywordTag);
  Result.Add(TdxHtmlTagNameID.Tfoot,         TFootKeywordTag);
  Result.Add(TdxHtmlTagNameID.Thead,         THeadKeywordTag);

  Result.Add(TdxHtmlTagNameID.TR,            TrKeywordTag);
  Result.Add(TdxHtmlTagNameID.TH,            ThKeywordTag);
  Result.Add(TdxHtmlTagNameID.TD,            TdKeywordTag);
  Result.Add(TdxHtmlTagNameID.LI,            LevelKeywordTag);
  Result.Add(TdxHtmlTagNameID.NumberingList, NumberingListKeywordTag);
  Result.Add(TdxHtmlTagNameID.BulletList,    BulletListKeywordTag);
  Result.Add(TdxHtmlTagNameID.S,             StrikeoutKeywordTag);
  Result.Add(TdxHtmlTagNameID.Strike,        StrikeoutKeywordTag);
  Result.Add(TdxHtmlTagNameID.Code,          CodeKeywordTag);
  Result.Add(TdxHtmlTagNameID.Span,          SpanKeywordTag);
  Result.Add(TdxHtmlTagNameID.Div,           DivisionKeywordTag);
  Result.Add(TdxHtmlTagNameID.Script,        ScriptKeywordTag);
  Result.Add(TdxHtmlTagNameID.Blockquote,    BlockquoteKeywordTag);
  Result.Add(TdxHtmlTagNameID.Caption,       CaptionKeywordTag);
end;

class function TdxHtmlImporter.AbbrKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase;
begin
  Result := TdxAbberTag.Create(AImporter);
end;

class function TdxHtmlImporter.AcronymKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase;
begin
  Result := TdxAcronymTag.Create(AImporter);
end;

class function TdxHtmlImporter.AddressKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase;
begin
  Result := TdxAddressTag.Create(AImporter);
end;

class function TdxHtmlImporter.AreaKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase;
begin
  Result := TdxAreaTag.Create(AImporter);
end;

class function TdxHtmlImporter.BaseFontKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase;
begin
  Result := TdxBaseFontTag.Create(AImporter);
end;

class function TdxHtmlImporter.BdoKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase;
begin
  Result := TdxBdoTag.Create(AImporter);
end;

class function TdxHtmlImporter.BgsoundKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase;
begin
  Result := TdxBgsoundTag.Create(AImporter);
end;

class function TdxHtmlImporter.ButtonKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase;
begin
  Result := TdxButtonTag.Create(AImporter);
end;

class function TdxHtmlImporter.CiteKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase;
begin
  Result := TdxCiteTag.Create(AImporter);
end;

class function TdxHtmlImporter.DdKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase;
begin
  Result := TdxDdTag.Create(AImporter);
end;

class function TdxHtmlImporter.DelKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase;
begin
  Result := TdxDelTag.Create(AImporter);
end;

class function TdxHtmlImporter.DfnKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase;
begin
  Result := TdxDfnTag.Create(AImporter);
end;

class function TdxHtmlImporter.DlKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase;
begin
  Result := TdxDlTag.Create(AImporter);
end;

class function TdxHtmlImporter.DtKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase;
begin
  Result := TdxDtTag.Create(AImporter);
end;

class function TdxHtmlImporter.EmbedKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase;
begin
  Result := TdxEmbedTag.Create(AImporter);
end;

class function TdxHtmlImporter.FieldsetKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase;
begin
  Result := TdxFieldsetTag.Create(AImporter);
end;

class function TdxHtmlImporter.FormKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase;
begin
  Result := TdxFormTag.Create(AImporter);
end;

class function TdxHtmlImporter.FrameKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase;
begin
  Result := TdxFrameTag.Create(AImporter);
end;

class function TdxHtmlImporter.FrameSetKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase;
begin
  Result := TdxFrameSetTag.Create(AImporter);
end;

class function TdxHtmlImporter.HrKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase;
begin
  Result := TdxHrTag.Create(AImporter);
end;

class function TdxHtmlImporter.IframeKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase;
begin
  Result := TdxIframeTag.Create(AImporter);
end;

class function TdxHtmlImporter.InputKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase;
begin
  Result := TdxInputTag.Create(AImporter);
end;

class function TdxHtmlImporter.InsKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase;
begin
  Result := TdxInsTag.Create(AImporter);
end;

class function TdxHtmlImporter.KbdKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase;
begin
  Result := TdxKbdTag.Create(AImporter);
end;

class function TdxHtmlImporter.LabelKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase;
begin
  Result := TdxLabelTag.Create(AImporter);
end;

class function TdxHtmlImporter.LegendKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase;
begin
  Result := TdxLegendTag.Create(AImporter);
end;

class function TdxHtmlImporter.MapKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase;
begin
  Result := TdxMapTag.Create(AImporter);
end;

class function TdxHtmlImporter.MarqueeKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase;
begin
  Result := TdxMarqueeTag.Create(AImporter);
end;

class function TdxHtmlImporter.NobrKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase;
begin
  Result := TdxNobrTag.Create(AImporter);
end;

class function TdxHtmlImporter.NoembedKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase;
begin
  Result := TdxNoembedTag.Create(AImporter);
end;

class function TdxHtmlImporter.NoFramesKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase;
begin
  Result := TdxNoFramesTag.Create(AImporter);
end;

class function TdxHtmlImporter.NoScriptKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase;
begin
  Result := TdxNoScriptTag.Create(AImporter);
end;

class function TdxHtmlImporter.ObjectKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase;
begin
  Result := TdxObjectTag.Create(AImporter);
end;

class function TdxHtmlImporter.OptGroupKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase;
begin
  Result := TdxOptGroupTag.Create(AImporter);
end;

class function TdxHtmlImporter.OptionKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase;
begin
  Result := TdxOptionTag.Create(AImporter);
end;

class function TdxHtmlImporter.ParamKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase;
begin
  Result := TdxParamTag.Create(AImporter);
end;

class function TdxHtmlImporter.QKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase;
begin
  Result := TdxQTag.Create(AImporter);
end;

class function TdxHtmlImporter.SampKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase;
begin
  Result := TdxSampTag.Create(AImporter);
end;

class function TdxHtmlImporter.SelectKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase;
begin
  Result := TdxSelectTag.Create(AImporter);
end;

class function TdxHtmlImporter.TableKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase;
begin
  Result := TdxTableTag.Create(AImporter);
end;

class function TdxHtmlImporter.TdKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase;
begin
  Result := TdxTdTag.Create(AImporter);
end;

class function TdxHtmlImporter.TextAreaKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase;
begin
  Result := TdxTextAreaTag.Create(AImporter);
end;

class function TdxHtmlImporter.ThKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase;
begin
  Result := TdxThTag.Create(AImporter);
end;

class function TdxHtmlImporter.TFootKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase;
begin
  Result := TFootTag.Create(AImporter);
end;

class function TdxHtmlImporter.CaptionKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase;
begin
  Result := TdxCaptionTag.Create(AImporter);
end;

class function TdxHtmlImporter.TBodyKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase;
begin
  Result := TBodyTag.Create(AImporter);
end;

class function TdxHtmlImporter.THeadKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase;
begin
  Result := THeadTag.Create(AImporter);
end;

class function TdxHtmlImporter.TrKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase;
begin
  Result := TdxTrTag.Create(AImporter);
end;

class function TdxHtmlImporter.TtKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase;
begin
  Result := TdxTtTag.Create(AImporter);
end;

class function TdxHtmlImporter.VarKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase;
begin
  Result := TdxVarTag.Create(AImporter);
end;

class function TdxHtmlImporter.WbrKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase;
begin
  Result := TdxWbrTag.Create(AImporter);
end;

class function TdxHtmlImporter.XmpKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase;
begin
  Result := TdxXmpTag.Create(AImporter);
end;

class function TdxHtmlImporter.HtmlKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase;
begin
  Result := TdxHtmlTag.Create(AImporter);
end;

class function TdxHtmlImporter.HeadKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase;
begin
  Result := TdxHeadTag.Create(AImporter);
end;

class function TdxHtmlImporter.BaseKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase;
begin
  Result := TdxBaseTag.Create(AImporter);
end;

class function TdxHtmlImporter.MetaKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase;
begin
  Result := TdxMetaTag.Create(AImporter);
end;

class function TdxHtmlImporter.BodyKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase;
begin
  Result := TdxBodyTag.Create(AImporter);
end;

class function TdxHtmlImporter.TitleKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase;
begin
  Result := TdxTitleTag.Create(AImporter);
end;

class function TdxHtmlImporter.LinkKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase;
begin
  Result := TdxLinkTag.Create(AImporter);
end;

class function TdxHtmlImporter.AnchorKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase;
begin
  Result := TdxAnchorTag.Create(AImporter);
end;

class function TdxHtmlImporter.BoldKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase;
begin
  Result := TdxBoldTag.Create(AImporter);
end;

class function TdxHtmlImporter.ItalicKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase;
begin
  Result := TdxItalicTag.Create(AImporter);
end;

class function TdxHtmlImporter.UnderlineKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase;
begin
  Result := TdxUnderlineTag.Create(AImporter);
end;

class function TdxHtmlImporter.ParagraphKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase;
begin
  Result := TdxParagraphTag.Create(AImporter);
end;

class function TdxHtmlImporter.StrongKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase;
begin
  Result := TdxStrongTag.Create(AImporter);
end;

class function TdxHtmlImporter.BigKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase;
begin
  Result := TdxBigTag.Create(AImporter);
end;

class function TdxHtmlImporter.SmallKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase;
begin
  Result := TdxSmallTag.Create(AImporter);
end;

class function TdxHtmlImporter.PreformattedKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase;
begin
  Result := TdxPreformattedTag.Create(AImporter);
end;

class function TdxHtmlImporter.FontKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase;
begin
  Result := TdxFontTag.Create(AImporter);
end;

class function TdxHtmlImporter.LineBreakKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase;
begin
  Result := TdxLineBreakTag.Create(AImporter);
end;

class function TdxHtmlImporter.EmphasizedKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase;
begin
  Result := TdxEmphasizedTag.Create(AImporter);
end;

class function TdxHtmlImporter.ImageKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase;
begin
  Result := TdxImageTag.Create(AImporter);
end;

class function TdxHtmlImporter.SubScriptKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase;
begin
  Result := TdxSubScriptTag.Create(AImporter);
end;

class function TdxHtmlImporter.SuperScriptKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase;
begin
  Result := TdxSuperScriptTag.Create(AImporter);
end;

class function TdxHtmlImporter.CenterKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase;
begin
  Result := TdxCenterTag.Create(AImporter);
end;

class function TdxHtmlImporter.StrikeoutKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase;
begin
  Result := TdxStrikeoutTag.Create(AImporter);
end;

class function TdxHtmlImporter.Heading1KeywordTag(AImporter: TdxHtmlImporter): TdxTagBase;
begin
  Result := TdxHeadingTag.Create(AImporter, 6, 1);
end;

class function TdxHtmlImporter.Heading2KeywordTag(AImporter: TdxHtmlImporter): TdxTagBase;
begin
  Result := TdxHeadingTag.Create(AImporter, 5, 2);
end;

class function TdxHtmlImporter.Heading3KeywordTag(AImporter: TdxHtmlImporter): TdxTagBase;
begin
  Result := TdxHeadingTag.Create(AImporter, 4, 3);
end;

class function TdxHtmlImporter.Heading4KeywordTag(AImporter: TdxHtmlImporter): TdxTagBase;
begin
  Result := TdxHeadingTag.Create(AImporter, 3, 4);
end;

class function TdxHtmlImporter.Heading5KeywordTag(AImporter: TdxHtmlImporter): TdxTagBase;
begin
  Result := TdxHeadingTag.Create(AImporter, 2, 5);
end;

class function TdxHtmlImporter.Heading6KeywordTag(AImporter: TdxHtmlImporter): TdxTagBase;
begin
  Result := TdxHeadingTag.Create(AImporter, 1, 6);
end;

class function TdxHtmlImporter.NumberingListKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase;
begin
  Result := TdxNumberingListTag.Create(AImporter);
end;

class function TdxHtmlImporter.BulletListKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase;
begin
  Result := TdxBulletListTag.Create(AImporter);
end;

class function TdxHtmlImporter.LevelKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase;
begin
  Result := TdxLevelTag.Create(AImporter);
end;

class function TdxHtmlImporter.CodeKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase;
begin
  Result := TdxCodeTag.Create(AImporter);
end;

class function TdxHtmlImporter.SpanKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase;
begin
  Result := TdxSpanTag.Create(AImporter);
end;

class function TdxHtmlImporter.DivisionKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase;
begin
  Result := TdxDivisionTag.Create(AImporter);
end;

class function TdxHtmlImporter.ScriptKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase;
begin
  Result := TdxScriptTag.Create(AImporter);
end;

class function TdxHtmlImporter.BlockquoteKeywordTag(AImporter: TdxHtmlImporter): TdxTagBase;
begin
  Result := TdxBlockquoteTag.Create(AImporter);
end;

function TdxHtmlImporter.GetHtmlKeywordTable: TdxHtmlKeywordTranslatorTable;
begin
  Result := FHtmlKeywordTable;
end;

function TdxHtmlImporter.GetSpecialSymbolTable: TdxHtmlSpecialSymbolTable;
begin
  Result := FSpecialSymbolTable;
end;

procedure TdxHtmlImporter.SetIsEmptyParagraph(const AValue: Boolean);
begin
  FIsEmptyParagraph := AValue;
  if not AValue then
    IsEmptyListItem := False;
end;

procedure TdxHtmlImporter.SetProcessHyperlink(const Value: TdxImportFieldInfo);
begin
  if FProcessHyperlink = Value then
    Exit;
  FProcessHyperlink.Free;
  FProcessHyperlink := Value;
end;

procedure TdxHtmlImporter.SetBaseUri(const AValue: string);
begin
  FBaseUri := TdxUri.UnescapeDataString(AValue);
end;

function TdxHtmlImporter.GetCodePage: Integer;
begin
  Result := Encoding.CodePage;
end;

procedure TdxHtmlImporter.SetCodePage(const AValue: Integer);
begin
  if CodePage = AValue then
    Exit;
  FEncoding := TdxEncoding.GetEncoding(AValue);
end;

procedure TdxHtmlImporter.SetElement(const Value: TdxHtmlElement);
begin
  FElement.Value := Value;
end;

function TdxHtmlImporter.GetOptions: TdxHtmlDocumentImporterOptions;
begin
  Result := TdxHtmlDocumentImporterOptions(inherited Options);
end;

function TdxHtmlImporter.GetIgnoredTagIsOpen: Boolean;
begin
  Result := (TagsStack.Count > 0) and TagsStack[TagsStack.Count - 1].Tag.ShouldBeIgnored;
end;

procedure TdxHtmlImporter.Import(AStream: TStream);
begin
  Import(AStream, True);
end;

procedure TdxHtmlImporter.Import(AStream: TStream; ALeaveOpen: Boolean);
var
  AService: IdxUriStreamService;
  APosition: TdxHtmlInputPosition;
  AOptions: TdxFieldUpdateOnLoadOptions;
  ADataStringUriStreamProvider: IdxUriStreamProvider;
begin
  AService := DocumentModel.GetService<IdxUriStreamService>;
  DocumentModel.BeginSetContent;
  try
    if AService <> nil then
    begin
      ADataStringUriStreamProvider := TdxDataStringUriStreamProvider.Create;
      AService.RegisterProvider(ADataStringUriStreamProvider);
    end;
    SetDefaultDocumentProperties;
    try
      APosition := TdxHtmlInputPosition.Create(PieceTable);
      try
        ImportCore(AStream, APosition);
      finally
        APosition.Free;
      end;
    except
      DocumentModel.ClearDocument;
      raise;
    end
  finally
    AOptions := Options.UpdateField.GetNativeOptions;
    try
      DocumentModel.EndSetContent(TdxDocumentModelChangeType.LoadNewDocument, True, AOptions);
      if AService <> nil then
        AService.UnRegisterProvider(ADataStringUriStreamProvider);
    finally
      AOptions.Free;
    end;
  end;
end;

function TdxHtmlImporter.GetAbsoluteBaseUri(const ABaseUri: string): string;
var
  AResult: TdxUri;
begin
  if TdxUri.TryCreateAbsoluteUri(ABaseUri, AResult) then
    Exit(ABaseUri)
  else
  begin
    try
      if ABaseUri <> '' then
        Result := TPath.GetFullPath(ABaseUri)
      else
        Exit(ABaseUri);
    except
      Result := ABaseUri;
    end;
  end;
end;

procedure TdxHtmlImporter.ImportCore(AStream: TStream; APos: TdxHtmlInputPosition);
var
  AHtmlElements: TdxHtmlElementList;
  AStreamReader: TStreamReader;
  APieceTable: TdxPieceTable;
  ATables: TdxFastList;
  I: Integer;
begin
  FStyleTagCollection.Clear;
  FEmptyHyperlinks.Free;
  FEmptyHyperlinks := TdxFieldList.Create;
  AHtmlElements := TdxHtmlElementList.Create;
  try
    FPosition := APos;
    ClearTagStack;

    AStreamReader := TStreamReader.Create(AStream, GetDefaultEncoding(AStream), False, 256);
    try
      AStreamReader.Peek;
      if AStreamReader.CurrentEncoding <> TdxEncoding.Empty then
        FSuppressEncoding := True;
      ParseHtmlContent(AHtmlElements, AStreamReader);
    finally
      AStreamReader.Free;
    end;
    SortSelectors;

    PieceTable.InsertParagraph(PieceTable.DocumentEndLogPosition);
    ElementsImport(AHtmlElements);
    ProcessRemainTags;

    FixLastParagraph;

    ATables := DocumentModel.GetPieceTables(False);
    try
      for I := 0 to ATables.Count - 1 do
      begin
        APieceTable := TdxPieceTable(ATables[I]);
        FixBordersAndParagraphBetweenTables(APieceTable);
      end;
    finally
      ATables.Free;
    end;
    DocumentModel.NormalizeZOrder;
  finally
    AHtmlElements.Free;
  end;
end;

procedure TdxHtmlImporter.RemoveEmptyHyperlinks;
var
  I: Integer;
  AEmptyHyperlink: TdxField;
  AStart, AEnd: TdxDocumentLogPosition;
begin
  for I := FEmptyHyperlinks.Count - 1 downto 0 do
  begin
    AEmptyHyperlink := FEmptyHyperlinks[I];
    AStart := PieceTable.GetRunLogPosition(AEmptyHyperlink.FirstRunIndex);
    AEnd := PieceTable.GetRunLogPosition(AEmptyHyperlink.LastRunIndex);
    PieceTable.DeleteContent(AStart, AEnd - AStart + 1, False);
  end;
end;

procedure TdxHtmlImporter.SortSelectors;
var
  ACount, I: Integer;
  ACssElementComparsion: TComparison<TdxCssElement>;
begin
  ACount := FStyleTagCollection.Count;
  for I := 0 to ACount - 1 do
    FStyleTagCollection[I].Selector.InvalidateSpecifity;
  ACssElementComparsion :=
    function (const AElement1, AElement2: TdxCssElement): Integer
    var
      ASpecifity1, ASpecifity2: Integer;
    begin
      ASpecifity1 := AElement1.Selector.Specifity;
      ASpecifity2 := AElement2.Selector.Specifity;
      if ASpecifity1 <> ASpecifity2 then
        Result := ASpecifity1 - ASpecifity2
      else
        Result := AElement1.Index - AElement2.Index;
    end;
  FStyleTagCollection.Sort(TComparer<TdxCssElement>.Construct(ACssElementComparsion));
end;

procedure TdxHtmlImporter.ParseHtmlContent(AHtmlElements: TdxHtmlElementList; AStreamReader: TStreamReader);
var
  ARawText: string;
  ADetectedEncoding: TEncoding;
begin
  Assert(AStreamReader <> nil);
  ARawText := '';
  Element := FParser.ParseNext(AStreamReader);
  while Element <> nil do
  begin
    if FStyleTagIsOpen then
      ARawText := StyleTagIsOpen(ARawText)
    else
      StyleTagIsNotOpen(AStreamReader, AHtmlElements);
    Element := FParser.ParseNext(AStreamReader);
  end;

  if Options.AutoDetectEncoding and Options.IsDefaultEncoding then
  begin
    ADetectedEncoding := FParser.DetectEncoding;
    if ADetectedEncoding <> nil then
      FEncoding := ADetectedEncoding;
  end;
end;

procedure TdxHtmlImporter.FixBordersAndParagraphBetweenTables(APieceTable: TdxPieceTable);
var
  ANestedLevel: Integer;
  ATables: TdxList<TdxTable>;
begin
  ANestedLevel := 0;
  while True do
  begin
    ATables := GetTablesByLevel(APieceTable, ANestedLevel);
    try
      if ATables.Count <= 0 then
        Exit;
      ATables.Sort(TComparer<TdxTable>.Construct(
        function (const ALeft, ARight: TdxTable): Integer
        begin
          Result := TableComparer(ALeft, ARight);
        end));
      FixBordersAndParagraphBetweenTablesCore(ATables);
      Inc(ANestedLevel);
    finally
      ATables.Free;
    end;
  end;
end;

procedure TdxHtmlImporter.FixBordersAndParagraphBetweenTablesCore(ATables: TdxList<TdxTable>);
var
  ACount, I: Integer;
begin
  ACount := ATables.Count;
  for I := 0 to ACount - 1 - 1 do
  begin
    if ATables[I].EndParagraphIndex + 1 = ATables[I + 1].StartParagraphIndex then
      InsertParagraphBeforeTable(ATables[I + 1]);
    ResolveBorderConflicts(ATables[I]);
  end;
  ResolveBorderConflicts(ATables[ACount - 1]);
end;

procedure TdxHtmlImporter.ResolveBorderConflicts(ATable: TdxTable);
var
  ARowCount, I, ACellCount, J: Integer;
  ATableBorders: TdxTableBorders;
  ARow: TdxTableRow;
  ACells: TdxTableCellCollection;
  AFirstCell, ALastCell: TdxTableCell;
  ACellBorders: TdxTableCellBorders;
begin
  if ATable.CellSpacing.Value > 0 then
    Exit;
  ARowCount := ATable.Rows.Count;
  ATableBorders := ATable.TableProperties.Borders;
  for I := 0 to ARowCount - 1 do
  begin
    ARow := ATable.Rows[I];
    ACells := ARow.Cells;
    AFirstCell := ACells.First;
    ACellBorders := AFirstCell.Properties.Borders;
    if ShouldResetTableCellBorder(ATableBorders.UseLeftBorder, ACellBorders.UseLeftBorder, ATableBorders.LeftBorder,
       ACellBorders.LeftBorder) then
    begin
      ACellBorders.LeftBorder.ResetBorder;
    end;
    ALastCell := ACells.Last;
    ACellBorders := ALastCell.Properties.Borders;
    if ShouldResetTableCellBorder(ATableBorders.UseRightBorder, ACellBorders.UseRightBorder, ATableBorders.RightBorder,
      ACellBorders.RightBorder) then
    begin
      ACellBorders.RightBorder.ResetBorder;
    end;
    if I = 0 then
    begin
      ACellCount := ACells.Count;
      for J := 0 to ACellCount - 1 do
      begin
        ACellBorders := ACells[J].Properties.Borders;
        if ShouldResetTableCellBorder(ATableBorders.UseTopBorder, ACellBorders.UseTopBorder, ATableBorders.TopBorder,
          ACellBorders.TopBorder) then
        begin
          ACellBorders.TopBorder.ResetBorder;
        end;
      end;
    end;
    if I = ARowCount - 1 then
    begin
      ACellCount := ACells.Count;
      for J := 0 to ACellCount - 1 do
      begin
        ACellBorders := ACells[J].Properties.Borders;
        if ShouldResetTableCellBorder(ATableBorders.UseBottomBorder, ACellBorders.UseBottomBorder,
          ATableBorders.BottomBorder, ACellBorders.BottomBorder) then
        begin
          ACellBorders.BottomBorder.ResetBorder;
        end;
      end;
    end;
  end;
end;

function TdxHtmlImporter.ShouldResetTableCellBorder(AUseTableBorder: Boolean; AUseCellBorder: Boolean;
  ATableBorder: TdxBorderBase; ACellBorder: TdxBorderBase): Boolean;
var
  AHasTableBorder, AHasCellBorder: Boolean;
begin
  if not AUseTableBorder or not AUseCellBorder then
    Exit(False);
  AHasTableBorder := (ATableBorder.Style <> TdxBorderLineStyle.None) and (ATableBorder.Style <> TdxBorderLineStyle.Nil);
  AHasCellBorder := (ACellBorder.Style <> TdxBorderLineStyle.None) and (ACellBorder.Style <> TdxBorderLineStyle.Nil);
  Result := not AHasCellBorder and AHasTableBorder;
end;

procedure TdxHtmlImporter.InsertParagraphBeforeTable(ATable: TdxTable);
var
  APieceTable: TdxPieceTable;
  AParagraph, ANewParagraph: TdxParagraph;
  AParagraphCell: TdxTableCell;
  AParagraphMarkPos: TdxDocumentModelPosition;
begin
  APieceTable := TdxPieceTable(ATable.PieceTable);
  AParagraph := APieceTable.Paragraphs[ATable.StartParagraphIndex];
  AParagraphCell := AParagraph.GetCell;
  ANewParagraph := APieceTable.InsertParagraph(AParagraph.LogPosition);
  ShiftBookmarks(ANewParagraph.LogPosition);

  while (AParagraphCell <> nil) and (AParagraphCell.Table.NestedLevel >= ATable.NestedLevel) do
  begin
    APieceTable.ChangeCellStartParagraphIndex(AParagraphCell, ANewParagraph.Index + 1);
    AParagraphCell := AParagraphCell.Table.ParentCell;
  end;
  if ANewParagraph.IsInList then
    APieceTable.RemoveNumberingFromParagraph(ANewParagraph);
  ANewParagraph.ParagraphProperties.ResetAllUse;

  AParagraphMarkPos := TdxDocumentModelPosition.FromParagraphStart(PieceTable, ANewParagraph.Index);
  APieceTable.Runs[AParagraphMarkPos.RunIndex].CharacterProperties.ResetAllUse;
  APieceTable.Runs[AParagraphMarkPos.RunIndex].Hidden := True;
end;

procedure TdxHtmlImporter.ShiftBookmarks(ALogPosition: TdxDocumentLogPosition);
var
  ACount, I: Integer;
  ABookmark: TdxBookmark;
begin
  ACount := PieceTable.Bookmarks.Count;
  for I := 0 to ACount - 1 do
  begin
    ABookmark := PieceTable.Bookmarks[I];
    if ALogPosition <= ABookmark.Start then
      ABookmark.Interval.Start.LogPosition := ABookmark.Interval.Start.LogPosition + 1;
    if ALogPosition <= ABookmark.&End then
      ABookmark.Interval.&End.LogPosition := ABookmark.Interval.&End.LogPosition + 1;
  end;
end;

class function TdxHtmlImporter.TableComparer(const ALeft, ARight: TdxTable): Integer;
begin
  Result := ALeft.StartParagraphIndex - ARight.StartParagraphIndex;
end;

function TdxHtmlImporter.GetTablesByLevel(APieceTable: TdxPieceTable; ANestedLevel: Integer): TdxList<TdxTable>;
var
  ATables: TdxTableCollection;
  ACount, I: Integer;
begin
  Result := TdxList<TdxTable>.Create;
  ATables := APieceTable.Tables;
  ACount := ATables.Count;
  for I := 0 to ACount - 1 do
  begin
    if ATables[I].NestedLevel = ANestedLevel then
      Result.Add(ATables[I]);
  end;
end;


procedure TdxHtmlImporter.ProcessRemainTags;
var
  ACount, I: Integer;
  ATablesAllowed: Boolean;
begin
  ACount := TagsStack.Count;
  if ACount = 0 then
    Exit;
  ATablesAllowed := DocumentModel.DocumentCapabilities.TablesAllowed;
  for I := ACount - 1 downto 0 do
  begin
    ProcessAnchorTag(TagsStack[I].Tag);
    if ATablesAllowed then
      ProcessUnclosedTableTags(I);
  end;
end;

procedure TdxHtmlImporter.ProcessUnclosedTableTags(AIndex: Integer);
var
  ATag: TdxTagBase;
begin
  ATag := TagsStack[AIndex].Tag;
  if (ATag is TdxTrTag) or (ATag is TdxTdTag) or (ATag is TdxThTag) then
    ATag.BeforeDeleteTagFromStack(AIndex)
  else
    if ATag is TdxTableTag then
    begin
      ATag.BeforeDeleteTagFromStack(AIndex);

      SetAppendObjectProperty;
      ATag.ParagraphFunctionalProcess;
    end;
end;

procedure TdxHtmlImporter.ProcessAnchorTag(AHtmlTag: TdxTagBase);
begin
  if AHtmlTag is TdxAnchorTag then
    AHtmlTag.BeforeDeleteTagFromStack(-1);
end;

procedure TdxHtmlImporter.ClearTagStack;
begin
  TagsStack.Clear;
end;

function TdxHtmlImporter.AddTagToStack(ATag: TdxOpenHtmlTag): Boolean;
begin
  if ATag.Tag is TdxPreformattedTag then
    FHasPreformattedTagInStack := True;
  Result := ATag.Tag.CanAppendToTagStack;
  if Result then
    TagsStack.Add(ATag);
end;

procedure TdxHtmlImporter.RemoveTagFromStack(AIndex: Integer);
var
  ARecalculateHasPreformattedTagInStack: Boolean;
  I: Integer;
begin
  ARecalculateHasPreformattedTagInStack := (FHasPreformattedTagInStack and (TagsStack[AIndex].Tag is TdxPreformattedTag));
  TagsStack.Delete(AIndex);

  if ARecalculateHasPreformattedTagInStack then
  begin
    FHasPreformattedTagInStack := False;
    for I := TagsStack.Count - 1 downto 0 do
    begin
      if TagsStack[I].Tag is TdxPreformattedTag then
      begin
        FHasPreformattedTagInStack := True;
        Break;
      end;
    end;
  end;
end;

procedure TdxHtmlImporter.StyleTagIsNotOpen(AStreamReader: TStreamReader; AHtmlElements: TdxHtmlElementList);
var
  AHtmlTag: TdxTag;
  AScriptContent, ACloseScriptTag: TdxHtmlElement;
begin
  if (Element.ElementType in [TdxHtmlElementType.OpenTag, TdxHtmlElementType.EmptyTag]) then
  begin
    AHtmlTag := TdxTag(FElement);
    if AHtmlTag.NameID = TdxHtmlTagNameID.Style then
    begin
      FStyleTagIsOpen := True;
      Exit;
    end
    else
      if (Element.ElementType = TdxHtmlElementType.OpenTag) and (AHtmlTag.NameID = TdxHtmlTagNameID.Script) then
      begin
        FParser.ParseNextScript(AStreamReader, AScriptContent, ACloseScriptTag);
        if ACloseScriptTag <> nil then
          AHtmlElements.Add(ACloseScriptTag);
        Exit;
      end;
  end;
  AHtmlElements.Add(FElement);
end;

function TdxHtmlImporter.StyleTagIsOpen(const ARawText: string): string;
var
  AHtmlTag: TdxTag;
  S: string;
begin
  if Element.ElementType = TdxHtmlElementType.CloseTag then
  begin
    AHtmlTag := TdxTag(FElement);
    if AHtmlTag.NameID = TdxHtmlTagNameID.Style then
    begin
      FStyleTagIsOpen := False;
      ParseCssElementCollection(ARawText);
      Exit('');
    end;
  end;

  if Element.ElementType = TdxHtmlElementType.Comment then
    S := ARawText + TdxComment(FElement).CommentText
  else
    S := ARawText + Element.RawText;
  Result := DecodeStringContent(S);
end;

procedure TdxHtmlImporter.ParseCssElementCollection(const ARawText: string);
var
  AReader: TStringReader;
begin
  if ARawText = '' then
    Exit;
  AReader := TStringReader.Create(ARawText);
  try
    ParseCssElementCollection(AReader);
  finally
    AReader.Free;
  end;
end;

procedure TdxHtmlImporter.ParseCssElementCollection(AReader: TTextReader);
var
  ACssParser: TdxCssParser;
  ACssElements: TdxCssElementCollection;
begin
  if AReader = nil then
    Exit;
  ACssParser := TdxCssParser.Create(DocumentModel);
  try
    try
      ACssElements := ACssParser.Parse(AReader);
      if ACssElements <> nil then
        FStyleTagCollection.AddRange(ACssElements);
    except
    end;
  finally
    ACssParser.Free;
  end;
end;

procedure TdxHtmlImporter.ElementsImport(AHtmlElements: TdxHtmlElementList);
var
  AHtmlTagPresent: Boolean;
  AHtmlCorrector: TdxHtmlCorrector;
  ACount, I: Integer;
begin
  AHtmlCorrector := TdxHtmlCorrector.Create;
  try
    AHtmlElements := AHtmlCorrector.GetCorrectedHtmlElements(AHtmlElements, AHtmlTagPresent);
    try
      ACount := AHtmlElements.Count;

      ProgressIndication.&Begin(cxGetResourceString(@sdxRichEditMsg_Loading), 0, ACount, 0);
      try
        if not AHtmlTagPresent then
        begin
          Element := TdxTag.Create(TdxHtmlElementType.OpenTag);
          TdxTag(FElement).NameID := TdxHtmlTagNameID.Html;
          ElementsImportCore;
        end;
        for I := 0 to ACount - 1 do
        begin
          Element := AHtmlElements[I];
          ElementsImportCore;
          ProgressIndication.SetProgress(I);
        end;
      finally
        ProgressIndication.&End;
      end;
    finally
      AHtmlElements.Free;
    end;
  finally
    AHtmlCorrector.Free;
  end;
end;

procedure TdxHtmlImporter.ElementsImportCore;
var
  ACommentText: string;
  ALogPosition: TdxDocumentLogPosition;
  APrevParIndex: TdxParagraphIndex;
  ABm: TdxBookmark;
begin
  case Element.ElementType of
    TdxHtmlElementType.Content:
      ProcessContentText;
    TdxHtmlElementType.Comment:
      begin
        ACommentText := TdxComment(FElement).CommentText;
        if FMarkTable.ContainsKey(ACommentText) then
        begin
          ALogPosition := Position.LogPosition;
          if IsEmptyParagraph then
          begin
            APrevParIndex := Position.ParagraphIndex - 1;
            if (APrevParIndex >= 0) and (PieceTable.Paragraphs[APrevParIndex].GetCell = nil) then
              Dec(ALogPosition);
          end;

          ABm := TdxBookmark.Create(Position.PieceTable, ALogPosition, ALogPosition);
          ABm.Name := '_dx_frag_' + ACommentText;
          PieceTable.Bookmarks.Add(ABm);
          FMarkTable[ACommentText] := ABm;
        end;
      end;
    else
      FindKeywordInTagTable;
  end;
end;

procedure TdxHtmlImporter.FixLastParagraph;
var
  AParagraphs: TdxParagraphCollection;
  ARuns: TdxTextRunCollection;
  ASections: TdxSectionCollection;
  I, ACount: Integer;
  ALastParagraph: TdxParagraph;
  ALastParagraphIndex: TdxParagraphIndex;
  ALastSection: TdxSection;
  ASectionIndex: TdxSectionIndex;
  ABookmarks: TdxBookmarkCollection;
  AEndLogPosition: TdxDocumentLogPosition;
  ALastPosition: TdxDocumentModelPosition;
  ABookmark: TdxBookmark;
  AParagraph: TdxParagraph;
begin
  AParagraphs := PieceTable.Paragraphs;

  ARuns := PieceTable.Runs;
  ASections := DocumentModel.Sections;

  for I := 0 to 1 do
  begin
    ALastParagraph := AParagraphs.Last;
    if (ALastParagraph.IsEmpty and (ALastParagraph.Index > 0)) and (AParagraphs[ALastParagraph.Index - 1].GetCell = nil) then
    begin
      ALastParagraphIndex := ALastParagraph.Index;
      AParagraph := PieceTable.Paragraphs[ALastParagraphIndex];
      AParagraphs.RemoveAt(ALastParagraphIndex);
      AParagraph.Free;
      ARuns.Delete(ARuns.Count - 1);

      ALastSection := ASections.Last;
      if ALastSection.FirstParagraphIndex = ALastParagraphIndex then
      begin
        ASectionIndex := ASections.Count - 1;
        ASections[ASectionIndex].UnsubscribeHeadersFootersEvents;
        ASections.Delete(ASectionIndex);
      end
      else
        ALastSection.LastParagraphIndex := ALastSection.LastParagraphIndex - 1;
    end
    else
      ARuns.Last.CharacterProperties.CopyFrom(Position.CharacterFormatting);
  end;
  ABookmarks := PieceTable.Bookmarks;
  AEndLogPosition := PieceTable.DocumentEndLogPosition;
  ACount := ABookmarks.Count;
  ALastPosition := TdxPositionConverter.ToDocumentModelPosition(PieceTable, AEndLogPosition);
  for I := 0 to ACount - 1 do
  begin
    ABookmark := ABookmarks[I];
    if ABookmark.Start > AEndLogPosition then
      ABookmark.SetStartCore(ALastPosition);
    if ABookmark.&End > AEndLogPosition then
      ABookmark.SetEndCore(ALastPosition);
  end;
end;

procedure TdxHtmlImporter.FindKeywordInTagTable;
var
  AHtmlTag: TdxTag;
  ATranslator: TdxHtmlTranslateKeywordHandler;
begin
  AHtmlTag := TdxTag(FElement);
  ATranslator := nil;
  if AHtmlTag.NameID <> TdxHtmlTagNameID.Unknown then
    HtmlKeywordTable.TryGetValue(AHtmlTag.NameID, ATranslator);
  if Assigned(ATranslator) then
  begin
    FTag := ATranslator(Self);
    FTagCollection.Add(FTag);
    ProcessTag(FTag);
  end;
end;

procedure TdxHtmlImporter.ProcessContentText;
var
  AContentText: string;
begin
  if FTag = nil then
    Exit;

  AContentText := GetContentText;
  if AContentText = '' then
    Exit;

  FTag.AppendContent(AContentText, UseRawContent);
  FCanInsertSpace := not TdxStringHelper.EndsWith(AContentText, ' ');
end;

function TdxHtmlImporter.GetContentText: string;
var
  AContent: TdxContent;
  AContentText: string;
begin
  if IgnoredTagIsOpen then
    Exit('');

  AContent := TdxContent(FElement);

  if UseRawContent then
    AContentText := AContent.RawText
  else
  begin
    AContentText := AContent.ContentText;
    if (AContentText <> '') and WhiteSpaceAtStartCanRemove(AContentText) then
      Delete(AContentText, 1, 1);
  end;

  Result := DecodeStringContent(AContentText);
end;

function TdxHtmlImporter.GetDefaultEncoding(AStream: TStream): TEncoding;
var
  ABuffer: TBytes;
  ASize, AOffset: Int64;
begin
  ASize := AStream.Size - AStream.Position;
  if ASize < 2 then
    Exit(TdxEncoding.Empty);
  ASize := Min(ASize, 3);
  SetLength(ABuffer, ASize);
  AStream.ReadBuffer(ABuffer[0], ASize);
  Result := nil;
  AOffset := TEncoding.GetBufferEncoding(ABuffer, Result, TdxEncoding.Empty);
  AStream.Seek(AOffset - ASize, TSeekOrigin.soCurrent);
end;

function TdxHtmlImporter.GetElement: TdxHtmlElement;
begin
  Result := FElement.Value;
end;

function TdxHtmlImporter.DecodeStringContent(const AContentText: string): string;
begin
  if AContentText = '' then
    Exit('');
  if not FSuppressEncoding then
    Result := ReplaceSpecialSymbols(TdxHtmlCodePageDecoder.ApplyEncoding(AContentText, Encoding))
  else
    Result := ReplaceSpecialSymbols(AContentText);
end;

function TdxHtmlImporter.UseRawContent: Boolean;
begin
  Result := FHasPreformattedTagInStack and  not TablesImportHelper.IsInTable;
end;

function TdxHtmlImporter.ReplaceSpecialSymbols(const AContentText: string): string;
var
  I: Integer;
  AContentTexts: TArray<string>;
  AReplacedString: string;
  AResult: TStringBuilder;
begin
  if Length(AContentText) = 0 then
    Exit('');
  AContentTexts := TdxStringHelper.Split(AContentText, ['&']);
  AResult := TStringBuilder.Create(AContentTexts[0], Length(AContentText));
  try
    for I := 1 to Length(AContentTexts) - 1 do
    begin
      if AContentTexts[I] <> '' then
      begin
        AReplacedString := ReplaceSpecialSymbolCore(AContentTexts[I]);
        if AContentTexts[I] = AReplacedString then
          AResult.Append('&');
        AResult.Append(AReplacedString);
      end;
    end;
    Result := StringReplace(AResult.ToString, TdxCharacters.OptionalHyphen, '', [rfReplaceAll]);
  finally
    AResult.Free;
  end;
end;

function TdxHtmlImporter.ReplaceSpecialSymbolCore(const ARawText: string): string;
var
  ASpecialSymbol, AResult: TStringBuilder;
  AIsNumeric: Boolean;
  I: Integer;
  ASpecialValue: Char;
begin
  AResult := TStringBuilder.Create;
  try
    ASpecialSymbol := TStringBuilder.Create;
    try
      AIsNumeric := ARawText[1] = '#';
      if AIsNumeric then
        AResult.Append(ARawText, 1, Length(ARawText) - 1)
      else
        AResult.Append(ARawText);

      I := 0;
      while (I < AResult.Length) and not IsSeparator(AResult.Chars[I]) do
      begin
        ASpecialSymbol.Append(AResult.Chars[I]);
        Inc(I);
      end;

      if (I < AResult.Length) and (AResult.Chars[I] = ';') then
        AResult.Remove(I, 1);

      ASpecialValue := GetSpecialValue(ASpecialSymbol.ToString, AIsNumeric);
      if ASpecialValue <> #$0000 then
      begin
        AResult.Remove(0, I);
        AResult.Insert(0, ASpecialValue);
      end;
      Result := AResult.ToString;
    finally
      ASpecialSymbol.Free;
    end;
  finally
    AResult.Free;
  end;
end;

function TdxHtmlImporter.GetSpecialValue(const ASpecialSymbol: string; AIsNumeric: Boolean): Char;
var
  ASpecialValue: Char;
begin
  ASpecialValue := #$0000;
  if AIsNumeric then
    ASpecialValue := GetUnicodeSymbol(ASpecialSymbol)
  else
    SpecialSymbolTable.TryGetValue(ASpecialSymbol, ASpecialValue);
  Result := ASpecialValue;
end;

function TdxHtmlImporter.GetUnicodeSymbol(const ASpecialSymbol: string): Char;
var
  ACode: Integer;
  AEncoding: TEncoding;
  AResult: string;
  ACharBuffer: TArray<Byte>;
begin
  if not TryParseSymbol(ASpecialSymbol, ACode) then
    Exit(#$0000);

  if ACode <= $FF then
  begin
    AEncoding := TdxEncoding.GetEncoding(1252);
    SetLength(ACharBuffer, 1);
    ACharBuffer[0] := Byte(ACode);
    AResult := AEncoding.GetString(ACharBuffer);
    if AResult <> '' then
      Exit(AResult[1]);
  end;

  Result := Char(ACode);
end;

function TdxHtmlImporter.TryParseSymbol(const ASpecialSymbol: string; out ACode: Integer): Boolean;
begin
  if ASpecialSymbol = '' then
    Exit(False);

  if CharInSet(ASpecialSymbol[1], ['x', 'X']) then
    Result := TdxNumber.TryParse(Copy(ASpecialSymbol, 2, Length(ASpecialSymbol) - 1), TdxNumberStyles.HexNumber, ACode)
  else
    Result := TdxNumber.TryParse(ASpecialSymbol, ACode);
end;

function TdxHtmlImporter.IsSeparator(ACh: Char): Boolean;
begin
{$IFDEF DELPHIXE4}
  Result := ACh.IsWhiteSpace or ACh.IsPunctuation or ACh.IsSymbol;
{$ELSE}
  Result := TCharacter.IsWhiteSpace(ACh) or TCharacter.IsPunctuation(ACh) or TCharacter.IsSymbol(ACh);
{$ENDIF}
end;

function TdxHtmlImporter.WhiteSpaceAtStartCanRemove(const AContentText: string): Boolean;
begin
{$IFDEF DELPHIXE4}
  Result := AContentText[1].IsWhiteSpace and (IsEmptyParagraph or IsEmptyLine or not FCanInsertSpace);
{$ELSE}
  Result := TCharacter.IsWhiteSpace(AContentText[1]) and (IsEmptyParagraph or IsEmptyLine or not FCanInsertSpace);
{$ENDIF}
end;

procedure TdxHtmlImporter.ProcessTag(ATag: TdxTagBase);
begin
  ATag.DeleteOldOpenTag;
  if Element.ElementType = TdxHtmlElementType.OpenTag then
    OpenProcess(ATag)
  else
    if Element.ElementType = TdxHtmlElementType.CloseTag then
      CloseProcess(ATag)
    else
      EmptyProcess(ATag);
end;

procedure TdxHtmlImporter.OpenProcess(ATag: TdxTagBase);
var
  AOpenTag: TdxOpenHtmlTag;
  ANewTag: TdxTagBase;
begin
  if IgnoredTagIsOpen then
    Exit;
  if (ATag is TdxParagraphTag) and (LastOpenParagraphTagIndex >= 0) then
  begin
    ANewTag := TdxParagraphTag.Create(Self);
    try
      OpenTagIsFoundAndRemoved(ANewTag);
    finally
      ANewTag.Free;
    end;
  end
  else
    if (ATag is TdxAnchorTag) and (LastOpenAnchorTagIndex >= 0) then
    begin
      ANewTag := TdxAnchorTag.Create(Self);
      try
        OpenTagIsFoundAndRemoved(ANewTag);
      finally
        ANewTag.Free;
      end;
    end;

  AOpenTag := TdxOpenHtmlTag.Create(ATag, PieceTable);
  AOpenTag.OldPosition.CopyFrom(Position);
  if not AddTagToStack(AOpenTag) then
    AOpenTag.Free;
  ATag.OpenTagProcess;
end;

procedure TdxHtmlImporter.CloseProcess(ATag: TdxTagBase);
begin
  if OpenTagIsFoundAndRemoved(ATag) then
    Exit;
  ATag.FunctionalTagProcess;
end;

function TdxHtmlImporter.OpenTagIsFoundAndRemoved(ATag: TdxTagBase): Boolean;
var
  AStartIndex, ACount, I: Integer;
begin
  AStartIndex := ATag.GetStartIndexAllowedSearchScope;
  ACount := TagsStack.Count;
  for I := ACount - 1 downto AStartIndex do
  begin
    if TagsStack[I].Tag.Name = ATag.Name then
    begin
      CloseUnClosedTag(ATag, I);
      Exit(True);
    end;
  end;
  Result := False;
end;

procedure TdxHtmlImporter.CloseUnClosedTag(ATag: TdxTagBase; AIndex: Integer);
begin
  ATag.BeforeDeleteTagFromStack(AIndex);
  Position.CopyFrom(TagsStack[AIndex].OldPosition);
  DeleteOpenTagFromStack(AIndex);
  ATag.FunctionalTagProcess;
  ApplyProperties(AIndex);
end;

procedure TdxHtmlImporter.DeleteOpenTagFromStack(AIndex: Integer);
begin
  if AIndex < FLastOpenParagraphTagIndex then
    Dec(FLastOpenParagraphTagIndex);
  if AIndex < FLastOpenAnchorTagIndex then
    Dec(FLastOpenAnchorTagIndex);
  RemoveTagFromStack(AIndex);
end;

procedure TdxHtmlImporter.ApplyProperties(AIndex: Integer);
var
  I: Integer;
begin
  for I := AIndex to TagsStack.Count - 1 do
  begin
    if not TagsStack[I].Tag.ApplyStylesToInnerHtml then
      Continue;
    TagsStack[I].OldPosition.CopyFrom(Position);
    TagsStack[I].Tag.ApplyProperties;
  end;
end;

procedure TdxHtmlImporter.EmptyProcess(ATag: TdxTagBase);
begin
  if IgnoredTagIsOpen then
    Exit;
  ATag.EmptyTagProcess;
end;

procedure TdxHtmlImporter.ApplyPreviousCharacterProperties(AIndex: TdxRunIndex);
var
  ACount: Integer;
  ARun: TdxTextRunBase;
begin
  ACount := TagsStack.Count;
  ARun := PieceTable.Runs[AIndex];
  if ACount > 0 then
    ARun.CharacterProperties.CopyFrom(TagsStack[ACount - 1].OldPosition.CharacterFormatting);
end;

function TdxHtmlImporter.CreateUriBasedRichEditImage(const AUri: string; APixelTargetWidth: Integer;
  APixelTargetHeight: Integer): TdxUriOfficeImage;
var
  AResult: TdxUriOfficeImage;
begin
  if FUniqueUriBasedImages.TryGetValue(AUri, AResult) then
    Result := TdxUriOfficeImage(AResult)
  else
    Result := CreateUriBasedRichEditImageCore(AUri, APixelTargetWidth, APixelTargetHeight);
end;

function TdxHtmlImporter.CreateUriBasedRichEditImageCore(const AUri: string; APixelTargetWidth: Integer;
  APixelTargetHeight: Integer): TdxUriOfficeImage;
var
  AUriList: TArray<string>;
begin
  AUriList := TArray<string>.Create(AUri);
  Result := TdxUriOfficeImage.Create(DocumentModel.ImageCache,
    DocumentModel, AUriList, APixelTargetWidth, APixelTargetHeight, Options.AsyncImageLoading);
  FUniqueUriBasedImages.Add(AUri, Result);
end;

procedure TdxHtmlImporter.AppendText(const AText: string);
begin
  if AText = '' then
    Exit;
  AppendText(PieceTable, AText);
end;

procedure TdxHtmlImporter.AppendText(APieceTable: TdxPieceTable; const AText: string);
var
  AInfo: TdxFontSizeInfo;
begin
  AInfo := ApplyPositionScriptFontSize;
  try
    APieceTable.InsertTextCore(Position, AText);
  finally
    RestorePositionScriptFontSize(AInfo);
  end;
  SetAppendObjectProperty;
end;

procedure TdxHtmlImporter.AppendPlainText(const AText: string);
var
  AInsertedText: string;
  AInfo: TdxFontSizeInfo;
begin
  if Options.ReplaceSpaceWithNonBreakingSpaceInsidePre then
    AInsertedText := TdxStringHelper.Replace(AText, TdxCharacters.Space, TdxCharacters.NonBreakingSpace)
  else
    AInsertedText := AText;
  AInfo := ApplyPositionScriptFontSize;
  try
    PieceTable.InsertPlainText(Position, AInsertedText);
  finally
    RestorePositionScriptFontSize(AInfo);
  end;
  SetAppendObjectProperty;
end;

procedure TdxHtmlImporter.AppendInlineImage(AImage: TdxOfficeImageReference;
  AScaleX, AScaleY: Single; const ADesiredSize: TSize);
var
  AInfo: TdxFontSizeInfo;
begin
  AInfo := ApplyPositionScriptFontSize;
  try
    PieceTable.AppendImage(Position, AImage, AScaleX, AScaleY, True);
  finally
    RestorePositionScriptFontSize(AInfo);
  end;
  SetAppendObjectProperty;
end;

procedure TdxHtmlImporter.AppendParagraph;
var
  AInfo: TdxFontSizeInfo;
begin
  AInfo := ApplyPositionScriptFontSize;
  try
    PieceTable.InsertParagraphCore(Position);
  finally
    RestorePositionScriptFontSize(AInfo);
  end;
end;

procedure TdxHtmlImporter.SetAppendObjectProperty;
begin
  IsEmptyParagraph := False;
  IsEmptyLine := False;
end;

function TdxHtmlImporter.ApplyPositionScriptFontSize: TdxFontSizeInfo;
var
  AFormatting: TdxCharacterFormattingBase;
begin
  AFormatting := Position.CharacterFormatting;
  Result := TdxFontSizeInfo.Create(AFormatting.DoubleFontSize, AFormatting.Options.UseDoubleFontSize);
end;

procedure TdxHtmlImporter.RestorePositionScriptFontSize(const AFontSizeInfo: TdxFontSizeInfo);
var
  AFormatting: TdxCharacterFormattingBase;
begin
  AFormatting := Position.CharacterFormatting;
  if AFormatting.DoubleFontSize <> AFontSizeInfo.DoubleFontSize then
    AFormatting.DoubleFontSize := AFontSizeInfo.DoubleFontSize;

  if not AFontSizeInfo.UseFontSize then
    AFormatting.ResetUse([TdxUsedCharacterFormattingOption.UseDoubleFontSize]);
end;

function TdxHtmlImporter.GetTagNameID(const AName: string): TdxHtmlTagNameID;
begin
  Result := Parser.GetTagNameID(AName);
end;

function TdxHtmlImporter.CreateHtmlParser: TdxHtmlParser;
begin
  Result := TdxHtmlParser.Create;
end;

procedure TdxHtmlImporter.ValidateHyperlinkInfo(AHyperlinkInfo: TdxHyperlinkInfo);
begin
end;

procedure TdxHtmlImporter.ProcessHyperlinkStart(AHyperlinkInfo: TdxHyperlinkInfo);
var
  AInfo: TdxImportFieldInfo;
  AImportFieldHelper: TdxImportFieldHelper;
begin
  if not TdxDocumentFormatsHelper.ShouldInsertHyperlink(DocumentModel) then
    Exit;
  ValidateHyperlinkInfo(AHyperlinkInfo);
  AInfo := TdxImportFieldInfo.Create(PieceTable);
  AImportFieldHelper := TdxImportFieldHelper.Create(PieceTable);
  try
    AImportFieldHelper.ProcessFieldBegin(AInfo, Position);
    AImportFieldHelper.InsertHyperlinkInstruction(AHyperlinkInfo, Position);
    AImportFieldHelper.ProcessFieldSeparator(AInfo, Position);
    ProcessHyperlink := AInfo;
  finally
    AImportFieldHelper.Free;
  end;
end;

procedure TdxHtmlImporter.ProcessHyperlinkEnd;
var
  ACurrentInfo: TdxImportFieldInfo;
  AImportFieldHelper: TdxImportFieldHelper;
  AField: TdxField;
begin
  if not TdxDocumentFormatsHelper.ShouldInsertHyperlink(DocumentModel) then
    Exit;
  ACurrentInfo := ProcessHyperlink;
  AImportFieldHelper := TdxImportFieldHelper.Create(PieceTable);
  try
    AField := AImportFieldHelper.ProcessFieldEnd(ACurrentInfo, Position);
    if ACurrentInfo.CodeEndIndex + 1 = ACurrentInfo.ResultEndIndex then
      FEmptyHyperlinks.Add(AField);
    FProcessHyperlink := nil;
  finally
    AImportFieldHelper.Free;
  end;
end;

function TdxHtmlImporter.ValidateBookmarkName(const AAnchorName: string): string;
begin
  Result := AAnchorName;
end;

procedure TdxHtmlImporter.ProcessBookmarkStart(const AAnchorName: string);
var
  ABookmarkInfo: TdxHtmlBookmarkInfo;
begin
  ABookmarkInfo := TdxHtmlBookmarkInfo.Create;
  ABookmarkInfo.Name := ValidateBookmarkName(AAnchorName);
  ABookmarkInfo.Start := Position.LogPosition;
  ProcessBookmarks.Push(ABookmarkInfo);
end;

procedure TdxHtmlImporter.ProcessBookmarkEnd;
var
  ABookmarkInfo: TdxHtmlBookmarkInfo;
begin
  ABookmarkInfo := ProcessBookmarks.Extract;
  try
    ABookmarkInfo.&End := Position.LogPosition;
    if DocumentModel.DocumentCapabilities.BookmarksAllowed then
      CreateBookmark(ABookmarkInfo);
  finally
    ABookmarkInfo.Free;
  end;
end;

procedure TdxHtmlImporter.CreateBookmark(ABookmarkInfo: TdxHtmlBookmarkInfo);
var
  ALength: Integer;
begin
  if ABookmarkInfo.Validate(PieceTable) then
  begin
    ALength := ABookmarkInfo.&End - ABookmarkInfo.Start;
    PieceTable.CreateBookmarkCore(ABookmarkInfo.Start, ALength, ABookmarkInfo.Name);
  end;
end;

procedure TdxHtmlImporter.SetDefaultDocumentProperties;
var
  AInfo: TdxParagraphFormattingInfo;
begin
  DocumentModel.DefaultCharacterProperties.DoubleFontSize := 24;
  DocumentModel.DefaultCharacterProperties.FontName := 'Times New Roman';

  AInfo := DocumentModel.Cache.ParagraphFormattingInfoCache.DefaultItem;
  AInfo.LineSpacing := 0.0;
  AInfo.LineSpacingType := TdxRichEditParagraphLineSpacing.Single;
  AInfo.SpacingBefore := 0;
  AInfo.SpacingAfter := 0;
end;

class procedure TdxHtmlImporter.ThrowInvalidFile;
begin
  TdxRichEditExceptions.ThrowInvalidOperationException('Invalid HTML file');
end;

procedure TdxHtmlImporter.RegisterCommentMarksToCollectPositions(const AMarks: array of string);
var
  AMark: string;
begin
  for AMark in AMarks do
    FMarkTable.Add(AMark, nil);
end;

function TdxHtmlImporter.GetMarkPosition(const AMark: string): TdxDocumentLogPosition;
var
  AResult: TdxBookmark;
begin
  if FMarkTable.TryGetValue(AMark, AResult) then
    if AResult <> nil then
      Exit(AResult.Start);
  Result := -1;
end;

{ TdxImportHtmlFormat }

class function TdxImportHtmlFormat.GetDocumentFormat: TdxRichEditDocumentFormat;
begin
  Result := TdxRichEditDocumentFormat.Html;
end;

function TdxImportHtmlFormat.GetDocumentImporter: IdxImporter<TdxRichEditDocumentFormat, Boolean>;
begin
  Result := TdxHtmlDocumentImporter.Create;
end;

function TdxImportHtmlFormat.GetImporter(ADocumentModel: TdxCustomDocumentModel;
  const AOptions: IdxImporterOptions): TdxDocumentModelImporter;
begin
  Result := TdxHtmlImporter.Create(TdxDocumentModel(ADocumentModel), AOptions as TdxHtmlDocumentImporterOptions);
end;

{ TdxPieceTablePasteHtmlTextCommand }

function TdxPieceTablePasteHtmlTextCommand.GetFormat: TdxRichEditDocumentFormat;
begin
  Result := TdxRichEditDocumentFormat.Html;
end;

function TdxPieceTablePasteHtmlTextCommand.GetContent: TdxClipboardStringContent;
begin
  Result := TdxClipboardStringContent.Create('');
end;

procedure TdxPieceTablePasteHtmlTextCommand.PopulateDocumentModelFromContentStringCore(
  ADocumentModel: TdxDocumentModel; AContent: TdxClipboardStringContent; const ASizeCollection: string);
var
  AOptions: TdxHtmlDocumentImporterOptions;
  AImporter: TdxHtmlImporter;
  AStream: TStream;
begin
  AOptions := TdxHtmlDocumentImporterOptions.Create;
  try
    AOptions.SourceUri := '';
    AOptions.Encoding := TdxEncoding.UTF8.CodePage;
    AOptions.IgnoreMetaCharset := True;
    AOptions.LineBreakSubstitute := DocumentModel.BehaviorOptions.PasteLineBreakSubstitution;
    AImporter := ADocumentModel.InternalAPI.ImporterFactory.CreateHtmlImporter(ADocumentModel, AOptions) as TdxHtmlImporter;
    AStream := PrepareInputStream(AContent.StringContent);
    try
      AImporter.Import(AStream);
    finally
      AStream.Free;
    end;
  finally
    AOptions.Free;
  end;
end;

function TdxPieceTablePasteHtmlTextCommand.PrepareInputStream(const AStr: string): TStream;
begin
  Result := TdxMemoryStream.Create(TdxEncoding.UTF8.GetBytes(AStr));
end;

function TdxPieceTablePasteHtmlTextCommand.IsDataAvailable: Boolean;
begin
  Result := False;
end;

{ TdxHtmlClipboardStringContent }

constructor TdxHtmlClipboardStringContent.Create(const ASourseUri, AContent: string; AFragmentStart, AFragmentEnd: Integer);
begin
  inherited Create(AContent);
  FSourceUri := ASourseUri;
  FStartFragment := AFragmentStart;
  FEndFragment := AFragmentEnd;
end;

{ TdxHtmlClipboardData }

constructor TdxHtmlClipboardData.Create(const AContent: TArray<Byte>);
begin
  if (AContent = nil) or (Length(AContent) = 0) then
    Exit;
  Parse(AContent);
end;

procedure TdxHtmlClipboardData.Parse(const AContent: TArray<Byte>);
var
  ADescription: TdxStringsDictionary;
  AOffset, AFragmentStart, AFragmentEnd: Integer;
begin
  ADescription := CreateContentDescription(AContent);
  try
    AOffset := CalculateHtmlContentOffset(ADescription);

    AFragmentStart := CalculateFragmentStart(ADescription);
    if AFragmentStart < 0 then
      AFragmentStart := AOffset;

    AFragmentEnd := CalculateFragmentEnd(ADescription);
    if AFragmentEnd < 0 then
      AFragmentEnd := Length(AContent);

    if (AOffset < 0) and (AFragmentStart >= 0) then
      AOffset := AFragmentStart;
    if AOffset < 0 then
      Exit;

    FHtmlContent := TdxEncoding.UTF8.GetString(AContent, AOffset, Length(AContent) - AOffset);
    FStartFragment := TdxEncoding.UTF8.GetCharCount(AContent, AOffset, AFragmentStart - AOffset);
    FEndFragment := TdxEncoding.UTF8.GetCharCount(AContent, AOffset, AFragmentEnd - AOffset);
    FSourceUri := CalculateSourceUri(ADescription);
  finally
    ADescription.Free;
  end;
end;

function TdxHtmlClipboardData.CalculateFragmentStart(ADescription: TdxStringsDictionary): Integer;
begin
  Result := GetIntegerValue(ADescription, 'StartFragment');
end;

function TdxHtmlClipboardData.CalculateFragmentEnd(ADescription: TdxStringsDictionary): Integer;
begin
  Result := GetIntegerValue(ADescription, 'EndFragment');
end;

function TdxHtmlClipboardData.CreateContentDescription(const AContent: TArray<Byte>): TdxStringsDictionary;
var
  AStream: TdxMemoryStream;
  AReader: TStreamReader;
  ALine: string;
  APair: TArray<string>;
begin
  Result := TdxStringsDictionary.Create(TdxIStringComparer.Ordinal);
  AStream := TdxMemoryStream.Create(AContent);
  try
    AReader := TStreamReader.Create(AStream, TdxEncoding.UTF8);
    try
      while not AReader.EndOfStream do
      begin
        ALine := AReader.ReadLine;
        if ALine = '' then
          Continue;
        APair := TdxStringHelper.Split(ALine, [':']);
        if Length(APair) < 2 then
          Break;

        Result.AddOrSetValue(APair[0], TdxStringHelper.Substring(ALine, Length(APair[0]) + 1));
      end;
    finally
      AReader.Free;
    end;
  finally
    AStream.Free;
  end;
end;

function TdxHtmlClipboardData.CalculateHtmlContentOffset(ADescription: TdxStringsDictionary): Integer;
begin
  Result := GetIntegerValue(ADescription, 'StartHTML');
end;

function TdxHtmlClipboardData.GetIntegerValue(ADescription: TdxStringsDictionary; const AKey: string): Integer;
var
  AValue: string;
begin
  if not ADescription.TryGetValue(AKey, AValue) then
    Exit(-1);
  Result := StrToIntDef(AValue, -1);
end;

function TdxHtmlClipboardData.CalculateSourceUri(ADescription: TdxStringsDictionary): string;
begin
  if not ADescription.TryGetValue('SourceURL', Result) then
    Result := '';
end;

{ TdxPieceTablePasteHtmlClipboardDataCommand }

function TdxPieceTablePasteHtmlClipboardDataCommand.GetContent: TdxClipboardStringContent;
var
  ACF_HTML, ALength: Integer;
  AHMem: HGLOBAL;
  APData: Pointer;
  ABytes: TArray<Byte>;
  AHtmlData: TdxHtmlClipboardData;
begin
  ACF_HTML := RegisterClipboardFormat('HTML Format');

  if not IsClipboardFormatAvailable(ACF_HTML) then
    Exit(TdxClipboardStringContent.Create(''));

  if not OpenClipboard(0) then
    Exit(TdxClipboardStringContent.Create(''));

  AHMem := GetClipboardData(ACF_HTML);
  APData := GlobalLock(AHMem);
  try
    ALength := GlobalSize(AHMem);
    SetLength(ABytes, ALength);
    Move(APData^, ABytes[0], ALength);
    AHtmlData := TdxHtmlClipboardData.Create(ABytes);
    try
      Result := TdxHtmlClipboardStringContent.Create(AHtmlData.SourceUri, AHtmlData.HtmlContent, AHtmlData.StartFragment, AHtmlData.EndFragment);
    finally
      AHtmlData.Free;
    end;
  finally
    GlobalUnlock(AHMem);
    CloseClipboard;
  end;
end;

procedure TdxPieceTablePasteHtmlClipboardDataCommand.PopulateDocumentModelFromContentStringCore(
  ADocumentModel: TdxDocumentModel; AContent: TdxClipboardStringContent; const ASizeCollection: string);
var
  AHtmlContent: TdxHtmlClipboardStringContent;
  AOptions: TdxHtmlDocumentImporterOptions;
  ATempModel: TdxDocumentModel;
  AImporter: TdxHtmlImporter;
  AStringContent: string;
  AStartFragmentPos, AEndFragmentPos: TdxDocumentLogPosition;
  AStream: TStream;
begin
  AHtmlContent := TdxHtmlClipboardStringContent(AContent);
  AOptions := TdxHtmlDocumentImporterOptions.Create;
  try
    AOptions.SourceUri := AHtmlContent.SourceUri;
    AOptions.Encoding := TdxEncoding.UTF8.CodePage;
    AOptions.LineBreakSubstitute := DocumentModel.BehaviorOptions.PasteLineBreakSubstitution;
    ATempModel := ADocumentModel.CreateNew;
    try
      AImporter := DocumentModel.InternalAPI.ImporterFactory.CreateHtmlImporter(ATempModel, AOptions) as TdxHtmlImporter;
      try
        AImporter.RegisterCommentMarksToCollectPositions([StartFragmentCommentText, EndFragmentCommentText]);
        AStringContent := ClearHTMLContent(AHtmlContent.StringContent, AHtmlContent.StartFragment, AHtmlContent.EndFragment);
        AStream := PrepareInputStream(AStringContent);
        try
          AImporter.Import(AStream);
        finally
          AStream.Free;
        end;
        AStartFragmentPos := AImporter.GetMarkPosition(StartFragmentCommentText);
        AEndFragmentPos := AImporter.GetMarkPosition(EndFragmentCommentText);
        CopyFragment(ATempModel, ADocumentModel, AStartFragmentPos, AEndFragmentPos);
      finally
        AImporter.Free;
      end;
    finally
      ATempModel.Free;
    end;
  finally
    AOptions.Free;
  end;
end;

procedure TdxPieceTablePasteHtmlClipboardDataCommand.CopyFragment(ASourceModel, ATargetModel: TdxDocumentModel;
  AStartFragmentPos, AEndFragmentPos: TdxDocumentLogPosition);
var
  ACopyManager: TdxDocumentModelCopyManager;
  AOperation: TdxCopySectionOperation;
begin
  ACopyManager := TdxDocumentModelCopyManager.Create(ASourceModel.MainPieceTable,
    ATargetModel.MainPieceTable, TdxParagraphNumerationCopyOptions.CopyAlways);
  try
    AOperation := ASourceModel.CreateCopySectionOperation(ACopyManager) as TdxCopySectionOperation;
    try
      AOperation.Execute(AStartFragmentPos, AEndFragmentPos - AStartFragmentPos, False);
    finally
      AOperation.Free;
    end;
  finally
    ACopyManager.Free;
  end;
end;

function TdxPieceTablePasteHtmlClipboardDataCommand.ClearHTMLContent(const AHtmlContent: string; AStartFragent, AEndFragment: Integer): string;
var
  APartBefore, AMiddlePart, APartAfter: string;
begin
  APartBefore := RemoveFragmentMarks(TdxStringHelper.Substring(AHtmlContent, 0, AStartFragent));
  AMiddlePart := RemoveFragmentMarks(TdxStringHelper.Substring(AHtmlContent,AStartFragent, AEndFragment - AStartFragent));
  APartAfter := RemoveFragmentMarks(TdxStringHelper.Substring(AHtmlContent,AEndFragment));
  Result := APartBefore + StartFragmentComment + AMiddlePart + EndFragmentComment + APartAfter;
end;

function TdxPieceTablePasteHtmlClipboardDataCommand.RemoveFragmentMarks(const AContent: string): string;
begin
  Result := TdxStringHelper.Replace(AContent, StartFragmentComment, '');
  Result := TdxStringHelper.Replace(Result, EndFragmentComment, '');
end;

function TdxPieceTablePasteHtmlClipboardDataCommand.IsDataAvailable: Boolean;
begin
  Result := Clipboard.ContainsData(TdxOfficeDataFormats.Html);
end;

function TdxPieceTablePasteHtmlClipboardDataCommand.SuppressCopySectionProperties(ASource: TdxDocumentModel): Boolean;
begin
  Result := True;
end;

{ TdxPasteHtmlTextCommand }

function TdxPasteHtmlTextCommand.CreateInnerCommandCore: TdxPieceTablePasteContentConvertedToDocumentModelCommandBase;
begin
  Result := TdxPieceTablePasteHtmlClipboardDataCommand.Create(ActivePieceTable);
end;

class function TdxPasteHtmlTextCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandPasteHtmlTextDescription);
end;

class function TdxPasteHtmlTextCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandPasteHtmlTextMenuCaption);
end;

initialization
  dxPasteHtmlTextCommandClass := TdxPasteHtmlTextCommand;
  CF_HTML := TClipboard.RegisterFormat(TdxOfficeDataFormats.Html);

finalization
  dxPasteHtmlTextCommandClass := nil;

end.
