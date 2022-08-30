{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressSpreadSheet                                       }
{                                                                    }
{           Copyright (c) 2001-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSSPREADSHEET CONTROL AND ALL    }
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

unit dxSpreadSheetFormatXLSXReader;

{$I cxVer.Inc}
{$R dxSpreadSheetFormatXLSXReader.res}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, Windows, SysUtils, Classes, Graphics, dxCore, dxCoreClasses, cxClasses, dxCustomTree, dxXMLDoc, dxZIPUtils,
  dxSpreadSheetCore, dxSpreadSheetTypes, dxSpreadSheetClasses, dxSpreadSheetStrs, dxSpreadSheetPackedFileFormatCore,
  dxSpreadSheetUtils, dxGDIPlusClasses, Generics.Defaults, Generics.Collections, dxCoreGraphics, cxGeometry,
  dxSpreadSheetPrinting, dxSpreadSheetConditionalFormattingRules, dxSpreadSheetContainers, dxSpreadSheetProtection,
  dxHashUtils, Variants, dxSpreadSheetCoreStyles;

type
  TdxSpreadSheetXLSXReader = class;

  { TdxSpreadSheetXLSXHashTableItemList }

  TdxSpreadSheetXLSXHashTableItemList = class(TList<TdxHashTableItem>)
  protected
    procedure Notify(const Item: TdxHashTableItem; Action: TCollectionNotification); override;
  end;

  { TdxSpreadSheetXLSXFormulaAsTextInfoList }

  TdxSpreadSheetXLSXFormulaAsTextInfoList = class(TdxSpreadSheetFormulaAsTextInfoList)
  strict private
    FSharedFormulas: TDictionary<Int64, TdxSpreadSheetFormulaAsTextInfo>;
  protected
    function CreateItem: TdxSpreadSheetFormulaAsTextInfo; override;
    function GetSharedFormula(AView: TdxSpreadSheetCustomView; AIndex: Integer): TdxSpreadSheetFormulaAsTextInfo;
  public
    constructor Create(ASpreadSheet: TdxCustomSpreadSheet); override;
    destructor Destroy; override;
    procedure Add(ACell: TdxSpreadSheetCell; AFunctionNode: TdxXMLNode); overload;
    procedure ResolveReferences; override;
  end;

  { TdxSpreadSheetXLSXFormulaAsTextInfo }

  TdxSpreadSheetXLSXFormulaAsTextInfo = class(TdxSpreadSheetFormulaAsTextInfo)
  strict private
    FOwner: TdxSpreadSheetXLSXFormulaAsTextInfoList;
  protected
    SharedIndex: Integer;

    procedure ResolveReferences(Parser: TObject); override;
  public
    constructor Create(AOwner: TdxSpreadSheetXLSXFormulaAsTextInfoList);
  end;

  { TdxSpreadSheetXLSXReaderContentIndex }

  TdxSpreadSheetXLSXReaderContentIndex = class
  strict private
    FData: TStringList;

    function GetContentType(const AFileName: AnsiString): AnsiString;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Load(AReader: TdxSpreadSheetXLSXReader; const AFileName: AnsiString);
    //
    property ContentType[const AFileName: AnsiString]: AnsiString read GetContentType;
  end;

  { TdxSpreadSheetXLSXReaderRelsItem }

  TdxSpreadSheetXLSXReaderRelsItem = class
  public
    ID, FileName, FileType: AnsiString;

    procedure SetValue(const AFileName, AFileType: AnsiString);
  end;

  { TdxSpreadSheetXLSXReaderRels }

  TdxSpreadSheetXLSXReaderRels = class
  strict private
    FList: TcxObjectList;

    function GetItem(const ID: AnsiString): TdxSpreadSheetXLSXReaderRelsItem;
    function GetCount: Integer;
    function GetItemByIndex(Index: Integer): TdxSpreadSheetXLSXReaderRelsItem;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;
    function Find(const ID: AnsiString; out AItem: TdxSpreadSheetXLSXReaderRelsItem): Boolean;
    function FindByType(const AType: AnsiString; out AItem: TdxSpreadSheetXLSXReaderRelsItem): Boolean;
    procedure Load(AReader: TdxSpreadSheetXLSXReader; const AOwnerFileName: AnsiString);

    property Count: Integer read GetCount;
    property Items[const ID: AnsiString]: TdxSpreadSheetXLSXReaderRelsItem read GetItem;
    property ItemsByIndex[Index: Integer]: TdxSpreadSheetXLSXReaderRelsItem read GetItemByIndex;
  end;

  { TdxSpreadSheetXLSXReaderCustomParser }

  TdxSpreadSheetXLSXReaderCustomParser = class(TdxSpreadSheetCustomPackedReaderParser)
  strict private
    procedure ExtractIndexedColor(ANode: TdxXMLNode; AColorIndex: Integer; var AColor: TColor);
    procedure ExtractThemedColor(ANode: TdxXMLNode; AThemeIndex: Integer; var AColor: TColor);
    function GetOwner: TdxSpreadSheetXLSXReader; inline;
  protected
    function CheckListIndex(AIndex: Integer; AList: TList;
      const AMessage: string; AMessageType: TdxSpreadSheetMessageType): Boolean; overload;
    function CheckListIndex(AIndex: Integer; AList: TdxSpreadSheetXLSXHashTableItemList;
      const AMessage: string; AMessageType: TdxSpreadSheetMessageType): Boolean; overload;
    function DecodeColor(const ANode: TdxXMLNode): TColor; overload;
    function DecodeColor(const AHexCode: string): TColor; overload;
  public
    property Owner: TdxSpreadSheetXLSXReader read GetOwner;
  end;

  { TdxSpreadSheetXLSXReaderCustomNodeParser }

  TdxSpreadSheetXLSXReaderCustomNodeParser = class(TdxSpreadSheetXLSXReaderCustomParser)
  protected
    FNode: TdxXMLNode;
  public
    constructor Create(ANode: TdxXMLNode; AOwner: TdxSpreadSheetXLSXReader);
    //
    property Node: TdxXMLNode read FNode;
  end;

  { TdxSpreadSheetXLSXReaderCustomDocumentParser }

  TdxSpreadSheetXLSXReaderCustomDocumentParser = class(TdxSpreadSheetXLSXReaderCustomNodeParser)
  protected
    FDocument: TdxXMLDocument;
    FDocumentFileName: AnsiString;
    FRels: TdxSpreadSheetXLSXReaderRels;
  public
    constructor Create(const AFileName: AnsiString; AOwner: TdxSpreadSheetXLSXReader); virtual;
    destructor Destroy; override;
    //
    property Document: TdxXMLDocument read FDocument;
    property DocumentFileName: AnsiString read FDocumentFileName;
    property Rels: TdxSpreadSheetXLSXReaderRels read FRels;
  end;

  { TdxSpreadSheetXLSXReaderCustomDocumentSubParser }

  TdxSpreadSheetXLSXReaderCustomDocumentSubParser = class(TdxSpreadSheetXLSXReaderCustomNodeParser)
  strict private
    FOwnerParser: TdxSpreadSheetXLSXReaderCustomDocumentParser;
  protected
    function ReadEmbeddedImage(const AValue: TdxXMLString; out AStream: TStream): Boolean;
    function ReadImage(ANode: TdxXMLNode; out AStream: TStream): Boolean;
  public
    constructor Create(ANode: TdxXMLNode; AOwnerParser: TdxSpreadSheetXLSXReaderCustomDocumentParser);
    //
    property OwnerParser: TdxSpreadSheetXLSXReaderCustomDocumentParser read FOwnerParser;
  end;

  { TdxSpreadSheetXLSXReader }

  TdxSpreadSheetXLSXReader = class(TdxSpreadSheetCustomPackedReader)
  strict private
    FBorders: TdxSpreadSheetXLSXHashTableItemList;
    FConditionalFormattingStyles: TdxSpreadSheetXLSXHashTableItemList;
    FFills: TdxSpreadSheetXLSXHashTableItemList;
    FFonts: TdxSpreadSheetXLSXHashTableItemList;
    FFormats: TdxSpreadSheetXLSXHashTableItemList;
    FFormulasRefs: TdxSpreadSheetXLSXFormulaAsTextInfoList;
    FHyperlinks: TStringList;
    FIndexedColors: TList<TColor>;
    FSharedStrings: TdxSpreadSheetXLSXHashTableItemList;
    FStyles: TdxSpreadSheetXLSXHashTableItemList;
    FThemedBrushes: TObjectList<TdxGPBrush>;
    FThemedColors: TList;
    FThemedPens: TObjectList<TdxGPPen>;

    FColorMap: TDictionary<TdxXMLString, TColor>;
    FColumnWidthHelper: TdxSpreadSheetExcelColumnWidthHelper;
    FContentIndex: TdxSpreadSheetXLSXReaderContentIndex;

    function GetColumnWidthHelper: TdxSpreadSheetExcelColumnWidthHelper;
    function GetContentIndex: TdxSpreadSheetXLSXReaderContentIndex;
  protected
    function CreateProgressHelper: TdxSpreadSheetCustomFilerProgressHelper; override;
    function CreateXML: TdxSpreadSheetXMLDocument; override;
    procedure ForEachNodeChild(ANode: TdxXMLNode; AProc: TdxXMLNodeForEachProc; AUserData: Pointer);
    procedure ResolveHyperlinks;
  public
    constructor Create(AOwner: TdxCustomSpreadSheet; AStream: TStream); override;
    destructor Destroy; override;
    function GetWorkbookFileName(out AFileName: AnsiString): Boolean; virtual;
    procedure ReadData; override;

    property Borders: TdxSpreadSheetXLSXHashTableItemList read FBorders;
    property ConditionalFormattingStyles: TdxSpreadSheetXLSXHashTableItemList read FConditionalFormattingStyles;
    property Fills: TdxSpreadSheetXLSXHashTableItemList read FFills;
    property Fonts: TdxSpreadSheetXLSXHashTableItemList read FFonts;
    property Formats: TdxSpreadSheetXLSXHashTableItemList read FFormats;
    property FormulasRefs: TdxSpreadSheetXLSXFormulaAsTextInfoList read FFormulasRefs;
    property Hyperlinks: TStringList read FHyperlinks;
    property IndexedColors: TList<TColor> read FIndexedColors;
    property SharedStrings: TdxSpreadSheetXLSXHashTableItemList read FSharedStrings;
    property Styles: TdxSpreadSheetXLSXHashTableItemList read FStyles;
    property ThemedBrushes: TObjectList<TdxGPBrush> read FThemedBrushes;
    property ThemedColors: TList read FThemedColors;
    property ThemedPens: TObjectList<TdxGPPen> read FThemedPens;

    property ColorMap: TDictionary<TdxXMLString, TColor> read FColorMap;
    property ColumnWidthHelper: TdxSpreadSheetExcelColumnWidthHelper read GetColumnWidthHelper;
    property ContentIndex: TdxSpreadSheetXLSXReaderContentIndex read GetContentIndex;
  end;

  { TdxSpreadSheetXLSXReaderBrushParser }

  TdxSpreadSheetXLSXReaderBrushParser = class(TdxSpreadSheetXLSXReaderCustomDocumentSubParser)
  strict private
    FBrush: TdxGPBrush;
  protected
    procedure ProcessGradientBrushPoints(ANode: TdxXMLNode; AUserData: Pointer); virtual;

    function ReadColor(AParentNode: TdxXMLNode): TdxAlphaColor;
    procedure ReadGradientBrushParameters; virtual;
    procedure ReadPatternBrushParameters; virtual;
    procedure ReadSolidBrushParameters; virtual;
    procedure ReadTexturedBrushParameters; virtual;
  public
    constructor Create(ANode: TdxXMLNode; ABrush: TdxGPBrush; AOwnerParser: TdxSpreadSheetXLSXReaderCustomDocumentParser);
    procedure Execute; override;
    //
    property Brush: TdxGPBrush read FBrush;
  end;

  { TdxSpreadSheetXLSXReaderPenParser }

  TdxSpreadSheetXLSXReaderPenParser = class(TdxSpreadSheetXLSXReaderCustomDocumentSubParser)
  strict private
    FPen: TdxGPPen;
  public
    constructor Create(ANode: TdxXMLNode; APen: TdxGPPen; AOwnerParser: TdxSpreadSheetXLSXReaderCustomDocumentParser);
    procedure Execute; override;
    //
    property Pen: TdxGPPen read FPen;
  end;

  { TdxSpreadSheetXLSXReaderExternalLinkParser }

  TdxSpreadSheetXLSXReaderExternalLinkParser = class(TdxSpreadSheetXLSXReaderCustomDocumentParser)
  protected
    function DecodePath(const APath: AnsiString): string;
  public
    procedure Execute; override;
  end;

  { TdxSpreadSheetXLSXReaderFontParser }

  TdxSpreadSheetXLSXReaderFontParser = class(TdxSpreadSheetXLSXReaderCustomParser)
  strict private
    FFont: TdxSpreadSheetFontHandle;
    FNode: TdxXMLNode;
  protected
    procedure ProcessParam(ANode: TdxXMLNode; AUserData: Pointer);
  public
    constructor Create(AFont: TdxSpreadSheetFontHandle; ANode: TdxXMLNode; AOwner: TdxSpreadSheetXLSXReader);
    procedure Execute; override;
    //
    property Font: TdxSpreadSheetFontHandle read FFont;
    property Node: TdxXMLNode read FNode;
  end;

  { TdxSpreadSheetXLSXReaderFormulasParser }

  TdxSpreadSheetXLSXReaderFormulasParser = class(TdxSpreadSheetXLSXReaderCustomParser)
  public
    procedure Execute; override;
  end;

  { TdxSpreadSheetXLSXReaderHeaderFooterParser }

  TdxSpreadSheetXLSXReaderHeaderFooterParser = class(TdxSpreadSheetXLSXReaderCustomNodeParser)
  strict private
    FText: TdxSpreadSheetTableViewOptionsPrintHeaderFooterText;
  public
    constructor Create(ANode: TdxXMLNode; AOwner: TdxSpreadSheetXLSXReader;
      AText: TdxSpreadSheetTableViewOptionsPrintHeaderFooterText);
    procedure Execute; override;
    //
    property Text: TdxSpreadSheetTableViewOptionsPrintHeaderFooterText read FText;
  end;

  { TdxSpreadSheetXLSXReaderSharedStringParser }

  TdxSpreadSheetXLSXReaderSharedStringParser = class(TdxSpreadSheetXLSXReaderCustomDocumentParser)
  protected
    procedure ProcessSharedString(ANode: TdxXMLNode; AUserData: Pointer);
  public
    procedure Execute; override;
    class function ParseString(AOwner: TdxSpreadSheetXLSXReader; AStringNode: TdxXMLNode): TdxSpreadSheetSharedString;
  end;

  { TdxSpreadSheetXLSXReaderRichTextParser }

  TdxSpreadSheetXLSXReaderRichTextParser = class
  strict private
    FMultiline: Boolean;
    FNamespaceURI: AnsiString;
    FOwner: TdxSpreadSheetXLSXReader;
    FRuns: TdxSpreadSheetFormattedSharedStringRuns;
    FText: TStringBuilder;

    procedure ProcessRichTextRun(ANode: TdxXMLNode; AUserData: Pointer);
  protected
    function CreateFontParser(AFont: TdxSpreadSheetFontHandle; ANode: TdxXMLNode): TdxSpreadSheetXLSXReaderFontParser; virtual;
    function DoParse(ANode: TdxXMLNode): TdxSpreadSheetFormattedSharedString;
  public
    constructor Create(AOwner: TdxSpreadSheetXLSXReader);
    destructor Destroy; override;
    class function Parse(AOwner: TdxSpreadSheetXLSXReader; ANode: TdxXMLNode; AMultiline: Boolean = True): TdxSpreadSheetFormattedSharedString;
    //
    property Multiline: Boolean read FMultiline write FMultiline;
    property Owner: TdxSpreadSheetXLSXReader read FOwner;
  end;

  { TdxSpreadSheetXLSXReaderStyleParser }

  TdxSpreadSheetXLSXReaderStyleParser = class(TdxSpreadSheetXLSXReaderCustomDocumentParser)
  protected
    procedure ProcessBorder(ANode: TdxXMLNode; AUserData: Pointer);
    procedure ProcessCellXfs(ANode: TdxXMLNode; AUserData: Pointer);
    procedure ProcessConditionalFormattingStyle(ANode: TdxXMLNode; AUserData: Pointer);
    procedure ProcessFill(ANode: TdxXMLNode; AUserData: Pointer);
    procedure ProcessFont(ANode: TdxXMLNode; AUserData: Pointer);
    procedure ProcessIndexedColors(ANode: TdxXMLNode; AUserData: Pointer);
    procedure ProcessNumberFormat(ANode: TdxXMLNode; AUserData: Pointer);
    procedure ProcessStyleCollection(ANode: TdxXMLNode; AUserData: Pointer);

    function ReadBorder(ANode: TdxXMLNode): TdxHashTableItem;
    function ReadConditionalFormattingFill(ANode: TdxXMLNode): TdxHashTableItem;
    function ReadFill(ANode: TdxXMLNode): TdxHashTableItem;
    function ReadFont(ANode: TdxXMLNode): TdxHashTableItem;
    function ReadFormat(ANode: TdxXMLNode): TdxHashTableItem;
    function ReadPatternFill(ANode: TdxXMLNode; const ADefaultPatternFillType: AnsiString): TdxHashTableItem;
  public
    procedure Execute; override;
  end;

  { TdxSpreadSheetXLSXReaderStyleParserCellStyleHelper }

  TdxSpreadSheetXLSXReaderStyleParserCellStyleHelper = class
  strict private
    FNodeCellStyleXf: TdxXMLNode;
    FNodeCellStyleXfAlignment: TdxXMLNode;
    FNodeCellStyleXfProtection: TdxXMLNode;
    FNodeCellXf: TdxXMLNode;
    FNodeCellXfAlignment: TdxXMLNode;
    FNodeCellXfProtection: TdxXMLNode;

    function FindAttr(const AAttrName: TdxXMLString; ANode, ANodeDefault: TdxXMLNode): TdxXMLNodeAttribute;
    function GetAlignmentNode(ANode: TdxXMLNode): TdxXMLNode;
    function GetCellXfsNode(ANode: TdxXMLNode; const ANodeName: TdxXMLString): TdxXMLNode;
    function GetProtectionNode(ANode: TdxXMLNode): TdxXMLNode;
  public
    constructor Create(ANode: TdxXMLNode);
    function GetAlignmentAttrValue(const AAttrName: TdxXMLString; const ADefaultValue: TdxXMLString = ''): TdxXMLString;
    function GetAlignmentAttrValueAsBoolean(const AAttrName: TdxXMLString; const ADefaultValue: Boolean = False): Boolean;
    function GetAlignmentAttrValueAsInteger(const AAttrName: TdxXMLString; const ADefaultValue: Integer = 0): Integer;
    function GetAttrValue(const AAttrName: TdxXMLString; ADefaultValue: Integer = 0): Integer;
    function GetNumberFormatHandle(AReader: TdxSpreadSheetXLSXReader): TObject;
    function GetProtectionAttrValueAsBoolean(const AAttrName: TdxXMLString; const ADefaultValue: Boolean = False): Boolean;
    function GetResourceHandle(const AResName, AOptionName: TdxXMLString; AResourceCollection: TdxSpreadSheetXLSXHashTableItemList): TObject;
  end;

  { TdxSpreadSheetXLSXReaderThemeParser }

  TdxSpreadSheetXLSXReaderThemeParser = class(TdxSpreadSheetXLSXReaderCustomDocumentParser)
  protected
    function GetColorMap: TDictionary<TdxXMLString, TColor>; virtual;
    function GetThemedBrushes: TObjectList<TdxGPBrush>; virtual;
    function GetThemedColors: TList; virtual;
    function GetThemedPens: TObjectList<TdxGPPen>; virtual;

    function ProcessColor(ANode: TdxXMLNode): TColor;
    procedure ProcessFillStyle(ANode: TdxXMLNode; AUserData: Pointer); virtual;
    procedure ProcessLineStyle(ANode: TdxXMLNode; AUserData: Pointer); virtual;

    procedure ReadColorSchema(ANode: TdxXMLNode); virtual;
  public
    procedure Execute; override;
    //
    property ColorMap: TDictionary<TdxXMLString, TColor> read GetColorMap;
    property ThemedBrushes: TObjectList<TdxGPBrush> read GetThemedBrushes;
    property ThemedColors: TList read GetThemedColors;
    property ThemedPens: TObjectList<TdxGPPen> read GetThemedPens;
  end;

  { TdxSpreadSheetXLSXReaderWorkbookParser }

  TdxSpreadSheetXLSXReaderWorkbookParser = class(TdxSpreadSheetXLSXReaderCustomParser)
  strict private
    FFileName: AnsiString;
    FLocalSheetIdMap: TDictionary<string, TdxSpreadSheetCustomView>;
    FRels: TdxSpreadSheetXLSXReaderRels;
  protected
    procedure ParseWorkbook; virtual;
    procedure ParseWorkbookDependencies; virtual;
    procedure ParseWorkbookProperties(AWorkbook: TdxXMLDocument); virtual;
    //
    procedure ProcessDefinedName(ANode: TdxXMLNode; AUserData: Pointer); virtual;
    procedure ProcessExternalReference(ANode: TdxXMLNode; AUserData: Pointer); virtual;
    procedure ProcessWorksheet(ANode: TdxXMLNode; AUserData: Pointer); virtual;
    //
    procedure ReadSharedStrings(const AFileName: AnsiString); virtual;
    procedure ReadStyles(const AFileName: AnsiString); virtual;
    procedure ReadTheme(const AFileName: AnsiString); virtual;
    function ReadProtectionInfoStandard(ANode: TdxXMLNode): IdxSpreadSheetProtectionInfo;
    function ReadProtectionInfoStrong(ANode: TdxXMLNode): IdxSpreadSheetProtectionInfo;
  public
    constructor Create(const AFileName: AnsiString; AOwner: TdxSpreadSheetXLSXReader);
    destructor Destroy; override;
    procedure Execute; override;
    //
    property FileName: AnsiString read FFileName;
    property Rels: TdxSpreadSheetXLSXReaderRels read FRels;
  end;

  { TdxSpreadSheetXLSXReaderWorksheetParser }

  TdxSpreadSheetXLSXReaderWorksheetParser = class(TdxSpreadSheetXLSXReaderCustomDocumentParser)
  strict private
    FView: TdxSpreadSheetTableView;

    function GetCellStyles: TdxSpreadSheetXLSXHashTableItemList;
    function GetChild(const AName: TdxXMLString; out ANode: TdxXMLNode): Boolean;
    function GetSharedStrings: TdxSpreadSheetXLSXHashTableItemList;
  protected
    function ConvertRowHeight(const AValue: Double): Integer; virtual;
    function ExtractColumnIndex(const S: string): Integer;

    procedure ProcessBreaks(ANode: TdxXMLNode; AUserData: Pointer); virtual;
    procedure ProcessColumn(ANode: TdxXMLNode; AUserData: Pointer); virtual;
    procedure ProcessFixedPane(ANode: TdxXMLNode); virtual;
    procedure ProcessHeaderFooter(ANode: TdxXMLNode); virtual;
    procedure ProcessHyperlinks(ANode: TdxXMLNode; AUserData: Pointer); virtual;
    procedure ProcessLegacyDrawings(ANode: TdxXMLNode); virtual;
    procedure ProcessMergedCells(ANode: TdxXMLNode; AUserData: Pointer); virtual;
    procedure ProcessPageMargins(ANode: TdxXMLNode); virtual;
    procedure ProcessPageSetup(ANode: TdxXMLNode); virtual;
    procedure ProcessPageSetupExProperties(ANode: TdxXMLNode); virtual;
    procedure ProcessPrintOptions(ANode: TdxXMLNode); virtual;
    procedure ProcessProperties(ANode: TdxXMLNode); virtual;
    procedure ProcessRow(ANode: TdxXMLNode; AUserData: Pointer); virtual;
    procedure ProcessRowCell(ANode: TdxXMLNode; AUserData: Pointer); virtual;
    procedure ProcessSelection(ANode: TdxXMLNode; ASelection: TdxSpreadSheetTableViewSelection); virtual;

    procedure ReadCells; virtual;
    procedure ReadConditionalFormatting; virtual;
    procedure ReadDrawing(const AFileName: AnsiString); virtual;
    procedure ReadDrawings; virtual;
    procedure ReadPrinting; virtual;
    procedure ReadProtection; virtual;
    function ReadProtectionInfoStandard(ANode: TdxXMLNode): IdxSpreadSheetProtectionInfo;
    function ReadProtectionInfoStrong(ANode: TdxXMLNode): IdxSpreadSheetProtectionInfo;
    procedure ReadViewProperties; virtual;

  public
    constructor Create(const AFileName: AnsiString; AView: TdxSpreadSheetTableView; AOwner: TdxSpreadSheetXLSXReader); reintroduce;
    procedure Execute; override;
    //
    property CellStyles: TdxSpreadSheetXLSXHashTableItemList read GetCellStyles;
    property SharedStrings: TdxSpreadSheetXLSXHashTableItemList read GetSharedStrings;
    property View: TdxSpreadSheetTableView read FView;
  end;

implementation

uses
  AnsiStrings, Math, cxGraphics, dxSpreadSheetGraphics, dxSpreadSheetFormulas, dxSpreadSheetFormatUtils,
  dxSpreadSheetConditionalFormattingIconSet, dxSpreadSheetHyperlinks, dxOLECryptoContainer,
  // XLSX
  dxSpreadSheetFormatXLSX, dxSpreadSheetFormatXLSXTags, dxSpreadSheetFormatXLSXReaderDrawing,
  dxSpreadSheetFormatXLSXReaderConditionalFormatting, dxSpreadSheetFormatXLSXReaderComments,
  dxSpreadSheetCoreFormulasParser, dxSpreadSheetCoreStrs;

type
  TdxSpreadSheetAccess = class(TdxCustomSpreadSheet);
  TdxSpreadSheetTableColumnAccess = class(TdxSpreadSheetTableColumn);
  TdxSpreadSheetTableItemGroupAccess = class(TdxSpreadSheetTableItemGroup);
  TdxSpreadSheetTableRowAccess = class(TdxSpreadSheetTableRow);
  TdxSpreadSheetTableViewAccess = class(TdxSpreadSheetTableView);

  { TdxSpreadSheetOpenXMLNode }

  TdxSpreadSheetOpenXMLNode = class(TdxSpreadSheetXMLNode)
  protected
    function GetNodeClass: TdxXMLNodeClass; override;
  public
    function FindChild(const AName: TdxXMLString; out ANode: TdxXMLNode): Boolean; override;
  end;

  { TdxSpreadSheetOpenXMLDocument }

  TdxSpreadSheetOpenXMLDocument = class(TdxSpreadSheetXMLDocument)
  protected
    function CreateRootNode: TdxXMLNode; override;
  end;

{ TdxSpreadSheetXLSXFormulaAsTextInfo }

constructor TdxSpreadSheetXLSXFormulaAsTextInfo.Create(AOwner: TdxSpreadSheetXLSXFormulaAsTextInfoList);
begin
  inherited Create;
  FOwner := AOwner;
end;

procedure TdxSpreadSheetXLSXFormulaAsTextInfo.ResolveReferences(Parser: TObject);
begin
  if IsShared then
    Cell.AsFormula := FOwner.GetSharedFormula(Cell.View, SharedIndex).Cell.AsFormula.Clone
  else
    inherited ResolveReferences(Parser);
end;

{ TdxSpreadSheetXLSXFormulaAsTextInfoList }

constructor TdxSpreadSheetXLSXFormulaAsTextInfoList.Create(ASpreadSheet: TdxCustomSpreadSheet);
begin
  inherited;
  FSharedFormulas := TDictionary<Int64, TdxSpreadSheetFormulaAsTextInfo>.Create;
end;

destructor TdxSpreadSheetXLSXFormulaAsTextInfoList.Destroy;
begin
  FreeAndNil(FSharedFormulas);
  inherited;
end;

procedure TdxSpreadSheetXLSXFormulaAsTextInfoList.Add(ACell: TdxSpreadSheetCell; AFunctionNode: TdxXMLNode);
var
  ACellType: AnsiString;
  AInfo: TdxSpreadSheetXLSXFormulaAsTextInfo;
begin
  ACellType := AFunctionNode.Attributes.GetValue(sdxXLSXAttrCellType);
  if AFunctionNode.Text <> '' then
  begin
    AInfo := TdxSpreadSheetXLSXFormulaAsTextInfo(Add(ACell,
      dxSpreadSheetFormulaIncludeEqualSymbol(AFunctionNode.TextAsString), SameText(ACellType, sdxXLSXValueArray), False,
      dxStringToReferenceArea(AFunctionNode.Attributes.GetValueAsString(sdxXLSXAttrRef))));
    if SameText(ACellType, sdxXLSXValueShared) then
    begin
      AInfo.SharedIndex := AFunctionNode.Attributes.GetValueAsInteger(sdxXLSXAttrSharedIndex);
      FSharedFormulas.AddOrSetValue(dxMakeInt64(ACell.View.Index, AInfo.SharedIndex), AInfo);
    end;
  end
  else
    if SameText(ACellType, sdxXLSXValueShared) then
    begin
      AInfo := TdxSpreadSheetXLSXFormulaAsTextInfo(Add(ACell, '', False, True, cxInvalidRect));
      AInfo.SharedIndex := AFunctionNode.Attributes.GetValueAsInteger(sdxXLSXAttrSharedIndex);
    end;
end;

procedure TdxSpreadSheetXLSXFormulaAsTextInfoList.ResolveReferences;

  procedure ResolveReferencesCore(AParser: TObject; ASharedFormulas: Boolean);
  var
    AInfo: TdxSpreadSheetXLSXFormulaAsTextInfo;
    I: Integer;
  begin
    for I := 0 to Count - 1 do
    begin
      AInfo := TdxSpreadSheetXLSXFormulaAsTextInfo(Items[I]);
      if (AInfo.Cell <> nil) and (AInfo.IsShared = ASharedFormulas) then
        AInfo.ResolveReferences(AParser);
    end;
  end;

var
  AParser: TObject;
begin
  AParser := CreateParser;
  try
    ResolveReferencesCore(AParser, False);
    ResolveReferencesCore(AParser, True);
  finally
    AParser.Free;
  end;
end;

function TdxSpreadSheetXLSXFormulaAsTextInfoList.CreateItem: TdxSpreadSheetFormulaAsTextInfo;
begin
  Result := TdxSpreadSheetXLSXFormulaAsTextInfo.Create(Self);
end;

function TdxSpreadSheetXLSXFormulaAsTextInfoList.GetSharedFormula(
  AView: TdxSpreadSheetCustomView; AIndex: Integer): TdxSpreadSheetFormulaAsTextInfo;
begin
  Result := FSharedFormulas.Items[dxMakeInt64(AView.Index, AIndex)];
end;

{ TdxSpreadSheetXLSXReaderContentIndex }

constructor TdxSpreadSheetXLSXReaderContentIndex.Create;
begin
  inherited Create;
  FData := TStringList.Create;
  FData.Delimiter := '|';
end;

destructor TdxSpreadSheetXLSXReaderContentIndex.Destroy;
begin
  FreeAndNil(FData);
  inherited Destroy;
end;

procedure TdxSpreadSheetXLSXReaderContentIndex.Load(AReader: TdxSpreadSheetXLSXReader; const AFileName: AnsiString);
var
  ADocument: TdxXMLDocument;
  ANode: TdxXMLNode;
begin
  FData.Clear;
  ADocument := AReader.ReadXML(AFileName);
  try
    if ADocument.Root.First <> nil then
    begin
      ANode := ADocument.Root.First.First;
      while ANode <> nil do
      begin
        if SameText(ANode.Name, sdxXLSXNodeOverride) then
          FData.Values[ANode.Attributes.GetValueAsString(sdxXLSXAttrPartName)] := ANode.Attributes.GetValueAsString(sdxXLSXAttrContentType);
        ANode := ANode.Next;
      end;
    end;
  finally
    ADocument.Free;
  end;
end;

function TdxSpreadSheetXLSXReaderContentIndex.GetContentType(const AFileName: AnsiString): AnsiString;
begin
  Result := dxStringToAnsiString(FData.Values[dxAnsiStringToString(AFileName)]);
end;

{ TdxSpreadSheetXLSXReaderRelsItem }

procedure TdxSpreadSheetXLSXReaderRelsItem.SetValue(const AFileName, AFileType: AnsiString);
begin
  FileName := AFileName;
  FileType := AFileType;
end;

{ TdxSpreadSheetXLSXReaderRels }

constructor TdxSpreadSheetXLSXReaderRels.Create;
begin
  inherited Create;
  FList := TcxObjectList.Create;
end;

destructor TdxSpreadSheetXLSXReaderRels.Destroy;
begin
  FreeAndNil(FList);
  inherited Destroy;
end;

procedure TdxSpreadSheetXLSXReaderRels.Clear;
begin
  FList.Clear;
end;

function TdxSpreadSheetXLSXReaderRels.Find(const ID: AnsiString; out AItem: TdxSpreadSheetXLSXReaderRelsItem): Boolean;
var
  I: Integer;
begin
  for I := 0 to FList.Count - 1 do
    if SameText(ID, TdxSpreadSheetXLSXReaderRelsItem(FList.List[I]).ID) then
    begin
      AItem := TdxSpreadSheetXLSXReaderRelsItem(FList.List[I]);
      Exit(True);
    end;

  Result := False;
end;

function TdxSpreadSheetXLSXReaderRels.FindByType(
  const AType: AnsiString; out AItem: TdxSpreadSheetXLSXReaderRelsItem): Boolean;
var
  I: Integer;
begin
  AItem := nil;
  for I := 0 to FList.Count - 1 do
    if SameText(AType, TdxSpreadSheetXLSXReaderRelsItem(FList.List[I]).FileType) then
    begin
      AItem := TdxSpreadSheetXLSXReaderRelsItem(FList.List[I]);
      Break;
    end;

  Result := AItem <> nil;
end;

procedure TdxSpreadSheetXLSXReaderRels.Load(AReader: TdxSpreadSheetXLSXReader; const AOwnerFileName: AnsiString);
var
  ADocument: TdxXMLDocument;
  AFileName: AnsiString;
  ANode: TdxXMLNode;
  AType, ARootPath: AnsiString;
begin
  Clear;
  AFileName := TdxSpreadSheetXLSXUtils.GetRelsFileNameForFile(AOwnerFileName);
  if AReader.FileExists(AFileName) then
  begin
    ADocument := AReader.ReadXML(AFileName);
    try
      if ADocument.Root.First <> nil then
      begin
        ANode := ADocument.Root.First.First;
        ARootPath := TdxZIPPathHelper.ExtractFilePath(AOwnerFileName);
        while ANode <> nil do
        begin
          AFileName := ANode.Attributes.GetValue(sdxXLSXAttrTarget);
          AFileName := TdxZIPPathHelper.EncodePath(AFileName);
          AType := ANode.Attributes.GetValue(sdxXLSXAttrType);
          if not SameText(ANode.Attributes.GetValue(sdxXLSXAttrTargetMode), sdxXLSXValueTargetModeExternal) and
            (AType <> sdxXLSXHyperlinkRelationship) then
            AFileName := TdxZIPPathHelper.AbsoluteFileName(ARootPath, AFileName);
          Items[ANode.Attributes.GetValue(sdxXLSXAttrId)].SetValue(TdxZIPPathHelper.ExpandFileName(AFileName), AType);
          ANode := ANode.Next;
        end;
      end;
    finally
      ADocument.Free;
    end;
  end;
end;

function TdxSpreadSheetXLSXReaderRels.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TdxSpreadSheetXLSXReaderRels.GetItem(const ID: AnsiString): TdxSpreadSheetXLSXReaderRelsItem;
begin
  if not Find(ID, Result) then
  begin
    Result := TdxSpreadSheetXLSXReaderRelsItem.Create;
    Result.ID := ID;
    FList.Add(Result);
  end;
end;

function TdxSpreadSheetXLSXReaderRels.GetItemByIndex(Index: Integer): TdxSpreadSheetXLSXReaderRelsItem;
begin
  Result := TdxSpreadSheetXLSXReaderRelsItem(FList.Items[Index]);
end;

{ TdxSpreadSheetXLSXReaderCustomParser }

function TdxSpreadSheetXLSXReaderCustomParser.CheckListIndex(AIndex: Integer;
  AList: TList; const AMessage: string; AMessageType: TdxSpreadSheetMessageType): Boolean;
begin
  Result := (AIndex >= 0) and (AIndex < AList.Count);
  if not Result then
    DoError(AMessage, [AIndex], AMessageType);
end;

function TdxSpreadSheetXLSXReaderCustomParser.CheckListIndex(AIndex: Integer;
  AList: TdxSpreadSheetXLSXHashTableItemList; const AMessage: string; AMessageType: TdxSpreadSheetMessageType): Boolean;
begin
  Result := (AIndex >= 0) and (AIndex < AList.Count);
  if not Result then
    DoError(AMessage, [AIndex], AMessageType);
end;

function TdxSpreadSheetXLSXReaderCustomParser.DecodeColor(const ANode: TdxXMLNode): TColor;
var
  AAttr: TdxXMLNodeAttribute;
begin
  Result := clDefault;
  if ANode.Attributes.Find(sdxXLSXAttrRGB, AAttr) then
    Result := DecodeColor(AAttr.ValueAsString)
  else

  if ANode.Attributes.Find(sdxXLSXAttrTheme, AAttr) then
    ExtractThemedColor(ANode, AAttr.ValueAsInteger, Result)
  else

  if ANode.Attributes.Find(sdxXLSXAttrIndexed, AAttr) then
    ExtractIndexedColor(ANode, AAttr.ValueAsInteger, Result)
  else

  if not ANode.Attributes.GetValueAsBoolean(sdxXLSXAttrAuto) then
    Result := clNone;
end;

function TdxSpreadSheetXLSXReaderCustomParser.DecodeColor(const AHexCode: string): TColor;
begin
  Result := dxAlphaColorToColor(TdxColorHelper.HexCodeToAlphaColor(AHexCode, False));
end;

procedure TdxSpreadSheetXLSXReaderCustomParser.ExtractIndexedColor(ANode: TdxXMLNode; AColorIndex: Integer; var AColor: TColor);
begin
  if Owner.IndexedColors.Count > 0 then
  begin
    if AColorIndex >= Owner.IndexedColors.Count then
      AColor := clDefault
    else
      AColor := Owner.IndexedColors[AColorIndex];
  end
  else
  begin
    if AColorIndex >= 8 then
      Dec(AColorIndex, 8);

    if (AColorIndex >= Low(dxExcelStandardColors)) and (AColorIndex <= High(dxExcelStandardColors)) then
      AColor := dxExcelStandardColors[AColorIndex]
    else
      case AColorIndex of
        56, 57:
          AColor := clDefault;
      else
        DoError(sdxErrorInvalidColorIndex, [AColorIndex], ssmtWarning);
      end;
  end;
end;

procedure TdxSpreadSheetXLSXReaderCustomParser.ExtractThemedColor(ANode: TdxXMLNode; AThemeIndex: Integer; var AColor: TColor);
var
  AAttr: TdxXMLNodeAttribute;
begin
  if CheckListIndex(AThemeIndex, Owner.ThemedColors, sdxErrorInvalidColorIndex, ssmtWarning) then
  begin
    AColor := TColor(Owner.ThemedColors[AThemeIndex]);
    if ANode.Attributes.Find(sdxXLSXAttrThemeTint, AAttr) then
      AColor := TdxSpreadSheetColorHelper.ApplyTint(AColor, AAttr.ValueAsFloat);
  end;
end;

function TdxSpreadSheetXLSXReaderCustomParser.GetOwner: TdxSpreadSheetXLSXReader;
begin
  Result := TdxSpreadSheetXLSXReader(inherited Owner);
end;

{ TdxSpreadSheetXLSXReaderCustomNodeParser }

constructor TdxSpreadSheetXLSXReaderCustomNodeParser.Create(ANode: TdxXMLNode; AOwner: TdxSpreadSheetXLSXReader);
begin
  inherited Create(AOwner);
  FNode := ANode;
end;

{ TdxSpreadSheetXLSXReaderCustomDocumentParser }

constructor TdxSpreadSheetXLSXReaderCustomDocumentParser.Create(const AFileName: AnsiString; AOwner: TdxSpreadSheetXLSXReader);
begin
  inherited Create(nil, AOwner);
  FDocumentFileName := AFileName;
  FDocument := ReadXML(AFileName);
  FNode := Document.Root.First;
  if FNode = nil then
    FNode := Document.Root;
  FRels := TdxSpreadSheetXLSXReaderRels.Create;
  FRels.Load(Owner, AFileName);
end;

destructor TdxSpreadSheetXLSXReaderCustomDocumentParser.Destroy;
begin
  FreeAndNil(FDocument);
  FreeAndNil(FRels);
  inherited Destroy;
end;

{ TdxSpreadSheetXLSXReaderCustomDocumentSubParser }

constructor TdxSpreadSheetXLSXReaderCustomDocumentSubParser.Create(
  ANode: TdxXMLNode; AOwnerParser: TdxSpreadSheetXLSXReaderCustomDocumentParser);
begin
  inherited Create(ANode, AOwnerParser.Owner);
  FOwnerParser := AOwnerParser;
end;

function TdxSpreadSheetXLSXReaderCustomDocumentSubParser.ReadEmbeddedImage(const AValue: TdxXMLString; out AStream: TStream): Boolean;
var
  AItem: TdxSpreadSheetXLSXReaderRelsItem;
begin
  Result := False;
  if OwnerParser.Rels.Find(AValue, AItem) then
  begin
    Result := Owner.FileExists(AItem.FileName);
    if Result then
      AStream := Owner.ReadFile(AItem.FileName)
    else
      DoError(sdxErrorPictureCannotBeFound, [AItem.FileName], ssmtWarning);
  end
  else
    DoError(sdxErrorPictureCannotBeFound, [OwnerParser.DocumentFileName + ':' + AValue], ssmtWarning)
end;

function TdxSpreadSheetXLSXReaderCustomDocumentSubParser.ReadImage(ANode: TdxXMLNode; out AStream: TStream): Boolean;
var
  AAttr: TdxXMLNodeAttribute;
begin
  Result := False;
  if ANode.Attributes.Find(sdxXLSXAttrDrawingResourceEmbed, AAttr) then
    Result := ReadEmbeddedImage(AAttr.Value, AStream)
  else
      DoError(sdxErrorPictureCannotBeFound, [OwnerParser.DocumentFileName], ssmtWarning);
end;

{ TdxSpreadSheetXLSXHashTableItemList }

procedure TdxSpreadSheetXLSXHashTableItemList.Notify(const Item: TdxHashTableItem; Action: TCollectionNotification);
begin
  inherited Notify(Item, Action);
  case Action of
    cnAdded:
      Item.AddRef;
    cnRemoved:
      Item.Release;
  end;
end;

{ TdxSpreadSheetXLSXReader }

constructor TdxSpreadSheetXLSXReader.Create(AOwner: TdxCustomSpreadSheet; AStream: TStream);
begin
  inherited Create(AOwner, AStream);
  FIndexedColors := TList<TColor>.Create;
  FFormulasRefs := TdxSpreadSheetXLSXFormulaAsTextInfoList.Create(SpreadSheet);
  FHyperlinks := TStringList.Create;
  FConditionalFormattingStyles := TdxSpreadSheetXLSXHashTableItemList.Create;
  FSharedStrings := TdxSpreadSheetXLSXHashTableItemList.Create;
  FThemedColors := TList.Create;
  FStyles := TdxSpreadSheetXLSXHashTableItemList.Create;
  FFormats := TdxSpreadSheetXLSXHashTableItemList.Create;
  FBorders := TdxSpreadSheetXLSXHashTableItemList.Create;
  FFills := TdxSpreadSheetXLSXHashTableItemList.Create;
  FFonts := TdxSpreadSheetXLSXHashTableItemList.Create;
  FThemedBrushes := TObjectList<TdxGPBrush>.Create;
  FThemedPens := TObjectList<TdxGPPen>.Create;
  FColorMap := TDictionary<TdxXMLString, TColor>.Create;
end;

destructor TdxSpreadSheetXLSXReader.Destroy;
begin
  FreeAndNil(FColorMap);
  FreeAndNil(FIndexedColors);
  FreeAndNil(FThemedBrushes);
  FreeAndNil(FThemedPens);
  FreeAndNil(FContentIndex);
  FreeAndNil(FConditionalFormattingStyles);
  FreeAndNil(FColumnWidthHelper);
  FreeAndNil(FFormats);
  FreeAndNil(FBorders);
  FreeAndNil(FStyles);
  FreeAndNil(FFills);
  FreeAndNil(FFonts);
  FreeAndNil(FSharedStrings);
  FreeAndNil(FFormulasRefs);
  FreeAndNil(FThemedColors);
  FreeAndNil(FHyperlinks);
  inherited Destroy;
end;

function TdxSpreadSheetXLSXReader.GetWorkbookFileName(out AFileName: AnsiString): Boolean;
const
  WorkbookSheetType = AnsiString('spreadsheetml.sheet.main');
  WorkbookTemplateType = AnsiString('spreadsheetml.template.main');
var
  AContentType: AnsiString;
  AItem: TdxSpreadSheetXLSXReaderRelsItem;
  ARels: TdxSpreadSheetXLSXReaderRels;
begin
  Result := False;
  ARels := TdxSpreadSheetXLSXReaderRels.Create;
  try
    ARels.Load(Self, '');
    if ARels.FindByType(sdxXLSXWorkbookRelationship, AItem) then
    begin
      AContentType := LowerCase(ContentIndex.ContentType[dxUnixPathDelim + AItem.FileName]);
      Result := (AContentType = '') or
        (Pos(WorkbookSheetType, AContentType) > 0) or
        (Pos(WorkbookTemplateType, AContentType) > 0);
      if Result then
        AFileName := AItem.FileName;
    end;
  finally
    ARels.Free;
  end;
end;

procedure TdxSpreadSheetXLSXReader.ReadData;
var
  AFileName: AnsiString;
begin
  if GetWorkbookFileName(AFileName) then
  begin
    ExecuteSubTask(TdxSpreadSheetXLSXReaderWorkbookParser.Create(AFileName, Self));
    ExecuteSubTask(TdxSpreadSheetXLSXReaderFormulasParser.Create(Self));
  end
  else
    DoError(sdxErrorInvalidDocumentType, ssmtError);
end;

function TdxSpreadSheetXLSXReader.CreateProgressHelper: TdxSpreadSheetCustomFilerProgressHelper;
begin
  Result := TdxSpreadSheetCustomFilerProgressHelper.Create(Self, 5);
end;

function TdxSpreadSheetXLSXReader.CreateXML: TdxSpreadSheetXMLDocument;
begin
  Result := TdxSpreadSheetOpenXMLDocument.Create(nil);
end;

procedure TdxSpreadSheetXLSXReader.ForEachNodeChild(ANode: TdxXMLNode; AProc: TdxXMLNodeForEachProc; AUserData: Pointer);
var
  AChildNode: TdxXMLNode;
begin
  if ANode <> nil then
  begin
    ProgressHelper.BeginStage(ANode.Count);
    try
      AChildNode := ANode.First;
      while AChildNode <> nil do
      begin
        AProc(AChildNode, AUserData);
        ProgressHelper.NextTask;
        AChildNode := AChildNode.Next;
      end;
    finally
      ProgressHelper.EndStage;
    end;
  end
  else
    ProgressHelper.SkipStage;
end;

procedure TdxSpreadSheetXLSXReader.ResolveHyperlinks;
var
  I: Integer;
begin
  for I := 0 to Hyperlinks.Count - 1 do
    TdxSpreadSheetHyperlink(Hyperlinks.Objects[I]).Value := Hyperlinks[I];
end;

function TdxSpreadSheetXLSXReader.GetColumnWidthHelper: TdxSpreadSheetExcelColumnWidthHelper;
begin
  if FColumnWidthHelper = nil then
  begin
    FColumnWidthHelper := TdxSpreadSheetExcelColumnWidthHelper.Create;
    CellStyles.Fonts.DefaultFont.AssignToFont(FColumnWidthHelper.Font);
  end;
  Result := FColumnWidthHelper;
end;

function TdxSpreadSheetXLSXReader.GetContentIndex: TdxSpreadSheetXLSXReaderContentIndex;
begin
  if FContentIndex = nil then
  begin
    FContentIndex := TdxSpreadSheetXLSXReaderContentIndex.Create;
    FContentIndex.Load(Self, sdxXLSXContentTypeFileName);
  end;
  Result := FContentIndex;
end;

{ TdxSpreadSheetXLSXReaderBrushParser }

constructor TdxSpreadSheetXLSXReaderBrushParser.Create(
  ANode: TdxXMLNode; ABrush: TdxGPBrush; AOwnerParser: TdxSpreadSheetXLSXReaderCustomDocumentParser);
begin
  inherited Create(ANode, AOwnerParser);
  FBrush := ABrush;
end;

procedure TdxSpreadSheetXLSXReaderBrushParser.Execute;
begin
  if SameText(Node.Name, sdxXLSXNodeSolidFill) then
    ReadSolidBrushParameters
  else if SameText(Node.Name, sdxXLSXNodeTexturedFill) then
    ReadTexturedBrushParameters
  else if SameText(Node.Name, sdxXLSXNodeGradientFill) then
    ReadGradientBrushParameters
  else if SameText(Node.Name, sdxXLSXNodePatternFill) then
    ReadPatternBrushParameters
  else if SameText(Node.Name, sdxXLSXNodeNoFill) then
    Brush.Style := gpbsClear;
end;

procedure TdxSpreadSheetXLSXReaderBrushParser.ProcessGradientBrushPoints(ANode: TdxXMLNode; AUserData: Pointer);
var
  APosition: Double;
begin
  APosition := TdxSpreadSheetXLSXUtils.DecodePercents(ANode.Attributes.GetValueAsInteger(sdxXLSXAttrGradientPointPos)) / 100;
  TdxGPBrush(AUserData).GradientPoints.Add(Min(Max(APosition, 0), 1), ReadColor(ANode));
end;

function TdxSpreadSheetXLSXReaderBrushParser.ReadColor(AParentNode: TdxXMLNode): TdxAlphaColor;

  function ReadAlpha(AColorNode: TdxXMLNode): Byte;
  var
    AAlphaNode: TdxXMLNode;
  begin
    if AColorNode.FindChild(sdxXLSXNodeColorAlpha, AAlphaNode) then
      Result := TdxSpreadSheetXLSXUtils.DecodeColorAlpha(AAlphaNode.Attributes.GetValueAsInteger(sdxXLSXAttrVal))
    else
      Result := MaxByte;
  end;

  function ReadSchemeColor(AColorNode: TdxXMLNode): TColor;
  var
    AColorHSL: TdxHSL;
    ANode: TdxXMLNode;
  begin
    if Owner.ColorMap.TryGetValue(AColorNode.Attributes.GetValue(sdxXLSXAttrVal), Result) then
    begin
      AColorHSL := TdxColorSpaceConverter.ColorToHSL(Result);
      if AColorNode.FindChild(sdxXLSXNodeLumMod, ANode) then
        AColorHSL.L := AColorHSL.L * TdxSpreadSheetXLSXUtils.DecodePercents(ANode.Attributes.GetValueAsInteger(sdxXLSXAttrVal)) / 100;
      if AColorNode.FindChild(sdxXLSXNodeLumOff, ANode) then
        AColorHSL.L := AColorHSL.L + TdxSpreadSheetXLSXUtils.DecodePercents(ANode.Attributes.GetValueAsInteger(sdxXLSXAttrVal)) / 100;
      AColorHSL.L := Min(Max(AColorHSL.L, 0), 1);
      Result := TdxColorSpaceConverter.HSLToColor(AColorHSL);

      if AColorNode.FindChild(sdxXLSXNodeShade, ANode) then
      begin
        Result := dxGetMiddleRGB(Result, clGray,
          Trunc(TdxSpreadSheetXLSXUtils.DecodePercents(ANode.Attributes.GetValueAsInteger(sdxXLSXAttrVal))));
      end;
    end
    else
      Result := clNone;
  end;

var
  AChildNode: TdxXMLNode;
begin
  AChildNode := AParentNode.First;
  while AChildNode <> nil do
  begin
    if SameText(AChildNode.Name, sdxXLSXNodeThemesCustomColor) then
      Exit(dxColorToAlphaColor(DecodeColor(AChildNode.Attributes.GetValueAsString(sdxXLSXAttrVal)), ReadAlpha(AChildNode)));
    if SameText(AChildNode.Name, sdxXLSXNodeSchemeColor) then
      Exit(dxColorToAlphaColor(ReadSchemeColor(AChildNode), ReadAlpha(AChildNode)));
    if SameText(AChildNode.Name, sdxXLSXNodeSystemColor) then
      Exit(dxColorToAlphaColor(DecodeColor(AChildNode.Attributes.GetValueAsString(sdxXLSXAttrLastColor))));
    AChildNode := AChildNode.Next;
  end;
  Result := clNone;
end;

procedure TdxSpreadSheetXLSXReaderBrushParser.ReadGradientBrushParameters;
var
  AChildNode: TdxXMLNode;
  AInverseOrder: Boolean;
begin
  Brush.Style := gpbsGradient;
  if Node.FindChild(sdxXLSXNodeGradientPoints, AChildNode) then
  begin
    Brush.GradientPoints.Clear;
    AChildNode.ForEach(ProcessGradientBrushPoints, Brush);
  end;

  if Node.FindChild(sdxXLSXNodeLinearGradientFill, AChildNode) then
  begin
    Brush.GradientMode := dxGetNearestGradientMode(TdxSpreadSheetXLSXUtils.DecodePositiveFixedAngle(
      AChildNode.Attributes.GetValueAsInteger(sdxXLSXAttrAng)), AInverseOrder);
    if AInverseOrder then
      Brush.GradientPoints.InvertOrder;
  end;
end;

procedure TdxSpreadSheetXLSXReaderBrushParser.ReadPatternBrushParameters;
var
  ABackgroundColor: TdxAlphaColor;
  AChildNode: TdxXMLNode;
  AForegroundColor: TdxAlphaColor;
begin
  if Node.FindChild(sdxXLSXNodeDrawingPatternBackgroundColor, AChildNode) then
    ABackgroundColor := ReadColor(AChildNode)
  else
    ABackgroundColor := 0;

  if Node.FindChild(sdxXLSXNodeDrawingPatternForegroundColor, AChildNode) then
    AForegroundColor := ReadColor(AChildNode)
  else
    AForegroundColor := 0;

  dxSpreadSheetLoadBrushPattern(Brush, HInstance, 'XLSX_BRUSHPATTERN_' +
    UpperCase(Node.Attributes.GetValueAsString(sdxXLSXAttrPreset)), AForegroundColor, ABackgroundColor);
end;

procedure TdxSpreadSheetXLSXReaderBrushParser.ReadSolidBrushParameters;
begin
  Brush.Style := gpbsSolid;
  Brush.Color := ReadColor(Node);
end;

procedure TdxSpreadSheetXLSXReaderBrushParser.ReadTexturedBrushParameters;
var
  AChildNode: TdxXMLNode;
  AStream: TStream;
begin
  Brush.Style := gpbsClear;
  if Node.FindChild(sdxXLSXNodeDrawingBlip, AChildNode) then
  begin
    if ReadImage(AChildNode, AStream) then
    try
      Brush.Texture.LoadFromStream(AStream);
      Brush.Style := gpbsTexture;
    finally
      AStream.Free;
    end;
  end;
end;

{ TdxSpreadSheetXLSXReaderPenParser }

constructor TdxSpreadSheetXLSXReaderPenParser.Create(
  ANode: TdxXMLNode; APen: TdxGPPen; AOwnerParser: TdxSpreadSheetXLSXReaderCustomDocumentParser);
begin
  inherited Create(ANode, AOwnerParser);
  FPen := APen;
end;

procedure TdxSpreadSheetXLSXReaderPenParser.Execute;
var
  AAttr: TdxXMLNodeAttribute;
  AChildNode: TdxXMLNode;
begin
  if Node.Attributes.Find(sdxXLSXAttrLineWidth, AAttr) then
    Pen.Width := dxEMUToPixelsF(AAttr.ValueAsInt64);
  if Node.FindChild(sdxXLSXNodeLineDash, AChildNode) then
    Pen.Style := TdxSpreadSheetXLSXHelper.StringToPenStyle(AChildNode.Attributes.GetValue(sdxXLSXAttrVal));
  if Node.Count > 0 then
    ExecuteSubTask(TdxSpreadSheetXLSXReaderBrushParser.Create(Node.First, Pen.Brush, OwnerParser));
end;

{ TdxSpreadSheetXLSXReaderExternalLinkParser }

procedure TdxSpreadSheetXLSXReaderExternalLinkParser.Execute;
var
  AChildNode: TdxXMLNode;
  ARelsItem: TdxSpreadSheetXLSXReaderRelsItem;
  ARID: AnsiString;
begin
  if (Node <> nil) and Node.FindChild(sdxXLSXNodeExternalBook, AChildNode) then
    ARID := AChildNode.Attributes.GetValue(sdxXLSXAttrRId)
  else
    ARID := '';

  if Rels.Find(ARID, ARelsItem) then
    SpreadSheet.ExternalLinks.Add(DecodePath(ARelsItem.FileName))
  else
    DoError(sdxErrorInvalidRelationshipId, [ARID], ssmtError);
end;

function TdxSpreadSheetXLSXReaderExternalLinkParser.DecodePath(const APath: AnsiString): string;
begin
  Result := dxXMLStringToString(APath);
end;

{ TdxSpreadSheetXLSXReaderFontParser }

constructor TdxSpreadSheetXLSXReaderFontParser.Create(
  AFont: TdxSpreadSheetFontHandle; ANode: TdxXMLNode; AOwner: TdxSpreadSheetXLSXReader);
begin
  inherited Create(AOwner);
  FFont := AFont;
  FNode := ANode;
end;

procedure TdxSpreadSheetXLSXReaderFontParser.Execute;
begin
  FNode.ForEach(ProcessParam);
end;

procedure TdxSpreadSheetXLSXReaderFontParser.ProcessParam(ANode: TdxXMLNode; AUserData: Pointer);
var
  AStyle: TFontStyle;
begin
  if SameText(ANode.Name, sdxXLSXNodeVertAlign) then
  begin
    if SameText(ANode.Attributes.GetValueAsString(sdxXLSXAttrVal), sdxXLSXValueSuperscript) then
      Font.Script := fsSuperscript
    else
    if SameText(ANode.Attributes.GetValueAsString(sdxXLSXAttrVal), sdxXLSXValueSubscript) then
        Font.Script := fsSubscript;
  end
  else

  if SameText(ANode.Name, sdxXLSXNodeSZ) then
    Font.Size := Round(ANode.Attributes.GetValueAsFloat(sdxXLSXAttrVal))
  else

  if SameText(ANode.Name, sdxXLSXNodeName) or SameText(ANode.Name, sdxXLSXNodeFontName) then
    Font.Name := ANode.Attributes.GetValueAsString(sdxXLSXAttrVal)
  else

  if SameText(ANode.Name, sdxXLSXNodeCharset) then
    Font.Charset := ANode.Attributes.GetValueAsInteger(sdxXLSXAttrVal)
  else

  if SameText(ANode.Name, sdxXLSXNodeColor) then
    Font.Color := DecodeColor(ANode)
  else
    for AStyle := Low(AStyle) to High(AStyle) do
      if SameText(dxXLSXFontStyles[AStyle], ANode.Name) and ANode.Attributes.GetValueAsBoolean('val', True) then
      begin
        Font.Style := Font.Style + [AStyle];
        Break;
      end;
end;

{ TdxSpreadSheetXLSXReaderFormulasParser }

procedure TdxSpreadSheetXLSXReaderFormulasParser.Execute;
begin
  try
    Owner.ResolveHyperlinks;
    Owner.FormulasRefs.ResolveReferences;
  except
    on E: Exception do
      DoError(E.Message, ssmtError);
  end;
end;

{ TdxSpreadSheetXLSXReaderHeaderFooterParser }

constructor TdxSpreadSheetXLSXReaderHeaderFooterParser.Create(ANode: TdxXMLNode;
  AOwner: TdxSpreadSheetXLSXReader; AText: TdxSpreadSheetTableViewOptionsPrintHeaderFooterText);
begin
  inherited Create(ANode, AOwner);
  FText := AText;
end;

procedure TdxSpreadSheetXLSXReaderHeaderFooterParser.Execute;
begin
  if Node <> nil then
    TdxSpreadSheetHeaderFooterHelper.Parse(Text, Node.TextAsString);
end;

{ TdxSpreadSheetXLSXReaderSharedStringParser }

procedure TdxSpreadSheetXLSXReaderSharedStringParser.Execute;
begin
  Owner.ForEachNodeChild(Node, ProcessSharedString, nil)
end;

class function TdxSpreadSheetXLSXReaderSharedStringParser.ParseString(
  AOwner: TdxSpreadSheetXLSXReader; AStringNode: TdxXMLNode): TdxSpreadSheetSharedString;
var
  ATextNode: TdxXMLNode;
begin
  if AStringNode.FindChild(sdxXLSXNodeText, ATextNode) then
    Result := AOwner.AddSharedString(ATextNode.TextAsString)
  else
    Result := TdxSpreadSheetXLSXReaderRichTextParser.Parse(AOwner, AStringNode, False);
end;

procedure TdxSpreadSheetXLSXReaderSharedStringParser.ProcessSharedString(ANode: TdxXMLNode; AUserData: Pointer);
begin
  Owner.SharedStrings.Add(ParseString(Owner, ANode));
end;

{ TdxSpreadSheetXLSXReaderRichTextParser }

constructor TdxSpreadSheetXLSXReaderRichTextParser.Create(AOwner: TdxSpreadSheetXLSXReader);
begin
  inherited Create;
  FOwner := AOwner;
  FRuns := TdxSpreadSheetFormattedSharedStringRuns.Create;
  FText := TStringBuilder.Create;
end;

destructor TdxSpreadSheetXLSXReaderRichTextParser.Destroy;
begin
  FreeAndNil(FRuns);
  FreeAndNil(FText);
  inherited Destroy;
end;

class function TdxSpreadSheetXLSXReaderRichTextParser.Parse(
  AOwner: TdxSpreadSheetXLSXReader; ANode: TdxXMLNode; AMultiline: Boolean = True): TdxSpreadSheetFormattedSharedString;
begin
  with Create(AOwner) do
  try
    Multiline := AMultiline;
    Result := DoParse(ANode);
  finally
    Free;
  end;
end;

function TdxSpreadSheetXLSXReaderRichTextParser.CreateFontParser(
  AFont: TdxSpreadSheetFontHandle; ANode: TdxXMLNode): TdxSpreadSheetXLSXReaderFontParser;
begin
  Result := TdxSpreadSheetXLSXReaderFontParser.Create(AFont, ANode, Owner);
end;

function TdxSpreadSheetXLSXReaderRichTextParser.DoParse(ANode: TdxXMLNode): TdxSpreadSheetFormattedSharedString;
var
  ANodeNext: TdxXMLNode;
begin
  FNamespaceURI := ANode.NameScope;
  repeat
    ANode.ForEach(ProcessRichTextRun, nil);
    ANodeNext := ANode.Next;
    if not Multiline or (ANodeNext = nil) or (ANode.Next.Name <> ANode.Name) then
      Break;
    FText.Append(#13#10);
    ANode := ANodeNext;
  until False;
  Result := FOwner.CreateTempFormattedSharedString(FText.ToString);
  Result.Runs.Assign(FRuns);
  Result := FOwner.AddFormattedSharedString(Result);
end;

procedure TdxSpreadSheetXLSXReaderRichTextParser.ProcessRichTextRun(ANode: TdxXMLNode; AUserData: Pointer);
var
  AFont: TdxSpreadSheetFontHandle;
  AParagraphNode: TdxXMLNode;
  ATextNode: TdxXMLNode;
begin
  if ANode.NameWithoutNameScope = sdxXLSXNodeRichTextRun then
  begin
    if ANode.FindChild(FNamespaceURI + sdxXLSXNodeText, ATextNode) and (ATextNode.Text <> '') then
    begin
      AFont := FOwner.CreateTempFontHandle;
      if ANode.FindChild(FNamespaceURI + sdxXLSXNodeRichTextRunParagraph, AParagraphNode) then
        FOwner.ExecuteSubTask(CreateFontParser(AFont, AParagraphNode));
      if (FText.Length > 0) or not AFont.IsEqual(FOwner.CellStyles.Fonts.DefaultFont) then
        FRuns.Add(FText.Length + 1, FOwner.AddFont(AFont))
      else
        AFont.Free;
      FText.Append(ATextNode.TextAsString);
    end;
  end;
end;

{ TdxSpreadSheetXLSXReaderStyleParser }

procedure TdxSpreadSheetXLSXReaderStyleParser.Execute;
var
  AChildNode: TdxXMLNode;
begin
  if Node.FindChild(['colors', 'indexedColors'], AChildNode) then
    AChildNode.ForEach(ProcessIndexedColors);
  Owner.ForEachNodeChild(Node, ProcessStyleCollection, nil);
end;

procedure TdxSpreadSheetXLSXReaderStyleParser.ProcessBorder(ANode: TdxXMLNode; AUserData: Pointer);
begin
  Owner.Borders.Add(ReadBorder(ANode));
end;

procedure TdxSpreadSheetXLSXReaderStyleParser.ProcessCellXfs(ANode: TdxXMLNode; AUserData: Pointer);
var
  AHandle: TdxSpreadSheetCellStyleHandle;
  AHelper: TdxSpreadSheetXLSXReaderStyleParserCellStyleHelper;
begin
  AHelper := TdxSpreadSheetXLSXReaderStyleParserCellStyleHelper.Create(ANode);
  try
    AHandle := Owner.CreateTempCellStyle(
      TdxSpreadSheetFontHandle(AHelper.GetResourceHandle(sdxXLSXAttrFontId, sdxXLSXAttrApplyFont, Owner.Fonts)),
      TdxSpreadSheetFormatHandle(AHelper.GetNumberFormatHandle(Owner)),
      TdxSpreadSheetBrushHandle(AHelper.GetResourceHandle(sdxXLSXAttrFillId, sdxXLSXAttrApplyFill, Owner.Fills)),
      TdxSpreadSheetBordersHandle(AHelper.GetResourceHandle(sdxXLSXAttrBorderId, sdxXLSXAttrApplyBorder, Owner.Borders)));

    AHandle.AlignHorz := TdxSpreadSheetXLSXHelper.StringToAlignHorz(AHelper.GetAlignmentAttrValue(sdxXLSXAttrHorizontal));
    AHandle.AlignHorzIndent := AHelper.GetAlignmentAttrValueAsInteger(sdxXLSXAttrIndent);
    if AHandle.AlignHorzIndent <> 0 then
      AHandle.AlignHorzIndent := Owner.ColumnWidthHelper.SpacesNumberToPixels(AHandle.AlignHorzIndent);
    AHandle.AlignVert := TdxSpreadSheetXLSXHelper.StringToAlignVert(AHelper.GetAlignmentAttrValue(sdxXLSXAttrVertical));
    AHandle.Rotation := AHelper.GetAlignmentAttrValueAsInteger(sdxXLSXAttrTextRotation);
    AHandle.States := [];
    if AHelper.GetAlignmentAttrValueAsBoolean(sdxXLSXAttrShrinkToFit) then
      AHandle.States := AHandle.States + [csShrinkToFit];
    if AHelper.GetAlignmentAttrValueAsBoolean(sdxXLSXAttrWrapText) then
      AHandle.States := AHandle.States + [csWordWrap];
    if AHelper.GetProtectionAttrValueAsBoolean(sdxXLSXAttrLocked, True) then
      AHandle.States := AHandle.States + [csLocked];
    if AHelper.GetProtectionAttrValueAsBoolean(sdxXLSXAttrHidden) then
      AHandle.States := AHandle.States + [csHidden];
    Owner.Styles.Add(Owner.AddCellStyle(AHandle));
  finally
    AHelper.Free;
  end;
end;

procedure TdxSpreadSheetXLSXReaderStyleParser.ProcessConditionalFormattingStyle(ANode: TdxXMLNode; AUserData: Pointer);
type
  TReadProc = function (ANode: TdxXMLNode): TdxHashTableItem of object;

  function ReadSubStyle(const AAttrName: TdxXMLString; AProc: TReadProc): TdxHashTableItem;
  var
    ASubNode: TdxXMLNode;
  begin
    if ANode.FindChild(AAttrName, ASubNode) then
      Result := AProc(ASubNode)
    else
      Result := nil;
  end;

var
  AStyle: TdxSpreadSheetCellStyleHandle;
  ASubNode: TdxXMLNode;
begin
  AStyle := Owner.CreateTempCellStyle(
    TdxSpreadSheetFontHandle(ReadSubStyle(sdxXLSXNodeStyleFont, ReadFont)),
    TdxSpreadSheetFormatHandle(ReadSubStyle(sdxXLSXNodeStyleNumberFormat, ReadFormat)),
    TdxSpreadSheetBrushHandle(ReadSubStyle(sdxXLSXNodeStyleFill, ReadConditionalFormattingFill)),
    TdxSpreadSheetBordersHandle(ReadSubStyle(sdxXLSXNodeStyleBorder, ReadBorder)));

  if ANode.FindChild(sdxXLSXNodeAlignment, ASubNode) then
  begin
    AStyle.States := [];
    if ASubNode.Attributes.GetValueAsBoolean(sdxXLSXAttrShrinkToFit) then
      AStyle.States := AStyle.States + [csShrinkToFit];
    if ASubNode.Attributes.GetValueAsBoolean(sdxXLSXAttrWrapText) then
      AStyle.States := AStyle.States + [csWordWrap];

    AStyle.AlignVert := TdxSpreadSheetXLSXHelper.StringToAlignVert(ASubNode.Attributes.GetValue(sdxXLSXAttrVertical));
    AStyle.AlignHorz := TdxSpreadSheetXLSXHelper.StringToAlignHorz(ASubNode.Attributes.GetValue(sdxXLSXAttrHorizontal));
    AStyle.AlignHorzIndent := ASubNode.Attributes.GetValueAsInteger(sdxXLSXAttrIndent);
    if AStyle.AlignHorzIndent <> 0 then
      AStyle.AlignHorzIndent := Owner.ColumnWidthHelper.SpacesNumberToPixels(AStyle.AlignHorzIndent);
    AStyle.Rotation := ASubNode.Attributes.GetValueAsInteger(sdxXLSXAttrTextRotation);
  end;

  Owner.ConditionalFormattingStyles.Add(Owner.AddCellStyle(AStyle));
end;

procedure TdxSpreadSheetXLSXReaderStyleParser.ProcessFill(ANode: TdxXMLNode; AUserData: Pointer);
begin
  Owner.Fills.Add(ReadFill(ANode));
end;

procedure TdxSpreadSheetXLSXReaderStyleParser.ProcessFont(ANode: TdxXMLNode; AUserData: Pointer);
var
  AFont: TdxSpreadSheetFontHandle;
begin
  AFont := TdxSpreadSheetFontHandle(ReadFont(ANode));
  if Owner.Fonts.Count = 0 then
    Owner.CellStyles.Fonts.DefaultFont.Assign(AFont);
  Owner.Fonts.Add(AFont);
end;

procedure TdxSpreadSheetXLSXReaderStyleParser.ProcessIndexedColors(ANode: TdxXMLNode; AUserData: Pointer);
begin
  Owner.IndexedColors.Add(DecodeColor(ANode.Attributes.GetValueAsString(sdxXLSXAttrRGB)));
end;

procedure TdxSpreadSheetXLSXReaderStyleParser.ProcessNumberFormat(ANode: TdxXMLNode; AUserData: Pointer);
begin
  Owner.Formats.Add(ReadFormat(ANode));
end;

procedure TdxSpreadSheetXLSXReaderStyleParser.ProcessStyleCollection(ANode: TdxXMLNode; AUserData: Pointer);
var
  AName: TdxXMLString;
begin
  AName := ANode.NameWithoutNameScope;
  if SameText(AName, sdxXLSXNodeStyleNumberFormats) then
    ANode.ForEach(ProcessNumberFormat)
  else

  if SameText(AName, sdxXLSXNodeStyleFonts) then
    ANode.ForEach(ProcessFont)
  else

  if SameText(AName, sdxXLSXNodeStyleFills) then
    ANode.ForEach(ProcessFill)
  else

  if SameText(AName, sdxXLSXNodeStyleBorders) then
    ANode.ForEach(ProcessBorder)
  else

  if SameText(AName, sdxXLSXNodeStyleCellXfs) then
    ANode.ForEach(ProcessCellXfs)
  else

  if SameText(AName, sdxXLSXNodeDXFS) then
    ANode.ForEach(ProcessConditionalFormattingStyle)
end;

function TdxSpreadSheetXLSXReaderStyleParser.ReadBorder(ANode: TdxXMLNode): TdxHashTableItem;
var
  ABorder: TcxBorder;
  AChildNode: TdxXMLNode;
  AHandle: TdxSpreadSheetBordersHandle;
begin
  AHandle := Owner.CreateTempBordersHandle;
  for ABorder := Low(ABorder) to High(ABorder) do
  begin
    if ANode.FindChild(dxXLSXBorderNames[ABorder], AChildNode) then
    begin
      AHandle.BorderStyle[ABorder] := TdxSpreadSheetXLSXHelper.StringToBorderStyle(AChildNode.Attributes.GetValue(sdxXLSXAttrStyle));
      if AChildNode.FindChild(sdxXLSXNodeColor, AChildNode) then
        AHandle.BorderColor[ABorder] := DecodeColor(AChildNode);
    end;
  end;
  Result := Owner.AddBorders(AHandle);
end;

function TdxSpreadSheetXLSXReaderStyleParser.ReadConditionalFormattingFill(ANode: TdxXMLNode): TdxHashTableItem;
begin
  if ANode.FindChild(sdxXLSXNodeCellStylePatternFill, ANode) then
    Result := ReadPatternFill(ANode, '')
  else
    Result := nil;
end;

function TdxSpreadSheetXLSXReaderStyleParser.ReadFill(ANode: TdxXMLNode): TdxHashTableItem;
begin
  if ANode.FindChild(sdxXLSXNodeCellStylePatternFill, ANode) then
    Result := ReadPatternFill(ANode, sdxXLSXValueNone)
  else
    Result := Owner.AddBrush(Owner.CreateTempBrushHandle);
end;

function TdxSpreadSheetXLSXReaderStyleParser.ReadFont(ANode: TdxXMLNode): TdxHashTableItem;
var
  AHandle: TdxSpreadSheetFontHandle;
begin
  AHandle := Owner.CreateTempFontHandle;
  ExecuteSubTask(TdxSpreadSheetXLSXReaderFontParser.Create(AHandle, ANode, Owner));
  Result := Owner.AddFont(AHandle);
end;

function TdxSpreadSheetXLSXReaderStyleParser.ReadFormat(ANode: TdxXMLNode): TdxHashTableItem;
begin
  Result := Owner.AddNumberFormat(
    ANode.Attributes.GetValueAsString(sdxXLSXAttrFormatCode),
    ANode.Attributes.GetValueAsInteger(sdxXLSXAttrNumFmtId, -1));
end;

function TdxSpreadSheetXLSXReaderStyleParser.ReadPatternFill(
  ANode: TdxXMLNode; const ADefaultPatternFillType: AnsiString): TdxHashTableItem;
var
  AChildNode: TdxXMLNode;
  AHandle: TdxSpreadSheetBrushHandle;
  APatternType: TdxXMLString;
begin
  AHandle := Owner.CreateTempBrushHandle;
  APatternType := ANode.Attributes.GetValue(sdxXLSXAttrPatternType, ADefaultPatternFillType);
  if APatternType <> sdxXLSXValueNone then
  begin
    AHandle.Style := TdxSpreadSheetXLSXHelper.StringToFillStyle(APatternType);
    if ANode.FindChild(sdxXLSXNodeBackgroundColor, AChildNode) then
      AHandle.BackgroundColor := DecodeColor(AChildNode);
    if ANode.FindChild(sdxXLSXNodeForegroundColor, AChildNode) then
    begin
      AHandle.ForegroundColor := DecodeColor(AChildNode);
      if (AHandle.Style = sscfsSolid) and (AHandle.ForegroundColor <> clNone) then
        AHandle.BackgroundColor := AHandle.ForegroundColor;
    end;
  end;
  Result := Owner.AddBrush(AHandle);
end;

{ TdxSpreadSheetXLSXReaderStyleParserCellStyleHelper }

constructor TdxSpreadSheetXLSXReaderStyleParserCellStyleHelper.Create(ANode: TdxXMLNode);
begin
  inherited Create;
  FNodeCellXf := ANode;
  FNodeCellXfAlignment := GetAlignmentNode(FNodeCellXf);
  FNodeCellXfProtection := GetProtectionNode(FNodeCellXf);

  FNodeCellStyleXf := GetCellXfsNode(ANode, sdxXLSXNodeStyleCellStyleXfs);
  FNodeCellStyleXfAlignment := GetAlignmentNode(FNodeCellStyleXf);
  FNodeCellStyleXfProtection := GetProtectionNode(FNodeCellStyleXf);
end;

function TdxSpreadSheetXLSXReaderStyleParserCellStyleHelper.FindAttr(
  const AAttrName: TdxXMLString; ANode, ANodeDefault: TdxXMLNode): TdxXMLNodeAttribute;
begin
  if (ANode = nil) or not ANode.Attributes.Find(AAttrName, Result) then
  begin
    if (ANodeDefault = nil) or not ANodeDefault.Attributes.Find(AAttrName, Result) then
      Result := nil;
  end;
end;

function TdxSpreadSheetXLSXReaderStyleParserCellStyleHelper.GetAlignmentAttrValue(
  const AAttrName, ADefaultValue: TdxXMLString): TdxXMLString;
var
  AAttr: TdxXMLNodeAttribute;
begin
  AAttr := FindAttr(AAttrName, FNodeCellXfAlignment, FNodeCellStyleXfAlignment);
  if AAttr <> nil then
    Result := AAttr.Value
  else
    Result := ADefaultValue;
end;

function TdxSpreadSheetXLSXReaderStyleParserCellStyleHelper.GetAlignmentAttrValueAsBoolean(
  const AAttrName: TdxXMLString; const ADefaultValue: Boolean = False): Boolean;
var
  AAttr: TdxXMLNodeAttribute;
begin
  AAttr := FindAttr(AAttrName, FNodeCellXfAlignment, FNodeCellStyleXfAlignment);
  if AAttr <> nil then
    Result := AAttr.ValueAsBoolean
  else
    Result := ADefaultValue;
end;

function TdxSpreadSheetXLSXReaderStyleParserCellStyleHelper.GetAlignmentAttrValueAsInteger(
  const AAttrName: TdxXMLString; const ADefaultValue: Integer = 0): Integer;
begin
  Result := StrToIntDef(dxAnsiStringToString(GetAlignmentAttrValue(AAttrName)), ADefaultValue);
end;

function TdxSpreadSheetXLSXReaderStyleParserCellStyleHelper.GetAttrValue(
  const AAttrName: TdxXMLString; ADefaultValue: Integer = 0): Integer;
var
  AAttr: TdxXMLNodeAttribute;
begin
  Result := ADefaultValue;
  AAttr := FindAttr(AAttrName, FNodeCellXf, FNodeCellStyleXf);
  if AAttr <> nil then
  try
    Result := AAttr.ValueAsInteger;
  except
    Result := ADefaultValue;
  end;
end;

function TdxSpreadSheetXLSXReaderStyleParserCellStyleHelper.GetAlignmentNode(ANode: TdxXMLNode): TdxXMLNode;
var
  AAttr: TdxXMLNodeAttribute;
begin
  Result := nil;
  if ANode <> nil then
  begin
    if not ANode.Attributes.Find(sdxXLSXAttrApplyAlignment, AAttr) or AAttr.ValueAsBoolean then
      Result := ANode.FindChild(sdxXLSXNodeAlignment);
  end;
end;

function TdxSpreadSheetXLSXReaderStyleParserCellStyleHelper.GetCellXfsNode(ANode: TdxXMLNode; const ANodeName: TdxXMLString): TdxXMLNode;
var
  AChildNode: TdxXMLNode;
  AValue: Integer;
begin
  Result := nil;
  if ANode.Parent.Parent.FindChild(ANodeName, AChildNode) then
  begin
    AValue := ANode.Attributes.GetValueAsInteger(sdxXLSXAttrXFId, -1);
    if (AValue >= 0) and (AValue < AChildNode.Count) then
      Result := AChildNode.Items[AValue];
  end;
end;

function TdxSpreadSheetXLSXReaderStyleParserCellStyleHelper.GetNumberFormatHandle(AReader: TdxSpreadSheetXLSXReader): TObject;
var
  AResIndex: Integer;
  I: Integer;
begin
  Result := nil;
  if GetAttrValue(sdxXLSXAttrApplyNumberFormat, 1) <> 0 then
  begin
    AResIndex := GetAttrValue(sdxXLSXAttrNumFmtId, -1);
    for I := 0 to AReader.Formats.Count - 1 do
    begin
      if TdxSpreadSheetFormatHandle(AReader.Formats[I]).FormatCodeID = AResIndex then
        Exit(TdxSpreadSheetFormatHandle(AReader.Formats[I]));
    end;
    Result := AReader.SpreadSheet.CellStyles.Formats.PredefinedFormats.GetFormatHandleByID(AResIndex);
  end;
end;

function TdxSpreadSheetXLSXReaderStyleParserCellStyleHelper.GetProtectionAttrValueAsBoolean(
  const AAttrName: TdxXMLString; const ADefaultValue: Boolean = False): Boolean;
var
  AAttr: TdxXMLNodeAttribute;
begin
  AAttr := FindAttr(AAttrName, FNodeCellXfProtection, FNodeCellStyleXfProtection);
  if AAttr <> nil then
    Result := AAttr.ValueAsBoolean
  else
    Result := ADefaultValue;
end;

function TdxSpreadSheetXLSXReaderStyleParserCellStyleHelper.GetProtectionNode(ANode: TdxXMLNode): TdxXMLNode;
begin
  Result := nil;
  if ANode <> nil then
    if ANode.Attributes.GetValueAsBoolean(sdxXLSXAttrApplyProtection) then
    begin
      if not ANode.FindChild(sdxXLSXNodeProtection, Result) then
        Result := nil;
    end;
end;

function TdxSpreadSheetXLSXReaderStyleParserCellStyleHelper.GetResourceHandle(
  const AResName, AOptionName: TdxXMLString; AResourceCollection: TdxSpreadSheetXLSXHashTableItemList): TObject;
var
  AResIndex: Integer;
begin
  Result := nil;
  if GetAttrValue(AOptionName, 1) <> 0 then
  begin
    AResIndex := GetAttrValue(AResName, -1);
    if (AResIndex >= 0) and (AResIndex < AResourceCollection.Count) then
      Result := TObject(AResourceCollection[AResIndex])
  end;
end;

{ TdxSpreadSheetXLSXReaderThemeParser }

procedure TdxSpreadSheetXLSXReaderThemeParser.Execute;
var
  AChildNode: TdxXMLNode;
begin
  if Node.FindChild([sdxXLSXNodeThemesElements, sdxXLSXNodeThemesColorScheme], AChildNode) then
    ReadColorSchema(AChildNode);
  if Node.FindChild([sdxXLSXNodeThemesElements, sdxXLSXNodeThemesFormatScheme, sdxXLSXNodeThemesFormatSchemeFillStyleList], AChildNode) then
    AChildNode.ForEach(ProcessFillStyle);
  if Node.FindChild([sdxXLSXNodeThemesElements, sdxXLSXNodeThemesFormatScheme, sdxXLSXNodeThemesFormatSchemeLineStyleList], AChildNode) then
    AChildNode.ForEach(ProcessLineStyle);
end;

procedure TdxSpreadSheetXLSXReaderThemeParser.ReadColorSchema(ANode: TdxXMLNode);
const
  Names: array[0..11] of AnsiString = (
    'lt1', 'dk1', 'lt2', 'dk2', 'accent1', 'accent2', 'accent3',
    'accent4', 'accent5', 'accent6', 'hlink', 'folHlink'
  );
var
  AChildNode: TdxXMLNode;
  AColor: TColor;
  I: Integer;
begin
  for I := Low(Names) to High(Names) do
  begin
    if ANode.FindChild('a:' + Names[I], AChildNode) and (AChildNode.First <> nil) then
      AColor := ProcessColor(AChildNode.First)
    else
    begin
      AColor := clDefault;
      DoError(sdxErrorColorValueIsNotSpecified, ssmtWarning);
    end;

    ColorMap.Add(Names[I], AColor);
    ThemedColors.Add(Pointer(AColor));
  end;

  ColorMap.AddOrSetValue('tx1', ColorMap.Items['dk2']);
  ColorMap.AddOrSetValue('tx2', ColorMap.Items['dk1']);
  ColorMap.AddOrSetValue('bg1', ColorMap.Items['lt1']);
  ColorMap.AddOrSetValue('bg2', ColorMap.Items['lt2']);
  ColorMap.AddOrSetValue('phClr', ColorMap.Items['accent1']);
end;

function TdxSpreadSheetXLSXReaderThemeParser.ProcessColor(ANode: TdxXMLNode): TColor;
begin
  Result := clDefault;
  if SameText(ANode.Name, sdxXLSXNodeThemesSystemColor) then
  begin
    if not IdentToColor('cl' + ANode.Attributes.GetValueAsString(sdxXLSXAttrVal), Integer(Result)) then
      DoError(sdxErrorInvalidColor, [ANode.Attributes.GetValue(sdxXLSXAttrVal)], ssmtWarning);
  end
  else
    if SameText(ANode.Name, sdxXLSXNodeThemesCustomColor) then
    try
      Result := DecodeColor(ANode.Attributes.GetValueAsString(sdxXLSXAttrVal));
    except
      DoError(sdxErrorInvalidColor, [ANode.Attributes.GetValue(sdxXLSXAttrVal)], ssmtWarning);
    end
    else
      DoError(sdxErrorColorValueIsNotSpecified, ssmtWarning);
end;

procedure TdxSpreadSheetXLSXReaderThemeParser.ProcessFillStyle(ANode: TdxXMLNode; AUserData: Pointer);
var
  ABrush: TdxGPBrush;
begin
  ABrush := TdxGPBrush.Create;
  ExecuteSubTask(TdxSpreadSheetXLSXReaderBrushParser.Create(ANode, ABrush, Self));
  ThemedBrushes.Add(ABrush);
end;

procedure TdxSpreadSheetXLSXReaderThemeParser.ProcessLineStyle(ANode: TdxXMLNode; AUserData: Pointer);
var
  APen: TdxGPPen;
begin
  APen := TdxGPPen.Create;
  ExecuteSubTask(TdxSpreadSheetXLSXReaderPenParser.Create(ANode, APen, Self));
  ThemedPens.Add(APen);
end;

function TdxSpreadSheetXLSXReaderThemeParser.GetColorMap: TDictionary<TdxXMLString, TColor>;
begin
  Result := Owner.ColorMap;
end;

function TdxSpreadSheetXLSXReaderThemeParser.GetThemedBrushes: TObjectList<TdxGPBrush>;
begin
  Result := Owner.ThemedBrushes;
end;

function TdxSpreadSheetXLSXReaderThemeParser.GetThemedColors: TList;
begin
  Result := Owner.ThemedColors;
end;

function TdxSpreadSheetXLSXReaderThemeParser.GetThemedPens: TObjectList<TdxGPPen>;
begin
  Result := Owner.ThemedPens;
end;

{ TdxSpreadSheetXLSXReaderWorkbookParser }

constructor TdxSpreadSheetXLSXReaderWorkbookParser.Create(const AFileName: AnsiString; AOwner: TdxSpreadSheetXLSXReader);
begin
  inherited Create(AOwner);
  FFileName := AFileName;
  FLocalSheetIdMap := TDictionary<string, TdxSpreadSheetCustomView>.Create;
  FRels := TdxSpreadSheetXLSXReaderRels.Create;
  FRels.Load(Owner, AFileName);
end;

destructor TdxSpreadSheetXLSXReaderWorkbookParser.Destroy;
begin
  FreeAndNil(FLocalSheetIdMap);
  FreeAndNil(FRels);
  inherited Destroy;
end;

procedure TdxSpreadSheetXLSXReaderWorkbookParser.Execute;
begin
  ParseWorkbookDependencies;
  ParseWorkbook;
end;

procedure TdxSpreadSheetXLSXReaderWorkbookParser.ParseWorkbook;
var
  ANode: TdxXMLNode;
  AWorkbook: TdxXMLDocument;
begin
  AWorkbook := ReadXML(FileName);
  try
    ParseWorkbookProperties(AWorkbook);
    Owner.ForEachNodeChild(AWorkbook.FindChild([sdxXLSXNodeWorkbook, sdxXLSXNodeSheets]), ProcessWorksheet, nil);
    Owner.ForEachNodeChild(AWorkbook.FindChild([sdxXLSXNodeWorkbook, sdxXLSXNodeExternalReferences]), ProcessExternalReference, nil);
    Owner.ForEachNodeChild(AWorkbook.FindChild([sdxXLSXNodeWorkbook, sdxXLSXNodeDefinedNames]), ProcessDefinedName, nil);

    if AWorkbook.FindChild([sdxXLSXNodeWorkbook, sdxXLSXNodeBookViews, sdxXLSXNodeWorkBookView], ANode) then
    begin
      SpreadSheet.ActiveSheetIndex := ANode.Attributes.GetValueAsInteger(sdxXLSXAttrActiveTab, SpreadSheet.ActiveSheetIndex);
      SpreadSheet.OptionsView.HorizontalScrollBar := ANode.Attributes.GetValueAsBoolean(sdxXLSXAttrShowHorizontalScroll, True);
      SpreadSheet.OptionsView.VerticalScrollBar := ANode.Attributes.GetValueAsBoolean(sdxXLSXAttrShowVerticalScroll, True);
      SpreadSheet.PageControl.Visible := ANode.Attributes.GetValueAsBoolean(sdxXLSXAttrShowSheetTabs, True);
    end;
  finally
    AWorkbook.Free;
  end;
end;

procedure TdxSpreadSheetXLSXReaderWorkbookParser.ParseWorkbookDependencies;
var
  AItem: TdxSpreadSheetXLSXReaderRelsItem;
begin
  if Rels.FindByType(sdxXLSXThemeRelationship, AItem) then
    ReadTheme(AItem.FileName);
  if Rels.FindByType(sdxXLSXStyleRelationship, AItem) then
    ReadStyles(AItem.FileName);
  if Rels.FindByType(sdxXLSXSharedStringRelationship, AItem) then
    ReadSharedStrings(AItem.FileName);
end;

procedure TdxSpreadSheetXLSXReaderWorkbookParser.ParseWorkbookProperties(AWorkbook: TdxXMLDocument);
const
  Map: array[Boolean] of TdxSpreadSheetDateTimeSystem = (dts1900, dts1904);
var
  ANode: TdxXMLNode;
begin
  if AWorkbook.FindChild([sdxXLSXNodeWorkbook, sdxXLSXNodeCalcPr], ANode) then
  begin
    SpreadSheet.OptionsBehavior.IterativeCalculationMaxCount := ANode.Attributes.GetValueAsInteger(sdxXLSXAttrIterateCount, 100);
    SpreadSheet.OptionsBehavior.IterativeCalculation := ANode.Attributes.GetValueAsBoolean(sdxXLSXAttrIterate);
    SpreadSheet.OptionsView.R1C1Reference := SameText(ANode.Attributes.GetValue(sdxXLSXAttrRefMode), sdxXLSXValueR1C1);
  end
  else
  begin
    SpreadSheet.OptionsBehavior.IterativeCalculation := False;
    SpreadSheet.OptionsBehavior.IterativeCalculationMaxCount := 100;
    SpreadSheet.OptionsView.R1C1Reference := False;
  end;

  if AWorkbook.FindChild([sdxXLSXNodeWorkbook, sdxXLSXNodeWorkbookPr], ANode) then
    SpreadSheet.OptionsView.DateTimeSystem := Map[ANode.Attributes.GetValueAsBoolean(sdxXLSXAttrDate1904)]
  else
    SpreadSheet.OptionsView.DateTimeSystem := dts1900;

  SpreadSheet.OptionsProtection.Protected := False;
  SpreadSheet.OptionsProtection.ProtectionInfo := nil;
  if AWorkbook.FindChild([sdxXLSXNodeWorkbook, sdxXLSXNodeWorkbookProtection], ANode) then
  begin
    SpreadSheet.OptionsProtection.Protected := True;
    SpreadSheet.OptionsProtection.AllowChangeStructure := not ANode.Attributes.GetValueAsBoolean(sdxXLSXAttrLockStructure);
    if ANode.Attributes.Exists(sdxXLSXAttrWorkbookAlgorithmName) then
      SpreadSheet.OptionsProtection.ProtectionInfo := ReadProtectionInfoStrong(ANode)
    else if ANode.Attributes.Exists(sdxXLSXAttrWorkbookPassword) then
      SpreadSheet.OptionsProtection.ProtectionInfo := ReadProtectionInfoStandard(ANode);
  end;
end;

procedure TdxSpreadSheetXLSXReaderWorkbookParser.ProcessDefinedName(ANode: TdxXMLNode; AUserData: Pointer);
var
  AAttr: TdxXMLNodeAttribute;
  ADefinedName: TdxSpreadSheetDefinedName;
  AScope: TdxSpreadSheetCustomView;
begin
  AScope := nil;
  if ANode.Attributes.Find(sdxXLSXAttrLocalSheetId, AAttr) then
  begin
    if not FLocalSheetIdMap.TryGetValue(AAttr.ValueAsString, AScope) then
      DoError(sdxErrorInvalidSheetId, [AAttr.ValueAsString], ssmtWarning);
  end;

  ADefinedName := SpreadSheet.DefinedNames.AddOrSet(ANode.Attributes.GetValueAsString(sdxXLSXAttrName), ANode.TextAsString, AScope);
  if ADefinedName.Caption = sdxXLSXPrintAreaDefinedName then
    TdxSpreadSheetPrintAreasHelper.ImportPrintArea(ADefinedName)
  else
    if ADefinedName.Caption = sdxXLSXPrintTitlesDefinedName then
      TdxSpreadSheetPrintAreasHelper.ImportPrintTitles(ADefinedName);
end;

procedure TdxSpreadSheetXLSXReaderWorkbookParser.ProcessExternalReference(ANode: TdxXMLNode; AUserData: Pointer);
var
  ARelsItem: TdxSpreadSheetXLSXReaderRelsItem;
begin
  if Rels.Find(ANode.Attributes.GetValue(sdxXLSXAttrRId), ARelsItem) then
    ExecuteSubTask(TdxSpreadSheetXLSXReaderExternalLinkParser.Create(ARelsItem.FileName, Owner))
  else
    DoError(sdxErrorInvalidRelationshipId, [ANode.Attributes.GetValue(sdxXLSXAttrRId)], ssmtError);
end;

procedure TdxSpreadSheetXLSXReaderWorkbookParser.ProcessWorksheet(ANode: TdxXMLNode; AUserData: Pointer);
var
  AItem: TdxSpreadSheetXLSXReaderRelsItem;
  AView: TdxSpreadSheetTableView;
begin
  if not Rels.Find(ANode.Attributes.GetValue(sdxXLSXAttrRId), AItem) then
    DoError(sdxErrorInvalidRelationshipId, [ANode.Attributes.GetValue(sdxXLSXAttrRId)], ssmtError)
  else
    if SameText(AItem.FileType, sdxXLSXWorksheetRelationship) then
    begin
      AView := Owner.AddTableView(ANode.Attributes.GetValueAsString(sdxXLSXAttrName));
      AView.Visible := not SameText(ANode.Attributes.GetValue(sdxXLSXAttrState), sdxXLSXValueHidden);
      FLocalSheetIdMap.Add(IntToStr(ANode.Index), AView);
      ExecuteSubTask(TdxSpreadSheetXLSXReaderWorksheetParser.Create(AItem.FileName, AView, Owner));
    end;
end;

procedure TdxSpreadSheetXLSXReaderWorkbookParser.ReadSharedStrings(const AFileName: AnsiString);
begin
  ExecuteSubTask(TdxSpreadSheetXLSXReaderSharedStringParser.Create(AFileName, Owner));
end;

procedure TdxSpreadSheetXLSXReaderWorkbookParser.ReadStyles(const AFileName: AnsiString);
begin
  ExecuteSubTask(TdxSpreadSheetXLSXReaderStyleParser.Create(AFileName, Owner));
end;

procedure TdxSpreadSheetXLSXReaderWorkbookParser.ReadTheme(const AFileName: AnsiString);
begin
  ExecuteSubTask(TdxSpreadSheetXLSXReaderThemeParser.Create(AFileName, Owner));
end;

function TdxSpreadSheetXLSXReaderWorkbookParser.ReadProtectionInfoStandard(ANode: TdxXMLNode): IdxSpreadSheetProtectionInfo;
var
  AProtection: TdxSpreadSheetStandardProtectionInfo;
begin
  AProtection := TdxSpreadSheetStandardProtectionInfo.Create;
  AProtection.KeyWordAsString := ANode.Attributes.GetValueAsString(sdxXLSXAttrWorkbookPassword);
  Result := AProtection;
end;

function TdxSpreadSheetXLSXReaderWorkbookParser.ReadProtectionInfoStrong(ANode: TdxXMLNode): IdxSpreadSheetProtectionInfo;
var
  AProtection: TdxSpreadSheetStrongProtectionInfo;
begin
  AProtection := TdxSpreadSheetStrongProtectionInfo.Create;
  AProtection.HashAlgorithm := TdxSpreadSheetXLSXHelper.StringToHashAlgorithm(
    ANode.Attributes.GetValueAsString(sdxXLSXAttrWorkbookAlgorithmName));
  AProtection.HashValueAsString := ANode.Attributes.GetValueAsString(sdxXLSXAttrWorkbookHashValue);
  AProtection.SaltValueAsString := ANode.Attributes.GetValueAsString(sdxXLSXAttrWorkbookSaltValue);
  AProtection.SpinCount := ANode.Attributes.GetValueAsInteger(sdxXLSXAttrWorkbookSpinCount);
  Result := AProtection;
end;

{ TdxSpreadSheetXLSXReaderWorksheetParser }

constructor TdxSpreadSheetXLSXReaderWorksheetParser.Create(
  const AFileName: AnsiString; AView: TdxSpreadSheetTableView; AOwner: TdxSpreadSheetXLSXReader);
begin
  inherited Create(AFileName, AOwner);
  FView := AView;
end;

procedure TdxSpreadSheetXLSXReaderWorksheetParser.Execute;
begin
  ReadCells;
  ReadPrinting;
  ReadViewProperties;
  ReadConditionalFormatting;
  ReadDrawings;
  ReadProtection;
end;

function TdxSpreadSheetXLSXReaderWorksheetParser.ConvertRowHeight(const AValue: Double): Integer;
begin
  Result := TdxValueUnitsHelper.PointsToPixels(AValue);
end;

function TdxSpreadSheetXLSXReaderWorksheetParser.ExtractColumnIndex(const S: string): Integer;
var
  X: Integer;
begin
  dxStringToReference(S, Result, X);
  if Result < 0 then
  begin
    DoError(sdxErrorInvalidColumnIndex, [S], ssmtError);
    Result := 0;
  end;
end;

procedure TdxSpreadSheetXLSXReaderWorksheetParser.ProcessBreaks(ANode: TdxXMLNode; AUserData: Pointer);
begin
  if ANode.Attributes.GetValueAsBoolean(sdxXLSXAttrBreakManual) then
    TList<Cardinal>(AUserData).Add(ANode.Attributes.GetValueAsInteger(sdxXLSXAttrBreakID));
end;

procedure TdxSpreadSheetXLSXReaderWorksheetParser.ProcessColumn(ANode: TdxXMLNode; AUserData: Pointer);
var
  AAttr: TdxXMLNodeAttribute;
  AColumn: TdxSpreadSheetTableColumnAccess;
  AGroup: TdxSpreadSheetTableItemGroupAccess;
  AHidden: Boolean;
  AIsCustomWidth: Boolean;
  AMax: Integer;
  AMin: Integer;
  AStyleHandle: TdxSpreadSheetCellStyleHandle;
  AStyleIndex: Integer;
  AWidth: Integer;
  I: Integer;
begin

  AMin := Min(ANode.Attributes.GetValueAsInteger(sdxXLSXAttrMin) - 1, MAXWORD);
  AMax := Min(ANode.Attributes.GetValueAsInteger(sdxXLSXAttrMax) - 1, MAXWORD);

  if AMax < 0 then
    Exit;

  TdxSpreadSheetOutlineHelper.IncreaseOutlineLevelTo(View.Columns, AMin, AMax,
    ANode.Attributes.GetValueAsInteger(sdxXLSXAttrOutlineLevel) - 1);
  AGroup := TdxSpreadSheetTableItemGroupAccess(View.Columns.Groups.Find(AMin - 1));
  if AGroup <> nil then
    AGroup.FCollapsedByUser := ANode.Attributes.GetValueAsBoolean(sdxXLSXAttrCollapsed);

  AIsCustomWidth := ANode.Attributes.GetValueAsBoolean(sdxXLSXAttrCustomWidth);
  if ANode.Attributes.Find(sdxXLSXAttrWidth, AAttr) then
    AWidth := Owner.ColumnWidthHelper.WidthToPixels(AAttr.ValueAsFloat)
  else
    AWidth := -1;

  AStyleIndex := ANode.Attributes.GetValueAsInteger(sdxXLSXAttrStyle, -1);
  if (AStyleIndex >= 0) and CheckListIndex(AStyleIndex, CellStyles, sdxErrorInvalidStyleIndex, ssmtError) then
    AStyleHandle := TdxSpreadSheetCellStyleHandle(CellStyles[AStyleIndex])
  else
    AStyleHandle := nil;

  AHidden := ANode.Attributes.GetValueAsBoolean(sdxXLSXAttrHidden);
  if (AMax = dxSpreadSheetMaxColumnIndex) and ((AMin = 0) or (AMin + 1 = View.Columns.Count)) then
  begin
    View.Columns.DefaultSize := AWidth;
    if AStyleHandle <> nil then
      SpreadSheet.DefaultCellStyle.Handle := AStyleHandle;
  end
  else
    for I := AMin to AMax do
    begin
      AColumn := TdxSpreadSheetTableColumnAccess(View.Columns.CreateItem(I));
      if AStyleHandle <> nil then
        AColumn.Style.Handle := AStyleHandle;
      if AWidth >= 0 then
      begin
        AColumn.Size := AWidth;
        AColumn.IsCustomSize := AIsCustomWidth;
      end;
      AColumn.Visible := not AHidden;
    end;
end;

procedure TdxSpreadSheetXLSXReaderWorksheetParser.ProcessFixedPane(ANode: TdxXMLNode);
var
  AColumnIndex: Integer;
  AFrozenColumn: Integer;
  AFrozenRow: Integer;
  ARowIndex: Integer;
begin
  AFrozenRow := ANode.Attributes.GetValueAsInteger(sdxXLSXAttrSplitY) - 1;
  AFrozenColumn := ANode.Attributes.GetValueAsInteger(sdxXLSXAttrSplitX) - 1;
  dxStringToReference(ANode.Parent.Attributes.GetValueAsString(sdxXLSXAttrTopLeftCell), AColumnIndex, ARowIndex);
  if (AColumnIndex >= 0) and (ARowIndex >= 0) then
  begin
    if AFrozenColumn >= 0 then
      Inc(AFrozenColumn, AColumnIndex);
    if AFrozenRow >= 0 then
      Inc(AFrozenRow, ARowIndex);
  end;
  View.FrozenColumn := AFrozenColumn;
  View.FrozenRow := AFrozenRow;

  dxStringToReference(ANode.Attributes.GetValueAsString(sdxXLSXAttrTopLeftCell), AColumnIndex, ARowIndex);
  if (AColumnIndex >= 0) and (ARowIndex >= 0) then
  begin
    TdxSpreadSheetTableViewAccess(View).ViewInfo.FirstScrollableColumn := AColumnIndex;
    TdxSpreadSheetTableViewAccess(View).ViewInfo.FirstScrollableRow := ARowIndex;
  end;
end;

procedure TdxSpreadSheetXLSXReaderWorksheetParser.ProcessHeaderFooter(ANode: TdxXMLNode);
var
  AHeaderFooter: TdxSpreadSheetTableViewOptionsPrintHeaderFooter;
begin
  AHeaderFooter := View.OptionsPrint.HeaderFooter;
  AHeaderFooter.AlignWithMargins := ANode.Attributes.GetValueAsDefaultBoolean(sdxXLSXAttrHeaderFooterAlignWithMargins);
  AHeaderFooter.ScaleWithDocument := ANode.Attributes.GetValueAsDefaultBoolean(sdxXLSXAttrHeaderFooterScaleWithDocument);

  ExecuteSubTask(TdxSpreadSheetXLSXReaderHeaderFooterParser.Create(ANode.FindChild(sdxXLSXNodeOddFooter), Owner, AHeaderFooter.CommonFooter));
  ExecuteSubTask(TdxSpreadSheetXLSXReaderHeaderFooterParser.Create(ANode.FindChild(sdxXLSXNodeOddHeader), Owner, AHeaderFooter.CommonHeader));

  if ANode.Attributes.GetValueAsBoolean(sdxXLSXAttrHeaderFooterDifferentFirst) then
  begin
    ExecuteSubTask(TdxSpreadSheetXLSXReaderHeaderFooterParser.Create(ANode.FindChild(sdxXLSXNodeFirstFooter), Owner, AHeaderFooter.FirstPageFooter));
    ExecuteSubTask(TdxSpreadSheetXLSXReaderHeaderFooterParser.Create(ANode.FindChild(sdxXLSXNodeFirstHeader), Owner, AHeaderFooter.FirstPageHeader));
  end;

  if ANode.Attributes.GetValueAsBoolean(sdxXLSXAttrHeaderFooterDifferentOddEven) then
  begin
    ExecuteSubTask(TdxSpreadSheetXLSXReaderHeaderFooterParser.Create(ANode.FindChild(sdxXLSXNodeEvenFooter), Owner, AHeaderFooter.EvenPagesFooter));
    ExecuteSubTask(TdxSpreadSheetXLSXReaderHeaderFooterParser.Create(ANode.FindChild(sdxXLSXNodeEvenHeader), Owner, AHeaderFooter.EvenPagesHeader));
  end;
end;

procedure TdxSpreadSheetXLSXReaderWorksheetParser.ProcessLegacyDrawings(ANode: TdxXMLNode);
var
  AItem: TdxSpreadSheetXLSXReaderRelsItem;
begin
  if Rels.FindByType(sdxXLSXCommentsRelationship, AItem) then
    ExecuteSubTask(TdxSpreadSheetXLSXReaderCommentsParser.Create(AItem.FileName, View, Owner));
  if Rels.Find(ANode.Attributes.GetValue(sdxXLSXAttrRId), AItem) then
    ExecuteSubTask(TdxSpreadSheetXLSXReaderCommentContainersParser.Create(AItem.FileName, View, Owner));
end;

procedure TdxSpreadSheetXLSXReaderWorksheetParser.ProcessHyperlinks(ANode: TdxXMLNode; AUserData: Pointer);
var
  AValue: string;
  AAttr: TdxXMLNodeAttribute;
  AHyperlink: TdxSpreadSheetHyperlink;
  ARelsItem: TdxSpreadSheetXLSXReaderRelsItem;
begin
  AHyperlink := View.Hyperlinks.Add(dxStringToReferenceArea(ANode.Attributes.GetValueAsString(sdxXLSXAttrRef)));
  if ANode.Attributes.Find(sdxXLSXAttrDisplay, AAttr) then
    AHyperlink.DisplayText := AAttr.ValueAsString;
  if ANode.Attributes.Find(sdxXLSXAttrTooltip, AAttr) then
    AHyperlink.ScreenTip := AAttr.ValueAsString;
  AValue := '';
  if ANode.Attributes.Find(sdxXLSXAttrLocation, AAttr) then
    AValue := dxSpreadSheetFormulaIncludeEqualSymbol(AAttr.ValueAsString)
  else
    if ANode.Attributes.Find(sdxXLSXAttrRId, AAttr) then
    begin
      if Rels.Find(AAttr.Value, ARelsItem) then
        AValue := dxXMLStringToString(ARelsItem.FileName);
    end;
  Owner.Hyperlinks.AddObject(AValue, AHyperlink);
end;

procedure TdxSpreadSheetXLSXReaderWorksheetParser.ProcessMergedCells(ANode: TdxXMLNode; AUserData: Pointer);
var
  ARect: TRect;
begin
  ARect := dxStringToReferenceArea(ANode.Attributes.GetValueAsString(sdxXLSXAttrRef));
  if (ARect.Left >= 0) and (ARect.Top >= 0) and (ARect.Right >= 0) and (ARect.Bottom >= 0) then
    View.MergedCells.Add(ARect);
end;

procedure TdxSpreadSheetXLSXReaderWorksheetParser.ProcessPageMargins(ANode: TdxXMLNode);
var
  AMargins: TdxSpreadSheetTableViewOptionsPrintPageMargins;
begin
  AMargins := View.OptionsPrint.Page.Margins;
  AMargins.Left := ANode.Attributes.GetValueAsFloat(sdxXLSXAttrPageMarginsLeft);
  AMargins.Top := ANode.Attributes.GetValueAsFloat(sdxXLSXAttrPageMarginsTop);
  AMargins.Right := ANode.Attributes.GetValueAsFloat(sdxXLSXAttrPageMarginsRight);
  AMargins.Bottom := ANode.Attributes.GetValueAsFloat(sdxXLSXAttrPageMarginsBottom);
  AMargins.Header := ANode.Attributes.GetValueAsFloat(sdxXLSXAttrPageMarginsHeader);
  AMargins.Footer := ANode.Attributes.GetValueAsFloat(sdxXLSXAttrPageMarginsFooter);
  AMargins.Assigned := True;
end;

procedure TdxSpreadSheetXLSXReaderWorksheetParser.ProcessPageSetup(ANode: TdxXMLNode);
var
  APage: TdxSpreadSheetTableViewOptionsPrintPage;
  APaper: TdxSpreadSheetTableViewOptionsPrintPagePaper;
  APrinting: TdxSpreadSheetTableViewOptionsPrintPrinting;
begin
  APage := View.OptionsPrint.Page;
  APage.FitToHeight := ANode.Attributes.GetValueAsInteger(sdxXLSXAttrPageSetupFitToHeight, 1);
  APage.FitToWidth := ANode.Attributes.GetValueAsInteger(sdxXLSXAttrPageSetupFitToWidth, 1);
  APage.Orientation := TdxSpreadSheetXLSXHelper.StringToPrintPageOrientation(ANode.Attributes.GetValue(sdxXLSXAttrPageSetupOrientation));
  APage.Scale := ANode.Attributes.GetValueAsInteger(sdxXLSXAttrPageSetupScale, 100);
  if ANode.Attributes.GetValueAsBoolean(sdxXLSXAttrPageSetupUseFirstPageNumber) then
    APage.FirstPageNumber := ANode.Attributes.GetValueAsInteger(sdxXLSXAttrPageSetupFirstPageNumber);

  APaper := View.OptionsPrint.Page.Paper;
  APaper.SizeID := ANode.Attributes.GetValueAsInteger(sdxXLSXAttrPageSetupPaperSize);
  APaper.CustomSize.X := TdxValueUnitsHelper.ValueToInchesF(ANode.Attributes.GetValueAsString(sdxXLSXAttrPageSetupPaperWidth));
  APaper.CustomSize.Y := TdxValueUnitsHelper.ValueToInchesF(ANode.Attributes.GetValueAsString(sdxXLSXAttrPageSetupPaperHeight));
  APaper.Assigned := (APaper.SizeID <> 0) or (APaper.CustomSize.X > 0) and (APaper.CustomSize.Y > 0);

  APrinting := View.OptionsPrint.Printing;
  APrinting.Draft := ANode.Attributes.GetValueAsDefaultBoolean(sdxXLSXAttrPageSetupDraft);
  APrinting.BlackAndWhite := ANode.Attributes.GetValueAsDefaultBoolean(sdxXLSXAttrPageSetupBlackAndWhite);
  APrinting.Copies := ANode.Attributes.GetValueAsInteger(sdxXLSXAttrPageSetupCopies);
  APrinting.PageOrder := TdxSpreadSheetXLSXHelper.StringToPrintPageOrder(ANode.Attributes.GetValue(sdxXLSXAttrPageSetupPageOrder));


  View.OptionsPrint.Source.ErrorIndication := TdxSpreadSheetXLSXHelper.StringToPrintErrorIndication(ANode.Attributes.GetValue(sdxXLSXAttrPageSetupErrors));
  View.OptionsPrint.Source.CellComments := TdxSpreadSheetXLSXHelper.StringToPrintCellComments(ANode.Attributes.GetValue(sdxXLSXAttrPageSetupCellComments));
end;

procedure TdxSpreadSheetXLSXReaderWorksheetParser.ProcessPageSetupExProperties(ANode: TdxXMLNode);
begin
  if ANode.Attributes.GetValueAsBoolean(sdxXLSXAttrFitToPage) then
    View.OptionsPrint.Page.ScaleMode := oppsmFitToPage
  else
    View.OptionsPrint.Page.ScaleMode := oppsmAdjustToScale;
end;

procedure TdxSpreadSheetXLSXReaderWorksheetParser.ProcessPrintOptions(ANode: TdxXMLNode);
var
  ASource: TdxSpreadSheetTableViewOptionsPrintSource;
begin
  View.OptionsPrint.Printing.HorizontalCentered := ANode.Attributes.GetValueAsDefaultBoolean(sdxXLSXAttrPrintOptionsHorzCenter);
  View.OptionsPrint.Printing.VerticalCentered := ANode.Attributes.GetValueAsDefaultBoolean(sdxXLSXAttrPrintOptionsVertCenter);

  ASource := View.OptionsPrint.Source;
  ASource.Headers := ANode.Attributes.GetValueAsDefaultBoolean(sdxXLSXAttrPrintOptionsHeadings);
  ASource.GridLines := ANode.Attributes.GetValueAsDefaultBoolean(sdxXLSXAttrPrintOptionsGridLines);
end;

procedure TdxSpreadSheetXLSXReaderWorksheetParser.ProcessProperties(ANode: TdxXMLNode);
var
  AAttr: TdxXMLNodeAttribute;
begin
  if ANode.Attributes.Find(sdxXLSXAttrDefaultColumnWidth, AAttr) then
    View.Columns.DefaultSize := Owner.ColumnWidthHelper.WidthToPixels(AAttr.ValueAsFloat)
  else
    View.Columns.DefaultSize := 3 + Owner.ColumnWidthHelper.CharsNumberToPixels(
      ANode.Attributes.GetValueAsInteger(sdxXLSXAttrBaseColumnWidth, 8));

  if ANode.Attributes.GetValueAsBoolean(sdxXLSXAttrCustomHeight, True) then
    View.Rows.DefaultSize := ConvertRowHeight(ANode.Attributes.GetValueAsFloat(sdxXLSXAttrDefaultRowHeight));
end;

procedure TdxSpreadSheetXLSXReaderWorksheetParser.ProcessRow(ANode: TdxXMLNode; AUserData: Pointer);
var
  AAttr: TdxXMLNodeAttribute;
  AGroup: TdxSpreadSheetTableItemGroupAccess;
  AIndex: Integer;
  ARow: TdxSpreadSheetTableRowAccess;
begin
  AIndex := ANode.Attributes.GetValueAsInteger(sdxXLSXAttrRowIndex) - 1;
  if AIndex < 0 then
    AIndex := View.Rows.Count;
  ARow := TdxSpreadSheetTableRowAccess(View.Rows.CreateItem(AIndex));

  TdxSpreadSheetOutlineHelper.IncreaseOutlineLevelTo(View.Rows,
    ARow.Index, ANode.Attributes.GetValueAsInteger(sdxXLSXAttrOutlineLevel) - 1);
  AGroup := TdxSpreadSheetTableItemGroupAccess(View.Rows.Groups.Find(ARow.Index - 1));
  if AGroup <> nil then
    AGroup.FCollapsedByUser := ANode.Attributes.GetValueAsBoolean(sdxXLSXAttrCollapsed);

  if ANode.Attributes.Find(sdxXLSXAttrRowHeight, AAttr) then
  begin
    ARow.Size := ConvertRowHeight(AAttr.ValueAsFloat);
    ARow.IsCustomSize := ANode.Attributes.GetValueAsBoolean(sdxXLSXAttrCustomHeight);
  end;
  ARow.Visible := not ANode.Attributes.GetValueAsBoolean(sdxXLSXAttrHidden);

  if ANode.Attributes.GetValueAsBoolean(sdxXLSXAttrCustomFormat) then
  begin
    AIndex := ANode.Attributes.GetValueAsInteger(sdxXLSXAttrStyleIndex, 0);
    if CheckListIndex(AIndex, CellStyles, sdxErrorInvalidStyleIndex, ssmtError) then
      ARow.Style.Handle := TdxSpreadSheetCellStyleHandle(CellStyles[AIndex]);
  end;

  ANode.ForEach(ProcessRowCell, ARow);
end;

procedure TdxSpreadSheetXLSXReaderWorksheetParser.ProcessRowCell(ANode: TdxXMLNode; AUserData: Pointer);
var
  AAttr: TdxXMLNodeAttribute;
  ACell: TdxSpreadSheetCell;
  ACellType: TdxSpreadSheetXLSXCellType;
  ACellValue: TdxXMLNode;
  AChildNode: TdxXMLNode;
  AColumnIndex: Integer;
  AIndex: Integer;
  ARow: TdxSpreadSheetTableRow;
  ATempFloat: Double;
begin
  ARow := TdxSpreadSheetTableRow(AUserData);
  if ANode.Attributes.Find(sdxXLSXAttrCellColumn, AAttr) then
    AColumnIndex := ExtractColumnIndex(AAttr.ValueAsString)
  else
    AColumnIndex := TdxSpreadSheetTableRow(AUserData).CellCount;

  ACell := ARow.CreateCell(AColumnIndex);

  if ANode.Attributes.Find(sdxXLSXAttrStyleIndex, AAttr) then
  begin
    AIndex := AAttr.ValueAsInteger;
    if CheckListIndex(AIndex, CellStyles, sdxErrorInvalidStyleIndex, ssmtError) then
      ACell.StyleHandle := TdxSpreadSheetCellStyleHandle(CellStyles[AIndex]);
  end;

  if ANode.FindChild(sdxXLSXNodeCellFunction, AChildNode) then
  begin
    Owner.FormulasRefs.Add(ACell, AChildNode);
    Exit;
  end;

  ACellType := TdxSpreadSheetXLSXHelper.StringToCellType(ANode.Attributes.GetValue(sdxXLSXAttrCellType));
  if ACellType = sxctRichText then
  begin
    if ANode.FindChild(sdxXLSXNodeCellRichText, AChildNode) or
       ANode.FindChild(sdxXLSXNodeCellValue, ACellValue) and ACellValue.FindChild(sdxXLSXNodeCellRichText, AChildNode)
    then
      ACell.AsSharedString := TdxSpreadSheetXLSXReaderSharedStringParser.ParseString(Owner, AChildNode);
  end
  else
    if ANode.FindChild(sdxXLSXNodeCellValue, ACellValue) then
      case ACellType of
        sxctBoolean:
          ACell.AsBoolean := ACellValue.TextAsBoolean;
        sxctError:
          ACell.AsString := ACellValue.TextAsString;
        sxctFloat:
          ACell.AsFloat := StrToFloat(ACellValue.TextAsString, dxInvariantFormatSettings);
        sxctString:
          if TryStrToFloat(ACellValue.TextAsString, ATempFloat, dxInvariantFormatSettings) then
            ACell.AsFloat := ATempFloat
          else
            ACell.AsString := ACellValue.TextAsString;
        sxctSharedString:
          if ANode.FindChild(sdxXLSXNodeCellValue, ACellValue) then
          begin
            AIndex := StrToIntDef(ACellValue.TextAsString, -1);
            if CheckListIndex(AIndex, SharedStrings, sdxErrorInvalidSharedStringIndex, ssmtError) then
              ACell.AsSharedString := TdxSpreadSheetSharedString(SharedStrings[AIndex]);
          end;
      end;
end;

procedure TdxSpreadSheetXLSXReaderWorksheetParser.ProcessSelection(
  ANode: TdxXMLNode; ASelection: TdxSpreadSheetTableViewSelection);
var
  AAttr: TdxXMLNodeAttribute;
  AFocus: TPoint;
  AParts: TStringList;
  ARect: TRect;
  I: Integer;
begin
  ASelection.Clear;

  if ANode.Attributes.Find(sdxXLSXAttrActiveCell, AAttr) then
    AFocus := dxStringToReferenceArea(AAttr.ValueAsString).TopLeft
  else
    AFocus := cxInvalidPoint;

  AParts := TStringList.Create;
  try
    AParts.Text := StringReplace(ANode.Attributes.GetValueAsString(sdxXLSXAttrSqRef), ' ', #13#10, [rfReplaceAll]);
    for I := 0 to AParts.Count - 1 do
    begin
      ARect := dxStringToReferenceArea(AParts[I]);
      if (ARect.Left = 0) and (ARect.Right = dxXLSXMaxColumnIndex) then
        ARect.Right := MaxInt;
      if (ARect.Top = 0) and (ARect.Bottom = dxXLSXMaxRowIndex) then
        ARect.Bottom := MaxInt;
      if not ASelection.HasArea(ARect) then
        ASelection.Add(ARect, [ssCtrl], AFocus.Y, AFocus.X);
    end;
  finally
    AParts.Free;
  end;
end;

procedure TdxSpreadSheetXLSXReaderWorksheetParser.ReadCells;
var
  ANode: TdxXMLNode;
begin
  Include(TdxSpreadSheetAccess(SpreadSheet).FState, sssReadingCells);
  try
    if GetChild(sdxXLSXNodeColumns, ANode) then
      ANode.ForEach(ProcessColumn);
    if GetChild(sdxXLSXNodeSheetData, ANode) then
      ANode.ForEach(ProcessRow);
  finally
    Exclude(TdxSpreadSheetAccess(SpreadSheet).FState, sssReadingCells);
  end;

  if GetChild(sdxXLSXNodeMergeCells, ANode) then
    ANode.ForEach(ProcessMergedCells);
  if GetChild(sdxXLSXNodeSheetFormatPr, ANode) then
    ProcessProperties(ANode);
  if GetChild(sdxXLSXNodeHyperlinks, ANode) then
    ANode.ForEach(ProcessHyperlinks);

  if GetChild(sdxXLSXNodeSheetPr, ANode) and ANode.FindChild(sdxXLSXNodeOutlinePr, ANode) then
  begin
    if not ANode.Attributes.GetValueAsBoolean(sdxXLSXAttrSummaryRight, True) then
      View.Columns.Groups.ExpandButtonPosition := gebpGroupStart;
    if not ANode.Attributes.GetValueAsBoolean(sdxXLSXAttrSummaryBelow, True) then
      View.Rows.Groups.ExpandButtonPosition := gebpGroupStart;
  end;

  if GetChild(sdxXLSXNodeLegacyDrawing, ANode) then
    ProcessLegacyDrawings(ANode);
end;

procedure TdxSpreadSheetXLSXReaderWorksheetParser.ReadConditionalFormatting;
begin
  if Document.Root.First <> nil then
    ExecuteSubTask(TdxSpreadSheetXLSXReaderConditionalFormattingParser.Create(Self));
end;

procedure TdxSpreadSheetXLSXReaderWorksheetParser.ReadDrawing(const AFileName: AnsiString);
begin
  ExecuteSubTask(TdxSpreadSheetXLSXReaderDrawingParser.Create(AFileName, Owner, View));
end;

procedure TdxSpreadSheetXLSXReaderWorksheetParser.ReadDrawings;
var
  I: Integer;
begin
  for I := 0 to Rels.Count - 1 do
  begin
    if Rels.ItemsByIndex[I].FileType = sdxXLSXDrawingRelationship then
      ReadDrawing(Rels.ItemsByIndex[I].FileName);
  end;
end;

procedure TdxSpreadSheetXLSXReaderWorksheetParser.ReadPrinting;
var
  ANode: TdxXMLNode;
begin
  if GetChild(sdxXLSXNodeHeaderFooter, ANode) then
    ProcessHeaderFooter(ANode);
  if GetChild(sdxXLSXNodePageMargins, ANode) then
    ProcessPageMargins(ANode);
  if GetChild(sdxXLSXNodePageSetup, ANode) then
    ProcessPageSetup(ANode);
  if GetChild(sdxXLSXNodeRowBreaks, ANode) then
    ANode.ForEach(ProcessBreaks, View.OptionsPrint.Pagination.RowPageBreaks);
  if GetChild(sdxXLSXNodeColBreaks, ANode) then
    ANode.ForEach(ProcessBreaks, View.OptionsPrint.Pagination.ColumnPageBreaks);
  if GetChild(sdxXLSXNodeSheetPr, ANode) and ANode.FindChild(sdxXLSXNodePageSetUpPr, ANode) then
    ProcessPageSetupExProperties(ANode);
  if GetChild(sdxXLSXNodePrintOptions, ANode) then
    ProcessPrintOptions(ANode);
end;

procedure TdxSpreadSheetXLSXReaderWorksheetParser.ReadProtection;
var
  ANode: TdxXMLNode;
begin
  if GetChild(sdxXLSXNodeSheetProtection, ANode) then
  begin
    View.OptionsProtection.AllowDeleteColumns := not ANode.Attributes.GetValueAsBoolean(sdxXLSXAttrDeleteColumns, True);
    View.OptionsProtection.AllowDeleteRows := not ANode.Attributes.GetValueAsBoolean(sdxXLSXAttrDeleteRows, True);
    View.OptionsProtection.AllowFormatCells := not ANode.Attributes.GetValueAsBoolean(sdxXLSXAttrFormatCells, True);
    View.OptionsProtection.AllowResizeColumns := not ANode.Attributes.GetValueAsBoolean(sdxXLSXAttrFormatColumns, True);
    View.OptionsProtection.AllowResizeRows := not ANode.Attributes.GetValueAsBoolean(sdxXLSXAttrFormatRows, True);
    View.OptionsProtection.AllowInsertColumns := not ANode.Attributes.GetValueAsBoolean(sdxXLSXAttrInsertColumns, True);
    View.OptionsProtection.AllowInsertRows := not ANode.Attributes.GetValueAsBoolean(sdxXLSXAttrInsertRows, True);
    View.OptionsProtection.AllowEditHyperlinks := not ANode.Attributes.GetValueAsBoolean(sdxXLSXAttrInsertHyperlinks, True);
    View.OptionsProtection.AllowEditContainers := not ANode.Attributes.GetValueAsBoolean(sdxXLSXAttrObjects);
    View.OptionsProtection.AllowSelectLockedCells := not ANode.Attributes.GetValueAsBoolean(sdxXLSXAttrSelectLockedCells);
    View.OptionsProtection.AllowSelectUnlockedCells := not ANode.Attributes.GetValueAsBoolean(sdxXLSXAttrSelectUnlockedCell);
    View.OptionsProtection.AllowSort := not ANode.Attributes.GetValueAsBoolean(sdxXLSXAttrSort);
    View.OptionsProtection.Protected := ANode.Attributes.GetValueAsBoolean(sdxXLSXAttrSheet);

    if ANode.Attributes.Exists(sdxXLSXAttrAlgorithmName) then
      View.OptionsProtection.ProtectionInfo := ReadProtectionInfoStrong(ANode)
    else if ANode.Attributes.Exists(sdxXLSXAttrPassword) then
      View.OptionsProtection.ProtectionInfo := ReadProtectionInfoStandard(ANode)
  end;
end;

function TdxSpreadSheetXLSXReaderWorksheetParser.ReadProtectionInfoStandard(ANode: TdxXMLNode): IdxSpreadSheetProtectionInfo;
var
  AProtection: TdxSpreadSheetStandardProtectionInfo;
begin
  AProtection := TdxSpreadSheetStandardProtectionInfo.Create;
  AProtection.KeyWordAsString := ANode.Attributes.GetValueAsString(sdxXLSXAttrPassword);
  Result := AProtection;
end;

function TdxSpreadSheetXLSXReaderWorksheetParser.ReadProtectionInfoStrong(ANode: TdxXMLNode): IdxSpreadSheetProtectionInfo;
var
  AProtection: TdxSpreadSheetStrongProtectionInfo;
begin
  AProtection := TdxSpreadSheetStrongProtectionInfo.Create;
  AProtection.HashAlgorithm := TdxSpreadSheetXLSXHelper.StringToHashAlgorithm(ANode.Attributes.GetValueAsString(sdxXLSXAttrAlgorithmName));
  AProtection.HashValueAsString := ANode.Attributes.GetValueAsString(sdxXLSXAttrHashValue);
  AProtection.SaltValueAsString := ANode.Attributes.GetValueAsString(sdxXLSXAttrSaltValue);
  AProtection.SpinCount := ANode.Attributes.GetValueAsInteger(sdxXLSXAttrSpinCount);
  Result := AProtection;
end;

procedure TdxSpreadSheetXLSXReaderWorksheetParser.ReadViewProperties;
var
  AActivePane: string;
  AAttr: TdxXMLNodeAttribute;
  AChildNode: TdxXMLNode;
  ANode: TdxXMLNode;
begin
  if GetChild(sdxXLSXNodeSheetsView, ANode) and ANode.FindChild(sdxXLSXNodeSheetView, ANode) then
  begin
    View.Options.ZoomFactor := ANode.Attributes.GetValueAsInteger(sdxXLSXAttrZoomScaleNormal, 100);
    if ANode.Attributes.GetValueAsBoolean(sdxXLSXAttrTabSelected) then
      View.Active := True;
    if ANode.Attributes.Find(sdxXLSXAttrZeroValues, AAttr) then
      View.Options.ZeroValues := TdxDefaultBoolean(AAttr.ValueAsBoolean);
    if ANode.Attributes.Find(sdxXLSXAttrShowFormulas, AAttr) then
      View.Options.ShowFormulas := TdxDefaultBoolean(AAttr.ValueAsBoolean);
    if ANode.Attributes.Find(sdxXLSXAttrGridLines, AAttr) then
      View.Options.GridLines := TdxDefaultBoolean(AAttr.ValueAsBoolean);
    if ANode.Attributes.Find(sdxXLSXAttrShowRowColHeaders, AAttr) then
      View.Options.Headers := TdxDefaultBoolean(AAttr.ValueAsBoolean);

    AActivePane := '';
    if ANode.FindChild(sdxXLSXNodePane, AChildNode) then
    begin
      AActivePane := AChildNode.Attributes.GetValueAsString(sdxXLSXAttrActivePane);
      ProcessFixedPane(AChildNode);
    end;

    ANode.ForEach(
      procedure (ANode: TdxXMLNode; AUserData: Pointer)
      begin
        if SameText(ANode.Name, sdxXLSXNodeSelection) then
        begin
          if SameText(ANode.Attributes.GetValueAsString(sdxXLSXAttrPane), AActivePane) then
            ProcessSelection(ANode, View.Selection);
        end;
      end);
  end;
end;

function TdxSpreadSheetXLSXReaderWorksheetParser.GetCellStyles: TdxSpreadSheetXLSXHashTableItemList;
begin
  Result := Owner.Styles;
end;

function TdxSpreadSheetXLSXReaderWorksheetParser.GetChild(const AName: TdxXMLString; out ANode: TdxXMLNode): Boolean;
begin
  Result := (Document.Root.First <> nil) and Document.Root.First.FindChild(AName, ANode);
end;

function TdxSpreadSheetXLSXReaderWorksheetParser.GetSharedStrings: TdxSpreadSheetXLSXHashTableItemList;
begin
  Result := Owner.SharedStrings;
end;

{ TdxSpreadSheetOpenXMLDocument }

function TdxSpreadSheetOpenXMLDocument.CreateRootNode: TdxXMLNode;
begin
  Result := TdxSpreadSheetOpenXMLNode.Create;
end;

{ TdxSpreadSheetOpenXMLNode }

function TdxSpreadSheetOpenXMLNode.FindChild(const AName: TdxXMLString; out ANode: TdxXMLNode): Boolean;
begin
  ANode := First;
  while (ANode <> nil) and not SameText(AName, ANode.Name) do
    ANode := ANode.Next;
  if (ANode = nil) and (Pos(TdxXMLParser.NameScopeDelimiter, AName) = 0) then
  begin
    ANode := First;
    while (ANode <> nil) and not SameText(AName, ANode.NameWithoutNameScope) do
      ANode := ANode.Next;
  end;
  Result := ANode <> nil;
end;

function TdxSpreadSheetOpenXMLNode.GetNodeClass: TdxXMLNodeClass;
begin
  Result := TdxSpreadSheetOpenXMLNode;
end;

end.
