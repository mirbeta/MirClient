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

unit dxSpreadSheetFormatODSReader;

{$I cxVer.Inc}
{$R dxSpreadSheetFormatODSReader.res}

interface

uses
  Types, Windows, SysUtils, Classes, Generics.Defaults, Generics.Collections, Graphics, dxCore, cxGraphics, dxXMLDoc, dxCustomTree,
  dxSpreadSheetCore, dxSpreadSheetClasses, dxSpreadSheetPackedFileFormatCore, dxSpreadSheetFormatODSHelpers,
  dxGDIPlusClasses, dxCoreGraphics, dxSpreadSheetPrinting, cxGeometry, dxSpreadSheetConditionalFormattingRules,
  dxSpreadSheetContainers, dxSpreadSheetGraphics, dxSpreadSheetHyperlinks, dxCoreClasses, dxSpreadSheetCoreStyles,
  dxSpreadSheetConditionalFormatting;

const
  dxSpreadSheetODSAutoSizeValue = -1;

type
  TdxSpreadSheetODSReader = class;
  TdxSpreadSheetODSReaderDataFormatConverter = class;
  TdxSpreadSheetODSReaderHatchStyle = class;
  TdxSpreadSheetODSReaderImageStyle = class;
  TdxSpreadSheetODSReaderMasterPageStyle = class;
  TdxSpreadSheetODSReaderPageLayoutStyle = class;
  TdxSpreadSheetODSReaderStrokeDashStyle = class;
  TdxSpreadSheetODSReaderTableRowParser = class;
  TdxSpreadSheetODSReaderTableViewParser = class;

  { TdxSpreadSheetODSReaderCustomStyle }

  TdxSpreadSheetODSReaderCustomStyleClass = class of TdxSpreadSheetODSReaderCustomStyle;
  TdxSpreadSheetODSReaderCustomStyle = class
  strict private
    FOwner: TdxSpreadSheetODSReader;
  public
    constructor Create(ANode: TdxXMLNode; AOwner: TdxSpreadSheetODSReader); virtual;
    class function GetStyleName(ANode: TdxXMLNode): TdxXMLString; virtual;
    //
    property Owner: TdxSpreadSheetODSReader read FOwner;
  end;

  { TdxSpreadSheetODSReaderStyleRepository }

  TdxSpreadSheetODSReaderStyleRepository<T: TdxSpreadSheetODSReaderCustomStyle> = class(TObjectDictionary<TdxXMLString, T>)
  strict private
    FOwner: TdxSpreadSheetODSReader;
  public
    constructor Create(AOwner: TdxSpreadSheetODSReader);
    procedure Add(ANode: TdxXMLNode);
    function GetValueOrNil(const S: TdxXMLString): T;
    function TryGetValue(const S: TdxXMLString; out AValue: T): Boolean;
  end;

  { TdxSpreadSheetODSReaderTableStyle }

  TdxSpreadSheetODSReaderTableStyle = class(TdxSpreadSheetODSReaderCustomStyle)
  public
    PageStyle: TdxSpreadSheetODSReaderMasterPageStyle;
    Visible: Boolean;

    constructor Create(ANode: TdxXMLNode; AOwner: TdxSpreadSheetODSReader); override;
    procedure ApplyTo(ATableView: TdxSpreadSheetTableView);
  end;

  { TdxSpreadSheetODSReaderTableItemStyle }

  TdxSpreadSheetODSReaderTableItemStyle = class(TdxSpreadSheetODSReaderCustomStyle)
  public
    AutoSize: Boolean;
    Size: Integer;

    procedure ApplyTo(AItem: TdxSpreadSheetTableItem);
    function IsDefault: Boolean;
  end;

  { TdxSpreadSheetODSReaderColumnStyle }

  TdxSpreadSheetODSReaderColumnStyle = class(TdxSpreadSheetODSReaderTableItemStyle)
  public
    constructor Create(ANode: TdxXMLNode; AOwner: TdxSpreadSheetODSReader); override;
  end;

  { TdxSpreadSheetODSReaderGradientStyle }

  TdxSpreadSheetODSReaderGradientStyle = class(TdxSpreadSheetODSReaderCustomStyle)
  strict private
    function DecodeColor(ANode: TdxXMLNode; const AColorValueName, AColorIntensityName: TdxXMLString): TColor;
  public
    Color1: TColor;
    Color2: TColor;
    Mode: TdxGPBrushGradientMode;

    constructor Create(ANode: TdxXMLNode; AOwner: TdxSpreadSheetODSReader); override;
    class function GetStyleName(ANode: TdxXMLNode): TdxXMLString; override;
  end;

  { TdxSpreadSheetODSReaderParagraphStyle }

  TdxSpreadSheetODSReaderParagraphStyle = class(TdxSpreadSheetODSReaderCustomStyle)
  public
    Alignment: TdxSpreadSheetDataAlignHorz;

    constructor Create(ANode: TdxXMLNode; AOwner: TdxSpreadSheetODSReader); override;
  end;

  { TdxSpreadSheetODSReaderGraphicStyle }

  TdxSpreadSheetODSReaderGraphicStyle = class(TdxSpreadSheetODSReaderCustomStyle)
  strict private
    procedure LoadFill(ANode: TdxXMLNode);
    procedure LoadStroke(ANode: TdxXMLNode);
  public
    AutoSize: Boolean;
    FillColor: TColor;
    FillGradient: TdxSpreadSheetODSReaderGradientStyle;
    FillHatch: TdxSpreadSheetODSReaderHatchStyle;
    FillMode: TdxSpreadSheetODSFillMode;
    FillTexture: TdxSpreadSheetODSReaderImageStyle;
    Padding: TRect;
    ProtectPosition: Boolean;
    ProtectSize: Boolean;
    StrokeColor: TdxAlphaColor;
    StrokeStyle: TdxSpreadSheetODSReaderStrokeDashStyle;
    StrokeWidth: Single;
    TextAreaHorzAlign: TAlignment;
    TextAreaVertAlign: TVerticalAlignment;
    WordWrap: Boolean;

    constructor Create(ANode: TdxXMLNode; AOwner: TdxSpreadSheetODSReader); override;
    procedure ApplyToBrush(ABrush: TdxGPBrush);
    procedure ApplyToPen(APen: TdxGPPen);
  end;

  { TdxSpreadSheetODSReaderHatchStyle }

  TdxSpreadSheetODSReaderHatchStyle = class(TdxSpreadSheetODSReaderCustomStyle)
  strict private
    function DetectBrushPattern(ANode: TdxXMLNode): TdxSpreadSheetODSBrushPattern;
  public
    Color: TColor;
    Pattern: TdxSpreadSheetODSBrushPattern;

    constructor Create(ANode: TdxXMLNode; AOwner: TdxSpreadSheetODSReader); override;
    class function GetStyleName(ANode: TdxXMLNode): TdxXMLString; override;
  end;

  { TdxSpreadSheetODSReaderImageStyle }

  TdxSpreadSheetODSReaderImageStyle = class(TdxSpreadSheetODSReaderCustomStyle)
  public
    Data: TMemoryStream;

    constructor Create(ANode: TdxXMLNode; AOwner: TdxSpreadSheetODSReader); override;
    destructor Destroy; override;
    class function GetStyleName(ANode: TdxXMLNode): TdxXMLString; override;
  end;

  { TdxSpreadSheetODSReaderMasterPageStyle }

  TdxSpreadSheetODSReaderMasterPageStyle = class(TdxSpreadSheetODSReaderCustomStyle)
  strict private
    procedure ParseText(AText: TdxSpreadSheetTableViewOptionsPrintHeaderFooterText; ANode: TdxXMLNode);
  public
    DisplayName: string;
    HeaderFooter: TdxSpreadSheetTableViewOptionsPrintHeaderFooter;
    PageLayout: TdxSpreadSheetODSReaderPageLayoutStyle;

    constructor Create(ANode: TdxXMLNode; AOwner: TdxSpreadSheetODSReader); override;
    destructor Destroy; override;
    class function GetStyleName(ANode: TdxXMLNode): TdxXMLString; override;
    procedure ApplyTo(AOptions: TdxSpreadSheetTableViewOptionsPrint);
  end;

  { TdxSpreadSheetODSReaderPageLayoutStyle }

  TdxSpreadSheetODSReaderPageLayoutStyle = class(TdxSpreadSheetODSReaderCustomStyle)
  strict private
    procedure ReadMargins(ANode: TdxXMLNode);
    procedure ReadPageLayoutProperties(ANode: TdxXMLNode);
    procedure ReadPrintElements(ANode: TdxXMLNode);
  public
    FirstPageNumber: Cardinal;
    GridLines: TdxDefaultBoolean;
    Headers: TdxDefaultBoolean;
    Margins: TdxSpreadSheetTableViewOptionsPrintPageMargins;
    PageCenterHorz: TdxDefaultBoolean;
    PageCenterVert: TdxDefaultBoolean;
    PageOrder: TdxSpreadSheetTableViewOptionsPrintPrintingPageOrder;
    PageOrientation: TdxSpreadSheetTableViewOptionsPrintPageOrientation;
    PageSize: TdxPointDouble;
    Scale: Integer;

    constructor Create(ANode: TdxXMLNode; AOwner: TdxSpreadSheetODSReader); override;
    destructor Destroy; override;
    procedure ApplyTo(AOptions: TdxSpreadSheetTableViewOptionsPrint);
    class function GetStyleName(ANode: TdxXMLNode): TdxXMLString; override;
  end;

  { TdxSpreadSheetODSReaderRowStyle }

  TdxSpreadSheetODSReaderRowStyle = class(TdxSpreadSheetODSReaderTableItemStyle)
  public
    constructor Create(ANode: TdxXMLNode; AOwner: TdxSpreadSheetODSReader); override;
  end;

  { TdxSpreadSheetODSReaderStrokeDashStyle }

  TdxSpreadSheetODSReaderStrokeDashStyle = class(TdxSpreadSheetODSReaderCustomStyle)
  strict private
    function DetectPenStyle(ANode: TdxXMLNode): TdxGPPenStyle;
  public
    Style: TdxGPPenStyle;
    constructor Create(ANode: TdxXMLNode; AOwner: TdxSpreadSheetODSReader); override;
    class function GetStyleName(ANode: TdxXMLNode): TdxXMLString; override;
  end;

  { TdxSpreadSheetODSStyles }

  TdxSpreadSheetODSStyles = class(TDictionary<string, TdxSpreadSheetCellStyleHandle>)
  strict private
    FDisplayNameMap: TDictionary<string, string>;
  protected
    procedure ValueNotify(const Value: TdxSpreadSheetCellStyleHandle; Action: TCollectionNotification); override;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Add(const AName, ADisplayName: string; AHandle: TdxSpreadSheetCellStyleHandle);
    function TryGetValue(const AName: string; out AHandle: TdxSpreadSheetCellStyleHandle): Boolean;
  end;

  { TdxSpreadSheetODSFormulaAsTextInfoList }

  TdxSpreadSheetODSFormulaAsTextInfoList = class(TdxSpreadSheetFormulaAsTextInfoList)
  protected
    function CreateParser: TObject; override;
  end;

  { TdxSpreadSheetODSReader }

  TdxSpreadSheetODSReader = class(TdxSpreadSheetCustomPackedReader)
  strict private
    FColumnStyles: TdxSpreadSheetODSReaderStyleRepository<TdxSpreadSheetODSReaderColumnStyle>;
    FDataFormatConverter: TdxSpreadSheetODSReaderDataFormatConverter;
    FDataFormats: TDictionary<TdxXMLString, string>;
    FFormulas: TdxSpreadSheetODSFormulaAsTextInfoList;
    FGradientStyles: TdxSpreadSheetODSReaderStyleRepository<TdxSpreadSheetODSReaderGradientStyle>;
    FGraphicStyles: TdxSpreadSheetODSReaderStyleRepository<TdxSpreadSheetODSReaderGraphicStyle>;
    FHatchStyles: TdxSpreadSheetODSReaderStyleRepository<TdxSpreadSheetODSReaderHatchStyle>;
    FHyperlinks: TStringList;
    FImageStyles: TdxSpreadSheetODSReaderStyleRepository<TdxSpreadSheetODSReaderImageStyle>;
    FMasterPageStyles: TdxSpreadSheetODSReaderStyleRepository<TdxSpreadSheetODSReaderMasterPageStyle>;
    FPageLayouts: TdxSpreadSheetODSReaderStyleRepository<TdxSpreadSheetODSReaderPageLayoutStyle>;
    FParagraphStyles: TdxSpreadSheetODSReaderStyleRepository<TdxSpreadSheetODSReaderParagraphStyle>;
    FRowStyles: TdxSpreadSheetODSReaderStyleRepository<TdxSpreadSheetODSReaderRowStyle>;
    FStrokeDashStyles: TdxSpreadSheetODSReaderStyleRepository<TdxSpreadSheetODSReaderStrokeDashStyle>;
    FStyles: TdxSpreadSheetODSStyles;
    FTableStyles: TdxSpreadSheetODSReaderStyleRepository<TdxSpreadSheetODSReaderTableStyle>;

    function ReadMimeType: AnsiString;
  protected
    FVersion: Double;

    function CreateProgressHelper: TdxSpreadSheetCustomFilerProgressHelper; override;
    procedure ResolveHyperlinks;
  public
    constructor Create(ASpreadSheet: TdxCustomSpreadSheet; AStream: TStream); override;
    destructor Destroy; override;
    function CheckMimeType: Boolean;
    procedure ReadData; override;
    //
    property ColumnStyles: TdxSpreadSheetODSReaderStyleRepository<TdxSpreadSheetODSReaderColumnStyle> read FColumnStyles;
    property DataFormatConverter: TdxSpreadSheetODSReaderDataFormatConverter read FDataFormatConverter;
    property DataFormats: TDictionary<TdxXMLString, string> read FDataFormats;
    property Formulas: TdxSpreadSheetODSFormulaAsTextInfoList read FFormulas;
    property GradientStyles: TdxSpreadSheetODSReaderStyleRepository<TdxSpreadSheetODSReaderGradientStyle> read FGradientStyles;
    property GraphicStyles: TdxSpreadSheetODSReaderStyleRepository<TdxSpreadSheetODSReaderGraphicStyle> read FGraphicStyles;
    property HatchStyles: TdxSpreadSheetODSReaderStyleRepository<TdxSpreadSheetODSReaderHatchStyle> read FHatchStyles;
    property Hyperlinks: TStringList read FHyperlinks;
    property ImageStyles: TdxSpreadSheetODSReaderStyleRepository<TdxSpreadSheetODSReaderImageStyle> read FImageStyles;
    property MasterPageStyles: TdxSpreadSheetODSReaderStyleRepository<TdxSpreadSheetODSReaderMasterPageStyle> read FMasterPageStyles;
    property PageLayouts: TdxSpreadSheetODSReaderStyleRepository<TdxSpreadSheetODSReaderPageLayoutStyle> read FPageLayouts;
    property ParagraphStyles: TdxSpreadSheetODSReaderStyleRepository<TdxSpreadSheetODSReaderParagraphStyle> read FParagraphStyles;
    property RowStyles: TdxSpreadSheetODSReaderStyleRepository<TdxSpreadSheetODSReaderRowStyle> read FRowStyles;
    property StrokeDashStyles: TdxSpreadSheetODSReaderStyleRepository<TdxSpreadSheetODSReaderStrokeDashStyle> read FStrokeDashStyles;
    property Styles: TdxSpreadSheetODSStyles read FStyles;
    property TableStyles: TdxSpreadSheetODSReaderStyleRepository<TdxSpreadSheetODSReaderTableStyle> read FTableStyles;
    property Version: Double read FVersion;
  end;

  { TdxSpreadSheetODSReaderCustomParser }

  TdxSpreadSheetODSReaderCustomParser = class(TdxSpreadSheetCustomPackedReaderParser)
  strict private
    function GetOwner: TdxSpreadSheetODSReader; inline;
  public
    property Owner: TdxSpreadSheetODSReader read GetOwner;
  end;

  { TdxSpreadSheetODSReaderCustomNodeParser }

  TdxSpreadSheetODSReaderCustomNodeParser = class(TdxSpreadSheetODSReaderCustomParser)
  protected
    FNode: TdxXMLNode;
  public
    constructor Create(ANode: TdxXMLNode; AOwner: TdxSpreadSheetODSReader);
    //
    property Node: TdxXMLNode read FNode;
  end;

  { TdxSpreadSheetODSReaderCustomDocumentParser }

  TdxSpreadSheetODSReaderCustomDocumentParser = class(TdxSpreadSheetODSReaderCustomNodeParser)
  strict private
    FDocument: TdxXMLDocument;
    FDocumentFileName: AnsiString;
  public
    constructor Create(const AFileName: AnsiString; AOwner: TdxSpreadSheetODSReader);
    destructor Destroy; override;
    //
    property Document: TdxXMLDocument read FDocument;
    property DocumentFileName: AnsiString read FDocumentFileName;
  end;

  { TdxSpreadSheetODSReaderContentFileParser }

  TdxSpreadSheetODSReaderContentFileParser = class(TdxSpreadSheetODSReaderCustomDocumentParser)
  public
    procedure Execute; override;
  end;

  { TdxSpreadSheetODSReaderContentSpreadSheetParser }

  TdxSpreadSheetODSReaderContentSpreadSheetParser = class(TdxSpreadSheetODSReaderCustomNodeParser)
  protected
    procedure ProcessCalculationSettings(ANode: TdxXMLNode);
    procedure ProcessSubNode(ANode: TdxXMLNode; AUserData: Pointer);
  public
    procedure Execute; override;
  end;

  { TdxSpreadSheetODSReaderCustomContainerParser }

  TdxSpreadSheetODSReaderCustomContainerParserClass = class of TdxSpreadSheetODSReaderCustomContainerParser;
  TdxSpreadSheetODSReaderCustomContainerParser = class(TdxSpreadSheetODSReaderCustomNodeParser)
  strict private
    FBaseCell: TdxSpreadSheetCell;
    FContainer: TdxSpreadSheetContainer;
    FRowParser: TdxSpreadSheetODSReaderTableRowParser;
    FViewParser: TdxSpreadSheetODSReaderTableViewParser;

    function GetCellByRef(const ARef: string): TdxSpreadSheetCell;
  protected
    function CreateContainer: TdxSpreadSheetContainer; virtual;
    procedure ReadAnchorPoints; virtual;
  public
    constructor Create(ANode: TdxXMLNode; AViewParser: TdxSpreadSheetODSReaderTableViewParser;
      ARowParser: TdxSpreadSheetODSReaderTableRowParser = nil; ABaseCell: TdxSpreadSheetCell = nil);
    destructor Destroy; override;
    procedure Execute; override;
    //
    property BaseCell: TdxSpreadSheetCell read FBaseCell;
    property Container: TdxSpreadSheetContainer read FContainer;
    property RowParser: TdxSpreadSheetODSReaderTableRowParser read FRowParser;
    property ViewParser: TdxSpreadSheetODSReaderTableViewParser read FViewParser;
  end;

  { TdxSpreadSheetODSReaderShapeContainerParser }

  TdxSpreadSheetODSReaderShapeContainerParser = class(TdxSpreadSheetODSReaderCustomContainerParser)
  strict private
    function GetContainer: TdxSpreadSheetShapeContainer; inline;
  protected
    function CreateContainer: TdxSpreadSheetContainer; override;
  public
    procedure Execute; override;
    //
    property Container: TdxSpreadSheetShapeContainer read GetContainer;
  end;

  { TdxSpreadSheetODSReaderCustomTextBoxContainerParser }

  TdxSpreadSheetODSReaderCustomTextBoxContainerParser = class(TdxSpreadSheetODSReaderShapeContainerParser)
  strict private
    function GetTextBox: TdxSpreadSheetCustomTextBox;
  protected
    procedure ApplyStyleToTextBox(AStyle: TdxSpreadSheetODSReaderGraphicStyle); virtual;
  public
    procedure Execute; override;
    //
    property TextBox: TdxSpreadSheetCustomTextBox read GetTextBox;
  end;

  { TdxSpreadSheetODSReaderTextBoxContainerParser }

  TdxSpreadSheetODSReaderTextBoxContainerParser = class(TdxSpreadSheetODSReaderCustomTextBoxContainerParser)
  protected
    procedure ApplyStyleToTextBox(AStyle: TdxSpreadSheetODSReaderGraphicStyle); override;
    function CreateContainer: TdxSpreadSheetContainer; override;
  end;

  { TdxSpreadSheetODSReaderCommentContainerParser }

  TdxSpreadSheetODSReaderCommentContainerParser = class(TdxSpreadSheetODSReaderCustomTextBoxContainerParser)
  protected
    function CreateContainer: TdxSpreadSheetContainer; override;
    procedure ReadAnchorPoints; override;
  public
    procedure Execute; override;
  end;

  { TdxSpreadSheetODSReaderPictureContainerParser }

  TdxSpreadSheetODSReaderPictureContainerParser = class(TdxSpreadSheetODSReaderShapeContainerParser)
  strict private
    function GetContainer: TdxSpreadSheetPictureContainer; inline;
  protected
    function CreateContainer: TdxSpreadSheetContainer; override;
  public
    procedure Execute; override;
    //
    property Container: TdxSpreadSheetPictureContainer read GetContainer;
  end;

  { TdxSpreadSheetODSReaderDataFormatConverter }

  TdxSpreadSheetODSReaderDataFormatConverterProc = function (ANode: TdxXMLNode): string;

  TdxSpreadSheetODSReaderDataFormatConverter = class(TDictionary<TdxXMLString, TdxSpreadSheetODSReaderDataFormatConverterProc>)
  strict private
    FProcs: TDictionary<TdxXMLString, TdxSpreadSheetODSReaderDataFormatConverterProc>;

    class function IsLongStyle(ANode: TdxXMLNode): Boolean; static; inline;
  protected
    class function ConvertDay(ANode: TdxXMLNode): string; static;
    class function ConvertDayOfWeek(ANode: TdxXMLNode): string; static;
    class function ConvertHours(ANode: TdxXMLNode): string; static;
    class function ConvertMinutes(ANode: TdxXMLNode): string; static;
    class function ConvertMonth(ANode: TdxXMLNode): string; static;
    class function ConvertNumber(ANode: TdxXMLNode): string; static;
    class function ConvertSeconds(ANode: TdxXMLNode): string; static;
    class function ConvertText(ANode: TdxXMLNode): string; static;
    class function ConvertTextColor(ANode: TdxXMLNode): string; static;
    class function ConvertYear(ANode: TdxXMLNode): string; static;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function Convert(ANode: TdxXMLNode): string;
  end;

  { TdxSpreadSheetODSReaderDefinedNamesParser }

  TdxSpreadSheetODSReaderDefinedNamesParser = class(TdxSpreadSheetODSReaderCustomNodeParser)
  strict private
    FView: TdxSpreadSheetTableView;
  protected
    procedure ParseNamedExpression(ANode: TdxXMLNode);
    procedure ParseNamedRange(ANode: TdxXMLNode; const AValueName: TdxXMLString);
    procedure ProcessSubNodes(ANode: TdxXMLNode; AUserData: Pointer);
  public
    constructor Create(ANode: TdxXMLNode; AOwner: TdxSpreadSheetODSReader; AView: TdxSpreadSheetTableView = nil);
    procedure Execute; override;
  end;

  { TdxSpreadSheetODSReaderMasterStylesParser }

  TdxSpreadSheetODSReaderMasterStylesParser = class(TdxSpreadSheetODSReaderCustomNodeParser)
  protected
    procedure ProcessSubNodes(ANode: TdxXMLNode; AUserData: Pointer);
  public
    procedure Execute; override;
  end;

  { TdxSpreadSheetODSReaderNumberFormatParser }

  TdxSpreadSheetODSReaderNumberFormatParser = class(TdxSpreadSheetODSReaderCustomNodeParser)
  strict private
    procedure CheckStyleMapping(var AFormatCode: string);
    function PopulateSubStyles: TDictionary<TdxXMLString, string>;
  public
    procedure Execute; override;
  end;

  { TdxSpreadSheetODSReaderRichTextHelper }

  TdxSpreadSheetODSReaderRichTextHelper = class
  strict private
    procedure AddPart(const AText: string; AFont: TdxSpreadSheetFontHandle = nil);
    procedure ProcessSubNode(ANode: TdxXMLNode; AUserData: Pointer);

    function GetDefaultFontHandle: TdxSpreadSheetFontHandle;
    function GetFontHandle(ANode: TdxXMLNode): TdxSpreadSheetFontHandle;
  public
    HyperlinkValue: string;
    Owner: TdxSpreadSheetODSReaderTableRowParser;
    Runs: TdxSpreadSheetFormattedSharedStringRuns;
    Text: string;

    constructor Create(AOwner: TdxSpreadSheetODSReaderTableRowParser);
    destructor Destroy; override;
    procedure Process(ANode: TdxXMLNode; AFont: TdxSpreadSheetFontHandle = nil);
  end;

  { TdxSpreadSheetODSReaderSettingsFileParser }

  TdxSpreadSheetODSReaderSettingsParserProc = procedure (const AValue: string) of object;

  TdxSpreadSheetODSReaderSettingsFileParser = class(TdxSpreadSheetODSReaderCustomDocumentParser)
  strict private
    FProcs: TDictionary<TdxXMLString, TdxSpreadSheetODSReaderSettingsParserProc>;

    procedure BothProcessShowGrid(const AValue: string);
    procedure BothProcessZoomValue(const AValue: string);
    procedure CommonProcessActiveTable(const AValue: string);
    procedure CommonProcessHasColumnRowsHeaders(const AValue: string);
    procedure CommonProcessShowZeroValues(const AValue: string);
    procedure ViewProcessCursorX(const AValue: string);
    procedure ViewProcessCursorY(const AValue: string);
    procedure ViewProcessPositionBottom(const AValue: string);
    procedure ViewProcessPositionLeft(const AValue: string);
    procedure ViewProcessPositionRight(const AValue: string);
    procedure ViewProcessHorzSplitPosition(const AValue: string);
    procedure ViewProcessVertSplitPosition(const AValue: string);
  protected
    FCurrentView: TdxSpreadSheetTableView;

    procedure ProcessConfigItem(ANode: TdxXMLNode; AUserData: Pointer);
    procedure ProcessConfigSet(ANode: TdxXMLNode; AUserData: Pointer);
    procedure ProcessConfigSetTables(ANode: TdxXMLNode; AUserData: Pointer);
    procedure ProcessConfigSetViews(ANode: TdxXMLNode; AUserData: Pointer);
    procedure ProcessConfigSetViewSettings(ANode: TdxXMLNode; AUserData: Pointer);
  public
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure Execute; override;
  end;

  { TdxSpreadSheetODSReaderStyleParser }

  TdxSpreadSheetODSReaderStyleParser = class(TdxSpreadSheetODSReaderCustomNodeParser)
  protected
    procedure ParseBackgroundProperties(AHandle: TdxSpreadSheetCellStyleHandle; ANode: TdxXMLNode);
    procedure ParseBorder(AHandle: TdxSpreadSheetBordersHandle; ASide: TcxBorder; const S: string);
    procedure ParseBorders(AHandle: TdxSpreadSheetCellStyleHandle; ANode: TdxXMLNode);
    procedure ParseBrush(AHandle: TdxSpreadSheetCellStyleHandle; ANode: TdxXMLNode);
    procedure ParseFontProperties(AHandle: TdxSpreadSheetCellStyleHandle; ANode: TdxXMLNode);
    procedure ParseParagraphProperties(AHandle: TdxSpreadSheetCellStyleHandle; ANode: TdxXMLNode);
    function ParseStyle(out AStyleHandle: TdxSpreadSheetCellStyleHandle): Boolean;
  public
    procedure Execute; override;
  end;

  { TdxSpreadSheetODSReaderStylesParser }

  TdxSpreadSheetODSReaderStylesParser = class(TdxSpreadSheetODSReaderCustomNodeParser)
  protected
    procedure ProcessSubNode(ANode: TdxXMLNode; AUserData: Pointer); virtual;
  public
    procedure Execute; override;
  end;

  { TdxSpreadSheetODSReaderStylesFileParser }

  TdxSpreadSheetODSReaderStylesFileParser = class(TdxSpreadSheetODSReaderCustomDocumentParser)
  public
    procedure Execute; override;
  end;

  { TdxSpreadSheetODSReaderTableViewParser }

  TdxSpreadSheetODSReaderTableViewParser = class(TdxSpreadSheetODSReaderCustomNodeParser)
  strict private
    function GetColumnGroups: TdxSpreadSheetTableItemGroups;
    function GetRowGroups: TdxSpreadSheetTableItemGroups;
  protected
    FColumnIndex: Integer;
    FContainersZOrder: TList<TdxSpreadSheetContainer>;
    FRowIndex: Integer;
    FView: TdxSpreadSheetTableView;

    procedure AssignContainersZOrder;
    procedure AssignPrintRange;
    procedure ProcessColumnGroup(ANode: TdxXMLNode);
    procedure ProcessConditionalFormat(ANode: TdxXMLNode; AUserData: Pointer);
    procedure ProcessRowGroup(ANode: TdxXMLNode);
    procedure ProcessShapes(ANode: TdxXMLNode; AUserData: Pointer);
    procedure ProcessSubNode(ANode: TdxXMLNode; AUserData: Pointer);
  public
    constructor Create(ANode: TdxXMLNode; AOwner: TdxSpreadSheetODSReader);
    destructor Destroy; override;
    procedure Execute; override;
    //
    property ColumnGroups: TdxSpreadSheetTableItemGroups read GetColumnGroups;
    property ContainersZOrder: TList<TdxSpreadSheetContainer> read FContainersZOrder;
    property RowGroups: TdxSpreadSheetTableItemGroups read GetRowGroups;
    property View: TdxSpreadSheetTableView read FView;
  end;

  { TdxSpreadSheetODSReaderTableItemParser }

  TdxSpreadSheetODSReaderTableItemParser = class(TdxSpreadSheetODSReaderCustomNodeParser)
  strict private
    FTableViewParser: TdxSpreadSheetODSReaderTableViewParser;

    function GetTableView: TdxSpreadSheetTableView; inline;
  protected
    function ConvertReference(const S: string): string;
    function GetCellStyle(ANode: TdxXMLNode; const AAttrName: TdxXMLString; ACanBeNil: Boolean): TdxSpreadSheetCellStyleHandle;
  public
    constructor Create(ANode: TdxXMLNode; ATableViewParser: TdxSpreadSheetODSReaderTableViewParser);
    //
    property TableView: TdxSpreadSheetTableView read GetTableView;
    property TableViewParser: TdxSpreadSheetODSReaderTableViewParser read FTableViewParser;
  end;

  { TdxSpreadSheetODSReaderTableColumnParser }

  TdxSpreadSheetODSReaderTableColumnParser = class(TdxSpreadSheetODSReaderTableItemParser)
  public
    procedure Execute; override;
  end;

  { TdxSpreadSheetODSReaderTableRowParser }

  TdxSpreadSheetODSReaderTableRowParser = class(TdxSpreadSheetODSReaderTableItemParser)
  protected
    FCellIndex: Integer;

    procedure ParseCellTextValue(ATextNode: TdxXMLNode; ACell: TdxSpreadSheetCell);
    procedure ParseCellValue(ANode: TdxXMLNode; ACell: TdxSpreadSheetCell);
    procedure PostProcessCells(ARow: TdxSpreadSheetTableRow);
    procedure ProcessCell(ANode: TdxXMLNode; AUserData: Pointer);
    procedure ProcessCellRange(ANode: TdxXMLNode; ARow: TdxSpreadSheetTableRow; ARangeSize: Integer);
    procedure ProcessDrawObject(ANode: TdxXMLNode; AUserData: Pointer);
  public
    procedure Execute; override;
  end;

  { TdxSpreadSheetODSReaderConditionalFormatParser }

  TdxSpreadSheetODSReaderConditionalFormatParser = class(TdxSpreadSheetODSReaderTableItemParser)
  strict private
    function CreateAverageRule(ANode: TdxXMLNode;
      AOperator: TdxSpreadSheetConditionalFormattingRuleAboveOrBelowAverageComparisonOperator): TdxSpreadSheetConditionalFormattingRuleAboveOrBelowAverage;
    function CreateCellIsRule(AComparisonOperator: TdxSpreadSheetConditionalFormattingRuleCellIsComparisonOperator;
      const AExpression1, AExpression2: string; ANode: TdxXMLNode): TdxSpreadSheetConditionalFormattingRuleCellIs;
    function CreateDuplicateValuesRule(ANode: TdxXMLNode): TdxSpreadSheetConditionalFormattingRuleDuplicateValues;
    function CreateExpressionRule(const AExpression: string; ANode: TdxXMLNode): TdxSpreadSheetConditionalFormattingRuleExpression;
    function CreateTopBottomRule(ANode: TdxXMLNode;
      ADirection: TdxSpreadSheetConditionalFormattingRuleTopBottomValuesDirection;
      AValueType: TdxSpreadSheetConditionalFormattingRuleTopBottomValuesValueType;
      AValue: Integer): TdxSpreadSheetConditionalFormattingRuleTopBottomValues;
    function CreateUniqueValuesRule(ANode: TdxXMLNode): TdxSpreadSheetConditionalFormattingRuleUniqueValues;
    function TryCreateCellIsRule(const ACondition: string; ANode: TdxXMLNode): TdxSpreadSheetCustomConditionalFormattingRule;
    function TryCreateTopBottomRule(const AFuncName, ACondition: string; ANode: TdxXMLNode;
      ADirection: TdxSpreadSheetConditionalFormattingRuleTopBottomValuesDirection;
      AValueType: TdxSpreadSheetConditionalFormattingRuleTopBottomValuesValueType;
      var ARule: TdxSpreadSheetCustomConditionalFormattingRule): Boolean;

    function CheckFunctionAndExtractParams(const AFuncName, ACondition: string; out AParams: TStringList): Boolean;
    function IsBeginsWith(const ATestString, AFullString: string): Boolean;
    procedure ReadColorScaleStop(AStop: TdxSpreadSheetConditionalFormattingRuleColorScaleStop; ANode: TdxXMLNode);
    procedure ReadCustomScaleStop(AStop: TdxSpreadSheetConditionalFormattingRuleCustomScaleStop; ANode: TdxXMLNode);
    procedure ReadStyle(ANode: TdxXMLNode; ARule: TdxSpreadSheetConditionalFormattingRuleStyleBased);

    function CreateColorScaleRule(ANode: TdxXMLNode): TdxSpreadSheetCustomConditionalFormattingRule;
    function CreateConditionBasedRule(ANode: TdxXMLNode): TdxSpreadSheetCustomConditionalFormattingRule;
    function CreateDataBarRule(ANode: TdxXMLNode): TdxSpreadSheetConditionalFormattingRuleDataBar;
    function CreateIconSetRule(ANode: TdxXMLNode): TdxSpreadSheetConditionalFormattingRuleIconSet;
  public
    procedure Execute; override;
  end;

implementation

uses
  dxSpreadSheetFormatODSTags, dxSpreadSheetStrs, dxSpreadSheetTypes, dxColorPicker, Math, dxHashUtils,
  StrUtils, AnsiStrings, dxSpreadSheetUtils, dxSpreadSheetFormatUtils, dxSpreadSheetFormatODSFormulas,
  dxSpreadSheetConditionalFormattingIconSet, dxSpreadSheetCoreStrs;

type
  TdxSpreadSheetConditionalFormattingRuleCustomColorScaleAccess = class(TdxSpreadSheetConditionalFormattingRuleCustomColorScale);
  TdxSpreadSheetCustomTextBoxContainerAccess = class(TdxSpreadSheetCustomTextBoxContainer);
  TdxSpreadSheetPictureAccess = class(TdxSpreadSheetPicture);
  TdxSpreadSheetTableItemGroupAccess = class(TdxSpreadSheetTableItemGroup);
  TdxSpreadSheetTableRowAccess = class(TdxSpreadSheetTableRow);
  TdxSpreadSheetTableRowCellsAccess = class(TdxSpreadSheetTableRowCells);
  TdxSpreadSheetTableViewAccess = class(TdxSpreadSheetTableView);

{ TdxSpreadSheetODSReaderCustomStyle }

constructor TdxSpreadSheetODSReaderCustomStyle.Create(ANode: TdxXMLNode; AOwner: TdxSpreadSheetODSReader);
begin
  inherited Create;
  FOwner := AOwner;
end;

class function TdxSpreadSheetODSReaderCustomStyle.GetStyleName(ANode: TdxXMLNode): TdxXMLString;
begin
  Result := ANode.Parent.Attributes.GetValue(sdxODSAttrStyleName);
end;

{ TdxSpreadSheetODSReaderStyleRepository<T> }

constructor TdxSpreadSheetODSReaderStyleRepository<T>.Create(AOwner: TdxSpreadSheetODSReader);
begin
  inherited Create([doOwnsValues]);
  FOwner := AOwner;
end;

procedure TdxSpreadSheetODSReaderStyleRepository<T>.Add(ANode: TdxXMLNode);
begin
  inherited Add(
    TdxSpreadSheetODSReaderCustomStyleClass(T).GetStyleName(ANode),
    TdxSpreadSheetODSReaderCustomStyleClass(T).Create(ANode, FOwner));
end;

function TdxSpreadSheetODSReaderStyleRepository<T>.GetValueOrNil(const S: TdxXMLString): T;
begin
  if not TryGetValue(S, Result) then
    Result := Default(T);
end;

function TdxSpreadSheetODSReaderStyleRepository<T>.TryGetValue(const S: TdxXMLString; out AValue: T): Boolean;
begin
  Result := inherited TryGetValue(S, AValue);
end;

{ TdxSpreadSheetODSReaderTableStyle }

constructor TdxSpreadSheetODSReaderTableStyle.Create(ANode: TdxXMLNode; AOwner: TdxSpreadSheetODSReader);
begin
  inherited Create(ANode, AOwner);
  PageStyle := AOwner.MasterPageStyles.GetValueOrNil(ANode.Parent.Attributes.GetValue(sdxODSAttrStyleMasterPageName));
  Visible := ANode.Attributes.GetValueAsBoolean(sdxODSAttrTableDisplay, True);
end;

procedure TdxSpreadSheetODSReaderTableStyle.ApplyTo(ATableView: TdxSpreadSheetTableView);
begin
  if PageStyle <> nil then
    PageStyle.ApplyTo(ATableView.OptionsPrint);
  ATableView.Visible := Visible;
end;

{ TdxSpreadSheetODSReaderTableItemStyle }

procedure TdxSpreadSheetODSReaderTableItemStyle.ApplyTo(AItem: TdxSpreadSheetTableItem);
begin
  if AutoSize then
    AItem.DefaultSize := True
  else
    if Size > 0 then
      AItem.Size := Size;
end;

function TdxSpreadSheetODSReaderTableItemStyle.IsDefault: Boolean;
begin
  Result := (Self <> nil) and (AutoSize or (Size = 0));
end;

{ TdxSpreadSheetODSReaderColumnStyle }

constructor TdxSpreadSheetODSReaderColumnStyle.Create(ANode: TdxXMLNode; AOwner: TdxSpreadSheetODSReader);
begin
  inherited Create(ANode, AOwner);
  Size := TdxSpreadSheetODSHelper.StringToSize(ANode.Attributes.GetValueAsString(sdxODSAttrStyleColumnWidth));
end;

{ TdxSpreadSheetODSReaderGradientStyle }

constructor TdxSpreadSheetODSReaderGradientStyle.Create(ANode: TdxXMLNode; AOwner: TdxSpreadSheetODSReader);
var
  AInverseOrder: Boolean;
begin
  inherited Create(ANode, AOwner);
  Mode := dxGetNearestGradientMode(ANode.Attributes.GetValueAsInteger(sdxODSAttrDrawAngle) / 10 - 90, AInverseOrder);
  Color1 := DecodeColor(ANode, sdxODSAttrDrawStartColor, sdxODSAttrDrawStartIntensity);
  Color2 := DecodeColor(ANode, sdxODSAttrDrawEndColor, sdxODSAttrDrawEndIntensity);
end;

function TdxSpreadSheetODSReaderGradientStyle.DecodeColor(
  ANode: TdxXMLNode; const AColorValueName, AColorIntensityName: TdxXMLString): TColor;
var
  AAttr: TdxXMLNodeAttribute;
  AColorIntensity: Integer;
  Q: TRGBQuad;
begin
  Result := TdxSpreadSheetODSHelper.StringToColor(ANode.Attributes.GetValueAsString(AColorValueName));
  if ANode.Attributes.Find(AColorIntensityName, AAttr) then
  begin
    AColorIntensity := TdxSpreadSheetODSHelper.StringToPercents(AAttr.ValueAsString);
    if AColorIntensity < 100 then
    begin
      Q := dxColorToRGBQuad(Result);
      Q.rgbRed := MulDiv(Q.rgbRed, AColorIntensity, 100);
      Q.rgbGreen := MulDiv(Q.rgbGreen, AColorIntensity, 100);
      Q.rgbBlue := MulDiv(Q.rgbBlue, AColorIntensity, 100);
      Result := dxRGBQuadToColor(Q);
    end;
  end;
end;

class function TdxSpreadSheetODSReaderGradientStyle.GetStyleName(ANode: TdxXMLNode): TdxXMLString;
begin
  Result := ANode.Attributes.GetValue(sdxODSAttrDrawName);
end;

{ TdxSpreadSheetODSReaderParagraphStyle }

constructor TdxSpreadSheetODSReaderParagraphStyle.Create(ANode: TdxXMLNode; AOwner: TdxSpreadSheetODSReader);
begin
  inherited Create(ANode, AOwner);
  Alignment := TdxSpreadSheetODSHelper.StringToAlignHorz(ANode.Attributes.GetValue(sdxODSAttrFOTextAlign));
end;

{ TdxSpreadSheetODSReaderGraphicStyle }

constructor TdxSpreadSheetODSReaderGraphicStyle.Create(ANode: TdxXMLNode; AOwner: TdxSpreadSheetODSReader);
begin
  inherited Create(ANode, AOwner);
  LoadFill(ANode);
  LoadStroke(ANode);
  ProtectSize := Pos(sdxODSValueStyleProtectSize, ANode.Attributes.GetValue(sdxODSAttrStyleProtect)) > 0;
  ProtectPosition := Pos(sdxODSValueStyleProtectPosition, ANode.Attributes.GetValue(sdxODSAttrStyleProtect)) > 0;

  AutoSize := ANode.Attributes.GetValueAsBoolean(sdxODSAttrDrawAutoGrowHeight);
  TextAreaHorzAlign := TdxSpreadSheetODSHelper.StringToAlignment(ANode.Attributes.GetValue(sdxODSAttrDrawTextAreaHorizontalAlign));
  TextAreaVertAlign := TdxSpreadSheetODSHelper.StringToVerticalAlignment(ANode.Attributes.GetValue(sdxODSAttrDrawTextAreaVerticalAlign));
  Padding.Bottom := TdxSpreadSheetODSHelper.StringToSize(ANode.Attributes.GetValueAsString(sdxODSAttrFOPaddingBottom));
  Padding.Left := TdxSpreadSheetODSHelper.StringToSize(ANode.Attributes.GetValueAsString(sdxODSAttrFOPaddingLeft));
  Padding.Right := TdxSpreadSheetODSHelper.StringToSize(ANode.Attributes.GetValueAsString(sdxODSAttrFOPaddingRight));
  Padding.Top := TdxSpreadSheetODSHelper.StringToSize(ANode.Attributes.GetValueAsString(sdxODSAttrFOPaddingTop));
  WordWrap := ANode.Attributes.GetValue(sdxODSAttrFOWrapOption) = 'wrap';
end;

procedure TdxSpreadSheetODSReaderGraphicStyle.ApplyToBrush(ABrush: TdxGPBrush);
begin
  case FillMode of
    sofmSolid:
      begin
        if FillColor <> clDefault then
          ABrush.Color := dxColorToAlphaColor(FillColor);
        ABrush.Style := gpbsSolid;
      end;

    sofmHatch:
      if FillHatch <> nil then
      begin
        dxSpreadSheetLoadBrushPattern(ABrush, HInstance, sdxODSBrushPatterns[FillHatch.Pattern],
          dxColorToAlphaColor(FillHatch.Color), dxColorToAlphaColor(FillColor));
      end;

    sofmTexture:
      if FillTexture <> nil then
      begin
        FillTexture.Data.Position := 0;
        ABrush.Texture.LoadFromStream(FillTexture.Data);
        ABrush.Style := gpbsTexture;
      end;

    sofmGradient:
      if FillGradient <> nil then
      begin
        ABrush.GradientPoints.Add(0, dxColorToAlphaColor(FillGradient.Color1));
        ABrush.GradientPoints.Add(1, dxColorToAlphaColor(FillGradient.Color2));
        ABrush.GradientMode := FillGradient.Mode;
        ABrush.Style := gpbsGradient;
      end;
  end;
end;

procedure TdxSpreadSheetODSReaderGraphicStyle.ApplyToPen(APen: TdxGPPen);
begin
  if StrokeStyle <> nil then
    APen.Style := StrokeStyle.Style;
  APen.Brush.Color := StrokeColor;
  APen.Width := StrokeWidth;
end;

procedure TdxSpreadSheetODSReaderGraphicStyle.LoadFill(ANode: TdxXMLNode);
begin
  FillMode := TdxSpreadSheetODSHelper.StringToFillStyle(ANode.Attributes.GetValue(sdxODSNodeFill));
  case FillMode of
    sofmGradient:
      FillGradient := Owner.GradientStyles.GetValueOrNil(ANode.Attributes.GetValue(sdxODSAttrDrawFillGradientName));
    sofmTexture:
      FillTexture := Owner.ImageStyles.GetValueOrNil(ANode.Attributes.GetValue(sdxODSAttrDrawFillImageName));
    sofmSolid:
      FillColor := TdxSpreadSheetODSHelper.StringToColor(ANode.Attributes.GetValueAsString(sdxODSAttrDrawFillColor));
    sofmHatch:
      begin
        FillHatch := Owner.HatchStyles.GetValueOrNil(ANode.Attributes.GetValue(sdxODSAttrDrawHatchName));
        if ANode.Attributes.GetValueAsBoolean(sdxODSAttrDrawFillHatchSolid) then
          FillColor := TdxSpreadSheetODSHelper.StringToColor(ANode.Attributes.GetValueAsString(sdxODSAttrDrawFillColor))
        else
          FillColor := clNone;
      end;
  end;
end;

procedure TdxSpreadSheetODSReaderGraphicStyle.LoadStroke(ANode: TdxXMLNode);
var
  AAttribute: TdxXMLNodeAttribute;
  AOpacity: Byte;
  AOpacityAttribute: TdxXMLNodeAttribute;
  AStrokeStyle: TdxXMLString;
begin
  AStrokeStyle := ANode.Attributes.GetValue(sdxODSAttrDrawStroke);
  if AStrokeStyle <> sdxODSValueDrawStrokeNone then
  begin
    StrokeWidth := Max(1, TdxSpreadSheetODSHelper.StringToSize(ANode.Attributes.GetValueAsString(sdxODSAttrSvgStrokeWidth)));

    if ANode.Attributes.Find(sdxODSAttrSvgStrokeColor, AAttribute) then
    begin
      if ANode.Attributes.Find(sdxODSAttrSvgStrokeOpacity, AOpacityAttribute) then
        AOpacity := TdxSpreadSheetODSHelper.StringToPercents(AOpacityAttribute.ValueAsString)
      else
        AOpacity := 100;

      StrokeColor := dxColorToAlphaColor(TdxSpreadSheetODSHelper.StringToColor(AAttribute.ValueAsString), MulDiv(AOpacity, MaxByte, 100));
    end
    else
      StrokeColor := $FF41719C;

    if AStrokeStyle = sdxODSValueDrawStrokeDash then
      StrokeStyle := Owner.StrokeDashStyles.GetValueOrNil(ANode.Attributes.GetValue(sdxODSAttrDrawStrokeDash));
  end;
end;

{ TdxSpreadSheetODSReaderHatchStyle }

constructor TdxSpreadSheetODSReaderHatchStyle.Create(ANode: TdxXMLNode; AOwner: TdxSpreadSheetODSReader);
begin
  inherited Create(ANode, AOwner);
  Color := TdxSpreadSheetODSHelper.StringToColor(ANode.Attributes.GetValueAsString(sdxODSAttrDrawColor));
  Pattern := DetectBrushPattern(ANode);
end;

class function TdxSpreadSheetODSReaderHatchStyle.GetStyleName(ANode: TdxXMLNode): TdxXMLString;
begin
  Result := ANode.Attributes.GetValue(sdxODSAttrDrawName);
end;

function TdxSpreadSheetODSReaderHatchStyle.DetectBrushPattern(ANode: TdxXMLNode): TdxSpreadSheetODSBrushPattern;
var
  AStyle: TdxXMLString;
  AUnused: Boolean;
begin
  AStyle := ANode.Attributes.GetValue(sdxODSAttrDrawStyle);
  if AStyle = sdxODSValueDrawStyleTriple then
    Result := sobpTriple
  else

  if AStyle = sdxODSValueDrawStyleDouble then
  begin
    if ANode.Attributes.GetValueAsInteger(sdxODSAttrDrawRotation) mod 900 = 0 then
      Result := sobpCell
    else
      Result := sobpDiagonal
  end
  else
    case dxGetNearestGradientMode(ANode.Attributes.GetValueAsInteger(sdxODSAttrDrawRotation) / 10, AUnused) of
      gpbgmHorizontal:
        Result := sobpSingle0;
      gpbgmVertical:
        Result := sobpSingle90;
      gpbgmForwardDiagonal:
        if TdxSpreadSheetODSHelper.StringToSize(ANode.Attributes.GetValueAsString(sdxODSAttrDrawDistance)) > 10 then
          Result := sobpSingle45Wide
        else
          Result := sobpSingle45;
    else
      Result := sobpSingle135;
    end;
end;

{ TdxSpreadSheetODSReaderImageStyle }

constructor TdxSpreadSheetODSReaderImageStyle.Create(ANode: TdxXMLNode; AOwner: TdxSpreadSheetODSReader);
begin
  inherited Create(ANode, AOwner);
  Data := Owner.ReadFile(ANode.Attributes.GetValue(sdxODSAttrXLinkHRef));
end;

destructor TdxSpreadSheetODSReaderImageStyle.Destroy;
begin
  FreeAndNil(Data);
  inherited Destroy;
end;

class function TdxSpreadSheetODSReaderImageStyle.GetStyleName(ANode: TdxXMLNode): TdxXMLString;
begin
  Result := ANode.Attributes.GetValue(sdxODSAttrDrawName);
end;

{ TdxSpreadSheetODSReaderMasterPageStyle }

constructor TdxSpreadSheetODSReaderMasterPageStyle.Create(ANode: TdxXMLNode; AOwner: TdxSpreadSheetODSReader);
begin
  inherited Create(ANode, AOwner);
  HeaderFooter := TdxSpreadSheetTableViewOptionsPrintHeaderFooter.Create(nil);
  DisplayName := ANode.Attributes.GetValueAsString(sdxODSAttrStyleDisplayName);
  PageLayout := Owner.PageLayouts.GetValueOrNil(ANode.Attributes.GetValue(sdxODSAttrStylePageLayoutName));

  if ANode.FindChild(sdxODSNodeStyleFooterLeft) <> nil then
  begin
    ParseText(HeaderFooter.EvenPagesFooter, ANode.FindChild(sdxODSNodeStyleFooter));
    ParseText(HeaderFooter.CommonFooter, ANode.FindChild(sdxODSNodeStyleFooterLeft));
  end
  else
    ParseText(HeaderFooter.CommonFooter, ANode.FindChild(sdxODSNodeStyleFooter));

  if ANode.FindChild(sdxODSNodeStyleHeaderLeft) <> nil then
  begin
    ParseText(HeaderFooter.CommonHeader, ANode.FindChild(sdxODSNodeStyleHeaderLeft));
    ParseText(HeaderFooter.EvenPagesHeader, ANode.FindChild(sdxODSNodeStyleHeader));
  end
  else
    ParseText(HeaderFooter.CommonHeader, ANode.FindChild(sdxODSNodeStyleHeader));
end;

destructor TdxSpreadSheetODSReaderMasterPageStyle.Destroy;
begin
  FreeAndNil(HeaderFooter);
  inherited Destroy;
end;

procedure TdxSpreadSheetODSReaderMasterPageStyle.ApplyTo(AOptions: TdxSpreadSheetTableViewOptionsPrint);
begin
  AOptions.HeaderFooter.Assign(HeaderFooter);
  if PageLayout <> nil then
    PageLayout.ApplyTo(AOptions);
end;

class function TdxSpreadSheetODSReaderMasterPageStyle.GetStyleName(ANode: TdxXMLNode): TdxXMLString;
begin
  Result := ANode.Attributes.GetValue(sdxODSAttrStyleName);
end;

procedure TdxSpreadSheetODSReaderMasterPageStyle.ParseText(
  AText: TdxSpreadSheetTableViewOptionsPrintHeaderFooterText; ANode: TdxXMLNode);

  function ParseParagraph(ANode: TdxXMLNode): string;
  var
    AHelper: TdxSpreadSheetODSReaderRichTextHelper;
  begin
    AHelper := TdxSpreadSheetODSReaderRichTextHelper.Create(nil);
    try
      AHelper.Process(ANode);
      Result := AHelper.Text;
    finally
      AHelper.Free;
    end;
  end;

  function ParseRegion(const ARegionName: TdxXMLString): string;
  var
    ASubNode: TdxXMLNode;
  begin
    if ANode.FindChild([ARegionName, sdxODSNodeText], ASubNode) then
      Result := ParseParagraph(ASubNode)
    else
      Result := '';
  end;

begin
  if ANode.Attributes.GetValueAsBoolean(sdxODSAttrStyleDisplay, True) then
  begin
    if (ANode.FindChild(sdxODSNodeStyleRegionLeft) <> nil) or
      (ANode.FindChild(sdxODSNodeStyleRegionRight) <> nil) or
      (ANode.FindChild(sdxODSNodeStyleRegionCenter) <> nil) then
    begin
      AText.LeftSection := ParseRegion(sdxODSNodeStyleRegionLeft);
      AText.CenterSection := ParseRegion(sdxODSNodeStyleRegionCenter);
      AText.RightSection := ParseRegion(sdxODSNodeStyleRegionRight);
    end
    else
      if ANode.HasChildren and (ANode.First.Name = sdxODSNodeText) then
        AText.CenterSection := ParseParagraph(ANode.First);
  end;
end;

{ TdxSpreadSheetODSReaderPageLayoutStyle }

constructor TdxSpreadSheetODSReaderPageLayoutStyle.Create(ANode: TdxXMLNode; AOwner: TdxSpreadSheetODSReader);
var
  ASubNode: TdxXMLNode;
begin
  inherited Create(ANode, AOwner);
  Margins := TdxSpreadSheetTableViewOptionsPrintPageMargins.Create(nil);
  PageCenterHorz := bDefault;
  PageCenterVert := bDefault;
  GridLines := bDefault;
  Headers := bDefault;

  ReadMargins(ANode);
  if ANode.FindChild(sdxODSNodeStylePageLayoutProperties, ASubNode) then
  begin
    ReadPrintElements(ASubNode);
    ReadPageLayoutProperties(ASubNode);
  end;
end;

destructor TdxSpreadSheetODSReaderPageLayoutStyle.Destroy;
begin
  FreeAndNil(Margins);
  inherited Destroy;
end;

procedure TdxSpreadSheetODSReaderPageLayoutStyle.ApplyTo(AOptions: TdxSpreadSheetTableViewOptionsPrint);
begin
  AOptions.Printing.PageOrder := PageOrder;
  AOptions.Printing.HorizontalCentered := PageCenterHorz;
  AOptions.Printing.VerticalCentered := PageCenterHorz;

  AOptions.Page.FirstPageNumber := FirstPageNumber;
  AOptions.Page.Orientation := PageOrientation;
  AOptions.Page.Margins.Assign(Margins);
  if (PageSize.X > 0) and (PageSize.Y > 0) then
    AOptions.Page.Paper.CustomSize.Point := PageSize;
  if Scale > 0 then
    AOptions.Page.Scale := Scale;

  AOptions.Source.GridLines := GridLines;
  AOptions.Source.Headers := Headers;
end;

class function TdxSpreadSheetODSReaderPageLayoutStyle.GetStyleName(ANode: TdxXMLNode): TdxXMLString;
begin
  Result := ANode.Attributes.GetValue(sdxODSAttrStyleName);
end;

procedure TdxSpreadSheetODSReaderPageLayoutStyle.ReadMargins(ANode: TdxXMLNode);
var
  AAttr: TdxXMLNodeAttribute;
  ASubNode: TdxXMLNode;
begin
  if ANode.FindChild(sdxODSNodeStyleHeaderStyle, ASubNode) and ASubNode.Attributes.Find(sdxODSAttrFOMinHeight, AAttr) then
    Margins.Header := TdxValueUnitsHelper.ValueToInches(AAttr.ValueAsString);
  if ANode.FindChild(sdxODSNodeStyleFooterStyle, ASubNode) and ASubNode.Attributes.Find(sdxODSAttrFOMinHeight, AAttr) then
    Margins.Footer := TdxValueUnitsHelper.ValueToInches(AAttr.ValueAsString);
  if ANode.FindChild(sdxODSNodeStylePageLayoutProperties, ASubNode) then
  begin
    if ASubNode.HasAttribute(sdxODSAttrFOMarginLeft) or ASubNode.HasAttribute(sdxODSAttrFOMarginRight) or
      ASubNode.HasAttribute(sdxODSAttrFOMarginBottom) or ASubNode.HasAttribute(sdxODSAttrFOMarginTop) then
    begin
      Margins.Left := TdxValueUnitsHelper.ValueToInchesF(ASubNode.Attributes.GetValueAsString(sdxODSAttrFOMarginLeft));
      Margins.Bottom := TdxValueUnitsHelper.ValueToInchesF(ASubNode.Attributes.GetValueAsString(sdxODSAttrFOMarginBottom));
      Margins.Right := TdxValueUnitsHelper.ValueToInchesF(ASubNode.Attributes.GetValueAsString(sdxODSAttrFOMarginRight));
      Margins.Top := TdxValueUnitsHelper.ValueToInchesF(ASubNode.Attributes.GetValueAsString(sdxODSAttrFOMarginTop));
    end;
  end;
end;

procedure TdxSpreadSheetODSReaderPageLayoutStyle.ReadPageLayoutProperties(ANode: TdxXMLNode);
var
  AAttr: TdxXMLNodeAttribute;
begin
  FirstPageNumber := StrToIntDef(ANode.Attributes.GetValueAsString(sdxODSAttrStyleFirstPageNumber), 0);

  PageSize.X := TdxValueUnitsHelper.ValueToInchesF(ANode.Attributes.GetValueAsString(sdxODSAttrFOPageWidth));
  PageSize.Y := TdxValueUnitsHelper.ValueToInchesF(ANode.Attributes.GetValueAsString(sdxODSAttrFOPageHeight));

  if ANode.Attributes.Find(sdxODSAttrStyleScaleTo, AAttr) then
    Scale := TdxSpreadSheetODSHelper.StringToPercents(AAttr.ValueAsString);

  if ANode.Attributes.Find(sdxODSAttrStylePrintOrientation, AAttr) then
  begin
    if SameText(AAttr.ValueAsString, sdxODSValuePrintOrientationLandscape) then
      PageOrientation := oppoLandscape
    else
      PageOrientation := oppoPortrait;
  end;

  if ANode.Attributes.Find(sdxODSAttrStylePrintPageOrder, AAttr) then
  begin
    if SameText(AAttr.ValueAsString, sdxODSValuePrintPageOrderOverThenDown) then
      PageOrder := opppOverThenDown
    else
      PageOrder := opppDownThenOver;
  end;

  if ANode.Attributes.Find(sdxODSAttrStyleTableCentering, AAttr) then
  begin
    PageCenterHorz := dxBooleanToDefaultBoolean(
      SameText(AAttr.ValueAsString, sdxODSTableCenteringHorz) or
      SameText(AAttr.ValueAsString, sdxODSTableCenteringBoth));
    PageCenterVert := dxBooleanToDefaultBoolean(
      SameText(AAttr.ValueAsString, sdxODSTableCenteringVert) or
      SameText(AAttr.ValueAsString, sdxODSTableCenteringBoth));
  end;
end;

procedure TdxSpreadSheetODSReaderPageLayoutStyle.ReadPrintElements(ANode: TdxXMLNode);
var
  AAttr: TdxXMLNodeAttribute;
  AList: TStringList;
begin
  if ANode.Attributes.Find(sdxODSAttrStylePrint, AAttr) then
  begin
    AList := TStringList.Create;
    try
      AList.Text := StringReplace(AAttr.ValueAsString, ' ', #13#10, [rfReplaceAll]);
      Headers := dxBooleanToDefaultBoolean(AList.IndexOf(sdxODSValuePrintObjectHeaders) >= 0);
      GridLines := dxBooleanToDefaultBoolean(AList.IndexOf(sdxODSValuePrintObjectGridLines) >= 0);
    finally
      AList.Free;
    end;
  end;
end;

{ TdxSpreadSheetODSReaderRowStyle }

constructor TdxSpreadSheetODSReaderRowStyle.Create(ANode: TdxXMLNode; AOwner: TdxSpreadSheetODSReader);
begin
  inherited Create(ANode, AOwner);
  AutoSize := ANode.Attributes.GetValueAsBoolean(sdxODSAttrStyleRowAutoHeight);
  Size := TdxSpreadSheetODSHelper.StringToSize(ANode.Attributes.GetValueAsString(sdxODSAttrStyleRowHeight));
end;

{ TdxSpreadSheetODSReaderStrokeDashStyle }

constructor TdxSpreadSheetODSReaderStrokeDashStyle.Create(ANode: TdxXMLNode; AOwner: TdxSpreadSheetODSReader);
begin
  inherited Create(ANode, AOwner);
  Style := DetectPenStyle(ANode);
end;

class function TdxSpreadSheetODSReaderStrokeDashStyle.GetStyleName(ANode: TdxXMLNode): TdxXMLString;
begin
  Result := ANode.Attributes.GetValue(sdxODSAttrDrawName);
end;

function TdxSpreadSheetODSReaderStrokeDashStyle.DetectPenStyle(ANode: TdxXMLNode): TdxGPPenStyle;
const
  MaxDotSize = 4;
var
  ADotsCount: array[0..1] of Integer;
  ADotsLength: array[0..1] of Integer;
begin
  ADotsCount[0] := ANode.Attributes.GetValueAsInteger(sdxODSAttrDrawDots1);
  ADotsCount[1] := ANode.Attributes.GetValueAsInteger(sdxODSAttrDrawDots2);
  ADotsLength[0] := TdxSpreadSheetODSHelper.StringToSize(ANode.Attributes.GetValueAsString(sdxODSAttrDrawDots1Length));
  ADotsLength[1] := TdxSpreadSheetODSHelper.StringToSize(ANode.Attributes.GetValueAsString(sdxODSAttrDrawDots2Length));

  if ADotsCount[0] > 0 then
    ADotsLength[0] := Max(ADotsLength[0], 1);
  if ADotsCount[1] > 0 then
    ADotsLength[1] := Max(ADotsLength[1], 1);

  if (ADotsLength[1] > 0) and (ADotsLength[1] > ADotsLength[0]) then
  begin
    ExchangeLongWords(ADotsCount[0], ADotsCount[1]);
    ExchangeLongWords(ADotsLength[0], ADotsLength[1]);
  end;

  if (ADotsLength[1] = 0) or (ADotsLength[1] > MaxDotSize) then
  begin
    if ADotsLength[0] > MaxDotSize then
      Result := gppsDash
    else
      Result := gppsDot;
  end
  else
    if ADotsCount[1] > 1 then
      Result := gppsDashDotDot
    else
      Result := gppsDashDot;
end;

{ TdxSpreadSheetODSStyles }

constructor TdxSpreadSheetODSStyles.Create;
begin
  inherited Create;
  FDisplayNameMap := TDictionary<string, string>.Create;
end;

destructor TdxSpreadSheetODSStyles.Destroy;
begin
  FreeAndNil(FDisplayNameMap);
  inherited Destroy;
end;

procedure TdxSpreadSheetODSStyles.Add(const AName, ADisplayName: string; AHandle: TdxSpreadSheetCellStyleHandle);
begin
  inherited Add(AName, AHandle);
  if ADisplayName <> '' then
    FDisplayNameMap.Add(ADisplayName, AName);
end;

function TdxSpreadSheetODSStyles.TryGetValue(const AName: string; out AHandle: TdxSpreadSheetCellStyleHandle): Boolean;
var
  S: string;
begin
  Result := inherited TryGetValue(AName, AHandle);
  if not Result and FDisplayNameMap.TryGetValue(AName, S) then
    Result := inherited TryGetValue(S, AHandle);
end;

procedure TdxSpreadSheetODSStyles.ValueNotify(const Value: TdxSpreadSheetCellStyleHandle; Action: TCollectionNotification);
begin
  inherited ValueNotify(Value, Action);
  case Action of
    cnAdded:
      Value.AddRef;
    cnRemoved:
      Value.Release;
  end;
end;

{ TdxSpreadSheetODSFormulaAsTextInfoList }

function TdxSpreadSheetODSFormulaAsTextInfoList.CreateParser: TObject;
begin
  Result := TdxSpreadSheetODSFormulaParser.Create(SpreadSheet);
end;

{ TdxSpreadSheetODSReader }

constructor TdxSpreadSheetODSReader.Create(ASpreadSheet: TdxCustomSpreadSheet; AStream: TStream);
begin
  inherited Create(ASpreadSheet, AStream);
  FStyles := TdxSpreadSheetODSStyles.Create;
  FParagraphStyles := TdxSpreadSheetODSReaderStyleRepository<TdxSpreadSheetODSReaderParagraphStyle>.Create(Self);
  FStrokeDashStyles := TdxSpreadSheetODSReaderStyleRepository<TdxSpreadSheetODSReaderStrokeDashStyle>.Create(Self);
  FMasterPageStyles := TdxSpreadSheetODSReaderStyleRepository<TdxSpreadSheetODSReaderMasterPageStyle>.Create(Self);
  FHatchStyles := TdxSpreadSheetODSReaderStyleRepository<TdxSpreadSheetODSReaderHatchStyle>.Create(Self);
  FHyperlinks := TStringList.Create;
  FImageStyles := TdxSpreadSheetODSReaderStyleRepository<TdxSpreadSheetODSReaderImageStyle>.Create(Self);
  FColumnStyles := TdxSpreadSheetODSReaderStyleRepository<TdxSpreadSheetODSReaderColumnStyle>.Create(Self);
  FRowStyles := TdxSpreadSheetODSReaderStyleRepository<TdxSpreadSheetODSReaderRowStyle>.Create(Self);
  FTableStyles := TdxSpreadSheetODSReaderStyleRepository<TdxSpreadSheetODSReaderTableStyle>.Create(Self);
  FGraphicStyles := TdxSpreadSheetODSReaderStyleRepository<TdxSpreadSheetODSReaderGraphicStyle>.Create(Self);
  FGradientStyles := TdxSpreadSheetODSReaderStyleRepository<TdxSpreadSheetODSReaderGradientStyle>.Create(Self);
  FPageLayouts := TdxSpreadSheetODSReaderStyleRepository<TdxSpreadSheetODSReaderPageLayoutStyle>.Create(Self);
  FDataFormatConverter := TdxSpreadSheetODSReaderDataFormatConverter.Create;
  FFormulas := TdxSpreadSheetODSFormulaAsTextInfoList.Create(SpreadSheet);
  FDataFormats := TDictionary<TdxXMLString, string>.Create;
end;

destructor TdxSpreadSheetODSReader.Destroy;
begin
  FreeAndNil(FStrokeDashStyles);
  FreeAndNil(FPageLayouts);
  FreeAndNil(FHatchStyles);
  FreeAndNil(FImageStyles);
  FreeAndNil(FGraphicStyles);
  FreeAndNil(FGradientStyles);
  FreeAndNil(FMasterPageStyles);
  FreeAndNil(FParagraphStyles);
  FreeAndNil(FDataFormatConverter);
  FreeAndNil(FDataFormats);
  FreeAndNil(FColumnStyles);
  FreeAndNil(FTableStyles);
  FreeAndNil(FRowStyles);
  FreeAndNil(FFormulas);
  FreeAndNil(FStyles);
  FreeAndNil(FHyperlinks);
  inherited Destroy;
end;

function TdxSpreadSheetODSReader.CheckMimeType: Boolean;
var
  AMimeType: AnsiString;
begin
  AMimeType := ReadMimeType;
  Result := (AMimeType = sdxODSMimeTypeSpreadSheet) or (AMimeType = sdxODSMimeTypeSpreadSheetTemplate);
end;

procedure TdxSpreadSheetODSReader.ReadData;
begin
  if CheckMimeType then
  begin
    ExecuteSubTask(TdxSpreadSheetODSReaderStylesFileParser.Create(sdxODSStylesFile, Self));
    ExecuteSubTask(TdxSpreadSheetODSReaderContentFileParser.Create(sdxODSContentFile, Self));
    if PackageReader.Exists(sdxODSSettingsFile) then
      ExecuteSubTask(TdxSpreadSheetODSReaderSettingsFileParser.Create(sdxODSSettingsFile, Self));
    Formulas.ResolveReferences;
    ResolveHyperlinks;
  end
  else
    DoError(sdxErrorInvalidDocumentType, ssmtError);
end;

function TdxSpreadSheetODSReader.CreateProgressHelper: TdxSpreadSheetCustomFilerProgressHelper;
begin
  Result := TdxSpreadSheetCustomFilerProgressHelper.Create(Self, 4);
end;

procedure TdxSpreadSheetODSReader.ResolveHyperlinks;
var
  I: Integer;
  AValue: string;
begin
  for I := 0 to Hyperlinks.Count - 1 do
  begin
    AValue := Hyperlinks[I];
    if (Length(AValue) > 0) and (AValue[1] = '#') then
    begin
      Delete(AValue, 1, 1);
      AValue := StringReplace(AValue, ' ', '_', [rfReplaceAll]);
      AValue := TdxSpreadSheetODSFormula.Convert(
        SpreadSheet, TdxSpreadSheetODSFormulaParser.Tag + '[' + AValue + ']');

    end;
    TdxSpreadSheetHyperlink(Hyperlinks.Objects[I]).Value := AValue;
  end;
end;

function TdxSpreadSheetODSReader.ReadMimeType: AnsiString;
var
  AStream: TMemoryStream;
begin
  AStream := ReadFile(sdxODSMimeTypeFile);
  try
    SetString(Result, PAnsiChar(AStream.Memory), AStream.Size);
  finally
    AStream.Free;
  end;
end;

{ TdxSpreadSheetODSReaderCustomParser }

function TdxSpreadSheetODSReaderCustomParser.GetOwner: TdxSpreadSheetODSReader;
begin
  Result := TdxSpreadSheetODSReader(inherited Owner);
end;

{ TdxSpreadSheetODSReaderCustomNodeParser }

constructor TdxSpreadSheetODSReaderCustomNodeParser.Create(ANode: TdxXMLNode; AOwner: TdxSpreadSheetODSReader);
begin
  inherited Create(AOwner);
  FNode := ANode;
end;

{ TdxSpreadSheetODSReaderCustomDocumentParser }

constructor TdxSpreadSheetODSReaderCustomDocumentParser.Create(
  const AFileName: AnsiString; AOwner: TdxSpreadSheetODSReader);
begin
  inherited Create(nil, AOwner);
  FDocumentFileName := AFileName;
  FDocument := ReadXML(AFileName);
  FNode := Document.Root.First;
end;

destructor TdxSpreadSheetODSReaderCustomDocumentParser.Destroy;
begin
  FreeAndNil(FDocument);
  inherited Destroy;
end;

{ TdxSpreadSheetODSReaderContentFileParser }

procedure TdxSpreadSheetODSReaderContentFileParser.Execute;
var
  ANode: TdxXMLNode;
begin
  Owner.FVersion := Node.Attributes.GetValueAsFloat(sdxODSAttrVersion);
  if Node.FindChild(sdxODSNodeOfficeAutomaticStyles, ANode) then
    ExecuteSubTask(TdxSpreadSheetODSReaderStylesParser.Create(ANode, Owner))
  else
    Owner.ProgressHelper.SkipStage;

  if Node.FindChild([sdxODSNodeOfficeBody, sdxODSNodeOfficeSpreadSheet], ANode) then
    ExecuteSubTask(TdxSpreadSheetODSReaderContentSpreadSheetParser.Create(ANode, Owner));
end;

{ TdxSpreadSheetODSReaderContentSpreadSheetParser }

procedure TdxSpreadSheetODSReaderContentSpreadSheetParser.Execute;
begin
  Owner.ProgressHelper.BeginStage(Node.Count);
  try
    Node.ForEach(ProcessSubNode);
    SpreadSheet.OptionsBehavior.Protected := Node.Attributes.GetValueAsBoolean(sdxODSAttrTableStructureProtected);
  finally
    Owner.ProgressHelper.EndStage;
  end;
end;

procedure TdxSpreadSheetODSReaderContentSpreadSheetParser.ProcessCalculationSettings(ANode: TdxXMLNode);
const
  Map: array[Boolean] of TdxSpreadSheetDateTimeSystem = (dts1900, dts1904);
var
  AIs1904DateSystem: Boolean;
  AIterate: Boolean;
  AIterateCount: Integer;
  ASubNode: TdxXMLNode;
begin
  if ANode.FindChild(sdxODSNodeTableIteration, ASubNode) then
  begin
    AIterateCount := ASubNode.Attributes.GetValueAsInteger(sdxODSAttrTableSteps, 100);
    AIterate := ASubNode.Attributes.GetValue(sdxODSAttrTableStatus) = sdxODSValueEnable;
  end
  else
  begin
    AIterateCount := 100;
    AIterate := False;
  end;

  if ANode.FindChild(sdxODSNodeTableNullDate, ASubNode) then
    AIs1904DateSystem := ASubNode.Attributes.GetValue(sdxODSAttrTableDateValue) = sdxODSValueNullDate1904
  else
    AIs1904DateSystem := False;

  SpreadSheet.OptionsView.DateTimeSystem := Map[AIs1904DateSystem];
  SpreadSheet.OptionsBehavior.IterativeCalculation := AIterate;
  SpreadSheet.OptionsBehavior.IterativeCalculationMaxCount := AIterateCount;
end;

procedure TdxSpreadSheetODSReaderContentSpreadSheetParser.ProcessSubNode(ANode: TdxXMLNode; AUserData: Pointer);
begin
  if ANode.Name = sdxODSNodeTable then
  begin
    if ANode.FindChild(sdxODSNodeTableSource) = nil then
      ExecuteSubTask(TdxSpreadSheetODSReaderTableViewParser.Create(ANode, Owner));
  end
  else

  if ANode.Name = sdxODSNodeTableCalculationSettings then
    ProcessCalculationSettings(ANode)
  else

  if (ANode.Name = sdxODSNodeTableNamedExpressions) or (ANode.Name = sdxODSNodeTableDataBaseRanges) then
    ExecuteSubTask(TdxSpreadSheetODSReaderDefinedNamesParser.Create(ANode, Owner));

  Owner.ProgressHelper.NextTask;
end;

{ TdxSpreadSheetODSReaderCustomContainerParser }

constructor TdxSpreadSheetODSReaderCustomContainerParser.Create(ANode: TdxXMLNode;
  AViewParser: TdxSpreadSheetODSReaderTableViewParser; ARowParser: TdxSpreadSheetODSReaderTableRowParser = nil;
  ABaseCell: TdxSpreadSheetCell = nil);
begin
  inherited Create(ANode, AViewParser.Owner);
  FBaseCell := ABaseCell;
  FRowParser := ARowParser;
  FViewParser := AViewParser;
  FContainer := CreateContainer;
  FContainer.BeginUpdate;
end;

destructor TdxSpreadSheetODSReaderCustomContainerParser.Destroy;
begin
  FContainer.EndUpdate;
  inherited Destroy;
end;

function TdxSpreadSheetODSReaderCustomContainerParser.CreateContainer: TdxSpreadSheetContainer;
begin
  Result := ViewParser.View.Containers.Add(TdxSpreadSheetContainer)
end;

procedure TdxSpreadSheetODSReaderCustomContainerParser.Execute;
const
  ColumnHeaderHeight = 18;
var
  AAngle: Double;
  AAttr: TdxXMLNodeAttribute;
  AIndex: Integer;
  AOffset: TPoint;
begin
  Container.AnchorPoint1.Offset := cxPoint(
    TdxSpreadSheetODSHelper.StringToSize(Node.Attributes.GetValueAsString(sdxODSAttrSvgX)),
    TdxSpreadSheetODSHelper.StringToSize(Node.Attributes.GetValueAsString(sdxODSAttrSvgY)));
  Container.AnchorPoint2.Offset := cxPoint(
    TdxSpreadSheetODSHelper.StringToSize(Node.Attributes.GetValueAsString(sdxODSAttrSvgWidth)),
    TdxSpreadSheetODSHelper.StringToSize(Node.Attributes.GetValueAsString(sdxODSAttrSvgHeight)));

  if Node.Attributes.Find(sdxODSAttrDrawTransform, AAttr) then
  begin
    TdxSpreadSheetODSHelper.ParseTransform(AAttr.ValueAsString, AAngle, AOffset);
    Dec(AOffset.Y, ColumnHeaderHeight);
    Container.AnchorPoint1.Offset := cxPointOffset(Container.AnchorPoint1.Offset, AOffset);
    Container.Transform.RotationAngle := AAngle;
  end;

  ReadAnchorPoints;

  if Node.Attributes.Find(sdxODSAttrDrawZIndex, AAttr) then
  begin
    AIndex := AAttr.ValueAsInteger;
    ViewParser.ContainersZOrder.Count := Max(ViewParser.ContainersZOrder.Count, AIndex + 1);
    ViewParser.ContainersZOrder.Items[AIndex] := Container;
  end;
end;

procedure TdxSpreadSheetODSReaderCustomContainerParser.ReadAnchorPoints;
var
  AAttr: TdxXMLNodeAttribute;
begin
  if BaseCell <> nil then
  begin
    Container.AnchorType := catOneCell;
    Container.AnchorPoint1.FixedToCell := True;
    Container.AnchorPoint1.Cell := BaseCell;
    if Node.Attributes.Find(sdxODSAttrTableEndCellAddress, AAttr) then
    begin
      Container.AnchorPoint2.Cell := GetCellByRef(AAttr.ValueAsString);
      if Container.AnchorPoint2.Cell <> nil then
      begin
        Container.AnchorType := catTwoCell;
        Container.AnchorPoint2.FixedToCell := True;
        Container.AnchorPoint2.Offset := cxPoint(
          TdxSpreadSheetODSHelper.StringToSize(Node.Attributes.GetValueAsString(sdxODSAttrTableEndX)),
          TdxSpreadSheetODSHelper.StringToSize(Node.Attributes.GetValueAsString(sdxODSAttrTableEndY)));
      end;
    end;
  end
  else
    Container.AnchorType := catAbsolute;
end;

function TdxSpreadSheetODSReaderCustomContainerParser.GetCellByRef(const ARef: string): TdxSpreadSheetCell;
var
  AColumnIndex: Integer;
  ARowIndex: Integer;
begin
  dxStringToReference(Copy(ARef, Pos('.', ARef) + 1, MaxInt), AColumnIndex, ARowIndex);
  if (ARowIndex >= 0) and (AColumnIndex >= 0) then
    Result := ViewParser.View.CreateCell(ARowIndex, AColumnIndex)
  else
    Result := nil;
end;

{ TdxSpreadSheetODSReaderShapeContainerParser }

procedure TdxSpreadSheetODSReaderShapeContainerParser.Execute;
var
  AStyle: TdxSpreadSheetODSReaderGraphicStyle;
begin
  inherited Execute;
  Container.Shape.ShapeType := TdxSpreadSheetODSHelper.DetectShapeType(Node.Attributes.GetValue(sdxODSAttrDrawName, Node.Name));
  if Owner.GraphicStyles.TryGetValue(Node.Attributes.GetValue(sdxODSAttrDrawStyleName), AStyle) then
  begin
    AStyle.ApplyToBrush(Container.Shape.Brush);
    AStyle.ApplyToPen(Container.Shape.Pen);
    if AStyle.ProtectPosition then
      Container.Restrictions := Container.Restrictions + [crNoMove];
    if AStyle.ProtectSize then
      Container.Restrictions := Container.Restrictions + [crNoResize];
  end;
end;

function TdxSpreadSheetODSReaderShapeContainerParser.CreateContainer: TdxSpreadSheetContainer;
begin
  Result := ViewParser.View.Containers.Add(TdxSpreadSheetShapeContainer);
end;

function TdxSpreadSheetODSReaderShapeContainerParser.GetContainer: TdxSpreadSheetShapeContainer;
begin
  Result := TdxSpreadSheetShapeContainer(inherited Container);
end;

{ TdxSpreadSheetODSReaderCustomTextBoxContainerParser }

procedure TdxSpreadSheetODSReaderCustomTextBoxContainerParser.Execute;
const
  Map: array[TdxSpreadSheetDataAlignHorz] of TAlignment = (
    taLeftJustify, taLeftJustify, taCenter, taRightJustify, taLeftJustify, taLeftJustify, taLeftJustify
  );
var
  AFormattedString: TdxSpreadSheetFormattedSharedString;
  AHelper: TdxSpreadSheetODSReaderRichTextHelper;
  AParagraphStyle: TdxSpreadSheetODSReaderParagraphStyle;
  AStyle: TdxSpreadSheetODSReaderGraphicStyle;
  ATextNode: TdxXMLNode;
begin
  inherited Execute;

  if Node.FindChild(sdxODSNodeText, ATextNode) then
  begin
    Container.BeginUpdate;
    try
      if Owner.GraphicStyles.TryGetValue(Node.Attributes.GetValue(sdxODSAttrDrawStyleName), AStyle) then
        ApplyStyleToTextBox(AStyle);

      if Owner.ParagraphStyles.TryGetValue(ATextNode.Attributes.GetValue(sdxODSAttrTextStyleName), AParagraphStyle) then
        TextBox.AlignHorz := Map[AParagraphStyle.Alignment];

      AHelper := TdxSpreadSheetODSReaderRichTextHelper.Create(RowParser);
      try
        AHelper.Process(ATextNode);
        AFormattedString := Owner.CreateTempFormattedSharedString(AHelper.Text);
        AFormattedString.Runs.Assign(AHelper.Runs);
        if AFormattedString.Runs.Count > 0 then
          TextBox.Font.Handle := AFormattedString.Runs[0].FontHandle;
        TextBox.Text := Owner.AddFormattedSharedString(AFormattedString);
      finally
        AHelper.Free;
      end;
    finally
      Container.EndUpdate;
    end;
  end;
end;

procedure TdxSpreadSheetODSReaderCustomTextBoxContainerParser.ApplyStyleToTextBox(
  AStyle: TdxSpreadSheetODSReaderGraphicStyle);
begin
  TextBox.AlignHorz := AStyle.TextAreaHorzAlign;
  TextBox.AlignVert := AStyle.TextAreaVertAlign;
  TextBox.AutoSize := AStyle.AutoSize;
  TextBox.ContentOffsets := AStyle.Padding;
end;

function TdxSpreadSheetODSReaderCustomTextBoxContainerParser.GetTextBox: TdxSpreadSheetCustomTextBox;
begin
  Result := TdxSpreadSheetCustomTextBoxContainerAccess(Container).TextBox;
end;

{ TdxSpreadSheetODSReaderTextBoxContainerParser }

procedure TdxSpreadSheetODSReaderTextBoxContainerParser.ApplyStyleToTextBox(
  AStyle: TdxSpreadSheetODSReaderGraphicStyle);
begin
  inherited ApplyStyleToTextBox(AStyle);
  TdxSpreadSheetTextBox(TextBox).WordWrap := AStyle.WordWrap;
end;

function TdxSpreadSheetODSReaderTextBoxContainerParser.CreateContainer: TdxSpreadSheetContainer;
begin
  Result := ViewParser.View.Containers.Add(TdxSpreadSheetTextBoxContainer);
end;

{ TdxSpreadSheetODSReaderCommentContainerParser }

function TdxSpreadSheetODSReaderCommentContainerParser.CreateContainer: TdxSpreadSheetContainer;
begin
  Result := ViewParser.View.Containers.Add(TdxSpreadSheetCommentContainer);
end;

procedure TdxSpreadSheetODSReaderCommentContainerParser.Execute;
begin
  inherited Execute;
  TdxSpreadSheetCommentContainer(Container).Cell := BaseCell;
  Container.Visible := Node.Attributes.GetValueAsBoolean(sdxODSAttrOfficeDisplay);
end;

procedure TdxSpreadSheetODSReaderCommentContainerParser.ReadAnchorPoints;
begin
  Container.AnchorType := catAbsolute;
end;

{ TdxSpreadSheetODSReaderPictureContainerParser }

procedure TdxSpreadSheetODSReaderPictureContainerParser.Execute;
var
  AStream: TMemoryStream;
  ASubNode: TdxXMLNode;
begin
  inherited Execute;
  if Node.FindChild(sdxODSNodeDrawImage, ASubNode) then
  begin
    try
      AStream := Owner.ReadFile(ASubNode.Attributes.GetValue(sdxODSAttrXLinkHRef));
      try
        TdxSpreadSheetPictureAccess(Container.Picture).ImageHandle := Owner.AddImage(AStream);
      finally
        AStream.Free;
      end;
    except
      // Unsupported Image Format
    end;
  end;
end;

function TdxSpreadSheetODSReaderPictureContainerParser.CreateContainer: TdxSpreadSheetContainer;
begin
  Result := ViewParser.View.Containers.Add(TdxSpreadSheetPictureContainer);
end;

function TdxSpreadSheetODSReaderPictureContainerParser.GetContainer: TdxSpreadSheetPictureContainer;
begin
  Result := TdxSpreadSheetPictureContainer(inherited Container);
end;

{ TdxSpreadSheetODSReaderDataFormatConverter }

constructor TdxSpreadSheetODSReaderDataFormatConverter.Create;
begin
  inherited Create;
  FProcs := TDictionary<TdxXMLString, TdxSpreadSheetODSReaderDataFormatConverterProc>.Create;
  FProcs.Add(sdxODSAttrNumberCurrencySymbol, ConvertText);
  FProcs.Add(sdxODSAttrNumberHours, ConvertHours);
  FProcs.Add(sdxODSAttrNumberMinutes, ConvertMinutes);
  FProcs.Add(sdxODSAttrNumberSeconds, ConvertSeconds);
  FProcs.Add(sdxODSNodeNumberDay, ConvertDay);
  FProcs.Add(sdxODSNodeNumberDayOfWeek, ConvertDayOfWeek);
  FProcs.Add(sdxODSNodeNumberMonth, ConvertMonth);
  FProcs.Add(sdxODSNodeNumberNumber, ConvertNumber);
  FProcs.Add(sdxODSNodeNumberText, ConvertText);
  FProcs.Add(sdxODSNodeNumberYear, ConvertYear);
  FProcs.Add(sdxODSNodeStyleTextProperties, ConvertTextColor);
end;

destructor TdxSpreadSheetODSReaderDataFormatConverter.Destroy;
begin
  FreeAndNil(FProcs);
  inherited Destroy;
end;

function TdxSpreadSheetODSReaderDataFormatConverter.Convert(ANode: TdxXMLNode): string;
var
  AProc: TdxSpreadSheetODSReaderDataFormatConverterProc;
  ASubNode: TdxXMLNode;
begin
  Result := '';
  ASubNode := ANode.First;
  while ASubNode <> nil do
  begin
    if FProcs.TryGetValue(ASubNode.Name, AProc) then
      Result := Result + AProc(ASubNode);
    ASubNode := ASubNode.Next;
  end;
end;

class function TdxSpreadSheetODSReaderDataFormatConverter.ConvertDay(ANode: TdxXMLNode): string;
begin
  if IsLongStyle(ANode) then
    Result := 'dd'
  else
    Result := 'd';
end;

class function TdxSpreadSheetODSReaderDataFormatConverter.ConvertDayOfWeek(ANode: TdxXMLNode): string;
begin
  if IsLongStyle(ANode) then
    Result := 'dddd'
  else
    Result := 'ddd';
end;

class function TdxSpreadSheetODSReaderDataFormatConverter.ConvertHours(ANode: TdxXMLNode): string;
begin
  if IsLongStyle(ANode) then
    Result := 'hh'
  else
    Result := 'h';
end;

class function TdxSpreadSheetODSReaderDataFormatConverter.ConvertMinutes(ANode: TdxXMLNode): string;
begin
  if IsLongStyle(ANode) then
    Result := 'mm'
  else
    Result := 'm';
end;

class function TdxSpreadSheetODSReaderDataFormatConverter.ConvertMonth(ANode: TdxXMLNode): string;
var
  AIsTextual: Boolean;
begin
  AIsTextual := ANode.Attributes.GetValueAsBoolean(sdxODSAttrNumberTextual);
  if not AIsTextual then
    Result := 'mm'
  else
    if IsLongStyle(ANode) then
      Result := 'mmmm'
    else
      Result := 'mmm';
end;

class function TdxSpreadSheetODSReaderDataFormatConverter.ConvertNumber(ANode: TdxXMLNode): string;
var
  ADecimalPlaces: Integer;
  AMinIntegerDigits: Integer;
begin
  ADecimalPlaces := ANode.Attributes.GetValueAsInteger(sdxODSAttrNumberDecimalPlaces);
  AMinIntegerDigits := ANode.Attributes.GetValueAsInteger(sdxODSAttrNumberMinIntegerDigits);

  if ADecimalPlaces > 0 then
    Result := DupeString('0', AMinIntegerDigits) + '.' + DupeString('0', ADecimalPlaces)
  else
    Result := DupeString('0', AMinIntegerDigits);
end;

class function TdxSpreadSheetODSReaderDataFormatConverter.ConvertSeconds(ANode: TdxXMLNode): string;
begin
  if IsLongStyle(ANode) then
    Result := 'ss'
  else
    Result := 's';
end;

class function TdxSpreadSheetODSReaderDataFormatConverter.ConvertText(ANode: TdxXMLNode): string;
begin
  Result := ANode.TextAsString;
end;

class function TdxSpreadSheetODSReaderDataFormatConverter.ConvertTextColor(ANode: TdxXMLNode): string;
var
  AColorIndex: Integer;
begin
  AColorIndex := dxSpreadSheetGetStandardColorIndex(
    TdxSpreadSheetODSHelper.StringToColor(ANode.Attributes.GetValueAsString(sdxODSAttrFOColor)));
  if AColorIndex > 0 then
    Result := '[Color' + IntToStr(AColorIndex + 1) + ']'
  else
    Result := '';
end;

class function TdxSpreadSheetODSReaderDataFormatConverter.ConvertYear(ANode: TdxXMLNode): string;
begin
  if IsLongStyle(ANode) then
    Result := 'yyyy'
  else
    Result := 'yy'
end;

class function TdxSpreadSheetODSReaderDataFormatConverter.IsLongStyle(ANode: TdxXMLNode): Boolean;
begin
  Result := ANode.Attributes.GetValue(sdxODSAttrNumberStyle) = sdxODSValueNumberStyleLong;
end;

{ TdxSpreadSheetODSReaderDefinedNamesParser }

constructor TdxSpreadSheetODSReaderDefinedNamesParser.Create(
  ANode: TdxXMLNode; AOwner: TdxSpreadSheetODSReader; AView: TdxSpreadSheetTableView = nil);
begin
  inherited Create(ANode, AOwner);
  FView := AView;
end;

procedure TdxSpreadSheetODSReaderDefinedNamesParser.Execute;
begin
  Node.ForEach(ProcessSubNodes);
end;

procedure TdxSpreadSheetODSReaderDefinedNamesParser.ParseNamedExpression(ANode: TdxXMLNode);
begin
end;

procedure TdxSpreadSheetODSReaderDefinedNamesParser.ParseNamedRange(ANode: TdxXMLNode; const AValueName: TdxXMLString);
var
  AValue: string;
begin
  AValue := ANode.Attributes.GetValueAsString(AValueName);
  if AValue <> '' then
    AValue := TdxSpreadSheetODSFormulaParser.Tag + '[' + AValue + ']';
  SpreadSheet.DefinedNames.Add(ANode.Attributes.GetValueAsString(sdxODSAttrTableName),
    TdxSpreadSheetODSFormula.Convert(SpreadSheet, AValue), FView);
end;

procedure TdxSpreadSheetODSReaderDefinedNamesParser.ProcessSubNodes(ANode: TdxXMLNode; AUserData: Pointer);
begin
  if ANode.Name = sdxODSNodeTableNamedRange then
    ParseNamedRange(ANode, sdxODSAttrTableCellRangeAddress)
  else

  if ANode.Name = sdxODSNodeTableNamedExpression then
    ParseNamedExpression(ANode)
  else

  if ANode.Name = sdxODSNodeTableDataBaseRange then
    ParseNamedRange(ANode, sdxODSAttrTableTargetRangeAddress);
end;

{ TdxSpreadSheetODSReaderMasterStylesParser }

procedure TdxSpreadSheetODSReaderMasterStylesParser.Execute;
begin
  Node.ForEach(ProcessSubNodes);
end;

procedure TdxSpreadSheetODSReaderMasterStylesParser.ProcessSubNodes(ANode: TdxXMLNode; AUserData: Pointer);
begin
  if ANode.Name = sdxODSNodeStyleMasterPage then
    Owner.MasterPageStyles.Add(ANode);
end;

{ TdxSpreadSheetODSReaderNumberFormatParser }

procedure TdxSpreadSheetODSReaderNumberFormatParser.Execute;
var
  AFormatCode: string;
begin
  AFormatCode := Owner.DataFormatConverter.Convert(Node);
  if AFormatCode <> '' then
  begin
    CheckStyleMapping(AFormatCode);
    Owner.DataFormats.Add(Node.Attributes.GetValue(sdxODSAttrStyleName), AFormatCode);
  end;
end;

procedure TdxSpreadSheetODSReaderNumberFormatParser.CheckStyleMapping(var AFormatCode: string);
var
  ASubStyles: TDictionary<TdxXMLString, string>;
  AValue1: string;
  AValue2: string;
begin
  ASubStyles := PopulateSubStyles;
  try
    if ASubStyles.Count > 0 then
    begin
      if ASubStyles.TryGetValue(sdxODSValueConditionAboveOrEqual, AValue1) then
        AFormatCode := AValue1 + ';' + AFormatCode
      else

      if ASubStyles.TryGetValue(sdxODSValueConditionBelowOrEqual, AValue1) then
        AFormatCode := AFormatCode + ';' + AFormatCode
      else

      if ASubStyles.TryGetValue(sdxODSValueConditionAbove, AValue1) and
         ASubStyles.TryGetValue(sdxODSValueConditionBelow, AValue2)
      then
        AFormatCode := AValue1 + ';' + AValue2 + ';' + AFormatCode
      else

      if ASubStyles.TryGetValue(sdxODSValueConditionAbove, AValue1) and
         ASubStyles.TryGetValue(sdxODSValueConditionEqual, AValue2)
      then
        AFormatCode := AValue1 + ';' + AFormatCode + ';' + AValue2
      else

      if ASubStyles.TryGetValue(sdxODSValueConditionBelow, AValue1) and
         ASubStyles.TryGetValue(sdxODSValueConditionEqual, AValue2)
      then
        AFormatCode := AFormatCode + ';' + AValue1 + ';' + AValue2;
    end;
  finally
    ASubStyles.Free;
  end;
end;

function TdxSpreadSheetODSReaderNumberFormatParser.PopulateSubStyles: TDictionary<TdxXMLString, string>;
var
  ASubNode: TdxXMLNode;
begin
  Result := TDictionary<TdxXMLString, string>.Create;
  ASubNode := Node.First;
  while ASubNode <> nil do
  begin
    if ASubNode.Name = sdxODSNodeStyleMap then
    begin
      Result.Add(LowerCase(ASubNode.Attributes.GetValue(sdxODSAttrStyleCondition)),
        Owner.DataFormats.Items[ASubNode.Attributes.GetValue(sdxODSAttrStyleApplyStyleName)]);
    end;
    ASubNode := ASubNode.Next;
  end;
end;

{ TdxSpreadSheetODSReaderRichTextHelper }

constructor TdxSpreadSheetODSReaderRichTextHelper.Create(AOwner: TdxSpreadSheetODSReaderTableRowParser);
begin
  inherited Create;
  Owner := AOwner;
  Runs := TdxSpreadSheetFormattedSharedStringRuns.Create;
end;

destructor TdxSpreadSheetODSReaderRichTextHelper.Destroy;
begin
  FreeAndNil(Runs);
  inherited Destroy;
end;

procedure TdxSpreadSheetODSReaderRichTextHelper.Process(ANode: TdxXMLNode; AFont: TdxSpreadSheetFontHandle = nil);
var
  ANodeNext: TdxXMLNode;
begin
  repeat
    AddPart(ANode.TextAsString, AFont);
    ANode.ForEach(ProcessSubNode);
    ANodeNext := ANode.Next;
    if (ANodeNext = nil) or (ANodeNext.Name <> ANode.Name) then
      Break;
    AddPart(#13#10, AFont);
    ANode := ANodeNext;
  until False;
end;

procedure TdxSpreadSheetODSReaderRichTextHelper.AddPart(const AText: string; AFont: TdxSpreadSheetFontHandle = nil);
begin
  if AText <> '' then
  begin
    if AFont = nil then
      AFont := GetDefaultFontHandle;
    Runs.Add(Length(Text) + 1, AFont);
    Text := Text + AText;
  end;
end;

procedure TdxSpreadSheetODSReaderRichTextHelper.ProcessSubNode(ANode: TdxXMLNode; AUserData: Pointer);
var
  ADefaultFontHandle: TdxSpreadSheetFontHandle;
  AStyleHandle: TdxSpreadSheetFontHandle;
begin
  ADefaultFontHandle := TdxSpreadSheetFontHandle(AUserData);
  if ADefaultFontHandle = nil then
    ADefaultFontHandle := GetDefaultFontHandle;

  if ANode.Name = sdxODSNodeTextSpace then
    AddPart(' ', ADefaultFontHandle)
  else

  if ANode.Name = sdxODSNodeTextLineBreak then
    AddPart(#13#10, ADefaultFontHandle)
  else

  if ANode.Name = sdxODSNodeTextTab then
    AddPart(#9, ADefaultFontHandle)
  else

  if ANode.Name = sdxODSNodeTextXLink then
  begin
    AddPart(ANode.TextAsString, ADefaultFontHandle);
    HyperlinkValue := dxXMLStringToString(ANode.Attributes.GetValue(sdxODSAttrXLinkHRef));
  end
  else
  begin
    AStyleHandle := GetFontHandle(ANode);
    if AStyleHandle = nil then
      AStyleHandle := ADefaultFontHandle;
    if ANode.Name = sdxODSNodeTextSpan then
      Process(ANode, AStyleHandle)
    else
      AddPart(ANode.TextAsString, AStyleHandle);
  end;
end;

function TdxSpreadSheetODSReaderRichTextHelper.GetDefaultFontHandle: TdxSpreadSheetFontHandle;
begin
  if Owner <> nil then
    Result := Owner.Owner.CellStyles.DefaultStyle.Font
  else
    Result := nil;
end;

function TdxSpreadSheetODSReaderRichTextHelper.GetFontHandle(ANode: TdxXMLNode): TdxSpreadSheetFontHandle;
begin
  if Owner <> nil then
    Result := Owner.GetCellStyle(ANode, sdxODSAttrTextStyleName, True).Font
  else
    Result := nil;
end;

{ TdxSpreadSheetODSReaderSettingsFileParser }

destructor TdxSpreadSheetODSReaderSettingsFileParser.Destroy;
begin
  FreeAndNil(FProcs);
  inherited Destroy;
end;

procedure TdxSpreadSheetODSReaderSettingsFileParser.AfterConstruction;
begin
  inherited AfterConstruction;
  FProcs := TDictionary<TdxXMLString, TdxSpreadSheetODSReaderSettingsParserProc>.Create;

  FProcs.Add(sdxODSValueConfigShowGrid, BothProcessShowGrid);
  FProcs.Add(sdxODSValueConfigZoomValue, BothProcessZoomValue);

  FProcs.Add(sdxODSValueConfigActiveTable, CommonProcessActiveTable);
  FProcs.Add(sdxODSValueConfigShowZeroValues, CommonProcessShowZeroValues);
  FProcs.Add(sdxODSValueConfigHasColumnRowHeaders, CommonProcessHasColumnRowsHeaders);

  FProcs.Add(sdxODSValueConfigCursorPositionX, ViewProcessCursorX);
  FProcs.Add(sdxODSValueConfigCursorPositionY, ViewProcessCursorY);
  FProcs.Add(sdxODSValueConfigHorizontalSplitPosition, ViewProcessHorzSplitPosition);
  FProcs.Add(sdxODSValueConfigVerticalSplitPosition, ViewProcessVertSplitPosition);
  FProcs.Add(sdxODSValueConfigPositionBottom, ViewProcessPositionBottom);
  FProcs.Add(sdxODSValueConfigPositionLeft, ViewProcessPositionLeft);
  FProcs.Add(sdxODSValueConfigPositionRight, ViewProcessPositionRight);
end;

procedure TdxSpreadSheetODSReaderSettingsFileParser.Execute;
begin
  if Node.HasChildren then
    Node.First.ForEach(ProcessConfigSet);
end;

procedure TdxSpreadSheetODSReaderSettingsFileParser.ProcessConfigItem(ANode: TdxXMLNode; AUserData: Pointer);
var
  AProc: TdxSpreadSheetODSReaderSettingsParserProc;
begin
  if FProcs.TryGetValue(ANode.Attributes.GetValue(sdxODSAttrConfigName), AProc) then
    AProc(ANode.TextAsString);
end;

procedure TdxSpreadSheetODSReaderSettingsFileParser.ProcessConfigSet(ANode: TdxXMLNode; AUserData: Pointer);
begin
  if ANode.Name = sdxODSNodeConfigConfigItemSet then
  begin
    if ANode.Attributes.GetValue(sdxODSAttrConfigName) = sdxODSValueConfigViewSettings then
      ANode.ForEach(ProcessConfigSetViewSettings, AUserData);
  end;
end;

procedure TdxSpreadSheetODSReaderSettingsFileParser.ProcessConfigSetTables(ANode: TdxXMLNode; AUserData: Pointer);
begin
  FCurrentView := SpreadSheet.GetSheetByName(ANode.Attributes.GetValueAsString(sdxODSAttrConfigName)) as TdxSpreadSheetTableView;
  if FCurrentView <> nil then
    ANode.ForEach(ProcessConfigItem);
end;

procedure TdxSpreadSheetODSReaderSettingsFileParser.ProcessConfigSetViews(ANode: TdxXMLNode; AUserData: Pointer);
begin
  if ANode.Name = sdxODSNodeConfigConfigItem then
    ProcessConfigItem(ANode, nil)
  else
    if ANode.Name = sdxODSNodeConfigConfigItemMapNamed then
    begin
      if ANode.Attributes.GetValue(sdxODSAttrConfigName) = sdxODSValueConfigTables then
        ANode.ForEach(ProcessConfigSetTables);
    end;
end;

procedure TdxSpreadSheetODSReaderSettingsFileParser.ProcessConfigSetViewSettings(ANode: TdxXMLNode; AUserData: Pointer);
begin
  if ANode.Name = sdxODSNodeConfigConfigItemMapIndexed then
  begin
    if ANode.Attributes.GetValue(sdxODSAttrConfigName) = sdxODSValueConfigViews then
    begin
      if ANode.HasChildren then
        ANode.First.ForEach(ProcessConfigSetViews);
    end;
  end;
end;

procedure TdxSpreadSheetODSReaderSettingsFileParser.BothProcessShowGrid(const AValue: string);
begin
  if FCurrentView <> nil then
    FCurrentView.Options.GridLines := dxBooleanToDefaultBoolean(TdxXMLHelper.DecodeBoolean(AValue))
  else
    SpreadSheet.OptionsView.GridLines := TdxXMLHelper.DecodeBoolean(AValue);
end;

procedure TdxSpreadSheetODSReaderSettingsFileParser.BothProcessZoomValue(const AValue: string);
begin
  if FCurrentView <> nil then
    FCurrentView.Options.ZoomFactor := StrToIntDef(AValue, 100)
end;

procedure TdxSpreadSheetODSReaderSettingsFileParser.CommonProcessActiveTable(const AValue: string);
begin
  SpreadSheet.ActiveSheet := SpreadSheet.GetSheetByName(AValue);
end;

procedure TdxSpreadSheetODSReaderSettingsFileParser.CommonProcessHasColumnRowsHeaders(const AValue: string);
begin
  SpreadSheet.OptionsView.Headers := TdxXMLHelper.DecodeBoolean(AValue);
end;

procedure TdxSpreadSheetODSReaderSettingsFileParser.CommonProcessShowZeroValues(const AValue: string);
begin
  SpreadSheet.OptionsView.ZeroValues := TdxXMLHelper.DecodeBoolean(AValue);
end;

procedure TdxSpreadSheetODSReaderSettingsFileParser.ViewProcessCursorX(const AValue: string);
begin
  if FCurrentView <> nil then
    FCurrentView.Selection.FocusedColumn := StrToIntDef(AValue, 0);
end;

procedure TdxSpreadSheetODSReaderSettingsFileParser.ViewProcessCursorY(const AValue: string);
begin
  if FCurrentView <> nil then
    FCurrentView.Selection.FocusedRow := StrToIntDef(AValue, 0);
end;

procedure TdxSpreadSheetODSReaderSettingsFileParser.ViewProcessPositionBottom(const AValue: string);
begin
  if FCurrentView <> nil then
    TdxSpreadSheetTableViewAccess(FCurrentView).ViewInfo.FirstScrollableRow := StrToIntDef(AValue, 0);
end;

procedure TdxSpreadSheetODSReaderSettingsFileParser.ViewProcessPositionLeft(const AValue: string);
begin
  if (FCurrentView <> nil) and (FCurrentView.FrozenColumn < 0) then
    TdxSpreadSheetTableViewAccess(FCurrentView).ViewInfo.FirstScrollableColumn := StrToIntDef(AValue, 0);
end;

procedure TdxSpreadSheetODSReaderSettingsFileParser.ViewProcessPositionRight(const AValue: string);
begin
  if (FCurrentView <> nil) and (FCurrentView.FrozenColumn >= 0) then
    TdxSpreadSheetTableViewAccess(FCurrentView).ViewInfo.FirstScrollableColumn := StrToIntDef(AValue, 0);
end;

procedure TdxSpreadSheetODSReaderSettingsFileParser.ViewProcessHorzSplitPosition(const AValue: string);
begin
  if FCurrentView <> nil then
    FCurrentView.FrozenColumn := StrToIntDef(AValue, 0) - 1;
end;

procedure TdxSpreadSheetODSReaderSettingsFileParser.ViewProcessVertSplitPosition(const AValue: string);
begin
  if FCurrentView <> nil then
    FCurrentView.FrozenRow := StrToIntDef(AValue, 0) - 1;
end;

{ TdxSpreadSheetODSReaderStyleParser }

procedure TdxSpreadSheetODSReaderStyleParser.Execute;
var
  AStyleHandle: TdxSpreadSheetCellStyleHandle;
  ASubNode: TdxXMLNode;
begin
  if Node.FindChild(sdxODSNodeStyleTableColumnProperties, ASubNode) then
    Owner.ColumnStyles.Add(ASubNode);
  if Node.FindChild(sdxODSNodeStyleTableRowProperties, ASubNode) then
    Owner.RowStyles.Add(ASubNode);
  if Node.FindChild(sdxODSNodeStyleTableProperties, ASubNode) then
    Owner.TableStyles.Add(ASubNode);
  if Node.FindChild(sdxODSNodeStyleGraphicProperties, ASubNode) then
    Owner.GraphicStyles.Add(ASubNode);
  if Node.FindChild(sdxODSNodeStyleParagraph, ASubNode) then
    Owner.ParagraphStyles.Add(ASubNode);
  if ParseStyle(AStyleHandle) then
    Owner.Styles.Add(
      Node.Attributes.GetValueAsString(sdxODSAttrStyleName),
      Node.Attributes.GetValueAsString(sdxODSAttrStyleDisplayName), AStyleHandle);
end;

procedure TdxSpreadSheetODSReaderStyleParser.ParseBackgroundProperties(AHandle: TdxSpreadSheetCellStyleHandle; ANode: TdxXMLNode);
var
  AAttr: TdxXMLNodeAttribute;
begin
  ParseBrush(AHandle, ANode);
  ParseBorders(AHandle, ANode);
  AHandle.States := [];
  if ANode.Attributes.Find(sdxODSAttrStyleVerticalAlign, AAttr) then
    AHandle.AlignVert := TdxSpreadSheetODSHelper.StringToAlignVert(AAttr.Value);
  if ANode.Attributes.Find(sdxODSAttrStyleShrinkToFit, AAttr) and AAttr.ValueAsBoolean then
    AHandle.States := AHandle.States + [csShrinkToFit];
  if ANode.Attributes.Find(sdxODSAttrFOWrapOption, AAttr) then
    AHandle.States := AHandle.States + [csWordWrap];
  if ANode.Attributes.Find(sdxODSAttrStyleCellProtect, AAttr) then
  begin
    if AAttr.Value = sdxODSValueCellProtectionHiddenAndProtected then
      AHandle.States := AHandle.States + [csLocked, csHidden];
    if AAttr.Value = sdxODSValueCellProtectionProtected then
      AHandle.States := AHandle.States + [csLocked];
  end;
end;

procedure TdxSpreadSheetODSReaderStyleParser.ParseBorder(AHandle: TdxSpreadSheetBordersHandle; ASide: TcxBorder; const S: string);
var
  AList: TStringList;
begin
  AList := TStringList.Create;
  try
    AList.LineBreak := ' ';
    AList.Text := S;

    if AList.Count > 0 then
    begin
      if AList.Count > 1 then
        AHandle.BorderStyle[ASide] := TdxSpreadSheetODSHelper.StringToBorderStyle(AList[0], AList[1])
      else
        AHandle.BorderStyle[ASide] := TdxSpreadSheetODSHelper.StringToBorderStyle(AList[0], '');

      if AList.Count > 2 then
        AHandle.BorderColor[ASide] := TdxSpreadSheetODSHelper.StringToColor(AList[2]);
    end;
  finally
    AList.Free;
  end;
end;

procedure TdxSpreadSheetODSReaderStyleParser.ParseBorders(AHandle: TdxSpreadSheetCellStyleHandle; ANode: TdxXMLNode);
var
  AAttr: TdxXMLNodeAttribute;
  ABordersHandle: TdxSpreadSheetBordersHandle;
  ASide: TcxBorder;
begin
  ABordersHandle := Owner.CreateTempBordersHandle;
  if ANode.Attributes.Find(sdxODSAttrFOBorder, AAttr) then
  begin
    ParseBorder(ABordersHandle, bLeft, AAttr.ValueAsString);
    for ASide := Low(ASide) to High(ASide) do
    begin
      ABordersHandle.BorderColor[ASide] := ABordersHandle.BorderColor[bLeft];
      ABordersHandle.BorderStyle[ASide] := ABordersHandle.BorderStyle[bLeft];
    end;
  end;

  for ASide := Low(ASide) to High(ASide) do
  begin
    if ANode.Attributes.Find(sdxODSAttrFOBorders[ASide], AAttr) then
      ParseBorder(ABordersHandle, ASide, AAttr.ValueAsString);
  end;
  AHandle.Borders := Owner.AddBorders(ABordersHandle);
end;

procedure TdxSpreadSheetODSReaderStyleParser.ParseBrush(AHandle: TdxSpreadSheetCellStyleHandle; ANode: TdxXMLNode);
var
  AAttr: TdxXMLNodeAttribute;
  ABrushHandle: TdxSpreadSheetBrushHandle;
begin
  if ANode.Attributes.Find(sdxODSAttrFOBackgroundColor, AAttr) then
  begin
    ABrushHandle := Owner.CreateTempBrushHandle;
    ABrushHandle.BackgroundColor := TdxSpreadSheetODSHelper.StringToColor(AAttr.ValueAsString);
    AHandle.Brush := Owner.AddBrush(ABrushHandle);
  end;
end;

procedure TdxSpreadSheetODSReaderStyleParser.ParseFontProperties(AHandle: TdxSpreadSheetCellStyleHandle; ANode: TdxXMLNode);
var
  AAttr: TdxXMLNodeAttribute;
  AFontHandle: TdxSpreadSheetFontHandle;
begin
  AFontHandle := Owner.CreateTempFontHandle;
  if ANode.Attributes.Find(sdxODSAttrFOColor, AAttr) then
    AFontHandle.Color := TdxSpreadSheetODSHelper.StringToColor(AAttr.ValueAsString);
  if ANode.Attributes.Find(sdxODSAttrFOFontSize, AAttr) then
    AFontHandle.Size := TdxSpreadSheetODSHelper.StringToFontSize(AAttr.ValueAsString);
  if ANode.Attributes.Find(sdxODSAttrFOFontFamily, AAttr) then
    AFontHandle.Name := AAttr.ValueAsString;
  if ANode.Attributes.Find(sdxODSAttrStyleFontPitch, AAttr) then
    AFontHandle.Pitch := TdxSpreadSheetODSHelper.StringToFontPitch(AAttr.Value);

  if ANode.Attributes.Find(sdxODSAttrFOFontStyle, AAttr) and (AAttr.Value = sdxODSValueFontStyleItalic) then
    Include(AFontHandle.Style, fsItalic);
  if ANode.Attributes.Find(sdxODSAttrStyleTextUnderlineStyle, AAttr) and (AAttr.Value <> sdxODSValueBorderStyleNone) then
    Include(AFontHandle.Style, fsUnderline);
  if ANode.Attributes.Find(sdxODSAttrStyleTextLineThroughStyle, AAttr) and (AAttr.Value <> sdxODSValueBorderStyleNone) then
    Include(AFontHandle.Style, fsStrikeOut);
  if ANode.Attributes.Find(sdxODSAttrFOFontWeight, AAttr) and TdxSpreadSheetODSHelper.IsBoldFontStyle(AAttr.Value) then
    Include(AFontHandle.Style, fsBold);
  AHandle.Font := Owner.AddFont(AFontHandle);
end;

procedure TdxSpreadSheetODSReaderStyleParser.ParseParagraphProperties(AHandle: TdxSpreadSheetCellStyleHandle; ANode: TdxXMLNode);
begin
  AHandle.AlignHorz := TdxSpreadSheetODSHelper.StringToAlignHorz(ANode.Attributes.GetValue(sdxODSAttrFOTextAlign));
  AHandle.AlignHorzIndent := TdxSpreadSheetODSHelper.StringToSize(ANode.Attributes.GetValueAsString(sdxODSAttrFOMarginLeft));
end;

function TdxSpreadSheetODSReaderStyleParser.ParseStyle(out AStyleHandle: TdxSpreadSheetCellStyleHandle): Boolean;

  function CheckStyleHandle(var AHandle: TdxSpreadSheetCellStyleHandle): TdxSpreadSheetCellStyleHandle;
  begin
    if AHandle = nil then
      AHandle := Owner.CreateTempCellStyle(nil, nil, nil, nil);
    Result := AHandle;
  end;

var
  AAttr: TdxXMLNodeAttribute;
  ADataFormat: string;
  AParentStyle: TdxSpreadSheetCellStyleHandle;
  ASubNode: TdxXMLNode;
begin
  AStyleHandle := nil;
  if Node.Attributes.Find(sdxODSAttrStyleParentStyleName, AAttr) then
  begin
    if Owner.Styles.TryGetValue(AAttr.ValueAsString, AParentStyle) then
      CheckStyleHandle(AStyleHandle).Assign(AParentStyle);
  end;
  if Node.Attributes.Find(sdxODSAttrStyleDataStyleName, AAttr) then
  begin
    if Owner.DataFormats.TryGetValue(AAttr.Value, ADataFormat) then
      CheckStyleHandle(AStyleHandle).DataFormat := Owner.AddNumberFormat(ADataFormat);
  end;
  if Node.FindChild(sdxODSNodeStyleTableCellProperties, ASubNode) then
    ParseBackgroundProperties(CheckStyleHandle(AStyleHandle), ASubNode);
  if Node.FindChild(sdxODSNodeStyleTextProperties, ASubNode) then
    ParseFontProperties(CheckStyleHandle(AStyleHandle), ASubNode);
  if Node.FindChild(sdxODSNodeStyleParagraph, ASubNode) then
    ParseParagraphProperties(CheckStyleHandle(AStyleHandle), ASubNode);

  Result := AStyleHandle <> nil;
  if Result then
    AStyleHandle := Owner.AddCellStyle(AStyleHandle);
end;

{ TdxSpreadSheetODSReaderStylesParser }

procedure TdxSpreadSheetODSReaderStylesParser.Execute;
begin
  Owner.ProgressHelper.BeginStage(Node.Count);
  Node.ForEach(ProcessSubNode);
  Owner.ProgressHelper.EndStage;
end;

procedure TdxSpreadSheetODSReaderStylesParser.ProcessSubNode(ANode: TdxXMLNode; AUserData: Pointer);
begin
  if ANode.Name = sdxODSNodeStyle then
    ExecuteSubTask(TdxSpreadSheetODSReaderStyleParser.Create(ANode, Owner))
  else

  if ANode.NameScope = sdxODSNodeNumberNameScope then
    ExecuteSubTask(TdxSpreadSheetODSReaderNumberFormatParser.Create(ANode, Owner))
  else

  if ANode.Name = sdxODSNodeDrawGradient then
    Owner.GradientStyles.Add(ANode)
  else

  if ANode.Name = sdxODSNodeDrawFillImage then
    Owner.ImageStyles.Add(ANode)
  else

  if ANode.Name = sdxODSNodeDrawHatch then
    Owner.HatchStyles.Add(ANode)
  else

  if ANode.Name = sdxODSNodeDrawStrokeDash then
    Owner.StrokeDashStyles.Add(ANode)
  else

  if ANode.Name = sdxODSNodeStylePageLayout then
    Owner.PageLayouts.Add(ANode);

  Owner.ProgressHelper.NextTask;
end;

{ TdxSpreadSheetODSReaderStylesFileParser }

procedure TdxSpreadSheetODSReaderStylesFileParser.Execute;
var
  ASubNode: TdxXMLNode;
begin
  if Node.FindChild(sdxODSNodeOfficeStyles, ASubNode) then
    ExecuteSubTask(TdxSpreadSheetODSReaderStylesParser.Create(ASubNode, Owner))
  else
    Owner.ProgressHelper.SkipStage;

  if Node.FindChild(sdxODSNodeOfficeAutomaticStyles, ASubNode) then
    ExecuteSubTask(TdxSpreadSheetODSReaderStylesParser.Create(ASubNode, Owner))
  else
    Owner.ProgressHelper.SkipStage;

  if Node.FindChild(sdxODSNodeOfficeMasterStyles, ASubNode) then
    ExecuteSubTask(TdxSpreadSheetODSReaderMasterStylesParser.Create(ASubNode, Owner));
end;

{ TdxSpreadSheetODSReaderTableViewParser }

constructor TdxSpreadSheetODSReaderTableViewParser.Create(ANode: TdxXMLNode; AOwner: TdxSpreadSheetODSReader);
begin
  inherited Create(ANode, AOwner);
  FView := Owner.AddTableView(Node.Attributes.GetValueAsString(sdxODSAttrTableName));
  FContainersZOrder := TList<TdxSpreadSheetContainer>.Create;
end;

destructor TdxSpreadSheetODSReaderTableViewParser.Destroy;
begin
  FreeAndNil(FContainersZOrder);
  inherited Destroy;
end;

procedure TdxSpreadSheetODSReaderTableViewParser.Execute;
var
  AStyle: TdxSpreadSheetODSReaderTableStyle;
  ASubNode: TdxXMLNode;
begin
  View.Options.Protected := Node.Attributes.GetValueAsBoolean(sdxODSAttrTableProtected);
  if Owner.TableStyles.TryGetValue(Node.Attributes.GetValue(sdxODSAttrTableStyleName), AStyle) then
    AStyle.ApplyTo(View);
  if Node.FindChild(sdxODSNodeTableShapes, ASubNode) then
    ASubNode.ForEach(ProcessShapes);
  if Node.FindChild(sdxODSNodeTableNamedExpressions, ASubNode) then
    ExecuteSubTask(TdxSpreadSheetODSReaderDefinedNamesParser.Create(ASubNode, Owner, FView));
  Node.ForEach(ProcessSubNode);
  AssignContainersZOrder;
  AssignPrintRange;
end;

procedure TdxSpreadSheetODSReaderTableViewParser.AssignContainersZOrder;
var
  AIndex: Integer;
  I: Integer;
begin
  AIndex := 0;
  for I := 0 to ContainersZOrder.Count - 1 do
    if ContainersZOrder.Items[I] <> nil then
    begin
      ContainersZOrder[I].Index := AIndex;
      Inc(AIndex);
    end;
end;

procedure TdxSpreadSheetODSReaderTableViewParser.AssignPrintRange;
var
  AAttr: TdxXMLNodeAttribute;
  ADefinedName: TdxSpreadSheetDefinedName;
  I: Integer;
begin
  for I := SpreadSheet.DefinedNames.Count - 1 downto 0 do
  begin
    ADefinedName := SpreadSheet.DefinedNames[I];
    if ADefinedName.Scope = View then
    begin
      if (ADefinedName.Caption = sdxODSValuePrintArea) or (ADefinedName.Caption = sdxODSValuePrintArea2) then
        TdxSpreadSheetPrintAreasHelper.ImportPrintArea(ADefinedName);
    end;
  end;

  if Node.Attributes.Find(sdxODSAttrTablePrintRanges, AAttr) and (AAttr.Value <> '') then
  begin
    TdxSpreadSheetPrintAreasHelper.ImportPrintArea(SpreadSheet.DefinedNames.Add(sdxODSValuePrintArea,
      TdxSpreadSheetODSFormula.Convert(SpreadSheet, TdxSpreadSheetODSFormulaParser.Tag + '[' + AAttr.ValueAsString + ']'), View));
  end;
end;

procedure TdxSpreadSheetODSReaderTableViewParser.ProcessColumnGroup(ANode: TdxXMLNode);
var
  AGroup: TdxSpreadSheetTableItemGroupAccess;
begin
  ColumnGroups.BeginUpdate;
  try
    ColumnGroups.Add(FColumnIndex, dxSpreadSheetMaxColumnIndex);
    AGroup := TdxSpreadSheetTableItemGroupAccess(ColumnGroups.Find(FColumnIndex));
    ANode.ForEach(ProcessSubNode);
    if AGroup.StartIndex < FColumnIndex then
    begin
      AGroup.FCollapsedByUser := not ANode.Attributes.GetValueAsBoolean(sdxODSAttrTableDisplay, True);
      AGroup.FinishIndex := FColumnIndex - 1;
    end
    else
      AGroup.Free;
  finally
    ColumnGroups.EndUpdate;
  end;
end;

procedure TdxSpreadSheetODSReaderTableViewParser.ProcessConditionalFormat(ANode: TdxXMLNode; AUserData: Pointer);
begin
  ExecuteSubTask(TdxSpreadSheetODSReaderConditionalFormatParser.Create(ANode, Self));
end;

procedure TdxSpreadSheetODSReaderTableViewParser.ProcessRowGroup(ANode: TdxXMLNode);
var
  AGroup: TdxSpreadSheetTableItemGroupAccess;
begin
  RowGroups.BeginUpdate;
  try
    RowGroups.Add(FRowIndex, dxSpreadSheetMaxColumnIndex);
    AGroup := TdxSpreadSheetTableItemGroupAccess(RowGroups.Find(FRowIndex));
    ANode.ForEach(ProcessSubNode);
    if AGroup.StartIndex < FRowIndex then
    begin
      AGroup.FCollapsedByUser := not ANode.Attributes.GetValueAsBoolean(sdxODSAttrTableDisplay, True);
      AGroup.FinishIndex := FRowIndex - 1;
    end
    else
      AGroup.Free;
  finally
    RowGroups.EndUpdate;
  end;
end;

procedure TdxSpreadSheetODSReaderTableViewParser.ProcessShapes(ANode: TdxXMLNode; AUserData: Pointer);
begin
  ExecuteSubTask(TdxSpreadSheetODSReaderShapeContainerParser.Create(ANode, Self));
end;

procedure TdxSpreadSheetODSReaderTableViewParser.ProcessSubNode(ANode: TdxXMLNode; AUserData: Pointer);
begin
  if ANode.Name = sdxODSNodeTableColumn then
    ExecuteSubTask(TdxSpreadSheetODSReaderTableColumnParser.Create(ANode, Self))
  else

  if ANode.Name = sdxODSNodeTableRow then
    ExecuteSubTask(TdxSpreadSheetODSReaderTableRowParser.Create(ANode, Self))
  else

  if ANode.Name = sdxODSNodeTableColumnGroup then
    ProcessColumnGroup(ANode)
  else

  if ANode.Name = sdxODSNodeTableRowGroup then
    ProcessRowGroup(ANode)
  else

  if ANode.Name = sdxODSNodeCalcExtConditionalFormats then
    ANode.ForEach(ProcessConditionalFormat);
end;

function TdxSpreadSheetODSReaderTableViewParser.GetColumnGroups: TdxSpreadSheetTableItemGroups;
begin
  Result := View.Columns.Groups;
end;

function TdxSpreadSheetODSReaderTableViewParser.GetRowGroups: TdxSpreadSheetTableItemGroups;
begin
  Result := View.Rows.Groups;
end;

{ TdxSpreadSheetODSReaderTableItemParser }

constructor TdxSpreadSheetODSReaderTableItemParser.Create(
  ANode: TdxXMLNode; ATableViewParser: TdxSpreadSheetODSReaderTableViewParser);
begin
  inherited Create(ANode, ATableViewParser.Owner);
  FTableViewParser := ATableViewParser;
end;

function TdxSpreadSheetODSReaderTableItemParser.ConvertReference(const S: string): string;
begin
  Result := TdxSpreadSheetODSFormula.Convert(SpreadSheet, TdxSpreadSheetODSFormulaParser.Tag + S);
end;

function TdxSpreadSheetODSReaderTableItemParser.GetCellStyle(
  ANode: TdxXMLNode; const AAttrName: TdxXMLString; ACanBeNil: Boolean): TdxSpreadSheetCellStyleHandle;
begin
  if not Owner.Styles.TryGetValue(ANode.Attributes.GetValueAsString(AAttrName), Result) then
  begin
    if ACanBeNil then
      Result := nil
    else
      Result := Owner.CellStyles.DefaultStyle;
  end;
end;

function TdxSpreadSheetODSReaderTableItemParser.GetTableView: TdxSpreadSheetTableView;
begin
  Result := TableViewParser.View;
end;

{ TdxSpreadSheetODSReaderTableColumnParser }

procedure TdxSpreadSheetODSReaderTableColumnParser.Execute;
var
  ACellStyle: TdxSpreadSheetCellStyleHandle;
  AColumnStyle: TdxSpreadSheetODSReaderColumnStyle;
  ACount: Integer;
  AItem: TdxSpreadSheetTableColumn;
  AVisible: Boolean;
  I: Integer;
begin
  ACellStyle := GetCellStyle(Node, sdxODSAttrTableDefaultCellStyleName, True);
  AColumnStyle := Owner.ColumnStyles.GetValueOrNil(Node.Attributes.GetValue(sdxODSAttrTableStyleName));
  ACount := Max(1, Node.Attributes.GetValueAsInteger(sdxODSAttrTableNumberColumnsRepeated));
  AVisible := Node.Attributes.GetValue(sdxODSAttrTableVisibility) <> sdxODSValueTableVisibilityHidden;

  if (ACellStyle = nil) and (AColumnStyle = nil) and AVisible then
    Inc(TableViewParser.FColumnIndex, ACount)
  else
    for I := 0 to ACount - 1 do
    begin
      AItem := TableView.Columns.CreateItem(TableViewParser.FColumnIndex);
      if ACellStyle <> nil then
        AItem.Style.Handle := ACellStyle;
      if AColumnStyle <> nil then
        AColumnStyle.ApplyTo(AItem);
      AItem.Visible := AVisible;
      Inc(TableViewParser.FColumnIndex);
    end;
end;

{ TdxSpreadSheetODSReaderTableRowParser }

procedure TdxSpreadSheetODSReaderTableRowParser.Execute;
var
  ACount: Integer;
  ARow: TdxSpreadSheetTableRow;
  ARowStyle: TdxSpreadSheetODSReaderRowStyle;
  AVisible: Boolean;
  I: Integer;
begin
  ACount := Max(1, Node.Attributes.GetValueAsInteger(sdxODSAttrTableNumberRowsRepeated));
  ARowStyle := Owner.RowStyles.GetValueOrNil(Node.Attributes.GetValue(sdxODSAttrTableStyleName));
  AVisible := Node.Attributes.GetValue(sdxODSAttrTableVisibility) <> sdxODSValueTableVisibilityHidden;

  if ARowStyle.IsDefault and AVisible then
  begin
    if Node.HasChildren then
    begin
      ARow := TableView.Rows.CreateItem(TableViewParser.FRowIndex);
      Node.ForEach(ProcessCell, ARow);
      PostProcessCells(ARow);
    end;
    Inc(TableViewParser.FRowIndex, ACount);
  end
  else
    for I := 0 to ACount - 1 do
    begin
      ARow := TableView.Rows.CreateItem(TableViewParser.FRowIndex);
      ARow.Visible := AVisible;
      if ARowStyle <> nil then
        ARowStyle.ApplyTo(ARow);
      if I = 0 then
      begin
        Node.ForEach(ProcessCell, ARow);
        PostProcessCells(ARow);
      end;
      Inc(TableViewParser.FRowIndex);
    end;
end;

procedure TdxSpreadSheetODSReaderTableRowParser.ParseCellTextValue(ATextNode: TdxXMLNode; ACell: TdxSpreadSheetCell);
var
  AFormattedString: TdxSpreadSheetFormattedSharedString;
  AHelper: TdxSpreadSheetODSReaderRichTextHelper;
begin
  if ATextNode.HasChildren then
  begin
    AHelper := TdxSpreadSheetODSReaderRichTextHelper.Create(Self);
    try
      AHelper.Process(ATextNode, ACell.Style.Handle.Font);
      AFormattedString := Owner.CreateTempFormattedSharedString(AHelper.Text);
      AFormattedString.Runs.Assign(AHelper.Runs);
      ACell.AsSharedString := Owner.AddFormattedSharedString(AFormattedString);
      if AHelper.HyperlinkValue <> '' then
        Owner.Hyperlinks.AddObject(AHelper.HyperlinkValue, TableView.Hyperlinks.Add(ACell.RowIndex, ACell.ColumnIndex));
    finally
      AHelper.Free;
    end;
  end
  else
    ACell.AsString := ATextNode.TextAsString;
end;

procedure TdxSpreadSheetODSReaderTableRowParser.ParseCellValue(ANode: TdxXMLNode; ACell: TdxSpreadSheetCell);
var
  AAttr: TdxXMLNodeAttribute;
begin
  if ANode.Attributes.Find(sdxODSAttrTableFormula, AAttr) then
  begin
    Owner.Formulas.Add(ACell, AAttr.ValueAsString,
      (ANode.Attributes.GetValueAsInteger(sdxODSAttrTableNumberColumnsSpanned) > 0) or
      (ANode.Attributes.GetValueAsInteger(sdxODSAttrTableNumberRowsSpanned) > 0), False, cxNullRect);
  end
  else
    case TdxSpreadSheetODSHelper.StringToValueType(ANode.Attributes.GetValue(sdxODSAttrOfficeValueType)) of
      sovtBoolean:
        ACell.AsBoolean := ANode.Attributes.GetValueAsBoolean(sdxODSAttrOfficeBooleanValue);
      sovtTime:
        ACell.AsFloat := TdxSpreadSheetODSHelper.StringToTime(ANode.Attributes.GetValue(sdxODSAttrOfficeTimeValue));
      sovtCurrency:
        ACell.AsCurrency := ANode.Attributes.GetValueAsFloat(sdxODSAttrOfficeValue);
      sovtDate:
        ACell.AsDateTime := dxRealDateTimeToDateTime(TdxSpreadSheetODSHelper.StringToDate(
          ANode.Attributes.GetValue(sdxODSAttrOfficeDateValue)), SpreadSheet.OptionsView.ActualDateTimeSystem);
      sovtPercentage, sovtFloat:
        ACell.AsFloat := StrToFloatDef(ANode.Attributes.GetValueAsString(sdxODSAttrOfficeValue), 0, dxInvariantFormatSettings);
      sovtString:
        if ANode.Attributes.Find(sdxODSAttrOfficeStringValue, AAttr) then
          ACell.AsString := AAttr.ValueAsString
        else
          if ANode.FindChild(sdxODSNodeText, ANode) then
            ParseCellTextValue(ANode, ACell);
    end;
end;

procedure TdxSpreadSheetODSReaderTableRowParser.PostProcessCells(ARow: TdxSpreadSheetTableRow);
begin
  TdxSpreadSheetTableRowCellsAccess(TdxSpreadSheetTableRowAccess(ARow).RowCells).ForEach(
    procedure(AItem: TdxDynamicListItem)
    var
      ACell: TdxSpreadSheetCell;
    begin
      ACell := TdxSpreadSheetCell(AItem);
      if ACell.IsEmpty and (ACell.Style.Handle = ARow.Style.Handle) then
      begin
        if not (Owner.Formulas.HasCell(ACell) or ACell.View.Containers.IsCellUsed(ACell)) then
          AItem.Free;
      end;
    end);
end;

procedure TdxSpreadSheetODSReaderTableRowParser.ProcessCell(ANode: TdxXMLNode; AUserData: Pointer);
var
  ACell: TdxSpreadSheetCell;
  AColsSpan: Integer;
  ARangeSize: Integer;
  ARow: TdxSpreadSheetTableRow;
  ARowsSpan: Integer;
begin
  if ANode.Name <> sdxODSNodeTableCell then
    Exit;

  ARow := TdxSpreadSheetTableRow(AUserData);
  ARangeSize := ANode.Attributes.GetValueAsInteger(sdxODSAttrTableNumberColumnsRepeated);
  if ARangeSize > 0 then
  begin
    ProcessCellRange(ANode, ARow, ARangeSize);
    Exit;
  end;

  ACell := ARow.CreateCell(FCellIndex);
  ACell.Style.Handle := GetCellStyle(ANode, sdxODSAttrTableStyleName, False);
  ParseCellValue(ANode, ACell);

  AColsSpan := Max(1, ANode.Attributes.GetValueAsInteger(sdxODSAttrTableNumberColumnsSpanned));
  ARowsSpan := Max(1, ANode.Attributes.GetValueAsInteger(sdxODSAttrTableNumberRowsSpanned));
  if (ARowsSpan > 1) or (AColsSpan > 1) then
    TableView.MergedCells.Add(cxRectBounds(ACell.ColumnIndex, ACell.RowIndex, AColsSpan - 1, ARowsSpan - 1));
  Inc(FCellIndex, AColsSpan);

  ANode.ForEach(ProcessDrawObject, ACell);
end;

procedure TdxSpreadSheetODSReaderTableRowParser.ProcessCellRange(
  ANode: TdxXMLNode; ARow: TdxSpreadSheetTableRow; ARangeSize: Integer);
var
  ACell: TdxSpreadSheetCell;
  ACellStyle: TdxSpreadSheetCellStyleHandle;
  I: Integer;
begin
  ACellStyle := GetCellStyle(ANode, sdxODSAttrTableStyleName, True);
  if ACellStyle <> nil then
  begin
    if ARow.Style.IsDefault and (ARangeSize > 100) and not ANode.HasChildren then
      ARow.Style.Handle := ACellStyle
    else
      for I := 0 to ARangeSize - 1 do
      begin
        ACell := ARow.CreateCell(FCellIndex + I);
        ACell.Style.Handle := ACellStyle;
        ParseCellValue(ANode, ACell);
      end;
  end;
  Inc(FCellIndex, ARangeSize);
end;

procedure TdxSpreadSheetODSReaderTableRowParser.ProcessDrawObject(ANode: TdxXMLNode; AUserData: Pointer);
var
  AParserClass: TdxSpreadSheetODSReaderCustomContainerParserClass;
  ASubNode: TdxXMLNode;
begin
  if ANode.Name = sdxODSNodeDrawFrame then
    AParserClass := TdxSpreadSheetODSReaderPictureContainerParser
  else

  if ANode.Name = sdxODSNodeDrawCustomShape then
  begin
    if ANode.FindChild(sdxODSNodeText, ASubNode) then
      AParserClass := TdxSpreadSheetODSReaderTextBoxContainerParser
    else
      AParserClass := TdxSpreadSheetODSReaderShapeContainerParser;
  end
  else

  if ANode.Name = sdxODSNodeDrawAnnotation then
    AParserClass := TdxSpreadSheetODSReaderCommentContainerParser
  else
    AParserClass := nil;

  if AParserClass <> nil then
    ExecuteSubTask(AParserClass.Create(ANode, TableViewParser, Self, TdxSpreadSheetCell(AUserData)));
end;

{ TdxSpreadSheetODSReaderConditionalFormatParser }

procedure TdxSpreadSheetODSReaderConditionalFormatParser.Execute;
var
  ARule: TdxSpreadSheetCustomConditionalFormattingRule;
  ASubNode: TdxXMLNode;
begin
  ASubNode := Node.First;
  while ASubNode <> nil do
  begin
    if ASubNode.Name = sdxODSNodeCalcExtCondition then
      ARule := CreateConditionBasedRule(ASubNode)
    else

    if ASubNode.Name = sdxODSNodeCalcExtColorScale then
      ARule := CreateColorScaleRule(ASubNode)
    else

    if ASubNode.Name = sdxODSNodeCalcExtDataBar then
      ARule := CreateDataBarRule(ASubNode)
    else

    if ASubNode.Name = sdxODSNodeCalcExtIconSet then
      ARule := CreateIconSetRule(ASubNode)
    else
      ARule := nil;

    if ARule <> nil then
      ARule.Area := dxStringToReferenceArea(StringReplace(
        Node.Attributes.GetValueAsString(sdxODSAttrCalcExtTargetRangeAddress),
        TableView.Caption + '.', '', [rfReplaceAll, rfIgnoreCase]));

    ASubNode := ASubNode.Next;
  end;
end;

function TdxSpreadSheetODSReaderConditionalFormatParser.CreateAverageRule(ANode: TdxXMLNode;
  AOperator: TdxSpreadSheetConditionalFormattingRuleAboveOrBelowAverageComparisonOperator): TdxSpreadSheetConditionalFormattingRuleAboveOrBelowAverage;
begin
  Result := TdxSpreadSheetConditionalFormattingRuleAboveOrBelowAverage.Create(TableView.ConditionalFormatting);
  Result.BeginUpdate;
  try
    Result.ComparisonOperator := AOperator;
    ReadStyle(ANode, Result);
  finally
    Result.EndUpdate;
  end;
end;

function TdxSpreadSheetODSReaderConditionalFormatParser.CreateCellIsRule(
  AComparisonOperator: TdxSpreadSheetConditionalFormattingRuleCellIsComparisonOperator;
  const AExpression1, AExpression2: string; ANode: TdxXMLNode): TdxSpreadSheetConditionalFormattingRuleCellIs;
begin
  Result := TdxSpreadSheetConditionalFormattingRuleCellIs.Create(TableView.ConditionalFormatting);
  Result.BeginUpdate;
  try
    Result.ComparisonOperator := AComparisonOperator;
    Result.Expression := AExpression1;
    Result.Expression2 := AExpression2;
    ReadStyle(ANode, Result);
  finally
    Result.EndUpdate;
  end;
end;

function TdxSpreadSheetODSReaderConditionalFormatParser.CreateDuplicateValuesRule(
  ANode: TdxXMLNode): TdxSpreadSheetConditionalFormattingRuleDuplicateValues;
begin
  Result := TdxSpreadSheetConditionalFormattingRuleDuplicateValues.Create(TableView.ConditionalFormatting);
  ReadStyle(ANode, Result);
end;

function TdxSpreadSheetODSReaderConditionalFormatParser.CreateExpressionRule(
  const AExpression: string; ANode: TdxXMLNode): TdxSpreadSheetConditionalFormattingRuleExpression;
begin
  Result := TdxSpreadSheetConditionalFormattingRuleExpression.Create(TableView.ConditionalFormatting);
  Result.BeginUpdate;
  try
    Result.Expression := AExpression;
    ReadStyle(ANode, Result);
  finally
    Result.EndUpdate;
  end;
end;

function TdxSpreadSheetODSReaderConditionalFormatParser.CreateTopBottomRule(ANode: TdxXMLNode;
  ADirection: TdxSpreadSheetConditionalFormattingRuleTopBottomValuesDirection;
  AValueType: TdxSpreadSheetConditionalFormattingRuleTopBottomValuesValueType;
  AValue: Integer): TdxSpreadSheetConditionalFormattingRuleTopBottomValues;
begin
  Result := TdxSpreadSheetConditionalFormattingRuleTopBottomValues.Create(TableView.ConditionalFormatting);
  Result.BeginUpdate;
  try
    Result.Direction := ADirection;
    Result.ValueType := AValueType;
    Result.Value := AValue;
    ReadStyle(ANode, Result);
  finally
    Result.EndUpdate;
  end;
end;

function TdxSpreadSheetODSReaderConditionalFormatParser.CreateUniqueValuesRule(
  ANode: TdxXMLNode): TdxSpreadSheetConditionalFormattingRuleUniqueValues;
begin
  Result := TdxSpreadSheetConditionalFormattingRuleUniqueValues.Create(TableView.ConditionalFormatting);
  ReadStyle(ANode, Result);
end;

function TdxSpreadSheetODSReaderConditionalFormatParser.TryCreateCellIsRule(
  const ACondition: string; ANode: TdxXMLNode): TdxSpreadSheetCustomConditionalFormattingRule;
const
  BetweenOpName = 'between';
  BetweenOpMap: array[Boolean] of TdxSpreadSheetConditionalFormattingRuleCellIsComparisonOperator = (
    cicoNotBetween, cicoBetween
  );
  CellIsOpMap: array[TdxSpreadSheetConditionalFormattingRuleCellIsComparisonOperator] of string = (
    #0, '=', '>', '>=', '<', '<=', #0, '!='
  );
var
  AComparisonOperator: TdxSpreadSheetConditionalFormattingRuleCellIsComparisonOperator;
  AExpressions: TStringList;
begin
  for AComparisonOperator := Low(AComparisonOperator) to High(AComparisonOperator) do
  begin
    if IsBeginsWith(CellIsOpMap[AComparisonOperator], ACondition) then
      Exit(CreateCellIsRule(AComparisonOperator, ConvertReference(
        Copy(ACondition, Length(CellIsOpMap[AComparisonOperator]), MaxInt)), '', ANode));
  end;

  if CheckFunctionAndExtractParams(BetweenOpName, ACondition, AExpressions) or
     CheckFunctionAndExtractParams('not-' + BetweenOpName, ACondition, AExpressions)
  then
    try
      if AExpressions.Count = 2 then
      begin
        Exit(CreateCellIsRule(BetweenOpMap[IsBeginsWith(BetweenOpName, ACondition)],
          ConvertReference(AExpressions[0]), ConvertReference(AExpressions[1]), ANode));
      end;
    finally
      AExpressions.Free;
    end;

  Result := nil;
end;

function TdxSpreadSheetODSReaderConditionalFormatParser.TryCreateTopBottomRule(
  const AFuncName, ACondition: string; ANode: TdxXMLNode;
  ADirection: TdxSpreadSheetConditionalFormattingRuleTopBottomValuesDirection;
  AValueType: TdxSpreadSheetConditionalFormattingRuleTopBottomValuesValueType;
  var ARule: TdxSpreadSheetCustomConditionalFormattingRule): Boolean;
var
  AExpressions: TStringList;
begin
  Result := False;
  if CheckFunctionAndExtractParams(AFuncName, ACondition, AExpressions) then
  try
    Result := AExpressions.Count > 0;
    if Result then
      ARule := CreateTopBottomRule(ANode, ADirection, AValueType, StrToIntDef(AExpressions[0], 10));
  finally
    AExpressions.Free;
  end;
end;

function TdxSpreadSheetODSReaderConditionalFormatParser.CheckFunctionAndExtractParams(
  const AFuncName, ACondition: string; out AParams: TStringList): Boolean;
var
  AParser: TdxSpreadSheetODSFormulaParser;
begin
  Result := IsBeginsWith(AFuncName + '(', ACondition);
  if Result then
  begin
    AParser := TdxSpreadSheetODSFormulaParser.Create(SpreadSheet);
    try
      AParams := AParser.ExtractParams(Copy(ACondition, Length(AFuncName) + 2, Length(ACondition) - Length(AFuncName) - 2));
    finally
      AParser.Free;
    end;
  end;
end;

function TdxSpreadSheetODSReaderConditionalFormatParser.IsBeginsWith(const ATestString, AFullString: string): Boolean;
begin
  Result := (Length(AFullString) > Length(ATestString)) and
    CompareMem(@ATestString[1], @AFullString[1], Length(ATestString) * SizeOf(Char));
end;

procedure TdxSpreadSheetODSReaderConditionalFormatParser.ReadColorScaleStop(
  AStop: TdxSpreadSheetConditionalFormattingRuleColorScaleStop; ANode: TdxXMLNode);
begin
  ReadCustomScaleStop(AStop, ANode);
  AStop.Color := TdxSpreadSheetODSHelper.StringToColor(ANode.Attributes.GetValueAsString(sdxODSAttrCalcExtColor));
end;

procedure TdxSpreadSheetODSReaderConditionalFormatParser.ReadCustomScaleStop(
  AStop: TdxSpreadSheetConditionalFormattingRuleCustomScaleStop; ANode: TdxXMLNode);

  function ReadValueType(const S: AnsiString): TdxSpreadSheetConditionalFormattingRuleCustomScaleStopValueType;
  const
    Map: array[TdxSpreadSheetConditionalFormattingRuleCustomScaleStopValueType] of AnsiString = (
      '', sdxODSValueTypeNumber, sdxODSValueTypePercent, sdxODSValueTypeFormula, sdxODSValueTypePercentile
    );
  var
    I: TdxSpreadSheetConditionalFormattingRuleCustomScaleStopValueType;
  begin
    Result := cssvtLimitValue;
    for I := Low(I) to High(I) do
    begin
      if Map[I] = S then
        Exit(I);
    end;
  end;

begin
  AStop.ValueType := ReadValueType(ANode.Attributes.GetValue(sdxODSAttrCalcExtType));
  case AStop.ValueType of
    cssvtFormula:
      AStop.Value := ConvertReference(Copy(ANode.Attributes.GetValueAsString(sdxODSAttrCalcExtValue), 2, MaxInt));
    cssvtPercent, cssvtPercentile:
      AStop.Value := ANode.Attributes.GetValueAsInteger(sdxODSAttrCalcExtValue);
    cssvtValue:
      AStop.Value := ANode.Attributes.GetValueAsFloat(sdxODSAttrCalcExtValue);
  end;
end;

procedure TdxSpreadSheetODSReaderConditionalFormatParser.ReadStyle(
  ANode: TdxXMLNode; ARule: TdxSpreadSheetConditionalFormattingRuleStyleBased);
var
  AStyleHandle: TdxSpreadSheetCellStyleHandle;
begin
  if Owner.Styles.TryGetValue(ANode.Attributes.GetValueAsString(sdxODSAttrCalcExtStyleName), AStyleHandle) then
    ARule.Style.Handle := AStyleHandle;
end;

function TdxSpreadSheetODSReaderConditionalFormatParser.CreateColorScaleRule(
  ANode: TdxXMLNode): TdxSpreadSheetCustomConditionalFormattingRule;
var
  AIndex: Integer;
begin
  case ANode.Count of
    2: Result := TdxSpreadSheetConditionalFormattingRuleTwoColorScale.Create(TableView.ConditionalFormatting);
    3: Result := TdxSpreadSheetConditionalFormattingRuleThreeColorScale.Create(TableView.ConditionalFormatting);
  else
    Result := nil;
  end;

  if Result <> nil then
  begin
    AIndex := 0;
    ANode := ANode.First;
    while ANode <> nil do
    begin
      ReadColorScaleStop(TdxSpreadSheetConditionalFormattingRuleCustomColorScaleAccess(Result).Stops[AIndex], ANode);
      ANode := ANode.Next;
      Inc(AIndex);
    end;
  end;
end;

function TdxSpreadSheetODSReaderConditionalFormatParser.CreateDataBarRule(
  ANode: TdxXMLNode): TdxSpreadSheetConditionalFormattingRuleDataBar;

  function ReadColor(const AAttrName: TdxXMLString; ADefaultColor: TColor): TColor;
  var
    AAttr: TdxXMLNodeAttribute;
  begin
    if ANode.Attributes.Find(AAttrName, AAttr) then
      Result := TdxSpreadSheetODSHelper.StringToColor(AAttr.ValueAsString)
    else
      Result := ADefaultColor;
  end;

  function DecodeAxisPosition(const S: TdxXMLString): TdxSpreadSheetConditionalFormattingRuleDataBarAxisPosition;
  begin
    if S = 'middle' then
      Result := dbapMidpoint
    else
      if S = 'none' then
        Result := dbapNone
      else
        Result := dbapAuto;
  end;

begin
  Result := TdxSpreadSheetConditionalFormattingRuleDataBar.Create(TableView.ConditionalFormatting);
  Result.Style.AxisPosition := DecodeAxisPosition(ANode.Attributes.GetValue(sdxODSAttrCalcExtAxisPosition));
  Result.Style.AxisColor := ReadColor(sdxODSAttrCalcExtAxisColor, clNone);
  Result.Style.NegativeBarColor := ReadColor(sdxODSAttrCalcExtNegativeColor, clDefault);
  Result.Style.PositiveBarColor := ReadColor(sdxODSAttrCalcExtPositiveColor, $EF8B00);
  Result.Style.FillMode := dbfmGradient;
  if Node.Count >= 1 then
    ReadCustomScaleStop(Result.MinValue, ANode.First);
  if Node.Count >= 2 then
    ReadCustomScaleStop(Result.MaxValue, ANode[1]);
end;

function TdxSpreadSheetODSReaderConditionalFormatParser.CreateIconSetRule(
  ANode: TdxXMLNode): TdxSpreadSheetConditionalFormattingRuleIconSet;
var
  I: Integer;
begin
  Result := TdxSpreadSheetConditionalFormattingRuleIconSet.Create(TableView.ConditionalFormatting);
  Result.PresetName := ANode.Attributes.GetValueAsString(sdxODSAttrCalcExtIconSetType);
  for I := 0 to Result.StopCount - 1 do
    ReadCustomScaleStop(Result.Stops[I], ANode[I]);
end;

function TdxSpreadSheetODSReaderConditionalFormatParser.CreateConditionBasedRule(
  ANode: TdxXMLNode): TdxSpreadSheetCustomConditionalFormattingRule;
var
  ACondition: string;
  AExpressions: TStringList;
begin
  ACondition := ANode.Attributes.GetValueAsString(sdxODSAttrCalcExtValue);

  if CheckFunctionAndExtractParams('formula-is', ACondition, AExpressions) then
  try
    if AExpressions.Count > 0 then
      Exit(CreateExpressionRule(ConvertReference(AExpressions[0]), ANode));
  finally
    AExpressions.Free;
  end;

  if TryCreateTopBottomRule('top', ACondition, ANode, tbvdTop, tbvvtRank, Result) or
    TryCreateTopBottomRule('top-percent', ACondition, ANode, tbvdTop, tbvvtPercent, Result) or
    TryCreateTopBottomRule('bottom', ACondition, ANode, tbvdBottom, tbvvtRank, Result) or
    TryCreateTopBottomRule('bottom-percent', ACondition, ANode, tbvdBottom, tbvvtPercent, Result)
  then
    Exit;

  if ACondition = 'duplicate' then
    Exit(CreateDuplicateValuesRule(ANode));
  if ACondition = 'unique' then
    Exit(CreateUniqueValuesRule(ANode));
  if ACondition = 'above-average' then
    Exit(CreateAverageRule(ANode, abacoAboveAverage));
  if ACondition = 'above-equal-average' then
    Exit(CreateAverageRule(ANode, abacoAboveOrEqualAverage));
  if ACondition = 'below-average' then
    Exit(CreateAverageRule(ANode, abacoBelowAverage));
  if ACondition = 'below-equal-average' then
    Exit(CreateAverageRule(ANode, abacoBelowOrEqualAverage));

  Result := TryCreateCellIsRule(ACondition, ANode);
end;

end.
