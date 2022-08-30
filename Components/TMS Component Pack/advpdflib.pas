{*************************************************************************}
{ TMS PDF Engine                                                          }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright © 2015                                              }
{           Email : info@tmssoftware.com                                  }
{           Web : http://www.tmssoftware.com                              }
{                                                                         }
{ The source code is given as is. The author is not responsible           }
{ for any possible damage done due to the use of this code.               }
{ The component can be freely used in any application. The complete       }
{ source code remains property of the author and may not be distributed,  }
{ published, given or sold in any form as such. No parts of the source    }
{ code can be included in any other component or application without      }
{ written authorization of the author.                                    }
{*************************************************************************}

unit AdvPDFLIB;

{$I TMSDEFS.INC}

interface

uses
  ZLib, Windows, Classes, SysUtils, Dialogs, Graphics, StrUtils, Math,
  JPEG, PngImage, Generics.Collections, ImgList, AdvPDFFuncs
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;

var
  FontSub: THandle = INVALID_HANDLE_VALUE;

  CreateFontPackage: function(puchSrcBuffer: pointer; ulSrcBufferSize: cardinal;
    var puchFontPackageBuffer: PAnsiChar;
    var pulFontPackageBufferSize: cardinal; var pulBytesWritten: cardinal;
    usFlags, usTTCIndex, usSubsetFormat, usSubsetLanguage, usSubsetPlatform,
    usSubsetEncoding: word; pusSubsetKeepList: PWordArray;
    usSubsetKeepListCount: word; lpfnAllocate, lpfnReAllocate, lpfnFree,
    reserved: pointer): cardinal; cdecl;

const
//  sLineBreak =  #10;
  FONTSIZE = 11;
  TTFCFP_MAC_PLATFORMID = 1;
  TTFCFP_MS_PLATFORMID = 3;
  TTFCFP_SYMBOL_CHAR_SET = 0;
  TTFCFP_UNICODE_CHAR_SET = 1;
  TTFCFP_FLAGS_SUBSET = 1;
  TTFMFP_SUBSET = 0;

type
{$IFNDEF DELPHIXE2_LVL}
  z_stream = TZStreamRec;
{$ENDIF}

  RawUTF8 = type AnsiString(CP_UTF8);
  TRawUTF8DynArray = array of RawUTF8;
  PDFString = AnsiString;

  TPdfFont = class;

  TPDFDocument = class;
  TPDFPageItem = class;
  TPDFTableItem = class;
  TPDFTableRowItem = class;

  TMultiPageOption = (mpoHorizontal, mpoVertical, mpoNone);

  TPDFALIGNMENT = (alLeft, alRight, alCenter);

  TPDFPageLayout = (plLandscape, plPortrait);

  TPDFBulletStyle = (bsNone, bsStar, bsArrow, bsSquare, bsCheck, bsCircle);

  TCmapHeader = packed record
    // Version number (Set to zero)
    version: word;
    // Number of encoding subtables
    numberSubtables: word;
  end;

  // Points to every 'cmap' encoding subtables
  TCmapSubTableArray = packed array [byte] of packed record
    // Platform identifier
    platformID: word;
    // Platform-specific encoding identifier
    platformSpecificID: word;
    // Offset of the mapping table
    offset: cardinal;
  end;

  // The 'hhea' table contains information needed to layout fonts whose
  // characters are written horizontally, that is, either left to right or
  // right to left
  TCmapHHEA = packed record
    version: longint;
    ascent: word;
    descent: word;
    lineGap: word;
    advanceWidthMax: word;
    minLeftSideBearing: word;
    minRightSideBearing: word;
    xMaxExtent: word;
    caretSlopeRise: SmallInt;
    caretSlopeRun: SmallInt;
    caretOffset: SmallInt;
    reserved: Int64;
    metricDataFormat: SmallInt;
    numOfLongHorMetrics: word;
  end;

  // The 'head' table contains global information about the font
  TCmapHEAD = packed record
    version: longint;
    fontRevision: longint;
    checkSumAdjustment: cardinal;
    magicNumber: cardinal;
    flags: word;
    unitsPerEm: word;
    createdDate: Int64;
    modifiedDate: Int64;
    xMin: SmallInt;
    yMin: SmallInt;
    xMax: SmallInt;
    yMax: SmallInt;
    macStyle: word;
    lowestRec: word;
    fontDirection: SmallInt;
    indexToLocFormat: SmallInt;
    glyphDataFormat: SmallInt
  end;

  // Header for the 'cmap' Format 4 table
  // - this is a two-byte encoding format
  TCmapFmt4 = packed record
    format: word;
    length: word;
    language: word;
    segCountX2: word;
    searchRange: word;
    entrySelector: word;
    rangeShift: word;
  end;

  TPdfDictionaryWrapper = class(TPersistent)
  private
    FData: TObject;
    function GetHasData: Boolean;
  protected
    procedure SetData(AData: TObject);
  public
    // The associated dictionary, containing all data
    property Data: TObject read FData write SetData;
    // return TRUE if has any data stored within
    property HasData: Boolean read GetHasData;
  end;

  // Handle Unicode glyph description for a True Type Font
  // - cf http://www.microsoft.com/typography/OTSPEC/otff.htm#otttables
  // - handle Microsoft cmap format 4 encoding (i.e. most used
  // true type fonts on Windows)
  TPdfTTF = class
  protected
    // We use TWordDynArray for auto garbage collection and generic handling
    // - since the TTF file is big endian, we swap all words at loading, to
    // be used directly by the Intel x86 code; Integer (longint) values
    // must take care of this byte swapping
    fcmap,
    fhead,
    fhhea,
    fhmtx: TWordDynArray;
  public
    // These are pointers to the useful data of the True Type Font:
    // Font header
    head: ^TCmapHEAD;
    // Horizontal header
    hhea: ^TCmapHHEA;
    // Character to glyph mapping (cmap) table, in format 4
    fmt4: ^TCmapFmt4;
    // Start character code for each cmap format 4 segment
    startCode: PWordArray;
    // End characterCode for each cmap format 4 segment
    endCode: PWordArray;
    // Delta for all character codes in each cmap format 4 segment
    idDelta: PSmallIntArray;
    // Offsets into glyphIndexArray or 0
    idRangeOffset: PWordArray;
    // Glyph index array (arbitrary length)
    glyphIndexArray: PWordArray;
  public
    // Create Unicode glyph description for a supplied True Type Font
    // - the HDC of its corresponding document must have selected the font first
    // - this constructor will fill fUsedWide[] and fUsedWideChar of aUnicodeTTF
    // with every available unicode value, and its corresponding glyph and width
    constructor Create(aUnicodeTTF: TPdfFont); reintroduce;
  end;

  TPdfObjectMgr = class(TObject)
  public
    procedure AddObject(AObject: TObject); virtual; abstract;
    function GetObject(ObjectID: Integer): TObject; virtual; abstract;
  end;

  TPdfXrefEntry = class(TObject)
  private
    FEntryType: PDFString;
    FByteOffset: Integer;
    FGenerationNumber: Integer;
    FObjectStreamIndex: Integer;
    FValue: TObject;
  public
    // Create the entry, with the specified value
    // - if the value is nil (e.g. root entry), the type is 'f' (PDF_FREE_ENTRY),
    // otherwise the entry type is 'n' (PDF_IN_USE_ENTRY)
    constructor Create(AValue: TObject);
    // Release the memory, and the associated value, if any
    destructor Destroy; override;
    // Return either 'f' (PDF_FREE_ENTRY), either 'n' (PDF_IN_USE_ENTRY)
    property EntryType: PDFString read FEntryType write FEntryType;
    // The position (in bytes) in the PDF file content stream
    // - to be ignored if ObjectStreamIndex>=0
    property ByteOffset: Integer read FByteOffSet;
    // The index of this object in the global compressed /ObjStm object stream
    // - equals -1 by default, i.e. if stored within the main file content stream
    property ObjectStreamIndex: Integer read FObjectStreamIndex;
    // The associated Generation Number
    // - mostly 0, or 65535 (PDF_MAX_GENERATION_NUM) for the root 'f' entry
    property GenerationNumber: Integer read FGenerationNumber write FGenerationNumber;
    // The associated PDF object
    property Value: TObject read FValue;
  end;

  TPdfXref = class(TPdfObjectMgr)
  private
    FXrefEntries: TList;
    function GetItem(ObjectID: Integer): TPdfXrefEntry; {$ifdef HASINLINE}inline;{$endif}
    function GetItemCount: Integer; {$ifdef HASINLINE}inline;{$endif}
  public
    // Initialize the XRef object list
    // - create first a void 'f' (PDF_FREE_ENTRY) as root
    constructor Create;
    // Release instance memory and all associated XRef objects
    destructor Destroy; override;
    // Register object to the xref table, and set corresponding object ID
    function GetObject(ObjectID: Integer): TObject; override;
    // Retrieve a XRef object instance, from its object ID
    property Items[ObjectID: Integer]: TPdfXrefEntry read GetItem; default;
    // Retrieve the XRef object count
    property ItemCount: Integer read GetItemCount;
  end;

  TPdfFont = class(TPdfDictionaryWrapper)
  protected
    FM: TTextMetric;
    FOTM: POutlineTextmetric;
    FFontStyle:TFontStyles;
    FAlias: string;
    FWinAnsiWidth: PPdfWinAnsiWidth;
    FUsedWide: TUsedWide;
    FDocucment: TPDFDocument;
    FHGDI: HGDIOBJ;
    FUsedWideChar: TSortedWordArray;
    FSubType: string;
    Encoding: string;
    FFixedWidth: Boolean;
    FName: string;
    FShortCut: PDFString;
    FFirstChar, FLastChar: Integer;
    FDefaultWidth: word;
    FAscent, fDescent: Integer;
    FUnicode: Boolean;
    FEmbedFont:boolean;
    FCharAndWidthDic: TDictionary<Char, Extended>;
    // Index in TrueTypeFontsIndex[] + 1, 0 if not a TPdfFontTrueType
    // - same TPdfFontTrueType index may appear multiple times in the font list,
    // e.g. with normal, bold and/or italic attributes
    // - this hidden property is used by TPdfDocument for faster font list handling
    FTrueTypeFontsIndex: Integer;
    // Contains a bit for every WinAnsi encoded char
    // - encoding in TPdfFont, even if used by TPdfFontWinAnsi descendent only
    FWinAnsiUsed: set of AnsiChar;
    FWinUnicodeUsed: array of WideChar;
    procedure Init();
  public
    // Create the PDF font object instance
    property  FontStyle:TFontStyles read FFontStyle write FFontStyle;
    property EmbedFont:boolean read FEmbedFont write FEmbedFont;
    function FindOrAddUsedWideChar(aWideChar: WideChar): Integer;
    constructor Create(Parent:TPDFDocument;const AName: string);
    destructor Destroy; override;

    function GetTTFXData(Doc: TPDFDocument): ansistring;
    // Mark some WinAnsi char as used
    procedure AddUsedUnicodeChar(aChar: Widechar);
    procedure AddUsedWinAnsiChar(aChar: AnsiChar); {$ifdef HASINLINE}inline;{$endif}
    procedure SetCharAndWidthDic(ADictionary: TDictionary<Char, Extended>);
    // Retrieve the width of a specified character
    // - implementation of this method is either WinAnsi (by TPdfFontWinAnsi),
    // either compatible with MBCS strings (TPdfFontCIDFontType2)
    // - return 0 by default (descendant must handle the Ansi charset)
    function GetAnsiCharWidth(const AText: PDFString; APos: Integer): Integer; virtual;
    // The internal PDF font name (e.g. 'Helvetica-Bold')
    // - postscript font names are inside the unit: these postscript names
    // could not match the "official" True Type font name, stored as
    // UTF-8 in FTrueTypeFonts
    property Alias: string read FAlias write FAlias;
    property Name: string read FName;
    // The internal PDF shortcut (e.g. 'F3')
    property ShortCut: PDFString read FShortCut;
    // Is set to TRUE if the font is dedicated to Unicode Chars
    property Unicode: Boolean read fUnicode write FUnicode;
  end;


  // A generic PDF font object, handling at least WinAnsi encoding
  // - TPdfFontTrueType descendent will handle also Unicode chars,
  // for all WideChar which are outside the WinAnsi selection
  TPdfFontWinAnsi = class(TPdfFont)
  protected
    // Contain the Width array of the corresponding WinAnsi encoded char
    FWinAnsiWidth: PPdfWinAnsiWidth;
  public
    // Retrieve the width of a specified character
    // - implementation of this method expect WinAnsi encoding
    // - return the value contained in fWinAnsiWidth[] by default
    function GetAnsiCharWidth(const AText: PDFString; APos: Integer): Integer; override;
    // Release the used memory
    destructor Destroy; override;
  end;

  TPdfCellImageType = (citBitmap, citPicture, citFilePicture, citImageList, citImages);

  TPdfVerticalAlignment = (vtaTop, vtaCenter, vtaBottom);

  TPdfCellHAlign = (haLeft, haRight, haCenter, haBeforeText, haAfterText, haFull);

  TPdfCellVAlign = (vaTop, vaBottom, vaCenter, vaUnderText, vaAboveText, vaFull);


  THTMLRef = record
    BGColor: TColor;
    Color: TColor;
    HasImage: Boolean;
    IsUrl: Boolean;
    ImgIdx: Integer;
    FontName: string;
    FontStyle: TFontStyles;
    Size: Integer;
    Text: string;
    Url: string;
    X: Integer;
    Y: Integer;
  end;

  // Table supporting objects

  // Table cell

  TPDFTableRowCellItem = class(TCollectionItem)
  private
    FFont: TFont;
    FText: string;
    FAlignment: TPDFALIGNMENT;
    FBordercolor: TColor;
    FBackgroundColor: TColor;
    FIsMerged: Boolean;
    FBaseCell: TPoint;
    FHAlign: TAlignment;
    FVAlign: TPdfVerticalAlignment;
    FCellSpan: TPoint;
    FImageType: TPdfCellImageType;
    FBitmaps: TObjectList<TBitmap>;
    FImageHAlign: TPdfCellHAlign;
    FImageVAlign: TPdfCellVAlign;
    FBackgroundColorFrom: TColor;
    FBackgroundColorTo: TColor;
    FBackgroundMirrorColorTo: TColor;
    FBackgroundMirrorColorFrom: TColor;
    FBackgroundOrientation: Boolean;
    FGradientRef: string;
    FPage: Integer;
    FUrl: string;
    FUrlColor: TColor;
    FHTMLText: string;
    FHTML: TList<THTMLRef>;
    FIsCheckbox: Boolean;
    FChecked: Boolean;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    property Alignment: TPDFALIGNMENT read FAlignment write FAlignment;
    property BackgroundColor: TColor read FBackgroundColor write FBackgroundColor;
    property BackgroundColorFrom: TColor read FBackgroundColorFrom write FBackgroundColorFrom;
    property BackgroundColorTo: TColor read FBackgroundColorTo write FBackgroundColorTo;
    property BackgroundMirrorColorFrom: TColor read FBackgroundMirrorColorFrom write FBackgroundMirrorColorFrom;
    property BackgroundMirrorColorTo: TColor read FBackgroundMirrorColorTo write FBackgroundMirrorColorTo;
    property BackgroundOrientation: Boolean read FBackgroundOrientation write FBackgroundOrientation;
    property BaseCell: TPoint read FBaseCell write FBaseCell;
    property Bitmaps: TObjectList<TBitmap> read FBitmaps write FBitmaps;
    property BorderColor: TColor read FBordercolor write FBordercolor;
    property CellSpan: TPoint read FCellSpan write FCellSpan;
    property Checked: Boolean read FChecked write FChecked;
    property GradientRef: string read FGradientRef write FGradientRef;
    property HAlign: TAlignment read FHAlign write FHAlign;
    property HTMLText: string read FHTMLText write FHTMLText;
    property HTML: TList<THTMLRef> read FHTML write FHTML;
    property IsCheckbox: Boolean read FIsCheckbox write FIsCheckbox;
    property IsMerged: Boolean read FIsMerged write FIsMerged;
    property ImageType: TPdfCellImageType read FImageType write FImageType;
    property ImageHAlign: TPdfCellHAlign read FImageHAlign write FImageHAlign;
    property ImageVAlign: TPdfCellVAlign read FImageVAlign write FImageVAlign;
    property Page: Integer read FPage write FPage;
    property Text: string read FText write FText;
    property TextFont: TFont read FFont write FFont;
    property Url: string read FUrl write FUrl;
    property UrlColor: TColor read FUrlColor write FUrlColor;
    property VAlign: TPdfVerticalAlignment read FVAlign write FVAlign;
  end;

  // Table row of cells

  TPDFTableRowCellItems = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TPDFTableRowCellItem;
    procedure SetItem(Index: Integer; const Value: TPDFTableRowCellItem);
  public
    constructor Create(AOwner: TPDFTableRowItem);
    destructor Destroy; override;

    function Add: TPDFTableRowCellItem;
    function Insert(Index: Integer): TPDFTableRowCellItem;
    property Items[Index: Integer]: TPDFTableRowCellItem read GetItem write SetItem; default;
  end;

  // Table row

  TPDFTableRowItem = class(TCollectionItem)
  private
    FCells: TPDFTableRowCellItems;
    FHeight: Integer;
    FPageBreakAfter: Boolean;
    procedure SetCells(const Value: TPDFTableRowCellItems);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    property Cells: TPDFTableRowCellItems read FCells write SetCells;
    property Height: Integer read FHeight write FHeight;
    property PageBreakAfter: Boolean read FPageBreakAfter write FPageBreakAfter;
  end;

  // Table rows collection

  TPDFTableRowItems = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TPDFTableRowItem;
    procedure SetItem(Index: Integer; const Value: TPDFTableRowItem);
  public
    constructor Create(AOwner: TPDFTableItem);
    destructor Destroy; override;

    function Add: TPDFTableRowItem;
    function Insert(Index: Integer): TPDFTableRowItem;
    property Items[Index: Integer]: TPDFTableRowItem read GetItem write SetItem; default;
  end;

  // Table column

  TPDFTableColumnItem = class(TCollectionItem)
  private
    FCaption: string;
    FWidth: Integer;
    FTextFont: TFont;
    procedure SetTextFont(const Value: TFont);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    property Width: Integer read FWidth write FWidth;
    property Caption: string read FCaption write FCaption;
    property TextFont: TFont read FTextFont write SetTextFont;
  end;

  // Table columns collection

  TPDFTableColumnItems = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TPDFTableColumnItem;
    procedure SetItem(Index: Integer; const Value: TPDFTableColumnItem);
  public
    constructor Create(AOwner: TPDFTableItem);
    destructor Destroy; override;
    function Add: TPDFTableColumnItem;
    function Insert(Index: Integer): TPDFTableColumnItem;
    property Items[Index: Integer]: TPDFTableColumnItem read GetItem write SetItem; default;
  end;

  TTableType = (ttFit, ttWysiwyg);

  TPDFTableItem = class(TCollectionItem)
  private
    FColumns: TPDFTableColumnItems;
    FRows: TPDFTableRowItems;
    FX: Integer;
    FY: Integer;
    FWidth: Integer;
    FColor: TColor;
    FTableType: TTableType;
    procedure SetColumns(const Value: TPDFTableColumnItems);
    procedure SetRows(const Value: TPDFTableRowItems);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    function PDFPage: TPDFPageItem;
    property Color: TColor read FColor write FColor;
    property Columns: TPDFTableColumnItems read FColumns write SetColumns;
    property Rows: TPDFTableRowItems read FRows write SetRows;
    property TableType: TTableType read FTableType write FTableType;
    property Width: Integer read FWidth write Fwidth;
    property X: Integer read FX write FX;
    property Y: Integer read FY write FY;
  end;

  TPDFTableItems = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TPDFTableItem;
    procedure SetItem(Index: Integer; const Value: TPDFTableItem);
  public
    constructor Create(AOwner: TPDFPageItem);
    destructor Destroy; override;

    function Add: TPDFTableItem;
    function Insert(Index: Integer): TPDFTableItem;
    property Items[Index: Integer]: TPDFTableItem read GetItem write SetItem; default;
  end;

  TPdfMarges = record
    Left: Integer;
    Right: Integer;
    Top: Integer;
    Bottom: Integer;
  end;

  TImageRef = record
    ImageFileName: string;
    Image: TMemoryStream;
  end;

  TGradientPatern = record
    X1: Integer;
    X2: Integer;
    Y1: Integer;
    Y2: Integer;
    ColorFrom: TColor;
    ColorTo: TColor;
    ColorMirrorFrom: TColor;
    ColorMirrorTo: TColor;
    Orientation: Boolean;
  end;

  TTextLine = record
    PosX: Integer;
    Tw: Integer;
    FontStyles: TFontStyles;
    FontSize: Integer;
    R: String;
    G: String;
    B: String;
  end;

  TBackgroundRect = record
    PosX: Integer;
    PosY: Integer;
    Tw: Integer;
    FontSize: Integer;
    FontColor: TColor;
    BGColor: TColor;
  end;

  // PDF page within PDF document

  TPDFPageItem = class(TCollectionItem)
  private
    FFontSize: Integer;
    FContent: string;
    FTableItems: TPDFTableItems;
    FTextRise: Integer;
    FIndent: Integer;
    FBulletStyle: TPDFBulletStyle;
    FMarges: TPdfMarges;
    FBgColor: TColor;
    FIsUrl: Boolean;
    FImagesList: TList<TImageRef>;
    FLineWidth: Integer;
    FShadingCount: Integer;
    FUrl: string;
    FVLineSpacing: Integer;
    FShadingList: TList<TGradientPatern>;
    FTotalTextWidth: Integer;
    FTextLineText: string;
    FTextLines: TList<TTextLine>;
    FBackgroundRects: TList<TBackgroundRect>;
    FContentStream: TMemoryStream;
    procedure SetTableItems(const Value: TPDFTableItems);
    function AnsiBufferToUnicode(Dest: PWideChar; Source: PAnsiChar; SourceChars: Cardinal): PWideChar;
    function SetUnicodeText(IsUnicodeText: Boolean; AText: string): Boolean;
    function SetTextStyles(AFont: TFont): string;
    function SetTextLineStyles(R,G,B: string; APosX, tw: Integer; AFont: TFont): string;
    function SetTextFont(font: TpdfFont; AFont: TFont; IsUnicodeText: Boolean): string;
    function SetPosXByAlignment(APosX, X, tw, occ: Integer; Alignment: TPDFALIGNMENT): Integer;
    function SetPosXByIndent(APosX: Integer; Alignment: TPDFALIGNMENT): Integer;
    procedure SetBulletsInText(AFont: TFont);
    function SetDefaultFontName(AName: string): string;
    function SetTextBackgroundColor(AText: string; AFont: TFont; tw,occ,AposX,AposY: Integer): string;
    procedure SetMarges(const Value: TPdfMarges);
    procedure SetTextUrl(APosX, tw: Integer; AFont: TFont);
    function TagReplaceString(const Srch, Repl: string; var Dest: string): Boolean;
    function IPos(su, s: string): Integer;
    function VarPos(su, s: string; var Res: Integer): Integer;
    function Hex2Color(s: string): TColor;
    function HexVal(s: string): Integer;
    function Text2Color(s: string): tcolor;
    function IStrToInt(s: string): Integer;
  protected
    FAnsiToWide: TWordDynArray;
    FWideToAnsi: TByteDynArray;

    procedure DrawText;

    property ShadingList: TList<TGradientPatern> read FShadingList ;
    property ShadingCount: Integer read FShadingCount write FShadingCount;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    function FindOrAddUsedWideChar(aWideChar: WideChar): Integer;
    function SetCheckboxInText(X,Y: Integer; AFont: TFont; Checked: Boolean): string;

    class function GetConvertedText(AText: string;IsUnicodeText: Boolean;Font:TPdfFont): string;
    procedure AddTextLine(APosX, APosY: Integer; AText: string; AFont: TFont; Alignment: TPDFALIGNMENT = alLeft; AutomaticLineBreak: Boolean = True;IsUnicodeText: Boolean=false; AddNewPage: Boolean = True);
    function AddText(AText: string; AFont: TFont; AAlignment: TPDFALIGNMENT = alLeft; IsUnicodeText: Boolean = True): string;
    function ConvertHTMLLine(var s: string; Calc: Boolean; Cell: TPDFTableRowCellItem): string;
    function FitText(AText: string; AFont: TFont; AWidth,AHeight: Integer): string;
    function GetWidthText(const AText: String; AFont: TFont): Integer;
    function HtmlToColor(s:string;aDefault:Tcolor):TColor;
    function HtmlToPdf(s: string; Cell: TPDFTableRowCellItem): string;
    function PDFDocument: TPDFDocument;

    procedure AddNewLine;
    procedure AddBmp(APosX, APosY: Integer; AWidth, AHeight: Integer; AFileName: string; Alignment: TPDFALIGNMENT = alLeft);
    procedure AddJpeg(APosX, APosY: Integer; AWidth, AHeight: Integer; AJPEGFileName: string; Alignment: TPDFALIGNMENT = alLeft); overload;
    procedure AddJpeg(APosX, APosY, AWidth, AHeight: Integer; AJPEGImage: TMemoryStream; ImgRef: string = ''; Alignment: TPDFALIGNMENT = alLeft; AddRef: Boolean = False; IsInline: Boolean = False); overload;
    procedure AddPng(APosX, APosY: Integer; AWidth, AHeight: Integer; AFileName: string; Alignment: TPDFALIGNMENT = alLeft);
    procedure BeginLine(ATextColor: TColor);
    procedure EndLine(DoLineBreak: Boolean = True);
    procedure DrawLine(APosX, APosY, AHeight, AWidth: Single; APageIndex: Integer; ABorderColor: TColor = clBlack);

    function DrawRectangle(APosX,APosY,APosYEnd,AWidth: Integer; ABorderColor,ABackgroundColor,ABackgroundColorTo,ABackgroundMirrorColor,ABackgroundMirrorColorTo: TColor; ABackgroundOrientation: Boolean; APageIndex: Integer; AMultiPage: TMultiPageOption = mpoNone; NoBorder: Boolean = False): string; overload;
    function DrawRectangle(APosX,APosY,APosYEnd,AWidth: Integer; ABorderColor,ABackgroundColor: TColor; APageIndex: Integer; NoBorder: Boolean = False): string; overload;

    property BulletStyle: TPDFBulletStyle read FBulletStyle write FBulletStyle;
    property BackgroundColor: TColor read FBgColor write FBgColor;
    property BackgroundRects: TList<TBackgroundRect> read FBackgroundRects write FBackgroundRects;
    property Content: string read FContent write FContent;
    property ContentStream: TMemoryStream read FContentStream write FContentStream;
    property FontSize: Integer read FFontSize write FFontSize;
    property ImagesList: TList<TImageRef> read FImagesList;
    property Indenting: Integer read FIndent write FIndent;
    property IsUrl: Boolean read FIsUrl write FIsUrl;
    property Marges: TPdfMarges read FMarges write SetMarges;
    property Tables: TPDFTableItems read FTableItems write SetTableItems;
    property TextRise: Integer read FTextRise write FTextRise;
    property TextLineText: string read FTextLineText write FTextLineText;
    property TextLines: TList<TTextLine> read FTextLines write FTextLines;
    property TotalTextLineWidth: Integer read FTotalTextWidth write FTotalTextWidth;
    property Url: string read FUrl write FUrl;
    property VerticalLineSpacing: Integer read FVLineSpacing write FVLineSpacing;
  end;

  // PDF pages collection

  TPDFPageItems = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TPDFPageItem;
    procedure SetItem(Index: Integer; const Value: TPDFPageItem);
  public
    constructor Create(AOwner: TPDFDocument);
    function Add: TPDFPageItem;
    function Insert(Index: Integer): TPDFPageItem;
    property Items[Index: Integer]: TPDFPageItem read GetItem write SetItem; default;
  end;

  TPageSizes = (psA3, psA4, psA5, psLetter, psCustom);

  TPDFPageSize = class(TPersistent)
  private
    FWidth: Integer;
    FHeight: Integer;
    FPageSize: TPageSizes;
    procedure SetPageSize(const Value: TPageSizes);
    procedure SetHeight(const Value: Integer);
    procedure SetWidth(const Value: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property PageSize: TPageSizes read FPageSize write SetPageSize;
    property Height: Integer read FHeight write SetHeight;
    property Width: Integer read FWidth write SetWidth;
  end;

  // PDF Metadata

  TPDFMetaData = class(TPersistent)
  private
    FCompany: string;
    FCreateDate: TDateTime;
    FCreator: string;
    FCreatorTool: string;
    FProducer: string;
    FTitle: string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Company: string read FCompany write FCompany;
    property CreateDate: TDateTime read FCreateDate write FCreateDate;
    property Creator: string read FCreator write FCreator;
    property CreatorTool: string read FCreatorTool write FCreatorTool;
    property Producer: string read FProducer write FProducer;
    property Title: string read FTitle write FTitle;
  end;

  // PDF header of footer

  TPDFHeader = class(TPersistent)
  private
    FPageNumberFont: TFont;
    FPageNumberHAlign: TAlignment;
    FPageNumberVAlign: TVerticalAlignment;
    FText: string;
    FTextFont: TFont;
    FTextHAlign: TAlignment;
    FTextVAlign: TVerticalAlignment;
    FPageNumberVisible: Boolean;
    procedure SetPageNumberFont(const Value: TFont);
    procedure SetTextFont(const Value: TFont);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property PageNumberFont: TFont read FPageNumberFont write SetPageNumberFont;
    property PageNumberHAlign: TAlignment read FPageNumberHAlign write FPageNumberHAlign default taLeftJustify;
    property PageNumberVAlign: TVerticalAlignment read FPageNumberVAlign write FPageNumberVAlign default taAlignTop;
    property PageNumberVisible: Boolean read FPageNumberVisible write FPageNumberVisible default false;
    property Text: string read FText write FText;
    property TextFont: TFont read FTextFont write SetTextFont;
    property TextHAlign: TAlignment read FTextHAlign write FTextHAlign default taLeftJustify;
    property TextVAlign: TVerticalAlignment read FTextVAlign write FTextVAlign default taAlignTop;
  end;

  // PDF document

  TPDFDocument = class(TPersistent)
  private
    FFontFallBackIndex: integer;
    FTrueTypeFonts: TRawUTF8DynArray;
    FDC, FSelectedDCFontOld: HDC;
    FEnd: string;
    FHeight: Integer;
    FLineXPosition: Integer;
    FLineYPosition: Integer;
    FObjectCount: Integer;
    FOutput: string;
    FPages: TPDFPageItems;
    FStart: string;
    FTextWidth: Integer;
    FImageQuality: TJPEGQualityRange;
    FWidth: Integer;
    FDefaultFont: TFont;
    FFitOption: TPdfFitOptions;
    FFontsList: TList<TPDFFont>;
    FAnnots: TList<string>;
    FImagesList: TCustomImageList;
    FHeader: TPDFHeader;
    FFooter: TPDFHeader;
    FMetaData: TPDFMetaData;
    function GetFontIndexByName(Name: string;Style:TFontStyles): Integer;
    function GetFontByName(FontName: string;Style:TFontStyles):TPdfFont;
    function AddFont(FontName: string;Style:TFontStyles;Unicode: Boolean): TPdfFont;
    procedure AddFontAndUsedChar(FontName: string;Style:TFontStyles;AText:WideString);

    procedure SetPages(const Value: TPDFPageItems);
    procedure SetDefaultFont(const Value: TFont);
    procedure SetFitOption(const Value: TPdfFitOptions);
    function GenerateXrefTable: string;
    function GetXrefStart(outansi: string): string;
    procedure SetFooter(const Value: TPDFHeader);
    procedure SetHeader(const Value: TPDFHeader);
    procedure SetMetaData(const Value: TPDFMetaData);
    procedure SetImagesList(const Value: TCustomImageList);
  protected
    function FindFontIndex(Name:string):integer;
    function GetDCWithFont(TTF: TPdfFont): HDC;
    function GenerateJpeg(idx: Integer; objectindex: Integer; APage: Integer; var AMs: TMemoryStream): string;

    procedure WriteTextToStream(AText: string; var AStream: TMemoryStream);

    property Annotations: TList<string> read FAnnots write FAnnots;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;

    procedure DoSetFooter;
    procedure DoSetHeader;
    procedure GeneratePDF(AFileName: string);

    function CurrentPage: TPDFPageItem;
    function SetTextWidth(AWidth: Integer): Integer;
    property TextWidth: Integer read FTextWidth write FTextWidth;
    function TextWrap(AText: string; APosX,AWidth: Integer; AReplaceString: string; var occ: Integer;IsUnicodeText: Boolean=false;AFont: TFont=nil; IsLine: Boolean = True): string;
    function TextOccurences(const ASearch, AText: string): Integer;
    procedure GetRGBColor( AColor: TColor; var R,G,B: String);

    property ImagesList: TCustomImageList read FImagesList write SetImagesList;
    property Output: string read FOutput write FOutput;
    property DefaultFont: TFont read FDefaultFont write SetDefaultFont;
    property ImageQuality: TJPEGQualityRange read FImageQuality write FImageQuality default 100;
    property FitOption: TPdfFitOptions read FFitOption write SetFitOption;
    property Footer: TPDFHeader read FFooter write SetFooter;
    property Height: Integer read FHeight write FHeight;
    property Header: TPDFHeader read FHeader write SetHeader;
    property LineXPosition: Integer read FLineXPosition write FLineXPosition;
    property LineYPosition: Integer read FLineYPosition write FLineYPosition;
    property MetaData: TPDFMetaData read FMetaData write SetMetaData;
    property ObjectCount: Integer read FObjectCount write FObjectCount;
    property Pages: TPDFPageItems read FPages write SetPages;
    property Width: Integer read FWidth write FWidth;
  end;



implementation

{ TPDFDocument }
const
  MAX_WBITS   = 15; // 32K LZ77 window
  DEF_MEM_LEVEL = 8;

function AddRawUTF8(var Values: TRawUTF8DynArray; const Value: RawUTF8;
  NoDuplicates: Boolean = False; CaseSensitive: Boolean = True): Boolean;
var
  I: Integer;
begin
  I := Length(Values);
  SetLength(Values,i + 1);
  Values[I] := Value;
  Result := True;
end;


function EnumFontsProcW(var LogFont: TLogFontW; var TextMetric: TTextMetric;
  FontType: Integer; var List: TRawUTF8DynArray): Integer; stdcall;
// we enumerate all available fonts, whatever the charset is, because
// we may won't enumerate Arial or Times New Roman if current FCharSet is
// chinese e.g.
var
  Temp: RawUTF8;
begin
  with LogFont do
    if (FontType = TRUETYPE_FONTTYPE) and (lfFaceName[0]<>'@') then
    begin
      Temp := UTF8Encode(lfFaceName);
      if (pointer(List)=nil) or (List[high(List)] <> Temp) then
        AddRawUTF8(List, Temp, true, true);
    end;
  Result := 1;
end;

function lpfnAllocate(Size: integer): pointer; cdecl;
begin
  GetMem(Result, Size);
end;

function lpfnReAllocate(Buffer: pointer; Size: integer): pointer; cdecl;
begin
  ReallocMem(Buffer, Size);
  Result := Buffer;
end;

procedure lpfnFree(Buffer: Pointer); cdecl;
begin
  FreeMem(Buffer);
end;

function Check(const Code: Integer; const ValidCodes: array of Integer): Integer;
var
  I: Integer;
begin
  if Code = Z_MEM_ERROR then
    OutOfMemoryError;

  Result := code;
  for I := Low(ValidCodes) to High(ValidCodes) do
  begin
    if ValidCodes[I] = Code then
    begin
      Exit;
    end;
  end;

  raise Exception.CreateFmt('Error %d during zip/deflate process', [Code]);
end;

function CompressMem(src, dst: Pointer; srcLen, dstLen: Integer; CompressionLevel: Integer = 6; ZipFormat: Boolean = False) : Integer;
var
  Strm: z_stream;
  Bits: Integer;
begin
  FillChar(Strm, sizeof(z_stream), 0);
  Strm.next_in := src;
  Strm.avail_in := srcLen;
  Strm.next_out := dst;
  Strm.avail_out := dstLen; // -MAX_WBITS -> no zLib header => .zip compatible !

  if ZipFormat then
    Bits := MAX_WBITS
  else
    Bits := -MAX_WBITS;

  if deflateInit2_(Strm, CompressionLevel, Z_DEFLATED, Bits, DEF_MEM_LEVEL, Z_DEFAULT_STRATEGY, ZLIB_VERSION, sizeof(z_stream)) >= 0 then
  try
    Check(deflate(Strm,Z_FINISH), [Z_STREAM_END,Z_OK]);
  finally
    deflateEnd(Strm);
  end;

  Result := Strm.total_out;
end;

function CompressString(Value: AnsiString): AnsiString;
begin
  SetLength(Result, Length(Value));
  SetLength(Result, CompressMem(@Value[1], @Result[1], Length(Value), Length(Value), 6, True));
end;

function TPDFDocument.GetFontIndexByName(Name: string; Style: TFontStyles): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to FFontsList.Count - 1 do
  begin
     if (LowerCase(FFontsList[I].Name) = LowerCase(Name)) and (FFontsList[I].FontStyle = Style) then
       Exit(I);
  end;
end;

procedure TPDFDocument.GetRGBColor(AColor: TColor; var R, G, B: String);
var
  CL: Integer;
begin
  CL := ColorToRGB(AColor);
  FormatSettings.DecimalSeparator := '.';
  R := Format('%.2f', [GetRValue(CL)/255]);
  G := Format('%.2f', [GetGValue(CL)/255]);
  B := Format('%.2f', [GetBValue(CL)/255]);
end;

function TPDFDocument.GetFontByName(FontName: string; Style: TFontStyles): TPdfFont;
var
  I: Integer;
begin
  I := GetFontIndexByName(FontName, Style);

  Result := nil;
  if I <> -1 then
     Exit(FFontsList[I]);
end;

function TPDFDocument.AddFont(FontName: string; Style: TFontStyles; Unicode: Boolean): TPdfFont;
var
  F: TPdfFont;
begin
  Result := GetFontByName(FontName, Style);

  if not Assigned(Result) then
  begin
    f := TPdfFont.Create(Self, FontName);
    Result := f;
    Result.FontStyle := Style;
    Result.FDocucment := Self;
    FFontsList.Add(Result);
    Result.Alias := IntToStr(FFontsList.Count - 1);
    Result.Unicode := Unicode;
  end;
end;

procedure TPDFDocument.AddFontAndUsedChar(FontName: string; Style: TFontStyles; AText: WideString);
var
  font: TPdfFont;
  I: Integer;
begin
  font := AddFont(FontName, Style, true);
  for I := 1 to Length(AText) do
  begin
    font.AddUsedUnicodeChar(AText[I]);
  end;
end;

function CodePageToCharSet(CodePage: Cardinal): Integer;
begin
  case CodePage of
  932:  Result := SHIFTJIS_CHARSET;
  949:  Result := HANGEUL_CHARSET;
  936:  Result := GB2312_CHARSET;
  1255: Result := HEBREW_CHARSET;
  1256: Result := ARABIC_CHARSET;
  1253: Result := GREEK_CHARSET;
  1254: Result := TURKISH_CHARSET;
  1258: Result := VIETNAMESE_CHARSET;
  874:  Result := THAI_CHARSET;
  1250: Result := EASTEUROPE_CHARSET;
  1251: Result := RUSSIAN_CHARSET;
  1257: Result := BALTIC_CHARSET;
  else
    Result := ANSI_CHARSET; // default is iso-8859-1 = windows-1252
  end;
end;

procedure TPdfFont.Init;
var
  Size: Integer;
  AFont: TLogFontW;
  I: Integer;
begin
  FillChar(AFont,Sizeof(TLogFontW),#0);

  with AFont do
  begin
    lfHeight := -1000;
    if fsBold in FontStyle then
      lfWeight := FW_BOLD
    else
      lfWeight := FW_NORMAL;

    lfItalic := Byte(fsItalic in FontStyle);
    lfUnderline := Byte(fsUnderline in FontStyle);
    lfStrikeOut := Byte(fsStrikeOut in FontStyle);
    lfCharSet := CodePageToCharSet(0);
    for I := 0 to Length(Name) do
    begin
      lfFaceName[I] := Name[I+1];
    end;
  end;

  if FHGDI = 0 then
  begin
    FHGDI := CreateFontIndirectW(AFont);
    FDocucment.GetDCWithFont(Self);
    GetTextMetrics(FDocucment.FDC, FM);
    Size := GetOutlineTextMetrics(FDocucment.FDC, SizeOf(TOutlineTextMetric), nil);

    if FM.tmPitchAndFamily and TMPF_FIXED_PITCH = 0 then
    begin
      FFixedWidth := True;
    end;

    Dispose(FOTM);
    GetMem(FOTM, Size);
    FOTM.otmSize := Size;
    GetOutlineTextMetrics(FDocucment.FDc, Size, FOTM);
    FAscent := FOTM.otmAscent;
    fDescent := FOTM.otmDescent;
  end;

//  DeleteObject(FHGDI);
end;

procedure TPdfFont.SetCharAndWidthDic(ADictionary: TDictionary<Char, Extended>);
begin
  FCharAndWidthDic := ADictionary;
end;

function TPDFDocument.FindFontIndex(Name: string):integer;
var
  I: Integer;
  S1, S2: String;
begin
  Result := -1;
  S1 := Uppercase(ReplaceStr(Name, ' ', ''));
  for I := 0 to Length(FTrueTypeFonts) - 1 do
  begin
    S2 := Uppercase(ReplaceStr(string(FTrueTypeFonts[I]), ' ', ''));
    if S1 = S2 then
      Exit(I);
  end;
end;

constructor TPDFDocument.Create(AOwner: TComponent);
var
  Ds: string;
  LFont: TLogFontW;
  I: Integer;
begin
  inherited Create;

  ImageQuality := 100;
  FWidth := 595;
  FHeight := 842;

  FDc := CreateCompatibleDC(0);
  LFont.lfCharset := DEFAULT_CHARSET; // enumerate ALL fonts
  FillChar(LFont, SizeOf(LFont), 0);
  EnumFontFamiliesExW(FDC, LFont, @EnumFontsProcW, PtrInt(@FTrueTypeFonts), 0);
  FFontFallBackIndex := FindFontIndex('Calibri'); //Arial Unicode MS');

  if fFontFallBackIndex = -1 then
  begin
    for I := 0 to High(FTrueTypeFonts) do
      if Pos('Calibri', Lowercase(string(FTrueTypeFonts[I]))) > 0 then
      begin
        FFontFallBackIndex := I;
        Break;
      end;
  end;

  FAnnots := TList<string>.Create;
  FPages := TPDFPageItems.Create(Self);
  FFontsList := TList<TPDFFont>.Create;
  FDefaultFont := TFont.Create;
  FDefaultFont.Color := clBlack;
  FDefaultFont.Size := FONTSIZE;
  FDefaultFont.Name := 'Microsoft Sans Serif';

  FTextWidth := Self.Width;

  FOutput := EmptyStr;

  FStart := '%PDF-1.4' + sLineBreak + '%¡³Å×';
  FObjectCount := 1;

  DateTimeToString(Ds, 'yyyymmddhms', Now);

  FEnd := '%%EOF';

  FFooter := TPDFHeader.Create;
  FHeader := TPDFHeader.Create;
  FMetaData := TPDFMetaData.Create;

  // Begin the document by adding a page
  Pages.Add;
end;

function TPDFDocument.CurrentPage: TPDFPageItem;
begin
  Result := TPDFPageItem(Pages[Pages.Count - 1]);
end;

destructor TPDFDocument.Destroy;
var
  i: Integer;
begin
  ReleaseDC(0, FDC);
  ReleaseDC(0, FSelectedDCFontOld);

  FPages.Free;

  for i := 0 to FFontsList.Count - 1 do
    FFontsList.Items[i].Free;

  FFontsList.Clear;

  FFooter.Free;
  FHeader.Free;
  FMetaData.Free;

  FFontsList.Free;
  FDefaultFont.Free;
  FAnnots.Free;

  inherited;
end;

procedure TPDFDocument.DoSetFooter;
var
  X, Y: Integer;
  M: TPdfMarges;
  R: Integer;
begin
  X := 0;
  Y := 0;
  R := 0;

  TextWidth := Width;

  // Calculate text positions
  case Footer.TextHAlign of
    taLeftJustify:
    begin
      X := 15;
      if (Footer.PageNumberVisible) and (Footer.PageNumberHAlign = taLeftJustify) then
        X := X + Round(CurrentPage.GetWidthText(IntToStr(Pages.Count), Footer.PageNumberFont));
    end;
    taRightJustify:
    begin
      X := Width -  5 - Round(CurrentPage.GetWidthText(Footer.Text, Footer.TextFont));
      if (Header.PageNumberVisible) and (Header.PageNumberHAlign = taRightJustify) then
        X := X - Round(CurrentPage.GetWidthText(IntToStr(Pages.Count), Footer.PageNumberFont));
    end;
    taCenter: X := Round((Width / 2) -(CurrentPage.GetWidthText(Footer.Text, Footer.TextFont) / 2));
  end;

  case Footer.TextVAlign of
    taAlignTop: Y := 15 ;
    taAlignBottom: Y := 15 + Footer.TextFont.Size;
    taVerticalCenter: Y :=  15 + Round((Footer.TextFont.Size) / 2);
  end;

  // Draw the text
  CurrentPage.AddTextLine(X, Height - Y, Footer.Text, Footer.TextFont, alLeft, True, True, False);

//  R := 0;
  if Footer.PageNumberVisible then
  begin
    // Calculate pagenumber positions
    case Footer.PageNumberHAlign of
      taLeftJustify: X := 15;
      taRightJustify: X := Self.CurrentPage.PDFDocument.Width - 15 - Round(CurrentPage.GetWidthText(IntToStr(Pages.Count), Footer.PageNumberFont));
      taCenter: X := Round(Self.CurrentPage.PDFDocument.Width / 2) - Round(CurrentPage.GetWidthText(IntToStr(Pages.Count), Footer.PageNumberFont) / 2);
    end;

    case Footer.PageNumberVAlign of
      taAlignTop: Y := 15 ;
      taAlignBottom: Y := 15 + Footer.PageNumberFont.Size;
      taVerticalCenter: Y := 15 + Round(Footer.PageNumberFont.Size / 2);
    end;

    Y := Height - Y;

    // Draw the number
    CurrentPage.AddTextLine(X, Y, IntToStr(Pages.Count), Footer.PageNumberFont, alLeft, True, True, False);

    if Footer.PageNumberFont.Size > Footer.TextFont.Size then
      R := Footer.PageNumberFont.Size
    else
      R := Footer.TextFont.Size;

  end;

  if Pages.Count = 1 then
  begin
    M.Left := CurrentPage.Marges.Left;
    M.Right := CurrentPage.Marges.Right;
    m.Bottom := CurrentPage.Marges.Bottom + R;
    M.Top := CurrentPage.Marges.Top + Header.PageNumberFont.Size;
    CurrentPage.Marges := M;
  end;

  LineXPosition := M.Left;
end;

procedure TPDFDocument.DoSetHeader;
var
  X, Y, tw: Integer;
  M: TPdfMarges;
  R: Integer;
begin
  X := 0;
  Y := 0;

  // Calculate text positions
  if (Header.PageNumberFont.Size > Header.TextFont.Size) and (Header.PageNumberVisible) then
    R := Header.PageNumberFont.Size
  else
    R := Header.TextFont.Size;

  TextWidth := Width;

  case Header.TextHAlign of
    taLeftJustify:
    begin
      X :=  15;
      if (Header.PageNumberVisible) and (Header.PageNumberHAlign = taLeftJustify) then
        X := X + Round( CurrentPage.GetWidthText(IntToStr(Pages.Count), Header.TextFont)) + 5;
    end;
    taRightJustify:
    begin
      tw := CurrentPage.GetWidthText(Header.Text, Header.TextFont);

      X := Width - tw - CurrentPage.Marges.Left - CurrentPage.Marges.Right;
      if (Header.PageNumberVisible) and (Header.PageNumberHAlign = taRightJustify) then
        X := X - Round( CurrentPage.GetWidthText(IntToStr(Pages.Count), Header.PageNumberFont));

    end;
    taCenter:
    begin
      X := 15 + Round((Width / 2) - (CurrentPage.GetWidthText(Header.Text, Header.TextFont) / 2));
      if pages.Count > 1 then
      begin
        X := X - CurrentPage.Marges.Left;
      end;
    end;
  end;

  case Header.TextVAlign of
    taAlignTop: Y :=  15 + R;
    taAlignBottom: Y :=  15 + R;
    taVerticalCenter: Y :=  15 + Round((R) / 2);
  end;

  // Draw the text
  if X > Width then
    X :=  15;

  CurrentPage.AddTextLine(X, Y, Header.Text, Header.TextFont, alLeft, True, True, False);

  if Header.PageNumberVisible then
  begin
    // Calculate pagenumber positions
    case Header.PageNumberHAlign of
      taLeftJustify: X :=  15;
      taRightJustify: X := Width -  15 - Round( CurrentPage.GetWidthText(IntToStr(Pages.Count), Header.PageNumberFont));
      taCenter: X := Round(Width / 2) - Round( CurrentPage.GetWidthText(IntToStr(Pages.Count), Header.PageNumberFont) / 2);
    end;

    case Header.PageNumberVAlign of
      taAlignTop: Y :=  15 + R;
      taAlignBottom: Y :=  15 + R;
      taVerticalCenter: Y :=  15 + Round((R) / 2);
    end;

    // Draw the number
    if Pages.Count > 1 then
      CurrentPage.AddTextLine(X, Y, IntToStr(Pages.Count), Header.PageNumberFont, alLeft, False, True, False)
    else
    begin
      CurrentPage.AddTextLine(X, Y, IntToStr(Pages.Count), Header.PageNumberFont, alLeft, True, True, False);
      LineYPosition := LineYPosition - Header.PageNumberFont.Size;
    end;
  end;

  M.Left := CurrentPage.Marges.Left;
  M.Right := CurrentPage.Marges.Right;
  m.Bottom := CurrentPage.Marges.Bottom;
  M.Top := 15 + R + Round(1.5 * R);
  if Pages.Count = 1 then
    M.Top := 15 + R + R;
  CurrentPage.Marges := M;
  LineYPosition := M.Top;

  LineXPosition := M.Left;
end;

function TPDFDocument.GenerateJpeg(Idx, ObjectIndex: Integer; APage: Integer; var AMs: TMemoryStream): string;
var
  Len, BitDepth: Integer;
  AJpegFile: TMemoryStream;
  FPixelWidth, FPixelHeight: Integer;
  Ir: TImageRef;
begin
  Ir := Pages[APage].ImagesList.Items[idx];
  AJpegFile := ir.Image;

  Len := AJpegFile.Size;
  if not GetJpegSize(AJpegFile, FPixelWidth, FPixelHeight, BitDepth) then
    Exit; // JPEG format expected

  WriteTextToStream(IntToStr(ObjectIndex)+ ' 0 obj' + sLineBreak, AMs);
  WriteTextToStream('<<' + sLineBreak, AMs);
  WriteTextToStream('/Type              /XObject' + sLineBreak, AMs);
  WriteTextToStream('/Subtype           /Image' + sLineBreak, AMs);
  WriteTextToStream('/Filter            /DCTDecode' + sLineBreak, AMs);
  WriteTextToStream('/Height           ' + inttostr(FPixelHeight) + sLineBreak, AMs);
  WriteTextToStream('/Width            ' + inttostr(FPixelWidth) + sLineBreak, AMs);
  WriteTextToStream('/BitsPerComponent  8' + sLineBreak, AMs);
  WriteTextToStream('/Length           ' + IntToStr(Len) + sLineBreak, AMs);
  case BitDepth of
  8:  WriteTextToStream('/ColorSpace    /DeviceGray' + sLineBreak, AMs);
  24: WriteTextToStream('/ColorSpace    /DeviceRGB' + sLineBreak, AMs);
  end;

  WriteTextToStream('>>' + sLineBreak, AMs);
  WriteTextToStream('stream' + sLineBreak, AMs);

  AJpegFile.SaveToStream(Ams);
  AJpegFile.Free;

  WriteTextToStream(sLineBreak + 'endstream' + sLineBreak, AMs);
  WriteTextToStream('endobj' + sLineBreak, AMs);
end;


procedure TPDFDocument.GeneratePDF(AFileName: string);
var
  I,Y: Integer;
  MyFile : File;
  S, W, Fx, ImgX: string;
  Annotref, Annots: string;
  MetaRef, PageReferences: string;
  PatternRefs, Patterns, PTmp, Bounds: string;
  SBGR1, SBGR2, SBGR3, SBGR4, SBGG1, SBGG2, SBGG3, SBGG4,  SBGB1, SBGB2, SBGB3, SBGB4: string;
  CL: TColor;
  Mewo, Fg: Integer;
  Imgstr: string;
  Outansi: ansistring;
  Outstr: string;
  Ls, Os, Cs:integer;
  Md: string;
  Annot: string;
  P: TPDFPageItem;
  Shading: TGradientPatern;
  Dt: TDateTime;

  Ms, TMs: TMemoryStream;
  TmpStr: TBytes;
  sa: ansistring;
  ds,ts: char;
begin
  ds := FormatSettings.DecimalSeparator;
  ts := FormatSettings.ThousandSeparator;

  FObjectCount := 0;
  Ms := TMemoryStream.Create;
  TMs := TMemoryStream.Create;

  try
    FOutput := FStart + sLineBreak;
    FOutput := FOutput + '1 0 obj' + sLineBreak;
    FOutput := FOutput + '<</Type/Catalog/PageLayout/SinglePage/Pages 2 0 R /Metadata {METAREF} /AcroForm<</Fields[]>>>>' + sLineBreak;
    FOutput := FOutput + 'endobj' + sLineBreak;

    Inc(FObjectCount);
    FOutput := FOutput + '2 0 obj' + sLineBreak;
    FOutput := FOutput + '<</Type/Pages/Kids[{PAGEREFERENCES}]/Count ' + IntToStr(Pages.Count) + '>>' + sLineBreak;
    FOutput := FOutput + 'endobj' + sLineBreak;
    Inc(FObjectCount);
    Inc(FObjectCount);

    Fx := '';
    for I := 0 to FFontsList.Count - 1 do
    begin
      W := '';
      // 909[207]941[488]997[525]821[638]
      if TPdfFont(FFontsList[I]).FWinUnicodeUsed <> nil then
        for Mewo := 0 to length(TPdfFont(FFontsList[I]).FWinUnicodeUsed) do
        begin
          Fg := TPdfFont(FFontsList[I]).FindOrAddUsedWideChar(TPdfFont(FFontsList[I]).FWinUnicodeUsed[Mewo]);
          if TPdfFont(FFontsList[I]).FFixedWidth then
          begin
            W := W + IntToStr(TPdfFont(FFontsList[I]).FUsedWide[Fg].Glyph) + '[' + IntToStr(600) + ']'
          end
          else
          begin
            W := W + IntToStr(TPdfFont(FFontsList[I]).FUsedWide[Fg].Glyph) + '[' + IntToStr(TPdfFont(FFontsList[I]).FUsedWide[Fg].Width) + ']';
          end;
        end;

        WriteTextToStream(Format('%s 0 obj' + sLineBreak +
        '<</Type/Font/BaseFont/%s/Subtype/Type0/Encoding/Identity-H/Name/%s/DescendantFonts[%s 0 R]>>'
        + sLineBreak + 'endobj' + sLineBreak + '%s 0 obj' + sLineBreak +
        '<</Type/Font/Subtype/CIDFontType2/BaseFont/%s/CIDSystemInfo<</Supplement 0/Ordering(Identity)/Registry(Adobe)>>/DW 278/W [%s]/FontDescriptor %s 0 R>>'
        + sLineBreak + 'endobj' + sLineBreak, [IntToStr(FObjectCount),
        StripSpaces(TPdfFont(FFontsList[I]).Name), 'FU' + IntToStr(I),
        IntToStr(FObjectCount + 1), IntToStr(FObjectCount + 1),
        StripSpaces(TPdfFont(FFontsList[I]).Name), W, IntToStr(FObjectCount + 2)]), TMs);

        WriteTextToStream(Format('%s 0 obj'+  sLineBreak +
          '<</Type/FontDescriptor/FontName/%s/Ascent %s/CapHeight 666/Descent %s/ItalicAngle 0/StemV 87/Flags 32/FontBBox[%s %s %s %s]/FontFile2 %s 0 R>>'+  sLineBreak+
          'endobj'+  sLineBreak,
          [
            IntToStr(FObjectCount + 2),
            StripSpaces(TPdfFont(FFontsList[I]).Name),
            IntTostr(TPdfFont(FFontsList[I]).FAscent),
            IntTostr(TPdfFont(FFontsList[I]).fDescent),
            IntTostr(TPdfFont(FFontsList[I]).FOTM.otmrcFontBox.Left),
            IntTostr(TPdfFont(FFontsList[I]).FOTM.otmrcFontBox.Bottom),
            IntTostr(TPdfFont(FFontsList[I]).FOTM.otmrcFontBox.Top),
            IntTostr(TPdfFont(FFontsList[I]).FOTM.otmrcFontBox.Right),
            IntToStr(FObjectCount + 3)
           ]
      ), TMs);

      Sa := TPdfFont(FFontsList[I]).GetTTFXData(Self);
      Os := Length(sa);

      sa := CompressString(sa);
      Ls := Length(sa);

      WriteTextToStream(Format('%s 0 obj'+ sLineBreak +
          '<< /Length %s /Length1 %s /Filter /FlateDecode >>' + sLineBreak +
          'stream' + sLineBreak,[IntToStr(FObjectCount + 3),IntToStr(Ls), IntToStr(Os)]), TMs);

      // Tms.Write(@AnsiString(s)[1], Ls);
      Tms.WriteBuffer(sa[1], Ls);

      WriteTextToStream(sLineBreak + 'endstream' + sLineBreak +
          'endobj' + sLineBreak, TMs);

      FX := FX + '/FU' + IntToStr(I) + ' ' + IntToStr(FObjectCount) + ' 0 R/F' +
        IntToStr(I) + ' ' + IntToStr(FObjectCount + 1) + ' 0 R';
      FObjectCount := FObjectCount + 4;
    end;

    // Meta Data
    WriteTextToStream(IntToStr(FObjectCount)+ ' 0 obj' + sLineBreak, TMs);
    WriteTextToStream('<</Type/Metadata /Subtype/XML /Length ' + IntToStr(Length(Md)) + '>>', TMs);
    WriteTextToStream('stream' + sLineBreak, TMs);
    WriteTextToStream('<?xpacket begin="ï»¿"  id="W5M0MpCehiHzreSzNTczkc9d"?>' + sLineBreak +
            '<?adobe-xap-filters esc="CRLF"?>' + sLineBreak +
            '<x:xmpmeta xmlns:x="adobe:ns:meta/"  x:xmptk="XMP toolkit 2.9.1-14,  framework 1.6">' + sLineBreak +
            '<rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#" xmlns:iX="http://ns.adobe.com/iX/1.0/">' + sLineBreak +
              '<rdf:Description rdf:about="uuid:b8659d3a-369e-11d9-b951-000393c97fd8"' + sLineBreak +
                ' xmlns:pdf="http://ns.adobe.com/pdf/1.3/"' + sLineBreak +
                ' pdf:Producer="' + MetaData.Producer + '">' + sLineBreak +                                                            // Producer
              '</rdf:Description>' + sLineBreak +
              '<rdf:Description rdf:about="uuid:b8659d3a-369e-11d9-b951-000393c97fd8"' + sLineBreak +
                ' xmlns:xap="http://ns.adobe.com/xap/1.0/"' + sLineBreak, TMs);
    Dt := MetaData.CreateDate;
    WriteTextToStream(' xap:CreateDate="' + FormatDateTime('yyyy-mm-dd"T"hh:nn:ss"Z"', Dt) + '"' + sLineBreak +                                // CreateDate  (2004-11-14T08:41:16Z)
               ' xap:ModifyDate="' + FormatDateTime('yyyy-mm-dd"T"hh:nn:ss-hh:nn', Dt) + '"' + sLineBreak +                             // ModifyDate  (2004-11-14T16:38:50-08:00)
               ' xap:CreatorTool="' + MetaData.CreatorTool + '"' + sLineBreak +                                                        // CreatorTool
               ' xap:MetadataDate="' + FormatDateTime('yyyy-mm-dd"T"hh:nn:ss-hh:nn', Dt) + '">' + sLineBreak +                          // MetadataDate (2004-11-14T16:38:50-08:00)
              '</rdf:Description>' + sLineBreak +
              '<rdf:Description rdf:about="uuid:b8659d3a-369e-11d9-b951-000393c97fd8"' + sLineBreak +
                ' xmlns:xapMM="http://ns.adobe.com/xap/1.0/mm/"' + sLineBreak +
                ' xapMM:DocumentID="uuid:919b9378-369c-11d9-a2b5-000393c97fd8"/>' + sLineBreak +
              '<rdf:Description rdf:about="uuid:b8659d3a-369e-11d9-b951-000393c97fd8"' + sLineBreak +
              ' xmlns:dc="http://purl.org/dc/elements/1.1/"' + sLineBreak +
              ' dc:format="application/pdf">' + sLineBreak +
                '<dc:description><rdf:Alt>' + sLineBreak +                                                                              // Description
              '<rdf:li xml:lang="x-default">' + MetaData.Title + '</rdf:li>' + sLineBreak +
              '</rdf:Alt></dc:description>' + sLineBreak +
                '<dc:creator>' + sLineBreak +
              '<rdf:Seq>' + sLineBreak +
                '<rdf:li>' + MetaData.Creator + '</rdf:li>' + sLineBreak +
              '</rdf:Seq>' + sLineBreak +
            '</dc:creator>' + sLineBreak +
                '<dc:title> <rdf:Alt>' + sLineBreak +
              '<rdf:li xml:lang="x-default">' + MetaData.Title + '</rdf:li> </rdf:Alt>' + sLineBreak +                                  // Title
                '</dc:title></rdf:Description>' + sLineBreak +
              '</rdf:RDF>' + sLineBreak +
            '</x:xmpmeta>' + sLineBreak +
            '<?xpacket end="w"?>', TMs);
    WriteTextToStream('endstream' + sLineBreak, TMs);
    WriteTextToStream('endobj' + sLineBreak, TMs);
    FOutput:=  ReplaceStr(FOutput,'{METAREF}',IntToStr(FObjectCount)+ ' 0 R');
    Inc(FObjectCount);
    Inc(FObjectCount);

    annots := '';
    annotref := '';
    for annot in Annotations do
    begin
      annots := Annots + ' ' + IntToStr(FObjectCount) + string(Annot);
      annotref := Annotref + ' ' + IntToStr(FObjectCount) + ' 0 R ';
      Inc(FObjectCount);
    end;

    pageReferences := '';
    patternRefs := '';
    patterns := '';

    //Loop for pages
    for I := 0 to Pages.Count - 1 do
    begin
      P := Pages.Items[I];

      ImgX := '';
      for Y := 0 to P.FImagesList.Count - 1 do
      begin
        GenerateJpeg(Y, FObjectCount, I, TMS);
        ImgX := ImgX + '/Im' + IntToStr(Y + 1) + ' ' + inttostr(FObjectCount) + ' 0 R' + sLineBreak;
        Inc(FObjectCount);
      end;

      PageReferences := PageReferences + IntToStr(FObjectCount) + ' 0 R ';

      // Page References
      WriteTextToStream(sLineBreak, TMs);
      WriteTextToStream(' ' + IntToStr(FObjectCount) +' 0 obj' + sLineBreak, TMs);
      Inc(FObjectCount);
      WriteTextToStream(' << /Type /Page /Annots[' + Annotref + '] /Parent 2 0 R /MediaBox[0 0 ' + IntToStr(Width) + ' ' + IntToStr(Height) + '] /Resources<</Font<<' + FX + '>>/Pattern<<', TMs);

      patternRefs := '';
      Y := 0;
      for Shading in Pages[I].FShadingList do
      begin
        // Build the references string
        PatternRefs := PatternRefs +
        '/LGradient' + IntToStr(Y) + sLineBreak +
        '<<' + sLineBreak +
          '/Type /Pattern'+ sLineBreak +
          '/PatternType 2' + sLineBreak +
          '/Shading ' + IntToStr(FObjectCount) + ' 0 R' + sLineBreak +
        '>>';

        // Build the referenced objects

        // Cell 1st shading color
        GetRGBColor(Shading.ColorFrom, SBGR1, SBGG1, SBGB1);

        // Cell 2nd shading color
        GetRGBColor(Shading.ColorTo, SBGR2, SBGG2, SBGB2);

        ptmp := '   <<' + sLineBreak +
                  '     /FunctionType 2' + sLineBreak +
                  '     /Domain [0 1]' + sLineBreak +
                  '     /N 1' + sLineBreak +
                  '     /C0 [' + SBGR1 + ' ' + SBGG1 + ' ' + SBGB1 + ']' + sLineBreak +
                  '     /C1 [' + SBGR2 + ' ' + SBGG2 + ' ' + SBGB2 + ']' + sLineBreak +
                  '   >>' + sLineBreak;
        Bounds := '1';
        if Shading.ColorMirrorTo <> clNone then
        begin
          Bounds := '0.5';
          // Cell 3th shading color
          GetRGBColor(Shading.ColorMirrorFrom, SBGR3, SBGG3, SBGB3);

          // Cell 4th shading color
          GetRGBColor(Shading.ColorMirrorTo, SBGR4, SBGG4, SBGB4);

          PTmp := '   <<' + sLineBreak +
                  '     /FunctionType 2' + sLineBreak +
                  '     /Domain [0 1]' + sLineBreak +
                  '     /N 1' + sLineBreak +
                  '     /C0 [' + SBGR3 + ' ' + SBGG3 + ' ' + SBGB3 + ']' + sLineBreak +
                  '     /C1 [' + SBGR4 + ' ' + SBGG4 + ' ' + SBGB4 + ']' + sLineBreak +
                  '   >>' + sLineBreak;
        end;

        Patterns := Patterns + IntToStr(FObjectCount) +
        ' 0 obj' + sLineBreak +
        '<<' + sLineBreak +
        ' /ShadingType 2' + sLineBreak +
        ' /Extend [false false]' + sLineBreak +
        ' /ColorSpace /DeviceRGB' + sLineBreak +
        ' /Coords [' + IntToStr(Shading.X1) + ' ' + IntToStr(Shading.Y1) + ' ' + IntToStr(Shading.X2) + ' ' +IntToStr(Shading.Y2) + ']' + sLineBreak +
        ' /Function ' + sLineBreak +
        ' <<' + sLineBreak +
        '   /FunctionType 3' + sLineBreak +
        '   /Domain [0 1]' + sLineBreak +
        '   /Bounds [' + Bounds + ']' + sLineBreak +
        '   /Encode [0 1 0 1]' + sLineBreak +
        '   /Functions' + sLineBreak +
        '   [' + sLineBreak +
        '   <<' + sLineBreak +
        '     /FunctionType 2' + sLineBreak +
        '     /Domain [0 1]' + sLineBreak +
        '     /N 1' + sLineBreak +
        '     /C0 [' + SBGR1 + ' ' + SBGG1 + ' ' + SBGB1 + ']' + sLineBreak +
        '     /C1 [' + SBGR2 + ' ' + SBGG2 + ' ' + SBGB2 + ']' + sLineBreak +
        '   >>' + sLineBreak
        + PTmp +
        '   ]' + sLineBreak +
        ' >>' + sLineBreak +
        '>>' + sLineBreak +
        'endobj' + sLineBreak ;

        Inc(Y);
        Inc(FObjectCount);
      end;
      WriteTextToStream(PatternRefs + '>>/XObject<<' + imgX + '>>/ProcSet[/PDF/Text/ImageC]>>/Contents ' + IntToStr(FObjectCount) + ' 0 R >>' + sLineBreak, TMs);
      WriteTextToStream('endobj' + sLineBreak, TMs);

      // Page Content
      WriteTextToStream(IntToStr(FObjectCount) + ' 0 obj' + sLineBreak, TMs);
      if P.ContentStream.Size > 0 then
      begin
        WriteTextToStream(' << /Length ' +  IntToStr(P.ContentStream.Size + Length(P.Content)) + '>>', TMs);
        WriteTextToStream('stream' + sLineBreak, TMs);
        P.ContentStream.SaveToStream(TMs);
        if Length(P.Content) > 0 then
          WriteTextToStream(sLineBreak + P.Content + sLineBreak, TMs);
      end
      else
      begin
        WriteTextToStream(' << /Length ' +  IntToStr(Length(P.Content)) + '>>', TMs);
        WriteTextToStream('stream' + sLineBreak + P.Content + sLineBreak, TMs);
      end;
      WriteTextToStream('endstream' + sLineBreak, TMs);
      WriteTextToStream('endobj' + sLineBreak, TMs);
      Inc(FObjectCount);
    end;

    FOutput := StringReplace(FOutput,'{PAGEREFERENCES}', PageReferences, [rfReplaceAll]);
    FOutput := ReplaceStr(FOutput, '{FONTS}', Fx);
    FOutput := ReplaceStr(FOutput, '{ANNOTS}', Annotref);

    WriteTextToStream(FOutput, Ms);
    TMs.SaveToStream(Ms);

    // Page Initialization

    Outstr := FOutput;
    WriteTextToStream(annots + patterns, Ms);

    WriteTextToStream(GenerateXrefTable, Ms);

    WriteTextToStream('trailer' + sLineBreak, Ms);
    WriteTextToStream('<</Root 1 0 R /Size ' + IntToStr(FObjectCount) + '>>' + sLineBreak, Ms);

    WriteTextToStream(GetXrefStart(Outstr), Ms);

    WriteTextToStream(FEnd, Ms);

    Ms.SaveToFile(AFileName);
  finally
    TMs.Free;
    Ms.Free;
    FormatSettings.DecimalSeparator := ds;
    FormatSettings.ThousandSeparator := ts;
  end;
end;

function TPDFDocument.GenerateXrefTable: string;
var
  Xref, P: string;
  I: Integer;
begin
  Exit('xref' + sLineBreak + '0' + sLineBreak);
  Xref := 'Xref' + sLineBreak;
  Xref := Xref + '0 ' + IntToStr(FObjectCount) + sLineBreak;
  Xref := Xref + '0000000000 65535 f ' + sLineBreak;

  for I := 1 to FObjectCount - 1 do
  begin
    p := IntToStr(Pos( IntToStr(I) + ' 0 obj', Foutput)-1);
    p := StringOfChar( '0', 10 - Length(P) ) + P + ' 00000 n ';
    Xref := Xref + P + sLineBreak;
  end;

  Result := Xref;
end;

function TPDFDocument.GetXrefStart(OutAnsi: string): string;
begin
  Result := 'startxref ' + sLineBreak + IntToStr(Pos( 'xref', OutAnsi)) + sLineBreak;
end;

procedure TPDFDocument.SetDefaultFont(const Value: TFont);
begin
  FDefaultFont.Assign(Value);
end;

procedure TPDFDocument.SetFitOption(const Value: TPdfFitOptions);
begin
  FFitOption := Value;
end;

procedure TPDFDocument.SetFooter(const Value: TPDFHeader);
begin
  FFooter.Assign(Value);

  DoSetFooter;

  LineYPosition := CurrentPage.Marges.Top;
end;

procedure TPDFDocument.SetHeader(const Value: TPDFHeader);
begin
  FHeader.Assign(Value);
  DoSetHeader;
end;

procedure TPDFDocument.SetImagesList(const Value: TCustomImageList);
begin
  FImagesList := (Value);
end;

procedure TPDFDocument.SetMetaData(const Value: TPDFMetaData);
begin
  FMetaData.Assign(Value);
end;

procedure TPDFDocument.SetPages(const Value: TPDFPageItems);
begin
  FPages.Assign(Value);
end;

function TPDFDocument.SetTextWidth(AWidth: Integer): Integer;
begin
  FTextWidth := AWidth;
  Result := AWidth;
end;

function TPDFDocument.TextOccurences(const ASearch, AText: string): Integer;
var
  Offset: Integer;
begin
  Result := 0;
  Offset := PosEx(ASearch, AText, 1);
  while offset <> 0 do
  begin
    Inc(Result);
    Offset := PosEx(ASearch, AText, Offset + Length(ASearch));
  end;
end;

function TPDFDocument.TextWrap(AText: string; APosX, AWidth: Integer; AReplaceString: string; var occ: Integer; IsUnicodeText: Boolean = False; AFont: TFont=nil; IsLine: Boolean = True): string;
var
  I, Y, Tw: Integer;
  ATest, Tmp, Ct: string;
  PdfFont: TPdfFont;
begin
  if AFont = nil then
    AFont := DefaultFont;

  if LineXPosition = 0 then
    LineXPosition := CurrentPage.Marges.Left;

  IsLine := IsLine and (Tmp = '');

  Tmp := '';
  occ := 0;
  PdfFont := AddFont(AFont.Name, AFont.Style, IsUnicodeText);
  PdfFont.Init;

  while (AText <> '') do
  begin
    i := 1;
    ATest := '';

    while (I < Length(AText)) do
    begin
      while (I < Length(AText)) and (AText[I] <> ' ') do
      begin
        Inc(I);
      end;

      Ct := Copy(AText, 1, I);
      Tw := Round(CurrentPage.GetWidthText(Ct, AFont));
      ATest := '';
      Ct := Copy(AText, I + 1, Length(AText));
      Y := 1;
      while (Y < Length(Ct)) and (Ct[Y] <> ' ') do
      begin
        ATest := ATest + Ct[Y];
        Inc(Y);
      end;

      Tw := Tw + Round(CurrentPage.GetWidthText(ATest, AFont));
      if APosX + Tw > AWidth then
      begin
       break;
      end;

      Inc(i);
    end;

    ATest := Copy(AText, 1, I);
    AText := Copy(AText, I + 1, 65535);

    AddFontAndUsedChar(AFont.Name, AFont.Style, ATest);

    if (IsLine)  then
    begin
        if (atest <> ' ') then
        begin
          Tmp := Tmp + #10 + Format(AReplaceString,[
                                                    IntToStr(APosX),
                                                    IntToStr(Height - LineYPosition),
                                                    TPDFPageItem.GetConvertedText(Copy(ATest, 0, 65535), IsUnicodeText, PdfFont)
                                                   ]);
        end;
    end
    else
    begin
      tmp := tmp + #10 + Format(AReplaceString,[TPDFPageItem.GetConvertedText(Copy(ATest, 0, 65535), IsUnicodeText, PdfFont)]);
    end;

    LineYPosition:=(LineYPosition + Round((AFont.Size * 1.5)));

    APosX := CurrentPage.Marges.Left;

    Inc(occ);
  end;

  if occ = 0 then
    occ := 1;

  if occ > 1 then
  begin
    LineXPosition := CurrentPage.Marges.Left;
    LineYPosition:=(LineYPosition - Round((AFont.Size * 1.5)));
  end;

  LineXPosition := LineXPosition + Round(CurrentPage.GetWidthText(ATest, AFont));

  Result:=tmp;
end;

procedure TPDFDocument.WriteTextToStream(AText: string; var AStream: TMemoryStream);
var
  B: TBytes;
begin
  B := TEncoding.UTF8.GetBytes(AText);
  AStream.WriteBuffer(B[0], Length(B));
end;

//------------------------------------------------------------------------------

{ TPDFPageItems }

function TPDFPageItems.Add: TPDFPageItem;
begin
  Result := TPDFPageItem(inherited Add);
end;

constructor TPDFPageItems.Create(AOwner: TPDFDocument);
begin
  inherited Create(AOwner, TPDFPageItem);
end;

function TPDFPageItems.GetItem(Index: Integer): TPDFPageItem;
begin
  Result := TPDFPageItem(inherited Items[Index]);
end;

function TPDFPageItems.Insert(Index: Integer): TPDFPageItem;
begin
  Result := TPDFPageItem(inherited Insert(Index));
end;

procedure TPDFPageItems.SetItem(Index: Integer; const Value: TPDFPageItem);
begin
  inherited Items[Index] := Value;
end;

//------------------------------------------------------------------------------

{ TPDFPageItem }

class function TPDFPageItem.GetConvertedText(AText: string; IsUnicodeText: Boolean; Font: TPdfFont): string;
const
  HexChars: array[0..15] of Char = '0123456789ABCDEF';

  function AddHex4(aWordValue: Cardinal) : String;
  var
    v: Cardinal;
    B: String;
  begin
    v := aWordValue shr 8;
    aWordValue := aWordValue and $ff;
    B := '    ';
    B[1] := HexChars[v shr 4];            // MSB stored first (BigEndian)
    B[2] := HexChars[v and $F];
    B[3] := HexChars[aWordValue shr 4];   // LSB stored last (BigEndian)
    B[4] := HexChars[aWordValue and $F];
    result := B;
  end;

var
  lntext: string;
  I: Integer;
  isinUnicode: Boolean;
  glyph: Cardinal;

begin
  lntext:='';

  isinUnicode := IsUnicodeText;

  if IsInUnicode then
  begin
    lntext := '<';//+#254#255;
    for I := 1 to Length(AText) do
    begin
      if High(Font.fUsedWide) > 0 then
      begin
        glyph := Font.fUsedWide[Font.FindOrAddUsedWideChar(AText[I])].Glyph;
        lntext := lntext + AddHex4(Cardinal(glyph));
      end
      else
        lntext := lntext + AText[i];
    end;
    lntext := lntext + '>';
    Result := lntext;
  end else
  begin
    AText := StringReplace(AText, ')', '\)', [rfReplaceAll]);
    AText := StringReplace(AText, '(', '\(', [rfReplaceAll]);
    Result := '(' + AText + ')';
  end;
end;

procedure TPDFPageItem.AddPng(APosX, APosY, AWidth, AHeight: Integer; AFileName: string; Alignment: TPDFALIGNMENT = alLeft);
var
  Jpg: TJPegImage;
  Ms: TMemoryStream;
  Ir: TImageRef;
begin
  Jpg := PNGtoJpg(AFileName, PDFDocument.ImageQuality);

  Ms := TMemoryStream.Create;
  Jpg.SaveToStream(Ms);

  Jpg.Free;

  AddJpeg(APosX, APosY, AWidth, AHeight, Ms, '', Alignment);

  Ir.ImageFileName := AFileName;
  Ir.Image := Ms;
  ImagesList.Add(Ir);
end;

procedure TPDFPageItem.AddBmp(APosX, APosY, AWidth, AHeight: Integer; AFileName: string; Alignment: TPDFALIGNMENT = alLeft);
var
  Jpg: TJPegImage;
  Ms: TMemoryStream;
  Ir: TImageRef;
begin
  Jpg := BMPtoJpg(AFileName, PDFDocument.ImageQuality);

  Ms := TMemoryStream.Create;
  Jpg.SaveToStream(Ms);

  Jpg.Free;

  AddJpeg(APosX, APosY, AWidth, AHeight, Ms, '', Alignment);

  Ir.ImageFileName := AFileName;
  Ir.Image := Ms;
  ImagesList.Add(Ir);
end;

procedure TPDFPageItem.AddJpeg(APosX, APosY, AWidth, AHeight: Integer; AJPEGImage: TMemoryStream; ImgRef: string = ''; Alignment: TPDFALIGNMENT = alLeft; AddRef: Boolean = False; IsInline: Boolean = False );
var
  fPixelWidth, fPixelHeight, BitDepth: Integer;
  res: Boolean;
  Ir: TImageRef;
  dpi: Integer;
  dpiScale: Single;
  bmp: TBitmap;
begin
  res := GetJpegSize(AJpegImage,fPixelWidth,fPixelHeight,BitDepth);

  if not res then
    Exit; // JPEG format expected, if not, skip

  if (AHeight + APosY > PDFDocument.Height) then
  begin
    PDFDocument.Pages.Add;
    PDFDocument.CurrentPage.Marges := Marges;
    PDFDocument.DoSetFooter;
    PDFDocument.DoSetHeader;

    APosX := PDFDocument.CurrentPage.Marges.Left;
    APosY := PDFDocument.CurrentPage.Marges.Top;
  end;

  bmp := TBitmap.Create;
  dpi := GetDeviceCaps(bmp.Canvas.Handle, LOGPIXELSX);
  bmp.Free;
  dpiScale := 1;
  case dpi of
  120: dpiScale := 1.25;
  144: dpiScale := 1.5;
  end;

  if AWidth = 0 then
    AWidth := fPixelWidth;

  if AHeight = 0 then
    AHeight := fPixelHeight;

  if (ImgRef <> '') and AddRef then
  begin
    Ir.ImageFileName := ImgRef;
    Ir.Image := AJpegImage;
    PDFDocument.CurrentPage.ImagesList.Add(Ir);
  end;

  AWidth := Round((AWidth*dpiScale) * 0.77);
  AHeight := Round((AHeight*dpiScale) * 0.77);

  if (AWidth + APosX > PDFDocument.Width + 2) then
  begin
    AddNewLine;
    APosX := PDFDocument.LineXPosition;
    APosY := PDFDocument.LineYPosition;
  end
  else
  begin
    case Alignment of
    alLeft:
      if (IsInline) then
        APosX := APosX + PDFDocument.LineXPosition + AWidth
    ;
    alRight:
    begin
      APosX := PDFDocument.Width  - APosX - AWidth - Marges.Right;
    end;
    alCenter:
    begin
      APosX := Round((PDFDocument.Width  -  APosX - AWidth) / 2 );
    end;
  end;

    APosX := APosX + Indenting;
  end;

  PDFDocument.CurrentPage.FContent := PDFDocument.CurrentPage.FContent + 'q' + sLineBreak;
  PDFDocument.CurrentPage.FContent := PDFDocument.CurrentPage.FContent + IntToStr(AWidth)+ ' 0 0 ' + IntToStr(AHeight)+ ' ' +IntToStr(APosX) +' '  + IntToStr(PDFDocument.Height - APosY - AHeight) + ' cm' + sLineBreak;
  PDFDocument.CurrentPage.FContent := PDFDocument.CurrentPage.FContent + '/Im' + IntToStr(PDFDocument.CurrentPage.ImagesList.Count + 1) + ' Do' + sLineBreak;
  PDFDocument.CurrentPage.FContent := PDFDocument.CurrentPage.FContent + 'Q' + sLineBreak;

  PDFDocument.LineXPosition := PDFDocument.LineXPosition + AWidth;
  if(NOT IsInline) then
  begin
    PDFDocument.LineYPosition := PDFDocument.LineYPosition + AHeight;
  end
  else
  begin
    PDFDocument.CurrentPage.TotalTextLineWidth := PDFDocument.CurrentPage.TotalTextLineWidth + PDFDocument.LineXPosition + AWidth;
  end;

end;


procedure TPDFPageItem.AddNewLine;
begin
  PDFDocument.LineYPosition := (PDFDocument.LineYPosition + Round((PDFDocument.CurrentPage.FontSize) * 1.5));
  PDFDocument.LineXPosition := Marges.Left;

  if PDFDocument.LineYPosition > PDFDocument.Height - PDFDocument.CurrentPage.Marges.Bottom then
  begin
    PDFDocument.Pages.Add;
    PDFDocument.CurrentPage.Marges := Marges;
    PDFDocument.DoSetFooter;
    PDFDocument.DoSetHeader;
    PDFDocument.LineYPosition := PDFDocument.CurrentPage.Marges.Top;
  end;
end;

procedure TPDFPageItem.AddJpeg(APosX, APosY: Integer; AWidth, AHeight: Integer; AJPEGFileName: string; Alignment: TPDFALIGNMENT = alLeft);
var
  aJpegFile: TMemoryStream;
  Ir: TImageRef;
begin
  aJpegFile := TMemoryStream.Create;
  AJPEGFile.LoadFromFile(AJPEGFileName);

  AddJpeg(APosX, APosY, AWidth, AHeight, ajpegfile, '', Alignment);

  ir.ImageFileName := AJPegFileName;
  ir.Image := aJpegFile;
  ImagesList.Add(Ir);
end;

function TPDFPageItem.AddText(AText: string; AFont: TFont; AAlignment: TPDFALIGNMENT = alLeft; IsUnicodeText: Boolean = True): string;
var
  Tw: Integer;
  R, G, B : String;
  DrawStr, Stl, Udl, Tf, Bgc, Rise, Part: String;
  Font: TPdfFont;
  M: TPdfMarges;
  Tl: TTextLine;
  Bgr: TBackgroundRect;
begin

  if PDFDocument.FindFontIndex(AFont.Name)=-1 then
  begin
    AFont.Name := string(PDFDocument.FTrueTypeFonts[PDFDocument.fFontFallBackIndex]);
  end;

  SelectObject(PDFDocument.FDc, AFont.Handle);

  // For the moment we set unicode standard to true
  IsUnicodeText := True;
  IsUnicodeText := SetUnicodeText(IsUnicodeText, AText);

  // Check for bullets
  SetBulletsInText(AFont);

  PDFDocument.GetRGBColor(AFont.Color, R, G, B);

  // Bold / Italic / BoldItalic
  Stl := SetTextStyles(AFont);

  AFont.Name := SetDefaultFontName(AFont.Name);

  // Set the Font
  Font := PDFDocument.AddFont(AFont.Name, AFont.Style, True);
  Font.Init;
  PDFDocument.AddFontAndUsedChar(AFont.Name,AFont.Style, AText);
  Tf := SetTextFont(Font, AFont, IsUnicodeText) + sLineBreak;

  Udl := '';
  Bgc := '';

  while AText <> '' do
  begin
    DrawStr := '';
    PDFDocument.CurrentPage.FontSize := AFont.Size;

    Part := Copy(AText, 0, Pos(' ', AText));
    if Part = '' then
      Part := AText;

    AText := StringReplace(AText, Part, '', []);

    Tw := PDFDocument.CurrentPage.GetWidthText(Part, AFont);

    case AAlignment of
      TPdfAlignment.alLeft: ;
      TPdfAlignment.alRight: PDFDocument.LineXPosition := PDFDocument.Width - PDFDocument.CurrentPage.TotalTextLineWidth - PDFDocument.CurrentPage.Marges.Right;
      TPdfAlignment.alCenter: PDFDocument.LineXPosition := Round(((PDFDocument.Width) / 2) - (PDFDocument.CurrentPage.TotalTextLineWidth / 2));
    end;

    if (TotalTextLineWidth + tw > PDFDocument.Width - Marges.Left - Marges.Right - 15) and (Length(part) > 0) then
    begin
      begin
        PDFDocument.CurrentPage.EndLine;
        PDFDocument.CurrentPage.BeginLine(AFont.Color);
        FContent := FContent + Tf;
      end;
    end;

    // Underline / StrikeOut
    if (fsUnderline in AFont.Style) or (fsStrikeOut in AFont.Style) then
    begin
      Tl.PosX := PDFDocument.CurrentPage.TotalTextLineWidth;
      Tl.Tw := Tw;
      Tl.FontStyles := AFont.Style;
      Tl.FontSize := AFont.Size;
      Tl.R := R;
      Tl.G := G;
      Tl.B := B;
      PDFDocument.CurrentPage.TextLines.Add(Tl);
    end;

    // Make url if neccessairy
    if FUrl <> '' then
    begin
      PDFDocument.CurrentPage.SetTextUrl(TotalTextLineWidth, Tw, AFont);
    end;

    DrawStr := DrawStr + R + ' ' + G + ' ' + B + ' rg' + sLineBreak;
    DrawStr := DrawStr + PDFDocument.CurrentPage.GetConvertedText(Part, IsUnicodeText, Font);

    // Set the Background Color
    if (BackgroundColor <> clNone) and (BackgroundColor <> clWhite) then
    begin
      Bgr.PosX := PDFDocument.CurrentPage.TotalTextLineWidth;
      Bgr.PosY := PDFDocument.LineYPosition;
      Bgr.Tw := Tw;
      Bgr.FontSize := AFont.Size;
      Bgr.FontColor := AFont.Color;
      Bgr.BGColor := BackgroundColor;
      PDFDocument.CurrentPage.BackgroundRects.Add(Bgr);
    end;

    PDFDocument.CurrentPage.TotalTextLineWidth := PDFDocument.CurrentPage.TotalTextLineWidth + Tw;

    Rise := IntToStr(PDFDocument.CurrentPage.TextRise) + ' Ts' + sLineBreak;

    Bgc := '';

    if (PDFDocument.LineYPosition > PDFDocument.Height - PDFDocument.CurrentPage.Marges.Bottom - 5) and (Length(part) > 0) then
    begin                   
      PDFDocument.CurrentPage.DrawText;
      PDFDocument.CurrentPage.Content := PDFDocument.CurrentPage.Content + 'ET' + sLineBreak;
      PDFDocument.Pages.Add;
      PDFDocument.LineXPosition := Marges.Left;
      PDFDocument.LineYPosition := 15;
      M.Left := 15;
      M.Right := 15;
      M.Top := 15;
      M.Bottom := 15;
      PDFDocument.CurrentPage.Marges := M;
      PDFDocument.DoSetFooter;
      PDFDocument.DoSetHeader;
      PDFDocument.CurrentPage.Content := PDFDocument.CurrentPage.Content + 'ET' + sLineBreak;
      PDFDocument.LineXPosition := PDFDocument.CurrentPage.Marges.Left;
      PDFDocument.LineYPosition := PDFDocument.CurrentPage.Marges.Top;
      PDFDocument.CurrentPage.BeginLine(AFont.Color);
      PDFDocument.CurrentPage.AddText(Part + AText, AFont);
      Exit;
    end;

    PDFDocument.CurrentPage.TextLineText := PDFDocument.CurrentPage.TextLineText + Bgc + tf + Rise + DrawStr + ' Tj' + sLineBreak;
  end;

  Result := Udl;
end;

procedure TPDFPageItem.AddTextLine(APosX, APosY: Integer; AText: string; AFont: TFont; Alignment: TPDFALIGNMENT = alLeft; AutomaticLineBreak: Boolean = True; IsUnicodeText: Boolean=false; AddNewPage: Boolean = True);
var
  tf, tmp, stl, udl: string;
  tw,occ, tmpX: Integer;
  R, G, B : string;
  X,Y: Integer;
  font: TPdfFont;
  Bytes: TBytes;
begin
  if AText = '' then
    Exit;

  if PDFDocument.FindFontIndex(AFont.Name)=-1 then
  begin
    AFont.Name := string(PDFDocument.FTrueTypeFonts[PDFDocument.fFontFallBackIndex]);
  end;

  SelectObject(PDFDocument.FDC, AFont.Handle);
  tmpX := APosX;

  // For the moment we set unicode standard to true
  IsUnicodeText := true;
  IsUnicodeText := SetUnicodeText(IsUnicodeText, AText);

  // Check for bullets
  SetBulletsInText(AFont);

  // Set the Vars
  occ := 1;
  tmp := '';
  X := APosX;
  Y := APosY;

  PDFDocument.GetRGBColor(AFont.Color, R, G, B);

  // Bold / Italic / BoldItalic
  stl := SetTextStyles(AFont);

  AFont.Name := SetDefaultFontName(AFont.Name);

//   AFont.Name := StripSpaces(AFont.Name) + stl;

  // Set the Font
  font := PDFDocument.AddFont(AFont.Name, AFont.Style, True);
  tf := SetTextFont(font, AFont, IsUnicodeText);

  tw := GetWidthText(AText, AFont);

  // Position Calculations
  PDFDocument.LineYPosition := APosY;
  APosX := SetPosXByAlignment(APosX, X, tw, occ, Alignment);
  APosX := SetPosXByIndent(APosX, Alignment);

  // Underline / StrikeOut
  udl := SetTextLineStyles(R, G, B, APosX, tw, AFont);

  // Make url if neccessairy
  if FUrl <> '' then
  begin
    SetTextUrl(APosX, tw, AFont);
  end;

  tmp := tmp + 'BT' + sLineBreak;
  tmp := tmp + '%s %s' +  ' Td' + sLineBreak;
  tmp := tmp + tf + sLineBreak;
  tmp := tmp + R + ' ' + G + ' ' + B + ' rg' + sLineBreak;
  tmp := tmp + '%s Tj' + sLineBreak;
  tmp := tmp + 'ET' + sLineBreak;

  // Calculate Y with Sub/SuperScript
  PDFDocument.LineYPosition := PDFDocument.LineYPosition - TextRise;

  // Process the text
  tmp := PDFDocument.TextWrap(AText, APosX, PDFDocument.TextWidth, tmp, occ,IsUnicodeText,AFont);

  // Set the Background Color
  if BackgroundColor <> clNone then
    SetTextBackgroundColor(AText, AFont, tw, occ, AposX, APosY);

  // Reset Y with Sub/SuperScript
  PDFDocument.LineYPosition := PDFDocument.LineYPosition + TextRise;

  // Store the size for later use
  FFontSize := AFont.Size;

  // X & Y persisting
  if not (AutomaticLineBreak) and (occ <= 1) then
    PDFDocument.LineYPosition := Y
  else
    PDFDocument.LineYPosition := PDFDocument.LineYPosition + VerticalLineSpacing;

  if PDFDocument.LineYPosition > PDFDocument.Height then
  begin
    Bytes := TEncoding.UTF8.GetBytes(tmp + udl);
    FContentStream.WriteBuffer(Bytes[0], Length(Bytes));

    if AddNewPage then
    begin
      PDFDocument.Pages.Add;
      PDFDocument.CurrentPage.Marges := Marges;
      PDFDocument.DoSetFooter;
      PDFDocument.DoSetHeader;
      PDFDocument.SetTextWidth(PDFDocument.Width);
      PDFDocument.CurrentPage.AddTextLine(tmpx, PDFDocument.LineYPosition , AText, AFont, Alignment, AutomaticLineBreak, IsUnicodeText);
    end;
  end
  else
  begin
    /// Put it all together
    Bytes := TEncoding.UTF8.GetBytes(tmp + udl);
    FContentStream.WriteBuffer(Bytes[0], Length(Bytes));
  end;
end;

function TPDFPageItem.SetDefaultFontName(AName: string): string;
begin
  AName := StringReplace(AName, 'Bold', '', [rfReplaceAll]);
  AName := StringReplace(AName, 'Italic', '', [rfReplaceAll]);
  AName := StringReplace(AName, 'Oblique', '', [rfReplaceAll]);
  Result := AName;
end;

procedure TPDFPageItem.SetMarges(const Value: TPdfMarges);
begin
  FMarges := Value;
  PDFDocument.LineXPosition := PDFDocument.LineXPosition + FMarges.Left;
end;

procedure TPDFPageItem.SetBulletsInText(AFont: TFont);
var
  font: TFont;
  fn,chr: string;
  Y,X,nd: Integer;
  bs: TPDFBulletStyle;
begin
  if BulletStyle = bsNone then
    Exit;

  nd := Indenting;
  bs := BulletStyle;
  fn := AFont.Name;
  font := PDFDocument.DefaultFont;
  font.Name := 'ArialBold';

  Indenting := 30;
  X := -3;
  Y := 0;

  case BulletStyle of
    bsNone: Indenting := nd;
    bsStar:
    begin
      X := -2;
      Y := PDFDocument.LineYPosition + 2;
      chr := '* ';
    end;
    bsArrow:
    begin
      X := -9;
      Y := PDFDocument.LineYPosition;
      chr := '→  ';
    end;
    bsSquare:
    begin
      Y := PDFDocument.LineYPosition;
      chr := '■  ';
    end;
    bsCheck:
    begin
      Y := PDFDocument.LineYPosition;
      chr := 'ѵ ';
    end;
    bsCircle:
    begin
      Y := PDFDocument.LineYPosition;
      chr := '●  ';
    end;
  end;

  BulletStyle := bsNone;
  AddTextLine(X, Y, chr , font, alLeft, False);
  font.Name := fn;
  AddTextLine(0, PDFDocument.LineYPosition, ' ', font, alLeft, False);
  PDFDocument.LineXPosition := PDFDocument.LineXPosition + 4;
  BulletStyle := bs;
end;

function TPDFPageItem.SetCheckboxInText(X,Y: Integer; AFont: TFont; Checked: Boolean): string;
var
  font: TFont;
  fn, chr: string;
  fs: Integer;
begin
  fn := AFont.Name;
  fs := AFont.Size;
  font := PDFDocument.DefaultFont;
  font.Name := 'Arial';

  Indenting := 0;

  if not Checked then
  begin
    font.Size := 17;
    Y := Y + 1;
    chr := '□ ';
    AddTextLine(X - 3, Y, chr , font, alLeft, False);
  end
  else
  begin
    font.Size := 17;
    Y := Y + 1;
    chr := '□ ';
    AddTextLine(X - 3, Y, chr , font, alLeft, False);

    font.Size := 6;
    Y := Y - 2;
    X := X + 3;
    chr := 'ѵ ';
    AddTextLine(X - 3, Y, chr , font, alLeft, False);
  end;

  font.Name := fn;
  font.Size := fs;
end;

function TPDFPageItem.SetTextBackgroundColor(AText: string; AFont: TFont; tw, occ, AposX,AposY: Integer): string;
var
  R, G, B, temp: string;
  W, H: Integer;
begin
  temp := '';
  PDFDocument.GetRGBColor(BackgroundColor, R, G, B);

  if R + G + B = '1.001.001.00' then
    Exit('');

  tw := tw + 1;

  temp := temp + ' ' + R + ' ' + G + ' ' + B + ' rg' + sLineBreak;

  // move the line
  if occ > 1 then
  begin
    W := APosX - 2;
    H := PDFDocument.Height - AposY + Round(AFont.Size * (occ+1)) ;
  end
  else
  begin
    W := APosX - 2;
    H := PDFDocument.Height - AposY + Round(AFont.Size) ;
  end;

  temp := temp + ' ' + IntToStr(W) + ' ' + IntToStr(H) + ' m' + sLineBreak;

  // go right
  if occ > 1 then
  begin
    W := PDFDocument.Width - APosX - Round(Marges.Right * 2) - Round(Marges.Left * 2);
    H := H;
  end
  else
  begin
    W := APosX + tw;
    H := H;
  end;
  temp := temp + ' ' + IntToStr(W) + ' ' + IntToStr(H) + ' l' + sLineBreak;

  // go down
  if occ > 1 then
  begin
    W := W ;
    H := H - (AFont.Size * (occ+1) ) - 5;
  end
  else
  begin
    W := W;
    H := H - AFont.Size - 5;
  end;
    temp := temp + ' ' + IntToStr(W) + ' ' + IntToStr(H) + ' l' + sLineBreak;

  // go left
  W := AposX - 2;
  temp := temp + ' ' + IntToStr(W) + ' ' + IntToStr(H) + ' l' + sLineBreak;

  // Fill it
  temp := temp + 'f' + sLineBreak ;

  if AFont.color < 0 then
    AFont.color := clBlack;

  PDFDocument.GetRGBColor(AFont.Color, R, G, B);

  Result := temp;

  FContent := FContent + temp;
end;

function TPDFPageItem.SetUnicodeText(IsUnicodeText: Boolean; AText: string): Boolean;
var
  I: Integer;
begin
  if not IsUnicodeText then
  begin
    for I := 1 to Length(AText) do
    begin
      if (Word(AText[I]) > 255) then
        IsUnicodeText := true;
    end;
  end;

  Result := IsUnicodeText;
end;

function TPDFPageItem.SetTextStyles(AFont: TFont): string;
var
  stl: string;
begin
  stl := EmptyStr;

  if fsBold in Afont.Style then
    stl := stl + 'Bold'
  else
    stl := stl + '';

  if fsItalic in Afont.Style then
    stl := stl + 'Italic'
  else
    stl := stl + '';

  Result := stl;
end;

function TPDFPageItem.SetTextLineStyles(R, G, B: string; APosX, tw: Integer; AFont: TFont): string;
var
  udl: string;
begin
  udl := EmptyStr;

  if fsUnderline in AFont.Style then
  begin
    udl := udl + R + ' ' + G + ' ' + B + ' rg' + sLineBreak;
    udl := udl + IntToStr(AposX) + ' ' + IntToStr(PDFDocument.Height - PDFDocument.LineYPosition - 2 + TextRise) + ' m' + sLineBreak;
    udl := udl + IntToStr(AposX + Round(tw)) + ' ' + IntToStr(PDFDocument.Height - PDFDocument.LineYPosition - 2 + TextRise) + ' l' + sLineBreak;
    udl := udl + 'f' + sLineBreak;
  end;

  if fsStrikeOut in AFont.Style then
  begin
    udl := udl + R + ' ' + G + ' ' + B + ' rg' + sLineBreak;
    udl := udl + IntToStr(AposX) + ' ' + IntToStr(PDFDocument.Height - PDFDocument.LineYPosition + TextRise + Round(AFont.Size/4)) + ' m' + sLineBreak;
    udl := udl + IntToStr(AposX + Round(tw)) + ' ' + IntToStr(PDFDocument.Height - PDFDocument.LineYPosition + TextRise + Round(AFont.Size/4)) + ' l' + sLineBreak;
    udl := udl + 's' + sLineBreak;
  end;

  Result := udl;
end;

procedure TPDFPageItem.SetTextUrl(APosX, tw: Integer; AFont: TFont);
var
  url, L, R ,T, B: string;
  RR, GG, BB: string;
begin
  if FUrl = '' then
    Exit;

  FormatSettings.DecimalSeparator := '.';

  L := Format('%.2f', [APosX - 2 + 0.00]);
  T := Format('%.2f', [PDFDocument.Height - PDFDocument.LineYPosition - 4 + 0.00]);
  R := Format('%.2f', [APosX + tw + 1.50]);
  B := Format('%.2f', [PDFDocument.Height - PDFDocument.LineYPosition + AFont.Size + 8 + 0.00]);

  PDFDocument.GetRGBColor(AFont.Color, RR, GG, BB);

  url :=  ' 0 obj' +  sLineBreak +
  '<<' + sLineBreak +
  '/Rect[ ' + L + ' ' + T + ' ' + R + ' ' + B + ']' + sLineBreak +
  '/Subtype' + sLineBreak +
  '/Link' + sLineBreak +
  '/P 3 0 R ' + sLineBreak +
  '/F 4' + sLineBreak +
  '/Rotate 0' + sLineBreak +
  '/BS' + sLineBreak +
  '<<' + sLineBreak +
  '/W 0' + sLineBreak +
  '/S' + sLineBreak +
  '/S' + sLineBreak +
  '>>' + sLineBreak +
  '/C[ ' + RR + ' ' + GG + ' ' + BB + ']' + sLineBreak +
  '/H' + sLineBreak +
  '/N' + sLineBreak +
  '/A ' + sLineBreak +
  '<</Type/Action/S/URI/URI(' + FUrl + ')>>' + sLineBreak +
  '>>' + sLineBreak +
  'endobj' +  sLineBreak ;

  PDFDocument.Annotations.Add(url);
  PDFDocument.ObjectCount := PDFDocument.ObjectCount + 1;
end;

function TPDFPageItem.SetTextFont(font: TpdfFont; AFont: TFont; IsUnicodeText: Boolean): string;
var
  tf, FontName: string;
begin
  tf := EmptyStr;

  if IsUnicodeText then
    FontName := 'FU' + font.Alias
  else
    FontName := 'F' + font.Alias;

  // Correcting Text adjustments
  if AFont.Name <> '' then
    tf := '/' + FontName
  else
    tf := '/F1';

  if AFont.Size <> FONTSIZE then
    tf := tf + ' ' + IntToStr(AFont.Size) + ' Tf'
  else
    tf := tf + ' ' + IntToStr(FONTSIZE) + ' Tf';

  Result := tf;
end;

function TPDFPageItem.SetPosXByAlignment(APosX, X, tw, occ: Integer; Alignment: TPDFALIGNMENT): Integer;
begin
  case Alignment of
    alRight:
    begin
      if occ > 1 then
      begin
        APosX := PDFDocument.Width - APosX - Round((tw) / occ) - Marges.Right;
      end
      else
      begin
        APosX := PDFDocument.Width  -  X - Round(tw) - Marges.Right;
      end;
    end;
    alCenter:
    begin
      if occ > 1 then
      begin
        APosX := Round((PDFDocument.Width - APosX - Round((tw/occ))) / 2);
      end
      else
      begin
        APosX := Round((PDFDocument.Width  -  X - Round(tw)) / 2 );
      end;
    end;
  end;

  Result := APosX;
end;

function TPDFPageItem.SetPosXByIndent(APosX: Integer; Alignment: TPDFALIGNMENT): Integer;
begin
  case Alignment of
    alLeft:
      APosX := APosX + Indenting;
    alRight:
      APosX := APosX - Indenting - 15;
    alCenter:
      APosX := APosX + Indenting;
  end;

  Result := APosX;
end;

function TPDFPageItem.FindOrAddUsedWideChar(aWideChar: WideChar): Integer;
begin
  result := ord(aWideChar);
  if result < 0 then begin
    result := -(result + 1); // this WideChar was already existing -> return index
    exit;
  end;
end;

const
  HexChars: array[0..15] of AnsiChar = '0123456789ABCDEF';

function TPDFPageItem.GetWidthText(const AText: String; AFont: TFont) : Integer;
var
  LBmp: TBitmap;
  Font: TPdfFont;
  Width, Sum, tmp: Extended;
  I: Integer;
begin

  Font := PDFDocument.GetFontByName(AFont.Name, AFont.Style);
  Result := 0;
  Sum := 0;
  if Font <> nil then
  begin
    for I := 1 to Length(AText) do
    begin
      if (Font.FCharAndWidthDic.TryGetValue(AText[I], tmp)) then
        Width := Font.FCharAndWidthDic.Items[AText[I]]
      else
        Width := Font.FCharAndWidthDic.Items[' '];

      Sum := Sum + Width;
    end;
  end
  else
  begin
    LBmp := TBitmap.Create;
    try
     LBmp.Canvas.Font := AFont;
     Result := Round(LBmp.Canvas.TextWidth(AText) * 0.76);
    finally
      LBmp.Free;
    end;
  end;

  if Font <> nil then
    Result := Round(((Sum * AFont.Size*10)/ 3.5) * 0.72);
end;

function TPDFPageItem.HtmlToColor(s: string; aDefault: TColor): TColor;
begin
  if Copy(s, 1, 1) = '#' then
  begin
    s := '$' + Copy(s, 6, 2) + Copy(s, 4, 2) + Copy(s, 2, 2);
  end
  else
    s := 'clNone';
  try
    Result := StringToColor(s);
  except
    Result := aDefault;
  end;
end;

function TPDFPageItem.HtmlToPdf(s: string; Cell: TPDFTableRowCellItem): string;
var
  su: string;
  FoundTag: Boolean;
begin
  if Pos('&',s) > 0 then
  begin
    repeat
      Foundtag := False;

      if TagReplacestring('&amp;','&&',s) then Foundtag := True;
      if TagReplacestring('&quot;','"',s) then Foundtag := True;

      if TagReplacestring('&sect;','§',s) then Foundtag := True;
      if TagReplacestring('&permil;','®‰',s) then Foundtag := True;
      if TagReplacestring('&reg;','®',s) then Foundtag := True;

      if TagReplacestring('&copy;','©',s) then Foundtag := True;
      if TagReplacestring('&para;','¶',s) then Foundtag := True;

      if TagReplacestring('&trade;','™',s) then Foundtag := True;
      if TagReplacestring('&euro;','€',s) then Foundtag := True;

    until not Foundtag;
  end;

  su := '';
  while Length(s) > 0 do
  begin
    su := su + ConvertHTMLLine(s,True, Cell);
  end;

  Result := su;
end;

function TPDFPageItem.ConvertHTMLLine(var s: string; Calc: Boolean; Cell: TPDFTableRowCellItem): string;
var
  su,Res,TagProp,Prop,Tagp,LineText: string;
  cr: TRect;
  linebreak,imgbreak: Boolean;
  TagPos,SpacePos: Integer;
  WordLen: Integer;
  TagChar: Char;
  LengthFits: Boolean;
  Anchor: Boolean;
  LastAnchor, AnchorText: string;
  IMGSize: TPoint;
  isSup,isSub,isImg: Boolean;
  subh,suph,imgali: Integer;
  ListIndex: Integer;
  Invisible: Boolean;
  FrstBullet: Boolean;
  Pic: TPicture;
  DefFont: TFont;
  html: THTMLRef;
  bmp: TBitmap;
begin
  Anchor := False;
  FrstBullet := False;
  Invisible := False;
  ListIndex := 0;
  suph := 0;
  subh := 0;
  imgali := 0;
  isSup := False;
  isSub := False;
  SpacePos := 0;

  Result := '';
  LineText := '';

  linebreak := false;
  imgbreak := false;
  res := '';

  DefFont := TFont.Create;
  DefFont.Assign(Cell.TextFont);
  DefFont.Color := clBlack;
  DefFont.Size := 8;
  DefFont.Style := [];
  DefFont.Name := Cell.TextFont.Name;

  html.BGColor := clNone;
  while (length(s) > 0) and not linebreak and not imgbreak do
  begin
    isImg := False;
    html.HasImage := False;
    // get next word or till next HTML tag
    TagPos := Pos('<', s);

    if (TagPos > 0) and ((SpacePos > TagPos) or (SpacePos = 0)) then
    begin
      su := Copy(s, 1, TagPos - 1);
    end
    else
    begin
      if SpacePos > 0 then
        su := Copy(s, 1, SpacePos)
      else
        su := s;
    end;

    WordLen := length(su);

    while Pos('&nbsp;', su) > 0 do
    begin
      TagReplaceString('&nbsp;', ' ', su);
    end;

    while Pos('&lt;', su) > 0 do
    begin
      TagReplaceString('&lt;', '<', su);
    end;

    while Pos('&gt;', su) > 0 do
    begin
      TagReplaceString('&gt;', '>', su);
    end;

    if WordLen > 0 then
    begin
      if Invisible then
        Delete(s, 1, WordLen);

      if not Invisible then
      begin
        // draw mode
        if not Calc then
        begin
          if isSup then
            cr.Bottom := cr.Bottom - suph;
          if isSub then
            cr.Bottom := cr.Bottom + subh;

          cr.Bottom := cr.Bottom - imgali;

        end
        else
        begin
          html.IsUrl := Anchor;
          html.Text := su;
          html.FontName := Cell.TextFont.Name;
          if html.Size <= 0 then
            html.Size := Cell.TextFont.Size;
          html.HasImage := isImg;

          if Anchor then
          begin
            AnchorText := su;
            html.Color := Cell.UrlColor;
            html.Url := LastAnchor;
          end;

          if (html.Text <> '') or (html.HasImage) then
            Cell.HTML.Add(html);
        end;

        LengthFits := True;
        LineText := LineText + su;

        if LengthFits then
        begin
          res := res + Copy(s, 1, WordLen);
          if not LengthFits and Calc and (LineText <> su) then
            s := '';
          Delete(s, 1, WordLen);
          if length(su) >= WordLen then
          begin
          end
        end
        else
        begin
          linebreak := True;
        end;
      end;
    end;

    TagPos := Pos('<', s);

    if (TagPos = 1) and (length(s) <= 2) then
      s := '';

    if not linebreak and (TagPos = 1) and (length(s) > 2) then
    begin
      if (s[2] = '/') and (length(s) > 3) then
      begin
        case UpCase(s[3]) of
          'A':
            begin
              if Anchor then
              begin
                PDFDocument.CurrentPage.Url := LastAnchor;
                Cell.Text := AnchorText;
                html.Color:= DefFont.Color;

                Anchor := false;
              end;
            end;
          'B':
            begin
              html.FontStyle := html.FontStyle - [fsBold];
            end;
          'S':
            begin
              TagChar := UpCase(s[4]);

              if (TagChar = 'U') then
              begin
                isSup := false;
                isSub := false;
              end;
              html.FontStyle := html.FontStyle + [fsStrikeout];
            end;
          'F':
            begin
              html.FontName := Cell.TextFont.Name;
              html.FontStyle := Cell.TextFont.Style;
              html.Size := Cell.TextFont.Size;
              html.Color := Cell.TextFont.Color;
            end;
          'I':
            begin
              html.FontStyle := html.FontStyle - [fsItalic];
            end;
          'L':
            begin
              linebreak := True;
              PDFDocument.CurrentPage.AddNewLine;
            end;
          'P':
            begin
              linebreak := True;
            end;
          'U':
            begin
              if (s[4] <> '>') and (ListIndex > 0) then
              begin
                FrstBullet := false;
              end
              else
                html.FontStyle := html.FontStyle - [fsUnderline];
            end;
          'R':
            begin

            end;
          'Z':
            Invisible := false;
        end;
      end
      else
      begin
        case UpCase(s[2]) of
          'A':
            begin
              TagProp := Uppercase(Copy(s, 3, Pos('>', s) - 1));
              // <A href="anchor">

              if (VarPos('HREF', TagProp, TagPos) > 0) then
              begin
                TagProp := Copy(s, 3, Pos('>', s) - 1);
                // restore case ie: without upppercase
                Prop := Copy(TagProp, TagPos + 4, length(TagProp));
                Prop := Copy(Prop, Pos('"', Prop) + 1, length(Prop));
                Prop := Copy(Prop, 1, Pos('"', Prop) - 1);
                LastAnchor := Prop;
                Anchor := True;
              end;
            end;
          'B':
            begin
              TagChar := UpCase(s[3]);
              if TagChar = '>' then // <B> tag
              begin
                html.FontStyle := html.FontStyle + [fsBold];
              end
              else if TagChar = 'R' then // <BR> tag
              begin
                linebreak := True;
//                PDFDocument.CurrentPage.AddNewLine;
                html.Y := 1;
              end
              else
              begin
                if TagChar = 'L' then // <BLINK> tag
                begin
                  // Skip
                end
                else if TagChar = 'O' then // <BODY ... >
                begin
                  res := res + Copy(s, 1, Pos('>', s));
                  TagProp := Uppercase(Copy(s, 6, Pos('>', s) - 1));

                  if (Pos('BACKGROUND', TagProp) > 0) and not Calc then
                  begin
                    Prop := Copy(TagProp, Pos('BACKGROUND', TagProp) + 10,
                      length(TagProp));
                    Prop := Copy(Prop, Pos('"', Prop) + 1, length(Prop));
                    Prop := Copy(Prop, 1, Pos('"', Prop) - 1);
                    if (Pos('://', Prop) > 0) then
                    begin
                    end;
                  end; // end of background

                  if (Pos('BGCOLOR', TagProp) > 0) then
                  begin
                    Prop := Copy(TagProp, Pos('BGCOLOR', TagProp) + 7,
                      length(TagProp));
                    Prop := Copy(Prop, Pos('"', Prop) + 1, length(Prop));
                    Prop := Copy(Prop, 1, Pos('"', Prop) - 1);
                    if not Calc then
                    begin
                      if Pos('CL', Prop) > 0 then;
                      if Pos('#', Prop) > 0 then;
                    end;
                  end;
                end;
              end;
            end;
          'H':
            begin
              case UpCase(s[3]) of
                'R':
                  begin
                    linebreak := True;
                  end;
              end;
            end;
          'I':
            begin
              TagChar := UpCase(s[3]);

              if TagChar = '>' then // <I> tag
                 html.FontStyle := html.FontStyle + [fsItalic]
              else
              if TagChar = 'N' then // <IND> tag
              begin
                TagProp := Copy(s, 3, Pos('>', s) - 1);

                Prop := Copy(TagProp, IPos('x', TagProp) + 2, length(TagProp));
                Prop := Copy(Prop, Pos('"', Prop) + 1, length(Prop));
                Prop := Copy(Prop, 1, Pos('"', Prop) - 1);
              end
              else if TagChar = 'M' then               //<IMG>
              begin
                TagProp := Uppercase(Copy(s, 3, Pos('>', s) - 1));
                Prop := Copy(TagProp, Pos('SRC', TagProp) + 4, length(TagProp));
                Prop := Copy(Prop, Pos('"', Prop) + 1, length(Prop));
                Prop := Copy(Prop, 1, Pos('"', Prop) - 1);

                if Pos('WIDTH', TagProp) > 0 then
                begin
                  Tagp := Copy(TagProp, Pos('WIDTH', TagProp) + 6,
                    length(TagProp));
                  Tagp := Copy(Tagp, Pos('"', Tagp) + 1, length(Tagp));
                  Tagp := Copy(Tagp, 1, Pos('"', Tagp) - 1);
                end;

                if Pos('HEIGHT', TagProp) > 0 then
                begin
                  Tagp := Copy(TagProp, IPos('HEIGHT', TagProp) + 7,
                    length(TagProp));
                  Tagp := Copy(Tagp, Pos('"', Tagp) + 1, length(Tagp));
                  Tagp := Copy(Tagp, 1, Pos('"', Tagp) - 1);
                end;

                IMGSize.X := 0;
                IMGSize.Y := 0;

                if Pos('IDX:', Prop) > 0 then
                begin
                  Delete(Prop, 1, 4);
                  if Assigned(Cell.Bitmaps)
                  then
                  begin
                    Pic := TPicture.Create;
                    try
                      html.HasImage:= False;

                      if Assigned(PDFDocument.ImagesList) then
                        PDFDocument.ImagesList.getIcon(IStrToInt(Prop), pic.Icon);
                      if Assigned(pic.Icon) and not pic.Icon.Empty then
                      begin
                        bmp := TBitmap.Create;
                        bmp.Width := pic.Icon.Width;
                        bmp.Height := pic.Icon.Height;
                        bmp.Canvas.Draw(0,0, pic.Icon);
                        bmp.Transparent := true;
                        Cell.Bitmaps.Add(bmp);
                        Cell.ImageHAlign := haLeft;
                        Cell.ImageVAlign := vaTop;
                        html.HasImage:= true;
                        html.Text := '';

                        html.FontName := Cell.TextFont.Name;
                        html.FontStyle := Cell.TextFont.Style;
                        html.Size := Cell.TextFont.Size;
                        html.Color := Cell.TextFont.Color;

                        cell.HTML.Add(html);
                      end;
                    finally
                      Pic.Free;
                    end;
                  end;
                end;
              end;
            end;
          'L':
            begin
              if not FrstBullet then
                PDFDocument.CurrentPage.AddNewLine
              else
                FrstBullet := false;
            end;
          'U':
            begin
              if s[3] <> '>' then
              begin
                Inc(ListIndex);
                linebreak := True;
                FrstBullet := True;
              end
              else
                html.FontStyle := html.FontStyle + [fsUnderline];
            end;
          'P':
            begin
              if (VarPos('>', s, TagPos) > 0) then
              begin
                TagProp := Uppercase(Copy(s, 3, TagPos - 1));

                if VarPos('ALIGN', TagProp, TagPos) > 0 then
                begin
                  Prop := Copy(TagProp, TagPos + 5, length(TagProp));
                  Prop := Copy(Prop, Pos('"', Prop) + 1, length(Prop));
                  Prop := Copy(Prop, 1, Pos('"', Prop) - 1);
                end;

                if VarPos('INDENT', TagProp, TagPos) > 0 then
                begin
                  Prop := Copy(TagProp, TagPos + 6, length(TagProp));
                  Prop := Copy(Prop, Pos('"', Prop) + 1, length(Prop));
                  Prop := Copy(Prop, 1, Pos('"', Prop) - 1);
                end;
                if VarPos('BGCOLOR', TagProp, TagPos) > 0 then
                begin
                  Prop := Copy(TagProp, TagPos + 5, length(TagProp));
                  Prop := Copy(Prop, Pos('"', Prop) + 1, length(Prop));
                  Prop := Copy(Prop, 1, Pos('"', Prop) - 1);

                  html.BGColor := HtmlToColor(Prop, clNone);
                end;
              end;
            end;
          'F':
            begin
              if (VarPos('>', s, TagPos) > 0) then
              begin
                TagProp := Uppercase(Copy(s, 6, TagPos - 6));

                if (VarPos('FACE', TagProp, TagPos) > 0) then
                begin
                  Prop := Copy(TagProp, TagPos + 4, length(TagProp));
                  Prop := Copy(Prop, Pos('"', Prop) + 1, length(Prop));
                  Prop := Copy(Prop, 1, Pos('"', Prop) - 1);
                  html.FontName := Prop;
                end;

                if (VarPos(' COLOR', TagProp, TagPos) > 0) then
                begin
                  Prop := Copy(TagProp, TagPos + 6, length(TagProp));
                  Prop := Copy(Prop, Pos('"', Prop) + 1, length(Prop));
                  Prop := Copy(Prop, 1, Pos('"', Prop) - 1);

                  if length(Prop) > 0 then
                  begin
                    if Prop[1] = '#' then
                      html.Color := Hex2Color(Prop)
//                      Cell.TextFont.Color := Hex2Color(Prop)
                    else
                      html.Color := Text2Color(AnsiLowerCase(Prop));
                  end;

                end;

                if (VarPos('BGCOLOR', TagProp, TagPos) > 0) and not Calc
                then
                begin
                  Prop := Copy(TagProp, TagPos + 7, length(TagProp));
                  Prop := Copy(Prop, Pos('"', Prop) + 1, length(Prop));
                  Prop := Copy(Prop, 1, Pos('"', Prop) - 1);
                end;

                if (VarPos('SIZE', TagProp, TagPos) > 0) then
                begin
                  Prop := Copy(TagProp, TagPos + 4, length(TagProp));
                  Prop := Copy(Prop, Pos('=', Prop) + 1, length(Prop));
                  Prop := Copy(Prop, Pos('"', Prop) + 1, length(Prop));

                  case IStrToInt(Prop) of
                    1:
                      html.Size := (8 * 2);
                    2:
                      html.Size := (10 * 2);
                    3:
                      html.Size := (12 * 2);
                    4:
                      html.Size := (14 * 2);
                    5:
                      html.Size := (16 * 2);
                  else
                    html.Size := (IStrToInt(Prop) {* 2});
                  end;
                end;
              end;
            end;
          'S':
            begin
              TagChar := UpCase(s[3]);

              if TagChar = '>' then
                html.FontStyle := html.FontStyle + [fsStrikeout]
              else
              begin
                if TagChar <> 'H' then
                begin
                  if IPos('<SUB>', s) = 1 then
                    isSub := True
                  else if IPos('<SUP>', s) = 1 then
                    isSup := True;
                end;
              end;
            end;
          'R':
            begin
              // Skip
            end;
          'Z':
            Invisible := True;
        end;
      end;
      if (VarPos('>', s, TagPos) > 0) and not imgbreak then
      begin
        res := res + Copy(s, 1, TagPos);
        Delete(s, 1, TagPos);
      end
      else
      begin
        if not imgbreak then
          Delete(s, 1, length(s));
      end;
    end;
  end;

  if linebreak then
  begin
    html := Cell.HTML[Cell.HTML.Count - 1];
    html.Y := 1;
    Cell.HTML[Cell.HTML.Count - 1] := html;
  end;

  Result := res;
  DefFont.Free;
end;

function TPDFPageItem.IStrToInt(s: string): Integer;
var
  Err, Res: Integer;
begin
  Val(s, Res, Err);
  Result := Res;
end;

function TPDFPageItem.Text2Color(s: string):tcolor;
begin
  Result := clBlack;

  if (s = 'clred') then Result := clred else
  if (s = 'clblack') then Result := clblack else
  if (s = 'clblue') then Result := clblue else
  if (s = 'clgreen') then Result := clgreen else
  if (s = 'claqua') then Result := claqua else
  if (s = 'clyellow') then Result := clyellow else
  if (s = 'clfuchsia') then Result := clfuchsia else
  if (s = 'clwhite') then Result := clwhite else
  if (s = 'cllime') then Result := cllime else
  if (s = 'clsilver') then Result := clsilver else
  if (s = 'clgray') then Result := clgray else
  if (s = 'clolive') then Result := clolive else
  if (s = 'clnavy') then Result := clnavy else
  if (s = 'clpurple') then Result := clpurple else
  if (s = 'clteal') then Result := clteal else
  if (s = 'clmaroon') then Result := clmaroon;

  if Result <> clBlack then Exit;

  if (s = 'clbackground') then Result := clbackground else
  if (s = 'clactivecaption') then Result := clactivecaption else
  if (s = 'clinactivecaption') then Result := clinactivecaption else
  if (s = 'clmenu') then Result := clmenu else
  if (s = 'clwindow') then Result := clwindow else
  if (s = 'clwindowframe') then Result := clwindowframe else
  if (s = 'clmenutext') then Result := clmenutext else
  if (s = 'clwindowtext') then Result := clwindowtext else
  if (s = 'clcaptiontext') then Result := clcaptiontext else
  if (s = 'clactiveborder') then Result := clactiveborder else
  if (s = 'clinactiveborder') then Result := clinactiveborder else
  if (s = 'clappworkspace') then Result := clappworkspace else
  if (s = 'clhighlight') then Result := clhighlight else
  if (s = 'clhighlighttext') then Result := clhighlighttext else
  if (s = 'clbtnface') then Result := clbtnface else
  if (s = 'clbtnshadow') then Result := clbtnshadow else
  if (s = 'clgraytext') then Result := clgraytext else
  if (s = 'clbtntext') then Result := clbtntext else
  if (s = 'clinactivecaptiontext') then Result := clinactivecaptiontext else
  if (s = 'clbtnhighlight') then Result := clbtnhighlight else
  if (s = 'cl3ddkshadow') then Result := clgraytext else
  if (s = 'cl3dlight') then Result := cl3dlight else
  if (s = 'clinfotext') then Result := clinfotext else
  if (s = 'clinfobk') then Result := clinfobk;
end;

function TPDFPageItem.FitText(AText: string; AFont: TFont; AWidth, AHeight: Integer): string;
var
  TW, TH, W, H: Integer;
  TL: Integer;
  R: Single;

begin
    TW := GetWidthText(AText, AFont);
    TH := Round(AFont.Size * 1.5);
    W := AWidth;
    H := AHeight;

    if (W > 0) and ((Ceil(TW/W) * TH) > H )then
    begin
      /// To Big!
      TL := Length(AText);
      R := Floor(H / TH);
      W := Floor((W * R));
      if (TL * (W/TW) * 0.85) > 0 then
        Result := Copy(AText, 0, Ceil((TL * (W/TW) * 0.85)))
      else
        Result := AText;
    end
    else
    begin
      Result := AText;
    end;
end;

function TPDFPageItem.HexVal(s: string): Integer;
var
  i,j: Integer;
begin
  if Length(s) < 2 then
  begin
    Result := 0;
    Exit;
  end;

  if s[1] >= 'A' then
    i := ord(s[1]) - ord('A') + 10
  else
    i := ord(s[1]) - ord('0');

  if s[2] >= 'A' then
    j := ord(s[2]) - ord('A') + 10
  else
    j := ord(s[2]) - ord('0');

  Result := i shl 4 + j;
end;

function TPDFPageItem.Hex2Color(s: string): TColor;
var
  r,g,b: Integer;
begin
  r := Hexval(Copy(s,2,2));
  g := Hexval(Copy(s,4,2)) shl 8;
  b := Hexval(Copy(s,6,2)) shl 16;
  Result := TColor(b + g + r);
end;

function TPDFPageItem.VarPos(su,s: string; var Res: Integer): Integer;
begin
  Res := Pos(su, s);
  Result := Res;
end;

function TPDFPageItem.IPos(su, s: string): Integer;
begin
  Result := Pos(UpperCase(su), UpperCase(s));
end;

function TPDFPageItem.TagReplaceString(const Srch, Repl: string; var Dest: string): Boolean;
var
  i: Integer;
begin
  i := IPos(srch, dest);
  if i > 0 then
  begin
    Result := True;
    Delete(Dest, i, Length(Srch));
    Dest := Copy(Dest, 1, i-1) + Repl + Copy(Dest, i, Length(Dest));
  end
  else
    Result := False;
end;

constructor TPDFPageItem.Create(Collection: TCollection);
var
  i: Integer;
  A256: array[0..256] of AnsiChar;
  U256: array[0..256] of WideChar;
  M: TPdfMarges;
begin
  inherited;

  FLineWidth := 0;

  M.Top := 15;
  M.Left := 15;
  M.Right := 15;
  M.Bottom := 15;

  FTextRise := 0;
  FContent := ' ';
  FFontSize:= PDFDocument.DefaultFont.Size;
  PDFDocument.SetTextWidth(500);
  BulletStyle := bsNone;
  FBgColor := clNone;
  FIsUrl := False;
  FUrl := '';
  FVLineSpacing := 0;
  FTotalTextWidth := 0;
  FTextLineText := '';

  PDFDocument.LineYPosition := 20;

  SetLength(fAnsiToWide,256);
  for i := 0 to 255 do
    A256[i] := AnsiChar(i);

  AnsiBufferToUnicode(U256, A256, 256);
  move(U256,fAnsiToWide[0], 256 * 2);

  SetLength(fWideToAnsi, 65536);
  fillchar(fWideToAnsi[1], 65535, ord('?'));
  for i := 1 to 255 do
    if fAnsiToWide[i]<>0 then
      fWideToAnsi[fAnsiToWide[i]] := i;

  FShadingCount := 0;

  FTableItems := TPDFTableItems.Create(Self);
  FImagesList := TList<TImageRef>.Create;
  FShadingList := TList<TGradientPatern>.Create;
  FTextLines := TList<TTextLine>.Create;
  FBackgroundRects := TList<TBackgroundRect>.Create;

  FContentStream := TMemoryStream.Create;
end;

function TPDFPageItem.AnsiBufferToUnicode(Dest: PWideChar; Source: PAnsiChar; SourceChars: Cardinal): PWideChar;
var
  c: cardinal;
begin
  // first handle trailing 7 bit ASCII chars, by quad (Sha optimization)
  if SourceChars >= 4 then
  repeat
    c := pCardinal(Source)^;
    if c and $80808080 <> 0 then
      break; // break on first non ASCII quad
    dec(SourceChars,4);
    inc(Source,4);
    pCardinal(Dest)^ := (c shl 8 or (c and $FF)) and $00ff00ff;
    c := c shr 16;
    pCardinal(Dest+2)^ := (c shl 8 or c) and $00ff00ff;
    inc(Dest,4);
  until SourceChars < 4;

  if (SourceChars>0) and (ord(Source^)<128) then
  repeat
    dec(SourceChars);
    Dest^ := WideChar(ord(Source^));
    inc(Source);
    inc(Dest);
  until (SourceChars=0) or (ord(Source^)>=128);

  // rely on the Operating System for all remaining ASCII characters
  if SourceChars = 0 then
    result := Dest
  else
  begin
    {$ifdef ISDELPHIXE} // use cross-platform wrapper for MultiByteToWideChar()
    Result := Dest+UnicodeFromLocaleChars(
      PDFDocument.Pages.Count,MB_PRECOMPOSED,Source,SourceChars,Dest,SourceChars);
    {$else}
    Result := Dest+MultiByteToWideChar(
      PDFDocument.Pages.Count,MB_PRECOMPOSED,Source,SourceChars,Dest,SourceChars);
    {$endif}
  end;

  Result^ := #0;
end;

procedure TPDFPageItem.BeginLine(ATextColor: TColor);
var
  R, G, B: String;
begin
  PDFDocument.GetRGBColor(ATextColor, R, G, B);
  Content := Content + 'BT' + sLineBreak;
  Content := Content + '{XPOSITION} ' + IntToStr(PDFDocument.Height - PDFDocument.LineYPosition) + ' Td' + sLineBreak;
  Content := Content + R + ' ' + G + ' ' + B + ' rg' + sLineBreak;
end;

destructor TPDFPageItem.Destroy;
begin
  FTableItems.Free;
  FImagesList.Free;
  FShadingList.Free;
  FTextLines.Free;
  FBackgroundRects.Free;
  FContentStream.Free;
  inherited;
end;

procedure TPDFPageItem.DrawLine(APosX, APosY, AHeight, AWidth: Single; APageIndex: Integer; ABorderColor: TColor);
var
  P: TPDFPageItem;
  R, G, B, Tmp: string;
  Bytes: TBytes;
begin
  APosY := PDFDocument.Height - APosY;
  P := PDFDocument.Pages[APageIndex];

  P.PDFDocument.GetRGBColor(ABorderColor, R,G,B);

  FormatSettings.DecimalSeparator := '.';
  Tmp := R + ' ' + G + ' ' + B + ' rg' + sLineBreak +
  Format('%.2f', [APosX]) + ' ' + Format('%.2f', [APosY]) + ' m' + sLineBreak +
  Format('%.2f', [APosX]) + ' ' + Format('%.2f', [(APosY + AHeight)]) + ' l' + sLineBreak +
  Format('%.2f', [(APosX + AWidth)]) + ' ' + Format('%.2f', [(APosY + AHeight)]) + ' l' + sLineBreak +
  Format('%.2f', [(APosX + AWidth)]) + ' ' + Format('%.2f', [APosY]) + ' l' + sLineBreak +
  'f' + sLineBreak;

  Bytes := TEncoding.UTF8.GetBytes(Tmp);
  FContentStream.WriteBuffer(Bytes[0], Length(Bytes));
end;


function TPDFPageItem.DrawRectangle(APosX,APosY,APosYEnd,AWidth: Integer; ABorderColor,ABackgroundColor: TColor; APageIndex: Integer; NoBorder: Boolean = False): string;
begin
  DrawRectangle(APosX, APosY, APosYEnd, AWidth, ABorderColor, ABackgroundColor, clNone, clNone, clNone, false, APageIndex, mpoNone, NoBorder);
end;

procedure TPDFPageItem.DrawText;
var
  TmpFont: TFont;
  I, X: Integer;
  Bgc: TColor;
begin
  TmpFont := TFont.Create;
  for I := 0 to TextLines.Count - 1 do
  begin
    TmpFont.Size := TextLines[I].FontSize;
    TmpFont.Style := TextLines[I].FontStyles;
    X := PDFDocument.LineXPosition + TextLines[I].PosX - 1;

    Content := Content + PDFDocument.CurrentPage.SetTextLineStyles(TextLines[I].R, TextLines[I].G, TextLines[I].B, X, TextLines[I].Tw, TmpFont);
  end;

  Bgc := BackgroundColor;
  for I := 0 to BackgroundRects.Count - 1 do
  begin
    TmpFont.Size := BackgroundRects[I].FontSize;
    TmpFont.Color := BackgroundRects[I].FontColor;
    X := PDFDocument.LineXPosition + BackgroundRects[I].PosX - 1;

    BackgroundColor := BackgroundRects[I].BGColor;
    Content := Content + PDFDocument.CurrentPage.SetTextBackgroundColor('', TmpFont, BackgroundRects[I].Tw, 1, X, BackgroundRects[I].PosY);
  end;
  BackgroundColor := Bgc;

  TmpFont.Free;
  TextLines.Clear;
  BackgroundRects.Clear;

  Content := StringReplace(Content, '{XPOSITION}', IntToStr(PDFDocument.LineXPosition), []);
  Content := Content + TextLineText;
  TextLineText := '';
  TotalTextLineWidth := 0;
end;

function TPDFPageItem.DrawRectangle( APosX,APosY,APosYEnd,AWidth: Integer;
  ABorderColor,ABackgroundColor,ABackgroundColorTo,ABackgroundMirrorColor,ABackgroundMirrorColorTo: TColor; ABackgroundOrientation: Boolean;
  APageIndex: Integer; AMultiPage: TMultiPageOption = mpoNone; NoBorder: Boolean = False ): string;
var
  P: TPDFPageItem;
  ShadeText: string;
  R, G, B : string;
  Y, YE: Integer;
  Shade: TGradientPatern;
begin
  Y := APosY;
  YE := APosYEnd;
  APosY := PDFDocument.Height - APosY;
  AposYEnd := PDFDocument.Height - APosYEnd;

  P := PDFDocument.Pages[APageIndex];

  if not NoBorder then
  begin
    ShadeText := '';
    {$REGION 'Gradients'}
    if ABackgroundColorTo <> clNone then
    begin
      shade.ColorFrom := ABackgroundColor;
      shade.ColorTo := ABackgroundColorTo;
      shade.ColorMirrorFrom := ABackgroundMirrorColor;
      shade.ColorMirrorTo := ABackgroundMirrorColorTo;
      shade.Orientation := ABackgroundOrientation;

      if ABackgroundOrientation then
      begin
      {$REGION 'VERTICAL'}
        shade.Y1 := AposY;
        shade.Y2 := APosYEnd;
        shade.X1 := 0;
        shade.X2 := 0;
      {$ENDREGION}
      end
      else
      begin
      {$REGION 'HORIZONTAL'}
        shade.X1 := APosX;
        shade.X2 := APosX + AWidth;
        Shade.Y1 := 0;
        shade.Y2 := 0;
      {$ENDREGION}
      end;

      P.FShadingList.Add(shade);
      ShadeText := '/Pattern cs /LGradient' + IntToStr(P.ShadingCount) + ' scn ';
      P.ShadingCount := P.ShadingCount + 1;
    end;
    {$ENDREGION}
  end;

  // Draw the background
  P.PDFDocument.GetRGBColor(ABackgroundColor, R,G,B);

  PDFDocument.WriteTextToStream(
  R + ' ' + G + ' ' + B + ' rg' + sLineBreak + ShadeText + sLineBreak +
  R + ' ' + G + ' ' + B + ' rg' + sLineBreak + ShadeText + sLineBreak +
  IntToStr(APosX) + ' ' + IntToStr(APosY) + ' m' + sLineBreak +
  IntToStr(APosX) + ' ' + IntToStr(APosYEnd) + ' l' + sLineBreak +
  IntToStr(APosX + AWidth) + ' ' + IntToStr(APosYEnd) + ' l' + sLineBreak +
  IntToStr(APosX + AWidth) + ' ' + IntToStr(APosY) + ' l' + sLineBreak +
  'f' + sLineBreak, P.FContentStream);

  // down
    P.DrawLine(APosX, Y, Y - YE, 0.5, APageIndex, ABorderColor);
  // right
  if AMultiPage <> mpoVertical then
  begin
    P.DrawLine(APosX, YE, 0.5, AWidth, APageIndex, ABorderColor);
  end;
  // up
  if AMultiPage <> mpoHorizontal then
  begin
    P.DrawLine(APosX + AWidth, Y, Y - YE, 0.5, APageIndex, ABorderColor);
  end;
  // left
    P.DrawLine(APosX, Y, 0.5, AWidth + 0.5, APageIndex, ABorderColor);

  Result := ShadeText;
end;

procedure TPDFPageItem.EndLine(DoLineBreak: Boolean = True);
begin
  PDFDocument.CurrentPage.DrawText;
  Content := Content + 'ET' + sLineBreak;

  if DoLineBreak then
    PDFDocument.CurrentPage.AddNewLine;
end;

function TPDFPageItem.PDFDocument: TPDFDocument;
begin
  Result := (Collection as TPDFPageItems).Owner as TPDFDocument;
end;

procedure TPDFPageItem.SetTableItems(const Value: TPDFTableItems);
begin
  FTableItems := Value;
end;

//------------------------------------------------------------------------------

{ TPDFTableItems }

function TPDFTableItems.Add: TPDFTableItem;
begin
  Result := TPDFTableItem(inherited Add);
end;

constructor TPDFTableItems.Create(AOwner: TPDFPageItem);
begin
  inherited Create(AOwner,TPDFTableItem);
end;

destructor TPDFTableItems.Destroy;
begin
  inherited;
end;

function TPDFTableItems.GetItem(Index: Integer): TPDFTableItem;
begin
  Result := TPDFTableItem(inherited Items[Index]);
end;

function TPDFTableItems.Insert(Index: Integer): TPDFTableItem;
begin
  Result := TPDFTableItem(inherited Insert(Index));
end;

procedure TPDFTableItems.SetItem(Index: Integer; const Value: TPDFTableItem);
begin
  inherited Items[Index] := Value;
end;

//------------------------------------------------------------------------------

{ TPDFTableItem }

constructor TPDFTableItem.Create(Collection: TCollection);
begin
  inherited;
  FColor := clBlack;
  FColumns := TPDFTableColumnItems.Create(Self);
  FRows := TPDFTableRowItems.Create(Self);
  FX := 0;
  FY := 0;
  FWidth := 150;
end;

destructor TPDFTableItem.Destroy;
begin
  FColumns.Free;
  FRows.Free;
  inherited;
end;

function TPDFTableItem.PDFPage: TPDFPageItem;
begin
  Result := (Collection as TPDFTableItems).Owner as TPDFPageItem;
end;

procedure TPDFTableItem.SetRows(const Value: TPDFTableRowItems);
begin
  FRows := Value;
end;

procedure TPDFTableItem.SetColumns(const Value: TPDFTableColumnItems);
begin
  FColumns := Value;
end;

//------------------------------------------------------------------------------

{ TPDFTableColumnItems }

function TPDFTableColumnItems.Add: TPDFTableColumnItem;
begin
  Result := TPDFTableColumnItem(inherited Add);
end;

constructor TPDFTableColumnItems.Create(AOwner: TPDFTableItem);
begin
  inherited Create(AOwner,TPDFTableColumnItem);
end;

destructor TPDFTableColumnItems.Destroy;
begin
  inherited;
end;

function TPDFTableColumnItems.GetItem(Index: Integer): TPDFTableColumnItem;
begin
  Result := TPDFTableColumnItem(inherited Items[Index]);
end;

function TPDFTableColumnItems.Insert(Index: Integer): TPDFTableColumnItem;
begin
  Result := TPDFTableColumnItem(inherited Insert(Index));
end;

procedure TPDFTableColumnItems.SetItem(Index: Integer; const Value: TPDFTableColumnItem);
begin
  inherited Items[Index] := Value;
end;

//------------------------------------------------------------------------------

{ TPDFTableColumnItem }

constructor TPDFTableColumnItem.Create(Collection: TCollection);
begin
  inherited;
  FTextFont := TFont.Create;
  FTextFont.Color := clBlack;
  FTextFont.Size := FONTSIZE;
  FTextFont.Name := 'Helvetica';
end;

destructor TPDFTableColumnItem.Destroy;
begin
  FTextFont.Free;
  inherited;
end;

procedure TPDFTableColumnItem.SetTextFont(const Value: TFont);
begin
  FTextFont.Assign(Value);
end;

//------------------------------------------------------------------------------

{ TPDFTableRowItems }

function TPDFTableRowItems.Add: TPDFTableRowItem;
begin
  Result := TPDFTableRowItem(inherited Add);
end;

constructor TPDFTableRowItems.Create(AOwner: TPDFTableItem);
begin
  inherited Create(AOwner,TPDFTableRowItem);
end;

destructor TPDFTableRowItems.Destroy;
begin
  inherited;
end;

function TPDFTableRowItems.GetItem(Index: Integer): TPDFTableRowItem;
begin
  Result := TPDFTableRowItem(inherited Items[Index]);
end;

function TPDFTableRowItems.Insert(Index: Integer): TPDFTableRowItem;
begin
  Result := TPDFTableRowItem(inherited Insert(Index));
end;

procedure TPDFTableRowItems.SetItem(Index: Integer; const Value: TPDFTableRowItem);
begin
  inherited Items[Index] := Value;
end;

//------------------------------------------------------------------------------

{ TPDFTableRowItem }

constructor TPDFTableRowItem.Create(Collection: TCollection);
begin
  inherited;
  Cells := TPDFTableRowCellItems.Create(Self);
end;

destructor TPDFTableRowItem.Destroy;
begin
  FCells.Free;
  inherited;
end;

procedure TPDFTableRowItem.SetCells(const Value: TPDFTableRowCellItems);
begin
  FCells := Value;
end;

//------------------------------------------------------------------------------

{ TPDFTableRowCellItems }

function TPDFTableRowCellItems.Add: TPDFTableRowCellItem;
begin
  Result := TPDFTableRowCellItem(inherited Add);
end;

constructor TPDFTableRowCellItems.Create(AOwner: TPDFTableRowItem);
begin
  inherited Create(AOwner,TPDFTableRowCellItem);
end;

destructor TPDFTableRowCellItems.Destroy;
begin
  inherited;
end;

function TPDFTableRowCellItems.GetItem(Index: Integer): TPDFTableRowCellItem;
begin
  Result := TPDFTableRowCellItem(inherited Items[Index]);
end;

function TPDFTableRowCellItems.Insert(Index: Integer): TPDFTableRowCellItem;
begin
  Result := TPDFTableRowCellItem(inherited Insert(Index));
end;

procedure TPDFTableRowCellItems.SetItem(Index: Integer; const Value: TPDFTableRowCellItem);
begin
  inherited Items[Index] := Value;
end;

//------------------------------------------------------------------------------

{ TPDFTableRowCellItem }

constructor TPDFTableRowCellItem.Create(Collection: TCollection);
begin
  inherited;
  FFont := TFont.Create;
  FFont.Color := clBlack;
  FUrlColor := clBlack;
  FFont.Size := FONTSIZE;
  FFont.Name := 'Helvetica';
  FAlignment := alLeft;
  FBitmaps := TObjectList<TBitmap>.Create;
  FBackgroundColor := clNone;
  FBackgroundColorTo := clNone;
  FBackgroundColorFrom := clNone;
  FBackgroundMirrorColorTo := clNone;
  FBackgroundMirrorColorFrom := clNone;
  FHTML := TList<THTMLRef>.Create;
  FUrlColor := clBlack;
end;

destructor TPDFTableRowCellItem.Destroy;
begin
  FFont.Free;
  FBitmaps.Free;
  FHTML.Free;
  inherited;
end;

//------------------------------------------------------------------------------

{ TPdfDictionaryWrapper }

function TPdfDictionaryWrapper.GetHasData: Boolean;
begin
  Result := (FData = nil);
end;

procedure TPdfDictionaryWrapper.SetData(AData: TObject);
begin
  FData := AData;
end;

//------------------------------------------------------------------------------

{ TPdfXrefEntry }

constructor TPdfXrefEntry.Create(AValue: TObject);
begin
  FByteOffset := -1;
  FObjectStreamIndex := -1;
  if AValue<>nil then begin
    FEntryType := 'n';
    FValue := AValue;
  end else
    FEntryType := 'f';
end;

destructor TPdfXrefEntry.Destroy;
begin
  if FEntryType='n' then
    FValue.Free;
  FValue.Free;
  inherited;
end;


//------------------------------------------------------------------------------

{ TPdfXref }

const
  PDF_MAX_GENERATION_NUM = 65535;
constructor TPdfXref.Create;
var RootEntry: TPdfXrefEntry;
begin
  FXrefEntries := TList.Create;
  // create first a void PDF_FREE_ENTRY as root
  RootEntry := TPdfXrefEntry.Create(nil);
  RootEntry.GenerationNumber := PDF_MAX_GENERATION_NUM;
  FXrefEntries.Add(RootEntry);
end;

destructor TPdfXref.Destroy;
var i: Integer;
begin
  for i := 0 to FXrefEntries.Count-1 do
    GetItem(i).Free;
  FXrefEntries.Free;
  inherited;
end;

function TPdfXref.GetItem(ObjectID: Integer): TPdfXrefEntry;
begin
  Result := TPdfXrefEntry(FXrefEntries[ObjectID]);
end;

function TPdfXref.GetItemCount: Integer;
begin
  Result := FXrefEntries.Count;
end;

function TPdfXref.GetObject(ObjectID: Integer): TObject;
begin
  if cardinal(ObjectID)<cardinal(FXrefEntries.Count) then
    Result := TPdfXrefEntry(FXrefEntries.List[ObjectID]).Value
  else
    Result := nil;
end;

//------------------------------------------------------------------------------

{ TPdfFont }

procedure TPdfFont.AddUsedWinAnsiChar(aChar: AnsiChar);
begin
  if Assigned(Self) then
    include(fWinAnsiUsed, aChar);
end;

procedure TPdfFont.AddUsedUnicodeChar(aChar: WideChar);
var
  I: Integer;
begin
  if Assigned(Self) then
  begin
    for I := 0 to Length(fWinUnicodeUsed)-1 do
    begin
      if aChar = FWinUnicodeUsed[I] then
        Exit;
    end;
    SetLength(fWinUnicodeUsed, Length(fWinUnicodeUsed) + 1);
    fWinUnicodeUsed[Length(fWinUnicodeUsed) - 1] := aChar;
  end;
end;

function TPdfDocument.GetDCWithFont(TTF: TPdfFont): HDC;
begin
  if not Assigned(Self) then
    Result := 0
  else
  begin
    if FSelectedDCFontOld <> 0 then // prevent resource leak
      SelectObject(FDC, FSelectedDCFontOld);
    FSelectedDCFontOld := SelectObject(FDC,TTF.FHGDI);
    Result := FDC;
  end;
end;

constructor TPdfFont.Create(Parent: TPDFDocument; const AName: string);
begin
  inherited Create;

  FEmbedFont := true;
  FDocucment := Parent;
  FName := AName;
  FData := TPdfDictionaryWrapper.Create;

  FSubType := 'Type0';
  Encoding := 'Identity-H';

  FCharAndWidthDic := TDictionary<Char, Extended>.Create;

  if Assigned(Parent) then
    TPdfTTF.Create(Self).Free; // all the magic in one line :)
end;

function TPdfFont.GetTTFXData(Doc: TPDFDocument): ansistring;
var
  used: TWordDynArray;
  I: integer;
  ttfSize: cardinal;
  TTF: PDFString;
  SubSetData: PAnsiChar;
  SubSetMem: cardinal;
  SubSetSize: cardinal;
begin
  if  EmbedFont then
  begin
    if FontSub = INVALID_HANDLE_VALUE then
    begin
      FontSub := SafeLoadLibrary('FontSub.dll');
      if FontSub <> 0 then
        CreateFontPackage := GetProcAddress(FontSub, 'CreateFontPackage');
    end;
    if (FontSub <> 0) and (@CreateFontPackage <> nil) then
    begin
      // subset magic is done by Windows (API available since XP) :)
      SetLength(used, length(FWinUnicodeUsed));

      for I := 0 to length(FWinUnicodeUsed) - 1 do
        used[I] := word(FWinUnicodeUsed[I]);
      TArray.Sort<word>(used);

      Doc.GetDCWithFont(Self);
      ttfSize := GetFontData(Doc.FDc, 0, 0, nil, 0);

      SetLength(TTF, ttfSize);

      GetFontData(Doc.FDc, 0, 0, pointer(TTF), ttfSize);
      if CreateFontPackage(pointer(TTF), ttfSize, SubSetData, SubSetMem,
        SubSetSize, TTFCFP_FLAGS_SUBSET, 0, TTFMFP_SUBSET, 0,
        TTFCFP_MS_PLATFORMID, TTFCFP_UNICODE_CHAR_SET, pointer(used),
        length(used), @lpfnAllocate, @lpfnReAllocate, @lpfnFree, nil) = 0 then
      begin
        // subset was created successfully -> save to PDF file
        SetString(TTF, SubSetData, SubSetSize);
        Result := TTF;
        FreeMem(SubSetData);
      end;
    end;
  end;
end;

destructor TPdfFont.Destroy;
begin
  Dispose(FOTM);
  FData.Free;
  DeleteObject(FHGDI);
  FCharAndWidthDic.Free;
  inherited;
end;

function TPdfFont.GetAnsiCharWidth(const AText: PDFString; APos: Integer): Integer;
begin
  Result := 0;
end;

{ TPdfFontWinAnsi }

destructor TPdfFontWinAnsi.Destroy;
begin
  FreeMem(fWinAnsiWidth);
  inherited;
end;

function TPdfFontWinAnsi.GetAnsiCharWidth(const AText: PDFString; APos: Integer): Integer;
begin
  if (fWinAnsiWidth <> nil) and (AText[APos] >= #32) then
    result := fWinAnsiWidth[AText[APos]] else
    result := fDefaultWidth;
end;

function TPdfFont.FindOrAddUsedWideChar(aWideChar: WideChar): Integer;
var
  i: Integer;
begin
  (*
  result := fUsedWideChar.Add(ord(aWideChar));
  if result<0 then begin
    result := -(result+1); // this WideChar was already existing -> return index
    exit;
  end;
  // this WideChar was just added -> reserve space in fUsedWide[]
  if length(fUsedWide)=fUsedWideChar.Count-1 then
    SetLength(fUsedWide,fUsedWideChar.Count+100);
  n := fUsedWideChar.Count-1;
  if result<n then
    Move(fUsedWide[result],fUsedWide[result+1],(n-result)*4);
  // create associated Unicode Font if necessary
  // update fUsedWide[result] for current glyph
  *)

  i := FUsedWideChar.IndexOf(ord(aWideChar));
  Exit(I);

  if i < 0 then // if this glyph doesn't exist in this font -> set to zero
    i := 0
  else
    i := FUsedWide[i].Int;

  FUsedWide[result].Int := i; // update Width and Glyph
end;

//------------------------------------------------------------------------------

{ TPdfTTF }

constructor TPdfTTF.Create(aUnicodeTTF: TPdfFont);
var
  p: pointer;
  SubTable: ^TCmapSubTableArray absolute p;
  Header: ^TCmapHeader;
  I, n, code, ndx: PtrInt;
  off: cardinal;
  glyphIndex: integer;
  idDeltai, glyphi: PtrInt;
  w, numOfLongHorMetrics: word;
  fUnitsPerEmShr: cardinal;
begin
  // retrieve the 'cmap' (character code mapping) table
  // see http://developer.apple.com/fonts/TTRefMan/RM06/Chap6cmap.html
  // and http://www.microsoft.com/typography/OTSPEC/cmap.htm
  inherited Create;

  p := GetTTFData(aUnicodeTTF.FDocucment.FDc, 'cmap', fcmap);

  if not Assigned(p) then
    Exit;

  Header := p;
  Inc(PtrInt(p), SizeOf(TCmapHeader));
  off := 0;

  for I := 0 to Header^.numberSubtables - 1 do
    with SubTable^[I] do
      if platformID = TTFCFP_MS_PLATFORMID then
        if platformSpecificID = TTFCFP_SYMBOL_CHAR_SET then
          off := offset
        else if platformSpecificID = TTFCFP_UNICODE_CHAR_SET then
        begin
          off := offset;
          break; // prefered specific ID
        end;

  if (off = 0) or (off and 1 <> 0) then
    Exit; // we handle only Microsoft platform

  I := LongRec(off).Lo; // offset swap to bswap conversion :)
  LongRec(off).Lo := LongRec(off).Hi;
  LongRec(off).Hi := I;
  if off > cardinal(length(fcmap) * 2) then
    Exit; // avoid GPF

  fmt4 := pointer(PtrUInt(fcmap) + off);
  with fmt4^ do
  begin
    if format <> 4 then
      Exit; // we handle only cmap table format 4
    endCode := pointer(PtrUInt(@format) + SizeOf(TCmapFmt4));
    startCode := pointer(PtrUInt(endCode) + segCountX2 + 2); // +2 = reservedPad
    idDelta := pointer(PtrUInt(startCode) + segCountX2);
    idRangeOffset := pointer(PtrUInt(idDelta) + segCountX2);
    glyphIndexArray := pointer(PtrUInt(idRangeOffset) + segCountX2);
  end;

  // 'head', 'hmtx' (horizontal metrics) and 'hhea' (Horizontal Header) tables
  // see http://developer.apple.com/fonts/TTRefMan/RM06/Chap6hmtx.html
  head := GetTTFData(aUnicodeTTF.FDocucment.FDc, 'head', fhead);
  if head = nil then
    Exit;

  p := GetTTFData(aUnicodeTTF.FDocucment.FDc, 'hmtx', fhmtx);
  if p = nil then
    Exit;

  hhea := GetTTFData(aUnicodeTTF.FDocucment.FDc, 'hhea', fhhea);
  if hhea = nil then
    Exit;

  // fill aUnicodeTTF.fUsedWide[] and aUnicodeTTF.fUsedWideChar data
  n := fmt4^.segCountX2 shr 1;
  with aUnicodeTTF.FUsedWideChar do
  begin
    for I := 0 to n - 1 do
      Inc(Count, endCode[I] - startCode[I] + 1);
    SetLength(Values, Count);
    SetLength(aUnicodeTTF.FUsedWide, Count);
  end;

  ndx := 0;
  for I := 0 to n - 1 do
  begin
    idDeltai := idDelta[I];
    glyphi := idRangeOffset[I];
    if glyphi <> 0 then
      glyphi := glyphi shr 1 + I - n - startCode[I];
    for code := startCode[I] to endCode[I] do
    begin
      aUnicodeTTF.FUsedWideChar.Values[ndx] := code;
      if glyphi = 0 then
        glyphIndex := code + idDeltai
      else
      begin
        glyphIndex := glyphIndexArray[glyphi + code];
        if glyphIndex <> 0 then
          Inc(glyphIndex, idDeltai);
      end;
      aUnicodeTTF.FUsedWide[ndx].Glyph := glyphIndex;
      Inc(ndx);
    end;
  end;

  // UnitsPerEm range is from 16 to 16384. This value should be a power of 2.
  // (from http://www.microsoft.com/typography/OTSPEC/head.htm)
  fUnitsPerEmShr := 0; // fastest integer div for width calculating
  for I := 14 downto 4 do
    if GetBit(head^.unitsPerEm, I) then
    begin
      fUnitsPerEmShr := I;
      break;
    end;

  if fUnitsPerEmShr <> 0 then
  begin
    w := (cardinal(fhmtx[0]) * 1000) shr fUnitsPerEmShr;
    if aUnicodeTTF.FFixedWidth then
      for I := 0 to aUnicodeTTF.FUsedWideChar.Count - 1 do
        aUnicodeTTF.FUsedWide[I].Width := w
    else
    begin
      numOfLongHorMetrics := fhhea[17];  
      
      for I := 0 to aUnicodeTTF.FUsedWideChar.Count - 1 do
        with aUnicodeTTF.FUsedWide[I] do
        begin
          if Glyph <> 0 then
            if Glyph <= numOfLongHorMetrics then
              Width := (cardinal(fhmtx[Glyph * 2]) * 1000) shr fUnitsPerEmShr
            else
              Width := w;

          if Width = 0 then
            Width := w;
          aUnicodeTTF.FCharAndWidthDic.Add(Chr(aUnicodeTTF.FUsedWideChar.Values[I]), (Width/head^.unitsPerEm));
        end;
    end;
  end;
end;

{ TPDFHeader }

procedure TPDFHeader.Assign(Source: TPersistent);
begin
  if (Source is TPDFHeader) then
  begin
    FPageNumberFont.Assign((Source as TPDFHeader).PageNumberFont);
    FPageNumberHAlign := (Source as TPDFHeader).PageNumberHAlign;
    FPageNumberVAlign := (Source as TPDFHeader).PageNumberVAlign;
    FPageNumberVisible := (Source as TPDFHeader).PageNumberVisible;
    FText := (Source as TPDFHeader).Text;
    FTextFont.Assign((Source as TPDFHeader).TextFont);
    FTextHAlign := (Source as TPDFHeader).TextHAlign;
    FTextVAlign := (Source as TPDFHeader).TextVAlign;
  end;
end;

constructor TPDFHeader.Create;
begin
  inherited;
  FPageNumberVisible := False;
  FTextFOnt := TFont.Create;
  FPageNumberFont := TFont.Create;
end;

destructor TPDFHeader.Destroy;
begin
  FTextFont.Free;
  FPageNumberFont.Free;
  inherited;
end;

procedure TPDFHeader.SetPageNumberFont(const Value: TFont);
begin
  FPageNumberFont.Assign(Value);
end;

procedure TPDFHeader.SetTextFont(const Value: TFont);
begin
  FTextFont.Assign(Value);
end;

{ TPDFMetaData }

procedure TPDFMetaData.Assign(Source: TPersistent);
begin
  if (Source is TPDFMetaData) then
  begin
    FProducer := (Source as TPDFMetaData).Producer;
    FCreateDate := (Source as TPDFMetaData).CreateDate;
    FCreatorTool := (Source as TPDFMetaData).CreatorTool;
    FCreator := (Source as TPDFMetaData).Creator;
    FCompany := (Source as TPDFMetaData).Company;
    FTitle := (Source as TPDFMetaData).Title;
  end;
end;

constructor TPDFMetaData.Create;
begin
  inherited;
  FCompany := 'TMSSoftware';
  FCreateDate := Now;
  FCreator := 'TMSSoftware';
  FCreatorTool := 'TMSAdvPdfLib';
  FProducer := 'TMSSoftware';
  FTitle := 'MyPdf';
end;

destructor TPDFMetaData.Destroy;
begin
  inherited;
end;

{ TPDFPageSize }

procedure TPDFPageSize.Assign(Source: TPersistent);
begin
  if (Source is TPDFPageSize) then
  begin
    FWidth := (Source as TPDFPageSize).Width;
    FHeight := (Source as TPDFPageSize).Height;
    FPageSize := (Source as TPDFPageSize).PageSize;
  end;
end;

constructor TPDFPageSize.Create;
begin
  inherited;
  FWidth := 595;
  FHeight := 842;
  FPageSize := psA4;
end;

destructor TPDFPageSize.Destroy;
begin
  inherited;
end;

procedure TPDFPageSize.SetHeight(const Value: Integer);
begin
  FHeight := Value;
//  FPageSize := psCustom;
end;

procedure TPDFPageSize.SetPageSize(const Value: TPageSizes);
begin
  FPageSize := Value;

  case FPageSize of
    psA3:
    Begin
      FWidth := 842;
      FHeight := 1191;
      FPageSize := psA3;
    end;
    psA4:
    Begin
      FWidth := 595;
      FHeight := 842;
      FPageSize := psA4;
    end;
    psA5:
    Begin
      FWidth := 420;
      FHeight := 595;
      FPageSize := psA5;
    end;
    psLetter:
    Begin
      FWidth := 612;
      FHeight := 792;
      FPageSize := psLetter;
    end;
    psCustom: ;
  end;
end;

procedure TPDFPageSize.SetWidth(const Value: Integer);
begin
  FWidth := Value;
end;

end.
