{*************************************************************************}
{ TMS TAdvRichEditor                                                      }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright © 2014 - 2015                                       }
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

unit AdvRichEditorBase;

interface

{$I TMSDEFS.INC}

//{$DEFINE TMSDEBUG}

uses
  Classes, Generics.Collections, Rtti, Types, SysUtils
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  {$IFDEF VCLLIB}
  , Windows, Graphics, Controls, ActnList, Dialogs
  , AdvOfficeSelectors, AdvOfficeComboBox, AdvRichEditorRTF
  , JPEG, PNGImage, GIFImg, GDIPicture
  {$ENDIF}
  {$IFDEF FMXLIB}
  , System.UIConsts, System.Actions
  , FMX.Graphics, FMX.Types, FMX.Controls,  FMX.ActnList, FMX.TMSRTFEngine
  {$ENDIF}
  {$IFDEF LCLLIB}
  , Windows, Graphics, Controls, ActnList, Dialogs
  {$ENDIF}
  {$IFDEF TMSDEBUG}
  , TMSLogger
  {$ENDIF}
  , AdvScrollControl
  ;

{$IFDEF FMXLIB}
const
  clBlue = claBlue;
  clWhite = claWhite;
  clBlack = claBlack;
  clNone = claNull;
  clGray = claGray;
  clYellow = claYellow;
{$ENDIF}

const
  VK_A = $41;
  VK_B = $42;
  VK_C = $43;

  VK_E = $45;

  VK_I = $49;
  VK_L = $4C;

  VK_R = $52;

  VK_U = $55;
  VK_V = $56;
  VK_X = $58;
  VK_Y = $59;
  VK_Z = $5A;

{$IFDEF FMXLIB}
  KEY_DELETE = VKDELETE;
  KEY_INSERT = VKINSERT;
  KEY_LEFT = VKLEFT;
  KEY_RIGHT = VKRIGHT;
  KEY_UP = VKUP;
  KEY_DOWN = VKDOWN;
  KEY_NEXT = VKNEXT;
  KEY_PRIOR = VKPRIOR;
  KEY_RETURN = VKRETURN;
  KEY_TAB = VKTAB;
  KEY_HOME = VKHOME;
  KEY_END = VKEND;
{$ENDIF}

{$IFNDEF FMXLIB}
  KEY_DELETE = VK_DELETE;
  KEY_INSERT = VK_INSERT;
  KEY_LEFT = VK_LEFT;
  KEY_RIGHT = VK_RIGHT;
  KEY_UP = VK_UP;
  KEY_DOWN = VK_DOWN;
  KEY_NEXT = VK_NEXT;
  KEY_PRIOR = VK_PRIOR;
  KEY_RETURN = VK_RETURN;
  KEY_TAB = VK_TAB;
  KEY_HOME = VK_HOME;
  KEY_END = VK_END;
{$ENDIF}

const
  LINEFEED = #13#10;

  PICTURE_MARGIN = 4;

  BULLETINDENT = 24;

  BULLETSPACING = 0;

type
  {$IFDEF FMXLIB}
  TPicture = class(TBitmap);
  TGDIPPicture = class(TBitmap);
  TColor = TAlphaColor;

  TPenStyle = (psSolid, psDash, psDot, psDashDot, psDashDotDot, psClear,
    psInsideFrame, psUserStyle, psAlternate);

  TPictureFormat = (pfBMP, pfGIF, pfJPG, pfPNG, pfICO, pfTiff, pfMetaFile, pfNone);

  TRTFDocument = TTMSFMXRTFDocument;
  TRTFBaseLine = TTMSFMXRTFBaseLine;
  {$ENDIF}

  {$IFDEF LCLLIB}
  TPicture = class(TBitmap);
  TGDIPPicture = class(TBitmap);
  TPictureFormat = (pfBMP, pfGIF, pfJPG, pfPNG, pfICO, pfTiff, pfMetaFile, pfNone);
  TRTFEngine = class(TPersistent);
  TEncoding = (Unicode, UTF7, UTF8, Ansi);
  {$ENDIF}

  TURLAuto = (uAuto, uOff);

  TRTEProducer = (pVCL, pFMX);

  TMemoState = class(TPersistent)
  private
    FVersion: integer;
    FSelectionFromIndex: integer;
    FCaretIndex: integer;
    FSelectionTo: integer;
    FSelectionFrom: integer;
    FCaret: integer;
    FSelectionToIndex: integer;
    FTop: integer;
    FLeft: integer;
    FAuthor: string;
    FProducer: TRTEProducer;
    FTags: string;
    FLastModifiedBy: string;
    FComments: string;
    FDefaultFont: TFont;
    FColor: TColor;
    FTextColor: TColor;
    procedure SetDefaultFont(const Value: TFont);
    function GetColor: cardinal;
    function GetFontName: string;
    function GetFontSize: integer;
    function GetFontStyle: TFontStyles;
    function GetTextColor: cardinal;
    procedure SetColor(const Value: cardinal);
    procedure SetFontName(const Value: string);
    procedure SetFontSize(const Value: integer);
    procedure SetFontStyle(const Value: TFontStyles);
    procedure SetTextColor(const Value: cardinal);
  public
    constructor Create;
    destructor Destroy; override;
    property DefaultFont: TFont read FDefaultFont write SetDefaultFont;
  published
    property Author: string read FAuthor write FAuthor;
    property Producer: TRTEProducer read FProducer write FProducer;
    property Comments: string read FComments write FComments;
    property FontName: string read GetFontName write SetFontName;
    property FontSize: integer read GetFontSize write SetFontSize;
    property FontStyle: TFontStyles read GetFontStyle write SetFontStyle;
    property TextColorC: cardinal read GetTextColor write SetTextColor;
    property ColorC: cardinal read GetColor write SetColor;
    property LastModifiedBy: string read FLastModifiedBy write FLastModifiedBy;
    property Caret: integer read FCaret write FCaret;
    property CaretIndex: integer read FCaretIndex write FCaretIndex;
    property SelectionFrom: integer read FSelectionFrom write FSelectionFrom;
    property SelectionFromIndex: integer read FSelectionFromIndex write FSelectionFromIndex;
    property SelectionTo: integer read FSelectionTo write FSelectionTo;
    property SelectionToIndex: integer read FSelectionToIndex write FSelectionToIndex;
    property Version: integer read FVersion write FVersion;
    property Tags: string read FTags write FTags;
    property Top: integer read FTop write FTop;
    property Left: integer read FLeft write FLeft;
  end;

  TStyleElement = class(TObject)
  private
    FFont: TFont;
    FColor: TColor;
    FTextColor: TColor;
    FAlignment: TAlignment;
    procedure SetFont(const Value: TFont);
  public
    constructor Create;
    destructor Destroy; override;
    property Alignment: TAlignment read FAlignment write FAlignment;
    property Color: TColor read FColor write FColor;
    property TextColor: TColor read FTextColor write FTextColor;
    property Font: TFont read FFont write SetFont;
  end;

  TTextBaseLine = (tbRegular, tbSubscript, tbSuperScript);

  TREElement = class(TPersistent)
  private
    FXY: TPoint;
    FXYE: TPoint;
    FLH: integer;
    FTH: integer;
    FSelected: boolean;
    FSelFrom, FSelTo: integer;
    FAlignment: TAlignment;
    FText: string;
    FIndent: integer;
    FTag: integer;
    FURL: string;
    FCN: string;
    FColor: TColor;
    FTextColor: TColor;
    FFont: TFont;
    FBulletIndent: integer;
    FBaseLine: TTextBaseLine;
    FTagText: string;
    FMergeRef: string;
    FHighlight: boolean;
    FComment: string;
    FHint: string;
    FRW: boolean;
    procedure SetFont(const Value: TFont);
    function GetFontName: string;
    function GetFontSize: integer;
    function GetFontStyle: TFontStyles;
    procedure SetFontName(const Value: string);
    procedure SetFontSize(const Value: integer);
    procedure SetFontStyle(const Value: TFontStyles);
    function GetColor: cardinal;
    function GetTextColor: cardinal;
    procedure SetColor(const Value: cardinal);
    procedure SetTextColor(const Value: cardinal);
  protected
    function GetCN: string;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function Equal(AElement: TREElement): boolean; virtual;
    function TextLength: integer; virtual;
    function IndexAtEnd: integer; virtual;
    function Size: TSize; virtual;
    property Selected: boolean read FSelected write FSelected;
    property SelFrom: integer read FSelFrom write FSelFrom;
    property SelTo: integer read FSelTo write FSelTo;
    property XY: TPoint read FXY write FXY;
    property XYE: TPoint read FXYE write FXYE;
    property LH: integer read FLH write FLH;
    property TH: integer read FTH write FTH;
    property Font: TFont read FFont write SetFont;
    property Color: TColor read FColor write FColor;
    property TextColor: TColor read FTextColor write FTextColor;
    function DisplayText: string;
    class function Clone: TREElement;
    property WriteAccess: boolean read FRW write FRW;
  published
    property Alignment: TAlignment read FAlignment write FAlignment;
    property Baseline: TTextBaseLine read FBaseline write FBaseLine;
    property BulletIndent: integer read FBulletIndent write FBulletIndent;
    property ColorC: cardinal read GetColor write SetColor;
    property TextColorC: cardinal read GetTextColor write SetTextColor;
    property FontName: string read GetFontName write SetFontName;
    property FontSize: integer read GetFontSize write SetFontSize;
    property FontStyle: TFontStyles read GetFontStyle write SetFontStyle;
    property Highlight: boolean read FHighlight write FHighlight;
    property Hint: string read FHint write FHint;
    property Comment: string read FComment write FComment;
    property Text: string read FText write FText;
    property Indent: integer read FIndent write FIndent;
    property MergeRef: string read FMergeRef write FMergeRef;
    property Tag: integer read FTag write FTag;
    property TagText: string read FTagText write FTagText;
    property URL: string read FURL write FURL;
    property CN: string read GetCN;
  end;

  TREElementClass = class of TREElement;

  TTextElement = class(TREElement)
  private
    FStyleTag: integer;
    FTab: boolean;
    FLineBreak: boolean;
    FTextWidth: integer;
    FTextHeight: integer;
    FError: boolean;
    FLine: boolean;
  public
    constructor Create; override;
    destructor Destroy; override;
    function Equal(AElement: TREElement): boolean; override;
    procedure Assign(Source: TPersistent); override;
    function TextLength: integer; override;
    function IndexAtEnd: integer; override;
    property TextWidth: integer read FTextWidth write FTextWidth;
    property TextHeight: integer read FTextHeight write FTextHeight;
  published
    property Error: boolean read FError write FError;
    property StyleTag: integer read FStyleTag write FStyleTag;
    property Line: boolean read FLine write FLine;
  end;

  TTextElementClass = class of TTextElement;

  TLineBreakElement = class(TREElement)
  public
    constructor Create; override;
  end;

  TLineElement = class(TLineBreakElement)
  private
    FWidth: integer;
    FStyle: TPenStyle;
  public
    constructor Create; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Style: TPenStyle read FStyle write FStyle;
    property Width: integer read FWidth write FWidth;
  end;

  TTabElement = class(TREElement)
  private
    FWidth: integer;
  protected
    property TabWidth: integer read FWidth write FWidth;
  public
    function Size: TSize; override;
  end;

  TCustomGraphicElement = class(TREElement)
  private
    FDPIratio: double;
  public
    property DPIratio: double read FDPIratio write FDPIratio;
  end;

  TPictureElement = class(TCustomGraphicElement)
  private
    FPicture: TGDIPPicture;
    FPictureWidth: integer;
    FPictureHeight: integer;
    function GetPictureHeight: integer;
    function GetPictureWidth: integer;
  protected
    {$IFDEF FMXLIB}
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadData(Stream: TStream);
    procedure WriteData(Stream: TStream);
    {$ENDIF}
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function Size: TSize; override;
    procedure FitToPage(Width,Height: integer);
    {$IFDEF FMXLIB}
    property Picture: TGDIPPicture read FPicture write FPicture;
    {$ENDIF}
  published
    {$IFNDEF FMXLIB}
    property Picture: TGDIPPicture read FPicture write FPicture;
    {$ENDIF}
    property PictureWidth: integer read GetPictureWidth write FPictureWidth;
    property PictureHeight: integer read GetPictureHeight write FPictureHeight;
  end;

  TNamedPictureElement = class(TCustomGraphicElement)
  private
    FName: string;
    FWidth: integer;
    FHeight: integer;
  public
    procedure Assign(Source: TPersistent); override;
    function Size: TSize; override;
  published
    property Width: integer read FWidth write FWidth;
    property Height: integer read FHeight write FHeight;
    property Name: string read FName write FName;
  end;

  TEmoticonPictureElement = class(TNamedPictureElement);

  TGraphicElement = class(TCustomGraphicElement)
  private
    FWidth: integer;
    FHeight: integer;
    FID: string;
  public
    procedure Assign(Source: TPersistent); override;
    function Size: TSize; override;
  published
    property Width: integer read FWidth write FWidth;
    property Height: integer read FHeight write FHeight;
    property ID: string read FID write FID;
  end;

  TBulletType = (btCircle, btSquare, btArrow, btTick, btStar, btNumber, btChar, btCustom, btNone);

  TBulletElement = class(TREElement)
  private
    FType: TBulletType;
    FBulletFont: string;
    FBullet: string;
    FBulletFormat: string;
    FIndex: integer;
    FWidth: integer;
    FSpacing: integer;
  public
    constructor Create; override;
    procedure Assign(Source: TPersistent); override;
    function Size: TSize; override;
  published
    property Bullet: string read FBullet write FBullet;
    property BulletFont: string read FBulletFont write FBulletFont;
    property BulletFormat: string read FBulletFormat write FBulletFormat;
    property &Index: integer read FIndex write FIndex;
    property &Type: TBulletType read FType write FType;
    property Spacing: integer read FSpacing write FSpacing;
    property Width: integer read FWidth write FWidth;
  end;

  TBulletElementClass = class of TBulletElement;

  TBulletStart = class(TREElement)
  private
    FStartIndent: integer;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property StartIndent: integer read FStartIndent write FStartIndent;
  end;

  TBulletStartClass = class of TBulletStart;

  TBulletEnd = class(TREElement)
  end;

  TBulletEndClass = class of TBulletEnd;

  TPictureElementClass = class of TPictureElement;

  TNamedPictureElementClass = class of TNamedPictureElement;

  TEmoticonPictureElementClass = class of TEmoticonPictureElement;

  //TIPictureElementClass = class of TIPictureElement;

  TGraphicElementClass = class of TGraphicElement;

  TTableElement = class(TREElement)
  end;

  TElementSaver = class(TComponent)
  private
    FSaveElement: TREElement;
  published
    property SaveElement: TREElement read FSaveElement write FSaveElement;
  end;

  TElementClassSaver = class(TComponent)
  private
    FElementClass: string;
  published
    property ElementClass: string read FElementClass write FElementClass;
  end;

  TStateSaver = class(TComponent)
  private
    FSaveState: TMemoState;
  published
    property SaveState: TMemoState read FSaveState write FSaveState;
  end;

  TSelection = class(TPersistent)
  private
    FFromElement,FToElement: TREElement;
    FFromChar,FToChar: integer;
    FOnChange: TNotifyEvent;
    FToXY: TPoint;
    FFromXY: TPoint;
    procedure SetFromChar(const Value: integer);
    procedure SetFromElement(const Value: TREElement);
    procedure SetToChar(const Value: integer);
    procedure SetToElement(const Value: TREElement);
  protected
    procedure Changed;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
    property FromElement: TREElement read FFromElement write SetFromElement;
    property ToElement: TREElement read FToElement write SetToElement;
    property FromChar: integer read FFromChar write SetFromChar;
    property ToChar: integer read FToChar write SetToChar;
    property FromXY: TPoint read FFromXY write FFromXY;
    property ToXY: TPoint read FToXY write FToXY;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TCaret = class(TPersistent)
  private
    FEOL: boolean;
    FXY: TPoint;
    FLH: integer;
    FDescent: integer;
    FColor: TColor;
    FElement: TREElement;
    FCharIndex: integer;
    FOnChange: TNotifyEvent;
    FNextLine: boolean;
    FBaseLine: integer;
    procedure SetCharIndex(const Value: integer);
    procedure SetXY(const Value: TPoint);
  protected
    procedure Changed;
  public
    procedure Assign(Source: TPersistent); override;
    property Color: TColor read FColor write FColor;
    property EOL: boolean read FEOL write FEOL;
    property XY: TPoint read FXY write SetXY;
    property LH: integer read FLH write FLH;
    property Descent: integer read FDescent write FDescent;
    property BaseLine: integer read FBaseLine write FBaseLine;
    property Element: TREElement read FElement write FElement;
    property CharIndex: integer read FCharIndex write SetCharIndex;
    property NextLine: boolean read FNextLine write FNextLine;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TPageMargin = class(TPersistent)
  private
    FHorizontal: integer;
    FVertical: integer;
    FOnChange: TNotifyEvent;
    FColor: TColor;
    procedure SetHorizontal(const Value: integer);
    procedure SetVertical(const Value: integer);
    procedure SetColor(const Value: TColor);
  protected
    procedure Changed;
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create;
  published
    property Color: TColor read FColor write SetColor default clGray;
    property Horizontal: integer read FHorizontal write SetHorizontal default 0;
    property Vertical: integer read FVertical write SetVertical default 0;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TCaretPos = (cpBeginDoc, cpEndDoc);

  TCharCommandState = (ccNormal, ccShift, ccCtrl, ccCtrlShift);

  TErrorSelection = (esFirst, esNext, esPrevious, esLast, esUndo);

  TSelectionType = (stDefault, stWord, stLine, stSingle);

  TTextSize = record
    cx,cy: integer;
    sup: integer;
    sub: integer;
  end;

  TElements = TList<TREElement>;

  TContext = class(TObject)
  private
    FContent: TElements;
    FClientRect: TRect;
    FCaret: TCaret;
    FSelection: TSelection;
  public
    constructor Create(ARect: TRect);
    destructor Destroy; override;
    property ClientRect: TRect read FClientRect write FClientRect;
    property Caret: TCaret read FCaret write FCaret;
    property Selection: TSelection read FSelection write FSelection;
    property Content: TElements read FContent write FContent;
    procedure Clear;
    function Equal(AContext: TContext): boolean;
  end;

  TEnterWordEvent = procedure(Sender: TObject; AWord: string) of object;

  TCorrectWordEvent = procedure(Sender: TObject; var AWord: string; var Error: boolean) of object;

  TContextCorrectWordEvent = procedure(Sender: TObject; MousePos: TPoint; var AWord: string; AElement: TREElement; var Handled: boolean) of object;

  TRichEditorAutoReplace = class(TPersistent)
  private
    FOldValue: TStringList;
    FNewValue: TStringList;
    FDoAutoCorrect: boolean;
    FCaseSensitive: Boolean;
    procedure SetDoAutoCorrect(const Value: boolean);
    procedure SetNewValue(const Value: TStringList);
    procedure SetOldValue(const Value: TStringList);
  protected
    property CaseSensitive: Boolean read FCaseSensitive write FCaseSensitive default true;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Add(const AOldValue, ANewValue: string);
  published
    property Active: Boolean read FDoAutoCorrect write SetDoAutoCorrect default true;
    property OldValue: TStringList read FOldValue write SetOldValue;
    property NewValue: TStringList read FNewValue write SetNewValue;
  end;

  TRichEditorAutoCorrect = class(TRichEditorAutoReplace)
  published
    property CaseSensitive;
  end;

  THTMLImageGeneration = (igFile, igInline, igID);

  THTMLImageList = TList<TGDIPPicture>;

  TStyleElements = TList<TStyleElement>;

  TContexts = TList<TContext>;

  TAdvRichEditorBase = class(TCustomScrollingControl)
  private
    FAlignment: TAlignment;
    FContext: TContext;
    FMergeContext: TContext;
    FUndoRedo: TContexts;
    FUndoRedoIndex: integer;
    FSelectionElements: TElements;
    FStyles: TStyleElements;
    FURLColor: TColor;
    FURLAuto: TURLAuto;
    FSelection: TSelection;
    FWord: TSelection;
    FSelected: TREElement;
    FPaintSelection: TSelection;
    FCaret: TCaret;
    FDragCaret: TCaret;
    FTempCaret: TCaret;
    FTempSelection: TSelection;
    FSelectionTextColor: TColor;
    FSelectionColor: TColor;
    FHighlightColor: TColor;
    FHighlightTextColor: TColor;
    FClip: boolean;
    FPageWidth: integer;
    FPageHeight: integer;
    FPageMargin: TPageMargin;
    FLastElement: TREElement;
    FIndent: integer;
    FFindPos: TCaret;
    FFindText: string;
    FReplaceText: string;
    FFindMatchCase: boolean;
    FDoUndo: boolean;
    FModified: boolean;
    FUpdateCount: integer;
    FTextWidth: integer;
    FClassesRegistered: boolean;
    FFontBaseline: TTextBaseLine;
    FRTFEngine: TRTFEngine;
    FReadOnly: boolean;
    FDPIratio: double;
    FHideSelection: boolean;
    {$IFNDEF FMXLIB}
    FBuffer: Graphics.TBitmap;
    {$ENDIF}
    {$IFDEF FMXLIB}
    FBuffer: TBitmap;
    FFontColor: TColor;
    {$ENDIF}
    FOnEnterWord: TEnterWordEvent;
    FOnCorrectWord: TCorrectWordEvent;
    FOnAutoCorrectWord: TCorrectWordEvent;
    FOnContextCorrectWord: TContextCorrectWordEvent;
    FOnCaretChanged: TNotifyEvent;
    FOnSelectionChanged: TNotifyEvent;
    FOnToolBarUpdate: TNotifyEvent;
    FProducer: TRTEProducer;
    FAuthor: string;
    FComments: string;
    FTags: string;
    FLastModifiedBy: string;
    FLastResult: boolean;
    FDefaultChanged: boolean;
    FCurrErr: TREElement;
    FAutoCorrect: TRichEditorAutoCorrect;
    FSelectionType: TSelectionType;
    FHTMLImages: THTMLImageGeneration;
    FHTMLImageList: THTMLImageList;
    FTabSize: integer;
    FWantTab: boolean;
    FBulletSpacing: integer;
    procedure SetPageHeight(const Value: integer);
    procedure SetPageWidth(const Value: integer);
    procedure SetPageMargin(const Value: TPageMargin);
    function GetElement(Index: integer): TREElement;
    function GetElementCount: integer;
    function GetFocused: boolean;
    procedure SetSelLength(const Value: integer);
    procedure SetSelStart(const Value: integer);
    procedure SetAutoCorrect(const Value: TRichEditorAutoCorrect);
    function GetPlainTextContent: string;
    procedure SetPlainTextContent(const Value: string);
  protected
    FCaretUpdate: boolean;
    function GetTextSize(ACanvas: TCanvas; el: TREElement; AValue: string): TTextSize; virtual;
    function GetTextDescent(ACanvas: TCanvas; el: TREElement): integer; virtual;
    function GetBulletSize(el: TREElement; AValue: string): TSize; virtual;
    function GetPictureSize(el: TREElement): TSize; virtual;
    function GetText(el: TREElement): string;
    function GetCharInWord(el: TREElement; s: string; X: integer): integer; virtual; abstract;
    function GetCharPos(AValue: string; CharIndex: integer): integer; virtual; abstract;

    function GetDefaultFont: TFont; virtual; abstract;
    function GetDefaultFontColor: TColor; virtual; abstract;
    function GetDefaultFontName: string;
    function GetDefaultFontSize: integer;
    function GetLineHeight(AElement: TREElement): integer; virtual; abstract;

    // Calculation functions
    procedure CalcLine(ACanvas: TCanvas; Y,FromElementIndex, FromWordIndex, MaxLineWidth: integer; var LineWidth, LineHeight, ToElementIndex, ToWordIndex, Descent, BaseLine, Indent: integer; var Alignment: TAlignment);
    procedure CalcCaret(ACanvas: TCanvas; el: TREElement; x, y, LineHeight, LineDescent, LineBaseLine: integer; AValue: string; CharIndex: integer; DoDraw: boolean = true); virtual;

    // Abstract drawing functions
    procedure DrawElement(ACanvas: TCanvas; el: TREElement; x, y, MaxLineWidth, LineHeight, LineDescent, LineBaseLine: integer; AValue: string; CharIndex: integer); virtual; abstract;
    procedure DrawCaret(ACanvas: TCanvas); virtual; abstract;
    procedure DrawDragCaret(ACanvas: TCanvas); virtual; abstract;
    procedure DrawBackground(ACanvas: TCanvas); virtual; abstract;
    procedure DrawSelection(ACanvas: TCanvas; r: TRect); virtual; abstract;
    procedure DrawMargin(ACanvas: TCanvas); virtual; abstract;
    function DrawGraphic(ACanvas: TCanvas; ARect: TRect; ID: string): boolean; virtual;
    procedure DrawLineBreak(ACanvas: TCanvas; x,y: integer; el: TREElement); virtual; abstract;
    procedure DrawLine(ACanvas: TCanvas; X,Y,FromElementIndex, FromWordIndex: integer; MaxLineWidth, LineWidth, LineHeight, Descent, BaseLine: integer; DoDraw: boolean = true);

    // Clipboard functions
    procedure CutToClipboard; virtual;
    procedure CopyToClipboard(s: string); virtual; abstract;
    procedure CopyPictureToClipboard(APicture: TGDIPPicture); virtual; abstract;
    function PasteFromClipboard: string; virtual; abstract;
    function ClipboardHasContent: boolean; virtual; abstract;
    function GetContentAsRTE(UseSelection: boolean): string;
    function GetContentAsRTF(UseSelection: boolean): string;
    function GetContentAsPlainText(UseSelection: boolean): string;
    function GetContentAsHTML(UseSelection: boolean; ImgPath: string = ''): string;
    function GetRTEContent: string;
    procedure SetRTEContent(const Value: string);

    // Buffered paint functions
    procedure Refresh; virtual; abstract;
    procedure Paint; override;

    property Buffer: TBitmap read FBuffer write FBuffer;
    procedure BeginPaintBuffer; virtual; abstract;
    procedure EndPaintBuffer; virtual; abstract;
    {$IFNDEF FMXLIB}
    function CreateBuffer: Graphics.TBitmap; virtual; abstract;
    {$ENDIF}
    {$IFDEF FMXLIB}
    function CreateBuffer: TBitmap; virtual; abstract;
    {$ENDIF}
    procedure UpdateSelection; virtual; abstract;

    // Text manipulation
    function GetSize(VertSizeOnly: boolean): TSize;
    function GetLineWidth: integer; virtual;
    function GetSelLength: integer; overload;
    function GetSelStart: integer; overload;

    function SkipWords(Value: string; Count: integer; var CharsSkipped: integer): string;
    function GetNextWord(var Value: string): string;
    function FirstSeparator(Value: string): integer;
    function LastSeparator(Value: string): integer;
    function IsSpace(Ch: char): boolean; overload;
    function IsSpace(Ch: string): boolean; overload;
    function IsSeparator(Ch: char): boolean; overload;
    function IsSeparator(Ch: string): boolean; overload;
    function IsDelimiter(ch: char): boolean; virtual;
    procedure CaretChanged(Sender: TObject);
    procedure SelectionChanged(Sender: TObject);
    procedure CaretToSelection;
    procedure HandleURLS;
    function IsURL(const Value: string): boolean; virtual;
    property PaintSelection: TSelection read FPaintSelection write FPaintSelection;
    procedure SelectionToPaintSelection;
    function IsForwardSelection: boolean;
    property Clip: boolean read FClip write FClip;
    property Elements[Index: integer]: TREElement read GetElement;
    property ElementCount: integer read GetElementCount;
    property LastAddedElement: TREElement read FLastElement write FLastElement;
    property MergeContext: TContext read FMergeContext;
    function AddElement(el: TREElement): TREElement; overload;
    function AddElement(NewElement: TREElementClass): TREElement; overload;
    function InsertElement(Index: integer; el: TREElement): TREElement; overload;
    function InsertElement(Index: integer; NewElement: TREElementClass): TREElement; overload;

    function InsertLB(bullet: boolean; AType: TBulletType; AIndent: integer; IsShift, IsProg: boolean): TLineBreakElement;
    procedure OrderBullets(AElement: TREElement);
    procedure FixBullets(AElement: TREElement);
    function NextBullet(AElement: TREElement): TBulletElement;
    function PreviousBullet(AElement: TREElement): TBulletElement;
    function InBullet(AElement: TREElement): TBulletElement;
    procedure UnBullet(AElement: TREElement);
    function NextElement(AElement: TREElement): TREElement;
    function NextDocElement(AElement: TREElement): TREElement;
    function PreviousElement(AElement: TREElement): TREElement;
    function PreviousDocElement(AElement: TREElement): TREElement;
    function SelectIndexAtEnd(AElement: TREElement): integer;
    function DeleteElement(AElement: TREElement): integer;
    function ElementIndex(AElement: TREElement): integer;
    function LastIndex(AElement: TREElement): integer;
    function NearbyTextElement(AElement: TREElement; CharIndex: integer): TREElement;

    procedure CharCommand(AChar: Word; ACommandState: TCharCommandState);
    class function GetFontName(AFont: TFont): string;
    class procedure SetFontName(AFont: TFont; AName: string);
    function GetFontColor: TColor;
    procedure SetFontColor(AColor: TColor);
    class function GetFontSize(AFont: TFont): integer;
    class procedure SetFontSize(AFont: TFont; AValue: integer);
    function GetClientWidth: integer; virtual; abstract;
    function GetClientHeight: integer; virtual; abstract;
    {$IFDEF FMXLIB}
    property Focused: boolean read GetFocused;
    {$ENDIF}
    procedure InsertElementAtCaret(el: TREElement);
    function InsertAfterElement(AElement, NewElement: TREElement): TREElement; overload;
    function InsertBeforeElement(AElement, NewElement: TREElement): TREElement; overload;
    function InsertAfterElement(AElement: TREElement; NewElement: TREElementClass): TREElement; overload;
    function InsertBeforeElement(AElement: TREElement; NewElement: TREElementClass): TREElement; overload;

    function GetWordAtCaret(AElement: TREElement; AIndex: integer; var CharIndex: integer; SpaceOnly, Select: boolean): string;
    procedure SetWordAtCaret(AElement: TREElement; AIndex: integer; AValue: string);
    procedure GetSelectionElements(SelectionType: TSelectionType = stDefault);
    procedure EnsureCaret;
    function GetCaretTextElement: TTextElement;
    function IsNewBullet(var AType: TBulletType; var AIndex, AIndent: integer): boolean;
    function CaretInBullet: boolean;
    procedure ChangeBulletType(AType: TBulletType; AFontName: string);
    procedure DeleteBullets(AType: TBulletType);
    function GetFirstBullet(AElement: TREElement): TREElement;
    function GetLastBullet(AElement: TREElement): TREElement;
    function OffsetX: integer;
    function OffsetY: integer;

    function FindAndReplace(FromElement: TREElement; FromPos: integer; AText, AReplacement: string; MatchCase, DoReplace: boolean): boolean;

    procedure UpdateSize;
    procedure PushContext;
    procedure PopContext;
    procedure ClearContext;
    procedure CloneContext(FromContext, ToContext: TContext);

    procedure PushCaret;
    procedure PopCaret;
    procedure PushSelection;
    procedure PopSelection;
    property DragCaret: TCaret read FDragCaret;
    procedure RegisterClasses;
    procedure DelChar(const Back: boolean);
    procedure DelCaretElement(const Back: boolean);
    procedure DoEnterWord;
    procedure DoCorrectWord;
    function IsEmoticon(const EmoticonID: string): boolean; virtual; abstract;

    procedure LoadMemoState(AState: TMemoState; VersionOnly: boolean = false);
    procedure SaveMemoState(AState: TMemoState);
    property RTFEngine: TRTFEngine read FRTFEngine;
    property DPIratio: double read FDPIratio write FDPIratio;
    property HideSelection: boolean read FHideSelection write FHideSelection;
    procedure DoCaretChanged; virtual;
    procedure DoSelectionChanged; virtual;
    procedure DoPaintEditor; virtual; abstract;
    function GetBulletChar(const AType: TBulletType): char; virtual; abstract;
    property Producer: TRTEProducer read FProducer write FProducer;
    function InsertLineBreak(IsShift: boolean; AIndent: integer = 0; Prog: boolean = false): TREElement;
    procedure InsertLineBreakAndBullet(AType: TBulletType; AIndent: integer);
    function NextChar(SelectionUpdate: boolean = true): boolean;
    function PrevChar(SelectionUpdate: boolean = true): boolean;
    procedure NextWord(SelectionUpdate: boolean = true);
    procedure PrevWord(SelectionUpdate: boolean = true);
    procedure PrevCharSel;
    procedure NextCharSel;
    procedure PrevWordSel;
    procedure NextWordSel;
    procedure Backspace;
    function AppendLineBreak: TREElement;
    procedure InitializePictureSize(NamedPictureElement: TNamedPictureElement); virtual; abstract;
    function EditCanModify: Boolean; virtual;
    function CalcCaretXY: integer;
    property AutoCorrect: TRichEditorAutoCorrect read FAutoCorrect write SetAutoCorrect;
    property OnAutoCorrectWord: TCorrectWordEvent read FOnAutoCorrectWord write FOnAutoCorrectWord;
    property OnContextCorrectWord: TContextCorrectWordEvent read FOnContextCorrectWord write FOnContextCorrectWord;
    function ElementsSelected: boolean;
    property SelectionType: TSelectionType read FSelectionType write FSelectionType;
    property DefaultChanged: boolean read FDefaultChanged write FDefaultChanged;
    property TabSize: integer read FTabSize write FTabSize;
    property WantTab: boolean read FWantTab write FWantTab;
    property BulletSpacing: integer read FBulletSpacing write FBulletSpacing;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure TidyElements;
    function HasSelection: boolean;
    function FirstElement: TREElement;
    function LastElement: TREElement;

    procedure GetWordAndIndexAtCaret(var AValue: string; var AIndex: integer);
    procedure UpdateWordAndIndexAtCaret(AValue: string; AIndex: integer);

    function XYToElement(X,Y: integer; var el: TREElement): boolean;
    function XYToChar(X,Y: integer; el: TREElement; var CX,CY: integer): integer;
    function XYToWord(X,Y: integer; el: TREElement): string; overload;
    function XYToWord(X,Y: integer): string; overload;
    procedure XYToCaret(X,Y: integer; TestSelection: boolean; var IsSelected: boolean); overload;
    procedure XYToCaret(X,Y: single; TestSelection: boolean; var IsSelected: boolean); overload;
    function IsCaretInBulletList(var AType: TBulletType; var AIndex, AIndent: integer): boolean;
    function AddText(AValue: string): TTextElement; overload;
    function AddText(AValue: string; AColor: TColor): TTextElement; overload;
    function AddText(AValue: string; AColor: TColor; BkColor: TColor): TTextElement; overload;
    function AddText(AValue: string; AFont: TFont): TTextElement; overload;
    function AddText(AValue: string; AFontSize: integer; AFontName: string; AFontStyle: TFontStyles): TTextElement; overload;
    function AddText(AValue: string; AAlignment: TAlignment): TTextElement; overload;
    function AddText(AValue: string; AFontSize: integer; AFontName: string; AFontStyle: TFontStyles; AAlignment: TAlignment): TTextElement; overload;
    procedure AddHyperlink(AValue, AURL: string);
    procedure AddMultiLineText(AValue: string);
    function AddLineBreak: TREElement;
    procedure AddLine(AColor: TColor = clBlack; AWidth: integer = 1; APenStyle: TPenStyle = psSolid);
    function AddTab: TTabElement;
    procedure AddBullet(AType: TBulletType = btCircle);
    {$IFNDEF FMXLIB}
    function AddBitmap(Bitmap: TBitmap): TPictureElement;
    {$ENDIF}
    function AddImage(Picture: TPicture): TPictureElement; overload;
    function AddImage(Picture: TPicture; AWidth, AHeight: integer): TPictureElement; overload;
    function AddImage(FileName: string): TPictureElement; overload;
    function AddImage(FileName: string; AWidth, AHeight: integer): TPictureElement; overload;
    procedure AddGraphic(AWidth, AHeight: integer; const AID: string);
    procedure AddNamedPicture(AWidth, AHeight: integer; const AName: string);
    procedure AddEmoticonPicture(AWidth, AHeight: integer; const AName: string);
    function InsertText(Index: integer; AValue: string): TTextElement; overload;
    function InsertText(AValue: string): TTextElement; overload;
    procedure InsertMultiLineText(AValue: string);
    function InsertImage(FileName: string; AWidth: integer = 0; AHeight: integer = 0): TPictureElement; overload;
    function InsertImage(Picture: TPicture; AWidth: integer = 0; AHeight: integer = 0): TPictureElement; overload;
    function InsertGraphic(const ID: string; AWidth, AHeight: integer): TGraphicElement;
    function InsertNamedPicture(const AName: string; AWidth, AHeight: integer): TNamedPictureElement;
    function InsertEmoticonPicture(const AName: string; AWidth, AHeight: integer): TEmoticonPictureElement;
    procedure InsertChar(ch: char);
    procedure InsertTab;
    function InsertBullet(AType: TBulletType = btCircle): TBulletElement;
    procedure DeleteChar;
    procedure DeleteCaretElement;
    procedure DeleteSelection;
    procedure DeleteSelected;
    function Text: string;
    function SelectedText: string;
    function SelectWordAtXY(X,Y: integer): string;
    function SelectWordAtCaret: string;
    function WordAtXY(X,Y: integer): string;
    function WordAtCaret: string;
    function IsEmpty: boolean;

    procedure Merge(NamesAndValues: TStringList); overload; virtual;
    procedure UnMerge;

    procedure UnSelect;
    procedure SelectAll;

    procedure ShowTree;
    procedure ShowSelection;
    procedure SaveToText(AFileName: string);

    procedure SelectError(ErrorSelection: TErrorSelection);
    procedure CorrectSelectedError(NewWord: string);
    procedure CorrectAllErrors(ErrorWord, NewWord: string);
    procedure IgnoreSelectedError(ErrorWord: string);
    procedure IgnoreAllErrors(ErrorWord: string);
    function ErrorCount: integer;

    procedure SetSelectionAttribute(AFont: TFont; AColor: TColor); overload;
    procedure SetSelectionAttribute(AFont: TFont; AColor: TColor; BkColor: TColor); overload;
    procedure SetSelectionAttribute(AFontName: string; AFontSize: integer; AFontStyle: TFontStyles; AColor: TColor); overload;
    procedure SetSelectionAttribute(AFontName: string; AFontSize: integer; AFontStyle: TFontStyles; AColor, BkColor: TColor); overload;
    procedure SetSelectionAttribute(AAlignment: TAlignment); overload;
    procedure SetSelectionAttribute(AError: boolean); overload;

    procedure SetSelectionColor(AColor: TColor);
    procedure SetSelectionBkColor(AColor: TColor);
    procedure SetSelectionBold(DoBold: boolean);
    procedure SetSelectionItalic(DoItalic: boolean);
    procedure SetSelectionUnderline(DoUnderline: boolean);
    procedure SetSelectionStrikeOut(DoStrikeOut: boolean);
    procedure SetSelectionError(DoError: boolean);
    procedure SetSelectionLine(DoLine: boolean);
    procedure SetSelectionSubscript(DoSubScript: boolean);
    procedure SetSelectionSuperscript(DoSuperScript: boolean);
    procedure SetSelectionIndent(AIndent: integer);
    procedure SetSelectionBullets(AType: TBulletType); overload;
    procedure SetSelectionBullets(AType: TBulletType; AFontName: string); overload;
    procedure SetSelectionHyperlink(AURL: string);
    procedure SetSelectionFontName(AName: string);
    procedure SetSelectionFontSize(ASize: integer);
    procedure SetSelectionHighlight;
    procedure SetSelectionMergeField(AMergeName: string);
    procedure SetSelectionHint(AHint: string);

    function IsSelectionBold: boolean;
    function IsSelectionItalic: boolean;
    function IsSelectionUnderline: boolean;
    function IsSelectionStrikeOut: boolean;
    function IsSelectionSubscript: boolean;
    function IsSelectionSuperscript: boolean;

    function IsSelectionLeft: boolean;
    function IsSelectionCenter: boolean;
    function IsSelectionRight: boolean;

    function GetSelectionTextColor: TColor;
    function GetSelectionBkColor: TColor;
    function GetSelectionIndent: integer;
    function GetSelectionFontName: string;
    function GetSelectionFontSize: integer;
    function GetSelectionBullet: TBulletType;

    function ContentAsHTML(ImgPath: string = ''): string;
    function SelectionAsHTML: string;
    function ContentAsRTF: string;
    property ContentAsRTE: string read GetRTEContent write SetRTEContent;
    property ContentAsPlainText: string read GetPlainTextContent write SetPlainTextContent;
    function SelectionAsRTF: string;
    procedure InsertAsRTF(rtfstring: string);

    procedure Clear; virtual;
    procedure ClearSelection; virtual;
    procedure ClearErrors; virtual;
    procedure ClearCaret;
    procedure SelectText(FromChar, ALength: integer);
    procedure SelectElement(AElement: TREElement);
    procedure SetCaret(CaretPos: TCaretPos);

    property Selection: TSelection read FSelection write FSelection;

    property Caret: TCaret read FCaret write FCaret;
    property Selected: TREElement read FSelected write FSelected;
    procedure ScrollToCaret;

    function PlainText: string;
    procedure LoadFromTextFile(const FileName: string); overload;
    procedure LoadFromTextFile(const FileName: string; Encoding: TEncoding); overload;
    procedure LoadFromStream(const AStream: TStream);
    procedure InsertFromStream(const AStream: TStream; f: double);
    procedure LoadFromFile(const FileName: string);
    procedure AppendFile(const FileName: string);
    procedure SaveToFile(const FileName: string);
    procedure SaveToStream(const AStream: TStream);
    procedure SaveSelectionToStream(const AStream: TStream);

    function Find(AText: string; MatchCase: boolean = false): boolean;
    function FindFirst(AText: string; MatchCase: boolean = false): boolean;
    function FindNext: boolean;
    function Replace(AText, AReplacement: string; MatchCase: boolean = false): boolean;
    function ReplaceAll(AText, AReplacement: string; MatchCase: boolean = false): boolean;

    function ReplaceFirst(AText, AReplacement: string; MatchCase: boolean = false): boolean;
    function ReplaceNext: boolean;

    function CheckFirstWord: string;
    function CheckNextWord: string;
    property CheckWord: TSelection read FWord write FWord;

    function GetSelLength(ASelection: TSelection): integer; overload;
    function GetSelStart(ASelection: TSelection): integer; overload;

    property SelStart: integer read GetSelStart write SetSelStart;
    property SelLength: integer read GetSelLength write SetSelLength;

    function Highlight(AText: string; MatchCase: boolean = false): boolean;
    procedure UnHighlight;

    function CanUnindent: boolean;
    function CanUndo: boolean;
    function CanRedo: boolean;
    procedure Undo;
    procedure Redo;

    procedure GotoLineBegin;
    procedure GotoLineEnd;
    procedure GotoTextBegin;
    procedure GotoTextEnd;

    {$IFNDEF FMXLIB}
    procedure BeginUpdate;
    procedure EndUpdate;
    {$ENDIF}
    {$IFDEF FMXLIB}
    procedure BeginUpdate; override;
    procedure EndUpdate; override;
    {$ENDIF}

    property Context: TContext read FContext;
    property PageMargin: TPageMargin read FPageMargin write SetPageMargin;
    property Modified: boolean read FModified write FModified;
    property ReadOnly: boolean read FReadOnly write FReadOnly default false;
    property OnCaretChanged: TNotifyEvent read FOnCaretChanged write FOnCaretChanged;
    property OnCorrectWord: TCorrectWordEvent read FOnCorrectWord write FOnCorrectWord;
    property OnEnterWord: TEnterWordEvent read FOnEnterWord write FOnEnterWord;
    property OnSelectionChanged: TNotifyEvent read FOnSelectionChanged write FOnSelectionChanged;
    property OnToolBarUpdate: TNotifyEvent read FOnToolBarUpdate write FOnToolBarUpdate;
    property PageWidth: integer read FPageWidth write SetPageWidth;
    property PageHeight: integer read FPageHeight write SetPageHeight;
    property HTMLImages: THTMLImageGeneration read FHTMLImages write FHTMLImages;
    property HTMLImageList: THTMLImageList read FHTMLImageList;
  published
    property Author: string read FAuthor write FAuthor;
    property Comments: string read FComments write FComments;
    property HighlightColor: TColor read FHighlightColor write FHighlightColor default clYellow;
    property HighlightTextColor: TColor read FHighlightTextColor write FHighlightTextColor default clBlack;
    property LastModifiedBy: string read FLastModifiedBy write FLastModifiedBy;
    property SelectionColor: TColor read FSelectionColor write FSelectionColor default clBlue;
    property SelectionTextColor: TColor read FSelectionTextColor write FSelectionTextColor default clWhite;
    property Tags: string read FTags write FTags;
    property URLAuto: TURLAuto read FURLAuto write FURLAuto default uAuto;
    property URLColor: TColor read FURLColor write FURLColor default clBlue;
  end;

  TAdvRichEditorAction = class(TAction)
  private
    FControl: TAdvRichEditorBase;
    procedure SetControl(Value: TAdvRichEditorBase);
  protected
    function GetControl(Target: TObject): TAdvRichEditorBase; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    destructor Destroy; override;
    function HandlesTarget(Target: TObject): Boolean; override;
    procedure UpdateTarget(Target: TObject); override;
    property Control: TAdvRichEditorBase read FControl write SetControl;
  end;

  TAdvRichEditorClear = class(TAdvRichEditorAction)
  public
    procedure UpdateTarget(Target: TObject); override;
    procedure ExecuteTarget(Target: TObject); override;
  end;

  TAdvRichEditorCut = class(TAdvRichEditorAction)
  public
    procedure UpdateTarget(Target: TObject); override;
    procedure ExecuteTarget(Target: TObject); override;
  end;

  TAdvRichEditorCopy = class(TAdvRichEditorAction)
  public
    procedure UpdateTarget(Target: TObject); override;
    procedure ExecuteTarget(Target: TObject); override;
  end;

  TAdvRichEditorPaste = class(TAdvRichEditorAction)
  public
    procedure UpdateTarget(Target: TObject); override;
    procedure ExecuteTarget(Target: TObject); override;
  end;

  TAdvRichEditorUndo = class(TAdvRichEditorAction)
  public
    procedure UpdateTarget(Target: TObject); override;
    procedure ExecuteTarget(Target: TObject); override;
  end;

  TAdvRichEditorRedo = class(TAdvRichEditorAction)
  public
    procedure UpdateTarget(Target: TObject); override;
    procedure ExecuteTarget(Target: TObject); override;
  end;

  TAdvRichEditorSelectAll = class(TAdvRichEditorAction)
  public
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;
  end;

  TAdvRichEditorAlignRight = class(TAdvRichEditorAction)
  public
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;
  end;

  TAdvRichEditorAlignLeft = class(TAdvRichEditorAction)
  public
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;
  end;

  TAdvRichEditorAlignCenter = class(TAdvRichEditorAction)
  public
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;
  end;

  TAdvRichEditorBold = class(TAdvRichEditorAction)
  public
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;
  end;

  TAdvRichEditorItalic = class(TAdvRichEditorAction)
  public
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;
  end;

  TAdvRichEditorUnderline = class(TAdvRichEditorAction)
  public
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;
  end;

  TAdvRichEditorStrikeOut = class(TAdvRichEditorAction)
  public
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;
  end;

  TAdvRichEditorSubscript = class(TAdvRichEditorAction)
  public
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;
  end;

  TAdvRichEditorSuperscript = class(TAdvRichEditorAction)
  public
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;
  end;

  TAdvRichEditorTextColor = class(TAdvRichEditorAction)
  public
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;
  end;

  TAdvRichEditorFontName = class(TAdvRichEditorAction)
  public
    function HandlesTarget(Target: TObject): Boolean; override;
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;
  end;

  TAdvRichEditorFontSize = class(TAdvRichEditorAction)
  public
    function HandlesTarget(Target: TObject): Boolean; override;
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;
  end;

  TAdvRichEditorBulletType = class(TAdvRichEditorAction)
  public
    function HandlesTarget(Target: TObject): Boolean; override;
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;
  end;

  TAdvRichEditorNumberedBulletType = class(TAdvRichEditorAction)
  public
    function HandlesTarget(Target: TObject): Boolean; override;
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;
  end;

  TAdvRichEditorColor = class(TAdvRichEditorAction)
  public
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;
  end;

  TAdvRichEditorIndent = class(TAdvRichEditorAction)
  public
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;
  end;

  TAdvRichEditorUnIndent = class(TAdvRichEditorAction)
  public
    procedure ExecuteTarget(Target: TObject); override;
    procedure UpdateTarget(Target: TObject); override;
  end;

  function CharInStr(s: string; Index: Integer): Char;
  function ColorToHtml(const Value: TColor): string;

  function UnFixMarkup(su:string;SpecialChars: boolean = true):string;
  function FixMarkup(su:string; SpecialChars: boolean = true): string;

implementation

uses
  Math
  {$IFDEF DELPHIXE6_LVL}
  , Character
  {$ENDIF}
  ;

{$IFNDEF FMXLIB}
const
  claBlack = clBlack;
{$ENDIF}

const
  NumSpecialChar = 51;

var
  HTMLEncodedChar : array[1..55] of string = ('é','è','ë','ê',
                                             'ó','ò','ö','ô',
                                             'í','ì','ï','î',
                                             'ú','ù','ü','û',
                                             'á','à','ä','â',
                                             'É','È','Ë','Ê',
                                             'Ó','Ò','Ö','Ô',
                                             'Í','Ì','Ï','Î',
                                             'Ú','Ù','Ü','Û',
                                             'Á','À','Ä','Â',
                                             'ç','Ç','ø','Ø',
                                             'å','Å','©','®',
                                             '€','«','»','ã','Ã','õ','Õ');

  HTMLSpecialChar : array[1..55] of string = ('&eacute;','&egrave;','&euml;','&ecirc;',
                                             '&oacute;','&ograve;','&ouml;','&ocirc;',
                                             '&iacute;','&igrave;','&iuml;','&icirc;',
                                             '&uacute;','&ugrave;','&uuml;','&ucirc;',
                                             '&aacute;','&agrave;','&auml;','&acirc;',
                                             '&Eacute;','&Egrave;','&Euml;','&Ecirc;',
                                             '&Oacute;','&Ograve;','&Ouml;','&Ocirc;',
                                             '&Iacute;','&Igrave;','&Iuml;','&Icirc;',
                                             '&Uacute;','&Ugrave;','&Uuml;','&Ucirc;',
                                             '&Aacute;','&Agrave;','&Auml;','&Acirc;',
                                             '&ccedil;','&Ccedil;','&oslash;','&Oslash;',
                                             '&aring;','&Aring;','&copy;','&reg;',
                                             '&euro;','&laquo;','&raquo;','&atilde;','&Atilde;','&otilde;','&Otilde');

var
  EncodeTable: array[0..63] of Char =
    'ABCDEFGHIJKLMNOPQRSTUVWXYZ' +
    'abcdefghijklmnopqrstuvwxyz' +
    '0123456789+/';

  DecodeTable: array[#0..#127] of Integer = (
    Byte('='), 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64,
    64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64,
    64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 62, 64, 64, 64, 63,
    52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 64, 64, 64, 64, 64, 64,
    64,  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14,
    15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 64, 64, 64, 64, 64,
    64, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40,
    41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 64, 64, 64, 64, 64);

type
  PPacket = ^TPacket;
  TPacket = packed record
    case Integer of
      0: (b0, b1, b2, b3: Byte);
      1: (i: Integer);
      2: (a: array[0..3] of Byte);
      3: (c: array[0..3] of Char);
  end;


{------------------------------------------------------------------------------}
{ HELPER FUNCTIONS                                                             }
{------------------------------------------------------------------------------}

procedure EncodePacket(const Packet: TPacket; NumChars: Integer; ss: TStringStream);
var
  outb0,outb1,outb2,outb3: char;
begin
  outb0 := EnCodeTable[Packet.a[0] shr 2];
  outb1 := EnCodeTable[((Packet.a[0] shl 4) or (Packet.a[1] shr 4)) and $0000003f];

  if NumChars < 2 then
    outb2 := '='
  else
    outb2 := EnCodeTable[((Packet.a[1] shl 2) or (Packet.a[2] shr 6)) and $0000003f];

  if NumChars < 3 then
    outb3 := '='
  else
    outb3 := EnCodeTable[Packet.a[2] and $0000003f];

  ss.WriteString(outb0 + outb1 + outb2 + outb3);
end;

function Base64Encode(ms: TMemoryStream): string;
var
  I, K: Integer;
  Packet: TPacket;
  ss: TStringStream;
  b: byte;
begin
  Result := '';

  ms.Position := 0;

  ss := TStringStream.Create('');

  try
    for I := 1 to ms.Size div 3 do
    begin
      Packet.i := 0;
      ms.Read(Packet.a, 3);
      EncodePacket(Packet, 3, ss);
    end;

    K := 0;
    Packet.i := 0;
    for I := ms.Size - (ms.Size mod 3) + 1 to ms.Size do
    begin
      ms.Read(b,1);
      Packet.a[K] := b;
      Inc(K);
      if I = ms.Size then
        EncodePacket(Packet, ms.Size mod 3, ss);
    end;

    Result := ss.DataString;

  finally
    ss.Free;
  end;
end;


{$IFDEF FMXLIB}
function ColorToRgb(const AColor: TColor): TColor;
begin
  Result := AColor;
end;
{$ENDIF}

function AddBackslash(const s: string): string;
begin
  {$IFDEF DELPHI_LLVM}
  if (Length(s) >= 1) and (s[Length(s) - 1] <> '\') and (s[Length(s) - 1] <> '/') then
  {$ELSE}
  if (Length(s) >= 1) and (s[Length(s)] <> '\') and (s[Length(s)] <> '/') then
  {$ENDIF}
    Result := s + '\'
  else
    Result := s;
end;

function ColorToHtml(const Value: TColor): string;
type
  TColorRecord = record
    RedValue: Byte;    //  clRed = TColor($0000FF);   Low byte
    GreenValue: Byte;  //  clLime = TColor($00FF00);  Middle byte
    BlueValue: Byte;   //  clBlue = TColor($FF0000);  High byte
    SystemValue: Byte; //  becomes zero when calling ColorToRgb
  end;
const
  HtmlHexColor = '#RRGGBB';
  HexDigit: array[0..$F] of Char = '0123456789ABCDEF';
{$IFDEF FMXLIB}
var
  i: integer;
{$ENDIF}
begin
 //  HTML Color output as: #RRGGBB

 {$IFDEF VCLLIB}
  with TColorRecord(ColorToRGb(Value)) do
  begin
    Result := HtmlHexColor;
    Result[2] := HexDigit[RedValue shr 4];
    Result[3] := HexDigit[RedValue and $F];
    Result[4] := HexDigit[GreenValue shr 4];
    Result[5] := HexDigit[GreenValue and $F];
    Result[6] := HexDigit[BlueValue shr 4];
    Result[7] := HexDigit[BlueValue and $F];
  end;
  {$ENDIF}

 {$IFDEF FMXLIB}
  with TAlphaColorRec(Value) do
  begin
    Result := HtmlHexColor;
    {$IFDEF DELPHI_LLVM}
    i := 0;
    {$ELSE}
    i := 1;
    {$ENDIF}
    Result[1 + i] := HexDigit[R shr 4];
    Result[2 + i] := HexDigit[R and $F];
    Result[3 + i] := HexDigit[G shr 4];
    Result[4 + i] := HexDigit[G and $F];
    Result[5 + i] := HexDigit[B shr 4];
    Result[6 + i] := HexDigit[B and $F];
  end;
  {$ENDIF}

end;

function ToRGB(c: cardinal): cardinal;
var
  r,g,b: cardinal;
begin
  r := c and $FF;
  g := c and $FF00;
  b := c and $FF0000;

  Result := (r shl 16) + g + (b shr 16);
end;

function EqualFont(Font1,Font2: TFont): boolean;
begin
 {$IFDEF VCLLIB}
 Result := (Font1.Name = Font2.Name) and (Font1.Size = Font2.Size) and (Font1.Style = Font2.Style);
 {$ENDIF}

 {$IFDEF FMXLIB}
 Result := (Font1.Family = Font2.Family) and (Font1.Size = Font2.Size) and (Font1.Style = Font2.Style);
 {$ENDIF}
end;

function MidPos(su,s: string): integer;
var
  res,d: integer;
  ts: string;
begin
  Result := 0;
  ts := TrimLeft(s);
  d  := Length(s) - length(ts);
  res := d + Pos(su,TrimRight(ts));

  if res > 0 then
    Result := res;
end;

function PosFrom(ASubText, AText: string; FromPos: integer; MatchCase: boolean): integer;
var
  s: string;
begin
  s := Copy(AText,FromPos + 1,Length(AText));

  if not MatchCase then
    Result := Pos(AnsiUpperCase(ASubText),AnsiUppercase(s))
  else
    Result := Pos(ASubText,s);

  if Result > 0 then
  begin
    Result := Result + FromPos;
  end;
end;


function LastBlank(s: string): integer;
var
  res: integer;
begin
  Result := -1;
  res := Length(s);
  while res > 0 do
  begin
    if s[res] = ' ' then
    begin
      Result := res;
      break;
    end;
    dec(res);
  end;
end;

function CharInStr(s: string; Index: Integer): Char;
begin
  Result := #0;
  if (Index > 0) and (Index <= Length(s)) then
  begin
    {$IFDEF DELPHI_LLVM}
    dec(Index);
    {$ENDIF}
    Result := s[Index]
  end;
end;

{------------------------------------------------------------------------------}

{ TTextElement }

procedure TTextElement.Assign(Source: TPersistent);
begin
  inherited;

  if (Source is TTextElement) then
  begin
    FColor := (Source as TTextElement).Color;
    FTextColor := (Source as TTextElement).TextColor;
    FFont.Assign((Source as TTextElement).Font);
    FSelected := (Source as TTextElement).Selected;
    FStyleTag := (Source as TTextElement).StyleTag;
    FTextWidth := (Source as TTextElement).TextWidth;
    FTextHeight  := (Source as TTextElement).TextHeight;
    FError := (Source as TTextElement).Error;
    FLine := (Source as TTextElement).Line;
  end;
end;

constructor TTextElement.Create;
begin
  inherited;
  FTab := false;
  FLineBreak := false;
  FError := false;
  FColor := clNone;
  FIndent := 0;
  FAlignment := taLeftJustify;
end;

destructor TTextElement.Destroy;
begin
  inherited;
end;

function TTextElement.Equal(AElement: TREElement): boolean;
begin
  Result := inherited Equal(AElement);
  if Result then
  begin
    Result := ((AElement as TTextElement).Error = Error) and
              ((AElement as TTextElement).Highlight = Highlight) and
              (AElement.Indent = Indent) and
              (AElement.Baseline = BaseLine) and
              EqualFont(AElement.Font, Font);
  end;
end;


function TTextElement.IndexAtEnd: integer;
begin
  Result := Length(Text);
end;

function TTextElement.TextLength: integer;
begin
  Result := Length(DisplayText);
end;

{------------------------------------------------------------------------------}

{ TREElement }

procedure TREElement.Assign(Source: TPersistent);
begin
  if (Source is TREElement) then
  begin
    FXY := (Source as TREElement).XY;
    FXYE := (Source as TREElement).XYE;
    FLH := (Source as TREElement).LH;
    FTH := (Source as TREElement).TH;
    FAlignment := (Source as TREElement).Alignment;
    FSelected := (Source as TREElement).Selected;
    FSelFrom := (Source as TREElement).SelFrom;
    FSelTo := (Source as TREElement).SelTo;
    FText := (Source as TREElement).Text;
    FIndent := (Source as TREElement).Indent;
    FTag := (Source as TREElement).Tag;
    FFont.Assign((Source as TREElement).Font);
    FColor := (Source as TREElement).Color;
    FTextColor := (Source as TREElement).TextColor;
    FBulletIndent := (Source as TREElement).BulletIndent;
    FTagText := (Source as TREElement).TagText;
    FHighlight := (Source as TREElement).Highlight;
    FHint := (Source as TREElement).Hint;
    FMergeRef := (Source as TREElement).MergeRef;
    FBaseLine := (Source as TREElement).Baseline;
    FHighlight := (Source as TREElement).Highlight;
    FHint := (Source as TREElement).Hint;
    FURL := (Source as TREElement).URL;
    FComment := (Source as TREElement).Comment;
  end;
end;

class function TREElement.Clone: TREElement;
begin
  Result := Create;
end;

constructor TREElement.Create;
begin
  inherited Create;
  FCN := ClassName;
  FFont := TFont.Create;
  FColor := clNone;
  FTextColor := clBlack;
  FRW := false;
end;

destructor TREElement.Destroy;
begin
  FFont.Free;
  inherited;
end;

function TREElement.DisplayText: string;
begin
  if MergeRef <> '' then
    Result := '«' + Text + '»'
  else
    Result := Text;
end;

function TREElement.Equal(AElement: TREElement): boolean;
begin
  Result := (AElement.ClassName = ClassName) and
            (AElement.Alignment = Alignment) and (AElement.URL = URL) and
            (AElement.Color = Color) and (AElement.TextColor = TextColor) and (AElement.MergeRef = MergeRef);
end;

function TREElement.GetCN: string;
begin
  Result := ClassName;
end;

function TREElement.GetColor: cardinal;
begin
 {$IFDEF VCLLIB}
  Result := Cardinal(ColorToRGB(ToRGB(Color)));
  {$ENDIF}
  {$IFDEF FMXLIB}
  Result := Cardinal(Color);
  {$ENDIF}
end;

function TREElement.GetFontName: string;
begin
  Result := TAdvRichEditorBase.GetFontName(FFont);
end;

function TREElement.GetFontSize: integer;
begin
  Result := TAdvRichEditorBase.GetFontSize(FFont);
end;

function TREElement.GetFontStyle: TFontStyles;
begin
  Result := FFont.Style;
end;

function TREElement.GetTextColor: cardinal;
begin
  {$IFDEF VCLLIB}
  Result := Cardinal(ColorToRGB(ToRGB(TextColor)));
  {$ENDIF}
  {$IFDEF FMXLIB}
  Result := Cardinal(TextColor);
  {$ENDIF}
end;

function TREElement.IndexAtEnd: integer;
begin
  Result := 1;
end;

procedure TREElement.SetColor(const Value: cardinal);
begin
  {$IFDEF VCLLIB}
  if Value = 0 then
    Color := clNone
  else
    Color := TColor(ToRGB(Value));
  {$ENDIF}
  {$IFDEF FMXLIB}
  if Value <> 0 then
    Color := TColor(Value OR $FF000000)
  else
    Color := TColor(Value);
  {$ENDIF}
end;

procedure TREElement.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
end;

procedure TREElement.SetFontName(const Value: string);
begin
  TAdvRichEditorBase.SetFontName(FFont, Value);
end;

procedure TREElement.SetFontSize(const Value: integer);
begin
  TAdvRichEditorBase.SetFontSize(FFont, Value);
end;

procedure TREElement.SetFontStyle(const Value: TFontStyles);
begin
  FFont.Style := Value;
end;

procedure TREElement.SetTextColor(const Value: cardinal);
begin
  {$IFDEF VCLLIB}
  TextColor := TColor(ToRGB(Value));
  {$ENDIF}
  {$IFDEF FMXLIB}
  TextColor := TColor(Value OR $FF000000);
  {$ENDIF}
end;

function TREElement.Size: TSize;
begin
  Result.cx := 0;
  Result.cy := 0;
end;

function TREElement.TextLength: integer;
begin
  Result := 0;
end;

{------------------------------------------------------------------------------}

{ TAdvRichEditorBase }

function TAdvRichEditorBase.AddText(AValue: string): TTextElement;
begin
  Result := AddText(AValue, GetFontColor);
end;

function TAdvRichEditorBase.AddText(AValue: string; AColor: TColor): TTextElement;
var
  el: TTextElement;
begin
  el := TTextElement.Create;
  el.Alignment := FAlignment;
  el.Text := AValue;
  el.TextColor := AColor;
  el.Font.Assign(GetDefaultFont);
  el.FontStyle := GetDefaultFont.Style;
  el.Baseline := FFontBaseLine;
  if FDefaultChanged then
    el.Color := Color;
  AddElement(el);
  Result := el;
end;

function TAdvRichEditorBase.AddText(AValue: string; AAlignment: TAlignment): TTextElement;
var
  el: TTextElement;
begin
  el := TTextElement.Create;
  el.Alignment := AAlignment;
  el.Text := AValue;
  el.Font.Assign(GetDefaultFont);
  el.TextColor := claBlack;
  el.Baseline := FFontBaseLine;
  AddElement(el);
  Result := el;
end;

function TAdvRichEditorBase.AddText(AValue: string; AColor, BkColor: TColor): TTextElement;
var
  el: TTextElement;
begin
  el := TTextElement.Create;
  el.Alignment := FAlignment;
  el.Text := AValue;
  el.TextColor := AColor;
  el.Color := BkColor;
  el.Font.Assign(GetDefaultFont);
  el.Baseline := FFontBaseLine;
  AddElement(el);
  Result := el;
end;

function TAdvRichEditorBase.AddText(AValue: string; AFont: TFont): TTextElement;
var
  el: TTextElement;
begin
  el := TTextElement.Create;
  el.Alignment := FAlignment;
  el.Text := AValue;
  el.Font.Assign(AFont);
  el.Baseline := FFontBaseLine;
  AddElement(el);
  Result := el;
end;

procedure TAdvRichEditorBase.AddBullet(AType: TBulletType = btCircle);
var
  el: TBulletElement;
begin
  AddLineBreak;
  el := TBulletElement.Create;
  el.&Type := AType;
  el.Font.Assign(GetDefaultFont);
  el.BulletFont := GetFontName(Font);
  el.Spacing := BulletSpacing;
  AddElement(el);
end;

function TAdvRichEditorBase.AddElement(el: TREElement): TREElement;
begin
  Result := nil;
  if EditCanModify then
  begin
    FModified := true;
    FContext.Content.Add(el);
    FLastElement := el;
    Result := el;
    UpdateSize;
    if not Assigned(Caret.Element) then
      EnsureCaret;
  end;
end;

function TAdvRichEditorBase.AddElement(NewElement: TREElementClass): TREElement;
var
  pel: TREElement;
begin
  Result := nil;
  if EditCanModify then
  begin
    FModified := true;
    Result := NewElement.Create;
    FContext.Content.Add(Result);

    pel := PreviousElement(Result);

    if Assigned(pel) then
      Result.Font.Assign(pel.Font)
    else
      Result.Font.Assign(GetDefaultFont);

    FLastElement := Result;
    UpdateSize;
  end;
end;

procedure TAdvRichEditorBase.AddGraphic(AWidth, AHeight: integer; const AID: string);
var
  el: TGraphicElement;
begin
  el := TGraphicElement.Create;
  el.Width := AWidth;
  el.Height := AHeight;
  el.ID := AID;
  AddElement(el);
end;

procedure TAdvRichEditorBase.AddNamedPicture(AWidth, AHeight: integer; const AName: string);
var
  el: TNamedPictureElement;
begin
  el := TNamedPictureElement.Create;
  el.Width := AWidth;
  el.Height := AHeight;
  el.Name := AName;
  AddElement(el);
end;

procedure TAdvRichEditorBase.AddEmoticonPicture(AWidth, AHeight: integer; const AName: string);
var
  el: TEmoticonPictureElement;
begin
  el := TEmoticonPictureElement.Create;
  el.Width := AWidth;
  el.Height := AHeight;
  el.Name := AName;
  AddElement(el);
end;


procedure TAdvRichEditorBase.AddHyperlink(AValue, AURL: string);
var
  el: TTextElement;
begin
  el := TTextElement.Create;
  el.Alignment := taLeftJustify;
  el.Text := AValue;
  el.URL := AURL;
  el.Font.Assign(GetDefaultFont);
  AddElement(el);
end;

procedure TAdvRichEditorBase.AddLine(AColor: TColor = clBlack; AWidth: integer = 1; APenStyle: TPenStyle = psSolid);
var
  el: TLineElement;
begin
  el := TLineElement.Create;
  el.Color := AColor;
  el.Width := AWidth;
  el.Style := APenStyle;
  AddElement(el);
end;

function TAdvRichEditorBase.AddLineBreak: TREElement;
var
  el: TLineBreakElement;
begin
  el := TLineBreakElement.Create;
  el.Font.Assign(GetDefaultFont);
  Result := el;
  AddElement(Result);
end;

procedure TAdvRichEditorBase.AddMultiLineText(AValue: string);
var
  t: string;
  i,l: integer;
  ch: char;
begin
  i := 1;
  t := '';

  l := Length(AValue);

  while i <= l do
  begin
    ch := CharInStr(AValue, i);
    if (ch = #10) then
    begin
      {$IFNDEF MSWINDOWS}
      if (t <> '') then
        AddText(t);
      AddLineBreak;
      t := '';
      {$ENDIF}
    end
    else
    if (ch = #9) then
    begin
      if (t <> '') then
        AddText(t);
      if WantTab then
        AddTab
      else
        AddText(' ');

      t := '';
    end
    else
    if (ch = #13) then
    begin
      if (t <> '') then
        AddText(t);
      AddLineBreak;
      t := '';
    end
    else
      t := t + ch;

    inc(i);
  end;

  if (t <> '') then
    AddText(t);

  UpdateSize;
  Refresh;
end;

{$IFNDEF FMXLIB}
function TAdvRichEditorBase.AddBitmap(Bitmap: TBitmap): TPictureElement;
var
  el: TPictureElement;
begin
  el := TPictureElement.Create;
  el.FPicture.Assign(Bitmap);
  {$IFNDEF LCLLIB}
  el.FPicture.TransparentBitmap := false;
  {$ENDIF}
  el.Font.Assign(GetDefaultFont);
  el.PictureWidth := Bitmap.Width;
  el.PictureHeight := Bitmap.Height;
  AddElement(el);
  Result := el;
end;
{$ENDIF}

function TAdvRichEditorBase.AddImage(FileName: string): TPictureElement;
begin
  Result := AddImage(FileName, 0, 0);
end;

function TAdvRichEditorBase.AddImage(FileName: string; AWidth,
  AHeight: integer): TPictureElement;
var
  el: TPictureElement;
begin
  el := TPictureElement.Create;
  el.FPicture.LoadFromFile(FileName);
  el.Font.Assign(GetDefaultFont);
  el.PictureWidth := AWidth;
  el.PictureHeight := AHeight;
  AddElement(el);
  Result := el;
end;

function TAdvRichEditorBase.AddImage(Picture: TPicture): TPictureElement;
begin
  Result := AddImage(Picture, 0, 0);
end;

function TAdvRichEditorBase.AddImage(Picture: TPicture; AWidth, AHeight: integer): TPictureElement;
var
  el: TPictureElement;
begin
  el := TPictureElement.Create;
  el.Picture.Assign(Picture);
  el.Font.Assign(GetDefaultFont);
  el.PictureWidth := AWidth;
  el.PictureHeight := AHeight;
  AddElement(el);
  Result := el;
end;

procedure TAdvRichEditorBase.InsertMultiLineText(AValue: string);
var
  t: string;
  i,l: integer;
  ch: char;
  lb: boolean;

begin
  i := 1;
  t := '';
  lb := false;

  BeginUpdate;

  l := Length(AValue);

  while i <= l do
  begin
    ch := CharInStr(AValue, i);
    if ((ch = #10) or (ch = #13)) then
    begin
      if not lb then
      begin
        lb := true;
        if (t <> '') then
        begin
          InsertText(t);
          t := '';
        end;
        InsertLineBreak(false);
      end
      else
        lb := false;
    end
    else
    if (ch = #9) then
    begin
      if (t <> '') then
        InsertText(t);

      if WantTab then
        InsertTab
      else
        InsertText(' ');
      t := '';
    end
    else
    begin
      t := t + ch;
      lb := false;
    end;

    inc(i);
  end;

  InsertText(t);

  EndUpdate;

  UpdateSize;
end;

function TAdvRichEditorBase.InsertText(AValue: string): TTextElement;
var
  el: TTextElement;
  s,su: string;

  function DoAdd(s: string): TTextElement;
  begin
    el := TTextElement.Create;
    el.Alignment := FAlignment;
    el.Text := s;
    el.TextColor := GetFontColor;
    el.Font.Assign(GetDefaultFont);
    InsertElementAtCaret(el);
    Result := el;
  end;

begin
  Result := nil;
  if (URLAuto = uAuto) and IsURL(AValue) then
  begin
    s := AValue;
    while s <> '' do
    begin
      su := GetNextWord(s);
      Result := DoAdd(su);
      if IsURL(su) then
        Result.URL := su;
    end;
  end
  else
  begin
    Result := DoAdd(AValue);
  end;
end;

function TAdvRichEditorBase.AddTab: TTabElement;
begin
  Result := TTabElement.Create;
  AddElement(Result);
end;

function TAdvRichEditorBase.AddText(AValue: string; AFontSize: integer;
  AFontName: string; AFontStyle: TFontStyles): TTextElement;
var
  el: TTextElement;
begin
  el := TTextElement.Create;
  el.Text := AValue;
  SetFontName(el.Font, AFontName);
  el.Font.Size := AFontSize;
  el.Font.Style := AFontStyle;
  el.TextColor := claBlack;
  AddElement(el);
  Result := el;
end;

function TAdvRichEditorBase.AddText(AValue: string; AFontSize: integer;
  AFontName: string; AFontStyle: TFontStyles; AAlignment: TAlignment): TTextElement;
var
  el: TTextElement;
begin
  el := TTextElement.Create;
  el.Text := AValue;
  el.Alignment := AAlignment;
  SetFontName(el.Font, AFontName);
  el.Font.Size := AFontSize;
  el.Font.Style := AFontStyle;
  el.TextColor := claBlack;
  AddElement(el);
  Result := el;
end;

procedure TAdvRichEditorBase.AppendFile(const FileName: string);
var
  AStream: TMemoryStream;
begin
  RegisterClasses;

  AStream := TMemoryStream.Create;
  AStream.LoadFromFile(FileName);
  AStream.Position := 0;

  try
    LoadFromStream(AStream);
  finally
    AStream.Free;
  end;

  UpdateSize;
  Refresh;
  PushContext;
  ScrollHome;
end;

function TAdvRichEditorBase.EditCanModify: Boolean;
begin
  Result := true;
end;

procedure TAdvRichEditorBase.Backspace;
var
  el: TREElement;
  nel,pel: TREElement;
  bul: TBulletElement;
  bt: TBulletType;
  isinerror: boolean;
  Error: boolean;
  s: string;
begin
  if FDoUndo then
  begin
    PushContext;
    FDoUndo := false;
  end;
  if HasSelection then
    DeleteSelection
  else
  begin
    FDefaultChanged := false;
    if (Caret.Element is TTextElement) and (Caret.CharIndex > 0) then
    begin
      isinerror := (Caret.Element as TTextElement).Error;

      Caret.CharIndex :=  Caret.CharIndex - 1;
      if (Caret.CharIndex = 0) and (Length(Caret.Element.Text) <= 1) then
      begin
        el := Caret.Element;
        Caret.Element := PreviousElement(el);
        if Assigned(Caret.Element) then
        begin
          if Caret.Element is TTextElement then
            Caret.CharIndex := Caret.Element.TextLength
          else
            Caret.CharIndex := 1;
        end
        else
          Caret.CharIndex := 0;

        DeleteElement(el);
      end
      else
      begin
        DelChar(true);

        if isinerror then
        begin
          S := Caret.Element.Text;

          Error := false;

          if Assigned(OnAutoCorrectWord) then
            OnAutoCorrectWord(Self, S, Error);

          if Assigned(OnCorrectWord) then
            OnCorrectWord(Self, S, Error);

          if not Error then
            (Caret.Element as TTextElement).Error := false;
        end;
      end;
    end
    else
    if (Caret.Element is TTextElement) and (Caret.CharIndex = 0) then
    begin
      el := PreviousElement(Caret.Element);
      if Assigned(el) then
      begin
        if el is TBulletElement then
        begin
          nel := NextElement(Caret.Element);
          pel := PreviousElement(el);
          if Length(Caret.Element.Text) = 0 then
            DeleteElement(Caret.Element);

          DeleteElement(el);

          bul := NextBullet(pel);
          OrderBullets(bul);

          if (pel is TBulletStart) and (nel is TBulletEnd) then
          begin;
            el := PreviousElement(pel);
            Caret.Element := el;
            if Assigned(el) then
              Caret.CharIndex := Length(el.Text)
            else
              Caret.CharIndex := 0;

            DeleteElement(pel);
            DeleteElement(nel);
          end
          else
          if (pel is TBulletStart) then
          begin
            bul := NextBullet(pel);
            if Assigned(bul) then
            begin
              UnBullet(pel);
              DeleteElement(pel);
              InsertBeforeElement(bul, TBulletStart);
              OrderBullets(bul);
              // reset indent till bullet
            end;
          end;

        end
        else
        if el is TLineBreakElement then
        begin
          DeleteElement(el);
        end
        else
        begin
          PrevChar;
          DelChar(true);
        end;
      end
      else
      begin
        if Caret.Element.Indent > 0 then
          Caret.Element.Indent := 0;
      end;
    end
    else
    if (Caret.Element is TCustomGraphicElement) then
    begin
      if Caret.CharIndex = 1 then
      begin
        el := Caret.Element;
        pel := PreviousElement(el);
        nel := NextElement(el);
        DeleteElement(el);

        if Assigned(pel) then
        begin
          Caret.Element := pel;
          Caret.CharIndex := LastIndex(pel);
        end
        else
        begin
          Caret.Element := nel;
          Caret.CharIndex := 0;
        end;
      end
      else
      begin
        if PrevChar then
          DelChar(true);
      end;
    end
    else
    if (Caret.Element is TBulletStart) then
    begin
      pel := Caret.Element;
      nel := NextElement(Caret.Element);

      if Assigned(nel) and (nel is TBulletEnd) then
      begin
        Caret.Element := PreviousElement(pel);
        Caret.CharIndex := LastIndex(Caret.Element);
        DeleteElement(pel);
        DeleteElement(nel);
      end;
    end
    else
    if (Caret.Element is TBulletElement) then
    begin
      {$IFDEF TMSDEBUG}
      TDebug.Write('Delete:'+caret.Element.ClassName);
      {$ENDIF}

      bt := (Caret.Element as TBulletElement).&Type;
      pel := PreviousElement(Caret.Element);
      nel := NextElement(Caret.Element);
      el := nel;
      DeleteElement(Caret.Element);
      if (nel is TTextElement) then
      begin
        if (nel.Text = '') then
        begin
          el := nel;
          nel := NextElement(el);
          DeleteElement(el);
        end;
      end
      else
      if (nel is TLineBreakElement) then
      begin
        el := pel;
        DeleteElement(nel);
      end;

      Caret.Element := el;
      Caret.CharIndex := 0;

      if (pel is TBulletStart) and (nel is TBulletEnd) then
      begin
        el := PreviousElement(pel);
        DeleteElement(pel);
        DeleteElement(nel);
        Caret.Element := el;
        if Assigned(el) then
          Caret.CharIndex := Length(Caret.Element.Text);
      end
      else
      if (pel is TBulletStart) then
      begin
        bul := NextBullet(pel);
        if Assigned(bul) then
        begin
          DeleteElement(pel);
          InsertBeforeElement(bul, TBulletStart);
          OrderBullets(bul);
        end
        else
        begin //no more bullets found
          DeleteBullets(bt);
        end;
      end;

    end
    else
    if (Caret.Element is TBulletEnd) then
    begin
      el := PreviousElement(Caret.Element);
      pel := PreviousElement(el);

      if Assigned(el) then
        DeleteElement(el);

      Caret.Element := pel;
      if Assigned(pel) then
      begin
        if Caret.Element is TTextElement then
          Caret.CharIndex := pel.TextLength
        else
          Caret.CharIndex := 1;
      end
      else
        EnsureCaret;
    end
    else
    if (Caret.Element is TLineBreakElement) or (Caret.Element is TBulletElement) then
    begin
      {$IFDEF TMSDEBUG}
      TDebug.Write('Delete:'+caret.Element.ClassName);
      {$ENDIF}

      el := PreviousElement(Caret.Element);

      if (Caret.CharIndex = 0) and Assigned(el) then
      begin
        if (el is TTextElement) then
        begin
          PrevChar;
          DelChar(true);
        end
        else
        begin
          DeleteElement(el);
        end;
      end
      else
      begin
        DeleteElement(Caret.Element);
        Caret.Element := el;
        Caret.CharIndex := 0;

        if Assigned(el) then
        begin
          if not (el is TTextElement) then
            Caret.CharIndex := 1
          else
            Caret.CharIndex := Length(Caret.Element.Text);
        end
        else
          EnsureCaret;
      end;
    end;

    bul := PreviousBullet(Caret.Element);
    OrderBullets(bul);

    CaretToSelection;
    Refresh;
  end;
end;

function TAdvRichEditorBase.AppendLineBreak: TREElement;
begin
  Result := TLineBreakElement.Create;
  Result.Assign(LastElement);
  Result.Text := '';
  Context.Content.Add(Result);
  Caret.Element := Result;
  Caret.CharIndex := 1;
end;

procedure TAdvRichEditorBase.BeginUpdate;
begin
  {$IFDEF FMXLIB}
  inherited;
  {$ENDIF}
  inc(FUpdateCount);
end;

function TAdvRichEditorBase.DrawGraphic(ACanvas: TCanvas; ARect: TRect; ID: string): boolean;
begin
  Result := true;
end;

procedure TAdvRichEditorBase.CalcCaret(ACanvas: TCanvas; el: TREElement; x, y, LineHeight,
  LineDescent, LineBaseLine: integer; AValue: string; CharIndex: integer; DoDraw: boolean = true);
var
  SP: TPoint;
  cp: Integer;
begin
  if DoDraw and not Focused then
    Exit;

  SP := Point(0,0);

  Caret.LH := LineHeight;
  Caret.Descent := LineDescent;
  Caret.BaseLine := LineBaseLine;

  if DoDraw then
  begin
    if Assigned(el) and el.Selected and (el.SelFrom <> el.SelTo) then
      Caret.Color := SelectionTextColor
    else
      Caret.Color := GetDefaultFontColor;

    if (Caret.Element = el) and (Caret.CharIndex = 0) then
      Caret.Color := GetDefaultFontColor;
  end;

  if (el is TCustomGraphicElement) or (el is TBulletElement) or (el is TBulletStart) or (el is TBulletEnd) or  (el is TTabElement)  then
  begin
    if CharIndex = 0 then
    begin
      Caret.XY := Point(x,y);
    end
    else
    begin
      if (el is TTabElement) then
      begin
        Caret.XY := Point(x + el.Size.cx + TabSize,y);
      end
      else
        Caret.XY := Point(x + el.Size.cx,y);
    end;
    Exit;
  end;

  if CharIndex = 0 then
  begin
    Caret.XY := Point(x,y);
  end
  else
  begin
    cp := GetCharPos(AValue, CharIndex);
    Caret.XY := Point(x + cp,y);
  end;

  if Assigned(el) and (el.Baseline = tbRegular) and (LineBaseLine > 0) then
    Caret.XY := Point(Caret.XY.X, Caret.XY.Y - Round(el.TH/4));

  if Assigned(el) and (el.Baseline = tbSuperscript) then
  begin
    Caret.XY := Point(Caret.XY.X, Caret.XY.Y - Round(el.TH/4));
    if (LineBaseLine > 0) then
      Caret.XY := Point(Caret.XY.X, Caret.XY.Y - Round(el.TH/4));
  end;
end;

procedure TAdvRichEditorBase.CalcLine(ACanvas: TCanvas; Y, FromElementIndex, FromWordIndex, MaxLineWidth: integer;
  var LineWidth, LineHeight, ToElementIndex, ToWordIndex, Descent, BaseLine, Indent: integer; var Alignment: TAlignment);
var
  el,nel: TREElement;
  s,su: string;
  sz : TTextSize;
  des: integer;
  CharsSkipped: integer;
  pt: TPoint;
  TopLine, tx: integer;
  firstbul: boolean;

begin
  LineWidth := 0;
  LineHeight := 0;
  Descent := 0;
  BaseLine := 0;
  TopLine := 0;
  s := '';

  pt := Point(0,Y);

  el := FContext.Content.Items[FromElementIndex];

  firstbul := (el is TBulletEnd);

// TDebug.Write('Calc line: '+ el.ClassName + ':' + el.Text);

  if (FromWordIndex = 0) and (Y <> -1) then
  begin
    el.XY := pt;
  end;

  ToElementIndex := FromElementIndex;
  ToWordIndex := FromWordIndex;

  if (FromWordIndex = 0) then
  begin
    if firstbul then
    begin
      nel := NextElement(el);
      if not Assigned(nel) then
        nel := el;
    end
    else
      nel := el;

    nel.XY := Point(nel.XY.X + Round(nel.Indent * DPIRatio), nel.XY.Y);
    if nel.Alignment = taLeftJustify then
      pt.X := Round(nel.Indent * DPIratio) + Offsetx;
  end;

  Alignment := el.Alignment;

  if el is TTextElement then
    s := (el as TTextElement).DisplayText;

  sz := GetTextSize(ACanvas, el, 'gh');

  LineHeight := sz.cy;
  LineWidth := OffsetX + Round(el.Indent * DPIratio);
  BaseLine := sz.sub;

  if FromWordIndex > 0 then
  begin
    s := SkipWords(s, FromWordIndex, CharsSkipped);
  end;

  if (LineWidth >= MaxLineWidth) then
  begin
    FTextWidth := LineWidth + 1 + offsetx;

    if FromWordIndex = 0 then
    begin
      inc(ToElementIndex);
    end;
  end;

  while (LineWidth < MaxLineWidth) do
  begin
    if (el is TLineBreakElement) then
    begin
      el.XY := pt;

      if (Descent = 0) then
      begin
        nel := NearbyTextElement(el,1);

        sz := GetTextSize(ACanvas, nel, 'gh');
        if sz.cy > LineHeight then
          LineHeight := sz.cy;

        if sz.sub > BaseLine then
          BaseLine := sz.sub;

        des := GetTextDescent(ACanvas, nel);
        if des > Descent then
          Descent := des;
      end;

      Inc(ToElementIndex);
      Break;
    end;

    if (el is TBulletStart) then
    begin
      pt.X := offsetx + Round(el.Indent * DPIratio);
      el.XY := pt;
    end;

    if (el is TBulletEnd) then
    begin
      el.XY := pt;
    end;

    if (el is TBulletElement) then
    begin
      el.XY := pt;
      el.XY := Point(offsetx + Round(el.Indent * DPIratio), pt.Y);

      sz.cx := el.Size.cx;
      sz.cy := el.Size.cy;

      if sz.cy > LineHeight then
        LineHeight := sz.cy;

      LineWidth := Round(el.Indent * DPIratio) + offsetx;

      if (Descent = 0) then
      begin
        nel := NearbyTextElement(el,1);

        sz := GetTextSize(ACanvas, nel, 'gh');
        if sz.cy > LineHeight then
          LineHeight := sz.cy;

        if sz.sub > BaseLine then
          BaseLine := sz.sub;

        des := GetTextDescent(ACanvas, nel);
        if des > Descent then
          Descent := des;
      end;
    end;

    if (el is TCustomGraphicElement) then
    begin
      el.XY := pt;
      el.XY := Point(el.XY.X + Round(el.Indent * DPIratio), pt.Y);

      sz.cx := Round(el.Size.cx * DPIratio);
      sz.cy := Round(el.Size.cy * DPIratio);

      (el as TCustomGraphicElement).DPIratio := DPIratio;

      if sz.cy > LineHeight then
        LineHeight := sz.cy;

      LineWidth := LineWidth + sz.cx;
    end;

    if (el is TTabElement) then
    begin
      el.XY := pt;

      sz := GetTextSize(ACanvas, el, 'gh');
      tx := LineWidth div TabSize;
      el.TH := sz.cy;

      (el as TTabElement).TabWidth := (tx + 1) * TabSize - LineWidth;

      LineWidth := (tx + 1) * TabSize;
    end;

    su := GetNextWord(s);

    if (el is TTextElement) then
    begin
      sz := GetTextSize(ACanvas, el, su);

      el.TH := sz.cy;

      des := GetTextDescent(ACanvas, el);
      if des > Descent then
        Descent := des;

      if sz.sub > BaseLine then
        BaseLine := sz.sub;

      if sz.sup > TopLine then
        TopLine := sz.sup;

      if sz.cy > LineHeight then
        LineHeight := sz.cy;

      if (LineWidth + sz.cx < MaxLineWidth) or ((offsetx + sz.cx > MaxLineWidth) and (FromWordIndex = ToWordIndex) and (sz.cx > GetClientWidth)) then
      begin
        LineWidth := LineWidth + sz.cx;
        pt.X := pt.X + sz.cx;
        inc(ToWordIndex);
      end
      else
      begin
        if (s = '') and (FromWordIndex = ToWordIndex) and (sz.cx > GetClientWidth) then
        begin
          LineWidth := LineWidth + sz.cx;

          nel := NextElement(el);
          if (nel is TLineBreakElement) then
            inc(ToElementIndex);

          inc(ToElementIndex);

          ToWordIndex := 0;
        end
        else
          if (FromWordIndex = ToWordIndex) then
          begin
            if (s = '') then
            begin
              inc(ToElementIndex);
              ToWordIndex := 0;
            end
            else
              inc(ToWordIndex);
          end;

        Break;
      end;
    end;

    if (s = '') then
    begin
      inc(ToElementIndex);
      ToWordIndex := 0;

      if (ToElementIndex < ElementCount) then
      begin
        el := FContext.Content.Items[ToElementIndex];
        el.XY := pt;

        if (el.Alignment <> Alignment) and (el is TTextElement) then
          break;

        if (el is TBulletEnd) then
        begin
          inc(ToElementIndex);
          Break;
        end;

        if  (el is TTextElement) then
          s := (el as TTextElement).DisplayText;
      end
      else
        Break;
    end;
  end;

  LineHeight := LineHeight + TopLine + BaseLine;
end;

function TAdvRichEditorBase.CanUndo: boolean;
begin
  Result := (FUndoRedoIndex > 0) or ((FUndoRedoIndex = 0) and (FUndoRedo.Count = 1));
end;

function TAdvRichEditorBase.CanRedo: boolean;
begin
  Result := FUndoRedoIndex < FUndoRedo.Count - 1;
end;

procedure TAdvRichEditorBase.CaretChanged(Sender: TObject);
begin
  Refresh;
end;

procedure TAdvRichEditorBase.CaretToSelection;
begin
  Selection.FromElement := Caret.Element;
  Selection.ToElement := Caret.Element;
  Selection.FromChar := Caret.CharIndex;
  Selection.ToChar := Caret.CharIndex;
end;

procedure TAdvRichEditorBase.CharCommand(AChar: Word;
  ACommandState: TCharCommandState);
var
  SelFromEl: TREElement;
  SelFromChar: Integer;
  Bullet: boolean;
  s: string;
  SP: TPoint;
  OldCaret: TREElement;
  OldCaretIndex: integer;
  OldSelectionFrom: TREElement;
  OldSelectionFromIndex: integer;
  OldSelectionTo: TREElement;
  OldSelectionToIndex: integer;
  IsSel: boolean;
begin
  OldCaret := Caret.Element;
  OldCaretIndex := Caret.CharIndex;

  OldSelectionFrom := Selection.FromElement;
  OldSelectionFromIndex := Selection.FromChar;
  OldSelectionTo := Selection.ToElement;
  OldSelectionToIndex := Selection.ToChar;

  if (Selection.FromElement = nil) and (Selection.ToElement = nil) then
    CaretToSelection;

  SP := TopLeft;

  case AChar of
  KEY_DELETE:
    begin
      if ACommandState = ccShift then
      begin
        if Assigned(Selected) and (Selected is TPictureElement) then
        begin
          CopyPictureToClipboard((Selected as TPictureElement).Picture);
        end
        else
          CopyToClipboard(SelectedText);
      end;

      if Assigned(Selected) and (Selected is TCustomGraphicElement) then
        DeleteSelected;

      if HasSelection then
      begin
        DeleteSelection;
        CaretToSelection;
      end
      else
      begin
        bullet := CaretInBullet;

        if HasSelection then
          DeleteSelection
        else
          DelChar(false);

        if bullet then
          OrderBullets(Caret.Element);
      end;
    end;
  KEY_RIGHT:
    begin
      FDoUndo := true;
      case ACommandState of
      ccNormal:NextChar;
      ccShift:NextCharSel;
      ccCtrl:NextWord;
      ccCtrlShift:NextWordSel;
      end;

      DoPaintEditor;

      if Caret.XY.X - TopLeft.X >= GetClientWidth - offsetx then
        ScrollHorz(Caret.XY.X - GetClientWidth + offsetx);
    end;
  KEY_LEFT:
    begin
      FDoUndo := true;
      case ACommandState of
      ccNormal:PrevChar;
      ccShift:PrevCharSel;
      ccCtrl:PrevWord;
      ccCtrlShift:PrevWordSel;
      end;

      DoPaintEditor;

      if Caret.XY.X - offsetx < TopLeft.X then
        ScrollHorz(Caret.XY.X - offsetx);
    end;
  KEY_PRIOR:
    begin
      FDoUndo := true;

      SelFromEl := Selection.FromElement;
      SelFromChar := Selection.FromChar;

      if PageUp then
        XYToCaret(Caret.XY.X, Caret.XY.Y - SP.Y,False,IsSel)
      else
        XYToCaret(OffsetX, OffsetY + 2,False,IsSel);

      if ACommandState = ccShift then
      begin
        if SelFromEl = nil then
        begin
          Selection.FromElement := Elements[0];
          Selection.FromChar := 0;
        end
        else
        begin
          Selection.FromElement := SelFromEl;
          Selection.FromChar := SelFromChar;
        end;
        Selection.ToElement := Caret.Element;
        Selection.ToChar := Caret.CharIndex;
      end
      else
        CaretToSelection;
    end;
  KEY_NEXT:
    begin
      FDoUndo := true;

      SelFromEl := Selection.FromElement;
      SelFromChar := Selection.FromChar;

      if PageDown then
        XYToCaret(Caret.XY.X, Caret.XY.Y - SP.Y + Caret.LH,False,IsSel)
      else
        XYToCaret(Width,Height,False,IsSel);


      if ACommandState = ccShift then
      begin
        Selection.FromElement := SelFromEl;
        Selection.FromChar := SelFromChar;
        Selection.ToElement := Caret.Element;
        Selection.ToChar := Caret.CharIndex;
      end
      else
        CaretToSelection;
    end;
  VK_Z:
    begin
      if ACommandState = ccCtrl then
        Undo;
    end;
  VK_Y:
    begin
      if ACommandState = ccCtrl then
        Redo;
    end;
  VK_A:
    begin
      if ACommandState = ccCtrl then
        SelectAll;
    end;
  VK_C:
    begin
      if ACommandState = ccCtrl then
      begin
        s := SelectedText;
        if (Selected is TCustomGraphicElement) then
        begin
          if (Selected is TPictureElement) then
            CopyPictureToClipboard((Selected as TPictureElement).Picture);
          // copy image to clipboard
        end
        else
          CopyToClipboard(s);
      end;
    end;
  VK_B:
    begin
      SetSelectionBold(not IsSelectionBold);
    end;
  VK_I:
    begin
      SetSelectionItalic(not IsSelectionItalic);
    end;
  VK_U:
    begin
      SetSelectionUnderline(not IsSelectionUnderline);
    end;
  VK_L:
    begin
      SetSelectionAttribute(taLeftJustify);
    end;
  VK_E:
    begin
      SetSelectionAttribute(taCenter);
    end;
  VK_R:
    begin
      SetSelectionAttribute(taRightJustify);
    end;
  VK_X:
    begin
      if ACommandState = ccCtrl then
      begin
        if Assigned(Selected) and (Selected is TPictureElement) then
        begin
          CopyPictureToClipboard((Selected as TPictureElement).Picture);
          DeleteSelected;
        end
        else
          CopyToClipboard(SelectedText);

        if Assigned(Selected) and (Selected is TCustomGraphicElement) then
          DeleteSelected;

        DeleteSelection;
      end;
    end;
  VK_V, KEY_INSERT:
    begin
      if ((ACommandState = ccCtrl) and (AChar = VK_V)) or ((ACommandState = ccShift) and (AChar = KEY_INSERT)) then
      begin
        FLastElement := nil;

        PasteFromClipboard;

        if Assigned(FLastElement) then
        begin
          Caret.Element := FLastElement;

          if FLastElement is TTextElement then
            Caret.CharIndex := Length(FLastElement.Text)
          else
            Caret.CharIndex := 1;
        end;
      end;
    end;
  KEY_UP:
    begin
      FDoUndo := true;
      Caret.EOL := false;
      SelFromEl := Selection.FromElement;
      SelFromChar := Selection.FromChar;

      XYToCaret(Caret.XY.X, Max(0, Caret.XY.Y - SP.Y - 4),False,IsSel);

      if Caret.XY.Y - SP.Y - Caret.LH - 4 < offsetx then
        ScrollUp(Caret.LH);

      if ACommandState = ccShift then
      begin
        Selection.FromElement := SelFromEl;
        Selection.FromChar := SelFromChar;
        Selection.ToElement := Caret.Element;
        Selection.ToChar := Caret.CharIndex;
      end
      else
      begin
        CaretToSelection;
      end;
    end;
  KEY_DOWN:
    begin
      FDoUndo := true;
      Caret.EOL := false;
      SelFromEl := Selection.FromElement;
      SelFromChar := Selection.FromChar;

      XYToCaret(Caret.XY.X, Caret.XY.Y - SP.Y + Caret.LH + Caret.LH div 4, False, IsSel);

      if Caret.XY.Y -SP.Y + 2 * Caret.LH + 4 > Height then
        ScrollDown(Caret.LH);

      if ACommandState = ccShift then
      begin
        if SelFromEl = nil then
        begin
          Selection.FromElement := Elements[0];
          Selection.FromChar := 0;
        end
        else
        begin
          Selection.FromElement := SelFromEl;
          Selection.FromChar := SelFromChar;
        end;
        Selection.ToElement := Caret.Element;
        Selection.ToChar := Caret.CharIndex;
      end;
    end;
  KEY_HOME:
    begin
      FDoUndo := true;
      ScrollHorz(0);
      Caret.EOL := false;
      if ACommandState = ccCtrlShift then
      begin
        SelFromEl := Caret.Element;
        SelFromChar := Caret.CharIndex;

        Caret.Element := FContext.Content.Items[0];
        Caret.CharIndex := 0;

        Selection.FromElement := SelFromEl;
        Selection.FromChar := SelFromChar;
        Selection.ToElement := Caret.Element;
        Selection.ToChar := Caret.CharIndex;
        Refresh;
      end
      else
      if ACommandState = ccShift then
      begin
        if not HasSelection then
        begin
          SelFromEl := Caret.Element;
          SelFromChar := Caret.CharIndex;
          XYToCaret(OffsetX, Caret.XY.Y - SP.Y + 4, False, IsSel);
          Caret.EOL := false;
          Selection.FromElement := SelFromEl;
          Selection.FromChar := SelFromChar;
          Selection.ToElement := Caret.Element;
          Selection.ToChar := Caret.CharIndex;
        end
        else
        begin
          SelFromEl := Selection.FromElement;
          SelFromChar := Selection.FromChar;
          XYToCaret(OffsetX, Caret.XY.Y - SP.Y + 4, False, IsSel);
          Caret.EOL := false;
          Selection.ToElement := Caret.Element;
          Selection.ToChar := Caret.CharIndex;
          Selection.FromElement := SelFromEl;
          Selection.FromChar := SelFromChar;
        end;
      end
      else
      begin
        if ACommandState = ccCtrl then
        begin
          ScrollHome;
          if ElementCount > 0 then
          begin
            Caret.Element := Elements[0];
            Caret.CharIndex := 0;
            Caret.EOL := false;
            Refresh;
          end;
        end
        else
          XYToCaret(OffsetX, Caret.XY.Y -SP.Y + 4, False, IsSel);
      end;
    end;

  KEY_END:
    begin
      FDoUndo := true;

      ScrollHorz(1000);

      if ACommandState = ccShift then
      begin
        if not HasSelection then
        begin
          SelFromEl := Caret.Element;
          SelFromChar := Caret.CharIndex;
          XYToCaret(GetClientWidth - 4 - OffsetX, Caret.XY.Y - SP.Y + 4, False, IsSel);
          Caret.EOL := true;
          Selection.FromElement := SelFromEl;
          Selection.FromChar := SelFromChar;
          Selection.ToElement := Caret.Element;
          Selection.ToChar := Caret.CharIndex;
        end
        else
        begin
          SelFromEl := Selection.FromElement;
          SelFromChar := Selection.FromChar;
          XYToCaret(GetClientWidth - 4 - OffsetX, Caret.XY.Y - SP.Y + 4, False, IsSel);
          Caret.EOL := true;
          Selection.ToElement := Caret.Element;
          Selection.ToChar := Caret.CharIndex;
          Selection.FromElement := SelFromEl;
          Selection.FromChar := SelFromChar;
        end;
      end
      else
        if ACommandState = ccCtrl then
        begin
          ScrollEnd;
          if ElementCount > 0 then
          begin
            Caret.Element := Elements[ElementCount - 1];
            Caret.CharIndex := Length(Caret.Element.Text);
            Caret.EOL := true;
            Refresh;
          end;
        end
        else
        if ACommandState = ccCtrlShift then
        begin
          Selection.FromElement := Caret.Element;
          Selection.FromChar := Caret.CharIndex;

          if ElementCount > 0 then
          begin
            Caret.Element := Elements[ElementCount - 1];
            Caret.CharIndex := Length(Caret.Element.Text);
            Caret.EOL := true;
          end;

          Selection.ToElement := Caret.Element;
          Selection.ToChar := Caret.CharIndex;
          Refresh;

        end
        else
        begin
          XYToCaret(GetClientWidth - 4 - OffsetX, Caret.XY.Y - SP.Y + 4, False, IsSel);
          Caret.EOL := true;
        end;
      end;
    end;

  FDefaultChanged := false;

  if (Caret.Element <> OldCaret) or (Caret.CharIndex <> OldCaretIndex) then
  begin
    DoCaretChanged;
  end;

  if (OldSelectionFrom <> Selection.FromElement) or (OldSelectionFromIndex <> Selection.FromChar)
    or (OldSelectionTo <> Selection.ToElement) or (OldSelectionToIndex <> Selection.ToChar) then
  begin
    DoSelectionChanged;
  end;
end;

procedure TAdvRichEditorBase.Clear;
begin
  FUndoRedoIndex := 0;
  FModified := false;
  FContext.Clear;
  FSelection.FromElement := nil;
  FSelection.ToElement := nil;
  FCaret.Element := nil;
  FCaret.CharIndex := 0;
  FLastElement := nil;
  Selected := nil;
  FFontBaseline := tbRegular;
  FAlignment := taLeftJustify;
  Font.Style := [];
  UpdateSize;
end;

procedure TAdvRichEditorBase.ClearSelection;
begin
  Selection.FromElement := nil;
  Selection.ToElement := nil;
  Selection.ToChar := 0;
  Selection.FromChar := 0;
  UpdateSelection;
end;

procedure TAdvRichEditorBase.ClearErrors;
var
  re: TREElement;
begin
  BeginUpdate;
  for re in FContext.Content do
  begin
    if (re is TTextElement) then
      (re as TTextElement).Error := false;
  end;
  TidyElements;

  EndUpdate;
  Refresh;

end;

constructor TAdvRichEditorBase.Create(AOwner: TComponent);
begin
  inherited;
  FDoUndo := True;
  FLastResult := True;
  FClassesRegistered := False;
  FDefaultChanged := False;
  FContext := TContext.Create(Rect(0,0,0,0));
  FMergeContext := TContext.Create(Rect(0,0,0,0));
  FStyles := TStyleElements.Create;
  FUndoRedo := TContexts.Create;
  FUndoRedoIndex := -1;
  FAutoCorrect := TRichEditorAutoCorrect.Create;
  FSelectionElements := TElements.Create;
  FFindPos := TCaret.Create;
  FSelectionType := stWord;

  FCaret := TCaret.Create;
  FCaret.Element := nil;
  FCaret.CharIndex := -1;
  FCaret.OnChange := CaretChanged;

  FDragCaret := TCaret.Create;
  FTempCaret := TCaret.Create;

  FSelection := TSelection.Create;
  FSelection.OnChange := SelectionChanged;

  FPaintSelection := TSelection.Create;
  FTempSelection := TSelection.Create;

  FWord := TSelection.Create;

  FURLAuto := uAuto;
  FURLColor := clBlue;
  FSelectionColor := clBlue;
  FSelectionTextColor := clWhite;
  FHighlightColor := clYellow;
  FHighlightTextColor := clBlack;
  FClip := true;

  FPageMargin := TPageMargin.Create;
  FPageMargin.OnChange := SelectionChanged;

  FBuffer := nil;

  FFontBaseLine := tbRegular;

  FTabSize := 100;
  FWantTab := false;
  FDPIratio := 1.0;
  FBulletSpacing := 0;
  FHideSelection := false;
  FHTMLImageList := THTMLImageList.Create;
  FRTFEngine := TRTFEngine.Create;
end;

procedure TAdvRichEditorBase.Assign(Source: TPersistent);
begin
  if (Source is TAdvRichEditorBase) then
  begin
    FHighlightColor := (Source as TAdvRichEditorBase).HighlightColor;
    FHighlightTextColor := (Source as TAdvRichEditorBase).HighlightTextColor;
    FSelectionColor := (Source as TAdvRichEditorBase).SelectionColor;
    FSelectionTextColor := (Source as TAdvRichEditorBase).SelectionTextColor;
    FURLColor := (Source as TAdvRichEditorBase).URLColor;
    FPageWidth := (Source as TAdvRichEditorBase).PageWidth;
    FPageHeight := (Source as TAdvRichEditorBase).PageHeight;
    FTabSize := (Source as TAdvRichEditorBase).TabSize;
    FBulletSpacing := (Source as TAdvRichEditorBase).BulletSpacing;
  end;
end;

procedure TAdvRichEditorBase.CutToClipboard;
begin
  if Assigned(Selected) and (Selected is TPictureElement) then
  begin
    CopyPictureToClipboard((Selected as TPictureElement).Picture);
    DeleteSelected;
  end
  else
    CopyToClipboard(SelectedText);

  DeleteSelection;
end;

procedure TAdvRichEditorBase.DeleteCaretElement;
begin
  DelCaretElement(true);
end;

procedure TAdvRichEditorBase.DelCaretElement(const Back: boolean);
var
  pel,nel: TREElement;
begin
  pel := PreviousElement(Caret.Element);
  nel := NextElement(Caret.Element);

  if Back then
  begin
    DeleteElement(Caret.Element);

    if Assigned(pel) then
    begin
      Caret.CharIndex := pel.TextLength;
    end
    else
      Caret.Element := nel;
  end
  else
  begin
    DeleteElement(Caret.Element);

    if Assigned(nel) then
    begin
      Caret.Element := nel;
      Caret.CharIndex := 0;
    end
    else
    begin
      Caret.Element := pel;
      Caret.CharIndex := LastIndex(pel);
    end;
  end;
end;

procedure TAdvRichEditorBase.TidyElements;
var
  i: integer;
  el,nel: TREElement;
begin
  i := 0;

  while i < ElementCount do
  begin
    el := FContext.Content[i];

    if (el is TTextElement) and (i < ElementCount - 1) then
    begin
      inc(i);
      nel := FContext.Content[i];

      if (nel is TTextElement) then
      begin
        if el.Equal(nel) then
        begin

          if (Selection.FromElement = nel) then
          begin
            Selection.FromElement := el;
            Selection.FromChar := Selection.FromChar + el.TextLength;
          end;

          if (Selection.ToElement = nel) then
          begin
            Selection.ToElement := el;
            Selection.ToChar := Selection.ToChar + el.TextLength;
          end;

          if (Caret.Element = nel) then
          begin
            Caret.Element := el;
            Caret.CharIndex := Caret.CharIndex + el.TextLength;
          end;

          el.Text := el.Text + nel.Text;

          DeleteElement(nel);
          dec(i);
        end;
      end;
    end
    else
      inc(i);
  end;
end;

procedure TAdvRichEditorBase.DoEnterWord;
var
  s: string;
  ci: integer;
  pel: TREElement;
begin
  if Assigned(OnEnterWord) then
  begin
    pel := Caret.Element;
    ci := Caret.CharIndex - 1;

    if not (pel is TTextElement) then
    begin
      pel := NearbyTextElement(pel, Caret.CharIndex);
      ci := pel.TextLength;
    end;

    s := GetWordAtCaret(pel, ci, ci, False, False);
    if (s <> '') then
      OnEnterWord(Self,S);
  end;
end;

procedure TAdvRichEditorBase.DoCorrectWord;
var
  s,so,aso,cs: string;
  ci,nci,dc: integer;
  Error, Found: boolean;
  pel,el: TREElement;
  i: integer;

begin
  if Assigned(OnCorrectWord) or Assigned(OnAutoCorrectWord) or (AutoCorrect.OldValue.Count > 0) then
  begin
    pel := Caret.Element;
    ci := Caret.CharIndex - 1;

    if not (pel is TTextElement) then
    begin
      pel := NearbyTextElement(pel, Caret.CharIndex);
      ci := pel.TextLength - 1;
    end;

    s := GetWordAtCaret(pel, ci, nci, False, False);

    if (s <> '') and (Caret.CharIndex > 0) then
    begin
      Error := false;
      Found := false;
      so := s;

      if not AutoCorrect.CaseSensitive then
        aso := LowerCase(so)
      else
        aso := s;

      for i := 0 to AutoCorrect.OldValue.Count - 1 do
      begin
        cs := AutoCorrect.OldValue.Strings[i];
        if not AutoCorrect.CaseSensitive then
          cs := LowerCase(cs);

        if (aso = cs) and (i < AutoCorrect.NewValue.Count)  then
        begin
          s := AutoCorrect.NewValue.Strings[i];
          Found := true;
        end;
      end;

      if Found then
      begin
        dc := Length(s) - Length(so);
        SetWordAtCaret(pel, ci, s);
        Caret.Element := pel;
        Caret.CharIndex := ci + dc + 1;
      end
      else
      begin
        if Assigned(OnAutoCorrectWord) and not Found then
          OnAutoCorrectWord(Self, S, Error);

        if Assigned(OnCorrectWord) and not Found then
          OnCorrectWord(Self, S, Error);

        if (so <> s) then

           SetWordAtCaret(pel, ci, s);

           Caret.Element := pel;
           Caret.CharIndex := ci;
           SelectWordAtCaret;


        if not Error then
        begin
          if Selection.ToElement.TextLength > Selection.ToChar then
          begin
            s := Selection.ToElement.Text;
            if IsSeparator(CharInStr(s,Selection.ToChar + 1)) then
              Selection.ToChar := Selection.ToChar + 1;
          end;
        end;

        SetSelectionError(Error);

        el := NextElement(Selection.ToElement);
        if Assigned(el) then
        begin
          Caret.Element := el;
          Caret.CharIndex := 1;
        end
        else
        begin
          Caret.Element := Selection.ToElement;
          Caret.CharIndex := Caret.Element.TextLength;
        end;

        CaretToSelection;
      end;
    end;
  end;
end;


procedure TAdvRichEditorBase.DeleteChar;
begin
  DelChar(true);
end;

procedure TAdvRichEditorBase.DelChar(const Back: boolean);
var
  s: string;
  el,nel: TREElement;
  i: integer;
  bul: TBulletElement;

begin
  if FDoUndo then
  begin
    PushContext;
    FDoUndo := false;
  end;

  if Assigned(Caret.Element) then
  begin
    if (Caret.Element is TTextElement) then
    begin
      s := (Caret.Element as TTextElement).Text;

      if (Caret.CharIndex = Length(s)) then
      begin
        el := NextElement(Caret.Element);
        if Assigned(el) then
        begin
          Caret.Element := el;
          Caret.CharIndex := 0;
          DelChar(Back);
        end;
      end
      else
      begin
        if Length(s) > 0 then
        begin
          Delete(s, Caret.CharIndex + 1, 1);
          Modified := true;
          if s <> '' then
            (Caret.Element as TTextElement).Text := s
          else
            DelCaretElement(Back);
        end;
      end;
    end
    else
    if (Caret.Element is TBulletElement) then
    begin
      el := NextElement(Caret.Element);

      if (Caret.CharIndex = 0) and not Back then
      begin
        DelCaretElement(Back);
        if (Caret.Element is TLineBreakElement) then
          DelCaretElement(Back);
      end
      else
      begin
        if Assigned(el) then
        begin
          Caret.Element := el;
          Caret.CharIndex := 0;
          DelChar(Back);
        end;
      end;
    end
    else
    if (Caret.Element is TCustomGraphicElement) then
    begin
      el := NextElement(Caret.Element);

      if Caret.CharIndex = 0 then
      begin
        DeleteCaretElement;
        if Assigned(el) and (el is TBulletElement) and (Caret.Element is TBulletElement) then
          DelCaretElement(Back);
      end
      else
      begin
        if Assigned(el) then
        begin
          Caret.Element := el;
          Caret.CharIndex := 0;
          DelChar(Back);
        end;
      end;
    end
    else
    if (Caret.Element is TBulletEnd) then
    begin
      nel := NextElement(Caret.Element);
      if Assigned(nel) then
      begin
        if not (nel is TTextElement) then
          DeleteElement(nel)
        else
        begin
          i := ElementIndex(nel);

          FContext.Content.Items[i - 1] := nel;
          FContext.Content.Items[i] := Caret.Element;

          Caret.Element := nel;
          Caret.CharIndex := 0;
          Caret.EOL := false;
        end;
      end;
    end
    else
    if (Caret.Element is TLineBreakElement) then
    begin
      el := NextElement(Caret.Element);

      if Caret.CharIndex = 0 then
      begin
        DelCaretElement(Back);

        if el is TBulletEnd then
        begin
          nel := NextElement(el);
          if Assigned(nel) then
          begin
            i := ElementIndex(el);
            FContext.Content.Items[i] := nel;
            FContext.Content.Items[i + 1] := el;
            Caret.Element := nel;
            Caret.CharIndex := 0;
          end;
        end;

        if el is TBulletElement then
          DeleteElement(el);
      end
      else
      begin
        if Assigned(el) then
        begin
          Caret.Element := el;
          Caret.CharIndex := 0;
          DelChar(Back);
        end;
      end;
    end;
  end;

  bul := NextBullet(Caret.Element);
  OrderBullets(bul);

  if (Caret.Element is TBulletStart) then
  begin
    Caret.Element := NextElement(Caret.Element);
    Caret.CharIndex := 1;
  end;

  CaretToSelection;
  Refresh;
end;

function TAdvRichEditorBase.DeleteElement(AElement: TREElement): integer;
var
  nel,pel: TREElement;

begin
  Result := -1;

  if not Assigned(AElement) then
    Exit;

  if EditCanModify then
  begin
    nel := NextElement(AElement);
    pel := PreviousElement(AElement);

    if Selection.FromElement = AElement then
    begin
      Selection.FromElement := NextElement(AElement);
      Selection.FromChar := 0;
    end;

    if Selection.ToElement = AElement then
    begin
      Selection.ToElement := PreviousElement(AElement);
      if Assigned(Selection.ToElement) then
        Selection.ToChar := Length(Selection.ToElement.Text)
      else
        Selection.ToChar := 0;
    end;

    if Caret.Element = AElement then
    begin
      Caret.Element := NextElement(AElement);
      Caret.CharIndex := 0;
    end;

    if (Selected = AElement) then
      Selected := nil;

    Result := ElementIndex(AElement);
    FContext.Content.Delete(Result);
    AElement.Free;

    // reduce indents when deleting elements
    if Assigned(nel) and not (nel is TBulletStart) and not (nel is TBulletEnd) then
    begin
      if Assigned(pel) then
        nel.Indent := pel.Indent
      else
        nel.Indent := 0;
    end;

    FModified := true;
    UpdateSize;
  end;
end;

procedure TAdvRichEditorBase.DeleteSelected;
var
  i: integer;
  pel: TREElement;
begin
  i := ElementIndex(Selected);

  if i >= 0 then
  begin
    if EditCanModify then
    begin
      FModified := true;

      pel := PreviousElement(Selected);

      {$IFDEF DELPHI_LLVM}
      Selected.DisposeOf;
      {$ELSE}
      Selected.Free;
      {$ENDIF}
      Selected := nil;
      FContext.Content.Delete(i);
      Refresh;

      Caret.Element := pel;
      if Assigned(pel) then
        Caret.CharIndex := Caret.Element.IndexAtEnd
      else
        Caret.CharIndex := 0;
      EnsureCaret;
    end;
  end;
end;

procedure TAdvRichEditorBase.DeleteSelection;
var
  el,car: TREElement;
  s: string;
  i,carfc: integer;
  done: boolean;
  bs,be: integer;
  pel,nel: TREElement;

begin
  if EditCanModify then
  begin
    PushContext;

    SelectionToPaintSelection;

    el := PaintSelection.FromElement;

    pel := PreviousElement(PaintSelection.FromElement);
    nel := NextElement(PaintSelection.ToElement);

    if not Assigned(el) then
      Exit;

    if (PaintSelection.FromElement = PaintSelection.ToElement) and
       (PaintSelection.FromElement is TLineBreakElement) and
       (Caret.Element = PaintSelection.FromElement) then
      Exit;

    FLastElement := nil;

    bs := 0;
    be := 0;

    car := el;
    carfc := PaintSelection.FromChar;

    if (Caret.Element = el) and (Caret.CharIndex = 0) then
    begin
      car := PreviousElement(el);
      if Assigned(car) then
      begin
        if (car is TTextElement) then
          carfc := car.TextLength
        else
          carfc := 1;
      end
      else
        carfc := 0;
    end;

    if (Caret.Element = PaintSelection.ToElement) and (Caret.CharIndex = Length(Caret.Element.Text)) then
    begin
      if Assigned(nel) then
      begin
        car := nel;
        carfc := 0;
      end;

      if not Assigned(car) then
      begin
        car := PreviousElement(el);
        if Assigned(car) then
          carfc := SelectIndexAtEnd(el)
        else
          carfc := 0;
      end;
    end;

    FModified := true;

    done := false;

    repeat
      if (el is TTextElement) then
      begin
        s := el.Text;
        if (el <> PaintSelection.ToElement) and (el = PaintSelection.FromElement) then
        begin
          s := Copy(S, 1, PaintSelection.FromChar);
          el.Text := s;
          i := ElementIndex(el);

          if s = '' then
          begin
            if (el = car) then
            begin
              car := pel;
              if Assigned(pel) then
                carfc := pel.IndexAtEnd
              else
                carfc := 0;
            end;
            DeleteElement(el);
          end
          else
          begin
            carfc := PaintSelection.FromChar;
            inc(i);
          end;

          if i < ElementCount then
            el := FContext.Content.Items[i]
          else
            done := true;
        end
        else
        if (el = PaintSelection.ToElement) and (el = PaintSelection.FromElement) then
        begin
          s := Copy(s, 1, PaintSelection.FromChar) + Copy(s, PaintSelection.ToChar + 1, Length(s));
          if s = '' then
          begin
            car := PreviousElement(el);
            DeleteElement(el);
            Caret.Element := car;
            if Assigned(car) then
              carfc := LastIndex(car);
          end
          else
          begin
            el.Text := s;
            carfc := PaintSelection.FromChar;
            car := el;
          end;
          done := true;
        end
        else
        if (el <> PaintSelection.ToElement) and (el <> PaintSelection.FromElement) then
        begin
          i := ElementIndex(el);
          FContext.Content.Delete(i);
          if (el = car) then
            car := nil;
          el.Free;

          if i < ElementCount then
            el := FContext.Content.Items[i]
          else
            done := true;
        end
        else
        begin
          s := Copy(s, PaintSelection.ToChar + 1, Length(s));
          el.Text := s;
          if s = '' then
          begin
            i := ElementIndex(el);
            FContext.Content.Delete(i);
            if (el = car) then
              car := nil;
            el.Free;
          end;

          done := true;
        end;
      end
      else
      begin
        if (el is TBulletStart) then
        begin
          inc(bs);
        end;

        if (el is TBulletEnd) then
        begin
          if bs > 0 then
            dec(bs)
          else
            inc(be);
        end;

        if (el = PaintSelection.ToElement) then
        begin

          if (PaintSelection.ToChar > 0) or not (PaintSelection.ToElement is TTextElement) then
          begin
            if (car = el) then
            begin
              car := PreviousElement(el);
              carfc := LastIndex(car);
            end;

            DeleteElement(el);
          end
          else
          begin
            car := el;
            carfc := 0;
          end;

          done := true
        end
        else
        begin
          i := ElementIndex(el);

          if (i >= 0) and (i < FContext.Content.Count) then
          begin
            FContext.Content.Delete(i);
            if (el = car) then
              car := nil;
            el.Free;
          end;

          if i < FContext.Content.Count then
            el := FContext.Content.Items[i]
          else
            done := true;
        end;
      end;
    until done;

    Caret.EOL := false;
    Caret.Element := car;
    Caret.CharIndex := carfc;

    UnSelect;

    if not Assigned(car) then
      EnsureCaret;

    // auto correct bullet start / bullet end when deleting in middle of bullet list
    if (bs > be) then
    begin
      // restore a bullet start
      if (pel = nil) then
      begin
        if (FContext.Content.Count > 0) then
        begin
          pel := FContext.Content.Items[0];
          pel := NextBullet(pel);
          pel := InsertBeforeElement(pel,TBulletStart);
        end
        else
          pel := InsertElement(0,TBulletStart)
      end
      else
        pel := InsertAfterElement(pel,TBulletStart);

      OrderBullets(pel);
    end
    else
      if bs < be then
      begin
        if Assigned(nel) then
          pel := InsertBeforeElement(nel, TBulletEnd)
        else
          pel := AddElement(TBulletEnd);

        OrderBullets(pel);
      end;

    // auto reorder bullets when delete in bullet list
    if CaretInBullet then
    begin
      FixBullets(Caret.Element);
      OrderBullets(Caret.Element);
    end;

    UpdateSize;

    EnsureCaret;
    CaretToSelection;

    Refresh;
  end;
end;

destructor TAdvRichEditorBase.Destroy;
var
  i: integer;
begin
  FRTFEngine.Free;
  FHTMLImageList.Free;
  ClearContext;
  Clear;
  FContext.Free;
  FMergeContext.Free;
  FFindPos.Free;
  FSelectionElements.Free;
  FCaret.Free;
  FDragCaret.Free;
  FTempCaret.Free;
  FTempSelection.Free;
  FSelection.Free;
  FWord.Free;
  FAutoCorrect.Free;
  FPaintSelection.Free;

  for i := 0 to FUndoRedo.Count - 1 do
  begin
    FUndoRedo.Items[i].Clear;
    {$IFDEF DELPHI_LLVM}
    FUndoRedo.Items[i].DisposeOf;
    {$ELSE}
    FUndoRedo.Items[i].Free;
    {$ENDIF}
  end;

  FUndoRedo.Free;

  for i := 0 to FStyles.Count - 1 do
  {$IFDEF DELPHI_LLVM}
    FStyles.Items[i].DisposeOf;
  {$ELSE}
    FStyles.Items[i].Free;
  {$ENDIF}
  FStyles.Clear;
  FStyles.Free;
  FPageMargin.Free;

  if Assigned(FBuffer) then
    FBuffer.Free;

  inherited;
end;

procedure TAdvRichEditorBase.DoCaretChanged;
begin
  if Assigned(OnCaretChanged) then
    OnCaretChanged(Self);

  if Assigned(OnToolBarUpdate) then
    OnToolBarUpdate(Self);
end;

procedure TAdvRichEditorBase.DoSelectionChanged;
begin
  if Assigned(OnSelectionChanged) then
    OnSelectionChanged(Self);

  if Assigned(OnToolBarUpdate) then
    OnToolBarUpdate(Self);
end;

procedure TAdvRichEditorBase.DrawLine(ACanvas: TCanvas; X, Y, FromElementIndex, FromWordIndex, MaxLineWidth, LineWidth,
  LineHeight, Descent, BaseLine: integer; DoDraw: boolean = true);
var
  el,nel: TREElement;
  s,su,dcsu,lw: string;
  sz: TTextSize;
  CharsSkipped, wl, cx, tx: integer;
  FAlign: TAlignment;
  SP: TPoint;
  dcx,dcy,dskip,orgx: integer;
  del: TREElement;
  ToWordIndex: integer;
  firstbul: boolean;
begin
  LineWidth := 0;
  CharsSkipped := 0;
  ToWordIndex := FromWordIndex;

  //TDebug.Write('Draw line at: %d',[x]);

  s := '';
  SP := TopLeft;
  orgx := x;

  el := FContext.Content.Items[FromElementIndex];

  firstbul := el is TBulletEnd;
  nel := el;

  FAlign := el.Alignment;

  if (FromWordIndex = 0) then
  begin
    if firstbul then
    begin
      nel := NextElement(el);
      if not Assigned(nel) then
        nel := el;
    end
    else
      nel := el;

    nel.XY := Point(X + nel.Indent + SP.X, Y + SP.Y);
  end;

  X := X + Round(nel.Indent * DPIratio);

  if FromWordIndex = 0 then
    el.LH := LineHeight;

  if (el is TTextElement) then
    s := (el as TTextElement).DisplayText;

  if (el is TBulletEnd) then
  begin
    FIndent := 0;
  end;

  if FromWordIndex > 0 then
  begin
    s := SkipWords(s, FromWordIndex, CharsSkipped);
  end;

  dcx := -1;
  dcy := -1;
  dskip := 0;
  del := nil;
  lw := '';

  while (LineWidth < MaxLineWidth - SP.X) do
  begin
    el.Selected := false;

    if Assigned(PaintSelection.FromElement) and Assigned(PaintSelection.ToElement) then
    begin
      if (FContext.Content.IndexOf(el) >= FContext.Content.IndexOf(PaintSelection.FromElement)) and
         (FContext.Content.IndexOf(el) <= FContext.Content.IndexOf(PaintSelection.ToElement)) then
      begin
        el.Selected := true;

        if (el = PaintSelection.FromElement) then
          el.SelFrom:= PaintSelection.FromChar
        else
          el.SelFrom := -1;

        if (el = PaintSelection.ToElement) then
          el.SelTo := PaintSelection.ToChar
        else
          el.SelTo := -1;
      end;
    end;

    if (el is TBulletElement) then
    begin
      //TDebug.Write('caret baseline:%d descent:%d',[BaseLine, Descent]);
      if (el = Caret.Element) then
      begin
        if Caret.CharIndex = 0 then
          CalcCaret(ACanvas,el, x, y + SP.Y, LineHeight, Descent, BaseLine, ' ', 0, DoDraw)
        else
          CalcCaret(ACanvas,el, x, y + SP.Y, LineHeight, Descent, BaseLine, ' ', 0, DoDraw);
      end;
    end;

    if (el is TBulletEnd) then
    begin
      if (el = Caret.Element) then
      begin
        if Caret.CharIndex = 0 then
          CalcCaret(ACanvas,el, x, y + SP.Y, LineHeight, Descent, BaseLine, ' ', 0, DoDraw)
        else
          CalcCaret(ACanvas,el, x, y + SP.Y, LineHeight, Descent, BaseLine, ' ', 0, DoDraw)
      end;
    end;

    if (el is TBulletStart)  then
    begin
      x := orgx + Round(el.Indent * DPIratio);
      if (el = Caret.Element) then
      begin
        if Caret.CharIndex = 0 then
          CalcCaret(ACanvas,el, x, y + SP.Y, LineHeight, Descent, BaseLine, ' ', 0, DoDraw)
        else
          CalcCaret(ACanvas,el, x + el.Size.cx + FIndent, y + SP.Y, LineHeight, Descent, BaseLine, ' ', 0, DoDraw)
      end;
    end;

    if (el is TLineBreakElement) then
    begin
      if (el.Selected) and not HideSelection and ((Selection.FromElement <> el) or (Selection.ToElement <> el)) then
        DrawLineBreak(ACanvas,x - SP.X, y, el);

      if (el = Caret.Element) then
      begin
        if Caret.CharIndex = 0 then
          CalcCaret(ACanvas,el, x, y + SP.Y, LineHeight, Descent, BaseLine, ' ', 0, DoDraw)
        else
        begin
          cx := 0;
          case el.Alignment of
          taLeftJustify: cx := OffsetX + el.Indent;
          taCenter: cx := MaxLineWidth div 2;
          taRightJustify: cx := MaxLineWidth;
          end;
          CalcCaret(ACanvas,el, cx - SP.X, y + LineHeight + SP.Y, LineHeight, Descent, BaseLine, ' ', 0, DoDraw);
          Caret.NextLine := true;
        end;
      end;

      el.XYE := Point(SP.X + el.Indent, Y + SP.Y + LineHeight);

      if (el is TLineElement) then
      begin
        DrawElement(ACanvas, el, SP.X, Y + SP.Y, MaxLineWidth, LineHeight, Descent, BaseLine, '',0);
      end;

      Break;
    end;

    if (el is TBulletElement) then
    begin
      sz.cx := el.Size.cx;
      sz.cy := el.Size.cy;

      DrawElement(ACanvas, el, x - sz.cx - 2 - SP.X, y, MaxLineWidth, LineHeight, Descent, BaseLine, '', CharsSkipped);
    end;

    if (el is TCustomGraphicElement) then
    begin
      if (el = Caret.Element) then
        CalcCaret(ACanvas,el, x, y + SP.Y, LineHeight, Descent, Baseline, ' ', Caret.CharIndex, DoDraw);

      sz.cx := Round(el.Size.cx * DPIratio);
      sz.cy := Round(el.Size.cy * DPIratio);

      (el as TCustomGraphicElement).DPIratio := DPIratio;

      if DoDraw then
        DrawElement(ACanvas, el, x - SP.X, y, MaxLineWidth, LineHeight, Descent, BaseLine, '', CharsSkipped);

      x := x + sz.cx;
    end;

    if (el is TTabElement) then
    begin
      tx := x div TabSize;
      tx := (tx + 1) * TabSize;

      if (el = Caret.Element) then
      begin
        if Caret.CharIndex = 0 then
          CalcCaret(ACanvas,el, x, y + SP.Y, LineHeight, Descent, BaseLine, ' ', 0, DoDraw)
        else
          CalcCaret(ACanvas,el, tx, y + SP.Y, LineHeight, Descent, BaseLine, ' ', 0, DoDraw);
      end;

      x := tx;

      el.XYE := Point(SP.X + x, Y + SP.Y + LineHeight);

//      TDebug.Write('tab xy:');
//      TDebug.Write(el.XY);
//      TDebug.Write('tab xye:');
//      TDebug.Write(el.XYE);
    end;

    su := GetNextWord(s);

    wl := 0;

    if (el is TTextElement) then
    begin
      sz := GetTextSize(ACanvas, el, su);

      (el as TTextElement).TextWidth := sz.cx;
      (el as TTextElement).TextHeight := sz.cy;

      if (x + sz.cx < MaxLineWidth - SP.X) or ((x + sz.cx >= MaxLineWidth - SP.X) and (FromWordIndex = ToWordIndex) and (sz.cx > GetCLientWidth)) then
      begin
        if DoDraw then
        begin
          if s <> '' then
            DrawElement(ACanvas, el, x - SP.X, y, MaxLineWidth, LineHeight, Descent, BaseLine, su + ' ', CharsSkipped)
          else
            DrawElement(ACanvas, el, x - SP.X, y, MaxLineWidth, LineHeight, Descent, BaseLine, su, CharsSkipped);
        end;

        lw := su;

        wl := Length(su);

        if (el = Caret.Element) then
        begin
          if ((Caret.CharIndex >= CharsSkipped) and (Caret.CharIndex <= CharsSkipped + wl)) or ((Caret.CharIndex = -1) and (CharsSkipped = 0)) then
          begin
            dcx := x;
            dcy := y;
            del := el;
            dcsu := su;
            dskip := Caret.CharIndex - CharsSkipped;
          end;
        end;
        x := x + sz.cx;
      end
      else
      begin
        break;
      end;

      inc(ToWordIndex);
    end;

    CharsSkipped := CharsSkipped + wl;

    if (s = '') then
    begin
      CharsSkipped := 0;

      el.XYE := Point(x + SP.X, Y + SP.Y + LineHeight);

      inc(FromElementIndex);

      if (FromElementIndex < FContext.Content.Count) then
      begin
        el := FContext.Content.Items[FromElementIndex];

        FromWordIndex := 0;
        ToWordIndex := 0;

        if (el.Alignment <> FAlign) and (el is TTextElement) then
          break;

        if (el is TBulletEnd) then
        begin
          el.XY := Point(x + SP.X,y + SP.Y);
          el.LH := LineHeight;
          if (el = Caret.Element) then
          begin
            if Caret.CharIndex = 0 then
              CalcCaret(ACanvas,el, x, y +  SP.Y, LineHeight, Descent, Baseline, ' ', 0, DoDraw)
            else
              CalcCaret(ACanvas,el, OffsetX, y + LineHeight + SP.Y, LineHeight, Descent, BaseLine, ' ', 0, DoDraw)
          end;
          FIndent := 0;
          break;
        end;

        el.Selected := false;

        if Assigned(PaintSelection.FromElement) and Assigned(PaintSelection.ToElement) then
        begin
          if (FContext.Content.IndexOf(el) >= FContext.Content.IndexOf(PaintSelection.FromElement)) and
             (FContext.Content.IndexOf(el) <= FContext.Content.IndexOf(PaintSelection.ToElement)) then
          begin
            el.Selected := true;
          end;
        end;

        el.XY := Point(x + SP.X,y + SP.Y);

        el.LH := LineHeight;

        s := el.DisplayText;
      end
      else
        break;
    end;
  end;

  if (dcx <> -1) and Assigned(del) then
  begin
    if Caret.EOL and (dskip = 0) then
     Exit;

    CalcCaret(ACanvas,del, dcx, dcy + SP.Y, LineHeight, Descent, BaseLine, dcsu , dskip, DoDraw);
  end;
end;

function TAdvRichEditorBase.FirstElement: TREElement;
begin
  Result := nil;
  if FContext.Content.Count > 0 then
    Result := FContext.Content.Items[0];
end;

function TAdvRichEditorBase.LastElement: TREElement;
begin
  Result := nil;
  if FContext.Content.Count > 0 then
    Result := FContext.Content.Items[FContext.Content.Count - 1];
end;

function TAdvRichEditorBase.ElementIndex(AElement: TREElement): integer;
begin
  Result := FContext.Content.IndexOf(AElement);
end;

procedure TAdvRichEditorBase.EndUpdate;
begin
  {$IFDEF FMXLIB}
  inherited EndUpdate;
  {$ENDIF}

  if FUpdateCount > 0 then
  begin
    dec(FUpdateCount);

    if FUpdateCount = 0 then
      UpdateSize;
  end;
end;

procedure TAdvRichEditorBase.EnsureCaret;
begin
  if (FContext.Content.Count = 0) then
  begin
    Caret.Element := nil;
    Caret.CharIndex := 0;
    Selection.FromElement := nil;
    Selection.ToElement := nil;
  end;

  if not Assigned(Caret.Element) and (FContext.Content.Count > 0) then
  begin
    Caret.Element := FContext.Content.Items[0];
    Caret.CharIndex := 0;
    Caret.EOL := false;
    CaretToSelection;
  end;
end;

function TAdvRichEditorBase.ErrorCount: integer;
var
  i: integer;
begin
  Result := 0;

  for i := 0 to FContext.Content.Count - 1 do
  begin
    if (FContext.Content.Items[i] is TTextElement) then
      if (FContext.Content.Items[i] as TTextElement).Error then
        inc(Result);
  end;
end;

function TAdvRichEditorBase.IsURL(const Value: string): boolean;
begin
  Result := ((Pos('://',Value) > 0) or (Pos('mailto:',Value) > 0))
end;

procedure TAdvRichEditorBase.HandleURLs;
var
  i,j,d: integer;
  tel,ntel: TTextElement;
  s,su,buf: string;

  function VarPos(substr,str: string; var vp: integer): integer;
  begin
    vp := Pos(substr, str);
    Result := vp;
  end;

begin
  if URLAuto <> uAuto then
    Exit;

  i := 0;
  while i < FContext.Content.Count - 1 do
  begin
    if FContext.Content[i] is TTextElement then
    begin
      tel := FContext.Content[i] as TTextElement;
      s := tel.Text;
      buf := '';

      repeat
        su := GetNextWord(s);

        if IsURL(su) then
        begin
          if buf <> '' then
          begin
            ntel := TTextElement.Create;
            ntel.Text := buf;
            ntel.Color := Color;
            ntel.TextColor := GetDefaultFontColor;
            FContext.Content.Insert(i, ntel);
            inc(i);
            buf := '';
          end;

          ntel := TTextElement.Create;

          j := Length(Trim(su));
          d := Length(su) - j;
          if d > 0 then
          begin
            su := Copy(su, 1, j);
            for j := 1 to d do
              buf := buf + ' ';
          end;

          ntel.Text := su;
          ntel.URL := su;
          ntel.Color := Color;
          ntel.TextColor := GetDefaultFontColor;
          FContext.Content.Insert(i, ntel);
          inc(i);
        end
        else
          buf := buf + su;

      until s = '';

      tel.Text := buf;
    end;

    inc(i);
  end;
end;

function TAdvRichEditorBase.FindAndReplace(FromElement: TREElement; FromPos: integer; AText, AReplacement: string; MatchCase, DoReplace: boolean): boolean;
var
  el,pel,nel,first: TREElement;
  i, idx, sl, firstpos: integer;
  s: string;

begin
  Result := false;
  firstpos := -1;

  if AText = '' then
    Exit;

  FFindText := AText;
  FReplaceText := AReplacement;
  FFindMatchCase := MatchCase;

  s := '';
  first := nil;

  idx := FContext.Content.IndexOf(FromElement);

  for I := idx to FContext.Content.Count - 1 do
  begin
    el := FContext.Content.Items[i];

    if not (el is TTextElement) then
    begin
      s := '';
      first := nil;
      FromPos := 0;
    end
    else
    begin
      if s = '' then
        first := el;

      s := s + el.Text;
    end;

    firstpos := PosFrom(AText,s, FromPos, MatchCase);

    if (firstpos > 0) then
      break;
  end;

  if (firstpos > 0) and Assigned(first) then
  begin
    Result := true;
    el := first;
    s := el.Text;
    sl := 0;
    while (firstpos > Length(s)) do
    begin
      sl := Length(s);
      el := NextElement(el);
      s := s + el.Text;
    end;

    Caret.Element := el;
    Caret.CharIndex := firstpos - sl - 1;

    Selection.FromElement := Caret.Element;
    Selection.FromChar := Caret.CharIndex;

    while (firstpos + Length(FFindText) - 1 > Length(s)) do
    begin
      sl := Length(s);
      nel := NextElement(el);
      if Assigned(nel) then
      begin
        s := s + nel.Text;
        el := nel;
      end
      else
        break;
    end;

    Selection.ToElement := el;
    if el <> first then
    begin
      Selection.ToChar := firstpos + Length(FFindText) - sl - 1;
      first := el;
      firstpos := Selection.FromChar + 1;
    end
    else
      Selection.ToChar := firstpos + Length(FFindText) - 1;


    if DoReplace then
    begin
      pel := PreviousElement(first);

      if (Selection.FromElement = Selection.ToElement) and (Selection.FromElement is TTextElement) and
         (Selection.FromChar = 0) and (Selection.ToChar = Selection.ToElement.TextLength) then
      begin
        Selection.FromElement.Text := AReplacement;
        Caret.Element := Selection.ToElement;
        Caret.CharIndex := Selection.ToElement.TextLength;
        Selection.FromChar := Selection.ToElement.TextLength;
      end
      else
      begin
        DeleteSelection;

        s := first.Text;
        Insert(AReplacement, s, firstpos);

        if (s <> '') then
        begin
          first.Text := s;

          Caret.Element := first;
          Caret.CharIndex := firstpos + Length(AReplacement) - 1;
          Selection.FromElement := first;
          Selection.FromChar := firstpos{ - 1};
          Selection.ToElement := first;
          Selection.ToChar := firstpos + Length(AReplacement) - 1;
        end
        else
        begin
          Caret.Element := pel;
          Caret.CharIndex := LastIndex(Caret.Element);
          EnsureCaret;
          CaretToSelection;
        end;
      end;
    end;

    Refresh;
  end;
end;

function TAdvRichEditorBase.CheckFirstWord: string;
begin
  FWord.FromElement := nil;
  FWord.FromChar := 0;
  FWord.ToElement := nil;
  FWord.ToChar := 0;
  Result := CheckNextWord;
end;

function TAdvRichEditorBase.CheckNextWord: string;
var
  i,idx,charidx,ocharidx,len: integer;
  s: string;
  re: TREElement;
  sep: boolean;
begin
  Result := '';

  if Assigned(FWord.ToElement) then
  begin
    idx := ElementIndex(FWord.ToElement);
    charidx := FWord.ToChar;
  end
  else
  begin
    idx := 0;
    charidx := 0;
  end;

  FWord.FromElement := nil;

  for i := idx to FContext.Content.Count - 1 do
  begin
    re := FContext.Content.Items[i];

    if (re is TTextElement) then
    begin
      s := re.Text;
      len := Length(s);

      if not Assigned(FWord.FromElement) then
      begin
        FWord.FromElement := re;

        while (charidx <= len) and IsSeparator(CharInStr(s,charidx + 1)) do
          inc(charidx);

        FWord.FromChar := charidx;
      end;

      ocharidx := charidx;
      Delete(s,1,charidx);
      charidx := 0;
      len := Length(s);
      repeat
        sep := IsSeparator(CharInStr(s,charidx + 1));
        inc(charidx);
      until sep or (charidx >= len);

      if sep then
      begin
        Result := Result + Copy(s,1,charidx - 1);
        FWord.ToElement := re;
        FWord.ToChar := charidx + ocharidx - 1;
        if Length(Result) > 0 then
          break;
      end
      else
      begin
        FWord.ToElement := re;
        FWord.ToChar := charidx + ocharidx;

        Result := Result + s;
      end;
    end;
    charidx := 0;
  end;
end;


function TAdvRichEditorBase.Find(AText: string; MatchCase: boolean = false): boolean;
var
  i: integer;
begin
  Result := false;
  if not IsEmpty and (AText <> '') then
  begin
    if Assigned(Caret.Element) then
    begin
      i := ElementIndex(Caret.Element);
      if (i = FContext.Content.Count - 1) and (Caret.CharIndex = Length(Caret.Element.Text)) then
      begin
        Caret.Element := FirstElement;
        Caret.CharIndex := 0;
      end;
    end;

    if not FLastResult then
    begin
      Caret.Element := FirstElement;
      Caret.CharIndex := 0;
    end;

    FLastResult := FindAndReplace(Caret.Element, Caret.CharIndex + 1, AText,'', MatchCase, false);
    Result := FLastResult;
  end;
end;


function TAdvRichEditorBase.FindFirst(AText: string; MatchCase: boolean = false): boolean;
begin
  Result := false;
  if not IsEmpty and (AText <> '') then
  begin
    SetCaret(cpBeginDoc);
    Result := FindAndReplace(FContext.Content.Items[0], 0, AText, '', MatchCase, False);
  end;
end;

function TAdvRichEditorBase.FindNext: boolean;
begin
  Result := false;
  if not IsEmpty then
    Result := FindAndReplace(Caret.Element, Caret.CharIndex + 1, FFindText,'', FFindMatchCase, false);
end;

function TAdvRichEditorBase.Replace(AText: string; AReplacement: string; MatchCase: Boolean = False): boolean;
var
  i: integer;
begin
  Result := false;
  FFindText := AText;
  FReplaceText := AReplacement;


  if not IsEmpty and (AText <> '') then
  begin
    if Assigned(Caret.Element) then
    begin
      i := ElementIndex(Caret.Element);
      if (i = FContext.Content.Count - 1) and (Caret.CharIndex = Length(Caret.Element.Text)) then
      begin
        Caret.Element := FirstElement;
        Caret.CharIndex := 0;
      end;
    end;

    if not FLastResult then
    begin
      Caret.Element := FirstElement;
      Caret.CharIndex := 0;
    end;

    FLastResult := FindAndReplace(Caret.Element, Caret.CharIndex + 1, FFindText, FReplaceText, FFindMatchCase, True);

    Result := FLastResult;
  end;
end;

function TAdvRichEditorBase.ReplaceAll(AText: string; AReplacement: string; MatchCase: Boolean = False): boolean;
var
  fnd: boolean;
begin
  Result := false;

  if not IsEmpty and (AText <> '') then
  begin
    SetCaret(cpBeginDoc);
    repeat
       fnd := FindAndReplace(Caret.Element, Caret.CharIndex, AText, AReplacement, FFindMatchCase, True);
       if fnd then
         Result := true;
    until not fnd;
  end;
end;

function TAdvRichEditorBase.ReplaceFirst(AText, AReplacement: string; MatchCase: boolean = false): boolean;
begin
  Result := false;

  if not IsEmpty and (AText <> '') then
  begin
    SetCaret(cpBeginDoc);
    Result := FindAndReplace(FContext.Content.Items[0], 0, AText, AReplacement, MatchCase,True);
  end;
end;

function TAdvRichEditorBase.ReplaceNext: boolean;
begin
  Result := false;
  if not IsEmpty then
    Result := FindAndReplace(Caret.Element, Caret.CharIndex + 1, FFindText, FReplaceText, FFindMatchCase, True);
end;

function TAdvRichEditorBase.GetPictureSize(el: TREElement): TSize;
var
  sz: TSize;
begin
  sz.cx := 0;
  sz.cy := 0;
  Result := sz;
end;

function TAdvRichEditorBase.GetPlainTextContent: string;
begin
  Result := GetContentAsPlainText(false);
end;

function TAdvRichEditorBase.GetSelectionBkColor: TColor;
var
  clr: TColor;
begin
  Result := Color;
  if FDefaultChanged then
    Exit;

  if Assigned(Caret.Element) then
  begin
    if (Caret.Element is TTextElement) then
    begin
      clr := (Caret.Element as TTextElement).Color;
      if clr <> clNone then
        Result := (Caret.Element as TTextElement).Color;
    end;
  end;
end;

function TAdvRichEditorBase.GetSelectionBullet: TBulletType;
var
  pel: TREElement;
begin
  Result := btNone;
  pel := GetFirstBullet(Caret.Element);

  if Assigned(pel) then
    Result := (pel as TBulletElement).&Type;
end;

procedure TAdvRichEditorBase.GetSelectionElements(SelectionType: TSelectionType = stDefault);
var
  s,su: string;
  pel: TREElement;
  el: TTextElement;
  idx: integer;
  TempFont: TFont;
  TempColor: TColor;
  TempBkColor: TColor;
begin
  FSelectionElements.Clear;

  if not Assigned(Caret.Element) and (FContext.Content.Count > 0) then
    EnsureCaret;

  if (Selection.FromElement = nil) and (Selection.ToElement = nil) and Assigned(Caret.Element) then
    CaretToSelection;

  if (Selection.FromElement = Selection.ToElement) and (Selection.FromChar = Selection.ToChar) and (SelectionType = stWord) then
  begin
    SelectWordAtCaret;

    if (Selection.FromElement = Selection.ToElement) and (Selection.FromChar = Selection.ToChar) and Assigned(Selection.FromElement) and (Selection.FromElement is TTextElement) then
    begin
      Exit;
    end;
  end;

  if Assigned(Selection.FromElement) and (Selection.FromElement = Selection.ToElement) and (Selection.FromChar = Selection.ToChar) and (SelectionType = stLine) then
  begin
    FSelectionElements.Add(Selection.FromElement);
    Exit;
  end;

  TempFont := TFont.Create;

  SelectionToPaintSelection;

  Selection.FromElement := PaintSelection.FromElement;
  Selection.FromChar := PaintSelection.FromChar;
  Selection.ToElement := PaintSelection.ToElement;
  Selection.ToChar := PaintSelection.ToChar;

  if (PaintSelection.FromElement is TTextElement) then
  begin
    TempFont.Assign((PaintSelection.FromElement as TTextElement).Font);
    TempColor := (PaintSelection.FromElement as TTextElement).TextColor;
    TempBkColor := (PaintSelection.FromElement as TTextElement).Color;

    s := (PaintSelection.FromElement as TTextElement).Text;
    idx := FContext.Content.IndexOf(PaintSelection.FromElement);

    if (PaintSelection.FromChar >= 1) then
    begin
      if (PaintSelection.FromChar < Length(s)) then
      begin
        su := Copy(s, 1, PaintSelection.FromChar);
        (PaintSelection.FromElement as TTextElement).Text := su;
        el := TTextElement.Create;
        el.Assign(PaintSelection.FromElement);

        Selection.FromElement := el;
        Selection.FromChar := 0;

        if (PaintSelection.ToElement = PaintSelection.FromElement) then
          el.Text := Copy(s, PaintSelection.FromChar +1, PaintSelection.ToChar - PaintSelection.FromChar)
        else
          el.Text := Copy(s, PaintSelection.FromChar + 1, Length(s));

        InsertElement(idx + 1, el);
        Inc(idx);

        if (Caret.Element = PaintSelection.FromElement) then
        begin
          Caret.Element := el;
          Caret.CharIndex := 0;
          Selection.FromElement := el;
          Selection.FromChar := 0;
        end;

        FSelectionElements.Add(el);
      end;
    end
    else
    begin
      FSelectionElements.Add(PaintSelection.FromElement as TTextElement);

      if (PaintSelection.ToElement = PaintSelection.FromElement) then
        (PaintSelection.FromElement as TTextElement).Text := Copy(s, 1, PaintSelection.ToChar);
    end;

    if (PaintSelection.ToElement = PaintSelection.FromElement) then
    begin
      su := copy(s,PaintSelection.ToChar + 1, Length(s));
      if su <> '' then
      begin
        el := TTextElement.Create;
        el.Assign(PaintSelection.FromElement);
        el.Font.Assign(TempFont);
        el.TextColor := TempColor;
        el.Color := TempBkColor;
        el.Text := su;
        InsertElement(idx + 1, el);

        pel := FContext.Content[idx];
        Selection.ToElement := pel;
        Selection.ToChar := pel.TextLength;
      end
      else
      begin
        Selection.ToElement := Selection.FromElement;
        Selection.ToChar := Length(Selection.FromElement.Text);
      end;
    end;
  end;

  if Assigned(PaintSelection.FromElement) and not (PaintSelection.FromElement is TTextElement) and (SelectionType <> stSingle) then
    FSelectionElements.Add(PaintSelection.FromElement);

  if (PaintSelection.ToElement <> PaintSelection.FromElement) then
  begin
    idx := FContext.Content.IndexOf(PaintSelection.FromElement);
    inc(idx);

    // set attr of elements inbetween
    while (idx < FContext.Content.Count) and (FContext.Content.Items[idx] <> PaintSelection.ToElement) do
    begin
      FSelectionElements.Add(FContext.Content.Items[idx]);
      inc(idx);
    end;

    if (PaintSelection.ToElement is TTextElement) and (PaintSelection.ToChar > 0) then
    begin
      s := (PaintSelection.ToElement as TTextElement).Text;

      if (PaintSelection.ToChar < Length(s)) then
      begin
        el := TTextElement.Create;
        el.Assign(PaintSelection.ToElement);
        FSelectionElements.Add(el);

        el.Text := copy(s, 1, PaintSelection.ToChar);
        InsertElement(idx, el);

        (PaintSelection.ToElement as TTextElement).Text := Copy(s, PaintSelection.ToChar + 1, Length(s));
      end
      else
      begin
        el := PaintSelection.ToElement as TTextElement;
        FSelectionElements.Add(el);
      end;
    end;

    if Assigned(PaintSelection.ToElement) and not (PaintSelection.ToElement is TTextElement) then
      FSelectionElements.Add(PaintSelection.ToElement);
  end;

  TempFont.Free;
end;

function TAdvRichEditorBase.GetSelectionFontName: string;
begin
  Result := GetFontName(Font);

  if Assigned(Caret.Element) and not FDefaultChanged then
    Result := GetFontName(Caret.Element.Font);
end;

function TAdvRichEditorBase.GetSelectionFontSize: integer;
begin
  Result := GetFontSize(Font);

  if Assigned(Caret.Element) and not FDefaultChanged then
    Result := GetFontSize(Caret.Element.Font);
end;

function TAdvRichEditorBase.GetSelectionIndent: integer;
begin
  Result := 0;

  if Assigned(Caret.Element) then
    Result := Caret.Element.Indent;
end;

function TAdvRichEditorBase.GetSelectionTextColor: TColor;
var
  te: TREElement;
begin
  if FDefaultChanged then
    Result := GetDefaultFontColor
  else
  begin
    {$IFDEF FMXLIB}
    Result := clBlack;
    {$ENDIF}
    {$IFDEF VCLLIB}
    Result := Font.Color;
    {$ENDIF}

    te := NearbyTextElement(Caret.Element, Caret.CharIndex);

    if Assigned(te) and (te is TTextElement) then
    begin
      Result := (te as TTextElement).TextColor;
    end;
  end;
end;

function TAdvRichEditorBase.GetSelLength: integer;
begin
  Result := GetSelLength(Selection);
end;

function TAdvRichEditorBase.GetSelLength(ASelection: TSelection): integer;
var
  el: TREElement;
  SelStop, i: integer;

begin
  Result := 0;

  if not Assigned(Selection.ToElement) then
    Exit;

  SelStop := 0;

  for i := 0 to FContext.Content.Count - 1 do
  begin
    el := FContext.Content.Items[i];
    if el = ASelection.ToElement then
    begin
      SelStop := SelStop + ASelection.ToChar;
      break;
    end
    else
    begin
      SelStop := SelStop + el.TextLength;
      if el is TLineBreakElement then
        SelStop := SelStop + 2;

    end;
  end;

  Result := SelStop - GetSelStart(ASelection);
end;

function TAdvRichEditorBase.GetSelStart: integer;
begin
  Result := GetSelStart(Selection);
end;


function TAdvRichEditorBase.GetSelStart(ASelection: TSelection): integer;
var
  el: TREElement;
  i: integer;
begin
  Result := 0;

  if not Assigned(Selection.FromElement) then
    Exit;

  for i := 0 to FContext.Content.Count - 1 do
  begin
    el := FContext.Content.Items[i];
    if el = ASelection.FromElement then
    begin
      Result := Result + ASelection.FromChar;
      break;
    end
    else
    begin
      Result := Result + el.TextLength;
      if el is TLineBreakElement then
        Result := Result + 2;
    end;
  end;
end;

function TAdvRichEditorBase.GetSize(VertSizeOnly: boolean): TSize;
var
  y,i: integer;
  FElementIndex, WordIndex: integer;
  ToElementIndex, ToWordIndex: integer;
  LW, LH, Descent, Indent, BaseLine: integer;
  MLW: integer;
  FAlign: TAlignment;
  el: TREElement;
begin
  inherited;

  if not Assigned(FContext) then
  begin
    Result.cx := 0;
    Result.cy := 0;
    Exit;
  end;

  // reset precalculations
  for i := 0 to FContext.Content.Count - 1 do
  begin
    FContext.Content.Items[i].XY := Point(0,0);
    FContext.Content.Items[i].LH := 0;
    FContext.Content.Items[i].XYE := Point(0,0);
  end;

  y := OffsetY;
  MLW := 0;
  LH := 0;
  FTextWidth := 0;

  FElementIndex := 0;
  WordIndex := 0;
  Indent := 0;

  while (FElementIndex < FContext.Content.Count) do
  begin
    CalcLine(Canvas, Y, FElementIndex, WordIndex, GetLineWidth, LW, LH, ToElementIndex, ToWordIndex, Descent, Baseline, Indent, FAlign);

    y := y + LH;
    if LW > MLW then
      MLW := LW;

    FElementIndex := ToElementIndex;
    WordIndex := ToWordIndex;

    if VertSizeOnly and (y > Height) then
      break;
  end;

  el := LastElement;

  if Assigned(el) and (el is TLineBreakElement) then
    y := y + LH;

  if PageWidth > 0 then
    Result.cx := PageWidth
  else
    Result.cx := MLW + OffsetX;

  FTextWidth := MLW + OffsetX;

  Result.cy := y + 4;
end;

function TAdvRichEditorBase.GetText(el: TREElement): string;
begin
  Result := '';
  if (el is TTextElement) then
    Result := (el as TTextElement).Text;
end;

function TAdvRichEditorBase.GetTextDescent(ACanvas: TCanvas; el: TREElement): integer;
begin
  Result := 0;
end;

function TAdvRichEditorBase.GetTextSize(ACanvas: TCanvas; el: TREElement; AValue: string): TTextSize;
var
  sz: TTextSize;
begin
  sz.cx := 0;
  sz.cy := 0;
  sz.sup := 0;
  sz.sub := 0;
  Result := sz;
end;

function TAdvRichEditorBase.ElementsSelected: boolean;
begin
  Result := false;

  if (Selection.FromElement = Selection.ToElement) then
  begin
    if Assigned(Selection.FromElement) then
    begin
      if (Selection.FromElement is TCustomGraphicElement) then
        Result := Selection.FromElement.Selected;
    end;
  end;

  Result := Result or ((Selection.FromElement <> Selection.ToElement) or
    ((Selection.FromElement = Selection.ToElement) and (Selection.FromChar <> Selection.ToChar)));
end;

function TAdvRichEditorBase.HasSelection: boolean;
begin
  Result := ((Selection.FromElement <> Selection.ToElement) or
    ((Selection.FromElement = Selection.ToElement) and (Selection.FromChar <> Selection.ToChar)));
end;

function TAdvRichEditorBase.Highlight(AText: string;
  MatchCase: boolean): boolean;
begin
  //
  Result := FindFirst(AText, MatchCase);
  if Result then
  begin
    SetSelectionHighlight;

    while FindNext do
    begin
      SetSelectionHighlight;
    end;
  end;

  if Result then
    ClearSelection;
end;

procedure TAdvRichEditorBase.InsertTab;
var
  el: TREElement;
  tel: TTextElement;
  s: string;
begin
  if EditCanModify then
  begin

    if FDoUndo then
    begin
      PushContext;
      FDoUndo := false;
    end;

    FModified := true;

    if HasSelection then
      DeleteSelection;

    el := TTabElement.Create;

    if Assigned(Caret.Element) and (Caret.Element is TTextElement) then
    begin
      s := (Caret.Element as TTextElement).Text;
      if Caret.CharIndex = 0 then
      begin
        InsertBeforeElement(Caret.Element,el);
      end
      else
      if Caret.CharIndex = Length(s) then
      begin
        InsertAfterElement(Caret.Element,el);
      end
      else
      begin
        s := Copy(s, 1, Caret.CharIndex);

        tel := TTextElement.Create;
        tel.Assign(Caret.Element);
        tel.Text := s;
        InsertBeforeElement(Caret.Element, tel);
        InsertBeforeElement(Caret.Element, el);

        s := Caret.Element.Text;
        Delete(s, 1, Caret.CharIndex);
        Caret.Element.Text := s;
      end;
    end
    else
    begin
      if Assigned(Caret.Element) then
      begin
        if Caret.CharIndex = 0 then
          InsertBeforeElement(Caret.Element, el)
        else
          InsertAfterElement(Caret.Element, el);
      end
      else
        AddElement(el);
    end;

    Caret.CharIndex := 1;
    Caret.Element := el;
  end;
  Refresh;
end;

procedure TAdvRichEditorBase.InsertChar(ch: char);
var
  su,s,emoticonid: string;
  i,ci,nci: integer;
  SP: TPoint;
  al: TAlignment;
  ind: integer;
  re,pel: TREElement;
  nte: TTextElement;
  ge: TEmoticonPictureElement;
  editinerror: boolean;
  Error: boolean;
begin
  if EditCanModify then
  begin

    if FDoUndo then
    begin
      PushContext;
      FDoUndo := false;
    end;

    FModified := true;

    if HasSelection then
      DeleteSelection;

    su := ch;



    if Assigned(Caret.Element) and not FDefaultChanged then
    begin
      re := NearByTextElement(Caret.Element,0);
      pel := Caret.Element;
      al := Caret.Element.Alignment;
      ind := Caret.Element.Indent;

      if (Caret.Element is TTextElement) then
      begin
        editinerror := (Caret.Element as TTextElement).Error;

        // insert after merge field
        if (Caret.Element as TTextElement).MergeRef <> '' then
        begin
          if Caret.CharIndex = Length(Caret.Element.DisplayText) then
          begin
            nte := InsertText(su);
            nte.Font.Assign(Caret.Element.Font);
            Caret.Element := nte;
            Caret.CharIndex := 1;
            Exit;
          end;
        end;

        s := (Caret.Element as TTextElement).Text;

        // max word length = 1024
        if Length(s) < 1024 then
        begin
          Insert(su, s, Caret.CharIndex + 1);

          Caret.CharIndex := Caret.CharIndex + 1;
          (Caret.Element as TTextElement).Text := s;
          CaretToSelection;

          if IsSpace(ch) then
          begin
            ci := Caret.CharIndex - 1;
            EmoticonID := GetWordAtCaret(pel, ci, nci, True, False);

            if IsEmoticon(EmoticonID) then
            begin
              GetWordAtCaret(pel, ci, nci, True, True);
              Selection.ToChar := Selection.ToChar + 1;
              DeleteSelection;
              ge := InsertEmoticonPicture(EmoticonID, 0, 0);
              pel := PreviousElement(ge);
              if Assigned(pel) then
                ge.Font.Assign(pel.Font);
              InitializePictureSize(ge);
              if (Caret.Element is TEmoticonPictureElement) then
                Caret.CharIndex := 1;
              Refresh;
            end;
          end;

          if IsSeparator(su) then
          begin
            // finish URL
            DoCorrectWord;
            DoEnterWord;

            // Turn off hyperlink after space separator
            if (URLAuto = uAuto) and (Caret.Element.URL <> '') and (su = ' ') then
            begin
              Selection.FromElement := Caret.Element;
              Selection.FromChar := Caret.CharIndex - 1;
              Selection.ToElement := Caret.Element;
              Selection.ToChar := Caret.CharIndex;
              SetSelectionHyperLink('');
              Caret.Element := Selection.ToElement;
              Caret.CharIndex := Selection.ToElement.TextLength;
              CaretToSelection;
            end;
          end
          else
          begin
            if editinerror then
            begin
              Error := false;

              if Assigned(OnAutoCorrectWord) then
                OnAutoCorrectWord(Self, S, Error);

              if Assigned(OnCorrectWord) then
                OnCorrectWord(Self, S, Error);

              if not Error then
              begin
                (Caret.Element as TTextElement).Error := false;
              end;
            end;
          end;
        end;

        // Auto turn on hyperlink
        if (URLAuto = uAuto) and IsURL(s) and (Caret.Element.URL = '') then
        begin
          SelectWordAtCaret;
          SetSelectionHyperlink('#');
          Caret.Element := Selection.ToElement;
          Caret.CharIndex := Length(Selection.ToElement.Text);
          CaretToSelection;
        end;
      end
      else
      begin
        i := FContext.Content.IndexOf(Caret.Element);
        if (Caret.CharIndex = 0) then
        begin
          if (i > 0) and (FContext.Content.Items[i - 1] is TTextElement) then
          begin
            nte := (FContext.Content.Items[i - 1] as TTextElement);
            nte.Text := nte.Text + ch;
          end
          else
          begin
            nte := InsertText(i,ch);
            if Assigned(pel) then
            begin
              nte.Font.Assign(pel.Font);
              nte.Color := pel.Color;
              nte.TextColor := pel.TextColor;
            end;
            Caret.Element := nte;
            Caret.Element.Indent := ind;
            Caret.CharIndex := 1;
          end;
        end
        else
        begin
          nte := InsertText(i + 1,ch);
          if Assigned(re) then
          begin
            nte.Font.Assign(re.Font);
            nte.Color := re.Color;
            nte.TextColor := re.TextColor;
          end;
          Caret.Element := nte;
          Caret.Element.Alignment := al;
          Caret.Element.Indent := ind;
          Caret.CharIndex := 1;
        end;

        if IsSpace(su) then
        begin
          pel := Caret.Element;
          ci := Caret.CharIndex - 1;
          EmoticonID := GetWordAtCaret(pel, ci, nci, True, False);

          if IsEmoticon(EmoticonID) then
          begin
            GetWordAtCaret(pel, ci, nci, True, True);
            Selection.ToChar := Selection.ToChar + 1;
            DeleteSelection;
            ge := InsertEmoticonPicture(EmoticonID, 0, 0);
            pel := PreviousElement(ge);
            if Assigned(pel) then
              ge.Font.Assign(pel.Font);
            InitializePictureSize(ge);
            if (Caret.Element is TEmoticonPictureElement) then
              Caret.CharIndex := 1;
            Refresh;
          end;

        end;

        if IsSeparator(su) then
        begin
          DoCorrectWord;
          DoEnterWord;
        end;
      end;
    end
    else
    begin
      ind := 0;
      al := taLeftJustify;
      if Assigned(Caret.Element) then
      begin
        al := Caret.Element.Alignment;
        ind := Caret.Element.Indent;
      end;

      Caret.Element := AddText(ch);
      Caret.Element.Indent := ind;
      Caret.Element.Alignment := al;
      Caret.CharIndex := 1;
    end;

    FDefaultChanged := false;
    SP := TopLeft;

    if Caret.Element.XYE.X >= GetLineWidth then
    begin
      SetRange(GetSize(false));
    end;

    if Caret.XY.X > GetClientWidth then
    begin
      SP.X := Caret.XY.X - GetClientWidth;
      TopLeft := SP;
    end
    else
    if Caret.XY.X < SP.X then
    begin
      SP.X := Caret.XY.X;
      TopLeft := SP;
    end;

    if Caret.XY.Y - SP.Y > Height - Caret.LH - ScrollSizeHorz then
    begin
      UpdateSize;
      SP.Y := SP.Y + Caret.LH;
      TopLeft := SP;
    end;

    Refresh;
  end;
end;

function TAdvRichEditorBase.InsertGraphic(const ID: string; AWidth, AHeight: integer): TGraphicElement;
var
  ge: TGraphicElement;
begin
  ge := TGraphicElement.Create;
  ge.ID := ID;
  ge.Width := AWidth;
  ge.Height := AHeight;

  InsertElementAtCaret(ge);
  Result := ge;
  Refresh;
end;

function TAdvRichEditorBase.InsertImage(FileName: string; AWidth: integer = 0;
  AHeight: integer = 0): TPictureElement;
var
  lb: TPictureElement;
begin
  if HasSelection then
    DeleteSelection;

  lb := TPictureElement.Create;

  if Assigned(Caret.Element) then
  begin
    lb.Font.Assign(Caret.Element.Font);
    lb.TextColor := Caret.Element.TextColor;
    lb.Indent := Caret.Element.Indent;
  end
  else
  begin
    lb.Font.Assign(GetDefaultFont);
    lb.TextColor := GetDefaultFontColor;
  end;

  lb.Picture.LoadFromFile(FileName);
  lb.FitToPage(GetClientWidth, GetClientHeight);
  if (AWidth > 0) and (AHeight > 0) then
  begin
    lb.PictureWidth := AWidth;
    lb.PictureHeight := AHeight;
  end;
  InsertElementAtCaret(lb);
  Result := lb;
end;

function TAdvRichEditorBase.InsertNamedPicture(const AName: string; AWidth, AHeight: integer): TNamedPictureElement;
var
  ge: TNamedPictureElement;
begin
  ge := TNamedPictureElement.Create;
  ge.Name := AName;
  ge.Width := AWidth;
  ge.Height := AHeight;

  InsertElementAtCaret(ge);
  Result := ge;
end;

function TAdvRichEditorBase.InsertEmoticonPicture(const AName: string; AWidth, AHeight: integer): TEmoticonPictureElement;
var
  ge: TEmoticonPictureElement;
begin
  ge := TEmoticonPictureElement.Create;
  ge.Name := AName;
  ge.Width := AWidth;
  ge.Height := AHeight;

  InsertElementAtCaret(ge);
  Result := ge;
end;

function TAdvRichEditorBase.InsertImage(Picture: TPicture; AWidth: integer = 0; AHeight: integer = 0): TPictureElement;
var
  lb: TPictureElement;
begin
  if HasSelection then
    DeleteSelection;

  lb := TPictureElement.Create;

  if Assigned(Caret.Element) then
  begin
    lb.Font.Assign(Caret.Element.Font);
    lb.TextColor := Caret.Element.TextColor;
    lb.Indent := Caret.Element.Indent;
  end
  else
  begin
    lb.Font.Assign(GetDefaultFont);
    lb.TextColor := GetDefaultFontColor;
  end;

  lb.Picture.Assign(Picture);
  lb.FitToPage(GetClientWidth, GetClientHeight);


  if (AWidth > 0) and (AHeight > 0) then
  begin
    lb.PictureWidth := AWidth;
    lb.PictureHeight := AHeight;
  end;

  InsertElementAtCaret(lb);

  Result := lb;
end;


function TAdvRichEditorBase.InsertBullet(AType: TBulletType = btCircle): TBulletElement;
var
  bul: TBulletElement;
  i: integer;
  inds: TBulletStart;
  inde: TBulletEnd;

begin
  GetSelectionElements;

  if FSelectionElements.Count > 0 then
  begin
    i := FContext.Content.IndexOf(FSelectionElements.Items[0]);
    inds := TBulletStart.Create;
    inds.Indent := BULLETINDENT;
    FContext.Content.Insert(i,inds);
    inc(i);
    bul := TBulletElement.Create;
    bul.&Type := AType;
    bul.Font.Assign(GetDefaultFont);
    bul.BulletFont := GetFontName(Font);
    bul.Indent := BULLETINDENT;
    bul.Spacing := BulletSpacing;
    FContext.Content.Insert(i,bul);

    Caret.Element := bul;
    Caret.CharIndex := 1;

    // last element
    i := FContext.Content.IndexOf(FSelectionElements.Items[FSelectionElements.Count - 1]);
    inde := TBulletEnd.Create;
    FContext.Content.Insert(i + 1, inde);
  end
  else
  begin
    inds := TBulletStart.Create;
    inds.Indent := BULLETINDENT;
    FContext.Content.Add(inds);
    bul := TBulletElement.Create;
    bul.&Type := AType;
    bul.Font.Assign(GetDefaultFont);
    bul.BulletFont := GetFontName(Font);
    bul.Indent := BULLETINDENT;
    bul.Spacing := BulletSpacing;

    FContext.Content.Add(bul);

    Caret.Element := bul;
    Caret.CharIndex := 1;

    inde := TBulletEnd.Create;
    FContext.Content.Add(inde);
  end;

  Refresh;
  Result := bul;
end;

function TAdvRichEditorBase.InsertLB(Bullet: boolean; AType: TBulletType; AIndent: integer; IsShift, IsProg: boolean): TLineBreakElement;
var
  s,su: string;
  el,nel,pel: TREElement;
  e: TTextElement;
  lb: TLineBreakElement;
  bul: TBulletElement;
  idx, ln: integer;
  SP: TPoint;
  ci, nci: integer;
  ge: TEmoticonPictureElement;
begin
  Result := nil;

  if not IsProg then
  begin
    s := GetWordAtCaret(Caret.Element, Caret.CharIndex,ci, True, False);

    if (s <> '') then
    begin
      if IsEmoticon(s) then
      begin
        ci := Caret.CharIndex;
        GetWordAtCaret(Caret.Element, ci, nci, True, True);
        DeleteSelection;
        ge := InsertEmoticonPicture(s, 0, 0);
        pel := PreviousElement(ge);
        if Assigned(pel) then
          ge.Font.Assign(pel.Font);
        InitializePictureSize(ge);
        if (Caret.Element is TEmoticonPictureElement) then
          Caret.CharIndex := 1;
        Refresh;
      end
      else
      begin
        DoCorrectWord;
        DoEnterWord;
      end;
    end;
  end;

  if HasSelection then
    DeleteSelection;

  if Assigned(Caret.Element) then
  begin
    nel := NextElement(Caret.Element);
    pel := PreviousElement(Caret.Element);

    if (Caret.Element is TBulletStart) and Assigned(nel) and (Caret.CharIndex = 1) then
    begin
      if (nel is TBulletElement) then
        Caret.Element := NextElement(nel)
      else
        Caret.Element := nel;
      Caret.CharIndex := 0;
    end
    else

    if (Caret.Element is TBulletElement) and Assigned(nel) then
    begin
      if not (nel is TBulletEnd) then
      begin
        Caret.Element := nel;
        Caret.CharIndex := 0;
      end;
    end
    else
    if (Caret.Element is TBulletEnd) and Assigned(pel) then
    begin
      if (pel is TBulletElement) then
      begin
        if Assigned(nel) then
        begin
          Caret.Element := nel;
          Caret.CharIndex := 0;
        end
        else
        begin
          Caret.CharIndex := 1;
          Caret.Element.Indent := 0;

          lb := TLineBreakElement.Create;
          lb.Font.Assign(Caret.Element.Font);
          lb.Indent := 0;
          AddElement(lb);
          Result := lb;

          Exit;
        end;
      end;
    end
    else
    if (Caret.Element is TLineBreakElement) and Assigned(pel) and Assigned(nel) then
    begin
      if (pel is TBulletElement) and (nel is TBulletEnd) then
      begin
        DeleteElement(Caret.Element);
        nel.Indent := 0;
        Caret.Element := pel;
        Caret.CharIndex := 1;
      end;
    end;

    if (Caret.Element is TTextElement) then
    begin
      s := (Caret.Element as TTextElement).Text;
      idx := FContext.Content.IndexOf(Caret.Element);
      ln := Length(s);

      if (Caret.CharIndex < ln) and (Caret.CharIndex > 0) then
      begin
        su := Copy(s, 1, Caret.CharIndex);
        s := Copy(s, Caret.CharIndex + 1, Length(s));
        (Caret.Element as TTextElement).Text := su;
      end
      else
      begin
        su := s;
        s := '';
      end;

      if Caret.CharIndex <> 0 then
        inc(idx);

      lb := TLineBreakElement.Create;
      lb.Font.Assign(Caret.Element.Font);
      lb.Indent := Caret.Element.Indent;
      lb.BulletIndent := Caret.Element.BulletIndent;

      Result := lb;

      if IsShift then
      begin
        lb.Alignment := Caret.Element.Alignment;
        lb.Indent := Caret.Element.Indent;
        lb.BulletIndent := Caret.Element.BulletIndent;
      end;

      el := lb;

      InsertElement(idx,lb);
      inc(idx);

      if Bullet then
      begin
        bul := TBulletElement.Create;
        bul.&Type := AType;
        bul.Font.Assign(Caret.Element.Font);
        bul.TextColor := Caret.Element.TextColor;
        bul.Color := Caret.Element.Color;
        bul.BulletFont := GetFontName(Font);
        bul.Indent := AIndent;
        bul.Spacing := BulletSpacing;
        bul.BulletIndent := BulletIndent;

        InsertElement(idx,bul);
        inc(idx);
        el := bul;
      end;

      if s <> '' then
      begin
        e := TTextElement.Create;
        e.Assign(Caret.Element);
        e.Text := s;
        e.Indent := Caret.Element.Indent;
        e.BulletIndent := Caret.Element.BulletIndent;

        InsertElement(idx,e);
        Caret.Element := e;
        Caret.CharIndex := 0;
      end
      else
      begin
        Caret.Element := el;
        Caret.CharIndex := 1;
      end;
    end
    else
    if (Caret.Element is TCustomGraphicElement) or (Caret.Element is TTabElement) then
    begin
      idx := FContext.Content.IndexOf(Caret.Element);

      if Caret.CharIndex = 1 then
        inc(idx);

      lb := TLineBreakElement.Create;
      lb.Font.Assign(Caret.Element.Font);
      lb.Indent := Caret.Element.Indent;
      InsertElement(idx,lb);

      if IsShift then
        lb.Alignment := Caret.Element.Alignment;

      el := lb;

      Result := lb;

      if Bullet then
      begin
        inc(idx);
        bul := TBulletElement.Create;
        bul.&Type := AType;
        bul.Font.Assign(Caret.Element.Font);
        bul.TextColor := Caret.Element.TextColor;
        bul.Color := Caret.Element.Color;
        bul.BulletFont := GetFontName(Font);
        bul.Font.Assign(caret.Element.Font);
        bul.Spacing := BulletSpacing;
        bul.Indent := AIndent;

        InsertElement(idx,bul);
        el := bul;
      end;

      if (Caret.CharIndex = 1)  then
      begin
        Caret.Element := el;
        if Bullet then
          Caret.CharIndex := 1;
      end;
    end
    else
    if (Caret.Element is TLineBreakElement) then
    begin
      idx := FContext.Content.IndexOf(Caret.Element);
      if Caret.CharIndex = 1 then
        inc(idx);

      nel := NextElement(Caret.Element);
      pel := PreviousElement(Caret.Element);

      if Assigned(nel) and (nel is TBulletEnd) and (Caret.Element is TLineBreakElement) and (pel is TBulletElement) then
      begin
        DeleteElement(pel);
        DeleteElement(Caret.Element);
        Caret.Element := nel;
        Caret.CharIndex := 0;
      end
      else
      begin
        lb := TLineBreakElement.Create;
        lb.Font.Assign(Caret.Element.Font);
        if Caret.CharIndex = 0 then
          lb.Indent := Caret.Element.Indent;

        InsertElement(idx,lb);

        if IsShift then
          lb.Alignment := Caret.Element.Alignment;

        el := lb;

        Result := lb;

        if bullet then
        begin
          inc(idx);
          bul := TBulletElement.Create;
          bul.&Type := AType;
          bul.Font.Assign(Caret.Element.Font);
          bul.TextColor := Caret.Element.TextColor;
          bul.Color := Caret.Element.Color;
          bul.BulletFont := GetFontName(Font);
          bul.Indent := AIndent;
          bul.Spacing := BulletSpacing;
          InsertElement(idx,bul);
          Caret.Element := NextElement(bul);
          Caret.CharIndex := 0;
        end
        else
        begin
          Caret.Element := el;
          Caret.CharIndex := 1;
        end;
      end;
    end
    else
    if (Caret.Element is TBulletElement) then
    begin
      nel := NextElement(Caret.Element);
      if (nel is TBulletEnd) then
      begin
        idx := FContext.Content.IndexOf(Caret.Element);
        {$IFDEF DELPHI_LLVM}
        Caret.Element.DisposeOf;
        {$ELSE}
        Caret.Element.Free;
        {$ENDIF}
        FContext.Content.Delete(idx);

        nel.Indent := 0;
        Caret.Element := nel;
        Caret.CharIndex := 1;
      end;
    end
    else
    if (Caret.Element is TBulletEnd) then
    begin
      idx := FContext.Content.IndexOf(Caret.Element);

      if Caret.CharIndex = 1 then
      begin
        inc(idx);
        lb := TLineBreakElement.Create;
        lb.Indent := Caret.Element.Indent;
        lb.Font.Assign(Caret.Element.Font);
        InsertElement(idx,lb);
        Caret.Element := lb;
        Caret.CharIndex := 1;

        Result := lb;
      end
      else
      begin
        lb := TLineBreakElement.Create;
        lb.Indent := AIndent;
        lb.Font.Assign(Caret.Element.Font);
        InsertElement(idx,lb);
        bul := TBulletElement.Create;
        bul.Spacing := BulletSpacing;
        bul.Indent := AIndent;
        bul.&Type := AType;
        bul.BulletFont := GetFontName(Font);
        bul.Font.Assign(Caret.Element.Font);
        bul.TextColor := Caret.Element.TextColor;
        bul.Color := Caret.Element.Color;

        InsertElement(idx + 1,bul);
        Caret.Element := bul;
        Caret.CharIndex := 1;

        Result := lb;
      end;
    end
    else
    if (Caret.Element is TBulletStart) and (Caret.CharIndex = 0) then
    begin
      idx := FContext.Content.IndexOf(Caret.Element);
      lb := TLineBreakElement.Create;
      InsertElement(idx,lb);

      Result := lb;
    end;
  end
  else
  begin
    Result := AddLineBreak as TLineBreakElement;
    Caret.Element := Result;
    Caret.CharIndex := 1;
  end;

  SP := TopLeft;

  if Caret.XY.Y - SP.Y > Height - Caret.LH - 20 then
  begin
    UpdateSize;
    SP.Y := SP.Y + Caret.LH;
    TopLeft := SP;
  end;

  if Bullet then
    OrderBullets(Caret.Element);

  CaretToSelection;

  Refresh;
end;

function TAdvRichEditorBase.InsertLineBreak(IsShift: boolean; AIndent: integer = 0; Prog: boolean = false): TREElement;
begin
  if FDoUndo then
  begin
    PushContext;
    FDoUndo := false;
  end;

  Result := InsertLB(false, btCustom, AIndent, IsShift, Prog);

  if Assigned(Result) then
    ScrollHorz(Result.Indent);

  UpdateSize;
end;

procedure TAdvRichEditorBase.InsertLineBreakAndBullet(AType: TBulletType; AIndent: integer);
var
  lb: TLineBreakElement;
begin
  if FDoUndo then
  begin
    PushContext;
    FDoUndo := false;
  end;

  lb := InsertLB(true, AType, AIndent, false, false);
  if Assigned(lb) then
    ScrollHorz(lb.Indent);
  UpdateSize;
end;

function TAdvRichEditorBase.InsertElement(Index: integer; el: TREElement): TREElement;
begin
  Result := nil;
  if EditCanModify then
  begin
    if Index < FContext.Content.Count then
      FContext.Content.Insert(Index, el)
    else
      FContext.Content.Add(el);

    FLastElement := el;
    Result := el;
    FModified := true;
    UpdateSize;
  end;
end;

function TAdvRichEditorBase.InsertAfterElement(AElement, NewElement: TREElement): TREElement;
var
  idx: integer;
begin
  if Assigned(AElement) then
  begin
    idx := ElementIndex(AElement) + 1;
    NewElement.Font.Assign(AElement.Font);
  end
  else
  begin
    idx := FContext.Content.Count;
    NewElement.Font.Assign(GetDefaultFont);
  end;

  Result := InsertElement(idx , NewElement);
end;

function TAdvRichEditorBase.InsertBeforeElement(AElement, NewElement: TREElement): TREElement;
var
  idx: integer;
begin
  if Assigned(AElement) then
  begin
    idx := ElementIndex(AElement);
    NewElement.Font.Assign(AElement.Font);
  end
  else
  begin
    idx := 0;
    NewElement.Font.Assign(GetDefaultFont);
  end;

  Result := InsertElement(idx, NewElement);
end;

function TAdvRichEditorBase.InsertAfterElement(AElement: TREElement; NewElement: TREElementClass): TREElement;
begin
  Result := InsertAfterElement(AElement, NewElement.Create);
end;

function TAdvRichEditorBase.InsertBeforeElement(AElement: TREElement; NewElement: TREElementClass): TREElement;
begin
  Result := InsertBeforeElement(AElement, NewElement.Create);
end;

function TAdvRichEditorBase.InsertElement(Index: integer; NewElement: TREElementClass): TREElement;
begin
  Result := InsertElement(Index, NewElement.Create);
end;

procedure TAdvRichEditorBase.InsertElementAtCaret(el: TREElement);
var
  s,su: string;
  e: TTextElement;
  idx: integer;
begin
  if Assigned(Caret.Element) then
  begin
    if (Caret.Element is TTextElement) then
    begin
      if Caret.CharIndex = 0 then
      begin
        idx := FContext.Content.IndexOf(Caret.Element);
        InsertElement(idx, el);
        Caret.Element := el;
        Caret.CharIndex := Length(el.Text);
      end
      else
      begin
        s := (Caret.Element as TTextElement).Text;

        if Caret.CharIndex < Length(s) then
        begin
          su := Copy(s, 1, Caret.CharIndex);
          s := Copy(s, Caret.CharIndex + 1, Length(s));
        end
        else
        begin
          su := s;
          s := '';
        end;

        (Caret.Element as TTextElement).Text := su;

        idx := FContext.Content.IndexOf(Caret.Element);
        inc(idx);

        InsertElement(idx,el);
        Caret.Element := FLastElement;
        Caret.CharIndex := Length(el.Text);
        inc(idx);

        if (s <> '') then
        begin
          e := TTextElement.Create;
          e.Assign(Caret.Element);
          e.Text := s;
          InsertElement(idx,e);
          Caret.Element := e;
          Caret.CharIndex := 0;
        end;
      end;
    end
    else
    if (Caret.Element is TCustomGraphicElement) then
    begin
      idx := FContext.Content.IndexOf(Caret.Element);
      if Caret.CharIndex = 1 then
        inc(idx);

      InsertElement(idx,el);
      if Caret.CharIndex = 1 then
      begin
        Caret.Element := el;

        if (el is TCustomGraphicElement) then
          Caret.CharIndex := 1
        else
          Caret.CharIndex := 0;
      end;
    end
    else
    if (Caret.Element is TLineBreakElement) or (Caret.Element is TBulletElement) or (Caret.Element is TBulletEnd) or (Caret.Element is TBulletStart) or (Caret.Element is TTabElement) then
    begin
      idx := FContext.Content.IndexOf(Caret.Element);
      if Caret.CharIndex = 1 then
        inc(idx);

      InsertElement(idx, el);
      Caret.Element := el;
      Caret.CharIndex := Length(el.Text);
    end;
  end
  else
  begin
    AddElement(el);
    Caret.Element := el;

    if not (el is TTextElement) then
      Caret.CharIndex := 1
    else
      Caret.CharIndex := Length(el.Text);

    el.Font.Assign(GetDefaultFont);
  end;

  CaretToSelection;
  Refresh;
end;


function TAdvRichEditorBase.InsertText(Index: integer; AValue: string): TTextElement;
var
  el: TTextElement;
begin
  el := TTextElement.Create;
  el.Alignment := taLeftJustify;
  el.Text := AValue;
  el.Font.Assign(GetDefaultFont);
  el.TextColor := GetFontColor;
  InsertElement(Index,el);
  Result := el;
end;

function TAdvRichEditorBase.IsCaretInBulletList(var AType: TBulletType; var AIndex, AIndent: integer): boolean;
begin
  Result := not IsNewBullet(AType, AIndex, AIndent);
end;

function TAdvRichEditorBase.IsDelimiter(ch: char): boolean;
begin
  Result := (ch = ' ') or (ch = ';') or (ch = '.') or (ch = ';') or (ch = ',')  or (ch = ')') or (ch = '(') or (ch = '[') or (ch = ']') or (ch = '}') or (ch = '{');
end;

function TAdvRichEditorBase.IsEmpty: boolean;
begin
  Result := FContext.Content.Count = 0;
end;

function TAdvRichEditorBase.IsForwardSelection: boolean;
var
  idxf,idxt: integer;
begin
  Result := true;

  idxf := FContext.Content.IndexOf(Selection.FromElement);
  idxt := FContext.Content.IndexOf(Selection.ToElement);

  if idxf > idxt then
  begin
    Result := false;
  end
  else
    if idxf = idxt then
    begin
      if Selection.FromChar > Selection.ToChar then
      begin
        Result := false;
      end;
    end;
end;

procedure TAdvRichEditorBase.DeleteBullets(AType: TBulletType);
var
  i,bulind: integer;
  startelement,endelement: integer;
  el: TREElement;
begin
  i := FContext.Content.IndexOf(Caret.Element);

  startelement := -1;
  endelement := -1;
  bulind := 0;

  ShowTree;

  while i >= 0 do
  begin
    if (FContext.Content.Items[i] is TBulletStart) then
    begin
      startelement := i;

      el := FContext.Content.Items[i];
      bulind := el.Indent - el.BulletIndent;
      break;
    end;
    dec(i);
  end;

  i := FContext.Content.IndexOf(Caret.Element);

  while i < FContext.Content.Count do
  begin
    if (FContext.Content.Items[i] is TBulletEnd) then
    begin
      endelement := i;
      break;
    end;
    inc(i);
  end;

  if (startelement = -1) or (endelement = -1) then
    Exit;

  i := startelement;

  while i <= endelement do
  begin
    FContext.Content.Items[i].Indent := bulind;
    if (FContext.Content.Items[i] is TBulletEnd) or (FContext.Content.Items[i] is TBulletStart) or (FContext.Content.Items[i] is TBulletElement) then
    begin
      DeleteElement(FContext.Content.Items[i]);
      dec(endelement);
    end
    else
      inc(i);
  end;
end;

procedure TAdvRichEditorBase.ChangeBulletType(AType: TBulletType; AFontName: string);
var
  i: integer;
begin
  i := FContext.Content.IndexOf(Caret.Element);

  while i > 0 do
  begin
    if (FContext.Content.Items[i] is TBulletStart) then
      break;

    if ((FContext.Content.Items[i] is TTextElement) or
       (FContext.Content.Items[i] is TLineBreakElement) or
       (FContext.Content.Items[i] is TCustomGraphicElement)) and (i > 0) then
      if (FContext.Content.Items[i] is TBulletStart) then
      begin
        break;
      end;

    if (FContext.Content.Items[i] is TBulletElement) then
    begin
      (FContext.Content.Items[i] as TBulletElement).&Type := AType;
    end;

    dec(i);
  end;

  i := FContext.Content.IndexOf(Caret.Element);

  while i < FContext.Content.Count do
  begin
    if (FContext.Content.Items[i] is TBulletEnd) then
      break;

    if ((FContext.Content.Items[i] is TTextElement) or
       (FContext.Content.Items[i] is TLineBreakElement) or
       (FContext.Content.Items[i] is TCustomGraphicElement)) and (i > 0) then
      if (FContext.Content.Items[i] is TBulletEnd) then
      begin
        break;
      end;

    if (FContext.Content.Items[i] is TBulletElement) then
    begin
      (FContext.Content.Items[i] as TBulletElement).&Type := AType;
    end;

    inc(i);
  end;
end;

function TAdvRichEditorBase.CaretInBullet: boolean;
var
  AIndent, AIndex: integer;
  AType: TBulletType;
begin
  Result := not IsNewBullet(AType, AIndent, AIndex);
end;


function TAdvRichEditorBase.IsNewBullet(var AType: TBulletType; var AIndex, AIndent: integer): boolean;
var
  i: integer;
  el: TREElement;
begin
  Result := true;

  el := Caret.Element;

  if Assigned(Caret.Element) and (Caret.Element is TBulletEnd) and (Caret.CharIndex = 0) then
  begin
    el := PreviousBullet(el);
  end;


  i := FContext.Content.IndexOf(el);

  while i > 0 do
  begin
    if  (FContext.Content.Items[i] is TBulletEnd) then
      break;

    if ((FContext.Content.Items[i] is TTextElement) or
       (FContext.Content.Items[i] is TCustomGraphicElement)) and (i > 0) then
      if (FContext.Content.Items[i - 1] is TBulletEnd) then
      begin
        break;
      end;

    if (FContext.Content.Items[i] is TBulletElement) then
    begin
      AType := (FContext.Content.Items[i] as TBulletElement).&Type;
      AIndex := (FContext.Content.Items[i] as TBulletElement).&Index;
      AIndent := (FContext.Content.Items[i] as TBulletElement).Indent;
      Result := false;
      break;
    end;

    dec(i);
  end;
end;

function TAdvRichEditorBase.IsSelectionSubscript: boolean;
var
  el: TREElement;
begin
  el := Caret.Element;
  if Assigned(el) and not FDefaultChanged then
    Result := el.Baseline = tbSubscript
  else
    Result := FFontBaseLine  = tbSubscript;
end;

function TAdvRichEditorBase.IsSelectionSuperscript: boolean;
var
  el: TREElement;
begin
  el := Caret.Element;

  if Assigned(el) and not FDefaultChanged then
    Result := el.Baseline = tbSuperscript
  else
    Result := FFontBaseLine  = tbSuperscript;
end;

function TAdvRichEditorBase.IsSelectionBold: boolean;
var
  el: TREElement;
begin
  el := Caret.Element;

  if Assigned(el) and not FDefaultChanged then
  begin
    Result := TFontStyle.fsBold in el.Font.Style;
  end
  else
    Result := TFontStyle.fsBold in GetDefaultFont.Style;
end;

function TAdvRichEditorBase.IsSelectionStrikeOut: boolean;
var
  el: TREElement;
begin
  el := Caret.Element;

  if Assigned(el) and not FDefaultChanged then
    Result := TFontStyle.fsStrikeOut in el.Font.Style
  else
    Result := TFontStyle.fsStrikeOut in GetDefaultFont.Style;
end;


function TAdvRichEditorBase.IsSelectionCenter: boolean;
begin
  Result := FAlignment = taCenter;

  if Assigned(Caret.Element) then
  begin
    Result := Caret.Element.Alignment = taCenter;
  end;
end;

function TAdvRichEditorBase.IsSelectionItalic: boolean;
var
  el: TREElement;
begin
  el := Caret.Element;

  if Assigned(el) and not FDefaultChanged then
    Result := TFontStyle.fsItalic in el.Font.Style
  else
    Result := TFontStyle.fsItalic in GetDefaultFont.Style;
end;

function TAdvRichEditorBase.IsSelectionLeft: boolean;
begin
  Result := FAlignment = taLeftJustify;

  if Assigned(Caret.Element) then
  begin
    Result := Caret.Element.Alignment = taLeftJustify;
  end;
end;

function TAdvRichEditorBase.IsSelectionRight: boolean;
begin
  Result := FAlignment = taRightJustify;

  if Assigned(Caret.Element) then
  begin
    Result := Caret.Element.Alignment = taRightJustify;
  end;
end;

function TAdvRichEditorBase.IsSelectionUnderline: boolean;
var
  el: TREElement;
begin
  el := Caret.Element;

  if Assigned(el) and not FDefaultChanged then
    Result := TFontStyle.fsUnderline in el.Font.Style
  else
    Result := TFontStyle.fsUnderline in GetDefaultFont.Style;
end;

procedure TAdvRichEditorBase.InsertFromStream(const AStream: TStream; f: double);
var
  es: TElementSaver;
  esc: TElementClassSaver;
  el: TREElement;
  CRef: TPersistentClass;
  idx: integer;
  cte,te: TTextElement;
  bul: TBulletElement;
  skip: boolean;
  firstbul,firstlb: boolean;
begin
  RegisterClasses;

  es := TElementSaver.Create(nil);
  esc := TElementClassSaver.Create(nil);

  bul := nil;
  firstbul := false;
  firstlb := false;

  if Assigned(Caret.Element) then
  begin
    firstbul := Caret.Element is TBulletElement;

    idx := ElementIndex(Caret.Element);

    bul := InBullet(Caret.Element);

    if (Caret.Element is TTextElement) then
    begin
      cte := (Caret.Element as TTextElement);

      if (Caret.CharIndex > 0) and (Caret.CharIndex < Caret.Element.TextLength) then
      begin
        te := TTextElement.Create;
        te.Assign(cte);
        te.Text := Copy(cte.Text, 1, Caret.CharIndex);
        cte.Text := Copy(cte.Text,Caret.CharIndex + 1, cte.TextLength);
        InsertElement(idx,te);
        inc(idx);
      end
      else
        if (Caret.CharIndex = Caret.Element.TextLength) then
          inc(idx);
    end;

    if not (Caret.Element is TTextElement) then
    begin
      if (Caret.CharIndex = 1) and (idx <= FContext.Content.Count - 1) then
        inc(idx);
    end;

    firstlb := (Caret.Element is TBulletEnd);
  end
  else
    idx := FContext.Content.Count;

  try
    while AStream.Position < AStream.Size do
    begin
      try
        AStream.ReadComponent(esc);

        CRef := GetClass(esc.ElementClass);

        if esc.ElementClass = 'TTextElement' then
          el := TREElement(TTextElementClass(Cref).Create)
        else
          if esc.ElementClass = 'TPictureElement' then
            el := TREElement(TPictureElementClass(Cref).Create)
        else
          if esc.ElementClass = 'TNamedPictureElement' then
            el := TREElement(TNamedPictureElementClass(Cref).Create)
        else
          if esc.ElementClass = 'TEmoticonPictureElement' then
            el := TREElement(TEmoticonPictureElementClass(Cref).Create)
        else
          if esc.ElementClass = 'TGraphicElement' then
            el := TREElement(TGraphicElementClass(Cref).Create)
        else
          el := TREElement(TREElementClass(Cref).Create);

        es.SaveElement := el;
        es.SaveElement.WriteAccess := false;
        AStream.ReadComponent(es);

        FLastElement := el;

        // adapt font VCL (72dpi) vs FMX (96dpi)
        if f <> 1 then
        begin
          el.Font.Size := Round(f * el.Font.Size);
        end;

        skip := false;

        if Assigned(bul) then
        begin
          skip := (es.SaveElement is TBulletStart) or (es.SaveElement is TBulletEnd);

          if firstbul and (es.SaveElement is TBulletElement) then
          begin
            firstbul := false;
            skip := true;
          end;

          if firstlb and (Caret.Element is TBulletEnd) and (es.SaveElement is TBulletElement) then
          begin
            firstlb := false;
            FContext.Content.Insert(idx, TLineBreakElement.Create);
            inc(idx);
          end;

          if skip then
          begin
            FLastElement := nil;
            {$IFDEF DELPHI_LLVM}
            es.SaveElement.DisposeOf;
            {$ELSE}
            es.SaveElement.Free;
            {$ENDIF}
          end;
        end;

        if not skip then
        begin
          FContext.Content.Insert(idx, es.SaveElement);
          if AStream.Position < AStream.Size then
            inc(idx);
        end;
      except

      end;
    end;
  finally
    es.Free;
    esc.Free;
  end;

  Caret.Element := FContext.Content.Items[idx];

  if Assigned(Caret.Element) then
  begin
    Caret.CharIndex := Caret.Element.IndexAtEnd
  end
  else
    Caret.CharIndex := 0;

  if Assigned(bul) then
    OrderBullets(bul);

  EnsureCaret;

  UpdateSize;
  Refresh;

  CaretToSelection;
end;

procedure TAdvRichEditorBase.LoadFromStream(const AStream: TStream);
var
  es: TElementSaver;
  esc: TElementClassSaver;
  el: TREElement;
  CRef: TPersistentClass;
  mss: TStateSaver;
  ms: TMemoState;
  i: integer;
  f: double;
begin
  if AStream.Size = 0 then
    Exit;

  RegisterClasses;

  es := TElementSaver.Create(nil);
  esc := TElementClassSaver.Create(nil);

  mss := TStateSaver.Create(nil);
  ms := TMemoState.Create;

  try
    mss.SaveState := ms;
    AStream.ReadComponent(mss);

    while AStream.Position < AStream.Size do
    begin
      try
        AStream.ReadComponent(esc);

        CRef := GetClass(esc.ElementClass);

        if esc.ElementClass = 'TTextElement' then
          el := TREElement(TTextElementClass(Cref).Create)
        else
          if esc.ElementClass = 'TPictureElement' then
            el := TREElement(TPictureElementClass(Cref).Create)
        else
          if esc.ElementClass = 'TNamedPictureElement' then
            el := TREElement(TNamedPictureElementClass(Cref).Create)
        else
          if esc.ElementClass = 'TEmoticonPictureElement' then
            el := TREElement(TEmoticonPictureElementClass(Cref).Create)
        else
          if esc.ElementClass = 'TGraphicElement' then
            el := TREElement(TGraphicElementClass(Cref).Create)
        else
          el := TREElement(TREElementClass(Cref).Create);

        es.SaveElement := el;
        es.SaveElement.WriteAccess := false;
        AStream.ReadComponent(es);
        FContext.Content.Add(es.SaveElement);
      except

      end;
    end;

    UpdateSize;

    LoadMemoState(ms);

    f := 1;

    {$IFDEF VCLLIB}
    if (Producer = pFMX) then
      f := 72/96;
    {$ENDIF}

    {$IFDEF FMXLIB}
    if (Producer = pVCL) then
      f := 96/72;
    {$ENDIF}

    // adapt font VCL (72dpi) vs FMX (96dpi)
    if f <> 1 then
    begin
      for i := 0 to FContext.Content.Count - 1 do
        FContext.Content.Items[i].Font.Size := Round(f * FContext.Content.Items[i].Font.Size);
    end;

  finally
    mss.Free;
    ms.Free;
    es.Free;
    esc.Free;
  end;
end;


procedure TAdvRichEditorBase.LoadFromFile(const FileName: string);
begin
  Clear;
  ClearContext;
  AppendFile(FileName);
end;

procedure TAdvRichEditorBase.RegisterClasses;
begin
  if not FClassesRegistered then
  begin
    FClassesRegistered := true;
    Classes.RegisterClass(TREElement);
    Classes.RegisterClass(TTextElement);
    Classes.RegisterClass(TFont);
    Classes.RegisterClass(TLineBreakElement);
    Classes.RegisterClass(TPictureElement);
    Classes.RegisterClass(TNamedPictureElement);
    Classes.RegisterClass(TEmoticonPictureElement);
    Classes.RegisterClass(TGraphicElement);
    Classes.RegisterClass(TBulletElement);
    Classes.RegisterClass(TLineElement);
    Classes.RegisterClass(TElementSaver);
    Classes.RegisterClass(TElementClassSaver);
    Classes.RegisterClass(TBulletStart);
    Classes.RegisterClass(TBulletEnd);
    Classes.RegisterClass(TStateSaver);
  end;
end;

procedure TAdvRichEditorBase.SaveMemoState(AState: TMemoState);
begin
  AState.Version := 1;
  AState.Caret := ElementIndex(Caret.Element);
  AState.CaretIndex := Caret.CharIndex;
  AState.SelectionFrom := ElementIndex(Selection.FromElement);
  AState.SelectionFromIndex := Selection.FromChar;
  AState.SelectionTo := ElementIndex(Selection.ToElement);
  AState.SelectionToIndex := Selection.ToChar;
  AState.Top := TopLeft.Y;
  AState.Author := Author;
  AState.Comments := Comments;
  AState.Tags := Tags;
  AState.ColorC := Color;
  AState.DefaultFont.Assign(Font);
  {$IFDEF VCLLIB}
  AState.TextColorC := Font.Color;
  AState.Producer := pVCL;
  {$ENDIF}
  {$IFDEF FMXLIB}
  AState.Producer := pFMX;
  {$ENDIF}
  AState.Left := TopLeft.X;
end;

procedure TAdvRichEditorBase.SaveSelectionToStream(const AStream: TStream);
var
  es: TElementSaver;
  esc: TElementClassSaver;
  el: TREElement;
  i: integer;
  ms: TMemoState;
  mss: TStateSaver;

begin
  RegisterClasses;

  ms := TMemoState.Create;
  SaveMemoState(ms);

  mss := TStateSaver.Create(nil);
  mss.SaveState := ms;

  es := TElementSaver.Create(nil);
  esc := TElementClassSaver.Create(nil);

  AStream.WriteComponent(mss);

  try
    GetSelectionElements;

    for i := 0 to FSelectionElements.Count - 1 do
    begin
      el := FSelectionElements[i];
      esc.ElementClass := el.ClassName;
      AStream.WriteComponent(esc);

      el.WriteAccess := true;
      es.SaveElement := el;
      AStream.WriteComponent(es);
    end;
  finally
    ms.Free;
    mss.Free;
    es.Free;
    esc.Free;
  end;
end;


procedure TAdvRichEditorBase.SaveToStream(const AStream: TStream);
var
  i: integer;
  es: TElementSaver;
  esc: TElementClassSaver;
  mss: TStateSaver;
  ms: TMemoState;
begin
  RegisterClasses;

  TidyElements;

  ms := TMemoState.Create;
  SaveMemoState(ms);

  mss := TStateSaver.Create(nil);
  mss.SaveState := ms;

  try
    AStream.WriteComponent(mss);

    es := TElementSaver.Create(nil);
    esc := TElementClassSaver.Create(nil);

    try
      for i := 0 to FContext.Content.Count - 1 do
      begin
        esc.ElementClass := FContext.Content.Items[i].ClassName;
        AStream.WriteComponent(esc);

        es.SaveElement := FContext.Content.Items[i];
        es.SaveElement.WriteAccess := true;
        AStream.WriteComponent(es);
      end;
    finally
      es.Free;
      esc.Free;
    end;
  finally
    ms.Free;
    mss.Free;
  end;
end;

procedure TAdvRichEditorBase.SaveToFile(const FileName: string);
var
  AStream: TFileStream;
begin
  AStream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(AStream);
  finally
    AStream.Free;
  end;
end;

procedure TAdvRichEditorBase.SaveToText(AFileName: string);
var
  AStream: TStringStream;
  FStream: TFileStream;
  i: integer;
  el: TREElement;
begin
  AStream := TStringStream.Create('');
  try
    for i := 0 to FContext.Content.Count - 1 do
    begin
      el := FContext.Content.Items[i];

      if el is TLineBreakElement then
        AStream.WriteString(#13#10);

      if el is TTextElement then
        AStream.WriteString(el.Text);
    end;

    FStream := TFileStream.Create(AFileName, fmCreate);
    try
      AStream.Position := 0;
      FStream.CopyFrom(AStream, AStream.Size);
    finally
      FStream.Free;
    end;
  finally
    AStream.Free;
  end;
end;

function TAdvRichEditorBase.CalcCaretXY: integer;
var
  x,y: integer;
  FElementIndex, WordIndex, Indent: integer;
  sp: TPoint;
  ToElementIndex, ToWordIndex: integer;
  LW,LH, Descent, BaseLine: integer;
  FAlign: TAlignment;
  MaxLineWidth: integer;

begin
  WordIndex := 0;

  //tl := TopLeft;
  //TopLeft := Point(0,0);

  sp := TopLeft;

  y := OffsetY {+ sp.Y};
  Indent := 0;
  BaseLine := 0;

  // scan for topmost drawn element
  //while (ElementIndex < FContext.Content.Count) and (FContext.Content.Items[ElementIndex].XYE.Y < sp.Y) and (FContext.Content.Items[ElementIndex].XYE.Y <> 0) do
  //begin
  //  y := FContext.Content.Items[ElementIndex].XYE.Y;
  //  inc(ElementIndex);
  //end;

  FElementIndex := 0; // FContext.Content.IndexOf(Caret.Element);

  LH := 0;

  MaxLineWidth := GetLineWidth;

  while (FElementIndex < FContext.Content.Count) and (Caret.XY.X = -1) do
  begin
    CalcLine(Canvas, -1, FElementIndex, WordIndex, MaxLineWidth, LW, LH, ToElementIndex, ToWordIndex, Descent, BaseLine, Indent, FAlign);

    x := OffsetX + Indent;

    case FAlign of
      taRightJustify: x := OffsetX + Round(GetLineWidth - LW - 1);
      taCenter: x := OffsetX + Round(GetLineWidth - LW - 1) div 2;
    end;

    DrawLine(Canvas, x - sp.X,y - sp.Y, FElementIndex, WordIndex, MaxLineWidth, LW, LH, Descent, BaseLine, False);

    y := y + LH;

    FElementIndex := ToElementIndex;
    WordIndex := ToWordIndex;
  end;

  Result := LH;
end;

procedure TAdvRichEditorBase.ScrollToCaret;
var
  LH: integer;
begin
  // start from top
  TopLeft := Point(0,0);

  Caret.XY := Point(-1,-1);

  LH := CalcCaretXY;

  if (Caret.XY.Y <> -1) then
    TopLeft := Point(0,Caret.XY.Y - LH);
end;

procedure TAdvRichEditorBase.LoadFromTextFile(const FileName: string; Encoding: TEncoding);
var
  sl: TStringList;
begin
  Clear;
  ClearContext;
  sl := TStringList.Create;
  try
    {$IFNDEF LCLLIB}
    sl.LoadFromFile(FileName, Encoding);
    {$ENDIF}

    {$IFDEF LCLLIB}
    sl.LoadFromFile(FileName);
    {$ENDIF}

    BeginUpdate;
    AddMultiLineText(sl.Text);

    if URLAuto = uAuto then
      HandleURLS;

    EndUpdate;
    Clip := false;
    Repaint;
    Clip := true;
  finally
    sl.Free;
  end;
end;

procedure TAdvRichEditorBase.LoadFromTextFile(const FileName: string);
var
  sl: TStringList;
begin
  Clear;
  ClearContext;
  sl := TStringList.Create;
  try
    sl.LoadFromFile(FileName);
    BeginUpdate;
    AddMultiLineText(sl.Text);

    if URLAuto = uAuto then
      HandleURLS;

    EndUpdate;
    Clip := false;
    Repaint;
    Clip := true;
  finally
    sl.Free;
  end;
end;

procedure TAdvRichEditorBase.LoadMemoState(AState: TMemoState; VersionOnly: boolean = false);
var
  pt: TPoint;
  updscroll: boolean;
begin
  if AState.Version = 1 then
  begin
    Producer := AState.Producer;

    if not VersionOnly then
    begin
      if AState.Caret >= 0 then
        Caret.Element := Elements[AState.Caret]
      else
        Caret.Element := nil;

      Caret.CharIndex := AState.CaretIndex;

      if AState.SelectionFrom >= 0 then
        Selection.FromElement := Elements[AState.SelectionFrom]
      else
        Selection.FromElement := nil;

      Selection.FromChar := AState.SelectionFromIndex;

      if AState.SelectionTo >= 0 then
        Selection.ToElement := Elements[AState.SelectionTo]
      else
        Selection.ToElement := nil;

      Selection.ToChar := AState.SelectionToIndex;

      pt := Point(AState.Left, AState.Top);

      Author := AState.Author;
      Comments := AState.Comments;
      LastModifiedBy := AState.LastModifiedBy;
      Tags := AState.Tags;

      Font.Assign(AState.DefaultFont);

      {$IFDEF VCLLIB}
      Font.Color := AState.TextColorC;
      Color := AState.ColorC;
      updscroll := HandleAllocated;
      {$ELSE}
      updscroll := true;
      {$ENDIF}

      if updscroll then
      begin
        ScrollHorz(pt.X);
        ScrollVert(pt.Y);
      end;
    end;
  end;
end;

function TAdvRichEditorBase.NextChar(SelectionUpdate: boolean = true): boolean;
var
  s: string;
  i: integer;
  nxtc: integer;
  lc: boolean;
  nel: TREElement;
begin
  Result := false;

  if not Assigned(Caret.Element) and (FContext.Content.Count > 0) then
  begin
    Result := true;
    Caret.Element := FContext.Content.Items[0];
    CaretToSelection;
  end;

  if Assigned(Caret.Element) then
  begin
    Caret.EOL := false;
    s := Caret.Element.Text;
    // when possible, just move to next char

    lc := false;

    if Caret.Element is TTextElement then
      lc := Caret.CharIndex = Length(s);

    if (Caret.CharIndex < Length(s) - 1) then
    begin
      Caret.CharIndex := Caret.CharIndex + 1;
      Result := true;
    end
    else
    begin
      i := FContext.Content.IndexOf(Caret.Element);
      inc(i);

      // search next text element?
      while (i < FContext.Content.Count - 1) and
        not (FContext.Content.Items[i] is TTextElement) and not (FContext.Content.Items[i] is TLineBreakElement) and not (FContext.Content.Items[i] is TTabElement) and not (FContext.Content.Items[i] is TBulletEnd) {and not (FContext.Content.Items[i] is TBulletElement)} and not (FContext.Content.Items[i] is TCustomGraphicElement) do
      begin
        inc(i);
      end;

      if (i <= FContext.Content.Count - 1) and
         ((FContext.Content.Items[i] is TTextElement) or (FContext.Content.Items[i] is TLineBreakElement) or (FContext.Content.Items[i] is TTabElement) or (FContext.Content.Items[i] is TBulletEnd) or (FContext.Content.Items[i] is TBulletElement) or (FContext.Content.Items[i] is TCustomGraphicElement)) then
      begin
        // force to next element for bulletstart & bulletend
        if (Caret.Element is TBulletStart) or (Caret.Element is TBulletEnd) then
          Caret.CharIndex := 1;

        if ((Caret.Element is TCustomGraphicELement) or (Caret.Element is TLineBreakElement) or (Caret.Element is TTabElement) or (Caret.Element is TBulletEnd) or (Caret.Element is TBulletStart) or (Caret.Element is TBulletElement)) and (Caret.CharIndex = 1) then
          nxtc := 1
        else
          nxtc := 0;

        if lc then
         nxtc := 1;

        Caret.Element := FContext.Content.Items[i];
        Caret.CharIndex := nxtc;

        if (Caret.Element is TLineBreakElement) and (nxtc = 1) then
        begin
          nel := NextElement(Caret.Element);
          if Assigned(nel) then
          begin
            Caret.Element := nel;
            Caret.CharIndex := 0;
          end;

        end;


        Result := true;
      end
      else
      begin
        Caret.CharIndex := Length(s);
        if (Caret.Element is TLineBreakElement) or (Caret.Element is TBulletElement) or (Caret.Element is TBulletStart) or (Caret.Element is TCustomGraphicElement) then
        begin
          Caret.CharIndex := 1;
          Result := true;
        end;
      end;

      nel := NextElement(Caret.Element);

      if (Caret.Element is TLineBreakElement) and (Caret.CharIndex = 1) then
      begin
        if Assigned(nel) and (nel is TBulletElement) then
        begin
          Caret.Element := nel;
          Result := true;
        end;
      end;

      if (Caret.Element is TTabElement) and (Caret.CharIndex = 1) then
      begin
        if Assigned(nel) and (nel is TBulletElement) then
        begin
          Caret.Element := nel;
          Result := true;
        end;
      end;

      if (Caret.Element is TBulletEnd) and (Caret.CharIndex = 1) then
      begin
        if Assigned(nel) then
        begin
          Caret.Element := nel;
          Caret.CharIndex := 0;
          Result := true;
        end;
      end;
    end;
    Refresh;
  end;

  if SelectionUpdate then
    CaretToSelection;
end;

procedure TAdvRichEditorBase.NextCharSel;
begin
  if not Assigned(Selection.FromElement) then
    CaretToSelection;

  NextChar(false);
  Selection.ToElement := Caret.Element;
  Selection.ToChar := Caret.CharIndex;
end;

function TAdvRichEditorBase.NextDocElement(AElement: TREElement): TREElement;
var
  i: integer;
  el: TREElement;
begin
  Result := nil;

  i := FContext.Content.IndexOf(AElement);

  while (i < FContext.Content.Count - 1) do
  begin
    el := FContext.Content.Items[i + 1];
    if (el is TTextElement) or (el is TCustomGraphicElement) then
    begin
      Result := el;
      break;
    end;
    inc(i);
  end;
end;

function TAdvRichEditorBase.NearbyTextElement(AElement: TREElement; CharIndex: integer): TREElement;
var
  te,el: TREElement;
  pel,nel: TREElement;

begin
  Result := AElement;

  if not Assigned(AElement) then
    Exit;

  pel := PreviousElement(AElement);
  nel := NextElement(AElement);

  if not Assigned(pel) then
    CharIndex := 1;

  if not Assigned(nel) then
    CharIndex := 0;

  te := AElement;

  if CharIndex = 0 then
  begin
    el := PreviousElement(te);
    if not Assigned(el) then
      CharIndex := 1;
  end
  else
  begin
    el := NextElement(te);
    if not Assigned(el) then
      CharIndex := 0;
  end;

  if not (te is TTextElement) then
  begin

    if CharIndex = 0 then
    begin
      while Assigned(te) and ((te is TLineBreakElement) or (te is TBulletElement) or (te is TBulletStart)) do
      begin
        te := PreviousElement(te);
      end;
    end;

    if CharIndex = 1 then
    begin
      while Assigned(te) and ((te is TLineBreakElement) or (te is TBulletElement) or (te is TBulletEnd)) do
      begin
        te := NextElement(te);
      end;
    end;
  end;

  Result := te;
end;

function TAdvRichEditorBase.NextBullet(AElement: TREElement): TBulletElement;
begin
  Result := nil;

  while Assigned(AElement) do
  begin
    if AElement is TBulletEnd then
      break;

    if AElement is TBulletElement then
    begin
      Result := AElement as TBulletElement;
      Break;
    end;
    AElement := NextElement(AElement);
  end;
end;

function TAdvRichEditorBase.PreviousBullet(AElement: TREElement): TBulletElement;
begin
  Result := nil;

  while Assigned(AElement) do
  begin

    if AElement is TBulletElement then
    begin
      Result := AElement as TBulletElement;
      Break;
    end;
    AElement := PreviousElement(AElement);

    if AElement is TBulletEnd then
      Break;
  end;
end;

function TAdvRichEditorBase.InBullet(AElement: TREElement): TBulletElement;
var
  bul: TBulletElement;
begin
  bul := PreviousBullet(AElement);
  if not Assigned(bul) then
  begin
    bul := NextBullet(AElement);
  end;

  Result := bul;
end;


procedure TAdvRichEditorBase.UnBullet(AElement: TREElement);
begin
  while Assigned(AElement) do
  begin
    if (AElement is TBulletEnd) or (AElement is TBulletElement) then
      break
    else
      AElement.Indent := 0;

    AElement := NextElement(AElement);
  end;
end;

function TAdvRichEditorBase.NextElement(AElement: TREElement): TREElement;
var
  i: integer;
begin
  Result := nil;
  i := FContext.Content.IndexOf(AElement);
  if i < FContext.Content.Count - 1 then
    Result := FContext.Content.Items[i + 1];
end;

procedure TAdvRichEditorBase.NextWordSel;
begin
  if not Assigned(Selection.FromElement) then
    CaretToSelection;

  NextWord(false);
  Selection.ToElement := Caret.Element;
  Selection.ToChar := Caret.CharIndex;
end;


function TAdvRichEditorBase.OffsetX: integer;
begin
  Result := PageMargin.Horizontal + 2;
end;

function TAdvRichEditorBase.OffsetY: integer;
begin
  Result := PageMargin.Vertical + 2;
end;

procedure TAdvRichEditorBase.FixBullets(AElement: TREElement);
var
  fromel,toel,nel: TREElement;
  idx: integer;
  lb: TLineBreakElement;
begin
  fromel := AElement;
  // scan till IndentStart
  repeat
    if not (fromel is TBulletStart) then
      fromel := PreviousElement(fromel);
  until (fromel = nil) or (fromel is TBulletStart);

  toel := AElement;

  // scan till IndentEnd
  repeat
    if not (toel is TBulletEnd) then
      toel := NextElement(toel);
  until (toel = nil) or (toel is TBulletEnd);

  while Assigned(fromel) and (fromel <> toel) do
  begin
    nel := fromel;
    fromel := NextElement(fromel);

    if (fromel is TBulletElement) and not ((nel is TLineBreakElement) or (nel is TBulletStart)) and (fromel <> toel) then
    begin
      lb := TLineBreakElement.Create;
      lb.Indent := fromel.Indent;
      idx := FContext.Content.IndexOf(fromel);
      InsertElement(idx,lb);
    end;
  end;
end;

procedure TAdvRichEditorBase.OrderBullets(AElement: TREElement);
var
  fromel,toel,bs,be: TREElement;
  i: integer;
  sz: TTextSize;
  BULIND: integer;
  ch: char;
  startindent: integer;
  buls: TSize;
begin
  if not Assigned(AElement) then
    Exit;

  fromel := AElement;

  // scan till TBulletStart
  repeat
    if not (fromel is TBulletStart) then
      fromel := PreviousElement(fromel);
  until (fromel = nil) or (fromel is TBulletStart);

  if not Assigned(fromel) then
    Exit;

  toel := AElement;

  // scan till TBulletEnd
  repeat
    if not (toel is TBulletEnd) then
      toel := NextElement(toel);
  until (toel = nil) or (toel is TBulletEnd);

  if not Assigned(toel) then
    Exit;

  i := 0;

  bs := fromel;
  be := toel;

  BULIND := BULLETINDENT;
  startindent := 0;

  while Assigned(fromel) and (fromel <> toel) do
  begin
    if fromel is TBulletStart then
    begin
      startindent := (fromel as TBulletStart).StartIndent;
    end;

    if fromel is TBulletElement then
    begin
      (fromel as TBulletElement).Index := i;

      if (fromel as TBulletElement).&Type = btNumber then
        sz := GetTextSize(Canvas, fromel,inttostr(i+1)+'.')
      else
      if (fromel as TBulletElement).&Type = btChar then
        sz := GetTextSize(Canvas, fromel,chr(ord('a') + i))
      else
      begin
        ch := GetBulletChar((fromel as TBulletElement).&Type);
        buls := GetBulletSize(fromel, ch);
        sz.cx := buls.cx;
        sz.cy := buls.cy;
      end;

      (fromel as TBulletElement).Width := sz.cx;

      if sz.cx + 20 > BULLETINDENT then
        BULIND := sz.cx + 20;

      inc(i);
    end;

    fromel.BulletIndent := BULIND;
    fromel.Indent := BULIND + startindent;
    fromel := NextElement(fromel);
  end;

  fromel := bs;
  toel := be;

  while Assigned(fromel) and (fromel <> toel) do
  begin
    fromel.BulletIndent := BULIND;
    fromel.Indent := BULIND + startindent;
    fromel := NextElement(fromel);
  end;
end;

procedure TAdvRichEditorBase.NextWord(SelectionUpdate: boolean = true);
var
  s: string;
  vp,i: integer;
  flg: boolean;
  el,nel: TREElement;

begin
  el := Caret.Element;

  while Assigned(el) and (el is TLineBreakElement) do
    el := NextElement(el);

  while Assigned(el) and (el is TBulletStart) do
    el := NextElement(el);

  while Assigned(el) and (el is TBulletEnd) do
    el := NextElement(el);

  while Assigned(el) and (el is TBulletElement) do
    el := NextElement(el);

  if (el is TCustomGraphicElement) then
  begin
    el := NextElement(el);
    Caret.Element := el;
    Caret.Charindex := 0;
    CaretToSelection;
    Refresh;
    Exit;
  end;

  if not Assigned(el) then
    Exit;

  if (el <> Caret.Element) then
  begin
    Caret.Element := el;
    Caret.CharIndex := 0;
    Caret.EOL := false;
  end;

  if Assigned(Caret.Element) then
  begin
    s := Caret.Element.Text;
    flg := false;

    if Caret.CharIndex < Length(s) then
    begin
      Delete(s, 1, Caret.CharIndex);
      vp := FirstSeparator(s);

      while IsSeparator(CharInStr(s,vp + 1)) do
        inc(vp);

      if (vp > 0) then
      begin
        Caret.CharIndex := Caret.CharIndex + vp;
        flg := true;
      end
      else
        Caret.CharIndex := Caret.CharIndex + Length(s);
    end;

    if not flg then
    begin
      i := FContext.Content.IndexOf(Caret.Element);
      inc(i);

      while (i < FContext.Content.Count) do
      begin
        s := FContext.Content.Items[i].Text;
        vp := FirstSeparator(s);

        if (FContext.Content.Items[i] is TLineBreakElement) then
        begin
          nel := NextDocElement(FContext.Content.Items[i]);
          if Assigned(nel) then
          begin
            Caret.Element := nel;
            Caret.CharIndex := 0;
            Caret.EOL := false;
          end
          else
          begin
            Caret.Element := FContext.Content.Items[i];
            Caret.CharIndex := 1;
            Caret.EOL := true;
          end;
          break;
        end;

        if (vp > 0) or (FContext.Content.Items[i] is TCustomGraphicElement) then
        begin
          Caret.Element := FContext.Content.Items[i];
          Caret.CharIndex := vp;
          break;
        end;
        inc(i);
      end;
    end;

    if SelectionUpdate then
      CaretToSelection;
  end;
end;

function TAdvRichEditorBase.GetBulletSize(el: TREElement;
  AValue: string): TSize;
var
  sz: TSize;
begin
  sz.cx := 0;
  sz.cy := 0;
  Result := sz;
end;

function TAdvRichEditorBase.GetCareTTextElement: TTextElement;
var
  el: TREElement;
begin
  if (Caret.Element is TTextElement) then
    Result := Caret.Element as TTextElement
  else
  begin
    el := Caret.Element;

    if Caret.CharIndex = 0 then
    begin
      while Assigned(el) and not (el is TTextElement) do
      begin
        el := PreviousElement(el);
      end;
    end
    else
    begin
      while Assigned(el) and not (el is TTextElement) do
      begin
        el := NextElement(el);
      end;
    end;
    Result := TTextElement(el);
  end;
end;


function TAdvRichEditorBase.GetElement(Index: integer): TREElement;
begin
  Result := nil;
  if Index < FContext.Content.Count then
    Result := FContext.Content.Items[Index];
end;

function TAdvRichEditorBase.GetElementCount: integer;
begin
  Result := FContext.Content.Count;
end;

function TAdvRichEditorBase.GetFirstBullet(AElement: TREElement): TREElement;
var
  pel,bul: TREElement;
begin
  Result := nil;

  bul := nil;

  pel := AElement;

  while Assigned(pel) do
  begin
    if (pel is TBulletElement) then
      bul := pel;

    if (pel is TBulletStart) then
    begin
      Result := pel;
      break;
    end;

    pel := PreviousElement(pel);

    if (pel is TBulletEnd) then
      break;
  end;

  if Assigned(Result) then
  begin
    if Assigned(bul) then
    begin
      Result := bul;
    end
    else
    begin
      pel := NextElement(pel);
      if pel is TBulletElement then
        Result := pel
      else
        Result := nil;
    end;
  end;
end;

function TAdvRichEditorBase.GetFocused: boolean;
begin
{$IFDEF FMXLIB}
  Result := IsFocused;
{$ENDIF}
{$IFNDEF FMXLIB}
  Result := Focused;
{$ENDIF}
end;


procedure TAdvRichEditorBase.SetFontColor(AColor: TColor);
begin
  {$IFDEF VCLLIB}
  Font.Color := AColor;
  {$ENDIF}
  {$IFDEF FMXLIB}
  FFontColor := AColor;
  {$ENDIF}
  Refresh;
end;

function TAdvRichEditorBase.GetFontColor: TColor;
begin
  {$IFDEF VCLLIB}
  Result := Font.Color;
  {$ENDIF}
  {$IFDEF FMXLIB}
  Result := FFontColor;
  {$ENDIF}
end;

class function TAdvRichEditorBase.GetFontName(AFont: TFont): string;
begin
{$IFDEF FMXLIB}
  Result := AFont.Family;
{$ENDIF}
{$IFNDEF FMXLIB}
  Result := AFont.Name;
{$ENDIF}
end;

class function TAdvRichEditorBase.GetFontSize(AFont: TFont): integer;
begin
{$IFDEF FMXLIB}
  Result := Round(AFont.Size);
{$ENDIF}
{$IFNDEF FMXLIB}
  Result := AFont.Size;
{$ENDIF}
end;

class procedure TAdvRichEditorBase.SetFontSize(AFont: TFont; AValue: integer);
begin
{$IFDEF FMXLIB}
  AFont.Size := AValue;
{$ENDIF}
{$IFNDEF FMXLIB}
  AFont.Size := AValue;
{$ENDIF}
end;

function TAdvRichEditorBase.GetDefaultFontName: string;
begin
  Result := GetFontName(GetDefaultFont);
end;

function TAdvRichEditorBase.GetDefaultFontSize: integer;
begin
  Result := GetFontSize(GetDefaultFont);
end;

function TAdvRichEditorBase.GetLastBullet(AElement: TREElement): TREElement;
var
  nel,bul: TREElement;
begin
  Result := nil;

  bul := nil;

  nel := AElement;

  while Assigned(nel) do
  begin
    if (nel is TBulletElement) then
      bul := nel;

    if (nel is TBulletEnd) then
    begin
      Result := nel;
      break;
    end;

    if (nel is TBulletStart) then
      break;

    nel := NextElement(nel);
  end;

  if Assigned(Result) then
  begin
    if Assigned(bul) then
    begin
      Result := bul;
    end
    else
    begin
      nel := Result;
      Result := nil;
      while Assigned(nel) do
      begin
        nel := PreviousElement(nel);
        if nel is TBulletElement then
        begin
          Result := nel;
          break;
        end;
        if (nel is TBulletStart) or (nel is TBulletEnd) then
          Break;
      end;
    end;
  end;
end;

function TAdvRichEditorBase.GetLineWidth: integer;
begin
  {$IFDEF VCLLIB}
  if not HandleAllocated then
  begin
    Result := Width;
    Exit;
  end;
  {$ENDIF}

  if PageWidth = 0 then
    Result := Round(Max(FTextWidth, GetClientWidth))
  else
    Result := PageWidth;

  Result := Result - OffsetX;
end;

function TAdvRichEditorBase.IsSpace(ch: Char): boolean;
begin
  Result := (ch = ' ');
end;

function TAdvRichEditorBase.IsSpace(ch: string): boolean;
begin
  Result := IsSpace(CharInStr(ch,1));
end;

function TAdvRichEditorBase.IsSeparator(ch: Char): boolean;
begin
  Result := (ch = ' ') or (ch = ';') or (ch = '.') or (ch = ',') or (ch = ':') or (ch = '!') or (ch = '?') or (ch = '"') or (ch='''') or (ch = '-');
end;

function TAdvRichEditorBase.IsSeparator(ch: string): boolean;
begin
  Result := IsSeparator(CharInStr(ch,1));
end;

function TAdvRichEditorBase.LastIndex(AElement: TREElement): integer;
begin
  if AElement is TTextElement then
    Result := AElement.TextLength
  else
    Result := 1;
end;

function TAdvRichEditorBase.LastSeparator(Value: string): integer;
var
  i: integer;
  ch: char;
begin
  Result := -1;

  for i := Length(Value) downto 1 do
  begin
    ch := CharInStr(Value, i);
    if IsSeparator(ch) then
    begin
      Result := i;
      break;
    end;
  end;
end;

function TAdvRichEditorBase.FirstSeparator(Value: string): integer;
var
  i: integer;
  ch: char;
begin
  Result := -1;

  for i := 1 to Length(Value) do
  begin
    ch := CharInStr(Value, i);
    if IsSeparator(ch) then
    begin
      Result := i;
      break;
    end;
  end;
end;

function TAdvRichEditorBase.GetNextWord(var Value: string): string;
var
  vp: integer;
begin
  vp := pos(' ', Value);
  if (vp > 0) then
  begin
    Result := Copy(Value, 1, vp);
    Delete(Value, 1, vp);
  end
  else
  begin
    Result := Value;
    Value := '';
  end;
end;

procedure TAdvRichEditorBase.Paint;
var
  x, y: integer;
  FElementIndex, WordIndex: integer;
  ToElementIndex, ToWordIndex: integer;
  LW, LH, Descent, BaseLine, Indent: integer;
  sz: TTextSize;
  FAlign: TAlignment;
  sp: TPoint;
  ACanvas: TCanvas;
  MaxLineWidth: integer;
begin
  if not FCaretUpdate then
  begin
    if not Assigned(FBuffer) then
      FBuffer := CreateBuffer;

    BeginPaintBuffer;

    ACanvas := FBuffer.Canvas;

    DrawBackground(ACanvas);

    FElementIndex := 0;
    WordIndex := 0;
    Indent := 0;
    BaseLine := 0;

    sp := TopLeft;

    SelectionToPaintSelection;

    y := OffsetY;

    Caret.XY := Point(-10, -1000);

    MaxLineWidth := GetLineWidth;

    // scan for topmost drawn element
    while (FElementIndex < FContext.Content.Count) and (FContext.Content.Items[FElementIndex].XYE.Y < sp.Y) and (FContext.Content.Items[FElementIndex].XYE.Y <> 0) do
    begin
      y := FContext.Content.Items[FElementIndex].XYE.Y;
      inc(FElementIndex);
    end;

    Caret.NextLine := false;

    while (FElementIndex < FContext.Content.Count) do
    begin
      CalcLine(ACanvas, -1, FElementIndex, WordIndex, MaxLineWidth, LW, LH, ToElementIndex, ToWordIndex, Descent, BaseLine, Indent, FAlign);

      x := OffsetX + FIndent;

      case FAlign of
        taRightJustify: x := OffsetX + Round(GetLineWidth - LW - 1);
        taCenter: x := OffsetX + Round(GetLineWidth - LW - 1) div 2;
      end;

      if Caret.NextLine then
      begin
        Caret.XY := Point(Caret.XY.X, y);
        Caret.LH := LH;
        Caret.NextLine := false;
      end;

      if ((y - sp.Y < Height) and (y - sp.Y + LH >= 0)) or not Clip then
        DrawLine(ACanvas, x - sp.X,y - sp.Y, FElementIndex, WordIndex, MaxLineWidth, LW, LH, Descent, BaseLine, True);

      y := y + LH;

      if (y - sp.Y > Height) and Clip then
        Break;

      FElementIndex := ToElementIndex;
      WordIndex := ToWordIndex;
    end;

    if (Caret.Element = nil) and (Caret.CharIndex = 0) then
    begin
      sz := GetTextSize(Canvas, nil,'gh');
      Descent := GetTextDescent(Canvas, nil);
      LH := sz.cy;
      CalcCaret(ACanvas,nil,OffsetX,OffsetY,LH,Descent,BaseLine,'',0);
    end;

    DrawMargin(ACanvas);
  end;

  if Caret.NextLine then
  begin
    Caret.LH := GetLineHeight(Caret.Element);
    Caret.NextLine := false;
  end;

  EndPaintBuffer;
  UpdateSelection;
end;

function TAdvRichEditorBase.PlainText: string;
var
  i: integer;
begin
  Result := '';

  for i := 0 to FContext.Content.Count - 1 do
  begin
    if FContext.Content.Items[i] is TLineBreakElement then
      Result := Result + #13#10
    else
      Result := Result + FContext.Content.Items[i].Text;
  end;
end;

procedure TAdvRichEditorBase.ClearCaret;
begin
  Caret.Element := nil;
  Caret.CharIndex := 0;
end;

procedure TAdvRichEditorBase.ClearContext;
begin
  while FUndoRedo.Count > 0 do
  begin
    {$IFDEF DELPHI_LLVM}
    FUndoRedo[0].DisposeOf;
    {$ELSE}
    FUndoRedo[0].Free;
    {$ENDIF}
    FUndoRedo.Delete(0);
  end;
end;

procedure TAdvRichEditorBase.PopContext;
var
  i: integer;
  el,nel: TREElement;
begin
  if FUndoRedo.Count > 0 then
  begin
    FContext.Clear;

    FContext.Caret.Element := nil;
    FContext.Selection.FromElement := nil;
    FContext.Selection.ToElement := nil;

    for i := 0 to FUndoRedo.Items[FUndoRedoIndex].Content.Count - 1 do
    begin
      el := FUndoRedo.Items[FUndoRedoIndex].Content.Items[i];
      nel := el.Clone;
      nel.Assign(el);

      FContext.Content.Add(nel);

      if el = FUndoRedo.Items[FUndoRedoIndex].Caret.Element then
      begin
        FContext.Caret.Element := nel;
        FContext.Caret.CharIndex := FUndoRedo.Items[FUndoRedoIndex].Caret.CharIndex;
        Caret.Element := nel;
        Caret.CharIndex := FUndoRedo.Items[FUndoRedoIndex].Caret.CharIndex;
      end;

      if el = FUndoRedo.Items[FUndoRedoIndex].Selection.FromElement then
      begin
        FContext.Selection.FromElement := nel;
        FContext.Selection.FromChar := FUndoRedo.Items[FUndoRedoIndex].Selection.FromChar;
        Selection.FromElement := nel;
        Selection.FromChar := FUndoRedo.Items[FUndoRedoIndex].Selection.FromChar;
      end;

      if el = FUndoRedo.Items[FUndoRedoIndex].Selection.ToElement then
      begin
        FContext.Selection.ToElement := nel;
        FContext.Selection.ToChar := FUndoRedo.Items[FUndoRedoIndex].Selection.ToChar;
        Selection.ToElement := nel;
        Selection.ToChar := FUndoRedo.Items[FUndoRedoIndex].Selection.ToChar;
      end;
    end;

    Caret.Element := FContext.Caret.Element;
    Caret.CharIndex := FContext.Caret.CharIndex;

    if not Assigned(Caret.Element) then
      EnsureCaret;

    Refresh;
  end
  else
  begin
    Clear;
    Caret.Element := nil;
    Caret.CharIndex := 0;
    EnsureCaret;
  end;
end;

procedure TAdvRichEditorBase.CloneContext(FromContext, ToContext: TContext);
var
  i: integer;
  el,nel: TREElement;

begin
  // clone all elements
  for i := 0 to FromContext.Content.Count - 1 do
  begin
    el := FromContext.Content.Items[i];
    nel := el.Clone;
    nel.Assign(el);

    if el = Caret.Element then
    begin
      ToContext.Caret.Element := nel;
      ToContext.Caret.CharIndex := Caret.CharIndex;
    end;

    if el = Selection.FromElement then
    begin
      ToContext.Selection.FromElement := nel;
      ToContext.Selection.FromChar := Selection.FromChar;
    end;

    if el = Selection.ToElement then
    begin
      ToContext.Selection.ToElement := nel;
      ToContext.Selection.ToChar := Selection.ToChar;
    end;

    ToContext.Content.Add(nel);
  end;
end;




procedure TAdvRichEditorBase.PushCaret;
begin
  FTempCaret.Assign(Caret);
end;

procedure TAdvRichEditorBase.PopCaret;
begin
  Caret.Assign(FTempCaret);
end;

procedure TAdvRichEditorBase.PushSelection;
begin
  FTempSelection.Assign(Selection);
end;

procedure TAdvRichEditorBase.PopSelection;
begin
  Selection.Assign(FTempSelection);
end;


procedure TAdvRichEditorBase.PushContext;
var
  NewContext: TContext;
begin
  while FUndoRedo.Count - 1 > FUndoRedoIndex do
  begin
    FUndoRedo.Items[FUndoRedo.Count - 1].Clear;

    {$IFDEF DELPHI_LLVM}
    FUndoRedo.Items[FUndoRedo.Count - 1].DisposeOf;
    {$ELSE}
    FUndoRedo.Items[FUndoRedo.Count - 1].Free;
    {$ENDIF}

    FUndoRedo.Delete(FUndoRedo.Count - 1);
  end;

  NewContext := TContext.Create(FContext.ClientRect);

  CloneContext(FContext, NewContext);

  FUndoRedo.Add(NewContext);
  FUndoRedoIndex := FUndoRedo.Count - 1;
end;


function TAdvRichEditorBase.PrevChar(SelectionUpdate: boolean = true): boolean;
var
  s: string;
  i: integer;
begin
  Result := false;

  if not Assigned(Caret.Element) and (FContext.Content.Count > 0) then
  begin
    Result := true;
    Caret.Element := FContext.Content.Items[0];
    CaretToSelection;
  end;

  if Assigned(Caret.Element) then
  begin
    Caret.EOL := false;
    s := Caret.Element.Text;
    if Caret.CharIndex > 0 then
    begin
      Result := true;
      Caret.CharIndex := Caret.CharIndex - 1;
      Refresh;
    end
    else
    begin
      i := FContext.Content.IndexOf(Caret.Element);
      if i > 0 then
      begin
        dec(i);
        Caret.Element := FContext.Content.Items[i];

        while (i > 0) and not (FContext.Content.Items[i] is TTextElement) and not (FContext.Content.Items[i] is TLineBreakElement) and not (FContext.Content.Items[i] is TTabElement) {and not (FContext.Content.Items[i] is TBulletEnd)} and not (FContext.Content.Items[i] is TCustomGraphicElement) do
        begin
          dec(i);
          Caret.Element := FContext.Content.Items[i];
          Result := true;
        end;

        if (Caret.Element is TTextElement) then
        begin
          Caret.CharIndex := Length(Caret.Element.Text) - 1;
          Result := true;
        end
        else
          if (Caret.Element is TLineBreakElement) or
             (Caret.Element is TTabElement) or
             (Caret.Element is TBulletElement) or
             (Caret.Element is TBulletEnd) or (Caret.Element is TBulletStart) or
             (Caret.Element is TCustomGraphicElement) then
          begin
            Caret.CharIndex := 0;
            Result := true;
          end;

        Refresh;
      end;
    end;
  end;

  if SelectionUpdate then
    CaretToSelection;
end;

procedure TAdvRichEditorBase.PrevCharSel;
begin
  if not Assigned(Selection.FromElement) then
    CaretToSelection;

  PrevChar(false);

  Selection.ToElement := Caret.Element;
  Selection.ToChar := Caret.CharIndex;
end;

function TAdvRichEditorBase.PreviousDocElement(AElement: TREElement): TREElement;
var
  i: integer;
  el: TREElement;
begin
  Result := nil;

  i := FContext.Content.IndexOf(AElement);

  while (i > 0) do
  begin
    el := FContext.Content.Items[i - 1];
    if (el is TTextElement) or (el is TCustomGraphicElement) then
    begin
      Result := el;
      break;
    end;
    dec(i);
  end;
end;

function TAdvRichEditorBase.PreviousElement(AElement: TREElement): TREElement;
var
  i: integer;
begin
  Result := nil;

  i := FContext.Content.IndexOf(AElement);

  if i > 0 then
    Result := FContext.Content.Items[i - 1];
end;

procedure TAdvRichEditorBase.PrevWordSel;
begin
  if not Assigned(Selection.FromElement) then
    CaretToSelection;

  PrevWord(false);
  Selection.ToElement := Caret.Element;
  Selection.ToChar := Caret.CharIndex;
end;

procedure TAdvRichEditorBase.PrevWord(SelectionUpdate: boolean = true);
var
  s: string;
  i,vp: integer;
  el: TREElement;
begin
  el := Caret.Element;
  if (el is TTextElement) and (Caret.CharIndex = 0) then
    el := PreviousElement(el);

  while Assigned(el) and not (el is TTextElement) do
    el := PreviousElement(el);

  if not Assigned(el) then
    Exit;

  if el <> Caret.Element then
  begin
    Caret.Element := el;
    Caret.CharIndex := el.TextLength;
    Caret.EOL := false;
  end;

  if Assigned(el) then
  begin
    if (el is TTextElement) then
    begin
      s := (el as TTextElement).Text;

      if Caret.CharIndex > 0 then
      begin
        i := Caret.CharIndex - 1;
        vp := 1;

        if IsSeparator(CharInStr(s,i)) then
        begin
          while IsSeparator(CharInStr(s,i)) do
          begin
            dec(i);
            inc(vp);
          end;
        end;


        while (i > 0) and not IsSeparator(CharInStr(s,i)) do
        begin
          dec(i);
          inc(vp);
        end;

        if i > 0 then
        begin
          Caret.CharIndex := Caret.CharIndex - vp;
        end
        else
        begin
          el := PreviousElement(Caret.Element);

          if (el is TTextElement) then
          begin
            while Assigned(el) and (el is TTextElement) do
            begin
              s := el.Text;
              vp := LastSeparator(s);

              if vp <> -1 then
              begin
                Caret.Element := el;
                Caret.CharIndex := vp;
                Break;
              end
              else
              begin
                el := PreviousElement(el);
                vp := 0;
              end;
            end;

            Caret.Element := el;
            Caret.CharIndex := vp;
          end
          else
          begin
            Caret.CharIndex := 0;
          end;
        end;
      end;
    end;
    if SelectionUpdate then
      CaretToSelection;
  end;
end;

procedure TAdvRichEditorBase.SelectAll;
begin
  if FContext.Content.Count > 0 then
  begin
    Selection.FromElement := FContext.Content.Items[0];
    Selection.FromChar := 0;
    Selection.ToElement := FContext.Content.Items[FContext.Content.Count - 1];
    Selection.ToChar := Length(Selection.ToElement.Text);
  end;
end;

function TAdvRichEditorBase.Text: string;
var
  sel: TSelection;
begin
  Result := '';
  sel := TSelection.Create;
  sel.Assign(Selection);

  try
    SelectAll;
    Result := SelectedText;
  finally
    Selection.Assign(sel);
    sel.Free;
  end;
end;

function TAdvRichEditorBase.SelectedText: string;
var
  el: TREElement;
  s: string;
  i: integer;
  done: boolean;
begin
  Result := '';

  SelectionToPaintSelection;

  el := PaintSelection.FromElement;

  if not Assigned(el) then
    Exit;

  done := false;

  repeat
    if (el is TLineBreakElement) then
    begin
      Result := Result + LINEFEED;
    end;

    if (el is TTextElement) then
    begin
      s := el.Text;
      if (el <> PaintSelection.ToElement) and (el = PaintSelection.FromElement) then
      begin
        Result := Result + Copy(S, PaintSelection.FromChar, Length(S));

        i := FContext.Content.IndexOf(el);
        inc(i);
        if i < FContext.Content.Count then
          el := FContext.Content.Items[i]
        else
          done := true;
      end
      else
      if (el = PaintSelection.ToElement) and (el = PaintSelection.FromElement) then
      begin
        if (PaintSelection.FromChar <> PaintSelection.ToChar) then
          Result := Result + Copy(S, PaintSelection.FromChar + 1, PaintSelection.ToChar - PaintSelection.FromChar {+ 1});
        done := true;
      end
      else
      if (el <> PaintSelection.ToElement) and (el <> PaintSelection.FromElement) then
      begin
        Result := Result + el.Text;
        i := FContext.Content.IndexOf(el);
        inc(i);
        if i < FContext.Content.Count   then
          el := FContext.Content.Items[i]
        else
          break;
      end
      else
      begin
        Result := Result + Copy(S, 1, PaintSelection.ToChar);
        done := true;
      end;
    end
    else
    begin
      if el = PaintSelection.ToElement then
        done := true
      else
      begin
        i := FContext.Content.IndexOf(el);
        inc(i);
        if i < FContext.Content.Count then
          el := FContext.Content.Items[i]
        else
          done := true;
      end;
    end;

  until done;
end;

function TAdvRichEditorBase.SelectIndexAtEnd(AElement: TREElement): integer;
begin
  if AElement is TTextElement then
    Result := Length(AElement.Text)
  else
    Result := 1;
end;

procedure TAdvRichEditorBase.SelectionChanged(Sender: TObject);
begin
  Refresh;
end;

procedure TAdvRichEditorBase.SelectionToPaintSelection;
var
  idxf,idxt: integer;
begin
  PaintSelection.Assign(Selection);

  idxf := FContext.Content.IndexOf(Selection.FromElement);
  idxt := FContext.Content.IndexOf(Selection.ToElement);

  if idxf > idxt then
  begin
    PaintSelection.FromElement := Selection.ToElement;
    PaintSelection.FromChar := Selection.ToChar;
    PaintSelection.ToElement := Selection.FromElement;
    PaintSelection.ToChar := Selection.FromChar;
  end
  else
    if idxf = idxt then
    begin
      if Selection.FromChar > Selection.ToChar then
      begin
        PaintSelection.FromChar := Selection.ToChar;
        PaintSelection.ToChar := Selection.FromChar;
      end;
    end;
end;

procedure TAdvRichEditorBase.SetCaret(CaretPos: TCaretPos);
begin
  if FContext.Content.Count > 0 then
  begin
    case CaretPos of
      cpBeginDoc:
        begin
          Caret.Element := FContext.Content.Items[0];
          Caret.CharIndex := 0;
        end;
      cpEndDoc:
        begin
          Caret.Element := LastAddedElement;
          if Caret.Element is TTextElement then
            Caret.CharIndex := LastAddedElement.TextLength
          else
            Caret.CharIndex := 1;
        end;
    end;
  end
  else
    EnsureCaret;

  CaretToSelection;
end;

procedure TAdvRichEditorBase.SelectElement(AElement: TREElement);
begin
  Selection.FromElement := AElement;
  Selection.FromChar := 0;
  Selection.ToElement := AElement;
  Selection.ToChar := AElement.TextLength;
end;

procedure TAdvRichEditorBase.SelectText(FromChar, ALength: integer);
var
  i,d: integer;
  s,su: string;
begin
  s := '';
  i := 0;
  su := '';

  while i < FContext.Content.Count do
  begin

    if FContext.Content.Items[i] is TLineBreakElement then
      su := #13#10
    else
      su := FContext.Content.Items[i].Text;

    d := Length(su) - FromChar;

    if d > 0 then
      break;

    FromChar := FromChar - Length(su);

    s := s + su;

    inc(i);
  end;

  if i < FContext.Content.Count then
  begin
    Selection.FromElement := FContext.Content.Items[i];
    Selection.FromChar := FromChar;
  end
  else
  begin
    Selection.FromElement := FContext.Content.Items[i - 1];
    Selection.FromChar := Selection.FromElement.TextLength;
    i := i - 1;
    FromChar := FromChar + Selection.FromChar;
  end;

  s := FContext.Content.Items[i].Text;
  s := Copy(s, FromChar, Length(s));
  su := s;
  d := FromChar;

  while i < FContext.Content.Count - 1 do
  begin
    if ALength < Length(su) then
      break;

    d := 0;
    ALength := ALength - Length(su);

    inc(i);

    if (FContext.Content.Items[i] is TLineBreakElement) then
      su := #13#10
    else
      su := FContext.Content.Items[i].Text;

    s := s + su;
  end;

  Selection.ToElement := FContext.Content.Items[i];
  Selection.ToChar := d + ALength;
  Caret.Element := Selection.FromElement;
  Caret.CharIndex := Selection.FromChar;
end;

procedure TAdvRichEditorBase.GetWordAndIndexAtCaret(var AValue: string; var AIndex: integer);
begin
  AValue := GetWordAtCaret(Caret.Element, Caret.CharIndex, AIndex, False, False);
end;

procedure TAdvRichEditorBase.UpdateWordAndIndexAtCaret(AValue: string; AIndex: integer);
var
  ci: integer;
  s: string;
  el: TREElement;
  tx: TTextElement;
  idx: integer;
begin
  GetWordAtCaret(Caret.Element, Caret.CharIndex, ci, False, True);

  if HasSelection then
    DeleteSelection;

  if not Assigned(Caret.Element) then
  begin
    if FContext.Content.Count = 0 then
    begin
      AddText(AValue);
      Caret.Element := FContext.Content.Items[0];
      Caret.CharIndex := AIndex;
    end
    else
    begin
      el := FContext.Content[0];
      el.Text := el.Text + AValue;
      Caret.Element := el;
      Caret.CharIndex := AIndex;
    end;
  end
  else
  begin
    if Caret.Element is TTextElement then
    begin
      s := Caret.Element.Text;
      ci := Caret.CharIndex;
      Insert(AValue, s, Caret.CharIndex + 1);
      Caret.Element.Text := s;
      Caret.CharIndex := ci + AIndex;
    end
    else
    begin
      idx := ElementIndex(Caret.Element);

      if Caret.CharIndex > 0 then
        inc(idx);

      tx := InsertText(idx, AValue);
      Caret.Element := tx;
      Caret.CharIndex := tx.TextLength;
    end;
  end;
end;

procedure TAdvRichEditorBase.SetWordAtCaret(AElement: TREElement; AIndex: integer; AValue: string);
begin
  Caret.Element := AElement;
  Caret.CharIndex := AIndex;

  UpdateWordAndIndexAtCaret(AValue, Length(AValue) + 1);
  Refresh;
end;

function TAdvRichEditorBase.GetWordAtCaret(AElement: TREElement; AIndex: integer; var CharIndex: integer; SpaceOnly, Select: boolean): string;
var
  el,nel,clickel: TREElement;
  clickidx, clickelidx, scanidx, idx, elidx: integer;
  s: string;
  found: boolean;
  elfrom,elto: TREElement;
  idxfrom,idxto: integer;

  function CheckEOW(ch: char): boolean;
  begin
    if SpaceOnly then
      Result := IsSpace(ch)
    else
      Result := IsDelimiter(ch);
  end;

begin
  if not Assigned(AElement) then
    Exit;

  el := AElement;
  Result := '';
  CharIndex := 0;

  elfrom := nil;
  elto := nil;
  idxfrom := 0;
  idxto := 0;

  // element is not a text element -> get next or previous
  if not (el is TTextElement) then
  begin
    if AIndex = 0 then
    begin
      el := PreviousElement(el);
      if Assigned(el) then
        AIndex := el.TextLength
      else
        AIndex := 0;
    end
    else
    begin
      el := NextElement(el);
      AIndex := 0;
    end;
  end;

  // is text element
  if (el is TTextElement) then
  begin
    clickel := el;
    s := el.Text;

    idx := AIndex;
    clickidx := idx;
    clickelidx := FContext.Content.IndexOf(el);
    scanidx := clickidx;

    if idx >= -1 then
    begin
      elidx := clickelidx;

      // scan till start
      found := false;

      repeat
        while (idx > 0) and not CheckEOW(CharInStr(s,idx)) do
        begin
          dec(idx);
          inc(CharIndex);
        end;

        Result := Copy(s, idx + 1, scanidx - idx) + Result;

        if (idx = 0) and (elidx > 0) and not CheckEOW(CharInStr(s,idx)) then
        begin
          dec(elidx);

          nel := FContext.Content.Items[elidx];
          s := nel.Text;
          if (nel is TTextElement) then
          begin
            if CheckEOW(CharInStr(s,Length(s))) then
              found := true
            else
            begin
              idx := Length(s);
              scanidx := idx;
              el := nel;
            end;
          end
          else
          begin
            found := true;
          end;
        end
        else
          found := true;

      until found;

      if Select then
      begin
        elfrom := el;
        idxfrom := idx;
      end;

      found := false;

      // scan till end
      idx := clickidx + 1;
      scanidx := clickidx + 1;
      //inc(idx);
      el := clickel;
      elidx := clickelidx;
      s := el.Text;

      repeat
        while (idx <= Length(s)) and not CheckEOW(CharInStr(s,idx)) do
          inc(idx);

        Result := Result + Copy(s, scanidx, idx - (clickidx + 1));

        if (idx = Length(s) + 1) and (elidx < FContext.Content.Count - 1) {and not IsDelimiter(CharInStr(s,idx))} then
        begin
          inc(elidx);

          nel := FContext.Content.Items[elidx];
          s := nel.Text;
          if (nel is TTextElement) then
          begin
            if CheckEOW(CharInStr(s,idx)) then
              found := true
            else
            begin
              el := nel;
              idx := 1;
            end;
          end
          else
            found := true;
        end
        else
          found := true;

      until found;

      if Select then
      begin
        elto := el;
        idxto := idx - 1;
      end;
    end;
  end;

  if Select {and (CharIndex <> Length(Result))} then
  begin
    Selection.ToElement := elto;
    Selection.ToChar := idxto;
    Selection.FromElement := elfrom;
    Selection.FromChar := idxfrom;
  end;
end;

procedure TAdvRichEditorBase.GotoLineBegin;
begin
  if Assigned(Caret.Element) then
  begin
    Caret.CharIndex := 0;
    CaretToSelection;
    Refresh;
  end;
end;

procedure TAdvRichEditorBase.GotoLineEnd;
begin
  if Assigned(Caret.Element) then
  begin
    Caret.CharIndex := Caret.Element.TextLength;
    CaretToSelection;
    Refresh;
  end;
end;

procedure TAdvRichEditorBase.GotoTextBegin;
begin
  if ElementCount > 0 then
  begin
    Caret.Element := GetElement(0);
    Caret.CharIndex := 0;
    CaretToSelection;
    ScrollToCaret;
    Refresh;
  end;
end;

procedure TAdvRichEditorBase.GotoTextEnd;
begin
  if ElementCount > 0 then
  begin
    Caret.Element := GetElement(ElementCount - 1);
    Caret.CharIndex := Caret.Element.TextLength;
    CaretToSelection;
    ScrollToCaret;
    Refresh;
  end;
end;

function TAdvRichEditorBase.SelectWordAtCaret: string;
var
  ci: integer;
begin
  Result := GetWordAtCaret(Caret.Element, Caret.CharIndex, ci, False, True);
end;

function TAdvRichEditorBase.SelectWordAtXY(x, y: integer): string;
var
  el: TREElement;
  idx,ci: integer;
  SP: TPoint;
  CX,CY: integer;
begin
  SP := TopLeft;

  Result := '';

  if XYToElement(X + SP.X,Y + SP.Y,el) then
  begin
    if (el is TTextElement) then
    begin
      idx := XYToChar(X + SP.X,Y + SP.Y,el, CX, CY);
      Result := GetWordAtCaret(el, idx, ci, False, True);
    end;
  end;
end;

class procedure TAdvRichEditorBase.SetFontName(AFont: TFont; AName: string);
begin
  {$IFDEF FMXLIB}
  AFont.Family := AName;
  {$ENDIF}
  {$IFDEF VCLLIB}
  AFont.Name := AName;
  {$ENDIF}
end;

procedure TAdvRichEditorBase.SetPageHeight(const Value: integer);
begin
  // PageHeight = 0 use continuous flow / else use fixed PageHeight
  FPageHeight := Value;
end;

procedure TAdvRichEditorBase.SetPageMargin(const Value: TPageMargin);
begin
  FPageMargin.Assign(Value);
end;

procedure TAdvRichEditorBase.SetPageWidth(const Value: integer);
begin
  // PageWidth = 0 -> use control width / else use fixed width
  FPageWidth := Value;
  UpdateSize;
end;

procedure TAdvRichEditorBase.SetPlainTextContent(const Value: string);
begin
  //
end;

procedure TAdvRichEditorBase.SetSelectionAttribute(AAlignment: TAlignment);
var
  idx: integer;
  el: TREElement;
begin
  if IsEmpty or (SelectionType = stSingle) then
    FAlignment := AAlignment;

  PushContext;

  if HasSelection then
  begin
    SelectionToPaintSelection;

    if Assigned(PaintSelection.FromElement) then
    begin
      PaintSelection.FromElement.Alignment := AAlignment;

      if PaintSelection.FromElement <> PaintSelection.ToElement then
      begin
        idx := FContext.Content.IndexOf(PaintSelection.FromElement);

        repeat
          inc(idx);
          if idx < FContext.Content.Count then
            FContext.Content.Items[idx].Alignment := AAlignment;
        until (idx >= FContext.Content.Count - 1) or (FContext.Content.Items[idx] = PaintSelection.ToElement);
      end;

    end;
  end
  else
  begin
    EnsureCaret;

    if Assigned(Caret.Element) then
    begin
      el := Caret.Element;

      if (el is TLineBreakElement) then
        el.Alignment := AAlignment;

      while Assigned(el) and ((el is TTextElement) or (el is TCustomGraphicElement)) do
      begin
        el.Alignment := AAlignment;
        el := PreviousElement(el);
      end;

      el := Caret.Element;

      while Assigned(el) and ((el is TTextElement) or (el is TCustomGraphicElement)) do
      begin
        el.Alignment := AAlignment;
        el := NextElement(el);
      end;
    end;
  end;

  Refresh;
end;

procedure TAdvRichEditorBase.SetSelectionAttribute(AFont: TFont;
  AColor: TColor);
begin
  SetSelectionAttribute(AFont,AColor,clNone);
end;

procedure TAdvRichEditorBase.SetSelectionAttribute(AFontName: string;
  AFontSize: integer; AFontStyle: TFontStyles; AColor, BkColor: TColor);
var
  fnt: TFont;
begin
  fnt := TFont.Create;

  {$IFDEF VCLLIB}
  fnt.Name := AFontName;
  fnt.Color := AColor;
  {$ENDIF}

  {$IFDEF FMXLIB}
  fnt.Family := AFontName;
  {$ENDIF}

  fnt.Size := AFontSize;
  fnt.Style := AFontStyle;

  try
    SetSelectionAttribute(fnt,AColor,BkColor);
  finally
    fnt.Free;
  end;
end;

procedure TAdvRichEditorBase.SetSelectionAttribute(AFontName: string;
  AFontSize: integer; AFontStyle: TFontStyles; AColor: TColor);
begin
  SetSelectionAttribute(AFontName, AFontSize, AFontStyle, AColor, clNone);
end;

procedure TAdvRichEditorBase.SetSelectionAttribute(AFont: TFont;
  AColor: TColor; BkColor: TColor);
var
  s,su: string;
  el: TTextElement;
  idx: integer;
  TempFont: TFont;
  TempColor: TColor;
  TempBkColor: TColor;
begin
  PushContext;
  //
  TempFont := TFont.Create;

  SelectionToPaintSelection;

  if (PaintSelection.FromElement is TTextElement) then
  begin
    TempFont.Assign((PaintSelection.FromElement as TTextElement).Font);
    TempColor := (PaintSelection.FromElement as TTextElement).TextColor;
    TempBkColor := (PaintSelection.FromElement as TTextElement).Color;

    s := (PaintSelection.FromElement as TTextElement).Text;
    idx := FContext.Content.IndexOf(PaintSelection.FromElement);
    if (PaintSelection.FromChar > 1) then
    begin
      if (PaintSelection.FromChar < Length(s)) then
      begin
        su := Copy(s, 1, PaintSelection.FromChar);
        (PaintSelection.FromElement as TTextElement).Text := su;
        el := TTextElement.Create;
        el.Assign(PaintSelection.FromElement);

        if (PaintSelection.ToElement = PaintSelection.FromElement) then
          el.Text := Copy(s, PaintSelection.FromChar +1, PaintSelection.ToChar - PaintSelection.FromChar)
        else
          el.Text := Copy(s, PaintSelection.FromChar + 1, Length(s));

        InsertElement(idx + 1, el);
        Inc(idx);

        if AColor <> clNone then
          el.TextColor := AColor;

        if bkColor <> clNone then
          el.Color := BkColor;

        el.Font.Assign(AFont);
      end;
    end
    else
    begin
      (PaintSelection.FromElement as TTextElement).Font.Assign(AFont);
      (PaintSelection.FromElement as TTextElement).TextColor := AColor;
      (PaintSelection.FromElement as TTextElement).Color := BKColor;
      if (PaintSelection.ToElement = PaintSelection.FromElement) then
        (PaintSelection.FromElement as TTextElement).Text := Copy(s, 1, PaintSelection.ToChar);
    end;

    if (PaintSelection.ToElement = PaintSelection.FromElement) then
    begin
      su := copy(s,PaintSelection.ToChar + 1, Length(s));
      if su <> '' then
      begin
        el := TTextElement.Create;
        el.Assign(PaintSelection.FromElement);
        el.Font.Assign(TempFont);
        el.TextColor := TempColor;
        el.Color := TempBkColor;
        el.Text := su;
        InsertElement(idx + 1, el);
      end;
    end;
  end;

  if (PaintSelection.ToElement <> PaintSelection.FromElement) then
  begin
    idx := FContext.Content.IndexOf(PaintSelection.FromElement);
    inc(idx);

    // set attr of elements inbetween
    while (FContext.Content.Items[idx] <> PaintSelection.ToElement) do
    begin
      if (FContext.Content.Items[idx] is TTextElement) then
      begin
        (FContext.Content.Items[idx] as TTextElement).Font.Assign(AFont);
        (FContext.Content.Items[idx] as TTextElement).TextColor := AColor;
        (FContext.Content.Items[idx] as TTextElement).Color := BkColor;
      end;

      inc(idx);
    end;

    if (PaintSelection.ToElement is TTextElement) then
    begin
      s := (PaintSelection.ToElement as TTextElement).Text;

      if (PaintSelection.ToChar < Length(s)) then
      begin
        el := TTextElement.Create;
        el.Assign(PaintSelection.ToElement);
        el.Font.Assign(AFont);
        el.TextColor := AColor;
        el.Color := BkColor;
        el.Text := copy(s, 1, PaintSelection.ToChar);
        InsertElement(idx, el);

        (PaintSelection.ToElement as TTextElement).Text := Copy(s, PaintSelection.ToChar + 1, Length(s));
      end
      else
      begin
        el := PaintSelection.ToElement as TTextElement;
        el.Font.Assign(AFont);
        el.TextColor := AColor;
        el.Color := BkColor;
      end;
    end;
  end;

  TempFont.Free;
  Refresh;
end;

function TAdvRichEditorBase.SkipWords(Value: string; Count: integer; var CharsSkipped: integer): string;
var
  vp,i: integer;
begin
  Result := Value;
  i := 0;
  CharsSkipped := 0;

  vp := pos(' ', Value);
  CharsSkipped := vp;

  while (vp > 0) and (i < Count) do
  begin
    Delete(Result,1, vp);
    vp := pos(' ', Result);
    inc(i);
    if i < Count then
      CharsSkipped := CharsSkipped + vp;
  end;
end;

procedure TAdvRichEditorBase.Undo;
begin
  if (FUndoRedoIndex >= 0) and (FUndoRedo.Count > 0) then
  begin
    // last state was not saved
    if not (FUndoRedo.Items[FUndoRedoIndex].Equal(FContext)) then
    begin
      PushContext;
      dec(FUndoRedoIndex);
    end;

    PopContext;

    if FUndoRedoIndex > 0 then
      dec(FUndoRedoIndex);
  end;
end;

procedure TAdvRichEditorBase.UnHighlight;
var
  i: integer;
begin
  for i := 0 to FContext.Content.Count - 1 do
    FContext.Content.Items[i].Highlight := false;

  TidyElements;
  Refresh;
end;

procedure TAdvRichEditorBase.Redo;
begin
  if FUndoRedoIndex < FUndoRedo.Count - 1 then
  begin
    inc(FUndoRedoIndex);
    PopContext;
  end;
end;

procedure TAdvRichEditorBase.UnSelect;
begin
  Selection.ToElement := nil;
  Selection.FromElement := nil;
  Selection.ToChar := -1;
  Selection.FromChar := -1;
end;

procedure TAdvRichEditorBase.UpdateSize;
var
  sz: TSize;
  cond: boolean;
begin
  if FUpdateCount > 0 then
    Exit;

  if (csDestroying in ComponentState) then
    Exit;

  {$IFDEF VCLLIB}
  if not HandleAllocated then
    Exit;
  {$ENDIF}

  FUpdateCount := 1;

  sz := GetSize(False);

  cond := sz.cx > GetClientWidth;
  sz.cx := 0;

  // recalculate size in case SetRange affects scrollbar
  if SetRange(sz) or cond then
  begin
    SetRange(GetSize(False));
  end;
  FUpdateCount := 0;
end;

function TAdvRichEditorBase.WordAtCaret: string;
var
  ci: integer;
begin
  Result := GetWordAtCaret(Caret.Element, Caret.CharIndex, ci, False, False);
end;

function TAdvRichEditorBase.WordAtXY(X, Y: integer): string;
var
  el: TREElement;
  idx,ci: integer;
  SP: TPoint;
  CX, CY: integer;
begin
  SP := TopLeft;
  Result := '';
  if XYToElement(X + SP.X,Y + SP.Y,el) then
  begin
    if (el is TTextElement) and (el.TextLength > 0) then
    begin
      idx := XYToChar(X + SP.X,Y + SP.Y,el, CX, CY);
      Result := GetWordAtCaret(el, idx, ci, False, False);
    end;
  end;
end;

procedure TAdvRichEditorBase.XYToCaret(X,Y: single; TestSelection: boolean; var IsSelected: boolean);
var
  xi,yi: integer;
begin
  xi := Round(X);
  yi := Round(Y);
  XYToCaret(xi, yi, TestSelection, IsSelected);
end;

procedure TAdvRichEditorBase.XYToCaret(X, Y: integer; TestSelection: boolean; var IsSelected: boolean);
var
  el,nel,pel: TREElement;
  idx: integer;
  s: string;
  SP: TPoint;
  CX, CY: integer;
  OldCaret: TREElement;
  OldCaretIndex: integer;
  OldSelectionFrom: TREElement;
  OldSelectionFromIndex: integer;
  OldSelectionTo: TREElement;
  OldSelectionToIndex: integer;

begin
  SP := TopLeft;
  IsSelected := false;
  OldCaret := Caret.Element;
  OldCaretIndex := Caret.CharIndex;

  OldSelectionFrom := Selection.FromElement;
  OldSelectionFromIndex := Selection.FromChar;
  OldSelectionTo := Selection.ToElement;
  OldSelectionToIndex := Selection.ToChar;


  if X < 0 then
    X := 2;

  if Y < 0 then
    Y := 2;

  if XYToElement(X + SP.X, Y + SP.Y,el) then
  begin
    if (el is TTextElement) then
    begin
      if (y < el.XYE.Y) or ((el.XYE.Y = 0) and (el.XYE.X = 0)) then
      begin
        s := (el as TTextElement).DisplayText;

        idx := XYToChar(X + SP.X,Y + SP.Y,el, CX, CY);

        // text before indented text
        if (idx = -1) and (x <= el.XY.X) then
          idx := 0;

        if (idx = -1) and ((y > el.XY.Y + el.LH) or (x > el.XYE.X)) then
          idx := el.TextLength;

        if (idx > -1) then
        begin
          if TestSelection then
          begin
            IsSelected := el.Selected
              and
                ((el.SelFrom = -1) or (idx >= el.SelFrom))
              and
                ((el.SelTo = -1) or (idx <= el.SelTo))
              and
                (el.SelFrom <> el.SelTo);
          end
          else
          begin
            Caret.Element := el;
            Caret.EOL := false;
            Caret.CharIndex := idx;
            CaretToSelection;
            Refresh;
          end;
        end;
      end
      else
      begin
        if TestSelection then
        begin

        end
        else
        begin
          Caret.Element := el;
          Caret.CharIndex := el.TextLength;
          CaretToSelection;
          Refresh;
        end;
      end;
    end;

    if (el is TCustomGraphicElement) then
    begin
      if TestSelection then
      begin
        IsSelected := el.Selected;

      end
      else
      begin
        Caret.Element := el;

        if X > el.XY.X + el.Size.cx div 2 then
          Caret.CharIndex := 1
        else
          Caret.CharIndex := 0;

        CaretToSelection;
        Refresh;
      end;
    end;

    if (el is TTabElement) then
    begin
      Caret.Element := el;

      if (X + SP.X < el.XY.X + el.Size.cx div 2) then
        Caret.CharIndex := 0
      else
        Caret.CharIndex := 1;
    end;

    if (el is TLineBreakElement) then
    begin
      if TestSelection then
      begin

      end
      else
      begin
        Caret.Element := el;
        Caret.CharIndex := 0;
      end;

      if (Y + SP.Y > el.XY.Y + el.LH) then
      begin
        nel := NextElement(el);

        while Assigned(nel) and ((nel is TBulletStart) or (nel is TBulletElement)) do
          nel := NextElement(nel);

        if Assigned(nel) and not (nel is TLineBreakElement) then
        begin
          if TestSelection then
          begin

          end
          else
          begin
            Caret.Element := nel;
            Caret.CharIndex := 0;
          end;
        end
        else
          Caret.CharIndex := 1;
      end;

      if TestSelection then
      begin

      end
      else
      begin
        CaretToSelection;
        Refresh;
      end;
    end;

    if (el is TBulletElement) then
    begin
      if TestSelection then
      begin

      end
      else
      begin
        Caret.Element := el;
        Caret.CharIndex := 1;
        Caret.EOL := false;
        CaretToSelection;
      end;
    end;

    if (el is TBulletEnd) then
    begin
      pel := PreviousElement(el);
      if pel is TTextElement then
      begin
        if TestSelection then
        begin

        end
        else
        begin
          Caret.Element := pel;
          Caret.CharIndex := Length(pel.DisplayText);
          Caret.EOL := false;
          CaretToSelection;
        end;
      end
      else
      begin
        if TestSelection then
        begin

        end
        else
        begin
          Caret.Element := el;
          Caret.CharIndex := 0;
          CaretToSelection;
        end;
      end;
      Refresh;
    end;

    if (el is TBulletStart) then
    begin
      nel := NextElement(el);
      if nel is TBulletElement then
      begin
        if TestSelection then
        begin

        end
        else
        begin
          Caret.Element := nel;
          Caret.CharIndex := 1;
          Caret.EOL := false;
          CaretToSelection;
        end;
      end
      else
      begin
        if TestSelection then
        begin

        end
        else
        begin
          Caret.Element := el;
          Caret.CharIndex := 0;
          CaretToSelection;
        end;
      end;
      Refresh;
    end;
  end
  else
  begin
    Caret.Element := nil;
    Caret.CharIndex := 0;
    Refresh;
  end;

  if not TestSelection then
  begin
    if (Caret.Element <> OldCaret) or (Caret.CharIndex <> OldCaretIndex) then
    begin
      DoCaretChanged;
    end;

    if (OldSelectionFrom <> Selection.FromElement) or (OldSelectionFromIndex <> Selection.FromChar)
      or (OldSelectionTo <> Selection.ToElement) or (OldSelectionToIndex <> Selection.ToChar) then
    begin
      DoSelectionChanged;
    end;
  end;
end;

function TAdvRichEditorBase.XYToChar(X,Y: integer; el: TREElement; var CX,CY: integer): integer;
var
  s,su,ss,sus,lsu: string;
  vp,vps: integer;
  sz,szs: TTextSize;
  xy,xye: TPoint;
  idx, wl, cw,cws,lh,lx,ly,lidx: integer;
  SP: TPoint;
  nel: TREElement;
  linecross: boolean;
begin
  Result := -1;


  if el is TTextElement then
  begin
    nel := NextElement(el);

    s := (el as TTextElement).DisplayText;

    sp := TopLeft;

    wl := Length(s);

    vp := MidPos(' ',s);

    xy := el.XY;
    xye := el.XYE;

    xy := Point(xy.X - SP.X , xy.Y);
    xye :=  Point(xye.X - SP.X , xye.Y);

    idx := 0;

    lsu := '';
    lx := -1;
    ly := -1;
    lidx := -1;

    LH := el.LH;

    while (vp > 0) do
    begin
      su := Copy(s,1,vp);
      sz := GetTextSize(Canvas, el, su);

      if xy.X + sz.cx >= GetLineWidth then
      begin
        xy.X := el.Indent + OffsetX;

        if (Y >= xy.Y) and (Y <= xy.Y + LH) then
        begin
          CX := xy.X;
          CY := xy.Y;

          Result := idx - 1;
          break;
        end;

        // calculat X-offset for a new line
        if (el.Alignment in [taCenter, taRightJustify]) then
        begin
          cws := OffsetX;
          ss := s;
          vps := MidPos(' ',ss);

          linecross := false;

          while (ss <> '') do
          begin
            if vps > 0 then
              sus := Copy(ss,1,vps)
            else
              sus := ss;

            szs := GetTextSize(Canvas, el, sus);

            if cws + szs.cx >= GetLineWidth then
            begin
              ss := '';
              linecross := true;
            end
            else
            begin
              cws := cws + szs.cx;
              if vps > 0 then
                Delete(ss,1,vps)
              else
                ss := '';

              vps := MidPos(' ',ss);
            end;
          end;

          if not linecross and Assigned(nel) then
            cws := cws + GetLineWidth - nel.XY.X;

          case el.Alignment of
          taRightJustify: xy.X := OffsetX + Round(GetLineWidth - cws - 1);
          taCenter: xy.X := OffsetX + Round(GetLineWidth - cws - 1) div 2;
          end;
        end;

        xy.Y := xy.Y + LH;
        lh := sz.cy;
      end;

      if (X >= xy.X) and (X < xy.X + sz.cx) and (Y >= xy.Y) and (Y <= xye.Y + LH)  then
      begin
        lsu := su;
        lx := xy.X;
        ly := xy.Y;
        lidx := idx;
      end;


      if (X >= xy.X) and (X < xy.X + sz.cx) and (Y >= xy.Y) and (Y <= xy.Y + LH)  then
      begin
        cw := GetCharInWord(el, su, X - xy.X);

        CX := xy.X + GetCharPos(su,cw);
        CY := xy.Y;

        Result := idx + cw;
        Delete(s,1,vp);
        break;
      end;

      idx := idx + Length(su);

      xy.X := xy.X + sz.cx;

      Delete(s,1,vp);
      vp := MidPos(' ',s);
    end;

    if (s <> '') and (Result = -1) then
    begin
      sz := GetTextSize(Canvas, el, s);

      if xy.X + sz.cx > GetLineWidth then
      begin
        xy.X := OffsetX;

        if (Y >= xy.Y) and (Y <= xye.Y + el.LH) then
        begin
          CX := xy.X;
          CY := xy.Y;

          Result := idx;
        end;

        // calculat X-offset for a new line
        if (el.Alignment in [taCenter, taRightJustify]) then
        begin
          cws := OffsetX;
          ss := s;
          vps := MidPos(' ',ss);

          while (ss <> '') do
          begin
            sus := Copy(ss,1,vps);
            szs := GetTextSize(Canvas, el, sus);

            if cws + szs.cx >= GetLineWidth then
            begin
              ss := '';
            end
            else
            begin
              cws := cws + szs.cx;
              if vps > 0 then
                Delete(ss,1,vps)
              else
                ss := '';
              vps := MidPos(' ',ss);
            end;
          end;

          case el.Alignment of
          taRightJustify: xy.X := OffsetX + Round(GetLineWidth - cws - 1);
          taCenter: xy.X := OffsetX + Round(GetLineWidth - cws - 1) div 2;
          end;
        end;

        xy.Y := xy.Y + el.LH;
      end;

      if (X >= xy.X) and (X < xy.X + sz.cx) and (Y >= xy.Y) and (Y <= xye.Y + el.LH) then
      begin
        cw := GetCharInWord(el, s, X - xy.X);
        CX := xy.X + GetCharPos(s, cw);
        Result := idx + cw;
      end
      else
        if (x >= xy.X + sz.cx) and (Result = -1) then
        begin
            Result := wl;
        end
        else
        begin
          if (lx <> -1) and (lsu <> '') then
          begin
            cw := GetCharInWord(el, lsu, X - lx);

            CX := lx + GetCharPos(lsu,cw);
            CY := ly;

            Result := lidx + cw;
          end
        end;
    end;
  end;
end;

function TAdvRichEditorBase.XYToWord(X,Y: integer; el: TREElement): string;
var
  s,su: string;
  vp: integer;
  sz: TTextSize;
  xy: TPoint;

begin
  Result := '';

  if el is TTextElement then
  begin
    s := (el as TTextElement).Text;

    vp := Pos(' ',s);

    xy := el.XY;

    while (vp > 0) do
    begin
      su := Copy(s,1,vp);
      sz := GetTextSize(Canvas, el, su);

      if xy.X + sz.cx > GetLineWidth then
      begin
        xy.X := 0;
        xy.Y := xy.Y + el.LH;
      end;

      if (X > xy.X) and (X < xy.X + sz.cx) and (Y > xy.Y) and (Y < xy.Y + el.LH) then
      begin
        Result := su;
        break;
      end;

      xy.X := xy.X + sz.cx;

      Delete(s,1,vp);
      vp := Pos(' ',s);
    end;

    if (s <> '') and (Result = '') then
    begin
      sz := GetTextSize(Canvas, el, s);

      if (X > xy.X) and (X < xy.X + sz.cx) then
        Result := s;
    end;
  end;
end;


function TAdvRichEditorBase.XYToElement(X,Y: integer; var el: TREElement): boolean;
var
  i: integer;
  tel: TREElement;
  flg: boolean;
  SP: TPoint;
  xy: TPoint;

begin
  el := nil;
  i := 0;
  flg := false;

  SP := TopLeft;

  while i < FContext.Content.Count do
  begin
    tel := FContext.Content.Items[i];

    xy := tel.XY;
    xy := Point(xy.X - SP.X, xy.Y);

    if (X < XY.X) and (Y > XY.Y) and (Y < XY.Y + tel.LH) and (i > 0) then
    begin
      el := FContext.Content.Items[i - 1];
      break;
    end;

    if XY.Y > 0 then
      flg := true;

    if (Y < XY.Y) and (i > 0) then
    begin
      el := FContext.Content.Items[i - 1];

      if (FContext.Content.Items[i] is TBulletElement) and (el is TLineBreakElement) and (i > 1) then
        el := FContext.Content.Items[i - 2];

      break;
    end;

    if flg and (XY.Y = 0) {and not (tel is TLineBreakElement)} then
    begin
      el := FContext.Content.Items[i - 1];
      break;
    end;

    inc(i);
  end;

  if (i = FContext.Content.Count) and (FContext.Content.Count > 0) then
  begin
    el := FContext.Content.Items[i - 1];
  end;

  if (el is TBulletEnd) then
    el := PreviousElement(el);

  Result := Assigned(el);
end;

function TAdvRichEditorBase.XYToWord(X, Y: integer): string;
var
  el: TREElement;
begin
  inherited;
  if XYToElement(X,Y,el) then
  begin
    if (el is TTextElement) then
    begin
      Result := XYToWord(X,Y,el);
    end;
  end;
end;

procedure TAdvRichEditorBase.SetSelectionAttribute(AError: boolean);
var
  i: integer;
begin
  PushContext;
  GetSelectionElements;
  for i := 0 to FSelectionElements.Count - 1 do
  begin
    if FSelectionElements[i] is TTextElement then
      (FSelectionElements[i] as TTextElement).Error := AError;
  end;
  Refresh;
end;

procedure TAdvRichEditorBase.SetSelectionHighlight;
var
  i: integer;
begin
  GetSelectionElements(stDefault);

  if FSelectionElements.Count > 0 then
  begin
    PushContext;

    for i := 0 to FSelectionElements.Count - 1 do
    begin
      FSelectionElements[i].Highlight := true;
    end;
  end;

  Refresh;
end;

procedure TAdvRichEditorBase.UnMerge;
begin
  if FMergeContext.Content.Count > 0 then
  begin
    FContext.Clear;
    CloneContext(FMergeContext, FContext);
    ClearSelection;
    Refresh;
  end;
end;

procedure TAdvRichEditorBase.Merge(NamesAndValues: TStringList);
var
  i,j: integer;
  el: TREElement;
  gr: TPictureElement;
  pic: TPicture;
begin
  FMergeContext.Clear;
  CloneContext(FContext, FMergeContext);

  for i := 0 to FContext.Content.Count - 1 do
  begin
    el := FContext.Content[i];

    if el.MergeRef <> '' then
    begin
      j := NamesAndValues.IndexOfName(el.MergeRef);

      if j >= 0 then
      begin
        el.Text := NamesAndValues.ValueFromIndex[j];

        if Assigned(NamesAndValues.Objects[j]) then
        begin
          pic := TPicture(NamesAndValues.Objects[j]);
          gr := TPictureElement.Create;
          gr.Picture.Assign(pic);
          FContext.Content.Insert(ElementIndex(el), gr);
        end;

        el.MergeRef := '';
      end;
    end;
  end;

  Refresh;
end;

procedure TAdvRichEditorBase.SetSelectionMergeField(AMergeName: string);
var
  i: integer;
begin
  GetSelectionElements(stDefault);

  if FSelectionElements.Count > 0 then
  begin
    PushContext;

    FSelectionElements[0].MergeRef := AMergeName;

    for i := 1 to FSelectionElements.Count - 1 do
    begin
      FSelectionElements[0].Text := FSelectionElements[0].Text + FSelectionElements[i].Text;
    end;

    for i := FSelectionElements.Count - 1 downto 1 do
      DeleteElement(FSelectionElements[i]);

    SelectWordAtCaret;
  end;

  Refresh;
end;

procedure TAdvRichEditorBase.SetSelectionHint(AHint: string);
var
  i: integer;
begin
  GetSelectionElements(stDefault);

  if FSelectionElements.Count > 0 then
  begin
    PushContext;

    for i := 0 to FSelectionElements.Count - 1 do
    begin
      FSelectionElements[0].Hint := AHint;
    end;
  end;

  Refresh;
end;


procedure TAdvRichEditorBase.SetSelectionBkColor(AColor: TColor);
var
  i: integer;
  clr: TColor;
begin
  GetSelectionElements(SelectionType);

  if AColor = clNone then
    clr := ColorToRGB(Color)
  else
    clr := ColorToRGB(AColor);

  if FSelectionElements.Count = 0 then
  begin
    Color := clr;
    FDefaultChanged := true;
  end
  else
  begin
    PushContext;

    for i := 0 to FSelectionElements.Count - 1 do
      FSelectionElements[i].Color := clr;
  end;
  Refresh;
end;

procedure TAdvRichEditorBase.SetSelectionSubscript(DoSubScript: boolean);
var
  i: integer;
begin
  GetSelectionElements(SelectionType);

  if FSelectionElements.Count = 0 then
  begin
    if DoSubscript then
      FFontBaseLine := tbSubscript
    else
      FFontBaseLine := tbRegular;

    FDefaultChanged := true;
  end
  else
  begin
    PushContext;
    for i := 0 to FSelectionElements.Count - 1 do
    begin
      if DoSubScript then
        FSelectionElements[i].BaseLine := tbSubscript
      else
        FSelectionElements[i].BaseLine := tbRegular;
    end;
  end;
  Refresh;
end;

procedure TAdvRichEditorBase.SetSelectionSuperscript(DoSuperScript: boolean);
var
  i: integer;
begin
  GetSelectionElements(SelectionType);

  if FSelectionElements.Count = 0 then
  begin
    if DoSuperscript then
      FFontBaseLine := tbSuperscript
    else
      FFontBaseLine := tbRegular;

    FDefaultChanged := true;
  end
  else
  begin
    PushContext;
    for i := 0 to FSelectionElements.Count - 1 do
    begin
      if DoSuperScript then
        FSelectionElements[i].BaseLine := tbSuperscript
      else
        FSelectionElements[i].BaseLine := tbRegular;
    end;
  end;
  Refresh;
end;

procedure TAdvRichEditorBase.SetSelectionBold(DoBold: boolean);
var
  i: integer;
begin
  GetSelectionElements(SelectionType);

  if FSelectionElements.Count = 0 then
  begin
    if DoBold then
      GetDefaultFont.Style := GetDefaultFont.Style + [TFontStyle.fsBold]
    else
      GetDefaultFont.Style := GetDefaultFont.Style - [TFontStyle.fsBold];

    FDefaultChanged := true;
  end
  else
  begin
    PushContext;
    for i := 0 to FSelectionElements.Count - 1 do
    begin
      if DoBold then
        FSelectionElements[i].Font.Style := FSelectionElements[i].Font.Style + [TFontStyle.fsBold]
      else
        FSelectionElements[i].Font.Style := FSelectionElements[i].Font.Style - [TFontStyle.fsBold];
    end;
  end;
  Refresh;
end;

procedure TAdvRichEditorBase.SetSelectionStrikeOut(DoStrikeOut: boolean);
var
  i: integer;
begin
  GetSelectionElements(SelectionType);

  if FSelectionElements.Count = 0 then
  begin
    if DoStrikeOut then
      GetDefaultFont.Style := GetDefaultFont.Style + [TFontStyle.fsStrikeOut]
    else
      GetDefaultFont.Style := GetDefaultFont.Style - [TFontStyle.fsStrikeOut];

    FDefaultChanged := true;
  end
  else
  begin
    PushContext;
    for i := 0 to FSelectionElements.Count - 1 do
    begin
      if DoStrikeOut then
        FSelectionElements[i].Font.Style := FSelectionElements[i].Font.Style + [TFontStyle.fsStrikeOut]
      else
        FSelectionElements[i].Font.Style := FSelectionElements[i].Font.Style - [TFontStyle.fsStrikeOut];
     end;
  end;
  Refresh;
end;

procedure TAdvRichEditorBase.SetSelectionColor(AColor: TColor);
var
  i,cnt: integer;
  clr: TColor;
begin
  GetSelectionElements(SelectionType);

  if AColor = clNone then
    clr := ColorToRGB(GetFontColor)
  else
    clr := ColorToRGB(AColor);

  if FSelectionElements.Count = 0 then
  begin
    SetFontColor(clr);
    FDefaultChanged := true;
  end
  else
  begin
    PushContext;

    cnt := 0;
    for i := 0 to FSelectionElements.Count - 1 do
    begin
      if FSelectionElements[i] is TTextElement then
        inc(cnt);

      FSelectionElements[i].TextColor := clr;
    end;
    if cnt = 0 then
      SetFontColor(clr);
  end;
  Refresh;
end;

procedure TAdvRichEditorBase.SetSelectionError(DoError: boolean);
var
  i: integer;
begin
  if not HasSelection then
    Exit;

  PushContext;

  GetSelectionElements;

  for i := 0 to FSelectionElements.Count - 1 do
  begin
    if FSelectionElements[i] is TTextElement then
      (FSelectionElements[i] as TTextElement).Error := DoError;
  end;
  Refresh;
end;

procedure TAdvRichEditorBase.SetSelectionFontName(AName: string);
var
  i: integer;
  foundbul: boolean;
begin
  PushContext;

  GetSelectionElements(SelectionType);

  if FSelectionElements.Count = 0 then
  begin
    SetFontName(Font, AName);
    FDefaultChanged := true;
  end
  else
  begin
    for i := 0 to FSelectionElements.Count - 1 do
    begin
      SetFontName(FSelectionElements[i].Font, AName);
    end;

    foundbul := false;

    for i := 0 to FSelectionElements.Count - 1 do
    begin
      if not foundbul and (FSelectionElements[i] is TBulletElement) then
      begin
        OrderBullets(FSelectionElements[i]);
      end;
      if (FSelectionElements[i] is TBulletEnd) then
        foundbul := false;
    end;
  end;

  UpdateSize;
  Refresh;
end;

procedure TAdvRichEditorBase.SetSelectionFontSize(ASize: integer);
var
  i: integer;
  foundbul: boolean;

begin
  PushContext;

  GetSelectionElements(SelectionType);

  if FSelectionElements.Count = 0 then
  begin
    Font.Size := ASize;
    FDefaultChanged := true;
  end
  else
  begin
    PushContext;
    for i := 0 to FSelectionElements.Count - 1 do
    begin
      FSelectionElements[i].Font.Size := ASize;
    end;

    foundbul := false;

    for i := 0 to FSelectionElements.Count - 1 do
    begin
      if not foundbul and (FSelectionElements[i] is TBulletElement) then
      begin
        OrderBullets(FSelectionElements[i]);
      end;

      if (FSelectionElements[i] is TBulletEnd) then
        foundbul := false;
    end;
  end;

  UpdateSize;
  Refresh;
end;

procedure TAdvRichEditorBase.SetSelectionHyperlink(AURL: string);
var
  i: integer;
begin
  if not HasSelection then
    Exit;

  PushContext;
  GetSelectionElements;
  for i := 0 to FSelectionElements.Count - 1 do
  begin
    FSelectionElements[i].URL := AURL;
  end;
  Refresh;
end;

procedure TAdvRichEditorBase.SetSelectionBullets(AType: TBulletType);
begin
  SetSelectionBullets(AType, GetFontName(Font));
end;

procedure TAdvRichEditorBase.SetSelectionBullets(AType: TBulletType; AFontName: string);
var
  i,j: integer;
  inds: TBulletStart;
  inde: TBulletEnd;
  bul: TBulletElement;
  CurType: TBulletType;
  CurIndex,CurIndent: integer;
  el,fel,nel: TREElement;
  selatend: boolean;
  hassel: boolean;
begin
  // bullet in bullet for now not allowed
  if IsCaretInBulletList(CurType,CurIndex,CurIndent) then
  begin
    if CurType = AType then
    begin
      DeleteBullets(AType);
    end
    else
    begin
      ChangeBulletType(AType, AFontName);
      OrderBullets(Caret.Element);
    end;

    Refresh;
    Exit;
  end;

  PushContext;

  hassel := HasSelection;

  if not hassel then
    CaretToSelection;

  GetSelectionElements(stLine);

  selatend := false;

  if (FSelectionElements.Count = 1) and (Caret.Element = FSelectionElements.Items[0])
    and (Caret.CharIndex = 1) and (Caret.Element = LastElement) then
    selatend := true;

  if (FSelectionElements.Count > 0) and not selatend then
  begin
    el := PreviousElement(FSelectionElements.Items[0]);

    while Assigned(el) and ((el is TTextElement) or (el is TCustomGraphicElement)) do
    begin
      FSelectionElements.Insert(0,el);
      el := PreviousElement(el);
    end;

    fel := FSelectionElements.Items[0];
    i := ElementIndex(fel);

    inds := TBulletStart.Create;
    inds.Font.Assign(fel.Font);
    inds.Alignment := fel.Alignment;
    inds.Indent := BULLETINDENT;
    inds.StartIndent := fel.Indent;
    inds.TextColor := fel.TextColor;
    inds.Color := fel.Color;
    FContext.Content.Insert(i,inds);

    Selection.FromElement := inds;
    Selection.FromChar := 0;

    bul := TBulletElement.Create;
    bul.&Type := AType;
    bul.Font.Assign(fel.Font);
    bul.BulletFont := AFontName;
    bul.Indent := BULLETINDENT + fel.Indent;
    bul.Alignment := fel.Alignment;
    bul.Color := fel.Color;
    bul.TextColor := fel.TextColor;
    bul.Spacing := BulletSpacing;
    FContext.Content.Insert(i + 1,bul);

    for j := 0 to FSelectionElements.Count - 1 do
    begin
      FSelectionElements.Items[j].Indent := BULLETINDENT + fel.Indent;
      FSelectionElements.Items[j].Alignment := fel.Alignment;
    end;

    j := 1;
    while (j <= FSelectionElements.Count - 2) do
    begin
      if (FSelectionElements.Items[j] is TBulletStart) or
         (FSelectionElements.Items[j] is TBulletEnd) or
         (FSelectionElements.Items[j] is TBulletElement) then
      begin
        DeleteElement(FSelectionElements.Items[j]);
        FSelectionElements.Delete(j);
      end
      else
        inc(j);
    end;

    for j := 1 to FSelectionElements.Count - 2 do
    begin
      if (FSelectionElements.Items[j] is TLineBreakElement) and not (FSelectionElements.Items[j - 1] is TLineBreakElement) then
      begin
        i := FContext.Content.IndexOf(FSelectionElements.Items[j]);
        bul := TBulletElement.Create;
        bul.&Type := AType;
        nel := NextElement(FSelectionElements.Items[j]);

        if not Assigned(nel) then
          nel := fel;

        bul.Font.Assign(nel.Font);
        bul.BulletFont := AFontName;
        bul.Indent := BULLETINDENT + nel.Indent;
        bul.Spacing := BulletSpacing;
        bul.Alignment := nel.Alignment;
        bul.Color := nel.Color;
        bul.TextColor := nel.TextColor;
        FContext.Content.Insert(i + 1,bul);
      end;
    end;

    // last element
    i := FContext.Content.IndexOf(FSelectionElements.Items[FSelectionElements.Count - 1]);
    inde := TBulletEnd.Create;
    inde.Font.Assign(fel.Font);
    inde.Alignment := fel.Alignment;
    inde.Color := fel.Color;
    inde.TextColor := fel.TextColor;
    FContext.Content.Insert(i + 1, inde);

    Selection.ToElement := inde;
    Selection.ToChar := 1;
  end
  else
  begin
    inds := TBulletStart.Create;
    inds.Indent := BULLETINDENT;
    FContext.Content.Add(inds);

    bul := TBulletElement.Create;
    bul.&Type := AType;
    bul.Font.Assign(GetDefaultFont);
    bul.BulletFont := GetFontName(Font);
    bul.Indent := BULLETINDENT;
    bul.Spacing := BulletSpacing;
    bul.TextColor := GetFontColor;
    FContext.Content.Add(bul);

    inde := TBulletEnd.Create;
    inde.Font.Assign(GetDefaultFont);
    FContext.Content.Add(inde);
    Caret.Element := inde;
    Caret.CharIndex := 0;
    CaretToSelection;
  end;

  if Assigned(bul) then
    OrderBullets(bul);

  if not hassel then
    CaretToSelection;

  ShowTree;


  Refresh;
end;

procedure TAdvRichEditorBase.SelectError(ErrorSelection: TErrorSelection);

   function MoveToError(StartElement: TREElement; Fwd: boolean): TREElement;
   var
     re: TREElement;
   begin
     Result := nil;
     re := StartElement;
     repeat
       if (re is TTextElement) and (re as TTextElement).Error and (re <> FCurrErr) then
       begin
         Result := re;
         break;
       end;

       if fwd then
         re := NextElement(re)
       else
         re := PreviousElement(re);
     until not Assigned(re);
   end;

begin
  case ErrorSelection  of
    esFirst:
      begin
        FCurrErr := nil;
        FCurrErr := MoveToError(FirstElement, true);
      end;
    esNext:
      begin
        if FCurrErr = nil then
          FCurrErr := MoveToError(FirstElement, true)
        else
          FCurrErr := MoveToError(FCurrErr, true);
      end;
    esPrevious:
      begin
        if FCurrErr = nil then
          FCurrErr := MoveToError(LastAddedElement, false)
        else
          FCurrErr := MoveToError(FCurrErr, false);
      end;
    esLast:
      begin
        FCurrErr := nil;
        FCurrErr := MoveToError(LastAddedElement, false);
      end;
  end;

  if Assigned(FCurrErr) then
    SelectElement(FCurrErr)
  else
    ClearSelection;
end;

procedure TAdvRichEditorBase.IgnoreSelectedError(ErrorWord: string);
begin
  if not Assigned(FCurrErr) then
    Exit;

  (FCurrErr as TTextElement).Error := false;

  Refresh;
end;

procedure TAdvRichEditorBase.IgnoreAllErrors(ErrorWord: string);
var
  i: integer;
  re: TREElement;
begin
  for i := 0 to FContext.Content.Count - 1 do
  begin
    re := FContext.Content.Items[i];

    if (re is TTextElement) and (re as TTextElement).Error then
    begin
      if re.Text = ErrorWord then
      begin
        (re as TTextElement).Error := false;
      end;
    end;
  end;

  Refresh;
end;

procedure TAdvRichEditorBase.CorrectSelectedError(NewWord: string);
begin
  if not Assigned(FCurrErr) then
    Exit;

  FCurrErr.Text := NewWord;
  (FCurrErr as TTextElement).Error := false;

  Refresh;
end;

procedure TAdvRichEditorBase.CorrectAllErrors(ErrorWord, NewWord: string);
var
  i: integer;
  re: TREElement;
begin
  for i := 0 to FContext.Content.Count - 1 do
  begin
    re := FContext.Content.Items[i];

    if (re is TTextElement) and (re as TTextElement).Error then
    begin
      if re.Text = ErrorWord then
      begin
        re.Text := NewWord;
        (re as TTextElement).Error := false;
      end;
    end;
  end;

  Refresh;
end;

function TAdvRichEditorBase.CanUnindent: boolean;
var
  el: TREElement;
begin
  Result := false;

  SelectionToPaintSelection;

  el := PaintSelection.FromElement;

  while Assigned(el) do
  begin
    if el.Indent > 0 then
    begin
      Result := true;
      break;
    end;
    if (el = PaintSelection.ToElement) then
      el := nil
    else
      el := NextElement(el);
  end;
end;


procedure TAdvRichEditorBase.SetSelectionIndent(AIndent: integer);
var
  i: integer;
  el,pel: TREElement;
begin
  PushContext;

  GetSelectionElements(stLine);

  if FSelectionElements.Count > 0 then
  begin
    // extend selection when in bullet list
    el := FSelectionElements.Items[0];
    pel := PreviousElement(el);
    if (pel is TBulletElement) then
    begin
      FSelectionElements.Insert(0,pel);
      pel := PreviousElement(pel);
      if (pel is TBulletStart) then
      begin
        FSelectionElements.Insert(0,pel);
      end;
    end;

    for i := 0 to FSelectionElements.Count - 1 do
    begin
      el := FSelectionElements.Items[i];

      if el is TBulletStart then
        (el as TBulletStart).StartIndent := Max(0,(el as TBulletStart).StartIndent + AIndent);

      if not (el is TBulletEnd) then
        el.Indent := Max(el.BulletIndent,el.Indent + AIndent);
    end;
  end;

  UpdateSize;

  Refresh;
end;

procedure TAdvRichEditorBase.SetSelectionItalic(DoItalic: boolean);
var
  i: integer;
begin
  GetSelectionElements(SelectionType);

  if FSelectionElements.Count = 0 then
  begin
    if DoItalic then
      GetDefaultFont.Style := GetDefaultFont.Style + [TFontStyle.fsItalic]
    else
      GetDefaultFont.Style := GetDefaultFont.Style - [TFontStyle.fsItalic];

    FDefaultChanged := true;
  end
  else
  begin
    PushContext;

    for i := 0 to FSelectionElements.Count - 1 do
    begin
      if DoItalic then
        FSelectionElements[i].Font.Style := FSelectionElements[i].Font.Style + [TFontStyle.fsItalic]
      else
        FSelectionElements[i].Font.Style := FSelectionElements[i].Font.Style - [TFontStyle.fsItalic];
    end;
  end;
  Refresh;
end;

procedure TAdvRichEditorBase.SetSelectionLine(DoLine: boolean);
var
  i: integer;
begin
  GetSelectionElements;
  for i := 0 to FSelectionElements.Count - 1 do
  begin
    if FSelectionElements[i] is TTextElement then
    begin
      (FSelectionElements[i] as TTextElement).Line := DoLine;
    end;
  end;
  Refresh;
end;

procedure TAdvRichEditorBase.SetSelectionUnderline(DoUnderline: boolean);
var
  i: integer;
begin
  GetSelectionElements(SelectionType);

  if FSelectionElements.Count = 0 then
  begin
    if DoUnderline then
      GetDefaultFont.Style := GetDefaultFont.Style + [TFontStyle.fsUnderline]
    else
      GetDefaultFont.Style := GetDefaultFont.Style - [TFontStyle.fsUnderline];
    FDefaultChanged := true;
  end
  else
  begin
    PushContext;

    for i := 0 to FSelectionElements.Count - 1 do
    begin
      if DoUnderline then
        FSelectionElements[i].Font.Style := FSelectionElements[i].Font.Style + [TFontStyle.fsUnderline]
      else
        FSelectionElements[i].Font.Style := FSelectionElements[i].Font.Style - [TFontStyle.fsUnderline];
    end;
  end;
  Refresh;
end;

procedure TAdvRichEditorBase.SetSelLength(const Value: integer);
var
  el: TREElement;
  ss,cnt,i,l: integer;
begin
  cnt := 0;

  ss := SelStart;

  for i := 0 to FContext.Content.Count - 1 do
  begin
    el := FContext.Content.Items[i];

    l := el.TextLength;

    if el is TLineBreakElement then
      l := 2;

    if cnt + l >= Value + ss then
    begin
      Selection.ToElement := el;
      Selection.ToChar := Value + ss - cnt;
      break;
    end;
    cnt := cnt + l;
  end;
end;

procedure TAdvRichEditorBase.SetSelStart(const Value: integer);
var
  el: TREElement;
  cnt,i,l: integer;
begin
  cnt := 0;

  for i := 0 to FContext.Content.Count - 1 do
  begin
    el := FContext.Content.Items[i];

    l := el.TextLength;

    if el is TLineBreakElement then
      l := 2;

    if cnt + l > Value then
    begin
      Selection.FromElement := el;
      Selection.FromChar := Value - cnt;
      break;
    end;
    cnt := cnt + l;
  end;
end;

procedure TAdvRichEditorBase.SetAutoCorrect(const Value: TRichEditorAutoCorrect);
begin
  FAutoCorrect.Assign(Value);
end;


procedure TAdvRichEditorBase.ShowSelection;
{$IFDEF TMSDEBUG}
var
  el: TREElement;
  al: string;
  i: integer;
{$ENDIF}
begin
  {$IFDEF TMSDEBUG}
  TDebug.Separator;

  i := 0;

  TDebug.Write('*******SELECTION***********');

  for el in FSelectionElements do
  begin
    case el.Alignment of
      taLeftJustify: al := ' al:Left';
      taRightJustify: al := ' al:Right';
      taCenter: al := ' al:Center'
    end;
    TDebug.Write(inttostr(i)+'|'+el.ClassName+':'+al+':'+inttostr(el.Indent)+' ['+inttostr(el.XY.X)+','+inttostr(el.XY.Y)+'] '+el.Text);
    inc(i);
  end;
  {$ENDIF}
end;


procedure TAdvRichEditorBase.ShowTree;
{$IFDEF TMSDEBUG}
var
  el: TREElement;
  al: string;
  i: integer;
  txt: string;
{$ENDIF}
begin
  {$IFDEF TMSDEBUG}

  TDebug.Separator;

  i := 0;

  for el in FContext.Content do
  begin
    case el.Alignment of
      taLeftJustify: al := ' al:Left';
      taRightJustify: al := ' al:Right';
      taCenter: al := ' al:Center'
    end;

    txt := el.Text;

    txt := StringReplace(txt,#13,'\n',[rfReplaceAll]);

    TDebug.Write(inttostr(i)+'|'+el.ClassName+':'+al+':'+inttostr(el.Indent)+' ['+inttostr(el.XY.X)+','+inttostr(el.XY.Y)+'] {'+GetFontName(el.Font)+'-' + inttostr(el.GetFontSize)+'} |' + txt + '| -clr:'+inttohex(el.Color,6) );
    inc(i);
  end;

  if Assigned(Caret.Element) then
  begin
    TDebug.Write('******CARET**********');
    TDebug.Write(Caret.Element.ClassName+':'+Caret.Element.Text+':'+inttostr(Caret.CharIndex)+' [' +  inttostr(Caret.XY.X)+','+inttostr(Caret.XY.Y)+','+inttostr(Caret.LH)+'] - index:' + inttostr(FContext.Content.IndexOf(Caret.Element)) );
  end;

  if Assigned(Selection.FromElement) then
  begin
    TDebug.Write('******SELFROM********');
    TDebug.Write(Selection.FromElement.ClassName+' ['+inttostr(Selection.FromChar)+'] '+Selection.FromElement.Text);
  end;

  if Assigned(Selection.ToElement) then
  begin
    TDebug.Write('******SELTO**********');
    TDebug.Write(Selection.ToElement.ClassName+' [' + inttostr(Selection.ToChar)+'] '+Selection.ToElement.Text+':');
  end;
  {$ENDIF}
end;

function TAdvRichEditorBase.ContentAsHTML(ImgPath: string = ''): string;
begin
  Result := GetContentAsHTML(false, ImgPath);
end;

function TAdvRichEditorBase.SelectionAsHTML: string;
begin
  Result := GetContentAsHTML(true);
end;

function IPos(su,s:string):Integer;
begin
  Result := Pos(UpperCase(su),UpperCase(s));
end;

function TagReplaceString(const Srch,Repl:string;var Dest:string):Boolean;
var
  i: Integer;
begin
  i := IPos(srch,dest);
  if i > 0 then
  begin
    Result := True;
    Delete(Dest,i,Length(Srch));
    Dest := Copy(Dest,1,i-1) + Repl + Copy(Dest,i,Length(Dest));
  end
  else
    Result := False;
end;

function UnFixMarkup(su: string; SpecialChars: boolean = true):string;
var
  i: integer;
begin
  while Pos('&lt;',su) > 0 do
  begin
    TagReplacestring('&lt;','<',su);
  end;

  while Pos('&gt;',su) > 0 do
  begin
    TagReplacestring('&gt;','>',su);
  end;

  while Pos('&amp;',su) > 0 do
  begin
    TagReplacestring('&amp;','&',su);
  end;

  while Pos('&quot;',su) > 0 do
  begin
    TagReplacestring('&quot;','"',su);
  end;

  if SpecialChars then
  begin
    for i := 1 to NumSpecialChar do
    begin
      while Pos(string(HTMLSpecialChar[i]),su) > 0 do
      begin
        TagReplacestring(string(HTMLSpecialChar[i]),string(HTMLEncodedChar[i]),su);
      end;
    end;
  end;

  Result := su;
end;


function FixMarkup(su:string; SpecialChars: boolean = true): string;
var
  i: integer;
begin
  while Pos('&',su) > 0 do
  begin
    TagReplacestring('&','*amp;',su);
  end;

  while Pos('*amp;',su) > 0 do
  begin
    TagReplacestring('*amp;','&amp;',su);
  end;

  while Pos('"',su) > 0 do
  begin
    TagReplacestring('"','&quot;',su);
  end;

  while Pos('<',su) > 0 do
  begin
    TagReplacestring('<','&lt;',su);
  end;

  while Pos('>',su) > 0 do
  begin
    TagReplacestring('>','&gt;',su);
  end;

  if SpecialChars then
  begin
    for i := 1 to NumSpecialChar do
    begin
      while Pos(string(HTMLEncodedChar[i]),su) > 0 do
      begin
        TagReplacestring(string(HTMLEncodedChar[i]),string(HTMLSpecialChar[i]),su);
      end;
    end;
  end;

  Result := su;
end;

function EncodePicture(const Pic: TGDIPPicture): string;
var
  stream: TMemoryStream;
begin
  stream := TMemoryStream.Create;
  try
    pic.SaveToStream(stream);

    Result := Base64Encode(stream);
  finally
    stream.Free;
  end;
end;

function TAdvRichEditorBase.GetContentAsHTML(UseSelection: boolean; ImgPath: string = ''): string;
var
  sl: TStringList;
  i,curindent: integer;
  el,firstel,nel: TREElement;
  isbul,isbold,isitalic,isunderline,isstrike,isaligned,issub,issuper,isfntclr,isbkclr,isfntface,isfntsize,isfntattr,isaurl,fntclose: boolean;
  fontface,fontattr, elfontface: string;
  fontsize,elfontsize: integer;
  clr,bkclr: TColor;
  s,url: string;
  alignment: TAlignment;
  orderedlist: boolean;
  imgidx: integer;
  imgname: string;
  imgext: string;
  pic: TPictureElement;
  pf: TPictureFormat;
  dolb: boolean;
  preamble: string;
  fstyle,fweight: string;
  didbold,diditalic,didunderline,didstrike: boolean;

  function AppendAttr(Attr,Key,AValue: string): string;
  begin
    Result := Attr;
    if Attr <>'' then
      Result := Result + ' ';

    Result := Result + Key +'="' + AValue + '"';
  end;

begin
  TidyElements;
  FHTMLImageList.Clear;

  if UseSelection then
    GetSelectionElements(stDefault)
  else
  begin
    FSelectionElements.Clear;

    for i := 0 to FContext.Content.Count - 1 do
      FSelectionElements.Add(FContext.Content.Items[i]);
  end;

  sl := TStringList.Create;

  firstel := TTextElement.Create;

  i := 0;
  curindent := 0;
  imgidx := 0;

  isbul := false;
  isbold := false;
  isitalic := false;
  isunderline := false;
  isstrike := false;
  isaligned := false;
  issuper := false;
  issub := false;
  isfntclr := false;
  isfntattr := false;
  isfntface := false;
  isfntsize := false;
  isaurl := false;
  isbkclr := false;
  orderedlist := false;

  sl.Add('<HTML>');

  fstyle := 'normal';
  fweight := 'normal';

  if TFontStyle.fsItalic in Font.Style then
    fstyle := 'italic';

  if TFontStyle.fsBold in Font.Style then
    fweight := 'bold';


  sl.Add('<BODY style=''font-family:"'+ GetFontName(Font)+'";font-style;'+ fstyle+';font-weight:'+fweight+';font-size:'+inttostr(GetFontSize(Font))+'pt''>');

  fontface := GetFontName(GetDefaultFont);
  fontsize := GetFontSize(GetDefaultFont);

  clr := GetDefaultFontColor;
  bkclr := Color;
  s := '';
  url := '';
  alignment := taLeftJustify;

  while (i < FSelectionElements.Count) do
  begin
    el := FSelectionElements[i];

    if (el is TLineBreakElement) then
    begin
      nel := NextElement(el);

      if Assigned(nel) then
      begin
        dolb := true;

        if (nel is TBulletElement) then
          dolb := false;

        if (nel.Alignment <> el.Alignment) then
          dolb := false;

        if dolb then
          s := s + '<BR>';
      end
    end;

    if url <> el.URL then
    begin
      if url <> '' then
      begin
        s := s + '</A>';
        isaurl := false;
      end;

      url := el.URL;
      if url <> '' then
      begin
        s := s + '<A href="'+ StringReplace(el.URL, ' ', '%20',[rfReplaceAll]) +'">';
        isaurl := true;
      end;
    end;

    if (el is TBulletEnd) then
    begin
      if isbul then
      begin
        if orderedlist then
          s := s + '</OL>'
        else
          s := s + '</UL>';
        isbul := false;
      end
    end;

    if el.Indent <> curindent then
    begin
      curindent := el.Indent;
      if curindent > 0 then
        s := s + '<DIV style="margin-left: '+ inttostr(curindent)+'px">'
      else
        s := s + '</DIV>';
    end;

    if alignment <> el.Alignment then
    begin
      if isaligned then
      begin
        s := s + '</P>';
        isaligned := false;
      end;

      alignment := el.Alignment;

      if alignment = taCenter then
      begin
        s := s  + '<P align="center">';
        isaligned := true;
      end;

      if alignment = taRightJustify then
      begin
        s := s  + '<P align="right">';
        isaligned := true;
      end;
    end;

    if el is TTextElement then
    begin
      didbold := false;
      diditalic := false;
      didunderline := false;
      didstrike := false;

      if not isbold and (TFontStyle.fsBold in (el as TTextElement).Font.Style) then
      begin
        s := s + '<B>';
        isbold := true;
        didbold := true;
      end;

      if not isitalic and (TFontStyle.fsItalic in (el as TTextElement).Font.Style) then
      begin
        s := s + '<I>';
        isitalic := true;
        diditalic := true;
      end;

      if not isunderline and (TFontStyle.fsUnderline in (el as TTextElement).Font.Style) then
      begin
        s := s + '<U>';
        isunderline := true;
        didunderline := true;
      end;

      if not isstrike and (TFontStyle.fsStrikeOut in (el as TTextElement).Font.Style) then
      begin
        s := s + '<STRIKE>';
        isstrike := true;
        didstrike := true;
      end;

      if not issuper and ((el as TTextElement).Baseline = TTextBaseLine.tbSuperscript)  then
      begin
        s := s + '<SUP>';
        issuper := true;
      end
      else
      if issuper and not ((el as TTextElement).Baseline = TTextBaseLine.tbSuperscript) then
      begin
        s := s + '</SUP>';
        issuper := false;
      end;

      if not issub and ((el as TTextElement).Baseline = TTextBaseLine.tbSubscript) then
      begin
        s := s + '<SUB>';
        issub := true;
      end
      else
      if issub and not ((el as TTextElement).Baseline = TTextBaseLine.tbSubscript) then
      begin
        s := s + '</SUB>';
        issub := false;
      end;

      fntclose := false;
      fontattr := '';

      elfontface := GetFontName((el as TTextElement).Font);
      elfontsize := GetFontSize((el as TTextElement).Font);

      if fontface <> elfontface then
      begin
        if isfntface then
          fntclose := true;

        //if elfontface <> GetDefaultFontName then
        begin
          fontattr := AppendAttr(fontattr,'face',elfontface);
          fontface := elfontface;
          isfntface := true;
        end;
      end;

      if fontsize <> elfontsize then
      begin
        if isfntsize then
          fntclose := true;

        //if elfontsize <> GetDefaultFontSize then
        begin
          fontattr := AppendAttr(fontattr,'style','font-size:' +inttostr(elfontsize)+'pt');
          fontsize := elfontsize;
          isfntsize := true;
        end;
      end;

      if (clr <> (el as TTextElement).TextColor) then
      begin
        if isfntclr then
        begin
          fntclose := true;
          //s := s + '</FONT>';
          isfntclr := false;
        end;

        clr := (el as TTextElement).TextColor;

        if clr <> GetDefaultFontColor then
        begin
          fontattr := AppendAttr(fontattr, 'color', ColorToHtml(clr));
          //s := s + '<FONT color="' + ColorToHtml(clr) + '">';
          isfntclr := true;
        end;
      end;

      if fntclose then
      begin
        s := s + '</FONT>';
        isfntattr := false;
      end;

      if fontattr <> '' then
      begin
        s := s + '<FONT ' + fontattr +'>';
        isfntattr := true;
      end;


      if not didbold and isbold and not (TFontStyle.fsBold in (el as TTextElement).Font.Style) then
      begin
        s := s + '</B>';
        isbold := false;
      end;

      if not diditalic and isitalic and not (TFontStyle.fsItalic in (el as TTextElement).Font.Style) then
      begin
        s := s + '</I>';
        isitalic := false;
      end;

      if not didunderline and isunderline and not (TFontStyle.fsUnderline in (el as TTextElement).Font.Style) then
      begin
        s := s + '</U>';
        isunderline := false;
      end;

      if not didstrike and isstrike and not (TFontStyle.fsStrikeOut in (el as TTextElement).Font.Style) then
      begin
        s := s + '</STRIKE>';
        isstrike := false;
      end;


      if (bkclr <> (el as TTextElement).Color) and ((el as TTextElement).Color <> clNone) then
      begin
        if isbkclr then
        begin
          s := s + '</SPAN>';
          isbkclr := false;
        end;

        bkclr := (el as TTextElement).Color;

        if bkclr and $FFFFFF <> Color and $FFFFFF then
        begin
          s := s + '<SPAN style="background-color:' + ColorToHTML(bkclr) + '">';
          isbkclr := true;
        end;
      end;

      s := s + FixMarkup(el.Text);
    end;

    if (el is TPictureElement) then
    begin
      pic := (el as TPictureElement);


      {$IFDEF VCLLIB}
      pf := pic.Picture.PictureFormat;

      case pf of
      pfBMP: imgext := 'BMP';
      pfJPG: imgext := 'JPG';
      pfPNG: imgext := 'PNG';
      pfGIF: imgext := 'GIF';
      pfICO: imgext := 'ICO';
      end;
      {$ENDIF}

      {$IFDEF FMXLIB}
      pf := pfPNG;
      imgext := 'PNG';
      {$ENDIF}

      if (pf in [pfBMP, pfJPG, pfPNG, pfGIF, pfICO]) then
      begin
        case HTMLImages of
        igFile:
          begin
          imgname := AddBackSlash(imgpath)+'image' + inttostr(imgidx) + '.' + imgext;
          s := s + '<IMG src=file://' + imgname + '>';
          pic.Picture.SaveToFile(imgname);
          end;
        igInline:
          begin
            preamble := '<IMG src="data:image/'+Lowercase(imgext)+';base64,';
            s := s + preamble + EncodePicture(pic.Picture) + '">'
          end;
        igID:
          begin
            s := s + '<IMG src="cid:image' + inttostr(imgidx)+ '">';
            FHTMLImageList.Add(pic.Picture);
          end;
        end;
        inc(imgidx);
      end;
    end;


    if el is TBulletElement then
    begin
      orderedlist := (el as TBulletElement).&Type = btNumber;

      if not isbul then
      begin
        if orderedlist then
          s := s + '<OL>'
        else
          s := s + '<UL>';
      end;
      isbul := true;

      if (el as TBulletElement).&Type = btSquare then
        s := s + '<LI style="list-style-type:square">'
      else
        s := s + '<LI>';
    end;

    inc(i);
  end;

  if isaurl then
    s := s + '</A>';

  if isfntattr then
    s := s + '</FONT>';

  if isbold then
    s := s + '</B>';

  if isitalic then
    s := s + '</I>';

  if isunderline then
    s := s + '</U>';

  if isstrike then
    s := s + '</STRIKE>';

  if issub then
    s := s + '</SUB>';

  if issuper then
    s := s + '</SUP>';

  if isbkclr then
    s := s + '</SPAN>';

  if isfntclr then
    s := s + '</FONT>';

  sl.Add(s);

  sl.Add('</BODY>');
  sl.Add('</HTML>');

  firstel.Free;

  Result := sl.Text;

  sl.Free;
end;

function TAdvRichEditorBase.GetContentAsPlainText(UseSelection: boolean): string;
var
  i: integer;
  el: TREElement;
  sb: TStringStream;
begin
  Result := '';

  if UseSelection then
    GetSelectionElements(stDefault)
  else
  begin
    FSelectionElements.Clear;

    for i := 0 to FContext.Content.Count - 1 do
      FSelectionElements.Add(FContext.Content.Items[i]);
  end;


  sb := TStringStream.Create('');
  try
      for i := 0 to FSelectionElements.Count - 1 do
      begin
        el := FSelectionElements.Items[i];

        if el is TLineBreakElement then
          sb.WriteString(#13#10);

        if el is TTextElement then
          sb.WriteString(el.Text);
      end;
      Result := sb.DataString;

  finally
    sb.Free;
  end;
end;

procedure TAdvRichEditorBase.InsertAsRTF(rtfstring: string);
{$IFNDEF LCLLIB}
var
  doc: TRTFDocument;
  ts: TStringStream;
  i,ind: integer;
  fsz, fszn: integer;
  fs,fsn: TFontStyles;
  fn,b: string;
  fc, fbc,bkclr: TColor;
  fa: TAlignment;
  fbl: TRTFBaseLine;
  picel: TPictureElement;
  bulStart: TBulletStart;
  bulEl: TBulletElement;
  txtEl: TTextElement;
  re,pel: TREElement;
  bulidx: integer;
  ch: char;
{$ENDIF}
begin
{$IFNDEF LCLLIB}
  ts := TStringStream.Create('');
  ts.WriteString(rtfstring);
  ts.Position := 0;

  SelectionType := stSingle;
  bkclr := Color;

  BeginUpdate;

  try
    doc := TRTFDocument.Create;
    doc.ParseRTF(ts);
    doc.ParseObjects;

    //
    fs := [];
    fn := '';
    fc := clBlack;
    fbc := clNone;
    fsz := -1;
    fa := taLeftJustify;
    fbl := bNormal;
    ind := 0;
    bulidx := 0;

    for i := 0 to doc.ElementTable.Count - 1 do
    begin
      if doc.ElementTable.Items[i].Tab then
        InsertTab;

      // bullet start
      if doc.ElementTable.Items[i].BulletType = rbtStart then
      begin
        bulStart := TBulletStart(AddElement(TBulletStart));
        bulStart.StartIndent := 0;
        bulStart.Indent := BULLETINDENT;
        bulidx := 0;
        Continue;
      end;

      // bullet end
      if doc.ElementTable.Items[i].BulletType = rbtEnd then
      begin
        AddElement(TBulletEnd);
        bulidx := 0;
        Continue;
      end;

      // bullet item
      if doc.ElementTable.Items[i].BulletType = rbtItem then
      begin
        pel := LastElement;

        if Assigned(pel) and (pel is TTextElement) then
        begin
          re := InsertLineBreak(false);
          re.Indent := pel.Indent;
        end;

        bulEl := TBulletElement(AddElement(TBulletElement));
        bulEl.BulletFont := doc.ElementTable.Items[i].FontName;
        bulEl.Index := bulidx;
        bulEl.&Type := btCircle;
        bulEl.Indent := BULLETINDENT;
        bulEl.TextColor := GetFontColor;

        inc(bulidx);

        b := doc.ElementTable.Items[i].Text;

        if length(b) > 0 then
        begin
          ch := CharInStr(b,1);

          {$IFDEF DELPHIXE6_LVL}
          if ch.IsNumber then
            bulEl.&Type := btNumber;

          if ch.IsLetter then
            bulEl.&Type := btChar;
          {$ELSE}
          if CharInSet(ch,['0'..'9']) then
            bulEl.&Type := btNumber;

          if CharInSet(ch,['a'..'z','A'..'Z']) then
            bulEl.&Type := btChar;
          {$ENDIF}

        end;

        bulEl.Bullet := doc.ElementTable.Items[i].Text;
        if doc.ElementTable.Items[i].FontSize > 0 then
          bulEl.Font.Size := doc.ElementTable.Items[i].FontSize
        else
          bulEl.Font.Size := GetDefaultFontSize;
        Continue;
      end;

      // image
      if Assigned(doc.ElementTable.Items[i].Bitmap) then
      begin
        {$IFNDEF FMXLIB}
        picel := AddBitmap(doc.ElementTable.Items[i].Bitmap);
        {$ENDIF}

        {$IFDEF FMXLIB}
        picel := AddImage(TPicture(doc.ElementTable.Items[i].Bitmap));
        {$ENDIF}

        pel := PreviousElement(picel);
        if Assigned(pel) then
          picel.Alignment := pel.Alignment;

        Caret.Element := picel;
        Caret.CharIndex := 1;
      end
      else
      begin
        fsn := doc.ElementTable.Items[i].FontStyle;
        if fs <> fsn then
        begin
          if (TFontStyle.fsBold in fs) and not (TFontStyle.fsBold in fsn) then
            SetSelectionBold(false);
          if not (TFontStyle.fsBold in fs) and (TFontStyle.fsBold in fsn) then
            SetSelectionBold(true);

          if (TFontStyle.fsItalic in fs) and not (TFontStyle.fsItalic in fsn) then
            SetSelectionItalic(false);
          if not (TFontStyle.fsItalic in fs) and (TFontStyle.fsItalic in fsn) then
            SetSelectionItalic(true);

          if (TFontStyle.fsUnderline in fs) and not (TFontStyle.fsUnderline in fsn) then
            SetSelectionUnderline(false);
          if not (TFontStyle.fsUnderline in fs) and (TFontStyle.fsUnderline in fsn) then
            SetSelectionUnderline(true);

          fs := fsn;
        end;

        if fbl <> doc.ElementTable.Items[i].BaseLine then
        begin
          fbl := doc.ElementTable.Items[i].BaseLine;

          if fbl = bNormal then
          begin
            SetSelectionSubscript(false);
            SetSelectionSuperscript(false);
          end;
          if fbl = bSubScript then
          begin
            SetSelectionSubscript(true);
          end;
          if fbl = bSuperScript then
          begin
            SetSelectionSuperscript(true);
          end;
        end;

        fszn := doc.ElementTable.Items[i].FontSize;

        if (fsz <> fszn) then
        begin
          SetSelectionFontSize(fszn);
          fsz := fszn;
        end;

        if fc <> doc.ElementTable.Items[i].Color then
        begin
          SetSelectionColor(doc.ElementTable.Items[i].Color);
          fc := doc.ElementTable.Items[i].Color;
        end;

        if fbc <> doc.ElementTable.Items[i].BkColor then
        begin
          if doc.ElementTable.Items[i].BkColor <> clNone then
            SetSelectionBkColor(doc.ElementTable.Items[i].BkColor)
          else
            SetSelectionBkColor(bkclr);

          fbc := doc.ElementTable.Items[i].BkColor;
        end;

        if ind <> doc.ElementTable.Items[i].Indent then
        begin
          SetSelectionIndent(RTFEngine.TwipsToPixels(doc.ElementTable.Items[i].Indent));
        end;

        if fn <> doc.ElementTable.Items[i].FontName then
        begin
          SetSelectionFontName(doc.ElementTable.Items[i].FontName);
          fn := doc.ElementTable.Items[i].FontName;
        end;

        if fa <> doc.ElementTable.Items[i].Alignment then
        begin
          SetSelectionAttribute(doc.ElementTable.Items[i].Alignment);
          fa := doc.ElementTable.Items[i].Alignment;
        end;

        if (doc.ElementTable.Items[i].Text <> '') then
        begin
          if (doc.ElementTable.Items[i].Text <> #13) then
          begin
            txtEl := InsertText(doc.ElementTable.Items[i].Text);

            if bulidx > 0 then
              txtEl.Indent := BULLETINDENT;
          end
          else
          begin
            re := InsertLineBreak(false);
            pel := PreviousElement(re);
            if Assigned(pel) then
              re.Alignment := pel.Alignment;
            if bulidx > 0 then
              re.Indent := BULLETINDENT;
          end;
        end;

        //SetCaret(cpEndDoc);
      end;
    end;

    doc.Free;
  finally
    EndUpdate;
    FDefaultChanged := false;
    SelectionType := stDefault;
    Color := bkclr;
    ts.Free;
  end;
{$ENDIF}
end;

function TAdvRichEditorBase.GetRTEContent: string;
begin
  Result := GetContentAsRTE(False);
end;

procedure TAdvRichEditorBase.SetRTEContent(const Value: string);
var
  AStream: TStringStream;
begin
  Clear;
  AStream := TStringStream.Create(Value);
  try
    AStream.Seek(0, TSeekOrigin.soBeginning);
    LoadFromStream(AStream);
  finally
    AStream.Free;
  end;
end;

function TAdvRichEditorBase.GetContentAsRTE(UseSelection: boolean): string;
var
  AStream: TStringStream;
begin
  AStream := TStringStream.Create('');
  try
    if UseSelection then
      SaveSelectionToStream(AStream)
    else
      SaveToStream(AStream);

    Result := AStream.DataString;
  finally
    AStream.Free;
  end;
end;

function TAdvRichEditorBase.ContentAsRTF: string;
begin
  Result := GetContentAsRTF(false);
end;

function TAdvRichEditorBase.SelectionAsRTF: string;
begin
  Result := GetContentAsRTF(true);
end;

function TAdvRichEditorBase.GetContentAsRTF(UseSelection: boolean): string;
var
  i,curindent: integer;
  el,firstel,nel: TREElement;
  isbul,isbold,isitalic,isstrike,isunderline,isaurl: boolean;
  clr,bkclr: TColor;
  s,url,urltext: string;
  curfontname: string;
  curfontsize: integer;
  alignment: TAlignment;
  bul: TBulletElement;

begin
  TidyElements;
  {$IFNDEF LCLLIB}
  FRTFEngine.Clear;
  FRTFEngine.AddFont(GetDefaultFont);
  FRTFEngine.AddForeColor(GetDefaultFontColor);

  firstel := TTextElement.Create;

  if UseSelection then
    GetSelectionElements(stDefault)
  else
  begin
    FSelectionElements.Clear;

    for i := 0 to FContext.Content.Count - 1 do
      FSelectionElements.Add(FContext.Content.Items[i]);
  end;

  i := 0;
  curindent := 0;

  isbold := false;
  isitalic := false;
  isstrike := false;
  isunderline := false;
  isbul := false;
  isaurl := false;
  curfontname := '';
  curfontsize := 0;

  clr := GetDefaultFontColor;
  bkclr := Color;
  s := '';
  url := '';
  alignment := taLeftJustify;

  while (i < FSelectionElements.Count) do
  begin
    el := FSelectionElements[i];

    nel := NextElement(el);

    if (el is TLineBreakElement) and Assigned(nel) and not (nel is TBulletElement) and not (nel is TBulletEnd)  then
    begin
      if isbul then
        FRTFEngine.AddLine
      else
        FRTFEngine.AddNewLine;
    end;

    if url <> el.URL then
    begin
      if isaurl then
        FRTFEngine.AddHyperLink(url, urltext, GetDefaultFont, URLColor);

      url := el.URL;
      isaurl := url <> '';
    end;

    if (curindent <> el.Indent)  then
    begin
      curindent := el.Indent;
      if not (el is TBulletEnd) then
      begin
        FRTFEngine.AddIndent(curindent);
      end;
    end;

    if alignment <> el.Alignment then
    begin
      FRTFEngine.AddHAlignment(el.Alignment);
      alignment := el.Alignment;
    end;

    if (curfontname <> GetFontName(el.Font)) then
    begin
      FRTFEngine.AddFont(el.Font);
      curfontname := GetFontName(el.Font);
    end;

    if (curfontsize <> GetFontSize(el.Font)) then
    begin
      FRTFEngine.AddFontSize(2 * GetFontSize(el.Font));
      curfontsize := GetFontSize(el.Font);
    end;

    if el is TPictureElement then
    begin

      {$IFNDEF FMXLIB}
      //if el.URL <> '' then
      //  FRTFEngine.AddHyperLinkPic(el.URL, (el as TPictureElement).Picture)
      //else
      FRTFEngine.AddPicture((el as TPictureElement).Picture);
      {$ENDIF}
      {$IFDEF FMXLIB}
      FRTFEngine.AddBitmap((el as TPictureElement).Picture, Color);
      {$ENDIF}
    end;

    if el is TTabElement then
    begin
      FRTFEngine.AddTab;
    end;

    if el is TTextElement then
    begin
      if not isbold and (TFontStyle.fsBold in (el as TTextElement).Font.Style) then
      begin
        FRTFEngine.AddBold(true);
        isbold := true;
      end
      else
      if isbold and not (TFontStyle.fsBold in (el as TTextElement).Font.Style) then
      begin
        FRTFEngine.AddBold(false);
        isbold := false;
      end;

      if not isstrike and (TFontStyle.fsStrikeOut in (el as TTextElement).Font.Style) then
      begin
        FRTFEngine.AddStrikeOut(true);
        isstrike := true;
      end
      else
      if isstrike and not (TFontStyle.fsStrikeOut in (el as TTextElement).Font.Style) then
      begin
        FRTFEngine.AddStrikeOut(false);
        isstrike := false;
      end;

      if not isitalic and (TFontStyle.fsItalic in (el as TTextElement).Font.Style) then
      begin
        FRTFEngine.AddItalic(true);
        isitalic := true;
      end
      else
      if isitalic and not (TFontStyle.fsItalic in (el as TTextElement).Font.Style) then
      begin
        FRTFEngine.AddItalic(false);
        isitalic := false;
      end;

      if not isunderline and (TFontStyle.fsUnderline in (el as TTextElement).Font.Style) then
      begin
        FRTFEngine.AddUnderLine(true);
        isunderline := true;
      end
      else
      if isunderline and not (TFontStyle.fsUnderline in (el as TTextElement).Font.Style) then
      begin
        FRTFEngine.AddUnderLine(false);
        isunderline := false;
      end;

      if (clr <> (el as TTextElement).TextColor) then
      begin
        clr := (el as TTextElement).TextColor;
        FRTFEngine.AddForeColor(clr);
      end;

      if (bkclr <> (el as TTextElement).Color) then
      begin
        if ((el as TTextElement).Color = clNone) then
          bkclr := clWhite
        else
          bkclr := (el as TTextElement).Color;

        FRTFEngine.AddBackGroundColor(bkclr);
      end;

      if isaurl then
        urltext := urltext + el.Text
      else
        FRTFEngine.AddText(el.Text);
    end;

    if (el is TBulletEnd) then
    begin
      if isbul then
      begin
        FRTFEngine.EndBullet;
        isbul := false;
      end;
    end;

    if (el is TBulletElement) then
    begin
      if clr <> el.TextColor then
      begin
        clr := GetDefaultFontColor;
        FRTFEngine.AddForeColor(clr);
      end;

      if bkclr <> el.TextColor then
      begin
        bkclr := Color;
        FRTFEngine.AddBackGroundColor(bkclr);
      end;

      bul := (el as TBulletElement);
      if not isbul then
        FRTFEngine.StartBullet(bul.&Type = btNumber, 'Arial',integer(bul.&Type), bul.TextColor)
      else
        FRTFEngine.NextBullet(bul.&Type = btNumber,bul.Index);

      isbul := true;
    end;

    inc(i);
  end;

  firstel.Free;

  Result := FRTFEngine.GetText;
  {$ENDIF}
end;

{------------------------------------------------------------------------------}

{ TStyleElement }

constructor TStyleElement.Create;
begin
  FFont := TFont.Create;
end;

destructor TStyleElement.Destroy;
begin
  FFont.Free;
  inherited;
end;

procedure TStyleElement.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
end;

{------------------------------------------------------------------------------}

{ TPictureElement }

procedure TPictureElement.Assign(Source: TPersistent);
begin
  inherited;

  if (Source is TPictureElement) then
  begin
    FPicture.Assign((Source as TPictureElement).Picture);
    FPictureWidth := (Source as TPictureElement).PictureWidth;
    FPictureHeight := (Source as TPictureElement).PictureHeight;
  end;
end;

constructor TPictureElement.Create;
begin
  inherited;
  FPicture := TGDIPPicture.Create;
end;

destructor TPictureElement.Destroy;
begin
  FPicture.Free;
  inherited;
end;

{$IFDEF FMXLIB}
procedure TPictureElement.DefineProperties(Filer: TFiler);
begin
  if WriteAccess then
    Filer.DefineBinaryProperty('Picture.Data', ReadData, WriteData, not Picture.IsEmpty)
  else
    Filer.DefineBinaryProperty('SaveElement.Picture.Data', ReadData, WriteData, not Picture.IsEmpty)
end;

procedure TPictureElement.ReadData(Stream: TStream);
begin
  Picture.LoadFromStream(Stream);
end;

procedure TPictureElement.WriteData(Stream: TStream);
begin
  Picture.SaveToStream(Stream);
end;
{$ENDIF}

procedure TPictureElement.FitToPage(Width,Height: integer);
var
  f: double;
  AWidth,AHeight: integer;

begin
  AWidth := 0;
  AHeight := 0;

  if (Picture.Width > Width) or (Picture.Height > Height) then
  begin
    if Picture.Width > Width then
    begin
      f := Width / Picture.Width;

      AWidth := Round(0.8 * Width);
      AHeight := Round(f * 0.8 * Picture.Height);
    end
    else
    if AHeight > Height then
    begin
      f := Height / Picture.Height;
      AHeight := Round(0.8 * Height);
      AWidth := Round(f * 0.8 * Picture.Width);
    end;

    PictureWidth := AWidth;
    PictureHeight := AHeight;
  end
  else
  begin
    PictureWidth := 0;
    PictureHeight := 0;
  end;
end;

function TPictureElement.GetPictureHeight: integer;
begin
  Result := 0;

  if FPictureHeight = 0 then
  begin
    if Assigned(FPicture) and not
    {$IFDEF FMXLIB}
    FPicture.IsEmpty then
    {$ENDIF}
    {$IFNDEF FMXLIB}
    FPicture.Empty then
    {$ENDIF}
      Result := FPicture.Height;
  end
  else
    Result := FPictureHeight;
end;

function TPictureElement.GetPictureWidth: integer;
begin
  Result := 0;

  if FPictureWidth = 0 then
  begin
    if Assigned(FPicture) and not
    {$IFDEF FMXLIB}
    FPicture.IsEmpty then
    {$ENDIF}
    {$IFNDEF FMXLIB}
    FPicture.Empty then
    {$ENDIF}
      Result := FPicture.Width;
  end
  else
    Result := FPictureWidth;
end;

function TPictureElement.Size: TSize;
begin
  Result.cx := 0;
  Result.cy := 0;

  if (PictureWidth > 0) and (PictureHeight > 0) then
  begin
    Result.cy := PictureHeight;
    Result.cx := PictureWidth + PICTURE_MARGIN;
  end
  else
  begin
    if Assigned(FPicture) and not
    {$IFDEF FMXLIB}
    FPicture.IsEmpty then
    {$ENDIF}
    {$IFNDEF FMXLIB}
    FPicture.Empty then
    {$ENDIF}
    begin
      Result.cx := FPicture.Width + PICTURE_MARGIN;
      Result.cy := FPicture.Height;
    end;
  end;
end;


{------------------------------------------------------------------------------}

{ TLineElement }

procedure TLineElement.Assign(Source: TPersistent);
begin
  inherited;

  if (Source is TLineElement) then
  begin
    FColor := (Source as TLineElement).Color;
    FWidth := (Source as TLineElement).Width;
    FStyle := (Source as TLineElement).Style;
  end;
end;

constructor TLineElement.Create;
begin
  inherited;
  FColor := clBlack;
  FWidth := 1;
  FStyle := psSolid;
end;

{------------------------------------------------------------------------------}

{ TGraphicElement }

procedure TGraphicElement.Assign(Source: TPersistent);
begin
  inherited;

  if (Source is TGraphicElement) then
  begin
    FWidth := (Source as TGraphicElement).Width;
    FHeight := (Source as TGraphicElement).Height;
    FID := (Source as TGraphicElement).ID;
  end;
end;

function TGraphicElement.Size: TSize;
begin
  Result.cx := Width;
  Result.cy := Height;
end;

{------------------------------------------------------------------------------}

{ TBulletElement }

procedure TBulletElement.Assign(Source: TPersistent);
begin
  inherited;
  if (Source is TBulletElement) then
  begin
    FType := (Source as TBulletElement).&Type;
    FBullet := (Source as TBulletElement).Bullet;
    FBulletFont := (Source as TBulletElement).BulletFont;
    FIndex := (Source as TBulletElement).Index;
    FBulletFormat := (Source as TBulletElement).BulletFormat;
    FSpacing := (Source as TBulletElement).Spacing;
  end;
end;

constructor TBulletElement.Create;
begin
  inherited Create;
  FBulletFormat := '%d.';
  FWidth := 12;
  FSpacing := BULLETSPACING;
end;

function TBulletElement.Size: TSize;
begin
  Result.cx := FWidth;
  Result.cy := 0;
end;

{------------------------------------------------------------------------------}

{ TLineBreakElement }

constructor TLineBreakElement.Create;
begin
  inherited;
end;

{------------------------------------------------------------------------------}

{ TBulletStart }

procedure TBulletStart.Assign(Source: TPersistent);
begin
  inherited;
  if (Source is TBulletStart) then
  begin
    FStartIndent := (Source as TBulletStart).StartIndent;
  end;
end;

{------------------------------------------------------------------------------}

{ TContext }

procedure TContext.Clear;
var
  i: integer;
begin
  for i := 0 to Content.Count - 1 do
  begin
    {$IFDEF DELPHI_LLVM}
    Content.Items[i].DisposeOf;
    {$ELSE}
    Content.Items[i].Free;
    {$ENDIF}
  end;
  Content.Clear;
end;

constructor TContext.Create(ARect: TRect);
begin
  FContent := TElements.Create;
  FCaret := TCaret.Create;
  FCaret.Element := nil;
  FCaret.CharIndex := -1;
  FSelection := TSelection.Create;
  FSelection.FromElement := nil;
  FSelection.ToElement := nil;
end;

destructor TContext.Destroy;
begin
  Clear;
  FCaret.Free;
  FContent.Free;
  FSelection.Free;
  inherited;
end;


function TContext.Equal(AContext: TContext): boolean;
var
  i: integer;
begin
  Result := true;
  for i := 0 to AContext.Content.Count - 1 do
  begin
    if i < Content.Count then
    begin
      if not (AContext.Content.Items[i].Equal(Content.Items[i])) then
      begin
        Result := false;
        break;
      end;
    end
    else
    begin
      Result := false;
      break;
    end;
  end;
end;

{------------------------------------------------------------------------------}

{ TCaret }

procedure TCaret.Assign(Source: TPersistent);
begin
  if (Source is TCaret) then
  begin
    FColor := (Source as TCaret).Color;
    FEOL := (Source as TCaret).EOL;
    FXY := (Source as TCaret).XY;
    FLH := (Source as TCaret).LH;
    FElement := (Source as TCaret).Element;
    FCharIndex := (Source as TCaret).CharIndex;
  end;
end;

procedure TCaret.Changed;
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

procedure TCaret.SetCharIndex(const Value: integer);
begin
  if (FCharIndex <> Value) then
  begin
    FCharIndex := Value;
    Changed;
  end;
end;

procedure TCaret.SetXY(const Value: TPoint);
begin
  FXY := Value;
end;


{------------------------------------------------------------------------------}

{ TSelection }

procedure TSelection.Assign(Source: TPersistent);
begin
  if (Source is TSelection) then
  begin
    FFromElement := (Source as TSelection).FromElement;
    FToElement := (Source as TSelection).ToElement;
    FFromChar := (Source as TSelection).FromChar;
    FToChar := (Source as TSelection).ToChar;
  end;
end;

procedure TSelection.Changed;
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

constructor TSelection.Create;
begin
  inherited;
  FromElement := nil;
  ToElement := nil;
end;

procedure TSelection.SetFromChar(const Value: integer);
begin
  if (FFromChar <> Value) then
  begin
    FFromChar := Value;
    Changed;
  end;
end;

procedure TSelection.SetFromElement(const Value: TREElement);
begin
  if (FFromElement <> Value) then
  begin

    FFromElement := Value;
    Changed;
  end;
end;

procedure TSelection.SetToChar(const Value: integer);
begin
  if (FToChar <> Value) then
  begin
    FToChar := Value;
    Changed;
  end;
end;

procedure TSelection.SetToElement(const Value: TREElement);
begin
  if (FToElement <> Value) then
  begin
    FToElement := Value;
    Changed;
  end;
end;

{------------------------------------------------------------------------------}

{ TPageMargin }

procedure TPageMargin.Assign(Source: TPersistent);
begin
  if (Source is TPageMargin) then
  begin
    FColor := (Source as TPageMargin).Color;
    FHorizontal := (Source as TPageMargin).Horizontal;
    FVertical := (Source as TPageMargin).Vertical;
  end;
end;

procedure TPageMargin.Changed;
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

constructor TPageMargin.Create;
begin
  inherited;
  FColor := clGray;
end;

procedure TPageMargin.SetColor(const Value: TColor);
begin
  if (FColor <> Value) then
  begin
    FColor := Value;
    Changed;
  end;
end;

procedure TPageMargin.SetHorizontal(const Value: integer);
begin
  if (FHorizontal <> Value) then
  begin
    FHorizontal := Value;
    Changed;
  end;
end;

procedure TPageMargin.SetVertical(const Value: integer);
begin
  if (FVertical <> Value) then
  begin
    FVertical := Value;
    Changed;
  end;
end;

{------------------------------------------------------------------------------}
{  ACTIONS                                                                     }
{------------------------------------------------------------------------------}

{ TAdvRichEditorAction }

destructor TAdvRichEditorAction.Destroy;
begin
  if Assigned(FControl) then
    FControl.RemoveFreeNotification(Self);

  inherited;
end;

function TAdvRichEditorAction.GetControl(Target: TObject): TAdvRichEditorBase;
begin
  Result := Target as TAdvRichEditorBase;
end;

function TAdvRichEditorAction.HandlesTarget(Target: TObject): Boolean;
begin
  Result := ((Control <> nil) and (Target = Control) or
    (Control = nil) and (Target is TAdvRichEditorBase)) and TAdvRichEditorBase(Target).GetFocused;
end;

procedure TAdvRichEditorAction.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = Control) then
    Control := nil;
end;

procedure TAdvRichEditorAction.SetControl(Value: TAdvRichEditorBase);
begin
  if Value <> FControl then
  begin
    FControl := Value;
    if Value <> nil then Value.FreeNotification(Self);
  end;
end;

procedure TAdvRichEditorAction.UpdateTarget(Target: TObject);
begin
  inherited;
end;

{ TAdvRichEditorClear }

procedure TAdvRichEditorClear.ExecuteTarget(Target: TObject);
begin
  inherited;
  GetControl(Target).Clear;
end;

procedure TAdvRichEditorClear.UpdateTarget(Target: TObject);
begin
  inherited;
  Enabled := not (GetControl(Target).ReadOnly);
end;

{ TAdvRichEditorCut }

procedure TAdvRichEditorCut.ExecuteTarget(Target: TObject);
begin
  inherited;
  GetControl(Target).CutToClipboard;
end;

procedure TAdvRichEditorCut.UpdateTarget(Target: TObject);
begin
  inherited;
  Enabled := (GetControl(Target).HasSelection or Assigned(GetControl(Target).Selected)) and not (GetControl(Target).ReadOnly);
end;

{ TAdvRichEditorCopy }

procedure TAdvRichEditorCopy.ExecuteTarget(Target: TObject);
begin
  inherited;
  with GetControl(Target) do
    CopyToClipboard(SelectedText);
end;

procedure TAdvRichEditorCopy.UpdateTarget(Target: TObject);
begin
  inherited;
  Enabled := GetControl(Target).HasSelection or Assigned(GetControl(Target).Selected);
end;

{ TAdvRichEditorPaste }

procedure TAdvRichEditorPaste.ExecuteTarget(Target: TObject);
begin
  inherited;
  with GetControl(Target) do
    PasteFromClipboard;
end;

procedure TAdvRichEditorPaste.UpdateTarget(Target: TObject);
begin
  inherited;
  Enabled := GetControl(Target).ClipboardHasContent and not (GetControl(Target).ReadOnly);
end;

{ TAdvRichEditorSelectAll }

procedure TAdvRichEditorSelectAll.ExecuteTarget(Target: TObject);
begin
  inherited;
  GetControl(Target).SelectAll;
end;

procedure TAdvRichEditorSelectAll.UpdateTarget(Target: TObject);
begin
  inherited;
  Enabled := (Target is TAdvRichEditorBase) and TAdvRichEditorBase(Target).Focused;
end;

{ TAdvRichEditorAlignCenter }

procedure TAdvRichEditorAlignCenter.ExecuteTarget(Target: TObject);
begin
  inherited;
  GetControl(Target).SetSelectionAttribute(taCenter);
end;

procedure TAdvRichEditorAlignCenter.UpdateTarget(Target: TObject);
begin
  inherited;
  Enabled := (Target is TAdvRichEditorBase) and TAdvRichEditorBase(Target).Focused;
  Checked := GetControl(Target).IsSelectionCenter;
end;

{ TAdvRichEditorAlignLeft }

procedure TAdvRichEditorAlignLeft.ExecuteTarget(Target: TObject);
begin
  inherited;
  GetControl(Target).SetSelectionAttribute(taLeftJustify);
end;

procedure TAdvRichEditorAlignLeft.UpdateTarget(Target: TObject);
begin
  inherited;
  Enabled := (Target is TAdvRichEditorBase) and TAdvRichEditorBase(Target).Focused;
  Checked := GetControl(Target).IsSelectionLeft;
end;

{ TAdvRichEditorAlignRight }

procedure TAdvRichEditorAlignRight.ExecuteTarget(Target: TObject);
begin
  inherited;
  GetControl(Target).SetSelectionAttribute(taRightJustify);
end;

procedure TAdvRichEditorAlignRight.UpdateTarget(Target: TObject);
begin
  inherited;
  Enabled := (Target is TAdvRichEditorBase) and TAdvRichEditorBase(Target).Focused;
  Checked := GetControl(Target).IsSelectionRight;
end;

{ TAdvRichEditorBold }

procedure TAdvRichEditorBold.ExecuteTarget(Target: TObject);
begin
  inherited;
  with GetControl(Target) do
  begin
    SetSelectionBold(not IsSelectionBold);
  end;
end;

procedure TAdvRichEditorBold.UpdateTarget(Target: TObject);
begin
  inherited;
  Enabled := (Target is TAdvRichEditorBase) and TAdvRichEditorBase(Target).Focused;
  Checked := GetControl(Target).IsSelectionBold;
end;

{ TAdvRichEditorStrikeOut }

procedure TAdvRichEditorStrikeOut.ExecuteTarget(Target: TObject);
begin
  inherited;
  with GetControl(Target) do
  begin
    SetSelectionStrikeOut(not IsSelectionStrikeOut);
  end;
end;

procedure TAdvRichEditorStrikeOut.UpdateTarget(Target: TObject);
begin
  inherited;
  Enabled := (Target is TAdvRichEditorBase) and TAdvRichEditorBase(Target).Focused;
  Checked := GetControl(Target).IsSelectionStrikeOut;
end;

{ TAdvRichEditorItalic }

procedure TAdvRichEditorItalic.ExecuteTarget(Target: TObject);
begin
  inherited;
  with GetControl(Target) do
    SetSelectionItalic(not IsSelectionItalic);
end;

procedure TAdvRichEditorItalic.UpdateTarget(Target: TObject);
begin
  inherited;
  Enabled := (Target is TAdvRichEditorBase) and TAdvRichEditorBase(Target).Focused;
  Checked := GetControl(Target).IsSelectionItalic;
end;

{ TAdvRichEditorUnderline }

procedure TAdvRichEditorUnderline.ExecuteTarget(Target: TObject);
begin
  inherited;
  with GetControl(Target) do
    SetSelectionUnderline(not IsSelectionUnderline);
end;

procedure TAdvRichEditorUnderline.UpdateTarget(Target: TObject);
begin
  inherited;
  Enabled := (Target is TAdvRichEditorBase) and TAdvRichEditorBase(Target).Focused;
  Checked := GetControl(Target).IsSelectionUnderline;
end;

{ TAdvRichEditorUndo }

procedure TAdvRichEditorUndo.ExecuteTarget(Target: TObject);
begin
  inherited;
  with GetControl(Target) do
    Undo;
end;

procedure TAdvRichEditorUndo.UpdateTarget(Target: TObject);
begin
  inherited;
  Enabled := GetControl(Target).CanUndo;
end;

{ TAdvRichEditorRedo }

procedure TAdvRichEditorRedo.ExecuteTarget(Target: TObject);
begin
  inherited;
  with GetControl(Target) do
    Redo;
end;

procedure TAdvRichEditorRedo.UpdateTarget(Target: TObject);
begin
  inherited;
  Enabled := GetControl(Target).CanRedo;
end;

{ TAdvRichEditorIndent }

procedure TAdvRichEditorIndent.ExecuteTarget(Target: TObject);
begin
  inherited;
  with GetControl(Target) do
    SetSelectionIndent(100);
end;

procedure TAdvRichEditorIndent.UpdateTarget(Target: TObject);
begin
  inherited;
  Enabled := (Target is TAdvRichEditorBase) and TAdvRichEditorBase(Target).Focused;
end;

{ TAdvRichEditorUnIndent }

procedure TAdvRichEditorUnIndent.ExecuteTarget(Target: TObject);
begin
  inherited;
  with GetControl(Target) do
    SetSelectionIndent(-100);
end;

procedure TAdvRichEditorUnIndent.UpdateTarget(Target: TObject);
begin
  inherited;
  Enabled := GetControl(Target).CanUnindent;
end;

{ TAdvRichEditorTextColor }

procedure TAdvRichEditorTextColor.ExecuteTarget(Target: TObject);
{$IFNDEF LCLLIB}
var
  AContext: TRttiContext;
  rt: TRttiType;
  prop: TRttiProperty;
{$ENDIF}
begin
  inherited;
{$IFNDEF LCLLIB}
  with GetControl(Target) do
  begin
    AContext := TRttiContext.Create;
    try
      rt := AContext.GetType(self.ActionComponent.ClassType);
      prop := rt.GetProperty('SelectedColor');
      if Assigned(Prop) then
      begin
        SetSelectionColor(TColor(prop.GetValue(ActionComponent).AsInteger));
      end;
    finally
      AContext.Free;
    end;
  end;
{$ENDIF}
end;

procedure TAdvRichEditorTextColor.UpdateTarget(Target: TObject);
{$IFNDEF FMXLIB}
var
  I: integer;
  clr: TColor;
{$ENDIF}
begin
  inherited;
  Enabled := (Target is TAdvRichEditorBase) and TAdvRichEditorBase(Target).Focused;

{$IFNDEF LCLLIB}
  {$IFNDEF FMXLIB}
  clr := GetControl(Target).GetSelectionTextColor;

  {$IFDEF DELPHIXE3_LVL}
  for I := 0 to ClientCount - 1 do
    if TBasicActionLink(Clients[I]) is TAdvColorSelectorActionLink then
        TAdvColorSelectorActionLink(Clients[I]).UpdateColor(clr);
  {$ENDIF}
  {$IFNDEF DELPHIXE3_LVL}
  for I := 0 to FClients.Count - 1 do
    if TBasicActionLink(FClients[I]) is TAdvColorSelectorActionLink then
        TAdvColorSelectorActionLink(FClients[I]).UpdateColor(clr);
  {$ENDIF}

  {$ENDIF}
{$ENDIF}
end;

{ TAdvRichEditorColor }

procedure TAdvRichEditorColor.ExecuteTarget(Target: TObject);
{$IFNDEF LCLLIB}
var
  AContext: TRttiContext;
  rt: TRttiType;
  prop: TRttiProperty;
{$ENDIF}
begin
  inherited;
{$IFNDEF LCLLIB}
  with GetControl(Target) do
  begin
    AContext := TRttiContext.Create;
    try
      rt := AContext.GetType(ActionComponent.ClassType);
      prop := rt.GetProperty('SelectedColor');
      if Assigned(Prop) then
      begin
        SetSelectionBkColor(TColor(prop.GetValue(ActionComponent).AsInteger));
      end;
    finally
      AContext.Free;
    end;
  end;
{$ENDIF}
end;

procedure TAdvRichEditorColor.UpdateTarget(Target: TObject);
{$IFNDEF FMXLIB}
var
  I: integer;
  clr: TColor;
{$ENDIF}
begin
  inherited;
  Enabled := (Target is TAdvRichEditorBase) and TAdvRichEditorBase(Target).Focused;

{$IFNDEF LCLLIB}

  {$IFNDEF FMXLIB}
  clr := GetControl(Target).GetSelectionBkColor;

  {$IFDEF DELPHIXE3_LVL}
  for I := 0 to ClientCount - 1 do
    if TBasicActionLink(Clients[I]) is TAdvColorSelectorActionLink then
        TAdvColorSelectorActionLink(Clients[I]).UpdateColor(clr);
  {$ENDIF}

  {$IFNDEF DELPHIXE3_LVL}
  for I := 0 to FClients.Count - 1 do
    if TBasicActionLink(FClients[I]) is TAdvColorSelectorActionLink then
        TAdvColorSelectorActionLink(FClients[I]).UpdateColor(clr);
  {$ENDIF}

  {$ENDIF}
{$ENDIF}
end;

{ TAdvRichEditorFontSize }

procedure TAdvRichEditorFontSize.ExecuteTarget(Target: TObject);
begin
  inherited;
end;

function TAdvRichEditorFontSize.HandlesTarget(Target: TObject): Boolean;
begin
  Result := ((Control <> nil) and (Target = Control) or
    (Control = nil) and (Target is TAdvRichEditorBase));
end;

procedure TAdvRichEditorFontSize.UpdateTarget(Target: TObject);
{$IFNDEF FMXLIB}
var
  i: integer;
  fname: string;
{$ENDIF}
begin
  Enabled := True; // (Target is TAdvRichEditorBase) and TAdvRichEditorBase(Target).Focused;

{$IFNDEF LCLLIB}
  {$IFNDEF FMXLIB}
  fname := inttostr(GetControl(Target).GetSelectionFontSize);

  {$IFDEF DELPHIXE3_LVL}
  for I := 0 to ClientCount - 1 do
    if TBasicActionLink(Clients[I]) is TAdvOfficeComboBoxActionLink then
        TAdvOfficeComboBoxActionLink(Clients[I]).UpdateText(fname);
  {$ENDIF}

  {$IFNDEF DELPHIXE3_LVL}
  for I := 0 to FClients.Count - 1 do
    if TBasicActionLink(FClients[I]) is TAdvOfficeComboBoxActionLink then
        TAdvOfficeComboBoxActionLink(FClients[I]).UpdateText(fname);
  {$ENDIF}
  {$ENDIF}
{$ENDIF}
end;

{ TAdvRichEditorFontName }

procedure TAdvRichEditorFontName.ExecuteTarget(Target: TObject);
begin
  inherited;
end;

function TAdvRichEditorFontName.HandlesTarget(Target: TObject): Boolean;
begin
  Result := ((Control <> nil) and (Target = Control) or
    (Control = nil) and (Target is TAdvRichEditorBase));
end;

procedure TAdvRichEditorFontName.UpdateTarget(Target: TObject);
{$IFNDEF FMXLIB}
var
  i: integer;
  fname: string;
{$ENDIF}
begin
  Enabled := True; //(Target is TAdvRichEditorBase) and TAdvRichEditorBase(Target).Focused;
{$IFNDEF LCLLIB}
  {$IFNDEF FMXLIB}
  fname := GetControl(Target).GetSelectionFontName;

  {$IFDEF DELPHIXE3_LVL}
  for I := 0 to ClientCount - 1 do
    if TBasicActionLink(Clients[I]) is TAdvOfficeComboBoxActionLink then
        TAdvOfficeComboBoxActionLink(Clients[I]).UpdateText(fname);
  {$ENDIF}

  {$IFNDEF DELPHIXE3_LVL}
  for I := 0 to FClients.Count - 1 do
    if TBasicActionLink(FClients[I]) is TAdvOfficeComboBoxActionLink then
        TAdvOfficeComboBoxActionLink(FClients[I]).UpdateText(fname);
  {$ENDIF}
  {$ENDIF}
{$ENDIF}
end;

{ TAdvRichEditorBulletType }

procedure TAdvRichEditorBulletType.ExecuteTarget(Target: TObject);
begin
  inherited;
  with GetControl(Target) do
  begin
    SetSelectionBullets(btCircle);
  end;
end;

function TAdvRichEditorBulletType.HandlesTarget(Target: TObject): Boolean;
begin
  Result := ((Control <> nil) and (Target = Control) or
    (Control = nil) and (Target is TAdvRichEditorBase));
end;

procedure TAdvRichEditorBulletType.UpdateTarget(Target: TObject);
var
{$IFNDEF FMXLIB}
  i: integer;
{$ENDIF}
  BulletType: TBulletType;
begin
  Enabled := (Target is TAdvRichEditorBase) and TAdvRichEditorBase(Target).Focused;

{$IFNDEF LCLLIB}

  BulletType := GetControl(Target).GetSelectionBullet;

  Checked := (BulletType <> btNone) and (BulletType <> btCustom) and (BulletType <> btNumber);

  if (BulletType <> btNone) and (BulletType <> btCustom) then
  begin
    {$IFNDEF FMXLIB}

    {$IFDEF DELPHIXE3_LVL}
    for I := 0 to ClientCount - 1 do
      if TBasicActionLink(Clients[I]) is TAdvToolSelectorActionLink then
          TAdvToolSelectorActionLink(Clients[I]).UpdateSelectedIndex(integer(BulletType));
    {$ENDIF}

    {$IFNDEF DELPHIXE3_LVL}
    for I := 0 to FClients.Count - 1 do
      if TBasicActionLink(FClients[I]) is TAdvToolSelectorActionLink then
          TAdvToolSelectorActionLink(FClients[I]).UpdateSelectedIndex(integer(BulletType));
    {$ENDIF}
    {$ENDIF}
  end;
{$ENDIF}
end;

{ TAdvRichEditorSubscript }

procedure TAdvRichEditorSubscript.ExecuteTarget(Target: TObject);
begin
  inherited;
  with GetControl(Target) do
  begin
    SetSelectionSubscript(not IsSelectionSubscript);
  end;
end;

procedure TAdvRichEditorSubscript.UpdateTarget(Target: TObject);
begin
  inherited;
  Enabled := (Target is TAdvRichEditorBase) and TAdvRichEditorBase(Target).Focused;
  Checked := GetControl(Target).IsSelectionSubscript;
end;

{ TAdvRichEditorSuperscript }

procedure TAdvRichEditorSuperscript.ExecuteTarget(Target: TObject);
begin
  inherited;
  with GetControl(Target) do
  begin
    SetSelectionSuperscript(not IsSelectionSuperscript);
  end;
end;

procedure TAdvRichEditorSuperscript.UpdateTarget(Target: TObject);
begin
  inherited;
  Enabled := (Target is TAdvRichEditorBase) and TAdvRichEditorBase(Target).Focused;
  Checked := GetControl(Target).IsSelectionSuperscript;
end;

{ TAdvRichEditorNumberedBulletType }

procedure TAdvRichEditorNumberedBulletType.ExecuteTarget(Target: TObject);
begin
  inherited;
  with GetControl(Target) do
  begin
    SetSelectionBullets(btNumber);
  end;
end;

function TAdvRichEditorNumberedBulletType.HandlesTarget(
  Target: TObject): Boolean;
begin
  Result := ((Control <> nil) and (Target = Control) or
    (Control = nil) and (Target is TAdvRichEditorBase));
end;

procedure TAdvRichEditorNumberedBulletType.UpdateTarget(Target: TObject);
var
  BulletType: TBulletType;
begin
  inherited;
  Enabled := (Target is TAdvRichEditorBase) and TAdvRichEditorBase(Target).Focused;

  BulletType := GetControl(Target).GetSelectionBullet;
  Checked := (BulletType = btNumber);
end;

{------------------------------------------------------------------------------}

{ TMemoState }

constructor TMemoState.Create;
begin
  inherited;
  FDefaultFont := TFont.Create;
end;

destructor TMemoState.Destroy;
begin
  FDefaultFont.Free;
  inherited;
end;

function TMemoState.GetColor: cardinal;
begin
  {$IFNDEF FMXLIB}
  Result := Cardinal(ColorToRGB(ToRGB(FColor)));
  {$ENDIF}
  {$IFDEF FMXLIB}
  Result := Cardinal(FColor);
  {$ENDIF}
end;

function TMemoState.GetFontName: string;
begin
  Result := TAdvRichEditorBase.GetFontName(FDefaultFont);
end;

function TMemoState.GetFontSize: integer;
begin
  Result := TAdvRichEditorBase.GetFontSize(FDefaultFont);
end;

function TMemoState.GetFontStyle: TFontStyles;
begin
  Result := FDefaultFont.Style;
end;

function TMemoState.GetTextColor: cardinal;
begin
  {$IFNDEF FMXLIB}
  Result := Cardinal(ColorToRGB(ToRGB(FTextColor)));
  {$ENDIF}
  {$IFDEF FMXLIB}
  Result := Cardinal(FTextColor);
  {$ENDIF}
end;

procedure TMemoState.SetColor(const Value: cardinal);
begin
  {$IFNDEF FMXLIB}
  if Value = 0 then
    FColor := clNone
  else
    FColor := TColor(ToRGB(Value));
  {$ENDIF}
  {$IFDEF FMXLIB}
  if Value <> 0 then
    FColor := TColor(Value OR $FF000000)
  else
    FColor := TColor(Value);
  {$ENDIF}
end;

procedure TMemoState.SetDefaultFont(const Value: TFont);
begin
  FDefaultFont.Assign(Value);
end;

procedure TMemoState.SetFontName(const Value: string);
begin
  TAdvRichEditorBase.SetFontName(FDefaultFont, Value);
end;

procedure TMemoState.SetFontSize(const Value: integer);
begin
  TAdvRichEditorBase.SetFontSize(FDefaultFont, Value);
end;

procedure TMemoState.SetFontStyle(const Value: TFontStyles);
begin
  FDefaultFont.Style := Value;
end;

procedure TMemoState.SetTextColor(const Value: cardinal);
begin
  {$IFNDEF FMXLIB}
  if Value = 0 then
    FTextColor := clNone
  else
    FTextColor := TColor(ToRGB(Value));
  {$ENDIF}
  {$IFDEF FMXLIB}
  if Value <> 0 then
    FTextColor := TColor(Value OR $FF000000)
  else
    FTextColor := TColor(Value);
  {$ENDIF}
end;

{ TRichEditorAutoReplace }

procedure TRichEditorAutoReplace.Add(const AOldValue, ANewValue: string);
begin
  FOldValue.Add(AOldValue);
  FNewValue.Add(ANewValue);
end;

procedure TRichEditorAutoReplace.Assign(Source: TPersistent);
begin
  if (Source is TRichEditorAutoReplace) then
  begin
    Active  := (Source as TRichEditorAutoCorrect).Active;
    CaseSensitive := (Source as TRichEditorAutoCorrect).CaseSensitive;
    OldValue.Assign((Source as TRichEditorAutoCorrect).OldValue);
    NewValue.Assign((Source as TRichEditorAutoCorrect).NewValue);
  end;
end;

constructor TRichEditorAutoReplace.Create;
begin
  inherited Create;
  FOldValue := TStringList.Create;
  FNewValue := TStringList.Create;
  FDoAutoCorrect := True;
  FCaseSensitive := True;
end;

destructor TRichEditorAutoReplace.Destroy;
begin
  FOldValue.Free;
  FNewValue.Free;
  inherited;
end;

procedure TRichEditorAutoReplace.SetDoAutoCorrect(const Value: boolean);
begin
  FDoAutoCorrect := Value;
end;

procedure TRichEditorAutoReplace.SetNewValue(const Value: TStringList);
begin
  FNewValue.Assign(Value);
end;

procedure TRichEditorAutoReplace.SetOldValue(const Value: TStringList);
begin
  FOldValue.Assign(Value);
end;


{ TNamedPictureElement }

procedure TNamedPictureElement.Assign(Source: TPersistent);
begin
  if (Source is TNamedPictureElement) then
  begin
    FWidth := (Source as TNamedPictureElement).Width;
    FHeight := (Source as TNamedPictureElement).Height;
    FName := (Source as TNamedPictureElement).Name;
  end;
end;

function TNamedPictureElement.Size: TSize;
begin
  Result.cx := Width;
  Result.cy := Height;
end;

{ TTabElement }

function TTabElement.Size: TSize;
begin
  Result.cx := FWidth;
  Result.cy := TH;
end;

end.
