{*************************************************************************}
{ TAdvStringGrid objects support file                                     }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{            copyright © 1996-2015                                        }
{            Email : info@tmssoftware.com                                 }
{            Web : http://www.tmssoftware.com                             }
{                                                                         }
{ The source code is given as is. The author is not responsible           }
{ for any possible damage done due to the use of this code.               }
{ The component can be freely used in any application. The complete       }
{ source code remains property of the author and may not be distributed,  }
{ published, given or sold in any form as such. No parts of the source    }
{ code can be included in any other component or application without      }
{ written authorization of the author.                                    }
{*************************************************************************}

unit AdvObj;

{$I TMSDEFS.INC}

{$IFNDEF DELPHI_UNICODE}
  {$DEFINE TMSUNICODE}
{$ENDIF}

interface

uses
  Windows, Controls, Graphics, ExtCtrls, Dialogs, Classes, Messages,
  SysUtils, Buttons, Menus, ImgList, StdCtrls, Grids
  {$IFDEF TMSGDIPLUS}
  , AdvHintInfo
  {$ENDIF}
  , AdvXPVS
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes, System.Types
  {$ENDIF}
  ;

type
  TObjStringGrid = class(TStringGrid);

  TEditorType = (edNormal,edSpinEdit,edComboEdit,edComboList,edEditBtn,edCheckBox,
    edDateEdit,edDateEditUpDown,edTimeEdit,edButton,edDataCheckBox,edNumeric,
    edPositiveNumeric,edFloat,edCapital,edMixedCase,edPassword,edUnitEditBtn,
    edLowerCase,edUpperCase,edFloatSpinEdit,edTimeSpinEdit,edDateSpinEdit,
    edNumericEditBtn,edFloatEditBtn,edCustom,edRichEdit,edNone
    {$IFDEF TMSUNICODE}
    , edUniEdit,edUniComboEdit,edUniComboList,edUniEditBtn, edUniMemo
    {$ENDIF}
    , edDateTimeEdit, edValidChars
    , edTrackbarDropDown, edMemoDropDown, edCalculatorDropDown
    , edTimePickerDropDown, edDetailDropDown, edGridDropDown, edColorPickerDropDown
    , edImagePickerDropDown, edAdvGridDropDown, edControlDropDown
    , edPositiveFloat, edNumericUnitEditBtn, edFloatUnitEditBtn, edMaskEditBtn
    );

{$IFNDEF TMSUNICODE}
const
    edUniEdit = edNormal;
    edUniComboList = edComboList;
    edUniComboEdit = edComboEdit;
    edUniEditBtn = edEditBtn;
{$ENDIF}

type

  TCellBorder = (cbTop,cbLeft,cbRight,cbBottom);

  TCellBorders = set of TCellBorder;

  TCellGradientDirection = (GradientVertical, GradientHorizontal);

  TGetDisplTextEvent = procedure(Sender: TObject; ACol,ARow: Integer; var Value: string) of object;

  TFloatFormatEvent = procedure(Sender: TObject; ACol,ARow: Integer;var IsFloat: Boolean;
    var FloatFormat: string) of object;

  TOnResizeEvent = procedure (Sender:TObject) of object;

  TColumnSizeEvent = procedure (Sender:TObject; ACol: Integer; var Allow: Boolean) of object;

  TColumnSizingEvent = procedure (Sender:TObject; ACol, ColumnSize: Integer) of object;

  TRowSizingEvent = procedure (Sender:TObject; ARow, RowSize: Integer) of object;

  TRowSizeEvent = procedure (Sender:TObject; ARow: Integer; var Allow: Boolean) of object;

  TEndColumnSizeEvent = procedure (Sender:TObject; ACol: Integer) of object;

  TUpdateColumnSizeEvent = procedure (Sender:TObject; ACol: Integer; var AWidth: Integer) of object;

  TEndRowSizeEvent = procedure (Sender:TObject; ARow: Integer) of object;

  TClickCellEvent = procedure (Sender:TObject;ARow,ACol: Integer) of object;

  TDblClickCellEvent = procedure (Sender: TObject;ARow,ACol: Integer) of object;

  TCanEditCellEvent = procedure (Sender: TObject;ARow,ACol: Integer;var CanEdit: Boolean) of object;

  TScrollHintEvent = procedure (Sender: TObject; ARow: Integer;var hintstr:string) of object;

  TButtonClickEvent = procedure(Sender: TObject;ACol,ARow: Integer) of object;

  TExpandClickEvent = procedure(Sender: TObject;ACol,ARow: Integer; Expanded: boolean) of object;

  TCheckBoxClickEvent = procedure(Sender: TObject;ACol,ARow: Integer; State: Boolean) of object;

  TCheckBoxCanToggleEvent = procedure(Sender: TObject; ACol,ARow: Integer; State: Boolean; var Allow: boolean) of object;

  TGetEditorTypeEvent = procedure(Sender:TObject;ACol,ARow: Integer;
    var AEditor:TEditorType) of object;

  TSortStyle = (ssAutomatic, ssAlphabetic, ssNumeric, ssDate, ssAlphaNoCase,
    ssAlphaCase, ssShortDateEU, ssShortDateUS, ssCustom, ssFinancial, ssAnsiAlphaCase,
    ssAnsiAlphaNoCase, ssRaw, ssHTML, ssImages, ssCheckBox
    {$IFDEF TMSUNICODE}
    , ssUnicode
    {$ENDIF}
    {$IFDEF DELPHI7_LVL}
    , ssDateTime, ssTime
    {$ENDIF}
    , ssAlphaNumeric, ssAlphaNumericNoCase, ssNatural
    );

{$IFNDEF TMSUNICODE}
const
   ssUnicode = ssAlphabetic;
{$ENDIF}

type

  TVAlignment = (vtaTop,vtaCenter,vtaBottom);

  TCustomCellDrawEvent = procedure(Sender: TObject; Canvas: TCanvas; ACol,ARow: Integer;
    AState: TGridDrawState; ARect: TRect; Printing: Boolean) of object;

  TAsgVAlignment = TVAlignment;

  TAnchorClickEvent = procedure(Sender:TObject;ARow,ACol: Integer; Anchor:string; var AutoHandle: Boolean) of object;

  TAnchorHintEvent = procedure(Sender:TObject;ARow,ACol: Integer;var Anchor:string) of object;

  TAnchorEvent = procedure(Sender:TObject;ARow,ACol: Integer; Anchor:string) of object;

  TClickSortEvent = procedure(Sender:TObject; ACol: Integer) of object;

  TCanSortEvent = procedure(Sender:TObject; ACol: Integer; var DoSort: Boolean) of object;

  TCustomCompareEvent = procedure(Sender:TObject; str1,str2:string;
    var Res: Integer) of object;

  TRawCompareEvent = procedure(Sender:TObject; ACol,Row1,Row2: Integer;
    var Res: Integer) of object;

  TGridFormatEvent = procedure(Sender : TObject; ACol: Integer;
    var AStyle: TSortStyle; var aPrefix,aSuffix:string) of object;

  TGridColorEvent = procedure(Sender: TObject; ARow, ACol: Integer;
    AState: TGridDrawState; ABrush: TBrush; AFont: TFont ) of object;

  TGridGradientEvent = procedure(Sender: TObject; ARow, ACol: Integer;
    var Color, ColorTo, ColorMirror, ColorMirrorTo: TColor; var GD: TCellGradientDirection) of object;

  TGridBorderEvent = procedure (Sender: TObject; ARow, ACol: Integer; APen: TPen;
    var Borders: TCellBorders) of object;

  TGridBorderPropEvent = procedure (Sender: TObject; ARow, ACol: Integer;
    LeftPen,TopPen,RightPen,BottomPen: TPen) of object;

  TGridAlignEvent = procedure (Sender: TObject; ARow, ACol: Integer;
    var HAlign: Classes.TAlignment;var VAlign: TAsgVAlignment) of object;

  TGridHintEvent = procedure (Sender:TObject; ARow, ACol: Integer;
    var hintstr:string) of object;

  TEllipsClickEvent = procedure(Sender:TObject;ACol,ARow: Integer;
    var S:string) of object;

  TGridBalloonEvent = procedure(Sender: TObject; ACol, ARow: Integer; var ATitle: string; var AText: string; var AIcon: Integer) of object;

  TCanInsertRowEvent = procedure(Sender: TObject; ARow: Integer;
    var CanInsert: Boolean) of object;

  TAutoInsertRowEvent = procedure(Sender:TObject; ARow: Integer) of object;

  TCanAddRowEvent = procedure(Sender: TObject; var CanAdd: Boolean) of object;
  TCanAddColEvent = procedure(Sender: TObject; var CanAdd: Boolean) of object;

  TAutoAddRowEvent = procedure(Sender:TObject; ARow: Integer) of object;

  TCanDeleteRowEvent = procedure(Sender: TObject; ARow: Integer;
    var CanDelete: Boolean) of object;

  TAutoDeleteRowEvent = procedure(Sender:TObject; ARow: Integer) of object;

  TAutoInsertColEvent = procedure(Sender:TObject; ACol: Integer) of object;

  TSearchEditChangeEvent = procedure(Sender: TObject; Value: string; var DefaultSearch: boolean) of object;

  TSpinClickEvent = procedure(Sender:TObject;ACol,ARow,
     AValue: Integer; UpDown: Boolean) of object;

  {$IFDEF TMSGDIPLUS}
  TOfficeHintEvent = procedure(Sender: TObject; ACol, ARow: Integer; OfficeHint: TAdvHintInfo) of object;
  {$ENDIF}

  TArrowDirection = (arrUp,arrDown,arrLeft,arrRight);

  TAdvGridButtonStyle = (tasButton, tasCheck);

  TAdvGridButton = class;

  TImageChangeEvent = procedure (Sender:TObject;Acol,Arow: Integer) of object;

  TArrowWindow = class(TPanel)
  private
    Dir: TArrowDirection;
    Arrow: array[0..8] of TPoint;
  public
    constructor Init(AOwner: TComponent;direction:TArrowDirection);
    procedure Loaded; override;
  protected
    procedure CreateWnd; override;
    procedure Paint; override;
    procedure CreateParams(var Params: TCreateParams); override;
  end;

  TPopupButton = class(TCustomControl)
  private
    FCaption: string;
    FImages: TCustomImageList;
    FFlat: boolean;
    FGradTo: TColor;
    FGradFrom: TColor;
    FGradMirrorTo: TColor;
    FGradMirrorFrom: TColor;
  private
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    procedure CreateWnd; override;
    property Images: TCustomImageList read FImages write FImages;
  published
    property Caption:string read FCaption write FCaption;
    property Flat: boolean read FFlat write FFlat;
    property GradFrom: TColor read FGradFrom write FGradFrom;
    property GradTo: TColor read FGradTo write FGradTo;
    property GradMirrorFrom: TColor read FGradMirrorFrom write FGradMirrorFrom;
    property GradMirrorTo: TColor read FGradMirrorTo write FGradMirrorTo;
  end;

  TIntList = class(TList)
  private
    FOnChange: TImageChangeEvent;
    FCol,FRow: Integer;
    procedure SetInteger(Index: Integer; Value: Integer);
    function GetInteger(Index: Integer):Integer;
    function GetStrValue: string;
    procedure SetStrValue(const Value: string);
  public
    constructor Create(Col,Row: Integer);
    procedure DeleteValue(Value: Integer);
    function HasValue(Value: Integer): Boolean;
    function SortedIndexOf(Value: Integer): Integer;
    function SortedPosOf(Value: Integer): Integer;
    property Items[index: Integer]: Integer read GetInteger write SetInteger; default;
    procedure Add(Value: Integer);
    procedure Insert(Index,Value: Integer); overload;
    procedure Delete(Index: Integer);
    procedure Insert(Value: integer); overload;
    property StrValue: string read GetStrValue write SetStrValue;
    property OnChange: TImageChangeEvent read FOnChange write FOnChange;
  end;

  TSortIndexList = class(TIntList)
  private
    function GetSortColumns(i: Integer): Integer;
    function GetSortDirections(i: Integer): Boolean;
    procedure SetSortColumns(i: Integer; const Value: Integer);
    procedure SetSortDirections(i: Integer; const Value: Boolean);
  public
    function SaveToString: string;
    procedure LoadFromString(s: string);
    procedure AddIndex(ColumnIndex: integer; Ascending:boolean);
    function FindIndex(ColumnIndex: integer):integer;
    procedure ToggleIndex(ColumnIndex: integer);
    property SortColumns[i: Integer]: Integer read GetSortColumns write SetSortColumns;
    property SortDirections[i: Integer]: Boolean read GetSortDirections write SetSortDirections;
  end;

  TControlItem = class(TObject)
  private
    FX: Integer;
    FY: Integer;
    FObject: TControl;
  public
    constructor Create(AX,AY: Integer; AObject: TControl);
    property X: integer read FX write FX;
    property Y: integer read FY write FY;
    property Control: TControl read FObject write FObject;
  end;

  TControlList = class(TList)
  private
    function GetControl(i: Integer): TControlItem;
  public
    procedure AddControl(X,Y: Integer; AObject: TControl);
    procedure RemoveControl(i: Integer);
    function ControlIndex(X,Y: Integer): Integer; 
    property Control[i: Integer]: TControlItem read GetControl;
    destructor Destroy; override;
    function HasHandle(Handle: THandle): Boolean; 
  end;

  TFilePicture = class(TPersistent)
  private
    FFilename: string;
    FWidth: Integer;
    FHeight: Integer;
    FPicture: TPicture;
    procedure SetFileName(const Value: string);
  public
    procedure DrawPicture(Canvas:TCanvas; r:TRect);
    procedure Assign(Source: TPersistent); override;
  published
    property Filename:string read FFileName write SetFileName;
    property Width:integer read FWidth;
    property Height:integer read FHeight;
  end;


  {$IFDEF DELPHI6_LVL}
  TAdvGridButtonActionLink = class(TControlActionLink)
  protected
    FClient: TAdvGridButton;
    procedure AssignClient(AClient: TObject); override;
    function IsCaptionLinked: Boolean; override;
    function IsCheckedLinked: Boolean; override;
    function IsGroupIndexLinked: Boolean; override;
    procedure SetGroupIndex(Value: Integer); override;
    procedure SetChecked(Value: Boolean); override;
    procedure SetCaption(const Value: string); override;
  end;
  {$ENDIF}

  TAdvGridButton = class(TGraphicControl)
  private
    FIsComCtl6: Boolean;
    FGroupIndex: Integer;
    FGlyph: TBitmap;
    FDown: Boolean;
    FDragging: Boolean;
    FAllowAllUp: Boolean;
    FLayout: TButtonLayout;
    FSpacing: Integer;
    FTransparent: Boolean;
    FMargin: Integer;
    FFlat: Boolean;
    FMouseInControl: Boolean;
    FColorTo: TColor;
    FColorHot: TColor;
    FColorHotTo: TColor;
    FColorDown: TColor;
    FColorDownTo: TColor;
    FBorderColor: TColor;
    FBorderDownColor: TColor;
    FBorderHotColor: TColor;
    FGlyphDisabled: TBitmap;
    FGlyphHot: TBitmap;
    FGlyphDown: TBitmap;
    FGlyphShade: TBitmap;
    FShaded: Boolean;
    FOnMouseLeave: TNotifyEvent;
    FOnMouseEnter: TNotifyEvent;
    FColorChecked: TColor;
    FColorCheckedTo: TColor;
    FStyle: TAdvGridButtonStyle;
    FLook: Integer;
    FRounded: Boolean;
    FDropDownButton: Boolean;
    FAutoThemeAdapt: Boolean;
    FAutoXPStyle: Boolean;
    FOnDropDown: TNotifyEvent;
    FDropDownMenu: TPopupMenu;
    FShowCaption: Boolean;
    procedure GlyphChanged(Sender: TObject);
    procedure UpdateExclusive;
    procedure SetGlyph(Value: TBitmap);
    procedure SetDown(Value: Boolean);
    procedure SetFlat(Value: Boolean);
    procedure SetAllowAllUp(Value: Boolean);
    procedure SetGroupIndex(Value: Integer);
    procedure SetLayout(Value: TButtonLayout);
    procedure SetSpacing(Value: Integer);
    procedure SetMargin(Value: Integer);
    procedure UpdateTracking;
    procedure WMLButtonDblClk(var Message: TWMLButtonDown); message WM_LBUTTONDBLCLK;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMButtonPressed(var Message: TMessage); message CM_BUTTONPRESSED;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure CMSysColorChange(var Message: TMessage); message CM_SYSCOLORCHANGE;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure SetGlyphDisabled(const Value: TBitmap);
    procedure SetGlyphDown(const Value: TBitmap);
    procedure SetGlyphHot(const Value: TBitmap);
    procedure GenerateShade;
    procedure SetShaded(const Value: Boolean);
    procedure SetColorTo(const Value: TColor);
    procedure SetColorChecked(const Value: TColor);
    procedure SetColorCheckedTo(const Value: TColor);
    procedure SetStyle(const Value: TAdvGridButtonStyle);
    procedure SetRounded(const Value: Boolean);
    procedure SetDropDownButton(const Value: Boolean);
    procedure SetShowCaption(const Value: Boolean);
    procedure SetBorderColor(const Value: TColor);
    procedure SetLook(const Value: Integer);
    function GetColor: TColor;
    procedure SetColor(const Value: TColor);
  protected
    FState: TButtonState;
    {$IFDEF DELPHI6_LVL}
    function GetActionLinkClass: TControlActionLinkClass; override;
    {$ENDIF}
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure DrawButtonGlyph(Canvas: TCanvas; const GlyphPos: TPoint;
      State: TButtonState; Transparent: Boolean);
    procedure DrawButtonText(Canvas: TCanvas; const Caption: string;
      TextBounds: TRect; State: TButtonState; BiDiFlags: LongInt);
    function DrawButton(Canvas: TCanvas; const Client: TRect;
      const Offset: TPoint; const Caption: string; Layout: TButtonLayout;
      Margin, Spacing: Integer; State: TButtonState; Transparent: Boolean;
      BiDiFlags: LongInt): TRect;
    procedure CalcButtonLayout(Canvas: TCanvas; const Client: TRect;
      const Offset: TPoint; const Caption: string; Layout: TButtonLayout; Margin,
      Spacing: Integer; var GlyphPos: TPoint; var TextBounds: TRect;
      BiDiFlags: LongInt);
    procedure Paint; override;
    property MouseInControl: Boolean read FMouseInControl;
    procedure WndProc(var Message: TMessage); override;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;    
    procedure ThemeAdapt;
    procedure SetAutoThemeAdapt(const Value: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Click; override;
    property Look: Integer read FLook write SetLook;
  published
    property Action;
    property AllowAllUp: Boolean read FAllowAllUp write SetAllowAllUp default False;
    property Anchors;
    property AutoThemeAdapt: Boolean read FAutoThemeAdapt write SetAutoThemeAdapt;
    property AutoXPStyle: Boolean read FAutoXPStyle write FAutoXPStyle;
    property BiDiMode;
    property BorderColor: TColor read FBorderColor write SetBorderColor default clNone;
    property BorderDownColor: TColor read FBorderDownColor write FBorderDownColor default clNone;
    property BorderHotColor: TColor read FBorderHotColor write FBorderHotColor default clNone;
    property Color: TColor read GetColor write SetColor default clBtnFace;
    property ColorTo: TColor read FColorTo write SetColorTo default clNone;
    property ColorDown: TColor read FColorDown write FColorDown;
    property ColorDownTo: TColor read FColorDownTo write FColorDownTo default clNone;
    property ColorHot: TColor read FColorHot write FColorHot;
    property ColorHotTo: TColor read FColorHotTo write FColorHotTo default clNone;
    property ColorChecked: TColor read FColorChecked write SetColorChecked default clGray;
    property ColorCheckedTo: TColor read FColorCheckedTo write SetColorCheckedTo default clNone;
    property Constraints;
    property GroupIndex: Integer read FGroupIndex write SetGroupIndex default 0;
    property Down: Boolean read FDown write SetDown default False;
    property DropDownButton: Boolean read FDropDownButton write SetDropDownButton default False;
    property DropDownMenu: TPopupMenu read FDropDownMenu write FDropDownMenu;
    property Caption;
    property Enabled;
    property Flat: Boolean read FFlat write SetFlat default True;
    property Font;
    property Glyph: TBitmap read FGlyph write SetGlyph;
    property GlyphHot: TBitmap read FGlyphHot write SetGlyphHot;
    property GlyphDown: TBitmap read FGlyphDown write SetGlyphDown;
    property GlyphDisabled: TBitmap read FGlyphDisabled write SetGlyphDisabled;
    property Layout: TButtonLayout read FLayout write SetLayout default blGlyphLeft;
    property Margin: Integer read FMargin write SetMargin default -1;
    property ParentFont;
    property ParentShowHint;
    property ParentBiDiMode;
    property PopupMenu;
    property Rounded: Boolean read FRounded write SetRounded default False;
    property Shaded: Boolean read FShaded write SetShaded default True;
    property ShowCaption: Boolean read FShowCaption write SetShowCaption default True;
    property ShowHint;
    property Spacing: Integer read FSpacing write SetSpacing default 4;
    property Style: TAdvGridButtonStyle read FStyle write SetStyle default tasButton;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnDropDown: TNotifyEvent read FOnDropDown write FOnDropDown;
  end;

  TFileStringList = class(TStringList)
  private
    fp: integer;
    cache: string;
    function GetEOF: boolean;
  public
    procedure Reset;
    procedure ReadLn(var s: string);
    procedure Write(s: string);
    procedure WriteLn(s: string);
    property Eof: boolean read GetEOF;
  end;

  TAdvCheckBox = class(TCustomControl)
  private
    FDown:Boolean;
    FState:TCheckBoxState;
    FFocused:Boolean;
    FReturnIsTab:Boolean;
    FBtnVAlign: TTextLayout;
    FAlignment: TLeftRight;
    FEllipsis: Boolean;
    FCaption: string;
    FIsWinXP: Boolean;
    FHot: Boolean;
    FClicksDisabled: Boolean;
    FReadOnly: Boolean;
    FMouseDown:boolean;
    FBkgBmp: TBitmap;
    FBkgCache: boolean;
    FTransparentCaching: boolean;
    FTransparent:boolean;
    procedure WMEraseBkGnd(var Message:TMessage); message WM_ERASEBKGND;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure SetState(Value:TCheckBoxState);
    procedure SetChecked(Value:Boolean);
    function  GetChecked:Boolean;
    procedure SetCaption(Value: string);
    procedure SetButtonVertAlign(const Value: TTextLayout);
    procedure SetAlignment(const Value: TLeftRight);
    procedure SetEllipsis(const Value: Boolean);
    procedure DrawParentImage (Control: TControl; Dest: TCanvas);
    procedure SetTransparent(value:boolean);
  protected
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState;X, Y: Integer); override;
    procedure KeyDown(var Key:Word;Shift:TShiftSTate); override;
    procedure KeyUp(var Key:Word;Shift:TShiftSTate); override;
    procedure SetDown(Value:Boolean);
    procedure DoEnter; override;
    procedure DoExit; override;
    property ClicksDisabled: Boolean read FClicksDisabled write FClicksDisabled;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Toggle; virtual;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    property TransparentChaching: boolean read FTransparentCaching write FTransparentCaching;
    property Down: Boolean read FDown write SetDown default False;
  published
    property Action;
    property Anchors;
    property Constraints;
    property Color;
    property Alignment: TLeftRight read FAlignment write SetAlignment;
    property ButtonVertAlign: TTextLayout read FBtnVAlign write setButtonVertAlign default tlTop;
    property Caption: string read FCaption write SetCaption;
    property Checked: Boolean read GetChecked write SetChecked default False;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Ellipsis: Boolean read FEllipsis write SetEllipsis default False;
    property Enabled;
    property Font;
    property ParentFont;
    property ParentColor;
    property PopupMenu;
    property ReadOnly: Boolean read FReadOnly write FReadOnly default False;
    property ReturnIsTab: Boolean read FReturnIsTab write FReturnIsTab;
    property ShowHint;
    property State: TCheckBoxState read FState write SetState default cbUnchecked;
    property TabOrder;
    property TabStop;
    property Transparent:boolean read FTransparent write SetTransparent default False;
    property Visible;
    property OnClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

  TSearchDirection = (sdTopBottom, sdLeftRight);

  THighlightType = (hMatchingText, hFullCell);

  { TSearchFooter }

  TSearchFooter = class(TPersistent)
  private
    FShowFindPrev: boolean;
    FShowFindNext: boolean;
    FFindNextCaption: string;
    FFindPrevCaption: string;
    FColorTo: TColor;
    FColor: TColor;
    FOnChange: TNotifyEvent;
    FVisible: boolean;
    FAutoThemeAdapt: Boolean;
    FShowClose: Boolean;
    FShowHighLight: Boolean;
    FShowMatchCase: Boolean;
    FMatchCaseCaption: string;
    FHintClose: string;
    FHintFindPrev: string;
    FHintHighLight: string;
    FHintFindNext: string;
    FAutoSearch: Boolean;
    FLastSearch: string;
    FSearchColumn: Integer;
    FSearchActiveColumnOnly: Boolean;
    FSearchMatchStart: Boolean;
    FSearchFixedCells: Boolean;
    FSearchHiddenRows: Boolean;
    FHighLightCaption: string;
    FAlwaysHighLight: Boolean;
    FSearchDirection: TSearchDirection;
    FFont: TFont;
    FHighlight: THighlightType;
    procedure SetColor(const Value: TColor);
    procedure SetColorTo(const Value: TColor);
    procedure SetFindNextCaption(const Value: string);
    procedure SetFindPrevCaption(const Value: string);
    procedure SetShowFindNext(const Value: boolean);
    procedure SetShowFindPrev(const Value: boolean);
    procedure SetVisible(const Value: boolean);
    procedure SetAutoThemeAdapt(const Value: boolean);
    procedure SetShowClose(const Value: Boolean);
    procedure SetShowHighLight(const Value: Boolean);
    procedure SetShowMatchCase(const Value: Boolean);
    procedure SetMatchCaseCaption(const Value: string);
    procedure SetHintClose(const Value: string);
    procedure SetHintFindNext(const Value: string);
    procedure SetHintFindPrev(const Value: string);
    procedure SetHintHighlight(const Value: string);
    procedure SetHighLightCaption(const Value: string);
    procedure SetFont(const Value: TFont);
    procedure SetAutoSearch(const Value: Boolean);
  protected
    procedure Changed;
  public
    constructor Create(AOwner: TComponent; InitDesign: boolean);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property AutoThemeAdapt: Boolean read FAutoThemeAdapt write SetAutoThemeAdapt;
    property LastSearch: string read FLastSearch write FLastSearch;
  published
    property AlwaysHighLight: boolean read FAlwaysHighLight write FAlwaysHighLight default False;
    property AutoSearch: Boolean read FAutoSearch write SetAutoSearch default True;
    property Color: TColor read FColor write SetColor default clWhite;
    property ColorTo: TColor read FColorTo write SetColorTo default clBtnFace;
    property FindNextCaption: string read FFindNextCaption write SetFindNextCaption;
    property FindPrevCaption: string read FFindPrevCaption write SetFindPrevCaption;
    property Font: TFont read FFont write SetFont;
    property HighLight: THighlightType read FHighlight write FHighlight default hMatchingText;
    property HighLightCaption: string read FHighLightCaption write SetHighLightCaption;
    property HintClose: string read FHintClose write SetHintClose;
    property HintFindNext: string read FHintFindNext write SetHintFindNext;
    property HintFindPrev: string read FHintFindPrev write SetHintFindPrev;
    property HintHighlight: string read FHintHighLight write SetHintHighlight;
    property MatchCaseCaption: string read FMatchCaseCaption write SetMatchCaseCaption;
    property SearchActiveColumnOnly: Boolean read FSearchActiveColumnOnly write FSearchActiveColumnOnly default False;
    property SearchColumn: Integer read FSearchColumn write FSearchColumn default -1;
    property SearchMatchStart: boolean read FSearchMatchStart write FSearchMatchStart default False;
    property SearchFixedCells: boolean read FSearchFixedCells write FSearchFixedCells default False;
    property SearchHiddenRows: boolean read FSearchHiddenRows write FSearchHiddenRows default False;
    property SearchDirection: TSearchDirection read FSearchDirection write FSearchDirection default sdTopBottom;
    property ShowClose: Boolean read FShowClose write SetShowClose default true;
    property ShowFindNext: boolean read FShowFindNext write SetShowFindNext default true;
    property ShowFindPrev: boolean read FShowFindPrev write SetShowFindPrev default true;
    property ShowHighLight: Boolean read FShowHighLight write SetShowHighLight default true;
    property ShowMatchCase: Boolean read FShowMatchCase write SetShowMatchCase default true;
    property Visible: boolean read FVisible write SetVisible default false;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  THomeEndAction = (heFirstLastColumn,heFirstLastRow,heFirstLastCell,heDefault);
  TClipboardPasteAction = (paReplace, paInsert);
  TClipboardCutAction = (caClear, caRemove);
  TAdvanceDirection = (adLeftRight,adTopBottom,adLeftRightInRow,adTopBottomInCol);
  TInsertPosition = (pInsertBefore,pInsertAfter);

  TCursorAdvance = (caDefault, caSnake, caLoop);

  { TNavigation }

  TNavigation = class(TPersistent)
  private
    FAllowInsertRow: Boolean;
    FAllowDeleteRow: Boolean;
    FAdvanceOnEnter: Boolean;
    FAdvanceInsert: Boolean;
    FAutoGotoWhenSorted: Boolean;
    FAutoGotoIncremental: Boolean;
    FAutoComboDropSize: Boolean;
    FAutoComboSelect: Boolean;
    FAllowClipboardShortcuts: Boolean;
    FAllowRTFClipboard: Boolean;
    FAllowSmartClipboard: Boolean;
    FAllowClipboardAlways: Boolean;
    FAllowClipboardColGrow: Boolean;
    FAllowClipboardRowGrow: Boolean;
    FClipboardPasteAction: TClipboardPasteAction;
    FCopyHTMLTagsToClipboard: Boolean;
    FAdvanceDirection: TAdvanceDirection;
    FAdvanceAuto: Boolean;
    FAdvanceAutoEdit: Boolean;
    FCursorWalkEditor: Boolean;
    FCursorWalkAlwaysEdit: Boolean;
    FMoveRowOnSort: Boolean;
    FKeepScrollOnSort: Boolean;
    FImproveMaskSel: Boolean;
    FAlwaysEdit: Boolean;
    FInsertPosition: TInsertPosition;
    FLineFeedOnEnter: Boolean;
    FHomeEndKey: THomeEndAction;
    FKeepHorizScroll: Boolean;
    FAllowFMTClipboard: Boolean;
    FTabToNextAtEnd: Boolean;
    FEditSelectAll: Boolean;
    FTabAdvanceDirection: TAdvanceDirection;
    FSkipFixedCells: Boolean;
    FSkipDisabledRows: Boolean;
    FAllowCtrlEnter: Boolean;
    FAppendOnArrowDown: Boolean;
    FLeftRightRowSelect: Boolean;
    FMoveScrollOnly: Boolean;
    FComboGetUpdown: Boolean;
    FOnChange: TNotifyEvent;
    FAdvanceOnEnterLoop: Boolean;
    FCursorAdvance: TCursorAdvance;
    FAdvanceSkipReadOnlyCells: Boolean;
    FCursorMoveRows: Boolean;
    FAllowEditButtonCells: Boolean;
    FClipboardCutAction: TClipboardCutAction;
    procedure SetAutoGoto(aValue: Boolean);
    procedure SetAdvanceDirection(const Value: TAdvanceDirection);
    procedure SetAdvanceInsert(const Value: Boolean);
    procedure SetAdvanceAuto(const Value: Boolean);
  protected
    procedure Changed;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property AllowInsertRow: Boolean read FAllowInsertRow write FAllowInsertRow default False;
    property AllowDeleteRow: Boolean read FAllowDeleteRow write FAllowDeleteRow default False;
    property AlwaysEdit: Boolean read FAlwaysEdit write FAlwaysEdit default False;
    property AdvanceOnEnter: Boolean read FAdvanceOnEnter write FAdvanceOnEnter default False;
    property AdvanceOnEnterLoop: Boolean read FAdvanceOnEnterLoop write FAdvanceOnEnterLoop default True;
    property AdvanceInsert: Boolean read FAdvanceInsert write SetAdvanceInsert default False;
    property AdvanceAutoEdit: Boolean read FAdvanceAutoEdit write FAdvanceAutoEdit default True;
    property AdvanceSkipReadOnlyCells: Boolean read FAdvanceSkipReadOnlyCells write FAdvanceSkipReadOnlyCells default True;
    property AutoGotoWhenSorted: Boolean read FAutoGotoWhenSorted write SetAutoGoto default False;
    property AutoGotoIncremental: Boolean read FAutoGotoIncremental write FAutoGotoIncremental default False;
    property AutoComboDropSize: Boolean read FAutoComboDropSize write FAutoComboDropSize default False;
    property AutoComboSelect: Boolean read FAutoComboSelect write FAutoComboSelect default True;
    property AdvanceDirection: TAdvanceDirection read FAdvanceDirection write SetAdvanceDirection default adLeftRight;
    property AllowClipboardShortCuts: Boolean read FAllowClipboardShortcuts write FAllowClipboardShortcuts default False;
    property AllowCtrlEnter: Boolean read FAllowCtrlEnter write FAllowCtrlEnter default True;
    property AllowSmartClipboard: Boolean read FAllowSmartClipboard write FAllowSmartClipboard default False;
    property AllowRTFClipboard: Boolean read FAllowRTFClipboard write FAllowRTFClipboard default False;
    property AllowFmtClipboard: Boolean read FAllowFMTClipboard write FAllowFMTClipboard default False;
    property AllowClipboardAlways: Boolean read FAllowClipboardAlways write FAllowClipboardAlways default False;
    property AllowClipboardRowGrow: Boolean read FAllowClipboardRowGrow write FAllowClipboardRowGrow default True;
    property AllowClipboardColGrow: Boolean read FAllowClipboardColGrow write FAllowClipboardColGrow default True;
    property AllowEditButtonCells: Boolean read FAllowEditButtonCells write FAllowEditButtonCells default False;
    property AdvanceAuto: Boolean read FAdvanceAuto write SetAdvanceAuto default False;
    property AppendOnArrowDown: Boolean read FAppendOnArrowDown write FAppendOnArrowDown default False;
    property EditSelectAll: Boolean read FEditSelectAll write FEditSelectAll default True;
    property InsertPosition: TInsertPosition read FInsertPosition write FInsertPosition default pInsertBefore;
    property ClipboardCutAction: TClipboardCutAction read FClipboardCutAction write FClipboardCutAction default caClear;
    property ClipboardPasteAction: TClipboardPasteAction read FClipboardPasteAction write FClipboardPasteAction default paReplace;
    property ComboGetUpDown: boolean read FComboGetUpDown write FComboGetUpdown default True;
    property CursorAdvance: TCursorAdvance read FCursorAdvance write FCursorAdvance default caDefault;
    property CursorWalkEditor: Boolean read FCursorWalkEditor write FCursorWalkEditor default False;
    property CursorWalkAlwaysEdit: Boolean read FCursorWalkAlwaysEdit write FCursorWalkAlwaysEdit default True;
    property CursorMoveRows: Boolean read FCursorMoveRows write FCursorMoveRows default False;
    property MoveRowOnSort: Boolean read FMoveRowOnSort write FMoveRowOnSort default False;
    property KeepScrollOnSort: Boolean read FKeepScrollOnSort write FKeepScrollOnSort default False;
    property MoveScrollOnly: Boolean read FMoveScrollOnly write FMoveScrollOnly default False;
    property ImproveMaskSel: Boolean read FImproveMaskSel write FImproveMaskSel default False;
    property LeftRightRowSelect: Boolean read FLeftRightRowSelect write FLeftRightRowSelect default true;
    property CopyHTMLTagsToClipboard: Boolean read FCopyHTMLTagsToClipboard write FCopyHTMLTagsToClipboard default True;
    property KeepHorizScroll: Boolean read FKeepHorizScroll write FKeepHorizScroll default False;
    property LineFeedOnEnter: Boolean read FLineFeedOnEnter write FLineFeedOnEnter default False;
    property HomeEndKey: THomeEndAction read FHomeEndKey write FHomeEndKey default heFirstLastColumn;
    property SkipFixedCells: Boolean read FSkipFixedCells write FSkipFixedCells default True;
    property SkipDisabledRows: Boolean read FSkipDisabledRows write FSkipDisabledRows default True;
    property TabToNextAtEnd: Boolean read FTabToNextAtEnd write FTabToNextAtEnd default False;
    property TabAdvanceDirection: TAdvanceDirection read FTabAdvanceDirection write FTabAdvanceDirection default adLeftRight;
  end;

  TGridFixedCellEdit = (fceNone, fceDblClick, fceLeftClick, fceRightClick);

  TGridFixedCellEditor = (fceEdit, fceComboEdit, fceComboList);

  TWheelAction = (waMoveSelection, waScroll);

  TWheelActive = (waFocus, waMouseOver);

  TIsDesigningEvent = procedure(Sender: TObject; var IsDesigning: boolean) of object;

  {TMouseActions}

  TMouseActions = class(TPersistent)
  private
    //FGrid: TAdvStringGrid;
    FColSelect: Boolean;
    FRowSelect: Boolean;
    FAllSelect: Boolean;
    FDirectEdit: Boolean;
    FDirectComboDrop: Boolean;
    FDirectComboClose: Boolean;
    FDirectDateClose: Boolean;
    FDirectDateDrop: Boolean;
    FDisjunctRowSelect: Boolean;
    FDisjunctColSelect: Boolean;
    FAllColumnSize: Boolean;
    FAllRowSize: Boolean;
    FCaretPositioning: Boolean;
    FSizeFixedCol: Boolean;
    FSizeFixedRow: Boolean;
    FDisjunctCellSelect: Boolean;
    FFixedRowsEdit: TGridFixedCellEdit;
    FFixedColsEdit: TGridFixedCellEdit;
    FFixedRowsEditor: TGridFixedCellEditor;
    FFixedColsEditor: TGridFixedCellEditor;
    FHotmailRowSelect: Boolean;
    FRangeSelectAndEdit: Boolean;
    FNoAutoRangeScroll: Boolean;
    FPreciseCheckBoxCheck: Boolean;
    FPreciseNodeClick: Boolean;
    FCheckAllCheck: Boolean;
    FNodeAllExpandContract: Boolean;
    FMoveRowOnNodeClick: Boolean;
    FRowSelectPersistent: Boolean;
    FSelectOnRightClick: Boolean;
    FNoScrollOnPartialRow: Boolean;
    FNoScrollOnPartialCol: Boolean;
    FWheelIncrement: Integer;
    FWheelAction: TWheelAction;
    FAutoSizeColOnDblClick: Boolean;
    FEditOnDblClickOnly: Boolean;
    FDisjunctRowSelectNoCtrl: Boolean;
    FOnChange: TNotifyEvent;
    FOnInvalidate: TNotifyEvent;
    FOnIsDesigning: TIsDesigningEvent;
    FOnDisableEdit: TNotifyEvent;
    FFixedComboValues: TStringList;
    FTouchScroll: boolean;
    FWheelActive: TWheelActive;
    FAutoFocus: Boolean;
    FPartialScrollDelta: integer;
    procedure SetDisjunctColSelect(const AValue: Boolean);
    procedure SetDisjunctRowSelect(const AValue: Boolean);
    procedure SetDisjunctCellSelect(const AValue: Boolean);
    procedure SetHotmailRowSelect(const AValue: Boolean);
    procedure SetEditOnDblClickOnly(const AValue: Boolean);
    procedure SetWheelAction(const Value: TWheelAction);
    procedure SetFixedComboValues(const Value: TStrings);
    function GetFixedComboValues: TStrings;
  protected
    procedure Changed;
    function IsDesigning: boolean;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    // Event notifiers to owner
    property DisjunctRowSelectDirect: boolean read FDisjunctRowSelect write FDisjunctRowSelect;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnIsDesigning: TIsDesigningEvent read FOnIsDesigning write FOnIsDesigning;
    property OnInvalidate: TNotifyEvent read FOnInvalidate write FOnInvalidate;
    property OnDisableEdit: TNotifyEvent read FOnDisableEdit write FOnDisableEdit;
  published
    property AllColumnSize: Boolean read FAllColumnSize write FAllColumnSize default False;
    property AllRowSize: Boolean read FAllRowSize write FAllRowSize default False;
    property AllSelect: Boolean read FAllSelect write FAllSelect default False;
    property AutoFocus: Boolean read FAutoFocus write FAutoFocus default False;
    property AutoSizeColOnDblClick: Boolean read FAutoSizeColOnDblClick write FAutoSizeColOnDblClick default True;
    property CaretPositioning: Boolean read FCaretPositioning write FCaretPositioning default False;
    property CheckAllCheck: Boolean read FCheckAllCheck write FCheckAllCheck default False;
    property ColSelect: Boolean read FColSelect write FColSelect default False;
    property DirectComboClose: Boolean read FDirectComboClose write FDirectComboClose default False;
    property DirectComboDrop: Boolean read FDirectComboDrop write FDirectComboDrop default False;
    property DirectDateClose: Boolean read FDirectDateClose write FDirectDateClose default False;
    property DirectDateDrop: Boolean read FDirectDateDrop write FDirectDateDrop default False;
    property DirectEdit: Boolean read FDirectEdit write FDirectEdit default False;
    property DisjunctRowSelect: Boolean read FDisjunctRowSelect write SetDisjunctRowSelect default False;
    property DisjunctRowSelectNoCtrl: Boolean read FDisjunctRowSelectNoCtrl write FDisjunctRowSelectNoCtrl default False;
    property DisjunctColSelect: Boolean read FDisjunctColSelect write SetDisjunctColSelect default False;
    property DisjunctCellSelect: Boolean read FDisjunctCellSelect write SetDisjunctCellSelect default False;
    property EditOnDblClickOnly: Boolean read FEditOnDblClickOnly write SetEditOnDblClickOnly default False;
    property FixedRowsEdit: TGridFixedCellEdit read FFixedRowsEdit write FFixedRowsEdit default fceNone;
    property FixedRowsEditor: TGridFixedCellEditor read FFixedRowsEditor write FFixedRowsEditor default fceEdit;
    property FixedColsEditor: TGridFixedCellEditor read FFixedColsEditor write FFixedColsEditor default fceEdit;
    property FixedColsEdit: TGridFixedCellEdit read FFixedColsEdit write FFixedColsEdit default fceNone;
    property FixedComboValues: TStrings read GetFixedComboValues write SetFixedComboValues;
    property HotmailRowSelect: Boolean read FHotmailRowSelect write SetHotmailRowSelect default false;
    property MoveRowOnNodeClick: Boolean read FMoveRowOnNodeClick write FMoveRowOnNodeClick default False;
    property NoAutoRangeScroll: Boolean read FNoAutoRangeScroll write FNoAutoRangeScroll default False;
    property NodeAllExpandContract: Boolean read FNodeAllExpandContract write FNodeAllExpandContract default False;
    property NoScrollOnPartialRow: Boolean read FNoScrollOnPartialRow write FNoScrollOnPartialRow default False;
    property NoScrollOnPartialCol: Boolean read FNoScrollOnPartialCol write FNoScrollOnPartialCol default False;
    property PartialScrollDelta: Integer read FPartialScrollDelta write FPartialScrollDelta default 0;
    property PreciseCheckBoxCheck: Boolean read FPreciseCheckBoxCheck write FPreciseCheckBoxCheck default False;
    property PreciseNodeClick: Boolean read FPreciseNodeClick write FPreciseNodeClick default True;
    property RangeSelectAndEdit: Boolean read FRangeSelectAndEdit write FRangeSelectAndEdit default False;
    property RowSelect: Boolean read FRowSelect write FRowSelect default False;
    property RowSelectPersistent: Boolean read FRowSelectPersistent write FRowSelectPersistent default False;
    property SelectOnRightClick: Boolean read FSelectOnRightClick write FSelectOnRightClick default False;
    property SizeFixedCol: Boolean read FSizeFixedCol write FSizeFixedCol default False;
    property SizeFixedRow: Boolean read FSizeFixedRow write FSizeFixedRow default False;
    property TouchScroll: Boolean read FTouchScroll write FTouchScroll default False;
    property WheelIncrement: Integer read FWheelIncrement write FWheelIncrement default 0;
    property WheelAction: TWheelAction read FWheelAction write SetWheelAction default waMoveSelection;
    property WheelActive: TWheelActive read FWheelActive write FWheelActive default waFocus;
  end;

  {TSyncGrid}

  TSyncGrid = class(TPersistent)
  private
    FOwner: TComponent;
    FScrollHorizontal: boolean;
    FGrid: TStringGrid;
    FScrollVertical: boolean;
    FSelectionRow: boolean;
    FSelectionColumn: boolean;
    FMoveRow: boolean;
    FMoveColumn: boolean;
    procedure SetGrid(const Value: TStringGrid);
  public
    constructor Create(AOwner: TComponent);
    procedure Assign(Source: TPersistent); override;
  published
    property Grid: TStringGrid read FGrid write SetGrid;
    property ScrollVertical: boolean read FScrollVertical write FScrollVertical default false;
    property ScrollHorizontal: boolean read FScrollHorizontal write FScrollHorizontal default false;
    property SelectionColumn: boolean read FSelectionColumn write FSelectionColumn default false;
    property SelectionRow: boolean read FSelectionRow write FSelectionRow default false;
    property MoveColumn: boolean read FMoveColumn write FMoveColumn default false;
    property MoveRow: boolean read FMoveRow write FMoveRow default false;
  end;


  {TBands}

  TBands = class(TPersistent)
  private
    FPrint: Boolean;
    FActive: Boolean;
    FTotalLength: Integer;
    FSecondaryLength: Integer;
    FPrimaryLength: Integer;
    FSecondaryColor: TColor;
    FPrimaryColor: TColor;
    FOnChange: TNotifyEvent;
    procedure SetActive(const Value: Boolean);
    procedure SetPrimaryColor(const Value: TColor);
    procedure SetPrimaryLength(const Value: Integer);
    procedure SetSecondaryColor(const Value: TColor);
    procedure SetSecondaryLength(const Value: Integer);
  protected
    procedure Changed;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
    property TotalLength: integer read FTotalLength;
  published
    property Active: Boolean read FActive write SetActive default False;
    property PrimaryColor: TColor read FPrimaryColor write SetPrimaryColor default clInfoBk;
    property PrimaryLength: Integer read FPrimaryLength write SetPrimaryLength default 1;
    property SecondaryColor: TColor read FSecondaryColor write SetSecondaryColor default clWindow;
    property SecondaryLength: Integer read FSecondaryLength write SetSecondaryLength default 1;
    property Print: Boolean read FPrint write FPrint default False;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;


  {TGridFocusWindow}

  TGridFocusWindow = class(TCustomControl)
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure CreateWnd; override;
  end;

  TGridAlphaMask = class(TBitmap)
  private
    FOpacity: Byte;
  public
    constructor Create; override;
    procedure Assign(Source: TPersistent); override;
    procedure MergeMask(Mask, Bitmap: TBitmap; Color: TColor);
    property Opacity: Byte read FOpacity write FOpacity;
  end;

  TFocusHelper = class(TPersistent)
  private
    FEnabled: boolean;
  public
    constructor Create(AOwner: TComponent);
    procedure Assign(Source: TPersistent); override;
  published
    property Enabled: boolean read FEnabled write FEnabled default false;
  end;

procedure DrawRoundRect(Bitmap: TBitmap; W, H: Integer; Thickness, Radius: Integer; Blur: Double);


implementation

uses
  ActnList, ComObj, AdvUtil, AsgHTMLE, Forms
  ;

const
  // theme changed notifier
  WM_THEMECHANGED = $031A;


const
  MaxKernelSize = 10;

{$IFNDEF DELPHI6_LVL}
  {$EXTERNALSYM AC_SRC_ALPHA}
  AC_SRC_ALPHA = $01;

  {$EXTERNALSYM AC_SRC_OVER}
  AC_SRC_OVER = $00;

  {$EXTERNALSYM ULW_ALPHA}
  ULW_ALPHA      = $00000002;

  {$EXTERNALSYM WS_EX_LAYERED}
  WS_EX_LAYERED  = $00080000;
{$ENDIF}

type
  PRGBA = ^TRGBA;
  TRGBA = record
    Blue: Byte;
    Green: Byte;
    Red: Byte;
    Alpha: Byte;
  end;

  PRGB = ^TRGB;
  TRGB = record
    Blue: Byte;
    Green: Byte;
    Red: Byte;
  end;

  PRGBTriplet = ^TRGBTriplet;
  TRGBTriplet = packed record
    b: byte;
    g: byte;
    r: byte;
  end;

  TKernelSize = 1..MaxKernelSize;
  TKernel = record
    Size: TKernelSize;
    Weights: array[-MaxKernelSize..MaxKernelSize] of single;
  end;
  PKernel = ^TKernel;

  PBlendFunction = ^TBlendFunction;
 _BLENDFUNCTION = packed record
    BlendOp: BYTE;
    BlendFlags: BYTE;
    SourceConstantAlpha: BYTE;
    AlphaFormat: BYTE;
 end;
 TBlendFunction = _BLENDFUNCTION;


  PRow = ^TRow;
  TRow = array[0..10000] of TRGBTriplet;
  PPRows = ^TPRows;
  TPRows = array[0..10000] of PRow;

type
  XPColorScheme = (xpNone, xpBlue, xpGreen, xpGray);

{$IFNDEF DELPHI7_LVL}
function GetFileVersion(FileName:string): Integer;
var
  FileHandle:dword;
  l: Integer;
  pvs: PVSFixedFileInfo;
  lptr: uint;
  querybuf: array[0..255] of char;
  buf: PChar;
begin
  Result := -1;

  StrPCopy(querybuf,FileName);
  l := GetFileVersionInfoSize(querybuf,FileHandle);
  if (l>0) then
  begin
    GetMem(buf,l);
    GetFileVersionInfo(querybuf,FileHandle,l,buf);
    if VerQueryValue(buf,'\',Pointer(pvs),lptr) then
    begin
      if (pvs^.dwSignature = $FEEF04BD) then
      begin
        Result := pvs^.dwFileVersionMS;
      end;
    end;
    FreeMem(buf);
  end;
end;
{$ENDIF}

function DoThemeDrawing: Boolean;
var
  VerInfo: TOSVersioninfo;
  FIsWinXP,FIsComCtl6: boolean;
  i: integer;
begin
  VerInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);

  GetVersionEx(verinfo);

  FIsWinXP := (verinfo.dwMajorVersion > 5) OR
    ((verinfo.dwMajorVersion = 5) AND (verinfo.dwMinorVersion >= 1));

  if FIsWinXP then
  begin
    FIsWinXP := FIsWinXP and IsThemeActive;
  end;

  i := GetFileVersion('COMCTL32.DLL');
  i := (i shr 16) and $FF;

  FIsComCtl6 := (i > 5);

  Result := FIsComCtl6 and FIsWinXP;
end;





{ TArrowWindow }

procedure TArrowWindow.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := WS_POPUP; // or WS_BORDER;
  end;
end;

procedure TArrowWindow.Loaded;
begin
  inherited;
end;


procedure TArrowWindow.CreateWnd;
var
  hrgn: THandle;

begin
  inherited;

  case Dir of
  arrDown:begin
         arrow[0] := point(3,0);
         arrow[1] := point(7,0);
         arrow[2] := point(7,4);
         arrow[3] := point(9,4);
         arrow[4] := point(5,8);
         arrow[5] := point(1,4);
         arrow[6] := point(3,4);
         end;
  arrUp:begin
         arrow[0] := point(5,0);
         arrow[1] := point(10,5);
         arrow[2] := point(7,5);
         arrow[3] := point(7,9);
         arrow[4] := point(3,9);
         arrow[5] := point(3,5);
         arrow[6] := point(0,5);
       end;
  arrLeft:begin
         arrow[0] := point(0,3);
         arrow[1] := point(0,7);
         arrow[2] := point(4,7);
         arrow[3] := point(4,10);
         arrow[4] := point(8,5);
         arrow[5] := point(4,0);
         arrow[6] := point(4,3);
         end;
  arrRight:begin
         arrow[0] := point(0,5);
         arrow[1] := point(4,10);
         arrow[2] := point(4,7);
         arrow[3] := point(8,7);
         arrow[4] := point(8,3);
         arrow[5] := point(4,3);
         arrow[6] := point(4,0);
         end;
  end;
  hrgn := CreatePolygonRgn(arrow,7,WINDING);
  SetWindowRgn(Handle,hrgn,True);
end;

procedure TArrowWindow.Paint;
begin
//  inherited;  // remove, is not working in Windows XP
  Canvas.Brush.Color := Color;
  Canvas.Pen.Color := Color;
  Canvas.Rectangle(ClientRect.Left,ClientRect.Top,ClientRect.Right,ClientRect.Bottom);
end;

constructor TArrowWindow.Init(AOwner: TComponent; Direction:TArrowDirection);
begin
  Dir := Direction;
  inherited Create(aOwner);
  Color := clLime;
  Parent := TWinControl(AOwner);
  Visible := False; 
end;

{ TIntList }

constructor TIntList.Create(Col,Row: Integer);
begin
  inherited Create;
  FCol := Col;
  FRow := Row;
end;

procedure TIntList.SetInteger(Index:Integer;Value:Integer);
begin
  inherited Items[Index] := Pointer(Value);

  if Assigned(OnChange) then
    OnChange(Self,FCol,FRow);
end;

function TIntList.GetInteger(Index: Integer): Integer;
begin
  Result := Integer(inherited Items[Index]);
end;

procedure TIntList.DeleteValue(Value: Integer);
var
  i: integer;
begin
  i := IndexOf(Pointer(Value));

  if i <> -1 then
    Delete(i);
end;

function TIntList.HasValue(Value: Integer): Boolean;
begin
  Result := IndexOf(Pointer(Value)) <> -1;
end;

procedure TIntList.Add(Value: Integer);
begin
  inherited Add(Pointer(Value));

  if Assigned(OnChange) then
    OnChange(Self,FCol,FRow);
end;

procedure TIntList.Delete(Index: Integer);
begin
  inherited Delete(Index);
  if Assigned(OnChange) then
    OnChange(Self,FCol,FRow);
end;

function TIntList.GetStrValue: string;
var
  i: integer;
begin
  for i := 1 to Count do
    if i = 1 then
      Result:= IntToStr(Items[i - 1])
    else
      Result := Result + ',' + IntToStr(Items[i - 1]);
end;

procedure TIntList.SetStrValue(const Value: string);
var
  sl:TStringList;
  i: Integer;
begin
  sl := TStringList.Create;
  sl.CommaText := Value;
  Clear;
  for i := 1 to sl.Count do
   Add(StrToInt(sl.Strings[i - 1]));
  sl.Free;
end;

procedure TIntList.Insert(Index, Value: Integer);
begin
  inherited Insert(Index, Pointer(Value));
end;

function TIntList.SortedIndexOf(Value: Integer): Integer;
var
  l,r,m: integer;
begin
  if Count = 0 then
    Result := -1
  else
  if (Count = 1) and (Items[0] = Value) then
  begin
    Result := 0;
  end
  else
  begin
    Result := -1;
    l := 0;
    r := Count - 1;

    while l <= r do
    begin
      m := l + (r - l) div 2;

      if Value < Items[m] then
        r := Pred(m)
      else
        if Value > Items[m] then
          l := Succ(m)
          else
            if Value = Items[m] then
            begin
              Result := m;
              break;
            end;
    end;
  end;
end;

function TIntList.SortedPosOf(Value: Integer): integer;
var
  l,r,m: integer;
begin
  if Count = 0 then
    Result := 0
  else
  begin
    m := 0;
    l := 0;
    r := Count - 1;

    while l <= r do
    begin
      m := l + (r - l) div 2;

      if Value < Items[m] then
        r := Pred(m)
      else
        if Value > Items[m] then
          l := Succ(m)
          else
            if Value = Items[m] then
              break;
    end;

    if Items[m] < Value then
      inc(m);

    Result := m;
  end;
end;

procedure TIntList.Insert(Value: Integer);
var
  l,r,m: integer;
begin
  if Count = 0 then
    Add(Value)
  else
  begin
    m := 0;
    l := 0;
    r := Count - 1;

    while l <= r do
    begin
      m := l + (r - l) div 2;

      if Value < Items[m] then
        r := Pred(m)
      else
        if Value > Items[m] then
          l := Succ(m)
          else
            if Value = Items[m] then
              break;
    end;

    if Items[m] < Value then
      inc(m);

    inherited Insert(m,Pointer(Value));
  end;
end;


{ TFilePicture }

procedure TFilePicture.Assign(Source: TPersistent);
begin
  FFileName := TFilePicture(Source).Filename;
  FWidth := TFilePicture(Source).Width;
  FHeight := TFilePicture(Source).Height;
end;

procedure TFilePicture.DrawPicture(Canvas: TCanvas; r: TRect);
begin
  if FFilename = '' then
    Exit;

  if FileExists(FFileName) then
  begin
    FPicture := TPicture.Create;
    try
      FPicture.LoadFromFile(FFilename);
      Canvas.StretchDraw(r,FPicture.Graphic);
    except

    end;
    FPicture.Free;
  end;
end;

procedure TFilePicture.SetFileName(const Value: string);
begin
  FFileName := Value;

  if FileExists(FFileName) then
  begin
    FPicture := TPicture.Create;
    try
      FPicture.LoadFromFile(FFilename);
      FWidth := FPicture.Width;
      FHeight := FPicture.Height;
    except
    end;
    FPicture.Free;
  end;
end;

{ TSortIndexList }

procedure TSortIndexList.AddIndex(ColumnIndex: Integer;
  Ascending: boolean);
begin
  if Ascending then
    Add(ColumnIndex)
  else
    Add(integer($80000000) or ColumnIndex);
end;

function TSortIndexList.FindIndex(ColumnIndex: integer): integer;
var
  i: Integer;
begin
  Result := -1;
  i := 0;
  while i < Count do
  begin
    if Items[i] and $7FFFFFFF = ColumnIndex then
    begin
      Result := i;
      Break;
    end;
    Inc(i);
  end;
end;

function TSortIndexList.GetSortColumns(i: Integer): Integer;
begin
  Result := Items[i] and $7FFFFFFF;
end;

function TSortIndexList.GetSortDirections(i: Integer): Boolean;
begin
  Result := not ((Items[i] and $80000000) = $80000000);
end;

procedure TSortIndexList.LoadFromString(s: string);
var
  a: string;
  i,e: integer;
begin
  Clear;

  while length(s) > 0 do
  begin
    if pos(';',s) > 0 then
    begin
      a := copy(s,1,pos(';',s) - 1);
      System.Delete(s,1,pos(';',s));
    end
    else
    begin
      a := s;
      s := '';
    end;

    if length(a) > 1 then
    begin
      val(copy(a,2,length(a)),i,e);
      if a[1] = 'u' then
        AddIndex(i,true)
      else
        AddIndex(i,false);
    end;
  end;
end;

function TSortIndexList.SaveToString: string;
var
  i: integer;
  s,a: string;
begin
  s := '';
  a := '';
  for i := 0 to Count - 1 do
  begin
    if Items[i] and $80000000 = $80000000 then
    begin
      a := 'd' + inttostr($FFFF and Items[i]);
    end
    else
    begin
      a := 'u' + inttostr($FFFF and Items[i]);
    end;
    if s = '' then
      s := a
    else
      s := s + ';'+ a;
  end;
  Result := s;
end;

procedure TSortIndexList.SetSortColumns(i: Integer; const Value: Integer);
begin
  Items[i] := (DWord(Value) and $7FFFFFFF) + (Items[i] and $80000000);
end;

procedure TSortIndexList.SetSortDirections(i: Integer;
  const Value: Boolean);
begin
  if Value then
    Items[i] := (Items[i] and $7FFFFFFF)
  else
    Items[i] := (DWord(Items[i]) or $80000000);
end;

procedure TSortIndexList.ToggleIndex(ColumnIndex: integer);
var
  i: Integer;
begin
  i := 0;
  while i < Count do
  begin
    if Items[i] and $7FFFFFFF = ColumnIndex then
    begin
      if Items[i] and $80000000 = $80000000 then
        Items[i] := Items[i] and $7FFFFFFF
      else
        Items[i] := Items[i] or Integer($80000000);
      Break;
    end;
    Inc(i);
  end;
end;

{ TPopupButton }

constructor TPopupButton.Create(AOwner: TComponent);
begin
  inherited;
  FGradFrom := clNone;
  FGradTo := clNone;
  FImages := nil;
  DoubleBuffered := true;
end;

procedure TPopupButton.CreateParams(var Params: TCreateParams);
begin
  inherited;
  with Params do
  begin
    Style := WS_POPUP or WS_BORDER or WS_DISABLED;
    WindowClass.Style := WindowClass.Style or CS_SAVEBITS;
  end;
  Color := clBtnFace;
end;


procedure TPopupButton.CreateWnd;
begin
  inherited;
  SetWindowPos(Handle,HWND_TOPMOST,0,0,0,0,SWP_NOSIZE or SWP_NOMOVE or SWP_NOACTIVATE or SWP_NOREPOSITION);
end;

procedure TPopupButton.Paint;
var
  r: TRect;
  v, a, fa, ah: string;
  xs, ys, ml, hl: Integer;
  cid, cv, ct: string;
  cr, hr: TRect;

begin
  r := GetClientRect;
  if not FFlat then
    Frame3D(Canvas,r,clWhite,clGray,1);

  DrawVistaGradient(Canvas,r,FGradFrom,FGradTo,FGradMirrorFrom,FGradMirrorTo,true,clNone);

  {
  if FGradFrom <> clNone then
  begin
    DrawGradient(Canvas,FGradFrom,FGradTo, 64, r, False);
  end;
  }

  SetBkMode(Canvas.Handle,TRANSPARENT);

  if Pos('</',FCaption) > 0 then
  begin
    HTMLDrawEx(Canvas,FCaption, r, FImages, 2, 2, -1, -1, 2, false, false, false, false, false, false, false, false, '', 1.0, clBlue, clNone, clNone,
      clGray, v, a, fa, ah, xs, ys, hl, ml, hr, cr, cid, cv, ct, nil, nil, self.Handle)
  end
  else
  begin
    DrawTextEx(Canvas.Handle,PChar(FCaption),Length(FCaption),r,DT_CENTER or DT_END_ELLIPSIS,nil);
  end;
end;


{ TControlList }

procedure TControlList.AddControl(X, Y: Integer; AObject: TControl);
begin
  Add(TControlItem.Create(X,Y,AObject));
end;

function TControlList.ControlIndex(X, Y: Integer): Integer;
var
  i: Integer;
  CI: TControlItem;
begin
  Result := -1;

  for i := 1 to Count do
  begin
    CI := GetControl(i - 1);
    if (CI.X = X) and (CI.Y = Y) then
    begin
      Result := i - 1;
      Break;
    end;
  end;
end;

destructor TControlList.Destroy;
begin
  while Count > 0 do
    RemoveControl(0);
  inherited;
end;

function TControlList.GetControl(i: Integer): TControlItem;
begin
  Result := TControlItem(Items[i]);
end;

function TControlList.HasHandle(Handle: THandle): Boolean;
var
  i: Integer;
  CI: TControlItem;
begin
  Result := False;

  for i := 1 to Count do
  begin
    CI := GetControl(i - 1);
    if (CI.Control is TWinControl) then
    begin
      if (CI.Control as TWinControl).Handle = Handle then
      begin
        Result := true;
        Break;
      end;

    end;
  end;
end;

procedure TControlList.RemoveControl(i: Integer);
begin
  TControlItem(Items[i]).Free;
  Delete(i);
end;

{ TControlItem }

constructor TControlItem.Create(AX, AY: Integer; AObject: TControl);
begin
  inherited Create;
  FX := AX;
  FY := AY;
  FObject := AObject;
end;


function IsWinXP: Boolean;
var
  VerInfo: TOSVersioninfo;
begin
  VerInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
  GetVersionEx(verinfo);
  Result := (verinfo.dwMajorVersion > 5) OR
    ((verinfo.dwMajorVersion = 5) AND (verinfo.dwMinorVersion >= 1));
end;

function CurrentXPTheme: XPColorScheme;
var
  FileName, ColorScheme, SizeName: WideString;
  hThemeLib: THandle;
begin
  hThemeLib := 0;
  Result := xpNone;

  if not IsWinXP then
    Exit;

  try
    if IsThemeActive then
    begin
      SetLength(FileName, 255);
      SetLength(ColorScheme, 255);
      SetLength(SizeName, 255);
      GetCurrentThemeName(PWideChar(FileName), 255,
        PWideChar(ColorScheme), 255, PWideChar(SizeName), 255);
      if(PWideChar(ColorScheme)='NormalColor') then
        Result := xpBlue
      else if(PWideChar(ColorScheme)='HomeStead') then
        Result := xpGreen
      else if(PWideChar(ColorScheme)='Metallic') then
        Result := xpGray
      else
        Result := xpNone;
    end;
  finally
    if hThemeLib <> 0 then
      FreeLibrary(hThemeLib);
  end;
end;

procedure DrawGradient(Canvas: TCanvas; FromColor,ToColor, PenColor: TColor; Steps: Integer; R: TRect; Direction: Boolean);
var
  diffr,startr,endr: Integer;
  diffg,startg,endg: Integer;
  diffb,startb,endb: Integer;
  rstepr,rstepg,rstepb,rstepw: Real;
  i,stepw: Word;

begin
  if Steps = 0 then
    Steps := 1;

  FromColor := ColorToRGB(FromColor);
  ToColor := ColorToRGB(ToColor);

  startr := (FromColor and $0000FF);
  startg := (FromColor and $00FF00) shr 8;
  startb := (FromColor and $FF0000) shr 16;
  endr := (ToColor and $0000FF);
  endg := (ToColor and $00FF00) shr 8;
  endb := (ToColor and $FF0000) shr 16;

  diffr := endr - startr;
  diffg := endg - startg;
  diffb := endb - startb;

  rstepr := diffr / steps;
  rstepg := diffg / steps;
  rstepb := diffb / steps;

  if Direction then
    rstepw := (R.Right - R.Left) / Steps
  else
    rstepw := (R.Bottom - R.Top) / Steps;

  with Canvas do
  begin
    Brush.Style := bsSolid;  
    for i := 0 to steps-1 do
    begin
      endr := startr + Round(rstepr*i);
      endg := startg + Round(rstepg*i);
      endb := startb + Round(rstepb*i);
      stepw := Round(i*rstepw);
      Pen.Color := endr + (endg shl 8) + (endb shl 16);
      Brush.Color := Pen.Color;
      if Direction then
        Rectangle(R.Left + stepw,R.Top,R.Left + stepw + Round(rstepw)+1,R.Bottom)
      else
        Rectangle(R.Left,R.Top + stepw,R.Right,R.Top + stepw + Round(rstepw)+1);
    end;

    if PenColor <> clNone then
    begin
      Pen.Color := PenColor;
      Brush.Style := bsClear;
      Rectangle(R.Left,R.Top,R.Right, R.Bottom);
    end;
  end;

end;


constructor TAdvGridButton.Create(AOwner: TComponent);
var
  i: integer;

begin
  FGlyph := TBitmap.Create;
  FGlyph.OnChange := GlyphChanged;
  FGlyphHot := TBitmap.Create;
  FGlyphDown := TBitmap.Create;
  FGlyphDisabled := TBitmap.Create;
  FGlyphShade := TBitmap.Create;
  inherited Create(AOwner);

  SetBounds(0, 0, 23, 22);
  ControlStyle := [csCaptureMouse, csDoubleClicks];
  ParentFont := True;
  Color := clBtnFace;
  FColorTo := clNone;
  FColorHot := RGB(199,199,202);
  FColorHotTo := clNone;
  FColorDown := RGB(210,211,216);
  FColorDownTo := clNone;
  FColorChecked := clGray;
  FColorCheckedTo := clNone;
  FBorderColor := clNone;
  FBorderDownColor := clNone;
  FBorderHotColor := clNone;  
  FSpacing := 4;
  FMargin := -1;
  Flat := True;
  FLayout := blGlyphLeft;
  FTransparent := True;
  FShaded := True;
  FShowCaption := True;

  i := GetFileVersion('COMCTL32.DLL');
  i := (i shr 16) and $FF;
  FIsComCtl6 := (i > 5);
end;

destructor TAdvGridButton.Destroy;
begin
  inherited Destroy;
  FGlyph.Free;
  FGlyphHot.Free;
  FGlyphDown.Free;
  FGlyphDisabled.Free;
  FGlyphShade.Free;
end;

procedure TAdvGridButton.DrawButtonGlyph(Canvas: TCanvas; const GlyphPos: TPoint;
  State: TButtonState; Transparent: Boolean);
var
  SelGlyph: TBitmap;
begin
  if FMouseInControl then
  begin
    if (FState in [ bsDown, bsExclusive]) then
    begin
      if GlyphDown.Empty then
        SelGlyph := FGlyph
      else
        SelGlyph := GlyphDown;
    end
    else
    begin
      if GlyphHot.Empty or (csDesigning in ComponentState) then
        SelGlyph := FGlyph
      else
        SelGlyph := GlyphHot;
    end;
  end
  else
  begin
    if (FState in [ bsDown, bsExclusive]) then
    begin
      if GlyphDown.Empty then
        SelGlyph := FGlyph
      else
        SelGlyph := GlyphDown;
    end
    else
      SelGlyph := FGlyph;
  end;

  if not Enabled then
  begin
    if FGlyphDisabled.Empty then
      SelGlyph := FGlyph
    else
      SelGlyph := FGlyphDisabled;
  end;

//  Shaded := true;

  if not SelGlyph.Empty then
  begin
    if FMouseInControl and Shaded and Enabled and not (FState = bsDown) then
    begin
      FGlyphShade.TransparentMode := tmAuto;
      FGlyphShade.Transparent := True;
      if Caption <> '' then
        Canvas.Draw(GlyphPos.X + 2,GlyphPos.Y + 2, FGlyphShade)
      else
        Canvas.Draw(GlyphPos.X,GlyphPos.Y, FGlyphShade)
    end;

    SelGlyph.TransparentMode := tmAuto;
    SelGlyph.Transparent := True;

    Canvas.Draw(GlyphPos.X,GlyphPos.Y,SelGlyph);
  end;
end;

procedure TAdvGridButton.DrawButtonText(Canvas: TCanvas; const Caption: string;
  TextBounds: TRect; State: TButtonState; BiDiFlags: LongInt);
begin
  with Canvas do
  begin
    Brush.Style := bsClear;
    if State = bsDisabled then
    begin
      OffsetRect(TextBounds, 1, 1);
      Font.Color := clBtnHighlight;
      DrawText(Handle, PChar(Caption), Length(Caption), TextBounds,
        DT_CENTER or DT_VCENTER or BiDiFlags);
      OffsetRect(TextBounds, -1, -1);
      Font.Color := clBtnShadow;
      DrawText(Handle, PChar(Caption), Length(Caption), TextBounds,
        DT_CENTER or DT_VCENTER or BiDiFlags);
    end else
      DrawText(Handle, PChar(Caption), Length(Caption), TextBounds,
        DT_CENTER or DT_VCENTER or BiDiFlags);
  end;
end;

procedure TAdvGridButton.CalcButtonLayout(Canvas: TCanvas; const Client: TRect;
  const Offset: TPoint; const Caption: string; Layout: TButtonLayout; Margin,
  Spacing: Integer; var GlyphPos: TPoint; var TextBounds: TRect;
  BiDiFlags: LongInt);
var
  TextPos: TPoint;
  ClientSize, GlyphSize, TextSize: TPoint;
  TotalSize, Ofs: TPoint;
begin
  if (BiDiFlags and DT_RIGHT) = DT_RIGHT then
    if Layout = blGlyphLeft then Layout := blGlyphRight
    else
      if Layout = blGlyphRight then Layout := blGlyphLeft;
  { calculate the item sizes }
  ClientSize := Point(Client.Right - Client.Left, Client.Bottom -
    Client.Top);

  if not FGlyph.Empty then
    GlyphSize := Point(FGlyph.Width, FGlyph.Height) else
    GlyphSize := Point(0, 0);

  if Length(Caption) > 0 then
  begin
    TextBounds := Rect(0, 0, Client.Right - Client.Left, 0);
    DrawText(Canvas.Handle, PChar(Caption), Length(Caption), TextBounds,
      DT_CALCRECT or BiDiFlags);
    TextSize := Point(TextBounds.Right - TextBounds.Left, TextBounds.Bottom -
      TextBounds.Top);
  end
  else
  begin
    TextBounds := Rect(0, 0, 0, 0);
    TextSize := Point(0,0);
  end;
    
  { If the layout has the glyph on the right or the left, then both the
    text and the glyph are centered vertically.  If the glyph is on the top
    or the bottom, then both the text and the glyph are centered horizontally.}
  if Layout in [blGlyphLeft, blGlyphRight] then
  begin
    GlyphPos.Y := (ClientSize.Y - GlyphSize.Y + 1) div 2;
    TextPos.Y := (ClientSize.Y - TextSize.Y + 1) div 2;
  end
  else
  begin
    GlyphPos.X := (ClientSize.X - GlyphSize.X + 1) div 2;
    TextPos.X := (ClientSize.X - TextSize.X + 1) div 2;
  end;

  { if there is no text or no bitmap, then Spacing is irrelevant }
  if (TextSize.X = 0) or (GlyphSize.X = 0) then
    Spacing := 0;
    
  { adjust Margin and Spacing }
  if Margin = -1 then
  begin
    if Spacing = -1 then
    begin
      TotalSize := Point(GlyphSize.X + TextSize.X, GlyphSize.Y + TextSize.Y);
      if Layout in [blGlyphLeft, blGlyphRight] then
        Margin := (ClientSize.X - TotalSize.X) div 3
      else
        Margin := (ClientSize.Y - TotalSize.Y) div 3;
      Spacing := Margin;
    end
    else
    begin
      TotalSize := Point(GlyphSize.X + Spacing + TextSize.X, GlyphSize.Y +
        Spacing + TextSize.Y);
      if Layout in [blGlyphLeft, blGlyphRight] then
        Margin := (ClientSize.X - TotalSize.X + 1) div 2
      else
        Margin := (ClientSize.Y - TotalSize.Y + 1) div 2;
    end;
  end
  else
  begin
    if Spacing = -1 then
    begin
      TotalSize := Point(ClientSize.X - (Margin + GlyphSize.X), ClientSize.Y -
        (Margin + GlyphSize.Y));
      if Layout in [blGlyphLeft, blGlyphRight] then
        Spacing := (TotalSize.X - TextSize.X) div 2
      else
        Spacing := (TotalSize.Y - TextSize.Y) div 2;
    end;
  end;
    
  case Layout of
    blGlyphLeft:
      begin
        GlyphPos.X := Margin;
        TextPos.X := GlyphPos.X + GlyphSize.X + Spacing;
      end;
    blGlyphRight:
      begin
        GlyphPos.X := ClientSize.X - Margin - GlyphSize.X;
        TextPos.X := GlyphPos.X - Spacing - TextSize.X;
      end;
    blGlyphTop:
      begin
        GlyphPos.Y := Margin;
        TextPos.Y := GlyphPos.Y + GlyphSize.Y + Spacing;
      end;
    blGlyphBottom:
      begin
        GlyphPos.Y := ClientSize.Y - Margin - GlyphSize.Y;
        TextPos.Y := GlyphPos.Y - Spacing - TextSize.Y;
      end;
  end;

  Ofs := Offset;

  // fixup the result variables
  with GlyphPos do
  begin
    Inc(X, Client.Left + Ofs.X);
    Inc(Y, Client.Top + Ofs.Y);
  end;

  OffsetRect(TextBounds, TextPos.X + Client.Left + Offset.X,
    TextPos.Y + Client.Top + Offset.X);
end;

function TAdvGridButton.DrawButton(Canvas: TCanvas; const Client: TRect;
  const Offset: TPoint; const Caption: string; Layout: TButtonLayout;
  Margin, Spacing: Integer; State: TButtonState; Transparent: Boolean;
  BiDiFlags: LongInt): TRect;
var
  GlyphPos: TPoint;
begin
  CalcButtonLayout(Canvas, Client, Offset, Caption, Layout, Margin, Spacing,
    GlyphPos, Result, BiDiFlags);

  if Caption = '' then
  begin
    GlyphPos.X := ((Client.Right - Client.Left)- Glyph.Width) div 2;
    GlyphPos.Y := ((Client.Bottom - Client.Top) - Glyph.Height) div 2;
  end;

  DrawButtonGlyph(Canvas, GlyphPos, State, Transparent);

  if not (State in [bsDown, bsExclusive]) then
  begin
    if FMouseInControl and Shaded and Enabled and not (State = bsDown) and Flat and not (csDesigning in ComponentState) then
    begin
      if Shaded then
        OffsetRect(Result, +1 , +1)
      else
        OffsetRect(Result, 0, +1);
    end;
  end;

  if ShowCaption then
    DrawButtonText(Canvas, Caption, Result, State, BiDiFlags);
end;


procedure TAdvGridButton.Paint;
const
  DownStyles: array[Boolean] of Integer = (BDR_RAISEDINNER, BDR_SUNKENOUTER);
  FillStyles: array[Boolean] of Integer = (BF_MIDDLE, 0);
  
var
  PaintRect: TRect;
  DrawFlags: Integer;
  Offset: TPoint;
  PColorTo: TColor;
  mid: Integer;
  dotheme: boolean;
  HTheme: THandle;

begin
  if not Enabled then
  begin
    FState := bsDisabled;
    FDragging := False;
  end
  else
  begin
    if (FState = bsDisabled) then
      if FDown and (GroupIndex <> 0) then
        FState := bsExclusive
      else
        FState := bsUp;
  end;

  if (Style = tasCheck) and (Down) then
  begin
    FState := bsDown;
  end;

  Canvas.Font := Self.Font;
  PaintRect := Rect(0, 0, Width, Height);

  if not FFlat then
  begin
    dotheme := false;

    if IsWinXP and AutoXPStyle and FIsComCtl6 then
      dotheme := IsThemeActive;

    if dotheme then
    begin
      HTHeme := OpenThemeData(Parent.Handle,'button');

      if FState in [bsDown,bsExclusive] then
        DrawThemeBackground(HTheme,Canvas.Handle, BP_PUSHBUTTON,PBS_PRESSED,@paintrect,nil)
      else

      if FMouseInControl then
      begin
        DrawThemeBackground(HTheme,Canvas.Handle, BP_PUSHBUTTON,PBS_HOT,@paintrect,nil)
      end
      else
        DrawThemeBackground(HTheme,Canvas.Handle, BP_PUSHBUTTON,PBS_NORMAL,@paintrect,nil);

      CloseThemeData(HTHeme);

    end
    else
    begin
      DrawFlags := DFCS_BUTTONPUSH or DFCS_ADJUSTRECT;
      if FState in [bsDown, bsExclusive] then
        DrawFlags := DrawFlags or DFCS_PUSHED;
      DrawFrameControl(Canvas.Handle, PaintRect, DFC_BUTTON, DrawFlags);
    end;
  end
  else
  begin
    if ((FState in [bsDown, bsExclusive]) or
      (FMouseInControl and (FState <> bsDisabled)) or
      (csDesigning in ComponentState)) and not Rounded and not Flat and (BorderDownColor = clNone) then
        DrawEdge(Canvas.Handle, PaintRect, DownStyles[FState in [bsDown, bsExclusive]],
          FillStyles[FTransparent] or BF_RECT);

      if {(csDesigning in ComponentState) or} (FBorderDownColor = clNone) or Rounded then
        InflateRect(PaintRect,-1,-1);

      if FMouseInControl and Enabled and not (csDesigning in ComponentState) then
      begin
        if (FState in [bsDown]) then
        begin
          Canvas.Brush.Color := ColorDown;
          PColorTo := ColorDownTo;
          Canvas.Pen.Color := FBorderDownColor;
        end
        else
        begin
          Canvas.Brush.Color := ColorHot;
          PColorTo := ColorHotTo;
          Canvas.Pen.Color := FBorderHotColor;
        end;

        if (Style = tasCheck) and Down and (FState <> bsDown) then
        begin
          Canvas.Pen.Color := FBorderDownColor;
          Canvas.Pen.Width := 1;
          Canvas.Brush.Color := ColorChecked;
          PColorTo := ColorCheckedTo;
          Canvas.Pen.Color := FBorderDownColor;
        end;

        Canvas.Pen.Width := 1;

        if Rounded then
          Canvas.Pen.Color := clNone;

        if PColorTo <> clNone then
        begin
          DrawGradient(Canvas, Canvas.Brush.Color, PColorTo, Canvas.Pen.Color, 16, PaintRect, False);
        end
        else
        begin
          if Canvas.Pen.Color = clNone then
            Canvas.Pen.Color := Canvas.Brush.Color;
          Canvas.Rectangle(PaintRect.Left,PaintRect.Top,PaintRect.Right,PaintRect.Bottom);
        end;

        if Rounded then
        begin
          InflateRect(PaintRect, +1, +1);
          if (FState in [bsDown]) then
            Canvas.Pen.Color := FBorderDownColor
          else
            Canvas.Pen.Color := FBorderHotColor;
          Canvas.Brush.Style := bsClear;
          Canvas.RoundRect(PaintRect.Left,PaintRect.Top,PaintRect.Right,PaintRect.Bottom,8,8);
        end;

        if FDropDownButton then
        begin
          if FState = bsDown then
            Canvas.Pen.COlor := FBorderDownColor
          else
            Canvas.Pen.COlor := FBorderHotColor;

          Canvas.MoveTo(PaintRect.Right - 12, PaintRect.Top);
          Canvas.LineTo(PaintRect.Right - 12, PaintRect.Bottom);
        end;
      end
      else
      begin
        Canvas.Pen.Width := 1;

        if (Style = tasCheck) and Down then
        begin
          Canvas.Pen.Color := FBorderDownColor;
          Canvas.Pen.Width := 1;
          Canvas.Brush.Color := ColorChecked;

          if ColorCheckedTo <> clNone then
            DrawGradient(Canvas, Canvas.Brush.Color, ColorCheckedTo, Canvas.Pen.Color, 16, PaintRect, False)
          else
          begin
            if FBorderDownColor = clNone then
              Canvas.Pen.Color := Canvas.Brush.Color;

            Canvas.Rectangle(PaintRect.Left,PaintRect.Top,PaintRect.Right,PaintRect.Bottom);
          end;
        end
        else
        begin
          Canvas.Brush.Color := ColorToRGB(Color);
          if ColorTo <> clNone then
          begin
            if Down then
              Canvas.Pen.Color := BorderDownColor
            else
              Canvas.Pen.Color := BorderColor;

            if Rounded then
              Canvas.Pen.Color := clNone;

            DrawGradient(Canvas, Color, ColorTo, Canvas.Pen.Color, 16, PaintRect, False);
          end
          else
            Canvas.FillRect(PaintRect);

          if (FBorderColor <> clNone) then
          begin
            if Rounded then
            begin
              InflateRect(PaintRect, +1, +1);
              Canvas.Pen.Color := FBorderColor;
              Canvas.Pen.Width := 1;
              Canvas.RoundRect(PaintRect.Left,PaintRect.Top,PaintRect.Right,PaintRect.Bottom,8,8);
            end
            else
            begin
              Canvas.Pen.Color := FBorderColor;
              Canvas.Brush.Style := bsClear;
              Canvas.Rectangle(PaintRect.Left, PaintRect.Top, PaintRect.Right, PaintRect.Bottom);
            end;
          end;

        end;
      end;

    InflateRect(PaintRect, -1, -1);
  end;

  Offset := Point(0,0);

  if FState in [bsDown, bsExclusive] then
  begin
    if (FState = bsExclusive) and (not FFlat or not FMouseInControl) then
    begin
      Canvas.Brush.Color := ColorChecked;
      PColorTo := ColorCheckedTo;

      if Down then
        Canvas.Pen.Color := BorderDownColor
      else
        Canvas.Pen.Color := BorderColor;

      InflateRect(PaintRect, +1, +1);
      if PColorTo <> clNone then
      begin
        DrawGradient(Canvas, ColorChecked, ColorCheckedTo, Canvas.Pen.Color, 16, PaintRect, False)
      end
      else
        Canvas.FillRect(PaintRect);
    end;
    if not FFlat and not (csDesigning in ComponentState) then
      Offset := Point(1,1);
  end
  else
  begin
    if FFlat then
    begin
      if FMouseInControl and Enabled and Shaded and not (FState = bsDown) and not (csDesigning in ComponentState) then
        Offset := Point(-1,-1)
    end;
  end;

  if FDropDownButton then
  begin
    mid := PaintRect.Top + (PaintRect.Bottom - PaintRect.Top) div 2;
    Canvas.Brush.Color := Font.Color;
    Canvas.Pen.Color := Font.Color;
    Canvas.Polygon([Point(PaintRect.Right -8, Mid -1),Point(PaintRect.Right - 4, Mid -1),Point(PaintRect.Right - 6, Mid + 1)]);
    PaintRect.Right := PaintRect.Right - 12;
  end;

  if FDown and (GroupIndex <> 0) then
    DrawButton(Canvas, PaintRect, Offset, Caption, FLayout, FMargin,
      FSpacing, bsDown, FTransparent, DrawTextBiDiModeFlags(0))
  else
    DrawButton(Canvas, PaintRect, Offset, Caption, FLayout, FMargin,
      FSpacing, FState, FTransparent, DrawTextBiDiModeFlags(0));
end;

procedure TAdvGridButton.UpdateTracking;
var
  P: TPoint;
begin
  if FFlat then
  begin
    if Enabled then
    begin
      GetCursorPos(P);
      FMouseInControl := not (FindDragTarget(P, True) = Self);
      if FMouseInControl then
        Perform(CM_MOUSELEAVE, 0, 0)
      else
        Perform(CM_MOUSEENTER, 0, 0);
    end;
  end;
end;
    
procedure TAdvGridButton.Loaded;
begin
  inherited Loaded;

  if FShaded then
    GenerateShade;

  if AutoThemeAdapt then
    ThemeAdapt;  
end;

procedure TAdvGridButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  pt: TPoint;
begin
  inherited MouseDown(Button, Shift, X, Y);

  if (Button = mbLeft) and Enabled then
  begin
    if (FDropDownButton) and (X > ClientRect.Right - 12) then
    begin
      FState := bsUp;
      //FMouseInControl := False;
      //Repaint;
      if Assigned(FOnDropDown) then
        FOnDropDown(Self);

      if Assigned(FDropDownMenu) then
      begin
        pt := Point(Left, Top + Height);
        pt := Parent.ClientToScreen(pt);
        FDropDownMenu.Popup(pt.X,pt.Y);
      end;

      Exit;
    end;

    if not FDown then
    begin
      FState := bsDown;
      Invalidate;
    end;
    if Style = tasCheck then
    begin
      FState := bsDown;
      Repaint;
    end;

    FDragging := True;
  end;
end;

procedure TAdvGridButton.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  NewState: TButtonState;
begin
  inherited MouseMove(Shift, X, Y);
  if FDragging then
  begin
    if (not FDown) then NewState := bsUp
    else NewState := bsExclusive;

    if (X >= 0) and (X < ClientWidth) and (Y >= 0) and (Y <= ClientHeight) then
      if FDown then NewState := bsExclusive else NewState := bsDown;

    if (Style = tasCheck) and FDown then
    begin
      NewState := bsDown;
    end;

    if NewState <> FState then
    begin
      FState := NewState;
      Invalidate;
    end;
  end
  else
  if not FMouseInControl then
    UpdateTracking;
end;
    
procedure TAdvGridButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  DoClick: Boolean;
begin
  inherited MouseUp(Button, Shift, X, Y);

  if FDragging then
  begin
    FDragging := False;
    DoClick := (X >= 0) and (X < ClientWidth) and (Y >= 0) and (Y <= ClientHeight);
    if FGroupIndex = 0 then
    begin
      // Redraw face in-case mouse is captured
      FState := bsUp;
      //FMouseInControl := False;

      if Style = tasCheck then
      begin
        SetDown(not FDown);
        FState := bsUp;
      end;

      if DoClick and not (FState in [bsExclusive, bsDown]) then
        Invalidate;
    end
    else
      if DoClick then
      begin
        SetDown(not FDown);
        if FDown then Repaint;
      end
      else
      begin
        if FDown then
          FState := bsExclusive;
        Repaint;
      end;
    if DoClick then Click;
    UpdateTracking;
  end;
end;
    
procedure TAdvGridButton.Click;
begin
  inherited Click;
end;
    

{$IFDEF DELPHI6_LVL}
function TAdvGridButton.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TAdvGridButtonActionLink;
end;
{$ENDIF}

procedure TAdvGridButton.SetGlyph(Value: TBitmap);
var
  x,y: Integer;
  PxlColor: TColor;
  c: byte;
begin
  FGlyph.Assign(Value);
  //if no disabled glyph is given... add this automatically...
  if FGlyphDisabled.Empty then
  begin
    FGlyphDisabled.Assign(Value);
    for x := 0 to FGlyphDisabled.Width - 1 do
      for y := 0 to FGlyphDisabled.Height - 1 do
      begin
        PxlColor := ColorToRGB(FGlyphDisabled.Canvas.Pixels[x, y]);
        c := Round((((PxlColor shr 16) + ((PxlColor shr 8) and $00FF) +
               (PxlColor and $0000FF)) div 3)) div 2 + 96;
        FGlyphDisabled.Canvas.Pixels[x, y] := RGB(c, c, c);
      end;
  end;
  Invalidate;
end;
    
procedure TAdvGridButton.GlyphChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TAdvGridButton.UpdateExclusive;
var
  Msg: TMessage;
begin
  if (FGroupIndex <> 0) and (Parent <> nil) then
  begin
    Msg.Msg := CM_BUTTONPRESSED;
    Msg.WParam := FGroupIndex;
    Msg.LParam := LParam(Self);
    Msg.Result := 0;
    Parent.Broadcast(Msg);
  end;
end;


procedure TAdvGridButton.SetDown(Value: Boolean);
begin
  if (FGroupIndex = 0) and (Style = tasButton) then
    Value := False;

  if (Style = tasCheck) then
  begin
    FDown := Value;
    //FState := bsDown;
    Repaint;
    Exit;
  end;

  if Value <> FDown then
  begin
    if FDown and (not FAllowAllUp) then Exit;
    FDown := Value;
    if Value then
    begin
      if FState = bsUp then Invalidate;
      FState := bsExclusive
    end
    else
    begin
      FState := bsUp;
      Repaint;
    end;
    if Value then UpdateExclusive;
  end;
end;

procedure TAdvGridButton.SetFlat(Value: Boolean);
begin
  if Value <> FFlat then
  begin
    FFlat := Value;
    Invalidate;
  end;
end;
    
procedure TAdvGridButton.SetGroupIndex(Value: Integer);
begin
  if FGroupIndex <> Value then
  begin
    FGroupIndex := Value;
    UpdateExclusive;
  end;
end;
    
procedure TAdvGridButton.SetLayout(Value: TButtonLayout);
begin
  if FLayout <> Value then
  begin
    FLayout := Value;
    Invalidate;
  end;
end;
    
procedure TAdvGridButton.SetMargin(Value: Integer);
begin
  if (Value <> FMargin) and (Value >= -1) then
  begin
    FMargin := Value;
    Invalidate;
  end;
end;
    
procedure TAdvGridButton.SetSpacing(Value: Integer);
begin
  if Value <> FSpacing then
  begin
    FSpacing := Value;
    Invalidate;
  end;
end;

procedure TAdvGridButton.SetAllowAllUp(Value: Boolean);
begin
  if FAllowAllUp <> Value then
  begin
    FAllowAllUp := Value;
    UpdateExclusive;
  end;
end;
    
procedure TAdvGridButton.WMLButtonDblClk(var Message: TWMLButtonDown);
begin
  inherited;
  if FDown then DblClick;
end;
    
procedure TAdvGridButton.CMEnabledChanged(var Message: TMessage);
const
  NewState: array[Boolean] of TButtonState = (bsDisabled, bsUp);
begin
  UpdateTracking;
  Repaint;
end;

procedure TAdvGridButton.CMButtonPressed(var Message: TMessage);
var
  Sender: TAdvGridButton;
begin
  if integer(Message.WParam) = FGroupIndex then
  begin
    Sender := TAdvGridButton(Message.LParam);
    if Sender <> Self then
    begin
      if Sender.Down and FDown then
      begin
        FDown := False;
        FState := bsUp;
        if (Action is TCustomAction) then
          TCustomAction(Action).Checked := False;
        Invalidate;
      end;
      FAllowAllUp := Sender.AllowAllUp;
    end;
  end;
end;

procedure TAdvGridButton.CMDialogChar(var Message: TCMDialogChar);
begin
  with Message do
    if IsAccel(CharCode, Caption) and Enabled and Visible and
      (Parent <> nil) and Parent.Showing then
    begin
      Click;
      Result := 1;
    end else
      inherited;
end;
    
procedure TAdvGridButton.CMFontChanged(var Message: TMessage);
begin
  Invalidate;
end;
    
procedure TAdvGridButton.CMTextChanged(var Message: TMessage);
begin
  Invalidate;
end;
    
procedure TAdvGridButton.CMSysColorChange(var Message: TMessage);
begin
  with TBitmap(FGlyph) do
  begin
    Invalidate;
  end;
end;
    
procedure TAdvGridButton.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  { Don't draw a border if DragMode <> dmAutomatic since this button is meant to
    be used as a dock client. }
  if {FFlat and} not FMouseInControl and Enabled and (DragMode <> dmAutomatic)
    and (GetCapture = 0) then
  begin
    FMouseInControl := True;
    Repaint;
  end;

  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TAdvGridButton.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if {FFlat and} FMouseInControl and Enabled and not FDragging then
  begin
    FMouseInControl := False;
    Invalidate;
  end;

  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

procedure TAdvGridButton.SetGlyphDisabled(const Value: TBitmap);
begin
  FGlyphDisabled.Assign(Value);
end;

procedure TAdvGridButton.SetGlyphDown(const Value: TBitmap);
begin
  FGlyphDown.Assign(Value);
end;

procedure TAdvGridButton.SetGlyphHot(const Value: TBitmap);
begin
  FGlyphHot.Assign(Value);
end;

procedure TAdvGridButton.GenerateShade;
var
  r: TRect;
  bmp: TBitmap;
begin
  if not FGlyph.Empty then
  begin
    FGlyphShade.Width := FGlyph.Width;
    FGlyphShade.Height := FGlyph.Height;

    r := Rect(0,0,FGlyphShade.Width,FGlyphShade.Height);
    FGlyphShade.Canvas.Brush.Color := ColorToRGB(clBlack);
    FGlyphShade.Canvas.BrushCopy(r,FGlyph,r, FGlyph.Canvas.Pixels[0,FGlyph.Height-1]);
    FGlyphShade.Canvas.CopyMode := cmSrcInvert;
    FGlyphShade.Canvas.CopyRect(r,FGlyph.Canvas,r);

    bmp := TBitmap.Create;
    bmp.Width := FGlyph.Width;
    bmp.Height := FGlyph.Height;
    bmp.Canvas.Brush.Color := ColorToRGB(clGray);
    bmp.Canvas.BrushCopy(r,FGlyphShade,r,ColorToRGB(clBlack));

    FGlyphShade.Canvas.CopyMode := cmSrcCopy;
    FGlyphShade.Canvas.CopyRect(r,bmp.Canvas,r);
    bmp.Free;
  end;
end;

procedure TAdvGridButton.SetShowCaption(const Value: Boolean);
begin
  if (FShowCaption <> Value) then
  begin
    FShowCaption := Value;
    Invalidate;
  end;
end;

procedure TAdvGridButton.SetShaded(const Value: Boolean);
begin
  FShaded := Value;

  if FShaded then
    if not (csLoading in ComponentState) then
    begin
      GenerateShade;
    end;
end;

procedure TAdvGridButton.SetColorTo(const Value: TColor);
begin
  FColorTo := Value;
  Invalidate;
end;

procedure TAdvGridButton.SetColorChecked(const Value: TColor);
begin
  FColorChecked := Value;
  Invalidate;
end;

procedure TAdvGridButton.SetStyle(const Value: TAdvGridButtonStyle);
begin
  FStyle := Value;
  Invalidate;
end;

procedure TAdvGridButton.SetColorCheckedTo(const Value: TColor);
begin
  FColorCheckedTo := Value;
  Invalidate;
end;

procedure TAdvGridButton.SetRounded(const Value: Boolean);
begin
  FRounded := Value;
  Invalidate;
end;

procedure TAdvGridButton.SetDropDownButton(const Value: Boolean);
begin
  FDropDownButton := Value;
  Invalidate;
end;

procedure TAdvGridButton.SetBorderColor(const Value: TColor);
begin
  FBorderColor := Value;
  Invalidate;
end;


procedure TAdvGridButton.WndProc(var Message: TMessage);
begin
  // message does not seem to get through always?
  if (Message.Msg = WM_THEMECHANGED) and AutoThemeAdapt then
  begin
    ThemeAdapt;
  end;

  if (Message.Msg = CM_SYSFONTCHANGED) and AutoThemeAdapt then
  begin
    ThemeAdapt;
  end;
  
  inherited;
end;

procedure TAdvGridButton.ThemeAdapt;
var
  eTheme: XPColorScheme;
begin
  eTheme := CurrentXPTheme();
  case eTheme of
    xpBlue: Look := 2;
    xpGreen: Look := 3;
    xpGray: Look := 4;
  else
    Look := 1;
  end;
end;

procedure TAdvGridButton.SetLook(const Value: Integer);
begin
  case Value of
  // Windows XP
  0:begin
      self.Color := $EDF1F1;
      self.ColorTo := $DFEBEB;
      self.ColorHot := $FAFCFC;
      self.ColorHotTo := $E5ECED;
      self.ColorDown := $E0E6E7;
      self.ColorDownTo := $D8E0E1;
      self.ColorChecked := $FFFFFF;
      self.ColorCheckedTo := clNone;
      self.BorderDownColor := $AF987A;
      self.BorderHotColor := $C3CECE;
      self.BorderColor := clNone;
      self.Rounded := True;
      self.Flat := True;
    end;
  // Office 2002
  1:begin
      self.Color := clBtnFace;
      self.ColorTo := clNone;
      self.ColorHot := $EED2C1;
      self.ColorHotTo := clNone;
      self.ColorDown := $E2B598;
      self.ColorDownTo := clNone;
      self.ColorChecked := $E8E6E1;
      self.ColorCheckedTo := clNone;
      self.BorderDownColor := $C56A31;
      self.BorderHotColor := $C56A31;
      self.BorderColor := clNone;
      self.Rounded := False;
      self.Flat := True;
    end;
  // XP (Blue)
  2:begin
      self.Color := $FDEADA;
      self.ColorTo := $E4AE88;
      self.ColorHot := $CCF4FF;
      self.ColorHotTo := $91D0FF;
      self.ColorDown := $4E91FE;
      self.ColorDownTo := $8ED3FF;
      self.ColorChecked := $8ED3FF;
      self.ColorCheckedTo := $55ADFF;
      self.BorderDownColor := clBlack;
      self.BorderHotColor := clBlack;
      self.BorderColor := clNone;
      self.Rounded := False;
      self.Flat := True;
    end;
  // XP (Olive)
  3:begin
      self.Color := $CFF0EA;
      self.ColorTo := $8CC0B1;
      self.ColorHot := $CCF4FF;
      self.ColorHotTo := $91D0FF;
      self.ColorDown := $4E91FE;
      self.ColorDownTo := $8ED3FF;
      self.ColorChecked := $8ED3FF;
      self.ColorCheckedTo := $55ADFF;
      self.BorderDownColor := clBlack;
      self.BorderHotColor := clBlack;
      self.BorderColor := clNone;
      self.Rounded := False;
      self.Flat := True;
    end;
  // XP (Silver)
  4:begin
      self.Color := $ECE2E1;
      self.ColorTo := $B39698;
      self.ColorHot := $CCF4FF;
      self.ColorHotTo := $91D0FF;
      self.ColorDown := $4E91FE;
      self.ColorDownTo := $8ED3FF;
      self.ColorChecked := $8ED3FF;
      self.ColorCheckedTo := $55ADFF;
      self.BorderDownColor := clBlack;
      self.BorderHotColor := clBlack;
      self.BorderColor := clNone;
      self.Rounded := False;
      self.Flat := True;
    end;
  // Flat style
  5:begin
      self.Color := clBtnFace;
      self.ColorTo := clNone;
      self.ColorHot := clBtnFace;
      self.ColorHotTo := clNone;
      self.ColorDown := $00D8D3D2;
      self.ColorDownTo := clNone;
      self.ColorChecked := $00CAC7C7;
      self.ColorCheckedTo := clNone;
      self.BorderDownColor := clNone;
      self.BorderHotColor := clNone;
      self.BorderColor := clNone;
      self.Rounded := false;
      self.Flat := True;
    end;
  // Avant garde
  6:begin
      self.Color := $00CAFFFF;
      self.ColorTo := $00A6FFFF;
      self.ColorHot := $00A8F0FD;
      self.ColorHotTo := $007CE9FC;
      self.ColorDown := $004DE0FB;
      self.ColorDownTo := $007AE9FC;
      self.ColorChecked := $00B5E6F2;
      self.ColorCheckedTo := $009CDDED;
      self.BorderDownColor := clGray;
      self.BorderHotColor := clGray;
      self.BorderColor := clNone;
      self.Rounded := false;
      self.Flat := True;
    end;
  end;
end;

procedure TAdvGridButton.SetAutoThemeAdapt(const Value: Boolean);
begin
  FAutoThemeAdapt := Value;

//  if not (csDesigning in ComponentState) then
//  begin
    if FAutoThemeAdapt then
      ThemeAdapt;
//  end;
end;


function TAdvGridButton.GetColor: TColor;
begin
  Result := inherited Color;
end;

procedure TAdvGridButton.SetColor(const Value: TColor);
begin
  inherited Color := Value;
end;


procedure TAdvGridButton.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;
  if (AOperation = opRemove) and (AComponent = FDropDownMenu) then
    FDropDownMenu := nil;
end;


{$IFDEF DELPHI6_LVL}

{ TAdvGridButtonActionLink }

procedure TAdvGridButtonActionLink.AssignClient(AClient: TObject);
begin
  inherited AssignClient(AClient);
  FClient := AClient as TAdvGridButton;
end;

function TAdvGridButtonActionLink.IsCaptionLinked: Boolean;
begin
  Result := inherited IsCaptionLinked and
    (FClient.Caption = (Action as TCustomAction).Caption);
end;

function TAdvGridButtonActionLink.IsCheckedLinked: Boolean;
begin
  Result := inherited IsCheckedLinked and (FClient.GroupIndex <> 0) and
    FClient.AllowAllUp and (FClient.Down = (Action as TCustomAction).Checked);
end;

function TAdvGridButtonActionLink.IsGroupIndexLinked: Boolean;
begin
  Result := (FClient is TAdvGridButton) and
    (TAdvGridButton(FClient).GroupIndex = (Action as TCustomAction).GroupIndex);
end;

procedure TAdvGridButtonActionLink.SetCaption(const Value: string);
begin
  if IsCaptionLinked then
    TAdvGridButton(FClient).Caption := Value;
end;

procedure TAdvGridButtonActionLink.SetChecked(Value: Boolean);
begin
  if IsCheckedLinked then
    TAdvGridButton(FClient).Down := Value;
end;

procedure TAdvGridButtonActionLink.SetGroupIndex(Value: Integer);
begin
  if IsGroupIndexLinked then
    TAdvGridButton(FClient).GroupIndex := Value;
end;

{$ENDIF}

procedure TFileStringList.Reset;
begin
  fp := 0;
  cache := '';
end;

function TFileStringList.GetEOF;
begin
  Result := fp >= Count;
end;

procedure TFileStringList.ReadLn(var s: string);
begin
  s := Strings[fp];
  inc(fp);
end;

procedure TFileStringList.Write(s: string);
begin
  cache := cache + s;
end;

procedure TFileStringList.WriteLn(s: string);
begin
  Add(cache + s);
  cache := '';
end;

{ TAdvCheckBox }

constructor TAdvCheckBox.Create(AOwner: TComponent);
var
  VerInfo: TOSVersioninfo;

begin
  inherited Create(AOwner);
  Width := 98;
  Height := 20;
  FBtnVAlign := tlTop;
  FCaption := self.ClassName;

  VerInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);

  GetVersionEx(verinfo);

  FIsWinXP := (verinfo.dwMajorVersion > 5) OR
    ((verinfo.dwMajorVersion = 5) AND (verinfo.dwMinorVersion >= 1));

  ControlStyle := ControlStyle - [csClickEvents];
  FReadOnly := False;

  FBkgBmp := TBitmap.Create;
  FBkgCache := false;
  FTransparentCaching := false;
end;

procedure TAdvCheckBox.SetTransparent(Value: Boolean);
begin
  if Value <> Ftransparent then
  begin
    Ftransparent := Value;
    FBkgCache := false;
  end;
  Repaint;
end;

procedure TAdvCheckBox.DrawParentImage(Control: TControl; Dest: TCanvas);
var
  SaveIndex: Integer;
  DC: HDC;
  Position: TPoint;
begin
  with Control do
  begin
    if Parent = nil then
      Exit;
    DC := Dest.Handle;
    SaveIndex := SaveDC(DC);
    GetViewportOrgEx(DC, Position);
    SetViewportOrgEx(DC, Position.X - Left, Position.Y - Top, nil);
    IntersectClipRect(DC, 0, 0, Parent.ClientWidth, Parent.ClientHeight);

    Parent.Perform(WM_ERASEBKGND, Integer(DC), Integer(0));
    Parent.Perform(WM_PAINT, Integer(DC), Integer(0));
    RestoreDC(DC, SaveIndex);
  end;
end;

procedure TAdvCheckBox.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
  if FTransparent then
  begin
    FBkgCache := false;
    Repaint;
  end;
end;

procedure TAdvCheckBox.Paint;
var
  R: TRect;
  flg: longint;
  ExtraBW: Integer;
  HTheme: THandle;
  DTSTYLE: DWORD;
  BT,BL: Integer;

begin
  Canvas.Font := Font;
  ExtraBW := 5;
  BT := 3;

  if FTransparent then
  begin
    if FTransparentCaching then
    begin
      if FBkgCache then
      begin
        Self.Canvas.Draw(0,0,FBkgBmp)
      end
      else
      begin
        FBkgBmp.Width := self.Width;
        FBkgBmp.Height := self.Height;
        DrawParentImage(Self, FBkgBmp.Canvas);
        Self.Canvas.Draw(0,0,FBkgBmp);
        FBkgCache := true;
      end;
    end
    else
      DrawParentImage(Self, self.Canvas);
  end;

  with Canvas do
  begin
    Text := Caption;
    if DoThemeDrawing then
    begin
      case FBtnVAlign of
      tlTop: BT := 4;
      tlCenter: BT := (ClientRect.Bottom - ClientRect.Top) div 2 - 6;
      tlBottom: BT := ClientRect.Bottom - 14;
      end;
      if FAlignment = taRightJustify then
        BL := ClientRect.Right - 15
      else
        BL := 0;

      HTheme := OpenThemeData(Self.Handle,'button');
      r := Rect(BL, BT, BL + 14, BT + 14);
      if State = cbChecked then
      begin
        if Enabled then
        begin
          if Down then
            DrawThemeBackground(HTheme,Canvas.Handle, BP_CHECKBOX,CBS_CHECKEDPRESSED,@r,nil)
          else
          if FHot then
            DrawThemeBackground(HTheme,Canvas.Handle, BP_CHECKBOX,CBS_CHECKEDHOT,@r,nil)
          else
            DrawThemeBackground(HTheme,Canvas.Handle, BP_CHECKBOX,CBS_CHECKEDNORMAL,@r,nil);
        end
        else
          DrawThemeBackground(HTheme,Canvas.Handle, BP_CHECKBOX,CBS_CHECKEDDISABLED,@r,nil);
        end
        else
          if State = cbGrayed then
          begin
            if Enabled then
            begin
              if Down then
                DrawThemeBackground(HTheme,Canvas.Handle, BP_CHECKBOX,CBS_MIXEDPRESSED,@r,nil)
              else
              if FHot then
                DrawThemeBackground(HTheme,Canvas.Handle, BP_CHECKBOX,CBS_MIXEDHOT,@r,nil)
              else
                DrawThemeBackground(HTheme,Canvas.Handle, BP_CHECKBOX,CBS_MIXEDNORMAL,@r,nil);
            end
            else
              DrawThemeBackground(HTheme,Canvas.Handle, BP_CHECKBOX,CBS_MIXEDDISABLED,@r,nil);
          end
          else
          begin
            if Enabled then
            begin
              if Down then
                DrawThemeBackground(HTheme,Canvas.Handle, BP_CHECKBOX,CBS_UNCHECKEDPRESSED,@r,nil)
              else

              if FHot then
                DrawThemeBackground(HTheme,Canvas.Handle, BP_CHECKBOX,CBS_UNCHECKEDHOT,@r,nil)
              else
                DrawThemeBackground(HTheme,Canvas.Handle, BP_CHECKBOX,CBS_UNCHECKEDNORMAL,@r,nil)
            end
            else
              DrawThemeBackground(HTheme,Canvas.Handle, BP_CHECKBOX,CBS_UNCHECKEDDISABLED,@r,nil);
          end;

          CloseThemeData(HTheme);
      end
    else
      begin
        if fAlignment = taRightJustify then
          r.Left := ClientRect.Right - 13
        else
          r.left := 0;
        r.Right := r.Left + 13;
      	case FBtnVAlign of
        tlTop: r.Top := 4;
        tlCenter: r.Top := (ClientRect.Bottom-ClientRect.Top) div 2 - 6;
        tlBottom: r.Top := ClientRect.Bottom - 17;
        end;
        r.Bottom := r.Top + 13;
        flg:=0;
        if not Enabled then flg := flg or DFCS_INACTIVE;
        if State = cbGrayed then flg := flg or DFCS_INACTIVE;
        if State = cbChecked then flg := flg or DFCS_CHECKED;
        DrawFrameControl(Canvas.handle,r,DFC_BUTTON,DFCS_BUTTONCHECK or flg);
      end;

    R := GetClientRect;

    if FAlignment = taRightJustify then
    begin
      r.Left := 0;
      r.Right := r.Right - 4 - ExtraBW;
    end
    else
      r.left := r.left + 4 + ExtraBW;

    r.Top := r.Top + 4;
    r.Left := r.Left + 6;

    Canvas.Brush.Style := bsClear;
    DTSTYLE := DT_LEFT;

    if Ellipsis then
      DTSTYLE := DTSTYLE or DT_END_ELLIPSIS;

    if not Enabled then
    begin
      OffsetRect(r,1,1);
      Canvas.Font.Color := clWhite;
      DrawText(Canvas.Handle, pchar(Text),Length(Text),R, DTSTYLE);
      Canvas.Font.Color := clGray;
      Offsetrect(r,-1,-1);
      DrawText(Canvas.Handle, pchar(Text),Length(Text),R, DTSTYLE);
   end
    else
      DrawText(Canvas.Handle, pchar(Text),Length(Text),R, DTSTYLE);

    if FFocused then
    begin
      Canvas.Brush.Style := bsSolid;
      r.right := r.left + Canvas.TextWidth(Text) + 1;
      r.bottom := r.top + Canvas.TextHeight(Text) + 1;
      r.Left := r.Left - 2;
      DrawFocusRect(R);
    end;
  end;
end;

procedure TAdvCheckBox.SetDown(Value:Boolean);
begin
  if FDown <> Value then
  begin
    FDown := Value;
  end;
end;

procedure TAdvCheckBox.SetState(Value:TCheckBoxState);
var
  r: TRect;
begin
  if FState <> Value then
  begin
    FState := Value;
    r := GetClientRect;
    case Alignment of
    taLeftJustify: r.Right := 20;
    taRightJustify: r.Left := r.Right - 20;
    end;
    if HandleAllocated then
    begin
      InvalidateRect(self.Handle,@r,True);
    end;
  end;
end;

function TAdvCheckBox.GetChecked: Boolean;
begin
  Result := State = cbChecked;
end;

procedure TAdvCheckBox.SetChecked(Value:Boolean);
begin
  if Value then
    State := cbChecked
  else
    State := cbUnchecked;
  Invalidate;
end;

procedure TAdvCheckBox.DoEnter;
begin
  inherited DoEnter;
  FFocused := True;
  Invalidate;
end;

procedure TAdvCheckBox.DoExit;
begin
  inherited DoExit;
  FFocused := False;
  Invalidate;
end;

procedure TAdvCheckBox.MouseDown(Button: TMouseButton; Shift: TShiftState;X, Y: Integer);
var
  Anchor:string;
  R: TRect;
begin
  Anchor := '';
  FMouseDown := true;

  if not FFocused then
  begin
    SetFocus;
    FFocused := True;
  end;

  inherited MouseDown(Button, Shift, X, Y);
  MouseCapture := True;
  Down := True;

  if FIsWinXP then
  begin
    R := Rect(0,0,16,16);
    InvalidateRect(self.Handle,@R,false);
  end;
end;

procedure TAdvCheckBox.MouseUp(Button: TMouseButton; Shift: TShiftState;
                                  X, Y: Integer);
var
  R: TRect;
begin
  MouseCapture := False;

  Down := False;

  if (X >= 0) and (X<=Width) and (Y>=0) and (Y<=Height) and FFocused and FMouseDown then
  begin
    ClicksDisabled := True;
    Toggle;
    ClicksDisabled := False;
    Click;
  end;
  FMouseDown := false;

  inherited MouseUp(Button, Shift, X, Y);

  if FIsWinXP then
  begin
    R := Rect(0,0,16,16);
    InvalidateRect(self.Handle,@r,True);
  end;
end;

procedure TAdvCheckBox.MouseMove(Shift: TShiftState;X, Y: Integer);
begin
  if MouseCapture then
    Down := (X >= 0) and (X <= Width) and (Y >= 0) and (Y <= Height);

  inherited MouseMove(Shift,X,Y);
end;

procedure TAdvCheckBox.KeyDown(var Key:Word;Shift:TShiftSTate);
begin
  if (Key=vk_return) and (fReturnIsTab) then
  begin
    Key := vk_tab;
    PostMessage(self.Handle,wm_keydown,VK_TAB,0);
  end;

  if Key = vk_Space then
    Down := True;
  inherited KeyDown(Key,Shift);
end;

procedure TAdvCheckBox.KeyUp(var Key:Word;Shift:TShiftSTate);
begin
  if Key = vk_Space then
  begin
    Down := False;
    Toggle;
    Click;
  end;
end;

procedure TAdvCheckBox.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;


procedure TAdvCheckBox.SetButtonVertAlign(const Value: TTextLayout);
begin
  if Value <> FBtnVAlign then
  begin
    FBtnVAlign := Value;
    Invalidate;
  end;
end;

procedure TAdvCheckBox.SetAlignment(const Value: TLeftRight);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    Invalidate;
  end;
end;

destructor TAdvCheckBox.Destroy;
begin
  FBkgBmp.Free;
  inherited;
end;

procedure TAdvCheckBox.SetEllipsis(const Value: Boolean);
begin
  if FEllipsis <> Value then
  begin
    FEllipsis := Value;
    Invalidate
  end;
end;

procedure TAdvCheckBox.SetCaption(Value: string);
begin
  if HandleAllocated then
  begin
    SetWindowText(Handle,pchar(Value));
  end;
  FCaption := Value;
  Invalidate;
end;


procedure TAdvCheckBox.Toggle;
begin
  if not FReadOnly then
    Checked := not Checked;
end;

procedure TAdvCheckBox.WMEraseBkGnd(var Message: TMessage);
begin
  if Transparent then
    Message.Result := 1
  else
    inherited;
end;

procedure TAdvCheckBox.CMDialogChar(var Message: TCMDialogChar);
begin
  with Message do
    if IsAccel(CharCode, FCaption) and CanFocus then
    begin
      Toggle;
      if TabStop then SetFocus;
      Result := 1;
    end else
      inherited;
end;

procedure TAdvCheckBox.CMMouseEnter(var Message: TMessage);
var
  R: TRect;
begin
  FHot := True;
  R := Rect(0,0,16,16);
  InvalidateRect(self.Handle,@r,True);
 end;

procedure TAdvCheckBox.CMMouseLeave(var Message: TMessage);
var
  R: TRect;
begin
  FHot := False;
  R := Rect(0,0,16,16);
  InvalidateRect(self.Handle,@r,True);
 end;

{ TSearchFooter }

procedure TSearchFooter.Assign(Source: TPersistent);
begin
  if (Source is TSearchFooter) then
  begin
    FAlwaysHighLight := (Source as TSearchFooter).AlwaysHighLight;
    FColor := (Source as TSearchFooter).Color;
    FColorTo := (Source as TSearchFooter).ColorTo;
    FFindNextCaption := (Source as TSearchFooter).FindNextCaption;
    FFindPrevCaption := (Source as TSearchFooter).FindPrevCaption;
    FHighLightCaption := (Source as TSearchFooter).HighLightCaption;
    FHintClose := (Source as TSearchFooter).HintClose;
    FHintFindNext := (Source as TSearchFooter).HintFindNext;
    FHintFindPrev := (Source as TSearchFooter).HintFindPrev;
    FHintHighlight := (Source as TSearchFooter).HintHighLight;
    FMatchCaseCaption := (Source as TSearchFooter).MatchCaseCaption;
    FShowClose := (Source as TSearchFooter).ShowClose;
    FShowFindNext := (Source as TSearchFooter).ShowFindNext;
    FShowFindPrev := (Source as TSearchFooter).ShowFindPrev;
    FShowHighLight := (Source as TSearchFooter).ShowHighLight;
    FShowMatchCase := (Source as TSearchFooter).ShowMatchCase;
    FSearchActiveColumnOnly := (Source as TSearchFooter).SearchActiveColumnOnly;
    FSearchMatchStart := (Source as TSearchFooter).SearchMatchStart;
    FSearchColumn := (Source as TSearchFooter).SearchColumn;
    FSearchDirection := (Source as TSearchFooter).SearchDirection;
    FSearchHiddenRows := (Source as TSearchFooter).SearchHiddenRows;
    FVisible := (Source as TSearchFooter).Visible;
    FFont.Assign((Source as TSearchFooter).Font);
    FHighlight := (Source as TSearchFooter).Highlight;
  end;
end;

procedure TSearchFooter.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

constructor TSearchFooter.Create(AOwner: TComponent; InitDesign: boolean);
begin
  inherited Create;
  FColor := clWhite;
  FColorTo := clBtnFace;
  FShowFindNext := true;
  FShowFindPrev := true;
  FSearchActiveColumnOnly := false;
  FSearchColumn := -1;

  if InitDesign then
  begin
    FFindPrevCaption := 'Find &previous';
    FFindNextCaption := 'Find &next';
    FMatchCaseCaption :='Match case';
    FHighLightCaption := 'Highlight';

    FHintClose := 'Close';
    FHintFindNext := 'Find next occurrence';
    FHintFindPrev := 'Find previous occurrence';
    FHintHighlight := 'Highlight occurrences';
  end;

  FShowMatchCase := true;
  FShowHighLight := true;
  FShowClose := true;
  FAutoSearch := true;
  FSearchDirection := sdTopBottom;
  FFont := TFont.Create;
  FHighlight := hMatchingText;
end;

destructor TSearchFooter.Destroy;
begin
  FFont.Free;
  inherited;
end;

procedure TSearchFooter.SetAutoSearch(const Value: Boolean);
begin
  FAutoSearch := Value;
  FLastSearch := '';
end;

procedure TSearchFooter.SetAutoThemeAdapt(const Value: boolean);
begin
  FAutoThemeAdapt := Value;
  Changed;
end;

procedure TSearchFooter.SetColor(const Value: TColor);
begin
  FColor := Value;
  Changed;
end;

procedure TSearchFooter.SetColorTo(const Value: TColor);
begin
  FColorTo := Value;
  Changed;
end;

procedure TSearchFooter.SetFindNextCaption(const Value: string);
begin
  FFindNextCaption := Value;
  Changed;
end;

procedure TSearchFooter.SetFindPrevCaption(const Value: string);
begin
  FFindPrevCaption := Value;
  Changed;
end;

procedure TSearchFooter.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
  Changed;
end;

procedure TSearchFooter.SetHighLightCaption(const Value: string);
begin
  FHighLightCaption := Value;
  Changed;
end;

procedure TSearchFooter.SetHintClose(const Value: string);
begin
  FHintClose := Value;
  Changed;
end;

procedure TSearchFooter.SetHintFindNext(const Value: string);
begin
  FHintFindNext := Value;
  Changed;
end;

procedure TSearchFooter.SetHintFindPrev(const Value: string);
begin
  FHintFindPrev := Value;
  Changed;
end;

procedure TSearchFooter.SetHintHighlight(const Value: string);
begin
  FHintHighLight := Value;
  Changed;
end;

procedure TSearchFooter.SetMatchCaseCaption(const Value: string);
begin
  FMatchCaseCaption := Value;
  Changed;
end;

procedure TSearchFooter.SetShowClose(const Value: Boolean);
begin
  FShowClose := Value;
  Changed;
end;

procedure TSearchFooter.SetShowFindNext(const Value: boolean);
begin
  if (FShowFindNext <> Value) then
  begin
    FShowFindNext := Value;
    Changed;
  end;
end;

procedure TSearchFooter.SetShowFindPrev(const Value: boolean);
begin
  if (FShowFindPrev <> Value) then
  begin
    FShowFindPrev := Value;
    Changed;
  end;
end;

procedure TSearchFooter.SetShowHighLight(const Value: Boolean);
begin
  if (FShowHighLight <> Value) then
  begin
    FShowHighLight := Value;
    Changed;
  end;
end;

procedure TSearchFooter.SetShowMatchCase(const Value: Boolean);
begin
  if (FShowMatchCase <> Value) then
  begin
    FShowMatchCase := Value;
    Changed;
  end;
end;

procedure TSearchFooter.SetVisible(const Value: boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    Changed;
  end;
end;

procedure TNavigation.Assign(Source: TPersistent);
begin
  if (Source is TNavigation) then
  begin
    FAllowInsertRow := (Source as TNavigation).AllowInsertRow;
    FAllowDeleteRow := (Source as TNavigation).AllowDeleteRow;
    FAlwaysEdit := (Source as TNavigation).AlwaysEdit;
    FAdvanceOnEnter := (Source as TNavigation).AdvanceOnEnter;
    FAdvanceOnEnterLoop := (Source as TNavigation).AdvanceOnEnterLoop;
    FAdvanceInsert := (Source as TNavigation).AdvanceInsert;
    FAdvanceAutoEdit := (Source as TNavigation).AdvanceAutoEdit;
    FAutoGotoWhenSorted := (Source as TNavigation).AutoGotoWhenSorted;
    FAutoGotoIncremental := (Source as TNavigation).AutoGotoIncremental;
    FAutoComboDropSize := (Source as TNavigation).AutoComboDropSize;
    FAdvanceDirection := (Source as TNavigation).AdvanceDirection;
    FAllowClipboardShortCuts := (Source as TNavigation).AllowClipboardShortcuts;
    FAllowCtrlEnter := (Source as TNavigation).AllowCtrlEnter;
    FAllowSmartClipboard := (Source as TNavigation).AllowSmartClipboard;
    FAllowRTFClipboard := (Source as TNavigation).AllowRTFClipboard;
    FAllowFmtClipboard := (Source as TNavigation).AllowFMTClipboard;
    FAllowClipboardAlways := (Source as TNavigation).AllowClipboardAlways;
    FAllowClipboardRowGrow := (Source as TNavigation).AllowClipboardRowGrow;
    FAllowClipboardColGrow := (Source as TNavigation).AllowClipboardColGrow;
    FAdvanceAuto := (Source as TNavigation).AdvanceAuto;
    FAppendOnArrowDown := (Source as TNavigation).AppendOnArrowDown;
    FEditSelectAll := (Source as TNavigation).EditSelectAll;
    FInsertPosition := (Source as TNavigation).InsertPosition;
    FClipboardPasteAction := (Source as TNavigation).ClipboardPasteAction;
    FCursorAdvance := (Source as TNavigation).CursorAdvance;
    FCursorWalkEditor := (Source as TNavigation).CursorWalkEditor;
    FCursorWalkAlwaysEdit := (Source as TNavigation).CursorWalkAlwaysEdit;
    FMoveRowOnSort := (Source as TNavigation).MoveRowOnSort;
    FImproveMaskSel := (Source as TNavigation).ImproveMaskSel;
    FCopyHTMLTagsToClipboard := (Source as TNavigation).CopyHTMLTagsToClipboard;
    FKeepHorizScroll := (Source as TNavigation).KeepHorizScroll;
    FLineFeedOnEnter := (Source as TNavigation).LineFeedOnEnter;
    FHomeEndKey := (Source as TNavigation).HomeEndKey;
    FSkipFixedCells := (Source as TNavigation).SkipFixedCells;
    FSkipDisabledRows := (Source as TNavigation).SkipDisabledRows;
    FTabToNextAtEnd := (Source as TNavigation).TabToNextAtEnd;
    FTabAdvanceDirection := (Source as TNavigation).AdvanceDirection;
    FLeftRightRowSelect := (Source as TNavigation).LeftRightRowSelect;
    FMoveScrollOnly := (Source as TNavigation).MoveScrollOnly;
    FComboGetUpDown := (Source as TNavigation).ComboGetUpDown;
    FAdvanceSkipReadOnlyCells := (Source as TNavigation).AdvanceSkipReadOnlyCells;
    FCursorMoveRows := (Source as TNavigation).CursorMoveRows;
    FAutoComboSelect := (Source as TNavigation).AutoComboSelect;
    FClipboardCutAction := (Source as TNavigation).ClipboardCutAction;
    FAllowEditButtonCells := (Source as TNavigation).AllowEditButtonCells;
  end;
end;

constructor TNavigation.Create;
begin
  inherited Create;
  FCopyHTMLTagsToClipboard := True;
  FAllowClipboardRowGrow := True;
  FAllowClipboardColGrow := True;
  FEditSelectAll := True;
  FSkipFixedCells := True;
  FAllowCtrlEnter := True;
  FCursorWalkAlwaysEdit := True;
  FAdvanceAutoEdit := True;
  FLeftRightRowSelect := True;
  FClipboardPasteAction := paReplace;
  FComboGetUpDown := True;
  FSkipDisabledRows := True;
  FAdvanceOnEnterLoop := True;
  FCursorAdvance := caDefault;
  FAdvanceSkipReadOnlyCells := True;
  FCursorMoveRows := False;
  FAutoComboSelect := True;
  FClipboardCutAction := caClear;
end;

destructor TNavigation.Destroy;
begin
  inherited Destroy;
end;

procedure TNavigation.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TNavigation.SetAutoGoto(aValue: Boolean);
begin
  FAutoGotoWhenSorted:=aValue;
end;

procedure TNavigation.SetAdvanceAuto(const Value: Boolean);
begin
  if (FAdvanceAuto <> Value) then
  begin
    FAdvanceAuto := Value;
    if FAdvanceAuto then
      FAdvanceOnEnter := true;
  end;
end;

procedure TNavigation.SetAdvanceDirection(const Value: TAdvanceDirection);
begin
  if (FAdvanceDirection <> Value) then
  begin
    FAdvanceDirection := Value;
    Changed;
  end;
end;

procedure TNavigation.SetAdvanceInsert(const Value: Boolean);
begin
  if (FAdvanceInsert <> Value) then
  begin
    FAdvanceInsert := Value;
    Changed;
  end;
end;

constructor TMouseActions.Create(AOwner: TComponent);
begin
  inherited Create;
  FAutoSizeColOnDblClick := True;
  FPreciseNodeClick := True;
  FFixedColsEditor := fceEdit;
  FFixedRowsEditor := fceEdit;
  FTouchScroll := false;
  FPartialScrollDelta := 0;

  FFixedComboValues := TStringList.Create;
end;

destructor TMouseActions.Destroy;
begin
  FFixedComboValues.Free;
  inherited Destroy;
end;

function TMouseActions.GetFixedComboValues: TStrings;
begin
  Result := FFixedComboValues;
end;

function TMouseActions.IsDesigning: boolean;
begin
  if Assigned(FOnIsDesigning) then
    FOnIsDesigning(Self, Result);
end;

procedure TMouseActions.SetHotmailRowSelect(const AValue: Boolean);
begin
  FHotmailRowSelect := AValue;
  if FHotmailRowSelect then  // hotmail style row selection only makes sense with disjunct row selection
  begin
    FDisjunctRowSelect := true;
    FCheckAllCheck := true;
  end;
  if Assigned(FOnInvalidate) then
    FOnInvalidate(Self);
end;

procedure TMouseActions.SetEditOnDblClickOnly(const AValue: Boolean);
begin
  FEditOnDblClickOnly := AValue;
  if AValue and Assigned(OnDisableEdit) then // make sure to turn off normal editing
    OnDisableEdit(Self);
end;

procedure TMouseActions.SetFixedComboValues(const Value: TStrings);
begin
  FFixedComboValues.Assign(Value);
end;

procedure TMouseActions.SetDisjunctColSelect(const AValue: Boolean);
begin
  FDisjunctColSelect := AValue;

  if FDisjunctColSelect and IsDesigning  then
  begin
    FDisjunctRowSelect := False;
    FDisjunctCellSelect := False;
  end;
end;

procedure TMouseActions.SetDisjunctRowSelect(const AValue: Boolean);
begin
  FDisjunctRowSelect := AValue;

  if FDisjunctRowSelect and IsDesigning then
  begin
    FDisjunctColSelect := False;
    FDisjunctCellSelect := False;
  end;

end;

procedure TMouseActions.SetDisjunctCellSelect(const AValue: Boolean);
begin
  FDisjunctCellSelect := AValue;
  if FDisjunctCellSelect and IsDesigning then
  begin
    FDisjunctRowSelect := False;
    FDisjunctColSelect := False;
  end;
end;

procedure TMouseActions.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TMouseActions.SetWheelAction(const Value: TWheelAction);
begin
  if (FWheelAction <> Value) then
  begin
    FWheelAction := Value;
    Changed;
  end;
end;

procedure TMouseActions.Assign(Source: TPersistent);
begin
  if (Source is TMouseActions) then
  begin
    FAllColumnSize := (Source as TMouseActions).AllColumnSize;
    FAllRowSize := (Source as TMouseActions).AllRowSize;
    FAllSelect := (Source as TMouseActions).AllSelect;
    FAutoFocus := (Source as TMouseActions).AutoFocus;
    FAutoSizeColOnDblClick := (Source as TMouseActions).AutoSizeColOnDblClick;
    FCaretPositioning := (Source as TMouseActions).CaretPositioning;
    FCheckAllCheck := (Source as TMouseActions).CheckAllCheck;
    FColSelect := (Source as TMouseActions).ColSelect;
    FDirectComboClose := (Source as TMouseActions).DirectComboClose;
    FDirectComboDrop := (Source as TMouseActions).DirectComboDrop;
    FDirectDateClose := (Source as TMouseActions).DirectDateClose;
    FDirectDateDrop := (Source as TMouseActions).DirectDateDrop;
    FDirectEdit := (Source as TMouseActions).DirectEdit;
    FDisjunctRowSelect := (Source as TMouseActions).DisjunctRowSelect;
    FDisjunctColSelect := (Source as TMouseActions).DisjunctColSelect;
    FDisjunctCellSelect := (Source as TMouseActions).DisjunctCellSelect;
    FFixedRowsEdit := (Source as TMouseActions).FixedRowsEdit;
    FFixedColsEdit := (Source as TMouseActions).FixedColsEdit;
    FFixedColsEditor := (Source as TMouseActions).FixedColsEditor;
    FFixedRowsEditor := (Source as TMouseActions).FixedRowsEditor;
    FMoveRowOnNodeClick := (Source as TMouseActions).MoveRowOnNodeClick;
    FNoAutoRangeScroll :=  (Source as TMouseActions).NoAutoRangeScroll;
    FNodeAllExpandContract := (Source as TMouseActions).NodeAllExpandContract;
    FNoScrollOnPartialRow := (Source as TMouseActions).NoScrollOnPartialRow;
    FNoScrollOnPartialCol := (Source as TMouseActions).NoScrollOnPartialCol;
    FPartialScrollDelta := (Source as TMouseActions).PartialScrollDelta;
    FPreciseCheckBoxCheck := (Source as TMouseActions).PreciseCheckBoxCheck;
    FPreciseNodeClick := (Source as TMouseActions).PreciseNodeClick;
    FRangeSelectAndEdit := (Source as TMouseActions).RangeSelectAndEdit;
    FRowSelect := (Source as TMouseActions).RowSelect;
    FRowSelectPersistent := (Source as TMouseActions).RowSelectPersistent;
    FSelectOnRightClick := (Source as TMouseActions).SelectOnRightClick;
    FSizeFixedCol := (Source as TMouseActions).SizeFixedCol;
    FTouchScroll := (Source as TMouseActions).TouchScroll;
    FWheelIncrement := (Source as TMouseActions).WheelIncrement;
    FWheelAction := (Source as TMouseActions).WheelAction;
    FWheelActive := (Source as TMouseActions).WheelActive;
    FDisjunctRowSelectNoCtrl := (Source as TMouseActions).DisjunctRowSelectNoCtrl;
  end;
end;

{ TSyncGrid }

procedure TSyncGrid.Assign(Source: TPersistent);
begin
  if (Source is TSyncGrid) then
  begin
    Grid := (Source as TSyncGrid).Grid;
    ScrollVertical := (Source as TSyncGrid).ScrollVertical;
    ScrollHorizontal:= (Source as TSyncGrid).ScrollHorizontal;
    SelectionColumn := (Source as TSyncGrid).SelectionColumn;
    SelectionRow := (Source as TSyncGrid).SelectionRow;
    MoveColumn := (Source as TSyncGrid).MoveColumn;
    MoveRow := (Source as TSyncGrid).MoveRow;
  end;
end;

constructor TSyncGrid.Create(AOwner: TComponent);
begin
  inherited Create;
  FOwner := AOwner;
end;

procedure TSyncGrid.SetGrid(const Value: TStringGrid);
begin
  if (Value <> FOwner) then
    FGrid := Value;
end;


{ TBands }

procedure TBands.Changed;
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

constructor TBands.Create;
begin
  inherited Create;
  FActive := False;
  FPrimaryColor := clInfoBk;
  FSecondaryColor := clWindow;
  FPrimaryLength := 1;
  FSecondaryLength := 1;
  FTotalLength := FPrimaryLength + FSecondaryLength;
end;

procedure TBands.Assign(Source: TPersistent);
begin
  if (Source is TBands) then
  begin
    FActive := (Source as TBands).Active;
    FPrimaryColor := (Source as TBands).PrimaryColor;
    FPrimaryLength := (Source as TBands).PrimaryLength;
    FSecondaryColor := (Source as TBands).SecondaryColor;
    FSecondaryLength := (Source as TBands).SecondaryLength;
    FPrint := Print;
  end;
end;

procedure TBands.SetActive(const Value: Boolean);
begin
  if (FActive <> Value) then
  begin
    FActive := Value;
    Changed;
  end;
end;

procedure TBands.SetPrimaryColor(const Value: TColor);
begin
  if (FPrimaryColor <> Value) then
  begin
    FPrimaryColor := Value;
    if FActive then
      Changed;
  end;
end;

procedure TBands.SetPrimaryLength(const Value: Integer);
begin
  if (FPrimaryLength <> Value) then
  begin
    FPrimaryLength := Value;
    FTotalLength :=  FSecondaryLength + FPrimaryLength;
    if FActive then
      Changed;
  end;
end;

procedure TBands.SetSecondaryColor(const Value: TColor);
begin
  if (FSecondaryColor <> Value) then
  begin
    FSecondaryColor := Value;
    if FActive then
      Changed;
  end;
end;

procedure TBands.SetSecondaryLength(const Value: Integer);
begin
  if (FSecondaryLength <> Value) then
  begin
    FSecondaryLength := Value;
    FTotalLength :=  FSecondaryLength + FPrimaryLength;
    if FActive then
      Changed;
  end;
end;


{ TGridFocusWindow }

constructor TGridFocusWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 0;
  Height := 0;
  Color := clRed;
end;

//------------------------------------------------------------------------------

procedure TGridFocusWindow.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.Style := WS_POPUP or WS_DISABLED;
  Params.ExStyle := WS_EX_TOPMOST or WS_EX_TOOLWINDOW; /// or WS_EX_TRANSPARENT;
end;

//------------------------------------------------------------------------------

procedure TGridFocusWindow.CreateWnd;
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TGridFocusWindow.Paint;
begin
  inherited;
end;



//------------------------------------------------------------------------------


constructor TGridAlphaMask.Create;
begin
  inherited Create;
  PixelFormat := pf32bit;
  FOpacity := $FF;
end;

procedure TGridAlphaMask.Assign(Source: TPersistent);
var
  AlphaMap: TGridAlphaMask absolute Source;
  I: Integer;
begin
  if Source is TGridAlphaMask then
  begin
    Opacity := AlphaMap.Opacity;
    for I := 0 to Height - 1 do
      Move(AlphaMap.ScanLine[I]^, ScanLine[I]^, Width * SizeOf(TRGBA));
  end
  else
    inherited Assign(Source);
  PixelFormat := pf32bit;
end;


procedure TGridAlphaMask.MergeMask(Mask, Bitmap: TBitmap; Color: TColor);
var
  Fill: TRGBA;
  Swap: Byte;
  A: PRGBA;
  B: PRGB;
  C: PRGB;
  X, Y: Integer;
begin
  if Bitmap.PixelFormat <> pf24Bit then
    Exit;

  Width := Bitmap.Width;
  Height := Bitmap.Height;

  Fill := TRGBA(ColorToRGB(Color));
  Swap := Fill.Blue;
  Fill.Blue := Fill.Red;
  Fill.Red := Swap;

  for Y := 0 to Height - 1 do
  begin
    A := ScanLine[Y];
    B := Bitmap.Scanline[Y];
    C := Mask.ScanLine[Y];

    for X := 0 to Width - 1 do
    begin
      A.Alpha := C.Red;
      A.Red := Round(B.Red * (C.Red / $FF));
      A.Green := Round(B.Green * (C.Red / $FF));
      A.Blue := Round(B.Blue * (C.Red / $FF));

      Inc(A);
      Inc(B);
      Inc(C);
    end;
  end;
end;



procedure MakeGaussianKernel(var K: TKernel; radius: double; MaxData, DataGranularity: double);
var
  j: integer; temp, delta: double;
  KernelSize: TKernelSize;

begin
  for j := Low(K.Weights) to High(K.Weights) do
  begin
    temp := j/radius;
    K.Weights[j] := exp(- temp*temp/2);
  end;

  temp := 0;

  for j := Low(K.Weights) to High(K.Weights) do
    temp := temp + K.Weights[j];

  for j := Low(K.Weights) to High(K.Weights) do
    K.Weights[j] := K.Weights[j] / temp;

  KernelSize := MaxKernelSize;
  delta := DataGranularity / (2*MaxData);
  temp := 0;

  while (temp < delta) and (KernelSize > 1) do
  begin
    temp := temp + 2 * K.Weights[KernelSize];
    dec(KernelSize);
  end;

  K.Size := KernelSize;

  temp := 0;

  for j := -K.Size to K.Size do
    temp := temp + K.Weights[j];

  for j := -K.Size to K.Size do
    K.Weights[j] := K.Weights[j] / temp;
end;


function TrimInt(Lower, Upper, theInteger: integer): integer;
begin
  if (theInteger <= Upper) and (theInteger >= Lower) then
    Result := theInteger
  else
    if theInteger > Upper then
      Result := Upper
    else
      Result := Lower;
end;

function TrimReal(Lower, Upper: integer; x: double): integer;
begin
  if (x < upper) and (x >= lower) then
    Result := trunc(x)
  else
    if x > Upper then
      Result := Upper
    else
      Result := Lower;
end;

procedure BlurRow(var theRow: array of TRGBTriplet; K: PKernel; P: PRow);
var
  j, n: integer; tr, tg, tb: double; //tempRed, etc
  w: double;

begin
  for j:= 0 to High(theRow) do
  begin
    tb := 0;
    tg := 0;
    tr := 0;

    for n := -K.Size to K.Size do
    begin
      w := K.Weights[n];

      with theRow[TrimInt(0, High(theRow), j - n)] do
      begin
        tb := tb + w * b;
        tg := tg + w * g;
        tr := tr + w * r;
      end;
    end;

    with P[j] do
    begin
      b := TrimReal(0, 255, tb);
      g := TrimReal(0, 255, tg);
      r := TrimReal(0, 255, tr);
    end;
  end;

  Move(P[0], theRow[0], (High(theRow) + 1) * Sizeof(TRGBTriplet));
end;



procedure GBlur(theBitmap: TBitmap; radius: double);
var
  i,
  Row, Col: integer;
  theRows: PPRows;
  K: TKernel;
  ACol: PRow; P:PRow;

begin
  if (theBitmap.HandleType <> bmDIB) or (theBitmap.PixelFormat <> pf24Bit) then
    raise exception.Create('Blur only works for 24-bit bitmaps');

  MakeGaussianKernel(K, radius, 255, 1);

  GetMem(theRows, theBitmap.Height * SizeOf(PRow));
  GetMem(ACol, theBitmap.Height * SizeOf(TRGBTriplet));

  for Row := 0 to theBitmap.Height - 1 do
    theRows[Row] := theBitmap.Scanline[Row];

  P := AllocMem(theBitmap.Width*SizeOf(TRGBTriplet));
  for Row := 0 to theBitmap.Height - 1 do
  begin
    //Optimization: rows between 20 and Height - 20 are the same...
    if (Row > 20) and (Row < theBitmap.Height - 20) then
      //...so copy a previous row
      for i := 0 to theBitmap.Width do
        theRows[Row][i] := theRows[15][i]
    else
      BlurRow(Slice(theRows[Row]^, theBitmap.Width), @K, P);
  end;

  ReAllocMem(P, theBitmap.Height*SizeOf(TRGBTriplet));

  for Col := 0 to theBitmap.Width - 1 do
  begin
    //Optimization: rows between 20 and With - 20 are the same...
    if (Col > 20) and (Col < theBitmap.Width - 20) then
    begin
      //...so copy a previous col
      for Row := 0 to theBitmap.Height - 1 do
        theRows[Row][Col]:= theRows[Row][15];
      Continue;
    end;

    for Row := 0 to theBitmap.Height - 1 do
      ACol[Row] := theRows[Row][Col];

    BlurRow(Slice(ACol^, theBitmap.Height), @K, P);

    for Row := 0 to theBitmap.Height - 1 do
     theRows[Row][Col]:= ACol[Row];
  end;

  FreeMem(theRows);
  FreeMem(ACol);
  ReAllocMem(P, 0);
end;



procedure DrawRoundRect(Bitmap: TBitmap; W, H: integer; Thickness, Radius: Integer; Blur: Double);
var
  DC: HDC;
  P: HPEN;
  B: HBRUSH;
  I: Integer;
begin
  Bitmap.PixelFormat := pf24bit;

  Bitmap.Width := W;
  Bitmap.Height := H;

  DC := Bitmap.Canvas.Handle;

  // outer black rectangle
  FillRect(DC, Rect(0, 0, Bitmap.Width, Bitmap.Height), GetStockObject(BLACK_BRUSH));

  if Thickness > 0 then
  begin
    P := SelectObject(DC, CreatePen(PS_SOLID, Thickness, $FFFFFF));
    B := SelectObject(DC, GetStockObject(BLACK_BRUSH));
  end
  else
  begin
    P := SelectObject(DC, GetStockObject(WHITE_PEN));
    B := SelectObject(DC, GetStockObject(WHITE_BRUSH));
  end;

  I := 2;

  RoundRect(DC, I + Thickness shr 1, I + Thickness shr 1, W - I - Thickness shr 1, H - I - Thickness shr 1, Radius, Radius);

  if Thickness > 0 then
  begin
    SelectObject(DC, B);
    DeleteObject(SelectObject(DC, P));
  end
  else
  begin
    SelectObject(DC, B);
    SelectObject(DC, P);
  end;

  if (Blur > 0) then
    GBlur(Bitmap, Blur);
end;


{ TFocusHelper }

procedure TFocusHelper.Assign(Source: TPersistent);
begin
  if (Source is TFocusHelper) then
    FEnabled := (Source as TFocusHelper).Enabled;
end;

constructor TFocusHelper.Create(AOwner: TComponent);
begin
  inherited Create;
  FEnabled := false;
end;


end.
