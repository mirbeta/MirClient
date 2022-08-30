{***************************************************************************}
{ TAdvSmoothSelectors components                                                  }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2004 - 2008                                        }
{            Email : info@tmssoftware.com                                   }
{            Web : http://www.tmssoftware.com                               }
{                                                                           }
{ The source code is given as is. The author is not responsible             }
{ for any possible damage done due to the use of this code.                 }
{ The component can be freely used in any application. The complete         }
{ source code remains property of the author and may not be distributed,    }
{ published, given or sold in any form as such. No parts of the source      }
{ code can be included in any other component or application without        }
{ written authorization of the author.                                      }
{***************************************************************************}

{$I TMSDEFS.INC}

unit AdvSmoothSelectors;

interface

uses
  Classes, Windows, Graphics, Controls, Messages, ExtCtrls, SysUtils, ImgList, Forms,
  Math, Dialogs
{$IFNDEF TMSDOTNET}
  {$IFDEF TMSPACKGDIP}
  ,AdvXPVS
  {$ELSE}
  ,AdvSmoothXPVS
  {$ENDIF}
{$ENDIF}
{$IFDEF TMSDOTNET}
  , uxTheme, System.Text, WinUtils, Types
{$ENDIF}
  ;

const
  MINBUTTONSIZE = 16;
  TABLECELLSIZE = 24;

  MAJ_VER = 1; // Major version nr.
  MIN_VER = 3; // Minor version nr.
  REL_VER = 5; // Release nr.
  BLD_VER = 0; // Build nr.

  // revision history
  // v1.0.0.0 : first release


type
  TAdvSelectorStyle = (ssButton, ssCombo {, ssCheck});
  TAdvAppearanceStyle = (esOffice2003Blue, esOffice2003Silver, esOffice2003Olive, esOffice2003Classic, esWhidbey, esCustom, esXP);
  TAdvButtonState = (absUp, absDown, absDropDown);
  TGradientDirection = (gdVertical, gdHorizontal);
  TSelectorItemType = (itAutoSizeButton, itFullWidthButton);
  TColorSelectionStyle = (csDiscrete, csColorCube, csSpectrum);
  TNoOfButtons = 1..16;

  TAdvSmoothSelectorPanel = class;

  TAdvSmoothSelectorItem = class(TCollectionItem)
  private
    FItemRect: TRect;
    FCaption: string;
    FEnable: boolean;
    FImageIndex: integer;
    FValue: string;
    FHint: string;
    FCaptionAlignment: TAlignment;
    FItemType: TSelectorItemType;
    FBackGroundColor: TColor;
    FTag: integer;
    procedure SetCaption(const Value: string);
    procedure SetEnable(const Value: boolean);
    procedure SetHint(const Value: string);
    procedure SetImageIndex(const Value: integer);
    procedure SetValue(const Value: string);
    procedure SetvCaptionAlignment(const Value: TAlignment);
    procedure SetItemType(const Value: TSelectorItemType);
    procedure SetBackGroundColor(const Value: TColor);
    procedure SetTag(const Value: integer);
  protected
    property ItemRect: TRect read FItemRect write FItemRect;

    property Enable: boolean read FEnable write SetEnable;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property BackGroundColor: TColor read FBackGroundColor write SetBackGroundColor default clNone;
    property Caption: string read FCaption write SetCaption;
    property CaptionAlignment: TAlignment read FCaptionAlignment write SetvCaptionAlignment;
    property ImageIndex: integer read FImageIndex write SetImageIndex;
    property Hint: string read FHint write SetHint;
    property Value: string read FValue write SetValue;
    //property Enable: boolean read FEnable write SetEnable;
    property ItemType: TSelectorItemType read FItemType write SetItemType default itAutoSizeButton;
    property Tag: integer read FTag write SetTag default 0;
    //property MultiSelect: boolean
  end;

  TAdvSmoothSelectorItems = class(TCollection)
  private
    FOwner: TPersistent;
    function GetItem(Index: Integer): TAdvSmoothSelectorItem;
    procedure SetItem(Index: Integer; const Value: TAdvSmoothSelectorItem);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TPersistent);
    function Add: TAdvSmoothSelectorItem;
    function Insert(Index: Integer): TAdvSmoothSelectorItem;
    property Items[Index: Integer]: TAdvSmoothSelectorItem read GetItem write SetItem; default;
  end;

  TChangeSelectionEvent = procedure(OldItemIndex, NewItemIndex: integer) of object;
  THotToolEvent = procedure(Sender: TObject; HotItemIndex: integer) of object;

  TAdvSmoothCustomSelectorPanel = class(TCustomPanel)
  private
    FOwner: TComponent;
    FColorTo: TColor;
    FGradientDirection: TGradientDirection;
    FWindowBorderColor: TColor;
    FOnShouldHide: TNotifyEvent;
    procedure SetColorTo(const Value: TColor);
    procedure SetTGradientDirection(const Value: TGradientDirection);
    procedure SetWindowBorderColor(const Value: TColor);
  protected
    procedure Paint; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    function GetVisibleHeight: integer; virtual;

    property OnShouldHide: TNotifyEvent read FOnShouldHide write FOnShouldHide;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property ColorTo: TColor read FColorTo write SetColorTo default clNone;
    property WindowBorderColor: TColor read FWindowBorderColor write SetWindowBorderColor default clGray;
    property GradientDirection: TGradientDirection read FGradientDirection write SetTGradientDirection;
  end;

  TItemDrawEvent = procedure(Sender: TObject; Index: integer; R: TRect) of object;

  TSelectEvent = procedure(Sender: TObject; Index: Integer; Item: TAdvSmoothSelectorItem) of object;

  TAdvSmoothSelectorPanel = class(TAdvSmoothCustomSelectorPanel)
  private
    FItemIndex: integer;
    FItems: TAdvSmoothSelectorItems;
    FColorHotTo: TColor;
    FBorderColor: TColor;
    FBorderDownColor: TColor;
    FColorHot: TColor;
    FBorderHotColor: TColor;
    FColorDownTo: TColor;
    FColorDown: TColor;
    FImages: TCustomImageList;
    FHotItemIndex: integer;
    FDownItemIndex: integer;
    FButtonHeight: integer;
    FTopOffSet: integer;
    FLeftOffSet: integer;
    FButtonsPerRow: TNoOfButtons;
    FOnChangeSelection: TChangeSelectionEvent;
    FButtonMargin: integer;
    FMouseDown: Boolean;
    FOnSelect: TNotifyEvent;
    FBorderSelectedColor: TColor;
    FColorSelected: TColor;
    FColorSelectedTo: TColor;
    FOnHotTool: THotToolEvent;
    FMaxCaptionLength: integer;
    FOnDrawItem: TItemDrawEvent;
    FMinButtonWidth: integer;
    FMinButtonHeight: integer;
    FNoPrefix: Boolean;
    FTwoColorImages: Boolean;
    procedure WMChar(var Msg: TWMKey); message WM_CHAR;
    procedure CMWantSpecialKey(var Msg: TCMWantSpecialKey); message CM_WANTSPECIALKEY;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure SetBorderColor(const Value: TColor);
    procedure SetItemIndex(const Value: integer);
    procedure SetItems(const Value: TAdvSmoothSelectorItems);
    procedure SetImages(const Value: TCustomImageList);
    procedure SetButtonsPerRow(const Value: TNoOfButtons);
    procedure SetButtonMargin(const Value: integer);
    procedure SetMinButtonHeight(const Value: integer);
    procedure SetMinButtonWidth(const Value: integer);
    procedure SetTwoColorImages(const Value: Boolean);
  protected
    procedure DrawItem(Index: integer; RefreshItem: boolean = false);
    procedure Paint; override;
    procedure SetItemsPosition;
    function GetMaxWidth: integer;
    function TotalAutoSizeButtons: integer;
    function ItemAtPos(X, Y: integer): integer;
    procedure SetPanelHeight;
    procedure AutoSizeBtnSize(var W, H: integer);
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    property MinButtonWidth: integer read FMinButtonWidth write SetMinButtonWidth default MINBUTTONSIZE;
    property MinButtonHeight: integer read FMinButtonHeight write SetMinButtonHeight default MINBUTTONSIZE;
    property NoPrefix: Boolean read FNoPrefix write FNoPrefix;

    property TwoColorImages: Boolean read FTwoColorImages write SetTwoColorImages;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Items: TAdvSmoothSelectorItems read FItems write SetItems;
    property ItemIndex: integer read FItemIndex write SetItemIndex;
    property HotItemIndex: integer read FHotItemIndex;
  published
    property BorderColor: TColor read FBorderColor write SetBorderColor default clNone;
    property BorderDownColor: TColor read FBorderDownColor write FBorderDownColor default clNone;
    property BorderHotColor: TColor read FBorderHotColor write FBorderHotColor default clNone;
    property BorderSelectedColor: TColor read FBorderSelectedColor write FBorderSelectedColor default clBlack;

    property ColorDown: TColor read FColorDown write FColorDown;
    property ColorDownTo: TColor read FColorDownTo write FColorDownTo default clNone;
    property ColorHot: TColor read FColorHot write FColorHot;
    property ColorHotTo: TColor read FColorHotTo write FColorHotTo default clNone;
    property ColorSelected: TColor read FColorSelected write FColorSelected;
    property ColorSelectedTo: TColor read FColorSelectedTo write FColorSelectedTo;

    property ButtonsPerRow: TNoOfButtons read FButtonsPerRow write SetButtonsPerRow default 1;
    property Images: TCustomImageList read FImages write SetImages;
    property ButtonMargin: integer read FButtonMargin write SetButtonMargin default 3;

    property OnChangeSelection: TChangeSelectionEvent read FOnChangeSelection write FOnChangeSelection;
    property OnSelect: TNotifyEvent read FOnSelect write FOnSelect;
    property OnHotTool: THotToolEvent read FOnHotTool write FOnHotTool;
    property OnDrawItem: TItemDrawEvent read FOnDrawItem write FOnDrawItem;
  end;

  TColorCubeCell = record
    CenterPos: TPoint;
    Color: TColor;
  end;

  TAdvSmoothColorCubePanel = class(TAdvSmoothCustomSelectorPanel)
  private
    FCubeCells: array[1..127] of TColorCubeCell;
    FCubeSize: TPoint;
    FSelectedColor: TColor;
    FSelectedIndex: integer;
    FHotIndex: integer;
    FOnSelect: TNotifyEvent;
    FShowRGBHint: Boolean;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure DrawColorCube(Index: integer);
    procedure DrawAllColorCube;
    procedure DrawSelectedBorder;
    procedure DrawHotBorder;
    procedure SetSelectedColor(const Value: TColor);
    procedure SetSelectedIndexAndColor(clr: TColor; index: integer = -1);
    procedure SetShowRGBHint(const Value: Boolean);
  protected
    procedure Paint; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

    function PtInCell(Index: integer; P: TPoint): Boolean;
    function IndexOfCellAt(X, Y: integer): integer;
    procedure DrawHexagon(aCanvas: TCanvas; P: TPoint; X, Y: integer);
    procedure Initialize;
    procedure SetItemsPosition;
    procedure SetPanelSize;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property SelectedColor: TColor read FSelectedColor write SetSelectedColor;
    property ShowRGBHint: Boolean read FShowRGBHint write SetShowRGBHint;
    property OnSelect: TNotifyEvent read FOnSelect write FOnSelect;
  end;

  TAdvSmoothColorSpectrumPanel = class(TAdvSmoothCustomSelectorPanel)
  private
    FSpectrumImage: TImage;
    FHotColor: TColor;
    FSelectedColor: TColor;
    FHotRect: TRect;
    FSelectedRect: TRect;
    FOnSelect: TNotifyEvent;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure SpectrumImageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure SpectrumImageMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure SpectrumImageMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure DrawHotRect;
    procedure DrawSelectedRect;
    procedure DrawFocusPoint;
    procedure SetSelectedColor(const Value: TColor);
  protected
    procedure Paint; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure SetItemsPosition;
    procedure SetPanelSize;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property SelectedColor: TColor read FSelectedColor write SetSelectedColor;
    property OnSelect: TNotifyEvent read FOnSelect write FOnSelect;
  end;

  TAdvSmoothTableSelectorPanel = class(TAdvSmoothCustomSelectorPanel)
  private
    FOnSelect: TNotifyEvent;
    FSelectedCols: integer;
    FSelectedRows: integer;
    FColCount: integer;
    FRowCount: integer;
    FCellSize: integer;
    FLeftMargin: integer;
    FTopMargin: integer;
    FCellSpace: integer;
    FLabelHeight: integer;
    FSelectionColor: TColor;
    FTextTable: string;
    FTextCancel: string;
    procedure SetColCount(const Value: integer);
    procedure SetRowCount(const Value: integer);
    procedure SetSelectedCols(const Value: integer);
    procedure SetSelectedRows(const Value: integer);
    procedure SetCellSize(const Value: integer);
  {  FSelectedColor: TColor;
    FSelectedIndex: integer;
    FHotIndex: integer;
    FOnSelect: TNotifyEvent;  }
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
   { procedure DrawColorCube(Index: integer);
    procedure DrawAllColorCube;
    procedure DrawSelectedBorder;
    procedure DrawHotBorder;
    procedure SetSelectedColor(const Value: TColor);
    procedure SetSelectedIndexAndColor(clr: TColor; index: integer = -1);  }
    procedure DrawAllCells;
    procedure DrawLabel;
    procedure InvalidateSelection(OldSelectedCols, OldSelectedRows: integer);
  protected
    procedure Paint; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    //procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

   { function PtInCell(Index: integer; P: TPoint): Boolean;
    function IndexOfCellAt(X, Y: integer): integer;
    procedure DrawHexagon(aCanvas: TCanvas; P: TPoint; X, Y: integer); }
    //procedure Initialize;
    procedure SetItemsPosition;
    procedure SetPanelSize;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    //property SelectedColor: TColor read FSelectedColor write SetSelectedColor;
    property ColCount: integer read FColCount write SetColCount default 5;
    property RowCount: integer read FRowCount write SetRowCount default 4;
    property SelectedCols: integer read FSelectedCols write SetSelectedCols default 0;
    property SelectedRows: integer read FSelectedRows write SetSelectedRows default 0;
    property CellSize: integer read FCellSize write SetCellSize default MINBUTTONSIZE;
    property SelectionColor: TColor read FSelectionColor write FSelectionColor default clNavy;
    property TextTable: string read FTextTable write FTextTable;
    property TextCancel: string read FTextCancel write FTextCancel;
    property OnSelect: TNotifyEvent read FOnSelect write FOnSelect;
  end;


  TSelectorDropDownWindow = class(TCustomForm)
  private
    FSelectorPanel: TAdvSmoothCustomSelectorPanel;
    FHideOnDeActivate: Boolean;
    FShowAbove: Boolean;
    FOwner: TComponent;
    FShowFullBorder: Boolean;
    FHideTimer: TTimer;
    FShowLeft: Boolean;
    procedure WMActivate(var Message: TWMActivate); message WM_ACTIVATE;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
    procedure HideTimerOnTime(Sender: TObject);
  protected
    procedure Paint; override;
    function GetParentWnd: HWnd;
    procedure CreateParams(var Params: TCreateParams); override;
    property HideOnDeActivate: Boolean read FHideOnDeActivate write FHideOnDeActivate;
    property ShowAbove: Boolean read FShowAbove write FShowAbove;
    property ShowLeft: Boolean read FShowLeft write FShowLeft default false;
    property ShowFullBorder: Boolean read FShowFullBorder write FShowFullBorder;
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateNew(AOwner: TComponent; Dummy: Integer = 0); override;
    destructor Destroy; override;
    procedure SetWindowSize;
  published
{$IFDEF TMSDOTNET}
    property AutoScroll;
    property BorderIcons;
    property BorderWidth;
    property Ctl3D;
    property FormStyle;
    property OnHide;
{$ENDIF}
    property SelectorPanel: TAdvSmoothCustomSelectorPanel read FSelectorPanel write FSelectorPanel;
  end;

  TDrawToolEvent = procedure(Sender: TObject; Canvas: TCanvas; ItemIndex: integer; R: TRect) of object;

  TAdvSmoothCustomSelector = class(TCustomControl)
  private
    FDropDownWindow: TSelectorDropDownWindow;
    FSelectorPanel: TAdvSmoothSelectorPanel;
    FMouseInControl: Boolean;
    FMouseDown: Boolean;
    FDropDownBtnWidth: integer;
    FDown: Boolean;
    FShaded: Boolean;
    FFlat: Boolean;
    FAllowAllUp: Boolean;
    FDropDownButton: Boolean;
    FAutoThemeAdapt: Boolean;
    FStyle: TAdvSelectorStyle;
    FGlyphDown: TBitmap;
    FGlyphDisabled: TBitmap;
    FGlyphHot: TBitmap;
    FGlyph: TBitmap;
    FGlyphShade: TBitmap;
    FBorderColor: TColor;
    FColorDownTo: TColor;
    FColorTo: TColor;
    FColorHotTo: TColor;
    FColorDown: TColor;
    FBorderDownColor: TColor;
    FColorHot: TColor;
    FColorCheckedTo: TColor;
    FColorChecked: TColor;
    FBorderHotColor: TColor;
    FImages: TCustomImageList;
    FOnMouseEnter: TNotifyEvent;
    FOnDropDown: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FGroupIndex: Integer;
    FAppearanceStyle: TAdvAppearanceStyle;
    FState: TAdvButtonState;
    FColorDropDownTo: TColor;
    FColorDropDown: TColor;
    FBorderDropDownColor: TColor;
    FDropDownCount: integer;
    FGradientDirection: TGradientDirection;
    FSelectedIndex: integer;
    FTools: TAdvSmoothSelectorItems;
    FButtonsPerRow: TNoOfButtons;
    FColorSelectedTo: TColor;
    FColorSelected: TColor;
    FBorderSelectedColor: TColor;
    FOnHotTool: THotToolEvent;
    FOnClick: TNotifyEvent;
    FOnSelect: TSelectEvent;
    FColorSelectionHotTo: TColor;
    FColorSelectionHot: TColor;
    FColorSelectionDownTo: TColor;
    FColorSelectionDown: TColor;
    FOnDrawTool: TDrawToolEvent;
    FDupSelectedIndex: integer;
    FTwoColorImages: Boolean;
    FBackGroundImageColor: TColor;
    FForeGroundImageColor: TColor;
    FOldForeGroundImgColor: TColor;
    FOldBkGroundImgColor: TColor;
    FStretchImageDraw: Boolean;
    FIsThemed: boolean;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;

    procedure GlyphChanged(Sender: TObject);
    procedure OnDropDownWindowHide(Sender: TObject);
    procedure OnDropDownPanelShouldHide(Sender: TObject);
    procedure OnDropDownPanelSelect(Sender: TObject);
    procedure OnDropDownPanelHotTool(Sender: TObject; HotItemIndex: integer);
    procedure OnDropDownPanelDrawTool(Sender: TObject; ItemIndex: integer; R: TRect);
    procedure PopupBtnDown;
    procedure ButtonDown;
    procedure DoDropDown;
    procedure SetAllowAllUp(const Value: Boolean);
    procedure SetBorderColor(const Value: TColor);
    procedure SetColorChecked(const Value: TColor);
    procedure SetColorCheckedTo(const Value: TColor);
    procedure SetColorTo(const Value: TColor);
    procedure SetDown(const Value: Boolean);
    procedure SetDropDownButton(const Value: Boolean);
    procedure SetFlat(const Value: Boolean);
    procedure SetGlyph(const Value: TBitmap);
    procedure SetGlyphDisabled(const Value: TBitmap);
    procedure SetGlyphDown(const Value: TBitmap);
    procedure SetGlyphHot(const Value: TBitmap);
    procedure SetImages(const Value: TCustomImageList);
    procedure SetShaded(const Value: Boolean);
    procedure SetStyle(const Value: TAdvSelectorStyle);
    procedure SetGroupIndex(const Value: Integer);
    procedure SetAppearanceStyle(const Value: TAdvAppearanceStyle);
    procedure SetColorDropDown(const Value: TColor);
    procedure SetColorDropDownTo(const Value: TColor);
    procedure SetDropDownCount(const Value: integer);
    procedure SetTGradientDirection(const Value: TGradientDirection);
    function GetCaption: string;
    procedure SetCaption(const Value: string);
    procedure SetSelectedIndex(const Value: integer);
    procedure SetTools(const Value: TAdvSmoothSelectorItems);
    function GetSelectedIndex: integer;
    procedure SetButtonsPerRow(const Value: TNoOfButtons);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    procedure SetTwoColorImages(const Value: Boolean);
    procedure SetForeGroundImageColor(const Value: TColor);
    procedure SetStretchImageDraw(const Value: Boolean);
  protected
    procedure OnToolSelect; virtual;
    function DrawGlyph(aGlyph: TBitMap; aRect: TRect): integer;
    procedure DrawGlyphAndCaption(aGlyph: TBitmap; R: TRect); virtual;
    procedure DrawButton;
    procedure DrawComboButton;
    procedure SetSelectorPanelItems;
    procedure SetSelectorPanel; virtual;
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;

    procedure Paint; override;
    property MouseInControl: Boolean read FMouseInControl;
    procedure WndProc(var Message: TMessage); override;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure ThemeAdapt;
    procedure SetAutoThemeAdapt(const Value: Boolean);
    function GetVersionNr: Integer; virtual;
    function DoVisualStyles: boolean;

    procedure ChangeImagesColor(ForeGColor, BkGColor: TColor);
    property TwoColorImages: Boolean read FTwoColorImages write SetTwoColorImages;
    property OldForeGroundImgColor: TColor read FOldForeGroundImgColor;
    property OldBkGroundImgColor: TColor read FOldBkGroundImgColor;
    property ForeGroundImageColor: TColor read FForeGroundImageColor write SetForeGroundImageColor;
    property BackGroundImageColor: TColor read FBackGroundImageColor write FBackGroundImageColor;

    property StretchImageDraw: Boolean read FStretchImageDraw write SetStretchImageDraw default True;

    property AllowAllUp: Boolean read FAllowAllUp write SetAllowAllUp default False;
    property AutoThemeAdapt: Boolean read FAutoThemeAdapt write SetAutoThemeAdapt;
    property ButtonsPerRow: TNoOfButtons read FButtonsPerRow write SetButtonsPerRow default 1;
    property BorderColor: TColor read FBorderColor write SetBorderColor default clNone;
    property BorderDownColor: TColor read FBorderDownColor write FBorderDownColor default clNone;
    property BorderHotColor: TColor read FBorderHotColor write FBorderHotColor default clNone;
    property BorderDropDownColor: TColor read FBorderDropDownColor write FBorderDropDownColor default clGray;
    property BorderSelectedColor: TColor read FBorderSelectedColor write FBorderSelectedColor default clBlack;
    property Caption: string read GetCaption write SetCaption;
    property Color;
    property ColorTo: TColor read FColorTo write SetColorTo default clNone;
    property ColorDown: TColor read FColorDown write FColorDown;
    property ColorDownTo: TColor read FColorDownTo write FColorDownTo default clNone;
    property ColorHot: TColor read FColorHot write FColorHot;
    property ColorHotTo: TColor read FColorHotTo write FColorHotTo default clNone;
    property ColorChecked: TColor read FColorChecked write SetColorChecked default clGray;
    property ColorCheckedTo: TColor read FColorCheckedTo write SetColorCheckedTo default clNone;
    property ColorDropDown: TColor read FColorDropDown write SetColorDropDown;
    property ColorDropDownTo: TColor read FColorDropDownTo write SetColorDropDownTo default clNone;
    property ColorSelected: TColor read FColorSelected write FColorSelected;
    property ColorSelectedTo: TColor read FColorSelectedTo write FColorSelectedTo;
    property ColorSelectionHot: TColor read FColorSelectionHot write FColorSelectionHot;
    property ColorSelectionHotTo: TColor read FColorSelectionHotTo write FColorSelectionHotTo;
    property ColorSelectionDown: TColor read FColorSelectionDown write FColorSelectionDown;
    property ColorSelectionDownTo: TColor read FColorSelectionDownTo write FColorSelectionDownTo;
    property Down: Boolean read FDown write SetDown default False;
    property DropDownButton: Boolean read FDropDownButton write SetDropDownButton default False;
    property DropDownCount: integer read FDropDownCount write SetDropDownCount;
    property Flat: Boolean read FFlat write SetFlat default True;
    property GroupIndex: Integer read FGroupIndex write SetGroupIndex default 0;
    property Glyph: TBitmap read FGlyph write SetGlyph;
    property GlyphHot: TBitmap read FGlyphHot write SetGlyphHot;
    property GlyphDown: TBitmap read FGlyphDown write SetGlyphDown;
    property GlyphDisabled: TBitmap read FGlyphDisabled write SetGlyphDisabled;
    property GradientDirection: TGradientDirection read FGradientDirection write SetTGradientDirection;
    property Images: TCustomImageList read FImages write SetImages;
    property Shaded: Boolean read FShaded write SetShaded default True;
    property Style: TAdvSelectorStyle read FStyle write SetStyle;

    property State: TAdvButtonState read FState write FState;

    property Tools: TAdvSmoothSelectorItems read FTools write SetTools;
    property SelectedIndex: integer read GetSelectedIndex write SetSelectedIndex;

    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnDropDown: TNotifyEvent read FOnDropDown write FOnDropDown;

    property OnSelect: TSelectEvent read FOnSelect write FOnSelect;
    property OnHotTool: THotToolEvent read FOnHotTool write FOnHotTool;
    property OnDrawTool: TDrawToolEvent read FOnDrawTool write FOnDrawTool;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Click; override;
    procedure ShowDropDown;
    procedure HideDropDown;
  published
    property Action;
    property TabOrder;
    property TabStop;
    property AppearanceStyle: TAdvAppearanceStyle read FAppearanceStyle write SetAppearanceStyle default esOffice2003Blue;
    property Version: string read GetVersion write SetVersion;
    property Visible;
  end;

  TAdvSmoothToolSelector = class(TAdvSmoothCustomSelector)
  private
  protected
  public
    property AppearanceStyle;
  published
    property AutoThemeAdapt;
    property BorderColor;
    property BorderDownColor;
    property BorderHotColor;
    property BorderDropDownColor;
    property ButtonsPerRow;
    property Caption;
    property Color;
    property ColorTo;
    property ColorDown;
    property ColorDownTo;
    property ColorHot;
    property ColorHotTo;
    property ColorDropDown;
    property ColorDropDownTo;
    property ColorSelected;
    property ColorSelectedTo;
    property DropDownButton;
    property Glyph;
    property GlyphHot;
    property GlyphDown;
    property Images;
    property Style;
    property ShowHint;
    property SelectedIndex;
    property Tools;

    property OnMouseEnter;
    property OnMouseLeave;
    property OnDropDown;

    property OnClick;
    property OnSelect;
    property OnHotTool;
    property OnDrawTool;
  end;

  TSelectionType = (stOffice, stBorland);
  
  TAdvSmoothPenStyleSelector = class(TAdvSmoothCustomSelector)
  private
    FSelectionType: TSelectionType;
    procedure Initialize;
    function GetSelectedPenStyle: TPenStyle;
    procedure SetSelectedPenStyle(const Value: TPenStyle);
    procedure SetSelectionType(const Value: TSelectionType);
    function GetPenColor: TColor;
    procedure SetPenColor(const Value: TColor);
  protected
    procedure Loaded; override;
    procedure SetSelectorPanel; override;

    function GetPenStyleAtIndex(Index: Integer): TPenStyle;
    function GetIndexOfStyle(APenStyle: TPenStyle): Integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property AppearanceStyle;
    property SelectedPenStyle: TPenStyle read GetSelectedPenStyle write SetSelectedPenStyle;
  published
    property AutoThemeAdapt;
    property BorderColor;
    property BorderDownColor;
    property BorderHotColor;
    property BorderDropDownColor;
    //property BackGroundImageColor;
    property Caption;
    property Color;
    property ColorTo;
    property ColorDown;
    property ColorDownTo;
    property ColorHot;
    property ColorHotTo;
    property ColorDropDown;
    property ColorDropDownTo;
    property ColorSelected;
    property ColorSelectedTo;
    property DropDownButton;
    //property ForeGroundImageColor;
    property Glyph;
    property GlyphHot;
    property GlyphDown;
    property PenColor: TColor read GetPenColor write SetPenColor;
    property SelectedIndex;
    property SelectionType: TSelectionType read FSelectionType write SetSelectionType default stOffice;
    property ShowHint;
    //property ShowSelectedPen: Boolean read FShowSelectedPen write SetShowSelectedPen default false;
    property Style;
    property Tools;

    property OnMouseEnter;
    property OnMouseLeave;
    property OnDropDown;

    property OnClick;
    property OnSelect;
    property OnHotTool;
    property OnDrawTool;
  end;

  TAdvSmoothPenWidthSelector = class(TAdvSmoothCustomSelector)
  private
    procedure Initialize;
  protected
    procedure SetSelectorPanel; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property AppearanceStyle;
  published
    property AutoThemeAdapt;
    property BorderColor;
    property BorderDownColor;
    property BorderHotColor;
    property BorderDropDownColor;
    property Caption;
    property Color;
    property ColorTo;
    property ColorDown;
    property ColorDownTo;
    property ColorHot;
    property ColorHotTo;
    property ColorDropDown;
    property ColorDropDownTo;
    property ColorSelected;
    property ColorSelectedTo;
    property DropDownButton;
    property Glyph;
    property GlyphHot;
    property GlyphDown;
    property ShowHint;
    property Style;

    property SelectedIndex;
    property Tools;

    property OnMouseEnter;
    property OnMouseLeave;
    property OnDropDown;

    property OnClick;
    property OnSelect;
    property OnHotTool;
    property OnDrawTool;
  end;

  TAdvSmoothBrushStyleSelector = class(TAdvSmoothCustomSelector)
  private
    FSelectionType: TSelectionType;
    procedure Initialize;
    function GetSelectedBrushStyle: TBrushStyle;
    procedure SetSelectedBrushStyle(const Value: TBrushStyle);
    function GetBrushColor: TColor;
    procedure SetBrushColor(const Value: TColor);
    procedure SetSelectionType(const Value: TSelectionType);
  protected
    procedure Loaded; override;
    procedure SetSelectorPanel; override;

    function GetBrushStyleAtIndex(Index: Integer): TBrushStyle;
    function GetIndexOfBrushStyle(ABrushStyle: TBrushStyle): Integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property AppearanceStyle;
    property SelectedBrushStyle: TBrushStyle read GetSelectedBrushStyle write SetSelectedBrushStyle;
  published
    property AutoThemeAdapt;
    //property BackGroundImageColor;
    property BorderColor;
    property BorderDownColor;
    property BorderHotColor;
    property BorderDropDownColor;
    property BrushColor: TColor read GetBrushColor write SetBrushColor;
    property Caption;
    property Color;
    property ColorTo;
    property ColorDown;
    property ColorDownTo;
    property ColorHot;
    property ColorHotTo;
    property ColorDropDown;
    property ColorDropDownTo;
    property ColorSelected;
    property ColorSelectedTo;
    //property ForeGroundImageColor;
    property DropDownButton;
    property Glyph;
    property GlyphHot;
    property GlyphDown;

    property SelectionType: TSelectionType read FSelectionType write SetSelectionType default stOffice;
    property SelectedIndex;
    property ShowHint;
    //property ShowSelectedBrush: boolean read FShowSelectedBrush write SetShowSelectedBrush default false;
    property Style;
    property Tools;

    property OnMouseEnter;
    property OnMouseLeave;
    property OnDropDown;

    property OnClick;
    property OnSelect;
    property OnHotTool;
    property OnDrawTool;
  end;

  TAdvSmoothShadowSelector = class(TAdvSmoothCustomSelector)
  private
    procedure Initialize;
  protected
    procedure Loaded; override;
    procedure SetSelectorPanel; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property AppearanceStyle;
  published
    property AutoThemeAdapt;
    property BorderColor;
    property BorderDownColor;
    property BorderHotColor;
    property BorderDropDownColor;
    property Caption;
    property Color;
    property ColorTo;
    property ColorDown;
    property ColorDownTo;
    property ColorHot;
    property ColorHotTo;
    property ColorDropDown;
    property ColorDropDownTo;
    property ColorSelected;
    property ColorSelectedTo;
    property DropDownButton;
    property Glyph;
    property GlyphHot;
    property GlyphDown;
    property ShowHint;
    property Style;
    property SelectedIndex;
    property Tools;

    property OnMouseEnter;
    property OnMouseLeave;
    property OnDropDown;

    property OnClick;
    property OnSelect;
    property OnHotTool;
    property OnDrawTool;
  end;

  TAdvSmoothTableBorderSelector = class(TAdvSmoothCustomSelector)
  private
    procedure Initialize;
  protected
    procedure SetSelectorPanel; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property AppearanceStyle;
  published
    property AutoThemeAdapt;
    property BorderColor;
    property BorderDownColor;
    property BorderHotColor;
    property BorderDropDownColor;
    property Caption;
    property Color;
    property ColorTo;
    property ColorDown;
    property ColorDownTo;
    property ColorHot;
    property ColorHotTo;
    property ColorDropDown;
    property ColorDropDownTo;
    property ColorSelected;
    property ColorSelectedTo;
    property DropDownButton;
    property Glyph;
    property GlyphHot;
    property GlyphDown;
    property Style;
    property ShowHint;
    property SelectedIndex;
    property Tools;

    property OnMouseEnter;
    property OnMouseLeave;
    property OnDropDown;

    property OnClick;
    property OnSelect;
    property OnHotTool;
    property OnDrawTool;
  end;


  TAdvSmoothGradientDirectionSelector = class(TAdvSmoothCustomSelector)
  private
    FStartColor: TColor;
    FEndColor: TColor;
    FShowSelectedGradient: boolean;
    procedure SetEndColor(const Value: TColor);
    procedure SetStartColor(const Value: TColor);
    procedure SelectorPanelOnDrawItem(Sender: TObject; Index: integer; R: TRect);
    procedure Initialize;
    procedure SetShowSelectedGradient(const Value: boolean);
  protected
    procedure SetSelectorPanel; override;
    procedure DrawGlyphAndCaption(aGlyph: TBitmap; R: TRect); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property AppearanceStyle;
  published
    property StartColor: TColor read FStartColor write SetStartColor;
    property EndColor: TColor read FEndColor write SetEndColor;

    property AutoThemeAdapt;
    property BorderColor;
    property BorderDownColor;
    property BorderHotColor;
    property BorderDropDownColor;
    property Caption;
    property Color;
    property ColorTo;
    property ColorDown;
    property ColorDownTo;
    property ColorHot;
    property ColorHotTo;
    property ColorDropDown;
    property ColorDropDownTo;
    property ColorSelected;
    property ColorSelectedTo;
    property DropDownButton;
    property Glyph;
    property GlyphHot;
    property GlyphDown;
    property Style;

    property SelectedIndex;
    property ShowSelectedGradient: boolean read FShowSelectedGradient write SetShowSelectedGradient default false;
    property Tools;
    property ShowHint;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnDropDown;

    property OnClick;
    property OnSelect;
    property OnHotTool;
  end;

  TAdvSmoothCustomColorSelector = class(TAdvSmoothCustomSelector)
  private
    FColorCubePanel: TAdvSmoothColorCubePanel;
    FSpectrumPanel: TAdvSmoothColorSpectrumPanel;
    FColorSelectionStyle: TColorSelectionStyle;
    FShowSelectedColor: Boolean;
    FSelectedColor: TColor;
    FShowRGBHint: Boolean;
    procedure SetColorSelectionStyle(const Value: TColorSelectionStyle);
    procedure SelectorPanelOnDrawItem(Sender: TObject; Index: integer; R: TRect);
    procedure SetShowSelectedColor(const Value: Boolean);

    procedure CubePanelOnSelect(Sender: TObject);
    procedure SpectrumPanelOnSelect(Sender: TObject);
    procedure SetSelectedColor(const Value: TColor);
    function GetSelectedColor: TColor;
  protected
    procedure Initialize; virtual;
    procedure Loaded; override;
    procedure OnToolSelect; override;
    procedure SetSelectorPanel; override;
    procedure DrawGlyphAndCaption(aGlyph: TBitmap; R: TRect); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property AppearanceStyle;
    property ButtonsPerRow;
    property SelectionStyle: TColorSelectionStyle read FColorSelectionStyle write SetColorSelectionStyle default csDiscrete;
    property ShowSelectedColor: Boolean read FShowSelectedColor write SetShowSelectedColor default true;
    property SelectedColor: TColor read GetSelectedColor write SetSelectedColor;
    property ShowRGBHint: Boolean read FShowRGBHint write FShowRGBHint;
  end;

  TAdvSmoothColorSelector = class(TAdvSmoothCustomColorSelector)
  protected
    procedure Initialize; override;
  public
    property SelectedIndex;
  published
    property SelectionStyle;
    property ShowSelectedColor;
    property SelectedColor;
    property ShowRGBHint;
    property AutoThemeAdapt;
    property BorderColor;
    property BorderDownColor;
    property BorderHotColor;
    property BorderDropDownColor;
    property Caption;
    property Color;
    property ColorTo;
    property ColorDown;
    property ColorDownTo;
    property ColorHot;
    property ColorHotTo;
    property ColorDropDown;
    property ColorDropDownTo;
    property ColorSelected;
    property ColorSelectedTo;
    property DropDownButton;
    property Glyph;
    property GlyphHot;
    property GlyphDown;
    property ShowHint;
    property Style;
    property Tools;

    property OnMouseEnter;
    property OnMouseLeave;
    property OnDropDown;
    property OnClick;
    property OnSelect;

  end;

  TAdvSmoothTextColorSelector = class(TAdvSmoothCustomColorSelector)
  protected
    procedure Initialize; override;
  public
  published
    property AutoThemeAdapt;
    property BorderColor;
    property BorderDownColor;
    property BorderHotColor;
    property BorderDropDownColor;
    property Caption;
    property Color;
    property ColorTo;
    property ColorDown;
    property ColorDownTo;
    property ColorHot;
    property ColorHotTo;
    property ColorDropDown;
    property ColorDropDownTo;
    property ColorSelected;
    property ColorSelectedTo;
    property DropDownButton;
    property Glyph;
    property GlyphHot;
    property GlyphDown;
    property ShowHint;
    property ShowSelectedColor;
    property SelectedColor;
    property Style;
    property Tools;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnDropDown;
    property OnClick;
    property OnSelect;

  end;

  TAdvSmoothCustomTableSelector = class(TAdvSmoothCustomSelector)
  private
    FTableSelectorPanel: TAdvSmoothTableSelectorPanel;
    FSelectedColumns: integer;
    FSelectedRows: integer;
    FDefaultRowCount: integer;
    FDefaultColCount: integer;
    FOnSelect: TNotifyEvent;
    FTextTable: string;
    FTextCancel: string;
    procedure SetDefaultColCount(const Value: integer);
    procedure SetDefaultRowCount(const Value: integer);
    //procedure SelectorPanelOnDrawItem(Sender: TObject; Index: integer; R: TRect);

    procedure TableSelectorOnSelect(Sender: TObject);
  protected
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    //procedure Initialize; virtual;
    procedure Loaded; override;
    //procedure OnToolSelect; override;
    procedure SetSelectorPanel; override;
    //procedure DrawGlyphAndCaption(aGlyph: TBitmap; R: TRect); override;

    property DefaultColCount: integer read FDefaultColCount write SetDefaultColCount default 5;
    property DefaultRowCount: integer read FDefaultRowCount write SetDefaultRowCount default 4;
    property SelectedColumns: integer read FSelectedColumns default 0;
    property SelectedRows: integer read FSelectedRows default 0;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    //property ButtonsPerRow;
    property TextTable: string read FTextTable write FTextTable;
    property TextCancel: string read FTextCancel write FTextCancel;
    property OnSelect: TNotifyEvent read FOnSelect write FOnSelect;
  end;

  TAdvSmoothTableSelector = class(TAdvSmoothCustomTableSelector)
  protected
  public
    property SelectedColumns;
    property SelectedRows;
    property AppearanceStyle;
  published
    property AutoThemeAdapt;
    property BorderColor;
    property BorderDownColor;
    property BorderHotColor;
    //property BorderDropDownColor;
    property Caption;
    property Color;
    property ColorTo;
    property ColorDown;
    property ColorDownTo;
    property ColorHot;
    property ColorHotTo;
    //property ColorDropDown;
    //property ColorDropDownTo;
    //property ColorSelected;
    //property ColorSelectedTo;
    property DefaultColCount;
    property DefaultRowCount;
    property DropDownButton;
    property Glyph;
    property GlyphHot;
    property GlyphDown;
    property ShowHint;
    property Style;
    property TextTable;
    property TextCancel;
    //property Tools;

    property OnMouseEnter;
    property OnMouseLeave;
    property OnDropDown;
    property OnClick;
    property OnSelect;
  end;

  TAdvSmoothCharacterSelector = class(TAdvSmoothCustomSelector)
  private
    FOnSelect: TNotifyEvent;
    FCharacters: string;
    FSelectedChar: char;
    FAutoLoad: boolean;
    FCharFont: TFont;
    procedure AddItemsFromChars;
    procedure LoadCharFromFont;
    procedure SetCharacters(const Value: string);
    procedure SetSelectedChar(const Value: char);
    procedure SetAutoLoad(const Value: boolean);
    procedure SetCharFont(const Value: TFont);
  protected
    //procedure Initialize; virtual;
    procedure Loaded; override;
    procedure OnToolSelect; override;
    procedure SetSelectorPanel; override;
    procedure DrawGlyphAndCaption(AGlyph: TBitmap; R: TRect); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property AppearanceStyle;
    property SelectedChar: char read FSelectedChar write SetSelectedChar;
    property Tools;
  published
    property AutoLoad: boolean read FAutoLoad write SetAutoLoad default true;
    property AutoThemeAdapt;
    property BorderColor;
    property BorderDownColor;
    property BorderHotColor;
    property BorderDropDownColor;
    property ButtonsPerRow;
    property Caption;
    property Color;
    property ColorTo;
    property ColorDown;
    property ColorDownTo;
    property ColorHot;
    property ColorHotTo;
    property ColorDropDown;
    property ColorDropDownTo;
    //property ColorSelected;
    //property ColorSelectedTo;

    property Characters: string read FCharacters write SetCharacters;
    property CharFont: TFont read FCharFont write SetCharFont;
    property DropDownButton;
    property Glyph;
    property GlyphHot;
    property GlyphDown;
    property ShowHint;
    property Style;

    property OnMouseEnter;
    property OnMouseLeave;
    property OnDropDown;
    property OnClick;
    property OnSelect: TNotifyEvent read FOnSelect write FOnSelect;
  end;


implementation


{$R AdvSmoothSelectors.RES}

uses
  ComObj;

const
  crTMSCur1 = 54;
  // theme changed notifier
  WM_THEMECHANGED = $031A;

type
  XPColorScheme = (xpNone, xpBlue, xpGreen, xpGray);

{$IFNDEF DELPHI7_LVL}
{$IFNDEF TMSDOTNET}
function GetFileVersion(FileName:string): Integer; var
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
      if (pvs^.dwSignature=$FEEF04BD) then
      begin
        Result := pvs^.dwFileVersionMS;
      end;
    end;
    FreeMem(buf);
  end;
end;
{$ENDIF}
{$ENDIF}

{$IFNDEF TMSDOTNET}
var
  GetCurrentThemeName: function(pszThemeFileName: PWideChar;
    cchMaxNameChars: Integer;
    pszColorBuff: PWideChar;
    cchMaxColorChars: Integer;
    pszSizeBuff: PWideChar;
    cchMaxSizeChars: Integer): THandle cdecl stdcall;
{$ENDIF}

{$IFNDEF TMSDOTNET}

function CurrentXPTheme: XPColorScheme;
var
  FileName, ColorScheme, SizeName: WideString;
  hThemeLib: THandle;
  IsThemeActive: function: BOOL cdecl stdcall;
begin
  hThemeLib := 0;
  Result := xpNone;

  try
    hThemeLib := LoadLibrary('uxtheme.dll');

    if hThemeLib > 0 then
    begin
      IsThemeActive := GetProcAddress(hThemeLib, 'IsThemeActive');

      if Assigned(IsThemeActive) then
        if IsThemeActive then
        begin
          GetCurrentThemeName := GetProcAddress(hThemeLib, 'GetCurrentThemeName');
          if Assigned(GetCurrentThemeName) then
          begin
            SetLength(FileName, 255);
            SetLength(ColorScheme, 255);
            SetLength(SizeName, 255);
            OleCheck(GetCurrentThemeName(PWideChar(FileName), 255,
              PWideChar(ColorScheme), 255, PWideChar(SizeName), 255));
            if (PWideChar(ColorScheme) = 'NormalColor') then
              Result := xpBlue
            else if (PWideChar(ColorScheme) = 'HomeStead') then
              Result := xpGreen
            else if (PWideChar(ColorScheme) = 'Metallic') then
              Result := xpGray
            else
              Result := xpNone;
          end;
        end;
    end;
  finally
    if hThemeLib <> 0 then
      FreeLibrary(hThemeLib);
  end;
end;
{$ENDIF}

{$IFDEF TMSDOTNET}

function CurrentXPTheme: XPColorScheme;
var
  FileName, ColorScheme, SizeName: StringBuilder;
  hThemeLib: THandle;
begin
  hThemeLib := 0;
  Result := xpNone;

  if IsThemeActive then
  begin
    FileName := StringBuilder.Create(255);
    SizeName := StringBuilder.Create(255);
    ColorScheme := StringBuilder.Create(255);
    GetCurrentThemeName(FileName, 255, ColorScheme, 255, SizeName, 255);
    if (ColorScheme.ToString = 'NormalColor') then
      Result := xpBlue
    else if (ColorScheme.ToString = 'HomeStead') then
      Result := xpGreen
    else if (ColorScheme.ToString = 'Metallic') then
      Result := xpGray
  end;
end;
{$ENDIF}

//----------------------------------------------------------------- DrawGradient

procedure DrawGradient(Canvas: TCanvas; FromColor, ToColor: TColor; Steps: Integer; R: TRect; Direction: Boolean);
var
  diffr, startr, endr: Integer;
  diffg, startg, endg: Integer;
  diffb, startb, endb: Integer;
  rstepr, rstepg, rstepb, rstepw: Real;
  i, stepw: Word;

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
    for i := 0 to steps - 1 do
    begin
      endr := startr + Round(rstepr * i);
      endg := startg + Round(rstepg * i);
      endb := startb + Round(rstepb * i);
      stepw := Round(i * rstepw);
      Pen.Color := endr + (endg shl 8) + (endb shl 16);
      Brush.Color := Pen.Color;
      if Direction then
        Rectangle(R.Left + stepw, R.Top, R.Left + stepw + Round(rstepw) + 1, R.Bottom)
      else
        Rectangle(R.Left, R.Top + stepw, R.Right, R.Top + stepw + Round(rstepw) + 1);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure ChangeBackAndForeGroundColors(bmp: TBitMap; ForeColor, BackColor, NewForeColor, NewBackColor: TColor);
var
  w, h: Integer;
begin
  if not Assigned(bmp) then
    Exit;

  for w := 0 to bmp.Width - 1 do
  begin
    for h := 0 to bmp.Height - 1 do
    begin
      if bmp.Canvas.Pixels[w,h] = ForeColor then
        bmp.Canvas.Pixels[w,h] := NewForeColor;
     { else if (bmp.Canvas.Pixels[w,h] = BackColor) and (NewBackColor <> clNone) then
        bmp.Canvas.Pixels[w,h] := NewBackColor; }
    end;
  end;
end;

//------------------------------------------------------------------------------

{ TAdvSmoothCustomSelector }

constructor TAdvSmoothCustomSelector.Create(AOwner: TComponent);
var
  i: integer;
begin
  inherited;
  FGlyph := TBitmap.Create;
  FGlyph.OnChange := GlyphChanged;
  FGlyphHot := TBitmap.Create;
  FGlyphDown := TBitmap.Create;
  FGlyphDisabled := TBitmap.Create;
  FGlyphShade := TBitmap.Create;

  // app is linked with COMCTL32 v6 or higher -> xp themes enabled
  i := GetFileVersion('COMCTL32.DLL');
  i := (i shr 16) and $FF;
  FIsThemed := (i > 5);

  if not (csDesigning in ComponentState) then
  begin
    FDropDownWindow := TSelectorDropDownWindow.CreateNew(self);
    FDropDownWindow.BorderIcons := [];
    FDropDownWindow.BorderStyle := bsNone;
    FDropDownWindow.Ctl3D := false;
    FDropDownWindow.FormStyle := fsStayOnTop;
    FDropDownWindow.Visible := False;
    FDropDownWindow.Width := 100;
    FDropDownWindow.Height := 100;
    FDropDownWindow.AutoScroll := true;

    FDropDownWindow.BorderWidth := 0;
    FDropDownWindow.OnHide := OnDropDownWindowHide;
  end;

  SetBounds(0, 0, 23, 22);
  ControlStyle := [csCaptureMouse, csDoubleClicks];
  ParentFont := True;
  Color := clBtnFace;

  FColorTo := clNone;
  FColorHot := $D6BEB5; //RGB(182, 189, 210);
  FColorHotTo := clNone;
  FColorDown := $B59584; //RGB(133, 146, 181);
  FColorDownTo := clNone;
  FColorChecked := clGray;
  FColorCheckedTo := clNone;
  FColorDropDown := $00F7F8F9;
  FColorDropDownTo := clNone;
  FColorSelected := $DED7D6;
  FColorSelectedTo := clNone;
  FColorSelectionHot := $D6BEB5; //RGB(246, 240, 216);
  FColorSelectionHotTo := clNone;
  FColorSelectionDown := $DED7D6; //RGB(255, 169, 64);
  FColorSelectionDownTo := clNone;
  FGradientDirection := gdVertical;
  FBorderColor := clNone;
  FBorderDownColor := $6B2408;
  FBorderHotColor := $6B2408;
  FBorderDropDownColor := clGray;
  FBorderSelectedColor := clBlack;
  Flat := True;
  FShaded := True;

  FTools := TAdvSmoothSelectorItems.Create(Self);
  FSelectedIndex := -1;
  FDupSelectedIndex := -1;

  FStyle := ssButton;
  FState := absUp;
  FDropDownBtnWidth := 12;
  FButtonsPerRow := 1;

  FTwoColorImages := False;
  FOldForeGroundImgColor := clBlack;
  FOldBkGroundImgColor := clWhite;
  FForeGroundImageColor := clBlack;
  FBackGroundImageColor := clNone;

  FStretchImageDraw := True;

  AppearanceStyle := esOffice2003Classic;
end;

//------------------------------------------------------------------------------

destructor TAdvSmoothCustomSelector.Destroy;
begin
  FGlyph.Free;
  FGlyphHot.Free;
  FGlyphDown.Free;
  FGlyphDisabled.Free;
  FGlyphShade.Free;

  FTools.Free;

  if not (csDesigning in ComponentState) then
  begin
    if assigned(FSelectorPanel) then
      FSelectorPanel.Free;
    FDropDownWindow.Free;
  end;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCustomSelector.Click;
begin
  inherited Click;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCustomSelector.CMFontChanged(var Message: TMessage);
begin
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCustomSelector.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if FFlat and not FMouseInControl and Enabled and (GetCapture = 0) then
  begin
    FMouseInControl := True;
    Repaint;

  end;

  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCustomSelector.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if FFlat and FMouseInControl and Enabled then
  begin
    FMouseInControl := False;
    Invalidate;
  end;

  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCustomSelector.CMTextChanged(var Message: TMessage);
begin
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCustomSelector.Loaded;
begin
  inherited Loaded;

  if FDupSelectedIndex < FTools.Count then
    SelectedIndex := FDupSelectedIndex;

  if AutoThemeAdapt and not (csDesigning in ComponentState) then
    ThemeAdapt;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCustomSelector.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
{var
  pt: TPoint;}
begin
  inherited MouseDown(Button, Shift, X, Y);

  FMouseDown := true;
  if Style in [ssButton {, ssCheck}] then
  begin
    if FDropDownButton and (X > (width - FDropDownBtnWidth)) then
    begin
      PopupBtnDown;
    end
    else
      ButtonDown;
  end
  else if Style = ssCombo then
  begin
    PopupBtnDown;
  end;

end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCustomSelector.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);

end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCustomSelector.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  FMouseDown := false;
  if (DropDownButton) and (Style = ssButton) and (x < Width - 8) then
  begin
    if Assigned(OnClick) then
      OnClick(Self);
  end;
  Invalidate;

end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCustomSelector.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;
  if (AOperation = opRemove) and (AComponent = FImages) then
  begin
    FImages := nil;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

function TAdvSmoothCustomSelector.DrawGlyph(aGlyph: TBitMap; aRect: TRect): integer;
var
  aP: TPoint;
begin
  Result := 0;
  if not aGlyph.Empty then
  begin
    if Caption = '' then
      aP := Point(((aRect.Right - aRect.Left) - aGlyph.Width) div 2, ((aRect.Bottom - aRect.Top) - aGlyph.Height) div 2)
    else
      aP := Point(2, ((aRect.Bottom - aRect.Top) - aGlyph.Height) div 2);

    if aGlyph.Width < (aRect.Right - aRect.Left) then
    begin
      aGlyph.Transparent := true;
      Canvas.Draw(aP.X, aP.Y, aGlyph);
      Result := aP.X + aGlyph.Width + 2;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCustomSelector.DrawGlyphAndCaption(aGlyph: TBitmap;
  R: TRect);
var
  CapR: TRect;
  tbmp: TBitmap;
  Rgn: HRGN;
begin
  CapR := R;
  if Style = ssButton then
  begin
    if not aGlyph.Empty then
    begin
      CapR.Left := DrawGlyph(aGlyph, R);
    end
    else
    begin
      if (Caption = '') and (SelectedIndex >= 0) and Assigned(Images) then
      begin
        tbmp := TBitmap.Create;
        tbmp.Width := FImages.width;
        tbmp.Height := FImages.Height;

        tbmp.Canvas.Brush.Color := clFuchsia;
        tbmp.Canvas.FillRect(rect(0, 0, FImages.Width, FIMages.Height));

        FImages.DrawingStyle := dsTransparent;
        FImages.Draw(tbmp.Canvas, 0, 0, Tools.Items[SelectedIndex].ImageIndex);

        tbmp.Transparent := true;
        tbmp.TransparentMode := tmAuto;

        if FStretchImageDraw then
        begin
          if (tbmp.Width < (R.Right - R.Left)) and (tbmp.Height < (R.Bottom - R.Top)) then
            DrawGlyph(tbmp, R)
          else
            Canvas.StretchDraw(Rect(R.Left + 2, R.Top + 2, R.Right - 2, R.Bottom - 2), tbmp);
        end
        else // not FStretchImageDraw
        begin
          Rgn := CreateRectRgn(R.Left+2, R.Top+2, R.Right-3, R.Bottom-2);
          SelectClipRgn(Canvas.Handle, Rgn);
          Canvas.Draw(R.Left, R.Top + 2, tbmp);
          SelectClipRgn(Canvas.Handle, 0);
          DeleteObject(Rgn);
        end;
        tbmp.Free;
      end;
    end;
    if Caption <> '' then
{$IFNDEF TMSDOTNET}
      DrawText(Canvas.Handle, PChar(Caption), -1, CapR, DT_SINGLELINE or DT_VCENTER);
{$ENDIF}
{$IFDEF TMSDOTNET}
    DrawText(Canvas.Handle, Caption, -1, CapR, DT_SINGLELINE or DT_VCENTER);
{$ENDIF}
  end
  else if Style = ssCombo then
  begin
    if (SelectedIndex >= 0) and (Caption = '') and Assigned(FImages) then
    begin
      if Tools.Items[SelectedIndex].ImageIndex > -1 then
      begin
        tbmp := TBitmap.Create;
        tbmp.Width := FImages.width; //((R.Right - R.Left)-6));
        tbmp.Height := FImages.Height; //min(FImages.Height, (R.Bottom - R.Top)-2);
        tbmp.Transparent := true;

        FImages.DrawingStyle := dsTransparent;
        FImages.Draw(tbmp.Canvas, 0, 0, Tools.Items[SelectedIndex].ImageIndex);

        Canvas.StretchDraw(Rect(R.Left + 2, R.Top + 2, R.Right - 2, R.Bottom - 2), tbmp);
        tbmp.Free;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCustomSelector.DrawButton;
var
  R, R2, BtnR, CapR: TRect;
  AP {, GP}: TPoint;

  procedure DrawArrow(ArP: TPoint; ArClr: TColor);
  begin
    Canvas.Pen.Color := ArClr;
    Canvas.MoveTo(ArP.X, ArP.Y);
    Canvas.LineTo(ArP.X + 5, ArP.Y);
    Canvas.MoveTo(ArP.X + 1, ArP.Y + 1);
    Canvas.LineTo(ArP.X + 4, ArP.Y + 1);
    Canvas.Pixels[ArP.X + 2, ArP.Y + 2] := ArClr;
  end;
begin
  R := Rect(0, 0, Width, Height);

  if Style in [ssButton {, ssCheck}] then
  begin
    if FDropDownButton then
    begin
      R2 := Rect(R.Left, R.Top, R.Right - FDropDownBtnWidth, R.Bottom);
      CapR := Rect(R.Left + 2, R.Top, R2.Right, R.Bottom);
      BtnR := Rect(R.Right - FDropDownBtnWidth, R.Top, R.Right, R.Bottom);
    end
    else
    begin
      R2 := R;
      CapR := Rect(R.Left + 2, R.Top, R2.Right, R.Bottom);
      BtnR := Rect(-1, -1, -1, -1);
    end;

    AP.X := BtnR.Left + ((BtnR.Right - BtnR.Left - 5) div 2) + 1;
    AP.Y := BtnR.Top + ((BtnR.Bottom - BtnR.Top - 3) div 2) + 1;

    if state = absUp then
    begin
      if FMouseInControl and not (csDesigning in ComponentState) then
      begin
        if FMouseDown then
        begin // Down
          if FDropDownButton then
          begin
            if ColorHotTo <> clNone then
              DrawGradient(Canvas, ColorHot, ColorHotTo, 16, R, false)
            else
            begin
              Canvas.Pen.Color := ColorHot;
              Canvas.Brush.Color := ColorHot;
              Canvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
            end;
            Canvas.Brush.Style := bsClear;

            if BorderHotColor <> clNone then
            begin
              Canvas.Pen.Color := BorderHotColor;
              Canvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
            end;

            Canvas.MoveTo(BtnR.Left, R.Top);
            Canvas.LineTo(BtnR.Left, R.Bottom);
            DrawArrow(AP, clBlack);
          end;

          if ColorDownTo <> clNone then
            DrawGradient(Canvas, ColorDown, ColorDownTo, 16, R2, false)
          else
          begin
            Canvas.Brush.Color := FColorDown;
            Canvas.Pen.Color := FColorDown;
            Canvas.Rectangle(R2.Left, R2.Top, R2.Right, R2.Bottom);
          end;
          Canvas.Brush.Style := bsClear;

          if BorderDownColor <> clNone then
          begin
            Canvas.Pen.Color := BorderDownColor;
            Canvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
          end;

          {
          if not GlyphDown.Empty then
            CapR.Left:= DrawGlyph(GlyphDown, R2)
          else if not Glyph.Empty then
            CapR.Left:= DrawGlyph(Glyph, R2);
          if Caption <> '' then
            DrawText(Canvas.Handle, PChar(Caption),-1, CapR, DT_SINGLELINE or DT_VCENTER);
          }

          if not GlyphDown.Empty then
            DrawGlyphAndCaption(GlyphDown, R2)
          else //if not Glyph.Empty then
            DrawGlyphAndCaption(Glyph, R2);

        end
        else // Hot
        begin
          if ColorHotTo <> clNone then
            DrawGradient(Canvas, ColorHot, ColorHotTo, 16, R, false)
          else
          begin
            Canvas.Pen.Color := ColorHot;
            Canvas.Brush.Color := ColorHot;
            Canvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
          end;

          Canvas.Brush.Style := bsClear;

          if BorderHotColor <> clNone then
          begin
            Canvas.Pen.Color := BorderHotColor;
            Canvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
          end;

          {
          if not GlyphHot.Empty then
            CapR.Left:= DrawGlyph(GlyphHot, R2)
          else if not Glyph.Empty then
            CapR.Left:= DrawGlyph(Glyph, R2);
          if Caption <> '' then
            DrawText(Canvas.Handle, PChar(Caption),-1, CapR, DT_SINGLELINE or DT_VCENTER);
          }

          if not GlyphHot.Empty then
            DrawGlyphAndCaption(GlyphHot, R2)
          else //if not Glyph.Empty then
            DrawGlyphAndCaption(Glyph, R2);

          if FDropDownButton then
          begin
            Canvas.MoveTo(BtnR.Left, R.Top);
            Canvas.LineTo(BtnR.Left, R.Bottom);
            DrawArrow(AP, clBlack);
          end;
        end;
      end
      else // Normal
      begin

        if ColorTo <> clNone then
          DrawGradient(Canvas, Color, ColorTo, 16, R, false)
        else
        begin
          Canvas.Pen.Color := Color;
          Canvas.Brush.Color := Color;
          Canvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
        end;

        Canvas.Brush.Style := bsClear;
        if BorderColor <> clNone then
        begin
          Canvas.Pen.Color := BorderColor;
          Canvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
        end;
        {
        if not Glyph.Empty then
          CapR.Left:= DrawGlyph(Glyph, R2);

        if Caption <> '' then
          DrawText(Canvas.Handle, PChar(Caption),-1, CapR, DT_SINGLELINE or DT_VCENTER);
        }

        DrawGlyphAndCaption(Glyph, R2);

        if FDropDownButton then
        begin
          Canvas.MoveTo(BtnR.Left, R.Top);
          Canvas.LineTo(BtnR.Left, R.Bottom);
          DrawArrow(AP, clBlack);
        end;
      end;
    end
    else if State = absDropDown then
    begin // DropDown State
      if (ColorTo <> clNone) then
        DrawGradient(Canvas, Color, ColorTo, 16, R, false)
      else
      begin
        Canvas.Pen.Color := Color;
        Canvas.Brush.Color := Color;
        Canvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
      end;

      {
      if ColorDropDownTo <> clNone then
        DrawGradient(Canvas, ColorDropDown, ColorDropDownTo, 16, R, true)
      else
      begin
        Canvas.Pen.Color := ColorDropDown;
        Canvas.Brush.Color := ColorDropDown;
        Canvas.Rectangle(R);
      end;
      }

      Canvas.Brush.Style := bsClear;

      if BorderDropDownColor <> clNone then
      begin
        Canvas.Pen.Color := BorderDropDownColor;
        Canvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
      end;

      {
      if not Glyph.Empty then
      begin
        if Caption = '' then
          GP:= Point(((R2.Right - R2.Left) - Glyph.Width) div 2, ((R2.Bottom - R2.Top) - Glyph.Height) div 2)
        else
          GP:= Point(2, ((R2.Bottom - R2.Top) - Glyph.Height) div 2);

        Glyph.Transparent:= true;
        Canvas.Draw(GP.X, GP.Y, Glyph);
        CapR.Left:= GP.X + Glyph.Width + 2;
      end;
      }

      {
      if not Glyph.Empty then
        CapR.Left:= DrawGlyph(Glyph, R2);

      if Caption <> '' then
        DrawText(Canvas.Handle, PChar(Caption),-1, CapR, DT_SINGLELINE or DT_VCENTER);
      }
      DrawGlyphAndCaption(Glyph, R2);


      if FDropDownButton then
      begin
        DrawArrow(AP, clBlack);
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TAdvSmoothCustomSelector.DoVisualStyles: Boolean;
begin
  if FIsThemed then
    Result := IsThemeActive
  else
    Result := False;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCustomSelector.DrawComboButton;
var
  R, R2, BtnR, CapR: TRect;
  AP: TPoint;

  procedure DrawArrow(ArP: TPoint; ArClr: TColor);
  begin
    Canvas.Pen.Color := ArClr;
    Canvas.MoveTo(ArP.X, ArP.Y);
    Canvas.LineTo(ArP.X + 5, ArP.Y);
    Canvas.MoveTo(ArP.X + 1, ArP.Y + 1);
    Canvas.LineTo(ArP.X + 4, ArP.Y + 1);
    Canvas.Pixels[ArP.X + 2, ArP.Y + 2] := ArClr;
  end;

  procedure DrawButton;
  var
    ARect: TRect;
    htheme: THandle;
  begin
    GetWindowRect(Handle, ARect);
    OffsetRect(ARect, -ARect.Left, -ARect.Top);
    Inc(ARect.Left, ClientWidth - FDropDownBtnWidth);
    InflateRect(ARect, -1, -1);

    if DoVisualStyles then
    begin
      htheme := OpenThemeData(Handle,'combobox');

      if not Enabled then
      begin
        {$IFNDEF TMSDOTNET}
        DrawThemeBackground(htheme,Canvas.Handle,CP_DROPDOWNBUTTON,CBXS_DISABLED,@ARect,nil)
        {$ENDIF}
        {$IFDEF TMSDOTNET}
        DrawThemeBackground(htheme,Canvas.Handle,CP_DROPDOWNBUTTON,CBXS_DISABLED,ARect,nil)
        {$ENDIF}
      end
      else
      begin
        if FMouseDown and (state = absUp) then
        begin
          {$IFNDEF TMSDOTNET}
          DrawThemeBackground(htheme,Canvas.Handle,CP_DROPDOWNBUTTON,CBXS_PRESSED,@ARect,nil)
          {$ENDIF}
          {$IFDEF TMSDOTNET}
          DrawThemeBackground(htheme,Canvas.Handle,CP_DROPDOWNBUTTON,CBXS_PRESSED,ARect,nil)
          {$ENDIF}
        end
        else
        begin
          {$IFNDEF TMSDOTNET}
          if not FMouseDown and FMouseInControl and not (state = absDown) then
            DrawThemeBackground(htheme,Canvas.Handle,CP_DROPDOWNBUTTON,CBXS_HOT,@ARect,nil)
          else
          DrawThemeBackground(htheme,Canvas.Handle,CP_DROPDOWNBUTTON,CBXS_NORMAL,@ARect,nil);
          {$ENDIF}
          {$IFDEF TMSDOTNET}
          if not FMouseDown and FMouseInControl and not (state = absDown) then
            DrawThemeBackground(htheme,Canvas.Handle,CP_DROPDOWNBUTTON,CBXS_HOT,ARect,nil)
          else
          DrawThemeBackground(htheme,Canvas.Handle,CP_DROPDOWNBUTTON,CBXS_NORMAL,ARect,nil);
          {$ENDIF}
        end;
      end;

      CloseThemeData(htheme);
    end
    else
    begin
      if Enabled then
        DrawFrameControl(Canvas.Handle, ARect, DFC_SCROLL, DFCS_SCROLLCOMBOBOX or DFCS_FLAT )
      else
        DrawFrameControl(Canvas.Handle, ARect, DFC_SCROLL, DFCS_SCROLLCOMBOBOX or DFCS_INACTIVE )
    end;

    //ExcludeClipRect(Canvas.Handle, ClientWidth - FButtonWidth -4 , 0, ClientWidth +2, ClientHeight);
  end;


begin
  R := Rect(0, 0, Width, Height);

  if Style = ssCombo then
  begin
    R2 := Rect(R.Left, R.Top, R.Right - FDropDownBtnWidth, R.Bottom);
    CapR := Rect(R.Left + 2, R.Top, R2.Right, R.Bottom);
    BtnR := Rect(R.Right - FDropDownBtnWidth - 1, R.Top + 1, R.Right - 1, R.Bottom - 1);

    AP.X := BtnR.Left + ((BtnR.Right - BtnR.Left - 5) div 2) + 1;
    AP.Y := BtnR.Top + ((BtnR.Bottom - BtnR.Top - 3) div 2) + 1;

    if AppearanceStyle = esXP then
    begin
      Canvas.Brush.Style := bsSolid;
      Canvas.Brush.Color := clWindow;
      Canvas.Pen.Color := BorderColor;
      Canvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);

      DrawButton;

      DrawGlyphAndCaption(Glyph, R2);
      Exit;
    end;


    if state = absUp then
    begin
      if FMouseInControl and not (csDesigning in ComponentState) then
      begin
        if FMouseDown then
        begin // Down

        end
        else // Hot
        begin
          Canvas.Pen.Color := clWhite;
          Canvas.Brush.Color := clWhite;
          Canvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);

          if ColorHotTo <> clNone then
            DrawGradient(Canvas, ColorHot, ColorHotTo, 16, BtnR, false)
          else
          begin
            Canvas.Pen.Color := ColorHot;
            Canvas.Brush.Color := ColorHot;
            Canvas.Rectangle(BtnR.Left, BtnR.Top, BtnR.Right, BtnR.Bottom);
          end;

          Canvas.Brush.Style := bsClear;

          if BorderHotColor <> clNone then
          begin
            Canvas.Pen.Color := BorderHotColor;
            Canvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
          end;
          {
          if not GlyphHot.Empty then
            CapR.Left:= DrawGlyph(GlyphHot, R2)
          else if not Glyph.Empty then
            CapR.Left:= DrawGlyph(Glyph, R2);
          }
          {
          if Caption <> '' then
            DrawText(Canvas.Handle, PChar(Caption),-1, CapR, DT_SINGLELINE or DT_VCENTER);
          }
          if not GlyphHot.Empty then
            DrawGlyphAndCaption(GlyphHot, R2)
          else
            DrawGlyphAndCaption(GlyphHot, R2);

          Canvas.MoveTo(BtnR.Left, R.Top);
          Canvas.LineTo(BtnR.Left, R.Bottom);
          DrawArrow(AP, clBlack);
        end;
      end
      else // Normal
      begin
        Canvas.Pen.Color := clWhite;
        Canvas.Brush.Color := clWhite;
        Canvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);

        if ColorTo <> clNone then
          DrawGradient(Canvas, Color, ColorTo, 16, Rect(BtnR.Left, BtnR.Top, BtnR.Right, BtnR.Bottom - 1), false)
        else
        begin
          Canvas.Pen.Color := Color;
          Canvas.Brush.Color := Color;
          Canvas.Rectangle(BtnR.Left, BtnR.Top, BtnR.Right, BtnR.Bottom);
        end;

        Canvas.Brush.Style := bsClear;
        if BorderColor <> clNone then
        begin
          Canvas.Pen.Color := BorderColor;
          Canvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
          Canvas.MoveTo(BtnR.Left, R.Top);
          Canvas.LineTo(BtnR.Left, R.Bottom);
        end;
        {
        if not Glyph.Empty then
          CapR.Left:= DrawGlyph(Glyph, R2);
        }
        {
        if Caption <> '' then
          DrawText(Canvas.Handle, PChar(Caption),-1, CapR, DT_SINGLELINE or DT_VCENTER);
        }
        DrawGlyphAndCaption(Glyph, R2);

        //Canvas.MoveTo(BtnR.Left, BtnR.Top);
        //Canvas.LineTo(BtnR.Left, BtnR.Bottom);
        DrawArrow(AP, clBlack);
      end;

    end
    else
    if State = absDropDown then
    begin // DropDown State
      Canvas.Pen.Color := clWhite;
      Canvas.Brush.Color := clWhite;
      Canvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);

      if ColorDownTo <> clNone then
        DrawGradient(Canvas, ColorDown, ColorDownTo, 16, BtnR, false)
      else
      begin
        Canvas.Pen.Color := ColorDown;
        Canvas.Brush.Color := ColorDown;
        Canvas.Rectangle(BtnR.Left, BtnR.Top, BtnR.Right, BtnR.Bottom);
      end;

      Canvas.Brush.Style := bsClear;

      if BorderDownColor <> clNone then
      begin
        Canvas.Pen.Color := BorderDownColor;
        Canvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
        Canvas.MoveTo(BtnR.Left, R.Top);
        Canvas.LineTo(BtnR.Left, R.Bottom);
      end;
      
      {
      if not Glyph.Empty then
        CapR.Left:= DrawGlyph(Glyph, R2);
      }
      {
      if Caption <> '' then
        DrawText(Canvas.Handle, PChar(Caption),-1, CapR, DT_SINGLELINE or DT_VCENTER);
      }

      DrawGlyphAndCaption(Glyph, R2);
      DrawArrow(AP, clBlack);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCustomSelector.Paint;
begin
  if Style in [ssButton {, ssCheck}] then
  begin
    Canvas.Font := Self.Font;
    if not FFlat then
    begin

    end
    else
    begin
      DrawButton;
    end;
  end
  else //Style = ssCombo
  begin
    DrawComboButton;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCustomSelector.SetAutoThemeAdapt(const Value: Boolean);
begin
  FAutoThemeAdapt := Value;

  if not (csDesigning in ComponentState) then
  begin
    if FAutoThemeAdapt then
      ThemeAdapt;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCustomSelector.ThemeAdapt;
var
  eTheme: XPColorScheme;
begin
  eTheme := CurrentXPTheme();
  case eTheme of
    xpBlue: AppearanceStyle := esOffice2003Blue;
    xpGreen: AppearanceStyle := esOffice2003Olive;
    xpGray: AppearanceStyle := esOffice2003Silver;
  else
    AppearanceStyle := esOffice2003Classic;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCustomSelector.WndProc(var Message: TMessage);
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

//------------------------------------------------------------------------------

procedure TAdvSmoothCustomSelector.SetAllowAllUp(const Value: Boolean);
begin
  if FAllowAllUp <> Value then
  begin
    FAllowAllUp := Value;
    //UpdateExclusive;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCustomSelector.SetBorderColor(const Value: TColor);
begin
  FBorderColor := Value;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCustomSelector.SetColorChecked(const Value: TColor);
begin
  FColorChecked := Value;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCustomSelector.SetColorCheckedTo(const Value: TColor);
begin
  FColorCheckedTo := Value;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCustomSelector.SetColorTo(const Value: TColor);
begin
  FColorTo := Value;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCustomSelector.SetDown(const Value: Boolean);
begin
{  if (FGroupIndex = 0) and (Style = tasButton) then
    Value := False;

  if (Style = tasCheck) then
  begin
    FDown := Value;
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
}
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCustomSelector.SetDropDownButton(const Value: Boolean);
begin
  FDropDownButton := Value;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCustomSelector.SetFlat(const Value: Boolean);
begin
  if Value <> FFlat then
  begin
    FFlat := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCustomSelector.SetGlyph(const Value: TBitmap);
var
  x, y: Integer;
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

//------------------------------------------------------------------------------

procedure TAdvSmoothCustomSelector.SetGlyphDisabled(const Value: TBitmap);
begin
  FGlyphDisabled.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCustomSelector.SetGlyphDown(const Value: TBitmap);
begin
  FGlyphDown.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCustomSelector.SetGlyphHot(const Value: TBitmap);
begin
  FGlyphHot.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCustomSelector.SetImages(const Value: TCustomImageList);
begin
  if Value <> FImages then
  begin
    FImages := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCustomSelector.SetShaded(const Value: Boolean);
begin
  FShaded := Value;

  if FShaded then
    if not (csLoading in ComponentState) then
    begin
      //GenerateShade;
    end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCustomSelector.SetStyle(const Value: TAdvSelectorStyle);
begin
  FStyle := Value;

  if (csDesigning in ComponentState) then
  begin
    if Value = ssButton then
      Width := 23
    else
      Width := 128;  
  end;

  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCustomSelector.SetGroupIndex(const Value: Integer);
begin
  if FGroupIndex <> Value then
  begin
    FGroupIndex := Value;
    //UpdateExclusive;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCustomSelector.GlyphChanged(Sender: TObject);
begin
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCustomSelector.SetAppearanceStyle(
  const Value: TAdvAppearanceStyle);
begin
  FAppearanceStyle := Value;
  FDropDownBtnWidth := 12;
  case FAppearanceStyle of
    esOffice2003Blue:
      begin
        Color := $FDEADA;
        ColorTo := $E4AE88;

        ColorSelected := RGB(255, 191, 113); //$94E6FB;
        ColorSelectedTo := clNone; //$1595EE;

        ColorDown := $087FE8;
        ColorDownTo := $7CDAF7;

        ColorHot := $DCFFFF;
        ColorHotTo := $5BC0F7;

        ColorSelectionHot := $C2EEFF; //RGB(246, 240, 216);
        ColorSelectionHotTo := clNone;

        ColorSelectionDown := $6FC0FF; // RGB(255, 169, 64);
        ColorSelectionDownTo := clNone;

        BorderColor := clNone;
        BorderHotColor := clBlack;
        BorderDropDownColor := $962D00;

        GradientDirection := gdVertical;
      end;
    esOffice2003Olive:
      begin
        Color := $CFF0EA;
        ColorTo := $8CC0B1;

        ColorSelected := RGB(255, 191, 113); //$94E6FB;
        ColorSelectedTo := clNone; //$1595EE;

        ColorDown := $087FE8;
        ColorDownTo := $7CDAF7;

        ColorHot := $DCFFFF;
        ColorHotTo := $5BC0F7;

        ColorSelectionHot := $C2EEFF; //RGB(246, 240, 216);
        ColorSelectionHotTo := clNone;

        ColorSelectionDown := $6FC0FF; // RGB(255, 169, 64);
        ColorSelectionDownTo := clNone;

        BorderColor := clNone;
        BorderHotColor := clBlack;
        BorderDropDownColor := $5E8D75;

        GradientDirection := gdVertical;
      end;
    esOffice2003Silver:
      begin
        Color := $ECE2E1;
        ColorTo := $B39698;
        ColorSelected := RGB(255, 191, 113); //$94E6FB;
        ColorSelectedTo := clNone; //$1595EE;

        ColorDown := $087FE8;
        ColorDownTo := $7CDAF7;

        ColorHot := $DCFFFF;
        ColorHotTo := $5BC0F7;

        ColorSelectionHot := $C2EEFF; //RGB(246, 240, 216);
        ColorSelectionHotTo := clNone;

        ColorSelectionDown := $6FC0FF; // RGB(255, 169, 64);
        ColorSelectionDownTo := clNone;

        BorderColor := clNone;
        BorderHotColor := clBlack;
        BorderDropDownColor := $947C7C;

        GradientDirection := gdVertical;
      end;
    esOffice2003Classic:
      begin
        Color := clBtnFace;
        ColorTo := clNone;

        ColorSelected := $DED7D6;
        ColorSelectedTo := clNone;

        ColorDown := $B59584;
        ColorDownTo := clNone; //$B59285;

        ColorHot := $D6BEB5;
        ColorHotTo := clNone; //$D2BDB6;

        ColorSelectionHot := $D6BEB5; //RGB(246, 240, 216);
        ColorSelectionHotTo := clNone;

        ColorSelectionDown := $DED7D6; // RGB(133, 146, 181);
        ColorSelectionDownTo := clNone;

        BorderColor := clNone;
        BorderHotColor := clBlack;
        BorderDropDownColor := clGray;

        GradientDirection := gdVertical;
      end;

    esWhidbey:
      begin
        Color := $EBEEEF;//clBtnFace;
        ColorTo := $7E9898;//clNone;

        ColorSelected := $94E6FB;//$DED7D6;
        ColorSelectedTo := $1595EE;//clNone;

        ColorDown := $087FE8;//$B59584;
        ColorDownTo := $7CDAF7;//clNone; //$B59285;

        ColorHot := $DCFFFF;//$D6BEB5;
        ColorHotTo := $5BC0F7;//clNone; //$D2BDB6;

        ColorSelectionHot := $94E6FB;//$D6BEB5; //RGB(246, 240, 216);
        ColorSelectionHotTo := clNone;

        ColorSelectionDown := $DCFFFF;//$DED7D6; // RGB(133, 146, 181);
        ColorSelectionDownTo := clNone;

        BorderColor := $962D00;//clNone;
        BorderHotColor := clBlack;
        BorderDropDownColor := clGray;

        GradientDirection := gdVertical;

        AutoThemeAdapt := false;
      end;
    esCustom:
      begin
        AutoThemeAdapt := false;
      end;
    esXP:
      begin
        Color := clBtnFace;
        ColorTo := clNone;

        ColorSelected := $DED7D6;
        ColorSelectedTo := clNone;

        ColorDown := $B59584;
        ColorDownTo := clNone; //$B59285;

        ColorHot := $D6BEB5;
        ColorHotTo := clNone; //$D2BDB6;

        ColorSelectionHot := $D6BEB5; //RGB(246, 240, 216);
        ColorSelectionHotTo := clNone;

        ColorSelectionDown := $DED7D6; // RGB(133, 146, 181);
        ColorSelectionDownTo := clNone;

        BorderColor := clNone;

        if FIsThemed and (Style = ssCombo) then
        begin
          BorderColor := $B99D7F;
        end;
        
        if not FIsThemed and (Style = ssCombo) then
        begin
          BorderColor := clBlack;
        end;

        if FIsThemed  then
        begin
          BorderHotColor := $B99D7F;
          BorderDropDownColor := $B99D7F;
        end
        else
        begin
          BorderHotColor := clBlack;
          BorderDropDownColor := clBlack;
        end;



        AutoThemeAdapt := false;
        FDropDownBtnWidth := 18;
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCustomSelector.ButtonDown;
begin
  if not FDropDownButton then
    DoDropDown
  else
  begin
    if Assigned(FOnSelect) then
    begin
      if Assigned(FSelectorPanel) then
      begin
        if (FSelectorPanel.ItemIndex >= 0) and (FSelectorPanel.ItemIndex < Tools.Count) then
          FOnSelect(Self, FSelectorPanel.ItemIndex, Tools[FSelectorPanel.ItemIndex])
        else
          FOnSelect(Self, FSelectorPanel.ItemIndex, nil)
      end
      else
        FOnSelect(Self, -1, nil)

    end;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCustomSelector.DoDropDown;
var
  R: TRect;
  P: TPoint;
begin
  if not Assigned(FDropDownWindow) then
    Exit;

  if FDropDownWindow.Visible then
  begin
    HideDropDown;
    Exit;
  end;

  SetSelectorPanel;
{$IFNDEF TMSDOTNET}
  FDropDownWindow.Position := poDesigned;
{$ENDIF}  
  FDropDownWindow.SetWindowSize;

{$IFNDEF TMSDOTNET}
  SystemParametersInfo(SPI_GETWORKAREA, 0, @R, 0);
{$ENDIF}
{$IFDEF TMSDOTNET}
  SystemParametersInfo(SPI_GETWORKAREA, 0, R, 0);
{$ENDIF}

  P := Point(0, self.Height);
  P := ClientToScreen(P);

  if R.Bottom > (P.Y + FDropDownWindow.Height + 2) then
  begin
    FDropDownWindow.Left := P.X;
    if FDropDownWindow.Left + FDropDownWindow.Width > R.Right then
    begin
      FDropDownWindow.Left:= (P.X+ Width) - FDropDownWindow.Width;
      FDropDownWindow.ShowLeft:= true;
    end
    else
      FDropDownWindow.ShowLeft:= false;

    FDropDownWindow.Top := P.Y - 1;
    FDropDownWindow.ShowAbove := false;
  end
  else
  begin
    FDropDownWindow.Left := P.X;
    if FDropDownWindow.Left + FDropDownWindow.Width > R.Right then
    begin
      FDropDownWindow.Left:= (P.X+ Width) - FDropDownWindow.Width;
      FDropDownWindow.ShowLeft:= true;
    end
    else
      FDropDownWindow.ShowLeft:= false;

    FDropDownWindow.Top := P.Y - self.Height - FDropDownWindow.Height + 1;
    FDropDownWindow.ShowAbove := true;
  end;

  FDropDownWindow.ShowFullBorder := Style = ssCombo;

  State := absDropDown;
  //SetSelectorPanel;
  //FDropDownWindow.SetWindowSize;

  //FDropDownWindow.Width:= 100;
  FDropDownWindow.Visible := true;

  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCustomSelector.HideDropDown;
begin
  FDropDownWindow.Visible := false;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCustomSelector.PopupBtnDown;
begin
  DoDropDown;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCustomSelector.SetColorDropDown(const Value: TColor);
begin
  FColorDropDown := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCustomSelector.SetColorDropDownTo(const Value: TColor);
begin
  FColorDropDownTo := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCustomSelector.SetSelectorPanelItems;
var
  i: integer;
begin
  FSelectorPanel.Items.Clear;
  for i := 0 to Tools.Count - 1 do
  begin
    with FselectorPanel.Items.Add do
    begin
      Caption := Tools.Items[i].Caption;
      CaptionAlignment := Tools.Items[i].CaptionAlignment;
      ImageIndex := Tools.Items[i].ImageIndex;
      Hint := Tools.Items[i].Hint;
      Value := Tools.Items[i].Value;
      Enable := Tools.Items[i].Enable;
      ItemType := Tools.Items[i].ItemType;
      BackGroundColor := Tools.Items[i].BackGroundColor;
    end;
  end;

  FSelectorPanel.ItemIndex := FSelectedIndex;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCustomSelector.SetSelectorPanel;
begin
  if not assigned(FSelectorPanel) and assigned(FDropDownWindow) then
  begin
    FSelectorPanel := TAdvSmoothSelectorPanel.Create(FDropDownWindow);
    FSelectorPanel.Parent := FDropDownWindow;
  end;
  FSelectorPanel.WindowBorderColor := FBorderDropDownColor;
  FSelectorPanel.OnShouldHide := OnDropDownPanelShouldHide;
  FSelectorPanel.OnSelect := OnDropDownPanelSelect;
  FSelectorPanel.OnHotTool := OnDropDownPanelHotTool;

  if Assigned(FOnDrawTool) then
    FSelectorPanel.OnDrawItem := OnDropDownPanelDrawTool
  else
    FSelectorPanel.OnDrawItem := nil;

  FSelectorPanel.Color := ColorDropDown;
  FSelectorPanel.ColorTo := ColorDropDownTo;

  FSelectorPanel.BorderColor := BorderColor;
  FSelectorPanel.BorderDownColor := FBorderDownColor;
  FSelectorPanel.BorderHotColor := BorderHotColor;
  FSelectorPanel.BorderSelectedColor := BorderSelectedColor;


  FSelectorPanel.ColorDown := FColorSelectionDown; // ColorDown;
  FSelectorPanel.ColorDownTo := FColorSelectionDownTo; //ColorDownTo;
  FSelectorPanel.ColorHot := FColorSelectionHot; //  ColorHot;
  FSelectorPanel.ColorHotTo := FColorSelectionHotTo; //ColorHotTo;
  FSelectorPanel.ColorSelected := ColorSelected;
  FSelectorPanel.ColorSelectedTo := ColorSelectedTo;

  FSelectorPanel.Images := Images;


  SetSelectorPanelItems;
  FSelectorPanel.ButtonsPerRow := ButtonsPerRow;

  FSelectorPanel.SetItemsPosition;
  FDropDownWindow.SelectorPanel := FSelectorPanel;

  FSelectorPanel.Left := 0; //1;
  FSelectorPanel.Top := 0; //1;



  FSelectorPanel.TwoColorImages := self.TwoColorImages;
  
  //FSelectorPanel.Height:= 140;
  //FSelectorPanel.Width:= 120;

  //FSelectorPanel.Color:= clyellow;

end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCustomSelector.SetDropDownCount(const Value: integer);
begin
  FDropDownCount := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCustomSelector.SetTGradientDirection(
  const Value: TGradientDirection);
begin
  FGradientDirection := Value;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCustomSelector.OnDropDownWindowHide(Sender: TObject);
begin
  State := absUp;
  Invalidate;
end;

//------------------------------------------------------------------------------

function TAdvSmoothCustomSelector.GetCaption: string;
begin
  Result := inherited Caption;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCustomSelector.SetCaption(const Value: string);
begin
  inherited Caption := Value;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCustomSelector.OnDropDownPanelShouldHide(Sender: TObject);
begin
  HideDropDown;
end;

//------------------------------------------------------------------------------

function TAdvSmoothCustomSelector.GetSelectedIndex: integer;
begin
  Result := FSelectedIndex;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCustomSelector.SetSelectedIndex(const Value: integer);
begin
  if Value < Tools.Count then
  begin
    FSelectedIndex := Value;
    //OnToolSelect;
    Invalidate;
  end;
  FDupSelectedIndex := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCustomSelector.SetTools(const Value: TAdvSmoothSelectorItems);
begin
  FTools := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCustomSelector.OnDropDownPanelSelect(Sender: TObject);
begin
  SelectedIndex := FSelectorPanel.ItemIndex;
  OnToolSelect;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCustomSelector.SetButtonsPerRow(const Value: TNoOfButtons);
begin
  FButtonsPerRow := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCustomSelector.OnDropDownPanelHotTool(Sender: TObject;
  HotItemIndex: integer);
begin
  if Assigned(FOnHotTool) then
    FOnHotTool(self, HotItemIndex);
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCustomSelector.OnToolSelect;
begin
  if Assigned(FOnSelect) then
    FOnSelect(Self, FSelectorPanel.ItemIndex, Tools[FSelectorPanel.ItemIndex]);
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCustomSelector.OnDropDownPanelDrawTool(Sender: TObject;
  ItemIndex: integer; R: TRect);
begin
  if Assigned(FOnDrawTool) then
    FOnDrawTool(self, FSelectorPanel.Canvas, ItemIndex, R);
end;

//------------------------------------------------------------------------------

function TAdvSmoothCustomSelector.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER, REL_VER), MakeWord(MIN_VER, MAJ_VER));
end;

//------------------------------------------------------------------------------

function TAdvSmoothCustomSelector.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn))) + '.' + IntToStr(Lo(Hiword(vn))) + '.' + IntToStr(Hi(Loword(vn))) + '.' + IntToStr(Lo(Loword(vn)));
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCustomSelector.SetVersion(const Value: string);
begin

end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCustomSelector.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if (Key = VK_F2) then
    DoDropDown;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCustomSelector.ShowDropDown;
begin
  DoDropDown;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCustomSelector.SetTwoColorImages(const Value: Boolean);
begin
  FTwoColorImages := Value;
  if FTwoColorImages then
    ChangeImagesColor(FForeGroundImageColor, FBackGroundImageColor);
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCustomSelector.ChangeImagesColor(ForeGColor,
  BkGColor: TColor);
var
  I: Integer;
  bmp: TBitMap;
  ImgList: TCustomImageList;
begin
  if Assigned(FImages) then
  begin
    ImgList:= TCustomImageList.Create(self);
    ImgList.Width := FImages.Width;
    ImgList.Height := FImages.Height;
    for I := 0 to FImages.Count-1 do
    begin
      bmp := TBitMap.Create;
      bmp.Width := FImages.Width;
      bmp.Height := FImages.Height;

      {$IFDEF DELPHI7_LVL}
      if FImages.GetBitmap(I, bmp) then
      {$ELSE}
      FImages.GetBitmap(I, bmp);
      {$ENDIF}
      begin
        ChangeBackAndForeGroundColors(bmp, FOldForeGroundImgColor, FOldBkGroundImgColor, ForeGroundImageColor, BackGroundImageColor);
        bmp.TransparentMode := tmAuto;
        bmp.Transparent := True;
        ImgList.Add(bmp, nil);
      end;
      bmp.Free;
    end;
    FImages.Clear;
    FImages.AddImages(ImgList);
    ImgList.Free;
    FOldForeGroundImgColor := ForeGroundImageColor;
    FOldBkGroundImgColor := BackGroundImageColor;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCustomSelector.SetForeGroundImageColor(const Value: TColor);
begin
  if FForeGroundImageColor <> Value then
  begin
    FForeGroundImageColor := Value;
    TwoColorImages := TwoColorImages; // To reflect color change
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCustomSelector.SetStretchImageDraw(const Value: Boolean);
begin
  if FStretchImageDraw <> Value then
  begin
    FStretchImageDraw := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

{ TSelectorDropDownWindow }

constructor TSelectorDropDownWindow.Create(AOwner: TComponent);
begin
  inherited;
  {BorderIcons:= [];
  BorderStyle:= bsNone;
  Ctl3D:= false;
  Color:= clGray;      }
  FHideOnDeActivate := true;
  FHideTimer := TTimer.Create(self);
  FHideTimer.Interval := 1;
  FHideTimer.Enabled := false;
  FHideTimer.OnTimer := HideTimerOnTime;
end;

//------------------------------------------------------------------------------

constructor TSelectorDropDownWindow.CreateNew(AOwner: TComponent; Dummy: Integer);
begin
  inherited;
  FOwner := AOwner;
  FHideOnDeActivate := true;
  FHideTimer := TTimer.Create(self);
  FHideTimer.Interval := 1;
  FHideTimer.Enabled := false;
  FHideTimer.OnTimer := HideTimerOnTime;
end;

//------------------------------------------------------------------------------

procedure TSelectorDropDownWindow.CreateParams(var Params: TCreateParams);
const
  CS_DROPSHADOW = $00020000;
begin
  inherited CreateParams(Params);
  //Params.Style := Params.Style - WS_BORDER;
  {
  if (Win32Platform = VER_PLATFORM_WIN32_NT) and
     ((Win32MajorVersion > 5) or
      ((Win32MajorVersion = 5) and (Win32MinorVersion >= 1))) then
    Params.WindowClass.Style := Params.WindowClass.Style or CS_DROPSHADOW;

  Params.ExStyle := Params.ExStyle or WS_EX_TOPMOST; }
end;

//------------------------------------------------------------------------------

destructor TSelectorDropDownWindow.Destroy;
begin
  FHideTimer.Free;
  inherited;
end;

//------------------------------------------------------------------------------

function TSelectorDropDownWindow.GetParentWnd: HWnd;
var
  Last, P: HWnd;
begin
  P := GetParent((Owner as TWinControl).Handle);
  Last := P;
  while P <> 0 do
  begin
    Last := P;
    P := GetParent(P);
  end;
  Result := Last;
end;

//------------------------------------------------------------------------------

procedure TSelectorDropDownWindow.HideTimerOnTime(Sender: TObject);
begin
  Hide;
  FHideTimer.Enabled := false;
end;

procedure TSelectorDropDownWindow.Paint;
begin
  inherited;
{  Canvas.Brush.Style:= bsClear;
  Canvas.Pen.Color:= clGray;

  Canvas.MoveTo(0, 0);
  Canvas.LineTo(0, Height);

  if ShowAbove then
  begin
    Canvas.MoveTo(0, 0);
    Canvas.LineTo(Width-1, 0);

    Canvas.MoveTo(TWinControl(FOwner).Width, Height-1);
    Canvas.LineTo(Width-1, Height-1);
  end
  else
  begin
    Canvas.MoveTo(TWinControl(FOwner).Width-1, 0);
    Canvas.LineTo(Width, 0);

    Canvas.MoveTo(0, Height-1);
    Canvas.LineTo(Width-1, Height-1);
  end;

  Canvas.MoveTo(Width-1, Height);
  Canvas.LineTo(Width-1, 0); }
end;

//------------------------------------------------------------------------------

procedure TSelectorDropDownWindow.SetWindowSize;
begin
  if Assigned(FSelectorPanel) then
  begin
    Height := FSelectorPanel.GetVisibleHeight; //+2;
    //ClientHeight:= FSelectorPanel.GetVisibleHeight + 1;
    Width := FSelectorPanel.Width; // + 2;
  end;
end;

//------------------------------------------------------------------------------

procedure TSelectorDropDownWindow.WMActivate(var Message: TWMActivate);
begin
  inherited;

  if Message.Active = integer(False) then
  begin
    if HideOnDeactivate and Visible then
    begin
      Hide;
      FHideTimer.Enabled := true;
    end;
  end
  else if Assigned(FSelectorPanel) then
  begin
    FSelectorPanel.SetFocus;
    SendMessage(getParentWnd, WM_NCACTIVATE, 1, 0);
  end;
end;

//------------------------------------------------------------------------------

procedure TSelectorDropDownWindow.WMNCHitTest(var Message: TWMNCHitTest);
var
  pt: TPoint;
begin
  // Make the hint sizable
  pt := ScreenToClient(Point(Message.XPos, Message.YPos));

  if (pt.X > Width - 10) and (pt.Y > Height - 10) then
    message.Result := HTBOTTOMRIGHT
end;

//------------------------------------------------------------------------------


{ TAdvSmoothCustomSelectorPanel }

constructor TAdvSmoothCustomSelectorPanel.Create(AOwner: TComponent);
begin
  inherited;
  FOwner := AOwner;
  BevelOuter := bvNone;
  BevelWidth := 1;
  Color := $00F7F8F9;
  FColorTo := clNone;
  FWindowBorderColor := clGray;
  FGradientDirection := gdHorizontal;
end;

//------------------------------------------------------------------------------

function TAdvSmoothCustomSelectorPanel.GetVisibleHeight: integer;
begin
  Result := Height;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCustomSelectorPanel.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCustomSelectorPanel.Paint;
var
  R: TRect;
begin
  inherited;
  R := Rect(0, 0, Width, Height);

  if ColorTo <> clNone then
    DrawGradient(Canvas, Color, ColorTo, 40, R, FGradientDirection = gdHorizontal);

  Canvas.Brush.Style := bsClear;
  Canvas.Pen.Color := FWindowBorderColor;

  Canvas.MoveTo(0, 0);
  Canvas.LineTo(0, Height);

  if TSelectorDropDownWindow(FOwner).ShowFullBorder then
  begin
    Canvas.MoveTo(0, 0);
    Canvas.LineTo(Width - 1, 0);
    Canvas.MoveTo(0, Height - 1);
    Canvas.LineTo(Width - 1, Height - 1);
  end
  else
  begin
    if TSelectorDropDownWindow(FOwner).ShowAbove then
    begin
      Canvas.MoveTo(0, 0);
      Canvas.LineTo(Width - 1, 0);

      if TSelectorDropDownWindow(FOwner).ShowLeft then
      begin
        Canvas.MoveTo(0, Height - 1);
        Canvas.LineTo(Width - TWinControl(TSelectorDropDownWindow(Owner).Owner).Width + 1, Height - 1);
      end
      else
      begin
        Canvas.MoveTo(TWinControl(TSelectorDropDownWindow(Owner).Owner).Width-1, Height - 1);
        Canvas.LineTo(Width - 1, Height - 1);
      end;
    end
    else
    begin
      if TSelectorDropDownWindow(FOwner).ShowLeft then
      begin
        Canvas.MoveTo(0, 0);
        Canvas.LineTo(Width - TWinControl(TSelectorDropDownWindow(Owner).Owner).Width + 1, 0);
      end
      else
      begin
        Canvas.MoveTo(TWinControl(TSelectorDropDownWindow(Owner).Owner).Width - 1, 0);
        Canvas.LineTo(Width, 0);
      end;

      Canvas.MoveTo(0, Height - 1);
      Canvas.LineTo(Width - 1, Height - 1);
    end;
  end;

  Canvas.MoveTo(Width - 1, Height);
  Canvas.LineTo(Width - 1, {0}-1);
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCustomSelectorPanel.SetColorTo(const Value: TColor);
begin
  FColorTo := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCustomSelectorPanel.SetTGradientDirection(
  const Value: TGradientDirection);
begin
  FGradientDirection := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCustomSelectorPanel.SetWindowBorderColor(
  const Value: TColor);
begin
  FWindowBorderColor := Value;
end;

//------------------------------------------------------------------------------


{ TAdvSmoothSelectorPanel }

constructor TAdvSmoothSelectorPanel.Create(AOwner: TComponent);
begin
  inherited;
  FItems := TAdvSmoothSelectorItems.Create(self);
  FColorHot := RGB(182, 189, 210);
  FColorHotTo := clNone;
  FColorDown := RGB(255, 169, 64); //RGB(133,146,181);
  FColorDownTo := clNone;
  FColorSelected := RGB(212, 213, 216);
  FColorSelectedTo := clNone;
  FBorderColor := clNone;
  FBorderDownColor := clBlack;
  FBorderHotColor := clBlack;
  FBorderSelectedColor := clBlack;
  FHotItemIndex := -1;
  FDownItemIndex := -1;
  FNoPrefix := false;

  FItemIndex := -1;

  FButtonHeight := 20;
  FTopOffSet := 4;
  FLeftOffSet := 4;
  FButtonMargin := 3;
  FButtonsPerRow := 1;

  ShowHint := true;
  FMaxCaptionLength := 0;

  FMinButtonWidth := MINBUTTONSIZE;
  FMinButtonHeight := MINBUTTONSIZE;

  FTwoColorImages := False;
end;

//------------------------------------------------------------------------------

destructor TAdvSmoothSelectorPanel.Destroy;
begin
  FItems.Free;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothSelectorPanel.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  i, j: integer;
begin
  inherited;
  i := ItemAtPos(X, Y);
  if (i >= 0) then
  begin
    if (FHotItemIndex <> i) then
    begin
      if (FHotItemIndex >= 0) then
      begin
        j := FHotItemIndex;
        FHotItemIndex := -1;
        if FDownItemIndex > -1 then // means mouse down move
          FDownItemIndex := i;
        DrawItem(j, true);
      end;

      FHotItemIndex := i;
      if FMouseDown then // means mouse down move
        FDownItemIndex := i;
      DrawItem(i, true);

      //if FItems.Items[i].Hint <> '' then
      begin
        Hint := FItems.Items[i].Hint;
        Application.CancelHint;
      end;

      if Assigned(FOnHotTool) then
        FOnHotTool(self, i);
    end;
  end
  else if (FHotItemIndex >= 0) then
  begin
    j := FHotItemIndex;
    FHotItemIndex := -1;
    if FDownItemIndex > -1 then // means mouse down move
      FDownItemIndex := -1;
    DrawItem(j, true);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothSelectorPanel.DrawItem(Index: integer; RefreshItem: boolean = false);
var
  Gr, R: TRect;
  DTSTYLE: dword;
  bmp: TBitMap;
  DR: TRect;
begin
  if csDesigning in ComponentState then
    Exit;

  DTSTYLE := DT_SINGLELINE or DT_VCENTER;

  if FNoPrefix then
    DTSTYLE := DTSTYLE or DT_NOPREFIX;  

  R := FItems.Items[Index].ItemRect;
  Gr := FItems.Items[Index].ItemRect;
  Gr.Left := Gr.Left + ButtonMargin;
  Gr.Top := Gr.Top; // + ButtonMargin;

  if (Index = FDownItemIndex) then
  begin
    if self.FColorDownTo <> clNone then
      DrawGradient(Canvas, ColorDown, ColorDownTo, 16, FItems.Items[Index].ItemRect, true)
    else
    begin
      Canvas.Brush.Color := ColorDown;
      Canvas.Pen.Color := ColorDown;
      DR := FItems.Items[Index].ItemRect;
      Canvas.Rectangle(DR.Left, DR.Top, DR.Right, DR.Bottom);
    end;

    if BorderDownColor <> clNone then
    begin
      Canvas.Brush.Style := bsClear;
      Canvas.Pen.Color := BorderDownColor;
      DR := FItems.Items[Index].ItemRect;
      Canvas.Rectangle(DR.Left, DR.Top, DR.Right, DR.Bottom);
    end;

    Canvas.Brush.Style := bsClear;
    Canvas.Font.Color := clWhite;

    if not Assigned(FOnDrawItem) and (FItems.Items[Index].CaptionAlignment = taLeftJustify) and (FItems.Items[Index].Caption <> '') then
    begin
      //Gr.Left := 2 + Gr.Left + DrawText(Canvas.Handle, PChar(FItems.Items[Index].Caption),-1, Gr, DT_SINGLELINE or DT_VCENTER);
{$IFNDEF TMSDOTNET}
      DrawText(Canvas.Handle, PChar(FItems.Items[Index].Caption), -1, Gr, DTSTYLE);
{$ENDIF}
{$IFDEF TMSDOTNET}
      DrawText(Canvas.Handle, FItems.Items[Index].Caption, -1, Gr, DTSTYLE);
{$ENDIF}
    end;

    if not Assigned(FOnDrawItem) and Assigned(FImages) and (FItems.Items[Index].ImageIndex >= 0) then
    begin
      if TwoColorImages then
      begin
        bmp := TBitmap.Create;
        bmp.Width := FImages.width;
        bmp.Height := FImages.Height;

        bmp.Canvas.Brush.Color := clFuchsia;
        bmp.Canvas.FillRect(rect(0, 0, FImages.Width, FIMages.Height));

        FImages.DrawingStyle := dsTransparent;
        FImages.Draw(bmp.Canvas, 0, 0, FItems.Items[Index].ImageIndex);

        bmp.Transparent := true;
        bmp.TransparentMode := tmAuto;

        if (FItems.Items[Index].CaptionAlignment = taLeftJustify) then
          Canvas.Draw(fmaxCaptionLength + Gr.left + 2, Gr.Top + 2, bmp)
        else
          Canvas.Draw(Gr.left, Gr.Top + 2, bmp);
        bmp.Free;
      end
      else
      begin
        if (FItems.Items[Index].CaptionAlignment = taLeftJustify) then
          FImages.Draw(Canvas, fmaxCaptionLength + Gr.left + 2, Gr.Top + 2, FItems.Items[Index].ImageIndex)
        else
          FImages.Draw(Canvas, Gr.left, Gr.Top + 2, FItems.Items[Index].ImageIndex);
      end;
      Gr.Left := Gr.Left + FImages.Width + 2;
    end;

    if not Assigned(FOnDrawItem) then
    begin
{$IFNDEF TMSDOTNET}
      if (FItems.Items[Index].CaptionAlignment = taCenter) and (FItems.Items[Index].Caption <> '') then
        DrawText(Canvas.Handle, PChar(FItems.Items[Index].Caption), -1, Gr, DTSTYLE or DT_CENTER)
      else if (FItems.Items[Index].CaptionAlignment = taRightJustify) and (FItems.Items[Index].Caption <> '') then
        DrawText(Canvas.Handle, PChar(FItems.Items[Index].Caption), -1, Gr, DTSTYLE);
{$ENDIF}
{$IFDEF TMSDOTNET}
      if (FItems.Items[Index].CaptionAlignment = taCenter) and (FItems.Items[Index].Caption <> '') then
        DrawText(Canvas.Handle, FItems.Items[Index].Caption, -1, Gr, DTSTYLE or DT_CENTER)
      else if (FItems.Items[Index].CaptionAlignment = taRightJustify) and (FItems.Items[Index].Caption <> '') then
        DrawText(Canvas.Handle, FItems.Items[Index].Caption, -1, Gr, DTSTYLE);
{$ENDIF}
    end;

    if Assigned(FOnDrawItem) then
      FOnDrawItem(self, Index, R);

  end
  else if (Index = FHotItemIndex) then
  begin
    if self.FColorHotTo <> clNone then
      DrawGradient(Canvas, ColorHot, ColorHotTo, 16, Rect(R.Left, R.Top, R.Right - 1, R.Bottom), true)
    else
    begin
      Canvas.Brush.Color := ColorHot;
      Canvas.Pen.Color := ColorHot;
      DR := FItems.Items[Index].ItemRect;
      Canvas.Rectangle(DR.Left, DR.Top, DR.Right, DR.Bottom);
    end;

    if BorderHotColor <> clNone then
    begin
      Canvas.Brush.Style := bsClear;
      Canvas.Pen.Color := BorderHotColor;
      DR := FItems.Items[Index].ItemRect;
      Canvas.Rectangle(DR.Left, DR.Top, DR.Right, DR.Bottom);
    end;

    Canvas.Brush.Style := bsClear;
    Canvas.Font.Color := clBlack;


    if not Assigned(FOnDrawItem) and (FItems.Items[Index].CaptionAlignment = taLeftJustify) and (FItems.Items[Index].Caption <> '') then
    begin
      //Gr.Left := 2 + Gr.Left + DrawText(Canvas.Handle, PChar(FItems.Items[Index].Caption),-1, Gr, DT_SINGLELINE or DT_VCENTER);
{$IFNDEF TMSDOTNET}
      DrawText(Canvas.Handle, PChar(FItems.Items[Index].Caption), -1, Gr, DTSTYLE);
{$ENDIF}
{$IFDEF TMSDOTNET}
      DrawText(Canvas.Handle, FItems.Items[Index].Caption, -1, Gr, DTSTYLE);
{$ENDIF}
    end;

    if not Assigned(FOnDrawItem) and Assigned(FImages) and (FItems.Items[Index].ImageIndex >= 0) then
    begin
      if TwoColorImages then
      begin
        bmp := TBitmap.Create;
        bmp.Width := FImages.width;
        bmp.Height := FImages.Height;

        bmp.Canvas.Brush.Color := clFuchsia;
        bmp.Canvas.FillRect(rect(0, 0, FImages.Width, FIMages.Height));

        FImages.DrawingStyle := dsTransparent;
        FImages.Draw(bmp.Canvas, 0, 0, FItems.Items[Index].ImageIndex);

        bmp.Transparent := true;
        bmp.TransparentMode := tmAuto;

        if (FItems.Items[Index].CaptionAlignment = taLeftJustify) then
          Canvas.Draw(fmaxCaptionLength + Gr.left + 2, Gr.Top + 2, bmp)
        else
          Canvas.Draw(Gr.left, Gr.Top + 2, bmp);
        bmp.Free;
      end
      else
      begin
        if (FItems.Items[Index].CaptionAlignment = taLeftJustify) then
          FImages.Draw(Canvas, fmaxCaptionLength + Gr.left + 2, Gr.Top + 2, FItems.Items[Index].ImageIndex)
        else
          FImages.Draw(Canvas, Gr.left, Gr.Top + 2, FItems.Items[Index].ImageIndex);
      end;
      Gr.Left := Gr.Left + FImages.Width + 2;
    end;

   { if (FItems.Items[Index].CaptionAlignment in [taRightJustify, taCenter]) and (FItems.Items[Index].Caption <> '') then
    begin
      DrawText(Canvas.Handle, PChar(FItems.Items[Index].Caption),-1, Gr, DT_SINGLELINE or DT_VCENTER);
    end; }
    if not Assigned(FOnDrawItem) then
    begin
{$IFNDEF TMSDOTNET}
      if (FItems.Items[Index].CaptionAlignment = taCenter) and (FItems.Items[Index].Caption <> '') then
        DrawText(Canvas.Handle, PChar(FItems.Items[Index].Caption), -1, Gr, DTSTYLE or DT_CENTER)
      else if (FItems.Items[Index].CaptionAlignment = taRightJustify) and (FItems.Items[Index].Caption <> '') then
        DrawText(Canvas.Handle, PChar(FItems.Items[Index].Caption), -1, Gr, DTSTYLE);
{$ENDIF}
{$IFDEF TMSDOTNET}
      if (FItems.Items[Index].CaptionAlignment = taCenter) and (FItems.Items[Index].Caption <> '') then
        DrawText(Canvas.Handle, FItems.Items[Index].Caption, -1, Gr, DTSTYLE or DT_CENTER)
      else if (FItems.Items[Index].CaptionAlignment = taRightJustify) and (FItems.Items[Index].Caption <> '') then
        DrawText(Canvas.Handle, FItems.Items[Index].Caption, -1, Gr, DTSTYLE);
{$ENDIF}
    end;

    if Assigned(FOnDrawItem) then
      FOnDrawItem(self, Index, R);

  end
  else // Normal
  begin
    if RefreshItem then
    begin
{$IFNDEF TMSDOTNET}
      InvalidateRect(Handle, @R, True);
{$ENDIF}
{$IFDEF TMSDOTNET}
      InvalidateRect(Handle, R, True);
{$ENDIF}
      Exit;
    end;

    if not Assigned(FOnDrawItem) then
    begin
      if FItems.Items[Index].backGroundColor <> clNone then
      begin
        Canvas.Pen.Color := FItems.Items[Index].backGroundColor;
        Canvas.Brush.Color := FItems.Items[Index].backGroundColor;
        Canvas.Rectangle(R.Left + 3, R.Top + 3, R.Right - 3, R.Bottom - 3);
      end;
    end;

    if Index = ItemIndex then // Selected Item
    begin
      if self.FColorSelectedTo <> clNone then
        DrawGradient(Canvas, ColorSelected, ColorSelectedTo, 16, FItems.Items[Index].ItemRect, true)
      else
      begin
        Canvas.Brush.Color := ColorSelected;
        Canvas.Pen.Color := ColorSelected;
	DR := FItems.Items[Index].ItemRect;
        Canvas.Rectangle(DR.Left, DR.Top, DR.Right, DR.Bottom);
      end;

      if BorderSelectedColor <> clNone then
      begin
        Canvas.Brush.Style := bsClear;
        Canvas.Pen.Color := BorderSelectedColor;
	DR := FItems.Items[Index].ItemRect;
        Canvas.Rectangle(DR.Left, DR.Top, DR.Right, DR.Bottom);
      end;
    end;

    Canvas.Brush.Style := bsClear;
    Canvas.Font.Color := clBlack;

    if not Assigned(FOnDrawItem) and (FItems.Items[Index].CaptionAlignment = taLeftJustify) and (FItems.Items[Index].Caption <> '') then
    begin
      //Gr.Left := 2 + Gr.Left + DrawText(Canvas.Handle, PChar(FItems.Items[Index].Caption),-1, Gr, DT_SINGLELINE or DT_VCENTER);
{$IFNDEF TMSDOTNET}
      DrawText(Canvas.Handle, PChar(FItems.Items[Index].Caption), -1, Gr, DTSTYLE);
{$ENDIF}
{$IFDEF TMSDOTNET}
      DrawText(Canvas.Handle, FItems.Items[Index].Caption, -1, Gr, DTSTYLE);
{$ENDIF}
      //Gr.Left:= Gr.Left + Canvas.TextWidth(FItems.Items[Index].Caption);
    end;

    if not Assigned(FOnDrawItem) and Assigned(FImages) and (FItems.Items[Index].ImageIndex >= 0) then
    begin
      if TwoColorImages then
      begin
        bmp := TBitmap.Create;
        bmp.Width := FImages.width;
        bmp.Height := FImages.Height;

        bmp.Canvas.Brush.Color := clFuchsia;
        bmp.Canvas.FillRect(rect(0, 0, FImages.Width, FIMages.Height));

        FImages.DrawingStyle := dsTransparent;
        FImages.Draw(bmp.Canvas, 0, 0, FItems.Items[Index].ImageIndex);

        bmp.Transparent := true;
        bmp.TransparentMode := tmAuto;

        if (FItems.Items[Index].CaptionAlignment = taLeftJustify) then
          Canvas.Draw(fmaxCaptionLength + Gr.left + 2, Gr.Top + 2, bmp)
        else
          Canvas.Draw(Gr.left, Gr.Top + 2, bmp);
        bmp.Free;
      end
      else
      begin
        if (FItems.Items[Index].CaptionAlignment = taLeftJustify) then
          FImages.Draw(Canvas, fmaxCaptionLength + Gr.left + 2, Gr.Top + 2, FItems.Items[Index].ImageIndex)
        else
          FImages.Draw(Canvas, Gr.left, Gr.Top + 2, FItems.Items[Index].ImageIndex);
      end;
      Gr.Left := Gr.Left + FImages.Width + 2;
    end;

    if not Assigned(FOnDrawItem) then
    begin
{$IFNDEF TMSDOTNET}
      if (FItems.Items[Index].CaptionAlignment = taCenter) and (FItems.Items[Index].Caption <> '') then
        DrawText(Canvas.Handle, PChar(FItems.Items[Index].Caption), -1, Gr, DTSTYLE or DT_CENTER)
      else if (FItems.Items[Index].CaptionAlignment = taRightJustify) and (FItems.Items[Index].Caption <> '') then
        DrawText(Canvas.Handle, PChar(FItems.Items[Index].Caption), -1, Gr, DTSTYLE);
{$ENDIF}
{$IFDEF TMSDOTNET}
      if (FItems.Items[Index].CaptionAlignment = taCenter) and (FItems.Items[Index].Caption <> '') then
        DrawText(Canvas.Handle, FItems.Items[Index].Caption, -1, Gr, DTSTYLE or DT_CENTER)
      else if (FItems.Items[Index].CaptionAlignment = taRightJustify) and (FItems.Items[Index].Caption <> '') then
        DrawText(Canvas.Handle, FItems.Items[Index].Caption, -1, Gr, DTSTYLE);
{$ENDIF}
    end;

    if Assigned(FOnDrawItem) then
      FOnDrawItem(self, Index, R);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothSelectorPanel.Paint;
var
  i: integer;
begin
  inherited;

  for i := 0 to FItems.Count - 1 do
  begin
    DrawItem(i);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothSelectorPanel.SetBorderColor(const Value: TColor);
begin
  FBorderColor := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothSelectorPanel.SetImages(const Value: TCustomImageList);
begin
  FImages := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothSelectorPanel.SetItemIndex(const Value: integer);
begin
  if Value < 0 then
  begin
    FItemIndex := -1;
    exit;
  end;

  if Value < FItems.Count then
  begin
    if FItems.Items[Value].Enable then
    begin
      if FItemIndex <> Value then
      begin
        FItemIndex := Value;
        //if Assigned(FOnSelect) then
        //  FOnSelect(self);

      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothSelectorPanel.SetItems(const Value: TAdvSmoothSelectorItems);
begin
  FItems := Value;
end;

//------------------------------------------------------------------------------

function TAdvSmoothSelectorPanel.TotalAutoSizeButtons: integer;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to FItems.Count - 1 do
  begin
    if FItems.Items[i].ItemType = itAutoSizeButton then
      inc(Result);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothSelectorPanel.AutoSizeBtnSize(var W, H: integer);
var
  i: integer;
begin
  w := MinButtonWidth; //MINBUTTONSIZE;
  H := MinButtonHeight; //MINBUTTONSIZE;
  for i := 0 to FItems.Count - 1 do
  begin
    if FItems.Items[i].ItemType = itAutoSizeButton then
    begin
      if Assigned(FImages) and (FItems.Items[i].ImageIndex >= 0) then
      begin
        W := Max(W, Canvas.TextWidth(FItems.Items[i].Caption) + FImages.Width + (ButtonMargin * 2));
        H := Max(H, MAX(Canvas.TextHeight('gh'), FImages.Height) + ButtonMargin); // Single Margin added
      end
      else
      begin
        W := Max(W, Canvas.TextWidth(FItems.Items[i].Caption) + (ButtonMargin * 2));
        H := Max(H, Canvas.TextHeight('gh') + ButtonMargin); // Single Margin added
      end;
      FMaxCaptionLength := Max(FMaxCaptionLength, Canvas.TextWidth(FItems.Items[i].Caption));
    end;
  end;
end;

//------------------------------------------------------------------------------

function TAdvSmoothSelectorPanel.GetMaxWidth: integer;
var
  i, asb, fwb: integer;
begin
  Result := FleftOffSet * 2 + MinButtonWidth; //MINBUTTONSIZE;
  fwb := Result;
  asb := Result;
  AutoSizeBtnSize(asb {GetWidth}, i {dummy});

  for i := 0 to FItems.Count - 1 do
  begin
    if FItems.Items[i].ItemType = itFullWidthButton then
    begin
      if Assigned(FImages) and (FItems.Items[i].ImageIndex >= 0) then
        fwb := Max(fwb, Canvas.TextWidth(FItems.Items[i].Caption) + FImages.Width + (ButtonMargin * 2) + (FleftOffSet * 2))
      else
        fwb := Max(fwb, Canvas.TextWidth(FItems.Items[i].Caption) + (ButtonMargin * 2) + (FleftOffSet * 2));
    end;
  end;

  Result := Max(fwb, asb * min(ButtonsPerRow, TotalAutoSizeButtons) + (FleftOffSet * 2) {(ButtonMargin*2)});
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothSelectorPanel.SetItemsPosition;
var
  i, c, r, absW, absH, MaxW, bNo: integer;
  ShouldChangeRow: boolean;
begin
  r := FTopOffSet;
  c := FLeftOffSet;
  bNo := 0;
  MaxW := GetMaxWidth;
  AutoSizeBtnSize(absW, absH);

  Width := MaxW;

  ShouldChangeRow := false;
  for i := 0 to FItems.Count - 1 do
  begin
    if FItems.Items[i].ItemType = itAutoSizeButton then
    begin
      inc(bNo);
      if bNo > self.ButtonsPerRow then
      begin
        r := r + absH;
        bNo := 1;
        c := FLeftOffSet;
      end;
      FItems.Items[i].ItemRect := Rect(c, r, c + absW, r + absH);
      c := c + absW;
      ShouldChangeRow := true;
    end
    else if FItems.Items[i].ItemType = itFullWidthButton then
    begin
      if ShouldChangeRow then
        r := r + absH + 2; //FButtonHeight;
      FItems.Items[i].ItemRect := Rect(FLeftOffSet, r, MaxW - FLeftOffSet, r + FButtonHeight);
      r := r + FButtonHeight;
      c := FLeftOffSet;
      bNo := 0;
      ShouldChangeRow := false;
    end;
  end;

  SetPanelHeight;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothSelectorPanel.SetPanelHeight;
begin
  if FItems.Count > 0 then
  begin
    Height := FItems.Items[FItems.Count - 1].ItemRect.Bottom + FTopOffSet;
    // SetVisibleHeight;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothSelectorPanel.SetButtonsPerRow(const Value: TNoOfButtons);
begin
  FButtonsPerRow := Value;
end;

function TAdvSmoothSelectorPanel.ItemAtPos(X, Y: integer): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to FItems.Count - 1 do
  begin
    if PtInRect(FItems.Items[i].ItemRect, Point(X, Y)) then
    begin
      Result := i;
      break;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothSelectorPanel.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  i: integer;
begin
  inherited;

  FMouseDown := true;
  i := ItemAtPos(X, Y);
  if (i >= 0) then
  begin
    FDownItemIndex := i;
    DrawItem(i, true);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothSelectorPanel.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  FMouseDown := false;
  if Assigned(FOnShouldHide) then
    FOnShouldHide(self);

  if (FDownItemIndex > -1) and (FHotItemIndex > -1) then
  begin
    ItemIndex := FDownItemIndex;

    if Assigned(FOnSelect) then
      FOnSelect(self);
  end;
  FHotItemIndex := -1;
  FDownItemIndex := -1;
  //if Assigned(FOnShouldHide) then
    //FOnShouldHide(self);
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothSelectorPanel.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;

end;

//------------------------------------------------------------------------------

procedure TAdvSmoothSelectorPanel.WMChar(var Msg: TWMKey);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothSelectorPanel.CMWantSpecialKey(var Msg: TCMWantSpecialKey);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothSelectorPanel.SetButtonMargin(const Value: integer);
begin
  FButtonMargin := Value;
end;

//------------------------------------------------------------------------------


procedure TAdvSmoothSelectorPanel.CMMouseLeave(var Message: TMessage);
var
  i: integer;
begin
  inherited;
  i := FHotItemIndex;
  FHotItemIndex := -1;
  FDownItemIndex := -1;

  if (i > -1) then
    DrawItem(i, true);
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothSelectorPanel.SetMinButtonHeight(const Value: integer);
begin
  FMinButtonHeight := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothSelectorPanel.SetMinButtonWidth(const Value: integer);
begin
  FMinButtonWidth := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothSelectorPanel.SetTwoColorImages(const Value: Boolean);
begin
  FTwoColorImages := Value;
end;

//------------------------------------------------------------------------------

{ TAdvSmoothSelectorItem }

procedure TAdvSmoothSelectorItem.Assign(Source: TPersistent);
begin
  if (Source is TAdvSmoothSelectorItem) then
  begin
    BackGroundColor := (Source as TAdvSmoothSelectorItem).BackGroundColor;
    Caption := (Source as TAdvSmoothSelectorItem).Caption;
    CaptionAlignment := (Source as TAdvSmoothSelectorItem).CaptionAlignment;
    ImageIndex := (Source as TAdvSmoothSelectorItem).ImageIndex;
    Hint := (Source as TAdvSmoothSelectorItem).Hint;
    Value := (Source as TAdvSmoothSelectorItem).Value;
    ItemType := (Source as TAdvSmoothSelectorItem).ItemType;
    Tag := (Source as TAdvSmoothSelectorItem).Tag;
  end;

end;

constructor TAdvSmoothSelectorItem.Create(Collection: TCollection);
begin
  inherited;
  FCaption := '';
  FEnable := true;
  FImageIndex := -1;
  FValue := '';
  FHint := '';
  FCaptionAlignment := taCenter;
  FItemType := itAutoSizeButton;
  FBackGroundColor := clNone;
  FTag := 0;
end;

//------------------------------------------------------------------------------

destructor TAdvSmoothSelectorItem.Destroy;
begin

  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothSelectorItem.SetBackGroundColor(const Value: TColor);
begin
  FBackGroundColor := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothSelectorItem.SetCaption(const Value: string);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothSelectorItem.SetEnable(const Value: boolean);
begin
  if FEnable <> Value then
  begin
    FEnable := Value;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothSelectorItem.SetHint(const Value: string);
begin
  FHint := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothSelectorItem.SetImageIndex(const Value: integer);
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothSelectorItem.SetItemType(const Value: TSelectorItemType);
begin
  if FItemType <> Value then
  begin
    FItemType := Value;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothSelectorItem.SetTag(const Value: integer);
begin
  FTag := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothSelectorItem.SetValue(const Value: string);
begin
  if FValue <> Value then
  begin
    FValue := Value;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothSelectorItem.SetvCaptionAlignment(const Value: TAlignment);
begin
  if FCaptionAlignment <> Value then
  begin
    FCaptionAlignment := Value;
  end;
end;

//------------------------------------------------------------------------------


{ TAdvSmoothSelectorItems }

function TAdvSmoothSelectorItems.Add: TAdvSmoothSelectorItem;
begin
  Result := TAdvSmoothSelectorItem(inherited Add);
end;

//------------------------------------------------------------------------------

constructor TAdvSmoothSelectorItems.Create(AOwner: TPersistent);
begin
  inherited Create(TAdvSmoothSelectorItem);
  FOwner := AOwner;
end;

//------------------------------------------------------------------------------

function TAdvSmoothSelectorItems.GetItem(Index: Integer): TAdvSmoothSelectorItem;
begin
  Result := TAdvSmoothSelectorItem(inherited Items[Index]);
end;

//------------------------------------------------------------------------------

function TAdvSmoothSelectorItems.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

//------------------------------------------------------------------------------

function TAdvSmoothSelectorItems.Insert(Index: Integer): TAdvSmoothSelectorItem;
begin
  Result := TAdvSmoothSelectorItem(inherited Insert(Index));
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothSelectorItems.SetItem(Index: Integer;
  const Value: TAdvSmoothSelectorItem);
begin
  inherited Items[Index] := Value;
end;

//------------------------------------------------------------------------------


{ TAdvSmoothColorCubePanel }

procedure TAdvSmoothColorCubePanel.CMMouseLeave(var Message: TMessage);
begin
  inherited;
end;

//------------------------------------------------------------------------------

constructor TAdvSmoothColorCubePanel.Create(AOwner: TComponent);
begin
  inherited;
  FSelectedColor := clNone;
  FHotIndex := -1;
  FSelectedIndex := -1;
  Initialize;
  ShowRGBHint := true;
end;

//------------------------------------------------------------------------------

destructor TAdvSmoothColorCubePanel.Destroy;
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothColorCubePanel.DrawColorCube(Index: integer);
begin
  if (Index >= 1) and (Index <= High(FCubeCells)) then
  begin
    Canvas.Pen.Color := FCubeCells[Index].Color;
    Canvas.Brush.Color := FCubeCells[Index].Color;
    DrawHexagon(Canvas, FCubeCells[Index].CenterPos, FCubeSize.X, FCubeSize.Y);
    if FHotIndex = Index then
      DrawHotBorder;
    if FSelectedIndex = Index then
      DrawSelectedBorder;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothColorCubePanel.DrawAllColorCube;
var
  i: integer;
begin
  for i := 1 to high(FCubeCells) do
  begin
    DrawColorCube(i);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothColorCubePanel.DrawHexagon(aCanvas: TCanvas; P: TPoint; X, Y: integer);
begin
  P.X := P.X - X;
  P.Y := P.Y - y * 2;
  aCanvas.Polygon([Point(P.X, P.Y + Y), Point(P.X + X, P.Y),
    Point(P.X + X * 2, P.Y + Y), Point(P.X + X * 2, P.Y + Y * 3), Point(P.X + X, P.Y + Y * 4), Point(P.X, P.Y + Y * 3)]);
end;

//------------------------------------------------------------------------------

function TAdvSmoothColorCubePanel.IndexOfCellAt(X, Y: integer): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 1 to high(FCubeCells) do
  begin
    if PtInCell(i, Point(X, Y)) then
    begin
      Result := i;
      break;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothColorCubePanel.Initialize;
var
  c, r, i, j, cl: integer;
begin
  FCubeSize.X := 6;
  FCubeSize.Y := 3;

  FCubeCells[1].Color := $00663300; FCubeCells[2].Color := $00996633;
  FCubeCells[3].Color := $00CC6633; FCubeCells[4].Color := $00993300;
  FCubeCells[5].Color := $00990000; FCubeCells[6].Color := $00CC0000;
  FCubeCells[7].Color := $00660000; FCubeCells[8].Color := $00666600;
  FCubeCells[9].Color := $00996600; FCubeCells[10].Color := $00CC9900;
  FCubeCells[11].Color := $00CC6600; FCubeCells[12].Color := $00CC3300;
  FCubeCells[13].Color := clBlue; FCubeCells[14].Color := $00FF3333;
  FCubeCells[15].Color := $00993333; FCubeCells[16].Color := clTeal;
  FCubeCells[17].Color := $00999900; FCubeCells[18].Color := $00CCCC33;
  FCubeCells[19].Color := $00FFCC00; FCubeCells[20].Color := $00FF9900;
  FCubeCells[21].Color := $00FF6600; FCubeCells[22].Color := $00FF6633;
  FCubeCells[23].Color := $00CC3333; FCubeCells[24].Color := $00996666;
  FCubeCells[25].Color := $00669933; FCubeCells[26].Color := $0099CC00;
  FCubeCells[27].Color := $00CCFF00; FCubeCells[28].Color := clAqua;
  FCubeCells[29].Color := $00FFCC33; FCubeCells[30].Color := $00FF9933;
  FCubeCells[31].Color := $00FF9966; FCubeCells[32].Color := $00FF6666;
  FCubeCells[33].Color := $00FF0066; FCubeCells[34].Color := $00CC0066;
  FCubeCells[35].Color := $00339933; FCubeCells[36].Color := $0066CC00;
  FCubeCells[37].Color := $0099FF00; FCubeCells[38].Color := $00CCFF66;
  FCubeCells[39].Color := $00FFFF66; FCubeCells[40].Color := $00FFCC66;
  FCubeCells[41].Color := $00FFCC99; FCubeCells[42].Color := $00FF9999;
  FCubeCells[43].Color := $00FF6699; FCubeCells[44].Color := $00FF3399;
  FCubeCells[45].Color := $00FF0099; FCubeCells[46].Color := $00006600;
  FCubeCells[47].Color := $0000CC00; FCubeCells[48].Color := clLime;
  FCubeCells[49].Color := $0099FF66; FCubeCells[50].Color := $00CCFF99;
  FCubeCells[51].Color := $00FFFFCC; FCubeCells[52].Color := $00FFECCC;
  FCubeCells[53].Color := $00FFCCCC; FCubeCells[54].Color := $00FF99CC;
  FCubeCells[55].Color := $00FF66CC; FCubeCells[56].Color := $00FF00CC;
  FCubeCells[57].Color := $00CC0099; FCubeCells[58].Color := $00003300;
  FCubeCells[59].Color := clGreen; FCubeCells[60].Color := $0033CC33;
  FCubeCells[61].Color := $0066FF66; FCubeCells[62].Color := $0099FF99;
  FCubeCells[63].Color := $00CCFFCC; FCubeCells[64].Color := clWhite;
  FCubeCells[65].Color := $00FFCCFF; FCubeCells[66].Color := $00FF99FF;
  FCubeCells[67].Color := $00FF66FF; FCubeCells[68].Color := clFuchsia;
  FCubeCells[69].Color := $00CC00CC; FCubeCells[70].Color := $00660066;
  FCubeCells[71].Color := $00006633; FCubeCells[72].Color := $00009900;
  FCubeCells[73].Color := $0033FF66; FCubeCells[74].Color := $0066FF99;
  FCubeCells[75].Color := $0099FFCC; FCubeCells[76].Color := $00CCFFFF;
  FCubeCells[77].Color := $00CCCCFF; FCubeCells[78].Color := $00CC99FF;
  FCubeCells[79].Color := $00CC66FF; FCubeCells[80].Color := $00CC33FF;
  FCubeCells[81].Color := $009900CC; FCubeCells[82].Color := clPurple;
  FCubeCells[83].Color := $00003333; FCubeCells[84].Color := $00009966;
  FCubeCells[85].Color := $0033FF99; FCubeCells[86].Color := $0066FFCC;
  FCubeCells[87].Color := $0099FFFF; FCubeCells[88].Color := $0099CCFF;
  FCubeCells[89].Color := $009999FF; FCubeCells[90].Color := $009966FF;
  FCubeCells[91].Color := $009933FF; FCubeCells[92].Color := $009933CC;
  FCubeCells[93].Color := $00990099; FCubeCells[94].Color := $00336666;
  FCubeCells[95].Color := $0000CC99; FCubeCells[96].Color := $0033FFCC;
  FCubeCells[97].Color := $0066FFFF; FCubeCells[98].Color := $0066CCFF;
  FCubeCells[99].Color := $006699FF; FCubeCells[100].Color := $00807CFF;
  FCubeCells[101].Color := $006600FF; FCubeCells[102].Color := $009300D6;
  FCubeCells[103].Color := $00663399; FCubeCells[104].Color := clOlive;
  FCubeCells[105].Color := $0000CCCC; FCubeCells[106].Color := clYellow;
  FCubeCells[107].Color := $0000CCFF; FCubeCells[108].Color := $003399FF;
  FCubeCells[109].Color := $000066FF; FCubeCells[110].Color := $005050FF;
  FCubeCells[111].Color := $006600CC; FCubeCells[112].Color := $00330066;
  FCubeCells[113].Color := $00336699; FCubeCells[114].Color := $000099CC;
  FCubeCells[115].Color := $000099FF; FCubeCells[116].Color := $000066CC;
  FCubeCells[117].Color := $000033FF; FCubeCells[118].Color := clRed;
  FCubeCells[119].Color := $000000CC; FCubeCells[120].Color := $00330099;
  FCubeCells[121].Color := $00003366; FCubeCells[122].Color := $00006699;
  FCubeCells[123].Color := $000033CC; FCubeCells[124].Color := $00003399;
  FCubeCells[125].Color := $00000099; FCubeCells[126].Color := clMaroon;
  FCubeCells[127].Color := $002100A5;

  c := 55; // Left
  r := 20; // Top
  cl := 1;

  // Set Cells Position
  for i := 1 to 13 do
  begin
    if i < 8 then
    begin
      for j := 1 to 6 + i do
      begin
        //DrawHexagon(Canvas, Point(C,R), 6, 3);
        FCubeCells[cl].CenterPos := Point(C, R);
        c := c + FCubeSize.x * 2; //12;
        inc(cl);
      end;
      r := r + 10;
      c := 55 - (FCubeSize.x * i);
    end
    else
    begin
      c := 19 + (FCubeSize.x * (i - 7));
      for j := 1 to 13 - (i - 7) do
      begin
        //DrawHexagon(Canvas, Point(C,R), 6, 3);
        FCubeCells[cl].CenterPos := Point(C, R);
        c := c + FCubeSize.x * 2; //12;
        inc(cl);
      end;
      r := r + 10;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothColorCubePanel.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  i, j: integer;
begin
  inherited;
  i := IndexOfCellAt(X, Y);
  if i >= 1 then
  begin
    //SelectedColor:= FCubeCells[i].Color;
    if FSelectedIndex >= 1 then
    begin
      j := FSelectedIndex;
      FSelectedIndex := -1;
      DrawColorCube(j);
    end;

    SetSelectedIndexAndColor(FCubeCells[i].Color, i);

    DrawColorCube(i);

    if Assigned(FOnSelect) then
      FOnSelect(self);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothColorCubePanel.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  i, j: integer;
begin
  inherited;
  i := IndexOfCellAt(X, Y);
  if (i >= 1) then
  begin
    if (FHotIndex <> i) then
    begin
      if FHotIndex >= 1 then
      begin
        j := FHotIndex;
        FHotIndex := -1;
        DrawColorCube(j);
      end;
      FHotIndex := i;
      DrawColorCube(FHotIndex);
      if ShowRGBHint then
      begin
        Hint := Format('Red: %d,  Green: %d,  Blue: %d', [GetRValue(FCubeCells[i].Color), GetGValue(FCubeCells[i].Color), GetBValue(FCubeCells[i].Color)]);
        Application.CancelHint;
      end;
    end;
  end
  else
  begin
    if ShowRGBHint then
    begin
      Hint := '';
      Application.CancelHint;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothColorCubePanel.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  if Assigned(FOnShouldHide) then
    FOnShouldHide(self);
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothColorCubePanel.Paint;
begin
  inherited;
  DrawAllColorCube;
end;

//------------------------------------------------------------------------------

function TAdvSmoothColorCubePanel.PtInCell(Index: integer; P: TPoint): Boolean;
begin
  Result := sqr(FCubeCells[Index].CenterPos.X - P.X) + sqr(FCubeCells[Index].CenterPos.Y - P.Y) <= (FCubeSize.X * FCubeSize.X);
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothColorCubePanel.SetItemsPosition;
begin
  FHotIndex := -1;
  SetPanelSize;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothColorCubePanel.SetPanelSize;
begin
  Height := 160;
  Width := 182;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothColorCubePanel.SetSelectedColor(const Value: TColor);
begin
  //FSelectedColor := Value;
  SetSelectedIndexAndColor(Value);
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothColorCubePanel.DrawSelectedBorder;
begin
  if (FSelectedIndex >= 1) and (FSelectedIndex <= High(FCubeCells)) then
  begin
    Canvas.Brush.style := bsClear;
    Canvas.Pen.Color := clBlack;
    DrawHexagon(Canvas, FCubeCells[FSelectedIndex].centerPos, FCubeSize.X - 1, FCubeSize.Y);
    Canvas.Pen.Color := clWhite;
    DrawHexagon(Canvas, FCubeCells[FSelectedIndex].centerPos, FCubeSize.X - 2, FCubeSize.Y - 1);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothColorCubePanel.DrawHotBorder;
begin
  if (FHotIndex >= 1) and (FHotIndex <= High(FCubeCells)) then
  begin
    Canvas.Brush.style := bsClear;
    Canvas.Pen.Color := clRed;
    DrawHexagon(Canvas, FCubeCells[FHotIndex].centerPos, FCubeSize.X - 1, FCubeSize.Y);
    Canvas.Pen.Color := clWhite;
    DrawHexagon(Canvas, FCubeCells[FHotIndex].centerPos, FCubeSize.X - 2, FCubeSize.Y - 1);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothColorCubePanel.SetSelectedIndexAndColor(clr: TColor;
  index: integer);
var
  i: integer;
  //OldSelClr: TColor;
begin
  //OldSelClr := FSelectedColor;
  FSelectedColor := clr;
  if (Index >= 1) and (Index <= High(FCubeCells)) then
  begin
    FSelectedIndex := Index;
  end
  else
  begin
    FSelectedIndex := -1;
    for i := 1 to high(FCubeCells) do
      if FCubeCells[i].Color = FSelectedColor then
      begin
        FSelectedIndex := i;
        break;
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothColorCubePanel.SetShowRGBHint(const Value: Boolean);
begin
  FShowRGBHint := Value;
  ShowHint := Value;
end;

//------------------------------------------------------------------------------


{ TAdvSmoothColorSpectrumPanel }

procedure TAdvSmoothColorSpectrumPanel.CMMouseLeave(var Message: TMessage);
begin
  inherited;
end;

//------------------------------------------------------------------------------

constructor TAdvSmoothColorSpectrumPanel.Create(AOwner: TComponent);
var
  //frm: TSpectrumForm;
  tbmp: TBitmap;
begin
  inherited;
  FSpectrumImage := TImage.Create(self);
  FSpectrumImage.Parent := self;
  FSpectrumImage.Height := 128;
  FSpectrumImage.Width := 128;

  {
  frm:= TSpectrumForm.Create(nil);
  FSpectrumImage.Picture.Assign(frm.SpectrumImage.Picture);
  //FSpectrumImage.Picture.LoadFromFile('Spectrum.bmp');
  frm.Free;
  }
  tbmp := TBitmap.Create;
  tbmp.LoadFromResourceName(HInstance, 'AC_SPECTRUM');
  FSpectrumImage.Picture.Assign(tbmp);
  tbmp.Free;

  FSpectrumImage.OnMouseMove := SpectrumImageMouseMove;
  FSpectrumImage.OnMouseDown := SpectrumImageMouseDown;
  FSpectrumImage.OnMouseUp := SpectrumImageMouseUp;
  FHotColor := clNone;
  FSelectedColor := clWhite;

  Screen.Cursors[crTMSCur1] := LoadCursor(HInstance, 'TMS_AC_CUR1');
  FSpectrumImage.Cursor := crTMSCur1;
end;

//------------------------------------------------------------------------------

destructor TAdvSmoothColorSpectrumPanel.Destroy;
begin
  FSpectrumImage.Free;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothColorSpectrumPanel.DrawFocusPoint;
begin
  {
  FSpectrumImage.Canvas.Pen.Color:= clWhite;
  FSpectrumImage.Canvas.MoveTo(50, 50);
  FSpectrumImage.Canvas.LineTo(100, 100);
  }
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothColorSpectrumPanel.DrawHotRect;
begin
  Canvas.Pen.Color := clGray;
  Canvas.Brush.Style := bsClear;
  Canvas.Rectangle(FHotRect.Left - 1, FHotRect.Top - 1, FHotRect.Right + 1, FHotRect.Bottom + 1);

  Canvas.Pen.Color := clWhite;
  Canvas.Brush.Color := FHotColor;
  Canvas.Rectangle(FHotRect.Left, FHotRect.Top, FHotRect.Right, FHotRect.Bottom);
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothColorSpectrumPanel.DrawSelectedRect;
begin
  Canvas.Pen.Color := clGray;
  Canvas.Brush.Style := bsClear;
  Canvas.Rectangle(FSelectedRect.Left - 1, FSelectedRect.Top - 1, FSelectedRect.Right + 1, FSelectedRect.Bottom + 1);

  Canvas.Pen.Color := clWhite;
  Canvas.Brush.Color := FSelectedColor;
  Canvas.Rectangle(FSelectedRect.Left, FSelectedRect.Top, FSelectedRect.Right, FSelectedRect.Bottom);
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothColorSpectrumPanel.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothColorSpectrumPanel.MouseMove(Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothColorSpectrumPanel.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if Assigned(FOnShouldHide) then
    FOnShouldHide(self);
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothColorSpectrumPanel.Paint;
begin
  inherited;
  Canvas.Brush.Style := bsClear;
  Canvas.Pen.Color := clwhite;
  Canvas.Rectangle(FSpectrumImage.Left - 1, FSpectrumImage.Top - 1, FSpectrumImage.Left + FSpectrumImage.Width + 1, FSpectrumImage.Top + FSpectrumImage.Height + 1);
  Canvas.Pen.Color := clGray;
  Canvas.Rectangle(FSpectrumImage.Left - 2, FSpectrumImage.Top - 2, FSpectrumImage.Left + FSpectrumImage.Width + 2, FSpectrumImage.Top + FSpectrumImage.Height + 2);

  DrawSelectedRect;
  DrawHotRect;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothColorSpectrumPanel.SetItemsPosition;
begin
  SetPanelSize;
  FSpectrumImage.Left := (Width - FSpectrumImage.Width) div 2;
  FSpectrumImage.Top := 10;
  FSelectedRect := Rect(FSpectrumImage.Left - 1, FSpectrumImage.Top + FSpectrumImage.Height + 8, FSpectrumImage.Left + (FSpectrumImage.Width div 2) - 2, FSpectrumImage.Top + FSpectrumImage.Height + 8 + 20);
  FHotRect := Rect(FSelectedRect.Right + 4, FSelectedRect.Top, FSpectrumImage.Left + FSpectrumImage.Width + 1, FSelectedRect.Bottom);
  FHotColor := clNone;

  DrawFocusPoint;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothColorSpectrumPanel.SetPanelSize;
begin
  Height := 174;
  Width := 150
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothColorSpectrumPanel.SetSelectedColor(const Value: TColor);
begin
  if Value <> FSelectedColor then
  begin
    FSelectedColor := Value;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothColorSpectrumPanel.SpectrumImageMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  SelectedColor := FHotColor;
  DrawSelectedRect;
  if Assigned(OnSelect) then
    FOnSelect(self);
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothColorSpectrumPanel.SpectrumImageMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  FHotColor := Canvas.Pixels[X + FSpectrumImage.Left, Y + FSpectrumImage.Top];
  DrawHotRect;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothColorSpectrumPanel.SpectrumImageMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin

  if Assigned(FOnShouldHide) then
    FOnShouldHide(self);
end;

//------------------------------------------------------------------------------


{ TAdvPenStyleSelector }

constructor TAdvSmoothPenStyleSelector.Create(AOwner: TComponent);
begin
  inherited;
  //if not (csDesigning in ComponentState) then
  begin
    FImages := TCustomImageList.Create(self);
    FImages.Width := 99;
    FImages.Height := 9;
    FImages.ResInstLoad(HInstance, rtBitmap, 'AC_PENSTYLE0', clWhite);
    FImages.ResInstLoad(HInstance, rtBitmap, 'AC_PENSTYLE1', clWhite);
    FImages.ResInstLoad(HInstance, rtBitmap, 'AC_PENSTYLE2', clWhite);
    FImages.ResInstLoad(HInstance, rtBitmap, 'AC_PENSTYLE3', clWhite);
    FImages.ResInstLoad(HInstance, rtBitmap, 'AC_PENSTYLE4', clWhite);
    FImages.ResInstLoad(HInstance, rtBitmap, 'AC_PENSTYLE5', clWhite);
    FImages.ResInstLoad(HInstance, rtBitmap, 'AC_PENSTYLE6', clWhite);
    FImages.ResInstLoad(HInstance, rtBitmap, 'AC_PENSTYLE7', clWhite);
    //b:= FImages.GetInstRes(ac, rtBitmap	, S, 99, lrTransparent, clWhite);
  end;

  FSelectionType := stOffice;
  FTwoColorImages := True;
  Initialize;
end;

//------------------------------------------------------------------------------

destructor TAdvSmoothPenStyleSelector.Destroy;
begin
  //if not (csDesigning in ComponentState) and Assigned(FImages) then
  begin
    FImages.Free;
  end;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothPenStyleSelector.Initialize;
begin
  Tools.Clear;

  if (SelectionType = stOffice) then
  begin
    with Tools.Add do
    begin
      ItemType := itAutoSizeButton;
      Hint := 'Solid';
      ImageIndex := 0;
    end;
    with Tools.Add do
    begin
      ItemType := itAutoSizeButton;
      ImageIndex := 1;
      Hint := 'Round Dot';
    end;
    with Tools.Add do
    begin
      ItemType := itAutoSizeButton;
      ImageIndex := 2;
      Hint := 'Square Dots';
    end;
    with Tools.Add do
    begin
      ItemType := itAutoSizeButton;
      ImageIndex := 3;
      Hint := 'Dash';
    end;
    with Tools.Add do
    begin
      ItemType := itAutoSizeButton;
      ImageIndex := 4;
      Hint := 'Dash Dots';
    end;
    with Tools.Add do
    begin
      ItemType := itAutoSizeButton;
      ImageIndex := 5;
      Hint := 'Long Dash';
    end;
    with Tools.Add do
    begin
      ItemType := itAutoSizeButton;
      ImageIndex := 6;
      Hint := 'Long Dash Dot';
    end;
    with Tools.Add do
    begin
      ItemType := itAutoSizeButton;
      ImageIndex := 7;
      Hint := 'Long Dash Dot Dot';
    end;

    with Tools.Add do
    begin
      ItemType := itFullWidthButton;
      CaptionAlignment := taCenter;
      Caption := 'More Styles';
      Hint := 'More Styles';
    end;
  end
  else  // stBorland
  begin
    with Tools.Add do
    begin
      ItemType := itAutoSizeButton;
      Hint := 'Solid';
      ImageIndex := 0;
    end;
    with Tools.Add do
    begin
      ItemType := itAutoSizeButton;
      ImageIndex := 3;
      Hint := 'Dash';
    end;
    with Tools.Add do
    begin
      ItemType := itAutoSizeButton;
      ImageIndex := 1;
      Hint := 'Dot';
    end;
    with Tools.Add do
    begin
      ItemType := itAutoSizeButton;
      ImageIndex := 4;
      Hint := 'DashDot';
    end;
    with Tools.Add do
    begin
      ItemType := itAutoSizeButton;
      ImageIndex := 7;
      Hint := 'DashDotDot';
    end;
    with Tools.Add do
    begin
      ItemType := itAutoSizeButton;
      ImageIndex := -1;
      Hint := 'Clear';
    end;
    with Tools.Add do
    begin
      ItemType := itAutoSizeButton;
      ImageIndex := 0;
      Hint := 'InsideFrame';
    end;
   {with Tools.Add do
    begin
      ItemType := itFullWidthButton;
      CaptionAlignment := taCenter;
      Caption := 'More Styles';
      Hint := 'More Styles';
    end;
   }
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothPenStyleSelector.Loaded;
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothPenStyleSelector.SetSelectorPanel;
begin
  //self.TwoColorImages := True;
  inherited;
  FSelectorPanel.Images := Fimages;
  FSelectorPanel.SetItemsPosition;

  //FSelectorPanel.TwoColorImages := True;
end;

//------------------------------------------------------------------------------

function TAdvSmoothPenStyleSelector.GetSelectedPenStyle: TPenStyle;
begin
  Result := GetPenStyleAtIndex(SelectedIndex);
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothPenStyleSelector.SetSelectedPenStyle(const Value: TPenStyle);
begin
  SelectedIndex := GetIndexOfStyle(Value);
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothPenStyleSelector.SetSelectionType(
  const Value: TSelectionType);
begin
  if FSelectionType <> Value then
  begin
    FSelectionType := Value;
    Initialize;
    if SelectedIndex >= Tools.Count then
      SelectedIndex := Tools.Count-1;
  end;
end;

//------------------------------------------------------------------------------

function TAdvSmoothPenStyleSelector.GetPenColor: TColor;
begin
  Result := ForeGroundImageColor;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothPenStyleSelector.SetPenColor(const Value: TColor);
begin
  ForeGroundImageColor := Value;
  Invalidate;
end;

//------------------------------------------------------------------------------

function TAdvSmoothPenStyleSelector.GetPenStyleAtIndex(
  Index: Integer): TPenStyle;
begin
  Result := psClear;
  if SelectionType = stOffice then
  begin
    case Index of
      0: Result := psSolid;
      1: Result := psDot;
      2: Result := psDot;
      3: Result := psDash;
      4: Result := psDashDot;
      5: Result := psDash;
      6: Result := psDashDot;
      7: Result := psDashDotDot;
      8: Result := psClear;
    end;
  end
  else  // stBorland
  begin
    case Index of
      0: Result := psSolid;
      1: Result := psDash;
      2: Result := psDot;
      3: Result := psDashDot;
      4: Result := psDashDotDot;
      5: Result := psClear;
      6: Result := psInsideFrame;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TAdvSmoothPenStyleSelector.GetIndexOfStyle(
  APenStyle: TPenStyle): Integer;
begin
  Result := -1;
  if SelectionType = stOffice then
  begin
    case APenStyle of
      psSolid     : Result := 0;
      psDot       : Result := 1;
      //psDot       : Result := 2;
      psDash      : Result := 3;
      psDashDot   : Result := 4;
      //psDash      : Result := 5;
      //psDashDot   : Result := 6;
      psDashDotDot: Result := 7;
      psClear     : Result := 8;
    end;
  end
  else  // stBorland
  begin
    case APenStyle of
      psSolid      : Result := 0;
      psDash       : Result := 1;
      psDot        : Result := 2;
      psDashDot    : Result := 3;
      psDashDotDot : Result := 4;
      psClear      : Result := 5;
      psInsideFrame: Result := 6;
    end;
  end;

end;

//------------------------------------------------------------------------------

{ TAdvSmoothBrushStyleSelector }

constructor TAdvSmoothBrushStyleSelector.Create(AOwner: TComponent);
begin
  inherited;
  //if not (csDesigning in ComponentState) then
  begin
    FImages := TCustomImageList.Create(self);
    FImages.Width := 102;
    FImages.Height := 14;
    FImages.ResInstLoad(HInstance, rtBitmap, 'AC_BRUSHSTYLE0', clWhite);
    FImages.ResInstLoad(HInstance, rtBitmap, 'AC_BRUSHSTYLE1', clWhite);
    FImages.ResInstLoad(HInstance, rtBitmap, 'AC_BRUSHSTYLE2', clWhite);
    FImages.ResInstLoad(HInstance, rtBitmap, 'AC_BRUSHSTYLE3', clWhite);
    FImages.ResInstLoad(HInstance, rtBitmap, 'AC_BRUSHSTYLE4', clWhite);
    FImages.ResInstLoad(HInstance, rtBitmap, 'AC_BRUSHSTYLE5', clWhite);
    FImages.ResInstLoad(HInstance, rtBitmap, 'AC_BRUSHSTYLE6', clWhite);
    FImages.ResInstLoad(HInstance, rtBitmap, 'AC_BRUSHSTYLE7', clWhite);

    // stBorland
    FImages.ResInstLoad(HInstance, rtBitmap, 'AC_BRUSHSTYLE8', clWhite);
    FImages.ResInstLoad(HInstance, rtBitmap, 'AC_BRUSHSTYLE9', clWhite);
    FImages.ResInstLoad(HInstance, rtBitmap, 'AC_BRUSHSTYLE10', clWhite);
    FImages.ResInstLoad(HInstance, rtBitmap, 'AC_BRUSHSTYLE11', clWhite);
    FImages.ResInstLoad(HInstance, rtBitmap, 'AC_BRUSHSTYLE12', clWhite);
    FImages.ResInstLoad(HInstance, rtBitmap, 'AC_BRUSHSTYLE13', clWhite);
    FImages.ResInstLoad(HInstance, rtBitmap, 'AC_BRUSHSTYLE14', clWhite);
  end;

  FSelectionType := stOffice;
  FTwoColorImages := True;
  FStretchImageDraw := False;

  Initialize;
end;

//------------------------------------------------------------------------------

destructor TAdvSmoothBrushStyleSelector.Destroy;
begin
  //if not (csDesigning in ComponentState) and Assigned(FImages) then
  begin
    FImages.Free;
  end;
  inherited;
end;

//------------------------------------------------------------------------------

function TAdvSmoothBrushStyleSelector.GetBrushColor: TColor;
begin
  Result := ForeGroundImageColor;
end;

//------------------------------------------------------------------------------

function TAdvSmoothBrushStyleSelector.GetBrushStyleAtIndex(
  Index: Integer): TBrushStyle;
begin
  Result := bsClear;
  if SelectionType = stOffice then
  begin
    case Index of
      0: Result := bsFDiagonal;
      1: Result := bsBDiagonal;
      2: Result := bsFDiagonal;
      3: Result := bsHorizontal;
      4: Result := bsVertical;
      5: Result := bsCross;
      6: Result := bsSolid;
      7: Result := bsDiagCross;
      8: Result := bsClear;
    end;
  end
  else  // stBorland
  begin
    case Index of
      0: Result := bsSolid;
      1: Result := bsClear;
      2: Result := bsHorizontal;
      3: Result := bsVertical;
      4: Result := bsFDiagonal;
      5: Result := bsBDiagonal;
      6: Result := bsCross;
      7: Result := bsDiagCross;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TAdvSmoothBrushStyleSelector.GetIndexOfBrushStyle(
  ABrushStyle: TBrushStyle): Integer;
begin
  Result := -1;
  if SelectionType = stOffice then
  begin
    case ABrushStyle of
      bsFDiagonal : Result := 0;
      bsBDiagonal : Result := 1;
      //bsFDiagonal : Result := 2;
      bsHorizontal: Result := 3;
      bsVertical  : Result := 4;
      bsCross     : Result := 5;
      bsSolid     : Result := 6;
      bsDiagCross : Result := 7;
      bsClear     : Result := 8;
    end;
  end
  else  // stBorland
  begin
    case ABrushStyle of
      bsSolid     : Result := 0;
      bsClear     : Result := 1;
      bsHorizontal: Result := 2;
      bsVertical  : Result := 3;
      bsFDiagonal : Result := 4;
      bsBDiagonal : Result := 5;
      bsCross     : Result := 6;
      bsDiagCross : Result := 7;
    end;
  end;

end;

//------------------------------------------------------------------------------

function TAdvSmoothBrushStyleSelector.GetSelectedBrushStyle: TBrushStyle;
begin
  Result := GetBrushStyleAtIndex(SelectedIndex);
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothBrushStyleSelector.Initialize;
begin
  Tools.Clear;

  if (SelectionType = stOffice) then
  begin
    with Tools.Add do
    begin
      ItemType := itAutoSizeButton;
      ImageIndex := 0;
    end;
    with Tools.Add do
    begin
      ItemType := itAutoSizeButton;
      ImageIndex := 1;
    end;
    with Tools.Add do
    begin
      ItemType := itAutoSizeButton;
      ImageIndex := 2;
    end;
    with Tools.Add do
    begin
      ItemType := itAutoSizeButton;
      ImageIndex := 3;
    end;
    with Tools.Add do
    begin
      ItemType := itAutoSizeButton;
      ImageIndex := 4;
    end;
    with Tools.Add do
    begin
      ItemType := itAutoSizeButton;
      ImageIndex := 5;
    end;
    with Tools.Add do
    begin
      ItemType := itAutoSizeButton;
      ImageIndex := 6;
    end;
    with Tools.Add do
    begin
      ItemType := itAutoSizeButton;
      ImageIndex := 7;
    end;
    with Tools.Add do
    begin
      ItemType := itFullWidthButton;
      CaptionAlignment := taCenter;
      Caption := ' More Styles';
      Hint := 'More Styles';
    end;
  end
  else  // stBorland
  begin
    with Tools.Add do
    begin
      ItemType := itAutoSizeButton;
      Hint := 'Solid';
      ImageIndex := 8;
    end;
    with Tools.Add do
    begin
      ItemType := itAutoSizeButton;
      Hint := 'Clear';
      ImageIndex := -1;
    end;
    with Tools.Add do
    begin
      ItemType := itAutoSizeButton;
      Hint := 'Horizontal';
      ImageIndex := 9;
    end;
    with Tools.Add do
    begin
      ItemType := itAutoSizeButton;
      Hint := 'Vertical';
      ImageIndex := 10;
    end;
    with Tools.Add do
    begin
      ItemType := itAutoSizeButton;
      Hint := 'FDiagonal';
      ImageIndex := 11;
    end;
    with Tools.Add do
    begin
      ItemType := itAutoSizeButton;
      Hint := 'BDiagonal';
      ImageIndex := 12;
    end;
    with Tools.Add do
    begin
      ItemType := itAutoSizeButton;
      Hint := 'Cross';
      ImageIndex := 13;
    end;
    with Tools.Add do
    begin
      ItemType := itAutoSizeButton;
      Hint := 'DiagCross';
      ImageIndex := 14;
    end;
   {with Tools.Add do
    begin
      ItemType := itFullWidthButton;
      CaptionAlignment := taCenter;
      Caption := ' More Styles';
      Hint := 'More Styles';
    end;
   }
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothBrushStyleSelector.Loaded;
begin
  inherited;

end;

//------------------------------------------------------------------------------

procedure TAdvSmoothBrushStyleSelector.SetBrushColor(const Value: TColor);
begin
  ForeGroundImageColor := Value;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothBrushStyleSelector.SetSelectedBrushStyle(
  const Value: TBrushStyle);
begin
  SelectedIndex := GetIndexOfBrushStyle(Value);
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothBrushStyleSelector.SetSelectionType(
  const Value: TSelectionType);
begin
  if FSelectionType <> Value then
  begin
    FSelectionType := Value;
    Initialize;
    if SelectedIndex >= Tools.Count then
      SelectedIndex := Tools.Count-1;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothBrushStyleSelector.SetSelectorPanel;
begin
  //self.TwoColorImages := True;
  inherited;
  FSelectorPanel.Images := Fimages;
  FSelectorPanel.SetItemsPosition;

  //FSelectorPanel.TwoColorImages := True;
end;

//------------------------------------------------------------------------------

{ TAdvSmoothShadowSelector }

constructor TAdvSmoothShadowSelector.Create(AOwner: TComponent);
begin
  inherited;
  //if not (csDesigning in ComponentState) then
  begin
    FImages := TCustomImageList.Create(self);
    FImages.Width := 20;
    FImages.Height := 20;
    FImages.ResInstLoad(HInstance, rtBitmap, 'AC_SHADOW0', clWhite);
    FImages.ResInstLoad(HInstance, rtBitmap, 'AC_SHADOW1', clWhite);
    FImages.ResInstLoad(HInstance, rtBitmap, 'AC_SHADOW2', clWhite);
    FImages.ResInstLoad(HInstance, rtBitmap, 'AC_SHADOW3', clWhite);
    FImages.ResInstLoad(HInstance, rtBitmap, 'AC_SHADOW4', clWhite);
    FImages.ResInstLoad(HInstance, rtBitmap, 'AC_SHADOW5', clWhite);
    FImages.ResInstLoad(HInstance, rtBitmap, 'AC_SHADOW6', clWhite);
    FImages.ResInstLoad(HInstance, rtBitmap, 'AC_SHADOW7', clWhite);
    FImages.ResInstLoad(HInstance, rtBitmap, 'AC_SHADOW8', clWhite);
    FImages.ResInstLoad(HInstance, rtBitmap, 'AC_SHADOW9', clWhite);
    FImages.ResInstLoad(HInstance, rtBitmap, 'AC_SHADOW10', clWhite);
    FImages.ResInstLoad(HInstance, rtBitmap, 'AC_SHADOW11', clWhite);
    FImages.ResInstLoad(HInstance, rtBitmap, 'AC_SHADOW12', clWhite);
    FImages.ResInstLoad(HInstance, rtBitmap, 'AC_SHADOW13', clWhite);
    FImages.ResInstLoad(HInstance, rtBitmap, 'AC_SHADOW14', clWhite);
    FImages.ResInstLoad(HInstance, rtBitmap, 'AC_SHADOW15', clWhite);
    FImages.ResInstLoad(HInstance, rtBitmap, 'AC_SHADOW16', clWhite);
    FImages.ResInstLoad(HInstance, rtBitmap, 'AC_SHADOW17', clWhite);
    FImages.ResInstLoad(HInstance, rtBitmap, 'AC_SHADOW18', clWhite);
    FImages.ResInstLoad(HInstance, rtBitmap, 'AC_SHADOW19', clWhite);
  end;
  Initialize;
end;

//------------------------------------------------------------------------------

destructor TAdvSmoothShadowSelector.Destroy;
begin
  //if not (csDesigning in ComponentState) and Assigned(FImages) then
  begin
    FImages.Free;
  end;

  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothShadowSelector.Initialize;
begin
  Tools.Clear;

  with self.Tools.Add do
  begin
    ItemType := itFullWidthButton;
    CaptionAlignment := taCenter;
    Caption := 'No Shadow';
    Hint := 'No Shadow';
  end;

  with self.Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    Hint := 'Shadow Style 1';
    ImageIndex := 0;
  end;
  with self.Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    ImageIndex := 1;
    Hint := 'Shadow Style 2';
  end;
  with self.Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    ImageIndex := 2;
    Hint := 'Shadow Style 3';
  end;
  with self.Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    ImageIndex := 3;
    Hint := 'Shadow Style 4';
  end;
  with self.Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    ImageIndex := 4;
    Hint := 'Shadow Style 5';
  end;
  with self.Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    ImageIndex := 5;
    Hint := 'Shadow Style 6';
  end;
  with self.Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    ImageIndex := 6;
    Hint := 'Shadow Style 7';
  end;
  with self.Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    ImageIndex := 7;
    Hint := 'Shadow Style 8';
  end;

  with self.Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    ImageIndex := 8;
    Hint := 'Shadow Style 9';
  end;
  with self.Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    ImageIndex := 9;
    Hint := 'Shadow Style 10';
  end;
  with self.Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    ImageIndex := 10;
    Hint := 'Shadow Style 11';
  end;
  with self.Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    ImageIndex := 11;
    Hint := 'Shadow Style 12';
  end;
  with self.Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    ImageIndex := 12;
    Hint := 'Shadow Style 13';
  end;
  with self.Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    ImageIndex := 13;
    Hint := 'Shadow Style 14';
  end;
  with self.Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    ImageIndex := 14;
    Hint := 'Shadow Style 15';
  end;
  with self.Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    ImageIndex := 15;
    Hint := 'Shadow Style 16';
  end;
  with self.Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    ImageIndex := 16;
    Hint := 'Shadow Style 17';
  end;
  with self.Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    ImageIndex := 17;
    Hint := 'Shadow Style 18';
  end;
  with self.Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    ImageIndex := 18;
    Hint := 'Shadow Style 19';
  end;
  with self.Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    ImageIndex := 19;
    Hint := 'Shadow Style 20';
  end;

  with self.Tools.Add do
  begin
    ItemType := itFullWidthButton;
    CaptionAlignment := taCenter;
    Caption := 'Shadow Settings';
    Hint := 'Shadow Settings';
  end;

end;

//------------------------------------------------------------------------------

procedure TAdvSmoothShadowSelector.Loaded;
begin
  inherited;

end;

//------------------------------------------------------------------------------

procedure TAdvSmoothShadowSelector.SetSelectorPanel;
begin
  inherited;
  FSelectorPanel.Images := Fimages;
  FSelectorPanel.ButtonsPerRow := 4;
  FSelectorPanel.SetItemsPosition;
end;

//------------------------------------------------------------------------------


{ TAdvSmoothTableBorderSelector }

constructor TAdvSmoothTableBorderSelector.Create(AOwner: TComponent);
begin
  inherited;
  //if not (csDesigning in ComponentState) then
  begin
    FImages := TCustomImageList.Create(self);
    FImages.Width := 15;
    FImages.Height := 15;
    FImages.ResInstLoad(HInstance, rtBitmap, 'AC_TABLE0', clWhite);
    FImages.ResInstLoad(HInstance, rtBitmap, 'AC_TABLE1', clWhite);
    FImages.ResInstLoad(HInstance, rtBitmap, 'AC_TABLE2', clWhite);
    FImages.ResInstLoad(HInstance, rtBitmap, 'AC_TABLE3', clWhite);
    FImages.ResInstLoad(HInstance, rtBitmap, 'AC_TABLE4', clWhite);
    FImages.ResInstLoad(HInstance, rtBitmap, 'AC_TABLE5', clWhite);
    FImages.ResInstLoad(HInstance, rtBitmap, 'AC_TABLE6', clWhite);
    FImages.ResInstLoad(HInstance, rtBitmap, 'AC_TABLE7', clWhite);
    FImages.ResInstLoad(HInstance, rtBitmap, 'AC_TABLE8', clWhite);
    FImages.ResInstLoad(HInstance, rtBitmap, 'AC_TABLE9', clWhite);
    FImages.ResInstLoad(HInstance, rtBitmap, 'AC_TABLE10', clWhite);
    FImages.ResInstLoad(HInstance, rtBitmap, 'AC_TABLE11', clWhite);
    FImages.ResInstLoad(HInstance, rtBitmap, 'AC_TABLE12', clWhite);
  end;
  Initialize;
end;

//------------------------------------------------------------------------------

destructor TAdvSmoothTableBorderSelector.Destroy;
begin
  //if not (csDesigning in ComponentState) and Assigned(FImages) then
  begin
    FImages.Free;
  end;

  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothTableBorderSelector.Initialize;
begin
  Tools.Clear;

  with Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    Hint := 'OutSide Border';
    ImageIndex := 0;
  end;
  with Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    ImageIndex := 1;
    Hint := 'All Border';
  end;
  with Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    ImageIndex := 2;
    Hint := 'Top Border';
  end;
  with Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    ImageIndex := 3;
    Hint := 'Left Border';
  end;
  with Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    ImageIndex := 4;
    Hint := 'InSide Horizontal Border';
  end;
  with Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    ImageIndex := 5;
    Hint := 'Decending Diagonal';
  end;
  with Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    ImageIndex := 6;
    Hint := 'Horizontal Line';
  end;
  with Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    ImageIndex := 7;
    Hint := 'Inside Border';
  end;
  with Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    ImageIndex := 8;
    Hint := 'No Border';
  end;
  with Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    ImageIndex := 9;
    Hint := 'Bottom Border';
  end;
  with Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    ImageIndex := 10;
    Hint := 'Right Border';
  end;
  with Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    ImageIndex := 11;
    Hint := 'InSide Vertical Border';
  end;
  with Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    ImageIndex := 12;
    Hint := 'Ascending Diagonal';
  end;

end;

//------------------------------------------------------------------------------

procedure TAdvSmoothTableBorderSelector.SetSelectorPanel;
begin
  inherited;
  FSelectorPanel.Images := Fimages;
  FSelectorPanel.ButtonsPerRow := 7;
  FSelectorPanel.SetItemsPosition;
end;

//------------------------------------------------------------------------------


{ TAdvSmoothGradientDirectionSelector }

constructor TAdvSmoothGradientDirectionSelector.Create(AOwner: TComponent);
begin
  inherited;
{  if not (csDesigning in ComponentState) then
  begin
    FImages:= TCustomImageList.Create(self);
    FImages.Width:= 101;
    FImages.Height:= 22;
    FImages.ResInstLoad(HInstance, rtBitmap, 'GRADIENT0', clBlack);
    FImages.ResInstLoad(HInstance, rtBitmap, 'GRADIENT1', clBlack);
    FImages.ResInstLoad(HInstance, rtBitmap, 'GRADIENT2', clBlack);
    FImages.ResInstLoad(HInstance, rtBitmap, 'GRADIENT3', clBlack);
    FImages.ResInstLoad(HInstance, rtBitmap, 'GRADIENT4', clBlack);
    FImages.ResInstLoad(HInstance, rtBitmap, 'GRADIENT5', clBlack);
  end; }
  FStartColor := clGray;
  FEndColor := clWhite;
  FShowSelectedGradient := false;
  Initialize;
end;

//------------------------------------------------------------------------------

destructor TAdvSmoothGradientDirectionSelector.Destroy;
begin
 { if not (csDesigning in ComponentState) and Assigned(FImages) then
  begin
    FImages.Free;
  end;
 }
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothGradientDirectionSelector.DrawGlyphAndCaption(
  aGlyph: TBitmap; R: TRect);
var
  CapR: TRect;
  clr: TColor;

  procedure DrawGradientGlyph(ACanvas: TCanvas; Color1, Color2: TColor; Index: integer; R: TRect);
  var
    R2: TRect;
  begin
    R2 := Rect(R.Left + 4, R.Top + 4, R.Right - 4, R.Bottom - 4);

    case Index of
    0: DrawGradient(ACanvas, Color1, Color2, 80, R2, false);
    1: DrawGradient(ACanvas, Color2, Color1, 80, R2, false);
    2: DrawGradient(ACanvas, Color1, Color2, 80, R2, true);
    3: DrawGradient(ACanvas, Color2, Color1, 80, R2, true);
    4: begin
         DrawGradient(ACanvas, Color1, Color2, 80, Rect(R2.Left, R2.top, R2.Right, R2.top + ((R2.Bottom - R2.Top) div 2)), false);
         DrawGradient(ACanvas, Color2, Color1, 80, Rect(R2.Left, R2.top + ((R2.Bottom - R2.Top) div 2), R2.Right, R2.Bottom), false);
       end;
    5: begin
         DrawGradient(ACanvas, Color1, Color2, 80, Rect(R2.Left, R2.top, R2.Left + ((R2.Right - R2.Left) div 2), R2.Bottom), true);
         DrawGradient(ACanvas, Color2, Color1, 80, Rect(R2.Left + ((R2.Right - R2.Left) div 2), R2.Top, R2.Right, R2.Bottom), true);
       end;
    end;
  end;

begin
  CapR := R;
  clr := Canvas.Pen.Color;
  if Style = ssButton then
  begin
    if not aGlyph.Empty then
    begin
      if ShowSelectedGradient then
      begin
        CapR.Left := DrawGlyph(Glyph, Rect(R.Left, R.Top, R.Right, R.Bottom - 5));
        CapR.Bottom := CapR.Bottom - 5;
        DrawGradientGlyph(Canvas,StartColor, EndColor, SelectedIndex, R);
      end
      else
      begin
        CapR.Left := DrawGlyph(Glyph, Rect(R.Left, R.Top, R.Right, R.Bottom));
      end;
    end
    else
    begin
      if ShowSelectedGradient then
      begin
        if (Caption = '') then
        begin
          DrawGradientGlyph(Canvas,StartColor, EndColor, SelectedIndex, R);
        end
        else
        begin
          DrawGradientGlyph(Canvas,StartColor, EndColor, SelectedIndex, R);
          CapR.Bottom := CapR.Bottom - 5;
        end;
      end;
    end;
    SetBkMode(Canvas.Handle, TRANSPARENT);
    if Caption <> '' then
{$IFNDEF TMSDOTNET}
      DrawText(Canvas.Handle, PChar(Caption), -1, CapR, DT_SINGLELINE or DT_CENTER or DT_VCENTER);
{$ENDIF}
{$IFDEF TMSDOTNET}
    DrawText(Canvas.Handle, Caption, -1, CapR, DT_SINGLELINE or DT_CENTER or DT_VCENTER);
{$ENDIF}
  end
  else if Style = ssCombo then
  begin
    if {(SelectedIndex >= 0) and }(Caption = '') then
    begin
      Canvas.Pen.Color := clBlack;
      Canvas.Brush.Color := StartColor;
      DrawGradientGlyph(Canvas,StartColor, EndColor, SelectedIndex, R);
      //Canvas.Rectangle(R.Left + 4, R.Top + 3, R.Right - 4, R.Bottom - 3);
    end;
  end;
  Canvas.Pen.Color := clr;
end;

procedure TAdvSmoothGradientDirectionSelector.Initialize;
begin
  Tools.Clear;

  with Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    Hint := 'VerticalIn';
    Value := 'VerticalIn';
  end;
  with Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    Hint := 'VerticalOut';
    Value := 'VerticalOut';
  end;
  with Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    Hint := 'HorizontalIn';
    Value := 'HorizontalIn';
  end;
  with Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    Hint := 'HorizontalOut';
    Value := 'HorizontalOut';
  end;
  with Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    Hint := 'VerticalInOut';
    Value := 'VerticalInOut';
  end;
  with Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    Hint := 'HorizontalInOut';
    Value := 'HorizontalInOut';
  end;

end;

//------------------------------------------------------------------------------

procedure TAdvSmoothGradientDirectionSelector.SelectorPanelOnDrawItem(Sender: TObject;
  Index: integer; R: TRect);
var
  R2: TRect;
begin
  R2 := Rect(R.Left + 4, R.Top + 4, R.Right - 4, R.Bottom - 4);

  if FSelectorPanel.Items.Items[Index].Value = 'VerticalIn' then
  begin
    DrawGradient(FSelectorPanel.canvas, StartColor, EndColor, 80, R2, false);
  end
  else if FSelectorPanel.Items.Items[Index].Value = 'VerticalOut' then
  begin
    DrawGradient(FSelectorPanel.canvas, EndColor, StartColor, 80, R2, false);
  end
  else if FSelectorPanel.Items.Items[Index].Value = 'HorizontalIn' then
  begin
    DrawGradient(FSelectorPanel.canvas, StartColor, EndColor, 80, R2, true);
  end
  else if FSelectorPanel.Items.Items[Index].Value = 'HorizontalOut' then
  begin
    DrawGradient(FSelectorPanel.canvas, EndColor, StartColor, 80, R2, true);
  end
  else if FSelectorPanel.Items.Items[Index].Value = 'VerticalInOut' then
  begin
    DrawGradient(FSelectorPanel.canvas, StartColor, EndColor, 80, Rect(R2.Left, R2.top, R2.Right, R2.top + ((R2.Bottom - R2.Top) div 2)), false);
    DrawGradient(FSelectorPanel.canvas, EndColor, StartColor, 80, Rect(R2.Left, R2.top + ((R2.Bottom - R2.Top) div 2), R2.Right, R2.Bottom), false);
  end
  else if FSelectorPanel.Items.Items[Index].Value = 'HorizontalInOut' then
  begin
    DrawGradient(FSelectorPanel.canvas, StartColor, EndColor, 80, Rect(R2.Left, R2.top, R2.Left + ((R2.Right - R2.Left) div 2), R2.Bottom), true);
    DrawGradient(FSelectorPanel.canvas, EndColor, StartColor, 80, Rect(R2.Left + ((R2.Right - R2.Left) div 2), R2.Top, R2.Right, R2.Bottom), true);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothGradientDirectionSelector.SetEndColor(const Value: TColor);
begin
  FEndColor := Value;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothGradientDirectionSelector.SetSelectorPanel;
begin
  inherited;
  FSelectorPanel.OnDrawItem := SelectorPanelOnDrawItem;
  FSelectorPanel.MinButtonWidth := 100;
  FSelectorPanel.MinButtonHeight := 20;

  FSelectorPanel.SetItemsPosition;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothGradientDirectionSelector.SetShowSelectedGradient(
  const Value: boolean);
begin
  FShowSelectedGradient := Value;
  Invalidate;
end;

procedure TAdvSmoothGradientDirectionSelector.SetStartColor(const Value: TColor);
begin
  FStartColor := Value;
  Invalidate;
end;

//------------------------------------------------------------------------------


{ TAdvSmoothPenStyleSelector }

constructor TAdvSmoothPenWidthSelector.Create(AOwner: TComponent);
begin
  inherited;
  //if not (csDesigning in ComponentState) then
  begin
    FImages := TCustomImageList.Create(self);
    FImages.Width := 97;
    FImages.Height := 10;
    FImages.ResInstLoad(HInstance, rtBitmap, 'AC_PENWIDTH0', clWhite);
    FImages.ResInstLoad(HInstance, rtBitmap, 'AC_PENWIDTH1', clWhite);
    FImages.ResInstLoad(HInstance, rtBitmap, 'AC_PENWIDTH2', clWhite);
    FImages.ResInstLoad(HInstance, rtBitmap, 'AC_PENWIDTH3', clWhite);
    FImages.ResInstLoad(HInstance, rtBitmap, 'AC_PENWIDTH4', clWhite);
    FImages.ResInstLoad(HInstance, rtBitmap, 'AC_PENWIDTH5', clWhite);
    FImages.ResInstLoad(HInstance, rtBitmap, 'AC_PENWIDTH6', clWhite);
    FImages.ResInstLoad(HInstance, rtBitmap, 'AC_PENWIDTH7', clWhite);
    FImages.ResInstLoad(HInstance, rtBitmap, 'AC_PENWIDTH8', clWhite);
    FImages.ResInstLoad(HInstance, rtBitmap, 'AC_PENWIDTH9', clWhite);
    FImages.ResInstLoad(HInstance, rtBitmap, 'AC_PENWIDTH10', clWhite);
    FImages.ResInstLoad(HInstance, rtBitmap, 'AC_PENWIDTH11', clWhite);
    FImages.ResInstLoad(HInstance, rtBitmap, 'AC_PENWIDTH12', clWhite);
  end;
  Initialize;
end;

//------------------------------------------------------------------------------

destructor TAdvSmoothPenWidthSelector.Destroy;
begin
  //if not (csDesigning in ComponentState) and Assigned(FImages) then
  begin
    FImages.Free;
  end;

  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothPenWidthSelector.Initialize;
begin
  Tools.Clear;

  with Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    CaptionAlignment := taLeftJustify;
    Caption := '1/4 pt';
    ImageIndex := 0;
  end;
  with Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    ImageIndex := 1;
    CaptionAlignment := taLeftJustify;
    Caption := '1/2 pt';
  end;
  with Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    ImageIndex := 2;
    CaptionAlignment := taLeftJustify;
    Caption := '3/4 pt';
  end;
  with Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    ImageIndex := 3;
    CaptionAlignment := taLeftJustify;
    Caption := '1 pt';
  end;
  with Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    ImageIndex := 4;
    CaptionAlignment := taLeftJustify;
    Caption := '1 1/2 pt';
  end;
  with Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    ImageIndex := 5;
    CaptionAlignment := taLeftJustify;
    Caption := '2 1/4 pt';
  end;
  with Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    ImageIndex := 6;
    CaptionAlignment := taLeftJustify;
    Caption := '3 pt';
  end;
  with Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    ImageIndex := 7;
    CaptionAlignment := taLeftJustify;
    Caption := '4 1/2 pt';
  end;
  with Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    ImageIndex := 8;
    CaptionAlignment := taLeftJustify;
    Caption := '6 pt';
  end;
  with Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    ImageIndex := 9;
    CaptionAlignment := taLeftJustify;
    Caption := '3 pt';
  end;
  with Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    ImageIndex := 10;
    CaptionAlignment := taLeftJustify;
    Caption := '4 1/2 pt';
  end;
  with Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    ImageIndex := 11;
    CaptionAlignment := taLeftJustify;
    Caption := '4 1/2 pt';
  end;
  with Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    ImageIndex := 12;
    CaptionAlignment := taLeftJustify;
    Caption := '6 pt';
  end;

  with Tools.Add do
  begin
    ItemType := itFullWidthButton;
    CaptionAlignment := taCenter;
    Caption := 'More Lines';
    Hint := 'More Lines';
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothPenWidthSelector.SetSelectorPanel;
begin
  inherited;
  FSelectorPanel.Images := Fimages;
  FSelectorPanel.SetItemsPosition;
end;

//------------------------------------------------------------------------------


{ TAdvColorSelector }

constructor TAdvSmoothCustomColorSelector.Create(AOwner: TComponent);
begin
  inherited;
  FColorSelectionStyle := csDiscrete;
  FShowSelectedColor := true;
  FSelectedColor := clNone;
  FShowRGBHint := true;
  ButtonsPerRow := 8;
  Initialize;
end;

//------------------------------------------------------------------------------

destructor TAdvSmoothCustomColorSelector.Destroy;
begin
  if not (csDesigning in ComponentState) then
  begin
    if Assigned(FColorCubePanel) then
      FColorCubePanel.Free;
    if Assigned(FSpectrumPanel) then
      FSpectrumPanel.Free;
  end;
  inherited;
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

procedure TAdvSmoothCustomColorSelector.Initialize;
begin
end;

procedure TAdvSmoothColorSelector.Initialize;
begin
  Tools.Clear;

  with Tools.Add do
  begin
    ItemType := itFullWidthButton;
    CaptionAlignment := taCenter;
    Caption := 'Automatic';
    Hint := 'Automatic';
    //Value:= ColorToString(clBlack);
    BackGroundColor := clBlack;
  end;

  with Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    //Value:= ColorToString(clBlack);
    BackGroundColor := clBlack;
  end;
  with Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    //Value:= ColorToString($00003399);
    BackGroundColor := $00003399;
  end;
  with Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    //Value:= ColorToString($00003333);
    BackGroundColor := $00003333;
  end;
  with Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    //Value:= ColorToString($00003300);
    BackGroundColor := $00003300;
  end;
  with Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    //Value:= ColorToString($00663300);
    BackGroundColor := $00663300;
  end;
  with Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    //Value:= ColorToString(clNavy);
    BackGroundColor := clNavy;
  end;
  with Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    //Value:= ColorToString($00353333);
    BackGroundColor := $00353333;
  end;
  with Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    //Value:= ColorToString($00333333);
    BackGroundColor := $00333333;
  end;

  with Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    //Value:= ColorToString(RGB(128, 0,0));
    BackGroundColor := RGB(128, 0, 0);
  end;
  with Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    //Value:= ColorToString(RGB(255, 102, 0));
    BackGroundColor := RGB(255, 102, 0);
  end;
  with Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    //Value:= ColorToString(RGB(128, 128, 0));
    BackGroundColor := RGB(128, 128, 0);
  end;
  with Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    //Value:= ColorToString(RGB(0, 128, 0));
    BackGroundColor := RGB(0, 128, 0);
  end;
  with Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    //Value:= ColorToString(RGB(0, 128, 128));
    BackGroundColor := RGB(0, 128, 128);
  end;
  with Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    //Value:= ColorToString(RGB(0, 0, 255));
    BackGroundColor := RGB(0, 0, 255);
  end;
  with Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    //Value:= ColorToString(RGB(102, 102, 153));
    BackGroundColor := RGB(102, 102, 153);
  end;
  with Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    //Value:= ColorToString(RGB(128, 128, 128));
    BackGroundColor := RGB(128, 128, 128);
  end;

  with Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    //Value:= ColorToString(RGB(255, 0,0));
    BackGroundColor := RGB(255, 0, 0);
  end;
  with Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    //Value:= ColorToString(RGB(255, 153, 0));
    BackGroundColor := RGB(255, 153, 0);
  end;
  with Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    //Value:= ColorToString(RGB(153, 204, 0));
    BackGroundColor := RGB(153, 204, 0);
  end;
  with Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    //Value:= ColorToString(RGB(51, 153, 102));
    BackGroundColor := RGB(51, 153, 102);
  end;
  with Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    //Value:= ColorToString(RGB(51, 204, 204));
    BackGroundColor := RGB(51, 204, 204);
  end;
  with Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    //Value:= ColorToString(RGB(51, 102, 255));
    BackGroundColor := RGB(51, 102, 255);
  end;
  with Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    //Value:= ColorToString(RGB(128, 0, 128));
    BackGroundColor := RGB(128, 0, 128);
  end;
  with Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    //Value:= ColorToString(RGB(153, 153, 153));
    BackGroundColor := RGB(153, 153, 153);
  end;

  with Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    //Value:= ColorToString(RGB(255, 0, 255));
    BackGroundColor := RGB(255, 0, 255);
  end;
  with Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    //Value:= ColorToString(RGB(255, 204, 0));
    BackGroundColor := RGB(255, 204, 0);
  end;
  with Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    //Value:= ColorToString(RGB(255, 255, 0));
    BackGroundColor := RGB(255, 255, 0);
  end;
  with Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    //Value:= ColorToString(RGB(0, 255, 0));
    BackGroundColor := RGB(0, 255, 0);
  end;
  with Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    //Value:= ColorToString(RGB(0, 255, 255));
    BackGroundColor := RGB(0, 255, 255);
  end;
  with Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    //Value:= ColorToString(RGB(0, 204, 255));
    BackGroundColor := RGB(0, 204, 255);
  end;
  with Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    //Value:= ColorToString(RGB(153, 51, 102));
    BackGroundColor := RGB(153, 51, 102);
  end;
  with Tools.Add do
  begin
    ItemType := itAutoSizeButton;
   //Value:= ColorToString(RGB(192, 192, 192));
    BackGroundColor := RGB(192, 192, 192);
  end;

  with Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    //Value:= ColorToString(RGB(255, 153, 204));
    BackGroundColor := RGB(255, 153, 204);
  end;
  with Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    //Value:= ColorToString(RGB(255, 204, 153));
    BackGroundColor := RGB(255, 204, 153);
  end;
  with Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    //Value:= ColorToString(RGB(255, 255, 153));
    BackGroundColor := RGB(255, 255, 153);
  end;
  with Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    //Value:= ColorToString(RGB(204, 255, 204));
    BackGroundColor := RGB(204, 255, 204);
  end;
  with Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    //Value:= ColorToString(RGB(204, 255, 255));
    BackGroundColor := RGB(204, 255, 255);
  end;
  with Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    //Value:= ColorToString(RGB(153, 204, 255));
    BackGroundColor := RGB(153, 204, 255);
  end;
  with Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    //Value:= ColorToString(RGB(204, 153, 255));
    BackGroundColor := RGB(204, 153, 255);
  end;
  with Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    //Value:= ColorToString(RGB(255, 255, 255));
    BackGroundColor := RGB(255, 255, 255);
  end;

  with Tools.Add do
  begin
    ItemType := itFullWidthButton;
    CaptionAlignment := taCenter;
    Caption := 'More Colors...';
    Hint := 'More Color';
    //Value:= ColorToString(clNone);
    BackGroundColor := clNone;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCustomColorSelector.SetSelectorPanel;
var
  i: integer;
begin
  if SelectionStyle = csDiscrete then
  begin
    if Assigned(FColorCubePanel) then
      FColorCubePanel.Visible:= false;
    if Assigned(FSpectrumPanel) then
      FSpectrumPanel.Visible:= false;

    inherited;
    FSelectorPanel.OnShouldHide := OnDropDownPanelShouldHide;
    FSelectorPanel.OnDrawItem := SelectorPanelOnDrawItem;

    FSelectorPanel.MinButtonWidth := 18;
    FSelectorPanel.MinButtonHeight := 18;

    //---- Setting Selected Index
    if FSelectedColor <> clNone then
    begin
      for i := 0 to FSelectorPanel.Items.Count - 1 do
      begin
        if FSelectedColor = FSelectorPanel.Items.Items[i].BackGroundColor then
        begin
          FSelectorPanel.ItemIndex := i;
          FSelectedIndex := i;
          break;
        end;
      end;
    end;
    //----

    FSelectorPanel.ButtonsPerRow := ButtonsPerRow;
    FSelectorPanel.SetItemsPosition;

    if not FSelectorPanel.Visible then
      FSelectorPanel.Visible:= true;
  end
  else if SelectionStyle = csColorCube then
  begin
    if Assigned(FSelectorPanel) then
      FSelectorPanel.Visible:= false;
    if assigned(FSpectrumPanel) then
      FSpectrumPanel.Visible:= false;

    if not assigned(FColorCubePanel) and assigned(FDropDownWindow) then
    begin
      FColorCubePanel := TAdvSmoothColorCubePanel.Create(FDropDownWindow);
      FColorCubePanel.Parent := FDropDownWindow;
      FColorCubePanel.WindowBorderColor := FBorderDropDownColor;
      FColorCubePanel.OnShouldHide := OnDropDownPanelShouldHide;
      FColorCubePanel.OnSelect := CubePanelOnSelect;
    end;
    FColorCubePanel.ShowRGBHint := FShowRGBHint;
    FDropDownWindow.SelectorPanel := FColorCubePanel;

    FColorCubePanel.SelectedColor := FSelectedColor;

    FColorCubePanel.SetItemsPosition;

    FColorCubePanel.Left := 0;
    FColorCubePanel.Top := 0;

    if not FColorCubePanel.Visible then
      FColorCubePanel.Visible:= true;

    //FColorCubePanel.Width:= 100;
    //FColorCubePanel.Height:= 140;
  end
  else if SelectionStyle = csSpectrum then
  begin
    if Assigned(FSelectorPanel) then
      FSelectorPanel.Visible:= false;
    if assigned(FColorCubePanel) then
      FColorCubePanel.Visible:= false;

    if not assigned(FSpectrumPanel) and assigned(FDropDownWindow) then
    begin
      FSpectrumPanel := TAdvSmoothColorSpectrumPanel.Create(FDropDownWindow);
      FSpectrumPanel.Parent := FDropDownWindow;
      FSpectrumPanel.WindowBorderColor := FBorderDropDownColor;
      FSpectrumPanel.OnShouldHide := OnDropDownPanelShouldHide;
      FSpectrumPanel.OnSelect := SpectrumPanelOnSelect;
    end;
    FDropDownWindow.SelectorPanel := FSpectrumPanel;

    FSpectrumPanel.SelectedColor := FSelectedColor;

    FSpectrumPanel.SetItemsPosition;

    FSpectrumPanel.Left := 0;
    FSpectrumPanel.Top := 0;

    if not FSpectrumPanel.Visible then
      FSpectrumPanel.Visible:= true;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCustomColorSelector.SetColorSelectionStyle(
  const Value: TColorSelectionStyle);
begin
  if FColorSelectionStyle <> Value then
  begin
    FColorSelectionStyle := Value;
    case FColorSelectionStyle of
      csDiscrete:
        begin

        end;
      csColorCube:
        begin

        end;
      csSpectrum:
        begin

        end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCustomColorSelector.Loaded;
begin
  inherited;

end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCustomColorSelector.DrawGlyphAndCaption(aGlyph: TBitmap; R: TRect);
var
  CapR: TRect;
  clr: TColor;
begin
  CapR := R;
  clr := Canvas.Pen.Color;
  if Style = ssButton then
  begin
    if not aGlyph.Empty then
    begin
      if ShowSelectedColor and (SelectedColor <> clNone) then
      begin
        CapR.Left := DrawGlyph(Glyph, Rect(R.Left, R.Top, R.Right, R.Bottom - 5));
        CapR.Bottom := CapR.Bottom - 5;
        Canvas.Pen.Color := SelectedColor;
        Canvas.Brush.Color := SelectedColor;
        Canvas.Rectangle(R.Left + 3, R.Bottom - 6, R.Right - 3, R.Bottom - 2);
      end
      else
      begin
        CapR.Left := DrawGlyph(Glyph, Rect(R.Left, R.Top, R.Right, R.Bottom));
      end;
    end
    else
    begin
      if ShowSelectedColor and (SelectedColor <> clNone) then
      begin
        if (Caption = '') then
        begin
          Canvas.Pen.Color := SelectedColor;
          Canvas.Brush.Color := SelectedColor;
          Canvas.Rectangle(R.Left + 4, R.Top + 3, R.Right - 4, R.Bottom - 3);
        end
        else
        begin
          Canvas.Pen.Color := SelectedColor;
          Canvas.Brush.Color := SelectedColor;
          Canvas.Rectangle(R.Left + 4, R.Bottom - 6, R.Right - 4, R.Bottom - 3);
          CapR.Bottom := CapR.Bottom - 5;
        end;
      end;
    end;
    SetBkMode(Canvas.Handle, TRANSPARENT);
    if Caption <> '' then
{$IFNDEF TMSDOTNET}
      DrawText(Canvas.Handle, PChar(Caption), -1, CapR, DT_SINGLELINE or DT_CENTER or DT_VCENTER);
{$ENDIF}
{$IFDEF TMSDOTNET}
    DrawText(Canvas.Handle, Caption, -1, CapR, DT_SINGLELINE or DT_CENTER or DT_VCENTER);
{$ENDIF}
  end
  else
  if Style = ssCombo then
  begin
    if {(SelectedIndex >= 0) and }(Caption = '') and (SelectedColor <> clNone) then
    begin
      Canvas.Pen.Color := clBlack;
      Canvas.Brush.Color := SelectedColor;
      Canvas.Rectangle(R.Left + 4, R.Top + 3, R.Right - 4, R.Bottom - 3);
    end;
  end;
  Canvas.Pen.Color := clr;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCustomColorSelector.SelectorPanelOnDrawItem(Sender: TObject; Index: integer;
  R: TRect);
begin
  if FSelectorPanel.Items.Items[Index].ItemType = itAutoSizeButton then
  begin
    if FSelectorPanel.Items.Items[Index].BackGroundColor <> clNone then
    begin
      FSelectorPanel.Canvas.Pen.Color := clGray;
      FSelectorPanel.Canvas.Brush.Color := FSelectorPanel.Items.Items[Index].BackGroundColor;
      FSelectorPanel.Canvas.Rectangle(R.Left + 3, R.Top + 3, R.Left + 15, R.Top + 15);
    end;
  end
  else
  begin
    if FSelectorPanel.Items.Items[Index].BackGroundColor <> clNone then
    begin
      FSelectorPanel.Canvas.Pen.Color := clGray;
      FSelectorPanel.Canvas.Brush.Color := FSelectorPanel.Items.Items[Index].BackGroundColor;
      FSelectorPanel.Canvas.Rectangle(R.Left + 4, R.Top + 4, R.Left + 16, R.Top + 16);
    end;
    FSelectorPanel.Canvas.Brush.Style := bsClear;
{$IFNDEF TMSDOTNET}
    if (FSelectorPanel.Items.Items[Index].CaptionAlignment = taCenter) and (FSelectorPanel.Items.Items[Index].Caption <> '') then
      DrawText(FSelectorPanel.Canvas.Handle, PChar(FSelectorPanel.Items.Items[Index].Caption), -1, R, DT_SINGLELINE or DT_VCENTER or DT_CENTER)
    else if (FSelectorPanel.Items.Items[Index].CaptionAlignment = taRightJustify) and (FSelectorPanel.Items.Items[Index].Caption <> '') then
      DrawText(FSelectorPanel.Canvas.Handle, PChar(FSelectorPanel.Items.Items[Index].Caption), -1, R, DT_SINGLELINE or DT_VCENTER);
{$ENDIF}
{$IFDEF TMSDOTNET}
    if (FSelectorPanel.Items.Items[Index].CaptionAlignment = taCenter) and (FSelectorPanel.Items.Items[Index].Caption <> '') then
      DrawText(FSelectorPanel.Canvas.Handle, FSelectorPanel.Items.Items[Index].Caption, -1, R, DT_SINGLELINE or DT_VCENTER or DT_CENTER)
    else if (FSelectorPanel.Items.Items[Index].CaptionAlignment = taRightJustify) and (FSelectorPanel.Items.Items[Index].Caption <> '') then
      DrawText(FSelectorPanel.Canvas.Handle, FSelectorPanel.Items.Items[Index].Caption, -1, R, DT_SINGLELINE or DT_VCENTER);
{$ENDIF}
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCustomColorSelector.SetShowSelectedColor(const Value: Boolean);
begin
  FShowSelectedColor := Value;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCustomColorSelector.CubePanelOnSelect(Sender: TObject);
begin
  FSelectedColor := FColorCubePanel.SelectedColor;

  if Assigned(FOnSelect) then
    FOnSelect(self, -1, nil);
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCustomColorSelector.SpectrumPanelOnSelect(Sender: TObject);
begin
  FSelectedColor := FSpectrumPanel.SelectedColor;

  if Assigned(FOnSelect) then
    FOnSelect(self, -1, nil);
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCustomColorSelector.SetSelectedColor(const Value: TColor);
begin
  FSelectedColor := Value;
  Invalidate;
end;

//------------------------------------------------------------------------------

function TAdvSmoothCustomColorSelector.GetSelectedColor: TColor;
begin
  Result := FSelectedColor;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCustomColorSelector.OnToolSelect;
begin
  if SelectedIndex >= 0 then
  begin
    if Tools.Items[SelectedIndex].BackGroundColor <> clNone then
      FSelectedColor := Tools.Items[SelectedIndex].BackGroundColor;
  end;  
  inherited;
end;

//------------------------------------------------------------------------------

{ TAdvSmoothTextColorSelector }

procedure TAdvSmoothTextColorSelector.Initialize;
begin
  Tools.Clear;

  ButtonsPerRow := 8;

  with Tools.Add do
  begin
    ItemType := itFullWidthButton;
    CaptionAlignment := taCenter;
    Caption := 'Automatic';
    Hint := 'Color';
    //Value:= ColorToString(clBlack);
    BackGroundColor := clBlack;
  end;

  with Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    //Value:= ColorToString(clBlack);
    BackGroundColor := clBlack;
    Hint := 'Black';
  end;
  with Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    //Value:= ColorToString($00003399);
    BackGroundColor := clGray;
    Hint := 'Gray';
  end;
  with Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    //Value:= ColorToString($00003333);
    BackGroundColor := clMaroon;
    Hint := 'Maroon';
  end;
  with Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    //Value:= ColorToString($00003300);
    BackGroundColor := clOlive;
    Hint := 'Olive';
  end;
  with Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    //Value:= ColorToString($00663300);
    BackGroundColor := clGreen;
    Hint := 'Green';
  end;
  with Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    //Value:= ColorToString(clNavy);
    BackGroundColor := clTeal;
    Hint := 'Teal';
  end;
  with Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    //Value:= ColorToString($00353333);
    BackGroundColor := clNavy;
    Hint := 'Navy';
  end;
  with Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    //Value:= ColorToString($00333333);
    BackGroundColor := clPurple;
    Hint := 'Purple';
  end;

  with Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    //Value:= ColorToString(RGB(128, 0,0));
    BackGroundColor := clWhite;
    Hint := 'White';
  end;
  with Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    //Value:= ColorToString(RGB(255, 102, 0));
    BackGroundColor := clSilver;
    Hint := 'Silver';
  end;
  with Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    //Value:= ColorToString(RGB(128, 128, 0));
    BackGroundColor := clRed;
    Hint := 'Red';
  end;
  with Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    //Value:= ColorToString(RGB(0, 128, 0));
    BackGroundColor := clYellow;
    Hint := 'Yellow';
  end;
  with Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    //Value:= ColorToString(RGB(0, 128, 128));
    BackGroundColor := clLime;
    Hint := 'Lime';
  end;
  with Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    //Value:= ColorToString(RGB(0, 0, 255));
    BackGroundColor := clAqua;
    Hint := 'Aqua';
  end;
  with Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    //Value:= ColorToString(RGB(102, 102, 153));
    BackGroundColor := clBlue;
    Hint := 'Blue';
  end;

  with Tools.Add do
  begin
    ItemType := itAutoSizeButton;
    //Value:= ColorToString(RGB(128, 128, 128));
    BackGroundColor := clFuchsia;
    Hint := 'Fuchsia';
  end;

end;

//------------------------------------------------------------------------------

{ TAdvSmoothTableSelectorPanel }

constructor TAdvSmoothTableSelectorPanel.Create(AOwner: TComponent);
begin
  inherited;
  FColCount := 5;
  FRowCount := 4;
  FSelectedCols := 0;
  FSelectedRows := 0;
  FCellSize := TABLECELLSIZE;
  FLeftMargin := 4;
  FTopMargin := 4;
  FCellSpace := 2;
  FSelectionColor := clNavy;
  FLabelHeight := 20;
end;

//------------------------------------------------------------------------------

destructor TAdvSmoothTableSelectorPanel.Destroy;
begin

  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothTableSelectorPanel.MouseMove(Shift: TShiftState; X,
  Y: Integer);
var
  c, r, ax, ay: integer;
  OldSelCol, OldSelRow: integer;
  CellFound: boolean;
  R2: TRect;
  P: TPoint;
begin
  inherited;

  OldSelCol := FSelectedCols;
  OldSelRow := FSelectedRows;
  ax := FLeftMargin;
  CellFound := false;

  if TSelectorDropDownWindow(FOwner).ShowAbove then
  begin
    ay := FLabelHeight + FTopMargin;
    ay := ay + ((FCellSize + FCellSpace) * (RowCount - 1));

    for r := 1 to RowCount do
    begin
      for c := 1 to ColCount do
      begin
        if PtInRect(Rect(ax, ay, ax + FCellSize, ay + FCellSize), Point(X, Y)) then
        begin
          if (FSelectedCols <> c) or (FSelectedRows <> r) then
          begin
            FSelectedCols := c;
            FSelectedRows := r;
            InvalidateSelection(OldSelCol, OldSelRow);
          end;
          CellFound := true;
          break;
        end;
        ax := ax + FCellSize + FCellSpace;
      end;

      if CellFound then
        break;
      ax := FLeftMargin;
      ay := ay - FCellSize - FCellSpace;
    end;
  end
  else
  begin
    ay := FTopMargin;
    for r := 1 to RowCount do
    begin
      for c := 1 to ColCount do
      begin
        if PtInRect(Rect(ax, ay, ax + FCellSize, ay + FCellSize), Point(X, Y)) then
        begin
          if (FSelectedCols <> c) or (FSelectedRows <> r) then
          begin
            FSelectedCols := c;
            FSelectedRows := r;
            InvalidateSelection(OldSelCol, OldSelRow);
          end;
          CellFound := true;
          break;
        end;
        ax := ax + FCellSize + FCellSpace;
      end;

      if CellFound then
        break;
      ax := FLeftMargin;
      ay := ay + FCellSize + FCellSpace;
    end;
  end;


  if TSelectorDropDownWindow(FOwner).ShowAbove then
  begin
    if (Y < FLabelHeight + FTopMargin) and not CellFound then
    begin
      FSelectedCols := -1;
      FSelectedRows := -1;
      if (OldSelCol <> -1) and (OldSelRow <> -1) then
        InvalidateSelection(OldSelCol, OldSelRow);
    end;
  end
  else
  begin
    if (Y > ay) and not CellFound then
    begin
      FSelectedCols := -1;
      FSelectedRows := -1;
      if (OldSelCol <> -1) and (OldSelRow <> -1) then
        InvalidateSelection(OldSelCol, OldSelRow);
    end;
  end;

  if (ssLeft in Shift) then
  begin
    if (X > Width - FLeftMargin - 2) and not CellFound then
    begin
{$IFNDEF TMSDOTNET}
      SystemParametersInfo(SPI_GETWORKAREA, 0, @R2, 0);
{$ENDIF}
{$IFDEF TMSDOTNET}
      SystemParametersInfo(SPI_GETWORKAREA, 0, R2, 0);
{$ENDIF}

      P := Point(0, self.Height);
      P := ClientToScreen(P);

      if (P.X + Width + FCellSize + 2) < R2.Right then
      begin
        ColCount := ColCount + 1;
        SetPanelSize;
        TSelectorDropDownWindow(FOwner).SetWindowSize;
      end;
    end;

    if TSelectorDropDownWindow(FOwner).ShowAbove then
    begin
      if (Y < FLabelHeight + 2) then
      begin
{$IFNDEF TMSDOTNET}
        SystemParametersInfo(SPI_GETWORKAREA, 0, @R2, 0);
{$ENDIF}
{$IFDEF TMSDOTNET}
        SystemParametersInfo(SPI_GETWORKAREA, 0, R2, 0);
{$ENDIF}

        P := Point(0, 0);
        P := ClientToScreen(P);

        if (P.Y - FCellSize - 2) > R2.Top then
        begin
          RowCount := RowCount + 1;
          SetPanelSize;
          TSelectorDropDownWindow(FOwner).SetWindowSize;
          TSelectorDropDownWindow(FOwner).Top := TSelectorDropDownWindow(FOwner).Top - FCellSize - FCellSpace;
        end;
      end;
    end
    else // if not TSelectorDropDownWindow(FOwner).ShowAbove then
    begin
      if (Y > Height - FLabelHeight - 2) then
      begin
{$IFNDEF TMSDOTNET}
        SystemParametersInfo(SPI_GETWORKAREA, 0, @R2, 0);
{$ENDIF}
{$IFDEF TMSDOTNET}
        SystemParametersInfo(SPI_GETWORKAREA, 0, R2, 0);
{$ENDIF}

        P := Point(0, 0);
        P := ClientToScreen(P);

        if (P.Y + Height + FCellSize + 2) < R2.Bottom then
        begin
          RowCount := RowCount + 1;
          SetPanelSize;
          TSelectorDropDownWindow(FOwner).SetWindowSize;
        end;
      end;
    end;
  end;

end;

//------------------------------------------------------------------------------

procedure TAdvSmoothTableSelectorPanel.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  if Assigned(FOnShouldHide) then
    FOnShouldHide(self);

  if Assigned(FOnSelect) and (FSelectedCols > 0) and (FSelectedRows > 0) then
    FOnSelect(self);
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothTableSelectorPanel.DrawLabel;
var
  s: string;
  R: TRect;
begin
  if (FSelectedCols > 0) or (FSelectedRows > 0) then
    s := inttostr(FSelectedCols) + ' x ' + inttostr(FSelectedRows) + ' ' + FTextTable
  else
    s := FTextCancel;

  Canvas.Brush.Style := bsclear;

  if TSelectorDropDownWindow(FOwner).ShowAbove then
    R := Rect(FLeftMargin, 1, Width - FLeftMargin, FLabelHeight)
  else
    R := Rect(FLeftMargin, Height - FLabelHeight, Width - FLeftMargin, Height - FTopMargin);

{$IFNDEF TMSDOTNET}
  DrawText(Canvas.Handle, PChar(s), -1, R, DT_SINGLELINE or DT_VCENTER or DT_CENTER);
{$ENDIF}
{$IFDEF TMSDOTNET}
  DrawText(Canvas.Handle, s, -1, R, DT_SINGLELINE or DT_VCENTER or DT_CENTER);
{$ENDIF}

end;

//------------------------------------------------------------------------------

procedure TAdvSmoothTableSelectorPanel.DrawAllCells;
var
  c, r, x, y: integer;
begin
  x := FLeftMargin;
  if TSelectorDropDownWindow(FOwner).ShowAbove then
  begin
    y := FLabelHeight + FTopMargin;
    y := y + ((FCellSize + FCellSpace) * (RowCount - 1));

    for r := 1 to RowCount do
    begin
      for c := 1 to ColCount do
      begin
        Canvas.Pen.Color := FWindowBorderColor;

        if (c <= FSelectedCols) and (r <= FSelectedRows) then
          Canvas.Brush.Color := FSelectionColor
        else
          Canvas.Brush.Color := clWhite;
        Canvas.Rectangle(x, y, x + FCellSize, y + FCellSize);

        x := x + FCellSize + FCellSpace;
      end;
      x := FLeftMargin;
      y := y - FCellSize - FCellSpace;
    end;
  end
  else
  begin
    y := FTopMargin;
    for r := 1 to RowCount do
    begin
      for c := 1 to ColCount do
      begin
        Canvas.Pen.Color := FWindowBorderColor;

        if (c <= FSelectedCols) and (r <= FSelectedRows) then
          Canvas.Brush.Color := FSelectionColor
        else
          Canvas.Brush.Color := clWhite;
        Canvas.Rectangle(x, y, x + FCellSize, y + FCellSize);

        x := x + FCellSize + FCellSpace;
      end;
      x := FLeftMargin;
      y := y + FCellSize + FCellSpace;
    end;
  end;

end;

//------------------------------------------------------------------------------

procedure TAdvSmoothTableSelectorPanel.Paint;
begin
  inherited;
  DrawAllCells;
  DrawLabel;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothTableSelectorPanel.SetCellSize(const Value: integer);
begin
  FCellSize := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothTableSelectorPanel.SetColCount(const Value: integer);
begin
  FColCount := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothTableSelectorPanel.SetItemsPosition;
begin
  SetPanelSize;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothTableSelectorPanel.SetPanelSize;
var
  c, r, x, y: integer;
begin
  x := FLeftMargin;
  y := FTopMargin;

  for c := 1 to ColCount do
  begin
    x := x + FCellSize + FCellSpace;
  end;
  x := x + FLeftMargin - FCellSpace;

  for r := 1 to RowCount do
  begin
    y := y + FCellSize + FCellSpace;
  end;
  y := y + FTopMargin - FCellSpace;

  Height := y + FLabelHeight;
  Width := Max(x, Canvas.TextWidth(FTextCancel));
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothTableSelectorPanel.SetRowCount(const Value: integer);
begin
  FRowCount := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothTableSelectorPanel.SetSelectedCols(const Value: integer);
begin
  if FSelectedCols <> Value then
  begin
    FSelectedCols := Value;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothTableSelectorPanel.SetSelectedRows(const Value: integer);
begin
  FSelectedRows := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothTableSelectorPanel.InvalidateSelection(OldSelectedCols,
  OldSelectedRows: integer);
var
  Rc, Rr: TRect;
  c, r, x, y: integer;
  RgnC, RgnR, RgnL, RgnT: HRGN;
begin
  Rc := Rect(0, 0, 0, 0);
  if OldSelectedCols <> FSelectedCols then
  begin
    x := FLeftMargin;
    if TSelectorDropDownWindow(FOwner).ShowAbove then
    begin
      y := FLabelHeight + FTopMargin;
      y := y + ((FCellSize + FCellSpace) * (RowCount - 1));

      Rc.Bottom := y + (FCellSize + FCellSpace);
      for r := 1 to FSelectedRows do
      begin
        y := y - FCellSize - FCellSpace;
      end;
      Rc.Top := y;

      for c := 1 to ColCount do
      begin
        x := x + FCellSize + FCellSpace;
        if OldSelectedCols > FSelectedCols then
        begin
          if (FSelectedCols = c) then
            Rc.Left := x;

          if (OldSelectedCols = c) then
            Rc.Right := x;
        end
        else
        begin
          if (OldSelectedCols = c) then
            Rc.Left := x;

          if (FSelectedCols = c) then
            Rc.Right := x;
        end;

      end;

    end
    else
    begin
      y := FTopMargin;

      Rc.Top := y;
      for r := 1 to FSelectedRows do
      begin
        y := y + FCellSize + FCellSpace;
      end;
      Rc.Bottom := y;

      for c := 1 to ColCount do
      begin
        x := x + FCellSize + FCellSpace;
        if OldSelectedCols > FSelectedCols then
        begin
          if (FSelectedCols = c) then
            Rc.Left := x;

          if (OldSelectedCols = c) then
            Rc.Right := x;
        end
        else
        begin
          if (OldSelectedCols = c) then
            Rc.Left := x;

          if (FSelectedCols = c) then
            Rc.Right := x;
        end;
      end;

    end;

    //x:= x + FLeftMargin - FCellSpace;
  end;

  Rr := Rect(0, 0, 0, 0);
  if OldSelectedRows <> FSelectedRows then
  begin
    x := FLeftMargin;
    if TSelectorDropDownWindow(FOwner).ShowAbove then
    begin
      y := FLabelHeight + FTopMargin;
      y := y + ((FCellSize + FCellSpace) * (RowCount));

      Rr.Left := FLeftMargin;
      for c := 1 to ColCount do
      begin
        x := x + FCellSize + FCellSpace;
      end;
      Rr.Right := x;

      for r := 1 to RowCount do
      begin
        y := y - FCellSize - FCellSpace;

        if OldSelectedRows > FSelectedRows then
        begin
          if (FSelectedRows = r) then
            Rr.Bottom := y;

          if (OldSelectedRows = r) then
            Rr.Top := y;
        end
        else
        begin
          if (OldSelectedRows = r) then
            Rr.Bottom := y;

          if (FSelectedRows = r) then
            Rr.Top := y;
        end;

      end;

    end
    else
    begin
      y := FTopMargin;

      Rr.Left := FLeftMargin;
      for c := 1 to ColCount do
      begin
        x := x + FCellSize + FCellSpace;
      end;
      Rr.Right := x;

      for r := 1 to RowCount do
      begin
        y := y + FCellSize + FCellSpace;

        if OldSelectedRows > FSelectedRows then
        begin
          if (FSelectedRows = r) then
            Rr.Top := y;

          if (OldSelectedRows = r) then
            Rr.Bottom := y;
        end
        else
        begin
          if (OldSelectedRows = r) then
            Rr.Top := y;

          if (FSelectedRows = r) then
            Rr.Bottom := y;
        end;

      end;
    end;

    //y:= y + FTopMargin - FCellSpace;
  end;

  RgnT := CreateRectRgn(Rc.Left, Rc.Top, Rc.Right, Rc.Bottom);
  RgnC := CreateRectRgn(Rc.Left, Rc.Top, Rc.Right, Rc.Bottom);
  RgnR := CreateRectRgn(Rr.Left, Rr.Top, Rr.Right, Rr.Bottom);
  CombineRgn(RgnT, RgnC, RgnR, RGN_OR);

  if TSelectorDropDownWindow(FOwner).ShowAbove then
    RgnL := CreateRectRgn(FLeftMargin, 1, Width - FLeftMargin, FLabelHeight)
  else
    RgnL := CreateRectRgn(FLeftMargin, Height - FLabelHeight, Width - FLeftMargin, Height - FTopMargin);
  CombineRgn(RgnT, RgnT, RgnL, RGN_OR);
  InvalidateRgn(Handle, RgnT, true);
  //SelectClipRgn(Canvas.Handle,trgn);
  //DrawGradient();
  //SelectClipRgn(Canvas.Handle,0);

  DeleteObject(RgnC);
  DeleteObject(RgnR);
  DeleteObject(RgnL);
  DeleteObject(RgnT);
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothTableSelectorPanel.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  FSelectedCols := -1;
  FSelectedRows := -1;
  invalidate;
end;

//------------------------------------------------------------------------------

{ TAdvSmoothCustomTableSelector }

constructor TAdvSmoothCustomTableSelector.Create(AOwner: TComponent);
begin
  inherited;
  FDefaultColCount := 5;
  FDefaultRowCount := 4;
  FSelectedColumns := 0;
  FSelectedRows := 0;
  FTextTable := 'Table';
  FTextCancel := 'Cancel';
end;

//------------------------------------------------------------------------------

destructor TAdvSmoothCustomTableSelector.Destroy;
begin
  if not (csDesigning in ComponentState) then
  begin
    if Assigned(FTableSelectorPanel) then
      FTableSelectorPanel.Free;
  end;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCustomTableSelector.Loaded;
begin
  inherited;

end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCustomTableSelector.MouseMove(Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  if (ssLeft in Shift) and Assigned(FDropDownWindow) and (FDropDownWindow.Visible) then
  begin
    if FDropDownWindow.ShowAbove then
    begin
      if (Y < 3) then
      begin
        SendMessage(Handle, WM_LBUTTONUP, X, Y);
        SendMessage(FTableSelectorPanel.Handle, WM_LBUTTONDOWN, X, Y);
        FMouseInControl := false;
      end;
    end
    else
    begin
      if Y > Height - 3 then
      begin
        SendMessage(Handle, WM_LBUTTONUP, X, Y);
        SendMessage(FTableSelectorPanel.Handle, WM_LBUTTONDOWN, X, Y);
        FMouseInControl := false;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCustomTableSelector.SetDefaultColCount(const Value: integer);
begin
  if (Value > 0) and (Value <= 60) then
    FDefaultColCount := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCustomTableSelector.SetDefaultRowCount(const Value: integer);
begin
  if (Value > 0) and (Value <= 60) then
    FDefaultRowCount := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCustomTableSelector.SetSelectorPanel;
begin
  if not Assigned(FTableSelectorPanel) and Assigned(FDropDownWindow) then
  begin
    FTableSelectorPanel := TAdvSmoothTableSelectorPanel.Create(FDropDownWindow);
    FTableSelectorPanel.Parent := FDropDownWindow;
      //FTableSelectorPanel.WindowBorderColor := FBorderDropDownColor;
    FTableSelectorPanel.OnShouldHide := OnDropDownPanelShouldHide;
    FTableSelectorPanel.OnSelect := TableSelectorOnSelect;
  end;
  FDropDownWindow.SelectorPanel := FTableSelectorPanel;

  //FTableSelectorPanel.SelectionColor := FSelectedColor;
  FTableSelectorPanel.WindowBorderColor := FBorderDropDownColor;
  FTableSelectorPanel.ColCount := DefaultColCount;
  FTableSelectorPanel.RowCount := DefaultRowCount;
  FTableSelectorPanel.SelectedCols := 0;
  FTableSelectorPanel.SelectedRows := 0;
  FTableSelectorPanel.TextTable := FTextTable;
  FTableSelectorPanel.TextCancel := FTextCancel;

  FTableSelectorPanel.SetItemsPosition;

  FTableSelectorPanel.Left := 0;
  FTableSelectorPanel.Top := 0;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCustomTableSelector.TableSelectorOnSelect(Sender: TObject);
begin
  FSelectedColumns := FTableSelectorPanel.SelectedCols;
  FSelectedRows := FTableSelectorPanel.SelectedRows;

  if Assigned(FOnSelect) then
    FOnSelect(self);
end;

//------------------------------------------------------------------------------

{ TAdvSmoothCharacterSelector }

constructor TAdvSmoothCharacterSelector.Create(AOwner: TComponent);
begin
  inherited;
  FCharacters := '';
  FSelectedChar := #0;
  ButtonsPerRow := 10;

  Characters := 'abcdefghijklmnopqrstuvwxyz';

  FCharFont := TFont.Create;
  FAutoLoad := false;
end;

//------------------------------------------------------------------------------

destructor TAdvSmoothCharacterSelector.Destroy;
begin
  FCharFont.Free;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCharacterSelector.AddItemsFromChars;
var
  i: integer;
begin
  Tools.Clear;
  for i := 1 to length(FCharacters) do
  begin
    with Tools.Add do
    begin
      ItemType := itAutoSizeButton;
      Caption := FCharacters[i];
      CaptionAlignment := taCenter;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCharacterSelector.LoadCharFromFont;
var
  i: integer;
begin
  if FAutoLoad then
  begin
    FCharacters := #0;
    for i := 1 to 255 do
      FCharacters := FCharacters + char(i);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCharacterSelector.Loaded;
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCharacterSelector.OnToolSelect;
begin
  if SelectedIndex >= 0 then
    FSelectedChar := Tools.Items[SelectedIndex].Caption[1];
  inherited;
  if Assigned(FOnSelect) then
    FOnSelect(self);
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCharacterSelector.SetAutoLoad(const Value: boolean);
begin
  if FAutoLoad <> Value then
  begin
    FAutoLoad := Value;
    if FAutoLoad then
      LoadCharFromFont
    else
      FCharacters := '';
    AddItemsFromChars;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCharacterSelector.SetCharacters(const Value: string);
begin
  if AutoLoad then
    raise exception.Create('Can not change characters when AutoLoad is true.');
  if FCharacters <> Value then
  begin
    FCharacters := Value;
    AddItemsFromChars;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCharacterSelector.SetCharFont(const Value: TFont);
begin
  FCharFont.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCharacterSelector.SetSelectedChar(const Value: char);
begin
  FSelectedChar := Value;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCharacterSelector.SetSelectorPanel;
//var
  //i, j: integer;
begin
{
  LoadCharFromFont;
  Tools.Clear;
  j:= -1;
  for i:= 1 to length(FCharacters) do
  begin
    with Tools.Add do
    begin
      ItemType := itAutoSizeButton;
      Caption:= FCharacters[i];
      CaptionAlignment:= taCenter;
    end;
    if FSelectedChar = FCharacters[i] then
      j:= i - 1;
  end;
}
  inherited;
  FSelectorPanel.ButtonMargin := 0;
{  if j >= 0 then
    SelectedIndex:= j;
}
  //FSelectorPanel.Font.Name:= FCharFont.Name;
  FSelectorPanel.NoPrefix := true;
  FSelectorPanel.Canvas.Font.Assign(FCharFont);
  FSelectorPanel.Font.Assign(FCharFont);
  FSelectorPanel.SetItemsPosition;
end;

//------------------------------------------------------------------------------

procedure TAdvSmoothCharacterSelector.DrawGlyphAndCaption(aGlyph: TBitmap;
  R: TRect);
var
  CapR: TRect;
  ar: array[0..1] of char;
  DTSTYLE: dword;
begin
  CapR := R;



  DTSTYLE := DT_SINGLELINE or DT_VCENTER or DT_NOPREFIX;
  if Style = ssButton then
  begin
    if not aGlyph.Empty then
    begin
      CapR.Left := DrawGlyph(aGlyph, R);
    end
    else
    begin
      if (Caption = '') and (SelectedChar <> #0) then
      begin
        Canvas.Font.Assign(CharFont);
        {
        Canvas.Font.Name := CharFont.Name;
        Canvas.Font.Size := CharFont.Size;
        Canvas.Font.Color := CharFont.Color;
        }
        Canvas.Brush.Style := bsClear;
        ar[0] := SelectedChar;
        ar[1] := #0;
{$IFNDEF TMSDOTNET}
        DrawText(Canvas.Handle, ar, -1, CapR, DTSTYLE or DT_CENTER);
{$ENDIF}
{$IFDEF TMSDOTNET}
        DrawText(Canvas.Handle, SelectedChar, -1, CapR, DTSTYLE);
{$ENDIF}
      end;
    end;
    if Caption <> '' then
{$IFNDEF TMSDOTNET}
      DrawText(Canvas.Handle, PChar(Caption), -1, CapR, DTSTYLE);
{$ENDIF}
{$IFDEF TMSDOTNET}
    DrawText(Canvas.Handle, Caption, -1, CapR, DTSTYLE);
{$ENDIF}
  end
  else if Style = ssCombo then
  begin
    if (SelectedChar <> #0) and (Caption = '') then
    begin
      Canvas.Font.Name := CharFont.Name;
      Canvas.Font.Size := CharFont.Size;
      Canvas.Brush.Style := bsClear;
      ar[0] := SelectedChar;
      ar[1] := #0;
{$IFNDEF TMSDOTNET}
      DrawText(Canvas.Handle, ar, -1, CapR, DTSTYLE or DT_CENTER);
{$ENDIF}
{$IFDEF TMSDOTNET}
      DrawText(Canvas.Handle, SelectedChar, -1, CapR, DTSTYLE);
{$ENDIF}
    end;
  end;
end;

initialization

{$IFDEF FREEWARE}
if (FindWindow('TApplication', nil) = 0) or
{  (FindWindow('TAlignPalette', nil) = 0) or
  (FindWindow('TPropertyInspector', nil) = 0) or}
  (FindWindow('TAppBuilder', nil) = 0) then
begin
  ShowMessage('TMS Graphics Component Pack trial version');
end
{$ENDIF}

end.
