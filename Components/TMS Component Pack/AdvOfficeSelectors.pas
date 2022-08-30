{***************************************************************************}
{ TAdvOfficeSelectors components                                            }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2006 - 2015                                        }
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

unit AdvOfficeSelectors;

interface

uses
  Classes, Windows, Types, Graphics, Controls, Messages, ExtCtrls, SysUtils, ImgList, Forms,
  Math, Dialogs, AdvGlowButton, GDIPicture, AdvGDIP, ComCtrls, AdvStyleIF, CommCtrl
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;

const
  MINBUTTONSIZE = 16;
  TABLECELLSIZE = 24;
  DRAGGRIP_HEIGHT = 8;
  CAPTION_HEIGHT = 20;
  SCROLLER_WIDTH = 16;
  DROPDOWN_KEY = VK_F4;

  MAJ_VER = 1; // Major version nr.
  MIN_VER = 6; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 1; // Build nr.

  clMoreColors = TColor($FFFFFFFE);
  // version history
  // v1.1.0.0  : New AdvOfficeScrollSelector added
  //           : Improved rendering of fonts with only bold styles in font selector
  // v1.1.0.1  : Fixed : issue with OnDropDown event
  // v1.1.0.2  : Fixed : issue with closing selector panel & drawing update
  // v1.1.0.3  : Fixed : issue with More Colors selection from TAdvOfficeColorSelector
  // v1.1.0.4  : Fixed : painting issue with TAdvOfficeScrollSelector
  // v1.1.1.0  : Improved : exposed SelectedIndex as public property
  //           : Improved : exposed Layout & DropDownSplit properties
  // v1.1.2.0  : New : property SelectedIndex made public
  //           : New : property DropDownSplit and Layout published
  //           : Improved : TAdvCustomOfficeSelector.MouseDown modifed to check DropDownSplit
  // v1.1.2.1  : Fixed : issue with runtime creation of components
  // v1.2.0.0  : New : Exposed OnMouseDown, OnMouseUp, OnMouseMove events
  //           : Fixed : issue with scrolling in the office selector
  // v1.2.1.0  : New : exposed property DropDownPosition
  // v1.2.2.0  : New : property DropDownHeight added to TAdvOfficeScrollSelector
  // v1.3.0.0  : New : Windows Vista, Windows 7 & Terminal styles
  // v1.3.0.1  : Fixed : Issue with reparented parent forms
  // v1.3.1.0  : Improved : Keyboard support
  // v1.3.1.1  : Fixed : Issue with getting Owner of AdvScrollSelectorItems collection
  // v1.4.0.0  : New : Office 2010 color style support added
  // v1.4.0.1  : Fixed : Issue with keeping button displayed down during selection
  // v1.4.0.2  : Fixed : Issue with down state when closing selector with ESC, Unactivate
  //           : Improved : Blocked hints on other controls while selector displays
  // v1.4.0.3  : Fixed : Issue with clFuchsia in TAdvOfficeColorSelector
  // v1.4.1.0  : New : ColorDialog property added to allow to set a common color picker dialog for the selector
  // v1.4.1.1  : Fixed : Drawing issue of selector when DropDownButton = true
  // v1.4.2.0  : New : Exposed SelectionColor for TAdvOfficeTableSelector
  // v1.4.3.0  : New : DroppedDown public property added
  // v1.4.3.1  : Fixed : Issue with stack overflow on drop down
  // v1.4.3.2  : Fixed : Color picker only shows on left-click
  // v1.4.4.0  : New : Event OnCanDropDown added
  //           : Fixed : Issue with dropdown click when DropDownPosition = dpBottom
  //           : Fixed : Handling of up/down key in dropdown to select last item
  // v1.4.5.0  : New : TAdvOfficeScrollSelector.BeginUpdate / TAdvOfficeScrollSelector.EndUpdate added
  // v1.4.6.0  : New : Method BlockDropDown() added
  // v1.4.6.1  : Fixed : Issue with use on multimonitor config
  // v1.4.6.2  : Fixed : Issue with painting for disabled items
  // v1.4.7.0  : New : Added support for 32bit bitmaps with alpha transparency (Delphi XE2 or later)
  // v1.4.7.1  : Fixed : Issue with dbl click on TAdvOfficeColorSelector
  // v1.4.7.2  : Fixed : Persistence of custom colors in custom color dialog
  // v1.4.7.3  : Fixed : Issue with selecting table size with mouse
  // v1.5.0.0  : New : Picture property added in TAdvSelectorItem
  // v1.5.1.0  : New : Property DropDownCheck added
  //           : Improved : When ShowCaption = true, selected item caption is displayed on button when tool has caption
  // v1.5.2.0  : New : AutoUpdateCaption public property added to TAdvOfficeToolSelector
  // v1.5.3.0  : Improved : Action support
  // v1.6.0.0  : New : Windows 10, Office 2016 styles added
  // v1.6.0.1  : Fixed : Issue with text display in TAdvOfficeToolSelector


type
  TAdvSelectorStyle = (ssButton, ssCombo);
  TGradientDirection = (gdVertical, gdHorizontal);
  TSelectorItemType = (itAutoSizeButton, itFullWidthButton, itCaption);
  TColorSelectionStyle = (csDiscrete, csColorCube, csSpectrum);
  TGripPosition = (gpTop, gpBottom);
  TNoOfButtons = 1..16;

  TAdvSelectorPanel = class;
  TSelectorDropDownWindow = class;
  TAdvCustomOfficeScrollSelector = class;
  TAdvScrollSelectorPanel = class;

  TSelectorButtonSize = class(TPersistent)
  private
    FOnChange: TNotifyEvent;
    FHeight: Integer;
    FWidth: Integer;
    procedure Changed;
    procedure SetHeight(const Value: Integer);
    procedure SetWidth(const Value: Integer);
  protected
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property Height: Integer read FHeight write SetHeight;
    property Width: Integer read FWidth write SetWidth;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TGradientBackground = class(TPersistent)
  private
    FSteps: Integer;
    FColor: TColor;
    FColorTo: TColor;
    FDirection: TGradientDirection;
    FOnChange: TNotifyEvent;
    FBorderColor: TColor;
    procedure SetColor(const Value: TColor);
    procedure SetColorTo(const Value: TColor);
    procedure SetDirection(const Value: TGradientDirection);
    procedure SetSteps(const Value: Integer);
    procedure Changed;
    procedure SetBorderColor(const Value: TColor);
  protected
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property BorderColor: TColor read FBorderColor write SetBorderColor;
    property Color: TColor read FColor write SetColor;
    property ColorTo: TColor read FColorTo write SetColorTo;
    property Direction: TGradientDirection read FDirection write SetDirection;
    property Steps: Integer read FSteps write SetSteps default 64;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TGradientCaption = class(TGradientBackground)
  private
    FTextColor: TColor;
    FTextColorDown: TColor;
    FTextColorHot: TColor;
    FButtonAppearance: TGlowButtonAppearance;
    procedure SetButtonAppearance(const Value: TGlowButtonAppearance);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property TextColor: TColor read FTextColor write FTextColor;
    property TextColorHot: TColor read FTextColorHot write FTextColorHot;
    property TextColorDown: TColor read FTextColorDown write FTextColorDown;
    property ButtonAppearance: TGlowButtonAppearance read FButtonAppearance write SetButtonAppearance;
  end;

  TSimpleGradientCaption = class(TGradientBackground)
  private
    FTextColor: TColor;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property TextColor: TColor read FTextColor write FTextColor;
  end;

  TVistaBackground = class(TPersistent)
  private
    FSteps: Integer;
    FColor: TColor;
    FColorTo: TColor;
    FOnChange: TNotifyEvent;
    FColorMirror: TColor;
    FColorMirrorTo: TColor;
    FBorderColor: TColor;
    FGradientMirror: TGDIPGradient;
    FGradient: TGDIPGradient;
    procedure SetColor(const Value: TColor);
    procedure SetColorTo(const Value: TColor);
    procedure SetSteps(const Value: Integer);
    procedure Changed;
    procedure SetBorderColor(const Value: TColor);
    procedure SetColorMirror(const Value: TColor);
    procedure SetColorMirrorTo(const Value: TColor);
    procedure SetGradient(const Value: TGDIPGradient);
    procedure SetGradientMirror(const Value: TGDIPGradient);
  protected
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property BorderColor: TColor read FBorderColor write SetBorderColor;
    property Color: TColor read FColor write SetColor;
    property ColorTo: TColor read FColorTo write SetColorTo;
    property ColorMirror: TColor read FColorMirror write SetColorMirror;
    property ColorMirrorTo: TColor read FColorMirrorTo write SetColorMirrorTo;
    property Gradient: TGDIPGradient read FGradient write SetGradient;
    property GradientMirror: TGDIPGradient read FGradientMirror write SetGradientMirror;
    property Steps: Integer read FSteps write SetSteps default 64;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TVistaBackgroundHot = class(TVistaBackground)
  private
    FColorHot: TColor;
    FColorHotTo: TColor;
    FColorMirrorHot: TColor;
    FColorMirrorHotTo: TColor;
    FBorderColorHot: TColor;
    FGradientHot: TGDIPGradient;
    FGradientMirrorHot: TGDIPGradient;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property BorderColorHot: TColor read FBorderColorHot write FBorderColorHot;
    property ColorHot: TColor read FColorHot write FColorHot;
    property ColorHotTo: TColor read FColorHotTo write FColorHotTo;
    property ColorMirrorHot: TColor read FColorMirrorHot write FColorMirrorHot;
    property ColorMirrorHotTo: TColor read FColorMirrorHotTo write FColorMirrorHotTo;
    property GradientHot: TGDIPGradient read FGradientHot write FGradientHot;
    property GradientMirrorHot: TGDIPGradient read FGradientMirrorHot write FGradientMirrorHot;
  end;

  TSelectionAppearance = class(TGlowButtonAppearance)
  private
    FTextColor: TColor;
    FTextColorDisabled: TColor;
    FTextColorDown: TColor;
    FTextColorHot: TColor;
    FTextColorChecked: TColor;
    FRounded: Boolean;
    procedure SetTextColor(const Value: TColor);
    procedure SetTextColorChecked(const Value: TColor);
    procedure SetTextColorDisabled(const Value: TColor);
    procedure SetTextColorDown(const Value: TColor);
    procedure SetTextColorHot(const Value: TColor);
    procedure SetRounded(const Value: Boolean);
  protected
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property TextColor: TColor read FTextColor write SetTextColor;
    property TextColorHot: TColor read FTextColorHot write SetTextColorHot;
    property TextColorDown: TColor read FTextColorDown write SetTextColorDown;
    property TextColorChecked: TColor read FTextColorChecked write SetTextColorChecked;
    property TextColorDisabled: TColor read FTextColorDisabled write SetTextColorDisabled;
    property Rounded: Boolean read FRounded write SetRounded;
  end;

  TAdvSelectorItem = class(TCollectionItem)
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
    FPicture: TGDIPPicture;
    procedure SetEnable(const Value: boolean);
    procedure SetHint(const Value: string);
    procedure SetImageIndex(const Value: integer);
    procedure SetValue(const Value: string);
    procedure SetvCaptionAlignment(const Value: TAlignment);
    procedure SetItemType(const Value: TSelectorItemType);
    procedure SetBackGroundColor(const Value: TColor);
    procedure SetTag(const Value: integer);
    procedure SetPicture(const Value: TGDIPPicture);
  protected
    procedure SetCaption(const Value: string); virtual;
    property ItemRect: TRect read FItemRect write FItemRect;
    procedure Changed; virtual;
    //property Enable: boolean read FEnable write SetEnable;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property BackGroundColor: TColor read FBackGroundColor write SetBackGroundColor default clNone;
    property Caption: string read FCaption write SetCaption;
    property CaptionAlignment: TAlignment read FCaptionAlignment write SetvCaptionAlignment;
    property Enable: boolean read FEnable write SetEnable;
    property Hint: string read FHint write SetHint;
    property ImageIndex: integer read FImageIndex write SetImageIndex;
    property ItemType: TSelectorItemType read FItemType write SetItemType default itAutoSizeButton;
    property Picture: TGDIPPicture read FPicture write SetPicture;
    property Tag: integer read FTag write SetTag default 0;
    property Value: string read FValue write SetValue;
    //property MultiSelect: boolean
  end;

  TAdvSelectorItems = class(TCollection)
  private
    FOwner: TPersistent;
    function GetItem(Index: Integer): TAdvSelectorItem;
    procedure SetItem(Index: Integer; const Value: TAdvSelectorItem);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TPersistent);
    function Add: TAdvSelectorItem;
    function Insert(Index: Integer): TAdvSelectorItem;
    property Items[Index: Integer]: TAdvSelectorItem read GetItem write SetItem; default;
  end;

  TChangeSelectionEvent = procedure(OldItemIndex, NewItemIndex: integer) of object;
  THotToolEvent = procedure(Sender: TObject; HotItemIndex: integer) of object;

  TAdvCustomSelectorPanel = class(TCustomPanel)
  private
    FOwner: TComponent;
    FColorTo: TColor;
    FGradientDirection: TGradientDirection;
    FWindowBorderColor: TColor;
    FOnShouldHide: TNotifyEvent;
    FAllowFloating: Boolean;
    FDragGripAppearance: TVistaBackgroundHot;
    FGripPosition: TGripPosition;
    FDragGripHot: Boolean;
    FFloating: Boolean;
    FCaptionAppearance: TGradientCaption;
    FOnFloating: TNotifyEvent;
    FCloseBtnHot: Boolean;
    FCloseBtnDown: Boolean;
    FColorFloatingTo: TColor;
    FColorFloating: TColor;
    FOnCloseBtnClick: TNotifyEvent;
    FDropDownWindow: TSelectorDropDownWindow;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure SetColorTo(const Value: TColor);
    procedure SetTGradientDirection(const Value: TGradientDirection);
    procedure SetWindowBorderColor(const Value: TColor);
    procedure SetDragGripAppearance(const Value: TVistaBackgroundHot);
    procedure SetCaptionAppearance(const Value: TGradientCaption);
  protected
    //FFloating: Boolean;
    procedure DrawCloseBtn;
    procedure Paint; override;
    procedure ResetDown; virtual;
    procedure HandleKey(Key: word); virtual;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Initialize;
    function GetVisibleHeight: integer; virtual;
    procedure SetFloating(const Value: Boolean); virtual;
    property Floating: Boolean read FFloating write SetFloating;

    function GetDragGripRect: TRect;
    function GetCaptionRect: TRect;
    function GetCloseBtnRect: TRect;
    procedure InvalidateCloseBtn;
    procedure InvalidateDragGrip;
    procedure CloseBtnClick;

    property DropDownWindow: TSelectorDropDownWindow read FDropDownWindow write FDropDownWindow;
    property OnShouldHide: TNotifyEvent read FOnShouldHide write FOnShouldHide;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property ColorTo: TColor read FColorTo write SetColorTo default clNone;
    property ColorFloating: TColor read FColorFloating write FColorFloating;
    property ColorFloatingTo: TColor read FColorFloatingTo write FColorFloatingTo;
    property WindowBorderColor: TColor read FWindowBorderColor write SetWindowBorderColor default clGray;
    property GradientDirection: TGradientDirection read FGradientDirection write SetTGradientDirection;
    property AllowFloating: Boolean read FAllowFloating write FAllowFloating;
    property GripPosition: TGripPosition read FGripPosition write FGripPosition;
    property DragGripAppearance: TVistaBackgroundHot read FDragGripAppearance write SetDragGripAppearance;
    property CaptionAppearance: TGradientCaption read FCaptionAppearance write SetCaptionAppearance;
    property OnFloating: TNotifyEvent read FOnFloating write FOnFloating;
    property OnCloseBtnClick: TNotifyEvent read FOnCloseBtnClick write FOnCloseBtnClick;
  end;

  TItemDrawEvent = procedure(Sender: TObject; Index: integer; R: TRect) of object;

  TSelectEvent = procedure(Sender: TObject; Index: Integer; Item: TAdvSelectorItem) of object;

  TColorSelectEvent = procedure(Sender: TObject; AColor: TColor) of object;
  TPenStyleSelectEvent = procedure(Sender: TObject; AStyle: TPenStyle) of object;
  TBrushStyleSelectEvent = procedure(Sender: TObject; AStyle: TBrushStyle) of object;
  TCharSelectEvent = procedure(Sender: TObject; AChar: Char) of object;
  TTableSizeSelectEvent = procedure(Sender: TObject; Columns, Rows: integer) of object;

  TDropDownEvent = procedure(Sender: TObject; var Allow: boolean) of object;

  TAdvSelectorPanel = class(TAdvCustomSelectorPanel)
  private
    FItemIndex: integer;
    FItems: TAdvSelectorItems;
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
    FSelectionAppearance: TSelectionAppearance;
    procedure WMChar(var Msg: TWMKey); message WM_CHAR;
    procedure CMWantSpecialKey(var Msg: TCMWantSpecialKey); message CM_WANTSPECIALKEY;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure SetBorderColor(const Value: TColor);
    procedure SetItemIndex(const Value: integer);
    procedure SetItems(const Value: TAdvSelectorItems);
    procedure SetImages(const Value: TCustomImageList);
    procedure SetButtonsPerRow(const Value: TNoOfButtons);
    procedure SetButtonMargin(const Value: integer);
    procedure SetMinButtonHeight(const Value: integer);
    procedure SetMinButtonWidth(const Value: integer);
    procedure SetTwoColorImages(const Value: Boolean);
    procedure SetSelectionAppearance(const Value: TSelectionAppearance);
  protected
    procedure HandleKey(Key: word); override;
    procedure DrawItem(Index: integer; RefreshItem: boolean = false);
    procedure Paint; override;
    procedure ResetDown; override;
    procedure SetItemsPosition;
    function GetMaxWidth: integer;
    function TotalAutoSizeButtons: integer;
    procedure SetPanelHeight;
    procedure AutoSizeBtnSize(var W, H: integer);
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure SetFloating(const Value: Boolean); override;
    property MinButtonWidth: integer read FMinButtonWidth write SetMinButtonWidth default MINBUTTONSIZE;
    property MinButtonHeight: integer read FMinButtonHeight write SetMinButtonHeight default MINBUTTONSIZE;
    property NoPrefix: Boolean read FNoPrefix write FNoPrefix;

    property TwoColorImages: Boolean read FTwoColorImages write SetTwoColorImages;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ItemAtPos(X, Y: integer): integer;
    property Items: TAdvSelectorItems read FItems write SetItems;
    property ItemIndex: integer read FItemIndex write SetItemIndex;
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

    property SelectionAppearance: TSelectionAppearance read FSelectionAppearance write SetSelectionAppearance;

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

  TAdvColorCubePanel = class(TAdvCustomSelectorPanel)
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

  TAdvColorSpectrumPanel = class(TAdvCustomSelectorPanel)
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

  TAdvTableSelectorPanel = class(TAdvCustomSelectorPanel)
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
    procedure HandleKey(Key: word); override;
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
    property SelectionColor: TColor read FSelectionColor write FSelectionColor default clHighlight;
    property TextTable: string read FTextTable write FTextTable;
    property TextCancel: string read FTextCancel write FTextCancel;
    property OnSelect: TNotifyEvent read FOnSelect write FOnSelect;
  end;


  TSelectorDropDownWindow = class(TCustomForm)
  private
    FSelectorPanel: TAdvCustomSelectorPanel;
    FHideOnDeActivate: Boolean;
    FShowAbove: Boolean;
    FOwner: TComponent;
    FShowFullBorder: Boolean;
    FHideTimer: TTimer;
    FShowLeft: Boolean;
    FBlockHide: boolean;
    procedure WMActivate(var Message: TWMActivate); message WM_ACTIVATE;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMWindowPosChanging(var Message: TWMWindowPosChanging); message WM_WINDOWPOSCHANGING;
    procedure HideTimerOnTime(Sender: TObject);
    procedure SetSelectorPanel(const Value: TAdvCustomSelectorPanel);
  protected
    procedure Paint; override;
    function GetParentWnd: HWnd;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    property HideOnDeActivate: Boolean read FHideOnDeActivate write FHideOnDeActivate;
    property ShowAbove: Boolean read FShowAbove write FShowAbove;
    property ShowLeft: Boolean read FShowLeft write FShowLeft default false;
    property ShowFullBorder: Boolean read FShowFullBorder write FShowFullBorder;
    property BlockHide: boolean read FBlockHide write FBlockHide;
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateNew(AOwner: TComponent; Dummy: Integer = 0); override;
    destructor Destroy; override;
    procedure SetWindowSize;
  published
    property SelectorPanel: TAdvCustomSelectorPanel read FSelectorPanel write SetSelectorPanel;
  end;

  TDrawToolEvent = procedure(Sender: TObject; Canvas: TCanvas; ItemIndex: integer; R: TRect) of object;

  TAdvCustomOfficeSelector = class(TAdvCustomGlowButton, ITMSStyle)
  private
    FAppHintPause: integer;
    FDropDownWindow: TSelectorDropDownWindow;
    FSelectorPanel: TAdvSelectorPanel;
    FMouseInControl: Boolean;
    FMouseDown: Boolean;
    FDropDownBtnWidth: integer;
    FFlat: Boolean;
    FStyle: TAdvSelectorStyle;
    FToolImages: TCustomImageList;
    FOnDropDown: TNotifyEvent;
    FOnCanDropDown: TDropDownEvent;
    //FAppearanceStyle: TAdvAppearanceStyle;
    //FState: TAdvButtonState;
    FColorDropDownTo: TColor;
    FColorDropDown: TColor;
    FBorderDropDownColor: TColor;
    FDropDownCount: integer;
    FGradientDirection: TGradientDirection;
    FSelectedIndex: integer;
    FTools: TAdvSelectorItems;
    FButtonsPerRow: TNoOfButtons;
    FOnHotTool: THotToolEvent;
    FOnClick: TNotifyEvent;
    FOnSelect: TSelectEvent;
    FOnDrawTool: TDrawToolEvent;
    FDupSelectedIndex: integer;
    FTwoColorImages: Boolean;
    FBackGroundImageColor: TColor;
    FForeGroundImageColor: TColor;
    FOldForeGroundImgColor: TColor;
    FOldBkGroundImgColor: TColor;
    FStretchImageDraw: Boolean;
    FSelectionAppearance: TSelectionAppearance;
    FAllowFloating: Boolean;
    FDragGripAppearance: TVistaBackgroundHot;
    FCloseOnSelect: Boolean;
    FGripPosition: TGripPosition;
    FCaptionAppearance: TGradientCaption;
    FColorDropDownFloatingTo: TColor;
    FColorDropDownFloating: TColor;
    FOnDropDownFloat: TNotifyEvent;
    FOnDropDownClose: TNotifyEvent;
    FDropDownTopMost: Boolean;
    FDropDownCheck: Boolean;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure OnDropDownWindowHide(Sender: TObject);
    procedure OnDropDownPanelShouldHide(Sender: TObject);
    procedure OnDropDownPanelSelect(Sender: TObject);
    procedure OnDropDownPanelHotTool(Sender: TObject; HotItemIndex: integer);
    procedure OnDropDownPanelDrawTool(Sender: TObject; ItemIndex: integer; R: TRect);
    procedure OnDropDownPanelFloating(Sender: TObject);
    procedure OnDropDownPanelClose(Sender: TObject);
    procedure OnSelectionAppearanceChanged(Sender: TObject);
    procedure PopupBtnDown;
    procedure ButtonDown;
    procedure SetFlat(const Value: Boolean);
    procedure SetToolImages(const Value: TCustomImageList);
    procedure SetStyleEx(const Value: TAdvSelectorStyle);
    //procedure SetAppearanceStyle(const Value: TAdvAppearanceStyle);
    procedure SetColorDropDown(const Value: TColor);
    procedure SetColorDropDownTo(const Value: TColor);
    procedure SetDropDownCount(const Value: integer);
    procedure SetTGradientDirection(const Value: TGradientDirection);
    //function GetCaption: string;
    //procedure SetCaption(const Value: string);
    procedure SetSelectedIndex(const Value: integer);
    procedure SetTools(const Value: TAdvSelectorItems);
    function GetSelectedIndex: integer;
    procedure SetButtonsPerRow(const Value: TNoOfButtons);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    procedure SetTwoColorImages(const Value: Boolean);
    procedure SetForeGroundImageColor(const Value: TColor);
    procedure SetStretchImageDraw(const Value: Boolean);
    procedure SetSelectionAppearance(const Value: TSelectionAppearance);
    procedure SetDragGripAppearance(const Value: TVistaBackgroundHot);
    procedure SetCaptionAppearance(const Value: TGradientCaption);
    procedure SetDroppedDown(const Value: boolean);
    function GetDroppedDown: boolean;
    procedure SetDropDownCheck(const Value: Boolean);
  protected
    procedure OnToolSelect; virtual;
    procedure GetToolImage(bmp: TBitmap); override;
    procedure GetToolPicture(var pic: TGDIPPicture); override;
    procedure CopyPicture(Pic: TGDIPPicture; bmp: TBitmap);
    function DrawGlyph(aGlyph: TBitMap; aRect: TRect): integer;
    procedure DrawGlyphAndCaption(Pic: TGDIPPicture; R: TRect); virtual;
    procedure DrawComboButton;
    function GetBtnRect: TRect;
    procedure SetSelectorPanelItems;
    procedure SetSelectorPanel; virtual;
    procedure DoDropDown; virtual;
    function DoCanDropDown: boolean; virtual;
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;

    procedure DoSelect(Index: integer; Item: TAdvSelectorItem); virtual;
    procedure DropDownWindowClose(Sender: TObject; var Action: TCloseAction); virtual;
    procedure Paint; override;
    property MouseInControl: Boolean read FMouseInControl;
    procedure WndProc(var Message: TMessage); override;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure ThemeAdapt;
    function GetVersionNr: Integer; override; //virtual;

    procedure ChangeImagesColor(ForeGColor, BkGColor: TColor);
    property TwoColorImages: Boolean read FTwoColorImages write SetTwoColorImages;
    property OldForeGroundImgColor: TColor read FOldForeGroundImgColor;
    property OldBkGroundImgColor: TColor read FOldBkGroundImgColor;
    property ForeGroundImageColor: TColor read FForeGroundImageColor write SetForeGroundImageColor;
    property BackGroundImageColor: TColor read FBackGroundImageColor write FBackGroundImageColor;

    property StretchImageDraw: Boolean read FStretchImageDraw write SetStretchImageDraw default True;

    property ButtonsPerRow: TNoOfButtons read FButtonsPerRow write SetButtonsPerRow default 1;
    property BorderDropDownColor: TColor read FBorderDropDownColor write FBorderDropDownColor default clGray;
    //property Caption: string read GetCaption write SetCaption;
    property ColorDropDown: TColor read FColorDropDown write SetColorDropDown;
    property ColorDropDownTo: TColor read FColorDropDownTo write SetColorDropDownTo default clNone;
    property ColorDropDownFloating: TColor read FColorDropDownFloating write FColorDropDownFloating;
    property ColorDropDownFloatingTo: TColor read FColorDropDownFloatingTo write FColorDropDownFloatingTo default clNone;
    property SelectionAppearance: TSelectionAppearance read FSelectionAppearance write SetSelectionAppearance;
    property DragGripAppearance: TVistaBackgroundHot read FDragGripAppearance write SetDragGripAppearance;
    property CaptionAppearance: TGradientCaption read FCaptionAppearance write SetCaptionAppearance;
    property DropDownCount: integer read FDropDownCount write SetDropDownCount;
    property Flat: Boolean read FFlat write SetFlat default True;
    property GradientDirection: TGradientDirection read FGradientDirection write SetTGradientDirection;
    property ToolImages: TCustomImageList read FToolImages write SetToolImages;
    property Style: TAdvSelectorStyle read FStyle write SetStyleEx;
    property Tools: TAdvSelectorItems read FTools write SetTools;
    property AllowFloating: Boolean read FAllowFloating write FAllowFloating;
    property CloseOnSelect: Boolean read FCloseOnSelect write FCloseOnSelect;
    property DragGripPosition: TGripPosition read FGripPosition write FGripPosition;
    property DropDownTopMost: Boolean read FDropDownTopMost write FDropDownTopMost default True;
    property DropDownCheck: Boolean read FDropDownCheck write SetDropDownCheck default False;
    property OnCanDropDown: TDropDownEvent read FOnCanDropDown write FOnCanDropDown;
    property OnDropDown: TNotifyEvent read FOnDropDown write FOnDropDown;
    property OnDropDownFloat: TNotifyEvent read FOnDropDownFloat write FOnDropDownFloat;
    property OnDropDownClose: TNotifyEvent read FOnDropDownClose write FOnDropDownClose;
    property OnSelect: TSelectEvent read FOnSelect write FOnSelect;
    property OnHotTool: THotToolEvent read FOnHotTool write FOnHotTool;
    property OnDrawTool: TDrawToolEvent read FOnDrawTool write FOnDrawTool;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Click; override;
    function IsDropDown: Boolean; override;
    procedure ShowDropDown;
    procedure HideDropDown;
    procedure BlockDropDown(const Value: boolean);
    procedure SetComponentStyle(AStyle: TTMSStyle); override;
    property SelectedIndex: integer read GetSelectedIndex write SetSelectedIndex;
    property DroppedDown: boolean read GetDroppedDown write SetDroppedDown;
  published
    property Action;
    property TabOrder;
    property TabStop;
    property Version: string read GetVersion write SetVersion;
    property Visible;
    property ShowCaption default false;
    property DropDownSplit;
    property DropDownPosition;
    property Layout;
  end;

  TAdvToolSelectorActionLink = class(TAdvGlowButtonActionLink)
    procedure UpdateSelectedIndex(AIndex: integer); virtual;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvOfficeToolSelector = class(TAdvCustomOfficeSelector)
  private
    FToolWidth: Integer;
    FToolHeight: Integer;
    FAutoUpdateCaption: boolean;
  protected
    procedure DoSelect(Index: integer; Item: TAdvSelectorItem); override;
    procedure SetSelectorPanel; override;
    procedure GetToolImage(bmp: TBitmap); override;
    function GetActionLinkClass: TControlActionLinkClass; override;
  public
    constructor Create(AOwner: TComponent); override;
    property Down;
    property AutoUpdateCaption: boolean read FAutoUpdateCaption write FAutoUpdateCaption;
  published
    property AllowFloating;
    property Appearance;
    property BorderDropDownColor;
    property ButtonsPerRow;
    property Caption;
    property CaptionAppearance;
    property DragGripAppearance;
    property Enabled;
    property CloseOnSelect;
    property ColorDropDown;
    property ColorDropDownTo;
    property ColorDropDownFloating;
    property ColorDropDownFloatingTo;
    property DropDownButton;
    property DropDownCheck;
    property DragGripPosition;
    property ToolImages;
    property ShowHint;
    property SelectedIndex;
    property SelectionAppearance;
    property Tools;
    property OwnerDrawToolWidth: Integer read FToolWidth write FToolWidth default 0;
    property OwnerDrawToolHeight: Integer read FToolHeight write FToolHeight default 0;

    property OnMouseEnter;
    property OnMouseLeave;
    property OnCanDropDown;
    property OnDropDown;
    property OnDropDownFloat;
    property OnDropDownClose;

    property OnClick;
    property OnSelect;
    property OnHotTool;
    property OnDrawTool;
  end;

  TSelectionType = (stOffice, stBorland);

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvOfficePenStyleSelector = class(TAdvCustomOfficeSelector)
  private
    FSelectionType: TSelectionType;
    FOnSelectPenStyle: TPenStyleSelectEvent;    
    procedure Initialize;
    function GetSelectedPenStyle: TPenStyle;
    procedure SetSelectedPenStyle(const Value: TPenStyle);
    procedure SetSelectionType(const Value: TSelectionType);
    function GetPenColor: TColor;
    procedure SetPenColor(const Value: TColor);
  protected
    procedure Loaded; override;
    procedure SetSelectorPanel; override;
    procedure DoSelect(Index: Integer; Item: TAdvSelectorItem); override;
    function GetPenStyleAtIndex(Index: Integer): TPenStyle;
    function GetIndexOfStyle(APenStyle: TPenStyle): Integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property SelectedPenStyle: TPenStyle read GetSelectedPenStyle write SetSelectedPenStyle;
    property Down;
  published
    property AllowFloating;
    property CaptionAppearance;
    property DragGripAppearance;
    property Appearance;
    property BorderDropDownColor;
    property Caption;
    property CloseOnSelect;
    property ColorDropDown;
    property ColorDropDownTo;
    property ColorDropDownFloating;
    property ColorDropDownFloatingTo;

    property DropDownButton;
    property DropDownCheck;
    property DragGripPosition;
    property Enabled;
    property PenColor: TColor read GetPenColor write SetPenColor;
    property SelectedIndex;
    property SelectionType: TSelectionType read FSelectionType write SetSelectionType default stOffice;
    property ShowHint;
    property SelectionAppearance;
    property Tools;

    property OnMouseEnter;
    property OnMouseLeave;
    property OnCanDropDown;
    property OnDropDown;
    property OnDropDownFloat;
    property OnDropDownClose;

    property OnClick;
    property OnSelect;
    property OnSelectPenStyle: TPenStyleSelectEvent read FOnSelectPenStyle write FOnSelectPenStyle;
    property OnHotTool;
    property OnDrawTool;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvOfficePenWidthSelector = class(TAdvCustomOfficeSelector)
  private
    procedure Initialize;
  protected
    procedure SetSelectorPanel; override;
    function GetSelectedPenWidth: Integer; virtual;
    procedure SetSelectedPenWidth(const Value: Integer); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property SelectedPenWidth: Integer read GetSelectedPenWidth write SetSelectedPenWidth;
    property Down;
  published
    property AllowFloating;
    property Appearance;
    property BorderDropDownColor;
    property Caption;
    property CaptionAppearance;
    property DragGripAppearance;
    property DropDownCheck;
    property CloseOnSelect;
    property ColorDropDown;
    property ColorDropDownTo;
    property ColorDropDownFloating;
    property ColorDropDownFloatingTo;

    property DropDownButton;
    property DragGripPosition;
    property Enabled;
    property ShowHint;
    //property Style;

    property SelectedIndex;
    property SelectionAppearance;
    property Tools;

    property OnMouseEnter;
    property OnMouseLeave;
    property OnCanDropDown;
    property OnDropDown;
    property OnDropDownFloat;
    property OnDropDownClose;

    property OnClick;
    property OnSelect;
    property OnHotTool;
    property OnDrawTool;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvOfficeBrushStyleSelector = class(TAdvCustomOfficeSelector)
  private
    FSelectionType: TSelectionType;
    FOnSelectBrushStyle: TBrushStyleSelectEvent;
    procedure Initialize;
    function GetSelectedBrushStyle: TBrushStyle;
    procedure SetSelectedBrushStyle(const Value: TBrushStyle);
    function GetBrushColor: TColor;
    procedure SetBrushColor(const Value: TColor);
    procedure SetSelectionType(const Value: TSelectionType);
  protected
    procedure Loaded; override;
    procedure SetSelectorPanel; override;
    procedure DoSelect(Index: Integer; Item: TAdvSelectorItem); override;

    function GetBrushStyleAtIndex(Index: Integer): TBrushStyle;
    function GetIndexOfBrushStyle(ABrushStyle: TBrushStyle): Integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property SelectedBrushStyle: TBrushStyle read GetSelectedBrushStyle write SetSelectedBrushStyle;
    property Down;
  published
    property AllowFloating;
    property Appearance;
    //property BackGroundImageColor;
    property BorderDropDownColor;
    property BrushColor: TColor read GetBrushColor write SetBrushColor;
    property Caption;
    property CaptionAppearance;
    property DragGripAppearance;
    property DropDownCheck;
    property CloseOnSelect;
    property ColorDropDown;
    property ColorDropDownTo;
    property ColorDropDownFloating;
    property ColorDropDownFloatingTo;

    property DropDownButton;
    property DragGripPosition;
    property Enabled;

    property SelectionType: TSelectionType read FSelectionType write SetSelectionType default stOffice;
    property SelectedIndex;
    property ShowHint;
    property SelectionAppearance;
    property Tools;

    property OnMouseEnter;
    property OnMouseLeave;
    property OnCanDropDown;
    property OnDropDown;
    property OnDropDownFloat;
    property OnDropDownClose;

    property OnClick;
    property OnSelect;
    property OnSelectBrushStyle: TBrushStyleSelectEvent read FOnSelectBrushStyle write FOnSelectBrushStyle;
    property OnHotTool;
    property OnDrawTool;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvOfficeShadowSelector = class(TAdvCustomOfficeSelector)
  private
    procedure Initialize;
  protected
    procedure Loaded; override;
    procedure SetSelectorPanel; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    //property AppearanceStyle;
    property Down;
  published
    property AllowFloating;
    property Appearance;
    property BorderDropDownColor;
    property Caption;
    property CaptionAppearance;
    property DragGripAppearance;
    property DropDownCheck;
    property CloseOnSelect;
    property ColorDropDown;
    property ColorDropDownTo;
    property ColorDropDownFloating;
    property ColorDropDownFloatingTo;

    property DropDownButton;
    property DragGripPosition;
    property Enabled;
    property ShowHint;
    //property Style;
    property SelectedIndex;
    property SelectionAppearance;
    property Tools;

    property OnMouseEnter;
    property OnMouseLeave;
    property OnCanDropDown;
    property OnDropDown;
    property OnDropDownFloat;
    property OnDropDownClose;

    property OnClick;
    property OnSelect;
    property OnHotTool;
    property OnDrawTool;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvOfficeTableBorderSelector = class(TAdvCustomOfficeSelector)
  private
    procedure Initialize;
  protected
    procedure SetSelectorPanel; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Down;
  published
    property AllowFloating;
    property Appearance;
    property BorderDropDownColor;
    property Caption;
    property CaptionAppearance;
    property DragGripAppearance;
    property DropDownCheck;
    property CloseOnSelect;
    property ColorDropDown;
    property ColorDropDownTo;
    property ColorDropDownFloating;
    property ColorDropDownFloatingTo;

    property DropDownButton;
    property DragGripPosition;
    property Enabled;
    //property Style;
    property ShowHint;
    property SelectedIndex;
    property SelectionAppearance;
    property Tools;

    property OnMouseEnter;
    property OnMouseLeave;
    property OnCanDropDown;
    property OnDropDown;
    property OnDropDownFloat;
    property OnDropDownClose;

    property OnClick;
    property OnSelect;
    property OnHotTool;
    property OnDrawTool;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvOfficeGradientDirectionSelector = class(TAdvCustomOfficeSelector)
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
    procedure GetToolImage(bmp: TBitmap); override;
    procedure DrawGlyphAndCaption(Pic: TGDIPPicture; R: TRect); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    //property AppearanceStyle;
  published
    property AllowFloating;
    property Appearance;
    property CaptionAppearance;
    property DragGripAppearance;
    property StartColor: TColor read FStartColor write SetStartColor;
    property EndColor: TColor read FEndColor write SetEndColor;

    property BorderDropDownColor;
    property Caption;
    property CloseOnSelect;
    property ColorDropDown;
    property ColorDropDownTo;
    property ColorDropDownFloating;
    property ColorDropDownFloatingTo;

    property DropDownButton;
    property DragGripPosition;
    property Enabled;
    //property Style;
    property SelectionAppearance;

    property SelectedIndex;
    property ShowSelectedGradient: boolean read FShowSelectedGradient write SetShowSelectedGradient default true;
    property Tools;
    property ShowHint;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnCanDropDown;
    property OnDropDown;
    property OnDropDownFloat;
    property OnDropDownClose;

    property OnClick;
    property OnSelect;
    property OnHotTool;
  end;

  TAdvColorSelectorActionLink = class(TAdvGlowButtonActionLink)
    procedure UpdateColor(AColor: TColor); virtual;
  end;

  TAdvCustomOfficeColorSelector = class(TAdvCustomOfficeSelector)
  private
    FColorCubePanel: TAdvColorCubePanel;
    FSpectrumPanel: TAdvColorSpectrumPanel;
    FColorSelectionStyle: TColorSelectionStyle;
    FShowSelectedColor: Boolean;
    FSelectedColor: TColor;
    FOnSelectColor: TColorSelectEvent;
    FShowRGBHint: Boolean;
    FColorDialog: TColorDialog;
    FCustomColors: string;
    procedure SetColorSelectionStyle(const Value: TColorSelectionStyle);
    procedure SelectorPanelOnDrawItem(Sender: TObject; Index: integer; R: TRect);
    procedure SetShowSelectedColor(const Value: Boolean);
    procedure OnColorCubeFloating(Sender: TObject);
    procedure OnColorSpectrumFloating(Sender: TObject);
    procedure OnColorSpectrumShouldHide(Sender: TObject);
    procedure OnColorCubeShouldHide(Sender: TObject);

    procedure CubePanelOnSelect(Sender: TObject);
    procedure SpectrumPanelOnSelect(Sender: TObject);
    procedure SetSelectedColor(const Value: TColor);
    function GetSelectedColor: TColor;
  protected
    procedure Initialize; virtual;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure OnToolSelect; override;
    procedure SetSelectorPanel; override;
    procedure DropDownWindowClose(Sender: TObject; var Action: TCloseAction); override;
    procedure GetToolImage(bmp: TBitmap); override;
    procedure DrawGlyphAndCaption(Pic: TGDIPPicture; R: TRect); override;
    procedure DoSelect(Index: integer; Item: TAdvSelectorItem); override;
    function GetActionLinkClass: TControlActionLinkClass; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    //property AppearanceStyle;
    property ButtonsPerRow;
    property ColorDialog: TColorDialog read FColorDialog write FColorDialog;
    property SelectionStyle: TColorSelectionStyle read FColorSelectionStyle write SetColorSelectionStyle default csDiscrete;
    property ShowSelectedColor: Boolean read FShowSelectedColor write SetShowSelectedColor default true;
    property SelectedColor: TColor read GetSelectedColor write SetSelectedColor;
    property ShowRGBHint: Boolean read FShowRGBHint write FShowRGBHint;
    property DropDownTopMost;
    property OnSelectColor: TColorSelectEvent read FOnSelectColor write FOnSelectColor;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvOfficeColorSelector = class(TAdvCustomOfficeColorSelector)
  protected
    procedure Initialize; override;
  published
    property AllowFloating;
    property CloseOnSelect;
    property CaptionAppearance;
    property ColorDialog;
    property DragGripAppearance;
    property DragGripPosition;
    property DropDownButton;
    property Enabled;
    property Appearance;
    property SelectionStyle;
    property ShowSelectedColor;
    property SelectedColor;
    property ShowRGBHint;
    property BorderDropDownColor;
    property Caption;
    property ColorDropDown;
    property ColorDropDownTo;
    property ColorDropDownFloating;
    property ColorDropDownFloatingTo;

    property ShowHint;
    //property Style;
    property SelectionAppearance;
    property Tools;

    property OnMouseEnter;
    property OnMouseLeave;
    property OnCanDropDown;
    property OnDropDown;
    property OnDropDownFloat;
    property OnDropDownClose;
    property OnClick;
    property OnSelect;
    property OnSelectColor;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvOfficeTextColorSelector = class(TAdvCustomOfficeColorSelector)
  protected
    procedure Initialize; override;
  public
  published
    property AllowFloating;
    property Appearance;
    property BorderDropDownColor;
    property Caption;
    property CaptionAppearance;
    property DragGripAppearance;
    property CloseOnSelect;
    property ColorDropDown;
    property ColorDropDownTo;
    property DropDownButton;
    property DragGripPosition;
    property ShowHint;
    property ShowSelectedColor;
    property SelectedColor;
    //property Style;
    property SelectionAppearance;
    property Tools;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnCanDropDown;
    property OnDropDown;
    property OnDropDownFloat;
    property OnDropDownClose;
    property OnClick;
    property OnSelect;
    property OnSelectColor;
  end;

  TAdvCustomOfficeTableSelector = class(TAdvCustomOfficeSelector)
  private
    FTableSelectorPanel: TAdvTableSelectorPanel;
    FSelectedColumns: integer;
    FSelectedRows: integer;
    FDefaultRowCount: integer;
    FDefaultColCount: integer;
    FOnSelect: TNotifyEvent;
    FTextTable: string;
    FTextCancel: string;
    FSelectionColor: TColor;
    FOnSelectTableSize: TTableSizeSelectEvent;
    procedure SetDefaultColCount(const Value: integer);
    procedure SetDefaultRowCount(const Value: integer);
    procedure TableSelectorOnSelect(Sender: TObject);
    procedure SetSelectionColor(const Value: TColor);
  protected
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure Loaded; override;
    procedure SetSelectorPanel; override;
    procedure DropDownWindowClose(Sender: TObject; var Action: TCloseAction); override;
    property DefaultColCount: integer read FDefaultColCount write SetDefaultColCount default 5;
    property DefaultRowCount: integer read FDefaultRowCount write SetDefaultRowCount default 4;
    property SelectedColumns: integer read FSelectedColumns default 0;
    property SelectedRows: integer read FSelectedRows default 0;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property SelectionColor: TColor read FSelectionColor write SetSelectionColor default clHighlight;
    property TextTable: string read FTextTable write FTextTable;
    property TextCancel: string read FTextCancel write FTextCancel;
    property OnSelect: TNotifyEvent read FOnSelect write FOnSelect;
    property OnSelectTableSize: TTableSizeSelectEvent read FOnSelectTableSize write FOnSelectTableSize;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvOfficeTableSelector = class(TAdvCustomOfficeTableSelector)
  protected
  public
    property SelectedColumns;
    property SelectedRows;
  published
    property Appearance;
    property Caption;
    property DefaultColCount;
    property DefaultRowCount;
    property SelectionColor;
    property ShowHint;
    property TextTable;
    property TextCancel;

    property OnMouseEnter;
    property OnMouseLeave;
    property OnDropDown;
    property OnClick;
    property OnSelect;
    property OnSelectTableSize;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvOfficeCharacterSelector = class(TAdvCustomOfficeSelector)
  private
    FOnSelect: TNotifyEvent;
    FCharacters: string;
    FSelectedChar: char;
    FAutoLoad: boolean;
    FCharFont: TFont;
    FOnSelectChar: TCharSelectEvent;
    procedure AddItemsFromChars;
    procedure LoadCharFromFont;
    procedure SetCharacters(const Value: string);
    procedure SetSelectedChar(const Value: char);
    procedure SetAutoLoad(const Value: boolean);
    procedure SetCharFont(const Value: TFont);
  protected
    //procedure Initialize; virtual;
    procedure Loaded; override;
    procedure DoSelect(Index: integer; Item: TAdvSelectorItem); override;
    procedure OnToolSelect; override;
    procedure SetSelectorPanel; override;
    procedure GetToolImage(bmp: TBitmap); override;
    procedure DrawGlyphAndCaption(Pic: TGDIPPicture; R: TRect); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Down;
    property Tools;
  published
    property AllowFloating;
    property Appearance;
    property AutoLoad: boolean read FAutoLoad write SetAutoLoad default true;
    property BorderDropDownColor;
    property ButtonsPerRow;
    property Caption;
    property CaptionAppearance;
    property DragGripAppearance;
    property DropDownCheck;
    property CloseOnSelect;
    property ColorDropDown;
    property ColorDropDownTo;
    property ColorDropDownFloating;
    property ColorDropDownFloatingTo;
    property Characters: string read FCharacters write SetCharacters;
    property CharFont: TFont read FCharFont write SetCharFont;
    property DropDownButton;
    property DragGripPosition;
    property ShowHint;
    property SelectedChar: char read FSelectedChar write SetSelectedChar;
    property SelectionAppearance;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnCanDropDown;
    property OnDropDown;
    property OnDropDownFloat;
    property OnDropDownClose;
    property OnClick;
    property OnSelect: TNotifyEvent read FOnSelect write FOnSelect;
    property OnSelectChar: TCharSelectEvent read FOnSelectChar write FOnSelectChar;
  end;

  //------------------------------------------------------------- ScrollSelector

  TSelectorScroller = class(TObject)
  private
    FMin: integer;
    FMax: integer;
    FPosition: integer;
    FVisible: Boolean;
    procedure SetMax(const Value: integer);
    procedure SetMin(const Value: integer);
    procedure SetPosition(const Value: integer);
    procedure SetVisible(const Value: Boolean);
  protected
  public
    constructor Create;
    function CanGoForward: Boolean;
    function CanGoBack: Boolean;
    property Min: integer read FMin write SetMin;
    property Max: integer read FMax write SetMax;
    property Position: integer read FPosition write SetPosition;
    property Visible: Boolean read FVisible write SetVisible;
  end;

  TDbgList = class(TList)
  private
    function GetItemsEx(Index: Integer): Pointer;
    procedure SetItemsEx(Index: Integer; const Value: Pointer);
  public
    property Items[Index: Integer]: Pointer read GetItemsEx write SetItemsEx; default;
  end;

  TAdvScrollSelectorItem = class(TAdvSelectorItem)
  private
    FRow: Integer;
    FRow1: Integer;
    FRow2: Integer;
    FItemRect1: TRect;
    FItemRect2: TRect;
    FIPicture: TGDIPPicture;
    FCol: Integer;
    FCol1: Integer;
    FCol2: Integer;
    FMenuItem: boolean;
    procedure OnPictureChanged(Sender: TObject);
    procedure SetPicture(const Value: TGDIPPicture);
  protected
    procedure SetCaption(const Value: string); override;
    procedure SetIndex(Value: Integer); override;
    property Row: Integer read FRow write FRow;
    property Col: Integer read FCol write FCol;
    //property ItemRect; // With ShowAutoSizeButton = True only
    property Row1: Integer read FRow1 write FRow1;
    property Col1: Integer read FCol1 write FCol1;
    property ItemRect1: TRect read FItemRect1 write FItemRect1; // With ShowFullWidthItem = false only
    property Row2: Integer read FRow2 write FRow2;
    property Col2: Integer read FCol2 write FCol2;
    property ItemRect2: TRect read FItemRect2 write FItemRect2; // With ShowCaptionItem = True only
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Changed; override;
  published
    property Picture: TGDIPPicture read FIPicture write SetPicture;
    property MenuItem: boolean read FMenuItem write FMenuItem default false;
  end;

  TAdvScrollSelectorItems = class(TCollection)
  private
    FOwner: TPersistent;
    FOnChange: TNotifyEvent;
    function GetItem(Index: Integer): TAdvScrollSelectorItem;
    procedure SetItem(Index: Integer; const Value: TAdvScrollSelectorItem);
  protected
    function GetOwner: TPersistent; override;
    procedure Changed;
  public
    constructor Create(AOwner: TPersistent);
    function Add: TAdvScrollSelectorItem;
    function Insert(Index: Integer): TAdvScrollSelectorItem;
    property Items[Index: Integer]: TAdvScrollSelectorItem read GetItem write SetItem; default;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TScrollSelectEvent = procedure(Sender: TObject; Index: Integer; Item: TAdvScrollSelectorItem) of object;

  TAdvCustomScrollSelectorPanel = class(TCustomControl)
  private
    FOwner: TComponent;
    FColorTo: TColor;
    FGradientDirection: TGradientDirection;
    FWindowBorderColor: TColor;
    FOnShouldHide: TNotifyEvent;
    FShowBorder: Boolean;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure SetColorTo(const Value: TColor);
    procedure SetTGradientDirection(const Value: TGradientDirection);
    procedure SetWindowBorderColor(const Value: TColor);
  protected
    procedure Paint; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure AdjustClientRect(var Rect: TRect); override;

    property OnShouldHide: TNotifyEvent read FOnShouldHide write FOnShouldHide;
    property ShowBorder: Boolean read FShowBorder write FShowBorder;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property ColorTo: TColor read FColorTo write SetColorTo default clNone;
    property WindowBorderColor: TColor read FWindowBorderColor write SetWindowBorderColor default clGray;
    property GradientDirection: TGradientDirection read FGradientDirection write SetTGradientDirection;
  end;

  TScrollSelectorDropDownWindow = class(TCustomForm)
  private
    FSelectorPanel: TAdvCustomScrollSelectorPanel;
    FHideOnDeActivate: Boolean;
    FShowAbove: Boolean;
    FOwner: TComponent;
    FShowBorder: Boolean;
    FHideTimer: TTimer;
    FShowLeft: Boolean;
    FScrollSelector: TAdvCustomOfficeScrollSelector;
    FOldCursor: TCursor;
    FMouseX: integer;
    FMouseY: integer;
    FResizing: Boolean;
    FSizeGrip: Boolean;
    FDropDownCaption: String;
    FResizerHeight: Integer;
    FCaptionHeight: Integer;
    FDropDownBorderWidth: Integer;
    FFullWidthSelector: TAdvScrollSelectorPanel;
    FScrollBox: TScrollBox;
    FItemsSelector: TAdvScrollSelectorPanel;
    FInternalChange: Boolean;
    procedure WMActivate(var Message: TWMActivate); message WM_ACTIVATE;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
    procedure HideTimerOnTime(Sender: TObject);
    procedure SetSelectorPanel(const Value: TAdvCustomScrollSelectorPanel);
  protected
    procedure Loaded; override;
    procedure Paint; override;
    function GetParentWnd: HWnd;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;

    function GetCaptionRect: TRect;
    function GetResizerRect: TRect;

    procedure InvalidateResizer;
    procedure InvalidateCaption;

    property HideOnDeActivate: Boolean read FHideOnDeActivate write FHideOnDeActivate;
    property ShowAbove: Boolean read FShowAbove write FShowAbove;
    property ShowLeft: Boolean read FShowLeft write FShowLeft default false;
    property ShowBorder: Boolean read FShowBorder write FShowBorder;
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateNew(AOwner: TComponent; Dummy: Integer = 0); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure SetPositions;
  published
    property SelectorPanel: TAdvCustomScrollSelectorPanel read FSelectorPanel write SetSelectorPanel;
    property ScrollBox: TScrollBox read FScrollBox write FScrollBox;
    property ScrollSelector: TAdvCustomOfficeScrollSelector read FScrollSelector write FScrollSelector;
    property FullWidthSelector: TAdvScrollSelectorPanel read FFullWidthSelector write FFullWidthSelector;
    property ItemsSelector: TAdvScrollSelectorPanel read FItemsSelector write FItemsSelector;
    property SizeGrip: Boolean read FSizeGrip write FSizeGrip;
    property DropDownCaption: String read FDropDownCaption write FDropDownCaption;
    property CaptionHeight: Integer read FCaptionHeight write FCaptionHeight;
    property ResizerHeight: Integer read FResizerHeight write FResizerHeight;
    property DropDownBorderWidth: Integer read FDropDownBorderWidth write FDropDownBorderWidth;
  end;

  TAdvScrollSelectorPanel = class(TAdvCustomScrollSelectorPanel)
  private
    FItemIndex: integer;
    FImages: TCustomImageList;
    FHotItemIndex: integer;
    FDownItemIndex: integer;
    FButtonHeight: integer;
    FTopOffSet: integer;
    FLeftOffSet: integer;
    FOnChangeSelection: TChangeSelectionEvent;
    FButtonMargin: integer;
    FMouseDown: Boolean;
    FOnSelect: TNotifyEvent;
    FOnHotTool: THotToolEvent;
    FMaxCaptionLength: integer;
    FOnDrawItem: TItemDrawEvent;
    FMinButtonWidth: integer;
    FMinButtonHeight: integer;
    FNoPrefix: Boolean;
    FTwoColorImages: Boolean;
    FSelectionAppearance: TSelectionAppearance;
    FCaptionAppearance: TSimpleGradientCaption;
    FAutoHeight: Boolean;
    FShowAutoSizeButton: Boolean;
    FShowCaptionItem: Boolean;
    FShowFullWidthItem: Boolean;
    FAdvOfficeScrollSelector: TAdvCustomOfficeScrollSelector;
    FTopRow: Integer;
    FVisibleRowCount: Integer;
    FRowCount: Integer;
    FRowHeight: Integer;
    FInternalUpdatingSize: Boolean;
    FOnScroll: TNotifyEvent;
    FIntegralRows: Boolean;
    FNeedRePaint: Boolean;
    FBKGCache: TBitmap;
    procedure WMChar(var Msg: TWMKey); message WM_CHAR;
    procedure CMWantSpecialKey(var Msg: TCMWantSpecialKey); message CM_WANTSPECIALKEY;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure WMEraseBkGnd(var Message: TWMEraseBkGnd); message WM_ERASEBKGND;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure SetItemIndex(const Value: integer);
    procedure SetImages(const Value: TCustomImageList);
    procedure SetButtonMargin(const Value: integer);
    procedure SetMinButtonHeight(const Value: integer);
    procedure SetMinButtonWidth(const Value: integer);
    procedure SetTwoColorImages(const Value: Boolean);
    procedure SetSelectionAppearance(const Value: TSelectionAppearance);
    procedure SetCaptionAppearance(const Value: TSimpleGradientCaption);
    function GetItems: TAdvScrollSelectorItems;
    function GetButtonsPerRow: TNoOfButtons;
    function GetHotItem: Integer;
    procedure SetHotItem(const Value: Integer);
    procedure SetIntegralRows(const Value: Boolean);
  protected
    procedure DrawItem(Index: integer; RefreshItem: boolean = false; Graph: TGPGraphics = nil; Canvas: TCanvas = nil);
    procedure DrawItemEx(Index: integer; RefreshItem: boolean = false; Graph: TGPGraphics = nil);
    procedure Paint; override;
    procedure ResetDown; virtual;
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
    function GetItemRect(Item: TAdvScrollSelectorItem): TRect;
    function GetItemRow(Item: TAdvScrollSelectorItem): Integer;
    function GetItemCol(Item: TAdvScrollSelectorItem): Integer;

    function IsShowingIconBar: Boolean;

    procedure DoSelect;
    function UseCache: Boolean; virtual;

    property MinButtonWidth: integer read FMinButtonWidth write SetMinButtonWidth default MINBUTTONSIZE;
    property MinButtonHeight: integer read FMinButtonHeight write SetMinButtonHeight default MINBUTTONSIZE;
    property NoPrefix: Boolean read FNoPrefix write FNoPrefix;

    property TwoColorImages: Boolean read FTwoColorImages write SetTwoColorImages;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    property Items: TAdvScrollSelectorItems read GetItems;
    property ItemIndex: integer read FItemIndex write SetItemIndex;

    function CanScrollUp: Integer;
    function CanScrollDown: Integer;
    procedure ScrollRows(Value: Integer);
    procedure ViewButton(Index: Integer);
    procedure ViewSelectedButton;
    procedure SetTopRow(Value: Integer);

    property HotItemIndex: Integer read GetHotItem write SetHotItem;
    function FirstItemIndex: Integer;
    function LastItemIndex: Integer;
    function PreviousItemIndex(Index: Integer): Integer;
    function NextItemIndex(Index: Integer): Integer;
    function UpItemIndex(Index: Integer): Integer;
    function DownItemIndex(Index: Integer): Integer;

    property OnScroll: TNotifyEvent read FOnScroll write FOnScroll;
  published
    property AutoHeight: Boolean read FAutoHeight write FAutoHeight;
    property SelectionAppearance: TSelectionAppearance read FSelectionAppearance write SetSelectionAppearance;
    property CaptionAppearance: TSimpleGradientCaption read FCaptionAppearance write SetCaptionAppearance;

    property ButtonsPerRow: TNoOfButtons read GetButtonsPerRow;
    property Images: TCustomImageList read FImages write SetImages;
    property ButtonMargin: Integer read FButtonMargin write SetButtonMargin;

    property ShowAutoSizeButton: Boolean read FShowAutoSizeButton write FShowAutoSizeButton;
    property ShowCaptionItem: Boolean read FShowCaptionItem write FShowCaptionItem;
    property ShowFullWidthItem: Boolean read FShowFullWidthItem write FShowFullWidthItem;

    property RowCount: Integer read FRowCount;
    property VisibleRowCount: Integer read FVisibleRowCount;
    property TopRow: Integer read FTopRow;

    property IntegralRows: Boolean read FIntegralRows write SetIntegralRows;

    property AdvOfficeScrollSelector: TAdvCustomOfficeScrollSelector read FAdvOfficeScrollSelector write FAdvOfficeScrollSelector;

    property OnChangeSelection: TChangeSelectionEvent read FOnChangeSelection write FOnChangeSelection;
    property OnSelect: TNotifyEvent read FOnSelect write FOnSelect;
    property OnHotTool: THotToolEvent read FOnHotTool write FOnHotTool;
    property OnDrawItem: TItemDrawEvent read FOnDrawItem write FOnDrawItem;
  end;

  TAdvCustomOfficeScrollSelector = class(TCustomControl, ITMSStyle)
  private
    FTools: TAdvScrollSelectorItems;
    FScrollSelectorPanel: TAdvScrollSelectorPanel;
    FColorDropDownTo: TColor;
    FColorDropDown: TColor;
    FColorTo: TColor;
    FBorderDropDownColor: TColor;
    FBorderColor: TColor;
    FToolImages: TCustomImageList;
    FOnDrawTool: TDrawToolEvent;
    FCaptionAppearance: TSimpleGradientCaption;
    FCaptionItemAppearance: TSimpleGradientCaption;
    FGradientDirection: TGradientDirection;
    FOnHotTool: THotToolEvent;
    FOnDropDown: TNotifyEvent;
    FSelectionAppearance: TSelectionAppearance;
    //FSelectedIndex: Integer;
    //FDupSelectedIndex: Integer;
    FDropDownButton: Boolean;
    FDropDownButtonDown: Boolean;
    FDropDownButtonHot: Boolean;
    FSelectorScroller: TSelectorScroller;
    FScrollerAppearance: TSelectionAppearance;
    FUpScrollerDown: Boolean;
    FUpScrollerHot: Boolean;
    FDownScrollerDown: Boolean;
    FDownScrollerHot: Boolean;
    FDropDownWindow: TScrollSelectorDropDownWindow;
    FItemsScrollBox: TScrollBox;
    FDropDownItemsPanel: TAdvScrollSelectorPanel;
    FDropDownFullWidthItemPanel: TAdvScrollSelectorPanel;
    FResizerAppearance: TGradientBackground;
    FDropDownSizeable: Boolean;
    FDropDownCaption: String;
    FButtonSize: TSelectorButtonSize;
    FIconBarAppearance: TGradientBackground;
    FIconBarWidth: Integer;
    FOnSelect: TScrollSelectEvent;
    FOnMouseLeave: TNotifyEvent;
    FOnScrollDown: TNotifyEvent;
    FOnScrollUp: TNotifyEvent;
    FOnMouseEnter: TNotifyEvent;
    FFocusColorTo: TColor;
    FFocusColor: TColor;
    FIntegralRows: Boolean;
    FOldDropDownHeight: Integer;
    FOldDropDownWidth: Integer;
    FDropDownHeight: Integer;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure CMFocusChanged(var Message: TCMFocusChanged); message CM_FOCUSCHANGED;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TWMSetFocus); message WM_KILLFOCUS;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure DrawUpScroller;
    procedure DrawDownScroller;
    procedure DrawScroller;
    procedure DrawDropDownButton;
    procedure OnAppearanceChanged(Sender: TObject);
    procedure OnItemsChanged(Sender: TObject);
    procedure OnScrollSelectorPanelScroll(Sender: TObject);
    procedure OnDropDownWindowHide(Sender: TObject);
    procedure OnDropDownWindowClose(Sender: TObject; var Action: TCloseAction);
    procedure OnDropDownPanelShouldHide(Sender: TObject);
    procedure OnDropDownPanelSelect(Sender: TObject);
    procedure OnDropDownPanelHotTool(Sender: TObject; HotItemIndex: integer);
    procedure OnDropDownPanelDrawTool(Sender: TObject; ItemIndex: integer; R: TRect);
    procedure ButtonSizeChanged(Sender: TObject);
    function GetSelectedIndex: integer;
    procedure SetBorderColor(const Value: TColor);
    procedure SetCaptionAppearance(const Value: TSimpleGradientCaption);
    procedure SetCaptionItemAppearance(const Value: TSimpleGradientCaption);
    procedure SetColorDropDown(const Value: TColor);
    procedure SetColorDropDownTo(const Value: TColor);
    procedure SetColorTo(const Value: TColor);
    procedure SetSelectedIndex(const Value: integer);
    procedure SetSelectionAppearance(const Value: TSelectionAppearance);
    procedure SetTGradientDirection(const Value: TGradientDirection);
    procedure SetToolImages(const Value: TCustomImageList);
    procedure SetTools(const Value: TAdvScrollSelectorItems);
    procedure SetDropDownButton(const Value: Boolean);
    procedure SetScrollerAppearance(const Value: TSelectionAppearance);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    procedure SetResizerAppearance(const Value: TGradientBackground);
    procedure SetButtonSize(const Value: TSelectorButtonSize);
    procedure SetIconBarAppearance(const Value: TGradientBackground);
    function GetTopRow: Integer;
    procedure SetTopRow(const Value: Integer);
    function GetColor: TColor;
    procedure SetColor(const Value: TColor);
    procedure SetIntegralRows(const Value: Boolean);
  protected
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;

    procedure Paint; override;
    procedure WndProc(var Message: TMessage); override;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;

    function GetVersionNr: Integer; virtual;
    procedure DoSelect(Index: integer; Item: TAdvScrollSelectorItem); virtual;
    procedure OnToolSelect; virtual;

    function ScrollSelectorPanelRect: TRect;
    procedure SetScrollSelectorPanelPos;
    procedure UpdateScrollSelectorPanel;
    procedure UpdateScroller;

    function ScrollerRect: TRect;
    function ScrollerUpRect: TRect;
    function ScrollerDownRect: TRect;
    function DropDownBtnRect: TRect;

    procedure InvalidateScroller;
    procedure UpScrollerClick;
    procedure DownScrollerClick;
    procedure DropDownButtonClick;

    procedure ShowDropDown;
    procedure HideDropDown;

    function CaptionItemCount: Integer;
    function FullWidthButtonCount: Integer;
    function AutoSizeButtonCount: Integer;

    property BorderColor: TColor read FBorderColor write SetBorderColor;
    property BorderDropDownColor: TColor read FBorderDropDownColor write FBorderDropDownColor default clGray;
    property ColorDropDown: TColor read FColorDropDown write SetColorDropDown;
    property ColorDropDownTo: TColor read FColorDropDownTo write SetColorDropDownTo default clNone;
    property Color: TColor read GetColor write SetColor;
    property ColorTo: TColor read FColorTo write SetColorTo;
    property FocusColor: TColor read FFocusColor write FFocusColor;
    property FocusColorTo: TColor read FFocusColorTo write FFocusColorTo;
    property SelectionAppearance: TSelectionAppearance read FSelectionAppearance write SetSelectionAppearance;
    property CaptionAppearance: TSimpleGradientCaption read FCaptionAppearance write SetCaptionAppearance;
    property ResizerAppearance: TGradientBackground read FResizerAppearance write SetResizerAppearance;
    property CaptionItemAppearance: TSimpleGradientCaption read FCaptionItemAppearance write SetCaptionItemAppearance;
    property GradientDirection: TGradientDirection read FGradientDirection write SetTGradientDirection;
    property ToolImages: TCustomImageList read FToolImages write SetToolImages;
    property Tools: TAdvScrollSelectorItems read FTools write SetTools;
    property SelectedIndex: integer read GetSelectedIndex write SetSelectedIndex;
    property ScrollerAppearance: TSelectionAppearance read FScrollerAppearance write SetScrollerAppearance;

    property DropDownButton: Boolean read FDropDownButton write SetDropDownButton;
    property DropDownHeight: integer read FDropDownHeight write FDropDownHeight default 250;
    property DropDownSizeable: Boolean read FDropDownSizeable write FDropDownSizeable;
    property DropDownCaption: String read FDropDownCaption write FDropDownCaption;

    property ButtonSize: TSelectorButtonSize read FButtonSize write SetButtonSize;

    property IconBarWidth: Integer read FIconBarWidth write FIconBarWidth;
    property IconBarAppearance: TGradientBackground read FIconBarAppearance write SetIconBarAppearance;

    property TopRow: Integer read GetTopRow write SetTopRow;
    property IntegralRows: Boolean read FIntegralRows write SetIntegralRows default true;

    property OnDropDown: TNotifyEvent read FOnDropDown write FOnDropDown;
    property OnSelect: TScrollSelectEvent read FOnSelect write FOnSelect;
    property OnHotTool: THotToolEvent read FOnHotTool write FOnHotTool;
    property OnDrawTool: TDrawToolEvent read FOnDrawTool write FOnDrawTool;

    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnScrollUp: TNotifyEvent read FOnScrollUp write FOnScrollUp;
    property OnScrollDown: TNotifyEvent read FOnScrollDown write FOnScrollDown;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;

    procedure SetComponentStyle(AStyle: TTMSStyle);
    procedure BeginUpdate;
    procedure EndUpdate;
  published
    property Version: string read GetVersion write SetVersion;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvOfficeScrollSelector = class(TAdvCustomOfficeScrollSelector)
  private
  public
    property TopRow;
  published
    property BorderColor;
    property BorderDropDownColor;
    property ButtonSize;
    property DropDownCaption;
    property DropDownHeight;
    property DropDownSizeable;
    property ColorDropDown;
    property ColorDropDownTo;
    property Color;
    property ColorTo;
    property FocusColor;
    property FocusColorTo;
    property SelectionAppearance;
    property CaptionAppearance;
    property CaptionItemAppearance;
    property GradientDirection;
    property IconBarWidth;
    property IconBarAppearance;
    property IntegralRows;
    property ToolImages;
    property Tools;
    property SelectedIndex;
    property ScrollerAppearance;
    property ResizerAppearance;
    property TabOrder;
    property TabStop;

    property OnDropDown;
    property OnSelect;
    property OnHotTool;
    property OnDrawTool;

    property OnKeyDown;
    property OnKeyUp;
    property OnKeyPress;
    property OnEnter;
    property OnExit;
    property OnMouseLeave;
    property OnMouseEnter;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnScrollUp;
    property OnScrollDown;
  end;

implementation

{$R AdvOfficeSelectors.RES}

uses
  ComObj;

const
  crTMSCur1 = 54;
  // theme changed notifier
  WM_THEMECHANGED = $031A;

type
  XPColorScheme = (xpNone, xpBlue, xpGreen, xpGray);

var
  GetCurrentThemeName: function(pszThemeFileName: PWideChar;
    cchMaxNameChars: Integer;
    pszColorBuff: PWideChar;
    cchMaxColorChars: Integer;
    pszSizeBuff: PWideChar;
    cchMaxSizeChars: Integer): THandle cdecl stdcall;

  IsThemeActive: function: BOOL cdecl stdcall;


function CurrentXPTheme: XPColorScheme;
var
  FileName, ColorScheme, SizeName: WideString;
  hThemeLib: THandle;
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

procedure DrawCross(Canvas: TCanvas; R: TRect; Clr: TColor; Size: Integer);
var
  x, y: Integer;
begin
  with Canvas do
  begin
    x := (R.Right - R.Left - Size) div 2;
    y := ((R.Bottom - R.Top - Size) div 2) -1;
    Pen.Color := Clr;
                   {/}
    MoveTo(R.Left+x, R.Top + Size+y);
    LineTo(R.Left + Size+1+x, R.Top+y);
    MoveTo(R.Left + 1+x, R.Top + Size+y);
    LineTo(R.Left + Size+x, R.Top+y);
                   {\}
    MoveTo(R.Left+x, R.Top + 1+y);
    LineTo(R.Left + Size+1+x, R.Top + 1 + Size+y);
    MoveTo(R.Left + 1+x, R.Top + 1+y);
    LineTo(R.Left + Size+x, R.Top + 1 + Size+y);
  end;
end;

//------------------------------------------------------------------------------

function BrightnessColor(Col: TColor; Brightness: integer): TColor; overload;
var
  r1,g1,b1: Integer;
begin
  Col := ColorToRGB(Col);
  r1 := GetRValue(Col);
  g1 := GetGValue(Col);
  b1 := GetBValue(Col);

  if r1 = 0 then
    r1 := Max(0,Brightness)
  else
    r1 := Round( Min(100,(100 + Brightness))/100 * r1 );

  if g1 = 0 then
    g1 := Max(0,Brightness)
  else
    g1 := Round( Min(100,(100 + Brightness))/100 * g1 );

  if b1 = 0 then
    b1 := Max(0,Brightness)
  else
    b1 := Round( Min(100,(100 + Brightness))/100 * b1 );

  Result := RGB(r1,g1,b1);
end;

//------------------------------------------------------------------------------

function BrightnessColor(Col: TColor; BR,BG,BB: integer): TColor; overload;
var
  r1,g1,b1: Integer;
begin
  Col := Longint(ColorToRGB(Col));
  r1 := GetRValue(Col);
  g1 := GetGValue(Col);
  b1 := GetBValue(Col);

  if r1 = 0 then
    r1 := Max(0,BR)
  else
    r1 := Round( Min(100,(100 + BR))/100 * r1 );

  if g1 = 0 then
    g1 := Max(0,BG)
  else
    g1 := Round( Min(100,(100 + BG))/100 * g1 );

  if b1 = 0 then
    b1 := Max(0,BB)
  else
    b1 := Round( Min(100,(100 + BB))/100 * b1 );

  Result := RGB(r1,g1,b1);
end;

//------------------------------------------------------------------------------

function ColorToARGB(Color: TColor): ARGB;
var
  c: TColor;
begin
  c := ColorToRGB(Color);
  Result := ARGB( $FF000000 or ((DWORD(c) and $FF) shl 16) or ((DWORD(c) and $FF00) or ((DWORD(c) and $ff0000) shr 16)));
end;

//------------------------------------------------------------------------------

function BlendColor(Col1,Col2:TColor; BlendFactor:Integer): TColor;
var
  r1,g1,b1: Integer;
  r2,g2,b2: Integer;

begin
  if BlendFactor >= 100 then
  begin
    Result := Col1;
    Exit;
  end;
  if BlendFactor <= 0 then
  begin
    Result := Col2;
    Exit;
  end;

  Col1 := Longint(ColorToRGB(Col1));
  r1 := GetRValue(Col1);
  g1 := GetGValue(Col1);
  b1 := GetBValue(Col1);

  Col2 := Longint(ColorToRGB(Col2));
  r2 := GetRValue(Col2);
  g2 := GetGValue(Col2);
  b2 := GetBValue(Col2);

  r1 := Round( BlendFactor/100 * r1 + (1 - BlendFactor/100) * r2);
  g1 := Round( BlendFactor/100 * g1 + (1 - BlendFactor/100) * g2);
  b1 := Round( BlendFactor/100 * b1 + (1 - BlendFactor/100) * b2);

  Result := RGB(r1,g1,b1);
end;

//------------------------------------------------------------------------------

procedure DrawRoundRect(graphics: TGPGraphics; Pen: TGPPen; X,Y,Width,Height,Radius: integer);
var
  path:TGPGraphicsPath;
begin
  path := TGPGraphicsPath.Create;
  path.AddLine(X + radius, Y, X + width - (radius*2), Y);
  path.AddArc(X + width - (radius*2), Y, radius*2, radius*2, 270, 90);
  path.AddLine(X + width, Y + radius, X + width, Y + height - (radius*2));
  path.AddArc(X + width - (radius*2), Y + height - (radius*2), radius*2, radius*2,0,90);
  path.AddLine(X + width - (radius*2), Y + height, X + radius, Y + height);
  path.AddArc(X, Y + height - (radius*2), radius*2, radius*2, 90, 90);
  path.AddLine(X, Y + height - (radius*2), X, Y + radius);
  path.AddArc(X, Y, radius*2, radius*2, 180, 90);
  path.CloseFigure;
  graphics.DrawPath(pen, path);
  path.Free;
end;

//------------------------------------------------------------------------------

procedure DrawRect(graphics: TGPGraphics; Pen: TGPPen; X,Y,Width,Height: integer);
var
  path:TGPGraphicsPath;
begin
  path := TGPGraphicsPath.Create;
  path.AddLine(X, Y, X + width, Y);
  path.AddLine(X + width, Y, X + width, Y + height);
  path.AddLine(X + width, Y + height, X, Y + height);
  path.AddLine(X, Y + height, X, Y);
  path.CloseFigure;
  graphics.DrawPath(pen, path);
  path.Free;
end;

//------------------------------------------------------------------------------

function DrawVistaText(Canvas: TCanvas; Alignment: TAlignment; r: TRect; Caption:string; AFont: TFont; Enabled: Boolean; RealDraw: Boolean; AntiAlias: TAntiAlias; Direction: TTabPosition): TRect;
var
  graphics : TGPGraphics;
  w,h: Integer;
  fontFamily: TGPFontFamily;
  font: TGPFont;
  rectf: TGPRectF;
  stringFormat: TGPStringFormat;
  solidBrush: TGPSolidBrush;
  x1,y1,x2,y2: single;
  fs: integer;
  sizerect: TGPRectF;
  szRect: TRect;
  DTFLAG: DWORD;
begin
  if (Caption <> '') then
  begin
    graphics := TGPGraphics.Create(Canvas.Handle);
    fontFamily:= TGPFontFamily.Create(AFont.Name);
    fs := 0;

    font := TGPFont.Create(fontFamily, AFont.Size , fs, UnitPoint);
    graphics.SetSmoothingMode(SmoothingModeAntiAlias);

    w := R.Right - R.Left;
    h := R.Bottom - R.Top;

    x1 := r.Left;
    y1 := r.Top;
    x2 := w;
    y2 := h;

    rectf := MakeRect(x1,y1,x2,y2);

    stringFormat := nil;
    if RealDraw then
    begin
      case (Direction) of
        tpTop, tpBottom: stringFormat := TGPStringFormat.Create;
        tpLeft:
        begin
          stringFormat := TGPStringFormat.Create; //($00000002);
        end;
        tpRight: stringFormat := TGPStringFormat.Create($00000002);
      end;
    end
    else
      stringFormat := TGPStringFormat.Create;


    if Enabled then
      solidBrush := TGPSolidBrush.Create(ColorToARGB(AFont.Color))
    else
      solidBrush := TGPSolidBrush.Create(ColorToARGB(clGray));

    case Alignment of
      taLeftJustify: stringFormat.SetAlignment(StringAlignmentNear);
      taCenter:
      begin
        // Center-justify each line of text.
        stringFormat.SetAlignment(StringAlignmentCenter);
      end;
      taRightJustify: stringFormat.SetAlignment(StringAlignmentFar);
    end;

    // Center the block of text (top to bottom) in the rectangle.
    stringFormat.SetLineAlignment(StringAlignmentCenter);

    stringFormat.SetHotkeyPrefix(HotkeyPrefixShow);

    //graphics.SetTextRenderingHint(TextRenderingHintClearTypeGridFit);
    //graphics.MeasureString(Caption, Length(Caption), font, rectf, stringFormat, sizerect);
    case AntiAlias of
    aaClearType:graphics.SetTextRenderingHint(TextRenderingHintClearTypeGridFit);
    aaAntiAlias:graphics.SetTextRenderingHint(TextRenderingHintAntiAlias);
    end;

    if AntiAlias = aaNone then
    begin
      szRect.Left := round(rectf.X);
      szRect.Top := round(rectf.Y);

      szRect.Right := szRect.Left + 2;
      szRect.Bottom := DrawText(Canvas.Handle,PChar(Caption),Length(Caption), szrect, DT_CALCRECT or DT_LEFT or DT_WORDBREAK);

      sizeRect.X := szRect.Left;
      sizeRect.Y := szRect.Top;
      sizeRect.Width := szRect.Right - szRect.Left;
      sizeRect.Height := szRect.Bottom - szRect.Top;
    end
    else
      graphics.MeasureString(Caption, Length(Caption), font, rectf, stringFormat, sizerect);


    Result := Rect(round(sizerect.X), Round(sizerect.Y), Round(sizerect.X + sizerect.Width), Round(sizerect.Y + sizerect.Height));
    rectf := MakeRect(x1,y1,x2,y2);

    if RealDraw then
    begin
      //graphics.DrawString(Caption, Length(Caption), font, rectf, stringFormat, solidBrush);
      if AntiAlias = aaNone then
      begin
        szRect.Left := round(rectf.X);
        szRect.Top := round(rectf.Y);
        szRect.Right := szRect.Left + round(rectf.Width);
        szRect.Bottom := szRect.Top + round(rectf.Height);
        Canvas.Brush.Style := bsClear;

        DTFLAG := DT_LEFT;
        case Alignment of
        taRightJustify: DTFLAG := DT_RIGHT;
        taCenter: DTFLAG := DT_CENTER;
        end;
        DrawText(Canvas.Handle,PChar(Caption),Length(Caption), szrect, DTFLAG or DT_VCENTER or DT_SINGLELINE)
      end
      else
        graphics.DrawString(Caption, Length(Caption), font, rectf, stringFormat, solidBrush);
    end;
      
    stringformat.Free;
    solidBrush.Free;
    font.Free;
    fontfamily.Free;
    graphics.Free;
  end;
end;

//------------------------------------------------------------------------------

procedure DrawVistaGradient(Canvas: TCanvas; r: TRect; CFU, CTU, CFB, CTB, PC: TColor;
   GradientU,GradientB: TGDIPGradient; Caption:string; AFont: TFont;
   Images: TImageList; ImageIndex: integer; EnabledImage: Boolean; Layout: TButtonLayout;
   DropDownButton: Boolean; DrawDwLine: Boolean; Enabled: Boolean; Focus: Boolean; DropDownPos: TDropDownPosition;
   Picture: TGDIPPicture; AntiAlias: TAntiAlias; RoundEdges: Boolean; RotateLeftRight: Boolean; Direction: TTabPosition; Graph: TGPGraphics); overload;
var
  graphics : TGPGraphics;
  path: TGPGraphicsPath;
  pthGrBrush: TGPPathGradientBrush;
  linGrBrush: TGPLinearGradientBrush;
  gppen : tgppen;
  count: Integer;
  w,h,h2,w2: Integer;
  colors : array[0..0] of TGPColor;
  fontFamily: TGPFontFamily;
  font: TGPFont;
  rectf: TGPRectF;
  stringFormat: TGPStringFormat;
  solidBrush: TGPSolidBrush;
  x1,y1,x2,y2: single;
  fs: integer;
  sizerect: TGPRectF;
  ImgX, ImgY, ImgW, ImgH: Integer;
  BtnR, DwR: TRect;
  AP: TPoint;
  szRect: TRect;

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
  BtnR := R;
  if DropDownPos = dpRight then
  begin
    DwR := Rect(BtnR.Right - DropDownSectWidth, BtnR.Top, BtnR.Right, BtnR.Bottom);
    if DropDownButton then
      BtnR.Right := DwR.Left;
  end
  else // DropDownPos = doBottom
  begin
    DwR := Rect(BtnR.Left, BtnR.Bottom - DropDownSectWidth, BtnR.Right, BtnR.Bottom);
    if DropDownButton then
      BtnR.Bottom := DwR.Top;
  end;

  w := r.Right - r.Left;
  h := r.Bottom - r.Top;

  h2 := h div 2;
  w2 := w div 2;

  if Assigned(Graph) then
    graphics := Graph
  else
    graphics := TGPGraphics.Create(Canvas.Handle);

  case (Direction) of
    tpTop:
    begin
      // down ellips brush
      Canvas.Brush.Color := cfb;
      Canvas.FillRect(rect(r.Left , r.top +  h2, r.Right , r.Bottom ));

      // Create a path that consists of a single ellipse.
      path := TGPGraphicsPath.Create;
      //  path.AddRectangle(MakeRect(r.Left, r.Top +  (h div 2), w , h));
      path.AddEllipse(r.Left, r.Top +  h2, w , h);

      pthGrBrush := nil;
      linGrBrush := nil;

      case GradientB of
      ggRadial: pthGrBrush := TGPPathGradientBrush.Create(path);
      ggVertical: linGrBrush := TGPLinearGradientBrush.Create(MakeRect(r.Left,r.Top + h2,w,h2),ColorToARGB(CFB),ColorToARGB(CTB), LinearGradientModeVertical);
      ggDiagonalForward: linGrBrush := TGPLinearGradientBrush.Create(MakeRect(r.Left,r.Top + h2,w,h2),ColorToARGB(CFB),ColorToARGB(CTB), LinearGradientModeForwardDiagonal);
      ggDiagonalBackward: linGrBrush := TGPLinearGradientBrush.Create(MakeRect(r.Left,r.Top + h2,w,h2),ColorToARGB(CFB),ColorToARGB(CTB), LinearGradientModeBackwardDiagonal);
      end;

      if GradientB = ggRadial then
      begin
        pthGrBrush.SetCenterPoint(MakePoint(r.Left + w2, r.Bottom));

        // Set the color at the center point to blue.
        pthGrBrush.SetCenterColor(ColorToARGB(CTB));

        colors[0] := ColorToARGB(CFB);
        count := 1;
        pthGrBrush.SetSurroundColors(@colors, count);
        graphics.FillRectangle(pthGrBrush, r.Left + 1,r.Top + h2, w - 1, h2{+1});
        pthGrBrush.Free;
      end
      else
      begin
        if not RotateLeftRight then
          graphics.FillRectangle(linGrBrush, r.Left + 1,r.Top + h2 + 1, w - 1, h2 - 1)
        else
          graphics.FillRectangle(linGrBrush, r.Left + 1,r.Top + h2 + 1, w - 1, h2 + 1);
        linGrBrush.Free;
      end;

      path.Free;

      Canvas.Brush.Color := cfu;
      //Canvas.FillRect(rect(r.Left + 1, r.Top + 2, r.Right - 1, r.top +  h2));
      Canvas.FillRect(rect(r.Left , r.Top , r.Right , r.top +  h2));

      // Create a path that consists of a single ellipse.
      path := TGPGraphicsPath.Create;
      path.AddEllipse(r.Left, r.Top - h2 , w , h);

      case GradientU of
      ggRadial: pthGrBrush := TGPPathGradientBrush.Create(path);                      // FF: Gradient fix here replace h by h2
      ggVertical: linGrBrush := TGPLinearGradientBrush.Create(MakeRect(r.Left,r.Top,w,h2+1),ColorToARGB(CFU),ColorToARGB(CTU), LinearGradientModeVertical);
      ggDiagonalForward: linGrBrush := TGPLinearGradientBrush.Create(MakeRect(r.Left,r.Top,w,h),ColorToARGB(CFU),ColorToARGB(CTU), LinearGradientModeForwardDiagonal);
      ggDiagonalBackward: linGrBrush := TGPLinearGradientBrush.Create(MakeRect(r.Left,r.Top,w,h),ColorToARGB(CFU),ColorToARGB(CTU), LinearGradientModeBackwardDiagonal);
      end;

      if GradientU = ggRadial then
      begin
        pthGrBrush.SetCenterPoint(MakePoint(r.Left + w2, r.top));

        // Set the color at the center point to blue.
        pthGrBrush.SetCenterColor(ColorToARGB(CTU));

        colors[0] := ColorToARGB(CFU);
        count := 1;
        pthGrBrush.SetSurroundColors(@colors, count);

        graphics.FillRectangle(pthGrBrush, r.Left + 1,r.Top + 1, w - 1, h - h2 - 1);
        pthGrBrush.Free;
      end
      else
      begin
        graphics.FillRectangle(linGrBrush, r.Left + 1,r.Top + 1, w - 1, h2);
        linGrBrush.Free;
      end;

      path.Free;

    end;
    tpBottom:
    begin
      // down ellips brush
      Canvas.Brush.Color := cfb;
      Canvas.FillRect(rect(r.Left , r.top, r.Right , r.top +  h2));

      // Create a path that consists of a single ellipse.
      path := TGPGraphicsPath.Create;
      //  path.AddRectangle(MakeRect(r.Left, r.Top +  (h div 2), w , h));
      path.AddEllipse(r.Left, r.Top, w , h2);

      pthGrBrush := nil;
      linGrBrush := nil;

      case GradientB of
      ggRadial: pthGrBrush := TGPPathGradientBrush.Create(path);
      ggVertical: linGrBrush := TGPLinearGradientBrush.Create(MakeRect(r.Left,r.Top,w,h2),ColorToARGB(CTB),ColorToARGB(CFB), LinearGradientModeVertical);
      ggDiagonalForward: linGrBrush := TGPLinearGradientBrush.Create(MakeRect(r.Left,r.Top,w,h2),ColorToARGB(CTB),ColorToARGB(CFB), LinearGradientModeForwardDiagonal);
      ggDiagonalBackward: linGrBrush := TGPLinearGradientBrush.Create(MakeRect(r.Left,r.Top,w,h2),ColorToARGB(CTB),ColorToARGB(CFB), LinearGradientModeBackwardDiagonal);
      end;

      if GradientB = ggRadial then
      begin
        pthGrBrush.SetCenterPoint(MakePoint(r.Left + w2, r.Top));

        // Set the color at the center point to blue.
        pthGrBrush.SetCenterColor(ColorToARGB(CTB));

        colors[0] := ColorToARGB(CFB);
        count := 1;
        pthGrBrush.SetSurroundColors(@colors, count);
        graphics.FillRectangle(pthGrBrush, r.Left + 1,r.Top, w - 1, h2+1);
        pthGrBrush.Free;
      end
      else
      begin
        graphics.FillRectangle(linGrBrush, r.Left + 1,r.Top + 1, w - 1, h2 + 1);
        linGrBrush.Free;
      end;

      path.Free;

      Canvas.Brush.Color := cfu;
      Canvas.FillRect(rect(r.Left , r.top +  h2, r.Right , r.Bottom));

      // Create a path that consists of a single ellipse.
      path := TGPGraphicsPath.Create;
      path.AddEllipse(r.Left, r.Bottom - h2 , w , h);

      case GradientU of
      ggRadial: pthGrBrush := TGPPathGradientBrush.Create(path);                      // FF: Gradient fix here replace h by h2
      ggVertical: linGrBrush := TGPLinearGradientBrush.Create(MakeRect(r.Left,r.Top + h2-1,w,h2),ColorToARGB(CTU),ColorToARGB(CFU), LinearGradientModeVertical);
      ggDiagonalForward: linGrBrush := TGPLinearGradientBrush.Create(MakeRect(r.Left,r.Top + h2,w,h),ColorToARGB(CTU),ColorToARGB(CFU), LinearGradientModeForwardDiagonal);
      ggDiagonalBackward: linGrBrush := TGPLinearGradientBrush.Create(MakeRect(r.Left,r.Top + h2,w,h),ColorToARGB(CTU),ColorToARGB(CFU), LinearGradientModeBackwardDiagonal);
      end;

      if GradientU = ggRadial then
      begin
        pthGrBrush.SetCenterPoint(MakePoint(r.Left + w2, r.Bottom));

        // Set the color at the center point to blue.
        pthGrBrush.SetCenterColor(ColorToARGB(CTU));

        colors[0] := ColorToARGB(CFU);
        count := 1;
        pthGrBrush.SetSurroundColors(@colors, count);

        graphics.FillRectangle(pthGrBrush, r.Left + 1,r.Top + h2 + 1, w - 1, h2 - 1);
        pthGrBrush.Free;
      end
      else
      begin
        graphics.FillRectangle(linGrBrush, r.Left + 1,r.Top + h2, w - 1, h2 - 1);
        linGrBrush.Free;
      end;

      path.Free;
    end;
    tpLeft:
    begin
      // down ellips brush
      Canvas.Brush.Color := cfb;
      Canvas.FillRect(rect(r.Left + w2, r.top, r.Right , r.Bottom));

      // Create a path that consists of a single ellipse.
      path := TGPGraphicsPath.Create;
      //  path.AddRectangle(MakeRect(r.Left, r.Top +  (h div 2), w , h));
      path.AddEllipse(r.Left + w2, r.Top, w , h);

      pthGrBrush := nil;
      linGrBrush := nil;

      case GradientB of
      ggRadial: pthGrBrush := TGPPathGradientBrush.Create(path);
      ggVertical: linGrBrush := TGPLinearGradientBrush.Create(MakeRect(r.Left + w2,r.Top,w2,h),ColorToARGB(CFB),ColorToARGB(CTB), LinearGradientModeHorizontal);
      ggDiagonalForward: linGrBrush := TGPLinearGradientBrush.Create(MakeRect(r.Left + w2,r.Top,w2,h),ColorToARGB(CFB),ColorToARGB(CTB), LinearGradientModeForwardDiagonal);
      ggDiagonalBackward: linGrBrush := TGPLinearGradientBrush.Create(MakeRect(r.Left + w2,r.Top,w2,h),ColorToARGB(CFB),ColorToARGB(CTB), LinearGradientModeBackwardDiagonal);
      end;

      if GradientB = ggRadial then
      begin
        pthGrBrush.SetCenterPoint(MakePoint(r.Right, r.Top + h2));

        // Set the color at the center point to blue.
        pthGrBrush.SetCenterColor(ColorToARGB(CTB));

        colors[0] := ColorToARGB(CFB);
        count := 1;
        pthGrBrush.SetSurroundColors(@colors, count);
        graphics.FillRectangle(pthGrBrush, r.Left + w2, r.Top, w2 + 1, h-1);
        pthGrBrush.Free;
      end
      else
      begin
        graphics.FillRectangle(linGrBrush, r.Left + w2 + 1,r.Top, w2 + 1, h - 1);
        linGrBrush.Free;
      end;

      path.Free;

      Canvas.Brush.Color := cfu;
      //Canvas.FillRect(rect(r.Left + 1, r.Top + 2, r.Right - 1, r.top +  h2));
      Canvas.FillRect(rect(r.Left , r.Top , r.Left + w2 , r.Bottom));

      // Create a path that consists of a single ellipse.
      path := TGPGraphicsPath.Create;
      path.AddEllipse(r.Left - w2, r.Top, w , h);

      case GradientU of
      ggRadial: pthGrBrush := TGPPathGradientBrush.Create(path);                      // FF: Gradient fix here replace h by h2
      ggVertical: linGrBrush := TGPLinearGradientBrush.Create(MakeRect(r.Left,r.Top,w2,h),ColorToARGB(CFU),ColorToARGB(CTU), LinearGradientModeHorizontal);
      ggDiagonalForward: linGrBrush := TGPLinearGradientBrush.Create(MakeRect(r.Left,r.Top,w,h),ColorToARGB(CFU),ColorToARGB(CTU), LinearGradientModeForwardDiagonal);
      ggDiagonalBackward: linGrBrush := TGPLinearGradientBrush.Create(MakeRect(r.Left,r.Top,w,h),ColorToARGB(CFU),ColorToARGB(CTU), LinearGradientModeBackwardDiagonal);
      end;

      if GradientU = ggRadial then
      begin
        pthGrBrush.SetCenterPoint(MakePoint(r.Left, r.top + h2));

        // Set the color at the center point to blue.
        pthGrBrush.SetCenterColor(ColorToARGB(CTU));

        colors[0] := ColorToARGB(CFU);
        count := 1;
        pthGrBrush.SetSurroundColors(@colors, count);

        graphics.FillRectangle(pthGrBrush, r.Left + 1,r.Top + 1, w2 - 1, h - 1);
        pthGrBrush.Free;
      end
      else
      begin
        graphics.FillRectangle(linGrBrush, r.Left + 1,r.Top + 1, w2 - 1, h - 1);
        linGrBrush.Free;
      end;

      path.Free;

    end;
    tpRight:
    begin
      Canvas.Brush.Color := cfu;
      Canvas.FillRect(rect(r.Right - w2 , r.Top , r.Right ,r.Bottom));

      // Create a path that consists of a single ellipse.
      path := TGPGraphicsPath.Create;
      path.AddEllipse(r.Right - w2, r.Top, w, h);

      pthGrBrush := nil;
      linGrBrush := nil;

      case GradientU of
      ggRadial: pthGrBrush := TGPPathGradientBrush.Create(path);                      // FF: Gradient fix here replace h by h2
      ggVertical: linGrBrush := TGPLinearGradientBrush.Create(MakeRect(r.Right-w2,r.Top,w2,h),ColorToARGB(CTU),ColorToARGB(CFU), LinearGradientModeHorizontal);
      ggDiagonalForward: linGrBrush := TGPLinearGradientBrush.Create(MakeRect(r.Right-w2,r.Top,w,h),ColorToARGB(CTU),ColorToARGB(CFU), LinearGradientModeForwardDiagonal);
      ggDiagonalBackward: linGrBrush := TGPLinearGradientBrush.Create(MakeRect(r.Right-w2,r.Top,w,h),ColorToARGB(CTU),ColorToARGB(CFU), LinearGradientModeBackwardDiagonal);
      end;

      if GradientU = ggRadial then
      begin
        pthGrBrush.SetCenterPoint(MakePoint(r.Right, r.top + h2));

        // Set the color at the center point to blue.
        pthGrBrush.SetCenterColor(ColorToARGB(CTU));

        colors[0] := ColorToARGB(CFU);
        count := 1;
        pthGrBrush.SetSurroundColors(@colors, count);

        graphics.FillRectangle(pthGrBrush, r.Right - w2 + 1,r.Top + 1, w2 - 1, h - 1);
        pthGrBrush.Free;
      end
      else
      begin
        graphics.FillRectangle(linGrBrush, r.Right - w2, r.Top + 1, w2, h - 1);
        linGrBrush.Free;
      end;

      path.Free;

      // down ellips brush
      Canvas.Brush.Color := cfb;
      Canvas.FillRect(rect(r.Left , r.top, r.Left + w2, r.Bottom ));

      // Create a path that consists of a single ellipse.
      path := TGPGraphicsPath.Create;
      //  path.AddRectangle(MakeRect(r.Left, r.Top +  (h div 2), w , h));
      path.AddEllipse(r.Left - w2, r.Top, w , h);

      pthGrBrush := nil;
      linGrBrush := nil;

      case GradientB of
      ggRadial: pthGrBrush := TGPPathGradientBrush.Create(path);
      ggVertical: linGrBrush := TGPLinearGradientBrush.Create(MakeRect(r.Left,r.Top,w2+2,h),ColorToARGB(CTB),ColorToARGB(CFB), LinearGradientModeHorizontal);
      ggDiagonalForward: linGrBrush := TGPLinearGradientBrush.Create(MakeRect(r.Left,r.Top,w2,h),ColorToARGB(CTB),ColorToARGB(CFB), LinearGradientModeForwardDiagonal);
      ggDiagonalBackward: linGrBrush := TGPLinearGradientBrush.Create(MakeRect(r.Left,r.Top,w2,h),ColorToARGB(CTB),ColorToARGB(CFB), LinearGradientModeBackwardDiagonal);
      end;

      if GradientB = ggRadial then
      begin
        pthGrBrush.SetCenterPoint(MakePoint(r.Left, r.Top + h2));

        // Set the color at the center point to blue.
        pthGrBrush.SetCenterColor(ColorToARGB(CTB));

        colors[0] := ColorToARGB(CFB);
        count := 1;
        pthGrBrush.SetSurroundColors(@colors, count);
        graphics.FillRectangle(pthGrBrush, r.Left,r.Top, w2 + 1, h-1);
        pthGrBrush.Free;
      end
      else
      begin
        graphics.FillRectangle(linGrBrush, r.Left,r.Top, w2 + 2, h - 1);
        linGrBrush.Free;
      end;

      path.Free;

    end;
  end;


  gppen := tgppen.Create(ColorToARGB(PC),1);

  graphics.SetSmoothingMode(SmoothingModeAntiAlias);

  if (PC <> clNone) then
  begin
    if not RoundEdges then
      DrawRect(graphics, gppen,r.Left,r.Top, w - 1, h - 1)
    else
      DrawRoundRect(graphics, gppen,r.Left,r.Top, w - 1, h - 1, 3);
  end;

  gppen.Free;

  if Focus then
  begin
    gppen := tgppen.Create(ColorToARGB($E4AD89),1);
    graphics.SetSmoothingMode(SmoothingModeAntiAlias);
    DrawRoundRect(graphics, gppen,r.Left + 1,r.Top + 1, r.Right - 3, r.Bottom - 3, 3);
    gppen.Free;
    gppen := tgppen.Create(ColorToARGB(clgray),1);
    gppen.SetDashStyle(DashStyleDot);
    graphics.SetSmoothingMode(SmoothingModeAntiAlias);
    DrawRoundRect(graphics, gppen,r.Left + 2,r.Top + 2, r.Right - 5, r.Bottom - 5, 3);
    gppen.Free;
  end;

  fontFamily:= TGPFontFamily.Create(AFont.Name);

  fs := 0;

  ImgH := 0;
  ImgW := 0;
  ImgX := 0;
  ImgY := 0;

  if (fsBold in AFont.Style) then
    fs := fs + 1;
  if (fsItalic in AFont.Style) then
    fs := fs + 2;
  if (fsUnderline in AFont.Style) then
    fs := fs + 4;

  if Assigned(Picture) and not Picture.Empty then
  begin
    Picture.GetImageSizes;
    ImgW := Picture.Width;
    ImgH := Picture.Height;
  end
  else
  begin
    if (ImageIndex > -1) and Assigned(Images) then
    begin
      ImgW := Images.Width;
      ImgH := Images.Height;
    end;
  end;

  if (Caption <> '') then
  begin
    font := TGPFont.Create(fontFamily, AFont.Size , fs, UnitPoint);

    w := BtnR.Right - BtnR.Left;
    h := BtnR.Bottom - BtnR.Top;

    x1 := r.Left;
    y1 := r.Top;
    x2 := w;
    y2 := h;

    rectf := MakeRect(x1,y1,x2,y2);

    stringFormat := TGPStringFormat.Create;

    if Enabled then
      solidBrush := TGPSolidBrush.Create(ColorToARGB(AFont.Color))
    else
      solidBrush := TGPSolidBrush.Create(ColorToARGB(clGray));

    // Center-justify each line of text.
    stringFormat.SetAlignment(StringAlignmentCenter);

    // Center the block of text (top to bottom) in the rectangle.
    stringFormat.SetLineAlignment(StringAlignmentCenter);

    stringFormat.SetHotkeyPrefix(HotkeyPrefixShow);

    case AntiAlias of
    aaClearType:graphics.SetTextRenderingHint(TextRenderingHintClearTypeGridFit);
    aaAntiAlias:graphics.SetTextRenderingHint(TextRenderingHintAntiAlias);
    end;

    //graphics.MeasureString(Caption, Length(Caption), font, rectf, stringFormat, sizerect);
    if AntiAlias = aaNone then
    begin
      szRect.Left := round(rectf.X);
      szRect.Top := round(rectf.Y);

      szRect.Right := szRect.Left + 2;
      szRect.Bottom := DrawText(Canvas.Handle,PChar(Caption),Length(Caption), szrect, DT_CALCRECT or DT_LEFT or DT_WORDBREAK);

      sizeRect.X := szRect.Left;
      sizeRect.Y := szRect.Top;
      sizeRect.Width := szRect.Right - szRect.Left;
      sizeRect.Height := szRect.Bottom - szRect.Top;
    end
    else
      graphics.MeasureString(Caption, Length(Caption), font, rectf, stringFormat, sizerect);

    if (ImgW > 0) then
    begin
      case Layout of
        blGlyphLeft:
        begin
          x1 := r.Left + 2 + ImgW;
          x2 := w - 2 - ImgW;

          ImgX := round(sizerect.X - ImgW div 2);
          if ImgX < 2 then ImgX := 2;
          ImgY := r.Top + Max(0, (h - ImgH) div 2);
        end;
        blGlyphTop:
        begin
          y1 := r.Top{ + 2} + ImgH;
          y2 := h - 2 - ImgH;

          ImgX := r.Left + Max(0, (w - ImgW) div 2);
          ImgY := round(y2 - sizerect.Height);
          ImgY := Max(0, ImgY div 2);
          ImgY := round(y1) - ImgH + ImgY; //round(sizerect.Height) - ImgY - 4;
          if ImgY < 2 then ImgY := 2;
        end;
        blGlyphRight:
        begin
          x1 := 2;
          x2 := w - 4 - ImgW;

          ImgX := round(X2 - sizerect.width);
          ImgX := Max(0, ImgX div 2);
          ImgX := ImgX + round(sizerect.width) + 4;
          if ImgX > (w - ImgW) then
            ImgX := w - ImgW - 2;
          ImgY := r.Top + Max(0, (h - ImgH) div 2);
        end;
        blGlyphBottom:
        begin
          y1 := 2;
          y2 := h - 2 - ImgH;

          ImgX := r.Left + Max(0, (w - ImgW) div 2);
          ImgY := round(y2 - sizerect.Height);
          ImgY := Max(0, ImgY div 2);
          ImgY := round(sizerect.Height + 2) + ImgY;
          if ImgY > (h - ImgH) then ImgY := h - ImgH - 2;
        end;
      end;
    end;

    rectf := MakeRect(x1,y1,x2,y2);

    //graphics.DrawString(Caption, Length(Caption), font, rectf, stringFormat, solidBrush);
    if AntiAlias = aaNone then
    begin
      szRect.Left := round(rectf.X);
      szRect.Top := round(rectf.Y);
      szRect.Right := szRect.Left + round(rectf.Width);
      szRect.Bottom := szRect.Top + round(rectf.Height);
      Canvas.Brush.Style := bsClear;
      DrawText(Canvas.Handle,PChar(Caption),Length(Caption), szrect, DT_CENTER or DT_VCENTER or DT_SINGLELINE)
    end
    else
      graphics.DrawString(Caption, Length(Caption), font, rectf, stringFormat, solidBrush);

    stringformat.Free;
    font.Free;
  end;

  fontfamily.Free;

  if DropDownButton then
  begin

    if DropDownPos = dpRight then
      w := w - 8
    else
      h := h - 8;
  end;

  if Assigned(Picture) and not Picture.Empty then
  begin
     if Caption = '' then
       Canvas.Draw(r.Left + Max(0, (w - ImgW) div 2), r.Top + Max(0, (h - ImgH) div 2), Picture)
     else
       Canvas.Draw(ImgX, ImgY, Picture);
  end
  else
    if (ImageIndex <> -1) and Assigned(Images) then
    begin
      if Caption = '' then
        Images.Draw(Canvas, r.Left + Max(0, (w - Images.Width) div 2), r.Top + Max(0, (h - Images.Height) div 2), ImageIndex, EnabledImage)
      else
      begin
        Images.Draw(Canvas, ImgX, ImgY, ImageIndex, EnabledImage);
      end;
    end;


  Canvas.Brush.Style := bsClear;
  if DropDownButton then
  begin
    if DrawDwLine then
    begin
      Canvas.Pen.Color := PC;
      //Canvas.RoundRect(R.Left, R.Top, R.Right, R.Bottom, 6, 6);
      if (DropDownPos = dpRight) then
      begin
        Canvas.MoveTo(DwR.Left, DwR.Top);
        Canvas.LineTo(DwR.Left, DwR.Bottom);
      end
      else
      begin
        Canvas.MoveTo(DwR.Left, DwR.Top);
        Canvas.LineTo(DwR.Right, DwR.Top);
      end;
    end;
    AP.X := DwR.Left + ((DwR.Right - DwR.Left - 5) div 2);
    AP.Y := DwR.Top + ((DwR.Bottom - DwR.Top - 3) div 2) + 1;
    if not Enabled then
      DrawArrow(AP, clGray)
    else
      DrawArrow(AP, clBlack);
  end;

  if not Assigned(Graph) then
    graphics.Free;
end;


procedure DrawVistaGradient(Canvas: TCanvas; r: TRect; CFU, CTU, CFB, CTB, PC: TColor;
   GradientU,GradientB: TGDIPGradient; Caption:string; AFont: TFont; Enabled: Boolean; Focus: Boolean;
   AntiAlias: TAntiAlias; RoundEdges: Boolean; RotateLeftRight: Boolean; Direction: TTabPosition = tpTop; Graph: TGPGraphics = nil); overload;
begin
  DrawVistaGradient(Canvas, r, CFU, CTU, CFB, CTB, PC, GradientU,GradientB, Caption, AFont,
   nil, -1, True, blGlyphLeft, False, False, Enabled, Focus, dpRight, nil, AntiAlias, RoundEdges, RotateLeftRight, Direction, Graph);
end;

//------------------------------------------------------------------------------

{ TAdvCustomOfficeSelector }

constructor TAdvCustomOfficeSelector.Create(AOwner: TComponent);
begin
  inherited;

 { if not (csDesigning in ComponentState) then
  begin
    FDropDownWindow := TSelectorDropDownWindow.CreateNew(Self);
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
  }
  SetBounds(0, 0, 23, 22);
  ControlStyle := [csCaptureMouse, csDoubleClicks];
  ParentFont := True;
  Color := clBtnFace;

  Font.Name := 'Tahoma';

  FColorDropDown := $00F7F8F9;
  FColorDropDownTo := clNone;
  FColorDropDownFloating := RGB(196, 219, 249);
  FColorDropDownFloatingTo := clNone;
  FGradientDirection := gdVertical;
  FBorderDropDownColor := clGray;
  Flat := True;

  FSelectionAppearance := TSelectionAppearance.Create;
  FSelectionAppearance.OnChange := OnSelectionAppearanceChanged;

  FDragGripAppearance := TVistaBackgroundHot.Create;

  FCaptionAppearance := TGradientCaption.Create;

  FGripPosition := gpTop;

  FTools := TAdvSelectorItems.Create(Self);
  FSelectedIndex := -1;
  FDupSelectedIndex := -1;
  FAppHintPause := -1;

  FStyle := ssButton;
  State := absUp;
  FDropDownBtnWidth := 12;
  FButtonsPerRow := 1;

  FTwoColorImages := False;
  FOldForeGroundImgColor := clBlack;
  FOldBkGroundImgColor := clWhite;
  FForeGroundImageColor := clBlack;
  FBackGroundImageColor := clNone;

  FStretchImageDraw := True;
  ShowCaption := False;
  FDropDownTopMost := True;
end;

//------------------------------------------------------------------------------

destructor TAdvCustomOfficeSelector.Destroy;
begin
  FTools.Free;
  FSelectionAppearance.Free;
  FDragGripAppearance.Free;
  FCaptionAppearance.Free;


  {if not (csDesigning in ComponentState) then
  begin
    if Assigned(FSelectorPanel) then
      FSelectorPanel.Free;
    FDropDownWindow.Free;
  end;}

  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeSelector.Click;
begin
  inherited Click;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeSelector.CMFontChanged(var Message: TMessage);
begin
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeSelector.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if FFlat and not FMouseInControl and Enabled and (GetCapture = 0) then
  begin
    FMouseInControl := True;
    Repaint;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeSelector.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if FFlat and FMouseInControl and Enabled then
  begin
    FMouseInControl := False;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeSelector.CMTextChanged(var Message: TMessage);
begin
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeSelector.Loaded;
begin
  inherited Loaded;

  if FDupSelectedIndex < FTools.Count then
    SelectedIndex := FDupSelectedIndex;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeSelector.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);

  if (Button = mbLeft) then
  begin
    FMouseDown := true;

    case Style of
    ssButton:
      begin
        if DropDownButton and
          ( ((DropDownPosition = dpRight) and (X >= (width - FDropDownBtnWidth))) or
           ((DropDownPosition = dpBottom) and (Y >= (height - FDropDownBtnWidth))) or not DropDownSplit) then
        begin
          PopupBtnDown;
        end
        else
          ButtonDown;
      end;
    ssCombo:
      begin
        PopupBtnDown;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeSelector.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeSelector.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  FMouseDown := false;


  if (DropDownButton) and (Style = ssButton) and (x < Width - FDropDownBtnWidth) then
  begin
    if Assigned(OnClick) then
      OnClick(Self);
  end;
  Invalidate;

end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeSelector.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;
  if (AOperation = opRemove) and (AComponent = FToolImages) then
  begin
    FToolImages := nil;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

function TAdvCustomOfficeSelector.GetBtnRect: TRect;
var
  DwR: TRect;
begin
  Result := ClientRect;
  if DropDownPosition = dpRight then
  begin
    DwR := Rect(Result.Right - DropDownSectWidth, Result.Top, Result.Right, Result.Bottom);
    if DropDownButton then
      Result.Right := DwR.Left;
  end
  else // DropDownPos = doBottom
  begin
    DwR := Rect(Result.Left, Result.Bottom - DropDownSectWidth, Result.Right, Result.Bottom);
    if DropDownButton then
      Result.Bottom := DwR.Top;
  end;

end;

function TAdvCustomOfficeSelector.GetDroppedDown: boolean;
begin
  Result := false;
  if Assigned(FDropDownWindow) then
    Result := FDropDownWindow.Visible;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeSelector.CopyPicture(Pic: TGDIPPicture; bmp: TBitmap);
begin
  if Assigned(bmp) and Assigned(Pic) and not Pic.Empty then
  begin
    Pic.GetImageSizes;
    bmp.Height := Pic.Height;
    bmp.Width := Pic.Width;
    bmp.Canvas.Draw(0, 0, Pic);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeSelector.GetToolImage(bmp: TBitmap);
var
  bmp2: TBitmap;
  w: integer;

{$IFDEF DELPHI_UNICODE}
  procedure ClearBitmap32(ABitmap:TBitmap; Color: TColor; Alpha: Byte);
  type
     TColor32 = packed record
       case Integer of
         0: (B, G, R, A: Byte);
         1: (BGRA: UInt32);
       end;
     TColor32Array = array[0..0] of TColor32;
     PColor32Array = ^TColor32Array;
  var
     X, Y: Integer;
     R, G, B: Byte;
     Line: PColor32Array;
  begin
     ABitmap.AlphaFormat := afDefined;
     R := GetRValue(Color);
     G := GetGValue(Color);
     B := GetBValue(Color);
     for Y := 0 to ABitmap.Height-1 do
     begin
       Line := ABitmap.ScanLine[Y];
       for X := 0 to ABitmap.Width-1 do
       begin
         Line[X].R := R;
         Line[X].G := G;
         Line[X].B := B;
         Line[X].A := Alpha;
       end;
     end;
  end;
{$ENDIF}

begin
  if (not Assigned(Picture) or Picture.Empty) and (not Assigned(Images) or (ImageIndex < 0)) and (SelectedIndex >= 0) and Assigned(ToolImages) and Assigned(bmp) then
  begin
    w := Width - 3;
    if DropDownButton then
      w := w - 20;

    bmp.Width := Min(FToolImages.Width, w);
    bmp.Height := Min(FToolImages.Height, Height - 2);

{$IFDEF DELPHI_UNICODE}
    if FToolImages.ColorDepth = cd32Bit then
    begin
      bmp.PixelFormat := pf32bit;
      ClearBitmap32(bmp,clblack,0);

      if ((bmp.Width <> FToolImages.Width) or (bmp.Height <> FToolImages.Height)) and FStretchImageDraw then
      begin
        bmp2 := TBitmap.Create;
        bmp2.PixelFormat := pf32bit;
        bmp2.Width  := FToolImages.width;
        bmp2.Height := FToolImages.Height;
        ClearBitmap32(bmp2,clblack,0);
        FToolImages.Draw(bmp2.Canvas, 0, 0, Tools.Items[SelectedIndex].ImageIndex);
        bmp.Canvas.StretchDraw(Rect(0, 0, bmp.Width, bmp.Height), bmp2);
        bmp2.Free;
      end
      else
        ToolImages.Draw(bmp.Canvas, 0, 0, Tools.Items[SelectedIndex].ImageIndex);
    end
    else
{$ENDIF}
    begin
      bmp.Canvas.Brush.Color := clFuchsia;
      bmp.Canvas.FillRect(rect(0, 0, FToolImages.Width, FToolImages.Height));

      ToolImages.DrawingStyle := dsTransparent;
      if ((bmp.Width <> FToolImages.Width) or (bmp.Height <> FToolImages.Height)) and FStretchImageDraw then
      begin
        bmp2 := TBitmap.Create;
        bmp2.Width := FToolImages.Width;
        bmp2.Height := FToolImages.Height;
        ToolImages.Draw(bmp2.Canvas, 0, 0, Tools.Items[SelectedIndex].ImageIndex);
        bmp.Canvas.StretchDraw(Rect(0, 0, bmp.Width, bmp.Height), bmp2);
        bmp2.Free;
      end
      else
        ToolImages.Draw(bmp.Canvas, 0, 0, Tools.Items[SelectedIndex].ImageIndex);
      bmp.Transparent := True;
    end;
  end;
end;

procedure TAdvCustomOfficeSelector.GetToolPicture(var pic: TGDIPPicture);
begin
  if (SelectedIndex >= 0) then
  begin
    if not Tools[SelectedIndex].Picture.Empty then
      pic := Tools[SelectedIndex].Picture;
  end;
end;

//------------------------------------------------------------------------------

function TAdvCustomOfficeSelector.DrawGlyph(aGlyph: TBitMap; aRect: TRect): integer;
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

procedure TAdvCustomOfficeSelector.DrawGlyphAndCaption(Pic: TGDIPPicture;
  R: TRect);
var
  CapR: TRect;
  tbmp, tbmp2: TBitmap;
  Rgn: HRGN;
begin
  CapR := R;
  if Style = ssButton then
  begin
    if not Pic.Empty then
    begin
      tbmp2 := TBitmap.Create;
      CopyPicture(Pic, tbmp2);
      CapR.Left := DrawGlyph(tbmp2, R);
      tbmp2.Free;
    end
    else
    begin
      if (Caption = '') and (SelectedIndex >= 0) and Assigned(ToolImages) then
      begin
        tbmp := TBitmap.Create;
        tbmp.Width := FToolImages.width;
        tbmp.Height := FToolImages.Height;

        tbmp.Canvas.Brush.Color := clFuchsia;
        tbmp.Canvas.FillRect(rect(0, 0, FToolImages.Width, FToolImages.Height));

        FToolImages.DrawingStyle := dsTransparent;
        FToolImages.Draw(tbmp.Canvas, 0, 0, Tools.Items[SelectedIndex].ImageIndex);

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
      DrawText(Canvas.Handle, PChar(Caption), -1, CapR, DT_SINGLELINE or DT_VCENTER);
  end
  else if Style = ssCombo then
  begin
    if (SelectedIndex >= 0) and (Caption = '') and Assigned(FToolImages) then
    begin
      if Tools.Items[SelectedIndex].ImageIndex > -1 then
      begin
        tbmp := TBitmap.Create;
        tbmp.Width := FToolImages.Width; //((R.Right - R.Left)-6));
        tbmp.Height := FToolImages.Height; //min(FImages.Height, (R.Bottom - R.Top)-2);
        tbmp.Transparent := true;

        FToolImages.DrawingStyle := dsTransparent;
        FToolImages.Draw(tbmp.Canvas, 0, 0, Tools.Items[SelectedIndex].ImageIndex);

        Canvas.StretchDraw(Rect(R.Left + 2, R.Top + 2, R.Right - 2, R.Bottom - 2), tbmp);
        tbmp.Free;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeSelector.DrawComboButton;
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

begin
  R := Rect(0, 0, Width, Height);

  if Style = ssCombo then
  begin
    R2 := Rect(R.Left, R.Top, R.Right - FDropDownBtnWidth, R.Bottom);
    CapR := Rect(R.Left + 2, R.Top, R2.Right, R.Bottom);

    BtnR := Rect(R.Right - FDropDownBtnWidth - 1, R.Top + 1, R.Right - 1, R.Bottom - 1);

    AP.X := BtnR.Left + ((BtnR.Right - BtnR.Left - 5) div 2) + 1;
    AP.Y := BtnR.Top + ((BtnR.Bottom - BtnR.Top - 3) div 2) + 1;

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

          if Appearance.ColorHotTo{ColorHotTo} <> clNone then
            DrawGradient(Canvas, Appearance.ColorHot{ColorHot}, Appearance.ColorHotTo{ColorHotTo}, 16, BtnR, false)
          else
          begin
            Canvas.Pen.Color := Appearance.ColorHot; //ColorHot;
            Canvas.Brush.Color := Appearance.ColorHot; //ColorHot;
            Canvas.Rectangle(BtnR.Left, BtnR.Top, BtnR.Right, BtnR.Bottom);
          end;

          Canvas.Brush.Style := bsClear;

          if Appearance.BorderColorHot{BorderHotColor} <> clNone then
          begin
            Canvas.Pen.Color := Appearance.BorderColorHot; //BorderHotColor;
            Canvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
          end;

          if Enabled or DisabledPicture.Empty then
            DrawGlyphAndCaption(Picture, CapR)
          else
            DrawGlyphAndCaption(DisabledPicture, CapR);

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

        if Appearance.ColorTo{ColorTo} <> clNone then
          DrawGradient(Canvas, Appearance.Color{Color}, Appearance.ColorTo{ColorTo}, 16, Rect(BtnR.Left, BtnR.Top, BtnR.Right, BtnR.Bottom - 1), false)
        else
        begin
          Canvas.Pen.Color := Appearance.Color;//Color;
          Canvas.Brush.Color := Appearance.Color; //Color;
          Canvas.Rectangle(BtnR.Left, BtnR.Top, BtnR.Right, BtnR.Bottom);
        end;

        Canvas.Brush.Style := bsClear;
        if Appearance.BorderColor{BorderColor} <> clNone then
        begin
          Canvas.Pen.Color := Appearance.BorderColor{BorderColor};
          Canvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
          Canvas.MoveTo(BtnR.Left, R.Top);
          Canvas.LineTo(BtnR.Left, R.Bottom);
        end;

        if Enabled or DisabledPicture.Empty then
          DrawGlyphAndCaption(Picture, CapR)
        else
          DrawGlyphAndCaption(DisabledPicture, CapR);

        DrawArrow(AP, clBlack);
      end;

    end
    else if State = absDropDown then
    begin // DropDown State

      Canvas.Pen.Color := clWhite;
      Canvas.Brush.Color := clWhite;
      Canvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);

      if Appearance.ColorDownTo{ColorDownTo} <> clNone then
        DrawGradient(Canvas, Appearance.ColorDown{ColorDown}, Appearance.ColorDownTo{ColorDownTo}, 16, BtnR, false)
      else
      begin
        Canvas.Pen.Color := Appearance.ColorDown; //ColorDown;
        Canvas.Brush.Color := Appearance.ColorDown; //ColorDown;
        Canvas.Rectangle(BtnR.Left, BtnR.Top, BtnR.Right, BtnR.Bottom);
      end;

      Canvas.Brush.Style := bsClear;

      if Appearance.BorderColorDown{BorderDownColor} <> clNone then
      begin
        Canvas.Pen.Color := Appearance.BorderColorDown{BorderDownColor};
        Canvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
        Canvas.MoveTo(BtnR.Left, R.Top);
        Canvas.LineTo(BtnR.Left, R.Bottom);
      end;

      if Enabled or DisabledPicture.Empty then
        DrawGlyphAndCaption(Picture, CapR)
      else
        DrawGlyphAndCaption(DisabledPicture, CapR);

      DrawArrow(AP, clBlack);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeSelector.DoSelect(Index: integer; Item: TAdvSelectorItem);
begin
  if Assigned(Action) then
    Action.Execute;

  if Assigned(FOnSelect) then
    FOnSelect(Self, Index, Item);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeSelector.Paint;
begin
  if Style in [ssButton {, ssCheck}] then
  begin
    //Canvas.Font := Self.Font;
    if not FFlat then
    begin

    end
    else
    begin
      //DrawButton;
      inherited;
    end;
  end
  else //Style = ssCombo
  begin
    DrawComboButton;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeSelector.ThemeAdapt;
//var
  //eTheme: XPColorScheme;
begin
  {eTheme := CurrentXPTheme();
  case eTheme of
    xpBlue: AppearanceStyle := esOffice2003Blue;
    xpGreen: AppearanceStyle := esOffice2003Olive;
    xpGray: AppearanceStyle := esOffice2003Silver;
  else
    AppearanceStyle := esOffice2003Classic;
  end;}
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeSelector.WndProc(var Message: TMessage);
begin
  {if (Message.Msg = WM_DESTROY) and not (csDesigning in ComponentState) then
  begin
    if Assigned(FDropDownWindow) then
    begin
      if Assigned(FDropDownWindow.SelectorPanel) and (FDropDownWindow.SelectorPanel.Parent = FDropDownWindow) then
        FDropDownWindow.SelectorPanel.Parent := nil;
      FDropDownWindow.Free;
    end;
  end;}

  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeSelector.SetFlat(const Value: Boolean);
begin
  if Value <> FFlat then
  begin
    FFlat := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeSelector.SetToolImages(const Value: TCustomImageList);
begin
  if Value <> FToolImages then
  begin
    FToolImages := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeSelector.SetStyleEx(const Value: TAdvSelectorStyle);
begin
  FStyle := Value;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeSelector.BlockDropDown(const Value: boolean);
begin
  FDropDownWindow.BlockHide := Value;
end;

procedure TAdvCustomOfficeSelector.ButtonDown;
begin
  if not DropDownButton then
    DoDropDown
  else
  begin
    if Assigned(FSelectorPanel) then
    begin
      if (FSelectorPanel.ItemIndex >= 0) and (FSelectorPanel.ItemIndex < Tools.Count) then
        DoSelect(FSelectorPanel.ItemIndex, Tools[FSelectorPanel.ItemIndex])
      else
        DoSelect(FSelectorPanel.ItemIndex, nil)
    end
    else
    begin
    //  if SelectedIndex >= 0 then
    //    DoSelect(SelectedIndex, Tools[SelectedIndex])
    //  else
    //    DoSelect(SelectedIndex, nil);
    end;
  end;
end;

//------------------------------------------------------------------------------

function TAdvCustomOfficeSelector.DoCanDropDown: boolean;
begin
  Result := true;
  if Assigned(OnCanDropDown) then
    OnCanDropDown(Self, Result);
end;
//------------------------------------------------------------------------------

procedure TAdvCustomOfficeSelector.DoDropDown;
var
  R: TRect;
  P: TPoint;
  mon: TMonitor;
begin
  if not Assigned(FDropDownWindow) then
  begin
    FDropDownWindow := TSelectorDropDownWindow.CreateNew(Self);
    FDropDownWindow.BorderIcons := [];
    FDropDownWindow.BorderStyle := bsNone;
    FDropDownWindow.Ctl3D := false;
    if DropDownTopMost then
      FDropDownWindow.FormStyle := fsStayOnTop;
    FDropDownWindow.Visible := False;
    FDropDownWindow.Width := 100;
    FDropDownWindow.Height := 100;
    FDropDownWindow.AutoScroll := true;
    FDropDownWindow.BorderWidth := 0;
    FDropDownWindow.DefaultMonitor := dmDesktop;
    FDropDownWindow.OnHide := OnDropDownWindowHide;
    FDropDownWindow.OnClose := DropDownWindowClose;
  end;

  if FDropDownWindow.Visible then
  begin
    if Assigned(FDropDownWindow.SelectorPanel) and FDropDownWindow.SelectorPanel.FFloating then
    begin
      //HideDropDown
      FDropDownWindow.SelectorPanel.SetFloating(False);
    end
    else
    begin
      KeepDown := false;
      HideDropDown;
      Exit;
    end;
  end;

  if not DoCanDropDown then
    Exit;

  FAppHintPause := Application.HintPause;
  Application.HintPause := -1;

  SetSelectorPanel;
  FDropDownWindow.Position := poDesigned;
  FDropDownWindow.SetWindowSize;

  P := Point(0, self.Height);
  P := ClientToScreen(P);

  {$IFDEF DELPHI6_LVL}
  mon := Screen.MonitorFromPoint(p);
  if Assigned(mon) then
    R := mon.WorkAreaRect
  else
  {$ENDIF}
  begin
  SystemParametersInfo(SPI_GETWORKAREA, 0, @R, 0);
  end;

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

  FDropDownWindow.ShowFullBorder := True;  //Style = ssCombo;

  State := absDropDown;

  KeepDown := true;

  //SetDroppedDown(True);

  FDropDownWindow.Visible := true;
  FDropDownWindow.SetFocus;

  if Assigned(FOnDropDown) then
    FOnDropDown(Self);


  Invalidate;

end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeSelector.HideDropDown;
begin
  if Assigned(FDropDownWindow) then
  begin
    KeepDown := false;
    if FAppHintPause <> -1 then
      Application.HintPause := FAppHintPause;
    FAppHintPause := -1;

    FDropDownWindow.Visible := false;
  end;
end;

function TAdvCustomOfficeSelector.IsDropDown: Boolean;
begin
  Result := true;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeSelector.PopupBtnDown;
begin
  DoDropDown;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeSelector.SetColorDropDown(const Value: TColor);
begin
  FColorDropDown := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeSelector.SetColorDropDownTo(const Value: TColor);
begin
  FColorDropDownTo := Value;
end;

procedure TAdvCustomOfficeSelector.SetComponentStyle(AStyle: TTMSStyle);
begin
  //inherited;

  if AStyle <> tsCustom then
  begin
    ColorDropDown := $00F7F8F9;
    ColorDropDownTo := clNone;
    ColorDropDownFloating := $00F9DBC4;
    ColorDropDownFloatingTo := clNone;
    GradientDirection := gdHorizontal;
  end;

  SelectionAppearance.Rounded := False;
  Rounded := true;

  if (Astyle in [tsOffice2003Blue, tsOffice2003Silver, tsOffice2003Olive, tsWhidbey]) then
  begin
    BorderDropDownColor := clGray;

    Appearance.ColorHot := $EBFDFF;
    Appearance.ColorHotTo := $ACECFF;
    Appearance.ColorMirrorHot := $59DAFF;
    Appearance.ColorMirrorHotTo := $A4E9FF;
    Appearance.BorderColorHot := $99CEDB;
    Appearance.GradientHot := ggVertical;
    Appearance.GradientMirrorHot := ggVertical;

    Appearance.ColorDown := $76AFF1;
    Appearance.ColorDownTo := $4190F3;
    Appearance.ColorMirrorDown := $0E72F1;
    Appearance.ColorMirrorDownTo := $4C9FFD;
    Appearance.BorderColorDown := $45667B;
    Appearance.GradientDown := ggVertical;
    Appearance.GradientMirrorDown := ggVertical;

    Appearance.ColorChecked := $B5DBFB;
    Appearance.ColorCheckedTo := $78C7FE;
    Appearance.ColorMirrorChecked := $9FEBFD;
    Appearance.ColorMirrorCheckedTo := $56B4FE;
    Appearance.GradientChecked := ggVertical;
    Appearance.GradientMirrorChecked := ggVertical;

    SelectionAppearance.Color := $ACECFF;
    SelectionAppearance.ColorTo := $ACECFF;
    SelectionAppearance.ColorMirror := $ACECFF;
    SelectionAppearance.ColorMirrorTo := $ACECFF;

    SelectionAppearance.ColorHot := $ACECFF;
    SelectionAppearance.ColorHotTo := $ACECFF;
    SelectionAppearance.ColorMirrorHot := $ACECFF;
    SelectionAppearance.ColorMirrorHotTo := $ACECFF;
    SelectionAppearance.BorderColorHot := $0099CEDB;
    SelectionAppearance.GradientHot := ggVertical;
    SelectionAppearance.GradientMirrorHot := ggVertical;
    SelectionAppearance.TextColorHot := clBlack;

    SelectionAppearance.ColorDown := $4190F3;
    SelectionAppearance.ColorDownTo := $4190F3;
    SelectionAppearance.ColorMirrorDown := $4190F3;
    SelectionAppearance.ColorMirrorDownTo := $4190F3;
    SelectionAppearance.BorderColorDown := $45667B;
    SelectionAppearance.GradientDown := ggVertical;
    SelectionAppearance.GradientMirrorDown := ggVertical;
    SelectionAppearance.TextColorDown := clBlack;

    SelectionAppearance.ColorChecked := $78C7FE;
    SelectionAppearance.ColorCheckedTo := $78C7FE;
    SelectionAppearance.ColorMirrorChecked := $78C7FE;
    SelectionAppearance.ColorMirrorCheckedTo := $78C7FE;
    SelectionAppearance.BorderColorChecked := $45667B;
    SelectionAppearance.TextColorChecked := clBlack;
    SelectionAppearance.GradientChecked := ggVertical;
    SelectionAppearance.GradientMirrorChecked := ggVertical;

    CaptionAppearance.TextColor := clWhite;

    {
    CaptionAppearance.ButtonAppearance.Color := $EEDBC8;
    CaptionAppearance.ButtonAppearance.ColorTo := $F6DDC9;
    CaptionAppearance.ButtonAppearance.ColorMirror := $EDD4C0;
    CaptionAppearance.ButtonAppearance.ColorMirrorTo := $F7E1D0;
    CaptionAppearance.ButtonAppearance.BorderColor := $E0B99B;
    CaptionAppearance.ButtonAppearance.Gradient := ggVertical;
    CaptionAppearance.ButtonAppearance.GradientMirror := ggVertical;

    CaptionAppearance.ButtonAppearance.ColorHot := $EBFDFF;
    CaptionAppearance.ButtonAppearance.ColorHotTo := $ACECFF;
    CaptionAppearance.ButtonAppearance.ColorMirrorHot := $59DAFF;
    CaptionAppearance.ButtonAppearance.ColorMirrorHotTo := $A4E9FF;
    CaptionAppearance.ButtonAppearance.BorderColorHot := $99CEDB;
    CaptionAppearance.ButtonAppearance.GradientHot := ggVertical;
    CaptionAppearance.ButtonAppearance.GradientMirrorHot := ggVertical;

    CaptionAppearance.ButtonAppearance.ColorDown := $76AFF1;
    CaptionAppearance.ButtonAppearance.ColorDownTo := $4190F3;
    CaptionAppearance.ButtonAppearance.ColorMirrorDown := $0E72F1;
    CaptionAppearance.ButtonAppearance.ColorMirrorDownTo := $4C9FFD;
    CaptionAppearance.ButtonAppearance.BorderColorDown := $45667B;
    CaptionAppearance.ButtonAppearance.GradientDown := ggVertical;
    CaptionAppearance.ButtonAppearance.GradientMirrorDown := ggVertical;

    CaptionAppearance.ButtonAppearance.ColorChecked := $B5DBFB;
    CaptionAppearance.ButtonAppearance.ColorCheckedTo := $78C7FE;
    CaptionAppearance.ButtonAppearance.ColorMirrorChecked := $9FEBFD;
    CaptionAppearance.ButtonAppearance.ColorMirrorCheckedTo := $56B4FE;
    CaptionAppearance.ButtonAppearance.GradientChecked := ggVertical;
    CaptionAppearance.ButtonAppearance.GradientMirrorChecked := ggVertical;
    }

    CaptionAppearance.ButtonAppearance.ColorHot := $C2EEFF;
    CaptionAppearance.ButtonAppearance.ColorHotTo := $C2EEFF;
    CaptionAppearance.ButtonAppearance.ColorMirrorHot := $C2EEFF;
    CaptionAppearance.ButtonAppearance.ColorMirrorHotTo := $C2EEFF;
    CaptionAppearance.ButtonAppearance.BorderColorHot := clBlack;
    CaptionAppearance.ButtonAppearance.GradientHot := ggVertical;
    CaptionAppearance.ButtonAppearance.GradientMirrorHot := ggVertical;
    CaptionAppearance.TextColorHot := clBlack;


    CaptionAppearance.ButtonAppearance.ColorDown := $4190F3;
    CaptionAppearance.ButtonAppearance.ColorDownTo := $4190F3;
    CaptionAppearance.ButtonAppearance.ColorMirrorDown := $4190F3;
    CaptionAppearance.ButtonAppearance.ColorMirrorDownTo := $4190F3;
    CaptionAppearance.ButtonAppearance.BorderColorDown := clBlack;
    CaptionAppearance.TextColorDown := clBlack;


    DragGripAppearance.ColorHot := $C2EEFF;
    DragGripAppearance.ColorHotTo := $C2EEFF;
    DragGripAppearance.ColorMirrorHot := $C2EEFF;
    DragGripAppearance.ColorMirrorHotTo := $C2EEFF;
    DragGripAppearance.BorderColorHot := clBlack;

    ColorDropDown := $EEF4F4;
    ColorDropDownTo := clNone;
  end;

  case AStyle of
    tsOffice2003Blue:
      begin
        Appearance.Color := $EEDBC8;
        Appearance.ColorTo := $F6DDC9;
        Appearance.ColorMirror := $EDD4C0;
        Appearance.ColorMirrorTo := $F7E1D0;
        Appearance.BorderColor := $E0B99B;
        Appearance.Gradient := ggVertical;
        Appearance.GradientMirror := ggVertical;

        CaptionAppearance.Color := $00C9662A;

        DragGripAppearance.Color := $F0C7A9;
        DragGripAppearance.ColorTo := $F0C7A9;
        DragGripAppearance.ColorMirror := $F0C7A9;
        DragGripAppearance.ColorMirrorTo := $F0C7A9;


      end;
    tsOffice2003Olive:
      begin
        Appearance.Color := $CFF0EA;
        Appearance.ColorTo := $CFF0EA;
        Appearance.ColorMirror := $CFF0EA;
        Appearance.ColorMirrorTo := $8CC0B1;
        Appearance.BorderColor := $8CC0B1;
        Appearance.Gradient := ggVertical;
        Appearance.GradientMirror := ggVertical;

        CaptionAppearance.Color := $8CC0B1;

        DragGripAppearance.Color := $9FD4C5;
        DragGripAppearance.ColorTo := $9FD4C5;
        DragGripAppearance.ColorMirror := $9FD4C5;
        DragGripAppearance.ColorMirrorTo := $9FD4C5;
        DragGripAppearance.BorderColor := $9FD4C5;

        ColorDropDownFloating :=  $ADDED1;
        ColorDropDownFloatingTo := clNone;
      end;
    tsOffice2003Silver:
      begin
        Appearance.Color := $EDD4C0;
        Appearance.ColorTo := $00E6D8D8;
        Appearance.ColorMirror := $EDD4C0;
        Appearance.ColorMirrorTo := $C8B2B3;
        Appearance.BorderColor := $927476;
        Appearance.Gradient := ggVertical;
        Appearance.GradientMirror := ggVertical;

        CaptionAppearance.Color := $99797A;

        DragGripAppearance.Color := $D3C0C0;
        DragGripAppearance.ColorTo := $D3C0C0;
        DragGripAppearance.ColorMirror := $D3C0C0;
        DragGripAppearance.ColorMirrorTo := $D3C0C0;

        ColorDropDown := $FFFAFD;
        ColorDropDownTo := clNone;

        ColorDropDownFloating := $E4DADB;
        ColorDropDownFloatingTo := clNone;
      end;
    tsOffice2003Classic:
      begin
        Appearance.Color := clWhite;
        Appearance.ColorTo := $C9D1D5;
        Appearance.ColorMirror := clWhite;
        Appearance.ColorMirrorTo := $C9D1D5;
        Appearance.BorderColor := clBlack;
        Appearance.Gradient := ggVertical;
        Appearance.GradientMirror := ggVertical;

        Appearance.ColorHot := $EBFDFF;
        Appearance.ColorHotTo := $ACECFF;
        Appearance.ColorMirrorHot := $59DAFF;
        Appearance.ColorMirrorHotTo := $A4E9FF;
        Appearance.BorderColorHot := $99CEDB;
        Appearance.GradientHot := ggVertical;
        Appearance.GradientMirrorHot := ggVertical;

        Appearance.ColorDown := $76AFF1;
        Appearance.ColorDownTo := $4190F3;
        Appearance.ColorMirrorDown := $0E72F1;
        Appearance.ColorMirrorDownTo := $4C9FFD;
        Appearance.BorderColorDown := $45667B;
        Appearance.GradientDown := ggVertical;
        Appearance.GradientMirrorDown := ggVertical;

        Appearance.ColorChecked := $B5DBFB;
        Appearance.ColorCheckedTo := $78C7FE;
        Appearance.ColorMirrorChecked := $9FEBFD;
        Appearance.ColorMirrorCheckedTo := $56B4FE;
        Appearance.GradientChecked := ggVertical;
        Appearance.GradientMirrorChecked := ggVertical;

        CaptionAppearance.Color := $808080;
        CaptionAppearance.TextColor := clWhite;

        DragGripAppearance.Color := $D1D8DB;
        DragGripAppearance.ColorTo := $D1D8DB;
        DragGripAppearance.ColorMirror := $D1D8DB;
        DragGripAppearance.ColorMirrorTo := $D1D8DB;
        DragGripAppearance.BorderColor := $D1D8DB;

        DragGripAppearance.ColorHot := $D2BDB6;
        DragGripAppearance.ColorHotTo := $D2BDB6;
        DragGripAppearance.ColorMirrorHot := $D2BDB6;
        DragGripAppearance.ColorMirrorHotTo := $D2BDB6;
        DragGripAppearance.BorderColorHot := clBlack;

        ColorDropDown := $F7F8F9;
        ColorDropDownTo := clNone;

        ColorDropDownFloating := $D1D8DB;
        ColorDropDownFloatingTo := clNone;

        SelectionAppearance.Color := $D8D5D4;
        SelectionAppearance.ColorTo := $D8D5D4;
        SelectionAppearance.ColorMirror := $D8D5D4;
        SelectionAppearance.ColorMirrorTo := $D8D5D4;

        SelectionAppearance.ColorHot := $D2BDB6;
        SelectionAppearance.ColorHotTo := $D2BDB6;
        SelectionAppearance.ColorMirrorHot := $D2BDB6;
        SelectionAppearance.ColorMirrorHotTo := $D2BDB6;
        SelectionAppearance.BorderColorHot := clBlack;
        SelectionAppearance.GradientHot := ggVertical;
        SelectionAppearance.GradientMirrorHot := ggVertical;
        SelectionAppearance.TextColorHot := clBlack;

        SelectionAppearance.ColorDown := $B59285;
        SelectionAppearance.ColorDownTo := $B59285;
        SelectionAppearance.ColorMirrorDown := $B59285;
        SelectionAppearance.ColorMirrorDownTo := $B59285;
        SelectionAppearance.BorderColorDown := $B59285;
        SelectionAppearance.GradientDown := ggVertical;
        SelectionAppearance.GradientMirrorDown := ggVertical;
        SelectionAppearance.TextColorDown := clBlack;

        SelectionAppearance.ColorChecked := $D8D5D4;
        SelectionAppearance.ColorCheckedTo := $D8D5D4;
        SelectionAppearance.ColorMirrorChecked := $D8D5D4;
        SelectionAppearance.ColorMirrorCheckedTo := $D8D5D4;
        SelectionAppearance.BorderColorChecked := clBlack;
        SelectionAppearance.TextColorChecked := clBlack;
        SelectionAppearance.GradientChecked := ggVertical;
        SelectionAppearance.GradientMirrorChecked := ggVertical;

        CaptionAppearance.TextColor := clWhite;

        CaptionAppearance.ButtonAppearance.ColorHot := $D2BDB6;
        CaptionAppearance.ButtonAppearance.ColorHotTo := $D2BDB6;
        CaptionAppearance.ButtonAppearance.ColorMirrorHot := $D2BDB6;
        CaptionAppearance.ButtonAppearance.ColorMirrorHotTo := $D2BDB6;
        CaptionAppearance.ButtonAppearance.BorderColorHot := clBlack;
        CaptionAppearance.ButtonAppearance.GradientHot := ggVertical;
        CaptionAppearance.ButtonAppearance.GradientMirrorHot := ggVertical;
        CaptionAppearance.TextColorHot := clBlack;

        CaptionAppearance.ButtonAppearance.ColorDown := $B59285;
        CaptionAppearance.ButtonAppearance.ColorDownTo := $B59285;
        CaptionAppearance.ButtonAppearance.ColorMirrorDown := $B59285;
        CaptionAppearance.ButtonAppearance.ColorMirrorDownTo := $B59285;
        CaptionAppearance.ButtonAppearance.BorderColorDown := clBlack;
        CaptionAppearance.TextColorDown := clBlack;
      end;
    tsOffice2007Luna:
      begin
        BorderDropDownColor := clSilver;

        Appearance.Color := $EEDBC8;
        Appearance.ColorTo := $F6DDC9;
        Appearance.ColorMirror := $EDD4C0;
        Appearance.ColorMirrorTo := $F7E1D0;
        Appearance.BorderColor := $E0B99B;
        Appearance.Gradient := ggVertical;
        Appearance.GradientMirror := ggVertical;

        Appearance.ColorHot := $EBFDFF;
        Appearance.ColorHotTo := $ACECFF;
        Appearance.ColorMirrorHot := $59DAFF;
        Appearance.ColorMirrorHotTo := $A4E9FF;
        Appearance.BorderColorHot := $99CEDB;
        Appearance.GradientHot := ggVertical;
        Appearance.GradientMirrorHot := ggVertical;

        Appearance.ColorDown := $76AFF1;
        Appearance.ColorDownTo := $4190F3;
        Appearance.ColorMirrorDown := $0E72F1;
        Appearance.ColorMirrorDownTo := $4C9FFD;
        Appearance.BorderColorDown := $45667B;
        Appearance.GradientDown := ggVertical;
        Appearance.GradientMirrorDown := ggVertical;

        Appearance.ColorChecked := $B5DBFB;
        Appearance.ColorCheckedTo := $78C7FE;
        Appearance.ColorMirrorChecked := $9FEBFD;
        Appearance.ColorMirrorCheckedTo := $56B4FE;
        Appearance.BorderColorChecked := $45667B;
        Appearance.GradientChecked := ggVertical;
        Appearance.GradientMirrorChecked := ggVertical;

        SelectionAppearance.Color := $EBFDFF;
        SelectionAppearance.ColorTo := $ABEBFF;
        SelectionAppearance.ColorMirror := $69D6FF;
        SelectionAppearance.ColorMirrorTo := $96E4FF;

        SelectionAppearance.ColorHot := $EBFDFF;
        SelectionAppearance.ColorHotTo := $ABEBFF;
        SelectionAppearance.ColorMirrorHotTo := $ABEBFF;
        SelectionAppearance.ColorMirrorHotTo := $96E4FF;
        SelectionAppearance.BorderColorHot := $0099CEDB;
        SelectionAppearance.GradientHot := ggVertical;
        SelectionAppearance.GradientMirrorHot := ggVertical;
        SelectionAppearance.TextColorHot := clBlack;

        SelectionAppearance.ColorDown := $76AFF1;
        SelectionAppearance.ColorDownTo := $4190F3;
        SelectionAppearance.ColorMirrorDown := $0E72F1;
        SelectionAppearance.ColorMirrorDownTo := $4C9FFD;
        SelectionAppearance.BorderColorDown := $45667B;
        SelectionAppearance.GradientDown := ggVertical;
        SelectionAppearance.GradientMirrorDown := ggVertical;
        SelectionAppearance.TextColorDown := clBlack;

        SelectionAppearance.ColorChecked := $B5DBFB;
        SelectionAppearance.ColorCheckedTo := $78C7FE;
        SelectionAppearance.ColorMirrorChecked := $78C7FE;
        //SelectionAppearance.ColorMirrorChecked := $9FEBFD;
        SelectionAppearance.ColorMirrorCheckedTo := $56B4FE;
        SelectionAppearance.BorderColorChecked := $45667B;
        SelectionAppearance.TextColorChecked := clBlack;
        SelectionAppearance.GradientChecked := ggVertical;
        SelectionAppearance.GradientMirrorChecked := ggVertical;
        SelectionAppearance.Rounded := True;

        CaptionAppearance.Color := $00F2DAC2;
        CaptionAppearance.TextColor := $6E163E;

        DragGripAppearance.BorderColorHot := clBlue;

        DragGripAppearance.Color := $00F5F0E1;
        DragGripAppearance.ColorTo := $00F9D2B2;
        DragGripAppearance.ColorMirror := $00F5C8AD;
        DragGripAppearance.ColorMirrorTo := $00FFF8F4;

        DragGripAppearance.ColorHot := $00EBFDFF;
        DragGripAppearance.ColorHotTo := $00ACECFF;
        DragGripAppearance.ColorMirrorHot := $0059DAFF;
        DragGripAppearance.ColorMirrorHotTo := $00A4E9FF;

        ColorDropDownFloating := $00FEF6F0;
        ColorDropDownFloatingTo := clNone;

        ColorDropDown := $F7F8F9;
        ColorDropDownTo := clNone;

      end;
    tsOffice2007Obsidian:
      begin
        BorderDropDownColor := clSilver;

        SelectionAppearance.Rounded := True;

        Appearance.Color := $DFDED6;
        Appearance.ColorTo := $E4E2DB;
        Appearance.ColorMirror := $D7D5CE;
        Appearance.ColorMirrorTo := $E7E5E0;
        Appearance.BorderColor := $C0BCB2;
        Appearance.Gradient := ggVertical;
        Appearance.GradientMirror := ggVertical;

        Appearance.ColorHot := $EBFDFF;
        Appearance.ColorHotTo := $ACECFF;
        Appearance.ColorMirrorHot := $59DAFF;
        Appearance.ColorMirrorHotTo := $A4E9FF;
        Appearance.BorderColorHot := $99CEDB;
        Appearance.GradientHot := ggVertical;
        Appearance.GradientMirrorHot := ggVertical;

        Appearance.ColorDown := $76AFF1;
        Appearance.ColorDownTo := $4190F3;
        Appearance.ColorMirrorDown := $0E72F1;
        Appearance.ColorMirrorDownTo := $4C9FFD;
        Appearance.BorderColorDown := $45667B;
        Appearance.GradientDown := ggVertical;
        Appearance.GradientMirrorDown := ggVertical;

        Appearance.ColorChecked := $B5DBFB;
        Appearance.ColorCheckedTo := $78C7FE;
        Appearance.ColorMirrorChecked := $9FEBFD;
        Appearance.ColorMirrorCheckedTo := $56B4FE;
        Appearance.BorderColorChecked := $45667B;
        Appearance.GradientChecked := ggVertical;
        Appearance.GradientMirrorChecked := ggVertical;

        CaptionAppearance.Color := $00B8B8B6;
        CaptionAppearance.TextColor := $6E163E;

        DragGripAppearance.BorderColorHot := clBlue;

        DragGripAppearance.Color := $00DFDED6;
        DragGripAppearance.ColorTo := $00E4E2DB;
        DragGripAppearance.ColorMirror := $00D7D5CE;
        DragGripAppearance.ColorMirrorTo := $00E7E5E0;

        DragGripAppearance.ColorHot := $00EBFDFF;
        DragGripAppearance.ColorHotTo := $00ACECFF;
        DragGripAppearance.ColorMirrorHot := $0059DAFF;
        DragGripAppearance.ColorMirrorHotTo := $00A4E9FF;
        DragGripAppearance.BorderColorHot := clBlack;

        ColorDropDownFloating := $00ECECE5;
        ColorDropDownFloatingTo := clNone;

        ColorDropDown := $F7F8F9;
        ColorDropDownTo := clNone;


      end;
    tsWindowsXP:
      begin
        BorderDropDownColor := clGray;

        Appearance.Color := clWhite;
        Appearance.ColorTo := $B9D8DC;
        Appearance.ColorMirror := $B9D8DC;
        Appearance.ColorMirrorTo := $B9D8DC;
        Appearance.BorderColor := $B9D8DC;
        Appearance.Gradient := ggVertical;
        Appearance.GradientMirror := ggVertical;

        Appearance.ColorHot := $EFD3C6;
        Appearance.ColorHotTo := $EFD3C6;
        Appearance.ColorMirrorHot := $EFD3C6;
        Appearance.ColorMirrorHotTo := $EFD3C6;
        Appearance.BorderColorHot := clHighlight;
        Appearance.GradientHot := ggVertical;
        Appearance.GradientMirrorHot := ggVertical;

        Appearance.ColorDown := $B59284;
        Appearance.ColorDownTo := $B59284;
        Appearance.ColorMirrorDown := $B59284;
        Appearance.ColorMirrorDownTo := $B59284;
        Appearance.BorderColorDown := clHighlight;
        Appearance.GradientDown := ggVertical;
        Appearance.GradientMirrorDown := ggVertical;


        Appearance.ColorChecked := $B9D8DC;
        Appearance.ColorCheckedTo := $B9D8DC;
        Appearance.ColorMirrorChecked := $B9D8DC;
        Appearance.ColorMirrorCheckedTo := $B9D8DC;
        Appearance.BorderColorChecked := clBlack;
        Appearance.GradientChecked := ggVertical;
        Appearance.GradientMirrorChecked := ggVertical;

        CaptionAppearance.Color := $00C9662A;
        CaptionAppearance.TextColor := clWhite;

        DragGripAppearance.BorderColorHot := clBlue;
        DragGripAppearance.Color := clWhite;
        DragGripAppearance.ColorTo := clWhite;
        DragGripAppearance.ColorHot := clHighlight;
        DragGripAppearance.ColorHotTo := clHighlight;
        DragGripAppearance.ColorMirror := clWhite;
        DragGripAppearance.ColorMirrorTo := clWhite;
        DragGripAppearance.ColorMirrorHot := clHighlight;
        DragGripAppearance.ColorMirrorHotTo := clHighlight;        

      end;
    tsWhidbey:
      begin
        BorderDropDownColor := clGray;

        Appearance.Color := clWhite;
        Appearance.ColorTo := $DFEDF0;
        Appearance.ColorMirror := $DFEDF0;
        Appearance.ColorMirrorTo := $DFEDF0;
        Appearance.BorderColor := $99A8AC;
        Appearance.Gradient := ggVertical;
        Appearance.GradientMirror := ggVertical;

        CaptionAppearance.Color := $0099A8AC;
        CaptionAppearance.TextColor := clBlack;

        DragGripAppearance.Color := clWhite;
        DragGripAppearance.ColorTo := $DFEDF0;
        DragGripAppearance.ColorMirror := $DFEDF0;
        DragGripAppearance.ColorMirrorTo := $DFEDF0;

        ColorDropDownFloating := $DFEDF0;
        ColorDropDownFloatingTo := clNone;
      end;
      tsWindowsVista:
      begin
        BorderDropDownColor := clGray;

        Appearance.Color := $FDF8F1;
        Appearance.ColorTo := $FDF8F1;
        Appearance.ColorMirror := $FCEFD5;
        Appearance.ColorMirrorTo := $FDF8F1;
        Appearance.BorderColor := $FDDE99;
        Appearance.Gradient := ggVertical;
        Appearance.GradientMirror := ggVertical;

        Appearance.ColorHot := clNone;
        Appearance.ColorHotTo := $FFFAF0;
        Appearance.ColorMirrorHot := $FFFAF0;
        Appearance.ColorMirrorHotTo := $FFFAF0;
        Appearance.BorderColorHot := $FCF2DA;
        Appearance.GradientHot := ggVertical;
        Appearance.GradientMirrorHot := ggVertical;

        Appearance.ColorDown := $FCEFD5;
        Appearance.ColorDownTo := $FCEFD5;
        Appearance.ColorMirrorDown := $FDF4E3;
        Appearance.ColorMirrorDownTo := $FDF4E3;
        Appearance.BorderColorDown := $FEDF9A;
        Appearance.GradientDown := ggVertical;
        Appearance.GradientMirrorDown := ggVertical;

        Appearance.ColorChecked := $FCEFD5;
        Appearance.ColorCheckedTo := $FAE9C6;
        Appearance.ColorMirrorChecked := $F7DAA2;
        Appearance.ColorMirrorCheckedTo := $FBEDD3;
        Appearance.BorderColorChecked := $FEDF9A;
        Appearance.GradientChecked := ggVertical;
        Appearance.GradientMirrorChecked := ggVertical;

        CaptionAppearance.Color := $FBEDD3;
        CaptionAppearance.TextColor := clBlack;


        DragGripAppearance.BorderColorHot := clGray;
        DragGripAppearance.Color := $FEF9F0;
        DragGripAppearance.ColorTo := $FEF9F0;
        DragGripAppearance.ColorHot := $FBEDD3;
        DragGripAppearance.ColorHotTo := $FBEDD3;
        DragGripAppearance.ColorMirror := $FEF9F0;
        DragGripAppearance.ColorMirrorTo := $FEF9F0;
        DragGripAppearance.ColorMirrorHot := $FBEDD3;
        DragGripAppearance.ColorMirrorHotTo := $FBEDD3;


        SelectionAppearance.Color := $FFFFFF;
        SelectionAppearance.ColorTo := $FFFFFF;
        SelectionAppearance.ColorMirror := $FFFFFF;
        SelectionAppearance.ColorMirrorTo := $FFFFFF;


        SelectionAppearance.ColorHot := $FFFDF9;
        SelectionAppearance.ColorHotTo := $FFFAF0;
        SelectionAppearance.ColorMirrorHot := $FFFAF0;
        SelectionAppearance.ColorMirrorHotTo := $FFFDF9;
        SelectionAppearance.BorderColorHot := $FCF2DA;
        SelectionAppearance.GradientHot := ggVertical;
        SelectionAppearance.GradientMirrorHot := ggVertical;
        SelectionAppearance.TextColorHot := $CC6600;


        SelectionAppearance.ColorDown := $FEF9F0;
        SelectionAppearance.ColorDownTo := $FDF0D7;
        SelectionAppearance.ColorMirrorDown := $FDF0D7;
        SelectionAppearance.ColorMirrorDownTo := $FEF9F0;
        SelectionAppearance.BorderColorDown := $FEDF9A;
        SelectionAppearance.GradientDown := ggVertical;
        SelectionAppearance.GradientMirrorDown := ggVertical;
        SelectionAppearance.TextColorDown := clBlack;


        SelectionAppearance.ColorChecked := $FEF9F0;
        SelectionAppearance.ColorCheckedTo := $FDF0D7;
        SelectionAppearance.ColorMirrorChecked := $FDF0D7;
        SelectionAppearance.ColorMirrorCheckedTo := $FEF9F0;
        SelectionAppearance.BorderColorChecked := $FEDF9AB;
        SelectionAppearance.TextColorChecked := clBlack;
        SelectionAppearance.GradientChecked := ggVertical;
        SelectionAppearance.GradientMirrorChecked := ggVertical;
        SelectionAppearance.Rounded := True;

      end;
      tsWindows7:
      begin
        BorderDropDownColor := clGray;

        Appearance.Color := RGB(246, 248, 251);
        Appearance.ColorTo := RGB(230, 238, 245);
        Appearance.ColorMirror := RGB(223, 232, 241);
        Appearance.ColorMirrorTo := RGB(225, 234, 244);
        Appearance.BorderColor := RGB(176, 190, 210);
        Appearance.Gradient := ggVertical;
        Appearance.GradientMirror := ggVertical;

        Appearance.ColorHot := RGB(253, 240, 225);
        Appearance.ColorHotTo := RGB(255, 206, 105);
        Appearance.ColorMirrorHot := RGB(255, 255, 203);
        Appearance.ColorMirrorHotTo := RGB(255, 206, 105);
        Appearance.BorderColorHot := RGB(255, 183, 0);
        Appearance.GradientHot := ggVertical;
        Appearance.GradientMirrorHot := ggVertical;

        Appearance.ColorDown := RGB(238, 201, 139);
        Appearance.ColorDownTo := RGB(245, 199, 122);
        Appearance.ColorMirrorDown := RGB(243, 235, 147);
        Appearance.ColorMirrorDownTo := RGB(245, 187, 86);
        Appearance.BorderColorDown := RGB(194, 155, 41);
        Appearance.GradientDown := ggVertical;
        Appearance.GradientMirrorDown := ggVertical;

        Appearance.ColorChecked := RGB(246, 205, 152);
        Appearance.ColorCheckedTo := RGB(252, 208, 143);
        Appearance.ColorMirrorChecked := RGB(255, 196, 94);
        Appearance.ColorMirrorCheckedTo := RGB(255, 215, 131);
        Appearance.BorderColorChecked:= RGB(194, 146, 59);
        Appearance.GradientChecked := ggVertical;
        Appearance.GradientMirrorChecked := ggVertical;

        Appearance.ColorDisabled := clNone;
        Appearance.ColorDisabledTo := clNone;
        Appearance.ColorMirrorDisabled := clNone;
        Appearance.ColorMirrorDisabledTo := clNone;
        Appearance.BorderColorDisabled:= clgray;
        Appearance.GradientChecked := ggVertical;
        Appearance.GradientMirrorChecked := ggVertical;



        CaptionAppearance.Color := $FCDBC1;
        CaptionAppearance.TextColor := clBlack;


        DragGripAppearance.BorderColorHot := clGray;
        DragGripAppearance.Color := $FCEBDC;
        DragGripAppearance.ColorTo := $FCEBDC;
        DragGripAppearance.ColorHot := $FCDBC1;
        DragGripAppearance.ColorHotTo := $FCDBC1;
        DragGripAppearance.ColorMirror := $FCEBDC;
        DragGripAppearance.ColorMirrorTo := $FCEBDC;
        DragGripAppearance.ColorMirrorHot := $FCDBC1;
        DragGripAppearance.ColorMirrorHotTo := $FCDBC1;

        SelectionAppearance.Color := $FFFFFF;
        SelectionAppearance.ColorTo := $FFFFFF;
        SelectionAppearance.ColorMirror := $FFFFFF;
        SelectionAppearance.ColorMirrorTo := $FFFFFF;

        SelectionAppearance.ColorHot := $FDFBFA;
        SelectionAppearance.ColorHotTo := $FDF3EB;
        SelectionAppearance.ColorMirrorHot := $FDF3EB;
        SelectionAppearance.ColorMirrorHotTo := $FDFBFA;
        SelectionAppearance.BorderColorHot := $FBD6B8;
        SelectionAppearance.GradientHot := ggVertical;
        SelectionAppearance.GradientMirrorHot := ggVertical;
        SelectionAppearance.TextColorHot := $CC6600;

        SelectionAppearance.ColorDown := $FCEBDC;
        SelectionAppearance.ColorDownTo := $FCDBC1;
        SelectionAppearance.ColorMirrorDown := $FCDBC1;
        SelectionAppearance.ColorMirrorDownTo := $FCEBDC;
        SelectionAppearance.BorderColorDown := $CEA27D;
        SelectionAppearance.GradientDown := ggVertical;
        SelectionAppearance.GradientMirrorDown := ggVertical;
        SelectionAppearance.TextColorDown := clBlack;

        SelectionAppearance.ColorChecked := $FCEBDC;
        SelectionAppearance.ColorCheckedTo := $FCDBC1;
        SelectionAppearance.ColorMirrorChecked := $FCDBC1;
        SelectionAppearance.ColorMirrorCheckedTo := $FCEBDC;
        SelectionAppearance.BorderColorChecked := $CEA27D;
        SelectionAppearance.TextColorChecked := clBlack;
        SelectionAppearance.GradientChecked := ggVertical;
        SelectionAppearance.GradientMirrorChecked := ggVertical;
        SelectionAppearance.Rounded := True;

      end;
      tsTerminal:
      begin
        BorderDropDownColor := clGray;

        Appearance.Color := clbtnFace;
        Appearance.ColorTo := clbtnFace;
        Appearance.ColorMirror := clbtnFace;
        Appearance.ColorMirrorTo := clbtnFace;
        Appearance.BorderColor := ClGray;
        Appearance.Gradient := ggVertical;
        Appearance.GradientMirror := ggVertical;

        Appearance.ColorHot := clSilver;
        Appearance.ColorHotTo := clSilver;
        Appearance.ColorMirrorHot := clSilver;
        Appearance.ColorMirrorHotTo := clSilver;
        Appearance.BorderColorHot := clGray;
        Appearance.GradientHot := ggVertical;
        Appearance.GradientMirrorHot := ggVertical;

        Appearance.ColorDown := clHighlight;
        Appearance.ColorDownTo := clHighlight;
        Appearance.ColorMirrorDown := clHighlight;
        Appearance.ColorMirrorDownTo := clHighlight;
        Appearance.BorderColorDown := clHighlight;
        Appearance.GradientDown := ggVertical;
        Appearance.GradientMirrorDown := ggVertical;

        Appearance.ColorChecked := clGray;
        Appearance.ColorCheckedTo := clGray;
        Appearance.ColorMirrorChecked := clGray;
        Appearance.ColorMirrorCheckedTo := clGray;
        Appearance.BorderColorChecked := clGray;
        Appearance.GradientChecked := ggVertical;
        Appearance.GradientMirrorChecked := ggVertical;

        CaptionAppearance.Color := clSilver;
        CaptionAppearance.TextColor := clWhite;

        DragGripAppearance.BorderColorHot := clGray;
        DragGripAppearance.Color := clHighLight;
        DragGripAppearance.ColorTo := clHighLight;
        DragGripAppearance.ColorHot := clSilver;
        DragGripAppearance.ColorHotTo := clSilver;
        DragGripAppearance.ColorMirror := clHighLight;
        DragGripAppearance.ColorMirrorTo := clHighLight;
        DragGripAppearance.ColorMirrorHot := clSilver;
        DragGripAppearance.ColorMirrorHotTo := clSilver;

        SelectionAppearance.Color := clBtnFace;
        SelectionAppearance.ColorTo := clBtnFace;
        SelectionAppearance.ColorMirror := clBtnFace;
        SelectionAppearance.ColorMirrorTo := clBtnFace;

        SelectionAppearance.ColorHot := clSilver;
        SelectionAppearance.ColorHotTo := clSilver;
        SelectionAppearance.ColorMirrorHot := clSilver;
        SelectionAppearance.ColorMirrorHotTo := clSilver;
        SelectionAppearance.BorderColorHot := clGray;
        SelectionAppearance.GradientHot := ggVertical;
        SelectionAppearance.GradientMirrorHot := ggVertical;
        SelectionAppearance.TextColorHot := clBlack;

        SelectionAppearance.ColorDown := clHighLight;
        SelectionAppearance.ColorDownTo := clHighLight;
        SelectionAppearance.ColorMirrorDown := clHighLight;
        SelectionAppearance.ColorMirrorDownTo := clHighLight;
        SelectionAppearance.BorderColorDown := clGray;
        SelectionAppearance.GradientDown := ggVertical;
        SelectionAppearance.GradientMirrorDown := ggVertical;
        SelectionAppearance.TextColorDown := clWhite;

        SelectionAppearance.ColorChecked := clHighLight;
        SelectionAppearance.ColorCheckedTo := clHighLight;
        SelectionAppearance.ColorMirrorChecked := clHighLight;
        SelectionAppearance.ColorMirrorCheckedTo := clHighLight;
        SelectionAppearance.BorderColorChecked := clGray;
        SelectionAppearance.TextColorChecked := clWhite;
        SelectionAppearance.GradientChecked := ggVertical;
        SelectionAppearance.GradientMirrorChecked := ggVertical;
        SelectionAppearance.Rounded := True;
      end;

      tsOffice2010Blue:
      begin
        BorderDropDownColor := $C7B29F;

        Appearance.Color := $FDF6EF;
        Appearance.ColorTo := $F0DAC7;
        Appearance.ColorMirror := $F0DAC7;
        Appearance.ColorMirrorTo := $F0DAC7;
        Appearance.BorderColor := $C7B29F;
        Appearance.Gradient := ggVertical;
        Appearance.GradientMirror := ggVertical;

        Appearance.ColorHot := $8AE3FD;
        Appearance.ColorHotTo := $D9F9FD;
        Appearance.ColorMirrorHot := $D9F9FD;
        Appearance.ColorMirrorHotTo := $8AE3FD;
        Appearance.BorderColorHot := $58CAF1;
        Appearance.GradientHot := ggVertical;
        Appearance.GradientMirrorHot := ggVertical;

        Appearance.ColorDown := $6CD0FF;
        Appearance.ColorDownTo := $7BEEFF;
        Appearance.ColorMirrorDown := $7BEEFF;
        Appearance.ColorMirrorDownTo := $6CD0FF;
        Appearance.BorderColorDown := $308AC2;
        Appearance.GradientDown := ggVertical;
        Appearance.GradientMirrorDown := ggVertical;

        Appearance.ColorChecked := $D9F9FD;
        Appearance.ColorCheckedTo := $6CD0FF;
        Appearance.ColorMirrorChecked := $6CD0FF;
        Appearance.ColorMirrorCheckedTo := $6CD0FF;
        Appearance.BorderColorChecked := $308AC2;
        Appearance.GradientChecked := ggVertical;
        Appearance.GradientMirrorChecked := ggVertical;

        CaptionAppearance.Color := RGB(177, 195, 217);
        CaptionAppearance.TextColor := clBlack;

        DragGripAppearance.BorderColorHot := $58CAF1;
        DragGripAppearance.Color := $9C8B7B;
        DragGripAppearance.ColorTo := $9C8B7B;
        DragGripAppearance.ColorHot := $8AE3FD;
        DragGripAppearance.ColorHotTo := $8AE3FD;
        DragGripAppearance.ColorMirror := $9C8B7B;
        DragGripAppearance.ColorMirrorTo := $9C8B7B;
        DragGripAppearance.ColorMirrorHot := $8AE3FD;
        DragGripAppearance.ColorMirrorHotTo := $8AE3FD;

        SelectionAppearance.Color := clWhite;
        SelectionAppearance.ColorTo := RGB(237, 239, 241);
        SelectionAppearance.ColorMirror := clNone;
        SelectionAppearance.ColorMirrorTo := clNone;

        SelectionAppearance.ColorHot := $8AE3FD;
        SelectionAppearance.ColorHotTo := $8AE3FD;
        SelectionAppearance.ColorMirrorHot := $8AE3FD;
        SelectionAppearance.ColorMirrorHotTo := $D9F9FD;
        SelectionAppearance.BorderColorHot := $58CAF1;
        SelectionAppearance.GradientHot := ggVertical;
        SelectionAppearance.GradientMirrorHot := ggVertical;
        SelectionAppearance.TextColorHot := clBlack;

        SelectionAppearance.ColorDown := $6CD0FF;
        SelectionAppearance.ColorDownTo := $7BEEFF;
        SelectionAppearance.ColorMirrorDown := $7BEEFF;
        SelectionAppearance.ColorMirrorDownTo := $6CD0FF;
        SelectionAppearance.BorderColorDown := $308AC2;
        SelectionAppearance.GradientDown := ggVertical;
        SelectionAppearance.GradientMirrorDown := ggVertical;
        SelectionAppearance.TextColorDown := clBlack;

        SelectionAppearance.ColorChecked := $D9F9FD;
        SelectionAppearance.ColorCheckedTo := $6CD0FF;
        SelectionAppearance.ColorMirrorChecked := $6CD0FF;
        SelectionAppearance.ColorMirrorCheckedTo := $6CD0FF;
        SelectionAppearance.BorderColorChecked := $308AC2;
        SelectionAppearance.TextColorChecked := clBlack;
        SelectionAppearance.GradientChecked := ggVertical;
        SelectionAppearance.GradientMirrorChecked := ggVertical;

        end;

      tsOffice2010Silver:
      begin
        BorderDropDownColor := $D2CDC8;

        Appearance.Color := $FFFFFF;
        Appearance.ColorTo := $EDE5E0;
        Appearance.ColorMirror := $EDE5E0;
        Appearance.ColorMirrorTo := $EDE5E0;
        Appearance.BorderColor := $D2CDC8;
        Appearance.Gradient := ggVertical;
        Appearance.GradientMirror := ggVertical;

        Appearance.ColorHot := $8AE3FD;
        Appearance.ColorHotTo := $D9F9FD;
        Appearance.ColorMirrorHot := $D9F9FD;
        Appearance.ColorMirrorHotTo := $8AE3FD;
        Appearance.BorderColorHot := $58CAF1;
        Appearance.GradientHot := ggVertical;
        Appearance.GradientMirrorHot := ggVertical;

        Appearance.ColorDown := $6CD0FF;
        Appearance.ColorDownTo := $7BEEFF;
        Appearance.ColorMirrorDown := $7BEEFF;
        Appearance.ColorMirrorDownTo := $6CD0FF;
        Appearance.BorderColorDown := $308AC2;
        Appearance.GradientDown := ggVertical;
        Appearance.GradientMirrorDown := ggVertical;

        Appearance.ColorChecked := $D9F9FD;
        Appearance.ColorCheckedTo := $6CD0FF;
        Appearance.ColorMirrorChecked := $6CD0FF;
        Appearance.ColorMirrorCheckedTo := $6CD0FF;
        Appearance.BorderColorChecked := $308AC2;
        Appearance.GradientChecked := ggVertical;
        Appearance.GradientMirrorChecked := ggVertical;

        CaptionAppearance.Color := RGB(206, 212, 219);
        CaptionAppearance.TextColor := clBlack;

        DragGripAppearance.BorderColorHot := $58CAF1;
        DragGripAppearance.Color := $96908A;
        DragGripAppearance.ColorTo := $96908A;
        DragGripAppearance.ColorHot := $8AE3FD;
        DragGripAppearance.ColorHotTo := $8AE3FD;
        DragGripAppearance.ColorMirror := $96908A;
        DragGripAppearance.ColorMirrorTo := $96908A;
        DragGripAppearance.ColorMirrorHot := $8AE3FD;
        DragGripAppearance.ColorMirrorHotTo := $8AE3FD;

        SelectionAppearance.Color := clWhite;
        SelectionAppearance.ColorTo := RGB(237, 239, 241);
        SelectionAppearance.ColorMirror := clNone;
        SelectionAppearance.ColorMirrorTo := clNone;

        SelectionAppearance.ColorHot := $8AE3FD;
        SelectionAppearance.ColorHotTo := $8AE3FD;
        SelectionAppearance.ColorMirrorHot := $8AE3FD;
        SelectionAppearance.ColorMirrorHotTo := $D9F9FD;
        SelectionAppearance.BorderColorHot := $58CAF1;
        SelectionAppearance.GradientHot := ggVertical;
        SelectionAppearance.GradientMirrorHot := ggVertical;
        SelectionAppearance.TextColorHot := clBlack;

        SelectionAppearance.ColorDown := $6CD0FF;
        SelectionAppearance.ColorDownTo := $7BEEFF;
        SelectionAppearance.ColorMirrorDown := $7BEEFF;
        SelectionAppearance.ColorMirrorDownTo := $6CD0FF;
        SelectionAppearance.BorderColorDown := $308AC2;
        SelectionAppearance.GradientDown := ggVertical;
        SelectionAppearance.GradientMirrorDown := ggVertical;
        SelectionAppearance.TextColorDown := clBlack;

        SelectionAppearance.ColorChecked := $D9F9FD;
        SelectionAppearance.ColorCheckedTo := $6CD0FF;
        SelectionAppearance.ColorMirrorChecked := $6CD0FF;
        SelectionAppearance.ColorMirrorCheckedTo := $6CD0FF;
        SelectionAppearance.BorderColorChecked := $308AC2;
        SelectionAppearance.TextColorChecked := clBlack;
        SelectionAppearance.GradientChecked := ggVertical;
        SelectionAppearance.GradientMirrorChecked := ggVertical;

      end;

      tsOffice2010Black:
      begin
        BorderDropDownColor := $6D6D6D;

        Appearance.Color := $BFBFBF;
        Appearance.ColorTo := $919191;
        Appearance.ColorMirror := $919191;
        Appearance.ColorMirrorTo := $919191;
        Appearance.BorderColor := $6D6D6D;
        Appearance.Gradient := ggVertical;
        Appearance.GradientMirror := ggVertical;

        Appearance.ColorHot := $8AE3FD;
        Appearance.ColorHotTo := $D9F9FD;
        Appearance.ColorMirrorHot := $D9F9FD;
        Appearance.ColorMirrorHotTo := $8AE3FD;
        Appearance.BorderColorHot := $58CAF1;
        Appearance.GradientHot := ggVertical;
        Appearance.GradientMirrorHot := ggVertical;

        Appearance.ColorDown := $6CD0FF;
        Appearance.ColorDownTo := $7BEEFF;
        Appearance.ColorMirrorDown := $7BEEFF;
        Appearance.ColorMirrorDownTo := $6CD0FF;
        Appearance.BorderColorDown := $308AC2;
        Appearance.GradientDown := ggVertical;
        Appearance.GradientMirrorDown := ggVertical;

        Appearance.ColorChecked := $D9F9FD;
        Appearance.ColorCheckedTo := $6CD0FF;
        Appearance.ColorMirrorChecked := $6CD0FF;
        Appearance.ColorMirrorCheckedTo := $6CD0FF;
        Appearance.BorderColorChecked := $308AC2;
        Appearance.GradientChecked := ggVertical;
        Appearance.GradientMirrorChecked := ggVertical;

        CaptionAppearance.Color := RGB(124, 124, 124);
        CaptionAppearance.TextColor := clBlack;

        DragGripAppearance.BorderColorHot := $58CAF1;
        DragGripAppearance.Color := $444444;
        DragGripAppearance.ColorTo := $444444;
        DragGripAppearance.ColorHot := $8AE3FD;
        DragGripAppearance.ColorHotTo := $8AE3FD;
        DragGripAppearance.ColorMirror := $444444;
        DragGripAppearance.ColorMirrorTo := $444444;
        DragGripAppearance.ColorMirrorHot := $8AE3FD;
        DragGripAppearance.ColorMirrorHotTo := $8AE3FD;

        SelectionAppearance.Color := clWhite;
        SelectionAppearance.ColorTo := RGB(237, 239, 241);
        SelectionAppearance.ColorMirror := clNone;
        SelectionAppearance.ColorMirrorTo := clNone;

        SelectionAppearance.ColorHot := $8AE3FD;
        SelectionAppearance.ColorHotTo := $8AE3FD;
        SelectionAppearance.ColorMirrorHot := $8AE3FD;
        SelectionAppearance.ColorMirrorHotTo := $D9F9FD;
        SelectionAppearance.BorderColorHot := $58CAF1;
        SelectionAppearance.GradientHot := ggVertical;
        SelectionAppearance.GradientMirrorHot := ggVertical;
        SelectionAppearance.TextColorHot := clBlack;

        SelectionAppearance.ColorDown := $6CD0FF;
        SelectionAppearance.ColorDownTo := $7BEEFF;
        SelectionAppearance.ColorMirrorDown := $7BEEFF;
        SelectionAppearance.ColorMirrorDownTo := $6CD0FF;
        SelectionAppearance.BorderColorDown := $308AC2;
        SelectionAppearance.GradientDown := ggVertical;
        SelectionAppearance.GradientMirrorDown := ggVertical;
        SelectionAppearance.TextColorDown := clBlack;

        SelectionAppearance.ColorChecked := $D9F9FD;
        SelectionAppearance.ColorCheckedTo := $6CD0FF;
        SelectionAppearance.ColorMirrorChecked := $6CD0FF;
        SelectionAppearance.ColorMirrorCheckedTo := $6CD0FF;
        SelectionAppearance.BorderColorChecked := $308AC2;
        SelectionAppearance.TextColorChecked := clBlack;
        SelectionAppearance.GradientChecked := ggVertical;
        SelectionAppearance.GradientMirrorChecked := ggVertical;

      end;
   tsWindows8, tsWindows10:
      begin
        Rounded :=false;
        BorderDropDownColor := $DCDBDA;

        Appearance.Color := $F7F6F5;
        Appearance.ColorTo := clNone;
        Appearance.ColorMirror := $F7F6F5;
        Appearance.ColorMirrorTo := clNone;
        Appearance.BorderColor := $E4E3E2;
        Appearance.Gradient := ggVertical;
        Appearance.GradientMirror := ggVertical;

        Appearance.ColorHot := $F7EFE8;
        Appearance.ColorHotTo := clNone;
        Appearance.ColorMirrorHot := $F7EFE8;
        Appearance.ColorMirrorHotTo := clNone;
        Appearance.BorderColorHot := $F9CEA4;
        Appearance.GradientHot := ggVertical;
        Appearance.GradientMirrorHot := ggVertical;

        Appearance.ColorDown := $F7E0C9;
        Appearance.ColorDownTo := clNone;
        Appearance.ColorMirrorDown := $F7E0C9;
        Appearance.ColorMirrorDownTo := clNone;
        Appearance.BorderColorDown := $E4A262;
        Appearance.GradientDown := ggVertical;
        Appearance.GradientMirrorDown := ggVertical;

        Appearance.ColorChecked := $DAA026;
        Appearance.ColorCheckedTo := clNone;
        Appearance.ColorMirrorChecked := $DAA026; //$F6E8CB;
        Appearance.ColorMirrorCheckedTo := clNone;
        Appearance.BorderColorChecked := $DAA026;
        Appearance.GradientChecked := ggVertical;
        Appearance.GradientMirrorChecked := ggVertical;

        Appearance.ColorDisabled := $F7F7F7;
        Appearance.ColorDisabledTo := clNone;
        Appearance.ColorMirrorDisabled := $F7F7F7;
        Appearance.ColorMirrorDisabledTo := clNone;

        CaptionAppearance.Color := $F7F6F5;
        CaptionAppearance.TextColor := clBlack;


        DragGripAppearance.BorderColorHot := $F9CEA4;
        DragGripAppearance.Color := $F7F6F5;
        DragGripAppearance.ColorTo := DragGripAppearance.Color;
        DragGripAppearance.ColorHot := DragGripAppearance.Color;
        DragGripAppearance.ColorHotTo := DragGripAppearance.Color;
        DragGripAppearance.ColorMirror := DragGripAppearance.Color;
        DragGripAppearance.ColorMirrorTo := DragGripAppearance.Color;
        DragGripAppearance.ColorMirrorHot := DragGripAppearance.Color;
        DragGripAppearance.ColorMirrorHotTo := DragGripAppearance.Color;

        SelectionAppearance.Color := $F7F6F5;
        SelectionAppearance.ColorTo := clNone;
        SelectionAppearance.ColorMirror := $F7F6F5;
        SelectionAppearance.ColorMirrorTo := clNone;
        SelectionAppearance.BorderColor := $E4E3E2;
        SelectionAppearance.Gradient := ggVertical;
        SelectionAppearance.GradientMirror := ggVertical;

        SelectionAppearance.ColorHot := $F7EFE8;
        SelectionAppearance.ColorHotTo := clNone;
        SelectionAppearance.ColorMirrorHot := $F7EFE8;
        SelectionAppearance.ColorMirrorHotTo := clNone;
        SelectionAppearance.BorderColorHot := $F9CEA4;
        SelectionAppearance.GradientHot := ggVertical;
        SelectionAppearance.GradientMirrorHot := ggVertical;

        SelectionAppearance.ColorDown := $F7E0C9;
        SelectionAppearance.ColorDownTo := clNone;
        SelectionAppearance.ColorMirrorDown := $F7E0C9;
        SelectionAppearance.ColorMirrorDownTo := clNone;
        SelectionAppearance.BorderColorDown := $E4A262;
        SelectionAppearance.GradientDown := ggVertical;
        SelectionAppearance.GradientMirrorDown := ggVertical;

        SelectionAppearance.ColorChecked := $DAA026;
        SelectionAppearance.ColorCheckedTo := clNone;
        SelectionAppearance.ColorMirrorChecked := $DAA026; //$F6E8CB;
        SelectionAppearance.ColorMirrorCheckedTo := clNone;
        SelectionAppearance.BorderColorChecked := $DAA026;
        SelectionAppearance.GradientChecked := ggVertical;
        SelectionAppearance.GradientMirrorChecked := ggVertical;

        SelectionAppearance.ColorDisabled := $F7F7F7;
        SelectionAppearance.ColorDisabledTo := clNone;
        SelectionAppearance.ColorMirrorDisabled := $F7F7F7;
        SelectionAppearance.ColorMirrorDisabledTo := clNone;

        {
        SelectionAppearance.ColorDisabled := SelectionAppearance.Color;
        SelectionAppearance.ColorDisabledTo := SelectionAppearance.ColorTo;
        SelectionAppearance.ColorMirrorDisabled := SelectionAppearance.ColorMirror;
        SelectionAppearance.ColorMirrorDisabledTo := SelectionAppearance.ColorMirrorTo;
        SelectionAppearance.BorderColorDisabled := SelectionAppearance.Color;
        }



        SelectionAppearance.Rounded := false;

      end;
    tsOffice2013White:
      begin
        SelectionAppearance.Color := clWhite;
        SelectionAppearance.ColorTo := clNone;
        SelectionAppearance.ColorMirror := clWhite;
        SelectionAppearance.ColorMirrorTo := clNone;
        SelectionAppearance.BorderColor := $D4D4D4;
        SelectionAppearance.Gradient := ggVertical;
        SelectionAppearance.GradientMirror := ggVertical;

        SelectionAppearance.ColorHot := $FCF0E4;
        SelectionAppearance.ColorHotTo := clNone;
        SelectionAppearance.ColorMirrorHot := $FCF0E4;
        SelectionAppearance.ColorMirrorHotTo := clNone;
        SelectionAppearance.BorderColorHot := $EAB47E;
        SelectionAppearance.GradientHot := ggVertical;
        SelectionAppearance.GradientMirrorHot := ggVertical;

        SelectionAppearance.ColorDown := $FCE2C8;
        SelectionAppearance.ColorDownTo := clNone;
        SelectionAppearance.ColorMirrorDown := $FCE2C8;
        SelectionAppearance.ColorMirrorDownTo := clNone;
        SelectionAppearance.BorderColorDown := $E59D56;
        SelectionAppearance.GradientDown := ggVertical;
        SelectionAppearance.GradientMirrorDown := ggVertical;

        SelectionAppearance.ColorChecked := $FF9933;
        SelectionAppearance.ColorCheckedTo := clNone;
        SelectionAppearance.ColorMirrorChecked := $FF9933;
        SelectionAppearance.ColorMirrorCheckedTo := clNone;
        SelectionAppearance.BorderColorChecked := $FF9933;
        SelectionAppearance.GradientChecked := ggVertical;
        SelectionAppearance.GradientMirrorChecked := ggVertical;


        SelectionAppearance.ColorDisabled := $EEEEEE;
        SelectionAppearance.ColorDisabledTo := clNone;
        SelectionAppearance.ColorMirrorDisabled := $EEEEEE;
        SelectionAppearance.ColorMirrorDownTo := clNone;
        SelectionAppearance.BorderColorDisabled := $ACACAC;

        SelectionAppearance.TextColorChecked := clWhite;

      end;
    tsOffice2013LightGray:
      begin
        SelectionAppearance.Color := $F6F6F6;
        SelectionAppearance.ColorTo := clNone;
        SelectionAppearance.ColorMirror := $F6F6F6;
        SelectionAppearance.ColorMirrorTo := clNone;
        SelectionAppearance.BorderColor := $C6C6C6;
        SelectionAppearance.Gradient := ggVertical;
        SelectionAppearance.GradientMirror := ggVertical;

        SelectionAppearance.ColorHot := $FCF0E4;
        SelectionAppearance.ColorHotTo := clNone;
        SelectionAppearance.ColorMirrorHot := $FCF0E4;
        SelectionAppearance.ColorMirrorHotTo := clNone;
        SelectionAppearance.BorderColorHot := $EAB47E;
        SelectionAppearance.GradientHot := ggVertical;
        SelectionAppearance.GradientMirrorHot := ggVertical;

        SelectionAppearance.ColorDown := $FCE2C8;
        SelectionAppearance.ColorDownTo := clNone;
        SelectionAppearance.ColorMirrorDown := $FCE2C8;
        SelectionAppearance.ColorMirrorDownTo := clNone;
        SelectionAppearance.BorderColorDown := $E59D56;
        SelectionAppearance.GradientDown := ggVertical;
        SelectionAppearance.GradientMirrorDown := ggVertical;

        SelectionAppearance.ColorChecked := $FF9933;
        SelectionAppearance.ColorCheckedTo := clNone;
        SelectionAppearance.ColorMirrorChecked := $FF9933;
        SelectionAppearance.ColorMirrorCheckedTo := clNone;
        SelectionAppearance.BorderColorChecked := $FF9933;
        SelectionAppearance.GradientChecked := ggVertical;
        SelectionAppearance.GradientMirrorChecked := ggVertical;

        SelectionAppearance.ColorDisabled := $EEEEEE;
        SelectionAppearance.ColorDisabledTo := clNone;
        SelectionAppearance.ColorMirrorDisabled := $EEEEEE;
        SelectionAppearance.ColorMirrorDownTo := clNone;
        SelectionAppearance.BorderColorDisabled := $ACACAC;

        SelectionAppearance.TextColorChecked := clWhite;

      end;
    tsOffice2013Gray:
      begin
        SelectionAppearance.Color := $E5E5E5;
        SelectionAppearance.ColorTo := clNone;
        SelectionAppearance.ColorMirror := $E5E5E5;
        SelectionAppearance.ColorMirrorTo := clNone;
        SelectionAppearance.BorderColor := $ABABAB;
        SelectionAppearance.Gradient := ggVertical;
        SelectionAppearance.GradientMirror := ggVertical;

        SelectionAppearance.ColorHot := $FCF0E4;
        SelectionAppearance.ColorHotTo := clNone;
        SelectionAppearance.ColorMirrorHot := $FCF0E4;
        SelectionAppearance.ColorMirrorHotTo := clNone;
        SelectionAppearance.BorderColorHot := $EAB47E;
        SelectionAppearance.GradientHot := ggVertical;
        SelectionAppearance.GradientMirrorHot := ggVertical;

        SelectionAppearance.ColorDown := $FCE2C8;
        SelectionAppearance.ColorDownTo := clNone;
        SelectionAppearance.ColorMirrorDown := $FCE2C8;
        SelectionAppearance.ColorMirrorDownTo := clNone;
        SelectionAppearance.BorderColorDown := $E59D56;
        SelectionAppearance.GradientDown := ggVertical;
        SelectionAppearance.GradientMirrorDown := ggVertical;

        SelectionAppearance.ColorChecked := $FF9933;
        SelectionAppearance.ColorCheckedTo := clNone;
        SelectionAppearance.ColorMirrorChecked := $FF9933;
        SelectionAppearance.ColorMirrorCheckedTo := clNone;
        SelectionAppearance.BorderColorChecked := $FF9933;
        SelectionAppearance.GradientChecked := ggVertical;
        SelectionAppearance.GradientMirrorChecked := ggVertical;

        SelectionAppearance.ColorDisabled := $EEEEEE;
        SelectionAppearance.ColorDisabledTo := clNone;
        SelectionAppearance.ColorMirrorDisabled := $EEEEEE;
        SelectionAppearance.ColorMirrorDownTo := clNone;
        SelectionAppearance.BorderColorDisabled := $ACACAC;

        SelectionAppearance.TextColorChecked := clWhite;

      end;
   tsOffice2016White:
      begin
        BorderDropDownColor := $D4D4D4;
        Rounded :=false;

        Appearance.Color := clWhite;
        Appearance.ColorTo := clNone;
        Appearance.ColorMirror := clWhite;
        Appearance.ColorMirrorTo := clNone;
        Appearance.BorderColor := $D4D4D4;
        Appearance.Gradient := ggVertical;
        Appearance.GradientMirror := ggVertical;

        Appearance.ColorHot := $F2E1D5;
        Appearance.ColorHotTo := clNone;
        Appearance.ColorMirrorHot := $F2E1D5;
        Appearance.ColorMirrorHotTo := clNone;
        Appearance.BorderColorHot := $F2E1D5;
        Appearance.GradientHot := ggVertical;
        Appearance.GradientMirrorHot := ggVertical;

        Appearance.ColorDown := $E3BDA3;
        Appearance.ColorDownTo := clNone;
        Appearance.ColorMirrorDown := $E3BDA3;
        Appearance.ColorMirrorDownTo := clNone;
        Appearance.BorderColorDown := $E3BDA3;
        Appearance.GradientDown := ggVertical;
        Appearance.GradientMirrorDown := ggVertical;

        Appearance.ColorChecked := $F2D5C2;
        Appearance.ColorCheckedTo := clNone;
        Appearance.ColorMirrorChecked := $F2D5C2;
        Appearance.ColorMirrorCheckedTo := clNone;
        Appearance.BorderColorChecked := $F2D5C2;
        Appearance.GradientChecked := ggVertical;
        Appearance.GradientMirrorChecked := ggVertical;

        Appearance.ColorDisabled := clWhite;
        Appearance.ColorDisabledTo := clNone;
        Appearance.ColorMirrorDisabled := clWhite;
        Appearance.ColorMirrorDisabledTo := clNone;
        Appearance.BorderColorDisabled := $D4D4D4;

        CaptionAppearance.Color := clWhite;
        CaptionAppearance.TextColor := $444444;


        DragGripAppearance.BorderColorHot := $F2E1D5;
        DragGripAppearance.Color := clWhite;
        DragGripAppearance.ColorTo := DragGripAppearance.Color;
        DragGripAppearance.ColorHot := DragGripAppearance.Color;
        DragGripAppearance.ColorHotTo := DragGripAppearance.Color;
        DragGripAppearance.ColorMirror := DragGripAppearance.Color;
        DragGripAppearance.ColorMirrorTo := DragGripAppearance.Color;
        DragGripAppearance.ColorMirrorHot := DragGripAppearance.Color;
        DragGripAppearance.ColorMirrorHotTo := DragGripAppearance.Color;

        SelectionAppearance.Color := clWhite;
        SelectionAppearance.ColorTo := clWhite;
        SelectionAppearance.ColorMirror := clWhite;
        SelectionAppearance.ColorMirrorTo := clWhite;

        SelectionAppearance.ColorHot := $F2E1D5;
        SelectionAppearance.ColorHotTo := $F2E1D5;
        SelectionAppearance.ColorMirrorHot := $F2E1D5;
        SelectionAppearance.ColorMirrorHotTo := $F2E1D5;
        SelectionAppearance.BorderColorHot := $F2E1D5;
        SelectionAppearance.GradientHot := ggVertical;
        SelectionAppearance.GradientMirrorHot := ggVertical;
        SelectionAppearance.TextColorHot := $505050;

        SelectionAppearance.ColorDown := $E3BDA3;
        SelectionAppearance.ColorDownTo := $E3BDA3;
        SelectionAppearance.ColorMirrorDown := $E3BDA3;
        SelectionAppearance.ColorMirrorDownTo := $E3BDA3;
        SelectionAppearance.BorderColorDown := $E3BDA3;
        SelectionAppearance.GradientDown := ggVertical;
        SelectionAppearance.GradientMirrorDown := ggVertical;
        SelectionAppearance.TextColorDown := $505050;

        SelectionAppearance.ColorChecked := $F2D5C2;
        SelectionAppearance.ColorCheckedTo := $F2D5C2;
        SelectionAppearance.ColorMirrorChecked := $F2D5C2;
        SelectionAppearance.ColorMirrorCheckedTo := $F2D5C2;
        SelectionAppearance.BorderColorChecked := $F2D5C2;
        SelectionAppearance.TextColorChecked := $505050;
        SelectionAppearance.GradientChecked := ggVertical;
        SelectionAppearance.GradientMirrorChecked := ggVertical;
        SelectionAppearance.Rounded := false;

      end;
    tsOffice2016Gray:
      begin
        BorderDropDownColor := $444444;
        Rounded :=false;

        Appearance.Color := $B2B2B2;
        Appearance.ColorTo := clNone;
        Appearance.ColorMirror := $B2B2B2;
        Appearance.ColorMirrorTo := clNone;
        Appearance.BorderColor := $444444;
        Appearance.Gradient := ggVertical;
        Appearance.GradientMirror := ggVertical;

        Appearance.ColorHot := $F2E1D5;
        Appearance.ColorHotTo := clNone;
        Appearance.ColorMirrorHot := $F2E1D5;
        Appearance.ColorMirrorHotTo := clNone;
        Appearance.BorderColorHot := $F2E1D5;
        Appearance.GradientHot := ggVertical;
        Appearance.GradientMirrorHot := ggVertical;

        Appearance.ColorDown := $E3BDA3;
        Appearance.ColorDownTo := clNone;
        Appearance.ColorMirrorDown := $E3BDA3;
        Appearance.ColorMirrorDownTo := clNone;
        Appearance.BorderColorDown := $E3BDA3;
        Appearance.GradientDown := ggVertical;
        Appearance.GradientMirrorDown := ggVertical;

        Appearance.ColorChecked := $F2D5C2;
        Appearance.ColorCheckedTo := clNone;
        Appearance.ColorMirrorChecked := $F2D5C2;
        Appearance.ColorMirrorCheckedTo := clNone;
        Appearance.BorderColorChecked := $F2D5C2;
        Appearance.GradientChecked := ggVertical;
        Appearance.GradientMirrorChecked := ggVertical;

        Appearance.ColorDisabled := $B2B2B2;
        Appearance.ColorDisabledTo := clNone;
        Appearance.ColorMirrorDisabled := $B2B2B2;
        Appearance.ColorMirrorDisabledTo := clNone;
        Appearance.BorderColorDisabled := $444444;

        CaptionAppearance.Color := $B2B2B2;
        CaptionAppearance.TextColor := $424242;


        DragGripAppearance.BorderColorHot := $F2E1D5;
        DragGripAppearance.Color := $B2B2B2;
        DragGripAppearance.ColorTo := DragGripAppearance.Color;
        DragGripAppearance.ColorHot := DragGripAppearance.Color;
        DragGripAppearance.ColorHotTo := DragGripAppearance.Color;
        DragGripAppearance.ColorMirror := DragGripAppearance.Color;
        DragGripAppearance.ColorMirrorTo := DragGripAppearance.Color;
        DragGripAppearance.ColorMirrorHot := DragGripAppearance.Color;
        DragGripAppearance.ColorMirrorHotTo := DragGripAppearance.Color;

        SelectionAppearance.Color := $B2B2B2;
        SelectionAppearance.ColorTo := clNone;
        SelectionAppearance.ColorMirror := $B2B2B2;
        SelectionAppearance.ColorMirrorTo := clNone;
        SelectionAppearance.BorderColor := $444444;
        SelectionAppearance.Gradient := ggVertical;
        SelectionAppearance.GradientMirror := ggVertical;

        SelectionAppearance.ColorHot := $F2E1D5;
        SelectionAppearance.ColorHotTo := clNone;
        SelectionAppearance.ColorMirrorHot := $F2E1D5;
        SelectionAppearance.ColorMirrorHotTo := clNone;
        SelectionAppearance.BorderColorHot := $F2E1D5;
        SelectionAppearance.GradientHot := ggVertical;
        SelectionAppearance.GradientMirrorHot := ggVertical;

        SelectionAppearance.ColorDown := $E3BDA3;
        SelectionAppearance.ColorDownTo := clNone;
        SelectionAppearance.ColorMirrorDown := $E3BDA3;
        SelectionAppearance.ColorMirrorDownTo := clNone;
        SelectionAppearance.BorderColorDown := $E3BDA3;
        SelectionAppearance.GradientDown := ggVertical;
        SelectionAppearance.GradientMirrorDown := ggVertical;

        SelectionAppearance.ColorChecked := $F2D5C2;
        SelectionAppearance.ColorCheckedTo := clNone;
        SelectionAppearance.ColorMirrorChecked := $F2D5C2;
        SelectionAppearance.ColorMirrorCheckedTo := clNone;
        SelectionAppearance.BorderColorChecked := $F2D5C2;
        SelectionAppearance.GradientChecked := ggVertical;
        SelectionAppearance.GradientMirrorChecked := ggVertical;

        SelectionAppearance.ColorDisabled := $B2B2B2;
        SelectionAppearance.ColorDisabledTo := clNone;
        SelectionAppearance.ColorMirrorDisabled := $B2B2B2;
        SelectionAppearance.ColorMirrorDisabledTo := clNone;
        SelectionAppearance.BorderColorDisabled := $444444;


        SelectionAppearance.TextColorHot := $424242;
        SelectionAppearance.TextColorDown := $424242;
        SelectionAppearance.Rounded := false;

      end;
    tsOffice2016Black:
      begin
        BorderDropDownColor := $4E4E4E;
        Rounded :=false;

        Appearance.Color := $363636;
        Appearance.ColorTo := $363636;
        Appearance.ColorMirror := $363636;
        Appearance.ColorMirrorTo := $363636;
        Appearance.BorderColor := $444444;
        Appearance.Gradient := ggVertical;
        Appearance.GradientMirror := ggVertical;

        Appearance.ColorHot := $6A6A6A;
        Appearance.ColorHotTo := $6A6A6A;
        Appearance.ColorMirrorHot := $6A6A6A;
        Appearance.ColorMirrorHotTo := $6A6A6A;
        Appearance.BorderColorHot := $6A6A6A;
        Appearance.GradientHot := ggVertical;
        Appearance.GradientMirrorHot := ggVertical;

        Appearance.ColorDown := $444444;
        Appearance.ColorDownTo := $444444;
        Appearance.ColorMirrorDown := $444444;
        Appearance.ColorMirrorDownTo := $444444;
        Appearance.BorderColorDown := $444444;
        Appearance.GradientDown := ggVertical;
        Appearance.GradientMirrorDown := ggVertical;

        Appearance.ColorChecked := $575757;
        Appearance.ColorCheckedTo := $575757;
        Appearance.ColorMirrorChecked := $575757;
        Appearance.ColorMirrorCheckedTo := $575757;
        Appearance.BorderColorChecked := $575757;
        Appearance.GradientChecked := ggVertical;
        Appearance.GradientMirrorChecked := ggVertical;

        Appearance.ColorDisabled := $363636;
        Appearance.ColorDisabledTo := clnone;
        Appearance.ColorMirrorDisabled := $363636;
        Appearance.ColorMirrorDisabledTo := clnone;
        Appearance.BorderColorDisabled := $444444;



        CaptionAppearance.Color := $363636;
        CaptionAppearance.TextColor := $A6A6A6;


        DragGripAppearance.BorderColorHot := $6A6A6A;
        DragGripAppearance.Color := $363636;
        DragGripAppearance.ColorTo := DragGripAppearance.Color;
        DragGripAppearance.ColorHot := DragGripAppearance.Color;
        DragGripAppearance.ColorHotTo := DragGripAppearance.Color;
        DragGripAppearance.ColorMirror := DragGripAppearance.Color;
        DragGripAppearance.ColorMirrorTo := DragGripAppearance.Color;
        DragGripAppearance.ColorMirrorHot := DragGripAppearance.Color;
        DragGripAppearance.ColorMirrorHotTo := DragGripAppearance.Color;

           SelectionAppearance.Color := $363636;
        SelectionAppearance.ColorTo := $363636;
        SelectionAppearance.ColorMirror := $363636;
        SelectionAppearance.ColorMirrorTo := $363636;
        SelectionAppearance.BorderColor := $444444;
        SelectionAppearance.Gradient := ggVertical;
        SelectionAppearance.GradientMirror := ggVertical;

        SelectionAppearance.ColorHot := $6A6A6A;
        SelectionAppearance.ColorHotTo := $6A6A6A;
        SelectionAppearance.ColorMirrorHot := $6A6A6A;
        SelectionAppearance.ColorMirrorHotTo := $6A6A6A;
        SelectionAppearance.BorderColorHot := $6A6A6A;
        SelectionAppearance.GradientHot := ggVertical;
        SelectionAppearance.GradientMirrorHot := ggVertical;

        SelectionAppearance.ColorDown := $444444;
        SelectionAppearance.ColorDownTo := $444444;
        SelectionAppearance.ColorMirrorDown := $444444;
        SelectionAppearance.ColorMirrorDownTo := $444444;
        SelectionAppearance.BorderColorDown := $444444;
        SelectionAppearance.GradientDown := ggVertical;
        SelectionAppearance.GradientMirrorDown := ggVertical;

        SelectionAppearance.ColorChecked := $575757;
        SelectionAppearance.ColorCheckedTo := $575757;
        SelectionAppearance.ColorMirrorChecked := $575757;
        SelectionAppearance.ColorMirrorCheckedTo := $575757;
        SelectionAppearance.BorderColorChecked := $575757;
        SelectionAppearance.GradientChecked := ggVertical;
        SelectionAppearance.GradientMirrorChecked := ggVertical;

        SelectionAppearance.ColorDisabled := $363636;
        SelectionAppearance.ColorDisabledTo := clnone;
        SelectionAppearance.ColorMirrorDisabled := $363636;
        SelectionAppearance.ColorMirrorDisabledTo := clnone;
        SelectionAppearance.BorderColorDisabled := $444444;

        SelectionAppearance.TextColorHot := $A6A6A6;
        SelectionAppearance.TextColorDown := $A6A6A6;
        SelectionAppearance.Rounded := false;

      end;


    tsCustom:
      begin
      end;
  end;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeSelector.SetSelectorPanelItems;
var
  i: integer;
begin
  if Assigned(FSelectorPanel) then
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
        Picture.Assign(Tools.Items[i].Picture);
        BackGroundColor := Tools.Items[i].BackGroundColor;
      end;
    end;

    FSelectorPanel.ItemIndex := FSelectedIndex;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeSelector.SetSelectorPanel;
begin
  if not Assigned(FSelectorPanel) and Assigned(FDropDownWindow) then
  begin
    FSelectorPanel := TAdvSelectorPanel.Create(FDropDownWindow);
    FSelectorPanel.Parent := FDropDownWindow;
  end;

  if not Assigned(FSelectorPanel) then
    Exit;
    
  if FSelectorPanel.Parent <> FDropDownWindow then
    FSelectorPanel.Parent := FDropDownWindow;

  FSelectorPanel.WindowBorderColor := FBorderDropDownColor;
  FSelectorPanel.OnShouldHide := OnDropDownPanelShouldHide;
  FSelectorPanel.OnSelect := OnDropDownPanelSelect;
  FSelectorPanel.OnHotTool := OnDropDownPanelHotTool;
  FSelectorPanel.OnFloating := OnDropDownPanelFloating;
  FSelectorPanel.OnCloseBtnClick := OnDropDownPanelClose;

  if Assigned(FOnDrawTool) then
    FSelectorPanel.OnDrawItem := OnDropDownPanelDrawTool
  else
    FSelectorPanel.OnDrawItem := nil;

  FSelectorPanel.Initialize;

  FSelectorPanel.Color := ColorDropDown;
  FSelectorPanel.ColorTo := ColorDropDownTo;
  FSelectorPanel.ColorFloating := ColorDropDownFloating;
  FSelectorPanel.ColorFloatingTo := ColorDropDownFloatingTo;


  FSelectorPanel.BorderColor := Appearance.BorderColor; //BorderColor;
  FSelectorPanel.BorderDownColor := FSelectionAppearance.BorderColorDown; // FBorderDownColor;
  FSelectorPanel.BorderHotColor := FSelectionAppearance.BorderColorHot; // BorderHotColor;
  FSelectorPanel.BorderSelectedColor := FSelectionAppearance.BorderColorChecked; // BorderSelectedColor;

  FSelectorPanel.ColorDown := FSelectionAppearance.ColorDown; // FColorSelectionDown;
  FSelectorPanel.ColorDownTo := FSelectionAppearance.ColorDownTo; // FColorSelectionDownTo;
  FSelectorPanel.ColorHot := FSelectionAppearance.ColorHot; // FColorSelectionHot;
  FSelectorPanel.ColorHotTo := FSelectionAppearance.ColorHotTo; // FColorSelectionHotTo;
  FSelectorPanel.ColorSelected := FSelectionAppearance.ColorChecked; // ColorSelected;
  FSelectorPanel.ColorSelectedTo := FSelectionAppearance.ColorCheckedTo; // ColorSelectedTo;

  FSelectorPanel.SelectionAppearance.Assign(FSelectionAppearance);
  FSelectorPanel.DragGripAppearance.Assign(DragGripAppearance);
  FSelectorPanel.GripPosition := Self.DragGripPosition;
  FSelectorPanel.CaptionAppearance.Assign(Self.CaptionAppearance);
  FSelectorPanel.SelectionAppearance.Assign(Self.SelectionAppearance);

  FSelectorPanel.Images := ToolImages;

  FSelectorPanel.AllowFloating := AllowFloating;
  FSelectorPanel.Caption := Self.Caption;

  SetSelectorPanelItems;
  FSelectorPanel.ButtonsPerRow := ButtonsPerRow;

  FSelectorPanel.SetItemsPosition;
  FDropDownWindow.SelectorPanel := FSelectorPanel;

  FSelectorPanel.Left := 0;
  FSelectorPanel.Top := 0;

  FSelectorPanel.TwoColorImages := TwoColorImages;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeSelector.SetDropDownCheck(const Value: Boolean);
begin
  if (FDropDownCheck <> Value) then
  begin
    FDropDownCheck := Value;
    if FDropDownCheck then
      SetStyle(bsCheck)
    else
      SetStyle(bsButton);
  end;
end;

procedure TAdvCustomOfficeSelector.SetDropDownCount(const Value: integer);
begin
  FDropDownCount := Value;
end;

procedure TAdvCustomOfficeSelector.SetDroppedDown(const Value: boolean);
begin
  if Value then
    ShowDropDown
  else
    HideDropDown;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeSelector.SetTGradientDirection(
  const Value: TGradientDirection);
begin
  FGradientDirection := Value;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeSelector.DropDownWindowClose(Sender: TObject; var Action: TCloseAction);
begin
  OnDropDownPanelClose(Self);

  Action := caFree;
  //if Assigned(FDropDownWindow.SelectorPanel) and (FDropDownWindow.SelectorPanel.Parent = FDropDownWindow) then
    //FDropDownWindow.SelectorPanel.Parent := nil;
  KeepDown := false;

  if FAppHintPause <> -1 then
    Application.HintPause := FAppHintPause;
  FAppHintPause := -1;

  FDropDownWindow := nil;
  FSelectorPanel := nil;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeSelector.OnDropDownWindowHide(Sender: TObject);
begin
  State := absUp;
  Invalidate;
  SetDroppedDown(False);

  SendMessage(FDropDownWindow.Handle, WM_CLOSE, 0, 0);
  FDropDownWindow := nil;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeSelector.OnDropDownPanelShouldHide(Sender: TObject);
begin
  if Assigned(FSelectorPanel) and (FSelectorPanel.FFloating) then
  begin
    if CloseOnSelect then
      HideDropDown;
  end
  else
    HideDropDown;
end;

//------------------------------------------------------------------------------

function TAdvCustomOfficeSelector.GetSelectedIndex: integer;
begin
  Result := FSelectedIndex;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeSelector.SetSelectedIndex(const Value: integer);
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

procedure TAdvCustomOfficeSelector.SetTools(const Value: TAdvSelectorItems);
begin
  FTools := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeSelector.OnDropDownPanelSelect(Sender: TObject);
begin
  if Assigned(FSelectorPanel) then
  begin
    SelectedIndex := FSelectorPanel.ItemIndex;
  end;
  OnToolSelect;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeSelector.SetButtonsPerRow(const Value: TNoOfButtons);
begin
  FButtonsPerRow := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeSelector.OnDropDownPanelHotTool(Sender: TObject;
  HotItemIndex: integer);
begin
  if Assigned(FOnHotTool) then
    FOnHotTool(self, HotItemIndex);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeSelector.OnDropDownPanelFloating(Sender: TObject);
begin
  if Assigned(FSelectorPanel) then
  begin
    if Assigned(FDropDownWindow) then
      FDropDownWindow.Height := FDropDownWindow.Height + CAPTION_HEIGHT;

    FSelectorPanel.SetItemsPosition;

    if Assigned(FDropDownWindow) then
      FDropDownWindow.SetWindowSize;

    FSelectorPanel.FMouseDown := false;
    FMouseDown := false;
  end;

  SetDroppedDown(False);
  if Assigned(FOnDropDownFloat) then
    FOnDropDownFloat(Self);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeSelector.OnDropDownPanelClose(Sender: TObject);
begin
  if Assigned(FOnDropDownClose) then
    FOnDropDownClose(Self);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeSelector.OnToolSelect;
begin
  if Assigned(FSelectorPanel) and (FSelectorPanel.ItemIndex >= 0) then
  begin
    DoSelect(FSelectorPanel.ItemIndex, Tools[FSelectorPanel.ItemIndex]);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeSelector.OnDropDownPanelDrawTool(Sender: TObject;
  ItemIndex: integer; R: TRect);
begin
  if Assigned(FOnDrawTool) and Assigned(FSelectorPanel) then
    FOnDrawTool(self, FSelectorPanel.Canvas, ItemIndex, R);
end;

//------------------------------------------------------------------------------

function TAdvCustomOfficeSelector.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER, REL_VER), MakeWord(MIN_VER, MAJ_VER));
end;

//------------------------------------------------------------------------------

function TAdvCustomOfficeSelector.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn))) + '.' + IntToStr(Lo(Hiword(vn))) + '.' + IntToStr(Hi(Loword(vn))) + '.' + IntToStr(Lo(Loword(vn)));
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeSelector.SetVersion(const Value: string);
begin

end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeSelector.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if ((Key = DROPDOWN_KEY) and (GetKeystate(VK_MENU) and $8000 = $0)) or (Key = VK_SPACE) then
    DoDropDown;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeSelector.ShowDropDown;
begin
  DoDropDown;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeSelector.SetTwoColorImages(const Value: Boolean);
begin
  FTwoColorImages := Value;
  if FTwoColorImages then
    ChangeImagesColor(FForeGroundImageColor, FBackGroundImageColor);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeSelector.ChangeImagesColor(ForeGColor,
  BkGColor: TColor);
var
  I: Integer;
  bmp: TBitMap;
  ImgList: TCustomImageList;
begin
  if Assigned(FToolImages) then
  begin
    ImgList:= TCustomImageList.Create(self);
    ImgList.Width := FToolImages.Width;
    ImgList.Height := FToolImages.Height;
    for I := 0 to FToolImages.Count-1 do
    begin
      bmp := TBitMap.Create;
      try
        bmp.Width := FToolImages.Width;
        bmp.Height := FToolImages.Height;

        {$IFDEF DELPHI7_LVL}
        if FToolImages.GetBitmap(I, bmp) then
        {$ELSE}
        FToolImages.GetBitmap(I, bmp);
        {$ENDIF}
        begin
          ChangeBackAndForeGroundColors(bmp, FOldForeGroundImgColor, FOldBkGroundImgColor, ForeGroundImageColor, BackGroundImageColor);
          bmp.TransparentMode := tmAuto;
          bmp.Transparent := True;
          ImgList.Add(bmp, nil);
        end;
      finally
        bmp.Free;
      end;
    end;
    FToolImages.Clear;
    FToolImages.AddImages(ImgList);
    ImgList.Free;
    FOldForeGroundImgColor := ForeGroundImageColor;
    FOldBkGroundImgColor := BackGroundImageColor;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeSelector.SetForeGroundImageColor(const Value: TColor);
begin
  if FForeGroundImageColor <> Value then
  begin
    FForeGroundImageColor := Value;
    TwoColorImages := TwoColorImages; // To reflect color change
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeSelector.SetStretchImageDraw(const Value: Boolean);
begin
  if FStretchImageDraw <> Value then
  begin
    FStretchImageDraw := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeSelector.SetSelectionAppearance(
  const Value: TSelectionAppearance);
begin
  FSelectionAppearance.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeSelector.OnSelectionAppearanceChanged(
  Sender: TObject);
begin
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeSelector.SetDragGripAppearance(
  const Value: TVistaBackgroundHot);
begin
  FDragGripAppearance.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeSelector.SetCaptionAppearance(
  const Value: TGradientCaption);
begin
  FCaptionAppearance.Assign(Value);
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
var
  f: TCustomForm;

begin
  inherited CreateParams(Params);
  //Params.Style := Params.Style - WS_BORDER;
  {
  if (Win32Platform = VER_PLATFORM_WIN32_NT) and
     ((Win32MajorVersion > 5) or
      ((Win32MajorVersion = 5) and (Win32MinorVersion >= 1))) then
    Params.WindowClass.Style := Params.WindowClass.Style or CS_DROPSHADOW;

  Params.ExStyle := Params.ExStyle or WS_EX_TOPMOST; }

  if (Win32Platform = VER_PLATFORM_WIN32_NT) and
     ((Win32MajorVersion > 5) or
      ((Win32MajorVersion = 5) and (Win32MinorVersion >= 1))) then
    Params.WindowClass.Style := Params.WindowClass.Style or CS_DROPSHADOW;

  if FOwner is TWinControl then
  begin
    if Assigned(FOwner) then
      f := GetParentForm(Fowner as TWinControl)
    else
      f := nil;
  end
  else
    f := nil;

  if Assigned(f) then
    Params.WndParent := f.Handle;
end;

//------------------------------------------------------------------------------

destructor TSelectorDropDownWindow.Destroy;
begin
  FHideTimer.Enabled := false;
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


//------------------------------------------------------------------------------

procedure TSelectorDropDownWindow.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
end;

//------------------------------------------------------------------------------

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

procedure TSelectorDropDownWindow.SetSelectorPanel(
  const Value: TAdvCustomSelectorPanel);
begin
  FSelectorPanel := Value;
  FSelectorPanel.DropDownWindow := Self;
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

  if FBlockHide then
    Exit;

  if Message.Active = integer(False) then
  begin
    if HideOnDeactivate and Visible then
    begin
      if Assigned(FSelectorPanel) and FSelectorPanel.FFloating then
      begin
      end
      else
      begin
        Hide;
        FHideTimer.Enabled := true;
      end;
    end;
  end
  else
    if Assigned(FSelectorPanel) then
    begin
      FSelectorPanel.SetFocus;
      SendMessage(GetParentWnd, WM_NCACTIVATE, 1, 0);
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
    message.Result := HTBOTTOMRIGHT;
end;

//------------------------------------------------------------------------------

procedure TSelectorDropDownWindow.WMWindowPosChanging(
  var Message: TWMWindowPosChanging);
begin
  inherited;
end;

//------------------------------------------------------------------------------

{ TAdvCustomSelectorPanel }

constructor TAdvCustomSelectorPanel.Create(AOwner: TComponent);
begin
  inherited;
  FOwner := AOwner;
  BevelOuter := bvNone;
  BevelWidth := 1;
  Color := $00F7F8F9;
  FColorTo := clNone;
  FColorFloating := RGB(196, 219, 249);
  FColorFloatingTo := clNone;
  FWindowBorderColor := clGray;
  FGradientDirection := gdHorizontal;
  FDragGripAppearance := TVistaBackgroundHot.Create;
  FGripPosition := gpTop;
  FCaptionAppearance := TGradientCaption.Create;
  DoubleBuffered := true;
end;

//------------------------------------------------------------------------------

destructor TAdvCustomSelectorPanel.Destroy;
begin
  FDragGripAppearance.Free;
  FCaptionAppearance.Free;
  inherited;
end;

//------------------------------------------------------------------------------

function TAdvCustomSelectorPanel.GetVisibleHeight: integer;
begin
  Result := Height;
end;

procedure TAdvCustomSelectorPanel.HandleKey(Key: word);
begin
  //
end;

//------------------------------------------------------------------------------

function TAdvCustomSelectorPanel.GetCaptionRect: TRect;
begin
  Result := Rect(-1, -1, -1, -1);
  if Floating then
    Result := Rect(2, 2, Width - 2, CAPTION_HEIGHT - 1);
end;

//------------------------------------------------------------------------------

function TAdvCustomSelectorPanel.GetCloseBtnRect: TRect;
begin
  Result := GetCaptionRect;
  if Floating then
  begin
    Result.Top := Result.Top + 1;
    Result.Bottom := Result.Bottom - 1;
    Result.Right := Result.Right - 1;
    Result.Left := Result.Right - (Result.Bottom - Result.Top);
  end;
end;

//------------------------------------------------------------------------------

function TAdvCustomSelectorPanel.GetDragGripRect: TRect;
begin
  Result := Rect(-1, -1, -1, -1);
  if AllowFloating and not Floating then
  begin
    if GripPosition = gpTop then
      Result := Rect(0, 0, Width, DRAGGRIP_HEIGHT)
    else //GripPosition = gpBottom
      Result := Rect(0, Height - DRAGGRIP_HEIGHT, Width, Height);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomSelectorPanel.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  pt: TPoint;
  R: TRect;
begin
  inherited;
  
  pt := Point(X, Y);
  if not Floating and PtInRect(GetDragGripRect, pt) then
  begin
    Floating := True;
    ResetDown;

    ReleaseCapture;
    SendMessage(GetParentForm(Self).Handle, WM_SYSCOMMAND, SC_MOVE+1, 0);
    Exit;
  end
  else if Floating then
  begin
    R := GetCaptionRect;
    R.Right := GetcloseBtnRect.Left;
    if PtInRect(R, pt) then
    begin
      ReleaseCapture;
      SendMessage(GetParentForm(Self).Handle, WM_SYSCOMMAND, SC_MOVE+1, 0);
      Exit;
    end;

    if PtInRect(GetCloseBtnRect, pt) then
    begin
      if not FCloseBtnDown then
      begin
        FCloseBtnDown := True;
        DrawCloseBtn;
      end;
    end
    else if FCloseBtnDown then
    begin
      FCloseBtnDown := False;
      InvalidateCloseBtn;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomSelectorPanel.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  pt: TPoint;
begin
  inherited;

  if Floating then
  begin
    pt := Point(X, Y);
    if PtInRect(GetCloseBtnRect, pt) then
    begin
      if FCloseBtnDown then
      begin
        FCloseBtnDown := False;
        DrawCloseBtn;
        CloseBtnClick;
      end;
    end;

    if FCloseBtnDown then
    begin
      FCloseBtnDown := False;
      InvalidateCloseBtn;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomSelectorPanel.InvalidateDragGrip;
var
  R: TRect;
begin
  if AllowFloating and not Floating then
  begin
    R := GetDragGripRect;
    InvalidateRect(Handle, @R, True);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomSelectorPanel.InvalidateCloseBtn;
var
  R: TRect;
begin
  R := GetCloseBtnRect;
  InvalidateRect(Handle, @R, True);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomSelectorPanel.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  pt: TPoint;
begin
  inherited;
  pt := Point(X, Y);

  if not Floating and PtInRect(GetDragGripRect, pt) then
  begin
    if Self.Cursor <> crSizeAll then
      Self.Cursor := crSizeAll;

    if not FDragGripHot then
    begin
      FDragGripHot := True;
      InvalidateDragGrip;
    end;
  end
  else
  begin
    if Self.Cursor = crSizeAll then
      Self.Cursor := crDefault;
    if FDragGripHot then
    begin
      FDragGripHot := False;
      InvalidateDragGrip;
    end;
  end;

  if Floating then
  begin
    if PtInRect(GetCloseBtnRect, pt) then
    begin
      if not FCloseBtnHot then
      begin
        FCloseBtnHot := True;
        if (ssLeft in Shift) then
          FCloseBtnDown := True;
        DrawCloseBtn;
      end;
    end
    else if FCloseBtnHot then
    begin
      FCloseBtnHot := False;
      FCloseBtnDown := False;
      InvalidateCloseBtn;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomSelectorPanel.CloseBtnClick;
begin
  if Assigned(FDropDownWindow) then
  begin
    FDropDownWindow.Visible := False;
  end;

  if Assigned(FOnCloseBtnClick) then
    FOnCloseBtnClick(Self);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomSelectorPanel.DrawCloseBtn;
var
  R: TRect;
  Clr, ClrTo, ClrMirror, ClrMirrorTo, BrClr, TxtClr: TColor;
  GradU, GradB: TGDIPGradient;
begin
  if Floating then
  begin
    R := GetCloseBtnRect;

    with FCaptionAppearance.ButtonAppearance do
    begin
      TxtClr := FCaptionAppearance.TextColor;
      Clr := Color;
      ClrTo := ColorTo;
      ClrMirror := ColorMirror;
      ClrMirrorTo := ColorMirrorTo;
      GradU := Gradient;
      GradB := GradientMirror;
      BrClr := BorderColor;

      if FCloseBtnDown then
      begin
        TxtClr := FCaptionAppearance.TextColorDown;
        Clr := ColorDown;
        ClrTo := ColorDownTo;
        ClrMirror := ColorMirrorDown;
        ClrMirrorTo := ColorMirrorDownTo;
        GradU := GradientDown;
        GradB := GradientMirrorDown;
        BrClr := BorderColorDown;
      end
      else if FCloseBtnHot then
      begin
        TxtClr := FCaptionAppearance.TextColorHot;
        Clr := ColorHot;
        ClrTo := ColorHotTo;
        ClrMirror := ColorMirrorHot;
        ClrMirrorTo := ColorMirrorHotTo;
        GradU := GradientHot;
        GradB := GradientMirrorHot;
        BrClr := BorderColorHot;
      end;

      if FCloseBtnHot or FCloseBtnDown then
      begin
        if (ClrTo <> clNone) then
        begin
          DrawVistaGradient(Canvas, R, Clr, ClrTo, ClrMirror, ClrMirrorTo, BrClr, GradU, GradB, '', Canvas.Font, Enabled, False, aaClearType, False, False, tpTop);
        end
        else
        begin
          Canvas.Brush.Color := Clr;
          Canvas.Pen.Color := Clr;
          Canvas.Rectangle(R);
        end;
      end;

      DrawCross(Canvas, R, TxtClr, 7)
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomSelectorPanel.Paint;
var
  Clr, ClrTo: TColor;
  R, DgR, R2, TextR: TRect;
  i: Integer;
  DGClr, DGClrTo, DGClrMirror, DGClrMirrorTo, DGBrClr: TColor;
  DGGrdU, DGGrdB: TGDIPGradient;
begin
  //inherited;
  R := Rect(0, 0, Width, Height);

  Clr := Color;
  ClrTo := ColorTo;

  if Floating then
  begin
    Clr := ColorFloating;
    ClrTo := ColorFloatingTo;
  end;

  if ClrTo <> clNone then
    DrawGradient(Canvas, Clr, ClrTo, 40, R, FGradientDirection = gdHorizontal)
  else
  begin
    Canvas.Brush.Color := Clr;
    Canvas.Pen.Color := Clr;
    Canvas.Rectangle(R);
  end;

  //---- Draw Caption
  if Floating then
  begin
    R2 := GetCaptionRect;

    if (CaptionAppearance.Color <> clNone) or (CaptionAppearance.ColorTo <> clNone) then
    begin
      if (CaptionAppearance.Color <> clNone) and (CaptionAppearance.ColorTo <> clNone) then
        DrawGradient(Canvas, CaptionAppearance.Color, CaptionAppearance.ColorTo, 80, R2, CaptionAppearance.Direction = gdHorizontal)
      else
      begin
        Canvas.Pen.Color := CaptionAppearance.Color;
        Canvas.Brush.Color := CaptionAppearance.Color;
        Canvas.Rectangle(R2);
      end;
    end;

    if CaptionAppearance.BorderColor <> clNone then
    begin
      Canvas.Pen.Color := CaptionAppearance.BorderColor;
      Canvas.Brush.Style := bsClear;
      Canvas.Rectangle(R2);
    end;

    if Caption <> '' then
    begin
      Canvas.Brush.Style := bsClear;
      Canvas.Font.Style := [fsBold];
      Canvas.Font.Color := CaptionAppearance.TextColor;
      TextR := Rect(R2.Left + 3, R2.Top, GetCloseBtnRect.Left - 2, R2.Bottom);
      DrawText(Canvas.Handle, PChar(Caption), -1, TextR, DT_SINGLELINE or DT_VCENTER);
    end;

    DrawCloseBtn;
  end;
  //----

  //---- Draw DragGrip
  if AllowFloating and (not Floating) then
  begin
    DGClr := DragGripAppearance.Color;
    DGClrTo := DragGripAppearance.ColorTo;
    DGClrMirror := DragGripAppearance.ColorMirror;
    DGClrMirrorTo := DragGripAppearance.ColorMirrorTo;
    DGBrClr := DragGripAppearance.BorderColor;
    DGGrdU := DragGripAppearance.Gradient;
    DGGrdB := DragGripAppearance.GradientMirror;
    if FDragGripHot then
    begin
      DGClr := DragGripAppearance.ColorHot;
      DGClrTo := DragGripAppearance.ColorHotTo;
      DGClrMirror := DragGripAppearance.ColorMirrorHot;
      DGClrMirrorTo := DragGripAppearance.ColorMirrorHotTo;
      DGBrClr := DragGripAppearance.BorderColorHot;
      DGGrdU := DragGripAppearance.GradientHot;
      DGGrdB := DragGripAppearance.GradientMirrorHot;
    end;

    DgR := GetDragGripRect;
    DrawVistaGradient(Canvas, DgR, DGClr, DGClrTo, DGClrMirror, DGClrMirrorTo, DGBrClr,
      DGGrdU, DGGrdB, '', Canvas.Font, Enabled, False, aaClearType, False, False, tpTop);

    R2 := DgR;
    R2.Left := 1 + (DgR.Right - (4*4)) div 2;
    R2.Top := DgR.Top + 3;
    for i := 1 to 4 do
    begin
      Canvas.Brush.Color := clWhite;
      Canvas.Pen.Color := clWhite;
      Canvas.Rectangle(R2.Left + 1, R2.Top + 1, R2.Left + 3, R2.Top + 3);

      Canvas.Brush.Color := BlendColor(DGBrClr, clBlack, 50); //clBtnShadow;
      Canvas.Pen.Color := Canvas.Brush.Color;
      Canvas.Rectangle(R2.Left, R2.Top, R2.Left + 2, R2.Top + 2);
      R2.Left := R2.Left + 4;
    end;
  end;
  //----

  Canvas.Brush.Style := bsClear;
  Canvas.Pen.Color := FWindowBorderColor;

  Canvas.MoveTo(0, 0);
  Canvas.LineTo(0, Height);

  if Assigned(DropDownWindow) then
  begin
    if FDropDownWindow.ShowFullBorder then
    begin
      Canvas.MoveTo(0, 0);
      Canvas.LineTo(Width - 1, 0);
      Canvas.MoveTo(0, Height - 1);
      Canvas.LineTo(Width - 1, Height - 1);
    end
    else
    begin
      if FDropDownWindow.ShowAbove then
      begin
        Canvas.MoveTo(0, 0);
        Canvas.LineTo(Width - 1, 0);

        if FDropDownWindow.ShowLeft then
        begin
          Canvas.MoveTo(0, Height - 1);
          Canvas.LineTo(Width - TWinControl(FDropDownWindow.Owner).Width + 1, Height - 1);
        end
        else
        begin
          Canvas.MoveTo(TWinControl(FDropDownWindow.Owner).Width-1, Height - 1);
          Canvas.LineTo(Width - 1, Height - 1);
        end;
      end
      else
      begin
        if FDropDownWindow.ShowLeft then
        begin
          Canvas.MoveTo(0, 0);
          Canvas.LineTo(Width - TWinControl(FDropDownWindow.Owner).Width + 1, 0);
        end
        else
        begin
          Canvas.MoveTo(TWinControl(FDropDownWindow.Owner).Width - 1, 0);
          Canvas.LineTo(Width, 0);
        end;

        Canvas.MoveTo(0, Height - 1);
        Canvas.LineTo(Width - 1, Height - 1);
      end;
    end;
  end;

  Canvas.MoveTo(Width - 1, Height);
  Canvas.LineTo(Width - 1, {0}-1);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomSelectorPanel.ResetDown;
begin

end;

//------------------------------------------------------------------------------

procedure TAdvCustomSelectorPanel.SetColorTo(const Value: TColor);
begin
  FColorTo := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomSelectorPanel.SetTGradientDirection(
  const Value: TGradientDirection);
begin
  FGradientDirection := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomSelectorPanel.SetWindowBorderColor(
  const Value: TColor);
begin
  FWindowBorderColor := Value;
end;

procedure TAdvCustomSelectorPanel.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := DLGC_WANTALLKEYS or DLGC_WANTARROWS;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomSelectorPanel.SetDragGripAppearance(
  const Value: TVistaBackgroundHot);
begin
  FDragGripAppearance.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomSelectorPanel.Initialize;
begin
  FFloating := False;
  FCloseBtnHot := False;
  FCloseBtnDown := False;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomSelectorPanel.SetFloating(const Value: Boolean);
begin
  if (FFloating <> Value) then
  begin
    FFloating := Value;
    if Value then
    begin
      if Assigned(FOnFloating) then
        FOnFloating(Self);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomSelectorPanel.SetCaptionAppearance(
  const Value: TGradientCaption);
begin
  FCaptionAppearance.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomSelectorPanel.CMMouseLeave(var Message: TMessage);
begin
  if FCloseBtnHot or FCloseBtnDown then
  begin
    FCloseBtnHot := False;
    FCloseBtnDown := False;
    InvalidateCloseBtn;
  end;
  if FDragGripHot then
  begin
    FDragGripHot := False;
    InvalidateDragGrip;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomSelectorPanel.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;

  if (Key in [VK_ESCAPE, DROPDOWN_KEY]) and (FOwner is TSelectorDropDownWindow) then
    TSelectorDropDownWindow(FOwner).Hide;

  if (Key in [VK_LEFT,VK_DOWN,VK_RIGHT,VK_UP,VK_HOME,VK_END,VK_PRIOR,VK_NEXT,VK_RETURN]) then
    HandleKey(Key);

  if (Key in [VK_RETURN]) and (FOwner is TSelectorDropDownWindow) then
    TSelectorDropDownWindow(FOwner).Hide;
end;

//------------------------------------------------------------------------------

{ TAdvSelectorPanel }

constructor TAdvSelectorPanel.Create(AOwner: TComponent);
begin
  inherited;
  FItems := TAdvSelectorItems.Create(self);
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

  FSelectionAppearance := TSelectionAppearance.Create;

  FTwoColorImages := False;
end;

//------------------------------------------------------------------------------

destructor TAdvSelectorPanel.Destroy;
begin
  FItems.Free;
  FSelectionAppearance.Free;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvSelectorPanel.MouseMove(Shift: TShiftState; X, Y: Integer);
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

      if (FItems.Items[i].Enable) and (FItems.Items[i].ItemType <> itCaption) then
      begin
        FHotItemIndex := i;
        if FMouseDown then // means mouse down move
          FDownItemIndex := i;
        DrawItem(i, true);
      end;

      //if FItems.Items[i].Hint <> '' then
      begin
        Hint := FItems.Items[i].Hint;
        Application.CancelHint;
      end;

      if Assigned(FOnHotTool) and FItems.Items[i].Enable and (FItems.Items[i].ItemType <> itCaption) then
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

procedure TAdvSelectorPanel.DrawItem(Index: integer; RefreshItem: boolean = false);
var
  Gr, R: TRect;
  DTSTYLE: dword;
  bmp: TBitMap;
  DR, R2: TRect;
  i: Integer;
begin
  if csDesigning in ComponentState then
    Exit;

  if (FItems.Count = 0) then
    Exit;

  if (Index < 0) and (Index >= FItems.Count) then
    Exit;

  DTSTYLE := DT_SINGLELINE or DT_VCENTER;

  if FNoPrefix then
    DTSTYLE := DTSTYLE or DT_NOPREFIX;

  R := FItems.Items[Index].ItemRect;
  Gr := FItems.Items[Index].ItemRect;
  Gr.Left := Gr.Left + ButtonMargin;
  Gr.Top := Gr.Top; // + ButtonMargin;

  if (FItems.Items[Index].ItemType = itCaption) then
  begin
    if not Assigned(FOnDrawItem) then
    begin
      if FItems.Items[Index].backGroundColor <> clNone then
      begin
        Canvas.Pen.Color := FItems.Items[Index].backGroundColor;
        Canvas.Brush.Color := FItems.Items[Index].backGroundColor;
        Canvas.Rectangle(R.Left + 3, R.Top + 3, R.Right - 3, R.Bottom - 3);
      end;

      if not (not (Assigned(FImages) and (FItems.Items[Index].ImageIndex >= 0)) and (FItems.Items[Index].Caption = '')) then
      begin
        if (CaptionAppearance.Color <> clNone) or (CaptionAppearance.ColorTo <> clNone) then
        begin
          if (CaptionAppearance.Color <> clNone) and (CaptionAppearance.ColorTo <> clNone) then
            DrawGradient(Canvas, CaptionAppearance.Color, CaptionAppearance.ColorTo, 80, R, CaptionAppearance.Direction = gdHorizontal)
          else
          begin
            Canvas.Pen.Color := CaptionAppearance.Color;
            Canvas.Brush.Color := CaptionAppearance.Color;
            Canvas.Rectangle(R);
          end;
        end;

        if CaptionAppearance.BorderColor <> clNone then
        begin
          Canvas.Pen.Color := CaptionAppearance.BorderColor;
          Canvas.Brush.Style := bsClear;
          Canvas.Rectangle(R);
        end;
      end;
    end;

    Canvas.Brush.Style := bsClear;
    Canvas.Font.Color := CaptionAppearance.TextColor;

    if not FItems.Items[Index].Picture.Empty then
    begin
     Canvas.Draw(GR.Left, GR.Top + (GR.Bottom - GR.Top - FItems.Items[Index].Picture.Height) div 2, FItems.Items[Index].Picture);
     GR.Left := GR.Left + FItems.Items[Index].Picture.Width + 2;
    end;

    if not Assigned(FOnDrawItem) and Assigned(FImages) and (FItems.Items[Index].ImageIndex >= 0) then
    begin
      if TwoColorImages then
      begin
        bmp := TBitmap.Create;
        try
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
        finally
          bmp.Free;
        end;
      end
      else
      begin
        if (FItems.Items[Index].CaptionAlignment = taLeftJustify) then
          FImages.Draw(Canvas, fmaxCaptionLength + Gr.left + 2, Gr.Top + 2, FItems.Items[Index].ImageIndex)
        else
          FImages.Draw(Canvas, Gr.left, Gr.Top + 2, FItems.Items[Index].ImageIndex);
      end;
      if (FItems.Items[Index].CaptionAlignment = taLeftJustify) then
        Gr.Right := Gr.Right - FImages.Width - 2
      else
        Gr.Left := Gr.Left + FImages.Width + 2;
    end;

    if not Assigned(FOnDrawItem) then
    begin
      if (FItems.Items[Index].Caption <> '') then
      begin
        case FItems.Items[Index].CaptionAlignment of
        taLeftJustify:  DrawText(Canvas.Handle, PChar(FItems.Items[Index].Caption), -1, Gr, DTSTYLE or DT_LEFT);
        taRightJustify: DrawText(Canvas.Handle, PChar(FItems.Items[Index].Caption), -1, Gr, DTSTYLE or DT_RIGHT);
        taCenter:       DrawText(Canvas.Handle, PChar(FItems.Items[Index].Caption), -1, Gr, DTSTYLE or DT_CENTER);
        end;
      end;
    end;

    if not Assigned(FOnDrawItem) and not (Assigned(FImages) and (FItems.Items[Index].ImageIndex >= 0)) and (FItems.Items[Index].Caption = '') then
    begin
      R2 := FItems.Items[Index].ItemRect;
      i := R2.Top + (R2.Bottom - R2.Top) div 2;
      Canvas.Pen.Color := WindowBorderColor;
      Canvas.MoveTo(R2.Left, i);
      Canvas.LineTo(R2.Right, i);
    end;

    if Assigned(FOnDrawItem) then
      FOnDrawItem(self, Index, R);
  end
  else if not FItems.Items[Index].Enable then
  begin
    if (SelectionAppearance.ColorDisabledTo <> clNone) then
    begin
      DrawVistaGradient(Canvas, FItems.Items[Index].ItemRect, SelectionAppearance.ColorDisabled, SelectionAppearance.ColorDisabledTo, SelectionAppearance.ColorMirrorDisabled, SelectionAppearance.ColorMirrorDisabledTo, SelectionAppearance.BorderColorDisabled,
          SelectionAppearance.GradientDisabled, SelectionAppearance.GradientMirrorDisabled, '', Canvas.Font, Enabled, False, aaClearType, SelectionAppearance.Rounded, False, tpTop);
    end
    else if (self.SelectionAppearance.ColorDisabled <> clNone) then
    begin
      Canvas.Brush.Color := SelectionAppearance.ColorDisabled;
      Canvas.Pen.Color := SelectionAppearance.ColorDisabled;
      DR := FItems.Items[Index].ItemRect;
      Canvas.Rectangle(DR.Left, DR.Top, DR.Right, DR.Bottom);
    end;

    Canvas.Brush.Style := bsClear;
    Canvas.Font.Color := SelectionAppearance.TextColorDisabled; // clWhite;

    if not Assigned(FOnDrawItem) and (FItems.Items[Index].CaptionAlignment = taLeftJustify) and (FItems.Items[Index].Caption <> '') then
    begin
    Canvas.Brush.Style := bsClear;
    Canvas.Font.Color := clNone;
    Canvas.Font.Color := SelectionAppearance.TextColorDisabled; // clWhite;
     // DrawText(Canvas.Handle, PChar(FItems.Items[Index].Caption), -1, Gr, DTSTYLE);
    end;

    if not FItems.Items[Index].Picture.Empty then
    begin
     Canvas.Draw(GR.Left, GR.Top + (GR.Bottom - GR.Top - FItems.Items[Index].Picture.Height) div 2, FItems.Items[Index].Picture);
     GR.Left := GR.Left + FItems.Items[Index].Picture.Width + 2;
    end;

    if not Assigned(FOnDrawItem) and Assigned(FImages) and (FItems.Items[Index].ImageIndex >= 0) then
    begin
      if TwoColorImages then
      begin
        bmp := TBitmap.Create;
        try
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
        finally
          bmp.Free;
        end;
      end
      else
      begin
        if (FItems.Items[Index].CaptionAlignment = taLeftJustify) then
          FImages.Draw(Canvas, fmaxCaptionLength + Gr.left + 2, Gr.Top + 2, FItems.Items[Index].ImageIndex, False)
        else
          FImages.Draw(Canvas, Gr.left, Gr.Top + 2, FItems.Items[Index].ImageIndex, False);
      end;
      if (FItems.Items[Index].CaptionAlignment = taLeftJustify) then
        Gr.Right := Gr.Right - FImages.Width - 2
      else
        Gr.Left := Gr.Left + FImages.Width + 2;
    end;

    if not Assigned(FOnDrawItem) then
    begin
      Canvas.Brush.Style := bsClear;
      Canvas.Font.Color := clNone;
      Canvas.Font.Color := SelectionAppearance.TextColorDisabled; // clWhite;

      if (FItems.Items[Index].Caption <> '') then
      begin
        case FItems.Items[Index].CaptionAlignment of
        taLeftJustify:  DrawText(Canvas.Handle, PChar(FItems.Items[Index].Caption), -1, Gr, DTSTYLE or DT_LEFT);
        taRightJustify: DrawText(Canvas.Handle, PChar(FItems.Items[Index].Caption), -1, Gr, DTSTYLE or DT_RIGHT);
        taCenter:       DrawText(Canvas.Handle, PChar(FItems.Items[Index].Caption), -1, Gr, DTSTYLE or DT_CENTER);
        end;
      end;
    end;

    if Assigned(FOnDrawItem) then
      FOnDrawItem(self, Index, R);
  end
  else if (Index = FDownItemIndex) then
  begin
    if self.SelectionAppearance.ColorDownTo <> clNone then
    begin
      DrawVistaGradient(Canvas, FItems.Items[Index].ItemRect, SelectionAppearance.ColorDown, SelectionAppearance.ColorDownTo, SelectionAppearance.ColorMirrorDown, SelectionAppearance.ColorMirrorDownTo, SelectionAppearance.BorderColorDown,
          SelectionAppearance.GradientDown, SelectionAppearance.GradientMirrorDown, '', Canvas.Font, Enabled, False, aaClearType, SelectionAppearance.Rounded, False, tpTop);
    end
    else
    begin
      Canvas.Brush.Color := SelectionAppearance.ColorDown;
      Canvas.Pen.Color := SelectionAppearance.ColorDown;
      DR := FItems.Items[Index].ItemRect;
      Canvas.Rectangle(DR.Left, DR.Top, DR.Right, DR.Bottom);
    end;

    Canvas.Brush.Style := bsClear;
    Canvas.Font.Color := SelectionAppearance.TextColorDown; // clWhite;

    if not FItems.Items[Index].Picture.Empty then
    begin
     Canvas.Draw(GR.Left, GR.Top + (GR.Bottom - GR.Top - FItems.Items[Index].Picture.Height) div 2, FItems.Items[Index].Picture);
     GR.Left := GR.Left + FItems.Items[Index].Picture.Width + 2;
    end;

    if not Assigned(FOnDrawItem) and Assigned(FImages) and (FItems.Items[Index].ImageIndex >= 0) then
    begin
      if TwoColorImages then
      begin
        bmp := TBitmap.Create;
        try
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
        finally
          bmp.Free;
        end;
      end
      else
      begin
        if (FItems.Items[Index].CaptionAlignment = taLeftJustify) then
          FImages.Draw(Canvas, fmaxCaptionLength + Gr.left + 2, Gr.Top + 2, FItems.Items[Index].ImageIndex)
        else
          FImages.Draw(Canvas, Gr.left, Gr.Top + 2, FItems.Items[Index].ImageIndex);
      end;
      if (FItems.Items[Index].CaptionAlignment = taLeftJustify) then
        Gr.Right := Gr.Right - FImages.Width - 2
      else
        Gr.Left := Gr.Left + FImages.Width + 2;
    end;

    if not Assigned(FOnDrawItem) then
    begin
      if (FItems.Items[Index].Caption <> '') then
      begin
        case FItems.Items[Index].CaptionAlignment of
        taLeftJustify:  DrawText(Canvas.Handle, PChar(FItems.Items[Index].Caption), -1, Gr, DTSTYLE or DT_LEFT);
        taRightJustify: DrawText(Canvas.Handle, PChar(FItems.Items[Index].Caption), -1, Gr, DTSTYLE or DT_RIGHT);
        taCenter:       DrawText(Canvas.Handle, PChar(FItems.Items[Index].Caption), -1, Gr, DTSTYLE or DT_CENTER);
        end;
      end;
    end;

    if Assigned(FOnDrawItem) then
      FOnDrawItem(self, Index, R);

  end
  else if (Index = FHotItemIndex) then
  begin
    if SelectionAppearance.ColorHotTo{FColorHotTo} <> clNone then
    begin
      //DrawGradient(Canvas, ColorHot, ColorHotTo, 16, Rect(R.Left, R.Top, R.Right - 1, R.Bottom), true)
      DrawVistaGradient(Canvas, R, SelectionAppearance.ColorHot, SelectionAppearance.ColorHotTo, SelectionAppearance.ColorMirrorHot, SelectionAppearance.ColorMirrorHotTo, SelectionAppearance.BorderColorHot,
        SelectionAppearance.GradientHot, SelectionAppearance.GradientMirrorHot, '', Canvas.Font, Enabled, False, aaClearType, SelectionAppearance.Rounded, False, tpTop);
    end
    else
    begin
      Canvas.Brush.Color := SelectionAppearance.ColorHot;
      Canvas.Pen.Color := SelectionAppearance.ColorHot;
      DR := FItems.Items[Index].ItemRect;
      Canvas.Rectangle(DR.Left, DR.Top, DR.Right, DR.Bottom);
    end;

    {if BorderHotColor <> clNone then
    begin
      Canvas.Brush.Style := bsClear;
      Canvas.Pen.Color := BorderHotColor;
      DR := FItems.Items[Index].ItemRect;
      Canvas.Rectangle(DR.Left, DR.Top, DR.Right, DR.Bottom);
    end;}

    Canvas.Brush.Style := bsClear;
    Canvas.Font.Color := SelectionAppearance.TextColorHot; // clBlack;

    if not FItems.Items[Index].Picture.Empty then
    begin
     Canvas.Draw(GR.Left, GR.Top + (GR.Bottom - GR.Top - FItems.Items[Index].Picture.Height) div 2, FItems.Items[Index].Picture);
     GR.Left := GR.Left + FItems.Items[Index].Picture.Width + 2;
    end;

    if not Assigned(FOnDrawItem) and Assigned(FImages) and (FItems.Items[Index].ImageIndex >= 0) then
    begin
      if TwoColorImages then
      begin
        bmp := TBitmap.Create;
        try
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
        finally
          bmp.Free;
        end;
      end
      else
      begin
        if (FItems.Items[Index].CaptionAlignment = taLeftJustify) then
          FImages.Draw(Canvas, fmaxCaptionLength + Gr.left + 2, Gr.Top + 2, FItems.Items[Index].ImageIndex)
        else
          FImages.Draw(Canvas, Gr.left, Gr.Top + 2, FItems.Items[Index].ImageIndex);
      end;
      if (FItems.Items[Index].CaptionAlignment = taLeftJustify) then
        Gr.Right := Gr.Right - FImages.Width - 2
      else
        Gr.Left := Gr.Left + FImages.Width + 2;
    end;

   { if (FItems.Items[Index].CaptionAlignment in [taRightJustify, taCenter]) and (FItems.Items[Index].Caption <> '') then
    begin
      DrawText(Canvas.Handle, PChar(FItems.Items[Index].Caption),-1, Gr, DT_SINGLELINE or DT_VCENTER);
    end; }
    if not Assigned(FOnDrawItem) then
    begin
      if (FItems.Items[Index].Caption <> '') then
      begin
        case FItems.Items[Index].CaptionAlignment of
        taLeftJustify:  DrawText(Canvas.Handle, PChar(FItems.Items[Index].Caption), -1, Gr, DTSTYLE or DT_LEFT);
        taRightJustify: DrawText(Canvas.Handle, PChar(FItems.Items[Index].Caption), -1, Gr, DTSTYLE or DT_RIGHT);
        taCenter:       DrawText(Canvas.Handle, PChar(FItems.Items[Index].Caption), -1, Gr, DTSTYLE or DT_CENTER);
        end;
      end;
    end;

    if Assigned(FOnDrawItem) then
      FOnDrawItem(self, Index, R);

  end
  else // Normal
  begin
    if RefreshItem then
    begin
      InvalidateRect(Handle, @R, True);
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
      if SelectionAppearance.ColorCheckedTo{FColorSelectedTo} <> clNone then
      begin
        //DrawGradient(Canvas, ColorSelected, ColorSelectedTo, 16, FItems.Items[Index].ItemRect, true)
        DrawVistaGradient(Canvas, FItems.Items[Index].ItemRect, SelectionAppearance.ColorChecked, SelectionAppearance.ColorCheckedTo, SelectionAppearance.ColorMirrorChecked, SelectionAppearance.ColorMirrorCheckedTo, SelectionAppearance.BorderColorChecked,
          SelectionAppearance.GradientChecked, SelectionAppearance.GradientMirrorChecked, '', Canvas.Font, Enabled, False, aaClearType, SelectionAppearance.Rounded, False, tpTop);
      end
      else
      begin
        Canvas.Brush.Color := SelectionAppearance.ColorChecked; //ColorSelected;
        Canvas.Pen.Color := SelectionAppearance.ColorChecked; //ColorSelected;
	      DR := FItems.Items[Index].ItemRect;
        Canvas.Rectangle(DR.Left, DR.Top, DR.Right, DR.Bottom);
      end;

      {if BorderSelectedColor <> clNone then
      begin
        Canvas.Brush.Style := bsClear;
        Canvas.Pen.Color := BorderSelectedColor;
	      DR := FItems.Items[Index].ItemRect;
        Canvas.Rectangle(DR.Left, DR.Top, DR.Right, DR.Bottom);
      end;}
    end;

    Canvas.Brush.Style := bsClear;
    Canvas.Font.Color := SelectionAppearance.TextColor;// clBlack;

    if Index = ItemIndex then // Selected Item
      Canvas.Font.Color := SelectionAppearance.TextColorChecked;

    if not FItems.Items[Index].Picture.Empty then
    begin
     Canvas.Draw(GR.Left, GR.Top + (GR.Bottom - GR.Top - FItems.Items[Index].Picture.Height) div 2, FItems.Items[Index].Picture);
     GR.Left := GR.Left + FItems.Items[Index].Picture.Width + 2;
    end;

    if not Assigned(FOnDrawItem) and Assigned(FImages) and (FItems.Items[Index].ImageIndex >= 0) then
    begin
      if TwoColorImages then
      begin
        bmp := TBitmap.Create;
        try
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
        finally
          bmp.Free;
        end;
      end
      else
      begin
        if (FItems.Items[Index].CaptionAlignment = taLeftJustify) then
          FImages.Draw(Canvas, fmaxCaptionLength + Gr.left + 2, Gr.Top + 2, FItems.Items[Index].ImageIndex)
        else
          FImages.Draw(Canvas, Gr.left, Gr.Top + 2, FItems.Items[Index].ImageIndex);
      end;
      if (FItems.Items[Index].CaptionAlignment = taLeftJustify) then
        Gr.Right := Gr.Right - FImages.Width - 2
      else
        Gr.Left := Gr.Left + FImages.Width + 2;
    end;

    if not Assigned(FOnDrawItem) then
    begin
      if (FItems.Items[Index].Caption <> '') then
      begin
        case FItems.Items[Index].CaptionAlignment of
        taLeftJustify:  DrawText(Canvas.Handle, PChar(FItems.Items[Index].Caption), -1, Gr, DTSTYLE or DT_LEFT);
        taRightJustify: DrawText(Canvas.Handle, PChar(FItems.Items[Index].Caption), -1, Gr, DTSTYLE or DT_RIGHT);
        taCenter:       DrawText(Canvas.Handle, PChar(FItems.Items[Index].Caption), -1, Gr, DTSTYLE or DT_CENTER);
        end;
      end;
    end;

    if Assigned(FOnDrawItem) then
      FOnDrawItem(self, Index, R);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSelectorPanel.Paint;
var
  i: integer;
begin
  inherited;

  for i := 0 to FItems.Count - 1 do
  begin
    DrawItem(i);
  end;
end;

procedure TAdvSelectorPanel.ResetDown;
begin
  inherited;
  FDownItemIndex := -1;
  FMouseDown := false;
end;

//------------------------------------------------------------------------------

procedure TAdvSelectorPanel.SetBorderColor(const Value: TColor);
begin
  FBorderColor := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvSelectorPanel.SetImages(const Value: TCustomImageList);
begin
  FImages := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvSelectorPanel.SetItemIndex(const Value: integer);
begin
  if Value < 0 then
  begin
    FItemIndex := -1;
    Exit;
  end;

  if Value < FItems.Count then
  begin
    if (FItems.Items[Value].Enable) and (FItems.Items[Value].ItemType <> itCaption) then
    begin
      if FItemIndex <> Value then
      begin
        FItemIndex := Value;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSelectorPanel.SetItems(const Value: TAdvSelectorItems);
begin
  FItems := Value;
end;

//------------------------------------------------------------------------------

function TAdvSelectorPanel.TotalAutoSizeButtons: integer;
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

procedure TAdvSelectorPanel.AutoSizeBtnSize(var W, H: integer);
var
  i: integer;
  s: string;
begin
  w := MinButtonWidth;
  H := MinButtonHeight;

  for i := 0 to FItems.Count - 1 do
  begin
    if FItems.Items[i].ItemType = itAutoSizeButton then
    begin
      s := FItems.Items[i].Caption;
      if s = '' then
        s :='x';

      if not FItems.Items[i].Picture.Empty then
      begin
        W := Max(W, Canvas.TextWidth(s) + FItems.Items[i].Picture.Width + 2 * ButtonMargin);
        H := Max(H, MAX(Canvas.TextHeight('gh'), FItems.Items[i].Picture.Height) + ButtonMargin); // Single Margin added
      end
      else
      begin
        if Assigned(FImages) and (FItems.Items[i].ImageIndex >= 0) then
        begin
          W := Max(W, Canvas.TextWidth(s) + FImages.Width + (ButtonMargin * 2));
          H := Max(H, MAX(Canvas.TextHeight('gh'), FImages.Height) + ButtonMargin); // Single Margin added
        end
        else
        begin
          W := Max(W, Canvas.TextWidth(s) + (ButtonMargin * 2));
          H := Max(H, Canvas.TextHeight('gh') + ButtonMargin); // Single Margin added
        end;
      end;

      FMaxCaptionLength := Max(FMaxCaptionLength, Canvas.TextWidth(s));
    end;
  end;
end;

//------------------------------------------------------------------------------

function TAdvSelectorPanel.GetMaxWidth: integer;
var
  i, asb, fwb: integer;
begin
  Result := FleftOffSet * 2 + MinButtonWidth; //MINBUTTONSIZE;
  fwb := Result;
  asb := Result;
  AutoSizeBtnSize(asb {GetWidth}, i {dummy});

  for i := 0 to FItems.Count - 1 do
  begin
    if (FItems.Items[i].ItemType in [itFullWidthButton, itCaption]) then
    begin
      if Assigned(FImages) and (FItems.Items[i].ImageIndex >= 0) then
        fwb := Max(fwb, Canvas.TextWidth(FItems.Items[i].Caption) + FImages.Width + (ButtonMargin * 2) + (FleftOffSet * 2))
      else
        fwb := Max(fwb, Canvas.TextWidth(FItems.Items[i].Caption) + (ButtonMargin * 2) + (FleftOffSet * 2));
    end;
  end;

  Result := Max(fwb, asb * min(ButtonsPerRow, TotalAutoSizeButtons) + (FleftOffSet * 2) {(ButtonMargin*2)});
end;

procedure TAdvSelectorPanel.HandleKey(Key: word);
begin

  case Key of
  VK_DOWN: if ItemIndex + ButtonsPerRow < Items.Count then ItemIndex := ItemIndex + ButtonsPerRow;
  VK_UP: if ItemIndex >= ButtonsPerRow then ItemIndex := ItemIndex - ButtonsPerRow;
  VK_LEFT: if ItemIndex > 0 then ItemIndex := ItemIndex - 1;
  VK_RIGHT: if ItemIndex < Items.Count - 1 then ItemIndex := ItemIndex + 1;
  VK_HOME: ItemIndex := 0;
  VK_END: ItemIndex := Items.Count - 1;
  VK_RETURN: if Assigned(FOnSelect) then
               FOnSelect(self);
  end;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvSelectorPanel.SetItemsPosition;
var
  i, c, r, absW, absH, MaxW, bNo: integer;
  ShouldChangeRow: boolean;
begin
  r := FTopOffSet;
  if AllowFloating then
  begin
    if Floating then
      r := r + CAPTION_HEIGHT
    else if (GripPosition = gpTop) then
      r := r + DRAGGRIP_HEIGHT;
  end;  
    
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
    else if (FItems.Items[i].ItemType in [itFullWidthButton, itCaption]) then
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

procedure TAdvSelectorPanel.SetPanelHeight;
var
  h: Integer;
begin
  if FItems.Count > 0 then
  begin
    h := FItems.Items[FItems.Count - 1].ItemRect.Bottom + FTopOffSet;
    if AllowFloating and not Floating and (GripPosition = gpBottom) then
      h := h + DRAGGRIP_HEIGHT;
    Height := h;
    // SetVisibleHeight;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSelectorPanel.SetButtonsPerRow(const Value: TNoOfButtons);
begin
  FButtonsPerRow := Value;
end;

//------------------------------------------------------------------------------

function TAdvSelectorPanel.ItemAtPos(X, Y: integer): integer;
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

procedure TAdvSelectorPanel.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  i: integer;
begin
  if not (FFloating and (Y <= CAPTION_HEIGHT)) then
    FMouseDown := true;

  inherited;

  i := ItemAtPos(X, Y);
  if (i >= 0) and FItems.Items[i].Enable and (FItems.Items[i].ItemType <> itCaption) then
  begin
    FDownItemIndex := i;
    DrawItem(i, true);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSelectorPanel.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  OldItemIndex: Integer;
begin
  inherited;

  FMouseDown := false;

  if (FHotItemIndex = -1) and (FDownItemIndex = -1) then
    Exit;

  if (FDownItemIndex > -1) and (FHotItemIndex > -1) then
  begin
    OldItemIndex := ItemIndex;
    ItemIndex := FDownItemIndex;

    if (OldItemIndex >= 0) then
      DrawItem(OldItemIndex, true);

    if Assigned(FOnSelect) then
      FOnSelect(self);
  end;

  if Assigned(FOnShouldHide) then
    FOnShouldHide(self);

  FHotItemIndex := -1;
  FDownItemIndex := -1;
end;

//------------------------------------------------------------------------------

procedure TAdvSelectorPanel.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvSelectorPanel.WMChar(var Msg: TWMKey);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvSelectorPanel.CMWantSpecialKey(var Msg: TCMWantSpecialKey);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvSelectorPanel.SetButtonMargin(const Value: integer);
begin
  FButtonMargin := Value;
end;

//------------------------------------------------------------------------------


procedure TAdvSelectorPanel.CMMouseLeave(var Message: TMessage);
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

procedure TAdvSelectorPanel.SetMinButtonHeight(const Value: integer);
begin
  FMinButtonHeight := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvSelectorPanel.SetMinButtonWidth(const Value: integer);
begin
  FMinButtonWidth := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvSelectorPanel.SetTwoColorImages(const Value: Boolean);
begin
  FTwoColorImages := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvSelectorPanel.SetSelectionAppearance(
  const Value: TSelectionAppearance);
begin
  FSelectionAppearance.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TAdvSelectorPanel.SetFloating(const Value: Boolean);
begin
  inherited;
end;

//------------------------------------------------------------------------------

{ TAdvSelectorItem }

procedure TAdvSelectorItem.Assign(Source: TPersistent);
begin
  if (Source is TAdvSelectorItem) then
  begin
    BackGroundColor := (Source as TAdvSelectorItem).BackGroundColor;
    Caption := (Source as TAdvSelectorItem).Caption;
    CaptionAlignment := (Source as TAdvSelectorItem).CaptionAlignment;
    ImageIndex := (Source as TAdvSelectorItem).ImageIndex;
    Hint := (Source as TAdvSelectorItem).Hint;
    Value := (Source as TAdvSelectorItem).Value;
    ItemType := (Source as TAdvSelectorItem).ItemType;
    Picture.Assign((Source as TAdvSelectorItem).Picture);
    Tag := (Source as TAdvSelectorItem).Tag;
  end
  else
    Inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvSelectorItem.Changed;
begin

end;

//------------------------------------------------------------------------------

constructor TAdvSelectorItem.Create(Collection: TCollection);
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
  FPicture := TGDIPPicture.Create;
end;

//------------------------------------------------------------------------------

destructor TAdvSelectorItem.Destroy;
begin
  FPicture.Free;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvSelectorItem.SetBackGroundColor(const Value: TColor);
begin
  FBackGroundColor := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvSelectorItem.SetCaption(const Value: string);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSelectorItem.SetEnable(const Value: boolean);
begin
  if FEnable <> Value then
  begin
    FEnable := Value;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSelectorItem.SetHint(const Value: string);
begin
  FHint := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvSelectorItem.SetImageIndex(const Value: integer);
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSelectorItem.SetItemType(const Value: TSelectorItemType);
begin
  if FItemType <> Value then
  begin
    FItemType := Value;
    Changed;
  end;
end;

procedure TAdvSelectorItem.SetPicture(const Value: TGDIPPicture);
begin
  FPicture.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TAdvSelectorItem.SetTag(const Value: integer);
begin
  FTag := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvSelectorItem.SetValue(const Value: string);
begin
  if FValue <> Value then
  begin
    FValue := Value;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvSelectorItem.SetvCaptionAlignment(const Value: TAlignment);
begin
  if FCaptionAlignment <> Value then
  begin
    FCaptionAlignment := Value;
  end;
end;

//------------------------------------------------------------------------------


{ TAdvSelectorItems }

function TAdvSelectorItems.Add: TAdvSelectorItem;
begin
  Result := TAdvSelectorItem(inherited Add);
end;

//------------------------------------------------------------------------------

constructor TAdvSelectorItems.Create(AOwner: TPersistent);
begin
  inherited Create(TAdvSelectorItem);
  FOwner := AOwner;
end;

//------------------------------------------------------------------------------

function TAdvSelectorItems.GetItem(Index: Integer): TAdvSelectorItem;
begin
  Result := TAdvSelectorItem(inherited Items[Index]);
end;

//------------------------------------------------------------------------------

function TAdvSelectorItems.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

//------------------------------------------------------------------------------

function TAdvSelectorItems.Insert(Index: Integer): TAdvSelectorItem;
begin
  Result := TAdvSelectorItem(inherited Insert(Index));
end;

//------------------------------------------------------------------------------

procedure TAdvSelectorItems.SetItem(Index: Integer;
  const Value: TAdvSelectorItem);
begin
  inherited Items[Index] := Value;
end;

//------------------------------------------------------------------------------


{ TAdvColorCubePanel }

procedure TAdvColorCubePanel.CMMouseLeave(var Message: TMessage);
begin
  inherited;
end;

//------------------------------------------------------------------------------

constructor TAdvColorCubePanel.Create(AOwner: TComponent);
begin
  inherited;
  FSelectedColor := clNone;
  FHotIndex := -1;
  FSelectedIndex := -1;
  Initialize;
  ShowRGBHint := true;
end;

//------------------------------------------------------------------------------

destructor TAdvColorCubePanel.Destroy;
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvColorCubePanel.DrawColorCube(Index: integer);
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

procedure TAdvColorCubePanel.DrawAllColorCube;
var
  i: integer;
begin
  for i := 1 to high(FCubeCells) do
  begin
    DrawColorCube(i);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvColorCubePanel.DrawHexagon(aCanvas: TCanvas; P: TPoint; X, Y: integer);
begin
  P.X := P.X - X;
  P.Y := P.Y - y * 2;
  aCanvas.Polygon([Point(P.X, P.Y + Y), Point(P.X + X, P.Y),
    Point(P.X + X * 2, P.Y + Y), Point(P.X + X * 2, P.Y + Y * 3), Point(P.X + X, P.Y + Y * 4), Point(P.X, P.Y + Y * 3)]);
end;

//------------------------------------------------------------------------------

function TAdvColorCubePanel.IndexOfCellAt(X, Y: integer): integer;
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

procedure TAdvColorCubePanel.Initialize;
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

procedure TAdvColorCubePanel.MouseDown(Button: TMouseButton;
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

procedure TAdvColorCubePanel.MouseMove(Shift: TShiftState; X, Y: Integer);
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

procedure TAdvColorCubePanel.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  if Assigned(FOnShouldHide) then
    FOnShouldHide(self);
end;

//------------------------------------------------------------------------------

procedure TAdvColorCubePanel.Paint;
begin
  inherited;
  DrawAllColorCube;
end;

//------------------------------------------------------------------------------

function TAdvColorCubePanel.PtInCell(Index: integer; P: TPoint): Boolean;
begin
  Result := sqr(FCubeCells[Index].CenterPos.X - P.X) + sqr(FCubeCells[Index].CenterPos.Y - P.Y) <= (FCubeSize.X * FCubeSize.X);
end;

//------------------------------------------------------------------------------

procedure TAdvColorCubePanel.SetItemsPosition;
begin
  FHotIndex := -1;
  SetPanelSize;
end;

//------------------------------------------------------------------------------

procedure TAdvColorCubePanel.SetPanelSize;
var
  h: Integer;
begin
  h := 160;

  if AllowFloating then
  begin
    if Floating then
      h := h + CAPTION_HEIGHT
    else //if ((GripPosition = gpBottom) or ((not Floating) and (GripPosition = gpTop))) then
      h := h + DRAGGRIP_HEIGHT;
  end;

  Height := h;
  Width := 182;
end;

//------------------------------------------------------------------------------

procedure TAdvColorCubePanel.SetSelectedColor(const Value: TColor);
begin
  SetSelectedIndexAndColor(Value);
end;

//------------------------------------------------------------------------------

procedure TAdvColorCubePanel.DrawSelectedBorder;
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

procedure TAdvColorCubePanel.DrawHotBorder;
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

procedure TAdvColorCubePanel.SetSelectedIndexAndColor(clr: TColor;
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

procedure TAdvColorCubePanel.SetShowRGBHint(const Value: Boolean);
begin
  FShowRGBHint := Value;
  ShowHint := Value;
end;

//------------------------------------------------------------------------------


{ TAdvColorSpectrumPanel }

procedure TAdvColorSpectrumPanel.CMMouseLeave(var Message: TMessage);
begin
  inherited;
end;

//------------------------------------------------------------------------------

constructor TAdvColorSpectrumPanel.Create(AOwner: TComponent);
var
  tbmp: TBitmap;
begin
  inherited;
  FSpectrumImage := TImage.Create(self);
  FSpectrumImage.Parent := self;
  FSpectrumImage.Height := 128;
  FSpectrumImage.Width := 128;

  tbmp := TBitmap.Create;
  try
    tbmp.LoadFromResourceName(HInstance, 'OFFSPECTRUM');
    FSpectrumImage.Picture.Assign(tbmp);
  finally
    tbmp.Free;
  end;

  FSpectrumImage.OnMouseMove := SpectrumImageMouseMove;
  FSpectrumImage.OnMouseDown := SpectrumImageMouseDown;
  FSpectrumImage.OnMouseUp := SpectrumImageMouseUp;
  FHotColor := clNone;
  FSelectedColor := clWhite;

  Screen.Cursors[crTMSCur1] := LoadCursor(HInstance, 'TMS_OFFCUR1');
  FSpectrumImage.Cursor := crTMSCur1;
end;

//------------------------------------------------------------------------------

destructor TAdvColorSpectrumPanel.Destroy;
begin
  FSpectrumImage.Free;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvColorSpectrumPanel.DrawFocusPoint;
begin
  {
  FSpectrumImage.Canvas.Pen.Color:= clWhite;
  FSpectrumImage.Canvas.MoveTo(50, 50);
  FSpectrumImage.Canvas.LineTo(100, 100);
  }
end;

//------------------------------------------------------------------------------

procedure TAdvColorSpectrumPanel.DrawHotRect;
begin
  Canvas.Pen.Color := clGray;
  Canvas.Brush.Style := bsClear;
  Canvas.Rectangle(FHotRect.Left - 1, FHotRect.Top - 1, FHotRect.Right + 1, FHotRect.Bottom + 1);

  Canvas.Pen.Color := clWhite;
  Canvas.Brush.Color := FHotColor;
  Canvas.Rectangle(FHotRect.Left, FHotRect.Top, FHotRect.Right, FHotRect.Bottom);
end;

//------------------------------------------------------------------------------

procedure TAdvColorSpectrumPanel.DrawSelectedRect;
begin
  Canvas.Pen.Color := clGray;
  Canvas.Brush.Style := bsClear;
  Canvas.Rectangle(FSelectedRect.Left - 1, FSelectedRect.Top - 1, FSelectedRect.Right + 1, FSelectedRect.Bottom + 1);

  Canvas.Pen.Color := clWhite;
  Canvas.Brush.Color := FSelectedColor;
  Canvas.Rectangle(FSelectedRect.Left, FSelectedRect.Top, FSelectedRect.Right, FSelectedRect.Bottom);
end;

//------------------------------------------------------------------------------

procedure TAdvColorSpectrumPanel.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvColorSpectrumPanel.MouseMove(Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvColorSpectrumPanel.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if Assigned(FOnShouldHide) then
    FOnShouldHide(self);
end;

//------------------------------------------------------------------------------

procedure TAdvColorSpectrumPanel.Paint;
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

procedure TAdvColorSpectrumPanel.SetItemsPosition;
var
  y: Integer;
begin
  SetPanelSize;
  y := 10;
  if AllowFloating then
  begin
   if Floating then
     y := y + CAPTION_HEIGHT
   else if (GripPosition = gpTop) then
     y := y + DRAGGRIP_HEIGHT;
  end;

  FSpectrumImage.Left := (Width - FSpectrumImage.Width) div 2;
  FSpectrumImage.Top := y;
  FSelectedRect := Rect(FSpectrumImage.Left - 1, FSpectrumImage.Top + FSpectrumImage.Height + 8, FSpectrumImage.Left + (FSpectrumImage.Width div 2) - 2, FSpectrumImage.Top + FSpectrumImage.Height + 8 + 20);
  FHotRect := Rect(FSelectedRect.Right + 4, FSelectedRect.Top, FSpectrumImage.Left + FSpectrumImage.Width + 1, FSelectedRect.Bottom);
  FHotColor := clNone;

  DrawFocusPoint;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvColorSpectrumPanel.SetPanelSize;
var
  h: Integer;
begin
  h := 174;

  if AllowFloating then
  begin
    if Floating then
      h := h + CAPTION_HEIGHT
    else //if ((GripPosition = gpBottom) or ((not Floating) and (GripPosition = gpTop))) then
      h := h + DRAGGRIP_HEIGHT;
  end;

  Height := h;
  Width := 150
end;

//------------------------------------------------------------------------------

procedure TAdvColorSpectrumPanel.SetSelectedColor(const Value: TColor);
begin
  if Value <> FSelectedColor then
  begin
    FSelectedColor := Value;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvColorSpectrumPanel.SpectrumImageMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  SelectedColor := FHotColor;
  DrawSelectedRect;
  if Assigned(OnSelect) then
    FOnSelect(self);
end;

//------------------------------------------------------------------------------

procedure TAdvColorSpectrumPanel.SpectrumImageMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  FHotColor := Canvas.Pixels[X + FSpectrumImage.Left, Y + FSpectrumImage.Top];
  DrawHotRect;
end;

//------------------------------------------------------------------------------

procedure TAdvColorSpectrumPanel.SpectrumImageMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin

  if Assigned(FOnShouldHide) then
    FOnShouldHide(self);
end;

//------------------------------------------------------------------------------


{ TAdvOfficePenStyleSelector }

constructor TAdvOfficePenStyleSelector.Create(AOwner: TComponent);
begin
  inherited;
  //if not (csDesigning in ComponentState) then
  begin
    FToolImages := TCustomImageList.Create(self);
    FToolImages.Width := 99;
    FToolImages.Height := 9;
    FToolImages.ResInstLoad(HInstance, rtBitmap, 'OFFPENSTYLE0', clWhite);
    FToolImages.ResInstLoad(HInstance, rtBitmap, 'OFFPENSTYLE1', clWhite);
    FToolImages.ResInstLoad(HInstance, rtBitmap, 'OFFPENSTYLE2', clWhite);
    FToolImages.ResInstLoad(HInstance, rtBitmap, 'OFFPENSTYLE3', clWhite);
    FToolImages.ResInstLoad(HInstance, rtBitmap, 'OFFPENSTYLE4', clWhite);
    FToolImages.ResInstLoad(HInstance, rtBitmap, 'OFFPENSTYLE5', clWhite);
    FToolImages.ResInstLoad(HInstance, rtBitmap, 'OFFPENSTYLE6', clWhite);
    FToolImages.ResInstLoad(HInstance, rtBitmap, 'OFFPENSTYLE7', clWhite);
    //b:= FImages.GetInstRes(ac, rtBitmap	, S, 99, lrTransparent, clWhite);
  end;

  FSelectionType := stOffice;
  FTwoColorImages := True;
  Initialize;
end;

//------------------------------------------------------------------------------

destructor TAdvOfficePenStyleSelector.Destroy;
begin
  //if not (csDesigning in ComponentState) and Assigned(FImages) then
  begin
    FToolImages.Free;
  end;
  inherited;
end;

procedure TAdvOfficePenStyleSelector.DoSelect(Index: Integer;
  Item: TAdvSelectorItem);
begin
  inherited;
  if Assigned(OnSelectPenStyle) then
    OnSelectPenStyle(Self, SelectedPenStyle);
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePenStyleSelector.Initialize;
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

procedure TAdvOfficePenStyleSelector.Loaded;
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePenStyleSelector.SetSelectorPanel;
begin
  //self.TwoColorImages := True;
  inherited;
  if Assigned(FSelectorPanel) then
  begin
    FSelectorPanel.Images := FToolimages;
    FSelectorPanel.SetItemsPosition;
  end;
  //FSelectorPanel.TwoColorImages := True;
end;

//------------------------------------------------------------------------------

function TAdvOfficePenStyleSelector.GetSelectedPenStyle: TPenStyle;
begin
  Result := GetPenStyleAtIndex(SelectedIndex);
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePenStyleSelector.SetSelectedPenStyle(const Value: TPenStyle);
begin
  SelectedIndex := GetIndexOfStyle(Value);
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePenStyleSelector.SetSelectionType(
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

function TAdvOfficePenStyleSelector.GetPenColor: TColor;
begin
  Result := ForeGroundImageColor;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePenStyleSelector.SetPenColor(const Value: TColor);
begin
  ForeGroundImageColor := Value;
  Invalidate;
end;

//------------------------------------------------------------------------------

function TAdvOfficePenStyleSelector.GetPenStyleAtIndex(
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

function TAdvOfficePenStyleSelector.GetIndexOfStyle(
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

{ TAdvOfficeBrushStyleSelector }

constructor TAdvOfficeBrushStyleSelector.Create(AOwner: TComponent);
begin
  inherited;
  //if not (csDesigning in ComponentState) then
  begin
    FToolImages := TCustomImageList.Create(self);
    FToolImages.Width := 102;
    FToolImages.Height := 14;
    FToolImages.ResInstLoad(HInstance, rtBitmap, 'OFFBRUSHSTYLE0', clWhite);
    FToolImages.ResInstLoad(HInstance, rtBitmap, 'OFFBRUSHSTYLE1', clWhite);
    FToolImages.ResInstLoad(HInstance, rtBitmap, 'OFFBRUSHSTYLE2', clWhite);
    FToolImages.ResInstLoad(HInstance, rtBitmap, 'OFFBRUSHSTYLE3', clWhite);
    FToolImages.ResInstLoad(HInstance, rtBitmap, 'OFFBRUSHSTYLE4', clWhite);
    FToolImages.ResInstLoad(HInstance, rtBitmap, 'OFFBRUSHSTYLE5', clWhite);
    FToolImages.ResInstLoad(HInstance, rtBitmap, 'OFFBRUSHSTYLE6', clWhite);
    FToolImages.ResInstLoad(HInstance, rtBitmap, 'OFFBRUSHSTYLE7', clWhite);

    // stBorland
    FToolImages.ResInstLoad(HInstance, rtBitmap, 'OFFBRUSHSTYLE8', clWhite);
    FToolImages.ResInstLoad(HInstance, rtBitmap, 'OFFBRUSHSTYLE9', clWhite);
    FToolImages.ResInstLoad(HInstance, rtBitmap, 'OFFBRUSHSTYLE10', clWhite);
    FToolImages.ResInstLoad(HInstance, rtBitmap, 'OFFBRUSHSTYLE11', clWhite);
    FToolImages.ResInstLoad(HInstance, rtBitmap, 'OFFBRUSHSTYLE12', clWhite);
    FToolImages.ResInstLoad(HInstance, rtBitmap, 'OFFBRUSHSTYLE13', clWhite);
    FToolImages.ResInstLoad(HInstance, rtBitmap, 'OFFBRUSHSTYLE14', clWhite);
  end;

  FSelectionType := stOffice;
  FTwoColorImages := True;
  FStretchImageDraw := False;

  Initialize;
end;

//------------------------------------------------------------------------------

destructor TAdvOfficeBrushStyleSelector.Destroy;
begin
  //if not (csDesigning in ComponentState) and Assigned(FImages) then
  begin
    FToolImages.Free;
  end;
  inherited;
end;

procedure TAdvOfficeBrushStyleSelector.DoSelect(Index: Integer;
  Item: TAdvSelectorItem);
begin
  inherited;
  if Assigned(FOnSelectBrushStyle) then
    FOnSelectBrushStyle(self, SelectedBrushStyle);
end;

//------------------------------------------------------------------------------

function TAdvOfficeBrushStyleSelector.GetBrushColor: TColor;
begin
  Result := ForeGroundImageColor;
end;

//------------------------------------------------------------------------------

function TAdvOfficeBrushStyleSelector.GetBrushStyleAtIndex(
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

function TAdvOfficeBrushStyleSelector.GetIndexOfBrushStyle(
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

function TAdvOfficeBrushStyleSelector.GetSelectedBrushStyle: TBrushStyle;
begin
  Result := GetBrushStyleAtIndex(SelectedIndex);
end;

//------------------------------------------------------------------------------

procedure TAdvOfficeBrushStyleSelector.Initialize;
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

procedure TAdvOfficeBrushStyleSelector.Loaded;
begin
  inherited;

end;

//------------------------------------------------------------------------------

procedure TAdvOfficeBrushStyleSelector.SetBrushColor(const Value: TColor);
begin
  ForeGroundImageColor := Value;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficeBrushStyleSelector.SetSelectedBrushStyle(
  const Value: TBrushStyle);
begin
  SelectedIndex := GetIndexOfBrushStyle(Value);
end;

//------------------------------------------------------------------------------

procedure TAdvOfficeBrushStyleSelector.SetSelectionType(
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

procedure TAdvOfficeBrushStyleSelector.SetSelectorPanel;
begin
  //self.TwoColorImages := True;
  inherited;
  if Assigned(FSelectorPanel) then
  begin
    FSelectorPanel.Images := FToolImages;
    FSelectorPanel.SetItemsPosition;
  end;
  //FSelectorPanel.TwoColorImages := True;
end;

//------------------------------------------------------------------------------

{ TAdvOfficeShadowSelector }

constructor TAdvOfficeShadowSelector.Create(AOwner: TComponent);
begin
  inherited;
  //if not (csDesigning in ComponentState) then
  begin
    FToolImages := TCustomImageList.Create(self);
    FToolImages.Width := 20;
    FToolImages.Height := 20;
    FToolImages.ResInstLoad(HInstance, rtBitmap, 'SHADOW0', clWhite);
    FToolImages.ResInstLoad(HInstance, rtBitmap, 'OFFSHADOW1', clWhite);
    FToolImages.ResInstLoad(HInstance, rtBitmap, 'OFFSHADOW2', clWhite);
    FToolImages.ResInstLoad(HInstance, rtBitmap, 'OFFSHADOW3', clWhite);
    FToolImages.ResInstLoad(HInstance, rtBitmap, 'OFFSHADOW4', clWhite);
    FToolImages.ResInstLoad(HInstance, rtBitmap, 'OFFSHADOW5', clWhite);
    FToolImages.ResInstLoad(HInstance, rtBitmap, 'OFFSHADOW6', clWhite);
    FToolImages.ResInstLoad(HInstance, rtBitmap, 'OFFSHADOW7', clWhite);
    FToolImages.ResInstLoad(HInstance, rtBitmap, 'OFFSHADOW8', clWhite);
    FToolImages.ResInstLoad(HInstance, rtBitmap, 'OFFSHADOW9', clWhite);
    FToolImages.ResInstLoad(HInstance, rtBitmap, 'OFFSHADOW10', clWhite);
    FToolImages.ResInstLoad(HInstance, rtBitmap, 'OFFSHADOW11', clWhite);
    FToolImages.ResInstLoad(HInstance, rtBitmap, 'OFFSHADOW12', clWhite);
    FToolImages.ResInstLoad(HInstance, rtBitmap, 'OFFSHADOW13', clWhite);
    FToolImages.ResInstLoad(HInstance, rtBitmap, 'OFFSHADOW14', clWhite);
    FToolImages.ResInstLoad(HInstance, rtBitmap, 'OFFSHADOW15', clWhite);
    FToolImages.ResInstLoad(HInstance, rtBitmap, 'OFFSHADOW16', clWhite);
    FToolImages.ResInstLoad(HInstance, rtBitmap, 'OFFSHADOW17', clWhite);
    FToolImages.ResInstLoad(HInstance, rtBitmap, 'OFFSHADOW18', clWhite);
    FToolImages.ResInstLoad(HInstance, rtBitmap, 'OFFSHADOW19', clWhite);
  end;
  Initialize;
end;

//------------------------------------------------------------------------------

destructor TAdvOfficeShadowSelector.Destroy;
begin
  //if not (csDesigning in ComponentState) and Assigned(FImages) then
  begin
    FToolImages.Free;
  end;

  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficeShadowSelector.Initialize;
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

procedure TAdvOfficeShadowSelector.Loaded;
begin
  inherited;

end;

//------------------------------------------------------------------------------

procedure TAdvOfficeShadowSelector.SetSelectorPanel;
begin
  inherited;
  if Assigned(FSelectorPanel) then
  begin
    FSelectorPanel.Images := FToolImages;
    FSelectorPanel.ButtonsPerRow := 4;
    FSelectorPanel.SetItemsPosition;
  end;
end;

//------------------------------------------------------------------------------


{ TAdvOfficeTableBorderSelector }

constructor TAdvOfficeTableBorderSelector.Create(AOwner: TComponent);
begin
  inherited;
  //if not (csDesigning in ComponentState) then
  begin
    FToolImages := TCustomImageList.Create(self);
    FToolImages.Width := 15;
    FToolImages.Height := 15;
    FToolImages.ResInstLoad(HInstance, rtBitmap, 'OFFTABLE0', clWhite);
    FToolImages.ResInstLoad(HInstance, rtBitmap, 'OFFTABLE1', clWhite);
    FToolImages.ResInstLoad(HInstance, rtBitmap, 'OFFTABLE2', clWhite);
    FToolImages.ResInstLoad(HInstance, rtBitmap, 'OFFTABLE3', clWhite);
    FToolImages.ResInstLoad(HInstance, rtBitmap, 'OFFTABLE4', clWhite);
    FToolImages.ResInstLoad(HInstance, rtBitmap, 'OFFTABLE5', clWhite);
    FToolImages.ResInstLoad(HInstance, rtBitmap, 'OFFTABLE6', clWhite);
    FToolImages.ResInstLoad(HInstance, rtBitmap, 'OFFTABLE7', clWhite);
    FToolImages.ResInstLoad(HInstance, rtBitmap, 'OFFTABLE8', clWhite);
    FToolImages.ResInstLoad(HInstance, rtBitmap, 'OFFTABLE9', clWhite);
    FToolImages.ResInstLoad(HInstance, rtBitmap, 'OFFTABLE10', clWhite);
    FToolImages.ResInstLoad(HInstance, rtBitmap, 'OFFTABLE11', clWhite);
    FToolImages.ResInstLoad(HInstance, rtBitmap, 'OFFTABLE12', clWhite);
  end;
  Initialize;
end;

//------------------------------------------------------------------------------

destructor TAdvOfficeTableBorderSelector.Destroy;
begin
  //if not (csDesigning in ComponentState) and Assigned(FImages) then
  begin
    FToolImages.Free;
  end;

  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficeTableBorderSelector.Initialize;
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

procedure TAdvOfficeTableBorderSelector.SetSelectorPanel;
begin
  inherited;
  if Assigned(FSelectorPanel) then
  begin
    FSelectorPanel.Images := FToolImages;
    FSelectorPanel.ButtonsPerRow := 7;
    FSelectorPanel.SetItemsPosition;
  end;
end;

//------------------------------------------------------------------------------


{ TAdvOfficeGradientDirectionSelector }

constructor TAdvOfficeGradientDirectionSelector.Create(AOwner: TComponent);
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
  FShowSelectedGradient := true;
  Initialize;
end;

//------------------------------------------------------------------------------

destructor TAdvOfficeGradientDirectionSelector.Destroy;
begin
 { if not (csDesigning in ComponentState) and Assigned(FImages) then
  begin
    FImages.Free;
  end;
 }
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficeGradientDirectionSelector.GetToolImage(bmp: TBitmap);
var
  R: TRect;

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
  if ShowSelectedGradient and (SelectedIndex >= 0) and Assigned(bmp) then
  begin
    R := GetBtnRect;
    bmp.Width := Min(R.Right, Self.Width-3);
    bmp.Height := Min(R.Bottom, Self.Height-2);

    bmp.Canvas.Brush.Color := clFuchsia;
    bmp.Canvas.FillRect(rect(0, 0, bmp.Width, bmp.Height));

    R.Right := bmp.Width;
    R.Bottom := bmp.Height;
    DrawGradientGlyph(bmp.Canvas,StartColor, EndColor, SelectedIndex, R);
    bmp.Transparent := True;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficeGradientDirectionSelector.DrawGlyphAndCaption(
  Pic: TGDIPPicture; R: TRect);
var
  CapR: TRect;
  clr: TColor;
  bmp: TBitmap;

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
    if not Pic.Empty then
    begin
      bmp := TBitmap.Create;
      try
        CopyPicture(Picture, bmp);
        if ShowSelectedGradient then
        begin
          CapR.Left := DrawGlyph(bmp, Rect(R.Left, R.Top, R.Right, R.Bottom - 5));
          CapR.Bottom := CapR.Bottom - 5;
          DrawGradientGlyph(Canvas,StartColor, EndColor, SelectedIndex, R);
        end
        else
        begin
          CapR.Left := DrawGlyph(bmp, Rect(R.Left, R.Top, R.Right, R.Bottom));
        end;
      finally
        bmp.Free;
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
    SetBkMode(Canvas.Handle, Windows.TRANSPARENT);
    if Caption <> '' then
      DrawText(Canvas.Handle, PChar(Caption), -1, CapR, DT_SINGLELINE or DT_CENTER or DT_VCENTER);
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

procedure TAdvOfficeGradientDirectionSelector.Initialize;
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

procedure TAdvOfficeGradientDirectionSelector.SelectorPanelOnDrawItem(Sender: TObject;
  Index: integer; R: TRect);
var
  R2: TRect;
begin
  if not Assigned(FSelectorPanel) then
    Exit;
    
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

procedure TAdvOfficeGradientDirectionSelector.SetEndColor(const Value: TColor);
begin
  FEndColor := Value;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficeGradientDirectionSelector.SetSelectorPanel;
begin
  inherited;

  if Assigned(FSelectorPanel) then
  begin
    FSelectorPanel.OnDrawItem := SelectorPanelOnDrawItem;
    FSelectorPanel.MinButtonWidth := 100;
    FSelectorPanel.MinButtonHeight := 20;
    FSelectorPanel.SetItemsPosition;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficeGradientDirectionSelector.SetShowSelectedGradient(
  const Value: boolean);
begin
  FShowSelectedGradient := Value;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficeGradientDirectionSelector.SetStartColor(const Value: TColor);
begin
  FStartColor := Value;
  Invalidate;
end;

//------------------------------------------------------------------------------


{ TAdvOfficePenWidthSelector }

constructor TAdvOfficePenWidthSelector.Create(AOwner: TComponent);
begin
  inherited;
  //if not (csDesigning in ComponentState) then
  begin
    FToolImages := TCustomImageList.Create(self);
    FToolImages.Width := 97;
    FToolImages.Height := 10;
    FToolImages.ResInstLoad(HInstance, rtBitmap, 'OFFPENWIDTH0', clWhite);
    FToolImages.ResInstLoad(HInstance, rtBitmap, 'OFFPENWIDTH1', clWhite);
    FToolImages.ResInstLoad(HInstance, rtBitmap, 'OFFPENWIDTH2', clWhite);
    FToolImages.ResInstLoad(HInstance, rtBitmap, 'OFFPENWIDTH3', clWhite);
    FToolImages.ResInstLoad(HInstance, rtBitmap, 'OFFPENWIDTH4', clWhite);
    FToolImages.ResInstLoad(HInstance, rtBitmap, 'OFFPENWIDTH5', clWhite);
    FToolImages.ResInstLoad(HInstance, rtBitmap, 'OFFPENWIDTH6', clWhite);
    FToolImages.ResInstLoad(HInstance, rtBitmap, 'OFFPENWIDTH7', clWhite);
    FToolImages.ResInstLoad(HInstance, rtBitmap, 'OFFPENWIDTH8', clWhite);
    FToolImages.ResInstLoad(HInstance, rtBitmap, 'OFFPENWIDTH9', clWhite);
    FToolImages.ResInstLoad(HInstance, rtBitmap, 'OFFPENWIDTH10', clWhite);
    FToolImages.ResInstLoad(HInstance, rtBitmap, 'OFFPENWIDTH11', clWhite);
    FToolImages.ResInstLoad(HInstance, rtBitmap, 'OFFPENWIDTH12', clWhite);
  end;
  Initialize;
end;

//------------------------------------------------------------------------------

destructor TAdvOfficePenWidthSelector.Destroy;
begin
  //if not (csDesigning in ComponentState) and Assigned(FImages) then
  begin
    FToolImages.Free;
  end;

  inherited;
end;

function TAdvOfficePenWidthSelector.GetSelectedPenWidth: Integer;
begin
  Result := 1;
  case SelectedIndex of
  0: Result := 1;
  1: Result := 1;
  2: Result := 2;
  3: Result := 3;
  4: Result := 4;
  5: Result := 6;
  6: Result := 8;
  7: Result := 12;
  8: Result := 20;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePenWidthSelector.Initialize;
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

procedure TAdvOfficePenWidthSelector.SetSelectedPenWidth(const Value: Integer);
begin
  case Value of
  0,1: SelectedIndex := 0;
  2: SelectedIndex := 2;
  3: SelectedIndex := 3;
  4: SelectedIndex := 4;
  6: SelectedIndex := 5;
  8: SelectedIndex := 6;
  12: SelectedIndex := 7;
  20: SelectedIndex := 8;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePenWidthSelector.SetSelectorPanel;
begin
  inherited;
  if Assigned(FSelectorPanel) then
  begin
    FSelectorPanel.Images := FToolImages;
    FSelectorPanel.SetItemsPosition;
  end;
end;

//------------------------------------------------------------------------------

{ TAdvCustomOfficeColorSelector }

constructor TAdvCustomOfficeColorSelector.Create(AOwner: TComponent);
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

destructor TAdvCustomOfficeColorSelector.Destroy;
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

procedure TAdvCustomOfficeColorSelector.DoSelect(Index: integer;
  Item: TAdvSelectorItem);
begin
  inherited;
  if Assigned(FOnSelectColor) then
    FOnSelectColor(self, SelectedColor);
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

procedure TAdvCustomOfficeColorSelector.Initialize;
begin
end;

procedure TAdvOfficeColorSelector.Initialize;
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
    Hint := 'More Colors';
    //Value:= ColorToString(clNone);
    BackGroundColor := clMoreColors;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeColorSelector.DropDownWindowClose(Sender: TObject; var Action: TCloseAction);
begin
  inherited;
  FColorCubePanel := nil;
  FSpectrumPanel := nil;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeColorSelector.SetSelectorPanel;
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

    if not Assigned(FSelectorPanel) then
      Exit;

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

    if not Assigned(FColorCubePanel) and Assigned(FDropDownWindow) then
    begin
      FColorCubePanel := TAdvColorCubePanel.Create(Self);
      FColorCubePanel.Parent := FDropDownWindow;
      FColorCubePanel.WindowBorderColor := FBorderDropDownColor;
      FColorCubePanel.OnShouldHide := OnColorCubeShouldHide;
      FColorCubePanel.OnSelect := CubePanelOnSelect;
      FColorCubePanel.OnFloating := OnColorCubeFloating;
    end;

    if FColorCubePanel.Parent <> FDropDownWindow then
      FColorCubePanel.Parent := FDropDownWindow;
      
    FColorCubePanel.ShowRGBHint := FShowRGBHint;
    FDropDownWindow.SelectorPanel := FColorCubePanel;

    FColorCubePanel.SelectedColor := FSelectedColor;

    FColorCubePanel.Initialize;
    FColorCubePanel.Color := ColorDropDown;
    FColorCubePanel.ColorTo := ColorDropDownTo;
    FColorCubePanel.ColorFloating := ColorDropDownFloating;
    FColorCubePanel.ColorFloatingTo := ColorDropDownFloatingTo;
    FColorCubePanel.DragGripAppearance.Assign(DragGripAppearance);
    FColorCubePanel.GripPosition := Self.DragGripPosition;
    FColorCubePanel.CaptionAppearance.Assign(Self.CaptionAppearance);
    FColorCubePanel.AllowFloating := AllowFloating;
    FColorCubePanel.Caption := Self.Caption;

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
      FSpectrumPanel := TAdvColorSpectrumPanel.Create(Self);
      FSpectrumPanel.Parent := FDropDownWindow;
      FSpectrumPanel.WindowBorderColor := FBorderDropDownColor;
      FSpectrumPanel.OnShouldHide := OnColorSpectrumShouldHide;
      FSpectrumPanel.OnSelect := SpectrumPanelOnSelect;
      FSpectrumPanel.OnFloating := OnColorSpectrumFloating;
    end;

    if FSpectrumPanel.Parent <> FDropDownWindow then
      FSpectrumPanel.Parent := FDropDownWindow;
    FDropDownWindow.SelectorPanel := FSpectrumPanel;

    FSpectrumPanel.SelectedColor := FSelectedColor;

    FSpectrumPanel.Initialize;
    FSpectrumPanel.Color := ColorDropDown;
    FSpectrumPanel.ColorTo := ColorDropDownTo;
    FSpectrumPanel.ColorFloating := ColorDropDownFloating;
    FSpectrumPanel.ColorFloatingTo := ColorDropDownFloatingTo;
    FSpectrumPanel.DragGripAppearance.Assign(DragGripAppearance);
    FSpectrumPanel.GripPosition := Self.DragGripPosition;
    FSpectrumPanel.CaptionAppearance.Assign(Self.CaptionAppearance);
    FSpectrumPanel.AllowFloating := AllowFloating;
    FSpectrumPanel.Caption := Self.Caption;

    FSpectrumPanel.SetItemsPosition;

    FSpectrumPanel.Left := 0;
    FSpectrumPanel.Top := 0;

    if not FSpectrumPanel.Visible then
      FSpectrumPanel.Visible:= true;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeColorSelector.SetColorSelectionStyle(
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

procedure TAdvCustomOfficeColorSelector.Loaded;
begin
  inherited;

end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeColorSelector.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FColorDialog) then
    FColorDialog := nil;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeColorSelector.GetToolImage(bmp: TBitmap);
var
  bmp2: TBitmap;
  R: TRect;
begin
  if ShowSelectedColor and (SelectedColor <> clNone{SelectedIndex >= 0}) and Assigned(bmp) then
  begin
    if (Assigned(Picture) and not Picture.Empty) then
    begin
      Picture.GetImageSizes;
      bmp.Width := Min(Picture.Width, Self.Width-3);
      bmp.Height := Min(Picture.Height + 4, Self.Height-2);

      if SelectedColor <> clFuchsia then
        bmp.Canvas.Brush.Color := clFuchsia;

      bmp.Canvas.FillRect(rect(0, 0, bmp.Width, bmp.Height));
      if ((bmp.Width <> Picture.Width) or (bmp.Height <> Picture.Height+4)) then
      begin
        bmp.Canvas.StretchDraw(Rect(0, 0, bmp.Width, bmp.Height-4), Picture);
      end
      else
        bmp.Canvas.Draw(0, 0, Picture);

      bmp.Canvas.Brush.Color := SelectedColor;
      bmp.Canvas.Pen.Color := SelectedColor;
      bmp.Canvas.Rectangle(1, bmp.Height - 4, bmp.Width-1, bmp.Height);

      bmp.Canvas.Brush.Color := clFuchsia;
    end
    else if (Assigned(Images) and (ImageIndex >= 0)) then
    begin
      bmp.Width := Min(Images.width, Self.Width - 3);
      bmp.Height := Min(Images.Height, Self.Height - 2);

      bmp.Canvas.Brush.Color := MakeLong(HiWord(SelectedColor),LoWord(SelectedColor)) + 1;  //
      bmp.Transparent := true;
      bmp.TransparentMode := tmFixed;
      bmp.TransparentColor := bmp.Canvas.Brush.Color;

      bmp.Canvas.FillRect(rect(0, 0, bmp.Width, bmp.Height));

      Images.DrawingStyle := dsTransparent;
      if ((bmp.Width <> Images.Width) or (bmp.Height <> Images.Height)) then
      begin
        bmp2 := TBitmap.Create;
        bmp2.Width := Images.width;
        bmp2.Height := Images.Height;
        Images.Draw(bmp2.Canvas, 0, 0, ImageIndex);
        bmp.Canvas.StretchDraw(Rect(0, 0, bmp.Width, bmp.Height-4), bmp2);
        bmp2.Free;
      end
      else
        Images.Draw(bmp.Canvas, 0, 0, ImageIndex);

      bmp.Canvas.Brush.Color := SelectedColor;
      bmp.Canvas.Pen.Color := SelectedColor;
      bmp.Canvas.FillRect(rect(0, bmp.Height - 4 , bmp.Width, bmp.Height));  // we draw the little selectedcolor rectangle at the bottom, 4 pixels high

      //bmp.Canvas.Rectangle(0, bmp.Height , bmp.Width, bmp.Height);
    end
    else
    begin
      R := GetBtnRect;
      bmp.Width := R.Right-4;
      bmp.Height := R.Bottom - 4;

      if SelectedColor = clFuchsia then
        bmp.Canvas.Brush.Color := clYellow
      else
        bmp.Canvas.Brush.Color := clFuchsia;

      bmp.Canvas.FillRect(rect(0, 0, bmp.Width, bmp.Height));

      bmp.Canvas.Brush.Color := SelectedColor;
      bmp.Canvas.Pen.Color := SelectedColor;
      bmp.Canvas.Rectangle(2, 2, bmp.Width - 2, bmp.Height - 2);

      OverlappedText := ShowCaption and (Caption <> '');
    end;
    bmp.Transparent := True;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeColorSelector.DrawGlyphAndCaption(Pic: TGDIPPicture; R: TRect);
var
  CapR: TRect;
  clr: TColor;
begin
  CapR := R;
  clr := Canvas.Pen.Color;
  if Style = ssButton then
  begin
    (*if not aGlyph.Empty then
    begin
      if ShowSelectedColor then
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
      if ShowSelectedColor then
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
    SetBkMode(Canvas.Handle, Windows.TRANSPARENT);
    if Caption <> '' then
      DrawText(Canvas.Handle, PChar(Caption), -1, CapR, DT_SINGLELINE or DT_CENTER or DT_VCENTER);
 *)
  end
  else if Style = ssCombo then
  begin
    if {(SelectedIndex >= 0) and }(Caption = '') then
    begin
      Canvas.Pen.Color := clBlack;
      Canvas.Brush.Color := SelectedColor;
      Canvas.Rectangle(R.Left + 4, R.Top + 3, R.Right - 4, R.Bottom - 3);
    end;
  end;
  Canvas.Pen.Color := clr;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeColorSelector.SelectorPanelOnDrawItem(Sender: TObject; Index: integer;
  R: TRect);
begin
  if not Assigned(FSelectorPanel) then
    Exit;
    
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
    if (FSelectorPanel.Items.Items[Index].CaptionAlignment = taCenter) and (FSelectorPanel.Items.Items[Index].Caption <> '') then
      DrawText(FSelectorPanel.Canvas.Handle, PChar(FSelectorPanel.Items.Items[Index].Caption), -1, R, DT_SINGLELINE or DT_VCENTER or DT_CENTER)
    else if (FSelectorPanel.Items.Items[Index].CaptionAlignment = taRightJustify) and (FSelectorPanel.Items.Items[Index].Caption <> '') then
      DrawText(FSelectorPanel.Canvas.Handle, PChar(FSelectorPanel.Items.Items[Index].Caption), -1, R, DT_SINGLELINE or DT_VCENTER);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeColorSelector.SetShowSelectedColor(const Value: Boolean);
begin
  FShowSelectedColor := Value;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeColorSelector.CubePanelOnSelect(Sender: TObject);
begin
  FSelectedColor := FColorCubePanel.SelectedColor;
  DoSelect(-1, nil);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeColorSelector.SpectrumPanelOnSelect(Sender: TObject);
begin
  FSelectedColor := FSpectrumPanel.SelectedColor;
  DoSelect(-1, nil);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeColorSelector.SetSelectedColor(const Value: TColor);
begin
  FSelectedColor := Value;
  Invalidate;
end;

//------------------------------------------------------------------------------

function TAdvCustomOfficeColorSelector.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TAdvColorSelectorActionLink;
end;

function TAdvCustomOfficeColorSelector.GetSelectedColor: TColor;
begin
  Result := FSelectedColor;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeColorSelector.OnToolSelect;
var
  dlg: TColorDialog;
begin
  if (SelectedIndex >= 0) then
  begin
    if (Tools.Items[SelectedIndex].BackGroundColor <> clNone) and
      (Tools.Items[SelectedIndex].BackGroundColor <> clMoreColors) then
    begin
      FSelectedColor := Tools.Items[SelectedIndex].BackGroundColor;
    end;

    if (Tools.Items[SelectedIndex].BackGroundColor = clMoreColors) and
      not Assigned(OnSelect) then
      begin
        FDropDownWindow.HideOnDeActivate := false;

        if Assigned(FColorDialog) then
          dlg := FColorDialog
        else
          dlg := TColorDialog.Create(self);

        dlg.CustomColors.Text := FCustomColors;
        if dlg.Execute then
        begin
          FCustomColors := dlg.CustomColors.Text;

          SelectedColor := dlg.Color;
          if Assigned(FOnSelectColor) then
            FOnSelectColor(Self, SelectedColor);
        end;

        if not Assigned(FColorDialog) then
          dlg.Free;
        FDropDownWindow.HideOnDeActivate := true;
        Exit;
      end;
  end;

  inherited;

  if (SelectedIndex >= 0) then
  begin
    if Assigned(Action) then
    begin
      Action.Execute;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeColorSelector.OnColorCubeFloating(
  Sender: TObject);
begin
  if Assigned(FColorCubePanel) then
  begin
    if Assigned(FDropDownWindow) then
      FDropDownWindow.Height := FDropDownWindow.Height + CAPTION_HEIGHT;
    FColorCubePanel.SetItemsPosition;
    if Assigned(FDropDownWindow) then
      FDropDownWindow.SetWindowSize;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeColorSelector.OnColorSpectrumFloating(
  Sender: TObject);
begin
  if Assigned(FSpectrumPanel) then
  begin
    if Assigned(FDropDownWindow) then
      FDropDownWindow.Height := FDropDownWindow.Height + CAPTION_HEIGHT;
    FSpectrumPanel.SetItemsPosition;
    if Assigned(FDropDownWindow) then
      FDropDownWindow.SetWindowSize;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeColorSelector.OnColorSpectrumShouldHide(
  Sender: TObject);
begin
  if Assigned(FSpectrumPanel) and (FSpectrumPanel.FFloating) then
  begin
    if CloseOnSelect then
      HideDropDown;
  end
  else
    HideDropDown;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeColorSelector.OnColorCubeShouldHide(
  Sender: TObject);
begin
  if Assigned(FColorCubePanel) and (FColorCubePanel.FFloating) then
  begin
    if CloseOnSelect then
      HideDropDown;
  end
  else
    HideDropDown;
end;

//------------------------------------------------------------------------------

{ TAdvOfficeTextColorSelector }

procedure TAdvOfficeTextColorSelector.Initialize;
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

{ TAdvTableSelectorPanel }

constructor TAdvTableSelectorPanel.Create(AOwner: TComponent);
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
  FSelectionColor := clHighlight;
  FLabelHeight := 20;
end;

//------------------------------------------------------------------------------

destructor TAdvTableSelectorPanel.Destroy;
begin

  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvTableSelectorPanel.MouseMove(Shift: TShiftState; X,
  Y: Integer);
var
  c, r, ax, ay: integer;
  OldSelCol, OldSelRow: integer;
  CellFound: boolean;
  R2: TRect;
  P: TPoint;
begin
  inherited;

  if not Assigned(DropDownWindow) then
    Exit;

  OldSelCol := FSelectedCols;
  OldSelRow := FSelectedRows;
  ax := FLeftMargin;
  CellFound := false;

  if DropDownWindow.ShowAbove then
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


  if DropDownWindow.ShowAbove then
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
      SystemParametersInfo(SPI_GETWORKAREA, 0, @R2, 0);

      P := Point(0, self.Height);
      P := ClientToScreen(P);

      if (P.X + Width + FCellSize + 2) < R2.Right then
      begin
        ColCount := ColCount + 1;
        SetPanelSize;
        DropDownWindow.SetWindowSize;
      end;
    end;

    if DropDownWindow.ShowAbove then
    begin
      if (Y < FLabelHeight + 2) then
      begin
        SystemParametersInfo(SPI_GETWORKAREA, 0, @R2, 0);

        P := Point(0, 0);
        P := ClientToScreen(P);

        if (P.Y - FCellSize - 2) > R2.Top then
        begin
          RowCount := RowCount + 1;
          SetPanelSize;
          DropDownWindow.SetWindowSize;
          DropDownWindow.Top := DropDownWindow.Top - FCellSize - FCellSpace;
        end;
      end;
    end
    else // if not TSelectorDropDownWindow(FOwner).ShowAbove then
    begin
      if (Y > Height - FLabelHeight - 2) then
      begin
        SystemParametersInfo(SPI_GETWORKAREA, 0, @R2, 0);

        P := Point(0, 0);
        P := ClientToScreen(P);

        if (P.Y + Height + FCellSize + 2) < R2.Bottom then
        begin
          RowCount := RowCount + 1;
          SetPanelSize;
          DropDownWindow.SetWindowSize;
        end;
      end;
    end;
  end;

end;

//------------------------------------------------------------------------------

procedure TAdvTableSelectorPanel.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  if Assigned(FOnSelect) and (FSelectedCols > 0) and (FSelectedRows > 0) then
    FOnSelect(self);

  if Assigned(FOnShouldHide) then
    FOnShouldHide(self);

end;

//------------------------------------------------------------------------------

procedure TAdvTableSelectorPanel.DrawLabel;
var
  s: string;
  R: TRect;
begin
  if not Assigned(DropDownWindow) then
    Exit;
    
  if (FSelectedCols > 0) or (FSelectedRows > 0) then
    s := inttostr(FSelectedCols) + ' x ' + inttostr(FSelectedRows) + ' ' + FTextTable
  else
    s := FTextCancel;

  Canvas.Brush.Style := bsclear;

  if DropDownWindow.ShowAbove then
    R := Rect(FLeftMargin, 1, Width - FLeftMargin, FLabelHeight)
  else
    R := Rect(FLeftMargin, Height - FLabelHeight, Width - FLeftMargin, Height - FTopMargin);

  DrawText(Canvas.Handle, PChar(s), -1, R, DT_SINGLELINE or DT_VCENTER or DT_CENTER);
end;

//------------------------------------------------------------------------------

procedure TAdvTableSelectorPanel.HandleKey(Key: word);
begin
  case Key of
  VK_RIGHT:
    begin
      FSelectedCols := FSelectedCols + 1;
      if FSelectedRows = 0 then
        FSelectedRows := 1;

      if FSelectedCols > ColCount then
        ColCount := FSelectedCols;
      SetPanelSize;
      DropDownWindow.SetWindowSize;
    end;

  VK_LEFT: if FSelectedCols > 0 then FSelectedCols := FSelectedCols - 1;
  VK_DOWN:
    begin
      FSelectedRows := FSelectedRows + 1;
      if FSelectedCols = 0 then
        FSelectedCols := 1;

      if FSelectedRows > RowCount then
        RowCount := FSelectedRows;
      SetPanelSize;
      DropDownWindow.SetWindowSize;
    end;
  VK_UP: if FSelectedRows > 0 then FSelectedRows := FSelectedRows - 1;

  VK_RETURN:
    begin
      if Assigned(FOnShouldHide) then
        FOnShouldHide(self);
      if Assigned(FOnSelect) and (FSelectedCols > 0) and (FSelectedRows > 0) then
        FOnSelect(self);
    end;
  end;

  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvTableSelectorPanel.DrawAllCells;
var
  c, r, x, y: integer;
begin
  if not Assigned(DropDownWindow) then
    Exit;

  x := FLeftMargin;
  if DropDownWindow.ShowAbove then
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

procedure TAdvTableSelectorPanel.Paint;
begin
  inherited;
  DrawAllCells;
  DrawLabel;
end;

//------------------------------------------------------------------------------

procedure TAdvTableSelectorPanel.SetCellSize(const Value: integer);
begin
  FCellSize := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvTableSelectorPanel.SetColCount(const Value: integer);
begin
  FColCount := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvTableSelectorPanel.SetItemsPosition;
begin
  SetPanelSize;
end;

//------------------------------------------------------------------------------

procedure TAdvTableSelectorPanel.SetPanelSize;
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

  if AllowFloating then
  begin
    if Floating then
      y := y + CAPTION_HEIGHT
    else // if ((GripPosition = gpBottom) or ((not Floating) and (GripPosition = gpTop))) then
      y := y + DRAGGRIP_HEIGHT;
  end;

  Height := y + FLabelHeight;
  Width := Max(x, Canvas.TextWidth(FTextCancel));
end;

//------------------------------------------------------------------------------

procedure TAdvTableSelectorPanel.SetRowCount(const Value: integer);
begin
  FRowCount := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvTableSelectorPanel.SetSelectedCols(const Value: integer);
begin
  if FSelectedCols <> Value then
  begin
    FSelectedCols := Value;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvTableSelectorPanel.SetSelectedRows(const Value: integer);
begin
  FSelectedRows := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvTableSelectorPanel.InvalidateSelection(OldSelectedCols,
  OldSelectedRows: integer);
var
  Rc, Rr: TRect;
  c, r, x, y: integer;
  RgnC, RgnR, RgnL, RgnT: HRGN;
begin
  if not Assigned(DropDownWindow) then
    Exit;
    
  Rc := Rect(0, 0, 0, 0);
  if OldSelectedCols <> FSelectedCols then
  begin
    x := FLeftMargin;
    if DropDownWindow.ShowAbove then
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
    if DropDownWindow.ShowAbove then
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

  if DropDownWindow.ShowAbove then
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

procedure TAdvTableSelectorPanel.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  FSelectedCols := -1;
  FSelectedRows := -1;
  invalidate;
end;

//------------------------------------------------------------------------------

{ TAdvCustomOfficeTableSelector }

constructor TAdvCustomOfficeTableSelector.Create(AOwner: TComponent);
begin
  inherited;
  FDefaultColCount := 5;
  FDefaultRowCount := 4;
  FSelectedColumns := 0;
  FSelectedRows := 0;
  FSelectionColor := clHighlight;
  FTextTable := 'Table';
  FTextCancel := 'Cancel';
end;

//------------------------------------------------------------------------------

destructor TAdvCustomOfficeTableSelector.Destroy;
begin
  if not (csDesigning in ComponentState) then
  begin
    //if Assigned(FTableSelectorPanel) then
    //  FTableSelectorPanel.Free;
  end;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTableSelector.Loaded;
begin
  inherited;

end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTableSelector.MouseMove(Shift: TShiftState; X,
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

procedure TAdvCustomOfficeTableSelector.SetDefaultColCount(const Value: integer);
begin
  if (Value > 0) and (Value <= 60) then
    FDefaultColCount := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTableSelector.SetDefaultRowCount(const Value: integer);
begin
  if (Value > 0) and (Value <= 60) then
    FDefaultRowCount := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTableSelector.DropDownWindowClose(Sender: TObject; var Action: TCloseAction);
begin
  inherited;
  FTableSelectorPanel := nil;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTableSelector.SetSelectionColor(const Value: TColor);
begin
  FSelectionColor := Value;
end;

procedure TAdvCustomOfficeTableSelector.SetSelectorPanel;
begin
  if not Assigned(FTableSelectorPanel) and Assigned(FDropDownWindow) then
  begin
    FTableSelectorPanel := TAdvTableSelectorPanel.Create(FDropDownWindow);
    FTableSelectorPanel.Parent := FDropDownWindow;
    FTableSelectorPanel.OnShouldHide := OnDropDownPanelShouldHide;
    FTableSelectorPanel.OnSelect := TableSelectorOnSelect;
  end;

  if FTableSelectorPanel.Parent <> FDropDownWindow then
    FTableSelectorPanel.Parent := FDropDownWindow;

  FDropDownWindow.SelectorPanel := FTableSelectorPanel;

  //FTableSelectorPanel.SelectionColor := FSelectedColor;
  FTableSelectorPanel.WindowBorderColor := FBorderDropDownColor;
  FTableSelectorPanel.ColCount := DefaultColCount;
  FTableSelectorPanel.RowCount := DefaultRowCount;
  FTableSelectorPanel.SelectedCols := 0;
  FTableSelectorPanel.SelectedRows := 0;
  FTableSelectorPanel.SelectionColor := FSelectionColor;
  FTableSelectorPanel.TextTable := FTextTable;
  FTableSelectorPanel.TextCancel := FTextCancel;

  FTableSelectorPanel.SetItemsPosition;

  FTableSelectorPanel.Left := 0;
  FTableSelectorPanel.Top := 0;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTableSelector.TableSelectorOnSelect(Sender: TObject);
begin
  FSelectedColumns := FTableSelectorPanel.SelectedCols;
  FSelectedRows := FTableSelectorPanel.SelectedRows;

  if Assigned(FOnSelect) then
    FOnSelect(self);

  if Assigned(FOnSelectTableSize) then
    FOnSelectTableSize(Self, FSelectedColumns, FSelectedRows);
end;

//------------------------------------------------------------------------------

{ TAdvOfficeCharacterSelector }

constructor TAdvOfficeCharacterSelector.Create(AOwner: TComponent);
begin
  inherited;
  FCharacters := '';
  FSelectedChar := #0;
  ButtonsPerRow := 10;

  FCharFont := TFont.Create;
  FAutoLoad := false;
end;

//------------------------------------------------------------------------------

destructor TAdvOfficeCharacterSelector.Destroy;
begin
  FCharFont.Free;
  inherited;
end;

procedure TAdvOfficeCharacterSelector.DoSelect(Index: integer;
  Item: TAdvSelectorItem);
begin
  inherited;
  if Assigned(FOnSelectChar) then
    FOnSelectChar(Self, SelectedChar);
end;

//------------------------------------------------------------------------------

procedure TAdvOfficeCharacterSelector.AddItemsFromChars;
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

procedure TAdvOfficeCharacterSelector.LoadCharFromFont;
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

procedure TAdvOfficeCharacterSelector.Loaded;
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficeCharacterSelector.OnToolSelect;
begin
  if SelectedIndex >= 0 then
    FSelectedChar := Tools.Items[SelectedIndex].Caption[1];
    
  inherited;

  if Assigned(FOnSelect) then
    FOnSelect(self);
end;

//------------------------------------------------------------------------------

procedure TAdvOfficeCharacterSelector.SetAutoLoad(const Value: boolean);
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

procedure TAdvOfficeCharacterSelector.SetCharacters(const Value: string);
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

procedure TAdvOfficeCharacterSelector.SetCharFont(const Value: TFont);
begin
  FCharFont.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TAdvOfficeCharacterSelector.SetSelectedChar(const Value: char);
begin
  FSelectedChar := Value;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficeCharacterSelector.SetSelectorPanel;
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
  if Assigned(FSelectorPanel) then
  begin
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
end;

//------------------------------------------------------------------------------

procedure TAdvOfficeCharacterSelector.GetToolImage(bmp: TBitmap);
var
  ar: array[0..1] of char;
  DTSTYLE: dword;
  R: TRect;
begin
  if (not Assigned(Picture) or Picture.Empty) and (not Assigned(Images) or (ImageIndex < 0)) and (SelectedChar <> #0) and Assigned(bmp) and (Caption = '') then
  begin
    bmp.Width := Self.Width-3;
    bmp.Height := Self.Height-2;
    R := Rect(1, 1, bmp.Width-1, bmp.Height-1);

    DTSTYLE := DT_SINGLELINE or DT_VCENTER or DT_NOPREFIX;
    bmp.Canvas.Brush.Color := clFuchsia;
    bmp.Canvas.FillRect(rect(0, 0, bmp.Width, bmp.Height));

    bmp.Canvas.Font.Name := CharFont.Name;
    bmp.Canvas.Font.Size := CharFont.Size;
    bmp.Canvas.Brush.Style := bsClear;
    ar[0] := SelectedChar;
    ar[1] := #0;
    DrawText(bmp.Canvas.Handle, ar, -1, R, DTSTYLE or DT_CENTER);

    bmp.Transparent := True;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficeCharacterSelector.DrawGlyphAndCaption(Pic: TGDIPPicture;
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
    (*if not aGlyph.Empty then
    begin
      CapR.Left := DrawGlyph(aGlyph, R);
    end
    else
    begin
      if (Caption = '') and (SelectedChar <> #0) then
      begin
        Canvas.Font.Name := CharFont.Name;
        Canvas.Font.Size := CharFont.Size;
        Canvas.Brush.Style := bsClear;
        ar[0] := SelectedChar;
        ar[1] := #0;
        DrawText(Canvas.Handle, ar, -1, CapR, DTSTYLE or DT_CENTER);
      end;
    end;
    if Caption <> '' then
      DrawText(Canvas.Handle, PChar(Caption), -1, CapR, DTSTYLE);
*)
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
      DrawText(Canvas.Handle, ar, -1, CapR, DTSTYLE or DT_CENTER);
    end;
  end;
end;

//------------------------------------------------------------------------------

{ TSelectionAppearance }

procedure TSelectionAppearance.Assign(Source: TPersistent);
begin
  inherited;
  if (Source is TSelectionAppearance) then
  begin
    TextColor := TSelectionAppearance(Source).TextColor;
    TextColorHot := TSelectionAppearance(Source).TextColorHot;
    TextColorDown := TSelectionAppearance(Source).TextColorDown;
    TextColorChecked := TSelectionAppearance(Source).TextColorChecked;
    TextColorDisabled := TSelectionAppearance(Source).TextColorDisabled;
    Rounded := TSelectionAppearance(Source).Rounded;
  end;
end;

//------------------------------------------------------------------------------

constructor TSelectionAppearance.Create;
begin
  inherited;
  FTextColor := clBlack;
  FTextColorDisabled := clGray;
  FTextColorDown := clWhite;
  FTextColorHot := clWhite;
  FTextColorChecked := clBlack;
  FRounded := False;
end;

//------------------------------------------------------------------------------

procedure TSelectionAppearance.SetRounded(const Value: Boolean);
begin
  if (FRounded <> Value) then
  begin
    FRounded := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TSelectionAppearance.SetTextColor(const Value: TColor);
begin
  if (FTextColor <> Value) then
  begin
    FTextColor := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TSelectionAppearance.SetTextColorChecked(const Value: TColor);
begin
  if (FTextColorChecked <> Value) then
  begin
    FTextColorChecked := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TSelectionAppearance.SetTextColorDisabled(const Value: TColor);
begin
  if (FTextColorDisabled <> Value) then
  begin
    FTextColorDisabled := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TSelectionAppearance.SetTextColorDown(const Value: TColor);
begin
  if (FTextColorDown <> Value) then
  begin
    FTextColorDown := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TSelectionAppearance.SetTextColorHot(const Value: TColor);
begin
  if (FTextColorHot <> Value) then
  begin
    FTextColorHot := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

{ TVistaBackground }

constructor TVistaBackground.Create;
begin
  inherited;
  FSteps := 64;
  FColor := clWhite;
  FColorTo := clWhite;
  FColorMirror := clSilver;
  FColorMirrorTo := clWhite;
  FBorderColor := clGray;
  FGradient := ggVertical;
  FGradientMirror := ggVertical;
end;

//------------------------------------------------------------------------------

procedure TVistaBackground.Assign(Source: TPersistent);
begin
  if (Source is TVistaBackground) then
  begin
    FSteps := (Source as TVistaBackground).Steps;
    FColor := (Source as TVistaBackground).Color;
    FColorTo := (Source as TVistaBackground).ColorTo;
    FColorMirror := (Source as TVistaBackground).ColorMirror;
    FColorMirrorTo := (Source as TVistaBackground).ColorMirrorTo;
    FBorderColor := (Source as TVistaBackground).BorderColor;
    Gradient := (Source as TVistaBackground).Gradient;
    GradientMirror := (Source as TVistaBackground).GradientMirror;
  end
  else
    inherited Assign(Source);
end;

//------------------------------------------------------------------------------

procedure TVistaBackground.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

//------------------------------------------------------------------------------

procedure TVistaBackground.SetColor(const Value: TColor);
begin
  if (FColor <> Value) then
  begin
    FColor := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TVistaBackground.SetColorTo(const Value: TColor);
begin
  if (FColorTo  <> Value) then
  begin
    FColorTo := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TVistaBackground.SetBorderColor(const Value: TColor);
begin
  if (FBorderColor <> Value) then
  begin
    FBorderColor := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TVistaBackground.SetColorMirror(const Value: TColor);
begin
  if (FColorMirror <> Value) then
  begin
    FColorMirror := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TVistaBackground.SetColorMirrorTo(const Value: TColor);
begin
  if (FColorMirrorTo <> Value) then
  begin
    FColorMirrorTo := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TVistaBackground.SetGradient(const Value: TGDIPGradient);
begin
  if (FGradient <> Value) then
  begin
    FGradient := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TVistaBackground.SetGradientMirror(const Value: TGDIPGradient);
begin
  if(FGradientMirror <> Value) then
  begin
    FGradientMirror := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TVistaBackground.SetSteps(const Value: Integer);
begin
  if (FSteps <> Value) then
  begin
    FSteps := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

{ TVistaBackgroundHot }

constructor TVistaBackgroundHot.Create;
begin
  inherited;
  FBorderColorHot := clBlue;
  FColorHot := $F5F0E1;
  FColorHotTo := $F9D2B2;
  FColorMirrorHot := $F5C8AD;
  FColorMirrorHotTo := $FFF8F4;
  FGradientHot := ggRadial;
  FGradientMirrorHot := ggRadial;
end;

//------------------------------------------------------------------------------

procedure TVistaBackgroundHot.Assign(Source: TPersistent);
begin
  inherited;
  if (Source is TVistaBackgroundHot) then
  begin
    FBorderColorHot := TVistaBackgroundHot(Source).BorderColorHot;
    FColorHot := TVistaBackgroundHot(Source).ColorHot;
    FColorHotTo := TVistaBackgroundHot(Source).ColorHotTo;
    FColorMirrorHot := TVistaBackgroundHot(Source).ColorMirrorHot;
    FColorMirrorHotTo := TVistaBackgroundHot(Source).ColorMirrorHotTo;
    FGradientHot := TVistaBackgroundHot(Source).GradientHot;
    FGradientMirrorHot := TVistaBackgroundHot(Source).GradientMirrorHot;
  end;
end;

//------------------------------------------------------------------------------

{ TGradientBackground }

procedure TGradientBackground.Assign(Source: TPersistent);
begin
  if (Source is TGradientBackground) then
  begin
    FColor := (Source as TGradientBackground).Color;
    FColorTo := (Source as TGradientBackground).ColorTo;
    FDirection := (Source as TGradientBackground).Direction;
    FSteps := (Source as TGradientBackground).Steps;
    FBorderColor := (Source as TGradientBackground).BorderColor;
  end;
end;

//------------------------------------------------------------------------------

procedure TGradientBackground.Changed;
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

//------------------------------------------------------------------------------

constructor TGradientBackground.Create;
begin
  inherited;
  FColor := clWhite;
  FColorTo := clBtnFace;
  FSteps := 64;
  FDirection := gdHorizontal;
  FBorderColor := clNone;
end;

//------------------------------------------------------------------------------

procedure TGradientBackground.SetBorderColor(const Value: TColor);
begin
  FBorderColor := Value;
  Changed;
end;

//------------------------------------------------------------------------------

procedure TGradientBackground.SetColor(const Value: TColor);
begin
  FColor := Value;
  Changed;
end;

//------------------------------------------------------------------------------

procedure TGradientBackground.SetColorTo(const Value: TColor);
begin
  FColorTo := Value;
  Changed;
end;

//------------------------------------------------------------------------------

procedure TGradientBackground.SetDirection(
  const Value: TGradientDirection);
begin
  FDirection := Value;
  Changed;
end;

//------------------------------------------------------------------------------

procedure TGradientBackground.SetSteps(const Value: Integer);
begin
  FSteps := Value;
  Changed;
end;

//------------------------------------------------------------------------------

{ TGradientCaption }

constructor TGradientCaption.Create;
begin
  inherited;
  FColor := RGB(42, 102, 201);
  FColorTo := clNone;
  FTextColor := clWhite;
  FButtonAppearance := TGlowButtonAppearance.Create;
end;

//------------------------------------------------------------------------------

destructor TGradientCaption.destroy;
begin
  FButtonAppearance.Free;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TGradientCaption.Assign(Source: TPersistent);
begin
  inherited;
  if (Source is TGradientCaption) then
  begin
    TextColor := TGradientCaption(Source).TextColor;
    TextColorHot := TGradientCaption(Source).TextColorHot;
    TextColorDown := TGradientCaption(Source).TextColorDown;
    ButtonAppearance.Assign(TGradientCaption(Source).ButtonAppearance);
  end;
end;

//------------------------------------------------------------------------------

procedure TGradientCaption.SetButtonAppearance(
  const Value: TGlowButtonAppearance);
begin
  FButtonAppearance.Assign(Value);
end;

//------------------------------------------------------------------------------

{ TAdvOfficeToolSelector }

constructor TAdvOfficeToolSelector.Create(AOwner: TComponent);
begin
  inherited;
  FAutoUpdateCaption := true;
end;

procedure TAdvOfficeToolSelector.DoSelect(Index: integer;
  Item: TAdvSelectorItem);
begin
  inherited;
  if ShowCaption and (Item.Caption <> '') and AutoUpdateCaption then
    Caption := Item.Caption;
end;

function TAdvOfficeToolSelector.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TAdvToolSelectorActionLink;
end;

procedure TAdvOfficeToolSelector.GetToolImage(bmp: TBitmap);
begin
  if Picture.Empty and not Assigned(Images) and Assigned(OnDrawTool) then
  begin
    bmp.Width := Min(FToolWidth, Self.Width - 3);
    bmp.Height := Min(FToolHeight, Self.Height - 2);

    bmp.Canvas.Brush.Color := clFuchsia;
    bmp.Canvas.FillRect(Rect(0, 0, bmp.Width, bmp.Height));

    OnDrawTool(Self, bmp.Canvas, SelectedIndex, Rect(0, 0, bmp.Width, bmp.Height));

    bmp.TransparentMode := tmAuto;
    bmp.Transparent := true;
  end
  else
    inherited;
end;

procedure TAdvOfficeToolSelector.SetSelectorPanel;
begin
  inherited;
  if Assigned(FSelectorPanel) then
  begin
    FSelectorPanel.MinButtonWidth := Max(MINBUTTONSIZE, FToolWidth);
    FSelectorPanel.MinButtonHeight := Max(MINBUTTONSIZE, FToolHeight);
    FSelectorPanel.SetItemsPosition;
  end;
end;

//------------------------------------------------------------------------------

{ TAdvCustomOfficeScrollSelector }

constructor TAdvCustomOfficeScrollSelector.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csAcceptsControls];
  FScrollSelectorPanel := TAdvScrollSelectorPanel.Create(Self);
  FScrollSelectorPanel.Parent := Self;
  FScrollSelectorPanel.Width := 50;
  FScrollSelectorPanel.Height := 40;
  FScrollSelectorPanel.AdvOfficeScrollSelector := Self;
  FScrollSelectorPanel.ShowCaptionItem := False;
  FScrollSelectorPanel.ShowFullWidthItem := False;
  FScrollSelectorPanel.OnScroll := OnScrollSelectorPanelScroll;
  ControlStyle := ControlStyle - [csAcceptsControls];

  FTools := TAdvScrollSelectorItems.Create(Self);
  FTools.OnChange := OnItemsChanged;

  FDropDownHeight := 250;
  FColorDropDownTo := clNone;
  FColorDropDown := clWhite;
  Color := RGB(236, 243, 251);
  FColorTo := clNone;
  FFocusColor := RGB(236, 243, 251);
  FFocusColorTo := clNone;
  FBorderDropDownColor := clGray;
  FBorderColor := RGB(185, 208, 237);
  FCaptionAppearance := TSimpleGradientCaption.Create;
  FCaptionItemAppearance := TSimpleGradientCaption.Create;
  FSelectionAppearance := TSelectionAppearance.Create;
  FSelectionAppearance.OnChange := OnAppearanceChanged;
  //FSelectedIndex := -1;
  //FDupSelectedIndex := -1;
  FDropDownButton := True;
  FSelectorScroller := TSelectorScroller.Create;
  FSelectorScroller.Visible := True;

  FScrollerAppearance := TSelectionAppearance.Create;
  FScrollerAppearance.OnChange := OnAppearanceChanged;

  FResizerAppearance := TGradientBackground.Create;
  FDropDownSizeable := False;
  FDropDownCaption := '';
  FButtonSize := TSelectorButtonSize.Create;
  FButtonSize.OnChange := ButtonSizeChanged;
  FIconBarWidth := 24;
  FIconBarAppearance := TGradientBackground.Create;

  FIntegralRows := True;
  FOldDropDownHeight := 0;
  FOldDropDownWidth := 0;
  if not (csDesigning in ComponentState) and Assigned(AOwner) and not(csLoading in AOwner.ComponentState) then
  begin
    FScrollSelectorPanel.SelectionAppearance.Assign(SelectionAppearance);
    FScrollSelectorPanel.OnSelect := OnDropDownPanelSelect;

    SetScrollSelectorPanelPos;
    UpdateScrollSelectorPanel;
  end;
  DoubleBuffered := true;
end;

//------------------------------------------------------------------------------

destructor TAdvCustomOfficeScrollSelector.Destroy;
begin
  FTools.Free;
  FCaptionAppearance.Free;
  FCaptionItemAppearance.Free;
  FSelectionAppearance.Free;
  FSelectorScroller.Free;
  FScrollerAppearance.Free;
  FResizerAppearance.Free;
  FButtonSize.Free;
  FIconBarAppearance.Free;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeScrollSelector.CMMouseEnter(
  var Message: TMessage);
begin
  inherited;

  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeScrollSelector.CMMouseLeave(
  var Message: TMessage);
begin
  inherited;
  if FUpScrollerDown or FUpScrollerHot then
  begin
    FUpScrollerDown := False;
    FUpScrollerHot := False;
    DrawUpScroller;
  end;

  if FDownScrollerDown or FDownScrollerHot then
  begin
    FDownScrollerDown := False;
    FDownScrollerHot := False;
    DrawDownScroller;
  end;

  if FDropDownButtonDown or FDropDownButtonHot then
  begin
    FDropDownButtonDown := False;
    FDropDownButtonHot := False;
    DrawDropDownButton;
  end;

  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeScrollSelector.KeyDown(var Key: Word;
  Shift: TShiftState);
begin
  inherited;

end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeScrollSelector.WMKeyDown(
  var Message: TWMKeyDown);
var
  i: Integer;
begin
  inherited;
  case Message.CharCode of
    VK_LEFT:
    begin
      if (FScrollSelectorPanel.HotItemIndex >= 0) then
      begin
        FScrollSelectorPanel.HotItemIndex := FScrollSelectorPanel.PreviousItemIndex(FScrollSelectorPanel.HotItemIndex);
      end
      else if (SelectedIndex >= 0) then
      begin
        FScrollSelectorPanel.HotItemIndex := FScrollSelectorPanel.PreviousItemIndex(SelectedIndex);
      end
      else
      begin
        FScrollSelectorPanel.HotItemIndex := FScrollSelectorPanel.FirstItemIndex;
      end;

      FScrollSelectorPanel.ViewButton(FScrollSelectorPanel.HotItemIndex);
    end;
    VK_RIGHT:
    begin
      if (FScrollSelectorPanel.HotItemIndex >= 0) then
      begin
        FScrollSelectorPanel.HotItemIndex := FScrollSelectorPanel.NextItemIndex(FScrollSelectorPanel.HotItemIndex);
      end
      else if (SelectedIndex >= 0) then
      begin
        FScrollSelectorPanel.HotItemIndex := FScrollSelectorPanel.NextItemIndex(SelectedIndex);
      end
      else
      begin
        FScrollSelectorPanel.HotItemIndex := FScrollSelectorPanel.FirstItemIndex;
      end;

      FScrollSelectorPanel.ViewButton(FScrollSelectorPanel.HotItemIndex);
    end;
    VK_UP:
    begin
      if (FScrollSelectorPanel.HotItemIndex >= 0) then
      begin
        FScrollSelectorPanel.HotItemIndex := FScrollSelectorPanel.UpItemIndex(FScrollSelectorPanel.HotItemIndex);
      end
      else if (SelectedIndex >= 0) then
      begin
        FScrollSelectorPanel.HotItemIndex := FScrollSelectorPanel.UpItemIndex(SelectedIndex);
      end
      else
      begin
        FScrollSelectorPanel.HotItemIndex := FScrollSelectorPanel.FirstItemIndex;
      end;

      FScrollSelectorPanel.ViewButton(FScrollSelectorPanel.HotItemIndex);
    end;
    VK_DOWN:
    begin
      if (FScrollSelectorPanel.HotItemIndex >= 0) then
      begin
        FScrollSelectorPanel.HotItemIndex := FScrollSelectorPanel.DownItemIndex(FScrollSelectorPanel.HotItemIndex);
      end
      else if (SelectedIndex >= 0) then
      begin
        FScrollSelectorPanel.HotItemIndex := FScrollSelectorPanel.DownItemIndex(SelectedIndex);
      end
      else
      begin
        FScrollSelectorPanel.HotItemIndex := FScrollSelectorPanel.FirstItemIndex;
      end;

      FScrollSelectorPanel.ViewButton(FScrollSelectorPanel.HotItemIndex);
    end;
    VK_RETURN, VK_SPACE:
    begin
      if (FScrollSelectorPanel.HotItemIndex >= 0) then
      begin
        i := FScrollSelectorPanel.ItemIndex;
        SelectedIndex := FScrollSelectorPanel.HotItemIndex;
        FScrollSelectorPanel.HotItemIndex := -1;
        if (i >= 0) then
          FScrollSelectorPanel.DrawItem(i, true);
        OnToolSelect;
      end;
    end;
    VK_ESCAPE:
    begin
      FScrollSelectorPanel.HotItemIndex := -1;
    end;
    VK_HOME:
    begin
      FScrollSelectorPanel.HotItemIndex := FScrollSelectorPanel.FirstItemIndex;
      FScrollSelectorPanel.ViewButton(FScrollSelectorPanel.HotItemIndex);
    end;
    VK_END:
    begin
      FScrollSelectorPanel.HotItemIndex := FScrollSelectorPanel.LastItemIndex;
      FScrollSelectorPanel.ViewButton(FScrollSelectorPanel.HotItemIndex);
    end;
    DROPDOWN_KEY:
    begin
      if (GetKeystate(VK_MENU) and $8000 = $0) then
      begin
        if Assigned(FDropDownWindow) and FDropDownWindow.Visible then
          HideDropDown
        else
          ShowDropDown;
      end;
    end;
  end;  
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeScrollSelector.WMGetDlgCode(
  var Message: TWMGetDlgCode);
begin
  //if TabStop then
    Message.Result := DLGC_WANTARROWS + DLGC_WANTCHARS;
  //else
    //inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeScrollSelector.OnItemsChanged(Sender: TObject);
begin
  FScrollSelectorPanel.SetItemsPosition;
  if not (csLoading in ComponentState) and not (csDesigning in ComponentState) then
    UpdateScroller;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeScrollSelector.Loaded;
begin
  inherited;

  FScrollSelectorPanel.SelectionAppearance.Assign(SelectionAppearance);
  FScrollSelectorPanel.OnSelect := OnDropDownPanelSelect;

  SetScrollSelectorPanelPos;
  UpdateScrollSelectorPanel;

  //if (FDupSelectedIndex < FTools.Count) then
    //SelectedIndex := FDupSelectedIndex;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeScrollSelector.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;  
begin
  inherited;

  P := Point(X, Y);
  if PtInRect(ScrollerUpRect , P) and FSelectorScroller.CanGoBack then
  begin
    FUpScrollerDown := True;
    DrawUpScroller;
  end;

  if PtInRect(ScrollerDownRect , P) and FSelectorScroller.CanGoForward then
  begin
    FDownScrollerDown := True;
    DrawDownScroller;
  end;

  if PtInRect(DropDownBtnRect, P) then
  begin
    FDropDownButtonDown := True;
    DrawDropDownButton;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeScrollSelector.MouseMove(Shift: TShiftState; X,
  Y: Integer);
var
  P: TPoint;  
begin
  inherited;

  P := Point(X, Y);
  if PtInRect(ScrollerUpRect , P) then
  begin
    if not FUpScrollerHot and FSelectorScroller.CanGoBack then
    begin
      FUpScrollerHot := True;
      if (ssLeft in Shift) and not FUpScrollerDown then
        FUpScrollerDown := True;
      DrawUpScroller;
    end;
  end
  else if FUpScrollerHot then
  begin
    FUpScrollerHot := False;
    FUpScrollerDown := False;
    DrawUpScroller;
  end;

  if PtInRect(ScrollerDownRect , P) then
  begin
    if not FDownScrollerHot and FSelectorScroller.CanGoForward then
    begin
      FDownScrollerHot := True;
      if (ssLeft in Shift) and not FDownScrollerDown then
        FDownScrollerDown := True;
      DrawDownScroller;
    end;
  end
  else if FDownScrollerHot then
  begin
    FDownScrollerHot := False;
    FDownScrollerDown := False;
    DrawDownScroller;
  end;

  if PtInRect(DropDownBtnRect, P) then
  begin
    if not FDropDownButtonHot then
    begin
      FDropDownButtonHot := True;
      if (ssLeft in Shift) and not FDropDownButtonDown then
        FDropDownButtonDown := True;
      DrawDropDownButton;
    end;
  end
  else if FDropDownButtonHot then
  begin
    FDropDownButtonHot := False;
    FDropDownButtonDown := False;
    DrawDropDownButton;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeScrollSelector.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
begin
  inherited;

  P := Point(X, Y);
  if PtInRect(ScrollerUpRect , P) and FSelectorScroller.CanGoBack then
  begin
    if FUpScrollerDown then
    begin
      FUpScrollerDown := False;
      DrawUpScroller;
      UpScrollerClick;
    end;
  end;

  if PtInRect(ScrollerDownRect , P) and FSelectorScroller.CanGoForward then
  begin
    if FDownScrollerDown then
    begin
      FDownScrollerDown := False;
      DrawDownScroller;
      DownScrollerClick;
    end;
  end;

  if PtInRect(DropDownBtnRect, P) then
  begin
    if FDropDownButtonDown then
    begin
      FDropDownButtonDown := False;
      DrawDropDownButton;
      DropDownButtonClick;
    end;
  end;

end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeScrollSelector.Notification(
  AComponent: TComponent; AOperation: TOperation);
begin
  inherited;

  if (AOperation = opRemove) and (AComponent = FToolImages) then
  begin
    FToolImages := nil;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeScrollSelector.Paint;
var
  R: TRect;
begin
  inherited;
  R := ClientRect;
  Canvas.Pen.Color := BorderColor;
  Canvas.Brush.Style := bsClear;
  Canvas.RoundRect(R.Left, R.Top, R.Right, R.Bottom, 2, 2);

  DrawScroller;
  DrawDropDownButton;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeScrollSelector.SetBounds(ALeft, ATop, AWidth,
  AHeight: Integer);
begin
  inherited;
  SetScrollSelectorPanelPos;
  UpdateScroller;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeScrollSelector.WndProc(var Message: TMessage);
begin
  inherited;

end;

//------------------------------------------------------------------------------

function TAdvCustomOfficeScrollSelector.GetSelectedIndex: integer;
begin
  Result := FScrollSelectorPanel.ItemIndex; // FSelectedIndex;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeScrollSelector.SetBorderColor(
  const Value: TColor);
begin
  if (FBorderColor <> Value) then
  begin
    FBorderColor := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeScrollSelector.SetCaptionAppearance(
  const Value: TSimpleGradientCaption);
begin
  FCaptionAppearance.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeScrollSelector.SetCaptionItemAppearance(
  const Value: TSimpleGradientCaption);
begin
  FCaptionItemAppearance.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeScrollSelector.SetColorDropDown(
  const Value: TColor);
begin
  FColorDropDown := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeScrollSelector.SetColorDropDownTo(
  const Value: TColor);
begin
  FColorDropDownTo := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeScrollSelector.SetColorTo(const Value: TColor);
begin
  if (FColorTo <> Value) then
  begin
    FColorTo := Value;
    if Assigned(FScrollSelectorPanel) then
    begin
      FScrollSelectorPanel.ColorTo := FColorTo;
      FScrollSelectorPanel.Invalidate;
    end;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeScrollSelector.SetSelectedIndex(
  const Value: integer);
begin
  if Value < Tools.Count then
  begin
    //FSelectedIndex := Value;
    FScrollSelectorPanel.ItemIndex := Value;
    //Invalidate;
  end;
  //FDupSelectedIndex := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeScrollSelector.SetSelectionAppearance(
  const Value: TSelectionAppearance);
begin
  FSelectionAppearance.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeScrollSelector.SetTGradientDirection(
  const Value: TGradientDirection);
begin
  if (FGradientDirection <> Value) then
  begin
    FGradientDirection := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeScrollSelector.SetToolImages(
  const Value: TCustomImageList);
begin
  FToolImages := Value;
  FScrollSelectorPanel.Images := FToolImages;
  FScrollSelectorPanel.SetItemsPosition;
  FScrollSelectorPanel.Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeScrollSelector.SetTools(
  const Value: TAdvScrollSelectorItems);
begin
  FTools.Assign(Value);
end;

//------------------------------------------------------------------------------

function TAdvCustomOfficeScrollSelector.ScrollSelectorPanelRect: TRect;
begin
  Result := Rect(1, 1, Width - SCROLLER_WIDTH, Height-1);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeScrollSelector.SetScrollSelectorPanelPos;
var
  R: TRect;
begin
  if Assigned(FScrollSelectorPanel) then
  begin
    R := ScrollSelectorPanelRect;
    FScrollSelectorPanel.Left := R.Left;
    FScrollSelectorPanel.Top := R.Top;
    FScrollSelectorPanel.Width := R.Right - R.Left;
    FScrollSelectorPanel.Height := R.Bottom - R.Top;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeScrollSelector.UpdateScrollSelectorPanel;
begin
  if Assigned(FScrollSelectorPanel) then
  begin
    FScrollSelectorPanel.Color := Self.Color;
    FScrollSelectorPanel.ColorTo := ColorTo;
    FScrollSelectorPanel.GradientDirection := self.GradientDirection;
    FScrollSelectorPanel.Images := ToolImages;
    FScrollSelectorPanel.IntegralRows := Self.IntegralRows;
    FScrollSelectorPanel.SetItemsPosition;
    FScrollSelectorPanel.FNeedRePaint := True;
    UpdateScroller;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeScrollSelector.UpdateScroller;
var
  i, p: Integer;
begin
  FSelectorScroller.Min := 0;
  FSelectorScroller.Max := 0;
  FSelectorScroller.Position := 0;
  if Assigned(FScrollSelectorPanel) then
  begin
    FSelectorScroller.Min := 0;
    if (FScrollSelectorPanel.RowCount > FScrollSelectorPanel.VisibleRowCount) then
      FSelectorScroller.Max := (FScrollSelectorPanel.RowCount div FScrollSelectorPanel.VisibleRowCount) + (FScrollSelectorPanel.RowCount mod FScrollSelectorPanel.VisibleRowCount)
    else
      FSelectorScroller.Max := 1;
    FSelectorScroller.Min := 1;
    //FSelectorScroller.Position := FScrollSelectorPanel.FTopRow;
    //FSelectorScroller.Position := max(FSelectorScroller.FMin, (FScrollSelectorPanel.FTopRow div FScrollSelectorPanel.VisibleRowCount) + (FScrollSelectorPanel.FTopRow mod FScrollSelectorPanel.VisibleRowCount));

    p := FSelectorScroller.FMin;
    i := 1;
    while (p < FSelectorScroller.FMax) do
    begin
      if (i >= FScrollSelectorPanel.FTopRow) then
        Break;
      Inc(p);
      i := i + FScrollSelectorPanel.VisibleRowCount;
    end;
    FSelectorScroller.Position := p;
  end;
end;

//------------------------------------------------------------------------------

function TAdvCustomOfficeScrollSelector.DropDownBtnRect: TRect;
begin
  if DropDownButton then
    Result := Rect(Width - SCROLLER_WIDTH, Height - (SCROLLER_WIDTH + 4), Width-1, Height -1)
  else
    Result := Rect(-1, -1, -1, -1);  
end;

//------------------------------------------------------------------------------

function TAdvCustomOfficeScrollSelector.ScrollerRect: TRect;
begin
  Result := Rect(-1, -1, -1, -1);
  if FSelectorScroller.Visible then
  begin
    if DropDownButton then
    begin
      Result := DropDownBtnRect;
      Result.Bottom := Result.Top;
      Result.Top := 0;
    end
    else
    begin
      Result := Rect(Width - SCROLLER_WIDTH, 0, Width -1, Height -1);
    end;
  end;
end;

//------------------------------------------------------------------------------

function TAdvCustomOfficeScrollSelector.ScrollerDownRect: TRect;
begin
  Result := Rect(-1, -1, -1, -1);
  if FSelectorScroller.Visible then
  begin
    Result := ScrollerRect;
    Result.Top := ScrollerUpRect.Bottom+1;
  end;
end;

//------------------------------------------------------------------------------

function TAdvCustomOfficeScrollSelector.ScrollerUpRect: TRect;
begin
  Result := Rect(-1, -1, -1, -1);
  if FSelectorScroller.Visible then
  begin
    Result := ScrollerRect;
    Result.Bottom := Result.Top + (Result.Bottom - Result.Top) div 2;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeScrollSelector.SetDropDownButton(
  const Value: Boolean);
begin
  if (FDropDownButton <> Value) then
  begin
    FDropDownButton := Value;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeScrollSelector.DrawDownScroller;
var
  R: TRect;
  Clr, ClrTo, ClrMirror, ClrMirrorTo, BrClr, TxtClr: TColor;
  GradU, GradB: TGDIPGradient;
  x, y: Integer;
begin
  if FSelectorScroller.Visible then
  begin
    R := ScrollerDownRect;
    with FScrollerAppearance do
    begin
      TxtClr := FScrollerAppearance.TextColor;
      Clr := Color;
      ClrTo := ColorTo;
      ClrMirror := ColorMirror;
      ClrMirrorTo := ColorMirrorTo;
      GradU := Gradient;
      GradB := GradientMirror;
      BrClr := Self.BorderColor;

      if FDownScrollerDown then
      begin
        TxtClr := FScrollerAppearance.TextColorDown;
        Clr := ColorDown;
        ClrTo := ColorDownTo;
        ClrMirror := ColorMirrorDown;
        ClrMirrorTo := ColorMirrorDownTo;
        GradU := GradientDown;
        GradB := GradientMirrorDown;
        BrClr := BorderColorDown;
      end
      else if FDownScrollerHot then
      begin
        TxtClr := FScrollerAppearance.TextColorHot;
        Clr := ColorHot;
        ClrTo := ColorHotTo;
        ClrMirror := ColorMirrorHot;
        ClrMirrorTo := ColorMirrorHotTo;
        GradU := GradientHot;
        GradB := GradientMirrorHot;
        BrClr := BorderColorHot;
      end;

      if not FSelectorScroller.CanGoForward or not Enabled then
      begin
        TxtClr := FScrollerAppearance.TextColorDisabled;
        Clr := ColorDisabled;
        ClrTo := ColorDisabledTo;
        ClrMirror := ColorMirrorDisabled;
        ClrMirrorTo := ColorMirrorDisabledTo;
        GradU := GradientDisabled;
        GradB := GradientMirrorDisabled;
        BrClr := Self.BorderColor; // BorderColorDisabled;
      end;

      R := Rect(R.Left+1, R.Top+1, R.Right-1, R.Bottom-1);
      if (ClrTo <> clNone) then
      begin
        DrawVistaGradient(Canvas, R, Clr, ClrTo, ClrMirror, ClrMirrorTo, Clr{BrClr}, GradU, GradB, '', Canvas.Font, Enabled, False, aaClearType, False, False, tpTop);
      end
      else
      begin
        Canvas.Brush.Color := Clr;
        Canvas.Pen.Color := Clr;
        Canvas.Rectangle(R);
      end;
      R := Rect(R.Left-1, R.Top-1, R.Right+1, R.Bottom+1);

      //--- Draw Border
      Canvas.Pen.Color := BrClr;
      Canvas.MoveTo(R.Left, R.Bottom);
      Canvas.LineTo(R.Left, R.Top);
      Canvas.MoveTo(R.Left, R.Top);
      Canvas.LineTo(R.Right, R.Top);
      Canvas.MoveTo(R.Right, R.Top);
      Canvas.LineTo(R.Right, R.Bottom);

      //--- Draw DownArrow
      x := R.Left + (R.Right - R.Left - 5)div 2;
      y := R.Top + ((R.Bottom - R.Top)div 2) - 1;
      Canvas.Pen.Color := TxtClr;
      Canvas.MoveTo(x, y);
      Canvas.LineTo(x + 5, y);
      Inc(y);
      Inc(x);
      Canvas.MoveTo(x, y);
      Canvas.LineTo(x + 3, y);
      Canvas.Pixels[x - 1, y] := Clr;
      Canvas.Pixels[x + 3, y] := Clr;
      Inc(y);
      Inc(x);
      Canvas.MoveTo(x, y);
      Canvas.LineTo(x + 1, y);
      Canvas.Pixels[x - 1, y] := Clr;
      Canvas.Pixels[x + 1, y] := Clr;
      Canvas.Pixels[x, y+1] := Clr;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeScrollSelector.DrawScroller;
begin
  DrawUpScroller;
  DrawDownScroller;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeScrollSelector.DrawUpScroller;
var
  R: TRect;
  Clr, ClrTo, ClrMirror, ClrMirrorTo, BrClr, TxtClr: TColor;
  GradU, GradB: TGDIPGradient;
  x, y: Integer;
begin
  if FSelectorScroller.Visible then
  begin
    R := ScrollerUpRect;
    with FScrollerAppearance do
    begin
      TxtClr := FScrollerAppearance.TextColor;
      Clr := Color;
      ClrTo := ColorTo;
      ClrMirror := ColorMirror;
      ClrMirrorTo := ColorMirrorTo;
      GradU := Gradient;
      GradB := GradientMirror;
      BrClr := Self.BorderColor;

      if FUpScrollerDown then
      begin
        TxtClr := FScrollerAppearance.TextColorDown;
        Clr := ColorDown;
        ClrTo := ColorDownTo;
        ClrMirror := ColorMirrorDown;
        ClrMirrorTo := ColorMirrorDownTo;
        GradU := GradientDown;
        GradB := GradientMirrorDown;
        BrClr := BorderColorDown;
      end
      else if FUpScrollerHot then
      begin
        TxtClr := FScrollerAppearance.TextColorHot;
        Clr := ColorHot;
        ClrTo := ColorHotTo;
        ClrMirror := ColorMirrorHot;
        ClrMirrorTo := ColorMirrorHotTo;
        GradU := GradientHot;
        GradB := GradientMirrorHot;
        BrClr := BorderColorHot;
      end;

      if not FSelectorScroller.CanGoBack or not Enabled then
      begin
        TxtClr := FScrollerAppearance.TextColorDisabled;
        Clr := ColorDisabled;
        ClrTo := ColorDisabledTo;
        ClrMirror := ColorMirrorDisabled;
        ClrMirrorTo := ColorMirrorDisabledTo;
        GradU := GradientDisabled;
        GradB := GradientMirrorDisabled;
        BrClr := Self.BorderColor; // BorderColorDisabled
      end;

      R := Rect(R.Left+1, R.Top+1, R.Right-1, R.Bottom-1);
      if (ClrTo <> clNone) then
      begin
        DrawVistaGradient(Canvas, R, Clr, ClrTo, ClrMirror, ClrMirrorTo, Clr{BrClr}, GradU, GradB, '', Canvas.Font, Enabled, False, aaClearType, False, False, tpTop);
      end
      else
      begin
        Canvas.Brush.Color := Clr;
        Canvas.Pen.Color := Clr;
        Canvas.Rectangle(R);
      end;
      R := Rect(R.Left-1, R.Top-1, R.Right+1, R.Bottom+1);

      //--- Draw Border
      Canvas.Pen.Color := BrClr;
      Canvas.MoveTo(R.Left, R.Top);
      Canvas.LineTo(R.Left, R.Bottom);
      Canvas.MoveTo(R.Left, R.Bottom);
      Canvas.LineTo(R.Right, R.Bottom);
      Canvas.MoveTo(R.Right, R.Bottom);
      Canvas.LineTo(R.Right, R.Top);
      Canvas.MoveTo(R.Right-1, R.Top);
      Canvas.LineTo(R.Left, R.Top);

      //--- Draw UpArrow
      x := R.Left + (R.Right - R.Left - 5)div 2;
      y := R.Top + ((R.Bottom - R.Top)div 2) + 1;
      Canvas.Pen.Color := TxtClr;
      Canvas.MoveTo(x, y);
      Canvas.LineTo(x + 5, y);
      Dec(y);
      Inc(x);
      Canvas.MoveTo(x, y);
      Canvas.LineTo(x + 3, y);
      Canvas.Pixels[x - 1, y] := Clr;
      Canvas.Pixels[x + 3, y] := Clr;
      Dec(y);
      Inc(x);
      Canvas.MoveTo(x, y);
      Canvas.LineTo(x + 1, y);
      Canvas.Pixels[x - 1, y] := Clr;
      Canvas.Pixels[x + 1, y] := Clr;
      Canvas.Pixels[x, y-1] := Clr;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeScrollSelector.DrawDropDownButton;
var
  R: TRect;
  Clr, ClrTo, ClrMirror, ClrMirrorTo, BrClr, TxtClr: TColor;
  GradU, GradB: TGDIPGradient;
  x, y: Integer;
begin
  if DropDownButton then
  begin
    R := DropDownBtnRect;
    with FScrollerAppearance do
    begin
      TxtClr := FScrollerAppearance.TextColor;
      Clr := Color;
      ClrTo := ColorTo;
      ClrMirror := ColorMirror;
      ClrMirrorTo := ColorMirrorTo;
      GradU := Gradient;
      GradB := GradientMirror;
      BrClr := Self.BorderColor;

      if FDropDownButtonDown then
      begin
        TxtClr := FScrollerAppearance.TextColorDown;
        Clr := ColorDown;
        ClrTo := ColorDownTo;
        ClrMirror := ColorMirrorDown;
        ClrMirrorTo := ColorMirrorDownTo;
        GradU := GradientDown;
        GradB := GradientMirrorDown;
        BrClr := BorderColorDown;
      end
      else if FDropDownButtonHot then
      begin
        TxtClr := FScrollerAppearance.TextColorHot;
        Clr := ColorHot;
        ClrTo := ColorHotTo;
        ClrMirror := ColorMirrorHot;
        ClrMirrorTo := ColorMirrorHotTo;
        GradU := GradientHot;
        GradB := GradientMirrorHot;
        BrClr := BorderColorHot;
      end;

      R := Rect(R.Left+1, R.Top+1, R.Right-1, R.Bottom-1);
      if (ClrTo <> clNone) then
      begin
        DrawVistaGradient(Canvas, R, Clr, ClrTo, ClrMirror, ClrMirrorTo, Clr{BrClr}, GradU, GradB, '', Canvas.Font, Enabled, False, aaClearType, False, False, tpTop);
      end
      else
      begin
        Canvas.Brush.Color := Clr;
        Canvas.Pen.Color := Clr;
        Canvas.Rectangle(R);
      end;
      R := Rect(R.Left-1, R.Top-1, R.Right+1, R.Bottom+1);

      //--- Draw Border
      Canvas.Pen.Color := BrClr;
      Canvas.MoveTo(R.Left, R.Bottom);
      Canvas.LineTo(R.Left, R.Top);
      Canvas.MoveTo(R.Left, R.Top);
      Canvas.LineTo(R.Right, R.Top);
      Canvas.MoveTo(R.Right, R.Top);
      Canvas.LineTo(R.Right, R.Bottom);
      Canvas.MoveTo(R.Right-1, R.Bottom);
      Canvas.LineTo(R.Left, R.Bottom);

      //--- Draw DownArrow
      x := R.Left + (R.Right - R.Left - 5)div 2;
      y := R.Top + ((R.Bottom - R.Top)div 2);
      Canvas.Pen.Color := TxtClr;
      Canvas.MoveTo(x - 1, y - 3);
      Canvas.LineTo(x + 6, y - 3);

      Canvas.MoveTo(x, y);
      Canvas.LineTo(x + 5, y);
      Inc(y);
      Inc(x);
      Canvas.MoveTo(x, y);
      Canvas.LineTo(x + 3, y);
      Canvas.Pixels[x - 1, y] := Clr;
      Canvas.Pixels[x + 3, y] := Clr;
      Inc(y);
      Inc(x);
      Canvas.MoveTo(x, y);
      Canvas.LineTo(x + 1, y);
      Canvas.Pixels[x - 1, y] := Clr;
      Canvas.Pixels[x + 1, y] := Clr;
      Canvas.Pixels[x, y+1] := Clr;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeScrollSelector.SetScrollerAppearance(
  const Value: TSelectionAppearance);
begin
  FScrollerAppearance.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeScrollSelector.HideDropDown;
begin
  if Assigned(FDropDownWindow) and FDropDownWindow.Visible then
    FDropDownWindow.Hide;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeScrollSelector.ShowDropDown;
begin
  DropDownButtonClick;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeScrollSelector.DropDownButtonClick;
var
  R: TRect;
  P: TPoint;
  i, h, w: Integer;
  mon: TMonitor;
begin
  if (FOldDropDownHeight > 0) and (FOldDropDownWidth > 0) then
  begin
    h := FOldDropDownHeight;
    w := FOldDropDownWidth;
  end
  else
  begin
    h := FDropDownHeight;
    w := Max(200, Width);
  end;

  if not Assigned(FDropDownWindow) then
  begin
    FDropDownWindow := TScrollSelectorDropDownWindow.CreateNew(Self);
    FDropDownWindow.BorderIcons := [];
    FDropDownWindow.BorderStyle := bsNone;
    FDropDownWindow.Ctl3D := false;
    FDropDownWindow.FormStyle := fsStayOnTop;
    FDropDownWindow.Visible := False;
    FDropDownWindow.AutoScroll := False;
    FDropDownWindow.BorderWidth := 0;
    FDropDownWindow.OnHide := OnDropDownWindowHide;
    FDropDownWindow.OnClose := OnDropDownWindowClose;
    FDropDownWindow.ScrollSelector := Self;
    FDropDownWindow.ShowBorder := True;
    FDropDownWindow.Color := Self.ColorDropDown;
    FDropDownWindow.SizeGrip := DropDownSizeable;
    FDropDownWindow.DropDownCaption := DropDownCaption;
  end;
  FDropDownWindow.Height := h;
  FDropDownWindow.Width := w;

  //-- Set Panels here
  if not Assigned(FDropDownFullWidthItemPanel) then
  begin
    FDropDownFullWidthItemPanel := TAdvScrollSelectorPanel.Create(FDropDownWindow);
    FDropDownFullWidthItemPanel.Parent := FDropDownWindow;
    FDropDownFullWidthItemPanel.AutoHeight := True;
    FDropDownFullWidthItemPanel.ShowCaptionItem := False;
    FDropDownFullWidthItemPanel.ShowAutoSizeButton := False;
    FDropDownFullWidthItemPanel.ShowFullWidthItem := True;
    FDropDownFullWidthItemPanel.AdvOfficeScrollSelector := Self;
    FDropDownFullWidthItemPanel.ButtonMargin := 0;
    FDropDownFullWidthItemPanel.Images := ToolImages;
    FDropDownFullWidthItemPanel.SelectionAppearance.Assign(SelectionAppearance);
    FDropDownFullWidthItemPanel.CaptionAppearance.Assign(CaptionItemAppearance);
    FDropDownFullWidthItemPanel.Color := ColorDropDown;
    FDropDownFullWidthItemPanel.ColorTo := ColorDropDownTo;

    FDropDownFullWidthItemPanel.SetItemsPosition;
    FDropDownFullWidthItemPanel.Visible := (FullWidthButtonCount > 0);
    FDropDownFullWidthItemPanel.ItemIndex := Self.SelectedIndex;
    FDropDownFullWidthItemPanel.OnShouldHide := OnDropDownPanelShouldHide;
    FDropDownFullWidthItemPanel.OnSelect := OnDropDownPanelSelect;
    FDropDownFullWidthItemPanel.OnHotTool := OnDropDownPanelHotTool;
    if Assigned(FOnDrawTool) then
      FDropDownFullWidthItemPanel.OnDrawItem := OnDropDownPanelDrawTool
    else
      FDropDownFullWidthItemPanel.OnDrawItem := nil;
  end;
  FDropDownWindow.FullWidthSelector := FDropDownFullWidthItemPanel;

  if not Assigned(FItemsScrollBox) then
  begin
    FItemsScrollBox := TScrollBox.Create(FDropDownWindow);
    FItemsScrollBox.Parent := FDropDownWindow;
    FItemsScrollBox.Height := 100;
    FItemsScrollBox.BorderStyle := bsNone;
    FItemsScrollBox.Visible := (AutoSizeButtonCount > 0);
    FItemsScrollBox.Color := ColorDropDown;
    FItemsScrollBox.DoubleBuffered := True;
  end;
  FDropDownWindow.ScrollBox := FItemsScrollBox;

  if not Assigned(FDropDownItemsPanel) then
  begin
    FDropDownItemsPanel := TAdvScrollSelectorPanel.Create(FItemsScrollBox);
    FDropDownItemsPanel.Parent := FItemsScrollBox;
    FDropDownItemsPanel.AutoHeight := True;
    FDropDownItemsPanel.ShowCaptionItem := True;
    FDropDownItemsPanel.ShowAutoSizeButton := True;
    FDropDownItemsPanel.ShowFullWidthItem := False;
    FDropDownItemsPanel.AdvOfficeScrollSelector := Self;
    FDropDownItemsPanel.Images := ToolImages;
    FDropDownItemsPanel.Color := ColorDropDown;
    FDropDownItemsPanel.ColorTo := ColorDropDownTo;
    FDropDownItemsPanel.IntegralRows := Self.IntegralRows;

    FDropDownItemsPanel.SelectionAppearance.Assign(SelectionAppearance);
    FDropDownItemsPanel.CaptionAppearance.Assign(CaptionItemAppearance);
    FDropDownItemsPanel.Align := alTop;
    FDropDownItemsPanel.SetItemsPosition;
    FDropDownItemsPanel.Visible := True;
    FDropDownItemsPanel.ItemIndex := Self.SelectedIndex;
    FDropDownItemsPanel.OnShouldHide := OnDropDownPanelShouldHide;
    FDropDownItemsPanel.OnSelect := OnDropDownPanelSelect;
    FDropDownItemsPanel.OnHotTool := OnDropDownPanelHotTool;

    i := Min(200, FDropDownItemsPanel.Height);
    FItemsScrollBox.Height := i;
  end;
  FDropDownWindow.ItemsSelector := FDropDownItemsPanel;

  FDropDownWindow.Position := poDesigned;
  FDropDownWindow.SetPositions;

  P := Point(0, 0);
  P := ClientToScreen(P);

  mon := Screen.MonitorFromPoint(p);
  if Assigned(mon) then
    R := mon.WorkAreaRect
  else  
    SystemParametersInfo(SPI_GETWORKAREA, 0, @R, 0);

  if R.Bottom > (P.Y + FDropDownWindow.Height + 2) then
  begin
    FDropDownWindow.Left := P.X;
    if FDropDownWindow.Left + FDropDownWindow.Width > R.Right then
    begin
      FDropDownWindow.Left:= P.X - ((P.X + FDropDownWindow.Width) - R.Right); //(P.X+ Width) - FDropDownWindow.Width;
      FDropDownWindow.ShowLeft:= true;
    end
    else
      FDropDownWindow.ShowLeft:= false;

    FDropDownWindow.Top := P.Y;
    FDropDownWindow.ShowAbove := false;
  end
  else
  begin
    FDropDownWindow.Left := P.X;
    if FDropDownWindow.Left + FDropDownWindow.Width > R.Right then
    begin
      FDropDownWindow.Left:= P.X - ((P.X + FDropDownWindow.Width) - R.Right); //(P.X+ Width) - FDropDownWindow.Width;
      FDropDownWindow.ShowLeft:= true;
    end
    else
      FDropDownWindow.ShowLeft:= false;

    FDropDownWindow.Top := P.Y - ((P.Y + FDropDownWindow.Height + 2) - R.Bottom); //P.Y - self.Height - FDropDownWindow.Height + 1;
    FDropDownWindow.ShowAbove := true;
  end;

  FDropDownWindow.Visible := true;

  Invalidate;
  if Assigned(FOnScrollDown) then
    FOnScrollDown(Self);
end;

procedure TAdvCustomOfficeScrollSelector.EndUpdate;
begin
  SetScrollSelectorPanelPos;
  UpdateScrollSelectorPanel;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeScrollSelector.DownScrollerClick;
begin
  if Assigned(FScrollSelectorPanel) then
  begin
    FScrollSelectorPanel.ScrollRows({1}FScrollSelectorPanel.FVisibleRowCount);
    //UpdateScroller;
    //InvalidateScroller;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeScrollSelector.UpScrollerClick;
begin
  if Assigned(FScrollSelectorPanel) then
  begin
    FScrollSelectorPanel.ScrollRows({-1}-FScrollSelectorPanel.FVisibleRowCount);
    //UpdateScroller;
    //InvalidateScroller;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeScrollSelector.OnScrollSelectorPanelScroll(Sender: TObject);
begin
  UpdateScroller;
  InvalidateScroller;
end;

//------------------------------------------------------------------------------

function TAdvCustomOfficeScrollSelector.GetTopRow: Integer;
begin
  Result := -1;
  if Assigned(FScrollSelectorPanel) then
    Result := FScrollSelectorPanel.TopRow;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeScrollSelector.SetTopRow(const Value: Integer);
begin
  if Assigned(FScrollSelectorPanel) and (Value <> FScrollSelectorPanel.TopRow) then
  begin
    FScrollSelectorPanel.SetTopRow(Value);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeScrollSelector.InvalidateScroller;
var
  R: TRect;
begin
  R := ScrollerRect;
  R.Right := R.Right + 1;
  R.Bottom := R.Bottom + 1; 
  InvalidateRect(Handle, @R, True);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeScrollSelector.OnDropDownWindowClose(
  Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
  FDropDownWindow := nil;
  //FDropDownWindowPanel := nil;
  //FDropDownCaptionPanel := nil;
  FItemsScrollBox := nil;
  FDropDownItemsPanel := nil;
  FDropDownFullWidthItemPanel := nil;
  //FDropDownResizePanel := nil;

  if Assigned(FOnScrollUp) then
    FOnScrollUp(Self);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeScrollSelector.OnDropDownWindowHide(
  Sender: TObject);
begin
  if Assigned(FDropDownWindow) then
  begin
    FOldDropDownHeight := FDropDownWindow.Height;
    FOldDropDownWidth := FDropDownWindow.Width;
  end;
  
  PostMessage(FDropDownWindow.Handle, WM_CLOSE, 0, 0);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeScrollSelector.OnDropDownPanelShouldHide(Sender: TObject);
begin
  if Assigned(FDropDownWindow) then
    FDropDownWindow.Hide;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeScrollSelector.DoSelect(Index: integer;
  Item: TAdvScrollSelectorItem);
begin
  if Assigned(FOnSelect) then
    FOnSelect(Self, Index, Item);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeScrollSelector.OnToolSelect;
begin
  DoSelect(SelectedIndex, Tools[SelectedIndex]);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeScrollSelector.OnDropDownPanelSelect(
  Sender: TObject);
begin
  if (Sender is TAdvScrollSelectorPanel) then
  begin
    SelectedIndex := TAdvScrollSelectorPanel(Sender).ItemIndex;
  end;

  if (Sender <> FScrollSelectorPanel) then
    FScrollSelectorPanel.ViewSelectedButton;
  OnToolSelect;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeScrollSelector.OnDropDownPanelHotTool(
  Sender: TObject; HotItemIndex: integer);
begin
  if Assigned(FOnHotTool) then
    FOnHotTool(self, HotItemIndex);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeScrollSelector.OnDropDownPanelDrawTool(Sender: TObject; ItemIndex: integer; R: TRect);
begin
  if (Sender is TAdvScrollSelectorPanel) then
    FOnDrawTool(self, TAdvScrollSelectorPanel(Sender).Canvas, ItemIndex, R);
end;

//------------------------------------------------------------------------------

function TAdvCustomOfficeScrollSelector.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn))) + '.' + IntToStr(Lo(Hiword(vn))) + '.' + IntToStr(Hi(Loword(vn))) + '.' + IntToStr(Lo(Loword(vn)));
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeScrollSelector.SetVersion(const Value: string);
begin

end;

//------------------------------------------------------------------------------

function TAdvCustomOfficeScrollSelector.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER, REL_VER), MakeWord(MIN_VER, MAJ_VER));
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeScrollSelector.SetComponentStyle(
  AStyle: TTMSStyle);
begin
  if (Astyle in [tsOffice2007Luna, tsOffice2007Obsidian]) then
  begin
      ScrollerAppearance.TextColor := $7F7F7F;
      ScrollerAppearance.TextColorChecked := $7F7F7F;
      ScrollerAppearance.TextColorDisabled := $B7B7B7;
      ScrollerAppearance.TextColorDown := clBlack;
      ScrollerAppearance.TextColorHot := clBlack;

      SelectionAppearance.Color := $EBFDFF;
      SelectionAppearance.ColorTo := $ABEBFF;
      SelectionAppearance.ColorMirror := $69D6FF;
      SelectionAppearance.ColorMirrorTo := $96E4FF;

      SelectionAppearance.ColorHot := $EBFDFF;
      SelectionAppearance.ColorHotTo := $ABEBFF;
      SelectionAppearance.ColorMirrorHot := $69D6FF;
      SelectionAppearance.ColorMirrorHotTo := $96E4FF;
      SelectionAppearance.BorderColorHot := $0099CEDB;
      SelectionAppearance.GradientHot := ggVertical;
      SelectionAppearance.GradientMirrorHot := ggVertical;
      SelectionAppearance.TextColorHot := clBlack;

      SelectionAppearance.ColorDown := $76AFF1;
      SelectionAppearance.ColorDownTo := $4190F3;
      SelectionAppearance.ColorMirrorDown := $0E72F1;
      SelectionAppearance.ColorMirrorDownTo := $4C9FFD;
      SelectionAppearance.BorderColorDown := $45667B;
      SelectionAppearance.GradientDown := ggVertical;
      SelectionAppearance.GradientMirrorDown := ggVertical;
      SelectionAppearance.TextColorDown := clBlack;

      SelectionAppearance.ColorChecked := $B5DBFB;
      SelectionAppearance.ColorCheckedTo := $78C7FE;
      SelectionAppearance.ColorMirrorChecked := $9FEBFD;
      SelectionAppearance.ColorMirrorCheckedTo := $56B4FE;
      SelectionAppearance.BorderColorChecked := $45667B;
      SelectionAppearance.TextColorChecked := clBlack;
      SelectionAppearance.GradientChecked := ggVertical;
      SelectionAppearance.GradientMirrorChecked := ggVertical;
      SelectionAppearance.Rounded := True;
  end;


  if (Astyle in [tsOffice2003Blue, tsOffice2003Silver, tsOffice2003Olive, tsWhidbey]) then
  begin
      ScrollerAppearance.TextColor := clBlack;
      ScrollerAppearance.TextColorChecked := clBlack;
      ScrollerAppearance.TextColorDisabled := $8D8D8D;
      ScrollerAppearance.TextColorDown := clBlack;
      ScrollerAppearance.TextColorHot := clBlack;

      ScrollerAppearance.ColorHot := $EBFDFF;
      ScrollerAppearance.ColorHotTo := $ACECFF;
      ScrollerAppearance.ColorMirrorHot := $59DAFF;
      ScrollerAppearance.ColorMirrorHotTo := $A4E9FF;
      ScrollerAppearance.BorderColorHot := $75A3BB;//$99CEDB;
      ScrollerAppearance.GradientHot := ggVertical;
      ScrollerAppearance.GradientMirrorHot := ggVertical;

      ScrollerAppearance.ColorDown := $76AFF1;
      ScrollerAppearance.ColorDownTo := $4190F3;
      ScrollerAppearance.ColorMirrorDown := $0E72F1;
      ScrollerAppearance.ColorMirrorDownTo := $4C9FFD;
      ScrollerAppearance.BorderColorDown := $45667B;
      ScrollerAppearance.GradientDown := ggVertical;
      ScrollerAppearance.GradientMirrorDown := ggVertical;

      ScrollerAppearance.ColorChecked := $B5DBFB;
      ScrollerAppearance.ColorCheckedTo := $78C7FE;
      ScrollerAppearance.ColorMirrorChecked := $9FEBFD;
      ScrollerAppearance.ColorMirrorCheckedTo := $56B4FE;
      ScrollerAppearance.GradientChecked := ggVertical;
      ScrollerAppearance.GradientMirrorChecked := ggVertical;

      ScrollerAppearance.ColorDisabled := $F2ECE9;
      ScrollerAppearance.ColorDisabledTo := $F2ECE9;
      ScrollerAppearance.ColorMirrorDisabled := $F2ECE9;
      ScrollerAppearance.ColorMirrorDisabledTo := clSilver;

      SelectionAppearance.Color := clRed;//$EBFDFF;
      SelectionAppearance.ColorTo := clRed;//$ABEBFF;
      SelectionAppearance.ColorMirror := clRed;//$69D6FF;
      SelectionAppearance.ColorMirrorTo := clRed;//$96E4FF;

      {
      SelectionAppearance.ColorHot := $C2EEFF;
      SelectionAppearance.ColorHotTo := $C2EEFF;
      SelectionAppearance.ColorMirrorHot := $C2EEFF;
      SelectionAppearance.ColorMirrorHotTo := $C2EEFF;
      SelectionAppearance.BorderColorHot := $800000;
      SelectionAppearance.GradientHot := ggVertical;
      SelectionAppearance.GradientMirrorHot := ggVertical;
      SelectionAppearance.TextColorHot := clBlack;

      SelectionAppearance.ColorDown := $3E80FE;
      SelectionAppearance.ColorDownTo := $3E80FE;
      SelectionAppearance.ColorMirrorDown := $3E80FE;
      SelectionAppearance.ColorMirrorDownTo := $3E80FE;
      SelectionAppearance.BorderColorDown := $800000;
      SelectionAppearance.GradientDown := ggVertical;
      SelectionAppearance.GradientMirrorDown := ggVertical;
      SelectionAppearance.TextColorDown := clBlack;

      SelectionAppearance.ColorChecked := $6FC0FF;
      SelectionAppearance.ColorCheckedTo := $6FC0FF;
      SelectionAppearance.ColorMirrorChecked := $6FC0FF;
      SelectionAppearance.ColorMirrorCheckedTo := $6FC0FF;
      SelectionAppearance.BorderColorChecked := $800000;
      SelectionAppearance.TextColorChecked := clBlack;
      SelectionAppearance.GradientChecked := ggVertical;
      SelectionAppearance.GradientMirrorChecked := ggVertical;
      SelectionAppearance.Rounded := False;
      }

      SelectionAppearance.ColorHot := $EBFDFF;
      SelectionAppearance.ColorHotTo := $ACECFF;
      SelectionAppearance.ColorMirrorHot := $59DAFF;
      SelectionAppearance.ColorMirrorHotTo := $A4E9FF;
      SelectionAppearance.BorderColorHot := $99CEDB;
      SelectionAppearance.GradientHot := ggVertical;
      SelectionAppearance.GradientMirrorHot := ggVertical;


      SelectionAppearance.ColorDown := $76AFF1;
      SelectionAppearance.ColorDownTo := $4190F3;
      SelectionAppearance.ColorMirrorDown := $0E72F1;
      SelectionAppearance.ColorMirrorDownTo := $4C9FFD;
      SelectionAppearance.BorderColorDown := $45667B;
      SelectionAppearance.GradientDown := ggVertical;
      SelectionAppearance.GradientMirrorDown := ggVertical;

      SelectionAppearance.ColorChecked := $B5DBFB;
      SelectionAppearance.ColorCheckedTo := $78C7FE;
      SelectionAppearance.ColorMirrorChecked := $9FEBFD;
      SelectionAppearance.ColorMirrorCheckedTo := $56B4FE;
      SelectionAppearance.GradientChecked := ggVertical;
      SelectionAppearance.GradientMirrorChecked := ggVertical;

  end;

  case AStyle of
  tsOffice2003Blue:
   begin
      Color := $F6F6F6;
      ColorTo := clNone;
      ColorDropDown := $FDEBDC;
      ColorDropDownTo := $F9DBC4;
      BorderColor := $E0B99B;//$962D00;
      BorderDropDownColor := $C9662A;
      FocusColor := $FDEBDC;
      FocusColorTo := clNone;

      ResizerAppearance.BorderColor := clInactiveBorder;
      ResizerAppearance.Color := $FFEFE3;
      ResizerAppearance.ColorTo := $E0A47B;

      IconBarAppearance.BorderColor := clInactiveBorder;
      IconBarAppearance.Color := $FFEFE3;
      IconBarAppearance.ColorTo := $E0A47B;

      CaptionAppearance.BorderColor := $F9DBC4;
      CaptionAppearance.Color := $C9662A;
      CaptionAppearance.ColorTo := clNone;
      CaptionAppearance.TextColor := clWhite;

      CaptionItemAppearance.BorderColor := $F9DBC4;
      CaptionItemAppearance.Color := $C9662A;
      CaptionItemAppearance.ColorTo := clNone;
      CaptionItemAppearance.TextColor := clWhite;

      ScrollerAppearance.Color := $EEDBC8;
      ScrollerAppearance.ColorTo := $F6DDC9;
      ScrollerAppearance.ColorMirror := $EDD4C0;
      ScrollerAppearance.ColorMirrorTo := $F7E1D0;
      ScrollerAppearance.BorderColor := $E0B99B;
      ScrollerAppearance.Gradient := ggVertical;
      ScrollerAppearance.GradientMirror := ggVertical;

     end;

    tsOffice2003Olive:
      begin

        Color := $EEF4F4;
        ColorTo := clNone;
        ColorDropDown := $D0EAE6;
        ColorDropDownTo := $ADDED1;
        BorderColor := $5E8D75;
        BorderDropDownColor := $5E8674;
        FocusColor := $EEF4F4;
        FocusColorTo := clNone;

        ResizerAppearance.BorderColor := clInactiveBorder;
        ResizerAppearance.Color := $D4F2ED;
        ResizerAppearance.ColorTo := $8FC4B5;

        IconBarAppearance.BorderColor := clInactiveBorder;
        IconBarAppearance.Color := $D4F2ED;
        IconBarAppearance.ColorTo := $8FC4B5;

        CaptionAppearance.BorderColor := $ADDED1;
        CaptionAppearance.Color := $5E8674;
        CaptionAppearance.ColorTo := clNone;
        CaptionAppearance.TextColor := clWhite;

        CaptionItemAppearance.BorderColor := $ADDED1;
        CaptionItemAppearance.Color := $9FD4C5;
        CaptionItemAppearance.ColorTo := clNone;
        CaptionItemAppearance.TextColor := clBlack;

        ScrollerAppearance.Color := $CFF0EA;
        ScrollerAppearance.ColorTo := $CFF0EA;
        ScrollerAppearance.ColorMirror := $CFF0EA;
        ScrollerAppearance.ColorMirrorTo := $8CC0B1;
        ScrollerAppearance.BorderColor := $8CC0B1;
        ScrollerAppearance.Gradient := ggVertical;
        ScrollerAppearance.GradientMirror := ggVertical;
      end;

      
    tsOffice2003Silver:
      begin

        Color := $F0E8E8;
        ColorTo := clNone;
        ColorDropDown := $FFFAFD;
        ColorDropDownTo := $E5D7D7;
        BorderColor := $947C7C;
        BorderDropDownColor := $99797A;
        FocusColor := $FFFAFD;
        FocusColorTo := clNone;

        ResizerAppearance.BorderColor := clInactiveBorder;
        ResizerAppearance.Color := $F8F0F0;
        ResizerAppearance.ColorTo := $B09193;

        IconBarAppearance.BorderColor := clInactiveBorder;
        IconBarAppearance.Color := $F8F0F0;
        IconBarAppearance.ColorTo := $B09193;

        CaptionAppearance.BorderColor := $E4DADB;
        CaptionAppearance.Color := $99797A;
        CaptionAppearance.ColorTo := clNone;
        CaptionAppearance.TextColor := clWhite;

        CaptionItemAppearance.BorderColor := $E4DADB;
        CaptionItemAppearance.Color := $99797A;
        CaptionItemAppearance.ColorTo := clNone;
        CaptionItemAppearance.TextColor := clWhite;

              
        ScrollerAppearance.Color := $EDD4C0;
        ScrollerAppearance.ColorTo := $00E6D8D8;
        ScrollerAppearance.ColorMirror := $EDD4C0;
        ScrollerAppearance.ColorMirrorTo := $C8B2B3;
        ScrollerAppearance.BorderColor := $927476;
        ScrollerAppearance.Gradient := ggVertical;
        ScrollerAppearance.GradientMirror := ggVertical;
      end;

    tsOffice2003Classic:
      begin

        Color := clBtnFace;
        ColorTo := clNone;
        ColorDropDown := clWhite;
        ColorDropDownTo := clNone;
        BorderColor := clBlack;//clActiveBorder;
        BorderDropDownColor := clInactiveBorder;
        FocusColor := clBtnHighLight;
        FocusColorTo := clNone;

        ResizerAppearance.BorderColor := clBlack;
        ResizerAppearance.Color := clBtnFace;
        ResizerAppearance.ColorTo := clBtnFace;

        IconBarAppearance.BorderColor := clActiveBorder;
        IconBarAppearance.Color := clBtnFace;
        IconBarAppearance.ColorTo := clNone;

        CaptionAppearance.BorderColor := clWhite;
        CaptionAppearance.Color := clActiveCaption;
        CaptionAppearance.ColorTo := clNone;
        CaptionAppearance.TextColor := clWhite;

        CaptionItemAppearance.BorderColor := clWhite;
        CaptionItemAppearance.Color := clActiveCaption;
        CaptionItemAppearance.ColorTo := clNone;
        CaptionItemAppearance.TextColor := clWhite;

        ScrollerAppearance.TextColor := clBlack;
        ScrollerAppearance.TextColorChecked := clWhite;
        ScrollerAppearance.TextColorDisabled := clGray;
        ScrollerAppearance.TextColorDown := clWhite;
        ScrollerAppearance.TextColorHot := clWhite;

        ScrollerAppearance.Color := clWhite;
        ScrollerAppearance.ColorTo := $B9D8DC;
        ScrollerAppearance.ColorMirror := $B9D8DC;
        ScrollerAppearance.ColorMirrorTo := $B9D8DC;
        ScrollerAppearance.BorderColor := $B9D8DC;
        ScrollerAppearance.Gradient := ggVertical;
        ScrollerAppearance.GradientMirror := ggVertical;

        ScrollerAppearance.ColorHot := $EFD3C6;
        ScrollerAppearance.ColorHotTo := $EFD3C6;
        ScrollerAppearance.ColorMirrorHot := $EFD3C6;
        ScrollerAppearance.ColorMirrorHotTo := $EFD3C6;
        ScrollerAppearance.BorderColorHot := clHighlight;
        ScrollerAppearance.GradientHot := ggVertical;
        ScrollerAppearance.GradientMirrorHot := ggVertical;

        ScrollerAppearance.ColorDown := $B59284;
        ScrollerAppearance.ColorDownTo := $B59284;
        ScrollerAppearance.ColorMirrorDown := $B59284;
        ScrollerAppearance.ColorMirrorDownTo := $B59284;
        ScrollerAppearance.BorderColorDown := clHighlight;
        ScrollerAppearance.GradientDown := ggVertical;
        ScrollerAppearance.GradientMirrorDown := ggVertical;

        ScrollerAppearance.ColorChecked := $B9D8DC;
        ScrollerAppearance.ColorCheckedTo := $B9D8DC;
        ScrollerAppearance.ColorMirrorChecked := $B9D8DC;
        ScrollerAppearance.ColorMirrorCheckedTo := $B9D8DC;
        ScrollerAppearance.BorderColorChecked := clBlack;
        ScrollerAppearance.GradientChecked := ggVertical;
        ScrollerAppearance.GradientMirrorChecked := ggVertical;

        SelectionAppearance.Color := clRed;
        SelectionAppearance.ColorTo := $ABEBFF;
        SelectionAppearance.ColorMirror := $69D6FF;
        SelectionAppearance.ColorMirrorTo := $96E4FF;

        SelectionAppearance.ColorHot := $EFD3C6;
        SelectionAppearance.ColorHotTo := $EFD3C6;
        SelectionAppearance.ColorMirrorHot := $EFD3C6;
        SelectionAppearance.ColorMirrorHotTo := $EFD3C6;
        SelectionAppearance.BorderColorHot := $C66931;
        SelectionAppearance.GradientHot := ggVertical;
        SelectionAppearance.GradientMirrorHot := ggVertical;
        SelectionAppearance.TextColorHot := clBlack;

        SelectionAppearance.ColorDown := $EFD3C6;
        SelectionAppearance.ColorDownTo := $EFD3C6;
        SelectionAppearance.ColorMirrorDown := $EFD3C6;
        SelectionAppearance.ColorMirrorDownTo := $EFD3C6;
        SelectionAppearance.BorderColorDown := $C66931;
        SelectionAppearance.GradientDown := ggVertical;
        SelectionAppearance.GradientMirrorDown := ggVertical;
        SelectionAppearance.TextColorDown := clBlack;

        SelectionAppearance.ColorChecked := $EFD3C6;
        SelectionAppearance.ColorCheckedTo := $EFD3C6;
        SelectionAppearance.ColorMirrorChecked := $EFD3C6;
        SelectionAppearance.ColorMirrorCheckedTo := $EFD3C6;
        SelectionAppearance.BorderColorChecked := $C66931;
        SelectionAppearance.TextColorChecked := clBlack;
        SelectionAppearance.GradientChecked := ggVertical;
        SelectionAppearance.GradientMirrorChecked := ggVertical;
        SelectionAppearance.Rounded := False;

      ScrollerAppearance.ColorDisabled := $F2ECE9;
      ScrollerAppearance.ColorDisabledTo := $F2ECE9;
      ScrollerAppearance.ColorMirrorDisabled := $F2ECE9;
      ScrollerAppearance.ColorMirrorDisabledTo := clSilver;

      end;

  tsOffice2007Luna:
   begin
      Color := $F8E6D4;
      ColorTo := clNone;
      ColorDropDown := $FAFAFA;
      ColorDropDownTo := clNone;
      BorderColor := $EDD0B9;
      BorderDropDownColor := $868686;
      FocusColor := $FBF3EC;
      FocusColorTo := clNone;

      ResizerAppearance.BorderColor := $EEE7DD;
      ResizerAppearance.Color := clWhite;
      ResizerAppearance.ColorTo := $EEE7DD;

      IconBarAppearance.BorderColor := $C5C5C5;
      IconBarAppearance.Color := $EEEEE9;
      IconBarAppearance.ColorTo := clNone;

      CaptionAppearance.BorderColor := clWhite;
      CaptionAppearance.Color := $00F7D2B5;
      CaptionAppearance.ColorTo := clNone;
      CaptionAppearance.TextColor := $6E1500;

      CaptionItemAppearance.BorderColor := $C5C5C5;
      CaptionItemAppearance.Color := $EBEBEB;
      CaptionItemAppearance.ColorTo := clNone;
      CaptionItemAppearance.TextColor := $6E1500;

      ScrollerAppearance.Color := $00F9EFE8;
      ScrollerAppearance.ColorTo := $00F9EFE8;
      ScrollerAppearance.ColorMirror := $00F4DCCA;
      ScrollerAppearance.ColorMirrorTo := $00F4DCCA;
      ScrollerAppearance.BorderColor := $C0BCB2;
      ScrollerAppearance.Gradient := ggVertical;
      ScrollerAppearance.GradientMirror := ggVertical;

      ScrollerAppearance.ColorHot := $EBFDFF;
      ScrollerAppearance.ColorHotTo := $ACECFF;
      ScrollerAppearance.ColorMirrorHot := $59DAFF;
      ScrollerAppearance.ColorMirrorHotTo := $A4E9FF;
      ScrollerAppearance.BorderColorHot := $99CEDB;
      ScrollerAppearance.GradientHot := ggVertical;
      ScrollerAppearance.GradientMirrorHot := ggVertical;

      ScrollerAppearance.ColorDown := $76AFF1;
      ScrollerAppearance.ColorDownTo := $4190F3;
      ScrollerAppearance.ColorMirrorDown := $0E72F1;
      ScrollerAppearance.ColorMirrorDownTo := $4C9FFD;
      ScrollerAppearance.BorderColorDown := $45667B;
      ScrollerAppearance.GradientDown := ggVertical;
      ScrollerAppearance.GradientMirrorDown := ggVertical;

      ScrollerAppearance.ColorChecked := $B5DBFB;
      ScrollerAppearance.ColorCheckedTo := $78C7FE;
      ScrollerAppearance.ColorMirrorChecked := $9FEBFD;
      ScrollerAppearance.ColorMirrorCheckedTo := $56B4FE;
      ScrollerAppearance.BorderColorChecked := $45667B;
      ScrollerAppearance.GradientChecked := ggVertical;
      ScrollerAppearance.GradientMirrorChecked := ggVertical;

      ScrollerAppearance.ColorDisabled := $DFDED6;
      ScrollerAppearance.ColorDisabledTo := $E4E2DB;
      ScrollerAppearance.ColorMirrorDisabled := $D7D5CE;
      ScrollerAppearance.ColorMirrorDisabledTo := $E7E5E0;

    end;

  tsOffice2007Obsidian:
   begin
      Color := $E2E2DA;
      ColorTo := clNone;
      ColorDropDown := $FAFAFA;
      ColorDropDownTo := clNone;
      BorderColor := $ACACAC;
      BorderDropDownColor := $868686;
      FocusColor := $F7F7F7;
      FocusColorTo := clNone;

      ResizerAppearance.BorderColor := $E1E1E1;
      ResizerAppearance.Color := clWhite;
      ResizerAppearance.ColorTo := $E1E1E1;

      IconBarAppearance.BorderColor := $C5C5C5;
      IconBarAppearance.Color := $EFEFEF;
      IconBarAppearance.ColorTo := clNone;

      CaptionAppearance.BorderColor := clWhite;
      CaptionAppearance.Color := $E1E1E1;
      CaptionAppearance.ColorTo := clNone;
      CaptionAppearance.TextColor := $6E1500;

      CaptionItemAppearance.BorderColor := $C5C5C5;
      CaptionItemAppearance.Color := $EBEBEB;
      CaptionItemAppearance.ColorTo := clNone;
      CaptionItemAppearance.TextColor := $6E1500;

      ScrollerAppearance.Color := $DFDED6;
      ScrollerAppearance.ColorTo := $E4E2DB;
      ScrollerAppearance.ColorMirror := $D7D5CE;
      ScrollerAppearance.ColorMirrorTo := $E7E5E0;
      ScrollerAppearance.BorderColor := $C0BCB2;
      ScrollerAppearance.Gradient := ggVertical;
      ScrollerAppearance.GradientMirror := ggVertical;

      ScrollerAppearance.ColorHot := $EBFDFF;
      ScrollerAppearance.ColorHotTo := $ACECFF;
      ScrollerAppearance.ColorMirrorHot := $59DAFF;
      ScrollerAppearance.ColorMirrorHotTo := $A4E9FF;
      ScrollerAppearance.BorderColorHot := $99CEDB;
      ScrollerAppearance.GradientHot := ggVertical;
      ScrollerAppearance.GradientMirrorHot := ggVertical;

      ScrollerAppearance.ColorDown := $76AFF1;
      ScrollerAppearance.ColorDownTo := $4190F3;
      ScrollerAppearance.ColorMirrorDown := $0E72F1;
      ScrollerAppearance.ColorMirrorDownTo := $4C9FFD;
      ScrollerAppearance.BorderColorDown := $45667B;
      ScrollerAppearance.GradientDown := ggVertical;
      ScrollerAppearance.GradientMirrorDown := ggVertical;

      ScrollerAppearance.ColorChecked := $B5DBFB;
      ScrollerAppearance.ColorCheckedTo := $78C7FE;
      ScrollerAppearance.ColorMirrorChecked := $9FEBFD;
      ScrollerAppearance.ColorMirrorCheckedTo := $56B4FE;
      ScrollerAppearance.BorderColorChecked := $45667B;
      ScrollerAppearance.GradientChecked := ggVertical;
      ScrollerAppearance.GradientMirrorChecked := ggVertical;

      ScrollerAppearance.ColorDisabled := $F6F3F0;
      ScrollerAppearance.ColorDisabledTo := $F6F3F0;
      ScrollerAppearance.ColorMirrorDisabled := $EBE6E3;
      ScrollerAppearance.ColorMirrorDisabledTo := $EBE6E3;

    end;

    tsWindowsXP:
      begin
        Color := clBtnFace;
        ColorTo := clNone;
        ColorDropDown := clWhite;
        ColorDropDownTo := clNone;
        BorderColor := clActiveBorder;
        BorderDropDownColor := clInactiveBorder;
        FocusColor := clBtnHighLight;
        FocusColorTo := clNone;

        ResizerAppearance.BorderColor := clActiveBorder;
        ResizerAppearance.Color := clBtnFace;
        ResizerAppearance.ColorTo := clBtnFace;

        IconBarAppearance.BorderColor := clActiveBorder;
        IconBarAppearance.Color := clBtnFace;
        IconBarAppearance.ColorTo := clNone;

        CaptionAppearance.BorderColor := clActiveBorder;
        CaptionAppearance.Color := clActiveCaption;
        CaptionAppearance.ColorTo := clNone;
        CaptionAppearance.TextColor := clWhite;

        CaptionItemAppearance.BorderColor := clActiveBorder;
        CaptionItemAppearance.Color := clActiveCaption;
        CaptionItemAppearance.ColorTo := clNone;
        CaptionItemAppearance.TextColor := clWhite;

        ScrollerAppearance.TextColor := clBlack;
        ScrollerAppearance.TextColorChecked := clWhite;
        ScrollerAppearance.TextColorDisabled := clGray;
        ScrollerAppearance.TextColorDown := clWhite;
        ScrollerAppearance.TextColorHot := clWhite;

        ScrollerAppearance.Color := clWhite;
        ScrollerAppearance.ColorTo := $B9D8DC;
        ScrollerAppearance.ColorMirror := $B9D8DC;
        ScrollerAppearance.ColorMirrorTo := $B9D8DC;
        ScrollerAppearance.BorderColor := $B9D8DC;
        ScrollerAppearance.Gradient := ggVertical;
        ScrollerAppearance.GradientMirror := ggVertical;

        ScrollerAppearance.ColorHot := $EFD3C6;
        ScrollerAppearance.ColorHotTo := $EFD3C6;
        ScrollerAppearance.ColorMirrorHot := $EFD3C6;
        ScrollerAppearance.ColorMirrorHotTo := $EFD3C6;
        ScrollerAppearance.BorderColorHot := clHighlight;
        ScrollerAppearance.GradientHot := ggVertical;
        ScrollerAppearance.GradientMirrorHot := ggVertical;

        ScrollerAppearance.ColorDown := $B59284;
        ScrollerAppearance.ColorDownTo := $B59284;
        ScrollerAppearance.ColorMirrorDown := $B59284;
        ScrollerAppearance.ColorMirrorDownTo := $B59284;
        ScrollerAppearance.BorderColorDown := clHighlight;
        ScrollerAppearance.GradientDown := ggVertical;
        ScrollerAppearance.GradientMirrorDown := ggVertical;


        ScrollerAppearance.ColorChecked := $B9D8DC;
        ScrollerAppearance.ColorCheckedTo := $B9D8DC;
        ScrollerAppearance.ColorMirrorChecked := $B9D8DC;
        ScrollerAppearance.ColorMirrorCheckedTo := $B9D8DC;
        ScrollerAppearance.BorderColorChecked := clBlack;
        ScrollerAppearance.GradientChecked := ggVertical;
        ScrollerAppearance.GradientMirrorChecked := ggVertical;

        SelectionAppearance.Color := clRed;
        SelectionAppearance.ColorTo := $ABEBFF;
        SelectionAppearance.ColorMirror := $69D6FF;
        SelectionAppearance.ColorMirrorTo := $96E4FF;

        SelectionAppearance.ColorHot := clHighlight;
        SelectionAppearance.ColorHotTo := clHighlight;
        SelectionAppearance.ColorMirrorHot := clHighlight;
        SelectionAppearance.ColorMirrorHotTo := clHighlight;
        SelectionAppearance.BorderColorHot := clActiveBorder;
        SelectionAppearance.GradientHot := ggVertical;
        SelectionAppearance.GradientMirrorHot := ggVertical;
        SelectionAppearance.TextColorHot := clHighlightText;

        SelectionAppearance.ColorDown := clHighlight;
        SelectionAppearance.ColorDownTo := clHighlight;
        SelectionAppearance.ColorMirrorDown := clHighlight;
        SelectionAppearance.ColorMirrorDownTo := clHighlight;
        SelectionAppearance.BorderColorDown := clHighlight;
        SelectionAppearance.GradientDown := ggVertical;
        SelectionAppearance.GradientMirrorDown := ggVertical;
        SelectionAppearance.TextColorDown := clHighlightText;

        SelectionAppearance.ColorChecked := clHighlight;
        SelectionAppearance.ColorCheckedTo := clHighlight;
        SelectionAppearance.ColorMirrorChecked := clHighlight;
        SelectionAppearance.ColorMirrorCheckedTo := clHighlight;
        SelectionAppearance.BorderColorChecked := clHighlight;
        SelectionAppearance.TextColorChecked := clHighlightText;
        SelectionAppearance.GradientChecked := ggVertical;
        SelectionAppearance.GradientMirrorChecked := ggVertical;
        SelectionAppearance.Rounded := False;

        ScrollerAppearance.ColorDisabled := clSilver;
        ScrollerAppearance.ColorDisabledTo := clSilver;
        ScrollerAppearance.ColorMirrorDisabled := clSilver;
        ScrollerAppearance.ColorMirrorDisabledTo := clSilver;

      end;
    tsWhidbey:
      begin

        Color := $E7F2F3;
        ColorTo := clNone;
        ColorDropDown := $F9FCFC;
        ColorDropDownTo := clNone;
        BorderColor := $99A8AC;
        BorderDropDownColor := $7A868A;
        FocusColor := $E7F2F3;
        FocusColorTo := clNone;

        ResizerAppearance.BorderColor := $99A8AC;
        ResizerAppearance.Color := $FDFDFE;
        ResizerAppearance.ColorTo := $E2EFF1;

        IconBarAppearance.BorderColor := $99A8AC;
        IconBarAppearance.Color := $F8FBFC;
        IconBarAppearance.ColorTo := $B1C6C7;

        CaptionAppearance.BorderColor := $F9FCFC;
        CaptionAppearance.Color := $BAC7CC;
        CaptionAppearance.ColorTo := clNone;
        CaptionAppearance.TextColor := clBlack;

        CaptionItemAppearance.BorderColor := $F9FCFC;
        CaptionItemAppearance.Color := $D5E2E4;
        CaptionItemAppearance.ColorTo := clNone;
        CaptionItemAppearance.TextColor := clBlack;

        ScrollerAppearance.Color := clWhite;
        ScrollerAppearance.ColorTo := $DFEDF0;
        ScrollerAppearance.ColorMirror := $DFEDF0;
        ScrollerAppearance.ColorMirrorTo := $DFEDF0;
        ScrollerAppearance.BorderColor := $99A8AC;
        ScrollerAppearance.Gradient := ggVertical;
        ScrollerAppearance.GradientMirror := ggVertical;

      end;

      tsOffice2010Blue:
      begin

        Color := clWhite;
        ColorTo := clWhite;
        ColorDropDown := clWhite;
        ColorDropDownTo := clWhite;
        BorderColor := $C7B29F;
        BorderDropDownColor := $C7B29F;
        FocusColor := clWhite;
        FocusColorTo := clWhite;

        ResizerAppearance.BorderColor := $C7B29F;
        ResizerAppearance.Color := clWhite;
        ResizerAppearance.ColorTo := clWhite;

        IconBarAppearance.BorderColor := $C7B29F;
        IconBarAppearance.Color := clWhite;
        IconBarAppearance.ColorTo := clWhite;

        CaptionAppearance.BorderColor := $F6E9DC;
        CaptionAppearance.Color := $F6E9DC;
        CaptionAppearance.ColorTo := $F6E9DC;
        CaptionAppearance.TextColor := $5B391E;

        CaptionItemAppearance.BorderColor := $F6E9DC;
        CaptionItemAppearance.Color := $F6E9DC;
        CaptionItemAppearance.ColorTo := $F6E9DC;
        CaptionItemAppearance.TextColor := $5B391E;

        ScrollerAppearance.TextColor := clBlack;
        ScrollerAppearance.TextColorChecked := clBlack;
        ScrollerAppearance.TextColorDisabled := clBlack;
        ScrollerAppearance.TextColorDown := clBlack;
        ScrollerAppearance.TextColorHot := clBlack;

        ScrollerAppearance.Color := $F0DAC7;
        ScrollerAppearance.ColorTo := $F0DAC7;
        ScrollerAppearance.ColorMirror := $F0DAC7;
        ScrollerAppearance.ColorMirrorTo := $F0DAC7;
        ScrollerAppearance.BorderColor := $C7B29F;
        ScrollerAppearance.Gradient := ggVertical;
        ScrollerAppearance.GradientMirror := ggVertical;

        ScrollerAppearance.ColorHot := $BFF0FE;
        ScrollerAppearance.ColorHotTo := $8AE3FD;
        ScrollerAppearance.ColorMirrorHot := $8AE3FD;
        ScrollerAppearance.ColorMirrorHotTo := $D8F6FE;
        ScrollerAppearance.BorderColorHot := $60CDF2;
        ScrollerAppearance.GradientHot := ggVertical;
        ScrollerAppearance.GradientMirrorHot := ggVertical;

        ScrollerAppearance.ColorDown := $8DE1FE;
        ScrollerAppearance.ColorDownTo := $6BD8FE;
        ScrollerAppearance.ColorMirrorDown := $6BD8FE;
        ScrollerAppearance.ColorMirrorDownTo := $EBFAFF;
        ScrollerAppearance.BorderColorDown := $308AC2;
        ScrollerAppearance.GradientDown := ggVertical;
        ScrollerAppearance.GradientMirrorDown := ggVertical;

        ScrollerAppearance.ColorChecked := $84EAFD;
        ScrollerAppearance.ColorCheckedTo := $45E1FE;
        ScrollerAppearance.ColorMirrorChecked := $45E1FE;
        ScrollerAppearance.ColorMirrorCheckedTo := $AFF1FE;
        ScrollerAppearance.BorderColorChecked := $4FA0CE;
        ScrollerAppearance.TextColorChecked := clBlack;
        ScrollerAppearance.GradientChecked := ggVertical;
        ScrollerAppearance.GradientMirrorChecked := ggVertical;

        ScrollerAppearance.ColorDisabled := $00F2F2F2;
        ScrollerAppearance.ColorDisabledTo := $00F2F2F2;
        ScrollerAppearance.ColorMirrorDisabled := $00F2F2F2;
        ScrollerAppearance.ColorMirrorDisabledTo :=  $00F2F2F2;
        ScrollerAppearance.BorderColorDisabled := $D2CDC8;
        ScrollerAppearance.TextColorDisabled := clBlack;

        SelectionAppearance.Color := clWhite;
        SelectionAppearance.ColorTo := clWhite;
        SelectionAppearance.ColorMirror := clWhite;
        SelectionAppearance.ColorMirrorTo := clWhite;

        SelectionAppearance.ColorHot := $BFF0FE;
        SelectionAppearance.ColorHotTo := $8AE3FD;
        SelectionAppearance.ColorMirrorHot := $8AE3FD;
        SelectionAppearance.ColorMirrorHotTo := $D8F6FE;
        SelectionAppearance.BorderColorHot := $60CDF2;
        SelectionAppearance.GradientHot := ggVertical;
        SelectionAppearance.GradientMirrorHot := ggVertical;
        SelectionAppearance.TextColorHot := clBlack;

        SelectionAppearance.ColorDown := $8DE1FE;
        SelectionAppearance.ColorDownTo := $6BD8FE;
        SelectionAppearance.ColorMirrorDown := $6BD8FE;
        SelectionAppearance.ColorMirrorDownTo := $EBFAFF;
        SelectionAppearance.BorderColorDown := $308AC2;
        SelectionAppearance.GradientDown := ggVertical;
        SelectionAppearance.GradientMirrorDown := ggVertical;
        SelectionAppearance.TextColorDown := clBlack;

        SelectionAppearance.ColorChecked := $84EAFD;
        SelectionAppearance.ColorCheckedTo := $45E1FE;
        SelectionAppearance.ColorMirrorChecked := $45E1FE;
        SelectionAppearance.ColorMirrorCheckedTo := $AFF1FE;
        SelectionAppearance.BorderColorChecked := $4FA0CE;
        SelectionAppearance.TextColorChecked := clBlack;
        SelectionAppearance.GradientChecked := ggVertical;
        SelectionAppearance.GradientMirrorChecked := ggVertical;
        SelectionAppearance.Rounded := True;

        SelectionAppearance.ColorDisabled := $00F2F2F2;
        SelectionAppearance.ColorDisabledTo := $00F2F2F2;
        SelectionAppearance.ColorMirrorDisabled := $00F2F2F2;
        SelectionAppearance.ColorMirrorDisabledTo :=  $00F2F2F2;
        SelectionAppearance.BorderColorDisabled := $D2CDC8;
        SelectionAppearance.TextColorDisabled := clBlack;

      end;

      tsOffice2010Silver:
      begin

        Color := clWhite;
        ColorTo := clNone;
        ColorDropDown := clWhite;
        ColorDropDownTo := clNone;
        BorderColor := $D2CDC8;
        BorderDropDownColor := $D2CDC8;
        FocusColor := clWhite;
        FocusColorTo := clWhite;

        ResizerAppearance.BorderColor := $D2CDC8;
        ResizerAppearance.Color := clWhite;
        ResizerAppearance.ColorTo := clWhite;

        IconBarAppearance.BorderColor := $D2CDC8;
        IconBarAppearance.Color := clWhite;
        IconBarAppearance.ColorTo := clWhite;

        CaptionAppearance.BorderColor := $F2EFED;
        CaptionAppearance.Color := $F2EFED;
        CaptionAppearance.ColorTo := $F2EFED;
        CaptionAppearance.TextColor := $675F58;

        CaptionItemAppearance.BorderColor := $F2EFED;
        CaptionItemAppearance.Color := $F2EFED;
        CaptionItemAppearance.ColorTo := $F2EFED;
        CaptionItemAppearance.TextColor := $675F58;

        ScrollerAppearance.TextColor := clBlack;
        ScrollerAppearance.TextColorChecked := clBlack;
        ScrollerAppearance.TextColorDisabled := clBlack;
        ScrollerAppearance.TextColorDown := clBlack;
        ScrollerAppearance.TextColorHot := clBlack;

        ScrollerAppearance.Color :=$EDE5E0;
        ScrollerAppearance.ColorTo := $EDE5E0;
        ScrollerAppearance.ColorMirror := $EDE5E0;
        ScrollerAppearance.ColorMirrorTo := $EDE5E0;
        ScrollerAppearance.BorderColor := $D2CDC8;
        ScrollerAppearance.Gradient := ggVertical;
        ScrollerAppearance.GradientMirror := ggVertical;

        ScrollerAppearance.ColorHot := $BFF0FE;
        ScrollerAppearance.ColorHotTo := $8AE3FD;
        ScrollerAppearance.ColorMirrorHot := $8AE3FD;
        ScrollerAppearance.ColorMirrorHotTo := $D8F6FE;
        ScrollerAppearance.BorderColorHot := $60CDF2;
        ScrollerAppearance.GradientHot := ggVertical;
        ScrollerAppearance.GradientMirrorHot := ggVertical;

        ScrollerAppearance.ColorDown := $8DE1FE;
        ScrollerAppearance.ColorDownTo := $6BD8FE;
        ScrollerAppearance.ColorMirrorDown := $6BD8FE;
        ScrollerAppearance.ColorMirrorDownTo := $EBFAFF;
        ScrollerAppearance.BorderColorDown := $308AC2;
        ScrollerAppearance.GradientDown := ggVertical;
        ScrollerAppearance.GradientMirrorDown := ggVertical;

        ScrollerAppearance.ColorChecked := $84EAFD;
        ScrollerAppearance.ColorCheckedTo := $45E1FE;
        ScrollerAppearance.ColorMirrorChecked := $45E1FE;
        ScrollerAppearance.ColorMirrorCheckedTo := $AFF1FE;
        ScrollerAppearance.BorderColorChecked := $4FA0CE;
        ScrollerAppearance.TextColorChecked := clBlack;
        ScrollerAppearance.GradientChecked := ggVertical;
        ScrollerAppearance.GradientMirrorChecked := ggVertical;

        ScrollerAppearance.ColorDisabled := $00F2F2F2;
        ScrollerAppearance.ColorDisabledTo := $00F2F2F2;
        ScrollerAppearance.ColorMirrorDisabled := $00F2F2F2;
        ScrollerAppearance.ColorMirrorDisabledTo :=  $00F2F2F2;
        ScrollerAppearance.BorderColorDisabled := $D2CDC8;
        ScrollerAppearance.TextColorDisabled := clBlack;

        SelectionAppearance.Color := clWhite;
        SelectionAppearance.ColorTo := clWhite;
        SelectionAppearance.ColorMirror := clWhite;
        SelectionAppearance.ColorMirrorTo := clWhite;

        SelectionAppearance.ColorHot := $BFF0FE;
        SelectionAppearance.ColorHotTo := $8AE3FD;
        SelectionAppearance.ColorMirrorHot := $8AE3FD;
        SelectionAppearance.ColorMirrorHotTo := $D8F6FE;
        SelectionAppearance.BorderColorHot := $60CDF2;
        SelectionAppearance.GradientHot := ggVertical;
        SelectionAppearance.GradientMirrorHot := ggVertical;
        SelectionAppearance.TextColorHot := clBlack;

        SelectionAppearance.ColorDown := $8DE1FE;
        SelectionAppearance.ColorDownTo := $6BD8FE;
        SelectionAppearance.ColorMirrorDown := $6BD8FE;
        SelectionAppearance.ColorMirrorDownTo := $EBFAFF;
        SelectionAppearance.BorderColorDown := $308AC2;
        SelectionAppearance.GradientDown := ggVertical;
        SelectionAppearance.GradientMirrorDown := ggVertical;
        SelectionAppearance.TextColorDown := clBlack;

        SelectionAppearance.ColorChecked := $84EAFD;
        SelectionAppearance.ColorCheckedTo := $45E1FE;
        SelectionAppearance.ColorMirrorChecked := $45E1FE;
        SelectionAppearance.ColorMirrorCheckedTo := $AFF1FE;
        SelectionAppearance.BorderColorChecked := $4FA0CE;
        SelectionAppearance.TextColorChecked := clBlack;
        SelectionAppearance.GradientChecked := ggVertical;
        SelectionAppearance.GradientMirrorChecked := ggVertical;
        SelectionAppearance.Rounded := True;

        SelectionAppearance.ColorDisabled := $00F2F2F2;
        SelectionAppearance.ColorDisabledTo := $00F2F2F2;
        SelectionAppearance.ColorMirrorDisabled := $00F2F2F2;
        SelectionAppearance.ColorMirrorDisabledTo :=  $00F2F2F2;
        SelectionAppearance.BorderColorDisabled := $D2CDC8;
        SelectionAppearance.TextColorDisabled := clBlack;

      end;

      tsOffice2010Black:
      begin

        Color := clWhite;
        ColorTo := clWhite;
        ColorDropDown := clWhite;
        ColorDropDownTo := clWhite;
        BorderColor := $6D6D6D;
        BorderDropDownColor := $6D6D6D;
        FocusColor := clWhite;
        FocusColorTo := clWhite;

        ResizerAppearance.BorderColor := $6D6D6D;;
        ResizerAppearance.Color := clWhite;
        ResizerAppearance.ColorTo := clWhite;

        IconBarAppearance.BorderColor := $6D6D6D;;
        IconBarAppearance.Color := clBtnFace;
        IconBarAppearance.ColorTo := clNone;

        CaptionAppearance.BorderColor := $A7A7A7;
        CaptionAppearance.Color := $A7A7A7;
        CaptionAppearance.ColorTo := $A7A7A7;
        CaptionAppearance.TextColor := $242424;

        CaptionItemAppearance.BorderColor := $A7A7A7;
        CaptionItemAppearance.Color := $A7A7A7;
        CaptionItemAppearance.ColorTo := $A7A7A7;
        CaptionItemAppearance.TextColor := $242424;

        ScrollerAppearance.TextColor := clBlack;
        ScrollerAppearance.TextColorChecked := clBlack;
        ScrollerAppearance.TextColorDisabled := clBlack;
        ScrollerAppearance.TextColorDown := clBlack;
        ScrollerAppearance.TextColorHot := clBlack;

        ScrollerAppearance.Color := $919191;
        ScrollerAppearance.ColorTo := $919191;
        ScrollerAppearance.ColorMirror := $919191;
        ScrollerAppearance.ColorMirrorTo := $919191;
        ScrollerAppearance.BorderColor := $6D6D6D;
        ScrollerAppearance.Gradient := ggVertical;
        ScrollerAppearance.GradientMirror := ggVertical;

       	ScrollerAppearance.ColorHot := $BFF0FE;
        ScrollerAppearance.ColorHotTo := $8AE3FD;
        ScrollerAppearance.ColorMirrorHot := $8AE3FD;
        ScrollerAppearance.ColorMirrorHotTo := $D8F6FE;
        ScrollerAppearance.BorderColorHot := $60CDF2;
        ScrollerAppearance.GradientHot := ggVertical;
        ScrollerAppearance.GradientMirrorHot := ggVertical;

        ScrollerAppearance.ColorDown := $8DE1FE;
        ScrollerAppearance.ColorDownTo := $6BD8FE;
        ScrollerAppearance.ColorMirrorDown := $6BD8FE;
        ScrollerAppearance.ColorMirrorDownTo := $EBFAFF;
        ScrollerAppearance.BorderColorDown := $308AC2;
        ScrollerAppearance.GradientDown := ggVertical;
        ScrollerAppearance.GradientMirrorDown := ggVertical;

        ScrollerAppearance.ColorChecked := $84EAFD;
        ScrollerAppearance.ColorCheckedTo := $45E1FE;
        ScrollerAppearance.ColorMirrorChecked := $45E1FE;
        ScrollerAppearance.ColorMirrorCheckedTo := $AFF1FE;
        ScrollerAppearance.BorderColorChecked := $4FA0CE;
        ScrollerAppearance.TextColorChecked := clBlack;
        ScrollerAppearance.GradientChecked := ggVertical;
        ScrollerAppearance.GradientMirrorChecked := ggVertical;


        ScrollerAppearance.ColorDisabled := $00F2F2F2;
        ScrollerAppearance.ColorDisabledTo := $00F2F2F2;
        ScrollerAppearance.ColorMirrorDisabled := $00F2F2F2;
        ScrollerAppearance.ColorMirrorDisabledTo :=  $00F2F2F2;
        ScrollerAppearance.BorderColorDisabled := $D2CDC8;
        ScrollerAppearance.TextColorDisabled := clBlack;

        SelectionAppearance.Color := clWhite;
        SelectionAppearance.ColorTo := clWhite;
        SelectionAppearance.ColorMirror := clWhite;
        SelectionAppearance.ColorMirrorTo := clWhite;

        SelectionAppearance.ColorHot := $BFF0FE;
        SelectionAppearance.ColorHotTo := $8AE3FD;
        SelectionAppearance.ColorMirrorHot := $8AE3FD;
        SelectionAppearance.ColorMirrorHotTo := $D8F6FE;
        SelectionAppearance.BorderColorHot := $60CDF2;
        SelectionAppearance.GradientHot := ggVertical;
        SelectionAppearance.GradientMirrorHot := ggVertical;
        SelectionAppearance.TextColorHot := clBlack;

        SelectionAppearance.ColorDown := $8DE1FE;
        SelectionAppearance.ColorDownTo := $6BD8FE;
        SelectionAppearance.ColorMirrorDown := $6BD8FE;
        SelectionAppearance.ColorMirrorDownTo := $EBFAFF;
        SelectionAppearance.BorderColorDown := $308AC2;
        SelectionAppearance.GradientDown := ggVertical;
        SelectionAppearance.GradientMirrorDown := ggVertical;
        SelectionAppearance.TextColorDown := clBlack;

        SelectionAppearance.ColorChecked := $84EAFD;
        SelectionAppearance.ColorCheckedTo := $45E1FE;
        SelectionAppearance.ColorMirrorChecked := $45E1FE;
        SelectionAppearance.ColorMirrorCheckedTo := $AFF1FE;
        SelectionAppearance.BorderColorChecked := $4FA0CE;
        SelectionAppearance.TextColorChecked := clBlack;
        SelectionAppearance.GradientChecked := ggVertical;
        SelectionAppearance.GradientMirrorChecked := ggVertical;
        SelectionAppearance.Rounded := True;

        SelectionAppearance.ColorDisabled := $00F2F2F2;
        SelectionAppearance.ColorDisabledTo := $00F2F2F2;
        SelectionAppearance.ColorMirrorDisabled := $00F2F2F2;
        SelectionAppearance.ColorMirrorDisabledTo :=  $00F2F2F2;
        SelectionAppearance.BorderColorDisabled := $D2CDC8;
        SelectionAppearance.TextColorDisabled := clBlack;
      end;



    tsCustom:
      begin
      end;

  end;

end;

//------------------------------------------------------------------------------

function TAdvCustomOfficeScrollSelector.AutoSizeButtonCount: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i:= 0 to Tools.Count-1 do
  begin
    if (Tools.Items[i].ItemType = itAutoSizeButton) then
      Result := Result + 1;
  end;
end;

//------------------------------------------------------------------------------

function TAdvCustomOfficeScrollSelector.CaptionItemCount: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i:= 0 to Tools.Count-1 do
  begin
    if (Tools.Items[i].ItemType = itCaption) then
      Result := Result + 1;
  end;
end;

//------------------------------------------------------------------------------

function TAdvCustomOfficeScrollSelector.FullWidthButtonCount: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i:= 0 to Tools.Count-1 do
  begin
    if (Tools.Items[i].ItemType = itFullWidthButton) then
      Result := Result + 1;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeScrollSelector.SetResizerAppearance(
  const Value: TGradientBackground);
begin
  FResizerAppearance.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeScrollSelector.SetButtonSize(
  const Value: TSelectorButtonSize);
begin
  FButtonSize.Assign(Value);
  FScrollSelectorPanel.SetItemsPosition;
  FScrollSelectorPanel.Invalidate;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeScrollSelector.BeginUpdate;
begin

end;

procedure TAdvCustomOfficeScrollSelector.ButtonSizeChanged(
  Sender: TObject);
begin
  if Assigned(FScrollSelectorPanel) then
  begin
    FScrollSelectorPanel.SetItemsPosition;
    FScrollSelectorPanel.Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeScrollSelector.SetIconBarAppearance(
  const Value: TGradientBackground);
begin
  FIconBarAppearance.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeScrollSelector.OnAppearanceChanged(
  Sender: TObject);
begin
  FScrollSelectorPanel.SelectionAppearance.Assign(SelectionAppearance);
  FScrollSelectorPanel.Invalidate;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeScrollSelector.CMFocusChanged(
  var Message: TCMFocusChanged);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeScrollSelector.WMSetFocus(
  var Message: TWMSetFocus);
begin
  if Assigned(FScrollSelectorPanel) then
  begin
    FScrollSelectorPanel.Color := FocusColor;
    FScrollSelectorPanel.ColorTo := FocusColorTo;
    FScrollSelectorPanel.Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeScrollSelector.WMKillFocus(
  var Message: TWMSetFocus);
begin
  inherited;
  if Assigned(FScrollSelectorPanel) then
  begin
    FScrollSelectorPanel.Color := Color;
    FScrollSelectorPanel.ColorTo := ColorTo;
    FScrollSelectorPanel.Invalidate;
  end;
end;

procedure TAdvCustomOfficeScrollSelector.WMPaint(var Message: TWMPaint);
var
  DC, MemDC: HDC;
  MemBitmap, OldBitmap: HBITMAP;
  PS: TPaintStruct;
begin
  if not FDoubleBuffered or (Message.DC <> 0) then
  begin
    if not (csCustomPaint in ControlState) and (ControlCount = 0) then
      inherited
    else
      PaintHandler(Message);
  end
  else
  begin
    DC := GetDC(0);
    MemBitmap := CreateCompatibleBitmap(DC, ClientRect.Right, ClientRect.Bottom);
    ReleaseDC(0, DC);
    MemDC := CreateCompatibleDC(0);
    OldBitmap := SelectObject(MemDC, MemBitmap);
    try
      DC := BeginPaint(Handle, PS);
      Perform(WM_ERASEBKGND, MemDC, MemDC);
      Message.DC := MemDC;
      WMPaint(Message);
      Message.DC := 0;
      BitBlt(DC, 0, 0, ClientRect.Right, ClientRect.Bottom, MemDC, 0, 0, SRCCOPY);
      EndPaint(Handle, PS);
    finally
      SelectObject(MemDC, OldBitmap);
      DeleteDC(MemDC);
      DeleteObject(MemBitmap);
    end;
  end;
end;

//------------------------------------------------------------------------------

function TAdvCustomOfficeScrollSelector.GetColor: TColor;
begin
  Result := inherited Color;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeScrollSelector.SetColor(const Value: TColor);
begin
  inherited Color := Value;
  if Assigned(FScrollSelectorPanel) then
  begin
    FScrollSelectorPanel.Color := Color;
    FScrollSelectorPanel.Invalidate;
  end;  
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeScrollSelector.SetIntegralRows(
  const Value: Boolean);
begin
  if (FIntegralRows <> Value) then
  begin
    FIntegralRows := Value;
    if Assigned(FScrollSelectorPanel) then
      FScrollSelectorPanel.IntegralRows := FIntegralRows;
  end;
end;

//------------------------------------------------------------------------------

{ TAdvScrollSelectorItem }

constructor TAdvScrollSelectorItem.Create(Collection: TCollection);
begin
  inherited;
  FRow := -1;
  FRow1 := -1;
  FRow2 := -1;
  FItemRect1 := Rect(-1, -1, -1, -1);
  FItemRect2 := Rect(-1, -1, -1, -1);
  FIPicture := TGDIPPicture.Create;
  FMenuItem := false;
  FIPicture.OnChange := OnPictureChanged;
  TAdvScrollSelectorItems(Collection).Changed;
end;

//------------------------------------------------------------------------------

destructor TAdvScrollSelectorItem.Destroy;
begin
  FIPicture.Free;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvScrollSelectorItem.Assign(Source: TPersistent);
begin
  inherited;
  if (Source is TAdvScrollSelectorItem) then
    Picture.Assign(TAdvScrollSelectorItem(Source).Picture);
end;

//------------------------------------------------------------------------------

procedure TAdvScrollSelectorItem.OnPictureChanged(Sender: TObject);
begin
//
end;

//------------------------------------------------------------------------------

procedure TAdvScrollSelectorItem.SetPicture(const Value: TGDIPPicture);
begin
  FIPicture.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TAdvScrollSelectorItem.SetIndex(Value: Integer);
var
  NewValue: Boolean;
begin
  NewValue := Index <> Value;
  inherited;
  if NewValue then
    TAdvScrollSelectorItems(Collection).Changed;
end;

//------------------------------------------------------------------------------

procedure TAdvScrollSelectorItem.Changed;
begin
  inherited;
  TAdvScrollSelectorItems(Collection).Changed;
end;

//------------------------------------------------------------------------------

procedure TAdvScrollSelectorItem.SetCaption(const Value: string);
begin
  inherited;
  Changed;
end;

//------------------------------------------------------------------------------

{ TAdvScrollSelectorItems }

function TAdvScrollSelectorItems.Add: TAdvScrollSelectorItem;
begin
  Result := TAdvScrollSelectorItem(inherited Add);
end;

//------------------------------------------------------------------------------

procedure TAdvScrollSelectorItems.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

//------------------------------------------------------------------------------

constructor TAdvScrollSelectorItems.Create(AOwner: TPersistent);
begin
  inherited Create(TAdvScrollSelectorItem);
  FOwner := AOwner;
end;

//------------------------------------------------------------------------------

function TAdvScrollSelectorItems.GetItem(
  Index: Integer): TAdvScrollSelectorItem;
begin
  Result := TAdvScrollSelectorItem(inherited Items[Index]);
end;

function TAdvScrollSelectorItems.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

//------------------------------------------------------------------------------

function TAdvScrollSelectorItems.Insert(
  Index: Integer): TAdvScrollSelectorItem;
begin
  Result := TAdvScrollSelectorItem(inherited Insert(Index));
end;

//------------------------------------------------------------------------------

procedure TAdvScrollSelectorItems.SetItem(Index: Integer;
  const Value: TAdvScrollSelectorItem);
begin
  inherited Items[Index] := Value;
end;

//------------------------------------------------------------------------------

{ TAdvCustomScrollSelectorPanel }

constructor TAdvCustomScrollSelectorPanel.Create(AOwner: TComponent);
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

destructor TAdvCustomScrollSelectorPanel.Destroy;
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomScrollSelectorPanel.CMMouseLeave(
  var Message: TMessage);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomScrollSelectorPanel.KeyDown(var Key: Word;
  Shift: TShiftState);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomScrollSelectorPanel.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomScrollSelectorPanel.MouseMove(Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomScrollSelectorPanel.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomScrollSelectorPanel.Paint;
var
  Clr, ClrTo: TColor;
  R: TRect;
begin
  //inherited;
  R := Rect(0, 0, Width, Height);

  Clr := Color;
  ClrTo := ColorTo;

  if ClrTo <> clNone then
    DrawGradient(Canvas, Clr, ClrTo, 40, R, FGradientDirection = gdHorizontal)
  else
  begin
    Canvas.Brush.Color := Clr;
    Canvas.Pen.Color := Clr;
    Canvas.Rectangle(R);
  end;

  if ShowBorder then
  begin
    Canvas.Brush.Style := bsClear;
    Canvas.Pen.Color := WindowBorderColor;
    Canvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomScrollSelectorPanel.SetColorTo(const Value: TColor);
begin
  FColorTo := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomScrollSelectorPanel.SetTGradientDirection(
  const Value: TGradientDirection);
begin
  FGradientDirection := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomScrollSelectorPanel.SetWindowBorderColor(
  const Value: TColor);
begin
  FWindowBorderColor := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomScrollSelectorPanel.AdjustClientRect(var Rect: TRect);
begin
  if ShowBorder then
    Rect := Classes.Rect(0, 0, Rect.Right - BorderWidth, Rect.Bottom - BorderWidth);
  inherited AdjustClientRect(Rect);
end;

//------------------------------------------------------------------------------

{ TAdvScrollSelectorPanel }

constructor TAdvScrollSelectorPanel.Create(AOwner: TComponent);
begin
  inherited;
  FHotItemIndex := -1;
  FDownItemIndex := -1;
  FNoPrefix := false;

  ControlStyle := ControlStyle - [csSetCaption];
  FItemIndex := -1;

  FButtonHeight := 20;
  FTopOffSet := 4;
  FLeftOffSet := 4;
  FButtonMargin := 3;

  ShowHint := true;
  FMaxCaptionLength := 0;

  FMinButtonWidth := MINBUTTONSIZE;
  FMinButtonHeight := MINBUTTONSIZE;

  FSelectionAppearance := TSelectionAppearance.Create;

  FCaptionAppearance := TSimpleGradientCaption.Create;

  FTwoColorImages := False;

  FShowAutoSizeButton := True;
  FShowCaptionItem := True;
  FShowFullWidthItem := True;

  FTopRow := 1;
  FVisibleRowCount := 1;
  FRowCount := 1;
  FRowHeight := FMinButtonHeight;

  FIntegralRows := True;

  FBKGCache := nil;
  FNeedRePaint := True;
  //DoubleBuffered := True;
end;

//------------------------------------------------------------------------------

destructor TAdvScrollSelectorPanel.Destroy;
begin
  FSelectionAppearance.Free;
  FCaptionAppearance.Free;
  if Assigned(FBKGCache) then
    FBKGCache.Free;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvScrollSelectorPanel.MouseMove(Shift: TShiftState; X, Y: Integer);
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
        DrawItemEx(j, true);
      end;

      if (Items.Items[i].Enable) and (Items.Items[i].ItemType <> itCaption) then
      begin
        FHotItemIndex := i;
        if FMouseDown then // means mouse down move
          FDownItemIndex := i;
        DrawItemEx(i, true);
      end;

      //if FItems.Items[i].Hint <> '' then
      begin
        Hint := Items.Items[i].Hint;
        Application.CancelHint;
      end;

      if Assigned(FOnHotTool) and Items.Items[i].Enable and (Items.Items[i].ItemType <> itCaption) then
        FOnHotTool(self, i);
    end;
  end
  else if (FHotItemIndex >= 0) then
  begin
    j := FHotItemIndex;
    FHotItemIndex := -1;
    if FDownItemIndex > -1 then // means mouse down move
      FDownItemIndex := -1;
    DrawItemEx(j, true);
  end;

  if Assigned(AdvOfficeScrollSelector) then
    if Assigned(AdvOfficeScrollSelector.OnMouseMove) then
      AdvOfficeScrollSelector.OnMouseMove(AdvOfficeScrollSelector, Shift, X, Y);
end;

//------------------------------------------------------------------------------

procedure TAdvScrollSelectorPanel.DrawItem(Index: integer; RefreshItem: boolean = false; Graph: TGPGraphics = nil; Canvas: TCanvas = nil);
var
  Gr, R: TRect;
  DTSTYLE: dword;
  bmp: TBitMap;
  DR, R2: TRect;
  i, ItemRow: Integer;
begin
  if not Assigned(AdvOfficeScrollSelector) then
    Exit;

  if (Canvas = nil) then
    Canvas := Self.Canvas;

  if ((Items.Items[Index].ItemType = itCaption) and (not ShowCaptionItem)) or
     ((Items.Items[Index].ItemType = itFullWidthButton) and (not ShowFullWidthItem)) or
     ((Items.Items[Index].ItemType = itAutoSizeButton) and (not ShowAutoSizeButton)) then
    Exit; 

  DTSTYLE := DT_SINGLELINE or DT_VCENTER;

  if FNoPrefix then
    DTSTYLE := DTSTYLE or DT_NOPREFIX;  

  R := GetItemRect(Items.Items[Index]); //Items.Items[Index].ItemRect;
  ItemRow := GetItemRow(Items.Items[Index]);
  if ((R.Top < 0) and (R.Bottom < 0)) or (not AutoHeight and ((ItemRow < FTopRow) or (ItemRow > FTopRow + FVisibleRowCount))) then
    Exit;

  case (Items.Items[Index].CaptionAlignment) of
    taLeftJustify: DTSTYLE := DTSTYLE or DT_LEFT;
    taRightJustify: DTSTYLE := DTSTYLE or DT_RIGHT;
    taCenter: DTSTYLE := DTSTYLE or DT_RIGHT;
  end;

  Gr := R;  //Items.Items[Index].ItemRect;
  Gr.Left := Gr.Left + ButtonMargin;
  Gr.Top := Gr.Top; // + ButtonMargin;

  if (Items.Items[Index].ItemType = itCaption) then
  begin
    if not Assigned(FOnDrawItem) then
    begin
      if Items.Items[Index].backGroundColor <> clNone then
      begin
        Canvas.Pen.Color := Items.Items[Index].backGroundColor;
        Canvas.Brush.Color := Items.Items[Index].backGroundColor;
        Canvas.Rectangle(R.Left + 3, R.Top + 3, R.Right - 3, R.Bottom - 3);
      end;

      if not (not (Assigned(FImages) and (Items.Items[Index].ImageIndex >= 0)) and (Items.Items[Index].Caption = '')) then
      begin
        if (CaptionAppearance.Color <> clNone) or (CaptionAppearance.ColorTo <> clNone) then
        begin
          if (CaptionAppearance.Color <> clNone) and (CaptionAppearance.ColorTo <> clNone) then
            DrawGradient(Canvas, CaptionAppearance.Color, CaptionAppearance.ColorTo, 80, R, CaptionAppearance.Direction = gdHorizontal)
          else
          begin
            Canvas.Pen.Color := CaptionAppearance.Color;
            Canvas.Brush.Color := CaptionAppearance.Color;
            Canvas.Rectangle(R);
          end;
        end;

        Canvas.Pen.Color := clSilver;
        Canvas.MoveTo(R.Left, R.Bottom-1);
        Canvas.LineTo(R.Right, R.Bottom-1);

        if CaptionAppearance.BorderColor <> clNone then
        begin
          Canvas.Pen.Color := CaptionAppearance.BorderColor;
          Canvas.Brush.Style := bsClear;
          Canvas.Rectangle(R);
        end;
      end;
    end;

    if not Assigned(FOnDrawItem) then
    begin
      if not Items.Items[Index].Picture.Empty then
      begin
        Items.Items[Index].Picture.GetImageSizes;
        {if (Items.Items[Index].CaptionAlignment = taLeftJustify) then
          Canvas.Draw(fmaxCaptionLength + Gr.left + 2, Gr.Top + 2, Items.Items[Index].Picture)
        else}
          Canvas.Draw(Gr.left, Gr.Top + 2, Items.Items[Index].Picture);

        gr.Left := gr.left + Items.Items[Index].Picture.Width + 2;
      end
      else if Assigned(FImages) and (Items.Items[Index].ImageIndex >= 0) then
      begin
        if TwoColorImages then
        begin
          bmp := TBitmap.Create;
          try
            bmp.Width := FImages.width;
            bmp.Height := FImages.Height;

            bmp.Canvas.Brush.Color := clFuchsia;
            bmp.Canvas.FillRect(rect(0, 0, FImages.Width, FIMages.Height));

            FImages.DrawingStyle := dsTransparent;
            FImages.Draw(bmp.Canvas, 0, 0, Items.Items[Index].ImageIndex);

            bmp.Transparent := true;
            bmp.TransparentMode := tmAuto;

            {if (Items.Items[Index].CaptionAlignment = taLeftJustify) then
              Canvas.Draw(fmaxCaptionLength + Gr.left + 2, Gr.Top + 2, bmp)
            else}
              Canvas.Draw(Gr.left, Gr.Top + 2, bmp);
          finally
            bmp.Free;
          end;
        end
        else
        begin
          {if (Items.Items[Index].CaptionAlignment = taLeftJustify) then
            FImages.Draw(Canvas, fmaxCaptionLength + Gr.left + 2, Gr.Top + 2, Items.Items[Index].ImageIndex)
          else}
            FImages.Draw(Canvas, Gr.left, Gr.Top + 2, Items.Items[Index].ImageIndex);
        end;
        Gr.Left := Gr.Left + FImages.Width + 2;
      end;

      if IsShowingIconBar and (Items.Items[Index].CaptionAlignment = taLeftJustify) then
      begin
        Gr.Left := FAdvOfficeScrollSelector.IconBarWidth + 4;
      end;
    end;

    Canvas.Brush.Style := bsClear;
    Canvas.Font.Color := CaptionAppearance.TextColor;

    if not Assigned(FOnDrawItem) and (Items.Items[Index].CaptionAlignment = taLeftJustify) and (Items.Items[Index].Caption <> '') then
    begin
      DrawText(Canvas.Handle, PChar(Items.Items[Index].Caption), -1, Gr, DTSTYLE);
    end;

    if not Assigned(FOnDrawItem) then
    begin
      if (Items.Items[Index].CaptionAlignment = taCenter) and (Items.Items[Index].Caption <> '') then
        DrawText(Canvas.Handle, PChar(Items.Items[Index].Caption), -1, Gr, DTSTYLE or DT_CENTER)
      else if (Items.Items[Index].CaptionAlignment = taRightJustify) and (Items.Items[Index].Caption <> '') then
        DrawText(Canvas.Handle, PChar(Items.Items[Index].Caption), -1, Gr, DTSTYLE);
    end;

    if not Assigned(FOnDrawItem) and not (Assigned(FImages) and (Items.Items[Index].ImageIndex >= 0)) and (Items.Items[Index].Caption = '') then
    begin
      R2 := R; //Items.Items[Index].ItemRect;
      i := R2.Top + (R2.Bottom - R2.Top) div 2;
      Canvas.Pen.Color := WindowBorderColor;
      Canvas.MoveTo(R2.Left, i);
      Canvas.LineTo(R2.Right, i);
    end;

    if Assigned(FOnDrawItem) then
      FOnDrawItem(self, Index, R);
  end
  else if not Items.Items[Index].Enable then
  begin
    if (SelectionAppearance.ColorDisabledTo <> clNone) then
    begin
      DrawVistaGradient(Canvas, R, SelectionAppearance.ColorDisabled, SelectionAppearance.ColorDisabledTo, SelectionAppearance.ColorMirrorDisabled, SelectionAppearance.ColorMirrorDisabledTo, SelectionAppearance.BorderColorDisabled,
          SelectionAppearance.GradientDisabled, SelectionAppearance.GradientMirrorDisabled, '', Canvas.Font, Enabled, False, aaClearType, SelectionAppearance.Rounded, False, tpTop, Graph);
    end
    else if (self.SelectionAppearance.ColorDisabled <> clNone) then
    begin
      Canvas.Brush.Color := SelectionAppearance.ColorDisabled;
      Canvas.Pen.Color := SelectionAppearance.ColorDisabled;
      DR := R;
      Canvas.Rectangle(DR.Left, DR.Top, DR.Right, DR.Bottom);
    end;

    if not Assigned(FOnDrawItem) then
    begin
      if not Items.Items[Index].Picture.Empty then
      begin
        Items.Items[Index].Picture.GetImageSizes;
        {if (Items.Items[Index].CaptionAlignment = taLeftJustify) then
          Canvas.Draw(fmaxCaptionLength + Gr.left + 2, Gr.Top + 2, Items.Items[Index].Picture)
        else}
          Canvas.Draw(Gr.left, Gr.Top + 2, Items.Items[Index].Picture);

        gr.Left := gr.left + Items.Items[Index].Picture.Width + 2;
      end
      else if Assigned(FImages) and (Items.Items[Index].ImageIndex >= 0) then
      begin
        if TwoColorImages then
        begin
          bmp := TBitmap.Create;
          try
            bmp.Width := FImages.width;
            bmp.Height := FImages.Height;

            bmp.Canvas.Brush.Color := clFuchsia;
            bmp.Canvas.FillRect(rect(0, 0, FImages.Width, FIMages.Height));

            FImages.DrawingStyle := dsTransparent;
            FImages.Draw(bmp.Canvas, 0, 0, Items.Items[Index].ImageIndex);

            bmp.Transparent := true;
            bmp.TransparentMode := tmAuto;

            {if (Items.Items[Index].CaptionAlignment = taLeftJustify) then
              Canvas.Draw(fmaxCaptionLength + Gr.left + 2, Gr.Top + 2, bmp)
            else}
              Canvas.Draw(Gr.left, Gr.Top + 2, bmp);
          finally
            bmp.Free;
          end;
        end
        else
        begin
          {if (Items.Items[Index].CaptionAlignment = taLeftJustify) then
            FImages.Draw(Canvas, fmaxCaptionLength + Gr.left + 2, Gr.Top + 2, Items.Items[Index].ImageIndex, False)
          else}
            FImages.Draw(Canvas, Gr.left, Gr.Top + 2, Items.Items[Index].ImageIndex, False);
        end;
        Gr.Left := Gr.Left + FImages.Width + 2;
      end;

      if IsShowingIconBar and (Items.Items[Index].CaptionAlignment = taLeftJustify) then
      begin
        Gr.Left := FAdvOfficeScrollSelector.IconBarWidth + 4;
      end;
    end;

    Canvas.Brush.Style := bsClear;
    Canvas.Font.Color := SelectionAppearance.TextColorDisabled; // clWhite;

    if not Assigned(FOnDrawItem) and (Items.Items[Index].CaptionAlignment = taLeftJustify) and (Items.Items[Index].Caption <> '') then
    begin
      DrawText(Canvas.Handle, PChar(Items.Items[Index].Caption), -1, Gr, DTSTYLE);
    end;

    if not Assigned(FOnDrawItem) then
    begin
      if (Items.Items[Index].CaptionAlignment = taCenter) and (Items.Items[Index].Caption <> '') then
        DrawText(Canvas.Handle, PChar(Items.Items[Index].Caption), -1, Gr, DTSTYLE or DT_CENTER)
      else if (Items.Items[Index].CaptionAlignment = taRightJustify) and (Items.Items[Index].Caption <> '') then
        DrawText(Canvas.Handle, PChar(Items.Items[Index].Caption), -1, Gr, DTSTYLE);
    end;

    if Assigned(FOnDrawItem) then
      FOnDrawItem(self, Index, R);
  end
  else if (Index = FDownItemIndex) or ((Index = FHotItemIndex) and (Index = ItemIndex)) then
  begin
    if self.SelectionAppearance.ColorDownTo <> clNone then
    begin
      DrawVistaGradient(Canvas, R, SelectionAppearance.ColorDown, SelectionAppearance.ColorDownTo, SelectionAppearance.ColorMirrorDown, SelectionAppearance.ColorMirrorDownTo, SelectionAppearance.BorderColorDown,
          SelectionAppearance.GradientDown, SelectionAppearance.GradientMirrorDown, '', Canvas.Font, Enabled, False, aaClearType, SelectionAppearance.Rounded, False, tpTop, Graph);
    end
    else
    begin
      Canvas.Brush.Color := SelectionAppearance.ColorDown;
      Canvas.Pen.Color := SelectionAppearance.ColorDown;
      DR := R;
      Canvas.Rectangle(DR.Left, DR.Top, DR.Right, DR.Bottom);
    end;

    if not Assigned(FOnDrawItem) then
    begin
      if not Items.Items[Index].Picture.Empty then
      begin
        Items.Items[Index].Picture.GetImageSizes;
        {if (Items.Items[Index].CaptionAlignment = taLeftJustify) then
          Canvas.Draw(fmaxCaptionLength + Gr.left + 2, Gr.Top + 2, Items.Items[Index].Picture)
        else}
          Canvas.Draw(Gr.left, Gr.Top + 2, Items.Items[Index].Picture);

        gr.Left := gr.left + Items.Items[Index].Picture.Width + 2;
      end
      else if Assigned(FImages) and (Items.Items[Index].ImageIndex >= 0) then
      begin
        if TwoColorImages then
        begin
          bmp := TBitmap.Create;
          bmp.Width := FImages.width;
          bmp.Height := FImages.Height;

          bmp.Canvas.Brush.Color := clFuchsia;
          bmp.Canvas.FillRect(rect(0, 0, FImages.Width, FIMages.Height));

          FImages.DrawingStyle := dsTransparent;
          FImages.Draw(bmp.Canvas, 0, 0, Items.Items[Index].ImageIndex);

          bmp.Transparent := true;
          bmp.TransparentMode := tmAuto;

          {if (Items.Items[Index].CaptionAlignment = taLeftJustify) then
            Canvas.Draw(fmaxCaptionLength + Gr.left + 2, Gr.Top + 2, bmp)
          else}
            Canvas.Draw(Gr.left, Gr.Top + 2, bmp);
          bmp.Free;
        end
        else
        begin
          {if (Items.Items[Index].CaptionAlignment = taLeftJustify) then
            FImages.Draw(Canvas, fmaxCaptionLength + Gr.left + 2, Gr.Top + 2, Items.Items[Index].ImageIndex)
          else}
            FImages.Draw(Canvas, Gr.left, Gr.Top + 2, Items.Items[Index].ImageIndex);
        end;
        Gr.Left := Gr.Left + FImages.Width + 2;
      end;

      if IsShowingIconBar and (Items.Items[Index].CaptionAlignment = taLeftJustify) then
      begin
        Gr.Left := FAdvOfficeScrollSelector.IconBarWidth + 4;
      end;
    end;

    Canvas.Brush.Style := bsClear;
    Canvas.Font.Color := SelectionAppearance.TextColorDown; // clWhite;

    if not Assigned(FOnDrawItem) and (Items.Items[Index].CaptionAlignment = taLeftJustify) and (Items.Items[Index].Caption <> '') then
    begin
      DrawText(Canvas.Handle, PChar(Items.Items[Index].Caption), -1, Gr, DTSTYLE);
    end;

    if not Assigned(FOnDrawItem) then
    begin
      if (Items.Items[Index].CaptionAlignment = taCenter) and (Items.Items[Index].Caption <> '') then
        DrawText(Canvas.Handle, PChar(Items.Items[Index].Caption), -1, Gr, DTSTYLE or DT_CENTER)
      else if (Items.Items[Index].CaptionAlignment = taRightJustify) and (Items.Items[Index].Caption <> '') then
        DrawText(Canvas.Handle, PChar(Items.Items[Index].Caption), -1, Gr, DTSTYLE);
    end;

    if Assigned(FOnDrawItem) then
      FOnDrawItem(self, Index, R);

  end
  else if (Index = FHotItemIndex) then
  begin
    if SelectionAppearance.ColorHotTo{FColorHotTo} <> clNone then
    begin
      //DrawGradient(Canvas, ColorHot, ColorHotTo, 16, Rect(R.Left, R.Top, R.Right - 1, R.Bottom), true)
      DrawVistaGradient(Canvas, R, SelectionAppearance.ColorHot, SelectionAppearance.ColorHotTo, SelectionAppearance.ColorMirrorHot, SelectionAppearance.ColorMirrorHotTo, SelectionAppearance.BorderColorHot,
        SelectionAppearance.GradientHot, SelectionAppearance.GradientMirrorHot, '', Canvas.Font, Enabled, False, aaClearType, SelectionAppearance.Rounded, False, tpTop, Graph);
    end
    else
    begin
      Canvas.Brush.Color := SelectionAppearance.ColorHot;
      Canvas.Pen.Color := SelectionAppearance.ColorHot;
      DR := R;
      Canvas.Rectangle(DR.Left, DR.Top, DR.Right, DR.Bottom);
    end;

    if not Assigned(FOnDrawItem) then
    begin
      if not Items.Items[Index].Picture.Empty then
      begin
        Items.Items[Index].Picture.GetImageSizes;
        {if (Items.Items[Index].CaptionAlignment = taLeftJustify) then
          Canvas.Draw(fmaxCaptionLength + Gr.left + 2, Gr.Top + 2, Items.Items[Index].Picture)
        else}
          Canvas.Draw(Gr.left, Gr.Top + 2, Items.Items[Index].Picture);

        gr.Left := gr.left + Items.Items[Index].Picture.Width + 2;
      end
      else if Assigned(FImages) and (Items.Items[Index].ImageIndex >= 0) then
      begin
        if TwoColorImages then
        begin
          bmp := TBitmap.Create;
          bmp.Width := FImages.width;
          bmp.Height := FImages.Height;

          bmp.Canvas.Brush.Color := clFuchsia;
          bmp.Canvas.FillRect(rect(0, 0, FImages.Width, FIMages.Height));

          FImages.DrawingStyle := dsTransparent;
          FImages.Draw(bmp.Canvas, 0, 0, Items.Items[Index].ImageIndex);

          bmp.Transparent := true;
          bmp.TransparentMode := tmAuto;

          {if (Items.Items[Index].CaptionAlignment = taLeftJustify) then
            Canvas.Draw(fmaxCaptionLength + Gr.left + 2, Gr.Top + 2, bmp)
          else}
            Canvas.Draw(Gr.left, Gr.Top + 2, bmp);
          bmp.Free;
        end
        else
        begin
          {if (Items.Items[Index].CaptionAlignment = taLeftJustify) then
            FImages.Draw(Canvas, fmaxCaptionLength + Gr.left + 2, Gr.Top + 2, Items.Items[Index].ImageIndex)
          else}
            FImages.Draw(Canvas, Gr.left, Gr.Top + 2, Items.Items[Index].ImageIndex);
        end;
        Gr.Left := Gr.Left + FImages.Width + 2;
      end;
      
      if IsShowingIconBar and (Items.Items[Index].CaptionAlignment = taLeftJustify) then
      begin
        Gr.Left := FAdvOfficeScrollSelector.IconBarWidth + 4;
      end;
    end;

    Canvas.Brush.Style := bsClear;
    Canvas.Font.Color := SelectionAppearance.TextColorHot; // clBlack;

    if not Assigned(FOnDrawItem) and (Items.Items[Index].CaptionAlignment = taLeftJustify) and (Items.Items[Index].Caption <> '') then
    begin
      DrawText(Canvas.Handle, PChar(Items.Items[Index].Caption), -1, Gr, DTSTYLE);
    end;

   { if (Items.Items[Index].CaptionAlignment in [taRightJustify, taCenter]) and (Items.Items[Index].Caption <> '') then
    begin
      DrawText(Canvas.Handle, PChar(Items.Items[Index].Caption),-1, Gr, DT_SINGLELINE or DT_VCENTER);
    end; }
    if not Assigned(FOnDrawItem) then
    begin
      if (Items.Items[Index].CaptionAlignment = taCenter) and (Items.Items[Index].Caption <> '') then
        DrawText(Canvas.Handle, PChar(Items.Items[Index].Caption), -1, Gr, DTSTYLE or DT_CENTER)
      else if (Items.Items[Index].CaptionAlignment = taRightJustify) and (Items.Items[Index].Caption <> '') then
        DrawText(Canvas.Handle, PChar(Items.Items[Index].Caption), -1, Gr, DTSTYLE);
    end;

    if Assigned(FOnDrawItem) then
      FOnDrawItem(self, Index, R);

  end
  else // Normal
  begin
    if RefreshItem and (Index <> ItemIndex) then
    begin
      InvalidateRect(Handle, @R, True);
      Exit;
    end;

    if not Assigned(FOnDrawItem) then
    begin
      if Items.Items[Index].backGroundColor <> clNone then
      begin
        Canvas.Pen.Color := Items.Items[Index].backGroundColor;
        Canvas.Brush.Color := Items.Items[Index].backGroundColor;
        Canvas.Rectangle(R.Left + 3, R.Top + 3, R.Right - 3, R.Bottom - 3);
      end;
    end;

    if Index = ItemIndex then // Selected Item
    begin
      if SelectionAppearance.ColorCheckedTo{FColorSelectedTo} <> clNone then
      begin
        //DrawGradient(Canvas, ColorSelected, ColorSelectedTo, 16, Items.Items[Index].ItemRect, true)
        DrawVistaGradient(Canvas, R, SelectionAppearance.ColorChecked, SelectionAppearance.ColorCheckedTo, SelectionAppearance.ColorMirrorChecked, SelectionAppearance.ColorMirrorCheckedTo, SelectionAppearance.BorderColorChecked,
          SelectionAppearance.GradientChecked, SelectionAppearance.GradientMirrorChecked, '', Canvas.Font, Enabled, False, aaClearType, SelectionAppearance.Rounded, False, tpTop, Graph);
      end
      else
      begin
        Canvas.Brush.Color := SelectionAppearance.ColorChecked; //ColorSelected;
        Canvas.Pen.Color := SelectionAppearance.ColorChecked; //ColorSelected;
	      DR := R;
        Canvas.Rectangle(DR.Left, DR.Top, DR.Right, DR.Bottom);
      end;
    end;

    if not Assigned(FOnDrawItem) then
    begin
      if not Items.Items[Index].Picture.Empty then
      begin
        Items.Items[Index].Picture.GetImageSizes;
        {if (Items.Items[Index].CaptionAlignment = taLeftJustify) then
          Canvas.Draw(fmaxCaptionLength + Gr.left + 2, Gr.Top + 2, Items.Items[Index].Picture)
        else}
          Canvas.Draw(Gr.left, Gr.Top + 2, Items.Items[Index].Picture);

        gr.Left := gr.left + Items.Items[Index].Picture.Width + 2;
      end
      else if Assigned(FImages) and (Items.Items[Index].ImageIndex >= 0) then
      begin
        if TwoColorImages then
        begin
          bmp := TBitmap.Create;
          bmp.Width := FImages.width;
          bmp.Height := FImages.Height;

          bmp.Canvas.Brush.Color := clFuchsia;
          bmp.Canvas.FillRect(rect(0, 0, FImages.Width, FIMages.Height));

          FImages.DrawingStyle := dsTransparent;
          FImages.Draw(bmp.Canvas, 0, 0, Items.Items[Index].ImageIndex);

          bmp.Transparent := true;
          bmp.TransparentMode := tmAuto;

          {if (Items.Items[Index].CaptionAlignment = taLeftJustify) then
            Canvas.Draw(fmaxCaptionLength + Gr.left + 2, Gr.Top + 2, bmp)
          else}
            Canvas.Draw(Gr.left, Gr.Top + 2, bmp);
          bmp.Free;
        end
        else
        begin
          {if (Items.Items[Index].CaptionAlignment = taLeftJustify) then
            FImages.Draw(Canvas, fmaxCaptionLength + Gr.left + 2, Gr.Top + 2, Items.Items[Index].ImageIndex)
          else}
            FImages.Draw(Canvas, Gr.left, Gr.Top + 2, Items.Items[Index].ImageIndex);
        end;
        Gr.Left := Gr.Left + FImages.Width + 2;
      end;

      if IsShowingIconBar and (Items.Items[Index].CaptionAlignment = taLeftJustify) then
      begin
        Gr.Left := FAdvOfficeScrollSelector.IconBarWidth + 4;
      end;      
    end;

    Canvas.Brush.Style := bsClear;
    Canvas.Font.Color := SelectionAppearance.TextColor;// clBlack;

    if Index = ItemIndex then // Selected Item
      Canvas.Font.Color := SelectionAppearance.TextColorChecked;

    if not Assigned(FOnDrawItem) and (Items.Items[Index].CaptionAlignment = taLeftJustify) and (Items.Items[Index].Caption <> '') then
    begin
      DrawText(Canvas.Handle, PChar(Items.Items[Index].Caption), -1, Gr, DTSTYLE);
    end;

    if not Assigned(FOnDrawItem) then
    begin
      if (Items.Items[Index].CaptionAlignment = taCenter) and (Items.Items[Index].Caption <> '') then
        DrawText(Canvas.Handle, PChar(Items.Items[Index].Caption), -1, Gr, DTSTYLE or DT_CENTER)
      else if (Items.Items[Index].CaptionAlignment = taRightJustify) and (Items.Items[Index].Caption <> '') then
        DrawText(Canvas.Handle, PChar(Items.Items[Index].Caption), -1, Gr, DTSTYLE);
    end;

    if Assigned(FOnDrawItem) then
      FOnDrawItem(self, Index, R);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvScrollSelectorPanel.DrawItemEx(Index: integer; RefreshItem: boolean = false; Graph: TGPGraphics = nil);
begin
  if UseCache and not RefreshItem then
  begin
    DrawItem(Index, RefreshItem, nil, FBKGCache.Canvas);
    Canvas.Draw(0, 0, FBKGCache);
  end
  else
  begin
    FNeedRePaint := True;
    DrawItem(Index, RefreshItem, Graph);
  end;
end;

//------------------------------------------------------------------------------

function TAdvScrollSelectorPanel.IsShowingIconBar: Boolean;
begin
  Result := ShowFullWidthItem and not ShowCaptionItem and not ShowAutoSizeButton and Assigned(AdvOfficeScrollSelector)
            and (AdvOfficeScrollSelector.IconBarWidth > 0) and Assigned(Images);
end;

//------------------------------------------------------------------------------

procedure TAdvScrollSelectorPanel.Paint;
var
  i: integer;
  R: TRect;
  Graph: TGPGraphics;
begin
  if ShowFullWidthItem or ShowCaptionItem then
    inherited;

  if not Assigned(AdvOfficeScrollSelector) then
    Exit;

  if not FNeedRePaint and UseCache then
  begin
    Canvas.Draw(0, 0, FBKGCache);
    Exit;
  end;

  if not ShowFullWidthItem and not ShowCaptionItem then
  begin
    if not Assigned(FBKGCache) then
      FBKGCache := TBitmap.Create;
    FBKGCache.Width := Width;
    FBKGCache.Height := AdvOfficeScrollSelector.Height;
    FBKGCache.Canvas.Brush.Color := Self.Color;
    FBKGCache.Canvas.FillRect(Rect(0, 0, FBKGCache.Width, FBKGCache.Height));
  end;

  if ShowFullWidthItem and not ShowCaptionItem and not ShowAutoSizeButton then
  begin
    R := Rect(0, 0, AdvOfficeScrollSelector.IconBarWidth, Height);
    if (AdvOfficeScrollSelector.IconBarAppearance.Color <> clNone) and (AdvOfficeScrollSelector.IconBarAppearance.ColorTo <> clNone) then
      DrawGradient(Canvas, AdvOfficeScrollSelector.IconBarAppearance.Color, AdvOfficeScrollSelector.IconBarAppearance.ColorTo, AdvOfficeScrollSelector.IconBarAppearance.steps, R, True)
    else
    begin
      Canvas.Brush.Color := AdvOfficeScrollSelector.IconBarAppearance.Color;
      Canvas.Pen.Color := AdvOfficeScrollSelector.IconBarAppearance.Color;
      Canvas.Rectangle(R);
    end;

    Canvas.Pen.Color := clSilver;
    Canvas.MoveTo(R.Right, 1);
    Canvas.LineTo(R.Right, Height);

    Canvas.MoveTo(0, 1);
    Canvas.LineTo(Width, 1);
  end;

  if UseCache then
    Graph := TGPGraphics.Create(FBKGCache.Canvas.Handle)
  else
    Graph := TGPGraphics.Create(Canvas.Handle);

  for i := 0 to Items.Count - 1 do
  begin
    if UseCache then
      DrawItem(i, false, Graph, FBKGCache.Canvas)
    else
      DrawItem(i, false, Graph, Canvas);
  end;
  if UseCache then
    Canvas.Draw(0, 0, FBKGCache);
  Graph.free;
  FNeedRePaint := False;
end;

//------------------------------------------------------------------------------

procedure TAdvScrollSelectorPanel.ResetDown;
begin
  inherited;
  FDownItemIndex := -1;
  FMouseDown := false;
end;

//------------------------------------------------------------------------------

procedure TAdvScrollSelectorPanel.SetImages(const Value: TCustomImageList);
begin
  FImages := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvScrollSelectorPanel.SetItemIndex(const Value: integer);
begin
  FNeedRePaint := True;
  if Value < 0 then
  begin
    FItemIndex := -1;
    exit;
  end;

  if not Assigned(AdvOfficeScrollSelector) then
    Exit;
  
  if Value < Items.Count then
  begin
    if (Items.Items[Value].Enable) and (Items.Items[Value].ItemType <> itCaption) then
    begin
      if FItemIndex <> Value then
      begin
        FItemIndex := Value;
      end;
    end;
  end;

  if (csDesigning in ComponentState) then
    Invalidate;
end;

//------------------------------------------------------------------------------

function TAdvScrollSelectorPanel.TotalAutoSizeButtons: integer;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to Items.Count - 1 do
  begin
    if Items.Items[i].ItemType = itAutoSizeButton then
      inc(Result);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvScrollSelectorPanel.AutoSizeBtnSize(var W, H: integer);
var
  i, ImgW, ImgH, CapW, CapH: integer;
begin
  w := MinButtonWidth;
  H := MinButtonHeight;

  if not Assigned(AdvOfficeScrollSelector) then
    Exit;
    
  for i := 0 to Items.Count - 1 do
  begin
    ImgW := 0;
    ImgH := 0;
    if Items.Items[i].ItemType = itAutoSizeButton then
    begin
      if not Items.Items[i].Picture.Empty then
      begin
        Items.Items[i].Picture.GetImageSizes;
        ImgW := Items.Items[i].Picture.Width;
        ImgH := Items.Items[i].Picture.Height;
      end
      else if Assigned(FImages) and (Items.Items[i].ImageIndex >= 0) and (Items.Items[i].ImageIndex < FImages.Count) then
      begin
        ImgW := FImages.Width;
        ImgH := FImages.Height;
      end;

      CapW := Canvas.TextWidth(Items.Items[i].Caption);
      if (Items.Items[i].Caption <> '') and (ImgW > 4) then
        CapW := CapW + 2;
      CapH := Canvas.TextHeight('gh');

      W := Max(W, ImgW + CapW + (ButtonMargin * 2));
      H := Max(H, Max(ImgH, CapH) + (ButtonMargin * 2) - 2);

      if (AdvOfficeScrollSelector.ButtonSize.Width > 0) then
      begin
        W := AdvOfficeScrollSelector.ButtonSize.Width + (ButtonMargin * 2);
      end;

      if (AdvOfficeScrollSelector.ButtonSize.Height > 0) then
      begin
        H := AdvOfficeScrollSelector.ButtonSize.Height + (ButtonMargin * 2)-2;
      end;

      {if (AdvOfficeScrollSelector.ButtonSize.Width > 0) and (AdvOfficeScrollSelector.ButtonSize.Height > 0) then
      begin
        W := Max(W, Canvas.TextWidth(Items.Items[i].Caption) + AdvOfficeScrollSelector.ButtonSize.Width + (ButtonMargin * 2));
        H := Max(H, MAX(Canvas.TextHeight('gh'), AdvOfficeScrollSelector.ButtonSize.Height) + ButtonMargin); // Single Margin added
      end
      else
      begin
        if Assigned(FImages) and (Items.Items[i].ImageIndex >= 0) then
        begin
          W := Max(W, Canvas.TextWidth(Items.Items[i].Caption) + FImages.Width + (ButtonMargin * 2));
          H := Max(H, MAX(Canvas.TextHeight('gh'), FImages.Height) + ButtonMargin); // Single Margin added
        end
        else if not Items.Items[i].Picture.Empty then
        begin
          Items.Items[i].Picture.GetImageSizes;
          W := Max(W, Canvas.TextWidth(Items.Items[i].Caption) + Items.Items[i].Picture.Width + (ButtonMargin * 2));
          H := Max(H, MAX(Canvas.TextHeight('gh'), Items.Items[i].Picture.Height) + ButtonMargin); // Single Margin added
        end
        else
        begin
          W := Max(W, Canvas.TextWidth(Items.Items[i].Caption) + (ButtonMargin * 2));
          H := Max(H, Canvas.TextHeight('gh') + ButtonMargin); // Single Margin added
        end;   
      end;}
      FMaxCaptionLength := Max(FMaxCaptionLength, Canvas.TextWidth(Items.Items[i].Caption));
    end;
  end;
end;

//------------------------------------------------------------------------------

function TAdvScrollSelectorPanel.GetMaxWidth: integer;
var
  i, asb, fwb: integer;
begin
  Result := FleftOffSet * 2 + MinButtonWidth;
  if not Assigned(AdvOfficeScrollSelector) then
    Exit;
  
  fwb := Result;
  asb := Result;
  if ShowAutoSizeButton then
    AutoSizeBtnSize(asb {GetWidth}, i{dummy});

  for i := 0 to Items.Count - 1 do
  begin
    if (Items.Items[i].ItemType in [itFullWidthButton, itCaption]) then
    begin
      if Assigned(FImages) and (Items.Items[i].ImageIndex >= 0) then
        fwb := Max(fwb, Canvas.TextWidth(Items.Items[i].Caption) + FImages.Width + (ButtonMargin * 2) + (FleftOffSet * 2))
      else
        fwb := Max(fwb, Canvas.TextWidth(Items.Items[i].Caption) + (ButtonMargin * 2) + (FleftOffSet * 2));
    end;
  end;

  Result := Max(fwb, asb * min(ButtonsPerRow, TotalAutoSizeButtons) + (FleftOffSet * 2) {(ButtonMargin*2)});
end;

//------------------------------------------------------------------------------

function TAdvScrollSelectorPanel.GetItemRect(
  Item: TAdvScrollSelectorItem): TRect;
var
  y, i: Integer;
begin
  Result := Rect(-1, -1, -1, -1);

  if not Assigned(Item) then
    Exit;

  y := 0;

  if (FTopRow > 1) and not AutoHeight then
    y := FRowHeight * (FTopRow - 1);

  if ShowAutoSizeButton and not ShowCaptionItem and not ShowFullWidthItem then
  begin
    Result := Item.ItemRect;
  end
  else if ShowAutoSizeButton and ShowCaptionItem and not ShowFullWidthItem then
  begin
    Result := Item.ItemRect1;
  end
  else if not ShowAutoSizeButton and not ShowCaptionItem and ShowFullWidthItem then
  begin
    Result := Item.ItemRect2;
  end;

  if not AutoHeight then
  begin
    if IntegralRows then
      i := 1
    else
      i := 0;
      
    if (GetItemRow(Item) >= FTopRow) and (GetItemRow(Item) <= FTopRow + FVisibleRowCount - i) then
    begin
      Result := Rect(Result.Left, Result.Top - y, Result.Right, Result.Bottom - y);
    end
    else
    begin
      Result := Rect(-1, -1, -1, -1);
    end;
  end;
end;

//------------------------------------------------------------------------------

function TAdvScrollSelectorPanel.GetItemRow(Item: TAdvScrollSelectorItem): Integer;
begin
  Result := -1;
  if not Assigned(Item) then
    Exit;

  if ShowAutoSizeButton and not ShowCaptionItem and not ShowFullWidthItem then
  begin
    Result := Item.Row;
  end
  else if ShowAutoSizeButton and ShowCaptionItem and not ShowFullWidthItem then
  begin
    Result := Item.Row1;
  end
  else if not ShowAutoSizeButton and not ShowCaptionItem and ShowFullWidthItem then
  begin
    Result := Item.Row2;
  end;
end;

//------------------------------------------------------------------------------

function TAdvScrollSelectorPanel.GetItemCol(
  Item: TAdvScrollSelectorItem): Integer;
begin
  Result := -1;
  if not Assigned(Item) then
    Exit;

  if ShowAutoSizeButton and not ShowCaptionItem and not ShowFullWidthItem then
  begin
    Result := Item.Col;
  end
  else if ShowAutoSizeButton and ShowCaptionItem and not ShowFullWidthItem then
  begin
    Result := Item.Col1;
  end
  else if not ShowAutoSizeButton and not ShowCaptionItem and ShowFullWidthItem then
  begin
    Result := Item.Col2;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvScrollSelectorPanel.SetItemsPosition;
var
  i, c, r, absW, absH, MaxW, bNo, rc, H, LOffSt: integer;
  ShouldChangeRow, ShouldChangeRow2: boolean;
begin
  if not Assigned(AdvOfficeScrollSelector) then
    Exit;

  r := FTopOffSet;
  c := FLeftOffSet;
  bNo := 0;
  MaxW := GetMaxWidth;
  AutoSizeBtnSize(absW, absH);

  FRowHeight := absH;

  //Width := MaxW;
  MaxW := Max(MaxW, Width);

  rc := 1;
  if not ShowAutoSizeButton and not ShowCaptionItem and ShowFullWidthItem then
    rc := 0;
  ShouldChangeRow := false;
  ShouldChangeRow2 := false;
  for i := 0 to Items.Count - 1 do
  begin
    if (Items.Items[i].ItemType = itAutoSizeButton) then
    begin
      if ShowAutoSizeButton then
      begin
        inc(bNo);
        if (bNo > self.ButtonsPerRow) or (ShouldChangeRow2 and not ShowCaptionItem and ((rc > 1) or (bNo > 1))) then
        begin
          r := r + absH;
          bNo := 1;
          c := FLeftOffSet;
          Inc(rc);
        end;

        if ShowAutoSizeButton and not ShowCaptionItem and not ShowFullWidthItem then
        begin
          Items.Items[i].ItemRect := Rect(c, r, c + absW, r + absH);
          Items.Items[i].Row := rc;
          Items.Items[i].Col := bNo;
        end
        else if ShowAutoSizeButton and ShowCaptionItem and not ShowFullWidthItem then
        begin
          Items.Items[i].ItemRect1 := Rect(c, r, c + absW, r + absH);
          Items.Items[i].Row1 := rc;
          Items.Items[i].Col1 := bNo;
        end
        else if not ShowAutoSizeButton and not ShowCaptionItem and ShowFullWidthItem then
        begin
          Items.Items[i].ItemRect2 := Rect(c, r, c + absW, r + absH);
          Items.Items[i].Row2 := rc;
          Items.Items[i].Col2 := bNo;
        end;

        c := c + absW;
        ShouldChangeRow := true;
        ShouldChangeRow2 := false;
      end
      else
      begin
        if ShowAutoSizeButton and not ShowCaptionItem and not ShowFullWidthItem then
        begin
          Items.Items[i].ItemRect := Rect(-1, -1, -1, -1);
          Items.Items[i].Row := -1;
          Items.Items[i].Col := -1;
        end
        else if ShowAutoSizeButton and ShowCaptionItem and not ShowFullWidthItem then
        begin
          Items.Items[i].ItemRect1 := Rect(-1, -1, -1, -1);
          Items.Items[i].Row1 := -1;
          Items.Items[i].Col1 := -1;
        end
        else if not ShowAutoSizeButton and not ShowCaptionItem and ShowFullWidthItem then
        begin
          Items.Items[i].ItemRect2 := Rect(-1, -1, -1, -1);
          Items.Items[i].Row2 := -1;
          Items.Items[i].Col2 := -1;
        end;
      end;
    end
    else if (Items.Items[i].ItemType in [itFullWidthButton, itCaption]) then
    begin
      if (((Items.Items[i].ItemType = itFullWidthButton) and ShowFullWidthItem) or ((Items.Items[i].ItemType = itCaption) and ShowCaptionItem)) then
      begin
        Inc(rc);
        
        if ShouldChangeRow then
          r := r + absH + 2; //FButtonHeight;

        LOffSt := FLeftOffSet;
        if ((Items.Items[i].ItemType = itCaption) and ShowCaptionItem) then
          LOffSt := 1;

        if ShowAutoSizeButton and not ShowCaptionItem and not ShowFullWidthItem then
        begin
          Items.Items[i].ItemRect := Rect(LOffSt, r, MaxW - LOffSt, r + FButtonHeight);
          Items.Items[i].Row := rc;
          Items.Items[i].Col := 1;
        end
        else if ShowAutoSizeButton and ShowCaptionItem and not ShowFullWidthItem then
        begin
          Items.Items[i].ItemRect1 := Rect(LOffSt, r, MaxW - LOffSt, r + FButtonHeight);
          Items.Items[i].Row1 := rc;
          Items.Items[i].Col1 := 1;
        end
        else if not ShowAutoSizeButton and not ShowCaptionItem and ShowFullWidthItem then
        begin
          Items.Items[i].ItemRect2 := Rect(LOffSt, r, MaxW - LOffSt, r + FButtonHeight);
          Items.Items[i].Row2 := rc;
          Items.Items[i].Col2 := 1;
        end;

        r := r + FButtonHeight;
        c := FLeftOffSet;
        bNo := 0;
        ShouldChangeRow := false;
      end
      else
      begin
        if ShowAutoSizeButton and not ShowCaptionItem and not ShowFullWidthItem then
        begin
          Items.Items[i].ItemRect := Rect(-1, -1, -1, -1);
          Items.Items[i].Row := -1;
          Items.Items[i].Col := -1;
        end
        else if ShowAutoSizeButton and ShowCaptionItem and not ShowFullWidthItem then
        begin
          Items.Items[i].ItemRect1 := Rect(-1, -1, -1, -1);
          Items.Items[i].Row1 := -1;
          Items.Items[i].Col1 := -1;
        end
        else if not ShowAutoSizeButton and not ShowCaptionItem and ShowFullWidthItem then
        begin
          Items.Items[i].ItemRect2 := Rect(-1, -1, -1, -1);
          Items.Items[i].Row2 := -1;
          Items.Items[i].Col2 := -1;
        end;

        ShouldChangeRow2 := True;
      end;
    end;
  end;

  FRowCount := rc;
  if not AutoHeight then
  begin
    H := Height - FTopOffSet*2;
    FVisibleRowCount := Max(1, (H div absH));

    i := 1;
    if (RowCount > VisibleRowCount) then
      i := (RowCount div VisibleRowCount) + (RowCount mod VisibleRowCount);
    if (FTopRow > i) then
      FTopRow := Max(1, i);
  end
  else
  begin
    FVisibleRowCount := RowCount;
    FTopRow := 1;
  end;

  SetPanelHeight;
end;

//------------------------------------------------------------------------------

function TAdvScrollSelectorPanel.GetButtonsPerRow: TNoOfButtons;
var
  absW, absH, w: Integer;
begin
  Result := 1;
  if not Assigned(AdvOfficeScrollSelector) then
    Exit;

  AutoSizeBtnSize(absW, absH);
  w := Width - (FLeftOffSet*2);
  Result := Max(1, w div absW);    
end;

//------------------------------------------------------------------------------

procedure TAdvScrollSelectorPanel.SetPanelHeight;
var
  h, i: Integer;
begin
  if not Assigned(AdvOfficeScrollSelector) then
    Exit;

  if (Items.Count > 0) and AutoHeight then
  begin
    h := 0;
    for i:= 0 to Items.Count-1 do
    begin
      h := Max(h, GetItemRect(Items.Items[i]).Bottom);
    end;

    h := h + FTopOffSet;
    FInternalUpdatingSize := True;
    Height := h;
    FInternalUpdatingSize := False;
  end;
end;

//------------------------------------------------------------------------------

function TAdvScrollSelectorPanel.ItemAtPos(X, Y: integer): integer;
var
  i: integer;
begin
  Result := -1;
  if not Assigned(AdvOfficeScrollSelector) then
    Exit;

  for i := 0 to Items.Count - 1 do
  begin
    if PtInRect(GetItemRect(Items.Items[i]), Point(X, Y)) then
    begin
      Result := i;
      break;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvScrollSelectorPanel.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  i: integer;
begin
  FMouseDown := true;

  inherited;

  if not Assigned(AdvOfficeScrollSelector) then
    Exit;

  i := ItemAtPos(X, Y);
  if (i >= 0) and Items.Items[i].Enable and (Items.Items[i].ItemType <> itCaption) then
  begin
    FDownItemIndex := i;
    if (i <> ItemIndex) then
      DrawItemEx(i, true);
  end;

  if Assigned(AdvOfficeScrollSelector) then
    if Assigned(AdvOfficeScrollSelector.OnMouseDown) then
      AdvOfficeScrollSelector.OnMouseDown(AdvOfficeScrollSelector, Button, Shift, X, Y);
end;

//------------------------------------------------------------------------------

procedure TAdvScrollSelectorPanel.DoSelect;
begin
  if Assigned(FOnSelect) then
    FOnSelect(self);
end;

//------------------------------------------------------------------------------

procedure TAdvScrollSelectorPanel.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  OldItemIndex: Integer;
begin
  inherited;

  FMouseDown := false;

  if not Assigned(AdvOfficeScrollSelector) then
    Exit;

  if Assigned(AdvOfficeScrollSelector) then
    if Assigned(AdvOfficeScrollSelector.OnMouseUp) then
      AdvOfficeScrollSelector.OnMouseUp(AdvOfficeScrollSelector, Button, Shift, X, Y);
    
  if (FHotItemIndex = -1) and (FDownItemIndex = -1) then
    Exit;

  if Assigned(FOnShouldHide) then
    FOnShouldHide(self);

  if (FDownItemIndex > -1) and (FHotItemIndex > -1) then
  begin
    if not Items.Items[FDownItemIndex].MenuItem then
    begin
      OldItemIndex := ItemIndex;
      ItemIndex := FDownItemIndex;

      if (OldItemIndex >= 0) then
        DrawItemEx(OldItemIndex, true);

      DoSelect;
    end;
  end;
  FHotItemIndex := -1;
  FDownItemIndex := -1;
end;

//------------------------------------------------------------------------------

procedure TAdvScrollSelectorPanel.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvScrollSelectorPanel.WMChar(var Msg: TWMKey);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvScrollSelectorPanel.CMWantSpecialKey(var Msg: TCMWantSpecialKey);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvScrollSelectorPanel.SetButtonMargin(const Value: integer);
begin
  FButtonMargin := Value;
end;

//------------------------------------------------------------------------------


procedure TAdvScrollSelectorPanel.CMMouseLeave(var Message: TMessage);
var
  i: integer;
begin
  inherited;
  i := FHotItemIndex;
  FHotItemIndex := -1;
  FDownItemIndex := -1;

  if (i > -1) then
    DrawItemEx(i, true);
end;

//------------------------------------------------------------------------------

procedure TAdvScrollSelectorPanel.SetMinButtonHeight(const Value: integer);
begin
  FMinButtonHeight := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvScrollSelectorPanel.SetMinButtonWidth(const Value: integer);
begin
  FMinButtonWidth := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvScrollSelectorPanel.SetTwoColorImages(const Value: Boolean);
begin
  FTwoColorImages := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvScrollSelectorPanel.SetSelectionAppearance(
  const Value: TSelectionAppearance);
begin
  FSelectionAppearance.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TAdvScrollSelectorPanel.SetCaptionAppearance(
  const Value: TSimpleGradientCaption);
begin
  FCaptionAppearance.Assign(Value);
end;

//------------------------------------------------------------------------------

function TAdvScrollSelectorPanel.GetItems: TAdvScrollSelectorItems;
begin
  if Assigned(AdvOfficeScrollSelector) then
    Result := AdvOfficeScrollSelector.Tools
  else
    Result := nil;
end;

//------------------------------------------------------------------------------

function TAdvScrollSelectorPanel.CanScrollDown: Integer;
begin
  Result := 0;
  if not AutoHeight then
  begin
    Result := RowCount - (FTopRow + FVisibleRowCount -1);
  end;
end;

//------------------------------------------------------------------------------

function TAdvScrollSelectorPanel.CanScrollUp: Integer;
begin
  Result :=  0;
  if not AutoHeight then
  begin
    Result := FTopRow - 1;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvScrollSelectorPanel.SetTopRow(Value: Integer);
var
  r: Integer;
begin
  if not AutoHeight and (Value <> FTopRow) and (Value > 0) and (Value < Items.Count) then
  begin
    if (Value > FTopRow) then
    begin
      r := Value - FTopRow;
    end
    else
    begin
      r := Value - FTopRow;
    end;

    if (r <> 0) then
      ScrollRows(r);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvScrollSelectorPanel.ViewButton(Index: Integer);
var
  ItemRow, r: Integer;
begin
  if not AutoHeight and (Index >= 0) and (Index < Items.Count) and (GetItemRow(Items.Items[Index]) >= 0) then
  begin
    ItemRow := GetItemRow(Items.Items[Index]);
    if (ItemRow >= FTopRow) then
    begin
      r := 0;
      if (ItemRow > (FTopRow-1) + FVisibleRowCount) then
      begin
        r := ItemRow - ((FTopRow-1) + FVisibleRowCount);
      end;
    end
    else
    begin
      r := ItemRow - FTopRow;
    end;

    if (r <> 0) then
      ScrollRows(r);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvScrollSelectorPanel.ViewSelectedButton;
begin
  ViewButton(ItemIndex);
end;

//------------------------------------------------------------------------------

procedure TAdvScrollSelectorPanel.ScrollRows(Value: Integer);
begin
  if not AutoHeight then
  begin
    FNeedRePaint := True;
    if (Value < 0) then   // ScrollUp
    begin
      Value := Min(abs(Value), CanScrollUp);  // make it +ve
      if (Value > 0) then
      begin
        FTopRow := Max(1, FTopRow - Value);
        Invalidate;
        if Assigned(FOnScroll) then
          FOnScroll(Self);
      end;
    end
    else if (Value > 0) then  // ScrollDown
    begin
      Value := Min(Value, CanScrollDown);
      if (Value > 0) then
      begin
        FTopRow := Min(RowCount - FVisibleRowCount+1, FTopRow + Value);
        Invalidate;
        if Assigned(FOnScroll) then
          FOnScroll(Self);
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvScrollSelectorPanel.SetBounds(ALeft, ATop, AWidth,
  AHeight: Integer);
begin
  inherited;
  if not (csLoading in ComponentState) and not FInternalUpdatingSize then 
    SetItemsPosition;
end;

//------------------------------------------------------------------------------

function TAdvScrollSelectorPanel.GetHotItem: Integer;
begin
  Result := FHotItemIndex;
end;

//------------------------------------------------------------------------------

procedure TAdvScrollSelectorPanel.SetHotItem(const Value: Integer);
var
  j: Integer;
begin
  if (Value <> FHotItemIndex) then
  begin
    if (FHotItemIndex >= 0) then
    begin
      j := FHotItemIndex;
      FHotItemIndex := -1;
      DrawItemEx(j, true);
    end;

    if (Value >= 0) and (Value < Items.Count) and (Items.Items[Value].Enable) and (Items.Items[Value].ItemType <> itCaption) then
    begin
      FHotItemIndex := Value;
      if FMouseDown then // means mouse down move
        FDownItemIndex := Value;
      DrawItemEx(Value, true);

      if Assigned(FOnHotTool) then
        FOnHotTool(self, Value);
    end;
  end;
end;

//------------------------------------------------------------------------------

function TAdvScrollSelectorPanel.FirstItemIndex: Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Items.Count-1 do
  begin
    if (Items.Items[i].Enable) and (Items.Items[i].ItemType <> itCaption) and (GetItemRow(Items.Items[i]) > 0) then
    begin
      Result := i;
      Break;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TAdvScrollSelectorPanel.LastItemIndex: Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := Items.Count-1 downto 0 do
  begin
    if (Items.Items[i].Enable) and (Items.Items[i].ItemType <> itCaption) and (GetItemRow(Items.Items[i]) > 0) then
    begin
      Result := i;
      Break;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TAdvScrollSelectorPanel.DownItemIndex(Index: Integer): Integer;
var
  r, c, i: Integer;
begin
  Result := Index;
  if (Index >= 0) and (Index < Items.Count) then
  begin
    r := GetItemRow(Items.Items[Index]);
    c := GetItemCol(Items.Items[Index]);
    if (r >= RowCount) then
      r := 1
    else
      r := r + 1;

    for i := 0 to Items.Count-1 do
    begin
      if (Items.Items[i].Enable) and (Items.Items[i].ItemType <> itCaption) and (GetItemRow(Items.Items[i]) = r) then
      begin
        if (GetItemCol(Items.Items[i]) >= c) then
        begin
          Result := i;
          break;
        end
        else
        begin
          Result := i;
        end;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TAdvScrollSelectorPanel.NextItemIndex(Index: Integer): Integer;
var
  i, j: Integer;
begin
  Result := Index;
  i := Index;
  j := i;
  while (i < Items.Count) do
  begin
    Inc(j);
    if (j >= Items.Count) then
      j := 0;
      
    if (Items.Items[j].Enable) and (Items.Items[j].ItemType <> itCaption) and (GetItemRow(Items.Items[j]) > 0) then
    begin
      Result := j;
      Break;
    end;
    inc(i);
  end;
end;

//------------------------------------------------------------------------------

function TAdvScrollSelectorPanel.PreviousItemIndex(
  Index: Integer): Integer;
var
  i, j: Integer;
begin
  Result := Index;
  i := Index;
  j := i;
  while (i < Items.Count) do
  begin
    dec(j);
    if (j < 0) then
      j := Items.Count-1;

    if (Items.Items[j].Enable) and (Items.Items[j].ItemType <> itCaption) and (GetItemRow(Items.Items[j]) > 0) then
    begin
      Result := j;
      Break;
    end;
    inc(i);
  end;
end;

//------------------------------------------------------------------------------

function TAdvScrollSelectorPanel.UpItemIndex(Index: Integer): Integer;
var
  r, c, i: Integer;
begin
  Result := Index;
  if (Index >= 0) and (Index < Items.Count) then
  begin
    r := GetItemRow(Items.Items[Index]);
    c := GetItemCol(Items.Items[Index]);
    if (r <= 1) then
      r := RowCount
    else
      r := r - 1;

    for i := 0 to Items.Count-1 do
    begin
      if (Items.Items[i].Enable) and (Items.Items[i].ItemType <> itCaption) and (GetItemRow(Items.Items[i]) = r) then
      begin
        if (GetItemCol(Items.Items[i]) >= c) then
        begin
          Result := i;
          break;
        end
        else
        begin
          Result := i;
        end;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TAdvScrollSelectorPanel.UseCache: Boolean;
begin
  Result := not ShowFullWidthItem and not ShowCaptionItem and Assigned(FBKGCache);
end;

//------------------------------------------------------------------------------

procedure TAdvScrollSelectorPanel.WMEraseBkGnd(var Message: TWMEraseBkGnd);
begin
  if UseCache then
    Message.Result := 1
  else
    inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvScrollSelectorPanel.WMPaint(var Message: TWMPaint);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvScrollSelectorPanel.SetIntegralRows(const Value: Boolean);
begin
  if (FIntegralRows <> Value) then
  begin
    FIntegralRows := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

{ TSelectorScroller }

constructor TSelectorScroller.Create;
begin
  inherited;
  FMin := 0;
  FMax := 0;
  FPosition := 0;
  FVisible := False;
end;

//------------------------------------------------------------------------------

function TSelectorScroller.CanGoBack: Boolean;
begin
  Result := Position > Min;
end;

//------------------------------------------------------------------------------

function TSelectorScroller.CanGoForward: Boolean;
begin
  Result := Position < Max;
end;

//------------------------------------------------------------------------------

procedure TSelectorScroller.SetMax(const Value: integer);
begin
  if Value >= FMin then FMax := Value;
end;

//------------------------------------------------------------------------------

procedure TSelectorScroller.SetMin(const Value: integer);
begin
  if Value <= FMax then FMin := Value;
end;

//------------------------------------------------------------------------------

procedure TSelectorScroller.SetPosition(const Value: integer);
begin
  FPosition := Value;
end;

//------------------------------------------------------------------------------

procedure TSelectorScroller.SetVisible(const Value: Boolean);
begin
  FVisible := Value;
end;

//------------------------------------------------------------------------------

{ TDbgList }

function TDbgList.GetItemsEx(Index: Integer): Pointer;
begin
  if (Index >= Count) then
  begin
    raise Exception.Create('Index out of bounds in list read access');
  end;

  if Index < Count then
    Result := inherited Items[Index]
  else
    Result := nil;
end;

//------------------------------------------------------------------------------

procedure TDbgList.SetItemsEx(Index: Integer; const Value: Pointer);
begin
  if (Index >= Count) then
  begin
    raise Exception.Create('Index out of bounds in list write access');
  end;
  if Index < Count then
    inherited Items[Index] := value;
end;

//------------------------------------------------------------------------------

{ TScrollSelectorDropDownWindow }

constructor TScrollSelectorDropDownWindow.Create(AOwner: TComponent);
begin
  inherited;
  FHideOnDeActivate := true;
  FHideTimer := TTimer.Create(self);
  FHideTimer.Interval := 1;
  FHideTimer.Enabled := false;
  FHideTimer.OnTimer := HideTimerOnTime;
  FMouseX := 0;
  FMouseY := 0;
end;

//------------------------------------------------------------------------------

constructor TScrollSelectorDropDownWindow.CreateNew(AOwner: TComponent; Dummy: Integer);
begin
  inherited;
  FOwner := AOwner;
  FHideOnDeActivate := true;
  FHideTimer := TTimer.Create(self);
  FHideTimer.Interval := 1;
  FHideTimer.Enabled := false;
  FHideTimer.OnTimer := HideTimerOnTime;
  FResizerHeight := 12;
  FCaptionHeight := 18;
  FDropDownBorderWidth := 2;
  DoubleBuffered := True;
end;

//------------------------------------------------------------------------------

procedure TScrollSelectorDropDownWindow.CreateParams(var Params: TCreateParams);
const
  CS_DROPSHADOW = $00020000;
begin
  inherited CreateParams(Params);
  //Params.Style := Params.Style - WS_BORDER;
  {
  Params.ExStyle := Params.ExStyle or WS_EX_TOPMOST; }

  if (Win32Platform = VER_PLATFORM_WIN32_NT) and
     ((Win32MajorVersion > 5) or
      ((Win32MajorVersion = 5) and (Win32MinorVersion >= 1))) then
    Params.WindowClass.Style := Params.WindowClass.Style or CS_DROPSHADOW;
end;

//------------------------------------------------------------------------------

destructor TScrollSelectorDropDownWindow.Destroy;
begin
  FHideTimer.Enabled := false;
  FHideTimer.Free;
  inherited;
end;

//------------------------------------------------------------------------------

function TScrollSelectorDropDownWindow.GetParentWnd: HWnd;
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

procedure TScrollSelectorDropDownWindow.HideTimerOnTime(Sender: TObject);
begin
  Hide;
  FHideTimer.Enabled := false;
end;


//------------------------------------------------------------------------------

procedure TScrollSelectorDropDownWindow.Loaded;
begin
  inherited;
  FOldCursor := self.Cursor;
end;

//------------------------------------------------------------------------------

procedure TScrollSelectorDropDownWindow.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if (ssLeft in Shift) and SizeGrip then
  begin
    if (((X >= Width - 10) and (X <= Width))) then
    begin
      FResizing := true;
      FMouseX := X;
      FMouseY := Y;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TScrollSelectorDropDownWindow.MouseMove(Shift: TShiftState; X,
  Y: Integer);
var
  ax, ay: integer;
begin
  inherited;

  if ((X >= Width - 10) and (X <= Width)) and ((Y >= Height - 10) and (Y <= Height)) and SizeGrip then
  begin
    if (Cursor <> crSizeWE) then
      Cursor := crSizeNWSE;
  end
  else
  begin
    if Cursor <> FoldCursor then
      Cursor := FoldCursor;
  end;

  if (ssLeft in Shift) and FResizing then
  begin
    ax := (X - FMouseX);
    Width := Width + ax;
    FMouseX := Width;
    ay := (Y - FMouseY);
    Height := Height + ay;
    FMouseY := Height;
  end;
end;

//------------------------------------------------------------------------------

procedure TScrollSelectorDropDownWindow.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if FResizing then
  begin
    FResizing := false;
    FMouseX := 0;
    FMouseY := 0;
    if Cursor <> FoldCursor then
      Cursor := FoldCursor;
  end;
end;

//------------------------------------------------------------------------------

function TScrollSelectorDropDownWindow.GetResizerRect: TRect;
begin
  Result := Rect(-1, -1, -1, -1);
  if SizeGrip then
    Result := Rect(DropDownBorderWidth-1, Height - ResizerHeight - DropDownBorderWidth, Width -DropDownBorderWidth+1, Height -DropDownBorderWidth);
end;

//------------------------------------------------------------------------------

function TScrollSelectorDropDownWindow.GetCaptionRect: TRect;
begin
  Result := Rect(DropDownBorderWidth-1, DropDownBorderWidth-1, Width -DropDownBorderWidth+1, DropDownBorderWidth + CAPTION_HEIGHT);
end;

//------------------------------------------------------------------------------

procedure TScrollSelectorDropDownWindow.Paint;
var
  R, cr, rr: TRect;
  Clr: TColor;
begin
  inherited;
  if not Assigned(ScrollSelector) then
    Exit;

  R := ClientRect;

  // Draw Caption
  with ScrollSelector.CaptionAppearance do
  begin
    cr := GetCaptionRect;
    cr.Bottom := cr.Bottom - 4;
    if ColorTo <> clNone then
      DrawGradient(Canvas, Color, ColorTo, 40, cr, Direction = gdHorizontal)
    else
    begin
      Canvas.Brush.Color := Color;
      Canvas.Pen.Color := Color;
      Canvas.Rectangle(cr);
    end;

    Canvas.Brush.Style := bsClear;
    Canvas.Font.Color := TextColor;

    if (DropDownCaption <> '') then
    begin
      cr.Left := cr.Left + 4;
      DrawText(Canvas.Handle, PChar(DropDownCaption), -1, cr, DT_SINGLELINE or DT_VCENTER or DT_LEFT);
      cr.Left := cr.Left - 4;
    end;

    if (BorderColor <> clNone) then
    begin
      Canvas.Brush.Style := bsClear;
      Canvas.Pen.Color := BorderColor;
      Canvas.Rectangle(cr);
    end;
  end;

  // Draw Resizer
  with ScrollSelector.ResizerAppearance do
  begin
    rr := GetResizerRect;
    if ColorTo <> clNone then
      DrawGradient(Canvas, Color, ColorTo, 40, rr, Direction = gdHorizontal)
    else
    begin
      Canvas.Brush.Color := Color;
      Canvas.Pen.Color := Color;
      Canvas.Rectangle(rr);
    end;

    if (ColorTo <> clNone) then
    begin
      Canvas.Pen.Color := ColorTo;
      Canvas.MoveTo(0, rr.Top);
      Canvas.LineTo(Width, rr.Top);
    end;

    Canvas.Pen.Color := Color;
    Canvas.MoveTo(rr.Left, rr.Top+1);
    Canvas.LineTo(rr.Left, rr.Bottom);

    if SizeGrip then
    begin
      Canvas.Brush.Color := clWhite;
      Canvas.Pen.Color := clWhite;
      Canvas.Rectangle(rr.Right - 6, rr.Bottom - 7, rr.Right - 4, rr.Bottom - 5);
      Canvas.Rectangle(rr.Right - 6, rr.Bottom - 3, rr.Right - 4, rr.Bottom - 1);
      Canvas.Rectangle(rr.Right - 10, rr.Bottom - 3, rr.Right - 8, rr.Bottom - 1);

      Clr := Color;
      if (ColorTo <> clNone) then
        Clr := ColorTo;

      Clr := BlendColor(Clr, clBlack, 50);
      Canvas.Brush.Color :=  Clr;
      Canvas.Pen.Color := Clr;
      Canvas.Rectangle(rr.Right - 5, rr.Bottom - 8, rr.Right - 3, rr.Bottom - 6);
      Canvas.Rectangle(rr.Right - 5, rr.Bottom - 4, rr.Right - 3, rr.Bottom - 2);
      Canvas.Rectangle(rr.Right - 9, rr.Bottom - 4, rr.Right - 7, rr.Bottom - 2);
    end;
  end;

  if ShowBorder then
  begin
    R := ClientRect;
    Canvas.Pen.Color := ScrollSelector.BorderDropDownColor;
    Canvas.Brush.Style := bsClear;
    Canvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
  end;
  
end;

//------------------------------------------------------------------------------

procedure TScrollSelectorDropDownWindow.SetSelectorPanel(
  const Value: TAdvCustomScrollSelectorPanel);
begin
  FSelectorPanel := Value;
  //FSelectorPanel.DropDownWindow := Self;
end;

//------------------------------------------------------------------------------

procedure TScrollSelectorDropDownWindow.SetBounds(ALeft, ATop, AWidth,
  AHeight: Integer);
begin
  if (AHeight > Height) and Assigned(FScrollBox) and FScrollBox.Visible and Assigned(FItemsSelector) then
  begin
    if not FScrollBox.VertScrollBar.IsScrollBarVisible then
      AHeight := Height;
  end;

  inherited;
  if not FInternalChange then
    SetPositions;
  //Invalidate;
  InvalidateCaption;
  InvalidateResizer;
end;

//------------------------------------------------------------------------------

procedure TScrollSelectorDropDownWindow.SetPositions;
var
  i, j, h, br: Integer;
begin
  if SizeGrip then
    i := getresizerRect.Top
  else
    i := Height - DropDownBorderWidth;

  br := DropDownBorderWidth -1;
  if Assigned(FFullWidthSelector) and FFullWidthSelector.Visible then
  begin
    FFullWidthSelector.Left := br;
    FFullWidthSelector.Top := i - FFullWidthSelector.Height;
    FFullWidthSelector.Width := Width - br*2;
    i := FFullWidthSelector.Top;
  end;

  if Assigned(FScrollBox) and FScrollBox.Visible and Assigned(FItemsSelector) then
  begin
    FScrollBox.Left := br;
    FScrollBox.Width := Width - br*2;
    h := i - CaptionHeight;
    if (h > FItemsSelector.Height + 4) then
    begin
      j := h - FItemsSelector.Height;
      h := h - j;
      FInternalChange := True;
      Height := Height - j;
      FInternalChange := False;
    end;
    FScrollBox.Height := h;
    FScrollBox.Top := i - FScrollBox.Height;
  end;
end;

//------------------------------------------------------------------------------

procedure TScrollSelectorDropDownWindow.WMActivate(var Message: TWMActivate);
{var
  rgn: THandle;
  R: TRect; }
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
  else
  begin
    if Assigned(FScrollSelector) then
    begin
      //FScrollSelector.SetFocus;
      SendMessage(GetParentWnd, WM_NCACTIVATE, 1, 0);
    end;

    {R := ClientRect;
    rgn := CreateRoundRectRgn(0,0,R.Right-R.Left,R.Bottom-R.Top, 2, 2);
    if rgn > 0 then
    begin
      try
        SetWindowRgn(Handle,rgn,true);
      finally
        DeleteObject(rgn);
      end;
    end;}
  end;
end;

//------------------------------------------------------------------------------

procedure TScrollSelectorDropDownWindow.WMNCHitTest(var Message: TWMNCHitTest);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TScrollSelectorDropDownWindow.WMKeyDown(var Message: TWMKeyDown);
  procedure UpdateScroller;
  var
    R, SR: TRect;
  begin
    if Assigned(FScrollBox) and FScrollBox.Visible and Assigned(FItemsSelector) and FItemsSelector.Visible and (FItemsSelector.HotItemIndex >= 0) then
    begin
      R := FItemsSelector.GetItemRect(FItemsSelector.Items.Items[FItemsSelector.HotItemIndex]);
      SR := FScrollBox.ClientRect;
      SR.Top := FScrollBox.vertScrollBar.Position;
      SR.Bottom := SR.Top + FScrollBox.ClientHeight;
      
      if R.Top < SR.Top then
        with FScrollBox.VertScrollBar do Position := Position - (SR.Top - R.Top)
      else if R.Bottom > SR.Bottom then
      begin
        with FScrollBox.VertScrollBar do Position := Position + (R.Bottom - SR.Bottom);
      end;
    end;
  end;
  
begin
  inherited;

  case Message.CharCode of
    VK_LEFT:
    begin
      if Assigned(FItemsSelector) and FItemsSelector.Visible and (FItemsSelector.HotItemIndex >= 0) then
      begin
        if (FItemsSelector.HotItemIndex = FItemsSelector.FirstItemIndex) and
           Assigned(FFullWidthSelector) and FFullWidthSelector.Visible and (FFullWidthSelector.RowCount > 0) then
        begin
          FFullWidthSelector.HotItemIndex := FFullWidthSelector.LastItemIndex;
          FItemsSelector.HotItemIndex := -1;
        end
        else
        begin
          FItemsSelector.HotItemIndex := FItemsSelector.PreviousItemIndex(FItemsSelector.HotItemIndex);
          UpdateScroller;
        end;
      end
      else if Assigned(FFullWidthSelector) and FFullWidthSelector.Visible and (FFullWidthSelector.HotItemIndex >= 0) then
      begin
        if (FFullWidthSelector.HotItemIndex = FFullWidthSelector.FirstItemIndex) and
           Assigned(FItemsSelector) and FItemsSelector.Visible and (FItemsSelector.RowCount > 0) then
        begin
          FItemsSelector.HotItemIndex := FItemsSelector.LastItemIndex;
          UpdateScroller;
          FFullWidthSelector.HotItemIndex := -1;
        end
        else
        begin
          FFullWidthSelector.HotItemIndex := FFullWidthSelector.PreviousItemIndex(FFullWidthSelector.HotItemIndex);
        end;
      end
      else if Assigned(FItemsSelector) and FItemsSelector.Visible and (FItemsSelector.ItemIndex >= 0) then
      begin
        if (FItemsSelector.ItemIndex = FItemsSelector.FirstItemIndex) and
           Assigned(FFullWidthSelector) and FFullWidthSelector.Visible and (FFullWidthSelector.RowCount > 0) then
        begin
          FFullWidthSelector.HotItemIndex := FFullWidthSelector.LastItemIndex;
          FItemsSelector.HotItemIndex := -1;
        end
        else
        begin
          FItemsSelector.HotItemIndex := FItemsSelector.PreviousItemIndex(FItemsSelector.ItemIndex);
          UpdateScroller;
        end;
      end
      else if Assigned(FFullWidthSelector) and FFullWidthSelector.Visible and (FFullWidthSelector.ItemIndex >= 0) then
      begin
        if (FFullWidthSelector.ItemIndex = FFullWidthSelector.FirstItemIndex) and
           Assigned(FItemsSelector) and FItemsSelector.Visible and (FItemsSelector.RowCount > 0) then
        begin
          FItemsSelector.HotItemIndex := FItemsSelector.LastItemIndex;
          UpdateScroller;
          FFullWidthSelector.HotItemIndex := -1;
        end
        else
        begin
          FFullWidthSelector.HotItemIndex := FFullWidthSelector.PreviousItemIndex(FFullWidthSelector.ItemIndex);
        end;
      end
      else if Assigned(FItemsSelector) and FItemsSelector.Visible then
      begin
        FItemsSelector.HotItemIndex := FItemsSelector.FirstItemIndex;
        UpdateScroller;
      end
      else if Assigned(FFullWidthSelector) and FFullWidthSelector.Visible then
      begin
        FFullWidthSelector.HotItemIndex := FFullWidthSelector.FirstItemIndex;
      end;
    end;
    VK_RIGHT:
    begin
      if Assigned(FItemsSelector) and FItemsSelector.Visible and (FItemsSelector.HotItemIndex >= 0) then
      begin
        if (FItemsSelector.HotItemIndex = FItemsSelector.LastItemIndex) and
           Assigned(FFullWidthSelector) and FFullWidthSelector.Visible and (FFullWidthSelector.RowCount > 0) then
        begin
          FFullWidthSelector.HotItemIndex := FFullWidthSelector.FirstItemIndex;
          FItemsSelector.HotItemIndex := -1;
        end
        else
        begin
          FItemsSelector.HotItemIndex := FItemsSelector.NextItemIndex(FItemsSelector.HotItemIndex);
          UpdateScroller;
        end;
      end
      else if Assigned(FFullWidthSelector) and FFullWidthSelector.Visible and (FFullWidthSelector.HotItemIndex >= 0) then
      begin
        if (FFullWidthSelector.HotItemIndex = FFullWidthSelector.LastItemIndex) and
           Assigned(FItemsSelector) and FItemsSelector.Visible and (FItemsSelector.RowCount > 0) then
        begin
          FItemsSelector.HotItemIndex := FItemsSelector.FirstItemIndex;
          UpdateScroller;
          FFullWidthSelector.HotItemIndex := -1;
        end
        else
        begin
          FFullWidthSelector.HotItemIndex := FFullWidthSelector.NextItemIndex(FFullWidthSelector.HotItemIndex);
        end;
      end
      else if Assigned(FItemsSelector) and FItemsSelector.Visible and (FItemsSelector.ItemIndex >= 0) then
      begin
        if (FItemsSelector.ItemIndex = FItemsSelector.LastItemIndex) and
           Assigned(FFullWidthSelector) and FFullWidthSelector.Visible and (FFullWidthSelector.RowCount > 0) then
        begin
          FFullWidthSelector.HotItemIndex := FFullWidthSelector.FirstItemIndex;
          FItemsSelector.HotItemIndex := -1;
        end
        else
        begin
          FItemsSelector.HotItemIndex := FItemsSelector.NextItemIndex(FItemsSelector.ItemIndex);
          UpdateScroller;
        end;
      end
      else if Assigned(FFullWidthSelector) and FFullWidthSelector.Visible and (FFullWidthSelector.ItemIndex >= 0) then
      begin
        if (FFullWidthSelector.ItemIndex = FFullWidthSelector.LastItemIndex) and
           Assigned(FItemsSelector) and FItemsSelector.Visible and (FItemsSelector.RowCount > 0) then
        begin
          FItemsSelector.HotItemIndex := FItemsSelector.FirstItemIndex;
          UpdateScroller;
          FFullWidthSelector.HotItemIndex := -1;
        end
        else
        begin
          FFullWidthSelector.HotItemIndex := FFullWidthSelector.NextItemIndex(FFullWidthSelector.ItemIndex);
        end;
      end
      else if Assigned(FItemsSelector) and FItemsSelector.Visible then
      begin
        FItemsSelector.HotItemIndex := FItemsSelector.FirstItemIndex;
        UpdateScroller;
      end
      else if Assigned(FFullWidthSelector) and FFullWidthSelector.Visible then
      begin
        FFullWidthSelector.HotItemIndex := FFullWidthSelector.FirstItemIndex;
      end;
    end;
    VK_UP:
    begin
      if Assigned(FItemsSelector) and FItemsSelector.Visible and (FItemsSelector.HotItemIndex >= 0) then
      begin
        if (FItemsSelector.GetItemRow(FItemsSelector.Items.Items[FItemsSelector.HotItemIndex]) = 1) and
           Assigned(FFullWidthSelector) and FFullWidthSelector.Visible and (FFullWidthSelector.RowCount > 0) then
        begin
          FFullWidthSelector.HotItemIndex := FFullWidthSelector.LastItemIndex;
          FItemsSelector.HotItemIndex := -1;
        end
        else
        begin
          FItemsSelector.HotItemIndex := FItemsSelector.UpItemIndex(FItemsSelector.HotItemIndex);
          UpdateScroller;
        end;
      end
      else if Assigned(FFullWidthSelector) and FFullWidthSelector.Visible and (FFullWidthSelector.HotItemIndex >= 0) then
      begin
        if (FFullWidthSelector.GetItemRow(FFullWidthSelector.Items.Items[FFullWidthSelector.HotItemIndex]) = 1) and
           Assigned(FItemsSelector) and FItemsSelector.Visible and (FItemsSelector.RowCount > 0) then
        begin
          FItemsSelector.HotItemIndex := FItemsSelector.LastItemIndex;
          UpdateScroller;
          FFullWidthSelector.HotItemIndex := -1;
        end
        else
        begin
          FFullWidthSelector.HotItemIndex := FFullWidthSelector.PreviousItemIndex(FFullWidthSelector.HotItemIndex);
        end;
      end
      else if Assigned(FItemsSelector) and FItemsSelector.Visible and (FItemsSelector.ItemIndex >= 0) then
      begin
        if (FItemsSelector.GetItemRow(FItemsSelector.Items.Items[FItemsSelector.ItemIndex]) = 1) and
           Assigned(FFullWidthSelector) and FFullWidthSelector.Visible and (FFullWidthSelector.RowCount > 0) then
        begin
          FFullWidthSelector.HotItemIndex := FFullWidthSelector.LastItemIndex;
          FItemsSelector.HotItemIndex := -1;
        end
        else
        begin
          FItemsSelector.HotItemIndex := FItemsSelector.UpItemIndex(FItemsSelector.ItemIndex);
          UpdateScroller;
        end;
      end
      else if Assigned(FFullWidthSelector) and FFullWidthSelector.Visible and (FFullWidthSelector.ItemIndex >= 0) then
      begin
        if (FFullWidthSelector.GetItemRow(FFullWidthSelector.Items.Items[FFullWidthSelector.HotItemIndex]) = 1) and
           Assigned(FItemsSelector) and FItemsSelector.Visible and (FItemsSelector.RowCount > 0) then
        begin
          FItemsSelector.HotItemIndex := FItemsSelector.LastItemIndex;
          UpdateScroller;
          FFullWidthSelector.HotItemIndex := -1;
        end
        else
        begin
          FFullWidthSelector.HotItemIndex := FFullWidthSelector.PreviousItemIndex(FFullWidthSelector.ItemIndex);
        end;
      end
      else if Assigned(FItemsSelector) and FItemsSelector.Visible then
      begin
        FItemsSelector.HotItemIndex := FItemsSelector.FirstItemIndex;
        UpdateScroller;
      end
      else if Assigned(FFullWidthSelector) and FFullWidthSelector.Visible then
      begin
        FFullWidthSelector.HotItemIndex := FFullWidthSelector.FirstItemIndex;
      end;
    end;
    VK_DOWN:
    begin
      if Assigned(FItemsSelector) and FItemsSelector.Visible and (FItemsSelector.HotItemIndex >= 0) then
      begin
        if (FItemsSelector.GetItemRow(FItemsSelector.Items.Items[FItemsSelector.HotItemIndex]) = FItemsSelector.RowCount) and
           Assigned(FFullWidthSelector) and FFullWidthSelector.Visible and (FFullWidthSelector.RowCount > 0) then
        begin
          FFullWidthSelector.HotItemIndex := FFullWidthSelector.FirstItemIndex;
          FItemsSelector.HotItemIndex := -1;
        end
        else
        begin
          FItemsSelector.HotItemIndex := FItemsSelector.DownItemIndex(FItemsSelector.HotItemIndex);
          UpdateScroller;
        end;
      end
      else if Assigned(FFullWidthSelector) and FFullWidthSelector.Visible and (FFullWidthSelector.HotItemIndex >= 0) then
      begin
        if (FFullWidthSelector.GetItemRow(FFullWidthSelector.Items.Items[FFullWidthSelector.HotItemIndex]) = FFullWidthSelector.RowCount) and
           Assigned(FItemsSelector) and FItemsSelector.Visible and (FItemsSelector.RowCount > 0) then
        begin
          FItemsSelector.HotItemIndex := FItemsSelector.FirstItemIndex;
          UpdateScroller;
          FFullWidthSelector.HotItemIndex := -1;
        end
        else
        begin
          FFullWidthSelector.HotItemIndex := FFullWidthSelector.NextItemIndex(FFullWidthSelector.HotItemIndex);
        end;
      end
      else if Assigned(FItemsSelector) and FItemsSelector.Visible and (FItemsSelector.ItemIndex >= 0) then
      begin
        if (FItemsSelector.GetItemRow(FItemsSelector.Items.Items[FItemsSelector.ItemIndex]) = FItemsSelector.RowCount) and
           Assigned(FFullWidthSelector) and FFullWidthSelector.Visible and (FFullWidthSelector.RowCount > 0) then
        begin
          FFullWidthSelector.HotItemIndex := FFullWidthSelector.FirstItemIndex;
          FItemsSelector.HotItemIndex := -1;
        end
        else
        begin
          FItemsSelector.HotItemIndex := FItemsSelector.DownItemIndex(FItemsSelector.ItemIndex);
          UpdateScroller;
        end;
      end
      else if Assigned(FFullWidthSelector) and FFullWidthSelector.Visible and (FFullWidthSelector.ItemIndex >= 0) then
      begin
        if (FFullWidthSelector.GetItemRow(FFullWidthSelector.Items.Items[FFullWidthSelector.ItemIndex]) = FFullWidthSelector.RowCount) and
           Assigned(FItemsSelector) and FItemsSelector.Visible and (FItemsSelector.RowCount > 0) then
        begin
          FItemsSelector.HotItemIndex := FItemsSelector.FirstItemIndex;
          UpdateScroller;
          FFullWidthSelector.HotItemIndex := -1;
        end
        else
        begin
          FFullWidthSelector.HotItemIndex := FFullWidthSelector.NextItemIndex(FFullWidthSelector.ItemIndex);
        end;
      end
      else if Assigned(FItemsSelector) and FItemsSelector.Visible then
      begin
        FItemsSelector.HotItemIndex := FItemsSelector.FirstItemIndex;
        UpdateScroller;
      end
      else if Assigned(FFullWidthSelector) and FFullWidthSelector.Visible then
      begin
        FFullWidthSelector.HotItemIndex := FFullWidthSelector.FirstItemIndex;
      end;
    end;
    VK_RETURN, VK_SPACE:
    begin
      if Assigned(FItemsSelector) and FItemsSelector.Visible and (FItemsSelector.HotItemIndex >= 0) then
      begin
        FItemsSelector.ItemIndex := FItemsSelector.HotItemIndex;
        FItemsSelector.DoSelect;
        if Visible then
          Hide;
      end;
      if Assigned(FFullWidthSelector) and FFullWidthSelector.Visible and (FFullWidthSelector.HotItemIndex >= 0) then
      begin
        FFullWidthSelector.ItemIndex := FFullWidthSelector.HotItemIndex;
        FFullWidthSelector.DoSelect;
        if Visible then
          Hide;
      end;
    end;
    VK_HOME:
    begin
      if Assigned(FItemsSelector) and FItemsSelector.Visible then
      begin
        FItemsSelector.HotItemIndex := FItemsSelector.FirstItemIndex;
        UpdateScroller;
        if Assigned(FFullWidthSelector) and FFullWidthSelector.Visible then
          FFullWidthSelector.HotItemIndex := -1;
      end
      else if Assigned(FFullWidthSelector) and FFullWidthSelector.Visible then
      begin
        FFullWidthSelector.HotItemIndex := FFullWidthSelector.FirstItemIndex;
        if Assigned(FItemsSelector) and FItemsSelector.Visible then
          FItemsSelector.HotItemIndex := -1;
      end;
    end;
    VK_END:
    begin
      if Assigned(FFullWidthSelector) and FFullWidthSelector.Visible then
      begin
        FFullWidthSelector.HotItemIndex := FFullWidthSelector.FirstItemIndex;
        if Assigned(FItemsSelector) and FItemsSelector.Visible then
          FItemsSelector.HotItemIndex := -1;
      end
      else if Assigned(FItemsSelector) and FItemsSelector.Visible then
      begin
        FItemsSelector.HotItemIndex := FItemsSelector.FirstItemIndex;
        UpdateScroller;
        if Assigned(FFullWidthSelector) and FFullWidthSelector.Visible then
          FFullWidthSelector.HotItemIndex := -1;
      end;
    end;
    DROPDOWN_KEY:
    begin
      if (GetKeystate(VK_MENU) and $8000 = $0) and Visible then
        Hide;
    end;

    VK_ESCAPE: if Visible then
                 Hide;
  end;
end;

//------------------------------------------------------------------------------

procedure TScrollSelectorDropDownWindow.InvalidateCaption;
var
  R: TRect;
begin
  if HandleAllocated then
  begin
    R := GetCaptionRect;
    R.Right := R.Right + 1;
    R.Bottom := R.Bottom + 1;
    InvalidateRect(Handle, @R, True);
  end;
end;

//------------------------------------------------------------------------------

procedure TScrollSelectorDropDownWindow.InvalidateResizer;
var
  R: TRect;
begin
  if HandleAllocated then
  begin
    R := GetResizerRect;
    R.Right := R.Right + 1;
    R.Bottom := R.Bottom + 1;
    InvalidateRect(Handle, @R, True);
  end;  
end;

//------------------------------------------------------------------------------

{ TSimpleGradientCaption }

procedure TSimpleGradientCaption.Assign(Source: TPersistent);
begin
  inherited;
  if (Source is TSimpleGradientCaption) then
  begin
    TextColor := TSimpleGradientCaption(Source).TextColor;
  end;
end;

//------------------------------------------------------------------------------

constructor TSimpleGradientCaption.Create;
begin
  inherited;
  FColor := RGB(42, 102, 201);
  FColorTo := clNone;
  FTextColor := clWhite;
end;

//------------------------------------------------------------------------------

destructor TSimpleGradientCaption.Destroy;
begin
  inherited;
end;

//------------------------------------------------------------------------------

{ TSelectorButtonSize }

constructor TSelectorButtonSize.Create;
begin
  inherited;
  FHeight:= 0;
  FWidth := 0;
end;

//------------------------------------------------------------------------------

procedure TSelectorButtonSize.Assign(Source: TPersistent);
begin
  if (Source is TSelectorButtonSize) then
  begin
    Height := TSelectorButtonSize(Source).Height;
    Width := TSelectorButtonSize(Source).Width;
  end
  else
    inherited Assign(Source);
end;

//------------------------------------------------------------------------------

procedure TSelectorButtonSize.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

//------------------------------------------------------------------------------

procedure TSelectorButtonSize.SetHeight(const Value: Integer);
begin
  if (FHeight <> Value) then
  begin
    FHeight := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TSelectorButtonSize.SetWidth(const Value: Integer);
begin
  if (FWidth <> Value) then
  begin
    FWidth := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

{ TAdvColorSelectorActionLink }

procedure TAdvColorSelectorActionLink.UpdateColor(AColor: TColor);
begin
  TAdvOfficeColorSelector(FClient).SelectedColor  := AColor;
end;

//------------------------------------------------------------------------------

{ TAdvToolSelectorActionLink }

procedure TAdvToolSelectorActionLink.UpdateSelectedIndex(AIndex: integer);
begin
  TAdvOfficeToolSelector(FClient).SelectedIndex := AIndex;
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
