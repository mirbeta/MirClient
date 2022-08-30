{***************************************************************************}
{ TInspectorBar component                                                   }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2001 - 2015                                        }
{            Email : info@tmssoftware.com                                   }
{            Web : http://www.tmssoftware.com                               }
{                                                                           }
{ The source code is given as is. The author is not responsible             }
{ for any possible damage done due to the use of this code.                 }
{ The component can be freely used in any application. The complete         }
{ source code remains property of the author and may not be distributed,    }
{ published, given or sold in any form as such. No parts of the source      }
{ code can be included in any other component or application without        }
{ written authorization of TMS software.                                    }
{***************************************************************************}

{$I TMSDEFS.INC}

unit InspectorBar;

{$R INSPECTORBAR.RES}

interface

uses
  Classes, Windows, Controls, ExtCtrls, Graphics, StdCtrls, Messages, Dialogs,
  Sysutils, Forms, Mask, ComCtrls, CommCtrl, ComObj, ActiveX, Menus,
  InspEdits, AdvStyleIF, InspXPVS, InspDD, InspImg, JPEG, Types
  {$IFDEF DELPHI_UNICODE}
  , Character
  {$ENDIF}
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 8; // Minor version nr.
  REL_VER = 1; // Release nr.
  BLD_VER = 1; // Build nr.
  DATE_VER = 'Oct, 2015'; // Month version

  // version history
  // 1.3.1.2 : fix for negative sign entry in property inplace editors
  // 1.4.0.0 : Added Style property
  //         : Added IconSize property
  // 1.4.0.1 : Fixed issue with ShowFocus on checkbox items
  //         : Fixed double OnEditUpdate event for checkboxes
  // 1.4.0.2 : Fixed issue with editing for non visible items
  // 1.4.0.3 : Autoscroll for psButtons InspectorPanel for click on partially visible items
  //         : Autoscroll for psButtons InspectorPanel for setting ItemIndex
  // 1.4.0.4 : Fixed issue with removing of editlinks
  // 1.4.0.5 : Fixed issue with setting Panel.ItemIndex to -1
  // 1.4.0.6 : Fixed memory leak issue
  // 1.5.0.0 : New: Style interface
  //         : New: Office 2007 Luna & Obsidian styles
  // 1.5.1.0 : New: Home,End,Prior,Next keys can be used to navigate panels
  //         : New: Shortcut for item on properties panel starts inplace edit
  // 1.5.1.1 : Fixed issue with use of Panel.IconLargeSize
  // 1.5.1.2 : New: public property EnableRepeatButton added
  // 1.5.2.0 : Improved: runtime created panels automatically get the InspectorBar.Style setting
  // 1.5.2.1 : Changed OnItemDraw handling to allow to just change canvas properties
  // 1.5.2.2 : Fixed : issue with showing EditLink inplace editor for Delphi 2005,2006
  // 1.5.3.0 : New support for Office 2007 silver added
  // 1.5.4.0 : New events OnSetEditText, OnGetEditText added in TInspectorEditLink
  // 1.5.4.1 : Fixed issue in TDBInspectorBar with items not connnected to a DataField
  // 1.5.4.2 : Fixed painting issue with D2007
  // 1.5.4.3 : Fixed combobox dropdown button painting problem
  // 1.5.4.4 : Fixed painting issue with D2007
  // 1.5.4.5 : Fixed issue with INIInspectorBar editing
  // 1.5.4.6 : Fixed issue with OnItemClick for psProperties panel
  // 1.5.4.7 : Fixed issue with click on properties panel with zero items
  // 1.5.4.8 : Improved : ellipsis button drawing in flat mode
  // 1.5.4.9 : Fixed : issue with INIInspectorBar edit updates
  // 1.5.4.10: Fixed : issue with time editor type
  // 1.5.4.11: Fixed : issue with hovering on non active window
  // 1.5.5.0 : New : OnPanelCaptionClick added
  //         : Fixed : issue with Panel Assign() method 
  // 1.5.6.0 : New : OnPanelCaptionRightClick added
  // 1.5.6.1 : Improved : position of images in psSmallIcon panel style
  // 1.5.6.2 : Improved : item hovered painting
  // 1.5.6.3 : Fixed : hovering of items when InspectorBar is on MDI form
  // 1.5.6.4 : Fixed : issue with painting button in down state
  // 1.6.0.0 : New : Windows Vista, Windows 7 and Terminal styles
  // 1.6.0.1 : Fixed : issue with auto repeat on DBInspectorBar navigator
  // 1.6.0.2 : Fixed : issue with GetParentForm with forms hosted on forms
  // 1.6.1.0 : New : event OnEditCheckChange event added
  // 1.6.2.0 : New : InspectorItem.DateTimeFormat property to format date & time
  // 1.6.2.1 : Fixed : Issue with using InspectorBar on frame
  // 1.6.2.2 : Fixed : Issue with adding JPEG images to property panel
  // 1.6.2.3 : Improved : Inplace combobox editor positioning
  // 1.6.3.0 : New : Built in support for Office 2010 colors
  // 1.6.3.1 : Fixed : Small issue with font color in Office 2010 style
  // 1.6.3.2 : Fixed : Issue with hovering on MDI child window
  // 1.6.3.3 : Fixed : Issue with OnStartEdit event triggered twice on line between two items
  // 1.6.3.4 : Improved : Positioning & wordwrap of HTML formatted text in items
  // 1.6.3.5 : Fixed : Issue with inplace date editing when InspectorItem.DateValue is not set
  // 1.6.4.0 : Improved : Buttons for Items with ReadOnly = true are treated as disabled buttons
  // 1.6.4.1 : Fixed : Issue with handling Name property of components in TRTTIInspectorBar
  //         : Improved : Handling of not existing INI files in TINIInspectorBar
  // 1.6.4.2 : Fixed : Issue with OnEditColorChange & custom color picker dialog use
  // 1.6.5.0 : New : Windows 8, Office 2013 styles added
  // 1.6.5.1 : Improved : Behavior in 125% DPI mode
  // 1.6.5.2 : Fixed : Issue with calling StartEdit function from OnEnter event
  // 1.6.6.0 : New : Delphi XE5 & C++Builder XE5 support
  // 1.6.6.1 : Fixed : Issue with persisting boolean values in INIInspectorBar
  // 1.7.0.0 : New : Support for DB lookup fields in TDBInspectorBar
  // 1.7.0.1 : Improved : Display of scrollbar in multi panel state
  // 1.7.0.2 : Fixed : Issue with OnPanelOpen/OnPanelClose events
  // 1.7.0.3 : Improved : Display clNone color value as "None"
  // 1.7.1.0 : New : Added "None" in color selector
  //         : New : Delphi XE7 & C++Builder XE7 support
  // 1.7.2.0 : New : OnEditProp event added
  // 1.7.2.1 : Fixed : Issue with write-only properties in TINIInspectorBar
  // 1.7.3.0 : New : Delphi XE8 & C++Builder XE8 support
  // 1.8.0.0 : New : Windows 10, Office 2016 styles added
  // 1.8.0.1 : Fixed : Issue with handling panel caption clicks in multipanel mode
  // 1.8.1.0 : New : RAD Studio 10 Seattle support
  // 1.8.1.1 : Fixed : Issue with hover on transparent PNG based imagelist images in item


  // theme changed notifier
  {$IFDEF DELPHI2006_LVL}
  {$EXTERNALSYM WM_THEMECHANGED}
  {$ENDIF}
  WM_THEMECHANGED = $031A;

type
  {$IFDEF DELPHI_UNICODE}
  THintInfo = Controls.THintInfo;
  PHintInfo = Controls.PHintInfo;
  {$ENDIF}

  TInspectorBar = class;
  TInspectorItem = class;
  TInspectorPanel = class;
  TShadeType = (stNormal, stNoise, stDiagShade, stHShade, stVShade, stHBump, stVBump,
    stSoftBump, stHardBump, stLMetal, stRMetal, stIRadial, stORadial,
    stHShadeInv, stVShadeInv, stXPButton, stBitmap, stBitmapRStretch, stBitmapLStretch);

  // to be extended property types ...
  TPropertyType = (ptInteger, ptFloat, ptText, ptColor, ptFont, ptBoolean, ptValues,
    ptIntSpin, ptTextButton, ptPropButton, ptDate, ptTime, ptCustom, ptValuesList, ptFixedColor, ptButton, ptPassword, ptPicture);

  TVAlignment = (vaTop, vaCenter, vaBottom);

  TCaptionShape = (csRectangle, csRounded, csSemiRounded);

  TOpenClosePosition = (ocpLeft, ocpRight);
  TOpenCloseGraphic = (ocgNone, ocgCross, ocgTriangle, ocgGlyph);

  TInspectorItemEvent = procedure(Sender: TObject;
    AInspectorPanel: TInspectorPanel; AInspectorItem: TInspectorItem) of object;

  TInspectorItemAnchorEvent = procedure(Sender: TObject;
    AInspectorPanel: TInspectorPanel; AInspectorItem: TInspectorItem; Anchor: string) of object;

  TInspectorItemEditEvent = procedure(Sender: TObject;
    AInspectorPanel: TInspectorPanel; AInspectorItem: TInspectorItem; var Value: string) of object;

  TInspectorItemCheckEvent = procedure(Sender: TObject;
    AInspectorPanel: TInspectorPanel; AInspectorItem: TInspectorItem; Value: boolean) of object;

  TInspectorItemComboEvent = procedure(Sender: TObject;
    AInspectorPanel: TInspectorPanel; AInspectorItem: TInspectorItem; Index: Integer) of object;

  TInspectorItemColorEvent = procedure(Sender: TObject;
    AInspectorPanel: TInspectorPanel; AInspectorItem: TInspectorItem; AColor: TColor) of object;

  TInspectorPanelEvent = procedure(Sender: TObject;
    AInspectorPanel: TInspectorPanel) of object;

  TInspectorURLEvent = procedure(Sender: TObject; URL: string;
    var DefaultHandler: Boolean) of object;

  TInspectorFileDropEvent = procedure(Sender: TObject; FileName: string;
    var DefaultHandler: Boolean) of object;

  TPanelDrawEvent = procedure(Sender: TObject; AInspectorPanel: TInspectorPanel;
    ACanvas: TCanvas; ARect: TRect; var DefaultDraw: Boolean) of object;

  TItemDrawEvent = procedure(Sender: TObject; AInspectorItem: TInspectorItem;
    ACanvas: TCanvas; ARect: TRect; var DefaultDraw: Boolean) of object;

  TGetValueListEvent = procedure(Sender: TObject; AInspectorItem: TInspectorItem;
    ValueList: TStringList) of object;

  TInspectorEditButton = (ebDropDown, ebMore, ebSpin, ebNone);

  TCustomEditButtonEvent = procedure(Sender: TObject; AInspectorItem: TInspectorItem;
    var AEditButton: TInspectorEditButton) of object;

  TCustomEditDrawEvent = procedure(Sender: TObject; AInspectorItem: TInspectorItem;
    Canvas: TCanvas; ARect: TRect; var DefaultDraw: Boolean) of object;

  THelpAnchorEvent = procedure(Sender: TObject; Anchor: string) of object;

  TStartLabelEdit = procedure(Sender: TObject; var text: string) of object;
  TStopLabelEdit = procedure(Sender: TObject; OldText: string; var NewText: string; var accept: boolean) of object;

  TItemValueEvent = procedure(sender: TObject; AInspectorPanel: TInspectorPanel; AInspectorItem: TInspectorItem; var Value: string) of object;

  TInspectorEditType = (ieText, ieInteger, ieFloat);

  TInspectorBarStyle = (esOffice2003Blue, esOffice2003Silver, esOffice2003Olive, esOffice2003Classic, esOffice2007Luna, esOffice2007Obsidian, esWindowsXP, esWhidbey, esCustom, esOffice2007Silver, esWindowsVista, esWindows7, esTerminal, esOffice2010Blue, esOffice2010Silver, esOffice2010Black,
    esWindows8, esOffice2013White, esOffice2013LightGray, esOffice2013Gray,
    esWindows10, esOffice2016White, esOffice2016Gray, esOffice2016Black );

  XPColorScheme = (xpNone, xpBlue, xpGreen, xpGray);


  TInspectorEdit = class(TMaskEdit)
  private
    FInspectorItem: TInspectorItem;
    FOrigValue: string;
    FInspEditType: TInspectorEditType;
    FMultiLine: Boolean;
    procedure SetMultiLine(const Value: Boolean);
  protected
    procedure DoExit; override;
    procedure KeyPress(var Key: Char); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure CreateParams(var Params: TCreateParams); override;
  published
    property OrigValue: string read FOrigValue write FOrigValue;
    property InspectorItem: TInspectorItem read FInspectorItem write FInspectorItem;
    property InspEditType: TInspectorEditType read FInspEditType write FInspEditType;
    property MultiLine: Boolean read FMultiLine write SetMultiLine;
  end;

  TInspectorCombo = class(TInspComboBox)
  private
    FInspectorItem: TInspectorItem;
    FOrigValue: string;
  protected
    procedure DoExit; override;
    procedure KeyPress(var Key: Char); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property OrigValue: string read FOrigValue write FOrigValue;
    property InspectorItem: TInspectorItem read FInspectorItem write FInspectorItem;
  end;

  TInspectorColorCombo = class(TInspColorComboBox)
  private
    FInspectorItem: TInspectorItem;
    FOrigValue: TColor;
  protected
    procedure DoExit; override;
    procedure KeyPress(var Key: Char); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property OrigValue: TColor read FOrigValue write FOrigValue;
    property InspectorItem: TInspectorItem read FInspectorItem write FInspectorItem;
  end;

  TInspectorSpin = class(TInspSpinEdit)
  private
    FInspectorItem: TInspectorItem;
    FOrigValue: string;
  protected
    procedure DoExit; override;
    procedure KeyPress(var Key: Char); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure CreateParams(var Params: TCreateParams); override;
  published
    property OrigValue: string read FOrigValue write FOrigValue;
    property InspectorItem: TInspectorItem read FInspectorItem write FInspectorItem;
  end;

  TInspectorEditBtn = class(TInspEditBtn)
  private
    FInspectorItem: TInspectorItem;
    FOrigValue: string;
  protected
    procedure DoExit; override;
    procedure KeyPress(var Key: Char); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure CreateParams(var Params: TCreateParams); override;
  published
    property OrigValue: string read FOrigValue write FOrigValue;
    property InspectorItem: TInspectorItem read FInspectorItem write FInspectorItem;
  end;

  TInspectorDateTimePicker = class(TInspDateTimePicker)
  private
    FInspectorItem: TInspectorItem;
    FOrigValue: TDateTime;
  protected
    procedure DoExit; override;
    procedure KeyPress(var Key: Char); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure CreateParams(var Params: TCreateParams); override;
  published
    property OrigValue: TDateTime read FOrigValue write FOrigValue;
    property InspectorItem: TInspectorItem read FInspectorItem write FInspectorItem;
  end;

  TInspectorCaption = class(TPersistent)
  private
    FHeight: Integer;
    FShape: TCaptionShape;
    FOnChange: TNotifyEvent;
    FAlignment: TAlignment;
    FFont: TFont;
    FVAlignment: TVAlignment;
    FColor: TColor;
    FShadeType: TShadeType; // icon shade type field
    FShadeLight: Byte; // icon shade light field values: 200 - 255
    FShadeGrain: Byte;
    FOnShadeChange: TNotifyEvent;
    FGlyphClose: TBitmap;
    FGlyphOpen: TBitmap;
    FCursor: TCursor;
    FBarCursor: TCursor;
    FButton: Boolean;
    FOpenCloseGraphic: TOpenCloseGraphic;
    FOpenClosePosition: TOpenClosePosition;
    FFlat: Boolean;
    FBackground: TBitmap;
    FActiveBackground: TBitmap;
    FActiveFont: TFont;
    FIndent: Integer;
    FSideDisplay: Boolean;
    FSideWidth: Integer;
    FUnderLine: Boolean;
    FUnderlineWidth: Integer;
    FUnderlineColor: TColor;
    FInspectorBar: TInspectorBar;
    FColorTo: TColor;
    procedure SetHeight(const Value: Integer);
    procedure SetShape(const Value: TCaptionShape);
    procedure SetAlignment(const Value: TAlignment);
    procedure SetFont(const Value: TFont);
    procedure SetVAlignment(const Value: TVAlignment);
    procedure SetColor(const Value: TColor);
    procedure SetShadeGrain(const Value: Byte);
    procedure SetShadeLight(const Value: Byte);
    procedure SetShadeType(const Value: TShadeType);
    procedure SetGlyphClose(const Value: TBitmap);
    procedure SetGlyphOpen(const Value: TBitmap);
    procedure SetCursor(const Value: TCursor);
    procedure SetOpenCloseGraphic(const Value: TOpenCloseGraphic);
    procedure SetOpenClosePosition(const Value: TOpenClosePosition);
    procedure SetFlat(const Value: Boolean);
    procedure SetActiveBackground(const Value: TBitmap);
    procedure SetBackground(const Value: TBitmap);
    procedure SetActiveFont(const Value: TFont);
    procedure SetIndent(const Value: Integer);
    procedure SetSideDisplay(const Value: Boolean);
    procedure SetSideWidth(const Value: Integer);
    procedure SetUnderLine(const Value: Boolean);
    procedure SetUnderlineColor(const Value: TColor);
    procedure SetUnderlineWidth(const Value: Integer);
    procedure SetInspectorBar(const Value: TInspectorBar);
    procedure SetColorTo(const Value: TColor);
  protected
    procedure Changed;
    procedure ShadeChanged;
    procedure FontChanged(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnShadeChange: TNotifyEvent read FOnShadeChange write FOnShadeChange;
    property InspectorBar: TInspectorBar read FInspectorBar write SetInspectorBar;
  published
    property ActiveBackground: TBitmap read FActiveBackground write SetActiveBackground;
    property ActiveFont: TFont read FActiveFont write SetActiveFont;
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property Background: TBitmap read FBackground write SetBackground;
    property Button: Boolean read FButton write FButton;
    property Color: TColor read FColor write SetColor;
    property ColorTo: TColor read FColorTo write SetColorTo default clNone;
    property Cursor: TCursor read FCursor write SetCursor default crDefault;
    property Flat: Boolean read FFlat write SetFlat default False;
    property Font: TFont read FFont write SetFont;
    property SideDisplay: Boolean read FSideDisplay write SetSideDisplay;
    property SideWidth: Integer read FSideWidth write SetSideWidth;
    property GlyphOpen: TBitmap read FGlyphOpen write SetGlyphOpen;
    property GlyphClose: TBitmap read FGlyphClose write SetGlyphClose;
    property Height: Integer read FHeight write SetHeight default 20;
    property Indent: Integer read FIndent write SetIndent default 0;
    property OpenClosePosition: TOpenClosePosition read FOpenClosePosition write SetOpenClosePosition;
    property OpenCloseGraphic: TOpenCloseGraphic read FOpenCloseGraphic write SetOpenCloseGraphic;
    property ShadeGrain: Byte read FShadeGrain write SetShadeGrain;
    property ShadeType: TShadeType read FShadeType write SetShadeType;
    property ShadeLight: Byte read FShadeLight write SetShadeLight default 255;
    property Shape: TCaptionShape read FShape write SetShape default csRectangle;
    property Underline: Boolean read FUnderLine write SetUnderLine default False;
    property UnderlineColor: TColor read FUnderlineColor write SetUnderlineColor default clBlue;
    property UnderlineWidth: Integer read FUnderlineWidth write SetUnderlineWidth default 1;
    property VAlignment: TVAlignment read FVAlignment write SetVAlignment;
  end;

  TInspectorEditStyle = (esInplace, esPopup);

  TInspSetPropertiesEvent = procedure(Sender: TObject; R: TRect; Item: TInspectorItem) of object;

  TSetEditTextEvent = procedure(Sender: TObject; Item: TInspectorItem; var AText: string) of object;
  TGetEditTextEvent = procedure(Sender: TObject; Item: TInspectorItem; var AText: string) of object;

  TInspectorEditLink = class(TComponent)
  private
    FWantKeyUpDown: Boolean;
    FWantKeyPriorNext: Boolean;
    FWantKeyHomeEnd: Boolean;
    FWantKeyLeftRight: Boolean;
    FWantKeyEscape: Boolean;
    FWantKeyReturn: Boolean;
    FPopupWidth: Integer;
    FPopupHeight: Integer;
    FTag: Integer;
    FEditStyle: TInspectorEditStyle;
    FInspector: TInspectorBar;
    FPopupForm: TForm;
    FOnSetProperties: TInspSetPropertiesEvent;
    FOnSetEditText: TSetEditTextEvent;
    FOnGetEditText: TGetEditTextEvent;
  protected
  public
    procedure EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditExit(Sender: TObject);
    procedure FormExit(Sender: TObject);
    function GetEditor: TWinControl; virtual;
    procedure CreateEditor(AParent: TWinControl); virtual;
    procedure DestroyEditor; virtual;
    procedure SetProperties(R: TRect; Item: TInspectorItem); virtual;
    procedure StartEdit(Item: TInspectorItem); virtual;
    procedure StopEdit(Item: TInspectorItem); virtual;
    function GetEditorValue(s: string): string; virtual;
    procedure SetEditorValue(var s: string); virtual;
    procedure SetOriginalValue; virtual;
    property Inspector: TInspectorBar read FInspector write FInspector;
  published
    property EditStyle: TInspectorEditStyle read FEditStyle write FEditStyle;
    property PopupWidth: Integer read FPopupWidth write FPopupWidth;
    property PopupHeight: Integer read FPopupHeight write FPopupHeight;
    property WantKeyLeftRight: Boolean read FWantKeyLeftRight write FWantKeyLeftRight;
    property WantKeyUpDown: Boolean read FWantKeyUpDown write FWantKeyUpDown;
    property WantKeyHomeEnd: Boolean read FWantKeyHomeEnd write FWantKeyHomeEnd;
    property WantKeyPriorNext: Boolean read FWantKeyPriorNext write FWantKeyPriorNext;
    property WantKeyReturn: Boolean read FWantKeyReturn write FWantKeyReturn;
    property WantKeyEscape: Boolean read FWantKeyEscape write FWantKeyEscape;
    property Tag: Integer read FTag write FTag;
    property OnSetProperties: TInspSetPropertiesEvent read FOnSetProperties write FOnSetProperties;
    property OnGetEditText: TGetEditTextEvent read FOnGetEditText write FOnGetEditText;
    property OnSetEditText: TSetEditTextEvent read FOnSetEditText write FOnSetEditText;
  end;

  TItemNodeState = (nsOpen, nsClose);

  TInspBackgroundPos = (bpTopLeft, bpBottomLeft, bpTopRight, bpBottomRight);

  TInspectorItem = class(TCollectionItem)
  private
    FCaption: string;
    FIcon: TIcon;
    FVisible: Boolean;
    FIntValue: Integer;
    FTextValue: string;
    FHeight: integer;
    FEditMask: string;
    FValues: TStringList;
    FPropertyType: TPropertyType;
    FBoolValue: Boolean;
    FHint: string;
    FImageIndex: Integer;
    FShortCut: TShortCut;
    FTag: Integer;
    FReadOnly: Boolean;
    FDateTimeValue: TDateTime;
    FDateTime: TDateTime;
    FOwnsObject: Boolean;
    FObject: TObject;
    FAutoIcon: Boolean;
    FURL: string;
    FColorValue: TColor;
    FFontValue: TFont;
    FSpinMax: Integer;
    FSpinMin: Integer;
    FAutoIconIndex: Integer;
    FBackground: TInspImage;
    FBkgPos: TInspBackgroundPos;
    FEditLink: TInspectorEditLink;
    FDown: Boolean;
    FEditing: Boolean;
    FModified: Boolean;
    FIndent: Integer;
    FHelp: string;
    FLevel: Integer;
    FNodeState: TItemNodeState;
    FHotAnchor: string;
    FItemFocused: Boolean;
    Flocalrect: Trect;
    FSelected: Boolean;
    FBitmap: TBitmap;
    FPictureValue: TPicture;
    FMaxLength: Integer;
    FDateTimeFormat: string;
    procedure SetCaption(const Value: string);
    procedure SetIcon(const Value: TIcon);
    function GetIcon: TIcon;
    procedure SetIntValue(const Value: Integer);
    procedure SetTextValue(const Value: string);
    procedure SetValues(const Value: TStringList);
    procedure SetPropertyType(const Value: TPropertyType);
    procedure SetBoolValue(const Value: Boolean);
    procedure SetImageIndex(const Value: Integer);
    function GetInspectorBar: TInspectorBar;
    function GetInspectorPanel: TInspectorPanel;
    procedure SetDateTimeValue(const Value: TDateTime);
    procedure SetDateValue(const Value: TDateTime);
    procedure SetTimeValue(const Value: TDateTime);
    procedure SetAutoIcon(const Value: Boolean);
    procedure SetURL(const Value: string);
    procedure SetColorValue(const Value: TColor);
    procedure SetFontValue(const Value: TFont);
    procedure SetAutoIconIndex(const Value: Integer);
    procedure SetBackground(const Value: TInspImage);
    procedure SetBackgroundPos(const Value: TInspBackgroundPos);
    procedure SetDown(const Value: Boolean);
    procedure SetHeight(const Value: integer);
    procedure SetVisible(const Value: Boolean);
    procedure SetModified(const Value: Boolean);
    procedure SetIndent(const Value: Integer);
    procedure SetLevel(const Value: Integer);
    procedure SetNodeState(const Value: TItemNodeState);
    procedure SetItemFocused(const Value: Boolean);
    procedure SetShortCut(const Value: TShortCut);
    procedure SetSelected(const Value: Boolean);
    procedure SetBitmap(const Value: TBitmap);
    procedure SetPictureValue(const Value: TPicture);
    procedure SetMaxLength(const Value: Integer);
    procedure SetDateTimeFormat(const Value: string);
  protected
    procedure Changed;
    function GetDisplayName: string; override;
    procedure EditStart; virtual;
    procedure EditStop; virtual;
    procedure EditChange; virtual;
    procedure BkgChanged(Sender: TObject);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function NumChilds: Integer;
    procedure ExpandChilds;
    procedure CollapsChilds;
    property InspectorPanel: TInspectorPanel read GetInspectorPanel;
    property InspectorBar: TInspectorBar read GetInspectorBar;
    property DateTimeValue: TDateTime read FDateTimeValue write SetDateTimeValue;
    property DateValue: TDateTime read FDateTime write SetDateValue;
    property TimeValue: TDateTime read FDateTime write SetTimeValue;
    property OwnsObject: Boolean read FOwnsObject write FOwnsObject;
    property ItemObject: TObject read FObject write FObject;
    property Down: Boolean read FDown write SetDown;
    property Editing: Boolean read FEditing;
    property NodeState: TItemNodeState read FNodeState write SetNodeState;
    property HotAnchor: string read FHotAnchor write FHotAnchor;
    property ItemFocused: Boolean read FItemFocused write SetItemFocused;
    property Selected: Boolean read FSelected write SetSelected;
    procedure Update; virtual;
    procedure DoEdit; virtual;
  published
    property AutoIcon: Boolean read FAutoIcon write SetAutoIcon default False;
    property AutoIconIndex: Integer read FAutoIconIndex write SetAutoIconIndex default 0;
    property Background: TInspImage read FBackground write SetBackground;
    property BackgroundPosition: TInspBackgroundPos read FBkgPos write SetBackgroundPos default bpBottomRight;
    property Bitmap: TBitmap read FBitmap write SetBitmap;
    property BoolValue: Boolean read FBoolValue write SetBoolValue;
    property Caption: string read FCaption write SetCaption;
    property ColorValue: TColor read FColorValue write SetColorValue;
    property DateTimeFormat: string read FDateTimeFormat write SetDateTimeFormat;
    property EditLink: TInspectorEditLink read FEditLink write FEditLink;
    property EditMask: string read FEditMask write FEditMask;
    property FontValue: TFont read FFontValue write SetFontValue;
    property Help: string read FHelp write FHelp;
    property Hint: string read FHint write FHint;
    property Height: integer read FHeight write SetHeight;
    property Icon: TIcon read GetIcon write SetIcon;
    property ImageIndex: Integer read FImageIndex write SetImageIndex default -1;
    property Indent: Integer read FIndent write SetIndent;
    property IntValue: Integer read FIntValue write SetIntValue;
    property Level: Integer read FLevel write SetLevel;
    property MaxLength: Integer read FMaxLength write SetMaxLength default 0;
    property Modified: Boolean read FModified write SetModified;
    property PictureValue: TPicture read FPictureValue write SetPictureValue;
    property PropertyType: TPropertyType read FPropertyType write SetPropertyType;
    property ReadOnly: Boolean read FReadOnly write FReadOnly;
    property ShortCut: TShortCut read FShortCut write SetShortCut;
    property SpinMin: Integer read FSpinMin write FSpinMin default 0;
    property SpinMax: Integer read FSpinMax write FSpinMax default 100;
    property Tag: Integer read FTag write FTag default 0;
    property TextValue: string read FTextValue write SetTextValue;
    property Values: TStringList read FValues write SetValues;
    property Visible: Boolean read FVisible write SetVisible default True;
    property URL: string read FURL write SetURL;
  end;

  TInspectorItems = class(TOwnedCollection)
  private
    FOwner: TInspectorPanel;
    function GetItem(Index: Integer): TInspectorItem;
    procedure SetItem(Index: Integer; const Value: TInspectorItem);
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    property InspectorPanel: TInspectorPanel read FOwner;
    function CreateItemClass: TCollectionItemClass; virtual;
    constructor Create(AOwner: TInspectorPanel);
    function Add: TInspectorItem;
    function Insert(index: Integer): TInspectorItem;
    property Items[Index: Integer]: TInspectorItem read GetItem write SetItem; default;
  published

  end;

  TPanelStyle = (psLargeIcon, psSmallIcon, psProperties, psButtons);
  TInspectorBarMode = (imSinglePanelActive, imMultiPanelActive);

  TPanelBackground = (pbSolid, pbGradient, pbTexture);
  TGradientDirection = (gdVertical, gdHorizontal);

  TMultiPanelState = (mpsOpen, mpsClose, mpsAlwaysOpen);

  TInplaceEditType = (itCombo, itPicker, itBtnEdit, itNormal, itColorCombo);

  TImageAlign = (iaLeft,iaRight);

  TInspectorPanel = class(TCollectionItem)
  private
    FItems: TInspectorItems;
    FTopItem: Integer;
    FCaption: string;
    FColor: TColor;
    FFont: TFont;
    FIsOpen: boolean;
    FVariableItemHeight: Boolean;
    FPanelStyle: TPanelStyle;
    FItemHeight: Integer;
    FGradientStart: TColor;
    FGradientEnd: TColor;
    FBackground: TPanelBackground;
    FGradientDirection: TGradientDirection;
    FGradientSteps: Integer;
    FGridLines: Boolean;
    FGridLineColor: TColor;
    FCaptionWidth: Integer;
    FCaptionColor: TColor;
    FCaptionFont: TFont;
    FWordWrap: Boolean;
    FTexture: TBitmap;
    FEditBox: Boolean;
    FEditColor: TColor;
    FHint: string;
    FImageIndex: Integer;
    FTag: Integer;
    FEditFontColor: TColor;
    FAllowResize: Boolean;
    FHoverIconDownColor: TColor;
    FHoverIconColor: TColor;
    FHover: Boolean;
    FSplitter: TInspImage;
    FButtonDownColor: TColor;
    FAlignment: TAlignment;
    FIndent: Integer;
    FControl: TWinControl;
    FModifiedFont: TFont;
    FShowModified: Boolean;
    FHoverIconBorderColor: TColor;
    FHoverFullWidth: Boolean;
    FPopupMenu: TPopupMenu;
    FEditBorderColor: TColor;
    FMultiPanelState: TMultiPanelState;
    FVisible: Boolean;
    FShortCut: TShortCut;
    FitemIndex: Integer;
    FSelectFontColor: Tcolor;
    FSelectColor: Tcolor;
    FLabelEdit: Boolean;
    FSelectBorderColor: TColor;
    FWordWrapCaption: Boolean;
    FHoverIconColorTo: TColor;
    FHoverIconDownColorTo: TColor;
    FSelectColorTo: TColor;
    FSelectDownColorTo: TColor;
    FSelectDownColor: TColor;
    FIconLargeSize: Integer;
    FImageAlign: TImageAlign;
    procedure SetInspectorItems(const Value: TInspectorItems);
    procedure SetCaption(const Value: string);
    procedure SetColor(const Value: TColor);
    procedure SetFont(const Value: TFont);
    procedure SetItemHeight(const Value: Integer);
    procedure SetGradientEnd(const Value: TColor);
    procedure SetGradientStart(const Value: TColor);
    procedure SetBackground(const Value: TPanelBackground);
    procedure SetGradientDirection(const Value: TGradientDirection);
    procedure SetGradientSteps(const Value: Integer);
    procedure SetPanelStyle(const Value: TPanelStyle);
    procedure SetGridLineColor(const Value: TColor);
    procedure SetGridLines(const Value: Boolean);
    procedure SetCaptionWidth(const Value: Integer);
    function GetTopItem: Integer;
    procedure SetTopItem(const Value: Integer);
    procedure SetWordWrap(const Value: Boolean);
    procedure SetTexture(const Value: TBitmap);
    procedure SetEditBox(const Value: Boolean);
    procedure SetEditColor(const Value: TColor);
    procedure SetImageIndex(const Value: Integer);
    function GetInspectorBar: TInspectorBar;
    procedure SetEditFontColor(const Value: TColor);
    procedure SetSplitter(const Value: TInspImage);
    procedure SetButtonDownColor(const Value: TColor);
    procedure SetAlignment(const Value: TAlignment);
    procedure SetIndent(const Value: Integer);
    procedure SetControl(const Value: TWinControl);
    procedure SetOpen(const Value: Boolean);
    procedure SetModifiedFont(const Value: TFont);
    procedure SetShowModified(const Value: Boolean);
    procedure SetEditBorderColor(const Value: TColor);
    procedure SetMultiPanelState(const Value: TMultiPanelState);
    function GetMultiPanelState: TMultiPanelState;
    procedure SetVariableItemHeight(const Value: Boolean);
    function GetPanelHeight: Integer;
    procedure SetVisible(const Value: Boolean);
    procedure SetShortCut(const Value: TShortCut);
    procedure SetItemIndex(const Value: Integer);
    procedure SetSelectColor(const Value: Tcolor);
    procedure SetSelectFontColor(const Value: Tcolor);
    procedure SetSelectBorderColor(const Value: TColor);
    procedure SetWordWrapCaption(const Value: Boolean);
    procedure SetCaptionColor(const Value: TColor);
    procedure SetCaptionFont(const Value: TFont);
    procedure SetHoverIconColorTo(const Value: TColor);
    procedure SetHoverIconDownColorTo(const Value: TColor);
    procedure SetSelectColorTo(const Value: TColor);
    procedure SetSelectDownColor(const Value: TColor);
    procedure SetSelectDownColorTo(const Value: TColor);
    procedure SetImageAlign(const Value: TImageAlign);
  protected
    procedure FontChanged(Sender: TObject);
    procedure ImgChanged(Sender: TObject);
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function CreateItems: TInspectorItems; virtual;
    property Open: Boolean read FIsOpen write SetOpen;
    property InspectorBar: TInspectorBar read GetInspectorBar;
    procedure ExpandAll;
    procedure CollapsAll;
  published
    property Alignment: TAlignment read FAlignment write SetAlignment default taCenter;
    property AllowResize: Boolean read FAllowResize write FAllowResize;
    property Background: TPanelBackground read FBackground write SetBackground;
    property ButtonDownColor: TColor read FButtonDownColor write SetButtonDownColor;
    property Caption: string read FCaption write SetCaption;
    property CaptionWidth: Integer read FCaptionWidth write SetCaptionWidth;
    property CaptionColor: TColor read FCaptionColor write SetCaptionColor default clBtnFace; // default clNone;
    property CaptionFont: TFont read FCaptionFont write SetCaptionFont;
    property Color: TColor read FColor write SetColor;
    property Control: TWinControl read FControl write SetControl;
    property Font: TFont read FFont write SetFont;
    property EditBorderColor: TColor read FEditBorderColor write SetEditBorderColor;
    property EditBox: Boolean read FEditBox write SetEditBox default False;
    property EditColor: TColor read FEditColor write SetEditColor default clWhite;
    property EditFontColor: TColor read FEditFontColor write SetEditFontColor default clBlack;
    property GradientDirection: TGradientDirection read FGradientDirection write SetGradientDirection;
    property GradientEnd: TColor read FGradientEnd write SetGradientEnd;
    property GradientStart: TColor read FGradientStart write SetGradientStart;
    property GradientSteps: Integer read FGradientSteps write SetGradientSteps;
    property GridLineColor: TColor read FGridLineColor write SetGridLineColor;
    property GridLines: Boolean read FGridLines write SetGridLines;
    property Hint: string read FHint write FHint;
    property HoverCaption: Boolean read FHover write FHover;
    property HoverFullWidth: Boolean read FHoverFullWidth write FHoverFullWidth;
    property HoverIconColor: TColor read FHoverIconColor write FHoverIconColor;
    property HoverIconColorTo: TColor read FHoverIconColorTo write SetHoverIconColorTo default clNone;
    property HoverIconBorderColor: TColor read FHoverIconBorderColor write FHoverIconBorderColor;
    property HoverIconDownColor: TColor read FHoverIconDownColor write FHoverIconDownColor;
    property HoverIconDownColorTo: TColor read FHoverIconDownColorTo write SetHoverIconDownColorTo;
    property IconLargeSize: Integer read FIconLargeSize write FIconLargeSize default 32;
    property ImageAlign: TImageAlign read FImageAlign write SetImageAlign;
    property ImageIndex: Integer read FImageIndex write SetImageIndex default -1;
    property Indent: Integer read FIndent write SetIndent default 0;
    property LabelEdit: Boolean read FLabelEdit write FLabelEdit;
    property PopupMenu: TPopupMenu read FPopupMenu write FPopupMenu;
    property ModifiedFont: TFont read FModifiedFont write SetModifiedFont;
    property MultiPanelState: TMultiPanelState read GetMultiPanelState write SetMultiPanelState;
    property ShortCut: TShortCut read FShortCut write SetShortCut;
    property VariableItemHeight: Boolean read FVariableItemHeight write SetVariableItemHeight;
    property ItemHeight: Integer read FItemHeight write SetItemHeight;
    property Items: TInspectorItems read FItems write SetInspectorItems;
    property ShowModified: Boolean read FShowModified write SetShowModified;
    property Splitter: TInspImage read FSplitter write SetSplitter;
    property Style: TPanelStyle read FPanelStyle write SetPanelStyle;
    property Tag: Integer read FTag write FTag;
    property Texture: TBitmap read FTexture write SetTexture;
    property TopItem: Integer read GetTopItem write SetTopItem;
    property Visible: Boolean read FVisible write SetVisible default true;
    property WordWrap: Boolean read FWordWrap write SetWordWrap;
    property WordWrapCaption: Boolean read FWordWrapCaption write SetWordWrapCaption;
    property ItemIndex: Integer read FitemIndex write SetItemIndex default -1;
    
    property SelectBorderColor: TColor read FSelectBorderColor write SetSelectBorderColor default clBlack;
    property SelectColor: TColor read FSelectColor write SetSelectColor default clNavy;
    property SelectColorTo: TColor read FSelectColorTo write SetSelectColorTo;
    property SelectFontColor: TColor read FSelectFontColor write SetSelectFontColor default clYellow;
    property SelectDownColor: TColor read FSelectDownColor write SetSelectDownColor;
    property SelectDownColorTo: TColor read FSelectDownColorTo write SetSelectDownColorTo default clNone;
  end;

  TInspectorPanels = class(TOwnedCollection)
  private
    FOwner: TInspectorBar;
    function GetItem(Index: Integer): TInspectorPanel;
    procedure SetItem(Index: Integer; const Value: TInspectorPanel);
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    function CreateItemClass: TCollectionItemClass; virtual;
    constructor Create(AOwner: TInspectorBar);
    function Add: TInspectorPanel;
    function Insert(index: Integer): TInspectorPanel;
    property Items[Index: Integer]: TInspectorPanel read GetItem write SetItem; default;
  published
  end;

  TInspectorBarDropTarget = class(TInspectorDropTarget)
  private
    FInspectorBar: TInspectorBar;
  public
    constructor Create(AInspectorBar: TInspectorBar);
    destructor Destroy; override;
    procedure DropText(pt: TPoint; s: string); override;
    procedure DropURL(pt: TPoint; s: string); override;
    procedure DropFiles(pt: TPoint; Files: TStrings); override;
    procedure DragMouseMove(pt: TPoint; var Allow: Boolean; DropFormats: TDropFormats); override;
    procedure DragMouseLeave; override;
  end;

  TInspectorHelp = class(TPersistent)
  private
    FVisible: Boolean;
    FHeight: Integer;
    FColor: TColor;
    FFont: TFont;
    FOnChange: TNotifyEvent;
    FBevelInner: TBevelCut;
    FBevelOuter: TBevelCut;
    FBevelWidth: Integer;
    FText: string;
    FColorTo: TColor;
    procedure SetColor(const Value: TColor);
    procedure SetFont(const Value: TFont);
    procedure SetHeight(const Value: Integer);
    procedure SetVisible(const Value: Boolean);
    procedure Changed;
    procedure SetBevelInner(const Value: TBevelCut);
    procedure SetBevelOuter(const Value: TBevelCut);
    procedure SetBevelWidth(const Value: Integer);
    procedure SetText(const Value: string);
    procedure SetColorTo(const Value: TColor);
  public
    constructor Create;
    destructor Destroy; override;
  published
    property BevelInner: TBevelCut read FBevelInner write SetBevelInner;
    property BevelOuter: TBevelCut read FBevelOuter write SetBevelOuter;
    property BevelWidth: Integer read FBevelWidth write SetBevelWidth;
    property Color: TColor read FColor write SetColor default clBtnFace;
    property ColorTo: TColor read FColorTo write SetColorTo default clNone;
    property Font: TFont read FFont write SetFont;
    property Height: Integer read FHeight write SetHeight;
    property Text: string read FText write SetText;
    property Visible: Boolean read FVisible write SetVisible;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TEditAlign = class;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TInspectorBar = class(TCustomPanel, ITMSStyle)
  private
    FRepeatTimer: TTimer;
    FPanels: TInspectorPanels;
    FTopPanel: Integer;
    FInspectorCaption: TInspectorCaption;
    FShowUP: Boolean;
    FShowDown: Boolean;
    FHotAnchor: string;
    FLastOpen: Integer;
    FFirstOpen: Integer;
    FMode: TInspectorBarMode;
    FScrollBar: TScrollBar;
    FOldWidth: Integer;
    FDefCursor: TCursor;
    FMouseItem: TInspectorItem;
    FMousePanel: Integer;
    FMouseDown: Boolean;
    FShadedHeader: TBitmap;
    FFlat: Boolean;
    FInspectorEdit: TInspectorEdit;
    FInspectorCombo: TInspectorCombo;
    FInspectorColorCombo: TInspectorColorCombo;
    FInspectorSpin: TInspectorSpin;
    FInspectorEditBtn: TInspectorEditBtn;
    FInspectorDateTimePicker: TInspectorDateTimePicker;
    FEditing: Boolean;
    FEditItem: TInspectorItem;
    FEditLast: TInspectorItem;
    FDisableExit: Boolean;
    FOnItemClick: TInspectorItemEvent;
    FOnPanelOpen: TInspectorPanelEvent;
    FOnPanelClose: TInspectorPanelEvent;
    FOnStartLabelEdit: TStartLabelEdit;
    FOnStopLabelEdit: TStopLabelEdit;
    FImages: TImageList;
    FIsWinXP: Boolean;
    FOldHint: string;
    FOnEditStart: TInspectorItemEvent;
    FOnEditStop: TInspectorItemEvent;
    FOnEditProp: TInspectorItemEvent;
    FOnGetValueList: TGetValueListEvent;
    FOnItemDraw: TItemDrawEvent;
    FOnPanelDraw: TPanelDrawEvent;
    FAutoAdvance: Boolean;
    FEditType: TInplaceEditType;
    FOnEditUpdate: TInspectorItemEvent;
    FOnEditDblClick: TInspectorItemEvent;
    FOnEditBtnClick: TInspectorItemEvent;
    FOnURLClick: TInspectorURLEvent;
    FAcceptFiles: Boolean;
    FOnFileDrop: TInspectorFileDropEvent;
    FOnItemRightClick: TInspectorItemEvent;
    FShowFocus: Boolean;
    FOleDropTarget: Boolean;
    FMainPopupMenu: TPopupMenu;
    FInspectorDropTarget: TInspectorBarDropTarget;
    FOleDropTargetAssigned: Boolean;
    FOnURLDrop: TInspectorFileDropEvent;
    FOnCustomEditButton: TCustomEditButtonEvent;
    FOnCustomEditDraw: TCustomEditDrawEvent;
    FShowEditorAlways: Boolean;
    FInspectorHelp: TInspectorHelp;
    FOnButtonClick: TInspectorItemEvent;
    FOnEditSpinUp: TInspectorItemEditEvent;
    FOnEditSpinDown: TInspectorItemEditEvent;
    FOnEditSpinChange: TInspectorItemEditEvent;
    FOnEditCheckChange: TInspectorItemCheckEvent;
    FOnEditComboChange: TInspectorItemComboEvent;
    FOnEditColorChange: TInspectorItemColorEvent;
    FCheckTrue: string;
    FCheckFalse: string;
    FButtonRect: TRect;
    FButtonUpPress: Boolean;
    FButtonDownPress: Boolean;
    FOnItemAnchorClick: TInspectorItemAnchorEvent;
    FOnItemClose: TInspectorItemEvent;
    FOnItemOpen: TInspectorItemEvent;
    FOnHelpAnchorClick: THelpAnchorEvent;
    FOnEditAutoAdvance: TInspectorItemEvent;
    FOnItemDblClick: TInspectorItemEvent;
    iteminplace: TInspectoritem;
    FEditInplace: TEditAlign;
    FmemoInplace: Tmemo;
    FhideMemo: Tmemo;
    FEllipsis: Boolean;
    FUpdateCount: Integer;
    FCheckTextShow: Boolean;
    FStyle: TInspectorBarStyle;
    FColorTo: TColor;
    FDefaultGradientDirection: TGradientDirection;
    FAutoThemeAdapt: Boolean;
    FHoverColorTo: TColor;
    FOnPanelOpened: TInspectorPanelEvent;
    FOnPanelCaptionClick: TInspectorPanelEvent;
    FOnPanelCaptionRightClick: TInspectorPanelEvent;

    FNodeOpenGlyph: TBitmap;
    FNodeCloseGlyph: TBitmap;
    FOnItemValue: TItemValueEvent;
    //FHoverColor: TColor;
    procedure SetInspectorPanels(const Value: TInspectorPanels);
    procedure SetTopPanel(const Value: Integer);
    procedure SetMode(const Value: TInspectorBarMode);
    procedure SetFlat(const Value: Boolean);
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMControlChange(var Message: TMessage); message CM_CONTROLCHANGE;
    procedure CMHintShow(var Msg: TMessage); message CM_HINTSHOW;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMWantSpecialKey(var Msg: TCMWantSpecialKey); message CM_WANTSPECIALKEY;
    procedure WMDropFiles(var Message: TMessage); message WM_DROPFILES;
    procedure WMDestroy(var Message: TMessage); message WM_DESTROY;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMEraseBkGnd(var Message: TMessage); message WM_ERASEBKGND;
    procedure LabelInplaceExit(sender: tobject);
    procedure LabelInplaceKeyPress(Sender: TObject; var Key: Char);
    function GetAlignEx: TAlign;
    procedure SetAlignEx(const Value: TAlign);
    procedure SetAcceptFiles(const Value: Boolean);
    function DoVisualStyles: Boolean;
    function GetVersionString: string;
    procedure SetOleDropTarget(const Value: Boolean);
    procedure SetShowEditorAlways(const Value: Boolean);
    procedure DrawItemNode(X, Y: Integer; State: Boolean);
    function GetPopupMenuEx: TPopupMenu;
    procedure SetPopupMenu(const Value: TPopupMenu);
    procedure SetCheckFalse(const Value: string);
    procedure SetCheckTrue(const Value: string);
    function GetVersionNr: Integer;
    function FoundShortcut(Key: Word; Shift: TShiftState): boolean;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure SetEllipsis(const Value: Boolean);
    procedure SetCheckTextShow(const Value: Boolean);
    function GetVersionComp: string;
    procedure SetVersion(const Value: string);
    procedure SetStyle(const Value: TInspectorBarStyle);
    procedure SetPanelStyle(const Value: TInspectorBarStyle; Panel: TInspectorPanel);
    procedure SetColorTo(const Value: TColor);
    procedure SetDefaultGradientDirection(const Value: TGradientDirection);

    procedure ThemeAdapt;
    function GetTimerEnabled: boolean;
    procedure SetTimerEnabled(const Value: boolean);
  protected
    procedure AlignControls(AControl: TControl; var Rect: TRect); override;
    procedure InvalidateItem(Panel: TInspectorPanel; Item: TInspectorItem);
    function GetItemRect(Panel: TInspectorPanel; Item: TInspectorItem): TRect; virtual;
    function GetCaptionRect(Panel: TInspectorPanel): TRect;
    function IsMouseDown: Boolean;
    procedure DrawGradient(ACanvas: TCanvas; FromColor, ToColor: TColor; Steps: Integer; R: TRect; Direction: Boolean);
    procedure DrawCaption(Panel: TInspectorPanel; R: TRect); virtual;
    procedure DrawCaptionOptions(Panel: TInspectorPanel; Canvas: TCanvas; var R: TRect); virtual;
    function HintCaptionOptions(Panel: TInspectorPanel; x, y: Integer; var Hint: string): Boolean; virtual;
    function MouseMoveCaptionOptions(Panel: TInspectorPanel; x, y: Integer): Boolean; virtual;
    function MouseDownCaptionOptions(Panel: TInspectorPanel; x, y: Integer): Boolean; virtual;
    function isItemIndex(Panel: TInspectorPanel; Item: TInspectorItem): boolean;
    function prepareWordWraptext(Panel: TInspectorPanel; text: string; r: trect): string;
    procedure Getlabelrect(Panel: TInspectorPanel; Item: TInspectorItem; var R: TRect);
    function Visiblelabelinplace: boolean;
    procedure hideinplace;
    procedure DrawItem(Panel: TInspectorPanel; Item: TInspectorItem; R: TRect); virtual;
    procedure DrawButtonUp;
    procedure DrawButtonDown;
    procedure CaptionChanged(Sender: TObject);
    procedure ShadeChanged(Sender: TObject);
    procedure Paint; override;
    procedure PaintSinglePanel;
    procedure PaintMultiPanel;
    function GetNumbervisiblePanelUp(NoPanel: Integer): integer;
    function GetNumbervisiblePanelDown(NoPanel: Integer): integer;
    function GetRealTopPanel: integer;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure ScrollKeydown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUpSinglePanel(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure MouseDownSinglePanel(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure MouseUpMultiPanel(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure MouseDownMultiPanel(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure RepeatTimer(Sender: TObject);
    procedure ItemClicked(Button: TMouseButton; Panel: TInspectorPanel; Item: TInspectorItem);
    procedure EditDblClick(Sender: TObject);
    procedure EditBtnClick(Sender: TObject);
    procedure EditChanged(Sender: TObject);
    procedure ColComboChanged(Sender: TObject);
    procedure ComboChanged(Sender: TObject);
    procedure CheckChange(Sender: TObject);
    procedure PanelCaptionClick(Panel: TInspectorPanel);
    procedure PanelCaptionRightClick(Panel: TInspectorPanel);
    procedure PanelOpened(Panel: TInspectorPanel);
    procedure PanelClosed(Panel: TInspectorPanel);
    procedure AfterPanelOpened(Panel: TInspectorPanel);
    procedure SpinUp(Sender: TObject);
    procedure SpinDown(Sender: TObject);
    procedure SpinChange(Sender: TObject);
    procedure Scroll(Sender: TObject);
    function StartEditingInplace(Panel: TInspectorPanel; Item: TInspectorItem): boolean;
    procedure StopEditingInplace;
    procedure ArrangeOpenClose;
    procedure ShadeHeader; // method that redraws the shaded bitmap with the current shade
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure NextEdit(InspectorItem: TInspectorItem; NextKey: word);
    function GetHint(x, y: Integer): string;
    function GetHintSingle(x, y: Integer): string;
    function GetHintMulti(x, y: Integer): string;
    function GetCursorMulti(x, y: Integer): Boolean;
    function GetCursorSingle(x, y: Integer): Boolean;
    function GetCaption(x, y: Integer): TInspectorPanel;
    function GetItem(x, y: Integer): TInspectorItem;
    function GetPanel(x, y: Integer): TInspectorPanel;
    function GetBarHeight: Integer;
    procedure SearchShortcutCaption(key: Word);
    procedure UpdateEdit;
    function GetCursorHint(P: TPoint): string;
    function InspectorWidth: Integer;
    function HelpWidth: Integer;
    function InspectorIndent: Integer;
    function InDesign: Boolean;
    function GetParentForm(Control: TControl): TCustomForm; virtual;
    procedure WndProc(var Message: TMessage); override;

    procedure DoExit; override;
    procedure DoEnter; override;
    procedure DoEditStart(APanel: TInspectorPanel; AItem: TInspectorItem); virtual;
    procedure DoEditProp(APanel: TInspectorPanel; AItem: TInspectorItem); virtual;
    property BarHeight: Integer read GetBarHeight;
  public
    procedure ArrangeControls;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Resize; override;
    procedure Loaded; override;
    function CreatePanels: TInspectorPanels; virtual;
    procedure StartEdit(InspectorItem: TInspectorItem); virtual;
    procedure StopEdit(InspectorItem: TInspectorItem); virtual;
    procedure HidePopup;
    procedure GetValueList(InspectorItem: TInspectorItem; Values: TStringList); virtual;
    function GetPanelItemAtXY(x, y: Integer; var Panel: TInspectorPanel; var Item: TInspectorItem): Boolean;
    function IsPanelItemAtXY(x, y: Integer): Boolean;
    function GetPanelAtXY(x, y: Integer; var Panel: TInspectorPanel): Boolean;
    function IsShortCut(var Message: TWMKey): Boolean; dynamic;
    property VersionNr: Integer read GetVersionNr;
    property VersionString: string read GetVersionString;
    property EditItem: TInspectorItem read FEditItem;
    procedure ExpandAll;
    procedure CollapsAll;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure SetComponentStyle(AStyle: TTMSStyle);
    property Combo: TInspectorCombo read FInspectorCombo;
    property ColorCombo: TInspectorColorCombo read FInspectorColorCombo;
    property Editing: boolean read FEditing;
    property Edit: TInspectorEdit read FInspectorEdit;
    property EditBtn: TInspectorEditBtn read FInspectorEditBtn;
    property DateTimePicker: TInspectorDateTimePicker read FInspectorDateTimePicker;
    property Spin: TInspectorSpin read FInspectorSpin;
    property ScrollBar: TScrollBar read FScrollBar;
    property EnableRepeatButton: boolean read GetTimerEnabled write SetTimerEnabled;
  published
    property AcceptFiles: Boolean read FAcceptFiles write SetAcceptFiles;
    property AutoAdvance: Boolean read FAutoAdvance write FAutoAdvance;
    property Align: TAlign read GetAlignEx write SetAlignEx;
    property Anchors;
    property Constraints;
    property CheckTrue: string read FCheckTrue write SetCheckTrue;
    property CheckFalse: string read FCheckFalse write SetCheckFalse;
    property CheckTextShow: Boolean read FCheckTextShow write SetCheckTextShow default True;
    property Color;
    property DragMode;
    property DragKind;
    property DragCursor;
    property Ellipsis: Boolean read FEllipsis write SetEllipsis;
    property Flat: Boolean read FFlat write SetFlat;
    property Images: TImageList read FImages write FImages;
    property HelpPanel: TInspectorHelp read FInspectorHelp write FInspectorHelp;
    property Mode: TInspectorBarMode read Fmode write SetMode default imSinglePanelActive;
    property OleDropTarget: Boolean read FOleDropTarget write SetOleDropTarget default False;
    property PanelCaption: TInspectorCaption read FInspectorCaption write FInspectorCaption;
    property Panels: TInspectorPanels read FPanels write SetInspectorPanels;
    property PopupMenu: TPopupMenu read GetPopupMenuEx write SetPopupMenu;
    property ShowEditorAlways: Boolean read FShowEditorAlways write SetShowEditorAlways;
    property ShowFocus: Boolean read FShowFocus write FShowFocus default False;
    property ShowHint;
    property TabStop;
    property TabOrder;
    property TopPanel: Integer read FTopPanel write SetTopPanel;
    property Visible;
    property OnEnter;
    property OnExit;
    property OnMouseMove;
    property OnMouseDown;
    property OnMouseUp;
    property OnKeyDown;
    property OnKeyUp;
    property OnKeyPress;

    property OnButtonClick: TInspectorItemEvent read FOnButtonClick write FOnButtonClick;
    property OnCustomEditButton: TCustomEditButtonEvent read FOnCustomEditButton write FOnCustomEditButton;
    property OnCustomEditDraw: TCustomEditDrawEvent read FOnCustomEditDraw write FOnCustomEditDraw;

    property OnEditAutoAdvance: TInspectorItemEvent read FOnEditAutoAdvance write FOnEditAutoAdvance;
    property OnEditProp: TInspectorItemEvent read FOnEditProp write FOnEditProp;
    property OnEditStart: TInspectorItemEvent read FOnEditStart write FOnEditStart;
    property OnEditStop: TInspectorItemEvent read FOnEditStop write FOnEditStop;
    property OnEditUpdate: TInspectorItemEvent read FOnEditUpdate write FOnEditUpdate;
    property OnEditDblClick: TInspectorItemEvent read FOnEditDblClick write FOnEditDblClick;
    property OnEditBtnClick: TInspectorItemEvent read FOnEditBtnClick write FOnEditBtnClick;
    property OnEditCheckChange: TInspectorItemCheckEvent read FOnEditCheckChange write FOnEditCheckChange;
    property OnEditComboChange: TInspectorItemComboEvent read FOnEditComboChange write FOnEditComboChange;
    property OnEditColorChange: TInspectorItemColorEvent read FOnEditColorChange write FOnEditColorChange;
    property OnEditSpinUp: TInspectorItemEditEvent read FOnEditSpinUp write FOnEditSpinUp;
    property OnEditSpinDown: TInspectorItemEditEvent read FOnEditSpinDown write FOnEditSpinDown;
    property OnEditSpinChange: TInspectorItemEditEvent read FOnEditSpinChange write FOnEditSpinChange;

    property OnGetValueList: TGetValueListEvent read FOnGetValueList write FOnGetValueList;
    property OnHelpAnchorClick: THelpAnchorEvent read FOnHelpAnchorClick write FOnHelpAnchorClick;
    property OnItemAnchorClick: TInspectorItemAnchorEvent read FOnItemAnchorClick write FOnItemAnchorClick;
    property OnItemClick: TInspectorItemEvent read FOnItemClick write FOnItemClick;
    property OnItemClose: TInspectorItemEvent read FOnItemClose write FOnItemClose;
    property OnItemDblClick: TInspectorItemEvent read FOnItemDblClick write FOnItemDblClick;
    property OnItemOpen: TInspectorItemEvent read FOnItemOpen write FOnItemOpen;
    property OnItemRightClick: TInspectorItemEvent read FOnItemRightClick write FOnItemRightClick;
    property OnItemDraw: TItemDrawEvent read FOnItemDraw write FOnItemDraw;
    property OnItemValue: TItemValueEvent read FOnItemValue write FOnItemValue;
    property OnPanelDraw: TPanelDrawEvent read FOnPanelDraw write FOnPanelDraw;
    property OnPanelOpen: TInspectorPanelEvent read FOnPanelOpen write FOnPanelOpen;
    property OnPanelCaptionClick: TInspectorPanelEvent read FOnPanelCaptionClick write FOnPanelCaptionClick;
    property OnPanelCaptionRightClick: TInspectorPanelEvent read FOnPanelCaptionRightClick write FOnPanelCaptionRightClick;    
    property OnPanelOpened: TInspectorPanelEvent read FOnPanelOpened write FOnPanelOpened;
    property OnPanelClose: TInspectorPanelEvent read FOnPanelClose write FOnPanelClose;
    property OnURLClick: TInspectorURLEvent read FOnURLClick write FOnURLClick;
    property OnFileDrop: TInspectorFileDropEvent read FOnFileDrop write FOnFileDrop;
    property OnURLDrop: TInspectorFileDropEvent read FOnURLDrop write FOnURLDrop;
    property OnStartLabelEdit: TStartLabelEdit read FOnStartLabelEdit write FOnStartLabelEdit;
    property OnStopLabelEdit: TStopLabelEdit read FOnStopLabelEdit write FOnStopLabelEdit;
    property OnDragDrop;
    property OnDragOver;
    property OnStartDrag;
    property OnEndDrag;
    property Version: string read GetVersionComp write SetVersion;

    property Style: TInspectorBarStyle read FStyle write SetStyle default esCustom;
    property ColorTo: TColor read FColorTo write SetColorTo default clNone;
    property DefaultGradientDirection: TGradientDirection read FDefaultGradientDirection write SetDefaultGradientDirection;
    property AutoThemeAdapt: Boolean read FAutoThemeAdapt write FAutoThemeAdapt default False;
  end;
  //Simple Edit with text allign at left,right,center
  TEditAlign = class(TEdit)
  private
  protected
    FAlignment: TAlignment;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure SetAlignment(AValue: TAlignment);
  published
    property Alignment: TAlignment read FAlignment write SetAlignment;
  end;

implementation

uses
  ShellAPI
  , ImgList
  ;

const
  BoxOffset = 1;

{$DEFINE REMOVEDRAW}
{$DEFINE REMOVESTRIP}

{$I DELPHIXE.INC}

{$I HTMLENGL.PAS}

function IsNumChar(ch: char): boolean;
begin
  {$IFNDEF DELPHIXE4_LVL}

  {$IFNDEF DELPHI_UNICODE}
  Result := (ch in ['0'..'9']);
  {$ENDIF}
  {$IFDEF DELPHI_UNICODE}
  Result := Character.IsNumber(ch);
  {$ENDIF}

  {$ENDIF}

  {$IFDEF DELPHIXE4_LVL}
  Result := ch.IsNumber;
  {$ENDIF}
end;


function Min(a, b: Integer): Integer;
begin
  if a > b then
    Result := b
  else
    Result := a;
end;

{
function Max(a,b: Integer): Integer;
begin
  if a >  b then
    Result := a
  else
    Result := b;
end;
}

function IsHTML(s: string): Boolean;
begin
  Result := Pos('</', s) > 0;
end;

function WidthSizeIcon(ico: Ticon): TPoint;
begin
  Result := Point(0, 0);

  if ico = nil then
    Exit;

  if (not Assigned(ico)) then
    Exit;

  if (ico.Empty) then
    Exit;

  Result.X := ico.Width;
  Result.Y := ico.Height;
end;

function SysImageEx(Canvas: TCanvas; x, y: Integer; APath: string;
  Large, Draw: Boolean): TPoint;
var
  SFI: TSHFileInfo;
  i: Integer;
  imglsthandle: THandle;
  rx, ry: Integer;
  SILFlags: DWORD;

begin
  Result := Point(0, 0);

  if (APath <> '') then
  begin
    if Large then
      SILFlags := SHGFI_SYSICONINDEX or SHGFI_LARGEICON
    else
      SILFlags := SHGFI_SYSICONINDEX or SHGFI_SMALLICON;

    FillChar(SFI, sizeof(SFI), 0);

    if FileExists(APath) or DirExists(APath) then
      // If the file or directory exists, just let Windows figure out it's attrs.
      imglsthandle := SHGetFileInfo(PChar(APath), 0, SFI, SizeOf(TSHFileInfo),
        SILFlags {or OPEN_FLAG[Open] or SELECTED_FLAG[Selected]})
    else
    // File doesn't exist, so Windows doesn't know what to do with it.  We have
    // to tell it by passing the attributes we want, and specifying the
    // SHGFI_USEFILEATTRIBUTES flag so that the function knows to use them.
      imglsthandle := SHGetFileInfo(PChar(APath), 0, SFI, SizeOf(TSHFileInfo),
        SILFlags or SHGFI_USEFILEATTRIBUTES {or OPEN_FLAG[Open] or SELECTED_FLAG[Selected]});
    i := SFI.iIcon;

    ImageList_GetIconSize(imglsthandle, rx, ry);
    Result := Point(rx, ry);

    if Draw then
      ImageList_Draw(imglsthandle, i, Canvas.Handle, x, y, ILD_TRANSPARENT);
  end;
end;

function IsWinXP: Boolean;
var
  VerInfo: TOSVersioninfo;
begin
  VerInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
  GetVersionEx(verinfo);
  Result := (verinfo.dwMajorVersion > 5) or
    ((verinfo.dwMajorVersion = 5) and (verinfo.dwMinorVersion >= 1));
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

procedure TEditAlign.CreateParams(var Params: TCreateParams);
var
  CenterStyle: DWORD;
begin
  inherited CreateParams(Params);

  with Params do
  begin
    case FAlignment of
      taLeftJustify: CenterStyle := ES_LEFT;
      taRightJustify: CenterStyle := ES_RIGHT;
      taCenter: CenterStyle := ES_CENTER;
    else
      CenterStyle := 0;
    end;
    Params.Style := Params.Style or CenterStyle;
  end;

end;

procedure TEditAlign.SetAlignment(AValue: TAlignment);
begin
  if AValue <> FAlignment then
  begin
    FAlignment := AValue;
    RecreateWnd;
  end;
end;


{ TInspectorItems }

function TInspectorItems.Add: TInspectorItem;
begin
  Result := TInspectorItem(inherited Add);
  TInspectorPanels(FOwner.GetOwner).FOwner.Invalidate;
end;

constructor TInspectorItems.Create(AOwner: TInspectorPanel);
begin
  inherited Create(AOwner, CreateItemClass);
  FOwner := AOwner;
end;

function TInspectorItems.CreateItemClass: TCollectionItemClass;
begin
  Result := TInspectorItem;
end;

function TInspectorItems.GetItem(Index: Integer): TInspectorItem;
begin
  Result := TInspectorItem(inherited Items[Index]);
end;

function TInspectorItems.Insert(index: Integer): TInspectorItem;
begin
  Result := TInspectorItem(inherited Insert(Index));
  TInspectorPanels(FOwner.GetOwner).FOwner.Invalidate;
end;

procedure TInspectorItems.SetItem(Index: Integer;
  const Value: TInspectorItem);
begin
  inherited Items[Index] := Value;
end;

procedure TInspectorItems.Update(Item: TCollectionItem);
begin
  inherited;
  if Assigned(Item) then
    TInspectorPanels(FOwner.GetOwner).FOwner.InvalidateItem(FOwner, TInspectorItem(Item))
  else
    TInspectorPanels(FOwner.GetOwner).FOwner.Invalidate;
end;

{ TInspectorPanels }

function TInspectorPanels.Add: TInspectorPanel;
begin
  Result := TInspectorPanel(inherited Add);
  FOwner.ArrangeControls;
end;

constructor TInspectorPanels.Create(AOwner: TInspectorBar);
begin
  inherited Create(AOwner, CreateItemClass);
  FOwner := AOwner;
end;

function TInspectorPanels.CreateItemClass: TCollectionItemClass;
begin
  Result := TInspectorPanel;
end;

function TInspectorPanels.GetItem(Index: Integer): TInspectorPanel;
begin
  Result := TInspectorPanel(inherited Items[Index]);
end;

function TInspectorPanels.Insert(Index: Integer): TInspectorPanel;
begin
  Result := TInspectorPanel(inherited Insert(Index));
  FOwner.ArrangeControls;
end;

procedure TInspectorPanels.SetItem(Index: Integer;
  const Value: TInspectorPanel);
begin
  inherited Items[Index] := Value;
end;

procedure TInspectorPanels.Update(Item: TCollectionItem);
begin
  inherited;
  FOwner.Invalidate;
end;

{ TInspectorPanel }

procedure TInspectorPanel.Assign(Source: TPersistent);
begin
  if (Source is TInspectorPanel) then
  begin
    FAlignment := (Source as TInspectorPanel).Alignment;
    FAllowResize := (Source as TInspectorPanel).AllowResize;
    FBackground := (Source as TInspectorPanel).Background;
    FButtonDownColor := (Source as TInspectorPanel).ButtonDownColor;
    FCaption := (Source as TInspectorPanel).Caption;
    FCaptionWidth := (Source as TInspectorPanel).CaptionWidth;
    FColor := (Source as TInspectorPanel).Color;
    FControl := (Source as TInspectorPanel).Control;
    FFont.Assign((Source as TInspectorPanel).Font);
    FEditBorderColor := (Source as TInspectorPanel).Color;
    FEditBox := (Source as TInspectorPanel).EditBox;
    FEditColor := (Source as TInspectorPanel).Color;
    FEditFontColor := (Source as TInspectorPanel).Color;
    FGradientDirection := (Source as TInspectorPanel).GradientDirection;
    FGradientEnd := (Source as TInspectorPanel).Color;
    FGradientStart := (Source as TInspectorPanel).Color;
    FGradientSteps := (Source as TInspectorPanel).GradientSteps;
    FGridLineColor := (Source as TInspectorPanel).GridLineColor;
    FGridLines := (Source as TInspectorPanel).GridLines;
    FHint := (Source as TInspectorPanel).Hint;
    FHover := (Source as TInspectorPanel).HoverCaption;
    FHoverFullWidth := (Source as TInspectorPanel).HoverFullWidth;
    FHoverIconColor := (Source as TInspectorPanel).HoverIconColor;
    FHoverIconBorderColor := (Source as TInspectorPanel).HoverIconBorderColor;
    FHoverIconDownColor := (Source as TInspectorPanel).HoverIconDownColor;
    FImageIndex := (Source as TInspectorPanel).ImageIndex;
    FIndent := (Source as TInspectorPanel).Indent;
    FPopupMenu := (Source as TInspectorPanel).PopupMenu;
    FModifiedFont.Assign((Source as TInspectorPanel).Font);
    FMultiPanelState := (Source as TInspectorPanel).MultiPanelState;
    FVariableItemHeight := (Source as TInspectorPanel).VariableItemHeight;
    FItemHeight := (Source as TInspectorPanel).ItemHeight;
    FItems.Assign((Source as TInspectorPanel).Items);
    FShowModified := (Source as TInspectorPanel).ShowModified;
    FSplitter := (Source as TInspectorPanel).Splitter;
    FPanelStyle := (Source as TInspectorPanel).Style;
    FTag := (Source as TInspectorPanel).Tag;
    FTexture.Assign((Source as TInspectorPanel).Texture);
    FTopItem := (Source as TInspectorPanel).TopItem;
    FWordWrap := (Source as TInspectorPanel).WordWrap;
    FWordWrapCaption := (Source as TInspectorPanel).WordWrapCaption;
    FVisible := (Source as TInspectorPanel).Visible;
    FShortCut := (Source as TInspectorPanel).ShortCut;
    FSelectFontColor := (Source as TInspectorPanel).SelectFontColor;
    FSelectColor := (Source as TInspectorPanel).SelectColor;
    FitemIndex := (Source as TInspectorPanel).ItemIndex;
  end;
end;

procedure TInspectorPanel.CollapsAll;
var
  i: Integer;
begin
  for i := 1 to Items.Count do
    Items[i - 1].CollapsChilds;
end;

constructor TInspectorPanel.Create(Collection: TCollection);
begin
  inherited;
  FAlignment := taCenter;
  FItems := CreateItems;
  FFont := TFont.Create;
  FVisible := True;
  FItemIndex := -1;
  FSelectColor := clNavy;
  FSelectFontColor := clYellow;
  FIconLargeSize := 32;
  FFont.OnChange := FontChanged;
  FModifiedFont := TFont.Create;
  FModifiedFont.OnChange := FontChanged;
  FModifiedFont.Style := [fsBold];
  FModifiedFont.Color := clNavy;
  FColor := clGray;
  FSelectColorTo := clNone;
  FSelectDownColorTo := clNone;
  FHoverIconColorTo := clNone;
  FHoverIconDownColorTo := clNone;
  FPanelStyle := psLargeIcon;
  FGradientDirection := gdVertical;
  FGradientStart := clWhite;
  FGradientEnd := clSilver;
  FGradientSteps := 20;
  FItemHeight := 48;
  FCaptionWidth := 60;
  FCaptionColor := clNone;
  FEditBorderColor := clNone;
  FEditColor := clWhite;
  FEditBox := False;
  FIsOpen := False;
  FTexture := TBitmap.Create;
  FImageIndex := -1;
  FGridLineColor := clGray;
  FHoverIconColor := clNone;
  FHoverIconBorderColor := clNone;
  FHoverIconDownColor := clNone;
  FSplitter := TInspImage.Create;
  FSplitter.OnChange := ImgChanged;
  FButtonDownColor := clSilver;
  FVariableItemHeight := False;
  FPopupMenu := nil;
  FCaptionFont := TFont.Create;
  FCaptionFont.OnChange := FontChanged;

  FImageAlign := iaLeft;
  InspectorBar.SetPanelStyle(InspectorBar.Style, self);   
end;

function TInspectorPanel.CreateItems: TInspectorItems;
begin
  Result := TInspectorItems.Create(Self);
end;

destructor TInspectorPanel.Destroy;
begin
  if (InspectorBar.TopPanel = Index) and (Index > 0) then
    InspectorBar.FTopPanel := Index - 1;
  InspectorBar.FMousePanel := -1;
  InspectorBar.FMouseItem := nil;
  FItems.Free;
  FFont.Free;
  FCaptionFont.Free;
  FModifiedFont.Free;
  FSplitter.Free;
  FTexture.Free;
  inherited;
end;

procedure TInspectorPanel.ExpandAll;
var
  i: Integer;
begin
  for i := 1 to Items.Count do
    Items[i - 1].ExpandChilds;
end;

procedure TInspectorPanel.FontChanged(Sender: TObject);
begin
  Changed(False);
end;

function TInspectorPanel.GetDisplayName: string;
begin
  if Caption <> '' then
    Result := Caption
  else
    Result := 'TInspectorPanel' + IntToStr(Index);
end;

function TInspectorPanel.GetInspectorBar: TInspectorBar;
begin
  Result := TInspectorPanels(Collection).FOwner;
end;

function TInspectorPanel.GetMultiPanelState: TMultiPanelState;
begin
  if FIsOpen then
  begin
    if (FMultiPanelState = mpsAlwaysOpen) then
      Result := mpsAlwaysOpen
    else
      Result := mpsOpen;
  end
  else
  begin
    Result := mpsClose;
  end;
end;

function TInspectorPanel.GetPanelHeight: Integer;
begin
  Result := InspectorBar.BarHeight - ((InspectorBar.getNumbervisiblePanelDown(InspectorBar.GetRealtoppanel) + 1) * InspectorBar.PanelCaption.Height);
end;

function TInspectorPanel.GetTopItem: Integer;
begin
  Result := FTopItem;
end;

procedure TInspectorPanel.ImgChanged(Sender: TObject);
begin
  Changed(False);
end;

procedure TInspectorPanel.SetAlignment(const Value: TAlignment);
begin
  FAlignment := Value;
  Changed(False);
end;

procedure TInspectorPanel.SetBackground(const Value: TPanelBackground);
begin
  FBackground := Value;
  Changed(False);
end;

procedure TInspectorPanel.SetButtonDownColor(const Value: TColor);
begin
  FButtonDownColor := Value;
  Changed(False);
end;

procedure TInspectorPanel.SetCaption(const Value: string);
begin
  FCaption := Value;
  Changed(False);
end;

procedure TInspectorPanel.SetCaptionColor(const Value: TColor);
begin
  FCaptionColor := Value;
  Changed(False);
end;

procedure TInspectorPanel.SetCaptionFont(const Value: TFont);
begin
  FCaptionFont.Assign(Value);
  Changed(False);
end;

procedure TInspectorPanel.SetCaptionWidth(const Value: Integer);
begin
  FCaptionWidth := Value;
  Changed(False);
end;

procedure TInspectorPanel.SetColor(const Value: TColor);
begin
  FColor := Value;
  Changed(False);
end;

procedure TInspectorPanel.SetControl(const Value: TWinControl);
var
  i: Integer;
begin
  if Assigned(Value) then
  begin
    for i := 1 to InspectorBar.Panels.Count do
      if (i <> Index) and (InspectorBar.Panels[i - 1].Control = Value) then
      begin
        ShowMessage('Control already assigned to another panel');
        Exit;
      end;
  end;

  FControl := Value;

  InspectorBar.ArrangeControls;

  Changed(False);
end;

procedure TInspectorPanel.SetEditBorderColor(const Value: TColor);
begin
  FEditBorderColor := Value;
  Changed(False);
end;

procedure TInspectorPanel.SetEditBox(const Value: Boolean);
begin
  FEditBox := Value;
  Changed(False);
end;

procedure TInspectorPanel.SetEditColor(const Value: TColor);
begin
  FEditColor := Value;
  Changed(False);
end;

procedure TInspectorPanel.SetEditFontColor(const Value: TColor);
begin
  FEditFontColor := Value;
  Changed(False);
end;

procedure TInspectorPanel.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
  Changed(False);
end;

procedure TInspectorPanel.SetGradientDirection(
  const Value: TGradientDirection);
begin
  FGradientDirection := Value;
  Changed(False);
end;

procedure TInspectorPanel.SetGradientEnd(const Value: TColor);
begin
  FGradientEnd := Value;
  Changed(False);
end;

procedure TInspectorPanel.SetGradientStart(const Value: TColor);
begin
  FGradientStart := Value;
  Changed(False);
end;

procedure TInspectorPanel.SetGradientSteps(const Value: Integer);
begin
  FGradientSteps := Value;
  Changed(False);
end;

procedure TInspectorPanel.SetGridLineColor(const Value: TColor);
begin
  FGridLineColor := Value;
  Changed(False);
end;

procedure TInspectorPanel.SetGridLines(const Value: Boolean);
begin
  FGridLines := Value;
  Changed(False);
end;

procedure TInspectorPanel.SetHoverIconColorTo(const Value: TColor);
begin
  FHoverIconColorTo := Value;
end;

procedure TInspectorPanel.SetHoverIconDownColorTo(const Value: TColor);
begin
  FHoverIconDownColorTo := Value;
end;

procedure TInspectorPanel.SetImageAlign(const Value: TImageAlign);
begin
  FImageAlign := Value;
  Changed(False);
end;

procedure TInspectorPanel.SetImageIndex(const Value: Integer);
begin
  FImageIndex := Value;
  Changed(False);
end;

procedure TInspectorPanel.SetIndent(const Value: Integer);
begin
  FIndent := Value;
  Changed(False);
end;

procedure TInspectorPanel.SetInspectorItems(const Value: TInspectorItems);
begin
  FItems.Assign(Value);
  Changed(False);
end;

procedure TInspectorPanel.SetItemHeight(const Value: Integer);
begin
  if Value > 0 then
    FItemHeight := Value;
  Changed(False);
end;

procedure TInspectorPanel.SetItemIndex(const Value: Integer);
var
  Item: TInspectorItem;
  ir: TRect;
  dx,y: integer;
begin
  if (Value >= 0) and (Value < Items.Count) then
  begin
    FitemIndex := Value;

    if (Style = psButtons) then
    begin
      Item := Items[FItemIndex];
      ir := InspectorBar.GetItemRect (Self, Item);

      dx := ir.Bottom - GetPanelHeight;

      if dx > 0 then
      begin
        Self.TopItem := Self.TopItem + (dx div ItemHeight) + 1;
      end;

      y := (InspectorBar.getNumbervisiblePanelUp(InspectorBar.GetRealTopPanel) + 1) * InspectorBar.PanelCaption.Height;

      if (ir.Top <= y) then
      begin
        self.TopItem := ItemIndex;
      end;

    end;
    Changed(false);
  end
  else
  begin
    FItemIndex := -1;
    Changed(false);
    InspectorBar.Invalidate;
  end;
end;

procedure TInspectorPanel.SetModifiedFont(const Value: TFont);
begin
  FModifiedFont.Assign(Value);
end;

procedure TInspectorPanel.SetMultiPanelState(
  const Value: TMultiPanelState);
begin
  FMultiPanelState := Value;
  FIsOpen := (FMultiPanelState in [mpsOpen, mpsAlwaysOpen]);
  Changed(False);
end;

procedure TInspectorPanel.SetOpen(const Value: Boolean);
begin
  FIsOpen := Value;
  InspectorBar.ArrangeOpenClose;
  InspectorBar.Invalidate;
end;

procedure TInspectorPanel.SetPanelStyle(const Value: TPanelStyle);
begin
  FPanelStyle := Value;
  if (csDesigning in InspectorBar.ComponentState) and not
    (csLoading in InspectorBar.ComponentState) then
    case FPanelStyle of
      psLargeIcon: ItemHeight := 48;
      psSmallIcon: ItemHeight := 20;
      psProperties: ItemHeight := 24;
      psButtons: ItemHeight := 32;
    end;

  Changed(False);
end;

procedure TInspectorPanel.SetSelectBorderColor(const Value: TColor);
begin
  FSelectBorderColor := Value;
  Changed(False);
end;

procedure TInspectorPanel.SetSelectColor(const Value: Tcolor);
begin
  FSelectColor := Value;
  Changed(False);
end;

procedure TInspectorPanel.SetSelectColorTo(const Value: TColor);
begin
  FSelectColorTo := Value;
end;

procedure TInspectorPanel.SetSelectDownColor(const Value: TColor);
begin
  FSelectDownColor := Value;
end;

procedure TInspectorPanel.SetSelectDownColorTo(const Value: TColor);
begin
  FSelectDownColorTo := Value;
end;

procedure TInspectorPanel.SetSelectFontColor(const Value: Tcolor);
begin
  FSelectFontColor := Value;
  Changed(False);
end;

procedure TInspectorPanel.SetShortCut(const Value: TShortCut);
var
  i: Integer;
  P: Tinspectorbar;
begin
  p := GetInspectorBar;
  for i := 0 to p.FPanels.Count - 1 do
  begin
    if (p.FPanels[i].fshortcut = value) and (value <> 0) then
    begin
      ShowMessage('ShortCut already assigned to another panel [' + inttostr(i) + ']');
      Exit;
    end;
  end;

  for i := 0 to FItems.Count - 1 do
  begin
    if (FItems[i].FShortCut = Value) and (Value <> 0) then
    begin
      ShowMessage('ShortCut already assigned to another item [' + inttostr(i) + ']');
      Exit;
    end;
  end;

  FShortCut := Value;
end;

procedure TInspectorPanel.SetShowModified(const Value: Boolean);
var
  i: Integer;
begin
  FShowModified := Value;

  if not FShowModified then
  begin
    for i := 1 to Items.Count do
      Items[i - 1].Modified := False;
  end;
  Changed(False);
end;

procedure TInspectorPanel.SetSplitter(const Value: TInspImage);
begin
  FSplitter.Assign(Value);
  Changed(False);
end;

procedure TInspectorPanel.SetTexture(const Value: TBitmap);
begin
  FTexture.Assign(Value);
  Changed(False);
end;

procedure TInspectorPanel.SetTopItem(const Value: Integer);
begin
  if (value > FItems.Count - 1) or (value < 0) then Exit;
  FTopItem := Value;
end;

procedure TInspectorPanel.SetVariableItemHeight(const Value: Boolean);
begin
  FVariableItemHeight := Value;
  Changed(False);
end;

procedure TInspectorPanel.SetVisible(const Value: Boolean);
begin
  FVisible := Value;
  if Assigned(Control) then Control.visible := Value;
  GetInspectorBar.ArrangeControls;
  Changed(False);
end;

procedure TInspectorPanel.SetWordWrap(const Value: Boolean);
begin
  FWordWrap := Value;
  Changed(False);
end;

procedure TInspectorPanel.SetWordWrapCaption(const Value: Boolean);
begin
  FWordWrapCaption := Value;
  Changed(False);
end;

{ TInspectorItem }

procedure TInspectorItem.Assign(Source: TPersistent);
begin
  if (Source is TInspectorItem) then
  begin
    FAutoIcon := (Source as TInspectorItem).AutoIcon;
    FAutoIconIndex := (Source as TInspectorItem).AutoIconIndex;
    FBackground := (Source as TInspectorItem).Background;
    FBkgPos := (Source as TInspectorItem).BackgroundPosition;
    FBoolValue := (Source as TInspectorItem).BoolValue;
    FCaption := (Source as TInspectorItem).Caption;
    FColorValue := (Source as TInspectorItem).ColorValue;
    FEditLink := (Source as TInspectorItem).EditLink;
    FEditMask := (Source as TInspectorItem).EditMask;
    FFontValue.Assign((Source as TInspectorItem).FontValue);
    FHelp := (Source as TInspectorItem).Help;
    FHint := (Source as TInspectorItem).Hint;
    FHeight := (Source as TInspectorItem).Height;
    FIcon.Assign((Source as TInspectorItem).Icon);
    FImageIndex := (Source as TInspectorItem).ImageIndex;
    FIndent := (Source as TInspectorItem).Indent;
    FIntValue := (Source as TInspectorItem).IntValue;
    FLevel := (Source as TInspectorItem).Level;
    FModified := (Source as TInspectorItem).Modified;
    FPropertyType := (Source as TInspectorItem).PropertyType;
    FReadOnly := (Source as TInspectorItem).ReadOnly;
    FSpinMin := (Source as TInspectorItem).SpinMin;
    FSpinMax := (Source as TInspectorItem).SpinMax;
    FTag := (Source as TInspectorItem).Tag;
    FTextValue := (Source as TInspectorItem).TextValue;
    FValues.Assign((Source as TInspectorItem).Values);
    FVisible := (Source as TInspectorItem).Visible;
    FURL := (Source as TInspectorItem).URL;
    FBitmap.Assign((Source as TInspectorItem).Bitmap);
    FShortCut := (Source as TInspectorItem).ShortCut;
  end;
end;

procedure TInspectorItem.BkgChanged(Sender: TObject);
begin
  Changed;
end;

procedure TInspectorItem.Changed;
begin
  TInspectorItems(GetOwner).Update(Self);
end;

procedure TInspectorItem.CollapsChilds;
var
  i: Integer;
begin
  FNodeState := nsClose;
  i := Index + 1;
  while (i < TInspectorItems(Collection).Count) do
  begin
    if TInspectorItems(Collection).Items[i].Level > Level then
    begin
      TInspectorItems(Collection).Items[i].NodeState := nsClose;
      TInspectorItems(Collection).Items[i].Visible := False
    end
    else
      Break;
    Inc(i);
  end;
end;

constructor TInspectorItem.Create(Collection: TCollection);
begin
  inherited;
  FIcon := TIcon.Create;
  FIcon.Width := (Collection as TInspectorItems).InspectorPanel.IconLargeSize;
  FIcon.Height := (Collection as TInspectorItems).InspectorPanel.IconLargeSize;

  FCaption := 'Item '+ Inttostr(Index);

  FValues := TStringList.Create;
  FImageIndex := -1;

  FFontValue := TFont.Create;
  FFontValue.OnChange := BkgChanged;
  FSpinMin := 0;
  FSpinMax := 100;
  FBackground := TInspImage.Create;
  FBackground.OnChange := BkgChanged;
  FBkgPos := bpBottomRight;
  FVisible := True;
  FHeight := 48; //default!!!
  FNodeState := nsOpen;
  FBitmap := TBitmap.Create;
  FPictureValue := TPicture.Create;
  FMaxLength := 0;
end;

destructor TInspectorItem.Destroy;
begin
  if InspectorBar.EditItem = Self then
  begin
    InspectorBar.StopEdit(Self);
    InspectorBar.FEditing := false;
  end;

  InspectorBar.FMousePanel := -1;
  InspectorBar.FMouseItem := nil;

  if Assigned(EditLink) then
    EditLink.DestroyEditor;

  if FOwnsObject then
    if Assigned(FObject) then
      FObject.Free;

  if FIcon <> nil then
    FIcon.Free;

  FValues.Free;
  FBitmap.Free;
  FPictureValue.Free;
  FFontValue.Free;
  FBackground.Free;
  inherited;
end;

procedure TInspectorItem.DoEdit;
begin
  EditChange;
end;

procedure TInspectorItem.EditChange;
begin
// virtual methods, used for descendent class
end;

procedure TInspectorItem.EditStart;
begin
// virtual methods, used for descendent class
end;

procedure TInspectorItem.EditStop;
begin
// virtual methods, used for descendent class
end;

procedure TInspectorItem.ExpandChilds;
var
  i: Integer;
begin
  FNodeState := nsOpen;
  i := Index + 1;
  while (i < TInspectorItems(Collection).Count) do
  begin
    if TInspectorItems(Collection).Items[i].Level = Level + 1 then
    begin
      TInspectorItems(Collection).Items[i].Visible := True;
    end;

    if TInspectorItems(Collection).Items[i].Level = Level then
      Break;
    Inc(i);
  end;
end;

function TInspectorItem.GetDisplayName: string;
begin
  if Caption <> '' then
    Result := Caption
  else
    Result := 'TInspectorItem' + IntToStr(Index);
end;

function TInspectorItem.GetIcon: TIcon;
begin
  Result := FIcon;
end;

function TInspectorItem.GetInspectorBar: TInspectorBar;
begin
  Result := TInspectorPanels(TInspectorItems(Collection).FOwner.Collection).FOwner;
end;

function TInspectorItem.GetInspectorPanel: TInspectorPanel;
begin
  Result := TInspectorItems(Collection).FOwner
end;

function TInspectorItem.NumChilds: Integer;
var
  i: Integer;
begin
  Result := 0;
  i := Index + 1;
  while (i < TInspectorItems(Collection).Count) do
  begin
    if TInspectorItems(Collection).Items[i].Level > Level then
      Result := Result + 1
    else
      Break;
    inc(i);
  end;
end;

procedure TInspectorItem.SetAutoIcon(const Value: Boolean);
begin
  FAutoIcon := Value;
  Changed;
end;

procedure TInspectorItem.SetAutoIconIndex(const Value: Integer);
begin
  FAutoIconIndex := Value;
  Changed;
end;

procedure TInspectorItem.SetBackground(const Value: TInspImage);
begin
  FBackground := Value;
  Changed;
end;

procedure TInspectorItem.SetBackgroundPos(const Value: TInspBackgroundPos);
begin
  FBkgPos := Value;
  Changed;
end;

procedure TInspectorItem.SetBitmap(const Value: TBitmap);
begin
  FBitmap.Assign(Value);
  Changed;
end;

procedure TInspectorItem.SetBoolValue(const Value: Boolean);
begin
  FBoolValue := Value;
  if FBoolValue then
    FTextValue := InspectorBar.CheckTrue
  else
    FTextValue := InspectorBar.CheckFalse;
  Changed;
end;

procedure TInspectorItem.SetCaption(const Value: string);
begin
  FCaption := Value;
  Changed;
end;

procedure TInspectorItem.SetColorValue(const Value: TColor);
begin
  FColorValue := Value;
  case FColorValue of
    clLime: TextValue := 'Lime';
    clRed: TextValue := 'Red';
    clWhite: TextValue := 'White';
    clBlack: TextValue := 'Black';
    clAqua: TextValue := 'Aqua';
    clGreen: TextValue := 'Green';
    clBlue: TextValue := 'Blue';
    clYellow: TextValue := 'Yellow';
    clSilver: TextValue := 'Silver';
    clGray: TextValue := 'Gray';
    clFuchsia: TextValue := 'Fuchsia';
    clMaroon: TextValue := 'Maroon';
    clOlive: TextValue := 'Olive';
    clNavy: TextValue := 'Navy';
    clPurple: TextValue := 'Purple';
    clTeal: TextValue := 'Teal';
    clBackGround: TextValue := 'Background';
    clActiveCaption: TextValue := 'ActiveCaption';
    clInActiveCaption: TextValue := 'InactiveCaption';
    clMenu: TextValue := 'Menu';
    clWindow: TextValue := 'Window';
    clWindowFrame: TextValue := 'WindowFrame';
    clMenuText: TextValue := 'MenuText';
    clWindowText: TextValue := 'WindowText';
    clCaptionText: TextValue := 'CaptionText';
    clActiveBorder: TextValue := 'ActiveBorder';
    clInactiveBorder: TextValue := 'InactiveBorder';
    clAppWorkSpace: TextValue := 'AppWorkspace';
    clHighLight: TextValue := 'Highlight';
    clHighLightText: TextValue := 'HighlightText';
    clBtnFace: TextValue := 'BtnFace';
    clBtnShadow: TextValue := 'BtnShadow';
    clGrayText: TextValue := 'GrayText';
    clBtnText: TextValue := 'BtnText';
    clInactiveCaptionText: TextValue := 'InactiveCaptionText';
    clBtnHighLight: TextValue := 'BtnHighlight';
    cl3DDkShadow: TextValue := '3ddkShadow';
    cl3DLight: TextValue := '3dLight';
    clInfoText: TextValue := 'InfoText';
    clInfoBk: TextValue := 'Infobk';
    clNone: TextValue := 'None';
  else
    TextValue := '$' + IntToHex(Integer(Value), 8);
  end;
  Changed;
end;

procedure TInspectorItem.SetDateTimeFormat(const Value: string);
begin
  FDateTimeFormat := Value;
end;

procedure TInspectorItem.SetDateTimeValue(const Value: TDateTime);
begin
  FDateTimeValue := Value;

  if FDateTimeFormat <> '' then
    FTextValue := FormatDateTime(FDateTimeFormat, Value)
  else
    FTextValue := DateToStr(Value) + ' ' + TimeToStr(Value);
  Changed;
end;

procedure TInspectorItem.SetDateValue(const Value: TDateTime);
begin
  FDateTime := Value;
  if FDateTimeFormat <> '' then
    FTextValue := FormatDateTime(FDateTimeFormat, Value)
  else
    FTextValue := DateToStr(Value);
  Changed;
end;

procedure TInspectorItem.SetDown(const Value: Boolean);
begin
  FDown := Value;
  Changed;
end;

procedure TInspectorItem.SetFontValue(const Value: TFont);
begin
  FFontValue.Assign(Value);
  if Assigned(FFontValue) then
    FTextValue := FFontValue.Name;
  Changed;
end;

procedure TInspectorItem.SetHeight(const Value: integer);
begin
  FHeight := Value;
  InspectorBar.Invalidate;
end;

procedure TInspectorItem.SetIcon(const Value: TIcon);
begin
  if Value = nil then
  begin
    if FIcon <> nil then
    begin
      FIcon.Free;
      FIcon := nil;
    end
  end
  else
  begin
    if FIcon = nil then
      FIcon := TIcon.Create;
    FIcon.Assign(Value);
  end;
  Changed;
end;

procedure TInspectorItem.SetImageIndex(const Value: Integer);
begin
  FImageIndex := Value;
  Changed;
end;

procedure TInspectorItem.SetIndent(const Value: Integer);
begin
  FIndent := Value;
  Changed;
end;

procedure TInspectorItem.SetIntValue(const Value: Integer);
begin
  FIntValue := Value;
  FTextValue := IntToStr(Value);
  Changed;
end;

procedure TInspectorItem.SetItemFocused(const Value: Boolean);
begin
  FItemFocused := Value;
  Changed;
end;

procedure TInspectorItem.SetLevel(const Value: Integer);
begin
  FLevel := Value;
  Changed;
end;

procedure TInspectorItem.SetMaxLength(const Value: Integer);
begin
  FMaxLength := Value;
  Changed;
end;

procedure TInspectorItem.SetModified(const Value: Boolean);
begin
  FModified := Value;
  Changed;
end;

procedure TInspectorItem.SetNodeState(const Value: TItemNodeState);
begin
  FNodeState := Value;
  if FNodeState = nsClose then
    CollapsChilds
  else
    ExpandChilds;
end;

procedure TInspectorItem.SetPictureValue(const Value: TPicture);
begin
  FPictureValue.Assign(Value);
  Changed;
end;

procedure TInspectorItem.SetPropertyType(const Value: TPropertyType);
begin
  FPropertyType := Value;

  case FPropertyType of
    ptColor, ptFixedColor:
      TextValue := 'clBlack';
    ptBoolean:
      TextValue := InspectorBar.CheckFalse;
    ptFloat:
      TextValue := '0.0';
    ptInteger, ptIntSpin:
      TextValue := '0';
  else
    TextValue := '';
  end;

  Changed;
end;

procedure TInspectorItem.SetSelected(const Value: Boolean);
begin
  FSelected := Value;
  Changed;
end;

procedure TInspectorItem.SetShortCut(const Value: TShortCut);
var
  i: integer;
  P: TinspectorPanel;
begin
  p := TInspectorItems(GetOwner).FOwner;
  if (p.fshortcut = value) and (value <> 0) then
  begin
    ShowMessage('ShortCut already assigned to panel');
    Exit;
  end;

  for i := 0 to p.FItems.Count - 1 do
  begin
    if (p.FItems[i].FShortCut = Value) and (value <> 0)
      then
    begin
      ShowMessage('ShortCut already assigned to another item [' + inttostr(i) + ']');
      Exit;
    end;
  end;

  FShortCut := Value;

end;

procedure TInspectorItem.SetTextValue(const Value: string);
begin
  FTextValue := Value;
  Changed;
end;

procedure TInspectorItem.SetTimeValue(const Value: TDateTime);
begin
  FDateTime := Value;

  if FDateTimeFormat <> '' then
    FTextValue := FormatDateTime(FDateTimeFormat, Value)
  else
    FTextValue := TimeToStr(Value);
  Changed;
end;

procedure TInspectorItem.SetURL(const Value: string);
begin
  FURL := Value;
  Changed;
end;

procedure TInspectorItem.SetValues(const Value: TStringList);
begin
  FValues.Assign(Value);
end;

procedure TInspectorItem.SetVisible(const Value: Boolean);
begin
  FVisible := Value;
  InspectorBar.Invalidate;
end;

procedure TInspectorItem.Update;
begin

end;

{ TInspectorBar }

procedure TInspectorBar.CaptionChanged(Sender: TObject);
begin
  Invalidate;
end;

constructor TInspectorBar.Create(AOwner: TComponent);
var
  dwVersion: Dword;
  dwWindowsMajorVersion, dwWindowsMinorVersion: Dword;
begin
  inherited;
  FUpdateCount := 0;
  FEditInplace := TEditAlign.Create(nil);
  FEditInplace.Parent := Self;
  FEditInplace.BorderStyle := bsNone;
  FEditInplace.Color := clGreen;
  FEditInplace.OnKeyPress := LabelInplaceKeyPress;
  FEditInplace.OnExit := LabelInplaceExit;
  FEditInplace.Visible := False;

  FMemoInplace := TMemo.Create(nil);

  with FMemoInplace do
  begin
    Parent := Self;
    BorderStyle := bsNone;
    Color := clGreen;
    OnKeyPress := LabelInplaceKeyPress;
    OnExit := LabelInplaceExit;
    Visible := False;
    WantReturns := False;
    WordWrap := True;
    ScrollBars := ssNone;
  end;

  FHideMemo := TMemo.Create(nil);
  with FHideMemo do
  begin
    Parent := Self;
    BorderStyle := bsNone;
    Color := clGreen;
    OnKeyPress := LabelInplaceKeyPress;
    OnExit := LabelInplaceExit;
    Visible := False;
    WantReturns := False;
    WordWrap := True;
    ScrollBars := ssNone;
  end;

  FPanels := CreatePanels;
  Width := 180;
  Align := alLeft;
  dwVersion := GetVersion;
  dwWindowsMajorVersion := DWORD(LOBYTE(LOWORD(dwVersion)));
  dwWindowsMinorVersion := DWORD(HIBYTE(LOWORD(dwVersion)));

  FIsWinXP := (dwWindowsMajorVersion > 5) or
    ((dwWindowsMajorVersion = 5) and (dwWindowsMinorVersion >= 1));

  FInspectorCaption := TInspectorCaption.Create;
  FInspectorCaption.OnChange := CaptionChanged;
  FInspectorCaption.OnShadeChange := ShadeChanged;
  FInspectorCaption.InspectorBar := Self;
  FInspectorHelp := TInspectorHelp.Create;
  FInspectorHelp.OnChange := CaptionChanged;

  FScrollBar := TScrollBar.Create(nil);
  FScrollBar.Parent := Self;

  FScrollBar.Align := alRight;
  FScrollBar.Visible := False;
  FScrollBar.Position := 0;
  FScrollBar.Kind := sbVertical;
  FScrollBar.OnChange := Scroll;
  FScrollBar.OnKeyDown := ScrollKeydown;
  DoubleBuffered := True;
  {$IFDEF DELPHI_UNICODE}
  FScrollBar.DoubleBuffered := false;
  {$ENDIF}
  FShadedHeader := TBitmap.Create;
  ShadeHeader;
  FInspectorEdit := TInspectorEdit.Create(Self);
  FInspectorEdit.OnDblClick := EditDblClick;
  FInspectorEdit.OnChange := EditChanged;
  FInspectorCombo := TInspectorCombo.Create(Self);
  FInspectorCombo.IsWinXP := FIsWinXP;
  FInspectorCombo.OnDblClick := EditDblClick;
  FInspectorCombo.OnChange := ComboChanged;

  FInspectorColorCombo := TInspectorColorCombo.Create(Self);
  FInspectorColorCombo.IsWinXP := FIsWinXP;
  FInspectorColorCombo.Style := csOwnerDrawFixed;
  FInspectorColorCombo.OnDblClick := EditDblClick;
  FInspectorColorCombo.OnChange := ColComboChanged;

  FInspectorSpin := TInspectorSpin.Create(Self);
  FInspectorSpin.IsWinXP := FIsWinXP;
  FInspectorSpin.OnDblClick := EditDblClick;
  FInspectorSpin.OnChange := EditChanged;
  FInspectorSpin.OnSpinUp := SpinUp;
  FInspectorSpin.OnSpinDown := SpinDown;
  FInspectorSpin.OnSpinChange := SpinChange;

  FInspectorEditBtn := TInspectorEditBtn.Create(Self);
  FInspectorEditBtn.IsWinXP := FIsWinXP;
  FInspectorEditBtn.OnDblClick := EditDblClick;
  FInspectorEditBtn.OnClickBtn := EditBtnClick;
  FInspectorEditBtn.OnChange := EditChanged;
  FInspectorDateTimePicker := TInspectorDateTimePicker.Create(Self);
  FInspectorDateTimePicker.OnChange := EditChanged;
  OnExit := CaptionChanged;
  OnEnter := CaptionChanged;
  FOnPanelOpen := nil;
  FOnPanelClose := nil;
  FMousePanel := -1;
  FMouseItem := nil;

  FCheckTrue := 'True';
  FCheckFalse := 'False';
  FCheckTextShow := True;

  FInspectorDropTarget := nil;
  FOleDropTargetAssigned := False;

  FRepeatTimer := TTimer.Create(Self);
  FRepeatTimer.Interval := 150;
  FRepeatTimer.OnTimer := RepeatTimer;
  FRepeatTimer.Enabled := True;

  TabStop := True;

  FHoverColorTo := clNone;
  FColorTo := clNone;
  FStyle := esCustom;
  SetStyle(esCustom);

  FNodeOpenGlyph := TBitmap.Create;
  FNodeOpenGlyph.LoadFromResourceName(HInstance, 'INSPNODEOPEN');
  FNodeCloseGlyph := TBitmap.Create;
  FNodeCloseGlyph.LoadFromResourceName(HInstance, 'INSPNODECLOSE');
end;

destructor TInspectorBar.Destroy;
begin
  if Assigned(FInspectorDropTarget) and FOleDropTargetAssigned then
    FInspectorDropTarget.Free;
  FInspectorDropTarget := nil;
  FEditInplace.Free;
  FMemoInplace.Free;
  FHideMemo.Free;
  FPanels.Free;
  FPanels := nil;
  FInspectorCaption.Free;
  FShadedHeader.Free;
  FInspectorEdit.Free;
  FInspectorCombo.Free;
  FInspectorColorCombo.Free;
  FInspectorSpin.Free;
  FInspectorEditBtn.Free;
  FInspectorDateTimePicker.Free;
  FInspectorHelp.Free;
  FRepeatTimer.Free;
  FNodeOpenGlyph.Free;
  FNodeCloseGlyph.Free;
  inherited;
end;

procedure TInspectorBar.WMDestroy(var Message: TMessage);
begin
  if OleDropTarget then
    OleDropTarget := False;
  inherited;
end;

function TInspectorBar.CreatePanels: TInspectorPanels;
begin
  Result := TInspectorPanels.Create(Self);
end;

procedure TInspectorBar.DrawCaption(Panel: TInspectorPanel; R: TRect);
var
  DWSTYLE: DWord;
  i, ind: Integer;
  BorderColor1, BorderColor2: TColor;
  pt: TPoint;
  tRgn, rgn1, rgn2: HRGN;
  DefaultDraw: Boolean;
  HTheme: THandle;
  Hot: Boolean;
  Down: Boolean;
  a, s, fa: string;
  xs, ys, ml, hl: Integer;
  mr, ARect: TRect;
  ocgraphic: TOpenCloseGraphic;
  realtoppanel: integer;

begin
  DefaultDraw := True;

  if Assigned(FOnPanelDraw) then
    FOnPanelDraw(Self, Panel, Canvas, R, DefaultDraw);

  realtoppanel := getRealTopPanel;

  if not DefaultDraw then
    Exit;

  ARect := R;
  Canvas.Brush.Color := Panel.Color;
  Canvas.Pen.Color := Panel.Color;
  Canvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom); //background
  Canvas.Brush.Style := bsClear;
  tRgn := 0;
  rgn1 := 0;
  rgn2 := 0;

// Set the contour colors

  GetCursorPos(pt);
  pt := ScreenToClient(pt);

  if PtInRect(R, pt) and FMouseDown and PanelCaption.Button then
  begin
    BorderColor1 := clGray;
    BorderColor2 := clWhite;
  end
  else
  begin
    BorderColor1 := clWhite;
    BorderColor2 := clGray;
  end;

  if (PanelCaption.ShadeType = stXPButton) and DoVisualStyles then
  begin
    HTheme := OpenThemeData(Handle, 'button');
    InflateRect(r, 1, 1);
    Hot := Panel.Index = FMousePanel;
    Down := Hot and FMouseDown;

    if Down then
      DrawThemeBackground(HTheme, Canvas.Handle, BP_PUSHBUTTON, PBS_PRESSED, @r, nil)
    else
      if Hot then
        DrawThemeBackground(HTheme, Canvas.Handle, BP_PUSHBUTTON, PBS_HOT, @r, nil)
      else
        DrawThemeBackground(HTheme, Canvas.Handle, BP_PUSHBUTTON, PBS_NORMAL, @r, nil);
    CloseThemeData(HTheme);
  end;



  if (PanelCaption.ShadeType in [stBitmap, stBitmapLStretch, stBitmapRStretch]) then
  begin
    Hot := Panel.Index = realtoppanel;

    if not PanelCaption.Background.Empty and not Hot then
    begin
      if PanelCaption.ShadeType = stBitmapLStretch then
      begin
        ind := Width - PanelCaption.Background.Width;
        if ind < 0 then ind := 0;
        Canvas.Draw(r.Left + Ind, r.Top, PanelCaption.Background);

        Canvas.CopyRect(Rect(0, r.Top, Ind, R.Bottom), PanelCaption.Background.Canvas,
          Rect(0, 0, 2, PanelCaption.Background.Height));
      end;

      if PanelCaption.ShadeType = stBitmapRStretch then
      begin
        ind := PanelCaption.Background.Width;

        Canvas.Draw(r.Left, r.Top, PanelCaption.Background);

        Canvas.CopyRect(Rect(ind, r.Top, R.Right, R.Bottom), PanelCaption.Background.Canvas,
          Rect(PanelCaption.Background.Width - 3, 0, PanelCaption.Background.Width, PanelCaption.Background.Height));
      end;

      if PanelCaption.ShadeType = stBitmap then
      begin
        Canvas.StretchDraw(r, PanelCaption.Background);
      end;
    end;

    if not PanelCaption.ActiveBackground.Empty and Hot then
    begin
      if PanelCaption.ShadeType = stBitmapLStretch then
      begin
        ind := Width - PanelCaption.Background.Width;
        if ind < 0 then ind := 0;
        Canvas.Draw(r.Left + Ind, r.Top, PanelCaption.ActiveBackground);
        Canvas.CopyRect(Rect(0, r.Top, Ind, R.Bottom), PanelCaption.ActiveBackground.Canvas,
          Rect(0, 0, 2, PanelCaption.ActiveBackground.Height));
      end;

      if PanelCaption.ShadeType = stBitmapRStretch then
      begin
        ind := PanelCaption.Background.Width;

        Canvas.Draw(r.Left, r.Top, PanelCaption.ActiveBackground);
        Canvas.CopyRect(Rect(ind, r.Top, R.Right, R.Bottom), PanelCaption.ActiveBackground.Canvas,
          Rect(PanelCaption.ActiveBackground.Width - 3, 0, PanelCaption.ActiveBackground.Width, PanelCaption.ActiveBackground.Height));
      end;

      if PanelCaption.ShadeType = stBitmap then
      begin
        Canvas.StretchDraw(r, PanelCaption.ActiveBackground);
      end;
    end;
  end;

  if not ((PanelCaption.ShadeType in [stBitmap, stBitmapLStretch, stBitmapRStretch]) or
    ((PanelCaption.ShadeType = stXPButton) and DoVisualStyles)) then
  begin
    case PanelCaption.Shape of
      csRectangle:
        begin
          if not PanelCaption.Flat then
          begin
            Canvas.Pen.Color := BorderColor2; //lines for 3D effect
            Canvas.MoveTo(R.Left, R.Bottom - 1);
            Canvas.LineTo(R.Right - 1, R.Bottom - 1);
            Canvas.LineTo(R.Right - 1, R.Top);
            Canvas.Pen.Color := BorderColor1;
            Canvas.LineTo(R.Left, R.Top);
            Canvas.LineTo(R.Left, R.Bottom - 1);
            InflateRect(R, -1, -1);
          end;

          tRgn := CreateRectRgn(R.Left, R.Top, R.Right, R.Bottom);
        end; //standard rectangle
      csRounded:
        begin
          Canvas.Pen.Color := BorderColor2; //Round Rects for 3D effect
          tRgn := CreateRoundRectRgn(R.Left, R.Top + (r.Bottom - r.Top) div 2, R.Right, R.Bottom, 32, 32);
          SelectClipRgn(Canvas.Handle, tRgn);
          Canvas.RoundRect(R.Left, R.Top, R.Right, R.Bottom, 32, 32);
          Canvas.Pen.Color := BorderColor1;
          tRgn := CreateRoundRectRgn(R.Left, R.Top, R.Right, R.Bottom - (r.Bottom - r.Top) div 2, 32, 32);
          SelectClipRgn(Canvas.Handle, 0);
          DeleteObject(tRgn);

          Canvas.RoundRect(R.Left, R.Top, R.Right, R.Bottom, 32, 32);
          tRgn := CreateRectRgn(0, 0, Width, height);
          SelectClipRgn(Canvas.Handle, tRgn);
          Canvas.Pen.Color := BorderColor2;
          Canvas.MoveTo(R.Left + 16, r.Bottom - 1);
          Canvas.LineTo(r.Right - 16, r.Bottom - 1);
          R.Top := R.Top + 1;
          SelectClipRgn(Canvas.Handle, 0);
          DeleteObject(tRgn);
          tRgn := CreateRoundRectRgn(R.Left, R.Top, R.Right, R.Bottom, 32, 32);
        end; //round rectangle
      csSemiRounded:
        begin
          Canvas.Pen.Color := BorderColor2; //Round Rects for 3D effect
          tRgn := CreateRoundRectRgn(R.Left, R.Top + (r.Bottom - r.Top) div 2, R.Right, R.Bottom, 32, 32);
          SelectClipRgn(Canvas.Handle, tRgn);
          Canvas.RoundRect(R.Left, R.Top, R.Right, R.Bottom, 32, 32);
          Canvas.Pen.Color := BorderColor1;
          SelectClipRgn(Canvas.Handle, 0);
          DeleteObject(tRgn);
          tRgn := CreateRoundRectRgn(R.Left, R.Top, R.Right, R.Bottom - (r.Bottom - r.Top) div 2, 32, 32);
          SelectClipRgn(Canvas.Handle, tRgn);
          Canvas.RoundRect(R.Left, R.Top, R.Right, R.Bottom, 32, 32);
          SelectClipRgn(Canvas.Handle, 0);
          DeleteObject(tRgn);
          tRgn := CreateRectRgn(0, 0, Width, height);
          SelectClipRgn(Canvas.Handle, tRgn);
          Canvas.Pen.Color := BorderColor1; //Lines for 3D effect
          Canvas.MoveTo(R.Left + (r.Right - r.Left) div 2, r.Top);
          Canvas.LineTo(R.Left, r.Top);
          Canvas.LineTo(R.Left, r.Bottom - 1);
          Canvas.Pen.Color := BorderColor2;
          Canvas.LineTo(r.Right - 16, r.Bottom - 1);
          SelectClipRgn(Canvas.Handle, 0);
          DeleteObject(tRgn);
          R.Top := R.Top + 1;
          R.Left := R.Left + 1;
          rgn1 := CreateRoundRectRgn(R.Left, R.Top, R.Right, R.Bottom, 32, 32);
          rgn2 := CreateRectRgn(R.Left, R.Top, R.Right - (r.Right - r.Left) div 2, R.Bottom - 1);
          CombineRgn(tRgn, rgn1, rgn2, RGN_OR); //round rectangle + rectangle
        end;
    end;

    SelectClipRgn(Canvas.Handle, tRgn); //Set the Canvas Clip region
    Canvas.Draw(R.Left, R.Top, FShadedHeader); //Put the shade
  end;

  ocGraphic := PanelCaption.OpenCloseGraphic;

  if (Mode = imMultiPanelActive) and
    (Panel.MultiPanelState = mpsAlwaysOpen) then
    ocGraphic := ocgNone;

  if (PanelCaption.OpenClosePosition = ocpLeft) and
    (ocGraphic <> ocgNone) then
  begin
    pt := Point(R.Left, R.Top);
    R.Left := R.Left + 16;
  end
  else
    pt := Point(R.Right - 20, R.Top);

  if Panel.FIsOpen then
  begin
    case ocGraphic of
      ocgGlyph:
        begin
          if not FInspectorCaption.FGlyphOpen.Empty then
          begin
            FInspectorCaption.FGlyphOpen.TransparentMode := tmAuto;
            FInspectorCaption.FGlyphOpen.Transparent := True;
            Canvas.Draw(pt.X, pt.Y, FInspectorCaption.FGlyphOpen)
          end;
        end;
      ocgCross:
        begin
          Canvas.Draw(pt.X + 4, pt.Y + 4, FNodeCloseGlyph);
         { Canvas.Pen.Color := clBlack;
          Canvas.Pen.Width := 1;
          Canvas.MoveTo(pt.X + 4, pt.Y + 4);
          Canvas.LineTo(pt.X + 12, pt.Y + 4);
          Canvas.LineTo(pt.X + 12, pt.Y + 12);
          Canvas.LineTo(pt.X + 4, pt.Y + 12);
          Canvas.LineTo(pt.X + 4, pt.Y + 4);
          Canvas.MoveTo(pt.X + 6, pt.Y + 8);
          Canvas.LineTo(pt.X + 11, pt.Y + 8);
         }
        end;
      ocgTriangle:
        begin
          Canvas.Brush.Color := clBlack;
          Canvas.Pen.Color := clBlack;
          Canvas.Pen.Width := 1;
          Canvas.Polygon([Point(pt.X + 4, pt.Y + 4), Point(pt.X + 10, pt.Y + 4), Point(pt.X + 7, pt.Y + 7)]);
        end;
    end;
  end
  else
  begin
    case ocGraphic of
      ocgGlyph:
        begin
          if not FInspectorCaption.FGlyphClose.Empty then
          begin
            FInspectorCaption.FGlyphClose.TransparentMode := tmAuto;
            FInspectorCaption.FGlyphClose.Transparent := True;
            Canvas.Draw(pt.X, pt.Y, FInspectorCaption.FGlyphClose)
          end;
        end;
      ocgCross:
        begin
          Canvas.Draw(pt.X + 4, pt.Y + 4, FNodeOpenGlyph);
          {Canvas.Pen.Color := clBlack;
          Canvas.Pen.Width := 1;
          Canvas.MoveTo(pt.X + 4, pt.Y + 4);
          Canvas.LineTo(pt.X + 12, pt.Y + 4);
          Canvas.LineTo(pt.X + 12, pt.Y + 12);
          Canvas.LineTo(pt.X + 4, pt.Y + 12);
          Canvas.LineTo(pt.X + 4, pt.Y + 4);
          Canvas.MoveTo(pt.X + 6, pt.Y + 8);
          Canvas.LineTo(pt.X + 11, pt.Y + 8);
          Canvas.MoveTo(pt.X + 8, pt.Y + 6);
          Canvas.LineTo(pt.X + 8, pt.Y + 11);
          }
        end;
      ocgTriangle:
        begin
          Canvas.Brush.Color := clBlack;
          Canvas.Pen.Color := clBlack;
          Canvas.Pen.Width := 1;
          Canvas.Polygon([Point(pt.X + 4, pt.Y + 4), Point(pt.X + 4, pt.Y + 10), Point(pt.X + 7, pt.Y + 7)]);
        end;
    end;
  end;

  if (FImages <> nil) and (Panel.FImageIndex <> -1) then //Add image
  begin
    i := ((R.Bottom - R.Top - FImages.Height) div 2);
    if i < 0 then
      i := R.Top
    else
      i := R.Top + i;

    FImages.Draw(Canvas, R.Left, i, Panel.FImageIndex);
    R.Left := R.Left + FImages.Width;
  end;

  R.Left := R.Left + 4;

  SelectClipRgn(Canvas.Handle, 0); //Restore the Canvas Clip region
  if tRgn <> 0 then
    DeleteObject(tRgn);

  if PanelCaption.Shape = csSemiRounded then
  begin
    DeleteObject(rgn1);
    DeleteObject(rgn2);
  end;

  DrawCaptionOptions(Panel, Canvas, R);

  if Panel.WordWrapCaption then
    DWSTYLE := DT_WORDBREAK or DT_NOPREFIX
  else
    DWSTYLE := DT_SINGLELINE or DT_NOPREFIX;

  if FEllipsis then
    DWSTYLE := DWSTYLE or DT_END_ELLIPSIS;

  case PanelCaption.Alignment of
    taLeftJustify: DWSTYLE := DWSTYLE or DT_LEFT;
    taRightJustify: DWSTYLE := DWSTYLE or DT_RIGHT;
    taCenter: DWSTYLE := DWSTYLE or DT_CENTER;
  end;

  case PanelCaption.VAlignment of
    vaCenter: DWSTYLE := DWSTYLE or DT_VCENTER;
    vaTop: DWSTYLE := DWSTYLE or DT_TOP;
    vaBottom: DWSTYLE := DWSTYLE or DT_BOTTOM;
  end;

  if Panel.Index = realtoppanel then
    Canvas.Font.Assign(PanelCaption.ActiveFont)
  else
    Canvas.Font.Assign(PanelCaption.Font);

  Canvas.Brush.Style := bsClear;

  OffsetRect(R, PanelCaption.Indent, 0);

  if IsHTML(Panel.Caption) then
    HTMLDrawEx(Canvas, Panel.Caption, R, Images, r.Left, r.Top, -1, -1, 2, False, False, False, False, False, False, True,
      1.0, clBlue, clNone, clNone, clNone, a, s, fa, xs, ys, ml, hl, mr)
  else
    DrawText(Canvas.Handle, PChar(Panel.Caption), Length(Panel.Caption), R, DWSTYLE);

  if PanelCaption.Underline then
  begin
    Canvas.Pen.Color := PanelCaption.UnderlineColor;
    Canvas.Pen.Width := PanelCaption.UnderlineWidth;

    Canvas.MoveTo(ARect.Left, ARect.Bottom - PanelCaption.UnderlineWidth);
    Canvas.LineTo(ARect.Right, ARect.Bottom - PanelCaption.UnderlineWidth);
  end;
end;

procedure TInspectorBar.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  R, mr: TRect;
  Panel: TInspectorPanel;
  Item: TInspectorItem;
  NewHint: string;
  a, s, fa: string;
  ml, hl: Integer;
  xs, ys: Integer;
  frm: TCustomForm;

begin
  inherited;

  frm := GetParentForm(self);

  if (frm is TForm) and ((frm as TForm).FormStyle = fsMDIChild) then
  begin
  end
  else
    if GetParentForm(self).Handle <> GetActiveWindow then
      Exit;

  if Panels.Count = 0 then
    Exit;

  if ShowHint then
  begin
    NewHint := GetHint(x, y);
    if NewHint <> FOldHint then
      Application.CancelHint;
    FOldHint := NewHint;
  end;

  if GetPanelItemAtXY(x, y, Panel, Item) then
  begin
    if (Panel.GridLines) and (Panel.Style = psProperties) and Panel.AllowResize then
    begin
      if (Cursor = crHSplit) and FMouseDown then
      begin
        if PanelCaption.SideDisplay then
          Panel.CaptionWidth := x - PanelCaption.SideWidth
        else
          Panel.CaptionWidth := x;
        Exit;
      end;

      if not PanelCaption.SideDisplay then
      begin
        if Abs(X - Panel.CaptionWidth) < 4 then
          Cursor := crHSplit
        else
          Cursor := FDefCursor;
      end
      else
      begin
        if Abs(X - PanelCaption.SideWidth - Panel.CaptionWidth) < 4 then
          Cursor := crHSplit
        else
          Cursor := FDefCursor;
      end;
    end
    else
      Cursor := FDefCursor;

    if Item.HotAnchor <> '' then
      Cursor := crHandPoint;
  end
  else
  begin
    Cursor := FDefCursor;
  end;


  if (FInspectorCaption.Cursor <> FDefCursor) then
  begin
    if (Mode = imSinglePanelActive) and GetCursorSingle(x, y) or
      (Mode = imMultiPanelActive) and GetCursorMulti(x, y) then
    begin //Save the Bar cursor if needed
      if FInspectorCaption.FBarCursor = FDefCursor then
        FInspectorCaption.FBarCursor := Cursor;
      Cursor := FInspectorCaption.Cursor;
    end
    else
    begin //Restore the Bar cursor if needed
      if FInspectorCaption.FBarCursor <> FDefCursor then
        Cursor := FInspectorCaption.FBarCursor;
      FInspectorCaption.FBarCursor := FDefCursor;
    end;
  end;

// Invalidate old mouse-over item

  if Assigned(Item) then
  begin
    if (Item.InspectorPanel.HoverIconColor = clNone) then
    begin
      if (Item <> FMouseItem) and Assigned(FMouseItem) then
      begin
        r := GetItemRect(FMouseItem.InspectorPanel, FMouseItem);
        {$IFDEF VER185}
        Invalidate;
        {$ELSE}
        InflateRect(r, 4, 4);
        //InvalidateRect(Handle, @r, False);
        Invalidate;
        {$ENDIF}
      end;

      if Assigned(Item) then
      begin
        r := GetItemRect(Panel, Item);
        {$IFDEF VER185}
        Invalidate;
        {$ELSE}
        InflateRect(r, 4, 4);
        InvalidateRect(Handle, @r, False);
        {$ENDIF}
      end;
    end;
  end
  else
    if Assigned(FMouseItem) then
    begin
      if Assigned(FMouseItem) then
      begin
        r := GetItemRect(FMouseItem.InspectorPanel, FMouseItem);

        {$IFDEF VER185}
        Invalidate;
        {$ELSE}
        InflateRect(r, 4, 4);
        InvalidateRect(Handle, @r, False);
        {$ENDIF}
      end;
    end;

  Panel := GetCaption(X, Y);

  if Assigned(Panel) then
  begin
    if (FMousePanel <> Panel.Index) or MouseMoveCaptionOptions(Panel, X, Y) then
    begin
      if FMousePanel <> -1 then
      begin
        r := GetCaptionRect(Panels[FMousePanel]);
        {$IFDEF VER185}
        Invalidate;
        {$ELSE}
        InvalidateRect(Handle, @r, False);
        {$ENDIF}
      end;
      FMousePanel := Panel.Index;
      r := GetCaptionRect(Panels[FMousePanel]);
      {$IFDEF VER185}
      Invalidate;
      {$ELSE}
      InvalidateRect(Handle, @r, False);
      {$ENDIF}
    end;
  end
  else
  begin
    if FMousePanel <> -1 then
    begin
      r := GetCaptionRect(Panels[FMousePanel]);
      {$IFDEF VER185}
      Invalidate;
      {$ELSE}
      InvalidateRect(Handle, @r, False);
      {$ENDIF}
      FMousePanel := -1;
    end;
  end;

  if (Y > GetBarHeight) and HelpPanel.Visible then
  begin
    R := Rect(0, GetBarHeight, HelpWidth, Height);
    InflateRect(R, 0, -1);

    if IsHTML(HelpPanel.Text) then
    begin
      FHotAnchor := '';
      if HTMLDrawEx(Canvas, HelpPanel.Text, R, Images, X, Y, -1, -1, 2, True, False, False, False, False, False, True,
        1.0, clBlue, clNone, clNone, clNone, a, s, fa, xs, ys, ml, hl, mr) then
        FHotAnchor := a;
      if FHotAnchor <> '' then
        Cursor := crHandPoint
      else
        Cursor := FDefCursor;
    end;
  end;

  FMouseItem := Item;

  if Assigned(Item) then
  begin
    if (Item.InspectorPanel.HoverIconColor <> clNone) then
      Invalidate;
  end;

end;

procedure TInspectorBar.MouseUpSinglePanel(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  i, j, realtoppanel: Integer;

begin
  if (FPanels.Count = 0) or (getNumbervisiblePanelUp(FPanels.Count) = -1) then
    Exit;

  realtoppanel := GetRealTopPanel;

  //------ Panel Caption Click-------------
  for i := 0 to FPanels.Count - 1 do
  begin
    if not FPanels.Items[i].Visible then
      Continue;

    if i <= realtoppanel then
      j := (getNumbervisiblePanelUp(i)) * PanelCaption.Height
    else
      j := BarHeight - ((getNumbervisiblePanelDown(i) + 1) * PanelCaption.Height);

    if (y >= j) and (y <= j + PanelCaption.Height) then
    begin
      if (Button = mbLeft) then
      begin
        if MouseMoveCaptionOptions(FPanels.Items[i], X, Y) then
          Break;

        PanelCaptionClick(FPanels.Items[i]);

        {
        if realtoppanel <> i then
        begin
          PanelOpened(FPanels.Items[i]);
          PanelClosed(FPanels.Items[RealTopPanel]);
          TopPanel := i;
          AfterPanelOpened(FPanels.Items[i]);
        end;
        }

      end;
      if (Button = mbRight) then
      begin
        if MouseMoveCaptionOptions(FPanels.Items[i], X, Y) then
          Break;
        PanelCaptionRightClick(FPanels.Items[i]);
      end;
      Break;
    end;
  end;
end;

procedure TInspectorBar.MouseDownSinglePanel(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  i, j, k, l, m, RealWidth, realtoppanel, h: Integer;
  Hover: Boolean;
  tempHeight: Integer;
  Item: TInspectorItem;
  FMDown: Boolean;
  ir: TRect;

begin
  if (FPanels.Count = 0) or (getNumbervisiblePanelUp(FPanels.Count) = -1) then
    Exit;

  realtoppanel := GetRealTopPanel;

  //------ Panel Caption Click-------------
  for i := 0 to FPanels.Count - 1 do
  begin
    if not FPanels.Items[i].Visible then
      Continue;

    if i <= realtoppanel then
      j := (getNumbervisiblePanelUp(i)) * PanelCaption.Height
    else
      j := BarHeight - ((getNumbervisiblePanelDown(i) + 1) * PanelCaption.Height);

    if (y >= j) and (y <= j + PanelCaption.Height) then
    begin
      if Button = mbLeft then
      begin
        if MouseDownCaptionOptions(FPanels.Items[i], X, Y) then
          Break;

        if realtoppanel <> i then
        begin
          PanelOpened(FPanels.Items[i]);
          PanelClosed(FPanels.Items[RealTopPanel]);
          TopPanel := i;
          AfterPanelOpened(FPanels.Items[i]);
          Exit;
        end;
      end;
      Break;
    end;
  end;

//------ Buttons for scroll  Click-------------
  j := (GetNumbervisiblePanelup(realtoppanel) + 1) * PanelCaption.Height; //Click Up
  i := BarHeight - ((GetNumbervisiblePanelDown(realtoppanel)) * PanelCaption.Height); //Click Down

//-------Button for scroll 20x20---------------
  if (x >= self.Width - 20) and (x <= self.Width) and
    (y >= j) and (y <= j + 20) and FShowUP then
  begin
    FPanels[realtoppanel].TopItem := FPanels[realtoppanel].FTopItem - 1;
    k := FPanels[realtoppanel].TopItem;

    repeat
      if not FPanels[realtoppanel].FItems[k].FVisible then
        Dec(k);
      if k <= 0 then
        Break;
    until (FPanels[realtoppanel].FItems[k].FVisible);

    if k >= 0 then
      FPanels[realtoppanel].TopItem := k
    else
      FPanels[realtoppanel].TopItem := 0;

    FButtonUpPress := True;
    FButtonRect := Rect(Width - 20, j, Width, j + 20);

    DrawButtonUp;
    Exit;
  end;

  if (x >= self.Width - 20) and (x <= self.Width) and
    (y <= i) and (y >= i - 20) and FShowDown then
  begin
    FPanels[realtoppanel].TopItem := FPanels[realtoppanel].FTopItem + 1;

    k := FPanels[realtoppanel].TopItem;

    repeat
      if not FPanels[realtoppanel].FItems[k].FVisible then
        inc(k);
      if k >= FPanels[realtoppanel].FItems.Count then
        Break;
    until (FPanels[realtoppanel].FItems[k].FVisible);

    if k < FPanels[realtoppanel].FItems.Count then
      FPanels[realtoppanel].TopItem := k
    else
      FPanels[realtoppanel].TopItem := FPanels[realtoppanel].FItems.Count - 1;

    FButtonDownPress := True;

    FButtonRect := Rect(Width - 20, i - 20, Width, i);

    DrawButtonDown;
    Exit;
  end;

  Hover := FPanels[realtoppanel].HoverCaption;

  case FPanels[realtoppanel].Style of
    psLargeIcon:
      begin
        if (y > j) and (y < i) then
        begin
          FMouseDown := true;
          Repaint;
          tempHeight := j;
          for m := FPanels[realtoppanel].TopItem to FPanels[realtoppanel].Items.Count do
          begin
            k := m;
            if k >= FPanels[realtoppanel].Items.Count then
              Break;
            if not FPanels[realtoppanel].Items[m].Visible then
              Continue;

            if FPanels[realtoppanel].VariableItemHeight
              then h := FPanels[realtoppanel].Items[m].Height
            else h := FPanels[realtoppanel].ItemHeight;

            if (((x >= 0) and (x <= width)) or Hover) and
              (tempHeight <= y) and (y <= tempHeight + h) then
            begin
              ItemClicked(Button, FPanels[realtoppanel], FPanels[realtoppanel].Items[k]);
              Break;
            end;
            Inc(tempHeight, h);
          end;
        end;

      end;
    psSmallIcon, psButtons:
      begin
        if (y > j) and (y < i) and ((x < Width) or Hover or (FPanels[realtoppanel].Style = psButtons)) then
        begin
          FMouseDown := true;
          Repaint;

          tempHeight := j;
          k := FPanels[realtoppanel].Items.Count;

          for m := FPanels[realtoppanel].TopItem to FPanels[realtoppanel].Items.Count do
          begin
            k := m;
            if k >= FPanels[realtoppanel].Items.Count then
              Break;
            if not FPanels[realtoppanel].Items[m].Visible then
              Continue;
            if FPanels[realtoppanel].VariableItemHeight then
              Inc(tempHeight, FPanels[realtoppanel].Items[m].Height)
            else
              Inc(tempHeight, FPanels[realtoppanel].ItemHeight);
            if tempHeight > y then
              Break;
          end;

          if (FPanels[realtoppanel].Style = psButtons) and (k < FPanels[realtoppanel].Items.Count) then
          begin
            Item := FPanels[realtoppanel].Items[k];

            ir := GetItemRect(FPanels[realtoppanel], Item);

            if ir.Bottom > FPanels[realtoppanel].GetPanelHeight then
              FPanels[realtoppanel].TopItem := FPanels[realtoppanel].TopItem + 1;
          end;

          if k < FPanels[realtoppanel].Items.Count then
            ItemClicked(Button, FPanels[realtoppanel], FPanels[realtoppanel].Items[k]);

          if FPanels[realtoppanel].Style = psButtons then
          begin
            for l := 1 to FPanels[realtoppanel].Items.Count do
              if l - 1 <> k then
                FPanels[realtoppanel].Items[l - 1].Down := False;

            if k < FPanels[realtoppanel].Items.Count then
              FPanels[realtoppanel].Items[k].Down := not FPanels[realtoppanel].Items[k].Down;
          end;
        end;
      end;
    psProperties:
      begin
        if (y > j) and (y < i) and
          (Cursor <> crHSplit) then
        begin
          tempHeight := j;

          k := FPanels[realtoppanel].Items.Count;
          for m := FPanels[realtoppanel].TopItem to FPanels[realtoppanel].Items.Count do
          begin
            k := m;
            if k >= FPanels[realtoppanel].Items.Count then
              Break;

            if not FPanels[realtoppanel].Items[m].Visible then
              Continue;

            if FPanels[realtoppanel].VariableItemHeight then
              Inc(tempHeight, FPanels[realtoppanel].Items[m].Height)
            else
              Inc(tempHeight, FPanels[realtoppanel].ItemHeight);

            if tempHeight > y then
              Break;
          end;

          if k < FPanels[realtoppanel].Items.Count then
          begin
            Item := FPanels[realtoppanel].Items[k];
            if (x < FPanels[realtoppanel].Indent + InspectorIndent + (Item.Level) * 12) and
              (x > FPanels[realtoppanel].Indent + InspectorIndent + (Item.Level - 1) * 12) and
              (Item.NumChilds > 0) and (Button = mbLeft) then
            begin
              if Item.NodeState = nsOpen then
                Item.NodeState := nsClose
              else
                Item.NodeState := nsOpen;

              if Assigned(FOnItemOpen) and (Item.NodeState = nsOpen) then
                FOnItemOpen(Self, Item.InspectorPanel, Item);

              if Assigned(FOnItemClose) and (Item.NodeState = nsClose) then
                FOnItemClose(Self, Item.InspectorPanel, Item);
            end;
          end;
        end;

        tempHeight := j;

        k := FPanels[realtoppanel].Items.Count;

        for m := FPanels[realtoppanel].TopItem to FPanels[realtoppanel].Items.Count do
        begin
          k := m;
          if k >= FPanels[realtoppanel].Items.Count then
            Break;

          if not FPanels[realtoppanel].Items[m].Visible then
            Continue;

          if FPanels[realtoppanel].VariableItemHeight then
            Inc(tempHeight, FPanels[realtoppanel].Items[m].Height)
          else
            Inc(tempHeight, FPanels[realtoppanel].ItemHeight);

          if tempHeight > y then
            Break;
        end;

        if (y > j) and (y < i) and (x < FPanels[realtoppanel].CaptionWidth + InspectorIndent) and
          (Cursor <> crHSplit) and (k < FPanels[realtoppanel].Items.Count) then
        begin
          ItemClicked(mbLeft, FPanels[realtoppanel], FPanels[realtoppanel].Items[k]);
          Exit;
        end;


        if (y > j) and (y < i) and (x > FPanels[realtoppanel].CaptionWidth + InspectorIndent) and
          (Cursor <> crHSplit) and (x < InspectorWidth) then
        begin
          RealWidth := InspectorWidth;

          if k < FPanels[realtoppanel].Items.Count then
          begin
            FMDown := FMouseDown;

            ItemClicked(Button, FPanels[realtoppanel], FPanels[realtoppanel].Items[k]);

            if FMDown and (FPanels[realtoppanel].Items[k].PropertyType = ptButton) then
            begin
              InvalidateItem(FPanels[realtoppanel], FPanels[realtoppanel].Items[k]);
              Exit;
            end;

            if FMDown and not FPanels[realtoppanel].Items[k].ReadOnly then
            begin
              StartEdit(FPanels[realtoppanel].Items[k]);
              if (x > RealWidth - 32) and ShowEditorAlways then
                case FEditType of
                  itCombo: FInspectorCombo.DroppedDown := True;
                  itColorCombo: FInspectorColorCombo.DroppedDown := True;
                  itPicker: PostMessage(FInspectorDateTimePicker.Handle, WM_KEYDOWN, VK_F4, 0);
                  itBtnEdit: EditBtnClick(Self);
                end;
            end;
          end;
        end;
      end;
  end;

  Invalidate;
end;

procedure TInspectorBar.MouseUpMultiPanel(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  i, j, k: Integer;
  tempHeight: integer;

begin
  if FPanels.Count = 0 then
    Exit;
  if FScrollBar.Visible
    then j := -FScrollBar.Position
  else j := 0;

  for i := 0 to FPanels.Count - 1 do
  begin
    if not FPanels[i].Visible then continue;
    if (y >= j) and (y <= j + PanelCaption.Height) then
    begin
  // if requested an empty panel does not expand
      if FPanels[i].FItems.Count = 0 then
        Break;

      if MouseMoveCaptionOptions(FPanels[i], X, Y) then
        Break;

      if (FPanels[i].MultiPanelState = mpsAlwaysOpen) and
        (FPanels[i].FIsOpen) then
        Break;

      if Button = mbLeft then
      begin
        PanelCaptionClick(FPanels[i]);
        Exit;
      end;

      if Button = mbRight then
      begin
        PanelCaptionRightClick(FPanels[i]);
        Exit;
      end;
      Break;
    end;

    Inc(j, PanelCaption.Height);

    if FPanels.Items[i].FIsOpen then
    begin
      for k := 1 to FPanels[i].Items.Count do
      begin
        if not FPanels[i].Items[k - 1].FVisible then
          Continue;

        if FPanels.Items[i].VariableItemHeight then
          tempHeight := FPanels.Items[i].Items[k - 1].Height
        else
          tempHeight := FPanels.Items[i].ItemHeight;

        inc(j, tempheight);
      end;
    end;
  end;
end;


procedure TInspectorBar.MouseDownMultiPanel(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  i, j, k, l, RealWidth: Integer;
  tempHeight: integer;
  Item: TInspectorItem;
  inItem: boolean;
begin
  if FPanels.Count = 0 then
    Exit;
  if FScrollBar.Visible
    then j := -FScrollBar.Position
  else j := 0;

  for i := 0 to FPanels.Count - 1 do
  begin
    if not FPanels[i].Visible then continue;
    if (y >= j) and (y <= j + PanelCaption.Height) then
    begin
  // if requested an empty panel does not expand
      if FPanels[i].FItems.Count = 0 then
        Break;

      if MouseDownCaptionOptions(FPanels[i], X, Y) then
        Break;

      if (FPanels[i].MultiPanelState = mpsAlwaysOpen) and
        (FPanels[i].FIsOpen) then
        Break;

      if Button = mbLeft then
      begin
        FPanels[i].FIsOpen := not (FPanels[i].FIsOpen); // open <-> close
        if (FPanels[i].FIsOpen) then
          PanelOpened(FPanels[i])
        else
          PanelClosed(FPanels[i]);

        ArrangeOpenClose; //Set the last open panel and the first opened panels
        if (FPanels[i].FIsOpen) then
          AfterPanelOpened(FPanels[i]);
        invalidate;
        Exit;
      end;

      Break;
    end;

// compute absolute coordinates from local coordinates
    if FPanels.Items[i].FIsOpen then
    begin
  //for open panel
      Inc(j, PanelCaption.Height); //Panel caption

  //Items
      for k := 1 to FPanels[i].Items.Count do
      begin
        if not FPanels[i].Items[k - 1].FVisible then
          Continue;

        if FPanels.Items[i].VariableItemHeight then
          tempHeight := FPanels.Items[i].Items[k - 1].Height
        else
          tempHeight := FPanels.Items[i].ItemHeight;

        inItem :=  (y >= j) and (y < j + tempHeight);

        if FPanels.Items[i].Style = psProperties then
        begin
          RealWidth := InspectorWidth;

          if inItem and (Cursor <> crHSplit) and (Button = mbLeft) then
          begin
            Item := FPanels[i].Items[k - 1];

            if (x < InspectorIndent + Item.InspectorPanel.Indent + (Item.Level) * 12) and
              (x > InspectorIndent + Item.InspectorPanel.Indent + (Item.Level - 1) * 12) and
              (Item.NumChilds > 0) then
            begin
              if Item.NodeState = nsOpen then
                Item.NodeState := nsClose
              else
                Item.NodeState := nsOpen;

              if Assigned(FOnItemOpen) and (Item.NodeState = nsOpen) then
                FOnItemOpen(Self, Item.InspectorPanel, Item);

              if Assigned(FOnItemClose) and (Item.NodeState = nsClose) then
                FOnItemClose(Self, Item.InspectorPanel, Item);
            end;
          end;

          if inItem and (x < FPanels[i].CaptionWidth + InspectorIndent) and
            (Cursor <> crHSplit) and (x > InspectorIndent) then
          begin
            ItemClicked(mbLeft, FPanels[i], FPanels[i].Items[k - 1]);
          end;

          if inItem
            and (x > FPanels[i].CaptionWidth + InspectorIndent) and (Cursor <> crHSplit)
            and (x < InspectorWidth)
            and not FPanels[i].Items[k - 1].ReadOnly then
          begin
            if FPanels.Items[i].Items[k - 1].PropertyType = ptButton then
            begin
              InvalidateItem(FPanels.Items[i], FPanels.Items[i].Items[k - 1]);
              Exit;
            end;

            StartEdit(FPanels.Items[i].Items[k - 1]);
            if (x > RealWidth - 32) and ShowEditorAlways then
              case FEditType of
                itCombo: FInspectorCombo.DroppedDown := True;
                itColorCombo: FInspectorColorCombo.DroppedDown := True;
                itPicker: PostMessage(FInspectorDateTimePicker.Handle, WM_KEYDOWN, VK_F4, 0);
                itBtnEdit: EditBtnClick(Self);
              end;
          end;
        end
        else
        begin
          if inItem and (x > 0) and (x < Width) then
          begin
            ItemClicked(Button, FPanels.Items[i], FPanels.Items[i].Items[k - 1]);

            if FPanels[i].Style = psButtons then
            begin
              for l := 1 to FPanels[i].Items.Count do
                if l - 1 <> k - 1 then FPanels[i].Items[l - 1].Down := False;
              FPanels[i].Items[k - 1].Down := not FPanels[i].Items[k - 1].Down;
            end;
          end;
        end;
        Inc(j, tempHeight);
      end;

      if i = FLastOpen then
      begin
        j := BarHeight - (FPanels.Count - FLastOpen - 1) * PanelCaption.Height;
      end;
    end
    else
    begin
  //for close panel
      Inc(j, PanelCaption.Height);
    end
  end;

  ArrangeOpenClose; //Set the last open panel and the first opened panels
end;

procedure TInspectorBar.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Panel: TInspectorPanel;
  a, s, fa: string;
  xs, ys: Integer;
  R, mr: TRect;
  hl, ml: Integer;
begin

  if (Button = mbRight) and not (csDesigning in ComponentState) then
  begin
    if GetPanelAtXY(X, Y, Panel) then
    begin
      if Assigned(Panel) then
        if Assigned(Panel.PopupMenu) then
          inherited PopupMenu := Panel.PopupMenu
        else
          inherited PopupMenu := FMainPopupMenu;
    end;
  end;

  inherited;

  if TabStop then
    SetFocus;

  if (GetFocus <> Handle) then
  begin
    // OnEnter event caused inplace editor or other control to have focus
    Exit;
  end;

  FMouseDown := (Button = mbLeft);

  StopEdit(FEditItem);
  StopEditingInplace;

  FMouseDown := (Button = mbLeft);

  if (Y > GetBarHeight) and HelpPanel.Visible then
  begin
    R := Rect(0, Height - HelpPanel.Height, HelpWidth, Height);
    InflateRect(R, 0, -1);

    if IsHTML(HelpPanel.Text) then
    begin
      FHotAnchor := '';
      if HTMLDrawEx(Canvas, HelpPanel.Text, R, Images, X, Y, -1, -1, 2, True, False, False, False, False, False, True,
        1.0, clBlue, clNone, clNone, clNone, a, s, fa, xs, ys, ml, hl, mr) then
        FHotAnchor := a;
      if (FHotAnchor <> '') and Assigned(FOnHelpAnchorClick) then
        FOnHelpAnchorClick(Self, FHotAnchor);
    end;
    Exit;
  end;

  if FPanels.Count = 0 then
    Exit;

  if FMode = imSinglePanelActive then
    MouseDownSinglePanel(Button, Shift, X, Y)
  else
    MouseDownMultiPanel(Button, Shift, X, Y);
end;

procedure TInspectorBar.DrawGradient(ACanvas: TCanvas; FromColor, ToColor: TColor; Steps: Integer; R: TRect; Direction: Boolean);
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

  with ACanvas do
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

procedure TInspectorBar.PaintSinglePanel;
var
  i, j, h, q, top, realtop, realheight, IndexV, realtoppanel: Integer;
  R: TRect;
  VisibleItems, FalseVisibleItems: Integer;
  xo, yo: Integer;
  Rgn, rgn2: THandle;
  TopPanel: TInspectorPanel;
  tempHeight, tempTotalHeight: Integer;
  PanelIndent: Integer;
begin
  if PanelCaption.SideDisplay then
    PanelIndent := PanelCaption.SideWidth
  else
    PanelIndent := 0;

  IndexV := 0;
  realtoppanel := GetRealTopPanel;
  if realtoppanel = -1 then
    Exit;

//panels at top of inspectorbar
  i := 1;
  while i <= FPanels.Count do
  begin
    if FPanels.Items[i - 1].Visible then
    begin
      DrawCaption(FPanels.Items[i - 1], Rect(0, indexV, Width, indexV + PanelCaption.Height));
      IndexV := IndexV + PanelCaption.Height;
    end;
    if i > realtoppanel then
      Break;
    inc(i);
  end;

  R := Rect(3, realtoppanel * PanelCaption.Height + 3, Width - 3, (realtoppanel + 1) * PanelCaption.Height - 3);

  if Focused and FShowFocus then
    DrawFocusRect(Canvas.Handle, R);

  indexv := FPanels.Count;
//panels at bottom of inspectorbar
  for i := FPanels.Count downto realtoppanel + 2 do
  begin
    if not FPanels.Items[i - 1].Visible then continue;
    j := FPanels.Count - indexv + 1;
    dec(indexv, 1);
    DrawCaption(FPanels.Items[i - 1], Rect(0, BarHeight - (j) * PanelCaption.Height, Width, BarHeight - ((j - 1) * PanelCaption.Height)));
  end;

//focused panel background
  indexv := GetNumbervisiblePanelUp(FPanels.Count) + 1;
  h := BarHeight - (indexv) * PanelCaption.Height;
  Top := (getNumbervisiblePanelUp(Realtoppanel)) * PanelCaption.Height;

  R := Rect(PanelIndent, Top + PanelCaption.Height, Width, Top + PanelCaption.Height + h);

  if PanelCaption.SideDisplay then
  begin
    Canvas.Brush.Color := PanelCaption.Color;
    Canvas.Pen.Color := PanelCaption.Color;
    Canvas.Rectangle(0, R.Top - 1, PanelIndent, R.Bottom);

    if not PanelCaption.Flat then
    begin
      Canvas.Pen.Color := clWhite;
      Canvas.MoveTo(0, R.Top - 1);
      Canvas.LineTo(0, R.Bottom);
      Canvas.Pen.Color := clGray;
      Canvas.MoveTo(PanelIndent - 1, R.Top);
      Canvas.LineTo(PanelIndent - 1, R.Bottom);
    end;
  end;

  case FPanels[realtoppanel].Background of
    pbSolid:
      begin
        Canvas.Brush.Color := FPanels[realtoppanel].Color;
        Canvas.Pen.Color := Canvas.Brush.Color;
        Canvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
      end;
    pbGradient:
      begin
        with FPanels[realtoppanel] do
          DrawGradient(Canvas, ColorToRGB(GradientStart), ColorToRGB(GradientEnd), GradientSteps, R, GradientDirection = gdHorizontal);
      end;
    pbTexture:
      begin
        Rgn := CreateRectRgn(R.Left, R.Top, R.Right, R.Bottom);
        SelectClipRgn(Canvas.Handle, Rgn);

        if not FPanels[realtoppanel].FTexture.Empty then
        begin
          yo := R.Top;
          while (yo < R.Bottom) do
          begin
            xo := R.Left;
            while (xo < R.Right) do
            begin
              Canvas.Draw(xo, yo, FPanels[realtoppanel].FTexture);
              xo := xo + FPanels[realtoppanel].FTexture.Width;
            end;
            yo := yo + FPanels[realtoppanel].FTexture.Height;
          end;
        end;
        SelectClipRgn(Canvas.Handle, 0);
        DeleteObject(Rgn);
      end;
  end;

{
if not FFlat then
begin
Canvas.Pen.Color := clGray;
Canvas.MoveTo(R.Right-1,R.Top);
Canvas.LineTo(R.Right-1,R.Bottom);
Canvas.MoveTo(R.Left,R.Top);
Canvas.LineTo(R.Left,R.Bottom);
end;
}

//draw the items
  TopPanel := FPanels.Items[realtoppanel];
  Canvas.Font.Assign(TopPanel.Font);

  RealTop := Top + PanelCaption.Height;
  RealHeight := BarHeight - RealTop - (getNumbervisiblePanelDown(realtoppanel) * PanelCaption.Height);

  VisibleItems := 0;
  FalseVisibleItems := 0;
  tempTotalHeight := 0;
  for i := TopPanel.FTopItem + 1 to TopPanel.Items.Count do
    if TopPanel.Items.Items[i - 1].FVisible then
    begin
      if TopPanel.VariableItemHeight then
        tempTotalHeight := tempTotalHeight + TopPanel.Items.Items[i - 1].Height
      else
        tempTotalHeight := tempTotalHeight + TopPanel.ItemHeight;

      if RealHeight > tempTotalHeight then
        Inc(VisibleItems);
    end
    else
      inc(FalseVisibleItems);

  if VisibleItems > TopPanel.Items.Count then
    VisibleItems := TopPanel.Items.Count;

  q := 0;

  tempTotalHeight := RealTop;
  Rgn2 := CreateRectRgn(R.Left, R.Top, R.Right, R.Bottom);
  SelectClipRgn(Canvas.Handle, Rgn2);

  for i := TopPanel.FTopItem + 1 to TopPanel.Items.Count do
  begin
    if TopPanel.VariableItemHeight then
      tempHeight := TopPanel.Items[i - 1].Height
    else
      tempHeight := TopPanel.ItemHeight;

    if (TopPanel.Items[i - 1].FVisible) and (q <= VisibleItems) then
    begin
      Inc(q);
      R := Rect(PanelIndent, tempTotalHeight, Width, tempTotalHeight + tempHeight);
      DrawItem(TopPanel, TopPanel.Items[i - 1], R);
      Inc(tempTotalHeight, tempHeight);
    end;
  end;
  SelectClipRgn(Canvas.Handle, 0);
  DeleteObject(Rgn2);

  Canvas.Pen.Color := clBlack;

//draw Up and Down Buttons
  if FPanels.Items[realtoppanel].TopItem <> 0 then
  begin
    if not FShowUP then
    begin
      FShowUP := True;
      PaintSinglePanel;
    end
    else
      FShowUP := True;
    DrawButtonUp; // draw Up button
  end
  else
  begin
    if FShowUP then
    begin
      FShowUP := False;
      PaintSinglePanel;
    end
    else
      FShowUP := False;
  end;

  if FPanels.Items[realtoppanel].Items.Count - VisibleItems - FalseVisibleItems > FPanels.Items[realtoppanel].TopItem then
  begin
    if not FShowDown then
    begin
      FShowDown := True;
      PaintSinglePanel;
    end
    else
      FShowDown := True;
    DrawButtonDown; // draw Down button
  end
  else
  begin
    if FShowDown then
    begin
      FShowDown := False;
      PaintSinglePanel;
    end
    else
      FShowDown := False;
  end;
end;

procedure TInspectorBar.PaintMultiPanel;
var
  i, j, h, Top, RealTop, RealWidth, q: Integer;
  R: TRect;
  xo, yo: Integer;
  Rgn: THandle;
  TopPanel: TInspectorPanel;
  tempHeight: Integer;
  PanelIndent: Integer;

begin
// panels at top of inspectorbar
// RealTop := 0;
// j holds the current drawing position



  if PanelCaption.SideDisplay then
    PanelIndent := PanelCaption.SideWidth
  else
    PanelIndent := 0;

  if FScrollBar.Visible then
  begin
    RealWidth := Width - FScrollBar.Width;
    j := -FScrollBar.Position;
  end
  else
  begin
    RealWidth := Width;
    j := 0;
  end;

  for i := 1 to FPanels.Count do
  begin
    if not FPanels.Items[i - 1].Visible then
      Continue;

    if not FPanels.Items[i - 1].FIsOpen then
    begin
  //draw the panel caption for closed panels
      DrawCaption(FPanels.Items[i - 1],
        Rect(0, j, RealWidth, j + PanelCaption.Height));
      Inc(j, PanelCaption.Height);
    end
    else
    begin
  //draw the panel caption for opened panels
      DrawCaption(FPanels.Items[i - 1],
        Rect(0, j, RealWidth, j + PanelCaption.Height));

      Inc(j, PanelCaption.Height);
      h := 0;
      for q := 0 to FPanels.Items[i - 1].Items.Count - 1 do
        if FPanels.Items[i - 1].Items[q].Visible then
          if FPanels.Items[i - 1].VariableItemHeight then
            Inc(h, FPanels.Items[i - 1].Items[q].Height)
          else
            Inc(h, FPanels.Items[i - 1].ItemHeight);

      Top := j;

      if (i = FLastOpen + 1) then //LastOpen + 1 because i is 1 based
      begin
    // if it is the last panel and it is shorter than TInspectorbar
        RealTop := BarHeight - (FPanels.Count - FLastOpen - 1) * PanelCaption.Height - h - Top;
        if RealTop < 0 then
          RealTop := 0;
      end
      else
        Realtop := 0;

      R := Rect(PanelIndent, Top, RealWidth, Top + h + RealTop);

      if PanelCaption.SideDisplay then
      begin
        Canvas.Brush.Color := PanelCaption.Color;
        Canvas.Pen.Color := PanelCaption.Color;
        Canvas.Rectangle(0, R.Top - 1, PanelIndent, R.Bottom);

        if not PanelCaption.Flat then
        begin
          Canvas.Pen.Color := clWhite;
          Canvas.MoveTo(0, R.Top - 1);
          Canvas.LineTo(0, R.Bottom);
          Canvas.Pen.Color := clGray;
          Canvas.MoveTo(PanelIndent - 1, R.Top);
          Canvas.LineTo(PanelIndent - 1, R.Bottom);
        end;
      end;


      case FPanels.Items[i - 1].Background of
        pbSolid:
          begin
            Canvas.Brush.Color := FPanels[i - 1].Color;
            Canvas.Pen.Color := Canvas.Brush.Color;
            Canvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
          end;
        pbGradient:
          begin
            with FPanels.Items[i - 1] do
              DrawGradient(Canvas, ColorToRGB(GradientStart), ColorToRGB(GradientEnd), GradientSteps, R, GradientDirection = gdHorizontal);
          end;
        pbTexture:
          begin
            Rgn := CreateRectRgn(R.Left, R.Top, R.Right, R.Bottom);
            SelectClipRgn(Canvas.Handle, Rgn);
            if not FPanels[i - 1].FTexture.Empty then
            begin
              yo := R.Top;
              while (yo < R.Bottom) do
              begin
                xo := R.Left;
                while (xo < R.Right) do
                begin
                  Canvas.Draw(xo, yo, FPanels[i - 1].FTexture);
                  xo := xo + FPanels[i - 1].FTexture.Width;
                end;
                yo := yo + FPanels[i - 1].FTexture.Height;
              end;
            end;
            SelectClipRgn(Canvas.Handle, 0);
            DeleteObject(Rgn);
          end;
      end;

  //draw the items
      TopPanel := FPanels.Items[i - 1];
      Canvas.Font.Assign(TopPanel.Font);

      for q := 1 to TopPanel.Items.Count do
      begin
        if TopPanel.VariableItemHeight then
          tempHeight := TopPanel.Items.Items[q - 1].Height
        else
          tempHeight := TopPanel.ItemHeight;

        R := Rect(PanelIndent, j, RealWidth, j + tempHeight);
        if TopPanel.Items[q - 1].FVisible then
        begin
          if (r.Top < Height) and (r.Top >= 0) then
            DrawItem(TopPanel, TopPanel.Items.Items[q - 1], R);
          inc(j, tempHeight);
        end;
      end;

      inc(j, RealTop);
    end;
  end;

//---------  End DRAW -------------
  if j < BarHeight then
  begin
    Canvas.Brush.Color := Color;
    Canvas.Pen.Color := Color;
    Canvas.Rectangle(0, j, Width, BarHeight);
  end
  else
  begin
    if (not FScrollBar.Visible) and (j > BarHeight) then
    begin
      ArrangeOpenClose;
      PaintMultiPanel;
    end;
  end;
end;

procedure TInspectorBar.Paint;
var
  TopColor, BottomColor: TColor;
  R: TRect;
  xs, ys, ml, hl: Integer;
  mr: TRect;
  a, s, fa: string;

  procedure AdjustColors(Bevel: TPanelBevel);
  begin
    TopColor := clBtnHighlight;
    if Bevel = bvLowered then TopColor := clBtnShadow;
    BottomColor := clBtnShadow;
    if Bevel = bvLowered then BottomColor := clBtnHighlight;
  end;

begin
  if (FPanels.Count = 0) or (csDestroying in ComponentState) or
    (GetNumberVisiblePanelUp(FPanels.Count) = -1) then
  begin
    Canvas.Brush.Color := clBtnFace;
    Canvas.Rectangle(ClientRect);
    Exit;
  end;

  if FUpdateCount > 0 then
    Exit;

  if FShadedHeader.Width <> Width then
    ShadeHeader;

  if FMode = imSinglePanelActive then
    PaintSinglePanel
  else
    PaintMultiPanel;

  if HelpPanel.Visible then
  begin
    R := Rect(0, Height - HelpPanel.Height, HelpWidth, Height);
    if HelpPanel.ColorTo <> clNone then
      DrawGradient(Canvas, HelpPanel.Color, HelpPanel.ColorTo, 128, R, True)
    else
    begin
      Canvas.Brush.Color := HelpPanel.Color;
      Canvas.Pen.Color := HelpPanel.Color;
      Canvas.Rectangle(0, Height - HelpPanel.Height, HelpWidth, Height);
    end;

    Canvas.Font.Assign(HelpPanel.Font);

    R := Rect(0, Height - HelpPanel.Height, HelpWidth, Height);

    InflateRect(R, 0, -1);

    if HelpPanel.BevelOuter <> bvNone then
    begin
      AdjustColors(HelpPanel.BevelOuter);
      Frame3D(Canvas, R, TopColor, BottomColor, HelpPanel.BevelWidth);
    end;

    Frame3D(Canvas, R, Color, Color, BorderWidth);

    if HelpPanel.BevelInner <> bvNone then
    begin
      AdjustColors(HelpPanel.BevelInner);
      Frame3D(Canvas, R, TopColor, BottomColor, HelpPanel.BevelWidth);
    end;

    if IsHTML(HelpPanel.Text) then
      HTMLDrawEx(Canvas, HelpPanel.Text, R, Images, R.Left, R.Top, -1, -1, 2, False, False, False, False, False, False, True,
        1.0, clBlue, clNone, clNone, clNone, a, s, fa, xs, ys, ml, hl, mr)
    else
      DrawText(Canvas.Handle, PChar(HelpPanel.Text), Length(HelpPanel.Text), R, DT_WORDBREAK or DT_NOPREFIX);
  end;
end;

procedure TInspectorBar.SetInspectorPanels(const Value: TInspectorPanels);
begin
  FPanels.Assign(Value);
end;

procedure TInspectorBar.SetTimerEnabled(const Value: boolean);
begin
  FRepeatTimer.Enabled := value;
end;

procedure TInspectorBar.SetTopPanel(const Value: Integer);
var
  i: Integer;
begin
  if (Value < FPanels.Count) and (Mode = imSinglePanelActive) then
  begin
    FTopPanel := Value;
    for i := 1 to FPanels.Count do
      FPanels.Items[i - 1].FIsOpen := False;
    FPanels.Items[FTopPanel].FIsOpen := True;
    ArrangeOpenClose;
    Invalidate;
  end;
end;

procedure TInspectorBar.ShadeHeader;
var
  a, x, y, xs, i, j, h, k, s, sizeX, sizeY: Integer;
  d: TColor;
  R: Trect;
  Light: Byte;
  rr, br, gr: Integer;

  function Dist(x1, y1, x2, y2: Integer): Integer;
  begin
    Result := Round(sqrt(sqr(x1 - x2) + sqr(y1 - y2)));
  end;

begin
  rr := GetRValue(ColorToRGB(FInspectorCaption.Color));
  gr := GetGValue(ColorToRGB(FInspectorCaption.Color));
  br := GetBValue(ColorToRGB(FInspectorCaption.Color));

  Light := FInspectorCaption.ShadeLight;

  FShadedHeader.Width := Width;
  FShadedHeader.Height := FInspectorCaption.FHeight;

  Randomize;
  SizeX := FShadedHeader.Width;
  SizeY := FShadedHeader.Height;
  FShadedHeader.Canvas.Brush.Color := clWhite;
  r := Rect(0, 0, SizeX, SizeY);
  FShadedHeader.Canvas.FillRect(r); //clear the bitmap

  if (SizeX = 0) or (SizeY = 0) then
    Exit;

  case FInspectorCaption.ShadeType of
    stIRADIAL, stORADIAL:
      begin
        h := Dist(0, SizeX, 0, SizeY);
        x := sizeX div 2;
        y := sizeY div 2;

        for i := 0 to x do
          for j := 0 to y do
          begin
            k := Dist(i, j, x, y);

            if FInspectorCaption.ShadeType = stIRADIAL then
              k := Round((h - k) / h * Light)
            else
              k := Round(k / h * Light);

            d := RGB((rr * k) div 255, (gr * k) div 255, (br * k) div 255);

            FShadedHeader.Canvas.Pixels[i, j] := d;
            FShadedHeader.Canvas.Pixels[sizex - i, sizey - j] := d;
            FShadedHeader.Canvas.Pixels[sizex - i, j] := d;
            FShadedHeader.Canvas.Pixels[i, sizey - j] := d;
          end;
      end;
    stLMETAL, stRMETAL:
      begin
        for a := 0 to 250 do
        begin
          x := Random(sizeX);
          y := Random(sizeY);
          xs := Random(Min(sizeX, sizeY) div 2);
          i := Light - Random(40);
          d := RGB((rr * i) div 255, (gr * i) div 255, (br * i) div 255);
          for i := 0 to xs - 1 do
          begin
            if FInspectorCaption.ShadeType = stLMetal then
            begin
              if (((x - i) > 0) and ((y + i) < sizeY)) then
                FShadedHeader.Canvas.Pixels[x + i, y + i] := d;
              if (((x + i) < sizeX) and ((y - i) > 0)) then
                FShadedHeader.Canvas.Pixels[x - i, y - i] := d;
            end
            else
            begin
              if (((x - i) > 0) and ((y + i) < sizeY)) then
                FShadedHeader.Canvas.Pixels[x - i, y + i] := d;
              if (((x + i) < sizeX) and ((y - i) > 0)) then
                FShadedHeader.Canvas.Pixels[x + i, y - i] := d;
            end;
          end;
        end;
        a := 120;
        for i := 0 to sizeX do
          for j := 0 to sizeY do
          begin
            d := FShadedHeader.Canvas.Pixels[i, j];
            x := GetBValue(d);
            x := Light - x;
            x := x + ((a * i) div sizeX) + ((a * j) div sizeY);
            x := Light - x div 2;
            d := RGB((rr * x) div 255, (gr * x) div 255, (br * x) div 255);
            FShadedHeader.Canvas.Pixels[i, j] := d;
          end;
      end;
    stHARDBUMP:
      begin
        for i := 0 to sizeY do
        begin
          x := (255 * i div sizeY) - 127;
          x := (x * (x * x) div 128) div 128;
          x := ((x * 112) div 128) + 128;
          for j := 0 to sizeX do
          begin
            y := Light - x div 2; //offset
            d := RGB((rr * y) div 255, (gr * y) div 255, (br * y) div 255);
            FShadedHeader.Canvas.Pixels[j, i] := d;
          end;
        end;
        k := min(16, sizeX div 6);
        a := (sizeY * sizeY) div 4;
        for i := 0 to sizeY do
        begin
          y := i - sizeY div 2;
          for j := 0 to sizeX do
          begin
            x := j - sizeX div 2;
            xs := sizeX div 2 - k + (y * y * k) div a;
            if (x > xs) then
            begin
              s := 8 + (((sizeX - j) * 128) div k);
              s := Light - s div 2; //offset
              d := RGB((rr * s) div 255, (gr * s) div 255, (br * s) div 255);
              FShadedHeader.Canvas.Pixels[j, i] := d;
            end;
            if (x + xs) < 0 then
            begin
              s := 247 - ((j * 128) div k);
              s := Light - s div 2; //offset
              d := RGB((rr * s) div 255, (gr * s) div 255, (br * s) div 255);
              FShadedHeader.Canvas.Pixels[j, i] := d;
            end;
          end;
        end;
      end;
    stSOFTBUMP:
      begin
        for i := 0 to sizeY do
        begin
          h := ((255 * i) div sizeY) - 127;
          for j := 0 to sizeX do
          begin
            k := 255 * (sizeX - j) div sizeX - 127;
            k := ((h * (h * h)) div 128) div 128 + (k * (k * k) div 128) div 128;
            k := k * (128 - 8) div 128 + 128;
            if (k < 8) then k := 8;
            if (k > 247) then k := 247;
            s := Light - k div 2; //offset
            d := RGB((rr * s) div 255, (gr * s) div 255, (br * s) div 255);
            FShadedHeader.Canvas.Pixels[j, i] := d;
          end;
        end;
      end;
    stHBUMP:
      begin
        for j := 0 to sizeX do
        begin
          k := (255 * (sizeX - j) div sizeX) - 127;
          k := (k * (k * k) div 128) div 128;
          k := (k * (128 - 8)) div 128 + 128;
          for i := 0 to sizeY do
          begin
            s := Light - k div 2; //offset
            d := RGB((rr * s) div 255, (gr * s) div 255, (br * s) div 255);
            FShadedHeader.Canvas.Pixels[j, i] := d;
          end;
        end;
      end;
    stVBUMP:
      begin
        for i := 0 to sizeY do
        begin
          k := (255 * i div sizeY) - 127;
          k := (k * (k * k) div 128) div 128;
          k := (k * (128 - 8)) div 128 + 128;
          for j := 0 to sizeX do
          begin
            s := Light - k div 2; //offset
            d := RGB((rr * s) div 255, (gr * s) div 255, (br * s) div 255);
            FShadedHeader.Canvas.Pixels[j, i] := d;
          end;
        end;
      end;
    stDIAGSHADE:
      begin
        a := 129;
        for i := 0 to sizeX do
          for j := 0 to sizeY do
          begin
            d := FShadedHeader.Canvas.Pixels[i, j];
            x := GetBValue(d);
            x := Light - x;
            x := x + ((a * i) div sizeX) + ((a * j) div sizeY);
            x := Light - x div 2; //offset
            d := RGB((rr * x) div 255, (gr * x) div 255, (br * x) div 255);
            FShadedHeader.Canvas.Pixels[i, j] := d;
          end;
      end;
    stVSHADE, stVSHADEInv:
      begin
        a := 239;
        for i := 0 to sizeY do
        begin
          k := a * i div sizeY + 8;
          k := Light - k div 4; //offset
          d := RGB((rr * k) div 255, (gr * k) div 255, (br * k) div 255);
          for j := 0 to sizeX do
            if FInspectorCaption.ShadeType = stVSHADEInv then
              FShadedHeader.Canvas.Pixels[j, sizey - i] := d
            else
              FShadedHeader.Canvas.Pixels[sizeX - j, i] := d
        end;
      end;
    stHSHADE, stHShadeInv:
      begin
        a := 239;
        for j := 0 to sizeX do
        begin
          k := a * (sizeX - j) div sizeX + 8;
          k := Light - k div 2; //offset
          d := RGB((rr * k) div 255, (gr * k) div 255, (br * k) div 255);
          for i := 0 to sizeY do
            if FInspectorCaption.ShadeType = stHSHADE then
              FShadedHeader.Canvas.Pixels[j, i] := d
            else
              FShadedHeader.Canvas.Pixels[sizeX - j, i] := d
        end;
      end;
    stNOISE:
      begin
        for i := 0 to sizeX do
          for j := 0 to sizeY do
          begin
            k := 128 + random(FInspectorCaption.ShadeGrain);
            k := Light - k div 2; //offset
            d := RGB((rr * k) div 255, (gr * k) div 255, (br * k) div 255);
            FShadedHeader.Canvas.Pixels[i, j] := d;
          end;
      end;

    stNORMAL, stXPButton, stBitmap, stBitmapLStretch, stBitmapRStretch:
      begin //for normal we use the panel caption color
        if PanelCaption.ColorTo <> clNone then
        begin
          Drawgradient(FShadedHeader.Canvas, PanelCaption.Color, PanelCaption.ColorTo, 128, r, DefaultGradientDirection = gdHorizontal);
        end
        else
        begin
          FShadedHeader.Canvas.Brush.Color := PanelCaption.Color;
          FShadedHeader.Canvas.FillRect(r);
        end;
      end;
  end;
end;

procedure TInspectorBar.GetLabelRect(Panel: TInspectorPanel;
  Item: TInspectorItem; var R: TRect);
var
  text: string;
  TempIcon: TIcon;
  h, w, hh, i: integer;

begin
  if (item.FCaption = '') or (not ((panel.Style = psSmallIcon) or
    (panel.Style = psLargeIcon))) then
  begin
    r := Rect(0, 0, 0, 0);
    Exit;
  end;

  r := item.FLocalRect;
  TempIcon := Item.Icon;
  Canvas.Font.Assign(panel.Font);

  Text := Item.FCaption;

  w := Canvas.TextWidth(text) + 2;
  hh := Canvas.TextHeight(text);

  if panel.WordWrap then
  begin
    if w > Width then
    begin
      w := width - 50;
      FhideMemo.Font.Assign(Panel.Font);
      FhideMemo.Text := Text;
      FhideMemo.Width := w;
      hh := 0;
      for i := 0 to FhideMemo.Lines.Count - 1 do
      begin
        hh := hh + Canvas.TextHeight(FhideMemo.lines[i]);
      end;
    end;
    hh := r.top + hh;
    r.Bottom := min(r.Bottom, hh);
    r.Right := r.left + w;
    Exit;
  end
  else
  begin
    w := min(r.Right - r.Left, w);
  end;

  if Panel.VariableItemHeight then
    h := Item.Height
  else
    h := panel.ItemHeight;

  case panel.Alignment of
    taLeftJustify:
      begin
        r.Right := r.left + w;
      end;
    taCenter:
      begin
        r.Left := r.Left + (((r.Right - r.Left) div 2) - w div 2);
        r.Right := r.Left + w;
      end;
    taRightJustify:
      begin
        r.Left := r.Right - w;
      end;
  end;

  if panel.Style = psLargeicon then
  begin
    if TempIcon <> nil then
    begin
      if TempIcon.Empty then
      begin
        h := (h div 2) - (hh div 2);
        R.Top := R.Top + h;
        r.Bottom := R.Top + hh;
      end;
    end
    else
    begin
      h := (h div 2) - (hh div 2);
      R.Top := R.Top + h;
      r.Bottom := R.Top + hh;
    end;
  end
  else
  begin
    h := (h div 2) - (hh div 2);
    R.Top := R.Top + h - 2;
    r.Bottom := R.Top + hh;
  end;
end;

procedure TInspectorBar.DrawItem(Panel: TInspectorPanel;
  Item: TInspectorItem; R: TRect);
var
  TempIcon: TIcon;
  DrwRect, CR, AR, IR: TRect;
  IcoRect, MouseRect: TRect;
  pt: TPoint;
  DTSTYLE: DWord;
  HTheme: THandle;
  Hot, Down: Boolean;
  H, Ind, VInd: Integer;
  DefaultDraw: Boolean;
  HIcon: THandle;
  DefDraw: Boolean;
  HasIcon: Boolean;
  IEButton: TInspectorEditButton;
  a, s, fa: string;
  xs, ys, ml, hl: Integer;
  mr: TRect;
  Offset: Integer;
  HTMLText: Boolean;
  FocusStyle: DWord;
  SizeIcoX, SizeIcoY: Integer;
  lr: Trect;
  Text, ValueText: string;
  rw: integer;
  pti: TPoint;
  Rgn: HRGN;
  clr, clrto: TColor;
  uFormat: Cardinal;
  FontValue: String;
  bmp: TBitMap;
  custform: TCustomForm;
  custformactive: boolean;

  procedure DrawTheItem(WithGradient: boolean; clr, clrto: tcolor);
  begin
    if WithGradient then
      DrawGradient(Canvas, clr, clrTo, 128, R, false)
    else
    begin
      Canvas.Brush.Color := clr;
      Canvas.Rectangle(R.Left, r.Top, r.Right, r.Bottom);

      if FMouseDown or Item.Down then
        Canvas.Pen.Color := clGray
      else
        Canvas.Pen.Color := clWhite;

      Canvas.MoveTo(R.Left, R.Bottom - 1);
      Canvas.LineTo(R.Left, R.Top);
      Canvas.LineTo(R.Right - 1, R.Top);

      if FMouseDown or Item.Down then
        Canvas.Pen.Color := clWhite
      else
        Canvas.Pen.Color := clGray;

      Canvas.LineTo(R.Right - 1, R.Bottom - 1);
      Canvas.LineTo(R.Left, R.Bottom - 1);
    end;
  end;

begin
  DefaultDraw := True;

  if mode = imSinglePanelActive then
  begin
    if FShowUP or FShowDown then
      r.Right := r.Right - 19; //18 width of scroll button
  end;

  item.Flocalrect := Rect(0, 0, 0, 0);

  Text := Item.FCaption;

  IR := GetItemRect(Panel, Item);

  SetBkMode(Canvas.Handle, TRANSPARENT);

  TempIcon := Item.FIcon;

// you can remove DT_END_ELLIPSIS in order to remove ... display
  if not Panel.WordWrap then
    DTSTYLE := DT_END_ELLIPSIS or DT_VCENTER or DT_SINGLELINE
  else
    DTSTYLE := DT_WORDBREAK;

  custform := GetParentForm(self);

  custformactive := false;

  if Assigned(custform) then
  begin
    if (custform is TForm) then
    begin
      custformactive := custform.Active or ((custform as TForm).FormStyle = fsMDIForm);
    end
    else
      custformactive := custform.Active
  end;

  if custformactive then
  begin
    GetCursorPos(pt);
    pt := ScreenToClient(pt);
  end
  else
  begin
    pt := Point(0,0); // to be safe no invalid hover-markings, etc. are placed!
  end; //if


  AR := R;

  HasIcon := False;

  if (TempIcon <> nil) and not Item.AutoIcon then
    HasIcon := not TempIcon.Empty;

  if Item.AutoIcon then
    HasIcon := Item.URL <> '';


  Canvas.Font.Assign(Panel.Font);

  if Assigned(FOnItemDraw) then
    FOnItemDraw(Self, Item, Canvas, R, DefaultDraw);

  if not DefaultDraw then
    Exit;


  if not Item.Background.Empty then
  begin
    case Item.BackgroundPosition of
      bpTopLeft: Canvas.Draw(R.Left, R.Top, Item.Background);
      bpBottomLeft: Canvas.Draw(R.Left, R.Bottom - Item.Background.Height, Item.Background);
      bpTopRight: Canvas.Draw(R.Right - Item.Background.Width, R.Top, Item.Background);
      bpBottomRight: Canvas.Draw(R.Right - Item.Background.Width, R.Bottom - Item.Background.Height, Item.Background);
    end;
  end;


  if IsItemIndex(Panel, Item) or Item.Selected then
  begin
    Canvas.Pen.Color := Panel.FSelectBorderColor;
    Canvas.Rectangle(r.Left, r.Top, r.Right, r.Bottom + 1);

    Canvas.Font.Color := Panel.FSelectFontColor;
//    InflateRect(r, -1, -1);
    if Panel.SelectColorTo <> clNone then
      DrawGradient(Canvas, Panel.SelectColorTo, Panel.SelectColor, 128, r, false)
    else
    begin
      Canvas.Brush.Color := Panel.SelectColor;
      Canvas.Rectangle(r.Left, r.Top, r.Right, r.Bottom + 1);
    end;

  end;

  lr := Rect(0, 0, 0, 0);

  if Panel.WordWrap then
    DrawText(canvas.Handle, PChar(Text), length(Text), lr, DT_CALCRECT or DT_NOPREFIX)
  else
    DrawText(canvas.Handle, PChar(Text), length(Text), lr, DTSTYLE or DT_CALCRECT or DT_NOPREFIX);

  SizeIcoX := 0;
  SizeIcoY := 0;

  if HasIcon then
  begin
    pti := WidthSizeIcon(TempIcon);
    SizeIcoX := pti.X;
    SizeIcoY := pti.Y;

    if (Panel.IconLargeSize > 0) and (Panel.Style = psLargeIcon) then
    begin
      SizeIcoX := Panel.IconLargeSize;
      SizeIcoY := Panel.IconLargeSize;
    end;
  end
  else
  begin
    if not Item.Bitmap.Empty then
    begin
      SizeIcoX := Item.Bitmap.Width;
      SizeIcoY := Item.Bitmap.Height;
    end;
  end;

  if not HasIcon and Assigned(Images) and (Item.ImageIndex >= 0) then
  begin
    SizeIcoX := Images.Width;
    SizeIcoY := Images.Height;
  end;

{
if Panel.VariableItemHeight then
SizeIcoY := Item.Height
else
SizeIcoY := panel.ItemHeight;

SizeIcoY := SizeIcoY - lr.Bottom - lr.Top - 9;  //9 because add all inflate rect
}

  rw := Canvas.TextWidth(Text) + 2;

  if panel.WordWrap then
    if rw > Width then
      rw := Width - 50;

  // I have left a maximum of 50 for the eventuality that an icon exists
  // so the WordWrap text will be no wider than width-50

  case Panel.Style of
    psLargeIcon:
      begin
        if Panel.HoverFullWidth then
        begin
          IcoRect := IR;
          if PanelCaption.SideDisplay then
            IcoRect.Left := IcoRect.Left + PanelCaption.SideWidth;
        end
        else
        begin
          IcoRect.Top := R.Top + 2;
          IcoRect.Bottom := IcoRect.Top + SizeIcoY;

          case panel.Alignment of
            taCenter:
              begin
                IcoRect.Left := panel.Indent + R.Left + (R.Right - R.Left - SizeIcoX) shr 1;
              end;
            taLeftJustify:
              begin
                IcoRect.Left := 2 + panel.Indent + R.Left;
              end;
            taRightJustify:
              begin
                IcoRect.Left := R.Right + panel.Indent - SizeIcoX - 2;
              end;
          end;

          IcoRect.Right := IcoRect.Left + SizeIcoX;
          InflateRect(IcoRect, 2, 2);
        end;

        MouseRect := IcoRect;

        InflateRect(MouseRect, -1, -1);

        Offset := 0;

        if not InDesign then
        begin
          if ((Panel.HoverIconColor <> clNone) and (Panel.HoverIconDownColor <> clNone)
            and PtInRect(MouseRect, pt) and (HasIcon or Panel.HoverFullWidth or not Item.Bitmap.Empty or (Item.ImageIndex >= 0))) or Item.Down then
          begin
            if FMouseDown and PtInRect(MouseRect, pt) then
              Offset := 2;

            if FMouseDown or Item.Down then
            begin
              clr := Panel.SelectColor;
              clrTo := Panel.SelectColorTo;
            end
            else
            begin
              clr := Panel.HoverIconColor;
              clrTo := Panel.HoverIconColorTo;
            end;

            if Panel.HoverIconBorderColor <> clNone then
              Canvas.Pen.Color := Panel.HoverIconBorderColor
            else
              Canvas.Pen.Color := clBlack;

            Canvas.Pen.Width := 1;

            {  if FMouseDown or Item.Down then
                Canvas.Brush.Color := Panel.HoverIconDownColor
              else
                Canvas.Brush.Color := Panel.HoverIconColor; }

            Canvas.Rectangle(IcoRect.Left, IcoRect.Top, IcoRect.Right, IcoRect.Bottom + 1);

            //InflateRect(IcoRect, -1, -1);

            if IsItemIndex(Panel, Item) or Item.Selected then
            begin
              if (Panel.SelectColorTo <> clNone) then
                DrawGradient(Canvas, panel.SelectDownColor, panel.SelectDownColorTo, 128, IcoRect, false)
              else
              begin
                if FMouseDown or Item.Down then
                  Canvas.Brush.Color := Panel.SelectDownColor
                else
                  Canvas.Brush.Color := Panel.SelectColor;
                Canvas.Rectangle(IcoRect.Left, IcoRect.Top, IcoRect.Right, IcoRect.Bottom + 1);
              end;
            end
            else
            begin
              if (Panel.HoverIconColorTo <> clNone) then
              begin
                DrawGradient(Canvas, clr, clrTo, 128, IcoRect, false)
              end
              else
              begin
                if FMouseDown or Item.Down then
                  Canvas.Brush.Color := Panel.HoverIconDownColor
                else
                  Canvas.Brush.Color := Panel.HoverIconColor;

                Canvas.Rectangle(IcoRect.Left, IcoRect.Top, IcoRect.Right, IcoRect.Bottom + 1);
              end;
            end;

            FMouseItem := item;
          end;
        end;

        if not HasIcon and Assigned(Images) and (Item.ImageIndex >= 0) then
        begin
          SizeIcoX := 0;
          SizeIcoY := 0;
        end;


        if Assigned(FImages) and (Item.ImageIndex > -1) and not Item.AutoIcon then
        begin
          IcoRect.Top := R.Top + 2;
          IcoRect.Bottom := IcoRect.Top + FImages.Height;
          case panel.Alignment of
            taCenter:
              begin
                IcoRect.Left := panel.Indent + R.Left + (R.Right - R.Left - FImages.Width) shr 1;
              end;
            taLeftJustify:
              begin
                IcoRect.Left := 2 + panel.Indent + R.Left;
              end;
            taRightJustify:
              begin
                IcoRect.Left := R.Right + panel.Indent - FImages.Width - 2;
              end;
          end;

          IcoRect.Right := IcoRect.Left + FImages.Width;
          if Panel.HoverFullWidth then
          begin
            MouseRect := R;
            if PanelCaption.SideDisplay then
              MouseRect.Left := R.Left + PanelCaption.SideWidth;
          end
          else
            MouseRect := IcoRect;

          InflateRect(MouseRect, -1, -1);

          FImages.Draw(Canvas, IcoRect.Left + Offset, IcoRect.Top + Offset, Item.ImageIndex);
          R.Top := R.Top + FImages.Height + 4;
        end;


        if Item.AutoIcon and (Item.URL <> '') then
        begin
          IcoRect.Top := R.Top + 2;
          IcoRect.Bottom := IcoRect.Top + 32;
          case panel.Alignment of
            taCenter:
              begin
                IcoRect.Left := panel.Indent + R.Left + (R.Right - R.Left - 32) shr 1;
              end;
            taLeftJustify:
              begin
                IcoRect.Left := 2 + panel.Indent + R.Left;
              end;
            taRightJustify:
              begin
                IcoRect.Left := R.Right + panel.Indent - 32 - 2;
              end;
          end;

          IcoRect.Right := IcoRect.Left + 32;
          if Panel.HoverFullWidth then
          begin
            MouseRect := R;
            if PanelCaption.SideDisplay then
              MouseRect.Left := R.Left + PanelCaption.SideWidth;
          end
          else
            MouseRect := IcoRect;

          InflateRect(MouseRect, -1, -1);

          if (Pos('.EXE', UpperCase(Item.URL)) > 0) or
            (Pos('.ICO', UpperCase(Item.URL)) > 0) or
            (Pos('.ICL', UpperCase(Item.URL)) > 0) or
            (Pos('.DLL', UpperCase(Item.URL)) > 0) then
          begin
            HIcon := ExtractIcon(HInstance, PChar(Item.URL), Item.AutoIconIndex);
            DrawIconEx(Canvas.Handle, IcoRect.left + Offset, IcoRect.Top + 2 + Offset, HIcon, 32, 32, 0, 0, DI_Normal);
            DestroyIcon(HIcon);
          end
          else
          begin
            if Pos('://', Item.URL) > 0 then
              SysImageEx(Canvas, IcoRect.Left + Offset, IcoRect.Top + Offset, 'a.html', True, True)
            else
              SysImageEx(Canvas, IcoRect.Left + Offset, IcoRect.Top + Offset, Item.URL, True, True)
          end;

          R.Top := R.Top + 32 + 4;
        end;

        if ((TempIcon <> nil) and not Item.AutoIcon)
          or not Item.Bitmap.Empty then
        begin
          IcoRect.Top := R.Top + 2;
          IcoRect.Bottom := IcoRect.Top + SizeIcoY;
          case panel.Alignment of
            taCenter:
              begin
                IcoRect.Left := panel.Indent + R.Left + (R.Right - R.Left - SizeIcoX) shr 1;
              end;
            taLeftJustify:
              begin
                IcoRect.Left := 2 + panel.Indent + R.Left;
              end;
            taRightJustify:
              begin
                IcoRect.Left := R.Right + panel.Indent - SizeIcoX - 2;
              end;
          end;

          IcoRect.Right := IcoRect.Left + SizeIcoX;

          if HasIcon then
          begin
            Canvas.Draw(IcoRect.Left + Offset, IcoRect.Top + Offset, TempIcon);
      {
      bmp := TBitmap.Create;
      bmp.Width := min(Offset+SizeIcoX,TempIcon.Width);
      bmp.Height := min(Offset+SizeIcoY,TempIcon.Height);
      bmp.Canvas.Draw(0,0,TempIcon);
      bmp.Transparent := True;
      Canvas.Draw(IcoRect.Left + Offset,IcoRect.Top + Offset,bmp);
      bmp.free;
      }
          end
          else
          begin
            Item.Bitmap.Transparent := True;
            Canvas.Draw(IcoRect.Left + Offset, IcoRect.Top + Offset, Item.Bitmap);
          end;

          InflateRect(IcoRect, 2, 2);

          R.Top := R.Top + SizeIcoY + 3; // We don't use >3 because of failed underline
        end;

        HTMLText := IsHTML(text);

        if PtInRect(MouseRect, pt) and (HasIcon or not Item.Bitmap.Empty) and
          ((Panel.HoverIconColor = clNone) or (Panel.HoverIconDownColor = clNone)) then
        begin
          if FMouseDown then
            Canvas.Pen.Color := clBlack
          else
            Canvas.Pen.Color := clWhite;

          Canvas.MoveTo(IcoRect.Left, IcoRect.Bottom);
          Canvas.LineTo(IcoRect.Left, IcoRect.Top);
          Canvas.LineTo(IcoRect.Right, IcoRect.Top);
          if FMouseDown then
            Canvas.Pen.Color := clWhite
          else
            Canvas.Pen.Color := clBlack;

          Canvas.LineTo(IcoRect.Right, IcoRect.Bottom);
          Canvas.LineTo(IcoRect.Left, IcoRect.Bottom);
          FMouseItem := item;
        end;

        if Panel.HoverCaption and not HTMLText then
        begin
          CR := R;
          MouseRect := AR;
          InflateRect(MouseRect, -1, -1);
          if PtInRect(MouseRect, pt) then
          begin
            Canvas.Font.Style := Canvas.Font.Style + [fsUnderline];
            FMouseItem := item;
          end;
        end;

        SetBkMode(Canvas.Handle, TRANSPARENT);

        case Panel.Alignment of
          taLeftJustify: DTSTYLE := DTSTYLE or DT_LEFT;
          taCenter: DTSTYLE := DTSTYLE or DT_CENTER;
          taRightJustify: DTSTYLE := DTSTYLE or DT_RIGHT;
        end;

        R.Left := R.Left + Panel.Indent;
        R.Left := R.Left + Item.Indent;
// R.Right := R.Right + Panel.Indent;

        if HTMLText then
        begin
          Item.HotAnchor := '';

          if HTMLDrawEx(Canvas, text, R, Images, pt.X, pt.Y, -1, -1, 2, True, False, False, False, False, False, True,
            1.0, clBlue, clNone, clNone, clNone, a, s, fa, xs, ys, ml, hl, mr) then
            Item.HotAnchor := a;

          HTMLDrawEx(Canvas, text, R, Images, pt.X, pt.Y, -1, -1, 2, False, False, False, False, False, False, True,
            1.0, clBlue, clNone, clNone, clNone, a, s, fa, xs, ys, ml, hl, mr);

        end
        else
        begin
          item.Flocalrect := r;
          if (item <> iteminplace) or (not visiblelabelinplace) then
          begin
            if panel.WordWrap then
              Text := prepareWordWraptext(panel, text, R);
            case panel.Alignment of
              taLeftJustify: DrawText(Canvas.Handle, PChar(text), Length(Text), R, DTSTYLE);
              taRightJustify: DrawText(Canvas.Handle, PChar(text), Length(Text), R, DTSTYLE or DT_RIGHT);
              taCenter: DrawText(Canvas.Handle, PChar(text), Length(Text), R, DTSTYLE or DT_CENTER);
            end;
          end;
        end;

        if not Panel.Splitter.Empty then
        begin
          Ind := Width - Panel.Splitter.Width;
          if Ind < 0 then
            Ind := 0;
          Canvas.Draw(AR.Left + Ind, AR.Bottom - Panel.Splitter.Height, Panel.Splitter);
        end;

      end;
    psSmallIcon:
      begin
        R.Left := R.Left + Panel.Indent;
        R.Right := R.Right + Panel.Indent;

        case Panel.Alignment of
          taLeftJustify: DTSTYLE := DTSTYLE or DT_LEFT;
          taCenter: DTSTYLE := DTSTYLE or DT_CENTER;
          taRightJustify: DTSTYLE := DTSTYLE or DT_RIGHT;
        end;

        if Panel.HoverFullWidth then
        begin
          IcoRect := R;
        end
        else
        begin
          IcoRect.Top := R.Top + 4;
          IcoRect.Bottom := IcoRect.Top + 16;
          case panel.Alignment of
            taCenter:
              begin
                IcoRect.Left := R.Left + ((R.Right - R.Left - rw) div 2) - 6;
              end;
            taLeftJustify:
              begin
                IcoRect.Left := 4 + R.Left;
              end;
            taRightJustify:
              begin
                IcoRect.Left := R.Right - 16 - rw - 4;
              end;
          end;

          IcoRect.Right := IcoRect.Left + 16;
          InflateRect(IcoRect, 2, 2);
        end;

        IcoRect.Top := r.Top + ((r.Bottom - r.Top) div 2) - 9;
        IcoRect.Bottom := IcoRect.Top + 16;

        MouseRect := IcoRect;

        InflateRect(MouseRect, -1, -1);

        HTMLText := IsHTML(text);

        if not InDesign then
        begin
          if ((Panel.HoverIconColor <> clNone) and (Panel.HoverIconDownColor <> clNone)
            and PtInRect(MouseRect, pt) and (HasIcon or Panel.HoverFullWidth)) or Item.Down then
          begin
            Canvas.Pen.Color := clBlack;
            Canvas.Pen.Width := 1;
            IcoRect.Top := icoRect.Top - 1;
            IcoRect.Bottom := icoRect.Top + 19;

            if FMouseDown or Item.Down then
              Canvas.Brush.Color := Panel.HoverIconDownColor
            else
              Canvas.Brush.Color := Panel.HoverIconColor;

            if FMouseDown  then
            begin
              clr := Panel.SelectColor;
              clrTo := Panel.SelectColorTo;
            end
            else
            begin
              clr := Panel.HoverIconColor;
              clrTo := Panel.HoverIconColorTo;
              Canvas.Brush.Color := clr;
            end;

            if Panel.HoverIconBorderColor <> clNone then
              Canvas.Pen.Color := Panel.HoverIconBorderColor
            else
              Canvas.Pen.Color := clBlack;

            if (clrto <> clNone) then
              DrawGradient(Canvas, clr, clrTo, 128, IcoRect, false)
            else
              Canvas.Rectangle(IcoRect.Left, IcoRect.Top, IcoRect.Right, IcoRect.Bottom);

            FMouseItem := item;
          end;
        end;

  // for URLs, the icons are aligned immediately near the text
        if Panel.HoverFullWidth then
        begin
          MouseRect := R;
          IcoRect.Top := R.Top;
          IcoRect.Bottom := IcoRect.Top + 16;

          case panel.Alignment of
            taCenter:
              begin
                IcoRect.Left := R.Left + ((R.Right - R.Left - rw) div 2) - 6;
              end;
            taLeftJustify:
              begin
                IcoRect.Left := 4 + R.Left;
              end;
            taRightJustify:
              begin
                IcoRect.Left := R.Right - 16 - rw - 2;
              end;
          end;
          IcoRect.Right := IcoRect.Left + 16;
          IcoRect.Top := r.Top + ((r.Bottom - r.top) div 2) - 9;
          IcoRect.Bottom := IcoRect.Top + 16;
        end
        else
        begin

          if Panel.ImageAlign = iaLeft then
            MouseRect := IcoRect
           else
             MouseRect := Rect(R.Right - 16, IcoRect.Top, R.Right, IcoRect.Bottom);
        end;

        InflateRect(MouseRect, -1, -1);

        if Item.AutoIcon and (Item.URL <> '') then
        begin
          if (Pos('.EXE', UpperCase(Item.URL)) > 0) or
            (Pos('.ICO', UpperCase(Item.URL)) > 0) or
            (Pos('.DLL', UpperCase(Item.URL)) > 0) then
          begin
            HIcon := ExtractIcon(HInstance, PChar(Item.URL), 0);
            DrawIconEx(Canvas.Handle, IcoRect.Left {+ 4}, IcoRect.Top + 2, HIcon, 16, 16, 0, 0, DI_Normal);
            DestroyIcon(HIcon);
          end
          else
          begin
            if Pos('://', Item.URL) > 0 then
              SysImageEx(Canvas, IcoRect.Left {+ 4}, IcoRect.Top + 2, 'a.htm', False, True)
            else
              SysImageEx(Canvas, IcoRect.Left {+ 4}, IcoRect.Top + 2, Item.URL, False, True)
          end;

          IcoRect.Left := IcoRect.Left {+ 4};
          IcoRect.Top := IcoRect.Top + 2;
          IcoRect.Right := IcoRect.Left + 16;
          IcoRect.Bottom := IcoRect.Top + 16;
          R.Left := R.Left + 16 + 4;
        end;

        if Assigned(FImages) and (Item.ImageIndex > -1) and not Item.AutoIcon then
        begin
          if (FImages.Height < Panel.ItemHeight) then
            VInd := (Panel.ItemHeight - FImages.Height) div 2
          else
            VInd := 0;

          if Panel.ImageAlign = iaLeft then
          begin
            FImages.Draw(Canvas, R.Left, R.Top+ VInd, Item.ImageIndex);
            R.Left := R.Left + FImages.Width + 2;
          end
          else
          begin
            FImages.Draw(Canvas, R.Right - FImages.Width-1, R.Top + VInd, Item.ImageIndex);
            R.Right := R.Right - FImages.Width - 2;
          end;
        end;

        if (TempIcon <> nil) and (Item.ImageIndex = -1) and not Item.AutoIcon then
        begin
          if not TempIcon.Empty then
          begin
            IcoRect.Top := R.Top + 1; // + 4;
            IcoRect.Bottom := IcoRect.Top + 16;
            case panel.Alignment of
              taCenter:
                begin
                  IcoRect.Left := R.Left + ((R.Right - R.Left - rw) div 2) - 6;
                end;
              taLeftJustify:
                begin
                  IcoRect.Left := 4 + R.Left;
                end;
              taRightJustify:
                begin
                  IcoRect.Left := R.Right - 16 - 4 - rw;
                end;
            end;

            if Panel.ImageAlign = iaLeft then
            begin
              IcoRect.Right := IcoRect.Left + 16;
              IcoRect.Top := r.Top + ((r.Bottom - r.Top) div 2) - 8;
              IcoRect.Bottom := IcoRect.Top + 16;

              DrawIconEx(Canvas.Handle, IcoRect.Left, IcoRect.Top, TempIcon.Handle, 16, 16, 0, 0, DI_Normal);
              InflateRect(IcoRect, 2, 0);
              IcoRect.top := IcoRect.top - 1;
              IcoRect.Bottom := IcoRect.top + 16;
              R.Left := R.Left + 8 + 16;
            end
            else
            begin
              IcoRect.Right := r.Right;
              IcoRect.Left := IcoRect.Right - 16;

              IcoRect.Top := r.Top + ((r.Bottom - r.Top) div 2) - 8;
              IcoRect.Bottom := IcoRect.Top + 16;

              DrawIconEx(Canvas.Handle, IcoRect.Left, IcoRect.Top, TempIcon.Handle, 16, 16, 0, 0, DI_Normal);
              InflateRect(IcoRect, 2, 0);

              IcoRect.top := IcoRect.top - 1;
              IcoRect.Bottom := IcoRect.top + 16;
              R.Right := R.Right - 8 - 16;
            end;

            r.Top := R.top + 2;
          end;
        end;

        if PtInRect(MouseRect, pt) and HasIcon and
          ((Panel.HoverIconColor = clNone) or (Panel.HoverIconDownColor = clNone)) then
        begin
          if FMouseDown then
            Canvas.Pen.Color := clBlack
          else
            Canvas.Pen.Color := clWhite;

          Canvas.MoveTo(IcoRect.Left, IcoRect.Bottom + 2);
          Canvas.LineTo(IcoRect.Left, IcoRect.Top);
          Canvas.LineTo(IcoRect.Right, IcoRect.Top);

          if FMouseDown then
            Canvas.Pen.Color := clWhite
          else
            Canvas.Pen.Color := clBlack;

          Canvas.LineTo(IcoRect.Right, IcoRect.Bottom + 2);
          Canvas.LineTo(IcoRect.Left, IcoRect.Bottom + 2);
          FMouseItem := item;
        end;

        if Panel.HoverCaption and not HTMLText then
        begin
          CR := R;
    // H := DrawText(Canvas.Handle,PChar(Item.Caption),Length(Item.Caption),CR,DT_CENTER or DTSTYLE or DT_CALCRECT);

          MouseRect := AR;
          InflateRect(MouseRect, -1, -1);
          if PtInRect(MouseRect, pt) then
            Canvas.Font.Style := Canvas.Font.Style + [fsUnderline];
        end;

        SetBkMode(Canvas.Handle, TRANSPARENT);

        if IsHTML(text) then
        begin
          Item.HotAnchor := '';

          R.Top := R.Top + 4;
          R.Bottom := R.Bottom + 4;


          if HTMLDrawEx(Canvas, text, R, Images, pt.X, pt.Y, -1, -1, 2, True, False, False, False, False, False, panel.WordWrap,
            1.0, clBlue, clNone, clNone, clNone, a, s, fa, xs, ys, ml, hl, mr) then
            Item.HotAnchor := a;

          HTMLDrawEx(Canvas, text, R, Images, r.Left, r.Top, -1, -1, 2, False, False, False, False, False, False, panel.WordWrap,
            1.0, clBlue, clNone, clNone, clNone, a, s, fa, xs, ys, ml, hl, mr)
        end
        else
        begin
          case panel.Alignment of
            taLeftJustify: r.Right := min(r.Right, r.Left + rw);
            taRightJustify: r.Left := r.Right - rw;
            taCenter:
              begin
                r.Left := r.Left + ((r.Right - r.Left) div 2) - (rw div 2);
                r.Right := min(r.Left + rw, r.Right);
              end;
          end;

          item.FLocalRect := r;
          if (item <> iteminplace) or (not visiblelabelinplace) then
          begin
            if panel.WordWrap then
              Text := PrepareWordWraptext(panel, Text, r);

            DrawText(Canvas.Handle, PChar(Text), Length(Text), R, DTSTYLE);
          end;
        end;

        if not Panel.Splitter.Empty then
        begin
          Ind := Width - Panel.Splitter.Width;
          if Ind < 0 then
            Ind := 0;
          Canvas.Draw(AR.Left + Ind, AR.Bottom - Panel.Splitter.Height, Panel.Splitter);
        end;
      end;
    psButtons:
      begin
        InflateRect(R, -1, -1);
        (*
        if Item.Down and not (PtInRect(R, pt) and not FMouseDown) and
          not (csDesigning in ComponentState) then
        begin
          Canvas.Brush.Color := Item.InspectorPanel.ButtonDownColor;
          Canvas.Pen.Color := Canvas.Brush.Color;
          Canvas.Rectangle(R.Left,R.Top,R.Right,R.Bottom);

          DrawGradient(Canvas, panel.SelectDownColor, panel.SelectDownColorTo, 128 , R, false)
        end;
        *)

        if PtInRect(R, pt) or Item.Down then
        begin
          if FMouseDown then
          begin
            if Item.Down then
            begin
              if Panel.SelectDownColorTo <> clNone then
                DrawTheItem(true, Panel.SelectDownColorTo, Panel.SelectDownColor)
              else
                if Panel.SelectDownColor <> clNone then
                  DrawTheItem(Panel.SelectDownColorTo <> clNone, Panel.SelectDownColor, Panel.SelectDownColorTo)
                else
                  DrawTheItem(false, Panel.Color, Panel.Color)
            end
            else
            begin
              if Panel.SelectDownColorTo <> clNone then
                DrawTheItem(true, Panel.SelectDownColorTo, Panel.SelectDownColor)
              else
                if Panel.SelectDownColor <> clNone then
                  DrawTheItem(Panel.SelectDownColorTo <> clNone, Panel.SelectDownColor, Panel.SelectDownColorTo)
                else
                  DrawTheItem(false, Panel.Color, Panel.Color)

              //DrawTheItem(Panel.SelectDownColorTo <> clNone, Panel.SelectDownColorTo, Panel.SelectDownColor);
            end;
          end
          else
          begin
            if Item.Down then
            begin
              if not PtInRect(R, pt) then
              begin
                if Panel.SelectDownColor <> clNone then
                  DrawTheItem(Panel.SelectDownColorTo <> clNone, Panel.SelectDownColor, Panel.SelectDownColorTo)
                else
                  DrawTheItem(false, Panel.Color, Panel.Color)
              end
              else
              begin
                if Panel.SelectDownColor <> clNone then
                  DrawTheItem(Panel.SelectDownColorTo <> clnone, Panel.SelectDownColor, Panel.SelectDownColorTo)
                else
                  DrawTheItem(false, Panel.Color, Panel.Color)
              end;
            end
            else
            begin
              if Panel.HoverIconColor <> clNone then
                DrawTheItem(Panel.HoverIconColorTo <> clNone, Panel.HoverIconColor, Panel.HoverIconColorTo)
              else
                DrawTheItem(false, Panel.Color, Panel.Color)
            end;
          end;

        end;


        OffsetRect(R, 2, 2);
        if Assigned(FImages) and (Item.ImageIndex > -1) then
        begin
          FImages.Draw(Canvas, R.Left, R.Top, Item.ImageIndex);
          R.Left := R.Left + FImages.Width + 2;
        end;
        Canvas.Font.Assign(Panel.Font);

        SetBKMode(Canvas.Handle, TRANSPARENT);

        if Item.Down then
          Canvas.Font.Color := Panel.SelectFontColor;
  // Caption & Value here
        if IsHTML(text) then
        begin
          Item.HotAnchor := '';

          if HTMLDrawEx(Canvas, text, R, Images, pt.X, pt.Y, -1, -1, 2, True, False, False, False, False, False, True,
            1.0, clBlue, clNone, clNone, clNone, a, s, fa, xs, ys, ml, hl, mr) then
            Item.HotAnchor := a;

          HTMLDrawEx(Canvas, text, R, Images, r.Left, r.Top, -1, -1, 2, False, False, False, False, False, False, True,
            1.0, clBlue, clNone, clNone, clNone, a, s, fa, xs, ys, ml, hl, mr)
        end
        else
        begin
          uFormat := DT_LEFT;
          case Panel.Alignment of
            taCenter:       uFormat := DT_CENTER;
            taLeftJustify:  uFormat := DT_LEFT;
            taRightJustify: uFormat := DT_RIGHT;
          end;

          if Item.InspectorPanel.WordWrap then
            DrawText(Canvas.Handle, PChar(text), Length(text), R, {DT_LEFT}uFormat or DT_WORDBREAK)
          else
            DrawText(Canvas.Handle, PChar(text), Length(text), R, {DT_LEFT}uFormat or DT_END_ELLIPSIS);
        end
      end;
    psProperties:
      begin
        if Item.PropertyType = ptPassword then
          ValueText := StringOfChar('*', length(Item.TextValue))
        else
          ValueText := Item.TextValue;

        if Assigned(FOnItemValue) and (Item.PropertyType <> ptFont) then
          FOnItemValue(self, Panel, Item, ValueText);

        if Panel.GridLines then
        begin
          Canvas.Pen.Color := Panel.GridLineColor;
          Canvas.MoveTo(R.Left, R.Bottom - 1);
          Canvas.LineTo(R.Right, R.Bottom - 1);
          Canvas.MoveTo(R.Left + Panel.CaptionWidth, R.Top);
          Canvas.LineTo(R.Left + Panel.CaptionWidth, R.Bottom);
          Canvas.Pen.Color := clWhite;
          Canvas.MoveTo(R.Left + Panel.CaptionWidth + 1, R.Top);
          Canvas.LineTo(R.Left + Panel.CaptionWidth + 1, R.Bottom);
        end;

        DrwRect := R;
        R.Right := R.Left + Panel.CaptionWidth;

        if Panel.CaptionColor <> clNone then
        begin
          Canvas.Brush.Color := Panel.CaptionColor;
          if Panel.GridLines then
            R.Bottom := R.Bottom - 1;
          Canvas.FillRect(R);
          if Panel.GridLines then
            R.Bottom := R.Bottom + 1;
          Canvas.Brush.Color := Panel.Color;
        end;

        R.Left := R.Left + 4;
        R.Top := R.Top + 2;


        if Panel.EditBox and (Item.PropertyType <> ptButton) and not Item.ReadOnly then
          R.Top := R.Top + BoxOffset;

        if Assigned(FImages) and (Item.ImageIndex > -1) then
        begin
          FImages.Draw(Canvas, R.Left, R.Top, Item.ImageIndex);
          R.Left := R.Left + FImages.Width + 2;
        end;

        R.Left := R.Left + Item.Indent + Item.Level * 12 + Panel.Indent;

        if (Item.NumChilds > 0) then
        begin
          DrawItemNode(R.Left - 20, r.Top, Item.NodeState = nsClose);
        end;

        Canvas.Font.Assign(Panel.CaptionFont);

        SetBKMode(Canvas.Handle, TRANSPARENT);

  // Caption & Value here
        if IsHTML(Text) then
        begin
          Item.HotAnchor := '';

          Rgn := CreateRectRgn(R.Left, R.Top, R.Right, R.Bottom);

          SelectClipRgn(Canvas.Handle, Rgn);

          if HTMLDrawEx(Canvas, Text, R, Images, pt.X, pt.Y, -1, -1, 2, True, False, False, False, False, False, True,
            1.0, clBlue, clNone, clNone, clNone, a, s, fa, xs, ys, ml, hl, mr) then
            Item.HotAnchor := a;

          HTMLDrawEx(Canvas, Text, R, Images, r.Left, r.Top, -1, -1, 2, False, False, False, False, False, False, True,
            1.0, clBlue, clNone, clNone, clNone, a, s, fa, xs, ys, ml, hl, mr);

          SelectClipRgn(Canvas.Handle, Rgn);
          DeleteObject(Rgn);

        end
        else
          DrawText(Canvas.Handle, PChar(Text), Length(Text), R, DT_LEFT or DT_END_ELLIPSIS);

        Canvas.Font.Assign(Panel.Font);

        R.Left := R.Right + 4;
        R.Right := DrwRect.Right - 4;

//      if (FShowUP or FShowDown) and (FMode = imSinglePanelActive) then
//         Dec(R.Right,20);

        if Panel.EditBox and (Item.PropertyType <> ptButton) and not Item.ReadOnly then
        begin
          R.Top := R.Top - BoxOffset;
          InflateRect(R, -BoxOffset, -BoxOffset);
          R.Bottom := R.Bottom - BoxOffset - 2;

          if Panel.EditBorderColor <> clNone then
          begin
            Canvas.Pen.Color := Panel.EditBorderColor;
            Canvas.Pen.Width := 1;
          end;
          Canvas.Brush.Color := Panel.EditColor;
          Canvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
          R.Left := R.Left + 3;
          R.Top := R.Top + 1;
        end;

        IEButton := ebNone;

        if Item.PropertyType = ptCustom then
        begin
          if Assigned(OnCustomEditButton) then
            OnCustomEditButton(Self, Item, IEButton);
        end;

        if Item.PropertyType in [ptColor, ptFixedColor] then
        begin
          DrwRect := Rect(R.Left + 2, R.Top + 2, R.Left + 20, R.Top + 16);
          Canvas.Pen.Color := clBlack;
          Canvas.Brush.Color := Item.ColorValue;
          Canvas.Rectangle(DrwRect.Left, DrwRect.Top, DrwRect.Right, DrwRect.Bottom);
          R.Left := R.Left + 22;
          R.Top := R.Top + 2;
          Canvas.Brush.Color := Color;
        end;

        if ((Item.PropertyType in [ptValues, ptValuesList, ptDate, ptColor, ptFixedColor]) or
          (IEButton = ebDropDown)) and ShowEditorAlways then
        begin
          if Panel.EditBox then
          begin
            R.Right := R.Right - BoxOffset - 1;
            R.Top := R.Top + BoxOffset;
          end;

          if DoVisualStyles then
          begin
            HTheme := OpenThemeData(Handle, 'combobox');

            DrwRect := Rect(R.Right - 15, R.Top , R.Right, Min(R.Top + 17, R.Bottom - 1));

            GetCursorPos(pt);
            pt := ScreenToClient(pt);
            Hot := PtInRect(DrwRect, pt);

            if Hot then
              DrawThemeBackground(HTheme, Canvas.Handle, CP_DROPDOWNBUTTON, CBXS_HOT, @DrwRect, nil)
            else
              DrawThemeBackground(HTheme, Canvas.Handle, CP_DROPDOWNBUTTON, CBXS_NORMAL, @DrwRect, nil);

            CloseThemeData(HTheme);
          end
          else
            DrawFrameControl(Canvas.Handle, Rect(R.Right - 16, R.Top, R.Right, R.Top + 18), DFC_SCROLL, DFCS_SCROLLCOMBOBOX or DFCS_FLAT);
          R.Right := R.Right - 16;
        end;

        if (Item.PropertyType = ptButton) then
        begin
          R.Right := R.Right - BoxOffset - 2;
          R.Top := R.Top + BoxOffset;

          if DoVisualStyles then
          begin
            HTheme := OpenThemeData(Handle, 'button');

            DrwRect := Rect(R.Left, R.Top - 1, R.Right, R.Bottom - 2);

            if Item.ItemFocused and (GetFocus = Handle) then
              FocusStyle := PBS_DEFAULTED
            else
              FocusStyle := 0;

            GetCursorPos(pt);
            pt := ScreenToClient(pt);
            Hot := PtInRect(DrwRect, pt);

            if Item.ReadOnly then
              DrawThemeBackground(HTheme, Canvas.Handle, BP_PUSHBUTTON, PBS_DISABLED, @DrwRect, nil)
            else
            begin
              if FMouseDown and Hot then
              begin
                DrawThemeBackground(HTheme, Canvas.Handle, BP_PUSHBUTTON, PBS_PRESSED or FocusStyle, @DrwRect, nil)
              end
              else
              begin
                if Hot then
                  DrawThemeBackground(HTheme, Canvas.Handle, BP_PUSHBUTTON, PBS_HOT or FocusStyle, @DrwRect, nil)
                else
                  DrawThemeBackground(HTheme, Canvas.Handle, BP_PUSHBUTTON, PBS_NORMAL or FocusStyle, @DrwRect, nil);
              end;

              CloseThemeData(HTheme);
            end;
            DrwRect := Rect(R.Left, R.Top, R.Right, R.Bottom - 2);
          end
          else
          begin
            DrwRect := Rect(R.Left, R.Top - 1, R.Right, R.Bottom - 2);
            Hot := PtInRect(DrwRect, pt);

            if Item.ReadOnly then
              DrawFrameControl(Canvas.Handle, DrwRect, DFC_BUTTON, DFCS_BUTTONPUSH or DFCS_INACTIVE)
            else
            begin
              if FMouseDown and Hot then
                DrawFrameControl(Canvas.Handle, DrwRect, DFC_BUTTON, DFCS_BUTTONPUSH or DFCS_PUSHED)
              else
                DrawFrameControl(Canvas.Handle, DrwRect, DFC_BUTTON, DFCS_BUTTONPUSH);
            end;

            DrwRect := Rect(R.Left, R.Top, R.Right, R.Bottom);
          end;

          if Item.InspectorPanel.EditBox then
            DrwRect.Top := DrwRect.Top + BoxOffset;


          SetBKMode(Canvas.Handle, TRANSPARENT);

          if Item.ReadOnly then
            Canvas.Font.Color := clGray;

          DrawText(Canvas.Handle, PChar(ValueText), Length(ValueText), DrwRect, DT_CENTER or DT_SINGLELINE or DT_VCENTER);

          if not (DoVisualStyles and not Flat) then
          begin
            InflateRect(DrwRect, -2, -2);

            if Item.ItemFocused and (GetFocus = Handle) then
              DrawFocusRect(Canvas.Handle, DrwRect);
          end;
        end;

        if ((Item.PropertyType in [ptTextButton, ptPropButton, ptFont]) or (IEButton = ebMore))
          and ShowEditorAlways then
        begin
          if Panel.EditBox then
          begin
            R.Right := R.Right - BoxOffset - 2;
            R.Top := R.Top + BoxOffset;
          end;

          if DoVisualStyles and not Flat then
          begin
            HTheme := OpenThemeData(Handle, 'button');

            DrwRect := Rect(R.Right - 18, R.Top - 1, R.Right, R.Bottom - 2);

            GetCursorPos(pt);
            pt := ScreenToClient(pt);
            Hot := PtInRect(DrwRect, pt);

            if Hot then
              DrawThemeBackground(HTheme, Canvas.Handle, BP_PUSHBUTTON, PBS_HOT, @DrwRect, nil)
            else
              DrawThemeBackground(HTheme, Canvas.Handle, BP_PUSHBUTTON, PBS_NORMAL, @DrwRect, nil);

            CloseThemeData(HTheme);
            DrwRect := Rect(R.Right - 12, R.Top + 2, R.Right, R.Bottom);
          end
          else
          begin
            DrawFrameControl(Canvas.Handle, Rect(R.Right - 14, R.Top, R.Right, R.Top + 18), DFC_BUTTON, DFCS_BUTTONPUSH or DFCS_FLAT);
            DrwRect := Rect(R.Right - 10, R.Top + 2, R.Right, R.Bottom);
          end;

          SetBKMode(Canvas.Handle, TRANSPARENT);
          DrwRect.Left := DrwRect.Left - 2;
          DrawText(Canvas.Handle, '...', 3, DrwRect, DT_LEFT);
          R.Right := R.Right - 16;
        end;

        if ((Item.PropertyType in [ptIntSpin, ptTime]) or (IEButton = ebSpin)) and ShowEditorAlways then
        begin
          if Panel.EditBox then
          begin
            R.Right := R.Right - BoxOffset - 2;
            R.Top := R.Top + BoxOffset;
          end;

          H := (R.Bottom - R.Top) div 2;

          if DoVisualStyles then
          begin
            Dec(h, 2);
            HTheme := OpenThemeData(Self.Handle, 'spin');

            DrwRect := Rect(R.Right - 17, R.Top, R.Right, R.Top + H + 2);

            GetCursorPos(pt);
            pt := ScreenToClient(pt);
            Hot := PtInRect(DrwRect, pt);
            Down := Hot and FMouseDown;

            if Down then
              DrawThemeBackground(HTheme, Canvas.Handle, SPNP_UP, UPS_PRESSED, @DrwRect, nil)
            else
              if Hot then
                DrawThemeBackground(HTheme, Canvas.Handle, SPNP_UP, UPS_HOT, @DrwRect, nil)
              else
                DrawThemeBackground(HTheme, Canvas.Handle, SPNP_UP, UPS_NORMAL, @DrwRect, nil);

            DrwRect := Rect(R.Right - 17, R.Top + H + 2, R.Right, R.Top + 2 + 2 * H + 1);

            Hot := PtInRect(DrwRect, pt);
            Down := Hot and FMouseDown;

            if Down then

              DrawThemeBackground(HTheme, Canvas.Handle, SPNP_DOWN, DNS_PRESSED, @DrwRect, nil)
            else
              if Hot then
                DrawThemeBackground(HTheme, Canvas.Handle, SPNP_DOWN, DNS_HOT, @DrwRect, nil)
              else
                DrawThemeBackground(HTheme, Canvas.Handle, SPNP_DOWN, DNS_NORMAL, @DrwRect, nil);

            CloseThemeData(HTheme);
          end
          else
          begin
            DrawFrameControl(Canvas.Handle, Rect(R.Right - 16, R.Top, R.Right, R.Top + H), DFC_SCROLL, DFCS_SCROLLUP or DFCS_FLAT);
            DrawFrameControl(Canvas.Handle, Rect(R.Right - 16, R.Top + H, R.Right, R.Top + 2 * H), DFC_SCROLL, DFCS_SCROLLDOWN or DFCS_FLAT);
          end;
          R.Right := R.Right - 16;
        end;

        if Item.PropertyType = ptBoolean then
        begin
          if DoVisualStyles then
          begin
            HTheme := OpenThemeData(Self.Handle, 'button');

            DrwRect := Rect(R.Left, R.Top, R.Left + 14, R.Top + 14);

            if Item.InspectorPanel.EditBox then
              DrwRect.Top := DrwRect.Top + BoxOffset;

            GetCursorPos(pt);
            pt := ScreenToClient(pt);
            Hot := PtInRect(DrwRect, pt);
            Down := Hot and FMouseDown;

            if Item.BoolValue then
            begin
              if Down then
                DrawThemeBackground(HTheme, Canvas.Handle, BP_CHECKBOX, CBS_UNCHECKEDPRESSED, @DrwRect, nil)

              else
                if Hot then
                  DrawThemeBackground(HTheme, Canvas.Handle, BP_CHECKBOX, CBS_CHECKEDHOT, @DrwRect, nil)
                else
                  DrawThemeBackground(HTheme, Canvas.Handle, BP_CHECKBOX, CBS_CHECKEDNORMAL, @DrwRect, nil)

            end
            else
            begin
              if Down then
                DrawThemeBackground(HTheme, Canvas.Handle, BP_CHECKBOX, CBS_CHECKEDPRESSED, @DrwRect, nil)
              else
                if Hot then
                  DrawThemeBackground(HTheme, Canvas.Handle, BP_CHECKBOX, CBS_UNCHECKEDHOT, @DrwRect, nil)
                else
                  DrawThemeBackground(HTheme, Canvas.Handle, BP_CHECKBOX, CBS_UNCHECKEDNORMAL, @DrwRect, nil);
            end;

            CloseThemeData(HTheme);
          end
          else
          begin
            if Item.BoolValue then
              DrawFrameControl(Canvas.Handle, Rect(R.Left, R.Top, R.Left + 14, R.Top + 14), DFC_BUTTON, DFCS_BUTTONCHECK or DFCS_FLAT or DFCS_MONO or DFCS_CHECKED)
            else
              DrawFrameControl(Canvas.Handle, Rect(R.Left, R.Top, R.Left + 14, R.Top + 14), DFC_BUTTON, DFCS_BUTTONCHECK or DFCS_FLAT or DFCS_MONO);
          end;
          R.Left := R.Left + 16;
        end;

        if Item.PropertyType = ptPicture then
        begin
          if Assigned(Item.PictureValue.Graphic) then
          begin
            bmp := TBitMap.Create;
            try
              bmp.Width := min(R.Right - R.Left,Item.PictureValue.Width);
              bmp.Height := min(R.Bottom - R.Top,Item.PictureValue.Height);
              bmp.Canvas.Draw(0,0,Item.PictureValue.Graphic);
              if Item.PictureValue.Graphic.ClassName = 'TBitmap' then
                bmp.Transparent := True;
              Canvas.Draw(R.Left, R.top , bmp);
              R.Left := R.Left + Item.PictureValue.Width;
            finally
              bmp.Free;
            end;
          end;
        end;

        if Panel.EditFontColor <> clNone then
          Canvas.Font.Color := Panel.EditFontColor
        else
          Canvas.Font.Color := Item.FontValue.Color;


        SetBKMode(Canvas.Handle, TRANSPARENT);

        DefDraw := True;

        if Item.PropertyType = ptCustom then
        begin
          if Assigned(FOnCustomEditDraw) then
          begin
            DefDraw := False;
            FOnCustomEditDraw(Self, Item, Canvas, R, DefDraw);
          end;
        end;

        if Item.PropertyType = ptButton then
          DefDraw := False;

        if Item.Modified and Panel.ShowModified then
          Canvas.Font.Assign(Panel.ModifiedFont);

        SetBkMode(Canvas.Handle, TRANSPARENT);

        if DefDraw then
        begin
          DTSTYLE := DTSTYLE and not DT_VCENTER or DT_NOPREFIX;
          if Item.PropertyType = ptFont then
          begin
            FontValue := Item.FontValue.Name;
            if Assigned(FOnItemValue) then
              FOnItemValue(self, Panel, Item, FontValue);

            Canvas.Font.Assign(Item.FontValue);
            DrawText(Canvas.Handle, PChar({Item.FontValue.Name}FontValue), Length({Item.FontValue.Name}FontValue), R, DTSTYLE or DT_END_ELLIPSIS);
          end
          else
          begin
            if not ((Item.PropertyType = ptBoolean) and not CheckTextShow) then
              DrawText(Canvas.Handle, PChar(ValueText), Length(ValueText), R, DTSTYLE);
          end;

          if Item.PropertyType = ptBoolean then
          begin
            if Item.ItemFocused and (GetFocus = Handle) then
            begin
              if Item.BoolValue then
                mr := Rect(R.Left, R.Top, R.Left + Canvas.TextWidth(CheckTrue) + 2, R.Top + Canvas.TextHeight(CheckTrue) + 2)
              else
                mr := Rect(R.Left, R.Top, R.Left + Canvas.TextWidth(CheckFalse) + 2, R.Top + Canvas.TextHeight(CheckFalse) + 2);

              DrawFocusRect(Canvas.Handle, mr);
            end;
          end;
        end;

        if not Panel.Splitter.Empty then
        begin
          Ind := Width - Panel.Splitter.Width;
          if Ind < 0 then
            Ind := 0;
          Canvas.Draw(AR.Left + Ind, AR.Bottom - Panel.Splitter.Height, Panel.Splitter);
        end;
      end;
  end;
end;

procedure TInspectorBar.DrawButtonUp;
var
  r: TRect;
  HTheme: THandle;
  pt: TPoint;
  Hot: Boolean;
  y: Integer;
begin
  y := (getNumbervisiblePanelUp(getRealTopPanel) + 1) * PanelCaption.Height;
  if y < 0 then
    Exit;
  r.Bottom := y + 18;
  r.Right := self.Width;
  r.Left := r.Right - 18;
  r.Top := y;
  if DoVisualStyles then
  begin
    HTheme := OpenThemeData(Handle, 'scrollbar');

    GetCursorPos(pt);
    pt := ScreenToClient(pt);
    Hot := PtInRect(r, pt);

    if FMouseDown and Hot then
      DrawThemeBackground(HTheme, Canvas.Handle, SBP_ARROWBTN, ABS_UPPRESSED, @r, nil)
    else
      if Hot then
        DrawThemeBackground(HTheme, Canvas.Handle, SBP_ARROWBTN, ABS_UPHOT, @r, nil)
      else
        DrawThemeBackground(HTheme, Canvas.Handle, SBP_ARROWBTN, ABS_UPNORMAL, @r, nil);

    CloseThemeData(HTheme);
  end
  else
    DrawFrameControl(Canvas.Handle, r, DFC_SCROLL, DFCS_SCROLLUP);
end;

procedure TInspectorBar.DrawButtonDown;
var
  r: TRect;
  HTheme: THandle;
  pt: TPoint;
  Hot: Boolean;
  y: Integer;
begin
  y := BarHeight - (getNumbervisiblePanelDown(GetRealtoppanel)) * PanelCaption.Height;
  r.Bottom := y;
  r.Right := self.Width;
  r.Left := r.Right - 18;
  r.Top := y - 18;
  if DoVisualStyles then
  begin
    HTheme := OpenThemeData(Handle, 'scrollbar');

    GetCursorPos(pt);
    pt := ScreenToClient(pt);
    Hot := PtInRect(r, pt);

    if FMouseDown and Hot then
      DrawThemeBackground(HTheme, Canvas.Handle, SBP_ARROWBTN, ABS_DOWNPRESSED, @r, nil)
    else
      if Hot then
        DrawThemeBackground(HTheme, Canvas.Handle, SBP_ARROWBTN, ABS_DOWNHOT, @r, nil)
      else
        DrawThemeBackground(HTheme, Canvas.Handle, SBP_ARROWBTN, ABS_DOWNNORMAL, @r, nil);

    CloseThemeData(HTheme);
  end
  else
    DrawFrameControl(Canvas.Handle, r, DFC_SCROLL, DFCS_SCROLLDOWN);
end;

procedure TInspectorBar.SetMode(const Value: TInspectorBarMode);
var
  i: Integer;
begin
  if Value = imSinglePanelActive then
    FScrollBar.Visible := False
  else
  begin
    FLastOpen := 0;
    FFirstOpen := 0;
    for i := 1 to FPanels.Count - 1 do
      FPanels.Items[i].FIsOpen := False;
    i := getRealTopPanel;
    if i <> -1 then
      FPanels.Items[i].FIsOpen := True;
  end;
  FMode := Value;
  ArrangeOpenClose;
  Invalidate;
end;

procedure TInspectorBar.Scroll(Sender: TObject);
begin
  StopEdit(FEditItem);
  Invalidate;
  ArrangeControls;
  hideinplace;
end;

procedure TInspectorBar.ArrangeOpenClose;
var
  i, h: Integer;
  j: integer;
begin
  FLastOpen := -1;
  FFirstOpen := -1;
  for i := 0 to FPanels.Count - 1 do
  begin
    if not FPanels[i].Visible then continue;
    if FPanels.Items[i].FIsOpen then
    begin
      if FFirstOpen = -1 then
        FFirstOpen := i;
      FLastopen := i;
    end;
  end;

  if Mode = imMultiPanelActive then
  begin
    FLastOpen := -1;

    h := 0;
    for i := 0 to FPanels.Count - 1 do
    begin
      if not FPanels[i].Visible then continue;
      h := h + PanelCaption.Height;
      if FPanels[i].Open then
        for j := 0 to FPanels[i].Items.Count - 1 do
          if FPanels[i].Items[j].Visible then
            if FPanels[i].VariableItemHeight then
              Inc(h, FPanels[i].Items[j].Height)
            else
              Inc(h, FPanels[i].ItemHeight);
    end;

    if h > BarHeight then
    begin
      FScrollBar.Max := h - BarHeight;
      FScrollBar.LargeChange := 100;
      FScrollBar.Visible := True;
    end
    else
    begin
      FscrollBar.Visible := False;
    end;
  end;
  ArrangeControls;
end;

procedure TInspectorBar.ShadeChanged(Sender: TObject);
begin
  ShadeHeader;
  Invalidate;
end;

procedure TInspectorBar.Resize;
begin
  inherited;
  if FOldWidth <> Width then
    ShadeHeader;

  if Assigned(EditItem) then
    StopEdit(EditItem);

  FOldWidth := Width;
  ArrangeControls;
  ArrangeOpenClose;
end;

procedure TInspectorBar.ItemClicked(Button: TMouseButton; Panel: TInspectorPanel;
  Item: TInspectorItem);
var
  DefaultHandler: Boolean;
begin
  if Button = mbLeft then
  begin
    if (Item.Help <> '') and (HelpPanel.Visible) then
      HelpPanel.Text := Item.Help;

    if not StartEditingInplace(panel, item) then
    begin
      if Assigned(FOnItemClick) then
        FOnItemClick(Self, Panel, Item);
    //  FMouseDown := false;
    end;

    InvalidateItem(Panel, Item);

    if Item.HotAnchor <> '' then
    begin
      FMouseDown := False;
      if Assigned(FOnItemAnchorClick) then
        FOnItemAnchorClick(Self, Item.InspectorPanel, Item, Item.HotAnchor);
    end;

    if Item.URL <> '' then
    begin
      DefaultHandler := True;
      if Assigned(FOnURLClick) then
        FOnURLClick(Self, Item.URL, DefaultHandler);
      if DefaultHandler then
        ShellExecute(Handle, 'open', PChar(Item.URL), nil, nil, SW_NORMAL);
    end;
  end;

  if Button = mbRight then
  begin
    if Assigned(FOnItemRightClick) then
      FOnItemRightClick(Self, Panel, Item);
  end;
end;

procedure TInspectorBar.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  APanel: TInspectorPanel;
  AItem: TInspectorItem;

begin
  inherited;
  FMouseDown := False;
  FButtonDownPress := False;
  FButtonUpPress := False;

  if Panels.Count = 0 then
    Exit;

  if GetPanelItemAtXY(X, Y, APanel, AItem) then
  begin
    if Assigned(APanel) and Assigned(AItem) then
    begin
      if (APanel.Style = psProperties) and (AItem.PropertyType = ptButton) and (AItem.ReadOnly = false) then
      begin
        if TabStop then
        begin
          SetFocus;
          AItem.ItemFocused := True;
          StartEdit(AItem);
        end;

        if (X < InspectorWidth - 4) and (X > APanel.CaptionWidth + InspectorIndent + 4) then
          if Assigned(FOnButtonClick) then
            FOnButtonClick(Self, APanel, AItem);

        if TabStop then
        begin
          FEditing := true;
          StopEdit(AItem);
        end;
      end;
    end;
  end;


  if FMode = imSinglePanelActive then
    MouseUpSinglePanel(Button, Shift, X, Y)
  else
    MouseUpMultiPanel(Button, Shift, X, Y);


  Invalidate;
end;

procedure TInspectorBar.InvalidateItem(Panel: TInspectorPanel;
  Item: TInspectorItem);
var
  r: TRect;

begin
  if csLoading in ComponentState then
    Exit;

  if Mode = imSinglePanelActive then
  begin
    if Panel.Index = getRealTopPanel then
    begin
      r := GetItemRect(Panel, Item);
      {$IFDEF VER185}
      Invalidate;
      {$ELSE}
      InvalidateRect(Handle, @r, False);
      {$ENDIF}
    end;
  end
  else
  begin
// EXTEND DONE
    if Panel.FIsOpen then
    begin
  //MULTIPANEL MODE
      r := GetItemRect(Panel, Item);
      {$IFDEF VER185}
      Invalidate;
      {$ELSE}
      InvalidateRect(Handle, @r, False);
      {$ENDIF}
    end;
  end;

end;

function TInspectorBar.GetItemRect(Panel: TInspectorPanel; Item: TInspectorItem): TRect;
var
  j, i, m, RealWidth: Integer;
  tempHeight: integer;
begin
  Result := Rect(0, 0, 0, 0);
  if not Assigned(Panel) then
    Exit;
  if not Assigned(Item) then
    Exit;

  if Mode = imSinglePanelActive then
  begin
    tempHeight := 0;
    for i := Panel.TopItem to Item.Index - 1 do
      if Panel.Items[i].Visible then
        if Panel.VariableItemHeight then
          Inc(tempHeight, Panel.Items[i].Height)
        else
          Inc(tempHeight, Panel.ItemHeight);

    i := (((getNumbervisiblePanelUp(getRealTopPanel)) + 1) * PanelCaption.Height) + tempHeight;

    if Panel.VariableItemHeight then
      j := i + item.Height
    else
      j := i + Panel.ItemHeight;

    Result := Rect(0, i, Width, j);
  end
  else
  begin
// MULTIPANEL MODE
    if Panel.FIsOpen then
    begin
      if FscrollBar.Visible then j := -FscrollBar.Position
      else j := 0;
      for i := 1 to FPanels.Count do
      begin
        if not FPanels[i - 1].Visible then continue;
        if Panel = FPanels.Items[i - 1] then
        begin
          Inc(j, PanelCaption.Height); //Panel caption

          for m := 1 to Item.Index do
            if FPanels.Items[i - 1].Items[m - 1].FVisible then
              if FPanels.Items[i - 1].VariableItemHeight then
                Inc(j, FPanels.Items[i - 1].Items[m - 1].Height)
              else
                Inc(j, FPanels.Items[i - 1].ItemHeight);

          RealWidth := InspectorWidth;

          if Panel.FItems.Count = 0 then
            Result := Rect(0, 0, 0, 0)
          else
          begin
            if FPanels.Items[i - 1].VariableItemHeight then
              tempHeight := Item.Height
            else
              tempHeight := FPanels.Items[i - 1].ItemHeight;
            Result := Rect(0, j, RealWidth, j + tempHeight);
          end;
          Exit;
        end;
        if FPanels.Items[i - 1].FIsOpen then
        begin
      //for open panel
          Inc(j, PanelCaption.Height); //Panel caption

          for m := 1 to FPanels.Items[i - 1].Items.Count do
            if FPanels.Items[i - 1].Items[m - 1].FVisible then
              if FPanels.Items[i - 1].VariableItemHeight then
                Inc(j, FPanels.Items[i - 1].Items[m - 1].Height)
              else
                Inc(j, FPanels.Items[i - 1].ItemHeight);

          if i = FLastOpen + 1 then
            j := Height - (FPanels.Count - FLastOpen - 1) * PanelCaption.Height;
        end
        else
      //for close panel
          Inc(j, PanelCaption.Height);
      end;
    end;
  end;
end;

procedure TInspectorBar.SetFlat(const Value: Boolean);
begin
  FFlat := Value;
  Invalidate;
end;

procedure TInspectorBar.StartEdit(InspectorItem: TInspectorItem);
var
  EditRect: TRect;
  sl: TStringList;
  i: Integer;
  EL: TInspectorEditLink;
  pt: TPoint;
  realtoppanel: Integer;
  {$IFDEF DELPHI6_LVL}
  tf: string;
  {$ENDIF}

begin
  if InspectorItem.FReadOnly then
    Exit;

  realtoppanel := GetRealToppanel;

  if realtoppanel = -1 then
    Exit;

  EditRect := GetItemRect(InspectorItem.InspectorPanel, InspectorItem);

  if InspectorItem.Index < InspectorItem.InspectorPanel.TopItem then
  begin
    InspectorItem.InspectorPanel.TopItem :=
      InspectorItem.InspectorPanel.TopItem - 1;
  end;

  if (EditRect.Bottom > InspectorItem.InspectorPanel.GetPanelHeight) then
  begin
    InspectorItem.InspectorPanel.TopItem :=
      InspectorItem.InspectorPanel.TopItem + 1;
  end;

  if EditRect.Bottom > GetBarHeight then
  begin
    FScrollBar.Position := FScrollBar.Position + InspectorItem.Height;
    Scroll(self);
  end;

  if EditRect.Top <= 0 then
  begin
    FScrollBAr.Position := FScrollBar.Position - InspectorItem.Height;
    Scroll(Self);
  end;

  if FEditing then
    StopEdit(FEditItem);


  DoEditStart(InspectorItem.InspectorPanel, InspectorItem);

  if Assigned(FOnEditStart) then
    FOnEditStart(Self, InspectorItem.InspectorPanel, InspectorItem);

  FEditItem := InspectorItem;
  EditRect := InspectorItem.InspectorBar.GetItemRect(InspectorItem.InspectorPanel, InspectorItem);
  InspectorItem.FEditing := True;

  if (FShowUP or FShowDown) and (FMode = imSinglePanelActive) then
    Dec(EditRect.Right, 20);

  EditRect.Left := InspectorItem.InspectorPanel.CaptionWidth;
  InflateRect(EditRect, -2, -1);

  if PanelCaption.SideDisplay then
    EditRect.Left := EditRect.Left + PanelCaption.SideWidth;

  FMouseDown := False;

  FEditItem.EditStart;

  if InspectorItem.InspectorPanel.EditBox then
  begin
    InflateRect(EditRect, -2 - BoxOffset, -BoxOffset);
    OffsetRect(EditRect, 0, 1);
    EditRect.Bottom := EditRect.Bottom - BoxOffset - 2;
  end;

  if InspectorItem.InspectorPanel.EditBox and
    (InspectorItem.InspectorPanel.EditBorderColor <> clNone) then
  begin
    InflateRect(EditRect, -1, -1);
  end;

  FDisableExit := True;

  FEditType := itNormal;

  case InspectorItem.PropertyType of
    ptCustom:
      begin
        if Assigned(InspectorItem.EditLink) then
        begin
          EL := InspectorItem.EditLink;
          EL.Inspector := Self;

          if EL.EditStyle = esPopup then
          begin
            EL.FPopupForm := TForm.Create(Application);
            EL.FPopupForm.FormStyle := fsStayOnTop;
            pt := ClientToScreen(Point(EditRect.Left, EditRect.Bottom));
            EL.FPopupForm.OnDeactivate := EL.FormExit;
            {$IFNDEF DELPHI9_LVL}
            EL.FPopupForm.Left := pt.x;
            EL.FPopupForm.Top := pt.y;
            EL.FPopupForm.Width := EL.FPopupWidth;
            EL.FPopupForm.Height := EL.FPopupHeight;
            {$ELSE}
            EL.FPopupForm.Width := 0;
            EL.FPopupForm.Height := 0;
            {$ENDIF}

            EL.FPopupForm.BorderStyle := bsNone;
            EL.FPopupForm.Show;
            {$IFDEF DELPHI9_LVL}
            EL.FPopupForm.Left := pt.x;
            EL.FPopupForm.Top := pt.y;
            EL.FPopupForm.Width := EL.FPopupWidth;
            EL.FPopupForm.Height := EL.FPopupHeight;
            {$ENDIF}
            EL.CreateEditor(EL.FPopupForm);
          end
          else
            EL.CreateEditor(Self);

          EL.SetProperties(EditRect, InspectorItem);
          EL.StartEdit(InspectorItem);
          FEditing := True;
        end;
      end;
    ptButton:
      begin
        InspectorItem.ItemFocused := True;
        if TabStop then
          SetFocus;
      end;
    ptBoolean:
      begin
        InspectorItem.EditChange;
        InspectorItem.BoolValue := not InspectorItem.BoolValue;
        InspectorItem.Modified := True;

        CheckChange(self);
        FEditing := True;
        //UpdateEdit;
        InspectorItem.ItemFocused := ShowFocus;
        StopEdit(InspectorItem);
        InspectorItem.ItemFocused := ShowFocus;
        if TabStop then
          SetFocus;
      end;
    ptIntSpin:
      begin
        FInspectorSpin.Visible := False;
        FInspectorSpin.Parent := Self;
        FInspectorSpin.Color := FPanels[realtoppanel].EditColor;

        if InspectorItem.Modified then
          FInspectorSpin.Font.Assign(InspectorItem.InspectorPanel.ModifiedFont)
        else
          FInspectorSpin.Font.Assign(InspectorItem.InspectorPanel.Font);

        FInspectorSpin.Font.Color := InspectorItem.InspectorPanel.EditFontColor;

        {
        if InspectorItem.Modified then
          FInspectorSpin.Font.Assign(FPanels[realtoppanel].ModifiedFont)
        else
          FInspectorSpin.Font.Assign(FPanels[realtoppanel].Font);

        FInspectorSpin.Font.Color := FPanels[realtoppanel].EditFontColor;
        }

        FInspectorSpin.Top := EditRect.Top;
        FInspectorSpin.Left := EditRect.Left;
        FInspectorSpin.Width := EditRect.Right - EditRect.Left - 1;
        FInspectorSpin.Height := EditRect.Bottom - EditRect.Top;
        FInspectorSpin.Text := InspectorItem.TextValue;
        FInspectorSpin.MinValue := InspectorItem.SpinMin;
        FInspectorSpin.MaxValue := InspectorItem.SpinMax;
        FInspectorSpin.InspectorItem := InspectorItem;
        InflateRect(EditRect, 2, 2);
        FInspectorSpin.BorderStyle := bsNone;
        FInspectorSpin.OrigValue := InspectorItem.TextValue;
        FInspectorSpin.SetEditRect;
        if InspectorItem.MaxLength > 0 then
          FInspectorSpin.MaxLength := InspectorItem.MaxLength;

        FInspectorSpin.Visible := True;
        FInspectorSpin.SetFocus;
        FInspectorSpin.SelectAll;

        FEditing := True;
      end;
    ptInteger, ptFloat, ptText, ptPassword:
      begin
        FInspectorEdit.Visible := False;
        FInspectorEdit.Parent := Self;

        FInspectorEdit.MultiLine := not (InspectorItem.PropertyType = ptPassword);

        if InspectorItem.PropertyType = ptPassword then
          FInspectorEdit.PasswordChar := '*'
        else
          FInspectorEdit.PasswordChar := #0;

        FInspectorEdit.EditMask := InspectorItem.EditMask;

        //FInspectorEdit.Color := FPanels[RealTopPanel].EditColor;

        FInspectorEdit.Color := InspectorItem.InspectorPanel.EditColor;

        FInspectorEdit.Top := EditRect.Top;
        FInspectorEdit.Left := EditRect.Left;
        FInspectorEdit.Width := EditRect.Right - EditRect.Left;
        FInspectorEdit.Height := EditRect.Bottom - EditRect.Top;
        FInspectorEdit.Text := InspectorItem.TextValue;
        FInspectorEdit.InspectorItem := InspectorItem;
        InflateRect(EditRect, 2, 2);

        if InspectorItem.Modified then
          FInspectorEdit.Font.Assign(InspectorItem.InspectorPanel.ModifiedFont)
        else
          FInspectorEdit.Font.Assign(InspectorItem.InspectorPanel.Font);

        FInspectorEdit.Font.Color := InspectorItem.InspectorPanel.EditFontColor;

        {
        if InspectorItem.Modified then
          FInspectorEdit.Font.Assign(FPanels[RealTopPanel].ModifiedFont)
        else
          FInspectorEdit.Font.Assign(FPanels[RealTopPanel].Font);

        FInspectorEdit.Font.Color := FPanels[RealTopPanel].EditFontColor;
        }

        case InspectorItem.PropertyType of
          ptInteger: FInspectorEdit.InspEditType := ieInteger;
          ptFloat: FInspectorEdit.InspEditType := ieFloat;
          ptText, ptPassword: FInspectorEdit.InspEditType := ieText;
        end;

        EditRect := Rect(2, 1, FInspectorEdit.Width, FInspectorEdit.Height);
        SendMessage(FInspectorEdit.Handle, EM_SETRECT, 0, LParam(@EditRect));

        FInspectorEdit.OrigValue := InspectorItem.TextValue;

        if InspectorItem.MaxLength > 0 then
          FInspectorEdit.MaxLength := InspectorItem.MaxLength;

        FInspectorEdit.Visible := True;
        FInspectorEdit.SetFocus;
        FInspectorEdit.SelectAll;


        FEditing := True;
      end;
    ptTextButton, ptPropButton:
      begin
        FInspectorEditBtn.Visible := False;
        FInspectorEditBtn.Parent := Self;
        //FInspectorEditBtn.Color := FPanels[RealTopPanel].EditColor;
        FInspectorEditBtn.Color := InspectorItem.InspectorPanel.EditColor;
        FInspectorEditBtn.Top := EditRect.Top;
        FInspectorEditBtn.Left := EditRect.Left;
        FInspectorEditBtn.Width := EditRect.Right - EditRect.Left;
        FInspectorEditBtn.Height := EditRect.Bottom - EditRect.Top;
        FInspectorEditBtn.Text := InspectorItem.TextValue;
        FInspectorEditBtn.InspectorItem := InspectorItem;
        InflateRect(EditRect, 2, 2);

        if InspectorItem.Modified then
          FInspectorEditBtn.Font.Assign(InspectorItem.InspectorPanel.ModifiedFont)
        else
          FInspectorEditBtn.Font.Assign(InspectorItem.InspectorPanel.Font);

        FInspectorEditBtn.Font.Color := InspectorItem.InspectorPanel.EditFontColor;

        {
        if InspectorItem.Modified then
          FInspectorEditBtn.Font.Assign(FPanels[RealTopPanel].ModifiedFont)
        else
          FInspectorEditBtn.Font.Assign(FPanels[RealTopPanel].Font);

        FInspectorEditBtn.Font.Color := FPanels[RealTopPanel].EditFontColor;
        }
        FInspectorEditBtn.OrigValue := InspectorItem.TextValue;
        FInspectorEditBtn.EditorEnabled := (InspectorItem.PropertyType = ptTextButton);

        if InspectorItem.MaxLength > 0 then
          FInspectorEditBtn.MaxLength := InspectorItem.MaxLength;

        FInspectorEditBtn.Visible := True;
        FInspectorEditBtn.SetEditRect;
        FInspectorEditBtn.SetFocus;
        FInspectorEditBtn.SelectAll;
        FEditing := True;
        FEditType := itBtnEdit;
      end;
    ptFont:
      begin
        FInspectorEditBtn.Visible := False;
        FInspectorEditBtn.Parent := Self;
        //FInspectorEditBtn.Color := FPanels[RealTopPanel].EditColor;
        FInspectorEditBtn.Color := InspectorItem.InspectorPanel.EditColor;

        FInspectorEditBtn.Top := EditRect.Top;
        FInspectorEditBtn.Left := EditRect.Left;
        FInspectorEditBtn.Width := EditRect.Right - EditRect.Left;
        FInspectorEditBtn.Height := EditRect.Bottom - EditRect.Top;
        FInspectorEditBtn.Text := InspectorItem.FontValue.Name;
        FInspectorEditBtn.InspectorItem := InspectorItem;
        InflateRect(EditRect, 2, 2);

        if InspectorItem.Modified then
          FInspectorEditBtn.Font.Assign(InspectorItem.InspectorPanel.ModifiedFont)
        else
          FInspectorEditBtn.Font.Assign(InspectorItem.FontValue);

        {
        if InspectorItem.Modified then
          FInspectorEditBtn.Font.Assign(FPanels[RealTopPanel].ModifiedFont)
        else
          FInspectorEditBtn.Font.Assign(InspectorItem.FontValue);
        }

        FInspectorEditBtn.OrigValue := InspectorItem.TextValue;
        FInspectorEditBtn.EditorEnabled := False;

        if InspectorItem.MaxLength > 0 then
          FInspectorEditBtn.MaxLength := InspectorItem.MaxLength;

        FInspectorEditBtn.Visible := True;
        FInspectorEditBtn.SetEditRect;
        FInspectorEditBtn.SetFocus;
        FInspectorEditBtn.SelectAll;
        FEditing := True;
        FEditType := itBtnEdit;
      end;
    ptValues, ptValuesList:
      begin
        FInspectorCombo.Visible := False;
        FInspectorCombo.Parent := Self;
        //FInspectorCombo.Color := FPanels[RealTopPanel].EditColor;
        FInspectorCombo.Color := InspectorItem.InspectorPanel.EditColor;
        FInspectorCombo.Top := EditRect.Top - 1;
        FInspectorCombo.Left := EditRect.Left - 1;
        FInspectorCombo.Width := EditRect.Right - EditRect.Left;
        FInspectorCombo.Height := EditRect.Bottom - EditRect.Top;
        FInspectorCombo.Text := InspectorItem.TextValue;
        FInspectorCombo.InspectorItem := InspectorItem;

        sl := TStringList.Create;
        sl.Assign(InspectorItem.Values);

        GetValueList(InspectorItem, sl);

        FInspectorCombo.Items.Assign(sl);

        sl.Free;

        if InspectorItem.Modified then
          FInspectorCombo.Font.Assign(InspectorItem.InspectorPanel.ModifiedFont)
        else
          FInspectorCombo.Font.Assign(InspectorItem.InspectorPanel.Font);

        FInspectorCombo.Font.Color := InspectorItem.InspectorPanel.EditFontColor;

        {
        if InspectorItem.Modified then
          FInspectorCombo.Font.Assign(FPanels[RealTopPanel].ModifiedFont)
        else
          FInspectorCombo.Font.Assign(FPanels[RealTopPanel].Font);

        FInspectorCombo.Font.Color := FPanels[RealTopPanel].EditFontColor;
        }
        FInspectorCombo.BkColor := FPanels[RealTopPanel].EditColor;
        FInspectorCombo.OrigValue := InspectorItem.TextValue;

        if InspectorItem.PropertyType = ptValues then
          FInspectorCombo.Style := csDropDown
        else
          FInspectorCombo.Style := csDropDownList;

        if InspectorItem.PropertyType = ptValuesList then
          FInspectorCombo.ItemIndex := FInspectorCombo.Items.IndexOf(Inspectoritem.TextValue)
        else
          FInspectorCombo.Text := InspectorItem.TextValue;

        if InspectorItem.MaxLength > 0 then
          FInspectorCombo.MaxLength := InspectorItem.MaxLength;

        FInspectorCombo.Visible := True;
        FInspectorCombo.SetFocus;
        FEditing := True;
        FEditType := itCombo;
      end;
    ptColor, ptFixedColor:
      begin
        FInspectorColorCombo.Visible := False;
        FInspectorColorCombo.Parent := Self;
        //FInspectorColorCombo.Color := FPanels[realTopPanel].EditColor;
        FInspectorColorCombo.Color := InspectorItem.InspectorPanel.EditColor;
        FInspectorColorCombo.Top := EditRect.Top - 1;
        FInspectorColorCombo.Left := EditRect.Left - 1;
        FInspectorColorCombo.Width := EditRect.Right - EditRect.Left;
        FInspectorColorCombo.Height := EditRect.Bottom - EditRect.Top;
        FInspectorColorCombo.Text := InspectorItem.TextValue;
        FInspectorColorCombo.InspectorItem := InspectorItem;

        FInspectorColorCombo.Items.Clear;

        if InspectorItem.PropertyType = ptFixedColor then
          for i := 1 to 16 do
            FInspectorColorCombo.Items.Add(IntToStr(i))
        else
          for i := 1 to 42 do
            FInspectorColorCombo.Items.Add(IntToStr(i));

        if InspectorItem.Modified then
          FInspectorColorCombo.Font.Assign(InspectorItem.InspectorPanel.ModifiedFont)
        else
          FInspectorColorCombo.Font.Assign(InspectorItem.InspectorPanel.Font);

        FInspectorColorCombo.Font.Color := InspectorItem.InspectorPanel.EditFontColor;

        FInspectorColorCombo.BkColor := InspectorItem.InspectorPanel.EditColor;

        {
        if InspectorItem.Modified then
          FInspectorColorCombo.Font.Assign(FPanels[RealTopPanel].ModifiedFont)
        else
          FInspectorColorCombo.Font.Assign(FPanels[RealTopPanel].Font);

        FInspectorColorCombo.Font.Color := FPanels[RealTopPanel].EditFontColor;

        FInspectorColorCombo.BkColor := FPanels[RealTopPanel].EditColor;
        }


        FInspectorColorCombo.OrigValue := InspectorItem.ColorValue;

        if InspectorItem.MaxLength > 0 then
          FInspectorColorCombo.MaxLength := InspectorItem.MaxLength;

        FInspectorColorCombo.Visible := True;
        FInspectorColorCombo.ColorValue := InspectorItem.ColorValue;
        FInspectorColorCombo.SetFocus;
        FEditing := True;
        FEditType := itColorCombo;
      end;
    ptDate, ptTime:
      begin
        FInspectorDateTimePicker.Visible := False;
        FInspectorDateTimePicker.Parent := Self;
  //ShowMessage('realtoppanel ' + inttostr(realtoppanel));
//      FInspectorDateTimePicker.Color := FPanels[RealTopPanel].EditColor;
        FInspectorDateTimePicker.Top := EditRect.Top;
        FInspectorDateTimePicker.Left := EditRect.Left;
        FInspectorDateTimePicker.Width := EditRect.Right - EditRect.Left;
        FInspectorDateTimePicker.Height := EditRect.Bottom - EditRect.Top;

        if (InspectorItem.DateValue = 0) and (InspectorItem.TextValue <> '') then
        begin
          try
            if pos(DateSeparator,InspectorItem.TextValue) > 0  then
              InspectorItem.DateTimeValue := StrToDate(InspectorItem.TextValue)
            else
              if pos(TimeSeparator,InspectorItem.TextValue) > 0  then
                InspectorItem.DateTimeValue := StrToTime(InspectorItem.TextValue);

            InspectorItem.DateValue := InspectorItem.DateTimeValue;
            InspectorItem.TimeValue := InspectorItem.DateTimeValue;
          except
          end;
        end;

        if InspectorItem.PropertyType = ptDate then
        begin
          FInspectorDateTimePicker.Kind := dtkDate;
          FInspectorDateTimePicker.Date := InspectorItem.DateValue;
{$IFDEF DELPHI6_LVL}
          FInspectorDateTimePicker.Format := InspectorItem.DateTimeFormat;
{$ENDIF}
        end
        else
        begin
          FInspectorDateTimePicker.Kind := dtkTime;
          FInspectorDateTimePicker.Time := InspectorItem.TimeValue;
{$IFDEF DELPHI6_LVL}
          tf := StringReplace(InspectorItem.DateTimeFormat,'n','m',[rfReplaceAll]);
          tf := StringReplace(tf,'N','M',[rfReplaceAll]);
          FInspectorDateTimePicker.Format := tf;
{$ENDIF}
        end;

        FInspectorDateTimePicker.InspectorItem := InspectorItem;

        if InspectorItem.Modified then
          FInspectorDateTimePicker.Font.Assign(InspectorItem.InspectorPanel.ModifiedFont)
        else
          FInspectorDateTimePicker.Font.Assign(InspectorItem.InspectorPanel.Font);

        FInspectorDateTimePicker.Font.Color := InspectorItem.InspectorPanel.EditFontColor;

        {
        if InspectorItem.Modified then
          FInspectorDateTimePicker.Font.Assign(FPanels[RealTopPanel].ModifiedFont)
        else
          FInspectorDateTimePicker.Font.Assign(FPanels[RealTopPanel].Font);

        FInspectorDateTimePicker.Font.Color := FPanels[RealTopPanel].EditFontColor;
        }
        FInspectorDateTimePicker.OrigValue := InspectorItem.DateValue;
        FInspectorDateTimePicker.Visible := True;
        FInspectorDateTimePicker.SetFocus;
        FEditing := True;
        FEditType := itPicker;
      end;
  end;

  FDisableExit := False;

  DoEditProp(InspectorItem.InspectorPanel, InspectorItem);
end;

procedure TInspectorBar.UpdateEdit;
var
  I, E: Integer;
begin
  case FEditItem.PropertyType of
    ptIntSpin:
      begin
        FInspectorSpin.InspectorItem.Modified := FInspectorSpin.InspectorItem.Modified or
          (FInspectorSpin.Text <> FInspectorSpin.OrigValue);
        FInspectorSpin.InspectorItem.IntValue := FInspectorSpin.Value;
        FInspectorSpin.SelectAll;
        FInspectorSpin.OrigValue := FInspectorSpin.Text;
      end;
    ptInteger:
      begin
        FInspectorEdit.InspectorItem.Modified := FInspectorEdit.InspectorItem.Modified or
          (FInspectorEdit.Text <> FInspectorEdit.OrigValue);
        val(FInspectorEdit.Text, I, E);
        if E = 0 then
          FInspectorEdit.InspectorItem.IntValue := I;
        FInspectorEdit.SelectAll;
        FInspectorEdit.OrigValue := FInspectorEdit.Text;
      end;
    ptFloat, ptText, ptPassword:
      begin
        FInspectorEdit.InspectorItem.Modified := FInspectorEdit.InspectorItem.Modified or
          (FInspectorEdit.Text <> FInspectorEdit.OrigValue);
        FInspectorEdit.InspectorItem.TextValue := FInspectorEdit.Text;
        FInspectorEdit.SelectAll;
        FInspectorEdit.OrigValue := FInspectorEdit.Text;
      end;
    ptValues, ptValuesList:
      begin
        FInspectorCombo.InspectorItem.Modified := FInspectorCombo.InspectorItem.Modified or
          (FInspectorCombo.Text <> FInspectorCombo.OrigValue);
        FInspectorCombo.InspectorItem.TextValue := FInspectorCombo.Text;
        FInspectorCombo.SelectAll;
        FInspectorCombo.OrigValue := FInspectorCombo.Text;
      end;
    ptTextButton:
      begin
        FInspectorEditBtn.InspectorItem.Modified := FInspectorEditBtn.InspectorItem.Modified or
          (FInspectorEditBtn.Text <> FInspectorEditBtn.OrigValue);
        FInspectorEditBtn.InspectorItem.TextValue := FInspectorEditBtn.Text;
        FInspectorEditBtn.SelectAll;
        FInspectorEditBtn.OrigValue := FInspectorEditBtn.Text;
      end;
    ptDate:
      begin
        FInspectorDateTimePicker.InspectorItem.Modified := FInspectorDateTimePicker.InspectorItem.Modified or
          (Int(FInspectorDateTimePicker.Date) <> Int(FInspectorDateTimePicker.OrigValue));
        FInspectorDateTimePicker.InspectorItem.DateValue := FInspectorDateTimePicker.Date;
        FInspectorDateTimePicker.OrigValue := FInspectorDateTimePicker.Date;
      end;
    ptTime:
      begin
        FInspectorDateTimePicker.InspectorItem.Modified := FInspectorDateTimePicker.InspectorItem.Modified or
          (Frac(FInspectorDateTimePicker.Time) <> Frac(FInspectorDateTimePicker.OrigValue));
        FInspectorDateTimePicker.InspectorItem.TimeValue := FInspectorDateTimePicker.Time;
        FInspectorDateTimePicker.OrigValue := FInspectorDateTimePicker.Time;
      end;
    ptColor, ptFixedColor:
      begin
        FInspectorColorCombo.InspectorItem.Modified := FInspectorColorCombo.InspectorItem.Modified or
          (FInspectorColorCombo.ColorValue <> FInspectorColorCombo.OrigValue);
        FInspectorColorCombo.InspectorItem.ColorValue := FInspectorColorCombo.ColorValue;
        FInspectorColorCombo.OrigValue := FInspectorColorCombo.ColorValue;
      end;
    ptFont:
      begin
        FEditItem.Modified := True;
      end;
  end;

  if Assigned(FOnEditUpdate) then
    FOnEditUpdate(Self, FEditItem.InspectorPanel, FEditItem);
end;

procedure TInspectorBar.StopEdit(InspectorItem: TInspectorItem);
begin
  if not FEditing then
    Exit;

  if (csDestroying in ComponentState) then
    Exit;

  UpdateEdit;

  InspectorItem.FEditing := False;

  FEditing := False;

  case InspectorItem.PropertyType of
    ptCustom:
      begin
        InspectorItem.EditLink.StopEdit(InspectorItem);
      end;
    ptIntSpin:
      FInspectorSpin.Hide;
    ptInteger, ptFloat, ptText, ptPassword:
      FInspectorEdit.Hide;
    ptValues, ptValuesList:
      FInspectorCombo.Hide;
    ptTextButton, ptFont, ptPropButton:
      FInspectorEditBtn.Hide;
    ptDate, ptTime:
      FInspectorDateTimePicker.Hide;
    ptColor, ptFixedColor:
      FInspectorColorCombo.Hide;
    ptButton:
      InspectorItem.ItemFocused := False;
    ptBoolean:
      InspectorItem.ItemFocused := False;
  end;

  if Assigned(FOnEditStop) then
    FOnEditStop(Self, InspectorItem.InspectorPanel, InspectorItem);

  InspectorItem.EditStop;
end;

procedure TInspectorBar.Notification(AComponent: TComponent;
  AOperation: TOperation);
var
  i,j: Integer;
begin
  inherited;
  if (AOperation = opRemove) and (AComponent = FImages) then
  begin
    FImages := nil;
    Invalidate;
  end;

  if (AOperation = opRemove) and Assigned(FPanels) then
    for i := 1 to Panels.Count do
    begin
      if Panels[i - 1].Control = AComponent then
        Panels[i - 1].Control := nil;

      if Panels[i - 1].PopupMenu = AComponent then
        Panels[i - 1].PopupMenu := nil;

      for j := 0 to Panels[i - 1].Items.Count - 1 do
      begin
        if Panels[i - 1].Items[j].EditLink = AComponent then
          Panels[i - 1].Items[j].EditLink := nil;
      end;
    end;
end;

procedure TInspectorBar.CMHintShow(var Msg: TMessage);
var
  pt: TPoint;
begin
  GetCursorPos(pt);
  pt := ScreenToClient(pt);
  if MSg.LParam <> 0 then //Set the Hint String if available
    THintInfo(ptr(MSg.LParam)^).HintStr := GetCursorHint(pt); // exactly like in VCL code
end;

function TInspectorBar.GetAlignEx: TAlign;
begin
  Result := inherited Align;
end;

procedure TInspectorBar.SetAlignEx(const Value: TAlign);
begin
  inherited Align := Value;
end;

procedure TInspectorBar.KeyDown(var Key: Word; Shift: TShiftState);
var
  realtoppanel: integer;
begin
  inherited;

  if FoundShortcut(key, Shift) then
  begin
    Invalidate;
    Exit;
  end;


  if (Key = VK_PRIOR) and (FShowUp) then
  begin
    realtoppanel := getRealTopPanel;

    if realtoppanel >= 0 then
    begin
      Panels[realtopPanel].TopItem := Panels[realtopPanel].TopItem - 1;
      Scroll(self);
    end;
  end;

  if (Key = VK_PRIOR) and not (FShowUp) then
  begin
    if TopPanel > 0 then
    begin
      PanelClosed(Panels[TopPanel]);
      PanelOpened(Panels[TopPanel - 1]);
      TopPanel := TopPanel - 1;
      AfterPanelOpened(Panels[TopPanel]);
    end;
  end;

  if (Key = VK_HOME) and not (FShowUp) then
  begin
    TopPanel := 0;
  end;

  if (Key = VK_END) and not (FShowDown) then
  begin
    TopPanel := Panels.Count - 1;
  end;


  if (Key = VK_NEXT) and (FShowDown) then
  begin
    realtoppanel := getRealTopPanel;

    if (realtoppanel >= 0) then
    begin
      Panels[realtopPanel].TopItem := Panels[realtopPanel].TopItem + 1;
      Scroll(self);
    end;
  end;

  if (Key = VK_NEXT) and not (FShowDown) then
  begin
    if TopPanel < Panels.Count - 1 then
    begin
      PanelClosed(Panels[TopPanel]);
      PanelOpened(Panels[TopPanel + 1]);

      TopPanel := TopPanel + 1;
      AfterPanelOpened(Panels[TopPanel]);
    end;
  end;

  if (ssAlt in Shift) then SearchShortCutCaption(key);

  if Assigned(FEditItem) then
  begin
    if FEditItem.PropertyType = ptBoolean then
    begin
      if Key = VK_SPACE then
      begin
        if Assigned(OnEditStart) then
          OnEditStart(Self, FEditItem.InspectorPanel, FEditItem);
        //StartEdit(FEditItem);
        FEditItem.BoolValue := not FEditItem.BoolValue;

        CheckChange(self);
    // Force update of editor
        //StopEdit(FEditItem);

        if Assigned(OnEditStop) then
          OnEditStop(Self, FEditItem.InspectorPanel, FEditItem);
      end;

      if Key in [VK_UP, VK_DOWN, VK_RETURN] then
      begin
        FEditItem.ItemFocused := False;
        NextEdit(FEditItem, Key);
      end;
    end;

    if FEditItem.PropertyType = ptButton then
    begin
      if Key = VK_SPACE then
      begin
        if Assigned(FOnButtonClick) then
          FOnButtonClick(Self, FEditItem.InspectorPanel, EditItem);
      end;

      if Key in [VK_UP, VK_DOWN, VK_RETURN] then
      begin
        FEditItem.ItemFocused := False;
        NextEdit(FEditItem, Key);
      end;
    end;

  end;

end;

procedure TInspectorBar.SearchShortcutCaption(key: word);
var
  i, j: Integer;
  ch: Char;
  ss: string;
  realtopPanel: Integer;
begin
  realtoppanel := Getrealtoppanel;
  if realtoppanel = -1 then exit;

  ch := UpCase(chr(key));

//----- Open or close Panels----
// Dual MODE -----
  for i := 1 to FPanels.Count do
  begin
//-- Open / Close Panel by shortcut
    ss := UpperCase(FPanels.Items[i - 1].FCaption);
    if (AnsiPos('&' + ch, ss) <> 0) then
    begin
      if Fmode = imMultiPanelActive then
      begin
        if (i - 1 = FLastOpen) and (i - 1 = FFirstOpen) then Break;
        if FPanels.Items[i - 1].FItems.Count = 0 then
          Break;
        FPanels.Items[i - 1].FIsOpen := not (FPanels.Items[i - 1].FIsOpen); //open<->close
        if (FPanels.Items[i - 1].FIsOpen) then
          PanelOpened(FPanels.Items[i - 1])
        else
          PanelClosed(FPanels.Items[i - 1]);
        ArrangeOpenClose;

        if (FPanels.Items[i - 1]).FIsOpen then
          AfterPanelOpened(FPanels.Items[i - 1]);
        Break;
      end
      else
      begin
        if (RealTopPanel <> i - 1) then
        begin
          PanelOpened(FPanels.Items[i - 1]);
          PanelClosed(FPanels.Items[RealTopPanel]);
          TopPanel := i - 1;
          AfterPanelOpened(FPanels.Items[i - 1]);
        end;
        Break;
      end;
    end;

//-------- Items--------
    if not FPanels.Items[i - 1].FIsOpen then
      Continue;
    j := 0;
    while j < FPanels.Items[i - 1].FItems.Count do
    begin
      ss := FPanels.Items[i - 1].FItems.Items[j].FCaption;
      if FPanels.Items[i - 1].FItems.Items[j].FVisible then
        if (AnsiPos('&' + ch, ss) <> 0) then
        begin
          ItemClicked(mbLeft, FPanels.Items[i - 1], FPanels.Items[i - 1].FItems.Items[j]);
          Break;
        end;
      inc(j);
    end;

    if j < FPanels.Items[i - 1].FItems.Count - 1 then
      Break;
  end;
  invalidate;
end;

function TInspectorBar.GetHintSingle(x, y: Integer): string;
var
  i, j, k, q: Integer;
  tempheight, realTopPanel: integer;
  tempPanel: TInspectorPanel;
begin
  Result := '';
  realTopPanel := getRealTopPanel;
  if realTopPanel = -1 then exit;

  for i := 0 to FPanels.Count - 1 do
  begin
    if not FPanels.Items[i].Visible then continue;
    if i <= realTopPanel then
      j := (getNumbervisiblePanelUp(i)) * PanelCaption.Height
    else
      j := Height - ((getNumbervisiblePanelDown(i) + 1) * PanelCaption.Height);


    if (y >= j) and (y <= j + PanelCaption.Height) then
    begin
      if not HintCaptionOptions(FPanels.Items[i], x, y, Result) then
        Result := FPanels.Items[i].FHint;
      Break;
    end;
  end;

  j := (getNumbervisiblePanelUp(realTopPanel) + 1) * PanelCaption.Height; //Click Up
  i := Height - ((getNumbervisiblePanelDown(realTopPanel)) * PanelCaption.Height); //Click Down

  if (y > j) and (y < i) and (x > 0) and (x < Width) then
  begin
    tempPanel := FPanels[realTopPanel];
    tempheight := j;
    k := tempPanel.Items.Count;
    for q := tempPanel.TopItem to tempPanel.Items.Count do
    begin
      k := q;
      if k >= tempPanel.Items.Count then
        Break;
      if not tempPanel.Items[q].Visible then
        Continue;
      if tempPanel.VariableItemHeight then
        Inc(tempheight, tempPanel.Items[q].Height)
      else
        Inc(tempheight, tempPanel.ItemHeight);
      if tempheight > y then
        Break;
    end;
    if k < tempPanel.Items.Count then
      Result := tempPanel.Items[k].FHint;
  end;
end;

//------------   Cursor   -------------
function TInspectorBar.GetCursorSingle(x, y: integer): boolean;
var
  RealTopPanel, ys, yj: integer;
begin
  RealTopPanel := getRealTopPanel;
  if RealTopPanel = -1 then
  begin
    result := false;
    exit;
  end;
  ys := getNumbervisiblePanelUp(RealTopPanel);
  yj := getNumbervisiblePanelDown(RealTopPanel);
  Result := (y < (ys + 1) * PanelCaption.Height)
    or (y > Height - (yj) * PanelCaption.Height);
end;

function TInspectorBar.GetCursorMulti(x, y: integer): boolean;
var
  i, j, m: Integer;
  tempHeight: integer;
begin
  Result := False;
  if FscrollBar.Visible then j := -FscrollBar.Position
  else j := 0;
  for i := 0 to FPanels.Count - 1 do
  begin
    if not FPanels.Items[i].Visible then continue;
    if (y >= j) and (y <= j + PanelCaption.Height) then
    begin
      Result := True;
      Exit;
    end;
    if FPanels.Items[i].FIsOpen then //for open panel
    begin
      Inc(j, PanelCaption.Height); //Panel caption
  //Items
      for m := 1 to FPanels.Items[i].Items.Count do
      begin
        if not FPanels.Items[i].Items[m - 1].FVisible then
          Continue;
        if FPanels.Items[i].VariableItemHeight then
          tempHeight := FPanels.Items[i].Items[m - 1].Height
        else
          tempHeight := FPanels.Items[i].ItemHeight;

        if (y >= j) and (y <= j + tempHeight) then
          Exit;
        inc(j, tempHeight);
      end;

      if i = FLastOpen then
        j := Height - (FPanels.Count - FLastOpen - 1) * PanelCaption.Height;
    end
    else inc(j, PanelCaption.Height); //for close panel
  end;
end;

function TInspectorBar.GetHintMulti(x, y: integer): string;
var
  i, j, k: Integer;
  tempHeight: Integer;
begin
  Result := '';
  if FscrollBar.visible then j := -FscrollBar.Position
  else j := 0;
  for i := 0 to FPanels.Count - 1 do
  begin
    if not FPanels.Items[i].Visible then continue;
    if (y >= j) and (y <= j + PanelCaption.Height) then
    begin
      if not HintCaptionOptions(FPanels.Items[i], x, y, Result) then
        Result := FPanels.Items[i].FHint;
      Break;
    end;
// compute absolutete coordinates from local coordinates
    if FPanels.Items[i].FIsOpen then
    begin
  //for open panel
      inc(j, PanelCaption.Height); //Panel caption
  //Items
      for k := 1 to FPanels.Items[i].Items.Count do
      begin
        if not FPanels.Items[i].Items[k - 1].FVisible then
          Continue;
        if FPanels.Items[i].VariableItemHeight then
          tempHeight := FPanels.Items[i].Items[k - 1].Height
        else
          tempHeight := FPanels.Items[i].ItemHeight;

        if (y >= j) and (y <= j + tempHeight) then
        begin
          Result := FPanels.Items[i].Items[k - 1].FHint;
          Exit;
        end;
        inc(j, tempHeight);
      end;

      if i = FLastOpen then
      begin
        j := Height - (FPanels.Count - FLastOpen - 1) * PanelCaption.Height;
      end;
    end
    else
    begin
  //for close panel
      inc(j, PanelCaption.Height);
    end
  end;
end;

function TInspectorBar.GetHint(x, y: integer): string;
begin
  if FMode = imSinglePanelActive then
    Result := GetHintSingle(x, y)
  else
    Result := GetHintMulti(x, y);
end;

function TInspectorBar.GetCursorHint(P: TPoint): string;
var
  s: string;
begin
  s := GetHint(p.x, p.y);
  if s <> '' then
    Result := s
  else
    Result := Hint;
end;

procedure TInspectorBar.PanelClosed(Panel: TInspectorPanel);
begin
  if Assigned(FOnPanelClose) then
    FOnPanelClose(Self, Panel);
end;

procedure TInspectorBar.PanelOpened(Panel: TInspectorPanel);
begin
  if Assigned(FOnPanelOpen) then
    FOnPanelOpen(Self, Panel);
end;

procedure TInspectorBar.PanelCaptionClick(Panel: TInspectorPanel);
begin
  if Assigned(FOnPanelCaptionClick) then
    FOnPanelCaptionClick(Self, Panel);
end;

procedure TInspectorBar.PanelCaptionRightClick(Panel: TInspectorPanel);
begin
  if Assigned(FOnPanelCaptionRightClick) then
    FOnPanelCaptionRightClick(Self, Panel);
end;

procedure TInspectorBar.NextEdit(InspectorItem: TInspectorItem;
  NextKey: Word);
var
  NxtItem: TInspectorItem;
  Idx: Integer;
begin
  if not AutoAdvance and (NextKey = VK_RETURN) then
  begin
    UpdateEdit;
    Exit;
  end;

  FDisableExit := True;

  if NextKey in [VK_RETURN, VK_DOWN] then
  begin
    if InspectorItem.Index + 1 < InspectorItem.Collection.Count then
    begin
      StopEdit(FEditItem);

      Idx := InspectorItem.Index;

      NxtItem := TInspectorItem(InspectorItem.Collection.Items[Idx + 1]);

      while (not NxtItem.Visible or NxtItem.ReadOnly) and (Idx + 1 < InspectorItem.Collection.Count) do
      begin
        NxtItem := TInspectorItem(InspectorItem.Collection.Items[Idx + 1]);
        Inc(Idx);
      end;

      if Assigned(NxtItem) then
        if NxtItem.PropertyType = ptBoolean then
          NxtItem.BoolValue := not NxtItem.BoolValue;

      if NxtItem.Visible then
        StartEdit(NxtItem)
      else
        StartEdit(InspectorItem);
    end;
  end
  else
  begin
    if InspectorItem.Index > 0 then
    begin
      StopEdit(FEditItem);

      Idx := InspectorItem.Index;

      NxtItem := TInspectorItem(InspectorItem.Collection.Items[InspectorItem.Index - 1]);

      while (not NxtItem.Visible or NxtItem.ReadOnly) and (Idx > 0) do
      begin
        NxtItem := TInspectorItem(InspectorItem.Collection.Items[Idx - 1]);
        Dec(Idx);
      end;

      if Assigned(NxtItem) then
        if NxtItem.PropertyType = ptBoolean then
          NxtItem.BoolValue := not NxtItem.BoolValue;

      if NxtItem.Visible then
        StartEdit(NxtItem)
      else
        StartEdit(InspectorItem);  
    end;
  end;

  FDisableExit := False;
end;

procedure TInspectorBar.EditDblClick(Sender: TObject);
var
  ip: TInspectorItem;
begin
  if Assigned(FOnEditDblClick) then
  begin
    if Assigned(FEditItem) then
    begin
      ip := FEditItem;
      StopEdit(ip);
      FOnEditDblClick(Sender, ip.InspectorPanel, ip);
      StartEdit(ip);
    end;
  end;
end;

procedure TInspectorBar.EditBtnClick(Sender: TObject);
var
  FD: TFontDialog;
  AItem: TInspectorItem;
begin
  FMouseDown := False;

  if FEditItem.PropertyType = ptFont then
  begin
    FD := TFontDialog.Create(Self);
    FD.Font.Assign(FEditItem.FontValue);
    if FD.Execute then
    begin
      FEditItem.FontValue.Assign(FD.Font);
      FInspectorEditBtn.Text := FD.Font.Name;
    end;
    StopEdit(FEditItem);
    Exit;
  end;

  if Assigned(FOnEditBtnClick) then
  begin
    AItem := FEditItem;
    StopEdit(FEditItem);
    FOnEditBtnClick(Sender, AItem.InspectorPanel, AItem);
    StartEdit(AItem);
  end;
end;

function TInspectorBar.GetCaption(x, y: Integer): TInspectorPanel;
var
  i, j: Integer;
  tempHeight, k: integer;
  realTopPanel: Integer;
begin
  Result := nil;
  if fMode = imSinglePanelActive then
  begin
    realTopPanel := getRealTopPanel;
    if realTopPanel = -1 then exit;
    for i := 0 to FPanels.Count - 1 do
    begin
      if not FPanels[i].Visible then continue;
      if i <= realtoppanel then
        j := getNumbervisiblePanelUp(i) * PanelCaption.Height
      else
        j := Height - ((getNumbervisiblePanelDown(i) + 1) * PanelCaption.Height);
      if (y >= j) and (y <= j + PanelCaption.Height) then
      begin
        Result := FPanels.Items[i];
        Break;
      end;
    end;
  end
  else
  begin
    if FScrollBar.Visible then j := -FscrollBar.Position
    else j := 0;

    for i := 0 to FPanels.Count - 1 do
    begin
      if not FPanels[i].Visible then continue;
      if (y >= j) and (y <= j + PanelCaption.Height) then
      begin
        result := FPanels.Items[i];
        exit;
      end;
      if FPanels.Items[i].FIsOpen then
      begin
        inc(j, PanelCaption.Height);
        tempHeight := 0;
        for k := 0 to FPanels[i].Items.Count - 1 do
          if FPanels[i].Items[k].Visible then
            if FPanels[i].VariableItemHeight then
              Inc(tempHeight, FPanels[i].Items[k].Height)
            else
              Inc(tempHeight, FPanels[i].ItemHeight);

        if (y >= j) and (y <= j + tempHeight) then
          Exit;
        inc(j, tempHeight);
        if i = FLastOpen then
          j := Height - (FPanels.Count - FLastOpen - 1) * PanelCaption.Height;
      end
      else
        inc(j, PanelCaption.Height);
    end;
  end;
end;

function TInspectorBar.GetPanel(x, y: Integer): TInspectorPanel;
var
  i, j, k: Integer;
  tempHeight, realTopPanel: Integer;
begin
  Result := nil;
  if FMode = imSinglePanelActive then
  begin
    realTopPanel := getRealTopPanel;
    if realTopPanel = -1 then exit;
    Result := FPanels[realTopPanel];
  end
  else
  begin
    if FScrollBar.Visible then j := -FscrollBar.Position
    else j := 0;
    for i := 0 to FPanels.Count - 1 do
    begin
      if not FPanels[i].Visible then continue;
      if FPanels.Items[i].FIsOpen then
      begin
  //for open panel
        Inc(j, PanelCaption.Height); //Panel caption
  //Items
        for k := 1 to FPanels[i].Items.Count do
        begin
          if not FPanels[i].Items[k - 1].Visible then
            Continue;

          if FPanels[i].VariableItemHeight then
            tempHeight := Panels[i].Items[k - 1].Height
          else
            tempHeight := Panels[i].ItemHeight;

          if (y > j) and (y < j + tempHeight) then
          begin
            Result := FPanels.Items[i];
            Exit;
          end;
          Inc(j, tempHeight);
        end;
        if i = FLastOpen then
        begin
          j := Height - (FPanels.Count - FLastOpen - 1) * PanelCaption.Height;
        end;
      end
      else
      begin
  //for closed panel
        inc(j, PanelCaption.Height);
      end
    end;
  end;
end;


function TInspectorBar.GetItem(x, y: Integer): TInspectorItem;
var
  i, j, k: Integer;
  tempHeight, m: Integer;
  scrlwidth, realTopPanel: Integer;
begin
  Result := nil;
  scrlwidth := 0;

  if FShowUp or FShowDown then
    scrlwidth := 20;

  if FMode = imSinglePanelActive then
  begin
    RealTopPanel := getRealTopPanel;
    if RealTopPanel = -1 then exit;
    j := (getNumbervisiblePanelUp(RealTopPanel) + 1) * PanelCaption.Height;
    i := Height - (getNumbervisiblePanelDown(RealTopPanel)) * PanelCaption.Height;
    if (y > j) and (y < i) and (x > 0) and (x < Width - scrlwidth) then
    begin
      k := FPanels[RealTopPanel].Items.Count;
      tempHeight := j;
      for m := FPanels[RealTopPanel].TopItem to FPanels[RealTopPanel].Items.Count do
      begin
        k := m;
        if k >= FPanels[RealTopPanel].Items.Count then
          Break;
        if not FPanels[RealTopPanel].Items[m].Visible then
          Continue;

        if FPanels[RealTopPanel].VariableItemHeight then
          Inc(tempHeight, FPanels[RealTopPanel].Items[m].Height)
        else
          Inc(tempHeight, FPanels[RealTopPanel].ItemHeight);

        if tempHeight > y then
          Break;
      end;
      if k < FPanels[RealTopPanel].Items.Count then
        Result := FPanels[RealTopPanel].Items[k];
    end;
  end
  else
  begin
//Multi Panel
    if FScrollBar.Visible then j := -FscrollBar.Position
    else j := 0;
    for i := 0 to FPanels.Count - 1 do
    begin
      if not FPanels[i].Visible then continue;
      if FPanels[i].FIsOpen then
      begin
  //for open panel
        Inc(j, PanelCaption.Height); //Panel caption
  //Items
        for k := 1 to FPanels[i].Items.Count do
        begin
          if not FPanels[i].Items[k - 1].Visible then
            Continue;

          if FPanels[i].VariableItemHeight then
            tempHeight := Panels[i].Items[k - 1].Height
          else
            tempHeight := Panels[i].ItemHeight;

          if (y > j) and (y < j + tempHeight) and (x > 0) and (x < Width - scrlwidth) then
          begin
            Result := FPanels[i].Items[k - 1];
            Exit;
          end;

          Inc(j, tempHeight);
        end;

        if i = FLastOpen then
        begin
          j := Height - (FPanels.Count - FLastOpen - 1) * PanelCaption.Height;
        end;
      end
      else
      begin
  //for close panel
        inc(j, PanelCaption.Height);
      end
    end;
  end;
end;

function TInspectorBar.GetPanelItemAtXY(x, y: Integer;
  var Panel: TInspectorPanel; var Item: TInspectorItem): Boolean;
begin
  Result := False;

  if FPanels.Count = 0 then
    Exit;

  Item := GetItem(x, y);
  if Assigned(Item) then
    Panel := Item.InspectorPanel
  else
    Panel := nil;

  Result := Assigned(Item);
end;

function TInspectorBar.GetParentForm(Control: TControl): TCustomForm;
begin
  Result := nil;
  if Assigned(Control) then
  begin
//    if (Control is TCustomForm) then
//    begin
//      Result := Control as TCustomForm;
//      Exit;
//    end else
    begin
      if Assigned(Control.Parent) then
      begin
        Result := GetParentForm(Control.Parent)
      end
      else
      begin
        if (Control is TCustomForm) then
          Result := Control as TCustomForm;
      end;
    end;
  end;
end;

function TInspectorBar.IsPanelItemAtXY(x, y: Integer): Boolean;
var
  Item: TInspectorItem;
begin
  Item := GetItem(x, y);
  Result := Assigned(Item);
end;

function TInspectorBar.GetPanelAtXY(x, y: Integer; var Panel: TInspectorPanel): Boolean;
begin
  Panel := GetPanel(x, y);
  Result := Assigned(Panel);
end;

function TInspectorBar.GetCaptionRect(Panel: TInspectorPanel): TRect;
var
  i, j: Integer;
  tempHeight, k, RealtopPanel: integer;

begin
  if Mode = imSinglePanelActive then
  begin
    RealTopPanel := GetRealTopPAnel;
    if Panel.Index <= RealTopPanel then
      j := (getNumbervisiblePanelUp(Panel.Index)) * PanelCaption.Height
    else
      j := Height - ((getNumbervisiblePanelDown(Panel.Index) + 1) * PanelCaption.Height);
    Result := Rect(0, j, Width, j + PanelCaption.Height);
  end
  else
  begin
    if FScrollBar.Visible then j := -FscrollBar.Position
    else j := 0;

    for i := 0 to FPanels.Count - 1 do
    begin
      if not FPanels[i].Visible then continue;
      if FPanels[i] = Panel then
      begin
        Result := Rect(0, j, Width, j + PanelCaption.Height);
      end
      else
      begin
        Inc(j, PanelCaption.Height);
        if FPanels.Items[i].FIsOpen then
        begin
          tempHeight := 0;
          for k := 0 to FPanels[i].Items.Count - 1 do
            if FPanels[i].Items[k].Visible then
              if FPanels[i].VariableItemHeight then
                Inc(tempHeight, FPanels[i].Items[k].Height)
              else
                Inc(tempHeight, FPanels[i].ItemHeight);
          Inc(j, tempHeight);
        end
      end;
    end;
  end;
end;

procedure TInspectorBar.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  FMouseItem := nil;
end;

procedure TInspectorBar.CMMouseLeave(var Message: TMessage);
var
  r: TRect;
  i: Integer;
begin
  inherited;

  if GetParentForm(self).Handle <> GetActiveWindow then
    Exit;

  if FMousePanel <> -1 then
  begin
    r := GetCaptionRect(Panels[FMousePanel]);
    {$IFDEF VER185}
    Invalidate;
    {$ELSE}

    InvalidateRect(Handle, @r, False);
    {$ENDIF}
    FMousePanel := -1;
  end;

  if Assigned(FMouseItem) then
  begin
    r := GetItemRect(FMouseItem.InspectorPanel, FMouseItem);
    {$IFDEF VER185}
    Invalidate;
    {$ELSE}
    InvalidateRect(Handle, @r, False);
    {$ENDIF}
  end;

  for i := 1 to Panels.Count do
  begin
    if Panels[i - 1].HoverFullWidth then
    begin
      Invalidate;
      Break;
    end;
  end;

// always restore cursor on mouseleave
  Cursor := FDefCursor;
end;

procedure TInspectorBar.EditChanged(Sender: TObject);
begin
  FEditItem.EditChange;
end;

procedure TInspectorBar.GetValueList(InspectorItem: TInspectorItem;
  Values: TStringList);
begin
  if Assigned(FOnGetValueList) then
    FOnGetValueList(Self, InspectorItem, Values);
end;

procedure TInspectorBar.SetAcceptFiles(const Value: Boolean);
begin
  FAcceptFiles := Value;
  DragAcceptFiles(Self.Handle, Value);
end;

procedure TInspectorBar.WMDropFiles(var Message: TMessage);
var
  Pos: TPoint;
  Files: TStringList;
  FileCount, Len, i, realTopPanel: Integer;
  FileName: array[0..255] of Char;
  DefaultHandler: Boolean;
begin
  DragQueryPoint(Message.wParam, Pos);
  realTopPanel := GetrealTopPanel;
  if realTopPanel = -1 then exit;
  Files := TStringList.Create;
  try
    FileCount := DragQueryFile(Message.wParam, UINT(-1), nil, 0);
    for i := 0 to FileCount - 1 do
    begin
      Len := DragQueryFile(Message.wParam, I, FileName, 255);
      if Len > 0 then
      begin
        Files.Add(StrPas(FileName));

        DefaultHandler := True;
        if Assigned(FOnFileDrop) then
          FOnFileDrop(Self, StrPas(FileName), DefaultHandler);
        if DefaultHandler then
        begin
          with Panels[realTopPanel].Items.Add do
          begin
            URL := StrPas(FileName);
            AutoIcon := True;
            Caption := URL;
          end;
        end;
      end;
    end;
  finally
    Files.Free;
  end;
end;

procedure TInspectorBar.ComboChanged(Sender: TObject);
begin
  FEditItem.EditChange;

  if Assigned(FOnEditComboChange) and Assigned(FEditItem) then
  begin
    FOnEditComboChange(Self, FEditItem.InspectorPanel, FEditItem,
      FInspectorCombo.ItemIndex);
  end;
end;

procedure TInspectorBar.CheckChange(Sender: TObject);
begin
  if Assigned(FOnEditCheckChange) and Assigned(FEditItem) then
  begin
    FOnEditCheckChange(Self, FEditItem.InspectorPanel, FEditItem, FEditItem.BoolValue);
  end;
end;


procedure TInspectorBar.ColComboChanged(Sender: TObject);
var
  CD: TColorDialog;
  EdItem: TInspectorItem;
begin
  EdItem := FEditItem;

  if (FInspectorColorCombo.ItemIndex = 41) and Assigned(FEditItem) then
  begin

    CD := TColorDialog.Create(Self);
    CD.Color := FEditItem.ColorValue;

    if CD.Execute then
    begin
      FEditItem.ColorValue := CD.Color;
      FInspectorColorCombo.ColorValue := CD.Color;
    end;
    CD.Free;

    StopEdit(FEditItem);
  end;

  if Assigned(FOnEditColorChange) and Assigned(EdItem) then
  begin
    FOnEditColorChange(Self, EdItem.InspectorPanel, EdItem,
      FInspectorColorCombo.ColorValue);
  end;
end;

procedure TInspectorBar.DrawCaptionOptions(Panel: TInspectorPanel; Canvas: TCanvas;
  var R: TRect);
begin
// implementation left for inherited components
end;

function TInspectorBar.MouseDownCaptionOptions(Panel: TInspectorPanel; x,
  y: Integer): Boolean;
begin
  Result := False;
end;

function TInspectorBar.MouseMoveCaptionOptions(Panel: TInspectorPanel; x,
  y: Integer): Boolean;
begin
  Result := False;
end;

function TInspectorBar.InspectorIndent: Integer;
begin
  if PanelCaption.SideDisplay then
    Result := PanelCaption.SideWidth
  else
    Result := 0;
end;

function TInspectorBar.InspectorWidth: Integer;
begin
  if FScrollBar.Visible then
    Result := Width - FScrollBar.Width
  else
    Result := Width;

  if (FShowUP or FShowDown) and (FMode = imSinglePanelActive) then
    Dec(Result, 20);
end;

function TInspectorBar.HelpWidth: Integer;
begin
  if FScrollBar.Visible then
    Result := Width - FScrollBar.Width
  else
    Result := Width;
end;


function TInspectorBar.IsMouseDown: Boolean;
begin
  Result := FMouseDown;
end;

function TInspectorBar.HintCaptionOptions(Panel: TInspectorPanel; x,
  y: Integer; var Hint: string): Boolean;
begin
  Result := False;
end;

function TInspectorBar.DoVisualStyles: Boolean;
begin
  if FIsWinXP then
    Result := IsThemeActive
  else
    Result := False;
end;

function TInspectorBar.GetVersionString: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn))) + '.' + IntToStr(Lo(Hiword(vn))) + '.' + IntToStr(Hi(Loword(vn))) + '.' + IntToStr(Lo(Loword(vn))) + ' ' + DATE_VER;
end;

procedure TInspectorBar.SetOleDropTarget(const Value: Boolean);
begin
  FOleDropTarget := Value;
  if not (csDesigning in ComponentState) then
  begin
    if FOleDropTarget then
    begin
      FInspectorDropTarget := TInspectorBarDropTarget.Create(Self);
      FOleDropTargetAssigned := RegisterDragDrop(Handle, FInspectorDropTarget) = s_OK;
    end
    else
    begin
      if FOleDropTargetAssigned then RevokeDragDrop(Handle);
      FOleDropTargetAssigned := False;
    end;
  end;
end;

procedure TInspectorBar.AlignControls(AControl: TControl; var Rect: TRect);
begin
  inherited;

  if (csDesigning in ComponentState) and not (csDestroying in ComponentState) then
    ArrangeControls;
end;

procedure TInspectorBar.CMControlChange(var Message: TMessage);
begin
{$IFDEF TMSDEBUG}
  if (csDesigning in COmponentState) and not (csLoading in ComponentSTate) then
  begin
    if Boolean(Message.LParam) then
      ShowMessage('control added : ' + TWinControl(Message.WParam).Name + ' at (' + IntToStr(TWinControl(Message.WParam).Left) + ':' +
        IntToStr(TWinControl(Message.WParam).Top) + ')')
    else
      ShowMessage('control removed : ' + TWinControl(Message.WParam).Name);

    ShowMessage(inttostr(controlcount));
  end;
{$ENDIF}
end;

procedure TInspectorBar.CMFontChanged(var Message: TMessage);
begin
  PanelCaption.ActiveFont.Size := Font.Size;
  PanelCaption.Font.Size := Font.Size;
end;

procedure TInspectorBar.ArrangeControls;
var
  i: Integer;
  r: TRect;
  sw, RealTopPanel: Integer;
  tempHeight, k: integer;
begin
  if Mode = imSinglePanelActive then
  begin
    RealTopPanel := GetRealTopPanel;
    if RealTopPanel = -1 then exit;
    for i := 1 to Panels.Count do
    begin
      if i - 1 = RealTopPanel then
      begin
        if Assigned(Panels[i - 1].Control) then
        begin
          Panels[i - 1].Control.Left := 0;
          Panels[i - 1].Control.Width := Width;
          Panels[i - 1].Control.Top := (getNumbervisiblePanelUp(RealTopPanel) + 1) * PanelCaption.Height;
          Panels[i - 1].Control.Height := BarHeight - ((getNumbervisiblePanelDown(0) + 1) * PanelCaption.Height); //All panles =>(down(0)+1)
          Panels[i - 1].Control.Visible := True;
        end;
      end
      else
      begin
        if Assigned(Panels[i - 1].Control) then
        begin
          Panels[i - 1].Control.Width := 0;
          Panels[i - 1].Control.Height := 0;
          Panels[i - 1].Control.Visible := False;
        end;
      end;
    end;
  end
  else
  begin
    if FScrollBar.Visible then
      sw := Width - FScrollBar.Width
    else
      sw := Width;
    for i := 1 to Panels.Count do
    begin
      if not FPanels[i - 1].Visible then continue;
      if Assigned(Panels[i - 1].Control) then
      begin
        if Panels[i - 1].Open then
        begin
          R := GetCaptionRect(Panels[i - 1]);
          Panels[i - 1].Control.Left := 0;
          Panels[i - 1].Control.Width := sw;
          tempHeight := 0;
          for k := 1 to Panels[i - 1].Items.Count do
            if Panels[i - 1].Items[k - 1].Visible then
              if Panels[i - 1].VariableItemHeight then
                Inc(tempHeight, Panels[i - 1].Items[k - 1].Height)
              else
                Inc(tempHeight, Panels[i - 1].ItemHeight);

          Panels[i - 1].Control.Height := tempHeight;
          Panels[i - 1].Control.Top := R.Bottom;
          Panels[i - 1].Control.Visible := True;
        end
        else
        begin
          Panels[i - 1].Control.Width := 0;
          Panels[i - 1].Control.Height := 0;
          Panels[i - 1].Control.Visible := False;
        end;
      end;
    end;
  end;
end;

procedure TInspectorBar.HidePopup;
begin
  PostMessage(Handle, WM_SETFOCUS, 0, 0);
end;

procedure TInspectorBar.CMWantSpecialKey(var Msg: TCMWantSpecialKey);
begin
  inherited;
  if Msg.CharCode in [VK_UP, VK_DOWN] then
    Msg.Result := 1;
end;


procedure TInspectorBar.SetShowEditorAlways(const Value: Boolean);
begin
  if FShowEditorAlways <> Value then
  begin
    FShowEditorAlways := Value;
    Invalidate;
  end;
end;

procedure TInspectorBar.DrawItemNode(X, Y: Integer; State: Boolean);
var
  OldColor: TColor;
begin
  OldColor := Canvas.Brush.Color;
  if not State then
  begin
    Canvas.Draw(X + 4, Y + 4, FNodeCloseGlyph);
   { Canvas.Pen.Color := clBlack;
    Canvas.Pen.Width := 1;
    Canvas.Brush.Color := clWhite;

    Canvas.Rectangle(X + 4, Y + 4, X + 13, Y + 13);
    Canvas.MoveTo(X + 6, Y + 8);
    Canvas.LineTo(X + 11, Y + 8);
   }
  end
  else
  begin
    Canvas.Draw(X + 4, Y + 4, FNodeOpenGlyph);
   { Canvas.Pen.Color := clBlack;
    Canvas.Pen.Width := 1;
    Canvas.Brush.Color := clWhite;

    Canvas.Rectangle(X + 4, Y + 4, X + 13, Y + 13);
    Canvas.MoveTo(X + 6, Y + 8);
    Canvas.LineTo(X + 11, Y + 8);
    Canvas.MoveTo(X + 8, Y + 6);
    Canvas.LineTo(X + 8, Y + 11);
   }
  end;
  Canvas.Brush.Color := OldColor;
end;

procedure TInspectorBar.DoExit;
begin
  inherited;
  if Assigned(EditItem) then
    StopEdit(EditItem);

  if not FDisableExit then
    FEditLast := EditItem
  else
    FEditLast := nil;

  FEditItem := nil;
end;

procedure TInspectorBar.DoEditProp(APanel: TInspectorPanel; AItem: TInspectorItem);
begin
  if Assigned(FOnEditProp) then
    FOnEditProp(Self, APanel, AItem);
end;

procedure TInspectorBar.DoEditStart(APanel: TInspectorPanel; AItem: TInspectorItem);
begin
  if Assigned(FOnEditStart) then
    FOnEditStart(Self, APanel, AItem);
end;

procedure TInspectorBar.DoEnter;
begin
  inherited;

  if Assigned(FEditLast) then
  begin
    FEditItem := FEditLast;
// if not (FEditLast.PropertyType in [ptTextButton,ptPropButton,ptFont]) then
//   StartEdit(FEditLast);
  end;
end;

procedure TInspectorBar.WMPaint(var Message: TWMPaint);
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


procedure TInspectorBar.WMEraseBkGnd(var Message: TMessage);
begin
  Message.Result := 1;
end;

function TInspectorBar.GetBarHeight: Integer;
begin
  if HelpPanel.Visible then
    Result := Height - HelpPanel.Height
  else
    Result := Height;
end;

function TInspectorBar.InDesign: Boolean;
begin
  InDesign := csDesigning in ComponentState;
end;

procedure TInspectorBar.SpinChange(Sender: TObject);
var
  s: string;
begin
  if Assigned(FOnEditSpinChange) and Assigned(FEditItem) then
  begin
    s := FInspectorSpin.Text;
    FOnEditSpinChange(Self, FEditItem.InspectorPanel, FEditItem, s);
    if s <> FInspectorSpin.Text then
      FInspectorSpin.Text := s;
  end;
end;

procedure TInspectorBar.SpinDown(Sender: TObject);
var
  s: string;
begin
  if Assigned(FOnEditSpinDown) and Assigned(FEditItem) then
  begin
    s := FInspectorSpin.Text;
    FOnEditSpinDown(Self, FEditItem.InspectorPanel, FEditItem, s);
    if s <> FInspectorSpin.Text then
      FInspectorSpin.Text := s;
  end;
end;

procedure TInspectorBar.SpinUp(Sender: TObject);
var
  s: string;
begin
  if Assigned(FOnEditSpinUp) and Assigned(FEditItem) then
  begin
    s := FInspectorSpin.Text;
    FOnEditSpinUp(Self, FEditItem.InspectorPanel, FEditItem, s);
    if s <> FInspectorSpin.Text then
      FInspectorSpin.Text := s;
  end;
end;

procedure TInspectorBar.Loaded;
begin
  inherited;
  FDefCursor := Cursor;
  FMainPopupMenu := PopupMenu;
  ArrangeOpenClose;
// If this is not an icon <> 32x32, LargeIcons doesn't refresh correctly.
  Paint;
end;

function TInspectorBar.GetPopupMenuEx: TPopupMenu;
begin
  Result := FMainPopupMenu;
end;

procedure TInspectorBar.SetPopupMenu(const Value: TPopupMenu);
begin
  FMainPopupMenu := Value;
  inherited PopupMenu := Value;
end;

procedure TInspectorBar.SetCheckFalse(const Value: string);
begin
  FCheckFalse := Value;
end;

procedure TInspectorBar.SetCheckTrue(const Value: string);
begin
  FCheckTrue := Value;
end;

procedure TInspectorBar.CollapsAll;
var
  i, j: Integer;
begin
  for i := 1 to Panels.Count do
    for j := 1 to Panels[i - 1].Items.Count do
      Panels[i - 1].Items[j - 1].CollapsChilds;
end;

procedure TInspectorBar.ExpandAll;
var
  i, j: Integer;
begin
  for i := 1 to Panels.Count do
    for j := 1 to Panels[i - 1].Items.Count do
      Panels[i - 1].Items[j - 1].ExpandChilds;
end;

procedure TInspectorBar.RepeatTimer(Sender: TObject);
var
  pt: TPoint;
  RealTopPanel: Integer;
begin
  if csDestroying in ComponentState then
    Exit;

  if Mode = imSinglePanelActive then
  begin
    RealTopPanel := getRealTopPanel;
    if RealTopPanel = -1 then exit;

    GetCursorPos(pt);
    pt := ScreenToClient(pt);

    if PtInRect(FButtonRect, pt) then
    begin
      if FButtonDownPress and FShowDown then
      begin
        if FPanels[RealTopPanel].TopItem < FPanels[RealTopPanel].Items.Count then
          FPanels[RealTopPanel].TopItem := FPanels[RealTopPanel].TopItem + 1;
        Invalidate;
      end;

      if FButtonUpPress and FShowUp then
      begin
        if FPanels[RealTopPanel].TopItem > 0 then
          FPanels[RealTopPanel].TopItem := FPanels[RealTopPanel].TopItem - 1;
        Invalidate;
      end;
    end;
  end;
end;

function TInspectorBar.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER, REL_VER), MakeWord(MIN_VER, MAJ_VER));
end;


procedure TInspectorBar.WMLButtonDblClk(var Message: TWMLButtonDblClk);
var
  Panel: TInspectorPanel;
  Item: TInspectorItem;
begin
  if GetPanelItemAtXY(Message.XPos, Message.YPos, Panel, Item) then
  begin
    if Assigned(FOnItemDblClick) then
      FOnItemDblClick(Self, Panel, Item);
  end;

end;

function TInspectorBar.getNumbervisiblePanelUp(NoPanel: Integer): integer;
var
  i, rez: integer;
begin
  result := -1;
  if (NoPanel > FPanels.Count) or (NoPanel < 0) then exit;
  if NoPanel = FPanels.Count then NoPanel := FPanels.Count - 1;
  rez := -1;
  for i := 0 to NoPanel do
    if FPanels[i].visible then inc(rez);
  result := rez;
end;

function TInspectorBar.getNumbervisiblePanelDown(
  NoPanel: Integer): integer;
var
  i, rez: integer;
begin
  result := 0;
  if (NoPanel > FPanels.Count) or (NoPanel < 0) then exit;
  if NoPanel = FPanels.Count then NoPanel := FPanels.Count - 1;
  rez := -1;
  for i := FPanels.Count - 1 downto NoPanel do
    if FPanels[i].visible then inc(rez);
  result := rez;
end;

function TInspectorBar.getRealTopPanel: integer;
var
  i: integer;
begin
  result := -1;

  if (FTopPanel >= FPanels.Count) or (Ftoppanel < 0) then exit;

  if FPanels.Items[ftoppanel].Visible
    then
  begin
    result := ftoppanel;
    exit;
  end
  else
    for i := ftoppanel to FPanels.count - 1 do
      if FPanels.Items[i].Visible then
      begin
        result := i;
        exit;
      end;
  for i := ftoppanel downto 0 do
    if FPanels.Items[i].Visible then
    begin
      result := i;
      exit;
    end;
end;

function TInspectorBar.GetTimerEnabled: boolean;
begin
  Result := FRepeatTimer.Enabled;
end;

function TInspectorBar.FoundShortcut(Key: Word; Shift: TShiftState): boolean;
var
  i: integer;
  RealTopPanel: Integer;
  ls: Tshortcut;
begin
  result := false;
  ls := key;
  if ssShift in shift then Inc(ls, scShift);
  if ssCtrl in shift then Inc(ls, scCtrl);
  if ssAlt in shift then Inc(ls, scAlt);
  if ls = 0 then exit;
  RealTopPanel := getRealTopPanel;
  if RealTopPanel <> -1 then
  begin
    for i := 0 to panels[RealTopPanel].FItems.Count - 1 do
      if (panels[RealTopPanel].FItems[i].FShortCut = ls) and (panels[RealTopPanel].FItems[i].Visible) then
      begin
        ItemClicked(mbLeft, panels[RealTopPanel], panels[RealTopPanel].FItems[i]);

        if (Panels[realtopPanel].Style = psProperties) then
        begin
          if not panels[RealTopPanel].FItems[i].ReadOnly then
            StartEdit(panels[RealTopPanel].FItems[i]);
        end;


        Result := true;
        Exit;
      end;
  end;

  for i := 0 to panels.Count - 1 do
    if (panels[i].FShortCut = ls) and (panels[i].Visible)
      then
    begin
      if FMode = imSinglePanelActive then
      begin
        if RealTopPanel <> i then
        begin
          PanelOpened(panels[i]);
          TopPanel := i;
          Result := true;
          AfterPanelOpened(panels[i]);
          Exit;
        end;
      end
      else
      begin
        if (not panels[i].FIsOpen) then
        begin
          PanelOpened(panels[i]);
          panels[i].FIsOpen := true;
          ArrangeOpenClose;
          result := true;
          AfterPanelOpened(panels[i]);
          exit;
        end;
      end;
    end;
end;

function TInspectorBar.isItemIndex(Panel: TInspectorPanel;
  Item: TInspectorItem): boolean;

begin
  result := false;
  if Panel.ItemIndex < 0 then exit;
  if Panel.Itemindex >= Panel.FItems.Count then exit;
  result := item = Panel.FItems[Panel.Itemindex];
end;



procedure TInspectorBar.LabelInplaceKeyPress(Sender: TObject;
  var Key: Char);
begin
  case key of
    #13:
      begin
        StopEditingInplace;
        hideinplace;
      end;
    #27: hideinplace;
  end;


end;

procedure TInspectorBar.LabelInplaceExit(sender: tobject);
begin
  StopEditingInplace;
end;

function TInspectorBar.StartEditingInplace(Panel: TInspectorPanel; Item: TInspectorItem): boolean;
var
  pt: Tpoint;
  r: Trect;
  text: string;
  letedit: boolean;
  h: integer;
begin
  Result := False;
  if item.ReadOnly then
    Exit;

  GetCursorPos(pt);
  pt := ScreenToClient(pt);
  GetLabelRect(panel, item, r);

  if PtInRect(r, pt) then
  begin
    if isItemIndex(panel, item) then
    begin
      if panel.WordWrap then
      begin
        FMemoInplace.Color := Panel.SelectColor;
        FMemoInplace.Font.Assign(Panel.FFont);
        FMemoInplace.Font.Color := Panel.SelectFontColor;
      end
      else
      begin
        FEditInplace.Color := Panel.SelectColor;
        FEditInplace.Font.Assign(Panel.FFont);
        FEditInplace.Font.Color := Panel.SelectFontColor;
      end;
    end
    else
    begin
      if panel.WordWrap then
      begin
        FMemoInplace.Color := Panel.Color;
        FMemoInplace.Font.Assign(Panel.FFont);
      end
      else
      begin
        FEditInplace.Color := Panel.Color;
        FEditInplace.Font.Assign(Panel.FFont);
      end;
    end;

    if (panel.Style = psLargeIcon) and (not panel.WordWrap) then
    begin
      case panel.Alignment of
        taLeftJustify:
          begin
            if (FShowUP) or (FShowDown) then
              r.Right := Width - 20
            else
              r.Right := Width;
          end;
        taCenter:
          begin
            if (FShowUP) or (FShowDown) then
              InflateRect(r, ((width - 40) - (r.Right - r.Left)) div 2, 0)
            else
              InflateRect(r, ((width) - (r.Right - r.Left)) div 2, 0);
          end;
        taRightJustify:
          begin
            r.Left := 0;
          end;
      end;
    end;

    if panel.WordWrap then
    begin
      FMemoInplace.Top := r.Top;
      FMemoInplace.Width := r.Right - r.left;
      FMemoInplace.left := r.Left;
      FMemoInplace.Height := r.Bottom - r.Top;
      FMemoInplace.Alignment := panel.Alignment;
      if mode = imSinglePanelActive then
      begin
        h := BarHeight - ((getNumbervisiblePanelDown(getRealTopPanel)) * PanelCaption.Height);
        letedit := FMemoInplace.Top + FMemoInplace.height < h;
      end
      else
        letedit := True;
    end
    else
    begin
      FEditInplace.Top := r.Top;
      FEditInplace.Width := r.Right - r.left;
      FEditInplace.left := r.Left;
      FEditInplace.Height := r.Bottom - r.Top;
      FEditInplace.Alignment := panel.Alignment;

      if mode = imSinglePanelActive then
      begin
        h := BarHeight - ((getNumbervisiblePanelDown(getRealTopPanel)) * PanelCaption.Height);
        letedit := FEditInplace.Top + FEditInplace.height < h;
      end
      else
        letedit := True;
    end;

    if letedit and panel.LabelEdit then
    begin
      ItemInplace := item;
      Text := item.FCaption;
      if Assigned(FOnStartLabelEdit) then
        FOnStartLabelEdit(self, text);

      if panel.WordWrap then
      begin
        FMemoInplace.Text := Text;
        FeditInplace.Visible := False;
        FMemoInplace.Visible := True;
        FMemoInplace.SelStart := 0; // minimize jumpy behavior
        FMemoInplace.SetFocus;
        FMemoInplace.SelectAll;
      end
      else
      begin
        FEditInplace.Text := Text;
        FMemoInplace.Visible := false;
        FEditInplace.Visible := true;
        FEditInplace.SelStart := 0; // minimize jumpy behavior
        FEditInplace.SetFocus;
        FEditInplace.SelectAll;
      end;
      result := true;
    end;
  end; //in rect
end;

procedure TInspectorBar.StopEditingInplace;
var
  accept: Boolean;
  sold, snew: string;
begin
  if ItemInplace = nil then
    Exit;

  if not Assigned(ItemInplace) then
    Exit;

  if not (FEditInplace.Visible or FMemoInplace.Visible) then
    Exit;

  Accept := True;
  sold := ItemInplace.Caption;

  if FEditInplace.Visible then
    snew := FEditInplace.Text
  else
    snew := FMemoInplace.Text;

  if Assigned(FOnStopLabelEdit) then
    FOnStopLabelEdit(self, sold, snew, accept);
  if Accept then
    ItemInplace.Caption := snew;
  HideInplace;
end;

procedure TInspectorBar.ScrollKeydown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  KeyDown(key, shift);
end;

function TInspectorBar.Visiblelabelinplace: boolean;
begin
  Result := FEditInplace.Visible or FmemoInplace.Visible;
end;

function TInspectorBar.PrepareWordWraptext(Panel: TInspectorPanel; text: string; r: trect): string;
var
  i: Integer;
  rez: string;
begin
  FhideMemo.Font.Assign(panel.Font);
  rez := '';
  FhideMemo.Width := r.Right - r.left;
  FhideMemo.Clear;
  FhideMemo.Lines.Add(text);
  for i := 0 to FhideMemo.Lines.count - 1 do
  begin
    if i = FhideMemo.Lines.count - 1 then
      rez := rez + FhideMemo.Lines[i]
    else
      rez := rez + FhideMemo.Lines[i] + #13;

  end;
  Result := rez;
end;

procedure TInspectorBar.HideInplace;
begin
  FEditInplace.Visible := False;
  FmemoInplace.Visible := False;
  ItemInplace := nil;
end;

procedure TInspectorBar.SetEllipsis(const Value: Boolean);
begin
  FEllipsis := Value;
  Invalidate;
end;

procedure TInspectorBar.BeginUpdate;
begin
  inc(FUpdateCount);
  SendMessage(Handle, WM_SETREDRAW, integer(False), 0);
end;

procedure TInspectorBar.EndUpdate;
begin
  if FUpdateCount > 0 then
  begin
    dec(FUpdateCount);
    if FUpdateCount = 0 then
    begin
      SendMessage(Handle, WM_SETREDRAW, integer(True), 0);
      Invalidate;
    end;
  end;
end;

procedure TInspectorBar.SetCheckTextShow(const Value: Boolean);
begin
  FCheckTextShow := Value;
  Invalidate;
end;

function TInspectorBar.GetVersionComp: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn))) + '.' + IntToStr(Lo(Hiword(vn))) + '.' + IntToStr(Hi(Loword(vn))) + '.' + IntToStr(Lo(Loword(vn)));
end;

procedure TInspectorBar.SetVersion(const Value: string);
begin

end;

procedure TInspectorBar.SetComponentStyle(AStyle: TTMSStyle);
begin
  Style := TInspectorBarStyle(AStyle);
end;


procedure TInspectorBar.SetPanelStyle(const Value: TInspectorBarStyle; Panel: TInspectorPanel);
begin
  case FStyle of
    esCustom:
      begin

      end;
    esOffice2003Blue:
    begin
      Panel.Font.Color := clBlack;
      Panel.GradientDirection := gdVertical;
      Panel.GradientStart := PanelCaption.Color;
      Panel.GradientEnd := PanelCaption.ColorTo;
      Panel.GradientSteps := 128;
      Panel.Background := pbGradient;
      Panel.HoverIconColor := $DCFFFF;
      Panel.HoverIconColorTo := $5BC0F7;
      Panel.HoverIconDownColor := $FADAC4;
      Panel.HoverIconDownColorTo := $F5BFA0;
      Panel.HoverIconBorderColor := $962D00;
      Panel.SelectColor := $087FE8;
      Panel.SelectColorTo := $7CDAF7;
      Panel.SelectDownColor := $087FE8;
      Panel.SelectDownColorTo := $7CDAF7;
      Panel.SelectFontColor := clWhite;
    end;
    esOffice2003Olive:
    begin
      Panel.Font.Color := clBlack;
      Panel.GradientDirection := gdVertical;
      Panel.GradientStart := $CFF0EA;
      Panel.GradientEnd := $8CC0B1;
      Panel.GradientSteps := 128;
      Panel.Background := pbGradient;
      Panel.HoverIconColor := $DCFFFF;
      Panel.HoverIconColorTo := $5BC0F7;
      Panel.HoverIconDownColor := $E4F1F2;
      Panel.HoverIconDownColorTo := $AADADA;
      Panel.HoverIconBorderColor := $588060;
      Panel.SelectColor := $087FE8;
      Panel.SelectColorTo := $7CDAF7;
      Panel.SelectDownColor := $087FE8;
      Panel.SelectDownColorTo := $7CDAF7;
      Panel.SelectFontColor := clWhite;
    end;
    esOffice2003Silver:
    begin
      Panel.Font.Color := clBlack;
      Panel.GradientDirection := gdVertical;
      Panel.GradientStart := PanelCaption.Color;
      Panel.GradientEnd := PanelCaption.ColorTo;
      Panel.GradientSteps := 128;
      Panel.Background := pbGradient;
      Panel.HoverIconColor := $DCFFFF;
      Panel.HoverIconColorTo := $5BC0F7;
      Panel.HoverIconDownColor := $E4F1F2;
      Panel.HoverIconDownColorTo := $F7F3F3;
      Panel.HoverIconBorderColor := $947C7C;
      Panel.SelectColor := $087FE8;
      Panel.SelectColorTo := $7CDAF7;
      Panel.SelectDownColor := $087FE8;
      Panel.SelectDownColorTo := $7CDAF7;
      Panel.SelectFontColor := clWhite;
    end;
    esOffice2003Classic:
    begin
      Panel.Font.Color := clBlack;
      Panel.GradientDirection := gdVertical;
      Panel.GradientStart := PanelCaption.Color;
      Panel.GradientEnd := PanelCaption.ColorTo;
      Panel.GradientSteps := 128;
      Panel.Background := pbGradient;
      Panel.HoverIconColor := $D2BDB6;
      Panel.HoverIconColorTo := $D2BDB6;
      Panel.HoverIconDownColor := $E4F1F2;
      Panel.HoverIconDownColorTo := $F4F5F6;
      Panel.HoverIconBorderColor := $808080;
      Panel.SelectColor := $D8D5D4;
      Panel.SelectColorTo := $D8D5D4;
      Panel.SelectDownColor := $B59285;
      Panel.SelectDownColorTo := $B59285;
      Panel.SelectFontColor := clWhite;
    end;
    esWhidbey:
    begin
      Panel.Font.Color := clBlack;
      Panel.GradientDirection := gdVertical;
      Panel.GradientStart := PanelCaption.Color;
      Panel.GradientEnd := PanelCaption.ColorTo;
      Panel.GradientSteps := 128;
      Panel.Background := pbGradient;
      Panel.HoverIconColor := $DCFFFF;
      Panel.HoverIconColorTo := $5BC0F7;
      Panel.HoverIconDownColor := $E4F1F2;
      Panel.HoverIconDownColorTo := $EBEEEF;
      Panel.HoverIconBorderColor := $A8C0C0;
      Panel.SelectColor := $087FE8;
      Panel.SelectColorTo := $7CDAF7;
      Panel.SelectDownColor := $087FE8;
      Panel.SelectDownColorTo := $7CDAF7;
      Panel.SelectFontColor := clWhite;
    end;
    esOffice2007Luna:
    begin
      Panel.Font.Color := clBlack;
      Panel.GradientDirection := gdVertical;
      Panel.GradientStart := $F2E4D5;
      Panel.GradientEnd := $F2E4D5;
      Panel.GradientSteps := 128;
      Panel.Background := pbGradient;
      Panel.HoverIconColor := $DCFFFF;
      Panel.HoverIconColorTo := $5BC0F7;
      Panel.HoverIconDownColor := $FADAC4;
      Panel.HoverIconDownColorTo := $F5BFA0;
      Panel.HoverIconBorderColor := $962D00;
      Panel.SelectColor := $087FE8;
      Panel.SelectColorTo := $7CDAF7;
      Panel.SelectDownColor := $087FE8;
      Panel.SelectDownColorTo := $7CDAF7;
      Panel.SelectFontColor := clWhite;

    end;
    esOffice2007Obsidian:
    begin
      Panel.Font.Color := clBlack;
      Panel.GradientDirection := gdVertical;
      Panel.GradientStart := $EBEBEB;
      Panel.GradientEnd := $EBEBEB;
      Panel.GradientSteps := 128;
      Panel.Background := pbGradient;
      Panel.HoverIconColor := $DCFFFF;
      Panel.HoverIconColorTo := $5BC0F7;
      Panel.HoverIconDownColor := $FADAC4;
      Panel.HoverIconDownColorTo := $F5BFA0;
      Panel.HoverIconBorderColor := $962D00;
      Panel.SelectColor := $087FE8;
      Panel.SelectColorTo := $7CDAF7;
      Panel.SelectDownColor := $087FE8;
      Panel.SelectDownColorTo := $7CDAF7;
      Panel.SelectFontColor := clWhite;
    end;
    esOffice2007Silver:
    begin
      Panel.Font.Color := clBlack;
      Panel.GradientDirection := gdVertical;
      Panel.GradientStart := $ECEAE8;
      Panel.GradientEnd := $ECEAE8;
      Panel.GradientSteps := 128;
      Panel.Background := pbGradient;
      Panel.HoverIconColor := $DCFFFF;
      Panel.HoverIconColorTo := $5BC0F7;
      Panel.HoverIconDownColor := $FADAC4;
      Panel.HoverIconDownColorTo := $F5BFA0;
      Panel.HoverIconBorderColor := $962D00;
      Panel.SelectColor := $087FE8;
      Panel.SelectColorTo := $7CDAF7;
      Panel.SelectDownColor := $087FE8;
      Panel.SelectDownColorTo := $7CDAF7;
      Panel.SelectFontColor := clWhite;
    end;
    esWindowsXP:
    begin
      Panel.Font.Color := clBlack;
      Panel.GradientDirection := gdVertical;
      Panel.GradientStart := clWhite;
      Panel.GradientEnd := clWhite;
      Panel.GradientSteps := 128;
      Panel.Background := pbGradient;
      Panel.HoverIconColor := clHighlight;
      Panel.HoverIconColorTo := clHighlight;
      Panel.HoverIconDownColor := clHighlight;
      Panel.HoverIconDownColorTo := clHighlight;
      Panel.HoverIconBorderColor := clBlack;
      Panel.SelectColor := clInactiveCaption;
      Panel.SelectColorTo := clInactiveCaption;
      Panel.SelectDownColor := clInactiveCaption;
      Panel.SelectDownColorTo := clInactiveCaption;
      Panel.SelectFontColor := clWhite;
    end;
    esWindowsVista:
    begin
      Panel.Font.Color := clBlack;
      Panel.GradientDirection := gdVertical;
      Panel.GradientStart := $FFFFFF;
      Panel.GradientEnd := $FFFFFF;
      Panel.GradientSteps := 128;
      Panel.Background := pbGradient;
      Panel.HoverIconColor := $FFFDF9;
      Panel.HoverIconColorTo := $FFFAF0;
      Panel.HoverIconDownColor := $FEF9F0;
      Panel.HoverIconDownColorTo := $FDF0D7;
      Panel.HoverIconBorderColor := $FCF2DA;
      Panel.SelectColor := $FEF9F0;
      Panel.SelectColorTo := $FDF0D7;
      Panel.SelectDownColor := $FDF0D7;
      Panel.SelectDownColorTo := $FDF0D7;
      Panel.SelectFontColor := clBlack;
    end;
    esWindows7:
    begin
      Panel.Font.Color := clBlack;
      Panel.GradientDirection := gdVertical;
      Panel.GradientStart := $FFFFFF;
      Panel.GradientEnd := $FFFFFF;
      Panel.GradientSteps := 128;
      Panel.Background := pbGradient;
      Panel.HoverIconColor := $FDFBFA;
      Panel.HoverIconColorTo := $FDF3EB;
      Panel.HoverIconDownColor := $FCEBDC;
      Panel.HoverIconDownColorTo := $FCDBC1;
      Panel.HoverIconBorderColor := $FBD6B8;
      Panel.SelectColor := $FCEBDC;
      Panel.SelectColorTo := $FCDBC1;
      Panel.SelectDownColor := $FCDBC1;
      Panel.SelectDownColorTo := $FCDBC1;
      Panel.SelectFontColor := clBlack;
    end;
    esTerminal:
    begin
      Panel.Font.Color := clBlack;
      Panel.GradientDirection := gdVertical;
      Panel.GradientStart := clBtnFace;
      Panel.GradientEnd := clBtnFace;
      Panel.GradientSteps := 128;
      Panel.Background := pbGradient;
      Panel.HoverIconColor := clSilver;
      Panel.HoverIconColorTo := clSilver;
      Panel.HoverIconDownColor := clHighlight;
      Panel.HoverIconDownColorTo := clHighlight;
      Panel.HoverIconBorderColor := clGray;
      Panel.SelectColor := clHighLight;
      Panel.SelectColorTo := clHighLight;
      Panel.SelectDownColor := clSilver;
      Panel.SelectDownColorTo := clSilver;
      Panel.SelectFontColor := clWhite;
    end;
    esOffice2010Blue:
    begin
      Panel.Font.Color := clBlack;
      Panel.GradientDirection := gdVertical;
      Panel.GradientStart := $EAD3BF;
      Panel.GradientEnd := $EAD3BF;
      Panel.GradientSteps := 128;
      Panel.Background := pbGradient;
      Panel.HoverIconColor := $8AE3FD;
      Panel.HoverIconColorTo := $8AE3FD;
      Panel.HoverIconDownColor := $6CD0FF;
      Panel.HoverIconDownColorTo := $7BEEFF;
      Panel.HoverIconBorderColor := $58CAF1;
      Panel.SelectColor := $6CD0FF;
      Panel.SelectColorTo := $7BEEFF;
      Panel.SelectDownColor := $6CD0FF;
      Panel.SelectDownColorTo := $7BEEFF;
      Panel.SelectFontColor := clBlack;
    end;
    esOffice2010Silver:
    begin
      Panel.Font.Color := clBlack;
      Panel.GradientDirection := gdVertical;
      Panel.GradientStart := $D4CFCB;
      Panel.GradientEnd := $D4CFCB;
      Panel.GradientSteps := 128;
      Panel.Background := pbGradient;
      Panel.HoverIconColor := $8AE3FD;
      Panel.HoverIconColorTo := $8AE3FD;
      Panel.HoverIconDownColor := $6CD0FF;
      Panel.HoverIconDownColorTo := $7BEEFF;
      Panel.HoverIconBorderColor := $58CAF1;
      Panel.SelectColor := $6CD0FF;
      Panel.SelectColorTo := $7BEEFF;
      Panel.SelectDownColor := $6CD0FF;
      Panel.SelectDownColorTo := $7BEEFF;
      Panel.SelectFontColor := clBlack;
    end;
    esOffice2010Black:
    begin
      Panel.Font.Color := clWhite;
      Panel.GradientDirection := gdVertical;
      Panel.GradientStart := $656565;
      Panel.GradientEnd := $656565;
      Panel.GradientSteps := 128;
      Panel.Background := pbGradient;
      Panel.HoverIconColor := $8AE3FD;
      Panel.HoverIconColorTo := $8AE3FD;
      Panel.HoverIconDownColor := $6CD0FF;
      Panel.HoverIconDownColorTo := $7BEEFF;
      Panel.HoverIconBorderColor := $58CAF1;
      Panel.SelectColor := $6CD0FF;
      Panel.SelectColorTo := $7BEEFF;
      Panel.SelectDownColor := $6CD0FF;
      Panel.SelectDownColorTo := $7BEEFF;
      Panel.SelectFontColor := clBlack;
    end;
    esWindows8, esWindows10:
    begin
      Panel.Font.Color := clBlack;
      Panel.GradientDirection := gdVertical;
      Panel.GradientStart := clWhite;
      Panel.GradientEnd := clWhite;
      Panel.GradientSteps := 128;
      Panel.Background := pbGradient;
      Panel.HoverIconColor := $F7EFE8;
      Panel.HoverIconColorTo := $F7EFE8;
      Panel.HoverIconDownColor := $F7E0C9;
      Panel.HoverIconDownColorTo := $F7E0C9;
      Panel.HoverIconBorderColor := $F9CEA4;
      Panel.SelectColor := $F7E0C9;
      Panel.SelectColorTo := $F7E0C9;
      Panel.SelectDownColor := $DAA026;
      Panel.SelectDownColorTo := $DAA026;
      Panel.SelectFontColor := clBlack;
    end;
    esOffice2013White:
    begin
      Panel.Font.Color := clBlack;
      Panel.GradientDirection := gdVertical;
      Panel.GradientStart := clWhite;
      Panel.GradientEnd := clWhite;
      Panel.GradientSteps := 128;
      Panel.Background := pbGradient;
      Panel.HoverIconColor := $FCF0E4;
      Panel.HoverIconColorTo := $FCF0E4;
      Panel.HoverIconDownColor := $FCE2C8;
      Panel.HoverIconDownColorTo := $FCE2C8;
      Panel.HoverIconBorderColor := $EAB47E;
      Panel.SelectColor := $FCE2C8;
      Panel.SelectColorTo := $FCE2C8;
      Panel.SelectDownColor := $FCE2C8;
      Panel.SelectDownColorTo := $FCE2C8;
      Panel.SelectFontColor := clBlack;
    end;
    esOffice2013LightGray:
    begin
      Panel.Font.Color := clBlack;
      Panel.GradientDirection := gdVertical;
      Panel.GradientStart := clWhite;
      Panel.GradientEnd := clWhite;
      Panel.GradientSteps := 128;
      Panel.Background := pbGradient;
      Panel.HoverIconColor := $FCF0E4;
      Panel.HoverIconColorTo := $FCF0E4;
      Panel.HoverIconDownColor := $FCE2C8;
      Panel.HoverIconDownColorTo := $FCE2C8;
      Panel.HoverIconBorderColor := $EAB47E;
      Panel.SelectColor := $FCE2C8;
      Panel.SelectColorTo := $FCE2C8;
      Panel.SelectDownColor := $FCE2C8;
      Panel.SelectDownColorTo := $FCE2C8;
      Panel.SelectFontColor := clBlack;
    end;
    esOffice2013Gray:
    begin
      Panel.Font.Color := clBlack;
      Panel.GradientDirection := gdVertical;
      Panel.GradientStart := clWhite;
      Panel.GradientEnd := clWhite;
      Panel.GradientSteps := 128;
      Panel.Background := pbGradient;
      Panel.HoverIconColor := $FCF0E4;
      Panel.HoverIconColorTo := $FCF0E4;
      Panel.HoverIconDownColor := $FCE2C8;
      Panel.HoverIconDownColorTo := $FCE2C8;
      Panel.HoverIconBorderColor := $EAB47E;
      Panel.SelectColor := $FCE2C8;
      Panel.SelectColorTo := $FCE2C8;
      Panel.SelectDownColor := $FCE2C8;
      Panel.SelectDownColorTo := $FCE2C8;
      Panel.SelectFontColor := clBlack;
    end;
    esOffice2016White:
    begin
      Panel.Font.Color := clBlack;
      Panel.GradientDirection := gdVertical;
      Panel.GradientStart := clWhite;
      Panel.GradientEnd := clWhite;
      Panel.GradientSteps := 128;
      Panel.Background := pbGradient;
      Panel.HoverIconColor := $F2E1D5;
      Panel.HoverIconColorTo := $F2E1D5;
      Panel.HoverIconDownColor := $E3BDA3;
      Panel.HoverIconDownColorTo := $E3BDA3;
      Panel.HoverIconBorderColor := $F2E1D5;
      Panel.SelectColor := $E3BDA3;
      Panel.SelectColorTo := $E3BDA3;
      Panel.SelectDownColor := $E3BDA3;
      Panel.SelectDownColorTo := $E3BDA3;
      Panel.SelectFontColor := $505050;

    end;
    esOffice2016Gray:
    begin
      Panel.Font.Color := clWhite;
      Panel.GradientDirection := gdVertical;
      Panel.GradientStart := $B2B2B2;
      Panel.GradientEnd := $B2B2B2;
      Panel.GradientSteps := 128;
      Panel.Background := pbGradient;
      Panel.HoverIconColor := $F2E1D5;
      Panel.HoverIconColorTo := $F2E1D5;
      Panel.HoverIconDownColor :=$E3BDA3;
      Panel.HoverIconDownColorTo := $E3BDA3;
      Panel.HoverIconBorderColor := $F2E1D5;
      Panel.SelectColor := $E3BDA3;
      Panel.SelectColorTo := $E3BDA3;
      Panel.SelectDownColor := $E3BDA3;
      Panel.SelectDownColorTo := $E3BDA3;
      Panel.SelectFontColor := $424242;
    end;
    esOffice2016Black:
    begin
      Panel.Font.Color := clWhite;
      Panel.GradientDirection := gdVertical;
      Panel.GradientStart := $363636;
      Panel.GradientEnd := $363636;
      Panel.GradientSteps := 128;
      Panel.Background := pbGradient;
      Panel.HoverIconColor := $6A6A6A;
      Panel.HoverIconColorTo := $6A6A6A;
      Panel.HoverIconDownColor := $444444;
      Panel.HoverIconDownColorTo := $444444;
      Panel.HoverIconBorderColor := $6A6A6A;
      Panel.SelectColor := $444444;
      Panel.SelectColorTo := $444444;
      Panel.SelectDownColor := $444444;
      Panel.SelectDownColorTo := $444444;
      Panel.SelectFontColor := $A6A6A6;
    end;
  end;

end;

procedure TInspectorBar.SetStyle(const Value: TInspectorBarStyle);
var
  i: integer;

begin
  if (FStyle <> Value) then
  begin
    FStyle := Value;
    case FStyle of
      esCustom:
        begin
          {
          PanelCaption.ColorTo := clNone;
          Color := PanelCaption.Color;
          for i := 0 to Panels.Count - 1 do
          begin
            p := panels[i];
            p.GradientDirection := gdVertical;
            p.GradientEnd := PanelCaption.ColorTo;
            p.HoverIconColorTo := clNone;
            p.HoverIconDownColorTo := clNone;
            p.SelectColorTo := clNone;
            p.SelectDownColorTo := clNone;
            p.SelectFontColor := clWhite;
          end;
          }
        end;
      esOffice2003Blue:
        begin
          PanelCaption.Color := $00FFE7D6;
          PanelCaption.ColorTo := $00C67B52;
          Color := PanelCaption.Color;
          PanelCaption.ActiveFont.Color := clBlack;
          for i := 0 to Panels.Count - 1 do
            SetPanelStyle(esOffice2003Blue, Panels[i]);

          HelpPanel.ColorTo := Color;
          HelpPanel.Color := PanelCaption.ColorTo;
        end;
      esOffice2003Olive:
        begin
          PanelCaption.Color := $82C0AF;
          PanelCaption.ColorTo := $447A63;
          PanelCaption.ActiveFont.Color := clBlack;
          Color := PanelCaption.Color;
          for i := 0 to Panels.Count - 1 do
            SetPanelStyle(esOffice2003Olive, Panels[i]);

          HelpPanel.ColorTo := Color;
          HelpPanel.Color := PanelCaption.ColorTo;
        end;
      esOffice2003Silver:
        begin
          PanelCaption.Color := $00F7E7E7;
          PanelCaption.ColorTo := $00B5A5A5;
          PanelCaption.ActiveFont.Color := clBlack;
          Color := PanelCaption.Color;
          for i := 0 to Panels.Count - 1 do
            SetPanelStyle(esOffice2003Silver, Panels[i]);

          HelpPanel.ColorTo := Color;
          HelpPanel.Color := PanelCaption.ColorTo;
        end;
      esOffice2003Classic:
        begin
          PanelCaption.Color := $00E7E7EF;
          PanelCaption.ColorTo := $00C6CECE;
          PanelCaption.ActiveFont.Color := clBlack;
          Color := PanelCaption.Color;
          for i := 0 to Panels.Count - 1 do
            SetPanelStyle(esOffice2003Classic, Panels[i]);

          HelpPanel.ColorTo := Color;
          HelpPanel.Color := PanelCaption.ColorTo;
        end;
      esWhidbey:
        begin
          PanelCaption.Color := $EBEEEF;
          PanelCaption.ColorTo := $7E9898;
          PanelCaption.ActiveFont.Color := clBlack;
          Color := PanelCaption.Color;
          for i := 0 to Panels.Count - 1 do
            SetPanelStyle(esWhidbey, panels[i]);

          HelpPanel.ColorTo := Color;
          HelpPanel.Color := PanelCaption.ColorTo;
        end;
      esOffice2007Luna:
        begin
          PanelCaption.Color := $FFEFE3;
          PanelCaption.ColorTo := $FFD2AF;
          PanelCaption.ActiveFont.Color := clBlack;
          Color := PanelCaption.Color;
          for i := 0 to Panels.Count - 1 do
            SetPanelStyle(esOffice2007Luna, Panels[i]);

          HelpPanel.ColorTo := PanelCaption.Color;
          HelpPanel.Color := PanelCaption.ColorTo;
        end;
     esOffice2007Obsidian:
        begin
          PanelCaption.Color := $F8F7F6;
          PanelCaption.ColorTo := $E8E0DB;
          PanelCaption.ActiveFont.Color := clBlack;
          Color := PanelCaption.Color;
          for i := 0 to Panels.Count - 1 do
            SetPanelStyle(esOffice2007Obsidian, Panels[i]);

          HelpPanel.ColorTo := PanelCaption.Color;
          HelpPanel.Color := PanelCaption.ColorTo;
        end;
      esOffice2007Silver:
        begin
          PanelCaption.Color := $F8F7F6;
          PanelCaption.ColorTo := $E8E0DB;
          PanelCaption.ActiveFont.Color := clBlack;
          Color := PanelCaption.Color;
          for i := 0 to Panels.Count - 1 do
            SetPanelStyle(esOffice2007Silver, Panels[i]);

          HelpPanel.ColorTo := PanelCaption.Color;
          HelpPanel.Color := PanelCaption.ColorTo;
        end;
     esWindowsXP:
        begin
          PanelCaption.Color := clWhite;
          PanelCaption.ColorTo := clBtnFace;
          PanelCaption.ActiveFont.Color := clBlack;
          Color := PanelCaption.Color;
          for i := 0 to Panels.Count - 1 do
            SetPanelStyle( esWindowsXP, Panels[i]);

          HelpPanel.ColorTo := clBtnFace;
          HelpPanel.Color := clBtnFace;
        end;
     esWindowsVista:
        begin
          PanelCaption.Color := $FDF8F1;
          PanelCaption.ColorTo := $FCEFD5;
          PanelCaption.ActiveFont.Color := clBlack;
          Color := PanelCaption.Color;
          for i := 0 to Panels.Count - 1 do
            SetPanelStyle( esWindowsVista, Panels[i]);

          HelpPanel.ColorTo := $FCEFD5;
          HelpPanel.Color := $FCEFD5;
        end;
     esWindows7:
        begin
          PanelCaption.Color := $FCEBDC;
          PanelCaption.ColorTo := $FCDBC1;
          PanelCaption.ActiveFont.Color := clBlack;
          Color := PanelCaption.Color;
          for i := 0 to Panels.Count - 1 do
            SetPanelStyle( esWindows7, Panels[i]);

          HelpPanel.ColorTo := $FCDBC1;
          HelpPanel.Color := $FCDBC1;
        end;
     esTerminal:
        begin
          PanelCaption.Color := clBtnFace;
          PanelCaption.ColorTo := clBtnFace;
          PanelCaption.ActiveFont.Color := clBlack;
          Color := PanelCaption.Color;
          for i := 0 to Panels.Count - 1 do
            SetPanelStyle( esTerminal, Panels[i]);

          HelpPanel.ColorTo := clBtnFace;
          HelpPanel.Color := clBtnFace;
        end;
        esOffice2010Blue:
        begin
          PanelCaption.Color := $FDF6EF;
          PanelCaption.ColorTo := $F0DAC7;
          PanelCaption.ActiveFont.Color := clBlack;
          Color := PanelCaption.Color;
          for i := 0 to Panels.Count - 1 do
            SetPanelStyle( esOffice2010Blue, Panels[i]);

          HelpPanel.ColorTo := PanelCaption.Color;
          HelpPanel.Color := PanelCaption.ColorTo;
        end;
        esOffice2010Silver:
        begin
          PanelCaption.Color := $FFFFFF;
          PanelCaption.ColorTo := $EDE5E0;
          PanelCaption.ActiveFont.Color := clBlack;
          Color := PanelCaption.Color;
          for i := 0 to Panels.Count - 1 do
            SetPanelStyle( esOffice2010Silver, Panels[i]);

          HelpPanel.ColorTo := PanelCaption.Color;
          HelpPanel.Color := PanelCaption.ColorTo;
        end;
        esOffice2010Black:
        begin
          PanelCaption.Color := $BFBFBF;
          PanelCaption.ColorTo := $919191;
          PanelCaption.ActiveFont.Color := clBlack;
          Color := PanelCaption.Color;
          for i := 0 to Panels.Count - 1 do
            SetPanelStyle( esOffice2010Black, Panels[i]);

          HelpPanel.ColorTo := PanelCaption.Color;
          HelpPanel.Color := PanelCaption.ColorTo;
        end;
        esWindows8, esWindows10:
        begin
          PanelCaption.Color := $F7F6F5;
          PanelCaption.ColorTo := $F7F6F5;
          PanelCaption.ActiveFont.Color := clBlack;
          Color := PanelCaption.Color;
          for i := 0 to Panels.Count - 1 do
            SetPanelStyle( esWindows8, Panels[i]);

          HelpPanel.ColorTo := PanelCaption.Color;
          HelpPanel.Color := PanelCaption.ColorTo;
        end;
        esOffice2013White:
        begin
          PanelCaption.Color := $EEEEEE;
          PanelCaption.ColorTo := $EEEEEE;
          PanelCaption.ActiveFont.Color := clBlack;
          Color := PanelCaption.Color;
          for i := 0 to Panels.Count - 1 do
            SetPanelStyle( esOffice2013White, Panels[i]);

          HelpPanel.ColorTo := PanelCaption.Color;
          HelpPanel.Color := PanelCaption.ColorTo;
        end;
        esOffice2013LightGray:
        begin
          PanelCaption.Color := $F6F6F6;
          PanelCaption.ColorTo := $F6F6F6;
          PanelCaption.ActiveFont.Color := clBlack;
          Color := PanelCaption.Color;
          for i := 0 to Panels.Count - 1 do
            SetPanelStyle( esOffice2013LightGray, Panels[i]);

          HelpPanel.ColorTo := PanelCaption.Color;
          HelpPanel.Color := PanelCaption.ColorTo;
        end;
        esOffice2013Gray:
        begin
          PanelCaption.Color := $E5E5E5;
          PanelCaption.ColorTo := $E5E5E5;
          PanelCaption.ActiveFont.Color := clBlack;
          Color := PanelCaption.Color;
          for i := 0 to Panels.Count - 1 do
            SetPanelStyle( esOffice2013Gray, Panels[i]);

          HelpPanel.ColorTo := PanelCaption.Color;
          HelpPanel.Color := PanelCaption.ColorTo;
        end;
        esOffice2016White:
        begin
          PanelCaption.Color := $D4D4D4;
          PanelCaption.ColorTo := $D4D4D4;
          PanelCaption.ActiveFont.Color := $444444;
          Color := PanelCaption.Color;
          for i := 0 to Panels.Count - 1 do
            SetPanelStyle( esOffice2016White, Panels[i]);

          HelpPanel.ColorTo := PanelCaption.Color;
          HelpPanel.Color := PanelCaption.ColorTo;
        end;
        esOffice2016Gray:
        begin
          PanelCaption.Color := $444444;
          PanelCaption.ColorTo := $444444;
          PanelCaption.ActiveFont.Color := $F0F0F0;
          Color := PanelCaption.Color;
          for i := 0 to Panels.Count - 1 do
            SetPanelStyle( esOffice2016Gray, Panels[i]);

          HelpPanel.ColorTo := PanelCaption.Color;
          HelpPanel.Color := PanelCaption.ColorTo;
        end;
        esOffice2016Black:
        begin
          PanelCaption.Color := $444444;
          PanelCaption.ColorTo := $444444;
          PanelCaption.ActiveFont.Color := $F0F0F0;
          Color := PanelCaption.Color;
          for i := 0 to Panels.Count - 1 do
            SetPanelStyle( esOffice2016Black, Panels[i]);

          HelpPanel.ColorTo := PanelCaption.Color;
          HelpPanel.Color := PanelCaption.ColorTo;
        end;


    end;
    DefaultGradientDirection := gdVertical;
    ShadeHeader;
  end;
end;

procedure TInspectorBar.SetColorTo(const Value: TColor);
begin
  FColorTo := Value;
end;

procedure TInspectorBar.SetDefaultGradientDirection(
  const Value: TGradientDirection);
begin
  FDefaultGradientDirection := Value;
  Invalidate;
end;

procedure TInspectorBar.ThemeAdapt;
var
  eTheme: XPColorScheme;
begin
  eTheme := CurrentXPTheme();
  case eTheme of
    xpBlue: Style := esOffice2003Blue;
    xpGreen: Style := esOffice2003Olive;
    xpGray: Style := esOffice2003Silver;
  else
    Style := esOffice2003Classic;
  end;
end;

procedure TInspectorBar.WndProc(var Message: TMessage);
begin
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

procedure TInspectorBar.AfterPanelOpened(Panel: TInspectorPanel);
begin
  if Assigned(FOnPanelOpened) then
    FOnPanelOpened(Self, Panel);
end;

function TInspectorBar.IsShortCut(var Message: TWMKey): Boolean;
const
  AltMask = $20000000;
var
  i, j: integer;
  RealTopPanel: Integer;

  AShortCut: TShortCut;
begin
  result := false;
  AShortCut := Byte(Message.CharCode);
  if AShortCut = 0 then exit;
  if GetKeyState(VK_SHIFT) < 0 then Inc(AShortCut, scShift);
  if GetKeyState(VK_CONTROL) < 0 then Inc(AShortCut, scCtrl);
  if Message.KeyData and AltMask <> 0 then Inc(AShortCut, scAlt);

  RealTopPanel := getRealTopPanel;
  for j:= 0 to Panels.Count-1 do
  begin
    if (panels[j].FShortCut = AShortCut) and (panels[j].Visible)
      then
    begin
      if FMode = imSinglePanelActive then
      begin
        if RealTopPanel <> j then
        begin
          PanelOpened(panels[j]);
          TopPanel := j;
          result := true;
          AfterPanelOpened(panels[j]);
          exit;
        end;
      end
      else
      begin
        if (not panels[j].FIsOpen) then
        begin
          PanelOpened(panels[j]);
          panels[j].FIsOpen := true;
          ArrangeOpenClose;
          result := true;
          AfterPanelOpened(panels[j]);
          exit;
        end;
      end;
    end;

    for i := 0 to panels[j].FItems.Count - 1 do
    begin
      if (panels[j].FItems[i].FShortCut = AShortCut) and (panels[j].FItems[i].Visible) then
      begin
        ItemClicked(mbLeft, panels[RealTopPanel], panels[RealTopPanel].FItems[i]);
        result := true;
        exit;
      end;
    end;
  end;
end;

{ TInspectorCaption }

procedure TInspectorCaption.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

constructor TInspectorCaption.Create;
begin
  inherited;
  FHeight := 20;
  FShadeType := stNORMAL;
  FShadeLight := 255;
  FShadeGrain := 32;
  FColor := clBtnFace;
  FColorTo := clNone;
  FActiveFont := TFont.Create;
  FFont := TFont.Create;
  FActiveFont.OnChange := FontChanged;
  FFont.OnChange := FontChanged;
  FGlyphOpen := TBitmap.Create;
  FGlyphClose := TBitmap.Create;
  FCursor := crDefault;
  FBarCursor := crDefault;
  FBackground := TBitmap.Create;
  FActiveBackground := TBitmap.Create;
  FSideWidth := 20;
  FUnderline := False;
  FUnderlineColor := clBlue;
  FUnderlineWidth := 1;
end;

destructor TInspectorCaption.Destroy;
begin
  FBackground.Free;
  FActiveBackground.Free;
  FActiveFont.Free;
  FFont.Free;
  FGlyphOpen.Free;
  FGlyphClose.Free;
  inherited;
end;

procedure TInspectorCaption.FontChanged(Sender: TObject);
begin
  Changed;
end;

procedure TInspectorCaption.SetActiveBackground(const Value: TBitmap);
begin
  FActiveBackground.Assign(Value);
  Changed;
end;

procedure TInspectorCaption.SetActiveFont(const Value: TFont);
begin
  FActiveFont.Assign(Value);
  Changed;
end;

procedure TInspectorCaption.SetAlignment(const Value: TAlignment);
begin
  FAlignment := Value;
  Changed;
end;

procedure TInspectorCaption.SetBackground(const Value: TBitmap);
begin
  FBackground.Assign(Value);
  Changed;
end;

procedure TInspectorCaption.SetColor(const Value: TColor);
begin
  FColor := Value;
  ShadeChanged;
end;

procedure TInspectorCaption.SetColorTo(const Value: TColor);
begin
  FColorTo := Value;
  Changed;
end;

procedure TInspectorCaption.SetCursor(const Value: TCursor);
begin
  FCursor := Value;
end;

procedure TInspectorCaption.SetFlat(const Value: Boolean);
begin
  FFlat := Value;
  Changed;
end;

procedure TInspectorCaption.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
  Changed;
end;

procedure TInspectorCaption.SetGlyphClose(const Value: TBitmap);
begin
  FGlyphClose.Assign(Value);
  Changed;
end;

procedure TInspectorCaption.SetGlyphOpen(const Value: TBitmap);
begin
  FGlyphOpen.Assign(Value);
  Changed;
end;

procedure TInspectorCaption.SetHeight(const Value: Integer);
begin
  FHeight := Value;
  ShadeChanged;
  Changed;
end;

procedure TInspectorCaption.SetIndent(const Value: Integer);
begin
  FIndent := Value;
  Changed;
end;

procedure TInspectorCaption.SetInspectorBar(const Value: TInspectorBar);
begin
  FInspectorBar := Value;
end;

procedure TInspectorCaption.SetOpenCloseGraphic(
  const Value: TOpenCloseGraphic);
begin
  FOpenCloseGraphic := Value;
  Changed;
end;

procedure TInspectorCaption.SetOpenClosePosition(
  const Value: TOpenClosePosition);
begin
  FOpenClosePosition := Value;
  Changed;
end;

procedure TInspectorCaption.SetShadeGrain(const Value: Byte);
begin
  FShadeGrain := Value;
  ShadeChanged;
end;

procedure TInspectorCaption.SetShadeLight(const Value: Byte);
begin
  if Value < 200 then
    FShadeLight := 200
  else
    FShadeLight := Value;
  ShadeChanged;
end;

procedure TInspectorCaption.SetShadeType(const Value: TShadeType);
begin
  FShadeType := Value;
  ShadeChanged;
end;

procedure TInspectorCaption.SetShape(const Value: TCaptionShape);
begin
  FShape := Value;
  Changed;
end;

procedure TInspectorCaption.SetSideDisplay(const Value: Boolean);
begin
  FSideDisplay := Value;
  Changed;
end;

procedure TInspectorCaption.SetSideWidth(const Value: Integer);
begin
  FSideWidth := Value;
  Changed;
end;

procedure TInspectorCaption.SetUnderLine(const Value: Boolean);
begin
  FUnderLine := Value;
  Changed;
end;

procedure TInspectorCaption.SetUnderlineColor(const Value: TColor);
begin
  FUnderlineColor := Value;
  Changed;
end;

procedure TInspectorCaption.SetUnderlineWidth(const Value: Integer);
begin
  FUnderlineWidth := Value;
  Changed;
end;

procedure TInspectorCaption.SetVAlignment(const Value: TVAlignment);
begin
  FVAlignment := Value;
  Changed;
end;

procedure TInspectorCaption.ShadeChanged;
begin
  if Assigned(FOnShadeChange) then
    FOnShadeChange(Self);
  FInspectorBar.ShadeHeader;
end;


{ TInspectorEdit }

constructor TInspectorEdit.Create(AOwner: TComponent);
begin
  inherited;
  BorderStyle := bsNone;
  FMultiLine := True;
end;

procedure TInspectorEdit.CreateParams(var Params: TCreateParams);
begin
  inherited;
  if FMultiLine then
    Params.Style := Params.Style or ES_MULTILINE;
end;

procedure TInspectorEdit.DoExit;
begin
  inherited;
  (Parent as TInspectorBar).StopEdit(FInspectorItem);
end;

procedure TInspectorEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if Key in [VK_DOWN, VK_UP] then
  begin
    FInspectorItem.InspectorBar.NextEdit(FInspectorItem, Key);
    Key := 0;
  end;
  inherited;
end;

procedure TInspectorEdit.KeyPress(var Key: Char);
begin
  FInspectorItem.InspectorBar.KeyPress(Key);

  if Key = #13 then
  begin
    Key := #0;
    FInspectorItem.InspectorBar.NextEdit(FInspectorItem, VK_RETURN);
  end;

  if Key = #27 then
  begin
    Key := #0;
    Text := FOrigValue;
    SelectAll;
  end;

  if InspEditType = ieInteger then
  begin
    if not (IsNumChar(Key) or (Key = '-') or (Key = #8)) then
        Key := #0;

    if (Key = '-') and ((Pos('-', Text) > 0) and not (SelLength = Length(Text))) then
      Key := #0;

    if (Key = '-') and (SelStart > 0) then
    begin
      Text := '-' + Text;
      Key := #0;
    end;
  end;

  if InspEditType = ieFloat then
  begin
    if not (IsNumChar(Key) or (Key = 'e') or (Key = 'E') or (Key = '-') or (Key = DecimalSeparator) or (Key = #8)) then
      Key := #0;

    if (Key = '-') and ((Pos('-', Text) > 0) and (pos('E',uppercase(Text)) = 0) and not (SelLength = Length(Text))) then
      Key := #0;

    if (Key = '-') and (SelStart > 0) and (pos('E',uppercase(Text)) = 0) then
    begin
      Text := '-' + Text;
      Key := #0;
    end;

    if (Key = DecimalSeparator) and (Pos(DecimalSeparator, Text) > 0) then
      Key := #0;
  end;

  inherited;
end;

procedure TInspectorEdit.SetMultiLine(const Value: Boolean);
begin
  FMultiLine := Value;
  RecreateWnd;
end;

{ TInspectorCombo }

constructor TInspectorCombo.Create(AOwner: TComponent);
begin
  inherited;
  Flat := True;
end;

procedure TInspectorCombo.DoExit;
begin
  inherited;
  (Parent as TInspectorBar).StopEdit(FInspectorItem);
end;

procedure TInspectorCombo.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (Key in [VK_DOWN, VK_UP]) and not (ssCtrl in Shift) and not DroppedDown then
  begin
    FInspectorItem.InspectorBar.NextEdit(FInspectorItem, Key);
    Key := 0;
  end;
  inherited;
end;

procedure TInspectorCombo.KeyPress(var Key: Char);
begin
  if (Key = #13) and not DroppedDown and FInspectorItem.InspectorBar.AutoAdvance then
  begin
    Key := #0;
    if Assigned(FInspectorItem.InspectorBar.FOnEditAutoAdvance) then
      FInspectorItem.InspectorBar.FOnEditAutoAdvance(Self, FInspectorItem.InspectorPanel, FInspectorItem);
    FInspectorItem.InspectorBar.NextEdit(FInspectorItem, VK_RETURN);
  end;
  if Key = #27 then
  begin
    Key := #0;
    Text := FOrigValue;
    SelectAll;
  end;
  inherited;
end;

{ TInspectorColorCombo }

constructor TInspectorColorCombo.Create(AOwner: TComponent);
begin
  inherited;
  Flat := True;
end;

procedure TInspectorColorCombo.DoExit;
begin
  inherited;
  (Parent as TInspectorBar).StopEdit(FInspectorItem);
end;

procedure TInspectorColorCombo.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_RETURN) and not DroppedDown and FInspectorItem.InspectorBar.AutoAdvance then
  begin
    Key := 0;
    if Assigned(FInspectorItem.InspectorBar.FOnEditAutoAdvance) then
      FInspectorItem.InspectorBar.FOnEditAutoAdvance(Self, FInspectorItem.InspectorPanel, FInspectorItem);

    FInspectorItem.InspectorBar.NextEdit(FInspectorItem, VK_RETURN);
  end;

  if (Key in [VK_DOWN, VK_UP]) and not (ssCtrl in Shift) and not DroppedDown then
  begin
    FInspectorItem.InspectorBar.NextEdit(FInspectorItem, Key);
    Key := 0;
  end;
  inherited;
end;

procedure TInspectorColorCombo.KeyPress(var Key: Char);
begin

  if Key = #27 then
  begin
    Key := #0;
    ColorValue := FOrigValue;
    SelectAll;
  end;
  inherited;
end;


{ TInspectorSpin }

constructor TInspectorSpin.Create(AOwner: TComponent);
begin
  inherited;
end;

procedure TInspectorSpin.CreateParams(var Params: TCreateParams);
begin
  inherited;
end;

procedure TInspectorSpin.DoExit;
begin
  inherited;
  (Parent as TInspectorBar).StopEdit(FInspectorItem);
end;

procedure TInspectorSpin.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (Key in [VK_UP, VK_DOWN]) and not (ssCtrl in Shift) then
  begin
    FInspectorItem.InspectorBar.NextEdit(FInspectorItem, Key);
    Key := 0;
    Exit;
  end
  else
    inherited;
end;

procedure TInspectorSpin.KeyPress(var Key: Char);
begin
  if Key = #13 then
  begin
    Key := #0;
    FInspectorItem.InspectorBar.NextEdit(FInspectorItem, VK_RETURN);
  end;
  if Key = #27 then
  begin
    Key := #0;
    Text := FOrigValue;
    SelectAll;
  end;
  inherited;
end;

{ TInspectorEditBtn }

constructor TInspectorEditBtn.Create(AOwner: TComponent);
begin
  inherited;
  BorderStyle := bsNone;
end;

procedure TInspectorEditBtn.CreateParams(var Params: TCreateParams);
begin
  inherited;
end;

procedure TInspectorEditBtn.DoExit;
begin
  inherited;
  (Parent as TInspectorBar).StopEdit(FInspectorItem);
end;

procedure TInspectorEditBtn.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if Key in [VK_DOWN, VK_UP] then
  begin
    FInspectorItem.InspectorBar.NextEdit(FInspectorItem, Key);
    Key := 0;
  end;
  inherited;
end;

procedure TInspectorEditBtn.KeyPress(var Key: Char);
begin
  if Key = #13 then
  begin
    Key := #0;
    FInspectorItem.InspectorBar.NextEdit(FInspectorItem, VK_RETURN);
  end;
  if Key = #27 then
  begin
    Key := #0;
    Text := FOrigValue;
    SelectAll;
  end;
  inherited;

end;

{ TInspectorDateTimePicker }

constructor TInspectorDateTimePicker.Create(AOwner: TComponent);
begin
  inherited;

end;

procedure TInspectorDateTimePicker.CreateParams(var Params: TCreateParams);
begin
  inherited;
end;

procedure TInspectorDateTimePicker.DoExit;
begin
  inherited;
  (Parent as TInspectorBar).StopEdit(FInspectorItem);
end;

procedure TInspectorDateTimePicker.KeyDown(var Key: Word;
  Shift: TShiftState);
begin
  inherited;
  if Key in [VK_DOWN, VK_UP] then
  begin
    FInspectorItem.InspectorBar.NextEdit(FInspectorItem, Key);
    Key := 0;
  end;
  inherited;
end;

procedure TInspectorDateTimePicker.KeyPress(var Key: Char);
begin
  inherited;
  if Key = #13 then
  begin
    Key := #0;
    FInspectorItem.InspectorBar.NextEdit(FInspectorItem, VK_RETURN);
  end;
  if Key = #27 then
  begin
    Key := #0;
    Date := FOrigValue;
  end;
  inherited;
end;



{ TInspectorBarDropTarget }

constructor TInspectorBarDropTarget.Create(AInspectorBar: TInspectorBar);
begin
  inherited Create;
  FInspectorBar := AInspectorBar;
end;

destructor TInspectorBarDropTarget.Destroy;
begin
  inherited;
end;

procedure TInspectorBarDropTarget.DragMouseLeave;
begin
  inherited;
end;

procedure TInspectorBarDropTarget.DragMouseMove(pt: TPoint;
  var Allow: Boolean; DropFormats: TDropFormats);
begin
  inherited;
  Allow := True; //dfURL in DropFormats;
end;

procedure TInspectorBarDropTarget.DropFiles(pt: TPoint; Files: TStrings);
var
  i: Integer;
  DefaultHandler: Boolean;
begin
  inherited;

  for i := 1 to Files.Count do
  begin
    DefaultHandler := True;
    if Assigned(FInspectorBar.OnURLDrop) then
      FInspectorBar.OnURLDrop(FInspectorBar, Files.Strings[i - 1], DefaultHandler);
    if DefaultHandler then
    begin
    end;
  end;
end;

procedure TInspectorBarDropTarget.DropText(pt: TPoint; s: string);
begin
  inherited;
end;

procedure TInspectorBarDropTarget.DropURL(pt: TPoint; s: string);
var
  DefaultHandler: Boolean;
  FInspectorPanel: TInspectorPanel;
begin
  DefaultHandler := True;
  if Assigned(FInspectorBar.OnURLDrop) then
    FInspectorBar.OnURLDrop(FInspectorBar, s, DefaultHandler);

  if DefaultHandler then
  begin
    pt := FInspectorBar.ScreenToClient(pt);
    FInspectorBar.GetPanelAtXY(pt.X, pt.Y, FInspectorPanel);

    if Assigned(FInspectorPanel) then
    begin
      with FInspectorPanel.Items.Add do
      begin
        AutoIcon := True;
        Caption := s;
        URL := s;
      end;
    end;
  end;
end;

// Required for system image list handling

{ TInspectorEditLink }

procedure TInspectorEditLink.CreateEditor;
begin

end;

procedure TInspectorEditLink.DestroyEditor;
begin

end;

procedure TInspectorEditLink.EditExit(Sender: TObject);
begin
  StopEdit(FInspector.FEditItem);

  if (EditStyle = esPopup) and Assigned(FPopupForm) then
  begin
    DestroyEditor;
    FPopupForm.Free;
    FPopupForm := nil;
  end;
end;

procedure TInspectorEditLink.EditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  Allow: Boolean;
begin
  Allow := Key in [VK_LEFT, VK_RIGHT, VK_DOWN, VK_UP, VK_PRIOR, VK_NEXT,
    VK_END, VK_UP, VK_RETURN, VK_ESCAPE];

  if FWantKeyUpDown and (Key in [VK_UP, VK_DOWN]) then Allow := False;
  if FWantKeyLeftRight and (Key in [VK_LEFT, VK_RIGHT]) then Allow := False;
  if FWantKeyHomeEnd and (Key in [VK_UP, VK_END]) then Allow := False;
  if FWantKeyPriorNext and (Key in [VK_PRIOR, VK_NEXT]) then Allow := False;
  if FWantKeyReturn and (Key in [VK_RETURN]) then Allow := False;
  if FWantKeyEscape and (Key in [VK_ESCAPE]) then Allow := False;

  if Allow then
  begin
    if Key = VK_ESCAPE then
      SetOriginalValue;

    if Key = VK_RETURN then
    begin
      FInspector.NextEdit(Inspector.FEditItem, VK_RETURN);
      Key := 0;
    end;

    if Key in [VK_UP, VK_DOWN] then
    begin
      FInspector.NextEdit(FInspector.FEditItem, Key);
      Key := 0;
    end;
  end
  else
    inherited;
end;

procedure TInspectorEditLink.FormExit(Sender: TObject);
begin
  EditExit(Sender);
end;

function TInspectorEditLink.GetEditor: TWinControl;
begin
  Result := nil;
end;

function TInspectorEditLink.GetEditorValue(s: string): string;
begin
  if Assigned(OnSetEditText) then
    OnSetEditText(self, FInspector.FEditItem, s);
  Result := s;
end;

procedure TInspectorEditLink.SetEditorValue(var s: string);
begin
  if Assigned(OnGetEditText) then
    OnGetEditText(self, FInspector.FEditItem, s);
end;

procedure TInspectorEditLink.SetOriginalValue;
begin
end;

procedure TInspectorEditLink.SetProperties;
begin
  if Assigned(FOnSetProperties) then
    FOnSetProperties(Self, R, Item);
end;

procedure TInspectorEditLink.StartEdit;
begin

end;

procedure TInspectorEditLink.StopEdit;
begin

end;

{ TInspectorHelp }

procedure TInspectorHelp.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

constructor TInspectorHelp.Create;
begin
  inherited;
  FFont := TFont.Create;
  FColor := clBtnFace;
  FColorto := clNone;
  FBevelInner := bvLowered;
  FBevelOuter := bvNone;
end;

destructor TInspectorHelp.Destroy;
begin
  FFont.Free;
  inherited;
end;

procedure TInspectorHelp.SetBevelInner(const Value: TBevelCut);
begin
  FBevelInner := Value;
  Changed;
end;

procedure TInspectorHelp.SetBevelOuter(const Value: TBevelCut);
begin
  FBevelOuter := Value;
  Changed;
end;

procedure TInspectorHelp.SetBevelWidth(const Value: Integer);
begin
  FBevelWidth := Value;
  Changed;
end;

procedure TInspectorHelp.SetColor(const Value: TColor);
begin
  FColor := Value;
  Changed;
end;

procedure TInspectorHelp.SetColorTo(const Value: TColor);
begin
  FColorTo := Value;
end;

procedure TInspectorHelp.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
  Changed;
end;

procedure TInspectorHelp.SetHeight(const Value: Integer);
begin
  FHeight := Value;
  Changed;
end;

procedure TInspectorHelp.SetText(const Value: string);
begin
  FText := Value;
  Changed;
end;

procedure TInspectorHelp.SetVisible(const Value: Boolean);
begin
  FVisible := Value;
  Changed;
end;

initialization
  OleInitialize(nil);

finalization
  OleUnInitialize;

end.
