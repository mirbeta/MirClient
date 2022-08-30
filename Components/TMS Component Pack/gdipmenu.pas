{***************************************************************************}
{ GDI+ Menu                                                                 }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2015		                                            }
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

unit GDIPMenu;

{$I TMSDEFS.INC}

interface

uses
  Forms, Messages, Windows, SysUtils, Classes, Graphics, Controls, StdCtrls,
  Comobj, AdvStyleIF, ImgList,
  GDIPPictureContainer, ExtCtrls, Math, GDIPFill,
  AdvSmoothTheme, IniFiles, Menus, Types, AdvGDIP
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;

const
  TEAROFFELLIPSESIZE = 4;
  TEAROFFELLIPSESPACING = 2;

type
  TGDIPMenu = class;

  TGDIPMenuSectionItemAppearance = class;

  TGDIPMenuSection = class;

  TGDIPMenuTopLayerItem = class;

  ITMSMegaMenu = interface
  ['{05B08D30-11B9-48D6-92A0-83904F27F19A}']
    function GetPictureContainer: TGDIPPictureContainer;
    function GetImageList: TCustomImageList;
    function GetDefaultItemAppearance: TGDIPMenuSectionItemAppearance;
    function GetDefaultSection: TGDIPMenuSection;
    function GetDefaultTopLayerItem: TGDIPMenuTopLayerItem;
    function HasMultipleMenus: Boolean;
    function GetNextMenu(Menu: TGDIPMenu): TGDIPMenu;
    function GetPreviousMenu(Menu: TGDIPMenu): TGDIPMenu;
    function GetFirstMenu: TGDIPMenu;
  end;

  TGDIPMenuLocation = (mlTopLeft, mlTopCenter, mlTopRight, mlCenterLeft, mlCenterCenter, mlCenterRight, mlBottomLeft, mlBottomCenter, mlBottomRight, mlCustom);

  TGDIPMenuDropDownLocation = (ddTopLeft, ddTopCenter, ddTopRight,ddLeftCenterTop, ddLeftCenterCenter, ddLeftCenterBottom, ddRightCenterTop, ddRightCenterCenter, ddRightCenterBottom, ddBottomLeft, ddBottomCenter, ddBottomRight, ddCustom);

  TGDIPMenuHTMLText = class(TPersistent)
  private
    FDisableRepaint: Boolean;
    FOwner: TGDIPMenu;
    FURLColor: TColor;
    FShadowOffset: integer;
    FText: string;
    FShadowColor: TColor;
    FOnChange: TNotifyEvent;
    FTop: integer;
    FLeft: integer;
    procedure SetLeft(const Value: integer);
    procedure SetShadowColor(const Value: TColor);
    procedure SetShadowOffset(const Value: integer);
    procedure SetText(const Value: string);
    procedure SetTop(const Value: integer);
    procedure SetURLColor(const Value: TColor);
  protected
    procedure Changed;
    procedure FontChanged(Sender: TObject);
  public
    constructor Create(AOwner: TGDIPMenu);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Text: string read FText write SetText;
    property Top: integer read FTop write SetTop default 0;
    property Left: integer read FLeft write SetLeft default 0;
    property URLColor: TColor read FURLColor write SetURLColor default clBlue;
    property ShadowColor: TColor read FShadowColor write SetShadowColor default clGray;
    property ShadowOffset: integer read FShadowOffset write SetShadowOffset default 5;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TGDIPMenuMargin = class(TPersistent)
  private
    FOwner: TGDIPMenu;
    FRight: integer;
    FBottom: integer;
    FTop: integer;
    FLeft: integer;
    FOnChange: TNotifyEvent;
    procedure SetBottom(const Value: integer);
    procedure SetLeft(const Value: integer);
    procedure SetRight(const Value: integer);
    procedure SetTop(const Value: integer);
  protected
    procedure Changed;
  public
    constructor Create(AOwner: TGDIPMenu);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Left: integer read FLeft write SetLeft default 0;
    property Top: integer read FTop write SetTop default 0;
    property Bottom: integer read FBottom write SetBottom default 0;
    property Right: integer read FRight write SetRight default 0;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TGDIPMenuTopLayerItem = class(TCollectionItem)
  private
    FTopLayerRect: TGPRectF;
    FOwner: TGDIPMenu;
    FAlign: TAlign;
    FWidth: integer;
    FVisible: Boolean;
    FFill: TGDIPFill;
    FTop: integer;
    FHeight: integer;
    FLeft: integer;
    FHTMLText: String;
    FHTMLTextLocation: TGDIPMenuLocation;
    FHTMLTextTop: integer;
    FHTMLTextLeft: integer;
    FHTMLTextFont: TFont;
    procedure SetAlign(const Value: TAlign);
    procedure SetFill(const Value: TGDIPFill);
    procedure SetHeight(const Value: integer);
    procedure SetLeft(const Value: integer);
    procedure SetTop(const Value: integer);
    procedure SetVisible(const Value: Boolean);
    procedure SetWidth(const Value: integer);
    procedure SetHTMLLocation(const Value: TGDIPMenuLocation);
    procedure SetHTMLText(const Value: String);
    procedure SetHTMLTextLeft(const Value: integer);
    procedure SetHTMLTextTop(const Value: integer);
    procedure SetHTMLTextFont(const Value: TFont);
  protected
    procedure Changed;
    procedure HTMLTextChanged(Sender: TObject);
    procedure HTMLFontChanged(Sender: TObject);
    procedure FillChanged(Sender: TObject);
    procedure Draw(g: TGPGraphics; r: TGPRectF; Detailtxt: TGDIPMenuHTMLText);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function GetTopLayerRect: TGPRectF;
    procedure SaveToFile(ini: TIniFile; Section: String);
    procedure LoadFromFile(ini: TIniFile; Section: String);
  published
    property Visible: Boolean read FVisible write SetVisible default true;
    property Top: integer read FTop write SetTop default 0;
    property Left: integer read FLeft write SetLeft default 0;
    property Fill: TGDIPFill read FFill write SetFill;
    property Align: TAlign read FAlign write SetAlign default alCustom;
    property Width: integer read FWidth write SetWidth default 100;
    property Height: integer read FHeight write SetHeight default 100;
    property HTMLText: String read FHTMLText write SetHTMLText;
    property HTMLTextLocation: TGDIPMenuLocation read FHTMLTextLocation write SetHTMLLocation default mlCenterCenter;
    property HTMLTextLeft: integer read FHTMLTextLeft write SetHTMLTextLeft default 0;
    property HTMLTextTop: integer read FHTMLTextTop write SetHTMLTextTop default 0;
    property HTMLTextFont: TFont read FHTMLTextFont write SetHTMLTextFont;
  end;

  TGDIPMenuTopLayerItems = class(TOwnedCollection)
  private
    FOwner: TGDIPMenu;
    FOnChange: TNotifyEvent;
    function GetItem(Index: Integer): TGDIPMenuTopLayerItem;
    procedure SetItem(Index: Integer; const Value: TGDIPMenuTopLayerItem);
  protected
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TWinControl; Menu: TGDIPMenu);
    property Items[Index: Integer]: TGDIPMenuTopLayerItem read GetItem write SetItem; default;
    function Add: TGDIPMenuTopLayerItem;
    function Insert(Index: Integer): TGDIPMenuTopLayerItem;
    procedure Delete(Index: Integer);
  end;

  TGDIPMenuSectionItemType = (itNormal, itHeader, itSeparator, itBreak, itLineSeparator, itLineBreak);

  TGDIPMenuSectionControlType = (ctNone, ctControl, ctCheckBox, ctRadioButton, ctEdit);

  TGDIPMenuSectionItemAppearance = class(TPersistent)
  private
    FOwner: TGDIPMenu;
    FFontDisabled: TFont;
    FFontSelected: TFont;
    FFillDisabled: TGDIPFill;
    FFillSelected: TGDIPFill;
    FFontHover: TFont;
    FFillHover: TGDIPFill;
    FFont: TFont;
    FOnChange: TNotifyEvent;
    FBreakSize: integer;
    FBreakFill: TGDIPFill;
    FSeparatorSize: integer;
    FSeparatorFill: TGDIPFill;
    FFill: TGDIPFill;
    FURLColor: TColor;
    FShadowOffset: integer;
    FShadowColor: TColor;
    FDetailFont: TFont;
    FAllowSelection: Boolean;
    FShortCutFont: TFont;
    procedure SetFillDisabled(const Value: TGDIPFill);
    procedure SetFillHover(const Value: TGDIPFill);
    procedure SetFillSelected(const Value: TGDIPFill);
    procedure SetFontDisabled(const Value: TFont);
    procedure SetFontHover(const Value: TFont);
    procedure SetFontSelected(const Value: TFont);
    procedure SetFont(const Value: TFont);
    procedure SetBreakFill(const Value: TGDIPFill);
    procedure SetBreakWidth(const Value: integer);
    procedure SetSeparatorFill(const Value: TGDIPFill);
    procedure SetSeparatorWidth(const Value: integer);
    procedure SetFill(const Value: TGDIPFill);
    procedure SetShadowColor(const Value: TColor);
    procedure SetShadowOffset(const Value: integer);
    procedure SetURLColor(const Value: TColor);
    procedure SetDetailFont(const Value: TFont);
    procedure SetAllowSelection(const Value: Boolean);
    procedure SetShortCutFont(const Value: TFont);
  protected
    procedure Changed;
    procedure FillChanged(Sender: TObject);
    procedure FontChanged(Sender: TObject);
  public
    constructor Create(AOwner: TGDIPMenu);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure SaveToFile(ini: TIniFile; Section: String);
    procedure LoadFromFile(ini: TIniFile; Section: String);
  published
    property URLColor: TColor read FURLColor write SetURLColor default clBlue;
    property ShadowColor: TColor read FShadowColor write SetShadowColor default clGray;
    property ShadowOffset: integer read FShadowOffset write SetShadowOffset default 5;
    property SeparatorFill: TGDIPFill read FSeparatorFill write SetSeparatorFill;
    property BreakFill: TGDIPFill read FBreakFill write SetBreakFill;
    property SeparatorSize: integer read FSeparatorSize write SetSeparatorWidth default 2;
    property BreakSize: integer read FBreakSize write SetBreakWidth default 2;
    property FillSelected: TGDIPFill read FFillSelected write SetFillSelected;
    property FillDisabled: TGDIPFill read FFillDisabled write SetFillDisabled;
    property FillHover: TGDIPFill read FFillHover write SetFillHover;
    property Fill: TGDIPFill read FFill write SetFill;
    property FontSelected: TFont read FFontSelected write SetFontSelected;
    property FontDisabled: TFont read FFontDisabled write SetFontDisabled;
    property FontHover: TFont read FFontHover write SetFontHover;
    property Font: TFont read FFont write SetFont;
    property ShortCutFont: TFont read FShortCutFont write SetShortCutFont;
    property DetailFont: TFont read FDetailFont write SetDetailFont;
    property AllowSelection: Boolean read FAllowSelection write SetAllowSelection default true;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TGDIPMenuSectionItem = class(TCollectionItem)
  private
    FEdit: TEdit;
    FCount: integer;
    FItemRect, FFillRect: TGPRectF;
    FOwner: TGDIPMenu;
    FSectionOwner: TGDIPMenuSection;
    FControlType: TGDIPMenuSectionControlType;
    FItemType: TGDIPMenuSectionItemType;
    FText: String;
    FData: String;
    FEnabled: Boolean;
    FControl: TControl;
    FHoverText: String;
    FHoverTextLocation: TGDIPMenuLocation;
    FHoverTextTop: integer;
    FHoverTextLeft: integer;
    FChecked: Boolean;
    FGroupIndex: integer;
    FControlIndent: integer;
    FGraphicLeftName: String;
    FGraphicRightName: String;
    FEnableFill: Boolean;
    FSelectedTopLayerItem: integer;
    FHoverTopLayerItem: integer;
    FHeight: integer;
    FTextTop: integer;
    FTextLeft: integer;
    FTextLocation: TGDIPMenuLocation;
    FItemObject: TObject;
    FTag: integer;
    FHideOnSelection: Boolean;
    FShortCut: TShortCut;
    FOnClick: TNotifyEvent;
    FImageIndex: integer;
    FVisible: Boolean;
    procedure SetControlType(const Value: TGDIPMenuSectionControlType);
    procedure SetItemType(const Value: TGDIPMenuSectionItemType);
    procedure SetText(const Value: String);
    procedure SetData(const Value: String);
    procedure SetEnabled(const Value: Boolean);
    procedure SetHoverText(const Value: String);
    procedure SetHoverTextLocation(const Value: TGDIPMenuLocation);
    procedure SetHoverTextLeft(const Value: integer);
    procedure SetHoverTextTop(const Value: integer);
    procedure SetControl(const Value: TControl);
    procedure SetChecked(const Value: Boolean);
    procedure SetGroupIndex(const Value: integer);
    procedure SetGraphicLeftName(const Value: String);
    procedure SetGraphicRightName(const Value: String);
    procedure SetControlIndent(const Value: integer);
    procedure SetEnableFill(const Value: Boolean);
    procedure SetHoverTopLayerItem(const Value: integer);
    procedure SetSelectedTopLayerItem(const Value: integer);
    procedure SetHeight(const Value: integer);
    procedure SetTextLeft(const Value: integer);
    procedure SetTextLocation(const Value: TGDIPMenuLocation);
    procedure SetTextTop(const Value: integer);
    procedure SetItemObject(const Value: TObject);
    procedure SetTag(const Value: integer);
    procedure SetHideOnSelection(const Value: Boolean);
    procedure SetShortCut(const Value: TShortCut);
    procedure SetImageIndex(const Value: integer);
    procedure SetVisible(const Value: Boolean);
  protected
    procedure Changed;
    procedure HTMLChanged(Sender: TObject);
    procedure HoverTextChanged(Sender: TObject);
    procedure ControlItemsChange(Sender: TObject);
    function GetAnchorAt(X, Y: integer): String;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure EditChange(Sender: TObject);
    procedure Assign(Source: TPersistent); override;
    procedure Draw(g: TGPGraphics; rd, r, rf: TGPRectF; DoItemHoverFill, DoItemSelectedFill: Boolean);
    property Edit: TEdit read FEdit write FEdit;
    function Section: TGDIPMenuSection;
    function Menu: TGDIPMenu;
    function ItemRect: TGPRectF;
  published
    property Height: integer read FHeight write SetHeight default 25;
    property GroupIndex: integer read FGroupIndex write SetGroupIndex default 0;
    property Checked: Boolean read FChecked write SetChecked default false;
    property ItemType: TGDIPMenuSectionItemType read FItemType write SetItemType default itNormal;
    property ControlType: TGDIPMenuSectionControlType read FControlType write SetControlType default ctNone;
    property Control: TControl read FControl write SetControl;
    property ControlIndent: integer read FControlIndent write SetControlIndent default 20;
    property Text: String read FText write SetText;
    property TextLocation: TGDIPMenuLocation read FTextLocation write SetTextLocation default mlCenterLeft;
    property TextLeft: integer read FTextLeft write SetTextLeft default 0;
    property TextTop: integer read FTextTop write SetTextTop default 0;
    property Data: String read FData write SetData;
    property EnableFill: Boolean read FEnableFill write SetEnableFill default true;
    property Enabled: Boolean read FEnabled write SetEnabled default true;
    property Visible: Boolean read FVisible write SetVisible default true;
    property DetailText: String read FHoverText write SetHoverText;
    property HoverTopLayerItem: integer read FHoverTopLayerItem write SetHoverTopLayerItem default -1;
    property SelectedTopLayerItem: integer read FSelectedTopLayerItem write SetSelectedTopLayerItem default -1;
    property DetailTextLocation: TGDIPMenuLocation read FHoverTextLocation write SetHoverTextLocation default mlCenterCenter;
    property DetailTextLeft: integer read FHoverTextLeft write SetHoverTextLeft default 0;
    property DetailTextTop: integer read FHoverTextTop write SetHoverTextTop default 0;
    property GraphicLeftName: String read FGraphicLeftName write SetGraphicLeftName;
    property GraphicRightName: String read FGraphicRightName write SetGraphicRightName;
    property Tag: integer read FTag write SetTag default 0;
    property ItemObject: TObject read FItemObject write SetItemObject;
    property HideOnSelection: Boolean read FHideOnSelection write SetHideOnSelection default true;
    property ShortCut: TShortCut read FShortCut write SetShortCut default 0;
    property ImageIndex: integer read FImageIndex write SetImageIndex default -1;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
  end;

  TGDIPMenuSectionItems = class(TOwnedCollection)
  private
    FOwner: TGDIPMenu;
    FSectionOwner: TGDIPMenuSection;
    FOnChange: TNotifyEvent;
    function GetItem(Index: Integer): TGDIPMenuSectionItem;
    procedure SetItem(Index: Integer; const Value: TGDIPMenuSectionItem);
  protected
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TGDIPMenu; ASectionOwner: TGDIPMenuSection);
    property Items[Index: Integer]: TGDIPMenuSectionItem read GetItem write SetItem; default;
    function Add: TGDIPMenuSectionItem;
    function Insert(Index: Integer): TGDIPMenuSectionItem;
    procedure Delete(Index: Integer);
  end;

  TGDIPMenuSection = class(TCollectionItem)
  private
    FBR: integer;
    FAutoWidth, FAutoHeight: Double;
    FCnt: integer;
    FOwner: TGDIPMenu;
    FBackGroundFill: TGDIPFill;
    FCaptionFill: TGDIPFill;
    FItems: TGDIPMenuSectionItems;
    FCaptionSize: integer;
    FCaptionTop: integer;
    FCaptionLeft: integer;
    FCaption: String;
    FCaptionLocation: TGDIPMenuLocation;
    FCaptionFont: TFont;
    FWidth: integer;
    FHeight: integer;
    FItemSpacing: integer;
    FItemMargin: TGDIPMenuMargin;
    FItemRectangleMargin: TGDIPMenuMargin;
    FAutoItemHeight: boolean;
    FVisible: Boolean;
    procedure SetBackGroundFill(const Value: TGDIPFill);
    procedure SetCaptionFill(const Value: TGDIPFill);
    procedure SetItems(const Value: TGDIPMenuSectionItems);
    procedure SetCaptionSize(const Value: integer);
    procedure SetCaption(const Value: String);
    procedure SetCaptionLeft(const Value: integer);
    procedure SetCaptionLocation(const Value: TGDIPMenuLocation);
    procedure SetCaptionTop(const Value: integer);
    procedure SetCaptionFont(const Value: TFont);
    procedure SetHeight(const Value: integer);
    procedure SetWidth(const Value: integer);
    procedure SetItemSpacing(const Value: integer);
    procedure SetItemMargin(const Value: TGDIPMenuMargin);
    procedure SetItemRectangleMargin(const Value: TGDIPMenuMargin);
    procedure SetAutoItemHeight(const Value: boolean);
    procedure SetVisible(const Value: Boolean);
  protected
    procedure Changed;
    procedure ItemsChanged(Sender: TObject);
    procedure FontChanged(Sender: TObject);
    procedure ItemMarginChanged(Sender: TObject);
    function GetContentRect(r: TGPRectF): TGPRectF;
    function GetCaptionRect(r: TGPRectF): TGPRectF;
    function GetSectionRect(r: TGPRectF): TGPRectF;
    function GetSectionInsideRect(r: TGPRectF): TGPRectF;
    procedure BuildItemRects(first: Boolean = false);
    procedure CalculateAutoSize;
  public
    property AutoHeight: Double read FAutoHeight write FAutoHeight;
    property AutoWidth: Double read FAutoWidth write FAutoWidth;

    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Draw(g: TGPGraphics; r: TGPRectF);
    procedure SetComponentStyle(AStyle: TTMSStyle);
    procedure SetColorTones(ATones: TColorTones);
    function GetWidth: Double;
    function GetHeight: Double;
    procedure SaveToFile(ini: TIniFile; Section: String);
    procedure LoadFromFile(ini: TIniFile; Section: String);
  published
    property CaptionFont: TFont read FCaptionFont write SetCaptionFont;
    property Caption: String read FCaption write SetCaption;
    property CaptionLocation: TGDIPMenuLocation read FCaptionLocation write SetCaptionLocation default mlCenterLeft;
    property CaptionLeft: integer read FCaptionLeft write SetCaptionLeft default 0;
    property CaptionTop: integer read FCaptionTop write SetCaptionTop default 0;
    property CaptionSize: integer read FCaptionSize write SetCaptionSize default 20;
    property CaptionFill: TGDIPFill read FCaptionFill write SetCaptionFill;
    property Height: integer read FHeight write SetHeight default 150;
    property Width: integer read FWidth write SetWidth default 250;
    property BackGroundFill: TGDIPFill read FBackGroundFill write SetBackGroundFill;
    property Items: TGDIPMenuSectionItems read FItems write SetItems;
    property AutoItemHeight: boolean read FAutoItemHeight write SetAutoItemHeight;
    property ItemMargin: TGDIPMenuMargin read FItemMargin write SetItemMargin;
    property ItemRectangleMargin: TGDIPMenuMargin read FItemRectangleMargin write SetItemRectangleMargin;
    property ItemSpacing: integer read FItemSpacing write SetItemSpacing default 2;
    property Visible: Boolean read FVisible write SetVisible default True;
  end;

  TGDIPMenuSections = class(TOwnedCollection)
  private
    FMenuOwner: TWinControl;
    FOwner: TGDIPMenu;
    FOnChange: TNotifyEvent;
    function GetItem(Index: Integer): TGDIPMenuSection;
    procedure SetItem(Index: Integer; const Value: TGDIPMenuSection);
  protected
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TWinControl; Menu: TGDIPMenu);
    property Items[Index: Integer]: TGDIPMenuSection read GetItem write SetItem; default;
    function Add: TGDIPMenuSection;
    function Insert(Index: Integer): TGDIPMenuSection;
    procedure Delete(Index: Integer);
  end;

  TGDIPMenuSectionLayout = (slHorizontal, slVertical);

  TGDIPMenuItemCheckChangedEvent = procedure(Sender: TObject; item: TGDIPMenuSectionItem) of object;

  TGDIPMenuItemEditChanged = procedure(Sender: TObject; Text: String; item: TGDIPMenuSectionItem) of object;

  TGDIPMenuItemEvent = procedure(Sender: TObject; Item: TGDIPMenuSectionItem; Text: String) of object;

  TGDIPMenuItemAnchorEvent = procedure(Sender: TObject; Item: TGDIPMenuSectionItem; Anchor: String) of object;

  TCurrentItem = record
    item: TGDIPMenuSectionItem;
    section: TGDIPMenuSection;
  end;

  TGDIPMenu = class(TPersistent)
  private
    Fdestroying: Boolean;
    FNeedsUpdate: Boolean;
    FWinC: TWinControl;
    FIsWinXP, FDesignTime: boolean;
    FHoveredItem: TCurrentItem;
    FSelectedItem: TCurrentItem;
    FRect: TRect;
    FUpdateCount: integer;
    FContentFill: TGDIPFill;
    FOnChange: TNotifyEvent;
    FTopLayerItems: TGDIPMenuTopLayerItems;
    FSections: TGDIPMenuSections;
    FSectionLayout: TGDIPMenuSectionLayout;
    FSectionMargin: TGDIPMenuMargin;
    FPictureContainer: TGDIPPictureContainer;
    FImageList: TCustomImageList;
    FItemAppearance: TGDIPMenuSectionItemAppearance;
    FOnItemClick: TGDIPMenuItemEvent;
    FOnItemHover: TGDIPMenuItemEvent;
    FDropDownTop: integer;
    FDropDownLeft: integer;
    FDropDownLocation: TGDIPMenuDropDownLocation;
    FAutoSectionSize: Boolean;
    FOnItemCheckChanged: TGDIPMenuItemCheckChangedEvent;
    FTearOff: Boolean;
    FTearOffSize: integer;
    FTearOffFill: TGDIPFill;
    FOnItemEditChanged: TGDIPMenuItemEditChanged;
    FOnItemAnchorClick: TGDIPMenuItemAnchorEvent;
    FRootCaption: String;
    function GetImageList: TCustomImageList;
    function GetPictureContainer: TGDIPPictureContainer;
    procedure SetContentFill(const Value: TGDIPFill);
    procedure SetSections(const Value: TGDIPMenuSections);
    procedure SetTopLayerItems(const Value: TGDIPMenuTopLayerItems);
    procedure SetSectionLayout(const Value: TGDIPMenuSectionLayout);
    procedure SetSectionMargin(const Value: TGDIPMenuMargin);
    procedure SetImageList(const Value: TCustomImageList);
    procedure SetPictureContainer(const Value: TGDIPPictureContainer);
    procedure SetItemAppearance(const Value: TGDIPMenuSectionItemAppearance);
    procedure SetDropDownLeft(const Value: integer);
    procedure SetDropDownLocation(const Value: TGDIPMenuDropDownLocation);
    procedure SetDropDownTop(const Value: integer);
    procedure SetAutoSectionSize(const Value: Boolean);
    procedure SetTearOff(const Value: Boolean);
    procedure SetTearOffFill(const Value: TGDIPFill);
    procedure SetTearOffSize(const Value: integer);
    procedure SetSelectedItem(const Value: TCurrentItem);
  protected
    procedure Changed;
    procedure FillChanged(Sender: TObject);
    procedure ItemAppearanceChanged(Sender: TObject);
    procedure TopLayerItemsChanged(Sender: TObject);
    procedure SectionsChanged(Sender: TObject);
    function InsideRect(r: TRect): TRect;
    function GetContentRect(r: TRect): TGPRectF;
    function GetSectionsRect(r: TRect): TGPRectF;
    function GetTearOffRect(r: TRect): TGPRectF;
    procedure GetLocation(var x, y: Double; rectangle: TGPRectF;
      objectwidth, objectheight: Double; location: TGDIPMenuLocation);
    function PtInGPRect(r: TGPRectF; pt: TPoint): Boolean;
    procedure ReBuildTopLayerItems;
    procedure BuildTopLayerItems(r: TGPRectF);
  public
    constructor Create(AOwner: TWinControl);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure SetComponentStyle(AStyle: TTMSStyle);
    procedure SetColorTones(ATones: TColorTones);
    procedure Draw(g: TGPGraphics; r: TRect);
    function XYToTopLayerItem(X, Y: integer): TGDIPMenuTopLayerItem;
    function XYToSectionItem(X, Y: integer): TCurrentItem;
    function XYToSectionItemControl(X, Y: integer): TCurrentItem;
    function GetStrippedHTMLText(txt: String): String;
    function DrawItemHTMLText(g: TGPGraphics; var htmlr: TGPRectF; f: TFont; HTML: TGDIPMenuHTMLText; r: TGPRectF; str: String; Location: TGDIPMenuLocation; TextLeft, TextTop: integer;
      DoCalculate: Boolean = false; DoAnchor: Boolean = false; fX: integer = -1; fY: integer = -1): String;
    function DrawTopLayerHTMLText(g: TGPGraphics; f: TFont; HTML: TGDIPMenuHTMLText; Location: TGDIPMenuLocation; r: TGPRectF; str: String;
      DoAnchor: Boolean = false; fX: integer = -1; fY: integer = -1): String;
    procedure DoMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure DoMouseMove(Shift: TShiftState; X, Y: Integer);
    procedure DoMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure DoCMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure DoAutoSize;
    function UsesControls: Boolean;
    function GetWinOwner: TWinControl;
    procedure Init(r: TRect; Update: Boolean; UpdateRectangle: Boolean);
    property SelectedItem: TCurrentItem read FSelectedItem write SetSelectedItem;
    function HasMultipleMenus: Boolean;
    function GetNextMenu: TGDIPMenu;
    function GetPreviousMenu: TGDIPMenu;
    function GetFirstMenu: TGDIPMenu;
    property NeedsUpdate: Boolean read FNeedsUpdate write FNeedsUpdate;
    property RootCaption: String read FRootCaption write FRootCaption;
  published
    property AutoSectionSize: Boolean read FAutoSectionSize write SetAutoSectionSize default true;
    property ItemAppearance: TGDIPMenuSectionItemAppearance read FItemAppearance write SetItemAppearance;
    property TearOffFill: TGDIPFill read FTearOffFill write SetTearOffFill;
    property TearOffSize: integer read FTearOffSize write SetTearOffSize default 8;
    property ContentFill: TGDIPFill read FContentFill write SetContentFill;
    property SectionMargin: TGDIPMenuMargin read FSectionMargin write SetSectionMargin;
    property Sections: TGDIPMenuSections read FSections write SetSections;
    property DropDownLocation: TGDIPMenuDropDownLocation read FDropDownLocation write SetDropDownLocation default ddBottomRight;
    property DropDownLeft: integer read FDropDownLeft write SetDropDownLeft default 0;
    property DropDownTop: integer read FDropDownTop write SetDropDownTop default 0;
    property SectionLayout: TGDIPMenuSectionLayout read FSectionLayout write SetSectionLayout default slHorizontal;
    property TopLayerItems: TGDIPMenuTopLayerItems read FTopLayerItems write SetTopLayerItems;
    property ImageList: TCustomImageList read GetImageList write SetImageList;
    property PictureContainer: TGDIPPictureContainer read GetPictureContainer write SetPictureContainer;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property TearOff: Boolean read FTearOff write SetTearOff default true;

    property OnItemClick: TGDIPMenuItemEvent read FOnItemClick write FOnItemClick;
    property OnItemAnchorClick: TGDIPMenuItemAnchorEvent read FOnItemAnchorClick write FOnItemAnchorClick;
    property OnItemHover: TGDIPMenuItemEvent read FOnItemHover write FOnItemHover;
    property OnItemCheckChanged: TGDIPMenuItemCheckChangedEvent read FOnItemCheckChanged write FOnItemCheckChanged;
    property OnItemEditChanged: TGDIPMenuItemEditChanged read FOnItemEditChanged write FOnItemEditChanged;
  end;

implementation

uses
  CommCtrl, ShellApi;

{$I GDIPHTMLEngine.pas}

function PtInGPRect(r: TGPRectF; pt: TPoint): Boolean;
begin
  result := ((pt.X >= r.X) and (pt.X <= r.X + r.Width)) and
     ((pt.Y >= r.Y) and (pt.Y <= r.Y + r.Height));
end;

{$IFNDEF DELPHI7_LVL}
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


function GetFontStyles(Style: TFontStyles): String;
var
  str: String;
begin
  str := '';
  if (fsBold in Style) then
    str := str + ':0';
  if (fsItalic in Style) then
    str := str + ':1';
  if (fsUnderline in Style) then
    str := str + ':2';
  if (fsStrikeOut in Style) then
    str := str + ':3';

  Result := str;
end;

procedure Split
   (const Delimiter: Char;
    Input: string;
    const Strings: TStrings) ;
begin
   Assert(Assigned(Strings)) ;
   Strings.Clear;
   Strings.Delimiter := Delimiter;
   Strings.DelimitedText := Input;
end;

procedure SaveFont(ini: TIniFile; Section: String; f: TFont);
begin
  ini.WriteInteger(Section, 'FontSize', f.Size);
  ini.WriteInteger(Section, 'FontColor', f.Color);
  ini.WriteString(Section, 'FontName', f.Name);
  ini.WriteString(Section, 'FontStyle', GetFontStyles(f.Style));
end;

procedure LoadFont(ini: TIniFile; Section: String; f: TFont);
var
  str: String;
  a: TStringList;
  i: integer;
begin
  f.Size := ini.ReadInteger(Section, 'FontSize', f.Size);
  f.Color := ini.ReadInteger(Section, 'FontColor', f.Color);
  f.Name := ini.ReadString(Section, 'FontName', f.Name);
  str := ini.ReadString(Section, 'FontStyle', '');
  A := TStringList.Create;
  Split(':',str, A);
  for I := 1 to A.Count - 1 do
  begin
    f.Style := f.Style + [Graphics.TFontStyle(strtoint(A[I]))];
  end;
  A.Free;
end;

{ TGDIPMenu }

procedure TGDIPMenu.Assign(Source: TPersistent);
begin
  if (Source is TGDIPMenu) then
  begin
    FContentFill.Assign((Source as TGDIPMenu).ContentFill);
    FTopLayerItems.Assign((Source as TGDIPMenu).TopLayerItems);
    FSections.Assign((Source as TGDIPMenu).Sections);
    FSectionMargin.Assign((Source as TGDIPMenu).SectionMargin);
    FSectionLayout := (Source as TGDIPMenu).SectionLayout;
    FItemAppearance.Assign((Source as TGDIPMenu).ItemAppearance);
    FDropDownTop := (Source as TGDIPMenu).DropDownTop;
    FDropDownLeft := (Source as TGDIPMenu).DropDownLeft;
    FDropDownLocation := (Source as TGDIPMenu).DropDownLocation;
    FAutoSectionSize := (Source as TGDIPMenu).AutoSectionSize;
    FTearOffSize := (Source as TGDIPMenu).TearOffSize;
    FTearOffFill.Assign((Source as TGDIPMenu).TearOffFill);
    FTearOff := (Source as TGDIPMenu).TearOff;
    FRootCaption := (Source as TGDIPMenu).RootCaption;
    Changed;
  end;
end;

procedure TGDIPMenu.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TGDIPMenu.BuildTopLayerItems(r: TGPRectF);
var
  i: integer;
begin
  for I := 0 to TopLayerItems.Count - 1 do
  begin
    with TopLayerItems[I] do
    begin
      case Align of
        alNone:;
        alTop: FTopLayerRect := MakeRect(r.X, r.Y, r.Width, FHeight);
        alBottom: FTopLayerRect := MakeRect(r.X, R.Y + r.Height - FHeight, r.Width, FHeight);
        alLeft: FTopLayerRect := MakeRect(r.X, r.Y, FWidth, r.Height);
        alRight: FTopLayerRect := MakeRect(r.Width - FWidth, r.Y, FWidth, r.Height);
        alClient: FTopLayerRect := MakeRect(r.X, R.Y, R.Width, R.Height);
        alCustom: FTopLayerRect := MakeRect(r.X + FLeft, R.Y + FTop, FWidth, FHeight);
      end;
    end;
  end;
end;

procedure TGDIPMenu.Changed;
begin
  if Assigned(FOnChange) and (FUpdateCount = 0) then
    FOnChange(Self);
end;

constructor TGDIPMenu.Create(AOwner: TWinControl);
var
  i: integer;
  tmsif: ITMSMegaMenu;
begin
  Fdestroying := false;
  FWinC := AOwner;
  FContentFill := TGDIPFill.Create;
  FContentFill.OnChange := FillChanged;
  FTopLayerItems := TGDIPMenuTopLayerItems.Create(AOwner, Self);
  FTopLayerItems.OnChange := TopLayerItemsChanged;
  FSections := TGDIPMenuSections.Create(AOwner, Self);
  FSections.OnChange := SectionsChanged;
  FSectionMargin := TGDIPMenuMargin.Create(Self);
  FSectionMargin.OnChange := SectionsChanged;
  FItemAppearance := TGDIPMenuSectionItemAppearance.Create(Self);
  FItemAppearance.OnChange := ItemAppearanceChanged;
  FHoveredItem.item := nil;
  FHoveredItem.section := nil;
  FSelectedItem.item := nil;
  FSelectedItem.section := nil;
  FDropDownTop := 0;
  FDropDownLeft := 0;
  FDropDownLocation := ddBottomRight;
  FAutoSectionSize := true;
  FSectionLayout := slHorizontal;
  FTearOff := true;
  FTearOffSize := 8;
  FTearOffFill := TGDIPFill.Create;
  FTearOffFill.OnChange := FillChanged;

  // app is linked with COMCTL32 v6 or higher -> xp themes enabled
  i := GetFileVersion('COMCTL32.DLL');
  i := (i shr 16) and $FF;

  FIsWinXP := (i > 5);
  if Assigned(FWinC) then
    FDesignTime := (csDesigning in FWinC.ComponentState) and not
      ((csReading in FWinC.ComponentState) or (csLoading in FWinC.ComponentState));

  if FDesignTime then
    SetComponentStyle(tsOffice2007Luna);

  if Assigned(FWinC) then
  begin
    if FWinC.GetInterface(ITMSMegaMenu, tmsif) then
    begin
      if Assigned(tmsif.GetDefaultItemAppearance) then
        ItemAppearance.Assign(tmsif.GetDefaultItemAppearance);
    end;
  end;
end;

destructor TGDIPMenu.Destroy;
begin
  Fdestroying := true;
  FTearOffFill.Free;
  FContentFill.Free;
  FTopLayerItems.Free;
  FSections.Free;
  FSectionMargin.Free;
  FItemAppearance.Free;
  inherited;
end;

procedure TGDIPMenu.DoAutoSize;
var
  i: integer;
begin
  for I := 0 to Sections.Count - 1 do
  begin
    if Sections[I].Visible then
    begin
      with Sections[I] do
      begin
        BuildItemRects(true);
        CalculateAutoSize;
      end;
    end;
  end;
end;

procedure TGDIPMenu.DoCMMouseLeave(var Message: TMessage);
begin
  FHoveredItem.item := nil;
  FHoveredItem.section := nil;
  Changed;
end;

procedure TGDIPMenu.DoMouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin

end;

procedure TGDIPMenu.DoMouseMove(Shift: TShiftState; X, Y: Integer);
var
  it: TCurrentItem;
  a: String;
begin
  inherited;
  it := XYToSectionItem(X, Y);
  if (it.item <> nil) then
  begin
    a := it.item.GetAnchorAt(X, Y);
    if a <> '' then
      Screen.Cursor := crHandPoint
    else
      Screen.Cursor := crDefault;

    if (it.item <> FHoveredItem.item) then
    begin
      FHoveredItem.item := it.Item;
      FHoveredItem.section := it.section;
      if Assigned(OnItemHover) then
        OnItemHover(Self, it.item, it.item.Text);
      Changed;
    end;
  end
  else if Assigned(FHoveredItem.item) then
  begin
    Screen.Cursor := crDefault;
    FHoveredItem.item := nil;
    FHoveredItem.section := nil;
    Changed;
  end
  else
    Screen.Cursor := crDefault;
end;

procedure TGDIPMenu.DoMouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  it, itchk: TCurrentItem;
  a: String;
begin
  inherited;
  itchk := XYToSectionItemControl(X, Y);
  if (itchk.item <> nil) and (itchk.section <> nil)  then
  begin
    case itchk.item.ControlType of
      ctRadioButton, ctCheckBox:
        itchk.item.Checked := not itchk.item.Checked;
    end;
    Changed;
  end
  else
  begin
    it := XYToSectionItem(X, Y);
    if (it.item <> nil) then
    begin
      a := it.item.GetAnchorAt(X, Y);
      if a <> '' then
      begin
        if Assigned(FOnItemAnchorClick) then
          FOnItemAnchorClick(Self, it.item, a);
      end;

      FSelectedItem.item := it.Item;
      FSelectedItem.section := it.section;
      if Assigned(OnItemClick) then
        OnItemClick(Self, it.item, it.item.Text);
      Changed;
    end
    else if Assigned(FSelectedItem.item) then
    begin
      FSelectedItem.item := nil;
      FSelectedItem.section := nil;
      Changed;
    end;
  end;
end;

function Darker(Color:TColor; Percent:Byte):TColor;
var
  r, g, b:Byte;
begin
  Color := ColorToRGB(Color);
  r := GetRValue(Color);
  g := GetGValue(Color);
  b := GetBValue(Color);
  r := r - muldiv(r, Percent, 100);  //Percent% closer to black
  g := g - muldiv(g, Percent, 100);
  b := b - muldiv(b, Percent, 100);
  result := RGB(r, g, b);
end;

procedure TGDIPMenu.Draw(g: TGPGraphics; r: TRect);
var
  I: Integer;
  rcon, tr, r1, r2, R3: TGPRectF;
  b: TGPSolidBrush;
  bl: TGPSolidBrush;
  s, sp: integer;
  rsec: TGPRectF;
  FDetailtxt: TGDIPMenuHTMLText;
begin
  r := Rect(r.Left, r.Top, r.Right - 1, r.Bottom - 1);
  //Content fill
  rcon := GetContentRect(r);
  b := TGPSolidBrush.Create(MakeColor(1, clWhite));
  g.FillRectangle(b, rcon);
  b.free;
  ContentFill.Fill(g, rcon);

  rsec := GetSectionsRect(r);
  for I := 0 to Sections.Count - 1 do
  begin
    if Sections[I].Visible then
      Sections[I].Draw(g, rsec);
  end;

  FDetailtxt := TGDIPMenuHTMLText.Create(nil);
  FDetailtxt.URLColor := ItemAppearance.URLColor;
  FDetailtxt.ShadowColor := ItemAppearance.ShadowColor;
  FDetailtxt.ShadowOffset := ItemAppearance.ShadowOffset;
  for I := 0 to TopLayerItems.Count - 1 do
  begin
    FDetailtxt.Left := TopLayerItems[I].HTMLTextLeft;
    FDetailtxt.Top := TopLayerItems[I].HTMLTextTop;
    TopLayerItems[I].Draw(g, rsec, FDetailtxt);
  end;
  FDetailtxt.Free;

  if TearOff and (TearOffSize > 0) then
  begin
    //tear off
    tr := GetTearOffRect(r);
    FTearOffFill.Fill(g, tr);
    s := TEAROFFELLIPSESIZE;
    sp := TEAROFFELLIPSESPACING;
    r1 := MakeRect(tr.X + (tr.Width - s) / 2, tr.Y + (tr.Height - s) / 2, s, s);
    r2 := MakeRect(tr.X + tr.Width / 2 - (s / 2) - s - sp, tr.Y + (tr.Height - s) / 2, s, s);
    r3 := MakeRect(tr.X + tr.Width / 2 + (s / 2) + sp, tr.Y + (tr.Height - s) / 2, s, s);
    bl := TGPSolidBrush.Create(MakeColor(255, clWhite));
    g.FillEllipse(bl, r1);
    g.FillEllipse(bl, r2);
    g.FillEllipse(bl, r3);
    bl.Free;
    r1 := MakeRect(tr.X + (tr.Width - s) / 2, tr.Y + (tr.Height - s) / 2, s - 2, s - 2);
    r2 := MakeRect(tr.X + tr.Width / 2 - (s / 2) - s - sp, tr.Y + (tr.Height - s) / 2, s - 2, s - 2);
    r3 := MakeRect(tr.X + tr.Width / 2 + (s / 2) + sp, tr.Y + (tr.Height - s) / 2, s - 2, s - 2);
    bl := TGPSolidBrush.Create(MakeColor(255, clBlack));
    g.FillEllipse(bl, r1);
    g.FillEllipse(bl, r2);
    g.FillEllipse(bl, r3);
    bl.Free;
    //
  end; 
end;

function TGDIPMenu.DrawItemHTMLText(g: TGPGraphics; var htmlr: TGPRectF; f: TFont; HTML: TGDIPMenuHTMLText;
  r: TGPRectF; str: String; Location: TGDIPMenuLocation; TextLeft, TextTop: integer; DoCalculate, DoAnchor: Boolean; fX, fY: integer): String;
var
  a, s, k: String;
  l, m, XSize, YSize: integer;
  xs, ys: Double;
  x, y: Double;
  hr, htmlrect: TRect;
  UseHTML: Boolean;      
  cf: TGPFont;
  ff: TGPFontFamily;
  fs: integer;
  sf: TGPStringFormat;
  b: TGPSolidBrush;
  sizerect: TGPRectF;
begin
  with HTML do
  begin
    if str <> '' then
    begin
      UseHTML := (Pos('</', str) > 0) or (Pos('/>', str) > 0) or (Pos('<BR>',uppercase(str)) > 0);

      cf := nil;
      ff := nil;
      sf := nil;

      htmlrect := Bounds(Round(r.X), Round(r.Y), Round(r.Width), Round(r.Height));

      if UseHTML then
      begin
        HTMLDrawGDIP(g, f, str,htmlrect,ImageList, 0,0,-1,-1,FShadowOffset,False,true,false,false,
          False,False,true,1.0,FURLColor,clNone,clNone,FShadowColor,a,s,k,XSize,YSize,l,m,hr,nil,PictureContainer,2);
        xs := xsize;
        ys := ysize;
      end          
      else
      begin
        ff := TGPFontFamily.Create(f.Name);
        if (ff.Status in [FontFamilyNotFound, FontStyleNotFound]) then
        begin
          ff.Free;
          ff := TGPFontFamily.Create('Arial');
        end;

        fs := 0;
        if (fsBold in f.Style) then
          fs := fs + 1;
        if (fsItalic in f.Style) then
          fs := fs + 2;
        if (fsUnderline in f.Style) then
          fs := fs + 4;

        sf := TGPStringFormat.Create(0);
        sf.SetHotkeyPrefix(HotkeyPrefixShow);
        cf := TGPFont.Create(ff, f.Size , fs, UnitPoint);
        g.MeasureString(str, Length(str), cf, r, sf, sizerect);

        // correction for GDI+ measurestring issue
        sizerect.Width := sizerect.Width + 10;

        xs := sizerect.Width;
        ys := sizerect.Height;      
      end;

      if DoCalculate then
      begin
        htmlr := MakeRect(r.X, r.Y, xs, ys);
        if not UseHTML then
        begin
          cf.Free;
          ff.Free;
          sf.Free;
        end;
        Exit;
      end;

      if Location <> mlCustom then
        GetLocation(x, y, r, xs, ys, Location)
      else
      begin
        x := TextLeft + htmlr.x;
        y := TextTop + htmlr.y;
      end;

      htmlr := MakeRect(x, y, xs, ys);

      htmlrect := Bounds(Round(r.X), Round(r.Y), Round(r.Width), Round(r.Height));

      if UseHTML then     
      begin
        HTMLDrawGDIP(g, f, str,htmlrect,ImageList, fx,fy,-1,-1,FShadowOffset,DoAnchor,false,false,false,
          False,False,true,1.0,FURLColor,clNone,clNone,FShadowColor,a,s,k,XSize,YSize,l,m,hr,nil,PictureContainer,2);
      end
      else
      begin
        b := TGPSolidBrush.Create(MakeColor(255, f.Color));
        g.DrawString(str, Length(str), cf, htmlr, sf, b);
        b.Free;
        cf.Free;            
        ff.Free;          
        sf.Free;        
      end;
      result := a;
    end;
  end;
end;

function TGDIPMenu.DrawTopLayerHTMLText(g: TGPGraphics; f: TFont; HTML: TGDIPMenuHTMLText;
  Location: TGDIPMenuLocation; r: TGPRectF; str: String; DoAnchor: Boolean; fX,
  fY: integer): String;
var
  htmlr: TRect;
  a, s, k: String;
  l, m, XSize, YSize: integer;
  hr: TRect;
  x, y: Double;
begin
  if str <> '' then
  begin
    htmlr := Rect(0, 0, Round(r.Width), Round(r.Height));

    HTMLDrawGDIP(g, f, str,htmlr, ImageList, 0,0,-1,-1,ItemAppearance.ShadowOffset,False,true,false,false,
      False,False,true,1.0,ItemAppearance.URLColor,clNone,clNone,ItemAppearance.ShadowColor,a,s,k,XSize,YSize,l,m,hr,nil,PictureContainer,2);

    if Location <> mlCustom then
      GetLocation(x, y, r, XSize, YSize, Location)
    else
    begin
      x := HTML.Left;
      y := HTML.Top;
    end;

    htmlr := Bounds(Round(x), Round(y), xsize, ysize);

    HTMLDrawGDIP(g, f, str,htmlr,ImageList, fx,fy,-1,-1,ItemAppearance.ShadowOffset,DoAnchor,false,false,false,
      False,False,true,1.0,ItemAppearance.URLColor,clNone,clNone,ItemAppearance.ShadowColor,a,s,k,XSize,YSize,l,m,hr,nil,PictureContainer,2);

    result := a;
  end;
end;

procedure TGDIPMenu.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount = 0 then
    Changed;
end;

procedure TGDIPMenu.FillChanged(Sender: TObject);
begin
  Changed;
end;

function TGDIPMenu.GetContentRect(r: TRect): TGPRectF;
begin
  Result.X := r.Left;
  Result.Y := r.Top;
  Result.Height := r.Bottom - r.Top;
  result.Width := r.Right - r.Left;
end;

function TGDIPMenu.GetFirstMenu: TGDIPMenu;
var
  tmsif: ITMSMegaMenu;
begin
  if Assigned(FWinc) then
  begin
    if FWinC.GetInterface(ITMSMegaMenu, tmsif) then
      Result := tmsif.GetFirstMenu
    else
      result := nil;
  end
  else
    Result := nil;
end;

function TGDIPMenu.GetImageList: TCustomImageList;
var
  tmsif: ITMSMegaMenu;
begin
   if Assigned(FWinc) then
   begin
     if FWinC.GetInterface(ITMSMegaMenu, tmsif) then
       Result := tmsif.GetImageList
     else
       Result := FImageList;
   end
   else
     Result := FImageList;
end;

procedure TGDIPMenu.GetLocation(var x, y: Double; rectangle: TGPRectF;
  objectwidth, objectheight: Double; location: TGDIPMenuLocation);
var
  w, h, tw, th: Double;
begin
  tw := objectwidth;
  th := objectheight;
  w := rectangle.Width;
  h := rectangle.Height;
  case location of
    mlTopLeft:
    begin
      x := 0;
      y := 0;
    end;
    mlTopRight:
    begin
      x := w - tw;
      y := 0;
    end;
    mlBottomLeft:
    begin
      x := 0;
      y := h - th;
    end;
    mlBottomRight:
    begin
      x := w - tw;
      y := h - th;
    end;
    mlTopCenter:
    begin
      x := (w - tw) / 2;
      y := 0;
    end;
    mlBottomCenter:
    begin
      x := (w - tw) / 2;
      y := h - th;
    end;
    mlCenterCenter:
    begin
      x := (w - tw) / 2;
      y := (h - th) / 2;
    end;
    mlCenterLeft:
    begin
      x := 0;
      y := (h - th) / 2;
    end;
    mlCenterRight:
    begin
      x := w - tw;
      y := (h - th) / 2;
    end;
  end;

  x := x + rectangle.X;
  y := y + rectangle.Y;
end;

function TGDIPMenu.GetNextMenu: TGDIPMenu;
var
  tmsif: ITMSMegaMenu;
begin
  if Assigned(FWinc) then
  begin
    if FWinC.GetInterface(ITMSMegaMenu, tmsif) then
      Result := tmsif.GetNextMenu(Self)
    else
      result := nil;
  end
  else
    Result := nil;
end;

function TGDIPMenu.GetPictureContainer: TGDIPPictureContainer;
var
  tmsif: ITMSMegaMenu;
begin
   if Assigned(FWinc) then
   begin
     if FWinC.GetInterface(ITMSMegaMenu, tmsif) then
       Result := tmsif.GetPictureContainer
     else
       Result := FPictureContainer;
   end
   else
     Result := FPictureContainer;
end;

function TGDIPMenu.GetPreviousMenu: TGDIPMenu;
var
  tmsif: ITMSMegaMenu;
begin
  if Assigned(FWinc) then
  begin
    if FWinC.GetInterface(ITMSMegaMenu, tmsif) then
      Result := tmsif.GetPreviousMenu(Self)
    else
      result := nil;
  end
  else
    Result := nil;
end;

function TGDIPMenu.GetSectionsRect(r: TRect): TGPRectF;
var
  rcon: TGPRectF;
  sr: TRect;
begin
  sr := InsideRect(r);
  rcon := GetContentRect(r);

  if TearOff then
    result := MakeRect(sr.Left, sr.Top + TearOffSize, sr.Right, sr.Bottom - TearOffSize)
  else
    result := MakeRect(sr.Left, sr.Top, sr.Right, sr.Bottom);
end;

function TGDIPMenu.GetStrippedHTMLText(txt: String): String;
var
  g: TGPGraphics;
  a, s, k: string;
  XSize, YSize: integer;
  l, m: integer;
  hr: TRect;
  bmp: TBitmap;
  f: TFont;
begin
  Result := '';
  bmp := TBitmap.Create;
  g := TGPGraphics.Create(bmp.Canvas.Handle);
  f := TFont.Create;
  try
    HTMLDrawGDIP(g, f, txt,Rect(0, 0, 0, 0),nil, 0,0,-1,-1,ItemAppearance.ShadowOffset,False,true,false,false,
      False,False,true,1.0,ItemAppearance.URLColor,clNone,clNone,ItemAppearance.ShadowColor,a,s,k,XSize,YSize,l,m,hr,nil,nil,2);
    Result := s;
  finally
    f.Free;
    g.Free;
    bmp.free;
  end;
end;

function TGDIPMenu.GetTearOffRect(r: TRect): TGPRectF;
var
  rcon: TGPRectF;
  sr: TRect;
begin
  sr := InsideRect(r);
  rcon := GetContentRect(r);
  result := MakeRect(sr.Left, sr.Top, sr.Right, TearOffSize)
end;

function TGDIPMenu.GetWinOwner: TWinControl;
begin
  Result := FWinC;
end;

function TGDIPMenu.HasMultipleMenus: Boolean;
var
  tmsif: ITMSMegaMenu;
begin
  if Assigned(FWinc) then
  begin
    if FWinC.GetInterface(ITMSMegaMenu, tmsif) then
      Result := tmsif.HasMultipleMenus
    else
      result := false;
  end
  else
    Result := false;
end;

procedure TGDIPMenu.Init(r: TRect; Update: Boolean; UpdateRectangle: Boolean);
var
  i: integer;
begin
  if UpdateRectangle then
  begin
    r := Rect(r.Left, r.Top, r.Right - 1, r.Bottom - 1);
    FRect := r;
  end;

  for I := 0 to Sections.Count - 1 do
  begin
    if Sections[I].Visible then
    begin
      with Sections[I] do
      begin
        if AutoSectionSize then
        begin
          if FAutoWidth = 0 then
            FAutoWidth := r.Right - r.Left - 2;

          if FAutoHeight = 0 then
            FAutoHeight := r.Bottom - r.Top - 1;
        end;

        BuildItemRects;
      end;
    end;
  end;

  BuildTopLayerItems(GetSectionsRect(r));

  if Update then
    Changed;
end;

function TGDIPMenu.InsideRect(r: TRect): TRect;
var
  sh, bw: integer;
begin
  sh := 0;
  if (ContentFill.ShadowColor <> clNone) {and not Transparent} then
    sh := ContentFill.ShadowOffset;

  Result := r;
  // adapt width & height for GDI+ drawing rect

  Result.Right := Result.Right - 1 - sh;
  Result.Bottom := Result.Bottom - 1 - sh;

  if (ContentFill.BorderColor <> clNone) {and not Transparent} then
  begin
    if ContentFill.BorderWidth = 1 then
      bw := 1
    else
      bw := (ContentFill.BorderWidth + 1) div 2;

    InflateRect(Result, -bw, -bw);
  end;

  Result.Left := Result.Left + SectionMargin.Left;
  Result.Right := Result.Right - SectionMargin.Right;
  Result.Top := Result.Top + SectionMargin.Top;
  Result.Bottom := Result.Bottom - SectionMargin.Bottom;
end;

procedure TGDIPMenu.ItemAppearanceChanged(Sender: TObject);
begin
  Changed;
end;

function TGDIPMenu.PtInGPRect(r: TGPRectF; pt: TPoint): Boolean;
begin
  result := ((pt.X >= r.X) and (pt.X <= r.X + r.Width)) and
     ((pt.Y >= r.Y) and (pt.Y <= r.Y + r.Height));
end;

procedure TGDIPMenu.ReBuildTopLayerItems;
begin
  BuildTopLayerItems(GetSectionsRect(FRect));
end;

procedure TGDIPMenu.SectionsChanged(Sender: TObject);
begin
  Changed;
end;

procedure TGDIPMenu.SetAutoSectionSize(const Value: Boolean);
begin
  if FAutoSectionSize <> value then
  begin
    FAutoSectionSize := Value;
    Changed;
  end;
end;

procedure TGDIPMenu.SetColorTones(ATones: TColorTones);
var
  i: integer;
begin
  TearOffFill.Color := ATones.Foreground.BrushColor;
  TearOffFill.ColorTo := ATones.Foreground.BrushColor;
  TearOffFill.BorderColor := ATones.Foreground.BorderColor;

  ItemAppearance.Font.Color := ATones.Foreground.TextColor;
  ItemAppearance.FontHover.Color := ATones.Hover.TextColor;
  ItemAppearance.FontSelected.Color := ATones.Selected.TextColor;
  ItemAppearance.Fill.Glow := gmNone;
  ItemAppearance.FillSelected.Glow := gmNone;
  ItemAppearance.FillHover.Glow := gmNone;
  ItemAppearance.FillSelected.GlowGradientColor:= clWhite;

  ContentFill.Color := ATones.Background.BrushColor;
  ContentFill.ColorTo := ATones.Background.BrushColor;
  ContentFill.BorderColor := clNone;

  ItemAppearance.FillHover.Color := ATones.Hover.BrushColor;
  ItemAppearance.FillHover.ColorTo := ATones.Hover.BrushColor;
  ItemAppearance.FillHover.ColorMirror := ATones.Hover.BrushColor;
  ItemAppearance.FillHover.ColorMirrorTo := ATones.Hover.BrushColor;
  ItemAppearance.FillHover.BorderColor :=  ATones.Hover.BorderColor;
  ItemAppearance.FillHover.GradientMirrorType := gtVertical;

  ItemAppearance.FillSelected.Color := ATones.Selected.BrushColor;
  ItemAppearance.FillSelected.ColorTo := ATones.Selected.BrushColor;
  ItemAppearance.FillSelected.ColorMirror := ATones.Selected.BrushColor;
  ItemAppearance.FillSelected.ColorMirrorTo := ATones.Selected.BrushColor;
  ItemAppearance.FillSelected.BorderColor := ATones.Selected.BorderColor;
  ItemAppearance.FillSelected.GradientMirrorType := gtVertical;

  ItemAppearance.FillDisabled.ColorTo := ATones.Disabled.BrushColor;
  ItemAppearance.FillDisabled.ColorMirror := ATones.Disabled.BrushColor;
  ItemAppearance.FillDisabled.ColorMirrorTo := ATones.Disabled.BrushColor;
  ItemAppearance.FontDisabled.Color := ATones.Disabled.BrushColor;
  ItemAppearance.FillDisabled.GradientMirrorType := gtVertical;

  for I := 0 to Sections.Count - 1 do
    Sections[I].SetColorTones(ATones);

end;

procedure TGDIPMenu.SetComponentStyle(AStyle: TTMSStyle);
var
  i: integer;
begin
  ItemAppearance.Font.Color := clBlack;
  ItemAppearance.FontHover.Color := clBlack;
  ItemAppearance.FontSelected.Color := clBlack;
  ItemAppearance.Fill.Glow := gmNone;
  ItemAppearance.FillSelected.Glow := gmNone;
  ItemAppearance.FillHover.Glow := gmNone;
  ItemAppearance.FillSelected.GlowGradientColor:= clWhite;
  // TODO : do color settings here
 case AStyle of
    tsOffice2003Blue:
      begin
        ContentFill.Color := $00FFD2AF;
        ContentFill.ColorTo := $00FFD2AF;
        ContentFill.BorderColor := clNone;

        ItemAppearance.FillHover.Color := $EBFDFF;
        ItemAppearance.FillHover.ColorTo := $ACECFF;
        ItemAppearance.FillHover.ColorMirror := $59DAFF;
        ItemAppearance.FillHover.ColorMirrorTo := $A4E9FF;
        ItemAppearance.FillHover.BorderColor :=  $99CEDB;
        ItemAppearance.FillHover.GradientMirrorType := gtVertical;

        ItemAppearance.FillSelected.Color := $AAD9FF;
        ItemAppearance.FillSelected.ColorTo := $6EBBFF;
        ItemAppearance.FillSelected.ColorMirror := $42AEFE;
        ItemAppearance.FillSelected.ColorMirrorTo := $7AE1FE;
        ItemAppearance.FillSelected.BorderColor := $42AEFE;
        ItemAppearance.FillSelected.GradientMirrorType := gtVertical;

        ItemAppearance.FillDisabled.Color := $00F2F2F2;
        ItemAppearance.FillDisabled.ColorTo := $00B6B6B6;
        ItemAppearance.FillDisabled.ColorMirror := clNone;
        ItemAppearance.FillDisabled.ColorMirrorTo := clNone;
        ItemAppearance.FontDisabled.Color := clGray;
        ItemAppearance.FillDisabled.GradientMirrorType := gtVertical;

      end;
    tsOffice2003Silver:
      begin
        ContentFill.Color := $00E6D8D8;
        ContentFill.ColorTo := $00E6D8D8;
        ContentFill.BorderColor := clNone;

        ItemAppearance.FillHover.Color := $EBFDFF;
        ItemAppearance.FillHover.ColorTo := $ACECFF;
        ItemAppearance.FillHover.ColorMirror := $59DAFF;
        ItemAppearance.FillHover.ColorMirrorTo := $A4E9FF;
        ItemAppearance.FillHover.BorderColor :=  $99CEDB;
        ItemAppearance.FillHover.GradientMirrorType := gtVertical;

        ItemAppearance.FillDisabled.Color := $00F2F2F2;
        ItemAppearance.FillDisabled.ColorTo := $00B6B6B6;
        ItemAppearance.FillDisabled.ColorMirror := clNone;
        ItemAppearance.FillDisabled.ColorMirrorTo := clNone;
        ItemAppearance.fontDisabled.Color := clGray;
        ItemAppearance.FillDisabled.GradientMirrorType := gtVertical;

        ItemAppearance.FillSelected.Color := $AAD9FF;
        ItemAppearance.FillSelected.ColorTo := $6EBBFF;
        ItemAppearance.FillSelected.ColorMirror := $42AEFE;
        ItemAppearance.FillSelected.ColorMirrorTo := $7AE1FE;
        ItemAppearance.FillSelected.BorderColor := $42AEFE;
        ItemAppearance.FillSelected.GradientMirrorType := gtVertical;
      end;
    tsOffice2003Olive:
      begin
        ContentFill.Color := RGB(225, 234, 185);
        ContentFill.ColorTo := RGB(225, 234, 185);
        ContentFill.BorderColor := clNone;

        ItemAppearance.FillDisabled.Color := $00F2F2F2;
        ItemAppearance.FillDisabled.ColorTo := $00B6B6B6;
        ItemAppearance.FillDisabled.ColorMirror := clNone;
        ItemAppearance.FillDisabled.ColorMirrorTo := clNone;
        ItemAppearance.FontDisabled.Color := clGray;
        ItemAppearance.FillDisabled.GradientMirrorType := gtVertical;

        ItemAppearance.FillHover.Color := $EBFDFF;
        ItemAppearance.FillHover.ColorTo := $ACECFF;
        ItemAppearance.FillHover.ColorMirror := $59DAFF;
        ItemAppearance.FillHover.ColorMirrorTo := $A4E9FF;
        ItemAppearance.FillHover.BorderColor :=  $99CEDB;
        ItemAppearance.FillHover.GradientMirrorType := gtVertical;

        ItemAppearance.FillSelected.Color := $AAD9FF;
        ItemAppearance.FillSelected.ColorTo := $6EBBFF;
        ItemAppearance.FillSelected.ColorMirror := $42AEFE;
        ItemAppearance.FillSelected.ColorMirrorTo := $7AE1FE;
        ItemAppearance.FillSelected.BorderColor := $42AEFE;
        ItemAppearance.FillSelected.GradientMirrorType := gtVertical;
      end;
    tsOffice2003Classic:
      begin
        ContentFill.Color := $00F2F2F2;
        ContentFill.ColorTo := $00F2F2F2;
        ContentFill.BorderColor := clNone;

        ItemAppearance.FillHover.Color := $D2BDB6;
        ItemAppearance.FillHover.ColorTo := $D2BDB6;
        ItemAppearance.FillHover.ColorMirror := clNone;
        ItemAppearance.FillHover.ColorMirrorTo := clNone;
        ItemAppearance.FillHover.BorderColor := $808080;
        ItemAppearance.FillHover.GradientMirrorType := gtVertical;

        ItemAppearance.FillDisabled.Color := $D8D5D4;
        ItemAppearance.FillDisabled.ColorTo := $D8D5D4;
        ItemAppearance.FillDisabled.ColorMirror := clNone;
        ItemAppearance.FillDisabled.ColorMirrorTo := clNone;
        ItemAppearance.FontDisabled.Color := clGray;
        ItemAppearance.FillDisabled.GradientMirrorType := gtVertical;

        ItemAppearance.FillSelected.Color := $B59285;
        ItemAppearance.FillSelected.ColorTo := $B59285;
        ItemAppearance.FillSelected.ColorMirror := clNone;
        ItemAppearance.FillSelected.ColorMirrorTo := clNone;
        ItemAppearance.FillSelected.BorderColor := $808080;
        ItemAppearance.FillSelected.GradientMirrorType := gtVertical;

      end;
    tsOffice2007Luna:
      begin
        ContentFill.Color := $00F3E5DA;
        ContentFill.ColorTo := $00F0DED0;
        ContentFill.BorderColor := clNone;

        ItemAppearance.FillHover.Color := $EBFDFF;
        ItemAppearance.FillHover.ColorTo := $ACECFF;
        ItemAppearance.FillHover.ColorMirror := $59DAFF;
        ItemAppearance.FillHover.ColorMirrorTo := $A4E9FF;
        ItemAppearance.FillHover.BorderColor :=  $99CEDB;
        ItemAppearance.FillHover.GradientMirrorType := gtVertical;

        ItemAppearance.FillDisabled.Color := $00F2F2F2;
        ItemAppearance.FillDisabled.ColorTo := $00B6B6B6;
        ItemAppearance.FillDisabled.ColorMirror := $00B6B6B6;
        ItemAppearance.FillDisabled.ColorMirrorTo := $00F2F2F2;
        ItemAppearance.FontDisabled.Color := clGray;
        ItemAppearance.FillDisabled.GradientMirrorType := gtVertical;

        ItemAppearance.FillSelected.Color := $AAD9FF;
        ItemAppearance.FillSelected.ColorTo := $6EBBFF;
        ItemAppearance.FillSelected.ColorMirror := $42AEFE;
        ItemAppearance.FillSelected.ColorMirrorTo := $7AE1FE;
        ItemAppearance.FillSelected.BorderColor := $42AEFE;
        ItemAppearance.FillSelected.GradientMirrorType := gtVertical;

      end;
    tsOffice2007Obsidian:
      begin
        ContentFill.Color := $5C534C;
        ContentFill.ColorTo := $5C534C;
        ContentFill.BorderColor := clNone;

        ItemAppearance.Font.Color := clWhite;
        ItemAppearance.FillHover.Color := $EBFDFF;
        ItemAppearance.FillHover.ColorTo := $ACECFF;
        ItemAppearance.FillHover.ColorMirror := $59DAFF;
        ItemAppearance.FillHover.ColorMirrorTo := $A4E9FF;
        ItemAppearance.FillHover.BorderColor :=  $99CEDB;
        ItemAppearance.FillHover.GradientMirrorType := gtVertical;
        ItemAppearance.FontHover.Color := clBlack;

        ItemAppearance.FillDisabled.Color := $00F2F2F2;
        ItemAppearance.FillDisabled.ColorTo := $00B6B6B6;
        ItemAppearance.FillDisabled.ColorMirror := $00B6B6B6;
        ItemAppearance.FillDisabled.ColorMirrorTo := $00F2F2F2;
        ItemAppearance.FontDisabled.Color := clGray;
        ItemAppearance.FillDisabled.GradientMirrorType := gtVertical;

        ItemAppearance.FillSelected.Color := $AAD9FF;
        ItemAppearance.FillSelected.ColorTo := $6EBBFF;
        ItemAppearance.FillSelected.ColorMirror := $42AEFE;
        ItemAppearance.FillSelected.ColorMirrorTo := $7AE1FE;
        ItemAppearance.FillSelected.BorderColor := $42AEFE;
        ItemAppearance.FontSelected.Color := clBlack;
        ItemAppearance.FillSelected.GradientMirrorType := gtVertical;

      end;
    tsWindowsXP:
      begin
        ContentFill.Color := $00B6B6B6;
        ContentFill.ColorTo := $00B6B6B6;

        ItemAppearance.FillDisabled.Color := $00B6B6B6;
        ItemAppearance.FillDisabled.ColorTo := $00B6B6B6;
        ItemAppearance.FillDisabled.ColorMirror := clNone;
        ItemAppearance.FillDisabled.ColorMirrorTo := clNone;
        ItemAppearance.FontDisabled.Color := clGray;
        ItemAppearance.FillDisabled.GradientMirrorType := gtVertical;

        ItemAppearance.FillHover.Color := $EFD3C6;
        ItemAppearance.FillHover.ColorTo := $EFD3C6;
        ItemAppearance.FillHover.ColorMirror := clNone;
        ItemAppearance.FillHover.ColorMirrorTo := clNone;
        ItemAppearance.FillHover.BorderColor :=  clHighlight;
        ItemAppearance.FillHover.GradientMirrorType := gtVertical;

        ItemAppearance.FillSelected.Color := clInactiveCaption;
        ItemAppearance.FillSelected.ColorTo := clInactiveCaption;
        ItemAppearance.FillSelected.ColorMirror := clNone;
        ItemAppearance.FillSelected.ColorMirrorTo := clNone;
        ItemAppearance.FillSelected.BorderColor := clHighLight;
        ItemAppearance.FillSelected.GradientMirrorType := gtVertical;
      end;
    tsWhidbey:
      begin
        ContentFill.Color := $F5F9FA;
        ContentFill.ColorTo := $F5F9FA;
        ContentFill.BorderColor := clNone;

        ItemAppearance.FillDisabled.Color := $00F2F2F2;
        ItemAppearance.FillDisabled.ColorTo := $00B6B6B6;
        ItemAppearance.FillDisabled.ColorMirror := clNone;
        ItemAppearance.FillDisabled.ColorMirrorTo := clNone;
        ItemAppearance.FontDisabled.Color := clGray;
        ItemAppearance.FillDisabled.GradientMirrorType := gtVertical;

        ItemAppearance.FillHover.Color := $EBFDFF;
        ItemAppearance.FillHover.ColorTo := $ACECFF;
        ItemAppearance.FillHover.ColorMirror := $59DAFF;
        ItemAppearance.FillHover.ColorMirrorTo := $A4E9FF;
        ItemAppearance.FillHover.BorderColor :=  $99CEDB;
        ItemAppearance.FillHover.GradientMirrorType := gtVertical;

        ItemAppearance.FillSelected.Color := $AAD9FF;
        ItemAppearance.FillSelected.ColorTo := $6EBBFF;
        ItemAppearance.FillSelected.ColorMirror := $42AEFE;
        ItemAppearance.FillSelected.ColorMirrorTo := $7AE1FE;
        ItemAppearance.FillSelected.BorderColor := $42AEFE;
        ItemAppearance.FillSelected.GradientMirrorType := gtVertical;
      end;
    tsCustom: ;
    tsOffice2007Silver:
      begin
        ContentFill.Color := RGB(241, 244, 248);
        ContentFill.ColorTo := RGB(227, 232, 240);
        ContentFill.BorderColor := clNone;

        ItemAppearance.FillHover.Color := $EBFDFF;
        ItemAppearance.FillHover.ColorTo := $ACECFF;
        ItemAppearance.FillHover.ColorMirror := $59DAFF;
        ItemAppearance.FillHover.ColorMirrorTo := $A4E9FF;
        ItemAppearance.FillHover.BorderColor :=  $99CEDB;
        ItemAppearance.FillHover.GradientMirrorType := gtVertical;

        ItemAppearance.FillSelected.Color := $AAD9FF;
        ItemAppearance.FillSelected.ColorTo := $6EBBFF;
        ItemAppearance.FillSelected.ColorMirror := $42AEFE;
        ItemAppearance.FillSelected.ColorMirrorTo := $7AE1FE;
        ItemAppearance.FillSelected.BorderColor := $42AEFE;
        ItemAppearance.FillSelected.GradientMirrorType := gtVertical;

        ItemAppearance.FillDisabled.Color := $00F2F2F2;
        ItemAppearance.FillDisabled.ColorTo := $00B6B6B6;
        ItemAppearance.FillDisabled.ColorMirror := $00B6B6B6;
        ItemAppearance.FillDisabled.ColorMirrorTo := $00F2F2F2;
        ItemAppearance.FontDisabled.Color := clGray;
        ItemAppearance.FillDisabled.GradientMirrorType := gtVertical;
      end;
    tsWindowsVista:
      begin
        ContentFill.Color := $F7DAA2;
        ContentFill.ColorTo := $F5D089;
        ContentFill.BorderColor := clNone;

        ItemAppearance.FillHover.Color := $FFFDF9;
        ItemAppearance.FillHover.ColorTo := $FFFAF0;
        ItemAppearance.FillHover.ColorMirror := clNone;
        ItemAppearance.FillHover.ColorMirrorTo := clNone;
        ItemAppearance.FillHover.BorderColor :=  $FCF2DA;
        ItemAppearance.FillHover.GradientMirrorType := gtVertical;

        ItemAppearance.FillSelected.Color := $FEF9F0;
        ItemAppearance.FillSelected.ColorTo := $FDF0D7;
        ItemAppearance.FillSelected.ColorMirror := clNone;
        ItemAppearance.FillSelected.ColorMirrorTo := clNone;
        ItemAppearance.FillSelected.BorderColor := $FEDF9A;
        ItemAppearance.FillSelected.GradientMirrorType := gtVertical;

        ItemAppearance.FillDisabled.Color := $00F2F2F2;
        ItemAppearance.FillDisabled.ColorTo := $00B6B6B6;
        ItemAppearance.FillDisabled.ColorMirror := $00B6B6B6;
        ItemAppearance.FillDisabled.ColorMirrorTo := $00F2F2F2;
        ItemAppearance.FontDisabled.Color := clGray;
        ItemAppearance.FillDisabled.GradientMirrorType := gtVertical;
      end;
    tswindows7:
      begin
        ContentFill.Color := $FFFDF9;
        ContentFill.ColorTo := $FFFDF9;

        //ContentFill.Color := $F7DAA2;
        //ContentFill.ColorTo := $F5D089;
        ContentFill.BorderColor := clNone;

        ItemAppearance.FillHover.Color := $FDFBFA;
        ItemAppearance.FillHover.ColorTo := $FDF3EB;
        ItemAppearance.FillHover.ColorMirror := clNone;
        ItemAppearance.FillHover.ColorMirrorTo := clNone;
        ItemAppearance.FillHover.BorderColor :=  $FBD6B8;
        ItemAppearance.FillHover.GradientMirrorType := gtVertical;

        ItemAppearance.FillSelected.Color := $FCEBDC;
        ItemAppearance.FillSelected.ColorTo := $FCDBC1;
        ItemAppearance.FillSelected.ColorMirror := clNone;
        ItemAppearance.FillSelected.ColorMirrorTo := clNone;
        ItemAppearance.FillSelected.BorderColor := $CEA27D;
        ItemAppearance.FillSelected.GradientMirrorType := gtVertical;

        ItemAppearance.FillDisabled.Color := $00F2F2F2;
        ItemAppearance.FillDisabled.ColorTo := $00B6B6B6;
        ItemAppearance.FillDisabled.ColorMirror := $00B6B6B6;
        ItemAppearance.FillDisabled.ColorMirrorTo := $00F2F2F2;
        ItemAppearance.FontDisabled.Color := clGray;
        ItemAppearance.FillDisabled.GradientMirrorType := gtVertical;
      end;
    tsTerminal:
      begin
        ContentFill.Color := clBtnFace;
        ContentFill.ColorTo := clBtnFace;
        ContentFill.BorderColor := clNone;

        ItemAppearance.FillHover.Color := clSilver;
        ItemAppearance.FillHover.ColorTo := clSilver;
        ItemAppearance.FillHover.ColorMirror := clNone;
        ItemAppearance.FillHover.ColorMirrorTo := clNone;
        ItemAppearance.FillHover.BorderColor :=  clGray;


        ItemAppearance.FillSelected.Color := clHighLight;
        ItemAppearance.FillSelected.ColorTo := clHighLight;
        ItemAppearance.FillSelected.ColorMirror := clNone;
        ItemAppearance.FillSelected.ColorMirrorTo := clNone;
        ItemAppearance.FillSelected.BorderColor := clGray;
        ItemAppearance.FontSelected.Color:= clWhite;

        ItemAppearance.FillDisabled.Color := clBtnFace;
        ItemAppearance.FillDisabled.ColorTo := clBtnFace;
        ItemAppearance.FillDisabled.ColorMirror := clNone;
        ItemAppearance.FillDisabled.ColorMirrorTo := clNone;
        ItemAppearance.FontDisabled.Color := clGray;
      end;
       tsOffice2010Blue:
      begin
        ItemAppearance.FillSelected.Glow := gmGradient;
        ItemAppearance.FillHover.Glow := gmGradient;

        ContentFill.Color := $FDF6EF;
        ContentFill.ColorTo := $F0DAC7;
        ContentFill.BorderColor := clNone;

        ItemAppearance.FillHover.Color := RGB(253, 227, 138);
        ItemAppearance.FillHover.ColorTo := clNone;
        ItemAppearance.FillHover.ColorMirror := clNone;
        ItemAppearance.FillHover.ColorMirrorTo := clNone;
        ItemAppearance.FillHover.BorderColor :=  RGB(242, 205, 96);
        ItemAppearance.FillHover.GradientMirrorType := gtVertical;

        ItemAppearance.FillSelected.Color := RGB(254, 225, 69);
        ItemAppearance.FillSelected.ColorTo := clNone;
        ItemAppearance.FillSelected.ColorMirror := clNone;
        ItemAppearance.FillSelected.ColorMirrorTo := clNone;
        ItemAppearance.FillSelected.BorderColor := RGB(206, 160, 79);
        ItemAppearance.FillSelected.GradientMirrorType := gtVertical;

        ItemAppearance.FillDisabled.Color := $00F2F2F2;
        ItemAppearance.FillDisabled.ColorTo := $00B6B6B6;
        ItemAppearance.FillDisabled.ColorMirror := clNone;
        ItemAppearance.FillDisabled.ColorMirrorTo := clNone;
        ItemAppearance.FontDisabled.Color := clGray;
        ItemAppearance.FillDisabled.GradientMirrorType := gtVertical;

      end;
       tsOffice2010Silver:
      begin
        ItemAppearance.FillSelected.Glow := gmGradient;
        ItemAppearance.FillHover.Glow := gmGradient;

        ContentFill.Color := $FFFFFF;
        ContentFill.ColorTo := $EDE5E0;
        ContentFill.BorderColor := clNone;

        ItemAppearance.FillHover.Color := RGB(253, 227, 138);
        ItemAppearance.FillHover.ColorTo := clNone;
        ItemAppearance.FillHover.ColorMirror := clNone;
        ItemAppearance.FillHover.ColorMirrorTo := clNone;
        ItemAppearance.FillHover.BorderColor :=  RGB(242, 205, 96);
        ItemAppearance.FillHover.GradientMirrorType := gtVertical;

        ItemAppearance.FillSelected.Color := RGB(254, 225, 69);
        ItemAppearance.FillSelected.ColorTo := clNone;
        ItemAppearance.FillSelected.ColorMirror := clNone;
        ItemAppearance.FillSelected.ColorMirrorTo := clNone;
        ItemAppearance.FillSelected.BorderColor := RGB(206, 160, 79);
        ItemAppearance.FillSelected.GradientMirrorType := gtVertical;

        ItemAppearance.FillDisabled.Color := $00F2F2F2;
        ItemAppearance.FillDisabled.ColorTo := $00B6B6B6;
        ItemAppearance.FillDisabled.ColorMirror := clNone;
        ItemAppearance.FillDisabled.ColorMirrorTo := clNone;
        ItemAppearance.FontDisabled.Color := clGray;
        ItemAppearance.FillDisabled.GradientMirrorType := gtVertical;

      end;
       tsOffice2010Black:
      begin
        ItemAppearance.FillSelected.Glow := gmGradient;
        ItemAppearance.FillSelected.GlowGradientColor:= $67BCF6;
        ItemAppearance.FillHover.Glow := gmGradient;

        ContentFill.Color := $BFBFBF;
        ContentFill.ColorTo := $919191;
        ContentFill.BorderColor := clNone;

        ItemAppearance.FillHover.Color := RGB(253, 227, 138);
        ItemAppearance.FillHover.ColorTo := clNone;
        ItemAppearance.FillHover.ColorMirror := clNone;
        ItemAppearance.FillHover.ColorMirrorTo := clNone;
        ItemAppearance.FillHover.BorderColor :=  RGB(242, 205, 96);
        ItemAppearance.FillHover.GradientMirrorType := gtVertical;

        ItemAppearance.FillSelected.Color := RGB(254, 225, 69);
        ItemAppearance.FillSelected.ColorTo := clNone;
        ItemAppearance.FillSelected.ColorMirror := clNone;
        ItemAppearance.FillSelected.ColorMirrorTo := clNone;
        ItemAppearance.FillSelected.BorderColor := RGB(206, 160, 79);
        ItemAppearance.FillSelected.GradientMirrorType := gtVertical;

        ItemAppearance.FillDisabled.Color := $00F2F2F2;
        ItemAppearance.FillDisabled.ColorTo := $00B6B6B6;
        ItemAppearance.FillDisabled.ColorMirror := clNone;
        ItemAppearance.FillDisabled.ColorMirrorTo := clNone;
        ItemAppearance.FontDisabled.Color := clGray;
        ItemAppearance.FillDisabled.GradientMirrorType := gtVertical;

      end;
        tsWindows8, tsWindows10:
      begin

        ItemAppearance.FillSelected.Glow := gmNone;
        ItemAppearance.FillSelected.GlowGradientColor:= clNone;
        ItemAppearance.FillHover.Glow := gmNone;

        ContentFill.Color := clWhite;
        ContentFill.ColorTo := clNone;
        ContentFill.BorderColor := clNone;

        ItemAppearance.FillDisabled.Color := $F7F7F7;
        ItemAppearance.FillDisabled.ColorTo := $F7F7F7;
        ItemAppearance.FillDisabled.ColorMirror := clNone;
        ItemAppearance.FillDisabled.ColorMirrorTo := clNone;
        ItemAppearance.FillDisabled.BorderColor := $DEDEDE;
        ItemAppearance.FillDisabled.GradientMirrorType := gtVertical;

        ItemAppearance.FillSelected.Color := $F7E0C9;
        ItemAppearance.FillSelected.ColorTo := clNone;
        ItemAppearance.FillSelected.ColorMirror := clNone;
        ItemAppearance.FillSelected.ColorMirrorTo := clNone;
        ItemAppearance.FillSelected.BorderColor := $E4A262;
        ItemAppearance.FillSelected.GradientMirrorType := gtVertical;

        ItemAppearance.FillHover.Color := $F7EFE8;
        ItemAppearance.FillHover.ColorTo := $F7EFE8;
        ItemAppearance.FillHover.ColorMirror := $F7EFE8;
        ItemAppearance.FillHover.ColorMirrorTo := $F7EFE8;
        ItemAppearance.FillHover.BorderColor :=  $F9CEA4;
        ItemAppearance.FillHover.GradientMirrorType := gtVertical;

      end;
    tsOffice2013White:
      begin

        ItemAppearance.FillSelected.Glow := gmNone;
        ItemAppearance.FillSelected.GlowGradientColor:= clNone;
        ItemAppearance.FillHover.Glow := gmNone;

        ContentFill.Color := clWhite;
        ContentFill.ColorTo := clNone;
        ContentFill.BorderColor := clNone;


        ItemAppearance.FillDisabled.Color := $EEEEEE;
        ItemAppearance.FillDisabled.ColorTo := $EEEEEE;
        ItemAppearance.FillDisabled.ColorMirror := clNone;
        ItemAppearance.FillDisabled.ColorMirrorTo := clNone;
        ItemAppearance.FillDisabled.BorderColor := $ACACAC;
        ItemAppearance.FillDisabled.GradientMirrorType := gtVertical;

        ItemAppearance.FillSelected.Color := $FCE2C8;
        ItemAppearance.FillSelected.ColorTo := clNone;
        ItemAppearance.FillSelected.ColorMirror := clNone;
        ItemAppearance.FillSelected.ColorMirrorTo := clNone;
        ItemAppearance.FillSelected.BorderColor := $E59D56;
        ItemAppearance.FillSelected.GradientMirrorType := gtVertical;

        ItemAppearance.FillHover.Color := $FCF0E4;
        ItemAppearance.FillHover.ColorTo := $FCF0E4;
        ItemAppearance.FillHover.ColorMirror := clNone;
        ItemAppearance.FillHover.ColorMirrorTo := clNone;
        ItemAppearance.FillHover.BorderColor :=  $EAB47E;
        ItemAppearance.FillHover.GradientMirrorType := gtVertical;
      end;

    tsOffice2013LightGray:
      begin

        ItemAppearance.FillSelected.Glow := gmNone;
        ItemAppearance.FillSelected.GlowGradientColor:= clNone;
        ItemAppearance.FillHover.Glow := gmNone;

        ContentFill.Color := clWhite;
        ContentFill.ColorTo := clNone;
        ContentFill.BorderColor := clNone;

        ItemAppearance.FillDisabled.Color := $EEEEEE;
        ItemAppearance.FillDisabled.ColorTo := $EEEEEE;
        ItemAppearance.FillDisabled.ColorMirror := clNone;
        ItemAppearance.FillDisabled.ColorMirrorTo := clNone;
        ItemAppearance.FillDisabled.BorderColor := $ACACAC;
        ItemAppearance.FillDisabled.GradientMirrorType := gtVertical;

        ItemAppearance.FillSelected.Color := $FCE2C8;
        ItemAppearance.FillSelected.ColorTo := clNone;
        ItemAppearance.FillSelected.ColorMirror := clNone;
        ItemAppearance.FillSelected.ColorMirrorTo := clNone;
        ItemAppearance.FillSelected.BorderColor := $E59D56;
        ItemAppearance.FillSelected.GradientMirrorType := gtVertical;


        ItemAppearance.FillHover.Color := $FCF0E4;
        ItemAppearance.FillHover.ColorTo := $FCF0E4;
        ItemAppearance.FillHover.ColorMirror := clNone;
        ItemAppearance.FillHover.ColorMirrorTo := clNone;
        ItemAppearance.FillHover.BorderColor :=  $EAB47E;
        ItemAppearance.FillHover.GradientMirrorType := gtVertical;

      end;

    tsOffice2013Gray:
      begin

        ItemAppearance.FillSelected.Glow := gmNone;
        ItemAppearance.FillSelected.GlowGradientColor:= clNone;
        ItemAppearance.FillHover.Glow := gmNone;
        ContentFill.Color := clWhite;
        ContentFill.ColorTo := clNone;
        ContentFill.BorderColor := clNone;


        ItemAppearance.FillDisabled.Color := $EEEEEE;
        ItemAppearance.FillDisabled.ColorTo := $EEEEEE;
        ItemAppearance.FillDisabled.ColorMirror := clNone;
        ItemAppearance.FillDisabled.ColorMirrorTo := clNone;
        ItemAppearance.FillDisabled.BorderColor := $ACACAC;
        ItemAppearance.FillDisabled.GradientMirrorType := gtVertical;

        ItemAppearance.FillSelected.Color := $FCE2C8;
        ItemAppearance.FillSelected.ColorTo := clNone;
        ItemAppearance.FillSelected.ColorMirror := clNone;
        ItemAppearance.FillSelected.ColorMirrorTo := clNone;
        ItemAppearance.FillSelected.BorderColor := $E59D56;
        ItemAppearance.FillSelected.GradientMirrorType := gtVertical;

        ItemAppearance.FillHover.Color := $FCF0E4;
        ItemAppearance.FillHover.ColorTo := $FCF0E4;
        ItemAppearance.FillHover.ColorMirror := clNone;
        ItemAppearance.FillHover.ColorMirrorTo := clNone;
        ItemAppearance.FillHover.BorderColor :=  $EAB47E;
        ItemAppearance.FillHover.GradientMirrorType := gtVertical;
      end;
   tsOffice2016White:
      begin

        ItemAppearance.FillSelected.Glow := gmNone;
        ItemAppearance.FillSelected.GlowGradientColor:= clNone;
        ItemAppearance.FillHover.Glow := gmNone;

        ContentFill.Color := clWhite;
        ContentFill.ColorTo := clNone;
        ContentFill.BorderColor := clNone;


        ItemAppearance.FillDisabled.Color := clWhite;
        ItemAppearance.FillDisabled.ColorTo := clNone;
        ItemAppearance.FillDisabled.ColorMirror := clNone;
        ItemAppearance.FillDisabled.ColorMirrorTo := clNone;
        ItemAppearance.FillDisabled.BorderColor := $D4D4D4;
        ItemAppearance.FillDisabled.GradientMirrorType := gtVertical;

        ItemAppearance.FillSelected.Color := $E3BDA3;
        ItemAppearance.FillSelected.ColorTo := clNone;
        ItemAppearance.FillSelected.ColorMirror := clNone;
        ItemAppearance.FillSelected.ColorMirrorTo := clNone;
        ItemAppearance.FillSelected.BorderColor := $E3BDA3;
        ItemAppearance.FillSelected.GradientMirrorType := gtVertical;

        ItemAppearance.FillHover.Color := $F2E1D5;
        ItemAppearance.FillHover.ColorTo := clNone;
        ItemAppearance.FillHover.ColorMirror := clNone;
        ItemAppearance.FillHover.ColorMirrorTo := clNone;
        ItemAppearance.FillHover.BorderColor :=  $F2E1D5;
        ItemAppearance.FillHover.GradientMirrorType := gtVertical;
      end;

    tsOffice2016Gray:
      begin

        ItemAppearance.FillSelected.Glow := gmNone;
        ItemAppearance.FillSelected.GlowGradientColor:= clNone;
        ItemAppearance.FillHover.Glow := gmNone;

        ContentFill.Color := $B2B2B2;
        ContentFill.ColorTo := clNone;
        ContentFill.BorderColor := clNone;

        ItemAppearance.FillDisabled.Color := $B2B2B2;
        ItemAppearance.FillDisabled.ColorTo := $B2B2B2;
        ItemAppearance.FillDisabled.ColorMirror := clNone;
        ItemAppearance.FillDisabled.ColorMirrorTo := clNone;
        ItemAppearance.FillDisabled.BorderColor := $444444;
        ItemAppearance.FillDisabled.GradientMirrorType := gtVertical;

        ItemAppearance.FillSelected.Color := $E3BDA3;
        ItemAppearance.FillSelected.ColorTo := clNone;
        ItemAppearance.FillSelected.ColorMirror := clNone;
        ItemAppearance.FillSelected.ColorMirrorTo := clNone;
        ItemAppearance.FillSelected.BorderColor := $E3BDA3;
        ItemAppearance.FillSelected.GradientMirrorType := gtVertical;


        ItemAppearance.FillHover.Color := $F2E1D5;
        ItemAppearance.FillHover.ColorTo := $F2E1D5;
        ItemAppearance.FillHover.ColorMirror := clNone;
        ItemAppearance.FillHover.ColorMirrorTo := clNone;
        ItemAppearance.FillHover.BorderColor :=  $F2E1D5;
        ItemAppearance.FillHover.GradientMirrorType := gtVertical;

      end;

    tsOffice2016Black:
      begin

        ItemAppearance.FillSelected.Glow := gmNone;
        ItemAppearance.FillSelected.GlowGradientColor:= clNone;
        ItemAppearance.FillHover.Glow := gmNone;

        ContentFill.Color := $B2B2B2;
        ContentFill.ColorTo := clNone;
        ContentFill.BorderColor := clNone;


        ItemAppearance.FillDisabled.Color := $363636;
        ItemAppearance.FillDisabled.ColorTo := $363636;
        ItemAppearance.FillDisabled.ColorMirror := clNone;
        ItemAppearance.FillDisabled.ColorMirrorTo := clNone;
        ItemAppearance.FillDisabled.BorderColor := $444444;
        ItemAppearance.FillDisabled.GradientMirrorType := gtVertical;

        ItemAppearance.FillSelected.Color := $444444;
        ItemAppearance.FillSelected.ColorTo := clNone;
        ItemAppearance.FillSelected.ColorMirror := clNone;
        ItemAppearance.FillSelected.ColorMirrorTo := clNone;
        ItemAppearance.FillSelected.BorderColor := $444444;
        ItemAppearance.FillSelected.GradientMirrorType := gtVertical;

        ItemAppearance.FillHover.Color := $6A6A6A;
        ItemAppearance.FillHover.ColorTo := $6A6A6A;
        ItemAppearance.FillHover.ColorMirror := clNone;
        ItemAppearance.FillHover.ColorMirrorTo := clNone;
        ItemAppearance.FillHover.BorderColor :=  $6A6A6A;
        ItemAppearance.FillHover.GradientMirrorType := gtVertical;
      end;

  end;

  ItemAppearance.Fill.Color := clNone;
  ItemAppearance.Fill.BorderColor := clNone;
  ContentFill.BorderColor := clBlack;
  ContentFill.BorderOpacity := 50;

  case AStyle of
    tsOffice2003Blue:
      begin
        TearOffFill.Color := $D68759;
        TearOffFill.ColorTo := $933803;
        TearOffFill.BorderColor := $962D00;
      end;
    tsOffice2003Silver:
      begin
        TearOffFill.Color := $BDA4A5;
        TearOffFill.ColorTo := $957475;
        TearOffFill.BorderColor := $947C7C;
      end;
    tsOffice2003Olive:
      begin
        TearOffFill.Color := $82C0AF;
        TearOffFill.ColorTo := $447A63;
        TearOffFill.BorderColor := $588060;
      end;
    tsOffice2003Classic:
      begin
        TearOffFill.Color := $808080;
        TearOffFill.ColorTo := $808080;
        TearOffFill.BorderColor := $808080;
      end;
    tsOffice2007Luna:
      begin
        TearOffFill.Color := $FFEFE3;
        TearOffFill.ColorTo := $FFD2AF;
        TearOffFill.BorderColor := $00FFD2AF;
      end;
    tsOffice2007Obsidian:
      begin
        TearOffFill.Color := $F2F1F0;
        TearOffFill.ColorTo := $C9C2BD;
        TearOffFill.BorderColor := $5C534C;
      end;
    tsWindowsXP:
      begin
        TearOffFill.Color := clBtnFace;
        TearOffFill.ColorTo := clBtnFace;
        TearOffFill.BorderColor := clBlack;
      end;
    tsWhidbey:
      begin
        TearOffFill.Color := $EBEEEF;
        TearOffFill.ColorTo := $7E9898;
        TearOffFill.BorderColor := $962D00;
      end;
    tsCustom: ;
    tsOffice2007Silver:
      begin
        TearOffFill.Color := $F8F7F6;
        TearOffFill.ColorTo := $E8E0DB;
        TearOffFill.BorderColor := $74706F;
      end;
    tsWindowsVista:
      begin
        TearOffFill.Color := $FBEDD3;
        TearOffFill.ColorTo := $FAE9C6;
        TearOffFill.BorderColor := $FEDF9A;
      end;
    tsWindows7:
      begin
        TearOffFill.Color := $FCEBDC;
        TearOffFill.ColorTo := $FCDBC1;
        TearOffFill.BorderColor := $CEA27D;
      end;
    tsTerminal:
      begin
        TearOffFill.Color := clBtnFace;
        TearOffFill.ColorTo := clBtnFace;
        TearOffFill.BorderColor := clGray;
      end;
      tsOffice2010Blue:
      begin
        TearOffFill.Color := $9C8B7B;
        TearOffFill.ColorTo := clNone;
        TearOffFill.BorderColor := clNone;
      end;
      tsOffice2010Silver:
      begin
        TearOffFill.Color := $96908A;
        TearOffFill.ColorTo := clNone;
        TearOffFill.BorderColor := clNone;
      end;
      tsOffice2010Black:
      begin
        TearOffFill.Color := $444444;
        TearOffFill.ColorTo := clNone;
        TearOffFill.BorderColor := clNone;
      end;
   tsWindows8, tsWindows10:
      begin
        TearOffFill.Color := $DAA026;
        TearOffFill.ColorTo := clNone;
        TearOffFill.BorderColor := clNone;
      end;

    tsOffice2013White:
      begin
        TearOffFill.Color := $FF9933;
        TearOffFill.ColorTo := clNone;
        TearOffFill.BorderColor := clNone;
      end;

    tsOffice2013LightGray:
      begin
        TearOffFill.Color := $FF9933;
        TearOffFill.ColorTo := clNone;
        TearOffFill.BorderColor := clNone;
      end;

    tsOffice2013Gray:
      begin
        TearOffFill.Color := $FF9933;
        TearOffFill.ColorTo := clNone;
        TearOffFill.BorderColor := clNone;
      end;
    tsOffice2016White:
      begin
        TearOffFill.Color := $F2D5C2;
        TearOffFill.ColorTo := clNone;
        TearOffFill.BorderColor := clNone;
      end;

    tsOffice2016Gray:
      begin
        TearOffFill.Color := $F2D5C2;
        TearOffFill.ColorTo := clNone;
        TearOffFill.BorderColor := clNone;
      end;

    tsOffice2016Black:
      begin
        TearOffFill.Color := $575757;
        TearOffFill.ColorTo := clNone;
        TearOffFill.BorderColor := clNone;
      end;
  end;

  for I := 0 to Sections.Count - 1 do
    Sections[I].SetComponentStyle(Astyle);
end;

procedure TGDIPMenu.SetContentFill(const Value: TGDIPFill);
begin
  if FContentFill <> value then
  begin
    FContentFill.Assign(Value);
    Changed;
  end;
end;

procedure TGDIPMenu.SetDropDownLeft(const Value: integer);
begin
  if FDropDownLeft <> value then
  begin
    FDropDownLeft := Value;
    Changed;
  end;
end;

procedure TGDIPMenu.SetDropDownLocation(const Value: TGDIPMenuDropDownLocation);
begin
  if FDropDownLocation <> value then
  begin
    FDropDownLocation := Value;
    Changed;
  end;
end;

procedure TGDIPMenu.SetDropDownTop(const Value: integer);
begin
  if FDropDownTop <> value then
  begin
    FDropDownTop := Value;
    Changed;
  end;
end;

procedure TGDIPMenu.SetImageList(const Value: TCustomImageList);
begin
  FImageList := Value;
  Changed;
end;

procedure TGDIPMenu.SetItemAppearance(
  const Value: TGDIPMenuSectionItemAppearance);
begin
  if FItemAppearance <> value then
  begin
    FItemAppearance.Assign(Value);
    Changed;
  end;
end;

procedure TGDIPMenu.SetPictureContainer(const Value: TGDIPPictureContainer);
begin
  FPictureContainer := Value;
  Changed;
end;

procedure TGDIPMenu.SetSectionLayout(const Value: TGDIPMenuSectionLayout);
begin
  if FSectionLayout <> value then
  begin
    FSectionLayout := Value;
    Changed;
  end;
end;

procedure TGDIPMenu.SetSectionMargin(const Value: TGDIPMenuMargin);
begin
  if FSectionMargin <> value then
  begin
    FSectionMargin.Assign(Value);
    Changed;
  end;
end;

procedure TGDIPMenu.SetSections(const Value: TGDIPMenuSections);
begin
  if FSections <> value then
  begin
    FSections.Assign(Value);
    Changed;
  end;
end;

procedure TGDIPMenu.SetSelectedItem(const Value: TCurrentItem);
begin
  FSelectedItem := Value;
end;

procedure TGDIPMenu.SetTearOff(const Value: Boolean);
begin
  if FTearOff <> value then
  begin
    FTearOff := Value;
    Changed;
  end;
end;

procedure TGDIPMenu.SetTearOffFill(const Value: TGDIPFill);
begin
  if FTearOffFill <> value then
  begin
    FTearOffFill.Assign(value);
    Changed;
  end;
end;

procedure TGDIPMenu.SetTearOffSize(const Value: integer);
begin
  if FTearOffSize <> Value then
  begin
    FTearOffSize := Value;
    Changed;
  end;
end;

procedure TGDIPMenu.SetTopLayerItems(const Value: TGDIPMenuTopLayerItems);
begin
  if FTopLayerItems <> value then
  begin
    FTopLayerItems.Assign(Value);
    Changed;
  end;
end;

procedure TGDIPMenu.TopLayerItemsChanged(Sender: TObject);
begin
  Changed;
end;

function TGDIPMenu.UsesControls: Boolean;
var
  i, K: integer;
begin
  Result := false;
  for I := 0 to Sections.Count - 1 do
  begin
    if Sections[I].Visible then
    begin
      with Sections[I] do
      begin
        for K := 0 to Items.Count - 1 do
        begin
          if Assigned(Items[K].Control) or (Items[K].ControlType = ctRadioButton) or (Items[K].ControlType = ctCheckBox) then
          begin
            Result := true;
            Exit;
          end;
        end;
      end;
    end;
  end;
end;

function TGDIPMenu.XYToSectionItem(X, Y: integer): TCurrentItem;
var
  i, k: integer;
  r: TGPRectF;
  chk: Boolean;
begin
  Result.item := nil;
  Result.section := nil;
  for I := 0 to Sections.Count - 1 do
  begin
    if Sections[I].Visible then
    begin
      for K := 0 to Sections[I].Items.Count - 1 do
      begin
        r := Sections[I].Items[K].FFillRect;

        chk := false;
        if (Sections[I].Items[K].ControlType = ctCheckBox) or (Sections[I].Items[K].ControlType = ctRadioButton) then
          chk := PtInGPRect(MakeRect(r.X, r.Y + (r.Height - 17) / 2, 17, 17), Point(X, Y));

        if not chk and PtInGPRect(r, Point(X, Y)) and Sections[I].Items[K].Enabled and Sections[I].Items[K].Visible then
        begin
          Result.item := Sections[I].Items[K];
          Result.section := Sections[I];
          Exit;
        end;
      end;
    end;
  end;
end;

function TGDIPMenu.XYToSectionItemControl(X, Y: integer): TCurrentItem;
var
  i, k: integer;
  r: TGPRectF;
begin
  Result.item := nil;
  Result.section := nil;
  for I := 0 to Sections.Count - 1 do
  begin
    if Sections[I].Visible then
    begin
      for K := 0 to Sections[I].Items.Count - 1 do
      begin
        r := Sections[I].Items[K].FFillRect;
        if PtInGPRect(MakeRect(r.X, r.Y + (r.Height - 17) / 2, 17, 17), Point(X, Y)) and Sections[I].Items[K].Enabled and Sections[I].Items[K].Visible
        and ((Sections[I].Items[K].ControlType = ctCheckBox) or (Sections[I].Items[K].ControlType = ctRadioButton)) then
        begin
          Result.item := Sections[I].Items[K];
          Result.section := Sections[I];
          Exit;
        end;
      end;
    end;
  end;
end;

function TGDIPMenu.XYToTopLayerItem(X, Y: integer): TGDIPMenuTopLayerItem;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to TopLayerItems.Count - 1 do
  begin
    if TopLayerItems[I].Visible then
    begin
      if PtInGPRect(TopLayerItems[I].GetTopLayerRect, Point(X, Y)) then
      begin
        Result := TopLayerItems[I];
        break;
      end;
    end;
  end;
end;

{ TGDIPMenuSections }

function TGDIPMenuSections.Add: TGDIPMenuSection;
begin
  Result := TGDIPMenuSection(inherited Add);
end;

constructor TGDIPMenuSections.Create(AOwner: TWinControl; Menu: TGDIPMenu);
begin
  inherited Create(AOwner, TGDIPMenuSection);
  FOwner := Menu;
  FMenuOwner := AOwner;
end;

procedure TGDIPMenuSections.Delete(Index: Integer);
begin
  Items[Index].Free;
end;

function TGDIPMenuSections.GetItem(Index: Integer): TGDIPMenuSection;
begin
  Result := TGDIPMenuSection(inherited Items[Index]);
end;

function TGDIPMenuSections.GetOwner: TPersistent;
begin
  result := FMenuOwner;
end;

function TGDIPMenuSections.Insert(Index: Integer): TGDIPMenuSection;
begin
  Result := TGDIPMenuSection(inherited Insert(Index));
end;

procedure TGDIPMenuSections.SetItem(Index: Integer;
  const Value: TGDIPMenuSection);
begin
  inherited Items[Index] := Value;
end;

{ TGDIPMenuTopLayerItems }

function TGDIPMenuTopLayerItems.Add: TGDIPMenuTopLayerItem;
begin
  Result := TGDIPMenuTopLayerItem(inherited Add);
end;

constructor TGDIPMenuTopLayerItems.Create(AOwner: TWinControl; Menu: TGDIPMenu);
begin
  inherited Create(AOwner, TGDIPMenuTopLayerITem);
  FOwner := Menu;
end;

procedure TGDIPMenuTopLayerItems.Delete(Index: Integer);
begin
  Items[Index].Free;
end;

function TGDIPMenuTopLayerItems.GetItem(Index: Integer): TGDIPMenuTopLayerItem;
begin
  Result := TGDIPMenuTopLayerItem(inherited Items[Index]);
end;

function TGDIPMenuTopLayerItems.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TGDIPMenuTopLayerItems.Insert(Index: Integer): TGDIPMenuTopLayerItem;
begin
  Result := TGDIPMenuTopLayerItem(inherited Insert(Index));
end;

procedure TGDIPMenuTopLayerItems.SetItem(Index: Integer;
  const Value: TGDIPMenuTopLayerItem);
begin
  inherited Items[Index] := value;
end;

{ TGDIPMenuSection }

procedure TGDIPMenuSection.Assign(Source: TPersistent);
begin
  if (Source is TGDIPMenuSection) then
  begin
    FVisible := (Source as TGDIPMenuSection).Visible;
    FBackGroundFill.Assign((Source as TGDIPMenuSection).BackGroundFill);
    FCaptionFill.Assign((Source as TGDIPMenuSection).CaptionFill);
    FItems.Assign((Source as TGDIPMenuSection).Items);
    FCaptionSize := (Source as TGDIPMenuSection).CaptionSize;
    FCaption := (Source as TGDIPMenuSection).Caption;
    FCaptionLocation := (Source as TGDIPMenuSection).CaptionLocation;
    FCaptionLeft := (Source as  TGDIPMenuSection).CaptionLeft;
    FCaptionTop := (Source as TGDIPMenuSection).CaptionTop;
    FcaptionFont.Assign((Source as TGDIPMenuSection).CaptionFont);
    FItemSpacing := (Source as TGDIPMenuSection).ItemSpacing;
    FItemMargin.Assign((Source as TGDIPMenuSection).ItemMargin);
    FWidth := (Source as TGDIPMenuSection).Width;
    FHeight := (Source as TGDIPMenuSection).Height;
    FAutoItemHeight := (Source as TGDIPMenuSection).AutoItemHeight;
    Changed;
  end;
end;

procedure TGDIPMenuSection.BuildItemRects(first: boolean);
var
  rd, conr: TGPRectF;
  l, t, w: Double;
  i: integer;
  itr, fr: TGPRectF;
  picl, picr: TAdvGDIPPicture;
  wil, wir, hil, hir, totalh: Double;
  bmp: TBitmap;
  g: TGPGraphics;
  k: integer;
  htmlr: TGPRectF;
  str: String;
  cf: TFont;
  fitemtxt: TGDIPMenuHTMLText;
  strshort: String;
  max, testw: integer;
  ca: TCanvas;
begin
  bmp := TBitmap.Create;

  cf := TFont.Create;
  max := 0;
  for I := 0 to Items.Count - 1 do
  begin
    with Items[I] do
    begin
      if Items[I].Visible then
      begin
        bmp.Canvas.Font.Assign(FOwner.ItemAppearance.Font);
        testw := bmp.Canvas.TextWidth('W');
        if testw  > max then
        begin
          max := testw;
          cf.Assign(FOwner.ItemAppearance.Font);
        end;

        bmp.Canvas.Font.Assign(FOwner.ItemAppearance.FontSelected);
        testw := bmp.Canvas.TextWidth('W');
        if testw > max then
        begin
          max := testw;
          cf.Assign(FOwner.ItemAppearance.FontSelected);
        end;

        bmp.Canvas.Font.Assign(FOwner.ItemAppearance.FontHover);
        testw := bmp.Canvas.TextWidth('W');
        if testw  > max then
        begin
          max := testw;
          cf.Assign(FOwner.ItemAppearance.FontHover);
        end;

        bmp.Canvas.Font.Assign(FOwner.ItemAppearance.FontDisabled);
        testw := bmp.Canvas.TextWidth('W');
        if testw  > max then
        begin
          max := testw;
          cf.Assign(FOwner.ItemAppearance.FontDisabled);
        end;
      end;
    end;
  end;

  g := TGPGraphics.Create(bmp.Canvas.Handle);
  fitemtxt := TGDIPMenuHTMLText.Create(nil);
  fitemtxt.URLColor := FOwner.ItemAppearance.URLColor;
  fitemtxt.ShadowColor := FOwner.ItemAppearance.ShadowColor;
  fitemtxt.ShadowOffset := FOwner.ItemAppearance.ShadowOffset;

  FCnt := 1;
  for K := 0 to Items.Count - 1 do
  begin
    if Items[k].Visible and (Items[K].ItemType = itBreak) or (Items[K].ItemType = itLineBreak) and (K < Items.Count - 1) then
      Inc(FCnt);
  end;

  if not first then
  begin
    rd := GetSectionInsideRect(FOwner.GetSectionsRect(FOwner.FRect));
    conr := GetContentRect(rd);
  end
  else
  begin
    rd := GetSectionInsideRect(FOwner.GetSectionsRect(Rect(0, 0, 0, 0)));
    conr := GetContentRect(rd);
  end;

  l := conr.X + ItemMargin.Left;
  t := conr.Y + ItemMargin.Top;
  if (FCnt > 0) then
    w := (conr.Width - ((ItemMargin.Right + ItemMargin.Left) * FCnt)) / FCnt
  else
    w := Conr.Width - (ItemMargin.Right + ItemMargin.Left);

  for I := 0 to Items.Count - 1 do
  begin
    with Items[I] do
    begin
      if Items[I].Visible then
      begin
        FCount := FCnt;
        str := FText;
        if (ItemType = itSeparator) or (ItemType = itLineSeparator) then
          str := '';

        FFillRect := MakeRect(ItemMargin.Left, ItemMargin.Top, w, GetHeight);
        if str <> '' then
        begin
          htmlr := FFillRect;
          fitemtxt.Text := str;
          FOwner.DrawItemHTMLText(g, htmlr, cf, fitemtxt, FFillRect, str, TextLocation, TextLeft, TextTop, True);
          FItemRect := MakeRect(0, 0, htmlr.Width + 5 , htmlr.Height);
        end
        else
          FItemRect := MakeRect(0, 0, 0, 0);


        if ShortCut <> 0 then
        begin
          strshort := ShortCutToText(ShortCut);
          ca := TCanvas.Create;
          ca.Handle := g.GetHDC;
          FItemRect := MakeRect(FItemRect.X, FItemRect.Y, FItemRect.Width + ca.TextWidth(strshort) + 5, FItemRect.Height);
          g.ReleaseHDC(ca.Handle);
          ca.Free;
        end;

        if first then
          FFillRect.Width := FItemRect.Width;

        if not FAutoItemHeight then
          FFillRect.Height := Height + ItemRectangleMargin.Bottom + ItemRectangleMargin.Top;

        case ItemType of
          itHeader, itNormal, itSeparator, itLineSeparator:
          begin
            itr := FItemRect;
            // Add 17 default size of radiobutton or checkbox
            if (ControlType = ctRadioButton) or (ControlType = ctCheckBox) then
              FItemRect := MakeRect(l + 17, t, itr.Width, itr.Height)
            else
              FItemRect := MakeRect(l, t, itr.Width, itr.Height);

            hil := 0;
            hir := 0;
            wil := 0;
            wir := 0;
            if Assigned(FOwner.PictureContainer) then
            begin
              picl := FOwner.PictureContainer.FindPicture(GraphicLeftName);
              if Assigned(picl) and not picl.Empty then
              begin
                picl.GetImageSizes;
                wil := picl.Width;
                hil := picl.Height;
              end;
              picr := FOwner.PictureContainer.FindPicture(GraphicRightName);
              if Assigned(picr) and not picr.Empty then
              begin
                picr.GetImageSizes;
                hir := picr.Height;
                wir := picr.Width;
              end;
            end;

            if Assigned(FOwner.ImageList) then
            begin
              if (ImageIndex >= 0) and (ImageIndex <= FOwner.ImageList.Count - 1) then
              begin
                if FOwner.ImageList.Width > wil then
                  wil := FOwner.ImageList.Width;
              end;
            end;

            itr := FItemRect;
            totalh := itr.Height;
            if hir > totalh then
              totalh := hir;
            if hil > totalh then
              totalh := hil;

            if Assigned(Control) then
            begin
              if Control.Height > totalh then
                totalh := Control.Height + 5;
            end;

            fr := FFillRect;

            if not FAutoItemHeight then
              totalh := Height;

            if (ControlType = ctRadioButton) or (ControlType = ctCheckBox) then
              FItemRect := MakeRect(itr.X + wil + ItemRectangleMargin.Left , itr.Y + ItemrectangleMargin.Top,
                fr.Width - ItemRectangleMargin.Right - ItemRectangleMargin.Left - 17 - wil, totalh)
            else
              FItemRect := MakeRect(itr.X + wil + ItemRectangleMargin.Left, itr.Y + ItemrectangleMargin.Top,
                fr.Width - ItemRectangleMargin.Right - ItemRectangleMargin.Left - wil, totalh);

            if FAutoItemHeight then
              FFillRect.Height := FItemRect.Height + ItemRectangleMargin.Bottom + ItemRectangleMargin.Top;

            fr := FFillRect;
            FFillRect := MakeRect(l, t, fr.Width, fr.Height);

            if Assigned(Control) and not (csDesigning in FOwner.FWinC.ComponentState) then
            begin
              Control.Left := Round(l + Items[I].ControlIndent);
              Control.Top := Round(t + (fr.Height - Control.Height) / 2);
              if ControlType = ctEdit then
                Control.Width := Round(fr.Width - ControlIndent - wir - 5);
            end;

            t := t + fr.Height + ItemSpacing;
          end;
          itBreak, itLineBreak:
          begin
            l := l + w + ItemMargin.Left + ItemMargin.Right;
            t := conr.Y + ItemMargin.Top;
          end;
        end;
      end;
    end;
  end;
  cf.Free;
  g.Free;
  bmp.Free;
  fitemtxt.Free;
end;

procedure TGDIPMenuSection.CalculateAutoSize;
var
  maxh, maxw, temph, tempw, totalw, totalh: Double;
  k: integer;
  ff: TGPFontFamily;
  fs: integer;
  f: TGPFont;
  sf: TGPStringFormat;
  g: TGPGraphics;
  bmp: TBitmap;
  sizer: TGPRectF;
  szl, szr: integer;
  picl, picr: TAdvGDIPPicture;
begin
  maxh := 0;
  maxw := 0;
  temph := 0;
  tempw := 0;
  for K := 0 to Items.Count - 1 do
  begin
    with Items[K] do
    begin
      if Items[K].Visible then
      begin
        if (ItemType = itBreak) or (ItemType = itLineBreak) then
        begin
          if (K < Items.Count - 1) then
          begin
            tempw := tempw + maxw;

            maxw := 0;

            if maxh > temph then
              temph := maxh;

            maxh := 0;
          end;
        end
        else
        begin
          szl := 0;
          szr := 0;
          if Assigned(FOwner.PictureContainer) then
          begin
            picl := FOwner.PictureContainer.FindPicture(FGraphicLeftName);
            picr := FOwner.PictureContainer.FindPicture(FGraphicRightName);
            if Assigned(picl) then
            begin
              picl.GetImageSizes;
              szl := picl.Width;
            end;
            if Assigned(picr) then
            begin
              picr.GetImageSizes;
              szr := picr.Width;
            end;
          end;

          if FFillRect.Width > maxw then
          begin
            maxw := FFillRect.Width + 10 + szl + szr;
          end;

          if Assigned(Control) and (ControlType = ctControl) then
          begin
            if Control.Width + ControlIndent > maxw then
            begin
              maxw := Control.Width + ControlIndent + 10;
            end;
          end;


          maxh := maxh + FFillRect.Height + ItemSpacing;
        end;
      end;
    end;
  end;

  sizer := MakeRect(0, 0, 0, 0);
  if (Caption <> '') and (CaptionSize <> 0) then
  begin
    bmp := TBitmap.Create;
    g := TGPGraphics.Create(bmp.Canvas.Handle);
    ff := TGPFontFamily.Create(CaptionFont.Name);
    if (ff.Status in [FontFamilyNotFound, FontStyleNotFound]) then
    begin
      ff.Free;
      ff := TGPFontFamily.Create('Arial');
    end;

    fs := 0;
    if (fsBold in CaptionFont.Style) then
      fs := fs + 1;
    if (fsItalic in CaptionFont.Style) then
      fs := fs + 2;
    if (fsUnderline in CaptionFont.Style) then
      fs := fs + 4;

    sf := TGPStringFormat.Create;
    f := TGPFont.Create(ff, CaptionFont.Size , fs, UnitPoint);

    g.MeasureString(FCaption, Length(FCaption), f, MakeRect(0, 0, 10000, 10000), sf, sizer);

    f.Free;
    sf.Free;
    ff.Free;
    g.Free;
    bmp.Free;
  end;

  totalw := tempw + maxw;

  if sizer.Width + 10 > totalw then
    totalw := sizer.Width + 10;

  if temph > maxh then
    totalh := temph
  else
    totalh := maxh;

  totalh := totalh + CaptionSize + ItemMargin.Top + ItemMargin.Bottom ;
  totalw := totalw + ItemMargin.Right + ItemMargin.Left;

  FAutoWidth := totalw;
  FAutoHeight := totalh;
end;

procedure TGDIPMenuSection.Changed;
begin
  if Assigned(FOwner) then
  begin
    FOwner.FNeedsUpdate := true;
    FOwner.Changed;
  end;
end;

constructor TGDIPMenuSection.Create(Collection: TCollection);
var
  tmsif: ITMSMegaMenu;
begin
  inherited;
  FOwner := (Collection as TGDIPMenuSections).FOwner;
  FBackGroundFill := TGDIPFill.Create;
  FCaptionFill := TGDIPFill.Create;
  FItems := TGDIPMenuSectionItems.Create(FOwner, Self);
  FItems.OnChange := ItemsChanged;
  FCaptionSize := 20;
  FVisible := True;
  FCaptionTop := 0;
  FCaptionLeft := 0;
  FCaptionLocation := mlCenterLeft;
  FCaptionFont := TFont.Create;
  FCaptionFont.OnChange := FontChanged;
  {$IFNDEF DELPHI9_LVL}
  FCaptionFont.Name := 'Tahoma';
  {$ENDIF}
  FWidth := 250;
  FHeight := 150;
  FItemSpacing := 2;
  FItemMargin := TGDIPMenuMargin.Create(FOwner);
  FItemMargin.OnChange := ItemMarginChanged;
  FItemRectangleMargin := TGDIPMenuMargin.Create(Fowner);
  FItemRectangleMargin.OnChange := ItemMarginChanged;
  FItemRectangleMargin.Left := 3;
  FItemRectangleMargin.Top := 3;
  FItemRectangleMargin.Right := 3;
  FItemRectangleMargin.Bottom := 3;
  FAutoItemHeight := true;

  if Assigned(FOwner) then
  begin
    if Assigned(FOwner.FWinC) then
    begin
      if FOwner.FWinC.GetInterface(ITMSMegaMenu, tmsif) then
      begin
        if Assigned(tmsif.GetDefaultSection) then
          Assign(tmsif.GetDefaultSection);
      end;
    end;
  end;
  Changed;
end;

destructor TGDIPMenuSection.Destroy;
begin
  FBackGroundFill.Free;
  FcaptionFill.Free;
  FItems.Free;
  FCaptionFont.Free;
  FItemMargin.Free;
  FItemRectangleMargin.Free;
  inherited;
  if Assigned(FOwner) then
  begin
    if not FOwner.Fdestroying then
      Changed;
  end;
end;

procedure TGDIPMenuSection.Draw(g: TGPGraphics; r: TGPRectF);
var
  f: TGPFont;
  fs: integer;
  ff: TGPFontFamily;
  conr, sizer, capr, rd: TGPRectF;
  sf: TGPStringFormat;
  x, y: Double;
  b: TGPSolidBrush;
  i: integer;
  chkh, chks: Boolean;
begin
  rd := GetSectionRect(r);
  //section background
  conr := GetContentRect(rd);
  BackGroundFill.Fill(g, conr);
  //section caption
  if CaptionSize > 0 then
  begin
    capr := GetCaptionRect(rd);
    CaptionFill.Fill(g, capr);
    ff := TGPFontFamily.Create(CaptionFont.Name);
    if (ff.Status in [FontFamilyNotFound, FontStyleNotFound]) then
    begin
      ff.Free;
      ff := TGPFontFamily.Create('Arial');
    end;

    fs := 0;
    if (fsBold in CaptionFont.Style) then
      fs := fs + 1;
    if (fsItalic in CaptionFont.Style) then
      fs := fs + 2;
    if (fsUnderline in CaptionFont.Style) then
      fs := fs + 4;

    sf := TGPStringFormat.Create;
    f := TGPFont.Create(ff, CaptionFont.Size , fs, UnitPoint);

    capr := MakeRect(capr.X + 2, capr.Y + 2, capr.Width - 4, capr.Height - 4);
    g.MeasureString(FCaption, Length(FCaption), f, capr,sf, sizer);

    case CaptionLocation of
      mlCustom:
      begin
        x := CaptionLeft;
        y := CaptionTop;
      end
      else
        FOwner.GetLocation(x, y, capr, sizer.Width, sizer.Height, CaptionLocation);
    end;

    b := TGPSolidBrush.Create(MakeColor(255, CaptionFont.Color));
    g.DrawString(Caption, Length(Caption), f, MakePoint(x, y), sf, b);

    b.Free;
    f.Free;
    sf.Free;
    ff.Free;

  end;

  chkh := Assigned(FOwner.FHoveredItem.section) and (FOwner.FHoveredItem.section.Index = Self.Index);
  chks := Assigned(FOwner.FSelectedItem.section) and (FOwner.FSelectedItem.section.Index = Self.Index);
  FBR := 0;
  for I := 0 to Items.Count - 1 do
    Items[I].Draw(g, rd, Items[I].FItemRect, Items[I].FFillRect, chkh, chks);
end;

procedure TGDIPMenuSection.FontChanged(Sender: TObject);
begin
  Changed;
end;

function TGDIPMenuSection.GetCaptionRect(r: TGPRectF): TGPRectF;
begin
  Result.X := r.x;
  Result.Y := r.Y;
  Result.Height := CaptionSize;
  Result.Width := r.Width;
end;

function TGDIPMenuSection.GetContentRect(r: TGPRectF): TGPRectF;
var
  cr: TGPRectF;
begin
  cr := GetCaptionRect(r);
  result.X := cr.X;
  result.Y := cr.Y + cr.Height;
  result.Width := cr.Width;
  Result.Height := r.Height - cr.Height;
end;

function TGDIPMenuSection.GetHeight: Double;
begin
  if FOwner.FAutoSectionSize then
    Result := FAutoHeight
  else
    Result := Height;
end;

function TGDIPMenuSection.GetSectionInsideRect(r: TGPRectF): TGPRectF;
var
  bw: integer;
begin
  Result := GetSectionrect(r);
  if BackGroundFill.BorderColor <> clNone then
  begin
    bw := BackGroundFill.BorderWidth;
    result.X := Result.X + bw;
    result.Width := Result.Width - (bw * 2);
    Result.y := Result.Y + bw;
    result.Height := result.Height - (bw * 2);
  end;
end;

function TGDIPMenuSection.GetSectionRect(r: TGPRectF): TGPRectF;
var
  x, y, wsec, hsec: Double;
  i: integer;
begin
  x := r.X;
  y := r.Y;
  wsec := r.Width;
  hsec := r.Height;
  case FOwner.SectionLayout of
    slHorizontal: wsec := GetWidth;
    slVertical: hsec := GetHeight;
  end;
  for I := 0 to Index - 1 do
  begin
    case FOwner.SectionLayout of
      slHorizontal: x := x + FOwner.Sections[I].GetWidth;
      slVertical: y := y + FOwner.Sections[I].GetHeight;
    end;
  end;

  Result := MakeRect(x, y, wsec, hsec);
end;

function TGDIPMenuSection.GetWidth: double;
begin
  if FOwner.FAutoSectionSize then
    Result := FAutoWidth
  else
    Result := FWidth;
end;

procedure TGDIPMenuSection.ItemMarginChanged(Sender: TObject);
begin
  Changed;
end;

procedure TGDIPMenuSection.ItemsChanged(Sender: TObject);
begin
  Changed;
end;

procedure TGDIPMenuSection.LoadFromFile(ini: TIniFile; Section: String);
begin
  LoadFont(ini, Section + '.CaptionFont', CaptionFont);
  CaptionFill.LoadFromFile(ini, Section + '.CaptionFill');
  BackGroundFill.LoadFromFile(ini, Section + '.BackGroundFill');
end;

procedure TGDIPMenuSection.SaveToFile(ini: TIniFile; Section: String);
begin
  SaveFont(ini, Section + '.CaptionFont', CaptionFont);
  CaptionFill.SaveToFile(ini, Section + '.CaptionFill');
  BackGroundFill.SaveToFile(ini, Section + '.BackGroundFill');
end;

procedure TGDIPMenuSection.SetAutoItemHeight(const Value: boolean);
begin
  if FAutoItemHeight <> value then
  begin
    FAutoItemHeight := Value;
    Changed;
  end;
end;

procedure TGDIPMenuSection.SetBackGroundFill(const Value: TGDIPFill);
begin
  if FBackGroundFill <> value then
  begin
    FBackGroundFill.Assign(Value);
    Changed;
  end;
end;

procedure TGDIPMenuSection.SetCaption(const Value: String);
begin
  if FCaption <> value then
  begin
    FCaption := Value;
    Changed;
  end;
end;

procedure TGDIPMenuSection.SetCaptionFill(const Value: TGDIPFill);
begin
  if FCaptionFill <> value then
  begin
    FCaptionFill.Assign(Value);
    Changed;
  end;
end;

procedure TGDIPMenuSection.SetCaptionFont(const Value: TFont);
begin
  if FCaptionFont <> value then
  begin
    FCaptionFont.Assign(Value);
    Changed;
  end;
end;

procedure TGDIPMenuSection.SetCaptionLeft(const Value: integer);
begin
  if FCaptionLeft <> value then
  begin
    FCaptionLeft := Value;
    Changed;
  end;
end;

procedure TGDIPMenuSection.SetCaptionLocation(const Value: TGDIPMenuLocation);
begin
  if FCaptionLocation <> value then
  begin
    FCaptionLocation := Value;
    Changed;
  end;
end;

procedure TGDIPMenuSection.SetCaptionSize(const Value: integer);
begin
  if FCaptionSize <> value then
  begin
    FCaptionSize := Value;
    Changed;
  end;
end;

procedure TGDIPMenuSection.SetCaptionTop(const Value: integer);
begin
  if FCaptionTop <> value then
  begin
    FCaptionTop := Value;
    Changed;
  end;
end;

procedure TGDIPMenuSection.SetColorTones(ATones: TColorTones);
begin
  BackGroundFill.Color := ATones.Background.BrushColor;
  BackGroundFill.ColorTo := ATones.Background.BrushColor;
  BackGroundFill.BorderColor := ATones.Background.BorderColor;

  CaptionFill.Color := ATones.Foreground.BrushColor;
  CaptionFill.ColorTo := ATones.Foreground.BrushColor;
  CaptionFill.ColorMirror := ATones.Foreground.BrushColor;
  CaptionFill.ColorMirrorTo := ATones.Foreground.BrushColor;
  //CaptionFill.BorderColor := $E0B99B;
  CaptionFill.BorderColor := ATones.Foreground.BorderColor;
  CaptionFill.GradientMirrorType := gtVertical;

  CaptionFont.Color := ATones.Foreground.TextColor;
end;

procedure TGDIPMenuSection.SetComponentStyle(AStyle: TTMSStyle);
begin
  // TODO : do color settings here
 case AStyle of
    tsOffice2003Blue:
      begin
        BackGroundFill.Color := $00FFD2AF;
        BackGroundFill.ColorTo := $00FFD2AF;
        BackGroundFill.BorderColor := clNone;

        CaptionFill.Color := $EEDBC8;
        CaptionFill.ColorTo := $F6DDC9;
        CaptionFill.ColorMirror := $EDD4C0;
        CaptionFill.ColorMirrorTo := $F7E1D0;
        //CaptionFill.BorderColor := $E0B99B;
        CaptionFill.BorderColor := clNone;
        CaptionFill.GradientMirrorType := gtVertical;

      end;
    tsOffice2003Silver:
      begin
        BackGroundFill.Color := $00E6D8D8;
        BackGroundFill.ColorTo := $00E6D8D8;
        BackGroundFill.BorderColor := clNone;

        CaptionFill.Color := $E6E9E2;
        CaptionFill.ColorTo := $00E6D8D8;
        CaptionFill.ColorMirror := $C8B2B3;
        CaptionFill.ColorMirrorTo := $E6E9E2;
        //CaptionFill.BorderColor := $927476;
        CaptionFill.BorderColor := clNone;
        CaptionFill.GradientMirrorType := gtVertical;
      end;
    tsOffice2003Olive:
      begin
        BackGroundFill.Color := RGB(225, 234, 185);
        BackGroundFill.ColorTo := RGB(225, 234, 185);
        BackGroundFill.BorderColor := clNone;

        CaptionFill.Color := $CFF0EA;
        CaptionFill.ColorTo := $CFF0EA;
        CaptionFill.ColorMirror := $8CC0B1;
        CaptionFill.ColorMirrorTo := $CFF0EA;
//        CaptionFill.BorderColor := $8CC0B1;
        CaptionFill.BorderColor := clNone;
        CaptionFill.GradientMirrorType := gtVertical;

      end;
    tsOffice2003Classic:
      begin
        BackGroundFill.Color := $00F2F2F2;
        BackGroundFill.ColorTo := $00F2F2F2;
        BackGroundFill.BorderColor := clNone;

        CaptionFill.Color := clWhite;
        CaptionFill.ColorTo := $C9D1D5;
        CaptionFill.ColorMirror := clNone;
        CaptionFill.ColorMirrorTo := clNone;
//        CaptionFill.BorderColor := $8CC0B1;
        CaptionFill.BorderColor := clNone;
        CaptionFill.GradientMirrorType := gtVertical;
      end;
    tsOffice2007Luna:
      begin
        BackGroundFill.Color := $00F3E5DA;
        BackGroundFill.ColorTo := $00F0DED0;
        BackGroundFill.BorderColor := clNone;

        CaptionFill.Color := $FFEFE3;
        CaptionFill.ColorTo := $FFDDC4;
        CaptionFill.ColorMirror := $FFD1AD;
        CaptionFill.ColorMirrorTo := $FFDBC0;
//        CaptionFill.BorderColor := $FFD1AD;
        CaptionFill.BorderColor := clNone;
        CaptionFill.GradientMirrorType := gtVertical;

      end;
    tsOffice2007Obsidian:
      begin
        BackGroundFill.Color := $5C534C;
        BackGroundFill.ColorTo := $5C534C;
        BackGroundFill.BorderColor := clNone;

        CaptionFill.Color := $F9F8F8;
        CaptionFill.ColorTo := $E4E2DF;
        CaptionFill.ColorMirror := $D1CBC7;
        CaptionFill.ColorMirrorTo := $E2DEDB;
//        CaptionFill.BorderColor := $D1CBC7;
        CaptionFill.BorderColor := clNone;
        CaptionFill.GradientMirrorType := gtVertical;

      end;
    tsWindowsXP:
      begin
        BackGroundFill.Color := $00B6B6B6;
        BackGroundFill.ColorTo := $00B6B6B6;

        CaptionFill.Color := clBtnFace;//clWhite;
        CaptionFill.ColorTo := clBtnFace;//$B9D8DC;
        CaptionFill.ColorMirror := clNone;
        CaptionFill.ColorMirrorTo := clNone;
//        CaptionFill.BorderColor := $B9D8DC;
        CaptionFill.BorderColor := clNone;
        CaptionFill.GradientMirrorType := gtVertical;

      end;
    tsWhidbey:
      begin
        BackGroundFill.Color := $F5F9FA;
        BackGroundFill.ColorTo := $F5F9FA;
        BackGroundFill.BorderColor := clNone;

        CaptionFill.Color := clWhite;
        CaptionFill.ColorTo := $DFEDF0;
        CaptionFill.ColorMirror := $DFEDF0;
        CaptionFill.ColorMirrorTo := $DFEDF0;
        //CaptionFill.BorderColor := $99A8AC;
        CaptionFill.BorderColor := clNone;
        CaptionFill.GradientMirrorType := gtVertical;

      end;
    tsCustom: ;
    tsOffice2007Silver:
      begin
        BackGroundFill.Color := RGB(241, 244, 248);
        BackGroundFill.ColorTo := RGB(227, 232, 240);
        BackGroundFill.BorderColor := clNone;

        CaptionFill.Color := $F9F8F8;
        CaptionFill.ColorTo := $E4E2DF;
        CaptionFill.ColorMirror := $D1CBC7;
        CaptionFill.ColorMirrorTo := $E2DEDB;
        //CaptionFill.BorderColor := $D1CBC7;
        CaptionFill.BorderColor := clNone;
        CaptionFill.GradientMirrorType := gtVertical;

      end;

    tsWindowsVista:
      begin
        BackGroundFill.Color := $FFFFFF;
        BackGroundFill.ColorTo := $FFFFFF;
        BackGroundFill.BorderColor := clNone;

        CaptionFill.Color := $FEF9F0;
        CaptionFill.ColorTo := $FDF0D7;
        CaptionFill.ColorMirror := clNone;
        CaptionFill.ColorMirrorTo := clNone;
        CaptionFill.BorderColor := $FCF2DA;
        CaptionFill.GradientMirrorType := gtVertical;

      end;
    tsWindows7:
      begin
        BackGroundFill.Color := $FFFFFF;
        BackGroundFill.ColorTo := $FFFFFF;
        BackGroundFill.BorderColor := clNone;

        CaptionFill.Color := $FCEBDC;
        CaptionFill.ColorTo := $FCDBC1;
        CaptionFill.ColorMirror := clNone;
        CaptionFill.ColorMirrorTo := clNone;
        CaptionFill.BorderColor := $CEA27D;
        CaptionFill.GradientMirrorType := gtVertical;

      end;
    tsTerminal:
      begin
        BackGroundFill.Color := clBtnFace;
        BackGroundFill.ColorTo := clBtnFace;
        BackGroundFill.BorderColor := clNone;

        CaptionFill.Color := clSilver;
        CaptionFill.ColorTo := clSilver;
        CaptionFill.ColorMirror := clNone;
        CaptionFill.ColorMirrorTo := clNone;
        CaptionFill.BorderColor := clGray;
      end;
       tsOffice2010Blue:
      begin
        BackGroundFill.Color := clWhite;
        BackGroundFill.ColorTo := RGB(237, 239, 241);
        BackGroundFill.BorderColor := RGB(236, 237, 237);

        CaptionFill.Color := $D9C3B1;
        CaptionFill.ColorTo := clNone;
        CaptionFill.ColorMirror := clNone;
        CaptionFill.ColorMirrorTo := clNone;
        CaptionFill.BorderColor := clNone;
        CaptionFill.GradientMirrorType := gtVertical;

      end;
       tsOffice2010Silver:
      begin
        BackGroundFill.Color := clWhite;
        BackGroundFill.ColorTo := RGB(237, 239, 241);
        BackGroundFill.BorderColor := RGB(236, 237, 237);

        CaptionFill.Color := $DBD4CE;
        CaptionFill.ColorTo := clNone;
        CaptionFill.ColorMirror := clNone;
        CaptionFill.ColorMirrorTo := clNone;
        CaptionFill.BorderColor := clNone;
        CaptionFill.GradientMirrorType := gtVertical;

      end;
       tsOffice2010Black:
      begin
        BackGroundFill.Color := clWhite;
        BackGroundFill.ColorTo := RGB(237, 239, 241);
        BackGroundFill.BorderColor := RGB(236, 237, 237);

        CaptionFill.Color := $7C7C7C;
        CaptionFill.ColorTo := clNone;
        CaptionFill.ColorMirror := clNone;
        CaptionFill.ColorMirrorTo := clNone;
        CaptionFill.BorderColor := clNone;
        CaptionFill.GradientMirrorType := gtVertical;

      end;
    tsWindows8, tsWindows10:
      begin
        BackGroundFill.Color := clWhite;
        BackGroundFill.ColorTo := clWhite;
        BackGroundFill.BorderColor := $E4E3E2;

        CaptionFill.Color := $F7F6F5;
        CaptionFill.ColorTo := clNone;
        CaptionFill.ColorMirror := clNone;
        CaptionFill.ColorMirrorTo := clNone;
        CaptionFill.BorderColor := clNone;
        CaptionFill.GradientMirrorType := gtVertical;

      end;

    tsOffice2013White:
      begin
        BackGroundFill.Color := clWhite;
        BackGroundFill.ColorTo := clWhite;
        BackGroundFill.BorderColor := $D4D4D4;

        CaptionFill.Color := clWhite;
        CaptionFill.ColorTo := clNone;
        CaptionFill.ColorMirror := clNone;
        CaptionFill.ColorMirrorTo := clNone;
        CaptionFill.BorderColor := clNone;
        CaptionFill.GradientMirrorType := gtVertical;

      end;

    tsOffice2013LightGray:
      begin
        BackGroundFill.Color := clWhite;
        BackGroundFill.ColorTo := clWhite;
        BackGroundFill.BorderColor := $C6C6C6;

        CaptionFill.Color := $F6F6F6;
        CaptionFill.ColorTo := clNone;
        CaptionFill.ColorMirror := clNone;
        CaptionFill.ColorMirrorTo := clNone;
        CaptionFill.BorderColor := clNone;
        CaptionFill.GradientMirrorType := gtVertical;

      end;

    tsOffice2013Gray:
      begin
        BackGroundFill.Color := clWhite;
        BackGroundFill.ColorTo := clWhite;
        BackGroundFill.BorderColor := $ABABAB;

        CaptionFill.Color := $E5E5E5;
        CaptionFill.ColorTo := clNone;
        CaptionFill.ColorMirror := clNone;
        CaptionFill.ColorMirrorTo := clNone;
        CaptionFill.BorderColor := clNone;
        CaptionFill.GradientMirrorType := gtVertical;

      end;
   tsOffice2016White:
      begin
        BackGroundFill.Color := clWhite;
        BackGroundFill.ColorTo := clWhite;
        BackGroundFill.BorderColor := $D4D4D4;

        CaptionFill.Color := clWhite;
        CaptionFill.ColorTo := clNone;
        CaptionFill.ColorMirror := clNone;
        CaptionFill.ColorMirrorTo := clNone;
        CaptionFill.BorderColor := $D4D4D4;
        CaptionFill.GradientMirrorType := gtVertical;

      end;

    tsOffice2016Gray:
      begin
        BackGroundFill.Color := $B2B2B2;
        BackGroundFill.ColorTo := $B2B2B2;
        BackGroundFill.BorderColor := $444444;

        CaptionFill.Color := $444444;
        CaptionFill.ColorTo := clNone;
        CaptionFill.ColorMirror := clNone;
        CaptionFill.ColorMirrorTo := clNone;
        CaptionFill.BorderColor := $444444;
        CaptionFill.GradientMirrorType := gtVertical;

      end;

    tsOffice2016Black:
      begin
        BackGroundFill.Color := $363636;
        BackGroundFill.ColorTo := $363636;
        BackGroundFill.BorderColor := $ABABAB;

        CaptionFill.Color := $444444;
        CaptionFill.ColorTo := clNone;
        CaptionFill.ColorMirror := clNone;
        CaptionFill.ColorMirrorTo := clNone;
        CaptionFill.BorderColor := $444444;
        CaptionFill.GradientMirrorType := gtVertical;

      end;


  end;

  ItemMargin.Left := 0;
  ItemMargin.Top := 0;
  ItemMargin.Right := 0;
  ItemMargin.Bottom := 0;

  ItemSpacing := 0;
end;

procedure TGDIPMenuSection.SetHeight(const Value: integer);
begin
  if FHeight <> Value then
  begin
    FHeight := Value;
    Changed;
  end;
end;

procedure TGDIPMenuSection.SetItemMargin(const Value: TGDIPMenuMargin);
begin
  if FItemMargin <> value then
  begin
    FItemMargin.Assign(Value);
    Changed;
  end;
end;

procedure TGDIPMenuSection.SetItemRectangleMargin(const Value: TGDIPMenuMargin);
begin
  if FItemRectangleMargin <> value then
  begin
    FItemRectangleMargin := Value;
    Changed;
  end;
end;

procedure TGDIPMenuSection.SetItems(const Value: TGDIPMenuSectionItems);
begin
  if FItems <> value then
  begin
    FItems.Assign(Value);
    Changed;
  end;
end;

procedure TGDIPMenuSection.SetItemSpacing(const Value: integer);
begin
  if FItemSpacing <> value then
  begin
    FItemSpacing := Value;
    Changed;
  end;
end;

procedure TGDIPMenuSection.SetVisible(const Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    Changed;
  end;
end;

procedure TGDIPMenuSection.SetWidth(const Value: integer);
begin
  if FWidth <> value then
  begin
    FWidth := Value;
    if Assigned(FOwner) then
      FOwner.ReBuildTopLayerItems;
    Changed;
  end;
end;

{ TGDIPMenuSectionItems }

function TGDIPMenuSectionItems.Add: TGDIPMenuSectionItem;
begin
  Result := TGDIPMenuSectionItem(inherited Add);
end;

constructor TGDIPMenuSectionItems.Create(AOwner: TGDIPMenu; ASectionOwner: TGDIPMenuSection);
begin
  inherited Create(AOwner, TGDIPMenuSectionItem);
  FSectionOwner := ASectionOwner;
  FOwner := AOwner;
end;

procedure TGDIPMenuSectionItems.Delete(Index: Integer);
begin
  Items[Index].Free;
end;

function TGDIPMenuSectionItems.GetItem(Index: Integer): TGDIPMenuSectionItem;
begin
  Result := TGDIPMenuSectionItem(inherited Items[Index]);
end;

function TGDIPMenuSectionItems.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TGDIPMenuSectionItems.Insert(Index: Integer): TGDIPMenuSectionItem;
begin
  Result := TGDIPMenuSectionItem(inherited insert(Index));
end;

procedure TGDIPMenuSectionItems.SetItem(Index: Integer;
  const Value: TGDIPMenuSectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TGDIPMenuMargin }

procedure TGDIPMenuMargin.Assign(Source: TPersistent);
begin
  if (Source is TGDIPMenuMargin) then
  begin
    FLeft := (Source as TGDIPMenuMargin).Left;
    FTop := (Source as TGDIPMenuMargin).Top;
    FBottom := (Source as TGDIPMenuMargin).Bottom;
    FRight := (Source as TGDIPMenuMargin).Right;
  end;
end;

procedure TGDIPMenuMargin.Changed;
begin
  if Assigned(FOwner) then
  begin
    FOwner.FNeedsUpdate := true;
    FOwner.Changed;
  end;
end;

constructor TGDIPMenuMargin.Create(AOwner: TGDIPMenu);
begin
  FOwner := AOwner;
  FLeft := 0;
  FRight := 0;
  FBottom := 0;
  FTop := 0;
end;

destructor TGDIPMenuMargin.Destroy;
begin
  inherited;
end;

procedure TGDIPMenuMargin.SetBottom(const Value: integer);
begin
  if FBottom <> value then
  begin
    FBottom := Value;
    changed;
  end;
end;

procedure TGDIPMenuMargin.SetLeft(const Value: integer);
begin
  if FLeft <> value then
  begin
    FLeft := Value;
    Changed;
  end;
end;

procedure TGDIPMenuMargin.SetRight(const Value: integer);
begin
  if FRight <> value then
  begin
    FRight := Value;
    Changed;
  end;
end;

procedure TGDIPMenuMargin.SetTop(const Value: integer);
begin
  if FTop <> value then
  begin
    FTop := Value;
    Changed;
  end;
end;

{ TGDIPMenuTopLayerItem }

procedure TGDIPMenuTopLayerItem.Assign(Source: TPersistent);
begin
  if Source is TGDIPMenuTopLayerItem then
  begin
    FVisible := (Source as TGDIPMenuTopLayerItem).Visible;
    FTop := (Source as TGDIPMenuTopLayerItem).Top;
    FLeft := (Source as TGDIPMenuTopLayerItem).Left;
    FHeight := (Source as TGDIPMenuTopLayerItem).Height;
    FWidth := (Source as TGDIPMenuTopLayerItem).Width;
    FFill.Assign((Source as TGDIPMenuTopLayerItem).Fill);
    FAlign := (Source as TGDIPMenuTopLayerItem).Align;
    FHTMLText := (Source as TGDIPMenuTopLayerItem).HTMLText;
    FHTMLTextLocation := (Source as TGDIPMenuTopLayerItem).HTMLTextLocation;
    FHTMLTextTop := (Source as TGDIPMenuTopLayerItem).HTMLTextTop;
    FHTMLTextLeft := (Source as TGDIPMenuTopLayerItem).HTMLTextLeft;
    FHTMLTextFont.Assign((Source as TGDIPMenuTopLayerItem).HTMLTextFont);
    Changed;
  end;
end;

procedure TGDIPMenuTopLayerItem.Changed;
begin
  if Assigned(FOwner) then
  begin
    FOwner.FNeedsUpdate := true;
    FOwner.Changed;
  end;
end;

constructor TGDIPMenuTopLayerItem.Create(Collection: TCollection);
var
  tmsif: ITMSMegaMenu;
begin
  inherited;
  FAlign := alCustom;
  FOwner := (Collection as TGDIPMenuTopLayerItems).FOwner;
  FWidth := 100;
  FHeight := 100;
  FVisible := true;
  FLeft := 0;
  FTop := 0;
  FFill := TGDIPFill.Create;
  FFill.OnChange := FillChanged;
  FHTMLTextLocation := mlCenterCenter;
  FHTMLTextTop := 0;
  FHTMLTextLeft := 0;
  FHTMLTextFont := TFont.Create;
  FHTMLTextFont.OnChange := HTMLFontChanged;

  if Assigned(FOwner) then
  begin
    if Assigned(FOwner.FWinC) then
    begin
      if FOwner.FWinC.GetInterface(ITMSMegaMenu, tmsif) then
      begin
        if Assigned(tmsif.GetDefaultTopLayerItem) then
          Assign(tmsif.GetDefaultTopLayerItem);
      end;
    end;
    FOwner.FNeedsUpdate := true;
    FOwner.Changed;
  end;
end;

destructor TGDIPMenuTopLayerItem.Destroy;
begin
  FFill.Free;
  FHTMLTextFont.Free;
  inherited;
end;

procedure TGDIPMenuTopLayerItem.Draw(g: TGPGraphics; r: TGPRectF; Detailtxt: TGDIPMenuHTMLText);
var
  rd: TGPRectF;
  item: TGDIPMenuSectionItem;
begin
  if FVisible then
  begin
    //FILL
    rd := GetTopLayerRect;
    Fill.Fill(g, rd);

    if Assigned(FOwner.FHoveredItem.item) and (FOwner.FHoveredItem.item.HoverTopLayerItem = Self.Index) then
    begin
      item := FOwner.FHoveredItem.item;
      detailtxt.Left := item.DetailTextLeft;
      detailtxt.Top := item.DetailTextTop;
      Fowner.DrawTopLayerHTMLText(g, FOwner.ItemAppearance.DetailFont, Detailtxt, item.DetailTextLocation, rd, item.DetailText);
    end
    else
    if Assigned(FOwner.FSelectedItem.item) and (FOwner.FSelectedItem.item.SelectedTopLayerItem = Self.Index) then
    begin
      item := FOwner.FSelectedItem.item;
      detailtxt.Left := item.DetailTextLeft;
      detailtxt.Top := item.DetailTextTop;
      Fowner.DrawTopLayerHTMLText(g, FOwner.ItemAppearance.DetailFont, Detailtxt, item.DetailTextLocation, rd, item.DetailText);
    end
    else
    begin
      //default html text
      FOwner.DrawTopLayerHTMLText(g, HTMLTextFont, detailtxt, HTMLTextLocation, rd, FHTMLText);
    end;
  end;
end;

procedure TGDIPMenuTopLayerItem.FillChanged(Sender: TObject);
begin
  Changed;
end;

function TGDIPMenuTopLayerItem.GetTopLayerRect: TGPRectF;
begin
  Result := FTopLayerRect;
end;

procedure TGDIPMenuTopLayerItem.HTMLFontChanged(Sender: TObject);
begin
  Changed;
end;

procedure TGDIPMenuTopLayerItem.HTMLTextChanged(Sender: TObject);
begin
  Changed;
end;

procedure TGDIPMenuTopLayerItem.LoadFromFile(ini: TIniFile; Section: String);
begin
  Fill.LoadFromFile(ini, Section + '.Fill');
end;

procedure TGDIPMenuTopLayerItem.SaveToFile(ini: TIniFile; Section: String);
begin
  Fill.SaveToFile(ini, Section + '.Fill');
end;

procedure TGDIPMenuTopLayerItem.SetAlign(const Value: TAlign);
begin
  if FAlign <> value then
  begin
    FAlign := Value;
    if Assigned(FOwner) then
      FOwner.ReBuildTopLayerItems;
    Changed;
  end;
end;

procedure TGDIPMenuTopLayerItem.SetFill(const Value: TGDIPFill);
begin
  if FFill <> value then
  begin
    FFill := Value;
    Changed;
  end;
end;

procedure TGDIPMenuTopLayerItem.SetHeight(const Value: integer);
begin
  if FHeight <> Value then
  begin
    FHeight := Value;
    if Assigned(FOwner) then
      FOwner.ReBuildTopLayerItems;
    Changed;
  end;
end;

procedure TGDIPMenuTopLayerItem.SetHTMLLocation(const Value: TGDIPMenuLocation);
begin
  if FHTMLTextLocation <> value then
  begin
    FHTMLTextLocation := Value;
    Changed;
  end;
end;

procedure TGDIPMenuTopLayerItem.SetHTMLText(const Value: String);
begin
  if FHTMLText <> value then
  begin
    FHTMLText := Value;
    Changed;
  end;
end;

procedure TGDIPMenuTopLayerItem.SetHTMLTextFont(const Value: TFont);
begin
  if FHTMLTextFont <> value then
  begin
    FHTMLTextFont.Assign(Value);
    Changed;
  end;
end;

procedure TGDIPMenuTopLayerItem.SetHTMLTextLeft(const Value: integer);
begin
  if FHTMLTextLeft <> value then
  begin
    FHTMLTextLeft := Value;
    Changed;
  end;
end;

procedure TGDIPMenuTopLayerItem.SetHTMLTextTop(const Value: integer);
begin
  if FHTMLTextTop <> value then
  begin
    FHTMLTextTop := Value;
    Changed;
  end;
end;

procedure TGDIPMenuTopLayerItem.SetLeft(const Value: integer);
begin
  if FLeft <> value then
  begin
    FLeft := Value;
    if Assigned(FOwner) then
      FOwner.ReBuildTopLayerItems;
    Changed;
  end;
end;

procedure TGDIPMenuTopLayerItem.SetTop(const Value: integer);
begin
  if FTop <> value then
  begin
    FTop := Value;
    if Assigned(FOwner) then
      FOwner.ReBuildTopLayerItems;
    Changed;
  end;
end;

procedure TGDIPMenuTopLayerItem.SetVisible(const Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    Changed;
  end;
end;

procedure TGDIPMenuTopLayerItem.SetWidth(const Value: integer);
begin
  if FWidth <> Value then
  begin
    FWidth := Value;
    Changed;
  end;
end;

{ TGDIPMenuHTMLText }

procedure TGDIPMenuHTMLText.Assign(Source: TPersistent);
begin
  if (Source is TGDIPMenuHTMLText) then
  begin
    FURLColor := (Source as TGDIPMenuHTMLText).URLColor;
    FShadowOffset := (Source as TGDIPMenuHTMLText).ShadowOffset;
    FText := (Source as TGDIPMenuHTMLText).Text;
    FShadowColor := (Source as TGDIPMenuHTMLText).ShadowColor;
    FTop := (Source as TGDIPMenuHTMLText).Top;
    FLeft := (Source as TGDIPMenuHTMLText).Left;
  end;
end;

procedure TGDIPMenuHTMLText.Changed;
begin
  if not FDisableRepaint and Assigned(FOwner) then
  begin
    FOwner.Changed;
  end;
end;

constructor TGDIPMenuHTMLText.Create(AOwner: TGDIPMenu);
begin
  FOwner := AOwner;
  FURLColor := clBlue;
  FShadowOffset := 5;
  FShadowColor := clGray;
  FTop := 0;
  FLeft := 0;
end;

destructor TGDIPMenuHTMLText.Destroy;
begin
  inherited;
end;

procedure TGDIPMenuHTMLText.FontChanged(Sender: TObject);
begin
  Changed;
end;

procedure TGDIPMenuHTMLText.SetLeft(const Value: integer);
begin
  if FLeft <> value then
  begin
    FLeft := Value;
    Changed;
  end;
end;

procedure TGDIPMenuHTMLText.SetShadowColor(const Value: TColor);
begin
  if FShadowColor <> value then
  begin
    FShadowColor := Value;
    Changed;
  end;
end;

procedure TGDIPMenuHTMLText.SetShadowOffset(const Value: integer);
begin
  if FShadowOffset <> value then
  begin
    FShadowOffset := Value;
    Changed;
  end;
end;

procedure TGDIPMenuHTMLText.SetText(const Value: string);
begin
  if FText <> Value then
  begin
    FText := Value;
    Changed;
  end;
end;

procedure TGDIPMenuHTMLText.SetTop(const Value: integer);
begin
  if FTop <> value then
  begin
    FTop := Value;
    Changed;
  end;
end;

procedure TGDIPMenuHTMLText.SetURLColor(const Value: TColor);
begin
  if FURLColor <> value then
  begin
    FURLColor := Value;
    Changed;
  end;
end;

{ TGDIPMenuSectionItem }

procedure TGDIPMenuSectionItem.Assign(Source: TPersistent);
begin
  if (Source is TGDIPMenuSectionItem) then
  begin
    FEdit := (Source as TGDIPMenuSectionItem).Edit;
    if Assigned(FEdit) then
      FEdit.OnChange := EditChange;

    FVisible := (Source as TGDIPMenuSectionItem).Visible;
    FControlType := (Source as TGDIPMenuSectionItem).ControlType;
    FItemType := (Source as TGDIPMenuSectionItem).ItemType;
    FText := (Source as TGDIPMenuSectionItem).Text;
    FData := (Source as TGDIPMenuSectionItem).Data;
    FHoverText := (Source as TGDIPMenuSectionItem).DetailText;
    FHoverTextLocation := (Source as TGDIPMenuSectionItem).DetailTextLocation;
    FHoverTextTop := (Source as TGDIPMenuSectionItem).DetailTextTop;
    FHoverTextLeft := (Source as TGDIPMenuSectionItem).DetailTextLeft;
    FChecked := (Source as TGDIPMenuSectionItem).Checked;
    FControlIndent := (Source as TGDIPMenuSectionItem).ControlIndent;
    FEnableFill := (Source as TGDIPMenuSectionItem).EnableFill;
    FControl := (Source as TGDIPMenuSectionItem).Control;
    FEnabled := (Source as TGDIPMenuSectionItem).Enabled;
    FGraphicLeftName := (Source as TGDIPMenuSectionItem).GraphicLeftName;
    FGraphicRightName := (Source as TGDIPMenuSectionItem).GraphicRightName;
    FSelectedTopLayerItem := (Source as TGDIPMenuSectionItem).SelectedTopLayerItem;
    FHoverTopLayerItem := (Source as TGDIPMenuSectionItem).HoverTopLayerItem;
    FHeight := (Source as TGDIPMenuSectionItem).Height;
    FTextLocation := (Source as TGDIPMenuSectionItem).TextLocation;
    FTextLeft := (Source as TGDIPMenuSectionItem).TextLeft;
    FTextTop := (Source as TGDIPMenuSectionItem).TextTop;
    FTag := (Source as TGDIPMenuSectionItem).Tag;
    FItemObject := (Source as TGDIPMenuSectionItem).ItemObject;
    FHideOnSelection := (Source as TGDIPMenuSectionItem).HideOnSelection;
    FShortCut := (Source as TGDIPMenuSectionItem).ShortCut;
    FImageIndex := (source as TGDIPMenuSectionItem).ImageIndex;
    FGroupIndex := (Source as TGDIPMenuSectionItem).GroupIndex;
    Changed;
  end;
end;

procedure TGDIPMenuSectionItem.Changed;
begin
  if Assigned(FOwner) then
  begin
    FOwner.FNeedsUpdate := true;
    FOwner.Changed;
  end;
end;

procedure TGDIPMenuSectionItem.ControlItemsChange(Sender: TObject);
begin
  Changed;
end;

constructor TGDIPMenuSectionItem.Create(Collection: TCollection);
begin
  inherited;
  FOwner := (Collection as TGDIPMenuSectionItems).FOwner;
  FSectionOwner := (Collection as TGDIPMenuSectionItems).FSectionOwner;
  FControlType := ctNone;
  FItemType := itNormal;
  FEnabled := True;
  FVisible := True;
  FHoverTopLayerItem := -1;
  FSelectedTopLayerItem := -1;
  FHoverTextLocation := mlCenterCenter;
  FChecked := false;
  FEnableFill := true;
  FControlIndent := 20;
  FGroupIndex := 0;
  FHoverTextLeft := 0;
  FHoverTextTop := 0;
  FHeight := 25;
  FTextLocation := mlCenterLeft;
  FTextTop := 0;
  FTextLeft := 0;
  FTag := 0;
  FHideOnSelection := true;
  FShortCut := 0;
  FImageIndex := -1;
  Changed;
end;

destructor TGDIPMenuSectionItem.Destroy;
begin
  if Assigned(fOwner) then
  begin
    if (Self = FOwner.FSelectedItem.item) then
      FOwner.FSelectedItem.item := nil;

    if (Self = FOwner.FHoveredItem.item) then
      FOwner.FHoveredItem.item := nil;
  end;
  inherited;
  if Assigned(FOwner) then
  begin
    if not FOwner.Fdestroying then
      Changed;
  end;
end;

procedure TGDIPMenuSectionItem.Draw(g: TGPGraphics; rd, r, rf: TGPRectF; DoItemHoverFill, DoItemSelectedFill: Boolean);
var
  htmlr: TGPRectF;
  cf: TFont;
  f: TGDIPFill;
  hdl, htheme: THandle;
  ThemeStyle: dword;
  rc: TRect;
  DChecked: Cardinal;
  picl, picr: TAdvGDIPPicture;
  capr: TGPRectF;
  fitemtxt: TGDIPMenuHTMLText;
  strShortCut: string;
  hdc: HWND;
  ca: TCanvas;
begin
  if Visible then
  begin
    fitemtxt := TGDIPMenuHTMLText.Create(nil);
    fitemtxt.URLColor := FOwner.ItemAppearance.URLColor;
    fitemtxt.ShadowColor := FOwner.ItemAppearance.ShadowColor;
    fitemtxt.ShadowOffset := FOwner.ItemAppearance.ShadowOffset;
    cf := TFont.Create;
    f := TGDIPFill.Create;
    capr := Section.GetCaptionRect(r);
    case ItemType of
      itLineSeparator: FOwner.ItemAppearance.SeparatorFill.Fill(g, MakeRect(rd.X, FItemRect.Y + FItemRect.Height / 2, rd.Width, FOwner.ItemAppearance.SeparatorSize));
      itLineBreak, itBreak:
      begin
        if ItemType = itLineBreak then
        begin
          if Fcount > 1 then
            FOwner.ItemAppearance.BreakFill.Fill(g, MakeRect(rd.X - Fowner.ItemAppearance.BreakSize  / 2 + (rd.Width / FCount) * (Section.FBR + 1), rd.Y + capr.Height, FOwner.ItemAppearance.BreakSize, rd.Height - capr.Height))
          else
            FOwner.ItemAppearance.BreakFill.Fill(g, MakeRect(rd.X - Fowner.ItemAppearance.BreakSize  / 2 + rd.Width, rd.Y + capr.Height, FOwner.ItemAppearance.BreakSize, rd.Height - capr.Height));
        end;

        Inc(Section.FBR);
      end;
      itHeader, itNormal:
      begin
        DChecked := 0;
        ThemeStyle := 0;
        if (ItemType = itNormal) then
        begin
          if Enabled then
          begin
            if DoItemSelectedFill and Assigned(FOwner.FSelectedItem.item) and (Index = FOwner.FSelectedItem.item.Index) and FOwner.ItemAppearance.AllowSelection then
            begin
              cf.Assign(FOwner.ItemAppearance.FontSelected);
              f.Assign(FOwner.ItemAppearance.FillSelected);
            end
            else if DoItemHoverFill and Assigned(FOwner.FHoveredItem.item) and (Index = FOwner.FHoveredItem.item.Index) then
            begin
              cf.Assign(FOwner.ItemAppearance.FontHover);
              f.Assign(FOwner.ItemAppearance.FillHover);
            end
            else
            begin
              cf.Assign(FOwner.ItemAppearance.Font);
              f.Assign(FOwner.ItemAppearance.Fill);
            end;
          end
          else
          begin
            cf.Assign(FOwner.ItemAppearance.FontDisabled);
            f.Assign(FOwner.ItemAppearance.FillDisabled);
          end;
        end
        else if ItemType = itHeader then
        begin
          cf.Assign(FOwner.ItemAppearance.Font);
        end;

        if Checked then
        begin
          case ControlType of
            ctCheckBox:
              begin
                DChecked := DFCS_BUTTONCHECK or DFCS_CHECKED;
                if FEnabled then
                  ThemeStyle := CBS_CHECKEDNORMAL
                else
                  ThemeStyle := CBS_CHECKEDDISABLED;
              end;
            ctRadioButton:
              begin
                DChecked := DFCS_BUTTONRADIO or DFCS_CHECKED;
                if FEnabled then
                  ThemeStyle := RBS_CHECKEDNORMAL
                else
                  ThemeStyle := RBS_CHECKEDDISABLED;
              end;
          end;
        end;

        if not Enabled then
        begin
          DChecked := DChecked or DFCS_INACTIVE;
        end;

        htmlr := r;
        if EnableFill and (ItemType = itNormal) then
          f.Fill(g, rf);

        fitemtxt.Text := FText;
        FOwner.DrawItemHTMLText(g, htmlr, cf, fitemtxt, r, FText, TextLocation, TextLeft, TextTop);
        strShortCut := ShortCutToText(ShortCut);
        if strShortCut <> '' then
        begin
          FItemtxt.Text := strShortCut;
          FOwner.DrawItemHTMLText(g, htmlr, FOwner.ItemAppearance.ShortCutFont, fitemtxt, r, strShortCut, mlCenterRight, -1, -1);
        end;

        with FOwner do
        begin
          rc := Bounds(Round(FFillRect.X), Round(FFillRect.Y + (FFillRect.Height - 17) / 2), 17, 17);
          hdl := g.GetHDC;
          case ControlType of
            ctCheckBox:
            begin
              if FIsWinXP and IsThemeActive then
              begin
                htheme := OpenThemeData(FWinC.Handle,'button');
                DrawThemeBackground(HTheme,hdl, BP_CHECKBOX,ThemeStyle,@rc,nil);
                CloseThemeData(htheme);
              end
              else
                DrawFrameControl(hdl,rc,DFC_BUTTON, DChecked);
            end;
            ctRadioButton:
            begin
              if FIsWinXP and IsThemeActive then
              begin
                htheme := OpenThemeData(FWinC.Handle,'button');
                DrawThemeBackground(HTheme,hdl, BP_RADIOBUTTON,ThemeStyle,@rc,nil);
                CloseThemeData(htheme);
              end
              else
                DrawFrameControl(hdl,rc,DFC_BUTTON, DChecked);
            end;
          end;
          g.ReleaseHDC(hdl);
        end;
        if Assigned(FOwner.PictureContainer) then
        begin
          picl := FOwner.PictureContainer.FindPicture(GraphicLeftName);
          if Assigned(picl) and not picl.Empty then
          begin
            picl.GetImageSizes;
            picl.GDIPDraw(g, Bounds(Round(FItemRect.x - picl.Width), Round(FItemRect.Y + (FItemRect.Height - picl.Height)/ 2), picl.Width, picl.Height));
          end;
          picr := FOwner.PictureContainer.FindPicture(GraphicRightName);
          if Assigned(picr) and not picr.Empty then
          begin
            picr.GetImageSizes;
            picr.GDIPDraw(g, Bounds(Round(FFillRect.x + FFillRect.Width - 2 - picr.Width), Round(FFillRect.Y + (FFillRect.Height - picr.Height) / 2), picr.Width, picr.Height));
          end;
        end;
        if Assigned(FOwner.ImageList) then
        begin
          hdc := g.GetHDC;
          ca := TCanvas.Create;
          ca.Handle := hdc;
          if (ImageIndex > -1) and (ImageIndex < FOwner.ImageList.Count) then
            FOwner.ImageList.Draw(ca, Round(FItemRect.X - Fowner.ImageList.Width), Round(FItemRect.Y + (FItemRect.Height - FOwner.ImageList.Height)/ 2), ImageIndex);
          ca.Free;
          g.releaseHdc(hdc);
        end;
      end;
    end;
    fitemtxt.Free;
    cf.Free;
    f.Free;
  end;
end;

procedure TGDIPMenuSectionItem.EditChange(Sender: TObject);
var
  str: String;
begin
  if Assigned(FOwner.FOnItemEditChanged) and Assigned(FEdit) then
  begin
    str := FEdit.Text;
    FOwner.FOnItemEditChanged(FOwner, str, Self);
  end;
end;

function TGDIPMenuSectionItem.GetAnchorAt(X, Y: integer): String;
var
  g: TGPGraphics;
  bmp: TBitmap;
  str: string;
  usehtml: boolean;
  htmlr: TRect;
  r: TGPRectF;
  s, k: String;
  l, m, xsize, ysize: integer;
  hr: TRect;
begin
  Result := '';
  str := Ftext;
  UseHTML := (Pos('</', str) > 0) or (Pos('/>', str) > 0) or (Pos('<BR>',uppercase(str)) > 0);
  r := FItemRect;
  htmlr := Bounds(Round(r.X), Round(r.Y), Round(r.Width), Round(r.Height));
  if PtInGPRect(r, Point(X, Y)) and UseHtml then
  begin
    bmp := TBitmap.Create;
    g := TGPGraphics.Create(bmp.Canvas.Handle);

    HTMLDrawGDIP(g, FOwner.ItemAppearance.Font, str, htmlr,FOwner.GetImageList, x,y,-1,-1,0,true,false,false,false,
      False,False,true,1.0,clBlue,clNone,clNone,clBlue,Result,s,k,xsize,ysize,l,m,hr,nil,FOwner.GetPictureContainer,2);

    g.Free;
    bmp.Free;
  end;
end;

procedure TGDIPMenuSectionItem.HoverTextChanged(Sender: TObject);
begin
  Changed;
end;

procedure TGDIPMenuSectionItem.HTMLChanged(Sender: TObject);
begin
  Changed;
end;

function TGDIPMenuSectionItem.ItemRect: TGPRectF;
begin
  Result := FItemRect;
end;

function TGDIPMenuSectionItem.Menu: TGDIPMenu;
begin
  result := FOwner;
end;

function TGDIPMenuSectionItem.Section: TGDIPMenuSection;
begin
  Result := FSectionOwner;
end;

procedure TGDIPMenuSectionItem.SetChecked(const Value: Boolean);
var
  i: integer;
begin
  if FChecked <> value then
  begin
    if Self.ControlType = ctRadioButton then
    begin
      for I := 0 to Section.Items.Count - 1 do
      begin
        if I <> Index then
        begin
          if (Section.Items[I].ControlType = ctRadioButton)
            and (Section.Items[I].GroupIndex = GroupIndex) then
          begin
            if Section.Items[I].FChecked then
            begin
              Section.Items[I].FChecked := false;
              if Assigned(Menu.OnItemCheckChanged) then
                menu.OnItemCheckChanged(Menu, Section.Items[i]);
            end;
          end;
        end;
      end;
    end;
    FChecked := Value;
    if Assigned(Menu.OnItemCheckChanged) then
      menu.OnItemCheckChanged(Menu, Self);
    Changed;
  end;
end;

procedure TGDIPMenuSectionItem.SetControl(const Value: TControl);
begin
  if Assigned(FControl) and Assigned(Value) then
  begin
    FControl.Visible := false;
    FControl.Parent := Value.Parent;
    Value.Visible := false;
  end;

  FControl := Value;
  if Assigned(FControl) and not (csDesigning in Application.ComponentState) then
    FControl.Visible := false;

  FOwner.Init(FOwner.FRect, true, false);

  Changed;
end;

procedure TGDIPMenuSectionItem.SetControlIndent(const Value: integer);
begin
  if FControlIndent <> value then
  begin
    FControlIndent := Value;
    Changed;
  end;
end;

procedure TGDIPMenuSectionItem.SetControlType(
  const Value: TGDIPMenuSectionControlType);
begin
  if FControlType <> value then
  begin
    FControlType := Value;
    if FControlType = ctEdit then
    begin
      if not Assigned(FEdit) then
      begin
        FEdit := TEdit.Create(FOwner.FWinC);
        FEdit.Parent := FOwner.FWinC;
        FEdit.OnChange := EditChange;
        Control := FEdit;
      end;
    end
    else if Assigned(FEdit) then
    begin
      Control := nil;
      FEdit.Free;
      FEdit := nil;
    end;

    Changed;
  end;
end;

procedure TGDIPMenuSectionItem.SetData(const Value: String);
begin
  if FData <> value then
  begin
    FData := Value;
    Changed;
  end;
end;

procedure TGDIPMenuSectionItem.SetEnabled(const Value: Boolean);
begin
  if FEnabled <> value then
  begin
    FEnabled := Value;
    Changed;
  end;
end;

procedure TGDIPMenuSectionItem.SetEnableFill(const Value: Boolean);
begin
  if FEnableFill <> value then
  begin
    FEnableFill := Value;
    Changed;
  end;
end;

procedure TGDIPMenuSectionItem.SetGraphicLeftName(const Value: String);
begin
  if FGraphicLeftName <> value then
  begin
    FGraphicLeftName := Value;
    Changed;
  end;
end;

procedure TGDIPMenuSectionItem.SetGraphicRightName(const Value: String);
begin
  if FGraphicRightName <> value then
  begin
    FGraphicRightName := Value;
    Changed;
  end;
end;

procedure TGDIPMenuSectionItem.SetGroupIndex(const Value: integer);
begin
  if FGroupIndex <> value then
  begin
    FGroupIndex := Value;
    Changed;
  end;
end;

procedure TGDIPMenuSectionItem.SetHeight(const Value: integer);
begin
  if FHeight <> value then
  begin
    FHeight := Value;
    Changed;
  end;
end;

procedure TGDIPMenuSectionItem.SetHideOnSelection(const Value: Boolean);
begin
  if FHideOnSelection <> value then
  begin
    FHideOnSelection := Value;
    Changed;
  end;
end;

procedure TGDIPMenuSectionItem.SetHoverText(const Value: String);
begin
  if FHoverText <> value then
  begin
    FHoverText := Value;
    Changed;
  end;
end;

procedure TGDIPMenuSectionItem.SetHoverTextLeft(const Value: integer);
begin
  if FHoverTextLeft <> value then
  begin
    FHoverTextLeft := Value;
    Changed;
  end;
end;

procedure TGDIPMenuSectionItem.SetHoverTextLocation(
  const Value: TGDIPMenuLocation);
begin
  if FHoverTextLocation <> value then
  begin
    FHoverTextLocation := Value;
    Changed;
  end;
end;

procedure TGDIPMenuSectionItem.SetHoverTextTop(const Value: integer);
begin
  if FHoverTextTop <> value then
  begin
    FHoverTextTop := Value;
    Changed;
  end;
end;

procedure TGDIPMenuSectionItem.SetHoverTopLayerItem(const Value: integer);
begin
  if FHoverTopLayerItem <> value then
  begin
    FHoverTopLayerItem := Value;
    Changed;
  end;
end;

procedure TGDIPMenuSectionItem.SetImageIndex(const Value: integer);
begin
  if FImageIndex <> value then
  begin
    FImageIndex := Value;
    Changed;
  end;
end;

procedure TGDIPMenuSectionItem.SetItemObject(const Value: TObject);
begin
  if FItemObject <> Value then
  begin
    FItemObject := Value;
    Changed;
  end;
end;

procedure TGDIPMenuSectionItem.SetItemType(
  const Value: TGDIPMenuSectionItemType);
begin
  if FItemType <> value then
  begin
    FItemType := Value;
    Changed;
  end;
end;

procedure TGDIPMenuSectionItem.SetSelectedTopLayerItem(const Value: integer);
begin
  if FSelectedTopLayerItem <> Value then
  begin
    FSelectedTopLayerItem := Value;
    Changed;
  end;
end;

procedure TGDIPMenuSectionItem.SetShortCut(const Value: TShortCut);
begin
  if FShortCut <> Value then
  begin
    FShortCut := Value;
    Changed;
  end;
end;

procedure TGDIPMenuSectionItem.SetTag(const Value: integer);
begin
  if FTag <> value then
  begin
    FTag := Value;
    Changed;
  end;
end;

procedure TGDIPMenuSectionItem.SetText(const Value: String);
begin
  if FText <> value then
  begin
    FText := Value;
    Changed;
  end;
end;

procedure TGDIPMenuSectionItem.SetTextLeft(const Value: integer);
begin
  if FTextLeft <> value then
  begin
    FTextLeft := Value;
    Changed;
  end;
end;

procedure TGDIPMenuSectionItem.SetTextLocation(const Value: TGDIPMenuLocation);
begin
  if FTextLocation <> value then
  begin
    FTextLocation := Value;
    Changed;
  end;
end;

procedure TGDIPMenuSectionItem.SetTextTop(const Value: integer);
begin
  if FTextTop <> value then
  begin
    FTextTop := Value;
    Changed;
  end;
end;

procedure TGDIPMenuSectionItem.SetVisible(const Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    Changed;
  end;
end;

{ TGDIPMenuSectionItemAppearance }

procedure TGDIPMenuSectionItemAppearance.Assign(Source: TPersistent);
begin
  if (Source is TGDIPMenuSectionItemAppearance) then
  begin
    FFontDisabled.Assign((Source as TGDIPMenuSectionItemAppearance).FontDisabled);
    FFontSelected.Assign((Source as TGDIPMenuSectionItemAppearance).FontSelected);
    FFillDisabled.Assign((Source as TGDIPMenuSectionItemAppearance).FillDisabled);
    FFillSelected.Assign((Source as TGDIPMenuSectionItemAppearance).FillSelected);
    FFontHover.Assign((Source as TGDIPMenuSectionItemAppearance).FontHover);
    FFillHover.Assign((Source as TGDIPMenuSectionItemAppearance).FillHover);
    FFont.Assign((Source as TGDIPMenuSectionItemAppearance).Font);
    FShortCutFont.Assign((Source as TGDIPMenuSectionItemAppearance).ShortCutFont);
    FFill.Assign((Source as TGDIPMenuSectionItemAppearance).Fill);
    FSeparatorSize := (source as TGDIPMenuSectionItemAppearance).SeparatorSize;
    FSeparatorFill.Assign((source as TGDIPMenuSectionItemAppearance).SeparatorFill);
    FBreakFill.Assign((source as TGDIPMenuSectionItemAppearance).BreakFill);
    FBreakSize := (Source as TGDIPMenuSectionItemAppearance).BreakSize;
    FShadowOffset := (Source as TGDIPMenuSectionItemAppearance).ShadowOffset;
    FShadowColor := (Source as TGDIPMenuSectionItemAppearance).ShadowColor;
    FURLColor := (Source as TGDIPMenuSectionItemAppearance).URLColor;
    FAllowSelection := (Source as TGDIPMenuSectionItemAppearance).AllowSelection;
  end;
end;

procedure TGDIPMenuSectionItemAppearance.Changed;
begin
  if Assigned(FOwner) then
    FOwner.Changed;
end;

constructor TGDIPMenuSectionItemAppearance.Create(AOwner: TGDIPMenu);
begin
  FOwner := AOwner;
  FFontDisabled := TFont.Create;
  FFontDisabled.OnChange := FontChanged;
  FFontSelected := TFont.Create;
  FFontSelected.OnChange := FontChanged;
  FFillDisabled := TGDIPFill.Create;
  FFillDisabled.OnChange := FillChanged;
  FFillSelected := TGDIPFill.Create;
  FFillSelected.OnChange := FillChanged;
  FFontHover := TFont.Create;
  FFontHover.OnChange := FontChanged;
  FFillHover := TGDIPFill.Create;
  FFillHover.OnChange := FillChanged;
  FFill := TGDIPFill.Create;
  FFill.OnChange := FillChanged;
  FFont := TFont.Create;
  FFont.OnChange := FontChanged;
  FSeparatorSize := 2;
  FBreakSize := 2;
  FSeparatorFill := TGDIPFill.Create;
  FBreakFill := TGDIPFill.Create;
  FShadowOffset := 5;
  FShadowColor := clGray;
  FURLColor := clBlue;
  FDetailFont := TFont.Create;
  FDetailFont.OnChange := FontChanged;
  FShortCutFont := TFont.Create;
  FShortcutFont.OnChange := FontChanged;
  FAllowSelection := true;

  {$IFNDEF DELPHI9_LVL}
  FFont.Name := 'Tahoma';
  FFontDisabled.Name := 'Tahoma';
  FFontSelected.Name := 'Tahoma';
  FFontHover.Name := 'Tahoma';
  FDetailFont.Name := 'Tahoma';
  FShortCutFont.Name := 'Tahoma';
  {$ENDIF}
end;

destructor TGDIPMenuSectionItemAppearance.Destroy;
begin
  FFill.Free;
  FFont.Free;
  FFontDisabled.Free;
  FFontSelected.Free;
  FFontHover.Free;
  FShortCutFont.Free;
  FFillDisabled.Free;
  FFillSelected.Free;
  FFillHover.Free;
  FBreakFill.Free;
  FSeparatorFill.Free;
  FDetailFont.Free;
  inherited;
end;

procedure TGDIPMenuSectionItemAppearance.FillChanged(Sender: TObject);
begin
  Changed;
end;

procedure TGDIPMenuSectionItemAppearance.FontChanged(Sender: TObject);
begin
  Changed;
end;

procedure TGDIPMenuSectionItemAppearance.LoadFromFile(ini: TIniFile;
  Section: String);
begin
  SeparatorFill.LoadFromFile(ini, Section + '.SeparatorFill');
  BreakFill.LoadFromFile(ini, Section + '.BreakFill');
  FillSelected.LoadFromFile(ini, Section + '.FillSelected');
  FillDisabled.LoadFromFile(ini, Section + '.FillDisabled');
  FillHover.LoadFromFile(ini, Section + '.FillHover');
  Fill.LoadFromFile(ini, Section + '.Fill');
  LoadFont(ini, Section + '.Font', Font);
  LoadFont(ini, Section + '.FontSelected', FontSelected);
  LoadFont(ini, Section + '.FontDisabled', FontDisabled);
  LoadFont(ini, Section + '.FontHover', FontHover);
end;

procedure TGDIPMenuSectionItemAppearance.SaveToFile(ini: TIniFile;
  Section: String);
begin
  SeparatorFill.SaveToFile(ini, Section + '.SeparatorFill');
  BreakFill.SaveToFile(ini, Section + '.BreakFill');
  FillSelected.SaveToFile(ini, Section + '.FillSelected');
  FillDisabled.SaveToFile(ini, Section + '.FillDisabled');
  FillHover.SaveToFile(ini, Section + '.FillHover');
  Fill.SaveToFile(ini, Section + '.Fill');
  SaveFont(ini, Section + '.Font', Font);
  SaveFont(ini, Section + '.FontSelected', FontSelected);
  SaveFont(ini, Section + '.FontDisabled', FontDisabled);
  SaveFont(ini, Section + '.FontHover', FontHover);
end;

procedure TGDIPMenuSectionItemAppearance.SetAllowSelection(
  const Value: Boolean);
begin
  if FAllowSelection <> value then
  begin
    FAllowSelection := Value;
    Changed;
  end;
end;

procedure TGDIPMenuSectionItemAppearance.SetBreakFill(const Value: TGDIPFill);
begin
  if FBreakFill <> value then
  begin
    FBreakFill.Assign(Value);
    Changed;
  end;
end;

procedure TGDIPMenuSectionItemAppearance.SetBreakWidth(const Value: integer);
begin
  if FBreakSize <> value then
  begin
    FBreakSize := Value;
    Changed;
  end;
end;

procedure TGDIPMenuSectionItemAppearance.SetDetailFont(const Value: TFont);
begin
  if FDetailFont <> value then
  begin
    FDetailFont.Assign(value);
    Changed;
  end;
end;

procedure TGDIPMenuSectionItemAppearance.SetFill(const Value: TGDIPFill);
begin
  if FFill <> Value then
  begin
    FFill.Assign(Value);
    Changed;
  end;
end;

procedure TGDIPMenuSectionItemAppearance.SetFillDisabled(
  const Value: TGDIPFill);
begin
  if FFillDisabled <> value then
  begin
    FFillDisabled.Assign(Value);
    Changed;
  end;
end;

procedure TGDIPMenuSectionItemAppearance.SetFillHover(const Value: TGDIPFill);
begin
  if FFillHover <> value then
  begin
    FFillHover.Assign(Value);
    Changed;
  end;
end;

procedure TGDIPMenuSectionItemAppearance.SetFillSelected(
  const Value: TGDIPFill);
begin
  if FFillSelected <> Value then
  begin
    FFillSelected.Assign(Value);
    Changed;
  end;
end;

procedure TGDIPMenuSectionItemAppearance.SetFont(const Value: TFont);
begin
  if FFont <> value then
  begin
    FFont.Assign(Value);
    Changed;
  end;
end;

procedure TGDIPMenuSectionItemAppearance.SetFontDisabled(const Value: TFont);
begin
  if FFontDisabled <> value then
  begin
    FFontDisabled.Assign(Value);
    Changed;
  end;
end;

procedure TGDIPMenuSectionItemAppearance.SetFontHover(const Value: TFont);
begin
  if FFontHover <> value then
  begin
    FFontHover.Assign(Value);
    Changed;
  end;
end;

procedure TGDIPMenuSectionItemAppearance.SetFontSelected(const Value: TFont);
begin
  if FFontSelected <> value then
  begin
    FFontSelected.Assign(Value);
    Changed;
  end;
end;

procedure TGDIPMenuSectionItemAppearance.SetSeparatorFill(
  const Value: TGDIPFill);
begin
  if FSeparatorFill <> value then
  begin
    FSeparatorFill.Assign(Value);
    Changed;
  end;
end;

procedure TGDIPMenuSectionItemAppearance.SetSeparatorWidth(
  const Value: integer);
begin
  if FSeparatorSize <> value then
  begin
    FSeparatorSize := Value;
    Changed;
  end;
end;

procedure TGDIPMenuSectionItemAppearance.SetShadowColor(const Value: TColor);
begin
  if FShadowColor <> value then
  begin
    FShadowColor := Value;
    Changed;
  end;
end;

procedure TGDIPMenuSectionItemAppearance.SetShadowOffset(const Value: integer);
begin
  if FShadowOffset <> value then
  begin
    FShadowOffset := Value;
    Changed;
  end;
end;

procedure TGDIPMenuSectionItemAppearance.SetShortCutFont(const Value: TFont);
begin
  if FShortCutFont <> value then
  begin
    FShortCutFont := Value;
    Changed;
  end;
end;

procedure TGDIPMenuSectionItemAppearance.SetURLColor(const Value: TColor);
begin
  if FURLColor <> value then
  begin
    FURLColor := Value;
    Changed;
  end;
end;

end.
