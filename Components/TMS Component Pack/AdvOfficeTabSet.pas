{***************************************************************************}
{ TAdvOfficeTabSet component                                                }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2007 - 2015                                        }
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

unit AdvOfficeTabSet;

{$I TMSDEFS.INC}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Math, Menus,
  Dialogs, Forms, ImgList, CommCtrl, ExtCtrls, ComCtrls, AdvGDIP, GDIPicture,
  AdvHintInfo, AdvGlowButton, ACXPVS, AdvDWM, AdvStyleIf, AxCtrls
  , Types
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;

{$R ADVOFFICETABSET.RES}

{$DEFINE TMS_DWM}

const
  GLOWSPEED = 50;
  IMG_SPACE = 2;
  DEFAULT_TABHEIGHT = 26;
  ADVPAGE_OFFSET = 1;
  SCROLLER_SIZE = 32;
  TabBUTTON_SIZE = 18;

  MAJ_VER = 3; // Major version nr.
  MIN_VER = 4; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 0; // Build nr.

  MAX_ROUNDING = 8;

  // version history
  // 1.0.0.1 : Fixed issue with hidden tabs & scrolling
  // 1.0.0.2 : Fixed issue with deleting tabs
  // 1.1.0.0 : New : property TabRearrange added
  //         : New : OnTabMoved event added
  //         : New : TAdvOfficeMDITabSet
  // 1.1.1.0 : New : Office 2007 silver style added
  //         : Fixed : issue with close button position
  // 1.1.2.0 : Improved : MDI child OnClose, OnCloseQuery event handled in AdvOfficeMDITabSet
  // 1.1.2.1 : Fixed : issue with right click on tab
  // 1.1.2.2 : Improved : paint issue with TabSettings.StartMargin = 0
  // 1.2.0.0 : New : ImagePosition can be set in TabSettings as left, top, bottom, right from tab caption
  //         : New : Wordwrapped tab caption support
  //         : New : autoscrolling pages while mouse is down on scroll buttons
  //         : New : ScrollButtonsAlways property to show scroll buttons always irrespective of nr. of tabs
  //         : New : OnDrawTab event for custom tab drawing
  // 1.2.0.1 : Fixed : issue with OnDestroy for TAdvOfficeMDITabSet
  // 1.2.0.2 : Fixed : issue with order of MDI child deletes
  // 1.2.1.0 : Improved : shows images of tabs in MDI child window menu for TAdvOfficeMDITabSet
  // 1.2.2.0 : Improved : AdvOfficeMDITabSet.GetTab(Child) : TOfficeTabCollectionItem added
  // 1.2.3.0 : Improved : exposed drag & drop events
  // 1.2.4.0 : New : Moved PtOnTab method to public section
  // 1.3.0.0 : New : Optional TabAppearance control per tab
  //         : New : Optional control on close button per tab
  //         : New : left, right or left/right rounded tabs
  // 1.3.1.0 : New : public method added to be used after runtime creation
  // 1.3.1.1 : Fixed : issue with using font not installed on the system
  // 1.3.1.2 : Fixed : issue with drag & drop on tabs from other controls
  // 1.3.1.3 : Fixed : handling caNone CloseAction for MDI child form closing
  // 1.3.1.4 : Improved : behaviour when closing MDI childs from the tabset
  // 1.3.1.5 : Improved : tab border color handling
  // 1.3.2.0 : Improved : gradient, highlight & shadow painting improvements
  // 1.3.2.1 : Fixed : issue with updating when calling OfficeTabs.Clear;
  // 1.3.2.2 : Improved : GDI+ drawing
  // 1.3.3.0 : New : property Glow added
  // 1.3.3.1 : Fixed : issue with font styles on right positioned tabs
  // 1.3.3.2 : Fixed : issue with possible double auto creation of tabs
  // 1.3.3.3 : Improved : autosizing of tabs  
  // 1.3.4.0 : New : Alignment property added for fixed width tab text alignment
  // 1.3.4.1 : Fixed : issue with child form OnDestroy event
  // 1.3.4.2 : Fixed : issue with text color when Tab width > 0
  // 1.3.4.3 : Improved : tab hint compatibility with regular VCL hints
  // 1.4.0.0 : New : capability to have a checkbox per tab
  //         : New : capability to have a close button on each tab instead of active tab only
  //         : New : capability to have First/Last button in scroll area
  // 1.4.0.1 : Fixed : issue with ShowClose = false and ShowCloseOnNonSelectedTabs
  // 1.4.1.0 : New : Events OnFirstClick, OnLastClick, OnPrevClick, OnNextClick added
  // 1.4.1.1 : Fixed : Issue with Free of tab
  // 1.4.1.2 : Fixed : Issue with setting Tab.Visible = false
  // 1.5.0.0 : New : Windows Vista, Windows 7 and Terminal styles
  // 1.5.1.0 : New : property Transparent added
  // 1.5.1.1 : Fixed : Issue with destroying close buttons for older compilers
  // 1.5.2.0 : New : method CloseAllTabs added
  // 1.5.2.1 : Fixed : issue with updating ActiveTabIndex when closing tabs
  // 1.5.2.2 : Fixed : issue with scrolling & hidden tabs

  // 2.0.0.0 : New : ClosedList, CloseList button capability added
  //           New : TabSettings.EndMargin added
  //           New : FixedTabs property added to have always fixed / non scrolling tabs
  //           New : Progress indication on tab added
  //           New : OnTabMouseLeave / OnTabMouseEnter events added
  //           New : Indicator during tab reorder added to indicate new position
  //           New : Glow / GlowColor property added per page as indicator for a page
  // 2.0.0.1 : Fixed : Issue with closing last tab when FreeOnClose = true

  // 2.5.0.0 : New : Chrome style close button
  //         : New : Support for undocking tabs
  //         : New : Support to be used on glass frame
  //         : New : Optional Insert tab button
  //         : New : Automatic theme adaption with Office 2007 / 2010
  //         : New : ButtonSize to control size of buttons and capability to make it bigger for touchscreens
  //         : New : Capability to persist tab order & visibility to file
  //         : Improved : Support to be used on glass frame
  // 2.5.1.0 : New : Added RemoveTab() public function in TAdvOfficeMDITabSet
  // 3.0.0.0 : New : Metro style
  //         : New : Settings property
  //         : New : SetFloating method added
  //         : New : TabByName, TabByCaption, TabByTabIndex functions added
  //         : New : ResetTabs method added
  //         : Improved : SaveToFile/LoadFromFile also loads page floating state. Breaking change for file format!
  // 3.0.1.0 : New : Event OnTabInsert added
  // 3.1.0.0 : New : Multiline tab capability added
  //         : New : AutoHeight property added / AdjustHeight method added to auto adjust height of multiline tabs
  // 3.2.0.0 : New : Windows 8, Office 2013 styles added
  // 3.2.0.1 : Fixed : Issue with tab font on glass for Whidbey style
  // 3.2.0.2 : Fixed : Issue with changing AdvOfficeTabs.TabAppearance.Font when UseTabAppearance = true
  // 3.2.0.3 : Fixed : Issue with close button position not updating when tab resizes
  // 3.2.0.4 : Fixed : Issue with destroying
  // 3.2.0.5 : Fixed : Issue with scrolling tabs in combination with hidden tabs
  // 3.2.0.6 : Fixed : Issue with order of tabs when switching active tab in multiline setting
  // 3.2.0.7 : Fixed : Issue with auto tab height calculation for multirow tabs
  // 3.2.0.8 : Fixed : Issue with destroying tabs with close button
  // 3.2.1.0 : New : Public property TabObject added to TAdvOfficeTabItem
  // 3.3.0.0 : New : Windows 10, Office 2016 styles added
  // 3.3.0.1 : Fixed : Issue with AutoThemeAdapt and Office 2013 / 2016 styles
  // 3.3.0.2 : Fixed : Issue with OnTabChanging and specific sequences of deleting tabs
  // 3.3.1.0 : New : Exposed OnTabInsert in TAdvOfficeMDITabSet
  // 3.4.0.0 : New : Support for HighDPI mode

var
  WM_OTSDESTROYCLOSEBTN: Word;

type
  {$IFDEF DELPHI_UNICODE}
  THintInfo = Controls.THintInfo;
  PHintInfo = Controls.PHintInfo;
  {$ENDIF}

  TAdvCustomOfficeTabSet = class;
  TAdvOfficeTabSet = class;
  TOfficeTabCollection = class;

  TGradientDirection = (gdHorizontal, gdVertical);
  TGlowState = (gsHover, gsPush, gsNone);
  TButtonLayout = (blGlyphLeft, blGlyphTop, blGlyphRight, blGlyphBottom);
  TDropDownPosition = (dpRight, dpBottom);
  //TGDIPGradient = (ggRadial, ggVertical, ggDiagonalForward, ggDiagonalBackward);
  //TAntiAlias = (aaNone, aaClearType, aaAntiAlias);
  TImagePosition = (ipLeft, ipTop, ipRight, ipBottom);
  TCloseOnTabPos = (cpRight, cpLeft);
  TAdvTabShape =  (tsRectangle, tsLeftRamp, tsRightRamp, tsLeftRightRamp);
  TCloseButtonLook = (cblOffice, cblChrome);

  TTabRounding = 0..MAX_ROUNDING;

  TTabMovedEvent = procedure (Sender : TObject; FromIndex, ToIndex: integer) of object;

  TTabSetTabSettings = class(TPersistent)
  private
    FLeftMargin: Integer;
    FRightMargin: Integer;
    FOnChange: TNotifyEvent;
    FHeight: Integer;
    FStartMargin: Integer;
    FEndMargin: Integer;
    FSpacing: Integer;
    FImagePosition: TImagePosition;
    FWordWrap: Boolean;
    FWidth: Integer;
    FShape: TAdvTabShape;
    FRounding: TTabRounding;
    FAlignment: TAlignment;
    FGlass: Boolean;
    FTabSet: TAdvCustomOfficeTabSet;
    procedure SetLeftMargin(const Value: Integer);
    procedure SetRightMargin(const Value: Integer);
    procedure SetHeight(const Value: Integer);
    procedure SetStartMargin(const Value: Integer);
    procedure SetEndMargin(const Value: Integer);
    procedure SetSpacing(const Value: Integer);
    procedure SetImagePosition(const Value: TImagePosition);
    procedure SetWidth(const Value: Integer);
    procedure SetWordWrap(const Value: Boolean);
    procedure SetRounding(const Value: TTabRounding);
    procedure SetShape(const Value: TAdvTabShape);
    procedure SetAlignment(const Value: TAlignment);
    procedure SetGlass(const Value: Boolean);
  protected
    procedure Changed;
    property TabSet: TAdvCustomOfficeTabSet read FTabSet write FTabSet;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property LeftMargin: Integer read FLeftMargin write SetLeftMargin default 4;
    property RightMargin: Integer read FRightMargin write SetRightMargin default 4;
    property StartMargin: Integer read FStartMargin write SetStartMargin default 4;
    property EndMargin: Integer read FEndMargin write SetEndMargin default 0;
    property Height: Integer read FHeight write SetHeight default 26;
    property Spacing: Integer read FSpacing write SetSpacing default 4;
    property Width: Integer read FWidth write SetWidth default 0;
    property WordWrap: Boolean read FWordWrap write SetWordWrap default False;
    property ImagePosition: TImagePosition read FImagePosition write SetImagePosition default ipLeft;
    property Shape: TAdvTabShape read FShape write SetShape default tsRectangle;
    property Rounding: TTabRounding read FRounding write SetRounding default 1;
    property Glass: Boolean read FGlass Write SetGlass default False;
  end;

  TGradientBackground = class(TPersistent)
  private
    FSteps: Integer;
    FColor: TColor;
    FColorTo: TColor;
    FDirection: TGradientDirection;
    FOnChange: TNotifyEvent;
    procedure SetColor(const Value: TColor);
    procedure SetColorTo(const Value: TColor);
    procedure SetDirection(const Value: TGradientDirection);
    procedure SetSteps(const Value: Integer);
    procedure Changed;
  protected
  public
    constructor Create; 
    procedure Assign(Source: TPersistent); override;  
  published
    property Color: TColor read FColor write SetColor;
    property ColorTo: TColor read FColorTo write SetColorTo;
    property Direction: TGradientDirection read FDirection write SetDirection;
    property Steps: Integer read FSteps write SetSteps default 64;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
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

  TCustomTabAppearance = class(TPersistent)
  private
    FOnChange: TNotifyEvent;
    FBorderColor: TColor;
    FBorderColorHot: TColor;
    FColor: TColor;
    FColorTo: TColor;
    FColorHot: TColor;
    FColorHotTo: TColor;
    FColorSelectedTo: TColor;
    FBorderColorDisabled: TColor;
    FBorderColorSelected: TColor;
    FColorDisabled: TColor;
    FColorDisabledTo: TColor;
    FColorSelected: TColor;
    FColorMirror: TColor;
    FColorMirrorTo: TColor;
    FColorMirrorHot: TColor;
    FColorMirrorHotTo: TColor;
    FGradientMirror: TGDIPGradient;
    FGradientMirrorHot: TGDIPGradient;
    FGradient: TGDIPGradient;
    FGradientHot: TGDIPGradient;
    FColorMirrorDisabledTo: TColor;
    FColorMirrorDisabled: TColor;
    FColorMirrorSelectedTo: TColor;
    FColorMirrorSelected: TColor;
    FGradientSelected: TGDIPGradient;
    FGradientDisabled: TGDIPGradient;
    FGradientMirrorSelected: TGDIPGradient;
    FGradientMirrorDisabled: TGDIPGradient;
    FTextColorDisabled: TColor;
    FTextColorSelected: TColor;
    FTextColor: TColor;
    FTextColorHot: TColor;
    FBackGround: TGradientBackground;
    FBorderColorSelectedHot: TColor;
    FBorderColorDown: TColor;
    FFont: TFont;
    FHighLightColorHot: TColor;
    FShadowColor: TColor;
    FHighLightColorDown: TColor;
    FHighLightColorSelectedHot: TColor;
    FHighLightColorSelected: TColor;
    procedure OnBackGroundChanged(Sender: TObject);
    procedure SetBackGround(const Value: TGradientBackground);
    procedure SetBorderColor(const Value: TColor);
    procedure SetBorderColorDisabled(const Value: TColor);
    procedure SetBorderColorSelected(const Value: TColor);
    procedure SetBorderColorSelectedHot(const Value: TColor);
    procedure SetColor(const Value: TColor);
    procedure SetColorDisabled(const Value: TColor);
    procedure SetColorDisabledTo(const Value: TColor);
    procedure SetColorHot(const Value: TColor);
    procedure SetColorHotTo(const Value: TColor);
    procedure SetColorMirror(const Value: TColor);
    procedure SetColorMirrorDisabled(const Value: TColor);
    procedure SetColorMirrorDisabledTo(const Value: TColor);
    procedure SetColorMirrorHot(const Value: TColor);
    procedure SetColorMirrorHotTo(const Value: TColor);
    procedure SetColorMirrorSelected(const Value: TColor);
    procedure SetColorMirrorSelectedTo(const Value: TColor);
    procedure SetColorMirrorTo(const Value: TColor);
    procedure SetColorSelected(const Value: TColor);
    procedure SetColorSelectedTo(const Value: TColor);
    procedure SetColorTo(const Value: TColor);
    procedure SetGradient(const Value: TGDIPGradient);
    procedure SetGradientDisabled(const Value: TGDIPGradient);
    procedure SetGradientHot(const Value: TGDIPGradient);
    procedure SetGradientMirror(const Value: TGDIPGradient);
    procedure SetGradientMirrorDisabled(const Value: TGDIPGradient);
    procedure SetGradientMirrorHot(const Value: TGDIPGradient);
    procedure SetGradientMirrorSelected(const Value: TGDIPGradient);
    procedure SetGradientSelected(const Value: TGDIPGradient);
    procedure SetTextColor(const Value: TColor);
    procedure SetTextColorDisabled(const Value: TColor);
    procedure SetTextColorHot(const Value: TColor);
    procedure SetTextColorSelected(const Value: TColor);
    procedure SetBorderColorDown(const Value: TColor);
    procedure SetFont(const Value: TFont);
  protected
    procedure Changed;
    procedure ClearValues;
    property BackGround: TGradientBackground read FBackGround write SetBackGround;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property BorderColor: TColor read FBorderColor write SetBorderColor;
    property BorderColorHot: TColor read FBorderColorHot write FBorderColorHot;
    property BorderColorSelected: TColor read FBorderColorSelected write SetBorderColorSelected;
    property BorderColorSelectedHot: TColor read FBorderColorSelectedHot write SetBorderColorSelectedHot;
    property BorderColorDisabled: TColor read FBorderColorDisabled write SetBorderColorDisabled;
    property BorderColorDown: TColor read FBorderColorDown write SetBorderColorDown;
    property Color: TColor read FColor write SetColor;
    property ColorTo: TColor read FColorTo write SetColorTo;
    property ColorSelected: TColor read FColorSelected write SetColorSelected;
    property ColorSelectedTo: TColor read FColorSelectedTo write SetColorSelectedTo;
    property ColorDisabled: TColor read FColorDisabled write SetColorDisabled;
    property ColorDisabledTo: TColor read FColorDisabledTo write SetColorDisabledTo;
    property ColorHot: TColor read FColorHot write SetColorHot;
    property ColorHotTo: TColor read FColorHotTo write SetColorHotTo;
    property ColorMirror: TColor read FColorMirror write SetColorMirror;
    property ColorMirrorTo: TColor read FColorMirrorTo write SetColorMirrorTo;
    property ColorMirrorHot: TColor read FColorMirrorHot write SetColorMirrorHot;
    property ColorMirrorHotTo: TColor read FColorMirrorHotTo write SetColorMirrorHotTo;
    property ColorMirrorSelected: TColor read FColorMirrorSelected write SetColorMirrorSelected;
    property ColorMirrorSelectedTo: TColor read FColorMirrorSelectedTo write SetColorMirrorSelectedTo;
    property ColorMirrorDisabled: TColor read FColorMirrorDisabled write SetColorMirrorDisabled;
    property ColorMirrorDisabledTo: TColor read FColorMirrorDisabledTo write SetColorMirrorDisabledTo;
    property Font: TFont read FFont write SetFont;
    property Gradient: TGDIPGradient read FGradient write SetGradient;
    property GradientMirror: TGDIPGradient read FGradientMirror write SetGradientMirror;
    property GradientHot: TGDIPGradient read FGradientHot write SetGradientHot;
    property GradientMirrorHot: TGDIPGradient read FGradientMirrorHot write SetGradientMirrorHot;
    property GradientSelected: TGDIPGradient read FGradientSelected write SetGradientSelected;
    property GradientMirrorSelected: TGDIPGradient read FGradientMirrorSelected write SetGradientMirrorSelected;
    property GradientDisabled: TGDIPGradient read FGradientDisabled write SetGradientDisabled;
    property GradientMirrorDisabled: TGDIPGradient read FGradientMirrorDisabled write SetGradientMirrorDisabled;
    property TextColor: TColor read FTextColor write SetTextColor;
    property TextColorHot: TColor read FTextColorHot write SetTextColorHot;
    property TextColorSelected: TColor read FTextColorSelected write SetTextColorSelected;
    property TextColorDisabled: TColor read FTextColorDisabled write SetTextColorDisabled;
    property ShadowColor: TColor read FShadowColor write FShadowColor;
    property HighLightColorSelected: TColor read FHighLightColorSelected write FHighLightColorSelected;
    property HighLightColorHot: TColor read FHighLightColorHot write FHighLightColorHot;
    property HighLightColorSelectedHot: TColor read FHighLightColorSelectedHot write FHighLightColorSelectedHot;
    property HighLightColorDown: TColor read FHighLightColorDown write FHighLightColorDown;
  end;

  TTabAppearance = class(TCustomTabAppearance)
  private
  protected
  public
  published
    property BackGround;
  end;

  TTabSetTabScroller = class(TObject)
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

  TTabSetButtonSettings = class(TPersistent)
  private
    FOnChange: TNotifyEvent;
    FTabListButton: boolean;
    FCloseButton: Boolean;
    FScrollButtonNextPicture: TGDIPPicture;
    FScrollButtonPrevPicture: TGDIPPicture;
    FTabListButtonPicture: TGDIPPicture;
    FCloseButtonPicture: TGDIPPicture;
    FScrollButtonPrevHint: String;
    FScrollButtonNextHint: String;
    FCloseButtonHint: String;
    FTabListButtonHint: String;
    FScrollButtonsAlways: Boolean;
    FScrollButtonFirstPicture: TGDIPPicture;
    FScrollButtonLastPicture: TGDIPPicture;
    FScrollButtonFirstHint: String;
    FScrollButtonLastHint: String;
    FLastButton: Boolean;
    FFirstButton: Boolean;
    FClosedListButtonHint: String;
    FClosedListButtonPicture: TGDIPPicture;
    FClosedListButton: Boolean;
    FCloseButtonLook: TCloseButtonLook;
    FButtonSize: Integer;
    FShowInsertButton: Boolean;
    FInsertButtonHint: String;
    procedure Changed;
    procedure OnPictureChanged(Sender: TObject);
    procedure SetCloseButton(const Value: Boolean);
    procedure SetCloseButtonPicture(const Value: TGDIPPicture);
    procedure SetTabListButton(const Value: boolean);
    procedure SetTabListButtonPicture(const Value: TGDIPPicture);
    procedure SetScrollButtonNextPicture(const Value: TGDIPPicture);
    procedure SetScrollButtonPrevPicture(const Value: TGDIPPicture);
    procedure SetScrollButtonsAlways(const Value: Boolean);
    procedure SetScrollButtonFirstPicture(const Value: TGDIPPicture);
    procedure SetScrollButtonLastPicture(const Value: TGDIPPicture);
    procedure SetFirstButton(const Value: Boolean);
    procedure SetLastButton(const Value: Boolean);
    procedure SetClosedListButtonPicture(const Value: TGDIPPicture);
    procedure SetClosedListButton(const Value: Boolean);
    procedure SetButtonSize(const Value: Integer);
    procedure SetCloseButtonLook(const Value: TCloseButtonLook);
    procedure SetShowInsertButton(const Value: Boolean);
  protected
    FScrollButtonDownPicture, FScrollButtonDownLastPicture: TGDIPPicture;
    FScrollButtonUpPicture, FScrollButtonUpFirstPicture: TGDIPPicture;
    FPrevPictureChanged: Boolean;
    FNextPictureChanged: Boolean;
    FFirstPictureChanged: Boolean;
    FLastPictureChanged: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property CloseButton: Boolean read FCloseButton write SetCloseButton default false;
    property TabListButton: boolean read FTabListButton write SetTabListButton default false;
    property ClosedListButton: Boolean read FClosedListButton write SetClosedListButton default false;
    property CloseButtonPicture: TGDIPPicture read FCloseButtonPicture write SetCloseButtonPicture;
    property ClosedListButtonPicture: TGDIPPicture read FClosedListButtonPicture write SetClosedListButtonPicture;
    property TabListButtonPicture: TGDIPPicture read FTabListButtonPicture write SetTabListButtonPicture;
    property ScrollButtonPrevPicture: TGDIPPicture read FScrollButtonPrevPicture write SetScrollButtonPrevPicture;
    property ScrollButtonNextPicture: TGDIPPicture read FScrollButtonNextPicture write SetScrollButtonNextPicture;
    property ScrollButtonFirstPicture: TGDIPPicture read FScrollButtonFirstPicture write SetScrollButtonFirstPicture;
    property ScrollButtonLastPicture: TGDIPPicture read FScrollButtonLastPicture write SetScrollButtonLastPicture;
    property CloseButtonHint: String read FCloseButtonHint write FCloseButtonHint;
    property InsertButtonHint: String read FInsertButtonHint write FInsertButtonHint;
    property TabListButtonHint: String read FTabListButtonHint write FTabListButtonHint;
    property ClosedListButtonHint: String read FClosedListButtonHint write FClosedListButtonHint;
    property ScrollButtonsAlways: Boolean read FScrollButtonsAlways write SetScrollButtonsAlways default False;
    property ScrollButtonNextHint: String read FScrollButtonNextHint write FScrollButtonNextHint;
    property ScrollButtonPrevHint: String read FScrollButtonPrevHint write FScrollButtonPrevHint;
    property ScrollButtonFirstHint: String read FScrollButtonFirstHint write FScrollButtonFirstHint;
    property ScrollButtonLastHint: String read FScrollButtonLastHint write FScrollButtonLastHint;
    property FirstButton: Boolean read FFirstButton write SetFirstButton default false;
    property LastButton: Boolean read FLastButton write SetLastButton default false;
    property CloseButtonLook: TCloseButtonLook read FCloseButtonLook write SetCloseButtonLook default cblOffice;
    property ButtonSize: Integer read FButtonSize write SetButtonSize default TabBUTTON_SIZE;
    property ShowInsertButton: Boolean read FShowInsertButton write SetShowInsertButton default false;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TCustomAdvOfficeTabSetStyler = class(TComponent)
  private
    FControlList: TDbgList;
    FTabAppearance: TTabAppearance;
    FRoundEdges: Boolean;
    FAutoThemeAdapt: boolean;
    FGlowButtonAppearance: TGlowButtonAppearance;
    FBlendFactor: Integer;
    FButtonBorderColor: TColor;
    FTabRounding: TTabRounding;
    procedure OnTabAppearanceChanged(Sender: TObject);
    procedure OnGlowButtonAppearanceChanged(Sender: TObject);
    procedure SetRoundEdges(const Value: boolean);
    procedure SetTabRounding(const Value: TTabRounding);
    procedure SetTabAppearance(const Value: TTabAppearance);
    procedure SetGlowButtonAppearance(const Value: TGlowButtonAppearance);
    procedure SetButtonBorderColor(const Value: TColor);
  protected
    procedure AddControl(AControl: TCustomControl);
    procedure RemoveControl(AControl: TCustomControl);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Change(PropID: integer);
    procedure SetColorTones(ATones: TColorTones);
    procedure InitColorTones; virtual;
    property BlendFactor: Integer read FBlendFactor write FBlendFactor;
    procedure SetAutoThemeAdapt(const Value: boolean); virtual;

    property AutoThemeAdapt: boolean read FAutoThemeAdapt write SetAutoThemeAdapt default False;
    property TabAppearance: TTabAppearance read FTabAppearance write SetTabAppearance;  // 1
    property RoundEdges: boolean read FRoundEdges write SetRoundEdges default True;   // 3
    property TabRounding: TTabRounding read FTabRounding write SetTabRounding default 1;
    property ButtonBorderColor: TColor read FButtonBorderColor write SetButtonBorderColor;
    property GlowButtonAppearance: TGlowButtonAppearance read FGlowButtonAppearance write SetGlowButtonAppearance; // 4
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure Assign(Source: TPersistent); override;
  end;

  TDummyHintControl = class(TControl)
  private
    FOfficeHint: TAdvHintInfo;
    procedure SetOfficeHint(const Value: TAdvHintInfo);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property OfficeHint: TAdvHintInfo read FOfficeHint write SetOfficeHint;
  end;

  TTabProgress = class(TPersistent)
  private
    FColor: TColor;
    FCompleteColor: TColor;
    FOnChange: TNotifyEvent;
    FMax: Integer;
    FMin: Integer;
    FVisible: Boolean;
    FPosition: Integer;
    FColorTo: TColor;
    FCompleteColorTo: TColor;
    procedure SetColor(const Value: TColor);
    procedure SetCompleteColor(const Value: TColor);
    procedure SetMax(const Value: Integer);
    procedure SetMin(const Value: Integer);
    procedure SetPosition(const Value: Integer);
    procedure SetVisible(const Value: Boolean);
    procedure SetColorTo(const Value: TColor);
    procedure SetCompleteColorTo(const Value: TColor);
  protected
    procedure Changed;
    procedure Draw(g: TGPGraphics; R: TRect; RotateTabLeftRight: Boolean; Direction: TTabPosition);
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property Color: TColor read FColor write SetColor default clWhite;
    property ColorTo: TColor read FColorTo write SetColorTo default clSilver;
    property CompleteColor: TColor read FCompleteColor write SetCompleteColor default clWhite;
    property CompleteColorTo: TColor read FCompleteColorTo write SetCompleteColorTo default clRed;
    property Min: Integer read FMin write SetMin default 0;
    property Max: Integer read FMax write SetMax default 100;
    property Position: Integer read FPosition write SetPosition default 50;
    property Visible: Boolean read FVisible write SetVisible default false;
  end;

  TArrowDirection = (arrUp, arrDown, arrLeft, arrRight);
  TArrowWindow = class(TPanel)
  private
    Dir: TArrowDirection;
    Arrow: array[0..8] of TPoint;
    FBrColor: TColor;
  public
    constructor Init(AOwner: TComponent;direction:TArrowDirection);
    procedure Loaded; override;
  protected
    procedure CreateWnd; override;
    procedure Paint; override;
    procedure CreateParams(var Params: TCreateParams); override;
  end;

  TAdvTabSetButton = class(TGraphicControl)
  private
    FMouseEnter: Boolean;
    FOnMouseLeave: TNotifyEvent;
    FOnMouseEnter: TNotifyEvent;
    FTransparent: Boolean;
    FMouseDown: Boolean;
    FDrawClose: Boolean;
    procedure WMEraseBkGnd(var Message: TWMEraseBkGnd); message WM_ERASEBKGND;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure SetTransparent(const Value: Boolean);
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;

    // published
    property Enabled;
    property Font;
    property Transparent: Boolean read FTransparent write SetTransparent default True;
    property DrawClose: Boolean read FDrawClose write FDrawClose default True;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Click; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  end;


  TOfficeTabCollectionItem = class(TCollectionItem)
  private
    FCloseButton: TAdvGlowButton;
    FCloseButtonChrome: TAdvTabSetButton;
    FCaption: string;
    FVisible: Boolean;
    FTag: integer;
    FImageIndex: Integer;
    FEnabled: boolean;
    FTimer: TTimer;
    FTimeInc: Integer;
    FStepHover: Integer;
    FStepPush: Integer;
    FGlowState: TGlowState;
    FIPicture: TGDIPPicture;
    FIDisabledPicture: TGDIPPicture;
    FOfficeHint: TAdvHintInfo;
    FShowClose: Boolean;
    FUseTabAppearance: Boolean;
    FTabAppearance: TTabAppearance;
    FShowCheckBox: Boolean;
    FChecked: Boolean;
    FProgress: TTabProgress;
    FGlow: Boolean;
    FGlowColor: TColor;
    FTabIndex: integer;
    //--- Docking related
    FLocked: Boolean;
    FParentCollection: TOfficeTabCollection;
    FFloating: Boolean;   // True only when floating
    FDraggingAccept: Boolean;
    FName: string;
    FDestroying: boolean;
    FObject: TObject;
    //---
    procedure TimerProc(Sender: TObject);
    procedure PictureChanged(Sender: TObject);
    procedure OnTabAppearanceChanged(Sender: TObject);
    procedure OnProgressChanged(Sender: TObject);
    procedure SetCaption(const Value: string);
    procedure SetVisible(const Value: Boolean);
    procedure SetImageIndex(const Value: Integer);
    procedure SetEnabled(const Value: boolean);
    procedure SetDisabledPicture(const Value: TGDIPPicture);
    procedure SetPicture(const Value: TGDIPPicture);
    procedure SetOfficeHint(const Value: TAdvHintInfo);
    procedure SetShowClose(const Value: Boolean);
    procedure SetTabAppearance(const Value: TTabAppearance);
    procedure SetUseTabAppearance(const Value: Boolean);
    procedure SetChecked(const Value: Boolean);
    procedure SetShowCheckBox(const Value: Boolean);
    procedure SetProgress(const Value: TTabProgress);
    procedure SetGlow(const Value: Boolean);
    procedure SetGlowColor(const Value: TColor);
    procedure SetLocked(const Value: Boolean);
  protected
    FChildForm: TForm;
    FOnActivateForm: TNotifyEvent;
    FOnDestroyForm: TNotifyEvent;
    FTabRect: TRect;
    function GetDisplayName: string; override;
    procedure SetIndex(Value: Integer); override;
    procedure Refresh;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property TabIndex: integer read FTabIndex write FTabIndex;
    property TabObject: TObject read FObject write FObject;
  published
    property Caption: string read FCaption write SetCaption;
    property Checked: Boolean read FChecked write SetChecked default false;
    property DisabledPicture: TGDIPPicture read FIDisabledPicture write SetDisabledPicture;
    property Enabled: boolean read FEnabled write SetEnabled default true;
    property Glow: Boolean read FGlow write SetGlow default false;
    property GlowColor: TColor read FGlowColor write SetGlowColor default clRed;
    property ImageIndex: Integer read FImageIndex write SetImageIndex default -1;
    property Locked: Boolean read FLocked write SetLocked default false;
    property Name: string read FName write FName;
    property OfficeHint: TAdvHintInfo read FOfficeHint write SetOfficeHint;
    property Picture: TGDIPPicture read FIPicture write SetPicture;
    property Progress: TTabProgress read FProgress write SetProgress;
    property ShowCheckBox: Boolean read FShowCheckBox write SetShowCheckBox default false;
    property ShowClose: Boolean read FShowClose write SetShowClose default true;
    property Tag: integer read FTag write FTag default 0;
    property TabAppearance: TTabAppearance read FTabAppearance write SetTabAppearance;
    property UseTabAppearance: Boolean read FUseTabAppearance write SetUseTabAppearance default false;
    property Visible: Boolean read FVisible write SetVisible default true;
  end;

  TOfficeTabCollection = class(TCollection)
  private
    FOwner: TAdvCustomOfficeTabSet;
    function GetItem(Index: Integer): TOfficeTabCollectionItem;
    procedure SetItem(Index: Integer; const Value: TOfficeTabCollectionItem);
  protected
    {$IFDEF DELPHI7_LVL}
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); override;
    {$ENDIF}
  public
    constructor Create(AOwner: TAdvCustomOfficeTabSet);
    property Items[Index: Integer]: TOfficeTabCollectionItem read GetItem write SetItem; default;
    property AdvOfficeTabSet: TAdvCustomOfficeTabSet read FOwner;
    function Add: TOfficeTabCollectionItem;
    procedure Delete(Index: Integer);
    procedure Clear;
    function Insert(Index: Integer): TOfficeTabCollectionItem;
    function GetOwner: TPersistent; override;
    procedure Move(CurIndex, NewIndex : integer);
  end;

  TTabChangingEvent = procedure(Sender: TObject; FromTab, ToTab: Integer; var AllowChange: Boolean) of object;
  TOnTabClose = procedure (Sender:TObject; TabIndex: integer; var Allow: boolean) of object;
  TOnTabListClick = procedure (Sender: TObject; X, Y: integer) of object;
  TDrawTabEvent = procedure(Sender:TObject; Canvas: TCanvas; TabIndex: integer; TabRect: TRect) of object;
  TTabClickEvent = procedure(Sender: TObject; TabIndex: integer) of object;
  TClosedListClick = procedure (Sender: TObject; X, Y: integer) of object;
  TTabEvent = procedure(Sender: TObject; TabIndex: integer) of object;
  TTabItemEvent = procedure(Sender: TObject; ATab: TOfficeTabCollectionItem) of object;

  TAdvCustomOfficeTabSet = class(TCustomControl, ITMSTones)
  private
    FCloseButtonGlobal: TAdvGlowButton;
    FIsWinXP: Boolean;
    FInternalOfficeTabSetStyler: TCustomAdvOfficeTabSetStyler;
    FOfficeTabSetStyler: TCustomAdvOfficeTabSetStyler;
    FCurrentOfficeTabSetStyler: TCustomAdvOfficeTabSetStyler;
    FOffSetY: integer;
    FOffSetX: integer;
    //FAdvPages: TDbgList;
    FAdvOfficeTabs: TOfficeTabCollection;
    FPropertiesLoaded: Boolean;
    FShowNonSelectedTabs: Boolean;
    FTabSettings: TTabSetTabSettings;
    FTabScroller: TTabSetTabScroller;
    FActiveTabIndex: Integer;
    FHotTabIndex: Integer;
    FDownTabIndex: Integer;
    FOldHotTabIndex: Integer;
    FHintTabIndex: Integer;
    FImages: TCustomImageList;
    FDisabledImages: TCustomImageList;
    //FScrollerHoverLeftBtn: Boolean;
    //FScrollerDownLeftBtn: Boolean;
    //FScrollerHoverRightBtn: Boolean;
    //FScrollerDownRightBtn: Boolean;
    FShowTabHint: Boolean;
    FOnChange: TNotifyEvent;
    FOnChanging: TTabChangingEvent;
    FOldCapRightIndent: Integer;
    FOfficeHint: TAdvHintInfo;
    FTabPosition: TTabPosition;
    FAntiAlias: TAntiAlias;
    FButtonSettings: TTabSetButtonSettings;
    FTabListMenu: TPopupMenu;
    FOnTabClose: TOnTabClose;
    FOnTabListClick: TOnTabListClick;
    FRotateTabLeftRight: Boolean;
    FCloseOnTab: Boolean;
    FTabListButton: TAdvGlowButton;
    FScrollPrevButton: TAdvGlowButton;
    FScrollNextButton: TAdvGlowButton;
    FScrollFirstButton: TAdvGlowButton;
    FScrollLastButton: TAdvGlowButton;
    FTabOffSet: Integer;
    FUseMaxSpace: Boolean;
    FFreeOnClose: Boolean;
    FFormScaled: Boolean;
    FDummyHintControl: TDummyHintControl;
    FTabRearrange: Boolean;
    FOnTabMoved: TTabMovedEvent;
    FOnDrawTab: TDrawTabEvent;
    FCloseOnTabPosition: TCloseOnTabPos;
    FDesignTime: boolean;
    FOnTabRightClick: TTabClickEvent;
    FOnTabClick: TTabClickEvent;
    FOnTabDblClick: TTabClickEvent;
    FUpdateCount: integer;
    FGlow: Boolean;
    FTransparent: Boolean;
    FShowCloseOnNonSelectedTabs: Boolean;
    FOnTabCheckBoxClick: TTabClickEvent;
    FOnLastClick: TNotifyEvent;
    FOnFirstClick: TNotifyEvent;
    FOnPrevClick: TNotifyEvent;
    FOnNextClick: TNotifyEvent;
    FIsAeroVista: Boolean;
    FClosedListButton: TAdvGlowButton;
    FClosedListMenu: TPopupMenu;
    FInternalClosedListMenu: TPopupMenu;
    FOnClosedListClick: TClosedListClick;
    FClosedTabList: TStringList;
    FOnTabMouseLeave: TTabEvent;
    FOnTabMouseEnter: TTabEvent;
    FBrGlowTimer: TTimer;
    FStepHover: Integer;
    FTimeInc: Integer;
    FArrow: TArrowWindow;
    FFixedTabs: Integer;
    FTabRearrangeIndicatorColor: TColor;
    FInsertButtonHot: Boolean;
    FInsertButtonDown: Boolean;
    FAllowTabUndock: Boolean;
    //--- Docking
    FDraging: Boolean;
    FOldMouseX, FOldMouseY: Integer;
    FFloatingTabs: TDbgList;
    FDragingTab: TOfficeTabCollectionItem;
    FOnTabUnDock: TTabItemEvent;
    //---
    FTabRoundEdges: Boolean;
    FShadow: Boolean;
    FShow3D: Boolean;
    FItones: Boolean;
    FTabAppearance: TTabAppearance;
    FGlowButtonAppearance: TGlowButtonAppearance;
    FOnTabInsert: TTabItemEvent;
    //--- MultiLine
    FMultiLine: Boolean;
    FTabRows: array of TPoint;  // 1: start index;  2: end index
    FMultiLineRowCount: Integer;
    FTabRowSize: Integer;
    FAutoHeight: Boolean;  // Height in case of tpTop, tpBottom and width otherwise
    //---
    procedure WMEraseBkGnd(var Message: TWMEraseBkGnd); message WM_ERASEBKGND;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure CMVisibleChanged(var Message: TMessage); message CM_VISIBLECHANGED;
    procedure CMShowingChanged(var Message: TMessage); message CM_SHOWINGCHANGED;
    procedure CMControlChange(var Message: TCMControlChange); message CM_CONTROLCHANGE;
    procedure CMControlListChange(var Message: TCMControlListChange); message CM_CONTROLLISTCHANGE;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMDesignHitTest(var Msg: TCMDesignHitTest); message CM_DESIGNHITTEST;
    procedure CMHintShow(var Message: TMessage); message CM_HINTSHOW;
    procedure WMNCHitTest(var Msg: TWMNCHitTest); message WM_NCHITTEST;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure OnTabSettingsChanged(Sender: TObject);
    procedure OnButtonSettingChanged(Sender: TObject);
    procedure OnCloseButtonClick(Sender: TObject);
    procedure OnTabListButtonClick(Sender: TObject);
    procedure OnTabListMenuClick(Sender: TObject);
    procedure OnScrollPrevButtonClick(Sender: TObject);
    procedure OnScrollNextButtonClick(Sender: TObject);
    procedure OnScrollFirstButtonClick(Sender: TObject);
    procedure OnScrollLastButtonClick(Sender: TObject);
    procedure OnEnterTab(TabIndex: Integer);
    procedure OnExitTab(TabIndex: Integer);
    //procedure SetPagePosition(AdvPage: TAdvOfficePage);
    //procedure SetAllPagesPosition;
    procedure SetOfficeTabSetStyler(const Value: TCustomAdvOfficeTabSetStyler);
    //function GetAdvOfficePageCount: integer;
    //function GetAdvPages(index: integer): TAdvOfficePage;
    function GetPopupMenuEx: TPopupMenu;
    procedure SetPopupMenuEx(const Value: TPopupMenu);
    procedure SetShowNonSelectedTabs(const Value: Boolean);
    //function GetActivePage: TAdvOfficePage;
    function GetActiveTabIndex: Integer;
    //procedure SetActivePage(const Value: TAdvOfficePage);
    procedure SetActiveTabIndex(const Value: Integer);
    procedure SetTabSettings(const Value: TTabSetTabSettings);
    procedure SetImages(const Value: TCustomImageList);
    procedure SetDisabledImages(const Value: TCustomImageList);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    procedure SetOfficeHint(const Value: TAdvHintInfo);
    procedure SetTabPosition(const Value: TTabPosition);
    procedure SetAntiAlias(const Value: TAntiAlias);
    procedure SetButtonSettings(const Value: TTabSetButtonSettings);
    procedure SetTabListMenu(const Value: TPopupMenu);
    procedure SetCloseOnTab(const Value: Boolean);
    procedure SetRotateTabLeftRight(const Value: Boolean);
    procedure SetAdvOfficeTabs(Value: TOfficeTabCollection);
    procedure SetCloseOnTabPosition(const Value: TCloseOnTabPos);
    procedure SetShowCloseOnNonSelectedTabs(const Value: Boolean);
    procedure SetTransparent(const Value: Boolean);
    procedure OnCloseListMenuClick(Sender: TObject);
    procedure ClosedListButtonClick(Sender: TObject);
    procedure CreateClosedListButton;
    procedure UpdateClosedListButton;
    procedure OnGlowTimerTime(Sender: TObject);
    procedure CreateGlowTimer;
    function IsTabShowing(TabIndex: Integer): Boolean;
    function IsAnyTabGlowing: Boolean;
    procedure CreateDropArrow;
    procedure SetFixedTabs(Value: Integer);
    procedure SetAllowTabUndock(const Value: Boolean);
    procedure SetGlowButtonColorTones(AdvGlowButton: TAdvGlowButton);
    function GetSettings: string;
    procedure SetSettings(const Value: string);
    procedure SetMultiLine(const Value: Boolean);
    procedure SetAutoHeight(const Value: Boolean);
  protected
    procedure AlignControls(AControl: TControl; var ARect: TRect); override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure AdjustClientRect(var Rect: TRect); override;
    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean); override;
    procedure DragCanceled; override;
    function DrawTab(TabIndex: Integer): boolean;
    procedure DrawAllTabs;
    //procedure DrawTabScrollBtnLeft;
    //procedure DrawTabScrollBtnRight;
    procedure DrawTabScrollButtons;
    procedure Paint; override;
    procedure WndProc(var Msg: TMessage); override;

    procedure SetChildOrder(Child: TComponent; Order: Integer); override;
    //procedure SetAdvPageBounds(AdvPage: TAdvOfficePage; var ALeft, ATop, AWidth, AHeight: Integer);

    procedure UpdateGlowTimer;

    procedure UpdateMe(PropID: integer);
    procedure ChangeActiveTab(TabIndex: Integer); virtual;

    procedure UpdateTabScroller;
    procedure ScrollLeftBtnClick;
    procedure ScrollRightBtnClick;
    procedure ScrollLastBtnClick;
    procedure ScrollFirstBtnClick;

    procedure InitializeAndUpdateButtons;
    function AnyButtonVisible: Boolean;

    function CanShowTab(TabIndex: Integer): Boolean;
    function GetVisibleTabCount: Integer;

    procedure BeforeCloseTab(Tab: TOfficeTabCollectionItem; var CloseAction: TCloseAction); virtual;
    function CanCloseTab(TabIndex: Integer; var CloseAction: TCloseAction): Boolean; virtual;

    procedure DoChanging(FromTab, ToTab: integer; var AllowChange: boolean); virtual;
    procedure DoChange; virtual;

    function GetTextSize(TabIndex: Integer): TSize;
    function GetTabImageSize(TabIndex: Integer): TSize;
    procedure GetCloseBtnImageAndTextRect(TabIndex: Integer; var CloseBtnR, TextR: TRect; var ImgP: TPoint); // used when TabSettings.Width > 0
    procedure InvalidateTab(TabIndex: Integer);
    function GetButtonsRect: TRect;
    function GetTabsArea: TRect;
    function GetTabsRect: TRect;
    function GetTabRect(StartIndex, TabIndex: Integer; ConsiderTabScroller: Boolean): TRect;  overload;
    function GetTabRect(TabIndex: Integer): TRect; overload;
    function GetCheckBoxRect(TabIndex: integer): TRect;
    function GetCloseButtonRect(TabIndex: integer): TRect;
    function GetTabListRect: TRect;
    function GetClosedListButtonRect: TRect;
    function GetTabScrollerRect: TRect;
    function GetTabScrollerFirstRect: TRect;
    function GetTabScrollerLastRect: TRect;
    function GetTabScrollerLeftRect: TRect;
    function GetTabScrollerRightRect: TRect;
    function PtOnTabScrollLeftBtn(X, Y: integer): Boolean;
    function PtOnTabScrollRightBtn(X, Y: integer): Boolean;
    function PtOnInsertButton(X, Y: Integer): Boolean;
    function GetInsertButtonRect: TRect;
    procedure InvalidateInsertButton;
    procedure DoInsertButtonClick;
    function GetLastDisplayTab: Integer;   // returns visually last tab index

    procedure SelectNextSequentialTab;

    function IsGlass: Boolean;
    function IsOnGlass: Boolean;
    procedure GlassChanged;

    function CanGlow: Boolean;

    procedure DrawInsertButton; overload;
    procedure DrawInsertButton(Canvas: TCanvas; R: TRect); overload;

    procedure BeginMove(Shift: TShiftState; X, Y: Integer; Tab: TOfficeTabCollectionItem);
    procedure Move(Shift: TShiftState; X, Y: Integer);
    procedure EndMove;

    function CanShowCloseButton: Boolean;
    function IsActiveTabNeighbour(TabIndex: Integer): Integer;   // -1= previous;  0= No;   +1= Next
    function GetLeftRoundingOffset: Integer;
    function GetRightRoundingOffset: Integer;
    function UseOldDrawing: Boolean;
    procedure UpdateTabAppearanceOfTabs;

    property AdvOfficeTabs: TOfficeTabCollection read FAdvOfficeTabs write SetAdvOfficeTabs;
    property AdvOfficeTabSetStyler: TCustomAdvOfficeTabSetStyler read FOfficeTabSetStyler write SetOfficeTabSetStyler;
    property ActiveTabIndex: Integer read GetActiveTabIndex write SetActiveTabIndex;
    property AntiAlias: TAntiAlias read FAntiAlias write SetAntiAlias default aaClearType;
    property AllowTabUndock: Boolean read FAllowTabUndock write SetAllowTabUndock default False;
    property ButtonSettings: TTabSetButtonSettings read FButtonSettings write SetButtonSettings;
    property CloseOnTab: Boolean read FCloseOnTab write SetCloseOnTab default false;
    property CloseOnTabPosition: TCloseOnTabPos read FCloseOnTabPosition write SetCloseOnTabPosition default cpRight;
    property ClosedListMenu: TPopupMenu read FClosedListMenu write FClosedListMenu;
    property DisabledImages: TCustomImageList read FDisabledImages write SetDisabledImages;
    property FreeOnClose: boolean read FFreeOnClose write FFreeOnClose default false;
    property FixedTabs: Integer read FFixedTabs write SetFixedTabs default 0;
    property Glow: Boolean read FGlow write FGlow default true;
    property Images: TCustomImageList read FImages write SetImages;
    property OfficeHint: TAdvHintInfo read FOfficeHint write SetOfficeHint;
    property TabListMenu: TPopupMenu read FTabListMenu write SetTabListMenu;
    property PopupMenu: TPopupMenu read GetPopupMenuEx write SetPopupMenuEx;
    property RotateTabLeftRight: Boolean read FRotateTabLeftRight write SetRotateTabLeftRight default true;
    property ShowCloseOnNonSelectedTabs: Boolean read FShowCloseOnNonSelectedTabs write SetShowCloseOnNonSelectedTabs default false;
    property ShowNonSelectedTabs: Boolean read FShowNonSelectedTabs write SetShowNonSelectedTabs default False;
    property ShowTabHint: Boolean read FShowTabHint write FShowTabHint default false;
    property TabPosition: TTabPosition read FTabPosition write SetTabPosition default tpTop;
    property TabRearrange: Boolean read FTabRearrange write FTabRearrange default false;
    property TabRearrangeIndicatorColor: TColor read FTabRearrangeIndicatorColor write FTabRearrangeIndicatorColor default clBlue;
    property TabSettings: TTabSetTabSettings read FTabSettings write SetTabSettings;
    property Transparent: Boolean read FTransparent write SetTransparent default false;
    property Version: string read GetVersion write SetVersion stored false;

    property TabRoundEdges: Boolean read FTabRoundEdges write FTabRoundEdges default True;
    property Show3D: Boolean read FShow3D write FShow3D default True;
    property Shadow: Boolean read FShadow write FShadow default True;

    property AutoHeight: Boolean read FAutoHeight write SetAutoHeight default False; // Works with MultiLine
    property MultiLine: Boolean read FMultiLine write SetMultiLine default False;
    procedure UpdateMultiLineTabs;
    procedure UpdateMultiLineActiveTab;
    procedure AdjustHeight;  // Works if AutoHeight and MultiLine = True, to set the required height

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChanging: TTabChangingEvent read FOnChanging write FOnChanging;
    property OnTabClose: TOnTabClose read FOnTabClose write FOnTabClose;
    property OnTabMoved: TTabMovedEvent read FOnTabMoved write FOnTabMoved;
    property OnTabListClick: TOnTabListClick read FOnTabListClick write FOnTabListClick;
    property OnDrawTab: TDrawTabEvent read FOnDrawTab write FOnDrawTab;
    property OnTabClick: TTabClickEvent read FOnTabClick write FOnTabClick;
    property OnTabDblClick: TTabClickEvent read FOnTabDblClick write FOnTabDblClick;
    property OnTabRightClick: TTabClickEvent read FOnTabRightClick write FOnTabRightClick;
    property OnTabCheckBoxClick: TTabClickEvent read FOnTabCheckBoxClick write FOnTabCheckBoxClick;
    property OnPrevClick: TNotifyEvent read FOnPrevClick write FOnPrevClick;
    property OnNextClick: TNotifyEvent read FOnNextClick write FOnNextClick;
    property OnFirstClick: TNotifyEvent read FOnFirstClick write FOnFirstClick;
    property OnLastClick: TNotifyEvent read FOnLastClick write FOnLastClick;
    property OnClosedListClick: TClosedListClick read FOnClosedListClick write FOnClosedListClick;
    property OnTabMouseEnter: TTabEvent read FOnTabMouseEnter write FOnTabMouseEnter;
    property OnTabMouseLeave: TTabEvent read FOnTabMouseLeave write FOnTabMouseLeave;
    property OnTabUnDock: TTabItemEvent read FOnTabUnDock write FOnTabUnDock;
    property OnTabInsert: TTabItemEvent read FOnTabInsert write FOnTabInsert;
  public
    constructor Create(AOwner: TComponent); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    destructor Destroy; override;
    procedure Init;
    procedure InitOrder;
    procedure ResetTabs;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure DragDrop(Source: TObject; X, Y: Integer); override;
    function PTOnTab(X, Y: Integer): Integer;
    function PTOnCheckBox(PageIndex, X, Y: integer): Boolean;
    function GetVersionNr: integer;
    //procedure MoveAdvOfficeTab(CurIndex, NewIndex: Integer);
    function FindNextTab(TabIndex: Integer; GoForward, CheckTabVisible: Boolean): Integer;
    procedure SelectNextTab(GoForward: Boolean);
    procedure SetFloating(Index: integer);
    procedure CloseAllTabs; virtual;
    //function IndexOfPage(AdvPage: TAdvOfficePage): Integer;
    procedure OpenAllClosedTabs;
    function OpenClosedTab(ATab: TOfficeTabCollectionItem): Boolean;
    procedure AddTab(Caption: string);
    procedure SaveToFile(FileName: String); // Save tab order and visibility
    procedure LoadFromFile(FileName: String); // load saved state ie: tab order, visibility
    procedure SetColorTones(ATones: TColorTones);
    property Settings: string read GetSettings write SetSettings;
    function TabByName(Value: string): TOfficeTabCollectionItem;
    function TabByCaption(Value: string): TOfficeTabCollectionItem;
    function TabAtTabIndex(Value: integer): TOfficeTabCollectionItem;
  published
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvOfficeTabSet = class(TAdvCustomOfficeTabSet)
  public
  published
    property AdvOfficeTabs;
    property AdvOfficeTabSetStyler;
    property Align;
    property AllowTabUndock;
    property Anchors;
    property ActiveTabIndex;
    property AntiAlias;
    property AutoHeight;
    property ButtonSettings;
    property Constraints;
    property CloseOnTab;
    property CloseOnTabPosition;
    property ClosedListMenu;
    property DisabledImages;
    property DragKind;
    property DragMode;
    property FreeOnClose;
    property FixedTabs;
    property Glow;
    property Images;
    property MultiLine;
    property OfficeHint;
    property TabListMenu;
    property PopupMenu;
    property RotateTabLeftRight;
    property ShowNonSelectedTabs;
    property ShowCloseOnNonSelectedTabs;
    property ShowTabHint;
    property ShowHint;
    property TabPosition;
    property TabRearrange;
    property TabSettings;
    property Transparent;
    property Version;
    property Visible;
    property OnChange;
    property OnChanging;
    property OnDragOver;
    property OnDragDrop;
    property OnDrawTab;
    property OnEndDrag;
    property OnTabClose;
    property OnTabMoved;
    property OnTabClick;
    property OnTabCheckBoxClick;
    property OnTabDblClick;
    property OnTabRightClick;
    property OnTabListClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDrag;
    property OnPrevClick;
    property OnNextClick;
    property OnFirstClick;
    property OnLastClick;
    property OnClosedListClick;
    property OnTabInsert;
    property OnTabMouseEnter;
    property OnTabMouseLeave;
    property OnTabUnDock;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvOfficeMDITabSet = class(TAdvCustomOfficeTabSet)
  private
    FInternalDelete: Boolean;
    function GetAdvOfficeTabCount: integer;
    function GetAdvOfficeTabs(index: integer): TOfficeTabCollectionItem;
    procedure OnChildFormActivate(Sender: TObject);
    procedure OnChildFormDestroy(Sender: TObject);
  protected
    procedure ChangeActiveTab(TabIndex: Integer); override;
    procedure BeforeCloseTab(Tab: TOfficeTabCollectionItem; var CloseAction: TCloseAction); override;
    function CanCloseTab(TabIndex: Integer; var CloseAction: TCloseAction): Boolean; override;
    procedure Change;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function AddTab(ChildForm: TForm): TOfficeTabCollectionItem;
    procedure RemoveTab(ChildForm: TForm);
    function GetChildForm(Tab: TOfficeTabCollectionItem): TForm;
    procedure CloseAllTabs; override;

    property AdvOfficeTabCount: integer read GetAdvOfficeTabCount;
    property AdvOfficeTabs[index: integer]: TOfficeTabCollectionItem read GetAdvOfficeTabs;
    function GetTab(AChild: TForm): TOfficeTabCollectionItem;
    property ActiveTabIndex;
  published
    property AdvOfficeTabSetStyler;
    property Align;
    property AllowTabUndock;
    property Anchors;
    property AntiAlias;
    property ButtonSettings;
    property Constraints;
    property CloseOnTab;
    property CloseOnTabPosition;
    property ClosedListMenu;
    property DisabledImages;
    //property FreeOnClose;
    property Glow;
    property Images;
    property OfficeHint;
    property TabListMenu;
    property PopupMenu;
    property RotateTabLeftRight;
    property ShowNonSelectedTabs;
    property ShowCloseOnNonSelectedTabs;
    property ShowTabHint;
    property ShowHint;
    property TabPosition;
    property TabRearrange;
    property TabSettings;
    property Version;
    property Visible;
    property OnChange;
    property OnChanging;
    property OnDrawTab;
    property OnTabClose;
    property OnTabMoved;
    property OnTabClick;
    property OnTabInsert;
    property OnTabDblClick;
    property OnTabRightClick;
    property OnTabListClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDrag;
    property OnPrevClick;
    property OnNextClick;
    property OnFirstClick;
    property OnLastClick;
    property OnClosedListClick;
    property OnTabMouseEnter;
    property OnTabMouseLeave;
    property OnTabUnDock;
  end;

implementation

uses
  AdvOfficeTabSetStylers
  {$IFDEF DELPHI2007_LVL}
  , uxtheme, Themes
  {$ENDIF}
  , ActiveX;

//------------------------------------------------------------------------------

function CalculateDPIScale(FHDC: HDC): single;
var
  FDPI: integer;

  function FontHeightAtDpi(iDPI, iFontSize: Integer): Integer;
  var
    FTmpCanvas: TCanvas;
  begin
    FTmpCanvas := TCanvas.Create;
    try
      FTmpCanvas.Handle := GetDC(0);
      FTmpCanvas.Font.PixelsPerInch := iDPI; //must be set BEFORE size
      FTmpCanvas.Font.Size := iFontSize;
      Result := FTmpCanvas.TextHeight('0');
    finally
      FTmpCanvas.Free;
    end;
  end;

begin
  Result := 1.0;

  if FHDC = 0 then
    FHDC:= GetDC(0);
  try
    FDPI := GetDeviceCaps(FHDC, LOGPIXELSX);
    if FDPI <> 96 then
      Result := FontHeightAtDpi(FDPI, 9) / FontHeightAtDpi(96, 9);
  finally
    if FHDC = 0 then
      ReleaseDC(0, FHDC);
  end;

end;

//------------------------------------------------------------------------------

function GetDPIScale(Scaled: boolean; FHandle: HDC = 0): single;
begin
  Result := 1.0;
  if Scaled then
    Result := CalculateDPIScale(FHandle);
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

procedure ClearAppearance(Appearance: TGlowButtonAppearance);
begin
  if not Assigned(Appearance) then
    Exit;

  with Appearance do
  begin
    BorderColor := clNone;
    BorderColorHot := clNone;
    BorderColorDown := clNone;
    Color := clNone;
    ColorTo := clNone;
    ColorDown := clNone;
    ColorDownTo := clNone;
    ColorHot := clNone;
    ColorHotTo := clNone;
    ColorMirror := clNone;
    ColorMirrorTo := clNone;
    ColorMirrorHot := clNone;
    ColorMirrorHotTo  := clNone;
    ColorMirrorDown := clNone;
    ColorMirrorDownTo := clNone;
    ColorDisabled := clNone;
    ColorDisabledTo := clNone;
  end;
end;

//------------------------------------------------------------------------------

procedure DrawGradient(Canvas: TCanvas; FromColor, ToColor: TColor; Steps: Integer; R: TRect; Direction: Boolean);
var
  diffr, startr, endr: Integer;
  diffg, startg, endg: Integer;
  diffb, startb, endb: Integer;
  rstepr, rstepg, rstepb, rstepw: Real;
  i, stepw: Word;

begin
  if Direction then
    R.Right := R.Right - 1
  else
    R.Bottom := R.Bottom - 1;

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

procedure DrawRectangle(Canvas: TCanvas; R: TRect; aColor: TColor);
begin
  Canvas.Brush.Color := aColor;
  Canvas.FillRect(R);
end;

//-------------------------------------------------------------------- DrawGauge

type
  TGaugeOrientation = (goHorizontal, goVertical);
  TGaugeSettings = record
    Level0Color: TColor;
    Level0ColorTo: TColor;
    Level1Color: TColor;
    Level1ColorTo: TColor;
    Level2Color: TColor;
    Level2ColorTo: TColor;
    Level3Color: TColor;
    Level3ColorTo: TColor;
    Level1Perc: Integer;
    Level2Perc: Integer;
    BorderColor: TColor;
    ShowBorder: Boolean;
    Stacked: Boolean;
    ShowPercentage: Boolean;
    Font: TFont;
    CompletionSmooth: Boolean;
    ShowGradient: Boolean;
    Steps: Integer;
    Position: Integer;
    BackgroundColor: TColor;
    Orientation: TGaugeOrientation;
    Min: Integer;
    Max: Integer;
  end;

procedure DrawGauge(Canvas: TCanvas; R: TRect; Position: Integer;
  Settings: TGaugeSettings);
var
  RectL: TRect;
  RectM: TRect;
  RectR: TRect;

  WidthBar: integer;
  WidthPart: Integer;
  Continue: Boolean;
  GradDir: Boolean;
  BrushColor: TColor;
  BrushColorTo: TColor;
  Percentage: Integer;
  BarFilled: Integer;
  NumberOfBlock: Integer;
  i: Integer;
  EmptyWidth: integer;

{$IFNDEF TMSCLX}
  lf: TLogFont;
{$ENDIF}
  tf: TFont;

  R1: TRect;
  R2: TRect;
  Max, OrgPosition: Integer;
begin
  if (Settings.Orientation = goHorizontal) then
    WidthBar := R.Right - R.Left
  else
    WidthBar := R.Bottom - R.Top;

  Continue := true;
  Percentage := -1;
  Canvas.Brush.Color := Settings.BackgroundColor;
  GradDir := not (Settings.Orientation = goHorizontal);

  if (Settings.ShowPercentage) then
    Percentage := Position;

  OrgPosition := Position;
  Max := Settings.Max - Settings.Min;
  Position := Math.Max(Position, Settings.Min);
  Position := Min(Position, Settings.Max);
  Position := abs(Position - Settings.Min);

  //Draw Border
  if (Settings.ShowBorder) then
    Canvas.Pen.Color := Settings.BorderColor
  else
    Canvas.Pen.Color := Settings.BackgroundColor;

  Canvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);

  WidthBar := WidthBar - 2;

  if (OrgPosition > Settings.Min) then
  begin
    if (Settings.Stacked) then
    begin
      if (OrgPosition >= Settings.Level1Perc) then
        WidthPart := Round((Settings.Level1Perc / Max) * WidthBar)
      else
      begin
        WidthPart := Round((Position / Max) * WidthBar);
        Continue := false;
      end;

      //Draw first part
      if (Settings.Orientation = goHorizontal) then
      begin
        RectL.Left := R.Left + 1;
        RectL.Top := R.Top + 1;
        RectL.Right := RectL.Left + WidthPart;
        RectL.Bottom := r.Bottom - 1;
      end
      else
      begin
        RectL.Left := r.Left + 1;
        RectL.Right := R.Right - 1;
        RectL.Top := R.Bottom - WidthPart;
        RectL.Bottom := R.Bottom - 1;
      end;

      if (Settings.ShowGradient) then
      begin
        if not (Settings.Orientation = goHorizontal) then
        begin
          R1.Left := RectL.Left;
          R1.Right := RectL.Left + (RectL.Right - RectL.Left) div 2;
          R1.Bottom := RectL.Bottom;
          R1.Top := RectL.Top;
          R2.Left := R1.Right;
          R2.Right := RectL.Right;
          R2.Bottom := RectL.Bottom;
          R2.Top := RectL.Top;
        end
        else
        begin
          R1.Left := RectL.Left;
          R1.Right := RectL.Right;
          R1.Top := RectL.Top;
          R1.Bottom := RectL.Top + (RectL.Bottom - RectL.Top) div 2;
          R2.Top := R1.Bottom;
          R2.Left := RectL.Left;
          R2.Right := RectL.Right;
          R2.Bottom := RectL.Bottom;
        end;
        DrawGradient(Canvas, Settings.Level0ColorTo,
          Settings.Level0Color, Settings.Steps, R1, GradDir);
        DrawGradient(Canvas, Settings.Level0Color,
          Settings.Level0ColorTo, Settings.Steps, R2, GradDir);
      end
      else
        DrawRectangle(Canvas, RectL, Settings.Level0Color);

      BarFilled := WidthPart;

      if (Continue) then
      begin
        //Draw second part
        if (Settings.Orientation = goHorizontal) then
        begin
          RectM.Left := RectL.Right;
          RectM.Top := r.Top + 1;
          RectM.Bottom := r.Bottom - 1;
        end
        else
        begin
          RectM.Left := R.Left + 1;
          RectM.Right := R.Right - 1;
          RectM.Bottom := RectL.Top;
        end;

        if (OrgPosition >= Settings.Level2Perc) then
          WidthPart := Round(WidthBar * ((Settings.Level2Perc -
            Settings.Level1Perc) / Max))
        else
        begin
          WidthPart := Round(WidthBar * ((Position -
            Settings.Level1Perc) / Max));
          Continue := false;
        end;

        if (Settings.Orientation = goHorizontal) then
          RectM.Right := WidthPart + RectM.Left
        else
          RectM.Top := RectM.Bottom - WidthPart;

        if (Settings.ShowGradient) then
        begin
          if not (Settings.Orientation = goHorizontal) then
          begin
            R1.Left := RectM.Left;
            R1.Right := RectM.Left + (RectM.Right - RectM.Left) div 2;
            R1.Bottom := RectM.Bottom;
            R1.Top := RectM.Top;
            R2.Left := R1.Right;
            R2.Right := RectM.Right;
            R2.Bottom := RectM.Bottom;
            R2.Top := RectM.Top;
          end
          else
          begin
            R1.Left := RectM.Left;
            R1.Right := RectM.Right;
            R1.Top := RectM.Top;
            R1.Bottom := RectM.Top + (RectM.Bottom - RectM.Top) div 2;
            R2.Top := R1.Bottom;
            R2.Left := RectM.Left;
            R2.Right := RectM.Right;
            R2.Bottom := RectM.Bottom;
          end;
          DrawGradient(Canvas, Settings.Level1ColorTo,
            Settings.Level1Color, Settings.Steps, R1, GradDir);
          DrawGradient(Canvas,
            Settings.Level1Color, Settings.Level1ColorTo,
            Settings.Steps, R2, GradDir);
        end
        else
          DrawRectangle(Canvas, RectM, Settings.Level1Color);

        BarFilled := BarFilled + WidthPart;
        if (Continue) then
        begin
          //Draw third part
          if (OrgPosition = Max) then
            WidthPart := Round(WidthBar - BarFilled)
          else
            WidthPart := Round(WidthBar * ((Position -
              Settings.Level2Perc) / Max));

          if (Settings.Orientation = goHorizontal) then
          begin
            RectR.Left := RectM.Right;
            RectR.Top := R.Top + 1;
            RectR.Bottom := r.Bottom - 1;
            RectR.Right := RectR.Left + WidthPart;
          end
          else
          begin
            RectR.Left := R.Left + 1;
            RectR.Right := R.Right - 1;
            RectR.Bottom := RectM.Top - 1;
            RectR.Top := RectR.Bottom - WidthPart;
          end;

          if (Settings.ShowGradient) then
          begin
            if not (Settings.Orientation = goHorizontal) then
            begin
              R1.Left := RectR.Left;
              R1.Right := RectR.Left + (RectR.Right - RectR.Left) div
                2;
              R1.Bottom := RectR.Bottom;
              R1.Top := RectR.Top;
              R2.Left := R1.Right;
              R2.Right := RectR.Right;
              R2.Bottom := RectR.Bottom;
              R2.Top := RectR.Top;
            end
            else
            begin
              R1.Left := RectR.Left;
              R1.Right := RectR.Right;
              R1.Top := RectR.Top;
              R1.Bottom := RectR.Top + (RectR.Bottom - RectR.Top) div
                2;
              R2.Top := R1.Bottom;
              R2.Left := RectR.Left;
              R2.Right := RectR.Right;
              R2.Bottom := RectR.Bottom;
            end;
            DrawGradient(Canvas, Settings.Level3ColorTo,
              Settings.Level3Color, Settings.Steps, R1, GradDir);
            DrawGradient(Canvas, Settings.Level3Color,
              Settings.Level3ColorTo, Settings.Steps, R2, GradDir);
          end
          else
            DrawRectangle(Canvas, RectR, Settings.Level3Color);
        end;
      end;
    end
    else
    begin
      if (OrgPosition < Settings.Level1Perc) then
      begin
        BrushColor := Settings.Level0Color;
        BrushColorTo := Settings.Level0ColorTo;
      end
      else
      begin
        if (OrgPosition < Settings.Level2Perc) then
        begin
          BrushColor := Settings.Level1Color;
          BrushColorTo := Settings.Level1ColorTo;
        end
        else
        begin
          if (OrgPosition < Max) then
          begin
            BrushColor := Settings.Level2Color;
            BrushColorTo := Settings.Level2ColorTo;
          end
          else
          begin
            BrushColor := Settings.Level3Color;
            BrushColorTo := Settings.Level3ColorTo;
          end;
        end;
      end;

      if not (Settings.CompletionSmooth) then
      begin
        Canvas.Brush.Color := Settings.BackgroundColor;

        if (Round((Position * WidthBar) / Max) > 9) then
        begin
          if (Settings.Orientation = goHorizontal) then
          begin
            RectL.Left := R.Left + 2;
            RectL.Right := RectL.Left + 7;
            RectL.Top := R.Top + 2;
            RectL.Bottom := R.Bottom - 2;
          end
          else
          begin
            RectL.Left := R.Left + 2;
            RectL.Right := R.Right - 2;
            RectL.Bottom := R.Bottom - 2;
            RectL.Top := RectL.Bottom - 7;
          end;

          if (Settings.ShowGradient) then
          begin
            if not (Settings.Orientation = goHorizontal) then
            begin
              R1.Left := RectL.Left;
              R1.Right := RectL.Left + (RectL.Right - RectL.Left) div
                2;
              R1.Bottom := RectL.Bottom;
              R1.Top := RectL.Top;
              R2.Left := R1.Right;
              R2.Right := RectL.Right;
              R2.Bottom := RectL.Bottom;
              R2.Top := RectL.Top;
            end
            else
            begin
              R1.Left := RectL.Left;
              R1.Right := RectL.Right;
              R1.Top := RectL.Top;
              R1.Bottom := RectL.Top + (RectL.Bottom - RectL.Top) div
                2;
              R2.Top := R1.Bottom;
              R2.Left := RectL.Left;
              R2.Right := RectL.Right;
              R2.Bottom := RectL.Bottom;
            end;
            DrawGradient(Canvas, BrushColorTo, BrushColor,
              Settings.Steps, R1, GradDir);
            DrawGradient(Canvas, BrushColor, BrushColorTo,
              Settings.Steps, R2, GradDir);
          end
          else
            DrawRectangle(Canvas, RectL, BrushColor);

          NumberOfBlock := (Round((Position * WidthBar) / Max) div 9) -
            1;
          EmptyWidth := Round((Position * WidthBar) / Max) mod 9;

          for i := 0 to NumberOfBlock - 1 do
          begin
            if (Settings.Orientation = goHorizontal) then
            begin
              RectL.Left := RectL.Right + 2;
              RectL.Right := RectL.Left + 7;
            end
            else
            begin
              RectL.Bottom := RectL.Top - 2;
              RectL.Top := RectL.Bottom - 7;
            end;

            if (Settings.ShowGradient) then
            begin
              if not (Settings.Orientation = goHorizontal) then
              begin
                R1.Left := RectL.Left;
                R1.Right := RectL.Left + (RectL.Right - RectL.Left) div
                  2;
                R1.Bottom := RectL.Bottom;
                R1.Top := RectL.Top;
                R2.Left := R1.Right;
                R2.Right := RectL.Right;
                R2.Bottom := RectL.Bottom;
                R2.Top := RectL.Top;
              end
              else
              begin
                R1.Left := RectL.Left;
                R1.Right := RectL.Right;
                R1.Top := RectL.Top;
                R1.Bottom := RectL.Top + (RectL.Bottom - RectL.Top) div
                  2;
                R2.Top := R1.Bottom;
                R2.Left := RectL.Left;
                R2.Right := RectL.Right;
                R2.Bottom := RectL.Bottom;
              end;
              DrawGradient(Canvas, BrushColorTo, BrushColor,
                Settings.Steps, R1, GradDir);
              DrawGradient(Canvas, BrushColor, BrushColorTo,
                Settings.Steps, R2, GradDir);
            end
            else
              DrawRectangle(Canvas, RectL, BrushColor);
          end;

          if (EmptyWidth > 2) then
          begin
            if (Settings.Orientation = goHorizontal) then
            begin
              RectL.Left := RectL.Right + 2;
              RectL.Right := RectL.Left + (EmptyWidth - 1);
            end
            else
            begin
              RectL.Bottom := RectL.Top - 2;
              RectL.Top := RectL.Bottom - (EmptyWidth - 1);
            end;

            if (Settings.ShowGradient) then
            begin
              if not (Settings.Orientation = goHorizontal) then
              begin
                R1.Left := RectL.Left;
                R1.Right := RectL.Left + (RectL.Right - RectL.Left) div
                  2;
                R1.Bottom := RectL.Bottom;
                R1.Top := RectL.Top;
                R2.Left := R1.Right;
                R2.Right := RectL.Right;
                R2.Bottom := RectL.Bottom;
                R2.Top := RectL.Top;
              end
              else
              begin
                R1.Left := RectL.Left;
                R1.Right := RectL.Right;
                R1.Top := RectL.Top;
                R1.Bottom := RectL.Top + (RectL.Bottom - RectL.Top) div
                  2;
                R2.Top := R1.Bottom;
                R2.Left := RectL.Left;
                R2.Right := RectL.Right;
                R2.Bottom := RectL.Bottom;
              end;
              DrawGradient(Canvas, BrushColorTo, BrushColor,
                Settings.Steps, R1, GradDir);
              DrawGradient(Canvas, BrushColor, BrushColorTo,
                Settings.Steps, R2, GradDir);
            end
            else
              DrawRectangle(Canvas, RectL, BrushColor);
          end;
          Canvas.Brush.style := bsClear;
        end
        else
        begin
          if (Round((Position * WidthBar) / Max) > 1) then
          begin
            if (Settings.Orientation = goHorizontal) then
            begin
              RectL.Left := R.Left + 2;
              RectL.Right := RectL.Left + (Round((Position *
                WidthBar) / Max) - 1);
              RectL.Top := R.Top + 2;
              RectL.Bottom := R.Bottom - 2;
            end
            else
            begin
              RectL.Left := R.Left + 2;
              RectL.Right := R.Right - 2;
              RectL.Bottom := R.Bottom - 2;
              RectL.Top := RectL.Bottom - (Round((Position *
                WidthBar) / Max) - 1);
            end;

            if (Settings.ShowGradient) then
            begin
              if not (Settings.Orientation = goHorizontal) then
              begin
                R1.Left := RectL.Left;
                R1.Right := RectL.Left + (RectL.Right - RectL.Left) div
                  2;
                R1.Bottom := RectL.Bottom;
                R1.Top := RectL.Top;
                R2.Left := R1.Right;
                R2.Right := RectL.Right;
                R2.Bottom := RectL.Bottom;
                R2.Top := RectL.Top;
              end
              else
              begin
                R1.Left := RectL.Left;
                R1.Right := RectL.Right;
                R1.Top := RectL.Top;
                R1.Bottom := RectL.Top + (RectL.Bottom - RectL.Top) div
                  2;
                R2.Top := R1.Bottom;
                R2.Left := RectL.Left;
                R2.Right := RectL.Right;
                R2.Bottom := RectL.Bottom;
              end;
              DrawGradient(Canvas, BrushColorTo, BrushColor,
                Settings.Steps, R1, GradDir);
              DrawGradient(Canvas, BrushColor, BrushColorTo,
                Settings.Steps, R2, GradDir);
            end
            else
              DrawRectangle(Canvas, RectL, BrushColor);
          end;
        end;
      end
      else
      begin
        WidthPart := Round((Position / Max) * WidthBar);

        if (Settings.Orientation = goHorizontal) then
        begin
          RectL.Left := R.Left + 1;
          RectL.Top := R.Top + 1;
          RectL.Right := RectL.Left + WidthPart;
          RectL.Bottom := R.Bottom - 1;
        end
        else
        begin
          RectL.Left := r.Left + 1;
          RectL.Bottom := R.Bottom - 1;
          RectL.Top := RectL.Bottom - WidthPart;
          RectL.Right := r.Right - 1;
        end;

        if (Settings.ShowGradient) then
        begin
          if not (Settings.Orientation = goHorizontal) then
          begin
            R1.Left := RectL.Left;
            R1.Right := RectL.Left + (RectL.Right - RectL.Left) div 2;
            R1.Bottom := RectL.Bottom;
            R1.Top := RectL.Top;
            R2.Left := R1.Right;
            R2.Right := RectL.Right;
            R2.Bottom := RectL.Bottom;
            R2.Top := RectL.Top;
          end
          else
          begin
            R1.Left := RectL.Left;
            R1.Right := RectL.Right;
            R1.Top := RectL.Top;
            R1.Bottom := RectL.Top + (RectL.Bottom - RectL.Top) div 2;
            R2.Top := R1.Bottom;
            R2.Left := RectL.Left;
            R2.Right := RectL.Right;
            R2.Bottom := RectL.Bottom;
          end;
          DrawGradient(Canvas, BrushColorTo, BrushColor,
            Settings.Steps, R1, GradDir);
          DrawGradient(Canvas, BrushColor, BrushColorTo,
            Settings.Steps, R2, GradDir);
        end
        else
          DrawRectangle(Canvas, RectL, BrushColor);
      end;
    end;
  end;

  //Draw text with PositionPercentage
  if (Percentage <> -1) then
  begin
    Canvas.Brush.Style := bsClear;
    Canvas.Font.Name := Settings.Font.Name;
    Canvas.Font.Size := Settings.Font.Size;
    Canvas.Font.Color := Settings.Font.Color;
    Canvas.Font.Style := Settings.Font.Style;
    if not (Settings.Orientation = goHorizontal) then
    begin
      tf := TFont.Create;
      try
        tf.Assign(Settings.Font);

{$IFNDEF TMSCLX}

        GetObject(tf.Handle, sizeof(lf), @lf);

        lf.lfEscapement := 900;
        lf.lfOrientation := 900;
        tf.Handle := CreateFontIndirect(lf);
{$ENDIF}

        Canvas.Font.Assign(tf);
        Canvas.TextOut(R.Left + ((R.Right - R.Left) div 2 -
          (Canvas.TextHeight(IntToStr(Percentage) + '%') div 2)), R.Top +
          ((R.Bottom
          - R.Top) div 2) + Canvas.TextWidth(IntToStr(Percentage) + '%') div 2
          , IntToStr(Percentage) + '%');
      finally
        tf.Free;
      end;
    end
    else
    begin
      Canvas.TextOut(((R.Right - R.Left) div 2) -
        (Canvas.TextWidth(IntToStr(Percentage) + '%') div 2) + r.Left, r.Top +
        ((R.Bottom - R.Top) div 2) - Canvas.TextHeight(IntToStr(Percentage) +
        '%') div 2, IntToStr(Percentage) + '%');
    end;
  end;

  if (Settings.ShowBorder) then
    Canvas.Pen.Color := Settings.BorderColor
  else
    Canvas.Pen.Color := Settings.BackgroundColor;

  Canvas.Brush.Style := bsClear;
  Canvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
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

procedure DrawGDIPImageFromImageList(gr: TGPGraphics; Canvas: TCanvas; P: TPoint; Images: TCustomImageList; ImageIndex: Integer; Enable: Boolean);
var
  Img: TGPImage;
  pstm: IStream;
  hGlobal: THandle;
  pcbWrite: Longint;
  ms: TMemoryStream;
  ImageAttributes: TGPImageAttributes;
  r, g, b: byte;
  GPBmp: TGPBitmap;
  Aclr: TGPColor;
  bmp: TBitmap;
  graphics: TGPGraphics;
  hr: HResult;
begin
  if not Assigned(Images) or (ImageIndex < 0) or (not Assigned(gr) and not Assigned(Canvas)) then
    Exit;

  graphics := gr;
  if not Assigned(graphics) then
  begin
    graphics := TGPGraphics.Create(Canvas.Handle);
    graphics.SetSmoothingMode(SmoothingModeAntiAlias);
  end;

  bmp := TBitmap.Create;
  try
    bmp.Width := Images.Width;
    bmp.Height := Images.Height;
    //bmp.Canvas.Brush.Color := clFuchsia;
    //bmp.Canvas.FillRect(Rect(0, 0, bmp.Width, bmp.Height));
    Images.Draw(bmp.Canvas, 0, 0, ImageIndex, Enable);

    ms := TMemoryStream.Create;
    bmp.SaveToStream(ms);
  finally
    bmp.Free;
  end;

  hGlobal := GlobalAlloc(GMEM_MOVEABLE, ms.Size);
  if (hGlobal = 0) then
  begin
    ms.Free;
    raise Exception.Create('Could not allocate memory for image');
  end;

  pstm := nil;
  pcbWrite := 0;

  // Create IStream* from global memory
  hr := CreateStreamOnHGlobal(hGlobal, TRUE, pstm);

  if hr = S_OK then
  begin
    pstm.Write(ms.Memory, ms.Size,@pcbWrite);

    if (ms.Size = pcbWrite) then
    begin
      Img := TGPImage.Create(pstm);

      GPBmp := TGPBitmap.Create(pstm);
      GPBmp.GetPixel(0, Img.GetHeight - 1, AClr);
      GPBmp.Free;

      r := ADVGDIP.GetRed(AClr);
      g := ADVGDIP.GetGreen(AClr);
      b := ADVGDIP.GetBlue(AClr);

      ImageAttributes := TGPImageAttributes.Create;
      ImageAttributes.SetColorKey(MakeColor(r, g, b), MakeColor(r, g, b), ColorAdjustTypeDefault);
      graphics.DrawImage(Img, MakeRect(P.X, P.Y, Img.GetWidth, Img.Getheight),  // destination rectangle
        0, 0,        // upper-left corner of source rectangle
        Img.GetWidth,       // width of source rectangle
        Img.GetHeight,      // height of source rectangle
        UnitPixel,
        ImageAttributes);

      ImageAttributes.Free;
      Img.Free;
    end;
    pstm := nil;
  end
  else
    GlobalFree(hGlobal);

  ms.Free;

  if not Assigned(gr) then
    graphics.Free;
end;

//------------------------------------------------------------------------------

procedure DrawGDIPImage(gr: TGPGraphics; Canvas: TCanvas; P: TPoint; bmp: TGraphic; Transparent: Boolean = False); overload;
var
  Img: TGPImage;
  pstm: IStream;
  hGlobal: THandle;
  pcbWrite: Longint;
  ms: TMemoryStream;
  graphics: TGPGraphics;
  ImageAttributes: TGPImageAttributes;
  r, g, b: byte;
  GPBmp: TGPBitmap;
  Aclr: TGPColor;
  hr: HResult;
begin
  if (not Assigned(gr) and not Assigned(Canvas)) then
    Exit;
    
  graphics := gr;
  if not Assigned(graphics) then
  begin
    graphics := TGPGraphics.Create(Canvas.Handle);
    graphics.SetSmoothingMode(SmoothingModeAntiAlias);
  end;

  ms := TMemoryStream.Create;
  bmp.SaveToStream(ms);
  hGlobal := GlobalAlloc(GMEM_MOVEABLE, ms.Size);
  if (hGlobal = 0) then
  begin
    ms.Free;
    raise Exception.Create('Could not allocate memory for image');
  end;

  pstm := nil;
  pcbWrite := 0;

  // Create IStream* from global memory
  hr := CreateStreamOnHGlobal(hGlobal, TRUE, pstm);

  if hr = S_OK then
  begin
    pstm.Write(ms.Memory, ms.Size,@pcbWrite);

    if (ms.Size = pcbWrite) then
    begin
      Img := TGPImage.Create(pstm);

      if Transparent and (Img.GetFormat <> ifPNG) then
      begin
        GPBmp := TGPBitmap.Create(pstm);
        GPBmp.GetPixel(0, 0, AClr);
        GPBmp.Free;

        r := ADVGDIP.GetRed(AClr);
        g := ADVGDIP.GetGreen(AClr);
        b := ADVGDIP.GetBlue(AClr);

        ImageAttributes := TGPImageAttributes.Create;
        ImageAttributes.SetColorKey(MakeColor(r, g, b), MakeColor(r, g, b), ColorAdjustTypeDefault);
        graphics.DrawImage(Img, MakeRect(P.X, P.Y, Img.GetWidth, Img.Getheight),  // destination rectangle
         0, 0,        // upper-left corner of source rectangle
         Img.GetWidth,       // width of source rectangle
         Img.GetHeight,      // height of source rectangle
         UnitPixel,
         ImageAttributes);
        //graphics.DrawImage(Img, P.X, P.y);
        ImageAttributes.Free;
      end
      else
        graphics.DrawImage(Img, P.X, P.y);

      Img.Free;
    end;
    pstm := nil;
  end
  else
    GlobalFree(hGlobal);

  ms.Free;

  if not Assigned(gr) then
    graphics.Free;
end;

//------------------------------------------------------------------------------

function TrimText(Text: String; r: TRect; GDIPDraw: Boolean; graphics : TGPGraphics; Canvas: TCanvas; font: TGPFont; stringFormat: TGPStringFormat; Ellipsis: Boolean; Direction: TTabPosition): string;
var
  rectf: TGPRectF;
  w, h: Integer;
  x1,y1,y2: single;
  sizerect: TGPRectF;
  s, s2: string;
  i, j: integer;
  R2: TRect;
begin
  //R.Right := R.Right - 2;
  w := R.Right - R.Left;
  h := R.Bottom - R.Top;
  x1 := r.Left;
  y1 := r.Top;
  y2 := h;

  if Direction in [tpLeft, tpRight] then
  begin
    //h := R.Right - R.Left;
    w := R.Bottom - R.Top;
  end;

  if Ellipsis then
    s := '...'
  else
    s := '';
    
  if GDIPDraw then
  begin
    stringFormat := TGPStringFormat.Create;
    w := w - 2;
    rectf := MakeRect(x1,y1,1000,y2);
    graphics.MeasureString(Text, Length(Text), font, rectf, stringFormat, sizerect);
    //-- Add ellipsis
    if (sizerect.Width >= w) then
    begin
      rectf := MakeRect(x1,y1,1000,y2);
      j := Length(Text);
      for i := 0 to j do
      begin
        s2 := Text + s;
        graphics.MeasureString(s2, Length(s2), font, rectf, stringFormat, sizerect);
        if (sizerect.Width >= w) and (Text <> '') then
        begin
          Text := Copy(Text, 1, Length(Text)-1);
        end
        else
        begin
          Break;
        end;
      end;
      Text := Text + s;
    end;
    stringFormat.Free;
  end
  else
  begin
    R2 := Rect(0, 0, 1000, 100);
    DrawText(Canvas.Handle,PChar(Text),Length(Text), R2, DT_CALCRECT or DT_LEFT or DT_SINGLELINE);
    if (R2.Right >= w) then
    begin
      j := Length(Text);
      for i := 0 to j do
      begin
        s2 := Text + s;
        DrawText(Canvas.Handle,PChar(s2),Length(s2), R2, DT_CALCRECT or DT_LEFT or DT_SINGLELINE);
        if (R2.Right >= w) and (Text <> '') then
        begin
          Text := Copy(Text, 1, Length(Text)-1);
        end
        else
        begin
          Break;
        end;
      end;
      Text := Text + s;
    end;
  end;

  Result := Text;
end;

//------------------------------------------------------------------------------

function DrawVistaText(Canvas: TCanvas; Alignment: TAlignment; r: TRect; Caption:string; AFont: TFont; Enabled: Boolean; RealDraw: Boolean; AntiAlias: TAntiAlias; Direction: TTabPosition; Ellipsis: Boolean): TRect;
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
  stringFormat := nil;
  graphics := nil;
  fontFamily := nil;
  font := nil;
  solidBrush := nil;

  if (Caption <> '') then
  begin
    w := R.Right - R.Left;
    h := R.Bottom - R.Top;

    x1 := r.Left;
    y1 := r.Top;
    x2 := w;
    y2 := h;

    rectf := MakeRect(x1,y1,x2,y2);

    try
      if (AntiAlias <> aaNone) then
      begin
        graphics := TGPGraphics.Create(Canvas.Handle);
        fontFamily:= TGPFontFamily.Create(AFont.Name);

        if (fontFamily.Status in [FontFamilyNotFound, FontStyleNotFound]) then
        begin
          fontFamily.Free;
          fontFamily := TGPFontFamily.Create('Arial');
        end;

        fs := 0;

        if (fsBold in AFont.Style) then
          fs := fs + 1;

        if (fsItalic in AFont.Style) then
          fs := fs + 2;

        if (fsUnderline in AFont.Style) then
          fs := fs + 4;


        font := TGPFont.Create(fontFamily, AFont.Size , fs, UnitPoint);
        graphics.SetSmoothingMode(SmoothingModeAntiAlias);

        stringFormat := nil;
        if RealDraw then
        begin
          case (Direction) of
            tpTop, tpBottom: stringFormat := TGPStringFormat.Create;
            tpLeft: stringFormat := TGPStringFormat.Create; //($00000002);
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
          taCenter: stringFormat.SetAlignment(StringAlignmentCenter);
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
      end;

      if (AntiAlias = aaNone) then
      begin
        szRect.Left := round(rectf.X);
        szRect.Top := round(rectf.Y);

        szRect.Right := szRect.Left + 2;
        DTFLAG := DT_CALCRECT or DT_LEFT;
        if Ellipsis then
          DTFLAG := DTFLAG or DT_END_ELLIPSIS
        else
          DTFLAG := DTFLAG or DT_WORDBREAK;

        if RealDraw and Ellipsis then
        begin
          Caption := TrimText(Caption, r, False, nil, Canvas, Font, nil, True, Direction);
        end;

        szRect.Bottom := DrawText(Canvas.Handle,PChar(Caption),Length(Caption), szrect, DTFLAG);

        sizeRect.X := szRect.Left;
        //sizeRect.Y := szRect.Top;
        sizeRect.Y := R.Top + ((R.Bottom - R.Top) - szRect.Bottom) div 2;
        sizeRect.Width := szRect.Right - szRect.Left;
        sizeRect.Height := szRect.Bottom {- szRect.Top};
      end
      else
      begin
        if RealDraw and Ellipsis then
        begin
          //stringFormat.SetTrimming(StringTrimmingEllipsisCharacter);
          Caption := TrimText(Caption, r, True, graphics, nil, font, stringformat, True, Direction);
        end;

        graphics.MeasureString(Caption, Length(Caption), font, rectf, stringFormat, sizerect);
      end;


    Result := Rect(round(sizerect.X), Round(sizerect.Y), Round(sizerect.X + sizerect.Width), Round(sizerect.Y + sizerect.Height));
    rectf := MakeRect(x1,y1,x2,y2);

    if RealDraw then
    begin
      //graphics.DrawString(Caption, Length(Caption), font, rectf, stringFormat, solidBrush);
      if (AntiAlias = aaNone) then
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

        {if Ellipsis then
        begin
          Caption := TrimText(Caption, r, False, nil, Canvas, font, stringformat, True);
        end;}
        DrawText(Canvas.Handle,PChar(Caption),Length(Caption), szrect, DTFLAG or DT_VCENTER or DT_SINGLELINE)
      end
      else
      begin
        {if Ellipsis then
        begin
          //stringFormat.SetTrimming(StringTrimmingEllipsisCharacter);
          Caption := TrimText(Caption, r, True, graphics, nil, font, stringformat, True);
        end;}

        graphics.DrawString(Caption, Length(Caption), font, rectf, stringFormat, solidBrush);
      end;
    end;
    finally

      if (AntiAlias <> aaNone) then
      begin
        if Assigned(stringformat) then
          FreeAndNil(stringformat);

        if Assigned(solidBrush) then
          FreeAndNil(solidBrush);

        if Assigned(font) then
          FreeAndNil(font);

        if Assigned(fontfamily) then
          FreeAndNil(fontfamily);

        if Assigned(graphics) then
          FreeAndNil(graphics);
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure DrawVistaGradient(Canvas: TCanvas; r: TRect; CFU, CTU, CFB, CTB, PC: TColor;
   GradientU,GradientB: TGDIPGradient; Caption:string; AFont: TFont;
   Images: TImageList; ImageIndex: integer; EnabledImage: Boolean; Layout: TButtonLayout;
   DropDownButton: Boolean; DrawDwLine: Boolean; Enabled: Boolean; Focus: Boolean; DropDownPos: TDropDownPosition;
   Picture: TGDIPPicture; AntiAlias: TAntiAlias; RoundEdges: Boolean; RotateLeftRight: Boolean; Direction: TTabPosition; Progress: TTabProgress); overload;
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

  graphics := TGPGraphics.Create(Canvas.Handle);

  if Assigned(Progress) and (Progress.Visible) then
  begin
    Progress.Draw(graphics, R, RotateLeftRight, Direction);
  end
  else
  begin
    if (CTU = clNone) and (CFB = clNone) then
    begin
      solidBrush := TGPSolidBrush.Create(ColorToARGB(CFU));
      graphics.FillRectangle(solidBrush, MakeRect(r.Left , r.top, w, h));
      solidBrush.Free;
    end
    else
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
          graphics.FillRectangle(pthGrBrush, r.Left + 1,r.Top + h2, w - 1, h2+1);
          pthGrBrush.Free;
        end
        else
        begin
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
  end;

  gppen := TGPPen.Create(ColorToARGB(PC),1);

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
    gppen := TGPPen.Create(ColorToARGB($E4AD89),1);
    graphics.SetSmoothingMode(SmoothingModeAntiAlias);
    DrawRoundRect(graphics, gppen,r.Left + 1,r.Top + 1, r.Right - 3, r.Bottom - 3, 3);
    gppen.Free;
    gppen := TGPPen.Create(ColorToARGB(clgray),1);
    gppen.SetDashStyle(DashStyleDot);
    graphics.SetSmoothingMode(SmoothingModeAntiAlias);
    DrawRoundRect(graphics, gppen,r.Left + 2,r.Top + 2, r.Right - 5, r.Bottom - 5, 3);
    gppen.Free;
  end;

  fontFamily:= TGPFontFamily.Create(AFont.Name);

  if (fontFamily.Status in [FontFamilyNotFound, FontStyleNotFound]) then
  begin
    fontFamily.Free;
    fontFamily := TGPFontFamily.Create('Arial');
  end;

  fs := 0;

  ImgH := 0;
  ImgW := 0;
  ImgY := 0;
  ImgX := 0;

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

  graphics.Free;
end;


procedure DrawVistaGradient(Canvas: TCanvas; r: TRect; CFU, CTU, CFB, CTB, PC: TColor;
   GradientU,GradientB: TGDIPGradient; Caption:string; AFont: TFont; Layout: TButtonLayout; Enabled: Boolean; Focus: Boolean;
   AntiAlias: TAntiAlias; RoundEdges: Boolean; Progress: TTabProgress; RotateLeftRight: Boolean; Direction: TTabPosition = tpTop); overload;
begin
  DrawVistaGradient(Canvas, r, CFU, CTU, CFB, CTB, PC, GradientU,GradientB, Caption, AFont,
   nil, -1, True, Layout, False, False, Enabled, Focus, dpRight, nil, AntiAlias, RoundEdges, RotateLeftRight, Direction, Progress);
end;

//------------------------------------------------------------------------------

function GetTabPath(R: TRect; Shape: TAdvTabShape; Rounding: TTabRounding; RotateLeftRight: Boolean; Direction: TTabPosition): TGPGraphicsPath;
var
  p, P2{, P3, P4}: array[0..2] of TGPPoint;
  P5, p6: array[0..3] of TGPPoint;
  tension: double;
  w, h, i, j, h3, w3, rd2: Integer;
begin
  w := r.Right - r.Left;
  h := r.Bottom - r.Top;
  //h2 := h div 2;
  h3 := h div 3;
  w3 := w div 3;
  tension := 0.8;
  i := 3;

  Result := TGPGraphicsPath.Create;
  case Shape of
    tsRectangle:
    begin
      case (Direction) of
        tpTop:
        begin
          p[0] := MakePoint(R.Left, R.Top + Rounding * i);
          p[1] := MakePoint(R.Left + Rounding, R.Top + Rounding);
          p[2] := MakePoint(R.Left + Rounding * i, R.Top);
          Result.AddLine(R.Left, R.Bottom, R.Left, P[0].Y);
          Result.AddCurve(PGPPoint(@p), 3, tension);

          p2[0] := MakePoint(R.Right - (Rounding * i), R.Top);
          p2[1] := MakePoint(R.Right - Rounding, R.Top + Rounding);
          p2[2] := MakePoint(R.Right, R.Top + (Rounding * i));
          Result.AddLine(P[2].x, R.Top, P2[0].X, R.Top);
          Result.AddCurve(PGPPoint(@p2), 3, tension);
          Result.AddLine(R.Right, P2[2].Y, R.Right, R.Bottom);
          Result.CloseFigure;
        end;
        tpBottom:
        begin
          p[0] := MakePoint(R.Left, R.Bottom - Rounding * i);
          p[1] := MakePoint(R.Left + Rounding, R.Bottom - Rounding);
          p[2] := MakePoint(R.Left + Rounding * i, R.Bottom);
          Result.AddLine(R.Left, R.Top, R.Left, P[0].Y);
          Result.AddCurve(PGPPoint(@p), 3, tension);

          p2[0] := MakePoint(R.Right - (Rounding * i), R.Bottom);
          p2[1] := MakePoint(R.Right - Rounding, R.Bottom - Rounding);
          p2[2] := MakePoint(R.Right, R.Bottom - (Rounding * i));
          Result.AddLine(P[2].x, R.Bottom, P2[0].X, R.Bottom);
          Result.AddCurve(PGPPoint(@p2), 3, tension);
          Result.AddLine(R.Right, P2[2].Y, R.Right, R.Top);
          Result.CloseFigure;
        end;
        tpLeft:
        begin
          p[0] := MakePoint(R.Left + Rounding * i, R.Top);
          p[1] := MakePoint(R.Left + Rounding, R.Top + Rounding);
          p[2] := MakePoint(R.Left, R.Top + Rounding * i);
          Result.AddLine(R.Right, R.Top, p[0].X, R.Top);
          Result.AddCurve(PGPPoint(@p), 3, tension);

          p2[0] := MakePoint(R.Left, R.Bottom - (Rounding * i));
          p2[1] := MakePoint(R.Left + Rounding, R.Bottom - Rounding);
          p2[2] := MakePoint(R.Left + (Rounding * i), R.Bottom);
          Result.AddLine(R.Left, P[2].Y, R.Left, p2[0].Y);
          Result.AddCurve(PGPPoint(@p2), 3, tension);
          Result.AddLine(p2[2].X, R.Bottom, R.Right, R.Bottom);
          Result.CloseFigure;
        end;
        tpRight:
        begin
          p[0] := MakePoint(R.Right - Rounding * i, R.Top);
          p[1] := MakePoint(R.Right - Rounding, R.Top + Rounding);
          p[2] := MakePoint(R.Right, R.Top + Rounding * i);
          Result.AddLine(R.Left, R.Top, p[0].X, R.Top);
          Result.AddCurve(PGPPoint(@p), 3, tension);

          p2[0] := MakePoint(R.Right, R.Bottom - (Rounding * i));
          p2[1] := MakePoint(R.Right - Rounding, R.Bottom - Rounding);
          p2[2] := MakePoint(R.Right - (Rounding * i), R.Bottom);
          Result.AddLine(R.Right, P[2].Y, R.Right, p2[0].Y);
          Result.AddCurve(PGPPoint(@p2), 3, tension);
          Result.AddLine(p2[2].X, R.Bottom, R.Left, R.Bottom);
          Result.CloseFigure;
        end;
      end;
    end;
    tsLeftRamp:
    begin
      case (Direction) of
        tpTop:
        begin
          j := h3 + Rounding;

          p5[0] := MakePoint(R.Left, R.Bottom);
          p5[1] := MakePoint(R.Left + Rounding*i, R.Bottom - Rounding{* 2});
          p5[2] := MakePoint(R.Left - Rounding + j, R.Top + Rounding);
          p5[3] := MakePoint(R.Left + (Rounding * 2) + j, R.Top);
          Result.AddBezier(P5[0],P5[1], P5[2], P5[3]);

          rd2 := Max(0, Rounding div 2);
          
          p2[0] := MakePoint(R.Right - (rd2 * i), R.Top);
          p2[1] := MakePoint(R.Right - rd2, R.Top + rd2);
          p2[2] := MakePoint(R.Right, R.Top + (rd2 * i));
          Result.AddLine(P5[3].x, R.Top, P2[0].X, R.Top);
          Result.AddCurve(PGPPoint(@p2), 3, tension);
          Result.AddLine(R.Right, p2[2].Y, R.Right, R.Bottom);

          //Result.AddLine(R.Right, R.Bottom, R.Left, R.Bottom);
          Result.CloseFigure;
        end;
        tpBottom:
        begin
          j := h3 + Rounding;

          p5[0] := MakePoint(R.Left + (Rounding * 2) + j, R.Bottom);
          p5[1] := MakePoint(R.Left - Rounding + j, R.Bottom - Rounding);
          p5[2] := MakePoint(R.Left + Rounding*i, R.Top + Rounding{* 2});
          p5[3] := MakePoint(R.Left, R.Top);
          Result.AddBezier(P5[0],P5[1], P5[2], P5[3]);

          rd2 := Max(0, Rounding div 2);

          p2[0] := MakePoint(R.Right, R.Bottom - (rd2 * i));
          p2[1] := MakePoint(R.Right - rd2, R.Bottom - rd2);
          p2[2] := MakePoint(R.Right - (rd2 * i), R.Bottom);

          Result.AddLine(R.Left, R.top, R.Right, R.Top);
          Result.AddLine(R.Right, R.Top, R.Right, p2[0].Y);
          Result.AddCurve(PGPPoint(@p2), 3, tension);
          //Result.AddLine(P2[2].X, R.Bottom, P5[0].x, R.Bottom);
          Result.CloseFigure;
        end;
        tpLeft:
        begin
          j := w3 + Rounding;
          if not RotateLeftRight then
            j := h3 + Rounding;
          rd2 := Max(0, Rounding div 2);

          p[0] := MakePoint(R.Left + Rd2 * i, R.Top);
          p[1] := MakePoint(R.Left + Rd2, R.Top + Rd2);
          p[2] := MakePoint(R.Left, R.Top + Rd2 * i);
          Result.AddLine(R.Right, R.Top, p[0].X, R.Top);
          Result.AddCurve(PGPPoint(@p), 3, tension);

          p5[0] := MakePoint(R.Left, R.Bottom - (Rounding * 2) - j);
          p5[1] := MakePoint(R.Left + Rounding, R.Bottom + Rounding - j);
          p5[2] := MakePoint(R.Right - Rounding{* 2}, R.Bottom - Rounding * i);
          p5[3] := MakePoint(R.Right, R.Bottom);
          Result.AddLine(R.Left, P[2].Y, R.Left, P5[0].Y);
          Result.AddBezier(P5[0],P5[1], P5[2], P5[3]);
          //Result.AddLine(R.Right, R.Bottom, R.Right, R.Top);
          Result.CloseFigure;
        end;
        tpRight:
        begin
          j := w3 + Rounding;
          if not RotateLeftRight then
            j := h3 + Rounding;

          p5[0] := MakePoint(R.Left, R.Top);
          p5[1] := MakePoint(R.Left + Rounding{* 2}, R.Top + Rounding*i);
          p5[2] := MakePoint(R.Right - Rounding, R.Top - Rounding + j);
          p5[3] := MakePoint(R.Right, R.Top + (Rounding * 2) + j);
          Result.AddBezier(P5[0],P5[1], P5[2], P5[3]);

          rd2 := Max(0, Rounding div 2);
          p2[0] := MakePoint(R.Right, R.Bottom - (Rd2 * i));
          p2[1] := MakePoint(R.Right - Rd2, R.Bottom - Rd2);
          p2[2] := MakePoint(R.Right - (Rd2 * i), R.Bottom);

          Result.AddLine(R.Right, P5[3].Y, R.Right, p2[0].Y);
          Result.AddCurve(PGPPoint(@p2), 3, tension);

          Result.AddLine(p2[2].X, R.Bottom, R.Left, R.Bottom);
          //Result.AddLine(R.Right, R.Bottom, R.Right, Top);
          Result.CloseFigure;
        end;
      end;
    end;
    tsRightRamp:
    begin
      case (Direction) of
        tpTop:
        begin
          //k := 0;
          //if (Rounding * i > h2) then
          //  k := i div 2;

          j := h3 + Rounding;
          //k := (j div 2);

          rd2 := Max(0, Rounding div 2);

          p[0] := MakePoint(R.Left, R.Top + rd2 * i);
          p[1] := MakePoint(R.Left + rd2, R.Top + rd2);
          p[2] := MakePoint(R.Left + rd2 * i, R.Top);

          Result.AddLine(R.Left, R.Bottom, R.Left, P[0].Y);
          Result.AddCurve(PGPPoint(@p), 3, tension);

          p5[0] := MakePoint(R.Right - (Rounding * 2) - j, R.Top);
          p5[1] := MakePoint(R.Right + Rounding - j, R.Top + Rounding);
          p5[2] := MakePoint(R.Right - Rounding*i, R.Bottom - Rounding{* 2});
          p5[3] := MakePoint(R.Right, R.Bottom);
          Result.AddLine(P[2].x, R.Top, P5[0].X, R.Top);
          //Result.AddCurve(PGPPoint(@p5), 4, tension);
          Result.AddBezier(P5[0],P5[1], P5[2], P5[3]);

          Result.AddLine(R.Left, R.Bottom, R.Right, R.Bottom);
          Result.CloseFigure;
        end;
        tpBottom:
        begin
          j := h3 + Rounding;
          rd2 := Max(0, Rounding div 2);

          p[0] := MakePoint(R.Left, R.Bottom - rd2 * i);
          p[1] := MakePoint(R.Left + Rd2, R.Bottom - Rd2);
          p[2] := MakePoint(R.Left + Rd2 * i, R.Bottom);
          Result.AddLine(R.Left, R.Top, R.Left, P[0].Y);
          Result.AddCurve(PGPPoint(@p), 3, tension);

          p5[0] := MakePoint(R.Right - (Rounding * 2) - j, R.Bottom);
          p5[1] := MakePoint(R.Right + Rounding - j, R.Bottom - Rounding);
          p5[2] := MakePoint(R.Right - Rounding*i, R.Top + Rounding{* 2});
          p5[3] := MakePoint(R.Right, R.Top);
          Result.AddLine(P[2].x, R.Bottom, P5[0].X, R.Bottom);
          Result.AddBezier(P5[0],P5[1], P5[2], P5[3]);

          Result.AddLine(R.Left, R.Top, R.Right, R.Top);
          Result.CloseFigure;
        end;
        tpLeft:
        begin
          j := w3 + Rounding;
          if not RotateLeftRight then
            j := h3 + Rounding;
          rd2 := Max(0, Rounding div 2);

          p5[0] := MakePoint(R.Right, R.Top);
          p5[1] := MakePoint(R.Right - Rounding{* 2}, R.Top + Rounding*i);
          p5[2] := MakePoint(R.Left + Rounding, R.Top - Rounding + j);
          p5[3] := MakePoint(R.Left, R.Top + (Rounding * 2) + j);
          Result.AddBezier(P5[0],P5[1], P5[2], P5[3]);

          p2[0] := MakePoint(R.Left, R.Bottom - (Rd2 * i));
          p2[1] := MakePoint(R.Left + Rd2, R.Bottom - Rd2);
          p2[2] := MakePoint(R.Left + (Rd2 * i), R.Bottom);

          Result.AddLine(R.Left, P5[3].Y, R.Left, p2[0].Y);
          Result.AddCurve(PGPPoint(@p2), 3, tension);

          Result.AddLine(p2[2].X, R.Bottom, R.Right, R.Bottom);
          //Result.AddLine(R.Right, R.Bottom, R.Right, Top);
          Result.CloseFigure;
        end;
        tpRight:
        begin
          j := w3 + Rounding;
          if not RotateLeftRight then
            j := h3 + Rounding;
          rd2 := Max(0, Rounding div 2);

          p[0] := MakePoint(R.Right - Rd2 * i, R.Top);
          p[1] := MakePoint(R.Right - Rd2, R.Top + Rd2);
          p[2] := MakePoint(R.Right, R.Top + Rd2 * i);
          Result.AddLine(R.Left, R.Top, p[0].X, R.Top);
          Result.AddCurve(PGPPoint(@p), 3, tension);

          p5[0] := MakePoint(R.Right, R.Bottom - (Rounding * 2) - j);
          p5[1] := MakePoint(R.Right - Rounding, R.Bottom + Rounding - j);
          p5[2] := MakePoint(R.Left + Rounding{* 2}, R.Bottom - Rounding * i);
          p5[3] := MakePoint(R.Left, R.Bottom);
          Result.AddLine(R.Right, P[2].Y, R.Right, P5[0].Y);
          Result.AddBezier(P5[0],P5[1], P5[2], P5[3]);
          //Result.AddLine(R.Right, R.Bottom, R.Right, R.Top);
          Result.CloseFigure;
        end;
      end;
    end;
    tsLeftRightRamp:
    begin
      case (Direction) of
        tpTop:
        begin
          j := h3 + Rounding;

          p5[0] := MakePoint(R.Left, R.Bottom);
          p5[1] := MakePoint(R.Left + Rounding*i, R.Bottom - Rounding{* 2});
          p5[2] := MakePoint(R.Left - Rounding + j, R.Top + Rounding);
          p5[3] := MakePoint(R.Left + (Rounding * 2) + j, R.Top);
          Result.AddBezier(P5[0],P5[1], P5[2], P5[3]);

          p6[0] := MakePoint(R.Right - (Rounding * 2) - j, R.Top);
          p6[1] := MakePoint(R.Right + Rounding - j, R.Top + Rounding);
          p6[2] := MakePoint(R.Right - Rounding*i, R.Bottom - Rounding{* 2});
          p6[3] := MakePoint(R.Right, R.Bottom);

          Result.AddLine(p5[3].X, R.Top, p6[0].X, R.Top);
          Result.AddBezier(P6[0],P6[1], P6[2], P6[3]);
          
          //Result.AddLine(R.Right, R.Bottom, R.Left, R.Bottom);
          Result.CloseFigure;
        end;
        tpBottom:
        begin
          j := h3 + Rounding;

          p5[0] := MakePoint(R.Left + (Rounding * 2) + j, R.Bottom);
          p5[1] := MakePoint(R.Left - Rounding + j, R.Bottom - Rounding);
          p5[2] := MakePoint(R.Left + Rounding*i, R.Top + Rounding{* 2});
          p5[3] := MakePoint(R.Left, R.Top);
          Result.AddBezier(P5[0],P5[1], P5[2], P5[3]);

          p6[0] := MakePoint(R.Right, R.Top);
          p6[1] := MakePoint(R.Right - Rounding*i, R.Top + Rounding{* 2});
          p6[2] := MakePoint(R.Right + Rounding - j, R.Bottom - Rounding);
          p6[3] := MakePoint(R.Right - (Rounding * 2) - j, R.Bottom);

          Result.AddLine(R.Left, R.top, R.Right, R.Top);
          Result.AddBezier(P6[0],P6[1], P6[2], P6[3]);
          Result.AddLine(P6[3].x, R.Bottom, P5[0].X, R.Bottom);

          Result.CloseFigure;
        end;
        tpLeft:
        begin
          j := w3 + Rounding;
          if not RotateLeftRight then
            j := h3 + Rounding;

          p5[0] := MakePoint(R.Right, R.Top);
          p5[1] := MakePoint(R.Right - Rounding{* 2}, R.Top + Rounding*i);
          p5[2] := MakePoint(R.Left + Rounding, R.Top - Rounding + j);
          p5[3] := MakePoint(R.Left, R.Top + (Rounding * 2) + j);
          Result.AddBezier(P5[0],P5[1], P5[2], P5[3]);

          p6[0] := MakePoint(R.Left, R.Bottom - (Rounding * 2) - j);
          p6[1] := MakePoint(R.Left + Rounding, R.Bottom + Rounding - j);
          p6[2] := MakePoint(R.Right - Rounding{* 2}, R.Bottom - Rounding * i);
          p6[3] := MakePoint(R.Right, R.Bottom);
          Result.AddLine(R.Left, P5[3].Y, R.Left, P6[0].Y);
          Result.AddBezier(P6[0],P6[1], P6[2], P6[3]);
          //Result.AddLine(R.Right, R.Bottom, R.Right, R.Top);
          Result.CloseFigure;
        end;
        tpRight:
        begin
          j := w3 + Rounding;
          if not RotateLeftRight then
            j := h3 + Rounding;

          p5[0] := MakePoint(R.Left, R.Top);
          p5[1] := MakePoint(R.Left + Rounding{* 2}, R.Top + Rounding*i);
          p5[2] := MakePoint(R.Right - Rounding, R.Top - Rounding + j);
          p5[3] := MakePoint(R.Right, R.Top + (Rounding * 2) + j);
          Result.AddBezier(P5[0],P5[1], P5[2], P5[3]);

          p6[0] := MakePoint(R.Right, R.Bottom - (Rounding * 2) - j);
          p6[1] := MakePoint(R.Right - Rounding, R.Bottom + Rounding - j);
          p6[2] := MakePoint(R.Left + Rounding{* 2}, R.Bottom - Rounding * i);
          p6[3] := MakePoint(R.Left, R.Bottom);
          Result.AddLine(R.Right, P5[3].Y, R.Right, P6[0].Y);
          Result.AddBezier(P6[0],P6[1], P6[2], P6[3]);

          //Result.AddLine(R.Right, R.Bottom, R.Right, R.Top);
          Result.CloseFigure;
        end;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure DrawVistaTab(Canvas: TCanvas; r: TRect; CFU, CTU, CFB, CTB, PC: TColor; GradientU,GradientB: TGDIPGradient;
   Enabled: Boolean; Shape: TAdvTabShape; Focus: Boolean; {AntiAlias: TAntiAlias; }Rounding: TTabRounding; RotateLeftRight: Boolean; Direction: TTabPosition; Progress: TTabProgress);
var
  graphics : TGPGraphics;
  TabPath, path: TGPGraphicsPath;
  pthGrBrush: TGPPathGradientBrush;
  solGrBrush: TGPSolidBrush;
  linGrBrush: TGPLinearGradientBrush;
  gppen : tgppen;
  count: Integer;
  w,h,h2,w2: Integer;
  colors : array[0..0] of TGPColor;
  BtnR: TRect;
  Rgn: TGPRegion;
begin
  BtnR := R;

  w := r.Right - r.Left;
  h := r.Bottom - r.Top;

  h2 := h div 2;
  w2 := w div 2;


  graphics := TGPGraphics.Create(Canvas.Handle);
  Tabpath := GetTabPath(R, Shape, Rounding, RotateLeftRight, Direction);

  if (Direction in [tpLeft, tpRight]) and not RotateLeftRight then
  begin
    Direction := tpTop;
    RotateLeftRight := False;
  end;

  Rgn := TGPRegion.Create(TabPath);
  graphics.SetClip(Rgn);

  if Assigned(Progress) and (Progress.Visible) then
  begin
    Progress.Draw(graphics, R, RotateLeftRight, Direction);
  end
  else
  begin
    if (CTU = clNone) and (CFB = clNone) then
    begin
      solGrBrush := TGPSolidBrush.Create(ColorToARGB(CFU));
      graphics.FillRectangle(solGrBrush, MakeRect(r.Left , r.top, w, h));
      solGrBrush.Free;
    end
    else
    case (Direction) of
      tpTop:
      begin
        // down ellips brush

        solGrBrush := TGPSolidBrush.Create(ColorToARGB(cfb));
        graphics.FillRectangle(solGrBrush, MakeRect(r.Left , r.top +  h2, w, h2));
        solGrBrush.Free;


        // Create a path that consists of a single ellipse.
        path := TGPGraphicsPath.Create;
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
          graphics.FillRectangle(pthGrBrush, r.Left + 1,r.Top + h2, w - 1, h2+1);
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

        solGrBrush := TGPSolidBrush.Create(ColorToARGB(cfu));
        graphics.FillRectangle(solGrBrush, MakeRect(r.Left , r.Top , w , h2));
        solGrBrush.Free;

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

        solGrBrush := TGPSolidBrush.Create(ColorToARGB(cfb));
        graphics.FillRectangle(solGrBrush, MakeRect(r.Left , r.top, w , h2));
        solGrBrush.Free;

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

        solGrBrush := TGPSolidBrush.Create(ColorToARGB(cfu));
        graphics.FillRectangle(solGrBrush, MakeRect(r.Left , r.top +  h2, w , h2));
        solGrBrush.Free;

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

        solGrBrush := TGPSolidBrush.Create(ColorToARGB(cfb));
        graphics.FillRectangle(solGrBrush, MakeRect(r.Left + w2, r.top, w2 , h));
        solGrBrush.Free;

        // Create a path that consists of a single ellipse.
        path := TGPGraphicsPath.Create;
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

        solGrBrush := TGPSolidBrush.Create(ColorToARGB(cfu));
        graphics.FillRectangle(solGrBrush, MakeRect(r.Left , r.Top , w2 , h));
        solGrBrush.Free;

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

        solGrBrush := TGPSolidBrush.Create(ColorToARGB(cfu));
        graphics.FillRectangle(solGrBrush, MakeRect(r.Right - w2 , r.Top , w2, h));
        solGrBrush.Free;

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

        solGrBrush := TGPSolidBrush.Create(ColorToARGB(cfb));
        graphics.FillRectangle(solGrBrush, MakeRect(r.Left , r.top, w2, h));
        solGrBrush.Free;

        // Create a path that consists of a single ellipse.
        path := TGPGraphicsPath.Create;
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
  end;

  if Rounding > 0 then
    graphics.SetSmoothingMode(SmoothingModeAntiAlias);

  if (PC <> clNone) then
  begin
    graphics.ResetClip;
    gppen := TGPPen.Create(ColorToARGB(PC), 1.6);
    graphics.DrawPath(gpPen, TabPath);
    gppen.Free;
  end;

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

  if Assigned(Rgn) then
    Rgn.Free;

  TabPath.Free;
  graphics.Free;
end;

//------------------------------------------------------------------------------

function GetInsertButtonPath(R: TRect; Shape: TAdvTabShape; Rounding: TTabRounding; RotateLeftRight: Boolean; Direction: TTabPosition): TGPGraphicsPath;
var
  p, P2: array[0..2] of TGPPoint;
  tension: double;
  w, h, rd2: Integer;
begin
  h := r.Bottom - r.Top;
  tension := 0.3;

  Result := TGPGraphicsPath.Create;
  case Shape of
    tsRectangle:
    begin
      case (Direction) of
        tpTop, tpBottom:
        begin
          R.Left := R.Left + 2;
          R.Right := R.Right - 1;
          w := r.Right - r.Left;
          rd2 := Min(6, Max(0, Rounding));

          Result.AddLine(R.Left + rd2, R.Top, R.Left + w - (rd2*2), R.Top);
          Result.AddArc(R.Left + w - (rd2*2), R.Top, rd2*2, rd2*2, 270, 90);
          Result.AddLine(R.Left + w, R.Top + rd2, R.Left + w, R.Top + h - (rd2*2));
          Result.AddArc(R.Left + w - (rd2*2), R.Top + h - (rd2*2), rd2*2, rd2*2,0,90);
          Result.AddLine(R.Left + w - (rd2*2), R.Top + h, R.Left + rd2, R.Top + h);
          Result.AddArc(R.Left, R.Top + h - (rd2*2), rd2*2, rd2*2, 90, 90);
          Result.AddLine(R.Left, R.Top + h - (rd2*2), R.Left, R.Top + rd2);
          Result.AddArc(R.Left, R.Top, rd2*2, rd2*2, 180, 90);
          Result.CloseFigure;
        end;
        tpLeft, tpRight:
        begin
          R.Top := R.Top + 2;
          R.Bottom := R.Bottom - 2;
          w := r.Right - r.Left;
          h := r.Bottom - r.Top;
          rd2 := Min(6, Max(0, Rounding));

          Result.AddLine(R.Left + rd2, R.Top, R.Left + w - (rd2*2), R.Top);
          Result.AddArc(R.Left + w - (rd2*2), R.Top, rd2*2, rd2*2, 270, 90);
          Result.AddLine(R.Left + w, R.Top + rd2, R.Left + w, R.Top + h - (rd2*2));
          Result.AddArc(R.Left + w - (rd2*2), R.Top + h - (rd2*2), rd2*2, rd2*2,0,90);
          Result.AddLine(R.Left + w - (rd2*2), R.Top + h, R.Left + rd2, R.Top + h);
          Result.AddArc(R.Left, R.Top + h - (rd2*2), rd2*2, rd2*2, 90, 90);
          Result.AddLine(R.Left, R.Top + h - (rd2*2), R.Left, R.Top + rd2);
          Result.AddArc(R.Left, R.Top, rd2*2, rd2*2, 180, 90);
          Result.CloseFigure;
        end;
      end;
    end;
    tsLeftRightRamp:
    begin
      case (Direction) of
        tpTop:
        begin
          rd2 := Min(6, Max(0, Rounding));
          p[0] := MakePoint(R.Left, R.Top);
          p[1] := MakePoint(R.Right - rd2 -2, R.Top + 1);
          p[2] := MakePoint(R.Right, R.Bottom);
          Result.AddCurve(PGPPoint(@p), 3, tension);

          p2[0] := MakePoint(R.Right, R.Bottom);
          p2[1] := MakePoint(R.Left + rd2 + 2, R.Bottom - 1);
          p2[2] := MakePoint(R.Left, R.Top);
          Result.AddCurve(PGPPoint(@p2), 3, tension);
          Result.CloseFigure;
        end;
        tpBottom:
        begin
          rd2 := Min(6, Max(0, Rounding));
          p[0] := MakePoint(R.Right, R.Top);
          p[1] := MakePoint(R.Left + rd2 +2, R.Top + 1);
          p[2] := MakePoint(R.Left, R.Bottom);
          Result.AddCurve(PGPPoint(@p), 3, tension);

          p2[0] := MakePoint(R.Left, R.Bottom);
          p2[1] := MakePoint(R.Right - rd2 - 2, R.Bottom - 1);
          p2[2] := MakePoint(R.Right, R.Top);
          Result.AddCurve(PGPPoint(@p2), 3, tension);
          Result.CloseFigure;
        end;
        tpLeft:
        begin
          rd2 := Min(6, Max(0, Rounding));
          p[0] := MakePoint(R.Left, R.Top);
          p[1] := MakePoint(R.Right - rd2, R.Top + rd2);
          p[2] := MakePoint(R.Right, R.Bottom);
          Result.AddCurve(PGPPoint(@p), 3, tension);

          p2[0] := MakePoint(R.Right, R.Bottom);
          p2[1] := MakePoint(R.Left + rd2, R.Bottom - rd2);
          p2[2] := MakePoint(R.Left, R.Top);
          Result.AddCurve(PGPPoint(@p2), 3, tension);
          Result.CloseFigure;
        end;
        tpRight:
        begin
          rd2 := Min(6, Max(0, Rounding));
          p[0] := MakePoint(R.Right, R.Top);
          p[1] := MakePoint(R.Left + rd2, R.Top + rd2);
          p[2] := MakePoint(R.Left, R.Bottom);
          Result.AddCurve(PGPPoint(@p), 3, tension);

          p2[0] := MakePoint(R.Left, R.Bottom);
          p2[1] := MakePoint(R.Right - rd2, R.Bottom - rd2);
          p2[2] := MakePoint(R.Right, R.Top);
          Result.AddCurve(PGPPoint(@p2), 3, tension);
          Result.CloseFigure;
        end;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

{ TTabSetTabSettings }

constructor TTabSetTabSettings.Create;
begin
  inherited;
  FLeftMargin := 4;
  FRightMargin := 4;
  FHeight := DEFAULT_TABHEIGHT;
  FStartMargin := 4;
  FEndMargin := 0;
  FSpacing := 4;
  FWidth := 0;
  FWordWrap := False;
  FImagePosition := ipLeft;
  FShape := tsRectangle;
  FRounding := 1;
  FAlignment := taLeftJustify;
  FGlass := False;   
end;

//------------------------------------------------------------------------------

procedure TTabSetTabSettings.Assign(Source: TPersistent);
begin
  if (Source is TTabSetTabSettings) then
  begin
    LeftMargin := (Source as TTabSetTabSettings).LeftMargin;
    RightMargin := (Source as TTabSetTabSettings).RightMargin;
    Height := (Source as TTabSetTabSettings).Height;
    StartMargin := (Source as TTabSetTabSettings).StartMargin;
    EndMargin := (Source as TTabSetTabSettings).EndMargin;
    ImagePosition := (Source as TTabSetTabSettings).ImagePosition;
    Width := (Source as TTabSetTabSettings).Width;
    WordWrap := (Source as TTabSetTabSettings).WordWrap;
    Shape := (Source as TTabSetTabSettings).Shape;
    Rounding := (Source as TTabSetTabSettings).Rounding;
    Alignment := (Source as TTabSetTabSettings).Alignment;
    Glass := (Source as TTabSetTabSettings).Glass;
  end
  else
    inherited;
end;

//------------------------------------------------------------------------------

procedure TTabSetTabSettings.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

//------------------------------------------------------------------------------

procedure TTabSetTabSettings.SetLeftMargin(const Value: Integer);
begin
  if (FLeftMargin <> Value) then
  begin
    FLeftMargin := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TTabSetTabSettings.SetRightMargin(const Value: Integer);
begin
  if (FRightMargin <> Value) then
  begin
    FRightMargin := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TTabSetTabSettings.SetHeight(const Value: Integer);
begin
  if (FHeight <> Value) then
  begin
    FHeight := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TTabSetTabSettings.SetStartMargin(const Value: Integer);
begin
  if (FStartMargin <> Value) then
  begin
    FStartMargin := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TTabSetTabSettings.SetEndMargin(const Value: Integer);
begin
  if (FEndMargin <> Value) then
  begin
    FEndMargin := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TTabSetTabSettings.SetGlass(const Value: Boolean);
begin
  if (FGlass <> Value) then
  begin
    FGlass := Value;
    if Assigned(FTabSet) then
      FTabSet.GlassChanged;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TTabSetTabSettings.SetSpacing(const Value: Integer);
begin
  if (FSpacing <> Value) then
  begin
    FSpacing := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TTabSetTabSettings.SetImagePosition(const Value: TImagePosition);
begin
  if (FImagePosition <> Value) then
  begin
    FImagePosition := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TTabSetTabSettings.SetWidth(const Value: Integer);
begin
  if (FWidth <> Value) then
  begin
    FWidth := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TTabSetTabSettings.SetWordWrap(const Value: Boolean);
begin
  if (FWordWrap <> Value) then
  begin
    FWordWrap := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TTabSetTabSettings.SetRounding(const Value: TTabRounding);
begin
  if (FRounding <> Value) and (Value <= MAX_ROUNDING) and (Value >= 0) then
  begin
    FRounding := Value;
    Changed;
  end;  
end;

//------------------------------------------------------------------------------

procedure TTabSetTabSettings.SetShape(const Value: TAdvTabShape);
begin
  if (FShape <> Value) then
  begin
    FShape := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TTabSetTabSettings.SetAlignment(const Value: TAlignment);
begin
  if (FAlignment <> Value) then
  begin
    FAlignment := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

{ TTabSetTabScroller }

constructor TTabSetTabScroller.Create;
begin
  inherited;
  FMin := 0;
  FMax := 0;
  FPosition := 0;
  FVisible := False;
end;

//------------------------------------------------------------------------------

function TTabSetTabScroller.CanGoBack: Boolean;
begin
  Result := Position > Min;
end;

//------------------------------------------------------------------------------

function TTabSetTabScroller.CanGoForward: Boolean;
begin
  Result := Position < Max;
end;

//------------------------------------------------------------------------------

procedure TTabSetTabScroller.SetMax(const Value: integer);
begin
  if Value >= FMin then FMax := Value;
end;

//------------------------------------------------------------------------------

procedure TTabSetTabScroller.SetMin(const Value: integer);
begin
  if Value <= FMax then FMin := Value;
end;

//------------------------------------------------------------------------------

procedure TTabSetTabScroller.SetPosition(const Value: integer);
begin
  FPosition := Value;
end;

//------------------------------------------------------------------------------

procedure TTabSetTabScroller.SetVisible(const Value: Boolean);
begin
  FVisible := Value;
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
  Color := clWhite;
  ColorTo := clBtnFace;
  Steps := 64;
  Direction := gdHorizontal;
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

{ TCustomTabAppearance }

constructor TCustomTabAppearance.Create;
begin
  inherited;
  FBorderColor := clBtnFace;
  FBorderColorHot := clBlue;
  FBorderColorSelectedHot := clNone;
  FBorderColorDown := clNone;
  FColor := clBtnFace;
  FColorTo := clWhite;
  FColorHot := clYellow;
  FColorHotTo := clNone;
  FColorSelectedTo := clBtnFace;
  FBorderColorDisabled := clNone;
  FBorderColorSelected := clBtnFace;
  FColorDisabled := clNone;
  FColorDisabledTo := clNone;
  FColorSelected := clWhite;
  FColorMirror := clWhite;
  FColorMirrorTo := clWhite;
  FColorMirrorHot := clNone;
  FColorMirrorHotTo := clNone;
  FGradientMirror := ggVertical;
  FGradientMirrorHot := ggVertical;
  FGradient := ggVertical;
  FGradientHot := ggVertical;
  FColorMirrorDisabledTo := clNone;
  FColorMirrorDisabled := clNone;
  FColorMirrorSelectedTo := clWhite;
  FColorMirrorSelected := clWhite;
  FGradientSelected := ggVertical;
  FGradientDisabled := ggVertical;
  FGradientMirrorSelected := ggVertical;
  FGradientMirrorDisabled := ggVertical;
  FTextColorDisabled := clWhite;
  FTextColorSelected := clBlue;
  FTextColor := clBlue;
  FTextColorHot := clBlue;
  FBackGround := TGradientBackground.Create;
  FBackGround.OnChange := OnBackGroundChanged;
  FFont := TFont.Create;
  FFont.Name := 'Tahoma';
  FFont.Size := 8;
  FFont.Style := [];
  FShadowColor := RGB(174, 199, 232);
  FHighLightColorSelected := RGB(191, 250, 255);
  //FHighLightColorSelected := RGB(248, 204, 99);
  FHighLightColorSelectedHot := RGB(255, 255, 189);
  FHighLightColorDown := RGB(208, 251, 255);
  FHighLightColorHot := RGB(237, 244, 253);
end;

//------------------------------------------------------------------------------

procedure TCustomTabAppearance.Assign(Source: TPersistent);
begin
  if (Source is TCustomTabAppearance) then
  begin
    FBorderColor := (Source as TCustomTabAppearance).BorderColor;
    FBorderColorHot := (Source as TCustomTabAppearance).BorderColorHot;
    FBorderColorSelectedHot := (Source as TCustomTabAppearance).BorderColorSelectedHot;
    FBorderColorDown := (Source as TCustomTabAppearance).BorderColorDown;
    FColor := (Source as TCustomTabAppearance).Color;
    FColorTo := (Source as TCustomTabAppearance).ColorTo;
    FColorHot := (Source as TCustomTabAppearance).ColorHot;
    FColorHotTo := (Source as TCustomTabAppearance).ColorHotTo;
    FColorSelectedTo := (Source as TCustomTabAppearance).ColorSelectedTo;
    FBorderColorDisabled := (Source as TCustomTabAppearance).BorderColorDisabled;
    FBorderColorSelected := (Source as TCustomTabAppearance).BorderColorSelected;
    FColorDisabled := (Source as TCustomTabAppearance).ColorDisabled;
    FColorDisabledTo := (Source as TCustomTabAppearance).ColorDisabledTo;
    FColorSelected := (Source as TCustomTabAppearance).ColorSelected;
    FColorMirror := (Source as TCustomTabAppearance).ColorMirror;
    FColorMirrorTo := (Source as TCustomTabAppearance).ColorMirrorTo;
    FColorMirrorHot := (Source as TCustomTabAppearance).ColorMirrorHot;
    FColorMirrorHotTo := (Source as TCustomTabAppearance).ColorMirrorHotTo;
    FGradientMirror := (Source as TCustomTabAppearance).GradientMirror;
    FGradientMirrorHot := (Source as TCustomTabAppearance).GradientMirrorHot;
    FGradient := (Source as TCustomTabAppearance).Gradient;
    FGradientHot := (Source as TCustomTabAppearance).GradientHot;
    FColorMirrorDisabledTo := (Source as TCustomTabAppearance).ColorMirrorDisabledTo;
    FColorMirrorDisabled := (Source as TCustomTabAppearance).ColorMirrorDisabled;
    FColorMirrorSelectedTo := (Source as TCustomTabAppearance).ColorMirrorSelectedTo;
    FColorMirrorSelected := (Source as TCustomTabAppearance).ColorMirrorSelected;
    FGradientSelected := (Source as TCustomTabAppearance).GradientSelected;
    FGradientDisabled := (Source as TCustomTabAppearance).GradientDisabled;
    FGradientMirrorSelected := (Source as TCustomTabAppearance).GradientMirrorSelected;
    FGradientMirrorDisabled := (Source as TCustomTabAppearance).GradientMirrorDisabled;
    FTextColorDisabled := (Source as TCustomTabAppearance).TextColorDisabled;
    FTextColorSelected := (Source as TCustomTabAppearance).TextColorSelected;
    Font.Assign((Source as TCustomTabAppearance).Font);
    TextColor := (Source as TCustomTabAppearance).TextColor;
    TextColorHot := (Source as TCustomTabAppearance).TextColorHot;
    FShadowColor := (Source as TCustomTabAppearance).ShadowColor;
    FHighLightColorSelected := (Source as TCustomTabAppearance).HighLightColorSelected;
    FHighLightColorHot := (Source as TCustomTabAppearance).HighLightColorHot;
    FHighLightColorDown := (Source as TCustomTabAppearance).HighLightColorDown;
    //FHighLightColorSelected := (Source as TCustomTabAppearance).HighLightColorSelected;
    FHighLightColorSelectedHot := (Source as TCustomTabAppearance).HighLightColorSelectedHot;
    BackGround.Assign((Source as TCustomTabAppearance).BackGround);
  end
  else
    inherited Assign(Source);
end;

//------------------------------------------------------------------------------

procedure TCustomTabAppearance.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

//------------------------------------------------------------------------------

procedure TCustomTabAppearance.ClearValues;
begin
  FBorderColor := clNone;
  FBorderColorHot := clNone;
  FColor := clNone;
  FColorTo := clNone;
  FColorHot := clNone;
  FColorHotTo := clNone;
  FColorSelectedTo := clNone;
  FBorderColorDisabled := clNone;
  FBorderColorSelected := clNone;
  FColorDisabled := clNone;
  FColorDisabledTo := clNone;
  FColorSelected := clNone;
  FColorMirror := clNone;
  FColorMirrorTo := clNone;
  FColorMirrorHot := clNone;
  FColorMirrorHotTo := clNone;
  FColorMirrorDisabledTo := clNone;
  FColorMirrorDisabled := clNone;
  FColorMirrorSelectedTo := clNone;
  FColorMirrorSelected := clNone;
  FTextColorDisabled := clNone;
  FTextColorSelected := clNone;
  FTextColor := clNone;
  FTextColorHot := clNone;
  FBorderColorSelectedHot := clNone;
  FBorderColorDown := clNone;
  FHighLightColorHot := clNone;
  FShadowColor := clNone;
  FHighLightColorDown := clNone;
  FHighLightColorSelected := clNone;
  FHighLightColorSelectedHot := clNone;
end;

//------------------------------------------------------------------------------

destructor TCustomTabAppearance.Destroy;
begin
  FBackGround.Free;
  FFont.Free;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TCustomTabAppearance.SetBackGround(const Value: TGradientBackground);
begin
  FBackGround.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TCustomTabAppearance.SetBorderColor(const Value: TColor);
begin
  if (FBorderColor <> Value) then
  begin
    FBorderColor := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomTabAppearance.SetBorderColorDisabled(const Value: TColor);
begin
  if (FBorderColorDisabled <> Value) then
  begin
    FBorderColorDisabled := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomTabAppearance.SetBorderColorSelected(const Value: TColor);
begin
  if (FBorderColorSelected <> Value) then
  begin
    FBorderColorSelected := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomTabAppearance.SetBorderColorSelectedHot(
  const Value: TColor);
begin
  if (FBorderColorSelectedHot <> Value) then
  begin
    FBorderColorSelectedHot := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomTabAppearance.SetColor(const Value: TColor);
begin
  if (FColor <> Value) then
  begin
    FColor := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomTabAppearance.SetColorDisabled(const Value: TColor);
begin
  if (FColorDisabled <> Value) then
  begin
    FColorDisabled := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomTabAppearance.SetColorDisabledTo(const Value: TColor);
begin
  if (FColorDisabledTo <> Value) then
  begin
    FColorDisabledTo := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomTabAppearance.SetColorHot(const Value: TColor);
begin
  if (FColorHot <> Value) then
  begin
    FColorHot := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomTabAppearance.SetColorHotTo(const Value: TColor);
begin
  if (FColorHotTo <> Value) then
  begin
    FColorHotTo := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomTabAppearance.SetColorMirror(const Value: TColor);
begin
  if (FColorMirror <> Value) then
  begin
    FColorMirror := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomTabAppearance.SetColorMirrorDisabled(const Value: TColor);
begin
  if (FColorMirrorDisabled <> Value) then
  begin
    FColorMirrorDisabled := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomTabAppearance.SetColorMirrorDisabledTo(
  const Value: TColor);
begin
  if (FColorMirrorDisabledTo <> Value) then
  begin
    FColorMirrorDisabledTo := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomTabAppearance.SetColorMirrorHot(const Value: TColor);
begin
  if (FColorMirrorHot <> Value) then
  begin
    FColorMirrorHot := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomTabAppearance.SetColorMirrorHotTo(const Value: TColor);
begin
  if (FColorMirrorHotTo <> Value) then
  begin
    FColorMirrorHotTo := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomTabAppearance.SetColorMirrorSelected(const Value: TColor);
begin
  if (FColorMirrorSelected <> Value) then
  begin
    FColorMirrorSelected := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomTabAppearance.SetColorMirrorSelectedTo(
  const Value: TColor);
begin
  if (FColorMirrorSelectedTo <> Value) then
  begin
    FColorMirrorSelectedTo := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomTabAppearance.SetColorMirrorTo(const Value: TColor);
begin
  if (FColorMirrorTo <> Value) then
  begin
    FColorMirrorTo := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomTabAppearance.SetColorSelected(const Value: TColor);
begin
  if (FColorSelected <> Value) then
  begin
    FColorSelected := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomTabAppearance.SetColorSelectedTo(const Value: TColor);
begin
  if (FColorSelectedTo <> Value) then
  begin
    FColorSelectedTo := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomTabAppearance.SetColorTo(const Value: TColor);
begin
  if (FColorTo <> Value) then
  begin
    FColorTo := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomTabAppearance.SetGradient(const Value: TGDIPGradient);
begin
  if (FGradient <> Value) then
  begin
    FGradient := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomTabAppearance.SetGradientDisabled(
  const Value: TGDIPGradient);
begin
  if (FGradientDisabled <> Value) then
  begin
    FGradientDisabled := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomTabAppearance.SetGradientHot(const Value: TGDIPGradient);
begin
  if (FGradientHot <> Value) then
  begin
    FGradientHot := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomTabAppearance.SetGradientMirror(
  const Value: TGDIPGradient);
begin
  if (FGradientMirror <> Value) then
  begin
    FGradientMirror := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomTabAppearance.SetGradientMirrorDisabled(
  const Value: TGDIPGradient);
begin
  if (FGradientMirrorDisabled <> Value) then
  begin
    FGradientMirrorDisabled := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomTabAppearance.SetGradientMirrorHot(
  const Value: TGDIPGradient);
begin
  if (FGradientMirrorHot <> Value) then
  begin
    FGradientMirrorHot := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomTabAppearance.SetGradientMirrorSelected(
  const Value: TGDIPGradient);
begin
  if (FGradientMirrorSelected <> Value) then
  begin
    FGradientMirrorSelected := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomTabAppearance.SetGradientSelected(
  const Value: TGDIPGradient);
begin
  if (FGradientSelected <> Value) then
  begin
    FGradientSelected := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomTabAppearance.SetTextColor(const Value: TColor);
begin
  if (FTextColor <> Value) then
  begin
    FTextColor := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomTabAppearance.SetTextColorDisabled(const Value: TColor);
begin
  if (FTextColorDisabled <> Value) then
  begin
    FTextColorDisabled := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomTabAppearance.SetTextColorHot(const Value: TColor);
begin
  if (FTextColorHot <> Value) then
  begin
    FTextColorHot := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomTabAppearance.SetTextColorSelected(const Value: TColor);
begin
  if (FTextColorSelected <> Value) then
  begin
    FTextColorSelected := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomTabAppearance.OnBackGroundChanged(Sender: TObject);
begin
  Changed;
end;

//------------------------------------------------------------------------------

procedure TCustomTabAppearance.SetBorderColorDown(const Value: TColor);
begin
  if (FBorderColorDown <> Value) then
  begin
    FBorderColorDown := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomTabAppearance.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
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

{ TCustomAdvOfficeTabSetStyler }

procedure TCustomAdvOfficeTabSetStyler.AddControl(AControl: TCustomControl);
begin
  FControlList.Add(AControl);
end;

//------------------------------------------------------------------------------

procedure TCustomAdvOfficeTabSetStyler.Assign(Source: TPersistent);
begin
  if Source is TCustomAdvOfficeTabSetStyler then
  begin
    TabAppearance.Assign((Source as TCustomAdvOfficeTabSetStyler).TabAppearance);
    RoundEdges := (Source as TCustomAdvOfficeTabSetStyler).RoundEdges;
    TabRounding := (Source as TCustomAdvOfficeTabSetStyler).TabRounding;
    ButtonBorderColor := (Source as TCustomAdvOfficeTabSetStyler).ButtonBorderColor;
  end
  else
    inherited Assign(Source);
end;

//------------------------------------------------------------------------------

constructor TCustomAdvOfficeTabSetStyler.Create(AOwner: TComponent);
begin
  inherited;
  FControlList := TDbgList.Create;
  FRoundEdges := True;
  FBlendFactor := 50;
  TabRounding := 1;

  FTabAppearance := TTabAppearance.Create;
  FTabAppearance.OnChange := OnTabAppearanceChanged;
  FGlowButtonAppearance := TGlowButtonAppearance.Create;
  with FGlowButtonAppearance do
  begin
    Color := $EEDBC8;
    ColorTo := $F6DDC9;
    ColorMirror := $EDD4C0;
    ColorMirrorTo := $F7E1D0;
    BorderColor := $E0B99B;
    Gradient := ggVertical;
    GradientMirror := ggVertical;

    ColorHot := $EBFDFF;
    ColorHotTo := $ACECFF;
    ColorMirrorHot := $59DAFF;
    ColorMirrorHotTo := $A4E9FF;
    BorderColorHot := $99CEDB;
    GradientHot := ggVertical;
    GradientMirrorHot := ggVertical;

    ColorDown := $76AFF1;
    ColorDownTo := $4190F3;
    ColorMirrorDown := $0E72F1;
    ColorMirrorDownTo := $4C9FFD;
    BorderColorDown := $45667B;
    GradientDown := ggVertical;
    GradientMirrorDown := ggVertical;

    ColorChecked := $B5DBFB;
    ColorCheckedTo := $78C7FE;
    ColorMirrorChecked := $9FEBFD;
    ColorMirrorCheckedTo := $56B4FE;
    BorderColorChecked := $45667B;
    GradientChecked := ggVertical;
    GradientMirrorChecked := ggVertical;
  end;
  FGlowButtonAppearance.OnChange := OnGlowButtonAppearanceChanged;

  FButtonBorderColor := clNone;
end;

//------------------------------------------------------------------------------

destructor TCustomAdvOfficeTabSetStyler.Destroy;
begin
  FControlList.Free;
  FTabAppearance.Free;
  FGlowButtonAppearance.Free;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TCustomAdvOfficeTabSetStyler.InitColorTones;
begin
  //
end;

//------------------------------------------------------------------------------

procedure TCustomAdvOfficeTabSetStyler.Change(PropID: integer);
var
  i: integer;
begin
  for i := 0 to FControlList.Count - 1 do
  begin
    try
      if TCustomControl(FControlList[i]).Name <> '' then
        if (TCustomControl(FControlList[i]) is TAdvCustomOfficeTabSet) then
          TAdvCustomOfficeTabSet(FControlList[i]).UpdateMe(PropID);
    except

    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomAdvOfficeTabSetStyler.SetColorTones(ATones: TColorTones);
var
  i: integer;
begin
  for i := 0 to FControlList.Count - 1 do
  begin
    try
      if TCustomControl(FControlList[i]).Name <> '' then
        if (TCustomControl(FControlList[i]) is TAdvCustomOfficeTabSet) then
          TAdvCustomOfficeTabSet(FControlList[i]).SetColorTones(ATones);
    except

    end;
  end;
end;


//------------------------------------------------------------------------------

procedure TCustomAdvOfficeTabSetStyler.Notification(AComponent: TComponent;
  Operation: TOperation);
var
  i: integer;
begin
  inherited;
  if not (csDestroying in ComponentState) and (Operation = opRemove) then
  begin
    i := FControlList.IndexOf(AComponent);
    if i >= 0 then
      FControlList.Remove(AComponent);
  end;

end;

//------------------------------------------------------------------------------

procedure TCustomAdvOfficeTabSetStyler.RemoveControl(AControl: TCustomControl);
var
  i: integer;
begin
  i := FControlList.IndexOf(AControl);
  if i >= 0 then
    FControlList.Delete(i);
end;

//------------------------------------------------------------------------------

procedure TCustomAdvOfficeTabSetStyler.OnTabAppearanceChanged(Sender: TObject);
begin
  Change(1);
end;

//------------------------------------------------------------------------------

procedure TCustomAdvOfficeTabSetStyler.OnGlowButtonAppearanceChanged(Sender: TObject);
begin
  Change(4);
end;

//------------------------------------------------------------------------------

procedure TCustomAdvOfficeTabSetStyler.Loaded;
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TCustomAdvOfficeTabSetStyler.SetRoundEdges(const Value: boolean);
begin
  if (FRoundEdges <> Value) then
  begin
    FRoundEdges := Value;
    Change(3);
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomAdvOfficeTabSetStyler.SetTabRounding(const Value: TTabRounding);
begin
  if (Value <> FTabRounding) then
  begin
    FTabRounding := Value;
    Change(3);
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomAdvOfficeTabSetStyler.SetTabAppearance(
  const Value: TTabAppearance);
begin
  FTabAppearance.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TCustomAdvOfficeTabSetStyler.SetGlowButtonAppearance(
  const Value: TGlowButtonAppearance);
begin
  FGlowButtonAppearance.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TCustomAdvOfficeTabSetStyler.SetAutoThemeAdapt(const Value: boolean);
begin
  FAutoThemeAdapt := Value;
end;

procedure TCustomAdvOfficeTabSetStyler.SetButtonBorderColor(
  const Value: TColor);
begin
  if (FButtonBorderColor <> Value) then
  begin
    FButtonBorderColor := Value;
    Change(1);
  end;
end;

//------------------------------------------------------------------------------

{ TTabProgress }

procedure TTabProgress.Assign(Source: TPersistent);
begin
  if (Source is TTabProgress) then
  begin
    FColor := TTabProgress(Source).Color;
    FColorTo := TTabProgress(Source).ColorTo;
    FCompleteColor := TTabProgress(Source).CompleteColor;
    FCompleteColorTo := TTabProgress(Source).CompleteColorTo;
    FMax := TTabProgress(Source).Max;
    FMin := TTabProgress(Source).Min;
    FVisible := TTabProgress(Source).Visible;
    FPosition := TTabProgress(Source).Position;
    Changed;
  end
  else
    inherited Assign(Source);
end;

//------------------------------------------------------------------------------

procedure TTabProgress.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

//------------------------------------------------------------------------------

constructor TTabProgress.Create;
begin
  inherited;
  FColor := clWhite;
  FColorTo := clSilver;
  FCompleteColor := clWhite;
  FCompleteColorTo := clRed;
  FMax := 100;
  FMin := 0;
  FVisible := false;
  FPosition := 50;
end;

//------------------------------------------------------------------------------

procedure TTabProgress.Draw(g: TGPGraphics; R: TRect; RotateTabLeftRight: Boolean;
  Direction: TTabPosition);
var
  w, cs: Integer;
  GPBrsh: TGPBrush;
  LGM: LinearGradientMode;
  gpRF: TGPRectF;
  //gpPen: TGPPen;
  Clr, ClrTo, cmClr, cmClrTo: TColor;
begin
  if not Assigned(g) then
    Exit;

  if (Direction = tpRight) then
  begin
    R.Top := R.Top + 2;
    R.Right := R.Right - 1;
  end;

  Clr := Color;
  ClrTo := ColorTo;
  cmClr := CompleteColor;
  cmClrTo := CompleteColorTo;
  if (Direction in [tpTop, tpBottom]) or not RotateTabLeftRight then
  begin
    w := R.Right - R.Left;
    LGM := LinearGradientModeVertical;
  end
  else if Direction = tpLeft then
  begin
    w := R.Bottom - R.Top;
    LGM := LinearGradientModeHorizontal;
  end
  else
  begin
    w := R.Bottom - R.Top;
    LGM := LinearGradientModeHorizontal;
    Clr := ColorTo;
    ClrTo := Color;
    cmClr := CompleteColorTo;
    cmClrTo := CompleteColor;
  end;


  cs := Round((Position / Max) * w);

  //--- Color, Color
  gpRF := MakeRect(R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top);
  if (ColorTo <> clNone) then
    GPBrsh := TGPLinearGradientBrush.Create(gpRF, ColorToARGB(Clr),ColorToARGB(ClrTo), LGM)
  else
    GPBrsh := TGPSolidBrush.Create(ColorToARGB(Color));

  g.FillRectangle(GPBrsh, gpRF);
  GPBrsh.Free;

  //--- CompleteColor, CompleteColorTo
  if (Direction in [tpTop, tpBottom]) or (not RotateTabLeftRight) then
    gpRF := MakeRect(R.Left, R.Top, cs, R.Bottom - R.Top)
  else if (Direction = tpLeft) then
    gpRF := MakeRect(R.Left, R.Bottom - cs, R.Right - R.Left, cs)
  else
    gpRF := MakeRect(R.Left, R.Top, R.Right - R.Left, cs);

  if (CompleteColorTo <> clNone) then
    GPBrsh := TGPLinearGradientBrush.Create(gpRF, ColorToARGB(cmClr),ColorToARGB(cmClrTo), LGM)
  else
    GPBrsh := TGPSolidBrush.Create(ColorToARGB(cmClr));

  g.FillRectangle(GPBrsh, gpRF);
  GPBrsh.Free;

  {
  gpPen := TGPPen.Create(ColorToARGB(BorderColor), 1);
  g.DrawRectangle(gpPen, R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top);
  gpPen.Free;
  }
end;

//------------------------------------------------------------------------------

procedure TTabProgress.SetCompleteColor(const Value: TColor);
begin
  if FCompleteColor <> Value then
  begin
    FCompleteColor := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TTabProgress.SetCompleteColorTo(const Value: TColor);
begin
  if FCompleteColorTo <> Value then
  begin
    FCompleteColorTo := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TTabProgress.SetMax(const Value: Integer);
begin
  if (FMax <> Value) then
  begin
    FMax := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TTabProgress.SetMin(const Value: Integer);
begin
  if (FMin <> Value) then
  begin
    FMin := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TTabProgress.SetPosition(const Value: Integer);
begin
  if (FPosition <> Value) then
  begin
    FPosition := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TTabProgress.SetColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TTabProgress.SetColorTo(const Value: TColor);
begin
  if FColorTo <> Value then
  begin
    FColorTo := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TTabProgress.SetVisible(const Value: Boolean);
begin
  if (FVisible <> Value) then
  begin
    FVisible := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

{ TArrowWindow }

procedure TArrowWindow.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := WS_POPUP; // or WS_BORDER;
  end;
end;

//------------------------------------------------------------------------------

procedure TArrowWindow.Loaded;
begin
  inherited;
end;

//------------------------------------------------------------------------------

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
  hrgn := CreatePolygonRgn(arrow,7{7},WINDING);
  SetWindowRgn(Handle,hrgn,True);
end;

//------------------------------------------------------------------------------

procedure TArrowWindow.Paint;
begin
//  inherited;  // remove, is not working in Windows XP
  Canvas.Brush.Color := Color;
  Canvas.Pen.Color := Color;
  Canvas.Rectangle(ClientRect.Left,ClientRect.Top,ClientRect.Right,ClientRect.Bottom);

  //Canvas.Pen.Color := FBrColor;
  //Canvas.Brush.Style := bsClear;
  //Canvas.Polygon(Arrow);
  //Canvas.MoveTo(Arrow[4].X, Arrow[4].Y-1);
  //Canvas.LineTo(Arrow[3].X, Arrow[3].Y);
end;

//------------------------------------------------------------------------------

constructor TArrowWindow.Init(AOwner: TComponent; Direction:TArrowDirection);
begin
  Dir := Direction;
  inherited Create(aOwner);
  Color := clBlue;// clSilver;
  FBrColor := clGray;
  Parent := TWinControl(AOwner);
  Visible := False;
  ControlStyle := [csOpaque];
  if Direction in [arrUp, arrDown] then
  begin
    Height := 9;
    Width := 9; //9
  end
  else
  begin
    Height := 9;
    Width := 9;
  end;
end;

//------------------------------------------------------------------------------

{ TOfficeTabCollectionItem }

constructor TOfficeTabCollectionItem.Create(Collection: TCollection);
begin
  inherited;

  FDestroying := false;

  FName := ClassName + inttostr(Collection.Count);
  FCaption := 'TabItem';
  FVisible := true;
  FEnabled := true;
  FImageIndex := -1;
  FCloseButton := nil;
  FCloseButtonChrome := nil;

  FProgress := TTabProgress.Create;
  FProgress.OnChange := OnProgressChanged;

  FCaption := FCaption + inttostr(Index+1);
  DisplayName:= FCaption;

  FTimer := nil;

  FIPicture := TGDIPPicture.Create;
  FIPicture.OnChange := PictureChanged;

  FIDisabledPicture := TGDIPPicture.Create;
  FIDisabledPicture.OnChange := PictureChanged;

  FOfficeHint := TAdvHintInfo.Create;
  
  FChildForm := nil;
  FShowClose := True;

  FTabAppearance := TTabAppearance.Create;
  FTabAppearance.OnChange := OnTabAppearanceChanged;
  FUseTabAppearance := false;

  FShowCheckBox := false;
  FChecked := false;

  FGlow := False;
  FGlowColor := clRed;

  if Assigned(TOfficeTabCollection(Collection).AdvOfficeTabSet) then
  begin
    if (TOfficeTabCollection(Collection).AdvOfficeTabSet.ActiveTabIndex < 0) and (Index = 0) then
      TOfficeTabCollection(Collection).AdvOfficeTabSet.FActiveTabIndex := Index;

    TOfficeTabCollection(Collection).AdvOfficeTabSet.UpdateTabScroller;
    if (csDesigning in TOfficeTabCollection(Collection).AdvOfficeTabSet.ComponentState) and Assigned(TOfficeTabCollection(Collection).AdvOfficeTabSet.FCurrentOfficeTabSetStyler) then
    begin
      if not UseTabAppearance then
      begin
        if Assigned(TOfficeTabCollection(Collection).AdvOfficeTabSet.FTabAppearance) and TOfficeTabCollection(Collection).AdvOfficeTabSet.FItones then
          TabAppearance.Assign(TOfficeTabCollection(Collection).AdvOfficeTabSet.FTabAppearance)
        else
          TabAppearance.Assign(TOfficeTabCollection(Collection).AdvOfficeTabSet.FCurrentOfficeTabSetStyler.TabAppearance);
      end;
    end;

    TOfficeTabCollection(Collection).AdvOfficeTabSet.UpdateMultiLineTabs;
    TOfficeTabCollection(Collection).AdvOfficeTabSet.AdjustHeight;
    TOfficeTabCollection(Collection).AdvOfficeTabSet.Invalidate;
  end;

  FTabIndex := Collection.Count - 1;
end;

//------------------------------------------------------------------------------
procedure TOfficeTabCollectionItem.Assign(Source: TPersistent);
begin
  if (Source is TOfficeTabCollectionItem) then
  begin
    FCaption := (Source as TOfficeTabCollectionItem).Caption;
    FVisible := (Source as TOfficeTabCollectionItem).Visible;
    FEnabled := (Source as TOfficeTabCollectionItem).Enabled;
    FOfficeHint.Assign((Source as TOfficeTabCollectionItem).OfficeHint);
    FIPicture.Assign((Source as TOfficeTabCollectionItem).FIPicture);
    FIDisabledPicture.Assign((Source as TOfficeTabCollectionItem).FIDisabledPicture);
    FImageIndex := (Source as TOfficeTabCollectionItem).ImageIndex;
    FTag := (Source as TOfficeTabCollectionItem).Tag;
    ShowClose := (Source as TOfficeTabCollectionItem).ShowClose;
    FShowCheckBox := (Source as TOfficeTabCollectionItem).ShowCheckBox;
    FChecked := (Source as TOfficeTabCollectionItem).Checked;
    FProgress.Assign((Source as TOfficeTabCollectionItem).Progress);
    FGlowColor := (Source as TOfficeTabCollectionItem).GlowColor;
    Glow := (Source as TOfficeTabCollectionItem).Glow;
    Locked := (Source as TOfficeTabCollectionItem).Locked;
  end
  else
    inherited Assign(Source);
end;


//------------------------------------------------------------------------------
destructor TOfficeTabCollectionItem.Destroy;
var
  i: Integer;
begin
  FDestroying := true;
  FIPicture.Free;
  FIPicture := nil;
  FIDisabledPicture.Free;
  FOfficeHint.Free;

  if Assigned(FTimer) then
    FreeAndNil(FTimer);

  FTabAppearance.Free;
  FProgress.Free;

  if Assigned(FParentCollection) and Assigned(FParentCollection.AdvOfficeTabSet) and
    Assigned(FParentCollection.AdvOfficeTabSet.FFloatingTabs) then
  begin
    i := FParentCollection.AdvOfficeTabSet.FFloatingTabs.IndexOf(Self);
    if (i >= 0) then
      FParentCollection.AdvOfficeTabSet.FFloatingTabs.Delete(i);
  end;

  if Assigned(FCloseButton) then
  begin
    TOfficeTabCollection(Collection).AdvOfficeTabSet.RemoveControl(FCloseButton);
    //FCloseButton.Free;
    //FCloseButton := nil;
  end;

  if Assigned(FCloseButtonChrome) then
  begin
    TOfficeTabCollection(Collection).AdvOfficeTabSet.RemoveControl(FCloseButtonChrome);
  end;

  if Assigned(TOfficeTabCollection(Collection).AdvOfficeTabSet) then
  begin
    TOfficeTabCollection(Collection).AdvOfficeTabSet.UpdateMultiLineTabs;
    TOfficeTabCollection(Collection).AdvOfficeTabSet.AdjustHeight;
  end;

  inherited;
end;

//------------------------------------------------------------------------------

procedure TOfficeTabCollectionItem.SetCaption(const Value: string);
begin
  FCaption := Value;
  if Assigned(TOfficeTabCollection(Collection).AdvOfficeTabSet) then
  begin
    TOfficeTabCollection(Collection).AdvOfficeTabSet.InitializeAndUpdateButtons;
    TOfficeTabCollection(Collection).AdvOfficeTabSet.UpdateTabScroller;
    TOfficeTabCollection(Collection).AdvOfficeTabSet.UpdateMultiLineTabs;
  end;
  Refresh;
end;

//------------------------------------------------------------------------------

procedure TOfficeTabCollectionItem.SetChecked(const Value: Boolean);
begin
  if FChecked <> Value then
  begin
    FChecked := Value;
    if Assigned(TOfficeTabCollection(Collection).AdvOfficeTabSet) then
    begin
      TOfficeTabCollection(Collection).AdvOfficeTabSet.InitializeAndUpdateButtons;
      TOfficeTabCollection(Collection).AdvOfficeTabSet.UpdateTabScroller;
      TOfficeTabCollection(Collection).AdvOfficeTabSet.UpdateMultiLineTabs;
    end;
    Refresh;
  end;
end;

//------------------------------------------------------------------------------

procedure TOfficeTabCollectionItem.SetImageIndex(const Value: Integer);
begin
  FImageIndex := Value;
  if Assigned(TOfficeTabCollection(Collection).AdvOfficeTabSet) then
  begin
    TOfficeTabCollection(Collection).AdvOfficeTabSet.InitializeAndUpdateButtons;
    TOfficeTabCollection(Collection).AdvOfficeTabSet.UpdateTabScroller;
    TOfficeTabCollection(Collection).AdvOfficeTabSet.UpdateMultiLineTabs;
  end;
  Refresh;
end;

//------------------------------------------------------------------------------
function TOfficeTabCollectionItem.GetDisplayName: string;
begin
  Result := 'Tab '+IntToStr(Index)+' : '+ Caption;
end;

//------------------------------------------------------------------------------

procedure TOfficeTabCollectionItem.SetEnabled(const Value: boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    Refresh;
  end;
end;

//------------------------------------------------------------------------------

procedure TOfficeTabCollectionItem.SetGlow(const Value: Boolean);
begin
  if (FGlow <> Value) then
  begin
    FGlow := Value;
    if Assigned(TOfficeTabCollection(Collection).AdvOfficeTabSet) then
      TOfficeTabCollection(Collection).AdvOfficeTabSet.UpdateGlowTimer;
    Refresh;
  end;
end;

//------------------------------------------------------------------------------

procedure TOfficeTabCollectionItem.SetGlowColor(const Value: TColor);
begin
  if (FGlowColor <> Value) then
  begin
    FGlowColor := Value;
    Refresh;
  end;
end;

//------------------------------------------------------------------------------

procedure TOfficeTabCollectionItem.SetVisible(const Value: Boolean);
var
  i: Integer;
begin
  if Value <> FVisible then
  begin
    if Assigned(TOfficeTabCollection(Collection).AdvOfficeTabSet) then
    begin
      if not Value then
        TOfficeTabCollection(Collection).AdvOfficeTabSet.FClosedTabList.AddObject(Self.Caption, Self)
      else
      begin
        i := TOfficeTabCollection(Collection).AdvOfficeTabSet.FClosedTabList.IndexOfObject(Self);
        if (i >= 0) then
          TOfficeTabCollection(Collection).AdvOfficeTabSet.FClosedTabList.Delete(i);
      end;
      TOfficeTabCollection(Collection).AdvOfficeTabSet.UpdateClosedListButton;
    end;

    FVisible := Value;

    if Assigned(TOfficeTabCollection(Collection).AdvOfficeTabSet) then
    begin
      TOfficeTabCollection(Collection).AdvOfficeTabSet.InitializeAndUpdateButtons;
      TOfficeTabCollection(Collection).AdvOfficeTabSet.UpdateTabScroller;
      TOfficeTabCollection(Collection).AdvOfficeTabSet.UpdateMultiLineTabs;
    end;
    Refresh;
  end;
end;

//------------------------------------------------------------------------------

procedure TOfficeTabCollectionItem.SetIndex(Value: Integer);
begin
  Inherited SetIndex(Value);
end;

//------------------------------------------------------------------------------

procedure TOfficeTabCollectionItem.SetDisabledPicture(
  const Value: TGDIPPicture);
begin
  FIDisabledPicture.Assign(Value);
  Refresh;
end;

//------------------------------------------------------------------------------

procedure TOfficeTabCollectionItem.SetPicture(const Value: TGDIPPicture);
begin
  FIPicture.Assign(Value);
  Refresh;
end;

//------------------------------------------------------------------------------

procedure TOfficeTabCollectionItem.SetProgress(
  const Value: TTabProgress);
begin
  FProgress.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TOfficeTabCollectionItem.SetLocked(const Value: Boolean);
begin
  FLocked := Value;
end;

//------------------------------------------------------------------------------

procedure TOfficeTabCollectionItem.TimerProc(Sender: TObject);
var
  P: TPoint;
begin
  case FGlowState of
    gsHover:
    begin
      FStepHover := FStepHover + FTimeInc;
      if ((FStepHover > 100) and (FTimeInc > 0))
         or ((FStepHover < 0) and (FTimeInc < 0)) then
      begin
        if (FStepHover > 100) and (FTimeInc > 0) and Assigned(TOfficeTabCollection(Collection).AdvOfficeTabSet) then
        begin
          FStepHover := 120;
          GetCursorPos(P);
          P := TOfficeTabCollection(Collection).AdvOfficeTabSet.ScreenToClient(P);
          if not PtInRect(TOfficeTabCollection(Collection).AdvOfficeTabSet.GetTabRect(Self.Index), P) then
          begin
            FTimeInc := -20;
            FGlowState := gsHover;
            TOfficeTabCollection(Collection).AdvOfficeTabSet.FHotTabIndex := -1;
            Exit;
          end;
        end
        else if ((FStepHover < 0) and (FTimeInc < 0)) then
        begin
          FreeAndNil(FTimer);
          FGlowState := gsNone;
          if Assigned(TOfficeTabCollection(Collection).AdvOfficeTabSet) then
            TOfficeTabCollection(Collection).AdvOfficeTabSet.InvalidateTab(Index);
        end;

        FStepPush := 0;
        if (FStepHover > 100) then
          FStepHover := 120;
        if (FStepHover < 0) then
          FStepHover := -20;
      end
      else if Assigned(TOfficeTabCollection(Collection).AdvOfficeTabSet) then
        TOfficeTabCollection(Collection).AdvOfficeTabSet.InvalidateTab(Index);
    end;
    gsPush:
    begin
      FStepPush := FStepPush + FTimeInc;
      if ((FStepPush > 100) and (FTimeInc > 0))
         or ((FStepPush < 0) and (FTimeInc < 0)) then
      begin
        FreeAndNil(FTimer);
        FGlowState := gsNone;
        FStepPush := 0;
      end
      else if Assigned(TOfficeTabCollection(Collection).AdvOfficeTabSet) then
        TOfficeTabCollection(Collection).AdvOfficeTabSet.InvalidateTab(Index);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TOfficeTabCollectionItem.PictureChanged(Sender: TObject);
begin
  Refresh;
end;

//------------------------------------------------------------------------------

procedure TOfficeTabCollectionItem.Refresh;
begin
  if Assigned(TOfficeTabCollection(Collection).AdvOfficeTabSet) then
    TOfficeTabCollection(Collection).AdvOfficeTabSet.Invalidate;
end;

//------------------------------------------------------------------------------

procedure TOfficeTabCollectionItem.SetOfficeHint(
  const Value: TAdvHintInfo);
begin
  FOfficeHint.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TOfficeTabCollectionItem.SetShowCheckBox(const Value: Boolean);
begin
  if FShowCheckBox <> Value then
  begin
    FShowCheckBox := Value;
    if Assigned(TOfficeTabCollection(Collection).AdvOfficeTabSet) then
    begin
      TOfficeTabCollection(Collection).AdvOfficeTabSet.InitializeAndUpdateButtons;
      TOfficeTabCollection(Collection).AdvOfficeTabSet.UpdateTabScroller;
      TOfficeTabCollection(Collection).AdvOfficeTabSet.UpdateMultiLineTabs;
    end;
    Refresh;
  end;
end;

procedure TOfficeTabCollectionItem.SetShowClose(const Value: Boolean);
begin
  if (FShowClose <> Value) then
  begin
    FShowClose := Value;
    if not FshowClose then
    begin
      if Assigned(FCloseButton) then
      begin
        FCloseButton.Free;
        FCloseButton := nil;
      end;

      if Assigned(FCloseButtonChrome) then
      begin
        FCloseButtonChrome.Free;
        FCloseButtonChrome := nil;
      end;
    end;

    if Assigned(TOfficeTabCollection(Collection).AdvOfficeTabSet) then
    begin
      TOfficeTabCollection(Collection).AdvOfficeTabSet.InitializeAndUpdateButtons;
      TOfficeTabCollection(Collection).AdvOfficeTabSet.UpdateTabScroller;
      TOfficeTabCollection(Collection).AdvOfficeTabSet.UpdateMultiLineTabs;
    end;
    Refresh;
  end;
end;

//------------------------------------------------------------------------------

procedure TOfficeTabCollectionItem.SetTabAppearance(
  const Value: TTabAppearance);
begin
  FTabAppearance.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TOfficeTabCollectionItem.SetUseTabAppearance(
  const Value: Boolean);
begin
  if (FUseTabAppearance <> Value) then
  begin
    FUseTabAppearance := Value;
    Refresh;
  end;
end;

//------------------------------------------------------------------------------

procedure TOfficeTabCollectionItem.OnProgressChanged(Sender: TObject);
begin
  Refresh;
end;

//------------------------------------------------------------------------------

procedure TOfficeTabCollectionItem.OnTabAppearanceChanged(Sender: TObject);
begin
  Refresh;
end;

//------------------------------------------------------------------------------

{ TOfficeTabCollection }

function TOfficeTabCollection.Add: TOfficeTabCollectionItem;
begin
  Result := TOfficeTabCollectionItem(inherited Add);
end;

//------------------------------------------------------------------------------

constructor TOfficeTabCollection.Create(AOwner: TAdvCustomOfficeTabSet);
begin
  inherited Create(TOfficeTabCollectionItem);
  FOwner := AOwner;
end;

//------------------------------------------------------------------------------

procedure TOfficeTabCollection.Clear;
begin
  AdvOfficeTabSet.BeginUpdate;

  while (Count>0) do
    Delete(0);

  AdvOfficeTabSet.EndUpdate;

  AdvOfficeTabSet.FActiveTabIndex := -1;
  AdvOfficeTabSet.UpdateTabScroller;
  AdvOfficeTabSet.UpdateMultiLineTabs;
  AdvOfficeTabSet.Invalidate;
end;

//------------------------------------------------------------------------------

procedure TOfficeTabCollection.Delete(Index: Integer);
var
  ActItem: TOfficeTabCollectionItem;
begin
  if not Assigned(AdvOfficeTabSet) then
    Exit;
    
  if (Index = AdvOfficeTabSet.ActiveTabIndex) then
  begin
    //AdvOfficeTabSet.SelectNextTab(True);
    AdvOfficeTabSet.SelectNextSequentialTab;
  end;

  ActItem := nil;
  if (AdvOfficeTabSet.ActiveTabIndex >= 0) then
  begin
    ActItem := Items[AdvOfficeTabSet.ActiveTabIndex];
    if (ActItem = Items[Index])then
      ActItem := nil;
  end;

  inherited Delete(Index);
  
  if (ActItem <> nil) then
  begin
    AdvOfficeTabSet.FActiveTabIndex := -1;
    AdvOfficeTabSet.ActiveTabIndex := ActItem.Index;
  end
  else
  begin
    AdvOfficeTabSet.ActiveTabIndex := -1;
  end;

  AdvOfficeTabSet.UpdateTabScroller;
  AdvOfficeTabSet.UpdateMultiLineTabs;
  AdvOfficeTabSet.Invalidate;
end;

//------------------------------------------------------------------------------

function TOfficeTabCollection.GetItem(Index: Integer): TOfficeTabCollectionItem;
begin
  Result := TOfficeTabCollectionItem(inherited Items[Index]);
end;

//------------------------------------------------------------------------------
function TOfficeTabCollection.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

//------------------------------------------------------------------------------

function TOfficeTabCollection.Insert(Index: Integer): TOfficeTabCollectionItem;
begin
  Result := TOfficeTabCollectionItem(inherited Insert(Index));
end;

//------------------------------------------------------------------------------

procedure TOfficeTabCollection.Move(CurIndex, NewIndex: integer);
begin
  if (CurIndex >= 0) and (CurIndex < Count) and (NewIndex >= 0) and (NewIndex < Count) and (CurIndex <> NewIndex) then
  begin
    Items[CurIndex].SetIndex(NewIndex);
    if Assigned(AdvOfficeTabSet) then
    begin
      AdvOfficeTabSet.UpdateTabScroller;
      AdvOfficeTabSet.UpdateMultiLineTabs;
      AdvOfficeTabSet.Invalidate;
    end;
  end;
end;

{$IFDEF DELPHI7_LVL}
procedure TOfficeTabCollection.Notify(Item: TCollectionItem;
  Action: TCollectionNotification);
var
  I: Integer;
begin
  inherited;
  if Assigned(Item) and (Action = cnAdded) and Assigned(AdvOfficeTabSet) and Assigned(AdvOfficeTabSet.FFloatingTabs) then
  begin
    I := AdvOfficeTabSet.FFloatingTabs.IndexOf(Item);
    if (I >= 0) then
      AdvOfficeTabSet.FFloatingTabs.Delete(I);
  end;
end;
{$ENDIF}

//------------------------------------------------------------------------------

procedure TOfficeTabCollection.SetItem(Index: Integer; const Value: TOfficeTabCollectionItem);
begin
  inherited Items[Index] := Value;
end;

//------------------------------------------------------------------------------

function IsComCtl6: Boolean;
var
  i: Integer;
begin
  i := GetFileVersion('COMCTL32.DLL');
  i := (i shr 16) and $FF;

  Result := (i > 5);
end;

//------------------------------------------------------------------------------

function AeroIsEnabled: boolean;
var
  enabled: bool;
begin
  Result := False;
  //if (DWMlibrary = 0) then
  begin
    if (@DwmIsCompositionEnabled <> nil) then
    begin
      DwmIsCompositionEnabled(enabled);
      Result := enabled;
    end;
  end;
end;

//------------------------------------------------------------------------------

{ TAdvCustomOfficeTabSet }

constructor TAdvCustomOfficeTabSet.Create(AOwner: TComponent);
var
  ts: TAdvOfficeTabSetOfficeStyler;
  i: integer;
begin
  FIsAeroVista := IsComCtl6 and IsVista {and ThemeServices.ThemesEnabled} and AeroIsEnabled and not (csDesigning in ComponentState);

{$IFNDEF TMS_DWM}
  FIsAeroVista := False;
{$ENDIF}

  inherited;

  ControlStyle := ControlStyle - [csOpaque] + [csAcceptsControls];

  if (csDesigning in ComponentState) then
    FIsAeroVista := False;

  FClosedTabList := TStringList.Create;
  FClosedListButton := nil;
  FInternalClosedListMenu := nil;

  FTabRoundEdges := True;
  FShow3D := True;
  FShadow := True;
  FItones := False;
  FTabAppearance := nil;
  FGlowButtonAppearance := nil;
  FFormScaled := true;

  FInternalOfficeTabSetStyler := TCustomAdvOfficeTabSetStyler.Create(self);
  FInternalOfficeTabSetStyler.Name := 'InternalStyler';

  FOfficeTabSetStyler := nil;
  FCurrentOfficeTabSetStyler := FInternalOfficeTabSetStyler;
  FCurrentOfficeTabSetStyler.AddControl(self);
  FInternalOfficeTabSetStyler.SetSubComponent(True);

  FOffSetX := 0;
  FOffSetY := 0;

  FTabOffSet := 4;

  FFixedTabs := 0;

  FTabPosition := tpTop;

  FAntiAlias := aaClearType;

  FAdvOfficeTabs := TOfficeTabCollection.Create(Self);

  FTabScroller := TTabSetTabScroller.Create;

  FTabSettings := TTabSetTabSettings.Create;
  FTabSettings.FTabSet := Self;
  FTabSettings.OnChange := OnTabSettingsChanged;

  FAllowTabUndock := False;

  FActiveTabIndex := -1;
  FHotTabIndex := -1;
  FOldHotTabIndex := -1;
  FDownTabIndex := -1;

  FShowTabHint := false;
  FHintTabIndex := -1;
  ShowHint := false;

  FButtonSettings := TTabSetButtonSettings.Create;
  FButtonSettings.OnChange := OnButtonSettingChanged;
  FTabListMenu := nil;
  FRotateTabLeftRight := true;
  FCloseOnTab := false;

  DoubleBuffered := true;
  Height := 27;
  Width := 300;
  FOldCapRightIndent := 0;

  TabRearrange := False;

  FOfficeHint := TAdvHintInfo.Create;
  FDummyHintControl := TDummyHintControl.Create(Self);
  FDummyHintControl.Visible := False;

  FCloseOnTabPosition := cpRight;

  FTabListButton := nil;
  FScrollPrevButton := nil;
  FScrollNextButton := nil;
  FScrollFirstButton := nil;
  FScrollLastButton := nil;
  FUpdateCount := 0;

  FDesignTime := (csDesigning in ComponentState) and not
      ((csReading in Owner.ComponentState) or (csLoading in Owner.ComponentState));

  ts := TAdvOfficeTabSetOfficeStyler.Create(self);
  ts.Style := tsOffice2007Luna;
  FInternalOfficeTabSetStyler.Assign(ts);
  ts.Free;

  FGlow := true;
  FStepHover := 0;
  FTimeInc := 20;

  i := GetFileVersion('COMCTL32.DLL');
  i := (i shr 16) and $FF;

  FIsWinXP := (i > 5);

  FTabRearrangeIndicatorColor := clBlue;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTabSet.CreateParams(var Params: TCreateParams);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTabSet.CreateWnd;
var
  t: TOfficeTabCollectionItem;
  p: TWinControl;

begin
  if (csDestroying in ComponentState) then
    Exit;

  inherited;

  if not (csDesigning in ComponentState) then
  begin
    p := self;

    repeat
      p := p.Parent;
    until (p is TForm) or (p is TActiveForm) or not Assigned(p);

    if Assigned(p) then
    begin
      if (p is TForm) then
        FFormScaled := (p as TForm).Scaled;
    end;

    TabSettings.Height := Round(GetDPIScale(FFormScaled, Canvas.Handle)* TabSettings.Height);

  end;


  if FDesignTime and (Name <> '') then
  begin
    FDesignTime := false;
    t := AdvOfficeTabs.Add;
    t.Caption := Name + '1';
    t := AdvOfficeTabs.Add;
    t.Caption := Name + '2';
    t := AdvOfficeTabs.Add;
    t.Caption := Name + '3';
    ActiveTabIndex := 0;
  end;
end;

//------------------------------------------------------------------------------

destructor TAdvCustomOfficeTabSet.Destroy;
var
  I: Integer;
begin
  if Assigned(FBrGlowTimer) then
    FBrGlowTimer.Free;

  if not (csDesigning in ComponentState) and Assigned(FArrow) then
    FreeAndNil(FArrow);

  if Assigned(FFloatingTabs) then
  begin
    for I := 0 to FFloatingTabs.Count - 1 do
      TOfficeTabCollectionItem(FFloatingTabs[i]).FParentCollection := nil;
    FFloatingTabs.Free;
  end;

  FInternalOfficeTabSetStyler.Free;
  FAdvOfficeTabs.Free;
  FTabSettings.Free;
  FTabScroller.Free;
  FOfficeHint.Free;
  FDummyHintControl.Free;
  FButtonSettings.Free;
  if (FTabListButton <> nil) then
    FTabListButton.Free;
  if (FScrollPrevButton <> nil) then
    FScrollPrevButton.Free;
  if (FScrollNextButton <> nil) then
    FScrollNextButton.Free;
  if (FScrollFirstButton <> nil) then
    FScrollFirstButton.Free;
  if (FScrollLastButton <> nil) then
    FScrollLastButton.Free;
  if (FCloseButtonGlobal <> nil) then
    FCloseButtonGlobal.Free;

  if Assigned(FClosedListButton) then
    FClosedListButton.Free;
  if Assigned(FInternalClosedListMenu) then
    FInternalClosedListMenu.Free;

  if FClosedTabList <> nil then
    FClosedTabList.Free;

  if Assigned(FTabAppearance) then
    FTabAppearance.Free;
  if Assigned(FGlowButtonAppearance) then
    FGlowButtonAppearance.Free;

  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTabSet.DoChange;
begin
  if Assigned(FOnChange) and (FUpdateCount = 0) then
    FOnChange(Self);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTabSet.DoChanging(FromTab, ToTab: integer; var AllowChange: boolean);
begin
  if Assigned(FOnChanging) and (FUpdateCount = 0) and (FromTab <> -1) and (ToTab <> -1) then
    FOnChanging(Self, FromTab, ToTab, AllowChange);
end;

//------------------------------------------------------------------------------
(*
procedure TAdvCustomOfficeTabSet.MoveAdvOfficeTab(CurIndex, NewIndex: Integer);
var
  OldActiveTab: Integer;
begin
  if (CurIndex >= 0) and (CurIndex < FAdvOfficeTabs.Count) and
     (NewIndex >= 0) and (NewIndex < FAdvOfficeTabs.Count) then
  begin
    OldActiveTab := ActiveTabIndex;
    FAdvOfficeTabs.Move(CurIndex, NewIndex);
    ActiveTabIndex := OldActiveTab;
    UpdateTabScroller;
  end;
end;

*)
//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTabSet.AlignControls(AControl: TControl;
  var ARect: TRect);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTabSet.Init;
begin
  FPropertiesLoaded := True;
  InitializeAndUpdateButtons;
  UpdateTabScroller;
  UpdateMultiLineTabs;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTabSet.Loaded;
begin
  inherited;
  Init;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTabSet.SaveToFile(FileName: String);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    sl.Add(Settings);
    sl.SaveToFile(FileName);
  finally
    sl.Free;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTabSet.LoadFromFile(FileName: String);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    sl.LoadFromFile(FileName);
    if sl.Count = 1 then
      Settings := sl.Strings[0];
  finally
    sl.Free;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTabSet.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;

  if not (csDestroying in ComponentState) and (Operation = opRemove) then
  begin
    if AComponent = AdvOfficeTabSetStyler then
      AdvOfficeTabSetStyler := nil;
    if (AComponent = PopupMenu) then
      PopupMenu := nil;
    if (AComponent = Images) then
      Images := nil;
    if (AComponent = DisabledImages) then
      DisabledImages := nil;
    if (AComponent = TabListMenu) then
      TabListMenu := nil;

    if (AComponent = ClosedListMenu) then
      ClosedListMenu := nil;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTabSet.AddTab(Caption: string);
begin
  AdvOfficeTabs.Add.Caption := Caption;
end;

procedure TAdvCustomOfficeTabSet.AdjustClientRect(var Rect: TRect);
begin
  inherited AdjustClientRect(Rect);
end;

//------------------------------------------------------------------------------

function TAdvCustomOfficeTabSet.DrawTab(TabIndex: Integer): boolean;
var
  GradColor: TColor;
  GradColorTo: TColor;
  GradColorMirror: TColor;
  GradColorMirrorTo: TColor;
  PenColor: TColor;
  GradB, GradU: TGDIPGradient;
  ImgList: TCustomImageList;
  Pic: TGDIPPicture;
  ImgEnabled: Boolean;
  R, CapR: TRect;
  ImgX, ImgY, ImgTxtSp: Integer;
  ImgW, ImgH: Integer;
  DCaption: string;
  DoRepaint: Boolean;
  TxtClr: TColor;
  TabAppearance: TCustomTabAppearance;
  tf: TFont;
  lf: TLogFont;
  bmp: TBitMap;
  R2, R3, CapR2: TRect;
  c1, c2, c3, c4, TempClr: TColor;
  Ellipsis: Boolean;
  Layout: TButtonLayout;
  TxtR: TRect;
  Shape: TAdvTabShape;
  HighLightClr: TColor;
  cbr, TR: TRect;
  ImgP: TPoint;
  AAlign: TAlignment;
  htheme: THandle;
  ThemeStyle: DWord;
  rc: TRect;
  DChecked: Cardinal;
  aa: TAntiAlias;
begin
  Result := false;

  if (TabIndex < 0) or (TabIndex >= FAdvOfficeTabs.Count) then
    Exit;

  ImgTxtSp := IMG_SPACE;
  GradColor := clNone;
  GradColorTo := clNone;
  GradColorMirror := clNone;
  GradColorMirrorTo := clNone;
  PenColor := clNone;
  TxtClr := clNone;
  GradB := ggRadial;
  GradU := ggRadial;

  ImgList := nil;
  DoRepaint := True;

  R := GetTabRect(TabIndex);

  if (R.Left <= -1) and (R.Right <= -1) then
  begin
    Result := not AdvOfficeTabs[TabIndex].Visible;
    Exit;
  end;

  Result := true;

  Layout := blGlyphLeft;
  ImgY := 0;
  ImgX := 0;
  ImgH := 0;
  ImgW := 0;
  ImgEnabled := true;

  Ellipsis := (TabSettings.Width > 0) and not TabSettings.WordWrap;

  if AdvOfficeTabs[TabIndex].UseTabAppearance then
    TabAppearance := AdvOfficeTabs[TabIndex].TabAppearance
  else if Assigned(FTabAppearance) and FItones then
    TabAppearance := FTabAppearance
  else
    TabAppearance := FCurrentOfficeTabSetStyler.TabAppearance;

  HighLightClr := TabAppearance.HighLightColorSelected;

  if IsGlass then
    aa := aaAntiAlias
  else
    aa := FAntiAlias;

  with TabAppearance do
  begin
    if not(FAdvOfficeTabs.Items[TabIndex].Enabled) and ShowNonSelectedTabs then
    begin
      if ShowNonSelectedTabs then
      begin
        GradColor := ColorDisabled;
        GradColorTo := ColorDisabledTo;
        GradColorMirror := ColorMirrorDisabled;
        GradColorMirrorTo := ColorMirrorDisabledTo;
        PenColor := BorderColorDisabled;
        GradU := GradientDisabled;
        GradB := GradientMirrorDisabled;
        TxtClr := TextColorDisabled;
      end
      else
      begin

      end;
    end
    else
    if (TabIndex = ActiveTabIndex) then
    begin
      GradColor := ColorSelected;
      GradColorTo := ColorSelectedTo;
      GradColorMirror := ColorMirrorSelected;
      GradColorMirrorTo := ColorMirrorSelectedTo;
      PenColor := BorderColorSelected;
      GradU := GradientSelected;
      GradB := GradientMirrorSelected;
      TxtClr := TextColorSelected;
      HighLightClr := TabAppearance.HighLightColorSelected;

      if (TabIndex = FHotTabIndex) then
      begin
        PenColor := BorderColorSelectedHot;
        HighLightClr := TabAppearance.HighLightColorSelectedHot;
      end;

      if Assigned(FAdvOfficeTabs.Items[TabIndex].FTimer) then
      begin
        if (FAdvOfficeTabs.Items[TabIndex].FGlowState = gsPush) then
        begin
          GradColor := BlendColor(GradColor, FColorHot, FAdvOfficeTabs.Items[TabIndex].FStepPush);
          GradColorTo := BlendColor(GradColorTo, FColorHotTo, FAdvOfficeTabs.Items[TabIndex].FStepPush);
          GradColorMirror := BlendColor(GradColorMirror, FColorMirrorHot, FAdvOfficeTabs.Items[TabIndex].FStepPush);
          GradColorMirrorTo := BlendColor(GradColorMirrorTo, FColorMirrorHotTo, FAdvOfficeTabs.Items[TabIndex].FStepPush);
          PenColor := BlendColor(PenColor, BorderColorHot, FAdvOfficeTabs.Items[TabIndex].FStepPush);
        end
        else if (FAdvOfficeTabs.Items[TabIndex].FGlowState = gsHover) then
          PenColor := BlendColor(BorderColorSelectedHot, BorderColorSelected, FAdvOfficeTabs.Items[TabIndex].FStepHover);
      end;

      if (FDownTabIndex = TabIndex) and not (csDesigning in ComponentState) then
      begin
        PenColor := BorderColorDown;
        HighLightClr := TabAppearance.HighLightColorDown;
      end;
    end
    else //if State = absUp then
    begin
      if (TabIndex = FHotTabIndex) then
      begin
        GradColor := ColorHot;
        GradColorTo := ColorHotTo;
        GradColorMirror := ColorMirrorHot;
        GradColorMirrorTo := ColorMirrorHotTo;
        PenColor := BorderColorHot;
        GradU := GradientHot;
        GradB := GradientMirrorHot;
        TxtClr := TextColorHot;
        HighLightClr := TabAppearance.HighLightColorHot;
        
        if Assigned(FAdvOfficeTabs.Items[TabIndex].FTimer) and (FAdvOfficeTabs.Items[TabIndex].FGlowState = gsHover) then
        begin
          if ShowNonSelectedTabs then
          begin
            GradColor := BlendColor(FColorHot, FColor, FAdvOfficeTabs.Items[TabIndex].FStepHover);
            GradColorTo := BlendColor(FColorHotTo, FColorTo, FAdvOfficeTabs.Items[TabIndex].FStepHover);
            GradColorMirror := BlendColor(FColorMirrorHot, FColorMirror, FAdvOfficeTabs.Items[TabIndex].FStepHover);
            GradColorMirrorTo := BlendColor(FColorMirrorHotTo, FColorMirrorTo, FAdvOfficeTabs.Items[TabIndex].FStepHover);
            PenColor := BlendColor(BorderColorHot, BorderColor, FAdvOfficeTabs.Items[TabIndex].FStepHover);
          end
          else
          begin
            GradColor := BlendColor(FColorHot, {FCurrentOfficeTabSetStyler.}TabAppearance.BackGround.Color, FAdvOfficeTabs.Items[TabIndex].FStepHover);
            if ({FCurrentOfficeTabSetStyler.}TabAppearance.BackGround.ColorTo <> clNone) then
              GradColorTo := BlendColor(FColorHotTo, {FCurrentOfficeTabSetStyler.}TabAppearance.BackGround.ColorTo, FAdvOfficeTabs.Items[TabIndex].FStepHover)
            else
              GradColorTo := BlendColor(FColorHotTo, {FCurrentOfficeTabSetStyler.}TabAppearance.BackGround.Color, FAdvOfficeTabs.Items[TabIndex].FStepHover);
            GradColorMirror := BlendColor(FColorMirrorHot, {FCurrentOfficeTabSetStyler.}TabAppearance.BackGround.Color, FAdvOfficeTabs.Items[TabIndex].FStepHover);
            GradColorMirrorTo := BlendColor(FColorMirrorHotTo, {FCurrentOfficeTabSetStyler.}TabAppearance.BackGround.Color, FAdvOfficeTabs.Items[TabIndex].FStepHover);

            if BorderColorHot <> clNone then
              PenColor := BlendColor(BorderColorHot, {FCurrentOfficeTabSetStyler.}TabAppearance.BackGround.Color, FAdvOfficeTabs.Items[TabIndex].FStepHover);
          end;
        end;
      end
      else // Normal draw
      begin
        if ShowNonSelectedTabs then
        begin
          GradColor := Color;
          GradColorTo := ColorTo;
          GradColorMirror := ColorMirror;
          GradColorMirrorTo := ColorMirrorTo;
          PenColor := BorderColor;
          GradU := Gradient;
          GradB := GradientMirror;
          TxtClr := TextColor;
          if Assigned(FAdvOfficeTabs.Items[TabIndex].FTimer) and (FAdvOfficeTabs.Items[TabIndex].FGlowState = gsHover) {and (TabIndex = FOldHotTabIndex)} then
          begin
            GradColor := BlendColor(FColorHot, FColor, FAdvOfficeTabs.Items[TabIndex].FStepHover);
            GradColorTo := BlendColor(FColorHotTo, FColorTo, FAdvOfficeTabs.Items[TabIndex].FStepHover);
            GradColorMirror := BlendColor(FColorMirrorHot, FColorMirror, FAdvOfficeTabs.Items[TabIndex].FStepHover);
            GradColorMirrorTo := BlendColor(FColorMirrorHotTo, FColorMirrorTo, FAdvOfficeTabs.Items[TabIndex].FStepHover);
            PenColor := BlendColor(BorderColorHot, BorderColor, FAdvOfficeTabs.Items[TabIndex].FStepHover);
          end;
        end
        else
        begin
          DoRepaint := False;
          TxtClr := TextColor;
          GradU := GradientHot;
          GradB := GradientMirrorHot;

          if not FAdvOfficeTabs.Items[TabIndex].Enabled then
           TxtClr := TextColorDisabled;


          if Assigned(FAdvOfficeTabs.Items[TabIndex].FTimer) and (FAdvOfficeTabs.Items[TabIndex].FGlowState = gsHover){ and (TabIndex = FOldHotTabIndex)} then
          begin
            GradColor := BlendColor(FColorHot, {FCurrentOfficeTabSetStyler.}TabAppearance.BackGround.Color, FAdvOfficeTabs.Items[TabIndex].FStepHover);
            if ({FCurrentOfficeTabSetStyler.}TabAppearance.BackGround.ColorTo <> clNone) then
              GradColorTo := BlendColor(FColorHotTo, {FCurrentOfficeTabSetStyler.}TabAppearance.BackGround.ColorTo, FAdvOfficeTabs.Items[TabIndex].FStepHover)
            else
              GradColorTo := BlendColor(FColorHotTo, {FCurrentOfficeTabSetStyler.}TabAppearance.BackGround.Color, FAdvOfficeTabs.Items[TabIndex].FStepHover);
            GradColorMirror := BlendColor(FColorMirrorHot, {FCurrentOfficeTabSetStyler.}TabAppearance.BackGround.Color, FAdvOfficeTabs.Items[TabIndex].FStepHover);
            GradColorMirrorTo := BlendColor(FColorMirrorHotTo, {FCurrentOfficeTabSetStyler.}TabAppearance.BackGround.Color, FAdvOfficeTabs.Items[TabIndex].FStepHover);

            if BorderColorHot <> clNone then
              PenColor := BlendColor(BorderColorHot, {FCurrentOfficeTabSetStyler.}TabAppearance.BackGround.Color, FAdvOfficeTabs.Items[TabIndex].FStepHover);
            DoRepaint := True;
          end;
        end;
      end;
    end;

    if not (csDesigning in ComponentState) and CanGlow and Assigned(FBrGlowTimer) and FAdvOfficeTabs[TabIndex].Glow and (FAdvOfficeTabs[TabIndex].GlowColor <> clNone) then
    begin
      PenColor := BlendColor(FAdvOfficeTabs[TabIndex].GlowColor, PenColor, FStepHover);
    end;

    DCaption := FAdvOfficeTabs.Items[TabIndex].Caption;
    Canvas.Font.Assign(TabAppearance.Font);
    Canvas.Font.Size := Round(GetDPIScale(FFormScaled, Canvas.Handle)* Canvas.Font.Size);

    Canvas.Font.Color := TxtClr;

    c1 := GradColor;
    c2 := c1;
    c3 := c1;
    c4 := c3;
    if DoRepaint then
    begin
      Shape := TabSettings.Shape;

      if (TabPosition in [tpLeft, tpRight]) and not RotateTabLeftRight then
      begin
        Shape := tsRectangle;
      end;

      case TabPosition of
        tpTop:
        begin
          c1 := Canvas.Pixels[R.Left, R.Top];
          c2 := Canvas.Pixels[R.Right - 1, R.Top];
        end;
        tpBottom:
        begin
          c1 := Canvas.Pixels[R.Left, R.Bottom-2];
          c2 := Canvas.Pixels[R.Right - 1, R.Bottom-1];
          c3 := Canvas.Pixels[R.Left, R.Top];
          c4 := Canvas.Pixels[R.Right - 1, R.Top];
        end;
        tpLeft:
        begin
          c1 := Canvas.Pixels[R.Left, R.Top];
          c2 := Canvas.Pixels[R.Left, R.Bottom-1];
        end;
        tpRight:
        begin
          c1 := Canvas.Pixels[R.Right, R.Top];
          c2 := Canvas.Pixels[R.Right, R.Bottom-1];
        end;
      end;

      if (ActiveTabIndex = TabIndex) and (CloseOnTab) and (ButtonSettings.CloseButton) and false then
      begin
        bmp := TBitMap.Create;
        try
          bmp.Height := (R.Bottom - R.Top) - 4;
          bmp.Width := R.Right - R.Left;
          DrawVistaGradient(bmp.Canvas, Rect(0, 0, R.Right - R.Left, R.Bottom-R.Top),GradColor, GradColorTo, GradColorMirror, GradColorMirrorTo, PenColor,
            GradU, GradB, '', {Canvas.}Font, Layout, Enabled, False, FAntiAlias, {True}TabRoundEdges, FAdvOfficeTabs.Items[TabIndex].Progress, RotateTabLeftRight, TabPosition);
          Canvas.Draw(R.Left, R.Top, bmp);
        finally
          bmp.Free;
        end;
      end
      else
      begin
        //DrawVistaGradient(Canvas, R,GradColor, GradColorTo, GradColorMirror, GradColorMirrorTo, PenColor,
         //GradU, GradB, '', {Canvas.}Font, Enabled, False, FAntiAlias, True{FCurrentOfficeTabSetStyler.RoundEdges}, TabPosition);
        bmp := TBitMap.Create;
        try
          if (TabPosition = tpTop) then
          begin
            if MultiLine and (TabIndex <> ActiveTabIndex) then
              bmp.Height := (R.Bottom - R.Top)
            else
              bmp.Height := (R.Bottom - R.Top) - 4;
            bmp.Width := R.Right - R.Left;
            R2 := Rect(0, 0, R.Right - R.Left, R.Bottom-R.Top);
          end
          else if (TabPosition = tpBottom) then
          begin
            bmp.Height := (R.Bottom - R.Top){- 4};
            bmp.Width := R.Right - R.Left;
            R2 := Rect(0, 0, R.Right - R.Left, R.Bottom-R.Top);
          end
          else if (TabPosition = tpLeft) then
          begin
            bmp.Height := (R.Bottom - R.Top);
            if MultiLine and (TabIndex <> ActiveTabIndex) then
              bmp.Width := (R.Right - R.Left)
            else
              bmp.Width := (R.Right - R.Left) - 4;
            R2 := Rect(0, 0, R.Right - R.Left, R.Bottom-R.Top);
          end
          else if (TabPosition = tpRight) then
          begin
            bmp.Height := (R.Bottom - R.Top);
            bmp.Width := (R.Right - R.Left);
            R2 := Rect(-4, 0, (R.Right - R.Left), R.Bottom-R.Top);
            //R.Left := R.Left+2;
          end;

          if UseOldDrawing then
            DrawVistaGradient(bmp.Canvas, R2,GradColor, GradColorTo, GradColorMirror, GradColorMirrorTo, PenColor,
              GradU, GradB, '', {Canvas.}Font, Layout, Enabled, False, FAntiAlias, {True}TabRoundEdges, FAdvOfficeTabs.Items[TabIndex].Progress, RotateTabLeftRight, TabPosition)
          else
          begin
            if (PenColor <> clNone) then
              TempClr := BlendColor(PenColor, clWhite, 50)
            else
              TempClr := BlendColor(clBlack, clWhite, 50);

            bmp.Canvas.Brush.Color := TempClr;
            bmp.Canvas.FillRect(Rect(0, 0, bmp.Width, bmp.Height));

            R3 := R2;
            case TabPosition of
              tpTop: R3.Right := R3.Right - 1;
              tpBottom:
              begin
                R3.Right := R3.Right - 1;
                R3.Bottom := R3.Bottom - 1;
              end;
              tpLeft: R3.Bottom := R3.Bottom - 1;
              tpRight:
              begin
                R3.Right := R3.Right - 1;
                R3.Bottom := R3.Bottom - 2;
              end;
            end;

            {case TabPosition of
              tpTop: R2.Bottom := R2.Bottom - 3;
              tpBottom: R2.Top := R2.Top + 2;
              tpLeft: R2.Right := R2.Right - 3;
              tpRight: R2.Left := R2.Left + 2;
            end; }

            DrawVistaTab(bmp.Canvas, R3,GradColor, GradColorTo, GradColorMirror, GradColorMirrorTo, PenColor,
              GradU, GradB, Enabled, Shape, False, TabSettings.Rounding, RotateTabLeftRight, TabPosition, FAdvOfficeTabs.Items[TabIndex].Progress);

            bmp.TransparentColor := TempClr;
            bmp.Transparent := True;
          end;
          if IsGlass then
            DrawGDIPImage(nil, Canvas, Point(R.Left, R.Top), bmp, (Shape <> tsRectangle) or (TabSettings.Rounding > 3))
          else
            Canvas.Draw(R.Left, R.Top, bmp);
        finally
          bmp.Free;
        end;
      end;


      if UseOldDrawing then
      begin
        case TabPosition of
          tpTop:
          begin
            if {True}TabRoundEdges then
            begin
              Canvas.Pixels[R.Left, R.Top] := c1; //Canvas.Pixels[R.Left - 1, R.Top - 1];
              Canvas.Pixels[R.Left + 1, R.Top] := c1; //Canvas.Pixels[R.Left + 1, R.Top - 1];
              Canvas.Pixels[R.Left, R.Top + 1] := c1; //Canvas.Pixels[R.Left - 1, R.Top];

              Canvas.Pixels[R.Right - 1, R.Top] := c2; //Canvas.Pixels[R.Right + 1, R.Top];
              Canvas.Pixels[R.Right - 2, R.Top] := c2; //Canvas.Pixels[R.Right + 1, R.Top];
              Canvas.Pixels[R.Right - 1, R.Top + 1] := c2; //Canvas.Pixels[R.Right + 1, R.Top];
            end;

            //--- Draw 3D effect
            if not FAdvOfficeTabs.Items[TabIndex].Progress.Visible and Show3D then
            begin
              if not Assigned(FAdvOfficeTabs.Items[TabIndex].FTimer) then
              begin
                Canvas.Pen.Color := BlendColor(clWhite, GradColor, 50);
                Canvas.MoveTo(R.Left+3, R.Top + 3);
                Canvas.LineTo(R.Right-3, R.Top+3);
              end
              else
              begin
                if (FAdvOfficeTabs.Items[TabIndex].FGlowState = gsHover) then
                  Canvas.Pen.Color := BlendColor(clWhite, GradColor, Min(20, FAdvOfficeTabs.Items[TabIndex].FStepHover))
                else
                  Canvas.Pen.Color := BlendColor(clWhite, GradColor, 50);
                Canvas.MoveTo(R.Left+3, R.Top + 3);
                Canvas.LineTo(R.Right-3, R.Top+3);
              end;
            end;

            //-- Draw Shadow
            if (TabAppearance.ShadowColor <> clNone) and not IsGlass and Shadow then
            begin
              Canvas.Pen.Color := TabAppearance.ShadowColor;
              Canvas.MoveTo(R.Right, R.Top + 3);
              Canvas.LineTo(R.Right, R.Bottom-4);
              Canvas.Pen.Color := BlendColor(TabAppearance.ShadowColor, TabAppearance.BackGround.Color, 40);
              Canvas.MoveTo(R.Right + 1, R.Top + 4);
              Canvas.LineTo(R.Right + 1, R.Bottom-4);
            end;

            if Show3D and (HighLightClr <> clNone) and not (FAdvOfficeTabs.Items[TabIndex].Progress.Visible and ShowNonSelectedTabs and (TabIndex <> ActiveTabIndex)) then
            begin
              Canvas.Pen.Color := BlendColor(GradColor, PenColor, 80);
              Canvas.MoveTo(R.Left+3, R.Top+1);
              Canvas.LineTo(R.Right-3, R.Top+1);
              Canvas.Pen.Color := HighLightClr; //BlendColor(GradColor, PenColor, FCurrentOfficeTabSetStyler.BlendFactor);
              Canvas.MoveTo(R.Left+1, R.Top + 3);
              Canvas.LineTo(R.Left+1, R.Bottom-5);
              Canvas.MoveTo(R.Right-2, R.Top + 3);
              Canvas.LineTo(R.Right-2, R.Bottom-5);
            end;
          end;
          tpBottom:
          begin
            if {True}TabRoundEdges then
            begin
              Canvas.Pixels[R.Left, R.Bottom-2] := c1; //Canvas.Pixels[R.Left - 1, R.Bottom - 1];
              Canvas.Pixels[R.Left + 1, R.Bottom-1] := c1; //Canvas.Pixels[R.Left - 1, R.Bottom - 1];
              Canvas.Pixels[R.Left, R.Bottom - 1] := c1; //Canvas.Pixels[R.Left - 1, R.Bottom -1];

              Canvas.Pixels[R.Right - 1, R.Bottom-1] := c2; //Canvas.Pixels[R.Right + 1, R.Bottom];
              Canvas.Pixels[R.Right - 2, R.Bottom-1] := c2; //Canvas.Pixels[R.Right + 1, R.Bottom];
              Canvas.Pixels[R.Right - 1, R.Bottom - 2] := c2; //Canvas.Pixels[R.Right + 1, R.Bottom];

              Canvas.Pixels[R.Left, R.Top] := c3;
              Canvas.Pixels[R.Left + 1, R.Top] := c3;
              Canvas.Pixels[R.Left, R.Top + 1] := c3;

              Canvas.Pixels[R.Right - 1, R.Top] := c4;
              Canvas.Pixels[R.Right - 2, R.Top] := c4;
              Canvas.Pixels[R.Right - 1, R.Top + 1] := c4;
            end;

            //--- Draw 3D effect
            if not FAdvOfficeTabs.Items[TabIndex].Progress.Visible and Show3D then
            begin
              if not Assigned(FAdvOfficeTabs.Items[TabIndex].FTimer) then
              begin
                Canvas.Pen.Color := BlendColor(clWhite, GradColor, 50);
                Canvas.MoveTo(R.Left+3, R.Bottom - 4);
                Canvas.LineTo(R.Right-3, R.Bottom - 4);
              end
              else
              begin
                if (FAdvOfficeTabs.Items[TabIndex].FGlowState = gsHover) then
                  Canvas.Pen.Color := BlendColor(clWhite, GradColor, Min(20, FAdvOfficeTabs.Items[TabIndex].FStepHover))
                else
                  Canvas.Pen.Color := BlendColor(clWhite, GradColor, 50);

                Canvas.MoveTo(R.Left+3, R.Bottom - 4);
                Canvas.LineTo(R.Right-3, R.Bottom - 4);
              end;
            end;

            //-- Draw Shadow
            if (TabAppearance.ShadowColor <> clNone) and not IsGlass and Shadow then
            begin
              Canvas.Pen.Color := TabAppearance.ShadowColor;
              Canvas.MoveTo(R.Right, R.Top + 3);
              Canvas.LineTo(R.Right, R.Bottom-2);
              Canvas.Pen.Color := BlendColor(TabAppearance.ShadowColor, TabAppearance.BackGround.Color, 40);
              Canvas.MoveTo(R.Right + 1, R.Top + 3);
              Canvas.LineTo(R.Right + 1, R.Bottom-3);
            end;

            if Show3D and (HighLightClr <> clNone) and not (FAdvOfficeTabs.Items[TabIndex].Progress.Visible and ShowNonSelectedTabs and (TabIndex <> ActiveTabIndex)) then
            begin
              Canvas.Pen.Color := BlendColor(GradColor, PenColor, 80);
              Canvas.MoveTo(R.Left+3, R.Bottom -2);
              Canvas.LineTo(R.Right-3, R.Bottom -2);
              Canvas.Pen.Color := HighLightClr; //BlendColor(GradColor, PenColor, FCurrentOfficeTabSetStyler.BlendFactor);
              Canvas.MoveTo(R.Left+1, R.Bottom - 3);
              Canvas.LineTo(R.Left+1, R.Top + 5);
              Canvas.MoveTo(R.Right-2, R.Bottom - 3);
              Canvas.LineTo(R.Right-2, R.Top + 5);
            end;
          end;
          tpLeft:
          begin
            if {True}TabRoundEdges then
            begin
              Canvas.Pixels[R.Left, R.Top] := c1; //Canvas.Pixels[R.Left - 1, R.Top - 1];
              Canvas.Pixels[R.Left + 1, R.Top] := c1; //Canvas.Pixels[R.Left + 1, R.Top - 1];
              Canvas.Pixels[R.Left, R.Top + 1] := c1; //Canvas.Pixels[R.Left - 1, R.Top];

              Canvas.Pixels[R.Left, R.Bottom-1] := c2; //Canvas.Pixels[R.Left - 1, R.Bottom];
              Canvas.Pixels[R.Left + 1, R.Bottom-1] := c2; //Canvas.Pixels[R.Left - 1, R.Bottom];
              Canvas.Pixels[R.Left, R.Bottom - 2] := c2; //Canvas.Pixels[R.Left - 1, R.Bottom];
            end;

            //--- Draw 3D effect
            if not FAdvOfficeTabs.Items[TabIndex].Progress.Visible and Show3D then
            begin
              if not Assigned(FAdvOfficeTabs.Items[TabIndex].FTimer) then
              begin
                Canvas.Pen.Color := BlendColor(clWhite, GradColor, 50);
                Canvas.MoveTo(R.Left+3, R.Top + 3);
                Canvas.LineTo(R.Left+3, R.Bottom - 3);
              end
              else
              begin
                if (FAdvOfficeTabs.Items[TabIndex].FGlowState = gsHover) then
                  Canvas.Pen.Color := BlendColor(clWhite, GradColor, Min(20, FAdvOfficeTabs.Items[TabIndex].FStepHover))
                else
                  Canvas.Pen.Color := BlendColor(clWhite, GradColor, 50);
                Canvas.MoveTo(R.Left+3, R.Top + 3);
                Canvas.LineTo(R.Left+3, R.Bottom - 3);
              end;
            end;

            //--- Draw Shadow
            if Shadow then
            begin
              if RotateTabLeftRight then
              begin
                if (TabAppearance.ShadowColor <> clNone) and not IsGlass then
                begin
                  Canvas.Pen.Color := TabAppearance.ShadowColor;
                  Canvas.MoveTo(R.Left + 3, R.Top - 1);
                  Canvas.LineTo(R.Right - 4, R.Top - 1);
                  Canvas.Pen.Color := BlendColor(TabAppearance.ShadowColor, TabAppearance.BackGround.Color, 40);
                  Canvas.MoveTo(R.Left + 4, R.Top - 2);
                  Canvas.LineTo(R.Right - 4, R.Top - 2);
                end;
              end
              else
              begin
                if (TabAppearance.ShadowColor <> clNone) and not IsGlass then
                begin
                  Canvas.Pen.Color := TabAppearance.ShadowColor;
                  Canvas.MoveTo(R.Left + 3, R.Bottom);
                  Canvas.LineTo(R.Right - 4, R.Bottom);
                  Canvas.Pen.Color := BlendColor(TabAppearance.ShadowColor, TabAppearance.BackGround.Color, 40);
                  Canvas.MoveTo(R.Left + 4, R.Bottom + 1);
                  Canvas.LineTo(R.Right - 4, R.Bottom + 1);
                end;
              end;
            end;

            if Show3D and (HighLightClr <> clNone) and not (FAdvOfficeTabs.Items[TabIndex].Progress.Visible and ShowNonSelectedTabs and (TabIndex <> ActiveTabIndex)) then
            begin
              Canvas.Pen.Color := BlendColor(GradColor, PenColor, 80);
              Canvas.MoveTo(R.Left+1, R.Top+3);
              Canvas.LineTo(R.Left+1, R.Bottom-3);
              Canvas.Pen.Color := HighLightClr; //BlendColor(GradColor, PenColor, FCurrentOfficeTabSetStyler.BlendFactor);
              Canvas.MoveTo(R.Left+3, R.Top + 1);
              Canvas.LineTo(R.Right-5, R.Top+1);
              Canvas.MoveTo(R.Left+3, R.Bottom-2);
              Canvas.LineTo(R.Right-5, R.Bottom-2);
            end;
          end;
          tpRight:
          begin
            if {True}TabRoundEdges then
            begin
              Canvas.Pixels[R.Right, R.Top] := c1; //Canvas.Pixels[R.Right + 1, R.Top - 1];
              Canvas.Pixels[R.Right - 1, R.Top] := c1; //Canvas.Pixels[R.Right + 1, R.Top - 1];
              Canvas.Pixels[R.Right, R.Top + 1] := c1; //Canvas.Pixels[R.Right + 1, R.Top];

              Canvas.Pixels[R.Right, R.Bottom-1] := c2; //Canvas.Pixels[R.Right + 1, R.Bottom];
              Canvas.Pixels[R.Right - 1, R.Bottom-1] := c2; //Canvas.Pixels[R.Right + 1, R.Bottom];
              Canvas.Pixels[R.Right, R.Bottom - 2] := c2; //Canvas.Pixels[R.Right + 1, R.Bottom];
            end;

            //--- Draw 3D effect
            if not FAdvOfficeTabs.Items[TabIndex].Progress.Visible and Show3D then
            begin
              if not Assigned(FAdvOfficeTabs.Items[TabIndex].FTimer) then
              begin
                Canvas.Pen.Color := BlendColor(clWhite, GradColor, 50);
                Canvas.MoveTo(R.Right-3, R.Top + 3);
                Canvas.LineTo(R.Right-3, R.Bottom - 3);
              end
              else
              begin
                if (FAdvOfficeTabs.Items[TabIndex].FGlowState = gsHover) then
                  Canvas.Pen.Color := BlendColor(clWhite, GradColor, Min(20, FAdvOfficeTabs.Items[TabIndex].FStepHover))
                else
                  Canvas.Pen.Color := BlendColor(clWhite, GradColor, 50);
                Canvas.MoveTo(R.Right-3, R.Top + 3);
                Canvas.LineTo(R.Right-3, R.Bottom - 3);
              end;
            end;

            //-- Draw Shadow
            if (TabAppearance.ShadowColor <> clNone) and not IsGlass and Shadow then
            begin
              Canvas.Pen.Color := TabAppearance.ShadowColor;
              Canvas.MoveTo(R.Left, R.Bottom);
              Canvas.LineTo(R.Right - 3, R.Bottom);
              Canvas.Pen.Color := BlendColor(TabAppearance.ShadowColor, TabAppearance.BackGround.Color, 40);
              Canvas.MoveTo(R.Left, R.Bottom + 1);
              Canvas.LineTo(R.Right - 4, R.Bottom + 1);
            end;

            if Show3D and (HighLightClr <> clNone) and not (FAdvOfficeTabs.Items[TabIndex].Progress.Visible and ShowNonSelectedTabs and (TabIndex <> ActiveTabIndex)) then
            begin
              Canvas.Pen.Color := BlendColor(GradColor, PenColor, 80);
              Canvas.MoveTo(R.Right-2, R.Top+3);
              Canvas.LineTo(R.Right-2, R.Bottom-3);
              Canvas.Pen.Color := HighLightClr; //BlendColor(GradColor, PenColor, FCurrentOfficeTabSetStyler.BlendFactor);
              Canvas.MoveTo(R.Left+5, R.Top + 1);
              Canvas.LineTo(R.Right-3, R.Top+1);
              Canvas.MoveTo(R.Left+5, R.Bottom-2);
              Canvas.LineTo(R.Right-3, R.Bottom-2);
            end;
          end;
        end;
      end;
    end;

    if AdvOfficeTabs[TabIndex].ShowCheckBox then
    begin
      ThemeStyle := 0;
      DChecked := 0;
      if AdvOfficeTabs[TabIndex].Checked then
      begin
        DChecked := DFCS_BUTTONCHECK or DFCS_CHECKED;
        if AdvOfficeTabs[TabIndex].Enabled then
          ThemeStyle := CBS_CHECKEDNORMAL
        else
          ThemeStyle := CBS_CHECKEDDISABLED;

        if not AdvOfficeTabs[TabIndex].Enabled then
          DChecked := DChecked or DFCS_INACTIVE;
      end;

      rc := GetCheckBoxRect(TabIndex);

      if FIsWinXP and IsThemeActive and not (csDesigning in ComponentState) then
      begin
        htheme := OpenThemeData(Self.Handle,'button');
        ACXPVS.DrawThemeBackground(HTheme, Canvas.Handle, BP_CHECKBOX,ThemeStyle,@rc,nil);
        CloseThemeData(htheme);
      end
      else
        DrawFrameControl(Canvas.Handle,rc,DFC_BUTTON, DChecked);
    end;


    if Assigned(FOnDrawTab) then
    begin
      FOnDrawTab(Self, Canvas, TabIndex, R);
      Exit;
    end;

    if not UseOldDrawing then
    begin
      if TabPosition in [tpTop, tpBottom] then
      begin
        R.Left := R.Left + GetLeftRoundingOffset;
      end
      else if RotateTabLeftRight then
      begin
        if TabPosition = tpLeft then
          R.Bottom := R.Bottom - GetLeftRoundingOffset
        else
        begin
          if TabSettings.Shape in [tsLeftRamp, tsLeftRightRamp] then
            R.Top := R.Top + GetLeftRoundingOffset;
        end;
      end;
    end;

    if (TabSettings.Width > 0) then
      GetCloseBtnImageAndTextRect(TabIndex, cbr, TR, ImgP);
    
    case TabPosition of
      tpTop, tpBottom:
      begin
        CapR := Rect(R.Left + FTabSettings.LeftMargin + 2, R.Top, R.Right, R.Bottom);
        if not ShowCloseOnNonSelectedTabs then
        begin
          if (ButtonSettings.CloseButton and CloseOnTab and (ActiveTabIndex <> TabIndex) and AdvOfficeTabs[TabIndex].ShowClose) then
            CapR.Left := CapR.Left + (ButtonSettings.ButtonSize + 4 ) div 2
          else if (ButtonSettings.CloseButton and CloseOnTab and (ActiveTabIndex = TabIndex) and AdvOfficeTabs[TabIndex].ShowClose) and (CloseOnTabPosition = cpLeft) and ((TabSettings.Width <= 0) or (TabSettings.Alignment <> taCenter)) then
            CapR.Left := CapR.Left + (ButtonSettings.ButtonSize + 4)
          else if (ButtonSettings.CloseButton and CloseOnTab and (ActiveTabIndex = TabIndex) and AdvOfficeTabs[TabIndex].ShowClose) and (CloseOnTabPosition = cpRight) and ((TabSettings.Width <= 0) or (TabSettings.Alignment <> taCenter)) then
            CapR.Right := CapR.Right - (ButtonSettings.ButtonSize + 4)
        end
        else
        begin
//          if (ButtonSettings.CloseButton and CloseOnTab and AdvOfficeTabs[TabIndex].ShowClose) then
//            CapR.Left := CapR.Left + (ButtonSettings.ButtonSize + 4 ) div 2
          if (ButtonSettings.CloseButton and CloseOnTab and AdvOfficeTabs[TabIndex].ShowClose) and (CloseOnTabPosition = cpLeft) and ((TabSettings.Width <= 0) or (TabSettings.Alignment <> taCenter)) then
            CapR.Left := CapR.Left + (ButtonSettings.ButtonSize + 4)
          else if (ButtonSettings.CloseButton and CloseOnTab and AdvOfficeTabs[TabIndex].ShowClose) and (CloseOnTabPosition = cpRight) and ((TabSettings.Width <= 0) or (TabSettings.Alignment <> taCenter)) then
            CapR.Right := CapR.Right - (ButtonSettings.ButtonSize + 4)
        end;

        if AdvOfficeTabs[TabIndex].ShowCheckBox then
          CapR.Left := Capr.Left + 20;
      end;
      tpLeft:
      begin
        if RotateTabLeftRight then
        begin
          CapR := Rect(R.Left, R.Top, R.Right, R.Bottom - FTabSettings.LeftMargin);
          if not ShowCloseOnNonSelectedTabs then
          begin
            if (ButtonSettings.CloseButton and CloseOnTab and (ActiveTabIndex <> TabIndex) and AdvOfficeTabs[TabIndex].ShowClose) then
              CapR.Bottom := CapR.Bottom - (ButtonSettings.ButtonSize + 4) div 2
            else if (ButtonSettings.CloseButton and CloseOnTab and (ActiveTabIndex = TabIndex) and AdvOfficeTabs[TabIndex].ShowClose) and (CloseOnTabPosition = cpLeft) and ((TabSettings.Width <= 0) or (TabSettings.Alignment <> taCenter)) then
              CapR.Bottom := CapR.Bottom - ButtonSettings.ButtonSize - 4;
          end
          else
          begin
//            if (ButtonSettings.CloseButton and CloseOnTab and AdvOfficeTabs[TabIndex].ShowClose) then
//              CapR.Bottom := CapR.Bottom - (ButtonSettings.ButtonSize + 4) div 2
            if (ButtonSettings.CloseButton and CloseOnTab and AdvOfficeTabs[TabIndex].ShowClose) and (CloseOnTabPosition = cpLeft) and ((TabSettings.Width <= 0) or (TabSettings.Alignment <> taCenter)) then
              CapR.Bottom := CapR.Bottom - ButtonSettings.ButtonSize - 4;
          end;

          if AdvOfficeTabs[TabIndex].ShowCheckBox then
            Capr.Bottom := Capr.Bottom - 20;
        end
        else
        begin
          CapR := Rect(R.Left + FTabSettings.LeftMargin, R.Top, R.Right, R.Bottom);
          if not ShowCloseOnNonSelectedTabs then
          begin
            if (ButtonSettings.CloseButton and CloseOnTab and (ActiveTabIndex <> TabIndex) and AdvOfficeTabs[TabIndex].ShowClose) and (CloseOnTabPosition = cpLeft) and ((TabSettings.Width <= 0) or (TabSettings.Alignment <> taCenter)) then
              CapR.Left := CapR.Left + ButtonSettings.ButtonSize + 4
            else if (ButtonSettings.CloseButton and CloseOnTab and (ActiveTabIndex = TabIndex) and AdvOfficeTabs[TabIndex].ShowClose) and (CloseOnTabPosition = cpRight) and ((TabSettings.Width <= 0) or (TabSettings.Alignment <> taCenter)) then
              CapR.Right := CapR.Right - (ButtonSettings.ButtonSize + 4)
          end
          else
          begin
//            if (ButtonSettings.CloseButton and CloseOnTab and AdvOfficeTabs[TabIndex].ShowClose) and (CloseOnTabPosition = cpLeft) and ((TabSettings.Width <= 0) or (TabSettings.Alignment <> taCenter)) then
//              CapR.Left := CapR.Left + ButtonSettings.ButtonSize + 4
            if (ButtonSettings.CloseButton and CloseOnTab and AdvOfficeTabs[TabIndex].ShowClose) and (CloseOnTabPosition = cpRight) and ((TabSettings.Width <= 0) or (TabSettings.Alignment <> taCenter)) then
              CapR.Right := CapR.Right - (ButtonSettings.ButtonSize + 4)
          end;

          if AdvOfficeTabs[TabIndex].ShowCheckBox then
            Capr.Left := CapR.Left + 20;
        end;
      end;
      tpRight:
      begin
        if RotateTabLeftRight then
        begin
          CapR := Rect(R.Left, R.Top + FTabSettings.LeftMargin, R.Right, R.Bottom);
          if not ShowCloseOnNonSelectedTabs then
          begin
            if (ButtonSettings.CloseButton and CloseOnTab and (ActiveTabIndex <> TabIndex) and AdvOfficeTabs[TabIndex].ShowClose) then
              CapR.Top := CapR.Top + (ButtonSettings.ButtonSize + 4) div 2
            else if (ButtonSettings.CloseButton and CloseOnTab and (ActiveTabIndex = TabIndex) and AdvOfficeTabs[TabIndex].ShowClose) and (CloseOnTabPosition = cpLeft) and ((TabSettings.Width <= 0) or (TabSettings.Alignment <> taCenter)) then
              CapR.Top := CapR.Top + ButtonSettings.ButtonSize + 4
          end
          else
          begin
            if (ButtonSettings.CloseButton and CloseOnTab and AdvOfficeTabs[TabIndex].ShowClose) then
              CapR.Top := CapR.Top + (ButtonSettings.ButtonSize + 4) div 2
            else if (ButtonSettings.CloseButton and CloseOnTab and AdvOfficeTabs[TabIndex].ShowClose) and (CloseOnTabPosition = cpLeft) and ((TabSettings.Width <= 0) or (TabSettings.Alignment <> taCenter)) then
              CapR.Top := CapR.Top + ButtonSettings.ButtonSize + 4
          end;
          if AdvOfficeTabs[TabIndex].ShowCheckBox then
            Capr.Top := CapR.Top + 20;
        end
        else
        begin
          CapR := Rect(R.Left + FTabSettings.LeftMargin + 3, R.Top, R.Right, R.Bottom);
          if not ShowCloseOnNonSelectedTabs then
          begin
            if (ButtonSettings.CloseButton and CloseOnTab and (ActiveTabIndex = TabIndex) and AdvOfficeTabs[TabIndex].ShowClose) and (CloseOnTabPosition = cpLeft) and ((TabSettings.Width <= 0) or (TabSettings.Alignment <> taCenter)) then
              CapR.Left := CapR.Left + ButtonSettings.ButtonSize + 5
            else if (ButtonSettings.CloseButton and CloseOnTab and (ActiveTabIndex = TabIndex) and AdvOfficeTabs[TabIndex].ShowClose) and (CloseOnTabPosition = cpRight) and ((TabSettings.Width <= 0) or (TabSettings.Alignment <> taCenter)) then
              CapR.Right := CapR.Right - (ButtonSettings.ButtonSize + 4)
          end
          else
          begin
//            if (ButtonSettings.CloseButton and CloseOnTab and AdvOfficeTabs[TabIndex].ShowClose) and (CloseOnTabPosition = cpLeft) and ((TabSettings.Width <= 0) or (TabSettings.Alignment <> taCenter)) then
//              CapR.Left := CapR.Left + ButtonSettings.ButtonSize + 5
            if (ButtonSettings.CloseButton and CloseOnTab and AdvOfficeTabs[TabIndex].ShowClose) and (CloseOnTabPosition = cpRight) and ((TabSettings.Width <= 0) or (TabSettings.Alignment <> taCenter)) then
              CapR.Right := CapR.Right - (ButtonSettings.ButtonSize + 4)
          end;
          if AdvOfficeTabs[TabIndex].ShowCheckBox then
            Capr.Left := CapR.Left + 20;
        end;
      end;
    end;

    if FAdvOfficeTabs.Items[TabIndex].Enabled or FAdvOfficeTabs.Items[TabIndex].DisabledPicture.Empty then
      Pic := FAdvOfficeTabs.Items[TabIndex].Picture
    else
      Pic := FAdvOfficeTabs.Items[TabIndex].DisabledPicture;

    if Assigned(Pic) and not Pic.Empty then
    begin
      Pic.GetImageSizes;
      ImgW := Pic.Width;
      ImgH := Pic.Height;

      ImgY := CapR.Top;
      ImgX := CapR.Left;
      case TabPosition of
        tpTop, tpBottom:
        begin
          case TabSettings.ImagePosition of
            ipTop:
            begin
              ImgX := R.Left + ((R.Right - R.Left) - ImgW) div 2;
              ImgY := CapR.Top;
              CapR.Top := CapR.Top + ImgH{ + ImgTxtSp};
            end;
            ipBottom:
            begin
              ImgX := R.Left + ((R.Right - R.Left) - ImgW) div 2;
              ImgY := CapR.Bottom - ImgH;
              CapR.Bottom := CapR.Bottom - ImgH;
            end;
            ipLeft:
            begin
              if (TabSettings.Width > 0) and (TabSettings.Alignment = taCenter) then
                ImgX := ImgP.X
              else
              ImgX := CapR.Left;
              CapR.Left := CapR.Left + ImgW + ImgTxtSp;
              ImgY := R.Top + ((R.Bottom - R.Top) - ImgH) div 2;
            end;
            ipRight:
            begin
              if (TabSettings.Width > 0) and (TabSettings.Alignment = taCenter) then
                ImgX := ImgP.X
              else
              ImgX := CapR.Right - ImgW - FTabSettings.RightMargin;
              CapR.Right := ImgX {- ImgTxtSp};
              ImgY := R.Top + ((R.Bottom - R.Top) - ImgH) div 2;
            end;
          end;
        end;
        tpLeft:
        begin
          if not RotateTabLeftRight then
          begin
            case TabSettings.ImagePosition of
              ipTop:
              begin
                ImgX := R.Left + ((R.Right - R.Left) - ImgW) div 2;
                ImgY := CapR.Top;
                CapR.Top := CapR.Top + ImgH;
              end;
              ipBottom:
              begin
                ImgX := R.Left + ((R.Right - R.Left) - ImgW) div 2;
                ImgY := CapR.Bottom - ImgH;
                CapR.Bottom := CapR.Bottom - ImgH;
              end;
              ipLeft:
              begin
                if (TabSettings.Width > 0) and (TabSettings.Alignment = taCenter) then
                  ImgX := ImgP.X
                else
                ImgX := CapR.Left;
                CapR.Left := CapR.Left + ImgW + ImgTxtSp;
                ImgY := R.Top + ((R.Bottom - R.Top) - ImgH) div 2;
              end;
              ipRight:
              begin
                if (TabSettings.Width > 0) and (TabSettings.Alignment = taCenter) then
                  ImgX := ImgP.X
                else
                ImgX := CapR.Right - ImgW - FTabSettings.RightMargin;
                CapR.Right := ImgX - ImgTxtSp;
                ImgY := R.Top + ((R.Bottom - R.Top) - ImgH) div 2;
              end;
            end;
          end
          else
          begin
            case TabSettings.ImagePosition of
              ipTop:
              begin
                ImgY := R.Top + ((R.Bottom - R.Top) - ImgH) div 2;
                ImgX := CapR.Left;
                CapR.Left := CapR.Left + ImgW;
              end;
              ipBottom:
              begin
                ImgY := R.Top + ((R.Bottom - R.Top) - ImgH) div 2;
                ImgX := CapR.Right - ImgW;
                CapR.Right := CapR.Right - ImgW;
              end;
              ipLeft:
              begin
                ImgX := R.Left + ((R.Right - R.Left) - ImgW) div 2;
                if (TabSettings.Width > 0) and (TabSettings.Alignment = taCenter) then
                  ImgY := ImgP.Y
                else
                ImgY := CapR.Bottom - ImgH;
                CapR.Bottom := ImgY - ImgTxtSp;
              end;
              ipRight:
              begin
                ImgX := R.Left + ((R.Right - R.Left) - ImgW) div 2;
                if (TabSettings.Width > 0) and (TabSettings.Alignment = taCenter) then
                  ImgY := ImgP.Y
                else
                ImgY := CapR.Top + TabSettings.RightMargin;
                CapR.Top := ImgY + ImgTxtSp;
              end;
            end;
          end;
        end;
        tpRight:
        begin
          if not RotateTabLeftRight then
          begin
            case TabSettings.ImagePosition of
              ipTop:
              begin
                ImgX := R.Left + ((R.Right - R.Left) - ImgW) div 2;
                ImgY := CapR.Top;
                CapR.Top := CapR.Top + ImgH;
              end;
              ipBottom:
              begin
                ImgX := R.Left + ((R.Right - R.Left) - ImgW) div 2;
                ImgY := CapR.Bottom - ImgH;
                CapR.Bottom := CapR.Bottom - ImgH;
              end;
              ipLeft:
              begin
                if (TabSettings.Width > 0) and (TabSettings.Alignment = taCenter) then
                  ImgX := ImgP.X
                else
                ImgX := CapR.Left;
                CapR.Left := CapR.Left + ImgW + ImgTxtSp;
                ImgY := R.Top + ((R.Bottom - R.Top) - ImgH) div 2;
              end;
              ipRight:
              begin
                if (TabSettings.Width > 0) and (TabSettings.Alignment = taCenter) then
                  ImgX := ImgP.X
                else
                ImgX := CapR.Right - ImgW;
                CapR.Right := ImgX - ImgTxtSp;
                ImgY := R.Top + ((R.Bottom - R.Top) - ImgH) div 2;
              end;
            end;
          end
          else
          begin
            case TabSettings.ImagePosition of
              ipTop:
              begin
                ImgY := R.Top + ((R.Bottom - R.Top) - ImgH) div 2;
                ImgX := CapR.Right - ImgW;
                CapR.Right := CapR.Right - ImgW;
              end;
              ipBottom:
              begin
                ImgY := R.Top + ((R.Bottom - R.Top) - ImgH) div 2;
                ImgX := CapR.Left;
                CapR.Left := CapR.Left + ImgW;
              end;
              ipLeft:
              begin
                ImgX := R.Left + ((R.Right - R.Left) - ImgW) div 2;
                if (TabSettings.Width > 0) and (TabSettings.Alignment = taCenter) then
                  ImgY := ImgP.Y
                else
                ImgY := CapR.Top;
                CapR.Top := CapR.Top + ImgH + ImgTxtSp;
              end;
              ipRight:
              begin
                ImgX := R.Left + ((R.Right - R.Left) - ImgW) div 2;
                if (TabSettings.Width > 0) and (TabSettings.Alignment = taCenter) then
                  ImgY := ImgP.Y
                else
                begin
                ImgY := CapR.Bottom - ImgH - TabSettings.RightMargin;

                end;
                CapR.Bottom := ImgY {- ImgTxtSp};
              end;
            end;
          end;
        end;
      end;

      //Canvas.Draw(ImgX, ImgY, Pic);
    end
    else
    if (Assigned(FImages) or Assigned(DisabledImages)) and (FAdvOfficeTabs.Items[TabIndex].ImageIndex >= 0) then
    begin
      ImgEnabled := True;
      if FAdvOfficeTabs.Items[TabIndex].Enabled then
      begin
        if Assigned(FImages) then
          ImgList := FImages;
      end
      else
      begin
        if Assigned(FDisabledImages) then
          ImgList := FDisabledImages
        else if Assigned(FImages) then
        begin
          ImgList := FImages;
          ImgEnabled := False;
        end;
      end;

      if (ImgList <> nil) then
      begin
        ImgY := CapR.Top;
        ImgX := CapR.Left;
        case TabPosition of
          tpTop, tpBottom:
          begin
            case TabSettings.ImagePosition of
              ipTop:
              begin
                ImgX := R.Left + ((R.Right - R.Left) - ImgList.Width) div 2;
                ImgY := CapR.Top;
                CapR.Top := CapR.Top + ImgList.Height{ + ImgTxtSp};
              end;
              ipBottom:
              begin
                ImgX := R.Left + ((R.Right - R.Left) - ImgList.Width) div 2;
                ImgY := CapR.Bottom - ImgList.Height;
                CapR.Bottom := CapR.Bottom - ImgList.Height;
              end;
              ipLeft:
              begin
                if (TabSettings.Width > 0) and (TabSettings.Alignment = taCenter) then
                  ImgX := ImgP.X
                else
                ImgX := CapR.Left;
                CapR.Left := CapR.Left + ImgList.Width + ImgTxtSp;
                ImgY := R.Top + ((R.Bottom - R.Top) - ImgList.Height) div 2;
              end;
              ipRight:
              begin
                if (TabSettings.Width > 0) and (TabSettings.Alignment = taCenter) then
                  ImgX := ImgP.X
                else
                ImgX := CapR.Right - ImgList.Width - FTabSettings.RightMargin;
                CapR.Right := ImgX {- ImgTxtSp};
                ImgY := R.Top + ((R.Bottom - R.Top) - ImgList.Height) div 2;
              end;
            end;
          end;
          tpLeft:
          begin
            if not RotateTabLeftRight then
            begin
              case TabSettings.ImagePosition of
                ipTop:
                begin
                  ImgX := R.Left + ((R.Right - R.Left) - ImgList.Width) div 2;
                  ImgY := CapR.Top;
                  CapR.Top := CapR.Top + ImgList.Height{ + ImgTxtSp};
                end;
                ipBottom:
                begin
                  ImgX := R.Left + ((R.Right - R.Left) - ImgList.Width) div 2;
                  ImgY := CapR.Bottom - ImgList.Height;
                  CapR.Bottom := CapR.Bottom - ImgList.Height;
                end;
                ipLeft:
                begin
                  if (TabSettings.Width > 0) and (TabSettings.Alignment = taCenter) then
                    ImgX := ImgP.X
                  else
                  ImgX := CapR.Left;
                  CapR.Left := CapR.Left + ImgList.Width + ImgTxtSp;
                  ImgY := R.Top + ((R.Bottom - R.Top) - ImgList.Height) div 2;
                end;
                ipRight:
                begin
                  if (TabSettings.Width > 0) and (TabSettings.Alignment = taCenter) then
                    ImgX := ImgP.X
                  else
                  ImgX := CapR.Right - ImgList.Width - FTabSettings.RightMargin;
                  CapR.Right := ImgX - ImgTxtSp;
                  ImgY := R.Top + ((R.Bottom - R.Top) - ImgList.Height) div 2;
                end;
              end;
            end
            else
            begin
              case TabSettings.ImagePosition of
                ipTop:
                begin
                  ImgY := R.Top + ((R.Bottom - R.Top) - ImgList.Height) div 2;
                  ImgX := CapR.Left;
                  CapR.Left := CapR.Left + ImgList.Width;
                end;
                ipBottom:
                begin
                  ImgY := R.Top + ((R.Bottom - R.Top) - ImgList.Height) div 2;
                  ImgX := CapR.Right - ImgList.Width;
                  CapR.Right := CapR.Right - ImgList.Width;
                end;
                ipLeft:
                begin
                  ImgX := R.Left + ((R.Right - R.Left) - ImgList.Width) div 2;
                  if (TabSettings.Width > 0) and (TabSettings.Alignment = taCenter) then
                    ImgY := ImgP.Y
                  else
                  ImgY := CapR.Bottom - ImgList.Height;
                  CapR.Bottom := ImgY - ImgTxtSp;
                end;
                ipRight:
                begin
                  ImgX := R.Left + ((R.Right - R.Left) - ImgList.Width) div 2;
                  if (TabSettings.Width > 0) and (TabSettings.Alignment = taCenter) then
                    ImgY := ImgP.Y
                  else
                  ImgY := CapR.Top + TabSettings.RightMargin;
                  CapR.Top := ImgY + ImgTxtSp;
                end;
              end;
            end;
          end;
          tpRight:
          begin
            if not RotateTabLeftRight then
            begin
              case TabSettings.ImagePosition of
                ipTop:
                begin
                  ImgX := R.Left + ((R.Right - R.Left) - ImgList.Width) div 2;
                  ImgY := CapR.Top;
                  CapR.Top := CapR.Top + ImgList.Height;
                end;
                ipBottom:
                begin
                  ImgX := R.Left + ((R.Right - R.Left) - ImgList.Width) div 2;
                  ImgY := CapR.Bottom - ImgList.Height;
                  CapR.Bottom := CapR.Bottom - ImgList.Height;
                end;
                ipLeft:
                begin
                  if (TabSettings.Width > 0) and (TabSettings.Alignment = taCenter) then
                    ImgX := ImgP.X
                  else
                  ImgX := CapR.Left;
                  CapR.Left := CapR.Left + ImgList.Width + ImgTxtSp;
                  ImgY := R.Top + ((R.Bottom - R.Top) - ImgList.Height) div 2;
                end;
                ipRight:
                begin
                  if (TabSettings.Width > 0) and (TabSettings.Alignment = taCenter) then
                    ImgX := ImgP.X
                  else
                  ImgX := CapR.Right - ImgList.Width;
                  CapR.Right := ImgX - ImgTxtSp;
                  ImgY := R.Top + ((R.Bottom - R.Top) - ImgList.Height) div 2;
                end;
              end;
            end
            else
            begin
              case TabSettings.ImagePosition of
                ipTop:
                begin
                  ImgY := R.Top + ((R.Bottom - R.Top) - ImgList.Height) div 2;
                  ImgX := CapR.Right - ImgList.Width;
                  CapR.Right := CapR.Right - ImgList.Width;
                end;
                ipBottom:
                begin
                  ImgY := R.Top + ((R.Bottom - R.Top) - ImgList.Height) div 2;
                  ImgX := CapR.Left;
                  CapR.Left := CapR.Left + ImgList.Width;
                end;
                ipLeft:
                begin
                  ImgX := R.Left + ((R.Right - R.Left) - ImgList.Width) div 2;
                  if (TabSettings.Width > 0) and (TabSettings.Alignment = taCenter) then
                    ImgY := ImgP.Y
                  else
                  ImgY := CapR.Top;
                  CapR.Top := CapR.Top + ImgList.Height + ImgTxtSp;
                end;
                ipRight:
                begin
                  ImgX := R.Left + ((R.Right - R.Left) - ImgList.Width) div 2;
                  if (TabSettings.Width > 0) and (TabSettings.Alignment = taCenter) then
                    ImgY := ImgP.Y
                  else
                  begin
                  ImgY := CapR.Bottom - ImgList.Height - TabSettings.RightMargin;
                  if (ButtonSettings.CloseButton and CloseOnTab and (ActiveTabIndex = TabIndex) and AdvOfficeTabs[TabIndex].ShowClose) and (CloseOnTabPosition = cpRight) then
                    ImgY := ImgY - ButtonSettings.ButtonSize - 4;
                  end;
                  CapR.Bottom := ImgY {- ImgTxtSp};
                end;
              end;
            end;
          end;
        end;

        //ImgList.Draw(Canvas, ImgX, ImgY, FAdvOfficeTabs.Items[TabIndex].ImageIndex, ImgEnabled);
      end;
    end;

    Canvas.Brush.Style := bsClear;
    if (not RotateTabLeftRight and (TabPosition in [tpLeft, tpRight])) then
    begin
      CapR2 := CapR;
      AAlign := taLeftJustify;
      if (TabSettings.Width <> 0) then
      begin
        case TabSettings.Alignment of
          taLeftJustify: AAlign := taLeftJustify;
          taCenter:
          begin
            AAlign := taLeftJustify;
            CapR2.Left := TR.Left;
          end;
          taRightJustify:
          begin
            AAlign := taRightJustify;
            CapR2.Right := CapR2.Right - 3;
          end;
        end;
      end;
      Canvas.Font.Color := TxtClr;
      TxtR := DrawVistaText(Canvas, AAlign, CapR2, DCaption, Canvas.Font, FAdvOfficeTabs.Items[TabIndex].Enabled, True, aa, tpTop, Ellipsis);
    end
    else
    if (TabPosition = tpLeft) then
    begin
      if (DCaption <> '') then
      begin
        CapR2 := CapR;
        TxtR.Left := CapR.Left + ((CapR.Right - CapR.Left)-Canvas.TextHeight('gh')) div 2;
        TxtR.Top := CapR.Bottom;
        TxtR.Right := TxtR.Left + Canvas.TextHeight('gh');
        TxtR.Bottom := TxtR.Top + Canvas.TextWidth(DCaption);

        if (TabSettings.Width > 0) then
        begin
          case TabSettings.Alignment of
            taCenter: CapR2.Bottom := TR.Bottom;
            taRightJustify: CapR2.Bottom := TR.Bottom;
          end;  
        end;

        // Make sure to use a truetype font!
        // Font.Name := 'Tahoma';
        tf := TFont.Create;
        try
          if (TabPosition = tpLeft) or (TabPosition = tpRight) then
          begin
            FillChar(lf, SizeOf(lf), 0);
            tf.Assign(Canvas.Font);
            GetObject(tf.Handle, SizeOf(Lf), @Lf);

            if TabPosition = tpLeft then
              lf.lfEscapement := -2700
            else
              lf.lfEscapement := -900;

            lf.lfOrientation := 30;

            tf.Handle := CreateFontIndirect(Lf);
            Canvas.Font.Assign(tf);
          end;
        finally
          tf.Free;
        end;

        DCaption := TrimText(DCaption, CapR, False, nil, Canvas, nil, nil, Ellipsis, TabPosition);
        canvas.Font.Color := TxtClr;
        Canvas.TextOut(CapR.Left + ((CapR.Right - CapR.Left)-Canvas.TextHeight('gh')) div 2, CapR2.Bottom, DCaption);
      end;
    end
    else
    if (TabPosition = tpRight) and ((AntiAlias = aaNone) or (TabSettings.Width > 0)) then
    begin
      if (DCaption <> '') then
      begin
        CapR2 := CapR;
        TxtR.Left := CapR.Left + ((CapR.Right - CapR.Left)-Canvas.TextHeight('gh')) div 2;
        TxtR.Top := CapR.Bottom;
        TxtR.Right := TxtR.Left + Canvas.TextHeight('gh');
        TxtR.Bottom := TxtR.Top + Canvas.TextWidth(DCaption);

        if (TabSettings.Width <> 0) then
        begin
          case TabSettings.Alignment of
            taCenter: CapR2.Top := TR.Top;
            taRightJustify: CapR2.Top := TR.Top;
          end;
        end;

        // Make sure to use a truetype font!
        // Font.Name := 'Tahoma';

        tf := TFont.Create;
        try
          if (TabPosition = tpLeft) or (TabPosition = tpRight) then
          begin
            FillChar(lf, SizeOf(lf), 0);
            tf.Assign(Canvas.Font);
            GetObject(tf.Handle, SizeOf(Lf), @Lf);

            if TabPosition = tpLeft then lf.lfEscapement := -900
            else lf.lfEscapement := -900;
            lf.lfOrientation := 30;

            tf.Handle := CreateFontIndirect(Lf);
            Canvas.Font.Assign(tf);
          end;
        finally
          tf.Free;
        end;
        DCaption := TrimText(DCaption, CapR, False, nil, Canvas, nil, nil, Ellipsis, TabPosition);
        canvas.Font.Color := TxtClr;
        Canvas.TextOut(CapR.Right - ((CapR.Right - CapR.Left)-Canvas.TextHeight('gh')) div 2, CapR2.Top, DCaption);
      end;
    end
    else
    begin
      CapR2 := CapR;
      AAlign := taLeftJustify;
      if (TabSettings.Width <> 0) then
      begin
        case TabSettings.Alignment of
          taLeftJustify:
          begin
            AAlign := taLeftJustify;
          end;
          taCenter:
          begin
            AAlign := taLeftJustify;
            CapR2.Left := TR.Left;
          end;
          taRightJustify:
          begin
            AAlign := taRightJustify;
            CapR2.Right := CapR2.Right - 3;
            if (TabSettings.Shape in [tsRightRamp, tsLeftRightRamp]) then
              CapR2.Right := CapR2.Right - GetRightRoundingOffset;
          end;
        end;
      end;

      Canvas.Font.Color := TxtClr;
      TxtR := DrawVistaText(Canvas, AAlign, CapR2, DCaption, Canvas.Font, FAdvOfficeTabs.Items[TabIndex].Enabled, True, aa, TabPosition, Ellipsis);
      //DrawText(Canvas.Handle, PChar(DCaption), Length(DCaption), R, DT_SINGLELINE or DT_VCENTER);
    end;

    if Assigned(Pic) and not Pic.Empty then
    begin
      case TabPosition of
        tpTop, tpBottom:
        begin
          case TabSettings.ImagePosition of
            ipTop: ImgY := Max(TxtR.Top - ImgH{ - ImgTxtSp}, 4);
            ipBottom: ImgY := Min(TxtR.Bottom {+ ImgTxtSp}, CapR.Bottom);
          end;
        end;
        tpLeft:
        begin
          if not RotateTabLeftRight then
          begin
            case TabSettings.ImagePosition of
              ipTop: ImgY := Max(TxtR.Top - ImgH - ImgTxtSp, 4);
              ipBottom: ImgY := Max(TxtR.Bottom + ImgTxtSp, 4);
              ipRight:
              begin
                if (TabSettings.Width <= 0) then
                  ImgX := TxtR.Right + ImgTxtSp*2;
              end;  
            end;
          end
          else
          begin
            case TabSettings.ImagePosition of
              ipTop: ImgX := Max(TxtR.Left - ImgW - ImgTxtSp, 4);
              ipBottom: ImgX := Max(TxtR.Right + ImgTxtSp, 4);
            end;
          end;
        end;
        tpRight:
        begin
          if not RotateTabLeftRight then
          begin
            case TabSettings.ImagePosition of
              ipTop: ImgY := Max(TxtR.Top - ImgH - ImgTxtSp, 4);
              ipBottom: ImgY := Max(TxtR.Bottom + ImgTxtSp, 4);
              ipRight:
              begin
                if (TabSettings.Width <= 0) then
                  ImgX := TxtR.Right + ImgTxtSp*2;
              end;
            end;
          end
          else
          begin
            case TabSettings.ImagePosition of
              ipTop: ImgX := Max(TxtR.Right + ImgTxtSp, 4);
              ipBottom: ImgX := Max(TxtR.Left - ImgW - ImgTxtSp, 4);
            end;
          end;
        end;
      end;

      if IsGlass then
        DrawGDIPImage(nil, Canvas, Point(ImgX, ImgY), Pic, True)
      else
        Canvas.Draw(ImgX, ImgY, Pic);
    end
    else
    if (Assigned(FImages) or Assigned(DisabledImages)) and (FAdvOfficeTabs.Items[TabIndex].ImageIndex >= 0) then
    begin
      if (ImgList <> nil) then
      begin
        case TabPosition of
          tpTop, tpBottom:
          begin
            case TabSettings.ImagePosition of
              ipTop: ImgY := Max(TxtR.Top - ImgList.Height{ - ImgTxtSp}, 4);
              ipBottom: ImgY := Min(TxtR.Bottom {+ ImgTxtSp}, CapR.Bottom);
            end;
          end;
          tpLeft:
          begin
            if not RotateTabLeftRight then
            begin
              case TabSettings.ImagePosition of
                ipTop: ImgY := Max(TxtR.Top - ImgList.Height - ImgTxtSp, 4);
                ipBottom: ImgY := Max(TxtR.Bottom + ImgTxtSp, 4);
                ipRight:
                begin
                  if (TabSettings.Width <= 0) then
                    ImgX := TxtR.Right + ImgTxtSp*2;
                end;    
              end;
            end
            else
            begin
              case TabSettings.ImagePosition of
                ipTop: ImgX := Max(TxtR.Left - ImgList.Width - ImgTxtSp, 4);
                ipBottom: ImgX := Max(TxtR.Right + ImgTxtSp, 4);
              end;
            end;
          end;
          tpRight:
          begin
            if not RotateTabLeftRight then
            begin
              case TabSettings.ImagePosition of
                ipTop: ImgY := Max(TxtR.Top - ImgList.Height - ImgTxtSp, 4);
                ipBottom: ImgY := Max(TxtR.Bottom + ImgTxtSp, 4);
                ipRight:
                begin
                  if (TabSettings.Width <= 0) then
                    ImgX := TxtR.Right + ImgTxtSp*2;
                end;
              end;
            end
            else
            begin
              case TabSettings.ImagePosition of
                ipTop: ImgX := Max(TxtR.Right + ImgTxtSp, 4);
                ipBottom: ImgX := Max(TxtR.Left - ImgList.Width - ImgTxtSp, 4);
              end;
            end;
          end;
        end;

        if IsGlass then
          DrawGDIPImageFromImageList(nil, Canvas, Point(ImgX, ImgY), ImgList, FAdvOfficeTabs.Items[TabIndex].ImageIndex, ImgEnabled)
        else
          ImgList.Draw(Canvas, ImgX, ImgY, FAdvOfficeTabs.Items[TabIndex].ImageIndex, ImgEnabled);
      end;
    end;

    if (TabIndex <> ActiveTabIndex) and (IsActiveTabNeighbour(TabIndex) <> 0) and (ActiveTabIndex >= 0) then
      DrawTab(ActiveTabIndex);

  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTabSet.DrawAllTabs;
var
  i, j, s: Integer;
  R: TRect;
  Tail: Boolean;
  TabAppearance: TTabAppearance;
begin
  // Draw TabBackGround

  R := GetTabsArea;

  if Assigned(FTabAppearance) and FItones then
    TabAppearance := FTabAppearance
  else
    TabAppearance := FCurrentOfficeTabSetStyler.TabAppearance;

  case TabPosition of
    tpTop: R.Bottom := ClientRect.Bottom;
    tpBottom: R.Top := ClientRect.Top;
    tpLeft: R.Right := ClientRect.Right;
    tpRight: R.Left := ClientRect.Left;
  end;

  if not FTransparent then
  begin
    with TabAppearance do
    begin
      if (BackGround.Color <> clNone) and (BackGround.ColorTo <> clNone) then
        DrawGradient(Canvas, BackGround.Color, BackGround.ColorTo, BackGround.Steps, R, BackGround.Direction = gdHorizontal)
      else if (BackGround.Color <> clNone) then
      begin
        Canvas.Brush.Color := BackGround.Color;
        Canvas.Pen.Color := BackGround.Color;
        Canvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
      end;
    end;
  end;

  if ((FixedTabs > 0) and not (csDesigning in ComponentState)) or MultiLine then
  begin
    for i := 0 to FAdvOfficeTabs.Count - 1 do
    begin
      DrawTab(i);
    end;
  end
  else
  begin
    j := 0;
    s := FTabScroller.Position;
    for i := 0 to FAdvOfficeTabs.Count - 1 do
    begin
      if j >= FTabScroller.Position then
      begin
        s := i;
        Break;
      end;

      if AdvOfficeTabs[i].Visible then
        Inc(j);
    end;

    Tail := False;
    for i := s to FAdvOfficeTabs.Count - 1 do
    begin
      if not DrawTab(i) and Tail then
        break;

      if AdvOfficeTabs[i].Visible then
        Tail := True;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTabSet.DrawInsertButton;
begin
  if not ButtonSettings.ShowInsertButton then
    Exit;

  DrawInsertButton(Canvas, GetInsertButtonRect);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTabSet.DrawInsertButton(Canvas: TCanvas; R: TRect);
var
  g: TGPGraphics;
  btnpath: TGPGraphicsPath;
  Pen: TGPPen;
  brClr, Clr, PBrClr, PClr: Cardinal;
  br: TGPSolidBrush;
  l, w: Integer;
  P1, P2, P3, P4: TPoint;
  Shape: TAdvTabShape;
begin
  if not Assigned(Canvas) then
    Exit;

  g := TGPGraphics.Create(Canvas.Handle);
  g.SetSmoothingMode(SmoothingModeAntiAlias);

  if FInsertButtonDown then
  begin
    Clr := MakeColor(25, 10, 10, 10);
    BrClr := MakeColor(170, 50, 79, 83);

    //PClr := MakeColor(230, 168, 188, 199);
    PClr := MakeColor(130, 255, 255, 255);
    PBrClr := MakeColor(180, 52, 87, 120);
  end
  else
  begin
    if FInsertButtonHot then
    begin
      Clr := MakeColor(70, clWhite);

      PClr := MakeColor(230, 217, 226, 231);
      PBrClr := MakeColor(240, 37, 70, 112);
    end
    else
    begin
      Clr := MakeColor(40, clWhite);

      //PClr := MakeColor(230, 196, 209, 216);
      PClr := MakeColor(180, 255, 255, 255);
      PBrClr := MakeColor(200, 52, 83, 121);
    end;
    BrClr := MakeColor(130, 41, 69, 77);
  end;

  if (TabSettings.Shape in [tsRightRamp, tsLeftRightRamp]) then
    Shape := tsLeftRightRamp
  else
    Shape := tsRectangle;

  case TabPosition of
    tpLeft:
    begin
      if (TabSettings.Shape in [tsLeftRamp, tsLeftRightRamp]) then
        Shape := tsLeftRightRamp
      else
        Shape := tsRectangle;
    end;
    tpRight:
    begin
      if (TabSettings.Shape in [tsRightRamp, tsLeftRightRamp]) then
        Shape := tsLeftRightRamp
      else
        Shape := tsRectangle;
    end;
  end;

  //--- body
  btnpath := GetInsertButtonPath(R, Shape, Max(TabSettings.Rounding, 1), RotateTabLeftRight, TabPosition);
  br := TGPSolidBrush.Create(Clr);
  g.FillPath(br, btnPath);
  br.Free;

  //--- border
  Pen := TGPPEn.Create(BrClr, 1.5);
  g.DrawPath(Pen, btnPath);
  Pen.Free;

  //--- Draw Plus sign
  g.SetSmoothingMode(SmoothingModeDefault);
  l := Min(R.Right - R.Left, R.Bottom - R.Top);
  w := Max(l div 4, 4);
  l := l - 6;
  if ((l mod 2) = 1) then
    l := l + 1;

  // -
  P1.X := R.Left + 1 + ((R.Right - R.Left) - l) div 2;
  P1.Y := R.Top + 1 + ((R.Bottom - R.Top)) div 2;
  P2.X := P1.X + l;
  P2.Y := P1.Y;

  // |
  P3.X := R.Left + 1 + (R.Right - R.Left) div 2;
  P3.Y := R.Top + 1 + ((R.Bottom - R.Top) - l) div 2;
  P4.X := P3.X;
  P4.Y := P3.Y + l;

  Pen := TGPPEn.Create(PBrClr, w);
  g.DrawLine(Pen,P1.X, P1.Y, P2.X, P2.Y);    // -
  g.DrawLine(Pen,P3.X, P3.Y, P4.X, P4.Y);    // |
  Pen.Free;

  Pen := TGPPEn.Create(PClr, w-2);
  g.DrawLine(Pen,P1.X+1, P1.Y, P2.X-1, P2.Y);   // -
  g.DrawLine(Pen,P3.X, P3.Y+1, P4.X, P4.Y-1);   // |
  Pen.Free;

  g.Free;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTabSet.Paint;
var
  DC: integer;
  P: TPoint;
  //TabRect: TRect;
begin
  {if FIsAeroVista then
  begin
    Canvas.Brush.Color := clBlack;
    Canvas.Brush.Style := bsSolid;
    TabRect := ClientRect;
    Canvas.FillRect(TabRect);
  end
  else }
  begin
    if FTransparent then
    begin
      DC := SaveDC(Canvas.Handle);
      P := ClientOrigin;
      Windows.ScreenToClient(Parent.Handle, P);
      P.x := -P.x;
      P.y := -P.y;
      MoveWindowOrg(Canvas.Handle, P.x, P.y);
      SendMessage(Parent.Handle, WM_ERASEBKGND, Canvas.Handle, 0);
      // transparency ?
      SendMessage(Parent.Handle, WM_PAINT, Canvas.Handle, 0);
      if (Parent is TWinCtrl) then
        (Parent as TWinCtrl).PaintCtrls(Canvas.Handle, nil);
      RestoreDC(Canvas.Handle, DC);
    end
    else
    begin
      inherited;
    end;
  end;

  DrawAllTabs;
  DrawInsertButton;
  DrawTabScrollButtons;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTabSet.SetOfficeTabSetStyler(
  const Value: TCustomAdvOfficeTabSetStyler);
begin
  if (FOfficeTabSetStyler <> Value) or (Value = nil) then
  begin
    if Assigned(FOfficeTabSetStyler) and (FOfficeTabSetStyler <> FInternalOfficeTabSetStyler) then
      FOfficeTabSetStyler.RemoveControl(self);

    FOfficeTabSetStyler := Value;

    if FOfficeTabSetStyler = nil then
    begin
      FCurrentOfficeTabSetStyler := FInternalOfficeTabSetStyler;
    end
    else
    begin
      FCurrentOfficeTabSetStyler := FOfficeTabSetStyler;
      FOfficeTabSetStyler.AddControl(self);
    end;

    UpdateMe(0);

    if Assigned(FOfficeTabSetStyler) then
      FOfficeTabSetStyler.InitColorTones;

    PopupMenu := PopupMenu;   // Refresh Styler
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTabSet.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;

end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTabSet.SetChildOrder(Child: TComponent; Order: Integer);
begin
  inherited SetChildOrder(Child, Order);
end;

//------------------------------------------------------------------------------


procedure TAdvCustomOfficeTabSet.WMSize(var Message: TWMSize);
begin
  inherited;
  UpdateTabScroller;
  if ButtonSettings.ClosedListButton then
    UpdateClosedListButton;
  if MultiLine then
    UpdateMultiLineTabs;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTabSet.SetPopupMenuEx(const Value: TPopupMenu);
begin
  Inherited PopupMenu := Value;
  (*if Assigned(PopupMenu) and (PopupMenu is TAdvPopupMenu) and Assigned(FCurrentOfficeTabSetStyler) then
    TAdvPopupMenu(PopupMenu).MenuStyler := FCurrentOfficeTabSetStyler.CurrentAdvMenuStyler; *)
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTabSet.CMShowingChanged(var Message: TMessage);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTabSet.CMVisibleChanged(var Message: TMessage);
begin
  inherited;
end;

//------------------------------------------------------------------------------

function TAdvCustomOfficeTabSet.GetPopupMenuEx: TPopupMenu;
begin
  Result := Inherited PopupMenu;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTabSet.SetSettings(const Value: string);
var
  sl: TStringList;
  i,idx,e,k: integer;
  s: string;
  pi: integer;
  idf: char;
  il: TList;
  page: TOfficeTabCollectionItem;

begin
  // reset default sequence
  ResetTabs;
  pi := 0;

  sl := TStringList.Create;
  il := TList.Create;

  try
    sl.CommaText := Value;

    for i := 0 to sl.Count - 1 do
    begin
      s := sl.Strings[i];

      if length(s) > 0 then
      begin
        idf := s[1];
        delete(s,1,2);

        case idf of
        'P': val(s,pi,e);
        'V','H':
          begin
            val(s,idx,e);
            TabAtTabIndex(idx).Visible := idf = 'V';
            if TabAtTabIndex(idx).Visible then
              il.Add(pointer(idx));
          end;
        'F':
          begin
            val(s,idx,e);
            SetFloating(TabAtTabIndex(idx).Index);
          end;
        end;
      end;
    end;

    for i := 0 to il.Count - 1 do
    begin
      k := Integer(il.Items[i]);
      page := TabAtTabIndex(k);
      AdvOfficeTabs.Move(page.Index,i);
    end;

    il.Clear;

  finally
    il.Free;
    sl.Free;
  end;

  ActiveTabIndex := pi;
end;

procedure TAdvCustomOfficeTabSet.SetShowCloseOnNonSelectedTabs(
  const Value: Boolean);
begin
  if FShowCloseOnNonSelectedTabs <> value then
  begin
    FShowCloseOnNonSelectedTabs := Value;
    UpdateTabScroller;
    UpdateMultiLineTabs;
    InvalidateTab(-1);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTabSet.SetShowNonSelectedTabs(const Value: Boolean);
begin
  FShowNonSelectedTabs := Value;
  InvalidateTab(-1);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTabSet.CMControlChange(var Message: TCMControlChange);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTabSet.CMControlListChange(
  var Message: TCMControlListChange);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTabSet.CMMouseEnter(var Msg: TMessage);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTabSet.CMMouseLeave(var Message: TMessage);
var
  P: TPoint;
  R: TRect;
  i: Integer;
begin
  inherited;

  {if FScrollerHoverLeftBtn or FScrollerHoverRightBtn then
  begin
    FScrollerHoverLeftBtn := false;
    FScrollerHoverRightBtn := false;
    DrawTabScrollButtons;
  end;
  }
  FHintTabIndex := -1;

  if (csDesigning in ComponentState) then
    Exit;

  // work around to avoid false call  
  GetCursorPos(P);
  P := ScreenToClient(P);
  R := GetTabsRect;
  case (TabPosition) of
    tpTop: R.Bottom := R.Bottom - 4;
    tpBottom: R.Top := R.Top + 4;
    tpLeft: R.Right := R.Right - 4;
    tpRight: R.Left := R.Left + 4;
  end;

  if PtInRect(R, P) then
    Exit;

  if (FHotTabIndex = FActiveTabIndex) then
  begin
    i := FHotTabIndex;
    FHotTabIndex := -1;
    InvalidateTab(i);
  end
  else if (FHotTabIndex >= 0) then
  begin
    if (FHotTabIndex < FAdvOfficeTabs.Count) then
    begin
      if not Assigned(FAdvOfficeTabs.Items[FHotTabIndex].FTimer) and CanGlow then
      begin
        FAdvOfficeTabs.Items[FHotTabIndex].FTimer := TTimer.Create(self);
        FAdvOfficeTabs.Items[FHotTabIndex].FTimer.OnTimer := FAdvOfficeTabs.Items[FHotTabIndex].TimerProc;
        FAdvOfficeTabs.Items[FHotTabIndex].FTimer.Interval := GlowSpeed;
        FAdvOfficeTabs.Items[FHotTabIndex].FTimer.Enabled := true;
      end;

      FAdvOfficeTabs.Items[FHotTabIndex].FTimeInc := -20;
      FAdvOfficeTabs.Items[FHotTabIndex].FGlowState := gsHover;
    end;
    i := FHotTabIndex;
    FHotTabIndex := -1;
    InvalidateTab(i);

    if Assigned(FOnTabMouseLeave) then
      FOnTabMouseLeave(Self, i);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTabSet.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
  Tab, OldSelTab: Integer;
begin
  inherited;
  P := Point(X, Y);

  FDownTabIndex := -1;

  if PtInRect(GetTabsArea, p) then
  begin
    Tab := PTOnTab(X, Y);
    if (Tab >= 0) then
    begin
    
      if (Button = mbLeft) then
      begin
        if Assigned(FOnTabClick) then
          FOnTabClick(Self, Tab);
      end
      else if (Button = mbRight) then
      begin
        if Assigned(FOnTabRightClick) then
          FOnTabRightClick(Self, Tab);
      end;
    
      if (Tab <> ActiveTabIndex) and FAdvOfficeTabs.Items[Tab].Enabled then
      begin
        // Select Tab
        OldSelTab := ActiveTabIndex;
        ChangeActiveTab(Tab);
        if not (csDesigning in ComponentState) then
        begin
          if not Assigned(FAdvOfficeTabs.Items[Tab].FTimer) and CanGlow then
          begin
            FAdvOfficeTabs.Items[Tab].FTimer := TTimer.Create(self);
            FAdvOfficeTabs.Items[Tab].FTimer.OnTimer := FAdvOfficeTabs.Items[Tab].TimerProc;
            FAdvOfficeTabs.Items[Tab].FTimer.Interval := GlowSpeed;
            FAdvOfficeTabs.Items[Tab].FTimer.Enabled := true;
          end;
          FAdvOfficeTabs.Items[Tab].FTimeInc := +20;
          FAdvOfficeTabs.Items[Tab].FGlowState := gsPush;
        end;
        //Invalidate;
        InvalidateTab(OldSelTab);        
        if not CanGlow then
          InvalidateTab(Tab);
      end
      else
      begin
        FDownTabIndex := Tab;
        InvalidateTab(-1);
      end;

      if (Button = mbLeft) then
      begin
        if FTabRearrange and ((csDesigning in ComponentState) or not AllowTabUndock or AdvOfficeTabs[Tab].Locked) then
        begin
          BeginDrag(false,6);

          if not (csDesigning in ComponentState) then
            CreateDropArrow;
        end
        else if not FDraging and AllowTabUndock and not AdvOfficeTabs[Tab].Locked then
        begin
          P := ClientToScreen(P);
          BeginMove(Shift, P.X, P.Y, AdvOfficeTabs[Tab]);
        end;
      end;
        
    end
    else
    begin
      {if PtOnTabScrollLeftBtn(X, Y) then
      begin
        FScrollerDownLeftBtn := true;
        DrawTabScrollBtnLeft;
      end;
      if PtOnTabScrollRightBtn(X, Y) then
      begin
        FScrollerDownRightBtn := true;
        DrawTabScrollBtnRight;
      end;  }

      if PtOnInsertButton(X, Y) then
      begin
        if not FInsertButtonDown then
        begin
          FInsertButtonDown := true;
          InvalidateInsertButton;
        end;
      end
      else if FInsertButtonDown then
      begin
        FInsertButtonDown := false;
        InvalidateInsertButton;
      end;

    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTabSet.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
  Tab: Integer;
  OnInsert: boolean;

begin
  inherited;

  if (csDesigning in ComponentState) then
    Exit;

  P := Point(X, Y);

  OnInsert := PtOnInsertButton(X,Y);

  if PtInRect(GetTabsArea, p) then
  begin
    Tab := PTOnTab(X, Y);
    if (Tab >= 0) and (Tab <> FHotTabIndex) then
    begin
      if (FDownTabIndex >= 0) then
      begin
        FDownTabIndex := -1;
        InvalidateTab(-1);
      end;

      if (FHotTabIndex >= 0) then
      begin
        OnExitTab(FHotTabIndex);
        begin
          if (FHotTabIndex < FAdvOfficeTabs.Count) then
          begin
            if not Assigned(FAdvOfficeTabs.Items[FHotTabIndex].FTimer) and CanGlow then
            begin
              FAdvOfficeTabs.Items[FHotTabIndex].FTimer := TTimer.Create(self);
              FAdvOfficeTabs.Items[FHotTabIndex].FTimer.OnTimer := FAdvOfficeTabs.Items[FHotTabIndex].TimerProc;
              FAdvOfficeTabs.Items[FHotTabIndex].FTimer.Interval := GlowSpeed;
              FAdvOfficeTabs.Items[FHotTabIndex].FTimer.Enabled := true;
            end;
            FAdvOfficeTabs.Items[FHotTabIndex].FTimeInc := -20;

            FAdvOfficeTabs.Items[FHotTabIndex].FGlowState := gsHover;
          end;
          FHotTabIndex := -1;
          InvalidateTab(-1);
        end;
      end;

      // Hot Tab
      OnEnterTab(Tab);

      //InvalidateTab(-1);
      //if (Tab <> FActiveTabIndex) then
      if FAdvOfficeTabs.Items[Tab].Enabled then
      begin
        FHotTabIndex := Tab;
        FOldHotTabIndex := FHotTabIndex;
        if not Assigned(FAdvOfficeTabs.Items[FHotTabIndex].FTimer) and CanGlow then
        begin
          FAdvOfficeTabs.Items[FHotTabIndex].FTimer := TTimer.Create(self);
          FAdvOfficeTabs.Items[FHotTabIndex].FTimer.OnTimer := FAdvOfficeTabs.Items[FHotTabIndex].TimerProc;
          FAdvOfficeTabs.Items[FHotTabIndex].FTimer.Interval := GlowSpeed;
          FAdvOfficeTabs.Items[FHotTabIndex].FTimer.Enabled := true;
        end;

        FAdvOfficeTabs.Items[FHotTabIndex].FTimeInc := 20;
        InvalidateTab(FHotTabIndex);// Invalidate;
        FAdvOfficeTabs.Items[FHotTabIndex].FGlowState := gsHover;
      end;

      if (FHintTabIndex <> Tab) and not OnInsert then
      begin
        FHintTabIndex := Tab;
        Application.CancelHint;
      end;
    end
    else if (Tab < 0) and (FHotTabIndex >= 0) then
    begin
      if (FDownTabIndex >= 0) then
      begin
        FDownTabIndex := -1;
        InvalidateTab(-1);
      end;
      OnExitTab(FHotTabIndex);
      if (FHotTabIndex = FActiveTabIndex) and false then
      begin
        FHotTabIndex := -1;
        Invalidate;
      end
      else
      begin
        if (FHotTabIndex < FAdvOfficeTabs.Count) then
        begin
          if not Assigned(FAdvOfficeTabs.Items[FHotTabIndex].FTimer) and CanGlow then
          begin
            FAdvOfficeTabs.Items[FHotTabIndex].FTimer := TTimer.Create(self);
            FAdvOfficeTabs.Items[FHotTabIndex].FTimer.OnTimer := FAdvOfficeTabs.Items[FHotTabIndex].TimerProc;
            FAdvOfficeTabs.Items[FHotTabIndex].FTimer.Interval := GlowSpeed;
            FAdvOfficeTabs.Items[FHotTabIndex].FTimer.Enabled := true;
          end;
          FAdvOfficeTabs.Items[FHotTabIndex].FTimeInc := -20;

          FAdvOfficeTabs.Items[FHotTabIndex].FGlowState := gsHover;
        end;
        FHotTabIndex := -1;
        InvalidateTab(-1);
      end;
    end;

    if (Tab < 0) and not OnInsert then
    begin
      FHintTabIndex := -1;
      Application.CancelHint;
    end;

    if (Tab < 0) and PtOnInsertButton(X, Y) then
    begin
      if not FInsertButtonHot then
      begin
        FInsertButtonHot := true;
        InvalidateInsertButton;
      end;
    end
    else if FInsertButtonHot then
    begin
      FInsertButtonHot := false;
      InvalidateInsertButton;
    end;

    {// Checking for TabScrollButtons
    if PtOnTabScrollLeftBtn(X, Y) then
    begin
      if not FScrollerHoverLeftBtn then
      begin
        FScrollerHoverLeftBtn := true;
        DrawTabScrollBtnLeft;
      end;
    end
    else if FScrollerHoverLeftBtn then
    begin
      FScrollerHoverLeftBtn := false;
      DrawTabScrollBtnLeft;
    end;

    if PtOnTabScrollRightBtn(X, Y) then
    begin
      if not FScrollerHoverRightBtn then
      begin
        FScrollerHoverRightBtn := true;
        DrawTabScrollBtnRight;
      end;
    end
    else if FScrollerHoverRightBtn then
    begin
      FScrollerHoverRightBtn := false;
      DrawTabScrollBtnRight;
    end;
    }
  end
  else
  begin
    if not CanGlow and (FHotTabIndex >= 0) and (FHotTabIndex < FAdvOfficeTabs.Count) then
    begin
      Tab := FHotTabIndex;
      FHotTabIndex := -1;
      InvalidateTab(Tab);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTabSet.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
  Tab: integer;
begin
  inherited;
  P := Point(X, Y);

  if not (csDesigning in ComponentState) and Assigned(FArrow) then
    FArrow.visible := False;

  if (FDownTabIndex >= 0) then
  begin
    FDownTabIndex := -1;
    InvalidateTab(-1);
  end;

  if FInsertButtonDown then
  begin
    FInsertButtonDown := False;
    InvalidateInsertButton;
    if PtOnInsertButton(X, Y) then
    begin
      DoInsertButtonClick;
    end;
  end;

  if PtInRect(GetTabsArea, p) then
  begin
    Tab := PTOnTab(X, Y);
    if (Tab >= 0) then
    begin
      if PTOnCheckBox(Tab, X, Y) then
      begin
        AdvOfficeTabs[Tab].Checked := not AdvOfficeTabs[Tab].Checked;
        if Assigned(FOnTabCheckBoxClick) then
          FOnTabCheckBoxClick(Self, Tab);
        InvalidateTab(Tab);
      end;
    end;
    {if PtOnTabScrollLeftBtn(X, Y) then
    begin
      FScrollerDownLeftBtn := false;
      DrawTabScrollBtnLeft;
      ScrollLeftBtnClick;
    end
    else if PtOnTabScrollRightBtn(X, Y) then
    begin
      FScrollerDownRightBtn := false;
      DrawTabScrollBtnRight;
      ScrollRightBtnClick;
    end; }
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTabSet.BeginMove(Shift: TShiftState; X, Y: Integer; Tab: TOfficeTabCollectionItem);
var
  Msg: TMsg;
  CurP: TPoint;
begin
  if (csDesigning in ComponentState) or not AllowTabUndock or not Assigned(Tab) then
    Exit;

  FOldMouseX := X;
  FOldMouseY := Y;
  FDragingTab := Tab;
  SetCapture(Handle);
  FDraging := true;
  Tab.FDraggingAccept := False;

  while GetCapture = Handle do
  begin
    case integer(GetMessage(Msg, 0, 0, 0)) of
      -1: break;
      0:
        begin
          PostQuitMessage(Msg.WParam);
          Break;
        end;
    end;

    case Msg.message of
      WM_MOUSEMOVE:
        begin
          GetCursorPos(CurP);
          Move(Shift, curP.X, Curp.Y);
        end;
      WM_LBUTTONUP:
        begin
          EndMove;
          break;
        end;
    else
      TranslateMessage(Msg);
      DispatchMessage(Msg);
    end;
  end;
  EndMove;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTabSet.Move(Shift: TShiftState; X, Y: Integer);
var
  ax, ay: integer;
  P, CP: TPoint;
  ExceedBounds: Boolean;
  R: TRect;
  ATab: TOfficeTabCollectionItem;
begin
  if FDraging and Assigned(FDragingTab) then
  begin
    FDragingTab.FDraggingAccept := False;
    ExceedBounds := false;
    P := Point(X, Y);
    if FDragingTab.FFloating then
    begin
      EndMove;
    end
    else
    begin
      R := GetTabsRect;
      CP := ScreenToClient(P);
      ay := CP.Y;
      ax := CP.X;
      if ((TabPosition in [tpTop, tpBottom]) and ((ay >= R.Bottom + 10) or (ay < R.Top - 10) or (ax < R.Left - 100) or (ax > R.Right + 100))) or
         ((TabPosition in [tpLeft, tpRight]) and ((ax >= R.Right + 10) or (ax < R.Left - 10) or (ay < R.Top - 100) or (ay > R.Bottom + 100))) then
      begin
        ExceedBounds := True;
        if Assigned(FArrow) then
          DragCanceled;
      end
      else
      begin
        if TabRearrange and PtInRect(R, CP) then
        begin
          if ((TabPosition in [tpTop, tpBottom]) and ((P.X > FOldMouseX + 5) or (P.X < FOldMouseX - 5))) or
             ((TabPosition in [tpLeft, tpRight]) and ((P.Y > FOldMouseY + 5) or (P.Y < FOldMouseY - 5))) then
          begin
            CreateDropArrow;
            FDragingTab.FDraggingAccept := True;
            DragOver(Self, CP.X, CP.Y, dsDragMove, FDragingTab.FDraggingAccept);
          end;
        end;
      end;

      if ExceedBounds then
      begin
        if Assigned(FOnTabUnDock) then
        begin
          if not Assigned(FFloatingTabs) then
            FFloatingTabs := TDbgList.Create;

          FFloatingTabs.Add(FDragingTab);

          if Assigned(FDragingTab.FTimer) then
          begin
            FDragingTab.FTimer.Enabled := False;
            FreeAndNil(FDragingTab.FTimer);
          end;

          SelectNextTab(False);

          if Assigned(FDragingTab.FCloseButton) then
          begin
            FDragingTab.FCloseButton.Free;
            FDragingTab.FCloseButton := nil;
          end;
          if Assigned(FDragingTab.FCloseButtonChrome) then
          begin
            FDragingTab.FCloseButtonChrome.Free;
            FDragingTab.FCloseButtonChrome := nil;
          end;

          FDragingTab.Collection := nil;
          ATab := FDragingTab;
          EndMove;
          Invalidate;
          FOnTabUnDock(Self, ATab);
        end;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTabSet.EndMove;
var
  P: TPoint;
begin
  FDraging := false;
  if GetCapture = Handle then
    ReleaseCapture;

  if Assigned(FDragingTab) then
  begin
    if (TabRearrange) and FDragingTab.FDraggingAccept then
    begin
      GetCursorPos(P);
      P := ScreenToClient(P);
      DragDrop(Self, P.X, P.Y);
    end;

    FDragingTab.FFloating := False;
    FDragingTab := nil;
  end;
end;

//------------------------------------------------------------------------------

function TAdvCustomOfficeTabSet.FindNextTab(TabIndex: Integer; GoForward,
  CheckTabVisible: Boolean): Integer;
var
  i, j: Integer;
begin
  Result := -1;
  if (TabIndex >= 0) and (TabIndex < FAdvOfficeTabs.Count) then
    Exit;
    
  if GoForward then
  begin
    i := TabIndex;
    j := 1;
    while (j < FAdvOfficeTabs.Count) do
    begin
      Inc(i);    
      if (i >= FAdvOfficeTabs.Count) then
        i := 0;
      if (CheckTabVisible and FAdvOfficeTabs.Items[i].Visible) or not CheckTabVisible then
      begin
        Result := i;
        Break;
      end;
      Inc(j);
    end;
  end
  else  // BackWard
  begin
    i := TabIndex;
    j := 1;
    while (j < FAdvOfficeTabs.Count) do
    begin
      dec(i);
      if (i >= FAdvOfficeTabs.Count) then
        i := 0;
      if (i < 0) then
        i := FAdvOfficeTabs.Count-1;
      if (CheckTabVisible and FAdvOfficeTabs.Items[i].Visible) or not CheckTabVisible then
      begin
        Result := i;
        Break;
      end;
      Inc(j);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTabSet.SetAllowTabUndock(const Value: Boolean);
begin
  FAllowTabUndock := Value;
end;

//------------------------------------------------------------------------------

function TAdvCustomOfficeTabSet.GetActiveTabIndex: Integer;
begin
  Result := FActiveTabIndex;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTabSet.SelectNextSequentialTab;
var
  i, j: Integer;
  found: Boolean;
begin
  if (ActiveTabIndex < 0) then
    Exit;

  found := False;
  i := ActiveTabIndex;
  j := 1;
  while (j < FAdvOfficeTabs.Count) do
  begin
    Inc(i);
    if (i >= FAdvOfficeTabs.Count) then
      Break;
    if (ActiveTabIndex <> i) and FAdvOfficeTabs.Items[i].Visible and FAdvOfficeTabs.Items[i].Enabled then
    begin
      ActiveTabIndex := i;
      found := True;
      Break;
    end;
    Inc(j);
  end;

  if not found then
  begin
    i := ActiveTabIndex;
    j := 1;
    while (j < FAdvOfficeTabs.Count) do
    begin
      dec(i);
      if (i >= FAdvOfficeTabs.Count) then
        Break;
      if (i < 0) then
        Break;
      if (ActiveTabIndex <> i) and FAdvOfficeTabs.Items[i].Visible and FAdvOfficeTabs.Items[i].Enabled then
      begin
        ActiveTabIndex := i;
        Break;
      end;
      Inc(j);
    end;
  end;
end;

procedure TAdvCustomOfficeTabSet.SelectNextTab(GoForward: Boolean);
var
  i, j: Integer;
begin
  if (ActiveTabIndex < 0) then
    Exit;

  if GoForward then
  begin
    i := ActiveTabIndex;
    j := 1;
    while (j < FAdvOfficeTabs.Count) do
    begin
      Inc(i);
      if (i >= FAdvOfficeTabs.Count) then
        i := 0;
      if (ActiveTabIndex <> i) and FAdvOfficeTabs.Items[i].Visible and FAdvOfficeTabs.Items[i].Enabled then
      begin
        ActiveTabIndex := i;
        Break;
      end;
      Inc(j);
    end;
  end
  else  // BackWard
  begin
    i := ActiveTabIndex;
    j := 1;
    while (j < FAdvOfficeTabs.Count) do
    begin
      dec(i);
      if (i >= FAdvOfficeTabs.Count) then
        i := 0;
      if (i < 0) then
        i := FAdvOfficeTabs.Count-1;
      if (ActiveTabIndex <> i) and FAdvOfficeTabs.Items[i].Visible and FAdvOfficeTabs.Items[i].Enabled then
      begin
        ActiveTabIndex := i;
        Break;
      end;
      Inc(j);
    end;
  end;
end;

//------------------------------------------------------------------------------
{
function TAdvCustomOfficeTabSet.IndexOfPage(AdvPage: TAdvOfficePage): Integer;
begin
  Result := FAdvOfficeTabs.IndexOf(AdvPage);
end;
}

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTabSet.ChangeActiveTab(TabIndex: Integer);
var
  aForm: TCustomForm;
  AllowChange: Boolean;
begin
  if (TabIndex >= 0) and (TabIndex < FAdvOfficeTabs.Count) and (TabIndex <> ActiveTabIndex) then
  begin
    AllowChange := True;

    DoChanging(ActiveTabIndex, TabIndex, AllowChange);

    if not AllowChange then
      Exit;

    FActiveTabIndex := TabIndex;

    DoChange;

    if (csDesigning in ComponentState) and not (csLoading in ComponentState) then
    begin
      aForm := GetParentForm(Self);
      if (aForm <> nil) and (aForm.Designer <> nil) then
        aForm.Designer.Modified;
    end;

    InitializeAndUpdateButtons;

    if MultiLine then
      UpdateMultiLineActiveTab;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTabSet.CloseAllTabs;
begin
  while AdvofficeTabs.Count > 0 do
  begin
    AdvOfficeTabs[0].Free;
  end;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTabSet.ClosedListButtonClick(Sender: TObject);
var
  Menu: TPopupMenu;
  MenuItem: TMenuItem;
  I: Integer;
  P: TPoint;
begin
  if (FClosedTabList.Count > 0) and not (csDesigning in ComponentState) then
  begin
    if Assigned(ClosedListMenu) then
      Menu := ClosedListMenu
    else if Assigned(FInternalClosedListMenu) then
      Menu := FInternalClosedListMenu
    else
    begin
      FInternalClosedListMenu := TPopupMenu.Create(Self);
      Menu := FInternalClosedListMenu;
    end;

    Menu.Items.Clear;
    Menu.Images := Self.Images;

    for I := 0 to FClosedTabList.Count - 1 do
    begin
      MenuItem := TMenuItem.Create(self);
      MenuItem.Caption := TOfficeTabCollectionItem(FClosedTabList.Objects[i]).Caption;
      MenuItem.Tag := I;
      MenuItem.OnClick := OnCloseListMenuClick;
      MenuItem.ImageIndex := TOfficeTabCollectionItem(FClosedTabList.Objects[i]).ImageIndex;
      Menu.Items.Add(MenuItem);
    end;

    if (FClosedListButton <> nil) then
    begin

      case (TabPosition) of
        tpTop:
        begin
          P.X := FClosedListButton.Left;
          P.Y := FClosedListButton.Top + FClosedListButton.Height;
          p := ClientToScreen(p);
        end;
        tpBottom:
        begin
          P.X := FClosedListButton.Left;
          P.Y := FClosedListButton.Top + FClosedListButton.Height;
          p := ClientToScreen(p);
          if (GetSystemMetrics(SM_CYMENU) * Menu.Items.Count) + P.Y + 10 >
          {$IFDEF DELPHI6_LVL}
            Screen.MonitorFromPoint(P).Height then
          {$ELSE}
            Screen.Height then
          {$ENDIF}
          begin
            Dec(P.Y, (GetSystemMetrics(SM_CYMENU) * Menu.Items.Count) + (FClosedListButton.Height) + 4);
          end;
        end;
        tpLeft:
        begin
          P.X := FClosedListButton.Left + FClosedListButton.Width;
          P.Y := FClosedListButton.Top + FClosedListButton.Height;
          p := ClientToScreen(p);
        end;
        tpRight:
        begin
          P.X := self.Left + Self.Width;
          P.Y := FClosedListButton.Top + FClosedListButton.Height;
          if Assigned(Parent) then
            p := Parent.ClientToScreen(p)
          else
            p := ClientToScreen(p);
        end;
      end;

      if Assigned(FOnClosedListClick) then
        FOnClosedListClick(Self, P.X, P.Y)
      else
        Menu.Popup(P.X, P.Y);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTabSet.SetActiveTabIndex(const Value: Integer);
var
  R: TRect;
  i, j: Integer;
begin
  if FUpdateCount > 0 then
    Exit;
  R := GetTabRect(Value);

  if (R.Left = -1) and (Value >= 0) and (Value < FAdvOfficeTabs.Count) then // force activated tab to be visible
  begin
    //FTabScroller.Position := Value;
    j := 0;
    for i:= 0 to Value - 1 do
    begin
      if FAdvOfficeTabs.Items[i].Visible then
        inc(j);
    end;

    FTabScroller.Position := Min(j, FTabScroller.Max);
  end;

  ChangeActiveTab(Value);
  R := GetTabsArea;
  InvalidateRect(Handle, @R, True);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTabSet.SetTabSettings(const Value: TTabSetTabSettings);
begin
  FTabSettings.Assign(Value);
end;

procedure TAdvCustomOfficeTabSet.SetTransparent(const Value: Boolean);
begin
  if (FTransparent <> Value) then
  begin
    FTransparent := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

function TAdvCustomOfficeTabSet.PTOnCheckBox(PageIndex, X, Y: integer): Boolean;
begin
  Result := false;
  if AdvOfficeTabs[PageIndex].ShowCheckBox then
    Result := PtInRect(GetCheckBoxRect(PageIndex), Point(X, Y));
end;

function TAdvCustomOfficeTabSet.PTOnTab(X, Y: Integer): Integer;
var
  i: Integer;
  P: TPoint;
  TabR: TRect;
begin
  Result := -1;
  P := Point(X, Y);
  for i:= 0 to FAdvOfficeTabs.Count-1 do
  begin
    TabR := GetTabRect(i);
    if PtInRect(TabR, P) then
    begin
      Result := i;
      Break;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TAdvCustomOfficeTabSet.GetButtonsRect: TRect;
var
  Spc: Integer;
  cnt: integer;
begin
  cnt := 0;
  if ButtonSettings.FirstButton and ButtonSettings.LastButton then
    cnt := 4
  else if ButtonSettings.FirstButton or ButtonSettings.LastButton then
    cnt := 3
  else if not ButtonSettings.FirstButton and not ButtonSettings.LastButton then
    cnt := 2;

  Result := ClientRect;
  Spc := 3;
  case TabPosition of
    tpTop:
    begin
      Result.Top := Result.Top + FTabOffSet;
      Result.Bottom := Result.Top + FTabSettings.Height;
      Result.Right := Result.Right - FTabSettings.EndMargin - ADVPAGE_OFFSET - 3;
      Result.Left := Result.Right-3;
      if FTabScroller.Visible then
        Result.Left := Result.Left -(ButtonSettings.ButtonSize * cnt + 2 + 3);
      if (ButtonSettings.TabListButton) then
        Result.Left := Result.Left - ButtonSettings.ButtonSize - Spc;
      if (ButtonSettings.CloseButton and not CloseOnTab) then
        Result.Left := Result.Left - ButtonSettings.ButtonSize - Spc;
      if (ButtonSettings.ClosedListButton and (FClosedTabList.Count > 0)) then
        Result.Left := Result.Left - ButtonSettings.ButtonSize - Spc;
    end;
    tpBottom:
    begin
      Result.Top := Result.Bottom - FTabSettings.Height - FTabOffSet;
      Result.Bottom := Result.Bottom - FTabOffSet;
      Result.Right := Result.Right - FTabSettings.EndMargin - ADVPAGE_OFFSET-3;
      Result.Left := Result.Right -3;
      if FTabScroller.Visible then
        Result.Left := Result.Left -(ButtonSettings.ButtonSize*cnt + 2 + 3);
      if (ButtonSettings.TabListButton) then
        Result.Left := Result.Left - ButtonSettings.ButtonSize - Spc;
      if (ButtonSettings.CloseButton and not CloseOnTab) then
        Result.Left := Result.Left - ButtonSettings.ButtonSize - Spc;
      if (ButtonSettings.ClosedListButton and (FClosedTabList.Count > 0)) then
        Result.Left := Result.Left - ButtonSettings.ButtonSize - Spc;
    end;
    tpLeft:
    begin
      Result.Bottom := Result.Bottom - FTabSettings.EndMargin - ADVPAGE_OFFSET-3;
      Result.Top := Result.Bottom -3;
      Result.Left := Result.Left + FTabOffSet;
      Result.Right := Result.Left + FTabSettings.Height;
      if FTabScroller.Visible then
        Result.Top := Result.Top -(ButtonSettings.ButtonSize * cnt + 2 + 3);
      if (ButtonSettings.TabListButton) then
        Result.Top := Result.Top - ButtonSettings.ButtonSize - Spc;
      if (ButtonSettings.CloseButton and not CloseOnTab) then
        Result.Top := Result.Top - ButtonSettings.ButtonSize - Spc;
      if (ButtonSettings.ClosedListButton and (FClosedTabList.Count > 0)) then
        Result.Top := Result.Top - ButtonSettings.ButtonSize - Spc;

      if not RotateTabLeftRight then
        Result.Left := Result.Right - 5 - ButtonSettings.ButtonSize - 3;
    end;
    tpRight:
    begin
      Result.Bottom := Result.Bottom - FTabSettings.EndMargin - ADVPAGE_OFFSET-3;
      Result.Top := Result.Bottom -3;
      Result.Left := Result.Right - TabSettings.Height - FTabOffSet;
      Result.Right := Result.Right - FTabOffSet;
      if FTabScroller.Visible then
        Result.Top := Result.Top -(ButtonSettings.ButtonSize*cnt + 2 + 3);
      if (ButtonSettings.TabListButton) then
        Result.Top := Result.Top - ButtonSettings.ButtonSize - Spc;
      if (ButtonSettings.CloseButton and not CloseOnTab) then
        Result.Top := Result.Top - ButtonSettings.ButtonSize - Spc;
      if (ButtonSettings.ClosedListButton and (FClosedTabList.Count > 0)) then
        Result.Top := Result.Top - ButtonSettings.ButtonSize - Spc;

      if not RotateTabLeftRight then
        Result.Right := Result.Left + 5 + ButtonSettings.ButtonSize + 3;
    end;
  end;
end;

//------------------------------------------------------------------------------

// Independent to Start/End Margins and Scroller
function TAdvCustomOfficeTabSet.GetTabsArea: TRect;
begin
  Result := ClientRect;
  if MultiLine then
    Exit;

  case TabPosition of
    tpTop: Result.Bottom := Result.Top + FTabSettings.Height;
    tpBottom: Result.Top := Result.Bottom - FTabSettings.Height;
    tpLeft: Result.Right := Result.Left + FTabSettings.Height;
    tpRight: Result.Left := Result.Right - FTabSettings.Height;
  end;
end;

//------------------------------------------------------------------------------

function TAdvCustomOfficeTabSet.GetTabsRect: TRect;
begin
  Result := ClientRect;
  case TabPosition of
    tpTop:
    begin
      Result.Top := Result.Top + FTabOffSet;
      Result.Bottom := Result.Top + FTabSettings.Height;
      Result.Left := Result.Left + FTabSettings.StartMargin;
      Result.Right := GetButtonsRect.Left; //Result.Right - FTabSettings.EndMargin;
      if ButtonSettings.ShowInsertButton then
        Result.Right := Result.Right - ButtonSettings.ButtonSize - 6;
    end;
    tpBottom:
    begin
      Result.Top := Result.Bottom - FTabSettings.Height - FTabOffSet;
      Result.Bottom := Result.Bottom - FTabOffSet;
      Result.Left := Result.Left + FTabSettings.StartMargin;
      Result.Right := GetButtonsRect.Left; //Result.Right - FTabSettings.EndMargin;
      if ButtonSettings.ShowInsertButton then
        Result.Right := Result.Right - ButtonSettings.ButtonSize - 6;
    end;
    tpLeft:
    begin
      Result.Top := Result.Top + FTabSettings.StartMargin;
      Result.Bottom := GetButtonsRect.Top; // Result.Bottom - FTabSettings.EndMargin;
      if ButtonSettings.ShowInsertButton then
        Result.Bottom := Result.Bottom - ButtonSettings.ButtonSize - 6;
      Result.Left := Result.Left + FTabOffSet;
      Result.Right := Result.Left + FTabSettings.Height;
    end;
    tpRight:
    begin
      Result.Top := Result.Top + FTabSettings.StartMargin;
      Result.Bottom := GetButtonsRect.Top; //Result.Bottom - FTabSettings.EndMargin;
      if ButtonSettings.ShowInsertButton then
        Result.Bottom := Result.Bottom - ButtonSettings.ButtonSize - 6;
      Result.Left := Result.Right - TabSettings.Height - FTabOffSet;
      Result.Right := Result.Right - FTabOffSet;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TAdvCustomOfficeTabSet.GetTabRect(TabIndex: Integer): TRect;
begin
  Result := GetTabRect(0, TabIndex, True);
end;

//------------------------------------------------------------------------------

function TAdvCustomOfficeTabSet.GetTabRect(StartIndex, TabIndex: Integer; ConsiderTabScroller: Boolean): TRect;
var
  i, TbW, TbH, j, Sp, fdW, ImgTxtSp, ImgW, ImgH: Integer;
  R, CR, R2: TRect;
  TabAppearance: TCustomTabAppearance;
  Pic: TGDIPPicture;
  fixTabs: Integer;
begin
  Result := Rect(-1, -1, -1, -1);
  Sp := FTabSettings.Spacing; //0;
  fdW := 5;
  ImgTxtSp := IMG_SPACE;

  if (TabIndex >= 0) and (TabIndex < FAdvOfficeTabs.Count) then
  begin
    if not FAdvOfficeTabs.Items[TabIndex].Visible then
      Exit;

    if MultiLine then
    begin
      Result := FAdvOfficeTabs.Items[TabIndex].FTabRect;
      Exit;
    end;

    CR := GetTabsRect; // ClientRect;
    fixTabs := FixedTabs;
    if (csDesigning in ComponentState) then  // No fixedtabs at design time
      fixTabs := 0;

    //if Align in [daTop, daBottom] then
    begin
      j := 0;
      for i := 0{StartIndex} to TabIndex do
      begin
        if not FAdvOfficeTabs.Items[i].Visible or ((i < StartIndex) and (i > fixTabs - 1)) then
          Continue;

        if Assigned(FTabAppearance) and FItones then
          TabAppearance := FTabAppearance
        else
          TabAppearance := FCurrentOfficeTabSetStyler.TabAppearance;

        if FAdvOfficeTabs.Items[i].UseTabAppearance then
          Canvas.Font.Assign(FAdvOfficeTabs.Items[i].TabAppearance.Font)
        else
          Canvas.Font.Assign(TabAppearance.Font);

        Canvas.Font.Size := Round(GetDPIScale(FFormScaled, Canvas.Handle)* Canvas.Font.Size);

        ImgW := 0;
        ImgH := 0;

        if (TabPosition in [tpTop, tpBottom]) then
        begin
          if FUseMaxSpace then
            CR.Right := GetTabsArea.Right;

          if (FAdvOfficeTabs.Items[i].Caption <> '') then
          begin
            R2 := Rect(0,0, 1000, 100);

            if AntiAlias = aaNone then
              DrawText(Canvas.Handle,PChar(FAdvOfficeTabs.Items[i].Caption),Length(FAdvOfficeTabs.Items[i].Caption), R2, DT_CALCRECT or DT_LEFT or DT_SINGLELINE)
            else
              R2 := DrawVistaText(Canvas, taLeftJustify, R2, FAdvOfficeTabs.Items[i].Caption,Canvas.Font, true, false, AntiAlias, tpTop,false);
          end
          else
            R2 := Rect(0, 0, 0, 0);

          TbW := GetLeftRoundingOffset + TabSettings.LeftMargin + R2.Right + fdW + TabSettings.RightMargin + GetRightRoundingOffset;

          if FAdvOfficeTabs.Items[i].Enabled or FAdvOfficeTabs.Items[i].DisabledPicture.Empty then
            Pic := FAdvOfficeTabs.Items[i].Picture
          else
            Pic := FAdvOfficeTabs.Items[i].DisabledPicture;

          if Assigned(Pic) and not Pic.Empty then
          begin
            Pic.GetImageSizes;
            //TbW := TbW + Pic.Width + ImgTxtSp;
            ImgW := Pic.Width;
          end
          else
          if (Assigned(FImages) or Assigned(DisabledImages)) and (FAdvOfficeTabs.Items[i].ImageIndex >= 0) then
          begin
            if FAdvOfficeTabs.Items[i].Enabled then
            begin
              if Assigned(FImages) then
              begin
                //TbW := TbW + FImages.Width + ImgTxtSp;
                ImgW := FImages.Width;
              end;
            end
            else
            begin
              if Assigned(FDisabledImages) then
              begin
                //TbW := TbW + FDisabledImages.Width + ImgTxtSp
                ImgW := FDisabledImages.Width;
              end
              else if Assigned(FImages) then
              begin
                //TbW := TbW + FImages.Width + ImgTxtSp;
                ImgW := FImages.Width;
              end;
            end;
          end;

          case TabSettings.ImagePosition of
            ipTop, ipBottom:
            begin
              // do nothing
            end;
            ipLeft, ipRight:
            begin
              TbW := TbW + ImgW + ImgTxtSp;
            end;
          end;

          if (ButtonSettings.CloseButton and CloseOnTab and AdvOfficeTabs[i].ShowClose) then
            TbW := Tbw + ButtonSettings.ButtonSize + 4;

          if AdvOfficeTabs[I].ShowCheckBox then
            TbW := TbW + 20;

          if (TabSettings.Width > 0) then
            TbW := TabSettings.Width;

          if ((CR.Left + TbW) > (CR.Right - ADVPAGE_OFFSET)) and (not ConsiderTabScroller or (FTabScroller.Visible and(i = TabIndex) and (FTabScroller.Position <> TabIndex))) then
          begin
            if not ((fixTabs > 0) and (i <= fixTabs - 1) and (StartIndex = 0)) then
              Break;
          end;

          if FTabScroller.Visible and ConsiderTabScroller then
          begin
            if (j >= FTabScroller.Position) then
            begin
              R := Rect(CR.Left, CR.Top, CR.Left + TbW, CR.Bottom);
              if (i = TabIndex) then
                Result := R;
              CR.Left := CR.Left + TbW + Sp;
            end
            else if (fixTabs > 0) and (i <= fixTabs - 1) and (StartIndex = 0) then
            begin
              R := Rect(CR.Left, CR.Top, CR.Left + TbW, CR.Bottom);
              if (i = TabIndex) then
                Result := R;
              CR.Left := CR.Left + TbW + Sp;
              Dec(j); // Counter to inc(j) at last
            end;
          end
          else
          begin
            R := Rect(CR.Left, CR.Top, CR.Left + TbW, CR.Bottom);
            if (i = TabIndex) then
              Result := R;
            CR.Left := CR.Left + TbW + Sp;
          end;
        end
        else // TabPosition in [tpLeft, tpRight]
        begin
          if FUseMaxSpace then
            CR.Bottom := GetTabsArea.Bottom;

          if (FAdvOfficeTabs.Items[i].Caption <> '') then
          begin

            R2 := Rect(0,0, 1000, 100);

            if AntiAlias = aaNone then
              DrawText(Canvas.Handle,PChar(FAdvOfficeTabs.Items[i].Caption),Length(FAdvOfficeTabs.Items[i].Caption), R2, DT_CALCRECT or DT_LEFT or DT_SINGlELINE)
            else
              R2 := DrawVistaText(Canvas, taLeftJustify, R2, FAdvOfficeTabs.Items[i].Caption,Canvas.Font, true, false, AntiAlias, tpTop,false);
          end
          else
            R2 := Rect(0, 0, 0, 0);

          TbW := TabSettings.LeftMargin + R2.Right+ fdW + TabSettings.RightMargin;
          TbH := R2.Bottom - R2.Top;

          if FAdvOfficeTabs.Items[i].Enabled or FAdvOfficeTabs.Items[i].DisabledPicture.Empty then
            Pic := FAdvOfficeTabs.Items[i].Picture
          else
            Pic := FAdvOfficeTabs.Items[i].DisabledPicture;

          if Assigned(Pic) and not Pic.Empty then
          begin
            Pic.GetImageSizes;
            //TbW := TbW + Pic.Height + ImgTxtSp;
            //TbH := Max(TbH, Pic.Height);
            ImgW := Pic.Width;
            ImgH := Pic.Height;
          end
          else
          if (Assigned(FImages) or Assigned(DisabledImages)) and (FAdvOfficeTabs.Items[i].ImageIndex >= 0) then
          begin
            if FAdvOfficeTabs.Items[i].Enabled then
            begin
              if Assigned(FImages) then
              begin
                //TbW := TbW + FImages.Height + ImgTxtSp;
                //TbH := Max(TbH, FImages.Height);
                ImgW := FImages.Width;
                ImgH := FImages.Height;
              end;
            end
            else
            begin
              if Assigned(FDisabledImages) then
              begin
                //TbW := TbW + FDisabledImages.Height + ImgTxtSp;
                //TbH := Max(TbH, FDisabledImages.Height);
                ImgW := FDisabledImages.Width;
                ImgH := FDisabledImages.Height;
              end
              else if Assigned(FImages) then
              begin
                //TbW := TbW + FImages.Height + ImgTxtSp;
                //TbH := Max(TbH, FImages.Height);
                ImgW := FImages.Width;
                ImgH := FImages.Height;
              end;
            end;
          end;

          case TabSettings.ImagePosition of
            ipTop, ipBottom:
            begin
              TbH := Tbh + ImgH + ImgTxtSp;
            end;
            ipLeft, ipRight:
            begin
              TbW := TbW + ImgW + ImgTxtSp;
              TbH := Max(TbH, ImgH);
            end;
          end;

          TbH := TbH + 12; //TabSettings.RightMargin;

          if not RotateTabLeftRight then
          begin
            if (TabSettings.Width > 0) then
              TbH := TabSettings.Width;

            if ((CR.Top + TbH) > (CR.Bottom - ADVPAGE_OFFSET)) and (not ConsiderTabScroller or (FTabScroller.Visible and(i = TabIndex) and (FTabScroller.Position <> TabIndex))) then
            begin
              if not ((fixTabs > 0) and (i <= fixTabs - 1) and (StartIndex = 0)) then
                Break;
            end;

            if FTabScroller.Visible and ConsiderTabScroller then
            begin
              if (j >= FTabScroller.Position) then
              begin
                R := Rect(CR.Left, CR.Top, CR.Right, CR.Top + TbH);
                if (i = TabIndex) then
                  Result := R;
                CR.Top := CR.Top + TbH + Sp;
              end
              else if (fixTabs > 0) and (i <= fixTabs - 1) and (StartIndex = 0) then
              begin
                R := Rect(CR.Left, CR.Top, CR.Right, CR.Top + TbH);
                if (i = TabIndex) then
                  Result := R;
                CR.Top := CR.Top + TbH + Sp;
                Dec(j); // Counter to inc(j) at last
              end;
            end
            else
            begin
              R := Rect(CR.Left, CR.Top, CR.Right, CR.Top + TbH);
              if (i = TabIndex) then
                Result := R;
              CR.Top := CR.Top + TbH + Sp;
            end;
          end
          else
          begin
            TbW := TbW + GetLeftRoundingOffset + GetRightRoundingOffset;
            if (ButtonSettings.CloseButton and CloseOnTab and AdvOfficeTabs[i].ShowClose) then
              TbW := Tbw + ButtonSettings.ButtonSize + 4;

            if AdvOfficeTabs[I].ShowCheckBox then
              TbW := TbW + 20;

            if (TabSettings.Width > 0) then
              TbW := TabSettings.Width;

            if ((CR.Top + TbW) > (CR.Bottom - ADVPAGE_OFFSET)) and (not ConsiderTabScroller or (FTabScroller.Visible and(i = TabIndex) and (FTabScroller.Position <> TabIndex))) then
            begin
              if not ((fixTabs > 0) and (i <= fixTabs - 1) and (StartIndex = 0)) then
                Break;
            end;

            if FTabScroller.Visible and ConsiderTabScroller then
            begin
              if (j >= FTabScroller.Position) then
              begin
                R := Rect(CR.Left, CR.Top, CR.Right, CR.Top + TbW);
                if (i = TabIndex) then
                  Result := R;
                CR.Top := CR.Top + TbW + Sp;
              end
              else if (fixTabs > 0) and (i <= fixTabs - 1) and (StartIndex = 0) then
              begin
                R := Rect(CR.Left, CR.Top, CR.Right, CR.Top + TbW);
                if (i = TabIndex) then
                  Result := R;
                CR.Top := CR.Top + TbW + Sp;
                Dec(j); // Counter to inc(j) at last
              end;
            end
            else
            begin
              R := Rect(CR.Left, CR.Top, CR.Right, CR.Top + TbW);
              if (i = TabIndex) then
                Result := R;
              CR.Top := CR.Top + TbW + Sp;
            end;
          end;
        end;

        Inc(j);
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTabSet.CMDesignHitTest(var Msg: TCMDesignHitTest);
var
  p: TPoint;
  Tab: Integer;
begin
  Tab := -1;
  if (csDesigning in ComponentState) then
  begin
    GetCursorPos(P);
    P := ScreenToClient(P);

    if PtInRect(GetTabsRect, p) and (GetAsyncKeyState(VK_LBUTTON) <> 0) then
    begin
      Tab := PTOnTab(P.X, P.Y);
      if (Tab >= 0) then
      begin
        Msg.Result := 1;
      end;
    end;

  end;

  if (Tab = -1) then
    inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTabSet.SetImages(const Value: TCustomImageList);
begin
  FImages := Value;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTabSet.SetMultiLine(const Value: Boolean);
begin
  if (FMultiLine <> Value) then
  begin
    if Value then
    begin
      if (FixedTabs > 0) then
      begin
        MessageDlg('Can not have multiline with Fixed tabs.', mtInformation, [mbOK], 0);
        Exit;
      end;

      if (not RotateTabLeftRight) then
      begin
        MessageDlg('RotateTabLeftRight must be True with MultiLine.', mtInformation, [mbOK], 0);
        Exit;
      end;
    end;

    FMultiLine := Value;

    if FMultiLine then
    begin
      UpdateMultiLineTabs;
      if AutoHeight then AdjustHeight;
    end
    else
    begin
      UpdateTabScroller;
      if AutoHeight then
      begin
        if TabPosition in [tpTop, tpBottom] then
          Height := TabSettings.Height + 1
        else  // TabPosition in [tpLeft, Right]
          Width := TabSettings.Height + 1;
      end;
    end;

    InitializeAndUpdateButtons;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTabSet.UpdateMultiLineTabs;
var
  avlspace, tbRowCount, i1, i2, acvTabR, RwSpace: Integer;
  R, TR, R2, CR: TRect;
  i, TbW, TbH, j, Sp, fdW, ImgTxtSp, ImgW, ImgH: Integer;
  TabAppearance: TCustomTabAppearance;
  Pic: TGDIPPicture;
  multiRow: Boolean;
  tbRows: array of TPoint;
begin
  if FMultiLine and not (csLoading in ComponentState) and not (csReading in ComponentState) and
     not (csDestroying in ComponentState) then
  begin
    FTabScroller.Visible := False;

    TR := GetTabsRect;
    CR := TR;

    if TabPosition in [tpTop, tpBottom] then
      avlspace := TR.Right - TR.Left
    else
      avlspace := TR.Bottom - TR.Top;

    if Assigned(FTabAppearance) and FItones then
      TabAppearance := FTabAppearance
    else
      TabAppearance := FCurrentOfficeTabSetStyler.TabAppearance;

    Canvas.Font.Assign(TabAppearance.Font);
    Canvas.Font.Size := Round(GetDPIScale(FFormScaled, Canvas.Handle)* Canvas.Font.Size);

    Sp := FTabSettings.Spacing;
    fdW := 5;
    ImgTxtSp := IMG_SPACE;
    MultiRow := False;

    if ShowNonSelectedTabs then
      RwSpace := 2   // Row space
    else
      RwSpace := 0;  // Row space

    for i := 0 to FAdvOfficeTabs.Count - 1 do
    begin
      // clear rects
      if not FAdvOfficeTabs.Items[i].Visible then
      begin
        FAdvOfficeTabs.Items[i].FTabRect := Rect(-1, -1, -1, -1);
        Continue;
      end;

      ImgW := 0;
      ImgH := 0;

      if (TabPosition in [tpTop, tpBottom]) then
      begin
        //if FUseMaxSpace then
          //CR.Right := GetTabsArea.Right;

        if (FAdvOfficeTabs.Items[i].Caption <> '') then
        begin
          R2 := Rect(0,0, 1000, 100);

          if AntiAlias = aaNone then
            DrawText(Canvas.Handle,PChar(FAdvOfficeTabs.Items[i].Caption),Length(FAdvOfficeTabs.Items[i].Caption), R2, DT_CALCRECT or DT_LEFT or DT_SINGLELINE)
          else
            R2 := DrawVistaText(Canvas, taLeftJustify, R2, FAdvOfficeTabs.Items[i].Caption,Canvas.Font, true, false, AntiAlias, tpTop,false);
        end
        else
          R2 := Rect(0, 0, 0, 0);

        TbW := GetLeftRoundingOffset + TabSettings.LeftMargin + R2.Right + fdW + TabSettings.RightMargin + GetRightRoundingOffset;

        if FAdvOfficeTabs.Items[i].Enabled or FAdvOfficeTabs.Items[i].DisabledPicture.Empty then
          Pic := FAdvOfficeTabs.Items[i].Picture
        else
          Pic := FAdvOfficeTabs.Items[i].DisabledPicture;

        if Assigned(Pic) and not Pic.Empty then
        begin
          Pic.GetImageSizes;
          ImgW := Pic.Width;
        end
        else
        if (Assigned(FImages) or Assigned(DisabledImages)) and (FAdvOfficeTabs.Items[i].ImageIndex >= 0) then
        begin
          if FAdvOfficeTabs.Items[i].Enabled then
          begin
            if Assigned(FImages) then
            begin
              ImgW := FImages.Width;
            end;
          end
          else
          begin
            if Assigned(FDisabledImages) then
            begin
              ImgW := FDisabledImages.Width;
            end
            else if Assigned(FImages) then
            begin
              ImgW := FImages.Width;
            end;
          end;
        end;

        case TabSettings.ImagePosition of
          ipTop, ipBottom:
          begin
            // do nothing
          end;
          ipLeft, ipRight:
          begin
            TbW := TbW + ImgW + ImgTxtSp;
          end;
        end;

        if (ButtonSettings.CloseButton and CloseOnTab and AdvOfficeTabs[i].ShowClose) then
          TbW := Tbw + ButtonSettings.ButtonSize + 4;

        if AdvOfficeTabs[I].ShowCheckBox then
          TbW := TbW + 20;

        if (TabSettings.Width > 0) then
          TbW := TabSettings.Width;
        {
        if ((CR.Left + TbW) > (CR.Right - ADVPAGE_OFFSET)) and (not ConsiderTabScroller or (FTabScroller.Visible and(i = TabIndex) and (FTabScroller.Position <> TabIndex))) then
        begin
          if not ((fixTabs > 0) and (i <= fixTabs - 1) and (StartIndex = 0)) then
            Break;
        end;}

        if ((CR.Left + TbW) > (CR.Right - ADVPAGE_OFFSET)) then
          MultiRow := True;

        //R := Rect(CR.Left, CR.Top, CR.Left + TbW, CR.Bottom);
        R := Rect(CR.Left, CR.Top, CR.Left + TbW, CR.Bottom);
        //if (i = TabIndex) then
          FAdvOfficeTabs.Items[i].FTabRect := R;
        CR.Left := CR.Left + TbW + Sp;
      end
      else // TabPosition in [tpLeft, tpRight]
      begin
        //if FUseMaxSpace then
          //CR.Bottom := GetTabsArea.Bottom;

        if (FAdvOfficeTabs.Items[i].Caption <> '') then
        begin

          R2 := Rect(0,0, 1000, 100);

          if AntiAlias = aaNone then
            DrawText(Canvas.Handle,PChar(FAdvOfficeTabs.Items[i].Caption),Length(FAdvOfficeTabs.Items[i].Caption), R2, DT_CALCRECT or DT_LEFT or DT_SINGlELINE)
          else
            R2 := DrawVistaText(Canvas, taLeftJustify, R2, FAdvOfficeTabs.Items[i].Caption,Canvas.Font, true, false, AntiAlias, tpTop,false);
        end
        else
          R2 := Rect(0, 0, 0, 0);

        TbW := TabSettings.LeftMargin + R2.Right+ fdW + TabSettings.RightMargin;
        TbH := R2.Bottom - R2.Top;

        if FAdvOfficeTabs.Items[i].Enabled or FAdvOfficeTabs.Items[i].DisabledPicture.Empty then
          Pic := FAdvOfficeTabs.Items[i].Picture
        else
          Pic := FAdvOfficeTabs.Items[i].DisabledPicture;

        if Assigned(Pic) and not Pic.Empty then
        begin
          Pic.GetImageSizes;
          ImgW := Pic.Width;
          ImgH := Pic.Height;
        end
        else
        if (Assigned(FImages) or Assigned(DisabledImages)) and (FAdvOfficeTabs.Items[i].ImageIndex >= 0) then
        begin
          if FAdvOfficeTabs.Items[i].Enabled then
          begin
            if Assigned(FImages) then
            begin
              ImgW := FImages.Width;
              ImgH := FImages.Height;
            end;
          end
          else
          begin
            if Assigned(FDisabledImages) then
            begin
              ImgW := FDisabledImages.Width;
              ImgH := FDisabledImages.Height;
            end
            else if Assigned(FImages) then
            begin
              ImgW := FImages.Width;
              ImgH := FImages.Height;
            end;
          end;
        end;

        case TabSettings.ImagePosition of
          ipTop, ipBottom:
          begin
            TbH := Tbh + ImgH + ImgTxtSp;
          end;
          ipLeft, ipRight:
          begin
            TbW := TbW + ImgW + ImgTxtSp;
            TbH := Max(TbH, ImgH);
          end;
        end;

        TbH := TbH + 12;

        // No MultiLine with RotateTabLeftRight
        if RotateTabLeftRight then
        begin
          TbW := TbW + GetLeftRoundingOffset + GetRightRoundingOffset;
          if (ButtonSettings.CloseButton and CloseOnTab and AdvOfficeTabs[i].ShowClose) then
            TbW := Tbw + ButtonSettings.ButtonSize + 4;

          if AdvOfficeTabs[I].ShowCheckBox then
            TbW := TbW + 20;

          if (TabSettings.Width > 0) then
            TbW := TabSettings.Width;
          {
          if ((CR.Top + TbW) > (CR.Bottom - ADVPAGE_OFFSET)) and (not ConsiderTabScroller or (FTabScroller.Visible and(i = TabIndex) and (FTabScroller.Position <> TabIndex))) then
          begin
            if not ((fixTabs > 0) and (i <= fixTabs - 1) and (StartIndex = 0)) then
              Break;
          end;
          }

          if Tbh < -1 then  // dummy and to be deleted
            TbW := Tbw;

          if ((CR.Top + TbW) > (CR.Bottom - ADVPAGE_OFFSET)) then
            MultiRow := True;

          R := Rect(CR.Left, CR.Top, CR.Right, CR.Top + TbW);
          //if (i = TabIndex) then
            FAdvOfficeTabs.Items[i].FTabRect := R;
          CR.Top := CR.Top + TbW + Sp;
        end;
      end;

    end;  // end of for loop

    SetLength(tbRows, FAdvOfficeTabs.Count);   // Max possible rows
    tbRowCount := 0;

    if FAdvOfficeTabs.Count > 0 then
    begin
      FTabRowSize := FAdvOfficeTabs.Items[0].FTabRect.Bottom - FAdvOfficeTabs.Items[0].FTabRect.Top
    end
    else
      FTabRowSize := 0;

    if multiRow then  // in case multi row
    begin
      i1 := 0;
      i2 := 0;
      j := 0;
      acvTabR := 1;
      Sp := 0;
      for i := 0 to FAdvOfficeTabs.Count - 1 do
      begin
        if not FAdvOfficeTabs.Items[i].Visible then
          Continue;

        if (i = ActiveTabIndex) then
          acvTabR := tbRowCount + 1;   // note active tab row

        if (TabPosition in [tpTop, tpBottom]) then
        begin
          if (j + sp + FAdvOfficeTabs.Items[i].FTabRect.Right - FAdvOfficeTabs.Items[i].FTabRect.Left < avlspace) then
          begin
            j := j + sp + FAdvOfficeTabs.Items[i].FTabRect.Right - FAdvOfficeTabs.Items[i].FTabRect.Left;
            i2 := i;
            Sp := FTabSettings.Spacing;
          end
          else
          begin
            if (j = 0) then   // in case only one tab in row
              i2 := i;

            tbRows[tbRowCount + 1].X := i1;     // tab row start index
            tbRows[tbRowCount + 1].Y := i2;     // tab row end index
            tbRowCount := tbRowCount + 1;
            i1 := i;

            if (i = ActiveTabIndex) then
              acvTabR := tbRowCount + 1;   // note active tab row

            if (i = FAdvOfficeTabs.Count - 1) then  // last tab
            begin
              tbRows[tbRowCount + 1].X := i1;     // tab row start index
              tbRows[tbRowCount + 1].Y := i;     // tab row end index
              tbRowCount := tbRowCount + 1;
              j := 0;
            end
            else
            begin
              j := FAdvOfficeTabs.Items[i].FTabRect.Right - FAdvOfficeTabs.Items[i].FTabRect.Left;
              i2 := i;
              Sp := FTabSettings.Spacing;
            end;
          end;
        end
        else // TabPosition in [tpLeft, tpRight]
        begin
          if (j + sp + FAdvOfficeTabs.Items[i].FTabRect.Bottom - FAdvOfficeTabs.Items[i].FTabRect.Top < avlspace) then
          begin
            j := j + sp + FAdvOfficeTabs.Items[i].FTabRect.Bottom - FAdvOfficeTabs.Items[i].FTabRect.Top;
            i2 := i;
            Sp := FTabSettings.Spacing;
          end
          else
          begin
            if (j = 0) then   // in case only one tab in row
              i2 := i;

            tbRows[tbRowCount + 1].X := i1;     // tab row start index
            tbRows[tbRowCount + 1].Y := i2;     // tab row end index
            tbRowCount := tbRowCount + 1;
            i1 := i;

            if (i = ActiveTabIndex) then
              acvTabR := tbRowCount + 1;   // note active tab row

            if (i = FAdvOfficeTabs.Count - 1) then  // last tab
            begin
              tbRows[tbRowCount + 1].X := i1;     // tab row start index
              tbRows[tbRowCount + 1].Y := i;     // tab row end index
              tbRowCount := tbRowCount + 1;
              j := 0;
            end
            else
            begin
              j := FAdvOfficeTabs.Items[i].FTabRect.Bottom - FAdvOfficeTabs.Items[i].FTabRect.Top;
              i2 := i;
              Sp := FTabSettings.Spacing;
            end;
          end;
        end;   // TabPosition in [tpLeft, tpRight]

      end;

      if (j > 0) then
      begin
        tbRows[tbRowCount + 1].X := i1;     // tab row start index
        tbRows[tbRowCount + 1].Y := i2;     // tab row end index
        tbRowCount := tbRowCount + 1;
      end;

      FMultiLineRowCount := tbRowCount;

      SetLength(FTabRows, FMultiLineRowCount + 1);

      i := acvTabR;

      for j := tbRowCount downto 1 do
      begin
        FTabRows[j].X := tbRows[i].X;     // tab row start index
        FTabRows[j].Y := tbRows[i].Y;     // tab row end index
        inc(i);
        if i > tbRowCount then
          i := 1;
      end;

      Sp := FTabSettings.Spacing;
      if (TabPosition in [tpTop, tpBottom]) then
      begin
        R := TR;
        TbH := 0;
        //R2 := TR;
        for j := 1 to tbRowCount do
        begin
          //if (j <> acvTabR) then
          begin
            R2 := TR;
            TbH := 0;
            for i := FTabRows[j].X to FTabRows[j].Y do
            begin
              if not FAdvOfficeTabs.Items[i].Visible then
                Continue;

              if TbH = 0 then
                TbH := FAdvOfficeTabs.Items[i].FTabRect.Bottom - FAdvOfficeTabs.Items[i].FTabRect.Top;
              FAdvOfficeTabs.Items[i].FTabRect := Rect(R2.Left, R.Top, R2.Left + FAdvOfficeTabs.Items[i].FTabRect.Right - FAdvOfficeTabs.Items[i].FTabRect.Left, R.Top + TbH);
              R2.Left := FAdvOfficeTabs.Items[i].FTabRect.Right + Sp;
            end;
            R.Top := R.Top + TbH + RwSpace;
            R.Bottom := R.Bottom + TbH + RwSpace;
          end;
        end;

        if TbH = 0 then
          TbH := TabSettings.Height;
        FTabRowSize := FMultiLineRowCount * Tbh + ((FMultiLineRowCount - 1) * RwSpace);
      end
      else  // (TabPosition in [tpLeft, tpRight]) then
      begin
        R := TR;
        TbW := 0;
        for j := 1 to tbRowCount do
        begin
          R2 := TR;
          TbW := 0;
          for i := FTabRows[j].X to FTabRows[j].Y do
          begin
            if not FAdvOfficeTabs.Items[i].Visible then
              Continue;

            if TbW = 0 then
              TbW := FAdvOfficeTabs.Items[i].FTabRect.Right - FAdvOfficeTabs.Items[i].FTabRect.Left;
            FAdvOfficeTabs.Items[i].FTabRect := Rect(R.Left, R2.Top, R.Left + TbW, R2.Top + FAdvOfficeTabs.Items[i].FTabRect.Bottom - FAdvOfficeTabs.Items[i].FTabRect.Top);
            R2.Top := FAdvOfficeTabs.Items[i].FTabRect.Bottom + Sp;
          end;
          R.Left := R.Left + TbW + RwSpace;
          R.Right := R.Right + TbW + RwSpace;
        end;

        if TbW = 0 then
          TbW := TabSettings.Height;
        FTabRowSize := FMultiLineRowCount * TbW + ((FMultiLineRowCount - 1) * RwSpace);
      end;

    end; // if multirow

  end;
  InitializeAndUpdateButtons;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTabSet.UpdateMultiLineActiveTab;
var
  i, acvTabR: Integer;

begin
  if MultiLine and (FMultiLineRowCount > 1) and (ActiveTabIndex >= 0) and (FAdvOfficeTabs.Count > 0) then
  begin
    acvTabR := -1;

    for i := 1 to FMultiLineRowCount do
    begin
      if (ActiveTabIndex >= FTabRows[i].X) and (ActiveTabIndex <= FTabRows[i].Y) then
      begin
        acvTabR := i;
        Break;
      end;
    end;

    if (acvTabR <= 0) then    // invalid row index
      Exit;

    UpdateMultiLineTabs;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTabSet.SetAutoHeight(const Value: Boolean);
begin
  if (FAutoHeight <> Value) then
  begin
    FAutoHeight := Value;
    AdjustHeight;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTabSet.AdjustHeight;
begin
  if FAutoHeight then
  begin
    if (TabPosition in [tpTop, tpBottom]) then
    begin
      if MultiLine then
        Height := FTabRowSize
      else
        Height := TabSettings.Height + 1;
    end
    else // TabPosition in [tpLeft, tpRight]
    begin
      if MultiLine then
        Width := FTabRowSize + 2
      else if RotateTabLeftRight  then
        Width := TabSettings.Height + 1;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTabSet.InvalidateTab(TabIndex: Integer);
var
  R: TRect;
  {
  R, CloseBtnR: TRect;
  rgn1, rgn2: HRGN;
  }
begin
  {
  if (TabIndex >= 0) and (TabIndex < FAdvOfficeTabs.Count) then
  begin
    R := GetTabRect(TabIndex);
    if (TabIndex = ActiveTabIndex) and (ActiveTabIndex >= 0) and CloseOnTab and CanShowCloseButton then
    begin
      CloseBtnR := GetCloseButtonRect(TabIndex);
      if (TabPosition in [tpTop, tpBottom]) or not RotateTabLeftRight then
      begin
        rgn1 := CreateRectRgn(R.Left, R.Top, CloseBtnR.Left, R.Bottom);
        rgn2 := CreateRectRgn(CloseBtnR.Left, R.Top, CloseBtnR.Right, CloseBtnR.Top);
        CombineRgn(rgn1, rgn1, rgn2, RGN_OR);
        DeleteObject(rgn2);
        rgn2 := CreateRectRgn(CloseBtnR.Left, CloseBtnR.Bottom, CloseBtnR.Right, R.Bottom);
        CombineRgn(rgn1, rgn1, rgn2, RGN_OR);
        DeleteObject(rgn2);

        rgn2 := CreateRectRgn(CloseBtnR.Right, R.Top, R.Right, R.Bottom);
        CombineRgn(rgn1, rgn1, rgn2, RGN_OR);
        InvalidateRgn(Handle, Rgn1, True);
        DeleteObject(rgn2);
        DeleteObject(rgn1);
      end
      else if (TabPosition in [tpLeft, tpRight]) then
      begin
        rgn1 := CreateRectRgn(R.Left, R.Top, R.Right, CloseBtnR.Top);
        rgn2 := CreateRectRgn(R.Left, R.Top, CloseBtnR.Left, CloseBtnR.Bottom);
        CombineRgn(rgn1, rgn1, rgn2, RGN_OR);
        DeleteObject(rgn2);
        rgn2 := CreateRectRgn(CloseBtnR.Right, R.Top, R.Right, CloseBtnR.Bottom);
        CombineRgn(rgn1, rgn1, rgn2, RGN_OR);
        DeleteObject(rgn2);

        rgn2 := CreateRectRgn(R.Left, CloseBtnR.Bottom, R.Right, R.Bottom);
        CombineRgn(rgn1, rgn1, rgn2, RGN_OR);
        InvalidateRgn(Handle, Rgn1, True);
        DeleteObject(rgn2);
        DeleteObject(rgn1);
      end;
    end
    else
    begin
      if UseOldDrawing then
      begin
        case TabPosition of
          tpTop, tpBottom: R.Right := R.Right + 2;
          tpLeft:
          begin
            if RotateTabLeftRight then
              R.Top := R.Top - 2
            else
              R.Bottom := R.Bottom + 2
          end;
          tpRight: R.Bottom := R.Bottom + 2
        end;
        InvalidateRect(Handle, @R, True);
      end
      else
        InvalidateRect(Handle, @R, True);
    end;
  end
  else
  begin
    R := GetTabsRect; //Area;
    InvalidateRect(Handle, @R, True);
  end;
  }
  if (TabIndex >= 0) and (TabIndex < FAdvOfficeTabs.Count) then
  begin
    R := GetTabRect(TabIndex);
    if MultiLine and (TabPosition in [tpLeft, tpRight]) then
      InflateRect(R, 0, 2);
  end
  else
    R := GetTabsArea;

  // compensate for glow
  R.Right := R.Right + 4;
  InvalidateRect(Handle, @R, True);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTabSet.OnEnterTab(TabIndex: Integer);
begin
  if not (csDesigning in ComponentState) and Assigned(OnTabMouseEnter) then
    FOnTabMouseEnter(Self, TabIndex);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTabSet.OnExitTab(TabIndex: Integer);
begin
  if not (csDesigning in ComponentState) and Assigned(OnTabMouseLeave) then
    FOnTabMouseLeave(Self, TabIndex);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTabSet.SetDisabledImages(
  const Value: TCustomImageList);
begin
  FDisabledImages := Value;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTabSet.SetFixedTabs(Value: Integer);
begin
  if (FFixedTabs <> Value) then
  begin
    if (Value < 0) or (Value >= FAdvOfficeTabs.Count) then
      Value := 0;

    if (Value > 0) and MultiLine then
    begin
      MessageDlg('Can not have Fixed tabs with multiline.', mtInformation, [mbOK], 0);
      Exit;
    end;

    FFixedTabs := Value;
    if not (csDesigning in ComponentState) then
    begin
      if FFixedTabs > 0 then
        FTabScroller.Position := 0;
      UpdateTabScroller;
    end;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTabSet.SetFloating(Index: integer);
begin
  if (Index < 0) or (Index >= AdvOfficeTabs.Count) then
    raise Exception.Create('Invalid tab index');

  if not Assigned(FFloatingTabs) then
    FFloatingTabs := TDbgList.Create;

  FFloatingTabs.Add(AdvOfficeTabs[Index]);

  AdvOfficeTabs[Index].Collection := nil;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTabSet.UpdateTabScroller;
var
  AnyTabVisible: Boolean;
  i, j: Integer;

  function GetScrollMax: Integer;
  var
    i, j, LastVisTab: Integer;
    R: TRect;
  begin
    Result := 0;
    LastVisTab := 0;
    for i := FAdvOfficeTabs.Count -1 downto 0 do
    begin
      if FAdvOfficeTabs.Items[i].Visible then
      begin
        LastVisTab := i;
        Break;
      end;
    end;

    if (LastVisTab <= 0) then
      Exit;

    i := 0;
    j := 0;

    while (i < FAdvOfficeTabs.Count) do
    begin
      R := GetTabRect(i, LastVisTab, False);

      if not FAdvOfficeTabs.Items[LastVisTab - i].Visible then
        inc(j);

      if ((R.Left > -1) and (R.Right > -1)) or ((TabPosition in [tpLeft, tpRight]) and ((R.Top > -1) and (R.Bottom > -1))) then
      begin
        Result := i - j;

        if (FixedTabs > 0) and not (csDesigning in ComponentState) and (Result > 0) then
          Result := Result - FixedTabs;
        AnyTabVisible := True;
        Break;
      end;

      inc(i);

      if (i >= FAdvOfficeTabs.Count) then
        Break;

      FTabScroller.Visible := True;  // just to be counted in calculation
    end;
  end;

begin
  if (FUpdateCount > 0) or MultiLine then
    Exit;
  AnyTabVisible := False;
  FUseMaxSpace := False;

  if not FButtonSettings.ScrollButtonsAlways then
    FTabScroller.Visible := False;

  FTabScroller.Min := 0;
  FTabScroller.Max := GetScrollMax;

  if (FTabScroller.Max = 0) and not AnyTabVisible then
  begin
    if (FixedTabs > 0) and not (csDesigning in ComponentState) then
    begin
      FTabScroller.Max := max(0, GetVisibleTabCount - FixedTabs);
    end
    else
    begin
      FUseMaxSpace := True;
      FTabScroller.Max := GetScrollMax;
      if (FTabScroller.Max = 0) then
      begin
        //FUseMaxSpace := False;
        FTabScroller.Max := max(0, GetVisibleTabCount-1);
      end;
    end;
  end
  else
  if (AnyTabVisible) then
  begin
    if (FTabScroller.Position > FTabScroller.Max) then
      FTabScroller.Position := FTabScroller.Max;

    if (FixedTabs > 0) and not (csDesigning in ComponentState) then
    begin
      j := 0;
      for i:= 0 to FAdvOfficeTabs.Count -1 do
      begin
        if FAdvOfficeTabs.Items[i].Visible and (I > FixedTabs - 1) then
        begin
          inc(j);
          if (j >= FTabScroller.Position) then
          begin
            j := i;
            Break;
          end;
        end;
      end;

      if (GetTabRect(j).Right = -1) and (FTabScroller.Position > 0) then
      begin
        FUseMaxSpace := True;
        FTabScroller.Max := GetScrollMax;
        if (FTabScroller.Max = 0) then
          FUseMaxSpace := False;
      end;
    end
    else
    begin
      j := 0;
      for i:= 0 to FAdvOfficeTabs.Count -1 do
      begin
        if FAdvOfficeTabs.Items[i].Visible then
        begin
          inc(j);
          if (j >= FTabScroller.Position) then
            Break;
        end;
      end;

      if (GetTabRect(j).Right = -1) and (FTabScroller.Position > 0) then
      begin
        FUseMaxSpace := True;
        FTabScroller.Max := GetScrollMax;
        if (FTabScroller.Max = 0) then
          FUseMaxSpace := False;
      end;
    end;
  end;
  FTabScroller.Visible := (FTabScroller.Min < FTabScroller.Max) or ButtonSettings.ScrollButtonsAlways;
  if (FTabScroller.Position > FTabScroller.Max) then
    FTabScroller.Position := FTabScroller.Max;

  InitializeAndUpdateButtons;
end;

//------------------------------------------------------------------------------

function TAdvCustomOfficeTabSet.PtOnTabScrollLeftBtn(X, Y: integer): Boolean;
var
  P: TPoint;
begin
  P := Point(X, Y);
  Result := PtInRect(GetTabScrollerLeftRect, P);
end;

//------------------------------------------------------------------------------

function TAdvCustomOfficeTabSet.PtOnTabScrollRightBtn(X, Y: integer): Boolean;
var
  P: TPoint;
begin
  P := Point(X, Y);
  Result := PtInRect(GetTabScrollerRightRect, P);
end;

procedure TAdvCustomOfficeTabSet.ResetTabs;
var
  i: integer;
  pg: TOfficeTabCollectionItem;
begin
  // add all floating pages back to pager
  if Assigned(FFloatingTabs) then
  begin
    if FFloatingTabs.Count > 0 then
    begin
      for i := FFloatingTabs.Count - 1 downto 0 do
      begin
        TOfficeTabCollectionItem(FFloatingTabs[i]).Collection := AdvOfficeTabs;
      end;
    end;
    FFloatingTabs.Clear;
  end;

  // unhide hidden tabs
  for i := 0 to AdvOfficeTabs.Count - 1 do
    AdvOfficeTabs[i].Visible := true;

  // reset sequence
  for i := 0 to AdvOfficeTabs.Count - 1 do
  begin
    pg := TabAtTabIndex(i);
    if Assigned(pg) then
      AdvOfficeTabs.Move(pg.Index, i);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTabSet.DrawTabScrollButtons;
var
  R: TRect;
begin
  if (FTabScroller.Visible or (ButtonSettings.CloseButton and not CloseOnTab) or ButtonSettings.TabListButton)
    and (FCurrentOfficeTabSetStyler.ButtonBorderColor <> clNone) then
  begin
    R := GetButtonsRect;
    Canvas.Brush.Style := bsClear;
    if FItones then
    begin
      Canvas.Pen.Color := FTabAppearance.BorderColorSelected;
      Canvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
    end
    else
    begin
      Canvas.Pen.Color := FCurrentOfficeTabSetStyler.ButtonBorderColor;
      Canvas.RoundRect(R.Left, R.Top, R.Right, R.Bottom, 5, 5);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTabSet.EndUpdate;
begin
  if FUpdateCount > 0 then
    dec(FUpdateCount);

  if FUpdateCount = 0 then
  begin
    UpdateTabScroller;
    UpdateMultiLineTabs;
    ActiveTabIndex := FActiveTabIndex;
  end;  
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTabSet.ScrollFirstBtnClick;
begin
  if FTabScroller.Visible then
  begin
    if FTabScroller.CanGoBack then
    begin
      FTabScroller.Position := FTabScroller.Min;
      if not FUseMaxSpace then
        InvalidateTab(-1)
      else
        Invalidate;
    end;
  end;
end;

procedure TAdvCustomOfficeTabSet.ScrollLastBtnClick;
begin
  if FTabScroller.Visible then
  begin
    if FTabScroller.CanGoForward then
    begin
      FTabScroller.Position := FTabScroller.Max;
      if not FUseMaxSpace then
        InvalidateTab(-1)
      else
        Invalidate;
    end;
  end;
end;

procedure TAdvCustomOfficeTabSet.ScrollLeftBtnClick;
begin
  if FTabScroller.Visible then
  begin
    if FTabScroller.CanGoBack then
    begin
      FTabScroller.Position := FTabScroller.Position - 1;
      if not FUseMaxSpace then
        InvalidateTab(-1)
      else
        Invalidate;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTabSet.ScrollRightBtnClick;
begin
  if FTabScroller.Visible then
  begin
    if FTabScroller.CanGoForward then
    begin
      FTabScroller.Position := FTabScroller.Position + 1;
      if not FUseMaxSpace then
        InvalidateTab(-1)
      else
        Invalidate;  
    end;
  end;
end;

//------------------------------------------------------------------------------

function TAdvCustomOfficeTabSet.GetTabScrollerFirstRect: TRect;
begin
  Result := Rect(-1, -1, -1, -1);
  if FTabScroller.Visible then
  begin
    Result := GetTabScrollerRect;
    case TabPosition of
      tpTop, tpBottom:
      begin
        Result.Left := Result.Left + 3;
        Result.Right := Result.Left + ButtonSettings.ButtonSize; // (Result.Right - Result.Left) div 2 - 1;
      end;
      tpLeft, tpRight:
      begin
        //Result.Bottom := Result.Top + (Result.Bottom - Result.Top) div 2 - 1;
        Result.Top := Result.Top + 3;
        Result.Bottom := Result.Top + ButtonSettings.ButtonSize;
      end;
    end;
  end;
end;

function TAdvCustomOfficeTabSet.GetTabScrollerLastRect: TRect;
begin
  Result := Rect(-1, -1, -1, -1);
  if FTabScroller.Visible then
  begin
    Result := GetTabScrollerRightRect;// GetTabScrollerRect;
    if ButtonSettings.LastButton then
    begin
      case TabPosition of
        tpTop, tpBottom:
        begin
          Result.Left := Result.Right + 2;
          Result.Right := Result.Left + ButtonSettings.ButtonSize; // ((Result.Right - Result.Left) div 2);
        end;
        tpLeft, tpRight:
        begin
          //Result.Top := Result.Top + ((Result.Bottom - Result.Top) div 2);
          Result.Top := Result.Bottom + 2;
          Result.Bottom := Result.Top + ButtonSettings.ButtonSize;
        end;
      end;
    end;
  end;
end;

function TAdvCustomOfficeTabSet.GetTabScrollerLeftRect: TRect;
begin
  Result := Rect(-1, -1, -1, -1);
  if FTabScroller.Visible then
  begin
    if ButtonSettings.FirstButton then
    begin
      Result := GetTabScrollerFirstRect;
      case TabPosition of
        tpTop, tpBottom:
        begin
          Result.Left := Result.Right + 3;
          Result.Right := Result.Left + ButtonSettings.ButtonSize; // (Result.Right - Result.Left) div 2 - 1;
        end;
        tpLeft, tpRight:
        begin
          //Result.Bottom := Result.Top + (Result.Bottom - Result.Top) div 2 - 1;
          Result.Top := Result.Bottom + 3;
          Result.Bottom := Result.Top + ButtonSettings.ButtonSize;
        end;
      end;
    end
    else
    begin
      Result := GetTabScrollerRect;
      case TabPosition of
        tpTop, tpBottom:
        begin
          Result.Left := Result.Left + 3;
          Result.Right := Result.Left + ButtonSettings.ButtonSize; // (Result.Right - Result.Left) div 2 - 1;
        end;
        tpLeft, tpRight:
        begin
          //Result.Bottom := Result.Top + (Result.Bottom - Result.Top) div 2 - 1;
          Result.Top := Result.Top + 3;
          Result.Bottom := Result.Top + ButtonSettings.ButtonSize;
        end;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TAdvCustomOfficeTabSet.GetCloseButtonRect(TabIndex: integer): TRect;
var
  i: Integer;
  cbr, tr: TRect;
  ip: TPoint;
begin
  Result := Rect(-1, -1, -1, -1);
  if ButtonSettings.CloseButton then
  begin
    if CloseOnTab and (ShowCloseOnNonSelectedTabs or ((ActiveTabIndex = TabIndex) and not ShowCloseOnNonSelectedTabs)) then
    begin
      case TabPosition of
        tpTop:
        begin
          Result := GetTabRect(TabIndex);
          if (TabSettings.Width > 0) and (TabSettings.alignment = taCenter) and (TabIndex >= 0) then
          begin
            GetCloseBtnImageAndTextRect(TabIndex, cbr, tr, ip);
            Result.Left := cbr.Left;
          end
          else
          begin
            if (CloseOnTabPosition = cpRight) then
              Result.Left := Result.Right - ButtonSettings.ButtonSize - 4 - GetRightRoundingOffset
            else //CloseOnTabPosition = cpLeft
              Result.Left := Result.Left + 4 + GetLeftRoundingOffset;
          end;

          Result.Right := Result.Left + ButtonSettings.ButtonSize;
          Result.Bottom := Result.Bottom - 5;
          Result.Top := Result.Bottom - ButtonSettings.ButtonSize;
        end;
        tpBottom:
        begin
          Result := GetTabRect(TabIndex);
          if (TabSettings.Width > 0) and (TabSettings.alignment = taCenter) and (TabIndex >= 0) then
          begin
            GetCloseBtnImageAndTextRect(TabIndex, cbr, tr, ip);
            Result.Left := cbr.Left;
          end
          else
          begin
            if (CloseOnTabPosition = cpRight) then
              Result.Left := Result.Right - ButtonSettings.ButtonSize - 4 - GetRightRoundingOffset
            else //CloseOnTabPosition = cpLeft
              Result.Left := Result.Left + 4 + GetLeftRoundingOffset;
          end;

          Result.Right := Result.Left + ButtonSettings.ButtonSize;
          Result.Top := Result.Top + 5;
          Result.Bottom := Result.Top + ButtonSettings.ButtonSize;
        end;
        tpLeft:
        begin
          if RotateTabLeftRight then
          begin
            Result := GetTabRect(TabIndex);
            Result.Right := Result.Right - 5;
            Result.Left := Result.Right - ButtonSettings.ButtonSize;
            if (TabSettings.Width > 0) and (TabSettings.alignment = taCenter) and (TabIndex >= 0) then
            begin
              GetCloseBtnImageAndTextRect(TabIndex, cbr, tr, ip);
              Result.Top := cbr.Top;
            end
            else
            begin
              if (CloseOnTabPosition = cpRight) then
                Result.Top := Result.Top + 4 + GetRightRoundingOffset
              else
                Result.Top := Result.Bottom - ButtonSettings.ButtonSize - 4 - GetLeftRoundingOffset;
            end;

            Result.Bottom := Result.Top + ButtonSettings.ButtonSize;
          end
          else
          begin
            Result := GetTabRect(TabIndex);
            i := (Result.Bottom - Result.Top - ButtonSettings.ButtonSize) div 2;

            if (TabSettings.Width > 0) and (TabSettings.alignment = taCenter) and (TabIndex >= 0) then
            begin
              GetCloseBtnImageAndTextRect(TabIndex, cbr, tr, ip);
              Result.Left := cbr.Left;
            end
            else
            begin
              if (CloseOnTabPosition = cpRight) then
                Result.Left := Result.Right - ButtonSettings.ButtonSize - 5
              else //CloseOnTabPosition = cpLeft
                Result.Left := Result.Left + 5;
            end;
            Result.Right := Result.Left + ButtonSettings.ButtonSize;
            Result.Top := Result.Top + i;
            Result.Bottom := Result.Top + ButtonSettings.ButtonSize;
          end;
        end;
        tpRight:
        begin
          if RotateTabLeftRight then
          begin
            Result := GetTabRect(TabIndex);
            Result.Left := Result.Left + 5;
            Result.Right := Result.Left + ButtonSettings.ButtonSize;
            if (TabSettings.Width > 0) and (TabSettings.alignment = taCenter) and (TabIndex >= 0) then
            begin
              GetCloseBtnImageAndTextRect(TabIndex, cbr, tr, ip);
              Result.Bottom := cbr.Bottom;
            end
            else
            begin
              if (CloseOnTabPosition = cpRight) then
                Result.Bottom := Result.Bottom - 4 - GetRightRoundingOffset
              else
                Result.Bottom := Result.Top + ButtonSettings.ButtonSize + 4 + GetLeftRoundingOffset;
            end;
            Result.Top := Result.Bottom - ButtonSettings.ButtonSize;
          end
          else
          begin
            Result := GetTabRect(TabIndex);
            i := (Result.Bottom - Result.Top - ButtonSettings.ButtonSize) div 2;

            if (TabSettings.Width > 0) and (TabSettings.alignment = taCenter) and (TabIndex >= 0) then
            begin
              GetCloseBtnImageAndTextRect(TabIndex, cbr, tr, ip);
              Result.Left := cbr.Left;
            end
            else
            begin
              if (CloseOnTabPosition = cpRight) then
                Result.Left := Result.Right - ButtonSettings.ButtonSize - 3
              else //CloseOnTabPosition = cpLeft
                Result.Left := Result.Left + 5;
            end;
            Result.Right := Result.Left + ButtonSettings.ButtonSize;
            Result.Top := Result.Top + i;
            Result.Bottom := Result.Top + ButtonSettings.ButtonSize;
          end;
        end;
      end;
    end
    else
    begin
      case TabPosition of
        tpTop:
        begin
          if ButtonSettings.TabListButton then
          begin
            Result := GetTabListRect;
            Result.Left := Result.Right + 2;
            Result.Right := Result.Left + ButtonSettings.ButtonSize;
          end
          else if ButtonSettings.ClosedListButton and (FClosedTabList.Count > 0) then
          begin
            Result := GetClosedListButtonRect;
            Result.Left := Result.Right + 2;
            Result.Right := Result.Left + ButtonSettings.ButtonSize;
          end
          else if FTabScroller.Visible then
          begin
            Result := GetTabScrollerLastRect;
            Result.Left := Result.Right + 3;
            Result.Right := Result.Left + ButtonSettings.ButtonSize;
          end
          else
          begin
            Result := GetButtonsRect; // GetTabsRect;
            Result.Left := Result.Left + 3;
            Result.Right := Result.Left + ButtonSettings.ButtonSize;
            Result.Bottom := Result.Bottom - 5;
            Result.Top := Result.Bottom - ButtonSettings.ButtonSize;
          end;
        end;
        tpBottom:
        begin
          if ButtonSettings.TabListButton then
          begin
            Result := GetTabListRect;
            Result.Left := Result.Right + 2;
            Result.Right := Result.Left + ButtonSettings.ButtonSize;
          end
          else if ButtonSettings.ClosedListButton and (FClosedTabList.Count > 0) then
          begin
            Result := GetClosedListButtonRect;
            Result.Left := Result.Right + 2;
            Result.Right := Result.Left + ButtonSettings.ButtonSize;
          end
          else if FTabScroller.Visible then
          begin
            Result := GetTabScrollerLastRect;
            Result.Left := Result.Right + 3;
            Result.Right := Result.Left + ButtonSettings.ButtonSize;
          end
          else
          begin
            Result := GetButtonsRect; // GetTabsRect;
            Result.Left := Result.Left + 3;
            Result.Right := Result.Left + ButtonSettings.ButtonSize;
            Result.Top := Result.Top + 5;
            Result.Bottom := Result.Top + ButtonSettings.ButtonSize;
          end;
        end;
        tpLeft:
        begin
          if ButtonSettings.TabListButton then
          begin
            Result := GetTabListRect;
            Result.Top := Result.Bottom + 2;
            Result.Bottom := Result.Top + ButtonSettings.ButtonSize;
          end
          else if ButtonSettings.ClosedListButton and (FClosedTabList.Count > 0) then
          begin
            Result := GetClosedListButtonRect;
            Result.Top := Result.Bottom + 2;
            Result.Bottom := Result.Top + ButtonSettings.ButtonSize;
          end
          else if FTabScroller.Visible then
          begin
            Result := GetTabScrollerLastRect;
            Result.Top := Result.Bottom + 3;
            Result.Bottom := Result.Top + ButtonSettings.ButtonSize;
          end
          else
          begin
            Result := GetButtonsRect; // GetTabsRect;
            Result.Top := Result.Top + 3;
            Result.Bottom := Result.Top + ButtonSettings.ButtonSize;
            Result.Right := Result.Right - 5;
            Result.Left := Result.Right - ButtonSettings.ButtonSize;
          end;
        end;
        tpRight:
        begin
          if ButtonSettings.TabListButton then
          begin
            Result := GetTabListRect;
            Result.Top := Result.Bottom + 2;
            Result.Bottom := Result.Top + ButtonSettings.ButtonSize;
          end
          else if ButtonSettings.ClosedListButton and (FClosedTabList.Count > 0) then
          begin
            Result := GetClosedListButtonRect;
            Result.Top := Result.Bottom + 2;
            Result.Bottom := Result.Top + ButtonSettings.ButtonSize;
          end
          else if FTabScroller.Visible then
          begin
            Result := GetTabScrollerLastRect;
            Result.Top := Result.Bottom + 3;
            Result.Bottom := Result.Top + ButtonSettings.ButtonSize;
          end
          else
          begin
            Result := GetButtonsRect; // GetTabsRect;
            Result.Top := Result.Top + 3;
            Result.Bottom := Result.Top + ButtonSettings.ButtonSize;
            Result.Left := Result.Left + 5;
            Result.Right := Result.Left + ButtonSettings.ButtonSize;
          end;
        end;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TAdvCustomOfficeTabSet.GetClosedListButtonRect: TRect;
begin
  Result := Rect(-1, -1, -1, -1);
  if ButtonSettings.ClosedListButton and (FClosedTabList.Count > 0) then
  begin
    case TabPosition of
      tpTop:
      begin
        if FTabScroller.Visible then
        begin
          Result := GetTabScrollerLastRect; // GetTabScrollerRect;
          Result.Left := Result.Right + 3;
          Result.Right := Result.Left + ButtonSettings.ButtonSize;
        end
        else
        begin
          Result := GetButtonsRect; // GetTabsRect;
          Result.Left := Result.Left + 3;
          Result.Right := Result.Left + ButtonSettings.ButtonSize;
          Result.Bottom := Result.Bottom - 5;
          Result.Top := Result.Bottom - ButtonSettings.ButtonSize;
        end;
      end;
      tpBottom:
      begin
        if FTabScroller.Visible then
        begin
          Result := GetTabScrollerLastRect;
          Result.Left := Result.Right + 3;
          Result.Right := Result.Left + ButtonSettings.ButtonSize;
        end
        else
        begin
          Result := GetButtonsRect;
          Result.Left := Result.Left + 3;
          Result.Right := Result.Left + ButtonSettings.ButtonSize;
          Result.Top := Result.Top + 5;
          Result.Bottom := Result.Top + ButtonSettings.ButtonSize;
        end;
      end;
      tpLeft:
      begin
        if FTabScroller.Visible then
        begin
          Result := GetTabScrollerLastRect;
          Result.Top := Result.Bottom + 3;
          Result.Bottom := Result.Top + ButtonSettings.ButtonSize;
        end
        else
        begin
          Result := GetButtonsRect;
          Result.Top := Result.Top + 3;
          Result.Bottom := Result.Top + ButtonSettings.ButtonSize;
          Result.Right := Result.Right - 5;
          Result.Left := Result.Right - ButtonSettings.ButtonSize;
        end;
      end;
      tpRight:
      begin
        if FTabScroller.Visible then
        begin
          Result := GetTabScrollerLastRect;
          Result.Top := Result.Bottom + 3;
          Result.Bottom := Result.Top + ButtonSettings.ButtonSize;
        end
        else
        begin
          Result := GetButtonsRect;
          Result.Top := Result.Top + 3;
          Result.Bottom := Result.Top + ButtonSettings.ButtonSize;
          Result.Left := Result.Left + 5;
          Result.Right := Result.Left + ButtonSettings.ButtonSize;
        end;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TAdvCustomOfficeTabSet.GetTabListRect: TRect;
begin
  Result := Rect(-1, -1, -1, -1);
  if ButtonSettings.TabListButton then
  begin
    case TabPosition of
      tpTop:
      begin
        if ButtonSettings.ClosedListButton and (FClosedTabList.Count > 0) then
        begin
          Result := GetClosedListButtonRect;
          Result.Left := Result.Right + 3;
          Result.Right := Result.Left + ButtonSettings.ButtonSize;
        end
        else if FTabScroller.Visible then
        begin
          Result := GetTabScrollerLastRect; // GetTabScrollerRect;
          Result.Left := Result.Right + 3;
          Result.Right := Result.Left + ButtonSettings.ButtonSize;
        end
        else
        begin
          Result := GetButtonsRect; // GetTabsRect;
          Result.Left := Result.Left + 3;
          Result.Right := Result.Left + ButtonSettings.ButtonSize;
          Result.Bottom := Result.Bottom - 5;
          Result.Top := Result.Bottom - ButtonSettings.ButtonSize;
        end;
      end;
      tpBottom:
      begin
        if ButtonSettings.ClosedListButton and (FClosedTabList.Count > 0) then
        begin
          Result := GetClosedListButtonRect;
          Result.Left := Result.Right + 3;
          Result.Right := Result.Left + ButtonSettings.ButtonSize;
        end
        else if FTabScroller.Visible then
        begin
          Result := GetTabScrollerLastRect;
          Result.Left := Result.Right + 3;
          Result.Right := Result.Left + ButtonSettings.ButtonSize;
        end
        else
        begin
          Result := GetButtonsRect;
          Result.Left := Result.Left + 3;
          Result.Right := Result.Left + ButtonSettings.ButtonSize;
          Result.Top := Result.Top + 5;
          Result.Bottom := Result.Top + ButtonSettings.ButtonSize;
        end;
      end;
      tpLeft:
      begin
        if ButtonSettings.ClosedListButton and (FClosedTabList.Count > 0) then
        begin
          Result := GetClosedListButtonRect;
          Result.Top := Result.Bottom + 3;
          Result.Bottom := Result.Top + ButtonSettings.ButtonSize;
        end
        else if FTabScroller.Visible then
        begin
          Result := GetTabScrollerLastRect;
          Result.Top := Result.Bottom + 3;
          Result.Bottom := Result.Top + ButtonSettings.ButtonSize;
        end
        else
        begin
          Result := GetButtonsRect;
          Result.Top := Result.Top + 3;
          Result.Bottom := Result.Top + ButtonSettings.ButtonSize;
          Result.Right := Result.Right - 5;
          Result.Left := Result.Right - ButtonSettings.ButtonSize;
        end;
      end;
      tpRight:
      begin
        if ButtonSettings.ClosedListButton and (FClosedTabList.Count > 0) then
        begin
          Result := GetClosedListButtonRect;
          Result.Top := Result.Bottom + 3;
          Result.Bottom := Result.Top + ButtonSettings.ButtonSize;
        end
        else if FTabScroller.Visible then
        begin
          Result := GetTabScrollerLastRect;
          Result.Top := Result.Bottom + 3;
          Result.Bottom := Result.Top + ButtonSettings.ButtonSize;
        end
        else
        begin
          Result := GetButtonsRect;
          Result.Top := Result.Top + 3;
          Result.Bottom := Result.Top + ButtonSettings.ButtonSize;
          Result.Left := Result.Left + 5;
          Result.Right := Result.Left + ButtonSettings.ButtonSize;
        end;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TAdvCustomOfficeTabSet.GetTabScrollerRect: TRect;
begin
  Result := Rect(-1, -1, -1, -1);
  if FTabScroller.Visible then
  begin
    Result := GetButtonsRect;
    case TabPosition of
      tpTop:
      begin
        Result.Right := Result.Left + 3 +(ButtonSettings.ButtonSize*2 + 2+ 3);// SCROLLER_SIZE;
        Result.Bottom := Result.Bottom - 5;
        Result.Top := Result.Bottom - ButtonSettings.ButtonSize;
      end;
      tpBottom:
      begin
        Result.Right := Result.Left + 3 +(ButtonSettings.ButtonSize*2 + 2+ 3);// SCROLLER_SIZE;
        Result.Top := Result.Top + 5;
        Result.Bottom := Result.Top + ButtonSettings.ButtonSize;
      end;
      tpLeft:
      begin
        Result.Bottom := Result.Top + 3 +(ButtonSettings.ButtonSize*2 + 2+ 3);
        Result.Right := Result.Right - 5;
        Result.Left := Result.Right - ButtonSettings.ButtonSize;
      end;
      tpRight:
      begin
        Result.Bottom := Result.Top + 3 +(ButtonSettings.ButtonSize*2 + 2+ 3);
        Result.Left := Result.Left + 5;
        Result.Right := Result.Left + ButtonSettings.ButtonSize;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TAdvCustomOfficeTabSet.GetTabScrollerRightRect: TRect;
begin
  Result := Rect(-1, -1, -1, -1);
  if FTabScroller.Visible then
  begin
    Result := GetTabScrollerLeftRect;// GetTabScrollerRect;
    case TabPosition of
      tpTop, tpBottom:
      begin
        Result.Left := Result.Right + 2;
        Result.Right := Result.Left + ButtonSettings.ButtonSize; // ((Result.Right - Result.Left) div 2);
      end;
      tpLeft, tpRight:
      begin
        //Result.Top := Result.Top + ((Result.Bottom - Result.Top) div 2);
        Result.Top := Result.Bottom + 2;
        Result.Bottom := Result.Top + ButtonSettings.ButtonSize;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTabSet.CMHintShow(var Message: TMessage);
var
  PHI: PHintInfo;
begin
  PHI := TCMHintShow(Message).HintInfo;

  if PtOnInsertButton(PHI^.CursorPos.X, PHI^.CursorPos.Y) then
  begin
    PHI^.HintStr := ButtonSettings.InsertButtonHint;
  end
  else
  begin
    if ShowTabHint then
    begin
      if (FHintTabIndex >= 0) then
      begin
        PHI^.HintStr := FAdvOfficeTabs.Items[FHintTabIndex].OfficeHint.Title;
        FDummyHintControl.OfficeHint.Assign(FAdvOfficeTabs.Items[FHintTabIndex].OfficeHint);
        PHI^.HintControl := FDummyHintControl;
      end;
    end
    else
      PHI^.HintStr := '';
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTabSet.OnTabSettingsChanged(Sender: TObject);
begin
  InitializeAndUpdateButtons;
  UpdateTabScroller;
  UpdateMultiLineTabs;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTabSet.OpenAllClosedTabs;
var
  i: integer;
begin
  for i := 0 to FClosedTabList.Count - 1 do
  begin
    TOfficeTabCollectionItem(FClosedTabList.Objects[i]).Visible := True;
  end;
  FClosedTabList.Clear;
  UpdateClosedListButton;
end;

//------------------------------------------------------------------------------

function TAdvCustomOfficeTabSet.OpenClosedTab(ATab: TOfficeTabCollectionItem): Boolean;
var
  i: integer;
begin
  Result := false;
  if not Assigned(ATab) then
    Exit;

  i := FClosedTabList.IndexOfObject(ATab);
  if i >= 0 then
  begin
    TOfficeTabCollectionItem(FClosedTabList.Objects[i]).Visible := True;
    Result := true;
    if (GetVisibleTabCount = 1) then
      ActiveTabIndex := ATab.Index;

    UpdateClosedListButton;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTabSet.CreateClosedListButton;
begin
  if not Assigned(FClosedListButton) then
  begin
    FClosedListButton := TAdvGlowButton.Create(Self);
    FClosedListButton.Parent := Self;
    FClosedListButton.OnClick := ClosedListButtonClick;
  end;
  UpdateClosedListButton;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTabSet.CreateGlowTimer;
begin
  if not Assigned(FBrGlowTimer) then
  begin
    FBrGlowTimer := TTimer.Create(self);
    FBrGlowTimer.OnTimer := OnGlowTimerTime;
    FBrGlowTimer.Interval := GlowSpeed + 20;
    FBrGlowTimer.Enabled := False;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTabSet.UpdateGlowTimer;
begin
  if not (csDesigning in ComponentState) and CanGlow and IsAnyTabGlowing then
  begin
    CreateGlowTimer;
    if not FBrGlowTimer.Enabled then
    begin
      FTimeInc := 20;
      FStepHover := 0;
      FBrGlowTimer.Enabled := True;
    end;
  end
  else if Assigned(FBrGlowTimer) and FBrGlowTimer.Enabled then
  begin
    FBrGlowTimer.Enabled := False;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTabSet.OnGlowTimerTime(Sender: TObject);
var
  I: Integer;
  //Tabfound: Boolean;
begin
  if not (csDesigning in ComponentState) then  
  begin
    FStepHover := FStepHover + FTimeInc;
    if ((FStepHover > 140) and (FTimeInc > 0)) or ((FStepHover < 0) and (FTimeInc < 0)) then
    begin
      if (FStepHover > 140) and (FTimeInc > 0) then
      begin
        FTimeInc := -FTimeInc;
      end
      else if ((FStepHover < 0) and (FTimeInc < 0)) then
      begin
        FTimeInc := -FTimeInc;
      end;
    end;

    //Tabfound := False;
    for I := 0 to AdvOfficeTabs.Count - 1 do
    begin
      if not AdvOfficeTabs[I].Visible or not AdvOfficeTabs[I].Enabled then
        Continue;

      if AdvOfficeTabs[I].Glow and (AdvOfficeTabs[I].GlowColor <> clNone) and ((I = ActiveTabIndex) or (I = FHotTabIndex) or ShowNonSelectedTabs) then
      begin
        InvalidateTab(I);
        //Tabfound := True;
      end;
    end;

    //InvalidateTab(-1);
    //FBrGlowTimer.Enabled := Tabfound;
  end;
end;

//------------------------------------------------------------------------------

function TAdvCustomOfficeTabSet.IsAnyTabGlowing: Boolean;
var
  i: Integer;
begin
  Result := False;
  for I := 0 to AdvOfficeTabs.Count - 1 do
  begin
    if not AdvOfficeTabs[I].Visible or not AdvOfficeTabs[I].Enabled then
      Continue;

    if AdvOfficeTabs[I].Glow and (AdvOfficeTabs[I].GlowColor <> clNone) and IsTabShowing(I) then
    begin
      Result := True;
      Break;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TAdvCustomOfficeTabSet.IsTabShowing(TabIndex: Integer): Boolean;
begin
  Result := CanShowTab(TabIndex);
  if Result then
  begin
    Result := GetTabRect(TabIndex).Left >= 0;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTabSet.UpdateClosedListButton;
var
  R: TRect;
begin
  if not Assigned(FClosedListButton) then
    Exit;

  FClosedListButton.Width := ButtonSettings.ButtonSize;
  FClosedListButton.Height := ButtonSettings.ButtonSize;
  FClosedListButton.Visible := FClosedTabList.Count > 0;

  if FClosedListButton.Visible then
  begin
    if Assigned(FGlowButtonAppearance) and FItones then
      SetGlowButtonColorTones(FClosedListButton)
    else if Assigned(FCurrentOfficeTabSetStyler) then
      FClosedListButton.Appearance.Assign(FCurrentOfficeTabSetStyler.GlowButtonAppearance);
    FClosedListButton.Picture.Assign(ButtonSettings.ClosedListButtonPicture);
    FClosedListButton.Hint := ButtonSettings.ClosedListButtonHint;
    FClosedListButton.ShowHint := True;

    R := GetClosedListButtonRect;
    FClosedListButton.Left := R.Left;
    FClosedListButton.Top := R.Top;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTabSet.UpdateMe(PropID: integer);
begin
  UpdateTabAppearanceOfTabs;
  Invalidate;

  case PropID of
    2, 4: InitializeAndUpdateButtons;
  end;

  if FItones then
  begin
    FItones := False;
    FTabRoundEdges := True;
    FShow3D := True;
    FShadow := True;
    InitializeAndUpdateButtons;
  end;

  if Assigned(AdvOfficeTabSetStyler) then
  begin
    TabSettings.Rounding := AdvOfficeTabSetStyler.TabRounding;
  end;

end;

//------------------------------------------------------------------------------

function TAdvCustomOfficeTabSet.IsOnGlass: Boolean;
begin
  Result := False;
  {$IFDEF DELPHI2007_LVL}
  if Assigned(Parent) and (Parent is TCustomForm) and TCustomForm(Parent).GlassFrame.Enabled then
    Result := IsGlass and (Align in [alTop, alBottom, alClient]);
  {$ENDIF}
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTabSet.WMNCHitTest(var Msg: TWMNCHitTest);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTabSet.WMPaint(var Message: TWMPaint);
{$IFDEF DELPHI2007_LVL}
var
  DC, MemDC: HDC;
  PS: TPaintStruct;
  PaintBuffer: HPAINTBUFFER;
{$ENDIF}
begin
  {$IFNDEF DELPHI2007_LVL}
  inherited;
  {$ENDIF}
  {$IFDEF DELPHI2007_LVL}
  if not IsGlass or not DwmCompositionEnabled or not Transparent then
    inherited
  else
  begin
    DC := BeginPaint(Handle, PS);
    try
      PaintBuffer := BeginBufferedPaint(DC, PS.rcPaint, BPBF_COMPOSITED, nil, MemDC);
      if PaintBuffer <> 0 then
        try
          Perform(WM_ERASEBKGND, MemDC, MemDC);
          // ssanders: Opaque before Paint() Glass behind tabs
          BufferedPaintMakeOpaque(PaintBuffer, @PS.rcPaint);
          Perform(WM_PRINTCLIENT, MemDC, PRF_CLIENT);
        finally
          EndBufferedPaint(PaintBuffer, True);
        end;
    finally
      EndPaint(Handle, PS);
    end;
  end;
  {$ENDIF}
end;

//------------------------------------------------------------------------------

function TAdvCustomOfficeTabSet.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTabSet.SetVersion(const Value: string);
begin

end;

function TAdvCustomOfficeTabSet.TabAtTabIndex(
  Value: integer): TOfficeTabCollectionItem;
var
  i: integer;
begin
  Result := nil;

  for i := 0 to AdvOfficeTabs.Count - 1 do
  begin
    if AdvOfficeTabs[i].TabIndex = Value then
    begin
      Result := AdvOfficeTabs[i];
      break;
    end;
  end;
end;

function TAdvCustomOfficeTabSet.TabByCaption(
  Value: string): TOfficeTabCollectionItem;
var
  i: integer;
begin
  Result := nil;

  for i := 0 to AdvOfficeTabs.Count - 1 do
  begin
    if Uppercase(AdvOfficeTabs[i].Caption) = Uppercase(Value) then
    begin
      Result := AdvOfficeTabs[i];
      break;
    end;
  end;

end;

function TAdvCustomOfficeTabSet.TabByName(
  Value: string): TOfficeTabCollectionItem;
var
  i: integer;
begin
  Result := nil;

  for i := 0 to AdvOfficeTabs.Count - 1 do
  begin
    if Uppercase(AdvOfficeTabs[i].Caption) = Uppercase(Value) then
    begin
      Result := AdvOfficeTabs[i];
      break;
    end;
  end;

end;

//------------------------------------------------------------------------------

function TAdvCustomOfficeTabSet.GetVersionNr: integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTabSet.SetOfficeHint(const Value: TAdvHintInfo);
begin
  FOfficeHint.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTabSet.SetTabPosition(const Value: TTabPosition);
begin
  if (FTabPosition <> Value) then
  begin
    FTabPosition := Value;
    UpdateTabScroller;
    UpdateMultiLineTabs;
    Invalidate;

    if Assigned(FArrow) then
      FreeAndNil(FArrow);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTabSet.CMDialogChar(var Message: TCMDialogChar);
var
  I: Integer;
begin
  for I:= 0 to FAdvOfficeTabs.Count-1 do
  begin
    if IsAccel(Message.CharCode, FAdvOfficeTabs.Items[I].Caption) and CanShowTab(I) and CanFocus then
    begin
      Message.Result := 1;
      ActiveTabIndex := I;
      Exit;
    end;
  end;
  inherited;
end;

//------------------------------------------------------------------------------

function TAdvCustomOfficeTabSet.CanShowTab(TabIndex: Integer): Boolean;
begin
  Result := (TabIndex >= 0) and (TabIndex < FAdvOfficeTabs.Count) and (FAdvOfficeTabs.Items[TabIndex].visible);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTabSet.SetAntiAlias(const Value: TAntiAlias);
begin
  FAntiAlias := Value;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTabSet.SetButtonSettings(
  const Value: TTabSetButtonSettings);
begin
  FButtonSettings.Assign(Value);
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTabSet.OnButtonSettingChanged(Sender: TObject);
begin
  if FButtonSettings.ClosedListButton and not (csDesigning in ComponentState) then
  begin
    CreateClosedListButton;
    UpdateClosedListButton;
  end;

  UpdateTabScroller;
  UpdateMultiLineTabs;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTabSet.SetTabListMenu(const Value: TPopupMenu);
begin
  FTabListMenu := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTabSet.SetCloseOnTab(const Value: Boolean);
begin
  if (FCloseOnTab <> Value) then
  begin
    FCloseOnTab := Value;
    UpdateTabScroller;
    UpdateMultiLineTabs;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTabSet.SetRotateTabLeftRight(const Value: Boolean);
begin
  if (FRotateTabLeftRight <> Value) then
  begin
    if not Value and MultiLine then
    begin
      MessageDlg('RotateTabLeftRight must be True with MultiLine.', mtInformation, [mbOK], 0);
      Exit;
    end;

    FRotateTabLeftRight := Value;
    UpdateTabScroller;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTabSet.SetAdvOfficeTabs(Value: TOfficeTabCollection);
begin
  FAdvOfficeTabs.Assign(Value);
end;

//------------------------------------------------------------------------------

function TAdvCustomOfficeTabSet.GetVisibleTabCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to FAdvOfficeTabs.Count-1 do
  begin
    if (FAdvOfficeTabs.Items[I].visible) then
      Result := Result + 1;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTabSet.InitializeAndUpdateButtons;
var
  R: TRect;
  I: Integer;
begin
  if (not FPropertiesLoaded) and not (csDesigning in ComponentState) then
    Exit;
  if (csDestroying in ComponentState) then
    Exit;


  if ButtonSettings.CloseButton{ and not CloseOnTab} and (CanShowCloseButton or not CloseOnTab) then
  begin
    if not CloseOnTab then
    begin
      for I := 0 to AdvOfficeTabs.Count - 1 do
      begin
        with AdvOfficeTabs[I] do
        begin
          if Assigned(FCloseButton) then
            FCloseButton.Width := 0;
        end;
      end;
      if (FCloseButtonGlobal = nil) then
      begin
        FCloseButtonGlobal := TAdvGlowButton.Create(Self);
        FCloseButtonGlobal.Parent := Self;
        FCloseButtonGlobal.OnClick := OnCloseButtonClick;
      end;

      R := GetCloseButtonRect(-1);
      FCloseButtonGlobal.Left := R.Left;
      FCloseButtonGlobal.Top := R.Top;
      FCloseButtonGlobal.Width := R.Right - R.Left;
      FCloseButtonGlobal.Height := R.Bottom - R.Top;
      if Assigned(FGlowButtonAppearance) and FItones then
        SetGlowButtonColorTones(FCloseButtonGlobal)
      else if Assigned(FCurrentOfficeTabSetStyler) then
        FCloseButtonGlobal.Appearance.Assign(FCurrentOfficeTabSetStyler.GlowButtonAppearance);
      FCloseButtonGlobal.Picture.Assign(ButtonSettings.CloseButtonPicture);
      FCloseButtonGlobal.Hint := ButtonSettings.CloseButtonHint;
      FCloseButtonGlobal.ShowHint := True;
      FCloseButtonGlobal.Enabled := GetVisibleTabCount > 0; //(ActivePage <> nil);
      if FCloseButtonGlobal.Enabled then
        if not CloseOnTab then
          FCloseButtonGlobal.Enabled := CanShowCloseButton;
    end
    else
    begin
      if Assigned(FCloseButtonGlobal) then
        FCloseButtonGlobal.Width := 0;


      for I := 0 to AdvOfficeTabs.Count - 1 do
      begin
        with AdvOfficeTabs[I] do
        begin

          if ShowClose and not FDestroying then
          begin
            R := GetCloseButtonRect(I);

            if (ButtonSettings.CloseButtonLook = cblOffice) then
            begin
              if Assigned(FCloseButtonChrome) then
                FCloseButtonChrome.Visible := false;

              if (FCloseButtonChrome <> nil) then
              begin
                PostMessage(Self.Handle, WM_OTSDESTROYCLOSEBTN, Integer(Pointer(FCloseButtonChrome)), 1);
                FCloseButtonChrome := nil;
              end;

              if (FCloseButton = nil) then
              begin
                FCloseButton := TAdvGlowButton.Create(Self);
                FCloseButton.Parent := Self;
                FCloseButton.OnClick := OnCloseButtonClick;
              end;

              FCloseButton.Left := R.Left;
              FCloseButton.Top := R.Top;
              FCloseButton.Width := R.Right - R.Left;
              FCloseButton.Height := R.Bottom - R.Top;
              FCloseButton.Tag := I;

              if Assigned(FGlowButtonAppearance) and FItones then
                SetGlowButtonColorTones(FCloseButton)
              else if Assigned(FCurrentOfficeTabSetStyler) then
                FCloseButton.Appearance.Assign(FCurrentOfficeTabSetStyler.GlowButtonAppearance);
              FCloseButton.Picture.Assign(ButtonSettings.CloseButtonPicture);
              FCloseButton.Hint := ButtonSettings.CloseButtonHint;
              FCloseButton.ShowHint := True;
              FCloseButton.Enabled := GetVisibleTabCount > 0; //(ActivePage <> nil);
              if FCloseButton.Enabled then
                if not CloseOnTab then
                  FCloseButton.Enabled := CanShowCloseButton;

              FCloseButton.Visible := ShowCloseOnNonSelectedTabs or ((ActiveTabIndex = I) and not ShowCloseOnNonSelectedTabs);
              if not FCloseButton.Visible then
                FCloseButton.Width := 0;
            end
            else //if (ButtonSettings.CloseButtonLook = cblChrome) then
            begin
              if Assigned(FCloseButton) then
                FCloseButton.Visible := false;

              if (FCloseButton <> nil) then
              begin
                PostMessage(Self.Handle, WM_OTSDESTROYCLOSEBTN, Integer(Pointer(FCloseButton)), 0);
                FCloseButton := nil;
              end;

              if (FCloseButtonChrome = nil) then
              begin
                FCloseButtonChrome := TAdvTabSetButton.Create(Self);
                FCloseButtonChrome.Parent := Self;
                FCloseButtonChrome.OnClick := OnCloseButtonClick;
                //FCloseButton.OnMouseUp := OnCloseButtonMouseUp;
              end;

              FCloseButtonChrome.Left := R.Left;
              FCloseButtonChrome.Top := R.Top;
              FCloseButtonChrome.Width := R.Right - R.Left;
              FCloseButtonChrome.Height := R.Bottom - R.Top;
              FCloseButtonChrome.Tag := I;


              FCloseButtonChrome.Hint := ButtonSettings.CloseButtonHint;
              FCloseButtonChrome.ShowHint := True;
              FCloseButtonChrome.Enabled := GetVisibleTabCount > 0; //(ActivePage <> nil);
              if FCloseButtonChrome.Enabled then
                if not CloseOnTab then
                  FCloseButtonChrome.Enabled := CanShowCloseButton;

              FCloseButtonChrome.Visible := ShowCloseOnNonSelectedTabs or ((ActiveTabIndex = I) and not ShowCloseOnNonSelectedTabs and ShowClose);
              if not FCloseButtonChrome.Visible then
                FCloseButtonChrome.Width := 0;
            end;
          end;
        end;
      end;
    end;
  end
  else
  for I := 0 to AdvOfficeTabs.Count - 1 do
  begin
    with AdvOfficeTabs[I] do
    begin
      if (FCloseButton <> nil) then
      begin
        PostMessage(Handle, WM_OTSDESTROYCLOSEBTN, Integer(Pointer(FCloseButton)), 0);
        FCloseButton := nil;
      end;

      if (FCloseButtonChrome <> nil) then
      begin
        PostMessage(Handle, WM_OTSDESTROYCLOSEBTN, Integer(Pointer(FCloseButtonChrome)), 1);
        FCloseButtonChrome := nil;
      end;
    end;
  end;

  UpdateClosedListButton;

  if ButtonSettings.TabListButton then
  begin
    if (FTabListButton = nil) then
    begin
      FTabListButton := TAdvGlowButton.Create(Self);
      FTabListButton.Parent := Self;
      FTabListButton.OnClick := OnTabListButtonClick;
    end;

    R := GetTabListRect;
    FTabListButton.Left := R.Left;
    FTabListButton.Top := R.Top;
    FTabListButton.Width := R.Right - R.Left;
    FTabListButton.Height := R.Bottom - R.Top;
    if Assigned(FGlowButtonAppearance) and FItones then
      SetGlowButtonColorTones(FTabListButton)
    else if Assigned(FCurrentOfficeTabSetStyler) then
      FTabListButton.Appearance.Assign(FCurrentOfficeTabSetStyler.GlowButtonAppearance);
    FTabListButton.Picture.Assign(ButtonSettings.TabListButtonPicture);
    FTabListButton.Hint := ButtonSettings.TabListButtonHint;
    FTabListButton.ShowHint := True;
    FTabListButton.Enabled := GetVisibleTabCount > 0; //(AdvPageCount > 0);
  end
  else
  if (FTabListButton <> nil) then
  begin
    FTabListButton.Free;
    FTabListButton := nil;
  end;

  if (FTabScroller.Visible) then
  begin
    if (FScrollFirstButton = nil) then
    begin
      FScrollFirstButton := TAdvGlowButton.Create(Self);
      FScrollFirstButton.Parent := Self;
      FScrollFirstButton.OnClick := OnScrollFirstButtonClick;
    end;
    R := GetTabScrollerFirstRect;
    FScrollFirstButton.Left := R.Left;
    FScrollFirstButton.Top := R.Top;
    FScrollFirstButton.Width := R.Right - R.Left;
    FScrollFirstButton.Height := R.Bottom - R.Top;
    if Assigned(FGlowButtonAppearance) and FItones then
      SetGlowButtonColorTones(FScrollFirstButton)
    else if Assigned(FCurrentOfficeTabSetStyler) then
      FScrollFirstButton.Appearance.Assign(FCurrentOfficeTabSetStyler.GlowButtonAppearance);
    if (TabPosition in [tpLeft, tpRight]) {and not RotateTabLeftRight} then
    begin
      if ButtonSettings.FFirstPictureChanged then
        FScrollFirstButton.Picture.Assign(ButtonSettings.ScrollButtonFirstPicture)
      else
        FScrollFirstButton.Picture.Assign(ButtonSettings.FScrollButtonUpFirstPicture);
    end
    else
      FScrollFirstButton.Picture.Assign(ButtonSettings.ScrollButtonFirstPicture);
    FScrollFirstButton.Hint := ButtonSettings.ScrollButtonFirstHint;
    FScrollFirstButton.ShowHint := True;
    FScrollFirstButton.Enabled := FTabScroller.CanGoBack;
    FScrollFirstButton.RepeatClick := True;
    FScrollFirstButton.Visible := ButtonSettings.FirstButton;

    if (FScrollLastButton = nil) then
    begin
      FScrollLastButton := TAdvGlowButton.Create(Self);
      FScrollLastButton.Parent := Self;
      FScrollLastButton.OnClick := OnScrollLastButtonClick;
    end;
    R := GetTabScrollerLastRect;
    FScrollLastButton.Left := R.Left;
    FScrollLastButton.Top := R.Top;
    FScrollLastButton.Width := R.Right - R.Left;
    FScrollLastButton.Height := R.Bottom - R.Top;
    if Assigned(FGlowButtonAppearance) and FItones then
      SetGlowButtonColorTones(FScrollLastButton)
    else if Assigned(FCurrentOfficeTabSetStyler) then
      FScrollLastButton.Appearance.Assign(FCurrentOfficeTabSetStyler.GlowButtonAppearance);
    if (TabPosition in [tpLeft, tpRight]) {and not RotateTabLeftRight} then
    begin
      if ButtonSettings.FLastPictureChanged then
        FScrollLastButton.Picture.Assign(ButtonSettings.ScrollButtonLastPicture)
      else
        FScrollLastButton.Picture.Assign(ButtonSettings.FScrollButtonDownLastPicture);
    end
    else
      FScrollLastButton.Picture.Assign(ButtonSettings.ScrollButtonLastPicture);
    FScrollLastButton.Hint := ButtonSettings.ScrollButtonLastHint;
    FScrollLastButton.ShowHint := True;
    FScrollLastButton.Enabled := FTabScroller.CanGoForward;
    FScrollLastButton.RepeatClick := True;
    FScrollLastButton.Visible := ButtonSettings.LastButton;

    if (FScrollPrevButton = nil) then
    begin
      FScrollPrevButton := TAdvGlowButton.Create(Self);
      FScrollPrevButton.Parent := Self;
      FScrollPrevButton.OnClick := OnScrollPrevButtonClick;
    end;
    R := GetTabScrollerLeftRect;
    FScrollPrevButton.Left := R.Left;
    FScrollPrevButton.Top := R.Top;
    FScrollPrevButton.Width := R.Right - R.Left;
    FScrollPrevButton.Height := R.Bottom - R.Top;
    if Assigned(FGlowButtonAppearance) and FItones then
      SetGlowButtonColorTones(FScrollPrevButton)
    else if Assigned(FCurrentOfficeTabSetStyler) then
      FScrollPrevButton.Appearance.Assign(FCurrentOfficeTabSetStyler.GlowButtonAppearance);
    if (TabPosition in [tpLeft, tpRight]) {and not RotateTabLeftRight} then
    begin
      if ButtonSettings.FPrevPictureChanged then
        FScrollPrevButton.Picture.Assign(ButtonSettings.ScrollButtonPrevPicture)
      else
        FScrollPrevButton.Picture.Assign(ButtonSettings.FScrollButtonUpPicture);
    end
    else
      FScrollPrevButton.Picture.Assign(ButtonSettings.ScrollButtonPrevPicture);
    FScrollPrevButton.Hint := ButtonSettings.ScrollButtonPrevHint;
    FScrollPrevButton.ShowHint := True;
    FScrollPrevButton.Enabled := FTabScroller.CanGoBack;
    FScrollPrevButton.RepeatClick := True;

    if (FScrollNextButton = nil) then
    begin
      FScrollNextButton := TAdvGlowButton.Create(Self);
      FScrollNextButton.Parent := Self;
      FScrollNextButton.OnClick := OnScrollNextButtonClick;
    end;
    R := GetTabScrollerRightRect;
    FScrollNextButton.Left := R.Left;
    FScrollNextButton.Top := R.Top;
    FScrollNextButton.Width := R.Right - R.Left;
    FScrollNextButton.Height := R.Bottom - R.Top;
    if Assigned(FGlowButtonAppearance) and FItones then
      SetGlowButtonColorTones(FScrollNextButton)
    else if Assigned(FCurrentOfficeTabSetStyler) then
      FScrollNextButton.Appearance.Assign(FCurrentOfficeTabSetStyler.GlowButtonAppearance);
    if (TabPosition in [tpLeft, tpRight]) {and not RotateTabLeftRight} then
    begin
      if ButtonSettings.FNextPictureChanged then
        FScrollNextButton.Picture.Assign(ButtonSettings.ScrollButtonNextPicture)
      else
        FScrollNextButton.Picture.Assign(ButtonSettings.FScrollButtonDownPicture);
    end
    else
      FScrollNextButton.Picture.Assign(ButtonSettings.ScrollButtonNextPicture);
    FScrollNextButton.Hint := ButtonSettings.ScrollButtonNextHint;
    FScrollNextButton.ShowHint := True;
    FScrollNextButton.Enabled := FTabScroller.CanGoForward;
    FScrollNextButton.RepeatClick := True;
  end
  else
  begin
    if (FScrollPrevButton <> nil) then
    begin
      FScrollPrevButton.Free;
      FScrollPrevButton := nil;
    end;

    if (FScrollNextButton <> nil) then
    begin
      FScrollNextButton.Free;
      FScrollNextButton := nil;
    end;

    if (FScrollFirstButton <> nil) then
    begin
      FScrollFirstButton.Free;
      FScrollFirstButton := nil;
    end;

    if (FScrollLastButton <> nil) then
    begin
      FScrollLastButton.Free;
      FScrollLastButton := nil;
    end;
  end;
end;

procedure TAdvCustomOfficeTabSet.InitOrder;
var
  i: integer;
begin
  for i := 0 to AdvOfficeTabs.Count - 1 do
  begin
    AdvOfficeTabs[i].FTabIndex := i + 1;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTabSet.WndProc(var Msg: TMessage);
var
  cb: TAdvGlowButton;
  cbc: TAdvTabSetButton;
begin
  inherited;

  if (Msg.Msg = WM_OTSDESTROYCLOSEBTN) then
  begin
    if (Msg.WParam <> 0) then
    begin
      if (Msg.LParam = 0) then
      begin
        cb := TAdvGlowButton(Pointer(Msg.WParam));
        cb.Free;
      end
      else
      begin
        cbc := TAdvTabSetButton(Pointer(Msg.WParam));
        cbc.Free;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTabSet.BeforeCloseTab(Tab: TOfficeTabCollectionItem; var CloseAction: TCloseAction);
begin

end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTabSet.BeginUpdate;
begin
  inc(FUpdateCount);
end;

//------------------------------------------------------------------------------

function TAdvCustomOfficeTabSet.CanCloseTab(TabIndex: Integer;
  var CloseAction: TCloseAction): Boolean;
begin
  Result := False;
  if (TabIndex >= 0) then
  begin
    Result := True;
    if Assigned(FOnTabClose) then
      FOnTabClose(Self, TabIndex, Result);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTabSet.OnCloseButtonClick(Sender: TObject);
var
  Allow: Boolean;
  ActTabIndex: Integer;
  ActItem: TOfficeTabCollectionItem;
  ca: TCloseAction;
  i: integer;
begin
  if CloseOnTab then
  begin
    if (Sender is TAdvGlowButton) then
      i := (Sender as TAdvGlowButton).Tag
    else if (Sender is TAdvTabSetButton) then
      i := (Sender as TAdvTabSetButton).Tag
    else
      i := ActiveTabIndex;
  end
  else
    i := ActiveTabIndex;

  if (ActiveTabIndex >= 0) then
  begin
    ca := caFree;
    Allow := CanCloseTab(i, ca);

    if FAdvOfficeTabs.Count > i then
    begin
      if Allow and (ca <> caNone) then
      begin
        ActTabIndex := i;
        SelectNextSequentialTab;
        ActItem := FAdvOfficeTabs.Items[ActTabIndex];
        InvalidateTab(-1);

        BeforeCloseTab(ActItem, ca);

        if FreeOnClose and (ca = caFree) then
        begin
          if Assigned(ActItem) and (ActItem <> ActItem) then
          begin
            if ActItem.FCloseButton <> nil then
            begin
              ActItem.FCloseButton.Free;
              ActItem.FCloseButton := nil;
            end;

            if ActItem.FCloseButtonChrome <> nil then
            begin
              ActItem.FCloseButtonChrome.Free;
              ActItem.FCloseButtonChrome := nil;
            end;

            ActItem.Free;
            FActiveTabIndex := -1;
            ActiveTabIndex := ActItem.Index;
          end
          else
          begin
            if (ActItem.FCloseButton = Sender) then
            begin
              RemoveControl(Sender as TAdvGlowButton);
              ActItem.FCloseButton := nil;
            end;

            if (ActItem.FCloseButtonChrome = Sender) then
            begin
              RemoveControl(Sender as TAdvTabSetButton);
              ActItem.FCloseButtonChrome := nil;
            end;

            ActItem.Free;
            ActiveTabIndex := ActTabIndex;
          end;
        end
        else if not (ca = caMinimize) then
        begin
          ActItem.Visible := False;
          if CloseOnTab then
          begin
            if Assigned(ActItem.FCloseButton) then
              ActItem.FCloseButton.Visible := false;

            if Assigned(ActItem.FCloseButtonChrome) then
              ActItem.FCloseButtonChrome.Visible := false;
          end;
          UpdateClosedListButton;
        end;

        UpdateTabScroller;
        UpdateMultiLineTabs;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTabSet.OnCloseListMenuClick(Sender: TObject);
begin
if (Sender is TMenuItem) and ((TMenuItem(Sender).Tag >= 0) and (TMenuItem(Sender).Tag < FClosedTabList.Count)) then
  begin
    OpenClosedTab(TOfficeTabCollectionItem(FClosedTabList.Objects[TMenuItem(Sender).Tag]));
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTabSet.OnTabListMenuClick(Sender: TObject);
begin
  if (Sender is TMenuItem) and ((TMenuItem(Sender).Tag >= 0) and (TMenuItem(Sender).Tag < FAdvOfficeTabs.Count)) then
  begin
    ActiveTabIndex := TMenuItem(Sender).Tag;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTabSet.OnTabListButtonClick(Sender: TObject);
var
  I: Integer;
  MenuItem: TMenuItem;
  P: TPoint;
  R: TRect;
begin
  if Assigned(TabListMenu) and (ButtonSettings.TabListButton) then
  begin
    TabListMenu.Items.Clear;
    for I := 0 to FAdvOfficeTabs.Count-1 do
    begin
      if FAdvOfficeTabs.Items[I].visible then
      begin
        MenuItem := TMenuItem.Create(self);
        MenuItem.Caption := FAdvOfficeTabs.Items[I].Caption;
        MenuItem.RadioItem := True;
        MenuItem.Checked := ActiveTabIndex = I;
        MenuItem.Tag := I;
        MenuItem.ImageIndex := FAdvOfficeTabs.Items[I].ImageIndex;
        MenuItem.OnClick := OnTabListMenuClick;
        TabListMenu.Items.Add(MenuItem);
      end;
    end;

    if (FTabListButton <> nil) then
    begin
      case (TabPosition) of
        tpTop:
        begin
          R := GetTabListRect;
          P.X := R.Left + self.Left;
          P.Y := R.Bottom + self.Top;

          p := Parent.ClientToScreen(p);
        end;
        tpBottom:
        begin
          R := GetTabListRect;
          P.X := R.Left + self.Left;
          P.Y := R.Bottom + self.Top;
          p := Parent.ClientToScreen(p);
          if (GetSystemMetrics(SM_CYMENU) * TabListMenu.Items.Count) + P.Y + 10 >
          {$IFDEF DELPHI6_LVL}
            Screen.MonitorFromPoint(P).Height then
          {$ELSE}
            Screen.Height then
          {$ENDIF}
          begin
            if (FTabListButton <> nil) then
              Dec(P.Y, (GetSystemMetrics(SM_CYMENU) * TabListMenu.Items.Count) + (FTabListButton.Height) + 4);
          end;
        end;
        tpLeft:
        begin
          R := GetTabListRect;
          P.X := R.Right + self.Left;
          P.Y := R.Top + self.Top;
          p := Parent.ClientToScreen(p);
        end;
        tpRight:
        begin
          R := GetTabListRect;
          P.X := R.Right + self.Left;
          P.Y := R.Top + self.Top;
          p := Parent.ClientToScreen(p);
        end;
      end;

      if Assigned(FOnTabListClick) then
        FOnTabListClick(Self, P.X, P.Y)
      else
        TabListMenu.Popup(P.X, P.Y);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTabSet.OnScrollPrevButtonClick(Sender: TObject);
begin
  if Assigned(FOnPrevClick) then
    FOnPrevClick(Sender);
  ScrollLeftBtnClick;
  InitializeAndUpdateButtons;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTabSet.OnScrollFirstButtonClick(Sender: TObject);
begin
  if Assigned(FOnFirstClick) then
    FOnFirstClick(Sender);
  ScrollFirstBtnClick;
  InitializeAndUpdateButtons;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTabSet.OnScrollLastButtonClick(Sender: TObject);
begin
  if Assigned(FOnLastClick) then
    FOnLastClick(Sender);
  ScrollLastBtnClick;
  InitializeAndUpdateButtons;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTabSet.OnScrollNextButtonClick(Sender: TObject);
begin
  if Assigned(FOnNextClick) then
    FOnNextClick(Sender);
  ScrollRightBtnClick;
  InitializeAndUpdateButtons;
end;

//------------------------------------------------------------------------------

function TAdvCustomOfficeTabSet.AnyButtonVisible: Boolean;
begin
  Result := (FTabScroller.Visible or (ButtonSettings.CloseButton and not CloseOnTab) or ButtonSettings.TabListButton);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTabSet.DragOver(Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var
  P: TPoint;
  i: integer;
  R: TRect;
begin
  inherited;

  if (Source = self) and (TabRearrange) then
  begin
    Accept := False;
    P.X:= X;
    P.Y:= Y;
    if PtInRect(GetTabsArea, p) then
    begin
      i := PTOnTab(X, Y);
      if (i >= 0) then
      begin
        Accept:= true;

        if Assigned(FArrow) then
        begin
          R := GetTabRect(i);
          case TabPosition of
            tpTop: P := Point(R.Left + (R.Right - R.Left) div 2, R.Top - FArrow.Height);
            tpBottom: P := Point(R.Left + (R.Right - R.Left) div 2, R.Top - FArrow.Height);
            tpLeft: P := Point(R.Left - FArrow.Width, R.Top + (R.Bottom - R.Top) div 2);
            tpRight: P := Point(R.Left - FArrow.Width, R.Top + (R.Bottom - R.Top) div 2);
          end;

          P := ClientToScreen(P);
          FArrow.Color := FTabRearrangeIndicatorColor;
          FArrow.Left := P.X;
          FArrow.Top := P.Y;
          FArrow.Visible := True;
        end;
      end;
    end;
  end;

  if not Accept and Assigned(FArrow) and FArrow.Visible then
    FArrow.Visible := False;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTabSet.DragCanceled;
begin
  inherited;
  if Assigned(FArrow) and FArrow.Visible then
    FArrow.Visible := False;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTabSet.DragDrop(Source: TObject; X, Y: Integer);
var
  i, OldIndex: integer;
  P: TPoint;
  aItem: TOfficeTabCollectionItem;
begin
  inherited;

  if (Source = Self) and (TabRearrange) then
  begin
    P.X := X;
    P.Y := Y;
    i := PTOnTab(X, Y);

    if (i >= 0) and (i <> ActiveTabIndex) and (ActiveTabIndex >= 0) and (AdvOfficeTabs.Count > 0) then
    begin
      aItem := AdvOfficeTabs.Items[ActiveTabIndex];
      OldIndex := aItem.Index;
      AdvOfficeTabs.Move(aItem.Index, i);
      ActiveTabIndex := aItem.Index;
      if Assigned(FOnTabMoved) then
        FOnTabMoved(self, OldIndex, i);
    end;
  end;

  if Assigned(FArrow) and FArrow.Visible then
    FArrow.Visible := False;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTabSet.CreateDropArrow;
var
  ad: TArrowDirection;
begin
  if not (csDesigning in ComponentState) and not Assigned(FArrow) then
  begin
    ad := arrDown;
    case TabPosition of
      tpTop, tpBottom: ad := arrDown;
      tpLeft: ad := arrLeft;
      tpRight: ad := arrLeft;
    end;

    FArrow := TArrowWindow.Init(Self, ad);
    FArrow.Color := FTabRearrangeIndicatorColor;
  end;
end;

//------------------------------------------------------------------------------

function TAdvCustomOfficeTabSet.CanShowCloseButton: Boolean;
begin
  Result := ButtonSettings.CloseButton;
  if (ActiveTabIndex >= 0) and (ActiveTabIndex < AdvOfficeTabs.Count) then
    Result := AdvOfficeTabs[ActiveTabIndex].ShowClose and Result;

  result := Result or (ShowCloseOnNonSelectedTabs and (AdvOfficeTabs.Count > 0));
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTabSet.SetCloseOnTabPosition(
  const Value: TCloseOnTabPos);
begin
  if (FCloseOnTabPosition <> Value) then
  begin
    FCloseOnTabPosition := Value;
    InitializeAndUpdateButtons;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

function TAdvCustomOfficeTabSet.GetLeftRoundingOffset: Integer;
begin
  Result := 0;
  if (TabSettings.Shape in [tsLeftRamp, tsLeftRightRamp]) and not UseOldDrawing and not ((TabPosition in [tpLeft, tpRight]) and not RotateTabLeftRight) then
    Result := TabSettings.Rounding * 2 + 5;
end;

//------------------------------------------------------------------------------

function TAdvCustomOfficeTabSet.GetRightRoundingOffset: Integer;
begin
  Result := 0;
  if (TabSettings.Shape in [tsRightRamp, tsLeftRightRamp]) and not UseOldDrawing and not ((TabPosition in [tpLeft, tpRight]) and not RotateTabLeftRight) then
    Result := TabSettings.Rounding * 2 + 5;
end;

function TAdvCustomOfficeTabSet.GetSettings: string;
var
  i: integer;
  ns: string;
begin
  Result := 'P:' + inttostr(ActiveTabIndex);

  for i := 0 to AdvOfficeTabs.Count - 1 do
  begin
    if AdvOfficeTabs[i].Visible then
      ns := 'V:' + IntToStr(AdvOfficeTabs[i].TabIndex)
    else
      ns := 'H:' + IntToStr(AdvOfficeTabs[i].TabIndex);

    Result := Result + ',' + ns;
  end;

  if Assigned(FFloatingTabs) then
  begin
    for i := 0 to FFloatingTabs.Count - 1 do
    begin
      ns := 'F:' + IntToStr(TOfficeTabCollectionItem(FFloatingTabs[i]).FTabIndex);
      Result := Result + ',' + ns;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TAdvCustomOfficeTabSet.IsActiveTabNeighbour(
  TabIndex: Integer): Integer;
var
  i: Integer;  
begin
  Result := 0;
  if (TabIndex = ActiveTabIndex) or (TabIndex < 0) or (TabIndex >= AdvOfficeTabs.Count) then
    Exit;

  if (TabIndex < ActiveTabIndex) then
  begin
    for i:= ActiveTabIndex - 1 downto TabIndex do
    begin
      if AdvOfficeTabs[i].Visible then
      begin
        if (i = TabIndex) then
          Result := -1;
        Break;
      end;
    end;
  end
  else // if (TabIndex > ActiveTabIndex) then
  begin
    for i:= ActiveTabIndex + 1 to TabIndex do
    begin
      if AdvOfficeTabs[i].Visible then
      begin
        if (i = TabIndex) then
          Result := 1;
        Break;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TAdvCustomOfficeTabSet.IsGlass: Boolean;
begin
  Result := TabSettings.Glass and FIsAeroVista and Transparent and not (csDesigning in ComponentState);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTabSet.UpdateTabAppearanceOfTabs;
var
  i: Integer;
begin
  if not (csDesigning in ComponentState) or not Assigned(FCurrentOfficeTabSetStyler) then
    Exit;

  for i := 0 to AdvOfficeTabs.Count -1 do
  begin
    if not AdvOfficeTabs[i].UseTabAppearance then
    begin
      if Assigned(FTabAppearance) and FItones then
        AdvOfficeTabs[i].TabAppearance.Assign(FTabAppearance)
      else
        AdvOfficeTabs[i].TabAppearance.Assign(FCurrentOfficeTabSetStyler.TabAppearance);
    end;
  end;
end;

//------------------------------------------------------------------------------

function TAdvCustomOfficeTabSet.UseOldDrawing: Boolean;
begin
  Result := (TabSettings.Shape = tsRectangle) and (TabSettings.Rounding = 1);
  if not Result and (TabPosition in [tpLeft, tpRight]) and not RotateTabLeftRight then
    Result := (TabSettings.Rounding = 1);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTabSet.WMEraseBkGnd(var Message: TWMEraseBkGnd);
begin
  Message.Result := 1;
end;

procedure TAdvCustomOfficeTabSet.WMLButtonDblClk(
  var Message: TWMLButtonDblClk);
var
  Tab: integer;
  p: TPoint;
begin
  inherited;

  p := Point(Message.XPos, Message.YPos);

  if PtInRect(GetTabsArea, p) then
  begin
    Tab := PTOnTab(p.X, p.Y);
    if (Tab >= 0) then
    begin
      if Assigned(FOnTabDblClick) then
        FOnTabDblClick(Self, Tab);
    end;
  end;
end;

//------------------------------------------------------------------------------

function TAdvCustomOfficeTabSet.GetCheckBoxRect(TabIndex: integer): TRect;
var
  rb, R: TRect;
  h, w: integer;
begin
  R := GetTabRect(TabIndex);
  w := 0;
  h := 0;
  if CloseOnTab and (CloseOnTabPosition = cpLeft) and (ShowCloseOnNonSelectedTabs or ((ActiveTabIndex = TabIndex) and not ShowCloseOnNonSelectedTabs)) then
  begin
    rb := GetCloseButtonRect(TabIndex);
    h := rb.Bottom - rb.Top;
    w := rb.Right - rb.Left;
  end;
  case TabPosition of
    tpTop, tpBottom: Result := Bounds(R.Left + 5 + w, r.Top + (r.Bottom - R.Top - 15) div 2, 15, 15);
    tpLeft: Result := Bounds(R.Left + (r.right - R.Left - 15) div 2, r.Bottom - h - 5 - 15, 15, 15);
    tpRight: Result := Bounds(R.Left + (r.right - R.Left - 15) div 2, r.Top + 5 + h, 15, 15);
  end;
end;

procedure TAdvCustomOfficeTabSet.GetCloseBtnImageAndTextRect(
  TabIndex: Integer; var CloseBtnR, TextR: TRect; var ImgP: TPoint);
var
  ActivePg: Boolean;
  R: TRect;
  i: Integer;
  ImgSize, TxtSize, CloseBtnSize: TSize;
begin
  if (TabIndex < 0) or (TabIndex >= AdvOfficeTabs.Count) or (TabSettings.Height <= 0) or (TabSettings.Width <= 0) then
    Exit;

  R := GetTabRect(TabIndex);
  if (R.Left <= -1) and (R.Right <= -1) then
    Exit;

  ActivePg := (ActiveTabIndex = TabIndex);

  ImgSize := GetTabImageSize(TabIndex);
  TxtSize := GetTextSize(TabIndex);
  if CloseOnTab then
  begin
    CloseBtnSize.cx := ButtonSettings.ButtonSize;
    CloseBtnSize.cy := ButtonSettings.ButtonSize;
  end
  else
  begin
    CloseBtnSize.cx := 0;
    CloseBtnSize.cy := 0;
  end;
  
  if (TabPosition in [tpTop, tpBottom]) or (not RotateTabLeftRight) then
  begin
    i := R.Right - R.Left;
    
    if (TabSettings.Shape in [tsRightRamp, tsLeftRightRamp]) then
      R.Right := R.Right - TabSettings.Rounding;

    case TabSettings.ImagePosition of
      ipTop, ipBottom:
        ImgSize.cx := 0;
      ipLeft, ipRight:
      begin
      end;
    end;
    
    case TabSettings.Alignment of
      taLeftJustify:
      begin
      end;
      taCenter:
      begin
        if (CloseBtnSize.cx > 0) and ActivePg then
          i := (i - CloseBtnSize.cx - 4);
        if (ImgSize.cx > 0) then
          i := i - ImgSize.cx - IMG_SPACE;
        i := (i - TxtSize.cx) div 2;
        R.Left := Max(R.Left + i, R.Left);
        
        if ActivePg and CloseOnTab and (CloseOnTabPosition = cpLeft) then
        begin
          CloseBtnR.Left := R.Left;
          CloseBtnR.Right := CloseBtnR.Left + ButtonSettings.ButtonSize;
          R.Left := CloseBtnR.Right + 4;
        end;

        if (TabSettings.ImagePosition = ipLeft) and (ImgSize.cx > 0) then
        begin
          ImgP.X := R.Left;
          R.Left := R.Left + ImgSize.cx + IMG_SPACE;
        end;

        if (TxtSize.cx > 0) then
        begin
          TextR.Left := R.Left;
          TextR.Right := TextR.Left + TxtSize.cx;
          R.Left := Min(R.Left + TxtSize.cx, R.Right);
        end;
        
        if (TabSettings.ImagePosition = ipRight) and (ImgSize.cx > 0) then
        begin
          ImgP.X := R.Left + IMG_SPACE;
          R.Left := R.Left + ImgSize.cx + IMG_SPACE;
        end;

        if ActivePg and CloseOnTab and (CloseOnTabPosition = cpRight) then
        begin
          CloseBtnR.Left := R.Left + 4;
          CloseBtnR.Right := CloseBtnR.Left + ButtonSettings.ButtonSize;
        end;
      end;
      taRightJustify:
      begin
      end;
    end;
  end
  else if (TabPosition = tpLeft) then
  begin
    i := R.Bottom - R.Top;
    if (TabSettings.Shape in [tsLeftRamp, tsLeftRightRamp]) then
      R.Bottom := R.Bottom - TabSettings.Rounding;
    if (TabSettings.Shape in [tsRightRamp, tsLeftRightRamp]) then
      R.Top := R.Top + TabSettings.Rounding;

    case TabSettings.ImagePosition of
      ipTop, ipBottom:
        ImgSize.cx := 0;
      ipLeft, ipRight:
      begin
      end;
    end;

    if (CloseBtnSize.cx > 0) and ActivePg then
      i := (i - CloseBtnSize.cx - 4);
    if (ImgSize.cx > 0) then
      i := i - ImgSize.cx - IMG_SPACE;
    i := (i - TxtSize.cx) div 2;
    
    case TabSettings.Alignment of
      taLeftJustify:
      begin
      end;
      taCenter:
      begin
        R.Bottom := Min(R.Bottom - i, R.Bottom);

        if ActivePg and CloseOnTab and (CloseOnTabPosition = cpLeft) then
        begin
          CloseBtnR.Top := R.Bottom - ButtonSettings.ButtonSize;
          CloseBtnR.Bottom := CloseBtnR.Top + ButtonSettings.ButtonSize;
          R.Bottom := CloseBtnR.Top - 4;
        end;

        if (TabSettings.ImagePosition = ipLeft) and (ImgSize.cx > 0) then
        begin
          ImgP.Y := R.Bottom - ImgSize.cy;
          R.Bottom := R.Bottom - ImgSize.cy - IMG_SPACE;
        end;

        if (TxtSize.cx > 0) then
        begin
          TextR.Bottom := R.Bottom;
          TextR.Top := TextR.Bottom - TxtSize.cx;
          R.Bottom := Max(R.Bottom - TxtSize.cx, R.Top);
        end;

        if (TabSettings.ImagePosition = ipRight) and (ImgSize.cx > 0) then
        begin
          ImgP.Y := R.Bottom - ImgSize.cy - IMG_SPACE;
          R.Bottom := ImgP.Y;
        end;

        if ActivePg and CloseOnTab and (CloseOnTabPosition = cpRight) then
        begin
          CloseBtnR.Bottom := R.Bottom - 4;
          CloseBtnR.Top := CloseBtnR.Bottom - ButtonSettings.ButtonSize;
        end;
      end;
      taRightJustify:
      begin
        R.Bottom := Min(R.Bottom - i, R.Bottom);

        if ActivePg and CloseOnTab and (CloseOnTabPosition = cpRight) then
        begin
          CloseBtnR.Top := R.Top;
          CloseBtnR.Bottom := CloseBtnR.Top + ButtonSettings.ButtonSize;
          R.Top := CloseBtnR.Bottom + 4;
        end;

        if (TabSettings.ImagePosition = ipRight) and (ImgSize.cx > 0) then
        begin
          ImgP.Y := R.Top;
          R.Top := ImgP.Y + ImgSize.cy + IMG_SPACE;
        end;

        if (TxtSize.cx > 0) then
        begin
          TextR.Top := R.Top;
          TextR.Bottom := TextR.Top + TxtSize.cx;
          R.Top := Min(R.Top + TxtSize.cx, R.Bottom);
        end;

        if (TabSettings.ImagePosition = ipLeft) and (ImgSize.cx > 0) then
        begin
          ImgP.Y := R.Top + IMG_SPACE;
          R.Top := ImgP.Y + ImgSize.cy;
        end;
        if ActivePg and CloseOnTab and (CloseOnTabPosition = cpLeft) then
        begin
          CloseBtnR.Top := R.Top + 4;
          CloseBtnR.Bottom := CloseBtnR.Top + ButtonSettings.ButtonSize;
          R.Top := CloseBtnR.Bottom;
        end;
      end;
    end;
  end
  else if (TabPosition = tpRight) then
  begin
    i := R.Bottom - R.Top;
    if (TabSettings.Shape in [tsLeftRamp, tsLeftRightRamp]) then
      R.Bottom := R.Bottom - TabSettings.Rounding;
    if (TabSettings.Shape in [tsRightRamp, tsLeftRightRamp]) then
      R.Top := R.Top + TabSettings.Rounding;

    case TabSettings.ImagePosition of
      ipTop, ipBottom:
        ImgSize.cx := 0;
      ipLeft, ipRight:
      begin
      end;
    end;

    if (CloseBtnSize.cx > 0) and ActivePg then
      i := (i - CloseBtnSize.cx - 4);
    if (ImgSize.cx > 0) then
      i := i - ImgSize.cx - IMG_SPACE;
    i := (i - TxtSize.cx) div 2;
    
    case TabSettings.Alignment of
      taLeftJustify:
      begin
      end;
      taCenter:
      begin
        R.Top := Min(R.Top + i, R.Bottom);

        if ActivePg and CloseOnTab and (CloseOnTabPosition = cpLeft) then
        begin
          CloseBtnR.Top := R.Top;
          CloseBtnR.Bottom := CloseBtnR.Top + ButtonSettings.ButtonSize;
          R.Top := CloseBtnR.Top + ButtonSettings.ButtonSize + 4;
        end;

        if (TabSettings.ImagePosition = ipLeft) and (ImgSize.cx > 0) then
        begin
          ImgP.Y := R.Top;
          R.Top := R.Top + ImgSize.cy + IMG_SPACE;
        end;

        if (TxtSize.cx > 0) then
        begin
          TextR.Top := R.Top;
          TextR.Bottom := TextR.Top + TxtSize.cx;
          R.Top := Min(R.Top + TxtSize.cx, R.Bottom);
        end;

        if (TabSettings.ImagePosition = ipRight) and (ImgSize.cx > 0) then
        begin
          ImgP.Y := R.Top + IMG_SPACE;
          R.Top := ImgP.Y + ImgSize.cy;
        end;

        if ActivePg and CloseOnTab and (CloseOnTabPosition = cpRight) then
        begin
          CloseBtnR.Top := R.Top + 4;
          CloseBtnR.Bottom := CloseBtnR.Top + ButtonSettings.ButtonSize;
        end;
      end;
      taRightJustify:
      begin
        //R.Top := Min(R.Top + i, R.Bottom);

        if ActivePg and CloseOnTab and (CloseOnTabPosition = cpRight) then
        begin
          CloseBtnR.Top := R.Bottom - ButtonSettings.ButtonSize;
          CloseBtnR.Bottom := CloseBtnR.Top + ButtonSettings.ButtonSize;
          R.Bottom := CloseBtnR.Top - 4;
        end;

        if (TabSettings.ImagePosition = ipRight) and (ImgSize.cx > 0) then
        begin
          ImgP.Y := R.Bottom - ImgSize.cy;
          R.Bottom := ImgP.Y - ImgSize.cy - IMG_SPACE;
        end;

        if (TxtSize.cx > 0) then
        begin
          TextR.Bottom := R.Bottom;
          TextR.Top := TextR.Bottom - TxtSize.cx;
          R.Bottom := Max(R.Bottom - TxtSize.cx, R.Top);
        end;

        if (TabSettings.ImagePosition = ipLeft) and (ImgSize.cx > 0) then
        begin
          ImgP.Y := R.Bottom - IMG_SPACE - ImgSize.cy;
          R.Bottom := ImgP.Y;
        end;
        if ActivePg and CloseOnTab and (CloseOnTabPosition = cpLeft) then
        begin
          CloseBtnR.Bottom := R.Bottom - 4;
          CloseBtnR.Top := CloseBtnR.Bottom - ButtonSettings.ButtonSize;
          R.Bottom := CloseBtnR.Top;
        end;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TAdvCustomOfficeTabSet.GetTabImageSize(TabIndex: Integer): TSize;
var
  Pic: TGDIPPicture;
begin
  Result.cx := 0;
  Result.cy := 0;
  if (TabIndex < 0) or (TabIndex >= FAdvOfficeTabs.Count) then
    Exit;

  if AdvOfficeTabs[TabIndex].Enabled or AdvOfficeTabs[TabIndex].DisabledPicture.Empty then
    Pic := AdvOfficeTabs[TabIndex].Picture
  else
    Pic := AdvOfficeTabs[TabIndex].DisabledPicture;

  if Assigned(Pic) and not Pic.Empty then
  begin
    Pic.GetImageSizes;
    Result.cx := Pic.Width;
    Result.cy := Pic.Height;
  end
  else
  if (Assigned(FImages) or Assigned(DisabledImages)) and (AdvOfficeTabs[TabIndex].ImageIndex >= 0) then
  begin
    if AdvOfficeTabs[TabIndex].Enabled then
    begin
      if Assigned(FImages) then
      begin
        Result.cx := FImages.Width;
        Result.cy := FImages.Height;
      end;
    end
    else
    begin
      if Assigned(FDisabledImages) then
      begin
        Result.cx := FDisabledImages.Width;
        Result.cy := FDisabledImages.Height;
      end
      else if Assigned(FImages) then
      begin
        Result.cx := FImages.Width;
        Result.cy := FImages.Height;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TAdvCustomOfficeTabSet.GetTextSize(TabIndex: Integer): TSize;
var
  R: TRect;
begin
  Result.cx := 0;
  Result.cy := 0;
  if (TabIndex < 0) or (TabIndex >= FAdvOfficeTabs.Count) then
    Exit;

  Canvas.Font.Assign(FCurrentOfficeTabSetStyler.TabAppearance.Font);
  Canvas.Font.Size := Round(GetDPIScale(FFormScaled, Canvas.Handle)* Canvas.Font.Size);

  if (AdvOfficeTabs[TabIndex].Caption <> '') then
  begin
    R := Rect(0,0, 1000, 100);
    DrawText(Canvas.Handle,PChar(AdvOfficeTabs[TabIndex].Caption),Length(AdvOfficeTabs[TabIndex].Caption), R, DT_CALCRECT or DT_LEFT or DT_SINGlELINE);
    Result.cx := R.Right;
    Result.cy := R.Bottom;
    case AntiAlias of
      aaNone, aaClearType:
        Result.cx := Result.cx + length(AdvOfficeTabs[TabIndex].Caption) div 3;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TAdvCustomOfficeTabSet.PtOnInsertButton(X, Y: Integer): Boolean;
begin
  Result := false;
  if ButtonSettings.ShowInsertButton then
  begin
    Result := PtInRect(GetInsertButtonRect, Point(X, Y));
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTabSet.DoInsertButtonClick;
var
  ATab: TOfficeTabCollectionItem;
begin
  ATab := FAdvOfficeTabs.Add;
  ATab.Caption := 'AdvOfficeTabSet' + Inttostr(FAdvOfficeTabs.Count);
  ActiveTabIndex := aTab.Index;
  if Assigned(FOnTabInsert) then
    FOnTabInsert(Self, ATab);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTabSet.InvalidateInsertButton;
var
  R: TRect;
begin
  R := GetInsertButtonRect;
  InvalidateRect(Handle, @R, True);
end;

//------------------------------------------------------------------------------

function TAdvCustomOfficeTabSet.GetInsertButtonRect: TRect;
var
  gap: Integer;
  R: TRect;
begin
  Result := Rect(-1, -1, -1, -1);
  if ButtonSettings.ShowInsertButton then
  begin
    gap := 0;
    R := GetTabRect(GetLastDisplayTab);
    if (R.Left < 0) or (R.Right < 0) then
    begin  // No tab is displayed
      R := GetTabsRect;
      case TabPosition of
        tpTop:
        begin
          R.Right := R.Left;
        end;
        tpBottom:
        begin
          R.Right := R.Left;
        end;
        tpLeft:
        begin
          R.Bottom := R.Top;
        end;
        tpRight:
        begin
          R.Bottom := R.Top;
        end;
      end;
    end;
    case TabPosition of
      tpTop:
      begin
        Result.Left := R.Right + gap;
        Result.Right := Result.Left + ButtonSettings.ButtonSize + 6;
        Result.Top := R.Top + 5;
        Result.Bottom := R.Bottom - 8;
      end;
      tpBottom:
      begin
        Result.Left := R.Right + gap;
        Result.Right := Result.Left + ButtonSettings.ButtonSize + 6;
        Result.Top := R.Top + 8;
        Result.Bottom := R.Bottom - 5;
      end;
      tpLeft:
      begin
        if RotateTabLeftRight then
        begin
          Result.Left := R.Left + 5;
          Result.Right := R.Right - 8;
          Result.Top := R.Bottom + gap;
          Result.Bottom := Result.Top + ButtonSettings.ButtonSize + 6;
        end
        else
        begin
          Result.Right := R.Right - 8;
          Result.Left := Result.Right - ButtonSettings.ButtonSize - 1;
          Result.Top := R.Bottom + gap;
          Result.Bottom := Result.Top + ButtonSettings.ButtonSize + 6;
        end;
      end;
      tpRight:
      begin
        if RotateTabLeftRight then
        begin
          Result.Left := R.Left + 8;
          Result.Right := R.Right - 5;
          Result.Top := R.Bottom + gap;
          Result.Bottom := Result.Top + ButtonSettings.ButtonSize + 6;
        end
        else
        begin
          Result.Left := R.Left + 8;
          Result.Right := Result.Left + ButtonSettings.ButtonSize + 1;
          Result.Top := R.Bottom + gap;
          Result.Bottom := Result.Top + ButtonSettings.ButtonSize + 6;
        end;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TAdvCustomOfficeTabSet.GetLastDisplayTab: Integer;
var
  i: Integer;
  R: TRect;
begin
  Result := -1;
  for i:= FAdvOfficeTabs.Count -1 downto 0 do
  begin
    if FAdvOfficeTabs[i].Visible then
    begin
      R := GetTabRect(i);
      if (R.Left > -1) and (R.Right > -1) then
      begin
        Result := i;
        Break;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTabSet.GlassChanged;
begin
  if TabSettings.Glass then
  begin
    if (csDesigning in ComponentState) and not (csLoading in ComponentState) then
    begin
      Transparent := True;
      {$IFDEF DELPHI2007_LVL}
      if (Align in [alTop, alBottom, alClient]) and (Parent is TCustomForm) then
      begin
        TCustomForm(Parent).GlassFrame.Top := 27;
        TCustomForm(Parent).GlassFrame.Enabled := True;
      end;
      {$ENDIF}
    end;
  end
  else
  begin
    if (csDesigning in ComponentState) and not (csLoading in ComponentState) then
    begin
      Transparent := False;
      {$IFDEF DELPHI2007_LVL}
      if (Align in [alTop, alBottom, alClient]) and (Parent is TCustomForm) then
      begin
        if (TCustomForm(Parent).GlassFrame.Top = 27) then
        begin
          TCustomForm(Parent).GlassFrame.Top := 0;
          TCustomForm(Parent).GlassFrame.Enabled := False;
        end;
      end;
      {$ENDIF}
    end;
  end;
end;

//------------------------------------------------------------------------------

function TAdvCustomOfficeTabSet.CanGlow: Boolean;
begin
  Result := Glow and not FItones;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTabSet.SetGlowButtonColorTones(
  AdvGlowButton: TAdvGlowButton);
begin
  if not Assigned(AdvGlowButton) or not Assigned(FGlowButtonAppearance) then
    Exit;

  AdvGlowButton.Appearance.Assign(FGlowButtonAppearance);
  AdvGlowButton.Rounded := false;
  //AdvGlowButton.Font.Color := ATones.Background.TextColor;
  AdvGlowButton.Font.Name := GetMetroFont;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeTabSet.SetColorTones(ATones: TColorTones);
begin
  FItones := True;
  TabRoundEdges := False;
  Show3D := False;
  Shadow := False;

  if not Assigned(FTabAppearance) then
    FTabAppearance := TTabAppearance.Create;
  FTabAppearance.ClearValues;
  FTabAppearance.BackGround.Color := ATones.Selected.BrushColor;
  FTabAppearance.BackGround.ColorTo := clNone;
  FTabAppearance.Color := FTabAppearance.BackGround.Color;
  FTabAppearance.TextColor := ATones.Background.TextColor;
  FTabAppearance.ColorSelected := ATones.Background.BrushColor;
  FTabAppearance.BorderColorSelected := ATones.Background.BrushColor;
  FTabAppearance.TextColorSelected := ATones.Background.TextColor;
  FTabAppearance.ColorHot := ATones.Hover.BrushColor;
  FTabAppearance.BorderColorHot := ATones.Hover.BorderColor;
  FTabAppearance.TextColorHot := ATones.Hover.TextColor;
  FTabAppearance.Font.Name := GetMetroFont;
  FTabAppearance.ColorDisabled := ATones.Disabled.BrushColor;
  FTabAppearance.BorderColorDisabled := ATones.Disabled.BorderColor;
  FTabAppearance.TextColorDisabled := ATones.Disabled.TextColor;
  FTabAppearance.BorderColorSelectedHot := FTabAppearance.BorderColorSelected;

  if not Assigned(FGlowButtonAppearance) then // independent to AdvGlowbutton settings
    FGlowButtonAppearance := TGlowButtonAppearance.Create;
  ClearAppearance(FGlowButtonAppearance);
  FGlowButtonAppearance.Color := ATones.Background.BrushColor;
  FGlowButtonAppearance.ColorTo := clNone;
  FGlowButtonAppearance.BorderColor := clNone; //ATones.Background.BorderColor;
  FGlowButtonAppearance.ColorMirror := ATones.Background.BrushColor;
  FGlowButtonAppearance.ColorMirrorTo := clNone;
  FGlowButtonAppearance.ColorHot := ATones.Hover.BrushColor;
  FGlowButtonAppearance.ColorHotTo := clNone;
  FGlowButtonAppearance.ColorMirrorHot := ATones.Hover.BrushColor;
  FGlowButtonAppearance.ColorMirrorHotTo := clNone;
  FGlowButtonAppearance.BorderColorHot := ATones.Hover.BorderColor;
  FGlowButtonAppearance.TextColorHot := ATones.Hover.TextColor;
  FGlowButtonAppearance.TextColorDown := ATones.Selected.TextColor;
  FGlowButtonAppearance.ColorDown := ATones.Selected.BrushColor;
  FGlowButtonAppearance.ColorDownTo := clNone;
  FGlowButtonAppearance.ColorMirrorDown := ATones.Selected.BrushColor;
  FGlowButtonAppearance.ColorMirrorDownTo := clNone;
  FGlowButtonAppearance.BorderColorDown := ATones.Selected.BorderColor;
  FGlowButtonAppearance.ColorDisabled := clSilver;
  FGlowButtonAppearance.ColorDisabledTo := clSilver;
  FGlowButtonAppearance.ColorMirrorDisabled := clSilver;
  FGlowButtonAppearance.ColorMirrorDisabledTo := clSilver;
  //FGlowButtonAppearance.TextColor := ATones.Background.TextColor;

  InitializeAndUpdateButtons;
  Invalidate;
end;

//------------------------------------------------------------------------------

{ TTabSetButtonSettings }

constructor TTabSetButtonSettings.Create;
begin
  inherited;
  FTabListButton := False;
  FCloseButton := False;
  FClosedListButton := False;

  FCloseButtonLook := cblOffice;
  FButtonSize := TabBUTTON_SIZE;
  FShowInsertButton := False;

  FScrollButtonNextPicture := TGDIPPicture.Create;
  FScrollButtonNextPicture.LoadFromResourceName(hinstance,'TMSTSNEXT');
  FScrollButtonNextPicture.OnChange := OnPictureChanged;
  FNextPictureChanged := False;

  FScrollButtonPrevPicture := TGDIPPicture.Create;
  FScrollButtonPrevPicture.LoadFromResourceName(hinstance,'TMSTSPREV');
  FScrollButtonPrevPicture.OnChange := OnPictureChanged;
  FPrevPictureChanged := False;

  FScrollButtonFirstPicture := TGDIPPicture.Create;
  FScrollButtonFirstPicture.LoadFromResourceName(hinstance,'TMSTSFIRST');
  FScrollButtonFirstPicture.OnChange := OnPictureChanged;
  FFirstPictureChanged := False;

  FScrollButtonLastPicture := TGDIPPicture.Create;
  FScrollButtonLastPicture.LoadFromResourceName(hinstance,'TMSTSLAST');
  FScrollButtonLastPicture.OnChange := OnPictureChanged;
  FLastPictureChanged := False;

  FTabListButtonPicture := TGDIPPicture.Create;
  FTabListButtonPicture.LoadFromResourceName(hinstance,'TMSTSLIST');
  FTabListButtonPicture.OnChange := OnPictureChanged;

  FCloseButtonPicture := TGDIPPicture.Create;
  FCloseButtonPicture.LoadFromResourceName(hinstance,'TMSTSCLOSE');
  FCloseButtonPicture.OnChange := OnPictureChanged;

  FClosedListButtonPicture := TGDIPPicture.Create;
  FClosedListButtonPicture.LoadFromResourceName(hinstance,'TMSTSLIST');
  FClosedListButtonPicture.OnChange := OnPictureChanged;

  FScrollButtonPrevHint := 'Previous';
  FScrollButtonNextHint := 'Next';
  FScrollButtonFirstHint := 'First';
  FScrollButtonLastHint := 'Last';
  FCloseButtonHint := 'Close';
  FInsertButtonHint := 'Insert new item';
  FTabListButtonHint := 'TabList';
  FClosedListButtonHint := 'Closed Pages';
  FScrollButtonsAlways := False;
  FFirstButton := false;
  FLastButton := false;

  FScrollButtonDownPicture := TGDIPPicture.Create;
  FScrollButtonDownPicture.LoadFromResourceName(hinstance,'TMSTSDOWN');

  FScrollButtonUpPicture := TGDIPPicture.Create;
  FScrollButtonUpPicture.LoadFromResourceName(hinstance,'TMSTSUP');

  FScrollButtonDownLastPicture := TGDIPPicture.Create;
  FScrollButtonDownLastPicture.LoadFromResourceName(hinstance,'TMSTSDOWNLAST');

  FScrollButtonUpFirstPicture := TGDIPPicture.Create;
  FScrollButtonUpFirstPicture.LoadFromResourceName(hinstance,'TMSTSUPFIRST');
end;

//------------------------------------------------------------------------------

destructor TTabSetButtonSettings.Destroy;
begin
  FScrollButtonNextPicture.Free;
  FScrollButtonPrevPicture.Free;
  FTabListButtonPicture.Free;
  FCloseButtonPicture.Free;
  FClosedListButtonPicture.Free;
  FScrollButtonDownPicture.Free;
  FScrollButtonUpPicture.Free;
  FScrollButtonFirstPicture.Free;
  FScrollButtonLastPicture.Free;
  FScrollButtonDownLastPicture.Free;
  FScrollButtonUpFirstPicture.Free;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TTabSetButtonSettings.Assign(Source: TPersistent);
begin
  if (Source is TTabSetButtonSettings) then
  begin
    FTabListButton := (Source as TTabSetButtonSettings).FTabListButton;
    FCloseButton := (Source as TTabSetButtonSettings).FCloseButton;
    FClosedListButton := (Source as TTabSetButtonSettings).ClosedListButton;
    FScrollButtonNextPicture.Assign((Source as TTabSetButtonSettings).FScrollButtonNextPicture);
    FScrollButtonPrevPicture.Assign((Source as TTabSetButtonSettings).FScrollButtonPrevPicture);
    FScrollButtonFirstPicture.Assign((Source as TTabSetButtonSettings).FScrollButtonFirstPicture);
    FScrollButtonLastPicture.Assign((Source as TTabSetButtonSettings).FScrollButtonLastPicture);
    FTabListButtonPicture.Assign((Source as TTabSetButtonSettings).FTabListButtonPicture);
    FCloseButtonPicture.Assign((Source as TTabSetButtonSettings).FCloseButtonPicture);
    FClosedListButtonPicture.Assign((Source as TTabSetButtonSettings).FClosedListButtonPicture);
    FScrollButtonsAlways := (Source as TTabSetButtonSettings).ScrollButtonsAlways;
    FFirstButton := (source as TTabSetButtonSettings).FirstButton;
    FLastButton := (source as TTabSetButtonSettings).LastButton;
    FScrollButtonPrevHint := (Source as TTabSetButtonSettings).ScrollButtonPrevHint;
    FScrollButtonNextHint := (Source as TTabSetButtonSettings).ScrollButtonNextHint;
    FScrollButtonFirstHint := (Source as TTabSetButtonSettings).ScrollButtonFirstHint;
    FScrollButtonLastHint := (Source as TTabSetButtonSettings).ScrollButtonLastHint;
    CloseButtonHint := (Source as TTabSetButtonSettings).CloseButtonHint;
    InsertButtonHint := (Source as TTabSetButtonSettings).InsertButtonHint;
    TabListButtonHint := (Source as TTabSetButtonSettings).TabListButtonHint;
    ClosedListButtonHint := (Source as TTabSetButtonSettings).ClosedListButtonHint;
    CloseButtonLook := (Source as TTabSetButtonSettings).CloseButtonLook;
    ShowInsertButton := (Source as TTabSetButtonSettings).ShowInsertButton;
    ButtonSize := (Source as TTabSetButtonSettings).ButtonSize;
  end
  else
    inherited Assign(Source);
end;

//------------------------------------------------------------------------------

procedure TTabSetButtonSettings.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

//------------------------------------------------------------------------------

procedure TTabSetButtonSettings.OnPictureChanged(Sender: TObject);
begin
  FNextPictureChanged := FNextPictureChanged or (Sender = FScrollButtonNextPicture);
  FPrevPictureChanged := FPrevPictureChanged or (Sender = FScrollButtonPrevPicture);
  FLastPictureChanged := FLastPictureChanged or (Sender = FScrollButtonLastPicture);
  FFirstPictureChanged := FFirstPictureChanged or (Sender = FScrollButtonFirstPicture);
  Changed;
end;

//------------------------------------------------------------------------------

procedure TTabSetButtonSettings.SetButtonSize(const Value: Integer);
begin
  if (FButtonSize <> Value) then
  begin
    FButtonSize := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TTabSetButtonSettings.SetCloseButton(const Value: Boolean);
begin
  if (FCloseButton <> Value) then
  begin
    FCloseButton := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TTabSetButtonSettings.SetCloseButtonLook(
  const Value: TCloseButtonLook);
begin
  if (FCloseButtonLook <> Value) then
  begin
    FCloseButtonLook := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TTabSetButtonSettings.SetShowInsertButton(const Value: Boolean);
begin
  if (FShowInsertButton <> Value) then
  begin
    FShowInsertButton := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TTabSetButtonSettings.SetCloseButtonPicture(
  const Value: TGDIPPicture);
begin
  FCloseButtonPicture.Assign(Value);
  Changed;
end;

//------------------------------------------------------------------------------

procedure TTabSetButtonSettings.SetClosedListButton(const Value: Boolean);
begin
  if (FClosedListButton <> Value) then
  begin
    FClosedListButton := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TTabSetButtonSettings.SetClosedListButtonPicture(
  const Value: TGDIPPicture);
begin
  FClosedListButtonPicture.Assign(Value);
  Changed;
end;

//------------------------------------------------------------------------------

procedure TTabSetButtonSettings.SetFirstButton(const Value: Boolean);
begin
  if FFirstButton <> value then
  begin
    FFirstButton := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TTabSetButtonSettings.SetLastButton(const Value: Boolean);
begin
  if FLastButton <> value then
  begin
    FLastButton := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TTabSetButtonSettings.SetTabListButton(const Value: boolean);
begin
  if (FTabListButton <> Value) then
  begin
    FTabListButton := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TTabSetButtonSettings.SetTabListButtonPicture(
  const Value: TGDIPPicture);
begin
  FTabListButtonPicture.Assign(Value);
  Changed;
end;

//------------------------------------------------------------------------------

procedure TTabSetButtonSettings.SetScrollButtonFirstPicture(
  const Value: TGDIPPicture);
begin
  FScrollButtonFirstPicture.Assign(Value);
  Changed;
end;

//------------------------------------------------------------------------------

procedure TTabSetButtonSettings.SetScrollButtonLastPicture(
  const Value: TGDIPPicture);
begin
  FScrollButtonLastPicture.Assign(Value);
  Changed;
end;

//------------------------------------------------------------------------------

procedure TTabSetButtonSettings.SetScrollButtonNextPicture(
  const Value: TGDIPPicture);
begin
  FScrollButtonNextPicture.Assign(Value);
  Changed;
end;

//------------------------------------------------------------------------------

procedure TTabSetButtonSettings.SetScrollButtonPrevPicture(
  const Value: TGDIPPicture);
begin
  FScrollButtonPrevPicture.Assign(Value);
  Changed;
end;

//------------------------------------------------------------------------------

procedure TTabSetButtonSettings.SetScrollButtonsAlways(
  const Value: Boolean);
begin
  if (FScrollButtonsAlways <> Value) then
  begin
    FScrollButtonsAlways := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

{ TAdvTabSetButton }

constructor TAdvTabSetButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTransparent := True;
  FDrawClose := True;

  SetBounds(0, 0, 23, 22);
  ControlStyle := [csCaptureMouse, csDoubleClicks];
  ParentFont := True;

  // make sure to use a Truetype font
  Font.Name := 'Tahoma';
  ShowHint := False;
end;

//------------------------------------------------------------------------------

destructor TAdvTabSetButton.Destroy;
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvTabSetButton.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if (csDesigning in ComponentState) or FMouseEnter then
    Exit;

  FMouseEnter := True;
  if Enabled then
    Invalidate;

  if Assigned(FOnMouseEnter) then
     FOnMouseEnter(Self);
end;

//------------------------------------------------------------------------------

procedure TAdvTabSetButton.CMMouseLeave(var Message: TMessage);
begin
  inherited;

  if (csDesigning in ComponentState) or not FMouseEnter then
    Exit;

  FMouseEnter := false;

  if Enabled then
    Invalidate;

  if Assigned(FOnMouseLeave) then
     FOnMouseLeave(Self);
end;

//------------------------------------------------------------------------------

procedure TAdvTabSetButton.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  if (Button <> mbLeft) or not Enabled or (csDesigning in ComponentState) then
    Exit;

  FMouseDown := true;
  if Enabled then
    Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvTabSetButton.MouseMove(Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;

  if (csDesigning in ComponentState) then
    Exit;

  if PtInRect(ClientRect, Point(X, Y)) then
  begin
    if not FMouseEnter then
    begin
      FMouseEnter := True;
      Invalidate;
    end
  end
  else
  begin
    if FMouseEnter then
    begin
      FMouseEnter := False;
      Invalidate;
    end
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvTabSetButton.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  if (csDesigning in ComponentState) then
    Exit;

  Invalidate;

  FMouseDown := false;
  if Enabled and FMouseEnter then
    Click;
end;

//------------------------------------------------------------------------------

procedure TAdvTabSetButton.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;

  if not (csDestroying in ComponentState) and (AOperation = opRemove) then
  begin
  end;

end;

//------------------------------------------------------------------------------

procedure TAdvTabSetButton.Paint;
var
  g: TGPGraphics;
  R: TRect;
  br: TGPSolidBrush;
  p: TGPPen;
  j, w, h: Integer;
begin
  R := ClientRect;

  g := TGPGraphics.Create(Canvas.Handle);
  g.SetSmoothingMode(SmoothingModeAntiAlias);

  if DrawClose then
  begin
    j := Width div 3;
    w := R.Right - R.Left;
    h := R.Bottom - R.Top;

    if FMouseDown and FMouseEnter then
    begin
      //--- DownPicture
      br := TGPSolidBrush.Create(ColorToARGB(RGB(67, 30, 32)));
      g.FillEllipse(br, 2, 1, W-3, H-3);
      br.Free;

      p := TGPPen.Create(MakeColor(220, 235, 235, 235), 1.6);
      g.DrawLine(p, j, j-1, W - j + 1, H - j);
      g.DrawLine(p, W - j + 1, j - 1, j, H - j);
      p.Free;
    end
    else if FMouseEnter then
    begin
      //--- HotPicture
      br := TGPSolidBrush.Create(ColorToARGB(RGB(192, 53, 53)));
      g.FillEllipse(br, 2, 1, W-3, H-3);
      br.Free;

      p := TGPPen.Create(MakeColor(220, 235, 235, 235), 1.6);
      g.DrawLine(p, j, j-1, W - j + 1, H - j);
      g.DrawLine(p, W - j + 1, j - 1, j, H - j);
      p.Free;
    end
    else    //--- Normal
    begin
      p := TGPPen.Create(MakeColor(220, 165, 165, 165), 1.6);
      g.DrawLine(p, j, j - 1, W - j + 1, H - j);
      g.DrawLine(p, W - j + 1, j - 1, j, h - j);
      p.Free;
    end;
  end
  else
  begin

  end;
  g.Free;
end;

//------------------------------------------------------------------------------

procedure TAdvTabSetButton.SetBounds(ALeft, ATop, AWidth,
  AHeight: Integer);
begin
  inherited;

end;

//------------------------------------------------------------------------------

procedure TAdvTabSetButton.Click;
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvTabSetButton.SetTransparent(const Value: Boolean);
begin
  FTransparent := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvTabSetButton.WMEraseBkGnd(var Message: TWMEraseBkGnd);
begin
  Message.Result := 1;
  inherited;
end;

//------------------------------------------------------------------------------

{ TDummyHintControl }

constructor TDummyHintControl.Create(AOwner: TComponent);
begin
  inherited;
  FOfficeHint := TAdvHintInfo.Create;
end;

//------------------------------------------------------------------------------

destructor TDummyHintControl.Destroy;
begin
  FOfficeHint.Free;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TDummyHintControl.SetOfficeHint(const Value: TAdvHintInfo);
begin
  FOfficeHint.Assign(Value);
end;

//------------------------------------------------------------------------------

{ TAdvOfficeMDITabSet }

constructor TAdvOfficeMDITabSet.Create(AOwner: TComponent);
var
  i: Integer;
begin
  if not(AOwner is TForm) then
  begin
    raise exception.create('AdvOfficeMDITabSet can only be placed on a Form.');
  end
  else if (TForm(AOwner).FormStyle <> fsMDIForm) then
  begin
    raise exception.create('AdvOfficeMDITabSet can only be placed on a MDIForm.');
  end;

  for i:= 0 to AOwner.ComponentCount-1 do
  begin
    if (AOwner.Components[i] is TAdvOfficeMDITabSet) and (AOwner.Components[i] <> Self) then
    begin
      raise exception.create('Only one instance of AdvOfficeMDITabSet can be placed on a MDIForm.');
    end;
  end;

  inherited;

  FFreeOnClose := True;
end;

//------------------------------------------------------------------------------

destructor TAdvOfficeMDITabSet.Destroy;
var
  i: Integer;
begin
  for i:= 0 to FAdvOfficeTabs.count-1 do
begin
    if Assigned(FAdvOfficeTabs[i].FChildForm) then
    begin
      FAdvOfficeTabs[i].FChildForm.OnDestroy := FAdvOfficeTabs[i].FOnDestroyForm;
    end;
  end;

  inherited;
end;

//------------------------------------------------------------------------------

function TAdvOfficeMDITabSet.AddTab(
  ChildForm: TForm): TOfficeTabCollectionItem;
var
  i: Integer;
begin
  Result := nil;

  if not Assigned(ChildForm) then
    Exit;

  for i:= 0 to FAdvOfficeTabs.Count-1 do
  begin
    if (FAdvOfficeTabs.Items[i].FChildForm = ChildForm) then
    begin
      Exit;
    end;
  end;

  Result := FAdvOfficeTabs.Add;
  Result.Caption := ChildForm.Caption;
  Result.FChildForm := ChildForm;
  Result.FOnActivateForm := ChildForm.OnActivate;
  Result.FOnDestroyForm := ChildForm.OnDestroy;

  ChildForm.OnActivate := OnChildFormActivate;
  ChildForm.OnDestroy := OnChildFormDestroy;


  ActiveTabIndex := Result.Index;
  if not (csLoading in ComponentState) then
    InitializeAndUpdateButtons;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficeMDITabSet.RemoveTab(ChildForm: TForm);
var
  i, j :integer;
begin
  if not (csDestroying in ComponentState) then
  begin
    j := -1;
    for i := 0 to FAdvOfficeTabs.Count - 1 do
    begin
      if (FAdvOfficeTabs[i].FChildForm = ChildForm) then
      begin
        j := i;
        break;
      end;
    end;

    if (j >= 0) and Assigned(FAdvOfficeTabs[j]) (*and Assigned(FAdvOfficeTabs[j].FOnDestroyForm)*)
       and Assigned(FAdvOfficeTabs[j].FChildForm) then
    begin
      ChildForm.OnActivate:=FAdvOfficeTabs[j].FOnActivateForm;
      ChildForm.OnDestroy:=FAdvOfficeTabs[j].FOnDestroyForm;
    end;

    if (j >= 0) and not FInternalDelete then
      FAdvOfficeTabs.Delete(j);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficeMDITabSet.ChangeActiveTab(TabIndex: Integer);
var
  OldActiveTabIndex: Integer;
begin
  OldActiveTabIndex := ActiveTabIndex;

  inherited;

  if (TabIndex >= 0) and (TabIndex < FAdvOfficeTabs.Count) and (TabIndex <> OldActiveTabIndex) then
  begin
    Change;
  end;
end;

procedure TAdvOfficeMDITabSet.CloseAllTabs;
var
  i: integer;
begin
  for i := AdvOfficeTabCount - 1 downto 0 do
  begin
    GetChildForm(AdvOfficeTabs[i]).Close;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficeMDITabSet.Change;
var
  AChildForm, AForm: TForm;
  i: integer;
begin
  if (ActiveTabIndex < 0) or (ActiveTabIndex >= FAdvOfficeTabs.Count) then
    Exit;

  AChildForm := FAdvOfficeTabs[ActiveTabIndex].FChildForm;
  if Assigned(AChildForm) then
  begin
    with TForm(GetParentForm(Self)) do
      for i := 0 to MDIChildCount-1 do
      begin
        AForm := MDIChildren[i];
        if (AForm <> AChildForm) then
          if (GetNextWindow(AForm.Handle, GW_HWNDNEXT) = AChildForm.Handle) then
          begin
            SendMessage(ClientHandle, WM_MDINEXT, AForm.Handle,0);
            Break;
          end;
      end;

    with AChildForm do
    begin
      if (Windowstate = WSMinimized) then
        WindowState := WSNormal;
      Visible := True;
      SetFocus;
      BringToFront;
    end;
  end;
end;

{begin
  if (ActiveTabIndex < 0) or (ActiveTabIndex >= FAdvOfficeTabs.Count) then
    Exit;

  if Assigned(FAdvOfficeTabs[ActiveTabIndex].FChildForm) then
  begin
    SendMessage(FAdvOfficeTabs[ActiveTabIndex].FChildForm.Handle, WM_NCActivate, WA_ACTIVE, 0);

    if FAdvOfficeTabs[ActiveTabIndex].FChildForm.Windowstate = WSMinimized then
      FAdvOfficeTabs[ActiveTabIndex].FChildForm.WindowState := WSNormal;

    FAdvOfficeTabs[ActiveTabIndex].FChildForm.BringToFront;
    FAdvOfficeTabs[ActiveTabIndex].FChildForm.SetFocus;
    FAdvOfficeTabs[ActiveTabIndex].FChildForm.Visible := True;
  end;
end;
}
//------------------------------------------------------------------------------

function TAdvOfficeMDITabSet.GetAdvOfficeTabCount: integer;
begin
  Result := FAdvOfficeTabs.Count;
end;

//------------------------------------------------------------------------------

function TAdvOfficeMDITabSet.GetAdvOfficeTabs(
  index: integer): TOfficeTabCollectionItem;
begin
  Result := nil;
  if (Index >= 0) and (Index < FAdvOfficeTabs.Count) then
  begin
    Result := FAdvOfficeTabs[Index];
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficeMDITabSet.OnChildFormActivate(Sender: TObject);
var
  i :integer;
begin
  if (Sender is TForm) and (TForm(Sender).FormStyle = fsMDIChild) then
  begin
    for i:= 0 to FAdvOfficeTabs.count-1 do
    begin
      if (FAdvOfficeTabs[i].FChildForm = Sender) then
      begin
        if not FAdvOfficeTabs[i].Visible then
        begin
          FAdvOfficeTabs[i].Visible := True;
        end;
        ActiveTabIndex := i;
        break;
      end;
    end;

    if (ActiveTabIndex >= 0) and Assigned(FAdvOfficeTabs[FActiveTabIndex]) and Assigned(FAdvOfficeTabs[FActiveTabIndex].FOnActivateForm)
       and Assigned(FAdvOfficeTabs[FActiveTabIndex].FChildForm) then
    begin
      FAdvOfficeTabs[FActiveTabIndex].FOnActivateForm(FAdvOfficeTabs[FActiveTabIndex].FChildForm);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficeMDITabSet.OnChildFormDestroy(Sender: TObject);
var
  i, j :integer;
begin
  if not (csDestroying in ComponentState) and (Sender is TForm) and (TForm(Sender).FormStyle = fsMDIChild) then
  begin
    j := -1;
    for i:= 0 to FAdvOfficeTabs.count-1 do
    begin
      if (FAdvOfficeTabs[i].FChildForm = Sender) then
      begin
        j := i;
        break;
      end;
    end;

    if (j >= 0) and Assigned(FAdvOfficeTabs[j]) and Assigned(FAdvOfficeTabs[j].FOnDestroyForm)
       and Assigned(FAdvOfficeTabs[j].FChildForm) then
    begin
      FAdvOfficeTabs[j].FOnDestroyForm(FAdvOfficeTabs[j].FChildForm);
    end;

    if (j >= 0) and not FInternalDelete then
      FAdvOfficeTabs.Delete(j);
  end;
end;

//------------------------------------------------------------------------------

function TAdvOfficeMDITabSet.GetChildForm(
  Tab: TOfficeTabCollectionItem): TForm;
begin
  Result := nil;
  if Assigned(Tab) then
  begin
    Result := Tab.FChildForm;
  end;
end;

//------------------------------------------------------------------------------

function TAdvOfficeMDITabSet.GetTab(AChild: TForm): TOfficeTabCollectionItem;
var
  i: integer;
begin
  Result := nil;
  for I := 0 to FAdvOfficeTabs.Count - 1 do
  begin
    if FAdvOfficeTabs.Items[I].FChildForm = AChild then
    begin
      Result := FAdvOfficeTabs.Items[I];
    end;
  end;
end;

//------------------------------------------------------------------------------

function TAdvOfficeMDITabSet.CanCloseTab(TabIndex: Integer;
  var CloseAction: TCloseAction): Boolean;
var
  Tab: TOfficeTabCollectionItem;
begin
  Result := inherited CanCloseTab(TabIndex, CloseAction);
  if not Result or (TabIndex < 0) or (TabIndex >= FAdvOfficeTabs.Count) then
    Exit;

  Tab := FAdvOfficeTabs.Items[TabIndex];
  if not Assigned(Tab.FChildForm) then
    Exit;

  if Assigned(Tab.FChildForm.OnCloseQuery) then
  begin
    Tab.FChildForm.OnCloseQuery(Tab.FChildForm, Result);
  end;

  if not Result then
    Abort;

  if FreeOnClose then
  begin
    if Assigned(Tab.FChildForm.OnClose) then
    begin
      Tab.FChildForm.OnClose(Tab.FChildForm, CloseAction);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficeMDITabSet.BeforeCloseTab(
  Tab: TOfficeTabCollectionItem; var CloseAction: TCloseAction);
begin
  if not Assigned(Tab) or not Assigned(Tab.FChildForm) then
  begin
    inherited;
    Exit
  end;

  if FreeOnClose then
  begin
    case CloseAction of
      caFree:
      begin
        //FInternalDelete := True;
        Tab.FChildForm.Release;
        CloseAction := caMinimize;  // just to avoid deletion of tab that should be delete on form's destoy
        //FInternalDelete := False;
      end;
      caMinimize: Tab.FChildForm.Windowstate := WSMinimized;
      caHide: Tab.FChildForm.visible := False;
    end;
  end
  else
  begin
    Tab.FChildForm.visible := False;
  end;
  
  inherited;
end;

{$IFDEF FREEWARE}
{$I TRIAL.INC}
{$ENDIF}


end.
