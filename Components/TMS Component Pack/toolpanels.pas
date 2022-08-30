{***********************************************************************}
{ TToolPanels component                                                 }
{ for Delphi & C++Builder                                               }
{                                                                       }
{ written by TMS Software                                               }
{            copyright © 2003 - 2015                                    }
{            Email: info@tmssoftware.com                                }
{            Web: http://www.tmssoftware.com                            }
{                                                                       }
{ The source code is given as is. The author is not responsible         }
{ for any possible damage done due to the use of this code.             }
{ The component can be freely used in any application. The complete     }
{ source code remains property of the author and may not be distributed,}
{ published, given or sold in any form as such. No parts of the source  }
{ code can be included in any other component or application without    }
{ written authorization of the author.                                  }
{***********************************************************************}

unit ToolPanels;

interface

{$R ToolPanels.res}

{$I TMSDEFS.INC}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ImgList, INIFiles, Registry, Math, AdvStyleIF
  , ActiveX, AxCtrls, Types
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;

const
  //SectionHeight = 16;
  SECTION_HEIGHT = 16;
  NODEBTN_SIZE = 8;
  CAPTION_HEIGHT = 23;

  MAJ_VER = 1;  // Major version nr.
  MIN_VER = 6;  // Minor version nr.
  REL_VER = 0;  // Release nr.
  BLD_VER = 0; // Build nr.

  // version history
  // 1.3.1.3  : fixed issue with AutoLock
  // 1.3.1.4  : fix to allow the use of a TAdvToolPanel without TAdvToolPanelTab
  // 1.3.1.5  : set name to TAdvToolPanelTab internal form
  //          : fix for panelform location issue in Delphi 2005
  // 1.3.2.0  : Custom & Whidbey style added
  // 1.3.3.0  : Improvements for hosting in ActiveForms
  // 1.3.3.1  : Fix for controls alignment within bottom toolpanels
  // 1.3.4.0  : Improved sectionimage painting with auto adapt height to image height
  // 1.3.4.1  : Fix for issue with parent form recreate
  // 1.3.5.0  : Added type ssInstant to SlideSpeed property
  // 1.3.5.1  : Fixed issue with width/height changes in floating panels
  // 1.3.5.2  : Fixed issue with form moving for bottom position toolpanels
  // 1.4.0.0  : Added Office 2007 styles
  //          : Added Font, ParentFont property in TAdvToolPanelTab
  // 1.4.0.1  : Fixed issue with tab size calculation for non default fonts
  // 1.4.0.2  : Fixed issue with Accept handling in OnDockOver
  // 1.4.1.0  : New support for Office 2007 silver style added
  // 1.4.1.1  : Improved slide-in with SlideSpeed = ssInstant
  // 1.4.1.2  : Improved drawing of TAdvToolPanel caption with ampersand
  // 1.4.2.0  : New : property AutoOpenOnMouseEnter added
  // 1.4.2.1  : Fixed : issue with ssInstant slidespeed for closing tabs
  // 1.4.2.2  : Fixed : issue with destroying panel while sliding
  // 1.4.2.3  : Fixed : issue with click on toolpanel tab for panels with AutoLock = false
  // 1.4.2.4  : Fixed : issue with top anchored controls on left panels
  // 1.4.2.5  : Fixed : issue with TabSlideOutDone, TabSlideInDone triggered twice
  // 1.4.2.6  : Fixed : painting issue with controls on right positioned TAdvToolPanelTab
  // 1.4.2.7  : Improved : resize handling of floating toolpanels
  // 1.4.2.8  : Fixed : issue with loading toolpanel state at runtime
  //          : Fixed : tab hides when all panels are closed
  // 1.4.2.9  : Fixed : issue with tab width/height with runtime created panels
  // 1.4.2.10 : Fixed : issue with panel position initialization
  // 1.4.2.11 : Improved : handling of destroying a TAdvToolPanel
  // 1.4.3.0  : New : AdvToolPanelTab.Panels.Move method added
  //          : Improved : sequence of programmatically setting items Locked
  // 1.4.4.0  : Fixed : issue with AV on app close with right aligned TAdvToolPanelTab
  // 1.4.4.1  : Fixed : small memory leak 
  // 1.4.4.2  : Fixed : tab hover color displayed when AutoOpenOnMouseEnter = false
  // 1.4.4.3  : Improved : client aligned controls on TAdvToolPanel.  
  // 1.4.4.4  : Fixed : issue with z-order when using multiple AdvToolPanelTab components
  // 1.4.4.5  : Fixed : issue with runtime destroy of TAdvToolPanelTab
  // 1.4.4.6  : Fixed : issue with closing parent form when panelform is still visible
  // 1.4.4.7  : Fixed : issue with border drawing
  //          : Fixed : issue with right-aligned toolpanel unlock
  // 1.4.4.8  : Fixed : issue with right-aligned toolpanel unlock on container control
  // 1.4.5.0  : Improved : public property TextDirection added to TAdvToolPanelTab
  // 1.4.5.1  : Fixed : issue with painting offset for bottom / top panels
  // 1.4.6.0  : New : exposed events OnLock, OnUnlock for TAdvToolPanel
  // 1.4.6.1  : Fixed : issue with closing form and use of right positioned panels
  // 1.4.6.2  : Fixed : issue with maximizing of form & locked panels
  // 1.5.0.0  : New : Windows Vista, Windows 7 & Terminal styles
  // 1.5.0.1  : Fixed : issue with restoring state
  // 1.5.1.0  : New : Exposed OnMouseLeave/OnMouseEnter on TAdvToolPanel level
  // 1.5.1.1  : Fixed : Memory leak issue with runtime created TAdvToolpaneltab
  // 1.5.1.2  : Improved : ssInstant mode for top & bottom positioned TAdvToolPanelTab
  // 1.5.2.0  : New : Built in support for Office 2010 colors
  // 1.5.3.0  : New : Exposed TextDirection property that sets direction of vertically oriented text in tabs
  // 1.5.4.0  : Improved : Code added to avoid that panels were shown in taskbar
  // 1.5.4.1  : Fixed : Issue with resizing unlocked right & bottom positioned toolpanels
  // 1.5.4.2  : Fixed : Issue with destructor under specific circumstances
  // 1.5.5.0  : New : function TAdvToolPanel.XYToSection added
  // 1.5.5.1  : Fixed : Issue with mouse leaving panel on right side over caption
  // 1.5.5.2  : Fixed : Issue with reparenting
  // 1.5.5.3  : Fixed : Design time issue when loading older projects
  // 1.5.5.4  : Fixed : Issue with internal form creating at design time
  // 1.5.5.5  : Improved : Issue with form activation/desactivation when toolpanels become visible
  // 1.5.5.6  : Fixed : Issue with setting all panels hidden and making visible again.
  // 1.5.5.7  : Fixed : Issue with persisting OpenWidth when roll-in process is interrupted
  // 1.5.6.0  : New : Function HidePanels added
  //          : Improved : Handling of TAdvToolPanelTab on modal forms
  // 1.5.6.1  : Fixed : Issue with RestoreState under specific circumstances
  //          : Fixed : Issue with alignment updating of controls on form with ToolPanels
  // 1.5.6.2  : Fixed : Issue with form's OnShow event handling
  // 1.5.7.0  : New : Added CaptionHeight property to TAdvToolPanel
  // 1.5.7.1  : Fixed: Issue with ToolPanel roll in from top position.
  // 1.5.7.2  : Fixed: Issue with opening right-aligned panels in very small forms
  // 1.5.7.3  : Fixed: Issue with ColorTo initialization in esCustom style
  // 1.5.7.4  : Fixed: Issue with resize of bottom or right positioned panels
  // 1.5.7.5  : Improved : Panel size limitation for resize in ppBottom & ppRight modes
  // 1.5.7.6  : Fixed : Issue with moving parent form when option to show Window during move is disabled in Windows
  // 1.5.7.7  : Fixed : Issue with auto hide bottom panels during resize
  // 1.5.7.8  : Fixed : Issue with sizing / lock&unlock in ppBottom tab position
  // 1.5.7.9  : Improved : Decreased sensitivity on splitter mouse handling
  // 1.5.7.10 : Fixed : Painting issue with themed controls on Windows XP
  // 1.6.0.0  : New : Windows 10, Office 2016 styles added
  //          : Improved : Persistence in different files

type
  {$IFDEF DELPHI_UNICODE}
  THintInfo = Controls.THintInfo;
  PHintInfo = Controls.PHintInfo;
  {$ENDIF}

  TAdvToolPanel = class;
  TAdvToolPanelTab = class;

  TPanelState = (psClosed, psOpened, psDocked);
  TGradientDirection = (gdHorizontal, gdVertical);

  TToolPanel = class(TCollectionItem)
  private
    FTag: Integer;
    FPanel: TAdvToolPanel;
    FCaption: string;
    FVisible: Boolean;
    FImageIndex: Integer;
    FState: TPanelState;
    FOpenWidth: Integer;
    procedure SetCaption(const Value: string);
    procedure SetVisible(const Value: Boolean);
    procedure SetImageIndex(const Value: Integer);
    procedure SetState(const Value: TPanelState);
    procedure SetOpenWidth(const Value: Integer);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property Panel: TAdvToolPanel read FPanel write FPanel;
  published
    property Tag: Integer read FTag write FTag;
    property Caption: string read FCaption write SetCaption;
    property Visible: Boolean read FVisible write SetVisible;
    property ImageIndex: Integer read FImageIndex write SetImageIndex;
    property State: TPanelState read FState write SetState;
    property OpenWidth: Integer read FOpenWidth write SetOpenWidth;
  end;

  TToolPanels = class(TCollection)
  private
    FOwner: TAdvToolPanelTab;
    function GetItem(Index: Integer): TToolPanel;
    procedure SetItem(Index: Integer; const Value: TToolPanel);
  public
    constructor Create(AOwner: TAdvToolPanelTab);
    function Add: TToolPanel;
    function Insert(index: Integer): TToolPanel;
    property Items[Index: Integer]: TToolPanel read GetItem write SetItem; default;
    property Panel: TAdvToolPanelTab read FOwner;
    procedure Move(FromIndex, ToIndex: integer);
  end;

  TToolPanelGlyphs = class(TPersistent)
  private
    FCloseGlyphHot: TBitmap;
    FLockGlyphDown: TBitmap;
    FLockGlyphHot: TBitmap;
    FLockGlyphNormal: TBitmap;
    FCloseGlyphDown: TBitmap;
    FCloseGlyphNormal: TBitmap;
    FOnChange: TNotifyEvent;
    FUnLockGlyphDown: TBitmap;
    FUnLockGlyphHot: TBitmap;
    FUnLockGlyphNormal: TBitmap;
    procedure SetCloseGlyphNormal(const Value: TBitmap);
    procedure SetCloseGlyphDown(const Value: TBitmap);
    procedure SetCloseGlyphHot(const Value: TBitmap);
    procedure SetLockGlyphDown(const Value: TBitmap);
    procedure SetLockGlyphHot(const Value: TBitmap);
    procedure SetLockGlyphNormal(const Value: TBitmap);
    procedure SetUnLockGlyphDown(const Value: TBitmap);
    procedure SetUnLockGlyphHot(const Value: TBitmap);
    procedure SetUnLockGlyphNormal(const Value: TBitmap);
  protected
    procedure Changed;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property CloseGlyphNormal: TBitmap read FCloseGlyphNormal write SetCloseGlyphNormal;
    property CloseGlyphDown: TBitmap read FCloseGlyphDown write SetCloseGlyphDown;
    property CloseGlyphHot: TBitmap read FCloseGlyphHot write SetCloseGlyphHot;
    property LockGlyphNormal: TBitmap read FLockGlyphNormal write SetLockGlyphNormal;
    property LockGlyphDown: TBitmap read FLockGlyphDown write SetLockGlyphDown;
    property LockGlyphHot: TBitmap read FLockGlyphHot write SetLockGlyphHot;
    property UnLockGlyphNormal: TBitmap read FUnLockGlyphNormal write SetUnLockGlyphNormal;
    property UnLockGlyphDown: TBitmap read FUnLockGlyphDown write SetUnLockGlyphDown;
    property UnLockGlyphHot: TBitmap read FUnLockGlyphHot write SetUnLockGlyphHot;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TSectionCorners = (scRectangle, scRoundLeft, scRoundRight, scRoundLeftRight);

  TSectionLayout = class(TPersistent)
  private
    FCaptionColor: TColor;
    FCaptionColorTo: TColor;
    FBackGroundColor: TColor;
    FBackGroundColorTo: TColor;
    FBorderColor: TColor;
    FBorderWidth: integer;

    FOnChange: TNotifyEvent;
    FBackGroundGradientDir: TGradientDirection;
    FCaptionGradientDirection: TGradientDirection;
    FItemFontColor: TColor;
    FUnderLineCaption: Boolean;
    FItemHoverTextColor: TColor;
    FItemHoverUnderline: Boolean;
    FCaptionFontColor: TColor;
    FSpacing: integer;
    FIndent: integer;
    FCaptionRounded: Boolean;
    FCorners: TSectionCorners;
    procedure SetBackGroundColor(const Value: TColor);
    procedure SetBackGroundColorTo(const Value: TColor);
    procedure SetBorderColor(const Value: TColor);
    procedure SetBorderWidth(const Value: integer);
    procedure SetCaptionColor(const Value: TColor);
    procedure SetCaptionColorTo(const Value: TColor);
    procedure SetBackGroundGradientDir(const Value: TGradientDirection);
    procedure SetCaptionGradientDirection(const Value: TGradientDirection);
    procedure SetItemFontColor(const Value: TColor);
    procedure SetUnderLineCaption(const Value: Boolean);
    procedure SetItemHoverTextColor(const Value: TColor);
    procedure SetItemHoverUnderline(const Value: Boolean);
    procedure SetCaptionFontColor(const Value: TColor);
    procedure SetIndent(const Value: integer);
    procedure SetSpacing(const Value: integer);
    procedure SetCaptionRounded(const Value: Boolean);
    procedure SetCorners(const Value: TSectionCorners);
  protected
    procedure Changed;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property CaptionColor: TColor read FCaptionColor write SetCaptionColor;
    property CaptionColorTo: TColor read FCaptionColorTo write SetCaptionColorTo;
    property CaptionFontColor: TColor read FCaptionFontColor write SetCaptionFontColor;
    property CaptionRounded: Boolean read FCaptionRounded write SetCaptionRounded;
    property Corners: TSectionCorners read FCorners write SetCorners;
    property BackGroundColor: TColor read FBackGroundColor write SetBackGroundColor;
    property BackGroundColorTo: TColor read FBackGroundColorTo write SetBackGroundColorTo;
    property BorderColor: TColor read FBorderColor write SetBorderColor;
    property BorderWidth: integer read FBorderWidth write SetBorderWidth;
    property CaptionGradientDir: TGradientDirection read FCaptionGradientDirection write SetCaptionGradientDirection default gdHorizontal;
    property BackGroundGradientDir: TGradientDirection read FBackGroundGradientDir write SetBackGroundGradientDir default gdHorizontal;
    property Indent: integer read FIndent write SetIndent;
    property Spacing: integer read FSpacing write SetSpacing;
    property ItemFontColor: TColor read FItemFontColor write SetItemFontColor;
    property ItemHoverTextColor: TColor read FItemHoverTextColor write SetItemHoverTextColor;
    property ItemHoverUnderline: Boolean read FItemHoverUnderline write SetItemHoverUnderline;
    property UnderLineCaption: Boolean read FUnderLineCaption write SetUnderLineCaption;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TAdvToolPanelSection = class;

  TSectionItem = class(TCollectionItem)
  private
    FImageIndex: Integer;
    FCaption: string;
    FHint: string;
    FEnabled: boolean;
    FTag: Integer;
    FObject: TObject;
    procedure SetCaption(const Value: string);
    procedure SetImageIndex(const Value: Integer);
    procedure SetEnabled(const Value: boolean);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property ItemObject: TObject read FObject write FObject;
  published
    property Caption: string read FCaption write SetCaption;
    property Enabled: boolean read FEnabled write SetEnabled;
    property Hint: string read FHint write FHint;
    property ImageIndex: Integer read FImageIndex write SetImageIndex;
    property Tag: Integer read FTag write FTag;
  end;

  TSectionItems = class(TCollection)
  private
    FOwner: TAdvToolPanel;
    FOnItemAdd: TNotifyEvent;
    FOnItemDelete: TNotifyEvent;
    FAdvToolPanelSection: TAdvToolPanelSection;
    function GetItem(Index: Integer): TSectionItem;
    procedure SetItem(Index: Integer; const Value: TSectionItem);
  protected
    function GetOwner: TPersistent; override;
    property AdvToolPanelSection: TAdvToolPanelSection read FAdvToolPanelSection write FAdvToolPanelSection;
  public
    constructor Create(AOwner: TAdvToolPanel);
    function Add: TSectionItem;
    function Insert(Index: Integer): TSectionItem;
    property Items[Index: Integer]: TSectionItem read GetItem write SetItem; default;
    property OnItemAdd: TNotifyEvent read FOnItemAdd write FOnItemAdd;
    property OnItemDelete: TNotifyEvent read FOnItemDelete write FOnItemDelete;
  end;

  TAdvToolPanelSection = class(TCollectionItem)
  private
    FHeight: Integer;
    FActHeight: integer;
    FCaption: string;
    FSectionItems: TSectionItems;
    FCaptionImageIndex: integer;
    FNode: Boolean;
    FExpanded: Boolean;
    FDraw: boolean;
    FAutosize: boolean;
    FSectionItemHeight: integer;
    FInternalCall: boolean;
    FControlList: TList;
    procedure SetCaption(const Value: string);
    procedure SetHeight(const Value: Integer);
    procedure SetCaptionImageIndex(const Value: integer);
    procedure SetNode(const Value: Boolean);
    procedure SetExpanded(const Value: Boolean);
    property ActualHeight: integer read FActHeight;
    procedure SetDraw(const Value: boolean);
    procedure SetAutosize(const Value: boolean);
    procedure OnSectionItemsAdd(Sender: TObject);
    procedure OnSectionItemsDelete(Sender: TObject);
  protected
    function GetDisplayName: string; override;
    property ControlList: TList read FControlList;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property Expanded: Boolean read FExpanded write SetExpanded default true;
  published
    property Autosize: boolean read FAutosize write SetAutosize;
    property Caption: string read FCaption write SetCaption;
    property Draw: boolean read FDraw write SetDraw;
    property SectionItems: TSectionItems read FSectionItems write FSectionItems;
    property Height: Integer read FHeight write SetHeight;
    property Node: Boolean read FNode write SetNode default false;
    property CaptionImageIndex: integer read FCaptionImageIndex write SetCaptionImageIndex;
  end;

  TAdvToolPanelSections = class(TCollection)
  private
    FOwner: TAdvToolPanel;
    function GetItem(Index: Integer): TAdvToolPanelSection;
    procedure SetItem(Index: Integer; const Value: TAdvToolPanelSection);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TAdvToolPanel);
    function Add: TAdvToolPanelSection;
    function Insert(Index: Integer): TAdvToolPanelSection;
    property Items[Index: Integer]: TAdvToolPanelSection read GetItem write SetItem; default;
  end;

  TOnItemClick = procedure(Sender: TObject; SectionIndex: Integer; ItemIndex: Integer) of object;
  TOnCaptionButtonClick = procedure(Sender: TObject; CaptionButtonRect: TRect) of object;
  TNodeExpandEvent = procedure(Sender: TObject; SectionIndex: integer) of object;
  TNodeCollapseEvent = TNodeExpandEvent;

  TTPBackGroundPosition = (bpTopLeft, bpTopRight, bpBottomLeft, bpBottomRight, bpTiled, bpStretched, bpCenter);
  TToolPanelStyle = (esOffice2003Blue, esOffice2003Silver, esOffice2003Olive, esOffice2003Classic, esOffice2007Luna, esOffice2007Obsidian, esWindowsXP, esWhidbey, esCustom, esOffice2007Silver, esWindowsVista,
    esWindows7, esTerminal, esOffice2010Blue, esOffice2010Silver, esOffice2010Black,
    esWindows8, esOffice2013White, esOffice2013Lightgray, esOffice2013Gray,
    esWindows10, esOffice2016White, esOffice2016Gray, esOffice2016Black);

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvToolPanel = class(TCustomPanel, ITMSStyleEx)
  private
    FImageIndex: Integer;
    FTab: TAdvToolPanelTab;
    FOpenWidth: Integer;
    FMouseDown: Boolean;
    FLocked: Boolean;
    FCloseGlyph: TBitmap;
    FLockedGlyph: TBitmap;
    FUnLockedGlyph: TBitmap;
    FCanSize: Boolean;
    FHidden: Boolean;
    FCLoseHint: string;
    FLockHint: string;
    FUnlockHint: string;
    FActive: Boolean;
    FFocusCaptionColor: TColor;
    FFocusCaptionFontColor: TColor;
    FNoFocusCaptionColor: TColor;
    FNoFocusCaptionFontColor: TColor;
    FDocking: Boolean;
    FShowCaption: Boolean;
    FAllowDocking: Boolean;
    FRestored: Boolean;
    FTabName: string;
    FShowClose: Boolean;
    FShowLock: Boolean;
    FForceResize: Boolean;
    FAutoLock: Boolean;
    FNoFocusCaptionColorTo: TColor;
    FFocusCaptionColorTo: TColor;
    FCaptionGradientDirection: TGradientDirection;
    FShowCaptionBorder: boolean;
    FDockDots: boolean;
    FCaptionButton: boolean;
    FOnCaptionBtnClick: TOnCaptionButtonClick;
    FOnCaptionDblClick: TNotifyEvent;
    FButton3D: Boolean;
    FHoverButtonColor: TColor;
    FDownButtonColor: TColor;
    FHoverButtonColorTo: TColor;
    FDownButtonColorTo: TColor;
    FSections: TAdvToolPanelSections;
    FSizing: Boolean;
    FSizeSection: Integer;
    FSizeHeight: Integer;
    FSizeY: Integer;
    //FSectionColorTo: TColor;
    //FSectionColor: TColor;
    FSectionLayout: TSectionLayout;
    FColorTo: TColor;
    FGradientDirection: TGradientDirection;
    FSectionImages: TImageList;
    FOnNodeCollapse: TNodeCollapseEvent;
    FOnNodeExpand: TNodeExpandEvent;
    FOldFloating: Boolean;
    FHoverSectionIndex: integer;
    FHoverSectionItemIndex: integer;
    FHoverSectionItemRect: TRect;
    FOnItemClick: TOnItemClick;
    FBackGround: TBitMap;
    FBackGroundPosition: TTPBackgroundPosition;
    FStyle: TToolPanelStyle;
    FBackgroundTransparent: Boolean;
    FOnLock: TNotifyEvent;
    FOnUnlock: TNotifyEvent;
    FCaptionHeight: integer;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMEXITSIZEMOVE(var Msg: TMessage); message WM_EXITSIZEMOVE;
    procedure WMENTERSIZEMOVE(var Msg: TMessage); message WM_ENTERSIZEMOVE;
    procedure WMNCHitTest(var Msg: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMEraseBkGnd(var Message: TMessage); message WM_ERASEBKGND;
    procedure WMLDblClk(var Msg: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure WMMouseLeave(var Message: TMessage); message WM_MOUSELEAVE;
    procedure WMSetCursor(var Message: TWMSetCursor); message WM_SETCURSOR;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMHintShow(var Msg: TMessage); message CM_HINTSHOW;
    procedure CMDesignHitTest(var Msg: TCMDesignHitTest); message CM_DESIGNHITTEST;
    procedure SetLocked(const Value: Boolean);
    function GetCaptionEx: string;
    procedure SetCaptionEx(const Value: string);
    procedure SetImageIndex(const Value: Integer);
    procedure SetHidden(const Value: Boolean);
    function GetIsVisible: Boolean;
    procedure SetFocusCaptionColor(const Value: TColor);
    procedure SetFocusCaptionFontColor(const Value: TColor);
    procedure SetNoFocusCaptionColor(const Value: TColor);
    procedure SetNoFocusCaptionFontColor(const Value: TColor);
    //procedure DrawMonoBitmap(ACanvas: TCanvas; X,Y: Integer;ABitmap: TBitmap; FGColor,BkColor: TColor);
    procedure SetDocking(const Value: Boolean);
    procedure SetShowCaption(const Value: Boolean);
    procedure SetOpenWidth(const Value: Integer);
    procedure SetShowClose(const Value: Boolean);
    procedure SetShowLock(const Value: Boolean);
    procedure SetCaptionGradientDirection(const Value: TGradientDirection);
    procedure SetFocusCaptionColorTo(const Value: TColor);
    procedure SetNoFocusCaptionColorTo(const Value: TColor);
    procedure SetShowCaptionBorder(const Value: boolean);
    procedure SetDockDots(const Value: boolean);
    procedure SetCaptionButton(const Value: boolean);
    procedure CaptionButtonClick(CaptionBtnRect: TRect);
    //procedure SetSectionColor(const Value: TColor);
    //procedure SetSectionColorTo(const Value: TColor);
    procedure SetColorTo(const Value: TColor);
    procedure SetGradientDirection(const Value: TGradientDirection);

    procedure SectionLayoutChanged(Sender: TObject);
    function SectionRect(SectionIndex: integer; Actual: boolean): TRect;
    //function SectionExpandedRect(SectionIndex: integer): TRect;
    function NodeBtnRect(SectionIndex: integer): TRect;
    procedure SetBackground(const Value: TBitMap);
    procedure SetBackGroundPosition(const value: TTPBackgroundPosition);
    procedure SetBackgroundTransparent(Value: Boolean);
    procedure SetStyle(const Value: TToolPanelStyle);
    procedure SetStyleAndAppColor(const Value: TToolPanelStyle; AppColor: TColor);
    procedure SetComponentStyle(AStyle: TTMSStyle);
    procedure SetComponentStyleAndAppColor(AStyle: TTMSStyle; AppColor: TColor);
    procedure SetColorEx(const Value: TColor);
    function GetColorEx: TColor;
    procedure SetVersion(const Value: string);
    function GetVersion: string;
    function GetVersionNr: Integer;
    function GetSectionHeight: Integer;
    procedure SetSectionImages(Value: TImageList);
    procedure SetCaptionHeight(const Value: integer);
  protected
    procedure Paint; override;
    procedure Resize; override;
    procedure AlignControls(AControl: TControl; var Rect: TRect); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    property Restored: Boolean read FRestored write FRestored default False;
    property TabName: string read FTabName write FTabName;
    function HasFocusControl: Boolean;
    procedure UpdateControlPos(SectionIndex, Diff: integer);
    procedure UpdateControlPosBelowSection(SectionIndex, Diff: integer);
    function PtOnAnyItem(X, Y: integer; var SectionIndex: integer; var ItemRect: TRect): Integer;
    property SectionHeight: Integer read GetSectionHeight;
    procedure FitInAlignedControls;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure DoEndDock(Target: TObject; X, Y: Integer); override;
    procedure DoStartDock(var DragObject: TDragObject); override;

    property Tab: TAdvToolPanelTab read FTab write FTab;
    property Locked: Boolean read FLocked write SetLocked;
    property Hidden: Boolean read FHidden write SetHidden;
    property Docking: Boolean read FDocking write SetDocking;
    property IsVisible: Boolean read GetIsVisible;
    function XYToSection(X,Y: integer): integer;
    procedure SetTopMost;
    property Sizing: boolean read FSizing;
  published
    property Align;
    property Alignment;
    property AllowDocking: Boolean read FAllowDocking write FAllowDocking default True;
    property AutoLock: Boolean read FAutoLock write FAutoLock default False;
    property Background: TBitmap read FBackground write SetBackground;
    property BackgroundTransparent: Boolean read FBackgroundTransparent write SetBackgroundTransparent;
    property BackGroundPosition: TTPBackgroundPosition read FBackGroundPosition write SetBackGroundPosition;
    property BevelInner;
    property BevelOuter;
    property BevelWidth;
    property BorderStyle;
    property BorderWidth;
    property Button3D: Boolean read FButton3D write FButton3D default true;
    property HoverButtonColor: TColor read FHoverButtonColor write FHoverButtonColor;
    property HoverButtonColorTo: TColor read FHoverButtonColorTo write FHoverButtonColorTo;
    property DownButtonColor: TColor read FDownButtonColor write FDownButtonColor;
    property DownButtonColorTo: TColor read FDownButtonColorTo write FDownButtonColorTo;
    property CaptionButton: Boolean read FCaptionButton write SetCaptionButton;
    property Color: TColor read GetColorEx write SetColorEx default $00FFE7D6;
    property ColorTo: TColor read FColorTo write SetColorTo default $00F7EFDE;
    property GradientDirection: TGradientDirection read FGradientDirection write SetGradientDirection default gdHorizontal;
    property Cursor;
    property DockDots: boolean read FDockDots write SetDockDots;
    property Hint;
    property PopupMenu;
    property CanSize: Boolean read FCanSize write FCanSize default True;
    property Caption: string read GetCaptionEx write SetCaptionEx;
    property Font;
    property CaptionGradientDirection: TGradientDirection read FCaptionGradientDirection write SetCaptionGradientDirection;
    property CaptionHeight: integer read FCaptionHeight write SetCaptionHeight default CAPTION_HEIGHT;
    property ImageIndex: Integer read FImageIndex write SetImageIndex default -1;
    property FocusCaptionFontColor: TColor read FFocusCaptionFontColor write SetFocusCaptionFontColor;
    property FocusCaptionColor: TColor read FFocusCaptionColor write SetFocusCaptionColor;
    property FocusCaptionColorTo: TColor read FFocusCaptionColorTo write SetFocusCaptionColorTo;
    property NoFocusCaptionFontColor: TColor read FNoFocusCaptionFontColor write SetNoFocusCaptionFontColor;
    property NoFocusCaptionColor: TColor read FNoFocusCaptionColor write SetNoFocusCaptionColor;
    property NoFocusCaptionColorTo: TColor read FNoFocusCaptionColorTo write SetNoFocusCaptionColorTo;
    property OpenWidth: Integer read FOpenWidth write SetOpenWidth default 150;
    property CloseHint: string read FCloseHint write FCloseHint;
    property LockHint: string read FLockHint write FLockHint;
    property UnlockHint: string read FUnlockHint write FUnlockHint;
    property Sections: TAdvToolPanelSections read FSections write FSections;
    property SectionLayout: TSectionLayout read FSectionLayout write FSectionLayout;
    property SectionImages: TImageList read FSectionImages write SetSectionImages;
    //property SectionColor: TColor read FSectionColor write SetSectionColor;
    //property SectionColorTo: TColor read FSectionColorTo write SetSectionColorTo;
    property ShowCaption: Boolean read FShowCaption write SetShowCaption default True;
    property ShowCaptionBorder: boolean read FShowCaptionBorder write SetShowCaptionBorder default true;
    property ShowClose: Boolean read FShowClose write SetShowClose default True;
    property ShowLock: Boolean read FShowLock write SetShowLock default True;
    property ShowHint;
    property Style: TToolPanelStyle read FStyle write SetStyle default esOffice2003Blue;
    property Version: string read GetVersion write SetVersion;
    property OnCaptionBtnClick: TOnCaptionButtonClick read FOnCaptionBtnClick write FOnCaptionBtnClick;
    property OnCaptionDblClick: TNotifyEvent read FOnCaptionDblClick write FOnCaptionDblClick;
    property OnClick;
    property OnDblClick;
    property OnCanResize;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;                
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnItemClick: TOnItemClick read FOnItemClick write FOnItemClick;
    property OnLock: TNotifyEvent read FOnLock write FOnLock;
    property OnUnlock: TNotifyEvent read FOnUnlock write FOnUnlock;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
    {$IFDEF DELPHI2006_LVL}
    property OnMouseLeave;
    property OnMouseEnter;
    {$ENDIF}
    property OnNodeExpand: TNodeExpandEvent read FOnNodeExpand write FOnNodeExpand;
    property OnNodeCollapse: TNodeCollapseEvent read FOnNodeCollapse write FOnNodeCollapse;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

  TToolPanelForm = class(TForm)
  private
    FTab: TAdvToolPanelTab;
    procedure WMNCHitTest(var Msg: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMActivate(var Message: TWMActivate); message WM_ACTIVATE;
    procedure WMGetMinMaxInfo(var Msg: TMessage); message WM_GETMINMAXINFO;
    procedure WMEnterSizeMove(var Msg: TMessage); message WM_ENTERSIZEMOVE;
    procedure WMExitSizeMove(var Msg: TMessage); message WM_EXITSIZEMOVE;
  protected
    function GetParentWnd: HWnd;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure FormShow(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    property Tab: TAdvToolPanelTab read FTab write FTab;
  end;

  TPanelPosition = (ppLeft, ppRight, ppTop, ppBottom);

  TSlideSpeed = (ssSlow, ssMedium, ssFast, ssInstant);

  TAutoOpenCloseSpeed = (aocSlow, aocMedium, aocFast, aocVerySlow, aocVeryFast);

  TTabEvent = procedure(Sender: TObject; Index: Integer; APanel: TAdvToolPanel) of object;

  TAllowTabEvent = procedure(Sender: TObject; Index: Integer; APanel: TAdvToolPanel; var Allow: Boolean) of object;

  TPanelPersistLocation = (pplINIFile, pplRegistry);

  TPanelPersistence = class(TPersistent)
  private
    FAutoSave: Boolean;
    FAutoLoad: Boolean;
    FINIFile: string;
    FLocation: TPanelPersistLocation;
    FRegistryKey: string;
    FOnChange: TNotifyEvent;
    procedure SetINIFile(const Value: string);
    procedure SetLocation(const Value: TPanelPersistLocation);
    procedure SetRegistryKey(const Value: string);
  protected
    procedure DoChange;
  published
    property AutoLoad: Boolean read FAutoLoad write FAutoLoad default False;
    property AutoSave: Boolean read FAutoSave write FAutoSave default False;
    property INIFile: string read FINIFile write SetINIFile;
    property RegistryUserKey: string read FRegistryKey write SetRegistryKey;
    property Location: TPanelPersistLocation read FLocation write SetLocation default pplINIFile;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TTextDirection = (tdTopToBottom, tdBottomToTop);

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvToolPanelTab = class(TCustomPanel, ITMSStyleEx)
  private
    FPanels: TToolPanels;
    FImages: TImageList;
    FRollInBusy: Boolean;
    FRollInOut: Boolean;
    FRollOutPanel: Integer;
    FRollInCandidate: Integer;
    FTimerID: Integer;
    FTimerCount: Integer;
    FTimerRollIn: Integer;
    FTimerEnable: Boolean;
    FPosition: TPanelPosition;
    FPanelForm: TToolPanelForm;
    FFormWndProc: TWndMethod;
    FDesignViewPanel: Integer;
    FTabColor: TColor;
    FLastMousePos: TPoint;
    FAutoDock: Boolean;
    FAutoOpenCloseSpeed: TAutoOpenCloseSpeed;
    FSlideSpeed: TSlideSpeed;
    FPaintOffset: Integer;
    FUpDown: TBitmap;
    FColorTo: TColor;
    FTabColorTo: TColor;
    FTabBorderColor: TColor;
    FHoverPanel: Integer;
    FMousePanel: Integer;
    FTabHoverColor: TColor;
    FTabHoverColorTo: TColor;
    FTabWidth: Integer;
    FTabGlyph: TBitmap;
    FOnTabLeave: TTabEvent;
    FOnTabEnter: TTabEvent;
    FOnTabRightClick: TTabEvent;
    FOnTabLeftClick: TTabEvent;
    FOnTabSlideIn: TTabEvent;
    FOnTabSlideInDone: TTabEvent;
    FOnTabSlideOut: TTabEvent;
    FOnTabSlideOutDone: TTabEvent;
    FPersist: TPanelPersistence;
    FOnTabHide: TTabEvent;
    FOnAllowTabHide: TAllowTabEvent;
    FPanelGlyphs: TToolPanelGlyphs;
    FAutoThemeAdapt: Boolean;
    FStyle: TToolPanelStyle;
    FMouseInControl: Boolean;
    FAutoOpenOnMouseEnter: Boolean;
    FParentHooked: boolean;
    FDisableParentHook: boolean;
    FRollInOutActive: boolean;
    FRestored: boolean;
    FWMDestroy: boolean;
    FTextDirection: TTextDirection;
    FCQEvent: TCloseQueryEvent;
    FFSEvent: TNotifyEvent;
    function GetPanelFromTab(x, y: Integer): Integer;
    function GetTabSize: Integer;
    procedure WMTimer(var Message: TWMTimer); message WM_TIMER;
    procedure WMCommand(var Message: TMessage); message WM_COMMAND;
    procedure WMEraseBkGnd(var Message: TMessage); message WM_ERASEBKGND;
    procedure SetPosition(const Value: TPanelPosition);
    procedure SubclassProc(var Msg: TMessage);
    procedure SetTabColor(const Value: TColor);
    procedure SetAutoDock(const Value: Boolean);
    procedure SetColorTo(const Value: TColor);
    procedure SetTabColorTo(const Value: TColor);
    procedure SetTabBorderColor(const Value: TColor);
    procedure SetTabWidth(const Value: Integer);
    procedure SetTabGlyph(const Value: TBitmap);
    procedure ThemeAdapt;
    procedure SetStyle(const Value: TToolPanelStyle);
    procedure SetStyleAndAppColor(const Value: TToolPanelStyle; AppColor: TColor);
    procedure SetComponentStyle(AStyle: TTMSStyle);
    procedure SetComponentStyleAndAppColor(AStyle: TTMSStyle; AppColor: TColor);
    procedure SetTextDirection(const Value: TTextDirection);
  protected
    procedure CMDesignHitTest(var Msg: TCMDesignHitTest); message CM_DESIGNHITTEST;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure Paint; override;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure LockChange(APanel: TAdvToolPanel);
    procedure PersistenceChange(Sender: TObject);
    procedure RollInOut(Index: Integer);
    procedure WndProc(var Msg: TMessage); override;
    procedure Resize; override;
    procedure GetSiteInfo(Client: TControl; var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean); override;
    procedure ResetTimer;
    procedure EnableTimer(Enabled: Boolean);
    procedure INISaveState;
    procedure REGSaveState;
    function INIRestoreState: Boolean;
    function REGRestoreState: Boolean;
    procedure IRollIn(APanel: TAdvToolPanel);
    procedure IRollOut(APanel: TAdvToolPanel);
    function GetVersion:string;
    procedure SetVersion(const Value: string);
    function GetVersionNr: Integer;
    property Restored: boolean read FRestored write FRestored;
    procedure CreatePanelForm;
    procedure TabCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure TabFormShow(Sender: TObject);
    procedure UpdatePanelForm;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdatePanels(Index: Integer);
    procedure UnHookParent;
    procedure HookParent;
    procedure Loaded; override;
    procedure CreateWnd; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure DockOver(Source: TDragDockObject; X, Y: Integer; State: TDragState; var Accept: Boolean); override;
    procedure DockDrop(Source: TDragDockObject; X, Y: Integer); override;
    procedure RemovePanel(APanel: TAdvToolPanel);
    procedure InsertPanel(APanel: TAdvToolPanel);
    procedure UpdatePanel(APanel: TAdvToolPanel);
    procedure UnHidePanel(APanel: TAdvToolPanel);
    procedure UnHidePanels;
    procedure HidePanels;
    procedure RollIn(APanel: TAdvToolPanel);
    procedure RollOut(APanel: TAdvToolPanel);
    function PanelsInTab: Integer;
    function CreatePanel: TAdvToolPanel;
    property Panels: TToolPanels read FPanels write FPanels;
    procedure NextPanel;
    procedure PrevPanel;
    function PanelIndex(APanel: TAdvToolPanel): Integer;
    function PanelByName(AName: string): TAdvToolPanel;
    procedure SaveState;
    procedure RestoreState;
    function NumPanelsLocked: Integer;
    property DisableParentHook: boolean read FDisableParentHook write FDisableParentHook;
  published
    property Alignment;
    property AutoDock: Boolean read FAutoDock write SetAutoDock default True;
    property AutoOpenCloseSpeed: TAutoOpenCloseSpeed read FAutoOpenCloseSpeed write FAutoOpenCloseSpeed default aocMedium;
    property AutoOpenOnMouseEnter: boolean read FAutoOpenOnMouseEnter write FAutoOpenOnMouseEnter default true;
    property AutoThemeAdapt: Boolean read FAutoThemeAdapt write FAutoThemeAdapt default False;
    property Color;
    property ColorTo: TColor read FColorTo write SetColorTo default clNone;
    property Font;
    property Hint;
    property Images: TImageList read FImages write FImages;
    property PanelGlyphs: TToolPanelGlyphs read FPanelGlyphs write FPanelGlyphs;
    property ParentFont;
    property Persist: TPanelPersistence read FPersist write FPersist;
    property PopupMenu;
    property Position: TPanelPosition read FPosition write SetPosition;
    property SlideSpeed: TSlideSpeed read FSlideSpeed write FSlideSpeed default ssMedium;
    property Style: TToolPanelStyle read FStyle write SetStyle default esOffice2003Blue;
    property TabBorderColor: TColor read FTabBorderColor write SetTabBorderColor default clGray;
    property TabColor: TColor read FTabColor write SetTabColor default clBtnFace;
    property TabColorTo: TColor read FTabColorTo write SetTabColorTo default clNone;
    property TabHoverColor: TColor read FTabHoverColor write FTabHoverColor default clNone;
    property TabHoverColorTo: TColor read FTabHoverColorTo write FTabHoverColorTo default clNone;
    property TabWidth: Integer read FTabWidth write SetTabWidth default 23;
    property TabGlyph: TBitmap read FTabGlyph write SetTabGlyph;
    property TextDirection: TTextDirection read FTextDirection write SetTextDirection default tdTopToBottom;
    property Version: string read GetVersion write SetVersion;
    property Visible;
    property OnDockOver;
    property OnDockDrop;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
    property OnKeyDown;
    property OnKeyUp;
    property OnKeyPress;
    property OnTabEnter: TTabEvent read FOnTabEnter write FOnTabEnter;
    property OnTabLeave: TTabEvent read FOnTabLeave write FOnTabLeave;
    property OnTabLeftClick: TTabEvent read FOnTabLeftClick write FOnTabLeftClick;
    property OnTabRightClick: TTabEvent read FOnTabRightClick write FOnTabRightClick;
    property OnTabSlideOut: TTabEvent read FOnTabSlideOut write FOnTabSlideOut;
    property OnTabSlideIn: TTabEvent read FOnTabSlideIn write FOnTabSlideIn;
    property OnTabSlideOutDone: TTabEvent read FOnTabSlideOutDone write FOnTabSlideOutDone;
    property OnTabSlideInDone: TTabEvent read FOnTabSlideInDone write FOnTabSlideInDone;
    property OnTabHide: TTabEvent read FOnTabHide write FOnTabHide;
    property OnAllowTabHide: TAllowTabEvent read FOnAllowTabHide write FOnAllowTabHide;
  end;


implementation

uses
  ComObj;

const
  DOCKDOTS_WIDTH = 8;
  // theme changed notifier
  WM_THEMECHANGED = $031A;

type
  TMonologPalette = packed record
    palVersion: Word;
    palNumEntries: Word;
    palPalEntry: array[0..1] of TPaletteEntry;
  end;

  XPColorScheme = (xpNone, xpBlue, xpGreen, xpGray);

var
  GetCurrentThemeName: function(pszThemeFileName: PWideChar;
    cchMaxNameChars: Integer;
    pszColorBuff: PWideChar;
    cchMaxColorChars: Integer;
    pszSizeBuff: PWideChar;
    cchMaxSizeChars: Integer): THandle cdecl stdcall;

  IsThemeActive: function: BOOL cdecl stdcall;


function IsWinXP: Boolean;
var
  VerInfo: TOSVersioninfo;
begin
  VerInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
  GetVersionEx(verinfo);
  Result := (verinfo.dwMajorVersion > 5) or
    ((verinfo.dwMajorVersion = 5) and (verinfo.dwMinorVersion >= 1));
end;

function IsWinVista: Boolean;
var
  VerInfo: TOSVersioninfo;
begin
  VerInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
  GetVersionEx(verinfo);
  Result := (verinfo.dwMajorVersion >= 6);
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


{ TToolPanels }


function TToolPanels.Add: TToolPanel;
begin
  Result := TToolPanel(inherited Add);
end;

constructor TToolPanels.Create(AOwner: TAdvToolPanelTab);
begin
  inherited Create(TToolPanel);
  FOwner := AOwner;
end;

function TToolPanels.GetItem(Index: Integer): TToolPanel;
begin
  Result := TToolPanel(inherited Items[Index]);
end;

function TToolPanels.Insert(Index: Integer): TToolPanel;
begin
  Result := TToolPanel(inherited Insert(Index));
end;

procedure TToolPanels.Move(FromIndex, ToIndex: integer);
var
  origitem: TToolPanel;
begin
  origitem := Items[FromIndex];
  Insert(ToIndex).Assign(origitem);
  origitem.Free;
  FOwner.Invalidate;
end;

procedure TToolPanels.SetItem(Index: Integer; const Value: TToolPanel);
begin
  inherited Items[Index] := Value;
end;

{ TAdvToolPanelTab }

constructor TAdvToolPanelTab.Create(AOwner: TComponent);
var
  FDesignTime: boolean;
begin
  inherited Create(AOwner);
  FPanels := TToolPanels.Create(Self);
  FWMDestroy := false;
  Caption := '';
  ControlStyle := ControlStyle - [csAcceptsControls, csDesignInteractive];
  FRollInOut := False;
  FRollInCandidate := -1;
  FRollOutPanel := -1;
  FHoverPanel := -1;
  FPosition := ppLeft;
  FTimerEnable := True;
  Align := alLeft;

  //CreatePanelForm;

  Color := clInfoBk;
  FColorTo := clNone;
  FSlideSpeed := ssMedium;
  FAutoOpenCloseSpeed := aocMedium;
  DoubleBuffered := True;
  FPaintOffset := 0;
  FUpDown := TBitmap.Create;
  FUpDown.LoadFromResourceName(Hinstance, 'TMS_UPDOWN');
  FTabColor := clBtnFace;
  FTabColorTo := clNone;
  FTabHoverColor := clNone;
  FTabHoverColorTo := clNone;
  FTabBorderColor := clGray;
  FTabWidth := 23;
  FTabGlyph := TBitmap.Create;
  FAutoDock := True;
  DockSite := True;
  FPersist := TPanelPersistence.Create;
  FPersist.OnChange := PersistenceChange;
  FPanelGlyphs := TToolPanelGlyphs.Create;

  FDesignTime := (csDesigning in ComponentState) and not
                 ((csReading in Owner.ComponentState) or (csLoading in Owner.ComponentState));
  if FDesignTime then
    Style := esOffice2003Blue;

  Font.Name := 'Tahoma'; // force a truetype font for font rotation
  FAutoOpenOnMouseEnter := true;
  FDisableParentHook := false;
  FTextDirection := tdTopToBottom;
end;

procedure TAdvToolPanelTab.CMDesignHitTest(var Msg: TCMDesignHitTest);
begin
  if Position = ppLeft then
  begin
    if (Msg.XPos <= FTabWidth) and (GetAsyncKeyState(VK_LBUTTON) <> 0) and (GetPanelFromTab(Msg.XPos, Msg.YPos) >= 0) then
      Msg.Result := 1
    else
      inherited;
  end;

  if Position = ppRight then
  begin
    if (Msg.XPos > Width - FTabWidth) and (GetAsyncKeyState(VK_LBUTTON) <> 0) and (GetPanelFromTab(Msg.XPos, Msg.YPos) >= 0) then
      Msg.Result := 1
    else
      inherited;
  end;

  if Position = ppTop then
  begin
    if (Msg.YPos <= FTabWidth) and (GetAsyncKeyState(VK_LBUTTON) <> 0) and (GetPanelFromTab(Msg.XPos, Msg.YPos) >= 0) then
      Msg.Result := 1
    else
      inherited;
  end;

  if Position = ppBottom then
  begin
    if (Msg.YPos > Height - FTabWidth) and (GetAsyncKeyState(VK_LBUTTON) <> 0) and (GetPanelFromTab(Msg.XPos, Msg.YPos) >= 0) then
      Msg.Result := 1
    else
      inherited;
  end;
end;

destructor TAdvToolPanelTab.Destroy;
begin
  while FRollInBusy do
  begin
  end;

  FPanels.Free;
  FUpDown.Free;
  FTabGlyph.Free;

  if Assigned(FPanelForm) and not FWMDestroy then
  begin
    SendMessage(FPanelForm.Handle, WM_DESTROY, 0,0);
    FreeAndNil(FPanelForm);
  end;

  FPersist.Free;
  FPanelGlyphs.Free;
  inherited;
end;

function TAdvToolPanelTab.GetTabSize: Integer;
var
  tf: TFont;
  lf: TLogFont;
  r: TRect;
  i, tw: Integer;
begin
  Result := 0;

  r := ClientRect;

  with Canvas do
  begin
    tf := TFont.Create;
    try
      if (Position = ppLeft) or (Position = ppRight) then
      begin
        FillChar(lf, SizeOf(lf), 0);
        tf.Assign(Font);
        GetObject(tf.Handle, SizeOf(Lf), @Lf);

        lf.lfEscapement := -900;
        lf.lfOrientation := 30;

        tf.Handle := CreateFontIndirect(Lf);
        Font.Assign(tf);

        r.Top := r.Top + 2;
      end
      else
      begin
        r.Left := r.Left + 2;
      end;

      Result := Result + 2;

      if csDesigning in ComponentState then
      begin
        for i := 1 to ControlCount do
        begin
          tw := TextWidth(TAdvToolPanel(Controls[i - 1]).Caption) + 4;

          if Assigned(Images) and (TAdvToolPanel(Controls[i - 1]).ImageIndex >= 0) then
            tw := tw + 6 + Images.Width;

          if (Position = ppLeft) or (Position = ppRight) then
          begin
            r.Top := r.Top + tw + 10;
          end
          else
          begin
            r.Left := r.Left + tw + 10;
          end;
          Result := Result + tw + 10;
        end;
      end
      else
      begin
        for i := 1 to Panels.Count do
        begin
          if Panels[i - 1].Panel.IsVisible then
          begin
            tw := TextWidth(Panels[i - 1].Caption) + 4;

            if Assigned(Images) and (Panels[i - 1].ImageIndex >= 0) then
              tw := tw + 6 + Images.Width;

            if (Position = ppLeft) or (Position = ppRight) then
            begin
              r.Top := r.Top + tw + 10;
            end
            else
            begin
              r.Left := r.Left + tw + 10;
            end;
            Result := Result + tw + 10;
          end;
        end;
      end;
    finally
      tf.Free;
    end;
  end;

end;


function TAdvToolPanelTab.GetPanelFromTab(x, y: Integer): Integer;
var
  tf: TFont;
  lf: TLogFont;
  r: TRect;
  i, tw: Integer;
begin
  Result := -1;

  r := ClientRect;

  if ((Position = ppLeft) or (Position = ppRight)) then
  begin
    if (GetTabSize > Height) and (y > Height - 10) then
      Exit;
  end
  else
  begin
    if (GetTabSize > Width) and (x > Width - 10) then
    begin
      Exit;
    end;
  end;

  if Position in [ppLeft,ppRight] then
  r.Top := r.Top + FPaintOffset;

  with Canvas do
  begin
    tf := TFont.Create;
    try
      if (Position = ppLeft) or (Position = ppRight) then
      begin
        FillChar(lf, SizeOf(lf), 0);
        tf.Assign(self.Font);
        GetObject(tf.Handle, SizeOf(Lf), @Lf);

        lf.lfEscapement := -900;
        lf.lfOrientation := 30;

        tf.Handle := CreateFontIndirect(Lf);
        Font.Assign(tf);

        r.Top := r.Top + 2;
      end
      else
      begin
        r.Left := r.Left + 2;
      end;

      if csDesigning in ComponentState then
      begin
        for i := 1 to ControlCount do
        begin
          tw := TextWidth(TAdvToolPanel(Controls[i - 1]).Caption) + 4;

          if Assigned(Images) and (TAdvToolPanel(Controls[i - 1]).ImageIndex >= 0) then
            tw := tw + 6 + Images.Width;

          if (Position = ppLeft) or (Position = ppRight) then
          begin
            if (y > r.Top) and (y < r.Top + tw + 10) then
            begin
              Result := i - 1;
              break;
            end;
            r.Top := r.Top + tw + 10;
          end
          else
          begin
            if (x > r.Left) and (x < r.Left + tw + 10) then
            begin
              Result := i - 1;
              break;
            end;
            r.Left := r.Left + tw + 10;
          end;
        end;
      end
      else
      begin
        for i := 1 to Panels.Count do
        begin
          if Panels[i - 1].Panel.IsVisible then
          begin
            tw := TextWidth(Panels[i - 1].Caption) + 4;

            if Assigned(Images) and (Panels[i - 1].ImageIndex >= 0) then
              tw := tw + 6 + Images.Width;

            if (Position = ppLeft) or (Position = ppRight) then
            begin
              if (y > r.Top) and (y < r.Top + tw + 10) then
              begin
                Result := i - 1;
                break;
              end;
              r.Top := r.Top + tw + 10;
            end
            else
            begin
              if (x > r.Left) and (x < r.Left + tw + 10) then
              begin
                Result := i - 1;
                break;
              end;
              r.Left := r.Left + tw + 10;
            end;
          end;
        end;
      end;
    finally
      tf.Free;
    end;
  end;

end;

procedure TAdvToolPanelTab.GetSiteInfo(Client: TControl;
  var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean);
begin
  inherited;
  CanDock := CanDock and (Client is TAdvToolPanel);
end;

procedure TAdvToolPanelTab.UpdatePanels(Index: Integer);
var
  j: Integer;
  LeftPos: Integer;
begin
  if (csDestroying in ComponentState) then
    Exit;

  if Position = ppLeft then
    LeftPos := FTabWidth
  else
    LeftPos := 0;

  if (PanelsInTab = 0) and not (csDesigning in ComponentState) and not (csLoading in ComponentState) then
  begin
    case Position of
      ppLeft: Width := 0;
      ppRight: Width := 1;
      ppTop: Height := 0;
      ppBottom: Height := 1;
    end;
  end;

  if (Width <= 1) and (Position in [ppLeft,ppRight]) and (PanelsInTab > 0) then
    Width := TabWidth;

  if (Height <= 1) and (Position in [ppTop,ppBottom]) and (PanelsInTab > 0) then
    Height := TabWidth;

  FDesignViewPanel := Index;

  if Index = -1 then
  begin
    if (csDesigning in ComponentState) then
    begin
      for j := 1 to ControlCount do
      begin
        if Controls[j - 1].Width > 0 then
        begin
          if ((Position = ppLeft) or (Position = ppRight)) then
          begin
            Controls[j - 1].Left := LeftPos;
            Controls[j - 1].Width := Width - FTabWidth;
            Controls[j - 1].Top := 0;
            Controls[j - 1].Height := self.Height;
            Controls[j - 1].Visible := True;
          end
          else {((Position = ppTop) or (Position = ppBottom))}
          begin
            Controls[j - 1].Left := LeftPos;
            Controls[j - 1].Width := Width;
            if (Position = ppTop) then
            begin
              Controls[j - 1].Top := FTabWidth;
            end
            else
            begin
              Controls[j - 1].Top := 0;
            end;
            Controls[j - 1].Height := self.Height - FTabWidth;
            Controls[j - 1].Visible := True;
          end;
        end;
      end;
    end;
  end
  else
    for j := 1 to ControlCount do
    begin
      if (Index = j - 1) then
      begin
        Controls[j - 1].Left := LeftPos;
        if ((Position = ppLeft) or (Position = ppRight)) then
        begin
          Controls[j - 1].Top := 0;
          Controls[j - 1].Width := Width - FTabWidth;
          Controls[j - 1].Height := Height;
        end
        else {((Position = ppTop) or (Position = ppBottom))}
        begin
          Controls[j - 1].Width := Width;
          Controls[j - 1].Height := Height - FTabWidth;
          if (Position = ppTop) then
            Controls[j - 1].Top := FTabWidth
          else
            Controls[j - 1].Top := 0;
        end;
        Controls[j - 1].Visible := True;
      end
      else
      begin
        Controls[j - 1].Visible := False;
        Controls[j - 1].Width := 0;
        Controls[j - 1].Height := 0;
      end;
    end;

  if Assigned(FPanelForm) then
  begin
    case Position of
    ppRight: if FPanelForm.Visible then
               FPanelForm.Height := Height;
    ppBottom: if FPanelForm.Visible then
                FPanelForm.Width := Width;
    end;
  end;

  Invalidate;
end;

procedure TAdvToolPanelTab.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  i: Integer;
  ts: Integer;

begin
  inherited;

  if FRollInBusy then
    Exit;

  i := GetPanelFromTab(x, y);

  FRollInBusy := true;

  if (i >= 0) then
  begin
    if (Button = mbLeft) and Assigned(FOnTabLeftClick) then
      FOnTabLeftClick(Self, i, FPanels[i].Panel);

    if (Button = mbRight) and Assigned(FOnTabRightClick) then
      FOnTabRightClick(Self, i, FPanels[i].Panel);

    if (csDesigning in ComponentState) then
    begin
      UpdatePanels(i);
      Invalidate;
    end
    else
      RollInOut(i);
  end
  else
  begin
    if FRollOutPanel <> -1 then
    begin
      if not FPanels[FRollOutpanel].Panel.AutoLock then
        IRollIn(FPanels[FRollOutpanel].Panel);
      //FRollOutpanel := -1;
    end;
  end;

  FRollInBusy := false;

  ts := GetTabSize;

  if (ts > Height) and (y > Height - 10) then
  begin
    if (x < 12) then
    begin
      if (FPaintOffset < 0) then
      begin
        FPaintOffset := FPaintOffset + 10;
        Invalidate;
      end;
    end;

    if (x > 12) and (x < 24) then
    begin
      if (Height - ts < FPaintOffset) then
      begin
        FPaintOffset := FPaintOffset - 10;
        Invalidate;
      end;
    end;
  end;

end;

procedure TAdvToolPanelTab.Notification(AComponent: TComponent;
  AOperation: TOperation);
var
  flg: Boolean;
begin
  flg := False;

  if (AOperation = opRemove) then
  begin
    flg := AComponent is TAdvToolPanel;
    if flg then
    begin
      if not (csDesigning in ComponentState) then
        RemovePanel(AComponent as TAdvToolPanel);
      Invalidate;
    end;
  end;

  if (AOperation = opRemove) and (AComponent = FImages) then
    FImages := nil;

  inherited;

  if flg then
  begin
    if ControlCount > 0 then
    begin
      UpdatePanels(0);
    end;
  end;
end;

procedure DrawGradient(Canvas: TCanvas; FromColor, ToColor: TColor; Steps: Integer; R: TRect; Direction: Boolean);
var
  diffr, startr, endr: Integer;
  diffg, startg, endg: Integer;
  diffb, startb, endb: Integer;
  rstepr, rstepg, rstepb, rstepw: Real;
  i, stepw: Word;

begin
  if Direction then
    r.Right := r.Right - 1
  else
    r.Bottom := r.Bottom - 1;

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

procedure DrawTransparentBitmap(hdc: THandle; hBitmap: THandle; xStart, yStart: Integer;
  width, height, offsx, offsy: Integer; cTransparentColor: TColor);
// The function draws a bitmap with a transparent background.
var
  cColor: TColor;
  bmAndBack, bmAndObject, bmAndMem, bmSave: THandle;
  bmBackOld, bmObjectOld, bmMemOld, bmSaveOld: THandle;
  hdcMem, hdcBack, hdcObject, hdcTemp, hdcSave: THandle;
  ptSize: TPoint;
begin
  hdcTemp := CreateCompatibleDC(hdc);
  SelectObject(hdcTemp, hBitmap);

  ptSize.x := width;
  ptSize.y := height;

  DPtoLP(hdcTemp, ptSize, 1);

  hdcBack := CreateCompatibleDC(hdc);
  hdcObject := CreateCompatibleDC(hdc);
  hdcMem := CreateCompatibleDC(hdc);
  hdcSave := CreateCompatibleDC(hdc);

  bmAndBack := CreateBitmap(ptSize.x, ptSize.y, 1, 1, nil);
  bmAndObject := CreateBitmap(ptSize.x, ptSize.y, 1, 1, nil);

  bmAndMem := CreateCompatibleBitmap(hdc, ptSize.x, ptSize.y);
  bmSave := CreateCompatibleBitmap(hdc, ptSize.x, ptSize.y);

  bmBackOld := SelectObject(hdcBack, bmAndBack);
  bmObjectOld := SelectObject(hdcObject, bmAndObject);
  bmMemOld := SelectObject(hdcMem, bmAndMem);
  bmSaveOld := SelectObject(hdcSave, bmSave);

  SetMapMode(hdcTemp, GetMapMode(hdc));

  BitBlt(hdcSave, 0, 0, ptSize.x, ptSize.y, hdcTemp, offsx, offsy, SRCCOPY);

  cColor := SetBkColor(hdcTemp, cTransparentColor);

  BitBlt(hdcObject, 0, 0, ptSize.x, ptSize.y, hdcTemp, offsx, offsy, SRCCOPY);

  SetBkColor(hdcTemp, cColor);

  BitBlt(hdcBack, 0, 0, ptSize.x, ptSize.y, hdcObject, 0, 0, NOTSRCCOPY);

  // take copy of existing canvas
  BitBlt(hdcMem, 0, 0, ptSize.x, ptSize.y, hdc, xStart, yStart, SRCCOPY);
  // and existing canvas with copy
  BitBlt(hdcMem, 0, 0, ptSize.x, ptSize.y, hdcObject, 0, 0, SRCAND);

  BitBlt(hdcTemp, offsx, offsy, ptSize.x, ptSize.y, hdcBack, 0, 0, SRCAND);
  BitBlt(hdcMem, 0, 0, ptSize.x, ptSize.y, hdcTemp, offsx, offsy, SRCPAINT);
  BitBlt(hdc, xStart, yStart, ptSize.x, ptSize.y, hdcMem, 0, 0, SRCCOPY);
  BitBlt(hdcTemp, 0, 0, ptSize.x, ptSize.y, hdcSave, 0, 0, SRCCOPY);

  DeleteObject(SelectObject(hdcBack, bmBackOld));
  DeleteObject(SelectObject(hdcObject, bmObjectOld));
  DeleteObject(SelectObject(hdcMem, bmMemOld));
  DeleteObject(SelectObject(hdcSave, bmSaveOld));

  DeleteDC(hdcMem);
  DeleteDC(hdcBack);

  DeleteDC(hdcObject);
  DeleteDC(hdcSave);
  DeleteDC(hdcTemp);
end;

procedure StretchTransparentBitmap(hdc: THandle; hBitmap: THandle; xStart, yStart: Integer;
  width, height, offsx, offsy, bmpw, bmph: Integer; cTransparentColor: TColor);
// The function draws a bitmap with a transparent background.
var
  cColor: TColor;
  bmAndBack, bmAndObject, bmAndMem, bmSave: THandle;
  bmBackOld, bmObjectOld, bmMemOld, bmSaveOld: THandle;
  hdcMem, hdcBack, hdcObject, hdcTemp, hdcSave: THandle;
  ptSize: TPoint;
begin
  hdcTemp := CreateCompatibleDC(hdc);
  SelectObject(hdcTemp, hBitmap);

  ptSize.x := width;
  ptSize.y := height;

  DPtoLP(hdcTemp, ptSize, 1);


  hdcBack := CreateCompatibleDC(hdc);
  hdcObject := CreateCompatibleDC(hdc);
  hdcMem := CreateCompatibleDC(hdc);
  hdcSave := CreateCompatibleDC(hdc);

  bmAndBack := CreateBitmap(ptSize.x, ptSize.y, 1, 1, nil);
  bmAndObject := CreateBitmap(ptSize.x, ptSize.y, 1, 1, nil);

  bmAndMem := CreateCompatibleBitmap(hdc, ptSize.x, ptSize.y);
  bmSave := CreateCompatibleBitmap(hdc, ptSize.x, ptSize.y);

  bmBackOld := SelectObject(hdcBack, bmAndBack);
  bmObjectOld := SelectObject(hdcObject, bmAndObject);
  bmMemOld := SelectObject(hdcMem, bmAndMem);
  bmSaveOld := SelectObject(hdcSave, bmSave);

  SetMapMode(hdcTemp, GetMapMode(hdc));

  StretchBlt(hdcSave, 0, 0, ptSize.x, ptSize.y, hdcTemp, offsx, offsy, bmpw, bmph, SRCCOPY);

  cColor := SetBkColor(hdcTemp, cTransparentColor);

  StretchBlt(hdcObject, 0, 0, ptSize.x, ptSize.y, hdcTemp, offsx, offsy, bmpw, bmph, SRCCOPY);

  SetBkColor(hdcTemp, cColor);

  BitBlt(hdcBack, 0, 0, ptSize.x, ptSize.y, hdcObject, 0, 0, NOTSRCCOPY);

  // take copy of existing canvas
  BitBlt(hdcMem, 0, 0, ptSize.x, ptSize.y, hdc, xStart, yStart, SRCCOPY);
  // and existing canvas with copy
  BitBlt(hdcMem, 0, 0, ptSize.x, ptSize.y, hdcObject, 0, 0, SRCAND);
  StretchBlt(hdcTemp, offsx, offsy, bmpw, bmph, hdcback, 0, 0, ptsize.x, ptsize.y, SRCAND);
  StretchBlt(hdcMem, 0, 0, ptSize.X, ptSize.Y, hdctemp, offsx, offsy, bmpw, bmph, SRCPAINT);
  BitBlt(hdc, xStart, yStart, ptSize.x, ptSize.y, hdcMem, 0, 0, SRCCOPY);
  StretchBlt(hdcTemp, offsx, offsy, bmpw, bmph, hdcSave, 0, 0, ptsize.x, ptsize.y, SRCCOPY);

  DeleteObject(SelectObject(hdcBack, bmBackOld));
  DeleteObject(SelectObject(hdcObject, bmObjectOld));
  DeleteObject(SelectObject(hdcMem, bmMemOld));
  DeleteObject(SelectObject(hdcSave, bmSaveOld));

  DeleteDC(hdcMem);
  DeleteDC(hdcBack);

  DeleteDC(hdcObject);
  DeleteDC(hdcSave);
  DeleteDC(hdcTemp);
end;

procedure BitmapStretch(bmp: tbitmap; canvas: tcanvas; x, y, height: integer);
var
  mid: integer;
  fillh: integer;
  c: TColor;
begin
  mid := bmp.height div 2;
  fillh := height - bmp.height;
  c := bmp.Canvas.Pixels[0, bmp.Height - 1];
  DrawTransparentBitmap(canvas.handle, bmp.handle, x, y, bmp.Width, bmp.Height div 2, 0, 0, c);
  StretchTransparentBitmap(canvas.handle, bmp.Handle, x, y + mid, bmp.width, fillh, 0, mid - 1, bmp.Width, 2, c);
  DrawTransparentBitmap(canvas.handle, bmp.handle, x, y + mid + fillh, bmp.width, bmp.Height div 2, 0, mid, c);
end;

procedure BitmapStretchInWidth(bmp: tbitmap; canvas: tcanvas; x, y, width: integer);
var
  mid: integer;
  fillw: integer;
  c: TColor;
begin
  mid := bmp.Width div 2;
  fillw := width - bmp.Width;
  c := bmp.Canvas.Pixels[bmp.Width - 1, 0];
  DrawTransparentBitmap(canvas.handle, bmp.handle, x, y, bmp.Width div 2, bmp.Height, 0, 0, c);
  StretchTransparentBitmap(canvas.handle, bmp.Handle, x + mid, y, fillw, bmp.height, mid - 1, 0, 2, bmp.height, c);
  DrawTransparentBitmap(canvas.handle, bmp.handle, x + mid + fillw, y, bmp.width div 2, bmp.Height, mid, 0, c);
end;


procedure TAdvToolPanelTab.Paint;
var
  tf: TFont;
  lf: TLogFont;
  r: TRect;
  i, ih, iw, ihu: Integer;
  tw, th, dx, dy: Integer;
  clr, clrto: TColor;
  bmp, tbmp: TBitmap;

begin
  bmp := TBitmap.Create;
  bmp.Width := Width;
  bmp.Height := Height;

  tbmp := TBitmap.Create;

  if not FTabGlyph.Empty then
  begin
    tbmp.Width := FTabGlyph.Width;
    tbmp.Height := FTabGlyph.Height;
  end;

  if Assigned(Images) then
  begin
    ih := Images.Height;
    iw := Images.Width;
  end
  else
  begin
    ih := 0;
    iw := 0;
  end;

  dx := 0;

  r := ClientRect;

  with bmp.Canvas do
  begin
    Font.Assign(self.Font);

    if (csDesigning in ComponentState) then
    begin
      Brush.Color := Color;
      Pen.Color := Color;
      Rectangle(r.Left, r.Top, r.Right, r.Bottom);

      if ColorTo <> clNone then
      begin
        case Position of
          //ppLeft, ppRight: DrawGradient(bmp.Canvas, Color, ColorTo, 16, Rect(r.Left, r.Top, r.Left + FTabWidth, r.Bottom), True);
          //ppTop, ppBottom: DrawGradient(bmp.Canvas, Color, ColorTo, 16, Rect(r.Left, r.Top, r.Right, r.Top + FTabWidth), False);
          ppLeft: DrawGradient(bmp.Canvas, Color, ColorTo, 16, Rect(r.Left, r.Top, r.Left + FTabWidth, r.Bottom), True);
          ppRight: DrawGradient(bmp.Canvas, Color, ColorTo, 16, Rect(r.Right - FTabWidth, r.Top, r.Right, r.Bottom), True);
          ppTop: DrawGradient(bmp.Canvas, Color, ColorTo, 16, Rect(r.Left, r.Top, r.Right, r.Top + FTabWidth), False);
          ppBottom: DrawGradient(bmp.Canvas, Color, ColorTo, 16, Rect(r.Left, r.Bottom - FTabWidth, r.Right, r.Bottom), False);
        end;
      end;

      if ControlCount = 0 then
      begin
        SetBkMode(bmp.Canvas.Handle, TRANSPARENT);
        TextOut(r.Left + 30, r.Top + 30, 'Right-click and choose ');
        TextOut(r.Left + 30, r.Top + 50, '"New Panel" to add a panel');

        Font.Style := Font.Style + [fsItalic];
        TextOut(r.Left + 30, r.Top + 90,  'If no such right-click menu');
        TextOut(r.Left + 30, r.Top + 105, 'option appears, please install');
        TextOut(r.Left + 30, r.Top + 120, 'designtime package!');
        Font.Style := Font.Style - [fsItalic];
      end;

    end
    else
    begin
      if ColorTo <> clNone then
      begin
        case Position of
          ppLeft, ppRight: DrawGradient(bmp.Canvas, Color, ColorTo, 16, r, True);
          ppTop, ppBottom: DrawGradient(bmp.Canvas, Color, ColorTo, 16, r, False);
        end;
      end
      else
      begin
        Brush.Color := Color;
        Pen.Color := Color;
        Rectangle(r.Left, r.Top, r.Right, r.Bottom);
      end;
    end;

    if Position in [ppLeft,ppRight] then
    r.Top := r.Top + FPaintOffset;

    // Make sure to use a truetype font!
    //Font.Name := 'Tahoma';

    tf := TFont.Create;

    try
      if (Position = ppLeft) or (Position = ppRight) then
      begin
        FillChar(lf, SizeOf(lf), 0);
        tf.Assign(Font);
        GetObject(tf.Handle, SizeOf(Lf), @Lf);

        if FTextDirection = tdTopToBottom then
          lf.lfEscapement := -900
        else
          lf.lfEscapement := +900;

        lf.lfOrientation := +30;

        tf.Handle := CreateFontIndirect(Lf);
        Font.Assign(tf);

        r.Top := r.Top + 2;
      end
      else
      begin
        r.Left := r.Left + 2;
      end;

      th := TextWidth('gh');

      if csDesigning in ComponentState then
      begin
        for i := 1 to ControlCount do
        begin
          if Controls[i - 1] is TAdvToolPanel then
          begin
            tw := TextWidth(TAdvToolPanel(Controls[i - 1]).Caption) + 4;

            if Assigned(Images) and (TAdvToolPanel(Controls[i - 1]).ImageIndex >= 0) then
              tw := tw + 6 + Images.Width;

            if (FHoverPanel = i - 1) and (TabHoverColor <> clNone) then
            begin
              clr := TabHoverColor;
              clrto := TabHoverColorTo;
            end
            else
            begin
              clr := TabColor;
              clrto := TabColorTo;
            end;

            Brush.Color := clr;
            Pen.Color := TabBorderColor;

            Pen.Width := 1;

            dy := r.Top + 4;
            if Position = ppLeft then
            begin
              if not FTabGlyph.Empty then
              begin
                tbmp.Canvas.Draw(0, 0, FTabGlyph);
                BitmapStretch(tbmp, bmp.Canvas, r.Left, r.Top, tw + ih + 8);
              end
              else
              begin
                if FTabColorTo = clNone then
                  Rectangle(r.Left, r.Top, r.Left + FTabWidth - 1, r.Top + tw + 4)
                else
                begin
                  DrawGradient(bmp.Canvas, clr, clrto, 16, Rect(r.Left, r.Top, r.Left + FTabWidth - 1, r.Top + tw + 4), True);
                  Brush.Style := bsClear;
                  Pen.Color := TabBorderColor;
                  Rectangle(r.Left, r.Top, r.Left + FTabWidth - 1, r.Top + tw + 4)
                end;
              end;

              dx := r.Left + TabWidth - ((FTabWidth - th) div 2);
            end
            else if Position = ppRight then
            begin
              if not FTabGlyph.Empty then
              begin
                tbmp.Canvas.Draw(0, 0, FTabGlyph);
                BitmapStretch(tbmp, bmp.Canvas, r.right - FTabWidth, r.Top, tw + ih + 8);
              end
              else
              begin
                if FTabColorTo = clNone then
                  Rectangle(r.Right - FTabWidth + 4, r.Top, r.Right, r.Top + tw + 4)
                else
                begin
                  DrawGradient(bmp.Canvas, clr, clrto, 16, Rect(r.Right - FTabWidth + 4, r.Top, r.Right, r.Top + tw + 4), True);
                  Brush.Style := bsClear;
                  Pen.Color := TabBorderColor;
                  //Rectangle(r.Left, r.Top, r.Left + FTabWidth - 1, r.Top + tw + 4)
                  Rectangle(r.Right - FTabWidth + 2, r.Top, r.Right, r.Top + tw + 4);
                end;
              end;
              dx := r.Right + 4 - (FTabWidth - th) div 2;
            end
            else if Position = ppTop then
            begin
              if not FTabGlyph.Empty then
              begin
                tbmp.Canvas.Draw(0, 0, FTabGlyph);
                BitmapStretch(tbmp, bmp.Canvas, r.right - FTabWidth, r.Top, tw + ih + 8);
              end
              else
              begin
                if FTabColorTo = clNone then
                  Rectangle(r.Left, r.Top, r.Left + tw + 4, r.Top + FTabWidth - 1)
                else
                begin
                  DrawGradient(bmp.Canvas, clr, clrto, 16, Rect(r.Left, r.Top, r.Left + tw + 4, r.Bottom - 1), True);
                  Brush.Style := bsClear;
                  Pen.Color := TabBorderColor;
                  Rectangle(r.Left, r.Top, r.Left + tw + 4, r.Bottom - 1);
                end;
              end;
              dx := r.Left + 4;
            end
            else if Position = ppBottom then
            begin
              if not FTabGlyph.Empty then
              begin
                tbmp.Canvas.Draw(0, 0, FTabGlyph);
                BitmapStretch(tbmp, bmp.Canvas, r.right - FTabWidth, r.Top, tw + ih + 8);
              end
              else
              begin
                if FTabColorTo = clNone then
                  Rectangle(r.Left, r.Bottom - FTabWidth + 1, r.Left + tw + 4, r.Bottom - 1)
                else
                begin
                  DrawGradient(bmp.Canvas, clr, clrto, 16, Rect(r.Left, r.Top + 1, r.Left + tw + 4, r.Bottom), True);
                  Brush.Style := bsClear;
                  Pen.Color := TabBorderColor;
                  Rectangle(r.Left, r.Top + 1, r.Left + tw + 4, r.Bottom);
                end;
              end;
              dx := r.Left + 4;
              dy := r.Bottom - FTabWidth + 4;
            end;

            if Assigned(Images) and (TAdvToolPanel(Controls[i - 1]).ImageIndex >= 0) then
            begin
              Images.DrawingStyle := dsTransparent;
              if ((Position = ppLeft) or (Position = ppRight)) then
              begin
                if Position = ppLeft then
                  Images.Draw(bmp.Canvas, r.Left + (FTabWidth - Images.Width) div 2, r.Top + 6, TAdvToolPanel(Controls[i - 1]).ImageIndex)
                else {if Position = ppRight then}
                  Images.Draw(bmp.Canvas, r.Right + 1 - Images.Width - ((FTabWidth - Images.Width) div 2), r.Top + 6, TAdvToolPanel(Controls[i - 1]).ImageIndex);
                dy := dy + ih + 4;
              end
              else {((Position = ppTop) or (Position = ppBottom))}
              begin
                if Position = ppTop then
                  Images.Draw(bmp.Canvas, r.Left + 6, r.Top + (FTabWidth - Images.Width) div 2, TAdvToolPanel(Controls[i - 1]).ImageIndex)
                else {if Position = ppBottom then}
                  Images.Draw(bmp.Canvas, r.Left + 6, r.Bottom + 1 - Images.Width - ((FTabWidth - Images.Width) div 2), TAdvToolPanel(Controls[i - 1]).ImageIndex);
                dx := dx + iw + 4;
              end;
            end;

            bmp.Canvas.Brush.Style := bsClear;

            if FTextDirection = tdBottomToTop then
            begin
              dx := dx + 2 - TabWidth + (TabWidth - th) div 2;
              dy := dy + tw - 4;
            end;

            TextOut(dx, dy, TAdvToolPanel(Controls[i - 1]).Caption);

            if (Position = ppLeft) or (Position = ppRight) then
              r.Top := r.Top + tw + 10
            else {(Position = ppTop) or (Position = ppBottom)}
              r.Left := r.Left + tw + 10;

          end;
        end;
      end
      else
      begin
        for i := 1 to Panels.Count do
        begin
          if Panels[i - 1].Panel.IsVisible then
          begin
            tw := TextWidth(Panels[i - 1].Caption) + 4;

            if Assigned(Images) and (Panels[i - 1].ImageIndex >= 0) then
              tw := tw + 6 + Images.Width;

            if (FHoverPanel = i - 1) and (TabHoverColor <> clNone) then
            begin
              clr := TabHoverColor;
              clrto := TabHoverColorTo;
            end
            else
            begin
              clr := TabColor;
              clrto := TabColorTo;
            end;

            Brush.Color := clr;
            Pen.Color := TabBorderColor;
            Pen.Width := 1;

            dy := r.Top + 4;
            if Position = ppLeft then
            begin
              if not FTabGlyph.Empty then
              begin
                tbmp.Canvas.Draw(0, 0, FTabGlyph);
                BitmapStretch(tbmp, bmp.Canvas, r.Left, r.Top, tw + ih + 8);
              end
              else
              begin
                if FTabColorTo = clNone then
                  Rectangle(r.Left, r.Top, r.Left + FTabWidth - 1, r.Top + tw + 4)
                else
                begin
                  DrawGradient(bmp.Canvas, clr, clrto, 16, Rect(r.Left, r.Top, r.Left + FTabWidth - 1, r.Top + tw + 4), True);
                  Brush.Style := bsClear;
                  Pen.Color := TabBorderColor;
                  Rectangle(r.Left, r.Top, r.Left + FTabWidth - 1, r.Top + tw + 4);
                end;
              end;

              dx := r.Left + TabWidth - ((FTabWidth - th) div 2);
            end
            else if Position = ppRight then
            begin
              if not FTabGlyph.Empty then
              begin
                tbmp.Canvas.Draw(0, 0, FTabGlyph);
                BitmapStretch(tbmp, bmp.Canvas, r.right - FTabWidth, r.Top, tw + ih + 8);
              end
              else
              begin
                if FTabColorTo = clNone then
                  Rectangle(r.Right - FTabWidth + 2, r.Top, r.Right, r.Top + tw + 4)
                else
                begin
                  DrawGradient(bmp.Canvas, clr, clrto, 16, Rect(r.Right - FTabWidth + 2, r.Top, r.Right, r.Top + tw + 4), True);
                  Brush.Style := bsClear;
                  Pen.Color := TabBorderColor;
                  Rectangle(r.Right - FTabWidth + 2, r.Top, r.Right, r.Top + tw + 4);
                end;
              end;

              dx := r.Right + 4 - (FTabWidth - th) div 2;
            end
            else if Position = ppTop then
            begin
              if not FTabGlyph.Empty then
              begin
                tbmp.Canvas.Draw(0, 0, FTabGlyph);
                BitmapStretchInWidth(tbmp, bmp.Canvas, r.Left, r.Top, tw + iw + 8);
                //BitmapStretch(tbmp,bmp.Canvas,r.right - FTabWidth,r.Top,tw + ih + 8);
              end
              else
              begin
                if FTabColorTo = clNone then
                  Rectangle(r.Left, r.Top, r.Left + tw + 4, r.Bottom - 1)
                else
                begin
                  DrawGradient(bmp.Canvas, clr, clrto, 16, Rect(r.Left, r.Top, r.Left + tw + 4, r.Bottom - 1), True);
                  Brush.Style := bsClear;
                  Pen.Color := TabBorderColor;
                  Rectangle(r.Left, r.Top, r.Left + tw + 4, r.Bottom - 1);
                end;
              end;
              dx := r.Left + 4;
            end
            else if Position = ppBottom then
            begin
              if not FTabGlyph.Empty then
              begin
                tbmp.Canvas.Draw(0, 0, FTabGlyph);
                BitmapStretchInWidth(tbmp, bmp.Canvas, r.Left, r.Bottom - FTabWidth, tw + iw + 8);
              end
              else
              begin
                if FTabColorTo = clNone then
                  Rectangle(r.Left, r.Top + 1, r.Left + tw + 4, r.Bottom)
                else
                begin
                  DrawGradient(bmp.Canvas, clr, clrto, 16, Rect(r.Left, r.Top + 1, r.Left + tw + 4, r.Bottom), True);
                  Brush.Style := bsClear;
                  Pen.Color := TabBorderColor;
                  Rectangle(r.Left, r.Top + 1, r.Left + tw + 4, r.Bottom);
                end;
              end;
              dx := r.Left + 4;
            end;

            ihu := 0;

            if Assigned(Images) and (Panels[i - 1].ImageIndex >= 0) then
            begin
              Images.DrawingStyle := dsTransparent;

              if ((Position = ppLeft) or (Position = ppRight)) then
              begin
                if Position = ppLeft then
                  Images.Draw(bmp.Canvas, r.Left + (FTabWidth - Images.Width) div 2, r.Top + 6, Panels[i - 1].ImageIndex)
                else {if Position = ppRight then}
                  Images.Draw(bmp.Canvas, r.Right + 1 - Images.Width - ((FTabWidth - Images.Width) div 2), r.Top + 6, Panels[i - 1].ImageIndex);
                dy := dy + ih + 4;
                ihu := ih + 4;
              end
              else {((Position = ppLeft) or (Position = ppRight))}
              begin
                if Position = ppTop then
                  Images.Draw(bmp.Canvas, r.Left + 6, r.Top + (FTabWidth - Images.Width) div 2, Panels[i - 1].ImageIndex)
                else {if Position = ppBottom then}
                  Images.Draw(bmp.Canvas, r.Left + 6, r.Bottom + 1 - Images.Width - ((FTabWidth - Images.Width) div 2), Panels[i - 1].ImageIndex);
                dx := dx + iw + 4;
              end;
            end;

            {
            dr := rect(-20,20,200,200);
            DrawTextEx(Canvas.Handle,'test',4,dr,DT_RIGHT or DT_SINGLELINE,nil);
            }
            TextFlags := TextFlags and not ETO_OPAQUE;

            if FTextDirection = tdBottomToTop then
            begin
              dx := dx + 2 - TabWidth + (TabWidth - th) div 2;
              dy := dy + tw - ihu - 4;
            end;

            Brush.Style := bsClear;
            TextOut(dx, dy, Panels[i - 1].Caption);
            if (Position = ppLeft) or (Position = ppRight) then
              r.Top := r.Top + tw + 10
            else
              r.Left := r.Left + tw + 10;
          end;
        end;
      end;
    finally
      tf.Free;
    end;

    if (Position = ppLeft) or (Position = ppRight) then
    begin
      if GetTabSize > Height then
      begin
        Brush.Color := Color;
        Pen.Color := Color;
        Rectangle(r.Left, r.Bottom - 9, r.Right, r.Bottom);
        FUpDown.Transparent := true;
        Draw(r.Left + 1, r.Bottom - 9, FUpDown);
      end;
    end;
  end;

  Canvas.Draw(0, 0, bmp);
  bmp.Free;
  tbmp.Free;
end;


procedure TAdvToolPanelTab.Loaded;
begin
  inherited;

  if not (csDesigning in ComponentState) then
  begin
    // start hooking panels to parent

    while ControlCount > 0 do
    begin
      with Panels.Add do
      begin
        Caption := TAdvToolPanel(Controls[0]).Caption;
        ImageIndex := TAdvToolPanel(Controls[0]).ImageIndex;
        Panel := TAdvToolPanel(Controls[0]);
        OpenWidth := TAdvToolPanel(Controls[0]).OpenWidth;
      end;

      Controls[0].Visible := False;
      TAdvToolPanel(Controls[0]).Tab := self;
      Controls[0].Parent := self.Parent;
    end;

    if ((Position = ppLeft) or (Position = ppRight)) then
    begin
      Width := FTabWidth;
    end
    else {((Position = ppTop) or (Position = ppBottom))}
    begin
      Height := FTabWidth;
    end;
    {
    case AutoOpenCloseSpeed of
      aocVerySlow: FTimerID := SetTimer(Handle, 500, 1500, nil);
      aocSlow: FTimerID := SetTimer(Handle, 500, 500, nil);
      aocMedium: FTimerID := SetTimer(Handle, 500, 200, nil);
      aocFast: FTimerID := SetTimer(Handle, 500, 75, nil);
      aocVeryFast: FTimerID := SetTimer(Handle, 500, 40, nil);
    end;
    }
    SetZOrder(true);
  end;

//  if Persist.AutoLoad and not (csDesigning in ComponentState) then
//    RestoreState;

  if AutoThemeAdapt and not (csDesigning in ComponentState) then
    ThemeAdapt;
end;

procedure TAdvToolPanelTab.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  i, tp: Integer;

begin
  inherited;

  if (csDesigning in ComponentState) then
    Exit;

  FMouseInControl:= true;

  tp := GetPanelFromTab(x, y);

  if (tp <> FMousePanel) then
  begin
    if (FMousePanel >= 0) then
      if Assigned(FOnTabLeave) then
        FOnTabLeave(Self, FMousePanel, Panels[FMousePanel].Panel);

    FMousePanel := tp;
    if (FMousePanel >= 0) then
      if Assigned(FOnTabEnter) then
        FOnTabEnter(Self, FMousePanel, Panels[FMousePanel].Panel);
  end;

  if (FHoverPanel <> tp) and (FTabHoverColor <> clNone) then
  begin
    FHoverPanel := tp;
    Invalidate;
  end;

  if not AutoOpenOnMouseEnter then
    Exit;

  if (Position in [ppLeft, ppRight]) then
  begin
    if Position = ppRight then
      Application.BringToFront;

    if (X > Width - FTabWidth) and not FRollInOut and
      not ((FLastMousePos.X = X) and (FLastMousePos.Y = Y)) then
    begin
      i := GetPanelFromTab(x, y);
      if (i <> FRollInCandidate) then
      begin
        FRollInCandidate := i;
        FTimerRollIn := 0;
      end;
    end
    else
    begin
      FRollInCandidate := -1;
    end;
  end;

  if (Position in [ppTop, ppBottom]) then
  begin
    if Position = ppBottom then
      Application.BringToFront;

    if (Y > Height - FTabWidth) and not FRollInOut and
      not ((FLastMousePos.X = X) and (FLastMousePos.Y = Y)) then
    begin
      i := GetPanelFromTab(x, y);
      if (i <> FRollInCandidate) then
      begin
        FRollInCandidate := i;
        FTimerRollIn := 0;
      end;
    end
    else
      FRollInCandidate := -1;
  end;

  FLastMousePos := Point(X, Y);
end;

procedure TAdvToolPanelTab.IRollIn(APanel: TAdvToolPanel);
var
  t: Cardinal;
  delay: Cardinal;
  actWin: THandle;
begin
  FRollInOutActive := true;
  if Assigned(FOnTabSlideIn) then
    FOnTabSlideIn(Self, PanelIndex(APanel), APanel);

  case FSlideSpeed of
    ssSlow: delay := 18;
    ssMedium: delay := 9;
    ssFast: delay := 3;
    ssInstant: delay := 0;
  else
    delay := 9;
  end;

  actWin := GetActiveWindow;

  SetZOrder(True);
  case Position of
    ppLeft:
      begin
        if delay = 0 then
        begin
          APanel.Left := -APanel.Width;
        end
        else
        while APanel.Left > -APanel.Width do
        begin
          if delay <> 0 then
          begin
            t := GetTickCount;
            while (GetTickCount - t) < delay do
              if ((GetTickCount - t) mod 3) = 0 then
                Application.ProcessMessages;
          end;
          APanel.Left := APanel.Left - 15;
        end;
      end;
    ppRight:
      begin
        //if (FPanelForm.Width > 0) and (Panels[PanelIndex(APanel)].State = psOpened) then
        //  APanel.OpenWidth := FPanelForm.Width;

{$IFDEF TMSDEBUG}
        outputdebugstring(pchar('rollin:' + inttostr(apanel.OpenWidth)));
{$ENDIF}
        if delay = 0 then
        begin
          FPanelForm.Width := 0;
          FPanelForm.Left := Left;
        end
        else
        begin
          while (FPanelForm.Width > 0) do
          begin
            if delay <> 0 then
            begin
              t := GetTickCount;
              while (GetTickCount - t) < delay do
                if ((GetTickCount - t) mod 3) = 0 then
                  Application.ProcessMessages;
            end
            else
            begin
              FPanelForm.Width := 0;
              FPanelForm.Left := Left;
            end;

            if FPanelForm.Width > 15 then
            begin
              FPanelForm.Width := FPanelForm.Width - 15;
              FPanelForm.Left := FPanelForm.Left + 15;
            end
            else
            begin
              FPanelForm.Width := 0;
              FPanelForm.Left := Left;
            end;
          end;
        end;
        APanel.Visible := False;
        if actWin = self.Parent.Handle then
          SetActiveWindow(self.Parent.Handle);
      end;
    ppTop:
      begin
        if delay = 0 then
        begin
          APanel.Top := -APanel.Height;
        end
        else
        begin

          while APanel.Top > -APanel.Height do
          begin
            t := GetTickCount;
            while (GetTickCount - t) < delay do
              if ((GetTickCount - t) mod 3) = 0 then
                Application.ProcessMessages;

            APanel.Top := APanel.Top - 15;
          end;
        end;
      end;
    ppBottom:
      begin
        if FPanelForm.Height > 0 then
          APanel.OpenWidth := FPanelForm.Height;

{$IFDEF TMSDEBUG}
        outputdebugstring(pchar('rollin:' + inttostr(apanel.OpenWidth)));
{$ENDIF}

        if delay = 0 then
        begin
          FPanelForm.Height := 0;
          FPanelForm.Top := Top;
        end
        else
        begin
          while FPanelForm.Height > 0 do
          begin
            t := GetTickCount;
            while (GetTickCount - t) < delay do
              if ((GetTickCount - t) mod 3) = 0 then
                Application.ProcessMessages;

            if FPanelForm.Height > 15 then
            begin
              FPanelForm.Height := FPanelForm.Height - 15;
              FPanelForm.Top := FPanelForm.Top + 15;
            end
            else
            begin
              FPanelForm.Height := 0;
              FPanelForm.Top := Top;
            end;
          end;
        end;
        APanel.Visible := False;

        if actWin = self.Parent.Handle then
          SetActiveWindow(self.Parent.Handle);
      end;
  end;


  if (csDestroying in ComponentState) then
    Exit;

  Panels[PanelIndex(APanel)].State := psClosed;

  SendToBack;

  FRollOutpanel := -1;

  if Assigned(FOnTabSlideInDone) then
    FOnTabSlideInDone(Self, PanelIndex(APanel), APanel);

  FRollInOutActive := false;
end;

procedure TAdvToolPanelTab.IRollOut(APanel: TAdvToolPanel);
var
  t: Cardinal;
  delay: Cardinal;
  tw,fw: Integer;
  actWin: THandle;
  frm: TCustomForm;
begin
  FRollInOutActive := true;

  if Assigned(FOnTabSlideOut) then
    FOnTabSlideOut(Self, PanelIndex(APanel), APanel);

  case FSlideSpeed of
    ssSlow: delay := 18;
    ssMedium: delay := 9;
    ssFast: delay := 3;
    ssInstant: delay := 0;
  else
    delay := 9;
  end;

  if PanelsInTab = 0 then
    tw := 0
  else
    tw := FTabWidth;

  actWin := GetActiveWindow;

  case Position of
    ppLeft:
      begin
        if delay = 0 then
        begin
          APanel.Left := FTabWidth;
        end
        else
          APanel.Height := APanel.Height - 1;
          APanel.Height := APanel.Height + 1;
          while (APanel.Left < tw) do
          begin

            if delay <> 0 then
            begin
              t := GetTickCount;

              while (GetTickCount - t) < delay do
                if ((GetTickCount - t) mod 3) = 0 then
                  Application.ProcessMessages;

            end;

            if APanel.Left + 15 < FTabWidth then
              APanel.Left := APanel.Left + 15
            else
              APanel.Left := FTabWidth;
          end;
      end;
    ppRight:
      begin
{$IFDEF TMSDEBUG}
        outputdebugstring(pchar('rollout:' + inttostr(apanel.OpenWidth) + ':' + inttostr(fpanelform.Width)+':'+inttostr(fpanelform.Left)));
{$ENDIF}

        APanel.Align := alNone;
        APanel.Width := APanel.OpenWidth;
        APanel.Anchors := [];

        APanel.Left := 2;
        APanel.Top := 0;
        APanel.Height := Height;

        frm := GetParentForm(self);
        fw := frm.ClientWidth - Width - 2* GetSystemMetrics(SM_CXBORDER);

        while (FPanelForm.Width < APanel.OpenWidth) and (FPanelForm.Width < fw) do
        begin
          if delay = 0 then
          begin
            FPanelForm.Left := FPanelForm.Left + FPanelForm.Width - APanel.OpenWidth ;
            FPanelForm.Width := APanel.OpenWidth;
            APanel.Left := 2;
            APanel.Width := FPanelForm.Width - 2; // size panel with form
            APanel.Anchors := [akLeft, akTop, akRight, akBottom];
          end
          else
          begin

            if delay <> 0 then
            begin
              t := GetTickCount;
              while (GetTickCount - t) < delay do
                if ((GetTickCount - t) mod 3) = 0 then
                  Application.ProcessMessages;
            end;

  //          FPanelForm.Perform(WM_SetRedraw,0,0);

            if FPanelForm.Width + 15 < APanel.OpenWidth then
            begin
              FPanelForm.Left := FPanelForm.Left - 15;
              FPanelForm.Width := FPanelForm.Width + 15;
            end
            else
            begin
              FPanelForm.Left := FPanelForm.Left - (APanel.OpenWidth - APanel.Width) + 2;
              FPanelForm.Width := APanel.OpenWidth;
            end;

          // final panel positioning
            APanel.Left := 2;
            APanel.Width := FPanelForm.Width - 2; // size panel with form
            APanel.Anchors := [akLeft, akTop, akRight, akBottom];
            if delay = 0 then
            begin
              FPanelForm.Left := FPanelForm.Left - (APanel.OpenWidth - APanel.Width) + 2;
              FPanelForm.Width := APanel.OpenWidth;
            end;
          end;

//          FPanelForm.Perform(WM_SetRedraw,1,0);
//          APanel.Invalidate;
//          APanel.Repaint;
//          FPanelForm.Invalidate;
//          FPanelForm.Repaint;
        end;

        APanel.Width := APanel.Width + 1;
        APanel.Width := APanel.Width - 1;

        if actWin = self.Parent.Handle then
          SetActiveWindow(self.Parent.Handle);
      end;
    ppTop:
      begin
        if delay = 0 then
        begin
          APanel.Top := tw;
        end
        else
        begin
          while APanel.Top < tw do
          begin
            t := GetTickCount;
            while (GetTickCount - t) < delay do
              if ((GetTickCount - t) mod 3) = 0 then
                Application.ProcessMessages;

            if APanel.Top + 15 < FTabWidth then
              APanel.Top := APanel.Top + 15
            else
              APanel.Top := FTabWidth;
          end;
        end;
      end;
    ppBottom:
      begin
        APanel.Align := alNone;
        APanel.Height := APanel.OpenWidth;
        APanel.Anchors := [];

        APanel.Left := 0;
        APanel.Top := 2;
        APanel.Width := Width;

        if delay = 0 then
        begin
          FPanelForm.Height := APanel.OpenWidth;
          FPanelForm.Top := FPanelForm.Top - APanel.OpenWidth;
          APanel.Top := 2;
          APanel.Height := FPanelForm.Height - 2; // size panel with form
          APanel.Anchors := [akLeft, akTop, akRight, akBottom];
        end
        else
        begin
          frm := GetParentForm(self);
          fw := frm.ClientHeight - Height - 2* GetSystemMetrics(SM_CYBORDER);

          while (FPanelForm.Height < APanel.OpenWidth) and (FPanelForm.Height < fw) do
          begin
            t := GetTickCount;

            while (GetTickCount - t) < delay do
              if ((GetTickCount - t) mod 3) = 0 then
                Application.ProcessMessages;

            if FPanelForm.Height + 15 < APanel.OpenWidth then
            begin
              FPanelForm.Top := FPanelForm.Top - 15;
              FPanelForm.Height := FPanelForm.Height + 15;
            end
            else
            begin
              FPanelForm.Top := FPanelForm.Top - (APanel.OpenWidth - APanel.Height) + 2;
              FPanelForm.Height := APanel.OpenWidth;
            end;

          // final panel positioning
            APanel.Top := 2;
            APanel.Height := FPanelForm.Height - 2; // size panel with form
            APanel.Anchors := [akLeft, akTop, akRight, akBottom];
          end;
        end;
        if actWin = self.Parent.Handle then
          SetActiveWindow(self.Parent.Handle);
      end;
  end;

  if (csDestroying in ComponentState) then
    Exit;

  Panels[PanelIndex(APanel)].State := psOpened;

  if APanel.AutoLock then
  begin
    APanel.Locked := true;
    LockChange(APanel);
    FRollInCandidate := -1;
    FRollOutPanel := -1;
  end;

  if Assigned(FOnTabSlideOutDone) then
    FOnTabSlideOutDone(Self, PanelIndex(APanel), APanel);

  FRollInOutActive := false;

end;


procedure TAdvToolPanelTab.WMTimer(var Message: TWMTimer);
var
  pt, ptp: TPoint;
  InPanel: Boolean;
  OutTab: Boolean;
  i: Integer;

begin
  inherited;

  GetCursorPos(pt);
  pt := ScreenToClient(pt);

  if (FRollInCandidate >= 0) and FMouseInControl and (Panels.Count > 0) then
  begin
    if (((Position = ppLeft) or (Position = ppRight)) and (pt.X > Width - FTabWidth) and (pt.X < Width))
      or (((Position = ppTop) or (Position = ppBottom)) and (pt.y > Height - FTabWidth) and (pt.y < Height)) then
    begin
      if (FTimerRollin >= 2) and not FRollInOut then
      begin
        i := GetPanelFromTab(pt.X, pt.Y);

        if i = FRollInCandidate then
        begin
          FRollInOut := True;
          FRollInCandidate := -1;
          RollInOut(i);
          FRollInOut := False;
        end
        else
          FRollInCandidate := -1;
      end
      else
      begin
        inc(FTimerRollin);
      end;
    end;
  end;

  if not FTimerEnable then
    Exit;

  Inc(FTimerCount);

  if (FRollOutPanel <> -1) and (Panels.Count > 0) then
  begin
    GetCursorPos(ptp);
    ptp := Panels[FRollOutPanel].Panel.ScreenToClient(ptp);
    InPanel := PtInRect(Panels[FRollOutPanel].Panel.ClientRect, ptp);

    case Position of //TODO:OK v1.2
      ppLeft, ppRight:
        if not InPanel then
          InPanel := (FRollOutPanel = GetPanelFromTab(0, pt.Y)) and (pt.X < Width) and (pt.X >= 0);
      ppTop, ppBottom: //TODO:OK v1.2
        if not InPanel then
          InPanel := (FRollOutPanel = GetPanelFromTab(pt.x, 0)) and (pt.y < Height) and (pt.y >= 0);
    end;

    if not InPanel then
      InPanel := Panels[FRollOutPanel].Panel.HasFocusControl;
  end
  else
    InPanel := False;

  OutTab := False;

  if not InPanel then
  begin
    case Position of //TODO:OK v1.2
      ppLeft, ppRight:
        OutTab := (pt.X < FTabWidth) and (GetPanelFromTab(0, pt.y) = -1);
      ppTop, ppBottom:
        OutTab := (pt.Y < FTabWidth) and (GetPanelFromTab(pt.x, 0) = -1);
    end;
  end;

  if (FRollOutPanel <> -1) and (OutTab or not InPanel) and (Panels.Count > 0) then
  begin

    if (FTimerCount = 10) and not FRollinout then
    begin
      if Position = ppLeft then
      begin
        Panels[FRollOutPanel].OpenWidth := Panels[FRollOutPanel].Panel.Width;
        Panels[FRollOutPanel].Panel.OpenWidth := Panels[FRollOutPanel].Panel.Width;
      end;

{$IFDEF TMSDEBUG}
      outputdebugstring(pchar('timer rollin: ' + inttostr(Panels[FRollOutPanel].OpenWidth)));
{$ENDIF}

      if not Panels[FRollOutPanel].Panel.Floating and not Panels[FRollOutPanel].Panel.Locked then
      begin
        FRollInOut := True;
        FRollInCandidate := -1;
        IRollIn(Panels[FRollOutPanel].Panel);
        FRollInOut := False;
        //FRollOutPanel := -1;
        FTimerCount := 0;
      end;
    end;
  end
  else
    FTimerCount := 0;
end;

procedure TAdvToolPanelTab.UnHookParent;
var
  p: TWinControl;
begin
  if FDisableParentHook then
    Exit;

  p := self;
  repeat
    p := p.Parent;
  until (p is TForm) or (p is TActiveForm) or not Assigned(p);

  if (p <> nil) then
  begin
    p.WindowProc := FFormWndProc;


    if (p is TForm) then
    begin
      (p as TForm).OnCloseQuery := FCQEvent;
      (p as TForm).OnShow := FFSEvent;
    end;
  end;

  FParentHooked := false;
end;

procedure TAdvToolPanelTab.WndProc(var Msg: TMessage);
begin
  if (Msg.Msg = WM_DESTROY) then
  begin
    FWMDestroy := true;
    KillTimer(Handle, FTimerID);

    if not (csDesigning in ComponentState) then
    begin
      if Assigned(fPanelForm) then
        FreeAndNil(FPanelForm);
    end;

    // restore subclassed proc
    if not (csDesigning in ComponentState) and FParentHooked then
    begin
      UnhookParent;
    end;
  end;

  if FAutoThemeAdapt and (Msg.Msg = WM_THEMECHANGED) then
  begin
    ThemeAdapt;
  end;

  inherited;
end;

procedure TAdvToolPanelTab.Resize;
begin
  inherited;
  FPaintOffset := 0;
  UpdatePanels(-1);
  UpdatePanelForm;
end;

procedure TAdvToolPanelTab.SetPosition(const Value: TPanelPosition);
var
  i: integer;
begin
  FPosition := Value;
  for i := 1 to Panels.Count do
  begin
    Panels[i - 1].Panel.Visible := false;
  end;

  if Assigned(FPanelForm) then
    FPanelForm.Visible := false;

  case FPosition of
    ppLeft: Align := alLeft;
    ppRight: Align := alRight;
    ppTop: Align := alTop;
    ppBottom: Align := alBottom;
  end;

  UpdatePanels(-1);
end;

procedure TAdvToolPanelTab.ResetTimer;
begin
  FTimerCount := 0;
end;

procedure TAdvToolPanelTab.SetBounds(ALeft, ATop, AWidth,
  AHeight: Integer);
var
  j: Integer;
  OldLeft: Integer;
begin
  OldLeft := Left;
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);

  if not Assigned(Panels) then
    Exit;

  if not (csLoading in ComponentState) and
    not (csDesigning in ComponentState) then
  begin
    for j := 1 to Panels.Count do
    begin
      if Panels[j - 1].Panel.Visible and not Panels[j - 1].Panel.Floating then
      begin
        if (Position = ppLeft) or (Position = ppRight) then
        begin
          Panels[j - 1].Panel.Height := AHeight;
        end
        else
        begin
          Panels[j - 1].Panel.Width := AWidth;
        end;
      end;
    end;

    if (Position = ppRight) and Assigned(FPanelForm) and (FPanelForm.Visible) then
    begin
      FPanelForm.Left := FPanelForm.Left + (ALeft - OldLeft);
    end;
  end;
end;

procedure TAdvToolPanelTab.EnableTimer(Enabled: Boolean);
begin
  FTimerEnable := Enabled;
end;

procedure TAdvToolPanelTab.RollIn(APanel: TAdvToolPanel);
var
  j: Integer;
begin
//  if Assigned(FOnTabSlideIn) then
//    FOnTabSlideIn(Self, PanelIndex(APanel), APanel);

  for j := 1 to Panels.Count do
  begin
    if (Panels[j - 1].Panel.Visible) and (Panels[j - 1].Panel = APanel)
      and not APanel.Locked and not APanel.Floating then
    begin
{$IFDEF TMSDEBUG}
      outputdebugstring(pchar('rollinout:' + inttostr(Panels[j - 1].Panel.Width) + ':' + inttostr(Panels[j - 1].OpenWidth)));
{$ENDIF}
      if (Position = ppLeft) or (Position = ppRight) then
      begin
        Panels[j - 1].OpenWidth := Panels[j - 1].Panel.Width;
      end
      else {if (Position = ppTop) or (Position = ppBottom)}
      begin
        Panels[j - 1].OpenWidth := Panels[j - 1].Panel.Height;
      end;
      IRollIn(Panels[j - 1].Panel);
      Panels[j - 1].Panel.Visible := False;
    end;
  end;

  Panels[PanelIndex(APanel)].State := psClosed;

//  if Assigned(FOnTabSlideInDone) then
//    FOnTabSlideInDone(Self, PanelIndex(APanel), APanel);
end;

procedure TAdvToolPanelTab.RollOut(APanel: TAdvToolPanel);
var
  j: Integer;
  pt: TPoint;

begin
//  if Assigned(FOnTabSlideOut) then
//    FOnTabSlideOut(Self, PanelIndex(APanel), APanel);

  for j := 1 to Panels.Count do
  begin
    if (Panels[j - 1].Panel = APanel) then
    begin
      case Position of
        ppLeft:
          begin
            Panels[j - 1].Panel.Align := alNone;
            Panels[j - 1].Panel.Height := 0;
            Panels[j - 1].Panel.Visible := True;
            Panels[j - 1].Panel.Height := Height;
            Panels[j - 1].Panel.Top := Top;
            Panels[j - 1].Panel.Left := -Panels[j - 1].OpenWidth;
            Panels[j - 1].Panel.Width := Panels[j - 1].OpenWidth;
            Panels[j - 1].Panel.Parent := self.Parent; // form becomes parent of panel
            self.BringToFront;

{$IFDEF TMSDEBUG}
            outputdebugstring(pchar('rollout:' + inttostr(Panels[j - 1].OpenWidth) + ':' + inttostr(Panels[j - 1].Panel.Left)));
{$ENDIF}
            IRollOut(Panels[j - 1].Panel);
            FRollOutPanel := j - 1;
          end;
        ppRight:
          begin
{$IFDEF TMSDEBUG}
            outputdebugstring(pchar('rollout:' + inttostr(height)));
{$ENDIF}
            pt := Point(Left, Top);
            pt := Parent.ClientToScreen(pt);
            Panels[j - 1].Panel.Parent := FPanelForm; // panelform becomes parent of panel
            Panels[j - 1].Panel.Left := 2;
            Panels[j - 1].Panel.Top := 0;
            Panels[j - 1].Panel.Anchors := [akLeft, akTop, akRight, akBottom];

            Panels[j - 1].Panel.Width := 0;
            Panels[j - 1].Panel.Height := Height;

            FPanelForm.Top := pt.Y;
            FPanelForm.Left := pt.X;
            FPanelForm.Width := 0;
            FPanelForm.Height := Height;

            Panels[j - 1].Panel.Visible := True;

            FPanelForm.Visible := True;

            {$IFDEF DELPHI9_LVL}
            FPanelForm.Top := pt.Y;
            FPanelForm.Left := pt.X;
            {$ENDIF}

            FPanelForm.BringToFront;
            SetWindowPos(FPanelForm.Handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE);

            IRollOut(Panels[j - 1].Panel);
            FRollOutPanel := j - 1;
          end;
        ppTop:
          begin
            Panels[j - 1].Panel.Align := alNone;
            Panels[j - 1].Panel.Height := 0;
            Panels[j - 1].Panel.Height := Panels[j - 1].OpenWidth;
            Panels[j - 1].Panel.Top := -Panels[j - 1].OpenWidth;
            Panels[j - 1].Panel.Left := Left;
            Panels[j - 1].Panel.Visible := True;

            Panels[j - 1].Panel.Width := Width;
            Panels[j - 1].Panel.Parent := self.Parent;
            self.BringToFront;
{$IFDEF TMSDEBUG}
            outputdebugstring(pchar('rollout:' + inttostr(Panels[j - 1].OpenWidth) + ':' + inttostr(Panels[j - 1].Panel.Left)));
{$ENDIF}
            IRollOut(Panels[j - 1].Panel);
            FRollOutPanel := j - 1;
          end;
        ppBottom:
          begin
            pt := Point(Left, Top);
            pt := Parent.ClientToScreen(pt);
            Panels[j - 1].Panel.Parent := FPanelForm;
            Panels[j - 1].Panel.Left := 2;
            Panels[j - 1].Panel.Top := 0;
            Panels[j - 1].Panel.Anchors := [akLeft, akTop, akRight, akBottom];

            Panels[j - 1].Panel.Width := Width;
            Panels[j - 1].Panel.Height := 0;

            FPanelForm.Top := pt.Y;
            FPanelForm.Left := pt.X;
            FPanelForm.Width := Width;
            FPanelForm.Height := 0;

            Panels[j - 1].Panel.Visible := True;

            FPanelForm.Visible := True;

            {$IFDEF DELPHI9_LVL}
            FPanelForm.Top := pt.Y;
            FPanelForm.Left := pt.X;
            {$ENDIF}

            FPanelForm.BringToFront;
            SetWindowPos(FPanelForm.Handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE);

            IRollOut(Panels[j - 1].Panel);
            FRollOutPanel := j - 1;
          end;
      end;
    end;
  end;

  Panels[PanelIndex(APanel)].State := psOpened;

//  if Assigned(FOnTabSlideOutDone) then
//    FOnTabSlideOutDone(Self, PanelIndex(APanel), APanel);
end;

procedure TAdvToolPanelTab.RollInOut(Index: Integer);
var
  j: Integer;
  pt: TPoint;
  actWin: THandle;
  ph: Integer;
begin
  actWin := GetActiveWindow;

  for j := 1 to Panels.Count do
  begin
    if (Panels[j - 1].Panel.Visible) and (j - 1 <> Index)
      and not Panels[j - 1].Panel.Locked and not Panels[j - 1].Panel.Floating then
    begin
{$IFDEF TMSDEBUG}
      outputdebugstring(pchar('rollinout:' + inttostr(Panels[j - 1].Panel.Width) + ':' + inttostr(Panels[j - 1].OpenWidth)));
{$ENDIF}
      if (Position = ppLeft) or (Position = ppRight) then
      begin
        Panels[j - 1].OpenWidth := Panels[j - 1].Panel.Width;
      end
      else {if (Position = ppTop) or (Position = ppBottom)}
      begin
        Panels[j - 1].OpenWidth := Panels[j - 1].Panel.Height;
      end;
      IRollIn(Panels[j - 1].Panel);
      Panels[j - 1].Panel.Visible := False;
    end;
  end;

  ph := 0;

  if Assigned(GetParentForm(Self)) then
  begin
    ph := TWinControl(GetParentForm(Self)).Height;
  end;

  for j := 1 to Panels.Count do
  begin
    if (j - 1 = Index) and (Index <> FRollOutPanel) then
    begin
      case Position of
        ppLeft:
          begin
            Panels[j - 1].Panel.Align := alNone;
            Panels[j - 1].Panel.Height := ph;
            Panels[j - 1].Panel.Visible := True;
            Panels[j - 1].Panel.Height := Height;
            Panels[j - 1].Panel.Top := Top;
            Panels[j - 1].Panel.Left := -Panels[j - 1].OpenWidth;
            Panels[j - 1].Panel.Width := Panels[j - 1].OpenWidth;
            Panels[j - 1].Panel.Parent := self.Parent;
            self.BringToFront; // v1.2

{$IFDEF TMSDEBUG}
            outputdebugstring(pchar('rollout:' + inttostr(Panels[j - 1].OpenWidth) + ':' + inttostr(Panels[j - 1].Panel.Left)));
{$ENDIF}
            IRollOut(Panels[j - 1].Panel);
            FRollOutPanel := Index;
          end;
        ppRight:
          begin
{$IFDEF TMSDEBUG}
            outputdebugstring(pchar('rollout:' + inttostr(height)));
{$ENDIF}
            pt := Point(Left, Top);
            pt := Parent.ClientToScreen(pt);
            Panels[j - 1].Panel.Parent := FPanelForm;
            Panels[j - 1].Panel.Left := 2;
            Panels[j - 1].Panel.Top := 0;
            Panels[j - 1].Panel.Anchors := [akLeft, akTop, akRight, akBottom];

            Panels[j - 1].Panel.Width := 0;
            Panels[j - 1].Panel.Height := Height;

            FPanelForm.Top := pt.Y;
            FPanelForm.Left := pt.X;
            FPanelForm.Width := 0;
            FPanelForm.Height := Height;

            Panels[j - 1].Panel.Visible := True;

            FPanelForm.Visible := True;

            {$IFDEF DELPHI9_LVL}
            FPanelForm.Top := pt.Y;
            FPanelForm.Left := pt.X;
            {$ENDIF}

            FPanelForm.BringToFront;
            SetWindowPos(FPanelForm.Handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE);

            if (actWin = self.Parent.Handle) then
              SetActiveWindow(Parent.Handle);

            IRollOut(Panels[j - 1].Panel);
            FRollOutPanel := Index;
          end;
        ppTop:
          begin
            Panels[j - 1].Panel.Align := alNone;
            Panels[j - 1].Panel.Height := 0;
            Panels[j - 1].Panel.Height := Panels[j - 1].OpenWidth;
            Panels[j - 1].Panel.Top := -Panels[j - 1].OpenWidth;
            Panels[j - 1].Panel.Left := Left;
            Panels[j - 1].Panel.Visible := True;
            Panels[j - 1].Panel.Width := Width;
            Panels[j - 1].Panel.Parent := self.Parent;
            self.BringToFront;
{$IFDEF TMSDEBUG}
            outputdebugstring(pchar('rollout:' + inttostr(Panels[j - 1].OpenWidth) + ':' + inttostr(Panels[j - 1].Panel.Left)));
{$ENDIF}
            IRollOut(Panels[j - 1].Panel);
            FRollOutPanel := Index;
          end;
        ppBottom:
          begin
            pt := Point(Left, Top);
            pt := Parent.ClientToScreen(pt);
            Panels[j - 1].Panel.Parent := FPanelForm;
            Panels[j - 1].Panel.Left := 2;
            Panels[j - 1].Panel.Top := 0;
            Panels[j - 1].Panel.Anchors := [akLeft, akTop, akRight, akBottom];

            Panels[j - 1].Panel.Width := Width;
            Panels[j - 1].Panel.Height := 0;

            FPanelForm.Top := pt.Y;
            FPanelForm.Left := pt.X;
            FPanelForm.Width := Width;
            FPanelForm.Height := 0;

            Panels[j - 1].Panel.Visible := True;

            FPanelForm.Visible := True;

            {$IFDEF DELPHI9_LVL}
            FPanelForm.Top := pt.Y;
            FPanelForm.Left := pt.X;
            {$ENDIF}

            FPanelForm.BringToFront;
            SetWindowPos(FPanelForm.Handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE);

            if actWin = self.Parent.Handle then
              SetActiveWindow(Parent.Handle);

            IRollOut(Panels[j - 1].Panel);
            FRollOutPanel := Index;
          end;
      end;
    end;
  end;
end;

procedure TAdvToolPanelTab.LockChange(APanel: TAdvToolPanel);
var
  i: Integer;
  pt: TPoint;
  pw, ph: Integer;
  actWin: THandle;
begin
  pw := APanel.Width;
  ph := APanel.Height;

  actWin := GetActiveWindow;

  if PanelsInTab = 0 then
  begin
    if (Position = ppLeft) or (Position = ppRight) then
      Width := 2
    else {(Position = ppTop) or (Position = ppBottom)}
      Height := 2;
  end
  else
  begin
    if (Position = ppLeft) or (Position = ppRight) then
      Width := FTabWidth
    else {(Position = ppTop) or (Position = ppBottom)}
      Height := FTabWidth;
  end;

  case Position of
    ppLeft:
      begin
        if APanel.Locked then
        begin
{$IFDEF TMSDEBUG}
          outputdebugstring(pchar('lock:' + apanel.Caption + ':' + inttostr(apanel.Left) + ':' + inttostr(apanel.OpenWidth)));
{$ENDIF}
          for i := 1 to Panels.Count do
          begin
            if Panels[i - 1].Panel.Locked and (Panels[i - 1].Panel <> APanel) then
            begin
{$IFDEF TMSDEBUG}
              outputdebugstring(pchar(panels[i - 1].panel.Caption + ':' + inttostr(Panels[i - 1].Panel.Left) + ':' + inttostr(apanel.openwidth)));
{$ENDIF}
              //Panels[i - 1].Panel.Left := Panels[i - 1].Panel.Left + APanel.OpenWidth;
              APanel.Left := APanel.Left + Panels[i - 1].OpenWidth;
            end;
          end;
          APanel.Align := alLeft;
          APanel.Width := APanel.OpenWidth;
          APanel.SendToBack;
          RemovePanel(nil);
          FRollOutPanel := -1
        end
        else
        begin
          // force alignment to leftmost position
          self.Left := 0;
          for i := 1 to Panels.Count do
          begin
            if Panels[i - 1].Panel.Locked and (Panels[i - 1].Panel <> APanel) then
            begin
              if APanel.Left < Panels[i - 1].Panel.Left then
                Panels[i - 1].Panel.Left := Panels[i - 1].Panel.Left - APanel.Width;
            end;
          end;

          for i := 1 to Panels.Count do
          begin
            if Panels[i - 1].Panel = APanel then
            begin
              FRollOutPanel := i - 1;
              Break;
            end;
          end;
          RemovePanel(nil);
          APanel.Align := alNone;
          APanel.Left := FTabWidth;
          APanel.SetTopMost;
        end;
      end;
    ppRight:
      begin
        if APanel.Locked then
        begin
{$IFDEF TMSDEBUG}
          outputdebugstring(pchar('lock new panel at : ' + inttostr(apanel.Left)));
{$ENDIF}

          for i := 1 to Panels.Count do
          begin
            if Panels[i - 1].Panel.Locked and (Panels[i - 1].Panel <> APanel) then
            begin
              Panels[i - 1].Panel.Left := Panels[i - 1].Panel.Left - APanel.Width;
            end;
          end;

          APanel.Align := alRight;
          APanel.Parent := self.Parent;

          for i := 1 to Panels.Count do
          begin
            if Panels[i - 1].Panel.Locked then
              Panels[i - 1].Panel.Width := Panels[i - 1].Panel.OpenWidth;
          end;

          APanel.SendToBack;
          FPanelForm.Hide;

          FRollOutPanel := -1
        end
        else
        begin
{$IFDEF TMSDEBUG}
          outputdebugstring(pchar('unlock new panel at : ' + inttostr(apanel.Left)));
{$ENDIF}

          for i := 1 to Panels.Count do
          begin
            if Panels[i - 1].Panel.Locked and (Panels[i - 1].Panel <> APanel) then
            begin
              if APanel.Left > Panels[i - 1].Panel.Left then
                Panels[i - 1].Panel.Left := Panels[i - 1].Panel.Left + APanel.OpenWidth;
            end;
          end;

          for i := 1 to Panels.Count do
          begin
            if Panels[i - 1].Panel = APanel then
            begin
              FRollOutPanel := i - 1;
              break;
            end;
          end;

          for i := 1 to Panels.Count do
          begin
            if Panels[i - 1].Panel.Locked then
            begin
{$IFDEF TMSDEBUG}
              outputdebugstring(pchar('restore:' + inttostr(Panels[i - 1].Panel.OpenWidth)));
{$ENDIF}
              Panels[i - 1].Panel.Width := Panels[i - 1].Panel.OpenWidth;
            end;
          end;

          APanel.OpenWidth := pw + 2;

{$IFDEF TMSDEBUG}
          outputdebugstring(pchar('should not be here:' + inttostr(apanel.openwidth)));
{$ENDIF}

          FPanelForm.Width := 0;
          APanel.Parent := FPanelForm;
          APanel.Align := alNone;
          APanel.Left := 2;
          APanel.Top := 0;
          APanel.Height := Height;
          APanel.Width := APanel.OpenWidth - 2;
          APanel.Anchors := [akLeft, akTop, akRight, akBottom];

          pt := Point(self.Left + self.Width, self.Top);
          pt := Parent.ClientToScreen(pt);
          
          //  FPanelForm.Left := pt.X - APanel.OpenWidth
          //  FPanelForm.Left := pt.X;

          FPanelForm.Left := pt.X - self.TabWidth - APanel.OpenWidth;

          FPanelForm.Top := pt.Y;
          FPanelForm.Height := Height;
          FPanelForm.Width := APanel.OpenWidth;
          FPanelForm.Visible := True;
          SetWindowPos(FPanelForm.Handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE);
        end;
        if actWin = self.Parent.Handle then
          SetActiveWindow(Parent.Handle);
      end;
    ppTop:
      begin
        if APanel.Locked then
        begin
{$IFDEF TMSDEBUG}
          outputdebugstring(pchar('lock:' + apanel.Caption + ':' + inttostr(apanel.Left) + ':' + inttostr(apanel.OpenWidth)));
{$ENDIF}
          for i := 1 to Panels.Count do
          begin
            if Panels[i - 1].Panel.Locked and (Panels[i - 1].Panel <> APanel) then
            begin
{$IFDEF TMSDEBUG}
              outputdebugstring(pchar(panels[i - 1].panel.Caption + ':' + inttostr(Panels[i - 1].Panel.Left) + ':' + inttostr(apanel.openwidth)));
{$ENDIF}
              APanel.Top := APanel.Top + Panels[i - 1].OpenWidth;
            end;
          end;

          APanel.Align := alTop;
          APanel.Height := APanel.OpenWidth;
          APanel.SendToBack;
          RemovePanel(nil);
          FRollOutPanel := -1
        end
        else
        begin
          for i := 1 to Panels.Count do
          begin
            if Panels[i - 1].Panel.Locked and (Panels[i - 1].Panel <> APanel) then
            begin
              if APanel.Top < Panels[i - 1].Panel.Top then
                Panels[i - 1].Panel.Top := Panels[i - 1].Panel.Top - APanel.Height;
            end;
          end;

          for i := 1 to Panels.Count do
          begin
            if Panels[i - 1].Panel = APanel then
            begin
              FRollOutPanel := i - 1;
              Break;
            end;
          end;
          RemovePanel(nil);
          APanel.Align := alNone;
          APanel.Top := FTabWidth;
          APanel.SetTopMost;
        end;
      end;
    ppBottom:
      begin
        if APanel.Locked then
        begin
{$IFDEF TMSDEBUG}
          outputdebugstring(pchar('lock new panel at : ' + inttostr(apanel.Left)));
{$ENDIF}
          APanel.Align := alBottom;
          APanel.Parent := self.Parent;

          for i := 1 to Panels.Count do
          begin
            if Panels[i - 1].Panel.Locked then
              Panels[i - 1].Panel.Height := Panels[i - 1].Panel.OpenWidth;
          end;

          APanel.SendToBack;
          FPanelForm.Hide;

          FRollOutPanel := -1
        end
        else
        begin
{$IFDEF TMSDEBUG}
          outputdebugstring(pchar('unlock new panel at : ' + inttostr(apanel.Left)));
{$ENDIF}

          for i := 1 to Panels.Count do
          begin
            if Panels[i - 1].Panel.Locked and (Panels[i - 1].Panel <> APanel) then
            begin
              if APanel.Top > Panels[i - 1].Panel.Top then
                Panels[i - 1].Panel.Top := Panels[i - 1].Panel.Top + APanel.OpenWidth;
            end;
          end;

          for i := 1 to Panels.Count do
          begin
            if Panels[i - 1].Panel = APanel then
            begin
              FRollOutPanel := i - 1;
              Break;
            end;
          end;

          for i := 1 to Panels.Count do
          begin
            if Panels[i - 1].Panel.Locked then
            begin
{$IFDEF TMSDEBUG}
              outputdebugstring(pchar('restore:' + inttostr(Panels[i - 1].Panel.OpenWidth)));
{$ENDIF}
              Panels[i - 1].Panel.Height := Panels[i - 1].Panel.OpenWidth;
            end;
          end;

          APanel.OpenWidth := ph + 2;
          pt := Point(Left, Top);
          pt := Parent.ClientToScreen(pt);

{$IFDEF TMSDEBUG}
          outputdebugstring(pchar('should not be here:' + inttostr(apanel.openwidth)));
{$ENDIF}

          FPanelForm.Height := 0;
          APanel.Parent := FPanelForm;
          APanel.Align := alNone;
          APanel.Left := 0;
          APanel.Top := 2;
          APanel.width := width;
          APanel.Height := APanel.OpenWidth - 2;
          APanel.Anchors := [akLeft, akTop, akRight, akBottom];
          FPanelForm.Left := pt.X;
          FPanelForm.Top := pt.Y - APanel.OpenWidth;
          FPanelForm.width := width;
          FPanelForm.Height := APanel.OpenWidth;
          FPanelForm.Visible := True;
          SetWindowPos(FPanelForm.Handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE);
        end;
        if actWin = self.Parent.Handle then
          SetActiveWindow(Parent.Handle);
      end;
  end;
  Invalidate;
end;

procedure TAdvToolPanelTab.WMCommand(var Message: TMessage);
begin
  inherited;
end;

procedure TAdvToolPanelTab.SubclassProc(var Msg: TMessage);
var
  pt: TPoint;
  i: Integer;
begin
  if (Msg.Msg = WM_SIZE) then
  begin
    if Position in [ppLeft, ppRight] then
    begin
      for i := 1 to Panels.Count do
        if Panels[i - 1].Panel.Height > Msg.LParamHi then
          Panels[i - 1].Panel.Height := Msg.LparamHi;
    end;
    if Position in [ppTop, ppBottom] then
    begin
      for i := 1 to Panels.Count do
        if Panels[i - 1].Panel.Width > Msg.LParamLo then
          Panels[i - 1].Panel.Width := Msg.LparamLo;
    end;
  end;

  FFormWndProc(Msg);

  if (Msg.Msg = WM_CLOSE) then
  begin
    if Persist.AutoSave and not (csDesigning in ComponentState) then
      SaveState;

    if Assigned(FPanelForm) and FPanelForm.Visible then
      FPanelForm.Hide;
  end;

  if ((Msg.Msg = WM_MOVING) or (Msg.Msg = WM_MOVE) or (Msg.Msg = WM_PAINT)) and (Position in [ppRight,ppBottom]) and (FRollOutPanel <> -1) then
  begin
    UpdatePanelForm;
    {
    pt := Point(Left, Top);
    pt := Parent.ClientToScreen(pt);
    NewX := pt.X - FPanelForm.Width;
    NewY := pt.Y;

    flg := (NewX <> FPanelForm.Left) or (NewY <> FPanelForm.Top);

    FPanelForm.Left := NewX;
    FPanelForm.Top := NewY;

    SetActiveWindow(self.Parent.Handle);

    if flg then
      self.Parent.Invalidate;
     }
  end;

  if ((Msg.Msg = WM_MOVING) or (Msg.Msg = WM_PAINT)) and (Position = ppBottom) and (FRollOutPanel <> -1) then
  begin
    UpdatePanelForm;
    {
    pt := Point(Left, Top);
    pt := Parent.ClientToScreen(pt);
    NewX := pt.X;
    NewY := pt.Y - FPanelForm.Height;

    flg := (NewX <> FPanelForm.Left) or (NewY <> FPanelForm.Top);

    FPanelForm.Left := NewX;
    FPanelForm.Top := NewY;

    SetActiveWindow(self.Parent.Handle);

    if flg then
      self.Parent.Invalidate;
    }
  end;

  if (Msg.Msg = WM_SYSCOMMAND) and
    ((Msg.WParam = SC_MAXIMIZE) or (Msg.WParam = SC_MINIMIZE) or
    (Msg.WParam = SC_RESTORE) or (Msg.WParam = 61458) or (Msg.WParam = 61490) or (Msg.WParam = 61730)) and
    (Position = ppRight) and (FRollOutPanel <> -1) then
  begin

    pt := Point(Left, Top);
    pt := Parent.ClientToScreen(pt);
    FPanelForm.Left := pt.X - FPanelForm.Width;
    FPanelForm.Top := pt.Y;
    FPanelForm.Height := Height;

    if (Msg.WParam = SC_MAXIMIZE) then
      for i := 1 to Panels.Count do
      begin
        if not Panels[i - 1].Panel.Locked then
          IRollIn(Panels[i - 1].Panel);
      end;

    SetActiveWindow(self.Parent.Handle);
  end;
end;

procedure TAdvToolPanelTab.UpdatePanelForm;
var
  pt: TPoint;
  NewX, NewY: integer;
  flg: Boolean;
begin
  if not Assigned(FPanelForm) then
    Exit;

  if not FPanelForm.Visible then
    Exit;

  pt := Point(Left, Top);
  pt := Parent.ClientToScreen(pt);

  NewX := FPanelForm.Left;
  NewY := FPanelForm.Top;

  if Position = ppRight then
  begin
    NewX := pt.X - FPanelForm.Width;
    NewY := pt.Y;
  end;

  if Position = ppBottom then
  begin
    NewX := pt.X;
    NewY := pt.Y - FPanelForm.Height;
  end;

  flg := (NewX <> FPanelForm.Left) or (NewY <> FPanelForm.Top);

  FPanelForm.Left := NewX;
  FPanelForm.Top := NewY;

  SetActiveWindow(self.Parent.Handle);

  if flg then
    self.Parent.Invalidate;
end;


procedure TAdvToolPanelTab.HookParent;
var
  p: TWinControl;
begin
  if FDisableParentHook then
    Exit;
  p := self;

  repeat
    p := p.Parent;
  until (p is TForm) or (p is TActiveForm) or not Assigned(p);

  if Assigned(p) then
  begin
    if (p is TActiveForm) then
    begin
      {
      hwnd := p.Handle;
      i := 1;
      repeat
        getwindowtext(hwnd, wt, sizeof(wt));
        hwnd := getparent(hwnd);
        inc(i);
      until (hwnd = 0) or (i = 100);
      }
    end;
  end;

  if Assigned(p) then
  begin
    FFormWndProc := p.WindowProc;
    p.WindowProc := SubClassProc;

    FParentHooked := true;

    if (p is TForm) then
    begin
      FCQEvent := (p as TForm).OnCloseQuery;
      FFSEvent := (p as TForm).OnShow;
      (p as TForm).OnCloseQuery := TabCloseQuery;
      (p as TForm).onShow := TabFormShow;
    end;
  end;
end;

procedure TAdvToolPanelTab.CreatePanelForm;
begin
  FPanelForm := TToolPanelForm.Create(Self);
  FPanelForm.BorderStyle := bsNone;
  FPanelForm.Parent := nil;
  FPanelForm.Visible := False;
  FPanelForm.DoubleBuffered := true;
  FPanelForm.Name := 'tabf' + Name + inttostr(Parent.ControlCount);
  FPanelForm.Tab := Self;
end;

procedure TAdvToolPanelTab.CreateWnd;
begin
  inherited;

  if not (csDesigning in ComponentState) then
  begin
    CreatePanelForm;
    case AutoOpenCloseSpeed of
      aocVerySlow: FTimerID := SetTimer(Handle, 500, 1500, nil);
      aocSlow: FTimerID := SetTimer(Handle, 500, 500, nil);
      aocMedium: FTimerID := SetTimer(Handle, 500, 200, nil);
      aocFast: FTimerID := SetTimer(Handle, 500, 75, nil);
      aocVeryFast: FTimerID := SetTimer(Handle, 500, 40, nil);
    end;

    HookParent;
  end;
end;

procedure TAdvToolPanelTab.NextPanel;
begin
  if FDesignViewPanel = -1 then
    FDesignViewPanel := 0;

  if csDesigning in ComponentState then
  begin
    if FDesignViewPanel + 1 < ControlCount then
    begin
      inc(FDesignViewPanel);
      UpdatePanels(FDesignViewPanel);
    end;
  end
  else
  begin
    if FDesignViewPanel + 1 < Panels.Count then
    begin
      inc(FDesignViewPanel);
      UpdatePanels(FDesignViewPanel);
    end;
  end;
end;

procedure TAdvToolPanelTab.PrevPanel;
begin
  if FDesignViewPanel = -1 then
    FDesignViewPanel := 0;

  if csDesigning in ComponentState then
  begin
    if (FDesignViewPanel > 0) then
    begin
      dec(FDesignViewPanel);
      if FDesignViewPanel < ControlCount then
        UpdatePanels(FDesignViewPanel);
    end;
  end
  else
  begin
    if (FDesignViewPanel > 0) then
    begin
      dec(FDesignViewPanel);
      if FDesignViewPanel < Panels.Count then
        UpdatePanels(FDesignViewPanel);
    end;
  end;
end;

procedure TAdvToolPanelTab.SetTabColor(const Value: TColor);
begin
  FTabColor := Value;
  Invalidate;
end;

procedure TAdvToolPanelTab.SetAutoDock(const Value: Boolean);
begin
  FAutoDock := Value;
  DockSite := FAutoDock;
end;

procedure TAdvToolPanelTab.DockDrop(Source: TDragDockObject; X,
  Y: Integer);
var
  s: string;
  ow: Integer;
begin
{$IFDEF TMSDEBUG}
  outputdebugstring(pchar('dock drop'));
{$ENDIF}

  if Source.Control is TAdvToolPanel then
  begin
    with Source.Control as TAdvToolPanel do
    begin
      Tab.RemovePanel(TAdvToolPanel(Source.Control));
      InsertPanel(TAdvToolPanel(Source.Control));
      FActive := False;
      Docking := False;

      DragKind := dkDrag;
      DragMode := dmManual;
      ow := OpenWidth;

{$IFDEF TMSDEBUG}
      // get width of the control
      outputdebugstring(pchar('width of docked control:' + inttostr(ow)));
{$ENDIF}

      Tab := self;
      case Position of
        ppLeft:
          begin
            Parent := Self.Parent;
            Left := FTabWidth;
            Align := alLeft;
            Top := self.Top + 10;
          end;
        ppRight:
          begin
{$IFDEF TMSDEBUG}
            outputdebugstring('set right alignment');
{$ENDIF}
            Left := self.Left - OpenWidth - 1;
            Width := OpenWidth;
            Align := alRight;
            Parent := Self.Parent;
          end;
        ppTop:
          begin
            Parent := Self.Parent;
          //Left := 0;
            Top := FTabWidth;
            Align := alTop;
          end;
        ppBottom:
          begin
          //Left := self.Left - OpenWidth - 1;
            Top := self.Top - OpenWidth - 1;
            Height := OpenWidth;
            Align := alBottom;
            Parent := Self.Parent;
          end;
      end;
      s := self.Parent.ClassName;

{$IFDEF TMSDEBUG}
      outputdebugstring(pchar(s + ':' + inttostr(openwidth)));
      outputdebugstring(pchar('dock width change: ' + inttostr(ow)));
{$ENDIF}

      OpenWidth := ow;
      Locked := True;
      OpenWidth := ow;
      if ((Position = ppLeft) or (Position = ppRight)) then
        Width := ow
      else {((Position = ppTop) or (Position = ppBottom))}
        Height := ow;
    end;
  end;
end;

procedure TAdvToolPanelTab.DockOver(Source: TDragDockObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var
  ARect: TRect;
  pt: TPoint;

begin
  inherited;

  Accept := Accept and (Source.Control is TAdvToolPanel);

  if Accept then
  begin
    //Modify the DockRect to preview dock area.
    case Position of
      ppLeft:
        begin
          ARect.TopLeft := ClientToScreen(Point(0, 0));
          ARect.BottomRight := ClientToScreen(Point(100, Height));
        end;
      ppRight:
        begin
          pt := ClientToScreen(Point(0, 0));
          pt.X := pt.X - 100 + Width;
          ARect.TopLeft := pt;
          ARect.BottomRight := ClientToScreen(Point(Width, Height));
        end;
      ppTop:
        begin
          ARect.TopLeft := ClientToScreen(Point(0, 0));
          ARect.BottomRight := ClientToScreen(Point(Width, 100));
        end;
      ppBottom:
        begin
          pt := ClientToScreen(Point(0, 0));
          pt.y := pt.y - 100 + Height;
          ARect.TopLeft := pt;
          ARect.BottomRight := ClientToScreen(Point(Width, Height));
        end;
    end;
    Source.DockRect := ARect;
  end;
end;

procedure TAdvToolPanelTab.UpdatePanel(APanel: TAdvToolPanel);
var
  i: Integer;
begin
  for i := 1 to Panels.Count do
  begin
    if Panels[i - 1].Panel = APanel then
    begin
      Panels[i - 1].Caption := APanel.Caption;
      Panels[i - 1].ImageIndex := APanel.ImageIndex;
      Panels[i - 1].OpenWidth := APanel.OpenWidth;
    end;
  end;
  Invalidate;
end;



procedure TAdvToolPanelTab.InsertPanel(APanel: TAdvToolPanel);
begin
  with Panels.Add do
  begin
    Caption := APanel.Caption;
    ImageIndex := APanel.ImageIndex;
    Panel := APanel;
    OpenWidth := APanel.OpenWidth;
    UpdatePanels(-1);
  end;
end;

procedure TAdvToolPanelTab.RemovePanel(APanel: TAdvToolPanel);
var
  i, j: Integer;
begin
  j := -1;
  // find panel to be removed
  for i := 1 to Panels.Count do
  begin
    if Panels[i - 1].Panel = APanel then
    begin
      j := i - 1;
      Break;
    end;
  end;

  // remove the panel from the collection
  if (j <> -1) then
    Panels[i - 1].Free;

  // reorganize remaining panels
  for i := 1 to Panels.Count do
  begin
    if Panels[i - 1].Panel.Locked then
    begin
{$IFDEF TMSDEBUG}
      outputdebugstring(pchar('restore orig width:' + inttostr(Panels[i - 1].Panel.OpenWidth)));
{$ENDIF}
      //Panels[i - 1].Panel.Width := Panels[i - 1].Panel.OpenWidth;  // FF: comented for alClient issue

      if (Position = ppLeft) or (Position = ppRight) then
      begin
        Panels[i - 1].Panel.Width := Panels[i - 1].Panel.OpenWidth;
      end
      else {if (Position = ppTop) or (Position = ppBottom)}
      begin
        Panels[i - 1].Panel.Height := Panels[i - 1].Panel.OpenWidth;
      end;

    end;
  end;

  // make sure right panel tab is rightmost control
  if Position = ppRight then
    Left := Parent.Width;

  UpdatePanels(-1);
end;

function TAdvToolPanelTab.CreatePanel: TAdvToolPanel;
begin
  Result := TAdvToolPanel.Create(Self.Parent);
  Result.Tab := Self;
  Result.Parent := self.Parent;
  Result.Visible := False;
  InsertPanel(Result);
end;

procedure TAdvToolPanelTab.UnHidePanel(APanel: TAdvToolPanel);
begin
  if APanel.Floating then
    APanel.Visible := True
  else
  begin
    if APanel.Locked then
    begin
      APanel.Hidden := False;
      APanel.Locked := False;
      APanel.Visible := True;
    end
    else
    begin
      APanel.Hidden := False;
    end;
  end;
end;

function TAdvToolPanelTab.PanelsInTab: Integer;
var
  i, j: Integer;
begin
  j := 0;
  for i := 1 to Panels.Count do
  begin
    if not Panels[i - 1].Panel.Locked and not Panels[i - 1].Panel.Floating and not Panels[i - 1].Panel.Hidden then
      inc(j);
  end;
  Result := j;
end;

procedure TAdvToolPanelTab.PersistenceChange(Sender: TObject);
var
  i: integer;
begin
  for i := 1 to Panels.Count do
  begin
    Panels[i - 1].Panel.Restored := false;
  end;
end;

procedure TAdvToolPanelTab.HidePanels;
var
  i: integer;
begin
  for i := 1 to Panels.Count do
  begin
    if not Panels[i - 1].Panel.Locked then
      Panels[i - 1].Panel.Visible := false;
  end;
  FPanelForm.Visible := false;
  FRollOutPanel := -1;
end;

procedure TAdvToolPanelTab.UnHidePanels;
var
  i: Integer;
begin
  for i := 1 to Panels.Count do
  begin
    if Panels[i - 1].Panel.Floating then
    begin
      Panels[i - 1].Panel.Visible := True;
      if i - 1 = FRollOutPanel then
        FRollOutPanel := -1;
    end
    else
    begin
      if Panels[i - 1].Panel.Locked then
      begin
        Panels[i - 1].Panel.Hidden := False;
{$IFDEF TMSDEBUG}
        outputdebugstring(pchar('unhide:' + inttostr(Panels[i - 1].Panel.OpenWidth) + ':' + inttostr(Panels[i - 1].Panel.Width)));
{$ENDIF}

        //Panels[i - 1].Panel.Locked := False;

        Panels[i - 1].Panel.Visible := True;

        Panels[i - 1].Panel.Width := Panels[i - 1].Panel.OpenWidth;

        if Position = ppLeft then
          Panels[i - 1].Panel.Left := Width
        else if Position = ppRight then
          Panels[i - 1].Panel.Left := Left - Panels[i - 1].Panel.OpenWidth
        else if Position = ppTop then
          Panels[i - 1].Panel.Top := Height
        else if Position = ppBottom then
          Panels[i - 1].Panel.Top := Top - Panels[i - 1].Panel.OpenWidth;

        if i - 1 = FRollOutPanel then
          FRollOutPanel := -1;
      end
      else
      begin
        if Panels[i - 1].Panel.Hidden then
          Panels[i - 1].Panel.Hidden := False;
      end;
    end;
  end;
  Invalidate;
end;

procedure TAdvToolPanelTab.SetColorTo(const Value: TColor);
begin
  FColorTo := Value;
  Invalidate;
end;

procedure TAdvToolPanelTab.SetTabColorTo(const Value: TColor);
begin
  FTabColorTo := Value;
  Invalidate;
end;

procedure TAdvToolPanelTab.SetTabBorderColor(const Value: TColor);
begin
  FTabBorderColor := Value;
  Invalidate;
end;

procedure TAdvToolPanelTab.SetVersion(const Value: string);
begin

end;

function TAdvToolPanelTab.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function TAdvToolPanelTab.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;


procedure TAdvToolPanelTab.CMMouseLeave(var Message: TMessage);
begin
  if FMousePanel >= 0 then
    if Assigned(FOnTabLeave) then
      FOnTabLeave(Self, FMousePanel, Panels[FMousePanel].Panel);

  FMouseInControl:= false;

  if Screen.Cursor = crSizeAll then
    Screen.Cursor := Cursor;

  FMousePanel := -1;
  FHoverPanel := -1;
  Invalidate;
  inherited;
end;

procedure TAdvToolPanelTab.WMEraseBkGnd(var Message: TMessage);
begin
  Message.Result := 1;
end;

procedure TAdvToolPanelTab.SetTabWidth(const Value: Integer);
begin
  FTabWidth := Value;
  Invalidate;
  if (csDesigning in ComponentState) then
    UpdatePanels(-1);
end;

procedure TAdvToolPanelTab.SetTextDirection(const Value: TTextDirection);
begin
  FTextDirection := Value;
  Invalidate;
end;

procedure TAdvToolPanelTab.SetTabGlyph(const Value: TBitmap);
begin
  FTabGlyph.Assign(Value);
  Invalidate;
end;

function TAdvToolPanelTab.PanelIndex(APanel: TAdvToolPanel): Integer;
var
  i: integer;
begin
  Result := -1;

  for i := 1 to FPanels.Count do
    if FPanels[i - 1].Panel = APanel then
    begin
      Result := i - 1;
      Break;
    end;
end;


function TAdvToolPanelTab.PanelByName(AName: string): TAdvToolPanel;
var
  i: Integer;
begin
  Result := nil;
  for i := 1 to Panels.Count do
  begin
    if Panels[i - 1].Panel.Name = AName then
    begin
      Result := Panels[i - 1].Panel;
      Break;
    end;
  end;
end;


procedure TAdvToolPanelTab.INISaveState;
var
  i: integer;
  INIFile: TINIFile;
  pnl: string;

begin
  if Persist.INIFile = '' then
    Exit;

  INIFIle := TINIFile.Create(Persist.INIFile);

  inifile.writeinteger(Name + 'PANELS', 'COUNT', Panels.Count);

  for i := 1 to Panels.Count do
  begin
    pnl := Name + 'PANEL' + inttostr(i);

    inifile.writestring(pnl, 'NAME', Panels[i - 1].Panel.Name);
    inifile.WriteInteger(pnl, 'OPENWIDTH', Panels[i - 1].OpenWidth);

    if Panels[i - 1].Panel.Locked then
      inifile.WriteInteger(pnl, 'LOCKED', 1)
    else
      inifile.WriteInteger(pnl, 'LOCKED', 0);

    if Panels[i - 1].Panel.Hidden then
      inifile.WriteInteger(pnl, 'HIDDEN', 1)
    else
      inifile.WriteInteger(pnl, 'HIDDEN', 0);

    if Panels[i - 1].Panel.Docking then
      inifile.WriteInteger(pnl, 'DOCKED', 1)
    else
      inifile.WriteInteger(pnl, 'DOCKED', 0);

    if Panels[i - 1].Panel.Visible then
      inifile.WriteInteger(pnl, 'VISIBLE', 1)
    else
      inifile.WriteInteger(pnl, 'VISIBLE', 0);

    if Assigned(Panels[i - 1].Panel.Tab) then
      inifile.WriteString(pnl, 'TAB', Panels[i - 1].Panel.Tab.Name);

    if Panels[i - 1].Panel.Floating then
    begin
      inifile.WriteInteger(pnl, 'LEFT', Panels[i - 1].Panel.Parent.Left);
      inifile.WriteInteger(pnl, 'TOP', Panels[i - 1].Panel.Parent.Top);
      inifile.WriteInteger(pnl, 'WIDTH', Panels[i - 1].Panel.Parent.Width);
      inifile.WriteInteger(pnl, 'HEIGHT', Panels[i - 1].Panel.Parent.Height);
    end
    else
    begin
      inifile.WriteInteger(pnl, 'LEFT', Panels[i - 1].Panel.Left);
      inifile.WriteInteger(pnl, 'TOP', Panels[i - 1].Panel.Top);
      if Panels[i - 1].Panel.Locked then
      begin
        if ((Position = ppLeft) or (Position = ppRight)) then
          inifile.WriteInteger(pnl, 'WIDTH', Panels[i - 1].Panel.Width)
        else {((Position = ppTop) or (Position = Bottom))}
          inifile.WriteInteger(pnl, 'WIDTH', Panels[i - 1].Panel.Height); // Placing height inplace of width
      end
      else
        inifile.WriteInteger(pnl, 'WIDTH', Panels[i - 1].Panel.OpenWidth);
    end;
  end;

  INIFile.Free;
end;

function TAdvToolPanelTab.INIRestoreState: Boolean;
var
  i, j, k: integer;
  INIFile: TINIFile;
  pnl, name: string;
  APanel: TAdvToolPanel;
  r: TRect;
  vis, lck: Boolean;
begin
  Result := False;

  if Persist.INIFile = '' then
    Exit;

  INIFIle := TINIFile.Create(Persist.INIFile);

  i := INIFile.ReadInteger(self.Name + 'PANELS', 'COUNT', 0);

  if i = 0 then
  begin
    INIFile.Free;
    Exit;
  end;  

  FRollOutPanel := -1;

  for j := 1 to i do
  begin
    pnl := self.Name + 'PANEL' + inttostr(j);

    name := INIFile.ReadString(pnl, 'NAME', '');

    APanel := PanelByName(name);

    if Assigned(APanel) and not APanel.Restored then
    begin
      APanel.Restored := True;

      k := PanelIndex(APanel);
      Panels[k].OpenWidth := inifile.ReadInteger(pnl, 'OPENWIDTH', 0);

      APanel.Docking := inifile.ReadInteger(pnl, 'DOCKED', 0) = 1;
      vis := inifile.ReadInteger(pnl, 'VISIBLE', 0) = 1;
      APanel.Hidden := inifile.ReadInteger(pnl, 'HIDDEN', 0) = 1;
      lck := inifile.ReadInteger(pnl, 'LOCKED', 0) = 1;

      if lck then
        APanel.OpenWidth := inifile.ReadInteger(pnl, 'WIDTH', 0)
      else
        APanel.OpenWidth := inifile.ReadInteger(pnl, 'OPENWIDTH', 0);

      if lck and (APanel.Locked <> lck) and not APanel.Hidden and not APanel.Docking then
      begin
        if Position = ppLeft then
          APanel.Left := FTabWidth;

        APanel.Align := alNone;
        if ((Position = ppLeft) or (Position = ppRight)) then
        begin
          APanel.Top := Top;
          APanel.Height := Height;
          APanel.Width := APanel.OpenWidth;
        end
        else {((Position = ppLeft) or (Position = ppRight))}
        begin
          APanel.Top := FTabWidth;
          APanel.Height := APanel.OpenWidth;
          APanel.Width := Width;
        end;
        APanel.Visible := True;
        APanel.Locked := True;
      end;

      if not lck and (APanel.Locked <> lck) and not APanel.Hidden and not APanel.Docking then
      begin
        APanel.Locked := False;
        APanel.Visible := False;
      end;

      if APanel.Docking and vis then
      begin
        r.Left := inifile.ReadInteger(pnl, 'LEFT', 0);
        r.Right := r.Left + inifile.ReadInteger(pnl, 'WIDTH', 0);
        r.Top := inifile.ReadInteger(pnl, 'TOP', 0);
        r.Bottom := r.Top + inifile.ReadInteger(pnl, 'HEIGHT', 0);
        //APanel.ManualFloat(r);
        APanel.ManualDock(nil,nil,alClient);
        APanel.HostDockSite.Visible := false;
        APanel.HostDockSite.Left := r.Left;
        APanel.HostDockSite.Top := r.Top;
        APanel.HostDockSite.Width := r.Right - r.Left;
        APanel.HostDockSite.Height := r.Bottom - r.Top;
        APanel.HostDockSite.Visible := true;
      end;
    end;
  end;

  if PanelsInTab = 0 then
    Width := 2
  else
    Width := FTabWidth;

  FRollOutPanel := -1;
  INIFile.Free;
  Result := True;
end;


procedure TAdvToolPanelTab.RestoreState;
var
  flg: Boolean;
  i: Integer;
  pnl: TAdvToolPanel;

begin
  if Persist.Location = pplINIFile then
    flg := INIRestoreState
  else
    flg := REGRestoreState;

  if not flg then
    Exit;

  flg := false;

  for i := 1 to Parent.ControlCount do
  begin
    if Parent.Controls[i - 1] is TAdvToolPanel then
    begin
      pnl := TAdvToolPanel(Parent.Controls[i - 1]);
      if not pnl.Restored then
      begin
        if Assigned(pnl.Tab) then
        begin
          pnl.Tab.RemovePanel(pnl);
        end;
        InsertPanel(pnl);
        pnl.Tab := self;
        flg := true;
      end;
    end;
  end;

  FRestored := true;

  if flg then
  begin
    if Persist.Location = pplINIFile then
      INIRestoreState
    else
      REGRestoreState;

    for i := 1 to Parent.ControlCount do
    begin
      if (Parent.Controls[i - 1] is TAdvToolPanelTab) and (Parent.Controls[i - 1] <> Self) then
      begin
        if not TAdvToolPanelTab(Parent.Controls[i - 1]).Restored then
          TAdvToolPanelTab(Parent.Controls[i - 1]).RestoreState;
      end;
    end;

  end;
end;

procedure TAdvToolPanelTab.SaveState;
begin
  if Persist.Location = pplINIFile then
    INISaveState
  else
    REGSaveState;

  FRestored := false; 
end;

function TAdvToolPanelTab.REGRestoreState: Boolean;
var
  i, j, k: integer;
  INIFile: TRegINIFile;
  pnl, name: string;
  APanel: TAdvToolPanel;
  r: TRect;
  vis, lck: Boolean;
begin
  Result := False;

  if Persist.RegistryUserKey = '' then
    Exit;

  INIFIle := TRegINIFile.Create(Persist.RegistryUserKey);

  i := INIFile.ReadInteger(Self.Name + 'PANELS', 'COUNT', 0);

  if i = 0 then
    Exit;

  FRollOutPanel := -1;

  for j := 1 to i do
  begin
    pnl := Self.Name + 'PANEL' + IntToStr(j);

    name := INIFile.ReadString(pnl, 'NAME', '');

    APanel := PanelByName(name);

    if Assigned(APanel) and not APanel.Restored then
    begin
      APanel.Restored := True;

      k := PanelIndex(APanel);
      Panels[k].OpenWidth := inifile.ReadInteger(pnl, 'OPENWIDTH', 0);

      APanel.Docking := inifile.ReadInteger(pnl, 'DOCKED', 0) = 1;
      vis := inifile.ReadInteger(pnl, 'VISIBLE', 0) = 1;
      APanel.Hidden := inifile.ReadInteger(pnl, 'HIDDEN', 0) = 1;
      lck := inifile.ReadInteger(pnl, 'LOCKED', 0) = 1;

      if lck then
        APanel.OpenWidth := inifile.ReadInteger(pnl, 'WIDTH', 0)
      else
        APanel.OpenWidth := inifile.ReadInteger(pnl, 'OPENWIDTH', 0);

      //if {lck and (APanel.Locked <> lck) and} not APanel.Hidden and not APanel.Docking  then
      if lck and (APanel.Locked <> lck) and not APanel.Hidden and not APanel.Docking  then
      begin
        if Position = ppLeft then
          APanel.Left := FTabWidth;

        APanel.Align := alNone;
        if ((Position = ppLeft) or (Position = ppRight)) then
        begin
          APanel.Top := Top;
          APanel.Height := Height;
          APanel.Width := APanel.OpenWidth;
        end
        else {((Position = ppTop) or (Position = ppBottom))}
        begin
          APanel.Top := FTabWidth;
          APanel.Height := APanel.OpenWidth;
          APanel.Width := Width;
        end;
        APanel.Visible := True;
        APanel.Locked := True;
      end;

      if not lck and (APanel.Locked <> lck) and not APanel.Hidden and not APanel.Docking then
      begin
        APanel.Locked := False;
        APanel.Visible := False;
      end;

      if APanel.Docking and vis then
      begin
        r.Left := inifile.ReadInteger(pnl, 'LEFT', 0);
        r.Right := r.Left + inifile.ReadInteger(pnl, 'WIDTH', 0);
        r.Top := inifile.ReadInteger(pnl, 'TOP', 0);
        r.Bottom := r.Top + inifile.ReadInteger(pnl, 'HEIGHT', 0);

        APanel.ManualDock(nil,nil,alClient);
        APanel.HostDockSite.Visible := false;
        APanel.HostDockSite.Left := r.Left;
        APanel.HostDockSite.Top := r.Top;
        APanel.HostDockSite.Width := r.Right - r.Left;
        APanel.HostDockSite.Height := r.Bottom - r.Top;
        APanel.HostDockSite.Visible := true;
      end;

    end;
  end;

  if PanelsInTab = 0 then
    Width := 2
  else
    Width := FTabWidth;

  FRollOutPanel := -1;
  INIFile.Free;
  Result := True;
end;

procedure TAdvToolPanelTab.REGSaveState;
var
  i: integer;
  INIFile: TRegINIFile;
  pnl: string;
begin
  if Persist.RegistryUserKey = '' then
    Exit;

  INIFIle := TRegINIFile.Create(Persist.RegistryUserKey);

  inifile.writeinteger(Name + 'PANELS', 'COUNT', Panels.Count);
  for i := 1 to Panels.Count do
  begin
    pnl := Name + 'PANEL' + IntToStr(i);
    inifile.writestring(pnl, 'NAME', Panels[i - 1].Panel.Name);
    inifile.WriteInteger(pnl, 'OPENWIDTH', Panels[i - 1].OpenWidth);
    if Panels[i - 1].Panel.Locked then
      inifile.WriteInteger(pnl, 'LOCKED', 1)
    else
      inifile.WriteInteger(pnl, 'LOCKED', 0);

    if Panels[i - 1].Panel.Hidden then
      inifile.WriteInteger(pnl, 'HIDDEN', 1)
    else
      inifile.WriteInteger(pnl, 'HIDDEN', 0);

    if Panels[i - 1].Panel.Docking then
      inifile.WriteInteger(pnl, 'DOCKED', 1)
    else
      inifile.WriteInteger(pnl, 'DOCKED', 0);

    if Panels[i - 1].Panel.Visible then
      inifile.WriteInteger(pnl, 'VISIBLE', 1)
    else
      inifile.WriteInteger(pnl, 'VISIBLE', 0);

    if Panels[i - 1].Panel.Floating then
    begin
      inifile.WriteInteger(pnl, 'LEFT', Panels[i - 1].Panel.Parent.Left);
      inifile.WriteInteger(pnl, 'TOP', Panels[i - 1].Panel.Parent.Top);
      inifile.WriteInteger(pnl, 'WIDTH', Panels[i - 1].Panel.Parent.Width);
      inifile.WriteInteger(pnl, 'HEIGHT', Panels[i - 1].Panel.Parent.Height);
    end
    else
    begin
      inifile.WriteInteger(pnl, 'LEFT', Panels[i - 1].Panel.Left);
      inifile.WriteInteger(pnl, 'TOP', Panels[i - 1].Panel.Top);
      if Panels[i - 1].Panel.Locked then
      begin
        if ((Position = ppLeft) or (Position = ppRight)) then
          inifile.WriteInteger(pnl, 'WIDTH', Panels[i - 1].Panel.Width)
        else {((Position = ppTop) or (Position = ppBottom))}
          inifile.WriteInteger(pnl, 'WIDTH', Panels[i - 1].Panel.Height);
      end
      else
        inifile.WriteInteger(pnl, 'WIDTH', Panels[i - 1].Panel.OpenWidth);
    end;
  end;

  INIFile.Free;
end;

function TAdvToolPanelTab.NumPanelsLocked: Integer;
var
  i: Integer;
  LC: Integer;
begin
  LC := 0;
  for i := 1 to Panels.Count do
  begin
    if Panels.Items[i - 1].Panel.Locked then
      inc(LC);
  end;
  Result := LC;
end;

procedure TAdvToolPanelTab.TabCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  HidePanels;
  if Assigned(FCQEvent) then
    FCQEvent(Sender,CanClose);
end;

procedure TAdvToolPanelTab.TabFormShow(Sender: TObject);
begin
  if Persist.AutoLoad then
    RestoreState;
  if Assigned(FFSEvent) then
    FFSEvent(Sender);
end;

procedure TAdvToolPanelTab.ThemeAdapt;
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

procedure TAdvToolPanelTab.SetComponentStyle(AStyle: TTMSStyle);
begin
  Style := TToolPanelStyle(AStyle);
end;

procedure TAdvToolPanelTab.SetComponentStyleAndAppColor(AStyle: TTMSStyle;
  AppColor: TColor);
begin
  SetStyleAndAppColor(TToolPanelStyle(AStyle), AppColor);
end;

procedure TAdvToolPanelTab.SetStyle(const Value: TToolPanelStyle);
begin
  SetStyleAndAppColor(Value, clBlack);
end;

procedure TAdvToolPanelTab.SetStyleAndAppColor(const Value: TToolPanelStyle; AppColor: TColor);
var
  i: integer;
  //CaptionClr, CaptionClrTo, CaptionFontClr, HoverBtnColor: TColor;
  //HoverBtnColorTo, DownBtnColor, DownColorTo, SectionColor, SectionColorTo: TColor;
  //SectionBkgColor, SectionBkgColorTo, PanelClr, PanelClrTo, FocusCaptionClr, FocusCaptionClrTo: TColor;
  //PanelGdDir, SectGdDir, SectBkgGdDir, CaptionGdDir: TGradientDirection;
  //SectItemTextClr, SectItemHoverTextClr, SectBorderColor, SectCaptionFontColor, FocusFontClr: TColor;
  //ItemUnderLine: Boolean;
  //SectBorderWidth: integer;

begin
  if (FStyle <> Value) or (1 > 0) then
  begin
    FStyle := Value;
    case FStyle of
      esOffice2003Blue:
        begin
          Font.Color := clBlack;

          TabColor := $FCE1CB;
          TabColorTo := $E0A57D;

          TabHoverColor := $DCFFFF;
          TabHoverColorTo := $5BC0F7;

          TabBorderColor := $962D00;

          Color := clWhite;
          ColorTo := $00F7EFDE;
        end;
      esOffice2003Olive:
        begin
          Font.Color := clBlack;

          TabColor := $CFF0EA;
          TabColorTo := $8CC0B1;

          TabHoverColor := $DCFFFF;
          TabHoverColorTo := $5BC0F7;

          TabBorderColor := $588060;

          Color := clWhite;
          ColorTo := $00CEE7E7;

        end;
      esOffice2003Silver:
        begin
          Font.Color := clBlack;

          TabColor := $ECE2E1;
          TabColorTo := $B39698;

          TabHoverColor := $DCFFFF;
          TabHoverColorTo := $5BC0F7;

          TabBorderColor := $947C7C;

          Color := clWhite;
          ColorTo := $00EFE7E7;
        end;
      esOffice2003Classic:
        begin
          Font.Color := clBlack;

          TabColor := clWhite;
          TabColorTo := $C9D1D5;

          TabHoverColor := $D2BDB6;
          TabHoverColorTo := $D2BDB6;

          TabBorderColor := $808080;

          Color := clWhite;
          ColorTo := $00ECEFF0;
        end;
     esOffice2007Luna:
        begin
          Font.Color := $723708;

          TabColor := $FFEFE3;
          TabColorTo := $FFD2AF;

          TabHoverColor := $DCFFFF;
          TabHoverColorTo := $5BC0F7;

          TabBorderColor := $FFD2AF;

          Color := $FFF4E3;
          ColorTo := $EDD9C8;
        end;
     esOffice2007Obsidian:
        begin
          Font.Color := $433C37;

          TabColor := $F2F1F0;
          TabColorTo := $C9C2BD;

          TabHoverColor := $DCFFFF;
          TabHoverColorTo := $5BC0F7;

          TabBorderColor := $5C534C;

          Color := $F1F0E6;
          ColorTo := $C6BCB5;
        end;
     esOffice2007Silver:
        begin
          Font.Color := $723708;

          TabColor := $F8F7F6;
          TabColorTo := $E8E0DB;

          TabHoverColor := $DCFFFF;
          TabHoverColorTo := $5BC0F7;

          TabBorderColor := $74706F;

          Color := clWhite;
          ColorTo := $DDD4D0;
        end;
     esWindowsXP:
        begin
          Font.Color := clWhite;

          TabColor := clInactiveCaption;
          TabColorTo := clInactiveCaption;

          TabHoverColor := clHighLight;
          TabHoverColorTo := clHighlight;

          TabBorderColor := clBlack;

          Color := clBtnFace;
          ColorTo := clBtnFace;
        end;
      esWhidbey:
        begin
          Font.Color := clBlack;

          TabColor := $EBEEEF;//clWhite;
          TabColorTo := $7E9898;//$C9D1D5;

          TabHoverColor := $DCFFFF;//$D2BDB6;
          TabHoverColorTo := $5BC0F7;//$D2BDB6;

          TabBorderColor := $962D00;//$808080;

          Color := TabColor;//$EBEEEF;//clWhite;
          ColorTo := TabColorTo; //$A8C0C0;//$00ECEFF0;

          AutoThemeAdapt := false;
        end;
      esTerminal:
        begin
          Font.Color := clBlack;

          TabColor := clBtnFace;
          TabColorTo := clNone;

          TabHoverColor := clWhite;
          TabHoverColorTo := clWhite;

          TabBorderColor := clBlack;//$808080;

          Color := TabColor;//$EBEEEF;//clWhite;
          ColorTo := TabColorTo; //$A8C0C0;//$00ECEFF0;

          AutoThemeAdapt := false;
        end;
      esWindowsVista:
        begin
          Font.Color := clBlack;
          TabColor := $FDF8F1;
          TabColorTo := $FCEFD5;
          TabHoverColor := $FFFDF9;
          TabHoverColorTo := $FFFAF0;
          TabBorderColor := $FDDE99;
          Color := TabColor;
          ColorTo := TabColorTo;
          AutoThemeAdapt := false;
        end;
      esWindows7:
        begin
          Font.Color := clBlack;
          TabColor := $FCEBDC;
          TabColorTo := $FCDBC1;
          TabHoverColor := $FDFBFA;
          TabHoverColorTo := $FDF3EB;
          TabBorderColor := $CEA27D;
          Color := TabColor;
          ColorTo := TabColorTo;
          AutoThemeAdapt := false;
        end;
      esWindows8, esWindows10:
        begin
          Font.Color := clBlack;
          TabColor := $F7F6F5;
          TabColorTo := $F7F6F5;
          TabHoverColor := $F7EFE8;
          TabHoverColorTo := $F7EFE8;
          TabBorderColor := $F9CEA4;
          Color := $F7F6F5;
          ColorTo := $F7F6F5;
          AutoThemeAdapt := false;
        end;

      esOffice2013White:
        begin
          Font.Color := clBlack;
          TabColor := clWhite;
          TabColorTo := clWhite;
          TabHoverColor := $00F7E6CD;
          TabHoverColorTo := $00F7E6CD;
          TabBorderColor := $00E7BE7A;
          Color := clWhite;
          ColorTo := clWhite;
          AutoThemeAdapt := false;
        end;
      esOffice2013LightGray:
        begin
          Font.Color := clBlack;
          TabColor := clWhite;
          TabColorTo := clWhite;
          TabHoverColor := $00F7E6CD;
          TabHoverColorTo := $00F7E6CD;
          TabBorderColor := $00E7BE7A;
          Color := clWhite;
          ColorTo := clWhite;
          AutoThemeAdapt := false;
        end;
      esOffice2013Gray:
        begin
          Font.Color := clBlack;
          TabColor := $FAFAFA;
          TabColorTo := $FAFAFA;
          TabHoverColor := $00F7E6CD;
          TabHoverColorTo := $00F7E6CD;
          TabBorderColor := $00E7BE7A;
          Color := $FAFAFA;
          ColorTo := $FAFAFA;
          AutoThemeAdapt := false;
        end;

      esOffice2016White:
        begin
          Font.Color := $444444;
          TabColor := clWhite;
          TabColorTo := clWhite;
          TabHoverColor := clWhite;
          TabHoverColorTo := clWhite;
          TabBorderColor := $D4D4D4;
          Color := clWhite;
          ColorTo := clWhite;
          AutoThemeAdapt := false;
        end;
      esOffice2016Gray:
        begin
          Font.Color := $F0F0F0;
          TabColor := $444444;
          TabColorTo := $444444;
          TabHoverColor := $454545;
          TabHoverColorTo := $454545;
          TabBorderColor := $B2B2B2;
          Color := $444444;
          ColorTo := $444444;
          AutoThemeAdapt := false;
        end;
      esOffice2016Black:
        begin
          Font.Color := $DADADA;
          TabColor := $252525;
          TabColorTo := $252525;
          TabHoverColor := $262626;
          TabHoverColorTo := $262626;
          TabBorderColor := $4D4D4D;
          Color := $252525;
          ColorTo := $252525;
          AutoThemeAdapt := false;
        end;

        esOffice2010Blue:
        begin
          Font.Color := $8B4215;

          TabColor := $FFEFE3;
          TabColorTo := $FFD1AD;

          TabHoverColor := $D9F9FD;
          TabHoverColorTo := $8AE3FD;

          TabBorderColor := $FFD1AD;

          Color := TabColor;
          ColorTo := TabColorTo;

          AutoThemeAdapt := false;
        end;
        esOffice2010Silver:
        begin
         Font.Color := $8B4215;

          TabColor := $F8F7F6;
          TabColorTo := $B5ACA5;

          TabHoverColor := $D9F9FD;
          TabHoverColorTo := $8AE3FD;

          TabBorderColor := $C7C7C5;

          Color := TabColor;
          ColorTo := TabColorTo;

          AutoThemeAdapt := false;
        end;
        esOffice2010Black:
        begin
      Font.Color := $8B4215;

          TabColor := $F2F1F0;
          TabColorTo := $C7C0BB;

          TabHoverColor := $D9F9FD;
          TabHoverColorTo := $8AE3FD;

          TabBorderColor := $D1CBC7;

          Color := TabColor;
          ColorTo := TabColorTo;

          AutoThemeAdapt := false;
        end;
      esCustom:
        begin
          AutoThemeAdapt := false;
        end;
    end;

    if (csDesigning in ComponentState) then
    begin
      for i := 1 to ControlCount do
      begin
        TAdvToolPanel(Controls[i - 1]).Style := FStyle;
      end;

      if ControlCount > 0 then
      begin
        if FDesignViewPanel < 0 then
          FDesignViewPanel := 0;
        UpdatePanels(FDesignViewPanel);
        Controls[FDesignViewPanel].Top := Controls[FDesignViewPanel].Top - Controls[FDesignViewPanel].Height;
        Controls[FDesignViewPanel].Top := Controls[FDesignViewPanel].Top + Controls[FDesignViewPanel].Height;
      end;
    end
    else
    begin
      for i := 1 to Panels.Count do
      begin
        Panels[i - 1].Panel.Style := FStyle;
      end;
    end;
  end;
end;

{ TToolPanel }

procedure TToolPanel.Assign(Source: TPersistent);
begin
  if (Source is TToolPanel) then
  begin
    FTag := (Source as TToolPanel).Tag;
    FCaption := (Source as TToolPanel).Caption;
    FVisible := (Source as TToolPanel).Visible;
    FImageIndex := (Source as TToolPanel).ImageIndex;
    FState := (Source as TToolPanel).State;
    FOpenWidth := (Source as TToolPanel).OpenWidth;
    FPanel := (Source as TToolPanel).Panel;
  end;

end;

constructor TToolPanel.Create(Collection: TCollection);
begin
  inherited;
end;

destructor TToolPanel.Destroy;
begin
  inherited;
end;

procedure TToolPanel.SetCaption(const Value: string);
begin
  FCaption := Value;
end;

procedure TToolPanel.SetImageIndex(const Value: Integer);
begin
  FImageIndex := Value;
end;

procedure TToolPanel.SetOpenWidth(const Value: Integer);
begin
  if Value > 0 then
    FOpenWidth := Value;
end;

procedure TToolPanel.SetState(const Value: TPanelState);
begin
  FState := Value;
end;

procedure TToolPanel.SetVisible(const Value: Boolean);
begin
  FVisible := Value;
end;


{ TAdvToolPanelSection }

procedure TAdvToolPanelSection.Assign(Source: TPersistent);
begin
  if Source is TAdvToolPanelSection then
  begin
    FAutosize := (Source as TAdvToolPanelSection).Autosize;
    FCaption := (Source as TAdvToolPanelSection).Caption;
    FCaptionImageIndex := (Source as TAdvToolPanelSection).CaptionImageIndex;
    FDraw := (Source as TAdvToolPanelSection).Draw;
    FHeight := (Source as TAdvToolPanelSection).Height;
    FNode := (Source as TAdvToolPanelSection).Node;
    FSectionItems.Assign((Source as TAdvToolPanelSection).SectionItems);
  end;
end;

constructor TAdvToolPanelSection.Create(Collection: TCollection);
begin
  inherited;
  FHeight := 48;
  FActHeight := 48;
  FNode := false;
  FExpanded := true;
  FCaptionImageIndex := -1;
  Caption := 'Untitled';
  FDraw := true;

  FAutosize := false;
  FSectionItemHeight := 0;
  FInternalCall := false;
  FControlList := TList.Create;

{$IFDEF DELPHI6_LVL}
  FSectionItems := TSectionItems.Create(TAdvToolPanel(Collection.Owner));
  TAdvToolPanel(Collection.Owner).Invalidate;
{$ELSE}
  FSectionItems := TSectionItems.Create(TAdvToolPanelSections(Collection).FOwner);
  TAdvToolPanelSections(Collection).FOwner.Invalidate;
{$ENDIF}
  FSectionItems.AdvToolPanelSection := self;
  FSectionItems.OnItemAdd := OnSectionItemsAdd;
  FSectionItems.OnItemDelete := OnSectionItemsDelete;
end;

destructor TAdvToolPanelSection.Destroy;
begin
  FSectionItems.Free;
  FControlList.Free;
{$IFDEF DELPHI6_LVL}
  TAdvToolPanel(Collection.Owner).Invalidate;
{$ELSE}
  TAdvToolPanelSections(Collection).FOwner.Invalidate;
{$ENDIF}
  inherited;
end;


function TAdvToolPanelSection.GetDisplayName: string;
begin
  Result := 'Section ' + IntToStr(Index) + ': ' + Caption;
end;

procedure TAdvToolPanelSection.OnSectionItemsAdd(Sender: TObject);
begin
  if FAutosize then
  begin
    FInternalCall := true;
    Height := Height + FSectionItemHeight;
    FInternalCall := false;
  end;
end;

procedure TAdvToolPanelSection.OnSectionItemsDelete(Sender: TObject);
var
  ch: integer;
begin
  if FAutosize then
  begin
    FInternalCall := true;

    ch := (Collection.Owner as TAdvToolPanel).CaptionHeight;

    if (Height - FSectionItemHeight) >= (ch + 2) then
      Height := Height - FSectionItemHeight;
    FInternalCall := false;
  end;
end;

procedure TAdvToolPanelSection.SetAutosize(const Value: boolean);
var
  i, fh, a: integer;
begin
{$IFDEF DELPHI6_LVL}
  if not (csLoading in TAdvToolPanel(Collection.Owner).ComponentState) {true FAutosize <> Value} then
{$ENDIF}
{$IFNDEF DELPHI6_LVL}
    if not (csLoading in TAdvToolPanelSections(Collection).FOwner.ComponentState) {true FAutosize <> Value} then
{$ENDIF}
    begin
      FAutosize := Value;
      if FAutosize then
      begin
{$IFDEF DELPHI6_LVL}
        TAdvToolPanel(Collection.Owner).Canvas.Font.Assign(TAdvToolPanel(Collection.Owner).Font);
        fh := TAdvToolPanel(Collection.Owner).Canvas.TextHeight('gh');
        a := TAdvToolPanel(Collection.Owner).SectionLayout.Spacing;
{$ELSE}
        TAdvToolPanelSections(Collection).FOwner.Canvas.Font.Assign(TAdvToolPanelSections(Collection).FOwner.Font);
        fh := TAdvToolPanelSections(Collection).FOwner.Canvas.TextHeight('gh');
        a := TAdvToolPanelSections(Collection).FOwner.SectionLayout.Spacing;
{$ENDIF}
        for i := 0 to SectionItems.Count - 1 do
        begin
          if SectionItems.Items[i].ImageIndex >= 0 then
          begin
{$IFDEF DELPHI6_LVL}
            if Assigned(TAdvToolPanel(Collection.Owner).SectionImages) then
              fh := max(fh, TAdvToolPanel(Collection.Owner).SectionImages.Height + 2);
{$ELSE}
            if Assigned(TAdvToolPanelSections(Collection).FOwner.SectionImages) then
              fh := max(fh, TAdvToolPanelSections(Collection).FOwner.SectionImages.Height + 2);
{$ENDIF}
            break;
          end;
        end;
        FSectionItemHeight := fh;
        FInternalCall := true;
        Height := FSectionItemHeight * SectionItems.Count + TAdvToolPanelSections(Collection).FOwner.SectionHeight + 6 + a;
        FInternalCall := false;
      end;
    //OnSectionItemsCountChanged(self);
{$IFDEF DELPHI6_LVL}
      TAdvToolPanel(Collection.Owner).Invalidate;
{$ELSE}
      TAdvToolPanelSections(Collection).FOwner.Invalidate;
{$ENDIF}
    end;
end;

procedure TAdvToolPanelSection.SetCaption(const Value: string);
begin
  if (FCaption <> Value) then
  begin
    FCaption := Value;
{$IFDEF DELPHI6_LVL}
    TAdvToolPanel(Collection.Owner).Invalidate;
{$ELSE}
    TAdvToolPanelSections(Collection).FOwner.Invalidate;
{$ENDIF}
  end;
end;

procedure TAdvToolPanelSection.SetCaptionImageIndex(const Value: integer);
begin
  if FCaptionImageIndex <> Value then
  begin
    FCaptionImageIndex := Value;
{$IFDEF DELPHI6_LVL}
    TAdvToolPanel(Collection.Owner).Invalidate;
{$ELSE}
    TAdvToolPanelSections(Collection).FOwner.Invalidate;
{$ENDIF}
  end;
end;

procedure TAdvToolPanelSection.SetDraw(const Value: boolean);
begin
  if FDraw <> Value then
  begin
    FDraw := Value;
{$IFDEF DELPHI6_LVL}
    TAdvToolPanel(Collection.Owner).Invalidate;
{$ELSE}
    TAdvToolPanelSections(Collection).FOwner.Invalidate;
{$ENDIF}
  end;
end;

procedure TAdvToolPanelSection.SetExpanded(const Value: Boolean);
var
  df: integer;
begin
  if (FExpanded <> Value) and (FNode) then
  begin
    FExpanded := Value;
    if FExpanded then // To be Expand
    begin
      df := FActHeight - FHeight;
      FHeight := FActHeight;
{$IFDEF DELPHI6_LVL}
      TAdvToolPanel(Collection.Owner).UpdateControlPos(Index, df);
{$ELSE}
      TAdvToolPanelSections(Collection).FOwner.UpdateControlPos(Index, df);
{$ENDIF}
    end
    else //  to be collapsed
    begin
      FActHeight := FHeight;
      //df := SectionHeight - FActHeight;

{$IFDEF DELPHI6_LVL}
      df := TAdvToolPanel(Collection.Owner).SectionHeight + TAdvToolPanel(Collection.Owner).SectionLayout.Spacing - FActHeight;
      TAdvToolPanel(Collection.Owner).UpdateControlPos(Index, df);
      FHeight := TAdvToolPanel(Collection.Owner).SectionHeight + TAdvToolPanel(Collection.Owner).SectionLayout.Spacing;
{$ELSE}
      df := TAdvToolPanelSections(Collection).FOwner.SectionHeight + TAdvToolPanelSections(Collection).FOwner.SectionLayout.Spacing - FActHeight;
      TAdvToolPanelSections(Collection).FOwner.UpdateControlPos(Index, df);
      FHeight := TAdvToolPanelSections(Collection).FOwner.SectionHeight + TAdvToolPanelSections(Collection).FOwner.SectionLayout.Spacing;
{$ENDIF}
      //FHeight := SectionHeight;
    end;
{$IFDEF DELPHI6_LVL}
    TAdvToolPanel(Collection.Owner).invalidate;
{$ELSE}
    TAdvToolPanelSections(Collection).FOwner.invalidate;
{$ENDIF}
  end;
end;

procedure TAdvToolPanelSection.SetHeight(const Value: Integer);
var
  df: integer;
begin
  if (FHeight <> Value) and ((not FAutosize) or FInternalCall) then
  begin
    df := Value - FHeight;
{$IFDEF DELPHI6_LVL}
    TAdvToolPanel(Collection.Owner).UpdateControlPosBelowSection(Index, df);
{$ELSE}
    TAdvToolPanelSections(Collection).FOwner.UpdateControlPosBelowSection(Index, df);
{$ENDIF}

    FHeight := Value;
{$IFDEF DELPHI6_LVL}
    TAdvToolPanel(Collection.Owner).Invalidate;
{$ELSE}
    TAdvToolPanelSections(Collection).FOwner.Invalidate;
{$ENDIF}
  end;
end;

procedure TAdvToolPanelSection.SetNode(const Value: Boolean);
begin
  if FNode <> Value then
  begin
    FNode := Value;
{$IFDEF DELPHI6_LVL}
    TAdvToolPanel(Collection.Owner).Invalidate;
{$ELSE}
    TAdvToolPanelSections(Collection).FOwner.Invalidate;
{$ENDIF}
  end;
end;

{ TAdvToolPanelSections }

function TAdvToolPanelSections.Add: TAdvToolPanelSection;
begin
  Result := TAdvToolPanelSection(inherited Add);
end;

constructor TAdvToolPanelSections.Create(AOwner: TAdvToolPanel);
begin
  inherited Create(TAdvToolPanelSection);
  FOwner := AOwner;
end;

function TAdvToolPanelSections.GetItem(
  Index: Integer): TAdvToolPanelSection;
begin
  Result := TAdvToolPanelSection(inherited Items[Index]);
end;

function TAdvToolPanelSections.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TAdvToolPanelSections.Insert(
  Index: Integer): TAdvToolPanelSection;
begin
  Result := TAdvToolPanelSection(inherited Insert(Index));
end;

procedure TAdvToolPanelSections.SetItem(Index: Integer;
  const Value: TAdvToolPanelSection);
begin
  inherited Items[Index] := Value;
end;


{ TAdvToolPanel }

procedure TAdvToolPanel.AlignControls(AControl: TControl; var Rect: TRect);
begin
  if ShowCaption then
  begin
    if (Rect.Top < FCaptionHeight) and not Floating then
      Rect.Top := FCaptionHeight;
  end;
  inherited;
end;

procedure TAdvToolPanel.CaptionButtonClick(CaptionBtnRect: TRect);
var
  p: TPoint;
begin
  CaptionBtnRect.Bottom := CaptionBtnRect.Bottom - 2;
  if Assigned(PopupMenu) then
  begin
    P := ClientToScreen(Point(CaptionBtnRect.Left, CaptionBtnRect.Bottom));
    PopupMenu.Popup(P.X, P.Y);
  end
  else
    if Assigned(FOnCaptionBtnClick) then
      FOnCaptionBtnClick(self, CaptionBtnRect);
end;

function TAdvToolPanel.GetSectionHeight: Integer;
var
  i: Integer;
begin
  Result := SECTION_HEIGHT;
  for i := 1 to Sections.Count do
  begin
    if Assigned(SectionImages) and (TAdvToolPanelSection(Sections.Items[i - 1]).CaptionImageIndex > -1) then
    begin
      Result := Max(Result, SectionImages.Height+2);
    end;
  end;
end;

procedure TAdvToolPanel.SetSectionImages(Value: TImageList);
begin
  FSectionImages := Value;
  Invalidate;
end;

procedure TAdvToolPanel.CMDesignHitTest(var Msg: TCMDesignHitTest);
var
  P: TPoint;
  i, h: Integer;
begin
  inherited;

  if (csDesigning in ComponentState) and FSizing then
  begin
    Msg.Result := 1;
    Exit;
  end;

  if (csDesigning in ComponentState) then
  begin
    GetCursorPos(P);
    P := ScreenToClient(P);

    if Floating or not FShowCaption then
      h := 1
    else
      h := FCaptionHeight;

    for i := 1 to Sections.Count do
    begin
      if (P.Y > h) and (P.Y < h + SectionHeight) then
      begin
        if (i > 1) then
        begin
          Msg.Result := 1;
          Exit;
        end;
      end;
      h := h + TAdvToolPanelSection(Sections.Items[i - 1]).Height;
    end;
  end;
end;

procedure TAdvToolPanel.SetTopMost;
begin
  SetZOrder(true);
end;


procedure TAdvToolPanel.CMHintShow(var Msg: TMessage);
var
  CanShow: Boolean;
  hi: PHintInfo;
  r, r1, r2, SecItmRct: TRect;
  SecItemIndex, SecIndex: integer;
begin
  CanShow := True;
  hi := PHintInfo(Msg.LParam);

  r := ClientRect;

  if ShowClose then
    r2 := Rect(r.Right - 42, r.Top + 2, r.Right - 23, r.Top + 21)
  else
    r2 := Rect(r.Right - 21, r.Top + 2, r.Right - 5, r.Top + 21);

  r1 := Rect(r.Right - 21, r.Top + 2, r.Right - 5, r.Top + 21);

  if PtInRect(r1, hi.CursorPos) and ShowClose then
  begin
    hi.HintStr := CloseHint;
  end;

  if PtInRect(r2, hi.CursorPos) and ShowLock then
  begin
    if Locked then
      hi.HintStr := UnlockHint
    else
      hi.HintStr := LockHint;
  end;

  SecItemIndex := PtOnAnyItem(hi.CursorPos.X, hi.CursorPos.Y, SecIndex, SecItmRct);
  if (SecItemIndex >= 0) and (SecIndex >= 0) then
  begin
    hi.HintStr := TAdvToolPanelSection(Sections.Items[SecIndex]).SectionItems.Items[SecItemIndex].Hint;
  end;

  Msg.Result := Ord(not CanShow);
end;

procedure TAdvToolPanel.WMMouseLeave(var Message: TMessage);
begin
  inherited;
  if Screen.Cursor = crSizeAll then
    Screen.Cursor := Cursor;
end;

procedure TAdvToolPanel.CMMouseLeave(var Message: TMessage);
var
  r: TRect;
begin
  FMouseDown := False;

  r := ClientRect;
  if CaptionButton then
    r := Rect(r.Left, r.Top, r.Right, r.Top + 21)
  else
    r := Rect(r.Right - 42, r.Top, r.Right, r.Top + 21);
  InvalidateRect(Handle, @r, false);
  inherited;
end;

constructor TAdvToolPanel.Create(AOwner: TComponent);
begin
  inherited;
  FOpenWidth := 150;
  FCanSize := True;
  FLocked := False;
  FHidden := False;
  FAllowDocking := True;
  FImageIndex := -1;
  FCloseGlyph := TBitmap.Create;
  FLockedGlyph := TBitmap.Create;
  FUnlockedGlyph := TBitmap.Create;
  FTab := nil;
  FForceResize := False;
  FCaptionHeight := CAPTION_HEIGHT;

  FLockedGlyph.LoadFromResourceName(Hinstance, 'TMS_LOCKPIN');
  FUnlockedGlyph.LoadFromResourceName(Hinstance, 'TMS_UNLOCKPIN');
  FCloseGlyph.LoadFromResourceName(Hinstance, 'TMS_CLOSEBTN');
  TabStop := True;
  FShowCaption := True;
  FShowLock := True;
  FShowClose := True;
  FCloseHint := 'Close panel';
  FLockHint := 'Lock panel';
  FUnlockHint := 'Unlock panel';
  FFocusCaptionFontColor := clBlack; //clCaptionText;
  FFocusCaptionColor := $94E6FB; //clActiveCaption;
  FNoFocusCaptionFontColor := clWhite; //clWindowText;
  FNoFocusCaptionColor := $D68759; //clBtnFace;

  DoubleBuffered := IsWinVista;

  DragKind := dkDock;
  DragMode := dmManual;

  FFocusCaptionColorTo := $1595EE; //clNone;
  FNoFocusCaptionColorTo := $933803; //clNone;
  FCaptionGradientDirection := gdVertical;
  FShowCaptionBorder := true;
  FDockDots := true; //false;

  FHoverButtonColor := $DCFFFF; //clNone;
  FHoverButtonColorTo := $5BC0F7; //clNone;
  FDownButtonColor := $087FE8; //clNone;
  FDownButtonColorTo := $7CDAF7; //clNone;

  FButton3D := true;

  FSections := TAdvToolPanelSections.Create(Self);
  FSizing := False;
//FSectionColor:= clBtnFace;
  FSectionLayout := TSectionLayout.Create;
  FSectionLayout.OnChange := SectionLayoutChanged;

  Color := $00FFE7D6;
  FColorTo := $00C67B52; //clNone;
  FGradientDirection := gdVertical;

  FHoverSectionIndex := -1; ;
  FHoverSectionItemIndex := -1;
  FHoverSectionItemRect := Rect(-1, -1, -1, -1);
  Width := 180;
  Height := 300;

  Style := esOffice2003Blue;
  FBackGround := TBitMap.Create;
  FBackgroundTransparent := false;
end;

procedure TAdvToolPanel.CreateParams(var Params: TCreateParams);
begin
  inherited;
end;

procedure TAdvToolPanel.CreateWnd;
begin
  inherited;
end;

destructor TAdvToolPanel.Destroy;
begin
  FCloseGlyph.Free;
  FLockedGlyph.Free;
  FUnlockedGlyph.Free;
  FSections.Free;
  FSectionLayout.Free;
  FBackGround.Free;
  inherited;
end;

procedure TAdvToolPanel.DoEndDock(Target: TObject; X, Y: Integer);
var
  diff, i: integer;
begin
  inherited;
{$IFDEF TMSDEBUG}
  outputdebugstring('do end dock');
{$ENDIF}

  if Screen.Cursor = crSizeAll then
    Screen.Cursor := Cursor;

// Setting Controls position as Caption hides and unhides on floating and unfloating
  if FOldFloating <> Floating then
  begin
    diff := FCaptionHeight;
    FOldFloating := Floating;
    if Floating then
    begin
      if ShowCaption then
      begin
        for i := 0 to ControlCount - 1 do
        begin
          Controls[i].Top := Controls[i].Top - diff;
        end;
      end;
    end
    else
    begin
      if ShowCaption then
      begin
        for i := 0 to ControlCount - 1 do
        begin
          Controls[i].Top := Controls[i].Top + diff;
        end;
      end;
      FTab.Panels[FTab.PanelIndex(Self)].State := psOpened;
    end;
  end;

  Tab.RemovePanel(nil);

  DragMode := dmManual;
end;

procedure TAdvToolPanel.DoEnter;
begin
  inherited;
  FActive := True;
  Invalidate;
end;

procedure TAdvToolPanel.DoExit;
begin
  inherited;
  FActive := False;
  Invalidate;
end;

procedure TAdvToolPanel.DoStartDock(var DragObject: TDragObject);
var
  pt: TPoint;
begin
{$IFDEF TMSDEBUG}
  outputdebugstring(pchar('start dock ow change:' + inttostr(width)));
{$ENDIF}
  if ((FTab.Position = ppLeft) or (FTab.Position = ppRight)) then
    OpenWidth := Width
  else {((FTab.Position = ppTop) or (FTab.Position = ppBottom))}
    OpenWidth := Height;

  FTab.Panels[FTab.PanelIndex(Self)].State := psDocked;

  inherited;

//  TForm(Parent).BorderIcons := TForm(Parent).BorderIcons - [biSystemMenu];

  Docking := true;

  GetCursorPos(pt);
  pt := ScreenToClient(pt);

// cancel drag when not started in caption
  if not ((pt.Y < FCaptionHeight) and (pt.X < Width - 40)) then
  begin
    EndDrag(false);
    CancelDrag;
//DragKind := dkDrag;
//DragMode := dmAutomatic;
    Docking := False;
  end;
end;
{
procedure TAdvToolPanel.DrawMonoBitmap(ACanvas: TCanvas; X,Y: Integer; ABitmap: TBitmap; FGColor,
BkColor: TColor);
var
bmp: TBitmap;
i,j: Integer;
begin
bmp := TBitmap.Create;
bmp.Width := 16;
bmp.Height := 16;
if BkColor = clNone then
begin
bmp.TransparentColor:= clWhite;
bmp.Transparent:= true;
end;

if FGColor = clWhite then
FGColor:= $00FCFCFC;

bmp.Canvas.Draw(0,0,ABitmap);

for i := 0 to 15 do
for j := 0 to 15 do
begin
  if (bmp.Canvas.Pixels[i,j] = clWhite) then
  begin
    if (BkColor <> clNone) then
      bmp.Canvas.Pixels[i,j] := BKColor;
  end
  else
    bmp.Canvas.Pixels[i,j] := FGColor;
end;

ACanvas.Draw(X,Y,bmp);
bmp.Free;
end;
}
function TAdvToolPanel.GetCaptionEx: string;
begin
  Result := inherited Caption;
end;

function TAdvToolPanel.GetIsVisible: Boolean;
begin
  Result := not Locked and not Hidden and not Docking;
end;

function TAdvToolPanel.HasFocusControl: Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 1 to ControlCount do
  begin
    if Controls[i - 1] is TWinControl then
    begin
      if GetFocus = (Controls[i - 1] as TWinControl).Handle then
      begin
        Result := True;
        Break;
      end;
    end;
  end;
end;

procedure TAdvToolPanel.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  r: TRect;
  i, h: integer;
begin
  inherited;
  if Floating or not FShowCaption then
    h := 1
  else
    h := FCaptionHeight;

  if (csDesigning in ComponentState) and not FSizing then
  begin
    for i := 1 to Sections.Count do
    begin
      if (Y > h) and (Y < h + SectionHeight) then
      begin
        if (i > 1) then
        begin
          FSizeSection := i - 1;
          FSizeHeight := TAdvToolPanelSection(Sections.Items[FSizeSection - 1]).Height;

          FSizing := true;
          FSizeY := Y;
        end;
        Exit;
      end;
      h := h + TAdvToolPanelSection(Sections.Items[i - 1]).Height;
    end;
  end;

  if Floating or not FShowCaption then
    h := 1
  else
    h := FCaptionHeight;

  if not (csDesigning in ComponentState) then
  begin
    for i := 1 to Sections.Count do
    begin
      if (Y > h) then
      begin
        if PtInRect(NodeBtnRect(i - 1), Point(X, y)) then
        begin
          TAdvToolPanelSection(Sections.Items[i - 1]).Expanded := not TAdvToolPanelSection(Sections.Items[i - 1]).Expanded;
          break;
        end;
      end;
    end;
  end;

  FMouseDown := true;

  SetFocus;
  r := ClientRect;
  if CaptionButton then
    r := Rect(r.left, r.Top, r.Right, r.Top + 21)
  else
    r := Rect(r.Right - 42, r.Top, r.Right, r.Top + 21);
  if PtInRect(r, Point(X, Y)) then
    InvalidateRect(Handle, @r, false);
end;

procedure TAdvToolPanel.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  r, SectItmRct: TRect;
  d, SectIndex, SectItemIndex: integer;
  evtrack: TTrackMouseEvent;
begin
  inherited;


  if (csDesigning in ComponentState) then
  begin
    if FSizing then
    begin
      d := Y - FSizeY;
      if FSizeHeight + d > SectionHeight then
        TAdvToolPanelSection(Sections.Items[FSizeSection - 1]).Height := FSizeHeight + d
      else if TAdvToolPanelSection(Sections.Items[FSizeSection - 1]).Height <> SectionHeight + 1 then
        TAdvToolPanelSection(Sections.Items[FSizeSection - 1]).Height := SectionHeight + 1;
      Exit;
    end;
  end;

  if Assigned(FTab) then
  begin
    if Locked and Tab.AutoDock and AllowDocking and (Y < FCaptionHeight) and not floating and (x > 0) and ((not FDockDots and (x < Width - 40)) or (FDockDots and (x < DOCKDOTS_WIDTH))) then
    begin
      if Tab.Position = ppRight then
      begin
        evtrack.cbSize := SizeOf(TTrackMouseEvent);
        evtrack.dwFlags := TME_LEAVE;
        evtrack.hwndTrack := Handle;
        evtrack.dwHoverTime := 0;
        TrackMouseEvent(evtrack);
      end;
      if Screen.Cursor <> crSizeAll then
        Screen.Cursor := crSizeAll;
    end
    else if Screen.Cursor = crSizeAll then
      Screen.Cursor := Cursor;
  end;

  if FMouseDown and Assigned(FTab) then
  begin
    if Locked and Tab.AutoDock and AllowDocking and (Y < FCaptionHeight) and not floating and ((not FDockDots and (x < Width - 40)) or (FDockDots and (x < DOCKDOTS_WIDTH))) then
    begin
      DragKind := dkDock;
      DragMode := dmAutomatic;
      BeginDrag(true);
    end;
  end;

  r := ClientRect;
  if CaptionButton then
    r := Rect(r.left, r.Top, r.Right, r.Top + 21)
  else
    r := Rect(r.Right - 42, r.Top, r.Right, r.Top + 21);

  if PtInRect(r, Point(X, Y)) then
    InvalidateRect(Handle, @r, false);

  SectItemIndex := PtOnAnyItem(X, Y, SectIndex, SectItmRct);
  if (SectItemIndex >= 0) and (SectItemIndex <> FHoverSectionItemIndex) then
  begin
    if (Screen.Cursor <> crHandPoint) and FSectionLayout.ItemHoverUnderline then
      Screen.Cursor := CrHandPoint;

    if FHoverSectionItemIndex >= 0 then
    begin
      FHoverSectionIndex := -1;
      FHoverSectionItemIndex := -1;
      InvalidateRect(Handle, @FHoverSectionItemRect, false);
    end;
    FHoverSectionIndex := SectIndex;
    FHoverSectionItemIndex := SectItemIndex;
    FHoverSectionItemRect := SectItmRct;
    Application.CancelHint;
    InvalidateRect(Handle, @SectItmRct, false);
  end
  else if (FHoverSectionItemIndex >= 0) and (SectItemIndex < 0) then
  begin
    if Screen.Cursor <> Cursor then
      Screen.Cursor := Cursor;

    FHoverSectionIndex := -1;
    FHoverSectionItemIndex := -1;
    InvalidateRect(Handle, @FHoverSectionItemRect, false);
  end;
end;

procedure TAdvToolPanel.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  r, r2: TRect;
  Allow: Boolean;
  ParentForm: TCustomForm;

begin
  inherited;
  if FSizing then
  begin
    if (csDesigning in ComponentState) and HandleAllocated and
      not (csUpdating in ComponentState) then
    begin
      ParentForm := GetParentForm(Self);
      if Assigned(ParentForm) and Assigned(ParentForm.Designer) then
        ParentForm.Designer.Modified;
    end
  end;

  FSizing := false;
//*******
  if FDocking then
  begin
    CancelDrag;
    FDocking := False;
    if Screen.Cursor = crSizeAll then
      Screen.Cursor := Cursor;
  end;

  if not (csDesigning in ComponentState) then
  begin
    if FMouseDown and Assigned(FTab) then
    begin
      if (FTab.Position = ppLeft) or (FTab.Position = ppRight) then
        OpenWidth := Width
      else {(FTab.Position = ppTop) or (FTab.Position = ppBottom)}
        OpenWidth := Height;

      FMouseDown := false;
      InvalidateRect(Handle, @r, false);

      r := ClientRect;
      r := Rect(r.Right - 16 - 4, r.Top + 2, r.Right - 5, r.Top + 2 + 16);

      if PtInRect(r, Point(X, Y)) and ShowClose and not Floating then
      begin
        Allow := true;

        if Assigned(Tab.OnAllowTabHide) then
          Tab.OnAllowTabHide(Tab, Tab.PanelIndex(Self), Self, Allow);

        if not Allow then
          Exit;

        if Assigned(Tab.OnTabHide) then
          Tab.OnTabHide(Tab, Tab.PanelIndex(Self), Self);

        if Parent = Tab.FPanelForm then
          Tab.FPanelForm.Hide;

        Hidden := True;
        Tab.UpdatePanels(-1);
        Visible := False;
        Exit;
      end;

      r := ClientRect;
      r2 := r;

      if ShowClose then
        r := Rect(r.Right - 40, r.Top + 2, r.Right - 16 - 5, r.Top + 2 + 16)
      else
        r := Rect(r.Right - 16 - 4, r.Top + 2, r.Right - 5, r.Top + 2 + 16);

      if PtInRect(r, Point(X, Y)) and ShowLock and not Floating then
      begin

        Locked := not Locked;

        if Locked and Assigned(FOnLock) then
          FOnLock(Self);

        if not Locked and Assigned(FOnUnlock) then
          FOnUnlock(Self);

        Exit;
      end;

      r2.Right := r.Left;
      if FDockDots then
        r2.Left := r2.Left + DOCKDOTS_WIDTH;
      r2.Bottom := r2.Top + FCaptionHeight;

      if PtInRect(r2, Point(X, Y)) and CaptionButton and not Floating and FShowCaption then
        CaptionButtonClick(r2);

      if (FHoverSectionItemIndex >= 0) and (FHoverSectionIndex >= 0) then
      begin
        if Assigned(FonItemClick) then
          FOnItemClick(self, FHoverSectionIndex, FHoverSectionItemIndex);
      end;

    end;

    if FMouseDown then
    begin
      if (FHoverSectionItemIndex >= 0) and (FHoverSectionIndex >= 0) then
      begin
        if Assigned(FonItemClick) then
          FOnItemClick(self, FHoverSectionIndex, FHoverSectionItemIndex);
      end;

    end;
  end;
end;



function TAdvToolPanel.NodeBtnRect(SectionIndex: integer): TRect;
var
  i, h, ct: integer;
begin
  Result := Rect(-1, -1, -1, -1);
  if (SectionIndex < 0) or (SectionIndex > Sections.Count) then
    Exit;

  if Floating or not FShowCaption then
    h := 1
  else
    h := FCaptionHeight;

  ct := (SectionHeight - NODEBTN_SIZE) div 2;
  for i := 1 to Sections.Count do
  begin
    if (i - 1 = SectionIndex) and (TAdvToolPanelSection(Sections.Items[i - 1]).Node) then
    begin
      Result := Rect(4 + FSectionLayout.Indent, h + ct{2 + 2}, 4 + NODEBTN_SIZE + 1 + FSectionLayout.Indent, h + ct{2 + 2} + NODEBTN_SIZE + 1);
      break;
    end;
    h := h + TAdvToolPanelSection(Sections.Items[i - 1]).Height;
  end;
end;

procedure TAdvToolPanel.SetBackground(const Value: TBitMap);
begin
  FBackGround.Assign(Value);
  Invalidate;
end;

procedure TAdvToolPanel.SetBackGroundPosition(const value: TTPBackgroundPosition);
begin
  FBackGroundPosition := Value;
  Invalidate;
end;

procedure TAdvToolPanel.SetBackgroundTransparent(Value: Boolean);
begin
  if FBackgroundTransparent <> Value then
  begin
    FBackgroundTransparent := Value;
    Invalidate;
  end;
end;

procedure TAdvToolPanel.SetVersion(const Value: string);
begin
end;

function TAdvToolPanel.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function TAdvToolPanel.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;


procedure TAdvToolPanel.SetColorEx(const Value: TColor);
begin
  inherited Color := Value;
end;

function TAdvToolPanel.GetColorEx: TColor;
begin
  Result := inherited Color;
end;

procedure TAdvToolPanel.SetComponentStyle(AStyle: TTMSStyle);
begin
  Style := TToolPanelStyle(AStyle);
end;

procedure TAdvToolPanel.SetComponentStyleAndAppColor(AStyle: TTMSStyle;
  AppColor: TColor);
begin
  SetStyleAndAppColor(TToolPanelStyle(AStyle), AppColor);
end;

procedure TAdvToolPanel.SetStyle(const Value: TToolPanelStyle);
begin
  SetStyleAndAppColor(Value, clBlack);
end;


procedure TAdvToolPanel.SetStyleAndAppColor(const Value: TToolPanelStyle; AppColor: TColor);
begin
  if (FStyle <> Value) or (1 > 0) then
  begin
    FStyle := Value;
    case FStyle of
      esOffice2003Blue:
        begin
          SectionLayout.CaptionColor := $00F7DEC6;
          SectionLayout.CaptionColorTo := $00D68C63;
          SectionLayout.CaptionGradientDir := gdVertical;

          NoFocusCaptionColor := $D68759;
          NoFocusCaptionColorTo := $933803;
          NoFocusCaptionFontColor := clWhite;

          HoverButtonColor := $DCFFFF;
          HoverButtonColorTo := $5BC0F7;

          DownButtonColor := $087FE8;
          DownButtonColorTo := $7CDAF7;

          SectionLayout.BackGroundColor := $00F7EFDE;
          SectionLayout.BackGroundColorTo := $00F7D6BD;

          Color := $00FFE7D6;
          ColorTo := $00C67B52;
          GradientDirection := gdVertical;

          SectionLayout.CaptionGradientDir := gdHorizontal;
          SectionLayout.BackGroundGradientDir := gdVertical;

          SectionLayout.ItemFontColor := $00B53900;
          SectionLayout.ItemHoverTextColor := $00B53900;
          SectionLayout.ItemHoverUnderLine := true;

          SectionLayout.BorderWidth := 1;
          SectionLayout.BorderColor := clWhite;
          SectionLayout.CaptionFontColor := $00842900;

          CaptionGradientDirection := gdVertical;

          FocusCaptionColor := $94E6FB;
          FocusCaptionColorTo := $1595EE;
          FocusCaptionFontColor := clBlack;

          ShowCaptionBorder := false;
          button3d := false;
        end;
      esOffice2003Olive:
        begin
          SectionLayout.CaptionColor := $00ADDED2;
          SectionLayout.CaptionColorTo := $007BB5A5;
          SectionLayout.CaptionGradientDir := gdVertical;

          NoFocusCaptionColor := $00C6EFE7;
          NoFocusCaptionColorTo := $0094BDB5;
          NoFocusCaptionFontColor := clBlack;

          HoverButtonColor := $DCFFFF;
          HoverButtonColorTo := $5BC0F7;

          DownButtonColor := $087FE8;
          DownButtonColorTo := $7CDAF7;

          SectionLayout.BackGroundColor := $00CEE7E7;
          SectionLayout.BackGroundColorTo := $00B5DED6;


          Color := $00DEEFEF;
          ColorTo := $0084ADA5;
          GradientDirection := gdVertical;
          CaptionGradientDirection := gdVertical;

          SectionLayout.CaptionGradientDir := gdHorizontal;
          SectionLayout.BackGroundGradientDir := gdVertical;
          SectionLayout.ItemFontColor := $00B53900;
          SectionLayout.ItemHoverTextColor := $00B53900;
          SectionLayout.ItemHoverUnderLine := true;
          SectionLayout.BorderWidth := 1;
          SectionLayout.BorderColor := clWhite;
          SectionLayout.CaptionFontColor := $00842900;

          FocusCaptionColor := $94E6FB;
          FocusCaptionColorTo := $1595EE;
          FocusCaptionFontColor := clBlack;

          ShowCaptionBorder := false;
          button3d := false;
        end;
      esOffice2003Silver:
        begin
          SectionLayout.CaptionColor := $00DECACE;
          SectionLayout.CaptionColorTo := $00BDADAD;
          SectionLayout.CaptionGradientDir := gdVertical;

          NoFocusCaptionColor := $00EFE7E7;
          NoFocusCaptionColorTo := $00AA8888;
          NoFocusCaptionFontColor := clBlack;

          HoverButtonColor := $DCFFFF;
          HoverButtonColorTo := $5BC0F7;

          DownButtonColor := $087FE8;
          DownButtonColorTo := $7CDAF7;

          SectionLayout.BackGroundColor := $00EFE7E7;
          SectionLayout.BackGroundColorTo := $00E7D6D6;


          Color := $00F7E7E7;
          ColorTo := $00B5A5A5;
          GradientDirection := gdVertical;

          SectionLayout.CaptionGradientDir := gdHorizontal;
          SectionLayout.BackGroundGradientDir := gdVertical;
          SectionLayout.ItemFontColor := $00B53900;
          SectionLayout.ItemHoverTextColor := $00B53900;
          SectionLayout.ItemHoverUnderLine := true;
          SectionLayout.BorderWidth := 1;
          SectionLayout.BorderColor := clWhite;
          SectionLayout.CaptionFontColor := $007B5A5A;

          CaptionGradientDirection := gdVertical;

          FocusCaptionColor := $94E6FB;
          FocusCaptionColorTo := $1595EE;
          FocusCaptionFontColor := clBlack;

          ShowCaptionBorder := false;
          button3d := false;
        end;
      esOffice2003Classic:
        begin
          SectionLayout.CaptionColor := $00E7E7E7;
          SectionLayout.CaptionColorTo := $00DEDEDE;
          SectionLayout.CaptionGradientDir := gdVertical;

          NoFocusCaptionColor := $00EFEFEF;
          NoFocusCaptionColorTo := $00C0C9C9;
          NoFocusCaptionFontColor := clBlack;

          HoverButtonColor := $D2BDB6;
          HoverButtonColorTo := $D2BDB6;

          DownButtonColor := $B59285;
          DownButtonColorTo := $B59285;

          SectionLayout.BackGroundColor := $00F7F7F7;
          SectionLayout.BackGroundColorTo := $00ECEFF0;

          Color := $00E7E7EF;
          ColorTo := $00C6CECE;
          GradientDirection := gdVertical;

          SectionLayout.CaptionGradientDir := gdHorizontal;
          SectionLayout.BackGroundGradientDir := gdVertical;
          SectionLayout.ItemFontColor := clBlue;
          SectionLayout.ItemHoverTextColor := clBlue;
          SectionLayout.ItemHoverUnderLine := true;
          SectionLayout.BorderWidth := 1;
          SectionLayout.BorderColor := clWhite;
          SectionLayout.CaptionFontColor := clBlack;

          CaptionGradientDirection := gdVertical;

          FocusCaptionColor := $00DFDFDF;
          FocusCaptionColorTo := $00AFA7A3;
          FocusCaptionFontColor := clBlack;

          ShowCaptionBorder := false;
          button3d := false;
        end;
     esOffice2007Luna:
        begin
          SectionLayout.CaptionColor :=  $FDD6B7;
          SectionLayout.CaptionColorTo := $C89A76;
          SectionLayout.CaptionGradientDir := gdVertical;

          NoFocusCaptionColor := $FFEFE3;
          NoFocusCaptionColorTo := $FFD2AF;
          NoFocusCaptionFontColor := $723708;

          HoverButtonColor := $DCFFFF;
          HoverButtonColorTo := $5BC0F7;

          DownButtonColor := $087FE8;
          DownButtonColorTo := $7CDAF7;

          SectionLayout.BackGroundColor := $00F7EFDE;
          SectionLayout.BackGroundColorTo := $00F7D6BD;

          Color := $FFF4E3;
          ColorTo := $EDD9C8;
          GradientDirection := gdVertical;

          SectionLayout.BackGroundGradientDir := gdVertical;

          SectionLayout.ItemFontColor := $00B53900;
          SectionLayout.ItemHoverTextColor := $00B53900;
          SectionLayout.ItemHoverUnderLine := true;

          SectionLayout.BorderWidth := 1;
          SectionLayout.BorderColor := $FFD2AF;
          SectionLayout.CaptionFontColor := clBlack;

          CaptionGradientDirection := gdVertical;

          FocusCaptionColor := $BBEEFF;
          FocusCaptionColorTo := $78DAFF;
          FocusCaptionFontColor := $723708;

          ShowCaptionBorder := false;
          button3d := false;
        end;
     esOffice2007Obsidian:
        begin
          SectionLayout.CaptionColor :=  $B8B8B6;
          SectionLayout.CaptionColorTo := $6E6E6D;
          SectionLayout.CaptionGradientDir := gdVertical;

          NoFocusCaptionColor := $F2F1F0;
          NoFocusCaptionColorTo := $C9C2BD;
          NoFocusCaptionFontColor := $433C37;

          HoverButtonColor := $DCFFFF;
          HoverButtonColorTo := $5BC0F7;

          DownButtonColor := $087FE8;
          DownButtonColorTo := $7CDAF7;

          SectionLayout.BackGroundColor := $E6E6DF;
          SectionLayout.BackGroundColorTo := $D7D5CE;

          Color := $F1F0E6;
          ColorTo := $C6BCB5;
          GradientDirection := gdVertical;

          SectionLayout.BackGroundGradientDir := gdVertical;

          SectionLayout.ItemFontColor := $00B53900;
          SectionLayout.ItemHoverTextColor := $00B53900;
          SectionLayout.ItemHoverUnderLine := true;

          SectionLayout.BorderWidth := 1;
          SectionLayout.BorderColor := $5C534C;
          SectionLayout.CaptionFontColor := clWhite;

          CaptionGradientDirection := gdVertical;

          FocusCaptionColor := $BBEEFF;
          FocusCaptionColorTo := $78DAFF;
          FocusCaptionFontColor := clBlack;

          ShowCaptionBorder := false;
          button3d := false;
        end;
     esOffice2007Silver:
        begin
          SectionLayout.CaptionColor :=  $FAEEEB;
          SectionLayout.CaptionColorTo := $E5DBD7;
          SectionLayout.CaptionGradientDir := gdVertical;

          NoFocusCaptionColor := $F8F7F6;
          NoFocusCaptionColorTo := $E8E0DB;
          NoFocusCaptionFontColor := clBlack;

          HoverButtonColor := $DCFFFF;
          HoverButtonColorTo := $5BC0F7;

          DownButtonColor := $087FE8;
          DownButtonColorTo := $7CDAF7;

          SectionLayout.BackGroundColor := $E7DCD5;
          SectionLayout.BackGroundColorTo := $FBFAF0;

          Color := $DEDDDE;
          ColorTo := $F7F3F3;
          GradientDirection := gdVertical;

          SectionLayout.BackGroundGradientDir := gdVertical;

          SectionLayout.ItemFontColor := $00B53900;
          SectionLayout.ItemHoverTextColor := $00B53900;
          SectionLayout.ItemHoverUnderLine := true;

          SectionLayout.BorderWidth := 1;
          SectionLayout.BorderColor := $CCCAC9;
          SectionLayout.CaptionFontColor := clBlack;

          CaptionGradientDirection := gdVertical;

          FocusCaptionColor := $BBEEFF;
          FocusCaptionColorTo := $78DAFF;
          FocusCaptionFontColor := $723708;

          ShowCaptionBorder := false;
          button3d := false;
        end;
      esWindowsXP:
        begin
          SectionLayout.CaptionColor := clWhite;
          SectionLayout.CaptionColorTo := cl3DLight;
          SectionLayout.CaptionGradientDir := gdVertical;

          NoFocusCaptionColor := clInactiveCaption;
          NoFocusCaptionColorTo := clInactiveCaption;
          NoFocusCaptionFontColor := clWhite;

          HoverButtonColor := clHighLight;
          HoverButtonColorTo := clNone;

          DownButtonColor := clInactiveCaption;
          DownButtonColorTo := clNone;

          SectionLayout.BackGroundColor := clBtnFace;
          SectionLayout.BackGroundColorTo := clBtnFace;

          Color := clBtnFace;
          ColorTo := clBtnFace;
          GradientDirection := gdVertical;

          SectionLayout.CaptionGradientDir := gdVertical;
          SectionLayout.BackGroundGradientDir := gdVertical;
          SectionLayout.ItemFontColor := clBlack;
          SectionLayout.ItemHoverTextColor := clBlack;
          SectionLayout.ItemHoverUnderLine := true;
          SectionLayout.BorderWidth := 1;
          SectionLayout.BorderColor := clWhite;
          SectionLayout.CaptionFontColor := clBlack;

          CaptionGradientDirection := gdVertical;

          FocusCaptionColor := clHighLight;
          FocusCaptionColorTo := clHighLight;
          FocusCaptionFontColor := clWhite;

          ShowCaptionBorder := false;
          button3d := false;
        end;
      esWhidbey:
        begin
          SectionLayout.CaptionColor := $EBEEEF;//$00E7E7E7;
          SectionLayout.CaptionColorTo := $A8C0C0;//$00DEDEDE;
          SectionLayout.CaptionGradientDir := gdVertical;

          NoFocusCaptionColor := $EBEEEF;//$00EFEFEF;
          NoFocusCaptionColorTo := $7E9898;//$00C0C9C9;
          NoFocusCaptionFontColor := clBlack;

          HoverButtonColor := $F5F9FA;//$D2BDB6;
          HoverButtonColorTo := $A8C0C0;//$D2BDB6;

          DownButtonColor := $087FE8;//$B59285;
          DownButtonColorTo := $7CDAF7;//$B59285;

          SectionLayout.BackGroundColor := clWhite;//$00F7F7F7;
          SectionLayout.BackGroundColorTo := clNone;//$00ECEFF0;

          Color := $EBEEEF; //$00E7E7EF;
          ColorTo := $7E9898; //$00C6CECE;
          GradientDirection := gdVertical;

          SectionLayout.CaptionGradientDir := gdVertical;//gdHorizontal;
          SectionLayout.BackGroundGradientDir := gdVertical;
          SectionLayout.ItemFontColor := clBlack;//clBlue;
          SectionLayout.ItemHoverTextColor := clBlack;//clBlue;
          SectionLayout.ItemHoverUnderLine := true;
          SectionLayout.BorderWidth := 1;
          SectionLayout.BorderColor := clWhite;
          SectionLayout.CaptionFontColor := clBlack;

          CaptionGradientDirection := gdVertical;

          FocusCaptionColor := $94E6FB;//$00DFDFDF;
          FocusCaptionColorTo := $1595EE;//$00AFA7A3;
          FocusCaptionFontColor := clBlack;

          ShowCaptionBorder := false;
          button3d := false;
        end;
      esTerminal:
        begin
          SectionLayout.CaptionColor := clSilver;
          SectionLayout.CaptionColorTo := clSilver;
          SectionLayout.CaptionGradientDir := gdVertical;

          NoFocusCaptionColor := clSilver;
          NoFocusCaptionColorTo := clNone;
          NoFocusCaptionFontColor := clBlack;

          HoverButtonColor := clBtnFace;
          HoverButtonColorTo := clBtnFace;

          DownButtonColor := clSilver;
          DownButtonColorTo := clSilver;

          SectionLayout.BackGroundColor := clWhite;
          SectionLayout.BackGroundColorTo := clNone;

          Color := clBtnFace;
          ColorTo := clNone;
          GradientDirection := gdVertical;

          SectionLayout.CaptionGradientDir := gdVertical;
          SectionLayout.BackGroundGradientDir := gdVertical;
          SectionLayout.ItemFontColor := clBlack;
          SectionLayout.ItemHoverTextColor := clBlack;
          SectionLayout.ItemHoverUnderLine := true;
          SectionLayout.BorderWidth := 1;
          SectionLayout.BorderColor := clBlack;
          SectionLayout.CaptionFontColor := clBlack;

          CaptionGradientDirection := gdVertical;

          FocusCaptionColor := clGray;
          FocusCaptionColorTo := clNone;
          FocusCaptionFontColor := clWhite;

          ShowCaptionBorder := false;
          button3d := false;

        end;
      esWindowsVista:
        begin
          SectionLayout.CaptionColor := $FFFDF9;
          SectionLayout.CaptionColorTo := $FFFAF0;
          SectionLayout.CaptionGradientDir := gdVertical;

          NoFocusCaptionColor := $FDF8F1;
          NoFocusCaptionColorTo := $FCEFD5;
          NoFocusCaptionFontColor := clBlack;

          HoverButtonColor := $FFFDF9;
          HoverButtonColorTo := $FFFAF0;

          DownButtonColor := $FDF8F1;
          DownButtonColorTo := $FCEFD5;

          SectionLayout.BackGroundColor := $FFFFFF;
          SectionLayout.BackGroundColorTo := $FFFFFF;

          Color := $FDF8F1;
          ColorTo := $FCEFD5;
          GradientDirection := gdVertical;

          SectionLayout.CaptionGradientDir := gdVertical;
          SectionLayout.BackGroundGradientDir := gdVertical;
          SectionLayout.ItemFontColor := clBlack;
          SectionLayout.ItemHoverTextColor := clBlack;
          SectionLayout.ItemHoverUnderLine := true;
          SectionLayout.BorderWidth := 1;
          SectionLayout.BorderColor := $FDDE99;
          SectionLayout.CaptionFontColor := clBlack;

          CaptionGradientDirection := gdVertical;

          FocusCaptionColor := $E4DED6;
          FocusCaptionColorTo := $E3D6BD;
          FocusCaptionFontColor := clBlack;

          ShowCaptionBorder := false;
          button3d := false;

        end;
      esWindows7:
        begin
          SectionLayout.CaptionColor := $FDFBFA;;
          SectionLayout.CaptionColorTo := $FDF3EB;
          SectionLayout.CaptionGradientDir := gdVertical;

          NoFocusCaptionColor := $FCEBDC;;
          NoFocusCaptionColorTo := $FCDBC1;
          NoFocusCaptionFontColor := clBlack;

          HoverButtonColor := $FDFBFA;
          HoverButtonColorTo := $FDF3EB;

          DownButtonColor := $FCEBDC;
          DownButtonColorTo := $FCDBC1;

          SectionLayout.BackGroundColor := $FDFBFA;
          SectionLayout.BackGroundColorTo := $FDF3EB;

          Color := $FDFBFA;
          ColorTo := $FDF3EB;
          GradientDirection := gdVertical;

          SectionLayout.CaptionGradientDir := gdVertical;
          SectionLayout.BackGroundGradientDir := gdVertical;
          SectionLayout.ItemFontColor := clBlack;
          SectionLayout.ItemHoverTextColor := clBlack;
          SectionLayout.ItemHoverUnderLine := true;
          SectionLayout.BorderWidth := 1;
          SectionLayout.BorderColor := $CEA27D;
          SectionLayout.CaptionFontColor := clBlack;

          CaptionGradientDirection := gdVertical;

          FocusCaptionColor := $E3D1C1;
          FocusCaptionColorTo := $E3C2A9;
          FocusCaptionFontColor := clBlack;

          ShowCaptionBorder := false;
          button3d := false;
        end;  
        esOffice2010Blue:
        begin
          SectionLayout.CaptionColor := $FFEEE2;
          SectionLayout.CaptionColorTo := $FFEEE2;
          SectionLayout.CaptionGradientDir := gdVertical;

          NoFocusCaptionColor := $FFEFE3;
          NoFocusCaptionColorTo := $FFD1AD;
          NoFocusCaptionFontColor := $8B4215;

          HoverButtonColor := $D9F9FD;
          HoverButtonColorTo := $8AE3FD;

          DownButtonColor := $7BEEFF;
          DownButtonColorTo := $6CD0FF;

          SectionLayout.BackGroundColor := $FFFFFF;
          SectionLayout.BackGroundColorTo := $FFFFFF;

          Color := $E7D1BF;
          ColorTo := $E7D1BF;
          GradientDirection := gdVertical;

          SectionLayout.CaptionGradientDir := gdVertical;
          SectionLayout.BackGroundGradientDir := gdVertical;
          SectionLayout.ItemFontColor := $8B4215;
          SectionLayout.ItemHoverTextColor := $8B4215;
          SectionLayout.ItemHoverUnderLine := true;
          SectionLayout.BorderWidth := 1;
          SectionLayout.BorderColor := $FFD1AD;
          SectionLayout.CaptionFontColor := $8B4215;

          CaptionGradientDirection := gdVertical;

          FocusCaptionColor := $67D7FF;
          FocusCaptionColorTo := $A2E7FF;
          FocusCaptionFontColor := $8B4215;

          ShowCaptionBorder := false;
          button3d := false;

        end;
        esOffice2010Silver:
        begin
          SectionLayout.CaptionColor := $F0EBE8;
          SectionLayout.CaptionColorTo := $F0EBE8;
          SectionLayout.CaptionGradientDir := gdVertical;

          NoFocusCaptionColor := $F8F7F6;
          NoFocusCaptionColorTo := $E6DED9;
          NoFocusCaptionFontColor := $8B4215;

          HoverButtonColor := $D9F9FD;
          HoverButtonColorTo := $8AE3FD;

          DownButtonColor := $7BEEFF;
          DownButtonColorTo := $6CD0FF;

          SectionLayout.BackGroundColor := $FFFFFF;
          SectionLayout.BackGroundColorTo := $FFFFFF;

          Color := $D4CFCB;
          ColorTo := $D4CFCB;
          GradientDirection := gdVertical;

          SectionLayout.CaptionGradientDir := gdVertical;
          SectionLayout.BackGroundGradientDir := gdVertical;
          SectionLayout.ItemFontColor := $8B4215;
          SectionLayout.ItemHoverTextColor := $8B4215;
          SectionLayout.ItemHoverUnderLine := true;
          SectionLayout.BorderWidth := 1;
          SectionLayout.BorderColor := $C7C7C5;
          SectionLayout.CaptionFontColor := $8B4215;

          CaptionGradientDirection := gdVertical;

          FocusCaptionColor := $67D7FF;
          FocusCaptionColorTo := $A2E7FF;
          FocusCaptionFontColor := $8B4215;

          ShowCaptionBorder := false;
          button3d := false;

        end;
        esOffice2010Black:
        begin
          SectionLayout.CaptionColor := $ECEAE9;
          SectionLayout.CaptionColorTo := $ECEAE9;
          SectionLayout.CaptionGradientDir := gdVertical;

          NoFocusCaptionColor := $F2F1F0;
          NoFocusCaptionColorTo := $C7C0BB;
          NoFocusCaptionFontColor := clBlack;

          HoverButtonColor := $D9F9FD;
          HoverButtonColorTo := $8AE3FD;

          DownButtonColor := $7BEEFF;
          DownButtonColorTo := $6CD0FF;

          SectionLayout.BackGroundColor := $FFFFFF;
          SectionLayout.BackGroundColorTo := $FFFFFF;

          Color := $7C7C7C;
          ColorTo := $7C7C7C;
          GradientDirection := gdVertical;

          SectionLayout.CaptionGradientDir := gdVertical;
          SectionLayout.BackGroundGradientDir := gdVertical;
          SectionLayout.ItemFontColor := clBlack;
          SectionLayout.ItemHoverTextColor := clBlack;
          SectionLayout.ItemHoverUnderLine := true;
          SectionLayout.BorderWidth := 1;
          SectionLayout.BorderColor := $D1CBC7;
          SectionLayout.CaptionFontColor := clBlack;

          CaptionGradientDirection := gdVertical;

          FocusCaptionColor := $67D7FF;
          FocusCaptionColorTo := $A2E7FF;
          FocusCaptionFontColor := $8B4215;

          ShowCaptionBorder := false;
          button3d := false;

        end;
      esWindows8, esWindows10:
        begin
          SectionLayout.CaptionColor := $EEEEEE;
          SectionLayout.CaptionColorTo := $EEEEEE;
          SectionLayout.CaptionGradientDir := gdVertical;

          NoFocusCaptionColor := $F7F6F5;
          NoFocusCaptionColorTo := $F7F6F5;
          NoFocusCaptionFontColor := clBlack;

          HoverButtonColor := $F7EFE8;
          HoverButtonColorTo := $F7EFE8;

          DownButtonColor := $F7E0C9;
          DownButtonColorTo := $F7E0C9;

          SectionLayout.BackGroundColor := clWhite;
          SectionLayout.BackGroundColorTo := clWhite;

          Color := $F7F6F5;
          ColorTo := $F7F6F5;
          GradientDirection := gdVertical;

          SectionLayout.CaptionGradientDir := gdVertical;
          SectionLayout.BackGroundGradientDir := gdVertical;
          SectionLayout.ItemFontColor := clBlack;
          SectionLayout.ItemHoverTextColor := clBlack;
          SectionLayout.ItemHoverUnderLine := true;
          SectionLayout.BorderWidth := 1;
          SectionLayout.BorderColor := $E4E3E2;
          SectionLayout.CaptionFontColor := clBlack;

          CaptionGradientDirection := gdVertical;

          FocusCaptionColor := $F7E0C9;
          FocusCaptionColorTo := $F7E0C9;
          FocusCaptionFontColor := clBlack;

          ShowCaptionBorder := false;
          button3d := false;

        end;
      esOffice2013White:
        begin
          SectionLayout.CaptionColor := $EEEEEE;
          SectionLayout.CaptionColorTo := $EEEEEE;
          SectionLayout.CaptionGradientDir := gdVertical;

          NoFocusCaptionColor := $F7F6F5;
          NoFocusCaptionColorTo := $F7F6F5;
          NoFocusCaptionFontColor := clBlack;

          HoverButtonColor := $FCF0E4;
          HoverButtonColorTo := $FCF0E4;

          DownButtonColor := $FCE2C8;
          DownButtonColorTo := $FCE2C8;

          SectionLayout.BackGroundColor := clWhite;
          SectionLayout.BackGroundColorTo := clWhite;

          Color := $F7F6F5;
          ColorTo := $F7F6F5;
          GradientDirection := gdVertical;

          SectionLayout.CaptionGradientDir := gdVertical;
          SectionLayout.BackGroundGradientDir := gdVertical;
          SectionLayout.ItemFontColor := clBlack;
          SectionLayout.ItemHoverTextColor := clBlack;
          SectionLayout.ItemHoverUnderLine := true;
          SectionLayout.BorderWidth := 1;
          SectionLayout.BorderColor := $D4D4D4;
          SectionLayout.CaptionFontColor := clBlack;

          CaptionGradientDirection := gdVertical;

          FocusCaptionColor := $FCE2C8;
          FocusCaptionColorTo := $FCE2C8;
          FocusCaptionFontColor := clBlack;

          ShowCaptionBorder := false;
          button3d := false;

        end;
     esOffice2013LightGray:
        begin
          SectionLayout.CaptionColor := $F6F6F6;
          SectionLayout.CaptionColorTo := $F6F6F6;
          SectionLayout.CaptionGradientDir := gdVertical;

          NoFocusCaptionColor := $F6F6F6;
          NoFocusCaptionColorTo := $F6F6F6;
          NoFocusCaptionFontColor := clBlack;

          HoverButtonColor := $FCF0E4;
          HoverButtonColorTo := $FCF0E4;

          DownButtonColor := $FCE2C8;
          DownButtonColorTo := $FCE2C8;

          SectionLayout.BackGroundColor := clWhite;
          SectionLayout.BackGroundColorTo := clWhite;

          Color := $FAFAFA;
          ColorTo := $FAFAFA;
          GradientDirection := gdVertical;

          SectionLayout.CaptionGradientDir := gdVertical;
          SectionLayout.BackGroundGradientDir := gdVertical;
          SectionLayout.ItemFontColor := clBlack;
          SectionLayout.ItemHoverTextColor := clBlack;
          SectionLayout.ItemHoverUnderLine := true;
          SectionLayout.BorderWidth := 1;
          SectionLayout.BorderColor := $C6C6C6;
          SectionLayout.CaptionFontColor := clBlack;

          CaptionGradientDirection := gdVertical;

          FocusCaptionColor := $FCE2C8;
          FocusCaptionColorTo := $FCE2C8;
          FocusCaptionFontColor := clBlack;

          ShowCaptionBorder := false;
          button3d := false;

        end;
    esOffice2013Gray:
        begin
          SectionLayout.CaptionColor := $E5E5E5;
          SectionLayout.CaptionColorTo := $E5E5E5;
          SectionLayout.CaptionGradientDir := gdVertical;

          NoFocusCaptionColor := $E5E5E5;
          NoFocusCaptionColorTo := $E5E5E5;
          NoFocusCaptionFontColor := clBlack;

          HoverButtonColor := $FCF0E4;
          HoverButtonColorTo := $FCF0E4;

          DownButtonColor := $FCE2C8;
          DownButtonColorTo := $FCE2C8;

          SectionLayout.BackGroundColor := clWhite;
          SectionLayout.BackGroundColorTo := clWhite;

          Color := $F3F3F3;
          ColorTo := $F3F3F3;
          GradientDirection := gdVertical;

          SectionLayout.CaptionGradientDir := gdVertical;
          SectionLayout.BackGroundGradientDir := gdVertical;
          SectionLayout.ItemFontColor := clBlack;
          SectionLayout.ItemHoverTextColor := clBlack;
          SectionLayout.ItemHoverUnderLine := true;
          SectionLayout.BorderWidth := 1;
          SectionLayout.BorderColor := $ABABAB;
          SectionLayout.CaptionFontColor := clBlack;

          CaptionGradientDirection := gdVertical;

          FocusCaptionColor := $FCE2C8;
          FocusCaptionColorTo := $FCE2C8;
          FocusCaptionFontColor := clBlack;

          ShowCaptionBorder := false;
          button3d := false;

        end;

      esOffice2016White:
        begin
          SectionLayout.CaptionColor := $D4D4D4;
          SectionLayout.CaptionColorTo := $D4D4D4;
          SectionLayout.CaptionGradientDir := gdVertical;

          NoFocusCaptionColor := clWhite;
          NoFocusCaptionColorTo := clWhite;
          NoFocusCaptionFontColor := $444444;

          HoverButtonColor := $F2E1D5;
          HoverButtonColorTo := $F2E1D5;

          DownButtonColor := $E3BDA3;
          DownButtonColorTo := $E3BDA3;

          SectionLayout.BackGroundColor := clWhite;
          SectionLayout.BackGroundColorTo := clWhite;

          Color := $F0F0F0;
          ColorTo := $F0F0F0;
          GradientDirection := gdVertical;

          SectionLayout.CaptionGradientDir := gdVertical;
          SectionLayout.BackGroundGradientDir := gdVertical;
          SectionLayout.ItemFontColor := $444444;
          SectionLayout.ItemHoverTextColor := AppColor;
          SectionLayout.ItemHoverUnderLine := true;
          SectionLayout.BorderWidth := 1;
          SectionLayout.BorderColor := $D4D4D4;
          SectionLayout.CaptionFontColor := $444444;

          CaptionGradientDirection := gdVertical;

          FocusCaptionColor := clWhite;
          FocusCaptionColorTo := clWhite;
          FocusCaptionFontColor := AppColor;

          ShowCaptionBorder := false;
          button3d := false;

        end;
     esOffice2016Gray:
        begin
          SectionLayout.CaptionColor := $B2B2B2;
          SectionLayout.CaptionColorTo := $B2B2B2;
          SectionLayout.CaptionGradientDir := gdVertical;

          NoFocusCaptionColor := $B2B2B2;
          NoFocusCaptionColorTo := $B2B2B2;
          NoFocusCaptionFontColor := $F0F0F0;

          HoverButtonColor := $F2E1D5;
          HoverButtonColorTo := $F2E1D5;

          DownButtonColor := $E3BDA3;
          DownButtonColorTo := $E3BDA3;

          SectionLayout.BackGroundColor := $D4D4D4;
          SectionLayout.BackGroundColorTo := $D4D4D4;

          Color := $B2B2B2;
          ColorTo := $B2B2B2;
          GradientDirection := gdVertical;

          SectionLayout.CaptionGradientDir := gdVertical;
          SectionLayout.BackGroundGradientDir := gdVertical;
          SectionLayout.ItemFontColor := $262626;
          SectionLayout.ItemHoverTextColor := $262626;
          SectionLayout.ItemHoverUnderLine := true;
          SectionLayout.BorderWidth := 1;
          SectionLayout.BorderColor := $C6C6C6;
          SectionLayout.CaptionFontColor := $262626;

          CaptionGradientDirection := gdVertical;

          FocusCaptionColor := $B2B2B2;
          FocusCaptionColorTo := $B2B2B2;
          FocusCaptionFontColor := $262626;

          ShowCaptionBorder := false;
          button3d := false;

        end;
    esOffice2016Black:
        begin
          SectionLayout.CaptionColor := $252525;
          SectionLayout.CaptionColorTo := $252525;
          SectionLayout.CaptionGradientDir := gdVertical;

          NoFocusCaptionColor := $252525;
          NoFocusCaptionColorTo := $252525;
          NoFocusCaptionFontColor := $DADADA;

          HoverButtonColor := $6A6A6A;
          HoverButtonColorTo := $6A6A6A;

          DownButtonColor := $444444;
          DownButtonColorTo := $444444;

          SectionLayout.BackGroundColor := $363636;
          SectionLayout.BackGroundColorTo := $363636;

          Color := $363636;
          ColorTo := $363636;
          GradientDirection := gdVertical;

          SectionLayout.CaptionGradientDir := gdVertical;
          SectionLayout.BackGroundGradientDir := gdVertical;
          SectionLayout.ItemFontColor := $DADADA;
          SectionLayout.ItemHoverTextColor := $FFFFFF;
          SectionLayout.ItemHoverUnderLine := true;
          SectionLayout.BorderWidth := 1;
          SectionLayout.BorderColor := $4D4D4D;
          SectionLayout.CaptionFontColor := $DADADA;

          CaptionGradientDirection := gdVertical;

          FocusCaptionColor := $252525;
          FocusCaptionColorTo := $252525;
          FocusCaptionFontColor := $FFFFFF;

          ShowCaptionBorder := false;
          button3d := false;

        end;

    end;
  end;
end;


procedure TAdvToolPanel.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  if (AOperation = opRemove) and (AComponent = FSectionImages) then
    FSectionImages := nil;

  inherited;
end;

procedure TAdvToolPanel.Paint;
var
  ct, cy: integer;
  r, r2, r3: TRect;
  fr, CapR: TRect;
  pt: TPoint;
  FGColor: TColor;
  //BkColor: TColor;
  NormalShow: Boolean;
  lf, tp, i, h, a, j, fh, yo, xo, OldFontS: integer;
  tRgn, rgn1, rgn2: HRGN;
begin
  inherited;
{
if Floating then
Exit;
if not FShowCaption then
Exit;
}
  GetCursorPos(pt);
  pt := ScreenToClient(pt);

  with Canvas do
  begin
    r := ClientRect;

    if FColorTo <> clNone then
    begin
      r.Right := r.Right - 2 * BorderWidth;
      r.Bottom := r.Bottom - 2 * BorderWidth;
      R.Left := r.Left + BorderWidth;
      DrawGradient(Canvas, Color, FColorTo, 128, r, FGradientDirection = gdHorizontal);
      R.Left := r.Left - 1;
      r.Right := r.Right + 2;
      r.Bottom := r.Bottom + 2;
    end
    else
    begin
      Brush.Color := Color;
      Rectangle(r.Left, r.Top, r.Right, r.Bottom);
    end;

  //------ BackGround Image
    if Assigned(FBackGround) then
    begin
      R.Top := R.Top + CaptionHeight;
      R.Left := R.Left + 1;
      if not FBackGround.Empty then
      begin
        FBackGround.Transparent := FBackgroundTransparent;
        case FBackgroundPosition of
          bpTopLeft: Canvas.Draw(r.Left, r.Top, FBackGround);
          bpTopRight: Canvas.Draw(Max(r.Left, r.Right - r.Left - FBackGround.Width - BevelWidth), r.top, FBackGround);
          bpBottomLeft: Canvas.Draw(r.left, Max(r.top, Height - FBackGround.Height - BevelWidth - 1), FBackGround);
          bpBottomRight: Canvas.Draw(Max(r.Left, r.Right - r.Left - FBackGround.Width - BevelWidth), Max(r.Top, Height - FBackGround.Height - BevelWidth - 1), FBackGround);
          bpCenter: Canvas.Draw(Max(r.Left, r.Right - r.Left - FBackGround.Width - BevelWidth) shr 1, Max(r.Top, Height - FBackGround.Height - BevelWidth) shr 1, FBackGround);
          bpTiled:
            begin
              yo := r.Top;
              while (yo < Height) do
              begin
                xo := r.Left;
                while (xo < Width) do
                begin
                  Canvas.Draw(xo, yo, FBackGround);
                  xo := xo + FBackGround.Width;
                end;
                yo := yo + FBackGround.Height;
              end;
            end;
          bpStretched: Canvas.StretchDraw(R, FBackGround);
        else
        end;
      end;
      R.Top := R.Top - CaptionHeight;
      R.Left := R.Left - 1;
    end;
  //----- End BackGround Image

//----- Section
    if Floating or not FShowCaption then
      h := 1
    else
      h := FCaptionHeight;

    for i := 1 to Sections.Count do
    begin
      if TAdvToolPanelSection(Sections.Items[i - 1]).Draw then
      begin

        if FSectionLayout.CaptionColor <> clNone then // Draw Section Caption
        begin
          CapR := Rect(1 + FSectionLayout.Indent, h, width - 1, h + SectionHeight + 1);
          case FSectionLayout.Corners of
            scRectangle:
              DrawGradient(Canvas, FSectionLayout.CaptionColor, FSectionLayout.CaptionColorTo, 48, Rect(1 + FSectionLayout.Indent, h, width - 2, h + SectionHeight + 1), FSectionLayout.CaptionGradientDir = gdHorizontal);
            scRoundLeft:
              begin
                trgn := CreateRoundRectRgn(CapR.Left, CapR.Top, CapR.Right, CapR.Bottom - (CapR.Bottom - CapR.Top) div 2, 16, 16);
                rgn1 := CreateRoundRectRgn(CapR.Left, CapR.Top, CapR.Right, CapR.Bottom {-(r.Bottom - r.Top)div 2}, 16, 16);
                rgn2 := CreateRectRgn(CapR.Left, CapR.Top + (CapR.Bottom - CapR.Top) div 2, CapR.Right - 1, CapR.Bottom);
                CombineRgn(trgn, rgn1, rgn2, RGN_OR);

                DeleteObject(rgn2);
                rgn2 := CreateRectRgn(CapR.Left + (CapR.Right - CapR.Left) div 2, CapR.Top, CapR.Right - 1, CapR.Bottom);
                CombineRgn(trgn, trgn, rgn2, RGN_OR);

                SelectClipRgn(Canvas.Handle, trgn);
                DrawGradient(Canvas, FSectionLayout.CaptionColor, FSectionLayout.CaptionColorTo, 48, Rect(1 + FSectionLayout.Indent, h, width - 1, h + SectionHeight + 1), FSectionLayout.CaptionGradientDir = gdHorizontal);

                SelectClipRgn(Canvas.Handle, 0);
                DeleteObject(Rgn1);
                DeleteObject(rgn2);
                DeleteObject(trgn);
              end;
            scRoundRight:
              begin
                trgn := CreateRoundRectRgn(CapR.Left, CapR.Top, CapR.Right, CapR.Bottom - (CapR.Bottom - CapR.Top) div 2, 16, 16);
                rgn1 := CreateRoundRectRgn(CapR.Left, CapR.Top, CapR.Right, CapR.Bottom {-(r.Bottom - r.Top)div 2}, 16, 16);
                rgn2 := CreateRectRgn(CapR.Left, CapR.Top + (CapR.Bottom - CapR.Top) div 2, CapR.Right - 1, CapR.Bottom);
                CombineRgn(trgn, rgn1, rgn2, RGN_OR);

                DeleteObject(rgn2);
                rgn2 := CreateRectRgn(CapR.Left, CapR.Top, CapR.Right - 1 - (CapR.Right - CapR.Left) div 2, CapR.Bottom);
                CombineRgn(trgn, trgn, rgn2, RGN_OR);

                SelectClipRgn(Canvas.Handle, trgn);
                DrawGradient(Canvas, FSectionLayout.CaptionColor, FSectionLayout.CaptionColorTo, 48, Rect(1 + FSectionLayout.Indent, h, width - 1, h + SectionHeight + 1), FSectionLayout.CaptionGradientDir = gdHorizontal);

                SelectClipRgn(Canvas.Handle, 0);
                DeleteObject(Rgn1);
                DeleteObject(rgn2);
                DeleteObject(trgn);
              end;
            scRoundLeftRight:
              begin
                trgn := CreateRoundRectRgn(CapR.Left, CapR.Top, CapR.Right, CapR.Bottom - (CapR.Bottom - CapR.Top) div 2, 16, 16);
                rgn1 := CreateRoundRectRgn(CapR.Left, CapR.Top, CapR.Right, CapR.Bottom {-(r.Bottom - r.Top)div 2}, 16, 16);
                rgn2 := CreateRectRgn(CapR.Left, CapR.Top + (CapR.Bottom - CapR.Top) div 2, CapR.Right - 1, CapR.Bottom);
                CombineRgn(trgn, rgn1, rgn2, RGN_OR);

                SelectClipRgn(Canvas.Handle, trgn);
                DrawGradient(Canvas, FSectionLayout.CaptionColor, FSectionLayout.CaptionColorTo, 48, Rect(1 + FSectionLayout.Indent, h, width - 1, h + SectionHeight + 1), FSectionLayout.CaptionGradientDir = gdHorizontal);

                SelectClipRgn(Canvas.Handle, 0);
                DeleteObject(Rgn1);
                DeleteObject(rgn2);
                DeleteObject(trgn);
              end;
          end;
        end;

        if FSectionLayout.UnderLineCaption then
        begin
          Pen.Width := FSectionLayout.BorderWidth;
          Pen.Color := FSectionLayout.BorderColor;

          MoveTo(1 + FSectionLayout.Indent, h + SectionHeight);
          LineTo(Width - 2, h + SectionHeight);
        end;

        a := 0; // Draw Node Button
        if TAdvToolPanelSection(Sections.Items[i - 1]).Node then
        begin
          a := 4;
          ct := (SectionHeight - NODEBTN_SIZE) div 2;
          r3 := Rect(a + FSectionLayout.Indent, h + ct{2 + 2}, a + NODEBTN_SIZE + 1 + FSectionLayout.Indent, h + ct{2 + 2} + NODEBTN_SIZE + 1);
          if (FSectionLayout.BackGroundColor <> clNone) and (FSectionLayout.BackGroundColorTo <> clNone) then
            DrawGradient(Canvas, FSectionLayout.BackGroundColor, FSectionLayout.BackGroundColorTo, 16, r3, true {FSectionLayout.CaptionGradientDir = gdVertical});
          Brush.Style := bsClear;
          Pen.Width := 1;
          pen.Color := clGray; //FSectionLayout.BorderColor;
          font.Color := clBlack;
          OldFontS := Font.Size;
          Font.Size := 7;
          RoundRect(r3.Left, r3.Top, r3.Right, r3.Bottom, 3, 3);
          if TAdvToolPanelSection(Sections.Items[i - 1]).Expanded then
          begin
        //TextOut(r3.Left+2, r3.Top-8, '_')
            Pen.Color := clBlack;
            Canvas.MoveTo(r3.left + 2, r3.Top + 4);
            Canvas.LineTo(R3.Left + 7, r3.Top + 4);
          end
          else
            TextOut(r3.Left + 1, r3.Top - 4 + 1, '+');
          font.Size := OldFontS;
          a := a + NODEBTN_SIZE;
        end;
                                                  // Draw Caption Image
        if Assigned(FSectionImages) and (TAdvToolPanelSection(Sections.Items[i - 1]).CaptionImageIndex > -1) then
        begin
          FSectionImages.Draw(Canvas, a + 2 + FSectionLayout.Indent, h + 2, TAdvToolPanelSection(Sections.Items[i - 1]).CaptionImageIndex);
          a := a + FSectionImages.Width;
        end; // Draw Caption Text
        Canvas.Brush.Style := bsClear;
        Font.Color := FSectionLayout.CaptionFontColor; // FNoFocusCaptionFontColor;
        ct := (SectionHeight - Canvas.TextHeight('gh')) div 2;
        Canvas.TextOut(4 + a + FSectionLayout.Indent, h + ct{2}, TAdvToolPanelSection(Sections.Items[i - 1]).Caption);

        if TAdvToolPanelSection(Sections.Items[i - 1]).Expanded then
        begin
                                                   // Draw BackGround
          if FSectionLayout.BackGroundColor <> clNone then
            DrawGradient(Canvas, FSectionLayout.BackGroundColor, FSectionLayout.BackGroundColorTo, 128, Rect(1 + FSectionLayout.Indent, h + SectionHeight + 1, width - 1, h + TAdvToolPanelSection(Sections.Items[i - 1]).Height - FSectionLayout.Spacing), FSectionLayout.BackGroundGradientDir = gdHorizontal);
                                                   // Draw Border
          if (FSectionLayout.BorderColor <> clNone) and (FSectionLayout.BorderWidth > 0) then
          begin
            Pen.Width := FSectionLayout.BorderWidth;
            Pen.Color := FSectionLayout.BorderColor;

            MoveTo(Width - 2, h + SectionHeight + 1);
            LineTo(1 + FSectionLayout.Indent, h + SectionHeight + 1);

            MoveTo(1 + FSectionLayout.Indent, h + SectionHeight + 1);
            LineTo(1 + FSectionLayout.Indent, h + TAdvToolPanelSection(Sections.Items[i - 1]).Height - 1 - FSectionLayout.Spacing);

            MoveTo(1 + FSectionLayout.Indent, h + TAdvToolPanelSection(Sections.Items[i - 1]).Height - 1 - FSectionLayout.Spacing);
            LineTo(Width - 2, h + TAdvToolPanelSection(Sections.Items[i - 1]).Height - 1 - FSectionLayout.Spacing);
          end;

          a := 12;
          fh := 0;
          if Assigned(FSectionImages) then
          begin
            for j := 0 to TAdvToolPanelSection(Sections.Items[i - 1]).SectionItems.Count - 1 do
            begin
              if (TAdvToolPanelSection(Sections.Items[i - 1]).SectionItems.Items[j].ImageIndex > -1) then
              begin
                a := a + FSectionImages.Width;
                fh := FSectionImages.Height + 2;
                break;
              end;
            end;
          end;

          fh := max(fh, Canvas.TextHeight('gh')); // Draw Items
          ct := (fh - Canvas.TextHeight('gh')) div 2;
          ct := Max(ct, 4);
          cy := Max(10, (fh div 2)+2);
          for j := 0 to TAdvToolPanelSection(Sections.Items[i - 1]).SectionItems.Count - 1 do
          begin
            if (h + ct{4} + SectionHeight + (j * fh)) >= (h + TAdvToolPanelSection(Sections.Items[i - 1]).Height - FSectionLayout.Spacing) then
              break;

            if TAdvToolPanelSection(Sections.Items[i - 1]).SectionItems.Items[j].Caption = '-' then
            begin
              Pen.Width := FSectionLayout.BorderWidth;
              Pen.Color := FSectionLayout.BorderColor;

              if Assigned(FSectionImages) and (a > 12) then
                MoveTo(a - FSectionImages.Width - 2 + FSectionLayout.Indent, h + cy{10} + SectionHeight + (j * fh))
              else
                MoveTo(a + FSectionLayout.Indent, h + cy{10} + SectionHeight + (j * fh));
              LineTo(Width - 2, h + cy{10} + SectionHeight + (j * fh));
            end
            else
            begin
              if Assigned(FSectionImages) and (TAdvToolPanelSection(Sections.Items[i - 1]).SectionItems.Items[j].ImageIndex > -1) then
              begin
            //draw Image
                FSectionImages.Draw(Canvas, a - FSectionImages.Width - 2 + FSectionLayout.Indent, h + 4 + SectionHeight + (j * fh), TAdvToolPanelSection(Sections.Items[i - 1]).SectionItems.Items[j].ImageIndex);
              end;

              Canvas.Brush.Style := bsClear;
          //Font.Color:= FSectionLayout.ItemFontColor;
              if (FHoverSectionIndex = (i - 1)) and (FHoverSectionItemIndex = j) then
              begin
                Font.Color := FSectionLayout.ItemHoverTextColor;
                if FSectionLayout.ItemHoverUnderline then
                  Font.Style := [fsUnderLine]
                else
                  Font.Style := [];
              end
              else
              begin
                Font.Color := FSectionLayout.ItemFontColor; ;
                Font.Style := [];
              end;

              if not TAdvToolPanelSection(Sections.Items[i - 1]).SectionItems.Items[j].Enabled then
              begin
                Font.Color := clGray;
                Font.Style := [];
              end;

              //Canvas.TextOut(a + FSectionLayout.Indent, h + ct{4} + SectionHeight + (j * fh), TAdvToolPanelSection(Sections.Items[i - 1]).SectionItems.Items[j].Caption);
              CapR := Rect(a + FSectionLayout.Indent, h + ct + SectionHeight + (j * fh), Width-2, h + ct + SectionHeight + ((j+1) * fh));
              DrawText(Canvas.Handle, PChar(TAdvToolPanelSection(Sections.Items[i - 1]).SectionItems.Items[j].Caption), Length(TAdvToolPanelSection(Sections.Items[i - 1]).SectionItems.Items[j].Caption), CapR, DT_SINGLELINE or DT_LEFT or DT_VCENTER or DT_END_ELLIPSIS or DT_NOPREFIX);
            end;
          end;
        end;
      end;
      h := h + TAdvToolPanelSection(Sections.Items[i - 1]).Height;
    end;
//----- End Section

    if Floating then
      Exit;
    if not FShowCaption then
      Exit;

    r := ClientRect;
    r := Rect(r.Left, r.Top, r.Right - 1, r.Top + CaptionHeight);

    InflateRect(r, -1, -1);

    if FActive then
    begin
      Brush.Color := FFocusCaptionColor;
      Pen.Color := FFocusCaptionColor;
      //BKColor := FFocusCaptionColor;
      FGColor := FFocusCaptionFontColor;
      if FFocusCaptionColorTo <> clNone then
      begin
        //BKColor := clNone;
        DrawGradient(Canvas, FFocusCaptionColor, FFocusCaptionColorTo, 16, r, FCaptionGradientDirection = gdHorizontal);
      end
      else
        FillRect(r);
    end
    else
    begin
      //BKColor := FNoFocusCaptionColor;
      FGColor := FNoFocusCaptionFontColor;
      Brush.Color := FNoFocusCaptionColor;
      if FNoFocusCaptionColorTo <> clNone then
      begin
        //BKColor := clNone;
        DrawGradient(Canvas, FNoFocusCaptionColor, FNoFocusCaptionColorTo, 16, r, FCaptionGradientDirection = gdHorizontal);
      end
      else
        FillRect(r);
      if FShowCaptionBorder then
      begin
        Brush.Style := bsClear;
        Pen.Color := clGray;
        Pen.Width := 1;
        RoundRect(r.Left, r.Top, r.Right, r.Bottom { r.top + CaptionHeight}, 3, 3);
      end;
    end;

    NormalShow := True;

    if Assigned(Tab) and ShowClose then
    begin
      if not Tab.PanelGlyphs.CloseGlyphNormal.Empty then
      begin
        NormalShow := False;
        fr := Rect(r.Right - Tab.PanelGlyphs.CloseGlyphNormal.Width - 4,
          r.Top + 2, r.Right - 6, r.Top + Tab.PanelGlyphs.CloseGlyphNormal.Height);
        InflateRect(fr, 1, 1);

        if PtInRect(fr, pt) then
        begin
          if FMouseDown then
            Canvas.Draw(fr.Left + 1, fr.Top + 1, Tab.PanelGlyphs.CloseGlyphDown)
          else
            Canvas.Draw(fr.Left + 1, fr.Top + 1, Tab.PanelGlyphs.CloseGlyphHot);
        end
        else
          Canvas.Draw(fr.Left + 1, fr.Top + 1, Tab.PanelGlyphs.CloseGlyphNormal);

        r.Right := r.Right - Tab.PanelGlyphs.CloseGlyphNormal.Width - 5;
      end;

    end;

    if not FCloseGlyph.Empty and ShowClose and NormalShow then
    begin
      fr := Rect(r.Right - FCloseGlyph.Width - 4, r.Top + 2, r.Right - 6, r.Top + FCloseGlyph.Height);
      InflateRect(fr, 1, 1);
  //DrawMonoBitmap(Canvas,r.Right - FCloseGlyph.Width - 5,r.Top + 2,FCloseGlyph,FGColor,BKColor);

      if PtInRect(fr, pt) and not (csDesigning in ComponentState) then
      begin
        if FButton3D then
        begin
          if FMouseDown then
            Frame3D(Canvas, fr, clGray, clWhite, 1)
          else
            Frame3D(Canvas, fr, clWhite, clGray, 1);
        end
        else // not Button3D
        begin
          if FMouseDown then
          begin
            if DownButtonColor <> clNone then
              DrawGradient(Canvas, FDownButtonColor, FDownButtonColorTo, 16, fr, CaptionGradientDirection = gdHorizontal);
            Pen.Color := clBlack;
            Brush.Style := bsClear;
            fr.Bottom := fr.Bottom + 1;
            Rectangle(fr.Left, fr.Top, fr.Right, fr.Bottom);
          end
          else
          begin
            if FHoverButtonColor <> clNone then
              DrawGradient(Canvas, FHoverButtonColor, FHoverButtonColorTo, 16, fr, CaptionGradientDirection = gdHorizontal);
            Pen.Color := clBlack;
            Brush.Style := bsClear;
            fr.Bottom := fr.Bottom + 1;
            Rectangle(fr.Left, fr.Top, fr.Right, fr.Bottom);
          end;
        end;
      end;

      Pen.Color := FGColor;
                 {/}
      MoveTo(r.Right - FCloseGlyph.Width - 1, r.Top + 11);
      LineTo(r.Right - FCloseGlyph.Width + 7, r.Top + 4);
      MoveTo(r.Right - FCloseGlyph.Width, r.Top + 11);
      LineTo(r.Right - FCloseGlyph.Width + 6, r.Top + 4);
                 {\}
      MoveTo(r.Right - FCloseGlyph.Width - 1, r.Top + 5);
      LineTo(r.Right - FCloseGlyph.Width + 7, r.Top + 12);
      MoveTo(r.Right - FCloseGlyph.Width, r.Top + 5);
      LineTo(r.Right - FCloseGlyph.Width + 6, r.Top + 12);

      r.Right := r.Right - FCloseGlyph.Width - 5;
    end;

    NormalShow := true;

    if Assigned(Tab) and ShowLock then
    begin
      if not Tab.PanelGlyphs.LockGlyphNormal.Empty then
      begin
        NormalShow := False;
        fr := Rect(r.Right - Tab.PanelGlyphs.LockGlyphNormal.Width - 4,
          r.Top + 2, r.Right - 6, r.Top + Tab.PanelGlyphs.LockGlyphNormal.Height);
        InflateRect(fr, 1, 1);

        if Locked then
        begin
          if PtInRect(fr, pt) then
          begin
            if FMouseDown then
              Canvas.Draw(fr.Left + 1, fr.Top + 1, Tab.PanelGlyphs.LockGlyphDown)
            else
              Canvas.Draw(fr.Left + 1, fr.Top + 1, Tab.PanelGlyphs.LockGlyphHot);
          end
          else
            Canvas.Draw(fr.Left + 1, fr.Top + 1, Tab.PanelGlyphs.LockGlyphNormal);
        end
        else
        begin
          if PtInRect(fr, pt) then
          begin
            if FMouseDown then
              Canvas.Draw(fr.Left + 1, fr.Top + 1, Tab.PanelGlyphs.UnLockGlyphDown)
            else
              Canvas.Draw(fr.Left + 1, fr.Top + 1, Tab.PanelGlyphs.UnLockGlyphHot);
          end
          else
            Canvas.Draw(fr.Left + 1, fr.Top + 1, Tab.PanelGlyphs.UnLockGlyphNormal);
        end;
      end;
    end;


    if NormalShow and ShowLock then
    begin
 { if Locked and not FLockedGlyph.Empty then
  begin
    DrawMonoBitmap(Canvas,r.Right - FLockedGlyph.Width,r.Top + 2,FLockedGlyph,FGColor,BKColor);
  end
  else
    if not FUnlockedGlyph.Empty and ShowLock then
     begin
       DrawMonoBitmap(Canvas,r.Right - FUnlockedGlyph.Width,r.Top + 2,FUnlockedGlyph,FGColor,BKColor);
     end;
 }
      fr := Rect(r.Right - FLockedGlyph.Width - 2, r.Top + 2, r.Right - 1, r.Top + FLockedGlyph.Height);
      InflateRect(fr, 1, 1);
      if PtInRect(fr, pt) and not (csDesigning in ComponentState) then
      begin
        if FButton3D then
        begin
          if FMouseDown then
            Frame3D(Canvas, fr, clGray, clWhite, 1)
          else
            Frame3D(Canvas, fr, clWhite, clGray, 1);
        end
        else // not Button3D
        begin
          if FMouseDown then
          begin
            if DownButtonColor <> clNone then
              DrawGradient(Canvas, FDownButtonColor, FDownButtonColorTo, 16, fr, CaptionGradientDirection = gdHorizontal);
            Pen.Color := clBlack;
            Brush.Style := bsClear;
            fr.Bottom := fr.Bottom + 1;
            Rectangle(fr.Left, fr.Top, fr.Right, fr.Bottom);
          end
          else
          begin
            if FHoverButtonColor <> clNone then
              DrawGradient(Canvas, FHoverButtonColor, FHoverButtonColorTo, 16, fr, CaptionGradientDirection = gdHorizontal);
            Pen.Color := clBlack;
            Brush.Style := bsClear;
            fr.Bottom := fr.Bottom + 1;
            Rectangle(fr.Left, fr.Top, fr.Right, fr.Bottom);
          end;
        end;
      end;

      fr := Rect(r.Right - FLockedGlyph.Width - 2, r.Top + 2, r.Right - 1, r.Top + FLockedGlyph.Height);
      if Locked {and not FLockedGlyph.Empty} then
      begin
        Pen.Color := FGColor;
                   {-}
        MoveTo(fr.left + 7, fr.Top + 2);
        LineTo(fr.left + 11, fr.Top + 2);
        MoveTo(fr.left + 7, fr.Top + 4);
        LineTo(fr.left + 11, fr.Top + 4);
                   {| |}
        MoveTo(fr.Left + 7, fr.Top + 2);
        LineTo(fr.Left + 7, fr.Top + 9);
        MoveTo(fr.Left + 11, fr.Top + 2);
        LineTo(fr.Left + 11, fr.Top + 9);
                   {__}
        MoveTo(fr.Left + 5, fr.Top + 9);
        LineTo(fr.Left + 14, fr.Top + 9);
                   {I}
        MoveTo(fr.Left + 9, fr.Top + 10);
        LineTo(fr.Left + 9, fr.Top + 14);
      end
      else if {not FUnlockedGlyph.Empty and}  ShowLock then
      begin
        Pen.Color := FGColor;
                   {  |}
        MoveTo(fr.right - 3, fr.Top + 4);
        LineTo(fr.right - 3, fr.Top + 8);
        MoveTo(fr.right - 5, fr.Top + 4);
        LineTo(fr.right - 5, fr.Top + 8);
                   {==}
        MoveTo(fr.right - 3, fr.Top + 4);
        LineTo(fr.right - 10, fr.Top + 4);
        MoveTo(fr.right - 3, fr.Top + 8);
        LineTo(fr.right - 10, fr.Top + 8);
                   {| }
        MoveTo(fr.right - 10, fr.Top + 2);
        LineTo(fr.right - 10, fr.Top + 11);
                   {I}
        MoveTo(fr.right - 11, fr.Top + 6);
        LineTo(fr.right - 15, fr.Top + 6);
      end;

    end;

    r := ClientRect;
    if DockDots then
    begin
      Canvas.Pen.Color := clWhite;
      Canvas.Brush.Color := clWhite;
      r2 := Rect(r.Left, r.Top, r.Left + DOCKDOTS_WIDTH, r.Top + FCaptionHeight);

      R2.Top := R2.Top + 2;
      lf := r2.left + 1 + ((DOCKDOTS_WIDTH - 2 {Dots Width}) div 2);
      tp := r2.Top + (CaptionHeight - 17) div 2;

      R2.Left := lf + 1;
      r2.Top := tp;
      for i := 1 to 4 do
      begin
        Canvas.Rectangle(R2.Left, R2.Top, R2.Left + 2, R2.Top + 2);
        R2.Top := R2.top + 4;
      end;

      Canvas.Pen.Color := clBlack;
      Canvas.Brush.Color := FNoFocusCaptionColorTo; //clBlack;
      R2.Top := tp - 1;
      R2.Left := lf;
      for i := 1 to 4 do
      begin
        Canvas.Rectangle(R2.Left, R2.Top, R2.Left + 2, R2.Top + 2);
        R2.Top := R2.Top + 4;
      end;
  {
  R2.Top := tp ;
  R2.Left := lf + 1;
  for i := 1 to 4 do
  begin
    Canvas.Pixels[R2.Left, R2.Top] := FNoFocusCaptionColor;
    R2.Top := R2.Top + 4;
  end;
  }

      r.Left := r.Left + DOCKDOTS_WIDTH - 1;
    end;

    r := Rect(r.Left + 4, r.Top + 4, r.Right - 32 - 5, r.Top + CaptionHeight - 2);

    if CaptionButton then
    begin
      R.Right := r.Right - 5;
      fr := Rect(r.Left - 1, r.top - 2, r.Right, r.Bottom - 1);

      if PtInRect(fr, pt) and not (csDesigning in ComponentState) then
      begin
        if FMouseDown then
        begin
          if FButton3D then
            Frame3D(Canvas, fr, clGray, clWhite, 1)
          else
          begin
            if DownButtonColor <> clNone then
              DrawGradient(Canvas, FDownButtonColor, FDownButtonColorTo, 16, fr, CaptionGradientDirection = gdHorizontal);
            Pen.Color := clBlack;
            Brush.Style := bsClear;
            fr.Bottom := fr.Bottom + 1;
            Rectangle(fr.Left, fr.Top, fr.Right, fr.Bottom);
          end;
        end
        else // Hover
        begin
          if FButton3D then
            Frame3D(Canvas, fr, clWhite, clGray, 1)
          else
          begin
            if FHoverButtonColor <> clNone then
              DrawGradient(Canvas, FHoverButtonColor, FHoverButtonColorTo, 16, fr, CaptionGradientDirection = gdHorizontal);
            Pen.Color := clBlack;
            Brush.Style := bsClear;
            fr.Bottom := fr.Bottom + 1;
            Rectangle(fr.Left, fr.Top, fr.Right, fr.Bottom);
          end;
        end;
      end;

      fr := Rect(r.Left - 1, r.top - 2, r.Right, r.Bottom - 1);

      if FActive then
        Pen.Color := FFocusCaptionFontColor
      else
        Pen.Color := FNoFocusCaptionFontColor;

        {---------}
//  MoveTo(fr.right-17 + 6, fr.Bottom - 11);
 // LineTo(fr.right-17 + 15, fr.Bottom - 11);
         {-------}
      MoveTo(fr.right - 17 + 7, fr.Bottom - 10);
      LineTo(fr.right - 17 + 14, fr.Bottom - 10);
          {-----}
      MoveTo(fr.right - 17 + 8, fr.Bottom - 9);
      LineTo(fr.right - 17 + 13, fr.Bottom - 9);
           {---}
      MoveTo(fr.right - 17 + 9, fr.Bottom - 8);
      LineTo(fr.right - 17 + 12, fr.Bottom - 8);
            {-}
      MoveTo(fr.right - 17 + 10, fr.Bottom - 7);
      LineTo(fr.right - 17 + 11, fr.Bottom - 7);
  //-------

      r.Right := r.Right - 10;
    end;

    Brush.Style := bsClear;
    if FActive then
      Font.Color := FFocusCaptionFontColor
    else
      Font.Color := FNoFocusCaptionFontColor;
    font.Style := [fsBold];
    DrawText(Handle, Pchar(Caption), Length(Caption), r, DT_LEFT or DT_SINGLELINE or DT_END_ELLIPSIS or DT_NOPREFIX);
    font.Style := [];
  end;

end;

function TAdvToolPanel.XYToSection(X,Y: integer): integer;
var
  r: TRect;
  i: integer;
begin
  Result := -1;
  for i := 1 to Sections.Count do
  begin
    R := SectionRect(i - 1, false);
    if (R.Top <= Y) and (R.Bottom >= Y) and (TAdvToolPanelSection(Sections.Items[i - 1]).Expanded) then
    begin
      Result := i;
      break;
    end;
  end;
end;

function TAdvToolPanel.PtOnAnyItem(X, Y: integer;
  var SectionIndex: integer; var ItemRect: TRect): Integer;
var
  i, h, a, fh, j, m: integer;
  R, R2: TRect;
begin
  SectionIndex := -1;
  ItemRect := Rect(-1, -1, -1, -1);
  Result := -1;
  for i := 1 to Sections.Count do
  begin
    R := SectionRect(i - 1, false);
    R2 := R;
    h := R.top;
    if (R.Top <= Y) and (R.Bottom >= Y) and (TAdvToolPanelSection(Sections.Items[i - 1]).Expanded) then
    begin
      a := 12;
      fh := 0;
      for j := 0 to TAdvToolPanelSection(Sections.Items[i - 1]).SectionItems.Count - 1 do
      begin
        if Assigned(FSectionImages) and (TAdvToolPanelSection(Sections.Items[i - 1]).SectionItems.Items[j].ImageIndex > -1) then
        begin
          a := a + FSectionImages.Width;
          fh := FSectionImages.Height + 2;
          break;
        end;
      end;

      fh := max(fh, Canvas.TextHeight('gh'));
      for j := 0 to TAdvToolPanelSection(Sections.Items[i - 1]).SectionItems.Count - 1 do
      begin
        m := 0;
        if TAdvToolPanelSection(Sections.Items[i - 1]).SectionItems.Items[j].Caption = '-'{'_'} then
        begin
        end
        else
        begin
          if Assigned(FSectionImages) and (TAdvToolPanelSection(Sections.Items[i - 1]).SectionItems.Items[j].ImageIndex > -1) then
          begin
            m := FSectionImages.Width;
          end;
          R2 := Rect(a - m + FSectionLayout.Indent, h + 4 + SectionHeight + (j * fh), a + Canvas.TextWidth(TAdvToolPanelSection(Sections.Items[i - 1]).SectionItems.Items[j].Caption) + FSectionLayout.Indent, h + 4 + SectionHeight + (j * fh) + fh);
          if PtInRect(R2, Point(X, Y)) then
          begin
            SectionIndex := i - 1;
            ItemRect := Rect(1 + FSectionLayout.Indent, R2.Top, Width - 1, R2.Bottom);
            Result := j;
            break;
          end;
        end;
      end;

    end;
  end;
end;

procedure TAdvToolPanel.Resize;
begin
  inherited;
  if not (csDesigning in ComponentState) then
    if Assigned(Tab) then
      Tab.ResetTimer;
end;


procedure TAdvToolPanel.SectionLayoutChanged(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to Sections.Count - 1 do
    Sections.Items[i].Autosize := Sections.Items[i].Autosize;
  invalidate;
end;

function TAdvToolPanel.SectionRect(SectionIndex: integer; Actual: boolean): TRect;
var
  i, h: integer;
begin
  if Floating or not FShowCaption then
    h := 1
  else
    h := FCaptionHeight;

  for i := 1 to Sections.Count do
  begin
    if (i - 1 = SectionIndex) then
    begin
      if Actual then
        Result := Rect(1 + FSectionLayout.Indent, h, width - 1, h + TAdvToolPanelSection(Sections.Items[i - 1]).ActualHeight)
      else
        Result := Rect(1 + FSectionLayout.Indent, h, width - 1, h + TAdvToolPanelSection(Sections.Items[i - 1]).Height);
      break;
    end;
    h := h + TAdvToolPanelSection(Sections.Items[i - 1]).Height;
  end;
end;

{
function TAdvToolPanel.SectionExpandedRect(SectionIndex: integer): TRect;
var
i, h: integer;
begin
if Floating or not FShowCaption then
  h := 1
else
  h := CAPTION_HEIGHT;

for i := 1 to Sections.Count do
begin
  if (i - 1 = SectionIndex) then
  begin
    Result := Rect(1+FSectionLayout.Indent, h, width - 1, h + TAdvToolPanelSection(Sections.Items[i - 1]).ActualHeight);
    break;
  end;
  h := h + TAdvToolPanelSection(Sections.Items[i - 1]).ActualHeight;
end;
end;
}
procedure TAdvToolPanel.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  tw: Integer;
begin
  if (csDesigning in ComponentState) then
  begin
    if Parent is TAdvToolPanelTab then
    begin
      tw := TAdvToolPanelTab(Parent).TabWidth;
      if TAdvToolPanelTab(Parent).Position = ppLeft then
      begin
        if (ALeft <> tw) and (AWidth > 0) then
          inherited SetBounds(tw, 0, TAdvToolPanelTab(Parent).Width - tw,
            TAdvToolPanelTab(Parent).Height)
        else
          inherited SetBounds(ALeft, ATop, AWidth, AHeight);
      end
      else if TAdvToolPanelTab(Parent).Position = ppRight then
      begin
        if (ALeft <> 0) and (AWidth > 0) then
          inherited SetBounds(0, 0, TAdvToolPanelTab(Parent).Width - tw,
            TAdvToolPanelTab(Parent).Height)
        else
          inherited SetBounds(ALeft, ATop, AWidth, AHeight);
      end
      else if TAdvToolPanelTab(Parent).Position = ppTop then
      begin
        if (ATop <> tw) and (AHeight > 0) then
          inherited SetBounds(0, tw, TAdvToolPanelTab(Parent).Width,
            TAdvToolPanelTab(Parent).Height - tw)
        else
          inherited SetBounds(ALeft, ATop, AWidth, AHeight);
      end
      else if TAdvToolPanelTab(Parent).Position = ppBottom then
      begin
        if (ATop <> 0) and (AHeight > 0) then
          inherited SetBounds(0, 0, TAdvToolPanelTab(Parent).Width,
            TAdvToolPanelTab(Parent).Height - tw)
        else
          inherited SetBounds(ALeft, ATop, AWidth, AHeight);
      end;
    end
    else
      inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  end
  else
    inherited SetBounds(ALeft, ATop, AWidth, AHeight);
end;


procedure TAdvToolPanel.SetCaptionButton(const Value: boolean);
begin
  FCaptionButton := Value;
  Invalidate;
end;

procedure TAdvToolPanel.SetCaptionEx(const Value: string);
begin
  inherited Caption := Value;
  Invalidate;
//  Parent.Repaint;
//  Width := Width + 1;
//  Width := Width - 1;
  if Assigned(Tab) then
    Tab.UpdatePanel(Self);
end;

procedure TAdvToolPanel.SetCaptionGradientDirection(
  const Value: TGradientDirection);
begin
  FCaptionGradientDirection := Value;
  Invalidate;
end;

procedure TAdvToolPanel.SetCaptionHeight(const Value: integer);
begin
  if (FCaptionHeight <> Value) then
  begin
    FCaptionHeight := Value;
    Invalidate;
  end;
end;

procedure TAdvToolPanel.SetColorTo(const Value: TColor);
begin
  FColorTo := Value;
  Invalidate;
end;

procedure TAdvToolPanel.SetDockDots(const Value: boolean);
begin
  FDockDots := Value;
  Invalidate;
end;

procedure TAdvToolPanel.SetDocking(const Value: Boolean);
begin
  FDocking := Value;
  if Assigned(Tab) then
    Tab.Invalidate;
end;

procedure TAdvToolPanel.SetFocusCaptionColor(const Value: TColor);
begin
  FFocusCaptionColor := Value;
  Invalidate;
end;

procedure TAdvToolPanel.SetFocusCaptionColorTo(const Value: TColor);
begin
  FFocusCaptionColorTo := Value;
  Invalidate;
end;

procedure TAdvToolPanel.SetFocusCaptionFontColor(const Value: TColor);
begin
  FFocusCaptionFontColor := Value;
  Invalidate;
end;

procedure TAdvToolPanel.SetGradientDirection(
  const Value: TGradientDirection);
begin
  FGradientDirection := Value;
  Invalidate;
end;

procedure TAdvToolPanel.SetHidden(const Value: Boolean);
begin
  FHidden := Value;
//Visible := not Value;

  if Assigned(Tab) then
  begin
    Tab.FRollOutPanel := -1;
    Tab.UpdatePanels(-1);
  end;
end;

procedure TAdvToolPanel.SetImageIndex(const Value: Integer);
begin
  FImageIndex := Value;
  Parent.Repaint;
  Width := Width + 1;
  Width := Width - 1;
  if Assigned(Tab) then
    Tab.UpdatePanel(Self);
end;

procedure TAdvToolPanel.SetLocked(const Value: Boolean);
var
  r: TRect;

begin
  if Assigned(Tab) and Value and not Visible then
    Tab.IRollOut(Self);

  FLocked := Value;
  RecreateWnd;
  r := ClientRect;
  r := Rect(r.Right - 42, r.Top, r.Right, r.Top + 21);
  InvalidateRect(Handle, @r, False);

  if Assigned(Tab) then
    Tab.LockChange(self);

  FitInAlignedControls;
end;

procedure TAdvToolPanel.SetNoFocusCaptionColor(const Value: TColor);
begin
  FNoFocusCaptionColor := Value;
  Invalidate;
end;

procedure TAdvToolPanel.SetNoFocusCaptionColorTo(const Value: TColor);
begin
  FNoFocusCaptionColorTo := Value;
  Invalidate;
end;

procedure TAdvToolPanel.SetNoFocusCaptionFontColor(const Value: TColor);
begin
  FNoFocusCaptionFontColor := Value;
  Invalidate;
end;

procedure TAdvToolPanel.SetOpenWidth(const Value: Integer);
begin
  FOpenWidth := Value;
{$IFDEF TMSDEBUG}
  outputdebugstring(pchar('panelwidth set openwidth:' + inttostr(value) + ':' + inttostr(width)));
{$ENDIF}
  if Assigned(Tab) then
    Tab.UpdatePanel(Self);

end;

{
procedure TAdvToolPanel.SetSectionColor(const Value: TColor);
begin
FSectionColor := Value;
end;

procedure TAdvToolPanel.SetSectionColorTo(const Value: TColor);
begin
FSectionColorTo := Value;
end;
}
procedure TAdvToolPanel.SetShowCaption(const Value: Boolean);
begin
  FShowCaption := Value;
  Invalidate;
end;

procedure TAdvToolPanel.SetShowCaptionBorder(const Value: boolean);
begin
  FShowCaptionBorder := Value;
  Invalidate;
end;

procedure TAdvToolPanel.SetShowClose(const Value: Boolean);
begin
  FShowClose := Value;
  Invalidate;
end;

procedure TAdvToolPanel.SetShowLock(const Value: Boolean);
begin
  FShowLock := Value;
  Invalidate;
end;

procedure TAdvToolPanel.UpdateControlPos(SectionIndex, Diff: integer);
var
  R: TRect;
  i, j: integer;
  UpperSectionControl: Boolean;
begin
  R := SectionRect(SectionIndex, False);

  for i := 0 to ControlCount - 1 do
  begin
    if diff > 0 then
    begin
      if (Controls[i].Top >= R.Top) and (Controls[i].Top <= R.Bottom) and (not Controls[i].Visible)
        and (TAdvToolPanelSection(Sections.Items[SectionIndex]).ControlList.IndexOf(Controls[i]) >= 0) then
      begin
        Controls[i].Visible := True;
      end
      else if Controls[i].Top >= R.Top then
      begin
        if Controls[i].Visible then
          Controls[i].Top := Controls[i].Top + diff
        else
        begin
          UpperSectionControl := false;
          for j := 0 to SectionIndex - 1 do
          begin
            if (TAdvToolPanelSection(Sections.Items[j]).ControlList.IndexOf(Controls[i]) >= 0) then
            begin
              UpperSectionControl := true;
              break;
            end;
          end;
          if not UpperSectionControl then
            Controls[i].Top := Controls[i].Top + diff;
        end;
      end;
    end
    else if diff < 0 then
    begin
      if (Controls[i].Top >= R.Top) and (Controls[i].Top <= R.Bottom) and (Controls[i].Visible) then
      begin
        TAdvToolPanelSection(Sections.Items[SectionIndex]).ControlList.Add(Controls[i]);
        Controls[i].Visible := False;
      end
      else if Controls[i].Top >= R.Bottom then
      begin
        if Controls[i].Visible then
          Controls[i].Top := Controls[i].Top + diff
        else
        begin
          UpperSectionControl := False;
          for j := 0 to SectionIndex - 1 do
          begin
            if (TAdvToolPanelSection(Sections.Items[j]).ControlList.IndexOf(Controls[i]) >= 0) then
            begin
              UpperSectionControl := true;
              Break;
            end;
          end;
          if not UpperSectionControl then
            Controls[i].Top := Controls[i].Top + diff
        end;
      end;
    end;
  end;

  if Diff > 0 then
  begin
    TAdvToolPanelSection(Sections.Items[SectionIndex]).ControlList.Clear;
    if Assigned(FOnNodeExpand) then
      FOnNodeExpand(self, SectionIndex);
  end
  else if Diff < 0 then
  begin
    if Assigned(FOnNodeCollapse) then
      FOnNodeCollapse(self, SectionIndex);
  end;

  if (Diff <> 0) then
  begin
    // force full repaint in case transparent controls are used
    Width := Width + 1;
    Width := Width - 1;
  end;
end;


procedure TAdvToolPanel.UpdateControlPosBelowSection(SectionIndex, Diff: integer);
var
  R: TRect;
  i: integer;
begin
  R := SectionRect(SectionIndex, false);
  for i := 0 to controlcount - 1 do
  begin
    if (Controls[i].Top >= R.Bottom) then
    begin
      Controls[i].Top := Controls[i].Top + diff;
    end;
{ else if diff < 0 then
begin
  if (Controls[i].Top >= R.Top) and (Controls[i].Top<= R.Bottom) and (Controls[i].Visible) then
  begin
    Controls[i].Visible:= false;
  end
  else if Controls[i].Top >= R.Bottom then
  begin
    Controls[i].Top:= Controls[i].Top + diff;
  end;
end;   }
  end;
end;

procedure TAdvToolPanel.WMENTERSIZEMOVE(var Msg: TMessage);
begin
  inherited;
  if Assigned(Tab) then
    Tab.EnableTimer(False);
end;

procedure TAdvToolPanel.WMEraseBkGnd(var Message: TMessage);
begin
  Message.Result := 1;
end;

procedure TAdvToolPanel.WMEXITSIZEMOVE(var Msg: TMessage);
begin
  inherited;

  if Assigned(Tab) then
  begin
    Tab.EnableTimer(True);
    if Assigned(Tab.Parent) then
      Tab.Parent.Realign;
  end;

  FitInAlignedControls;
end;

procedure TAdvToolPanel.FitInAlignedControls;
var
  frm: TCustomForm;
  i,d: integer;
begin
  if Tab.Position = ppBottom then
  begin
    if (Tab.Owner is TCustomForm) then
    begin
      frm := Tab.Owner as TCustomForm;

      d := 0;
      for i := 0 to frm.ControlCount - 1 do
      begin
        if (frm.Controls[i].Visible) and (frm.Controls[i].Align = alTop) then
          d := d + frm.Controls[i].Height;
      end;

      if frm.ClientHeight - d - Tab.Height < Height then
      begin
        Height := frm.ClientHeight - d - Tab.Height;
      end;
    end;
  end;
end;

procedure TAdvToolPanel.WMLDblClk(var Msg: TWMLButtonDblClk);
var
  h: integer;
begin
  inherited;

  if Floating or not FShowCaption then
    h := 1
  else
    h := FCaptionHeight;

  if (Msg.YPos < h) then
  begin
    if Assigned(OnCaptionDblClick) then
      OnCaptionDblClick(self);
  end
  else
    if Assigned(OnDblClick) then
      OnDblClick(self);
end;

procedure TAdvToolPanel.WMPaint(var Message: TWMPaint);
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


procedure TAdvToolPanel.WMNCHitTest(var Msg: TWMNCHitTest);
var
  pt: TPoint;
begin
  inherited;

  if (csDesigning in ComponentState) then
    Exit;

  if Floating then
    Exit;

  if not Assigned(Tab) then
    Exit;

  if not Locked then
    Tab.ResetTimer;

  pt := ScreenToClient(point(Msg.XPos, Msg.YPos));

  if (Tab.Position = ppLeft) and (CanSize) then
    if (pt.X >= Width - 3) and (pt.X <= Width + 2) then
    begin
      Msg.Result := HTRIGHT;
    end;

  if (Tab.Position = ppRight) and (CanSize) and Locked then
    if (pt.X <= 2) and (pt.X >= -3) then
    begin
      Msg.Result := HTLEFT;
    end;

  if (Tab.Position = ppTop) and (CanSize) then
    if (pt.Y >= Height - 2) and (pt.Y <= Height + 2) then
    begin
      Msg.Result := HTBOTTOM;
    end;

  if (Tab.Position = ppBottom) and (CanSize) and Locked then
  begin
    if (pt.Y <= 2) and (pt.Y >= -2) then
    begin
      Msg.Result := HTTOP;
    end;
  end;
end;

procedure TAdvToolPanel.WMSetCursor(var Message: TWMSetCursor);
var
  P: TPoint;
  i, h: Integer;
begin
  if (csDesigning in ComponentState) and FSizing then
    Exit;

  inherited;

  if (csDesigning in ComponentState) and (Sections.Count > 0) then
  begin
    GetCursorPos(P);
    P := ScreenToClient(P);

    if Floating or not FShowCaption then
      h := 1
    else
      h := FCaptionHeight;

    Windows.SetCursor(Screen.Cursors[Cursor]);

    for i := 1 to Sections.Count do
    begin
      if (P.Y > h) and (P.Y < h + SectionHeight) then
      begin
        if (i > 1) and TAdvToolPanelSection(Sections.Items[i - 1]).Draw then
          Windows.SetCursor(Screen.Cursors[crVSplit]);
        Break;
      end;
      h := h + TAdvToolPanelSection(Sections.Items[i - 1]).Height;
    end;
  end;
end;

{ TToolPanelForm }

constructor TToolPanelForm.Create(AOwner: TComponent);
begin
  inherited CreateNew(AOwner);
  OnShow := FormShow;
  OnActivate := FormActivate;
end;

procedure TToolPanelForm.FormShow(Sender: TObject);
var
  ExtendedStyle : Integer;
begin
  ExtendedStyle := GetWindowLong(Self.Handle, GWL_EXSTYLE);
  SetWindowLong(Handle, GWL_EXSTYLE, ExtendedStyle OR WS_EX_TOOLWINDOW AND NOT WS_EX_APPWINDOW);
end;

function TToolPanelForm.GetParentWnd: HWnd;
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

procedure TToolPanelForm.FormActivate(Sender: TObject);
var
  ExtendedStyle : Integer;
begin
  ExtendedStyle := GetWindowLong(Self.Handle, GWL_EXSTYLE);
  SetWindowLong(Handle, GWL_EXSTYLE, ExtendedStyle OR WS_EX_TOOLWINDOW AND NOT WS_EX_APPWINDOW);
end;

procedure TToolPanelForm.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.ExStyle := Params.ExStyle or WS_EX_NOACTIVATE;
end;

destructor TToolPanelForm.Destroy;
begin
  inherited;
end;

procedure TToolPanelForm.WMActivate(var Message: TWMActivate);
begin
  if Message.Active <> integer(False) then
  begin
    SendMessage(GetParentWnd, WM_NCACTIVATE, 1, 0);
  end;
end;

procedure TToolPanelForm.WMEnterSizeMove(var Msg: TMessage);
begin
  inherited;
  if Assigned(Tab) then
    Tab.EnableTimer(false);
end;

procedure TToolPanelForm.WMExitSizeMove(var Msg: TMessage);
begin
  inherited;
  if Assigned(Tab) then
    Tab.EnableTimer(true);
end;


procedure TToolPanelForm.WMGetMinMaxInfo(var Msg: TMessage);
var
  info: ^TMinMaxInfo;
  frm: TWinControl;
  mxw,mxh: integer;
begin
  inherited;

  if not (Tab.Position in [ppRight, ppBottom]) then
    Exit;

  frm := Tab.Parent;

  mxw := 0;
  mxh := 0;

  if Tab.Position = ppBottom then
  begin
    mxw := frm.ClientWidth;
    mxh := frm.ClientHeight - Tab.Height;
  end;

  if Tab.Position = ppRight then
  begin
    mxh := frm.ClientHeight;
    mxw := frm.ClientWidth - Tab.Width;
  end;

  info := pointer(Msg.LParam);

  info^.ptMaxSize.X := mxw;
  info^.ptMaxSize.Y := mxh;
  info^.ptMaxTrackSize.X := mxw;
  info^.ptMaxTrackSize.Y := mxh;

  msg.Result := 0;
end;

procedure TToolPanelForm.WMNCHitTest(var Msg: TWMNCHitTest);
var
  pt: TPoint;
begin
  if not Assigned(Tab) then
    Exit;

  Tab.ResetTimer;

  pt := ScreenToClient(Point(Msg.Xpos, Msg.Ypos));

  if Tab.Position = ppRight then
  begin
    if (Tab.FRollOutPanel <> -1) and (Tab.Panels[Tab.FRollOutPanel].Panel.CanSize) then
    if (pt.X <= 2) and (pt.X > -3) then
    begin
      Msg.Result := HTLEFT;
    end;
  end;

  if Tab.Position = ppBottom then
  begin
    if (Tab.FRollOutPanel <> -1) and (Tab.Panels[Tab.FRollOutPanel].Panel.CanSize) then
    if pt.Y <= 2 then
    begin
      Msg.Result := HTTOP;
    end;
  end;
end;

{ TToolPanelGlyphs }

procedure TToolPanelGlyphs.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

constructor TToolPanelGlyphs.Create;
begin
  inherited;
  FCloseGlyphNormal := TBitmap.Create;
  FCloseGlyphDown := TBitmap.Create;
  FCloseGlyphHot := TBitmap.Create;
  FLockGlyphNormal := TBitmap.Create;
  FLockGlyphDown := TBitmap.Create;
  FLockGlyphHot := TBitmap.Create;
  FUnLockGlyphNormal := TBitmap.Create;
  FUnLockGlyphDown := TBitmap.Create;
  FUnLockGlyphHot := TBitmap.Create;
end;

destructor TToolPanelGlyphs.Destroy;
begin
  FCloseGlyphNormal.Free;
  FCloseGlyphHot.Free;
  FCloseGlyphDown.Free;
  FLockGlyphNormal.Free;
  FLockGlyphHot.Free;
  FLockGlyphDown.Free;
  FUnLockGlyphNormal.Free;
  FUnLockGlyphHot.Free;
  FUnLockGlyphDown.Free;
  inherited;
end;

procedure TToolPanelGlyphs.SetCloseGlyphDown(const Value: TBitmap);
begin
  FCloseGlyphDown.Assign(Value);
  Changed;
end;

procedure TToolPanelGlyphs.SetCloseGlyphHot(const Value: TBitmap);
begin
  FCloseGlyphHot.Assign(Value);
  Changed;
end;

procedure TToolPanelGlyphs.SetCloseGlyphNormal(const Value: TBitmap);
begin
  FCloseGlyphNormal.Assign(Value);
  Changed;
end;

procedure TToolPanelGlyphs.SetLockGlyphDown(const Value: TBitmap);
begin
  FLockGlyphDown.Assign(Value);
  Changed;
end;

procedure TToolPanelGlyphs.SetLockGlyphHot(const Value: TBitmap);
begin
  FLockGlyphHot.Assign(Value);
  Changed;
end;

procedure TToolPanelGlyphs.SetLockGlyphNormal(const Value: TBitmap);
begin
  FLockGlyphNormal.Assign(Value);
  Changed;
end;


procedure TToolPanelGlyphs.SetUnLockGlyphDown(const Value: TBitmap);
begin
  FUnLockGlyphDown.Assign(Value);
  Changed;
end;

procedure TToolPanelGlyphs.SetUnLockGlyphHot(const Value: TBitmap);
begin
  FUnLockGlyphHot.Assign(Value);
  Changed;
end;

procedure TToolPanelGlyphs.SetUnLockGlyphNormal(const Value: TBitmap);
begin
  FUnLockGlyphNormal.Assign(Value);
  Changed;
end;


{ TSectionLayout }

procedure TSectionLayout.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

constructor TSectionLayout.Create;
begin
  inherited;
  FBorderColor := clWhite; //clGray;
  FBorderWidth := 1;
  FCaptionColor := $00F7DEC6; //clBtnFace;
  FCaptionColorTo := $00D68C63; //clBtnFace;
  FBackGroundColor := $00F7EFDE; //clNone;
  FBackGroundColorTo := $00F7D6BD; //clNone;
  FCaptionGradientDirection := gdHorizontal;
  FBackGroundGradientDir := gdVertical;
  FItemFontColor := $00B53900; //clBlack;
  FItemHoverTextColor := $00B53900; //clBlack;
  FItemHoverUnderline := true;
  FCaptionFontColor := $00842900; //clBlack;
  FCorners := scRectangle;
  FIndent := 4;
  FSpacing := 4;
  FCaptionRounded := false;
end;

destructor TSectionLayout.Destroy;
begin

  inherited;
end;

procedure TSectionLayout.SetBackGroundColor(const Value: TColor);
begin
  FBackGroundColor := Value;
  Changed;
end;

procedure TSectionLayout.SetBackGroundColorTo(const Value: TColor);
begin
  FBackGroundColorTo := Value;
  Changed;
end;

procedure TSectionLayout.SetBackGroundGradientDir(
  const Value: TGradientDirection);
begin
  FBackGroundGradientDir := Value;
  Changed;
end;

procedure TSectionLayout.SetBorderColor(const Value: TColor);
begin
  FBorderColor := Value;
  Changed;
end;

procedure TSectionLayout.SetBorderWidth(const Value: integer);
begin
  FBorderWidth := Value;
  Changed;
end;

procedure TSectionLayout.SetCaptionColor(const Value: TColor);
begin
  FCaptionColor := Value;
  Changed;
end;

procedure TSectionLayout.SetCaptionColorTo(const Value: TColor);
begin
  FCaptionColorTo := Value;
  Changed;
end;

procedure TSectionLayout.SetCaptionFontColor(const Value: TColor);
begin
  FCaptionFontColor := Value;
  Changed;
end;

procedure TSectionLayout.SetCaptionGradientDirection(
  const Value: TGradientDirection);
begin
  FCaptionGradientDirection := Value;
  Changed;
end;

procedure TSectionLayout.SetCaptionRounded(const Value: Boolean);
begin
  FCaptionRounded := Value;
  Changed;
end;

procedure TSectionLayout.SetCorners(const Value: TSectionCorners);
begin
  FCorners := Value;
  Changed;
end;

procedure TSectionLayout.SetIndent(const Value: integer);
begin
  FIndent := Value;
  Changed;
end;

procedure TSectionLayout.SetItemFontColor(const Value: TColor);
begin
  FItemFontColor := Value;
  Changed;
end;

procedure TSectionLayout.SetItemHoverTextColor(const Value: TColor);
begin
  FItemHoverTextColor := Value;
  Changed;
end;

procedure TSectionLayout.SetItemHoverUnderline(const Value: Boolean);
begin
  FItemHoverUnderline := Value;
  Changed;
end;

procedure TSectionLayout.SetSpacing(const Value: integer);
begin
  FSpacing := Value;
  Changed;
end;

procedure TSectionLayout.SetUnderLineCaption(const Value: Boolean);
begin
  FUnderLineCaption := Value;
  Changed;
end;

{ TSectionItem }

procedure TSectionItem.Assign(Source: TPersistent);
begin
  if (Source is TSectionItem) then
  begin
    FObject  := (Source as TSectionItem).ItemObject;
    FCaption := (Source as TSectionItem).Caption;
    FEnabled := (Source as TSectionItem).Enabled;
    FHint := (Source as TSectionItem).Hint;
    FImageIndex := (Source as TSectionItem).ImageIndex;
    FTag := (Source as TSectionItem).Tag;
  end;
end;

constructor TSectionItem.Create(Collection: TCollection);
begin
  inherited;
  FImageIndex := -1;
  FHint := '';
  FCaption := 'Untitled';
  FEnabled := true;

  if Assigned(TSectionItems(Collection).OnItemAdd) then
    TSectionItems(Collection).OnItemAdd(TSectionItems(Collection));

{$IFDEF DELPHI6_LVL}
  TAdvToolPanel(Collection.Owner).Invalidate;
{$ELSE}
  TSectionItems(Collection).FOwner.Invalidate;
{$ENDIF}

end;

destructor TSectionItem.Destroy;
begin
  if Assigned(TSectionItems(Collection).OnItemDelete) then
    TSectionItems(Collection).OnItemDelete(TSectionItems(Collection));

{$IFDEF DELPHI6_LVL}
  TAdvToolPanel(Collection.Owner).Invalidate;
{$ELSE}
  TSectionItems(Collection).FOwner.Invalidate;
{$ENDIF}
  inherited;
end;

function TSectionItem.GetDisplayName: string;
begin
  Result := 'Item ' + IntToStr(Index) + ': ' + Caption;
end;

procedure TSectionItem.SetCaption(const Value: string);
begin
  if (FCaption <> Value) then
  begin
    FCaption := Value;
{$IFDEF DELPHI6_LVL}
    TAdvToolPanel(Collection.Owner).Invalidate;
{$ELSE}
    TSectionItems(Collection).FOwner.Invalidate;
{$ENDIF}
  end;
end;

procedure TSectionItem.SetEnabled(const Value: boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
{$IFDEF DELPHI6_LVL}
    TAdvToolPanel(Collection.Owner).Invalidate;
{$ELSE}
    TSectionItems(Collection).FOwner.Invalidate;
{$ENDIF}
  end;
end;

procedure TSectionItem.SetImageIndex(const Value: Integer);
begin
  if (FImageIndex <> Value) then
  begin
    FImageIndex := Value;
    TSectionItems(Collection).AdvToolPanelSection.Autosize := TSectionItems(Collection).AdvToolPanelSection.Autosize;
{$IFDEF DELPHI6_LVL}
    TAdvToolPanel(Collection.Owner).Invalidate;
{$ELSE}
    TSectionItems(Collection).FOwner.Invalidate;
{$ENDIF}
  end;
end;

{ TSectionItems }

function TSectionItems.Add: TSectionItem;
begin
  Result := TSectionItem(inherited Add);
end;

constructor TSectionItems.Create(AOwner: TAdvToolPanel);
begin
  inherited Create(TSectionItem);
  FOwner := AOwner;
end;

function TSectionItems.GetItem(Index: Integer): TSectionItem;
begin
  Result := TSectionItem(inherited Items[Index]);
end;

function TSectionItems.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TSectionItems.Insert(Index: Integer): TSectionItem;
begin
  Result := TSectionItem(inherited Insert(Index));
end;

procedure TSectionItems.SetItem(Index: Integer; const Value: TSectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TPanelPersistence }

procedure TPanelPersistence.DoChange;
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

procedure TPanelPersistence.SetINIFile(const Value: string);
begin
  if (FINIFile <> Value) then
  begin
    FINIFile := Value;
    DoChange;
  end;

end;

procedure TPanelPersistence.SetLocation(const Value: TPanelPersistLocation);
begin
  if (FLocation <> Value) then
  begin
    FLocation := Value;
    DoChange;
  end;
end;

procedure TPanelPersistence.SetRegistryKey(const Value: string);
begin
  if (FRegistryKey <> Value) then
  begin
    FRegistryKey := Value;
    DoChange;
  end;
end;

end.
