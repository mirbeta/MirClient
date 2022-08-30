{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressStatusBar                                         }
{                                                                    }
{           Copyright (c) 1998-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSSTATUSBAR AND ALL              }
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

unit dxStatusBar;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Graphics, ImgList, Forms, ExtCtrls,
  dxThemeManager, dxCore, cxClasses, cxGraphics, cxGeometry, cxControls, cxLookAndFeels,
  cxLookAndFeelPainters;

type
  TdxStatusBarPainterClass = class of TdxStatusBarPainter;
  TdxCustomStatusBar = class;
  TdxStatusBarPanel = class;
  TdxStatusBarPanelClass = class of TdxStatusBarPanel;
  TdxStatusBarPanels = class;
  TdxStatusBarPanelsClass = class of TdxStatusBarPanels;
  TdxStatusBarPanelStyle = class;

  { TdxStatusBarPanelStyle }

  TdxStatusBarPanelStyleClass = class of TdxStatusBarPanelStyle;
  TdxStatusBarPanelStyle = class(TInterfacedPersistent, IdxScaleFactor)
  private
    FAlignment: TAlignment;
    FColor: TColor;
    FFont: TFont;
    FIsColorAssigned: Boolean;
    FIsRightToLeftConverted: Boolean;
    FOwner: TdxStatusBarPanel;
    FParentFont: Boolean;

    procedure FontChanged(Sender: TObject);
    function GetColor: TColor;
    function GetPainter: TdxStatusBarPainterClass;
    function GetScaleFactor: TdxScaleFactor;
    function GetStatusBarControl: TdxCustomStatusBar;
    function IsColorStored: Boolean;
    function IsFontStored: Boolean;
    procedure SetAlignment(Value: TAlignment);
    procedure SetColor(Value: TColor);
    procedure SetFont(Value: TFont);
    procedure SetParentFont(Value: Boolean);
  protected
    procedure AdjustTextColor(var AColor: TColor; Active: Boolean); virtual;
    procedure BoundsChanged(const ABounds: TRect); virtual;
    function CanDelete: Boolean; virtual;
    function CanSizing: Boolean; virtual;
    procedure Changed; virtual;
    procedure ChangeScale(M: Integer; D: Integer);
    procedure CheckSizeGripRect(var R: TRect); virtual;
    function DefaultColor: TColor; virtual;
    procedure DoAssign(ASource: TPersistent); virtual;
    procedure DoRightToLeftConversion(const ABounds: TRect); virtual;
    procedure DrawContent(ACanvas: TcxCanvas; R: TRect; APainter: TdxStatusBarPainterClass); virtual;
    function GetMinWidth: Integer; virtual;
    function GetPanelFixed: Boolean;
    class function GetVersion: Integer; virtual;
    function InternalBevel: Boolean; virtual;
    procedure Loaded; virtual;
    procedure ParentFontChanged; virtual;
    procedure UpdateResources; virtual;
    procedure UpdateSubControlsVisibility; virtual;

    property ScaleFactor: TdxScaleFactor read GetScaleFactor;
  public
    constructor Create(AOwner: TdxStatusBarPanel); virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure RestoreDefaults; virtual;
    procedure RightToLeftConversion(const ABounds: TRect);

    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property Color: TColor read GetColor write SetColor stored IsColorStored;
    property Font: TFont read FFont write SetFont stored IsFontStored;
    property Owner: TdxStatusBarPanel read FOwner;
    property Painter: TdxStatusBarPainterClass read GetPainter;
    property ParentFont: Boolean read FParentFont write SetParentFont default True;
    property StatusBarControl: TdxCustomStatusBar read GetStatusBarControl;
  end;

  { TdxStatusBarTextPanelStyle }

  TdxStatusBarEllipsisType = (dxetNone, dxetTruncate, dxetSmartPath);

  TdxStatusBarTextEvent = procedure(Sender: TObject; const R: TRect; var AText: string) of object;

  TdxStatusBarTextPanelStyle = class(TdxStatusBarPanelStyle)
  strict private
    FAutoHint: Boolean;
    FEllipsisType: TdxStatusBarEllipsisType;
    FImageIndex: TcxImageIndex;

    FOnGetText: TdxStatusBarTextEvent;

    procedure SetAutoHint(Value: Boolean);
    procedure SetEllipsisType(Value: TdxStatusBarEllipsisType);
    procedure SetImageIndex(Value: TcxImageIndex);
  protected
    procedure DoAssign(ASource: TPersistent); override;
    procedure DrawContent(ACanvas: TcxCanvas; R: TRect; APainter: TdxStatusBarPainterClass); override;
  public
    constructor Create(AOwner: TdxStatusBarPanel); override;
    procedure RestoreDefaults; override;
  published
    property Alignment;
    property AutoHint: Boolean read FAutoHint write SetAutoHint default False;
    property Color;
    property ImageIndex: TcxImageIndex read FImageIndex write SetImageIndex default -1;
    property EllipsisType: TdxStatusBarEllipsisType read FEllipsisType write SetEllipsisType default dxetNone;
    property Font;
    property ParentFont;
    property OnGetText: TdxStatusBarTextEvent read FOnGetText write FOnGetText;
  end;

  { TdxStatusBarStateIndicatorPanelStyle }

  TdxStatusBarStateIndicatorPanelStyle = class;

  TdxStatusBarStateIndicatorType = (sitOff, sitYellow, sitBlue, sitGreen, sitRed, sitTeal, sitPurple);

  TdxStatusBarStateIndicatorItem = class(TCollectionItem)
  strict private
    FIndicatorBitmap: TBitmap;
    FIndicatorType: TdxStatusBarStateIndicatorType;
    FVisible: Boolean;

    procedure SetIndicatorType(Value: TdxStatusBarStateIndicatorType);
    procedure SetVisible(Value: Boolean);
  protected
    function CanDisplay: Boolean;
    procedure UpdateResources;
    //
    property IndicatorBitmap: TBitmap read FIndicatorBitmap;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property IndicatorType: TdxStatusBarStateIndicatorType read FIndicatorType write SetIndicatorType default sitOff;
    property Visible: Boolean read FVisible write SetVisible default True;
  end;

  { TdxStatusBarStateIndicators }

  TdxStatusBarStateIndicators = class(TOwnedCollection)
  strict private
    FOnChange: TNotifyEvent;

    function GetItem(Index: Integer): TdxStatusBarStateIndicatorItem;
    procedure SetItem(Index: Integer; Value: TdxStatusBarStateIndicatorItem);
  protected
    procedure Update(Item: TCollectionItem); override;
    procedure UpdateResources;
  public
    constructor Create(AOwner: TPersistent = nil);
    function Add: TdxStatusBarStateIndicatorItem;
    function Insert(AIndex: Integer): TdxStatusBarStateIndicatorItem;
    property Items[Index: Integer]: TdxStatusBarStateIndicatorItem read GetItem write SetItem; default;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  { TdxStatusBarStateIndicatorPanelStyle }

  TdxStatusBarStateIndicatorPanelStyle = class(TdxStatusBarPanelStyle)
  strict private
    FIndicators: TdxStatusBarStateIndicators;
    FSpacing: Integer;

    procedure IndicatorChangeHandler(Sender: TObject);
    procedure SetIndicators(Value: TdxStatusBarStateIndicators);
    procedure SetSpacing(Value: Integer);
  protected
    function CanSizing: Boolean; override;
    procedure DoAssign(ASource: TPersistent); override;
    procedure DrawContent(ACanvas: TcxCanvas; R: TRect; APainter: TdxStatusBarPainterClass); override;
    function GetMinWidth: Integer; override;
    procedure UpdateResources; override;
  public
    constructor Create(AOwner: TdxStatusBarPanel); override;
    destructor Destroy; override;
    procedure RestoreDefaults; override;
  published
    property Color;
    property Indicators: TdxStatusBarStateIndicators read FIndicators write SetIndicators;
    property Spacing: Integer read FSpacing write SetSpacing default 4;
  end;

  { TdxStatusBarKeyboardStatePanelStyle }

  TdxStatusBarKeyboardStateWatchedKey = class
  strict private
    FKeyCode: Integer;
    FKeyState: Integer;

    procedure SetKeyState(Value: Integer);
  public
    constructor Create(AKeyCode: Integer);
    function GetCurrentState: Integer;
    //
    property KeyCode: Integer read FKeyCode;
    property KeyState: Integer read FKeyState write SetKeyState;
  end;

  { TdxStatusBarKeyboardStateNotifier }

  TdxStatusBarKeyboardStateNotifier = class
  private
    FKeys: array of TdxStatusBarKeyboardStateWatchedKey;
    FStatusBar: TdxCustomStatusBar;
    FTimer: TTimer;
  protected
    procedure Execute(Sender: TObject);
  public
    constructor Create(AStatusBar: TdxCustomStatusBar);
    destructor Destroy; override;
    procedure SubScribeKey(AKeyCode: Integer);
    procedure UnSubScribeKey(AKeyCode: Integer);
  end;

  { TdxStatusBarKeyStateAppearance }

  TdxStatusBarKeyboardState = (dxksCapsLock, dxksNumLock, dxksScrollLock, dxksInsert);
  TdxStatusBarKeyboardStates = set of TdxStatusBarKeyboardState;

  TdxStatusBarKeyStateAppearance = class(TPersistent)
  private
    FActiveCaption: string;
    FActiveFontColor: TColor;
    FCode: Integer;
    FId: TdxStatusBarKeyboardState;
    FInactiveCaption: string;
    FInactiveFontColor: TColor;

    FOnChange: TNotifyEvent;

    procedure SetActiveFontColor(Value: TColor);
    procedure SetInactiveFontColor(Value: TColor);
    procedure SetActiveCaption(const Value: string);
    procedure SetInactiveCaption(const Value: string);
  protected
    procedure Changed; virtual;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    constructor Create(AId: TdxStatusBarKeyboardState; ACode: Integer;
      const AActiveCaption: string; AActiveFontColor: TColor;
      const AInactiveCaption: string; AInactiveFontColor: TColor; AChangeHandler: TNotifyEvent);
    procedure Assign(Source: TPersistent); override;
    function GetRectWidth(ACanvas: TcxCanvas): Integer; overload;
    function GetRectWidth(ACanvas: TcxCanvas; AScaleFactor: TdxScaleFactor): Integer; overload;

    property Code: Integer read FCode;
    property Id: TdxStatusBarKeyboardState read FId;
  published
    property ActiveFontColor: TColor read FActiveFontColor write SetActiveFontColor default clWindowText;
    property InactiveFontColor: TColor read FInactiveFontColor write SetInactiveFontColor default clBtnShadow;
    property ActiveCaption: string read FActiveCaption write SetActiveCaption;
    property InactiveCaption: string read FInactiveCaption write SetInactiveCaption;
  end;

  { TdxStatusBarKeyboardStatePanelStyle }

  TdxStatusBarKeyboardStatePanelStyle = class(TdxStatusBarPanelStyle)
  strict private
    FFullRect: Boolean;
    FKeyboardStates: TdxStatusBarKeyboardStates;
    FKeyInfos: array[0..3] of TdxStatusBarKeyStateAppearance;
    FNotifier: TdxStatusBarKeyboardStateNotifier;

    function GetAppearance(Index: Integer): TdxStatusBarKeyStateAppearance;
    procedure NamesChangeHandler(Sender: TObject);
    procedure SetAppearance(Index: Integer; Value: TdxStatusBarKeyStateAppearance);
    procedure SetFullRect(Value: Boolean);
    procedure SetKeyboardStates(Value: TdxStatusBarKeyboardStates);
  protected
    function CanSizing: Boolean; override;
    procedure DoAssign(ASource: TPersistent); override;
    procedure DrawContent(ACanvas: TcxCanvas; R: TRect; APainter: TdxStatusBarPainterClass); override;
    function GetLastKeyIndex: Integer;
    function GetMinWidth: Integer; override;
    function InternalBevel: Boolean; override;
  public
    constructor Create(AOwner: TdxStatusBarPanel); override;
    destructor Destroy; override;
    procedure RestoreDefaults; override;
  published
    property Color;
    property Font;
    property KeyboardStates: TdxStatusBarKeyboardStates read FKeyboardStates write SetKeyboardStates default [dxksCapsLock, dxksNumLock, dxksScrollLock, dxksInsert];
    property FullRect: Boolean read FFullRect write SetFullRect default False;

    property CapsLockKeyAppearance: TdxStatusBarKeyStateAppearance index 0 read GetAppearance write SetAppearance;
    property NumLockKeyAppearance: TdxStatusBarKeyStateAppearance index 1 read GetAppearance write SetAppearance;
    property ScrollLockKeyAppearance: TdxStatusBarKeyStateAppearance index 2 read GetAppearance write SetAppearance;
    property InsertKeyAppearance: TdxStatusBarKeyStateAppearance index 3 read GetAppearance write SetAppearance;
    property ParentFont;
  end;

  { TdxStatusBarContainerPanelStyle }

  TdxStatusBarContainerPanelStyle = class;

  TdxStatusBarContainerControl = class(TcxControl)
  private
    FPanelStyle: TdxStatusBarContainerPanelStyle;
  protected
    procedure AlignChildren; virtual;
    procedure BoundsChanged; override;
    procedure Paint; override;
    function MayFocus: Boolean; override;
    function NeedAlignChildren: Boolean; virtual;
    function NeedsScrollBars: Boolean; override;
    //
    procedure CMControlChange(var Message: TCMControlChange); message CM_CONTROLCHANGE;
    property PanelStyle: TdxStatusBarContainerPanelStyle read FPanelStyle;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  { TdxStatusBarContainerPanelStyle }

  TdxStatusBarContainerPanelStyle = class(TdxStatusBarPanelStyle)
  private
    FAlignControl: Boolean;
    FContainer: TdxStatusBarContainerControl;
    FContainerBoundsRect: TRect;

    procedure SetContainer(Value: TdxStatusBarContainerControl);
  protected
    procedure BoundsChanged(const ABounds: TRect); override;
    function CanDelete: Boolean; override;
    procedure DoAssign(ASource: TPersistent); override;
    procedure DoRightToLeftConversion(const ABounds: TRect); override;
    procedure DrawContainerControl;
    procedure UpdateSubControlsVisibility; override;
  public
    constructor Create(AOwner: TdxStatusBarPanel); override;
    destructor Destroy; override;
    procedure RestoreDefaults; override;
  published
    property AlignControl: Boolean read FAlignControl write FAlignControl default True;
    property Container: TdxStatusBarContainerControl read FContainer write SetContainer;
  end;

  { TdxStatusBarPanel }

  TdxStatusBarPanelBevel = (dxpbNone, dxpbLowered, dxpbRaised);
  TdxStatusBarDrawPanelEvent = procedure(Sender: TdxStatusBarPanel; ACanvas: TcxCanvas;
    const ARect: TRect; var ADone: Boolean) of object;

  TdxStatusBarPanel = class(TCollectionItem)
  private
    FBevel: TdxStatusBarPanelBevel;
    FBiDiMode: TBiDiMode;
    FFixed: Boolean;
    FIsMinWidthAssigned: Boolean;
    FIsWidthAssigned: Boolean;
    FMinWidth: Integer;
    FPanelStyle: TdxStatusBarPanelStyle;
    FPanelStyleClass: TdxStatusBarPanelStyleClass;
    FPanelStyleEvents: TNotifyEvent;
    FParentBiDiMode: Boolean;
    FText: string;
    FVisible: Boolean;
    FWidth: Integer;

    FOnClick: TNotifyEvent;
    FOnDblClick: TNotifyEvent;
    FOnDrawPanel: TdxStatusBarDrawPanelEvent;

    function GetFixed: Boolean;
    function GetLookAndFeel: TcxLookAndFeel;
    function GetMinWidth: Integer;
    function GetPanelStyleClassName: string;
    function GetScaleFactor: TdxScaleFactor;
    function GetStatusBarControl: TdxCustomStatusBar;
    function GetWidth: Integer;
    function IsBiDiModeStored: Boolean;
    function IsMinWidthStored: Boolean;
    function IsWidthStored: Boolean;
    procedure SetBevel(Value: TdxStatusBarPanelBevel);
    procedure SetBiDiMode(Value: TBiDiMode);
    procedure SetFixed(Value: Boolean);
    procedure SetMinWidth(Value: Integer);
    procedure SetPanelStyle(Value: TdxStatusBarPanelStyle);
    procedure SetPanelStyleClass(const Value: TdxStatusBarPanelStyleClass);
    procedure SetPanelStyleClassName(Value: string);
    procedure SetParentBiDiMode(Value: Boolean);
    procedure SetText(const Value: string);
    procedure SetVisible(Value: Boolean);
    procedure SetWidth(Value: Integer);
  protected
    procedure ChangeScale(M: Integer; D: Integer);
    procedure Click; virtual;
    procedure CreatePanelStyle; virtual;
    procedure DblClick; virtual;
    function DefaultMinWidth: Integer; virtual;
    function DefaultWidth: Integer; virtual;
    procedure DestroyPanelStyle; virtual;
    function GetDisplayName: string; override;
    procedure Loaded;
    function PaintMinWidth: Integer; virtual;
    function PaintWidth: Integer; virtual;
    procedure PreparePaintWidth(var AWidth: Integer); virtual;
    procedure StatusBarPanelStyleChanged; virtual;
    procedure UpdateResources;
    procedure UpdateSubControlsVisibility; virtual;

    property LookAndFeel: TcxLookAndFeel read GetLookAndFeel;
    property ScaleFactor: TdxScaleFactor read GetScaleFactor;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure RestoreDefaults; virtual;

    procedure ParentBiDiModeChanged;
    function UseRightToLeftAlignment: Boolean;
    function UseRightToLeftReading: Boolean;

    property PanelStyleClass: TdxStatusBarPanelStyleClass read FPanelStyleClass write SetPanelStyleClass;
    property StatusBarControl: TdxCustomStatusBar read GetStatusBarControl;
  published
    property PanelStyleClassName: string read GetPanelStyleClassName write SetPanelStyleClassName;
    property PanelStyle: TdxStatusBarPanelStyle read FPanelStyle write SetPanelStyle;
    property PanelStyleEvents: TNotifyEvent read FPanelStyleEvents write FPanelStyleEvents;

    property Bevel: TdxStatusBarPanelBevel read FBevel write SetBevel default dxpbLowered;
    property BiDiMode: TBiDiMode read FBiDiMode write SetBiDiMode stored IsBiDiModeStored;
    property Fixed: Boolean read GetFixed write SetFixed default True;
    property MinWidth: Integer read GetMinWidth write SetMinWidth stored IsMinWidthStored;
    property ParentBiDiMode: Boolean read FParentBiDiMode write SetParentBiDiMode default True;
    property Text: string read FText write SetText;
    property Visible: Boolean read FVisible write SetVisible default True;
    property Width: Integer read GetWidth write SetWidth stored IsWidthStored;

    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property OnDblClick: TNotifyEvent read FOnDblClick write FOnDblClick;
    property OnDrawPanel: TdxStatusBarDrawPanelEvent read FOnDrawPanel write FOnDrawPanel;
  end;

  { TdxStatusBarPanels }

  TdxStatusBarPanels = class(TCollection)
  private
    FStatusBarControl: TdxCustomStatusBar;

    function GetItem(Index: Integer): TdxStatusBarPanel;
    procedure SetItem(Index: Integer; Value: TdxStatusBarPanel);
  protected
    procedure ChangeScale(M: Integer; D: Integer);
    function GetOwner: TPersistent; override;
    function GetStatusPanelClass: TdxStatusBarPanelClass; virtual;
    procedure Loaded;
    procedure Update(Item: TCollectionItem); override;
    procedure UpdateResources;
    procedure UpdateSubControlsVisibility;
  public
    constructor Create(AStatusBarControl: TdxCustomStatusBar);
    function Add: TdxStatusBarPanel;
    function Insert(Index: Integer): TdxStatusBarPanel;
    property Items[Index: Integer]: TdxStatusBarPanel read GetItem write SetItem; default;
  end;

  { TdxStatusBarSimplePanelStyle }

  TdxStatusBarSimplePanelStyle = class(TPersistent)
  private
    FActive: Boolean;
    FAutoHint: Boolean;
    FStatusBar: TdxCustomStatusBar;
    FText: string;
    procedure SetActive(const AValue: Boolean);
    procedure SetText(const AValue: string);
  protected
    procedure DoAssign(ASource: TPersistent); virtual;

    property StatusBar: TdxCustomStatusBar read FStatusBar;
  public
    constructor Create(AStatusBar: TdxCustomStatusBar); virtual;
    procedure Assign(Source: TPersistent); override;
  published
    property Active: Boolean read FActive write SetActive default False;
    property AutoHint: Boolean read FAutoHint write FAutoHint default False;
    property Text: string read FText write SetText;
  end;

  TdxStatusBarSimplePanelStyleClass = class of TdxStatusBarSimplePanelStyle;

  { Painters }

  TdxStatusBarPaintStyle = (stpsStandard, stpsFlat, stpsXP, stpsOffice11, stpsUseLookAndFeel);

  TdxStatusBarPainter = class
  protected
    class function GetSizeGripBackgroundColor(AStatusBar: TdxCustomStatusBar): TColor; virtual;
    class procedure DoDrawSizeGrip(ACanvas: TcxCanvas; const ARect: TRect); virtual;
    class procedure DrawContainerControl(APanelStyle: TdxStatusBarContainerPanelStyle); virtual;
  public
    // calc
    class procedure AdjustTextColor(AStatusBar: TdxCustomStatusBar; var AColor: TColor; Active: Boolean); virtual;
    class function ContentExtents(AStatusBar: TdxCustomStatusBar): TRect; virtual;
    class function GripAreaSize(AStatusBar: TdxCustomStatusBar): TSize; virtual;
    class function GripSize(AStatusBar: TdxCustomStatusBar): TSize; virtual;
    class function IsNativeBackground: Boolean; virtual;
    class function IsSizeGripInPanelArea(AStatusBar: TdxCustomStatusBar): Boolean; virtual;
    class function PanelBorderSizes(ABevel: TdxStatusBarPanelBevel): TRect; virtual;
    class function PanelContentExtents(AStatusBar: TdxCustomStatusBar; AHasBorders: Boolean): TRect; virtual;
    class function SeparatorSize(AStatusBar: TdxCustomStatusBar): Integer; virtual;
    class function SizeGripMargins(AStatusBar: TdxCustomStatusBar): TRect; virtual;
    class function TopBorderSize: Integer; virtual;
    // draw
    class procedure DrawBorder(AStatusBar: TdxCustomStatusBar; ACanvas: TcxCanvas; var R: TRect); virtual;
    class procedure DrawEmptyPanel(AStatusBar: TdxCustomStatusBar; ACanvas: TcxCanvas;  R: TRect); virtual;
    class procedure DrawPanel(AStatusBar: TdxCustomStatusBar;
      APanel: TdxStatusBarPanel; ACanvas: TcxCanvas; R: TRect); virtual;
    class procedure DrawPanelBorder(AStatusBar: TdxCustomStatusBar; ABevel: TdxStatusBarPanelBevel;
      ACanvas: TcxCanvas; var R: TRect); virtual;
    class procedure DrawPanelSeparator(AStatusBar: TdxCustomStatusBar; ACanvas: TcxCanvas;
      const R: TRect); virtual;
    class procedure DrawSimplePanel(AStatusBar: TdxCustomStatusBar;
      const AText: string; ACanvas: TcxCanvas; R: TRect); virtual;
    class procedure DrawSizeGrip(AStatusBar: TdxCustomStatusBar; ACanvas: TcxCanvas;
      R: TRect); virtual;
    class procedure DrawTopBorder(AStatusBar: TdxCustomStatusBar; ACanvas: TcxCanvas;
      const R: TRect); virtual;
    class procedure FillBackground(AStatusBar: TdxCustomStatusBar; APanel: TdxStatusBarPanel;
      ACanvas: TcxCanvas; const R: TRect); virtual;
    class function GetPanelBevel(APanel: TdxStatusBarPanel): TdxStatusBarPanelBevel; virtual;
    class function GetPanelColor(AStatusBar: TdxCustomStatusBar; APanel: TdxStatusBarPanel): TColor; virtual;
    class function ValidatePanelTextRect(AStatusBar: TdxCustomStatusBar;
      APanel: TdxStatusBarPanel; const R: TRect): TRect; virtual;
  end;

  { TdxStatusBarStandardPainter }

  TdxStatusBarStandardPainter = class(TdxStatusBarPainter)
  public
    class procedure DrawPanelBorder(AStatusBar: TdxCustomStatusBar; ABevel: TdxStatusBarPanelBevel;
      ACanvas: TcxCanvas; var R: TRect); override;
    class procedure DrawPanelSeparator(AStatusBar: TdxCustomStatusBar; ACanvas: TcxCanvas;
      const R: TRect); override;
    class procedure FillBackground(AStatusBar: TdxCustomStatusBar; APanel: TdxStatusBarPanel;
      ACanvas: TcxCanvas; const R: TRect); override;
    class function PanelBorderSizes(ABevel: TdxStatusBarPanelBevel): TRect; override;
  end;

  { TdxStatusBarFlatPainter }

  TdxStatusBarFlatPainter = class(TdxStatusBarStandardPainter)
  public
    class procedure DrawPanelBorder(AStatusBar: TdxCustomStatusBar;
      ABevel: TdxStatusBarPanelBevel; ACanvas: TcxCanvas; var R: TRect); override;
    class function PanelBorderSizes(ABevel: TdxStatusBarPanelBevel): TRect; override;
  end;

  { TdxStatusBarOffice11Painter }

  TdxStatusBarOffice11Painter = class(TdxStatusBarPainter)
  protected
    class procedure DoDrawSizeGrip(ACanvas: TcxCanvas; const ARect: TRect); override;
    class function GetSizeGripBackgroundColor(AStatusBar: TdxCustomStatusBar): TColor; override;
  public
    // calc
    class function ContentExtents(AStatusBar: TdxCustomStatusBar): TRect; override;
    class function SizeGripMargins(AStatusBar: TdxCustomStatusBar): TRect; override;
    // draw
    class procedure DrawBorder(AStatusBar: TdxCustomStatusBar; ACanvas: TcxCanvas;
      var R: TRect); override;
    class procedure DrawPanelBorder(AStatusBar: TdxCustomStatusBar; ABevel: TdxStatusBarPanelBevel;
      ACanvas: TcxCanvas; var R: TRect); override;
    class procedure DrawPanelSeparator(AStatusBar: TdxCustomStatusBar; ACanvas: TcxCanvas;
      const R: TRect); override;
    class function PanelBorderSizes(ABevel: TdxStatusBarPanelBevel): TRect; override;
  end;

  { TdxStatusBarXPPainter }

  TdxStatusBarXPPainter = class(TdxStatusBarPainter)
  protected
    class procedure DoDrawSizeGrip(ACanvas: TcxCanvas; const ARect: TRect); override;
  public
    // calc
    class function ContentExtents(AStatusBar: TdxCustomStatusBar): TRect; override;
    class function GripAreaSize(AStatusBar: TdxCustomStatusBar): TSize; override;
    class function SeparatorSize(AStatusBar: TdxCustomStatusBar): Integer; override;
    class function SizeGripMargins(AStatusBar: TdxCustomStatusBar): TRect; override;
    // draw
    class procedure DrawBorder(AStatusBar: TdxCustomStatusBar; ACanvas: TcxCanvas; var R: TRect); override;
    class procedure DrawPanelSeparator(AStatusBar: TdxCustomStatusBar; ACanvas: TcxCanvas; const R: TRect); override;
  end;

  { TdxStatusBarCustomPanelViewInfo }

  TdxStatusBarCustomPanelViewInfo = class(TObject)
  private
    FBounds: TRect;
    FIsRightToLeftConverted: Boolean;
    FStatusBar: TdxCustomStatusBar;
    function GetPainter: TdxStatusBarPainterClass;
  protected
    procedure DoRightToLeftConversion(const ABounds: TRect); virtual;
    function GetPanelBevel: TdxStatusBarPanelBevel; virtual; abstract;
    function GetPanelFont: TFont; virtual;
    procedure SetBounds(const AValue: TRect); virtual;
  public
    constructor Create(AStatusBar: TdxCustomStatusBar); virtual;
    function CalculateAutoHeight: Integer; virtual;
    procedure Draw(ACanvas: TcxCanvas); virtual; abstract;
    procedure RightToLeftConversion(const ABounds: TRect);

    property Bounds: TRect read FBounds write SetBounds;
    property Painter: TdxStatusBarPainterClass read GetPainter;
    property PanelBevel: TdxStatusBarPanelBevel read GetPanelBevel;
    property PanelFont: TFont read GetPanelFont;
    property StatusBar: TdxCustomStatusBar read FStatusBar;
  end;

  { TdxStatusBarPanelViewInfo }

  TdxStatusBarPanelViewInfo = class(TdxStatusBarCustomPanelViewInfo)
  private
    FPanel: TdxStatusBarPanel;
    FSeparatorRect: TRect;
    function GetHasSeparator: Boolean;
  protected
    procedure DoRightToLeftConversion(const ABounds: TRect); override;
    function GetPanelBevel: TdxStatusBarPanelBevel; override;
    function GetPanelFont: TFont; override;
    procedure SetBounds(const AValue: TRect); override;
  public
    constructor Create(APanel: TdxStatusBarPanel); reintroduce;
    procedure Draw(ACanvas: TcxCanvas); override;
    //
    property Panel: TdxStatusBarPanel read FPanel;
    property HasSeparator: Boolean read GetHasSeparator;
    property SeparatorRect: TRect read FSeparatorRect write FSeparatorRect;
  end;

  { TdxStatusBarSimplePanelViewInfo }

  TdxStatusBarSimplePanelViewInfo = class(TdxStatusBarCustomPanelViewInfo)
  protected
    function GetPanelBevel: TdxStatusBarPanelBevel; override;
  public
    procedure Draw(ACanvas: TcxCanvas); override;
  end;

  { TdxStatusBarViewInfo }

  TdxStatusBarViewInfo = class(TObject)
  private
    FIsRightToLeftConverted: Boolean;
    FPanelsViewInfos: TcxObjectList;
    FSimplePanelViewInfo: TdxStatusBarSimplePanelViewInfo;
    FSizeGripRect: TRect;
    FStatusBar: TdxCustomStatusBar;
    function GetPainter: TdxStatusBarPainterClass;
    function GetPanel(Index: Integer): TdxStatusBarPanel;
    function GetPanelCount: Integer;
    function GetPanelViewInfo(Index: Integer): TdxStatusBarPanelViewInfo;
    function GetSizeGripAreaRect: TRect;
  protected
    procedure AddCalculatedItems(AAutoWidthObject: TcxAutoWidthObject);
    procedure CalculatePanelsViewInfos(const ABounds: TRect; AAutoWidthObject: TcxAutoWidthObject);
    function CalculateSizeGripRect(const ABounds: TRect): TRect;
    function CreatePanelViewInfo(APanel: TdxStatusBarPanel): TdxStatusBarPanelViewInfo; virtual;
    function CreateSimplePanelViewInfo: TdxStatusBarSimplePanelViewInfo; virtual;
    procedure DoRightToLeftConversion(const ABounds: TRect); virtual;
    procedure DrawPanels(ACanvas: TcxCanvas; const R: TRect);
    procedure DrawSizeGrip(ACanvas: TcxCanvas);
    function GetCalculatedItemCount: Integer;
    function GetPanelAt(const P: TPoint): TdxStatusBarPanel;
    function IsSizeGripInPanelArea: Boolean; virtual;
    procedure RecreateChildrenViewInfo;

    property Painter: TdxStatusBarPainterClass read GetPainter;
  public
    constructor Create(AOwner: TdxCustomStatusBar); virtual;
    destructor Destroy; override;
    procedure Calculate(const ABounds: TRect); virtual;
    function CalculateAutoHeight: Integer; virtual;
    procedure Draw(ACanvas: TcxCanvas); virtual;
    procedure RightToLeftConversion(const ABounds: TRect);

    property PanelCount: Integer read GetPanelCount;
    property Panels[Index: Integer]: TdxStatusBarPanel read GetPanel;
    property PanelViewInfo[Index: Integer]: TdxStatusBarPanelViewInfo read GetPanelViewInfo;
    property SimplePanelViewInfo: TdxStatusBarSimplePanelViewInfo read FSimplePanelViewInfo;
    property SizeGripAreaRect: TRect read GetSizeGripAreaRect;
    property SizeGripRect: TRect read FSizeGripRect;
    property StatusBar: TdxCustomStatusBar read FStatusBar;
  end;

  { TdxCustomStatusBar }

  TdxStatusBarState = (sbsCalculating);
  TdxStatusBarStates = set of TdxStatusBarState;

  TdxStatusBarPanelCreateClassEvent = procedure(Sender: TdxCustomStatusBar;
    var AStatusPanelClass: TdxStatusBarPanelClass) of object;

  TdxCustomStatusBar = class(TcxControl, IdxSkinSupport)
  strict private
    FBorderWidth: Integer;
    FImageChangeLink: TChangeLink;
    FImages: TCustomImageList;
    FIsRightToLeftConverted: Boolean;
    FPainter: TdxStatusBarPainterClass;
    FPaintStyle: TdxStatusBarPaintStyle;
    FPanels: TdxStatusBarPanels;
    FSimplePanelStyle: TdxStatusBarSimplePanelStyle;
    FSizeGrip: Boolean;
    FThemeAvailable: Boolean;
    FThemeChangedNotificator: TdxThemeChangedNotificator;
    FViewInfo: TdxStatusBarViewInfo;

    FOnHint: TNotifyEvent;

    procedure ImageListChange(Sender: TObject);
    procedure SetBorderWidth(Value: Integer);
    procedure SetImages(const Value: TCustomImageList);
    procedure SetPanels(Value: TdxStatusBarPanels);
    procedure SetPaintStyle(Value: TdxStatusBarPaintStyle);
    procedure SetSimplePanelStyle(Value: TdxStatusBarSimplePanelStyle);
    procedure SetSizeGrip(Value: Boolean);
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure CMParentFontChanged(var Message: TMessage); message CM_PARENTFONTCHANGED;
    procedure CMSysColorChange(var Message: TMessage); message CM_SYSCOLORCHANGE;
    procedure CMWinIniChange(var Message: TMessage); message CM_WININICHANGE;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
  protected
    FState: TdxStatusBarStates;

    procedure AdjustTextColor(var AColor: TColor; Active: Boolean); virtual;
    procedure BiDiModeChanged; override;
    function CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;
    procedure ChangeScaleEx(M, D: Integer; isDpiChange: Boolean); override;
    procedure CheckAutoSize;
    function CreateViewInfo: TdxStatusBarViewInfo; virtual;
    procedure FontChanged; override;
    function HasBackground: Boolean; override;
    procedure Loaded; override;
    procedure LookAndFeelChanged(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues); override;
    function MayFocus: Boolean; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    function NeedsScrollBars: Boolean; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Paint; override;
    procedure Resize; override;

    procedure Calculate; virtual;
    function CalculateAutoHeight: Integer; virtual;
    function ContainerByName(const AName: string): TdxStatusBarContainerControl;
    function CreatePanel: TdxStatusBarPanel; virtual;
    function CreatePanels: TdxStatusBarPanels; virtual;
    function DoHint: Boolean; virtual;
    procedure DoRightToLeftConversion(const ABounds: TRect); virtual;
    class function GetDefaultPanelStyleClass: TdxStatusBarPanelStyleClass; virtual;
    function GetPainterClass: TdxStatusBarPainterClass; virtual;
    function GetPaintStyle: TdxStatusBarPaintStyle; virtual;
    function GetPanelsBounds: TRect;
    function GetPanelsOffsets: TRect; virtual;
    class function GetStatusPanelClass: TdxStatusBarPanelClass; virtual;
    class function GetStatusPanelsClass: TdxStatusBarPanelsClass; virtual;
    procedure InitPainterClass; virtual;
    procedure PaintStyleChanged; virtual;
    procedure Recalculate;
    procedure RightToLeftConversion;
    function SizeGripAllocated: Boolean; virtual;
    procedure ThemeChanged; virtual;
    procedure UpdatePanels; virtual;

    property BorderWidth: Integer read FBorderWidth write SetBorderWidth default 0;
    property Painter: TdxStatusBarPainterClass read FPainter;
    property PaintStyle: TdxStatusBarPaintStyle read FPaintStyle write SetPaintStyle default stpsStandard;
    property Panels: TdxStatusBarPanels read FPanels write SetPanels;
    property SimplePanelStyle: TdxStatusBarSimplePanelStyle read FSimplePanelStyle write SetSimplePanelStyle;
    property SizeGrip: Boolean read FSizeGrip write SetSizeGrip default True;
    property ViewInfo: TdxStatusBarViewInfo read FViewInfo;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function CanAcceptPanelStyle(Value: TdxStatusBarPanelStyleClass): Boolean; virtual;
    function GetPanelAt(X, Y: Integer): TdxStatusBarPanel; virtual;
    property OnHint: TNotifyEvent read FOnHint write FOnHint;
  published
    property AutoSize;
    property Images: TCustomImageList read FImages write SetImages;
  end;

  { TdxStatusBar }

  TdxStatusBar = class(TdxCustomStatusBar)
  published
    property Images;
    property Panels;
    property PaintStyle;
    property SimplePanelStyle;
    property SizeGrip;
    property LookAndFeel;
    property OnHint;
    property BorderWidth;
    { TcxControl properties}
    property Anchors;
    property BiDiMode;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property Color default clBtnFace;
    property Constraints;
    property ShowHint;
    property ParentBiDiMode;
    property ParentFont default False;
    property ParentShowHint;
    property PopupMenu;
    property Visible;
    property OnContextPopup;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
  end;

var
  dxStatusBarSkinPainterClass: TdxStatusBarPainterClass = nil;

function GetRegisteredStatusBarPanelStyles: TcxRegisteredClasses;

procedure LoadIndicatorBitmap(ABitmap: TBitmap; AIndicatorType: TdxStatusBarStateIndicatorType); overload;
procedure LoadIndicatorBitmap(ABitmap: TBitmap; AIndicatorType: TdxStatusBarStateIndicatorType; AScaleFactor: TdxScaleFactor); overload;
implementation

{$R dxStatusBar.res}

uses
  ActnList, StdActns, Types, Math, dxThemeConsts, dxUxTheme, dxOffice11, dxDPIAwareUtils, dxSkinsdxStatusBarPainter;

resourcestring
  cxSTextPanelStyle = 'Text Panel';
  cxSContainerPanelStyle = 'Container Panel';
  cxSKeyboardStatePanelStyle = 'Keyboard State Panel';
  cxSStateIndicatorPanelStyle = 'State Indicator Panel';
  // design-time
  cxSCantDeleteAncestor = 'Selection contains a component introduced in an ancestor form which cannot be deleted';

var
  FRegisteredStatusBarPanelStyles: TcxRegisteredClasses;

function GetRegisteredStatusBarPanelStyles: TcxRegisteredClasses;
begin
  if FRegisteredStatusBarPanelStyles = nil then
    FRegisteredStatusBarPanelStyles := TcxRegisteredClasses.Create;
  Result := FRegisteredStatusBarPanelStyles;
end;

procedure LoadIndicatorBitmap(ABitmap: TBitmap; AIndicatorType: TdxStatusBarStateIndicatorType);
begin
  LoadIndicatorBitmap(ABitmap, AIndicatorType, dxDefaultScaleFactor);
end;

procedure LoadIndicatorBitmap(ABitmap: TBitmap; AIndicatorType: TdxStatusBarStateIndicatorType; AScaleFactor: TdxScaleFactor);
const
  Names: array[TdxStatusBarStateIndicatorType] of string = (
    'DXSTATUSBAR_GRAY', 'DXSTATUSBAR_YELLOW', 'DXSTATUSBAR_BLUE', 'DXSTATUSBAR_GREEN',
    'DXSTATUSBAR_RED', 'DXSTATUSBAR_TEAL', 'DXSTATUSBAR_PURPLE'
  );
var
  ATempBitmap: TBitmap;
begin
  if AScaleFactor.Assigned then
  begin
    ATempBitmap := TBitmap.Create;
    try
      ATempBitmap.LoadFromResourceName(HInstance, Names[AIndicatorType]);
      ABitmap.Height := AScaleFactor.Apply(ATempBitmap.Height);
      ABitmap.Width := AScaleFactor.Apply(ATempBitmap.Width);
      ABitmap.PixelFormat := pf32bit;
      cxSmoothResizeBitmap(ATempBitmap, ABitmap, True);
    finally
      ATempBitmap.Free;
    end;
  end
  else
    ABitmap.LoadFromResourceName(HInstance, Names[AIndicatorType]);
end;

procedure GenContainerName(APanel: TdxStatusBarPanel; AContainer: TdxStatusBarContainerControl);
var
  I: Integer;
begin
  I := APanel.ID;
  while I <> -1 do
  try
    AContainer.Name := APanel.StatusBarControl.Name + 'Container' + IntToStr(I);
    I := -1;
  except
    on EComponentError do //Ignore rename errors
      Inc(I);
  end;
end;

{ TdxStatusBarPanelStyle }

constructor TdxStatusBarPanelStyle.Create(AOwner: TdxStatusBarPanel);
begin
  inherited Create;
  FOwner := AOwner;
  FFont := TFont.Create;
  FFont.OnChange := FontChanged;
  ParentFont := True;
end;

destructor TdxStatusBarPanelStyle.Destroy;
begin
  FOwner := nil;
  FreeAndNil(FFont);
  inherited;
end;

procedure TdxStatusBarPanelStyle.Assign(Source: TPersistent);
begin
  if Source is TdxStatusBarPanelStyle then
  begin
    BeginUpdate;
    try
      DoAssign(Source);
    finally
      EndUpdate;
    end;
  end
  else
    inherited Assign(Source);
end;

procedure TdxStatusBarPanelStyle.BeginUpdate;
begin
  if Assigned(Owner) and Assigned(Owner.Collection) then
    Owner.Collection.BeginUpdate;
end;

procedure TdxStatusBarPanelStyle.EndUpdate;
begin
  if Assigned(Owner) and Assigned(Owner.Collection) then
    Owner.Collection.EndUpdate;
end;

procedure TdxStatusBarPanelStyle.RestoreDefaults;
begin
  FAlignment := taLeftJustify;
  FIsColorAssigned := False;
  ParentFont := True;
  Owner.Changed(False);
end;

procedure TdxStatusBarPanelStyle.AdjustTextColor(var AColor: TColor;
  Active: Boolean);
begin
  if AColor = clDefault then
    StatusBarControl.AdjustTextColor(AColor, Active);
end;

procedure TdxStatusBarPanelStyle.BoundsChanged(const ABounds: TRect);
begin
end;

function TdxStatusBarPanelStyle.CanDelete: Boolean;
begin
  Result := True;
end;

function TdxStatusBarPanelStyle.CanSizing: Boolean;
begin
  Result := True;
end;

procedure TdxStatusBarPanelStyle.Changed;
begin
  if Assigned(FOwner) then
    FOwner.StatusBarPanelStyleChanged;
end;

procedure TdxStatusBarPanelStyle.ChangeScale(M: Integer; D: Integer);
begin
  if not ParentFont then
    Font.Height := MulDiv(Font.Height, M, D);
end;

procedure TdxStatusBarPanelStyle.CheckSizeGripRect(var R: TRect);
begin
  if StatusBarControl.SizeGripAllocated then
  begin
    if (StatusBarControl.ViewInfo.PanelCount > 0) and
       (StatusBarControl.ViewInfo.Panels[StatusBarControl.ViewInfo.PanelCount - 1] = Owner)
    then
      Dec(R.Right, Painter.GripAreaSize(StatusBarControl).cx);
  end;
end;

function TdxStatusBarPanelStyle.DefaultColor: TColor;
begin
  Result := StatusBarControl.Color;
end;

procedure TdxStatusBarPanelStyle.DoAssign(ASource: TPersistent);
var
  AStyle: TdxStatusBarPanelStyle;
begin
  AStyle := TdxStatusBarPanelStyle(ASource);
  RestoreDefaults;
  Alignment := AStyle.Alignment;
  if AStyle.IsColorStored then
    Color := AStyle.Color;
  if AStyle.IsFontStored then
    Font.Assign(AStyle.Font)
  else
    ParentFontChanged;
end;

procedure TdxStatusBarPanelStyle.DoRightToLeftConversion(const ABounds: TRect);
begin
end;

procedure TdxStatusBarPanelStyle.DrawContent(ACanvas: TcxCanvas; R: TRect;
  APainter: TdxStatusBarPainterClass);
begin
  APainter.FillBackground(Self.StatusBarControl, Owner, ACanvas, R);
end;

function TdxStatusBarPanelStyle.GetMinWidth: Integer;
begin
  Result := ScaleFactor.Apply(20);
end;

function TdxStatusBarPanelStyle.GetPanelFixed: Boolean;
begin
  Result := Owner.FFixed;
end;

class function TdxStatusBarPanelStyle.GetVersion: Integer;
begin
  Result := 0;
end;

function TdxStatusBarPanelStyle.InternalBevel: Boolean;
begin
  Result := False;
end;

procedure TdxStatusBarPanelStyle.Loaded;
begin
end;

procedure TdxStatusBarPanelStyle.ParentFontChanged;
begin
  if ParentFont then
  begin
    Font.Assign(StatusBarControl.Font);
    FParentFont := True;
  end;
end;

procedure TdxStatusBarPanelStyle.UpdateResources;
begin
  // do nothing
end;

procedure TdxStatusBarPanelStyle.UpdateSubControlsVisibility;
begin
  // do nothing
end;

procedure TdxStatusBarPanelStyle.FontChanged(Sender: TObject);
begin
  FParentFont := False;
  Owner.Changed(False);
end;

function TdxStatusBarPanelStyle.GetColor: TColor;
begin
  if FIsColorAssigned then
    Result := FColor
  else
    Result := DefaultColor;
end;

function TdxStatusBarPanelStyle.GetPainter: TdxStatusBarPainterClass;
begin
  Result := StatusBarControl.Painter;
end;

function TdxStatusBarPanelStyle.GetScaleFactor: TdxScaleFactor;
begin
  Result := StatusBarControl.ScaleFactor;
end;

function TdxStatusBarPanelStyle.GetStatusBarControl: TdxCustomStatusBar;
begin
  Result := TdxStatusBarPanels(Owner.Collection).FStatusBarControl;
end;

function TdxStatusBarPanelStyle.IsColorStored: Boolean;
begin
  Result := FIsColorAssigned;
end;

function TdxStatusBarPanelStyle.IsFontStored: Boolean;
begin
  Result := not ParentFont;
end;

procedure TdxStatusBarPanelStyle.RightToLeftConversion(const ABounds: TRect);
begin
  if not FIsRightToLeftConverted then
  begin
    DoRightToLeftConversion(ABounds);
    FIsRightToLeftConverted := True;
  end;
end;

procedure TdxStatusBarPanelStyle.SetAlignment(Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    Owner.Changed(False);
  end;
end;

procedure TdxStatusBarPanelStyle.SetColor(Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    FIsColorAssigned := True;
    Owner.Changed(False);
  end;
end;

procedure TdxStatusBarPanelStyle.SetFont(Value: TFont);
begin
  FFont.Assign(Value);
end;

procedure TdxStatusBarPanelStyle.SetParentFont(Value: Boolean);
begin
  if FParentFont <> Value then
  begin
    FParentFont := Value;
    ParentFontChanged;
  end;
end;

{ TdxStatusBarTextPanelStyle }

constructor TdxStatusBarTextPanelStyle.Create(AOwner: TdxStatusBarPanel);
begin
  inherited Create(AOwner);
  FImageIndex := -1;
end;

procedure TdxStatusBarTextPanelStyle.RestoreDefaults;
begin
  FAutoHint := False;
  FImageIndex := -1;
  FEllipsisType := dxetNone;
  inherited RestoreDefaults;
end;

procedure TdxStatusBarTextPanelStyle.DoAssign(ASource: TPersistent);
var
  AStyle: TdxStatusBarTextPanelStyle;
begin
  inherited;
  if ASource is TdxStatusBarTextPanelStyle then
  begin
    AStyle := TdxStatusBarTextPanelStyle(ASource);
    AutoHint := AStyle.AutoHint;
    ImageIndex := AStyle.ImageIndex;
    EllipsisType := AStyle.EllipsisType;
  end;
end;

procedure TdxStatusBarTextPanelStyle.DrawContent(ACanvas: TcxCanvas;
  R: TRect; APainter: TdxStatusBarPainterClass);
const
  AShowEndEllipsis: array[TdxStatusBarEllipsisType] of Integer = (0, cxShowEndEllipsis,
    cxShowPathEllipsis);
var
  ALeft, ATop, ARealLeft: Integer;
  ATextColor: TColor;
  S: string;
  AFlags: Cardinal;
  AAlignmentFlag: Cardinal;
begin
  inherited;
  if Assigned(StatusBarControl.Images) and
    (0 <= ImageIndex) and (ImageIndex < StatusBarControl.Images.Count) then
  begin
    ALeft := R.Left + 1;
    ATop := ((R.Top + R.Bottom) div 2) - (StatusBarControl.Images.Height div 2);
    R.Left := ALeft + StatusBarControl.Images.Width + 1;
    if StatusBarControl.UseRightToLeftAlignment then
      ARealLeft := R.Right - (ALeft - R.Left) - StatusBarControl.Images.Width
    else
      ARealLeft := ALeft;
    StatusBarControl.Images.Draw(ACanvas.Canvas, ARealLeft, ATop,
      ImageIndex, StatusBarControl.Enabled);
  end;
  ACanvas.Brush.Style := bsClear;
  ATextColor := Font.Color;
  APainter.AdjustTextColor(Owner.GetStatusBarControl, ATextColor, True);
  AdjustTextColor(ATextColor, True);
  ACanvas.Font.Assign(Font);
  ACanvas.Font.Color := ATextColor;
  InflateRect(R, -2, 0);
  S := Owner.Text;
  R := APainter.ValidatePanelTextRect(StatusBarControl, Owner, R);
  if Assigned(FOnGetText) then
    FOnGetText(Self, R, S);
  AFlags := cxSingleLine or cxAlignVCenter or AShowEndEllipsis[EllipsisType];
  if StatusBarControl.UseRightToLeftAlignment then
    AAlignmentFlag := TdxRightToLeftLayoutConverter.ConvertcxDrawTextAlignment(cxAlignmentsHorz[Alignment])
  else
    AAlignmentFlag := cxAlignmentsHorz[Alignment];
  ACanvas.DrawText(S, R, AFlags or AAlignmentFlag, StatusBarControl.Enabled);
  ACanvas.Brush.Style := bsSolid;
end;

procedure TdxStatusBarTextPanelStyle.SetAutoHint(Value: Boolean);
var
  I: Integer;
begin
  if FAutoHint <> Value then
  begin
    for I := 0 to StatusBarControl.Panels.Count - 1 do
      if StatusBarControl.Panels[I].PanelStyle is TdxStatusBarTextPanelStyle then
        TdxStatusBarTextPanelStyle(StatusBarControl.Panels[I].PanelStyle).FAutoHint := False;
    FAutoHint := Value;
  end;
end;

procedure TdxStatusBarTextPanelStyle.SetEllipsisType(Value: TdxStatusBarEllipsisType);
begin
  if FEllipsisType <> Value then
  begin
    FEllipsisType := Value;
    Owner.Changed(False);
  end;
end;

procedure TdxStatusBarTextPanelStyle.SetImageIndex(Value: TcxImageIndex);
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
    Owner.Changed(False);
  end;
end;

{ TdxStatusBarStateIndicatorItem }

constructor TdxStatusBarStateIndicatorItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FIndicatorBitmap := TBitmap.Create;
  FVisible := True;
  UpdateResources;
end;

destructor TdxStatusBarStateIndicatorItem.Destroy;
begin
  FreeAndNil(FIndicatorBitmap);
  inherited Destroy;
end;

procedure TdxStatusBarStateIndicatorItem.Assign(Source: TPersistent);
begin
  if Source is TdxStatusBarStateIndicatorItem then
  begin
    IndicatorType := TdxStatusBarStateIndicatorItem(Source).IndicatorType;
    Visible := TdxStatusBarStateIndicatorItem(Source).Visible;
  end
  else
    inherited Assign(Source);
end;

function TdxStatusBarStateIndicatorItem.CanDisplay: Boolean;
begin
  Result := Visible and (FIndicatorBitmap <> nil) and not FIndicatorBitmap.Empty;
end;

procedure TdxStatusBarStateIndicatorItem.UpdateResources;
begin
  LoadIndicatorBitmap(FIndicatorBitmap, FIndicatorType, dxGetScaleFactor(TdxStatusBarStateIndicators(Collection).Owner));
  Changed(False);
end;

procedure TdxStatusBarStateIndicatorItem.SetIndicatorType(Value: TdxStatusBarStateIndicatorType);
begin
  if FIndicatorType <> Value then
  begin
    FIndicatorType := Value;
    UpdateResources;
  end;
end;

procedure TdxStatusBarStateIndicatorItem.SetVisible(Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    Changed(False);
  end;
end;

{ TdxStatusBarStateIndicators }

constructor TdxStatusBarStateIndicators.Create(AOwner: TPersistent = nil);
begin
  inherited Create(AOwner, TdxStatusBarStateIndicatorItem);
end;

function TdxStatusBarStateIndicators.Add: TdxStatusBarStateIndicatorItem;
begin
  Result := TdxStatusBarStateIndicatorItem.Create(Self);
end;

function TdxStatusBarStateIndicators.Insert(AIndex: Integer): TdxStatusBarStateIndicatorItem;
begin
  BeginUpdate;
  try
    AIndex := Min(Max(AIndex, 0), Count);
    Result := Add;
    Result.Index := AIndex;
  finally
    EndUpdate;
  end;
end;

procedure TdxStatusBarStateIndicators.Update(Item: TCollectionItem);
begin
  dxCallNotify(OnChange, Self);
end;

procedure TdxStatusBarStateIndicators.UpdateResources;
var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := 0 to Count - 1 do
      Items[I].UpdateResources;
  finally
    EndUpdate;
  end;
end;

function TdxStatusBarStateIndicators.GetItem(Index: Integer): TdxStatusBarStateIndicatorItem;
begin
  Result := TdxStatusBarStateIndicatorItem(inherited GetItem(Index));
end;

procedure TdxStatusBarStateIndicators.SetItem(Index: Integer; Value: TdxStatusBarStateIndicatorItem);
begin
  inherited SetItem(Index, Value);
end;

{ TdxStatusBarStateIndicatorPanelStyle }

constructor TdxStatusBarStateIndicatorPanelStyle.Create(AOwner: TdxStatusBarPanel);
begin
  inherited Create(AOwner);
  FSpacing := 4;
  FIndicators := TdxStatusBarStateIndicators.Create(Self);
  FIndicators.OnChange := IndicatorChangeHandler;
end;

destructor TdxStatusBarStateIndicatorPanelStyle.Destroy;
begin
  FreeAndNil(FIndicators);
  inherited;
end;

procedure TdxStatusBarStateIndicatorPanelStyle.RestoreDefaults;
begin
  FSpacing := 4;
  inherited RestoreDefaults;
end;

function TdxStatusBarStateIndicatorPanelStyle.CanSizing: Boolean;
begin
  Result := False;
end;

procedure TdxStatusBarStateIndicatorPanelStyle.DoAssign(ASource: TPersistent);
begin
  inherited DoAssign(ASource);

  if ASource is TdxStatusBarStateIndicatorPanelStyle then
  begin
    Spacing := TdxStatusBarStateIndicatorPanelStyle(ASource).Spacing;
    Indicators := TdxStatusBarStateIndicatorPanelStyle(ASource).Indicators;
  end;
end;

procedure TdxStatusBarStateIndicatorPanelStyle.DrawContent(
  ACanvas: TcxCanvas; R: TRect; APainter: TdxStatusBarPainterClass);
var
  ABitmap: TBitmap;
  I: Integer;
  ABounds: TRect;
  AItemLeft: Integer;
begin
  inherited DrawContent(ACanvas, R, APainter);
  ABounds := R;
  Inc(R.Left, ScaleFactor.Apply(Spacing));
  for I := 0 to Indicators.Count - 1 do
    if Indicators[I].CanDisplay then
    begin
      ABitmap := Indicators[I].IndicatorBitmap;
      if StatusBarControl.UseRightToLeftAlignment then
        AItemLeft := ABounds.Right - (R.Left - ABounds.Left) - ABitmap.Width
      else
        AItemLeft := R.Left;
      ACanvas.Draw(AItemLeft, (R.Top + R.Bottom - ABitmap.Height) div 2, ABitmap);
      Inc(R.Left, ABitmap.Width + ScaleFactor.Apply(Spacing));
    end;
end;

function TdxStatusBarStateIndicatorPanelStyle.GetMinWidth: Integer;
var
  I: Integer;
begin
  // WARNING: sync with DrawContent
  if Indicators.Count > 0 then
  begin
    Result := Spacing;
    for I := 0 to Indicators.Count - 1 do
    begin
      if Indicators[I].CanDisplay then
        Inc(Result, Indicators[I].IndicatorBitmap.Width + ScaleFactor.Apply(Spacing));
    end;
    Inc(Result, cxMarginsWidth(Painter.PanelContentExtents(StatusBarControl, Painter.GetPanelBevel(Owner) <> dxpbNone)));
  end
  else
    Result := inherited GetMinWidth;
end;

procedure TdxStatusBarStateIndicatorPanelStyle.UpdateResources;
begin
  inherited;
  FIndicators.UpdateResources;
end;

procedure TdxStatusBarStateIndicatorPanelStyle.IndicatorChangeHandler(Sender: TObject);
begin
  Owner.Changed(False);
end;

procedure TdxStatusBarStateIndicatorPanelStyle.SetIndicators(Value: TdxStatusBarStateIndicators);
begin
  FIndicators.Assign(Value);
end;

procedure TdxStatusBarStateIndicatorPanelStyle.SetSpacing(Value: Integer);
begin
  if FSpacing <> Value then
  begin
    FSpacing := Value;
    Owner.Changed(False);
  end;
end;

{ TdxStatusBarKeyboardStateWatchedKey }

constructor TdxStatusBarKeyboardStateWatchedKey.Create(AKeyCode: Integer);
begin
  FKeyCode := AKeyCode;
  FKeyState := Lo(GetKeyState(AKeyCode));
end;

function TdxStatusBarKeyboardStateWatchedKey.GetCurrentState: Integer;
begin
  Result := Lo(GetKeyState(FKeyCode));
end;

procedure TdxStatusBarKeyboardStateWatchedKey.SetKeyState(Value: Integer);
begin
  FKeyState := Value;
end;

{ TdxStatusBarKeyboardStateNotifier }

constructor TdxStatusBarKeyboardStateNotifier.Create(AStatusBar: TdxCustomStatusBar);
begin
  inherited Create;
  FStatusBar := AStatusBar;
  FTimer := TTimer.Create(nil);
  FTimer.Interval := 100;
  FTimer.OnTimer := Execute;
end;

destructor TdxStatusBarKeyboardStateNotifier.Destroy;
begin
  FTimer.Free;
  inherited Destroy;
end;

procedure TdxStatusBarKeyboardStateNotifier.SubScribeKey(AKeyCode: Integer);
begin
  UnSubscribeKey(AKeyCode);

  SetLength(FKeys, Length(FKeys) + 1);
  FKeys[High(FKeys)] := TdxStatusBarKeyboardStateWatchedKey.Create(AKeyCode);
end;

procedure TdxStatusBarKeyboardStateNotifier.UnSubScribeKey(AKeyCode: Integer);
var
  I: Integer;
begin
  for I := 0 to High(FKeys) do
    if FKeys[I].KeyCode = AKeyCode then
    begin
      FreeAndNil(FKeys[I]);
      SetLength(FKeys, Length(FKeys) - 1);
      Break;
    end;
end;

procedure TdxStatusBarKeyboardStateNotifier.Execute(Sender: TObject);
var
  I, ACurState: Integer;
  AChanged: Boolean;
begin
  AChanged := False;
  for I := Low(FKeys) to High(FKeys) do
  begin
    ACurState := Lo(GetKeyState(FKeys[I].KeyCode));
    if ACurState <> FKeys[I].KeyState then
    begin
      FKeys[I].KeyState := ACurState;
      AChanged := True;
    end;
  end;
  if AChanged then
    FStatusBar.UpdatePanels;
end;

{ TdxStatusBarKeyStateAppearance }

constructor TdxStatusBarKeyStateAppearance.Create(AId: TdxStatusBarKeyboardState;
  ACode: Integer; const AActiveCaption: string; AActiveFontColor: TColor;
  const AInactiveCaption: string; AInactiveFontColor: TColor; AChangeHandler: TNotifyEvent);
begin
  inherited Create;
  FId := AId;
  FCode := ACode;
  FOnChange := AChangeHandler;
  FActiveFontColor := AActiveFontColor;
  FInactiveFontColor := AInactiveFontColor;
  FActiveCaption := AActiveCaption;
  FInactiveCaption := AInactiveCaption;
end;

procedure TdxStatusBarKeyStateAppearance.Assign(Source: TPersistent);
begin
  if Source is TdxStatusBarKeyStateAppearance then
  begin
    ActiveFontColor := TdxStatusBarKeyStateAppearance(Source).ActiveFontColor;
    InactiveFontColor := TdxStatusBarKeyStateAppearance(Source).InactiveFontColor;
    ActiveCaption := TdxStatusBarKeyStateAppearance(Source).ActiveCaption;
    InactiveCaption := TdxStatusBarKeyStateAppearance(Source).InactiveCaption;
  end
  else
    inherited Assign(Source);
end;

function TdxStatusBarKeyStateAppearance.GetRectWidth(ACanvas: TcxCanvas): Integer;
begin
  Result := GetRectWidth(ACanvas, dxSystemScaleFactor);
end;

function TdxStatusBarKeyStateAppearance.GetRectWidth(ACanvas: TcxCanvas; AScaleFactor: TdxScaleFactor): Integer;
begin
  Result := Max(ACanvas.TextWidth(FActiveCaption), ACanvas.TextWidth(FInactiveCaption)) + 2 * AScaleFactor.Apply(cxTextOffset);
end;

procedure TdxStatusBarKeyStateAppearance.Changed;
begin
  CallNotify(OnChange, Self);
end;

procedure TdxStatusBarKeyStateAppearance.SetActiveFontColor(Value: TColor);
begin
  FActiveFontColor := Value;
  Changed;
end;

procedure TdxStatusBarKeyStateAppearance.SetInactiveFontColor(Value: TColor);
begin
  FInactiveFontColor := Value;
  Changed;
end;

procedure TdxStatusBarKeyStateAppearance.SetActiveCaption(const Value: string);
begin
  FActiveCaption := Value;
  Changed;
end;

procedure TdxStatusBarKeyStateAppearance.SetInactiveCaption(const Value: string);
begin
  FInactiveCaption := Value;
  Changed;
end;

{ TdxStatusBarKeyboardStatePanelStyle }

constructor TdxStatusBarKeyboardStatePanelStyle.Create(AOwner: TdxStatusBarPanel);
var
  I: Integer;
begin
  inherited Create(AOwner);
  FKeyInfos[0] := TdxStatusBarKeyStateAppearance.Create(
    dxksCapsLock, VK_CAPITAL, 'CAPS', FFont.Color, 'CAPS', clBtnShadow, NamesChangeHandler);
  FKeyInfos[1] := TdxStatusBarKeyStateAppearance.Create(
    dxksNumLock, VK_NUMLOCK, 'NUM', FFont.Color, 'NUM', clBtnShadow, NamesChangeHandler);
  FKeyInfos[2] := TdxStatusBarKeyStateAppearance.Create(
    dxksScrollLock, VK_SCROLL, 'SCRL', FFont.Color, 'SCRL', clBtnShadow, NamesChangeHandler);
  FKeyInfos[3] := TdxStatusBarKeyStateAppearance.Create(
    dxksInsert, VK_INSERT, 'OVR', FFont.Color, 'INS', clBtnShadow, NamesChangeHandler);
  FKeyboardStates := [FKeyInfos[0].Id, FKeyInfos[1].Id, FKeyInfos[2].Id, FKeyInfos[3].Id];
  FNotifier := TdxStatusBarKeyboardStateNotifier.Create(StatusBarControl);
  for I := 0 to High(FKeyInfos) do
    FNotifier.SubscribeKey(FKeyInfos[I].Code);
end;

destructor TdxStatusBarKeyboardStatePanelStyle.Destroy;
var
  I: Integer;
begin
  for I := High(FKeyInfos) downto 0 do
    if Assigned(FKeyInfos[I]) then
      FNotifier.UnSubscribeKey(FKeyInfos[I].Code);
  FNotifier.Free;
  for I := High(FKeyInfos) downto 0 do
    FreeAndNil(FKeyInfos[I]);
  inherited;
end;

procedure TdxStatusBarKeyboardStatePanelStyle.RestoreDefaults;
begin
  FKeyboardStates := [FKeyInfos[0].Id, FKeyInfos[1].Id, FKeyInfos[2].Id, FKeyInfos[3].Id];
  inherited RestoreDefaults;
end;

function TdxStatusBarKeyboardStatePanelStyle.CanSizing: Boolean;
begin
  Result := False;
end;

procedure TdxStatusBarKeyboardStatePanelStyle.DoAssign(ASource: TPersistent);
var
  AStyle: TdxStatusBarKeyboardStatePanelStyle;
begin
  inherited;
  if ASource is TdxStatusBarKeyboardStatePanelStyle then
  begin
    AStyle := TdxStatusBarKeyboardStatePanelStyle(ASource);
    KeyboardStates := AStyle.KeyboardStates;
    FullRect := AStyle.FullRect;
    CapsLockKeyAppearance := AStyle.CapsLockKeyAppearance;
    NumLockKeyAppearance := AStyle.NumLockKeyAppearance;
    ScrollLockKeyAppearance := AStyle.ScrollLockKeyAppearance;
    InsertKeyAppearance := AStyle.InsertKeyAppearance;
  end;
end;

procedure TdxStatusBarKeyboardStatePanelStyle.DrawContent(
  ACanvas: TcxCanvas; R: TRect; APainter: TdxStatusBarPainterClass);
var
  AActive: Boolean;
  ACaption: string;
  AKeyInfo: TdxStatusBarKeyStateAppearance;
  ALastKeyIndex: Integer;
  ARect: TRect;
  ATextColor: TColor;
  I: Integer;
  ABounds: TRect;
begin
  inherited;
  ACanvas.Font.Assign(FFont);
  ALastKeyIndex := GetLastKeyIndex;
  ABounds := R;
  for I := Low(FKeyInfos) to High(FKeyInfos) do
  begin
    AKeyInfo := FKeyInfos[I];
    if AKeyInfo.Id in FKeyboardStates then
    begin
      AActive := not (csDesigning in StatusBarControl.ComponentState) and (Lo(GetKeyState(AKeyInfo.Code)) = 1);
      if AActive then
      begin
        ACaption := AKeyInfo.ActiveCaption;
        ATextColor := AKeyInfo.FActiveFontColor;
      end
      else
      begin
        ACaption := AKeyInfo.InactiveCaption;
        ATextColor := AKeyInfo.FInactiveFontColor;
      end;

      APainter.AdjustTextColor(Owner.GetStatusBarControl, ATextColor, AActive);
      AdjustTextColor(ATextColor, AActive);
      ACanvas.Font.Color := ATextColor;

      // key cell
      R.Right := R.Left + AKeyInfo.GetRectWidth(ACanvas, ScaleFactor) +
        cxMarginsWidth(APainter.PanelContentExtents(StatusBarControl, not FullRect));
      ARect := R;
      if StatusBarControl.UseRightToLeftAlignment then
        ARect := TdxRightToLeftLayoutConverter.ConvertRect(ARect, ABounds);
      if not FullRect then
        APainter.DrawPanelBorder(StatusBarControl, Owner.Bevel, ACanvas, ARect);

      ACanvas.Brush.Style := bsClear;
      ACanvas.DrawTexT(ACaption, ARect, cxSingleLine or cxAlignVCenter or cxAlignHCenter, StatusBarControl.Enabled);
      ACanvas.Brush.Style := bsSolid;

      if I <> ALastKeyIndex then
        R.Left := R.Right + APainter.SeparatorSize(StatusBarControl);
    end;
  end;
end;

function TdxStatusBarKeyboardStatePanelStyle.GetLastKeyIndex: Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := High(FKeyInfos) downto Low(FKeyInfos) do
  begin
    if FKeyInfos[I].Id in FKeyboardStates then
      Exit(I);
  end;
end;

function TdxStatusBarKeyboardStatePanelStyle.GetMinWidth: Integer;
var
  ALastKeyIndex: Integer;
  I: Integer;
begin
  // WARNING: sync with DrawContent
  if FKeyboardStates <> [] then
  begin
    Result := 0;
    StatusBarControl.Canvas.Font.Assign(FFont);
    ALastKeyIndex := GetLastKeyIndex;

    for I := Low(FKeyInfos) to High(FKeyInfos) do
    begin
      if FKeyInfos[I].Id in FKeyboardStates then
      begin
        Inc(Result, FKeyInfos[I].GetRectWidth(StatusBarControl.Canvas, ScaleFactor));
        Inc(Result, cxMarginsWidth(Painter.PanelContentExtents(StatusBarControl, not FullRect)));
        // key separator
        if I <> ALastKeyIndex then
          Inc(Result, Painter.SeparatorSize(StatusBarControl));
      end;
    end;
  end
  else
    Result := ScaleFactor.Apply(50);
end;

function TdxStatusBarKeyboardStatePanelStyle.InternalBevel: Boolean;
begin
  Result := not FullRect;
end;

function TdxStatusBarKeyboardStatePanelStyle.GetAppearance(Index: Integer): TdxStatusBarKeyStateAppearance;
begin
  Result := FKeyInfos[Index];
end;

procedure TdxStatusBarKeyboardStatePanelStyle.NamesChangeHandler(Sender: TObject);
begin
  Owner.Changed(False);
end;

procedure TdxStatusBarKeyboardStatePanelStyle.SetFullRect(Value: Boolean);
begin
  if FFullRect <> Value then
  begin
    FFullRect := Value;
    Owner.Changed(True);
  end;
end;

procedure TdxStatusBarKeyboardStatePanelStyle.SetAppearance(Index: Integer; Value: TdxStatusBarKeyStateAppearance);
begin
  FKeyInfos[Index].Assign(Value);
end;

procedure TdxStatusBarKeyboardStatePanelStyle.SetKeyboardStates(Value: TdxStatusBarKeyboardStates);
begin
  if FKeyboardStates <> Value then
  begin
    FKeyboardStates := Value;
    Owner.Changed(False);
  end;
end;

{ TdxStatusBarContainerControl }

constructor TdxStatusBarContainerControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csAcceptsControls, csNoDesignVisible];
  ParentDoubleBuffered := False;
end;

destructor TdxStatusBarContainerControl.Destroy;
var
  APanel:  TdxStatusBarPanel;
begin
  if FPanelStyle <> nil then
  begin
    APanel := FPanelStyle.Owner;
    FPanelStyle.FContainer := nil;
    FPanelStyle := nil;
    APanel.PanelStyleClass := nil;
  end;
  inherited Destroy;
end;

procedure TdxStatusBarContainerControl.AlignChildren;

  procedure PlaceContainerChild(AControl: TControl);
  begin
    if AControl.Align <> alNone then
      AControl.Align := alNone;
    AControl.SetBounds(0, 0, Width, Height);
  end;

var
  I: Integer;
begin
  if NeedAlignChildren then
  begin
    for I := 0 to ControlCount - 1 do
      PlaceContainerChild(Controls[I]);
  end;
end;

procedure TdxStatusBarContainerControl.BoundsChanged;
begin
  inherited BoundsChanged;
  AlignChildren;
end;

procedure TdxStatusBarContainerControl.Paint;
var
  R: TRect;
begin
  inherited Paint;
  R := ClientRect;
  if FPanelStyle <> nil then
    FPanelStyle.DrawContainerControl
  else
    Canvas.FillRect(R, clBtnFace);

  if csDesigning in ComponentState then
  begin
    Canvas.Pen.Color := clBtnShadow;
    Canvas.Brush.Color := clBtnShadow;
    Canvas.Brush.Style := bsBDiagonal;
    Canvas.Canvas.Rectangle(R);
    Canvas.Brush.Style := bsSolid;
  end;
end;

function TdxStatusBarContainerControl.MayFocus: Boolean;
begin
  Result := False;
end;

function TdxStatusBarContainerControl.NeedAlignChildren: Boolean;
begin
  Result := (FPanelStyle <> nil) and
    FPanelStyle.AlignControl and not FPanelStyle.StatusBarControl.IsDesigning;
end;

function TdxStatusBarContainerControl.NeedsScrollBars: Boolean;
begin
  Result := False;
end;

procedure TdxStatusBarContainerControl.CMControlChange(var Message: TCMControlChange);
begin
  inherited;
  if Message.Inserting then
    AlignChildren;
end;

{ TdxStatusBarContainerPanelStyle }

constructor TdxStatusBarContainerPanelStyle.Create(AOwner: TdxStatusBarPanel);
begin
  inherited Create(AOwner);
  FAlignControl := True;
  if StatusBarControl.ComponentState * [csUpdating, csLoading, csReading] = [] then
  begin
    FContainer := TdxStatusBarContainerControl.Create(StatusBarControl.Owner);
    FContainer.FPanelStyle := Self;
    FContainer.Parent := StatusBarControl;
    if csDesigning in StatusBarControl.ComponentState then
      GenContainerName(AOwner, FContainer);
  end;
end;

destructor TdxStatusBarContainerPanelStyle.Destroy;
begin
  Container := nil;
  inherited Destroy;
end;

procedure TdxStatusBarContainerPanelStyle.BoundsChanged(const ABounds: TRect);
var
  R: TRect;
begin
  inherited BoundsChanged(ABounds);
  if Container <> nil then
  begin
    R := cxRectContent(ABounds, Painter.PanelBorderSizes(Painter.GetPanelBevel(Owner)));
    CheckSizeGripRect(R);
    FContainerBoundsRect := R;
    if not StatusBarControl.UseRightToLeftAlignment then
      Container.BoundsRect := R;
  end;
end;

procedure TdxStatusBarContainerPanelStyle.RestoreDefaults;
begin
  FAlignControl := True;
  inherited RestoreDefaults;
end;

function TdxStatusBarContainerPanelStyle.CanDelete: Boolean;
begin
  Result := (Container = nil) or not (csAncestor in Container.ComponentState);
end;

procedure TdxStatusBarContainerPanelStyle.DoAssign(ASource: TPersistent);
var
  AStyle: TdxStatusBarContainerPanelStyle;
begin
  inherited;
  if ASource is TdxStatusBarContainerPanelStyle then
  begin
    AStyle := TdxStatusBarContainerPanelStyle(ASource);
    AlignControl := AStyle.AlignControl;
    if AStyle.Container <> nil then
      Container := StatusBarControl.ContainerByName(AStyle.Container.Name);
  end;
end;

procedure TdxStatusBarContainerPanelStyle.DoRightToLeftConversion(const ABounds: TRect);
begin
  if Container <> nil then
    Container.BoundsRect := TdxRightToLeftLayoutConverter.ConvertRect(FContainerBoundsRect, ABounds);
end;

procedure TdxStatusBarContainerPanelStyle.DrawContainerControl;
begin
  Painter.DrawContainerControl(Self);
end;

procedure TdxStatusBarContainerPanelStyle.UpdateSubControlsVisibility;
begin
  if Container <> nil then
    Container.Visible := Owner.Visible and not Owner.StatusBarControl.SimplePanelStyle.Active;
end;

procedure TdxStatusBarContainerPanelStyle.SetContainer(Value: TdxStatusBarContainerControl);
begin
  if FContainer <> Value then
  begin
    if Value = nil then
    begin
      FContainer.FPanelStyle := nil;
      if StatusBarControl.ComponentState * [csDestroying, csUpdating, csLoading, csReading] <> [] then
        FContainer := nil
      else
        FreeAndNil(FContainer);
    end
    else
    begin
      FContainer := Value;
      FContainer.FPanelStyle := Self;
    end;
    UpdateSubControlsVisibility;
  end;
end;

{ TdxStatusBarPanel }

constructor TdxStatusBarPanel.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FVisible := True;
  FMinWidth := 20;
  FWidth := 50;
  FFixed := True;
  FParentBiDiMode := True;
  FBevel := dxpbLowered;
  ParentBiDiModeChanged;
  // auto create
  if StatusBarControl.ComponentState * [csLoading, csReading] = [] then
    PanelStyleClass := StatusBarControl.GetDefaultPanelStyleClass;
end;

destructor TdxStatusBarPanel.Destroy;
begin
  DestroyPanelStyle;
  inherited Destroy;
end;

procedure TdxStatusBarPanel.Assign(Source: TPersistent);
begin
  if Source is TdxStatusBarPanel then
  begin
    if Assigned(Collection) then Collection.BeginUpdate;
    try
      RestoreDefaults;

      PanelStyleClassName := TdxStatusBarPanel(Source).PanelStyleClassName;
      PanelStyle := TdxStatusBarPanel(Source).PanelStyle;

      Visible := TdxStatusBarPanel(Source).Visible;
      Bevel := TdxStatusBarPanel(Source).Bevel;
      BiDiMode := TdxStatusBarPanel(Source).BiDiMode;
      Fixed := TdxStatusBarPanel(Source).Fixed;
      if TdxStatusBarPanel(Source).IsMinWidthStored then
        MinWidth := TdxStatusBarPanel(Source).MinWidth;
      ParentBiDiMode := TdxStatusBarPanel(Source).ParentBiDiMode;
      Text := TdxStatusBarPanel(Source).Text;
      if TdxStatusBarPanel(Source).IsWidthStored then
        Width := TdxStatusBarPanel(Source).Width;

      OnClick := TdxStatusBarPanel(Source).OnClick;
      OnDblClick := TdxStatusBarPanel(Source).OnDblClick;
      OnDrawPanel := TdxStatusBarPanel(Source).OnDrawPanel;
    finally
      if Assigned(Collection) then Collection.EndUpdate;
    end;
  end
  else
    inherited Assign(Source);
end;

procedure TdxStatusBarPanel.RestoreDefaults;
begin
  FVisible := True;
  FMinWidth := 20;
  FWidth := 50;
  FFixed := True;
  FParentBiDiMode := True;
  FBevel := dxpbLowered;
  FIsMinWidthAssigned := False;
  FIsWidthAssigned := False;
  Changed(True);
end;

procedure TdxStatusBarPanel.ParentBiDiModeChanged;
begin
  if FParentBiDiMode then
  begin
    if GetOwner <> nil then
    begin
      BiDiMode := TdxStatusBarPanels(GetOwner).FStatusBarControl.BiDiMode;
      FParentBiDiMode := True;
    end;
  end;
end;

function TdxStatusBarPanel.UseRightToLeftReading: Boolean;
begin
  Result := SysLocale.MiddleEast and (BiDiMode <> bdLeftToRight);
end;

procedure TdxStatusBarPanel.UpdateResources;
begin
  if PanelStyle <> nil then
    PanelStyle.UpdateResources;
end;

procedure TdxStatusBarPanel.UpdateSubControlsVisibility;
begin
  if PanelStyle <> nil then
    PanelStyle.UpdateSubControlsVisibility;
end;

function TdxStatusBarPanel.UseRightToLeftAlignment: Boolean;
begin
  Result := SysLocale.MiddleEast and (BiDiMode = bdRightToLeft);
end;

procedure TdxStatusBarPanel.ChangeScale(M: Integer; D: Integer);
begin
  if FPanelStyle <> nil then
    FPanelStyle.ChangeScale(M, D);
end;

procedure TdxStatusBarPanel.Click;
begin
  if Assigned(FOnClick) then FOnClick(Self);
end;

procedure TdxStatusBarPanel.CreatePanelStyle;
begin
  if FPanelStyleClass <> nil then
    FPanelStyle := FPanelStyleClass.Create(Self);
  StatusBarControl.UpdatePanels;
end;

procedure TdxStatusBarPanel.DblClick;
begin
  if Assigned(FOnDblClick) then FOnDblClick(Self);
end;

function TdxStatusBarPanel.DefaultMinWidth: Integer;
begin
  if PanelStyle <> nil then
    Result := PanelStyle.GetMinWidth
  else
    Result := ScaleFactor.Apply(20);
end;

function TdxStatusBarPanel.DefaultWidth: Integer;
begin
  if (PanelStyle <> nil) and not PanelStyle.CanSizing then
    Result := DefaultMinWidth
  else
    Result := ScaleFactor.Apply(50);
end;

procedure TdxStatusBarPanel.DestroyPanelStyle;
begin
  if (StatusBarControl.ComponentState * [csDestroying, csUpdating, csLoading, csReading] = []) and
    (PanelStyle <> nil) and not PanelStyle.CanDelete then
    raise EdxException.Create(cxSCantDeleteAncestor);
  FreeAndNil(FPanelStyle);
end;

function TdxStatusBarPanel.GetDisplayName: string;
begin
  Result := Text;
  if Result = '' then
    Result := inherited GetDisplayName;
end;

procedure TdxStatusBarPanel.Loaded;
begin
  if PanelStyle <> nil then
    PanelStyle.Loaded;
end;

function TdxStatusBarPanel.PaintMinWidth: Integer;
begin
  Result := MinWidth;
  PreparePaintWidth(Result);
end;

function TdxStatusBarPanel.PaintWidth: Integer;
begin
  Result := Width;
  PreparePaintWidth(Result);
end;

procedure TdxStatusBarPanel.PreparePaintWidth(var AWidth: Integer);
begin
  if PanelStyle <> nil then
  begin
    // bevel
    if not PanelStyle.InternalBevel then
      Inc(AWidth, 2);
    // size grip
    if StatusBarControl.SizeGripAllocated then
    begin
      if Self = StatusBarControl.ViewInfo.Panels[StatusBarControl.ViewInfo.PanelCount - 1] then
        Inc(AWidth, StatusBarControl.Painter.GripAreaSize(StatusBarControl).cx);
    end;
  end;
end;

procedure TdxStatusBarPanel.StatusBarPanelStyleChanged;
begin
  Changed(False);
end;

function TdxStatusBarPanel.GetFixed: Boolean;
begin
  if (PanelStyle <> nil) and not PanelStyle.CanSizing then
    Result := True
  else
    Result := FFixed;
end;

function TdxStatusBarPanel.GetLookAndFeel: TcxLookAndFeel;
begin
  Result := StatusBarControl.LookAndFeel;
end;

function TdxStatusBarPanel.GetMinWidth: Integer;
begin
  if not FIsMinWidthAssigned or (PanelStyle <> nil) and not PanelStyle.CanSizing then
    Result := DefaultMinWidth
  else
    Result := ScaleFactor.Apply(FMinWidth);
end;

function TdxStatusBarPanel.GetPanelStyleClassName: string;
begin
  if FPanelStyle = nil then
    Result := ''
  else
    Result := FPanelStyle.ClassName;
end;

function TdxStatusBarPanel.GetScaleFactor: TdxScaleFactor;
begin
  Result := StatusBarControl.ScaleFactor;
end;

function TdxStatusBarPanel.GetStatusBarControl: TdxCustomStatusBar;
begin
  Result := TdxStatusBarPanels(Collection).FStatusBarControl;
end;

function TdxStatusBarPanel.GetWidth: Integer;
begin
  if not FIsWidthAssigned or (PanelStyle <> nil) and not PanelStyle.CanSizing then
    Result := DefaultWidth
  else
    Result := ScaleFactor.Apply(FWidth);
end;

function TdxStatusBarPanel.IsBiDiModeStored: Boolean;
begin
  Result := not FParentBiDiMode;
end;

function TdxStatusBarPanel.IsMinWidthStored: Boolean;
begin
  Result := FIsMinWidthAssigned;
end;

function TdxStatusBarPanel.IsWidthStored: Boolean;
begin
  Result := FIsWidthAssigned;
end;

procedure TdxStatusBarPanel.SetBevel(Value: TdxStatusBarPanelBevel);
begin
  if FBevel <> Value then
  begin
    FBevel := Value;
    Changed(False);
  end;
end;

procedure TdxStatusBarPanel.SetBiDiMode(Value: TBiDiMode);
begin
  if Value <> FBiDiMode then
  begin
    FBiDiMode := Value;
    FParentBiDiMode := False;
    Changed(False);
  end;
end;

procedure TdxStatusBarPanel.SetFixed(Value: Boolean);
begin
  if FFixed <> Value then
  begin
    FFixed := Value;
    Changed(False);
  end;
end;

procedure TdxStatusBarPanel.SetMinWidth(Value: Integer);
begin
  Value := Max(Value, 0);
  FMinWidth := ScaleFactor.Revert(Value);
  FIsMinWidthAssigned := True;
  Width := Max(Width, FMinWidth);
  Changed(False);
end;

procedure TdxStatusBarPanel.SetPanelStyle(Value: TdxStatusBarPanelStyle);
begin
  if (FPanelStyle <> nil) and (Value <> nil) then
    FPanelStyle.Assign(Value);
end;

procedure TdxStatusBarPanel.SetPanelStyleClass(const Value: TdxStatusBarPanelStyleClass);
begin
  if (FPanelStyleClass <> Value) and StatusBarControl.CanAcceptPanelStyle(Value) then
  begin
    DestroyPanelStyle;
    FPanelStyleClass := Value;
    CreatePanelStyle;
  end;
end;

procedure TdxStatusBarPanel.SetPanelStyleClassName(Value: string);
begin
  PanelStyleClass := TdxStatusBarPanelStyleClass(GetRegisteredStatusBarPanelStyles.FindByClassName(Value));
end;

procedure TdxStatusBarPanel.SetParentBiDiMode(Value: Boolean);
begin
  if FParentBiDiMode <> Value then
  begin
    FParentBiDiMode := Value;
    ParentBiDiModeChanged;
  end;
end;

procedure TdxStatusBarPanel.SetText(const Value: string);
begin
  if FText <> Value then
  begin
    FText := Value;
    Changed(False);
  end;
end;

procedure TdxStatusBarPanel.SetVisible(Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    UpdateSubControlsVisibility;
    Changed(False);
  end;
end;

procedure TdxStatusBarPanel.SetWidth(Value: Integer);
begin
  FWidth := Max(ScaleFactor.Revert(Value), FMinWidth);
  FIsWidthAssigned := True;
  Changed(False);
end;

{ TdxStatusBarPanels }

constructor TdxStatusBarPanels.Create(AStatusBarControl: TdxCustomStatusBar);
begin
  inherited Create(GetStatusPanelClass);
  FStatusBarControl := AStatusBarControl;
end;

function TdxStatusBarPanels.Add: TdxStatusBarPanel;
begin
  Result := TdxStatusBarPanel.Create(Self);
end;

function TdxStatusBarPanels.Insert(Index: Integer): TdxStatusBarPanel;
begin
  BeginUpdate;
  try
    if Index < 0 then Index := 0;
    if Index > Count then Index := Count;
    Result := Add;
    Result.Index := Index;
  finally
    EndUpdate;
  end;
end;

procedure TdxStatusBarPanels.ChangeScale(M: Integer; D: Integer);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].ChangeScale(M, D);
end;

function TdxStatusBarPanels.GetOwner: TPersistent;
begin
  Result := FStatusBarControl;
end;

function TdxStatusBarPanels.GetStatusPanelClass: TdxStatusBarPanelClass;
begin
  Result := TdxStatusBarPanel;
end;

procedure TdxStatusBarPanels.Loaded;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].Loaded;
end;

procedure TdxStatusBarPanels.Update(Item: TCollectionItem);
begin
  if FStatusBarControl <> nil then
    FStatusBarControl.UpdatePanels;
end;

procedure TdxStatusBarPanels.UpdateResources;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].UpdateResources;
end;

procedure TdxStatusBarPanels.UpdateSubControlsVisibility;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].UpdateSubControlsVisibility;
end;

function TdxStatusBarPanels.GetItem(Index: Integer): TdxStatusBarPanel;
begin
  Result := TdxStatusBarPanel(inherited GetItem(Index));
end;

procedure TdxStatusBarPanels.SetItem(Index: Integer; Value: TdxStatusBarPanel);
begin
  inherited SetItem(Index, Value);
end;

{ TdxStatusBarSimplePanelStyle }

constructor TdxStatusBarSimplePanelStyle.Create(AStatusBar: TdxCustomStatusBar);
begin
  inherited Create;
  FStatusBar := AStatusBar;
end;

procedure TdxStatusBarSimplePanelStyle.Assign(Source: TPersistent);
begin
  if Source is TdxStatusBarSimplePanelStyle then
    DoAssign(Source)
  else
    inherited Assign(Source);
end;

procedure TdxStatusBarSimplePanelStyle.DoAssign(ASource: TPersistent);
begin
  if ASource is TdxStatusBarSimplePanelStyle then
  begin
    Active := TdxStatusBarSimplePanelStyle(ASource).Active;
    AutoHint := TdxStatusBarSimplePanelStyle(ASource).AutoHint;
    Text := TdxStatusBarSimplePanelStyle(ASource).Text;
  end;
end;

procedure TdxStatusBarSimplePanelStyle.SetActive(const AValue: Boolean);
begin
  if Active <> AValue then
  begin
    FActive := AValue;
    StatusBar.Panels.UpdateSubControlsVisibility;
    StatusBar.UpdatePanels;
  end;
end;

procedure TdxStatusBarSimplePanelStyle.SetText(const AValue: string);
begin
  if Text <> AValue then
  begin
    FText := AValue;
    StatusBar.Invalidate;
  end;
end;

{ TdxStatusBarViewInfo }

constructor TdxStatusBarViewInfo.Create(AOwner: TdxCustomStatusBar);
begin
  inherited Create;
  FPanelsViewInfos := TcxObjectList.Create;
  FStatusBar := AOwner;
end;

destructor TdxStatusBarViewInfo.Destroy;
begin
  FreeAndNil(FPanelsViewInfos);
  FreeAndNil(FSimplePanelViewInfo);
  inherited Destroy;
end;

procedure TdxStatusBarViewInfo.AddCalculatedItems(AAutoWidthObject: TcxAutoWidthObject);

  procedure AddAutoWidthSeparator(ASeparatorSize: Integer);
  begin
    with AAutoWidthObject.AddItem do
    begin
      MinWidth := ASeparatorSize;
      Width := ASeparatorSize;
      Fixed := True;
    end;
  end;

  procedure AddAutoWidthPanel(APanel: TdxStatusBarPanel; APanelIndex: Integer; var ANonFixedExists: Boolean);
  var
    AAutoWidthItem: TcxAutoWidthItem;
  begin
    AAutoWidthItem := AAutoWidthObject.AddItem;
    AAutoWidthItem.MinWidth := APanel.PaintMinWidth;
    AAutoWidthItem.Width := APanel.PaintWidth;
    if APanel.Fixed then
      AAutoWidthItem.Fixed := ANonFixedExists or (APanelIndex < PanelCount - 1)
    else
    begin
      AAutoWidthItem.Fixed := False;
      ANonFixedExists := True;
    end;
  end;

var
  ANonFixedExists: Boolean;
  I, ASeparatorSize: Integer;
begin
  ANonFixedExists := False;
  ASeparatorSize := StatusBar.Painter.SeparatorSize(StatusBar);
  for I := 0 to PanelCount - 1 do
  begin
    AddAutoWidthPanel(Panels[I], I, ANonFixedExists);
    if I < PanelCount - 1 then
      AddAutoWidthSeparator(ASeparatorSize);
  end;
end;

procedure TdxStatusBarViewInfo.Calculate(const ABounds: TRect);
var
  AAutoWidthObject: TcxAutoWidthObject;
  ACount: Integer;
  AWidth: Integer;
begin
  FIsRightToLeftConverted := False;
  RecreateChildrenViewInfo;
  ACount := GetCalculatedItemCount;
  FSizeGripRect := CalculateSizeGripRect(ABounds);

  if cxRectIsEmpty(SizeGripRect) or IsSizeGripInPanelArea then
    AWidth := cxRectWidth(ABounds) + cxMarginsWidth(Painter.ContentExtents(StatusBar))
  else
    AWidth := SizeGripRect.Left - Painter.SizeGripMargins(StatusBar).Left - ABounds.Left;

  if StatusBar.SimplePanelStyle.Active then
  begin
    SimplePanelViewInfo.FIsRightToLeftConverted := False;
    SimplePanelViewInfo.Bounds := cxRectSetWidth(ABounds, AWidth);
  end
  else
    if ACount > 0 then
    begin
      AAutoWidthObject := TcxAutoWidthObject.Create(ACount);
      try
        AddCalculatedItems(AAutoWidthObject);
        AAutoWidthObject.AvailableWidth := AWidth;
        AAutoWidthObject.Calculate;
        CalculatePanelsViewInfos(ABounds, AAutoWidthObject);
      finally
        AAutoWidthObject.Free;
      end;
    end;
end;

function TdxStatusBarViewInfo.CalculateAutoHeight: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to PanelCount - 1 do
    Result := Max(Result, PanelViewInfo[I].CalculateAutoHeight);
end;

procedure TdxStatusBarViewInfo.CalculatePanelsViewInfos(
  const ABounds: TRect; AAutoWidthObject: TcxAutoWidthObject);
var
  APanelViewInfo: TdxStatusBarPanelViewInfo;
  I, AIndex: Integer;
  R: TRect;
begin
  AIndex := 0;
  R := ABounds;
  for I := 0 to PanelCount - 1 do
  begin
    APanelViewInfo := PanelViewInfo[I];
    APanelViewInfo.FIsRightToLeftConverted := False;
    APanelViewInfo.Bounds := cxRectSetWidth(R, AAutoWidthObject[AIndex].AutoWidth);
    R.Left := APanelViewInfo.Bounds.Right;
    if AIndex + 1 < AAutoWidthObject.Count then
    begin
      APanelViewInfo.SeparatorRect := cxRectSetWidth(R, AAutoWidthObject[AIndex + 1].AutoWidth);
      R.Left := APanelViewInfo.SeparatorRect.Right;
    end;
    Inc(AIndex, 2);
  end;
end;

function TdxStatusBarViewInfo.CalculateSizeGripRect(const ABounds: TRect): TRect;
begin
  if StatusBar.SizeGripAllocated then
  begin
    Result := cxRectContent(ABounds, Painter.SizeGripMargins(StatusBar));
    Result.Left := Result.Right - Painter.GripAreaSize(StatusBar).cx;
  end
  else
    Result := cxNullRect;
end;

function TdxStatusBarViewInfo.CreatePanelViewInfo(APanel: TdxStatusBarPanel): TdxStatusBarPanelViewInfo;
begin
  Result := TdxStatusBarPanelViewInfo.Create(APanel);
end;

function TdxStatusBarViewInfo.CreateSimplePanelViewInfo: TdxStatusBarSimplePanelViewInfo;
begin
  Result := TdxStatusBarSimplePanelViewInfo.Create(StatusBar);
end;

procedure TdxStatusBarViewInfo.DoRightToLeftConversion(const ABounds: TRect);
var
  I: Integer;
begin
  FSizeGripRect := TdxRightToLeftLayoutConverter.ConvertRect(FSizeGripRect, ABounds);
  if StatusBar.SimplePanelStyle.Active then
    SimplePanelViewInfo.RightToLeftConversion(ABounds)
  else
    for I := 0 to PanelCount - 1 do
      PanelViewInfo[I].RightToLeftConversion(ABounds);
end;

procedure TdxStatusBarViewInfo.DrawPanels(ACanvas: TcxCanvas; const R: TRect);
var
  I: Integer;
begin
  if StatusBar.SimplePanelStyle.Active then
    SimplePanelViewInfo.Draw(ACanvas)
  else
    if PanelCount > 0 then
      for I := 0 to PanelCount - 1 do
        PanelViewInfo[I].Draw(ACanvas)
    else
      Painter.DrawEmptyPanel(StatusBar, ACanvas, R);
end;

procedure TdxStatusBarViewInfo.DrawSizeGrip(ACanvas: TcxCanvas);
begin
  if StatusBar.SizeGripAllocated then
    Painter.DrawSizeGrip(StatusBar, ACanvas, SizeGripRect);
end;

procedure TdxStatusBarViewInfo.Draw(ACanvas: TcxCanvas);
var
  R: TRect;
begin
  R := StatusBar.ClientBounds;
  Painter.DrawBorder(StatusBar, ACanvas, R);
  ACanvas.IntersectClipRect(R); // !!!
  DrawPanels(ACanvas, R);
  DrawSizeGrip(ACanvas);
end;

procedure TdxStatusBarViewInfo.RecreateChildrenViewInfo;
var
  I: Integer;
begin
  FPanelsViewInfos.Clear;
  FreeAndNil(FSimplePanelViewInfo);
  if StatusBar.SimplePanelStyle.Active then
    FSimplePanelViewInfo := CreateSimplePanelViewInfo
  else
    for I := 0 to StatusBar.Panels.Count - 1 do
    begin
      if StatusBar.Panels[I].Visible then
        FPanelsViewInfos.Add(CreatePanelViewInfo(StatusBar.Panels[I]));
    end;
end;

function TdxStatusBarViewInfo.GetCalculatedItemCount: Integer;
begin
  Result := PanelCount; //visible panels
  Inc(Result, Result - 1); //separators
end;

function TdxStatusBarViewInfo.GetPanelAt(const P: TPoint): TdxStatusBarPanel;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to PanelCount - 1 do
    if PtInRect(PanelViewInfo[I].Bounds, P) then
    begin
      Result := Panels[I];
      Break;
    end;
end;

function TdxStatusBarViewInfo.GetPainter: TdxStatusBarPainterClass;
begin
  Result := StatusBar.Painter;
end;

function TdxStatusBarViewInfo.GetPanel(Index: Integer): TdxStatusBarPanel;
begin
  Result := PanelViewInfo[Index].Panel;
end;

function TdxStatusBarViewInfo.GetPanelCount: Integer;
begin
  Result := FPanelsViewInfos.Count;
end;

function TdxStatusBarViewInfo.GetPanelViewInfo(Index: Integer): TdxStatusBarPanelViewInfo;
begin
  Result := TdxStatusBarPanelViewInfo(FPanelsViewInfos[Index]);
end;

function TdxStatusBarViewInfo.GetSizeGripAreaRect: TRect;
begin
  Result := cxRectSetRight(SizeGripRect, SizeGripRect.Right, cxRectWidth(SizeGripRect) + Painter.SeparatorSize(StatusBar));
end;

function TdxStatusBarViewInfo.IsSizeGripInPanelArea: Boolean;
begin
  Result := Painter.IsSizeGripInPanelArea(StatusBar);
end;

procedure TdxStatusBarViewInfo.RightToLeftConversion(const ABounds: TRect);
begin
  if not FIsRightToLeftConverted then
  begin
    DoRightToLeftConversion(ABounds);
    FIsRightToLeftConverted := True;
  end;
end;

{ TdxStatusBarCustomPanelViewInfo }

constructor TdxStatusBarCustomPanelViewInfo.Create(AStatusBar: TdxCustomStatusBar);
begin
  inherited Create;
  FStatusBar := AStatusBar;
end;

function TdxStatusBarCustomPanelViewInfo.CalculateAutoHeight: Integer;
begin
  Result := Max(Painter.GripAreaSize(StatusBar).cy, cxTextHeight(PanelFont) +
    cxMarginsHeight(Painter.PanelContentExtents(StatusBar, PanelBevel <> dxpbNone))) +
    cxMarginsHeight(Painter.PanelBorderSizes(PanelBevel));
end;

procedure TdxStatusBarCustomPanelViewInfo.DoRightToLeftConversion(const ABounds: TRect);
begin
  FBounds := TdxRightToLeftLayoutConverter.ConvertRect(FBounds, ABounds);
end;

function TdxStatusBarCustomPanelViewInfo.GetPainter: TdxStatusBarPainterClass;
begin
  Result := StatusBar.Painter;
end;

function TdxStatusBarCustomPanelViewInfo.GetPanelFont: TFont;
begin
  Result := StatusBar.Font;
end;

procedure TdxStatusBarCustomPanelViewInfo.RightToLeftConversion(const ABounds: TRect);
begin
  if not FIsRightToLeftConverted then
  begin
    DoRightToLeftConversion(ABounds);
    FIsRightToLeftConverted := True;
  end;
end;

procedure TdxStatusBarCustomPanelViewInfo.SetBounds(const AValue: TRect);
begin
  FBounds := AValue;
end;

{ TdxStatusBarPanelViewInfo }

constructor TdxStatusBarPanelViewInfo.Create(APanel: TdxStatusBarPanel);
begin
  inherited Create(APanel.StatusBarControl);
  FPanel := APanel;
end;

procedure TdxStatusBarPanelViewInfo.DoRightToLeftConversion(const ABounds: TRect);
begin
  inherited DoRightToLeftConversion(ABounds);
  FSeparatorRect := TdxRightToLeftLayoutConverter.ConvertRect(FSeparatorRect, ABounds);
  if Panel.PanelStyle <> nil then
    Panel.PanelStyle.RightToLeftConversion(ABounds);
end;

procedure TdxStatusBarPanelViewInfo.Draw(ACanvas: TcxCanvas);
begin
  Painter.DrawPanel(StatusBar, Panel, ACanvas, Bounds);
  if HasSeparator then
    Painter.DrawPanelSeparator(StatusBar, ACanvas, SeparatorRect);
end;

function TdxStatusBarPanelViewInfo.GetHasSeparator: Boolean;
begin
  Result := not IsRectEmpty(SeparatorRect);
end;

function TdxStatusBarPanelViewInfo.GetPanelBevel: TdxStatusBarPanelBevel;
begin
  Result := Painter.GetPanelBevel(Panel);
end;

function TdxStatusBarPanelViewInfo.GetPanelFont: TFont;
begin
  if Panel.PanelStyle <> nil then
    Result := Panel.PanelStyle.Font
  else
    Result := inherited GetPanelFont;
end;

procedure TdxStatusBarPanelViewInfo.SetBounds(const AValue: TRect);
begin
  inherited SetBounds(AValue);
  if Panel.PanelStyle <> nil then
  begin
    Panel.PanelStyle.FIsRightToLeftConverted := False;
    Panel.PanelStyle.BoundsChanged(Bounds);
  end;
end;

{ TdxStatusBarSimplePanelViewInfo }

procedure TdxStatusBarSimplePanelViewInfo.Draw(ACanvas: TcxCanvas);
begin
  Painter.DrawSimplePanel(StatusBar, StatusBar.SimplePanelStyle.Text, ACanvas, Bounds);
end;

function TdxStatusBarSimplePanelViewInfo.GetPanelBevel: TdxStatusBarPanelBevel;
begin
  Result := dxpbLowered;
end;

{ TdxCustomStatusBar }

constructor TdxCustomStatusBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FViewInfo := CreateViewInfo;
  ControlStyle := ControlStyle - [csSetCaption] + [csCaptureMouse, csClickEvents, csDoubleClicks, csOpaque];
  DoubleBuffered := True;
  Color := clBtnFace;
  Align := alBottom;
  ParentFont := False;
  FSizeGrip := True;
  FPanels := CreatePanels;
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;
  FThemeChangedNotificator := TdxThemeChangedNotificator.Create;
  FThemeChangedNotificator.OnThemeChanged := ThemeChanged;
  FSimplePanelStyle := TdxStatusBarSimplePanelStyle.Create(Self);
  PaintStyleChanged;
  CreateOffice11Colors;
  Height := 20;
end;

destructor TdxCustomStatusBar.Destroy;
begin
  ReleaseOffice11Colors;
  FreeAndNil(FSimplePanelStyle);
  FreeAndNil(FThemeChangedNotificator);
  FreeAndNil(FImageChangeLink);
  FreeAndNil(FPanels);
  FreeAndNil(FViewInfo);
  inherited Destroy;
end;

procedure TdxCustomStatusBar.Calculate;
begin
  FIsRightToLeftConverted := False;
  ViewInfo.Calculate(GetPanelsBounds);
end;

function TdxCustomStatusBar.CalculateAutoHeight: Integer;
begin
  Result := ViewInfo.CalculateAutoHeight + cxMarginsHeight(GetPanelsOffsets);
end;

function TdxCustomStatusBar.CanAcceptPanelStyle(Value: TdxStatusBarPanelStyleClass): Boolean;
begin
  Result := (Value = nil) or (Value.GetVersion = 0);
end;

function TdxCustomStatusBar.CanAutoSize(var NewWidth, NewHeight: Integer): Boolean;
begin
  Result := not (IsDestroying or (sbsCalculating in FState));
  if Result then
    NewHeight := CalculateAutoHeight;
end;

procedure TdxCustomStatusBar.ChangeScaleEx(M, D: Integer; isDpiChange: Boolean);
begin
  Panels.ChangeScale(M, D);
  inherited;
end;

procedure TdxCustomStatusBar.CheckAutoSize;
var
  ANewWidth, ANewHeight: Integer;
begin
  if AutoSize then
  begin
    ANewWidth := Width;
    ANewHeight := Height;
    if CanAutoSize(ANewWidth, ANewHeight) then
      SetBounds(Left, Top, ANewWidth, ANewHeight);
  end;
end;

function TdxCustomStatusBar.ContainerByName(const AName: string): TdxStatusBarContainerControl;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to ControlCount - 1 do
    if (Controls[I] is TdxStatusBarContainerControl) and
      (CompareText(TdxStatusBarContainerControl(Controls[I]).Name, AName) = 0) then
    begin
      Result := TdxStatusBarContainerControl(Controls[I]);
      Break;
    end;
end;

function TdxCustomStatusBar.CreatePanel: TdxStatusBarPanel;
begin
  Result := GetStatusPanelClass.Create(FPanels);
end;

function TdxCustomStatusBar.CreatePanels: TdxStatusBarPanels;
begin
  Result := GetStatusPanelsClass.Create(Self);
end;

function TdxCustomStatusBar.ExecuteAction(Action: TBasicAction): Boolean;
var
  I: Integer;
  APanel: TdxStatusBarPanel;
begin
  Result := False;
  if not (csDesigning in ComponentState) and (Action is THintAction) and not DoHint then
  begin
    if SimplePanelStyle.Active then
    begin
      if SimplePanelStyle.AutoHint then
      begin
        SimplePanelStyle.Text := THintAction(Action).Hint;
        Result := True;
      end;
    end
    else
      for I := 0 to Panels.Count - 1 do
      begin
        APanel := Panels[I];
        if (APanel.PanelStyle is TdxStatusBarTextPanelStyle) and
          TdxStatusBarTextPanelStyle(APanel.PanelStyle).AutoHint then
        begin
          APanel.Text := THintAction(Action).Hint;
          Result := True;
          Break;
        end;
      end;
  end;
  Result := Result or inherited ExecuteAction(Action);
end;

function TdxCustomStatusBar.GetPanelAt(X, Y: Integer): TdxStatusBarPanel;
begin
  Result := ViewInfo.GetPanelAt(Point(X, Y));
end;

procedure TdxCustomStatusBar.AdjustTextColor(var AColor: TColor; Active: Boolean);
begin
end;

procedure TdxCustomStatusBar.BiDiModeChanged;
var
  I: Integer;
begin
  inherited;
  if HandleAllocated then
  begin
    for I := 0 to Panels.Count - 1 do
      if Panels[I].ParentBiDiMode then
        Panels[I].ParentBiDiModeChanged;
    UpdatePanels;
  end;
end;

function TdxCustomStatusBar.CreateViewInfo: TdxStatusBarViewInfo;
begin
  Result := TdxStatusBarViewInfo.Create(Self);
end;

procedure TdxCustomStatusBar.FontChanged;
var
  I: Integer;
begin
  inherited;
  for I := 0 to Panels.Count - 1 do
    if Panels[I].PanelStyle <> nil then
      Panels[I].PanelStyle.ParentFontChanged;
end;

function TdxCustomStatusBar.HasBackground: Boolean;
begin
  Result := False;
end;

procedure TdxCustomStatusBar.Loaded;
begin
  inherited Loaded;
  Panels.Loaded;
  Recalculate;
end;

procedure TdxCustomStatusBar.LookAndFeelChanged(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues);
begin
  inherited;
  PaintStyleChanged;
end;

function TdxCustomStatusBar.MayFocus: Boolean;
begin
  Result := False;
end;

procedure TdxCustomStatusBar.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  APanel: TdxStatusBarPanel;
begin
  inherited;
  if Button = mbLeft then
  begin
    APanel := GetPanelAt(X, Y);
    if (APanel <> nil) and (ssDouble in Shift) then
      APanel.DblClick;
  end;
end;

procedure TdxCustomStatusBar.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  APanel: TdxStatusBarPanel;
begin
  inherited;
  if Button = mbLeft then
  begin
    APanel := GetPanelAt(X, Y);
    if APanel <> nil then
      APanel.Click;
  end;
end;

function TdxCustomStatusBar.NeedsScrollBars: Boolean;
begin
  Result := False;
end;

procedure TdxCustomStatusBar.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = Images) then
    Images := nil;
end;

procedure TdxCustomStatusBar.Paint;
begin
  inherited Paint;
  ViewInfo.Draw(Canvas);
end;

procedure TdxCustomStatusBar.Resize;
begin
  inherited Resize;
  UpdatePanels;
end;

function TdxCustomStatusBar.DoHint: Boolean;
begin
  Result := Assigned(FOnHint);
  if Result then
    FOnHint(Self);
end;

class function TdxCustomStatusBar.GetDefaultPanelStyleClass: TdxStatusBarPanelStyleClass;
begin
  Result := TdxStatusBarTextPanelStyle;
end;

function TdxCustomStatusBar.GetPainterClass: TdxStatusBarPainterClass;
begin
  if (PaintStyle = stpsUseLookAndFeel) and Assigned(dxStatusBarSkinPainterClass) and Assigned(LookAndFeel.SkinPainter) then
    Result := dxStatusBarSkinPainterClass
  else
    case GetPaintStyle of
      stpsStandard:
        Result := TdxStatusBarStandardPainter;
      stpsFlat:
        Result := TdxStatusBarFlatPainter;
      stpsXP:
        if FThemeAvailable then
          Result := TdxStatusBarXPPainter
        else
          Result := TdxStatusBarStandardPainter;
      stpsOffice11:
        Result := TdxStatusBarOffice11Painter;
    else
      Result := TdxStatusBarStandardPainter;
    end;
end;

function TdxCustomStatusBar.GetPaintStyle: TdxStatusBarPaintStyle;
const
  AStyles: array[TcxLookAndFeelKind] of TdxStatusBarPaintStyle = (
    stpsStandard, stpsStandard, stpsFlat, stpsOffice11);
begin
  if PaintStyle = stpsUseLookAndFeel then
  begin
    if LookAndFeel.NativeStyle and FThemeAvailable then
      Result := stpsXP
    else
      Result := AStyles[LookAndFeel.Kind];
  end
  else
    Result := PaintStyle;
end;

class function TdxCustomStatusBar.GetStatusPanelClass: TdxStatusBarPanelClass;
begin
  Result := TdxStatusBarPanel;
end;

class function TdxCustomStatusBar.GetStatusPanelsClass: TdxStatusBarPanelsClass;
begin
  Result := TdxStatusBarPanels;
end;

procedure TdxCustomStatusBar.InitPainterClass;
begin
  FThemeAvailable := AreVisualStylesAvailable([totStatus]);
  FPainter := GetPainterClass;
end;

procedure TdxCustomStatusBar.PaintStyleChanged;
begin
  InitPainterClass;
  Panels.UpdateResources;
  UpdatePanels;
  InvalidateWithChildren;
end;

procedure TdxCustomStatusBar.Recalculate;
begin
  if Assigned(Panels) and not (IsDestroying or (sbsCalculating in FState)) then
  begin
    Include(FState, sbsCalculating);
    try
      Calculate;
      if UseRightToLeftAlignment then
        RightToLeftConversion;
    finally
      Exclude(FState, sbsCalculating);
    end;
  end;
end;

procedure TdxCustomStatusBar.RightToLeftConversion;
begin
  if not FIsRightToLeftConverted then
  begin
    DoRightToLeftConversion(GetPanelsBounds);
    FIsRightToLeftConverted := True;
  end;
end;

function TdxCustomStatusBar.SizeGripAllocated: Boolean;
begin
  Result := FSizeGrip and TcxSizeGrip.IsAvailable(Self);
end;

procedure TdxCustomStatusBar.ThemeChanged;
begin
  PaintStyleChanged;
end;

procedure TdxCustomStatusBar.UpdatePanels;
begin
  Recalculate;
  CheckAutoSize;
  Invalidate;
end;

function TdxCustomStatusBar.GetPanelsBounds: TRect;
begin
  Result := cxRectContent(ClientBounds, GetPanelsOffsets);
end;

function TdxCustomStatusBar.GetPanelsOffsets: TRect;
begin
  Result := Painter.ContentExtents(Self);
end;

procedure TdxCustomStatusBar.ImageListChange(Sender: TObject);
begin
  UpdatePanels;
end;

procedure TdxCustomStatusBar.SetBorderWidth(Value: Integer);
begin
  if FBorderWidth <> Value then
  begin
    if Value <= 0 then
      FBorderWidth := 0
    else
      FBorderWidth := Value;
    UpdatePanels;
  end;
end;

procedure TdxCustomStatusBar.SetImages(const Value: TCustomImageList);
begin
  if Images <> nil then
    Images.UnRegisterChanges(FImageChangeLink);
  FImages := Value;
  if Images <> nil then
  begin
    Images.RegisterChanges(FImageChangeLink);
    Images.FreeNotification(Self);
  end;
  UpdatePanels;
end;

procedure TdxCustomStatusBar.SetPanels(Value: TdxStatusBarPanels);
begin
  FPanels.Assign(Value);
end;

procedure TdxCustomStatusBar.SetPaintStyle(Value: TdxStatusBarPaintStyle);
begin
  if FPaintStyle <> Value then
  begin
    FPaintStyle := Value;
    PaintStyleChanged;
  end;
end;

procedure TdxCustomStatusBar.SetSimplePanelStyle(Value: TdxStatusBarSimplePanelStyle);
begin
  FSimplePanelStyle.Assign(Value);
end;

procedure TdxCustomStatusBar.SetSizeGrip(Value: Boolean);
begin
  if FSizeGrip <> Value then
  begin
    FSizeGrip := Value;
    UpdatePanels;
  end;
end;

procedure TdxCustomStatusBar.CMColorChanged(var Message: TMessage);
begin
  inherited;
  UpdatePanels;
end;

procedure TdxCustomStatusBar.CMParentFontChanged(var Message: TMessage);
begin
  inherited;
  UpdatePanels;
end;

procedure TdxCustomStatusBar.CMSysColorChange(var Message: TMessage);
begin
  inherited;
  RefreshOffice11Colors;
  UpdatePanels;
end;

procedure TdxCustomStatusBar.CMWinIniChange(var Message: TMessage);
begin
  inherited;
  UpdatePanels;
end;

procedure TdxCustomStatusBar.DoRightToLeftConversion(const ABounds: TRect);
begin
  ViewInfo.RightToLeftConversion(ABounds);
end;

procedure TdxCustomStatusBar.WMNCHitTest(var Message: TWMNCHitTest);
var
  P: TPoint;
  R: TRect;
  ABounds: TRect;
begin
  inherited;
  if not (csDesigning in ComponentState) and SizeGripAllocated then
    with TWMNCHitTest(Message) do
    begin
      P := ScreenToClient(Point(XPos, YPos));
      ABounds := ClientBounds;
      R := Rect(ABounds.Right - Painter.GripAreaSize(Self).cx - 2,
        ABounds.Bottom - Painter.GripAreaSize(Self).cy - 2, ABounds.Right, ABounds.Bottom);
      if UseRightToLeftAlignment then
        R := TdxRightToLeftLayoutConverter.ConvertRect(R, ABounds);
      if PtInRect(R, P) then
        if UseRightToLeftAlignment then
          Result := HTBOTTOMLEFT
        else
          Result := HTBOTTOMRIGHT;
    end;
end;

{ TdxStatusBarPainter }

class procedure TdxStatusBarPainter.AdjustTextColor(AStatusBar: TdxCustomStatusBar; var AColor: TColor; Active: Boolean);
begin
end;

class function TdxStatusBarPainter.ContentExtents(AStatusBar: TdxCustomStatusBar): TRect;
begin
  Result := Rect(AStatusBar.BorderWidth, AStatusBar.BorderWidth + TopBorderSize, AStatusBar.BorderWidth, AStatusBar.BorderWidth);
end;

class function TdxStatusBarPainter.GripAreaSize(AStatusBar: TdxCustomStatusBar): TSize;
begin
  Result := AStatusBar.ScaleFactor.Apply(cxSize(16));
end;

class function TdxStatusBarPainter.GripSize(AStatusBar: TdxCustomStatusBar): TSize;
begin
  Result := AStatusBar.ScaleFactor.Apply(cxSize(12));
end;

class function TdxStatusBarPainter.IsNativeBackground: Boolean;
begin
  Result := False;
end;

class function TdxStatusBarPainter.IsSizeGripInPanelArea(AStatusBar: TdxCustomStatusBar): Boolean;
begin
  Result := True;
end;

class function TdxStatusBarPainter.PanelBorderSizes(ABevel: TdxStatusBarPanelBevel): TRect;
begin
  Result := cxNullRect;
end;

class function TdxStatusBarPainter.PanelContentExtents(AStatusBar: TdxCustomStatusBar; AHasBorders: Boolean): TRect;
begin
  if AHasBorders then
    Result := AStatusBar.ScaleFactor.Apply(Rect(1, 0, 1, 0))
  else
    Result := cxNullRect;
end;

class function TdxStatusBarPainter.SeparatorSize(AStatusBar: TdxCustomStatusBar): Integer;
begin
  Result := 2;
end;

class function TdxStatusBarPainter.SizeGripMargins(AStatusBar: TdxCustomStatusBar): TRect;
begin
  Result := AStatusBar.ScaleFactor.Apply(Rect(1, 1, 1, 1));
end;

class function TdxStatusBarPainter.TopBorderSize: Integer;
begin
  Result := 2;
end;

class procedure TdxStatusBarPainter.DrawBorder(AStatusBar: TdxCustomStatusBar; ACanvas: TcxCanvas; var R: TRect);
begin
  // top border
  DrawTopBorder(AStatusBar, ACanvas, Rect(R.Left, R.Top, R.Right, R.Top + TopBorderSize));
  Inc(R.Top, TopBorderSize);
  // border
  if AStatusBar.BorderWidth > 0 then
  begin
    ACanvas.Brush.Color := AStatusBar.Color;
    ACanvas.FillRect(Rect(R.Left, R.Top, R.Right, R.Top + AStatusBar.BorderWidth));
    Inc(R.Top, AStatusBar.BorderWidth);
    ACanvas.FillRect(Rect(R.Left, R.Bottom - AStatusBar.BorderWidth, R.Right, R.Bottom));
    Dec(R.Bottom, AStatusBar.BorderWidth);
    ACanvas.FillRect(Rect(R.Left, R.Top, R.Left + AStatusBar.BorderWidth, R.Bottom));
    Inc(R.Left, AStatusBar.BorderWidth);
    ACanvas.FillRect(Rect(R.Right - AStatusBar.BorderWidth, R.Top, R.Right, R.Bottom));
    Dec(R.Right, AStatusBar.BorderWidth);
  end;
end;

class procedure TdxStatusBarPainter.DrawEmptyPanel(AStatusBar: TdxCustomStatusBar; ACanvas: TcxCanvas; R: TRect);
begin
  DrawPanelBorder(AStatusBar, dxpbLowered, ACanvas, R);
  FillBackground(AStatusBar, nil, ACanvas, R);
end;

class procedure TdxStatusBarPainter.DrawPanel(
  AStatusBar: TdxCustomStatusBar; APanel: TdxStatusBarPanel; ACanvas: TcxCanvas; R: TRect);

  function DoCustomDrawPanel(ACanvas: TcxCanvas; R: TRect): Boolean;
  begin
    Result := False;
    if Assigned(APanel.OnDrawPanel) then
      APanel.OnDrawPanel(APanel, ACanvas, R, Result);
  end;

begin
  if APanel = nil then
    DrawEmptyPanel(AStatusBar, ACanvas, R)
  else
    if not APanel.Visible then
      FillBackground(AStatusBar, nil, ACanvas, R)
    else
    begin
      ACanvas.SaveState;
      try
        DrawPanelBorder(AStatusBar, GetPanelBevel(APanel), ACanvas, R);

        if not DoCustomDrawPanel(ACanvas, R) then
        begin
          if APanel.PanelStyle <> nil then
          begin
            ACanvas.ExcludeClipRect(AStatusBar.ViewInfo.SizeGripAreaRect);
            APanel.PanelStyle.DrawContent(ACanvas, R, Self)
          end
          else
            FillBackground(AStatusBar, APanel, ACanvas, R);
        end;
      finally
        ACanvas.RestoreState;
      end;
    end;
end;

class procedure TdxStatusBarPainter.DrawPanelBorder(AStatusBar: TdxCustomStatusBar;
  ABevel: TdxStatusBarPanelBevel; ACanvas: TcxCanvas; var R: TRect);
begin
end;

class procedure TdxStatusBarPainter.DrawPanelSeparator(AStatusBar: TdxCustomStatusBar;
  ACanvas: TcxCanvas; const R: TRect);
begin
end;

class procedure TdxStatusBarPainter.DrawSimplePanel(
  AStatusBar: TdxCustomStatusBar; const AText: string; ACanvas: TcxCanvas; R: TRect);

  procedure DrawSimplePanelText(ACanvas: TcxCanvas; R: TRect; const AText: string);
  var
    ATextColor: TColor;
    AFlags: Cardinal;
  begin
    ATextColor := AStatusBar.Font.Color;
    AdjustTextColor(AStatusBar, ATextColor, True);
    if ATextColor = clDefault then
      AStatusBar.AdjustTextColor(ATextColor, True);
    R := cxRectInflate(R, -AStatusBar.ScaleFactor.Apply(cxTextOffset), 0);

    ACanvas.Brush.Style := bsClear;
    ACanvas.Font := AStatusBar.Font;
    ACanvas.Font.Color := ATextColor;
    AFlags := cxSingleLine or cxAlignVCenter;
    if AStatusBar.UseRightToLeftAlignment then
      AFlags := AFlags or cxAlignRight
    else
      AFlags := AFlags or cxAlignLeft;
    ACanvas.DrawText(AText, R, AFlags, AStatusBar.Enabled);
  end;

begin
  DrawPanelBorder(AStatusBar, dxpbLowered, ACanvas, R);

  if AText <> '' then
  begin
    ACanvas.SaveState;
    try
      ACanvas.ExcludeClipRect(AStatusBar.ViewInfo.SizeGripAreaRect);
      DrawSimplePanelText(ACanvas, R, AText);
    finally
      ACanvas.RestoreState;
    end;
  end;
end;

class procedure TdxStatusBarPainter.DrawSizeGrip(AStatusBar: TdxCustomStatusBar; ACanvas: TcxCanvas; R: TRect);
const
  ACorner: array [Boolean] of TdxCorner = (coBottomRight, coBottomLeft);
begin
  dxRotateSizeGrip(ACanvas, R, GetSizeGripBackgroundColor(AStatusBar), ACorner[AStatusBar.UseRightToLeftAlignment], DoDrawSizeGrip);
end;

class procedure TdxStatusBarPainter.DrawTopBorder(AStatusBar: TdxCustomStatusBar; ACanvas: TcxCanvas; const R: TRect);
begin
  ACanvas.Brush.Color := AStatusBar.Color;
  ACanvas.FillRect(R);
end;

class procedure TdxStatusBarPainter.FillBackground(AStatusBar: TdxCustomStatusBar;
  APanel: TdxStatusBarPanel; ACanvas: TcxCanvas; const R: TRect);
begin
end;

class function TdxStatusBarPainter.GetPanelBevel(APanel: TdxStatusBarPanel): TdxStatusBarPanelBevel;
begin
  if APanel <> nil then
  begin
    if (APanel.PanelStyle <> nil) and APanel.PanelStyle.InternalBevel then
      Result := dxpbNone
    else
      Result := APanel.Bevel;
  end
  else
    Result := dxpbLowered;
end;

class function TdxStatusBarPainter.GetPanelColor(AStatusBar: TdxCustomStatusBar; APanel: TdxStatusBarPanel): TColor;
begin
  if (APanel <> nil) and (APanel.PanelStyle <> nil) then
    Result := APanel.PanelStyle.Color
  else
    Result := AStatusBar.Color;
end;

class function TdxStatusBarPainter.ValidatePanelTextRect(
  AStatusBar: TdxCustomStatusBar; APanel: TdxStatusBarPanel; const R: TRect): TRect;
begin
  Result := R;
end;

class function TdxStatusBarPainter.GetSizeGripBackgroundColor(AStatusBar: TdxCustomStatusBar): TColor;
begin
  Result := AStatusBar.Color;
end;

class procedure TdxStatusBarPainter.DoDrawSizeGrip(ACanvas: TcxCanvas; const ARect: TRect);
begin
  cxLookAndFeelPaintersManager.GetPainter(lfsStandard).DoDrawSizeGrip(ACanvas, ARect);
end;

class procedure TdxStatusBarPainter.DrawContainerControl(APanelStyle: TdxStatusBarContainerPanelStyle);
var
  AParentRect: TRect;
  AControl: TdxStatusBarContainerControl;
begin
  AControl := APanelStyle.Container;
  AParentRect := APanelStyle.StatusBarControl.ClientBounds;
  OffsetRect(AParentRect, -AControl.Left, -AControl.Top);
  DrawBorder(APanelStyle.StatusBarControl, AControl.Canvas, AParentRect);
  FillBackground(APanelStyle.StatusBarControl, APanelStyle.Owner, AControl.Canvas, AControl.ClientRect);
end;

class procedure TdxStatusBarStandardPainter.DrawPanelBorder(AStatusBar: TdxCustomStatusBar;
  ABevel: TdxStatusBarPanelBevel; ACanvas: TcxCanvas; var R: TRect);
const
  ABorders: array [TdxStatusBarPanelBevel] of Integer = (0, BDR_SUNKENOUTER, BDR_RAISEDINNER);
begin
  DrawEdge(ACanvas.Handle, R, ABorders[ABevel], BF_RECT or BF_ADJUST);
end;

class procedure TdxStatusBarStandardPainter.DrawPanelSeparator(AStatusBar: TdxCustomStatusBar;
  ACanvas: TcxCanvas; const R: TRect);
begin
  ACanvas.Brush.Color := AStatusBar.Color;
  ACanvas.FillRect(R);
end;

class procedure TdxStatusBarStandardPainter.FillBackground(AStatusBar: TdxCustomStatusBar;
  APanel: TdxStatusBarPanel; ACanvas: TcxCanvas; const R: TRect);
begin
  ACanvas.FillRect(R, GetPanelColor(AStatusBar, APanel));
end;

class function TdxStatusBarStandardPainter.PanelBorderSizes(ABevel: TdxStatusBarPanelBevel): TRect;
begin
  if ABevel = dxpbNone then
    Result := cxNullRect
  else
    Result := cxRect(2, 2, 2, 2);
end;

{ TdxStatusBarFlatPainter }

class procedure TdxStatusBarFlatPainter.DrawPanelBorder(
  AStatusBar: TdxCustomStatusBar; ABevel: TdxStatusBarPanelBevel; ACanvas: TcxCanvas; var R: TRect);
begin
  if ABevel <> dxpbNone then
  begin
    ACanvas.Brush.Color := clBtnShadow;
    ACanvas.FrameRect(R);
  end;
  R := cxRectContent(R, PanelBorderSizes(ABevel));
end;

class function TdxStatusBarFlatPainter.PanelBorderSizes(ABevel: TdxStatusBarPanelBevel): TRect;
begin
  if ABevel = dxpbNone then
    Result := cxNullRect
  else
    Result := cxRect(1, 1, 1, 1);
end;

{ TdxStatusBarOffice11Painter }

class function TdxStatusBarOffice11Painter.ContentExtents(AStatusBar: TdxCustomStatusBar): TRect;
begin
  Result := AStatusBar.ScaleFactor.Apply(Rect(1, 1, 1, 1));
end;

class procedure TdxStatusBarOffice11Painter.DoDrawSizeGrip(ACanvas: TcxCanvas; const ARect: TRect);
begin
  Office11DrawSizeGrip(ACanvas.Handle, ARect);
end;

class function TdxStatusBarOffice11Painter.GetSizeGripBackgroundColor(AStatusBar: TdxCustomStatusBar): TColor;
begin
  Result := clNone;
end;

class procedure TdxStatusBarOffice11Painter.DrawBorder(
  AStatusBar: TdxCustomStatusBar; ACanvas: TcxCanvas; var R: TRect);
begin
  Office11FillTubeGradientRect(ACanvas.Handle, R, dxOffice11ToolbarsColor1, dxOffice11ToolbarsColor2, False);
  R := cxRectInflate(R, -1);
end;

class procedure TdxStatusBarOffice11Painter.DrawPanelBorder(
  AStatusBar: TdxCustomStatusBar; ABevel: TdxStatusBarPanelBevel; ACanvas: TcxCanvas; var R: TRect);
begin
  if ABevel <> dxpbNone then
  begin
    ACanvas.Brush.Color := dxGetMiddleRGB(dxOffice11ToolbarsColor2, clBlack{clBtnShadow}, 90);
    ACanvas.FrameRect(R);
  end;
  R := cxRectContent(R, PanelBorderSizes(ABevel));
end;

class procedure TdxStatusBarOffice11Painter.DrawPanelSeparator(
  AStatusBar: TdxCustomStatusBar; ACanvas: TcxCanvas; const R: TRect);
begin
  // do nothing
end;

class function TdxStatusBarOffice11Painter.PanelBorderSizes(ABevel: TdxStatusBarPanelBevel): TRect;
begin
  if ABevel = dxpbNone then
    Result := cxNullRect
  else
    Result := cxRect(1, 1, 1, 1);
end;

class function TdxStatusBarOffice11Painter.SizeGripMargins(AStatusBar: TdxCustomStatusBar): TRect;
begin
  Result := AStatusBar.ScaleFactor.Apply(Rect(0, 1, 0, 1));
end;

{ TdxStatusBarXPPainter }

class function TdxStatusBarXPPainter.ContentExtents(AStatusBar: TdxCustomStatusBar): TRect;
begin
  Result := AStatusBar.ScaleFactor.Apply(Rect(0, TopBorderSize, 0, 0));
end;

class procedure TdxStatusBarXPPainter.DoDrawSizeGrip(ACanvas: TcxCanvas; const ARect: TRect);
begin
  DrawThemeBackground(OpenTheme(totStatus), ACanvas.Handle, SP_GRIPPER, 0, @ARect);
end;

class function TdxStatusBarXPPainter.GripAreaSize(AStatusBar: TdxCustomStatusBar): TSize;
begin
  GetThemePartSize(OpenTheme(totStatus), cxScreenCanvas.Handle, SP_GRIPPER, 0, nil, TS_TRUE, @Result);
  cxScreenCanvas.Dormant;
  Result := AStatusBar.ScaleFactor.Apply(Result, dxSystemScaleFactor);
end;

class function TdxStatusBarXPPainter.SeparatorSize(AStatusBar: TdxCustomStatusBar): Integer;
var
  ASize: TSize;
begin
  GetThemePartSize(OpenTheme(totStatus), cxScreenCanvas.Handle, SP_PANE, 0, nil, TS_TRUE, @ASize);
  cxScreenCanvas.Dormant;
  Result := ASize.cx;
end;

class function TdxStatusBarXPPainter.SizeGripMargins(AStatusBar: TdxCustomStatusBar): TRect;
begin
  Result := AStatusBar.ScaleFactor.Apply(Rect(1, 0, 1, 0));
end;

class procedure TdxStatusBarXPPainter.DrawBorder(AStatusBar: TdxCustomStatusBar; ACanvas: TcxCanvas; var R: TRect);
begin
  DrawThemeBackground(OpenTheme(totStatus), ACanvas.Handle, 0, 0, @R);
  Inc(R.Top, TopBorderSize);
end;

class procedure TdxStatusBarXPPainter.DrawPanelSeparator(AStatusBar: TdxCustomStatusBar; ACanvas: TcxCanvas; const R: TRect);
begin
  DrawThemeBackground(OpenTheme(totStatus), ACanvas.Handle, SP_PANE, 0, @R);
end;

initialization
  GetRegisteredStatusBarPanelStyles.Register(TdxStatusBarTextPanelStyle, cxSTextPanelStyle);
  GetRegisteredStatusBarPanelStyles.Register(TdxStatusBarContainerPanelStyle, cxSContainerPanelStyle);
  GetRegisteredStatusBarPanelStyles.Register(TdxStatusBarKeyboardStatePanelStyle, cxSKeyboardStatePanelStyle);
  GetRegisteredStatusBarPanelStyles.Register(TdxStatusBarStateIndicatorPanelStyle, cxSStateIndicatorPanelStyle);
  RegisterClasses([TdxStatusBarContainerControl]);

finalization
  GetRegisteredStatusBarPanelStyles.Unregister(TdxStatusBarTextPanelStyle);
  GetRegisteredStatusBarPanelStyles.Unregister(TdxStatusBarContainerPanelStyle);
  GetRegisteredStatusBarPanelStyles.Unregister(TdxStatusBarKeyboardStatePanelStyle);
  GetRegisteredStatusBarPanelStyles.Unregister(TdxStatusBarStateIndicatorPanelStyle);
  FreeAndNil(FRegisteredStatusBarPanelStyles);

end.
