{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressEditors                                           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSEDITORS AND ALL                }
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

unit dxUIAdorners;

{$I cxVer.inc}

interface

uses
  Windows, Classes, Controls, Messages, Graphics, ComCtrls, cxClasses, cxControls, cxGraphics, dxGDIPlusClasses, cxGeometry,
  dxCoreClasses, dxCoreGraphics, dxCalloutPopup, cxEdit, ImgList;

const
  dxAdornerTargetElementPathSeparator = '.';
  dxBadgeTextDefaultMargin = 2;
  dxBadgeDefaultHorz = taRightJustify;
  dxGuideNormalBorderWidth = 1;
  dxGuideSelectedBorderWidth = 2;
  dxGuidesDefaultDeactivateKey = 'Esc';

type
  TdxCustomAdornerViewInfo = class;
  TdxBadgeViewInfo = class;
  TdxGuideViewInfo = class;
  TdxGuideLayerViewInfo = class;
  TdxCustomAdorner = class;
  TdxBadge = class;
  TdxGuide = class;
  TdxCustomAdorners = class;
  TdxBadges = class;
  TdxGuides = class;
  TdxUIAdornerManager = class;

  TdxAdornerCustomDrawEvent = procedure(AManager: TdxUIAdornerManager; AAdorner: TdxCustomAdorner; ACanvas: TdxGPCanvas;
    AViewInfo: TdxCustomAdornerViewInfo; var ADone: Boolean) of object;
  TdxAdornerNotifyEvent = procedure(AManager: TdxUIAdornerManager; AAdorner: TdxCustomAdorner) of object;
  TdxAdornersNotifyEvent = procedure(AManager: TdxUIAdornerManager; AAdorners: TdxCustomAdorners) of object;
  TdxGuideGetCalloutPopupControl = procedure(AManager: TdxUIAdornerManager; AGuide: TdxGuide;
    {$IFDEF BCBCOMPATIBLE}var{$ELSE}out{$ENDIF} AControl: TWinControl) of object;

  TdxAdornerState = (astNone, astHotTracked, astPressed);


  { TdxUIAdornerManagerCustomDesignHelper }

  TdxUIAdornerManagerCustomDesignHelper = class(TcxInterfacedPersistent,
    IcxDesignSelectionChanged)
  private
    function GetManager: TdxUIAdornerManager;
  protected
    // IcxDesignSelectionChanged
    procedure DesignSelectionChanged(ASelection: TList);

    function IsObjectSelected(AObject: TPersistent): Boolean; virtual; abstract;
    procedure SelectObject(AObject: TPersistent; AClearSelection: Boolean); virtual; abstract;

    property Manager: TdxUIAdornerManager read GetManager;
  end;
  TdxUIAdornerManagerCustomDesignHelperClass = class of TdxUIAdornerManagerCustomDesignHelper;


  { TdxCustomAdornerOptions }

  TdxCustomAdornerOptions = class(TcxOwnedPersistent)
  strict private
    function GetAdorner: TdxCustomAdorner;
  protected
    procedure Changed; virtual;
  public
    property Adorner: TdxCustomAdorner read GetAdorner;
  end;


  TdxBadgeBackground = class(TdxCustomAdornerOptions)
  private
    FColor: TColor;
    FFitMode: TcxImageFitMode;
    FGlyph: TdxSmartGlyph;
    FImageIndex: TcxImageIndex;
    FTransparent: Boolean;

    function GetActualColor: TColor;
    function GetAdorner: TdxBadge;
    procedure SetColor(AValue: TColor);
    procedure SetFitMode(AValue: TcxImageFitMode);
    procedure SetGlyph(AValue: TdxSmartGlyph);
    procedure SetImageIndex(AValue: TcxImageIndex);
    procedure SetTransparent(AValue: Boolean);
    procedure ChangeHandler(ASender: TObject);
  protected
    function CreateGlyph: TdxSmartGlyph; virtual;

    property ActualColor: TColor read GetActualColor;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    property Adorner: TdxBadge read GetAdorner;
  published
    property Color: TColor read FColor write SetColor default clDefault;
    property FitMode: TcxImageFitMode read FFitMode write SetFitMode default ifmStretch;
    property Glyph: TdxSmartGlyph read FGlyph write SetGlyph;
    property ImageIndex: TcxImageIndex read FImageIndex write SetImageIndex default -1;
    property Transparent: Boolean read FTransparent write SetTransparent default False;
  end;


  { TdxAdornerCustomTargetElement }

  TdxAdornerCustomTargetElement = class(TdxCustomAdornerOptions)
  strict private
    FCache: Pointer;
    FUseCacheLockCount: Integer;

    function GetScreenBounds: TRect;
    function GetVisibleForControl: Boolean;
    function GetVisibleForParents: Boolean;
  protected
    function GetBounds: TRect; virtual; abstract;
    function GetCacheInstance: Pointer; virtual;
    function GetControl: TWinControl; virtual; abstract;
    function GetDisplayName: string; virtual; abstract;
    function GetVisible: Boolean; virtual;
    function HasControl: Boolean; virtual;
    function IsCoverChanged(AChangedHandle: HWND; ABounds, AOldBounds: TRect): Boolean;
    function IsFullyCovered: Boolean;
    function IsPositionChanged(AChangedHandle: HWND; AWasControlAvailable: Boolean): Boolean;
    procedure TargetControlFreed(ATargetControl: TControl); virtual;

    procedure BeginUseCache;
    procedure EndUseCache;
    function IsUsingCache: Boolean;

    property Bounds: TRect read GetBounds;
    property Cache: Pointer read FCache;
    property Control: TWinControl read GetControl;
    property ScreenBounds: TRect read GetScreenBounds;
    property Visible: Boolean read GetVisible;
    property VisibleForParents: Boolean read GetVisibleForParents;
    property VisibleForControl: Boolean read GetVisibleForControl;
  end;
  TdxAdornerCustomTargetElementClass = class of TdxAdornerCustomTargetElement;

  { TdxAdornerTargetElementControl }

  TdxAdornerTargetElementControl = class(TdxAdornerCustomTargetElement)
  strict private
    FControl: TWinControl;
    FFreeNotificator: TcxFreeNotificator;

    procedure SetControl(AValue: TWinControl);

    procedure ControlFreeNotificationHandler(Sender: TComponent);
  protected
    procedure AssignControlByElement(AElement: TdxAdornerTargetElementControl);
    function GetBounds: TRect; override;
    function GetControl: TWinControl; override;
    function GetDisplayName: string; override;
    procedure TargetControlFreed(ATargetControl: TControl); override;

    property FreeNotificator: TcxFreeNotificator read FFreeNotificator;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
  published
    property Control: TWinControl read FControl write SetControl;
  end;

  { TdxAdornerTargetElementPath }

  TdxAdornerTargetElementPath = class(TdxAdornerCustomTargetElement)
  strict private
    FPath: string;

    function GetCache: IdxAdornerTargetElement;
    function GetElement: IdxAdornerTargetElement;
    procedure SetPath(const AValue: string);
  protected
    function GetBounds: TRect; override;
    function GetCacheInstance: Pointer; override;
    function GetControl: TWinControl; override;
    function GetDisplayName: string; override;
    function GetVisible: Boolean; override;
    function HasControl: Boolean; override;

    property Cache: IdxAdornerTargetElement read GetCache;
    property Element: IdxAdornerTargetElement read GetElement;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Path: string read FPath write SetPath;
  end;


  { TdxUIAdornerManagerTargetElementTree }

  TdxUIAdornerManagerTargetElementTree = class(TcxIUnknownObject,
    IdxAdornerTargetElementCollection)
  strict private
    FManager: TdxUIAdornerManager;

    class function DoFindElement(const APath: string; AElements: TStrings): IdxAdornerTargetElement; overload;
    class function DoFindElement(const APath, AParentPath: string; AElements: TStrings): IdxAdornerTargetElement; overload;
    class function FindElement(const APath: string; AElementCollection: IdxAdornerTargetElementCollection): IdxAdornerTargetElement; overload;
    class function FindElement(const APath: string; AElements: TStrings): IdxAdornerTargetElement; overload;
    class function PopulateElement(ANodes: TTreeNodes; AParentNode: TTreeNode; AElement: TObject; AElementName: string): Boolean;
    class function PopulateElements(ANodes: TTreeNodes; AParentNode: TTreeNode; AElements: TStrings): Boolean; overload;
    class function PopulateElements(ANodes: TTreeNodes; AParentNode: TTreeNode; AElementCollection: IdxAdornerTargetElementCollection): Boolean; overload;

    // IdxAdornerTargetElementCollection
    procedure GetElements(AList: TStrings);
  protected
    procedure Build(ANodes: TTreeNodes); overload;
    class function CreateInstance(AManager: TdxUIAdornerManager): TdxUIAdornerManagerTargetElementTree; virtual;
    function FindElement(const APath: string): IdxAdornerTargetElement; overload;

    property Manager: TdxUIAdornerManager read FManager;
  public
    constructor Create(AManager: TdxUIAdornerManager); virtual;

    class procedure Build(AManager: TdxUIAdornerManager; ANodes: TTreeNodes); overload;
    class function FindElement(AManager: TdxUIAdornerManager; const APath: string): IdxAdornerTargetElement; overload;
  end;


  { TdxUIAdornerManagerCustomLayeredWindow }

  TdxUIAdornerManagerCustomLayeredWindow = class(TcxCustomControl)
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    function GetBounds: TRect; virtual; abstract;
    function GetParentWnd: HWND; virtual; abstract;
    function GetVisible: Boolean; virtual; abstract;
    procedure PaintLayer(ACanvas: TcxCanvas); overload;
    procedure PaintLayer(AGPGraphics: TdxGPGraphics); overload; virtual;
    procedure PaintLayer; overload;
    procedure UpdateParent(AHandle: HWND);

    property Bounds: TRect read GetBounds;
    property ParentWnd: HWND read GetParentWnd;
  public
    procedure Update; override;

    property Visible: Boolean read GetVisible;
  end;

  { TdxBadgeLayeredWindow }

  TdxBadgeLayeredWindow = class(TdxUIAdornerManagerCustomLayeredWindow)
  strict private
    FBadge: TdxBadge;

    function GetViewInfo: TdxBadgeViewInfo;
    procedure WMMouseActivate(var Message: TWMMouseActivate); message WM_MOUSEACTIVATE;
  protected
    procedure Click; override;
    function GetBounds: TRect; override;
    function GetParentWnd: HWND; override;
    function GetVisible: Boolean; override;
    procedure PaintLayer(AGPGraphics: TdxGPGraphics); override;

    property Badge: TdxBadge read FBadge;
    property ViewInfo: TdxBadgeViewInfo read GetViewInfo;
  public
    constructor Create(ABadge: TdxBadge); reintroduce; virtual;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
  end;

  { TdxGuideLayeredWindow }

  TdxGuideLayeredWindow = class(TdxUIAdornerManagerCustomLayeredWindow)
  strict private
    FGuides: TdxGuides;
    FOwnerControlWndProcObject: TcxWindowProcLinkedObject;

    function GetOwnerControl: TWinControl;
    function GetViewInfo: TdxGuideLayerViewInfo;
    procedure WMActivate(var Message: TWMActivate); message WM_ACTIVATE;
    procedure WMClose(var Message: TMessage); message WM_CLOSE;
    procedure WMWindowPosChanged(var Message: TWMWindowPosChanged); message WM_WINDOWPOSCHANGED;
    procedure OwnerWindowWndProc(var AMessage: TMessage);
  protected
    function GetBounds: TRect; override;
    function GetParentWnd: HWND; override;
    function GetVisible: Boolean; override;
    procedure PaintLayer(AGPGraphics: TdxGPGraphics); override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;

    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;

    property Guides: TdxGuides read FGuides;
    property OwnerControl: TWinControl read GetOwnerControl;
    property OwnerControlWndProcObject: TcxWindowProcLinkedObject read FOwnerControlWndProcObject;
    property ViewInfo: TdxGuideLayerViewInfo read GetViewInfo;
  public
    constructor Create(AGuides: TdxGuides); reintroduce; virtual;
    destructor Destroy; override;
  end;


  { TdxAdornerCustomHitTest }

  TdxAdornerCustomHitTest = class
  strict private
    FPos: TPoint;
  protected
    class function GetHitTestCode: Integer; virtual;
    procedure Init(const APos: TPoint);
  public
    class function HitTestCode: Integer;
    class function Instance(const APos: TPoint): TdxAdornerCustomHitTest;

    property Pos: TPoint read FPos;
  end;
  TdxAdornerCustomHitTestClass = class of TdxAdornerCustomHitTest;

  { TdxAdornerNoneHitTest }

  TdxAdornerNoneHitTest = class(TdxAdornerCustomHitTest)
  protected
    class function GetHitTestCode: Integer; override;
  end;

  { TdxGuideHitTest }

  TdxGuideHitTest = class(TdxAdornerCustomHitTest)
  strict private
    FViewInfo: TdxGuideViewInfo;
  protected
    class function GetHitTestCode: Integer; override;

    property ViewInfo: TdxGuideViewInfo read FViewInfo write FViewInfo;
  end;


  { TdxCustomAdornerPainter }

  TdxCustomAdornerPainter = class
  strict private
    FCanvas: TdxGPCanvas;
    FViewInfo: TdxCustomAdornerViewInfo;
  protected
    procedure Draw; virtual;
    procedure DrawBackground; virtual;
    procedure DrawContent; virtual;

    property Canvas: TdxGPCanvas read FCanvas;
    property ViewInfo: TdxCustomAdornerViewInfo read FViewInfo;
  public
    constructor Create(AViewInfo: TdxCustomAdornerViewInfo; ACanvas: TdxGPCanvas); virtual;

    procedure Paint;
  end;

  { TdxBadgePainter }

  TdxBadgePainter = class(TdxCustomAdornerPainter)
  strict private
    function GetViewInfo: TdxBadgeViewInfo;
  protected
    procedure Draw; override;
    procedure DrawBackground; override;
    procedure DrawContent; override;
    procedure DrawDefaultGlyph; virtual;
    procedure DrawDesignSelector; virtual;
    procedure DrawGlassText; virtual;
    procedure DrawGlyph; virtual;
    procedure DrawSimpleText; virtual;
    procedure DrawText; virtual;

    property ViewInfo: TdxBadgeViewInfo read GetViewInfo;
  end;

  { TdxGuidePainter }

  TdxGuidePainter = class(TdxCustomAdornerPainter)
  strict private
    function GetViewInfo: TdxGuideViewInfo;
  protected
    procedure Draw; override;
    procedure DrawBorders; virtual;

    property ViewInfo: TdxGuideViewInfo read GetViewInfo;
  end;

  { TdxGuideLayerPainter }

  TdxGuideLayerPainter = class
  strict private
    FCanvas: TdxGPCanvas;
    FViewInfo: TdxGuideLayerViewInfo;
  protected
    procedure Draw; virtual;
    procedure DrawBackground; virtual;
    procedure DrawContent; virtual;
    procedure DrawGuides; virtual;
    procedure DrawSubstrate; virtual;
    procedure ExcludeBounds; virtual;

    property Canvas: TdxGPCanvas read FCanvas;
    property ViewInfo: TdxGuideLayerViewInfo read FViewInfo;
  public
    constructor Create(AViewInfo: TdxGuideLayerViewInfo; ACanvas: TdxGPCanvas); virtual;

    procedure Paint;
  end;


  { TdxCustomAdornerViewInfo }

  TdxCustomAdornerViewInfo = class(TcxOwnedInterfacedPersistent)
  strict private
    FBounds: TRect;
    FClipRect: TRect;

    function GetAdorner: TdxCustomAdorner;
    function GetIsDesignSelected: Boolean;
    function GetTargetElement: TdxAdornerCustomTargetElement;
    function GetManager: TdxUIAdornerManager;
  protected
    FVisible: Boolean;

    function CreatePainter(ACanvas: TdxGPCanvas): TdxCustomAdornerPainter; virtual; abstract;

    function CalculateBounds: TRect; virtual; abstract;
    function CalculateClipRect: TRect; virtual;
    procedure CalculateVisibility; virtual;
    function CanBeVisible: Boolean; virtual;
    procedure Click;

    procedure DoCalculate; virtual;
    procedure DoPaint(ACanvas: TdxGPCanvas); virtual;
    procedure MouseDown(X, Y: Integer; AButton: TMouseButton; AShift: TShiftState); virtual;
    procedure MouseMove(X, Y: Integer; AShift: TShiftState); virtual;
    procedure MouseUp(X, Y: Integer; AButton: TMouseButton; AShift: TShiftState); virtual;

    function CustomDraw(ACanvas: TdxGPCanvas): Boolean; virtual;
    function CustomDrawBackground(ACanvas: TdxGPCanvas): Boolean; virtual;

    property ClipRect: TRect read FClipRect;
    property IsDesignSelected: Boolean read GetIsDesignSelected;
    property TargetElement: TdxAdornerCustomTargetElement read GetTargetElement;
    property Visible: Boolean read FVisible;
  public
    procedure Calculate;
    procedure Paint(ACanvas: TdxGPCanvas);

    property Adorner: TdxCustomAdorner read GetAdorner;
    property Bounds: TRect read FBounds;
    property Manager: TdxUIAdornerManager read GetManager;
  end;

  { TdxBadgeViewInfo }

  TdxBadgeViewInfo = class(TdxCustomAdornerViewInfo)
  strict private
    FColor: TColor;
    FDefaultGlyphBounds: TRect;
    FDesignSelectorBounds: TRect;
    FHeight: Integer;
    FLayerBounds: TRect;
    FLayerClientRect: TRect;
    FLayerParent: TWinControl;
    FLayerSize: TSize;
    FTextAreaBounds: TRect;
    FTextSize: TSize;
    FWidth: Integer;

    function GetAdorner: TdxBadge;
    function GetColor: TColor;
    function GetDefaultGlyph: TdxSmartGlyph;
    function GetDefaultGlyphMargin: TRect;
    function GetFitMode: TcxImageFitMode;
    function GetFont: TFont;
    function GetGlyph: TdxSmartGlyph;
    function GetHeight: Integer;
    function GetImageIndex: TcxImageIndex;
    function GetImages: TCustomImageList;
    function GetLayerParent: TWinControl;
    function GetLayerSize: TSize;
    function GetOffset: TPoint;
    function GetScaleFactor: TdxScaleFactor;
    function GetText: string;
    function GetTextFlags: Cardinal;
    function GetTextSize: TSize;
    function GetTransparent: Boolean;
    function GetVisibleForParents: Boolean;
    function GetWidth: Integer;
  protected
    function CreatePainter(ACanvas: TdxGPCanvas): TdxCustomAdornerPainter; override;

    function CalculateBounds: TRect; override;
    function CalculateClipRect: TRect; override;
    function CalculateDefaultGlyphBounds: TRect; virtual;
    function CalculateDesignSelectorBounds: TRect; virtual;
    procedure CalculateLayer; virtual;
    function CanBeVisible: Boolean; override;
    function CalculateLayerBounds: TRect; virtual;
    function CalculateLayerClientRect: TRect; virtual;
    function CalculateTextAreaBounds: TRect; virtual;
    procedure CalculateVisibility; override;
    procedure DoCalculate; override;

    function HasGlyph: Boolean;
    function HasText: Boolean;
    function NeedColorizedGlyph: Boolean;

    property DefaultGlyph: TdxSmartGlyph read GetDefaultGlyph;
    property DefaultGlyphBounds: TRect read FDefaultGlyphBounds;
    property DefaultGlyphMargin: TRect read GetDefaultGlyphMargin;
    property DesignSelectorBounds: TRect read FDesignSelectorBounds;
    property FitMode: TcxImageFitMode read GetFitMode;
    property Font: TFont read GetFont;
    property Glyph: TdxSmartGlyph read GetGlyph;
    property Height: Integer read FHeight;
    property ImageIndex: TcxImageIndex read GetImageIndex;
    property Images: TCustomImageList read GetImages;
    property LayerBounds: TRect read FLayerBounds;
    property LayerClientRect: TRect read FLayerClientRect;
    property LayerParent: TWinControl read FLayerParent;
    property LayerSize: TSize read FLayerSize;
    property ScaleFactor: TdxScaleFactor read GetScaleFactor;
    property Transparent: Boolean read GetTransparent;
    property VisibleForParents: Boolean read GetVisibleForParents;
    property Offset: TPoint read GetOffset;
    property Text: string read GetText;
    property Width: Integer read FWidth;
  public
    property Adorner: TdxBadge read GetAdorner;
    property Color: TColor read FColor;
    property TextAreaBounds: TRect read FTextAreaBounds;
    property TextFlags: Cardinal read GetTextFlags;
    property TextSize: TSize read FTextSize;
  end;

  { TdxGuideViewInfo }

  TdxGuideViewInfo = class(TdxCustomAdornerViewInfo,
    IcxMouseTrackingCaller,
    IcxMouseTrackingCaller2)
  strict private
    FBorderColor: TColor;
    FBorderWidth: Integer;
    FExcludeBounds: TRect;
    FState: TdxAdornerState;

    function GetBorderGlyph: TdxSmartGlyph;
    function GetAdorner: TdxGuide;
    function GetBorderColor: TColor;
    function GetBorderWidth: Integer;
    function GetGuides: TdxGuides;
    function GetVisibleForHitTest: Boolean;
  protected
    // IcxMouseTrackingCaller
    procedure TrackingMouseLeave; virtual;
    procedure IcxMouseTrackingCaller.MouseLeave = TrackingMouseLeave;
    // IcxMouseTrackingCaller2
    function PtInCaller(const APoint: TPoint): Boolean; virtual;

    function CreateHitTest(const APoint: TPoint): TdxAdornerCustomHitTest; virtual;
    function CreatePainter(ACanvas: TdxGPCanvas): TdxCustomAdornerPainter; override;

    function CalculateBounds: TRect; override;
    function CalculateExcludeBounds: TRect; virtual;
    procedure DoCalculate; override;
    function DoGetHitTest(const APoint: TPoint): TdxAdornerCustomHitTest; virtual;
    function GetHitTest(const APoint: TPoint): TdxAdornerCustomHitTest; overload;
    function GetHitTest(X, Y: Integer): TdxAdornerCustomHitTest; overload;
    function NeedExcludeFromClipRect: Boolean;
    function HasPoint(const APoint: TPoint): Boolean;
    procedure InitHitTest(AHitTest: TdxGuideHitTest); virtual;
    function MapTargetScreenBoundsToLayerBounds: TRect;
    procedure MouseDown(X, Y: Integer; AButton: TMouseButton; AShift: TShiftState); override;
    procedure MouseLeave; virtual;
    procedure MouseMove(X, Y: Integer; AShift: TShiftState); override;
    procedure MouseUp(X, Y: Integer; AButton: TMouseButton; AShift: TShiftState); override;
    procedure SetState(AValue: TdxAdornerState);
    procedure StateChanged(APrevState: TdxAdornerState); virtual;

    property ExcludeBounds: TRect read FExcludeBounds;
    property Guides: TdxGuides read GetGuides;
    property VisibleForHitTest: Boolean read GetVisibleForHitTest;
  public
    property Adorner: TdxGuide read GetAdorner;
    property BorderColor: TColor read FBorderColor;
    property BorderGlyph: TdxSmartGlyph read GetBorderGlyph;
    property BorderWidth: Integer read FBorderWidth;
    property State: TdxAdornerState read FState;
  end;

  { TdxGuideLayerViewInfo }

  TdxGuideLayerViewInfo = class(TcxOwnedInterfacedPersistent)
  strict private
    FBounds: TRect;
    FClipRect: TRect;
    FItems: TdxFastObjectList;
    FLayerBounds: TRect;
    FLayerParent: HWND;
    FVisible: Boolean;

    function GetGuides: TdxGuides;
    function GetItem(AIndex: Integer): TdxGuideViewInfo;
    function GetItemCount: Integer;
    function GetLayerColor: TdxAlphaColor;
    function GetLayerParent: HWND;
    function GetManager: TdxUIAdornerManager;
  protected
    procedure CreateItems; virtual;
    function CreateHitTest(const APoint: TPoint): TdxAdornerCustomHitTest; virtual;
    function CreatePainter(ACanvas: TdxGPCanvas): TdxGuideLayerPainter; virtual;

    function CalculateBounds: TRect; virtual;
    function CalculateClipRect: TRect; virtual;
    procedure CalculateItems; virtual;
    function CalculateLayerBounds: TRect; virtual;
    procedure CalculateVisibility; virtual;
    procedure DoCalculate; virtual;
    function DoGetHitTest(const APoint: TPoint): TdxAdornerCustomHitTest; virtual;
    procedure DoPaint(ACanvas: TdxGPCanvas); virtual;
    function FindGuideViewInfo(AGuide: TdxGuide): TdxGuideViewInfo;
    function GetHitTest(const APoint: TPoint): TdxAdornerCustomHitTest; overload;
    function GetHitTest(X, Y: Integer): TdxAdornerCustomHitTest; overload;

    property ClipRect: TRect read FClipRect;
    property Guides: TdxGuides read GetGuides;
    property ItemCount: Integer read GetItemCount;
    property Items[AIndex: Integer]: TdxGuideViewInfo read GetItem; default;
    property LayerBounds: TRect read FLayerBounds;
    property LayerColor: TdxAlphaColor read GetLayerColor;
    property LayerParent: HWND read FLayerParent;
    property Visible: Boolean read FVisible;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;

    procedure Calculate;
    procedure Paint(ACanvas: TdxGPCanvas);

    property Bounds: TRect read FBounds;
    property Manager: TdxUIAdornerManager read GetManager;
  end;


  { TdxCustomAdorner }

  TdxCustomAdorner = class(TcxComponentCollectionItem, IdxScaleFactor)
  strict private
    FScaleFactor: TdxOwnedScaleFactor;
    FTargetElement: TdxAdornerCustomTargetElement;
    FTargetElementClass: TdxAdornerCustomTargetElementClass;
    FVisible: Boolean;
    FOnChanged: TdxAdornerNotifyEvent;
    FOnClick: TdxAdornerNotifyEvent;
    FOnCustomDraw: TdxAdornerCustomDrawEvent;
    FOnCustomDrawBackground: TdxAdornerCustomDrawEvent;

    function GetAdorners: TdxCustomAdorners;
    function GetManager: TdxUIAdornerManager;
    function GetScaleFactor: TdxScaleFactor;
    function GetTargetElementClassName: string; overload;
    function GetTargetElementClassName(AValue: TdxAdornerCustomTargetElement): string; overload;
    function GetTargetElementVisible: Boolean;
    procedure ScaleFactorChangeHandler(Sender: TObject; M, D: Integer; IsLoading: Boolean);
    procedure SetTargetElement(AValue: TdxAdornerCustomTargetElement);
    procedure SetTargetElementClass(AValue: TdxAdornerCustomTargetElementClass);
    procedure SetTargetElementClassName(const AValue: string);
    procedure SetVisible(AValue: Boolean);
    procedure SetOnChanged(AValue: TdxAdornerNotifyEvent);
    procedure SetOnClick(AValue: TdxAdornerNotifyEvent);
    procedure SetOnCustomDraw(AValue: TdxAdornerCustomDrawEvent);
    procedure SetOnCustomDrawBackground(AValue: TdxAdornerCustomDrawEvent);
  protected
    procedure CreateTargetElement;
    procedure RecreateTargetElement;

    function CanUpdate: Boolean;
    procedure Changed;
    procedure ChangeScale(M, D: Integer); virtual;
    procedure CheckCover(AChangedHandle: HWND; ABounds, AOldBounds: TRect);
    procedure DoAssign(Source: TPersistent); virtual;
    function GetDefaultTargetElementClass: TdxAdornerCustomTargetElementClass; virtual;
    function GetDisplayName: string; override;
    function GetManagerFromParent(AParent: TComponent): TdxUIAdornerManager;
    function GetTargetElementName: string;
    procedure DoUpdate; virtual; abstract;
    function HasTargetElement: Boolean;
    function IsTargetElementClassNameStored: Boolean; virtual;
    procedure Update;

    procedure DoChanged;
    procedure DoClick;
    procedure DoCustomDraw(AViewInfo: TdxCustomAdornerViewInfo; ACanvas: TdxGPCanvas; var ADone: Boolean);
    procedure DoCustomDrawBackground(AViewInfo: TdxCustomAdornerViewInfo;
      ACanvas: TdxGPCanvas; var ADone: Boolean);

    property ScaleFactor: TdxOwnedScaleFactor read FScaleFactor;
    property TargetElementVisible: Boolean read GetTargetElementVisible;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    property Adorners: TdxCustomAdorners read GetAdorners;
    property Manager: TdxUIAdornerManager read GetManager;
    property TargetElementClass: TdxAdornerCustomTargetElementClass read FTargetElementClass write SetTargetElementClass;
  published
    property TargetElementClassName: string read GetTargetElementClassName write SetTargetElementClassName
      stored IsTargetElementClassNameStored;
    property TargetElement: TdxAdornerCustomTargetElement read FTargetElement write SetTargetElement;
    property Visible: Boolean read FVisible write SetVisible default True;
    property OnChanged: TdxAdornerNotifyEvent read FOnChanged write SetOnChanged;
    property OnClick: TdxAdornerNotifyEvent read FOnClick write SetOnClick;
    property OnCustomDraw: TdxAdornerCustomDrawEvent read FOnCustomDraw write SetOnCustomDraw;
    property OnCustomDrawBackground: TdxAdornerCustomDrawEvent read FOnCustomDrawBackground
      write SetOnCustomDrawBackground;
  end;

  { TdxBadge }

  TdxBadge = class(TdxCustomAdorner)
  strict private
    FAlignment: TcxEditAlignment;
    FBackground: TdxBadgeBackground;
    FFont: TFont;
    FLayeredWindow: TdxBadgeLayeredWindow;
    FOffset: TdxPoint;
    FOwnedScaledControl: TWinControl;
    FParentFont: Boolean;
    FIsParentFontAssigning: Boolean;
    FSize: TcxSize;
    FText: string;
    FTextMargins: TcxMargin;
    FViewInfo: TdxBadgeViewInfo;

    function GetAdorners: TdxBadges;
    function IsFontStored: Boolean;
    function IsTextStored: Boolean;
    procedure SetAlignment(AValue: TcxEditAlignment);
    procedure SetBackground(AValue: TdxBadgeBackground);
    procedure SetFont(AValue: TFont);
    procedure SetOffset(AValue: TdxPoint);
    procedure SetOwnedScaledControl(AValue: TWinControl);
    procedure SetParentFont(AValue: Boolean);
    procedure SetSize(AValue: TcxSize);
    procedure SetText(const AValue: string);
    procedure SetTextMargins(AValue: TcxMargin);

    procedure ChangeHandler(Sender: TObject);
    procedure FontChangeHandler(Sender: TObject);
  protected
    function CreateAlignment: TcxEditAlignment; virtual;
    function CreateBackground: TdxBadgeBackground; virtual;
    function CreateFont: TFont; virtual;
    function CreateLayeredWindow: TdxBadgeLayeredWindow; virtual;
    function CreateOffset: TdxPoint; virtual;
    function CreateSize: TcxSize; virtual;
    function CreateTextMargins: TcxMargin; virtual;
    function CreateViewInfo: TdxBadgeViewInfo; virtual;

    procedure ChangeScale(M: Integer; D: Integer); override;
    procedure CheckPosition(AChangedHandle: HWND);
    procedure DoAssign(Source: TPersistent); override;
    procedure DoUpdate; override;
    function GetCollectionFromParent(AParent: TComponent): TcxComponentCollection; override;
    procedure Notification(Sender: TComponent; Operation: TOperation); override;
    procedure ParentFontChanged;
    procedure UpdateLayeredWindow;

    property LayeredWindow: TdxBadgeLayeredWindow read FLayeredWindow;
    property OwnedScaledControl: TWinControl read FOwnedScaledControl write SetOwnedScaledControl;
    property ViewInfo: TdxBadgeViewInfo read FViewInfo;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SetParentComponent(Value: TComponent); override;

    property Adorners: TdxBadges read GetAdorners;
  published
    property Alignment: TcxEditAlignment read FAlignment write SetAlignment;
    property Background: TdxBadgeBackground read FBackground write SetBackground;
    property Font: TFont read FFont write SetFont stored IsFontStored;
    property Offset: TdxPoint read FOffset write SetOffset;
    property ParentFont: Boolean read FParentFont write SetParentFont default True;
    property Size: TcxSize read FSize write SetSize;
    property Text: string read FText write SetText stored IsTextStored;
    property TextMargins: TcxMargin read FTextMargins write SetTextMargins;
  end;

  { TdxGuide }

  TdxGuide = class(TdxCustomAdorner)
  strict private
    FAllowCalloutPopup: Boolean;
    FBorderColor: TColor;
    FCalloutPopupAlignment: TdxCalloutPopupAlignment;
    FTabOrder: TTabOrder;
    FOnGetCalloutPopupControl: TdxGuideGetCalloutPopupControl;

    function GetActualBorderColor: TColor;
    function GetAdorners: TdxGuides;
    function GetCalloutPopupControl: TWinControl;
    function GetSelected: Boolean;
    function GetViewInfo: TdxGuideViewInfo;
    procedure SetAllowCalloutPopup(AValue: Boolean);
    procedure SetBorderColor(AValue: TColor);
    procedure SetCalloutPopupAlignment(AValue: TdxCalloutPopupAlignment);
    procedure SetSelected(AValue: Boolean);
    procedure SetTabOrder(AValue: TTabOrder);
    procedure SetOnGetCalloutPopupControl(AValue: TdxGuideGetCalloutPopupControl);
  protected
    function CreateViewInfo: TdxGuideViewInfo; virtual;

    function CanSelect: Boolean;
    procedure DoAssign(Source: TPersistent); override;
    procedure DoUpdate; override;
    function GetCollectionFromParent(AParent: TComponent): TcxComponentCollection; override;
    procedure ToggleCalloutPopup;

    procedure DoGetCalloutPopupControl(out AControl: TWinControl);

    property ActualBorderColor: TColor read GetActualBorderColor;
    property CalloutPopupControl: TWinControl read GetCalloutPopupControl;
    property ViewInfo: TdxGuideViewInfo read GetViewInfo;
  public
    constructor Create(AOwner: TComponent); override;

    property Adorners: TdxGuides read GetAdorners;
    property Selected: Boolean read GetSelected write SetSelected;
  published
    property AllowCalloutPopup: Boolean read FAllowCalloutPopup write SetAllowCalloutPopup default True;
    property BorderColor: TColor read FBorderColor write SetBorderColor default clDefault;
    property CalloutPopupAlignment: TdxCalloutPopupAlignment read FCalloutPopupAlignment write SetCalloutPopupAlignment
      default cpaBottomCenter;
    property TabOrder: TTabOrder read FTabOrder write SetTabOrder default 0;
    property OnGetCalloutPopupControl: TdxGuideGetCalloutPopupControl read FOnGetCalloutPopupControl
      write SetOnGetCalloutPopupControl;
  end;


  { TdxGuideController }

  TdxGuideController = class
  strict private
    FCalloutPopup: TdxCalloutPopup;
    FClicked: TdxGuide;
    FGuides: TdxGuides;
    FSelected: TdxGuide;

    function GetManager: TdxUIAdornerManager;
    procedure SetSelected(AValue: TdxGuide);
  protected
    procedure CheckSelection; virtual;
    procedure ClearSelection;
    function CreateCalloutPopup: TdxCalloutPopup; virtual;
    function CompareTabOrder(AItem1, AItem2: Pointer): Integer; virtual;
    function FindNext(AGuide: TdxGuide; AGoForward: Boolean = True): TdxGuide;
    function IsClicked(AValue: TdxGuide): Boolean; virtual;
    procedure PopulateTabOrderList(AList: TdxFastObjectList); virtual;
    procedure SelectFirst; virtual;
    procedure SelectNext(AGoForward: Boolean = True); overload; virtual;
    procedure SelectNext(AGuide: TdxGuide; AGoForward: Boolean = True); overload; virtual;
    procedure ShowCalloutPopup(AForGuide: TdxGuide); virtual;
    procedure ToggleCalloutPopup; overload;
    procedure ToggleCalloutPopup(AGuide: TdxGuide); overload;

    property CalloutPopup: TdxCalloutPopup read FCalloutPopup;
    property Clicked: TdxGuide read FClicked write FClicked;
    property Guides: TdxGuides read FGuides;
    property Manager: TdxUIAdornerManager read GetManager;
    property Selected: TdxGuide read FSelected write SetSelected;
  public
    constructor Create(AGuides: TdxGuides); virtual;
    destructor Destroy; override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); virtual;
    procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer); virtual;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); virtual;

    procedure KeyDown(var Key: Word; Shift: TShiftState); virtual;
    procedure KeyPress(var Key: Char); virtual;
    procedure KeyUp(var Key: Word; Shift: TShiftState); virtual;
  end;


  { TdxCustomAdorners }

  TdxCustomAdorners = class(TcxComponentCollection)
  strict private
    FActive: Boolean;
    FIsActiveChanged: Boolean;

    function GetItem(Index: Integer): TdxCustomAdorner;
    function GetManager: TdxUIAdornerManager;
    procedure SetActive(AValue: Boolean);
    procedure SetItem(Index: Integer; AValue: TdxCustomAdorner);
  protected
    procedure ActiveChanged; virtual;
    function CanUpdateLayer: Boolean;
    procedure CheckAdornerCovers(AChangedHandle: HWND; ABounds, AOldBounds: TRect); virtual;
    procedure CheckAdornerPositions(AUpdatedHandle: HWND); virtual; abstract;
    procedure DoUpdateLayer; virtual; abstract;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent);
    function GetItemClass: TcxComponentCollectionItemClass; virtual; abstract;
    function GetItemPrefixName: string; override;
    function GetName: string; virtual; abstract;
    procedure TargetControlFreed(ATargetControl: TControl);
    procedure Update(AItem: TcxComponentCollectionItem; AAction: TcxComponentCollectionNotification); override;
    procedure UpdateLayer;

    property IsActiveChanged: Boolean read FIsActiveChanged;
  public
    constructor Create(AManager: TdxUIAdornerManager); reintroduce; virtual;

    procedure Assign(Source: TPersistent); override;

    property Items[Index: Integer]: TdxCustomAdorner read GetItem write SetItem; default;
    property Manager: TdxUIAdornerManager read GetManager;
  published
    property Active: Boolean read FActive write SetActive default False;
  end;

  { TdxBadges }

  TdxBadges = class(TdxCustomAdorners)
  strict private
    FColor: TColor;
    FFont: TFont;
    FOwnerControlWndProcObject: TcxWindowProcLinkedObject;
    FParentFont: Boolean;
    FIsParentFontAssigning: Boolean;

    function GetActualColor: TColor;
    function GetItem(Index: Integer): TdxBadge;
    function GetParentFontInstance: TFont;
    function IsFontStored: Boolean;
    procedure SetColor(AValue: TColor);
    procedure SetFont(AValue: TFont);
    procedure SetItem(Index: Integer; AValue: TdxBadge);
    procedure SetParentFont(AValue: Boolean);
    procedure FontChangeHandler(Sender: TObject);
    procedure OwnerWindowWndProc(var AMessage: TMessage);
  protected
    function CreateFont: TFont; virtual;

    procedure CheckAdornerPositions(AChangedHandle: HWND); override;
    procedure DoUpdateLayer; override;
    function GetItemClass: TcxComponentCollectionItemClass; override;
    function GetName: string; override;
    procedure ItemsParentFontChanged;
    procedure ParentFontChanged;
    procedure UpdateBadges;

    property ActualColor: TColor read GetActualColor;
    property OwnerControlWndProcObject: TcxWindowProcLinkedObject read FOwnerControlWndProcObject;
    property ParentFontInstance: TFont read GetParentFontInstance;
  public
    constructor Create(AManager: TdxUIAdornerManager); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    function Add: TdxBadge;
    function FindItemByID(ID: Integer): TdxBadge;
    function IndexOf(AItem: TdxBadge): Integer;
    function Insert(Index: Integer): TdxBadge;
    procedure Remove(AItem: TdxBadge);

    property Items[Index: Integer]: TdxBadge read GetItem write SetItem; default;
  published
    property Color: TColor read FColor write SetColor default clDefault;
    property Font: TFont read FFont write SetFont stored IsFontStored;
    property ParentFont: Boolean read FParentFont write SetParentFont default True;
  end;

  { TdxGuides }

  TdxGuides = class(TdxCustomAdorners)
  strict private
    FAllowArrowKeyNavigation: Boolean;
    FAllowTabKeyNavigation: Boolean;
    FBorderColor: TColor;
    FController: TdxGuideController;
    FDeactivateKey: TShortCut;
    FLayerColor: TdxAlphaColor;
    FLayeredWindow: TdxGuideLayeredWindow;
    FViewInfo: TdxGuideLayerViewInfo;

    function GetActualBorderColor: TColor;
    function GetActualLayerColor: TdxAlphaColor;
    function GetItem(Index: Integer): TdxGuide;
    function IsLayerColorStored: Boolean;
    procedure SetAllowArrowKeyNavigation(AValue: Boolean);
    procedure SetAllowTabKeyNavigation(AValue: Boolean);
    procedure SetBorderColor(AValue: TColor);
    procedure SetDeactivateKey(AValue: TShortCut);
    procedure SetItem(Index: Integer; AValue: TdxGuide);
    procedure SetLayerColor(AValue: TdxAlphaColor);
  protected
    function CreateController: TdxGuideController; virtual;
    function CreateLayeredWindow: TdxGuideLayeredWindow; virtual;
    function CreateViewInfo: TdxGuideLayerViewInfo; virtual;

    procedure ActiveChanged; override;
    procedure CheckAdornerPositions(AChangedHandle: HWND); override;
    procedure DoUpdateLayer; override;
    function GetItemClass: TcxComponentCollectionItemClass; override;
    function GetName: string; override;
    function IsDeactivateKeyStored: Boolean;
    procedure RecreateViewInfo;
    procedure ToggleCalloutPopup;
    procedure Update(AItem: TcxComponentCollectionItem; AAction: TcxComponentCollectionNotification); override;
    procedure UpdateLayeredWindow;

    property ActualBorderColor: TColor read GetActualBorderColor;
    property ActualLayerColor: TdxAlphaColor read GetActualLayerColor;
    property Controller: TdxGuideController read FController;
    property LayeredWindow: TdxGuideLayeredWindow read FLayeredWindow;
    property ViewInfo: TdxGuideLayerViewInfo read FViewInfo;
  public
    constructor Create(AManager: TdxUIAdornerManager); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    function Add: TdxGuide;
    function FindItemByID(ID: Integer): TdxGuide;
    function IndexOf(AItem: TdxGuide): Integer;
    function Insert(Index: Integer): TdxGuide;
    procedure Remove(AItem: TdxGuide);

    property Items[Index: Integer]: TdxGuide read GetItem write SetItem; default;
  published
    property AllowArrowKeyNavigation: Boolean read FAllowArrowKeyNavigation write SetAllowArrowKeyNavigation default True;
    property AllowTabKeyNavigation: Boolean read FAllowTabKeyNavigation write SetAllowTabKeyNavigation default True;
    property BorderColor: TColor read FBorderColor write SetBorderColor default clDefault;
    property DeactivateKey: TShortCut read FDeactivateKey write SetDeactivateKey stored IsDeactivateKeyStored;
    property LayerColor: TdxAlphaColor read FLayerColor write SetLayerColor stored IsLayerColorStored;
  end;


  { TdxUIAdornerManager }

  TdxUIAdornerManager = class(TcxCustomComponent)
  strict private
    FBadges: TdxBadges;
    FDesignHelper: TdxUIAdornerManagerCustomDesignHelper;
    FGuides: TdxGuides;
    FImageChangeLink: TChangeLink;
    FImages: TCustomImageList;
    FIsCreating: Boolean;
    FLockCount: Integer;
    FOnActiveChanged: TdxAdornersNotifyEvent;
    FOnAdornerChanged: TdxAdornerNotifyEvent;
    FOnAdornerClick: TdxAdornerNotifyEvent;
    FOnAdornerCustomDraw: TdxAdornerCustomDrawEvent;
    FOnAdornerCustomDrawBackground: TdxAdornerCustomDrawEvent;
    FOnGuideGetCalloutPopupControl: TdxGuideGetCalloutPopupControl;

    function GetDesignHelper: TdxUIAdornerManagerCustomDesignHelper;
    function GetIsDesigning: Boolean;
    function GetIsDestroying: Boolean;
    function GetIsLoading: Boolean;
    function GetOwnerControl: TWinControl;
    procedure ImageListChange(Sender: TObject);
    procedure SetBadges(AValue: TdxBadges);
    procedure SetGuides(AValue: TdxGuides);
    procedure SetImages(Value: TCustomImageList);
    procedure SetOnActiveChanged(AValue: TdxAdornersNotifyEvent);
    procedure SetOnAdornerChanged(AValue: TdxAdornerNotifyEvent);
    procedure SetOnAdornerClick(AValue: TdxAdornerNotifyEvent);
    procedure SetOnAdornerCustomDraw(Value: TdxAdornerCustomDrawEvent);
    procedure SetOnAdornerCustomDrawBackground(Value: TdxAdornerCustomDrawEvent);
    procedure SetOnGuideGetCalloutPopupControl(AValue: TdxGuideGetCalloutPopupControl);
  protected
    function CreateAdornerTargetElementTree: TdxUIAdornerManagerTargetElementTree; virtual;
    function CreateBadges: TdxBadges; virtual;
    function CreateDesignHelper: TdxUIAdornerManagerCustomDesignHelper;
    function CreateGuides: TdxGuides; virtual;

    function CanUpdate: Boolean;
    procedure CheckAdornerCovers(AChangedHandle: HWND; ABounds, AOldBounds: TRect);
    procedure CheckAdornerPositions(AChangedHandle: HWND);
    procedure Changed;
    procedure DoAssign(Source: TPersistent); virtual;
    procedure DoUpdate; virtual;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    function HasDesignHelper: Boolean;
    function IsUpdating: Boolean;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure TargetControlFreed(ATargetControl: TControl);
    procedure Update;

    procedure DoActiveChanged(AAdorners: TdxCustomAdorners);
    procedure DoAdornerChanged(AAdorner: TdxCustomAdorner);
    procedure DoAdornerClick(AAdorner: TdxCustomAdorner);
    procedure DoAdornerCustomDraw(AItem: TdxCustomAdorner; AViewInfo: TdxCustomAdornerViewInfo;
      ACanvas: TdxGPCanvas; var ADone: Boolean);
    procedure DoAdornerCustomDrawBackground(AItem: TdxCustomAdorner;
      AViewInfo: TdxCustomAdornerViewInfo; ACanvas: TdxGPCanvas; var ADone: Boolean);
    procedure DoGuideGetCalloutPopupControl(AGuide: TdxGuide; out AControl: TWinControl);

    property DesignHelper: TdxUIAdornerManagerCustomDesignHelper read GetDesignHelper;
    property IsCreating: Boolean read FIsCreating;
    property IsDesigning: Boolean read GetIsDesigning;
    property IsDestroying: Boolean read GetIsDestroying;
    property IsLoading: Boolean read GetIsLoading;
    property OwnerControl: TWinControl read GetOwnerControl;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    procedure BeginUpdate;
    procedure EndUpdate;
  published
    property Badges: TdxBadges read FBadges write SetBadges;
    property Guides: TdxGuides read FGuides write SetGuides;
    property Images: TCustomImageList read FImages write SetImages;
    property OnActiveChanged: TdxAdornersNotifyEvent read FOnActiveChanged write SetOnActiveChanged;
    property OnAdornerChanged: TdxAdornerNotifyEvent read FOnAdornerChanged write SetOnAdornerChanged;
    property OnAdornerClick: TdxAdornerNotifyEvent read FOnAdornerClick write SetOnAdornerClick;
    property OnAdornerCustomDraw: TdxAdornerCustomDrawEvent read FOnAdornerCustomDraw write SetOnAdornerCustomDraw;
    property OnAdornerCustomDrawBackground: TdxAdornerCustomDrawEvent read FOnAdornerCustomDrawBackground
      write SetOnAdornerCustomDrawBackground;
    property OnGuideGetCalloutPopupControl: TdxGuideGetCalloutPopupControl read FOnGuideGetCalloutPopupControl
      write SetOnGuideGetCalloutPopupControl;
  end;

function dxBadgeDefaultGlyph: TdxSmartGlyph;
function dxGuideNormalBorderGlyph: TdxSmartGlyph;
function dxGuideSelectedBorderGlyph: TdxSmartGlyph;
function dxRegisteredAdornerTargetElementTypes: TcxRegisteredClasses;

var
  dxUIAdornerManagerDesignHelperClass: TdxUIAdornerManagerCustomDesignHelperClass;

implementation

{$R dxUIAdorners.res}

uses
  Forms, Types, Menus, RTLConsts, SysUtils, Math, dxCore, cxEditConsts, dxGDIPlusAPI, dxSkinsCore, dxHooks,
  dxMessages, dxForms, cxDWMApi, dxDPIAwareUtils;

const
  dxBadgeDefaultColor = clRed;
  dxBadgeBackgroundImageMargin = 9;
  dxGuideDefaultBorderColor = clYellow;
  dxGuideBorderImageMargin = 3;
  dxGuideLayerDefaultOpacity = 90;

  htError = -1;
  htNone = 0;
  htGuide = 1;

type
  TControlAccess = class(TControl);
  TcxEditAlignmentAccess = class(TcxEditAlignment);
  TdxFormAccess = class(TdxForm);

  { TdxHitTests }

  TdxHitTests = class
  strict private
    FItems: TdxFastObjectList;

    function GetCount: Integer; inline;
    function GetInstance(AClass: TdxAdornerCustomHitTestClass): TdxAdornerCustomHitTest;
    function GetItem(Index: Integer): TdxAdornerCustomHitTest; inline;
  protected
    function GetObjectByClass(AClass: TdxAdornerCustomHitTestClass): TdxAdornerCustomHitTest;

    property Count: Integer read GetCount;
    property Items[Index: Integer]: TdxAdornerCustomHitTest read GetItem;
  public
    constructor Create;
    destructor Destroy; override;

    property Instances[AClass: TdxAdornerCustomHitTestClass]: TdxAdornerCustomHitTest read GetInstance; default;
  end;


  { TdxUIAdornerManagerHookController }

  TdxUIAdornerManagerHookController = class
  protected
    class procedure CheckAdornerCovers(AChangedHandle: HWND; AWindowPos: PWindowPos); overload;
    class procedure CheckAdornerCovers(AChangedHandle: HWND; ABounds, AOldBounds: TRect); overload;
    class procedure CheckAdornerCovers(AChangedHandle: HWND; ABounds, AOldBounds: TRect; AManager: TdxUIAdornerManager); overload;
    class procedure CheckAdornerPositions(AChangedHandle: HWND); overload;
    class procedure CheckAdornerPositions(AChangedHandle: HWND; AManager: TdxUIAdornerManager); overload;
    class procedure WindowPosChangedMessageHandler(AHandle: HWND; AWindowPos: PWindowPos);
    class procedure WndProc(ACode: Integer; AWParam: WPARAM; ALParam: LPARAM; var AHookResult: LRESULT);
  end;

var
  FHitTests: TdxHitTests;
  FBadgeDefaultGlyph: TdxSmartGlyph;
  FGuideNormalBorderGlyph: TdxSmartGlyph;
  FGuideSelectedBorderGlyph: TdxSmartGlyph;
  FRegisteredManagers: TdxFastObjectList;
  FRegisteredTargetElementTypes: TcxRegisteredClasses;

function dxBadgeDefaultGlyph: TdxSmartGlyph;
begin
  if FBadgeDefaultGlyph = nil then
  begin
    FBadgeDefaultGlyph := TdxSmartGlyph.Create;
    FBadgeDefaultGlyph.LoadFromResource(HInstance, 'CX_BADGEBACKGROUND', 'PNG');
  end;
  Result := FBadgeDefaultGlyph;
end;

function dxGuideNormalBorderGlyph: TdxSmartGlyph;
begin
  if FGuideNormalBorderGlyph = nil then
  begin
    FGuideNormalBorderGlyph := TdxSmartGlyph.Create;
    FGuideNormalBorderGlyph.LoadFromResource(HInstance, 'CX_GUIDENORMALBORDERS', 'PNG');
  end;
  Result := FGuideNormalBorderGlyph;
end;

function dxGuideSelectedBorderGlyph: TdxSmartGlyph;
begin
  if FGuideSelectedBorderGlyph = nil then
  begin
    FGuideSelectedBorderGlyph := TdxSmartGlyph.Create;
    FGuideSelectedBorderGlyph.LoadFromResource(HInstance, 'CX_GUIDESELECTEDBORDERS', 'PNG');
  end;
  Result := FGuideSelectedBorderGlyph;
end;

function dxRegisteredAdornerTargetElementTypes: TcxRegisteredClasses;
begin
  if FRegisteredTargetElementTypes = nil then
  begin
    FRegisteredTargetElementTypes := TcxRegisteredClasses.Create;
    FRegisteredTargetElementTypes.Sorted := True;
  end;
  Result := FRegisteredTargetElementTypes;
end;

function dxRegisteredManagers: TdxFastObjectList;
begin
  if FRegisteredManagers = nil then
    FRegisteredManagers := TdxFastObjectList.Create(False);
  Result := FRegisteredManagers;
end;

procedure dxManagerWndProcHook(ACode: Integer; AWParam: WPARAM; ALParam: LPARAM; var AHookResult: LRESULT);
begin
  TdxUIAdornerManagerHookController.WndProc(ACode, AWParam, ALParam, AHookResult);
end;

procedure dxRegisterManager(AManager: TdxUIAdornerManager);
begin
  if dxRegisteredManagers.IndexOf(AManager) < 0 then
  begin
    dxRegisteredManagers.Add(AManager);
    if dxRegisteredManagers.Count = 1 then
      dxSetHook(htWndProc, dxManagerWndProcHook);
  end;
end;

procedure dxUnregisterManager(AManager: TdxUIAdornerManager);
begin
  dxRegisteredManagers.Remove(AManager);
  if dxRegisteredManagers.Count = 0 then
    dxReleaseHook(dxManagerWndProcHook);
end;

function dxGPGetTextSize(const AText: string; AFont: TFont): TSize;
var
  ARect: TRect;
begin
  dxGPPaintCanvas.BeginPaint(cxScreenCanvas.Handle, cxNullRect);
  try
    dxGPGetTextRect(dxGPPaintCanvas, AText, AFont, False, cxNullRect, ARect);
    Result := cxRectSize(ARect);
  finally
    dxGPPaintCanvas.EndPaint;
  end;
end;

function dxIsMainFormIconic: Boolean;
begin
  Result := (Application.MainForm <> nil) and Application.MainForm.HandleAllocated and IsIconic(Application.MainForm.Handle);
end;

{ TdxUIAdornerManagerHitTests }

constructor TdxHitTests.Create;
begin
  inherited Create;
  FItems := TdxFastObjectList.Create;
end;

destructor TdxHitTests.Destroy;
begin
  FreeAndNil(FItems);
  inherited Destroy;
end;

function TdxHitTests.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TdxHitTests.GetInstance(AClass: TdxAdornerCustomHitTestClass): TdxAdornerCustomHitTest;
begin
  Result := GetObjectByClass(AClass);
  if Result = nil then
  begin
    Result := AClass.Create;
    FItems.Add(Result);
  end;
end;

function TdxHitTests.GetItem(Index: Integer): TdxAdornerCustomHitTest;
begin
  Result := TdxAdornerCustomHitTest(FItems[Index]);
end;

function TdxHitTests.GetObjectByClass(AClass: TdxAdornerCustomHitTestClass): TdxAdornerCustomHitTest;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Result := Items[I];
    if Result.ClassType = AClass then
      Exit;
  end;
  Result := nil;
end;

{ TdxUIAdornerManagerHookController }

class procedure TdxUIAdornerManagerHookController.CheckAdornerCovers(AChangedHandle: HWND; AWindowPos: PWindowPos);
var
  ABounds, AOldBounds: TRect;
  AControl: TWinControl;
begin
  AControl := FindControl(AChangedHandle);
  if AControl = nil then
    Exit;
  AOldBounds := AControl.BoundsRect;
  ABounds := Rect(AWindowPos^.x, AWindowPos^.y, AWindowPos^.x + AWindowPos^.cx, AWindowPos^.y + AWindowPos^.cy);
  if AControl.Parent <> nil then
  begin
    AOldBounds := dxMapWindowRect(AControl.Parent.Handle, 0, AOldBounds);
    ABounds := dxMapWindowRect(AControl.Parent.Handle, 0, ABounds);
  end;
  CheckAdornerCovers(AChangedHandle, ABounds, AOldBounds);
end;

class procedure TdxUIAdornerManagerHookController.CheckAdornerCovers(AChangedHandle: HWND; ABounds, AOldBounds: TRect);
var
  I: Integer;
begin
  for I := 0 to dxRegisteredManagers.Count - 1 do
    CheckAdornerCovers(AChangedHandle, ABounds, AOldBounds, TdxUIAdornerManager(dxRegisteredManagers[I]));
end;

class procedure TdxUIAdornerManagerHookController.CheckAdornerCovers(AChangedHandle: HWND; ABounds, AOldBounds: TRect;
  AManager: TdxUIAdornerManager);
begin
  AManager.CheckAdornerCovers(AChangedHandle, ABounds, AOldBounds);
end;

class procedure TdxUIAdornerManagerHookController.CheckAdornerPositions(AChangedHandle: HWND; AManager: TdxUIAdornerManager);
begin
  AManager.CheckAdornerPositions(AChangedHandle);
end;

class procedure TdxUIAdornerManagerHookController.CheckAdornerPositions(AChangedHandle: HWND);
var
  I: Integer;
begin
  for I := 0 to dxRegisteredManagers.Count - 1 do
    CheckAdornerPositions(AChangedHandle, TdxUIAdornerManager(dxRegisteredManagers[I]));
end;

class procedure TdxUIAdornerManagerHookController.WindowPosChangedMessageHandler(AHandle: HWND; AWindowPos: PWindowPos);
const
  AFlags = SWP_SHOWWINDOW or SWP_HIDEWINDOW;
  ANoFlags = SWP_NOSIZE or SWP_NOMOVE or SWP_NOZORDER;
begin
  if (AWindowPos^.flags and AFlags = 0) and (AWindowPos^.flags and ANoFlags = ANoFlags) then
    Exit;
  CheckAdornerPositions(AHandle);
  CheckAdornerCovers(AHandle, AWindowPos);
end;

class procedure TdxUIAdornerManagerHookController.WndProc(ACode: Integer; AWParam: WPARAM; ALParam: LPARAM;
  var AHookResult: LRESULT);
var
  AMessage: PCWPStruct;
begin
  AMessage := PCWPStruct(ALParam);
  case AMessage.message of
    WM_WINDOWPOSCHANGED:
      WindowPosChangedMessageHandler(AMessage.hwnd, PWindowPos(AMessage.lParam));
    DXM_UIADORNERMANAGERUPDATE, WM_HSCROLL, WM_VSCROLL, WM_PAINT:
      CheckAdornerPositions(AMessage.hwnd);
  end;
end;

{ TdxUIAdornerManagerCustomDesignHelper }

procedure TdxUIAdornerManagerCustomDesignHelper.DesignSelectionChanged(ASelection: TList);
begin
  Manager.Update;
end;

function TdxUIAdornerManagerCustomDesignHelper.GetManager: TdxUIAdornerManager;
begin
  Result := TdxUIAdornerManager(Owner);
end;

{ TdxCustomAdornerOptions }

procedure TdxCustomAdornerOptions.Changed;
begin
  Adorner.Changed;
end;

function TdxCustomAdornerOptions.GetAdorner: TdxCustomAdorner;
begin
  Result := TdxCustomAdorner(Owner);
end;

{ TdxBadgeBackground }

constructor TdxBadgeBackground.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FGlyph := CreateGlyph;

  FImageIndex := -1;
  FColor := clDefault;
  FFitMode := ifmStretch;

  Glyph.OnChange := ChangeHandler;
end;

destructor TdxBadgeBackground.Destroy;
begin
  FreeAndNil(FGlyph);
  inherited Destroy;
end;

procedure TdxBadgeBackground.Assign(Source: TPersistent);
var
  ASource: TdxBadgeBackground;
begin
  if Source is TdxBadgeBackground then
  begin
    ASource := TdxBadgeBackground(Source);
    Color := ASource.Color;
    FitMode := ASource.FitMode;
    Glyph := ASource.Glyph;
    ImageIndex := ASource.ImageIndex;
    Transparent := ASource.Transparent;
  end;
end;

function TdxBadgeBackground.CreateGlyph: TdxSmartGlyph;
begin
  Result := TdxSmartGlyph.Create;
end;

function TdxBadgeBackground.GetActualColor: TColor;
begin
  if Color = clDefault then
    Result := Adorner.Adorners.ActualColor
  else
    Result := Color;
end;

function TdxBadgeBackground.GetAdorner: TdxBadge;
begin
  Result := TdxBadge(inherited Adorner);
end;

procedure TdxBadgeBackground.SetColor(AValue: TColor);
begin
  if Color <> AValue then
  begin
    FColor := AValue;
    Changed;
  end;
end;

procedure TdxBadgeBackground.SetFitMode(AValue: TcxImageFitMode);
begin
  if FFitMode <> AValue then
  begin
    FFitMode := AValue;
    Changed;
  end;
end;

procedure TdxBadgeBackground.SetGlyph(AValue: TdxSmartGlyph);
begin
  Glyph.Assign(AValue);
end;

procedure TdxBadgeBackground.SetImageIndex(AValue: TcxImageIndex);
begin
  if FImageIndex <> AValue then
  begin
    FImageIndex := AValue;
    Changed;
  end;
end;

procedure TdxBadgeBackground.SetTransparent(AValue: Boolean);
begin
  if Transparent <> AValue then
  begin
    FTransparent := AValue;
    Changed;
  end;
end;

procedure TdxBadgeBackground.ChangeHandler(ASender: TObject);
begin
  Changed;
end;

{ TdxAdornerCustomTargetElement }

function TdxAdornerCustomTargetElement.GetCacheInstance: Pointer;
begin
  Result := nil;
end;

function TdxAdornerCustomTargetElement.GetVisible: Boolean;
begin
  Result := HasControl and IsControlVisible(Control) and not cxRectIsEmpty(Bounds) and VisibleForControl and VisibleForParents and
    not IsFullyCovered;
end;

function TdxAdornerCustomTargetElement.HasControl: Boolean;
begin
  Result := Control <> nil;
end;

function TdxAdornerCustomTargetElement.IsCoverChanged(AChangedHandle: HWND; ABounds, AOldBounds: TRect): Boolean;
var
  AHandle: HWND;
begin
  BeginUseCache;
  try
    Result := False;
    if HasControl and IsControlVisible(Control) and (cxRectIntersect(ScreenBounds, AOldBounds) or
      cxRectIntersect(ScreenBounds, ABounds)) then
    begin
      AHandle := GetNextWindow(Control.Handle, GW_HWNDPREV);
      while not Result and (AHandle <> 0) do
      begin
        Result := AHandle = AChangedHandle;
        if not Result then
          AHandle := GetNextWindow(AHandle, GW_HWNDPREV);
      end;
    end;
  finally
    EndUseCache;
  end;
end;

function TdxAdornerCustomTargetElement.IsFullyCovered: Boolean;
var
  AHandle: HWND;
  ARect: TRect;
  ARegion, AElementRegion: HRGN;
begin
  Result := False;
  AHandle := GetNextWindow(Control.Handle, GW_HWNDPREV);
  AElementRegion := CreateRectRgnIndirect(ScreenBounds);
  try
    while not Result and (AHandle <> 0) do
    begin
      if IsWindowVisible(AHandle) then
      begin
        GetWindowRect(AHandle, ARect);
        ARegion := CreateRectRgnIndirect(ARect);
        try
          Result := CombineRgn(AElementRegion, AElementRegion, ARegion, RGN_DIFF) = NULLREGION;
        finally
          DeleteObject(ARegion);
        end;
      end;
      if not Result then
        AHandle := GetNextWindow(AHandle, GW_HWNDPREV);
    end;
  finally
    DeleteObject(AElementRegion);
  end;
end;

function TdxAdornerCustomTargetElement.IsPositionChanged(AChangedHandle: HWND; AWasControlAvailable: Boolean): Boolean;
var
  AIsControlAvailable: Boolean;
begin
  BeginUseCache;
  try
    AIsControlAvailable := HasControl and Control.HandleAllocated;
    Result := AIsControlAvailable and ((AChangedHandle = Control.Handle) or IsChild(AChangedHandle, Control.Handle)) or
      not AIsControlAvailable and AWasControlAvailable;
  finally
    EndUseCache;
  end;
end;

procedure TdxAdornerCustomTargetElement.TargetControlFreed(ATargetControl: TControl);
begin
//do nothing
end;

procedure TdxAdornerCustomTargetElement.BeginUseCache;
begin
  if not IsUsingCache then
    FCache := GetCacheInstance;
  Inc(FUseCacheLockCount);
end;

procedure TdxAdornerCustomTargetElement.EndUseCache;
begin
  Dec(FUseCacheLockCount);
  if not IsUsingCache then
    FCache := nil;
end;

function TdxAdornerCustomTargetElement.IsUsingCache: Boolean;
begin
  Result := FUseCacheLockCount > 0;
end;

function TdxAdornerCustomTargetElement.GetScreenBounds: TRect;
begin
  Result := dxMapWindowRect(Control.Handle, 0, Bounds);
end;

function TdxAdornerCustomTargetElement.GetVisibleForControl: Boolean;
begin
  Result := cxRectIntersect(Control.ClientRect, Bounds);
end;

function TdxAdornerCustomTargetElement.GetVisibleForParents: Boolean;
var
  ARect: TRect;
  AParent: TWinControl;
begin
  Result := True;
  AParent := Control.Parent;
  while Result and (AParent <> nil) do
  begin
    ARect := dxMapWindowRect(Control.Handle, AParent.Handle, Bounds);
    Result := cxRectIntersect(AParent.ClientRect, ARect);
    AParent := AParent.Parent;
  end;
end;

{ TdxAdornerTargetElementControl }

constructor TdxAdornerTargetElementControl.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FFreeNotificator := TcxFreeNotificator.Create(nil);
  FreeNotificator.OnFreeNotification := ControlFreeNotificationHandler;
end;

destructor TdxAdornerTargetElementControl.Destroy;
begin
  FreeAndNil(FFreeNotificator);
  inherited Destroy;
end;

procedure TdxAdornerTargetElementControl.Assign(Source: TPersistent);
var
  ASource: TdxAdornerTargetElementControl;
begin
  if Source is TdxAdornerTargetElementControl then
  begin
    ASource := TdxAdornerTargetElementControl(Source);
    AssignControlByElement(ASource);
  end;
end;

procedure TdxAdornerTargetElementControl.AssignControlByElement(AElement: TdxAdornerTargetElementControl);
var
  AControl, AOwnerControl, AElementOwnerControl: TWinControl;
  AComponent: TComponent;
begin
  AControl := AElement.Control;
  AOwnerControl := Adorner.Manager.OwnerControl;
  AElementOwnerControl := AElement.Adorner.Manager.OwnerControl;
  if (AControl <> nil) and (AOwnerControl <> AElementOwnerControl) then
  begin
    AComponent := AOwnerControl.FindComponent(AControl.Name);
    if AComponent is TWinControl then
      AControl := TWinControl(AComponent)
    else
      AControl := nil;
  end;
  Control := AControl;
end;

function TdxAdornerTargetElementControl.GetBounds: TRect;
begin
  Result := Control.ClientRect;
end;

function TdxAdornerTargetElementControl.GetControl: TWinControl;
begin
  Result := Control;
end;

function TdxAdornerTargetElementControl.GetDisplayName: string;
begin
  if Control <> nil then
    Result := 'Control: ' + Control.Name
  else
    Result := '';
end;

procedure TdxAdornerTargetElementControl.TargetControlFreed(ATargetControl: TControl);
begin
  if Control = ATargetControl then
    Control := nil;
end;

procedure TdxAdornerTargetElementControl.SetControl(AValue: TWinControl);
begin
  if Control <> AValue then
  begin
    if Control <> nil then
      FreeNotificator.RemoveSender(Control);
    FControl := AValue;
    if Control <> nil then
      FreeNotificator.AddSender(Control);
    Changed;
  end;
end;

procedure TdxAdornerTargetElementControl.ControlFreeNotificationHandler(Sender: TComponent);
begin
  Adorner.Manager.TargetControlFreed(Control)
end;

{ TdxAdornerTargetElementPath }

procedure TdxAdornerTargetElementPath.Assign(Source: TPersistent);
var
  ASource: TdxAdornerTargetElementPath;
begin
  if Source is TdxAdornerTargetElementPath then
  begin
    ASource := TdxAdornerTargetElementPath(Source);
    Path := ASource.Path;
  end;
end;

function TdxAdornerTargetElementPath.GetBounds: TRect;
begin
  Result := Element.GetBounds;
end;

function TdxAdornerTargetElementPath.GetCacheInstance: Pointer;
begin
  Result := Pointer(Element);
end;

function TdxAdornerTargetElementPath.GetControl: TWinControl;
begin
  Result := Element.GetControl;
end;

function TdxAdornerTargetElementPath.GetDisplayName: string;
begin
  Result := Path;
  if Result <> '' then
    Result := 'Path: ' + Result;
end;

function TdxAdornerTargetElementPath.GetVisible: Boolean;
begin
  Result := (Element <> nil) and Element.GetVisible and inherited GetVisible;
end;

function TdxAdornerTargetElementPath.HasControl: Boolean;
begin
  Result := (Element <> nil) and inherited HasControl;
end;

function TdxAdornerTargetElementPath.GetCache: IdxAdornerTargetElement;
begin
  Result := IdxAdornerTargetElement(inherited Cache);
end;

function TdxAdornerTargetElementPath.GetElement: IdxAdornerTargetElement;
begin
  if IsUsingCache then
    Result := Cache
  else
    if Path <> '' then
      Result := TdxUIAdornerManagerTargetElementTree.FindElement(Adorner.Manager, Path)
    else
      Result := nil;
end;

procedure TdxAdornerTargetElementPath.SetPath(const AValue: string);
begin
  if Path <> AValue then
  begin
    FPath := AValue;
    Changed;
  end;
end;

{ TdxUIAdornerManagerTargetElementTree }

constructor TdxUIAdornerManagerTargetElementTree.Create(AManager: TdxUIAdornerManager);
begin
  inherited Create;
  FManager := AManager;
end;

procedure TdxUIAdornerManagerTargetElementTree.Build(ANodes: TTreeNodes);
begin
  PopulateElements(ANodes, nil, Self);
end;

class function TdxUIAdornerManagerTargetElementTree.CreateInstance(AManager: TdxUIAdornerManager): TdxUIAdornerManagerTargetElementTree;
begin
  Result := TdxUIAdornerManagerTargetElementTree.Create(AManager);
end;

function TdxUIAdornerManagerTargetElementTree.FindElement(const APath: string): IdxAdornerTargetElement;
begin
  Result := FindElement(APath, Self);
end;

class procedure TdxUIAdornerManagerTargetElementTree.Build(AManager: TdxUIAdornerManager; ANodes: TTreeNodes);
var
  AInstance: TdxUIAdornerManagerTargetElementTree;
begin
  AInstance := CreateInstance(AManager);
  try
    AInstance.Build(ANodes);
  finally
    AInstance.Free;
  end;
end;

class function TdxUIAdornerManagerTargetElementTree.FindElement(AManager: TdxUIAdornerManager; const APath: string): IdxAdornerTargetElement;
var
  AInstance: TdxUIAdornerManagerTargetElementTree;
begin
  AInstance := CreateInstance(AManager);
  try
    Result := AInstance.FindElement(APath);
  finally
    AInstance.Free;
  end;
end;

class function TdxUIAdornerManagerTargetElementTree.DoFindElement(const APath: string; AElements: TStrings): IdxAdornerTargetElement;
var
  AElementIndex: Integer;
begin
  AElementIndex := AElements.IndexOf(APath);
  if (AElementIndex = -1) or not Supports(AElements.Objects[AElementIndex], IdxAdornerTargetElement, Result) then
    Result := nil;
end;

class function TdxUIAdornerManagerTargetElementTree.DoFindElement(const APath, AParentPath: string;
  AElements: TStrings): IdxAdornerTargetElement;
var
  AParentIndex: Integer;
  AIntfElementCollection: IdxAdornerTargetElementCollection;
begin
  AParentIndex := AElements.IndexOf(AParentPath);
  if (AParentIndex <> -1) and Supports(AElements.Objects[AParentIndex], IdxAdornerTargetElementCollection, AIntfElementCollection) then
    Result := FindElement(APath, AIntfElementCollection)
  else
    Result := nil;
end;

class function TdxUIAdornerManagerTargetElementTree.FindElement(const APath: string;
  AElementCollection: IdxAdornerTargetElementCollection): IdxAdornerTargetElement;
var
  AElements: TStrings;
begin
  AElements := TStringList.Create;
  try
    AElementCollection.GetElements(AElements);
    Result := FindElement(APath, AElements);
  finally
    AElements.Free;
  end;
end;

class function TdxUIAdornerManagerTargetElementTree.FindElement(const APath: string; AElements: TStrings): IdxAdornerTargetElement;
var
  ASeparatorPos: Integer;
  AElementPath, AParentPath: string;
begin
  Result := nil;
  ASeparatorPos := AnsiPos(dxAdornerTargetElementPathSeparator, APath);
  if ASeparatorPos = 0 then
    Result := DoFindElement(APath, AElements)
  else
  begin
    AParentPath := Copy(APath, 1, ASeparatorPos - 1);
    AElementPath := Copy(APath, ASeparatorPos + 1, Length(APath) - ASeparatorPos);
    Result := DoFindElement(AElementPath, AParentPath, AElements);
  end
end;

class function TdxUIAdornerManagerTargetElementTree.PopulateElement(ANodes: TTreeNodes; AParentNode: TTreeNode;
  AElement: TObject; AElementName: string): Boolean;
var
  ANode: TTreeNode;
  AIsParent: Boolean;
  AIntfElement: IdxAdornerTargetElement;
  AIntfElementCollection: IdxAdornerTargetElementCollection;
begin
  Result := Supports(AElement, IdxAdornerTargetElement, AIntfElement);
  AIsParent := Supports(AElement, IdxAdornerTargetElementCollection, AIntfElementCollection);
  if Result or AIsParent then
  begin
    ANode := ANodes.AddChild(AParentNode, AElementName);
    ANode.Data := Pointer(Result);
    if AIsParent then
      Result := PopulateElements(ANodes, ANode, AIntfElementCollection) or Result;
    if not Result then
      ANodes.Delete(ANode);
  end;
end;

class function TdxUIAdornerManagerTargetElementTree.PopulateElements(ANodes: TTreeNodes; AParentNode: TTreeNode;
  AElements: TStrings): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to AElements.Count - 1 do
    Result := PopulateElement(ANodes, AParentNode, AElements.Objects[I], AElements[I]) or Result;
end;

class function TdxUIAdornerManagerTargetElementTree.PopulateElements(ANodes: TTreeNodes; AParentNode: TTreeNode;
  AElementCollection: IdxAdornerTargetElementCollection): Boolean;
var
  AElements: TStrings;
begin
  AElements := TStringList.Create;
  try
    AElementCollection.GetElements(AElements);
    Result := PopulateElements(ANodes, AParentNode, AElements);
  finally
    AElements.Free;
  end;
end;

procedure TdxUIAdornerManagerTargetElementTree.GetElements(AList: TStrings);
var
  I: Integer;
  AComponent: TComponent;
  AInfRootElement: IdxAdornerRootTargetElement;
begin
  for I := 0 to Manager.OwnerControl.ComponentCount - 1 do
  begin
    AComponent := Manager.OwnerControl.Components[I];
    if not (csDestroying in AComponent.ComponentState) and (AComponent.Name <> '') and
      Supports(AComponent, IdxAdornerRootTargetElement, AInfRootElement) then
      AList.AddObject(AComponent.Name, AComponent);
  end;
end;

{ TdxUIAdornerManagerCustomLayeredWindow }

procedure TdxUIAdornerManagerCustomLayeredWindow.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := WS_POPUP;
  Params.ExStyle := WS_EX_LAYERED;
end;

procedure TdxUIAdornerManagerCustomLayeredWindow.PaintLayer(ACanvas: TcxCanvas);
var
  AGPGraphics: TdxGPGraphics;
begin
  AGPGraphics := dxGpBeginPaint(ACanvas.Handle, ClientRect);
  try
    PaintLayer(AGPGraphics);
  finally
    dxGpEndPaint(AGPGraphics);
  end;
end;

procedure TdxUIAdornerManagerCustomLayeredWindow.PaintLayer(AGPGraphics: TdxGPGraphics);
begin
  AGPGraphics.SmoothingMode := smAntiAlias;
end;

procedure TdxUIAdornerManagerCustomLayeredWindow.PaintLayer;
var
  ABitmap: TcxBitmap32;
begin
  ABitmap := TcxBitmap32.CreateSize(ClientRect, True);
  try
    PaintLayer(ABitmap.cxCanvas);
    cxUpdateLayeredWindow(Handle, ABitmap);
  finally
    ABitmap.Free;
  end;
end;

procedure TdxUIAdornerManagerCustomLayeredWindow.Update;
begin
  if Visible then
  begin
    UpdateParent(ParentWnd);
    BoundsRect := Bounds;
    PaintLayer;
  end
  else
    UpdateParent(0);
end;

procedure TdxUIAdornerManagerCustomLayeredWindow.UpdateParent(AHandle: HWND);
var
  AOld: HWND;
begin
  AOld := ParentWindow;
  ParentWindow := AHandle;
  if ParentWindow <> AOld then
    RecreateWnd;
end;

{ TdxBadgeLayeredWindow }

constructor TdxBadgeLayeredWindow.Create(ABadge: TdxBadge);
begin
  inherited Create(nil);
  FBadge := ABadge;
end;

procedure TdxBadgeLayeredWindow.MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  ViewInfo.MouseDown(X, Y, Button, Shift);
end;

procedure TdxBadgeLayeredWindow.MouseMove(Shift: TShiftState; X: Integer; Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  ViewInfo.MouseMove(X, Y, Shift);
end;

procedure TdxBadgeLayeredWindow.MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  ViewInfo.MouseUp(X, Y, Button, Shift);
end;

procedure TdxBadgeLayeredWindow.Click;
begin
  ViewInfo.Click;
end;

function TdxBadgeLayeredWindow.GetBounds: TRect;
begin
  Result := ViewInfo.LayerBounds;
end;

function TdxBadgeLayeredWindow.GetParentWnd: HWND;
begin
  Result := ViewInfo.LayerParent.Handle;
end;

function TdxBadgeLayeredWindow.GetVisible: Boolean;
begin
  Result := ViewInfo.Visible
end;

procedure TdxBadgeLayeredWindow.PaintLayer(AGPGraphics: TdxGPGraphics);
begin
  inherited PaintLayer(AGPGraphics);
  ViewInfo.Paint(AGPGraphics);
end;

function TdxBadgeLayeredWindow.GetViewInfo: TdxBadgeViewInfo;
begin
  Result := Badge.ViewInfo;
end;

procedure TdxBadgeLayeredWindow.WMMouseActivate(var Message: TWMMouseActivate);
begin
  inherited;
  Message.Result := MA_NOACTIVATE;
end;

{ TdxGuideLayeredWindow }

constructor TdxGuideLayeredWindow.Create(AGuides: TdxGuides);
begin
  inherited Create(nil);
  FGuides := AGuides;
  FOwnerControlWndProcObject := cxWindowProcController.Add(OwnerControl, OwnerWindowWndProc);
end;

destructor TdxGuideLayeredWindow.Destroy;
begin
  cxWindowProcController.Remove(FOwnerControlWndProcObject);
  inherited Destroy;
end;

function TdxGuideLayeredWindow.GetBounds: TRect;
begin
  Result := ViewInfo.LayerBounds;
end;

function TdxGuideLayeredWindow.GetParentWnd: HWND;
begin
  Result := ViewInfo.LayerParent;
end;

function TdxGuideLayeredWindow.GetVisible: Boolean;
begin
  Result := ViewInfo.Visible;
end;

procedure TdxGuideLayeredWindow.PaintLayer(AGPGraphics: TdxGPGraphics);
begin
  inherited PaintLayer(AGPGraphics);
  ViewInfo.Paint(AGPGraphics);
end;

procedure TdxGuideLayeredWindow.MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  Guides.Controller.MouseDown(Button, Shift, X, Y);
end;

procedure TdxGuideLayeredWindow.MouseMove(Shift: TShiftState; X: Integer; Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  Guides.Controller.MouseMove(Shift, X, Y);
end;

procedure TdxGuideLayeredWindow.MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  Guides.Controller.MouseUp(Button, Shift, X, Y);
end;

procedure TdxGuideLayeredWindow.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  Guides.Controller.KeyDown(Key, Shift);
end;

procedure TdxGuideLayeredWindow.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  Guides.Controller.KeyPress(Key);
end;

procedure TdxGuideLayeredWindow.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited KeyUp(Key, Shift);
  Guides.Controller.KeyUp(Key, Shift);
end;

function TdxGuideLayeredWindow.GetOwnerControl: TWinControl;
begin
  Result := Guides.Manager.OwnerControl;
end;

function TdxGuideLayeredWindow.GetViewInfo: TdxGuideLayerViewInfo;
begin
  Result := Guides.ViewInfo;
end;

procedure TdxGuideLayeredWindow.WMActivate(var Message: TWMActivate);
var
  AActive: Boolean;
begin
  inherited;
  AActive := Message.Active <> WA_INACTIVE;
  SendMessage(OwnerControl.Handle, WM_NCACTIVATE, WPARAM(AActive), 0);
end;

procedure TdxGuideLayeredWindow.WMClose(var Message: TMessage);
begin
  inherited;
  Guides.Active := False;
end;

procedure TdxGuideLayeredWindow.WMWindowPosChanged(var Message: TWMWindowPosChanged);
begin
  inherited;
  if Message.WindowPos^.flags and SWP_SHOWWINDOW = SWP_SHOWWINDOW then
    PostMessage(Handle, WM_ACTIVATE, WPARAM(True), 0)
  else
    if Message.WindowPos^.flags and SWP_HIDEWINDOW = SWP_HIDEWINDOW then
      PostMessage(Handle, WM_ACTIVATE, WPARAM(False), 0);
end;

procedure TdxGuideLayeredWindow.OwnerWindowWndProc(var AMessage: TMessage);
begin
  if Guides.Active and (AMessage.Msg = WM_ACTIVATE) then
    PostMessage(Handle, WM_ACTIVATE, WPARAM(True), 0);
  OwnerControlWndProcObject.DefaultProc(AMessage);
end;

{ TdxUIAdornerManagerCustomHitTest }

class function TdxAdornerCustomHitTest.HitTestCode: Integer;
begin
  Result := GetHitTestCode;
end;

class function TdxAdornerCustomHitTest.Instance(const APos: TPoint): TdxAdornerCustomHitTest;
begin
  Result := FHitTests.Instances[Self];
  Result.Init(APos);
end;

class function TdxAdornerCustomHitTest.GetHitTestCode: Integer;
begin
  Result := htError;
end;

procedure TdxAdornerCustomHitTest.Init(const APos: TPoint);
begin
  FPos := APos;
end;

{ TdxUIAdornerManagerNoneHitTest }

class function TdxAdornerNoneHitTest.GetHitTestCode: Integer;
begin
  Result := htNone;
end;

{ TdxGuideHitTest }

class function TdxGuideHitTest.GetHitTestCode: Integer;
begin
  Result := htGuide;
end;

{ TdxCustomAdornerPainter }

constructor TdxCustomAdornerPainter.Create(AViewInfo: TdxCustomAdornerViewInfo; ACanvas: TdxGPCanvas);
begin
  inherited Create;
  FViewInfo := AViewInfo;
  FCanvas := ACanvas;
end;

procedure TdxCustomAdornerPainter.Paint;
begin
  Canvas.SaveClipRegion;
  try
    Canvas.SetClipRect(ViewInfo.ClipRect, gmIntersect);
    if not ViewInfo.CustomDraw(Canvas) then
      Draw;
  finally
    Canvas.RestoreClipRegion;
  end;
end;

procedure TdxCustomAdornerPainter.Draw;
begin
  DrawContent;
end;

procedure TdxCustomAdornerPainter.DrawBackground;
begin
//do nothing
end;

procedure TdxCustomAdornerPainter.DrawContent;
begin
  if not ViewInfo.CustomDrawBackground(Canvas) then
    DrawBackground;
end;

{ TdxBadgePainter }

procedure TdxBadgePainter.Draw;
begin
  inherited Draw;
  if ViewInfo.IsDesignSelected then
    DrawDesignSelector;
end;

procedure TdxBadgePainter.DrawBackground;
begin
  if not ViewInfo.Transparent then
    if ViewInfo.HasGlyph then
      DrawGlyph
    else
      DrawDefaultGlyph;
end;

procedure TdxBadgePainter.DrawContent;
begin
  inherited DrawContent;
  if ViewInfo.HasText then
    DrawText;
end;

procedure TdxBadgePainter.DrawDefaultGlyph;
var
  AImage: TdxSkinImage;
  AGlyph: TdxSmartGlyph;
begin
  AGlyph := TdxSmartGlyph.Create;
  try
    AGlyph.Assign(ViewInfo.DefaultGlyph);
    AGlyph.Resize(ViewInfo.ScaleFactor.Apply(AGlyph.Size));
    AImage := TdxSkinImage.Create(nil);
    try
      AImage.Texture.Assign(AGlyph);
      AImage.Texture.ChangeColor(ViewInfo.Color);
      AImage.Margins.Margin := ViewInfo.DefaultGlyphMargin;
      AImage.DrawEx(Canvas, ViewInfo.DefaultGlyphBounds);
    finally
      AImage.Free;
    end;
  finally
    AGlyph.Free;
  end;
end;

procedure TdxBadgePainter.DrawDesignSelector;
var
  I: Integer;
  ADC: HDC;
  ARect: TRect;
begin
  ARect := ViewInfo.DesignSelectorBounds;
  for I := 0 to cxDesignSelectionWidth - 1 do
  begin
    Canvas.Rectangle(ARect, dxColorToAlphaColor(clBlack), dxColorToAlphaColor(clBlack, 0));
    InflateRect(ARect, -1, -1);
  end;
  ADC := Canvas.GetHDC;
  try
    ARect := ViewInfo.DesignSelectorBounds;
    for I := 0 to cxDesignSelectionWidth - 1 do
    begin
      DrawFocusRect(ADC, ARect);
      InflateRect(ARect, -1, -1);
    end;
  finally
    Canvas.ReleaseHDC(ADC);
  end;
end;

procedure TdxBadgePainter.DrawGlassText;
var
  ADC: HDC;
begin
  ADC := Canvas.GetHDC;
  try
    dxDrawTextOnGlass(ADC, ViewInfo.Text, ViewInfo.Font, ViewInfo.TextAreaBounds, ViewInfo.Font.Color, ViewInfo.TextFlags, 0, True);
  finally
    Canvas.ReleaseHDC(ADC);
  end;
end;

procedure TdxBadgePainter.DrawGlyph;
var
  ABitmap: TcxAlphaBitmap;
  AGlyph: TdxSmartGlyph;
begin
  AGlyph := TdxSmartGlyph.Create;
  try
    ABitmap := TcxAlphaBitmap.CreateSize(ViewInfo.Bounds, True);
    try
      cxDrawImage(ABitmap.cxCanvas, ABitmap.ClientRect, ViewInfo.Glyph, ViewInfo.Images, ViewInfo.ImageIndex,
        ViewInfo.FitMode, idmNormal, True, nil, ViewInfo.ScaleFactor);
      AGlyph.Assign(ABitmap);
    finally
      ABitmap.Free;
    end;
    if ViewInfo.NeedColorizedGlyph then
      AGlyph.ChangeColor(ViewInfo.Color);
    dxGpDrawImage(Canvas.Handle, ViewInfo.Bounds, cxRect(AGlyph.Size), AGlyph.Handle);
  finally
    AGlyph.Free;
  end;
end;

procedure TdxBadgePainter.DrawSimpleText;
begin
  dxGPDrawText(Canvas, ViewInfo.Text, ViewInfo.TextAreaBounds, ViewInfo.Font, dxColorToAlphaColor(ViewInfo.Font.Color), taCenter,
    taVerticalCenter, False, TextRenderingHintAntiAlias, StringTrimmingEllipsisCharacter,
    ViewInfo.TargetElement.Control.UseRightToLeftReading);
end;

procedure TdxBadgePainter.DrawText;
begin
  if IsCompositionEnabled then
    DrawGlassText
  else
    DrawSimpleText;
end;

function TdxBadgePainter.GetViewInfo: TdxBadgeViewInfo;
begin
  Result := TdxBadgeViewInfo(inherited ViewInfo);
end;

{ TdxGuidePainter }

procedure TdxGuidePainter.Draw;
begin
  inherited Draw;
  DrawBorders;
end;

procedure TdxGuidePainter.DrawBorders;
var
  AImage: TdxSkinImage;
begin
  AImage := TdxSkinImage.Create(nil);
  try
    AImage.Texture.Assign(ViewInfo.BorderGlyph);
    AImage.Texture.ChangeColor(ViewInfo.BorderColor);
    AImage.Margins.All := dxGuideBorderImageMargin;
    AImage.DrawEx(Canvas, ViewInfo.Bounds);
  finally
    AImage.Free;
  end;
end;

function TdxGuidePainter.GetViewInfo: TdxGuideViewInfo;
begin
  Result := TdxGuideViewInfo(inherited ViewInfo);
end;

{ TdxGuideLayerPainter }

constructor TdxGuideLayerPainter.Create(AViewInfo: TdxGuideLayerViewInfo; ACanvas: TdxGPCanvas);
begin
  inherited Create;
  FViewInfo := AViewInfo;
  FCanvas := ACanvas;
end;

procedure TdxGuideLayerPainter.Paint;
begin
  Canvas.SaveClipRegion;
  try
    Canvas.SetClipRect(ViewInfo.ClipRect, gmIntersect);
    Draw;
  finally
    Canvas.RestoreClipRegion;
  end;
end;

procedure TdxGuideLayerPainter.Draw;
begin
  DrawSubstrate;
  ExcludeBounds;
  DrawContent;
end;

procedure TdxGuideLayerPainter.DrawBackground;
begin
  Canvas.FillRectangle(ViewInfo.Bounds, ViewInfo.LayerColor);
end;

procedure TdxGuideLayerPainter.DrawContent;
begin
  DrawBackground;
  DrawGuides;
end;

procedure TdxGuideLayerPainter.DrawGuides;
var
  I: Integer;
  AGuide: TdxGuideViewInfo;
begin
  for I := 0 to ViewInfo.ItemCount - 1 do
  begin
    AGuide := ViewInfo.Items[I];
    if AGuide.Visible then
      AGuide.Paint(Canvas);
  end;
end;

procedure TdxGuideLayerPainter.DrawSubstrate;
const
  ASubstrateAlpha = 1;
begin
  Canvas.FillRectangle(ViewInfo.Bounds, dxColorToAlphaColor(ViewInfo.LayerColor, ASubstrateAlpha));
end;

procedure TdxGuideLayerPainter.ExcludeBounds;
var
  I: Integer;
begin
  for I := 0 to ViewInfo.ItemCount - 1 do
    if ViewInfo.Items[I].NeedExcludeFromClipRect then
      Canvas.SetClipRect(ViewInfo.Items[I].ExcludeBounds, gmExclude);
end;

{ TdxCustomAdornerViewInfo }

procedure TdxCustomAdornerViewInfo.Calculate;
begin
  if Adorner.HasTargetElement then
    Adorner.TargetElement.BeginUseCache;
  try
    if CanBeVisible then
    begin
      DoCalculate;
      CalculateVisibility;
    end
    else
      FVisible := False;
  finally
    if Adorner.HasTargetElement then
      Adorner.TargetElement.EndUseCache;
  end;
end;

procedure TdxCustomAdornerViewInfo.Paint(ACanvas: TdxGPCanvas);
begin
  DoPaint(ACanvas);
end;

function TdxCustomAdornerViewInfo.CalculateClipRect: TRect;
begin
  Result := Bounds;
end;

procedure TdxCustomAdornerViewInfo.CalculateVisibility;
begin
  FVisible := True;
end;

function TdxCustomAdornerViewInfo.CanBeVisible: Boolean;
begin
  Result := Adorner.Adorners.Active and Adorner.Visible and Adorner.TargetElementVisible;
end;

procedure TdxCustomAdornerViewInfo.Click;
begin
  Adorner.DoClick;
end;

procedure TdxCustomAdornerViewInfo.DoCalculate;
begin
  FBounds := CalculateBounds;
  FClipRect := CalculateClipRect;
end;

procedure TdxCustomAdornerViewInfo.DoPaint(ACanvas: TdxGPCanvas);
var
  APainter: TdxCustomAdornerPainter;
begin
  APainter := CreatePainter(ACanvas);
  try
    APainter.Paint;
  finally
    APainter.Free;
  end;
end;

procedure TdxCustomAdornerViewInfo.MouseDown(X, Y: Integer; AButton: TMouseButton; AShift: TShiftState);
begin
  if Manager.HasDesignHelper then
    Manager.DesignHelper.SelectObject(Adorner, not (ssShift in AShift));
end;

procedure TdxCustomAdornerViewInfo.MouseMove(X, Y: Integer; AShift: TShiftState);
begin
//do nothing
end;

procedure TdxCustomAdornerViewInfo.MouseUp(X, Y: Integer; AButton: TMouseButton; AShift: TShiftState);
begin
//do nothing
end;

function TdxCustomAdornerViewInfo.CustomDraw(ACanvas: TdxGPCanvas): Boolean;
begin
  Result := False;
  Adorner.DoCustomDraw(Self, ACanvas, Result);
end;

function TdxCustomAdornerViewInfo.CustomDrawBackground(ACanvas: TdxGPCanvas): Boolean;
begin
  Result := False;
  Adorner.DoCustomDrawBackground(Self, ACanvas, Result);
end;

function TdxCustomAdornerViewInfo.GetAdorner: TdxCustomAdorner;
begin
  Result := TdxCustomAdorner(Owner);
end;

function TdxCustomAdornerViewInfo.GetIsDesignSelected: Boolean;
begin
  Result := Manager.HasDesignHelper and Manager.DesignHelper.IsObjectSelected(Adorner);
end;

function TdxCustomAdornerViewInfo.GetTargetElement: TdxAdornerCustomTargetElement;
begin
  Result := Adorner.TargetElement;
end;

function TdxCustomAdornerViewInfo.GetManager: TdxUIAdornerManager;
begin
  Result := Adorner.Manager;
end;

{ TdxBadgeViewInfo }

function TdxBadgeViewInfo.CreatePainter(ACanvas: TdxGPCanvas): TdxCustomAdornerPainter;
begin
  Result := TdxBadgePainter.Create(Self, ACanvas);
end;

function TdxBadgeViewInfo.CalculateBounds: TRect;
begin
  Result := cxRectCenter(LayerClientRect, Width, Height);
end;

function TdxBadgeViewInfo.CalculateClipRect: TRect;
begin
  Result := LayerClientRect;
end;

function TdxBadgeViewInfo.CalculateDefaultGlyphBounds: TRect;
begin
  if FitMode in [ifmStretch, ifmFill, ifmProportionalStretch] then
    Result := Bounds
  else
    Result := cxRectCenter(Bounds, ScaleFactor.Apply(DefaultGlyph.Size));
end;

function TdxBadgeViewInfo.CalculateDesignSelectorBounds: TRect;
begin
  Result := LayerClientRect;
end;

procedure TdxBadgeViewInfo.CalculateLayer;
begin
  FLayerSize := GetLayerSize;
  FLayerBounds := CalculateLayerBounds;
  FLayerClientRect := CalculateLayerClientRect;
  FLayerParent := GetLayerParent;
end;

function TdxBadgeViewInfo.CanBeVisible: Boolean;
begin
  Result := inherited CanBeVisible and not dxIsMainFormIconic;
end;

function TdxBadgeViewInfo.CalculateLayerBounds: TRect;
var
  AScreenBounds, ARect: TRect;
begin
  AScreenBounds := TargetElement.ScreenBounds;
  ARect := AScreenBounds;
  case Adorner.Alignment.Horz of
    taLeftJustify:
      ARect.Right := ARect.Left + 1;
    taRightJustify:
      ARect.Left := ARect.Right - 1;
    else //taCenter
      ARect := cxRectCenterHorizontally(ARect, 1);
  end;
  case Adorner.Alignment.Vert of
    taTopJustify:
      ARect.Bottom := ARect.Top + 1;
    taBottomJustify:
      ARect.Top := ARect.Bottom - 1;
    else //taVCenter
      ARect := cxRectCenterVertically(ARect, 1);
  end;
  Result := cxRectCenter(ARect, LayerSize);
  Result := cxRectOffset(Result, Offset);
  if TargetElement.Control.UseRightToLeftAlignment then
    Result := TdxRightToLeftLayoutConverter.ConvertRect(Result, AScreenBounds);
end;

function TdxBadgeViewInfo.CalculateLayerClientRect: TRect;
begin
  Result := cxRectSetNullOrigin(LayerBounds);
end;

function TdxBadgeViewInfo.CalculateTextAreaBounds: TRect;
begin
  Result := cxRectContent(Bounds, Adorner.TextMargins.Margin);
end;

procedure TdxBadgeViewInfo.CalculateVisibility;
begin
  FVisible := not cxRectIsEmpty(Bounds) and VisibleForParents;
end;

procedure TdxBadgeViewInfo.DoCalculate;
begin
  FColor := GetColor;
  FTextSize := GetTextSize;
  FHeight := GetHeight;
  FWidth := GetWidth;
  CalculateLayer;
  inherited DoCalculate;
  FDesignSelectorBounds := CalculateDesignSelectorBounds;
  FTextAreaBounds := CalculateTextAreaBounds;
  FDefaultGlyphBounds := CalculateDefaultGlyphBounds;
end;

function TdxBadgeViewInfo.HasGlyph: Boolean;
begin
  Result := not Glyph.Empty or ((Images <> nil) and (ImageINdex <> -1));
end;

function TdxBadgeViewInfo.HasText: Boolean;
begin
  Result := Text <> '';
end;

function TdxBadgeViewInfo.NeedColorizedGlyph: Boolean;
begin
  Result := Adorner.Background.Color <> clDefault;
end;

function TdxBadgeViewInfo.GetAdorner: TdxBadge;
begin
  Result := TdxBadge(inherited Adorner);
end;

function TdxBadgeViewInfo.GetColor: TColor;
begin
  Result := Adorner.Background.ActualColor;
end;

function TdxBadgeViewInfo.GetDefaultGlyph: TdxSmartGlyph;
begin
  Result := dxBadgeDefaultGlyph;
end;

function TdxBadgeViewInfo.GetDefaultGlyphMargin: TRect;
var
  ASize: TSize;
  AMargin: Integer;
begin
  Result := cxNullRect;
  AMargin := ScaleFactor.Apply(dxBadgeBackgroundImageMargin);
  ASize := ScaleFactor.Apply(DefaultGlyph.Size);
  if ASize.cx <= cxRectWidth(DefaultGlyphBounds) then
  begin
    Result.Left := AMargin;
    Result.Right := AMargin;
  end;
  if ASize.cy <= cxRectHeight(DefaultGlyphBounds) then
  begin
    Result.Top := AMargin;
    Result.Bottom := AMargin;
  end;
end;

function TdxBadgeViewInfo.GetFitMode: TcxImageFitMode;
begin
  Result := Adorner.Background.FitMode;
end;

function TdxBadgeViewInfo.GetFont: TFont;
begin
  Result := Adorner.Font;
end;

function TdxBadgeViewInfo.GetGlyph: TdxSmartGlyph;
begin
  Result := Adorner.Background.Glyph;
end;

function TdxBadgeViewInfo.GetHeight: Integer;
begin
  Result := Adorner.Size.Height;
  if Result = 0 then
  begin
    Result := Adorner.TextMargins.Top + TextSize.cy + Adorner.TextMargins.Bottom;
    if HasGlyph then
      Result := Max(Result, dxGetImageSize(Glyph, Images, ImageIndex, ScaleFactor).cy)
    else
      Result := Max(Result, ScaleFactor.Apply(DefaultGlyph.Height));
  end;
end;

function TdxBadgeViewInfo.GetImageIndex: TcxImageIndex;
begin
  Result := Adorner.Background.ImageIndex;
end;

function TdxBadgeViewInfo.GetImages: TCustomImageList;
begin
  Result := Manager.Images;
end;

function TdxBadgeViewInfo.GetLayerParent: TWinControl;
begin
  Result := TargetElement.Control;
end;

function TdxBadgeViewInfo.GetLayerSize: TSize;
begin
  Result := cxSize(Width, Height);
  if IsDesignSelected then
  begin
    Result.cx := Result.cx + 2 * cxDesignSelectionWidth;
    Result.cy := Result.cy + 2 * cxDesignSelectionWidth;
  end;
end;

function TdxBadgeViewInfo.GetOffset: TPoint;
begin
  Result := Adorner.Offset.Point;
end;

function TdxBadgeViewInfo.GetScaleFactor: TdxScaleFactor;
begin
  Result := Adorner.ScaleFactor;
end;

function TdxBadgeViewInfo.GetText: string;
begin
  Result := Adorner.Text;
end;

function TdxBadgeViewInfo.GetTextFlags: Cardinal;
begin
  Result := DT_CENTER or DT_VCENTER or DT_SINGLELINE or DT_END_ELLIPSIS or DT_NOPREFIX;
  if TargetElement.Control.UseRightToLeftReading then
    Result := Result or  DT_RTLREADING;
end;

function TdxBadgeViewInfo.GetTextSize: TSize;
begin
  Result := dxGPGetTextSize(Text, Font);
end;

function TdxBadgeViewInfo.GetTransparent: Boolean;
begin
  Result := Adorner.Background.Transparent;
end;

function TdxBadgeViewInfo.GetVisibleForParents: Boolean;
var
  AParent: TWinControl;
  ARect: TRect;
begin
  Result := True;
  AParent := TargetElement.Control.Parent;
  while Result and (AParent <> nil) do
  begin
    ARect := dxMapWindowRect(AParent.Handle, 0, AParent.ClientRect);
    if AParent.UseRightToLeftAlignment and dxWindowHasRightToLeftLayout(AParent.Handle) then
      ARect := cxRectOffsetHorz(ARect, -cxRectWidth(ARect));
    Result := cxRectIntersect(LayerBounds, ARect);
    AParent := AParent.Parent;
  end;
end;

function TdxBadgeViewInfo.GetWidth: Integer;
begin
  Result := Adorner.Size.Width;
  if Result = 0 then
  begin
    Result := Adorner.TextMargins.Left + TextSize.cx + Adorner.TextMargins.Right;
    if HasGlyph then
      Result := Max(Result, dxGetImageSize(Glyph, Images, ImageIndex, ScaleFactor).cx)
    else
      Result := Max(Result, ScaleFactor.Apply(DefaultGlyph.Width));
  end;
end;

{ TdxGuideViewInfo }

procedure TdxGuideViewInfo.TrackingMouseLeave;
begin
  MouseLeave;
end;

function TdxGuideViewInfo.PtInCaller(const APoint: TPoint): Boolean;
begin
  Result := HasPoint(APoint);
end;

function TdxGuideViewInfo.CreateHitTest(const APoint: TPoint): TdxAdornerCustomHitTest;
begin
  Result := TdxGuideHitTest.Instance(APoint);
end;

function TdxGuideViewInfo.CreatePainter(ACanvas: TdxGPCanvas): TdxCustomAdornerPainter;
begin
  Result := TdxGuidePainter.Create(Self, ACanvas);
end;

function TdxGuideViewInfo.CalculateBounds: TRect;
begin
  Result := cxRectInflate(MapTargetScreenBoundsToLayerBounds, BorderWidth);
end;

function TdxGuideViewInfo.CalculateExcludeBounds: TRect;
begin
  Result := cxRectInflate(Bounds, -BorderWidth)
end;

procedure TdxGuideViewInfo.DoCalculate;
begin
  FBorderColor := GetBorderColor;
  FBorderWidth := GetBorderWidth;
  inherited DoCalculate;
  FExcludeBounds := CalculateExcludeBounds;
end;

function TdxGuideViewInfo.DoGetHitTest(const APoint: TPoint): TdxAdornerCustomHitTest;
begin
  if VisibleForHitTest and HasPoint(APoint) then
    Result := CreateHitTest(APoint)
  else
    Result := nil;
  if Result is TdxGuideHitTest then
    InitHitTest(TdxGuideHitTest(Result));
end;

function TdxGuideViewInfo.GetHitTest(const APoint: TPoint): TdxAdornerCustomHitTest;
begin
  Result := DoGetHitTest(APoint);
end;

function TdxGuideViewInfo.GetHitTest(X, Y: Integer): TdxAdornerCustomHitTest;
begin
  Result := GetHitTest(Point(X, Y));
end;

function TdxGuideViewInfo.NeedExcludeFromClipRect: Boolean;
begin
  Result := Adorner.Selected;
end;

function TdxGuideViewInfo.HasPoint(const APoint: TPoint): Boolean;
begin
  Result := PtInRect(Bounds, APoint);
end;

procedure TdxGuideViewInfo.InitHitTest(AHitTest: TdxGuideHitTest);
begin
  AHitTest.ViewInfo := Self;
end;

function TdxGuideViewInfo.MapTargetScreenBoundsToLayerBounds: TRect;
var
  AOffset: TPoint;
begin
  AOffset := cxPointOffset(TargetElement.ScreenBounds.TopLeft, Guides.ViewInfo.LayerBounds.TopLeft, False);
  Result := cxRectSetOrigin(TargetElement.ScreenBounds, AOffset);
end;

procedure TdxGuideViewInfo.MouseDown(X, Y: Integer; AButton: TMouseButton; AShift: TShiftState);
begin
  inherited MouseDown(X, Y, AButton, AShift);
  Guides.Controller.Clicked := Adorner;
  Adorner.Selected := True;
  SetState(astPressed);
  Adorner.ToggleCalloutPopup;
end;

procedure TdxGuideViewInfo.MouseLeave;
begin
  if State in [astHotTracked, astPressed] then
    SetState(astNone);
end;

procedure TdxGuideViewInfo.MouseMove(X, Y: Integer; AShift: TShiftState);
begin
  if State <> astPressed then
    SetState(astHotTracked);
end;

procedure TdxGuideViewInfo.MouseUp(X, Y: Integer; AButton: TMouseButton; AShift: TShiftState);
begin
  if Guides.Controller.IsClicked(Adorner) then
    Click;
  Guides.Controller.Clicked := nil;
  SetState(astNone);
end;

procedure TdxGuideViewInfo.SetState(AValue: TdxAdornerState);
var
  APrevState: TdxAdornerState;
begin
  if State <> AValue then
  begin
    APrevState := State;
    FState := AValue;
    StateChanged(APrevState);
  end;
end;

procedure TdxGuideViewInfo.StateChanged(APrevState: TdxAdornerState);
begin
  Guides.UpdateLayer;
  case State of
    astNone:
      EndMouseTracking(Self);
    astHotTracked:
      BeginMouseTracking(Adorner.Adorners.LayeredWindow, Bounds, Self);
  end;
end;

function TdxGuideViewInfo.GetBorderGlyph: TdxSmartGlyph;
begin
  if Adorner.Selected then
    Result := dxGuideSelectedBorderGlyph
  else
    Result := dxGuideNormalBorderGlyph;
end;

function TdxGuideViewInfo.GetAdorner: TdxGuide;
begin
  Result := TdxGuide(inherited Adorner);
end;

function TdxGuideViewInfo.GetBorderColor: TColor;
begin
  Result := Adorner.ActualBorderColor;
end;

function TdxGuideViewInfo.GetBorderWidth: Integer;
begin
  if Adorner.Selected then
    Result := dxGuideSelectedBorderWidth
  else
    Result := dxGuideNormalBorderWidth;
end;

function TdxGuideViewInfo.GetGuides: TdxGuides;
begin
  Result := Adorner.Adorners;
end;

function TdxGuideViewInfo.GetVisibleForHitTest: Boolean;
begin
  Result := Visible;
end;

{ TdxGuideLayerViewInfo }

constructor TdxGuideLayerViewInfo.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FItems := TdxFastObjectList.Create;
  CreateItems;
end;

destructor TdxGuideLayerViewInfo.Destroy;
begin
  FreeAndNil(FItems);
  inherited Destroy;
end;
procedure TdxGuideLayerViewInfo.Calculate;
begin
  CalculateVisibility;
  if Visible then
    DoCalculate;
end;

procedure TdxGuideLayerViewInfo.Paint(ACanvas: TdxGPCanvas);
begin
  DoPaint(ACanvas);
end;

procedure TdxGuideLayerViewInfo.CreateItems;
var
  I: Integer;
begin
  for I := 0 to Guides.Count - 1 do
     FItems.Add(Guides[I].CreateViewInfo);
end;

function TdxGuideLayerViewInfo.CreateHitTest(const APoint: TPoint): TdxAdornerCustomHitTest;
begin
  Result := TdxAdornerNoneHitTest.Instance(APoint);
end;

function TdxGuideLayerViewInfo.CreatePainter(ACanvas: TdxGPCanvas): TdxGuideLayerPainter;
begin
  Result := TdxGuideLayerPainter.Create(Self, ACanvas);
end;

function TdxGuideLayerViewInfo.CalculateBounds: TRect;
begin
  Result := cxRectSetNullOrigin(LayerBounds);
end;

function TdxGuideLayerViewInfo.CalculateClipRect: TRect;
begin
  Result := Bounds;
end;

procedure TdxGuideLayerViewInfo.CalculateItems;
var
  I: Integer;
begin
  for I := 0 to ItemCount - 1 do
    Items[I].Calculate;
end;

function TdxGuideLayerViewInfo.CalculateLayerBounds: TRect;
begin
  Result := dxMapWindowRect(Manager.OwnerControl.Handle, 0, Manager.OwnerControl.ClientRect);
  if Manager.OwnerControl.UseRightToLeftAlignment and dxWindowHasRightToLeftLayout(Manager.OwnerControl.Handle) then
    Result := cxRectOffsetHorz(Result, -cxRectWidth(Result));
end;

procedure TdxGuideLayerViewInfo.CalculateVisibility;
begin
  FVisible := Guides.Active and IsControlVisible(Manager.OwnerControl) and not dxIsMainFormIconic;
end;

procedure TdxGuideLayerViewInfo.DoCalculate;
begin
  FLayerBounds := CalculateLayerBounds;
  FLayerParent := GetLayerParent;
  FBounds := CalculateBounds;
  FClipRect := CalculateClipRect;
  CalculateItems;
end;

function TdxGuideLayerViewInfo.DoGetHitTest(const APoint: TPoint): TdxAdornerCustomHitTest;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to ItemCount - 1 do
  begin
    Result := Items[I].GetHitTest(APoint);
    if Result <> nil then
      Exit;
  end;
  if Result = nil then
    Result := CreateHitTest(APoint);
end;

procedure TdxGuideLayerViewInfo.DoPaint(ACanvas: TdxGPCanvas);
var
  APainter: TdxGuideLayerPainter;
begin
  APainter := CreatePainter(ACanvas);
  try
    APainter.Paint;
  finally
    APainter.Free;
  end;
end;

function TdxGuideLayerViewInfo.FindGuideViewInfo(AGuide: TdxGuide): TdxGuideViewInfo;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to ItemCount - 1 do
    if Items[I].Adorner = AGuide then
    begin
      Result := Items[I];
      Exit;
    end;
end;

function TdxGuideLayerViewInfo.GetHitTest(const APoint: TPoint): TdxAdornerCustomHitTest;
begin
  Result := DoGetHitTest(APoint);
end;

function TdxGuideLayerViewInfo.GetHitTest(X, Y: Integer): TdxAdornerCustomHitTest;
begin
  Result := GetHitTest(Point(X, Y));
end;

function TdxGuideLayerViewInfo.GetGuides: TdxGuides;
begin
  Result := TdxGuides(Owner);
end;

function TdxGuideLayerViewInfo.GetItem(AIndex: Integer): TdxGuideViewInfo;
begin
  Result := TdxGuideViewInfo(FItems[AIndex]);
end;

function TdxGuideLayerViewInfo.GetItemCount: Integer;
begin
  Result := FItems.Count;
end;

function TdxGuideLayerViewInfo.GetLayerColor: TdxAlphaColor;
begin
  Result := Guides.ActualLayerColor;
end;

function TdxGuideLayerViewInfo.GetLayerParent: HWND;
begin
  Result := Manager.OwnerControl.Handle;
end;

function TdxGuideLayerViewInfo.GetManager: TdxUIAdornerManager;
begin
  Result := Guides.Manager;
end;

{ TdxCustomAdorner }

constructor TdxCustomAdorner.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTargetElementClass := GetDefaultTargetElementClass;
  RecreateTargetElement;
  FVisible := True;
  FScaleFactor := TdxOwnedScaleFactor.Create;
  ScaleFactor.ListenerAdd(ScaleFactorChangeHandler);
end;

destructor TdxCustomAdorner.Destroy;
begin
  ScaleFactor.ListenerRemove(ScaleFactorChangeHandler);
  FreeAndNil(FScaleFactor);
  FreeAndNil(FTargetElement);
  inherited Destroy;
end;

procedure TdxCustomAdorner.Assign(Source: TPersistent);
begin
  Manager.BeginUpdate;
  try
    DoAssign(Source);
  finally
    Manager.EndUpdate;
  end;
end;

procedure TdxCustomAdorner.CreateTargetElement;
begin
  FTargetElement := TargetElementClass.Create(Self)
end;

procedure TdxCustomAdorner.RecreateTargetElement;
begin
  FreeAndNil(FTargetElement);
  if TargetElementClass <> nil then
    CreateTargetElement;
end;

function TdxCustomAdorner.CanUpdate: Boolean;
begin
  Result := Adorners.CanUpdateLayer;
end;

procedure TdxCustomAdorner.Changed;
begin
  DoChanged;
  Update;
end;

procedure TdxCustomAdorner.ChangeScale(M, D: Integer);
begin
//do nothing
end;

procedure TdxCustomAdorner.CheckCover(AChangedHandle: HWND; ABounds, AOldBounds: TRect);
begin
  if Adorners.Active and Visible and HasTargetElement and TargetElement.IsCoverChanged(AChangedHandle, ABounds, AOldBounds) then
    Update;
end;

procedure TdxCustomAdorner.DoAssign(Source: TPersistent);
var
  ASource: TdxCustomAdorner;
begin
  if Source is TdxCustomAdorner then
  begin
    ASource := TdxCustomAdorner(Source);
    Tag := ASource.Tag;
    TargetElement := ASource.TargetElement;
    Visible := ASource.Visible;
    OnChanged := ASource.OnChanged;
    OnClick := ASource.OnClick;
    OnCustomDraw := ASource.OnCustomDraw;
    OnCustomDrawBackground := ASource.OnCustomDrawBackground;
  end;
end;

function TdxCustomAdorner.GetDefaultTargetElementClass: TdxAdornerCustomTargetElementClass;
begin
  Result := TdxAdornerTargetElementControl;
end;

function TdxCustomAdorner.GetDisplayName: string;
begin
  Result := inherited GetDisplayName;
  if GetTargetElementName <> '' then
    Result := Result + ' - ''' + GetTargetElementName + '''';
end;

function TdxCustomAdorner.GetManagerFromParent(AParent: TComponent): TdxUIAdornerManager;
begin
  Result := AParent as TdxUIAdornerManager;
end;

function TdxCustomAdorner.GetTargetElementName: string;
begin
  if HasTargetElement then
    Result := TargetElement.GetDisplayName
  else
    Result := '';
end;

function TdxCustomAdorner.HasTargetElement: Boolean;
begin
  Result := TargetElement <> nil;
end;

function TdxCustomAdorner.IsTargetElementClassNameStored: Boolean;
begin
  Result := TargetElementClass <> GetDefaultTargetElementClass;
end;

procedure TdxCustomAdorner.Update;
begin
  if CanUpdate then
    DoUpdate;
end;

procedure TdxCustomAdorner.DoChanged;
begin
  if Assigned(FOnChanged) then
    FOnChanged(Manager, Self)
  else
    Manager.DoAdornerChanged(Self);
end;

procedure TdxCustomAdorner.DoClick;
begin
  if Assigned(FOnClick) then
    FOnClick(Manager, Self)
  else
    Manager.DoAdornerClick(Self);
end;

procedure TdxCustomAdorner.DoCustomDraw(AViewInfo: TdxCustomAdornerViewInfo; ACanvas: TdxGPCanvas; var ADone: Boolean);
begin
  if Assigned(FOnCustomDraw) then
    FOnCustomDraw(Manager, Self, ACanvas, AViewInfo, ADone);
  if not ADone then
    Manager.DoAdornerCustomDraw(Self, AViewInfo, ACanvas, ADone);
end;

procedure TdxCustomAdorner.DoCustomDrawBackground(AViewInfo: TdxCustomAdornerViewInfo; ACanvas: TdxGPCanvas; var ADone: Boolean);
begin
  if Assigned(FOnCustomDrawBackground) then
    FOnCustomDrawBackground(Manager, Self, ACanvas, AViewInfo, ADone);
  if not ADone then
    Manager.DoAdornerCustomDrawBackground(Self, AViewInfo, ACanvas, ADone);
end;

function TdxCustomAdorner.GetAdorners: TdxCustomAdorners;
begin
  Result := TdxCustomAdorners(Collection);
end;

function TdxCustomAdorner.GetManager: TdxUIAdornerManager;
begin
  Result := Adorners.Manager;
end;

function TdxCustomAdorner.GetScaleFactor: TdxScaleFactor;
begin
  Result := FScaleFactor;
end;

function TdxCustomAdorner.GetTargetElementClassName: string;
begin
  Result := GetTargetElementClassName(TargetElement);
end;

function TdxCustomAdorner.GetTargetElementClassName(AValue: TdxAdornerCustomTargetElement): string;
begin
  if AValue <> nil then
    Result := AValue.ClassName
  else
    Result := '';
end;

function TdxCustomAdorner.GetTargetElementVisible: Boolean;
begin
  Result := HasTargetElement and TargetElement.Visible;
end;

procedure TdxCustomAdorner.ScaleFactorChangeHandler(Sender: TObject; M, D: Integer; IsLoading: Boolean);
begin
  if not IsLoading then
    ChangeScale(M, D);
end;

procedure TdxCustomAdorner.SetTargetElement(AValue: TdxAdornerCustomTargetElement);
begin
  TargetElementClassName := GetTargetElementClassName(AValue);
  if HasTargetElement then
    TargetElement.Assign(AValue);
end;

procedure TdxCustomAdorner.SetTargetElementClass(AValue: TdxAdornerCustomTargetElementClass);
begin
  if AValue = nil then
    AValue := GetDefaultTargetElementClass;
  if TargetElementClass <> AValue then
  begin
    FTargetElementClass := AValue;
    RecreateTargetElement;
    Changed;
  end;
end;

procedure TdxCustomAdorner.SetTargetElementClassName(const AValue: string);
begin
  if not SameText(TargetElementClassName, AValue) then
    TargetElementClass := TdxAdornerCustomTargetElementClass(dxRegisteredAdornerTargetElementTypes.FindByClassName(AValue));
end;

procedure TdxCustomAdorner.SetVisible(AValue: Boolean);
begin
  if Visible <> AValue then
  begin
    FVisible := AValue;
    Changed;
  end;
end;

procedure TdxCustomAdorner.SetOnChanged(AValue: TdxAdornerNotifyEvent);
begin
  if not dxSameMethods(FOnChanged, AValue) then
  begin
    FOnChanged := AValue;
    Changed;
  end;
end;

procedure TdxCustomAdorner.SetOnClick(AValue: TdxAdornerNotifyEvent);
begin
  if not dxSameMethods(FOnClick, AValue) then
  begin
    FOnClick := AValue;
    Changed;
  end;
end;

procedure TdxCustomAdorner.SetOnCustomDraw(AValue: TdxAdornerCustomDrawEvent);
begin
  if not dxSameMethods(FOnCustomDraw, AValue) then
  begin
    FOnCustomDraw := AValue;
    Changed;
  end;
end;

procedure TdxCustomAdorner.SetOnCustomDrawBackground(AValue: TdxAdornerCustomDrawEvent);
begin
  if not dxSameMethods(FOnCustomDrawBackground, AValue) then
  begin
    FOnCustomDrawBackground := AValue;
    Changed;
  end;
end;

{ TdxBadge }

constructor TdxBadge.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLayeredWindow := CreateLayeredWindow;
  FAlignment := CreateAlignment;
  FBackground := CreateBackground;
  FFont := CreateFont;
  FOffset := CreateOffset;
  FSize := CreateSize;
  FTextMargins := CreateTextMargins;
  FViewInfo := CreateViewInfo;

  FParentFont := True;

  Font.OnChange := FontChangeHandler;
  TcxEditAlignmentAccess(Alignment).OnChanged := ChangeHandler;
  Offset.OnChange := ChangeHandler;
  Size.OnChange := ChangeHandler;
  TextMargins.OnChange := ChangeHandler;
end;

destructor TdxBadge.Destroy;
begin
  FreeAndNil(FViewInfo);
  FreeAndNil(FTextMargins);
  FreeAndNil(FSize);
  FreeAndNil(FOffset);
  FreeAndNil(FFont);
  FreeAndNil(FBackground);
  FreeAndNil(FAlignment);
  inherited Destroy;
  FreeAndNil(FLayeredWindow);
end;

procedure TdxBadge.SetParentComponent(Value: TComponent);
begin
  inherited SetParentComponent(Value);
  ParentFontChanged;
end;

function TdxBadge.CreateAlignment: TcxEditAlignment;
begin
  Result := TcxEditAlignment.Create(Self, dxBadgeDefaultHorz, cxEditDefaultVertAlignment);
end;

function TdxBadge.CreateBackground: TdxBadgeBackground;
begin
  Result := TdxBadgeBackground.Create(Self);
end;

function TdxBadge.CreateFont: TFont;
begin
  Result := TFont.Create;
end;

function TdxBadge.CreateLayeredWindow: TdxBadgeLayeredWindow;
begin
  Result := TdxBadgeLayeredWindow.Create(Self);
end;

function TdxBadge.CreateOffset: TdxPoint;
begin
  Result := TdxPoint.Create(nil);
end;

function TdxBadge.CreateSize: TcxSize;
begin
  Result := TcxSize.Create(nil);
end;

function TdxBadge.CreateTextMargins: TcxMargin;
begin
  Result := TcxMargin.Create(nil, ScaleFactor.Apply(dxBadgeTextDefaultMargin));
end;

function TdxBadge.CreateViewInfo: TdxBadgeViewInfo;
begin
  Result := TdxBadgeViewInfo.Create(Self);
end;

procedure TdxBadge.ChangeScale(M, D: Integer);
begin
  inherited;
  Offset.ChangeScale(M, D);
  Size.ChangeScale(M, D);
  Font.Height := MulDiv(Font.Height, M, D);
  TextMargins.ChangeScale(M, D);
  ViewInfo.Calculate;
end;

procedure TdxBadge.CheckPosition(AChangedHandle: HWND);
begin
  if Adorners.Active and Visible and HasTargetElement and TargetElement.IsPositionChanged(AChangedHandle, ViewInfo.Visible) then
    Update;
end;

procedure TdxBadge.DoAssign(Source: TPersistent);
var
  ASource: TdxBadge;
begin
  inherited DoAssign(Source);
  if Source is TdxBadge then
  begin
    ASource := TdxBadge(Source);
    Alignment := ASource.Alignment;
    Background := ASource.Background;
    Offset := ASource.Offset;
    ParentFont := ASource.ParentFont;
    if not ParentFont then
      Font := ASource.Font;
    Size := ASource.Size;
    Text := ASource.Text;
    TextMargins := ASource.TextMargins;
  end;
end;

procedure TdxBadge.DoUpdate;
begin
  UpdateLayeredWindow;
end;

function TdxBadge.GetCollectionFromParent(AParent: TComponent): TcxComponentCollection;
begin
  Result := GetManagerFromParent(AParent).Badges;
end;

procedure TdxBadge.Notification(Sender: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (Sender = OwnedScaledControl) then
    OwnedScaledControl := nil;
end;

procedure TdxBadge.ParentFontChanged;
begin
  if not ParentFont then
    Exit;
  FIsParentFontAssigning := True;
  try
    Font := Adorners.Font;
  finally
    FIsParentFontAssigning := False;
  end;
end;

procedure TdxBadge.UpdateLayeredWindow;
begin
  ViewInfo.Calculate;
  if LayeredWindow.Visible then
    OwnedScaledControl := ViewInfo.LayerParent;
  LayeredWindow.Update;
end;

function TdxBadge.GetAdorners: TdxBadges;
begin
  Result := TdxBadges(inherited Adorners);
end;

function TdxBadge.IsFontStored: Boolean;
begin
  Result := not ParentFont;
end;

function TdxBadge.IsTextStored: Boolean;
begin
  Result := Text <> '';
end;

procedure TdxBadge.SetAlignment(AValue: TcxEditAlignment);
begin
  Alignment.Assign(AValue);
end;

procedure TdxBadge.SetBackground(AValue: TdxBadgeBackground);
begin
  Background.Assign(AValue);
end;

procedure TdxBadge.SetFont(AValue: TFont);
begin
  Font.Assign(AValue);
end;

procedure TdxBadge.SetOffset(AValue: TdxPoint);
begin
  Offset.Assign(AValue);
end;

procedure TdxBadge.SetOwnedScaledControl(AValue: TWinControl);

  procedure FindOwnedScaleFactor(out AControl: TWinControl; out AOwnedScaleFactor: IdxScaleFactor);
  begin
    AControl := AValue;
    if AControl <> nil then
    begin
      AControl := GetParentForm(AValue);
      if AControl = nil then
        AControl := AValue;
    end;
    while not Supports(AControl, IdxScaleFactor, AOwnedScaleFactor) and (AControl <> nil) do
      AControl := AControl.Parent;
  end;

var
  AControl: TWinControl;
  AOwnedScaleFactor: IdxScaleFactor;
begin
  FindOwnedScaleFactor(AControl, AOwnedScaleFactor);
  if FOwnedScaledControl <> AControl then
  begin
    if FOwnedScaledControl <> nil then
    begin
      FOwnedScaledControl.RemoveFreeNotification(Self);
      FOwnedScaledControl := nil;
      ScaleFactor.Owner := nil;
    end;
    if AControl <> nil then
    begin
      FOwnedScaledControl := AControl;
      FOwnedScaledControl.FreeNotification(Self);
      ScaleFactor.Owner := AOwnedScaleFactor.Value;
    end;
  end;
end;

procedure TdxBadge.SetParentFont(AValue: Boolean);
begin
  if ParentFont <> AValue then
  begin
    FParentFont := AValue;
    ParentFontChanged;
    Changed;
  end;
end;

procedure TdxBadge.SetSize(AValue: TcxSize);
begin
  Size.Assign(AValue);
end;

procedure TdxBadge.SetText(const AValue: string);
begin
  if Text <> AValue then
  begin
    FText := AValue;
    Changed;
  end;
end;

procedure TdxBadge.SetTextMargins(AValue: TcxMargin);
begin
  TextMargins.Assign(AValue);
end;

procedure TdxBadge.ChangeHandler(Sender: TObject);
begin
  Changed;
end;

procedure TdxBadge.FontChangeHandler(Sender: TObject);
begin
  if not FIsParentFontAssigning then
    ParentFont := False;
  Changed;
end;

{ TdxGuide }

constructor TdxGuide.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAllowCalloutPopup := True;
  FBorderColor := clDefault;
  FCalloutPopupAlignment := cpaBottomCenter;
end;

function TdxGuide.CreateViewInfo: TdxGuideViewInfo;
begin
  Result := TdxGuideViewInfo.Create(Self);
end;

function TdxGuide.CanSelect: Boolean;
begin
  Result := (ViewInfo <> nil) and ViewInfo.Visible;
end;

procedure TdxGuide.DoAssign(Source: TPersistent);
var
  ASource: TdxGuide;
begin
  inherited DoAssign(Source);
  if Source is TdxGuide then
  begin
    ASource := TdxGuide(Source);
    AllowCalloutPopup := ASource.AllowCalloutPopup;
    BorderColor := ASource.BorderColor;
    CalloutPopupAlignment := ASource.CalloutPopupAlignment;
    TabOrder := ASource.TabOrder;
    OnGetCalloutPopupControl := ASource.OnGetCalloutPopupControl;
  end;
end;

procedure TdxGuide.DoUpdate;
begin
  Adorners.UpdateLayer;
end;

function TdxGuide.GetCollectionFromParent(AParent: TComponent): TcxComponentCollection;
begin
  Result := GetManagerFromParent(AParent).Guides;
end;

procedure TdxGuide.ToggleCalloutPopup;
begin
  Adorners.Controller.ToggleCalloutPopup(Self);
end;

procedure TdxGuide.DoGetCalloutPopupControl(out AControl: TWinControl);
begin
  if Assigned(FOnGetCalloutPopupControl) then
    FOnGetCalloutPopupControl(Manager, Self, AControl)
  else
    Manager.DoGuideGetCalloutPopupControl(Self, AControl);
end;

function TdxGuide.GetActualBorderColor: TColor;
begin
  if BorderColor = clDefault then
    Result := Adorners.ActualBorderColor
  else
    Result := BorderColor;
end;

function TdxGuide.GetAdorners: TdxGuides;
begin
  Result := TdxGuides(inherited Adorners);
end;

function TdxGuide.GetCalloutPopupControl: TWinControl;
begin
  Result := nil;
  DoGetCalloutPopupControl(Result);
end;

function TdxGuide.GetSelected: Boolean;
begin
  Result := Adorners.Controller.Selected = Self;
end;

function TdxGuide.GetViewInfo: TdxGuideViewInfo;
begin
  Result := Adorners.ViewInfo.FindGuideViewInfo(Self);
end;

procedure TdxGuide.SetAllowCalloutPopup(AValue: Boolean);
begin
  if AllowCalloutPopup <> AValue then
  begin
    FAllowCalloutPopup := AValue;
    Changed;
  end;
end;

procedure TdxGuide.SetBorderColor(AValue: TColor);
begin
  if BorderColor <> AValue then
  begin
    FBorderColor := AValue;
    Changed;
  end;
end;

procedure TdxGuide.SetCalloutPopupAlignment(AValue: TdxCalloutPopupAlignment);
begin
  if CalloutPopupAlignment <> AValue then
  begin
    FCalloutPopupAlignment := AValue;
    Changed;
  end;
end;

procedure TdxGuide.SetSelected(AValue: Boolean);
begin
  if AValue then
    Adorners.Controller.Selected := Self
  else
    Adorners.Controller.ClearSelection;
end;

procedure TdxGuide.SetTabOrder(AValue: TTabOrder);
begin
  if TabOrder <> AValue then
  begin
    FTabOrder := AValue;
    Changed;
  end;
end;

procedure TdxGuide.SetOnGetCalloutPopupControl(AValue: TdxGuideGetCalloutPopupControl);
begin
  if not dxSameMethods(FOnGetCalloutPopupControl, AValue) then
  begin
    FOnGetCalloutPopupControl := AValue;
    Changed;
  end;
end;

{ TdxGuideController }

constructor TdxGuideController.Create(AGuides: TdxGuides);
begin
  inherited Create;
  FGuides := AGuides;
  FCalloutPopup := CreateCalloutPopup;
end;

destructor TdxGuideController.Destroy;
begin
  FreeAndNil(FCalloutPopup);
  inherited Destroy;
end;

procedure TdxGuideController.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  AHitTest: TdxAdornerCustomHitTest;
begin
  AHitTest := Guides.ViewInfo.GetHitTest(X, Y);
  if AHitTest is TdxGuideHitTest then
    TdxGuideHitTest(AHitTest).ViewInfo.MouseDown(AHitTest.Pos.X, AHitTest.Pos.Y, Button, Shift)
  else
    CalloutPopup.Close;
end;

procedure TdxGuideController.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  AHitTest: TdxAdornerCustomHitTest;
begin
  AHitTest := Guides.ViewInfo.GetHitTest(X, Y);
  if AHitTest is TdxGuideHitTest then
    TdxGuideHitTest(AHitTest).ViewInfo.MouseMove(AHitTest.Pos.X, AHitTest.Pos.Y, Shift);
end;

procedure TdxGuideController.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  AHitTest: TdxAdornerCustomHitTest;
begin
  AHitTest := Guides.ViewInfo.GetHitTest(X, Y);
  if AHitTest is TdxGuideHitTest then
    TdxGuideHitTest(AHitTest).ViewInfo.MouseUp(AHitTest.Pos.X, AHitTest.Pos.Y, Button, Shift);
end;

procedure TdxGuideController.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (Guides.DeactivateKey <> scNone) and (Guides.DeactivateKey = ShortCut(Key, Shift)) then
  begin
    Guides.Active := False;
    Exit;
  end;
  if Manager.IsDesigning then
    Exit;
  case Key of
    VK_RIGHT, VK_DOWN:
      if Guides.AllowArrowKeyNavigation then
        SelectNext;
    VK_LEFT, VK_UP:
      if Guides.AllowArrowKeyNavigation then
        SelectNext(False);
    VK_TAB:
      if Guides.AllowTabKeyNavigation then
        SelectNext(Shift <> [ssShift]);
  end;
end;

procedure TdxGuideController.KeyPress(var Key: Char);
begin
//do nothing
end;

procedure TdxGuideController.KeyUp(var Key: Word; Shift: TShiftState);
begin
//do nothing
end;

procedure TdxGuideController.CheckSelection;
begin
  if Selected = nil then
    SelectFirst;
end;

procedure TdxGuideController.ClearSelection;
begin
  Selected := nil;
end;

function TdxGuideController.CreateCalloutPopup: TdxCalloutPopup;
begin
  Result := TdxCalloutPopup.Create(nil);
end;

function TdxGuideController.CompareTabOrder(AItem1, AItem2: Pointer): Integer;
var
  AGuide1, AGuide2: TdxGuide;
begin
  AGuide1 := TdxGuide(AItem1);
  AGuide2 := TdxGuide(AItem2);
  if AGuide1.TabOrder < AGuide2.TabOrder  then
    Result := -1
  else
    if AGuide1.TabOrder > AGuide2.TabOrder then
      Result := 1
    else
      Result := 0;
end;

function TdxGuideController.FindNext(AGuide: TdxGuide; AGoForward: Boolean = True): TdxGuide;
var
  AIndex: Integer;
  AList: TdxFastObjectList;
begin
  Result := nil;
  AList := TdxFastObjectList.Create(False);
  try
    PopulateTabOrderList(AList);
    if AList.Count = 0 then
      Exit;
    AList.Sort(CompareTabOrder);
    AIndex := AList.IndexOf(AGuide);
    if AGoForward then
      if AIndex = AList.Count - 1 then
        AIndex := 0
      else
        Inc(AIndex)
    else
      if (AIndex = 0) or (AIndex = -1) then
        AIndex := AList.Count - 1
      else
        Dec(AIndex);
    Result := TdxGuide(AList[AIndex]);
  finally
    AList.Free;
  end;
end;

function TdxGuideController.IsClicked(AValue: TdxGuide): Boolean;
begin
  Result := Clicked = AValue;
end;

procedure TdxGuideController.PopulateTabOrderList(AList: TdxFastObjectList);
var
  I: Integer;
begin
  for I := 0 to Guides.Count - 1 do
    if Guides[I].CanSelect and (Guides[I].TabOrder <> -1) then
      AList.Add(Guides[I]);
end;

procedure TdxGuideController.SelectFirst;
begin
  SelectNext(nil);
end;

procedure TdxGuideController.SelectNext(AGoForward: Boolean = True);
begin
  SelectNext(Selected, AGoForward);
end;

procedure TdxGuideController.SelectNext(AGuide: TdxGuide; AGoForward: Boolean = True);
var
  ANext: TdxGuide;
begin
  ANext := FindNext(AGuide, AGoForward);
  if ANext <> nil then
    ANext.Selected := True;
end;

procedure TdxGuideController.ShowCalloutPopup(AForGuide: TdxGuide);
begin
  if AForGuide.ViewInfo <> nil then
  begin
    CalloutPopup.Alignment := AForGuide.CalloutPopupAlignment;
    CalloutPopup.PopupControl := AForGuide.CalloutPopupControl;
    Guides.LayeredWindow.ScaleFactor.Assign(dxGetScaleFactor(Guides.LayeredWindow.OwnerControl));
    Guides.LayeredWindow.BiDiMode := Guides.LayeredWindow.OwnerControl.BiDiMode;
    CalloutPopup.Popup(Guides.LayeredWindow, AForGuide.ViewInfo.Bounds);
  end;
end;

procedure TdxGuideController.ToggleCalloutPopup;
begin
  ToggleCalloutPopup(Selected);
end;

procedure TdxGuideController.ToggleCalloutPopup(AGuide: TdxGuide);
begin
  if not Manager.IsDesigning and AGuide.AllowCalloutPopup then
    if CalloutPopup.IsVisible then
      CalloutPopup.Close
    else
      ShowCalloutPopup(AGuide)
end;

function TdxGuideController.GetManager: TdxUIAdornerManager;
begin
  Result := Guides.Manager;
end;

procedure TdxGuideController.SetSelected(AValue: TdxGuide);
begin
  if ((AValue = nil) or AValue.CanSelect) and (Selected <> AValue) then
  begin
    CalloutPopup.Close;
    FSelected := AValue;
    Guides.UpdateLayer;
  end;
end;

{ TdxCustomAdorners }

constructor TdxCustomAdorners.Create(AManager: TdxUIAdornerManager);
begin
  inherited Create(AManager, GetItemClass);
end;

procedure TdxCustomAdorners.Assign(Source: TPersistent);
var
  ASource: TdxCustomAdorners;
begin
  inherited Assign(Source);
  if Source is TdxCustomAdorners then
  begin
    ASource := TdxCustomAdorners(Source);
    Active := ASource.Active;
  end;
end;

procedure TdxCustomAdorners.ActiveChanged;
begin
  Changed;
  Manager.DoActiveChanged(Self);
end;

function TdxCustomAdorners.CanUpdateLayer: Boolean;
begin
  Result := (Active or IsActiveChanged) and Manager.CanUpdate;
end;

procedure TdxCustomAdorners.CheckAdornerCovers(AChangedHandle: HWND; ABounds, AOldBounds: TRect);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].CheckCover(AChangedHandle, ABounds, AOldBounds);
end;

procedure TdxCustomAdorners.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Items[I].Owner = Root then
      Proc(Items[I]);
end;

function TdxCustomAdorners.GetItemPrefixName: string;
begin
  Result := 'Tdx';
end;

procedure TdxCustomAdorners.TargetControlFreed(ATargetControl: TControl);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Items[I].HasTargetElement then
      Items[I].TargetElement.TargetControlFreed(ATargetControl);
end;

procedure TdxCustomAdorners.Update(AItem: TcxComponentCollectionItem; AAction: TcxComponentCollectionNotification);
begin
  inherited Update(AItem, AAction);
  UpdateLayer;
end;

procedure TdxCustomAdorners.UpdateLayer;
begin
  if CanUpdateLayer then
    DoUpdateLayer;
end;

function TdxCustomAdorners.GetItem(Index: Integer): TdxCustomAdorner;
begin
  Result := TdxCustomAdorner(inherited Items[Index]);
end;

function TdxCustomAdorners.GetManager: TdxUIAdornerManager;
begin
  Result := TdxUIAdornerManager(ParentComponent);
end;

procedure TdxCustomAdorners.SetActive(AValue: Boolean);
begin
  if Active <> AValue then
  begin
    FActive := AValue;
    SetDesignerModified(Manager);
    FIsActiveChanged := True;
    try
      ActiveChanged;
    finally
      FIsActiveChanged := False;
    end;
  end;
end;

procedure TdxCustomAdorners.SetItem(Index: Integer; AValue: TdxCustomAdorner);
begin
  inherited Items[Index] := AValue;
end;

{ TdxBadges }

constructor TdxBadges.Create(AManager: TdxUIAdornerManager);
begin
  inherited Create(AManager);
  FFont := CreateFont;
  FOwnerControlWndProcObject := cxWindowProcController.Add(Manager.OwnerControl, OwnerWindowWndProc);

  FColor := clDefault;
  ParentFont := True;

  Font.OnChange := FontChangeHandler;
end;

destructor TdxBadges.Destroy;
begin
  cxWindowProcController.Remove(FOwnerControlWndProcObject);
  FreeAndNil(FFont);
  inherited Destroy;
end;

procedure TdxBadges.Assign(Source: TPersistent);
var
  ASource: TdxBadges;
begin
  inherited Assign(Source);
  if Source is TdxBadges then
  begin
    ASource := TdxBadges(Source);
    Color := ASource.Color;
    ParentFont := ASource.ParentFont;
    if not ParentFont then
      Font := ASource.Font;
  end;
end;

function TdxBadges.Add: TdxBadge;
begin
  Result := TdxBadge(inherited Add);
end;

function TdxBadges.FindItemByID(ID: Integer): TdxBadge;
begin
  Result := TdxBadge(inherited FindItemByID(ID));
end;

function TdxBadges.IndexOf(AItem: TdxBadge): Integer;
begin
  Result := inherited IndexOf(AItem);
end;

function TdxBadges.Insert(Index: Integer): TdxBadge;
begin
  Result := TdxBadge(inherited Insert(Index));
end;

procedure TdxBadges.Remove(AItem: TdxBadge);
begin
  inherited Remove(AItem);
end;

function TdxBadges.CreateFont: TFont;
begin
  Result := TFont.Create;
end;

procedure TdxBadges.CheckAdornerPositions(AChangedHandle: HWND);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].CheckPosition(AChangedHandle);
end;

procedure TdxBadges.DoUpdateLayer;
begin
  UpdateBadges;
end;

function TdxBadges.GetItemClass: TcxComponentCollectionItemClass;
begin
  Result := TdxBadge;
end;

function TdxBadges.GetName: string;
begin
  Result := 'Badges';
end;

procedure TdxBadges.ItemsParentFontChanged;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].ParentFontChanged;
end;

procedure TdxBadges.ParentFontChanged;
begin
  if not ParentFont then
    Exit;
  FIsParentFontAssigning := True;
  try
    Font := ParentFontInstance;
  finally
    FIsParentFontAssigning := False;
  end;
end;

procedure TdxBadges.UpdateBadges;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].Update;
end;

function TdxBadges.GetActualColor: TColor;
begin
  if Color = clDefault then
    Result := dxBadgeDefaultColor
  else
    Result := Color;
end;

function TdxBadges.GetItem(Index: Integer): TdxBadge;
begin
  Result := TdxBadge(inherited Items[Index]);
end;

function TdxBadges.GetParentFontInstance: TFont;
begin
  Result := TControlAccess(Manager.OwnerControl).Font;
end;

function TdxBadges.IsFontStored: Boolean;
begin
  Result := not ParentFont;
end;

procedure TdxBadges.SetColor(AValue: TColor);
begin
  if Color <> AValue then
  begin
    FColor := AValue;
    Changed;
  end;
end;

procedure TdxBadges.SetFont(AValue: TFont);
begin
  Font.Assign(AValue);
end;

procedure TdxBadges.SetItem(Index: Integer; AValue: TdxBadge);
begin
  inherited Items[Index] := AValue;
end;

procedure TdxBadges.SetParentFont(AValue: Boolean);
begin
  if ParentFont <> AValue then
  begin
    FParentFont := AValue;
    ParentFontChanged;
    Changed;
  end;
end;

procedure TdxBadges.FontChangeHandler(Sender: TObject);
begin
  if not FIsParentFontAssigning then
    ParentFont := False;
  ItemsParentFontChanged;
  Changed;
end;

procedure TdxBadges.OwnerWindowWndProc(var AMessage: TMessage);
begin
  if AMessage.Msg = CM_FONTCHANGED then
    ParentFontChanged;
  OwnerControlWndProcObject.DefaultProc(AMessage);
end;

{ TdxGuides }

constructor TdxGuides.Create(AManager: TdxUIAdornerManager);
begin
  inherited Create(AManager);
  FLayeredWindow := CreateLayeredWindow;
  FController := CreateController;
  FViewInfo := CreateViewInfo;

  FAllowArrowKeyNavigation := True;
  FAllowTabKeyNavigation := True;
  FBorderColor := clDefault;
  FDeactivateKey := TextToShortCut(dxGuidesDefaultDeactivateKey);
  FLayerColor := TdxAlphaColors.Default;
end;

destructor TdxGuides.Destroy;
begin
  FreeAndNil(FLayeredWindow);
  FreeAndNil(FViewInfo);
  FreeAndNil(FController);
  inherited Destroy;
end;

procedure TdxGuides.Assign(Source: TPersistent);
var
  ASource: TdxGuides;
begin
  inherited Assign(Source);
  if Source is TdxGuides then
  begin
    ASource := TdxGuides(Source);
    AllowArrowKeyNavigation := ASource.AllowArrowKeyNavigation;
    AllowTabKeyNavigation := ASource.AllowTabKeyNavigation;
    BorderColor := ASource.BorderColor;
    DeactivateKey := ASource.DeactivateKey;
    LayerColor := ASource.LayerColor;
  end;
end;

function TdxGuides.Add: TdxGuide;
begin
  Result := TdxGuide(inherited Add);
end;

function TdxGuides.FindItemByID(ID: Integer): TdxGuide;
begin
  Result := TdxGuide(inherited FindItemByID(ID));
end;

function TdxGuides.IndexOf(AItem: TdxGuide): Integer;
begin
  Result := inherited IndexOf(AItem);
end;

function TdxGuides.Insert(Index: Integer): TdxGuide;
begin
  Result := TdxGuide(inherited Insert(Index));
end;

procedure TdxGuides.Remove(AItem: TdxGuide);
begin
  inherited Remove(AItem);
end;

function TdxGuides.CreateController: TdxGuideController;
begin
  Result := TdxGuideController.Create(Self);
end;

function TdxGuides.CreateLayeredWindow: TdxGuideLayeredWindow;
begin
  Result := TdxGuideLayeredWindow.Create(Self);
end;

function TdxGuides.CreateViewInfo: TdxGuideLayerViewInfo;
begin
  Result := TdxGuideLayerViewInfo.Create(Self);
end;

procedure TdxGuides.ActiveChanged;
begin
  if not Active then
    Controller.ClearSelection;
  inherited ActiveChanged;
end;

procedure TdxGuides.CheckAdornerPositions(AChangedHandle: HWND);
begin
  if Active and Manager.OwnerControl.HandleAllocated and ((AChangedHandle = Manager.OwnerControl.Handle) or
    IsChild(AChangedHandle, Manager.OwnerControl.Handle) or IsChild(Manager.OwnerControl.Handle, AChangedHandle)) then
    UpdateLayer;
end;

procedure TdxGuides.DoUpdateLayer;
begin
  UpdateLayeredWindow;
end;

function TdxGuides.GetItemClass: TcxComponentCollectionItemClass;
begin
  Result := TdxGuide;
end;

function TdxGuides.GetName: string;
begin
  Result := 'Guides';
end;

function TdxGuides.IsDeactivateKeyStored: Boolean;
begin
  Result := DeactivateKey <> TextToShortCut(dxGuidesDefaultDeactivateKey);
end;

procedure TdxGuides.RecreateViewInfo;
begin
  FreeAndNil(FViewInfo);
  FViewInfo := CreateViewInfo;
end;

procedure TdxGuides.ToggleCalloutPopup;
begin
  Controller.ToggleCalloutPopup;
end;

procedure TdxGuides.Update(AItem: TcxComponentCollectionItem; AAction: TcxComponentCollectionNotification);
begin
  RecreateViewInfo;
  inherited Update(AItem, AAction);
end;

procedure TdxGuides.UpdateLayeredWindow;
begin
  ViewInfo.Calculate;
  LayeredWindow.Update;
  if LayeredWindow.Visible then
    Controller.CheckSelection;
end;

function TdxGuides.GetActualBorderColor: TColor;
begin
  if BorderColor = clDefault then
    Result := dxGuideDefaultBorderColor
  else
    Result := BorderColor;
end;

function TdxGuides.GetActualLayerColor: TdxAlphaColor;
begin
  if LayerColor = TdxAlphaColors.Default then
    Result := dxColorToAlphaColor(clBlack, dxGuideLayerDefaultOpacity)
  else
    Result := LayerColor;
end;

function TdxGuides.GetItem(Index: Integer): TdxGuide;
begin
  Result := TdxGuide(inherited Items[Index]);
end;

function TdxGuides.IsLayerColorStored: Boolean;
begin
  Result := LayerColor <> TdxAlphaColors.Default;
end;

procedure TdxGuides.SetAllowArrowKeyNavigation(AValue: Boolean);
begin
  if AllowArrowKeyNavigation <> AValue then
  begin
    FAllowArrowKeyNavigation := AValue;
    Changed;
  end;
end;

procedure TdxGuides.SetAllowTabKeyNavigation(AValue: Boolean);
begin
  if AllowTabKeyNavigation <> AValue then
  begin
    FAllowTabKeyNavigation := AValue;
    Changed;
  end;
end;

procedure TdxGuides.SetBorderColor(AValue: TColor);
begin
  if BorderColor <> AValue then
  begin
    FBorderColor := AValue;
    Changed;
  end;
end;

procedure TdxGuides.SetDeactivateKey(AValue: TShortCut);
begin
  if DeactivateKey <> AValue then
  begin
    FDeactivateKey := AValue;
    Changed;
  end;
end;

procedure TdxGuides.SetItem(Index: Integer; AValue: TdxGuide);
begin
  inherited Items[Index] := AValue;
end;

procedure TdxGuides.SetLayerColor(AValue: TdxAlphaColor);
begin
  if LayerColor <> AValue then
  begin
    FLayerColor := AValue;
    Changed;
  end;
end;

{ TdxUIAdornerManager }

constructor TdxUIAdornerManager.Create(AOwner: TComponent);
begin
  FIsCreating := True;
  if not (AOwner is TWinControl) then
    raise EdxException.Create(cxGetResourceString(@sdxUIAdornerManagerBadOwner));
  FIsCreating := False;
  inherited Create(AOwner);
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;
  FBadges := CreateBadges;
  FGuides := CreateGuides;
  dxRegisterManager(Self);
end;

destructor TdxUIAdornerManager.Destroy;
begin
  if not IsCreating then
  begin
    dxUnregisterManager(Self);
    FreeAndNil(FGuides);
    FreeAndNil(FBadges);
    FreeAndNil(FDesignHelper);
    FreeAndNil(FImageChangeLink);
  end;
  inherited Destroy;
end;

procedure TdxUIAdornerManager.Assign(Source: TPersistent);
begin
  BeginUpdate;
  try
    DoAssign(Source);
  finally
    EndUpdate;
  end;
end;

procedure TdxUIAdornerManager.BeginUpdate;
begin
  Inc(FLockCount);
end;

procedure TdxUIAdornerManager.EndUpdate;
begin
  Dec(FLockCount);
  Update;
end;

function TdxUIAdornerManager.CreateAdornerTargetElementTree: TdxUIAdornerManagerTargetElementTree;
begin
  Result := TdxUIAdornerManagerTargetElementTree.Create(Self);
end;

function TdxUIAdornerManager.CreateBadges: TdxBadges;
begin
  Result := TdxBadges.Create(Self);
end;

function TdxUIAdornerManager.CreateDesignHelper: TdxUIAdornerManagerCustomDesignHelper;
begin
  Result := dxUIAdornerManagerDesignHelperClass.Create(Self);
end;

function TdxUIAdornerManager.CreateGuides: TdxGuides;
begin
  Result := TdxGuides.Create(Self);
end;

function TdxUIAdornerManager.CanUpdate: Boolean;
begin
  Result := not IsUpdating and not IsDestroying and not IsLoading;
end;

procedure TdxUIAdornerManager.CheckAdornerCovers(AChangedHandle: HWND; ABounds, AOldBounds: TRect);
begin
  Badges.CheckAdornerCovers(AChangedHandle, ABounds, AOldBounds);
  Guides.CheckAdornerCovers(AChangedHandle, ABounds, AOldBounds);
end;

procedure TdxUIAdornerManager.CheckAdornerPositions(AChangedHandle: HWND);
begin
  Badges.CheckAdornerPositions(AChangedHandle);
  Guides.CheckAdornerPositions(AChangedHandle);
end;

procedure TdxUIAdornerManager.Changed;
begin
  Update;
end;

procedure TdxUIAdornerManager.DoAssign(Source: TPersistent);
var
  ASource: TdxUIAdornerManager;
begin
  if Source is TdxUIAdornerManager then
  begin
    ASource := TdxUIAdornerManager(Source);
    Badges := ASource.Badges;
    Guides := ASource.Guides;
    OnActiveChanged := ASource.OnActiveChanged;
    OnAdornerChanged := ASource.OnAdornerChanged;
    OnAdornerClick := ASource.OnAdornerClick;
    OnAdornerCustomDraw := ASource.OnAdornerCustomDraw;
    OnAdornerCustomDrawBackground := ASource.OnAdornerCustomDrawBackground;
  end;
end;

procedure TdxUIAdornerManager.DoUpdate;
begin
  Badges.UpdateLayer;
  Guides.UpdateLayer;
end;

procedure TdxUIAdornerManager.GetChildren(Proc: TGetChildProc; Root: TComponent);
begin
  inherited GetChildren(Proc, Root);
  Badges.GetChildren(Proc, Root);
  Guides.GetChildren(Proc, Root);
end;

function TdxUIAdornerManager.HasDesignHelper: Boolean;
begin
  Result := DesignHelper <> nil;
end;

function TdxUIAdornerManager.IsUpdating: Boolean;
begin
  Result := FLockCount > 0;
end;

procedure TdxUIAdornerManager.Loaded;
begin
  inherited Loaded;
  Update;
end;

procedure TdxUIAdornerManager.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FImages) then
    Images := nil;
end;

procedure TdxUIAdornerManager.TargetControlFreed(ATargetControl: TControl);
begin
  BeginUpdate;
  try
    Badges.TargetControlFreed(ATargetControl);
    Guides.TargetControlFreed(ATargetControl);
  finally
    EndUpdate;
  end;
end;

procedure TdxUIAdornerManager.Update;
begin
  if CanUpdate then
    DoUpdate;
end;

procedure TdxUIAdornerManager.DoActiveChanged(AAdorners: TdxCustomAdorners);
begin
  if Assigned(FOnActiveChanged) then
    FOnActiveChanged(Self, AAdorners);
end;

procedure TdxUIAdornerManager.DoAdornerChanged(AAdorner: TdxCustomAdorner);
begin
  if Assigned(FOnAdornerChanged) then
    FOnAdornerChanged(Self, AAdorner);
end;

procedure TdxUIAdornerManager.DoAdornerClick(AAdorner: TdxCustomAdorner);
begin
  if Assigned(FOnAdornerClick) then
    FOnAdornerClick(Self, AAdorner);
end;

procedure TdxUIAdornerManager.DoAdornerCustomDraw(AItem: TdxCustomAdorner; AViewInfo: TdxCustomAdornerViewInfo;
  ACanvas: TdxGPCanvas; var ADone: Boolean);
begin
  if Assigned(FOnAdornerCustomDraw) then
    FOnAdornerCustomDraw(Self, AItem, ACanvas, AViewInfo, ADone);
end;

procedure TdxUIAdornerManager.DoAdornerCustomDrawBackground(AItem: TdxCustomAdorner; AViewInfo: TdxCustomAdornerViewInfo;
  ACanvas: TdxGPCanvas; var ADone: Boolean);
begin
  if Assigned(FOnAdornerCustomDrawBackground) then
    FOnAdornerCustomDrawBackground(Self, AItem, ACanvas, AViewInfo, ADone);
end;

procedure TdxUIAdornerManager.DoGuideGetCalloutPopupControl(AGuide: TdxGuide; out AControl: TWinControl);
begin
  if Assigned(FOnGuideGetCalloutPopupControl) then
    FOnGuideGetCalloutPopupControl(Self, AGuide, AControl)
end;

function TdxUIAdornerManager.GetDesignHelper: TdxUIAdornerManagerCustomDesignHelper;
begin
  if IsDesigning and not IsDestroying and (FDesignHelper = nil) and (dxUIAdornerManagerDesignHelperClass <> nil) then
    FDesignHelper := CreateDesignHelper;
  Result := FDesignHelper;
end;

function TdxUIAdornerManager.GetIsDesigning: Boolean;
begin
  Result := csDesigning in ComponentState;
end;

function TdxUIAdornerManager.GetIsDestroying: Boolean;
begin
  Result := csDestroying in ComponentState;
end;

function TdxUIAdornerManager.GetIsLoading: Boolean;
begin
  Result := csLoading in ComponentState;
end;

function TdxUIAdornerManager.GetOwnerControl: TWinControl;
begin
  Result := TWinControl(Owner);
end;

procedure TdxUIAdornerManager.ImageListChange(Sender: TObject);
begin
  Badges.Changed;
end;

procedure TdxUIAdornerManager.SetBadges(AValue: TdxBadges);
begin
  Badges.Assign(AValue);
end;

procedure TdxUIAdornerManager.SetGuides(AValue: TdxGuides);
begin
  Guides.Assign(AValue);
end;

procedure TdxUIAdornerManager.SetImages(Value: TCustomImageList);
begin
  cxSetImageList(Value, FImages, FImageChangeLink, Self);
end;

procedure TdxUIAdornerManager.SetOnActiveChanged(AValue: TdxAdornersNotifyEvent);
begin
  if not dxSameMethods(FOnActiveChanged, AValue) then
  begin
    FOnActiveChanged := AValue;
    Changed;
  end;
end;

procedure TdxUIAdornerManager.SetOnAdornerChanged(AValue: TdxAdornerNotifyEvent);
begin
  if not dxSameMethods(FOnAdornerChanged, AValue) then
  begin
    FOnAdornerChanged := AValue;
    Changed;
  end;
end;

procedure TdxUIAdornerManager.SetOnAdornerClick(AValue: TdxAdornerNotifyEvent);
begin
  if not dxSameMethods(FOnAdornerClick, AValue) then
  begin
    FOnAdornerClick := AValue;
    Changed;
  end;
end;

procedure TdxUIAdornerManager.SetOnAdornerCustomDraw(Value: TdxAdornerCustomDrawEvent);
begin
  if not dxSameMethods(FOnAdornerCustomDraw, Value) then
  begin
    FOnAdornerCustomDraw := Value;
    Changed;
  end;
end;

procedure TdxUIAdornerManager.SetOnAdornerCustomDrawBackground(Value: TdxAdornerCustomDrawEvent);
begin
  if not dxSameMethods(FOnAdornerCustomDrawBackground, Value) then
  begin
    FOnAdornerCustomDrawBackground := Value;
    Changed;
  end;
end;

procedure TdxUIAdornerManager.SetOnGuideGetCalloutPopupControl(AValue: TdxGuideGetCalloutPopupControl);
begin
  if not dxSameMethods(FOnGuideGetCalloutPopupControl, AValue) then
  begin
    FOnGuideGetCalloutPopupControl := AValue;
    Changed;
  end;
end;

procedure UnitRegister;
begin
  FHitTests := TdxHitTests.Create;
  dxRegisteredAdornerTargetElementTypes.Register(TdxAdornerTargetElementControl, 'Control');
  dxRegisteredAdornerTargetElementTypes.Register(TdxAdornerTargetElementPath, 'Path');
end;

procedure UnitUnregister;
begin
  FreeAndNil(FRegisteredManagers);
  FreeAndNil(FRegisteredTargetElementTypes);
  FreeAndNil(FHitTests);
  FreeAndNil(FBadgeDefaultGlyph);
  FreeAndNil(FGuideNormalBorderGlyph);
  FreeAndNil(FGuideSelectedBorderGlyph);
end;

initialization
  dxUnitsLoader.AddUnit(@UnitRegister, @UnitUnregister);

finalization
  dxUnitsLoader.RemoveUnit(@UnitUnregister);

end.
