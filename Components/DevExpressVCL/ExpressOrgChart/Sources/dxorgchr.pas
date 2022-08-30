{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           Express OrgChart                                         }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSORGCHART AND ALL ACCOMPANYING  }
{   VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY.              }
{                                                                    }
{   THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED       }
{   FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE         }
{   COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE        }
{   AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT   }
{   AND PERMISSION FROM DEVELOPER EXPRESS INC.                       }
{                                                                    }
{   CONSULT THE end USER LICENSE AGREEMENT FOR INFORMATION ON        }
{   ADDITIONAL RESTRICTIONS.                                         }
{                                                                    }
{********************************************************************}

unit dxorgchr;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, ImgList, dxCore, cxControls, cxGraphics, cxGeometry,
  cxLookAndFeels, cxLookAndFeelPainters, dxGDIPlusAPI, dxGDIPlusClasses, dxTouch, dxForms;

const
  OtMaxAnimFrames = 16;
  OtScrollUnit = 16;
  OtScrollTimerInterval = 220;

type
  TdxCustomOrgChart = class;
  TdxOcNode = class;
  TdxOcNodeClass = class of TdxOcNode;

  TdxOcNodeAlign = (caLeft, caCenter, caRight);
  TdxOcImageAlign = (iaNone, iaLT, iaLC, iaLB, iaRT, iaRC, iaRB, iaTL, iaTC, iaTR, iaBL, iaBC, iaBR);
  TdxOcIvFlags = (ivWidth, ivHeight, ivBoth);
  TdxOcShape = (shRectangle, shRoundRect, shEllipse, shDiamond);
  TdxOcNodeAttachMode = (naAdd, naAddFirst, naAddChild, naAddChildFirst, naInsert);

  TdxOcNodeInfo = packed record
    Width: Word;
    Height: Word;
    Color: TColor;
    Align: TdxOcNodeAlign;
    Shape: TdxOcShape;
    Index: Smallint;
    IAlign: TdxOcImageAlign;
  end;

  { TdxOcInplaceEdit }

  TdxOcInplaceEdit = class(TCustomEdit)
  private
    FMinW: Integer;
    FMaxW: Integer;
    FMinH: Integer;
    FMaxH: Integer;
    procedure AdjustBounds;
    procedure WMGetDlgCode(var Msg: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMKillFocus(var Msg: TWMKillFocus); message WM_KILLFOCUS;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
  public
    constructor Create(AOwner: TComponent); override;
    function Tree: TdxCustomOrgChart;
    property MaxLength;
    property OEMConvert;
  end;

  TdxOcNodeData = Pointer;

  { TdxOcNode }

  TdxOcNode = class(TObject)
  private
    FOwner: TdxCustomOrgChart;
    FParent: TdxOcNode;
    FList: TList;
    FIndex: Integer;
    FData: TdxOcNodeData;
    FWidth: Word;
    FHeight: Word;
    FChildrenWidth: Integer;
    FChildrenHeight: Integer;
    FExpanded: Boolean;
    FDeleting: Boolean;
    FChildAlign: TdxOcNodeAlign;
    FShape: TdxOcShape;
    FColor: TColor;
    FText: string;
    FAnimX: Integer;
    FAnimY: Integer;
    FAnimX0: Integer;
    FAnimY0: Integer;
    FImageIndex: Smallint;
    FImageAlign: TdxOcImageAlign;
    FDraw: Boolean;
    procedure InvalidateSize(Flags: TdxOcIvFlags);
    procedure Enumerate(Value: Integer);
    procedure AdjustSizes(const S: string);
    function FullRect: TRect;
    function Radius: Integer;
    function Is3D: Boolean;
    procedure FullToDisplay(var Rect: TRect);
    procedure DisplayToClient(var Rect: TRect);
    function ClientToText(var Rect: TRect): TPoint;
    function GetAbsoluteCount: Integer;
    function GetAbsoluteItem(Index: Integer): TdxOcNode;
    function GetCount: Integer;
    function GetChildren: Boolean;
    function GetVisible: Boolean;
    function GetItem(Index: Integer): TdxOcNode;
    function GetLevel: Integer;
    function GetParent: TdxOcNode;
    function GetSelected: Boolean;
    function GetWidth: Word;
    function GetHeight: Word;
    procedure SetExpanded(Value: Boolean);
    procedure SetChildren(Value: Boolean);
    procedure SetAlign(Value: TdxOcNodeAlign);
    procedure SetWidth(Value: Word);
    procedure SetHeight(Value: Word);
    procedure SetSelected(Value: Boolean);
    procedure SetColor(Value: TColor);
    procedure SetShape(Value: TdxOcShape);
    procedure SetImageIndex(Value: Smallint);
    procedure SetImageAlign(Value: TdxOcImageAlign);
    procedure InternalSetText(const Value: string);
    procedure SetAnimXY(LeftX, TopY: Integer; const Clip: TRect; First: Boolean);
    function ExtWidth: Word;
    function ExtHeight: Word;
    function ChildOffset: Integer;
    procedure ReadChildren(Stream: TStream);
    procedure WriteChildren(Stream: TStream);
  protected
    procedure ChangeScale(M, D: Integer); virtual;
    procedure SetData(Value: TdxOcNodeData); virtual;
    procedure SetFont(Font: TFont); virtual;
    procedure ReadData(Stream: TStream); virtual;
    procedure WriteData(Stream: TStream); virtual;
    function GetText: string; virtual;
    procedure SetText(const Value: string); virtual;
  public
    constructor Create(AOwner: TdxCustomOrgChart); virtual;
    destructor Destroy; override;
    procedure DeleteChildren;
    procedure Collapse(Recurse: Boolean);
    procedure Expand(Recurse: Boolean);
    function GetFirstChild: TdxOcNode;
    function GetLastChild: TdxOcNode;
    function GetNextChild(Value: TdxOcNode): TdxOcNode;
    function GetPrevChild(Value: TdxOcNode): TdxOcNode;
    function GetNext: TdxOcNode;
    function GetPrev: TdxOcNode;
    function getNextSibling: TdxOcNode; // GetNextSibling conflicts with C++ macro
    function getPrevSibling: TdxOcNode; // GetPrevSibling conflicts with C++ macro
    function GetNextVisible: TdxOcNode;
    function GetPrevVisible: TdxOcNode;
    function HasAsParent(Value: TdxOcNode): Boolean;
    function IndexOf(Value: TdxOcNode): Integer;
    function Focused: Boolean;
    function FullWidth: Integer;
    function FullHeight: Integer;
    function ChildrenWidth: Integer;
    function ChildrenHeight: Integer;
    procedure MakeVisible;
    procedure MoveTo(Dest: TdxOcNode; Mode: TdxOcNodeAttachMode);
    function DisplayRect: TRect;
    function ClientRect: TRect;
    function IsParentRoot: Boolean;
    procedure GetNodeInfo(var AInfo: TdxOcNodeInfo; ATargetScaleFactor: TdxScaleFactor = nil);

    property AbsoluteCount: Integer read GetAbsoluteCount;
    property AbsoluteItems[Index: Integer]: TdxOcNode read GetAbsoluteItem;
    property Count: Integer read GetCount;
    property Data: TdxOcNodeData read FData write SetData;
    property Expanded: Boolean read FExpanded write SetExpanded;
    property HasChildren: Boolean read GetChildren write SetChildren;
    property Index: Integer read FIndex;
    property IsVisible: Boolean read GetVisible;
    property Items[Index: Integer]: TdxOcNode read GetItem; default;
    property Level: Integer read GetLevel;
    property Owner: TdxCustomOrgChart read FOwner;
    property Parent: TdxOcNode read GetParent;
    property Selected: Boolean read GetSelected write SetSelected;
    property ChildAlign: TdxOcNodeAlign read FChildAlign write SetAlign;
    property Width: Word read GetWidth write SetWidth;
    property Height: Word read GetHeight write SetHeight;
    property Deleting: Boolean read FDeleting;
    property Color: TColor read FColor write SetColor;
    property Shape: TdxOcShape read FShape write SetShape;
    property Text: string read GetText write InternalSetText;
    property ImageIndex: Smallint read FImageIndex write SetImageIndex;
    property ImageAlign: TdxOcImageAlign read FImageAlign write SetImageAlign;
  end;

  { TdxOrgChartCustomPainter }

  TdxOrgChartCustomPainter = class(TObject)
  private
    FChart: TdxCustomOrgChart;
    function GetLookAndFeelPainter: TcxCustomLookAndFeelPainter; virtual;
  public
    constructor Create(AChart: TdxCustomOrgChart); virtual;
    function GetBackgroundColor: TColor; virtual;
    function GetLineColor: TColor; virtual;
    function GetNodeColor(ACurrectColor: TColor; ASelected: Boolean): TColor; virtual;
    function GetNodeTextColor(ASelected: Boolean): TColor; virtual;
    procedure DrawBackground(ACanvas: TcxCanvas; const R: TRect); virtual;
    procedure DrawFrame(ACanvas: TcxCanvas; ANode: TdxOcNode; const R: TRect);
    procedure DrawFrameDiamond(ACanvas: TcxCanvas; ANode: TdxOcNode; const R: TRect); virtual;
    procedure DrawFrameEllipse(ACanvas: TcxCanvas; ANode: TdxOcNode; const R: TRect); virtual;
    procedure DrawFrameRectangle(ACanvas: TcxCanvas; ANode: TdxOcNode; const R: TRect; ARect3D: Boolean); virtual;
    procedure DrawFrameRoundRect(ACanvas: TcxCanvas; ANode: TdxOcNode; const R: TRect; ARadius: Integer); virtual;
    procedure DrawLine(ACanvas: TcxCanvas; X1, Y1, X2, Y2: Integer; AWidth: Single); virtual;
    //
    property Chart: TdxCustomOrgChart read FChart;
    property LookAndFeelPainter: TcxCustomLookAndFeelPainter read GetLookAndFeelPainter;
  end;

  { TdxOrgChartGdiPlusPainter }

  TdxOrgChartGdiPlusPainter = class(TdxOrgChartCustomPainter)
  private
    function GetBrushColor(ACanvas: TcxCanvas): TColor;
  public
    procedure DrawFrameDiamond(ACanvas: TcxCanvas; ANode: TdxOcNode; const R: TRect); override;
    procedure DrawFrameEllipse(ACanvas: TcxCanvas; ANode: TdxOcNode; const R: TRect); override;
    procedure DrawFrameRectangle(ACanvas: TcxCanvas; ANode: TdxOcNode; const R: TRect; ARect3D: Boolean); override;
    procedure DrawFrameRoundRect(ACanvas: TcxCanvas; ANode: TdxOcNode; const R: TRect; ARadius: Integer); override;
    procedure DrawLine(ACanvas: TcxCanvas; X1, Y1, X2, Y2: Integer; AWidth: Single); override;
  end;

  { TdxOrgChartAnimationThread }

  TdxOrgChartAnimationThread = class(TThread)
  private
    FChart: TdxCustomOrgChart;
  protected
    procedure AnimationDone;
    procedure AnimationStep;
    procedure Execute; override;
  public
    constructor Create(AChart: TdxCustomOrgChart);
    //
    property Chart: TdxCustomOrgChart read FChart;
  end;

  { TdxCustomOrgChart }

  TdxOcEvent = procedure(Sender: TObject; Node: TdxOcNode) of object;
  TdxOcAllowEvent = procedure(Sender: TObject; Node: TdxOcNode; var Allow: Boolean) of object;
  TdxOcFontEvent = procedure(Sender: TObject; Node: TdxOcNode; Font: TFont) of object;
  TdxOcDrawEvent = procedure(Sender: TObject; Node: TdxOcNode; ACanvas: TCanvas; Rect: TRect) of object;
  TdxOcGetTextEvent = procedure(Sender: TObject; Node: TdxOcNode; var Text: string) of object;
  TdxOcSetTextEvent = procedure(Sender: TObject; Node: TdxOcNode; const Text: string) of object;
  TdxOcNodeFunc = function(Value: TdxOcNode): TdxOcNode;
  TdxOcEditMode = set of (emLeft, emCenter, emRight, emVCenter,
    emWrap, emUpper, emLower, emGrow);
  TdxOcOptions = set of (ocSelect, ocFocus, ocButtons, ocDblClick, ocEdit,
    ocCanDrag, ocShowDrag, ocInsDel, ocRect3D, ocAnimate, ocImageFiltering);
  TdxOcHitTest = (htNowhere, htOnLeftIndentX, htOnRightIndentX, htOnIndentY,
    htUnder, htOnRect, htOnShape, htOnButton);
  TdxOcHitTests = set of TdxOcHitTest;
  TdxOcNavigate = (ocnLineLeft, ocnLineUp, ocnLineRight, ocnLineDown,
    ocnPageLeft, ocnPageUp, ocnPageRight, ocnPageDown,
    ocnLeft, ocnTop, ocnRight, ocnBottom,
    ocnLeftPosition, ocnTopPosition,
    ocnSelectNextLeft, ocnSelectNextUp, ocnSelectNextRight, ocnSelectNextDown);

  TdxCustomOrgChart = class(TcxScrollingControl, IdxSkinSupport, IdxRotateClient)
  private
    FAnimationStep: Integer;
    FAnimationThread: TThread;
    FAntialiasing: Boolean;
    FBorderStyle: TBorderStyle;
    FCollapsed: TdxOcNode;
    FCount: Integer;
    FDefaultImageAlign: TdxOcImageAlign;
    FDefaultNodeHeight: Word;
    FDefaultNodeWidth: Word;
    FDrag: Boolean;
    FDragParent: TdxOcNode;
    FDragX0: Integer;
    FDragX1: Integer;
    FDragY0: Integer;
    FDragY1: Integer;
    FEditMode: TdxOcEditMode;
    FEditor: TdxOcInplaceEdit;
    FHitTests: TdxOcHitTests;
    FHitX: Integer;
    FHitY: Integer;
    FImages: TCustomImageList;
    FImagesChangeLink: TChangeLink;
    FIndentX: Word;
    FIndentY: Word;
    FIsUnicode: Boolean;
    FLineColor: TColor;
    FLineWidth: Word;
    FNoAnim: Boolean;
    FNodeAt: TdxOcNode;
    FOptions: TdxOcOptions;
    FPainter: TdxOrgChartCustomPainter;
    FRoot: TdxOcNode;
    FRotated: Boolean;
    FScrollTimer: TTimer;
    FSelected: TdxOcNode;
    FSelectedNodeColor: TColor;
    FSelectedNodeTextColor: TColor;
    FSizeChanged: Boolean;
    FUpdate: Integer;
    FUpdated: Boolean;
    FZoom: Integer;
    FZoomHi: Integer;
    FZoomLo: Integer;

    FOnChange: TdxOcEvent;
    FOnChanging: TdxOcAllowEvent;
    FOnCollapsed: TdxOcEvent;
    FOnCollapsing: TdxOcAllowEvent;
    FOnCreateNode: TdxOcEvent;
    FOnDeletion: TdxOcEvent;
    FOnDrawNode: TdxOcDrawEvent;
    FOnEdited: TdxOcGetTextEvent;
    FOnEditing: TdxOcAllowEvent;
    FOnExpanded: TdxOcEvent;
    FOnExpansion: TdxOcAllowEvent;
    FOnGetText: TdxOcGetTextEvent;
    FOnSetFont: TdxOcFontEvent;
    FOnSetText: TdxOcSetTextEvent;

    procedure ImageListChange(Sender: TObject);
    function GetAbsoluteItem(Index: Integer): TdxOcNode;
    function GetClientBoundsHeight: Integer;
    function GetClientBoundsWidth: Integer;
    function GetEditing: Boolean;
    function GetFullRect: TRect;
    function GetIndentX: Word;
    function GetIndentY: Word;
    function GetItem(Index: Integer): TdxOcNode;
    function GetLeftEdge: Integer;
    function GetLineWidth: Word;
    function GetRootCount: Integer;
    function GetTopEdge: Integer;
    function GetZoom: Boolean;
    procedure InvalidateSizes(AFlags: TdxOcIvFlags);
    function InvalidateNode(Value: TdxOcNode): Boolean;
    function InvalidateSel: Boolean;
    function IsUpdated: Boolean;
    function IsMyNode(Value: TdxOcNode): Boolean;
    function HasButton(Node: TdxOcNode): Boolean;
    function CanSelect(Node: TdxOcNode): Boolean;
    function NextSel(Get: TdxOcNodeFunc): TdxOcNode;
    function MinSizes: TPoint;
    procedure DoAdd(AParent, ANode: TdxOcNode; AIndex: Integer);
    procedure HitTestsAt(X, Y: Integer);
    procedure ChangeSize;
    procedure RecalcSizes;
    function InitAnimate(Node: TdxOcNode): Boolean;
    procedure StopAnimation;
    procedure DragDraw(Source: TdxCustomOrgChart);
    procedure DoDrawText(Handle: HDC; Text: string; var Rect: TRect; Flags: Integer);
    procedure SetAntialiasing(AValue: Boolean);
    procedure SetLeftEdge(Value: Integer);
    procedure SetTopEdge(Value: Integer);
    procedure SetLineColor(Value: TColor);
    procedure SetLineWidth(Value: Word);
    procedure SetSelectedNodeColor(Value: TColor);
    procedure SetSelectedNodeTextColor(Value: TColor);
    procedure SetNodeWidth(Value: Word);
    procedure SetNodeHeight(Value: Word);
    procedure SetIndentX(Value: Word);
    procedure SetIndentY(Value: Word);
    procedure SetSelected(Value: TdxOcNode);
    procedure SetZoom(Value: Boolean);
    procedure SetZoomRatio;
    procedure SetEditing(Value: Boolean);
    procedure SetEditMode(Value: TdxOcEditMode);
    procedure SetImages(Value: TCustomImageList);
    procedure SetRotated(Value: Boolean);
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure SetOptions(Value: TdxOcOptions);

    function RotateRect(const Rect: TRect): TRect;
    procedure RotatePoint(var X, Y: Integer);

    procedure ReadData(Stream: TStream);
    procedure WriteData(Stream: TStream);

    procedure SetTimeScroll;
    procedure KillTimeScroll;
    procedure DoTimerScrolling(Sender: TObject);
    function DragScroll(X, Y: Integer; AState: TDragState): Boolean;

    procedure WMSetFocus(var Msg: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Msg: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
    procedure WMGetDlgCode(var Msg: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMDblClk(var Msg: TWMMouse); message WM_LBUTTONDBLCLK;
    procedure WMErase(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure CMCtl3DChanged(var Msg: TMessage); message CM_CTL3DCHANGED;
  protected
    function AllowTouchScrollUIMode: Boolean; override;
    procedure ChangeScaleEx(M: Integer; D: Integer; isDpiChange: Boolean); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateHandle; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;  X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    function GetIsCopyDragDrop: Boolean; override;
    function GetScrollContentForegroundColor: TColor; override;
    procedure DragCanceled; override;
    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean); override;
    function IsDefaultGesture(AGestureID: Integer): Boolean; override;
    procedure KeyPress(var Key: Char); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure DblClick; override;
    procedure Loaded; override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    // IdxRotateClient
    procedure Rotate(AAngle: Double; var AHandled: Boolean);

    procedure Paint; override;
    procedure PaintContent(ACanvas: TcxCanvas; const AClipRect: TRect);
    function PaintNode(ACanvas: TcxCanvas; AFont: TFont; ANode: TdxOcNode; const AUpdateRect: TRect): TPoint;

    procedure LookAndFeelChanged(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues); override;
    procedure NodeChanged(Node: TdxOcNode); virtual;

    function CreateEditor: TdxOcInplaceEdit; virtual;
    function CreateNode: TdxOcNode; virtual;
    function CreatePainter: TdxOrgChartCustomPainter; virtual;
    procedure DoChange(Node: TdxOcNode); virtual;
    procedure DoChanging(Node: TdxOcNode; var Allow: Boolean); virtual;
    procedure DoNavigate(ANavigateCode: TdxOcNavigate; AValue: Integer = 0);
    procedure DrawNode(Node: TdxOcNode; ACanvas: TCanvas; Rect: TRect); virtual;
    function GetContentSize: TSize; override;
    function GetNodeClass: TdxOcNodeClass; virtual;
    function InternalAdd(ParentNode: TdxOcNode; Data: TdxOcNodeData; Idx: Integer): TdxOcNode; virtual;
    procedure InternalMoveTo(ParentNode, Node: TdxOcNode; Idx: Integer); virtual;
    procedure SelectNode(ANode: TdxOcNode);

    property AnimationStep: Integer read FAnimationStep;
    property Antialiasing: Boolean read FAntialiasing write SetAntialiasing default False;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property ClientBoundsHeight: Integer read GetClientBoundsHeight;
    property ClientBoundsWidth: Integer read GetClientBoundsWidth;
    property DefaultImageAlign: TdxOcImageAlign read FDefaultImageAlign write FDefaultImageAlign default iaNone;
    property DefaultNodeHeight: Word read FDefaultNodeHeight write SetNodeHeight default 40;
    property DefaultNodeWidth: Word read FDefaultNodeWidth write SetNodeWidth default 64;
    property EditMode: TdxOcEditMode read FEditMode write SetEditMode default [emWrap];
    property Images: TCustomImageList read FImages write SetImages;
    property IndentX: Word read GetIndentX write SetIndentX default 16;
    property IndentY: Word read GetIndentY write SetIndentY default 16;
    property IsUnicode: Boolean read FIsUnicode write FIsUnicode;
    property LineColor: TColor read FLineColor write SetLineColor default clDefault;
    property LineWidth: Word read GetLineWidth write SetLineWidth default 1;
    property Options: TdxOcOptions read FOptions write SetOptions;
    property Painter: TdxOrgChartCustomPainter read FPainter;
    property Rotated: Boolean read FRotated write SetRotated default False;
    property SelectedNodeColor: TColor read FSelectedNodeColor write SetSelectedNodeColor default clDefault;
    property SelectedNodeTextColor: TColor read FSelectedNodeTextColor write SetSelectedNodeTextColor default clDefault;

    property OnChange: TdxOcEvent read FOnChange write FOnChange;
    property OnChanging: TdxOcAllowEvent read FOnChanging write FOnChanging;
    property OnCollapsed: TdxOcEvent read FOnCollapsed write FOnCollapsed;
    property OnCollapsing: TdxOcAllowEvent read FOnCollapsing write FOnCollapsing;
    property OnCreateNode: TdxOcEvent read FOnCreateNode write FOnCreateNode;
    property OnDeletion: TdxOcEvent read FOnDeletion write FOnDeletion;
    property OnDrawNode: TdxOcDrawEvent read FOnDrawNode write FOnDrawNode;
    property OnEdited: TdxOcGetTextEvent read FOnEdited write FOnEdited;
    property OnEditing: TdxOcAllowEvent read FOnEditing write FOnEditing;
    property OnExpanded: TdxOcEvent read FOnExpanded write FOnExpanded;
    property OnExpansion: TdxOcAllowEvent read FOnExpansion write FOnExpansion;
    property OnGetText: TdxOcGetTextEvent read FOnGetText write FOnGetText;
    property OnSetFont: TdxOcFontEvent read FOnSetFont write FOnSetFont;
    property OnSetText: TdxOcSetTextEvent read FOnSetText write FOnSetText;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure DragDrop(Source: TObject; X, Y: Integer); override;
    procedure Invalidate; override;
    procedure PaintContentTo(ACanvas: TCanvas; X, Y: Integer);

    procedure BeginUpdate;
    procedure EndUpdate;

    function Add(Node: TdxOcNode; Data: TdxOcNodeData): TdxOcNode;
    function AddChild(Node: TdxOcNode; Data: TdxOcNodeData): TdxOcNode;
    function AddFirst(Node: TdxOcNode; Data: TdxOcNodeData): TdxOcNode;
    function AddChildFirst(Node: TdxOcNode; Data: TdxOcNodeData): TdxOcNode;
    procedure Clear;
    procedure Delete(Node: TdxOcNode); virtual;
    function Insert(Node: TdxOcNode; Data: TdxOcNodeData): TdxOcNode;

    function GetFirstNode: TdxOcNode;
    procedure FullExpand;
    procedure FullCollapse;
    function FullWidth: Integer;
    function FullHeight: Integer;
    function IsZoomed: Boolean;
    function GetNodeAt(X, Y: Integer): TdxOcNode;
    function GetHitTestsAt(X, Y: Integer): TdxOcHitTests;
    function DoZoom(Value: Integer): Integer;
    procedure DefaultDrawNode(Node: TdxOcNode; ACanvas: TCanvas; R: TRect);
    procedure AssignData(Source: TdxCustomOrgChart);
    procedure ShowEditor; virtual;
    procedure HideEditor(Save: Boolean);

    procedure SaveToFile(const AFileName: string);
    procedure SaveToStream(AStream: TStream);

    property AbsoluteItems[Index: Integer]: TdxOcNode read GetAbsoluteItem;
    property Count: Integer read FCount;
    property DragParent: TdxOcNode read FDragParent;
    property Editing: Boolean read GetEditing write SetEditing;
    property FullRect: TRect read GetFullRect;
    property Items[Index: Integer]: TdxOcNode read GetItem; default;
    property LeftEdge: Integer read GetLeftEdge write SetLeftEdge;
    property RootCount: Integer read GetRootCount;
    property RootNode: TdxocNode read FRoot;
    property Selected: TdxOcNode read FSelected write SetSelected;
    property TopEdge: Integer read GetTopEdge write SetTopEdge;
    property Zoom: Boolean read GetZoom write SetZoom default False;
  end;

  { TdxOrgChart }

  TdxOrgChart = class(TdxCustomOrgChart)
  protected
    procedure DefineProperties(Filer: TFiler); override;
  public
    procedure LoadFromFile(const AFileName: string);
    procedure LoadFromStream(AStream: TStream);
  published
    property Anchors;
    property Antialiasing;
    property BorderStyle;
    property DefaultImageAlign;
    property DefaultNodeHeight;
    property DefaultNodeWidth;
    property EditMode;
    property Images;
    property IndentX;
    property IndentY;
    property LineColor;
    property LineWidth;
    property LookAndFeel;
    property Rotated;
    property SelectedNodeColor;
    property SelectedNodeTextColor;
    property Zoom;
    property Options;
    property Align;
    property Ctl3D;
    property Color default clDefault;
    property Enabled;
    property Font;
    property ParentColor default False;
    property ParentCtl3D;
    property TabStop default True;
    property Visible;

    property OnCreateNode;
    property OnChange;
    property OnChanging;
    property OnCollapsed;
    property OnCollapsing;
    property OnDeletion;
    property OnExpanded;
    property OnExpansion;
    property OnEditing;
    property OnEdited;
    property OnSetFont;
    property OnDrawNode;
    property OnGetText;
    property OnSetText;

    property OnClick;
    property OnDblClick;
    property OnEnter;
    property OnExit;
    property OnDragDrop;
    property OnDragOver;
    property OnStartDrag;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property PopupMenu;
    property ParentFont default False;
    property ParentShowHint;
    property ShowHint;
  end;

  { TdxOrgChartCustomCustomizeForm }

  TdxOrgChartCustomCustomizeForm = class(TdxForm)
  protected
    NodeInfo: TdxOcNodeInfo;

    function CalculateFormPosition(AOrgChart: TdxCustomOrgChart): TRect; virtual;
    function GetPreviewOrgChart: TdxCustomOrgChart; virtual; abstract;
    function InsertNode: TdxOcNode;
    function InsertSubNode: TdxOcNode;
    procedure PreparePreview(APreview, ASource: TdxCustomOrgChart); virtual;
    procedure SaveChanges(AOrgChart: TdxCustomOrgChart);
    //
    property PreviewOrgChart: TdxCustomOrgChart read GetPreviewOrgChart;
  public
    function Execute(AOrgChart: TdxCustomOrgChart): Boolean; virtual;
  end;

  TdxOrgChartCustomCustomizeFormClass = class of TdxOrgChartCustomCustomizeForm;

  { TdxOrgChartCustomizeFormManager }

  TdxOrgChartCustomizeFormManager = class
  strict private
    FList: TList;
  protected
    function GetActualClass: TdxOrgChartCustomCustomizeFormClass;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function Customize(AOrgChart: TdxCustomOrgChart): Boolean;
    procedure Register(AClass: TdxOrgChartCustomCustomizeFormClass);
    procedure Unregister(AClass: TdxOrgChartCustomCustomizeFormClass);
  end;

var
  dxOrgChartCustomizeFormManager: TdxOrgChartCustomizeFormManager;

function ShowOrgChartEditor(ATree: TdxCustomOrgChart): Boolean;

implementation

uses
  Types, Math, cxScrollBar, dxDPIAwareUtils;

const
  StreamDescriptionANSI: AnsiString = 'VER1.0A';
  StreamDescriptionUNICODE: AnsiString = 'VER1.0U';

function ReadStr(Stream: TStream; AIsUnicode: Boolean): string;
var
  L: Word;
  SA: AnsiString;
  SW: WideString;
begin
  Stream.ReadBuffer(L, SizeOf(Word));
  if AIsUnicode then
  begin
    SetLength(SW, L);
    if L > 0 then
      Stream.ReadBuffer(SW[1], L * 2);
    Result := SW;
  end
  else
  begin
    SetLength(SA, L);
    if L > 0 then
      Stream.ReadBuffer(SA[1], L);
    Result := UTF8ToWideString(SA);
  end;
end;


procedure WriteStr(Stream: TStream; const S: string);
var
  L: Integer;
{$IFDEF STREAMANSIFORMAT}
  SA: AnsiString;
{$ENDIF}
begin
  L := Length(S);
  if L > $FFFF then L := $FFFF;
  Stream.WriteBuffer(L, SizeOf(Word));
  if L > 0 then
  begin
  {$IFDEF STREAMANSIFORMAT}
    SA := UTF8Encode(S);
    Stream.WriteBuffer(SA[1], L);
  {$ELSE}
    Stream.WriteBuffer(S[1], L * SizeOf(Char));
  {$ENDIF}
  end;
end;

function GetNV(Value: TdxOcNode): TdxOcNode;
begin
  Result := Value.GetNextVisible;
end;

function GetPV(Value: TdxOcNode): TdxOcNode;
begin
  Result := Value.GetPrevVisible;
end;

function GetNS(Value: TdxOcNode): TdxOcNode;
begin
  Result := Value.getNextSibling;
end;

function GetPS(Value: TdxOcNode): TdxOcNode;
begin
  Result := Value.getPrevSibling;
end;

function ShowOrgChartEditor(ATree: TdxCustomOrgChart): Boolean;
begin
  Result := dxOrgChartCustomizeFormManager.Customize(ATree);
end;

{ TdxOrgChartAnimationThread }

constructor TdxOrgChartAnimationThread.Create(AChart: TdxCustomOrgChart);
begin
  inherited Create(False);
  FChart := AChart;
end;

procedure TdxOrgChartAnimationThread.AnimationDone;
begin
  Chart.FAnimationStep := 0;
  Chart.FCollapsed := nil;
  Chart.Invalidate;
  Chart.Update;
end;

procedure TdxOrgChartAnimationThread.AnimationStep;
begin
  Dec(Chart.FAnimationStep);
  Chart.Invalidate;
  Chart.Update;
end;

procedure TdxOrgChartAnimationThread.Execute;
const
  AnimationDelay = 10;
var
  ATime: Cardinal;
begin
  ATime := GetTickCount;
  while not Terminated and (Chart.AnimationStep > 0) do
  begin
    Inc(ATime, AnimationDelay);
    Synchronize(AnimationStep);
    while not Terminated and (ATime > GetTickCount) do ;
  end;
  Synchronize(AnimationDone);
end;

{ TdxOcNode }

constructor TdxOcNode.Create(AOwner: TdxCustomOrgChart);
begin
  inherited Create;
  FOwner := AOwner;
  FChildAlign := caCenter;
  FColor := clNone;
  FImageIndex := -1;
  FImageAlign := AOwner.DefaultImageAlign;
end;

destructor TdxOcNode.Destroy;
begin
  if Deleting then
    inherited Destroy
  else
    Owner.Delete(Self);
end;

function TdxOcNode.GetCount: Integer;
begin
  if FList = nil then
    Result := 0
  else
    Result := FList.Count;
end;

function TdxOcNode.GetAbsoluteCount: Integer;
var
  I: Integer;
begin
  Result := Count;
  if Result > 0 then
    for I := 0 to Count - 1 do
      Inc(Result, Items[I].AbsoluteCount);
end;

function TdxOcNode.GetAbsoluteItem(Index: Integer): TdxOcNode;
var
  I, APassedCount, AChildCount: Integer;
begin
  Result := nil;
  if Count = 0 then exit;
  APassedCount := 0;
  for I := 0 to Count - 1 do
  begin
    if Index - APassedCount = 0 then
    begin
      Result := Items[I];
      break;
    end
    else
      Inc(APassedCount);

    AChildCount := Items[I].AbsoluteCount;
    if Index - APassedCount < AChildCount then
    begin
      Result := Items[I].AbsoluteItems[Index - APassedCount];
      break;
    end
    else
      Inc(APassedCount, AChildCount);
  end;
end;

procedure TdxOcNode.SetExpanded(Value: Boolean);
begin
  if Value then
    Expand(False)
  else
    Collapse(False);
end;

function TdxOcNode.GetChildren: Boolean;
begin
  Result := FList <> nil;
end;

procedure TdxOcNode.SetChildren(Value: Boolean);
begin
  if Value = HasChildren then Exit;
  if Value then
    FList := TList.Create
  else
  begin
    DeleteChildren;
    FList.Free;
    FList := nil;
  end;
end;

function TdxOcNode.GetParent: TdxOcNode;
begin
  if IsParentRoot then
    Result := nil
  else
    Result := FParent;
end;

function TdxOcNode.GetSelected: Boolean;
begin
  Result := Owner.Selected = Self;
end;

procedure TdxOcNode.SetSelected(Value: Boolean);
begin
  if Value then
    Owner.Selected := Self
  else
    if Selected then
      Owner.Selected := nil;
end;

function TdxOcNode.Is3D: Boolean;
begin
  Result := (Shape = shRectangle) and (ocRect3D in Owner.Options);
end;

function TdxOcNode.GetWidth: Word;
begin
  Result := FWidth;
  if Result = 0 then
    Result := Owner.DefaultNodeWidth;
  Result := Owner.DoZoom(Result);
end;

function TdxOcNode.GetHeight: Word;
begin
  Result := FHeight;
  if Result = 0 then
    Result := Owner.DefaultNodeHeight;
  Result := Owner.DoZoom(Result);
end;

function TdxOcNode.ExtWidth: Word;
begin
  if Owner.Rotated then
    Result := GetHeight
  else
    Result := GetWidth;

  if Is3D then
    Inc(Result, Owner.LineWidth);
end;

function TdxOcNode.ExtHeight: Word;
begin
  if Owner.Rotated then
    Result := GetWidth
  else
    Result := GetHeight;

  if Is3D then
    Inc(Result, Owner.LineWidth);
end;

function TdxOcNode.GetLevel: Integer;
begin
  if Parent = nil then
    Result := 0
  else
    Result := FParent.Level + 1;
end;

function TdxOcNode.GetVisible: Boolean;
begin
  if Parent = nil then
    Result := (FParent <> nil) or (Self = Owner.FRoot)
  else
    Result := FParent.IsVisible and FParent.Expanded;
end;

procedure TdxOcNode.SetAlign(Value: TdxOcNodeAlign);

  function DoInvalidateNodeArea(ANode: TdxOcNode): TRect;
  var
    R: TRect;
  begin
    R := ANode.FullRect;
    Inc(R.Bottom, Owner.IndentY - 1);
    if ANode.FullWidth > ANode.ChildrenWidth then
      R.Bottom := R.Top + ANode.FullHeight + Owner.IndentY;
    Owner.InvalidateRect(Owner.RotateRect(R), True);
  end;

begin
  if Value <> FChildAlign then
  begin
    FChildAlign := Value;
    if FParent <> nil then
      DoInvalidateNodeArea(FParent);
    if IsVisible then
    begin
      Owner.HideEditor(True);
      if Owner.FNodeAt = Self then
        Owner.FHitTests := [];
      if not Owner.IsUpdated and Owner.HandleAllocated then
        DoInvalidateNodeArea(Self);
    end;
  end;
end;

procedure TdxOcNode.SetWidth(Value: Word);
var
  P: TPoint;
begin
  P := Owner.MinSizes;
  if (Value <> 0) and (Value < P.X) then Value := P.X;
  if Value <> FWidth then
  begin
    FWidth := Value;
    if FParent = nil then Exit;
    if Owner.Rotated then
      FParent.InvalidateSize(ivHeight)
    else
      FParent.InvalidateSize(ivWidth);
    Owner.NodeChanged(Self);
  end;
end;

procedure TdxOcNode.SetHeight(Value: Word);
var
  P: TPoint;
begin
  P := Owner.MinSizes;
  if (Value <> 0) and (Value < P.Y) then Value := P.Y;
  if Value <> FHeight then
  begin
    FHeight := Value;
    if FParent = nil then Exit;
    if Owner.Rotated then
      FParent.InvalidateSize(ivWidth)
    else
      FParent.InvalidateSize(ivHeight);
    Owner.NodeChanged(Self);
  end;
end;

function TdxOcNode.GetFirstChild: TdxOcNode;
begin
  if Count = 0 then
    Result := nil
  else
    Result := TdxOcNode(FList[0]);
end;

function TdxOcNode.GetLastChild: TdxOcNode;
begin
  if Count = 0 then
    Result := nil
  else
    Result := TdxOcNode(FList[FList.Count - 1]);
end;

function TdxOcNode.getNextSibling: TdxOcNode;
begin
  if Index >= FParent.Count - 1 then
    Result := nil
  else
    Result := FParent[Index + 1];
end;

function TdxOcNode.getPrevSibling: TdxOcNode;
begin
  if Index = 0 then
    Result := nil
  else
    Result := FParent[Index - 1];
end;

function TdxOcNode.GetNext: TdxOcNode;

  function NxtSbl(Node: TdxOcNode): TdxOcNode;
  begin
    Result := Node.getNextSibling;
    if Result <> nil then
      Exit;
    if Node.Parent <> nil then
      Result := NxtSbl(Node.FParent);
  end;

begin
  Result := GetFirstChild;
  if Result = nil then
    Result := NxtSbl(Self);
end;

function TdxOcNode.GetPrev: TdxOcNode;
begin
  Result := getPrevSibling;
  if Result = nil then
    Result := Parent
  else
    while Result.Count <> 0 do
      Result := Result.GetLastChild;
end;

function TdxOcNode.GetNextChild(Value: TdxOcNode): TdxOcNode;
begin
  if Value.FParent <> Self then
    Result := nil
  else
    Result := Value.getNextSibling;
end;

function TdxOcNode.GetPrevChild(Value: TdxOcNode): TdxOcNode;
begin
  if Value.FParent <> Self then
    Result := nil
  else
    Result := Value.getPrevSibling;
end;

function TdxOcNode.GetNextVisible: TdxOcNode;
var
  Node: TdxOcNode;
  Tmp: TList;
begin
  Node := Self;
  while not Node.IsVisible do
    Node := Node.FParent;
  Tmp := Node.FList;
  if not Node.Expanded then Node.FList := nil;
  Result := Node.GetNext;
  Node.FList := Tmp;
end;

function TdxOcNode.GetPrevVisible: TdxOcNode;
begin
  Result := GetPrev;
  if Result = nil then Exit;
  while not Result.IsVisible do
    Result := Result.FParent;
end;

function TdxOcNode.GetItem(Index: Integer): TdxOcNode;
begin
  Result := TdxOcNode(FList[Index])
end;

function TdxOcNode.HasAsParent(Value: TdxOcNode): Boolean;
var
  Node: TdxOcNode;
begin
  Result := True;
  if Value = nil then Exit;
  Node := FParent;
  while Node <> nil do
  begin
    if Node = Value then Exit;
    Node := Node.FParent;
  end;
  Result := False;
end;

function TdxOcNode.IndexOf(Value: TdxOcNode): Integer;
begin
  if Value.FParent <> Self then
    Result := -1
  else
    Result := Value.Index;
end;

procedure TdxOcNode.Enumerate(Value: Integer);
var
  I: Integer;
begin
  for I := Value to Count - 1 do
    TdxOcNode(FList[I]).FIndex := I;
end;

function TdxOcNode.Focused: Boolean;
begin
  Result := Selected and Owner.Focused;
end;

function TdxOcNode.ChildrenWidth: Integer;
var
  I: Integer;
begin
  if not Expanded then
    Result := 0
  else
  begin
    Result := FChildrenWidth;
    if Result = 0 then
    begin
      Inc(Owner.FZoom);
      for I := 0 to Count - 1 do
        Result := Result + Items[I].FullWidth;
      FChildrenWidth := Result;
      Dec(Owner.FZoom);
    end;
    Result := Owner.DoZoom(Result);
  end;
end;

function TdxOcNode.ChildrenHeight: Integer;
var
  I, H: Integer;
begin
  if not Expanded then
    Result := 0
  else
  begin
    Result := FChildrenHeight;
    if Result = 0 then
    begin
      Inc(Owner.FZoom);
      for I := 0 to Count - 1 do
      begin
        H := Items[I].FullHeight;
        if Result < H then Result := H;
      end;
      FChildrenHeight := Result;
      Dec(Owner.FZoom);
    end;
    Result := Owner.DoZoom(Result);
  end;
end;

function TdxOcNode.FullWidth: Integer;
begin
  Inc(Owner.FZoom);
  Result := Max(ChildrenWidth, ExtWidth + Owner.LineWidth + Owner.IndentX);
  Dec(Owner.FZoom);
  Result := Owner.DoZoom(Result);
end;

function TdxOcNode.FullHeight: Integer;
begin
  Inc(Owner.FZoom);
  Result := ExtHeight + Owner.LineWidth + Owner.IndentY + ChildrenHeight;
  Dec(Owner.FZoom);
  Result := Owner.DoZoom(Result);
end;

procedure TdxOcNode.InvalidateSize(Flags: TdxOcIvFlags);

  procedure IvSz(Node: TdxOcNode; Flags: TdxOcIvFlags);
  begin
    if Flags in [ivWidth, ivBoth] then
      Node.FChildrenWidth := 0;
    if Flags in [ivHeight, ivBoth] then
      Node.FChildrenHeight := 0;
    if Node.FParent <> nil then
      IvSz(Node.FParent, Flags);
  end;

begin
  IvSz(Self, Flags);
  if not IsVisible then
    Exit;
  if Expanded then
    Owner.ChangeSize
  else
    if Count < 2 then
      Owner.InvalidateNode(Self);
end;

procedure TdxOcNode.Expand(Recurse: Boolean);
var
  I: Integer;
  Allow: Boolean;
begin
  if Count = 0 then Exit;
  if Recurse then
  begin
    for I := 0 to Count - 1 do
      Items[I].Expand(True);
    Expand(False);
  end
  else
  begin
    if Expanded then Exit;
    Allow := True;
    if Assigned(Owner.OnExpansion) then Owner.OnExpansion(Owner, Self, Allow);
    if not Allow then Exit;
    Allow := Owner.InitAnimate(Self);
    FExpanded := True;
    FParent.InvalidateSize(ivBoth);
    if Allow then Owner.Update;
    if Assigned(Owner.OnExpanded) then Owner.OnExpanded(Owner, Self);
  end;
end;

procedure TdxOcNode.Collapse(Recurse: Boolean);
var
  I: Integer;
  Allow: Boolean;
begin
  if Count = 0 then
    Exit;

  if Recurse then
  begin
    Collapse(False);
    for I := 0 to Count - 1 do
      Items[I].Collapse(True);
  end
  else
  begin
    if (not Expanded) or (FParent = nil) then
      Exit;
    Allow := True;
    if Assigned(Owner.OnCollapsing) then
      Owner.OnCollapsing(Owner, Self, Allow);
    if not Allow then
      Exit;
    Allow := Owner.InitAnimate(Self);
    FExpanded := False;
    FParent.InvalidateSize(ivBoth);
    if Allow then
      Owner.Update;
    if Assigned(Owner.OnCollapsed) then
      Owner.OnCollapsed(Owner, Self);
  end;
end;

procedure TdxOcNode.ChangeScale(M, D: Integer);
var
  I: Integer;
begin
  FHeight := MulDiv(FHeight, M, D);
  FWidth := MulDiv(FWidth, M, D);
  for I := 0 to Count - 1 do
    Items[I].ChangeScale(M, D);
  InvalidateSize(ivBoth);
end;

procedure TdxOcNode.SetData(Value: TdxOcNodeData);
begin
  FData := Value;
end;

procedure TdxOcNode.DeleteChildren;
var
  I: Integer;
begin
  if Count = 0 then Exit;
  if (Owner.Selected <> nil) and Owner.Selected.HasAsParent(Self) then Selected := True;
  FDeleting := True;
  for I := 0 to Count - 1 do
    Owner.Delete(Items[I]);
  FDeleting := False;
  FList.Clear;
  if (FParent = nil) or (not FParent.Deleting) then
    InvalidateSize(ivBoth);
  if FParent <> nil then
    FExpanded := False;
end;

function TdxOcNode.FullRect: TRect;
var
  I: Integer;
  ALeft, ABottom: Integer;
begin
  Result := cxNullRect;
  if IsVisible then
  begin
    if Parent <> nil then
      Result := FParent.FullRect
    else
    begin
      ALeft := -Owner.LeftEdge;
      ABottom := -Owner.TopEdge;
      Owner.RotatePoint(ALeft, ABottom);
      Result.Left := ALeft;
      Result.Bottom := ABottom;
    end;
    Result.Top := Result.Bottom;
    Result.Bottom := Result.Top + ExtHeight + Owner.IndentY + Owner.LineWidth;
    if FParent <> nil then
      Inc(Result.Left, FParent.ChildOffset);
    for I := 0 to Index - 1 do
      Inc(Result.Left, FParent[I].FullWidth);
    Result.Right := Result.Left + FullWidth;
  end;
end;

procedure TdxOcNode.FullToDisplay(var Rect: TRect);
var
  W: Integer;
begin
  W := ExtWidth + Owner.LineWidth;
  with Rect do
  begin
    if ChildAlign = caRight then
      Left := Right - W;
    if ChildAlign = caCenter then
      Left := Left + (Right - Left - W) div 2;
    Right := Left + W;
  end;
end;

procedure TdxOcNode.DisplayToClient(var Rect: TRect);
var
  DX, DY: Integer;
begin
  DX := 0;
  DY := 0;
  case Shape of
    shDiamond:
      begin
        DX := Width div 4;
        DY := Height div 4;
      end;
    shRoundRect:
      begin
        DX := Radius * 3 div 20;
        DY := DX;
      end;
    shEllipse:
      begin
        DX := Width * 3 div 20;
        DY := Height * 3 div 20;
      end;
  end;
  Inc(DX, Owner.ScaleFactor.Apply(Owner.LineWidth div 2 + 1));
  Inc(DY, Owner.ScaleFactor.Apply(Owner.LineWidth div 2 + 1));

  Rect := cxRectInflate(Rect, -DX, -DY);
  if Owner.Rotated then
    Inc(Rect.Left, Owner.IndentY)
  else
    Inc(Rect.Top, Owner.IndentY);
end;

function TdxOcNode.ClientToText(var Rect: TRect): TPoint;
var
  ASize: TSize;
begin
  Result.X := -999999;
  Result.Y := -999999;
  if (ImageAlign = iaNone) or not IsImageAssigned(Owner.Images, ImageIndex) then
    Exit;

  ASize := dxGetImageSize(Owner.Images, Owner.ScaleFactor);
  ASize.cx := Owner.DoZoom(ASize.cx);
  ASize.cy := Owner.DoZoom(ASize.cy);
  case ImageAlign of
    iaLT, iaLC, iaLB:
      begin
        Result.X := Rect.Left;
        Rect.Left := Rect.Left + ASize.cx;
      end;
    iaRT, iaRC, iaRB:
      begin
        Rect.Right := Rect.Right - ASize.cx;
        Result.X := Rect.Right;
      end;
    iaTL, iaTC, iaTR:
      begin
        Result.Y := Rect.Top;
        Rect.Top := Rect.Top + ASize.cy;
      end;
    iaBL, iaBC, iaBR:
      begin
        Rect.Bottom := Rect.Bottom - ASize.cy;
        Result.Y := Rect.Bottom;
      end;
  end;
  case ImageAlign of
    iaLT, iaRT:
      Result.Y := Rect.Top;
    iaLB, iaRB:
      Result.Y := Rect.Bottom - ASize.cy;
    iaLC, iaRC:
      Result.Y := Rect.Top + (Rect.Bottom - Rect.Top - ASize.cy) div 2;
    iaTL, iaBL:
      Result.X := Rect.Left;
    iaTR, iaBR:
      Result.X := Rect.Right - ASize.cx;
    iaTC, iaBC:
      Result.X := Rect.Left + (Rect.Right - Rect.Left - ASize.cx) div 2;
  end;
end;

function TdxOcNode.DisplayRect: TRect;
begin
  Result := FullRect;
  if Result.Left <> Result.Right then
  begin
    FullToDisplay(Result);
    Result := Owner.RotateRect(Result);
  end;
end;

function TdxOcNode.ClientRect: TRect;
begin
  Result := DisplayRect;
  if Result.Left <> Result.Right then
    DisplayToClient(Result);
end;

function TdxOcNode.IsParentRoot: Boolean;
begin
  Result := FParent = Owner.RootNode;
end;

procedure TdxOcNode.MakeVisible;
var
  ANode: TdxOcNode;
  R: TRect;
begin
  Owner.FNoAnim := True;
  ANode := Parent;
  while ANode <> nil do
  begin
    ANode.Expand(False);
    ANode := ANode.Parent;
  end;
  Owner.FNoAnim := False;

  R := DisplayRect;
  if (cxRectWidth(R) > Owner.ClientBoundsWidth) or (cxRectHeight(R) > Owner.ClientBoundsHeight) then
    DisplayToClient(R);

  Owner.MakeVisible(R, vtFully);
end;

procedure TdxOcNode.MoveTo(Dest: TdxOcNode; Mode: TdxOcNodeAttachMode);
var
  ParNode: TdxOcNode;
  ParIdx: Integer;

  procedure SetPar(PN: TdxOcNode; PI: Integer);
  begin
    ParNode := PN;
    ParIdx := PI;
  end;

begin
  if Dest = nil then
    case Mode of
      naInsert: Exit;
      naAdd: Mode := naAddChild;
      naAddFirst: Mode := naAddChildFirst;
    end
  else
    if (Dest = Self) or Dest.HasAsParent(Self) or not Owner.IsMyNode(Dest)
    then Exit;
  case Mode of
    naInsert: SetPar(Dest.Parent, Dest.Index);
    naAdd: SetPar(Dest.Parent, -1);
    naAddFirst: SetPar(Dest.Parent, 0);
    naAddChild: SetPar(Dest, -1);
    naAddChildFirst: SetPar(Dest, 0);
  end;
  if ParNode = Parent then
  begin
    if ParIdx > Index then Dec(ParIdx);
    if (ParIdx = Index) or (ParIdx < 0) and (getNextSibling = nil) then Exit;
  end;
  Owner.InternalMoveTo(ParNode, Self, ParIdx);
end;

procedure TdxOcNode.SetColor(Value: TColor);
begin
  if Value <> FColor then
  begin
    FColor := Value;
    if FParent = nil then Exit;
    Owner.InvalidateNode(Self);
    Owner.NodeChanged(Self);
  end;
end;

procedure TdxOcNode.SetShape(Value: TdxOcShape);
begin
  if Value <> FShape then
  begin
    FShape := Value;
    if FParent = nil then Exit;
    with Owner do
    begin
      HideEditor(True);
      InvalidateNode(Self);
      if (FNodeAt = Self) and (htOnRect in FHitTests) then FHitTests := [];
      NodeChanged(Self);
    end;
  end;
end;

function TdxOcNode.ChildOffset: Integer;
begin
  if ChildAlign = caLeft then
    Result := 0
  else
  begin
    Result := FullWidth - ChildrenWidth;
    if ChildAlign = caCenter then
      Result := Result div 2;
  end;
end;

function TdxOcNode.Radius: Integer;
begin
  Result := Height;
  if Result > Width then Result := Width;
  Result := Result div 2;
end;

procedure TdxOcNode.SetFont(Font: TFont);
begin
  if Assigned(Owner.OnSetFont) then Owner.OnSetFont(Owner, Self, Font);
end;

procedure TdxOcNode.GetNodeInfo(var AInfo: TdxOcNodeInfo; ATargetScaleFactor: TdxScaleFactor = nil);
begin
  if ATargetScaleFactor = nil then
    ATargetScaleFactor := Owner.ScaleFactor;
  AInfo.Height := ATargetScaleFactor.Apply(FHeight, Owner.ScaleFactor);
  AInfo.Width := ATargetScaleFactor.Apply(FWidth, Owner.ScaleFactor);
  AInfo.Color := FColor;
  AInfo.Align := FChildAlign;
  AInfo.Shape := FShape;
  AInfo.Index := FImageIndex;
  AInfo.IAlign := FImageAlign;
end;

procedure TdxOcNode.SetImageIndex(Value: Smallint);
begin
  if Value <> FImageIndex then
  begin
    FImageIndex := Value;
    if FParent = nil then
      Exit;
    if Owner.Images <> nil then
      Owner.InvalidateNode(Self);
    Owner.NodeChanged(Self);
  end;
end;

procedure TdxOcNode.SetImageAlign(Value: TdxOcImageAlign);
begin
  if Value <> FImageAlign then
  begin
    FImageAlign := Value;
    if FParent = nil then
      Exit;
    if Owner.Images <> nil then
      Owner.InvalidateNode(Self);
    Owner.NodeChanged(Self);
  end;
end;

procedure TdxOcNode.AdjustSizes(const S: string);
var
  R: TRect;
  W, H, AImageWidth: Integer;
begin
  Inc(Owner.FZoom);
  try
    SetRect(R, 0, 0, Width + Owner.LineWidth, Height + Owner.LineWidth);
    if Owner.Rotated then
      Inc(R.Right, Owner.IndentY)
    else
      Inc(R.Bottom, Owner.IndentY);

    DisplayToClient(R);
    ClientToText(R);
    W := R.Right - R.Left;
    H := R.Bottom - R.Top;
    Owner.Canvas.Font := Owner.Font;
    SetFont(Owner.Canvas.Font);
    cxScreenCanvas.Font := Owner.Canvas.Font;
    Owner.DoDrawText(cxScreenCanvas.Handle, S, R, DT_CALCRECT);
    W := (R.Right - R.Left - W) * Width div W;

    if IsImageAssigned(Owner.Images, ImageIndex) then
    begin
      AImageWidth := dxGetImageSize(Owner.Images, Owner.ScaleFactor).cx;
      if emCenter in Owner.EditMode then
        AImageWidth := AImageWidth * 2;
      Inc(W, AImageWidth);
    end;

    H := (R.Bottom - R.Top - H) * Height div H;
    if (W > 0) and not (emWrap in Owner.EditMode) then
      Width := Width + W;
    if (H > 0) and (emGrow in Owner.EditMode) then
      Height := Height + H;
  finally
    Dec(Owner.FZoom);
    cxScreenCanvas.Dormant;
  end;
end;

procedure TdxOcNode.SetAnimXY(LeftX, TopY: Integer; const Clip: TRect; First: Boolean);
var
  R: TRect;
  I: Integer;
begin
  if First then FDraw := False;
  if FParent = nil then
  begin
    LeftX := -Owner.LeftEdge;
    TopY := -Owner.TopEdge;
    Owner.RotatePoint(LeftX, TopY);
    FDraw := True;
  end;
  if IsVisible then
  begin
    R.Left := LeftX; R.Top := TopY;
    R.Right := LeftX + FullWidth;
    if Self = Owner.FRoot then
      R.Bottom := TopY
    else
      R.Bottom := TopY + ExtHeight + Owner.IndentY + Owner.LineWidth;
    I := 0;
    if Expanded then
      I := 999999
    else
      if Owner.HasButton(Self) then I := 6 - Owner.LineWidth div 2;
    if I < 0 then I := 0;
    Inc(R.Bottom, I);
    if not ((R.Left > Clip.Right) or (R.Right < Clip.Left) or (R.Top > Clip.Bottom) or (R.Bottom < Clip.Top))
      then FDraw := True;
    Dec(R.Bottom, I);
    LeftX := LeftX + ChildOffset; TopY := R.Bottom;
    FullToDisplay(R);
    FAnimX := R.Left; FAnimY := R.Top;
  end
  else
  begin
    FDraw := FParent.FDraw;
    FAnimX := FParent.FAnimX;
    FAnimY := FParent.FAnimY + Owner.IndentY div 4;
    if FParent.ChildAlign <> caLeft then
    begin
      I := FParent.ExtWidth - ExtWidth;
      if FParent.ChildAlign = caCenter then I := I div 2;
      Inc(FAnimX, I);
    end;
  end;
  if First then
  begin
    FAnimX0 := FAnimX;
    FAnimY0 := FAnimY;
  end;
  if not (FDraw or First and (Owner.FCollapsed <> nil)) then Exit;
  if Expanded or (Owner.FCollapsed = Self) then
    for I := 0 to Count - 1 do
    begin
      Items[I].SetAnimXY(LeftX, TopY, Clip, First);
      if Expanded then Inc(LeftX, Items[I].FullWidth);
    end;
end;

procedure TdxOcNode.ReadData(Stream: TStream);
var
  Info: TdxOcNodeInfo;
begin
  Stream.ReadBuffer(Info, SizeOf(Info));
  Width := Owner.ScaleFactor.Apply(Info.Width);
  Height := Owner.ScaleFactor.Apply(Info.Height);
  Color := Info.Color;
  ChildAlign := Info.Align;
  Shape := Info.Shape;
  ImageIndex := Info.Index;
  ImageAlign := Info.IAlign;
end;

procedure TdxOcNode.WriteData(Stream: TStream);
var
  Info: TdxOcNodeInfo;
begin
  GetNodeInfo(Info, dxDefaultScaleFactor);
  Stream.WriteBuffer(Info, SizeOf(Info));
end;

procedure TdxOcNode.ReadChildren(Stream: TStream);
var
  Cnt: Word;
  Child, Par: TdxOcNode;
begin
  Text := ReadStr(Stream, Owner.IsUnicode);
  if FParent = nil then
    Par := nil
  else
    Par := Self;

  Stream.ReadBuffer(Cnt, SizeOf(Cnt));
  while Cnt > 0 do
  begin
    Child := Owner.AddChild(Par, nil);
    if Child = nil then Exit;
    Child.ReadData(Stream);
    Child.ReadChildren(Stream);
    Dec(Cnt);
  end;
end;

procedure TdxOcNode.WriteChildren(Stream: TStream);
var
  I: Integer;
begin
  WriteStr(Stream, Text);
  I := Count;
  Stream.WriteBuffer(I, SizeOf(Word));
  for I := 0 to Count - 1 do
  begin
    Items[I].WriteData(Stream);
    Items[I].WriteChildren(Stream);
  end;
end;

function TdxOcNode.GetText: string;
begin
  Result := FText;
  if Assigned(Owner.OnGetText) then
    Owner.OnGetText(Owner, self, FText);
end;

procedure TdxOcNode.SetText(const Value: string);
begin
  FText := Value;
  if Assigned(Owner.OnSetText) then
    Owner.OnSetText(Owner, self, Value);
end;

procedure TdxOcNode.InternalSetText(const Value: string);
begin
  if Text = Value then Exit;
  SetText(Value);
  if Owner.EditMode * [emWrap, emGrow] <> [emWrap] then AdjustSizes(Text);
  if FParent = nil then Exit;
  Owner.InvalidateNode(Self);
  Owner.NodeChanged(Self);
end;

{ TdxCustomOrgChart }

constructor TdxCustomOrgChart.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FScrollTimer := TTimer.Create(Self);
  FScrollTimer.Enabled := False;
  FScrollTimer.Interval := OtScrollTimerInterval;
  FScrollTimer.OnTimer := DoTimerScrolling;

  FRoot := TdxOcNode.Create(Self);
  FRoot.FExpanded := True;
  Width := 320;
  Height := 200;
  ParentColor := False;
  ParentFont := False;
  TabStop := True;
  FDefaultNodeWidth := 64;
  FDefaultNodeHeight := 40;
  FIndentX := 16;
  FIndentY := 16;
  FLineWidth := 1;
  FLineColor := clDefault;
  FSelectedNodeColor := clDefault;
  FSelectedNodeTextColor := clDefault;
  FSizeChanged := True;
  FOptions := [ocSelect, ocFocus, ocButtons, ocDblClick, ocEdit, ocCanDrag, ocShowDrag];
  FEditMode := [emWrap];
  FZoom := 1;
  FPainter := CreatePainter;
  FImagesChangeLink := TChangeLink.Create;
  FImagesChangeLink.OnChange := ImageListChange;
  DoubleBuffered := True;
  BorderStyle := bsSingle;
  Color := clDefault;
end;

destructor TdxCustomOrgChart.Destroy;
begin
  StopAnimation;
  HideEditor(False);
  FreeAndNil(FEditor);
  Selected := nil;
  Images := nil;
  FImagesChangeLink.Free;
  FRoot.HasChildren := False;
  FRoot.FDeleting := True;
  FreeAndNil(FRoot);
  FreeAndNil(FPainter);
  FreeAndNil(FScrollTimer);
  inherited Destroy;
end;

function TdxCustomOrgChart.CreatePainter: TdxOrgChartCustomPainter;
begin
  if Antialiasing and CheckGdiPlus then
    Result := TdxOrgChartGdiPlusPainter.Create(Self)
  else
    Result := TdxOrgChartCustomPainter.Create(Self);
end;

procedure TdxCustomOrgChart.ChangeScaleEx(M, D: Integer; isDpiChange: Boolean);
begin
  BeginUpdate;
  try
    inherited;
    DefaultNodeHeight := MulDiv(DefaultNodeHeight, M, D);
    DefaultNodeWidth := MulDiv(DefaultNodeWidth, M, D);
    IndentX := MulDiv(IndentX, M, D);
    IndentY := MulDiv(IndentY, M, D);
    RootNode.ChangeScale(M, D);
  finally
    EndUpdate;
  end;
end;

procedure TdxCustomOrgChart.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.WindowClass.Style := Params.WindowClass.Style or CS_HREDRAW or CS_VREDRAW;
  Params.Style := (Params.Style or WS_TABSTOP) and not (WS_HSCROLL or WS_VSCROLL);
end;

procedure TdxCustomOrgChart.CreateHandle;
begin
  inherited CreateHandle;
  if FZoom = 0 then
    UpdateScrollBars;
end;

procedure TdxCustomOrgChart.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = Images) then
    Images := nil;
end;

procedure TdxCustomOrgChart.Rotate(AAngle: Double; var AHandled: Boolean);
begin
  AHandled := True;
  if (AAngle > 0) and not Rotated or
    (AAngle < 0) and Rotated then
  begin
    if Abs(AAngle) >= 0.5 then
      Rotated := not Rotated;
  end;
end;

procedure TdxCustomOrgChart.SetBorderStyle(Value: TBorderStyle);
const
  BorderStyleMap: array[TBorderStyle] of TcxControlBorderStyle = (cxcbsNone, cxcbsDefault);
begin
  if Value <> BorderStyle then
  begin
    FBorderStyle := Value;
    inherited BorderStyle := BorderStyleMap[BorderStyle];
    if HandleAllocated then Invalidate;
  end;
end;

function TdxCustomOrgChart.IsMyNode(Value: TdxOcNode): Boolean;
begin
  if (Value = nil) or (Value = FRoot) then
    Result := True
  else
    Result := Value.HasAsParent(FRoot);
end;

procedure TdxCustomOrgChart.DoAdd(AParent, ANode: TdxOcNode; AIndex: Integer);
begin
  if AParent = nil then AParent := FRoot;
  if AIndex < 0 then AIndex := AParent.Count;
  ANode.FParent := AParent;
  with AParent do
  begin
    HasChildren := True;
    FList.Insert(AIndex, ANode);
    Enumerate(AIndex);
    InvalidateSize(ivBoth);
  end;
end;

function TdxCustomOrgChart.InternalAdd(ParentNode: TdxOcNode; Data: TdxOcNodeData; Idx: Integer): TdxOcNode;
begin
  if IsMyNode(ParentNode) then
    Result := CreateNode
  else
    Result := nil;

  if Result <> nil then
  begin
    Result.FData := Data;
    if Assigned(OnCreateNode) then
      OnCreateNode(Self, Result);
    DoAdd(ParentNode, Result, Idx);
    Inc(FCount);
    if Count = 1 then
      Selected := Result;
  end;
end;

procedure TdxCustomOrgChart.InternalMoveTo(ParentNode, Node: TdxOcNode; Idx: Integer);
begin
  with Node.FParent do
  begin
    FList.Delete(Node.Index);
    Enumerate(Node.Index);
    if Node.Parent <> ParentNode then
      InvalidateSize(ivBoth);
    if FList.Count = 0 then
    begin
      HasChildren := False;
      if (FParent <> nil) then
        FExpanded := False;
    end;
  end;
  DoAdd(ParentNode, Node, Idx);
end;

function TdxCustomOrgChart.Insert(Node: TdxOcNode; Data: TdxOcNodeData): TdxOcNode;
begin
  if Node = nil then
    Result := nil
  else
    Result := InternalAdd(Node.Parent, Data, Node.Index);
end;

function TdxCustomOrgChart.Add(Node: TdxOcNode; Data: TdxOcNodeData): TdxOcNode;
begin
  if Node = nil then
    Result := AddChild(nil, Data)
  else
    Result := InternalAdd(Node.Parent, Data, -1);
end;

function TdxCustomOrgChart.AddFirst(Node: TdxOcNode; Data: TdxOcNodeData): TdxOcNode;
begin
  if Node = nil then
    Result := AddChildFirst(nil, Data)
  else
    Result := InternalAdd(Node.Parent, Data, 0);
end;

function TdxCustomOrgChart.AddChild(Node: TdxOcNode; Data: TdxOcNodeData): TdxOcNode;
begin
  Result := InternalAdd(Node, Data, -1);
end;

function TdxCustomOrgChart.AddChildFirst(Node: TdxOcNode; Data: TdxOcNodeData): TdxOcNode;
begin
  Result := InternalAdd(Node, Data, 0);
end;

function TdxCustomOrgChart.AllowTouchScrollUIMode: Boolean;
begin
  Result := not IsDesigning;
end;

function TdxCustomOrgChart.GetAbsoluteItem(Index: Integer): TdxOcNode;
begin
  Result := RootNode.AbsoluteItems[Index];
end;

function TdxCustomOrgChart.GetItem(Index: Integer): TdxOcNode;
begin
  Result := FRoot.GetItem(Index);
end;

function TdxCustomOrgChart.GetRootCount: Integer;
begin
  Result := RootNode.Count;
end;

function TdxCustomOrgChart.HasButton(Node: TdxOcNode): Boolean;
begin
  Result := (ocButtons in Options) and (Node.Count > 0) and not (IsZoomed and Node.Expanded);
end;

procedure TdxCustomOrgChart.RotatePoint(var X, Y: LongInt);
var
  Tmp: Integer;
begin
  if Rotated then
  begin
    Tmp := X; X := Y; Y := Tmp;
  end;
end;

function TdxCustomOrgChart.RotateRect(const Rect: TRect): TRect;
begin
  if Rotated then
    Result := cxRectRotate(Rect)
  else
    Result := Rect;
end;

procedure TdxCustomOrgChart.Paint;
var
  AClipRect: TRect;
begin
  inherited Paint;
  if csPaintCopy in ControlState then
    AClipRect := FullRect
  else
    AClipRect := Canvas.Canvas.ClipRect;

  PaintContent(Canvas, AClipRect);
end;

procedure TdxCustomOrgChart.PaintContent(ACanvas: TcxCanvas; const AClipRect: TRect);
var
  ATempFont: TFont;
begin
  if not IsRectEmpty(AClipRect) then
  begin
    FRoot.SetAnimXY(0, 0, RotateRect(AClipRect), AnimationStep = 0);
    ATempFont := TFont.Create;
    try
      ATempFont.Assign(Font);
      ATempFont.Height := DoZoom(Font.Height);
      Painter.DrawBackground(ACanvas, AClipRect);
      PaintNode(ACanvas, ATempFont, FRoot, RotateRect(AClipRect));
    finally
      ATempFont.Free;
    end;
    FUpdated := False;
  end;
end;

procedure TdxCustomOrgChart.PaintContentTo(ACanvas: TCanvas; X, Y: Integer);
var
  AcxCanvas: TcxCanvas;
begin
  AcxCanvas := TcxCanvas.Create(ACanvas);
  try
    MoveWindowOrg(AcxCanvas.Handle, X, Y);
    PaintContent(AcxCanvas, FullRect);
    MoveWindowOrg(AcxCanvas.Handle, -X, -Y);
  finally
    AcxCanvas.Free;
  end;
end;

function TdxCustomOrgChart.PaintNode(ACanvas: TcxCanvas;
  AFont: TFont; ANode: TdxOcNode; const AUpdateRect: TRect): TPoint;

  function InRect(const R: TRect): Boolean;
  begin
    Result := not ((R.Left > AUpdateRect.Right) or (R.Right < AUpdateRect.Left) or
      (R.Top > AUpdateRect.Bottom) or (R.Bottom < AUpdateRect.Top));
  end;

  function CalculateAnimationRect(ANode: TdxOcNode; ACoef: Integer): TRect;
  begin
    Result.Left := ANode.FAnimX + (ANode.FAnimX0 - ANode.FAnimX) * ACoef div OtMaxAnimFrames;
    Result.Top := ANode.FAnimY + (ANode.FAnimY0 - ANode.FAnimY) * ACoef div OtMaxAnimFrames;
    Result.Right := Result.Left + ANode.ExtWidth + LineWidth;
    Result.Bottom := Result.Top + ANode.ExtHeight + IndentY + LineWidth;
  end;

  procedure ExtLine(X1, Y1, X2, Y2: Integer);
  var
    ALineWidth: Single;
  begin
    RotatePoint(X1, Y1);
    RotatePoint(X2, Y2);
    ALineWidth := FLineWidth;
    if IsZoomed then
      ALineWidth := FLineWidth * FZoomLo / FZoomHi;
    Painter.DrawLine(ACanvas, X1, Y1, X2, Y2, ALineWidth);
  end;

  procedure PaintButton(X, Y: Integer; Value: Boolean);
  var
    ARect: TRect;
    ASize: Integer;
  begin
    if Rotated then
    begin
      RotatePoint(X, Y);
      Dec(X);
      Inc(Y);
    end;
    ASize := LookAndFeelPainter.ScaledExpandButtonSize(ScaleFactor);
    ARect := cxRectCenter(Rect(X, Y, X, Y), ASize, ASize);
    ARect := cxRectOffset(ARect, ASize mod 2, 0);
    LookAndFeelPainter.DrawScaledExpandButton(ACanvas, ARect, Value, ScaleFactor);
  end;

  procedure PrepareCanvas(ACanvas: TcxCanvas; ANode: TdxOcNode; ASelected: Boolean);
  var
    ABrushColor: TColor;
  begin
    ABrushColor := Painter.GetNodeColor(ANode.Color, ASelected);
    if ABrushColor <> clNone then
      ACanvas.Brush.Color := ABrushColor
    else
      ACanvas.Brush.Style := bsClear;

    ACanvas.Font := AFont;
    ACanvas.Font.Color := Painter.GetNodeTextColor(ASelected);
  end;

var
  ALine: TPoint;
  AOffset: Integer;
  APlaceRect: TRect;
  I, ALineLeft, ALineRight: Integer;
begin
  APlaceRect := CalculateAnimationRect(ANode, AnimationStep);
  if ANode = FRoot then
    APlaceRect.Bottom := APlaceRect.Top;

  Result.X := (APlaceRect.Left + APlaceRect.Right) div 2;
  Result.Y := (APlaceRect.Top + IndentY div 2);

  if not ANode.FDraw then Exit;

  if (APlaceRect.Bottom < AUpdateRect.Bottom) and (ANode.Expanded or (FCollapsed = ANode)) then
  begin
    ALineLeft := 0;
    ALineRight := -1;
    for I := 0 to ANode.Count - 1 do
    begin
      ALine := PaintNode(ACanvas, AFont, ANode[I], AUpdateRect);
      ALineRight := ALine.X;
      if (I = 0) or (ALine.X < AUpdateRect.Left) then
        ALineLeft := ALine.X;
      if ALine.X >= AUpdateRect.Right then
        Break;
    end;
    if not ((ANode = FRoot) and (ANode.Count <= 1)) then
    begin
      if (ANode = FRoot) and (ANode.Count > 1) then
        Result.X := ALineRight;
      ALineRight := Max(ALineRight, Result.X);
      ALineLeft := Min(ALineLeft, Result.X);
      ExtLine(ALineLeft, ALine.Y, ALineRight, ALine.Y);
    end;
    if (ANode <> FRoot) and (ALine.Y > APlaceRect.Bottom) then
      ExtLine(Result.X, APlaceRect.Bottom - LineWidth, Result.X, ALine.Y);
  end;

  if HasButton(ANode) then
    AOffset := Max(0, LookAndFeelPainter.ScaledExpandButtonSize(ScaleFactor))
  else
    AOffset := 0;

  Inc(APlaceRect.Bottom, AOffset);
  if (ANode <> FRoot) and InRect(APlaceRect) then
  begin
    Dec(APlaceRect.Bottom, AOffset);
    AOffset := LineWidth;
    if ANode.Is3D then
    begin
      Dec(APlaceRect.Right, AOffset);
      Dec(APlaceRect.Bottom, AOffset);
    end;
    AOffset := AOffset div 2;
    InflateRect(APlaceRect, -AOffset, -AOffset);
    PrepareCanvas(ACanvas, ANode, (ocSelect in Options) and ANode.Focused);

    Inc(APlaceRect.Top, IndentY);
    if (ANode.Parent <> nil) or (FRoot.Count > 1) then
      ExtLine(Result.X, Result.Y, Result.X, APlaceRect.Top);
    Painter.DrawFrame(ACanvas, ANode, RotateRect(APlaceRect));
    Dec(APlaceRect.Top, IndentY);

    AOffset := APlaceRect.Bottom;
    APlaceRect := RotateRect(APlaceRect);
    ANode.DisplayToClient(APlaceRect);
    if InRect(RotateRect(APlaceRect)) then
    begin
      ANode.SetFont(ACanvas.Font);
      DrawNode(ANode, ACanvas.Canvas, APlaceRect);
      if (ocFocus in Options) and ANode.Selected then
        ACanvas.DrawFocusRect(APlaceRect);
    end;
    if HasButton(ANode) then
      PaintButton(Result.X, AOffset, ANode.Expanded);
  end;
end;

procedure TdxCustomOrgChart.DoDrawText(Handle: HDC; Text: string; var Rect: TRect; Flags: Integer);
var
  ARect: TRect;
  HText: Integer;
begin
  ARect := Rect;
  Flags := Flags or DT_EXPANDTABS;
  if emLeft in EditMode then
    Flags := Flags or DT_LEFT;
  if emCenter in EditMode then
    Flags := Flags or DT_CENTER;
  if emRight in EditMode then
    Flags := Flags or DT_RIGHT;
  if emWrap in EditMode then
    Flags := Flags or DT_WORDBREAK;
  if emVCenter in EditMode then
  begin
    HText := DrawText(Handle, PChar(Text), -1, ARect, Flags or DT_CALCRECT);
    if (Rect.Bottom - Rect.Top) > HText then
    begin
      inc(Rect.Top, (Rect.Bottom - Rect.Top - HText) div 2);
      Rect.Bottom := Rect.Top + HText;
    end;
  end;
  DrawText(Handle, PChar(Text), -1, Rect, Flags or DT_EDITCONTROL);
end;

procedure TdxCustomOrgChart.DrawNode(Node: TdxOcNode; ACanvas: TCanvas; Rect: TRect);
begin
  if Assigned(OnDrawNode) then
    OnDrawNode(Self, Node, ACanvas, Rect)
  else
    DefaultDrawNode(Node, ACanvas, Rect);
end;

function TdxCustomOrgChart.GetContentSize: TSize;
begin
  Result := cxSize(FullWidth, FullHeight);
end;

function TdxCustomOrgChart.GetNodeClass: TdxOcNodeClass;
begin
  Result := TdxOcNode;
end;

procedure TdxCustomOrgChart.DefaultDrawNode(Node: TdxOcNode; ACanvas: TCanvas; R: TRect);
var
  AImageRect: TRect;
  AScaleFactor: TdxScaleFactor;
  P: TPoint;
begin
  P := Node.ClientToText(R);
  if (P.X + P.Y >= -999999) and (Node.ImageIndex <> -1) then
  begin
    AScaleFactor := TdxScaleFactor.Create;
    try
      AScaleFactor.Assign(ScaleFactor);
      if IsZoomed then
        AScaleFactor.Change(FZoomLo, FZoomHi);
      AImageRect := cxRectOffset(cxRect(dxGetImageSize(Images, AScaleFactor)), P);
      cxDrawImage(ACanvas.Handle, AImageRect, AImageRect, nil, Images, Node.ImageIndex, idmNormal, ocImageFiltering in Options);
    finally
      AScaleFactor.Free;
    end;
  end;
  DoDrawText(ACanvas.Handle, Node.Text, R, 0);
end;

function TdxCustomOrgChart.InitAnimate(Node: TdxOcNode): Boolean;
begin
  Result := not FNoAnim and (ocAnimate in Options) and HandleAllocated and Node.IsVisible;
  if Result then
  begin
    FCollapsed := Node;
    FRoot.SetAnimXY(0, 0, ClientBounds, True);
    StopAnimation;
    FAnimationStep := OtMaxAnimFrames - 1;
    FAnimationThread := TdxOrgChartAnimationThread.Create(Self);
  end;
end;

procedure TdxCustomOrgChart.StopAnimation;
begin
  FreeAndNil(FAnimationThread);
end;

function TdxCustomOrgChart.MinSizes: TPoint;
var
  ASize: TSize;
begin
  ASize := dxGetImageSize(Images, ScaleFactor);
  Result.X := Max(ASize.cx * 2, 16);
  Result.Y := Max(ASize.cy * 2, 16);
end;

procedure TdxCustomOrgChart.SetNodeWidth(Value: Word);
var
  P: TPoint;
begin
  P := MinSizes;
  if Value < P.X then Value := P.X;
  if Value = DefaultNodeWidth then Exit;
  FDefaultNodeWidth := Value;
  if Rotated then
    InvalidateSizes(ivHeight)
  else
    InvalidateSizes(ivWidth);
end;

procedure TdxCustomOrgChart.SetNodeHeight(Value: Word);
var
  P: TPoint;
begin
  P := MinSizes;
  if Value < P.Y then Value := P.Y;
  if Value = DefaultNodeHeight then Exit;
  FDefaultNodeHeight := Value;
  if Rotated then
    InvalidateSizes(ivWidth)
  else
    InvalidateSizes(ivHeight);
end;

procedure TdxCustomOrgChart.SetIndentX(Value: Word);
begin
  if Value = FIndentX then Exit;
  FIndentX := Value;
  if Rotated then
    InvalidateSizes(ivHeight)
  else
    InvalidateSizes(ivWidth);
end;

procedure TdxCustomOrgChart.SetIndentY(Value: Word);
begin
  if Value = FIndentY then Exit;
  FIndentY := Value;
  if Rotated then
    InvalidateSizes(ivWidth)
  else
    InvalidateSizes(ivHeight);
end;

function TdxCustomOrgChart.IsUpdated: Boolean;
begin
  Result := FUpdated and (FUpdate <> 0);
  if not Result then
    FUpdated := False;
end;

procedure TdxCustomOrgChart.Invalidate;
begin
  if not IsUpdated then
    inherited Invalidate;
  FUpdated := True;
end;

procedure TdxCustomOrgChart.InvalidateSizes(AFlags: TdxOcIvFlags);

  procedure InternalInvalidateSizes(Node: TdxOcNode; AFlags: TdxOcIvFlags);
  var
    I: Integer;
  begin
    if AFlags in [ivWidth, ivBoth] then
      Node.FChildrenWidth := 0;
    if AFlags in [ivHeight, ivBoth] then
      Node.FChildrenHeight := 0;
    for I := 0 to Node.Count - 1 do
      InternalInvalidateSizes(Node[I], AFlags);
  end;

begin
  InternalInvalidateSizes(FRoot, AFlags);
  ChangeSize;
end;

procedure TdxCustomOrgChart.SetLineColor(Value: TColor);
begin
  if Value = clNone then
    Value := clDefault;
  if Value <> LineColor then
  begin
    FLineColor := Value;
    if HandleAllocated then
      Invalidate;
  end;
end;

procedure TdxCustomOrgChart.SetLineWidth(Value: Word);
begin
  if Value = 0 then
    Value := 1;
  if Value = FLineWidth then
    Exit;
  FLineWidth := Value;
  InvalidateSizes(ivBoth);
end;

procedure TdxCustomOrgChart.SetSelectedNodeColor(Value: TColor);
begin
  if Value = clNone then
    Value := clDefault;
  if Value <> SelectedNodeColor then
  begin
    FSelectedNodeColor := Value;
    if HandleAllocated then
      Invalidate;
  end;
end;

procedure TdxCustomOrgChart.SetSelectedNodeTextColor(Value: TColor);
begin
  if Value = clNone then
    Value := clDefault;
  if Value <> SelectedNodeTextColor then
  begin
    FSelectedNodeTextColor := Value;
    if HandleAllocated then
      Invalidate;
  end;
end;

procedure TdxCustomOrgChart.FullExpand;
begin
  FRoot.Expand(True);
end;

procedure TdxCustomOrgChart.FullCollapse;
begin
  FRoot.Collapse(True);
end;

procedure TdxCustomOrgChart.Delete(Node: TdxOcNode);
begin
  if (Node = nil) or not IsMyNode(Node) then
    Exit;
  if Node.Selected or ((Selected <> nil) and Selected.HasAsParent(Node)) then
  begin
    Selected := Node.getNextSibling;
    if Selected = nil then
      Selected := Node.getPrevSibling;
    if Selected = nil then
      Selected := Node.Parent;
  end;
  if Assigned(OnDeletion) then
    OnDeletion(Self, Node);
  with Node do
  begin
    HasChildren := False;
    FDeleting := True;
    Data := nil;
  end;
  with Node.FParent do
    if not Deleting then
    begin
      FList.Delete(Node.Index);
      Enumerate(Node.Index);
      InvalidateSize(ivBoth);
      if (FList.Count = 0) and (FParent <> nil) then
        FExpanded := False;
      if FList.Count = 0 then
        HasChildren := False;
    end;
  Dec(FCount);
  Node.Free;
end;

function TdxCustomOrgChart.CreateNode: TdxOcNode;
begin
  Result := GetNodeClass.Create(Self);
end;

procedure TdxCustomOrgChart.BeginUpdate;
begin
  if (FUpdate = 0) and HandleAllocated then
    SendMessage(Handle, WM_SETREDRAW, 0, 0);
  Inc(FUpdate);
end;

procedure TdxCustomOrgChart.EndUpdate;
begin
  if FUpdate = 0 then
    Exit;
  Dec(FUpdate);
  if (FUpdate = 0) and HandleAllocated then
  begin
    SendMessage(Handle, WM_SETREDRAW, 1, 0);
    FUpdated := False;
    Invalidate;
  end;
end;

procedure TdxCustomOrgChart.ChangeSize;
begin
  if FSizeChanged then
    Exit;
  HideEditor(True);
  Invalidate;
  FSizeChanged := True;
  FHitTests := [];
end;

procedure TdxCustomOrgChart.RecalcSizes;
begin
  if FSizeChanged then
  begin
    FSizeChanged := False;
    if FZoom = 0 then
      SetZoomRatio
    else
      if IsPopupScrollBars then
        LayoutChanged(ctLight)
      else
        UpdateScrollBars;
  end;
end;

function TdxCustomOrgChart.FullWidth: Integer;
begin
  if Rotated then
    Result := FRoot.ChildrenHeight + 6
  else
    Result := FRoot.ChildrenWidth;
end;

function TdxCustomOrgChart.FullHeight: Integer;
begin
  if Rotated then
    Result := FRoot.ChildrenWidth
  else
    Result := FRoot.ChildrenHeight + 6;
end;

function TdxCustomOrgChart.GetLeftEdge: Integer;
begin
  Result := LeftPos;
end;

function TdxCustomOrgChart.GetTopEdge: Integer;
begin
  Result := TopPos;
end;

procedure TdxCustomOrgChart.SetAntialiasing(AValue: Boolean);
var
  AOldPainter: TdxOrgChartCustomPainter;
begin
  if AValue <> FAntialiasing then
  begin
    AOldPainter := FPainter;
    FAntialiasing := AValue;
    FPainter := CreatePainter;
    FreeAndNil(AOldPainter);
    Invalidate;
  end;
end;

procedure TdxCustomOrgChart.SetLeftEdge(Value: Integer);
begin
  LeftPos := Value;
end;

procedure TdxCustomOrgChart.SetTopEdge(Value: Integer);
begin
  TopPos := Value;
end;

procedure TdxCustomOrgChart.Clear;
begin
  Selected := nil;
  FRoot.DeleteChildren;
end;

function TdxCustomOrgChart.InvalidateNode(Value: TdxOcNode): Boolean;
var
  R: TRect;
begin
  Result := False;
  if HandleAllocated and not IsUpdated then
  begin
    R := Value.DisplayRect;
    if not IsRectEmpty(R) then
    begin
      if Rotated then
        Dec(R.Left, 6)
      else
        Dec(R.Top, 6);

      Result := True;
      if HasButton(Value) then
      begin
        if Rotated then
          Inc(R.Right, 6)
        else
          Inc(R.Bottom, 6);
      end;
      InflateRect(R, 2 * FLineWidth, 2 * FLineWidth);
      InvalidateRect(R, True);
    end;
  end;
end;

function TdxCustomOrgChart.InvalidateSel: Boolean;
begin
  Result := (Selected <> nil) and InvalidateNode(Selected);
end;

procedure TdxCustomOrgChart.SetSelected(Value: TdxOcNode);
var
  Edit: Boolean;
begin
  if (Value = Selected) or not IsMyNode(Value) then
    Exit;
  Edit := Editing;
  HideEditor(True);
  if InvalidateSel then
  begin
    FSelected := nil;
    if (Value <> nil) and Value.IsVisible then
      Update;
  end;
  FSelected := Value;
  InvalidateSel;
  Editing := Edit;
  DoChange(Value);
end;

function TdxCustomOrgChart.GetFirstNode: TdxOcNode;
begin
  if Count = 0 then
    Result := nil
  else
    Result := FRoot[0];
end;

procedure TdxCustomOrgChart.HitTestsAt(X, Y: Integer);

  procedure InRect(Node: TdxOcNode; LeftX, TopY: Integer);
  var
    R, RN: TRect;
    I: Integer;

    function InFrame(Sh: TdxOcShape): Boolean;
    label
      Ell;
    var
      X0, Y0, X1, Y1, A, B: Integer;
    begin
      Result := False;

      if Sh = shRectangle then
      begin
        Result := True;
        Exit;
      end;

      case Node.ChildAlign of
        caLeft:
          begin
            X0 := 0;
            X1 := -IndentX;
          end;
        caCenter:
          begin
            X0 := IndentX div 2;
            X1 := -(IndentX div 2);
          end;
      else
        X0 := IndentX;
        X1 := 0;
      end;

      if Sh = shRoundRect then
      begin
        A := Node.Radius div 2;
        Inc(X0, (R.Left + A)); Inc(X1, (R.Right - A));
        Y0 := R.Top + A + IndentY; Y1 := R.Bottom - A;
        if ((X >= X0) and (X <= X1)) or ((Y >= Y0) and (Y <= Y1)) then
        begin
          Result := True;
          Exit;
        end;
        if X > X0 then X0 := X1;
        if Y > Y0 then Y0 := Y1;
        X1 := X - X0;
        Y1 := Y - Y0;
        goto Ell;
      end;

      A := (R.Right - R.Left - IndentX) div 2;
      B := (R.Bottom - R.Top - IndentY) div 2;
      Inc(X0, (R.Left + A)); Y0 := R.Top + B + IndentY;
      X1 := Abs(X - X0); Y1 := Abs(Y - Y0);
      if Sh = shDiamond then
      begin
        if X1 * B + Y1 * A <= A * B then Result := True;
        Exit;
      end;

      if A >= B then
        Y1 := Y1 * A div B
      else
      begin
        X1 := X1 * B div A;
        A := B;
      end;
      Ell:
      if X1 * X1 + Y1 * Y1 <= A * A then
        Result := True;
    end;

  var
    AExpandButtonAreaSize: Integer;
  begin
    R.Left := LeftX; R.Top := TopY;
    R.Right := R.Left + Node.FullWidth;
    if Node = FRoot then
      R.Bottom := R.Top - 7
    else
      R.Bottom := R.Top + Node.ExtHeight + IndentY + LineWidth;
    if (X < R.Left) or (X >= R.Right) then Exit;
    if (Y >= R.Top) and (Y < R.Bottom + 6) then
    begin
      FHitTests := []; FNodeAt := Node;
      RN := R; Node.FullToDisplay(RN);
      RN.Top := RN.Top + IndentY;
      if Y < RN.Top then
        Include(FHitTests, htOnIndentY);
      if X < RN.Left then
        Include(FHitTests, htOnLeftIndentX);
      if X > RN.Right then
        Include(FHitTests, htOnRightIndentX);
      if FHitTests = [] then
      begin
        if Y < RN.Bottom then
        begin
          FHitTests := [htOnRect];
          if Node.Is3D then Dec(RN.Bottom, LineWidth);
          if InFrame(Node.Shape) then
            Include(FHitTests, htOnShape);
        end;
        if HasButton(Node) then
        begin
          AExpandButtonAreaSize := LookAndFeelPainter.ScaledExpandButtonAreaSize(ScaleFactor);
          I := Node.ExtWidth div 2 - AExpandButtonAreaSize div 2;
          if (X >= RN.Left + I) and (X < RN.Left + I + AExpandButtonAreaSize) and (Y >= RN.Bottom - AExpandButtonAreaSize div 2)
            then Include(FHitTests, htOnButton);
        end;
      end;
      if (Y >= R.Bottom) and not (htOnButton in FHitTests) then FNodeAt := nil;
    end;
    if FNodeAt = nil then
    begin
      if not Node.Expanded then
      begin
        if (Y >= R.Bottom) and (Y <= R.Bottom + Node.ExtHeight) then
        begin
          FNodeAt := Node;
          FHitTests := [htUnder];
        end;
        Exit;
      end;
      R.Left := R.Left + Node.ChildOffset;
      if R.Bottom < R.Top then R.Bottom := R.Top;
      for I := 0 to Node.Count - 1 do
      begin
        InRect(Node[I], R.Left, R.Bottom);
        if FNodeAt <> nil then Exit;
        R.Left := R.Left + Node[I].FullWidth;
      end;
    end;
  end;

begin
  FNodeAt := nil;
  FHitX := X; FHitY := Y;
  RotatePoint(X, Y);
  if Rotated then
    InRect(FRoot, -TopEdge, -LeftEdge)
  else
    InRect(FRoot, -LeftEdge, -TopEdge);
  if FNodeAt = nil then FHitTests := [htNowhere];
end;

function TdxCustomOrgChart.GetNodeAt(X, Y: Integer): TdxOcNode;
begin
  if (FHitTests = []) or (X <> FHitX) or (Y <> FHitY) then HitTestsAt(X, Y);
  Result := FNodeAt;
end;

function TdxCustomOrgChart.GetHitTestsAt(X, Y: Integer): TdxOcHitTests;
begin
  if (FHitTests = []) or (X <> FHitX) or (Y <> FHitY) then HitTestsAt(X, Y);
  Result := FHitTests;
end;

procedure TdxCustomOrgChart.DoChange(Node: TdxOcNode);
begin
  if Assigned(OnChange) then OnChange(Self, Node);
end;

procedure TdxCustomOrgChart.DoChanging(Node: TdxOcNode; var Allow: Boolean);
begin
  Allow := True;
  if Assigned(OnChanging) then OnChanging(Self, Node, Allow);
end;

procedure TdxCustomOrgChart.DoNavigate(ANavigateCode: TdxOcNavigate; AValue: Integer);
var
  ANextNode: TdxOcNode;
begin
  case ANavigateCode of
    ocnLineLeft:
      LeftEdge := LeftEdge - OtScrollUnit;
    ocnLineUp:
      TopEdge := TopEdge - OtScrollUnit;
    ocnLineRight:
      LeftEdge := LeftEdge + OtScrollUnit;
    ocnLineDown:
      TopEdge := TopEdge + OtScrollUnit;
    ocnPageLeft:
      LeftEdge := LeftEdge - ClientBoundsWidth + OtScrollUnit;
    ocnPageUp:
      TopEdge := TopEdge - ClientBoundsHeight + OtScrollUnit;
    ocnPageRight:
      LeftEdge := LeftEdge + ClientBoundsWidth - OtScrollUnit;
    ocnPageDown:
      TopEdge := TopEdge + ClientBoundsHeight - OtScrollUnit;
    ocnLeft:
      LeftEdge := 0;
    ocnTop:
      TopEdge := 0;
    ocnRight:
      LeftEdge := FullWidth - ClientBoundsWidth;
    ocnBottom:
      TopEdge := FullHeight - ClientBoundsHeight;
    ocnLeftPosition:
      LeftEdge := AValue;
    ocnTopPosition:
      TopEdge := AValue;
    ocnSelectNextLeft:
      begin
        ANextNode := NextSel(GetPS);
        if ANextNode = nil then
          ANextNode := NextSel(GetPV);
        SelectNode(ANextNode);
      end;
    ocnSelectNextUp:
      SelectNode(NextSel(GetPV));
    ocnSelectNextRight:
      begin
        ANextNode := NextSel(GetNS);
        if ANextNode = nil then
          ANextNode := NextSel(GetNV);
        SelectNode(ANextNode);
      end;
    ocnSelectNextDown:
      SelectNode(NextSel(GetNV));
  end;
end;

function TdxCustomOrgChart.CanSelect(Node: TdxOcNode): Boolean;
begin
  DoChanging(Node, Result);
end;

function TdxCustomOrgChart.NextSel(Get: TdxOcNodeFunc): TdxOcNode;
begin
  if Selected = nil then
    Result := nil
  else
    Result := Get(Selected);
  while (Result <> nil) and not CanSelect(Result) do
    Result := Get(Result);
end;

procedure TdxCustomOrgChart.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);

  procedure DoSelect;
  begin
    if CanSelect(FNodeAt) then Selected := FNodeAt;
  end;

begin
  if not IsScrollBarsArea(Point(X, Y)) and (Button = mbLeft) and not (ssDouble in Shift) then
  begin
    SetFocus;
    FDrag := True;
    GetNodeAt(X, Y);
    if FHitTests * [htOnShape, htOnButton] = [htOnShape] then DoSelect;
    if htOnButton in FHitTests then
    begin
      if (Selected <> nil) and FNodeAt.Expanded and Selected.HasAsParent(FNodeAt)
        then DoSelect;
      with FNodeAt do
        Expanded := not Expanded;
    end;
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TdxCustomOrgChart.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  AShift: TShiftState;
begin
  AShift := Shift;
  AShift := AShift - [ssPen, ssTouch];
  if not IsScrollBarsArea(Point(X, Y)) and FDrag and (ocCanDrag in Options) and
    not Dragging and ((AShift = [ssLeft]) or (AShift = [ssLeft, ssCtrl])) and
    (htOnShape in GetHitTestsAt(X, Y)) and (FNodeAt = Selected) then
    BeginDrag(False);
  if not Dragging then
    inherited MouseMove(Shift, X, Y);
end;

procedure TdxCustomOrgChart.SetTimeScroll;
begin
  FScrollTimer.Enabled := True;
end;

procedure TdxCustomOrgChart.KillTimeScroll;
begin
  FScrollTimer.Enabled := False;
end;

procedure TdxCustomOrgChart.DoTimerScrolling(Sender: TObject);
var
  AMousePos: TPoint;
begin
  AMousePos := GetMouseCursorClientPos;
  DragScroll(AMousePos.X, AMousePos.Y, dsDragMove);
end;

procedure TdxCustomOrgChart.DragDraw(Source: TdxCustomOrgChart);
var
  W, H, X, Y: Integer;
begin
  if FDragParent = nil then Exit;
  W := Source.Selected.Width;
  H := Source.Selected.Height;
  if not Rotated then
  begin
    X := FDragX0 + W div 2;
    Y := FDragY1 + (FDragY0 - FDragY1) div 2;
  end
  else
  begin
    X := FDragX1 + (FDragX0 - FDragX1) div 2;
    Y := FDragY0 + H div 2;
  end;

  Canvas.SaveState;
  try
    Canvas.Pen.Mode := pmNotXor;
    Canvas.Pen.Width := 1;
    Canvas.Pen.Style := psDot;
    Canvas.Pen.Color := Painter.GetLineColor;
    Canvas.Brush.Style := bsClear;
    ShowCursor(False);
    try
      Canvas.Canvas.Rectangle(FDragX0, FDragY0, FDragX0 + W, FDragY0 + H);
      if FDragParent <> FRoot then
      begin
        if not Rotated then
        begin
          Canvas.MoveTo(X, FDragY0);
          Canvas.LineTo(X, Y);
          Canvas.LineTo(FDragX1, Y);
        end
        else
        begin
          Canvas.MoveTo(FDragX0, Y);
          Canvas.LineTo(X, Y);
          Canvas.LineTo(X, FDragY1);
        end;
        Canvas.LineTo(FDragX1, FDragY1);
      end;
    finally
      ShowCursor(True);
    end;
  finally
    Canvas.RestoreState;
  end;
end;

function TdxCustomOrgChart.DragScroll(X, Y: Integer; AState: TDragState): Boolean;
var
  ANewX, ANewY: Smallint;
begin
  Result := AState = dsDragMove;
  ANewX := LeftPos;
  ANewY := TopPos;
  if Result then
  begin
    if X <= OtScrollUnit then
      ANewX := LeftPos - OtScrollUnit;
    if X >= ClientBoundsWidth - OtScrollUnit then
      ANewX := LeftPos + OtScrollUnit;
    if Y <= OtScrollUnit then
      ANewY := TopPos - OtScrollUnit;
    if Y >= ClientBoundsHeight - OtScrollUnit then
      ANewY := TopPos + OtScrollUnit;
    Result := (ANewX <> LeftPos) or (ANewY <> TopPos);
  end;
  if Result then
  begin
    SetLeftTop(Point(ANewX, ANewY));
    SetTimeScroll
  end
  else
    KillTimeScroll;
end;

procedure TdxCustomOrgChart.DragOver(Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
var
  R: TRect;
  N: TdxOcNode;
  AScrolled: Boolean;
begin
  N := nil;
  Source := cxExtractDragObjectSource(Source);
  if Assigned(OnDragDrop) then
    Accept := Source is TdxCustomOrgChart
  else
    Accept := Source = Self;

  DragDraw(TdxCustomOrgChart(Source));
  AScrolled := DragScroll(X, Y, State);
  if Accept then
  begin
    N := GetNodeAt(X, Y);
    Accept := (N <> nil) and not N.HasAsParent(TdxCustomOrgChart(Source).Selected);
  end;
  if not Accept or AScrolled or not (ocShowDrag in Options) or (State <> dsDragMove) then
    FDragParent := nil
  else
  begin
    FDragX0 := X;
    FDragY0 := Y;

    if not (htUnder in FHitTests) or (N = Selected) then N := N.FParent;
    if N <> FDragParent then
    begin
      FDragParent := N;
      if N <> FRoot then
      begin
        R := N.DisplayRect;
        if not Rotated then
        begin
          FDragX1 := R.Left + (R.Right - R.Left) div 2;
          FDragY1 := R.Bottom;
        end
        else
        begin
          FDragX1 := R.Right;
          FDragY1 := R.Top + (R.Bottom - R.Top) div 2;
        end;
      end;
    end;
  end;
  DragDraw(TdxCustomOrgChart(Source));
  if Assigned(OnDragOver) then
    OnDragOver(Self, Source, X, Y, State, Accept);
end;

procedure TdxCustomOrgChart.DragDrop(Source: TObject; X, Y: Integer);
var
  Node: TdxOcNode;
  Mode: TdxOcNodeAttachMode;
begin
  KillTimeScroll;
  Source := cxExtractDragObjectSource(Source);
  if Assigned(OnDragDrop) then
    OnDragDrop(Self, Source, X, Y)
  else
    if (Source = Self) and (Selected <> nil) then
    begin
      Node := GetNodeAt(X, Y);
      if Node = nil then Exit;
      Mode := naInsert;
      if htUnder in FHitTests then
        Mode := naAddChild;
      if (htOnRightIndentX in FHitTests) or (htOnRect in FHitTests) then
        Node := Node.getNextSibling;
      if Node = nil then
      begin
        Node := FNodeAt.Parent;
        Mode := naAddChild;
      end;
      Selected.MoveTo(Node, Mode);
      Selected.MakeVisible;
    end;
end;

procedure TdxCustomOrgChart.DragCanceled;
begin
  KillTimeScroll;
  FDrag := False;
end;

function TdxCustomOrgChart.IsDefaultGesture(AGestureID: Integer): Boolean;
begin
  Result := inherited IsDefaultGesture(AGestureID) or (AGestureID = GID_ROTATE);
end;

procedure TdxCustomOrgChart.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  if not Editing and (Selected <> nil) then
    case Key of
      '+': Selected.Expand(False);
      '-': Selected.Collapse(False);
    end;
end;

procedure TdxCustomOrgChart.KeyDown(var Key: Word; Shift: TShiftState);

  function InsertNode(AsChild: Boolean): TdxOcNode;
  begin
    Result := nil;
    if not (ocInsDel in Options) or (Selected = nil) and (Count > 0) then Exit;
    if AsChild or (Selected = nil) then
      Result := AddChild(Selected, nil)
    else
    begin
      Result := Selected.getNextSibling;
      if Result = nil then
        Result := AddChild(Selected.Parent, nil)
      else
        Result := Insert(Result, nil);
    end;
  end;

begin
  inherited KeyDown(Key, Shift);
  if Shift = [ssCtrl] then
    case Key of
      VK_RIGHT: DoNavigate(ocnPageRight);
      VK_LEFT: DoNavigate(ocnPageLeft);
      VK_PRIOR: DoNavigate(ocnTop);
      VK_NEXT: DoNavigate(ocnBottom);
      VK_HOME: SetLeftTop(cxNullPoint);
      VK_END: SetLeftTop(Point(FullWidth, FullHeight));
      VK_INSERT: SelectNode(InsertNode(True));
    end;
  if Shift = [] then
    case Key of
      VK_NEXT: DoNavigate(ocnPageDown);
      VK_PRIOR: DoNavigate(ocnPageUp);
      VK_HOME: DoNavigate(ocnLeft);
      VK_END: DoNavigate(ocnRight);
      VK_DOWN: DoNavigate(ocnSelectNextDown);
      VK_UP: DoNavigate(ocnSelectNextUp);
      VK_ESCAPE: EndDrag(False);
      VK_INSERT: SelectNode(InsertNode(False));
      VK_DELETE:
        if ocInsDel in Options then
          Delete(Selected);
      VK_RETURN, VK_F2: ShowEditor;
      VK_RIGHT: DoNavigate(ocnSelectNextRight);
      VK_LEFT: DoNavigate(ocnSelectNextLeft);
    end;
end;

procedure TdxCustomOrgChart.SetOptions(Value: TdxOcOptions);
var
  Chgd: TdxOcOptions;
begin
  if Value <> Options then
  begin
    Chgd := (Options + Value) - (Options * Value);
    FOptions := Value;
    if ocRect3D in Chgd then InvalidateSizes(ivBoth);
    if ocButtons in Chgd then Invalidate;
    if ocSelect in Chgd then InvalidateSel;
    if (ocFocus in Chgd) and Focused then InvalidateSel;
  end;
end;

procedure TdxCustomOrgChart.SetRotated(Value: Boolean);
begin
  if Value <> Rotated then
  begin
    FRotated := Value;
    InvalidateSizes(ivBoth);
    SetLeftTop(cxNullPoint);
  end;
end;

procedure TdxCustomOrgChart.DblClick;
begin
  if not IsScrollBarsArea(ScreenToClient(GetMouseCursorPos)) and
    (ocDblClick in Options) and (FHitTests * [htOnShape, htOnButton] = [htOnShape]) then
    FNodeAt.Expanded := not FNodeAt.Expanded;
  inherited DblClick;
end;

function TdxCustomOrgChart.GetZoom: Boolean;
begin
  Result := FZoom = 0;
end;

function TdxCustomOrgChart.IsZoomed: Boolean;
begin
  if FZoom = 0 then RecalcSizes;
  Result := (FZoom = 0) and (FZoomLo < FZoomHi);
end;

function TdxCustomOrgChart.GetLineWidth: Word;
begin
////  if IsZoomed then Result := 1              // Fix: by Kirill (LineWidth)
  if IsZoomed then
    Result := DoZoom(FLineWidth) // Fix: by Kirill (LineWidth)
  else
    Result := FLineWidth;
end;

function TdxCustomOrgChart.GetIndentX: Word;
begin
  if Rotated then
    Result := DoZoom(FIndentY)
  else
    Result := DoZoom(FIndentX);
end;

function TdxCustomOrgChart.GetIndentY: Word;
begin
  if Rotated then
    Result := DoZoom(FIndentX)
  else
    Result := DoZoom(FIndentY);
end;

function TdxCustomOrgChart.GetIsCopyDragDrop: Boolean;
begin
  Result := False;
end;

function TdxCustomOrgChart.GetScrollContentForegroundColor: TColor;
begin
  Result := dxInvertColor(Painter.GetBackgroundColor);
end;

function TdxCustomOrgChart.DoZoom(Value: Integer): Integer;
begin
  if not IsZoomed then
    Result := Value
  else
    Result := MulDiv(Value, FZoomLo, FZoomHi);
end;

procedure TdxCustomOrgChart.SetZoom(Value: Boolean);
begin
  if Value <> Zoom then
  begin
    FZoom := Ord(not Value);
    ChangeSize;
    if Value then
      UpdateScrollBars;
  end;
end;

procedure TdxCustomOrgChart.SetZoomRatio;
begin
  Inc(FZoom);
  if ClientBoundsWidth * FullHeight <= ClientBoundsHeight * FullWidth then
  begin
    FZoomLo := ClientBoundsWidth;
    FZoomHi := FullWidth + 1;
  end
  else
  begin
    FZoomLo := ClientBoundsHeight;
    FZoomHi := FullHeight + 1;
  end;
  Dec(FZoom);
end;

procedure TdxCustomOrgChart.ImageListChange(Sender: TObject);
begin
  if HandleAllocated then
  begin
    BeginUpdate;
    EndUpdate;
  end;
end;

procedure TdxCustomOrgChart.SetImages(Value: TCustomImageList);
var
  P: TPoint;
  Node: TdxOcNode;
begin
  if (Value = FImages) or (csDestroying in ComponentState) then exit;
  BeginUpdate;
  if FImages <> nil then
    FImages.UnRegisterChanges(FImagesChangeLink);

  FImages := Value;

  if Value <> nil then
  begin
    FImages.RegisterChanges(FImagesChangeLink);
    if DefaultImageAlign = iaNone then
      DefaultImageAlign := iaLT;

    P := MinSizes;
    if DefaultNodeWidth < P.X then
      DefaultNodeWidth := P.X;
    if DefaultNodeHeight < P.Y then
      DefaultNodeHeight := P.Y;

    Node := GetFirstNode;
    while Node <> nil do
    begin
      if (Node.FWidth <> 0) and (Node.FWidth < P.X) then
        Node.Width := P.X;
      if (Node.FHeight <> 0) and (Node.FHeight < P.Y) then
        Node.Height := P.Y;
      Node := Node.GetNext;
    end;
  end;
  EndUpdate;
end;

procedure TdxCustomOrgChart.ReadData(Stream: TStream);
var
  B: array[0..6] of AnsiChar;
begin
  Clear;
  FIsUnicode := False;
  if (Stream.Size - Stream.Position) > SizeOf(B) then
  begin
    Stream.ReadBuffer(B, SizeOf(B));
    FIsUnicode := B = StreamDescriptionUNICODE;
    if not FIsUnicode and (B <> StreamDescriptionANSI) then
      Stream.Position := Stream.Position - SizeOf(B);
  end;
  FRoot.ReadChildren(Stream);
end;

procedure TdxCustomOrgChart.WriteData(Stream: TStream);
begin
{$IFNDEF STREAMANSIFORMAT}
  Stream.WriteBuffer(StreamDescriptionUNICODE[1], dxStrLen(PAnsiChar(StreamDescriptionUNICODE)));
{$ENDIF}
  FRoot.WriteChildren(Stream);
end;

procedure TdxCustomOrgChart.AssignData(Source: TdxCustomOrgChart);
var
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  try
    Source.WriteData(Stream);
    Stream.Position := 0;
    Clear;
    ReadData(Stream);
    if Count > 0 then
      Selected := Items[0];
  finally
    Stream.Free;
  end;
end;

procedure TdxCustomOrgChart.Loaded;
begin
  inherited Loaded;
  if csDesigning in ComponentState then FullExpand;
end;

function TdxCustomOrgChart.CreateEditor: TdxOcInplaceEdit;
begin
  Result := TdxOcInplaceEdit.Create(Self);
end;

function TdxCustomOrgChart.GetClientBoundsHeight: Integer;
begin
  Result := cxRectHeight(ClientBounds);
end;

function TdxCustomOrgChart.GetClientBoundsWidth: Integer;
begin
  Result := cxRectWidth(ClientBounds);
end;

function TdxCustomOrgChart.GetEditing: Boolean;
begin
  Result := (FEditor <> nil) and FEditor.Visible;
end;

function TdxCustomOrgChart.GetFullRect: TRect;
begin
  Result := cxRectBounds(-LeftEdge, -TopEdge, FullWidth, FullHeight);
end;

procedure TdxCustomOrgChart.SetEditing(Value: Boolean);
begin
  if Value then
    ShowEditor
  else
    HideEditor(True);
end;

procedure TdxCustomOrgChart.HideEditor(Save: Boolean);
var
  Focus: Boolean;
  TheText: string;
begin
  if not Editing then Exit;
  Focus := FEditor.Focused;
  if Save and FEditor.Modified then
  begin
    TheText := FEditor.Text;
    if Assigned(OnEdited) then OnEdited(Self, Selected, TheText);
    Selected.Text := TheText;
  end;
  FEditor.Hide;
  if Focus then SetFocus;
end;

procedure TdxCustomOrgChart.ShowEditor;
var
  R: TRect;
  Allow: Boolean;
begin
  if not (ocEdit in Options) or Editing or IsZoomed or (Selected = nil) then
    Exit;

  Allow := True;
  if Assigned(OnEditing) then
    OnEditing(Self, Selected, Allow);
  if not Allow then
    Exit;

  Selected.MakeVisible;
  if FEditor = nil then
    FEditor := CreateEditor;
  R := Selected.ClientRect;
  Selected.ClientToText(R);

  FEditor.BoundsRect := R;
  FEditor.ClientWidth := R.Right - R.Left;
  FEditor.ClientHeight := R.Bottom - R.Top;
  FEditor.FMinW := Canvas.TextWidth('W');
  FEditor.FMinH := Canvas.TextHeight('W');
  FEditor.FMaxW := Max(FEditor.FMinW, Screen.Width div 2);
  FEditor.FMaxH := Max(FEditor.FMinH, Screen.Height div 2);
  FEditor.Color := Painter.GetNodeColor(Selected.Color, False);
  if FEditor.Color = clNone then
    FEditor.Color := Painter.GetBackgroundColor;
  FEditor.Font := Font;
  FEditor.Font.Color := Painter.GetNodeTextColor(False);
  Selected.SetFont(Font);
  FEditor.Text := Selected.GetText;
  FEditor.SelStart := 0;
  FEditor.SelLength := 0;
  FEditor.Modified := False;
  FEditor.AdjustBounds;
  FEditor.Show;
  FEditor.SetFocus;
end;

procedure TdxCustomOrgChart.SetEditMode(Value: TdxOcEditMode);
begin
  if Value <> EditMode then
  begin
    FEditMode := Value;
    if FEditor <> nil then FEditor.RecreateWnd;
    if Count > 0 then Invalidate;
  end;
end;

procedure TdxCustomOrgChart.LookAndFeelChanged(
  Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues);
begin
  inherited LookAndFeelChanged(Sender, AChangedValues);
  if HandleAllocated then
    Invalidate;
end;

procedure TdxCustomOrgChart.NodeChanged(Node: TdxOcNode);
begin
end;

procedure TdxCustomOrgChart.SelectNode(ANode: TdxOcNode);
begin
  if ANode <> nil then
  begin
    Selected := ANode;
    Selected.MakeVisible;
  end;
end;

procedure TdxCustomOrgChart.DefineProperties(Filer: TFiler);
begin
  Inc(FZoom);
  inherited DefineProperties(Filer);
  Dec(FZoom);
end;

procedure TdxCustomOrgChart.WMSetFocus(var Msg: TWMSetFocus);
begin
  inherited;
  InvalidateSel;
end;

procedure TdxCustomOrgChart.WMKillFocus(var Msg: TWMKillFocus);
begin
  inherited;
  InvalidateSel;
end;

procedure TdxCustomOrgChart.WMSize(var Msg: TWMSize);
var
  PrevZoom: Boolean;
begin
  if GetEditing then HideEditor(False); // Fix: by Kirill (ModalWindow)
  inherited;
  if (Msg.SizeType = SIZE_MAXIMIZED) or (Msg.SizeType = SIZE_RESTORED) then
  begin
    PrevZoom := IsZoomed;
    FSizeChanged := True;
    RecalcSizes;
    if PrevZoom or IsZoomed then Invalidate;
  end;
end;

procedure TdxCustomOrgChart.WMGetDlgCode(var Msg: TWMGetDlgCode);
begin
  inherited;
  Msg.Result := Msg.Result or DLGC_WANTARROWS;
end;

procedure TdxCustomOrgChart.WMDblClk(var Msg: TWMMouse);
begin
  if ocDblClick in Options then GetNodeAt(Msg.XPos, Msg.YPos);
  inherited;
end;

procedure TdxCustomOrgChart.WMErase(var Msg: TWMEraseBkgnd);
begin
  RecalcSizes;
  Msg.Result := 1;
end;

procedure TdxCustomOrgChart.CMCtl3DChanged(var Msg: TMessage);
begin
  inherited;
  if BorderStyle = bsSingle then RecreateWnd;
end;

procedure TdxCustomOrgChart.SaveToFile(const AFileName: string);
var
  AStream : TMemoryStream;
begin
  AStream := TMemoryStream.Create;
  SaveToStream(AStream);
  try
    AStream.SaveToFile(AFileName);
  finally
    AStream.Free;
  end;
end;

procedure TdxCustomOrgChart.SaveToStream(AStream: TStream);
begin
  WriteData(AStream);
end;

{ TdxOrgChart }

procedure TdxOrgChart.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineBinaryProperty('Items', ReadData, WriteData, Count > 0);
end;

procedure TdxOrgChart.LoadFromFile(const AFileName: string);
var
  AStream : TMemoryStream;
begin
  AStream := TMemoryStream.Create;
  try
    AStream.LoadFromFile(AFileName);
    LoadFromStream(AStream);
  finally
    AStream.Free;
  end;
end;

procedure TdxOrgChart.LoadFromStream(AStream: TStream);
begin
  ReadData(AStream);
end;


{ TdxOrgChartCustomPainter }

constructor TdxOrgChartCustomPainter.Create(AChart: TdxCustomOrgChart);
begin
  inherited Create;
  FChart := AChart;
end;

function TdxOrgChartCustomPainter.GetBackgroundColor: TColor;
begin
  Result := Chart.Color;
  if Result = clDefault then
    Result := LookAndFeelPainter.DefaultEditorBackgroundColor(False);
  if Result = clDefault then
    Result := clWindow;
end;

function TdxOrgChartCustomPainter.GetLineColor: TColor;
begin
  Result := Chart.LineColor;
  if Result = clDefault then
    Result := LookAndFeelPainter.GetContainerBorderColor(False);
end;

function TdxOrgChartCustomPainter.GetNodeColor(ACurrectColor: TColor; ASelected: Boolean): TColor;
begin
  if ASelected then
    Result := cxGetActualColor(Chart.SelectedNodeColor, LookAndFeelPainter.DefaultSelectionColor)
  else
    Result := cxGetActualColor(ACurrectColor, GetBackgroundColor);
end;

function TdxOrgChartCustomPainter.GetNodeTextColor(ASelected: Boolean): TColor;
begin
  if ASelected then
  begin
    Result := Chart.SelectedNodeTextColor;
    if Result = clDefault then
      Result := LookAndFeelPainter.DefaultSelectionTextColor;
  end
  else
  begin
    Result := Chart.Font.Color;
    if Result = clWindowText then
      Result := LookAndFeelPainter.DefaultEditorTextColor(False);
    if Result = clDefault then
      Result := clWindowText;
  end;
end;

procedure TdxOrgChartCustomPainter.DrawBackground(ACanvas: TcxCanvas; const R: TRect);
begin
  ACanvas.FillRect(R, GetBackgroundColor);
end;

procedure TdxOrgChartCustomPainter.DrawFrame(ACanvas: TcxCanvas; ANode: TdxOcNode; const R: TRect);
begin
  case ANode.Shape of
    shRoundRect:
      DrawFrameRoundRect(ACanvas, ANode, R, ANode.Radius);
    shEllipse:
      DrawFrameEllipse(ACanvas, ANode, R);
    shDiamond:
      DrawFrameDiamond(ACanvas, ANode, R);
    else
      DrawFrameRectangle(ACanvas, ANode, R, ocRect3D in Chart.Options);
  end;
end;

procedure TdxOrgChartCustomPainter.DrawFrameDiamond(
  ACanvas: TcxCanvas; ANode: TdxOcNode; const R: TRect);
var
  APoints: array[0..3] of TPoint;
begin
  APoints[0].X := R.Left;
  APoints[0].Y := R.Top + (R.Bottom - R.Top) div 2;
  APoints[1].X := R.Left + (R.Right - R.Left) div 2;
  APoints[1].Y := R.Top;
  APoints[2].X := R.Right - 1;
  APoints[2].Y := APoints[0].Y;
  APoints[3].X := APoints[1].X;
  APoints[3].Y := R.Bottom - 1;
  ACanvas.Polygon(APoints);
end;

procedure TdxOrgChartCustomPainter.DrawFrameEllipse(
  ACanvas: TcxCanvas; ANode: TdxOcNode; const R: TRect);
begin
  ACanvas.Canvas.Pen.Color := GetLineColor;
  ACanvas.Canvas.Ellipse(R);
end;

procedure TdxOrgChartCustomPainter.DrawFrameRectangle(
  ACanvas: TcxCanvas; ANode: TdxOcNode; const R: TRect; ARect3D: Boolean);
var
  ABorderThin: Integer;
begin
  ACanvas.Canvas.Pen.Color := GetLineColor;
  ACanvas.Canvas.Rectangle(R);
  if ARect3D then
  begin
    ABorderThin := Chart.LineWidth div 2;
    ACanvas.MoveTo(R.Left + Chart.LineWidth, R.Bottom + ABorderThin);
    ACanvas.LineTo(R.Right + ABorderThin, R.Bottom + ABorderThin);
    ACanvas.LineTo(R.Right + ABorderThin, R.Top + Chart.LineWidth);
  end;
end;

procedure TdxOrgChartCustomPainter.DrawFrameRoundRect(
  ACanvas: TcxCanvas; ANode: TdxOcNode; const R: TRect; ARadius: Integer);
begin
  ACanvas.Canvas.RoundRect(R.Left, R.Top, R.Right, R.Bottom, ARadius, ARadius);
end;

procedure TdxOrgChartCustomPainter.DrawLine(
  ACanvas: TcxCanvas; X1, Y1, X2, Y2: Integer; AWidth: Single);
begin
  ACanvas.Pen.Mode := pmCopy;
  ACanvas.Pen.Style := psSolid;
  ACanvas.Pen.Color := GetLineColor;
  ACanvas.Pen.Width := Round(AWidth);
  ACanvas.MoveTo(X1, Y1);
  ACanvas.LineTo(X2, Y2);
end;

function TdxOrgChartCustomPainter.GetLookAndFeelPainter: TcxCustomLookAndFeelPainter;
begin
  Result := Chart.LookAndFeelPainter;
end;

{ TdxOcInplaceEdit }

constructor TdxOcInplaceEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Hide;
  TabStop := False;
  AutoSize := False;
  ParentCtl3D := False;
  Ctl3D := False;
  Parent := TWinControl(AOwner);
end;

procedure TdxOcInplaceEdit.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or ES_MULTILINE;
    if emLeft in Tree.EditMode then Style := Style or ES_LEFT;
    if emCenter in Tree.EditMode then Style := Style or ES_CENTER;
    if emRight in Tree.EditMode then Style := Style or ES_RIGHT;
    if emWrap in Tree.EditMode then Style := Style and not ES_AUTOHSCROLL;
    if emUpper in Tree.EditMode then Style := Style or ES_UPPERCASE;
    if emLower in Tree.EditMode then Style := Style or ES_LOWERCASE;
  end;
end;

function TdxOcInplaceEdit.Tree: TdxCustomOrgChart;
begin
  Result := TdxCustomOrgChart(Parent);
end;

procedure TdxOcInplaceEdit.WMGetDlgCode(var Msg: TWMGetDlgCode);
begin
  inherited;
  Msg.Result := Msg.Result or DLGC_WANTTAB or DLGC_WANTALLKEYS;
end;

procedure TdxOcInplaceEdit.WMKillFocus(var Msg: TWMKillFocus);
begin
  inherited;
  Tree.HideEditor(True);
end;

procedure TdxOcInplaceEdit.KeyUp(var Key: Word; Shift: TShiftState);
begin
  Tree.KeyUp(Key, Shift);
  AdjustBounds;
end;

procedure TdxOcInplaceEdit.KeyPress(var Key: Char);
begin
  Tree.KeyPress(Key);
  if Key <> #0 then
  begin
    inherited KeyPress(Key);
    AdjustBounds;
  end;
end;

procedure TdxOcInplaceEdit.KeyDown(var Key: Word; Shift: TShiftState);
  procedure EndEdit;
  begin
    if Key = VK_ESCAPE then Modified := False;
    Key := 0;
    Tree.HideEditor(True);
  end;
begin
  if Shift = [ssAlt] then
    case Key of
      VK_RETURN: EndEdit;
      VK_UP, VK_DOWN, VK_RIGHT, VK_LEFT:
        begin
          Tree.KeyDown(Key, []);
          Key := 0;
        end;
    end;
  if Shift = [] then
    case Key of
      VK_F2, VK_ESCAPE: EndEdit;
    end;
  if Key <> 0 then inherited KeyDown(Key, Shift);
end;

procedure TdxOcInplaceEdit.AdjustBounds;
var
  R: TRect;
  W, H: Integer;
begin
  R := ClientRect;
  Tree.DoDrawText(Tree.Canvas.Handle, Text + 'WW' + #13 + #10 + #13 + #10 + 'WW', R, DT_CALCRECT);
  W := R.Right - R.Left; H := R.Bottom - R.Top;
  if W < FMinW then W := FMinW;
  if W > FMaxW then W := FMaxW;
  if H < FMinH then H := FMinH;
  if H > FMaxH then H := FMaxH;
  W := W - ClientWidth; H := H - ClientHeight;
  if emWrap in Tree.EditMode then W := 0;
  if (W or H) <> 0 then
  begin
    R := BoundsRect;
    Inc(R.Right, W); Inc(R.Bottom, H);
    W := W div 2; H := H div 2;
    Dec(R.Left, W); Dec(R.Top, H);
    Dec(R.Right, W); Dec(R.Bottom, H);
    BoundsRect := R;
  end;
end;


{ TdxOrgChartGdiPlusPainter }

procedure TdxOrgChartGdiPlusPainter.DrawFrameDiamond(ACanvas: TcxCanvas; ANode: TdxOcNode; const R: TRect);
var
  AGraphics: TdxGPGraphics;
  APoints: array[0..3] of TPoint;
begin
  APoints[0].X := R.Left;
  APoints[0].Y := R.Top + (R.Bottom - R.Top) div 2;
  APoints[1].X := R.Left + (R.Right - R.Left) div 2;
  APoints[1].Y := R.Top;
  APoints[2].X := R.Right - 1;
  APoints[2].Y := APoints[0].Y;
  APoints[3].X := APoints[1].X;
  APoints[3].Y := R.Bottom - 1;

  AGraphics := dxGpBeginPaint(ACanvas.Handle, R);
  try
    AGraphics.SmoothingMode := smAntiAlias;
    AGraphics.Polygon(APoints, GetLineColor, GetBrushColor(ACanvas), Chart.LineWidth, psSolid, MaxByte, MaxByte);
  finally
    dxGpEndPaint(AGraphics);
  end;
end;

procedure TdxOrgChartGdiPlusPainter.DrawFrameEllipse(ACanvas: TcxCanvas; ANode: TdxOcNode; const R: TRect);
var
  AGraphics: TdxGPGraphics;
begin
  AGraphics := dxGpBeginPaint(ACanvas.Handle, R);
  try
    AGraphics.SmoothingMode := smAntiAlias;
    AGraphics.Ellipse(R, GetLineColor, GetBrushColor(ACanvas), Chart.LineWidth, psSolid, MaxByte, MaxByte);
  finally
    dxGpEndPaint(AGraphics);
  end;
end;

procedure TdxOrgChartGdiPlusPainter.DrawFrameRectangle(ACanvas: TcxCanvas; ANode: TdxOcNode; const R: TRect; ARect3D: Boolean);
var
  AGraphics: TdxGPGraphics;
  AShadowSize: Integer;
begin
  AShadowSize := Chart.LineWidth;
  AGraphics := dxGpBeginPaint(ACanvas.Handle, cxRectInflate(R, AShadowSize, AShadowSize));
  try
    AGraphics.SmoothingMode := smAntiAlias;
    if ARect3D then
      AGraphics.Rectangle(cxRectOffset(R, AShadowSize, AShadowSize), GetLineColor, clNone, Chart.LineWidth, psSolid, 200, MaxByte);
    AGraphics.Rectangle(R, GetLineColor, GetBrushColor(ACanvas), Chart.LineWidth, psSolid, MaxByte, MaxByte);
  finally
    AGraphics.Free;
  end;
end;

procedure TdxOrgChartGdiPlusPainter.DrawFrameRoundRect(ACanvas: TcxCanvas; ANode: TdxOcNode; const R: TRect; ARadius: Integer);
var
  AGraphics: TdxGPGraphics;
begin
  AGraphics := dxGpBeginPaint(ACanvas.Handle, R);
  try
    AGraphics.SmoothingMode := smAntiAlias;
    AGraphics.RoundRect(R, GetLineColor, GetBrushColor(ACanvas),
      ARadius div 2, ARadius div 2, Chart.LineWidth, MaxByte, MaxByte);
  finally
    dxGpEndPaint(AGraphics);
  end;
end;

procedure TdxOrgChartGdiPlusPainter.DrawLine(ACanvas: TcxCanvas; X1, Y1, X2, Y2: Integer; AWidth: Single);
var
  AGraphics: TdxGPGraphics;
begin
  AGraphics := dxGpBeginPaint(ACanvas.Handle, cxRect(Min(X1, X2), Min(Y1, Y2), Max(X1, X2), Max(Y1, Y2)));
  try
    AGraphics.SmoothingMode := smAntiAlias;
    AGraphics.Line(X1, Y1, X2, Y2, GetLineColor, AWidth, psSolid, MaxByte);
  finally
    dxGpEndPaint(AGraphics);
  end;
end;

function TdxOrgChartGdiPlusPainter.GetBrushColor(ACanvas: TcxCanvas): TColor;
begin
  if ACanvas.Brush.Style <> bsClear then
    Result := ACanvas.Brush.Color
  else
    Result := clNone;
end;


{ TdxOrgChartCustomCustomizeForm }

function TdxOrgChartCustomCustomizeForm.CalculateFormPosition(AOrgChart: TdxCustomOrgChart): TRect;
var
  APoint: TPoint;
begin
  APoint := AOrgChart.ClientToScreen(cxNullPoint);
  Inc(APoint.X, 32);
  Inc(APoint.Y, 40);
  Dec(APoint.X, Max(0, APoint.X + Width - Screen.Width));
  Dec(APoint.Y, Max(0, APoint.Y + Height - Screen.Height));
  Result := Bounds(APoint.X, APoint.Y, Width, Height);
end;

function TdxOrgChartCustomCustomizeForm.InsertNode: TdxOcNode;
begin
  Result := PreviewOrgChart.Selected;
  if (Result = nil) or (Result.getNextSibling = nil) then
    Result := PreviewOrgChart.Add(Result, nil)
  else
    Result := PreviewOrgChart.Insert(Result.getNextSibling, nil);

  if Result <> nil then
  begin
    PreviewOrgChart.Selected := Result;
    Result.MakeVisible;
  end;
end;

function TdxOrgChartCustomCustomizeForm.InsertSubNode: TdxOcNode;
begin
  Result := PreviewOrgChart.Selected;
  if Result <> nil then
  begin
    Result := PreviewOrgChart.AddChild(Result, nil);
    if Result <> nil then
    begin
      PreviewOrgChart.Selected := Result;
      Result.MakeVisible;
    end;
  end;
end;

function TdxOrgChartCustomCustomizeForm.Execute(AOrgChart: TdxCustomOrgChart): Boolean;
begin
  BoundsRect := CalculateFormPosition(AOrgChart);
  PreparePreview(PreviewOrgChart, AOrgChart);
  Result := ShowModal = mrOk;
  if Result then
    SaveChanges(AOrgChart);
end;

procedure TdxOrgChartCustomCustomizeForm.PreparePreview(APreview, ASource: TdxCustomOrgChart);
begin
  APreview.SetLeftTop(cxNullPoint);
  APreview.Antialiasing := ASource.Antialiasing;
  APreview.LookAndFeel.MasterLookAndFeel := ASource.LookAndFeel;
  APreview.DefaultNodeHeight := APreview.ScaleFactor.Apply(ASource.DefaultNodeHeight, ASource.ScaleFactor);
  APreview.DefaultNodeWidth := APreview.ScaleFactor.Apply(ASource.DefaultNodeWidth, ASource.ScaleFactor);
  APreview.DefaultImageAlign := ASource.DefaultImageAlign;
  APreview.IndentX := APreview.ScaleFactor.Apply(ASource.IndentX, ASource.ScaleFactor);
  APreview.IndentY := APreview.ScaleFactor.Apply(ASource.IndentY, ASource.ScaleFactor);
  APreview.LineWidth := ASource.LineWidth;
  APreview.Images := ASource.Images;
  APreview.EditMode := ASource.EditMode;
  APreview.Font := ASource.Font;
  APreview.Font.Height := APreview.ScaleFactor.Apply(ASource.Font.Height, ASource.ScaleFactor);
  APreview.Options := APreview.Options + ASource.Options;
  APreview.Rotated := ASource.Rotated;
  APreview.Zoom := ASource.Zoom;
  APreview.AssignData(ASource);
  APreview.FullExpand;
end;

procedure TdxOrgChartCustomCustomizeForm.SaveChanges(AOrgChart: TdxCustomOrgChart);
begin
  AOrgChart.AssignData(PreviewOrgChart);
  AOrgChart.Zoom := PreviewOrgChart.Zoom;
  AOrgChart.Rotated := PreviewOrgChart.Rotated;
  AOrgChart.Antialiasing := PreviewOrgChart.Antialiasing;
  AOrgChart.FullExpand;
end;


{ TdxOrgChartCustomizeFormManager }

constructor TdxOrgChartCustomizeFormManager.Create;
begin
  inherited Create;
  FList := TList.Create;
end;

destructor TdxOrgChartCustomizeFormManager.Destroy;
begin
  FreeAndNil(FList);
  inherited Destroy;
end;

function TdxOrgChartCustomizeFormManager.Customize(AOrgChart: TdxCustomOrgChart): Boolean;
begin
  Result := FList.Count > 0;
  if Result then
  begin
    with GetActualClass.Create(nil) do
    try
      Result := Execute(AOrgChart);
    finally
      Free;
    end;
  end;
end;

procedure TdxOrgChartCustomizeFormManager.Register(AClass: TdxOrgChartCustomCustomizeFormClass);
begin
  FList.Add(Pointer(AClass));
end;

procedure TdxOrgChartCustomizeFormManager.Unregister(AClass: TdxOrgChartCustomCustomizeFormClass);
begin
  FList.Remove(Pointer(AClass));
end;

function TdxOrgChartCustomizeFormManager.GetActualClass: TdxOrgChartCustomCustomizeFormClass;
begin
  if FList.Count > 0 then
    Result := TdxOrgChartCustomCustomizeFormClass(FList.Last)
  else
    Result := nil;
end;


initialization
  dxOrgChartCustomizeFormManager := TdxOrgChartCustomizeFormManager.Create;
finalization
  FreeAndNil(dxOrgChartCustomizeFormManager);
end.

