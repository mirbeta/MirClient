{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressBars components                                   }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSBARS AND ALL ACCOMPANYING VCL  }
{   CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY.                  }
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

unit dxTabbedMDI;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StrUtils, Contnrs, dxCore, cxClasses, cxGeometry, cxGraphics, cxControls,
  cxContainer, cxLookAndFeels, cxPC, dxHooks;

type
  EdxTabbedMDIManagerException = class(EdxException);
  TdxTabbedMDIManager = class;

  TdxTabbedMDITabControlHintHelper = class(TcxCustomTabControlHintHelper)
  protected
    function CanShowHint: Boolean; override;
    function GetOwnerWinControl: TWinControl; override;
    function PtInCaller(const P: TPoint): Boolean; override;
  end;

  TdxTabbedMDITabControllerClass = class of TdxTabbedMDITabController;
  TdxTabbedMDITabController = class(TcxCustomTabControlController)
  private
    FMDITabControl: TcxControl;
  protected
    function GetControlHandle: THandle; override;
    function GetClientToScreen(const APoint: TPoint): TPoint; override;
    function GetHintHelperClass: TcxCustomTabControlHintHelperClass; override;
    function GetScreenToClient(const APoint: TPoint): TPoint; override;
  public
    constructor Create(AOwner: TObject); override;
  end;

  TdxTabbedMDITabControlViewInfoClass = class of TdxTabbedMDITabControlViewInfo;
  TdxTabbedMDITabControlViewInfo = class(TcxCustomTabControlViewInfo)
  protected
    function HasBorders: Boolean; override;
  public
    function UseRightToLeftAlignment: Boolean; override;
    function UseRightToLeftReading: Boolean; override;
  end;

  { TdxTabbedMDIPage }

  TdxTabbedMDIPage = class
  private
    FForm: TForm;
    FPage: TObject;
    FTab: TcxTab;
    FTabbedMDIManager: TdxTabbedMDIManager;

    function GetCaption: string;
    function GetHandle: THandle;
    function GetImageIndex: Integer;
    function GetMDIChild: TForm;
    function GetTab: TcxTab;
    function GetTabCaptionFromForm: string;
    procedure SetCaption(const Value: string);
    procedure SetImageIndex(const Value: Integer);
    function GetIndex: Integer;
  protected
    procedure DoMDIChildCreated;
    procedure MDIChildDestroying;
    procedure UpdateActiveCaption;
    procedure UpdateBounds;
    procedure UpdateIcon;
    procedure UpdateCaption;

    property Caption: string read GetCaption write SetCaption;
    property Handle: THandle read GetHandle;
    property ImageIndex: Integer read GetImageIndex write SetImageIndex;
    property Tab: TcxTab read GetTab;
  public
    constructor Create(AMDIManager: TdxTabbedMDIManager; AMDIChild: THandle);
    destructor Destroy; override;
    property Index: Integer read GetIndex;
    property MDIChild: TForm read GetMDIChild;
  end;

  { TdxTabbedMDITabProperties }

  TdxTabbedMDITabPropertiesClass = class of TdxTabbedMDITabProperties;
  TdxTabbedMDITabProperties = class(TcxCustomTabControlProperties)
  strict private
    FFocusable: Boolean;
    FInternalImages: TcxImageList;
    FOldMDIChildCloseEvent: TCloseEvent;
    FPages: TObjectList;
    FTabbedMDIManager: TdxTabbedMDIManager;

    function GetActivePage: TdxTabbedMDIPage;
    function GetPageCount: Integer;
    function GetPage(AIndex: Integer): TdxTabbedMDIPage;
    function GetPageIndex: Integer;
    procedure MDIChildClose(Sender: TObject; var Action: TCloseAction);
    procedure SetPageIndex(AIndex: Integer);
  protected
    function AddPage(AMdiChild: THandle): Integer;
    procedure ClearPages;
    procedure CloseTab(AIndex: Integer); override;
    procedure DeletePage(AIndex: Integer);
    function GetPageIndexByHandle(AHandle: THandle): Integer;
    function GetTabControl: IcxTabControl; override;
    function InternalGetTabHint(ATab: TcxTab; var ACanShow: Boolean): string; override;
    procedure MDIChildDestroying(AHandle: THandle);
    procedure MoveTab(ACurrentIndex, ANewIndex: Integer); override;
    procedure SetActivePageByHandle(AHandle: THandle);
    procedure UpdateImages;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;

    property ActivePage: TdxTabbedMDIPage read GetActivePage;
    property PageCount: Integer read GetPageCount;
    property PageIndex: Integer read GetPageIndex write SetPageIndex;
    property Pages[AIndex: Integer]: TdxTabbedMDIPage read GetPage;
  published
    property AllowTabDragDrop;
    property CloseButtonMode;
    property CloseTabWithMiddleClick;
    property CustomButtons;
    property Focusable: Boolean read FFocusable write FFocusable default False;
    property HideTabs;
    property HotTrack;
    property ImageBorder;
    property RotatedTabsMaxWidth;
    property MultiLine;
    property NavigatorPosition;
    property Options;
    property OwnerDraw;
    property RaggedRight;
    property Rotate;
    property ScrollOpposite;
    property ShowButtonHints;
    property ShowFrame;
    property ShowTabHints;
    property Style;
    property TabCaptionAlignment;
    property TabHeight;
    property TabPosition;
    property TabSlants;
    property TabsScroll;
    property TabWidth;
  end;

  TdxTabbedMDIPageAddedEvent = procedure(Sender: TdxTabbedMDIManager; APage: TdxTabbedMDIPage) of object;
  TdxTabbedMDIGetTabHintEvent = procedure(Sender: TdxTabbedMDIManager;
    APage: TdxTabbedMDIPage; var AHint: string; var ACanShow: Boolean) of object;

  TdxTabbedMDIManager = class(TcxScalableComponent,
    IcxControlComponentState,
    IcxLookAndFeelContainer,
    IdxSkinSupport)
  private
    FActive: Boolean;
    FCaption: TCaption;
    FCaptionFormat: string;
    FClientFakeControl: TcxControl;
    FClientHandle: THandle;
    FForm: TForm;
    FIsClientWndDestroying: Boolean;
    FLoadedActive: Boolean;
    FLookAndFeel: TcxLookAndFeel;
    FFormCaptionMask: string;
    FMDIChildrenMaximized: Boolean;
    FMinimizedMDIChildren: TList;
    FStoredApplicationTitle: string;
    FStoredExStyle: Integer;
    FTabProperties: TdxTabbedMDITabProperties;
    FWindowProcObject: TcxWindowProcLinkedObject;
    FOnGetTabHint: TdxTabbedMDIGetTabHintEvent;
    FOnPageAdded: TdxTabbedMDIPageAddedEvent;

    procedure BuildFormatString;
    procedure CheckUnique(AOwner: TForm);
    function GetController: TdxTabbedMDITabController;
    function GetFormattedCaption: string;
    function GetTabbedControl: IcxTabControl;
    function GetViewInfo: TdxTabbedMDITabControlViewInfo;
    function IsFormCaptionMaskStored: Boolean;
    procedure MainFormWndProc(var Message: TMessage);
    procedure SetActive(const Value: Boolean);
    procedure SetFormCaptionMask(const Value: string);
    procedure SetForm(Value: TForm);
    procedure SetLookAndFeel(const Value: TcxLookAndFeel);
    procedure SetTabProperties(const Value: TdxTabbedMDITabProperties);
    procedure SubscribeClient;
    procedure SubscribeForm;
    procedure UnSubscribeClient;
    procedure UnSubscribeForm;
    procedure UpdateCaption;
    procedure UpdateScrollBarsArea;
  protected
    procedure Loaded; override;

    procedure ClientWndDestroyingNotify;
    procedure DoPageAdded(APage: TdxTabbedMDIPage);
    procedure LookAndFeelChangeHandler(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues);
    procedure TabPropertiesChangedHandler(Sender: TObject; AType: TcxCustomTabControlPropertiesChangedType);
    procedure TabPropertiesGetTabHintHandler(Sender: TObject; ATabIndex: Integer; var AHint: string; var ACanShow: Boolean);
    procedure TabPropertiesStyleChangedHandler(Sender: TObject);
    procedure TabPropertiesTabIndexChangedHandler(Sender: TObject);

    function GetControllerClass: TdxTabbedMDITabControllerClass; virtual;
    function GetPropertiesClass: TdxTabbedMDITabPropertiesClass; virtual;
    function GetViewInfoClass: TdxTabbedMDITabControlViewInfoClass; virtual;

    // IcxControlComponentState
    function IsDesigning: Boolean;
    function IsDestroying: Boolean;
    function IsLoading: Boolean;

    // IcxLookAndFeelContainer
    function GetLookAndFeel: TcxLookAndFeel;

    // IcxScalableComponent
    procedure ChangeScale(M, D: Integer); override;

    property ClientHandle: THandle read FClientHandle;
    property Form: TForm read FForm write SetForm;
    property TabbedControl: IcxTabControl read GetTabbedControl;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;


    procedure BeginUpdate;
    procedure EndUpdate(AForceUpdate: Boolean);

    property Controller: TdxTabbedMDITabController read GetController;
    property ViewInfo: TdxTabbedMDITabControlViewInfo read GetViewInfo;
  published
    property Active: Boolean read FActive write SetActive;
    property LookAndFeel: TcxLookAndFeel read GetLookAndFeel write SetLookAndFeel;
    property FormCaptionMask: string read FFormCaptionMask write SetFormCaptionMask stored IsFormCaptionMaskStored;
    property TabProperties: TdxTabbedMDITabProperties read FTabProperties write SetTabProperties;
    property OnGetTabHint: TdxTabbedMDIGetTabHintEvent read FOnGetTabHint write FOnGetTabHint;
    property OnPageAdded: TdxTabbedMDIPageAddedEvent read FOnPageAdded write FOnPageAdded;
  end;

implementation

uses
  Math, dxOffice11, dxTabbedMDIStrs, Types, dxMessages, dxDPIAwareUtils, dxForms;

const
  SMainFormCaptionMask = '[MainFormCaption]';
  SChildFormCaptionMask = '[ChildFormCaption]';
  SDefaultCaptionMask = SChildFormCaptionMask + ' - ' + SMainFormCaptionMask;

type
  TFormAccess = class(TForm);

var
  dxTabbedMDIManagers: TList;

function GetActiveMDIManager: TdxTabbedMDIManager;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to dxTabbedMDIManagers.Count - 1 do
    if TdxTabbedMDIManager(dxTabbedMDIManagers[I]).Active then
    begin
      Result := TdxTabbedMDIManager(dxTabbedMDIManagers[I]);
      Break;
    end;
end;

procedure RecalculateNCPart(AWnd: THandle);
begin
  SetWindowPos(AWnd, 0, 0, 0, 0, 0, SWP_FRAMECHANGED or SWP_NOMOVE or SWP_SHOWWINDOW or
    SWP_NOZORDER or SWP_NOACTIVATE or SWP_NOSIZE);
end;

procedure InvalidateAllWnd(AWnd: THandle);
begin
  cxRedrawWindow(AWnd, RDW_INVALIDATE or RDW_ERASE or RDW_ALLCHILDREN or RDW_FRAME or RDW_INTERNALPAINT);
end;

procedure RefreshWindow(AWnd: THandle);
begin
  RecalculateNCPart(AWnd);
  InvalidateAllWnd(AWnd);
end;

procedure RestoreFormToNormalState(AForm: TForm);
begin
  ShowWindow(AForm.Handle, SW_RESTORE);
  if AForm.WindowState <> wsNormal then
    ShowWindow(AForm.Handle, SW_RESTORE);
end;

// Internal classes
type
  TdxTabbedMDIClientControl = class;
  TdxTabbedMDIChild = class;

  TdxTabbedMDIChild = class
  private
    FOwner: TdxTabbedMDIPage;
    FWindowProcObject: TcxWindowProcLinkedObject;
    FHandle: THandle;
    FIsChangeStateAllowed: Boolean;
    FIsCreating: Boolean;
    FIsPropertiesStoring: Boolean;
    FIsWndDestroying: Boolean;
    FParent: TdxTabbedMDIClientControl;
    FStoredBounds: TRect;
    FStoredStyle: Integer;

    procedure CheckNormalState;
    procedure RestoreOriginalBounds;
    procedure RestoreProperties;
    function GetForm: TForm;
    procedure SetHandle(const Value: THandle);
    procedure SetIsCreating(AValue: Boolean);
    procedure StoreOriginalBounds;
    procedure StoreProperties;
    procedure WndProc(var Message: TMessage);
  protected
    procedure DoFormCreated;
    procedure SetSubclassedWindowProperties;
    procedure UpdateBounds;
    property Form: TForm read GetForm;
    property Handle: THandle read FHandle write SetHandle;
    property IsCreating: Boolean read FIsCreating write SetIsCreating;
    property IsWndDestroying: Boolean read FIsWndDestroying write FIsWndDestroying;
  public
    constructor Create(AOwner: TdxTabbedMDIPage; AParent: TdxTabbedMDIClientControl); virtual;
    destructor Destroy; override;
  end;

  { TdxTabbedMDIClientControl }

  TdxTabbedMDIChange = (tmcFocus, tmcLayout, tmcSize);
  TdxTabbedMDIChanges = set of TdxTabbedMDIChange;

  TdxTabbedMDIClientControl = class(TcxControl,
    IcxControlComponentState,
    IcxMouseTrackingCaller,
    IcxMouseTrackingCaller2,
    IcxTabControl)
  private
    FDragAndDropPrevCursor: TCursor;
    FRefreshedChildren: TList;
    FTabbedMDIManager: TdxTabbedMDIManager;
    FWindowProcObject: TcxWindowProcLinkedObject;

    FBoundsRect: TRect;
    FChanges: TdxTabbedMDIChanges;
    FClientBoundsRect: TRect;
    FIsApplicationHookProcessing: Boolean;
    FIsUpdating: Boolean;
    FLockCount: Integer;

      // Helpers
    FController: TdxTabbedMDITabController;
    FPainter: TcxPCCustomPainter;
    FViewInfo: TdxTabbedMDITabControlViewInfo;

    procedure CheckRefreshedChildCaptions;
    procedure CheckRefreshedChildren;
    function DoHandleKey(var AMessage: TWMKeyDown): Boolean;
    procedure DoMouseDown(var Message: TWMMouse);
    procedure DoMouseMove(var Message: TWMMouse);
    procedure DoMouseUp(var Message: TWMMouse);
    procedure DoNonClientAreaPaint(DC: THandle);
    procedure DoClientAreaPaint(DC: THandle);
    function GetActiveChild: THandle;
    function GetMouseButton(const Message: TWMMouse): TMouseButton;
    function GetMappedTabsMousePosition(const P: TSmallPoint): TPoint;
    function GetParentForm: TForm;
    function GetTabbedMDIProperties: TdxTabbedMDITabProperties;
    function IsLocked: Boolean;
    procedure SetActiveChild(AChildWnd: THandle);
    procedure SetBoundsRect(ARect: TRect);
    procedure SetClientBoundsRect(const Value: TRect);

    procedure SubscribeChild(AMDIChild: THandle);
    procedure SubscribeChildren;
    procedure UnSubscribeChild(AMDIChild: THandle);
    procedure UpdateActiveChildBounds;
  protected
    function ApplicationWndHook(var Message: TMessage): Boolean;
    procedure BoundsChanged; override;
    function CreateDragAndDropObject: TcxDragAndDropObject; override;
    procedure CreateHandle; override;
    procedure DestroyWindowHandle; override;
    function GetDeviceContext(var WindowHandle: HWnd): HDC; override;
    function NeedsScrollBars: Boolean; override;
    procedure WndProc(var Message: TMessage); override;

    procedure ActiveChildChanged(APreviousActiveChild, ANewActiveChild: THandle);
    procedure TabPropertiesChangedHandler(Sender: TObject; AType: TcxCustomTabControlPropertiesChangedType);
    procedure TabPropertiesStyleChangedHandler(Sender: TObject);
    procedure TabPropertiesTabIndexChangedHandler(Sender: TObject);
    procedure LookAndFeelChangeHandler(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues);

    procedure MouseDown(AButton: TMouseButton; AShift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(AShift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(AButton: TMouseButton; AShift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;

    // IcxTabControl
    function GetController: TcxCustomTabControlController;
    function GetPainter: TcxPCCustomPainter;
    function GetProperties: TcxCustomTabControlProperties;
    function GetViewInfo: TcxCustomTabControlViewInfo;
    function CanDrawParentBackground: Boolean;
    function GetBoundsRect: TRect;
    function GetCanvas: TcxCanvas;
    function GetControl: TWinControl;
    function GetColor: TColor;
    function GetDragAndDropObject: TcxDragAndDropObject;
    function GetDragAndDropState: TcxDragAndDropState;
    function GetFont: TFont;
    function GetLookAndFeel: TcxLookAndFeel;
    procedure InvalidateRect(const R: TRect; AEraseBackground: Boolean);
    procedure IcxTabControl.RequestLayout = LayoutChanged;
    procedure SetModified;
    function IsEnabled: Boolean;
    function IsDesigning: Boolean;
    function IsDestroying: Boolean;
    function IsFocused: Boolean;
    function IsLoading: Boolean;
    function IsParentBackground: Boolean;

    // IcxMouseTrackingCaller2
    function PtInCaller(const P: TPoint): Boolean;
    procedure MouseLeave; reintroduce;

    procedure CreateHelpers;
    procedure DestroyHelpers;

    procedure AddRefreshChild(AChild: TObject);
    procedure CalculateTabsViewInfo;
    procedure CheckChanges;
    procedure DeleteRefreshChild(AChild: TObject);
    procedure InitializeScaleFactor;
    procedure Invalidate; override;
    procedure LayoutChanged;
    procedure RecreatePainter;

    property BoundsRect: TRect read FBoundsRect;
    property ClientBoundsRect: TRect read FClientBoundsRect write SetClientBoundsRect;
    property ParentForm: TForm read GetParentForm;
    property TabbedMDIProperties: TdxTabbedMDITabProperties read GetTabbedMDIProperties;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure BeginDragAndDrop; override;
    procedure EndDragAndDrop(Accepted: Boolean); override;

    procedure BeginUpdate;
    procedure EndUpdate(AForceUpdate: Boolean);

    property Controller: TcxCustomTabControlController read GetController;
    property Painter: TcxPCCustomPainter read GetPainter;
    property Properties: TcxCustomTabControlProperties read GetProperties;
    property ViewInfo: TcxCustomTabControlViewInfo read GetViewInfo;
  end;

procedure dxTabbedMDICBTHook(ACode: Integer; wParam: WPARAM; lParam: LPARAM; var AHookResult: LRESULT);
var
  AActiveMDIManager: TdxTabbedMDIManager;
  APageIndex: Integer;
begin
  if (ACode = HCBT_MINMAX) and (GetWindowLong(wParam, GWL_EXSTYLE) and
    WS_EX_MDICHILD <> 0) then
  begin
    AActiveMDIManager := GetActiveMDIManager;
    APageIndex := AActiveMDIManager.TabProperties.GetPageIndexByHandle(wParam);
    if (APageIndex <> -1) and
       not TdxTabbedMDIChild(AActiveMDIManager.TabProperties.Pages[APageIndex].FPage).FIsChangeStateAllowed then
      AHookResult := 1;
  end;
end;

{ TdxTabbedMDIChild }

constructor TdxTabbedMDIChild.Create(AOwner: TdxTabbedMDIPage;
  AParent: TdxTabbedMDIClientControl);
begin
  inherited Create;
  FParent := AParent;
  FOwner := AOwner;
  FIsChangeStateAllowed := True;
end;

destructor TdxTabbedMDIChild.Destroy;
begin
  Handle := 0;
  inherited Destroy;
end;

procedure TdxTabbedMDIChild.DoFormCreated;
begin
  FOwner.DoMDIChildCreated;
end;

procedure TdxTabbedMDIChild.SetSubclassedWindowProperties;
begin
  SetWindowLong(FHandle, GWL_STYLE, FStoredStyle and not (WS_BORDER or WS_DLGFRAME or WS_THICKFRAME));
  CheckNormalState;
  FStoredStyle := FStoredStyle and not WS_MAXIMIZE and not WS_MINIMIZE;
end;

procedure TdxTabbedMDIChild.UpdateBounds;
begin
  if not FIsCreating then
    SetWindowPos(FHandle, 0, 0, 0, 0, 0, SWP_NOZORDER + SWP_NOACTIVATE);
end;

procedure TdxTabbedMDIChild.CheckNormalState;
begin
  if Form.WindowState <> wsNormal then
  begin
    RestoreFormToNormalState(Form);
    StoreOriginalBounds;
  end;
end;

procedure TdxTabbedMDIChild.RestoreOriginalBounds;
begin
  SetWindowPos(FHandle, 0, FStoredBounds.Left, FStoredBounds.Top,
    cxRectWidth(FStoredBounds), cxRectHeight(FStoredBounds),
    SWP_NOZORDER + SWP_NOACTIVATE);
end;

procedure TdxTabbedMDIChild.RestoreProperties;
begin
  RestoreOriginalBounds;
  SetWindowLong(FHandle, GWL_STYLE, FStoredStyle);
end;

function TdxTabbedMDIChild.GetForm: TForm;
begin
  Result := TForm(FindControl(FHandle));
end;

procedure TdxTabbedMDIChild.SetHandle(const Value: THandle);

  procedure DoSubclass;
  begin
    FWindowProcObject := cxWindowProcController.Add(Form, WndProc)
  end;

  procedure DoUnSubclass;
  begin
    cxWindowProcController.Remove(FWindowProcObject);
  end;

begin
  if Value <> FHandle then
  begin
    if FHandle <> 0 then
    begin
      FParent.DeleteRefreshChild(Self);
      DoUnSubclass;
      if not FIsWndDestroying then
      begin
        RestoreProperties;
        RefreshWindow(FHandle);
      end;
    end;
    FHandle := Value;
    if FHandle <> 0 then
    begin
      FIsCreating := True;
      DoSubclass;
      FParent.AddRefreshChild(Self);
    end;
  end;
end;

procedure TdxTabbedMDIChild.SetIsCreating(AValue: Boolean);
begin
  if FIsPropertiesStoring then Exit;
  if FIsCreating <> AValue then
  begin
    if not AValue then
      StoreProperties;
    FIsCreating := AValue;
    if not FIsCreating then
    begin
      SetSubclassedWindowProperties;
      UpdateBounds;
      RefreshWindow(FHandle);
      FParent.DeleteRefreshChild(Self);
    end;
    FIsChangeStateAllowed := False;
  end;
end;

procedure TdxTabbedMDIChild.StoreOriginalBounds;
var
  AOriginalBounds: TRect;
begin
  AOriginalBounds := cxGetWindowRect(FHandle);
  ScreenToClient(FParent.Handle, AOriginalBounds.TopLeft);
  ScreenToClient(FParent.Handle, AOriginalBounds.BottomRight);
  FStoredBounds := AOriginalBounds;
end;

procedure TdxTabbedMDIChild.StoreProperties;
var
  AForm: TForm;
begin
  FIsPropertiesStoring := True;
  try
    FStoredStyle := GetWindowLong(FHandle, GWL_STYLE);
    AForm := Form;
    if AForm.WindowState = wsNormal then
      StoreOriginalBounds
    else
      if AForm.WindowState = wsMaximized then
        FParent.FTabbedMDIManager.FMDIChildrenMaximized := True
      else  //wsMinimized
        FParent.FTabbedMDIManager.FMinimizedMDIChildren.Add(AForm);
  finally
    FIsPropertiesStoring := False;
  end;
end;

procedure TdxTabbedMDIChild.WndProc(var Message: TMessage);

  procedure Default;
  begin
    FWindowProcObject.DefaultProc(Message);
  end;

  procedure SysDefault;
  begin
    with Message do
      Result := DefMDIChildProc(FHandle, Msg, wParam, lParam);
  end;

  procedure DoWindowPosMessage(AMessage: TWMWindowPosMsg);
  var
    R: TRect;
  begin
    if FIsCreating then
    begin
      if AMessage.WindowPos^.flags and SWP_SHOWWINDOW <> 0 then
        AMessage.WindowPos^.flags := AMessage.WindowPos^.flags and not SWP_SHOWWINDOW;
    end
    else
      if AMessage.WindowPos^.flags and SWP_NOSIZE = 0 then
      begin
        R := FParent.ClientBoundsRect;
        with AMessage.WindowPos^ do
        begin
          x := 0;
          y := 0;
          cx := cxRectWidth(R);
          cy := cxRectHeight(R);
        end;
      end;
    Default;
  end;

  procedure DoNCCalcSize(var AMessage: TWMNCCalcSize);
  var
    R: TRect;
    AOffset: TPoint;
  begin
    AOffset := AMessage.CalcSize_Params^.rgrc[0].TopLeft;
    Default;
    if FIsCreating then Exit;
    if AMessage.CalcValidRects then
    begin
      R := FParent.ClientBoundsRect;
      AMessage.CalcSize_Params^.rgrc[0] := cxRectOffset(R, [cxPointInvert(R.TopLeft), AOffset]);
    end;
    AMessage.Result := 0;
  end;

  procedure DoChildActivate(var AMessage: TWMChildActivate);
  var
    APreviousActive: THandle;
    AWasClientFocused: Boolean;
  begin
    APreviousActive := FParent.GetActiveChild;
    AWasClientFocused := Windows.GetFocus = FParent.Handle;
    Default;
    if APreviousActive <> FHandle then
      FParent.ActiveChildChanged(APreviousActive, FHandle);
    if FIsCreating and not FParent.IsLocked then
    begin
      DoFormCreated;
      IsCreating := False;
    end;
    if AWasClientFocused then
      Windows.SetFocus(FParent.Handle);
    FOwner.UpdateActiveCaption;
  end;

  procedure DoDestroyWnd;
  var
    AHandle: THandle;
  begin
    AHandle := FHandle;
    FParent.UnSubscribeChild(FHandle);
    with Message do
      Result := SendMessage(AHandle, Msg, wParam, lParam);
  end;

  procedure DoNCActivate(var AMessage: TWMNCActivate);
  begin
    if AMessage.Active then
      UpdateBounds;
    Default;
  end;

  procedure DoShowWindow(var AMessage: TWMShowWindow);
  begin
    if FIsCreating and not FParent.IsLocked and AMessage.Show then
    begin
      DoFormCreated;
      IsCreating := False;
    end;
    Default;
  end;

  procedure DoSetText(var Message: TWMSetText);
  begin
    Default;
    FOwner.UpdateCaption;
  end;

  procedure DoSetIcon(var Message: TWMSetIcon);
  begin
    Default;
    FOwner.UpdateIcon;
  end;

begin
  with Message do
    case Msg of
      WM_WINDOWPOSCHANGING, WM_WINDOWPOSCHANGED:
        DoWindowPosMessage(TWMWindowPosMsg(Message));
      DXM_SKINS_SUPPRESSMDICHILDBORDERS:
        Result := 1;
      WM_DESTROY:
        DoDestroyWnd;
      WM_NCACTIVATE:
        DoNCActivate(TWMNCActivate(Message));
      WM_NCCALCSIZE:
        DoNCCalcSize(TWMNCCalcSize(Message));
      WM_CHILDACTIVATE:
        DoChildActivate(TWMChildActivate(Message));
      WM_SETTEXT:
        DoSetText(TWMSetText(Message));
      WM_SETICON:
        DoSetIcon(TWMSetIcon(Message));
      WM_SIZE:
        begin
         // if TWMSize(Message).SizeType = SIZE_MINIMIZED then
         //   ShowWindow(FHandle, SW_SHOWNORMAL);
          Default;
        end;
      WM_SHOWWINDOW:
        DoShowWindow(TWMShowWindow(Message));
      else
        Default;
    end;
end;

{ TdxTabbedMDIClientControl }

constructor TdxTabbedMDIClientControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTabbedMDIManager := TdxTabbedMDIManager(AOwner);
  FClientBoundsRect := cxInvalidRect;
  FRefreshedChildren := TList.Create;
  CreateHelpers;
  InitializeScaleFactor;
end;

destructor TdxTabbedMDIClientControl.Destroy;
begin
  BeginUpdate;
  cxControls.EndMouseTracking(Self as IcxMouseTrackingCaller2);
  TabbedMDIProperties.ClearPages;
  DestroyHelpers;
  FreeAndNil(FRefreshedChildren);
  inherited Destroy;
end;

procedure TdxTabbedMDIClientControl.BeginDragAndDrop;
begin
  DragAndDropObject.DoBeginDragAndDrop;
  SetCapture(Handle);
  FDragAndDropPrevCursor := Screen.Cursor;
  DragAndDropState := ddsInProcess;
end;

procedure TdxTabbedMDIClientControl.EndDragAndDrop(Accepted: Boolean);
begin
  DragAndDropState := ddsNone;
  Screen.Cursor := FDragAndDropPrevCursor;
  ReleaseCapture;
  DragAndDropObject.DoEndDragAndDrop(Accepted);
end;

procedure TdxTabbedMDIClientControl.BeginUpdate;
begin
  Inc(FLockCount);
end;

procedure TdxTabbedMDIClientControl.EndUpdate(AForceUpdate: Boolean);
begin
  if (FLockCount = 1) and (FRefreshedChildren.Count > 0) then
    CheckRefreshedChildCaptions;
  Dec(FLockCount);
  if not IsLocked then
  begin
    if AForceUpdate then
      FChanges := [tmcLayout];
    CheckChanges;
    if FRefreshedChildren.Count > 0 then
      CheckRefreshedChildren;
  end;
end;

function TdxTabbedMDIClientControl.ApplicationWndHook(var Message: TMessage): Boolean;
begin
  Result := False;
  case Message.Msg of
    CM_APPKEYDOWN:
      begin
        if not IsFocused or FIsApplicationHookProcessing then Exit;
        FIsApplicationHookProcessing := True;
        Message.Result := SendAppMessage(CM_APPKEYDOWN, Message.WParam, Message.LParam);
        if (Message.Result = 0) and DoHandleKey(TWMKey(Message)) then
          Message.Result := 1;
        Result := True;
        FIsApplicationHookProcessing := False;
      end;
  end;
end;

procedure TdxTabbedMDIClientControl.BoundsChanged;
begin
  Include(FChanges, tmcSize);
  CheckChanges;
end;

function TdxTabbedMDIClientControl.CreateDragAndDropObject: TcxDragAndDropObject;
begin
  Result := FController.GetDragAndDropObjectClass.Create(Self);
end;

procedure TdxTabbedMDIClientControl.CreateHandle;

  procedure DoSubclass;
  begin
    FWindowProcObject := cxWindowProcController.Add(WindowHandle, MainWndProc);
  end;

  procedure SetSubclassedWindowProperties;
  begin
    SetWindowLong(Handle, GWL_EXSTYLE, GetWindowLong(Handle, GWL_EXSTYLE) and not WS_EX_CLIENTEDGE and not WS_EX_LAYOUTRTL or WS_TABSTOP);
  end;

  procedure InitializeBoundsRect;
  begin
    FBoundsRect := cxRectOffset(cxGetWindowRect(Handle), cxGetWindowRect(ParentForm).TopLeft, False);
  end;

var
  APrevActiveChild: THandle;
begin
  WindowHandle := FTabbedMDIManager.ClientHandle;
  DoSubclass;
  SetSubclassedWindowProperties;
  InitializeBoundsRect;
  APrevActiveChild := GetActiveChild;
  BeginUpdate;
  try
    SubscribeChildren;
  finally
    EndUpdate(True);
  end;
  TabbedMDIProperties.SetActivePageByHandle(APrevActiveChild);
  Application.HookMainWindow(ApplicationWndHook);
end;

procedure TdxTabbedMDIClientControl.DestroyWindowHandle;

  procedure DoUnsubclass;
  begin
    cxWindowProcController.Remove(FWindowProcObject);
  end;

begin
  Application.UnhookMainWindow(ApplicationWndHook);
  DoUnsubclass;
  WindowHandle := 0;
end;

function TdxTabbedMDIClientControl.GetDeviceContext(var WindowHandle: HWnd): HDC;
begin
  Result := GetWindowDC(WindowHandle);
end;

function TdxTabbedMDIClientControl.NeedsScrollBars: Boolean;
begin
  Result := False;
end;

procedure TdxTabbedMDIClientControl.WndProc(var Message: TMessage);

  procedure SysDefault;
  begin
    with Message do
      Result := DefWindowProc(WindowHandle, Msg, wParam, lParam);
  end;

  procedure Default;
  begin
    if FWindowProcObject <> nil then
      FWindowProcObject.DefaultProc(Message)
    else
      SysDefault;
  end;

  procedure DoNCCalcSize(var AMessage: TWMNCCalcSize);
  var
    ABoundsRect: TRect;
  begin
    ABoundsRect := AMessage.CalcSize_Params^.rgrc[0];
    Default;
    if AMessage.CalcValidRects then
    begin
      SetBoundsRect(ABoundsRect);
      AMessage.CalcSize_Params^.rgrc[0] := cxRectOffset(FClientBoundsRect, FBoundsRect.TopLeft);
    end;
    AMessage.Result := 0;
  end;

  procedure DoNCPaint(var AMessage: TWMNCPaint);
  var
    DC: THandle;
    AFlags: Integer;
    ARegion, AUpdateRegion: HRGN;
  begin
    Default;
    AFlags := DCX_CACHE or DCX_CLIPSIBLINGS or DCX_WINDOW or DCX_VALIDATE;
    AUpdateRegion := AMessage.RGN;
    if AUpdateRegion <> 1 then
    begin
      ARegion := CreateRectRgnIndirect(cxEmptyRect);
      CombineRgn(ARegion, AUpdateRegion, 0, RGN_COPY);
      AFlags := AFlags or DCX_INTERSECTRGN;
    end
    else
      ARegion := 0;

    DC := GetDCEx(Handle, ARegion, AFlags);
    DoNonClientAreaPaint(DC);
    ReleaseDC(Handle, DC);
    AMessage.Result := 0;
  end;

  procedure DoPaint(var AMessage: TWMPaint);
  var
    DC: HDC;
    PS: TPaintStruct;
  begin
    DC := AMessage.DC;
    if DC = 0 then
      AMessage.DC := BeginPaint(Handle, PS);
    try
      DoClientAreaPaint(AMessage.DC);
    finally
      if DC = 0 then
        EndPaint(Handle, PS);
    end;
    AMessage.Result := 0;
  end;

  procedure DoKeyDown(var AMessage: TWMKeyDown);
  begin
    DoHandleKey(AMessage);
    if AMessage.CharCode = 0 then Exit;
    Default;
  end;

  procedure DoMDIChildCreate(var AMessage: TWMMDICreate);
  begin
    Default;
    SubscribeChild(AMessage.Result);
  end;

  procedure DoDestroyWnd;
  var
    AHandle: THandle;
  begin
    AHandle := Handle;
    FTabbedMDIManager.ClientWndDestroyingNotify;
    with Message do
      Result := SendMessage(AHandle, Msg, wParam, lParam)
  end;

  procedure DoFocusChanged;
  begin
    if not TabbedMDIProperties.Focusable then
      Default
    else
    begin
      SysDefault;
      Include(FChanges, tmcFocus);
      CheckChanges;
    end;
  end;

begin
  with Message do
    case Msg of
      WM_DESTROY:
        DoDestroyWnd;
      WM_MDIRESTORE, WM_MDIMAXIMIZE, WM_MDITILE,
      WM_MDICASCADE, WM_MDIICONARRANGE:
        Result := 0;
      WM_MDICREATE:
        DoMDIChildCreate(TWMMDICreate(Message));
      WM_NCACTIVATE:
        Result := 0;//do nothing (remove drawing inactive frame)
      WM_MDIDESTROY:
        begin
          UnSubscribeChild(WParam);
          Default;
          FTabbedMDIManager.UpdateCaption;
        end;
      WM_NCCALCSIZE:
        DoNCCalcSize(TWMNCCalcSize(Message));
      WM_KEYDOWN:
        DoKeyDown(TWMKeyDown(Message));
      CN_KEYDOWN:
        begin
          if DragAndDropState <> ddsNone then
            FinishDragAndDrop(False);
        end;
      WM_LBUTTONDOWN, WM_LBUTTONDBLCLK, WM_RBUTTONDOWN,
      WM_RBUTTONDBLCLK, WM_MBUTTONDOWN, WM_MBUTTONDBLCLK:
        begin
          Default;
          DoMouseDown(TWMMouse(Message));
        end;
      WM_LBUTTONUP, WM_RBUTTONUP, WM_MBUTTONUP:
        begin
          Default;
          DoMouseUp(TWMMouse(Message));
        end;
      WM_MOUSEMOVE:
        begin
          Default;
          DoMouseMove(TWMMouseMove(Message));
        end;
      WM_NCHITTEST:
        if TabbedMDIProperties.PageCount > 0 then
          Result := HTCLIENT
        else
          Default;
      $3F:;
      WM_NCPAINT:
        DoNCPaint(TWMNCPaint(Message));
      WM_PAINT:
        if TabbedMDIProperties.PageCount > 0 then
          DoPaint(TWMPaint(Message))
        else
          Default;
      WM_SETFOCUS, WM_KILLFOCUS:
        DoFocusChanged;
      WM_CANCELMODE:
        begin
          Default;
          FinishDragAndDrop(False);
        end;
    else
      Default;
    end;
end;

procedure TdxTabbedMDIClientControl.ActiveChildChanged(APreviousActiveChild, ANewActiveChild: THandle);
begin
  if ANewActiveChild <> 0 then
    TabbedMDIProperties.SetActivePageByHandle(ANewActiveChild);
end;

procedure TdxTabbedMDIClientControl.TabPropertiesChangedHandler(Sender: TObject;
  AType: TcxCustomTabControlPropertiesChangedType);
begin
  case AType of
    pctLight:
      Invalidate;
  else
    LayoutChanged;
  end;
end;

procedure TdxTabbedMDIClientControl.TabPropertiesStyleChangedHandler(Sender: TObject);
begin
  RecreatePainter;
end;

procedure TdxTabbedMDIClientControl.TabPropertiesTabIndexChangedHandler(Sender: TObject);
begin
  if TabbedMDIProperties.PageIndex <> -1 then
  begin
    UpdateActiveChildBounds;
    SetActiveChild(TabbedMDIProperties.ActivePage.Handle);
  end;
end;

procedure TdxTabbedMDIClientControl.LookAndFeelChangeHandler(
  Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues);
begin
  RecreatePainter;
  LayoutChanged;
end;

procedure TdxTabbedMDIClientControl.MouseDown(AButton: TMouseButton; AShift: TShiftState;
  X, Y: Integer);

  procedure ProcessDragAndDrop;
  begin
    if (AButton = mbLeft) and not (ssDouble in AShift) and
      FController.StartDragAndDrop(Point(X, Y)) then
      DragAndDropState := ddsStarting
    else
      FinishDragAndDrop(False);
  end;

begin
  MouseDownPos := Point(X, Y);
  if AButton = mbLeft then
    SetCapture(Handle);
  if FViewInfo.CanFocusOnClick(X, Y) and not IsFocused then
    Windows.SetFocus(Handle);
  ProcessDragAndDrop;
  FController.MouseDown(AButton, AShift, X, Y);
end;

procedure TdxTabbedMDIClientControl.MouseMove(AShift: TShiftState; X, Y: Integer);
var
  AAccepted: Boolean;
begin
  if (DragAndDropState = ddsStarting) and not IsMouseInPressedArea(X, Y) then
    BeginDragAndDrop;
  if DragAndDropState = ddsInProcess then
  begin
    AAccepted := False;
    DragAndDrop(Point(X, Y), AAccepted);
  end;
  FController.MouseMove(AShift, X, Y);
  cxControls.BeginMouseTracking(Self, cxEmptyRect, Self as IcxMouseTrackingCaller2);
end;

procedure TdxTabbedMDIClientControl.MouseUp(AButton: TMouseButton; AShift: TShiftState;
  X, Y: Integer);
begin
  if AButton = mbLeft then
    ReleaseCapture;
  FinishDragAndDrop(True);
  FController.MouseUp(AButton, AShift, X, Y);
end;

procedure TdxTabbedMDIClientControl.Paint;
begin
  if IsLocked then Exit;
  Canvas.ExcludeClipRect(FClientBoundsRect);
  FPainter.Paint(Canvas);
end;

function TdxTabbedMDIClientControl.GetController: TcxCustomTabControlController;
begin
  Result := FController;
end;

function TdxTabbedMDIClientControl.GetPainter: TcxPCCustomPainter;
begin
  if FPainter = nil then
    RecreatePainter;
  Result := FPainter;
end;

function TdxTabbedMDIClientControl.GetProperties: TcxCustomTabControlProperties;
begin
  Result := TabbedMDIProperties;
end;

function TdxTabbedMDIClientControl.GetViewInfo: TcxCustomTabControlViewInfo;
begin
  Result := FViewInfo;
end;

function TdxTabbedMDIClientControl.CanDrawParentBackground: Boolean;
begin
  Result := False;
end;

function TdxTabbedMDIClientControl.GetBoundsRect: TRect;
begin
  Result := FBoundsRect;
end;

function TdxTabbedMDIClientControl.GetCanvas: TcxCanvas;
begin
  Result := Canvas;
end;

function TdxTabbedMDIClientControl.GetControl: TWinControl;
begin
  Result := nil;
end;

function TdxTabbedMDIClientControl.GetColor: TColor;
begin
  Result := clNone;
end;

function TdxTabbedMDIClientControl.GetDragAndDropObject: TcxDragAndDropObject;
begin
  Result := DragAndDropObject;
end;

function TdxTabbedMDIClientControl.GetDragAndDropState: TcxDragAndDropState;
begin
  Result := DragAndDropState;
end;

function TdxTabbedMDIClientControl.GetFont: TFont;
begin
  Result := ParentForm.Font;
end;

function TdxTabbedMDIClientControl.GetLookAndFeel: TcxLookAndFeel;
begin
  Result := FTabbedMDIManager.LookAndFeel;
end;

procedure TdxTabbedMDIClientControl.InvalidateRect(const R: TRect;
  AEraseBackground: Boolean);
var
  ARect: TRect;
begin
  ARect := cxRectOffset(R, cxPointInvert(FClientBoundsRect.TopLeft));
  cxRedrawWindow(Handle, ARect, RDW_FRAME or RDW_INVALIDATE);
end;

procedure TdxTabbedMDIClientControl.SetModified;
begin
// TODO
end;

function TdxTabbedMDIClientControl.IsEnabled: Boolean;
begin
  Result := True;
end;

function TdxTabbedMDIClientControl.IsDesigning: Boolean;
begin
  Result := inherited IsDesigning;
end;

function TdxTabbedMDIClientControl.IsDestroying: Boolean;
begin
  Result := inherited IsDestroying;
end;

function TdxTabbedMDIClientControl.IsFocused: Boolean;
begin
  Result := inherited IsFocused and TabbedMDIProperties.Focusable;
end;

function TdxTabbedMDIClientControl.IsLoading: Boolean;
begin
  Result := False;
end;

function TdxTabbedMDIClientControl.IsParentBackground: Boolean;
begin
  Result := False;
end;

function TdxTabbedMDIClientControl.PtInCaller(const P: TPoint): Boolean;
begin
  Result := WindowFromPoint(GetMouseCursorPos) = Handle;
end;

procedure TdxTabbedMDIClientControl.MouseLeave;
begin
  FController.MouseLeave;
end;

procedure TdxTabbedMDIClientControl.CreateHelpers;
begin
  FController := FTabbedMDIManager.GetControllerClass.Create(Self);
  FViewInfo := FTabbedMDIManager.GetViewInfoClass.Create(Self);
end;

procedure TdxTabbedMDIClientControl.DestroyHelpers;
begin
  FreeAndNil(FPainter);
  FreeAndNil(FViewInfo);
  FreeAndNil(FController);
end;

procedure TdxTabbedMDIClientControl.AddRefreshChild(AChild: TObject);
begin
  FRefreshedChildren.Insert(0, AChild);
end;

procedure TdxTabbedMDIClientControl.CalculateTabsViewInfo;
begin
  FViewInfo.Calculate;
  ClientBoundsRect := FViewInfo.PageClientRect;
end;

procedure TdxTabbedMDIClientControl.CheckChanges;
var
  AOldClientBoundsRect: TRect;
begin
  if IsLocked or FIsUpdating or (FChanges = []) then Exit;
  FIsUpdating := True;
  AOldClientBoundsRect := ClientBoundsRect;
  if [tmcLayout, tmcSize] * FChanges <> [] then
    CalculateTabsViewInfo;
  if tmcLayout in FChanges then
  begin
    if not cxRectIsEqual(AOldClientBoundsRect, ClientBoundsRect) then
      RecalculateNCPart(Handle);
    InvalidateAllWnd(Handle);
  end
  else
    if tmcFocus in FChanges then
      FController.FocusChanged;
  FChanges := [];
  FIsUpdating := False;
end;

procedure TdxTabbedMDIClientControl.DeleteRefreshChild(AChild: TObject);
begin
  FRefreshedChildren.Remove(AChild);
end;

procedure TdxTabbedMDIClientControl.InitializeScaleFactor;
var
  N, D: Integer;
begin
  if dxGetCurrentScaleFactor(FTabbedMDIManager.Form, N, D) then
  begin
    if N <> D then
      ChangeScale(N, D);
  end;
end;

procedure TdxTabbedMDIClientControl.Invalidate;
begin
  cxRedrawWindow(Handle, RDW_FRAME or RDW_INVALIDATE);
end;

procedure TdxTabbedMDIClientControl.LayoutChanged;
begin
  Include(FChanges, tmcLayout);
  CheckChanges;
end;

procedure TdxTabbedMDIClientControl.RecreatePainter;
var
  ANewPainterClass: TcxPCPainterClass;
begin
  ANewPainterClass := FViewInfo.GetPainterClass;
  if (FPainter = nil) or (ANewPainterClass <> FPainter.ClassType) then
  begin
    FreeAndNil(FPainter);
    FPainter := ANewPainterClass.Create(FViewInfo);
    LayoutChanged;
  end;
end;

procedure TdxTabbedMDIClientControl.CheckRefreshedChildCaptions;
var
  I: Integer;
begin
  for I := FRefreshedChildren.Count - 1 downto 0 do
    TdxTabbedMDIChild(FRefreshedChildren[I]).DoFormCreated;
end;

procedure TdxTabbedMDIClientControl.CheckRefreshedChildren;
var
  I: Integer;
begin
  for I := FRefreshedChildren.Count - 1 downto 0 do
    TdxTabbedMDIChild(FRefreshedChildren[I]).IsCreating := False;
end;

function TdxTabbedMDIClientControl.DoHandleKey(var AMessage: TWMKeyDown): Boolean;
var
  AShiftState: TShiftState;
begin
  AShiftState := KeyDataToShiftState(AMessage.KeyData);
  Result := FController.KeyDown(AMessage.CharCode, AShiftState);
end;

procedure TdxTabbedMDIClientControl.DoMouseDown(var Message: TWMMouse);

  function GetDblClickShiftState: TShiftState;
  begin
    case Message.Msg of
      WM_LBUTTONDBLCLK, WM_RBUTTONDBLCLK, WM_MBUTTONDBLCLK:
        Result := [ssDouble];
    else
      Result := [];
    end;
  end;

var
  P: TPoint;
begin
  P := GetMappedTabsMousePosition(Message.Pos);
  MouseDown(GetMouseButton(Message), KeysToShiftState(Message.Keys) + GetDblClickShiftState, P.X, P.Y);
end;

procedure TdxTabbedMDIClientControl.DoMouseMove(var Message: TWMMouse);
var
  P: TPoint;
begin
  P := GetMappedTabsMousePosition(Message.Pos);
  MouseMove(KeysToShiftState(Message.Keys), P.X, P.Y);
end;

procedure TdxTabbedMDIClientControl.DoMouseUp(var Message: TWMMouse);
var
  P: TPoint;
begin
  P := GetMappedTabsMousePosition(Message.Pos);
  MouseUp(GetMouseButton(Message), KeysToShiftState(Message.Keys), P.X, P.Y);
end;

procedure TdxTabbedMDIClientControl.DoNonClientAreaPaint(DC: THandle);
begin
  PaintWindow(DC);
end;

procedure TdxTabbedMDIClientControl.DoClientAreaPaint(DC: THandle);
begin
  if IsLocked then Exit;
  MoveWindowOrg(DC, -FClientBoundsRect.Left, -FClientBoundsRect.Top);
  cxPaintCanvas.BeginPaint(DC);
  try
    FPainter.PaintPageClientArea(cxPaintCanvas);
  finally
    cxPaintCanvas.EndPaint;
  end;
end;

function TdxTabbedMDIClientControl.GetActiveChild: THandle;
begin
  Result := SendMessage(Handle, WM_MDIGETACTIVE, 0, 0);
end;

function TdxTabbedMDIClientControl.GetMouseButton(const Message: TWMMouse): TMouseButton;
begin
  case Message.Msg of
    WM_LBUTTONDOWN, WM_LBUTTONDBLCLK, WM_LBUTTONUP:
      Result := mbLeft;
    WM_RBUTTONDOWN, WM_RBUTTONDBLCLK, WM_RBUTTONUP:
      Result := mbRight;
  else
    Result := mbMiddle;
  end;
end;

function TdxTabbedMDIClientControl.GetMappedTabsMousePosition(const P: TSmallPoint): TPoint;
begin
  Result := cxPointOffset(SmallPointToPoint(P), FClientBoundsRect.TopLeft);
end;

function TdxTabbedMDIClientControl.GetParentForm: TForm;
begin
  Result := FTabbedMDIManager.Form;
end;

function TdxTabbedMDIClientControl.GetTabbedMDIProperties: TdxTabbedMDITabProperties;
begin
  Result := FTabbedMDIManager.TabProperties;
end;

function TdxTabbedMDIClientControl.IsLocked: Boolean;
begin
  Result := (FLockCount <> 0) or IsDestroying;
end;

procedure TdxTabbedMDIClientControl.SetActiveChild(AChildWnd: THandle);
begin
  if AChildWnd <> 0 then
    SendMessage(Handle, WM_MDIACTIVATE, AChildWnd, 0);
end;

procedure TdxTabbedMDIClientControl.SetBoundsRect(ARect: TRect);
begin
  if not cxRectIsEqual(FBoundsRect, ARect) then
  begin
    FBoundsRect := ARect;
    BoundsChanged;
  end;
end;

procedure TdxTabbedMDIClientControl.SetClientBoundsRect(const Value: TRect);
begin
  if not cxRectIsEqual(FClientBoundsRect, Value) then
  begin
    FClientBoundsRect := Value;
    UpdateActiveChildBounds;
  end;
end;

procedure TdxTabbedMDIClientControl.SubscribeChild(AMDIChild: THandle);
begin
  TabbedMDIProperties.AddPage(AMDIChild);
end;

procedure TdxTabbedMDIClientControl.SubscribeChildren;
var
  I: Integer;
begin
  for I := ParentForm.MDIChildCount - 1 downto 0 do
    SubscribeChild(ParentForm.MDIChildren[I].Handle);
end;

procedure TdxTabbedMDIClientControl.UnSubscribeChild(AMDIChild: THandle);
begin
  TabbedMDIProperties.MDIChildDestroying(AMDIChild);
end;

procedure TdxTabbedMDIClientControl.UpdateActiveChildBounds;
begin
  if TabbedMDIProperties.ActivePage <> nil then
    TabbedMDIProperties.ActivePage.UpdateBounds;
end;

{ TdxTabbedMDITabControlHintHelper }

function TdxTabbedMDITabControlHintHelper.CanShowHint: Boolean;
var
  AHintControl: TWinControl;
begin
  if IsSuppressHintOnMouseDown and IsMouseButtonPressed then
  begin
    Result := False;
    Exit;
  end;
  Result := Application.Active;
  if Result then
  begin
    AHintControl := GetHintWinControl;
    Result := AHintControl.HandleAllocated and IsWindowVisible(AHintControl.Handle) and
      (cxWindowFromPoint(GetMouseCursorPos) = AHintControl.Handle);
  end;
end;

function TdxTabbedMDITabControlHintHelper.GetOwnerWinControl: TWinControl;
begin
  Result := (Controller as TdxTabbedMDITabController).FMDITabControl;
end;

function TdxTabbedMDITabControlHintHelper.PtInCaller(const P: TPoint): Boolean;
begin
  Result := inherited PtInCaller(
    cxPointOffset(P,
      TdxTabbedMDIClientControl((Controller as TdxTabbedMDITabController).FMDITabControl).ClientBoundsRect.TopLeft));
end;

{ TdxTabbedMDITabController }

constructor TdxTabbedMDITabController.Create(AOwner: TObject);
begin
  inherited Create(AOwner);
  FMDITabControl := TcxControl(AOwner);
end;

function TdxTabbedMDITabController.GetControlHandle: THandle;
begin
  Result := FMDITabControl.Handle;
end;

function TdxTabbedMDITabController.GetClientToScreen(const APoint: TPoint): TPoint;
var
  AClientBounds: TRect;
begin
  Result := APoint;
  AClientBounds := TdxTabbedMDIClientControl(FMDITabControl).ClientBoundsRect;
  Result := cxPointOffset(Result, cxPointInvert(AClientBounds.TopLeft));
  Result := inherited GetClientToScreen(Result);
end;

function TdxTabbedMDITabController.GetHintHelperClass: TcxCustomTabControlHintHelperClass;
begin
  Result := TdxTabbedMDITabControlHintHelper;
end;

function TdxTabbedMDITabController.GetScreenToClient(const APoint: TPoint): TPoint;
var
  AControl: TdxTabbedMDIClientControl;
begin
  Result := inherited GetScreenToClient(APoint);
  AControl := TdxTabbedMDIClientControl(FMDITabControl);
  Result := cxPointOffset(Result, AControl.ClientBoundsRect.TopLeft);
end;

function TdxTabbedMDITabControlViewInfo.UseRightToLeftAlignment: Boolean;
begin
  Result := (Owner as TdxTabbedMDIClientControl).FTabbedMDIManager.Form.UseRightToLeftAlignment;
end;

function TdxTabbedMDITabControlViewInfo.UseRightToLeftReading: Boolean;
begin
  Result := (Owner as TdxTabbedMDIClientControl).FTabbedMDIManager.Form.UseRightToLeftReading;
end;

function TdxTabbedMDITabControlViewInfo.HasBorders: Boolean;
begin
  Result := False;
end;

{ TdxTabbedMDIPage }

constructor TdxTabbedMDIPage.Create(AMDIManager: TdxTabbedMDIManager; AMDIChild: THandle);
var
  ATabs: TcxTabs;
  AClient: TdxTabbedMDIClientControl;
  AProperties: TdxTabbedMDITabProperties;
begin
  inherited Create;
  FTabbedMDIManager := AMDIManager;
  FForm := TForm(FindControl(AMDIChild));
  AClient := TdxTabbedMDIClientControl(FTabbedMDIManager.FClientFakeControl);
  AProperties := FTabbedMDIManager.TabProperties;
  ATabs := AProperties.Tabs;
  ATabs.BeginUpdate;
  try
    FTab := ATabs[ATabs.Add(GetTabCaptionFromForm)];
    if not AClient.IsLocked then
      FTab.Visible := False;
  finally
    ATabs.EndUpdate;
  end;
  FPage := TdxTabbedMDIChild.Create(Self, AClient);
  TdxTabbedMDIChild(FPage).Handle := AMDIChild;
end;

destructor TdxTabbedMDIPage.Destroy;
begin
  FTabbedMDIManager.TabProperties.UpdateImages;
  FreeAndNil(FPage);
  FreeAndNil(FTab);
  inherited Destroy;
end;

procedure TdxTabbedMDIPage.DoMDIChildCreated;
begin
  UpdateCaption;
  UpdateIcon;
  FTabbedMDIManager.DoPageAdded(Self);
  Tab.Visible := True;
end;

procedure TdxTabbedMDIPage.MDIChildDestroying;
begin
  TdxTabbedMDIChild(FPage).IsWndDestroying := True;
  FTabbedMDIManager.FMinimizedMDIChildren.Remove(FForm);
end;

procedure TdxTabbedMDIPage.UpdateActiveCaption;
begin
  if FTabbedMDIManager.Form.ActiveMDIChild = FForm then
    FTabbedMDIManager.UpdateCaption;
end;

procedure TdxTabbedMDIPage.UpdateBounds;
begin
  TdxTabbedMDIChild(FPage).UpdateBounds;
end;

procedure TdxTabbedMDIPage.UpdateIcon;
begin
  FTabbedMDIManager.TabProperties.UpdateImages;
end;

procedure TdxTabbedMDIPage.UpdateCaption;
begin
  Caption := GetTabCaptionFromForm;
  UpdateActiveCaption;
end;

function TdxTabbedMDIPage.GetCaption: string;
begin
  Result := FTab.Caption;
end;

function TdxTabbedMDIPage.GetHandle: THandle;
begin
  Result := FForm.Handle;
end;

function TdxTabbedMDIPage.GetImageIndex: Integer;
begin
  Result := FTab.ImageIndex;
end;

function TdxTabbedMDIPage.GetIndex: Integer;
begin
  Result := FTab.Index;
end;

function TdxTabbedMDIPage.GetMDIChild: TForm;
begin
  Result := FForm;
end;

function TdxTabbedMDIPage.GetTab: TcxTab;
begin
  Result := FTab;
end;

function TdxTabbedMDIPage.GetTabCaptionFromForm: string;
begin
  Result := StringReplace(FForm.Caption, '&', '&&', [rfReplaceAll, rfIgnoreCase]);
end;

procedure TdxTabbedMDIPage.SetCaption(const Value: string);
begin
  FTab.Caption := Value;
end;

procedure TdxTabbedMDIPage.SetImageIndex(const Value: Integer);
begin
  FTab.ImageIndex := Value;
end;

{ TdxTabbedMDITabProperties }

constructor TdxTabbedMDITabProperties.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FTabbedMDIManager := TdxTabbedMDIManager(AOwner);
  FPages := TObjectList.Create;
  FInternalImages := TcxImageList.Create(nil);
  Images := FInternalImages;
end;

destructor TdxTabbedMDITabProperties.Destroy;
begin
  FreeAndNil(FInternalImages);
  FreeAndNil(FPages);
  inherited Destroy;
end;

function TdxTabbedMDITabProperties.AddPage(AMdiChild: THandle): Integer;
begin
  Result := FPages.Add(TdxTabbedMDIPage.Create(FTabbedMDIManager, AMdiChild));
end;

procedure TdxTabbedMDITabProperties.ClearPages;
begin
  FPages.Clear;
end;

procedure TdxTabbedMDITabProperties.CloseTab(AIndex: Integer);
var
  AMDIChild: TForm;
begin
  AMDIChild := Pages[AIndex].MDIChild;
  FOldMDIChildCloseEvent := AMDIChild.OnClose;
  AMDIChild.OnClose := MDIChildClose;
  try
    AMDIChild.Close;
  finally
    AMDIChild.OnClose := FOldMDIChildCloseEvent;
  end;
end;

procedure TdxTabbedMDITabProperties.DeletePage(AIndex: Integer);
begin
  BeginUpdate;
  try
    FPages.Delete(AIndex);
    SetActivePageByHandle(TdxTabbedMDIClientControl(FTabbedMDIManager.FClientFakeControl).GetActiveChild);
  finally
    EndUpdate;
  end;
end;

function TdxTabbedMDITabProperties.GetPageIndexByHandle(AHandle: THandle): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to PageCount - 1 do
    if Pages[I].Handle = AHandle then
      Exit(I);
end;

function TdxTabbedMDITabProperties.GetTabControl: IcxTabControl;
begin
  Result := FTabbedMDIManager.TabbedControl;
end;

function TdxTabbedMDITabProperties.InternalGetTabHint(ATab: TcxTab; var ACanShow: Boolean): string;
begin
  inherited InternalGetTabHint(ATab, ACanShow);
  Result := Pages[ATab.Index].MDIChild.Caption;
end;

procedure TdxTabbedMDITabProperties.MDIChildDestroying(AHandle: THandle);
var
  AIndex: Integer;
begin
  AIndex := GetPageIndexByHandle(AHandle);
  if AIndex <> -1 then
  begin
    Pages[AIndex].MDIChildDestroying;
    DeletePage(AIndex);
  end;
end;

procedure TdxTabbedMDITabProperties.MoveTab(ACurrentIndex, ANewIndex: Integer);
begin
  inherited MoveTab(ACurrentIndex, ANewIndex);
  FPages.Move(ACurrentIndex, ANewIndex);
end;

procedure TdxTabbedMDITabProperties.SetActivePageByHandle(AHandle: THandle);
begin
  TabIndex := GetPageIndexByHandle(AHandle);
end;

procedure TdxTabbedMDITabProperties.UpdateImages;
var
  AIcon: TIcon;
  APage: TdxTabbedMDIPage;
  I: Integer;
begin
  BeginUpdate;
  try
    FInternalImages.Clear;
    FInternalImages.SourceDPI := FTabbedMDIManager.ScaleFactor.Apply(dxDefaultDPI);
    FInternalImages.Height := dxGetSystemMetrics(SM_CYSMICON, FTabbedMDIManager.ScaleFactor);
    FInternalImages.Width := dxGetSystemMetrics(SM_CXSMICON, FTabbedMDIManager.ScaleFactor);

    AIcon := TIcon.Create;
    try
      for I := 0 to FPages.Count - 1 do
      begin
        APage := Pages[I];
        AIcon.Handle := dxGetFormIcon(APage.Handle, FInternalImages.Width, FInternalImages.Height);
        APage.ImageIndex := FInternalImages.AddIcon(AIcon);
      end;
    finally
      AIcon.Free;
    end;
  finally
    EndUpdate;
  end;
end;

function TdxTabbedMDITabProperties.GetActivePage: TdxTabbedMDIPage;
begin
  Result := Pages[TabIndex];
end;

function TdxTabbedMDITabProperties.GetPageCount: Integer;
begin
  Result := FPages.Count
end;

function TdxTabbedMDITabProperties.GetPage(AIndex: Integer): TdxTabbedMDIPage;
begin
  if InRange(AIndex, 0, FPages.Count - 1) then
    Result := TdxTabbedMDIPage(FPages[AIndex])
  else
    Result := nil;
end;

function TdxTabbedMDITabProperties.GetPageIndex: Integer;
begin
  Result := TabIndex;
end;

procedure TdxTabbedMDITabProperties.MDIChildClose(Sender: TObject; var Action: TCloseAction);
begin
  if Assigned(FOldMDIChildCloseEvent) then
    FOldMDIChildCloseEvent(Sender, Action);
  if Action = caMinimize then
    Action := caNone;
end;

procedure TdxTabbedMDITabProperties.SetPageIndex(AIndex: Integer);
begin
  if InRange(AIndex, 0, PageCount - 1) then
    TabIndex := AIndex;
end;

{ TdxTabbedMDIManager }

constructor TdxTabbedMDIManager.Create(Owner: TComponent);
begin
  if not(Owner is TForm) then
    raise EdxTabbedMDIManagerException.Create(cxGetResourceString(@sdxTabbedMDIOwnerIsNotForm));
  CheckUnique(TForm(Owner));
  inherited Create(Owner);
  dxTabbedMDIManagers.Add(Self);
  FTabProperties := GetPropertiesClass.Create(Self);
  FTabProperties.OnChanged := TabPropertiesChangedHandler;
  FTabProperties.OnChange := TabPropertiesTabIndexChangedHandler;
  FTabProperties.OnGetTabHint := TabPropertiesGetTabHintHandler;
  FTabProperties.OnStyleChanged := TabPropertiesStyleChangedHandler;
  FLookAndFeel := TcxLookAndFeel.Create(Self);
  FLookAndFeel.OnChanged := LookAndFeelChangeHandler;
  FormCaptionMask := SDefaultCaptionMask;
  FMinimizedMDIChildren := TList.Create;
end;

destructor TdxTabbedMDIManager.Destroy;
begin
  Active := False;
  FreeAndNil(FMinimizedMDIChildren);
  FreeAndNil(FLookAndFeel);
  FreeAndNil(FTabProperties);
  dxTabbedMDIManagers.Remove(Self);
  inherited Destroy;
end;

procedure TdxTabbedMDIManager.Assign(Source: TPersistent);
begin
  if Source is TdxTabbedMDIManager then
  begin
    LookAndFeel := TdxTabbedMDIManager(Source).LookAndFeel;
    TabProperties := TdxTabbedMDIManager(Source).TabProperties;
    FormCaptionMask := TdxTabbedMDIManager(Source).FormCaptionMask;
    Active := TdxTabbedMDIManager(Source).Active;
  end
  else
    inherited Assign(Source);
end;

procedure TdxTabbedMDIManager.BeginUpdate;
begin
  if FClientFakeControl <> nil then
    TdxTabbedMDIClientControl(FClientFakeControl).BeginUpdate;
end;

procedure TdxTabbedMDIManager.EndUpdate(AForceUpdate: Boolean);
begin
  if FClientFakeControl <> nil then
    TdxTabbedMDIClientControl(FClientFakeControl).EndUpdate(AForceUpdate);
end;

function TdxTabbedMDIManager.IsDesigning: Boolean;
begin
  Result := csDesigning in ComponentState;
end;

function TdxTabbedMDIManager.IsDestroying: Boolean;
begin
  Result := csDestroying in ComponentState;
end;

function TdxTabbedMDIManager.IsLoading: Boolean;
begin
  Result := csLoading in ComponentState;
end;

function TdxTabbedMDIManager.GetLookAndFeel: TcxLookAndFeel;
begin
  Result := FLookAndFeel;
end;

procedure TdxTabbedMDIManager.ChangeScale(M, D: Integer);
begin
  inherited ChangeScale(M, D);
  TabProperties.UpdateImages;
  if FClientFakeControl <> nil then
  begin
    TdxTabbedMDIClientControl(FClientFakeControl).ChangeScale(M, D);
    LookAndFeel.Refresh;
  end;
end;

procedure TdxTabbedMDIManager.BuildFormatString;
begin
  FCaptionFormat := StringReplace(FFormCaptionMask, '%', '%%', [rfReplaceAll]);
  FCaptionFormat := StringReplace(FCaptionFormat, SMainFormCaptionMask, '%0:s', [rfReplaceAll, rfIgnoreCase]);
  FCaptionFormat := StringReplace(FCaptionFormat, SChildFormCaptionMask, '%1:s', [rfReplaceAll, rfIgnoreCase]);
end;

procedure TdxTabbedMDIManager.CheckUnique(AOwner: TForm);
var
  I: Integer;
begin
  for I := 0 to dxTabbedMDIManagers.Count - 1 do
    if TComponent(dxTabbedMDIManagers[I]).Owner = AOwner then
      raise EdxTabbedMDIManagerException.Create(cxGetResourceString(@sdxTabbedMDIManagerAlreadyExists));
end;

function TdxTabbedMDIManager.GetController: TdxTabbedMDITabController;
begin
  if TabbedControl <> nil then
    Result := TabbedControl.Controller as TdxTabbedMDITabController
  else
    Result := nil;
end;

function TdxTabbedMDIManager.GetFormattedCaption: string;
begin
  if FForm.ActiveMDIChild <> nil then
  begin
    if not IsFormCaptionMaskStored and FForm.UseRightToLeftAlignment then
      Result := Format(FCaptionFormat, [FForm.ActiveMDIChild.Caption, FCaption])
    else
      Result := Format(FCaptionFormat, [FCaption, FForm.ActiveMDIChild.Caption]);
  end
  else
    Result := FCaption;
end;

function TdxTabbedMDIManager.GetTabbedControl: IcxTabControl;
begin
  if Active then
    Supports(FClientFakeControl, IcxTabControl, Result)
  else
    Result := nil;
end;

function TdxTabbedMDIManager.GetViewInfo: TdxTabbedMDITabControlViewInfo;
begin
  if TabbedControl <> nil then
    Result := TabbedControl.ViewInfo as TdxTabbedMDITabControlViewInfo
  else
    Result := nil;
end;

function TdxTabbedMDIManager.IsFormCaptionMaskStored: Boolean;
begin
  Result := FFormCaptionMask <> SDefaultCaptionMask;
end;

procedure TdxTabbedMDIManager.MainFormWndProc(var Message: TMessage);

  procedure Default;
  begin
    FWindowProcObject.DefaultProc(Message);
  end;

  procedure DoFontChanged;
  begin
    LookAndFeel.Refresh;
    Default;
  end;

  procedure DoSetText(var Message: TWMSetText);
  begin
    FCaption := Message.Text;
    Message.Text := PChar(GetFormattedCaption);
    Application.Title := Message.Text;
    Default;
  end;

var
  AWasActive: Boolean;
begin
  case Message.Msg of
    WM_SETTEXT:
      DoSetText(TWMSetText(Message));
    CM_FONTCHANGED:
      DoFontChanged;
    CM_BIDIMODECHANGED:
      begin
        if Active and not (Form is TdxForm) then
          TFormAccess(Form).RecreateWnd;
        Default;
      end;
    CM_RECREATEWND:
      begin
        AWasActive := Active;
        Default;
        FIsClientWndDestroying := False;
        Active := AWasActive;
      end;
  else
    Default;
  end;
end;

procedure TdxTabbedMDIManager.SetActive(const Value: Boolean);
var
  AForm: TForm;
begin
  if FActive <> Value then
    if IsLoading then
      FLoadedActive := Value
    else
    begin
      try
        if Value then
        begin
          AForm := TForm(Owner);
          AForm.HandleNeeded;
          Form := AForm;
          if not IsDesigning then
            dxSetHook(htCBT, dxTabbedMDICBTHook);
        end
        else
        begin
          if not IsDesigning then
            dxReleaseHook(dxTabbedMDICBTHook);
          Form := nil;
        end;
        FActive := Value;
      except
        on E: EdxTabbedMDIManagerException do
          FActive := False;
      end;
    end;
end;

procedure TdxTabbedMDIManager.SetFormCaptionMask(const Value: string);
begin
  if FFormCaptionMask <> Value then
  begin
    FFormCaptionMask := Value;
    BuildFormatString;
    UpdateCaption;
  end;
end;

procedure TdxTabbedMDIManager.SetForm(Value: TForm);
begin
  if (Value <> nil) and (Value.FormStyle <> fsMDIForm) then
    raise EdxTabbedMDIManagerException.Create(cxGetResourceString(@sdxTabbedMDIManagerFormIsNotMDIForm));
  if IsDesigning then Exit;
  if (Value <> nil) and (Application.MainForm <> nil) and (Value <> Application.MainForm) then
    raise EdxTabbedMDIManagerException.Create(cxGetResourceString(@sdxTabbedMDIManagerFormIsNotMainForm));
  if Value <> FForm then
  begin
    if FForm <> nil then
      UnSubscribeForm;
    FForm := Value;
    if FForm <> nil then
      SubscribeForm;
  end;
end;

procedure TdxTabbedMDIManager.SetLookAndFeel(const Value: TcxLookAndFeel);
begin
  FLookAndFeel.Assign(Value);
end;

procedure TdxTabbedMDIManager.SetTabProperties(
  const Value: TdxTabbedMDITabProperties);
begin
  FTabProperties.Assign(Value);
end;

procedure TdxTabbedMDIManager.SubscribeClient;

  procedure DoClientSubclass;
  begin
    FClientFakeControl := TdxTabbedMDIClientControl.Create(Self);
    FClientFakeControl.HandleNeeded;
  end;

  procedure StoreProperties;
  begin
    FStoredExStyle := GetWindowLong(FClientHandle, GWL_EXSTYLE);
  end;

  procedure HideAllChildren;
  var
    I: Integer;
  begin
    for I := 0 to FForm.MDIChildCount - 1 do
      ShowWindow(FForm.MDIChildren[I].Handle, SW_HIDE);
  end;

  procedure CheckMaximizedAndMinimized;
  var
    I: Integer;
    AChildForm: TForm;
    AWindowState: TWindowState;
  begin
    FMDIChildrenMaximized := False;
    FMinimizedMDIChildren.Clear;
    for I := 0 to FForm.MDIChildCount - 1 do
    begin
      AChildForm := FForm.MDIChildren[I];
      AWindowState := AChildForm.WindowState;
      if AWindowState <> wsNormal then
      begin
        RestoreFormToNormalState(AChildForm);
        if AWindowState = wsMaximized then
        begin
          FMDIChildrenMaximized := True;
          Break;
        end
        else
          FMinimizedMDIChildren.Add(AChildForm);
      end;
    end;
  end;

begin
  if FForm <> nil then
    FClientHandle := TFormAccess(FForm).ClientHandle
  else
    FClientHandle := 0;
  if FClientHandle <> 0 then
  begin
    StoreProperties;
    CheckMaximizedAndMinimized;
    FCaption := Form.Caption;
    HideAllChildren;
    UpdateScrollBarsArea;
    DoClientSubclass;
  end;
end;

procedure TdxTabbedMDIManager.SubscribeForm;
begin
  FStoredApplicationTitle := Application.Title;
  FWindowProcObject := cxWindowProcController.Add(FForm, MainFormWndProc);
  SubscribeClient;
end;

procedure TdxTabbedMDIManager.UnSubscribeClient;

  procedure DoUnSubclass;
  begin
    FreeAndNil(FClientFakeControl);
  end;

  procedure RestoreProperties;
  begin
    SetWindowLong(FClientHandle, GWL_EXSTYLE, FStoredExStyle);
  end;

var
  AActiveChild: THandle;
  I: Integer;
begin
  if (FClientHandle <> 0) then
  begin
    AActiveChild := TdxTabbedMDIClientControl(FClientFakeControl).GetActiveChild;
    DoUnSubclass;
    if not FIsClientWndDestroying then
    begin
      UpdateCaption;
      RestoreProperties;
      RefreshWindow(FClientHandle);
      if FMDIChildrenMaximized then
        ShowWindow(AActiveChild, SW_MAXIMIZE)
      else
        for I := 0 to FMinimizedMDIChildren.Count - 1 do
          TForm(FMinimizedMDIChildren[I]).WindowState := wsMinimized;
      UpdateScrollBarsArea;
    end;
  end;
end;

procedure TdxTabbedMDIManager.UnSubscribeForm;
begin
  cxWindowProcController.Remove(FWindowProcObject);
  Application.Title := FStoredApplicationTitle;
  UnSubscribeClient;
end;

procedure TdxTabbedMDIManager.UpdateCaption;
begin
  if FForm <> nil then
  begin
    FForm.Perform(WM_SETTEXT, 0, LPARAM(FCaption));
    FForm.Perform(CM_TEXTCHANGED, 0, 0);
  end;
end;

procedure TdxTabbedMDIManager.UpdateScrollBarsArea;
begin
  SendMessage(FClientHandle, $3F, 0, 0);
end;

procedure TdxTabbedMDIManager.Loaded;
begin
  inherited;
  Active := FLoadedActive;
end;

procedure TdxTabbedMDIManager.ClientWndDestroyingNotify;
begin
  FIsClientWndDestroying := True;
  Active := False;
end;

procedure TdxTabbedMDIManager.DoPageAdded(APage: TdxTabbedMDIPage);
begin
  if Assigned(FOnPageAdded) then
    FOnPageAdded(Self, APage);
end;

procedure TdxTabbedMDIManager.LookAndFeelChangeHandler(Sender: TcxLookAndFeel;
  AChangedValues: TcxLookAndFeelValues);
begin
  if FClientFakeControl <> nil then
    TdxTabbedMDIClientControl(FClientFakeControl).LookAndFeelChangeHandler(Sender, AChangedValues);
end;

procedure TdxTabbedMDIManager.TabPropertiesChangedHandler(Sender: TObject;
  AType: TcxCustomTabControlPropertiesChangedType);
begin
  if FClientFakeControl <> nil then
    TdxTabbedMDIClientControl(FClientFakeControl).TabPropertiesChangedHandler(Sender, AType);
end;

procedure TdxTabbedMDIManager.TabPropertiesGetTabHintHandler(
  Sender: TObject; ATabIndex: Integer; var AHint: string; var ACanShow: Boolean);
begin
  if Assigned(FOnGetTabHint) then
    FOnGetTabHint(Self, TabProperties.Pages[ATabIndex], AHint, ACanShow);
end;

procedure TdxTabbedMDIManager.TabPropertiesStyleChangedHandler(Sender: TObject);
begin
  if FClientFakeControl <> nil then
    TdxTabbedMDIClientControl(FClientFakeControl).TabPropertiesStyleChangedHandler(Sender);
end;

procedure TdxTabbedMDIManager.TabPropertiesTabIndexChangedHandler(Sender: TObject);
begin
  if FClientFakeControl <> nil then
    TdxTabbedMDIClientControl(FClientFakeControl).TabPropertiesTabIndexChangedHandler(Sender);
end;

function TdxTabbedMDIManager.GetControllerClass: TdxTabbedMDITabControllerClass;
begin
  Result := TdxTabbedMDITabController;
end;

function TdxTabbedMDIManager.GetPropertiesClass: TdxTabbedMDITabPropertiesClass;
begin
  Result := TdxTabbedMDITabProperties;
end;

function TdxTabbedMDIManager.GetViewInfoClass: TdxTabbedMDITabControlViewInfoClass;
begin
  Result := TdxTabbedMDITabControlViewInfo;
end;


initialization
  dxTabbedMDIManagers := TList.Create;

finalization
  FreeAndNil(dxTabbedMDIManagers);
end.
