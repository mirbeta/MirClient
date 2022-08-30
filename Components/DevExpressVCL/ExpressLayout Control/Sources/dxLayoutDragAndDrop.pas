{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressLayoutControl common routines                     }
{                                                                    }
{           Copyright (c) 2001-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSLAYOUTCONTROL AND ALL          }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM       }
{   ONLY.                                                            }
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

unit dxLayoutDragAndDrop;

{$I cxVer.inc}

interface

uses
  Windows, Messages, Classes, Controls, Graphics, Contnrs, Forms,
  dxCore, cxClasses, cxControls, cxContainer, cxGraphics, dxForms, cxGeometry,
  dxLayoutControl, dxLayoutContainer, cxLookAndFeelPainters;

type
  TdxLayoutCustomDragAndDropController = class;

  { TdxLayoutCustomDragAndDropObject }

  TdxLayoutCustomDragAndDropObject = class(TcxDragAndDropObject)
  private
    FContainer: TdxLayoutContainer;
    FSourceItem: TdxCustomLayoutItem;
    FStartDragPoint: TPoint;
  protected
    procedure Modified;
    //
    property Container: TdxLayoutContainer read FContainer;
  public
    constructor Create(AControl: TcxControl); override;
    procedure Init(ASourceItem: TdxCustomLayoutItem; const P: TPoint); virtual;
    //
    property SourceItem: TdxCustomLayoutItem read FSourceItem write FSourceItem;
  end;

  { TdxLayoutQuickCustomizationToolbar }

  TdxLayoutQuickCustomizeButton = class
  private
    FBounds: TRect;
    FParenHandle: THandle;

    FState: TcxEditCheckState;
    FChecked: Boolean;
    FGroupIndex: Integer;
    FTag: TcxTag;
    FGlyph: TBitmap;

    procedure SetChecked(AValue: Boolean);
    procedure SetState(AState: TcxEditCheckState);

    function GetGlyph: TBitmap;
  protected
    procedure Draw(ACanvas: TcxCanvas; AScaleFactor: TdxScaleFactor);
    procedure DrawBackground(ACanvas: TcxCanvas);
    procedure DrawIcon(ACanvas: TcxCanvas; AScaleFactor: TdxScaleFactor);

    property Checked: Boolean read FChecked write SetChecked;
    property State: TcxEditCheckState read FState write SetState;
    property Glyph: TBitmap read GetGlyph;
  public
    destructor Destroy; override;
  end;

  { TdxLayoutQuickCustomizationToolbar }

  TdxLayoutQuickCustomizationToolbar = class(TdxCustomFloatForm)
  private
    FDestinationItem: TdxCustomLayoutItem;
    FButtons: TObjectList;
    FcxCanvas: TcxCanvas;
    FWasMouseOver: Boolean;
    FMouseTrackingTimer: TcxTimer;

    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure WMMouseActivate(var Message: TWMMouseActivate); message WM_MOUSEACTIVATE;
    procedure WMNCActivate(var Message: TWMNCActivate); message WM_NCACTIVATE;

    function GetButtons(AIndex: Integer): TdxLayoutQuickCustomizeButton;
    procedure SetDestinationItem(AValue: TdxCustomLayoutItem);

    // event handlers
    procedure TimerHandler(Sender: TObject);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWindowHandle(const Params: TCreateParams); override;
    procedure Paint; override;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;

    procedure DoShow; override;

    procedure MouseLeave;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure TrackMouse;

    procedure CalculateButtonState(const P: TPoint; AShiftState: TShiftState);
    procedure ClickButton(AButton: TdxLayoutQuickCustomizeButton);
    function GetButton(const P: TPoint): TdxLayoutQuickCustomizeButton;
    function GetCheckedButton(AGroupIndex: Integer): TdxLayoutQuickCustomizeButton;
    function GetActiveButton: TdxLayoutQuickCustomizeButton;
    procedure InitButtons;

    function GetAlphaBlendValue: Integer;

    // IdxFloatForm
    function GetParentForm: TCustomForm; override;

    property Buttons[Index: Integer]: TdxLayoutQuickCustomizeButton read GetButtons;
    property DestinationItem: TdxCustomLayoutItem read FDestinationItem write SetDestinationItem;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  { TdxLayoutDragAndDropObject }

  TdxLayoutDragAndDropObject = class(TdxLayoutCustomDragAndDropObject)
  private
    FController: TdxLayoutCustomDragAndDropController;
    function GetDestinationItem: TdxCustomLayoutItem;
  protected
    function CreateController: TdxLayoutCustomDragAndDropController; virtual;
    function GetDragAndDropCursor(Accepted: Boolean): TCursor; override;

    property Controller: TdxLayoutCustomDragAndDropController read FController;
  public
    constructor Create(AControl: TcxControl); override;
    destructor Destroy; override;

    procedure BeginDragAndDrop; override;
    procedure DragAndDrop(const P: TPoint; var Accepted: Boolean); override;
    procedure EndDragAndDrop(Accepted: Boolean); override;
    procedure Init(ASourceItem: TdxCustomLayoutItem; const P: TPoint); override;

    function CanDrop: Boolean;
    property DestinationItem: TdxCustomLayoutItem read GetDestinationItem;
  end;

  { TdxLayoutCustomSizingDragAndDropObject }

  TdxLayoutCustomSizingDragAndDropObject = class(TdxLayoutCustomDragAndDropObject)
  protected
    procedure RestoreSize; virtual;
  public
    procedure EndDragAndDrop(Accepted: Boolean); override;
  end;

  { TdxLayoutSizingDragAndDropObject }

  TdxLayoutSizingDragAndDropObject = class(TdxLayoutCustomSizingDragAndDropObject)
  private
    FOriginalBounds: TRect;
    FOriginalSize: TSize;
    FMarkerIndex: Integer;
  protected
    procedure RestoreSize; override;

    property MarkerIndex: Integer read FMarkerIndex;
  public
    constructor Create(AControl: TcxControl); override;
    destructor Destroy; override;

    procedure BeginDragAndDrop; override;
    procedure DragAndDrop(const P: TPoint; var Accepted: Boolean); override;
    procedure Init(ASourceItem: TdxCustomLayoutItem; const P: TPoint); override;
  end;

  { TdxLayoutSplitterDragAndDropObject }

  TdxLayoutSizingStrategy = (lssLeft, lssRight, lssClient, lssNone);

  TdxLayoutSplitterDragAndDropObject = class(TdxLayoutCustomSizingDragAndDropObject)
  private
    FParentItem: TdxCustomLayoutGroup;
    FLeftItem: TdxCustomLayoutItem;
    FRightItem: TdxCustomLayoutItem;
    FOriginalSizes: array of TPoint;
    FSizingStrategy: TdxLayoutSizingStrategy;
    FLeftItemSize: Integer;
    FRightItemSize: Integer;
    FPrevPoint: TPoint;
    FIsClosed: Boolean;

    procedure InitRelatedItems(AIsClosed: Boolean);
  protected
    procedure RestoreSize; override;

    function GetWorkAlign(AViewInfo: TdxCustomLayoutItemViewInfo): TdxLayoutAlignHorz; virtual; abstract;
    function GetItemMaxSize(AViewInfo: TdxCustomLayoutItemViewInfo): Integer; virtual; abstract;
    function GetItemMinSize(AViewInfo: TdxCustomLayoutItemViewInfo): Integer; virtual; abstract;
    function GetItemSize(AViewInfo: TdxCustomLayoutItemViewInfo): Integer; virtual; abstract;
    function GetSignificantValue(const P: TPoint): Integer; virtual; abstract;
    procedure SetItemSize(AViewInfo: TdxCustomLayoutItemViewInfo; ASize: Integer); virtual; abstract;
  public
    procedure DragAndDrop(const P: TPoint; var Accepted: Boolean); override;
    procedure Init(ASourceItem: TdxCustomLayoutItem; const P: TPoint); override;
  end;

  { TdxLayoutHSplitterDragAndDropObject }

  TdxLayoutHSplitterDragAndDropObject = class(TdxLayoutSplitterDragAndDropObject)
  protected
    function GetWorkAlign(AViewInfo: TdxCustomLayoutItemViewInfo): TdxLayoutAlignHorz; override;
    function GetItemMaxSize(AViewInfo: TdxCustomLayoutItemViewInfo): Integer; override;
    function GetItemMinSize(AViewInfo: TdxCustomLayoutItemViewInfo): Integer; override;
    function GetItemSize(AViewInfo: TdxCustomLayoutItemViewInfo): Integer; override;
    function GetSignificantValue(const P: TPoint): Integer; override;
    procedure SetItemSize(AViewInfo: TdxCustomLayoutItemViewInfo; ASize: Integer); override;
  end;

  { TdxLayoutVSplitterDragAndDropObject }

  TdxLayoutVSplitterDragAndDropObject = class(TdxLayoutSplitterDragAndDropObject)
  protected
    function GetWorkAlign(AViewInfo: TdxCustomLayoutItemViewInfo): TdxLayoutAlignHorz; override;
    function GetItemMaxSize(AViewInfo: TdxCustomLayoutItemViewInfo): Integer; override;
    function GetItemMinSize(AViewInfo: TdxCustomLayoutItemViewInfo): Integer; override;
    function GetItemSize(AViewInfo: TdxCustomLayoutItemViewInfo): Integer; override;
    function GetSignificantValue(const P: TPoint): Integer; override;
    procedure SetItemSize(AViewInfo: TdxCustomLayoutItemViewInfo; ASize: Integer); override;
  end;

  { TdxLayoutDragAndDropHelper }

  TdxLayoutDragAndDropHelper = class(TObject)
  private
    FContainer: TdxLayoutContainer;
    FDragItem: TdxCustomLayoutItem;
    FMouseDownPos: TPoint;
    procedure BeginDragAndDrop;
    function CanBeginDragAndDrop(X, Y: Integer): Boolean;
    function GetControl: TcxControl;
    function GetDragAndDropObject: TdxLayoutDragAndDropObject;
  protected
    property Control: TcxControl read GetControl;
    property DragAndDropObject: TdxLayoutDragAndDropObject read GetDragAndDropObject;
  public
    constructor Create(AContainer: TdxLayoutContainer); virtual;

    procedure InitializeDragItem(AItem: TdxCustomLayoutItem; X, Y: Integer);
    procedure Reset;
    procedure TryBeginDragAndDrop(X, Y: Integer);

    property Container: TdxLayoutContainer read FContainer;
    property DragItem: TdxCustomLayoutItem read FDragItem;
  end;

  TdxLayoutItemInsertionKind = (ikNone, ikLeft, ikTop, ikRight, ikBottom, ikInside);

  { TdxLayoutCustomDragAndDropController }

  TdxLayoutCustomDragAndDropController = class
  private
    FClosedSplitter: TdxLayoutSplitterItem;
    FIsDropParamsChanged: Boolean;
    FLockCount: Integer;
    FDragDropInfo: TdxLayoutDragDropInfo;

    FDragImage: TcxDragImage;
    FDragImageOffset: TPoint;
    FOwner: TdxLayoutDragAndDropObject;

    function GetContainer: TdxLayoutContainer;
    function GetSourceItem: TdxCustomLayoutItem;
    function GetDropAreaPartBounds: TRect;
    procedure SetDropAreaPart(Value: TdxLayoutDropAreaPart);
    procedure SetDestinationContainer(Value: TdxLayoutContainer);
    procedure SetDestinationGroup(Value: TdxCustomLayoutGroup);
    procedure SetDestinationItem(Value: TdxCustomLayoutItem);
    procedure SetHitItem(Value: TdxCustomLayoutItem);
  protected
    procedure CreateDragImage;
    procedure ResetDragAndDropObjects;
    procedure ShowDragImage;

    procedure Changed;
    procedure DropParamsChanged;
    procedure DoDropParamsChanged; virtual;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure UpdateStates; virtual;

    procedure CorrectDropAreaPart(ADestinationItem: TdxCustomLayoutItem; var AAreaPart: TdxLayoutDropAreaPart); virtual;
    procedure DetermineDropPosition; virtual;
    procedure DetermineDestinationItems; virtual;
    procedure DetermineDropAlign;
    procedure DetermineDropParams;
    procedure DetermineHitItem; virtual;
    function FindDestinationContainer(const P: TPoint): TdxLayoutContainer;
    function GetDestinationGroup: TdxCustomLayoutGroup; virtual;
    function GetDestinationImageBounds: TRect; virtual;
    function GetFittedRect(const ARect: TRect): TRect;

    procedure DropItem(ALayoutAction: TdxLayoutActionType);
    procedure RemoveItem;
    procedure MakeItemFloat;

    // Drawing
    function GetDragImageBackgroundColor(AViewInfo: TdxCustomLayoutItemViewInfo): TColor; virtual;
    function GetDragImageFrameColor(AViewInfo: TdxCustomLayoutItemViewInfo): TColor; virtual;
    procedure PaintDestinationArea(ABitmap: TcxAlphaBitmap);
    procedure PaintDraggedItem(ABitmap: TcxAlphaBitmap; AViewInfo: TdxCustomLayoutItemViewInfo); virtual;
    procedure PrepareDragImage;

    property DragScreenPoint: TPoint read FDragDropInfo.DragScreenPoint write FDragDropInfo.DragScreenPoint;
    property DropClientPoint: TPoint read FDragDropInfo.DropClientPoint write FDragDropInfo.DropClientPoint;
    property DropAreaPart: TdxLayoutDropAreaPart read FDragDropInfo.DropAreaPart write SetDropAreaPart;
    property DestinationContainer: TdxLayoutContainer read FDragDropInfo.DestinationContainer write SetDestinationContainer;
    property DestinationGroup: TdxCustomLayoutGroup read FDragDropInfo.DestinationGroup write SetDestinationGroup;
    property DestinationItem: TdxCustomLayoutItem read FDragDropInfo.DestinationItem write SetDestinationItem;
    property HitItem: TdxCustomLayoutItem read FDragDropInfo.HitItem write SetHitItem;
    property HitTest: TdxCustomLayoutHitTest read FDragDropInfo.HitTest write FDragDropInfo.HitTest;
    property Container: TdxLayoutContainer read GetContainer;
    property Owner: TdxLayoutDragAndDropObject read FOwner;
    property SourceItem: TdxCustomLayoutItem read GetSourceItem;
    property SourceItemSize: TSize read FDragDropInfo.SourceItemSize;
  public
    constructor Create(AOwner: TdxLayoutDragAndDropObject); virtual;
    destructor Destroy; override;

    procedure BeginDragAndDrop;
    procedure EndDragAndDrop(Accepted: Boolean);
    procedure DragAndDrop(const P: TPoint; var Accepted: Boolean);

    function GetCursor(Accepted: Boolean): TCursor;

    function CanDrop: Boolean;
    function CanFloat: Boolean;
    function CanRemove: Boolean;
  end;

  TdxLayoutSingleDragAndDropController = class(TdxLayoutCustomDragAndDropController);

function dxLayoutDragAndDropObject: TdxLayoutDragAndDropObject;
function dxLayoutSizingDragAndDropObject: TdxLayoutSizingDragAndDropObject;
function dxLayoutDragAndDropController: TdxLayoutCustomDragAndDropController;

implementation

uses
  Types, SysUtils, Math,
  dxHooks, dxGDIPlusAPI, dxGDIPlusClasses, cxLibraryConsts, dxLayoutSelection, dxDPIAwareUtils;

const
  dxLayoutDefaultDragDestinationColor = $D0D0EE;
  dxLayoutDefaultDragDestinationBorderColor = $000059;
  dxLayoutDefaultDragImageBackgroundColor = $EDC8A3;

  dxLayoutMultiDragDestinationColor = $E5D4C8;
  dxLayoutMultiDragPlaceDefaultColor = $E2C6B2;
  dxLayoutMultiDragPlaceActiveColor = $D37D3D;
  dxLayoutMultiDragFrameColor = $B1B2B2;

  dxCustomizeFormHitTestCodes = [htCustomizeForm, htAvailableItems, htTreeViewItems];

var
  FLayoutDragAndDropObject: TdxLayoutDragAndDropObject;
  FLayoutSizingDragAndDropObject: TdxLayoutSizingDragAndDropObject;
  FQuickCustomizationToolbar: TdxLayoutQuickCustomizationToolbar;

type
  TdxCustomLayoutItemAccess = class(TdxCustomLayoutItem);
  TdxCustomLayoutItemViewInfoAccess = class(TdxCustomLayoutItemViewInfo);
  TdxLayoutContainerAccess = class(TdxLayoutContainer);
  TdxLayoutContainerViewInfoAccess = class(TdxLayoutContainerViewInfo);
  TdxLayoutGroupAccess = class(TdxCustomLayoutGroup);
  TdxLayoutGroupViewInfoAccess = class(TdxLayoutGroupViewInfo);
  TdxLayoutSizeOptionsAccess = class(TdxLayoutSizeOptions);
  TdxLayoutSplitterItemAccess = class(TdxLayoutSplitterItem);
  TcxDragImageAccess = class(TcxDragImage);
  TcxDragDestinationImageAccess = class(TcxDragImage);

function dxLayoutDragAndDropObject: TdxLayoutDragAndDropObject;
begin
  Result := FLayoutDragAndDropObject;
end;

function dxLayoutSizingDragAndDropObject: TdxLayoutSizingDragAndDropObject;
begin
  Result := FLayoutSizingDragAndDropObject;
end;

function dxLayoutDragAndDropController: TdxLayoutCustomDragAndDropController;
begin
  if dxLayoutDragAndDropObject <> nil then
    Result := dxLayoutDragAndDropObject.Controller
  else
    Result := nil;
end;

function dxLayoutGetItemViewInfo(AItem: TdxCustomLayoutItem): TdxCustomLayoutItemViewInfoAccess;
begin
  Result := TdxCustomLayoutItemViewInfoAccess(AItem.ViewInfo);
end;

procedure dxLayoutCustomizeMouseHook(ACode: Integer; wParam: WPARAM; lParam: LPARAM; var AHookResult: LRESULT);
var
  AMHS: PMouseHookStruct;
begin
  if FQuickCustomizationToolbar <> nil then
  begin
    AMHS := PMouseHookStruct(lParam);
    if IsMouseDownMessage(wParam) and not PtInRect(FQuickCustomizationToolbar.BoundsRect, AMHS.pt) then
      FQuickCustomizationToolbar.Free;
    if wParam = WM_MOUSEMOVE then
      FQuickCustomizationToolbar.TrackMouse;
  end;
end;

procedure dxLayoutCustomizeWndProcHook(ACode: Integer; AWParam: WPARAM; ALParam: LPARAM; var AHookResult: LRESULT);
var
  AMsg: PCWPStruct;
begin
  if FQuickCustomizationToolbar <> nil then
  begin
    AMsg := PCWPStruct(ALParam);

    if (AMsg.message = WM_ACTIVATEAPP) or (AMsg.message = WM_ACTIVATE) then
      FQuickCustomizationToolbar.Free;
  end;
end;

procedure ShowQuickCustomizationToolbar(AItem: TdxCustomLayoutItem);
var
  ABounds: TRect;
  P: TPoint;
begin
  if FQuickCustomizationToolbar = nil then
  begin
    FQuickCustomizationToolbar := TdxLayoutQuickCustomizationToolbar.Create(nil);
    FQuickCustomizationToolbar.DestinationItem := AItem;
    FQuickCustomizationToolbar.Scaled := False;
    FQuickCustomizationToolbar.ScaleFactor.Assign(dxGetScaleFactor(AItem));
    FQuickCustomizationToolbar.BiDiMode := AItem.Container.ItemsParent.BiDiMode;
    P := GetMouseCursorPos;
    ABounds := FQuickCustomizationToolbar.ScaleFactor.Apply(Rect(0, 0, 130, 80));
    ABounds := cxRectOffset(ABounds, Point(P.X, P.Y - ABounds.Bottom - FQuickCustomizationToolbar.ScaleFactor.Apply(11)));
    MakeVisibleOnDesktop(ABounds, P);

    FQuickCustomizationToolbar.BoundsRect := ABounds;
    FQuickCustomizationToolbar.DoShow;
  end;
end;

{ TdxLayoutCustomDragAndDropObject }

constructor TdxLayoutCustomDragAndDropObject.Create(AControl: TcxControl);
begin
  inherited Create(AControl);
  FContainer := TdxLayoutContainerAccess((AControl as IdxLayoutContainerOwner).GetContainer).RealContainer;
end;

procedure TdxLayoutCustomDragAndDropObject.Init(ASourceItem: TdxCustomLayoutItem; const P: TPoint);
begin
  FSourceItem := ASourceItem;
  FStartDragPoint := P;
end;

procedure TdxLayoutCustomDragAndDropObject.Modified;
begin
  Container.Modified;
end;

{ TdxLayoutDragAndDropObject }

function TdxLayoutDragAndDropObject.CanDrop: Boolean;
begin
  Result := Controller.CanDrop;
end;

constructor TdxLayoutDragAndDropObject.Create(AControl: TcxControl);
begin
  inherited Create(AControl);
  FLayoutDragAndDropObject := Self;
  FController := CreateController;
end;

destructor TdxLayoutDragAndDropObject.Destroy;
begin
  FLayoutDragAndDropObject := nil;
  FreeAndNil(FController);
  inherited Destroy;
end;


function TdxLayoutDragAndDropObject.CreateController: TdxLayoutCustomDragAndDropController;
begin
  case Container.DragDropMode of
    ddmDefault: Result := TdxLayoutSingleDragAndDropController.Create(Self);
//    ddmMultiChoice: Result := TdxLayoutMultiDragAndDropController.Create(Self);
    ddmMultiChoice: Result := TdxLayoutSingleDragAndDropController.Create(Self);
  else
    Result := nil;
  end;
end;

function TdxLayoutDragAndDropObject.GetDestinationItem: TdxCustomLayoutItem;
begin
  Result := Controller.DestinationItem;
end;

function TdxLayoutDragAndDropObject.GetDragAndDropCursor(Accepted: Boolean): TCursor;
begin
  Result := Controller.GetCursor(Accepted);
end;

procedure TdxLayoutDragAndDropObject.BeginDragAndDrop;
begin
  inherited;
  if not TdxLayoutContainerAccess((Control as TdxCustomLayoutControl).Container).IsStandardCustomization then
  begin
    TdxCustomLayoutItemAccess(SourceItem).SelectComponent;
    TdxLayoutContainerAccess(Container).QuickCustomization := True;
  end;
  TdxLayoutContainerAccess(Container).PostBuildSelectionLayer;

  Controller.BeginDragAndDrop;
end;

procedure TdxLayoutDragAndDropObject.DragAndDrop(const P: TPoint; var Accepted: Boolean);
begin
  Controller.DragAndDrop(P, Accepted);
  inherited;
end;

procedure TdxLayoutDragAndDropObject.EndDragAndDrop(Accepted: Boolean);
begin
  Controller.EndDragAndDrop(Accepted);
  TdxLayoutContainerAccess(Container).QuickCustomization := False;
  TdxLayoutContainerAccess(Container).PostBuildSelectionLayer;
  inherited;
end;

procedure TdxLayoutDragAndDropObject.Init(ASourceItem: TdxCustomLayoutItem; const P: TPoint);
begin
  inherited;
  Container.Update;
end;

{ TdxLayoutCustomSizingDragAndDropObject }

procedure TdxLayoutCustomSizingDragAndDropObject.EndDragAndDrop(Accepted: Boolean);
begin
  if Accepted then
    Modified
  else
    RestoreSize;
end;

procedure TdxLayoutCustomSizingDragAndDropObject.RestoreSize;
begin
// do nothing
end;

{ TdxLayoutSizingDragAndDropObject }

constructor TdxLayoutSizingDragAndDropObject.Create(AControl: TcxControl);
begin
  inherited Create(AControl);
  FLayoutSizingDragAndDropObject := Self;
end;

destructor TdxLayoutSizingDragAndDropObject.Destroy;
begin
  FLayoutSizingDragAndDropObject := nil;
  inherited Destroy;
end;

procedure TdxLayoutSizingDragAndDropObject.BeginDragAndDrop;
begin
  inherited BeginDragAndDrop;
  if not Container.IsDesigning then
    Container.SaveToUndo;
end;

procedure TdxLayoutSizingDragAndDropObject.DragAndDrop(const P: TPoint; var Accepted: Boolean);
var
  XC, YC: Integer;
begin
  if SourceItem.AlignHorz in [ahCenter] then
    XC := 2
  else
    XC := 1;

  if SourceItem.AlignVert in [avCenter] then
    YC := 2
  else
    YC := 1;

  if MarkerIndex in [0, 6, 7] then
    SourceItem.Width := cxRectWidth(FOriginalBounds) + XC * (FStartDragPoint.X - P.X);
  if MarkerIndex in [2, 3, 4] then
    SourceItem.Width := cxRectWidth(FOriginalBounds) + XC * (P.X - FStartDragPoint.X);
  if MarkerIndex in [0, 1, 2] then
    SourceItem.Height := cxRectHeight(FOriginalBounds) + YC * (FStartDragPoint.Y - P.Y);
  if MarkerIndex in [4, 5, 6] then
    SourceItem.Height := cxRectHeight(FOriginalBounds) + YC * (P.Y - FStartDragPoint.Y);
end;

procedure TdxLayoutSizingDragAndDropObject.Init(ASourceItem: TdxCustomLayoutItem; const P: TPoint);
begin
  inherited;
  FOriginalSize := cxSize(ASourceItem.Width, ASourceItem.Height);
  FOriginalBounds := ASourceItem.ViewInfo.Bounds;
  FMarkerIndex := dxLayoutGetItemViewInfo(ASourceItem).GetMarkerIndex(P);
end;

procedure TdxLayoutSizingDragAndDropObject.RestoreSize;
begin
  if Control.IsDesigning then
  begin
    SourceItem.Width := FOriginalSize.cx;
    SourceItem.Height := FOriginalSize.cy;
  end
  else
    Container.CancelLastUndo;
end;

{ TdxLayoutSplitterDragAndDropObject }

procedure TdxLayoutSplitterDragAndDropObject.DragAndDrop(const P: TPoint; var Accepted: Boolean);

  function GetNewLeftItemSize(ADelta: Integer): Integer;
  begin
    Result := FLeftItemSize + ADelta;
  end;

  function GetNewRightItemSize(ADelta: Integer): Integer;
  begin
    Result := FRightItemSize - ADelta;
  end;

  procedure CorrectDelta(var ADelta: Integer);
  begin
    if (FLeftItem <> nil) and (FSizingStrategy = lssClient) then
    begin
      if GetNewLeftItemSize(ADelta) < GetItemMinSize(FLeftItem.ViewInfo) then
        ADelta := GetItemMinSize(FLeftItem.ViewInfo) - FLeftItemSize
      else
        if GetNewLeftItemSize(ADelta) > GetItemMaxSize(FLeftItem.ViewInfo) then
          ADelta := GetItemMaxSize(FLeftItem.ViewInfo) - FLeftItemSize;
    end;
    if (FRightItem <> nil) and (FSizingStrategy = lssClient) then
    begin
      if GetNewRightItemSize(ADelta) < GetItemMinSize(FRightItem.ViewInfo) then
        ADelta := FRightItemSize - GetItemMinSize(FRightItem.ViewInfo)
      else
        if GetNewRightItemSize(ADelta) > GetItemMaxSize(FRightItem.ViewInfo) then
          ADelta := FRightItemSize - GetItemMaxSize(FRightItem.ViewInfo);
    end;
  end;

  function CanResize(ADelta: Integer; var ANewSize: Integer): Boolean;
  begin
    Result := False;
    case GetWorkAlign(FSourceItem.ViewInfo) of
      ahLeft: Result := (FLeftItem <> nil) and TdxLayoutSplitterItemAccess(SourceItem).DoCanResize(FLeftItem, ANewSize);
      ahRight: Result := (FRightItem <> nil) and TdxLayoutSplitterItemAccess(SourceItem).DoCanResize(FRightItem, ANewSize);
    end;
  end;

var
  ADelta: Integer;
  ASizingStrategy: TdxLayoutSizingStrategy;
  ANewSize: Integer;
begin
  if not cxPointIsEqual(FPrevPoint, P) and (GetWorkAlign(FSourceItem.ViewInfo) in [ahLeft, ahRight]) then
  begin
    FPrevPoint := P;
    ADelta := GetSignificantValue(P) - GetSignificantValue(FStartDragPoint);
    if TdxLayoutSplitterItemAccess(SourceItem).IsClosed then
      if ((GetWorkAlign(FSourceItem.ViewInfo) = ahLeft) and (ADelta > 0)) or
        ((GetWorkAlign(FSourceItem.ViewInfo) = ahRight) and (ADelta < 0)) then
          InitRelatedItems(True)
      else
        Exit;
    ASizingStrategy := FSizingStrategy;
    if (FParentItem.LayoutDirection = ldHorizontal) and TdxLayoutContainerAccess(Container).UseRightToLeftAlignment then
    begin
      case ASizingStrategy of
        lssLeft:
          ASizingStrategy := lssRight;
        lssRight:
          ASizingStrategy := lssLeft;
      end;
      ADelta := -ADelta;
    end;
    CorrectDelta(ADelta);
    case GetWorkAlign(FSourceItem.ViewInfo) of
      ahLeft:
        ANewSize := GetNewLeftItemSize(ADelta);
      ahRight:
        ANewSize := GetNewRightItemSize(ADelta);
    end;
    if CanResize(ADelta, ANewSize) then
    begin
      Container.BeginUpdate;
      try
        case GetWorkAlign(FSourceItem.ViewInfo) of
          ahLeft:
            begin
              SetItemSize(FLeftItem.ViewInfo, ANewSize);
              if ASizingStrategy = lssClient then
              SetItemSize(FRightItem.ViewInfo, GetNewRightItemSize(ADelta) + GetNewLeftItemSize(ADelta) - ANewSize);
            end;
          ahRight:
            begin
              SetItemSize(FRightItem.ViewInfo, ANewSize);
              if ASizingStrategy = lssClient then
                SetItemSize(FLeftItem.ViewInfo, GetNewLeftItemSize(ADelta) + GetNewRightItemSize(ADelta) - ANewSize);
            end;
        end;
      finally
        Container.CancelUpdate;
        case GetWorkAlign(FSourceItem.ViewInfo) of
          ahLeft:
            TdxLayoutSizeOptionsAccess(FLeftItem.SizeOptions).Changed;
          ahRight:
            TdxLayoutSizeOptionsAccess(FRightItem.SizeOptions).Changed;
        end;
      end;
    end;
    Container.Update;
  end;
end;

procedure TdxLayoutSplitterDragAndDropObject.Init(ASourceItem: TdxCustomLayoutItem; const P: TPoint);
begin
  inherited;

  FParentItem := ASourceItem.Parent;

  if not TdxLayoutSplitterItemAccess(SourceItem).IsClosed then
    InitRelatedItems(False);

  FPrevPoint := P;
end;

procedure TdxLayoutSplitterDragAndDropObject.RestoreSize;
var
  I: Integer;
begin
  Container.BeginUpdate;
  try
    for I := 0 to FParentItem.VisibleCount - 1 do
      SetItemSize(FParentItem.VisibleItems[I].ViewInfo, GetSignificantValue(FOriginalSizes[I]));
    if FIsClosed then
      TdxLayoutSplitterItemAccess(SourceItem).Close;
  finally
    Container.EndUpdate(False);
  end;
end;

procedure TdxLayoutSplitterDragAndDropObject.InitRelatedItems(AIsClosed: Boolean);

  function GetSimpleStrategy(AAlign: TdxLayoutAlignHorz): TdxLayoutSizingStrategy;
  begin
    case AAlign of
      ahLeft:
        if FLeftItem <> nil then
          Result := lssLeft
        else
          Result := lssNone;
      ahRight:
        if FRightItem <> nil then
          Result := lssRight
        else
          Result := lssNone;
    else
      Result := lssNone;
    end;
    if (Result in [lssRight, lssLeft]) and (FLeftItem <> nil) and (FRightItem <> nil) and
      (GetWorkAlign(FLeftItem.ViewInfo) = ahClient) and (GetWorkAlign(FRightItem.ViewInfo) = ahClient) then
      Result := lssClient;
  end;

  function GetLeftItem: TdxCustomLayoutItem;
  var
    I: Integer;
  begin
    Result := nil;
    for I := SourceItem.VisibleIndex - 1 downto 0 do
      if not (FParentItem.VisibleItems[I] is TdxLayoutSplitterItem) and
        (GetWorkAlign(FParentItem.VisibleItems[I].ViewInfo) in [GetWorkAlign(FSourceItem.ViewInfo), ahClient]) then
      begin
        Result := FParentItem.VisibleItems[I];
        Break;
      end;
  end;

  function GetRightItem: TdxCustomLayoutItem;
  var
    I: Integer;
  begin
    Result := nil;
    for I := SourceItem.VisibleIndex + 1 to FParentItem.VisibleCount - 1 do
      if not (FParentItem.VisibleItems[I] is TdxLayoutSplitterItem) and
        (GetWorkAlign(FParentItem.VisibleItems[I].ViewInfo) in [GetWorkAlign(FSourceItem.ViewInfo), ahClient]) then
      begin
        Result := FParentItem.VisibleItems[I];
        Break;
      end;
  end;

var
  I: Integer;
  AInitialPosition: Integer;
begin
  FIsClosed := AIsClosed;
  AInitialPosition := GetSignificantValue(SourceItem.ViewInfo.Bounds.TopLeft);

  if AIsClosed then
    TdxLayoutSplitterItemAccess(SourceItem).Open;

  SetLength(FOriginalSizes, FParentItem.VisibleCount);
  for I := 0 to FParentItem.VisibleCount - 1 do
    FOriginalSizes[I] := Point(FParentItem.VisibleItems[I].Width, FParentItem.VisibleItems[I].Height);

  FLeftItem := GetLeftItem;
  FRightItem := GetRightItem;

  if FLeftItem <> nil then
    FLeftItemSize := GetItemSize(FLeftItem.ViewInfo);
  if FRightItem <> nil then
    FRightItemSize := GetItemSize(FRightItem.ViewInfo);

  case GetWorkAlign(FSourceItem.ViewInfo) of
    ahLeft:
      if (FLeftItem <> nil) and (GetWorkAlign(FLeftItem.ViewInfo) = ahClient) then
        FSizingStrategy := GetSimpleStrategy(ahRight)
      else
        FSizingStrategy := GetSimpleStrategy(ahLeft);
    ahRight:
      if (FRightItem <> nil) and (GetWorkAlign(FRightItem.ViewInfo) = ahClient) then
        FSizingStrategy := GetSimpleStrategy(ahLeft)
      else
        FSizingStrategy := GetSimpleStrategy(ahRight);
  else
    FSizingStrategy := lssNone;
  end;

  Container.BeginUpdate;
  try
    if FSizingStrategy = lssClient then
      for I := 0 to FParentItem.VisibleCount - 1 do
        if GetWorkAlign(FParentItem.VisibleItems[I].ViewInfo) = ahClient then
          SetItemSize(FParentItem.VisibleItems[I].ViewInfo, GetItemSize(FParentItem.VisibleItems[I].ViewInfo));

    if AIsClosed then
      case GetWorkAlign(FSourceItem.ViewInfo) of
        ahLeft:
          if FLeftItem <> nil then
          begin
            FLeftItemSize := FLeftItemSize - (GetSignificantValue(SourceItem.ViewInfo.Bounds.TopLeft) - AInitialPosition);
            if (FSizingStrategy = lssClient) and (FRightItem <> nil) then
            begin
              FRightItemSize := FRightItemSize + GetItemSize(FLeftItem.ViewInfo) - FLeftItemSize;
              SetItemSize(FRightItem.ViewInfo, FRightItemSize - GetItemMinSize(FLeftItem.ViewInfo));
            end;
            SetItemSize(FLeftItem.ViewInfo, GetItemMinSize(FLeftItem.ViewInfo));
          end;
        ahRight:
          if FRightItem <> nil then
          begin
            FRightItemSize := FRightItemSize - (GetSignificantValue(SourceItem.ViewInfo.Bounds.TopLeft) - AInitialPosition);
            if (FSizingStrategy = lssClient) and (FLeftItem <> nil) then
            begin
              FLeftItemSize := FRightItemSize + GetItemSize(FLeftItem.ViewInfo) - FRightItemSize;
              SetItemSize(FLeftItem.ViewInfo, FLeftItemSize - GetItemMinSize(FRightItem.ViewInfo));
            end;
            SetItemSize(FRightItem.ViewInfo, GetItemMinSize(FRightItem.ViewInfo));
          end;
      end
  finally
    Container.CancelUpdate;
  end;
end;

{ TdxLayoutHSplitterDragAndDropObject }

function TdxLayoutHSplitterDragAndDropObject.GetWorkAlign(AViewInfo: TdxCustomLayoutItemViewInfo): TdxLayoutAlignHorz;
begin
  Result := AViewInfo.AlignHorz;
end;

function TdxLayoutHSplitterDragAndDropObject.GetItemMaxSize(AViewInfo: TdxCustomLayoutItemViewInfo): Integer;
begin
  Result := AViewInfo.MaxWidth;
end;

function TdxLayoutHSplitterDragAndDropObject.GetItemMinSize(AViewInfo: TdxCustomLayoutItemViewInfo): Integer;
begin
  Result := TdxCustomLayoutItemViewInfoAccess(AViewInfo).CalculateMinWidth;
end;

function TdxLayoutHSplitterDragAndDropObject.GetItemSize(AViewInfo: TdxCustomLayoutItemViewInfo): Integer;
begin
  Result := cxRectWidth(AViewInfo.Bounds);
end;

function TdxLayoutHSplitterDragAndDropObject.GetSignificantValue(const P: TPoint): Integer;
begin
  Result := P.X;
end;

procedure TdxLayoutHSplitterDragAndDropObject.SetItemSize(AViewInfo: TdxCustomLayoutItemViewInfo; ASize: Integer);
begin
  ASize := Max(ASize, 1);
  TdxCustomLayoutItemViewInfoAccess(AViewInfo).SetItemWidth(ASize, TdxLayoutSplitterItemAccess(SourceItem).DirectAccess);
end;

{ TdxLayoutHSplitterDragAndDropObject }

function TdxLayoutVSplitterDragAndDropObject.GetWorkAlign(AViewInfo: TdxCustomLayoutItemViewInfo): TdxLayoutAlignHorz;
begin
  Result := TdxLayoutAlignHorz(AViewInfo.AlignVert);
end;

function TdxLayoutVSplitterDragAndDropObject.GetItemMaxSize(AViewInfo: TdxCustomLayoutItemViewInfo): Integer;
begin
  Result := AViewInfo.MaxHeight;
end;

function TdxLayoutVSplitterDragAndDropObject.GetItemMinSize(AViewInfo: TdxCustomLayoutItemViewInfo): Integer;
begin
  Result := TdxCustomLayoutItemViewInfoAccess(AViewInfo).CalculateMinHeight;
end;

function TdxLayoutVSplitterDragAndDropObject.GetItemSize(AViewInfo: TdxCustomLayoutItemViewInfo): Integer;
begin
  Result := cxRectHeight(AViewInfo.Bounds);
end;

function TdxLayoutVSplitterDragAndDropObject.GetSignificantValue(const P: TPoint): Integer;
begin
  Result := P.Y;
end;

procedure TdxLayoutVSplitterDragAndDropObject.SetItemSize(AViewInfo: TdxCustomLayoutItemViewInfo; ASize: Integer);
begin
  ASize := Max(ASize, 1);
  TdxCustomLayoutItemViewInfoAccess(AViewInfo).SetItemHeight(ASize, TdxLayoutSplitterItemAccess(SourceItem).DirectAccess);
end;

{ TdxLayoutDragAndDropHelper }

constructor TdxLayoutDragAndDropHelper.Create(AContainer: TdxLayoutContainer);
begin
  inherited Create;
  FContainer := TdxLayoutContainerAccess(AContainer).RealContainer;
  Reset;
end;

procedure TdxLayoutDragAndDropHelper.InitializeDragItem(AItem: TdxCustomLayoutItem;
  X, Y: Integer);
begin
  FMouseDownPos := Point(X, Y);
  FDragItem := AItem;
end;

procedure TdxLayoutDragAndDropHelper.Reset;
begin
  FDragItem := nil;
  FMouseDownPos := cxInvalidPoint;
end;

procedure TdxLayoutDragAndDropHelper.TryBeginDragAndDrop(X, Y: Integer);
begin
  if CanBeginDragAndDrop(X, Y) then
    BeginDragAndDrop;
end;

function TdxLayoutDragAndDropHelper.CanBeginDragAndDrop(X, Y: Integer): Boolean;
begin
  Result := Container.CanDragAndDrop and (Control.DragAndDropState = ddsNone) and not
    (cxPointIsEqual(FMouseDownPos, cxInvalidPoint) or IsPointInDragDetectArea(FMouseDownPos, X, Y)) and
    (DragItem <> nil) and TdxCustomLayoutItemAccess(DragItem).CanDragAndDrop(Point(X, Y));
end;

procedure TdxLayoutDragAndDropHelper.BeginDragAndDrop;
begin
  DragAndDropObject.Init(DragItem, cxInvalidPoint);
  Control.BeginDragAndDrop;
end;

function TdxLayoutDragAndDropHelper.GetDragAndDropObject: TdxLayoutDragAndDropObject;
begin
  Result := TdxLayoutDragAndDropObject(Control.DragAndDropObject);
end;

function TdxLayoutDragAndDropHelper.GetControl: TcxControl;
begin
  Result := Container.ItemsParent;
end;

{ TdxLayoutCustomDragAndDropController }

constructor TdxLayoutCustomDragAndDropController.Create(AOwner: TdxLayoutDragAndDropObject);
begin
  inherited Create;
  FOwner := AOwner;

  FDragDropInfo.DropPartSize := dxLayoutThinPartWidth;
end;

destructor TdxLayoutCustomDragAndDropController.Destroy;
begin
  inherited;
end;

procedure TdxLayoutCustomDragAndDropController.BeginDragAndDrop;
begin
  FClosedSplitter := TdxCustomLayoutItemAccess(SourceItem).FindClosedSplitter;

  Container.SaveToUndo;

  CreateDragImage;

  if not cxPointIsEqual(Owner.FStartDragPoint, cxInvalidPoint) then
    FDragImageOffset := cxPointOffset(SourceItem.ViewInfo.Bounds.TopLeft, Owner.FStartDragPoint, False)
  else
    FDragImageOffset := cxNullPoint;

  TdxCustomLayoutItemAccess(SourceItem).RealContainer.BeginUpdate;
  TdxCustomLayoutItemAccess(SourceItem).FParentBeforeDrag := SourceItem.Parent;
  TdxCustomLayoutItemAccess(SourceItem).FIndexBeforeDrag := SourceItem.Index;
  SourceItem.Parent := nil;
  TdxCustomLayoutItemAccess(SourceItem).RealContainer.EndUpdate;

  if SourceItem.Container.CustomizeForm <> nil then
  begin
    SourceItem.Container.CustomizeForm.UpdateAvailableItems;
    SourceItem.Container.CustomizeForm.UpdateVisibleItems;
  end;

  PrepareDragImage;

  FDragImageOffset.X := Min(0, FDragImageOffset.X);
  FDragImageOffset.Y := Min(0, FDragImageOffset.Y);
  if Abs(FDragImageOffset.X) > FDragImage.Width then
    FDragImageOffset.X := -FDragImage.Width + 5;
  if Abs(FDragImageOffset.Y) > FDragImage.Height then
    FDragImageOffset.Y := -FDragImage.Height + 5;
end;

procedure TdxLayoutCustomDragAndDropController.EndDragAndDrop(Accepted: Boolean);

  function CanProcessDrop: Boolean;
  begin
    Result := Accepted and (DestinationContainer <> nil) and ((CanDrop and (DestinationItem <> nil)) or CanRemove or CanFloat);
  end;

  function GetDropActionType(AAreaPart: TdxLayoutDropAreaPart): TdxLayoutActionType;
  begin
    Result := TdxLayoutGroupViewInfoAccess(DestinationGroup.ViewInfo).GetDropActionType(AAreaPart);
  end;

var
  ALinkSelf: TcxObjectLink;
  ANewClosedSplitter: TdxLayoutSplitterItem;
begin
  FreeAndNil(FDragImage);
  UpdateStates;
  Changed;
  if CanProcessDrop then
  begin
    Container.BeginUpdate;
    if Container <> DestinationContainer then
    begin
      DestinationContainer.SaveToUndo;
      DestinationContainer.BeginUpdate;
    end;
    try
      if CanDrop and (DestinationGroup <> nil) then
        DropItem(GetDropActionType(DropAreaPart))
      else
        if CanRemove then
          RemoveItem
        else
          if CanFloat then
            MakeItemFloat;
       TdxCustomLayoutItemAccess(SourceItem).FParentBeforeDrag := nil;
    finally
      ALinkSelf := cxAddObjectLink(SourceItem);
      try
        ResetDragAndDropObjects;
        if Container <> DestinationContainer then
          DestinationContainer.EndUpdate;
        Container.EndUpdate;
        if ALinkSelf.Ref <> nil then
          (ALinkSelf.Ref as TdxCustomLayoutItem).MakeVisible;
      finally
        cxRemoveObjectLink(ALinkSelf);
      end;
    end;
    Container.Update;
    if DestinationContainer <> Container then
      DestinationContainer.Update;
    Owner.Modified;
    if SourceItem is TdxLayoutSplitterItem then
      TdxLayoutSplitterItemAccess(SourceItem).Open;

    ANewClosedSplitter := TdxCustomLayoutItemAccess(SourceItem).FindClosedSplitter;
    if (FClosedSplitter <> nil) and (ANewClosedSplitter <> FClosedSplitter) then
      TdxLayoutSplitterItemAccess(FClosedSplitter).Open
    else
      if ANewClosedSplitter <> nil then
        TdxLayoutSplitterItemAccess(ANewClosedSplitter).Open;
  end
  else
  begin
//    if TdxCustomLayoutItemAccess(SourceItem).FIsFloat then
//      TdxCustomLayoutItemAccess(SourceItem).ShowFloat;
    TdxCustomLayoutItemAccess(SourceItem).FParentBeforeDrag := nil;
    ResetDragAndDropObjects;
    Container.CancelLastUndo;
  end;
  FClosedSplitter := nil;
end;

procedure TdxLayoutCustomDragAndDropController.DragAndDrop(const P: TPoint; var Accepted: Boolean);
begin
  UpdateStates;
  ShowDragImage;
  Accepted := DropAreaPart <> apNone;
end;

function TdxLayoutCustomDragAndDropController.CanDrop: Boolean;
begin
  Result := (DestinationItem <> nil) and
    (DestinationGroup <> nil) and SourceItem.CanMoveTo(DestinationGroup) and
    not TdxLayoutGroupAccess(DestinationGroup).IsLocked and
    (TdxLayoutContainerAccess(Container).IsStandardCustomization or
      TdxLayoutGroupAccess(DestinationGroup).CanQuickCustomized);
end;

function TdxLayoutCustomDragAndDropController.CanFloat: Boolean;
begin
  Result := TdxCustomLayoutItemAccess(SourceItem).CanFloat;
end;

function TdxLayoutCustomDragAndDropController.CanRemove: Boolean;
begin
  Result := (HitTest.HitTestCode in [htAvailableItems, htNone]) and
    TdxCustomLayoutItemAccess(SourceItem).CanRemove and
    not TdxLayoutContainerAccess(Container).IsQuickCustomization;
end;

procedure TdxLayoutCustomDragAndDropController.PaintDraggedItem(ABitmap: TcxAlphaBitmap; AViewInfo: TdxCustomLayoutItemViewInfo);
begin
  TdxCustomLayoutItemViewInfoAccess(AViewInfo).PaintSelection(ABitmap);
end;

function TdxLayoutCustomDragAndDropController.FindDestinationContainer(const P: TPoint): TdxLayoutContainer;

  function HasRelation(AContainer1, AContainer2: TdxLayoutContainer): Boolean;
  begin
    Result := (TdxLayoutContainerAccess(AContainer1).MasterContainer = AContainer2) or
      (TdxLayoutContainerAccess(AContainer2).MasterContainer = AContainer1) or
      (TdxLayoutContainerAccess(AContainer1).MasterContainer = TdxLayoutContainerAccess(AContainer2).MasterContainer);
  end;

  function FindAnyContainer: TdxLayoutContainer;
  var
    AContainer: IdxLayoutContainerOwner;
    ALayoutContainer: TdxLayoutContainer;
    AControl: TWinControl;
    AParentForm: TCustomForm;
    AWnd: THandle;
  begin
    Result := nil;
    AWnd := cxWindowFromPoint(P);

    while (Result = nil) and (AWnd <> 0) do
    begin
      AControl := FindControl(AWnd);
      if Supports(AControl, IdxLayoutContainerOwner, AContainer) then
      begin
        AParentForm := GetParentForm(AControl);
        if AParentForm is TdxLayoutControlCustomCustomizeForm then
          ALayoutContainer := TdxLayoutControlCustomCustomizeForm(AParentForm).Container
        else
          ALayoutContainer := AContainer.GetContainer;

        if TdxLayoutContainerAccess(ALayoutContainer).IsCustomization then
          Result := ALayoutContainer;
      end;
      AWnd := GetAncestor(AWnd, GA_PARENT);
    end;
  end;

begin
  Result := FindAnyContainer;
  if (Result = nil) or not (Result.IsDesigning or HasRelation(Result, Container)) then
    Result := Container;
end;

function TdxLayoutCustomDragAndDropController.GetDestinationGroup: TdxCustomLayoutGroup;
begin
  if HitItem = nil then
    Result := nil
  else
    if HitItem.IsRoot or (DropAreaPart in [apLastChild, apNewLayout]) and (HitItem is TdxCustomLayoutGroup) then
      Result := HitItem as TdxCustomLayoutGroup
    else
      Result := HitItem.Parent;
end;

function TdxLayoutCustomDragAndDropController.GetDestinationImageBounds: TRect;
begin
  if DestinationGroup = nil then
    Result := cxInvalidRect
  else
    Result := GetFittedRect(DestinationGroup.ViewInfo.SelectionArea);
//   Result :=  GetFittedRect(GetDropAreaPartBounds(FDragDropInfo));
end;

function TdxLayoutCustomDragAndDropController.GetFittedRect(const ARect: TRect): TRect;
begin
  cxRectIntersect(Result, ARect, DestinationContainer.ClientRect);
end;

procedure TdxLayoutCustomDragAndDropController.DropItem(ALayoutAction: TdxLayoutActionType);

  function GetOrthogonalDirection: TdxLayoutDirection;
  begin
    Result := TdxLayoutGroupAccess(DestinationGroup).GetHelperClass.GetOrthogonalDirection;
  end;

  function IsHorizontalDropAreaPart: Boolean;
  begin
    Result := DropAreaPart in [apLeft, apRight, apAfter, apBefore];
  end;

  function GetDropIndex(AParentGroup: TdxCustomLayoutGroup; const ADropAreaInfo: TdxLayoutDragDropInfo): Integer;
  var
    AAlign: TdxLayoutAlign;
    ADropPart: TdxLayoutDropAreaPart;
  begin
    ADropPart := ADropAreaInfo.DropAreaPart;
    if TdxLayoutContainerAccess(Container).UseRightToLeftAlignment then
      if ADropPart = apLeft then
        ADropPart := apRight
      else
        if ADropPart = apRight then
          ADropPart := apLeft;
    case ADropPart of
      apLeft, apTop, apBefore:
        if ADropAreaInfo.DestinationItem.IsRoot then
          Result := 0
        else
          Result := ADropAreaInfo.DestinationItem.Index;
      apRight, apBottom, apAfter:
        if ADropAreaInfo.DestinationItem.IsRoot then
          Result := AParentGroup.Count
        else
          Result := ADropAreaInfo.DestinationItem.Index + 1;
      apBetween:
        if (AParentGroup.LayoutDirection = ldHorizontal) and (ADropAreaInfo.DropClientPoint.X > ADropAreaInfo.DestinationItem.ViewInfo.Bounds.Right) or
           (AParentGroup.LayoutDirection = ldVertical) and (ADropAreaInfo.DropClientPoint.Y > ADropAreaInfo.DestinationItem.ViewInfo.Bounds.Bottom) then
          Result := ADropAreaInfo.DestinationItem.Index + 1
        else
          Result := ADropAreaInfo.DestinationItem.Index;
      apNewLayout:
        if (AParentGroup.LayoutDirection = ldHorizontal) and (ADropAreaInfo.DestinationItem.AlignHorz in [ahParentManaged, ahLeft]) or
           (AParentGroup.LayoutDirection = ldVertical) and (ADropAreaInfo.DestinationItem.AlignVert in [avParentManaged, avTop]) then
          Result := 1
        else
          Result := 0;
      apLastChild:
        begin
          AAlign := ADropAreaInfo.ExpectedAlign;
          if (AParentGroup.LayoutDirection = ldHorizontal) and (AAlign.Horz = ahRight) or
             (AParentGroup.LayoutDirection = ldVertical) and (AAlign.Vert = avBottom) then
            Result := 0
          else
            Result := AParentGroup.Count;
        end
    else
      raise EdxTestException.Create('GetDropIndex fails');
    end;
  end;

  procedure DoInsert(AParentGroup: TdxCustomLayoutGroup);
  var
    ADropIndex: Integer;
  begin
    ADropIndex := GetDropIndex(AParentGroup, FDragDropInfo);
    SourceItem.Move(AParentGroup, ADropIndex, True);
    TdxCustomLayoutItemAccess(SourceItem).Align := FDragDropInfo.ExpectedAlign;
    TdxCustomLayoutItemAccess(SourceItem).OptimizeAlignForStoring;
  end;

  procedure DoCreateGroup;
  const
    LayoutDirections: array[Boolean] of TdxLayoutDirection = (ldVertical, ldHorizontal);
  var
    AParentGroup: TdxCustomLayoutGroup;
  begin
    if DestinationItem.IsRoot then
    begin
      DestinationGroup.PutChildrenIntoHiddenGroup;
      DestinationGroup.LayoutDirection := LayoutDirections[IsHorizontalDropAreaPart];
      AParentGroup := DestinationGroup;
    end
    else
      AParentGroup := DestinationItem.PutIntoHiddenGroup(GetOrthogonalDirection);
    TdxLayoutGroupAccess(AParentGroup).BuildVisibleItemsList;
    DoInsert(AParentGroup);
  end;

  procedure DoContentInsert;
  begin
    DestinationGroup.PutChildrenIntoHiddenGroup;
    DestinationGroup.LayoutDirection := GetOrthogonalDirection;
    DoInsert(DestinationGroup);
  end;

begin
  dxTestCheck(ALayoutAction <> atNone, 'TdxLayoutCustomDragAndDropController.DropItem fails');

  if TdxCustomLayoutItemAccess(SourceItem).FIsFloat then
    TdxCustomLayoutItemAccess(SourceItem).StopFloat;

  case ALayoutAction of
    atInsert:
      DoInsert(DestinationGroup);
    atCreateGroup:
      DoCreateGroup;
    atContentInsert:
      DoContentInsert;
  end;

  if TdxLayoutContainerAccess(Container).IsQuickCustomization and TdxLayoutContainerAccess(Container).ShowQuickCustomizationToolbar then
    ShowQuickCustomizationToolbar(SourceItem);
end;

procedure TdxLayoutCustomDragAndDropController.RemoveItem;
begin
  SourceItem.Parent := nil
end;

procedure TdxLayoutCustomDragAndDropController.MakeItemFloat;
begin
  TdxCustomLayoutItemAccess(SourceItem).MakeFloat(cxPointOffset(DragScreenPoint, FDragImageOffset), True);
end;

function TdxLayoutCustomDragAndDropController.GetCursor(Accepted: Boolean): TCursor;
begin
  if CanDrop then
    Result := HitTest.GetDropCursor
  else
    if CanRemove then
      Result := crdxLayoutControlRemove
    else
      if CanFloat then
        Result := crDefault
      else
        Result := crdxLayoutControlNoDrop;
end;

procedure TdxLayoutCustomDragAndDropController.CreateDragImage;
begin
  FDragImage := TcxDragImage.Create;
  TcxDragImageAccess(FDragImage).AlphaBlendValue := TcxDragImageAccess(FDragImage).AlphaBlendValue div 3;
end;

procedure TdxLayoutCustomDragAndDropController.Changed;
begin
  DropParamsChanged;
  Owner.Dirty := True;
end;

procedure TdxLayoutCustomDragAndDropController.DropParamsChanged;
begin
  if FLockCount > 0 then
    FIsDropParamsChanged := True
  else
    DoDropParamsChanged;
end;

procedure TdxLayoutCustomDragAndDropController.DoDropParamsChanged;
begin
  TdxLayoutContainerAccess(Container).BuildSelectionLayer;
end;

procedure TdxLayoutCustomDragAndDropController.BeginUpdate;
begin
  Inc(FLockCount);
end;

procedure TdxLayoutCustomDragAndDropController.EndUpdate;
begin
  Dec(FLockCount);
  if (FLockCount = 0) and FIsDropParamsChanged then
  begin
    DropParamsChanged;
    FIsDropParamsChanged := False;
  end;
end;

procedure TdxLayoutCustomDragAndDropController.CorrectDropAreaPart(ADestinationItem: TdxCustomLayoutItem; var AAreaPart: TdxLayoutDropAreaPart);
begin
  if TdxLayoutContainerAccess(Container).IsQuickCustomization then
    if (ADestinationItem.Parent <> nil) and not TdxCustomLayoutItemAccess(ADestinationItem.Parent).CanQuickCustomized or
       ADestinationItem.IsRoot and not TdxCustomLayoutItemAccess(ADestinationItem).CanQuickCustomized then
      AAreaPart := apNone;
end;

procedure TdxLayoutCustomDragAndDropController.DetermineDropPosition;

  function GetDropAreaPart(var P: TPoint): TdxLayoutDropAreaPart;
  begin
    if HitTest.IsDropAreaPartDetermined then
      Result := HitTest.GetDropAreaPart
    else
      if HitItem <> nil then
        Result := dxLayoutGetItemViewInfo(HitItem).GetDropAreaPart(P)
      else
        Result := apNone;
  end;

var
  P: TPoint;
begin
  P := DestinationContainer.ItemsParent.ScreenToClient(DragScreenPoint);
  DropAreaPart := GetDropAreaPart(P);
  DropClientPoint := P;
end;

procedure TdxLayoutCustomDragAndDropController.DetermineHitItem;
begin
  HitItem := HitTest.GetDestinationItem;
end;

procedure TdxLayoutCustomDragAndDropController.DetermineDestinationItems;
begin
  DestinationGroup := GetDestinationGroup;
  if (HitItem <> nil) and (DestinationGroup = HitItem) and TdxCustomLayoutItemAccess(DestinationGroup).IsViewInfoValid then
    DestinationItem := TdxLayoutGroupViewInfoAccess(DestinationGroup.ViewInfo).GetDestinationItem(FDragDropInfo)
  else
    DestinationItem := HitItem;
end;

procedure TdxLayoutCustomDragAndDropController.DetermineDropAlign;
var
  AExpectedAlign: TdxLayoutAlign;
begin
  if (DestinationGroup = nil) or (DestinationItem = nil) then
    Exit;

  if not TdxCustomLayoutItemAccess(DestinationGroup).IsViewInfoValid or (HitTest.HitTestCode in dxCustomizeFormHitTestCodes) then
    AExpectedAlign := dxLayoutAlign(ahParentManaged, avParentManaged)
  else
    AExpectedAlign := TdxLayoutGroupViewInfoAccess(DestinationGroup.ViewInfo).GetDropExpectedAlign(FDragDropInfo);

  if not dxLayoutIsSameAlign(AExpectedAlign, FDragDropInfo.ExpectedAlign) then
  begin
    FDragDropInfo.ExpectedAlign := AExpectedAlign;
    Changed;
  end;
end;

procedure TdxLayoutCustomDragAndDropController.DetermineDropParams;

  procedure DetermineDestinationContainer;
  begin
    if HitItem = nil then
      DestinationContainer := FDragDropInfo.HitContainer
    else
      DestinationContainer := HitItem.Container;
  end;

begin
  DetermineDestinationContainer;

  DetermineDropPosition;
  DetermineDestinationItems;
  DetermineDropAlign;
end;

function TdxLayoutCustomDragAndDropController.GetContainer: TdxLayoutContainer;
begin
  Result := Owner.Container;
end;

function TdxLayoutCustomDragAndDropController.GetSourceItem: TdxCustomLayoutItem;
begin
  Result := Owner.SourceItem;
end;

function TdxLayoutCustomDragAndDropController.GetDropAreaPartBounds: TRect;
var
  ADragDropInfo: TdxLayoutDragDropInfo;
begin
  ADragDropInfo := FDragDropInfo;
  if TdxLayoutGroupAccess(ADragDropInfo.DestinationGroup).IsSuperfluous(False) then
    ADragDropInfo.DestinationGroup := ADragDropInfo.DestinationGroup.Parent;
  Result := TdxLayoutGroupViewInfoAccess(ADragDropInfo.DestinationGroup.ViewInfo).GetDropExpectedBounds(ADragDropInfo);
end;

function TdxLayoutCustomDragAndDropController.GetDragImageBackgroundColor(AViewInfo: TdxCustomLayoutItemViewInfo): TColor;
begin
  Result := dxLayoutDefaultDragImageBackgroundColor;
end;

function TdxLayoutCustomDragAndDropController.GetDragImageFrameColor(AViewInfo: TdxCustomLayoutItemViewInfo): TColor;
begin
  Result := dxLayoutSelectionBorderColor;
end;

procedure TdxLayoutCustomDragAndDropController.PaintDestinationArea(ABitmap: TcxAlphaBitmap);

  procedure DrawArrow(const P: TPoint; AArrowPosition: TdxCorner);
  var
    ASign: TPoint;
    APoints: array [0..10] of TPoint;
  begin
    case AArrowPosition of
      coTopLeft: ASign := Point(-1, -1);
      coTopRight: ASign := Point(+1, -1);
      coBottomLeft: ASign := Point(-1, +1);
      coBottomRight: ASign := Point(+1, +1);
    end;

    APoints[0] := P;
    APoints[1] := Point(P.X, P.Y + 6 * ASign.Y);
    APoints[2] := Point(P.X + 1 * ASign.X, P.Y + 6 * ASign.Y);
    APoints[3] := Point(P.X + 2 * ASign.X, P.Y + 5 * ASign.Y);
    APoints[4] := Point(P.X + 3 * ASign.X, P.Y + 5 * ASign.Y);
    APoints[5] := Point(P.X + 5 * ASign.X, P.Y + 7 * ASign.Y);
    APoints[6] := Point(P.X + 7 * ASign.X, P.Y + 5 * ASign.Y);
    APoints[7] := Point(P.X + 5 * ASign.X, P.Y + 3 * ASign.Y);
    APoints[8] := Point(P.X + 5 * ASign.X, P.Y + 2 * ASign.Y);
    APoints[9] := Point(P.X + 6 * ASign.X, P.Y + 1 * ASign.Y);
    APoints[10] := Point(P.X + 6 * ASign.X, P.Y);
    dxGPPaintCanvas.Polygon(APoints, 0, clLime, 1, psSolid, 255, 255);
  end;

  procedure DrawArrowLT(const P: TPoint);
  begin
    DrawArrow(P, coTopLeft);
  end;

  procedure DrawArrowLB(const P: TPoint);
  begin
    DrawArrow(P, coBottomLeft);
  end;

  procedure DrawArrowRT(const P: TPoint);
  begin
    DrawArrow(P, coTopRight);
  end;

  procedure DrawArrowRB(const P: TPoint);
  begin
    DrawArrow(P, coBottomRight);
  end;

var
  R: TRect;
  ATargetRect: TRect;
  AArrowPositions: TdxCorners;
  AArrowPosition: TdxCorner;
begin
  if (DestinationItem = nil) or (DestinationGroup = nil) or (DropAreaPart = apNone) or
    (HitTest.HitTestCode in dxCustomizeFormHitTestCodes) or not CanDrop then
    Exit;

  R := GetDestinationImageBounds;
  dxGPPaintCanvas.BeginPaint(ABitmap.Canvas.Handle, R);

  ATargetRect := GetDropAreaPartBounds;

  AArrowPositions := [coTopLeft, coTopRight, coBottomLeft, coBottomRight];

  case TdxLayoutGroupViewInfoAccess(FDragDropInfo.DestinationGroup.ViewInfo).GetDropHorzAlignForLocale(FDragDropInfo.ExpectedAlign.Horz) of
    ahLeft: AArrowPositions :=  AArrowPositions - [coTopRight, coBottomRight];
    ahRight: AArrowPositions :=  AArrowPositions - [coTopLeft, coBottomLeft];
  end;
  case FDragDropInfo.ExpectedAlign.Vert of
    avTop: AArrowPositions :=  AArrowPositions - [coBottomLeft, coBottomRight];
    avBottom: AArrowPositions :=  AArrowPositions - [coTopLeft, coTopRight];
  end;
  dxGPPaintCanvas.RoundRect(ATargetRect, dxLayoutDefaultDragDestinationBorderColor, 0, 5, 5, 1, 255, 0);

  for AArrowPosition := Low(TdxCorner) to High(TdxCorner) do
  begin
    if AArrowPosition in AArrowPositions then
    case AArrowPosition of
      coTopLeft: DrawArrowLT(ATargetRect.TopLeft);
      coBottomLeft: DrawArrowLB(Point(ATargetRect.Left, ATargetRect.Bottom));
      coTopRight: DrawArrowRT(Point(ATargetRect.Right, ATargetRect.Top));
      coBottomRight: DrawArrowRB(ATargetRect.BottomRight);
    end;
  end;

  dxGPPaintCanvas.EndPaint;
end;

procedure TdxLayoutCustomDragAndDropController.PrepareDragImage;

  function GetItemDragBounds(AViewInfo: TdxCustomLayoutItemViewInfo): TRect;
  begin
    Result := AViewInfo.SelectionBorderRect;
  end;

  function GetItemDragSize(AViewInfo: TdxCustomLayoutItemViewInfo): TSize;
  begin
    Result := cxRectSize(GetItemDragBounds(AViewInfo));
  end;

  function GetItemSourcePPI(AItem: TdxCustomLayoutItemAccess): Integer;
  var
    AScaleFactor: TdxScaleFactor;
  begin
    if AItem.FParentBeforeDrag <> nil then
      AScaleFactor := TdxCustomLayoutItemAccess(AItem.FParentBeforeDrag).ScaleFactor
    else
      AScaleFactor := AItem.ScaleFactor;

    Result := AScaleFactor.Apply(dxDefaultDPI);
  end;

  procedure CreateFloatingDragImage(AItem: TdxCustomLayoutItemAccess);
  var
    ADragImageSize: TSize;
    APrevFloatPos: TPoint;
    ASourcePPI: Integer;
    AViewInfo: TdxCustomLayoutItemViewInfoAccess;
  begin
    APrevFloatPos := AItem.FFloatPos;

    ASourcePPI := GetItemSourcePPI(AItem);

    AItem.FLockEvents := True;
    AItem.MakeFloat(Point(10000, 10000), False);
    try
      AItem.FFloatForm.ScaleForPPI(ASourcePPI);

      AItem.Container.ViewInfo.StartDragImagePainting;
      try
        AViewInfo := dxLayoutGetItemViewInfo(AItem);
        FDragDropInfo.SourceItemSize := cxSize(AViewInfo.Bounds);
        ADragImageSize := GetItemDragSize(AViewInfo);
        FDragImage.SetBounds(0, 0, ADragImageSize.cx, ADragImageSize.cy);
        AViewInfo.PaintContent(TcxDragImageAccess(FDragImage).Image, True, False);
      finally
        AItem.Container.ViewInfo.FinishDragImagePainting;
      end;
    finally
      AItem.StopFloat;
      AItem.FLockEvents := False;
      AItem.FFloatPos := APrevFloatPos;
    end;
  end;

  procedure CreateDockedDragImage(AItem: TdxCustomLayoutItemAccess);

    function GetViewInfoBounds(AViewInfo: TdxCustomLayoutItemViewInfoAccess; AHasBorderChanged: Boolean): TRect;
    begin
      if AItem.IsAvailable or not AViewInfo.ActuallyVisible then
        Result := Rect(-AViewInfo.CalculateWidth, -AViewInfo.CalculateHeight, 0, 0)
      else
      begin
        Result := AViewInfo.OriginalBounds;
        if AHasBorderChanged then
          Result := TdxLayoutGroupViewInfoAccess(AViewInfo).GetItemsAreaBounds(Result);
      end;
    end;

  var
    ANeedDestroyViewInfo: Boolean;
    APrevHasBorder: Boolean;
    APrevBounds: TRect;
    AViewInfo: TdxCustomLayoutItemViewInfoAccess;
  begin
    if not AItem.ActuallyVisible then
    begin
      AViewInfo := TdxCustomLayoutItemViewInfoAccess(TdxLayoutContainerViewInfoAccess(Container.ViewInfo).CreateItemViewInfo(nil, AItem));
      ANeedDestroyViewInfo := True;
    end
    else
    begin
      AViewInfo := dxLayoutGetItemViewInfo(AItem);
      ANeedDestroyViewInfo := False;
      APrevBounds := AViewInfo.Bounds;
    end;
    try
      APrevHasBorder := AViewInfo.HasBorder;
      Container.ViewInfo.StartDragImagePainting;
      try
        AViewInfo.Calculate(GetViewInfoBounds(AViewInfo, AViewInfo.HasBorder <> APrevHasBorder));
        FDragDropInfo.SourceItemSize := GetItemDragSize(AViewInfo);
        FDragImage.SetBounds(0, 0, FDragDropInfo.SourceItemSize.cx, FDragDropInfo.SourceItemSize.cy);
        FDragImage.Canvas.WindowOrg := GetItemDragBounds(AViewInfo).TopLeft;
        try
          AViewInfo.GetPainter.PaintDragImage(FDragImage.Canvas);
        finally
          FDragImage.Canvas.WindowOrg := cxNullPoint;
        end;
      finally
        Container.ViewInfo.FinishDragImagePainting;
      end;
    finally
      if ANeedDestroyViewInfo then
        AViewInfo.Destroy
      else
        AViewInfo.Calculate(APrevBounds);
    end;
  end;

begin
  if TdxLayoutContainerAccess(SourceItem.Container).AllowFloatingDragImage then
    CreateFloatingDragImage(TdxCustomLayoutItemAccess(SourceItem))
  else
    CreateDockedDragImage(TdxCustomLayoutItemAccess(SourceItem));
end;

procedure TdxLayoutCustomDragAndDropController.ResetDragAndDropObjects;
begin
  FDragDropInfo.HitItem := nil;
  FDragDropInfo.DestinationItem := nil;
  FDragDropInfo.DestinationGroup := nil;
  FDragDropInfo.HitTest := nil;

  TdxLayoutContainerAccess(Container).ResetDragAndDropObjects;
end;

procedure TdxLayoutCustomDragAndDropController.SetDropAreaPart(Value: TdxLayoutDropAreaPart);
begin
  if DropAreaPart <> Value then
  begin
    FDragDropInfo.DropAreaPart := Value;
    Changed;
  end;
end;

procedure TdxLayoutCustomDragAndDropController.SetDestinationContainer(Value: TdxLayoutContainer);
begin
  if DestinationContainer <> Value then
  begin
    FDragDropInfo.DestinationContainer := Value;
    Changed;
  end;
end;

procedure TdxLayoutCustomDragAndDropController.SetDestinationGroup(Value: TdxCustomLayoutGroup);
begin
  if DestinationGroup <> Value then
  begin
    FDragDropInfo.DestinationGroup := Value;
    Changed;
  end;
end;

procedure TdxLayoutCustomDragAndDropController.SetDestinationItem(Value: TdxCustomLayoutItem);
begin
  if (Value = SourceItem) or ((Value is TdxCustomLayoutGroup) and not SourceItem.CanMoveTo(Value)) then
    Value := nil;

  if DestinationItem <> Value then
  begin
    if DestinationItem <> nil then
    begin
      if HitTest.HitTestCode in dxCustomizeFormHitTestCodes then
        DestinationItem.Container.CustomizeForm.ToggleHotTrackState(DestinationItem);
    end;
    FDragDropInfo.DestinationItem := Value;
    if DestinationItem <> nil then
    begin
      if HitTest.HitTestCode in dxCustomizeFormHitTestCodes then
        DestinationItem.Container.CustomizeForm.ToggleHotTrackState(DestinationItem);
    end;
    Changed;
  end;
end;

procedure TdxLayoutCustomDragAndDropController.SetHitItem(Value: TdxCustomLayoutItem);
begin
  if HitItem <> Value then
  begin
    FDragDropInfo.HitItem := Value;
    Changed;
  end;
end;

procedure TdxLayoutCustomDragAndDropController.ShowDragImage;
begin
  if FDragImage <> nil then
  begin
    FDragImage.MoveTo(cxPointOffset(DragScreenPoint, FDragImageOffset));
    FDragImage.Visible := True;
  end;
end;

procedure TdxLayoutCustomDragAndDropController.UpdateStates;

  procedure DetermineHitContainer;
  begin
    FDragDropInfo.HitContainer := FindDestinationContainer(DragScreenPoint);
  end;

  procedure DetermineHitTest;
  begin
    HitTest := FDragDropInfo.HitContainer.GetHitTest(FDragDropInfo.HitContainer.ScreenToClient(DragScreenPoint));
  end;

begin
  BeginUpdate;
  try
    DragScreenPoint := GetMouseCursorPos;
    DetermineHitContainer;
    DetermineHitTest;
    DetermineHitItem;

    DetermineDropParams;
  finally
     EndUpdate;
  end;
end;

{ TdxLayoutQuickCustomizationToolbar }

destructor TdxLayoutQuickCustomizeButton.Destroy;
begin
  FreeAndNil(FGlyph);
  inherited;
end;

procedure TdxLayoutQuickCustomizeButton.Draw(ACanvas: TcxCanvas; AScaleFactor: TdxScaleFactor);
begin
  DrawBackground(ACanvas);
  DrawIcon(ACanvas, AScaleFactor);
end;

procedure TdxLayoutQuickCustomizeButton.DrawBackground(ACanvas: TcxCanvas);
var
  AColor: TColor;
begin
  ACanvas.Pen.Color := $696969;

  case State of
    ecsHot:
      if FChecked then
        AColor := $555555
      else
        AColor := $D3D3D3;
    ecsPressed:
      if FChecked then
        AColor := $D3D3D3
      else
        AColor := $696969;
  else {ecsNormal}
    if FChecked then
      AColor := $898989
    else
      AColor := $FBFBFB;
  end;

  ACanvas.Brush.Color := AColor;
  ACanvas.Rectangle(FBounds);
end;

procedure TdxLayoutQuickCustomizeButton.DrawIcon(ACanvas: TcxCanvas; AScaleFactor: TdxScaleFactor);

  function Scale(const V: Integer): Integer; overload;
  begin
    Result := AScaleFactor.Apply(V);
  end;

  function Scale(const V: TRect): TRect; overload;
  begin
    Result := AScaleFactor.Apply(V);
  end;

const
  FrameColor = clBlack;
var
  AOffset, R: TRect;
begin
  case FTag of
    0:
      begin
        AOffset := AScaleFactor.Apply(Rect(6, 7, 4, 7));
        if FGroupIndex = 1 then
          AOffset := cxRectRotate(AOffset);
        R := cxRectContent(FBounds, AOffset);

        if FGroupIndex = 0 then
          ACanvas.FillRect(Rect(R.Left - Scale(2), R.Top - Scale(3), R.Left, R.Bottom + Scale(3)), FrameColor)
        else
          ACanvas.FillRect(Rect(R.Left - Scale(3), R.Top - Scale(2), R.Right + Scale(3), R.Top), FrameColor);
      end;

    1:
      begin
        AOffset := Scale(Rect(4, 7, 4, 7));
        if FGroupIndex = 1 then
          AOffset := cxRectRotate(AOffset);
        R := cxRectContent(FBounds, AOffset);

        if FGroupIndex = 0 then
          ACanvas.FillRect(Rect(R.Left + Scale(5), R.Top - Scale(3), R.Right - Scale(5), R.Bottom + Scale(3)), FrameColor)
        else
          ACanvas.FillRect(Rect(R.Left - Scale(3), R.Top + Scale(5), R.Right + Scale(3), R.Bottom - Scale(5)), FrameColor);
      end;

    2:
      begin
        AOffset := Scale(Rect(4, 7, 6, 7));
        if FGroupIndex = 1 then
          AOffset := cxRectRotate(AOffset);
        R := cxRectContent(FBounds, AOffset);
        if FGroupIndex = 0 then
          ACanvas.FillRect(Rect(R.Right, R.Top - Scale(3), R.Right + Scale(2), R.Bottom + Scale(3)), FrameColor)
        else
          ACanvas.FillRect(Rect(R.Left - Scale(3), R.Bottom, R.Right + Scale(3), R.Bottom + Scale(2)), FrameColor);
      end;

    3:
      begin
        AOffset := Scale(Rect(20, 7, 20, 7));
        if FGroupIndex = 1 then
          AOffset := cxRectRotate(AOffset);
        R := cxRectContent(FBounds, AOffset);
        if FGroupIndex = 0 then
        begin
          ACanvas.FillRect(Rect(R.Left - Scale(2), R.Top - Scale(3), R.Left, R.Bottom + Scale(3)), FrameColor);
          ACanvas.FillRect(Rect(R.Right, R.Top - Scale(3), R.Right + Scale(2), R.Bottom + Scale(3)), FrameColor);
        end
        else
        begin
          ACanvas.FillRect(Rect(R.Left - Scale(3), R.Top - Scale(2), R.Right + Scale(3), R.Top), FrameColor);
          ACanvas.FillRect(Rect(R.Left - Scale(3), R.Bottom, R.Right + Scale(3), R.Bottom + Scale(2)), FrameColor);
        end;
      end;
  end;

  ACanvas.Pen.Color := $808080;
  ACanvas.Brush.Color := $FFFFFF;
  ACanvas.RoundRect(R, 3, 3);
end;

procedure TdxLayoutQuickCustomizeButton.SetChecked(AValue: Boolean);
begin
  if FChecked <> AValue then
  begin
    FChecked := AValue;
    cxInvalidateRect(FParenHandle, FBounds);
  end;
end;

procedure TdxLayoutQuickCustomizeButton.SetState(AState: TcxEditCheckState);
begin
  if FState <> AState then
  begin
    FState := AState;
    cxInvalidateRect(FParenHandle, FBounds);
  end;
end;

function TdxLayoutQuickCustomizeButton.GetGlyph: TBitmap;
begin
  if FGlyph = nil then
   FGlyph := TBitmap.Create;
  Result := FGlyph;
end;

constructor TdxLayoutQuickCustomizationToolbar.Create(AOwner: TComponent);
begin
//  inherited;
  CreateNew(AOwner);

  Position := poDesigned;
  BorderStyle := bsNone;
  FormStyle := fsStayOnTop;

  FcxCanvas := TcxCanvas.Create(Canvas);
  FButtons := TObjectList.Create;
  FMouseTrackingTimer := TcxTimer.Create(nil);
  FMouseTrackingTimer.Interval := 10;
  FMouseTrackingTimer.OnTimer := TimerHandler;

  dxSetHook(htMouse, dxLayoutCustomizeMouseHook);
  dxSetHook(htWndProc, dxLayoutCustomizeWndProcHook);
end;

destructor TdxLayoutQuickCustomizationToolbar.Destroy;
begin
  FQuickCustomizationToolbar := nil;
  dxReleaseHook(dxLayoutCustomizeWndProcHook);
  dxReleaseHook(dxLayoutCustomizeMouseHook);

  FreeAndNil(FMouseTrackingTimer);
  FreeAndNil(FButtons);
  FreeAndNil(FcxCanvas);
  inherited;
end;

procedure TdxLayoutQuickCustomizationToolbar.CreateParams(var Params: TCreateParams);
//var
//  APopupParent: TCustomForm;
begin
  inherited CreateParams(Params);

{
  with Params do
  begin
    Style := WS_POPUP;
    ExStyle := WS_EX_TOOLWINDOW or WS_EX_TOPMOST;
  end;

  APopupParent := GetParentForm(FDestinationItem.Container.ItemsParent);
  if APopupParent <> nil then
    Params.WndParent := APopupParent.Handle
  else
    if Screen.ActiveForm <> nil then
      Params.WndParent := Screen.ActiveForm.Handle;
}
end;

procedure TdxLayoutQuickCustomizationToolbar.CreateWindowHandle(const Params: TCreateParams);
begin
  inherited;
  cxSetLayeredWindowAttributes(Handle, 153);
  SetWindowRgn(Handle, CreateRoundRectRgn(0, 0, Width + 1, Height + 1, 3, 3), False);

  InitButtons;
end;

procedure TdxLayoutQuickCustomizationToolbar.DoShow;
begin
  ShowWindow(Handle, SW_SHOWNA);
end;

procedure TdxLayoutQuickCustomizationToolbar.Paint;
var
  I: Integer;
begin
  Canvas.Pen.Color := $7F7F7F;
  Canvas.Brush.Color := $B2B2B2;
  Canvas.Rectangle(ClientRect);

  for I := 0 to FButtons.Count - 1 do
    TdxLayoutQuickCustomizeButton(FButtons[I]).Draw(FcxCanvas, ScaleFactor);
end;

procedure TdxLayoutQuickCustomizationToolbar.Notification(AComponent: TComponent; AOperation: TOperation);
begin
  inherited;
  if AComponent = DestinationItem then
  begin
    FDestinationItem := nil;
    Free;
  end;
end;

procedure TdxLayoutQuickCustomizationToolbar.MouseLeave;
begin
  CalculateButtonState(cxInvalidPoint, []);
end;

procedure TdxLayoutQuickCustomizationToolbar.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  CalculateButtonState(Point(X, Y), Shift);
end;

procedure TdxLayoutQuickCustomizationToolbar.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  CalculateButtonState(Point(X, Y), Shift);
end;

procedure TdxLayoutQuickCustomizationToolbar.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  AButton: TdxLayoutQuickCustomizeButton;
begin
  inherited;
  AButton := GetButton(Point(X, Y));
  if AButton <> nil then
    ClickButton(AButton);
  CalculateButtonState(Point(X, Y), Shift);
end;

procedure TdxLayoutQuickCustomizationToolbar.ClickButton(AButton: TdxLayoutQuickCustomizeButton);
begin
  GetCheckedButton(AButton.FGroupIndex).Checked := False;
  AButton.Checked := True;
  case AButton.FGroupIndex of
    0:
      FDestinationItem.AlignHorz := TdxLayoutAlignHorz(AButton.FTag);
    1:
      FDestinationItem.AlignVert := TdxLayoutAlignVert(AButton.FTag);
  end;
end;

procedure TdxLayoutQuickCustomizationToolbar.InitButtons;

  function CreateQuickButton(const R: TRect; AGroupIndex, ATag: TcxTag): TdxLayoutQuickCustomizeButton;
  begin
    Result := TdxLayoutQuickCustomizeButton.Create;
    Result.FParenHandle := Handle;
    Result.FBounds := ScaleFactor.Apply(R);
    Result.FGroupIndex := AGroupIndex;
    Result.FTag := ATag;
  end;

begin
  FButtons.Add(CreateQuickButton(cxRectBounds(10, 20, 20, 20), 0, 0));
  FButtons.Add(CreateQuickButton(cxRectBounds(30, 20, 20, 20), 0, 1));
  FButtons.Add(CreateQuickButton(cxRectBounds(50, 20, 20, 20), 0, 2));
  FButtons.Add(CreateQuickButton(cxRectBounds(10, 40, 60, 20), 0, 3));

  FButtons.Add(CreateQuickButton(cxRectBounds(80, 10, 20, 20), 1, 0));
  FButtons.Add(CreateQuickButton(cxRectBounds(80, 30, 20, 20), 1, 1));
  FButtons.Add(CreateQuickButton(cxRectBounds(80, 50, 20, 20), 1, 2));
  FButtons.Add(CreateQuickButton(cxRectBounds(100, 10, 20, 60), 1, 3));

  Buttons[Integer(TdxCustomLayoutItemAccess(FDestinationItem).RealAlign.Horz)].FChecked := True;
  Buttons[4 + Integer(TdxCustomLayoutItemAccess(FDestinationItem).RealAlign.Vert)].FChecked := True;
end;

function TdxLayoutQuickCustomizationToolbar.GetCheckedButton(AGroupIndex: Integer): TdxLayoutQuickCustomizeButton;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FButtons.Count - 1 do
    if Buttons[I].FChecked and (Buttons[I].FGroupIndex = AGroupIndex) then
    begin
      Result := TdxLayoutQuickCustomizeButton(FButtons[I]);
      Break;
    end;
end;

function TdxLayoutQuickCustomizationToolbar.GetAlphaBlendValue: Integer;
var
  AMousePos: TPoint;
  AMouseDistance: TPoint;
  AMinMouseDistance: Integer;
  ABoundsRect: TRect;
  ALimitVisibility: Integer;
begin

  AMousePos := GetMouseCursorPos;
  ABoundsRect := BoundsRect;
  AMouseDistance.X := Max(ABoundsRect.Left - AMousePos.X, AMousePos.X - ABoundsRect.Right);
  AMouseDistance.Y := Max(ABoundsRect.Top - AMousePos.Y, AMousePos.Y - ABoundsRect.Bottom);
  AMinMouseDistance := Max(AMouseDistance.X, AMouseDistance.Y);

  if AMinMouseDistance <= 0 then
  begin
    FWasMouseOver := True;
    Result := 255;
  end
  else
  begin
    if AMinMouseDistance > 176 then
      Result := -1
    else
    begin
      if FWasMouseOver then
        ALimitVisibility := ScaleFactor.Apply(84)
      else
        ALimitVisibility := ScaleFactor.Apply(13);

      if AMinMouseDistance > ALimitVisibility then
        Result := 0
      else
        Result := MulDiv(255, ALimitVisibility - AMinMouseDistance, ALimitVisibility);
    end;
  end;
end;

function TdxLayoutQuickCustomizationToolbar.GetButton(const P: TPoint): TdxLayoutQuickCustomizeButton;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FButtons.Count - 1 do
    if PtInRect(Buttons[I].FBounds, P) then
    begin
      Result := TdxLayoutQuickCustomizeButton(FButtons[I]);
      Break;
    end;
end;

function TdxLayoutQuickCustomizationToolbar.GetActiveButton: TdxLayoutQuickCustomizeButton;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FButtons.Count - 1 do
    if Buttons[I].State in [ecsHot, ecsPressed]  then
    begin
      Result := Buttons[I];
      Break;
    end;
end;

procedure TdxLayoutQuickCustomizationToolbar.CalculateButtonState(const P: TPoint; AShiftState: TShiftState);
var
  APrevHotButton, ANewHotButton: TdxLayoutQuickCustomizeButton;
begin
  APrevHotButton := GetActiveButton;
  ANewHotButton := GetButton(P);
  if (APrevHotButton <> ANewHotButton) or
     (ANewHotButton <> nil) and
       ((ssLeft in AShiftState) and (ANewHotButton.State = ecsHot) or
       not (ssLeft in AShiftState) and (ANewHotButton.State = ecsPressed)) then
  begin
    if APrevHotButton <> nil then
      APrevHotButton.State := ecsNormal;
    if ANewHotButton <> nil then
      if ssLeft in AShiftState then
        ANewHotButton.State := ecsPressed
      else
        ANewHotButton.State := ecsHot;
  end;
end;

procedure TdxLayoutQuickCustomizationToolbar.TrackMouse;
var
  AAlpha: Integer;
begin
  AAlpha := GetAlphaBlendValue;
  if AAlpha = -1 then
    Free
  else
    cxSetLayeredWindowAttributes(Handle, AAlpha);
end;

procedure TdxLayoutQuickCustomizationToolbar.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  MouseLeave;
end;

procedure TdxLayoutQuickCustomizationToolbar.WMMouseActivate(var Message: TWMMouseActivate);
begin
  inherited;
  Message.Result := MA_NOACTIVATE;
end;

procedure TdxLayoutQuickCustomizationToolbar.WMNCActivate(var Message: TWMNCActivate);
begin
  if Message.Active and (GetParentForm <> nil) and GetParentForm.HandleAllocated then
    SendMessage(GetParentForm.Handle, WM_NCACTIVATE, WPARAM(True), 0);
  Message.Active := True;
  inherited;
end;

function TdxLayoutQuickCustomizationToolbar.GetButtons(AIndex: Integer): TdxLayoutQuickCustomizeButton;
begin
  Result := FButtons[AIndex] as TdxLayoutQuickCustomizeButton;
end;

procedure TdxLayoutQuickCustomizationToolbar.SetDestinationItem(AValue: TdxCustomLayoutItem);
begin
  FDestinationItem := AValue;
  cxAddFreeNotification(Self, FDestinationItem);
end;

function TdxLayoutQuickCustomizationToolbar.GetParentForm: TCustomForm;
begin
  if DestinationItem <> nil then
    Result := Forms.GetParentForm(TdxCustomLayoutItemAccess(FDestinationItem).RealContainer.ItemsParent)
  else
    Result := nil;
end;

procedure TdxLayoutQuickCustomizationToolbar.TimerHandler(Sender: TObject);
begin
  TrackMouse;
end;

initialization

finalization
  FreeAndNil(FQuickCustomizationToolbar);

end.
