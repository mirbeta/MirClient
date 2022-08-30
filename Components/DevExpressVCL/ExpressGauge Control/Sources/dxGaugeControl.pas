{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressGaugeControl                                      }
{                                                                    }
{           Copyright (c) 2013-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSGAUGECONTROL AND ALL           }
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

unit dxGaugeControl;

{$I cxVer.inc}

interface

uses
  SysUtils, Classes, Controls, Windows, Graphics, Messages, Contnrs, Menus, Generics.Defaults, Generics.Collections,
  Types, dxCore, cxGeometry, cxGraphics, dxGDIPlusClasses, cxClasses, cxControls, cxLookAndFeels, dxGaugeCustomScale,
  dxGaugeControlSelection, dxAnimation;

type
  TdxCustomGaugeControlController = class;
  TdxCustomGaugeControlPainter = class;
  TdxGaugeControlHitTest = class;

  TdxGaugeDrawMode = (dmByLayer, dmByScale);

  { TdxCustomGaugeControl }

  TdxCustomGaugeControl = class(TcxControl, IdxSkinSupport)
  private
    FController: TdxCustomGaugeControlController;
    FDrawMode: TdxGaugeDrawMode;
    FIsDrawingPreview: Boolean;
    FLockCount: Integer;
    FPainter: TdxCustomGaugeControlPainter;
    FScales: TdxGaugeScaleCollection;

    procedure SetDrawMode(const AValue: TdxGaugeDrawMode);
    procedure SetScales(const AValue: TdxGaugeScaleCollection);

    procedure AddScaleHandler(ASender: TObject);
    procedure DeleteScaleHandler(ASender: TObject);
    procedure ScaleChangeHandler(Sender: TObject; AItem: TcxComponentCollectionItem;
      AAction: TcxComponentCollectionNotification);

    function GetLocked: Boolean;
    function GetLayerCount: Integer;
    function IsShiftPressed: Boolean;

    procedure Changed;
    procedure DoInvalidate;
    procedure WMSetCursor(var Message: TWMSetCursor); message WM_SETCURSOR;
  protected
    function GetDesignHitTest(X, Y: Integer; Shift: TShiftState): Boolean; override;
    function GetDragAndDropObjectClass: TcxDragAndDropObjectClass; override;
    function GetIsDesigning: Boolean; override;
    function HasBackground: Boolean; override;
    function IsDoubleBufferedNeeded: Boolean; override;
    function IsTransparentBackground: Boolean; override;
    function NeedRedrawOnResize: Boolean; override;
    procedure DoPaint; override;
    procedure Loaded; override;
    procedure LookAndFeelChanged(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues); override;
    procedure Resize; override;
    procedure ChangeScaleEx(M, D: Integer; isDpiChange: Boolean); override;
    function StartDragAndDrop(const P: TPoint): Boolean; override;
    procedure TransparentChanged; override;
    procedure WndProc(var Message: TMessage); override;

    procedure DrawPreview(ACanvas: TcxCanvas; ADrawBackground: Boolean = True);

    property IsDrawingPreview: Boolean read FIsDrawingPreview;
    property Controller: TdxCustomGaugeControlController read FController;
    property DrawMode: TdxGaugeDrawMode read FDrawMode write SetDrawMode;
    property LayerCount: Integer read GetLayerCount;
    property Locked: Boolean read GetLocked;
    property Painter: TdxCustomGaugeControlPainter read FPainter;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Assign(ASource: TPersistent); override;
    procedure GetChildren(AProc: TGetChildProc; ARoot: TComponent); override;

    function AddContainer: TdxGaugeContainerScale;
    function AddScale(AScaleClass: TdxGaugeCustomScaleClass): TdxGaugeCustomScale;
    procedure DeleteScale(AScaleIndex: Integer);
    procedure Clear;

    procedure BeginUpdate;
    procedure CancelUpdate;
    procedure EndUpdate;

    property Scales: TdxGaugeScaleCollection read FScales write SetScales;
    property Transparent;
  end;

  { TdxGaugeControl }

  TdxGaugeControl = class(TdxCustomGaugeControl)
  published
    property Align;
    property Anchors;
    property BorderStyle default cxcbsDefault;
    property Color default clWindow;
    property Constraints;
    property LookAndFeel;
    property ParentColor default False;
    property Scales;
    property ShowHint default False;
    property Transparent;
    property Visible;

    property OnClick;
    property OnDblClick;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
  end;

  { TdxGaugeControlHitTest }

  TdxGaugeControlHitTest = class
  private
    FCursor: TCursor;
    FControl: TdxCustomGaugeControl;
    FDragOperation: TdxGaugeScaleDragOperation;
    FHitObject: TComponent;

    function GetDesignCursor: TCursor;
    function GetDesignDragOperation(const P: TPoint; AHitObject: TComponent): TdxGaugeScaleDragOperation;
    function GetDesignMarkerIndex(const AMarkers: TRects; const P: TPoint): Integer;
    function GetHitObject(const P: TPoint; ACheckScaleSelectorRect: Boolean): TComponent;
    function IsHitObject(const P: TPoint; AComponent: TComponent; ACheckScaleSelectorRect: Boolean): Boolean;
  protected
    function CanAnchoring(const P: TPoint): Boolean;
    procedure Calculate(const P: TPoint);

    property Control: TdxCustomGaugeControl read FControl;
    property Cursor: TCursor read FCursor;
    property DragOperation: TdxGaugeScaleDragOperation read FDragOperation;
    property HitObject: TComponent read FHitObject write FHitObject;
  public
    constructor Create(AControl: TdxCustomGaugeControl);
  end;

  { TdxCustomGaugeControlController }

  TdxCustomGaugeControlController = class
  private
    FChangedLayers: TdxGaugeScaleChangedLayers;
    FControl: TdxCustomGaugeControl;
    FDesignCursor: TCursor;
    FDesignDragOperation: TdxGaugeScaleDragOperation;
    FDesignPopupMenu: TPopupMenu;
    FLayerCount: Integer;
    FHitTest: TdxGaugeControlHitTest;
    FSelectionHelper: TdxGaugeControlCustomSelectionHelper;
    FZOrders: TList;

    function GetIsControlSelected: Boolean;
    function GetSelections: TList;
    procedure PopulateAnchoredScales(AAnchoredScales: TList; AScale: TdxGaugeCustomScale);
    // Design Drag Object
    function GetDragObjectSizeDelta(ADragObject: TdxGaugeCustomScale; const AMousePosDelta: TPoint): TdxPointF;
    procedure MoveDragObject(ADragObject: TdxGaugeCustomScale; const AMousePosDelta: TPoint);
    procedure MoveDragObjects(const AMousePosDelta: TPoint);
    procedure SetDragObjectSize(ADragObject: TdxGaugeCustomScale; const AMousePosDelta: TPoint);
    procedure SetDragObjectWidth(ADragObject: TdxGaugeCustomScale; ADelta: Single; AApplyMinSize: Boolean);
    procedure SetDragObjectHeight(ADragObject: TdxGaugeCustomScale; ADelta: Single; AApplyMinSize: Boolean);
    // ZOrder
    procedure BringBackward(AScale: TdxGaugeCustomScale);
    procedure BringForward(AScale: TdxGaugeCustomScale);
    procedure BringToFront(AScale: TdxGaugeCustomScale);
    procedure DoChangeZOrder(AScale: TdxGaugeCustomScale; AType: TdxGaugeScaleZOrderChangeType);
    procedure SendToBack(AScale: TdxGaugeCustomScale);
    procedure SetZOrder(AScale: TdxGaugeCustomScale; const AValue: Integer);
    // Design Popup Menu
    function CanScaleElementEditors: Boolean;
    function CanShowRangeEditor: Boolean;
    function CheckDesignPopupMenu(AShift: TShiftState): Boolean;
    function CreatePopupMenuItem(const ACaption: string; ATag: TcxTag; AOnClick: TNotifyEvent): TMenuItem;
    procedure AddDeleteScaleMenuItem(APopupMenu: TPopupMenu);
    procedure AddRestoreScaleParametersMenuItem(APopupMenu: TPopupMenu);
    procedure AddScalesMenuItems(APopupMenu: TPopupMenu);
    procedure AddZOrdersMenuItems(APopupMenu: TPopupMenu);
    procedure MenuItemOnClickHandler(ASender: TObject);
    procedure PopupDesignMenu(const APopupPoint: TPoint);
    procedure ShowDesignPopupMenu(AShift: TShiftState);
    // Internal
    function CanCalculate(const ABounds: TRect): Boolean;
    function CheckComponentState: Boolean;
    function GetSelectionContentRects: TRects;
    function GetSelectionRects(AList: TList): TRects;
    function GetScaleBounds(AScale: TdxGaugeCustomScale): TdxRectF;
    procedure AddScale(AScaleClassIndex: Integer);
    procedure Calculate;
    procedure DoCalculateScales(AScaleList: TList);
  protected
    function CanAnchorScale(AScaleIndex, AAnchorScaleIndex: Integer): Boolean;
    function CreateAnchoredScaleList(AAnchorScaleList: TList): TList;
    function IsSelectionHelperAvailable: Boolean;
    function IsScaleElement: Boolean;
    function IsScalePopupMenuShowing: Boolean;
    function GetDesignCursor(const P: TPoint): TCursor;
    function GetDesignHitTest(const P: TPoint; AShift: TShiftState): Boolean;
    function GetDragAndDropObjectClass: TcxDragAndDropObjectClass;
    procedure AddCaption(AScale: TdxGaugeCustomScale);
    procedure AddRange(AScale: TdxGaugeCustomScale);
    procedure CopyDragObject(const AMousePosDelta: TPoint);
    procedure GetScalesByDependences(AList: TList);
    procedure LinkDragObject; overload;
    procedure LinkDragObject(AScale, AAnchorScale: TdxGaugeCustomScale); overload;
    procedure PopulateDesignPopupMenu(APopupMenu: TPopupMenu);
    procedure Select(AScale: TComponent; AShift: TShiftState = []);
    procedure DeleteSelection;
    procedure SetDragOperation(AOperation: TdxGaugeScaleDragOperation);
    procedure ShowScalesEditor;
    procedure UpdateAnchorReferences(AScale: TdxGaugeCustomScale);
    // ZOrder
    procedure AddZOrder(AScale: TdxGaugeCustomScale);
    procedure ChangeZOrder(AScale: TdxGaugeCustomScale; AType: TdxGaugeScaleZOrderChangeType); overload;
    procedure ChangeZOrder(AScales: TList; AType: TdxGaugeScaleZOrderChangeType); overload;
    procedure ChangeSelectionsZOrders(AType: TdxGaugeScaleZOrderChangeType);
    procedure PopulateZOrders;
    procedure RemoveZOrder(AScale: TdxGaugeCustomScale);

    property ChangedLayers: TdxGaugeScaleChangedLayers read FChangedLayers write FChangedLayers;
    property Control: TdxCustomGaugeControl read FControl;
    property DesignDragOperation: TdxGaugeScaleDragOperation read FDesignDragOperation;
    property IsControlSelected: Boolean read GetIsControlSelected;
    property LayerCount: Integer read FLayerCount;
    property HitTest: TdxGaugeControlHitTest read FHitTest;
    property Selections: TList read GetSelections;
    property ZOrders: TList read FZOrders;
  public
    constructor Create(AControl: TdxCustomGaugeControl);
    destructor Destroy; override;
  end;

  { TdxCustomGaugeControlPainter }

  TdxCustomGaugeControlPainter = class
  private
    FControl: TdxCustomGaugeControl;
    FDynamicElementsLayer: TcxBitmap32;
    FStaticElementsLayer: TcxBitmap32;
    FScaleDynamicLayerIndexes: TIntegerDynArray;
    FScaleStaticLayerIndexes: TIntegerDynArray;

    function NeedDrawSelector: Boolean;
    procedure DrawBackground(ACanvas: TcxCanvas);
    procedure DrawBorders(ACanvas: TcxCanvas);
    procedure DrawBitmaps(AGPGraphics: TdxGPGraphics);
    procedure DrawContentRects(AGPGraphics: TdxGPGraphics);
    procedure DrawControlSelector(ACanvas: TcxCanvas);
    procedure DrawGaugeLayer(ABitmap: TcxBitmap32; const AScaleLayerIndexes: array of Integer);
    procedure DrawLayer(AGPGraphics: TdxGPGraphics; const AScaleLayerIndexes: array of Integer);
    procedure DrawScales(AGPGraphics: TdxGPGraphics; ADrawMode: TdxGaugeDrawMode);
    procedure DrawSelectors(ACanvas: TcxCanvas);
    procedure DrawSelections(AGPGraphics: TdxGPGraphics);
    procedure PrepareBitmap(ABitmap: TcxBitmap32);
    procedure PrepareBitmaps;
  protected
    procedure DrawScale(AGPGraphics: TdxGPGraphics; const R: TRect; AScaleIndex: Integer);
    procedure Paint(ACanvas: TcxCanvas; ADrawMode: TdxGaugeDrawMode); overload;
    procedure Paint(ACanvas: TcxCanvas; ADrawMode: TdxGaugeDrawMode; ADrawBackground: Boolean); overload;
  public
    constructor Create(AControl: TdxCustomGaugeControl);
    destructor Destroy; override;
  end;

var
  dxGaugeControlSelectionHelperClass: TdxGaugeControlCustomSelectionHelperClass;

implementation

uses
  Math, Forms, cxLibraryConsts, dxCoreGraphics, dxGDIPlusAPI, dxGaugeQuantitativeScale, dxGaugeUtils;

type
  TdxCustomGaugeControlAccess = class(TdxCustomGaugeControl);
  TdxGaugeControlCustomSelectionHelperAccess = class(TdxGaugeControlCustomSelectionHelper);
  TdxGaugeCustomCaptionAccess = class(TdxGaugeCustomCaption);
  TdxGaugeCustomRangeAccess = class(TdxGaugeCustomRange);
  TdxGaugeCustomScaleAccess = class(TdxGaugeCustomScale);
  TdxGaugeCustomScaleAccessClass = class of TdxGaugeCustomScaleAccess;
  TdxGaugeScaleCollectionAccess = class(TdxGaugeScaleCollection);
  TdxGaugeQuantitativeScaleAccess = class(TdxGaugeQuantitativeScale);

  { TdxGaugeControlDragImageInfo }

  TdxGaugeControlDragImage = class(TcxDragImage)
  private
    FOffsetPoint: TPoint;
  protected
    property OffsetPoint: TPoint read FOffsetPoint write FOffsetPoint;
  end;

  { TdxGaugeControlDragAndDropObject }

  TdxGaugeControlDragAndDropObject = class(TcxDragAndDropObject)
  private
    FDragImages: TList;
    FIsCopying: Boolean;
    FStartDragPoint: TPoint;

    function CreateDragImage(ADragScale: TdxGaugeCustomScaleAccess): TdxGaugeControlDragImage;
    function GetControl: TdxCustomGaugeControl;
    function GetMousePosDelta: TPoint;
    function GetScaleSize(ADragScale: TdxGaugeCustomScale): TSize;
    function GetSizeDelta: TPoint;
    procedure DrawDragImage(ACanvas: TcxCanvas; const R: TRect; AScale: TdxGaugeCustomScale);
    procedure InitDragImages;
    procedure PopulateDragImages(AScaleList: TList);
    procedure ShowDragImages(const ACursorPos: TPoint);

    function CanChangeHeight(ADragObject: TdxGaugeCustomScale; AOperation: TdxGaugeScaleDragOperation;
      out ADelta: Integer): Boolean;
    function CanChangeWidth(ADragObject: TdxGaugeCustomScale; AOperation: TdxGaugeScaleDragOperation;
      out ADelta: Integer): Boolean;
    function GetCurrentCursor: TCursor;
    procedure MoveDragObjects(const AMousePosDelta: TPoint);
    procedure ResizeDragObject(const ACursorPos: TPoint);
    procedure UpdateCursor(AShiftState: TShiftState);
  protected
    function GetDragAndDropCursor(Accepted: Boolean): TCursor; override;
    function GetImmediateStart: Boolean; override;
    function ProcessKeyDown(AKey: Word; AShiftState: TShiftState): Boolean; override;
    function ProcessKeyUp(AKey: Word; AShiftState: TShiftState): Boolean; override;
    procedure BeginDragAndDrop; override;
    procedure DragAndDrop(const P: TPoint; var Accepted: Boolean); override;
    procedure EndDragAndDrop(Accepted: Boolean); override;
    procedure Init(const P: TPoint);
  end;

{ TdxCustomGaugeControl }

constructor TdxCustomGaugeControl.Create(AOwner: TComponent);
const
  dxGaugeControlDefaultWidth = 250;
  dxGaugeControlDefaultHeight = 250;
begin
  inherited Create(AOwner);
  BorderStyle := cxcbsDefault;
  Color := clWindow;
  ParentColor := False;
  Width := dxGaugeControlDefaultWidth;
  Height := dxGaugeControlDefaultHeight;

  FScales := TdxGaugeScaleCollection.Create(Self, TdxGaugeCustomScale);
  FScales.OnChange := ScaleChangeHandler;
  TdxGaugeScaleCollectionAccess(FScales).OnAdded := AddScaleHandler;
  TdxGaugeScaleCollectionAccess(FScales).OnDeleted := DeleteScaleHandler;
  FController := TdxCustomGaugeControlController.Create(Self);
  FPainter := TdxCustomGaugeControlPainter.Create(Self);
end;

destructor TdxCustomGaugeControl.Destroy;
begin
  FreeAndNil(FPainter);
  FreeAndNil(FController);
  FreeAndNil(FScales);
  inherited Destroy;
end;

procedure TdxCustomGaugeControl.Assign(ASource: TPersistent);
begin
  if ASource is TdxCustomGaugeControl then
  begin
    BeginUpdate;
    try
      Scales.Assign(TdxCustomGaugeControl(ASource).Scales);
      Transparent := TdxCustomGaugeControl(ASource).Transparent;
      Color := TdxCustomGaugeControl(ASource).Color;
    finally
      EndUpdate;
    end;
  end
  else
    inherited Assign(ASource);
end;

procedure TdxCustomGaugeControl.GetChildren(AProc: TGetChildProc; ARoot: TComponent);
var
  I: Integer;
begin
  for I := 0 to Scales.Count - 1 do
    if Scales[I].Owner = ARoot then
      AProc(Scales[I]);
end;

function TdxCustomGaugeControl.AddContainer: TdxGaugeContainerScale;
begin
  Result := AddScale(TdxGaugeContainerScale) as TdxGaugeContainerScale;
end;

function TdxCustomGaugeControl.AddScale(AScaleClass: TdxGaugeCustomScaleClass): TdxGaugeCustomScale;
begin
  Result := Scales.Add(AScaleClass);
  if IsDesigning then
    Controller.Select(Result, [ssLeft]);
end;

procedure TdxCustomGaugeControl.DeleteScale(AScaleIndex: Integer);
begin
  Scales.Delete(AScaleIndex);
end;

procedure TdxCustomGaugeControl.Clear;
begin
  BeginUpdate;
  Scales.Clear;
  EndUpdate;
end;

procedure TdxCustomGaugeControl.BeginUpdate;
begin
  Inc(FLockCount);
end;

procedure TdxCustomGaugeControl.CancelUpdate;
begin
  Dec(FLockCount);
end;

procedure TdxCustomGaugeControl.EndUpdate;
begin
  Dec(FLockCount);
  if FLockCount = 0 then
    Changed;
end;

function TdxCustomGaugeControl.GetDesignHitTest(X, Y: Integer; Shift: TShiftState): Boolean;
begin
  Result := inherited GetDesignHitTest(X, Y, Shift);
  if not Result and IsDesigning then
    Result := Controller.GetDesignHitTest(Point(X, Y), Shift);
end;

function TdxCustomGaugeControl.GetDragAndDropObjectClass: TcxDragAndDropObjectClass;
begin
  Result := Controller.GetDragAndDropObjectClass;
end;

function TdxCustomGaugeControl.GetIsDesigning: Boolean;
begin
  Result := inherited GetIsDesigning and (FController <> nil) and FController.IsSelectionHelperAvailable;
end;

function TdxCustomGaugeControl.HasBackground: Boolean;
begin
  Result := True;
end;

function TdxCustomGaugeControl.IsDoubleBufferedNeeded: Boolean;
begin
  Result := True;
end;

function TdxCustomGaugeControl.IsTransparentBackground: Boolean;
begin
  Result := Transparent;
end;

function TdxCustomGaugeControl.NeedRedrawOnResize: Boolean;
begin
  Result := True;
end;

procedure TdxCustomGaugeControl.DoPaint;
begin
  Painter.Paint(Canvas, DrawMode);
end;

procedure TdxCustomGaugeControl.Loaded;
begin
  inherited Loaded;
  Controller.PopulateZOrders;
end;

procedure TdxCustomGaugeControl.LookAndFeelChanged(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues);
begin
  DoInvalidate;
end;

procedure TdxCustomGaugeControl.Resize;
begin
  inherited Resize;
  Controller.ChangedLayers := [sclStaticLayer, sclDynamicLayer];
  Changed;
end;

procedure TdxCustomGaugeControl.ChangeScaleEx(M, D: Integer; isDpiChange: Boolean);
begin
  inherited;
  TdxGaugeScaleCollectionAccess(Scales).ScaleFactorChanged(M, D);
end;

function TdxCustomGaugeControl.StartDragAndDrop(const P: TPoint): Boolean;
begin
  Result := (Controller.HitTest.HitObject <> nil) and Supports(Controller.HitTest.HitObject, IdxGaugeSelectableElement)
    and (Controller.HitTest.HitObject is TdxGaugeCustomScale);
  if Result then
    if Controller.DesignDragOperation = sdoMove then
      TdxGaugeControlDragAndDropObject(DragAndDropObject).Init(P)
    else
      TdxGaugeControlDragAndDropObject(DragAndDropObject).Init(GetMouseCursorPos)
end;

procedure TdxCustomGaugeControl.TransparentChanged;
begin
  Changed;
end;

procedure TdxCustomGaugeControl.WndProc(var Message: TMessage);
begin
  if IsDesigning and (Message.Msg = WM_LBUTTONDBLCLK) then
    Controller.ShowScalesEditor;
  inherited WndProc(Message);
end;

procedure TdxCustomGaugeControl.DrawPreview(ACanvas: TcxCanvas; ADrawBackground: Boolean = True);
var
  AStoredChangedLayers: TdxGaugeScaleChangedLayers;
begin
  AStoredChangedLayers := Controller.ChangedLayers;
  Controller.ChangedLayers := [sclStaticLayer, sclDynamicLayer];
  FIsDrawingPreview := True;
  Painter.Paint(cxPaintCanvas, dmByLayer, ADrawBackground);
  FIsDrawingPreview := False;
  Controller.ChangedLayers := AStoredChangedLayers;
end;

procedure TdxCustomGaugeControl.SetDrawMode(const AValue: TdxGaugeDrawMode);
begin
  if FDrawMode <> AValue then
  begin
    FDrawMode := AValue;
    Controller.ChangedLayers := [sclStaticLayer, sclDynamicLayer];
    DoInvalidate;
  end;
end;

procedure TdxCustomGaugeControl.SetScales(const AValue: TdxGaugeScaleCollection);
begin
  FScales.Assign(AValue);
end;

procedure TdxCustomGaugeControl.AddScaleHandler(ASender: TObject);
begin
  Controller.AddZOrder(ASender as TdxGaugeCustomScale);
end;

procedure TdxCustomGaugeControl.DeleteScaleHandler(ASender: TObject);
begin
  Controller.RemoveZOrder(ASender as TdxGaugeCustomScale);
end;

procedure TdxCustomGaugeControl.ScaleChangeHandler(Sender: TObject; AItem: TcxComponentCollectionItem;
  AAction: TcxComponentCollectionNotification);
begin
  Changed;
end;

function TdxCustomGaugeControl.GetLocked: Boolean;
begin
  Result := FLockCount <> 0;
end;

function TdxCustomGaugeControl.GetLayerCount: Integer;
begin
  Result := Controller.LayerCount;
end;

function TdxCustomGaugeControl.IsShiftPressed: Boolean;
begin
  Result := GetAsyncKeyState(VK_SHIFT) < 0;
end;

procedure TdxCustomGaugeControl.Changed;
begin
  if FLockCount = 0 then
  begin
    Controller.Calculate;
    DoInvalidate;
  end;
end;

procedure TdxCustomGaugeControl.DoInvalidate;
begin
  InvalidateRect(ClientRect, False);
end;

procedure TdxCustomGaugeControl.WMSetCursor(var Message: TWMSetCursor);
var
  P: TPoint;
  ACursor: TCursor;
begin
  ACursor := crDefault;
  P := ScreenToClient(GetMouseCursorPos);
  if IsDesigning then
    ACursor := Controller.GetDesignCursor(P);
  if ACursor <> crDefault then
    SetCursor(Screen.Cursors[ACursor])
  else
    inherited;
end;

{ TdxGaugeControlHitTest }

constructor TdxGaugeControlHitTest.Create(AControl: TdxCustomGaugeControl);
begin
  inherited Create;
  FControl := AControl;
end;

function TdxGaugeControlHitTest.CanAnchoring(const P: TPoint): Boolean;
begin
  Result := PtInRect(TdxControlsDesignSelectorHelper.CalculateBounds(Control.ClientBounds, Control.ScaleFactor), P);
  if not Result then
  begin
    Calculate(P);
    Result := (HitObject <> nil) and PtInRect(TdxGaugeCustomScaleAccess(HitObject).GetSelectorRect, P);
  end;
end;

procedure TdxGaugeControlHitTest.Calculate(const P: TPoint);
begin
  FHitObject := GetHitObject(P, True);
  if FHitObject = nil then
    FHitObject := GetHitObject(P, False);
  FDragOperation := GetDesignDragOperation(P, FHitObject);
  FCursor := GetDesignCursor;
end;

function TdxGaugeControlHitTest.GetDesignCursor: TCursor;
begin
  case FDragOperation of
    sdoResizeLeft, sdoResizeRight:
      Result := crSizeWE;
    sdoResizeTop, sdoResizeBottom:
      Result := crSizeNS;
    sdoResizeTopRight, sdoResizeBottomLeft:
      Result := crSizeNESW;
    sdoResizeTopLeft, sdoResizeBottomRight:
      Result := crSizeNWSE;
    else
      Result := crDefault;
  end;
end;

function TdxGaugeControlHitTest.GetDesignDragOperation(const P: TPoint; AHitObject: TComponent): TdxGaugeScaleDragOperation;
var
  AIndex: Integer;
  AMarkers: TRects;
  AElement: IdxGaugeSelectableElement;
begin
  Result := sdoMove;
  if (AHitObject <> nil) and (Control.Controller.Selections.Count = 1) and (Control.Controller.Selections.IndexOf(AHitObject) <> -1) then
    if Supports(AHitObject, IdxGaugeSelectableElement, AElement) and not FControl.IsShiftPressed then
    begin
      AMarkers := AElement.GetSelectionMarkers;
      AIndex := GetDesignMarkerIndex(AMarkers, P);
      Result := sdoMove;
      if AElement.IsSizable then
        case AIndex of
          0:
            Result := sdoResizeTopLeft;
          1:
            Result := sdoResizeTop;
          2:
            Result := sdoResizeTopRight;
          3:
            Result := sdoResizeRight;
          4:
            Result := sdoResizeBottomRight;
          5:
            Result := sdoResizeBottom;
          6:
            Result := sdoResizeBottomLeft;
          7:
            Result := sdoResizeLeft;
        end;
    end;
end;

function TdxGaugeControlHitTest.GetDesignMarkerIndex(const AMarkers: TRects; const P: TPoint): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := Low(AMarkers) to High(AMarkers) do
    if PtInRect(AMarkers[I], P) then
    begin
      Result := I;
      Break;
    end;
end;

function TdxGaugeControlHitTest.GetHitObject(const P: TPoint; ACheckScaleSelectorRect: Boolean): TComponent;
var
  I, J: Integer;
  AFlag: Boolean;
  AScale: TdxGaugeCustomScale;
begin
  Result := nil;
  if not ACheckScaleSelectorRect and (Control.Controller.Selections.Count = 1) and
    IsHitObject(P, Control.Controller.Selections[0], False) and
    (GetDesignDragOperation(P, Control.Controller.Selections[0]) <> sdoMove)  then
    Result := Control.Controller.Selections[0]
  else
  begin
    AFlag := False;
    for I := Control.Controller.ZOrders.Count - 1 downto 0 do
      if not AFlag then
      begin
        AScale := TdxGaugeCustomScaleAccess(Control.Controller.ZOrders[I]);
        if IsHitObject(P, AScale, ACheckScaleSelectorRect) then
        begin
          Result := AScale;
          Break;
        end
        else
          for J := 0 to TdxGaugeCustomScaleAccess(AScale).Captions.Count - 1 do
          begin
            Result := TdxGaugeCustomCaptionAccess(TdxGaugeCustomScaleAccess(AScale).Captions[J]);
            if IsHitObject(P, Result, ACheckScaleSelectorRect) then
            begin
              AFlag := True;
              Break;
            end
            else
              Result := nil;
          end;
        if not AFlag and (AScale is TdxGaugeQuantitativeScale) then
          for J := 0 to TdxGaugeQuantitativeScaleAccess(AScale).Ranges.Count - 1 do
          begin
            Result := TdxGaugeCustomRangeAccess(TdxGaugeQuantitativeScaleAccess(AScale).Ranges[J]);
            if IsHitObject(P, Result, ACheckScaleSelectorRect) then
            begin
              AFlag := True;
              Break;
            end
            else
              Result := nil;
          end;
      end;
  end;
end;

function TdxGaugeControlHitTest.IsHitObject(const P: TPoint; AComponent: TComponent;
  ACheckScaleSelectorRect: Boolean): Boolean;

  function cxRectPtIn(const P: TPoint; const R: TdxRectF): Boolean; inline;
  begin
    Result := (P.X >= R.Left) and (P.X <= R.Right) and (P.Y >= R.Top) and (P.Y <= R.Bottom);
  end;


var
  R: TdxRectF;
  AElement: IdxGaugeSelectableElement;
begin
  Result := False;
  if Supports(AComponent, IdxGaugeSelectableElement, AElement) then
  begin
    if ACheckScaleSelectorRect then
      R := cxRectF(AElement.GetSelectorRect)
    else
      R := cxRectInflate(cxRectF(AElement.GetSelectionRect), dxGaugeSelectionMarkerSize / 2, dxGaugeSelectionMarkerSize / 2);
    Result := cxRectPtIn(P, R);
  end
end;

{   TdxCustomGaugeControlController }

constructor TdxCustomGaugeControlController.Create(AControl: TdxCustomGaugeControl);
begin
  inherited Create;
  FControl := AControl;
  FHitTest := TdxGaugeControlHitTest.Create(FControl);
  FZOrders := TList.Create;
  if csDesigning in Control.ComponentState then
  begin
    FSelectionHelper := dxGaugeControlSelectionHelperClass.Create(Control);
    dxGaugeControlSelectionHelpers.RegistrySelectionHelper(FSelectionHelper);
  end
  else
    FSelectionHelper := nil;
end;

destructor TdxCustomGaugeControlController.Destroy;
begin
  if csDesigning in Control.ComponentState then
    dxGaugeControlSelectionHelpers.UnregistrySelectionHelper(FSelectionHelper);
  FreeAndNil(FSelectionHelper);
  FreeAndNil(FZOrders);
  FreeAndNil(FHitTest);
  inherited Destroy;
end;

function TdxCustomGaugeControlController.CanAnchorScale(AScaleIndex, AAnchorScaleIndex: Integer): Boolean;

  function GetParentIndex(AIndex: Integer): Integer;
  begin
    if AIndex > -1 then
      Result := TdxGaugeCustomScaleAccess(Control.Scales[AIndex]).AnchorScaleIndex
    else
      Result := -1;
  end;

  function CheckIndexes(AIndex, AParentIndex: Integer): Boolean;
  begin
    Result := (AParentIndex = -1) or (AIndex <> AParentIndex) and (AParentIndex <> AScaleIndex) and
      CheckIndexes(AParentIndex, GetParentIndex(AParentIndex));
  end;

  function CheckAnchorScaleIndex: Boolean;
  begin
    Result := (AAnchorScaleIndex >= -1) and (AAnchorScaleIndex < Control.Scales.Count);
  end;

begin
  Result := CheckAnchorScaleIndex and ((((AAnchorScaleIndex = -1) and (AScaleIndex > -1)) or
    (AScaleIndex > -1) and (AScaleIndex <> AAnchorScaleIndex) and CheckIndexes(AAnchorScaleIndex,
    GetParentIndex(AAnchorScaleIndex))));
end;

function TdxCustomGaugeControlController.CreateAnchoredScaleList(AAnchorScaleList: TList): TList;
var
  I: Integer;
begin
  Result := TList.Create;
  for I := 0 to AAnchorScaleList.Count - 1 do
    PopulateAnchoredScales(Result, TdxGaugeCustomScale(AAnchorScaleList[I]));
end;

function TdxCustomGaugeControlController.IsScalePopupMenuShowing: Boolean;
begin
  Result := Control.IsDesigning and (FDesignPopupMenu <> nil);
end;

function TdxCustomGaugeControlController.GetDesignCursor(const P: TPoint): TCursor;
begin
  HitTest.Calculate(P);
  Result := HitTest.Cursor;
end;

function TdxCustomGaugeControlController.IsSelectionHelperAvailable: Boolean;
begin
  Result := FSelectionHelper <> nil;
end;

function TdxCustomGaugeControlController.IsScaleElement: Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Selections.Count - 1 do
  begin
    Result := ((TObject(Selections[I]) is TdxGaugeCustomRange) or (TObject(Selections[I]) is TdxGaugeCustomCaption));
    if Result then
      Break;
  end;
end;

function TdxCustomGaugeControlController.GetDesignHitTest(const P: TPoint; AShift: TShiftState): Boolean;
begin
  Result := PtInRect(TdxControlsDesignSelectorHelper.CalculateBounds(Control.ClientBounds, Control.ScaleFactor), P);
  if Result and (AShift * [ssRight, ssLeft] <> []) then
  begin
    TdxGaugeControlCustomSelectionHelperAccess(FSelectionHelper).SelectComponent(Control);
    Result := False;
  end
  else
  begin
    HitTest.Calculate(P);
    Result := HitTest.HitObject <> nil;
    if Result and Supports(HitTest.HitObject, IdxGaugeSelectableElement) then
    begin
      SetDragOperation(HitTest.DragOperation);
      FDesignCursor := HitTest.Cursor;
      if (ssLeft in AShift) and (Control.DragAndDropState = ddsNone) then
        Select(HitTest.HitObject, AShift)
      else
        Result := CheckDesignPopupMenu(AShift);
    end;
  end;
end;

function TdxCustomGaugeControlController.GetDragAndDropObjectClass: TcxDragAndDropObjectClass;
begin
  Result := TdxGaugeControlDragAndDropObject;
end;

procedure TdxCustomGaugeControlController.AddCaption(AScale: TdxGaugeCustomScale);
var
  AComponent: TComponent;
begin
  if AScale <> nil then
  begin
    AComponent := TdxGaugeCustomScaleAccess(AScale).Captions.Add;
    TdxGaugeControlCustomSelectionHelperAccess(FSelectionHelper).SelectComponent(AComponent);
  end;
end;

procedure TdxCustomGaugeControlController.AddRange(AScale: TdxGaugeCustomScale);
var
  AComponent: TComponent;
begin
  if AScale <> nil then
  begin
    AComponent := TdxGaugeQuantitativeScaleAccess(AScale).Ranges.Add;
    TdxGaugeControlCustomSelectionHelperAccess(FSelectionHelper).SelectComponent(AComponent);
  end;
end;

procedure TdxCustomGaugeControlController.CopyDragObject(const AMousePosDelta: TPoint);
var
  I: Integer;
  AScale, ABaseScale: TdxGaugeCustomScale;
  AScales, ASelection: TList;
begin
  Control.BeginUpdate;
  AScales := TList.Create;
  ASelection := TList.Create;
  try
    for I := 0 to Selections.Count - 1 do
      if TObject(Selections[I]) is TdxGaugeCustomScale then
        AScales.Add(Selections[I]);
    Selections.Clear;
    for I := 0 to AScales.Count - 1 do
    begin
      ABaseScale := TdxGaugeCustomScale(AScales[I]);
      AScale := Control.AddScale(TdxGaugeCustomScaleClass(ABaseScale.ClassType));
      AScale.Assign(ABaseScale);
      MoveDragObject(AScale, AMousePosDelta);
      ASelection.Add(AScale);
    end;
    Selections.Assign(ASelection);
    TdxGaugeControlCustomSelectionHelperAccess(FSelectionHelper).SetSelection;
  finally
    ASelection.Free;
    AScales.Free;
    Control.EndUpdate;
    Control.Modified;
  end;
end;

procedure TdxCustomGaugeControlController.GetScalesByDependences(AList: TList);

  procedure AddChild(AList: TList; AStack: TStack; AParent: TdxGaugeCustomScale);
  var
    I: Integer;
  begin
    for I := 0 to FControl.Scales.Count - 1 do
      if TdxGaugeCustomScaleAccess(FControl.Scales[I]).AnchorScale = AParent then
      begin
        AList.Insert(AList.Count, FControl.Scales[I]);
        AStack.Push(FControl.Scales[I]);
      end;
  end;

var
  AStack: TStack;
begin
  AList.Clear;
  AStack := TStack.Create;
  try
    AddChild(AList, AStack, nil);
    while AStack.Count > 0 do
      AddChild(AList, AStack, TdxGaugeCustomScale(AStack.Pop));
  finally
    AStack.Free;
  end;
end;

procedure TdxCustomGaugeControlController.LinkDragObject;
begin
  if HitTest.HitObject is TdxGaugeCustomScale then
    LinkDragObject(TdxGaugeCustomScaleAccess(Selections[0]), TdxGaugeCustomScale(HitTest.HitObject));
end;

procedure TdxCustomGaugeControlController.LinkDragObject(AScale, AAnchorScale: TdxGaugeCustomScale);
var
  AScaleAccess: TdxGaugeCustomScaleAccess;
begin
  AScaleAccess := TdxGaugeCustomScaleAccess(AScale);
  if AScaleAccess.AnchorScale <> AAnchorScale then
  begin
    Control.BeginUpdate;
    try
      AScaleAccess.AnchorScale := AAnchorScale;
      if AScaleAccess.CenterPositionType = sptFactor then
      begin
        AScaleAccess.CenterPositionFactorX := Max(Min(AScaleAccess.CenterPositionFactorX, 1), 0);
        AScaleAccess.CenterPositionFactorY := Max(Min(AScaleAccess.CenterPositionFactorY, 1), 0);
      end
      else
      begin
        AScaleAccess.CenterPositionX := Max(Min(AScaleAccess.CenterPositionX, Control.ClientBounds.Right),
          Control.ClientBounds.Left);
        AScaleAccess.CenterPositionY := Max(Min(AScaleAccess.CenterPositionY, Control.ClientBounds.Bottom),
          Control.ClientBounds.Top);
      end;
    finally
      Control.EndUpdate;
      Control.Modified;
    end;
  end;
end;

procedure TdxCustomGaugeControlController.PopulateDesignPopupMenu(APopupMenu: TPopupMenu);

  function HaveSelectedScale: Boolean;
  begin
    Result := Selections.Count > 0;
  end;

begin
  if not IsScaleElement then
    AddScalesMenuItems(APopupMenu);
  if HaveSelectedScale and not IsScaleElement then
    AddZOrdersMenuItems(APopupMenu);
  if HaveSelectedScale then
  begin
    AddDeleteScaleMenuItem(APopupMenu);
    AddRestoreScaleParametersMenuItem(APopupMenu);
  end;
end;

procedure TdxCustomGaugeControlController.Select(AScale: TComponent; AShift: TShiftState);
begin
  if ssLeft in AShift then
    TdxGaugeControlCustomSelectionHelperAccess(FSelectionHelper).Select(AScale, Control.IsShiftPressed);
end;

procedure TdxCustomGaugeControlController.DeleteSelection;
begin
  if Control.IsDesigning then
  begin
    Control.BeginUpdate;
    try
      TdxGaugeControlCustomSelectionHelperAccess(FSelectionHelper).DeleteSelections;
    finally
      Control.EndUpdate;
    end;
  end;
end;

procedure TdxCustomGaugeControlController.SetDragOperation(AOperation: TdxGaugeScaleDragOperation);
begin
  FDesignDragOperation := AOperation;
end;

procedure TdxCustomGaugeControlController.ShowScalesEditor;
begin
  if Control.IsDesigning then
    TdxGaugeControlCustomSelectionHelperAccess(FSelectionHelper).ShowScalesEditor;
end;

procedure TdxCustomGaugeControlController.AddZOrder(AScale: TdxGaugeCustomScale);
begin
  if CheckComponentState then
  begin
    if ZOrders.IndexOf(AScale) = -1 then
      ZOrders.Add(AScale);
    FChangedLayers := [sclStaticLayer, sclDynamicLayer];
  end;
end;

procedure TdxCustomGaugeControlController.ChangeZOrder(AScale: TdxGaugeCustomScale; AType: TdxGaugeScaleZOrderChangeType);
begin
  DoChangeZOrder(AScale, AType);
end;

procedure TdxCustomGaugeControlController.ChangeZOrder(AScales: TList; AType: TdxGaugeScaleZOrderChangeType);
var
  I: Integer;
begin
  Control.BeginUpdate;
  for I := 0 to AScales.Count - 1 do
    DoChangeZOrder(TdxGaugeCustomScale(AScales[I]), AType);
  Control.EndUpdate;
end;

procedure TdxCustomGaugeControlController.ChangeSelectionsZOrders(AType: TdxGaugeScaleZOrderChangeType);
begin
  ChangeZOrder(Selections, AType);
end;

function SortScalesByZOrder(AItem1, AItem2: Pointer): Integer;
begin
  Result := dxCompareValues(TdxGaugeCustomScaleAccess(AItem1).StoredZOrder, TdxGaugeCustomScaleAccess(AItem2).StoredZOrder);
end;

procedure TdxCustomGaugeControlController.PopulateZOrders;
var
  I: Integer;
  AScales: TList;
begin
  AScales := TList.Create;
  try
    for I := 0 to Control.Scales.Count - 1 do
      AScales.Add(Control.Scales[I]);
    AScales.Sort(SortScalesByZOrder);
    ZOrders.Clear;
    ZOrders.Assign(AScales);
  finally
    AScales.Free;
  end;
  Control.Changed;
end;

procedure TdxCustomGaugeControlController.RemoveZOrder(AScale: TdxGaugeCustomScale);
begin
  if CheckComponentState then
  begin
    if FSelectionHelper <> nil then
      Selections.Remove(AScale);
    UpdateAnchorReferences(AScale);
    ZOrders.Remove(AScale);
    FChangedLayers := [sclStaticLayer, sclDynamicLayer];
  end;
end;

function TdxCustomGaugeControlController.GetIsControlSelected: Boolean;
begin
  Result := (FSelectionHelper <> nil) and TdxGaugeControlCustomSelectionHelperAccess(FSelectionHelper).IsControlSelected;
end;

function TdxCustomGaugeControlController.GetSelections: TList;
begin
  Result := TdxGaugeControlCustomSelectionHelperAccess(FSelectionHelper).Selections;
end;

procedure TdxCustomGaugeControlController.PopulateAnchoredScales(AAnchoredScales: TList; AScale: TdxGaugeCustomScale);

  function CreateAnchoredScaleList(AScale: TdxGaugeCustomScale): TList;
  var
    I: Integer;
    AScaleAccess: TdxGaugeCustomScaleAccess;
  begin
    Result := TList.Create;
    for I := 0 to TdxCustomGaugeControl(Control).Scales.Count - 1 do
    begin
      AScaleAccess := TdxGaugeCustomScaleAccess(TdxCustomGaugeControl(Control).Scales[I]);
      if AScaleAccess.AnchorScale = AScale then
        Result.Add(AScaleAccess);
    end;
  end;

var
  ATempScale: TdxGaugeCustomScale;
  AScales: TList;
begin
  AScales := CreateAnchoredScaleList(AScale);
  try
    while AScales.Count > 0 do
    begin
      ATempScale := TdxGaugeCustomScaleAccess(AScales[0]);
      if AAnchoredScales.IndexOf(ATempScale) = -1 then
      begin
        AAnchoredScales.Add(ATempScale);
        PopulateAnchoredScales(AAnchoredScales, ATempScale);
      end;
      AScales.Remove(ATempScale);
    end;
  finally
    AScales.Free;
  end;
end;

function TdxCustomGaugeControlController.GetDragObjectSizeDelta(ADragObject: TdxGaugeCustomScale;
  const AMousePosDelta: TPoint): TdxPointF;
var
  AAnchorBounds: TdxRectF;
begin
  AAnchorBounds := GetScaleBounds(ADragObject);
  if TdxGaugeCustomScaleAccess(ADragObject).Width <> 0 then
    Result.X := AMousePosDelta.X * 2
  else
    Result.X := AMousePosDelta.X / AAnchorBounds.Width * 2;
  if TdxGaugeCustomScaleAccess(ADragObject).Height <> 0 then
    Result.Y := AMousePosDelta.Y * 2
  else
    Result.Y := AMousePosDelta.Y / AAnchorBounds.Height * 2;
end;

procedure TdxCustomGaugeControlController.MoveDragObject(ADragObject: TdxGaugeCustomScale; const AMousePosDelta: TPoint);

  procedure SetCenterPositionFactor(ADragObjectAccess: TdxGaugeCustomScaleAccess);

    function GetFactorDelta: TdxPointF;
    var
      AAnchorBounds: TdxRectF;
    begin
      AAnchorBounds := GetScaleBounds(ADragObjectAccess);
      Result := cxPointF(AMousePosDelta.X / AAnchorBounds.Width, AMousePosDelta.Y / AAnchorBounds.Height);
    end;

  begin
    ADragObjectAccess.CenterPositionFactorX := ADragObjectAccess.CenterPositionFactorX + GetFactorDelta.X;
    ADragObjectAccess.CenterPositionFactorY := ADragObjectAccess.CenterPositionFactorY + GetFactorDelta.Y;
  end;

  procedure SetCenterPosition(ADragObjectAccess: TdxGaugeCustomScaleAccess);
  begin
    ADragObjectAccess.CenterPositionX := ADragObjectAccess.CenterPositionX + AMousePosDelta.X;
    ADragObjectAccess.CenterPositionY := ADragObjectAccess.CenterPositionY + AMousePosDelta.Y;
  end;

begin
  if TdxGaugeCustomScaleAccess(ADragObject).CenterPositionType = sptFactor then
    SetCenterPositionFactor(TdxGaugeCustomScaleAccess(ADragObject))
  else
    SetCenterPosition(TdxGaugeCustomScaleAccess(ADragObject));
end;

procedure TdxCustomGaugeControlController.MoveDragObjects(const AMousePosDelta: TPoint);
var
  I: Integer;
  AScaleAccess: TdxGaugeCustomScaleAccess;
begin
  if DesignDragOperation = sdoMove then
  begin
    Control.BeginUpdate;
    try
      for I := 0 to Selections.Count - 1 do
        if TObject(Selections[I]) is TdxGaugeCustomScale then
        begin
          AScaleAccess := TdxGaugeCustomScaleAccess(Selections[I]);
          if (AScaleAccess.AnchorScaleIndex = -1) or (Selections.IndexOf(AScaleAccess.AnchorScale) = -1) then
            MoveDragObject(AScaleAccess, AMousePosDelta)
        end;
    finally
      Control.EndUpdate;
    end;
  end
end;

procedure TdxCustomGaugeControlController.SetDragObjectSize(ADragObject: TdxGaugeCustomScale; const AMousePosDelta: TPoint);
var
  AApplyMinWidth, AApplyMinHeight: Boolean;
  ASizeDelta: TdxPointF;
begin
  AApplyMinWidth := AMousePosDelta.X = MaxInt;
  AApplyMinHeight := AMousePosDelta.Y = MaxInt;
  ASizeDelta := GetDragObjectSizeDelta(ADragObject, AMousePosDelta);
  case DesignDragOperation of
    sdoResizeLeft:
      SetDragObjectWidth(ADragObject, -ASizeDelta.X, AApplyMinWidth);
    sdoResizeRight:
      SetDragObjectWidth(ADragObject, ASizeDelta.X, AApplyMinWidth);
    sdoResizeTop:
      SetDragObjectHeight(ADragObject, -ASizeDelta.Y, AApplyMinHeight);
    sdoResizeBottom:
      SetDragObjectHeight(ADragObject, ASizeDelta.Y, AApplyMinHeight);
    sdoResizeBottomRight:
      begin
        SetDragObjectWidth(ADragObject, ASizeDelta.X, AApplyMinWidth);
        SetDragObjectHeight(ADragObject, ASizeDelta.Y, AApplyMinHeight);
      end;
    sdoResizeTopLeft:
      begin
        SetDragObjectWidth(ADragObject, -ASizeDelta.X, AApplyMinWidth);
        SetDragObjectHeight(ADragObject, -ASizeDelta.Y, AApplyMinHeight);
      end;
    sdoResizeTopRight:
      begin
        SetDragObjectWidth(ADragObject, ASizeDelta.X, AApplyMinWidth);
        SetDragObjectHeight(ADragObject, -ASizeDelta.Y, AApplyMinHeight);
      end;
    sdoResizeBottomLeft:
      begin
        SetDragObjectWidth(ADragObject, -ASizeDelta.X, AApplyMinWidth);
        SetDragObjectHeight(ADragObject, ASizeDelta.Y, AApplyMinHeight);
      end;
  end;
end;

procedure TdxCustomGaugeControlController.SetDragObjectWidth(ADragObject: TdxGaugeCustomScale; ADelta: Single;
  AApplyMinSize: Boolean);
var
  ADragObjectAccess: TdxGaugeCustomScaleAccess;
begin
  ADragObjectAccess := TdxGaugeCustomScaleAccess(ADragObject);
  if ADragObjectAccess.Width = 0 then
    if AApplyMinSize then
      ADragObjectAccess.WidthFactor := 0
    else
      ADragObjectAccess.WidthFactor := ADragObjectAccess.WidthFactor + ADelta
  else
    if AApplyMinSize then
      ADragObjectAccess.Width := 1
    else
      ADragObjectAccess.Width := Max(ADragObjectAccess.Width + Round(ADelta), 2);
end;

procedure TdxCustomGaugeControlController.SetDragObjectHeight(ADragObject: TdxGaugeCustomScale; ADelta: Single;
  AApplyMinSize: Boolean);
var
  ADragObjectAccess: TdxGaugeCustomScaleAccess;
begin
  ADragObjectAccess := TdxGaugeCustomScaleAccess(ADragObject);
  if ADragObjectAccess.Height = 0 then
    if AApplyMinSize then
      ADragObjectAccess.HeightFactor := 0
    else
      ADragObjectAccess.HeightFactor := ADragObjectAccess.HeightFactor + ADelta
  else
    if AApplyMinSize then
      ADragObjectAccess.Height := 1
    else
      ADragObjectAccess.Height := Max(ADragObjectAccess.Height + Round(ADelta), 2);
end;

procedure TdxCustomGaugeControlController.BringBackward(AScale: TdxGaugeCustomScale);
begin
  SetZOrder(AScale, TdxGaugeCustomScaleAccess(AScale).ZOrder - 1);
end;

procedure TdxCustomGaugeControlController.BringForward(AScale: TdxGaugeCustomScale);
begin
  SetZOrder(AScale, TdxGaugeCustomScaleAccess(AScale).ZOrder + 1);
end;

procedure TdxCustomGaugeControlController.BringToFront(AScale: TdxGaugeCustomScale);
begin
  SetZOrder(AScale, Control.Scales.Count - 1);
end;

procedure TdxCustomGaugeControlController.DoChangeZOrder(AScale: TdxGaugeCustomScale; AType: TdxGaugeScaleZOrderChangeType);
begin
  case AType of
    octBringToFront:
      BringToFront(AScale);
    octSendToBack:
      SendToBack(AScale);
    octBringForward:
      BringForward(AScale);
    octSendBackward:
      BringBackward(AScale);
  end;
end;

procedure TdxCustomGaugeControlController.SendToBack(AScale: TdxGaugeCustomScale);
begin
  SetZOrder(AScale, 0);
end;

procedure TdxCustomGaugeControlController.SetZOrder(AScale: TdxGaugeCustomScale; const AValue: Integer);
var
  AZOrder: Integer;
begin
  AZOrder := ZOrders.IndexOf(AScale);
  if (AZOrder <> AValue) and not (csReading in Control.ComponentState) then
    ZOrders.Move(AZOrder, Max(Min(AValue, ZOrders.Count - 1), 0));
  FChangedLayers := [sclStaticLayer, sclDynamicLayer];
  Control.Invalidate;
end;

procedure TdxCustomGaugeControlController.UpdateAnchorReferences(AScale: TdxGaugeCustomScale);
var
  I: Integer;
begin
  if Control.ComponentState * [csDestroying] = [] then
  begin
    Control.Scales.BeginUpdate;
    for I := 0 to Control.Scales.Count - 1 do
      if TdxGaugeCustomScaleAccess(Control.Scales[I]).AnchorScale = AScale then
        TdxGaugeCustomScaleAccess(Control.Scales[I]).AnchorScale := TdxGaugeCustomScaleAccess(AScale).AnchorScale;
    Control.Scales.EndUpdate(False);
  end;
end;

function TdxCustomGaugeControlController.CheckDesignPopupMenu(AShift: TShiftState): Boolean;
begin
  Result := ssRight in AShift;
  if Result then
    ShowDesignPopupMenu(AShift);
end;

function TdxCustomGaugeControlController.CreatePopupMenuItem(const ACaption: string; ATag: TcxTag;
  AOnClick: TNotifyEvent): TMenuItem;
begin
  Result := TMenuItem.Create(nil);
  Result.Caption := ACaption;
  Result.Tag := ATag;
  Result.OnClick := AOnClick;
end;

procedure TdxCustomGaugeControlController.AddDeleteScaleMenuItem(APopupMenu: TPopupMenu);
begin
  APopupMenu.Items.Add(CreatePopupMenuItem(cLineCaption, -1, nil));
  APopupMenu.Items.Add(CreatePopupMenuItem('Delete', 1002, MenuItemOnClickHandler));
end;

procedure TdxCustomGaugeControlController.AddRestoreScaleParametersMenuItem(APopupMenu: TPopupMenu);
const
  sdxGaugeScaleRestoreStyleParameters = 'Restore Style Parameters';
begin
  if CanScaleElementEditors and not IsScaleElement then
  begin
    APopupMenu.Items.Add(NewLine);
    APopupMenu.Items.Add(CreatePopupMenuItem(sdxGaugeScaleRestoreStyleParameters, 1010, MenuItemOnClickHandler));
  end;
end;

procedure TdxCustomGaugeControlController.AddScalesMenuItems(APopupMenu: TPopupMenu);
var
  I: Integer;
  ANeedAddSeparator: Boolean;
  AScaleName: string;
  AScaleClasses: TList;
  AMenuItem: TMenuItem;
begin
  ANeedAddSeparator := True;
  APopupMenu.Items.Add(NewLine);
  AMenuItem := CreatePopupMenuItem('Add Scale', -1, nil);
  AScaleClasses := TList.Create;
  try
    dxGaugeGetRegisteredScaleClasses(AScaleClasses);
    if CanScaleElementEditors then
      APopupMenu.Items.Add(CreatePopupMenuItem('Add Caption', 4001, MenuItemOnClickHandler));
    for I := 0 to AScaleClasses.Count - 1 do
    begin
      AScaleName := TdxGaugeCustomScaleAccessClass(AScaleClasses[I]).GetScaleName;
      if TdxGaugeCustomScaleAccessClass(AScaleClasses[I]).GetScaleType = stContainerScale then
        APopupMenu.Items.Add(CreatePopupMenuItem('Add ' + AScaleName, 2001 + I, MenuItemOnClickHandler));
    end;
    for I := 0 to AScaleClasses.Count - 1 do
      if TdxGaugeCustomScaleAccessClass(AScaleClasses[I]).GetScaleType <> stContainerScale then
      begin
        AScaleName := TdxGaugeCustomScaleAccessClass(AScaleClasses[I]).GetScaleName;
        if ANeedAddSeparator and (Pos('DB ', AScaleName) = 1) then
        begin
          AMenuItem.Add(CreatePopupMenuItem(cLineCaption, -1, nil));
          ANeedAddSeparator := False;
        end;
        AMenuItem.Add(CreatePopupMenuItem(AScaleName, 2001 + I, MenuItemOnClickHandler));
      end;
  finally
    AScaleClasses.Free;
  end;
  if AMenuItem.Count > 0 then
    APopupMenu.Items.Add(AMenuItem)
  else
    AMenuItem.Free;
  if CanShowRangeEditor then
    APopupMenu.Items.Add(CreatePopupMenuItem('Add Range', 4002, MenuItemOnClickHandler));
end;

procedure TdxCustomGaugeControlController.AddZOrdersMenuItems(APopupMenu: TPopupMenu);
const
  dxGaugeScaleZOrderChangeTypeName: array[TdxGaugeScaleZOrderChangeType] of string =
    ('Bring to Front', 'Bring Forward', 'Send Backward', 'Send to Back');
var
  I: TdxGaugeScaleZOrderChangeType;
begin
  APopupMenu.Items.Add(NewLine);
  for I := Low(TdxGaugeScaleZOrderChangeType) to High(TdxGaugeScaleZOrderChangeType) do
    APopupMenu.Items.Add(CreatePopupMenuItem(dxGaugeScaleZOrderChangeTypeName[I], 101 + Integer(I),
      MenuItemOnClickHandler));
end;

procedure TdxCustomGaugeControlController.MenuItemOnClickHandler(ASender: TObject);

  procedure DoRestoreSelectionsParameters;
  var
    I: Integer;
  begin
    Control.BeginUpdate;
    for I := 0 to Selections.Count - 1 do
      TdxGaugeCustomScale(Selections[I]).RestoreStyleParameters;
    Control.EndUpdate;
    Control.Modified;
  end;

  function GetZOrderChangeType(AMenuItemTag: TcxTag): TdxGaugeScaleZOrderChangeType;
  begin
    Result := TdxGaugeScaleZOrderChangeType(AMenuItemTag);
  end;

var
  AItem: TMenuItem;
begin
  if Control.IsDesigning then
  begin
    AItem := ASender as TMenuItem;
    if InRange(AItem.Tag, 100, 1000) then
      ChangeSelectionsZOrders(GetZOrderChangeType(AItem.Tag - 101))
    else
      if InRange(AItem.Tag, 1000, 2000) then
        case AItem.Tag of
          1002:
            DeleteSelection;
          1010:
            DoRestoreSelectionsParameters
        end
      else
        if InRange(AItem.Tag, 2001, 3000) then
          AddScale(AItem.Tag - 2001)
        else
          if InRange(AItem.Tag, 4000, 5000) then
            case AItem.Tag of
              4001:
                AddCaption(HitTest.HitObject as TdxGaugeCustomScale);
              4002:
                AddRange(HitTest.HitObject as TdxGaugeCustomScale);
            end;
  end;
end;

procedure TdxCustomGaugeControlController.PopupDesignMenu(const APopupPoint: TPoint);
begin
  FDesignPopupMenu := TPopupMenu.Create(nil);
  try
    PopulateDesignPopupMenu(FDesignPopupMenu);
    FDesignPopupMenu.Popup(APopupPoint.X, APopupPoint.Y);
    Application.ProcessMessages;
  finally
    FreeAndNil(FDesignPopupMenu);
  end;
end;

function TdxCustomGaugeControlController.CanScaleElementEditors: Boolean;
begin
  Result := (Selections.Count = 1) and not (TObject(Selections[0]) is TdxGaugeContainerScale);
end;

function TdxCustomGaugeControlController.CanShowRangeEditor: Boolean;
begin
  Result := (TObject(Selections[0]) is TdxGaugeQuantitativeScale) and not (Selections.Count > 1);
end;

procedure TdxCustomGaugeControlController.ShowDesignPopupMenu(AShift: TShiftState);
begin
  if Selections.IndexOf(HitTest.HitObject) = -1 then
    Select(HitTest.HitObject, [ssLeft]);
  PopupDesignMenu(GetMouseCursorPos);
end;

function TdxCustomGaugeControlController.CanCalculate(const ABounds: TRect): Boolean;
begin
  Result := (cxRectWidth(ABounds) > 0) and (cxRectHeight(ABounds) > 0);
end;

function TdxCustomGaugeControlController.CheckComponentState: Boolean;
begin
  Result := FControl.ComponentState * [csLoading, csReading, csDestroying] = [];
end;

function TdxCustomGaugeControlController.GetSelectionContentRects: TRects;

  procedure AddRect(ARect: TRect);
  var
    L: Integer;
  begin
    L := Length(Result);
    SetLength(Result, L + 1);
    Result[L] := ARect;
  end;

  procedure PopulateRects(AScales: TList);
  var
    I: Integer;
    AElement: IdxGaugeSelectableElement;
  begin
    for I := 0 to AScales.Count - 1 do
      if Supports(TComponent(AScales[I]), IdxGaugeSelectableElement, AElement) then
        AddRect(AElement.GetSelectionContentRect);
  end;

var
  AAnchoredScales: TList;
begin
  SetLength(Result, 0);
  PopulateRects(Selections);
  AAnchoredScales := CreateAnchoredScaleList(Selections);
  try
    PopulateRects(AAnchoredScales);
  finally
    AAnchoredScales.Free;
  end;
end;

function TdxCustomGaugeControlController.GetSelectionRects(AList: TList): TRects;
var
  I: Integer;
  AElement: IdxGaugeSelectableElement;
begin
  SetLength(Result, AList.Count);
  for I := 0 to AList.Count - 1 do
    if Supports(TComponent(AList[I]), IdxGaugeSelectableElement, AElement) then
      Result[I] := AElement.GetSelectionRect;
end;

function TdxCustomGaugeControlController.GetScaleBounds(AScale: TdxGaugeCustomScale): TdxRectF;
var
  AAnchorScaleIndex: Integer;
begin
  AAnchorScaleIndex := TdxGaugeCustomScaleAccess(AScale).AnchorScaleIndex;
  if AAnchorScaleIndex = -1 then
    Result := cxRectF(FControl.ClientBounds)
  else
    Result := TdxGaugeCustomScaleAccess(FControl.Scales[AAnchorScaleIndex]).Bounds;
end;

procedure TdxCustomGaugeControlController.AddScale(AScaleClassIndex: Integer);
var
  AList: TList;
  AScale: TdxGaugeCustomScale;
begin
  AList := TList.Create;
  try
    dxGaugeGetRegisteredScaleClasses(AList);
    Control.BeginUpdate;
    AScale := Control.AddScale(TdxGaugeCustomScaleClass(AList[AScaleClassIndex]));
    TdxGaugeCustomScaleAccess(AScale).AnchorScale := HitTest.HitObject as TdxGaugeCustomScale;
    Control.EndUpdate;
    Control.Modified;
  finally
    AList.Free;
  end;
end;

procedure TdxCustomGaugeControlController.Calculate;
var
  AScales: TList;
begin
  if FControl.HandleAllocated and CheckComponentState and CanCalculate(FControl.ClientBounds) then
  begin
    AScales := TList.Create;
    try
      GetScalesByDependences(AScales);
      DoCalculateScales(AScales);
    finally
      AScales.Free;
    end;
  end;
end;

procedure TdxCustomGaugeControlController.DoCalculateScales(AScaleList: TList);

  procedure PopulateChangedLayers(AScaleChangedLayers: TdxGaugeScaleChangedLayers);
  var
    ALayer: TdxGaugeScaleChangedLayer;
  begin
    for ALayer in AScaleChangedLayers do
      Include(FChangedLayers, ALayer);
  end;

var
  I: Integer;
  AScale: TdxGaugeCustomScaleAccess;
begin
  FLayerCount := 0;
  for I := 0 to AScaleList.Count - 1 do
  begin
    AScale := TdxGaugeCustomScaleAccess(AScaleList[I]);
    AScale.Calculate(GetScaleBounds(AScale));
    FLayerCount := Max(FLayerCount, AScale.GetLayerCount);
    PopulateChangedLayers(AScale.ChangedLayers);
    AScale.ChangedLayers := [];
  end;
end;

{ TdxGaugeControlDragAndDropObject }

function TdxGaugeControlDragAndDropObject.GetDragAndDropCursor(Accepted: Boolean): TCursor;
begin
  Result := GetCurrentCursor;
end;

function TdxGaugeControlDragAndDropObject.GetImmediateStart: Boolean;
begin
  Result := True;
end;

function TdxGaugeControlDragAndDropObject.ProcessKeyDown(AKey: Word; AShiftState: TShiftState): Boolean;
begin
  Result := AKey <> VK_ESCAPE;
  UpdateCursor(AShiftState);
end;

function TdxGaugeControlDragAndDropObject.ProcessKeyUp(AKey: Word; AShiftState: TShiftState): Boolean;
begin
  Result := inherited ProcessKeyDown(AKey, AShiftState);
  UpdateCursor(AShiftState);
end;

procedure TdxGaugeControlDragAndDropObject.BeginDragAndDrop;
begin
  InitDragImages;
  inherited BeginDragAndDrop;
end;

procedure TdxGaugeControlDragAndDropObject.DragAndDrop(const P: TPoint; var Accepted: Boolean);
begin
  Accepted := PtInRect(Control.ClientBounds, P);
  if not (GetControl.Controller.DesignDragOperation in [sdoMove]) then
    ResizeDragObject(GetMouseCursorPos)
  else
    ShowDragImages(GetMouseCursorPos);
  inherited DragAndDrop(P, Accepted);
end;

procedure TdxGaugeControlDragAndDropObject.EndDragAndDrop(Accepted: Boolean);
var
  AIsModified: Boolean;
begin
  inherited EndDragAndDrop(Accepted);
  AIsModified := False;
  if Accepted then
  begin
    AIsModified := not cxPointIsEqual(GetClientCursorPos, FStartDragPoint);
    if FIsCopying then
      GetControl.Controller.CopyDragObject(GetMousePosDelta)
    else
      if AIsModified then
        MoveDragObjects(GetMousePosDelta);
  end;
  GetControl.Controller.SetDragOperation(sdoMove);
  FreeAndNil(FDragImages);
  if AIsModified then
    TdxCustomGaugeControlAccess(Control).Modified;
end;

procedure TdxGaugeControlDragAndDropObject.Init(const P: TPoint);
begin
  FStartDragPoint := P;
end;

function TdxGaugeControlDragAndDropObject.CreateDragImage(ADragScale: TdxGaugeCustomScaleAccess): TdxGaugeControlDragImage;
begin
  Result := TdxGaugeControlDragImage.Create;
  Result.SetBounds(0, 0, GetScaleSize(ADragScale).cx, GetScaleSize(ADragScale).cy);
  Result.OffsetPoint := cxPointOffset(ADragScale.GetSelectionRect.TopLeft, FStartDragPoint, False);
end;

function TdxGaugeControlDragAndDropObject.GetControl: TdxCustomGaugeControl;
begin
  Result := Control as TdxCustomGaugeControl;
end;

function TdxGaugeControlDragAndDropObject.GetMousePosDelta: TPoint;
begin
  Result.X := CurMousePos.X - FStartDragPoint.X;
  Result.Y := CurMousePos.Y - FStartDragPoint.Y;
end;

function TdxGaugeControlDragAndDropObject.GetScaleSize(ADragScale: TdxGaugeCustomScale): TSize;
begin
  Result := cxRectSize(TdxGaugeCustomScaleAccess(ADragScale).GetSelectionRect);
end;

function TdxGaugeControlDragAndDropObject.GetSizeDelta: TPoint;
begin
  Result.X := GetMouseCursorPos.X - FStartDragPoint.X;
  Result.Y := GetMouseCursorPos.Y - FStartDragPoint.Y;
end;

procedure TdxGaugeControlDragAndDropObject.DrawDragImage(ACanvas: TcxCanvas; const R: TRect; AScale: TdxGaugeCustomScale);
var
  AGPGraphics: TdxGPGraphics;
  AScaleAccess: TdxGaugeCustomScaleAccess;
begin
  AScaleAccess := TdxGaugeCustomScaleAccess(AScale);
  AGPGraphics := dxGpBeginPaint(ACanvas.Handle, R);
  try
    AGPGraphics.SmoothingMode := smAntiAlias;
    AGPGraphics.TranslateWorldTransform(-AScaleAccess.GetSelectionRect.Left, -AScaleAccess.GetSelectionRect.Top);
    GetControl.Painter.DrawScale(AGPGraphics, R, AScaleAccess.Index);
  finally
    dxGpEndPaint(AGPGraphics);
  end;
end;

procedure TdxGaugeControlDragAndDropObject.InitDragImages;
var
  I: Integer;
  ADragScaleList: TList;
  ATempList: TList;
begin
  FDragImages := TObjectList.Create(True);
  ADragScaleList := TList.Create;
  for I := 0 to GetControl.Controller.Selections.Count - 1 do
    if TObject(GetControl.Controller.Selections[I]) is TdxGaugeCustomScale then
      ADragScaleList.Add(GetControl.Controller.Selections[I]);
  ATempList := GetControl.Controller.CreateAnchoredScaleList(ADragScaleList);
  try
    ADragScaleList.Assign(ATempList, laOr);
    PopulateDragImages(ADragScaleList);
  finally
    ATempList.Free;
    ADragScaleList.Free;
  end;
end;

function CompareByZOrder(AItem1, AItem2: Pointer): Integer;
var
  AScale1, AScale2: TdxGaugeCustomScaleAccess;
begin
  AScale1 := TdxGaugeCustomScaleAccess(AItem1);
  AScale2 := TdxGaugeCustomScaleAccess(AItem2);
  Result := dxCompareValues(AScale1.ZOrder, AScale2.ZOrder);
end;

procedure TdxGaugeControlDragAndDropObject.PopulateDragImages(AScaleList: TList);
var
  I: Integer;
  ADragImage: TdxGaugeControlDragImage;
begin
  if GetControl.Controller.DesignDragOperation = sdoMove then
  begin
    AScaleList.Sort(CompareByZOrder);
    for I := 0 to AScaleList.Count - 1 do
    begin
      ADragImage := CreateDragImage(TdxGaugeCustomScaleAccess(AScaleList[I]));
      DrawDragImage(ADragImage.Canvas, ADragImage.ClientRect, TdxGaugeCustomScaleAccess(AScaleList[I]));
      FDragImages.Add(ADragImage);
    end;
  end;
end;

procedure TdxGaugeControlDragAndDropObject.ShowDragImages(const ACursorPos: TPoint);
var
  I: Integer;
begin
  if FDragImages <> nil then
    for I := 0 to FDragImages.Count - 1 do
    begin
      TdxGaugeControlDragImage(FDragImages[I]).MoveTo(cxPointOffset(ACursorPos,
        TdxGaugeControlDragImage(FDragImages[I]).OffsetPoint));
      TdxGaugeControlDragImage(FDragImages[I]).Visible := True;
    end;
end;

function TdxGaugeControlDragAndDropObject.CanChangeHeight(ADragObject: TdxGaugeCustomScale;
  AOperation: TdxGaugeScaleDragOperation; out ADelta: Integer): Boolean;
var
  ACenter: TdxPointF;
  AObjectAccess: TdxGaugeCustomScaleAccess;
begin
  AObjectAccess := TdxGaugeCustomScaleAccess(ADragObject);
  ACenter := cxRectCenter(AObjectAccess.Bounds);
  if AObjectAccess.Height = 0 then
    Result := AObjectAccess.Bounds.Height >= dxGaugeScaleMinScaleFactor.Y
  else
    Result := AObjectAccess.Bounds.Height >= 1;
  if Result and ((AOperation = sdoResizeBottom) and (GetClientCursorPos.Y >= ACenter.Y) or
    (AOperation = sdoResizeTop) and (GetClientCursorPos.Y <= ACenter.Y)) then
    ADelta := GetSizeDelta.Y
  else
    ADelta := MaxInt;
end;

function TdxGaugeControlDragAndDropObject.CanChangeWidth(ADragObject: TdxGaugeCustomScale;
  AOperation: TdxGaugeScaleDragOperation; out ADelta: Integer): Boolean;
var
  ACenter: TdxPointF;
  AObjectAccess: TdxGaugeCustomScaleAccess;
begin
  AObjectAccess := TdxGaugeCustomScaleAccess(ADragObject);
  ACenter := cxRectCenter(AObjectAccess.Bounds);
  if AObjectAccess.Width = 0 then
    Result := AObjectAccess.Bounds.Width >= dxGaugeScaleMinScaleFactor.X
  else
    Result := AObjectAccess.Bounds.Width >= 1;
  if Result and ((GetClientCursorPos.X >= ACenter.X) and (AOperation = sdoResizeRight) or
    (GetClientCursorPos.X <= ACenter.X) and (AOperation = sdoResizeLeft)) then
    ADelta := GetSizeDelta.X
  else
    ADelta := MaxInt;
end;

function TdxGaugeControlDragAndDropObject.GetCurrentCursor: TCursor;
begin
  if FIsCopying then
    Result := crDragCopy
  else
    Result := crDefault;
end;

procedure TdxGaugeControlDragAndDropObject.MoveDragObjects(const AMousePosDelta: TPoint);
begin
  GetControl.Controller.MoveDragObjects(AMousePosDelta);
end;

procedure TdxGaugeControlDragAndDropObject.ResizeDragObject(const ACursorPos: TPoint);
var
  ADragObject: TdxGaugeCustomScaleAccess;
  ADelta: TPoint;
  ACanResizeX, ACanResizeY: Boolean;
begin
  if not cxPointIsEqual(PrevMousePos, CurMousePos) then
  begin
    ADragObject := TdxGaugeCustomScaleAccess(GetControl.Controller.Selections[0]);
    ACanResizeX := True;
    ACanResizeY := True;
    case GetControl.Controller.DesignDragOperation of
      sdoResizeTop, sdoResizeBottom:
        ACanResizeX := CanChangeHeight(ADragObject, GetControl.Controller.DesignDragOperation, ADelta.Y);
      sdoResizeLeft, sdoResizeRight:
        ACanResizeX := CanChangeWidth(ADragObject, GetControl.Controller.DesignDragOperation, ADelta.X);
      sdoResizeTopLeft:
        begin
          ACanResizeX := CanChangeWidth(ADragObject, sdoResizeLeft, ADelta.X);
          ACanResizeY := CanChangeHeight(ADragObject, sdoResizeTop, ADelta.Y);
        end;
      sdoResizeTopRight:
        begin
          ACanResizeX := CanChangeWidth(ADragObject, sdoResizeRight, ADelta.X);
          ACanResizeY := CanChangeHeight(ADragObject, sdoResizeTop, ADelta.Y);
        end;
      sdoResizeBottomLeft:
        begin
          ACanResizeX := CanChangeWidth(ADragObject, sdoResizeLeft, ADelta.X);
          ACanResizeY := CanChangeHeight(ADragObject, sdoResizeBottom, ADelta.Y);
        end;
      sdoResizeBottomRight:
        begin
          ACanResizeX := CanChangeWidth(ADragObject, sdoResizeRight, ADelta.X);
          ACanResizeY := CanChangeHeight(ADragObject, sdoResizeBottom, ADelta.Y);
        end;
    end;
    GetControl.Controller.SetDragObjectSize(ADragObject, ADelta);
    if ACanResizeX then
      FStartDragPoint.X := ACursorPos.X;
    if ACanResizeY then
      FStartDragPoint.Y := ACursorPos.Y;
  end;
end;

procedure TdxGaugeControlDragAndDropObject.UpdateCursor(AShiftState: TShiftState);
begin
  FIsCopying := (ssCtrl in AShiftState) and (GetControl.Controller.DesignDragOperation = sdoMove) and
    not cxPointIsEqual(GetClientCursorPos, FStartDragPoint);
  Screen.Cursor := GetCurrentCursor;
end;

{ TdxCustomGaugeControlPainter }

constructor TdxCustomGaugeControlPainter.Create(AControl: TdxCustomGaugeControl);
begin
  inherited Create;
  FControl := AControl;

  FStaticElementsLayer := TcxBitmap32.CreateSize(FControl.Width, FControl.Height, True);
  SetLength(FScaleStaticLayerIndexes, 1);
  FScaleStaticLayerIndexes[0] := 0;

  FDynamicElementsLayer := TcxBitmap32.CreateSize(FControl.Width, FControl.Height, True);
  SetLength(FScaleDynamicLayerIndexes, 3);
  FScaleDynamicLayerIndexes[0] := 1;
  FScaleDynamicLayerIndexes[1] := 2;
  FScaleDynamicLayerIndexes[2] := 3;
end;

destructor TdxCustomGaugeControlPainter.Destroy;
begin
  SetLength(FScaleDynamicLayerIndexes, 0);
  FreeAndNil(FDynamicElementsLayer);
  SetLength(FScaleStaticLayerIndexes, 0);
  FreeAndNil(FStaticElementsLayer);
  inherited Destroy;
end;

procedure TdxCustomGaugeControlPainter.DrawScale(AGPGraphics: TdxGPGraphics; const R: TRect; AScaleIndex: Integer);
var
  I: Integer;
  AScaleAccess: TdxGaugeCustomScaleAccess;
begin
  AScaleAccess := TdxGaugeCustomScaleAccess(FControl.Scales[AScaleIndex]);
  for I := 0 to AScaleAccess.GetLayerCount - 1 do
    AScaleAccess.DrawLayer(AGPGraphics, I);
end;

procedure TdxCustomGaugeControlPainter.Paint(ACanvas: TcxCanvas; ADrawMode: TdxGaugeDrawMode);
begin
  Paint(ACanvas, ADrawMode, True);
end;

procedure TdxCustomGaugeControlPainter.Paint(ACanvas: TcxCanvas; ADrawMode: TdxGaugeDrawMode;
  ADrawBackground: Boolean);
var
  AGPGraphics: TdxGPGraphics;
begin
  if not FControl.Locked then
  begin
    if ADrawBackground then
      DrawBackground(ACanvas);
    AGPGraphics := dxGpBeginPaint(ACanvas.Handle, FControl.ClientBounds);
    try
      AGPGraphics.SmoothingMode := smAntiAlias;
      DrawContentRects(AGPGraphics);
      DrawScales(AGPGraphics, ADrawMode);
      DrawSelections(AGPGraphics);
    finally
      dxGpEndPaint(AGPGraphics);
    end;
    if NeedDrawSelector then
    begin
      DrawControlSelector(ACanvas);
      DrawSelectors(ACanvas);
    end;
    if ADrawBackground then
      DrawBorders(ACanvas);
  end;
end;

function TdxCustomGaugeControlPainter.NeedDrawSelector: Boolean;
begin
  Result := FControl.IsDesigning and not FControl.IsDrawingPreview;
end;

procedure TdxCustomGaugeControlPainter.DrawBackground(ACanvas: TcxCanvas);
begin
  if FControl.IsDrawingPreview and FControl.Transparent then
      cxDrawTransparentControlBackground(FControl, ACanvas, FControl.Bounds, True)
  else
    FControl.LookAndFeelPainter.DrawGaugeControlBackground(ACanvas, FControl.Bounds, FControl.Transparent,
      FControl.Color);
end;

procedure TdxCustomGaugeControlPainter.DrawBorders(ACanvas: TcxCanvas);
begin
  if FControl.BorderStyle = cxcbsDefault then
    FControl.LookAndFeelPainter.DrawBorder(ACanvas, FControl.Bounds);
end;

procedure TdxCustomGaugeControlPainter.DrawBitmaps(AGPGraphics: TdxGPGraphics);
begin
  AGPGraphics.DrawBitmap(FStaticElementsLayer, FControl.ClientRect);
  AGPGraphics.DrawBitmap(FDynamicElementsLayer, FControl.ClientRect);
end;

procedure TdxCustomGaugeControlPainter.DrawContentRects(AGPGraphics: TdxGPGraphics);
var
  ARects: TRects;
begin
  if FControl.IsDesigning then
  begin
    ARects := FControl.Controller.GetSelectionContentRects;
    dxGaugeDrawContentRects(AGPGraphics, ARects);
    SetLength(ARects, 0);
  end;
end;

procedure TdxCustomGaugeControlPainter.DrawControlSelector(ACanvas: TcxCanvas);
begin
  cxDrawDesignRect(ACanvas,
    TdxControlsDesignSelectorHelper.CalculateBounds(FControl.ClientBounds, FControl.ScaleFactor),
    FControl.Controller.IsControlSelected);
end;

procedure TdxCustomGaugeControlPainter.DrawGaugeLayer(ABitmap: TcxBitmap32; const AScaleLayerIndexes: array of Integer);
var
  AGPGraphics: TdxGPGraphics;
begin
  AGPGraphics := dxGpBeginPaint(ABitmap.Canvas.Handle, Rect(0, 0, ABitmap.Width, ABitmap.Height));
  try
    AGPGraphics.SmoothingMode := smAntiAlias;
    DrawLayer(AGPGraphics, AScaleLayerIndexes);
  finally
    dxGpEndPaint(AGPGraphics);
  end;
end;

procedure TdxCustomGaugeControlPainter.DrawLayer(AGPGraphics: TdxGPGraphics; const AScaleLayerIndexes: array of Integer);
var
  ALayerIndex, AScaleIndex: Integer;
begin
  for ALayerIndex := Low(AScaleLayerIndexes) to High(AScaleLayerIndexes) do
    for AScaleIndex := 0 to FControl.Controller.ZOrders.Count - 1 do
      TdxGaugeCustomScaleAccess(FControl.Controller.ZOrders[AScaleIndex]).DrawLayer(AGPGraphics,AScaleLayerIndexes[ALayerIndex]);
end;

procedure TdxCustomGaugeControlPainter.DrawScales(AGPGraphics: TdxGPGraphics; ADrawMode: TdxGaugeDrawMode);

  function GetScaleIndex(AZOrder: Integer): Integer;
  begin
    Result := TdxGaugeCustomScale(FControl.Controller.ZOrders[AZOrder]).Index;
  end;

var
  I: Integer;
begin
  case ADrawMode of
    dmByLayer:
      begin
        PrepareBitmaps;
        DrawBitmaps(AGPGraphics);
      end;
    dmByScale:
      begin
        FDynamicElementsLayer.SetSize(0, 0);
        FStaticElementsLayer.SetSize(0, 0);
        for I := 0 to FControl.Controller.ZOrders.Count - 1 do
          DrawScale(AGPGraphics, FControl.ClientBounds, GetScaleIndex(I));
        FControl.Controller.ChangedLayers := [];
      end;
  end;
end;

procedure TdxCustomGaugeControlPainter.DrawSelectors(ACanvas: TcxCanvas);

  function IsSelected(AComponent: TComponent): Boolean;
  begin
    Result := FControl.Controller.Selections.IndexOf(AComponent) <> -1;
  end;

var
  I, J: Integer;
  ACaptionAccess: TdxGaugeCustomCaptionAccess;
  ARangeAccess: TdxGaugeCustomRangeAccess;
  AScale: TdxGaugeCustomScale;
  AScaleAccess: TdxGaugeCustomScaleAccess;
begin
  for I := 0 to FControl.Controller.ZOrders.Count - 1 do
  begin
    AScale := (FControl.Controller.ZOrders[I]);
    AScaleAccess := TdxGaugeCustomScaleAccess(AScale);
    cxDrawDesignRect(ACanvas, AScaleAccess.GetSelectorRect, IsSelected(AScale));
    for J := 0 to AScaleAccess.Captions.Count - 1 do
    begin
      ACaptionAccess := TdxGaugeCustomCaptionAccess(AScaleAccess.Captions[J]);
      cxDrawDesignRect(ACanvas, ACaptionAccess.GetSelectorRect, IsSelected(ACaptionAccess));
    end;
    if AScale is TdxGaugeQuantitativeScale then
      for J := 0 to TdxGaugeQuantitativeScaleAccess(AScale).Ranges.Count - 1 do
      begin
        ARangeAccess := TdxGaugeCustomRangeAccess(TdxGaugeQuantitativeScaleAccess(AScale).Ranges[J]);
        cxDrawDesignRect(ACanvas, ARangeAccess.GetSelectorRect, IsSelected(ARangeAccess));
      end;
  end;
end;

procedure TdxCustomGaugeControlPainter.DrawSelections(AGPGraphics: TdxGPGraphics);

  procedure InternalDrawMarkers(AGPGraphics: TdxGPGraphics);
  var
    ARects: TRects;
    AIsSizable: Boolean;
    I: Integer;
    AElement: IdxGaugeSelectableElement;
  begin
    SetLength(ARects, 0);
    for I := 0 to FControl.Controller.Selections.Count - 1 do
      if Supports(TComponent(FControl.Controller.Selections[I]), IdxGaugeSelectableElement, AElement) then
      begin
        ARects := AElement.GetSelectionMarkers;
        AIsSizable := AElement.IsSizable;
        dxGaugeDrawMarkers(AGPGraphics, ARects, AIsSizable);
        SetLength(ARects, 0);
      end;
  end;

  procedure InternalDrawSelectionRects(AGPGraphics: TdxGPGraphics);
  var
    ARects: TRects;
    AScales: TList;
  begin
    AScales := FControl.Controller.CreateAnchoredScaleList(FControl.Controller.Selections);
    try
      ARects := FControl.Controller.GetSelectionRects(AScales);
      dxGaugeDrawRectangles(AGPGraphics, ARects);
      SetLength(ARects, 0);
    finally
      AScales.Free;
    end;
    ARects := FControl.Controller.GetSelectionRects(FControl.Controller.Selections);
    dxGaugeDrawSelections(AGPGraphics, ARects);
    SetLength(ARects, 0);
  end;

begin
  if FControl.IsDesigning then
  begin
    InternalDrawSelectionRects(AGPGraphics);
    InternalDrawMarkers(AGPGraphics);
  end;
end;

procedure TdxCustomGaugeControlPainter.PrepareBitmap(ABitmap: TcxBitmap32);
begin
  ABitmap.BeginUpdate;
  ABitmap.SetSize(FControl.Width, FControl.Height);
  ABitmap.Clear;
  ABitmap.EndUpdate(True);
end;

procedure TdxCustomGaugeControlPainter.PrepareBitmaps;

  procedure ExcludeChangeLayer(AChangedLayer: TdxGaugeScaleChangedLayer);
  begin
    FControl.Controller.ChangedLayers := FControl.Controller.ChangedLayers - [AChangedLayer];
  end;

  procedure PrepareLayerBitmap(AChangedLayer: TdxGaugeScaleChangedLayer);
  var
    ABitmap: TcxBitmap32;
    AScaleLayerIndexes: TIntegerDynArray;
  begin
    if AChangedLayer in FControl.Controller.ChangedLayers then
    begin
      ABitmap := nil;
      case AChangedLayer of
        sclStaticLayer:
          begin
            ABitmap := FStaticElementsLayer;
            AScaleLayerIndexes := FScaleStaticLayerIndexes;
            ExcludeChangeLayer(sclStaticLayer);
          end;
        sclDynamicLayer:
          begin
            ABitmap := FDynamicElementsLayer;
            AScaleLayerIndexes := FScaleDynamicLayerIndexes;
            ExcludeChangeLayer(sclDynamicLayer);
          end;
      end;
      PrepareBitmap(ABitmap);
      DrawGaugeLayer(ABitmap, AScaleLayerIndexes);
    end;
  end;

begin
  PrepareLayerBitmap(sclStaticLayer);
  PrepareLayerBitmap(sclDynamicLayer);
end;

initialization
  Classes.RegisterClass(TdxGaugeControl);

finalization
  Classes.UnRegisterClass(TdxGaugeControl);

end.









