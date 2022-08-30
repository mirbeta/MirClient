{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressRichEditControl                                   }
{                                                                    }
{           Copyright (c) 2000-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSRICHEDITCONTROL AND ALL        }
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

unit dxRichEdit.Ruler.Mouse;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, SysUtils, ActiveX, Windows, Messages, Generics.Defaults, Generics.Collections,
  Classes, Controls, SyncObjs, Graphics, dxCore, dxGDIPlusClasses,
  cxGraphics, dxCoreClasses, cxGeometry,

  dxRichEdit.Utils.Types,
  dxRichEdit.Utils.Graphics,
  dxRichEdit.Utils.DataObject,
  dxRichEdit.Utils.Mouse,
  dxRichEdit.Utils.Keyboard,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.Selection,
  dxRichEdit.Control,
  dxRichEdit.Control.HitTest,
  dxRichEdit.Control.HotZones,
  dxRichEdit.Control.Mouse,
  dxRichEdit.Control.Mouse.DragAndDrop,
  dxRichEdit.View.Core,
  dxRichEdit.View.ViewInfo,
  dxRichEdit.Platform.Win.Control,
  dxRichEdit.Platform.Win.Painter,
  dxRichEdit.Ruler;

type

  { TdxDefaultRulerMouseHandlerState }

  TdxDefaultRulerMouseHandlerState = class(TdxRichEditMouseCustomState)
  strict private
    function GetControl: TdxRulerControlBase;
    function GetController: TdxRulerMouseController;
  protected
    function GetStopClickTimerOnStart: Boolean; override;
    function CreateZone(const APoint: TPoint): TdxRulerHotZone; virtual;
    function GetRulerMovableElementIndex(const APoint: TPoint): TdxRulerHotZone;
    function GetHitActiveAreaStartPosition(const APoint: TPoint): Integer; virtual;

    property Control: TdxRulerControlBase read GetControl;
  public
    procedure HandleMouseDoubleClick(const Args: TdxMouseEventArgs); override;
    procedure HandleMouseDown(const Args: TdxMouseEventArgs); override;
    procedure HandleMouseMove(const Args: TdxMouseEventArgs); override;

    property Controller: TdxRulerMouseController read GetController;
  end;

  { TdxBeginDragHotZoneMouseDragHelperState }

  TdxBeginDragHotZoneMouseDragHelperState = class(TdxBeginMouseDragHelperState)
  strict private
    FHotZone: TdxRulerHotZone;
  public
    constructor Create(AController: TdxRichEditMouseController; ADragState: TdxRichEditMouseCustomState;
      const APoint: TPoint; AHotZone: TdxRulerHotZone); reintroduce;
    destructor Destroy; override;
    procedure HandleMouseUp(const E: TdxMouseEventArgs); override;
  end;

  { TdxDragAndDropMouseHandlerState }

  TdxDragAndDropMouseHandlerState = class(TdxRichEditMouseCustomVisualFeedbackState,
    IdxKeyboardHandlerService,
    IdxVisualFeedbackValueProvider<TdxRectangleVisualFeedbackValue>)
  strict private
    FHotZone: TdxRulerHotZone;
    FPageViewInfo: TdxPageViewInfo;
    FVisualFeedback: IdxVisualFeedback;
    FPoint: TPoint;
    function GetController: TdxRulerMouseController;
    function GetControl: TdxRulerControlBase;
  protected
    function GetStopClickTimerOnStart: Boolean; override;
    function GetVisualFeedbackValue: TdxRectangleVisualFeedbackValue; override;
    procedure MouseMoveCore(const APt: TPoint);
    procedure BeginVisualFeedback; virtual;
    procedure ShowVisualFeedback; virtual;
    procedure HideVisualFeedback; virtual;
    procedure EndVisualFeedback; virtual;

    property Control: TdxRulerControlBase read GetControl;
  public
    constructor Create(AController: TdxRichEditMouseController; AHotZone: TdxRulerHotZone;
      const AStartMousePosition: TPoint); reintroduce;
    destructor Destroy; override;

    procedure Cancel; virtual;
    procedure Commit; virtual;
    procedure Start; override;
    procedure Finish; override;
    procedure DoCancelMode; override;
    procedure HandleMouseMove(const E: TdxMouseEventArgs); override;
    procedure HandleMouseUp(const E: TdxMouseEventArgs); override;
    function CanKeyDownModifyEdit(const Args: TdxKeyEventArgs): Boolean;
    function HandleKeyDown(const E: TdxKeyEventArgs): Boolean;
    function HandleKeyPress(const E: TdxKeyPressEventArgs): Boolean;
    function HandleKeyUp(const E: TdxKeyEventArgs): Boolean;

    property Controller: TdxRulerMouseController read GetController;
  end;

  { TdxHorizontalRulerMouseHandler }

  TdxHorizontalRulerMouseHandler = class(TdxRulerMouseController)
  strict private
    function GetControl: TdxHorizontalRulerControl;
  protected
    function ConvertMouseEventArgs(const Args: TdxMouseEventArgs): TdxMouseEventArgs; override;
  public
    function CreateLineVisualFeedback(const AValueProvider: TdxRichEditMouseCustomVisualFeedbackState): IdxVisualFeedback; override;
    function GetHotZoneBounds(AHotZone: TdxRulerHotZone): TRect; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;

    property Control: TdxHorizontalRulerControl read GetControl;
  end;

  { TdxVerticalRulerMouseHandler }

  TdxVerticalRulerMouseHandler = class(TdxRulerMouseController)
  protected
    function ConvertMouseEventArgs(const Args: TdxMouseEventArgs): TdxMouseEventArgs; override;
    function CreateLogicalPoint(const AClientBounds: TRect; const APoint: TPoint): TPoint; virtual;
  public
    function CreateLineVisualFeedback(const AValueProvider: TdxRichEditMouseCustomVisualFeedbackState): IdxVisualFeedback; override;
  end;

  { TdxRichEditReversibleLineVisualFeedback }

  TdxRichEditReversibleLineVisualFeedback<T> = class abstract(TdxVisualFeedback<T>)
  strict private
    FControl: TdxCustomRichEditControl;
  protected
    procedure DrawReversibleLine; virtual; abstract;
  public
    constructor Create(AControl: TdxCustomRichEditControl; const AValueProvider: TdxRichEditMouseCustomVisualFeedbackState);
    procedure &Begin; override;
    procedure Show; override;
    procedure Hide; override;
    procedure &End; override;

    property Control: TdxCustomRichEditControl read FControl;
  end;

  { TdxRichEditHorizontalLineVisualFeedback }

  TdxRichEditHorizontalLineVisualFeedback = class(TdxRichEditReversibleLineVisualFeedback<TdxRectangleVisualFeedbackValue>)
  protected
    procedure DrawReversibleLine; override;
  end;

  { TdxRichEditVerticalLineVisualFeedback }

  TdxRichEditVerticalLineVisualFeedback = class(TdxRichEditReversibleLineVisualFeedback<TdxRectangleVisualFeedbackValue>)
  protected
    procedure DrawReversibleLine; override;
  end;

implementation

uses
  Contnrs, Math,
  dxTypeHelpers, cxDrawTextUtils,

  dxRichEdit.Utils.Cursors,
  dxRichEdit.DocumentModel.Borders,
  dxRichEdit.DocumentModel.ParagraphFormatting, dxCoreGraphics;

{ TdxDefaultRulerMouseHandlerState }

function TdxDefaultRulerMouseHandlerState.GetStopClickTimerOnStart: Boolean;
begin
  Result := False;
end;

function TdxDefaultRulerMouseHandlerState.GetControl: TdxRulerControlBase;
begin
  Result := Controller.Control;
end;

function TdxDefaultRulerMouseHandlerState.GetController: TdxRulerMouseController;
begin
  Result := TdxRulerMouseController(inherited Controller);
end;

procedure TdxDefaultRulerMouseHandlerState.HandleMouseDown(const Args: TdxMouseEventArgs);
var
  APoint: TPoint;
  AHotZone: TdxRulerHotZone;
begin
  if not Control.RichEditControl.InnerControl.IsEditable then
    Exit;

  Controller.CaptureMouse;
  APoint := Args.MousePos;
  AHotZone := GetRulerMovableElementIndex(APoint);
  if AHotZone = nil then
    AHotZone := CreateZone(APoint);
  if (AHotZone <> nil) and AHotZone.Enabled and AHotZone.CanEdit and AHotZone.CanActivate(Args) then
  begin
    AHotZone.Activate(Controller, APoint);
    Control.Repaint;
    if Args.Buttons <> [mbLeft] then
    begin
      AHotZone.Commit(APoint);
      Controller.SwitchToDefaultState;
    end;
  end
  else
    inherited HandleMouseDown(Args);

end;

procedure TdxDefaultRulerMouseHandlerState.HandleMouseDoubleClick(const Args: TdxMouseEventArgs);
var
  APoint: TPoint;
  AHotZone: TdxRulerHotZone;
begin
  if not Control.RichEditControl.InnerControl.IsEditable then
    Exit;

  APoint.Init(Args.MousePos.X, Args.MousePos.Y);
  AHotZone := GetRulerMovableElementIndex(APoint);
  if AHotZone = nil then
    AHotZone := CreateZone(APoint);
  if (AHotZone <> nil) and AHotZone.Enabled and AHotZone.CanEdit then
  begin
    AHotZone.OnMouseDoubleClick;
    Control.Reset;
  end;
  Controller.ReleaseMouseCapture;
end;

procedure TdxDefaultRulerMouseHandlerState.HandleMouseMove(const Args: TdxMouseEventArgs);
var
  APoint: TPoint;
  AHotZone: TdxRulerHotZone;
begin
  if not Control.RichEditControl.InnerControl.IsEditable then
  begin
    Controller.SetMouseCursor(TdxRichEditCursors.Default);
    Exit;
  end;
  APoint.Init(Args.MousePos.X, Args.MousePos.Y);
  AHotZone := GetRulerMovableElementIndex(APoint);
  if (AHotZone = nil) or not AHotZone.CanEdit then
    Controller.SetMouseCursor(TdxRichEditCursors.Default)
  else
    Controller.SetMouseCursor(AHotZone.Cursor);
end;

function TdxDefaultRulerMouseHandlerState.CreateZone(const APoint: TPoint): TdxRulerHotZone;
var
  ARulerControl: TdxHorizontalRulerControl;
  AHitActiveAreaStart, APosition: Integer;
  AHotZone: TdxRulerHotZone;
begin
  ARulerControl := Safe<TdxHorizontalRulerControl>.Cast(Control);
  if ARulerControl = nil then
    Exit(nil);

  AHitActiveAreaStart := GetHitActiveAreaStartPosition(APoint);
  if AHitActiveAreaStart >= 0 then
  begin
    APosition := Controller.GetSnappedPosition(Controller.GetPrimaryCoordinate(APoint));
    AHotZone := ARulerControl.ViewInfo.TabTypeToggleHotZone.HotZone.Clone;
    AHotZone.SetNewValue(Control.ViewInfo.GetRulerModelPosition(APosition - ARulerControl.ViewInfo.GetAdditionalCellIndent(True)));
    AHotZone.IsNew := True;
    Control.ViewInfo.HotZones.Add(AHotZone);
    Exit(AHotZone);
  end;
  Result := nil;
end;

function TdxDefaultRulerMouseHandlerState.GetRulerMovableElementIndex(const APoint: TPoint): TdxRulerHotZone;
var
  AHotZones: TdxReferencedObjectList<TdxRulerHotZone>;
  I: Integer;
  ABounds: TRect;
begin
  AHotZones := Control.ViewInfo.HotZones;
  for I := AHotZones.Count - 1 downto 0 do
  begin
    ABounds := Controller.GetHotZoneBounds(AHotZones[I]);
    if ABounds.Contains(APoint) then
      Exit(AHotZones[I]);
  end;
  Result := nil;
end;

function TdxDefaultRulerMouseHandlerState.GetHitActiveAreaStartPosition(const APoint: TPoint): Integer;
var
  AActiveAreaCollection: TList<TdxRectF>;
  ABounds: TdxRectF;
begin
  AActiveAreaCollection := Controller.Control.ViewInfo.ActiveAreaCollection;
  ABounds := AActiveAreaCollection[Control.ViewInfo.CurrentActiveAreaIndex];
  if TRect.Round(ABounds).Contains(APoint) then
    Exit(Trunc(Controller.GetNearPrimaryCoordinate(ABounds)));
  Result := -1;
end;

{ TdxBeginDragHotZoneMouseDragHelperState }

constructor TdxBeginDragHotZoneMouseDragHelperState.Create(AController: TdxRichEditMouseController;
  ADragState: TdxRichEditMouseCustomState; const APoint: TPoint; AHotZone: TdxRulerHotZone);
begin
  inherited Create(AController, ADragState, APoint);
  FHotZone := AHotZone;
  TdxRulerHotZone.AddReference(FHotZone);
end;

destructor TdxBeginDragHotZoneMouseDragHelperState.Destroy;
begin
  TdxRulerHotZone.Release(FHotZone);
  inherited Destroy;
end;

procedure TdxBeginDragHotZoneMouseDragHelperState.HandleMouseUp(const E: TdxMouseEventArgs);
var
  AControl: TdxRulerControlBase;
begin
  AControl := FHotZone.RulerControl;
  if FHotZone.IsNew then
    DragState.HandleMouseUp(E)
  else
    inherited HandleMouseUp(E);
  AControl.Reset;
  AControl.RichEditControl.Invalidate;
end;

{ TdxDragAndDropMouseHandlerState }

constructor TdxDragAndDropMouseHandlerState.Create(AController: TdxRichEditMouseController;
  AHotZone: TdxRulerHotZone; const AStartMousePosition: TPoint);
var
  AView: TdxRichEditView;
  ACaretPosition: TdxCaretPosition;
begin
  inherited Create(AController);
  FHotZone := AHotZone;
  TdxRulerHotZone.AddReference(FHotZone);

  FPoint := AStartMousePosition;
  FVisualFeedback := Controller.CreateLineVisualFeedback(Self);
  AView := Control.RichEditControl.InnerControl.ActiveView;
  ACaretPosition := AView.CaretPosition;
  if ACaretPosition.Update(TdxDocumentLayoutDetailsLevel.Column) then
    FPageViewInfo := ACaretPosition.PageViewInfo;
  TdxPageViewInfo.AddReference(FPageViewInfo);
end;

destructor TdxDragAndDropMouseHandlerState.Destroy;
begin
  TdxPageViewInfo.Release(FPageViewInfo);
  TdxRulerHotZone.Release(FHotZone);
  inherited Destroy;
end;

procedure TdxDragAndDropMouseHandlerState.Cancel;
begin
  HideVisualFeedback;
  Controller.ReleaseMouseCapture;
  Controller.SwitchToDefaultState;
  Control.Reset;
  Control.RichEditControl.Invalidate;
end;

procedure TdxDragAndDropMouseHandlerState.Commit;
begin
  HideVisualFeedback;
  FHotZone.Commit(FPoint);
  Controller.ReleaseMouseCapture;
  Controller.SwitchToDefaultState;
  Control.Repaint;
end;

function TdxDragAndDropMouseHandlerState.GetStopClickTimerOnStart: Boolean;
begin
  Result := True;
end;

function TdxDragAndDropMouseHandlerState.GetController: TdxRulerMouseController;
begin
  Result := TdxRulerMouseController(inherited Controller);
end;

function TdxDragAndDropMouseHandlerState.GetControl: TdxRulerControlBase;
begin
  Result := Controller.Control;
end;

procedure TdxDragAndDropMouseHandlerState.Start;
begin
  Controller.Control.AddKeyboardService(Self);
  if not FHotZone.IsNew then
    FHotZone.AddFakeHotZone;
  MouseMoveCore(FPoint);
  inherited Start;
  BeginVisualFeedback;
end;

procedure TdxDragAndDropMouseHandlerState.Finish;
begin
  EndVisualFeedback;
  Controller.Control.RemoveKeyboardService(Self);
  inherited Finish;
end;

procedure TdxDragAndDropMouseHandlerState.DoCancelMode;
begin
  Cancel;
end;

procedure TdxDragAndDropMouseHandlerState.HandleMouseMove(const E: TdxMouseEventArgs);
begin
  HideVisualFeedback;
  MouseMoveCore(E.MousePos);
  ShowVisualFeedback;
end;

procedure TdxDragAndDropMouseHandlerState.MouseMoveCore(const APt: TPoint);
var
  APrimary: Integer;
begin
  APrimary := Controller.GetSnappedPosition(Controller.GetPrimaryCoordinate(APt));
  FPoint := Controller.CreatePoint(APrimary, Controller.GetSecondaryCoordinate(APt));
  FHotZone.OnMove(FPoint);
  Control.Repaint;
end;

procedure TdxDragAndDropMouseHandlerState.HandleMouseUp(const E: TdxMouseEventArgs);
var
  APt: TPoint;
  APrimary: Integer;
begin
  APt := E.MousePos;
  APrimary := Controller.GetSnappedPosition(Controller.GetPrimaryCoordinate(APt));
  FPoint := Controller.CreatePoint(APrimary, Controller.GetSecondaryCoordinate(APt));
  Commit;
end;

function TdxDragAndDropMouseHandlerState.CanKeyDownModifyEdit(const Args: TdxKeyEventArgs): Boolean;
begin
  Result := Args.KeyData = VK_RETURN;
end;

function TdxDragAndDropMouseHandlerState.HandleKeyDown(const E: TdxKeyEventArgs): Boolean;
begin
  Result := True;
  if E.KeyData = VK_ESCAPE then
    Cancel;
  if E.KeyData = VK_RETURN then
    Commit;
end;

function TdxDragAndDropMouseHandlerState.HandleKeyPress(const E: TdxKeyPressEventArgs): Boolean;
begin
  Result := False;
end;

function TdxDragAndDropMouseHandlerState.HandleKeyUp(const E: TdxKeyEventArgs): Boolean;
begin
  Result := False;
end;

procedure TdxDragAndDropMouseHandlerState.BeginVisualFeedback;
begin
  FVisualFeedback.&Begin;
end;

procedure TdxDragAndDropMouseHandlerState.ShowVisualFeedback;
begin
  FVisualFeedback.Show;
end;

procedure TdxDragAndDropMouseHandlerState.HideVisualFeedback;
begin
  FVisualFeedback.Hide;
end;

procedure TdxDragAndDropMouseHandlerState.EndVisualFeedback;
begin
  FVisualFeedback.&End;
end;

function TdxDragAndDropMouseHandlerState.GetVisualFeedbackValue: TdxRectangleVisualFeedbackValue;
var
  AValue: Integer;
begin
  AValue := FHotZone.GetVisualFeedbackValue(FPoint, FPageViewInfo);
  Result := TdxRectangleVisualFeedbackValue.Create(FPageViewInfo, TRect.CreateSize(Controller.CreatePoint(AValue, 0), 0, 0));
end;

{ TdxHorizontalRulerMouseHandler }

function TdxHorizontalRulerMouseHandler.GetControl: TdxHorizontalRulerControl;
begin
  Result := TdxHorizontalRulerControl(inherited Control);
end;

function TdxHorizontalRulerMouseHandler.GetHotZoneBounds(AHotZone: TdxRulerHotZone): TRect;
begin
  Result := AHotZone.Bounds;
end;

procedure TdxHorizontalRulerMouseHandler.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  APoint: TPoint;
  ATabTypeToggleHotZone: TdxTabTypeToggleHotZone;
begin
  APoint.Init(X, Y);
  ATabTypeToggleHotZone := Control.ViewInfo.TabTypeToggleHotZone;
  if ATabTypeToggleHotZone.DisplayBounds.Contains(APoint) then
  begin
    ATabTypeToggleHotZone.Commit(APoint);
    Control.Repaint;
  end
  else
    inherited MouseDown(Button, Shift, X, Y);
end;

function TdxHorizontalRulerMouseHandler.ConvertMouseEventArgs(const Args: TdxMouseEventArgs): TdxMouseEventArgs;
var
  ALocation: TPoint;
begin
  Result := Args;
  ALocation := Control.GetPhysicalPoint(Args.MousePos);
  ALocation := Control.RichEditControl.ActiveView.CreateLogicalPoint(Control.ViewInfo.Bounds, ALocation);
  ALocation.Init(Trunc(ALocation.X * Control.ZoomFactor), Trunc(ALocation.Y * Control.ZoomFactor));
  Result.MousePos := ALocation;
end;

function TdxHorizontalRulerMouseHandler.CreateLineVisualFeedback(const AValueProvider: TdxRichEditMouseCustomVisualFeedbackState): IdxVisualFeedback;
begin
  Result := TdxRichEditVerticalLineVisualFeedback.Create(Control.RichEditControl, AValueProvider);
end;

{ TdxVerticalRulerMouseHandler }

function TdxVerticalRulerMouseHandler.ConvertMouseEventArgs(const Args: TdxMouseEventArgs): TdxMouseEventArgs;
var
  ALocation: TPoint;
begin
  Result := Args;
  ALocation := Control.GetPhysicalPoint(Args.MousePos);
  ALocation := CreateLogicalPoint(Control.ViewInfo.ClientBounds, ALocation);
  ALocation.Init(Trunc(ALocation.X), Trunc(ALocation.Y * Control.ZoomFactor));
  Result.MousePos := ALocation;
end;

function TdxVerticalRulerMouseHandler.CreateLogicalPoint(const AClientBounds: TRect; const APoint: TPoint): TPoint;
var
  Y: Integer;
  AMatrix: TdxGPMatrix;
begin
  Y := Control.RichEditControl.ActiveView.CaretPosition.PageViewInfo.ClientBounds.Y;
  AMatrix := TdxGPMatrix.Create;
  try
    AMatrix.Translate(0, Y);
    AMatrix.Scale(1, Control.ZoomFactor);
    AMatrix.Invert;
    Result := AMatrix.TransformPoint(APoint);
  finally
    AMatrix.Free;
  end;
end;

function TdxVerticalRulerMouseHandler.CreateLineVisualFeedback(const AValueProvider: TdxRichEditMouseCustomVisualFeedbackState): IdxVisualFeedback;
begin
  Result := TdxRichEditHorizontalLineVisualFeedback.Create(Control.RichEditControl, AValueProvider);
end;

{ TdxRichEditReversibleLineVisualFeedback }

constructor TdxRichEditReversibleLineVisualFeedback<T>.Create(AControl: TdxCustomRichEditControl;
  const AValueProvider: TdxRichEditMouseCustomVisualFeedbackState);
begin
  inherited Create(AValueProvider);
  Assert(AControl is TdxCustomRichEditControl);
  FControl := AControl;
end;

procedure TdxRichEditReversibleLineVisualFeedback<T>.&Begin;
begin
  DrawReversibleLine;
end;

procedure TdxRichEditReversibleLineVisualFeedback<T>.Show;
begin
  DrawReversibleLine;
end;

procedure TdxRichEditReversibleLineVisualFeedback<T>.Hide;
begin
  DrawReversibleLine;
end;

procedure TdxRichEditReversibleLineVisualFeedback<T>.&End;
begin
end;

{ TdxRichEditHorizontalLineVisualFeedback }

procedure TdxRichEditHorizontalLineVisualFeedback.DrawReversibleLine;
var
  AValue: TdxRectangleVisualFeedbackValue;
begin
  AValue := ValueProvider.VisualFeedbackValue;
  try
    if AValue.PageViewInfo <> nil then
      Control.Painter.DrawReversibleHorizontalLine(AValue.Bounds.Y, AValue.PageViewInfo);
  finally
    AValue.Free;
  end;
end;

{ TdxRichEditVerticalLineVisualFeedback }

procedure TdxRichEditVerticalLineVisualFeedback.DrawReversibleLine;
var
  AValue: TdxRectangleVisualFeedbackValue;
begin
  AValue := ValueProvider.VisualFeedbackValue;
  try
    if AValue.PageViewInfo <> nil then
      Control.Painter.DrawReversibleVerticalLine(AValue.Bounds.X, AValue.PageViewInfo);
  finally
    AValue.Free;
  end;
end;

end.
