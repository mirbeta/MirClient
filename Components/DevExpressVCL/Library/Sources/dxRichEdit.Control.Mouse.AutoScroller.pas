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

unit dxRichEdit.Control.Mouse.AutoScroller;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, SysUtils, Classes, Controls, Generics.Defaults, Generics.Collections, cxClasses, ActiveX, dxCoreClasses,

  dxGenerics,
  dxRichEdit.Options,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.PieceTableIterators,
  dxRichEdit.Utils.DataObject,
  dxRichEdit.Utils.Mouse,
  dxRichEdit.View.Core,
  dxRichEdit.Control.HitTest,
  dxRichEdit.Control.HotZones,
  dxRichEdit.Commands,
  dxRichEdit.Control.Keyboard,
  dxRichEdit.Utils.OfficeImage,
  dxRichEdit.View.ViewInfo,
  dxRichEdit.Commands.Selection;

type

  { TdxAutoScrollerHotZone }

  TdxAutoScrollerHotZone = class
  private
    FBounds: TRect;
  public
    function Initialize(const AMousePosition: TPoint): Boolean; virtual;

    function CanActivate(const AMousePosition: TPoint): Boolean; virtual; abstract;
		procedure PerformAutoScroll; virtual; abstract;
    function CalculateHotZoneBounds: TRect; virtual; abstract;
		function AdjustHotZoneBounds(const ABounds: TRect; const AMousePosition: TPoint): TRect; virtual; abstract;

    property Bounds: TRect read FBounds write FBounds;
  end;

  TdxAutoScrollerHotZoneCollection = class(TdxObjectList<TdxAutoScrollerHotZone>);

  { TdxAutoScroller }

  TdxAutoScroller = class
  public const
    AutoScrollTimerInterval = 100;
  private
    FController: TdxCustomMouseController;
    FIsActive: Boolean;
    FTimer: TcxTimer;
    FHotZones: TdxAutoScrollerHotZoneCollection;
    FActiveHotZone: TdxAutoScrollerHotZone;
    procedure TimerHandler(Sender: TObject);
  protected
    function CalculateActiveHotZone(const P: TPoint): TdxAutoScrollerHotZone;
    procedure PopulateHotZones; virtual;
    procedure StartTimer; virtual;
    procedure StopTimer; virtual;
  public
    constructor Create(AController: TdxCustomMouseController);
    destructor Destroy; override;

    procedure Activate(const AMousePosition: TPoint);
    procedure Deactivate;
    procedure HandleMouseMove(const P: TPoint);

    procedure Resume;
    procedure Suspend;

    procedure DoMouseMove(const P: TPoint);

    property Controller: TdxCustomMouseController read FController;
    property HotZones: TdxAutoScrollerHotZoneCollection read FHotZones;
    property IsActive: Boolean read FIsActive write FIsActive;
  end;

  { TdxRichEditAutoScrollerHotZone }

  TdxRichEditAutoScrollerHotZone = class abstract(TdxAutoScrollerHotZone)
  strict private
    FController: TdxCustomMouseController;
  private
    function GetControl: IdxRichEditControl;
  protected
    function CalculateCompatibleViewBounds: TRect; virtual;
    property Control: IdxRichEditControl read GetControl;
    property Controller: TdxCustomMouseController read FController;
  public
    constructor Create(AController: TdxCustomMouseController);
  end;

  { TdxRichEditVerticalScrollHotZone }

  TdxRichEditVerticalScrollHotZone = class abstract(TdxRichEditAutoScrollerHotZone)
  protected
    function CalculatePhysicalOffset(const Args: TdxMouseEventArgs): Integer; virtual; abstract;
    function GetHotZoneHeight: Integer;
  public
    procedure PerformAutoScroll; override;
  end;

  { TdxRichEditVerticalScrollForwardHotZone }

  TdxRichEditVerticalScrollForwardHotZone = class(TdxRichEditVerticalScrollHotZone)
  protected
    function CalculatePhysicalOffset(const Args: TdxMouseEventArgs): Integer; override;
  public
    function AdjustHotZoneBounds(const ABounds: TRect; const AMousePosition: TPoint): TRect; override;
    function CalculateHotZoneBounds: TRect; override;
    function CanActivate(const AMousePosition: TPoint): Boolean; override;
  end;

  { TdxRichEditVerticalScrollBackwardHotZone }

  TdxRichEditVerticalScrollBackwardHotZone = class(TdxRichEditVerticalScrollHotZone)
  protected
    function CalculatePhysicalOffset(const Args: TdxMouseEventArgs): Integer; override;
  public
    function AdjustHotZoneBounds(const ABounds: TRect; const AMousePosition: TPoint): TRect; override;
    function CalculateHotZoneBounds: TRect; override;
    function CanActivate(const AMousePosition: TPoint): Boolean; override;
  end;

  { TdxRichEditHorizontalScrollHotZone }

  TdxRichEditHorizontalScrollHotZone = class abstract(TdxRichEditAutoScrollerHotZone)
  protected
    function CalculatePhysicalOffset(const Args: TdxMouseEventArgs): Integer; virtual; abstract;
    function GetHotZoneWidth: Integer;
  public
    procedure PerformAutoScroll; override;
  end;

  { TdxRichEditHorizontalScrollForwardHotZone }

  TdxRichEditHorizontalScrollForwardHotZone = class(TdxRichEditHorizontalScrollHotZone)
  protected
    function CalculatePhysicalOffset(const Args: TdxMouseEventArgs): Integer; override;
  public
    function AdjustHotZoneBounds(const ABounds: TRect; const AMousePosition: TPoint): TRect; override;
    function CalculateHotZoneBounds: TRect; override;
    function CanActivate(const AMousePosition: TPoint): Boolean; override;
  end;

  TdxRichEditHorizontalScrollBackwardHotZone = class(TdxRichEditHorizontalScrollHotZone)
  protected
    function CalculatePhysicalOffset(const Args: TdxMouseEventArgs): Integer; override;
  public
    function AdjustHotZoneBounds(const ABounds: TRect; const AMousePosition: TPoint): TRect; override;
    function CalculateHotZoneBounds: TRect; override;
    function CanActivate(const AMousePosition: TPoint): Boolean; override;
  end;

implementation

uses
  Contnrs,
  dxCore, dxTypeHelpers,
  dxRichEdit.Utils.Graphics,
  dxRichEdit.View.PageViewInfoGenerator,
  dxRichEdit.DocumentLayout.UnitConverter,
  dxRichEdit.Control.Mouse;

type
  { TdxRichEditAutoScrollerHotZoneHelper }

  TdxRichEditAutoScrollerHotZoneHelper = class helper for TdxRichEditAutoScrollerHotZone
  private
    function GetController: TdxRichEditMouseController;
  public
    property Controller: TdxRichEditMouseController read GetController;
  end;

{ TdxRichEditAutoScrollerHotZoneHelper }

function TdxRichEditAutoScrollerHotZoneHelper.GetController: TdxRichEditMouseController;
begin
  Result := TdxRichEditMouseController(inherited Controller);
end;

{ TdxAutoScrollerHotZone }

function TdxAutoScrollerHotZone.Initialize(const AMousePosition: TPoint): Boolean;
begin
  FBounds := CalculateHotZoneBounds;
  Result := (FBounds.Width > 0) and (FBounds.Height > 0);
end;

{ TdxAutoScroller }

constructor TdxAutoScroller.Create(AController: TdxCustomMouseController);
begin
  inherited Create;
  FController := AController;
  FTimer := TcxTimer.Create(nil);
  FTimer.Interval := AutoScrollTimerInterval;
  FHotZones := TdxAutoScrollerHotZoneCollection.Create;
end;

destructor TdxAutoScroller.Destroy;
begin
  FreeAndNil(FHotZones);
  FreeAndNil(FTimer);
  inherited Destroy;
end;

procedure TdxAutoScroller.Deactivate;
begin
  FIsActive := False;
  FHotZones.Clear;
  FActiveHotZone := nil;
end;

procedure TdxAutoScroller.Activate(const AMousePosition: TPoint);
var
  ACount, I: Integer;
begin
  FIsActive := False;
  FHotZones.Clear;
  PopulateHotZones;
  ACount := FHotZones.Count;
  for I := 0 to ACount - 1 do
    FIsActive := FHotZones[I].Initialize(AMousePosition) or FIsActive;
end;

procedure TdxAutoScroller.HandleMouseMove(const P: TPoint);
var
  ANewActiveHotZone: TdxAutoScrollerHotZone;
begin
  if not FIsActive then
    Exit;
  ANewActiveHotZone := CalculateActiveHotZone(P);
  if ANewActiveHotZone <> FActiveHotZone then
  begin
    StopTimer;
    FActiveHotZone := ANewActiveHotZone;
    if FActiveHotZone <> nil then
      StartTimer;
  end;
end;

procedure TdxAutoScroller.Suspend;
begin
  StopTimer;
end;

procedure TdxAutoScroller.DoMouseMove(const P: TPoint);
var
  ANewActiveHotZone: TdxAutoScrollerHotZone;
begin
  if not FIsActive then
    Exit;

  ANewActiveHotZone := CalculateActiveHotZone(P);
  if ANewActiveHotZone <> FActiveHotZone then
  begin
    FActiveHotZone := ANewActiveHotZone;
    if FActiveHotZone <> nil then
      StartTimer
    else
      StopTimer;
  end;
end;

procedure TdxAutoScroller.TimerHandler(Sender: TObject);
begin
  if FActiveHotZone <> nil then
    FActiveHotZone.PerformAutoScroll;
end;

procedure TdxAutoScroller.Resume;
begin
  StartTimer;
end;

function TdxAutoScroller.CalculateActiveHotZone(const P: TPoint): TdxAutoScrollerHotZone;
var
  ACount, I: Integer;
begin
  ACount := HotZones.Count;
  for I := 0 to ACount - 1 do
    if HotZones[I].CanActivate(P) then
      Exit(HotZones[I]);
  Result := nil;
end;

procedure TdxAutoScroller.PopulateHotZones;
begin
  HotZones.Add(TdxRichEditVerticalScrollBackwardHotZone.Create(Controller));
  HotZones.Add(TdxRichEditVerticalScrollForwardHotZone.Create(Controller));
  HotZones.Add(TdxRichEditHorizontalScrollBackwardHotZone.Create(Controller));
  HotZones.Add(TdxRichEditHorizontalScrollForwardHotZone.Create(Controller));
end;

procedure TdxAutoScroller.StartTimer;
begin
  StopTimer;
  FTimer.OnTimer := TimerHandler;
  FTimer.Enabled := True;
end;

procedure TdxAutoScroller.StopTimer;
begin
  FTimer.Enabled := False;
end;

{ TdxRichEditAutoScrollerHotZone }

function TdxRichEditAutoScrollerHotZone.CalculateCompatibleViewBounds: TRect;
var
  AControl: IdxRichEditControl;
begin
  AControl := Control;
  Result := AControl.ViewBounds;
  Result.Left := 0;
  Result.Top := 0;
end;

constructor TdxRichEditAutoScrollerHotZone.Create(
  AController: TdxCustomMouseController);
begin
  inherited Create;
  FController := AController;
end;

function TdxRichEditAutoScrollerHotZone.GetControl: IdxRichEditControl;
begin
  Result := Controller.Control;
end;

{ TdxRichEditVerticalScrollHotZone }

function TdxRichEditVerticalScrollHotZone.GetHotZoneHeight: Integer;
var
  AUnitConverter: TdxDocumentLayoutUnitConverter;
begin
  AUnitConverter := Control.InnerControl.DocumentModel.LayoutUnitConverter;
  Result := AUnitConverter.PixelsToLayoutUnits(16, TdxDocumentModel.DpiY);
end;

procedure TdxRichEditVerticalScrollHotZone.PerformAutoScroll;
var
  Args: TdxMouseEventArgs;
  ACommand: TdxScrollVerticallyByPhysicalOffsetCommand;
begin
  Args := Controller.CreateFakeMouseMoveEventArgs;

  ACommand := TdxScrollVerticallyByPhysicalOffsetCommand.Create(Control);
  try
    ACommand.PhysicalOffset := CalculatePhysicalOffset(Args);
    ACommand.Execute;
  finally
    ACommand.Free;
  end;

  Controller.State.ContinueSelection(Args);
end;

{ TdxRichEditVerticalScrollForwardHotZone }

function TdxRichEditVerticalScrollForwardHotZone.AdjustHotZoneBounds(
  const ABounds: TRect; const AMousePosition: TPoint): TRect;
begin
  if AMousePosition.Y >= ABounds.Top then
    Result.Init(ABounds.Left, AMousePosition.Y + 1, ABounds.Right, ABounds.Bottom)
  else
    Result := ABounds;
end;

function TdxRichEditVerticalScrollForwardHotZone.CalculateHotZoneBounds: TRect;
var
  AHeight: Integer;
begin
  Result := CalculateCompatibleViewBounds;
  AHeight := GetHotZoneHeight;
  Result.Top := Result.Bottom - AHeight;
  Result.Height := MaxInt div 4;
end;

function TdxRichEditVerticalScrollForwardHotZone.CalculatePhysicalOffset(
  const Args: TdxMouseEventArgs): Integer;
var
  AUnitConverter: TdxDocumentLayoutUnitConverter;
begin
  AUnitConverter := Control.InnerControl.DocumentModel.LayoutUnitConverter;
  if (Args.MousePos.Y - Bounds.Top) > AUnitConverter.PixelsToLayoutUnits(10, TdxDocumentModel.DpiY) then
    Result := AUnitConverter.DocumentsToLayoutUnits(150)
  else
    Result := AUnitConverter.DocumentsToLayoutUnits(50);
end;

function TdxRichEditVerticalScrollForwardHotZone.CanActivate(
  const AMousePosition: TPoint): Boolean;
var
  AGenerator: TdxPageViewInfoGenerator;
begin
  AGenerator := Control.InnerControl.ActiveView.PageViewInfoGenerator;
  if AGenerator.TopInvisibleHeight >= AGenerator.TotalHeight - AGenerator.VisibleHeight then
    Result := False
  else
    Result := AMousePosition.Y >= Bounds.Top;
end;

{ TdxRichEditVerticalScrollBackwardHotZone }

function TdxRichEditVerticalScrollBackwardHotZone.AdjustHotZoneBounds(
  const ABounds: TRect; const AMousePosition: TPoint): TRect;
begin
  if AMousePosition.Y <= ABounds.Bottom then
    Result.Init(ABounds.Left, ABounds.Top, ABounds.Right, AMousePosition.Y - 1)
  else
    Result := ABounds;
end;

function TdxRichEditVerticalScrollBackwardHotZone.CalculateHotZoneBounds: TRect;
begin
  Result := CalculateCompatibleViewBounds;
  Result.Top := MinInt div 4;
  Result.Height := -Result.Top + GetHotZoneHeight;
end;

function TdxRichEditVerticalScrollBackwardHotZone.CalculatePhysicalOffset(
  const Args: TdxMouseEventArgs): Integer;
var
  AUnitConverter: TdxDocumentLayoutUnitConverter;
begin
  AUnitConverter := Control.InnerControl.DocumentModel.LayoutUnitConverter;
  if (Bounds.Bottom - Args.MousePos.Y) > AUnitConverter.PixelsToLayoutUnits(10, TdxDocumentModel.DpiY) then
    Result := -AUnitConverter.DocumentsToLayoutUnits(150)
  else
    Result := -AUnitConverter.DocumentsToLayoutUnits(50);
end;

function TdxRichEditVerticalScrollBackwardHotZone.CanActivate(
  const AMousePosition: TPoint): Boolean;
var
  AGenerator: TdxPageViewInfoGenerator;
begin
  AGenerator := Control.InnerControl.ActiveView.PageViewInfoGenerator;
  if AGenerator.TopInvisibleHeight <= 0 then
    Result := False
  else
    Result := AMousePosition.Y <= Bounds.Bottom;
end;

{ TdxRichEditHorizontalScrollHotZone }

function TdxRichEditHorizontalScrollHotZone.GetHotZoneWidth: Integer;
var
  AUnitConverter: TdxDocumentLayoutUnitConverter;
begin
  AUnitConverter := Control.InnerControl.DocumentModel.LayoutUnitConverter;
  Result := AUnitConverter.PixelsToLayoutUnits(16, TdxDocumentModel.DpiY);
end;

procedure TdxRichEditHorizontalScrollHotZone.PerformAutoScroll;
var
  Args: TdxMouseEventArgs;
  ACommand: TdxScrollHorizontallyByPhysicalOffsetCommand;
begin
  Args := Controller.CreateFakeMouseMoveEventArgs;
  ACommand := TdxScrollHorizontallyByPhysicalOffsetCommand.Create(Control);
  try
    ACommand.PhysicalOffset := CalculatePhysicalOffset(Args);
    ACommand.Execute;
  finally
    ACommand.Free;
  end;
  Controller.State.ContinueSelection(Args);
end;

{ TdxRichEditHorizontalScrollForwardHotZone }

function TdxRichEditHorizontalScrollForwardHotZone.AdjustHotZoneBounds(
  const ABounds: TRect; const AMousePosition: TPoint): TRect;
begin
  if AMousePosition.X >= ABounds.Left then
    Result.Init(AMousePosition.X + 1, ABounds.Top, ABounds.Right, ABounds.Bottom)
  else
    Result := ABounds;
end;

function TdxRichEditHorizontalScrollForwardHotZone.CalculateHotZoneBounds: TRect;
var
  AWidth: Integer;
begin
  Result := CalculateCompatibleViewBounds;
  AWidth := GetHotZoneWidth;
  Result.Left := Result.Right - AWidth;
  Result.Width := MaxInt div 4;
end;

function TdxRichEditHorizontalScrollForwardHotZone.CalculatePhysicalOffset(
  const Args: TdxMouseEventArgs): Integer;
var
  AUnitConverter: TdxDocumentLayoutUnitConverter;
begin
  AUnitConverter := Control.InnerControl.DocumentModel.LayoutUnitConverter;
  if (Args.MousePos.X - Bounds.Left) > AUnitConverter.PixelsToLayoutUnits(10, TdxDocumentModel.DpiX) then
    Result := AUnitConverter.DocumentsToLayoutUnits(150)
  else
    Result := AUnitConverter.DocumentsToLayoutUnits(50);
end;

function TdxRichEditHorizontalScrollForwardHotZone.CanActivate(
  const AMousePosition: TPoint): Boolean;
var
  AGenerator: TdxPageViewInfoGenerator;
begin
  AGenerator := Control.InnerControl.ActiveView.PageViewInfoGenerator;
  if AGenerator.VisibleWidth >= AGenerator.TotalWidth then
    Exit(False);
  if AGenerator.LeftInvisibleWidth >= AGenerator.TotalWidth - AGenerator.VisibleWidth then
    Result := False
  else
    Result := AMousePosition.X >= Bounds.Left;
end;

{ TdxRichEditHorizontalScrollBackwardHotZone }

function TdxRichEditHorizontalScrollBackwardHotZone.AdjustHotZoneBounds(
  const ABounds: TRect; const AMousePosition: TPoint): TRect;
begin
  if AMousePosition.X <= ABounds.Right then
    Result.Init(ABounds.Left, ABounds.Top, AMousePosition.X - 1, ABounds.Bottom)
  else
    Result := ABounds;
end;

function TdxRichEditHorizontalScrollBackwardHotZone.CalculateHotZoneBounds: TRect;
begin
  Result := CalculateCompatibleViewBounds;
  Result.Left := MinInt div 4;
  Result.Width := -Result.Left + GetHotZoneWidth;
end;

function TdxRichEditHorizontalScrollBackwardHotZone.CalculatePhysicalOffset(
  const Args: TdxMouseEventArgs): Integer;
var
  AUnitConverter: TdxDocumentLayoutUnitConverter;
begin
  AUnitConverter := Control.InnerControl.DocumentModel.LayoutUnitConverter;
  if (Bounds.Right - Args.MousePos.X) > AUnitConverter.PixelsToLayoutUnits(10, TdxDocumentModel.DpiX) then
    Result := -AUnitConverter.DocumentsToLayoutUnits(150)
  else
    Result := -AUnitConverter.DocumentsToLayoutUnits(50);
end;

function TdxRichEditHorizontalScrollBackwardHotZone.CanActivate(
  const AMousePosition: TPoint): Boolean;
var
  AGenerator: TdxPageViewInfoGenerator;
begin
  AGenerator := Control.InnerControl.ActiveView.PageViewInfoGenerator;
  if AGenerator.VisibleWidth >= AGenerator.TotalWidth then
    Exit(False);
  if AGenerator.LeftInvisibleWidth <= 0 then
    Result := False
  else
    Result := AMousePosition.X <= Bounds.Right;
end;

end.
