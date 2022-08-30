{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressWizardControl                                     }
{                                                                    }
{           Copyright (c) 2012-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSWIZARDCONTROL AND ALL          }
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

unit dxWizardControlForm;

{$I cxVer.inc}

interface

uses
  Types, Windows, Messages, Classes, Controls, Forms, dxMessages, dxCoreClasses, dxForms,
  dxCustomWizardControl, dxWizardControlViewStyleAero;

type

  { TdxWizardControlFormHelper }

  TdxWizardControlFormHelper = class(TcxIUnknownObject, IdxWizardControlFormHelper)
  private
    FForm: TForm;
    FIsGlassFrameExtended: Boolean;
    FGlassFrameTopExtend: Integer;
    FWizardControl: TdxCustomWizardControl;
    function GetIsDesigning: Boolean;
    function GetWizardControlTitleViewInfo: TdxWizardControlCustomTitleViewInfo;
    procedure SetGlassFrameTopExtend(AValue: Integer);
    procedure SetWizardControl(AValue: TdxCustomWizardControl);
  protected
    function CanExtendGlassFrame: Boolean; virtual;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); virtual;
    procedure NotifyGlassFrameChanged;
    function HitAtExtendedCaptionArea(const AScreenPoint: TPoint): Boolean; overload; virtual;
    function HitAtExtendedCaptionArea(const AScreenPoint: TSmallPoint): Boolean; overload;
    procedure ProcessHitTest(var Message: TWMNCHitTest); virtual;
    procedure ProcessWindowPosChanging(AWindowPos: PWindowPos); virtual;
    procedure UpdateGlassFrame; virtual;
    procedure UpdateViewInfo;
    // IdxWizardControlFormHelper
    procedure LayoutChanged;
    function IdxWizardControlFormHelper.ProcessHitTest = ProcessChildHitTest;
    function ProcessChildHitTest(var Message: TWMNCHitTest): Boolean;
  public
    constructor Create(AForm: TForm);
    destructor Destroy; override;
    procedure UpdateGlassFrameExtends;
    //
    property Form: TForm read FForm;
    property GlassFrameTopExtend: Integer read FGlassFrameTopExtend write SetGlassFrameTopExtend;
    property IsDesigning: Boolean read GetIsDesigning;
    property IsGlassFrameExtended: Boolean read FIsGlassFrameExtended;
    property WizardControl: TdxCustomWizardControl read FWizardControl write SetWizardControl;
    property WizardControlTitleViewInfo: TdxWizardControlCustomTitleViewInfo read GetWizardControlTitleViewInfo;
  end;

  { TdxWizardControlForm }

  TdxWizardControlForm = class(TdxForm)
  private
    FHelper: TdxWizardControlFormHelper;
    procedure CMControlChange(var Message: TCMControlChange); message CM_CONTROLCHANGE;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
  protected
    procedure AlignControls(AControl: TControl; var ARect: TRect); override;
    procedure CreateWnd; override;
    procedure InitializeNewForm; override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure PaintWindow(DC: HDC); override;
    procedure SetAutoSize(AValue: Boolean); override;
    procedure WndProc(var Message: TMessage); override;
    //
    property Helper: TdxWizardControlFormHelper read FHelper;
  public
    constructor CreateNew(AOwner: TComponent; Dummy: Integer = 0); override;
    destructor Destroy; override;
  end;

implementation

uses
  cxDWMApi, SysUtils, cxGeometry, cxControls, Math, dxUxTheme, MultiMon;

type
  TdxCustomWizardControlAccess = class(TdxCustomWizardControl);

  { TdxWizardControlHelper }

  TdxWizardControlHelper = class
  public
    class function GetController(AControl: TdxCustomWizardControl): TdxWizardControlController;
    class function GetViewInfo(AControl: TdxCustomWizardControl): TdxWizardControlViewInfo;
    class function IsResizeAnimationActive(AControl: TdxCustomWizardControl): Boolean;
    class procedure RecalculateLayout(AControl: TdxCustomWizardControl);
    class procedure SetFormHelper(AControl: TdxCustomWizardControl; AHelper: IdxWizardControlFormHelper);
  end;

{ TdxWizardControlHelper }

class function TdxWizardControlHelper.GetController(
  AControl: TdxCustomWizardControl): TdxWizardControlController;
begin
  if AControl <> nil then
    Result := TdxCustomWizardControlAccess(AControl).Controller
  else
    Result := nil;
end;

class function TdxWizardControlHelper.GetViewInfo(
  AControl: TdxCustomWizardControl): TdxWizardControlViewInfo;
begin
  if AControl <> nil then
    Result := TdxCustomWizardControlAccess(AControl).ViewInfo
  else
    Result := nil
end;

class function TdxWizardControlHelper.IsResizeAnimationActive(
  AControl: TdxCustomWizardControl): Boolean;
begin
  Result := (AControl <> nil) and
    TdxCustomWizardControlAccess(AControl).IsResizingAnimationActive;
end;

class procedure TdxWizardControlHelper.RecalculateLayout(AControl: TdxCustomWizardControl);
begin
  if AControl <> nil then
    TdxCustomWizardControlAccess(AControl).Changed([wccLayout]);
end;

class procedure TdxWizardControlHelper.SetFormHelper(
  AControl: TdxCustomWizardControl; AHelper: IdxWizardControlFormHelper);
begin
  if AControl <> nil then
    TdxCustomWizardControlAccess(AControl).FFormHelper := AHelper;
end;

{ TdxWizardControlFormHelper }

constructor TdxWizardControlFormHelper.Create(AForm: TForm);
begin
  inherited Create;
  FForm := AForm;
end;

destructor TdxWizardControlFormHelper.Destroy;
begin
  WizardControl := nil;
  inherited Destroy;
end;

function TdxWizardControlFormHelper.CanExtendGlassFrame: Boolean;

  function CheckAlignmentMargins(AControl: TControl): Boolean;
  begin
    Result := not AControl.AlignWithMargins or (AControl.Margins.Left = 0) and
      (AControl.Margins.Top = 0) and (AControl.Margins.Right = 0);
  end;

  function CheckWizardControlAlignment(AControl: TControl): Boolean;
  begin
    case AControl.Align of
      alTop, alClient:
        Result := True;
      alNone:
        Result := Form.AutoSize;
      else
        Result := False;
    end;
    Result := Result and CheckAlignmentMargins(AControl);
  end;

begin
  Result := not IsDesigning and IsCompositionEnabled and
    (WizardControl <> nil) and WizardControl.OptionsViewStyleAero.EnableTitleAero and
     CheckWizardControlAlignment(WizardControl);
end;

function TdxWizardControlFormHelper.HitAtExtendedCaptionArea(const AScreenPoint: TPoint): Boolean;
var
  AHitTest: TdxWizardControlHitTest;
begin
  Result := False;
  if (WizardControl <> nil) and IsGlassFrameExtended then
  begin
    AHitTest := TdxWizardControlHelper.GetController(WizardControl).HitTest;
    AHitTest.Calculate(WizardControl.ScreenToClient(AScreenPoint));
    Result := AHitTest.HitObject is TdxWizardControlViewStyleAeroTitleViewInfo;
  end
end;

function TdxWizardControlFormHelper.HitAtExtendedCaptionArea(const AScreenPoint: TSmallPoint): Boolean;
begin
  Result := HitAtExtendedCaptionArea(SmallPointToPoint(AScreenPoint));
end;

procedure TdxWizardControlFormHelper.Notification(AComponent: TComponent; AOperation: TOperation);
begin
  if AOperation = opRemove then
  begin
    if AComponent = WizardControl then
      WizardControl := nil;
  end;
end;

procedure TdxWizardControlFormHelper.NotifyGlassFrameChanged;
begin
  SendMessage(Form.Handle, DXM_SKINS_SETISSKINNED, Ord(IsGlassFrameExtended), 0);
end;

procedure TdxWizardControlFormHelper.LayoutChanged;
begin
  UpdateGlassFrameExtends;
  UpdateViewInfo;
end;

function TdxWizardControlFormHelper.ProcessChildHitTest(var Message: TWMNCHitTest): Boolean;
begin
  Result := HitAtExtendedCaptionArea(Message.Pos);
  if Result then
    Message.Result := HTTRANSPARENT;
end;

procedure TdxWizardControlFormHelper.ProcessHitTest(var Message: TWMNCHitTest);
begin
  if HitAtExtendedCaptionArea(Message.Pos) then
    Message.Result := HTCAPTION;
end;

procedure TdxWizardControlFormHelper.ProcessWindowPosChanging(AWindowPos: PWindowPos);
var
  AMonitor: HMONITOR;
  AScreenArea: TRect;
  X, Y: Integer;
begin
  if AWindowPos^.flags and SWP_NOSIZE = 0 then
  begin
    if TdxWizardControlHelper.IsResizeAnimationActive(WizardControl) then
    begin
      AMonitor := MonitorFromWindow(Form.Handle, MONITOR_DEFAULTTONEAREST);
      if AMonitor <> 0 then
      begin
        AScreenArea := GetMonitorWorkArea(AMonitor);
        if AWindowPos^.flags and SWP_NOMOVE = 0 then
        begin
          X := AWindowPos^.x;
          Y := AWindowPos^.y;
        end
        else
        begin
          X := Form.Left;
          Y := Form.Top;
        end;
        X := Min(X, AScreenArea.Right - AWindowPos^.cx);
        Y := Min(Y, AScreenArea.Bottom - AWindowPos^.cy);
        AWindowPos^.x := Max(X, AScreenArea.Left);
        AWindowPos^.y := Max(Y, AScreenArea.Top);
        AWindowPos^.flags := AWindowPos^.flags and not SWP_NOMOVE;
      end;
    end;
  end;
end;

procedure TdxWizardControlFormHelper.UpdateGlassFrame;
var
  AMargins: TdxMargins;
begin
  FIsGlassFrameExtended := False;
  if Assigned(DwmExtendFrameIntoClientArea) and not (csDestroying in Form.ComponentState) then
  begin
    ZeroMemory(@AMargins, SizeOf(AMargins));
    AMargins.cyTopHeight := GlassFrameTopExtend;
    DwmExtendFrameIntoClientArea(Form.Handle, @AMargins);
    FIsGlassFrameExtended := GlassFrameTopExtend > 0;
    NotifyGlassFrameChanged;
  end;
  UpdateViewInfo;
end;

procedure TdxWizardControlFormHelper.UpdateGlassFrameExtends;
begin
  if CanExtendGlassFrame then
    GlassFrameTopExtend := cxRectHeight(WizardControlTitleViewInfo.Bounds)
  else
    GlassFrameTopExtend := 0;
end;

procedure TdxWizardControlFormHelper.UpdateViewInfo;
begin
  if WizardControl <> nil then
  begin
    if WizardControlTitleViewInfo.IsPaintOnGlass <> IsGlassFrameExtended then
    begin
      WizardControlTitleViewInfo.IsPaintOnGlass := IsGlassFrameExtended;
      TdxWizardControlHelper.RecalculateLayout(WizardControl);
    end;
  end;
end;

function TdxWizardControlFormHelper.GetIsDesigning: Boolean;
begin
  Result := csDesigning in Form.ComponentState;
end;

function TdxWizardControlFormHelper.GetWizardControlTitleViewInfo: TdxWizardControlCustomTitleViewInfo;
begin
  Result := TdxWizardControlHelper.GetViewInfo(WizardControl).TitleViewInfo;
end;

procedure TdxWizardControlFormHelper.SetGlassFrameTopExtend(AValue: Integer);
begin
  AValue := Max(AValue, 0);
  if FGlassFrameTopExtend <> AValue then
  begin
    FGlassFrameTopExtend := AValue;
    UpdateGlassFrame;
  end;
end;

procedure TdxWizardControlFormHelper.SetWizardControl(AValue: TdxCustomWizardControl);
begin
  if AValue <> WizardControl then
  begin
    if WizardControl <> nil then
    begin
      TdxWizardControlHelper.SetFormHelper(FWizardControl, nil);
      FWizardControl.RemoveFreeNotification(Form);
      FWizardControl := nil;
    end;
    if AValue <> nil then
    begin
      FWizardControl := AValue;
      FWizardControl.FreeNotification(Form);
      TdxWizardControlHelper.SetFormHelper(FWizardControl, Self);
    end;
    UpdateGlassFrameExtends;
    UpdateViewInfo;
  end;
end;

{ TdxWizardControlForm }

constructor TdxWizardControlForm.CreateNew(AOwner: TComponent; Dummy: Integer);
begin
  inherited CreateNew(AOwner, Dummy);
end;

destructor TdxWizardControlForm.Destroy;
begin
  FreeAndNil(FHelper);
  inherited Destroy;
end;

procedure TdxWizardControlForm.AlignControls(AControl: TControl; var ARect: TRect);
begin
  inherited AlignControls(AControl, ARect);
  Helper.UpdateGlassFrameExtends;
end;

procedure TdxWizardControlForm.CreateWnd;
begin
  inherited CreateWnd;
  if not (csLoading in ComponentState) then
    Helper.UpdateGlassFrame;
end;

procedure TdxWizardControlForm.InitializeNewForm;
begin
  inherited InitializeNewForm;
  FHelper := TdxWizardControlFormHelper.Create(Self);
end;

procedure TdxWizardControlForm.Loaded;
begin
  inherited Loaded;
  Helper.UpdateGlassFrame;
end;

procedure TdxWizardControlForm.Notification(AComponent: TComponent; AOperation: TOperation);
begin
  inherited Notification(AComponent, AOperation);
  if Helper <> nil then
    Helper.Notification(AComponent, AOperation);
end;

procedure TdxWizardControlForm.PaintWindow(DC: HDC);
var
  ASaveIndex: Integer;
  R: TRect;
begin
  ASaveIndex := SaveDC(DC);
  try
    R := cxRectSetHeight(ClientRect, Helper.GlassFrameTopExtend);
    FillRect(DC, R, GetStockObject(BLACK_BRUSH));
    ExcludeClipRect(DC, R.Left, R.Top, R.Right, R.Bottom);
    inherited PaintWindow(DC);
  finally
    RestoreDC(DC, ASaveIndex);
  end;
end;

procedure TdxWizardControlForm.CMControlChange(var Message: TCMControlChange);
begin
  inherited;
  if Message.Inserting then
  begin
    if (Helper.WizardControl = nil) and (Message.Control is TdxCustomWizardControl) then
      Helper.WizardControl := TdxCustomWizardControl(Message.Control);
  end
  else
    if Message.Control = Helper.WizardControl then
      Helper.WizardControl := nil;
end;

procedure TdxWizardControlForm.WMEraseBkgnd(var Message: TWMEraseBkgnd);
var
  ASaveIndex: Integer;
begin
  if Helper.IsGlassFrameExtended and not cxIsDrawToMemory(Message) then
  begin
    ASaveIndex := SaveDC(Message.DC);
    try
      ExcludeClipRect(Message.DC, 0, 0, Width, Helper.GlassFrameTopExtend);
      inherited;
    finally
      RestoreDC(Message.DC, ASaveIndex);
    end;
  end
  else
    inherited;
end;

procedure TdxWizardControlForm.WndProc(var Message: TMessage);
begin
  inherited WndProc(Message);
  if Helper <> nil then
    case Message.Msg of
      DXM_SKINS_GETISSKINNED:
        Message.Result := Ord(Helper.IsGlassFrameExtended);
      WM_DWMCOMPOSITIONCHANGED, WM_DWMNCRENDERINGCHANGED:
        Helper.UpdateGlassFrame;
      WM_NCHITTEST:
        Helper.ProcessHitTest(TWMNCHitTest(Message));
      WM_WINDOWPOSCHANGING:
        Helper.ProcessWindowPosChanging(TWMWindowPosChanging(Message).WindowPos);
    end;
end;

procedure TdxWizardControlForm.SetAutoSize(AValue: Boolean);
begin
  if AValue <> AutoSize then
  begin
    inherited SetAutoSize(AValue);
    Helper.UpdateGlassFrameExtends;
  end;
end;

end.
