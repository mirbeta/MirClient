{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           Express Cross Platform Library controls                  }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSCROSSPLATFORMLIBRARY AND ALL   }
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

unit dxTouch;

{$I cxVer.inc}
{$A8}

interface
  uses
    Windows, Messages, Types, Forms, Controls, Graphics, Dialogs, Classes, Themes, RTLConsts,
    dxCore, cxGeometry, dxUxTheme;

const
  dxTouchInteractiveGestureToGestureID: array [TInteractiveGesture] of Integer = (
    GID_ZOOM, GID_PAN, GID_ROTATE, GID_TWOFINGERTAP, GID_PRESSANDTAP);
  dxTouchInteractiveGestureOptionToPanOption: array [igoPanSingleFingerHorizontal..igoPanGutter] of Integer = (
    GC_PAN_WITH_SINGLE_FINGER_HORIZONTALLY, GC_PAN_WITH_SINGLE_FINGER_VERTICALLY, GC_PAN_WITH_INERTIA, GC_PAN_WITH_GUTTER);

  dxTouchPanOptions = GC_PAN or GC_PAN_WITH_SINGLE_FINGER_HORIZONTALLY or
    GC_PAN_WITH_SINGLE_FINGER_VERTICALLY or GC_PAN_WITH_INERTIA or GC_PAN_WITH_GUTTER;
  dxSupportedGestureCount = 5;
  dxSupportedGestureOptions: array [0..dxSupportedGestureCount-1] of Integer = (
    dxTouchPanOptions, GC_ZOOM, GC_ROTATE, GC_TWOFINGERTAP, GC_PRESSANDTAP);
  dxSupportedGestureIDs: array [0..dxSupportedGestureCount-1] of Integer = (GID_PAN, GID_ZOOM, GID_ROTATE,
    GID_TWOFINGERTAP, GID_PRESSANDTAP);

type
  TdxGestureInfo = record
    cbSize: UINT;
    dwFlags: DWORD;
    dwID: DWORD;
    hwndTarget: HWND;
    ptsLocation: TSmallPoint;
    dwInstanceID: DWORD;
    dwSequenceID: DWORD;
    ullArguments: UInt64;
    cbExtraArgs: UINT;
  end;

  TdxGestureNotifyStruct = record
    cbSize: UINT;
    dwFlags: DWORD;
    hwndTarget: HWND;
    ptsLocation: TSmallPoint;
    dwInstanceID: DWORD;
  end;
  PdxGestureNotifyStruct = ^TdxGestureNotifyStruct;

  TWMGestureNotify = record
    Msg: TdxNativeUInt;
    Unused: WPARAM;
    NotifyStruct: PdxGestureNotifyStruct;
    Result: LRESULT;
  end;

  TdxGestureConfig = record
    dwID: DWORD;
    dwWant: DWORD;
    dwBlock: DWORD;
  end;

  TdxGestureConfigs = array of TdxGestureConfig;

  IdxGestureClient = interface
  ['{652B8A09-DAC8-4332-9206-C8905AEE7791}']
    function AllowGesture(AGestureId: Integer): Boolean;
    function AllowPan(AScrollKind: TScrollBarKind): Boolean;
    procedure BeginGestureScroll(APos: TPoint);
    procedure EndGestureScroll;
    procedure GestureScroll(ADeltaX, ADeltaY: Integer);
    function GetPanOptions: Integer;
    function IsPanArea(const APoint: TPoint): Boolean;
    function NeedPanningFeedback(AScrollKind: TScrollBarKind): Boolean;
  end;

  IdxGestureOwner = interface
    ['{4DF001DA-BE2C-4817-B75C-55171270D158}']
    function GetGestureClient(const APoint: TPoint): IdxGestureClient;
    function GetHandle: THandle;
    function IsGestureTarget(AWnd: THandle): Boolean;
    function ScreenToClient(const APoint: TPoint): TPoint;
  end;

  IdxZoomClient = interface
    ['{B3A0623E-D155-462F-B234-22E9CFC6BFBF}']
    procedure Zoom(ADelta: Integer; var AHandled: Boolean);
  end;

  IdxRotateClient = interface
    ['{EAC9BA3A-C2C3-42E2-8CA8-8CEA85821CAE}']
    procedure Rotate(AAngle: Double; var AHandled: Boolean);
  end;

  TdxGestureHelper = class
  private
    FIsInertia: Boolean;
    FGestureClient: IdxGestureClient;
    FGestureNotifyClient: TObject;
    FGestureOwner: IdxGestureOwner;
    FIsPanning: Boolean;
    FLastDistance: Integer;
    FPanPoint: TPoint;
    FOverPan: TPoint;
    FNotifyTouchControl: TControl;
    FTouchControl: TControl;
    procedure DoOverpan(AScrollKind: TScrollBarKind; ADelta: Integer);
    function GetGestureNotifyClient: IdxGestureClient;
    function GetTouchStartPoint: TPoint;
    function Pan(APos: TPoint; AIsBegin, AIsEnd, AIsInertia: Boolean): Boolean;
    function Rotate(AAngle: Double; AIsBegin, AIsEnd: Boolean): Boolean;
    function Zoom(ADistance: Integer; AIsBegin, AIsEnd: Boolean): Boolean;
  protected
    function AllowGesture(AGestureId: Integer): Boolean; virtual;
    function AllowPan(AScrollKind: TScrollBarKind): Boolean; virtual;
    procedure BeginGestureScroll(const APos: TPoint);
    function Gesture(var Message: TMessage): Boolean;
    function GestureNotify(var Message: TWMGestureNotify): Boolean;
    procedure EndGestureScroll;
    procedure GestureScroll(ADeltaX, ADeltaY: Integer);
    function GetAllowedPanOptions(const APoint: TPoint): Integer;
    function GetHandle: THandle; virtual;
    function GetPanOptions: Integer; virtual;
    function IsGestureTarget(AWnd: THandle): Boolean; virtual;
    function IsPanArea(const APoint: TPoint): Boolean; virtual;
    function NeedPanningFeedback(AScrollKind: TScrollBarKind): Boolean; virtual;
    function ScreenToClient(const APoint: TPoint): TPoint; virtual;

    property Handle: THandle read GetHandle;
  public
    constructor Create(AGestureOwner: IdxGestureOwner);
    destructor Destroy; override;
    procedure CheckOverpan(AScrollKind: TScrollBarKind;
      ANewDataPos, AMinDataPos, AMaxDataPos: Integer; ADeltaX, ADeltaY: Integer);
    procedure CheckGestureOptions(var Gestures: TInteractiveGestures;
      var Options: TInteractiveGestureOptions);
    procedure DoGesture(const EventInfo: TGestureEventInfo; var Handled: Boolean);
    function HandleMessage(var Message: TMessage): Boolean;

    property IsPanning: Boolean read FIsPanning;
  end;

  function dxGetGestureInfo(hGestureInfo: THandle; var AGestureInfo: TdxGestureInfo): Boolean;
  function dxCloseGestureInfoHandle(hGestureInfo: THandle): Boolean;
  function dxSetGestureConfig(AWindow: THandle; const AGestureConfigs: TdxGestureConfigs): Boolean;
  function dxLogicalToPhysicalPoint(AWindow: THandle; var APoint: TPoint): Boolean;
  function dxPhysicalToLogicalPoint(AWindow: THandle; var APoint: TPoint): Boolean;
  function dxLockGestures(AHandle: THandle; var Message: TWMGestureNotify): Boolean;
  function dxIsTouchEvent(Shift: TShiftState): Boolean;
  function GetInteractiveGestureByGestureID(AGestureID: Integer): TInteractiveGesture;
  function GetInteractiveGestureOptionByPanOptionID(APanOptionID: Integer): TInteractiveGestureOption;
  function GetInteractiveGestureOptionsByPanOptions(APanOptions: Integer): TInteractiveGestureOptions;
  function GetPanOptionsByInteractiveGestureOptions(AGestureOptions: TInteractiveGestureOptions): Integer;

implementation

uses
  Math, SysUtils, Registry, dxHooks, cxGraphics, dxCoreClasses, dxDPIAwareUtils;

var
  FUser32Lib: HMODULE;
  FGetGestureInfo: function (hGestureInfo: TdxNativeUInt; var pGestureInfo: TdxGestureInfo): BOOL; stdcall;
  FCloseGestureInfoHandle: function (hGestureInfo: TdxNativeUInt): BOOL; stdcall;
  FSetGestureConfig: function (hwnd: HWND; dwReserved: DWORD; cIDs: UINT; const pGestureConfig: TdxGestureConfigs; cbSize: UINT): BOOL; stdcall;
  FLogicalToPhysicalPoint: function (hWnd: HWND; var lpPoint: TPoint): BOOL; stdcall;
  FPhysicalToLogicalPoint: function (hWnd: HWND; var lpPoint: TPoint): BOOL; stdcall;
  FGestureStartPoint: TPoint;
  FNeedScalePanning: Boolean;
  FTouchHelpers: TdxFastObjectList;

procedure InitializeTouchHelpers;
var
  I: Integer;
begin
  for I := 0 to FTouchHelpers.Count - 1 do
    (FTouchHelpers[I] as TdxGestureHelper).FNotifyTouchControl := nil;
end;

procedure dxTouchWndProcHook(ACode: Integer; wParam: WPARAM; lParam: LPARAM; var AHookResult: LRESULT);
begin
  if ACode <> HC_ACTION then
    Exit;
  if Windows.PCWPStruct(LParam)^.message = WM_GESTURENOTIFY then
  begin
    FGestureStartPoint := SmallPointToPoint(PdxGestureNotifyStruct(Windows.PCWPStruct(LParam)^.lParam)^.ptsLocation);
    InitializeTouchHelpers;
  end;
end;

function dxIsTouchEvent(Shift: TShiftState): Boolean;
begin
  Result := [ssTouch, ssPen] * Shift <> [];
end;

function GetInteractiveGestureByGestureID(AGestureID: Integer): TInteractiveGesture;
begin
  Result := igPressAndTap;
  case AGestureID of
    GID_ZOOM: Result := igZoom;
    GID_PAN: Result := igPan;
    GID_ROTATE: Result := igRotate;
    GID_TWOFINGERTAP: Result := igTwoFingerTap;
    GID_PRESSANDTAP: Result := igPressAndTap;
  end;
end;

function GetInteractiveGestureOptionByPanOptionID(APanOptionID: Integer): TInteractiveGestureOption;
begin
  Result := igoPanInertia;
  case APanOptionID of
    GC_PAN_WITH_SINGLE_FINGER_VERTICALLY: Result := igoPanSingleFingerVertical;
    GC_PAN_WITH_SINGLE_FINGER_HORIZONTALLY: Result := igoPanSingleFingerHorizontal;
    GC_PAN_WITH_GUTTER: Result := igoPanGutter;
    GC_PAN_WITH_INERTIA: Result := igoPanInertia;
  end;
end;

function GetInteractiveGestureOptionsByPanOptions(APanOptions: Integer): TInteractiveGestureOptions;
const
  APanOptionIDCount = 4;
  APanOptionIDs: array [0..APanOptionIDCount-1] of Integer = (GC_PAN_WITH_SINGLE_FINGER_VERTICALLY,
    GC_PAN_WITH_SINGLE_FINGER_HORIZONTALLY, GC_PAN_WITH_GUTTER, GC_PAN_WITH_INERTIA);
var
  I: Integer;
begin
  Result := [];
  for I := 0 to APanOptionIDCount - 1 do
    if (APanOptionIDs[I] and APanOptions) <> 0 then
      Include(Result, GetInteractiveGestureOptionByPanOptionID(APanOptionIDs[I]));
end;

function GetPanOptionsByInteractiveGestureOptions(AGestureOptions: TInteractiveGestureOptions): Integer;
var
  I: TInteractiveGestureOption;
begin
  Result := 0;
  for I := igoPanSingleFingerHorizontal to igoPanGutter do
    if I in AGestureOptions then
      Result := Result or dxTouchInteractiveGestureOptionToPanOption[I];
end;

function dxGetGestureInfo(hGestureInfo: THandle;
    var AGestureInfo: TdxGestureInfo): Boolean;
begin
  if Assigned(FGetGestureInfo) then
    Result := FGetGestureInfo(hGestureInfo, AGestureInfo)
  else
    Result := False;
end;

function dxCloseGestureInfoHandle(hGestureInfo: THandle): Boolean;
begin
  if Assigned(FCloseGestureInfoHandle) then
    Result := FCloseGestureInfoHandle(hGestureInfo)
  else
    Result := False;
end;

function dxSetGestureConfig(AWindow: THandle; const AGestureConfigs: TdxGestureConfigs): Boolean;
begin
  if Assigned(FSetGestureConfig) then
    Result := FSetGestureConfig(AWindow, 0, Length(AGestureConfigs), AGestureConfigs, SizeOf(TdxGestureConfig))
  else
    Result := False;
end;

function dxLogicalToPhysicalPoint(AWindow: THandle; var APoint: TPoint): Boolean;
begin
  if Assigned(FLogicalToPhysicalPoint) then
    Result := FLogicalToPhysicalPoint(AWindow, APoint)
  else
    Result := False;
end;

function dxPhysicalToLogicalPoint(AWindow: THandle; var APoint: TPoint): Boolean;
begin
  if Assigned(FPhysicalToLogicalPoint) then
    Result := FPhysicalToLogicalPoint(AWindow, APoint)
  else
    Result := False;
end;

function dxLockGestures(AHandle: THandle; var Message: TWMGestureNotify): Boolean;
var
  AConfigs: TdxGestureConfigs;
begin
  SetLength(AConfigs, 1);
  AConfigs[0].dwID := 0;
  AConfigs[0].dwWant := 0;
  AConfigs[0].dwBlock := GC_ALLGESTURES;
  Result := dxSetGestureConfig(AHandle, AConfigs);
  Message.Result := 1;
end;

{ TdxGestureHelper }

constructor TdxGestureHelper.Create(AGestureOwner: IdxGestureOwner);
begin
  inherited Create;
  FGestureOwner := AGestureOwner;
  FTouchHelpers.Add(Self);
end;

destructor TdxGestureHelper.Destroy;
begin
  FTouchHelpers.Remove(Self);
  inherited;
end;

procedure TdxGestureHelper.CheckOverpan(AScrollKind: TScrollBarKind;
  ANewDataPos, AMinDataPos, AMaxDataPos: Integer; ADeltaX, ADeltaY: Integer);
begin
  if NeedPanningFeedback(AScrollKind) and
    not InRange(ANewDataPos, AMinDataPos + 1, AMaxDataPos - 1) then
    if AScrollKind = sbHorizontal then
      DoOverpan(AScrollKind, ADeltaX)
    else
      DoOverpan(AScrollKind, ADeltaY);
end;

procedure TdxGestureHelper.CheckGestureOptions(var Gestures: TInteractiveGestures;
  var Options: TInteractiveGestureOptions);
begin
  FGestureNotifyClient := FGestureOwner.GetGestureClient(GetTouchStartPoint) as TObject;
  if igPan in Gestures then
    if not IsPanArea(GetTouchStartPoint) then
      Options := Options - [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical]
    else
      if Options * [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical] <> [] then
      begin
        if not AllowPan(sbHorizontal) then
          Options := Options - [igoPanSingleFingerHorizontal];
        if not AllowPan(sbVertical) then
          Options := Options - [igoPanSingleFingerVertical];
      end;
  if Options * [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical] = [] then
    Gestures := Gestures - [igPan];
end;

procedure TdxGestureHelper.DoGesture(const EventInfo: TGestureEventInfo; var Handled: Boolean);
begin
  case EventInfo.GestureID of
    igiPan:
      Handled := Pan(EventInfo.Location, gfBegin in EventInfo.Flags, gfEnd in EventInfo.Flags, gfInertia in EventInfo.Flags);
    igiZoom:
      Handled := Zoom(EventInfo.Distance, gfBegin in EventInfo.Flags, gfEnd in EventInfo.Flags);
    igiRotate:
      Handled := Rotate(EventInfo.Angle, gfBegin in EventInfo.Flags, gfEnd in EventInfo.Flags);
  end;
end;

function TdxGestureHelper.HandleMessage(var Message: TMessage): Boolean;
begin
  case Message.Msg of
    WM_GESTURE:
      Result := Gesture(Message);
    WM_GESTURENOTIFY:
      Result := GestureNotify(TWMGestureNotify(Message));
    WM_DESTROY:
      begin
        Result := False;
        if FIsPanning then
          EndGestureScroll;
      end;
  else
    Result := False;
  end;
end;

function TdxGestureHelper.AllowGesture(AGestureId: Integer): Boolean;
begin
  Result := GetGestureNotifyClient.AllowGesture(AGestureId);
end;

function TdxGestureHelper.AllowPan(AScrollKind: TScrollBarKind): Boolean;
begin
  Result := GetGestureNotifyClient.AllowPan(AScrollKind);
end;

procedure TdxGestureHelper.BeginGestureScroll(const APos: TPoint);
begin
  FIsPanning := True;
  FGestureClient.BeginGestureScroll(APos);
  if NeedPanningFeedback(sbHorizontal) or
    NeedPanningFeedback(sbVertical) then
    dxBeginPanningFeedback(Handle);
  FOverPan := cxNullPoint;
end;

function TdxGestureHelper.Gesture(var Message: TMessage): Boolean;
const
  AGestureMap: array[0..4] of TInteractiveGesture = (igZoom, igPan, igRotate,
    igTwoFingerTap, igPressAndTap);
var
  APoint: TPoint;
  AControl: TControl;
  AGestureInfo: GestureInfo;
  AEventInfo: TGestureEventInfo;
begin
  Result := True;
  ZeroMemory(@AGestureInfo, SizeOf(AGestureInfo));
  AGestureInfo.cbSize := Sizeof(AGestureInfo);
  if GetGestureInfo(Message.LParam, AGestureInfo) then
  try
    ZeroMemory(@AEventInfo, SizeOf(AEventInfo));
    AEventInfo.GestureID := AGestureInfo.dwID + igiFirst;
    AEventInfo.Flags := [];
    if AGestureInfo.dwFlags and GF_BEGIN = GF_BEGIN then
      Include(AEventInfo.Flags, gfBegin);
    if AGestureInfo.dwFlags and GF_INERTIA = GF_INERTIA then
      Include(AEventInfo.Flags, gfInertia);
    if AGestureInfo.dwFlags and GF_END = GF_END then
      Include(AEventInfo.Flags, gfEnd);

    if (AEventInfo.GestureID <> igiBegin) and (AEventInfo.GestureID <> igiEnd) and
      ((FTouchControl <> nil) or ((gfBegin in AEventInfo.Flags) and (FNotifyTouchControl <> nil))) then
    begin
      if gfBegin in AEventInfo.Flags then
        FTouchControl := FNotifyTouchControl;

      AControl := FTouchControl;
      while (AControl.Parent <> nil) and
        (igoParentPassthrough in AControl.Touch.InteractiveGestureOptions) and
         not (AGestureMap[AEventInfo.GestureID - igiZoom] in AControl.Touch.InteractiveGestures) do
        AControl := AControl.Parent;

      APoint := SmallPointToPoint(AGestureInfo.ptsLocation);
      AEventInfo.Location := AControl.ScreenToClient(APoint);
      case AEventInfo.GestureID of
        igiPan:
          begin
            AEventInfo.Distance := Cardinal(AGestureInfo.ullArguments);
            AEventInfo.InertiaVector := InertiaVectorFromArgument(AGestureInfo.ullArguments);
          end;
        igiZoom, igiTwoFingerTap:
          AEventInfo.Distance := Cardinal(AGestureInfo.ullArguments);
        igiPressAndTap:
          begin
            APoint := SmallPointToPoint(TSmallPoint(Cardinal(AGestureInfo.ullArguments)));
            Inc(APoint.X, AGestureInfo.ptsLocation.X);
            Inc(APoint.Y, AGestureInfo.ptsLocation.Y);
            if AControl is TWinControl then
              PhysicalToLogicalPoint(TWinControl(AControl).Handle, APoint)
            else
              PhysicalToLogicalPoint(AControl.Parent.Handle, APoint);
            AEventInfo.TapLocation := PointToSmallPoint(AControl.ScreenToClient(APoint));
          end;
        igiRotate:
          AEventInfo.Angle := RotateAngleFromArgument(AGestureInfo.ullArguments);
      end;
      Message.Result := AControl.Perform(CM_GESTURE, 0, @AEventInfo);
      if gfEnd in AEventInfo.Flags then
        FTouchControl := nil;
    end;
    if Message.Result <> 1 then
      Message.Result := DefWindowProc(Handle, Message.Msg, Message.WParam, Message.LParam);
  finally
    CloseGestureInfoHandle(Message.LParam);
  end;
end;

function TdxGestureHelper.GestureNotify(var Message: TWMGestureNotify): Boolean;
var
  APoint: TPoint;
  AGestureOwnerControl: TWinControl;
begin
  Result := False;
  APoint := GetTouchStartPoint;
  AGestureOwnerControl := FindControl(Handle);
  if PtInRect(AGestureOwnerControl.ClientRect, APoint) then
  begin
    FNotifyTouchControl := AGestureOwnerControl.ControlAtPos(APoint, True);
    if FNotifyTouchControl = nil then
      FNotifyTouchControl := AGestureOwnerControl;
  end;
end;

procedure TdxGestureHelper.EndGestureScroll;
begin
  FIsPanning := False;
  FGestureClient.EndGestureScroll;
  if NeedPanningFeedback(sbHorizontal) or
    NeedPanningFeedback(sbVertical) then
    dxEndPanningFeedback(Handle, True);
end;

procedure TdxGestureHelper.GestureScroll(ADeltaX, ADeltaY: Integer);
begin
  FGestureClient.GestureScroll(ADeltaX, ADeltaY);
end;

function TdxGestureHelper.GetAllowedPanOptions(const APoint: TPoint): Integer;

  procedure CheckPanOptions(var APanOptions: Integer);
  begin
    if not IsPanArea(APoint) then
      APanOptions := APanOptions and
        not GC_PAN_WITH_SINGLE_FINGER_VERTICALLY and
        not GC_PAN_WITH_SINGLE_FINGER_HORIZONTALLY
    else
    begin
      if not AllowPan(sbVertical) then
        APanOptions := APanOptions and not GC_PAN_WITH_SINGLE_FINGER_VERTICALLY;
      if not AllowPan(sbHorizontal) then
        APanOptions := APanOptions and not GC_PAN_WITH_SINGLE_FINGER_HORIZONTALLY;
    end;
  end;

begin
  Result := GetPanOptions;
  CheckPanOptions(Result);
end;

function TdxGestureHelper.GetHandle: THandle;
begin
  Result := FGestureOwner.GetHandle;
end;

function TdxGestureHelper.GetPanOptions: Integer;
begin
  Result := GetGestureNotifyClient.GetPanOptions;
end;

function TdxGestureHelper.IsGestureTarget(AWnd: THandle): Boolean;
begin
  Result := FGestureOwner.IsGestureTarget(AWnd);
end;

function TdxGestureHelper.IsPanArea(const APoint: TPoint): Boolean;
begin
  Result := GetGestureNotifyClient.IsPanArea(APoint);
end;

function TdxGestureHelper.NeedPanningFeedback(AScrollKind: TScrollBarKind): Boolean;
begin
  Result := FGestureClient.NeedPanningFeedback(AScrollKind);
end;

function TdxGestureHelper.ScreenToClient(const APoint: TPoint): TPoint;
begin
  Result := FGestureOwner.ScreenToClient(APoint);
end;

procedure TdxGestureHelper.DoOverpan(AScrollKind: TScrollBarKind; ADelta: Integer);
begin
  if AScrollKind = sbHorizontal then
    Inc(FOverPan.X, ADelta)
  else
    Inc(FOverPan.Y, ADelta);
  dxUpdatePanningFeedback(Handle, FOverPan,  FIsInertia);
end;

function TdxGestureHelper.GetGestureNotifyClient: IdxGestureClient;
begin
  Supports(FGestureNotifyClient, IdxGestureClient, Result);
end;

function TdxGestureHelper.GetTouchStartPoint: TPoint;
begin
  Result := FGestureStartPoint;
  dxPhysicalToLogicalPoint(Handle, Result);
  Result := ScreenToClient(Result);
end;

function TdxGestureHelper.Pan(APos: TPoint; AIsBegin, AIsEnd, AIsInertia: Boolean): Boolean;

  procedure CheckOverpanned(AScrollKind: TScrollBarKind; var ADelta: Integer);
  var
    AOverpan, AOverpanDelta: Integer;
  begin
    AOverpan := IfThen(AScrollKind = sbHorizontal, FOverPan.X, FOverPan.Y);
    if (AOverpan <> 0) and (ADelta <> 0) and (Sign(ADelta) <> Sign(AOverpan)) and
      NeedPanningFeedback(AScrollKind) then
    begin
      if Abs(AOverpan) < Abs(ADelta) then
        AOverpanDelta := -AOverpan
      else
        AOverpanDelta := ADelta;
      DoOverpan(AScrollKind, AOverpanDelta);
      ADelta := ADelta - AOverpanDelta;
    end;
  end;

var
  ADeltaX, ADeltaY: Integer;
begin
  if FNeedScalePanning then
    APos := dxSystemScaleFactor.Apply(APos, dxDesktopScaleFactor);
  FIsInertia := AIsInertia;
  if AIsBegin then
  begin
    FGestureClient := GetGestureNotifyClient;
    BeginGestureScroll(APos);
  end
  else
    if FGestureClient <> nil then
    begin
      ADeltaX := APos.X - FPanPoint.X;
      ADeltaY := APos.Y - FPanPoint.Y;
      if AIsEnd then
        FOverPan := cxNullPoint;
      CheckOverpanned(sbHorizontal, ADeltaX);
      CheckOverpanned(sbVertical, ADeltaY);
      GestureScroll(ADeltaX, ADeltaY);
      if AIsEnd then
      begin
        EndGestureScroll;
        FGestureClient := nil;
      end;
    end;
  FPanPoint := APos;
  Result := True;
end;

function TdxGestureHelper.Rotate(AAngle: Double; AIsBegin, AIsEnd: Boolean): Boolean;
var
  ARotateClient: IdxRotateClient;
begin
  Result := False;
  if AIsBegin then
    FGestureClient := GetGestureNotifyClient;
  if Supports(FGestureClient, IdxRotateClient, ARotateClient) then
    ARotateClient.Rotate(AAngle, Result);
  if AIsEnd then
    FGestureClient := nil;
end;

function TdxGestureHelper.Zoom(ADistance: Integer; AIsBegin, AIsEnd: Boolean): Boolean;
var
  AZoomClient: IdxZoomClient;
begin
  Result := False;
  if AIsBegin then
  begin
    FLastDistance := ADistance;
    FGestureClient := GetGestureNotifyClient;
  end;
  if Supports(FGestureClient, IdxZoomClient, AZoomClient) then
    AZoomClient.Zoom(ADistance - FLastDistance, Result);
  FLastDistance := ADistance;
  if AIsEnd then
    FGestureClient := nil;
end;

initialization
  FUser32Lib := GetModuleHandle(user32);
  if FUser32Lib <> 0 then
  begin
    @FGetGestureInfo := GetProcAddress(FUser32Lib, 'GetGestureInfo');
    @FCloseGestureInfoHandle := GetProcAddress(FUser32Lib, 'CloseGestureInfoHandle');
    @FSetGestureConfig := GetProcAddress(FUser32Lib, 'SetGestureConfig');
    @FLogicalToPhysicalPoint := GetProcAddress(FUser32Lib, 'LogicalToPhysicalPoint');
    @FPhysicalToLogicalPoint := GetProcAddress(FUser32Lib, 'PhysicalToLogicalPoint');
  end;
  FNeedScalePanning := not IsWin8OrLater and not dxIsProcessDPIAware;
  FTouchHelpers := TdxFastObjectList.Create(False);
  dxSetHook(htWndProc, dxTouchWndProcHook);

finalization
  dxReleaseHook(dxTouchWndProcHook);
  FreeAndNil(FTouchHelpers);

end.
