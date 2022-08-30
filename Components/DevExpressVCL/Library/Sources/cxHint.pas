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

unit cxHint;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Variants, Types,
  Windows, Classes, Forms, Controls{must be after Forms for D11}, Graphics,
  ImgList, Messages, StdCtrls, SysUtils, cxClasses, cxContainer, cxControls,
  cxGraphics, cxLookAndFeels, cxLookAndFeelPainters, dxCustomHint, ComCtrls, CommCtrl, dxTypeHelpers, cxGeometry;

type
  TcxHintWindow = class;
  TcxHintAnimationDelay = 0..1000;
  TcxCallOutPosition = (cxbpNone, cxbpAuto, cxbpLeftBottom, cxbpLeftTop, cxbpTopLeft,
    cxbpTopRight, cxbpRightBottom, cxbpRightTop, cxbpBottomRight, cxbpBottomLeft);
  TcxHintIconType = (cxhiNone, cxhiApplication, cxhiInformation, cxhiWarning,
    cxhiError, cxhiQuestion, cxhiWinLogo, cxhiCurrentApplication, cxhiCustom);
  TcxHintAnimate = TcxHintAnimationStyle;
  TcxHintIconSize = (cxisDefault, cxisLarge, cxisSmall);

  IcxHint = interface
  ['{0680CE5D-391B-45A1-B55D-AFCAE92F2DA6}']
    function GetAnimate: TcxHintAnimate;
    function GetAnimationDelay: TcxHintAnimationDelay;
    function GetBorderColor: TColor;
    function GetCallOutPosition: TcxCallOutPosition;
    function GetColor: TColor;
    function GetIconSize: TcxHintIconSize;
    function GetIconType: TcxHintIconType;
    function GetHintCaption: string;
    function GetRounded: Boolean;
    function GetRoundRadius: Integer;
    function GetStandard: Boolean;
    function GetHintFont: TFont;
    function GetHintCaptionFont: TFont;
    function GetHintIcon: TIcon;
    procedure SetHintCaption(Value: string);
    property HintCaption: string read GetHintCaption write SetHintCaption;
  end;

  { TcxHintStyle }

  TcxHintStyle = class(TcxCustomHintStyle, IcxHint)
  private
    FAnimate: TcxHintAnimate;
    FAnimationDelay: TcxHintAnimationDelay;
    FCallOutPosition: TcxCallOutPosition;
    FBorderColor: TColor;
    FColor: TColor;
    FFont: TFont;
    FCaptionFont: TFont;
    FIcon: TIcon;
    FIconSize: TcxHintIconSize;
    FIconType: TcxHintIconType;
    FRounded: Boolean;
    FRoundRadius: Integer;
    FStandard: Boolean;
    FIsDestroying: Boolean;
    FModified: Boolean;
    FUpdateCount: Integer;
    function GetFont: TFont;
    procedure SetAnimate(Value: TcxHintAnimate);
    procedure SetAnimationDelay(Value: TcxHintAnimationDelay);
    procedure SetCallOutPosition(Value: TcxCallOutPosition);
    procedure SetBorderColor(Value: TColor);
    procedure SetCaptionFont(Value: TFont);
    procedure SetColor(Value: TColor);
    procedure SetFont(Value: TFont);
    procedure SetIcon(Value: TIcon);
    procedure SetIconSize(Value: TcxHintIconSize);
    procedure SetIconType(Value: TcxHintIconType);
    procedure SetRounded(Value: Boolean);
    procedure SetRoundRadius(Value: Integer);
    procedure SetStandard(Value: Boolean);
    procedure IconChangeHandler(Sender: TObject);
    procedure InternalRestoreDefault;
  protected
    FHintStyleController: TcxCustomHintStyleController;
    function BaseGetHintStyleController: TcxCustomHintStyleController;
    procedure BaseSetHintStyleController(Value: TcxCustomHintStyleController);
    procedure Changed; virtual;
    procedure ControllerChangedNotification(AStyleController: TcxCustomHintStyleController); override;
    procedure ControllerFreeNotification(AHintStyleController: TcxCustomHintStyleController); override;
    procedure HintStyleControllerChanged; virtual;
    // IcxHint
    function GetAnimate: TcxHintAnimate;
    function GetAnimationDelay: TcxHintAnimationDelay;
    function GetBorderColor: TColor;
    function GetCallOutPosition: TcxCallOutPosition;
    function GetColor: TColor;
    function GetIconSize: TcxHintIconSize;
    function GetIconType: TcxHintIconType;
    function GetHintCaption: string;
    function GetRounded: Boolean;
    function GetRoundRadius: Integer;
    function GetStandard: Boolean;
    function GetHintFont: TFont;
    function GetHintCaptionFont: TFont;
    function GetHintIcon: TIcon;
    procedure SetHintCaption(Value: string);

    property HintStyleController: TcxCustomHintStyleController read BaseGetHintStyleController
      write BaseSetHintStyleController;
    property IsDestroying: Boolean read FIsDestroying write FIsDestroying;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure BeginUpdate;
    procedure EndUpdate;
    class function GetDefaultHintStyleController: TcxCustomHintStyleController; virtual;
    function GetHintWindowClass: TcxCustomHintWindowClass; override;
    procedure RestoreDefaults; virtual;
  published
    property Animate: TcxHintAnimate read FAnimate write SetAnimate default cxhaAuto;
    property AnimationDelay: TcxHintAnimationDelay read FAnimationDelay write SetAnimationDelay default 100;
    property BorderColor: TColor read FBorderColor write SetBorderColor default clWindowFrame;
    property CallOutPosition: TcxCallOutPosition read FCallOutPosition write SetCallOutPosition default cxbpNone;
    property CaptionFont: TFont read FCaptionFont write SetCaptionFont;
    property Color: TColor read FColor write SetColor default clInfoBk;
    property Font: TFont read GetFont write SetFont;
    property Icon: TIcon read FIcon write SetIcon;
    property IconSize: TcxHintIconSize read FIconSize write SetIconSize default cxisDefault;
    property IconType: TcxHintIconType read FIconType write SetIconType default cxhiNone;
    property Rounded: Boolean read FRounded write SetRounded default False;
    property RoundRadius: Integer read FRoundRadius write SetRoundRadius default 11;
    property Standard: Boolean read FStandard write SetStandard default False;
  end;

  { TcxHintStyleController }

  TcxHintStyleController = class(TcxCustomHintStyleController)
  protected
    function GetHintStyleClass: TcxHintStyleClass; override;
  public
    function GetHintWidth(AHint: string): Integer;
    function GetHintHeight(AHint: string): Integer;
  published
    property Global;
    property HintStyleClassName;
    property HintStyle;
    property HintShortPause;
    property HintPause;
    property HintHidePause;
    property LookAndFeel;
    property UseHintControlLookAndFeel;
    property OnHintStyleChanged;
    property OnShowHint;
    property OnShowHintEx;
  end;

  { TcxHintViewInfo }

  TcxHintViewInfo = class
  private
    FCalculatedCallOutOffset: TPoint;
    FCalculatedCallOutPos: TcxCallOutPosition;
    FOwner: TcxHintWindow;
    FPainter: TcxCustomLookAndFeelPainter;
    // calculate without adjust in some inplace controls
    FContentOffset: TPoint;
    FWasAdjust: Boolean;
    //
    procedure CheckActivateRectSize(var ARect: TRect);
    function GetBorderColor: TColor;
    function GetCaption: string;
    function GetCaptionFont: TFont;
    function GetHint: string;
    function GetHintColor: TColor;
    function GetHintFont: TFont;
    function GetIconHorzOffset: Integer;
    function GetMaxCaptionWidth(AIsCaption: Boolean = True): Integer;
    function GetRoundRadius: Integer;
    function GetScaleFactor: TdxScaleFactor;
    procedure GetTextBounds(var ARect: TRect; const AText: string; AFont: TFont);
  protected
    FCallOutTops: array[0..2] of TPoint;
    FCallOutOrigin: TPoint;
    FCaptionBounds: TRect;
    FHintBounds: TRect;
    FHintWindowRect: TRect;
    FIconOrigin: TPoint;
    FIconSize: Integer;
    FIndentDelta: Integer;
    FMargins: TPoint;
    FMaxWidth: Integer;

    procedure AdjustHintPositionByHintBounds(var ARect: TRect); virtual;
    procedure AdjustHintPositionByPoint(const APoint: TPoint; var ARect: TRect); virtual;
    procedure CalculateCallOutPoints;
    procedure CalculateCaption;
    procedure CalculateHintBoundsOffset;
    procedure CalculateIcon; virtual;
    procedure CalculateMargins; virtual;
    procedure CalculateTextsBounds;
    procedure CalculateWindowRect;
    procedure CheckWindowRectSize;
    function CreateCallOutRegion(var R: TRect): HRGN;
    procedure NCPaint(ACanvas: TcxCanvas);
    function GetAutoCallOutPosition(const ARect: TRect): TcxCallOutPosition; virtual;
    function GetCallOutSize: Integer; virtual;
    function NeedCorrectByCursorSize: Boolean;
    procedure OffsetContent;

    property BorderColor: TColor read GetBorderColor;
    property CalculatedCallOutOffset: TPoint read FCalculatedCallOutOffset;
    property CalculatedCallOutPos: TcxCallOutPosition read FCalculatedCallOutPos;
    property CallOutSize: Integer read GetCallOutSize;
    property HintColor: TColor read GetHintColor;
    property HintWindowRect: TRect read FHintWindowRect;
    property Owner: TcxHintWindow read FOwner;
    property RoundRadius: Integer read GetRoundRadius;
    property ScaleFactor: TdxScaleFactor read GetScaleFactor;
  public
    constructor Create(AOwner: TcxHintWindow);

    procedure AdjustHintRect(var ARect: TRect);
    function Calculate(AMaxWidth: Integer): TRect; virtual;
    procedure Paint(ACanvas: TcxCanvas);
    procedure SetWindowRegion;

    property Caption: string read GetCaption;
    property CaptionBounds: TRect read FCaptionBounds;
    property CaptionFont: TFont read GetCaptionFont;
    property Hint: string read GetHint;
    property HintBounds: TRect read FHintBounds;
    property HintFont: TFont read GetHintFont;
    property IconOrigin: TPoint read FIconOrigin;
    property IconSize: Integer read FIconSize;
    property Margins: TPoint read FMargins;
  end;

  { TcxHintWindow }

  TcxHintWindow = class(TcxCustomHintWindow)
  private
    FAbsolutePosition: Boolean;
    FBorderColor: TColor;
    FBorderWidth: Integer;
    FCallOutPosition: TcxCallOutPosition;
    FCaptionFont: TFont;
    FHint: string;
    FHintColor: TColor;
    FIcon: TIcon;
    FIconSize: TcxHintIconSize;
    FIconType: TcxHintIconType;
    FRounded: Boolean;
    FRoundRadius: Integer;
    FScaleFactor: TdxScaleFactor;
    FViewInfo: TcxHintViewInfo;
    FWordWrap: Boolean;

    function GetAnimate: TcxHintAnimate;
    function InternalUseRightToLeftAlignment: Boolean;
    procedure SetAnimate(AValue: TcxHintAnimate);
    procedure SetIcon(Value: TIcon);
    procedure WMShowWindow(var Message: TWMShowWindow); message WM_SHOWWINDOW;
  protected
    procedure AdjustActivateRect(var ARect: TRect); override;
    procedure EnableRegion; override;

    procedure CalculateController; virtual;
    procedure CalculateIcon; virtual;
    procedure ChangeScale(M: Integer; D: Integer); override;
    procedure CreateBalloonForm; virtual;
    function CreateViewInfo: TcxHintViewInfo; virtual;
    function HasWindowRegion: Boolean; override;
    procedure LoadHintStyleData(const AHintData: IcxHint);
    procedure NCPaint(DC: HDC); override;
    procedure Paint; override;
    procedure ShowHint(X, Y: Integer; ACaption, AHint: string; AMaxWidth: Integer = 0); override;

    property AbsolutePosition: Boolean read FAbsolutePosition write FAbsolutePosition;
    property Hint: string read FHint;
    property ScaleFactor: TdxScaleFactor read FScaleFactor;
    property ViewInfo: TcxHintViewInfo read FViewInfo;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CalcHintRect(MaxWidth: Integer; const AHint: string; AData: Pointer): TRect; override;
    procedure ScaleForPPI(const PPI: Integer); reintroduce;

    property Animate: TcxHintAnimate read GetAnimate write SetAnimate; // obsolete
    property BorderColor: TColor read FBorderColor write FBorderColor;
    property CallOutPosition: TcxCallOutPosition read FCallOutPosition write FCallOutPosition;
    property CaptionFont: TFont read FCaptionFont write FCaptionFont;
    property Icon: TIcon read FIcon write SetIcon;
    property IconSize: TcxHintIconSize read FIconSize write FIconSize;
    property IconType: TcxHintIconType read FIconType write FIconType;
    property Rounded: Boolean read FRounded write FRounded;
    property RoundRadius: Integer read FRoundRadius write FRoundRadius;
    property WordWrap: Boolean read FWordWrap write FWordWrap;
  end;

implementation

uses
  Dialogs, Math, cxEditConsts, Themes, dxUxTheme, dxThemeManager, dxThemeConsts,
  cxEditUtils, cxExtEditUtils, dxDPIAwareUtils;

{ TcxHintStyle }

constructor TcxHintStyle.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFont := TFont.Create;
  FCaptionFont := TFont.Create;
  FIcon := TIcon.Create;
  FIcon.OnChange := IconChangeHandler;
  FModified := False;
  InternalRestoreDefault;
  HintStyleController := GetDefaultHintStyleController;
end;

destructor TcxHintStyle.Destroy;
begin
  FIsDestroying := True;
  if FHintStyleController <> nil then
    FHintStyleController.RemoveListener(Self);
  FreeAndNil(FIcon);
  FreeAndNil(FCaptionFont);
  FreeAndNil(FFont);
  inherited Destroy;
end;

procedure TcxHintStyle.Assign(Source: TPersistent);
begin
  if (Source is TcxHintStyle) then
  begin
    BeginUpdate;
    try
      with (Source as TcxHintStyle) do
      begin
        Self.Animate := Animate;
        Self.AnimationDelay := AnimationDelay;
        Self.BorderColor := BorderColor;
        Self.CallOutPosition := CallOutPosition;
        Self.CaptionFont.Assign(CaptionFont);
        Self.Color := Color;
        Self.HintStyleController := HintStyleController;
        Self.IconSize := IconSize;
        Self.IconType := IconType;
        Self.Rounded := Rounded;
        Self.RoundRadius := RoundRadius;
        Self.Standard := Standard;
        Self.Font.Assign(Font);
        Self.CaptionFont.Assign(CaptionFont);
        Self.Icon.Assign(Icon);
      end;
    finally
      EndUpdate;
    end
  end
  else
    inherited Assign(Source);
end;

procedure TcxHintStyle.InternalRestoreDefault;
var
  FRestoreFont: TFont;
begin
  FAnimate := cxhaAuto;
  FAnimationDelay := 100;
  FBorderColor := clWindowFrame;
  FCallOutPosition := cxbpNone;
  FColor := clInfoBk;
  FIconSize := cxisDefault;
  FIconType := cxhiNone;
  FRounded := False;
  FRoundRadius := 11;
  FStandard := False;
  FRestoreFont := TFont.Create;
  try
    FFont.Assign(FRestoreFont);
    FCaptionFont.Assign(FRestoreFont);
  finally
    FreeAndNil(FRestoreFont);
  end;
end;

procedure TcxHintStyle.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TcxHintStyle.EndUpdate;
begin
  if FUpdateCount <> 0 then
  begin
    Dec(FUpdateCount);
    if FModified then
      Changed;
  end;
end;

class function TcxHintStyle.GetDefaultHintStyleController: TcxCustomHintStyleController;
begin
  Result := nil;
end;

function TcxHintStyle.GetHintWindowClass: TcxCustomHintWindowClass;
begin
  Result := TcxHintWindow;
end;

procedure TcxHintStyle.RestoreDefaults;
begin
  BeginUpdate;
  try
    InternalRestoreDefault;
  finally
    EndUpdate;
  end;
end;

function TcxHintStyle.BaseGetHintStyleController: TcxCustomHintStyleController;
begin
  if FHintStyleController = GetDefaultHintStyleController then
    Result := nil
  else
    Result := FHintStyleController;
end;

procedure TcxHintStyle.BaseSetHintStyleController(Value: TcxCustomHintStyleController);

  function CheckHintStyleController(AHintStyleController: TcxCustomHintStyleController): Boolean;
  var
    AOwner: TPersistent;
  begin
    Result := False;
    AOwner := GetOwner;
    while AOwner <> nil do
    begin
      if (AOwner is TcxCustomHintStyleController) and (AOwner = AHintStyleController) then
        Exit;
      AOwner := GetPersistentOwner(AOwner);
    end;
    Result := True;
  end;

begin
  if Value = nil then
    Value := GetDefaultHintStyleController;

  if (Value <> nil) and (not CheckHintStyleController(Value)) then Exit;

  if Value <> FHintStyleController then
  begin
    if FHintStyleController <> nil then
      FHintStyleController.RemoveListener(Self);
    FHintStyleController := Value;
    if FHintStyleController <> nil then
      FHintStyleController.AddListener(Self);
    HintStyleControllerChanged;
  end;
end;

procedure TcxHintStyle.Changed;
begin
  if FUpdateCount = 0 then
  begin
    if Assigned(OnChanged) and not IsDestroying then
      OnChanged(Self);
    FModified := False;
  end
  else
    FModified := True;
end;

procedure TcxHintStyle.ControllerChangedNotification(AStyleController: TcxCustomHintStyleController);
begin
  Changed;
end;

procedure TcxHintStyle.ControllerFreeNotification(AHintStyleController: TcxCustomHintStyleController);
begin
  if (AHintStyleController <> nil) and (AHintStyleController = FHintStyleController) then
    HintStyleController := nil;
end;

procedure TcxHintStyle.HintStyleControllerChanged;
begin
  Changed;
end;

function TcxHintStyle.GetAnimate: TcxHintAnimate;
begin
  Result := Animate;
end;

function TcxHintStyle.GetAnimationDelay: TcxHintAnimationDelay;
begin
  Result := AnimationDelay;
end;

function TcxHintStyle.GetBorderColor: TColor;
begin
  Result := BorderColor;
end;

function TcxHintStyle.GetCallOutPosition: TcxCallOutPosition;
begin
  Result := CallOutPosition;
end;

function TcxHintStyle.GetColor: TColor;
begin
  Result := Color;
end;

function TcxHintStyle.GetIconSize: TcxHintIconSize;
begin
  Result := IconSize;
end;

function TcxHintStyle.GetIconType: TcxHintIconType;
begin
  Result := IconType;
end;

function TcxHintStyle.GetHintCaption: string;
begin
  Result := '';// not used
end;

function TcxHintStyle.GetRounded: Boolean;
begin
  Result := Rounded;
end;

function TcxHintStyle.GetRoundRadius: Integer;
begin
  Result := RoundRadius;
end;

function TcxHintStyle.GetStandard: Boolean;
begin
  Result := Standard;
end;

function TcxHintStyle.GetHintFont: TFont;
begin
  Result := Font;
end;

function TcxHintStyle.GetHintCaptionFont: TFont;
begin
  Result := CaptionFont;
end;

function TcxHintStyle.GetHintIcon: TIcon;
begin
  Result := Icon;
end;

procedure TcxHintStyle.SetHintCaption(Value: string);
begin
  // do nothing
end;

function TcxHintStyle.GetFont: TFont;
begin
  Result := FFont;
end;

procedure TcxHintStyle.SetAnimate(Value: TcxHintAnimate);
begin
  if Value <> FAnimate then
  begin
    FAnimate := Value;
    Changed;
  end;
end;

procedure TcxHintStyle.SetAnimationDelay(Value: TcxHintAnimationDelay);
begin
  if Value <> FAnimationDelay then
  begin
    FAnimationDelay := Value;
    Changed;
  end;
end;

procedure TcxHintStyle.SetCallOutPosition(Value: TcxCallOutPosition);
begin
  if Value <> FCallOutPosition then
  begin
    FCallOutPosition := Value;
    Changed;
  end;
end;

procedure TcxHintStyle.SetBorderColor(Value: TColor);
begin
  if Value <> FBorderColor then
  begin
    FBorderColor := Value;
    Changed;
  end;
end;

procedure TcxHintStyle.SetColor(Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Changed;
  end;
end;

procedure TcxHintStyle.SetCaptionFont(Value: TFont);
begin
  FCaptionFont.Assign(Value);
  Changed;
end;

procedure TcxHintStyle.SetFont(Value: TFont);
begin
  FFont.Assign(Value);
  Changed;
end;

procedure TcxHintStyle.SetIconSize(Value: TcxHintIconSize);
begin
  if FIconSize <> Value then
  begin
    FIconSize := Value;
    Changed;
  end;
end;

procedure TcxHintStyle.SetIconType(Value: TcxHintIconType);
begin
  if FIconType <> Value then
  begin
    FIconType := Value;
    Changed;
  end;
end;

procedure TcxHintStyle.SetIcon(Value: TIcon);
begin
  if FIcon <> Value then
  begin
    FIcon.Assign(Value);
    Changed;
  end;
end;

procedure TcxHintStyle.SetRounded(Value: Boolean);
begin
  if FRounded <> Value then
  begin
    FRounded := Value;
    Changed;
  end;
end;

procedure TcxHintStyle.SetRoundRadius(Value: Integer);
begin
  if FRoundRadius <> Value then
  begin
    FRoundRadius := Value;
    Changed;
  end;
end;

procedure TcxHintStyle.SetStandard(Value: Boolean);
begin
  if FStandard <> Value then
  begin
    FStandard := Value;
    Changed;
  end;
end;

procedure TcxHintStyle.IconChangeHandler(Sender: TObject);
begin
  Changed;
end;

{ TcxHintStyleController }

function TcxHintStyleController.GetHintStyleClass: TcxHintStyleClass;
begin
  Result := inherited GetHintStyleClass;
  if Result = nil then
    Result := TcxHintStyle;
end;

function TcxHintStyleController.GetHintWidth(AHint: string): Integer;
var
  R: TRect;
begin
  R := HintWindow.CalcHintRect(Screen.Width, AHint, nil);
  Result := R.Right - R.Left;
end;

function TcxHintStyleController.GetHintHeight(AHint: string): Integer;
var
  R: TRect;
begin
  R := HintWindow.CalcHintRect(Screen.Width, AHint, nil);
  Result := R.Bottom - R.Top;
end;

{ TcxHintViewInfo }

constructor TcxHintViewInfo.Create(AOwner: TcxHintWindow);
begin
  inherited Create;
  FOwner := AOwner;
  FCalculatedCallOutPos := cxbpNone;
end;

procedure TcxHintViewInfo.SetWindowRegion;
var
  R: TRect;
  ACallOutRegion: HRGN;
  ARegion: HRGN;
begin
  R := FHintWindowRect;
  ACallOutRegion := CreateCallOutRegion(R);
  ARegion := CreateRoundRectRgn(R.Left, R.Top, R.Right, R.Bottom, Owner.RoundRadius, Owner.RoundRadius);
  if ACallOutRegion <> 0 then
    CombineRgn(ARegion, ARegion, ACallOutRegion, RGN_OR);
  OffsetRgn(ARegion, 1, 1);
  SetWindowRgn(Owner.Handle, ARegion, True);
  if ACallOutRegion <> 0 then
    DeleteObject(ACallOutRegion);
end;

procedure TcxHintViewInfo.CheckActivateRectSize(var ARect: TRect);
var
  ARight: Integer;
  ABottom: Integer;
begin
  ABottom := Max(FHintWindowRect.Bottom, FCallOutOrigin.Y);
  ARight := Max(FHintWindowRect.Right, FCallOutOrigin.X);
  if ABottom > cxRectHeight(ARect) then
    ARect := cxRectSetHeight(ARect, ABottom);
  if ARight > cxRectWidth(ARect) then
    ARect := cxRectSetWidth(ARect, ARight);
end;

function TcxHintViewInfo.GetBorderColor: TColor;
begin
  if FPainter.GetHintBorderColor <> clDefault then
    Result := FPainter.GetHintBorderColor
  else
    Result := Owner.FBorderColor;
end;

procedure TcxHintViewInfo.AdjustHintRect(var ARect: TRect);
var
  P: TPoint;
begin
  FWasAdjust := True;
  if not cxRectIsEmpty(Owner.HintAreaBounds) then
  begin
    FCalculatedCallOutPos := GetAutoCallOutPosition(Owner.HintAreaBounds);
    if Owner.UseMousePos then
    begin
      P := GetMouseCursorPos;
      if FCalculatedCallOutPos in [cxbpTopLeft, cxbpTopRight] then
        Inc(P.Y, cxGetCursorSize.cy);
      AdjustHintPositionByPoint(P, ARect);
    end
    else
      AdjustHintPositionByHintBounds(ARect);
  end
  else
  begin
    FCalculatedCallOutPos := GetAutoCallOutPosition(ARect);
    P := ARect.TopLeft;
    if Owner.InternalUseRightToLeftAlignment then
      P.X := ARect.Right;
    if NeedCorrectByCursorSize then
      Dec(P.Y, cxGetCursorSize.cy);
    AdjustHintPositionByPoint(P, ARect);
  end;
end;

function TcxHintViewInfo.Calculate(AMaxWidth: Integer): TRect;
begin
  FWasAdjust := False;
  FPainter := Owner.GetPainter;
  FCalculatedCallOutPos := Owner.CallOutPosition;
  FMaxWidth := AMaxWidth;
  CalculateMargins;
  CalculateIcon;
  CalculateTextsBounds;
  CalculateCaption;
  CalculateHintBoundsOffset;
  CalculateWindowRect;
  Result := FHintWindowRect;
end;

procedure TcxHintViewInfo.Paint(ACanvas: TcxCanvas);

  procedure DrawHintBackground;
  begin
    FPainter.DrawHintBackground(ACanvas, Owner.ClientRect, HintColor);
  end;

  procedure DrawHintBorder;
  var
    ActualRgn: HRGN;
  begin
    ActualRgn := CreateRectRgnIndirect(Rect(0, 0, 0, 0));
    try
      GetWindowRgn(Owner.Handle, ActualRgn);
      OffsetRgn(ActualRgn, -Owner.FBorderWidth, -Owner.FBorderWidth);
      ACanvas.FrameRegion(ActualRgn, BorderColor);
    finally
      DeleteObject(ActualRgn);
    end;
  end;

  procedure DrawHintIcon;
  var
    R: TRect;
  begin
    if not Owner.Icon.Empty then
    begin
      R := cxRectBounds(FIconOrigin, IconSize, IconSize);
      if Owner.InternalUseRightToLeftAlignment then
        R := TdxRightToLeftLayoutConverter.ConvertRect(R, FHintWindowRect);
      DrawIconEx(ACanvas.Handle, R.Left, R.Top, Owner.Icon.Handle, IconSize, IconSize, 0, 0, DI_NORMAL);
    end;
  end;

  procedure InternalDrawHintText(ABounds: TRect; const AText: string; AFont: TFont; AColor: TColor; AFlags: Cardinal);
  begin
    ACanvas.Font.Assign(AFont);
    if AColor <> clDefault then
      ACanvas.Font.Color := AColor;
    if Owner.BiDiMode <> bdLeftToRight then
      AFlags := AFlags or DT_RTLREADING;
    DrawText(ACanvas.Handle, PChar(AText), Length(AText), ABounds, AFlags);
  end;

  procedure DrawHintText;
  var
    R: TRect;
  begin
    ACanvas.Brush.Style := bsClear;
    if Caption <> '' then
    begin
      R := FCaptionBounds;
      if Owner.InternalUseRightToLeftAlignment then
        R := TdxRightToLeftLayoutConverter.ConvertRect(R, FHintWindowRect);
      InternalDrawHintText(R, Caption, CaptionFont, FPainter.ScreenTipGetTitleTextColor, DT_WORDBREAK or DT_NOPREFIX or DT_VCENTER);
    end;

    InternalDrawHintText(FHintBounds, Hint, HintFont, FPainter.ScreenTipGetDescriptionTextColor, DT_WORDBREAK or DT_NOPREFIX);
    ACanvas.Brush.Style := bsSolid;
  end;

begin
  DrawHintBackground;
  if Owner.HasWindowRegion then
    DrawHintBorder;
  if not FWasAdjust then
    OffsetContent;
  DrawHintIcon;
  DrawHintText;
end;

function TcxHintViewInfo.CreateCallOutRegion(var R: TRect): HRGN;
begin
  if CalculatedCallOutPos = cxbpNone then
    Result := 0
  else
    Result := CreatePolygonRgn(FCallOutTops, 3, WINDING);
end;

procedure TcxHintViewInfo.NCPaint(ACanvas: TcxCanvas);
begin
  ACanvas.FrameRect(Rect(0, 0, Owner.Width, Owner.Height), BorderColor);
end;

function TcxHintViewInfo.GetAutoCallOutPosition(const ARect: TRect): TcxCallOutPosition;
var
  ACursorPos: TPoint;
  AMonitorRect: TRect;
begin
  Result := Owner.CallOutPosition;
  if Owner.InternalUseRightToLeftAlignment then
    case Result of
      cxbpLeftBottom:
        Result := cxbpRightBottom;
      cxbpLeftTop:
        Result := cxbpRightTop;
      cxbpTopLeft:
        Result := cxbpTopRight;
      cxbpTopRight:
        Result := cxbpTopLeft;
      cxbpRightBottom:
        Result := cxbpLeftBottom;
      cxbpRightTop:
        Result := cxbpLeftTop;
      cxbpBottomRight:
        Result := cxbpBottomLeft;
      cxbpBottomLeft:
        Result := cxbpBottomRight;
    end;
  if Result = cxbpAuto then
  begin
    ACursorPos := GetMouseCursorPos;
    AMonitorRect := Screen.MonitorFromPoint(ACursorPos).BoundsRect;
    if ACursorPos.Y < ((AMonitorRect.Bottom + AMonitorRect.Top) div 2) then
    begin
      if ACursorPos.X < ((AMonitorRect.Right + AMonitorRect.Left) div 2) then
        Result := cxbpTopLeft
      else
        Result := cxbpTopRight;
    end
    else
    begin
      if ACursorPos.X  < ((AMonitorRect.Right + AMonitorRect.Left) div 2) then
        Result := cxbpBottomLeft
      else
        Result := cxbpBottomRight;
    end;
  end;
end;

function TcxHintViewInfo.GetCallOutSize: Integer;
begin
  Result := ScaleFactor.Apply(15);
end;

function TcxHintViewInfo.NeedCorrectByCursorSize: Boolean;
begin
  Result := not Owner.AbsolutePosition and not (CalculatedCallOutPos in [cxbpNone, cxbpTopLeft, cxbpTopRight]);
end;

procedure TcxHintViewInfo.OffsetContent;
begin
  FHintWindowRect := cxRectOffset(FHintWindowRect, FContentOffset);
  FCaptionBounds := cxRectOffset(FCaptionBounds, FContentOffset);
  FHintBounds := cxRectOffset(FHintBounds, FContentOffset);
  FIconOrigin := cxPointOffset(FIconOrigin, FContentOffset);
end;

procedure TcxHintViewInfo.CalculateIcon;
var
  AIcon: TIcon;
begin
  AIcon := Owner.Icon;
  if AIcon.Empty then
    FIconSize := 0
  else
    case Owner.IconSize of
      cxisLarge:
        FIconSize := 32;
      cxisSmall:
        FIconSize := 16;
      else
      begin
        AIcon.Handle; //ensure HandleAllocated
        FIconSize := AIcon.Width;
      end;
    end;

  FIconSize := ScaleFactor.Apply(FIconSize);
end;

procedure TcxHintViewInfo.AdjustHintPositionByHintBounds(var ARect: TRect);
var
  ATextOffset: Integer;
  P: TPoint;
begin
  with Owner.HintAreaBounds do
  begin
    ATextOffset := ScaleFactor.Apply(cxTextOffset);
    case CalculatedCallOutPos of
      cxbpLeftBottom:
        P := cxPoint(Right - ATextOffset, Top + ATextOffset);
      cxbpLeftTop:
        P := cxPoint(Right - ATextOffset, Bottom - ATextOffset);
      cxbpTopLeft:
        P := cxPoint(Left + ATextOffset, Bottom - ATextOffset);
      cxbpTopRight:
        P := cxPoint(Right - ATextOffset, Bottom - ATextOffset);
      cxbpRightBottom:
        P := cxPoint(Left + ATextOffset, Top + ATextOffset);
      cxbpRightTop:
        P := cxPoint(Left + ATextOffset, Bottom - ATextOffset);
      cxbpBottomRight:
        P := cxPoint(Right - ATextOffset, Top + ATextOffset);
      cxbpBottomLeft:
        P := cxPoint(Left + ATextOffset, Top + ATextOffset);
    end;
  end;
  AdjustHintPositionByPoint(P, ARect);
end;

procedure TcxHintViewInfo.AdjustHintPositionByPoint(const APoint: TPoint; var ARect: TRect);
var
  AMonitorRect: TRect;
  ACalloutOffset: TPoint;
begin
  CheckWindowRectSize;
  CalculateCallOutPoints;
  CheckActivateRectSize(ARect);
  ACalloutOffset := cxPoint(CallOutSize + CalculatedCallOutOffset.X, CallOutSize + CalculatedCallOutOffset.Y);
  case CalculatedCallOutPos of
    cxbpLeftBottom:
      begin
        ARect := cxRectSetLeft(ARect, APoint.X);
        ARect := cxRectSetBottom(ARect, APoint.Y + ACalloutOffset.Y);
      end;
    cxbpLeftTop:
      begin
        ARect := cxRectSetLeft(ARect, APoint.X);
        ARect := cxRectSetTop(ARect, APoint.Y - ACalloutOffset.Y);
      end;
    cxbpTopLeft:
      begin
        ARect := cxRectSetTop(ARect, APoint.Y);
        ARect := cxRectSetLeft(ARect, APoint.X - ACalloutOffset.X);
      end;
    cxbpTopRight:
      begin
        ARect := cxRectSetTop(ARect, APoint.Y);
        ARect := cxRectSetRight(ARect, APoint.X + ACalloutOffset.X);
      end;
    cxbpRightBottom:
      begin
        ARect := cxRectSetRight(ARect, APoint.X);
        ARect := cxRectSetBottom(ARect, APoint.Y + ACalloutOffset.Y);
      end;
    cxbpRightTop:
      begin
        ARect := cxRectSetRight(ARect, APoint.X);
        ARect := cxRectSetTop(ARect, APoint.Y - ACalloutOffset.Y);
      end;
    cxbpBottomRight:
      begin
        ARect := cxRectSetBottom(ARect, APoint.Y);
        ARect := cxRectSetRight(ARect, APoint.X + ACalloutOffset.X);
      end;
    cxbpBottomLeft:
      begin
        ARect := cxRectSetBottom(ARect, APoint.Y);
        ARect := cxRectSetLeft(ARect, APoint.X - ACalloutOffset.X);
      end;
  end;
  if CalculatedCallOutPos <> cxbpNone then
  begin
    AMonitorRect := Screen.MonitorFromPoint(APoint).BoundsRect;
    if ARect.Left < AMonitorRect.Left then
      ARect := cxRectSetLeft(ARect, AMonitorRect.Left);
    if ARect.Right > AMonitorRect.Right then
      ARect := cxRectSetRight(ARect, AMonitorRect.Right);
    if ARect.Top < AMonitorRect.Top then
      ARect := cxRectSetTop(ARect, AMonitorRect.Top);
    if ARect.Bottom > AMonitorRect.Bottom then
      ARect := cxRectSetBottom(ARect, AMonitorRect.Bottom);
  end;
end;

procedure TcxHintViewInfo.CalculateCallOutPoints;
begin
  FContentOffset := cxNullPoint;
  case CalculatedCallOutPos of
    cxbpLeftBottom..cxbpLeftTop:
      FContentOffset.X := CallOutSize;
    cxbpTopLeft..cxbpTopRight:
      FContentOffset.Y := CallOutSize;
  end;
  OffsetContent;
  case CalculatedCallOutPos of
    cxbpLeftBottom:
      begin
        FCallOutTops[0] := Point(HintWindowRect.Left, HintWindowRect.Bottom - CallOutSize);
        FCallOutTops[1] := Point(HintWindowRect.Left, HintWindowRect.Bottom - CallOutSize * 2);
        FCallOutTops[2] := Point(HintWindowRect.Left - CallOutSize, HintWindowRect.Bottom - CallOutSize);
        cxPointsOffset(FCallOutTops, 0, -FCalculatedCallOutOffset.Y);
     end;
    cxbpLeftTop:
      begin
        FCallOutTops[0] := Point(HintWindowRect.Left, HintWindowRect.Top + CallOutSize);
        FCallOutTops[1] := Point(HintWindowRect.Left, HintWindowRect.Top + CallOutSize * 2);
        FCallOutTops[2] := Point(HintWindowRect.Left - CallOutSize, HintWindowRect.Top + CallOutSize);
        cxPointsOffset(FCallOutTops, 0, FCalculatedCallOutOffset.Y);
      end;
    cxbpTopRight:
      begin
        FCallOutTops[0] := Point(HintWindowRect.Right - CallOutSize, HintWindowRect.Top);
        FCallOutTops[1] := Point(HintWindowRect.Right - CallOutSize * 2, HintWindowRect.Top);
        FCallOutTops[2] := Point(HintWindowRect.Right - CallOutSize, HintWindowRect.Top - CallOutSize);
        cxPointsOffset(FCallOutTops, -FCalculatedCallOutOffset.X, 0);
      end;
    cxbpTopLeft:
      begin
        FCallOutTops[0] := Point(HintWindowRect.Left + CallOutSize, HintWindowRect.Top);
        FCallOutTops[1] := Point(HintWindowRect.Left + CallOutSize * 2, HintWindowRect.Top);
        FCallOutTops[2] := Point(HintWindowRect.Left + CallOutSize, HintWindowRect.Top - CallOutSize);
        cxPointsOffset(FCallOutTops, FCalculatedCallOutOffset.X, 0);
      end;
    cxbpRightBottom:
      begin
        FCallOutTops[0] := Point(HintWindowRect.Right - 1, HintWindowRect.Bottom - CallOutSize);
        FCallOutTops[1] := Point(HintWindowRect.Right - 1, HintWindowRect.Bottom - CallOutSize * 2);
        FCallOutTops[2] := Point(HintWindowRect.Right + CallOutSize, HintWindowRect.Bottom - CallOutSize);
        cxPointsOffset(FCallOutTops, 0, -FCalculatedCallOutOffset.Y);
      end;
    cxbpRightTop:
      begin
        FCallOutTops[0] := Point(HintWindowRect.Right - 1, HintWindowRect.Top + CallOutSize);
        FCallOutTops[1] := Point(HintWindowRect.Right - 1, HintWindowRect.Top + CallOutSize * 2);
        FCallOutTops[2] := Point(HintWindowRect.Right + CallOutSize, HintWindowRect.Top + CallOutSize);
        cxPointsOffset(FCallOutTops, 0, FCalculatedCallOutOffset.Y);
      end;
    cxbpBottomRight:
      begin
        FCallOutTops[0] := Point(HintWindowRect.Right - CallOutSize, HintWindowRect.Bottom - 1);
        FCallOutTops[1] := Point(HintWindowRect.Right - CallOutSize * 2, HintWindowRect.Bottom - 1);
        FCallOutTops[2] := Point(HintWindowRect.Right - CallOutSize, HintWindowRect.Bottom + CallOutSize);
        cxPointsOffset(FCallOutTops, -FCalculatedCallOutOffset.X, 0);
      end;
    cxbpBottomLeft:
      begin
        FCallOutTops[0] := Point(HintWindowRect.Left + CallOutSize, HintWindowRect.Bottom - 1);
        FCallOutTops[1] := Point(HintWindowRect.Left + CallOutSize * 2, HintWindowRect.Bottom - 1);
        FCallOutTops[2] := Point(HintWindowRect.Left + CallOutSize, HintWindowRect.Bottom + CallOutSize);
        cxPointsOffset(FCallOutTops, FCalculatedCallOutOffset.X, 0);
      end;
  end;
  FCallOutOrigin := FCallOutTops[2];
 end;

procedure TcxHintViewInfo.CalculateCaption;
var
  X: Integer;
begin
  if Caption = '' then Exit;
  X := Margins.X;
  if IconSize > 0 then
    Inc(X, FIndentDelta + IconSize);
  OffsetRect(FCaptionBounds, X, Margins.Y);
end;

procedure TcxHintViewInfo.CalculateHintBoundsOffset;
var
  Y: Integer;
begin
  Y := Margins.Y;
  if Caption <> '' then
    Y := Max(Y, CaptionBounds.Bottom + FIndentDelta);
  if IconSize > 0 then
    Y := Max(Y, IconOrigin.Y + IconSize + FIndentDelta);
  OffsetRect(FHintBounds, Margins.X, Y);
end;

procedure TcxHintViewInfo.CalculateMargins;
begin
  FIndentDelta := ScaleFactor.Apply(6);
  FMargins.X := Max(FIndentDelta, RoundRadius div 2);
  FMargins.Y := FMargins.X;
  FIconOrigin := Margins;
end;

procedure TcxHintViewInfo.CalculateTextsBounds;
begin
  if Caption = '' then
  begin
    FCaptionBounds := cxEmptyRect;
    FHintBounds := Rect(0, 0, GetMaxCaptionWidth, 1);
  end
  else
  begin
    FCaptionBounds := Rect(0, 0, GetMaxCaptionWidth, 1);
    GetTextBounds(FCaptionBounds, Caption, CaptionFont);
    FHintBounds := Rect(0, 0, GetMaxCaptionWidth(False), 1);
  end;
  if Hint = '' then
    FHintBounds := cxEmptyRect
  else
    GetTextBounds(FHintBounds, Hint, HintFont);
end;

procedure TcxHintViewInfo.CalculateWindowRect;
var
  AWidth, AHeight: Integer;
begin
  AWidth := HintBounds.Right + Margins.X;
  if Caption <> '' then
    AWidth := Max(AWidth, CaptionBounds.Right + Margins.X)
  else
    if IconSize > 0 then
      AWidth := Max(AWidth, IconOrigin.X + IconSize + Margins.X);
  AHeight := HintBounds.Bottom + Margins.Y;
  FHintWindowRect := Rect(0, 0, AWidth, AHeight);
end;

procedure TcxHintViewInfo.CheckWindowRectSize;
var
  AWidth, AHeight, AStraightSize: Integer;
begin
  if CalculatedCallOutPos = cxbpNone then Exit;
  FCalculatedCallOutOffset := cxNullPoint;
  AWidth := cxRectWidth(FHintWindowRect);
  AHeight := cxRectHeight(FHintWindowRect);
  if CalculatedCallOutPos in [cxbpTopLeft, cxbpTopRight, cxbpBottomLeft, cxbpBottomRight] then
  begin
    AStraightSize := Max(AWidth - 2 * RoundRadius, CallOutSize);
    if AStraightSize < 3 * CallOutSize then
      FCalculatedCallOutOffset.X := (AStraightSize - 3 * CallOutSize) div 2 + RoundRadius;
    AWidth := Max(AWidth, AStraightSize + 2 * RoundRadius);
  end
  else
  begin
    AStraightSize := Max(AHeight - 2 * RoundRadius, CallOutSize);
    if AStraightSize < 3 * CallOutSize then
      FCalculatedCallOutOffset.Y := (AStraightSize - 3 * CallOutSize) div 2 + RoundRadius;
    AHeight := Max(AHeight, AStraightSize + 2 * RoundRadius);
  end;
  FHintWindowRect.Right := AWidth;
  FHintWindowRect.Bottom := AHeight;
end;

procedure TcxHintViewInfo.GetTextBounds(var ARect: TRect; const AText: string; AFont: TFont);
begin
  cxGetTextRect(ARect, AText, AFont, DT_WORDBREAK or DT_NOPREFIX);
end;

function TcxHintViewInfo.GetIconHorzOffset: Integer;
begin
  if IconSize > 0 then
    Result := FIndentDelta
  else
    Result := 0;
end;

function TcxHintViewInfo.GetMaxCaptionWidth(AIsCaption: Boolean = True): Integer;
var
  ADec: Integer;
begin
  Result := FMaxWidth;
  if Result <= 0 then
  begin
    Result := cxMaxRectSize;
    Exit;
  end;
  ADec := GetIconHorzOffset + 2 * Margins.X + FIndentDelta;
  if AIsCaption then
    Inc(ADec, IconSize);
  Dec(Result, ADec);
end;

function TcxHintViewInfo.GetRoundRadius: Integer;
begin
  if Owner.Rounded then
    Result := Owner.RoundRadius
  else
    Result := 0;
end;

function TcxHintViewInfo.GetScaleFactor: TdxScaleFactor;
begin
  Result := Owner.ScaleFactor;
end;

function TcxHintViewInfo.GetCaption: string;
begin
  Result := Owner.Caption;
end;

function TcxHintViewInfo.GetCaptionFont: TFont;
begin
  Result := Owner.CaptionFont;
end;

function TcxHintViewInfo.GetHint: string;
begin
  Result := Owner.FHint;
end;

function TcxHintViewInfo.GetHintColor: TColor;
begin
  Result := Owner.FHintColor
end;

function TcxHintViewInfo.GetHintFont: TFont;
begin
  Result := Owner.Font;
end;

{ TcxHintWindow }

constructor TcxHintWindow.Create(AOwner: TComponent);
begin
  inherited;
  FScaleFactor := TdxScaleFactor.Create;
  FScaleFactor.Assign(dxGetSystemDPI, dxDefaultDPI);
  FCallOutPosition := cxbpNone;
  Caption := '';
  Color := clInfoBk;
  FHintColor := clInfoBk;
  FBorderColor := clWindowFrame;
  FRounded := False;
  FRoundRadius := 11;
  FIconType := cxhiQuestion;
  FStandardHint := True;
  FWordWrap := True;
  FCaptionFont := TFont.Create;
  FCaptionFont.Assign(Font);
  FCaptionFont.Style := FCaptionFont.Style + [fsBold];
  FIcon := TIcon.Create;

  BorderStyle := bsSingle;
  FBorderWidth := 1;// bsSingle
  FViewInfo := CreateViewInfo;
end;

destructor TcxHintWindow.Destroy;
begin
  FreeAndNil(FIcon);
  FreeAndNil(FCaptionFont);
  FreeAndNil(FViewInfo);
  FreeAndNil(FScaleFactor);
  inherited Destroy;
end;

procedure TcxHintWindow.SetIcon(Value: TIcon);
begin
  FIcon.Assign(Value);
end;

function TcxHintWindow.CalcHintRect(MaxWidth: Integer; const AHint: string; AData: Pointer): TRect;
begin
  CalculateController;
  if StandardHint then
  begin
    Canvas.Font.Assign(Screen.HintFont);
    Result := inherited CalcHintRect(MaxWidth, AHint, AData);
  end
  else
  begin
    FHint := AHint;
    CalculateIcon;
    Result := ViewInfo.Calculate(MaxWidth);
  end;
end;

procedure TcxHintWindow.ScaleForPPI(const PPI: Integer);
begin
  ChangeScale(PPI, ScaleFactor.TargetDPI);
  ScaleFactor.Assign(PPI, dxDefaultDPI);
end;

procedure TcxHintWindow.WMShowWindow(var Message: TWMShowWindow);
begin
  inherited;
  if not Message.Show then
    cxRemoveHintedControl;
end;

procedure TcxHintWindow.NCPaint(DC: HDC);
begin
  if StandardHint or HasWindowRegion then
    inherited NCPaint(DC)
  else
  begin
    cxPaintCanvas.BeginPaint(DC);
    try
      FViewInfo.NCPaint(cxPaintCanvas);
    finally
      cxPaintCanvas.EndPaint;
    end;
  end;
end;

procedure TcxHintWindow.Paint;
begin
  if StandardHint then
  begin
    DisableRegion;
    Canvas.Brush.Style := bsClear;
    inherited Paint;
  end
  else
    FViewInfo.Paint(cxCanvas);
end;

procedure TcxHintWindow.CalculateController;

  function CheckControlData(out AHintData: IcxHint): Boolean;
  begin
    Result := Supports(cxGetHintedControl, IcxHint, AHintData);
    if Result then
      Caption := AHintData.GetHintCaption;
  end;

  function CheckControllerData(out AHintData: IcxHint): Boolean;
  var
    AController: TcxCustomHintStyleController;
  begin
    AController := cxGetHintStyleController;
    Result := (AController <> nil) and Supports(AController.HintStyle, IcxHint, AHintData);
    if Result then
      Caption := AController.HintWindow.Caption;
  end;

var
  AHintData: IcxHint;
begin
  if CheckControlData(AHintData) or CheckControllerData(AHintData) then
    LoadHintStyleData(AHintData)
  else
    StandardHint := True;
end;

function TcxHintWindow.GetAnimate: TcxHintAnimate;
begin
  Result := AnimationStyle;
end;

function TcxHintWindow.InternalUseRightToLeftAlignment: Boolean;
begin
  Result := SysLocale.MiddleEast and (BiDiMode = bdRightToLeft);
end;

procedure TcxHintWindow.SetAnimate(AValue: TcxHintAnimate);
begin
  AnimationStyle := AValue;
end;

procedure TcxHintWindow.LoadHintStyleData(const AHintData: IcxHint);
var
  ADefaultFont: TFont;
begin
  StandardHint := AHintData.GetStandard;
  if StandardHint then Exit;
  Animate := AHintData.GetAnimate;
  AnimationDelay := AHintData.GetAnimationDelay;
  FCallOutPosition := AHintData.GetCallOutPosition;
  FBorderColor := AHintData.GetBorderColor;
  FHintColor := AHintData.GetColor;
  FIconSize := AHintData.GetIconSize;
  FIconType := AHintData.GetIconType;

  FRounded := AHintData.GetRounded;
  if FRounded then
    FRoundRadius := AHintData.GetRoundRadius
  else
    FRoundRadius := 0;

  if Assigned(AHintData.GetHintIcon) then
    FIcon.Assign(AHintData.GetHintIcon)
  else
    FreeAndNil(FIcon);

  ADefaultFont := TFont.Create;
  try
    if Assigned(AHintData.GetHintFont) then
      Font.Assign(AHintData.GetHintFont)
    else
      Font.Assign(ADefaultFont);

    if Assigned(AHintData.GetHintCaptionFont) then
      FCaptionFont.Assign(AHintData.GetHintCaptionFont)
    else
      FCaptionFont.Assign(ADefaultFont);

    CaptionFont.Height := ScaleFactor.Apply(CaptionFont.Height);
    Font.Height := ScaleFactor.Apply(Font.Height);
  finally
    FreeAndNil(ADefaultFont);
  end;
end;

procedure TcxHintWindow.ShowHint(X, Y: Integer; ACaption, AHint: string; AMaxWidth: Integer = 0);
var
  R: TRect;
begin
  ScaleForPPI(dxGetMonitorDPI(Point(X, Y)));

  Caption := ACaption;
  if AMaxWidth = 0 then
    AMaxWidth := Screen.Width;

  R := CalcHintRect(AMaxWidth, AHint, nil);
  OffsetRect(R, X, Y);
  try
    AbsolutePosition := True;
    ActivateHint(R, AHint);
  finally
    AbsolutePosition := False;
  end;
end;

function TcxHintWindow.CreateViewInfo: TcxHintViewInfo;
begin
  Result := TcxHintViewInfo.Create(Self);
end;

function TcxHintWindow.HasWindowRegion: Boolean;
begin
  Result := (ViewInfo.CalculatedCallOutPos <> cxbpNone) or Rounded;
end;

procedure TcxHintWindow.CalculateIcon;
type
  TcxRealHintIconType = (IDIAPPLICATION, IDIINFORMATION, IDIWARNING, IDIERROR, IDIQUESTION, IDIWINLOGO);
const
  LI_LARGE = 1;
  LI_SMALL = 0;
  RealIconTypes: array[TcxRealHintIconType] of MakeIntResource = (
    IDI_APPLICATION, IDI_INFORMATION, IDI_WARNING, IDI_ERROR, IDI_QUESTION, IDI_WINLOGO
  );
  IconSize: array[TcxHintIconSize] of Integer = (LI_LARGE, LI_LARGE, LI_SMALL);

  function InternalLoadIcon(AIconID: MakeIntResource; out AHandle: HICON): Boolean;
  begin
    Result := Succeeded(LoadIconMetric(0, MakeIntResourceW(AIconID), IconSize[FIconSize], AHandle));
  end;

var
  AHandle: HICON;
  AIconID: MakeIntResource;
begin
  case FIconType of
    cxhiCustom:
      Exit;
    cxhiCurrentApplication:
      FIcon.Assign(Application.Icon);
    cxhiNone:
      if Assigned(FIcon) and not FIcon.Empty then
      begin
        FreeAndNil(FIcon);
        FIcon := TIcon.Create;
      end;
  else
    begin
      AIconID := RealIconTypes[TcxRealHintIconType(Ord(FIconType) - 1)];
      if InternalLoadIcon(AIconID, AHandle) then
        FIcon.Handle := AHandle
      else
        FIcon.Handle := LoadIcon(0, AIconID);
    end;
  end;
end;

procedure TcxHintWindow.ChangeScale(M: Integer; D: Integer);
begin
  inherited ChangeScale(M, D);
  FCaptionFont.Height := MulDiv(FCaptionFont.Height, M, D);
end;

procedure TcxHintWindow.AdjustActivateRect(var ARect: TRect);
begin
  if InternalUseRightToLeftAlignment and cxRectIsEmpty(HintAreaBounds) then
    ARect := cxRectOffsetHorz(ARect, -ARect.Width);
  if not (StandardHint or (CallOutPosition = cxbpNone)) then
    ViewInfo.AdjustHintRect(ARect)
  else
  begin
    if not StandardHint then
       ViewInfo.FContentOffset := cxNullPoint;
    inherited;
  end;
  if HasWindowRegion then
  begin
    Inc(ARect.Right);
    Inc(ARect.Bottom);
  end;
end;

procedure TcxHintWindow.EnableRegion;
begin
  CreateBalloonForm;
end;

procedure TcxHintWindow.CreateBalloonForm;
begin
  if HasWindowRegion then
    ViewInfo.SetWindowRegion
  else
    DisableRegion;
end;

initialization
  cxRegisteredHintStyles.Register(TcxHintStyle, 'Advanced hint');
  InitCommonControl(0);// LoadIconMetric

end.
