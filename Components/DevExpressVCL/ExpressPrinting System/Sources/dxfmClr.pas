{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressPrinting System                                   }
{                                                                    }
{           Copyright (C) 1998-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSPRINTING SYSTEM AND            }
{   ALL ACCOMPANYING VCL CONTROLS AS PART OF AN                      }
{   EXECUTABLE PROGRAM ONLY.                                         }
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

unit dxfmClr;

interface

{$I cxVer.inc}

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, ExtCtrls, Buttons,
  dxCore, dxPSUtl, dxBkgnd, cxLookAndFeelPainters, cxControls, cxContainer,
  cxEdit, cxGroupBox, cxLabel, cxLookAndFeels, dxPSRes, cxGraphics, dxLayoutContainer, cxClasses, dxGalleryControl,
  dxColorGallery, dxLayoutControl, dxLayoutControlAdapters, Menus, StdCtrls, cxButtons, dxForms, dxLayoutcxEditAdapters;

type
  PdxBackgroundDlgData = ^TdxBackgroundDlgData;
  TdxBackgroundDlgData = record
    BorderStyle: TFormBorderStyle;
    FormCaption: string;
    NoBtnCaption: string;
    AutoColor: TColor;
    ShowFillEffects: Boolean;
    ShowMoreColors: Boolean;
  end;

  TdxfmColorPalette = class(TdxForm)
    dxLayoutControl1: TdxLayoutControl;
    cgColors: TdxColorGallery;
    dxLayoutControl1Group_Root: TdxLayoutGroup;
    dxLayoutItem4: TdxLayoutItem;
    btnNoFill: TcxButton;
    dxLayoutItem1: TdxLayoutItem;
    btnFillEffects: TcxButton;
    dxLayoutItem5: TdxLayoutItem;
    btnMoreColors: TcxButton;
    dxLayoutItem6: TdxLayoutItem;
    dxLayoutSeparatorItem1: TdxLayoutSeparatorItem;
    procedure btnMoreColorsClick(Sender: TObject);
    procedure btnFillEffectsClick(Sender: TObject);
    procedure ButtonClick(Sender: TObject);
    procedure cgColorsItemClick(Sender: TObject; AItem: TdxGalleryControlItem);
  private
    FAutoColor: TColor;
    FBackground: TdxBackground;
    FBorderStyle: TFormBorderStyle;
    FNoBtnCaption: string;
    FResult: TModalResult;
    FShowFillEffects: Boolean;
    FShowMoreColors: Boolean;
    procedure LoadStrings;
    procedure SetAutoColor(Value: TColor);
    procedure SetBackground(Value: TdxBackground);
    procedure SetBackgroundColor(AColor: TColor);
    procedure SetBorderStyle(Value: TFormBorderStyle);
    procedure SetNoBtnCaption(const Value: string);
    procedure SetShowFillEffects(Value: Boolean);
    procedure SetShowMoreColors(Value: Boolean);
    procedure WMKillFocus(var message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMNCCreate(var message: TWMNCCreate); message WM_NCCREATE;
    procedure WMNCDestroy(var message: TWMNCDestroy); message WM_NCDESTROY;
  protected
    procedure AdjustClientRect(var Rect: TRect); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Initialize(ABackground: TdxBackground; const APosition: TPoint; AData: TdxBackgroundDlgData); virtual;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    function Execute: Boolean;

    property AutoColor: TColor read FAutoColor write SetAutoColor;
    property Background: TdxBackground read FBackground write SetBackground;
    property BorderStyle: TFormBorderStyle read FBorderStyle write SetBorderStyle;
    property NoBtnCaption: string read FNoBtnCaption write SetNoBtnCaption;
    property ShowFillEffects: Boolean read FShowFillEffects write SetShowFillEffects;
    property ShowMoreColors: Boolean read FShowMoreColors write SetShowMoreColors;
  end;

function dxChooseBackgroundDlg(ABackground: TdxBackground; APosition: TPoint;
  const AParams: TdxBackgroundDlgData): Boolean; overload;
function dxDefaultBackgroundDlgData: TdxBackgroundDlgData;

implementation

{$R *.DFM}

uses
  dxPSEngn, dxPSImgs, dxPSGlbl;

function dxChooseBackgroundDlg(ABackground: TdxBackground; APosition: TPoint;
  const AParams: TdxBackgroundDlgData): Boolean;
begin
  with TdxfmColorPalette.Create(nil) do
  try
    Initialize(ABackground, APosition, AParams);
    Result := Execute;
  finally
    Free;
  end;
end;

function dxDefaultBackgroundDlgData: TdxBackgroundDlgData;
begin
  Result.BorderStyle := bsSingle;
  Result.FormCaption := cxGetResourceString(@sdxPageBackground);
  Result.NoBtnCaption := cxGetResourceString(@sdxBtnNoFill);
  Result.AutoColor := clBlack;
  Result.ShowFillEffects := True;
  Result.ShowMoreColors := True;
end;

{ TdxNoFillButton }

type
  TdxColorGalleryAccess = class(TdxColorGallery);

{ TdxfmColorPalette }

constructor TdxfmColorPalette.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  inherited BorderStyle := bsNone;
  ControlStyle := ControlStyle - [csCaptureMouse];
  FShowFillEffects := True;
  FShowMoreColors := True;
  LoadStrings;
  btnNoFill.Caption := NoBtnCaption;
  SetControlLookAndFeel(Self, dxPSEngine.DialogsLookAndFeel);
  TdxColorGalleryAccess(cgColors).CanBeFocused := False;
  FormStyle := fsStayOnTop;
end;

function TdxfmColorPalette.Execute: Boolean;
begin
  FResult := mrNone;
  Show;
  while FResult = mrNone do
    Application.HandleMessage;
  Result := FResult = mrOK;
end;

procedure TdxfmColorPalette.AdjustClientRect(var Rect: TRect);
begin
  inherited AdjustClientRect(Rect);
  if BorderStyle <> bsNone then
    InflateRect(Rect, -2, -2);
end;

procedure TdxfmColorPalette.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.WindowClass.Style := Params.WindowClass.Style or CS_SAVEBITS;
end;

procedure TdxfmColorPalette.cgColorsItemClick(Sender: TObject; AItem: TdxGalleryControlItem);
begin
  SetBackgroundColor(cgColors.ColorValue);
  FResult := mrOk;
end;

procedure TdxfmColorPalette.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_ESCAPE) or (ssAlt in Shift) then
    Close
  else
    if Key = Ord('f') then
      btnFillEffects.Click
    else
      if Key = Ord('b') then
        btnMoreColors.Click
      else
        inherited;
end;

procedure TdxfmColorPalette.Initialize(ABackground: TdxBackground;
  const APosition: TPoint; AData: TdxBackgroundDlgData);
var
  P: TPoint;
  R: TRect;
begin
  Background := ABackground;

  BorderStyle := bsSingle;
  Caption := AData.FormCaption;
  NoBtnCaption := AData.NoBtnCaption;
  AutoColor := AData.AutoColor;
  ShowFillEffects := AData.ShowFillEffects;
  ShowMoreColors := AData.ShowMoreColors;

  P := APosition;
  R := GetDesktopWorkArea(P);
  if P.X + Width > R.Right then
    P.X := R.Right - Width;
  if P.Y + Height > R.Bottom then
    P.Y := R.Bottom - Height;
  if P.X < R.Left then
    P.X := R.Left;
  SetBounds(P.X, P.Y, Width, Height);
end;

procedure TdxfmColorPalette.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;
  FResult := mrCancel;
  Hide;
end;

procedure TdxfmColorPalette.WMNCCreate(var Message: TWMNCCreate);
var
  SysMenu: HMENU;
begin
  inherited;
  SysMenu := GetSystemMenu(Handle, False);
  DeleteMenu(SysMenu, SC_RESTORE, MF_BYCOMMAND);
  DeleteMenu(SysMenu, SC_MINIMIZE, MF_BYCOMMAND);
  DeleteMenu(SysMenu, SC_MAXIMIZE, MF_BYCOMMAND);
  DeleteMenu(SysMenu, SC_SIZE, MF_BYCOMMAND);
end;

procedure TdxfmColorPalette.WMNCDestroy(var Message: TWMNCDestroy);
begin
  GetSystemMenu(Handle, True);
  inherited;
end;

procedure TdxfmColorPalette.Paint;
var
  ACanvas: TcxCanvas;
begin
  inherited Paint;
  if BorderStyle <> bsNone then
  begin
    ACanvas := TcxCanvas.Create(Canvas);
    try
      dxPSEngine.DialogsLookAndFeel.Painter.DrawBorder(ACanvas, ClientRect);
    finally
      ACanvas.Free;
    end;
  end;
end;

procedure TdxfmColorPalette.SetNoBtnCaption(const Value: string);
begin
  if FNoBtnCaption <> Value then
  begin
    FNoBtnCaption := Value;
    btnNoFill.Caption := FNoBtnCaption;
  end;
end;

procedure TdxfmColorPalette.SetShowFillEffects(Value: Boolean);
begin
  if FShowFillEffects <> Value then
  begin
    FShowFillEffects := Value;
    btnFillEffects.Visible := False;
    btnMoreColors.Visible := False;
  end;
end;

procedure TdxfmColorPalette.SetShowMoreColors(Value: Boolean);
begin
  if FShowMoreColors <> Value then
  begin
    FShowMoreColors := Value;
    btnMoreColors.Visible := FShowMoreColors or FShowFillEffects;
    btnFillEffects.Visible := btnMoreColors.Visible;
  end;
end;

procedure TdxfmColorPalette.SetBorderStyle(Value: TFormBorderStyle);
begin
  FBorderStyle := Value;
  ClientWidth := 148 + 4 * Ord(BorderStyle <> bsNone);
end;

procedure TdxfmColorPalette.LoadStrings;
begin
  btnMoreColors.Caption := cxGetResourceString(@sdxBtnMoreColors);
  btnFillEffects.Caption := cxGetResourceString(@sdxBtnFillEffects);
end;

procedure TdxfmColorPalette.btnMoreColorsClick(Sender: TObject);
begin
  Hide;
  dxPSGlbl.ColorDialog.Color := cgColors.ColorValue;
  if dxPSGlbl.ColorDialog.Execute then
  begin
    SetBackgroundColor(dxPSGlbl.ColorDialog.Color);
    FResult := mrOK;
  end
  else
    FResult := mrCancel;
end;

procedure TdxfmColorPalette.btnFillEffectsClick(Sender: TObject);
const
  ModalResults: array[Boolean] of TModalResult = (mrCancel, mrOK);
begin
  Hide;
  Application.ProcessMessages;
  FResult := ModalResults[Background.SetupEffects];
end;

procedure TdxfmColorPalette.SetAutoColor(Value: TColor);
begin
  if FAutoColor <> Value then
    FAutoColor := Value;
end;

procedure TdxfmColorPalette.SetBackground(Value: TdxBackground);
begin
  FBackground := Value;
  if FBackground <> nil then
  begin
    btnNoFill.Down := FBackground.Mode = bmNone;
    if FBackground.Mode = bmBrush then
      cgColors.ColorValue := FBackground.Brush.Color;
  end;
end;

procedure TdxfmColorPalette.SetBackgroundColor(AColor: TColor);
begin
  Background.BeginUpdate;
  try
    Background.Mode := bmBrush;
    Background.Brush.Style := bsSolid;
    Background.Brush.Color := AColor;
  finally
    Background.EndUpdate;
  end;
end;

procedure TdxfmColorPalette.ButtonClick(Sender: TObject);
begin
  Background.Clear;
  FResult := mrOk;
end;

end.

