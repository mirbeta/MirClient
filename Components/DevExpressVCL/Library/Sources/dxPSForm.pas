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

unit dxPSForm;

interface

{$I cxVer.inc}

uses
  Windows, Classes, Controls, Messages, Forms, Registry, cxLookAndFeels,
  cxLookAndFeelPainters, cxGraphics, cxControls, Graphics, IniFiles, dxForms;

type
  TdxPSFormOption = (foSizeableDialog);
  TdxPSFormOptions = set of TdxPSFormOption;

  TdxPSFormClass = class of TCustomdxPSForm;

  { TCustomdxPSForm }

  TCustomdxPSForm = class(TdxForm)
  private
    FOptions: TdxPSFormOptions;
    function GetGripperBackgroundColor: TColor;
    function GetGripperBounds: TRect;
    function GetIsSizeableDialog: Boolean;
    function GetPainter: TcxCustomLookAndFeelPainter;

    procedure WMNCCreate(var message: TWMNCCreate); message WM_NCCREATE;
    procedure WMNCDestroy(var message: TWMNCCreate); message WM_NCDESTROY;
    procedure WMNCHitTest(var message: TWMNCHitTest); message WM_NCHITTEST;
  protected
    procedure BeforeConstruction; virtual;
    function IsFormSizeable: Boolean; virtual;
    function PtInGripperBounds(const Pt: TPoint): Boolean;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure HelpButtonClick(Sender: TObject); virtual;
    procedure LoadDefaultSettings;
    procedure Paint; override;
    procedure SaveDefaultSettings;
    procedure SetupHelpEventHandler;

    property GripperBackgroundColor: TColor read GetGripperBackgroundColor;
    property GripperBounds: TRect read GetGripperBounds;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    //
    procedure LoadFromIniFile(AIniFile: TCustomIniFile; const ASectionName: string); virtual;
    procedure SaveToIniFile(AIniFile: TCustomIniFile; const ASectionName: string); virtual;
    procedure LoadFromRegistry(const APath: string);
    procedure SaveToRegistry(const APath: string);
    //
    property IsSizeableDialog: Boolean read GetIsSizeableDialog;
    property Options: TdxPSFormOptions read FOptions write FOptions;
    property Painter: TcxCustomLookAndFeelPainter read GetPainter;
  end;

function dxPSFormGetActualSectionName(AForm: TCustomForm; const ABaseSection: string = ''): string;
function dxPSFormLoadPositionFromIniFile(AForm: TCustomForm;
  AIniFile: TCustomIniFile; const ASectionName: string; AIsFormSizeable: Boolean = True): Boolean;
procedure dxPSFormSavePositionToIniFile(AForm: TCustomForm;
  AIniFile: TCustomIniFile; const ASectionName: string; AIsFormSizeable: Boolean = True);
implementation

uses
  Types, SysUtils, StdCtrls, Themes, Math,
  cxGeometry, dxPSGlbl, dxPSUtl, dxPSEngn, dxDPIAwareUtils;

const
  sdxFormLayouts = 'FormLayouts';    // Don't localize
  sdxWindowState = 'WindowState';     // Don't localize
  sdxLeft = 'Left';                   // Don't localize
  sdxTop = 'Top';                     // Don't localize
  sdxWidth = 'Width';                 // Don't localize
  sdxHeight = 'Height';               // Don't localize
  sdxScreenWidth = 'ScreenWidth';     // Don't localize
  sdxScreenHeight = 'ScreenHeight';   // Don't localize

type
  TCustomFormAccess = class(TCustomForm);

function dxPSFormGetActualSectionName(AForm: TCustomForm; const ABaseSection: string = ''): string;
begin
  Result := dxValidatePath(ABaseSection) + sdxFormLayouts + '\' + DropT(AForm.ClassName);
end;

function dxPSFormLoadPositionFromIniFile(AForm: TCustomForm;
  AIniFile: TCustomIniFile; const ASectionName: string; AIsFormSizeable: Boolean = True): Boolean;

  function ReadSettingValue(const AValueName: string;
    ADefaultValue: Integer; var APositionChanged: Boolean): Integer;
  begin
    Result := AIniFile.ReadInteger(ASectionName, AValueName, -1);
    if Result <> -1 then
      APositionChanged := True
    else
      Result := ADefaultValue;
  end;

  function ReadPosition(out ABounds: TRect; out AState: TWindowState): Boolean;
  begin
    Result := False;
    ABounds := AForm.BoundsRect;
    ABounds.Top := ReadSettingValue(sdxTop, ABounds.Top, Result);
    ABounds.Left := ReadSettingValue(sdxLeft, ABounds.Left, Result);
    if not AIsFormSizeable then
      ABounds := cxRectSetSize(ABounds, AForm.Width, AForm.Height)
    else
      ABounds := cxRectSetSize(ABounds,
        ReadSettingValue(sdxWidth, AForm.Width, Result),
        ReadSettingValue(sdxHeight, AForm.Height, Result));
    AState := TWindowState(ReadSettingValue(sdxWindowState, Integer(wsNormal), Result));
  end;

  function CheckBounds(const ABounds: TRect): TRect;
  var
    AScreenWidth, AScreenHeight: Integer;
  begin
    Result := ABounds;
    AScreenWidth := AIniFile.ReadInteger(ASectionName, sdxScreenWidth, Screen.Width);
    AScreenHeight := AIniFile.ReadInteger(ASectionName, sdxScreenHeight, Screen.Height);
    if (Screen.Width <> AScreenWidth) or (Screen.Height <> AScreenHeight) then
    begin
      Result.Bottom := Min(Result.Bottom, Screen.Height);
      Result.Right := Min(Result.Right, Screen.Width);
    end;
  end;

var
  AFormBounds: TRect;
  AWindowState: TWindowState;
begin
  Result := ReadPosition(AFormBounds, AWindowState);
  if Result then
  begin
    if AWindowState <> wsMaximized then
    begin
      TCustomFormAccess(AForm).Position := poDesigned;
      AForm.BoundsRect := CheckBounds(cxRectScaleSize(AFormBounds, dxGetFormDPI(AForm), dxDefaultDPI));
    end;
    AForm.WindowState := AWindowState;
  end;
end;

procedure dxPSFormSavePositionToIniFile(AForm: TCustomForm;
  AIniFile: TCustomIniFile; const ASectionName: string; AIsFormSizeable: Boolean = True);
var
  AWindowPlacement: TWindowPlacement;
  R: TRect;
begin
  AIniFile.WriteInteger(ASectionName, sdxScreenWidth, Screen.Width);
  AIniFile.WriteInteger(ASectionName, sdxScreenHeight, Screen.Height);
  AIniFile.WriteInteger(ASectionName, sdxWindowState, Integer(AForm.WindowState));

  ZeroMemory(@AWindowPlacement, SizeOf(TWindowPlacement));
  AWindowPlacement.Length := SizeOf(TWindowPlacement);
  if GetWindowPlacement(AForm.Handle, @AWindowPlacement) then
  begin
    R := cxRectScaleSize(AWindowPlacement.rcNormalPosition, dxDefaultDPI, dxGetFormDPI(AForm));
    AIniFile.WriteInteger(ASectionName, sdxLeft, R.Left);
    AIniFile.WriteInteger(ASectionName, sdxTop, R.Top);
    if AIsFormSizeable then
    begin
      AIniFile.WriteInteger(ASectionName, sdxWidth, R.Right - R.Left);
      AIniFile.WriteInteger(ASectionName, sdxHeight, R.Bottom - R.Top);
    end;
  end;
end;

{ TCustomdxPSForm }

procedure TCustomdxPSForm.AfterConstruction;
begin
  inherited AfterConstruction;
  SetControlLookAndFeel(Self, dxPSEngine.DialogsLookAndFeel);
  SetupHelpEventHandler;
  LoadDefaultSettings;
  PopupMode := pmAuto;
end;

procedure TCustomdxPSForm.BeforeDestruction;
begin
  SaveDefaultSettings;
  inherited BeforeDestruction;
end;

procedure TCustomdxPSForm.LoadDefaultSettings;
var
  AIniFile: TCustomIniFile;
begin
  if dxPSStoringManager.BeginStoring(AIniFile) then
  try
    LoadFromIniFile(AIniFile, dxPSFormGetActualSectionName(Self));
  finally
    dxPSStoringManager.EndStoring(AIniFile);
  end;
end;

procedure TCustomdxPSForm.SaveDefaultSettings;
var
  AIniFile: TCustomIniFile;
begin
  if dxPSStoringManager.BeginStoring(AIniFile) then
  try
    SaveToIniFile(AIniFile, dxPSFormGetActualSectionName(Self));
  finally
    dxPSStoringManager.EndStoring(AIniFile);
  end;
end;

procedure TCustomdxPSForm.LoadFromRegistry(const APath: string);
var
  ARegIniFile: TRegistryIniFile;
begin
  ARegIniFile := TRegistryIniFile.Create('');
  try
    LoadFromIniFile(ARegIniFile, APath);
  finally
    ARegIniFile.Free;
  end;
end;

procedure TCustomdxPSForm.SaveToRegistry(const APath: string);
var
  ARegIniFile: TRegistryIniFile;
begin
  ARegIniFile := TRegistryIniFile.Create('');
  try
    SaveToIniFile(ARegIniFile, APath);
  finally
    ARegIniFile.Free;
  end;
end;

procedure TCustomdxPSForm.LoadFromIniFile(AIniFile: TCustomIniFile; const ASectionName: string);
begin
  if dxPSEngine.SaveFormsPosition then
    dxPSFormLoadPositionFromIniFile(Self, AIniFile, ASectionName, IsFormSizeable);
end;

procedure TCustomdxPSForm.SaveToIniFile(AIniFile: TCustomIniFile; const ASectionName: string);
begin
  if dxPSEngine.SaveFormsPosition then
    dxPSFormSavePositionToIniFile(Self, AIniFile, ASectionName, IsFormSizeable);
end;

procedure TCustomdxPSForm.CreateParams(var Params: TCreateParams);
begin
  BeforeConstruction;
  inherited CreateParams(Params);
  if IsSizeableDialog then
  begin
    Params.Style := Params.Style or WS_THICKFRAME;
    Params.WindowClass.Style := Params.WindowClass.Style or CS_VREDRAW or CS_HREDRAW;
  end;
  if IsLibrary then // v3 -> Integrate PS into Windows Shell
    Params.WndParent := GetForegroundWindow;
end;

procedure TCustomdxPSForm.Paint;
var
  ACanvas: TcxCanvas;
begin
  inherited Paint;
  if IsSizeableDialog then
  begin
    ACanvas := TcxCanvas.Create(Canvas);
    try
      Painter.DrawSizeGrip(ACanvas, GripperBounds, GripperBackgroundColor);
    finally
      ACanvas.Free;
    end;
  end;
end;

procedure TCustomdxPSForm.BeforeConstruction;
begin
  HelpFile := dxPSEngine.HelpFile;
end;

procedure TCustomdxPSForm.HelpButtonClick(Sender: TObject);
begin
  if HelpContext <> 0 then
    Application.HelpContext(HelpContext);
end;

function TCustomdxPSForm.IsFormSizeable: Boolean;
begin
  Result := HandleAllocated and dxIsWindowStyleSet(Handle, WS_THICKFRAME);// and (dwoSizeable in Options);
end;

function TCustomdxPSForm.GetGripperBackgroundColor: TColor;
begin
  if dxPSEngine.IsSkinsStyle then
    Result := clNone
  else
    Result := Color;
end;

function TCustomdxPSForm.GetGripperBounds: TRect;
var
  ASize: TSize;
begin
  Result := ClientRect;
  ASize := Painter.ScaledSizeGripSize(ScaleFactor);
  Result.Left := Result.Right - ASize.cx;
  Result.Top := Result.Bottom - ASize.cy;
end;

function TCustomdxPSForm.GetIsSizeableDialog: Boolean;
begin
  Result := foSizeableDialog in Options;
end;

function TCustomdxPSForm.GetPainter: TcxCustomLookAndFeelPainter;
begin
  Result := dxPSEngine.DialogsLookAndFeel.Painter;
end;

function TCustomdxPSForm.PtInGripperBounds(const Pt: TPoint): Boolean;
begin
  Result := PtInRect(GripperBounds, Pt);
end;

procedure TCustomdxPSForm.SetupHelpEventHandler;
var
  Button: TComponent;
begin
  Button := FindComponent(sdxHelpButtonName);
  if Button is TButton then
    TButton(Button).OnClick := HelpButtonClick;
end;

procedure TCustomdxPSForm.WMNCCreate(var message: TWMNCCreate);
var
  ACaption: array[0..31] of Char;
  AHasItem: Boolean;
  AInfo: TMenuItemInfo;
  ASysMenu: HMENU;
begin
  ASysMenu := 0;
  AHasItem := False;
  if IsSizeableDialog then
  begin
    ASysMenu := GetSystemMenu(Handle, False);
    AInfo.cbSize := SizeOf(AInfo) - SizeOf(HBITMAP);
    AInfo.fMask := MIIM_ID or MIIM_TYPE;
    AInfo.dwTypeData := @ACaption[0];
    AInfo.cch := Length(ACaption);
    AHasItem := GetMenuItemInfo(ASysMenu, SC_SIZE, False, AInfo);
  end;
  inherited;
  if AHasItem then
    InsertMenuItem(ASysMenu, 0, True, AInfo);
end;

procedure TCustomdxPSForm.WMNCDestroy(var message: TWMNCCreate);
begin
  if IsSizeableDialog then
    GetSystemMenu(Handle, True);
  inherited;
end;

procedure TCustomdxPSForm.WMNCHitTest(var message: TWMNCHitTest);
var
  Pt: TPoint;
begin
  inherited;
  if IsSizeableDialog then
  begin
    Pt := SmallPointToPoint(Message.Pos);
    Pt := ScreenToClient(Pt);
    if PtInGripperBounds(Pt) then
      Message.Result := HTBOTTOMRIGHT;
  end;
end;

end.

