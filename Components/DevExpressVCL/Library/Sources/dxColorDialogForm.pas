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

unit dxColorDialogForm;

{$I cxVer.inc}

interface

uses
  Types, Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, Menus, StdCtrls, ExtCtrls,
  dxCore, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, dxColorPicker, cxButtons, dxCoreGraphics,
  cxContainer, cxEdit, cxGroupBox, dxGalleryControl, dxGallery, cxClasses, cxGeometry, dxColorDialog, dxBevel, dxForms;

type

  { TdxColorDialogForm }

  TdxColorDialogForm = class(TdxForm)
    btnAddtoCustomColors: TcxButton;
    btnCancel: TcxButton;
    btnDefineCustomColors: TcxButton;
    btnOK: TcxButton;
    bvlSeparator: TdxBevel;
    cpColorPicker: TdxColorPicker;
    gcPalette: TdxGalleryControl;
    gcPaletteGroup1: TdxGalleryControlGroup;
    gcPaletteGroup2: TdxGalleryControlGroup;
    pnlBottom: TcxGroupBox;

    procedure btnAddtoCustomColorsClick(Sender: TObject);
    procedure btnDefineCustomColorsClick(Sender: TObject);
    procedure cpColorPickerColorChanged(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure gcPaletteItemClick(Sender: TObject; AItem: TdxGalleryControlItem);
  strict private
    FColor: TdxAlphaColor;
    FCustomColorIndex: Integer;
    FInvoker: TComponent;
    FLockPaletteChanging: Integer;
    FLookAndFeel: TcxLookAndFeel;
    FOwnerWindow: HWND;
    FPreventShowFull: Boolean;
    FUpdatingLayout: Boolean;

    function GetShowFull: Boolean;
    function GetPaletteBasicColorsGroup: TdxGalleryControlGroup;
    function GetPaletteCustomColorsGroup: TdxGalleryControlGroup;
    procedure SetColor(AValue: TdxAlphaColor);
    procedure SetShowFull(AValue: Boolean);
    procedure SetPreventShowFull(AValue: Boolean);
    //
    procedure CMBiDiModeChanged(var Message: TMessage); message CM_BIDIMODECHANGED;
  protected
    procedure ApplyLocalization;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure InitializeLookAndFeel;
    procedure InitializePaletteItem(AItem: TdxGalleryControlItem; AAlphaColor: TdxAlphaColor);
    procedure LookAndFeelChangeHandler(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues);
    procedure ScaleFactorChanged(M, D: Integer); override;
    procedure SynchronizePalette;
    procedure UpdateLayout;

    property PaletteBasicColorsGroup: TdxGalleryControlGroup read GetPaletteBasicColorsGroup;
    property PaletteCustomColorsGroup: TdxGalleryControlGroup read GetPaletteCustomColorsGroup;
  public
    constructor Create(AInvoker: TComponent; AOwnerWindow: HWND); reintroduce; virtual;
    destructor Destroy; override;
    procedure LoadBasicColors(const AColors: TdxColorDialogBasicColors);
    procedure LoadCustomColors(const AColors: TdxColorDialogCustomColors);
    procedure SaveCustomColors(var AColors: TdxColorDialogCustomColors);
    //
    property Color: TdxAlphaColor read FColor write SetColor;
    property Invoker: TComponent read FInvoker;
    property LookAndFeel: TcxLookAndFeel read FLookAndFeel;
    property PreventShowFull: Boolean read FPreventShowFull write SetPreventShowFull;
    property ShowFull: Boolean read GetShowFull write SetShowFull;
  end;

implementation

uses
  cxEditConsts, Math, dxGDIPlusClasses, dxDPIAwareUtils;

{$R *.dfm}

{ TdxColorDialogForm }

constructor TdxColorDialogForm.Create(AInvoker: TComponent; AOwnerWindow: HWND);
begin
  FInvoker := AInvoker;
  FOwnerWindow := AOwnerWindow;
  inherited Create(nil);
  FLookAndFeel := TcxLookAndFeel.Create(Self);
  FLookAndFeel.OnChanged := LookAndFeelChangeHandler;
  InitializeLookAndFeel;
  ApplyLocalization;
end;

destructor TdxColorDialogForm.Destroy;
begin
  FreeAndNil(FLookAndFeel);
  inherited Destroy;
end;

procedure TdxColorDialogForm.LoadBasicColors(const AColors: TdxColorDialogBasicColors);
var
  AGroup: TdxGalleryControlGroup;
  I: Integer;
begin
  AGroup := PaletteBasicColorsGroup;
  AGroup.Items.BeginUpdate;
  try
    AGroup.Items.Clear;
    for I := 0 to Length(AColors) - 1 do
      InitializePaletteItem(AGroup.Items.Add, AColors[I]);
  finally
    AGroup.Items.EndUpdate;
  end;
end;

procedure TdxColorDialogForm.LoadCustomColors(const AColors: TdxColorDialogCustomColors);
var
  AGroup: TdxGalleryControlGroup;
  I: Integer;
begin
  AGroup := PaletteCustomColorsGroup;
  AGroup.Items.BeginUpdate;
  try
    AGroup.Items.Clear;
    for I := 0 to Length(AColors) - 1 do
      InitializePaletteItem(AGroup.Items.Add, AColors[I]);
  finally
    AGroup.Items.EndUpdate;
  end;
end;

procedure TdxColorDialogForm.SaveCustomColors(var AColors: TdxColorDialogCustomColors);
var
  AGroup: TdxGalleryControlGroup;
  I: Integer;
begin
  AGroup := PaletteCustomColorsGroup;
  for I := 0 to Min(Length(AColors), AGroup.ItemCount) - 1 do
    AColors[I] := AGroup.Items[I].Tag;
end;

procedure TdxColorDialogForm.ApplyLocalization;
begin
  btnOK.Caption := cxGetResourceString(@sdxColorDialogApply);
  btnCancel.Caption := cxGetResourceString(@sdxColorDialogCancel);
  btnDefineCustomColors.Caption := cxGetResourceString(@sdxColorDialogDefineCustomColor);
  btnAddtoCustomColors.Caption := cxGetResourceString(@sdxColorDialogAddToCustomColors);
  PaletteBasicColorsGroup.Caption := cxGetResourceString(@sdxColorDialogBasicColors);
  PaletteCustomColorsGroup.Caption := cxGetResourceString(@sdxColorDialogCustomColors);
  Caption := cxGetResourceString(@sdxColorDialogCaption);
end;

procedure TdxColorDialogForm.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  if FOwnerWindow <> 0 then
    Params.WndParent := FOwnerWindow;
end;

procedure TdxColorDialogForm.InitializeLookAndFeel;
begin
  btnAddtoCustomColors.LookAndFeel.MasterLookAndFeel := LookAndFeel;
  btnDefineCustomColors.LookAndFeel.MasterLookAndFeel := LookAndFeel;
  btnCancel.LookAndFeel.MasterLookAndFeel := LookAndFeel;
  btnOK.LookAndFeel.MasterLookAndFeel := LookAndFeel;
  cpColorPicker.LookAndFeel.MasterLookAndFeel := LookAndFeel;
  gcPalette.LookAndFeel.MasterLookAndFeel := LookAndFeel;
end;

procedure TdxColorDialogForm.InitializePaletteItem(AItem: TdxGalleryControlItem; AAlphaColor: TdxAlphaColor);
var
  AAlpha: Byte;
  ABitmap: TcxBitmap32;
  AColor: TColor;
  ASize: TcxSize;
begin
  ASize := gcPalette.OptionsView.Item.Image.Size;
  ABitmap := TcxBitmap32.CreateSize(ASize.Width, ASize.Height);
  try
    AColor := dxAlphaColorToColor(AAlphaColor, AAlpha);
    if AAlpha < MaxByte then
      cxDrawTransparencyCheckerboard(ABitmap.cxCanvas, ABitmap.ClientRect, 2);
    dxGpFillRect(ABitmap.cxCanvas.Handle, ABitmap.ClientRect, AColor, AAlpha);
    AItem.Glyph.Assign(ABitmap);
    AItem.Glyph.SourceDPI := ScaleFactor.Apply(dxDefaultDPI);
    AItem.Tag := AAlphaColor;
  finally
    ABitmap.Free;
  end;
end;

procedure TdxColorDialogForm.LookAndFeelChangeHandler(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues);
begin
  UpdateLayout;
end;

procedure TdxColorDialogForm.ScaleFactorChanged(M, D: Integer);
begin
  inherited ScaleFactorChanged(M, D);
  UpdateLayout;
end;

procedure TdxColorDialogForm.SynchronizePalette;
var
  AItem: TdxGalleryItem;
begin
  AItem := gcPalette.Gallery.GetCheckedItem;
  if (AItem = nil) or (TdxAlphaColor(AItem.Tag) <> Color) then
  begin
    AItem := gcPalette.Gallery.FindItemByTag(Color);
    if AItem <> nil then
      AItem.Checked := True;
  end;
end;

procedure TdxColorDialogForm.UpdateLayout;

  procedure PlaceButtonUnderControl(AButton: TcxButton; AControl: TControl);
  var
    R: TRect;
  begin
    R := AControl.BoundsRect;
    AButton.BoundsRect := cxRectBounds(R.Left, R.Bottom + BorderWidth, cxRectWidth(R), AButton.Height);
  end;

var
  AMaxHeight: Integer;
  R: TRect;
begin
  if FUpdatingLayout then Exit;

  FUpdatingLayout := True;
  try
    R := ClientRect;

    gcPalette.AutoSizeMode := asAutoSize;
    AMaxHeight := gcPalette.Height;
    if cpColorPicker.Visible then
    begin
      cpColorPicker.AutoSize := True;
      AMaxHeight := Max(AMaxHeight, cpColorPicker.Height);
    end;

    gcPalette.AutoSizeMode := asAutoWidth;
    gcPalette.BoundsRect := cxRectSetSize(R, gcPalette.Width, AMaxHeight);
    PlaceButtonUnderControl(btnDefineCustomColors, gcPalette);
    R.Bottom := btnDefineCustomColors.BoundsRect.Bottom;
    R.Right := gcPalette.BoundsRect.Right;

    btnAddtoCustomColors.Visible := cpColorPicker.Visible;
    if cpColorPicker.Visible then
    begin
      R.Left := R.Right + BorderWidth;
      cpColorPicker.AutoSize := False;
      cpColorPicker.BoundsRect := cxRectSetSize(R, cpColorPicker.Width, AMaxHeight);
      PlaceButtonUnderControl(btnAddtoCustomColors, cpColorPicker);
      R.Right := cpColorPicker.BoundsRect.Right;
    end;

    ClientWidth := R.Right;
    ClientHeight := R.Bottom + pnlBottom.Height + BorderWidth;
  finally
    FUpdatingLayout := False;
  end;
end;

function TdxColorDialogForm.GetShowFull: Boolean;
begin
  Result := cpColorPicker.Visible;
end;

function TdxColorDialogForm.GetPaletteBasicColorsGroup: TdxGalleryControlGroup;
begin
  Result := gcPalette.Gallery.Groups[0];
end;

function TdxColorDialogForm.GetPaletteCustomColorsGroup: TdxGalleryControlGroup;
begin
  Result := gcPalette.Gallery.Groups[1];
end;

procedure TdxColorDialogForm.SetColor(AValue: TdxAlphaColor);
begin
  if FColor <> AValue then
  begin
    FColor := AValue;
    cpColorPicker.Color := Color;
    if FLockPaletteChanging = 0 then
      SynchronizePalette;
  end;
end;

procedure TdxColorDialogForm.SetShowFull(AValue: Boolean);
begin
  AValue := AValue and not PreventShowFull;
  if ShowFull <> AValue then
  begin
    cpColorPicker.Visible := AValue;
    btnDefineCustomColors.Enabled := not AValue;
    UpdateLayout;
  end;
end;

procedure TdxColorDialogForm.SetPreventShowFull(AValue: Boolean);
begin
  if FPreventShowFull <> AValue then
  begin
    FPreventShowFull := AValue;
    if PreventShowFull then
      ShowFull := False;
    btnDefineCustomColors.Enabled := not PreventShowFull;
  end;
end;

procedure TdxColorDialogForm.CMBiDiModeChanged(var Message: TMessage);
begin
  inherited;

  if UseRightToLeftAlignment then
  begin
    btnOK.Align := alLeft;
    btnCancel.Align := alLeft;
    btnCancel.Left := 0;
  end
  else
  begin
    btnOK.Align := alRight;
    btnOK.Left := 0;
    btnCancel.Align := alRight;
  end;
end;

procedure TdxColorDialogForm.btnAddtoCustomColorsClick(Sender: TObject);
begin
  InitializePaletteItem(PaletteCustomColorsGroup.Items[FCustomColorIndex], cpColorPicker.Color);
  FCustomColorIndex := (FCustomColorIndex + 1) mod PaletteCustomColorsGroup.ItemCount;
end;

procedure TdxColorDialogForm.btnDefineCustomColorsClick(Sender: TObject);
begin
  ShowFull := True;
end;

procedure TdxColorDialogForm.cpColorPickerColorChanged(Sender: TObject);
begin
  Inc(FLockPaletteChanging);
  try
    Color := cpColorPicker.Color;
  finally
    Dec(FLockPaletteChanging);
  end;
end;

procedure TdxColorDialogForm.FormResize(Sender: TObject);
begin
  UpdateLayout;
end;

procedure TdxColorDialogForm.FormShow(Sender: TObject);
begin
  UpdateLayout;
end;

procedure TdxColorDialogForm.gcPaletteItemClick(Sender: TObject; AItem: TdxGalleryControlItem);
begin
  if AItem.Group = PaletteCustomColorsGroup then
    FCustomColorIndex := AItem.Index;
  Color := AItem.Tag;
end;

end.
