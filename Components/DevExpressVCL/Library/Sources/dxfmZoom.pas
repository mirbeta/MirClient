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

unit dxfmZoom;

interface

{$I cxVer.inc}

uses
  Types, Windows, SysUtils, Classes, Graphics, Controls, Forms, ExtCtrls, StdCtrls,
  ComCtrls, Commctrl, Buttons, ImgList,
  dxExtCtrls, dxPreVw, dxPSForm,{$IFDEF DELPHI16} UITypes,{$ENDIF}
  cxRadioGroup, Menus, cxLookAndFeelPainters, cxButtons, cxControls,
  cxContainer, cxEdit, cxLabel, cxGroupBox, cxTextEdit, cxMaskEdit,
  cxSpinEdit, cxGraphics, cxGeometry, cxLookAndFeels, dxLayoutControlAdapters, dxLayoutcxEditAdapters,
  dxLayoutContainer, dxLayoutLookAndFeels, cxClasses, dxLayoutControl, cxImageList;

type

  { TfmZoom }

  TfmZoom = class(TCustomdxPSForm)
    btnCancel: TcxButton;
    btnHelp: TcxButton;
    btnManyPages: TcxButton;
    btnOK: TcxButton;
    bvlFontPreviewHolder: TdxLayoutItem;
    dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel;
    dxLayoutGroup1: TdxLayoutGroup;
    dxLayoutGroup2: TdxLayoutGroup;
    dxLayoutGroup3: TdxLayoutGroup;
    dxLayoutGroup4: TdxLayoutGroup;
    dxLayoutGroup6: TdxLayoutGroup;
    dxLayoutGroup7: TdxLayoutGroup;
    dxLayoutItem1: TdxLayoutItem;
    dxLayoutItem10: TdxLayoutItem;
    dxLayoutItem11: TdxLayoutItem;
    dxLayoutItem12: TdxLayoutItem;
    dxLayoutItem13: TdxLayoutItem;
    dxLayoutItem14: TdxLayoutItem;
    dxLayoutItem16: TdxLayoutItem;
    dxLayoutItem18: TdxLayoutItem;
    dxLayoutItem19: TdxLayoutItem;
    dxLayoutItem2: TdxLayoutItem;
    dxLayoutItem3: TdxLayoutItem;
    dxLayoutItem4: TdxLayoutItem;
    dxLayoutItem5: TdxLayoutItem;
    dxLayoutItem6: TdxLayoutItem;
    dxLayoutItem7: TdxLayoutItem;
    dxLayoutItem8: TdxLayoutItem;
    dxLayoutItem9: TdxLayoutItem;
    dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList;
    gbxPreview: TdxLayoutGroup;
    gbxZoomTo: TdxLayoutGroup;
    ilMonitor: TcxImageList;
    ilStub: TcxImageList;
    lblPercent: TdxLayoutItem;
    lcMain: TdxLayoutControl;
    lcMainGroup_Root: TdxLayoutGroup;
    libtnHelp: TdxLayoutItem;
    pbPreview: TPaintBox;
    rbtn10: TcxRadioButton;
    rbtn100: TcxRadioButton;
    rbtn150: TcxRadioButton;
    rbtn200: TcxRadioButton;
    rbtn25: TcxRadioButton;
    rbtn50: TcxRadioButton;
    rbtn500: TcxRadioButton;
    rbtn75: TcxRadioButton;
    rbtnFourPages: TcxRadioButton;
    rbtnManyPages: TcxRadioButton;
    rbtnPageWidth: TcxRadioButton;
    rbtnTwoPages: TcxRadioButton;
    rbtnWholePage: TcxRadioButton;
    sePercent: TcxSpinEdit;

    procedure btnManyPagesClick(Sender: TObject);
    procedure lblPercentClick(Sender: TObject);
    procedure PercentChange(Sender: TObject);
    procedure PercentExit(Sender: TObject);
    procedure PreviewPaint(Sender: TObject);
    procedure rbtnClick(Sender: TObject);
    procedure sePercentKeyPress(Sender: TObject; var Key: Char);
  private
    FModified: Boolean;
    FpnlFontPreview: TCustomControl;
    FPreview: TdxPreview;
    FPreviewOwnerSize: TPoint;
    FUpdateCount: Integer;

    procedure FontPreviewPaint(Sender: TObject);
    procedure PercentsChanged(ANewValue: Integer);
    procedure SetZoomFactor(Value: Integer);
    procedure UpdateZoomRadioBoxes;
  protected
    procedure BeginUpdate;
    procedure EndUpdate;

    procedure CreateControls; virtual;
    procedure InitControls; virtual;
    procedure InitPreview(APreview: TdxPreview);
    procedure LoadStrings; virtual;
    procedure UpdateControlsState;
  public
    constructor Create(AOwner: TComponent); override;
    function Execute: Boolean;
  end;

function dxZoomDlg(APreview: TdxPreview): Boolean;

implementation

{$R *.DFM}

uses
  //Variants,
  Math, dxCustomPreview, dxPSGlbl, dxPSImgs, dxPSRes, dxPSUtl, dxfmMnPg, dxCore, cxDrawTextUtils;

type
  TdxPreviewAccess = class(TdxPreview);

function dxZoomDlg(APreview: TdxPreview): Boolean;
var
  AForm: TfmZoom;
begin
  AForm := TfmZoom.Create(nil);
  try
    AForm.CreateControls;
    AForm.InitPreview(APreview);
    if (APreview.Owner <> nil) and (APreview.Owner is TControl) then
      AForm.FPreviewOwnerSize := Point(TControl(APreview.Owner).Width, TControl(APreview.Owner).Height)
    else
      AForm.FPreviewOwnerSize := Point(APreview.Width, APreview.Height);

    Result := AForm.Execute;
    if Result then
    begin
      TdxPreviewAccess(APreview).SetPageXYCount(AForm.FPreview.PageXCount, AForm.FPreview.PageYCount);
      APreview.ZoomFactor := AForm.FPreview.ZoomFactor;
      APreview.ZoomMode := AForm.FPreview.ZoomMode;
    end;
  finally
    AForm.Free;
  end;
end;

{ TfmZoom }

constructor TfmZoom.Create(AOwner: TComponent);
begin
  inherited;
  HelpContext := dxhcZoomDlg;
  CheckDialogFormHelpContext(Self, libtnHelp);

  FPreview := TdxPreview.Create(Self);
  FPreview.Visible := False;
  FPreview.MinZoomFactor := 10;
  FPreview.Parent := Self;
end;

function TfmZoom.Execute: Boolean;
begin
  LoadStrings;
  InitControls;
  FModified := False;
  UpdateControlsState;
  Result := (ShowModal = mrOK) and FModified;
end;

procedure TfmZoom.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TfmZoom.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount = 0 then
  begin
    FpnlFontPreview.Invalidate;
    pbPreview.Invalidate;
  end;
end;

procedure TfmZoom.CreateControls;
begin
  FpnlFontPreview := TdxPSPaintPanel.Create(Self);
  bvlFontPreviewHolder.Control := FpnlFontPreview;
  TdxPSPaintPanel(FpnlFontPreview).OnPaint := FontPreviewPaint;
  TdxPSPaintPanel(FpnlFontPreview).EdgeInner := esNone;
  TdxPSPaintPanel(FpnlFontPreview).EdgeOuter := esNone;
end;

procedure TfmZoom.InitControls;
var
  I: Integer;
  Control: TControl;
begin
  case FPreview.ZoomMode of
    pzmNone:
      begin
        UpdateZoomRadioBoxes;
        for I := 0 to lcMain.ControlCount - 1 do
        begin
          Control := lcMain.Controls[I];
          if (Control is TcxRadioButton) and TcxRadioButton(Control).Checked then
          begin
            ActiveControl := TWinControl(Control);
            Break;
          end;
        end;
        sePercent.Value := FPreview.ZoomFactor;
      end;

    pzmPageWidth:
      rbtnPageWidth.Checked := True;

    pzmPages:
      begin
        rbtnWholePage.Checked := FPreview.PageXCount = 1;
        rbtnTwoPages.Checked := FPreview.PageXCount = 2;
        rbtnFourPages.Checked := FPreview.PageXCount = 4;
        rbtnManyPages.Checked := not (FPreview.PageXCount in [1, 2, 4]);
      end;
  end;
  rbtnTwoPages.Enabled := FPreview.PageCount > 1;
  rbtnFourPages.Enabled := FPreview.PageCount > 3;
end;

procedure TfmZoom.InitPreview(APreview: TdxPreview);
var
  I: Integer;
begin
  FPreview.MeasurementUnits := APreview.MeasurementUnits;
  TdxPreviewAccess(FPreview).PageSize := TdxPreviewAccess(FPreview).PageSize;
  for I := 0 to APreview.PageCount - 1 do
    TdxPreviewAccess(FPreview).CreatePage;
  FPreview.PageXCount := APreview.PageXCount;
  FPreview.PageYCount := APreview.PageYCount;
  FPreview.Width := APreview.Width;
  FPreview.Height := APreview.Height;
  FPreview.ZoomFactor := APreview.ZoomFactor;
  FPreview.ZoomMode := APreview.ZoomMode;
end;

procedure TfmZoom.LoadStrings;
begin
  btnOK.Caption := cxGetResourceString(@sdxBtnOK);
  btnCancel.Caption := cxGetResourceString(@sdxBtnCancel);
  btnHelp.Caption := cxGetResourceString(@sdxBtnHelp);

  Caption := cxGetResourceString(@sdxZoomDlgCaption);
  gbxZoomTo.Caption := cxGetResourceString(@sdxZoomDlgZoomTo);
  rbtnPageWidth.Caption := cxGetResourceString(@sdxZoomDlgPageWidth);
  rbtnWholePage.Caption := cxGetResourceString(@sdxZoomDlgWholePage);
  rbtnTwoPages.Caption := cxGetResourceString(@sdxZoomDlgTwoPages);
  rbtnFourPages.Caption := cxGetResourceString(@sdxZoomDlgFourPages);
  rbtnManyPages.Caption := cxGetResourceString(@sdxZoomDlgManyPages);
  lblPercent.Caption := cxGetResourceString(@sdxZoomDlgPercent);
  gbxPreview.Caption := cxGetResourceString(@sdxZoomDlgPreview);
  bvlFontPreviewHolder.Caption := cxGetResourceString(@sdxZoomDlgFontPreview);
end;

procedure TfmZoom.UpdateControlsState;
begin
  //btnOK.Enabled := FModified;
end;

procedure TfmZoom.SetZoomFactor(Value: Integer);
begin
  sePercent.Value := Value;
  FPreview.ZoomFactor := Value;
  FpnlFontPreview.Invalidate;
  pbPreview.Invalidate;
end;

procedure TfmZoom.UpdateZoomRadioBoxes;
begin
  rbtn500.Checked := FPreview.ZoomFactor = 500;
  rbtn200.Checked := FPreview.ZoomFactor = 200;
  rbtn150.Checked := FPreview.ZoomFactor = 150;
  rbtn100.Checked := FPreview.ZoomFactor = 100;
  rbtn75.Checked := FPreview.ZoomFactor = 75;
  rbtn50.Checked := FPreview.ZoomFactor = 50;
  rbtn25.Checked := FPreview.ZoomFactor = 25;
  rbtn10.Checked := FPreview.ZoomFactor = 10;
end;

procedure TfmZoom.lblPercentClick(Sender: TObject);
begin
  ActiveControl := TcxLabel(Sender).FocusControl;
end;

procedure TfmZoom.rbtnClick(Sender: TObject);
const
  cZoomFactors: array[0..7] of Integer = (500, 200, 150, 100, 75, 50, 25, 10);
var
  T: Longint;
begin
  T := TTagToInt(TComponent(Sender).Tag);
  if T < 8 then
  begin
    sePercent.Value := cZoomFactors[T];
    FPreview.ZoomMode := pzmNone;
    SetZoomFactor(cZoomFactors[T]);
  end
  else
    if T < 12 then
    begin
      SetZoomFactor(FPreview.ZoomFactor);
      if T = 8 then
        FPreview.ZoomMode := pzmPageWidth
      else
      begin
        FPreview.ZoomMode := pzmPages;
        case T of
          9: TdxPreviewAccess(FPreview).SetPageXYCount(1, 1);
          10: TdxPreviewAccess(FPreview).SetPageXYCount(2, 1);
          11: TdxPreviewAccess(FPreview).SetPageXYCount(2, 2);
        end;
      end;
      SetZoomFactor(FPreview.ZoomFactor);
    end
    else
      SetZoomFactor(FPreview.ZoomFactor);
  FModified := True;
  UpdateControlsState;
end;

procedure TfmZoom.PercentChange(Sender: TObject);
begin
  FModified := True;
  PercentsChanged(sePercent.Value);
  UpdateControlsState;
end;

procedure TfmZoom.PercentExit(Sender: TObject);
begin
  SetZoomFactor(TcxSpinEdit(Sender).Value);
end;

procedure TfmZoom.PercentsChanged(ANewValue: Integer);
begin
  BeginUpdate;
  try
    SetZoomFactor(ANewValue);
    UpdateZoomRadioBoxes;
  finally
    EndUpdate;
  end;
end;

procedure TfmZoom.btnManyPagesClick(Sender: TObject);
var
  AOrigin: TPoint;
  AYShift: Integer;
  AMaxColCount, AMaxRowCount: Integer;
  ARowCount, AColCount: Integer;
begin
  AOrigin := TcxButton(Sender).ClientOrigin;
  AYShift := TcxButton(Sender).Height;
  Inc(AOrigin.Y, AYShift);

  AMaxColCount := FPreview.Width div MulDiv(TdxPreviewAccess(FPreview).PageSize.ActualSizeInPixels.X, FPreview.MinZoomFactor, 100);
  AMaxRowCount := FPreview.Height div MulDiv(TdxPreviewAccess(FPreview).PageSize.ActualSizeInPixels.Y, 2 * FPreview.MinZoomFactor, 100);
  if AMaxColCount = 0 then
    AMaxColCount := 1;
  if AMaxRowCount = 0 then
    AMaxRowCount := 1;

  if AMaxColCount > 3 then
    AColCount := 3
  else
    AColCount := AMaxColCount;

  if AMaxRowCount > 3 then
    ARowCount := 2
  else
    ARowCount := AMaxRowCount;

  if dxChooseMultiplePages(ilStub, 0, AOrigin, AYShift, AMaxColCount, AMaxRowCount, AColCount, ARowCount) then
  begin
    FPreview.ZoomMode := pzmPages;
    TdxPreviewAccess(FPreview).SetPageXYCount(AColCount, ARowCount);
    if rbtnManyPages.Checked then
    begin
      SetZoomFactor(FPreview.ZoomFactor);
      FModified := True;
      UpdateControlsState;
    end
    else
      rbtnManyPages.Checked := True;
  end;
end;

procedure TfmZoom.PreviewPaint(Sender: TObject);
const
  ScreenRect: TRect = (Left: 12; Top: 12; Right: 138; Bottom: 92);

  function GetPreviewBounds: TRect;
  begin
    Result := cxRect(dxGetImageSize(ilMonitor, ScaleFactor));
  end;

  procedure DrawImage(ACanvas: TcxCanvas; AImageIndex: Integer);
  begin
    cxDrawImage(ACanvas, GetPreviewBounds, nil, ilMonitor, AImageIndex, True, nil, ScaleFactor);
  end;

  procedure DrawPages(ACanvas: TcxCanvas);
  const
    BorderColor = $B06232;
    ContentColor = $FFEDE3;
  var
    APageBounds: TRect;
    AScreenBounds: TRect;
    I: Integer;
  begin
    AScreenBounds := ScaleFactor.Apply(ScreenRect);
    ACanvas.SaveClipRegion;
    try
      ACanvas.IntersectClipRect(AScreenBounds);
      TdxPreviewAccess(FPreview).CalculatePagesLayout;
      for I := 0 to FPreview.PageCount - 1 do
      begin
        APageBounds := TdxPreviewAccess(FPreview).Pages[I].Bounds;
        APageBounds := cxRectScale(APageBounds,
          cxRectWidth(AScreenBounds), FPreview.Width,
          cxRectHeight(AScreenBounds), FPreview.Height);
        APageBounds := cxRectOffset(APageBounds, AScreenBounds.Left, AScreenBounds.Top);
        if ACanvas.RectVisible(APageBounds) then
        begin
          ACanvas.FrameRect(APageBounds, BorderColor);
          InflateRect(APageBounds, -1, -1);
          ACanvas.FillRect(APageBounds, ContentColor);
        end;
      end;
    finally
      ACanvas.RestoreClipRegion;
    end;
  end;

begin
  if FUpdateCount = 0 then
  begin
    cxPaintCanvas.BeginPaint(pbPreview.Canvas);
    try
      DrawImage(cxPaintCanvas, 0);
      DrawPages(cxPaintCanvas);
      DrawImage(cxPaintCanvas, 1);
    finally
      cxPaintCanvas.EndPaint;
    end;
  end;
end;

procedure TfmZoom.FontPreviewPaint(Sender: TObject);
const
  Format = CXTO_PATTERNEDTEXT or CXTO_CENTER_HORIZONTALLY or CXTO_WORDBREAK or CXTO_CHARBREAK;
var
  Template, S: string;
  I: Integer;
  R: TRect;
begin
  if FUpdateCount <> 0 then Exit;

  Template := cxGetResourceString(@sdxZoomDlgFontPreviewString);
  S := '';
  for I := 0 to 6 do
  begin
    S := S + Template;
    if I <> 6 then
      S := S + #13#10;
  end;

  with TdxPSPaintPanel(Sender), Canvas do
  begin
    R := ClientRect;

    Brush.Color := clWindowText;
    FrameRect(R);
    InflateRect(R, -1, -1);
    Brush.Color := clWindow;
    FillRect(R);

    Font.Size := Round(12 * FPreview.ZoomFactor / 100);
    Font.Name := 'Times New Roman';
    cxTextOut(Handle, S, R, Format, nil, 0, 0);
  end;
end;

procedure TfmZoom.sePercentKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = Char(VK_RETURN) then
  begin
    SetZoomFactor(TcxSpinEdit(Sender).Value);
    UpdateControlsState;
  end;
end;

end.

