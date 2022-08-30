{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressFlowChart                                         }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSFLOWCHART AND ALL ACCOMPANYING }
{   VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY.              }
{                                                                    }
{   THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED       }
{   FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE         }
{   COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE        }
{   AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT   }
{   AND PERMISSION FROM DEVELOPER EXPRESS INC.                       }
{                                                                    }
{   CONSULT THE end USER LICENSE AGREEMENT FOR INFORMATION ON        }
{   ADDITIONAL RESTRICTIONS.                                         }
{                                                                    }
{********************************************************************}

unit dxEditObj;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls, ComCtrls, Menus, Buttons,
  dxflchrt, cxGraphics, cxLookAndFeels, cxLookAndFeelPainters, cxButtons, cxControls, cxContainer, cxEdit, cxCheckBox,
  cxTextEdit, cxMaskEdit, cxDropDownEdit, cxMemo, dxLayoutLookAndFeels, cxClasses, dxLayoutContainer, dxLayoutControl,
  dxLayoutcxEditAdapters, dxLayoutControlAdapters, cxListView, dxForms, dxColorDialog,
  cxSpinEdit, dxGalleryControl, dxGallery;

type
  { TFEditObject }

  TFEditObject = class(TdxForm)
    btnCancel: TcxButton;
    btnClear: TcxButton;
    btnOK: TcxButton;
    cbAdjust: TcxCheckBox;
    cbBottom: TcxCheckBox;
    cbDiag: TcxCheckBox;
    cbFlat: TcxCheckBox;
    cbImagePosition: TcxComboBox;
    cbLeft: TcxCheckBox;
    cbMiddle: TcxCheckBox;
    cbMono: TcxCheckBox;
    cbRaisedIn: TcxCheckBox;
    cbRaisedOut: TcxCheckBox;
    cbRight: TcxCheckBox;
    cbShapeStyle: TcxComboBox;
    cbSoft: TcxCheckBox;
    cbSunkenIn: TcxCheckBox;
    cbSunkenOut: TcxCheckBox;
    cbTextPosition: TcxComboBox;
    cbTop: TcxCheckBox;
    cbTransparent: TcxCheckBox;
    ColorDialog: TdxColorDialog;
    dxLayoutAutoCreatedGroup1: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup2: TdxLayoutAutoCreatedGroup;
    dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel;
    dxLayoutGroup10: TdxLayoutGroup;
    dxLayoutGroup12: TdxLayoutGroup;
    dxLayoutGroup13: TdxLayoutGroup;
    dxLayoutGroup14: TdxLayoutGroup;
    dxLayoutGroup3: TdxLayoutGroup;
    dxLayoutGroup2: TdxLayoutGroup;
    dxLayoutGroup4: TdxLayoutGroup;
    dxLayoutGroup5: TdxLayoutGroup;
    dxLayoutGroup7: TdxLayoutGroup;
    dxLayoutGroup9: TdxLayoutGroup;
    dxLayoutItem11: TdxLayoutItem;
    dxLayoutItem12: TdxLayoutItem;
    dxLayoutItem13: TdxLayoutItem;
    dxLayoutItem14: TdxLayoutItem;
    dxLayoutItem15: TdxLayoutItem;
    dxLayoutItem16: TdxLayoutItem;
    dxLayoutItem17: TdxLayoutItem;
    dxLayoutItem18: TdxLayoutItem;
    dxLayoutItem19: TdxLayoutItem;
    dxLayoutItem20: TdxLayoutItem;
    dxLayoutItem21: TdxLayoutItem;
    dxLayoutItem22: TdxLayoutItem;
    dxLayoutItem23: TdxLayoutItem;
    dxLayoutItem24: TdxLayoutItem;
    dxLayoutItem25: TdxLayoutItem;
    dxLayoutItem26: TdxLayoutItem;
    dxLayoutItem27: TdxLayoutItem;
    dxLayoutItem30: TdxLayoutItem;
    dxLayoutItem7: TdxLayoutItem;
    dxLayoutItem8: TdxLayoutItem;
    dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList;
    FontDialog: TFontDialog;
    GroupBox1: TdxLayoutGroup;
    GroupBox2: TdxLayoutGroup;
    Label1: TdxLayoutItem;
    Label2: TdxLayoutItem;
    Label3: TdxLayoutItem;
    Label5: TdxLayoutItem;
    Label8: TdxLayoutItem;
    Label9: TdxLayoutItem;
    lcMain: TdxLayoutControl;
    lcMainGroup_Root: TdxLayoutGroup;
    gcImages: TdxGalleryControl;
    gcgImages: TdxGalleryControlGroup;
    memoText: TcxMemo;
    pBkColor: TPanel;
    pColor: TPanel;
    sbFont: TcxButton;
    tsFrame: TdxLayoutGroup;
    tsGeneral: TdxLayoutGroup;
    tsImage: TdxLayoutGroup;
    seAngle: TcxSpinEdit;
    liAngle: TdxLayoutItem;
    seLineWidth: TcxSpinEdit;
    liLineWidth: TdxLayoutItem;
    seHeight: TcxSpinEdit;
    liHeight: TdxLayoutItem;
    seWidth: TcxSpinEdit;
    liWidth: TdxLayoutItem;

    procedure btnClearClick(Sender: TObject);
    procedure pColorClick(Sender: TObject);
    procedure sbFontClick(Sender: TObject);
    procedure seHeightChange(Sender: TObject);
    procedure gcImagesItemClick(Sender: TObject; AItem: TdxGalleryControlItem);
  private
    function GetBorderStyle: Integer;
    function GetEdgeStyle: Integer;
    function IsBorderStyleChanged: Boolean;
    function IsEdgeStyleChanged: Boolean;
    procedure SetBorderStyle(AValue: Integer);
    procedure SetEdgeStyle(AValue: Word);
    //
    function IsModified(AComponent: TComponent): Boolean;
    procedure ClearModifiedFlag;
    procedure LoadAdvancedShapes(AItems: TStrings; AChart: TdxFlowChart);
    procedure LoadLocalizations;
    procedure LoadParams(AChart: TdxFlowChart; AObject: TdxFcObject);
    procedure Modified(AObject: TObject);
    procedure SaveParams(AChart: TdxFlowChart; AObject: TdxFcObject);
  public
    constructor Create(AOwner: TComponent); override;
  end;

function ObjectEditor(Chart: TdxFlowChart; Obj: TdxFcObject): Boolean;
implementation

{$R *.DFM}

uses
  dxFcEdit, dxCore, dxFcStrs, dxCoreGraphics, dxFlowChartShapes;

function ObjectEditor(Chart: TdxFlowChart; Obj: TdxFcObject): Boolean;
var
  I: Integer;
begin
  with TFEditObject.Create(nil) do
  try
    LoadParams(Chart, Obj);
    Result := ShowModal = mrOk;
    if Result then
    begin
      for I := 0 to Chart.SelectedObjectCount - 1 do
        SaveParams(Chart, Chart.SelectedObjects[I]);
    end;
  finally
    Free;
  end;
  PostMessage(Chart.Handle, WM_LBUTTONUP, 0, MakeLParam(0, 0)); // Fix: by Kirill
end;

{ TFEditObject }

constructor TFEditObject.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  LoadLocalizations;
  pBkColor.ControlStyle := pBkColor.ControlStyle - [csParentBackground];
  pColor.ControlStyle := pColor.ControlStyle - [csParentBackground];
end;

procedure TFEditObject.gcImagesItemClick(Sender: TObject; AItem: TdxGalleryControlItem);
begin
  Modified(Sender);
end;

procedure TFEditObject.LoadParams(AChart: TdxFlowChart; AObject: TdxFcObject);
var
  I: Integer;
  AImageItem: TdxGalleryControlItem;
begin
  MemoText.Text := AObject.Text;
  cbTextPosition.ItemIndex := Integer(AObject.VertTextPos) * 3 + Integer(AObject.HorzTextPos);
  cbImagePosition.ItemIndex := Integer(AObject.VertImagePos) * 3 + Integer(AObject.HorzImagePos);
  seHeight.Value := AObject.Height;
  seWidth.Value := AObject.Width;
  seLineWidth.Value := AObject.ShapeWidth;
  liAngle.Enabled := AObject.ShapeType = fcsAdvanced;
  if liAngle.Enabled then
    seAngle.Value := Round(AObject.Angle);
  LoadAdvancedShapes(cbShapeStyle.Properties.Items, AChart);
  if AObject.ShapeType = fcsAdvanced then
    cbShapeStyle.ItemIndex := cbShapeStyle.Properties.Items.IndexOfObject(AObject.AdvancedShape)
  else
    cbShapeStyle.ItemIndex := Integer(AObject.ShapeType);

  gcgImages.Items.Clear;
  if AChart.Images <> nil then
  begin
    gcImages.Images := AChart.Images;
    for I := 0 to AChart.Images.Count - 1 do
    begin
      AImageItem := gcgImages.Items.Add;
      AImageItem.ImageIndex := I;
      AImageItem.Caption := IntToStr(I);
      if AObject.ImageIndex = I then
        AImageItem.Checked := True;
    end;
  end;
  sbFont.Font.Assign(AObject.Font);
  pColor.Color := AObject.ShapeColor;
  pBkColor.Color := AObject.BkColor;
  cbTransparent.Checked := AObject.Transparent;
  SetEdgeStyle(AObject.EdgeStyle);
  SetBorderStyle(AObject.BorderStyle);
  ClearModifiedFlag;
end;

function TFEditObject.IsModified(AComponent: TComponent): Boolean;
begin
  Result := AComponent.Tag = 1;
end;

procedure TFEditObject.ClearModifiedFlag;
var
  I: Integer;
begin
  for I := 0 to ComponentCount - 1 do
    Components[I].Tag := 0;
end;

procedure TFEditObject.LoadAdvancedShapes(AItems: TStrings; AChart: TdxFlowChart);
var
  AShapeIndex, AStencilIndex: Integer;
  AShape: TdxFlowChartObjectAdvancedShape;
  AStencil: TdxFlowChartAdvancedShapeStencil;
begin
  for AStencilIndex := 0 to AChart.Repository.StencilCount - 1 do
  begin
    AStencil := AChart.Repository.Stencils[AStencilIndex];
    for AShapeIndex := 0 to AStencil.Count - 1 do
    begin
      AShape := AStencil.Shapes[AShapeIndex];
      AItems.AddObject(AShape.Caption, AShape);
    end;
  end;
end;

procedure TFEditObject.LoadLocalizations;

  procedure LocalizeLayoutCombo(AItems: TStrings);
  var
    I: Integer;
  begin
    AItems.BeginUpdate;
    try
      AItems.Clear;
      for I := Low(dxFlowChartLayoutNamesMap) to High(dxFlowChartLayoutNamesMap) do
        AItems.Add(cxGetResourceString(dxFlowChartLayoutNamesMap[I]));
    finally
      AItems.EndUpdate;
    end;
  end;

  procedure LocalizeShapeTypeCombo(AItems: TStrings);
  var
    AType: TdxFcShapeType;
  begin
    AItems.BeginUpdate;
    try
      AItems.Clear;
      for AType := fcsNone to fcsHexagon do
        AItems.Add(cxGetResourceString(dxFlowChartShapeNamesMap[AType]));
    finally
      AItems.EndUpdate;
    end;
  end;

begin
  LocalizeLayoutCombo(cbTextPosition.Properties.Items);
  LocalizeLayoutCombo(cbImagePosition.Properties.Items);
  LocalizeShapeTypeCombo(cbShapeStyle.Properties.Items);

  Caption := cxGetResourceString(@sdxFlowChartObjectEditorCaption);
  btnOK.Caption := cxGetResourceString(@sdxFlowChartDialogButtonOk);
  btnCancel.Caption := cxGetResourceString(@sdxFlowChartDialogButtonCancel);
  sbFont.Hint := cxGetResourceString(@sdxFlowChartConnectionEditorTextFontHint);

  tsFrame.Caption := cxGetResourceString(@sdxFlowChartObjectEditorFrameTab);
  tsGeneral.Caption := cxGetResourceString(@sdxFlowChartObjectEditorGeneralTab);
  tsImage.Caption := cxGetResourceString(@sdxFlowChartObjectEditorImageTab);

  cbTransparent.Caption := cxGetResourceString(@sdxFlowChartObjectEditorTransparent);
  Label1.Caption := cxGetResourceString(@sdxFlowChartObjectEditorText);
  Label2.Caption := cxGetResourceString(@sdxFlowChartObjectEditorTextLayout);
  Label3.Caption := cxGetResourceString(@sdxFlowChartObjectEditorShapeType);
  liLineWidth.Caption := cxGetResourceString(@sdxFlowChartObjectEditorLineWidth);
  liHeight.Caption := cxGetResourceString(@sdxFlowChartObjectEditorHeight);
  liWidth.Caption := cxGetResourceString(@sdxFlowChartObjectEditorWidth);
  Label8.Caption := cxGetResourceString(@sdxFlowChartObjectEditorShapeColor);
  Label9.Caption := cxGetResourceString(@sdxFlowChartObjectEditorBackgroundColor);

  Label5.Caption := cxGetResourceString(@sdxFlowChartObjectEditorImageLayout);
  btnClear.Caption := cxGetResourceString(@sdxFlowChartObjectEditorImageClear);

  GroupBox1.Caption := cxGetResourceString(@sdxFlowChartObjectEditorEdgeStyle);
  GroupBox2.Caption := cxGetResourceString(@sdxFlowChartObjectEditorBorderStyle);

  cbRaisedIn.Caption := cxGetResourceString(@sdxFlowChartEdgeStyleRaisedIn);
  cbRaisedOut.Caption := cxGetResourceString(@sdxFlowChartEdgeStyleRaisedOut);
  cbSunkenIn.Caption := cxGetResourceString(@sdxFlowChartEdgeStyleSunkenIn);
  cbSunkenOut.Caption := cxGetResourceString(@sdxFlowChartEdgeStyleSunkenOut);

  cbAdjust.Caption := cxGetResourceString(@sdxFlowChartBorderStyleAdjust);
  cbBottom.Caption := cxGetResourceString(@sdxFlowChartBorderStyleBottom);
  cbDiag.Caption := cxGetResourceString(@sdxFlowChartBorderStyleDiagonal);
  cbFlat.Caption := cxGetResourceString(@sdxFlowChartBorderStyleFlat);
  cbLeft.Caption := cxGetResourceString(@sdxFlowChartBorderStyleLeft);
  cbMiddle.Caption := cxGetResourceString(@sdxFlowChartBorderStyleMiddle);
  cbMono.Caption := cxGetResourceString(@sdxFlowChartBorderStyleMono);
  cbRight.Caption := cxGetResourceString(@sdxFlowChartBorderStyleRight);
  cbSoft.Caption := cxGetResourceString(@sdxFlowChartBorderStyleSoft);
  cbTop.Caption := cxGetResourceString(@sdxFlowChartBorderStyleTop);
end;

procedure TFEditObject.Modified(AObject: TObject);
begin
  if AObject is TComponent then
    TComponent(AObject).Tag := 1;
end;

procedure TFEditObject.SaveParams(AChart: TdxFlowChart; AObject: TdxFcObject);
var
  AItemObject: TObject;
begin
  if IsModified(cbShapeStyle) then
  begin
    AItemObject := cbShapeStyle.Properties.Items.Objects[cbShapeStyle.ItemIndex];
    if AItemObject <> nil then
      AObject.AdvancedShape := TdxFlowChartObjectAdvancedShape(AItemObject)
    else
      AObject.ShapeType := TdxFcShapeType(cbShapeStyle.ItemIndex);
  end;
  if IsModified(seLineWidth) then
    AObject.ShapeWidth := seLineWidth.Value;
  if IsModified(pColor) then
    AObject.ShapeColor := pColor.Color;
  if IsModified(pBkColor) then
    AObject.BkColor := pBkColor.Color;
  if IsModified(cbTextPosition) then
    AObject.HorzTextPos := TdxFcHorzPos(cbTextPosition.ItemIndex mod 3);
  if IsModified(cbTextPosition) then
    AObject.VertTextPos := TdxFcVertPos(cbTextPosition.ItemIndex div 3);
  if IsModified(sbFont) then
    AObject.Font.Assign(sbFont.Font);
  if IsModified(MemoText) then
    AObject.Text := MemoText.Text;
  if IsModified(seHeight) then
    AObject.Height := seHeight.Value;
  if IsModified(seWidth) then
    AObject.Width := seWidth.Value;
  if (AChart.Images <> nil) and IsModified(gcImages) then
  begin
    if gcImages.Gallery.GetCheckedItem = nil then
      AObject.ImageIndex := -1
    else
      AObject.ImageIndex := gcImages.Gallery.GetCheckedItem.ImageIndex;
  end;
  if IsModified(cbImagePosition) then
    AObject.HorzImagePos := TdxFcHorzPos(cbImagePosition.ItemIndex mod 3);
  if IsModified(cbImagePosition) then
    AObject.VertImagePos := TdxFcVertPos(cbImagePosition.ItemIndex div 3);
  if IsModified(cbTransparent) then
    AObject.Transparent := cbTransparent.Checked;
  if IsEdgeStyleChanged then
    AObject.EdgeStyle := GetEdgeStyle;
  if IsBorderStyleChanged then
    AObject.BorderStyle := GetBorderStyle;
  if (AObject.ShapeType = fcsAdvanced) and IsModified(seAngle) then
    AObject.Angle := seAngle.Value;
end;

procedure TFEditObject.btnClearClick(Sender: TObject);
begin
  if gcImages.Gallery.GetCheckedItem <> nil then
    gcImages.Gallery.GetCheckedItem.Checked := False;
  Modified(gcImages);
end;

procedure TFEditObject.sbFontClick(Sender: TObject);
begin
  FontDialog.Font.Assign(sbFont.Font);
  if FontDialog.Execute then
  begin
    sbFont.Font.Assign(FontDialog.Font);
    Modified(sbFont);
  end;
end;

procedure TFEditObject.pColorClick(Sender: TObject);
begin
  ColorDialog.Color := dxColorToAlphaColor(TPanel(Sender).Color);
  if ColorDialog.Execute then
  begin
    TPanel(Sender).Color := dxAlphaColorToColor(ColorDialog.Color);
    Modified(Sender);
  end;
end;

function TFEditObject.GetEdgeStyle: Integer;
begin
  Result := 0;
  if cbRaisedOut.Checked then
    Result := Result or BDR_RAISEDOUTER;
  if cbSunkenOut.Checked then
    Result := Result or BDR_SUNKENOUTER;
  if cbRaisedIn.Checked then
    Result := Result or BDR_RAISEDINNER;
  if cbSunkenIn.Checked then
    Result := Result or BDR_SUNKENINNER;
end;

procedure TFEditObject.SetEdgeStyle(AValue: Word);
begin
  cbRaisedIn.Checked  := AValue and BDR_RAISEDINNER <> 0;
  cbRaisedOut.Checked := AValue and BDR_RAISEDOUTER <> 0;
  cbSunkenIn.Checked  := AValue and BDR_SUNKENINNER <> 0;
  cbSunkenOut.Checked := AValue and BDR_SUNKENOUTER <> 0;
end;

function TFEditObject.GetBorderStyle: Integer;
begin
  Result := 0;
  if cbLeft.Checked then Result := Result or BF_LEFT;
  if cbTop.Checked then Result := Result or BF_TOP;
  if cbRight.Checked then Result := Result or BF_RIGHT;
  if cbBottom.Checked then Result := Result or BF_BOTTOM;
  if cbDiag.Checked then Result := Result or BF_DIAGONAL;
  if cbMiddle.Checked then Result := Result or BF_MIDDLE;
  if cbSoft.Checked then Result := Result or BF_SOFT;
  if cbAdjust.Checked then Result := Result or BF_ADJUST;
  if cbFlat.Checked then Result := Result or BF_FLAT;
  if cbMono.Checked then Result := Result or BF_MONO;
end;

procedure TFEditObject.SetBorderStyle(AValue: Integer);
begin
  cbAdjust.Checked := (AValue and BF_ADJUST) <> 0;
  cbBottom.Checked := (AValue and BF_BOTTOM) <> 0;
  cbDiag.Checked := (AValue and BF_DIAGONAL) <> 0;
  cbFlat.Checked := (AValue and BF_FLAT) <> 0;
  cbLeft.Checked := (AValue and BF_LEFT) <> 0;
  cbMiddle.Checked := (AValue and BF_MIDDLE) <> 0;
  cbMono.Checked := (AValue and BF_MONO) <> 0;
  cbRight.Checked := (AValue and BF_RIGHT) <> 0;
  cbSoft.Checked := (AValue and BF_SOFT) <> 0;
  cbTop.Checked := (AValue and BF_TOP) <> 0;
end;

function TFEditObject.IsEdgeStyleChanged: Boolean;
begin
  Result := IsModified(cbRaisedIn) or IsModified(cbSunkenOut) or IsModified(cbRaisedOut) or IsModified(cbSunkenIn);
end;

function TFEditObject.IsBorderStyleChanged: Boolean;
begin
  Result :=
    IsModified(cbLeft) or IsModified(cbTop) or IsModified(cbRight) or IsModified(cbBottom) or
    IsModified(cbDiag) or IsModified(cbMiddle) or IsModified(cbSoft) or IsModified(cbAdjust) or
    IsModified(cbFlat) or IsModified(cbMono);
end;

procedure TFEditObject.seHeightChange(Sender: TObject);
begin
  Modified(Sender);
end;

end.
