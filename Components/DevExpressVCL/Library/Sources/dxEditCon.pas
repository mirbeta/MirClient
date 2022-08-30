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

unit dxEditCon;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, Buttons, Menus, ExtCtrls,
  dxflchrt, cxGraphics, cxLookAndFeels, cxLookAndFeelPainters, cxButtons, cxControls, cxContainer, cxEdit, cxTextEdit,
  cxMaskEdit, cxDropDownEdit, cxMemo, dxLayoutcxEditAdapters, dxLayoutControlAdapters, dxLayoutContainer, cxClasses,
  dxLayoutControl, dxLayoutLookAndFeels, dxForms, dxColorDialog, cxSpinEdit;

type
  { TFEditConnection }

  TFEditConnection = class(TdxForm)
    btnCancel: TcxButton;
    btnOK: TcxButton;
    cbDArrowStyle: TcxComboBox;
    cbSArrowStyle: TcxComboBox;
    ColorDialog: TdxColorDialog;
    dxLayoutAutoCreatedGroup1: TdxLayoutAutoCreatedGroup;
    dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel;
    dxLayoutGroup1: TdxLayoutGroup;
    dxLayoutGroup2: TdxLayoutGroup;
    dxLayoutGroup3: TdxLayoutGroup;
    dxLayoutGroup9: TdxLayoutGroup;
    dxLayoutItem10: TdxLayoutItem;
    dxLayoutItem2: TdxLayoutItem;
    dxLayoutItem9: TdxLayoutItem;
    dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList;
    dxLayoutSeparatorItem1: TdxLayoutSeparatorItem;
    FontDialog: TFontDialog;
    lcMain: TdxLayoutControl;
    lcMainGroup_Root: TdxLayoutGroup;
    lgSource: TdxLayoutGroup;
    lgTarget: TdxLayoutGroup;
    liColor: TdxLayoutItem;
    liArrowStyle: TdxLayoutItem;
    liArrowStyle2: TdxLayoutItem;
    liSourceColor: TdxLayoutItem;
    liText: TdxLayoutItem;
    MemoText: TcxMemo;
    pSourceColor: TPanel;
    pColor: TPanel;
    sbFont: TcxButton;
    seStrokeThickness: TcxSpinEdit;
    liStrokeThickness: TdxLayoutItem;
    seDArrowSize: TcxSpinEdit;
    liDArrowSize: TdxLayoutItem;
    seSArrowSize: TcxSpinEdit;
    liSArrowSize: TdxLayoutItem;
    seSLinkedPoint: TcxSpinEdit;
    liSLinkedPoint: TdxLayoutItem;
    seDLinkedPoint: TcxSpinEdit;
    liDLinkedPoint: TdxLayoutItem;
    pDestColor: TPanel;
    liDestColor: TdxLayoutItem;
    cbLineStyle: TcxComboBox;
    liLineStyle: TdxLayoutItem;

    procedure MemoTextChange(Sender: TObject);
    procedure pColorClick(Sender: TObject);
    procedure sbFontClick(Sender: TObject);
  protected
    function IsModified(AComponent: TComponent): Boolean;
    procedure ClearModifiedFlag;
    procedure LoadLocalizations;
    procedure LoadParams(AConnection: TdxFcConnection);
    procedure Modified(AObject: TObject);
    procedure SaveParams(AConnection: TdxFcConnection);
  public
    constructor Create(AOwner: TComponent); override;
  end;

function ConnectEditor(AChart: TdxFlowChart; AConnection: TdxFcConnection): Boolean;

implementation

{$R *.DFM}

uses
  Math, dxFcEdit, dxFcStrs, dxCore, dxCoreGraphics;

function ConnectEditor(AChart: TdxFlowChart; AConnection: TdxFcConnection): Boolean;
var
  I: Integer;
begin
  with TFEditConnection.Create(nil) do
  try
    LoadParams(AConnection);
    Result := ShowModal = mrOK;
    if Result then
    begin
      for I := 0 to AChart.SelectedConnectionCount - 1 do
        SaveParams(AChart.SelectedConnections[I]);
    end;
  finally
    Free;
  end;
end;

type
  TdxFcObjectHelper = class helper for TdxFcObject
  public
    function GetLinkedPointCount: Integer;
  end;

{ TdxFcObjectHelper }

function TdxFcObjectHelper.GetLinkedPointCount: Integer;
begin
  Result := inherited GetLinkedPointCount;
end;

{ TFEditConnection }

constructor TFEditConnection.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  pColor.ControlStyle := pColor.ControlStyle - [csParentBackground];
  pSourceColor.ControlStyle := pSourceColor.ControlStyle - [csParentBackground];
  pDestColor.ControlStyle := pDestColor.ControlStyle - [csParentBackground];
  LoadLocalizations;
end;

procedure TFEditConnection.ClearModifiedFlag;
var
  I: Integer;
begin
  for I := 0 to ComponentCount - 1 do
    Components[I].Tag := 0;
end;

procedure TFEditConnection.LoadLocalizations;

  procedure LocalizeArrowStyleCombo(AComboItems: TStrings);
  var
    AType: TdxFcaType;
  begin
    AComboItems.BeginUpdate;
    try
      AComboItems.Clear;
      for AType := Low(AType) to High(AType) do
        AComboItems.Add(cxGetResourceString(dxFlowChartArrowStyleNamesMap[AType]));
    finally
      AComboItems.EndUpdate;
    end;
  end;

begin
  LocalizeArrowStyleCombo(cbSArrowStyle.Properties.Items);
  LocalizeArrowStyleCombo(cbDArrowStyle.Properties.Items);
  Caption := cxGetResourceString(@sdxFlowChartConnectionEditorCaption);
  btnCancel.Caption := cxGetResourceString(@sdxFlowChartDialogButtonCancel);
  btnOK.Caption := cxGetResourceString(@sdxFlowChartDialogButtonOk);
  lgSource.Caption := cxGetResourceString(@sdxFlowChartConnectionEditorSource);
  lgTarget.Caption := cxGetResourceString(@sdxFlowChartConnectionEditorDestination);

  liText.Caption := cxGetResourceString(@sdxFlowChartConnectionEditorText);
  liColor.Caption := cxGetResourceString(@sdxFlowChartConnectionEditorColor);
  liSourceColor.Caption := cxGetResourceString(@sdxFlowChartConnectionEditorColor);
  liDestColor.Caption := cxGetResourceString(@sdxFlowChartConnectionEditorColor);
  liSArrowSize.Caption := cxGetResourceString(@sdxFlowChartConnectionEditorArrowSize);
  liDArrowSize.Caption := cxGetResourceString(@sdxFlowChartConnectionEditorArrowSize);
  liArrowStyle.Caption := cxGetResourceString(@sdxFlowChartConnectionEditorArrowStyle);
  liArrowStyle2.Caption := cxGetResourceString(@sdxFlowChartConnectionEditorArrowStyle);
  liSLinkedPoint.Caption := cxGetResourceString(@sdxFlowChartConnectionEditorLinkedPoint);
  liDLinkedPoint.Caption := cxGetResourceString(@sdxFlowChartConnectionEditorLinkedPoint);

  sbFont.Hint := cxGetResourceString(@sdxFlowChartConnectionEditorTextFontHint);
end;

procedure TFEditConnection.LoadParams(AConnection: TdxFcConnection);
var
  AItemIndex: Integer;
begin
  MemoText.Text := AConnection.Text;
  sbFont.Font.Assign(AConnection.Font);
  seDArrowSize.Value := AConnection.ArrowDest.Height;
  seSArrowSize.Value := AConnection.ArrowSource.Width;
  cbSArrowStyle.ItemIndex := Integer(AConnection.ArrowSource.ArrowType);
  cbDArrowStyle.ItemIndex := Integer(AConnection.ArrowDest.ArrowType);
  liSLinkedPoint.Enabled := (AConnection.ObjectSource <> nil) and (AConnection.ObjectSource.GetLinkedPointCount > 0);
  if liSLinkedPoint.Enabled then
  begin
    seSLinkedPoint.Properties.MaxValue := AConnection.ObjectSource.GetLinkedPointCount;
    seSLinkedPoint.Properties.MinValue := 1;
    seSLinkedPoint.Value := AConnection.PointSource + 1;
  end;
  liDLinkedPoint.Enabled := (AConnection.ObjectDest <> nil) and (AConnection.ObjectDest.GetLinkedPointCount > 0);
  if liDLinkedPoint.Enabled then
  begin
    seDLinkedPoint.Properties.MaxValue := AConnection.ObjectDest.GetLinkedPointCount;
    seDLinkedPoint.Properties.MinValue := 1;
    seDLinkedPoint.Value := AConnection.PointDest + 1;
  end;
  pColor.Color := AConnection.Color;
  pSourceColor.Color := AConnection.ArrowSource.Color;
  pDestColor.Color := AConnection.ArrowDest.Color;
  seStrokeThickness.Value := AConnection.PenWidth;
  AItemIndex := Integer(AConnection.PenStyle);
  if AItemIndex > 4 then
    AItemIndex := -1;
  cbLineStyle.ItemIndex := AItemIndex;
  ClearModifiedFlag;
end;

function TFEditConnection.IsModified(AComponent: TComponent): Boolean;
begin
  Result := AComponent.Tag = 1;
end;

procedure TFEditConnection.Modified(AObject: TObject);
begin
  if AObject is TComponent then
    TComponent(AObject).Tag := 1;
end;

procedure TFEditConnection.sbFontClick(Sender: TObject);
begin
  FontDialog.Font.Assign(sbFont.Font);
  if FontDialog.Execute then
  begin
    sbFont.Font.Assign(FontDialog.Font);
    Modified(Sender);
  end;
end;

procedure TFEditConnection.pColorClick(Sender: TObject);
begin
  ColorDialog.Color := dxColorToAlphaColor(TPanel(Sender).Color);
  if ColorDialog.Execute then
  begin
    TPanel(Sender).Color := dxAlphaColorToColor(ColorDialog.Color);
    Modified(Sender);
  end;
end;

procedure TFEditConnection.MemoTextChange(Sender: TObject);
begin
  Modified(Sender);
end;

procedure TFEditConnection.SaveParams(AConnection: TdxFcConnection);
var
  AItemIndex: Integer;
begin
  if IsModified(MemoText) then
    AConnection.Text := MemoText.Text;
  if IsModified(sbFont) then
    AConnection.Font.Assign(sbFont.Font);
  if IsModified(cbSArrowStyle) then
    AConnection.ArrowSource.ArrowType := TdxFcaType(cbSArrowStyle.ItemIndex);
  if IsModified(cbDArrowStyle) then
    AConnection.ArrowDest.ArrowType := TdxFcaType(cbDArrowStyle.ItemIndex);
  if IsModified(seSArrowSize) then
    AConnection.ArrowSource.Width := seSArrowSize.Value;
  if IsModified(seSArrowSize) then
    AConnection.ArrowSource.Height := seSArrowSize.Value;
  if IsModified(seDArrowSize) then
    AConnection.ArrowDest.Width := seDArrowSize.Value;
  if IsModified(seDArrowSize) then
    AConnection.ArrowDest.Height := seDArrowSize.Value;
  if IsModified(seSLinkedPoint) then
    AConnection.SetObjectSource(AConnection.ObjectSource, seSLinkedPoint.Value - 1);
  if IsModified(seDLinkedPoint) then
    AConnection.SetObjectDest(AConnection.ObjectDest, seDLinkedPoint.Value - 1);
  if IsModified(pColor) then
    AConnection.Color := pColor.Color;
  if IsModified(pSourceColor) then
    AConnection.ArrowSource.Color := pSourceColor.Color;
  if IsModified(pDestColor) then
    AConnection.ArrowDest.Color := pDestColor.Color;
  if IsModified(seStrokeThickness) then
    AConnection.PenWidth := seStrokeThickness.Value;
  if IsModified(cbLineStyle) then
  begin
    AItemIndex := Max(cbLineStyle.ItemIndex, 0);
    AConnection.PenStyle := TPenStyle(AItemIndex);
  end;
end;

end.
