{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           Express OrgChart                                         }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSORGCHART AND ALL ACCOMPANYING  }
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

unit dxorgcedadv;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, Buttons, ExtCtrls, Menus, dxCore,
  dxorgchr, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit, cxGroupBox, cxLabel,
  cxButtons, cxTextEdit, cxMaskEdit, cxDropDownEdit, cxColorComboBox, dxLayoutLookAndFeels, cxClasses,
  dxLayoutContainer, dxLayoutControl, dxLayoutControlAdapters, dxLayoutcxEditAdapters;

type
  { TfrmOrgChartAdvEditor }

  TfrmOrgChartAdvEditor = class(TdxOrgChartCustomCustomizeForm)
    AlignEdit: TcxComboBox;
    btnAntialiasing: TcxButton;
    CancelButton: TcxButton;
    CInsButton: TcxButton;
    ColorEdit: TcxColorComboBox;
    DelButton: TcxButton;
    dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel;
    dxLayoutGroup1: TdxLayoutGroup;
    dxLayoutGroup3: TdxLayoutGroup;
    dxLayoutGroup5: TdxLayoutGroup;
    dxLayoutGroup6: TdxLayoutGroup;
    dxLayoutGroup7: TdxLayoutGroup;
    dxLayoutGroup8: TdxLayoutGroup;
    dxLayoutGroup9: TdxLayoutGroup;
    dxLayoutItem1: TdxLayoutItem;
    dxLayoutItem16: TdxLayoutItem;
    dxLayoutItem17: TdxLayoutItem;
    dxLayoutItem18: TdxLayoutItem;
    dxLayoutItem2: TdxLayoutItem;
    dxLayoutItem3: TdxLayoutItem;
    dxLayoutItem4: TdxLayoutItem;
    dxLayoutItem5: TdxLayoutItem;
    dxLayoutItem6: TdxLayoutItem;
    dxLayoutItem7: TdxLayoutItem;
    dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList;
    HeightEdit: TcxTextEdit;
    IAEdit: TcxComboBox;
    IIEdit: TcxTextEdit;
    InsButton: TcxButton;
    Label1: TdxLayoutItem;
    Label2: TdxLayoutItem;
    Label3: TdxLayoutItem;
    Label4: TdxLayoutItem;
    Label5: TdxLayoutItem;
    Label6: TdxLayoutItem;
    Label7: TdxLayoutItem;
    Label8: TdxLayoutItem;
    lcMain: TdxLayoutControl;
    lcMainGroup_Root: TdxLayoutGroup;
    MultiButton: TcxButton;
    OKButton: TcxButton;
    PropBox: TdxLayoutGroup;
    RotateButton: TcxButton;
    ShapeEdit: TcxComboBox;
    Tree: TdxOrgChart;
    TreeBox: TdxLayoutGroup;
    TTEdit: TcxTextEdit;
    WidthEdit: TcxTextEdit;
    ZoomButton: TcxButton;

    procedure AlignEditClick(Sender: TObject);
    procedure AlignEditExit(Sender: TObject);
    procedure btnAntialiasingClick(Sender: TObject);
    procedure CInsButtonClick(Sender: TObject);
    procedure ColorEditExit(Sender: TObject);
    procedure ColorEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure DelButtonClick(Sender: TObject);
    procedure FixButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure HeightEditExit(Sender: TObject);
    procedure IAEditExit(Sender: TObject);
    procedure IIEditExit(Sender: TObject);
    procedure InsButtonClick(Sender: TObject);
    procedure Label1Click(Sender: TObject);
    procedure MultiButtonClick(Sender: TObject);
    procedure RotateButtonClick(Sender: TObject);
    procedure SetButtonClick(Sender: TObject);
    procedure SetNodeInfo(Node: TdxOcNode);
    procedure ShapeEditClick(Sender: TObject);
    procedure ShapeEditExit(Sender: TObject);
    procedure TreeChange(Sender: TObject; Node: TdxOcNode);
    procedure TTEditExit(Sender: TObject);
    procedure WidthEditExit(Sender: TObject);
    procedure WidthEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ZoomButtonClick(Sender: TObject);
  protected
    function GetPreviewOrgChart: TdxCustomOrgChart; override;
    procedure PreparePreview(APreview, ASource: TdxCustomOrgChart); override;
    procedure ScaleFactorChanged(M: Integer; D: Integer); override;
  end;

implementation

uses
  cxGeometry, dxorgchrstrs;

{$R *.DFM}

{ TfrmOrgChartAdvEditor }

procedure TfrmOrgChartAdvEditor.FormCreate(Sender: TObject);
begin
  Caption := cxGetResourceString(@sdxOrgChartEditorCaption);
  TreeBox.Caption := cxGetResourceString(@sdxOrgChartEditorItems);
  PropBox.Caption := cxGetResourceString(@sdxOrgChartEditorProperties);
  CancelButton.Caption := cxGetResourceString(@sdxOrgChartEditorCancelButton);

  CInsButton.Hint := cxGetResourceString(@sdxOrgChartEditorHintInsertSubItem);
  DelButton.Hint := cxGetResourceString(@sdxOrgChartEditorHintDeleteItem);
  InsButton.Hint := cxGetResourceString(@sdxOrgChartEditorHintInsertItem);
  MultiButton.Hint := cxGetResourceString(@sdxOrgChartEditorHintApplyForAllChildren);
  RotateButton.Hint := cxGetResourceString(@sdxOrgChartEditorHintRotate);
  ZoomButton.Hint := cxGetResourceString(@sdxOrgChartEditorHintZoom);
  btnAntialiasing.Hint := cxGetResourceString(@sdxOrgChartEditorHintAntialiasing);

  Label1.Caption := cxGetResourceString(@sdxOrgChartEditorWidth);
  Label2.Caption := cxGetResourceString(@sdxOrgChartEditorHeight);
  Label3.Caption := cxGetResourceString(@sdxOrgChartEditorColor);
  Label4.Caption := cxGetResourceString(@sdxOrgChartEditorChildAlign);
  Label5.Caption := cxGetResourceString(@sdxOrgChartEditorShape);
  Label6.Caption := cxGetResourceString(@sdxOrgChartEditorImageIndex);
  Label7.Caption := cxGetResourceString(@sdxOrgChartEditorImageAlign);
  Label8.Caption := cxGetResourceString(@sdxOrgChartEditorText);

  Constraints.MinWidth := Width;
  Constraints.MinHeight := Height;
end;

procedure TfrmOrgChartAdvEditor.Label1Click(Sender: TObject);
begin
  (Sender as TcxLabel).FocusControl.SetFocus;
end;

procedure TfrmOrgChartAdvEditor.InsButtonClick(Sender: TObject);
begin
  InsertNode;
end;

procedure TfrmOrgChartAdvEditor.CInsButtonClick(Sender: TObject);
begin
  InsertSubNode;
end;

procedure TfrmOrgChartAdvEditor.DelButtonClick(Sender: TObject);
begin
  if Tree.Selected <> nil then
    Tree.Delete(Tree.Selected);
end;

procedure TfrmOrgChartAdvEditor.ZoomButtonClick(Sender: TObject);
begin
  Tree.Zoom := ZoomButton.Down;
end;

procedure TfrmOrgChartAdvEditor.TreeChange(Sender: TObject; Node: TdxOcNode);
begin
  if Node = nil then
    PropBox.Enabled := False
  else
  begin
    PropBox.Enabled := True;
    TTEdit.Text := Node.Text;
    Node.GetNodeInfo(NodeInfo);
    ColorEdit.ColorValue := NodeInfo.Color;
    WidthEdit.Text := IntToStr(NodeInfo.Width);
    HeightEdit.Text := IntToStr(NodeInfo.Height);
    AlignEdit.ItemIndex := Ord(NodeInfo.Align);
    ShapeEdit.ItemIndex := Ord(NodeInfo.Shape);
    IIEdit.Text := IntToStr(NodeInfo.Index);
    IAEdit.ItemIndex := Ord(NodeInfo.IAlign);
  end;
end;

procedure TfrmOrgChartAdvEditor.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Tree.OnChange := nil;
end;

procedure TfrmOrgChartAdvEditor.SetNodeInfo(Node: TdxOcNode);
begin
  if Node = nil then
    Exit;
  Node.Width := NodeInfo.Width;
  Node.Height := NodeInfo.Height;
  Node.Color := NodeInfo.Color;
  Node.ChildAlign := NodeInfo.Align;
  Node.Shape := NodeInfo.Shape;
  Node.ImageIndex := NodeInfo.Index;
  Node.ImageAlign := NodeInfo.IAlign;
end;

procedure TfrmOrgChartAdvEditor.WidthEditExit(Sender: TObject);
begin
  NodeInfo.Width := StrToInt(WidthEdit.Text);
  SetNodeInfo(Tree.Selected);
end;

procedure TfrmOrgChartAdvEditor.HeightEditExit(Sender: TObject);
begin
  NodeInfo.Height := StrToInt(HeightEdit.Text);
  SetNodeInfo(Tree.Selected);
end;

procedure TfrmOrgChartAdvEditor.ColorEditExit(Sender: TObject);
begin
  NodeInfo.Color := ColorEdit.ColorValue;
  SetNodeInfo(Tree.Selected);
end;

procedure TfrmOrgChartAdvEditor.AlignEditExit(Sender: TObject);
begin
  NodeInfo.Align := TdxOcNodeAlign(AlignEdit.ItemIndex);
  SetNodeInfo(Tree.Selected);
end;

procedure TfrmOrgChartAdvEditor.btnAntialiasingClick(Sender: TObject);
begin
  Tree.Antialiasing := btnAntialiasing.Down;
end;

procedure TfrmOrgChartAdvEditor.ShapeEditExit(Sender: TObject);
begin
  NodeInfo.Shape := TdxOcShape(ShapeEdit.ItemIndex);
  SetNodeInfo(Tree.Selected);
end;

procedure TfrmOrgChartAdvEditor.ScaleFactorChanged(M, D: Integer);
begin
  inherited;
  TreeChange(Tree, Tree.Selected);
end;

procedure TfrmOrgChartAdvEditor.SetButtonClick(Sender: TObject);
begin
  Tree.SetFocus;
  SetNodeInfo(Tree.Selected);
end;

procedure TfrmOrgChartAdvEditor.MultiButtonClick(Sender: TObject);

  procedure DoSetNodeInfo(Node: TdxOcNode);
  var
    I: Integer;
  begin
    SetNodeInfo(Node);
    for I := 0 to Node.Count-1 do
      DoSetNodeInfo(Node[I]);
  end;

var
  I: Integer;
begin
  Tree.SetFocus;
  for I := 0 to Tree.Selected.Count - 1 do
    DoSetNodeInfo(Tree.Selected[I]);
end;

procedure TfrmOrgChartAdvEditor.FixButtonClick(Sender: TObject);
begin
  TreeChange(Tree, Tree.Selected);
end;

procedure TfrmOrgChartAdvEditor.WidthEditKeyDown(
  Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
  begin
    (Sender as TcxTextEdit).SelectAll;
    (Sender as TcxTextEdit).OnExit(Sender);
  end;
end;

procedure TfrmOrgChartAdvEditor.ColorEditKeyDown(
  Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (not ColorEdit.DroppedDown) and (Key = VK_RETURN) then
  begin
    ColorEdit.SelectAll;
    ColorEditExit(Sender);
  end;
end;

procedure TfrmOrgChartAdvEditor.IIEditExit(Sender: TObject);
begin
  NodeInfo.Index := StrToInt(IIEdit.Text);
  SetNodeInfo(Tree.Selected);
end;

procedure TfrmOrgChartAdvEditor.IAEditExit(Sender: TObject);
begin
  NodeInfo.IAlign := TdxOcImageAlign(IAEdit.ItemIndex);
  SetNodeInfo(Tree.Selected);
end;

procedure TfrmOrgChartAdvEditor.RotateButtonClick(Sender: TObject);
begin
  Tree.Rotated := RotateButton.Down;
end;

procedure TfrmOrgChartAdvEditor.AlignEditClick(Sender: TObject);
begin
  AlignEditExit(Sender);
end;

procedure TfrmOrgChartAdvEditor.ShapeEditClick(Sender: TObject);
begin
  ShapeEditExit(Sender);
end;

procedure TfrmOrgChartAdvEditor.TTEditExit(Sender: TObject);
begin
  if Tree.Selected <> nil then
    Tree.Selected.Text := TTEdit.Text;
end;

function TfrmOrgChartAdvEditor.GetPreviewOrgChart: TdxCustomOrgChart;
begin
  Result := Tree;
end;

procedure TfrmOrgChartAdvEditor.PreparePreview(APreview, ASource: TdxCustomOrgChart);
begin
  inherited PreparePreview(APreview, ASource);
  btnAntialiasing.Down := Tree.Antialiasing;
  RotateButton.Down := Tree.Rotated;
  ZoomButton.Down := Tree.Zoom;
  TreeChange(Tree, Tree.Selected);
end;

initialization
  dxOrgChartCustomizeFormManager.Register(TfrmOrgChartAdvEditor);
finalization
  dxOrgChartCustomizeFormManager.Unregister(TfrmOrgChartAdvEditor);
end.
