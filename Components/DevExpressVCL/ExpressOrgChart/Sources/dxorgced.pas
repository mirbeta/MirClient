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

unit dxorgced;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls,
  dxCore, dxorgchr, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters;

type
  { TfrmEChartEditor }

  TfrmEChartEditor = class(TdxOrgChartCustomCustomizeForm)
    AlignEdit: TComboBox;
    btnAntialiasing: TSpeedButton;
    CancelButton: TButton;
    CInsButton: TSpeedButton;
    ColorEdit: TComboBox;
    DelButton: TSpeedButton;
    HeightEdit: TEdit;
    IAEdit: TComboBox;
    IIEdit: TEdit;
    InsButton: TSpeedButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    MultiButton: TSpeedButton;
    OKButton: TButton;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel5: TPanel;
    PropBox: TGroupBox;
    RotateButton: TSpeedButton;
    ShapeEdit: TComboBox;
    Tree: TdxOrgChart;
    TreeBox: TGroupBox;
    TTEdit: TEdit;
    WidthEdit: TEdit;
    ZoomButton: TSpeedButton;

    procedure AlignEditClick(Sender: TObject);
    procedure AlignEditExit(Sender: TObject);
    procedure CInsButtonClick(Sender: TObject);
    procedure ColorEditClick(Sender: TObject);
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
    procedure SetColors(const S: String);
    procedure SetNodeInfo(Node: TdxOcNode);
    procedure ShapeEditClick(Sender: TObject);
    procedure ShapeEditExit(Sender: TObject);
    procedure TreeChange(Sender: TObject; Node: TdxOcNode);
    procedure TTEditExit(Sender: TObject);
    procedure WidthEditExit(Sender: TObject);
    procedure WidthEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ZoomButtonClick(Sender: TObject);
    procedure btnAntialiasingClick(Sender: TObject);
  protected
    function GetPreviewOrgChart: TdxCustomOrgChart; override;
    procedure PreparePreview(APreview, ASource: TdxCustomOrgChart); override;
    procedure ScaleFactorChanged(M: Integer; D: Integer); override;
  end;

implementation

uses
  cxGeometry, cxClasses, dxorgchrstrs;

{$R *.DFM}

{ TfrmEChartEditor }

procedure TfrmEChartEditor.SetColors(const S: String);
begin
  ColorEdit.Items.Add(S);
end;

procedure TfrmEChartEditor.FormCreate(Sender: TObject);
begin
  GetColorValues(SetColors);

  Caption := cxGetResourceString(@sdxOrgChartEditorCaption);
  TreeBox.Caption := cxGetResourceString(@sdxOrgChartEditorItems);
  PropBox.Caption := cxGetResourceString(@sdxOrgChartEditorProperties);
  CancelButton.Caption := cxGetResourceString(@sdxOrgChartEditorCancelButton);

  btnAntialiasing.Hint := cxGetResourceString(@sdxOrgChartEditorHintAntialiasing);
  CInsButton.Hint := cxGetResourceString(@sdxOrgChartEditorHintInsertSubItem);
  DelButton.Hint := cxGetResourceString(@sdxOrgChartEditorHintDeleteItem);
  InsButton.Hint := cxGetResourceString(@sdxOrgChartEditorHintInsertItem);
  MultiButton.Hint := cxGetResourceString(@sdxOrgChartEditorHintApplyForAllChildren);
  RotateButton.Hint := cxGetResourceString(@sdxOrgChartEditorHintRotate);
  ZoomButton.Hint := cxGetResourceString(@sdxOrgChartEditorHintZoom);

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

procedure TfrmEChartEditor.Label1Click(Sender: TObject);
begin
  TLabel(Sender).FocusControl.SetFocus;
end;

procedure TfrmEChartEditor.InsButtonClick(Sender: TObject);
begin
  InsertNode;
end;

procedure TfrmEChartEditor.CInsButtonClick(Sender: TObject);
begin
  InsertSubNode;
end;

procedure TfrmEChartEditor.DelButtonClick(Sender: TObject);
begin
  if Tree.Selected <> nil then
    Tree.Delete(Tree.Selected);
end;

procedure TfrmEChartEditor.ZoomButtonClick(Sender: TObject);
begin
  Tree.Zoom := ZoomButton.Down;
end;

procedure TfrmEChartEditor.TreeChange(Sender: TObject; Node: TdxOcNode);
begin
  if Node = nil then
  begin
    PropBox.Enabled := False;
    PropBox.Font.Style := [];
  end
  else
  begin
    PropBox.Enabled := True;
    TTEdit.Text := Node.Text;
    Node.GetNodeInfo(NodeInfo);
    with NodeInfo do
    begin
      WidthEdit.Text := IntToStr(Width);
      HeightEdit.Text := IntToStr(Height);
      ColorEdit.Text := ColorToString(Color);
      AlignEdit.ItemIndex := Ord(Align);
      ShapeEdit.ItemIndex := Ord(Shape);
      IIEdit.Text := IntToStr(Index);
      IAEdit.ItemIndex := Ord(IAlign);
    end;
  end;
end;

procedure TfrmEChartEditor.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Tree.OnChange := nil;
end;

procedure TfrmEChartEditor.SetNodeInfo(Node: TdxOcNode);
begin
  if Node = nil then
    Exit;
  with NodeInfo do
  begin
    Node.Width := Width;
    Node.Height := Height;
    Node.Color := Color;
    Node.ChildAlign := Align;
    Node.Shape := Shape;
    Node.ImageIndex := Index;
    Node.ImageAlign := IAlign;
  end;
end;

procedure TfrmEChartEditor.WidthEditExit(Sender: TObject);
begin
  NodeInfo.Width := StrToInt(WidthEdit.Text);
  SetNodeInfo(Tree.Selected);
end;

procedure TfrmEChartEditor.HeightEditExit(Sender: TObject);
begin
  NodeInfo.Height := StrToInt(HeightEdit.Text);
  SetNodeInfo(Tree.Selected);
end;

procedure TfrmEChartEditor.ColorEditExit(Sender: TObject);
var
  AColor: LongInt;
  AColorString: string;
begin
  if not IdentToColor(ColorEdit.Text, AColor) then
  begin
    AColorString := ColorEdit.Text;
    if (Length(AColorString) > 0) and (AColorString[1] <> '$') then
      AColorString := '$' + AColorString;
    AColor := StrToInt(AColorString);
  end;
  NodeInfo.Color := TColor(AColor);
  SetNodeInfo(Tree.Selected);
end;

procedure TfrmEChartEditor.AlignEditExit(Sender: TObject);
begin
  NodeInfo.Align := TdxOcNodeAlign(AlignEdit.ItemIndex);
  SetNodeInfo(Tree.Selected);
end;

procedure TfrmEChartEditor.btnAntialiasingClick(Sender: TObject);
begin
  Tree.Antialiasing := btnAntialiasing.Down;
end;

procedure TfrmEChartEditor.ShapeEditExit(Sender: TObject);
begin
  NodeInfo.Shape := TdxOcShape(ShapeEdit.ItemIndex);
  SetNodeInfo(Tree.Selected);
end;

procedure TfrmEChartEditor.ScaleFactorChanged(M, D: Integer);
begin
  inherited;
  TreeChange(Tree, Tree.Selected);
end;

procedure TfrmEChartEditor.SetButtonClick(Sender: TObject);
begin
  Tree.SetFocus;
  SetNodeInfo(Tree.Selected);
end;

procedure TfrmEChartEditor.MultiButtonClick(Sender: TObject);

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

procedure TfrmEChartEditor.FixButtonClick(Sender: TObject);
begin
  TreeChange(Tree, Tree.Selected);
end;

procedure TfrmEChartEditor.WidthEditKeyDown(
  Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
  begin
    TEdit(Sender).SelectAll;
    TEdit(Sender).OnExit(Sender);
  end;
end;

procedure TfrmEChartEditor.ColorEditKeyDown(
  Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (not ColorEdit.DroppedDown) and (Key = VK_RETURN) then
  begin
    ColorEdit.SelectAll;
    ColorEditExit(Sender);
  end;
end;

procedure TfrmEChartEditor.IIEditExit(Sender: TObject);
begin
  NodeInfo.Index := StrToInt(IIEdit.Text);
  SetNodeInfo(Tree.Selected);
end;

procedure TfrmEChartEditor.IAEditExit(Sender: TObject);
begin
  NodeInfo.IAlign := TdxOcImageAlign(IAEdit.ItemIndex);
  SetNodeInfo(Tree.Selected);
end;

procedure TfrmEChartEditor.RotateButtonClick(Sender: TObject);
begin
  Tree.Rotated := RotateButton.Down;
end;

procedure TfrmEChartEditor.ColorEditClick(Sender: TObject);
begin
  with ColorEdit do
    if Text = Items[ItemIndex] then
      ColorEditExit(Self);
end;

procedure TfrmEChartEditor.AlignEditClick(Sender: TObject);
begin
  AlignEditExit(Sender);
end;

procedure TfrmEChartEditor.ShapeEditClick(Sender: TObject);
begin
  ShapeEditExit(Sender);
end;

procedure TfrmEChartEditor.TTEditExit(Sender: TObject);
begin
  if Tree.Selected <> nil then
    Tree.Selected.Text := TTEdit.Text;
end;

function TfrmEChartEditor.GetPreviewOrgChart: TdxCustomOrgChart;
begin
  Result := Tree;
end;

procedure TfrmEChartEditor.PreparePreview(APreview, ASource: TdxCustomOrgChart);
begin
  inherited PreparePreview(APreview, ASource);
  btnAntialiasing.Down := Tree.Antialiasing;
  RotateButton.Down := Tree.Rotated;
  ZoomButton.Down := Tree.Zoom;
  TreeChange(Tree, Tree.Selected);
end;

initialization
  dxOrgChartCustomizeFormManager.Register(TfrmEChartEditor);
finalization
  dxOrgChartCustomizeFormManager.Unregister(TfrmEChartEditor);
end.
