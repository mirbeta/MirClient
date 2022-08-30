{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressNavBar                                            }
{                                                                    }
{           Copyright (c) 2002-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSNAVBAR AND ALL ACCOMPANYING    }
{   VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY.              }
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

unit dxNavBarOfficeNavigationBarCustomizationDlg;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, Menus, Math, StdCtrls,
  Generics.Defaults, Generics.Collections, ActnList, ExtCtrls,
  cxGraphics, cxLookAndFeels, cxClasses, cxLookAndFeelPainters, cxButtons, cxControls, cxContainer, cxEdit, cxLabel,
  cxListBox, dxCore, cxTextEdit, cxMaskEdit, cxSpinEdit, dxNavBarOfficeNavigationBar, cxCheckBox, dxLayoutContainer,
  dxLayoutControl, dxLayoutcxEditAdapters, dxLayoutControlAdapters, dxLayoutLookAndFeels, dxForms;

type
  TfrmOfficeNavigationBarCustomizationDlg = class(TdxForm)
    ActionList1: TActionList;
    actMoveDown: TAction;
    actMoveUp: TAction;
    btnCancel: TcxButton;
    btnMoveDown: TcxButton;
    btnMoveUp: TcxButton;
    btnOk: TcxButton;
    btnReset: TcxButton;
    chbCompactNavigation: TcxCheckBox;
    dxLayoutControl1: TdxLayoutControl;
    dxLayoutControl1Group_Root: TdxLayoutGroup;
    dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel;
    dxLayoutGroup2: TdxLayoutGroup;
    dxLayoutGroup3: TdxLayoutGroup;
    dxLayoutGroup4: TdxLayoutGroup;
    dxLayoutGroup5: TdxLayoutGroup;
    dxLayoutItem1: TdxLayoutItem;
    dxLayoutItem3: TdxLayoutItem;
    dxLayoutItem4: TdxLayoutItem;
    dxLayoutItem5: TdxLayoutItem;
    dxLayoutItem6: TdxLayoutItem;
    dxLayoutItem7: TdxLayoutItem;
    dxLayoutItem8: TdxLayoutItem;
    dxLayoutLabeledItem1: TdxLayoutLabeledItem;
    dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList;
    dxLayoutSeparatorItem1: TdxLayoutSeparatorItem;
    dxLayoutSeparatorItem2: TdxLayoutSeparatorItem;
    edtMaxVisibleItems: TcxSpinEdit;
    lbNavigationItems: TcxListBox;
    liMaxVisibleItems: TdxLayoutItem;

    procedure ActionList1Update(Action: TBasicAction; var Handled: Boolean);
    procedure btnMoveDownClick(Sender: TObject);
    procedure btnMoveUpClick(Sender: TObject);
    procedure btnResetClick(Sender: TObject);
  private
    FIsDefaultOrder: Boolean;
    FNavigationBar: TdxNavBarCustomOfficeNavigationBar;
    FOrderChanged: Boolean;
    FOrderList: TList<Integer>;

    procedure InitControls;
    procedure Localize;
    procedure Move(ADirection: TcxDirection);
    function GetMaxVisibleItemCount: Integer;
    procedure SetOrderList(const Value: TList<Integer>);
    function GetCompactNavigation: Boolean;
  public
    constructor Create(ANavigationBar: TdxNavBarCustomOfficeNavigationBar); reintroduce; virtual;
    destructor Destroy; override;
    function ShowModal: Integer; override;

    property CompactNavigation: Boolean read GetCompactNavigation;
    property MaxVisibleItemCount: Integer read GetMaxVisibleItemCount;
    property OrderList: TList<Integer> read FOrderList write SetOrderList;
  end;

implementation

{$R *.dfm}

uses
  dxNavBarConsts;

{ TfrmOfficeNavigationBarCustomizationDlg }

constructor TfrmOfficeNavigationBarCustomizationDlg.Create(ANavigationBar: TdxNavBarCustomOfficeNavigationBar);
begin
  inherited Create(nil);
  FNavigationBar := ANavigationBar;
  FOrderList := TList<Integer>.Create;
  AutoSize := True;
end;

destructor TfrmOfficeNavigationBarCustomizationDlg.Destroy;
begin
  FreeAndNil(FOrderList);
  inherited;
end;

function TfrmOfficeNavigationBarCustomizationDlg.GetCompactNavigation: Boolean;
begin
  Result := chbCompactNavigation.Checked;
end;

function TfrmOfficeNavigationBarCustomizationDlg.GetMaxVisibleItemCount: Integer;
begin
  if edtMaxVisibleItems.Value = edtMaxVisibleItems.Properties.MaxValue then
    Result := 0
  else
    Result := edtMaxVisibleItems.Value;
end;

procedure TfrmOfficeNavigationBarCustomizationDlg.SetOrderList(const Value: TList<Integer>);
begin
  FOrderList.AddRange(Value);
end;

function TfrmOfficeNavigationBarCustomizationDlg.ShowModal: Integer;
var
  I: Integer;
begin
  cxDialogsMetricsStore.InitDialog(Self);
  InitControls;
  Result := inherited ShowModal;
  if Result = mrOk then
    if FIsDefaultOrder then
      OrderList.Clear
    else
      if FOrderChanged then
      begin
        OrderList.Clear;
        for I := 0 to lbNavigationItems.Count - 1 do
          OrderList.Add(Integer((lbNavigationItems.Items.Objects[I])));
      end;
  cxDialogsMetricsStore.StoreMetrics(Self);
end;

procedure TfrmOfficeNavigationBarCustomizationDlg.btnMoveUpClick(Sender: TObject);
begin
  Move(dirUp);
end;

procedure TfrmOfficeNavigationBarCustomizationDlg.ActionList1Update(Action: TBasicAction; var Handled: Boolean);
begin
  actMoveUp.Enabled := lbNavigationItems.ItemIndex > 0;
  actMoveDown.Enabled := InRange(lbNavigationItems.ItemIndex, 0, lbNavigationItems.Count - 2);
end;

procedure TfrmOfficeNavigationBarCustomizationDlg.btnMoveDownClick(Sender: TObject);
begin
  Move(dirDown);
end;

procedure TfrmOfficeNavigationBarCustomizationDlg.btnResetClick(Sender: TObject);
var
  AItem: IdxNavigationItem;
begin
  FOrderChanged := False;
  FIsDefaultOrder := True;
  lbNavigationItems.Clear;
  for AItem in FNavigationBar.GetActiveItemProvider.GetItems do
    lbNavigationItems.AddItem(AItem.GetText, TObject(AItem.GetID));
  if lbNavigationItems.Count > 0 then
    lbNavigationItems.ItemIndex := 0;
  edtMaxVisibleItems.EditValue := lbNavigationItems.Count;
end;

procedure TfrmOfficeNavigationBarCustomizationDlg.InitControls;

  procedure DisableMaxVisibleItemCount;
  begin
    edtMaxVisibleItems.EditValue := FNavigationBar.SortedItemCount;
    edtMaxVisibleItems.Enabled := False;
  end;

var
  I: Integer;
  AItemCount: Integer;
  AItem: IdxNavigationItem;
begin
  Localize;
  AItemCount := FNavigationBar.SortedItemCount;
  if AItemCount > 0 then
  begin
    for I := 0 to AItemCount - 1 do
    begin
      AItem := FNavigationBar.SortedItems[I];
      lbNavigationItems.AddItem(AItem.Text, TObject(AItem.ID));
    end;
    if AItemCount > 1 then
    begin
      edtMaxVisibleItems.Properties.MinValue := 1;
      edtMaxVisibleItems.Properties.MaxValue := AItemCount;
      if FNavigationBar.OptionsView.MaxVisibleItemCount > 0 then
        edtMaxVisibleItems.EditValue := Min(FNavigationBar.OptionsView.MaxVisibleItemCount, AItemCount)
      else
        edtMaxVisibleItems.EditValue := AItemCount;
    end
    else
      DisableMaxVisibleItemCount;
    lbNavigationItems.ItemIndex := 0;
  end
  else
    DisableMaxVisibleItemCount;
  chbCompactNavigation.Checked := FNavigationBar.OptionsView.CompactNavigation;
end;

procedure TfrmOfficeNavigationBarCustomizationDlg.Localize;
begin
  dxLayoutControl1.BeginUpdate;
  try
    Caption := cxGetResourceString(@sdxOfficeNavigationBarCustomizationDlgCaption);
    liMaxVisibleItems.CaptionOptions.Text := cxGetResourceString(@sdxOfficeNavigationBarMaxVisibleItems);
    chbCompactNavigation.Caption := cxGetResourceString(@sdxOfficeNavigationBarCompactNavigation);
    dxLayoutLabeledItem1.CaptionOptions.Text := cxGetResourceString(@sdxOfficeNavigationBarDisplayInThisOrder);
    btnMoveUp.Caption := cxGetResourceString(@sdxNavBarMoveUp);
    btnMoveDown.Caption := cxGetResourceString(@sdxNavBarMoveDown);
    btnReset.Caption := cxGetResourceString(@sdxOfficeNavigationBarReset);
    btnOk.Caption := cxGetResourceString(@sdxOfficeNavigationBarOk);
    btnCancel.Caption := cxGetResourceString(@sdxOfficeNavigationBarCancel);
  finally
    dxLayoutControl1.EndUpdate;
  end;
end;

procedure TfrmOfficeNavigationBarCustomizationDlg.Move(ADirection: TcxDirection);
var
  AIndex, ANewIndex: Integer;
begin
  if lbNavigationItems.ItemIndex <> -1 then
  begin
    AIndex := lbNavigationItems.ItemIndex;
    if ADirection = dirUp then
      ANewIndex := Max(0, AIndex - 1)
    else
      ANewIndex := Min(lbNavigationItems.Count - 1, AIndex + 1);
    if AIndex <> ANewIndex then
    begin
      lbNavigationItems.Items.Move(AIndex, ANewIndex);
      lbNavigationItems.ItemIndex := ANewIndex;
      FOrderChanged := True;
      FIsDefaultOrder := False;
    end;
  end;
end;

end.
