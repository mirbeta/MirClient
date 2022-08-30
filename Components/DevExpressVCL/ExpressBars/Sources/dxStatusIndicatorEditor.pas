{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressStatusBar                                         }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSSTATUSBAR AND ALL              }
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

unit dxStatusIndicatorEditor;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Controls, Forms, StdCtrls, ExtCtrls, CheckLst, dxStatusBar,
  cxGraphics, cxLookAndFeels, cxLookAndFeelPainters, Menus, cxButtons, cxControls, cxContainer, cxEdit,
  cxCheckListBox, cxTextEdit, cxMaskEdit, cxDropDownEdit, dxLayoutContainer, dxLayoutcxEditAdapters,
  dxLayoutControlAdapters, cxClasses, dxLayoutControl, dxLayoutLookAndFeels;

type
  TdxStatusBarIndicatorEditor = class(TForm)
    BtnAdd: TcxButton;
    BtnCancel: TcxButton;
    BtnClear: TcxButton;
    BtnDelete: TcxButton;
    BtnOK: TcxButton;
    cbItemTypes: TcxComboBox;
    chlbIndicators: TcxCheckListBox;
    dxLayoutAutoCreatedGroup1: TdxLayoutAutoCreatedGroup;
    dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel;
    dxLayoutGroup1: TdxLayoutGroup;
    dxLayoutGroup2: TdxLayoutGroup;
    dxLayoutGroup3: TdxLayoutGroup;
    dxLayoutGroup4: TdxLayoutGroup;
    dxLayoutGroup5: TdxLayoutGroup;
    dxLayoutItem1: TdxLayoutItem;
    dxLayoutItem2: TdxLayoutItem;
    dxLayoutItem3: TdxLayoutItem;
    dxLayoutItem4: TdxLayoutItem;
    dxLayoutItem5: TdxLayoutItem;
    liExample: TdxLayoutItem;
    dxLayoutItem8: TdxLayoutItem;
    dxLayoutItem9: TdxLayoutItem;
    dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList;
    dxLayoutSeparatorItem1: TdxLayoutSeparatorItem;
    imgExample: TImage;
    lcMain: TdxLayoutControl;
    lcMainGroup_Root: TdxLayoutGroup;

    procedure BtnAddClick(Sender: TObject);
    procedure BtnClearClick(Sender: TObject);
    procedure BtnDeleteClick(Sender: TObject);
    procedure cbItemTypesChange(Sender: TObject);
    procedure chlbIndicatorsClick(Sender: TObject);
    procedure chlbIndicatorsKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
  private
    procedure IndicatorChangeHandler(Sender: TObject);
    function StrToIndicatorType(const AStr: string): TdxStatusBarStateIndicatorType;
    procedure SetControlsState;
    procedure SetItemType;
  public
    Indicators: TdxStatusBarStateIndicators;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure PrepareIndicators;
  end;

implementation


{$R *.dfm}

const
  StateIndicatorTypeNames: array[TdxStatusBarStateIndicatorType] of string = (
    'sitOff', 'sitYellow', 'sitBlue', 'sitGreen', 'sitRed', 'sitTeal', 'sitPurple'
  );

{ TdxStatusBarIndicatorEditor }

constructor TdxStatusBarIndicatorEditor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Indicators := TdxStatusBarStateIndicators.Create;
  Indicators.OnChange := IndicatorChangeHandler;
end;

destructor TdxStatusBarIndicatorEditor.Destroy;
begin
  FreeAndNil(Indicators);
  inherited Destroy;
end;

procedure TdxStatusBarIndicatorEditor.IndicatorChangeHandler(Sender: TObject);
begin
  {}
end;

function TdxStatusBarIndicatorEditor.StrToIndicatorType(const AStr: string): TdxStatusBarStateIndicatorType;
begin
  for Result := Low(TdxStatusBarStateIndicatorType) to High(TdxStatusBarStateIndicatorType) do
  begin
    if SameText(AStr, StateIndicatorTypeNames[Result]) then
      Exit;
  end;
  Result := sitOff;
end;

procedure TdxStatusBarIndicatorEditor.FormShow(Sender: TObject);
var
  AItem: TcxCheckListBoxItem;
  I: Integer;
begin
  chlbIndicators.Clear;
  for I := 0 to Indicators.Count - 1 do
  begin
    AItem := chlbIndicators.Items.Add;
    AItem.Text := StateIndicatorTypeNames[Indicators[I].IndicatorType];
    AItem.Checked := Indicators[I].Visible;
  end;
  SetControlsState;
end;

procedure TdxStatusBarIndicatorEditor.BtnAddClick(Sender: TObject);
var
  AItem: TcxCheckListBoxItem;
begin
  AItem := chlbIndicators.Items.Add;
  AItem.Text := StateIndicatorTypeNames[sitOff];
  AItem.Checked := True;
  SetControlsState;
end;

procedure TdxStatusBarIndicatorEditor.SetControlsState;
begin
  BtnClear.Enabled := (chlbIndicators.Items.Count > 0);
  BtnDelete.Enabled := (chlbIndicators.Items.Count > 0) and (chlbIndicators.ItemIndex <> -1);
  cbItemTypes.Enabled := (chlbIndicators.Items.Count > 0) and (chlbIndicators.ItemIndex <> -1);
  liExample.Visible := (chlbIndicators.Items.Count > 0) and (chlbIndicators.ItemIndex <> -1);
  SetItemType;
end;

procedure TdxStatusBarIndicatorEditor.PrepareIndicators;
var
  I: Integer;
  FItem: TdxStatusBarStateIndicatorItem;
begin
  Indicators.Clear;
  for I := 0 to chlbIndicators.Items.Count - 1 do
  begin
    FItem := Indicators.Add;
    FItem.Visible := chlbIndicators.Items[I].Checked;
    FItem.IndicatorType := StrToIndicatorType(chlbIndicators.Items[I].Text);
  end;
end;

procedure TdxStatusBarIndicatorEditor.BtnDeleteClick(Sender: TObject);
begin
  if (chlbIndicators.Items.Count > 0) and (chlbIndicators.ItemIndex <> -1) then
  begin
    chlbIndicators.Items.Delete(chlbIndicators.ItemIndex);
    SetControlsState;
  end;
end;

procedure TdxStatusBarIndicatorEditor.BtnClearClick(Sender: TObject);
begin
  chlbIndicators.Items.Clear;
  SetControlsState;
end;

procedure TdxStatusBarIndicatorEditor.SetItemType;
begin
  if (chlbIndicators.Items.Count > 0) and (chlbIndicators.ItemIndex <> -1) then
  begin
    cbItemTypes.ItemIndex := cbItemTypes.Properties.Items.IndexOf(chlbIndicators.Items[chlbIndicators.ItemIndex].Text);
    LoadIndicatorBitmap(imgExample.Picture.Bitmap, StrToIndicatorType(cbItemTypes.Text));
  end
  else
    cbItemTypes.Text := '';
end;

procedure TdxStatusBarIndicatorEditor.chlbIndicatorsClick(Sender: TObject);
begin
  SetControlsState;
end;

procedure TdxStatusBarIndicatorEditor.chlbIndicatorsKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  SetControlsState;
end;

procedure TdxStatusBarIndicatorEditor.cbItemTypesChange(Sender: TObject);
begin
  if (chlbIndicators.Items.Count > 0) and (chlbIndicators.ItemIndex <> -1) then
    chlbIndicators.Items[chlbIndicators.ItemIndex].Text := cbItemTypes.Properties.Items[cbItemTypes.ItemIndex]
  else
    cbItemTypes.ItemIndex := -1;

  SetItemType;
end;

end.
