{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressQuantumGrid                                       }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSQUANTUMGRID AND ALL            }
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

unit cxGridWizardUnboundViewsEditItemDialog;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, Menus, StdCtrls,
  dxCore, cxGraphics, cxControls,
  cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit, dxLayoutcxEditAdapters, dxLayoutControlAdapters,
  cxButtons, cxMaskEdit, cxDropDownEdit, dxLayoutContainer, cxTextEdit, dxLayoutControl, cxClasses, cxGridWizardStrs,
  dxLayoutLookAndFeels;

type
  { TcxGridWizardUnboundViewsEditItemDialogForm }

  TcxGridWizardUnboundViewsEditItemDialogForm = class(TForm)
    btnCancel: TcxButton;
    btnOK: TcxButton;
    cbItemProperties: TcxComboBox;
    dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel;
    dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList;
    edItemCaption: TcxTextEdit;
    lcgButtons: TdxLayoutGroup;
    lciCancel: TdxLayoutItem;
    lciItemCaption: TdxLayoutItem;
    lciItemProperties: TdxLayoutItem;
    lciOK: TdxLayoutItem;
    lcMain: TdxLayoutControl;
    lcMainGroup_Root: TdxLayoutGroup;
    procedure FormCreate(Sender: TObject);
  private
    function GetItemCaption: string;
    function GetItemProperties: TStringList;
    function GetItemPropertyIndex: Integer;
    procedure SetItemCaption(const AValue: string);
    procedure SetItemProperties(AValue: TStringList);
    procedure SetItemPropertyIndex(AValue: Integer);
  public
    property ItemCaption: string read GetItemCaption write SetItemCaption;
    property ItemProperties: TStringList read GetItemProperties write SetItemProperties;
    property ItemPropertyIndex: Integer read GetItemPropertyIndex write SetItemPropertyIndex;
  end;

function cxgwExecuteEditItemDialog(AOwner: TComponent; const ATitle: string;
  var AItemCaption: string): Boolean; overload;
function cxgwExecuteEditItemDialog(AOwner: TComponent; const ATitle: string; AItemProperties: TStringList;
  var AItemCaption: string; var AItemPropertyIndex: Integer; const AItemPropertiesCaption: string = ''): Boolean; overload;

implementation

{$R *.dfm}

function DoExecuteEditItemDialog(AOwner: TComponent; const ATitle: string; AAllowSelectItemProperties: Boolean;
  AItemProperties: TStringList; var AItemCaption: string; var AItemPropertyIndex: Integer;
  const AItemPropertiesCaption: string): Boolean;
var
  AForm: TcxGridWizardUnboundViewsEditItemDialogForm;
begin
  AForm := TcxGridWizardUnboundViewsEditItemDialogForm.Create(AOwner);
  try
    AForm.Caption := ATitle;
    AForm.ItemCaption := AItemCaption;
    AForm.ItemProperties := AItemProperties;
    if AItemPropertiesCaption <> '' then
      AForm.lciItemProperties.Caption := AItemPropertiesCaption;
    AForm.ItemPropertyIndex := AItemPropertyIndex;
    AForm.lciItemProperties.Visible := AAllowSelectItemProperties;
    Result := AForm.ShowModal = mrOk;
    if Result then
    begin
      AItemCaption := AForm.ItemCaption;
      AItemPropertyIndex := AForm.ItemPropertyIndex;
    end;
  finally
    AForm.Free;
  end;
end;

function cxgwExecuteEditItemDialog(AOwner: TComponent; const ATitle: string; var AItemCaption: string): Boolean;
var
  AItemPropertyIndex: Integer;
  AList: TStringList;
begin
  AItemPropertyIndex := 0;
  AList := TStringList.Create;
  try
    Result := DoExecuteEditItemDialog(AOwner, ATitle, False, AList, AItemCaption, AItemPropertyIndex, '');
  finally
    AList.Free;
  end;
end;

function cxgwExecuteEditItemDialog(AOwner: TComponent; const ATitle: string; AItemProperties: TStringList;
  var AItemCaption: string; var AItemPropertyIndex: Integer; const AItemPropertiesCaption: string = ''): Boolean;
begin
  Result := DoExecuteEditItemDialog(AOwner, ATitle, True, AItemProperties, AItemCaption, AItemPropertyIndex,
    AItemPropertiesCaption);
end;

{ TcxGridWizardUnboundViewsEditItemDialogForm }

function TcxGridWizardUnboundViewsEditItemDialogForm.GetItemCaption: string;
begin
  Result := edItemCaption.Text;
end;

function TcxGridWizardUnboundViewsEditItemDialogForm.GetItemProperties: TStringList;
begin
  Result := cbItemProperties.Properties.Items as TStringList;
end;

function TcxGridWizardUnboundViewsEditItemDialogForm.GetItemPropertyIndex: Integer;
begin
  Result := cbItemProperties.ItemIndex;
end;

procedure TcxGridWizardUnboundViewsEditItemDialogForm.SetItemCaption(const AValue: string);
begin
  edItemCaption.Text := AValue;
end;

procedure TcxGridWizardUnboundViewsEditItemDialogForm.SetItemProperties(AValue: TStringList);
begin
  cbItemProperties.Properties.BeginUpdate;
  try
    cbItemProperties.Clear;
    cbItemProperties.Properties.Items.AddStrings(AValue);
  finally
    cbItemProperties.Properties.EndUpdate;
  end;
end;

procedure TcxGridWizardUnboundViewsEditItemDialogForm.SetItemPropertyIndex(AValue: Integer);
begin
  cbItemProperties.ItemIndex := AValue;
end;

{ Events }

procedure TcxGridWizardUnboundViewsEditItemDialogForm.FormCreate(Sender: TObject);
begin
  lciCancel.Caption := cxGetResourceString(@scxgwCommonCancel);
  lciItemCaption.Caption := cxGetResourceString(@scxgwCommonCaptionPrompt);
  lciItemProperties.Caption := cxGetResourceString(@scxgwCommonProperties);
  lciOK.Caption := cxGetResourceString(@scxgwCommonOK);
end;

end.
