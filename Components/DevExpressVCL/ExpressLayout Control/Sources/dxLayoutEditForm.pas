{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressLayoutControl edit form                           }
{                                                                    }
{           Copyright (c) 2001-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSLAYOUTCONTROL AND ALL          }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM       }
{   ONLY.                                                            }
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

unit dxLayoutEditForm;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,
  dxCore, dxLayoutControl, cxControls, dxLayoutLookAndFeels, cxGraphics,
  cxLookAndFeels, cxLookAndFeelPainters, dxLayoutControlAdapters,
  dxLayoutContainer;

type
  TLayoutEditForm = class(TForm)
    LayoutControl: TdxLayoutControl;
    LayoutControlItemEdit: TdxLayoutItem;
    dxLayoutControl1Group1: TdxLayoutGroup;
    btnOK: TButton;
    dxLayoutControl1Item2: TdxLayoutItem;
    btnCancel: TButton;
    dxLayoutControl1Item3: TdxLayoutItem;
    cbDescriptions: TComboBox;
  public
    constructor Create(AOwner: TComponent); override;
  end;

function dxLayoutControlSelectLookAndFeel(var ALookAndFeelClass: TdxCustomLayoutLookAndFeelClass): Boolean;

implementation

{$R *.DFM}

uses
  cxClasses, dxLayoutStrs;

type
  TControlAccess = class(TControl);

function dxLayoutControlSelectLookAndFeel(var ALookAndFeelClass: TdxCustomLayoutLookAndFeelClass): Boolean;

  procedure SetDescriptions(AComboBox: TComboBox);
  var
    I: Integer;
  begin
    for I := 0 to dxLayoutLookAndFeelDefs.Count - 1 do
      AComboBox.Items.Add(dxLayoutLookAndFeelDefs[I].Description);
    if AComboBox.Items.Count <> 0 then
      AComboBox.ItemIndex := 0;
  end;

var
  ADescription: string;
  AForm: TLayoutEditForm;
  AEditControl: TComboBox;
begin
  AForm := TLayoutEditForm.Create(nil);
  try
    AEditControl := AForm.cbDescriptions;
    AForm.Caption := 'New Look & Feel';
    AForm.LayoutControlItemEdit.Caption := 'Choose a new look && feel style:';
    SetDescriptions(AEditControl);

    Result := AForm.ShowModal = mrOk;
    ADescription := AEditControl.Text;
  finally
    AForm.Free;
  end;
  if Result then
    ALookAndFeelClass := dxLayoutLookAndFeelDefs.GetItemByDescription(ADescription);
end;

constructor TLayoutEditForm.Create(AOwner: TComponent);
begin
  inherited;
  btnOK.Caption := cxGetResourceString(@sdxLayoutControlEditFormOK);
  btnCancel.Caption := cxGetResourceString(@sdxLayoutControlEditFormCancel);
end;

end.
