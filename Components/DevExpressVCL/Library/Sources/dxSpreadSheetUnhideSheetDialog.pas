{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressSpreadSheet                                       }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSSPREADSHEET CONTROL AND ALL    }
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

unit dxSpreadSheetUnhideSheetDialog;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, Menus,
  dxCore, cxGraphics,
  cxControls, cxLookAndFeels, cxLookAndFeelPainters, dxLayoutControlAdapters, dxLayoutLookAndFeels, dxLayoutContainer,
  cxEdit, cxClasses, cxButtons, dxLayoutControl, cxContainer, cxListBox, dxSpreadSheetCore, dxForms;

type

  { TdxSpreadSheetUnhideSheetDialogForm }

  TdxSpreadSheetUnhideSheetDialogForm = class(TdxForm)
    btnCancel: TcxButton;
    btnOk: TcxButton;
    dxLayoutCxLookAndFeel: TdxLayoutCxLookAndFeel;
    dxLayoutGroup1: TdxLayoutGroup;
    dxLayoutItem1: TdxLayoutItem;
    dxLayoutLookAndFeelList: TdxLayoutLookAndFeelList;
    lbHiddenSheets: TcxListBox;
    lciHiddenSheets: TdxLayoutItem;
    lcMain: TdxLayoutControl;
    lcMainGroup_Root: TdxLayoutGroup;
    lcMainItem2: TdxLayoutItem;
    procedure lbHiddenSheetsDblClick(Sender: TObject);
  strict private
    FSpreadSheet: TdxCustomSpreadSheet;

    procedure ApplyLocalization;
    procedure PopulateHiddenSheets;
  protected
    procedure Initialize(ASpreadSheet: TdxCustomSpreadSheet);
    procedure Save;
    //
    property SpreadSheet: TdxCustomSpreadSheet read FSpreadSheet;
  end;

procedure ShowUnhideSheetDialog(ASpreadSheet: TdxCustomSpreadSheet);
implementation

uses
  dxSpreadSheetDialogStrs;

{$R *.dfm}

procedure ShowUnhideSheetDialog(ASpreadSheet: TdxCustomSpreadSheet);
var
  ADialog: TdxSpreadSheetUnhideSheetDialogForm;
begin
  ADialog := TdxSpreadSheetUnhideSheetDialogForm.Create(GetParentForm(ASpreadSheet));
  try
    ADialog.Initialize(ASpreadSheet);
    if ADialog.ShowModal = mrOk then
      ADialog.Save;
  finally
    ADialog.Free;
  end;
end;

{ TdxSpreadSheetUnhideSheetDialogForm }

procedure TdxSpreadSheetUnhideSheetDialogForm.Initialize(ASpreadSheet: TdxCustomSpreadSheet);
begin
  SetControlLookAndFeel(Self, ASpreadSheet.DialogsLookAndFeel);
  FSpreadSheet := ASpreadSheet;
  PopulateHiddenSheets;
  ApplyLocalization;
end;

procedure TdxSpreadSheetUnhideSheetDialogForm.Save;
var
  ASheet: TdxSpreadSheetCustomView;
begin
  if lbHiddenSheets.ItemIndex >= 0 then
  begin
    ASheet := TdxSpreadSheetCustomView(lbHiddenSheets.ItemObject);
    ASheet.Visible := True;
    ASheet.Active := True;
  end;
end;

procedure TdxSpreadSheetUnhideSheetDialogForm.ApplyLocalization;
begin
  Caption := cxGetResourceString(@sdxUnhideSheetDialogCaption);
  lciHiddenSheets.Caption := cxGetResourceString(@sdxUnhideSheetDialogHiddenSheets);
  btnOK.Caption := cxGetResourceString(@sdxContainerCustomizationDialogButtonOK);
  btnCancel.Caption := cxGetResourceString(@sdxContainerCustomizationDialogButtonCancel);
end;

procedure TdxSpreadSheetUnhideSheetDialogForm.PopulateHiddenSheets;
var
  ASheet: TdxSpreadSheetCustomView;
  I: Integer;
begin
  lbHiddenSheets.Items.BeginUpdate;
  try
    lbHiddenSheets.Items.Clear;
    for I := 0 to SpreadSheet.SheetCount - 1 do
    begin
      ASheet := SpreadSheet.Sheets[I];
      if not ASheet.Visible then
        lbHiddenSheets.Items.AddObject(ASheet.Caption, ASheet);
    end;
    lbHiddenSheets.ItemIndex := 0;
  finally
    lbHiddenSheets.Items.EndUpdate;
  end;
end;

procedure TdxSpreadSheetUnhideSheetDialogForm.lbHiddenSheetsDblClick(Sender: TObject);
begin
  btnOk.Click;
end;

end.
