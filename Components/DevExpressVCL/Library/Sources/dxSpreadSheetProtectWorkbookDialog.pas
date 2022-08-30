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

unit dxSpreadSheetProtectWorkbookDialog;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHIXE2}
  System.UITypes,
{$ENDIF}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, Menus, StdCtrls, TypInfo,
  Generics.Defaults, Generics.Collections,
  dxCore, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxClasses, cxContainer, cxEdit,
  cxCheckListBox, cxCheckBox, cxButtons, cxTextEdit, dxLayoutControlAdapters, dxLayoutcxEditAdapters,
  dxLayoutContainer, dxLayoutControl, dxLayoutLookAndFeels, dxSpreadSheetCore, dxSpreadSheetProtection, cxLabel, dxForms;

type

  { TdxSpreadSheetProtectWorkbookDialogForm }

  TdxSpreadSheetProtectWorkbookDialogFormClass = class of TdxSpreadSheetProtectWorkbookDialogForm;
  TdxSpreadSheetProtectWorkbookDialogForm = class(TdxForm)
    btnCancel: TcxButton;
    btnOK: TcxButton;
    cbProtectStructure: TcxCheckBox;
    dxLayoutAutoCreatedGroup1: TdxLayoutAutoCreatedGroup;
    dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel;
    dxLayoutEmptySpaceItem1: TdxLayoutEmptySpaceItem;
    dxLayoutItem1: TdxLayoutItem;
    dxLayoutItem2: TdxLayoutItem;
    dxLayoutItem3: TdxLayoutItem;
    dxLayoutItem4: TdxLayoutItem;
    dxLayoutLookAndFeelList: TdxLayoutLookAndFeelList;
    edPassword: TcxTextEdit;
    lbHeader: TcxLabel;
    lcMain: TdxLayoutControl;
    lcMainGroup_Root: TdxLayoutGroup;
    liPassword: TdxLayoutItem;

    procedure btnOKClick(Sender: TObject);
    procedure cbProtectStructureClick(Sender: TObject);
  strict private
    FSpreadSheet: TdxCustomSpreadSheet;
  protected
    procedure ApplyLocalizations; virtual;
    procedure Initialize(ASpreadSheet: TdxCustomSpreadSheet); virtual;
    procedure Load(AOptions: TdxSpreadSheetWorkbookProtectionOptions); virtual;
    procedure Save(AOptions: TdxSpreadSheetWorkbookProtectionOptions); virtual;
  public
    property SpreadSheet: TdxCustomSpreadSheet read FSpreadSheet;
  end;

var
  dxSpreadSheetProtectWorkbookDialogClass: TdxSpreadSheetProtectWorkbookDialogFormClass = TdxSpreadSheetProtectWorkbookDialogForm;

procedure ShowProtectWorkbookDialog(ASpreadSheet: TdxCustomSpreadSheet);
implementation

uses
  dxSpreadSheetDialogStrs, dxSpreadSheetPasswordDialog;

{$R *.dfm}

procedure ShowProtectWorkbookDialog(ASpreadSheet: TdxCustomSpreadSheet);
var
  ADialog: TdxSpreadSheetProtectWorkbookDialogForm;
begin
  ADialog := dxSpreadSheetProtectWorkbookDialogClass.Create(GetParentForm(ASpreadSheet));
  try
    ADialog.Initialize(ASpreadSheet);
    ADialog.Load(ASpreadSheet.OptionsProtection);
    if ADialog.ShowModal = mrOk then
    begin
      ASpreadSheet.BeginUpdate;
      try
        ADialog.Save(ASpreadSheet.OptionsProtection);
      finally
        ASpreadSheet.EndUpdate;
      end;
    end;
  finally
    ADialog.Free;
  end;
end;

{ TdxSpreadSheetProtectWorkbookDialogForm }

procedure TdxSpreadSheetProtectWorkbookDialogForm.ApplyLocalizations;
begin
  Caption := cxGetResourceString(@sdxProtectWorkbookDialogCaption);
  btnOK.Caption := cxGetResourceString(@sdxProtectWorkbookDialogButtonOK);
  btnCancel.Caption := cxGetResourceString(@sdxProtectWorkbookDialogButtonCancel);
  lbHeader.Caption := cxGetResourceString(@sdxProtectWorkbookDialogProtectionOptions);
  cbProtectStructure.Caption := cxGetResourceString(@sdxProtectWorkbookDialogProtectStructure);
  liPassword.Caption := cxGetResourceString(@sdxProtectWorkbookDialogPassword);
end;

procedure TdxSpreadSheetProtectWorkbookDialogForm.Initialize(ASpreadSheet: TdxCustomSpreadSheet);
begin
  FSpreadSheet := ASpreadSheet;
  SetControlLookAndFeel(Self, SpreadSheet.DialogsLookAndFeel);
  liPassword.Visible := dxSpreadSheetDefaultProtectionProvider <> nil;
  ApplyLocalizations;
end;

procedure TdxSpreadSheetProtectWorkbookDialogForm.Load(AOptions: TdxSpreadSheetWorkbookProtectionOptions);
begin
  cbProtectStructure.Checked := not (AOptions.Protected and AOptions.AllowChangeStructure);
  cbProtectStructureClick(nil);
end;

procedure TdxSpreadSheetProtectWorkbookDialogForm.Save(AOptions: TdxSpreadSheetWorkbookProtectionOptions);
begin
  AOptions.Protected := True;
  AOptions.AllowChangeStructure := not cbProtectStructure.Checked;

  if edPassword.Text <> '' then
    AOptions.ProtectionInfo := dxSpreadSheetDefaultProtectionProvider.Create(edPassword.Text)
  else
    AOptions.ProtectionInfo := nil;
end;

procedure TdxSpreadSheetProtectWorkbookDialogForm.btnOKClick(Sender: TObject);
begin
  if (edPassword.Text = '') or TdxSpreadSheetPasswordDialogForm.ExecuteConfirmation(Self, SpreadSheet, edPassword.Text) then
    ModalResult := mrOk;
end;

procedure TdxSpreadSheetProtectWorkbookDialogForm.cbProtectStructureClick(Sender: TObject);
begin
  btnOK.Enabled := cbProtectStructure.Checked;
end;

end.
