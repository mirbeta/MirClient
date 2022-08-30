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

unit dxSpreadSheetPasswordDialog;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHIXE2}
  System.UITypes,
{$ENDIF}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, Menus, StdCtrls,
  dxSpreadSheetCore, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxClasses, dxLayoutContainer,
  dxLayoutControl, dxLayoutLookAndFeels, dxLayoutControlAdapters, dxLayoutcxEditAdapters, cxContainer, cxEdit,
  cxTextEdit, cxButtons, dxForms;

type
  TdxSpreadSheetPasswordDialogMode = (pdmNew, pdmQuery, pdmConfirmation);

  { TdxSpreadSheetPasswordDialogForm }

  TdxSpreadSheetPasswordDialogFormClass = class of TdxSpreadSheetPasswordDialogForm;
  TdxSpreadSheetPasswordDialogForm = class(TdxForm)
    btnCancel: TcxButton;
    btnOk: TcxButton;
    dxLayoutAutoCreatedGroup1: TdxLayoutAutoCreatedGroup;
    dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel;
    dxLayoutItem1: TdxLayoutItem;
    dxLayoutItem2: TdxLayoutItem;
    dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList;
    edPassword: TcxTextEdit;
    lcMain: TdxLayoutControl;
    lcMainGroup_Root: TdxLayoutGroup;
    liNotes: TdxLayoutLabeledItem;
    liPassword: TdxLayoutItem;

    procedure btnOkClick(Sender: TObject);
  strict private
    FMode: TdxSpreadSheetPasswordDialogMode;
    FSpreadSheet: TdxCustomSpreadSheet;
  protected
    procedure ApplyLocalizations; virtual;
    procedure Initialize(ASpreadSheet: TdxCustomSpreadSheet; AMode: TdxSpreadSheetPasswordDialogMode); virtual;
    //
    property Mode: TdxSpreadSheetPasswordDialogMode read FMode;
    property SpreadSheet: TdxCustomSpreadSheet read FSpreadSheet;
  public
    // for internal use
    class function Execute(AOwner: TComponent; ASpreadSheet: TdxCustomSpreadSheet;
      AMode: TdxSpreadSheetPasswordDialogMode; out APassword: string): Boolean;
    class function ExecuteConfirmation(AOwner: TComponent; ASpreadSheet: TdxCustomSpreadSheet; const APassword: string): Boolean;
  end;

var
  dxSpreadSheetPasswordDialogClass: TdxSpreadSheetPasswordDialogFormClass = TdxSpreadSheetPasswordDialogForm;

function ShowPasswordDialog(ASpreadSheet: TdxCustomSpreadSheet;
  AMode: TdxSpreadSheetPasswordDialogMode; out APassword: string): Boolean;
implementation

uses
  dxCore, dxSpreadSheetDialogStrs;

{$R *.dfm}

function ShowPasswordDialog(ASpreadSheet: TdxCustomSpreadSheet;
  AMode: TdxSpreadSheetPasswordDialogMode; out APassword: string): Boolean;
begin
  Result := TdxSpreadSheetPasswordDialogForm.Execute(GetParentForm(ASpreadSheet), ASpreadSheet, AMode, APassword);
end;

{ TdxSpreadSheetPasswordDialogForm }

class function TdxSpreadSheetPasswordDialogForm.Execute(AOwner: TComponent;
  ASpreadSheet: TdxCustomSpreadSheet; AMode: TdxSpreadSheetPasswordDialogMode; out APassword: string): Boolean;
var
  ADialog: TdxSpreadSheetPasswordDialogForm;
begin
  ADialog := dxSpreadSheetPasswordDialogClass.Create(AOwner);
  try
    ADialog.Initialize(ASpreadSheet, AMode);
    Result := ADialog.ShowModal = mrOk;
    if Result then
      APassword := ADialog.edPassword.Text;
  finally
    ADialog.Free;
  end;
end;

class function TdxSpreadSheetPasswordDialogForm.ExecuteConfirmation(
  AOwner: TComponent; ASpreadSheet: TdxCustomSpreadSheet; const APassword: string): Boolean;
var
  AConfirmedPassword: string;
begin
  Result := False;
  if Execute(AOwner, ASpreadSheet, pdmConfirmation, AConfirmedPassword) then
  begin
    if APassword <> AConfirmedPassword then
      MessageDlg(cxGetResourceString(@sdxPasswordDialogPasswordNotMatch), mtWarning, [mbOK], 0)
    else
      Result := True;
  end;
end;

procedure TdxSpreadSheetPasswordDialogForm.ApplyLocalizations;
begin
  if Mode = pdmConfirmation then
    Caption := cxGetResourceString(@sdxPasswordDialogCaptionConfirm)
  else
    Caption := cxGetResourceString(@sdxPasswordDialogCaption);

  if Mode = pdmConfirmation then
    liPassword.Caption := cxGetResourceString(@sdxPasswordDialogPasswordConfirmation)
  else
    liPassword.Caption := cxGetResourceString(@sdxPasswordDialogPassword);

  liNotes.Caption := cxGetResourceString(@sdxPasswordDialogPasswordNotes);
  btnCancel.Caption := cxGetResourceString(@sdxPasswordDialogButtonCancel);
  btnOk.Caption := cxGetResourceString(@sdxPasswordDialogButtonOK);
end;

procedure TdxSpreadSheetPasswordDialogForm.Initialize(
  ASpreadSheet: TdxCustomSpreadSheet; AMode: TdxSpreadSheetPasswordDialogMode);
begin
  FMode := AMode;
  FSpreadSheet := ASpreadSheet;
  SetControlLookAndFeel(Self, ASpreadSheet.DialogsLookAndFeel);
  liNotes.Visible := Mode = pdmConfirmation;
  ApplyLocalizations;
end;

procedure TdxSpreadSheetPasswordDialogForm.btnOkClick(Sender: TObject);
var
  APassword: string;
begin
  if FMode = pdmNew then
  begin
    if Execute(Self, SpreadSheet, pdmConfirmation, APassword) then
    begin
      if APassword <> edPassword.Text then
        MessageDlg(cxGetResourceString(@sdxPasswordDialogPasswordNotMatch), mtWarning, [mbOK], 0)
      else
        ModalResult := mrOk;
    end;
  end
  else
    ModalResult := mrOk;
end;

end.
