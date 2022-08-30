{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressRichEditControl                                   }
{                                                                    }
{           Copyright (c) 2000-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSRICHEDITCONTROL AND ALL        }
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

unit dxRichEdit.Dialogs.QueryPassword;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHIXE2}
  System.UITypes,
{$ENDIF}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Menus, StdCtrls,
  Controls, Forms, Dialogs, cxGraphics, cxControls, cxLookAndFeels, dxForms,
  cxLookAndFeelPainters, dxLayoutLookAndFeels, cxClasses, dxLayoutContainer, dxLayoutControl,
  cxContainer, cxEdit, dxLayoutcxEditAdapters, cxTextEdit, dxLayoutControlAdapters, cxButtons;

type
  TdxQueryPasswordKind = (Query, SetNewWithConfirmation, SetNewWithOptionalConfirmation);

  TdxQueryPasswordForm = class(TdxForm)
    btnCancel: TcxButton;
    btnOk: TcxButton;
    dxLayoutControl1: TdxLayoutControl;
    dxLayoutControl1Group_Root: TdxLayoutGroup;
    dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList;
    dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel;
    dxLayoutAutoCreatedGroup1: TdxLayoutAutoCreatedGroup;
    dxLayoutItem1: TdxLayoutItem;
    dxLayoutItem2: TdxLayoutItem;
    edtPassword: TcxTextEdit;
    liPassword: TdxLayoutItem;
    liRepeatPassword: TdxLayoutItem;
    edtRepeatPassword: TcxTextEdit;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  strict private
    FPassword: string;
    FKind: TdxQueryPasswordKind;
  protected
    procedure ApplyLocalization; virtual;
    procedure UpdateLookAndFeel;
    procedure SetKind(AKind: TdxQueryPasswordKind);
  public
    constructor Create(AOwner: TComponent; AKind: TdxQueryPasswordKind; const APassword: string); reintroduce;
    property Password: string read FPassword write FPassword;
  end;

function ShowQueryPasswordDialog(AKind: TdxQueryPasswordKind; var APassword: string; ACaption: string = '';
  AOwner: TComponent = nil): Boolean;

implementation

uses
  dxRichEdit.Dialogs.Strs, dxCore;

{$R *.dfm}

function ShowQueryPasswordDialog(AKind: TdxQueryPasswordKind; var APassword: string; ACaption: string = '';
  AOwner: TComponent = nil): Boolean;
var
  AForm: TdxQueryPasswordForm;
begin
  AForm := TdxQueryPasswordForm.Create(AOwner, AKind, APassword);
  try
    if ACaption <> '' then
      AForm.Caption := ACaption;
    Result := AForm.ShowModal = mrOk;
    APassword := AForm.Password;
  finally
    AForm.Free;
  end;
end;

{ TdxQueryPasswordForm }

constructor TdxQueryPasswordForm.Create(AOwner: TComponent; AKind: TdxQueryPasswordKind; const APassword: string);
begin
  inherited Create(AOwner);
  FPassword := APassword;
  SetKind(AKind);
  ApplyLocalization;
  UpdateLookAndFeel;
end;

procedure TdxQueryPasswordForm.ApplyLocalization;
begin
  Caption := cxGetResourceString(@sdxQueryPasswordForm);
  btnOk.Caption := cxGetResourceString(@sdxRichEditDialogButtonOK);
  btnCancel.Caption := cxGetResourceString(@sdxRichEditDialogButtonCancel);
  case FKind of
    Query:
      liPassword.CaptionOptions.Text := cxGetResourceString(@sdxQueryPasswordPassword);
    SetNewWithConfirmation:
      begin
        liPassword.CaptionOptions.Text := cxGetResourceString(@sdxQueryNewPasswordPassword);
        liRepeatPassword.CaptionOptions.Text := cxGetResourceString(@sdxQueryNewPasswordRepeatPassword);
      end;
    SetNewWithOptionalConfirmation:
      begin
        liPassword.CaptionOptions.Text := cxGetResourceString(@sdxQueryNewPasswordPassword);
        liRepeatPassword.CaptionOptions.Text := cxGetResourceString(@sdxQueryNewPasswordRepeatPassword);
      end;
  end;
end;

procedure TdxQueryPasswordForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  AText: string;
begin
  if ModalResult <> mrOk then
    Exit;
  if FKind in [TdxQueryPasswordKind.SetNewWithConfirmation, TdxQueryPasswordKind.SetNewWithOptionalConfirmation ]then
  begin
    if not liRepeatPassword.Visible then
    begin
      if (edtPassword.Text <> '') and (FPassword <> edtPassword.Text) then
      begin
        dxLayoutControl1.BeginUpdate;
        liRepeatPassword.Visible := True;
        liPassword.Visible := False;
        dxLayoutControl1.EndUpdate;
        edtRepeatPassword.SetFocus;
        CanClose := False;
        Exit;
      end;
    end
    else
    begin
      CanClose := edtPassword.Text = edtRepeatPassword.Text;
      if not CanClose then
      begin
        AText := cxGetResourceString(@sdxQueryNewPasswordInvalidPasswordConfirmation);
        Application.MessageBox(PChar(AText), PChar(Application.Title), MB_OK + MB_ICONWARNING);
        Exit;
      end
    end;
  end;
  FPassword := edtPassword.Text;
end;

procedure TdxQueryPasswordForm.SetKind(AKind: TdxQueryPasswordKind);
begin
  FKind := AKind;
  case AKind of
    Query:
      liRepeatPassword.Visible := False;
    SetNewWithConfirmation:;
    SetNewWithOptionalConfirmation:
      begin
        liRepeatPassword.Visible := False;
        edtPassword.Text := FPassword;
      end;
  end;
end;

procedure TdxQueryPasswordForm.UpdateLookAndFeel;
var
  AOwnerLookAndFeel: IcxLookAndFeelContainer;
begin
  if Supports(Owner, IcxLookAndFeelContainer, AOwnerLookAndFeel) then
    SetControlLookAndFeel(Self, AOwnerLookAndFeel.GetLookAndFeel);
end;

end.
