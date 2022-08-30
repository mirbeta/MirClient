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

unit dxRichEdit.Dialogs.RangeEditingPermissions;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
{$IFDEF DELPHIXE2}
  System.UITypes,
{$ENDIF}
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, cxGraphics, cxControls, cxLookAndFeels,
  cxLookAndFeelPainters, dxLayoutLookAndFeels, cxClasses, dxLayoutContainer, dxLayoutControl, dxLayoutControlAdapters,
  Menus, StdCtrls, cxButtons, cxContainer, cxEdit, cxCheckListBox,
  dxRichEdit.Dialogs.CustomDialog,
  dxRichEdit.Dialogs.Core,
  dxRichEdit.Dialogs.RangeEditingPermissionsFormController;

type
  TdxRangeEditingPermissionsForm = class(TdxRichEditCustomDialogForm)
    btnApply: TcxButton;
    btnCancel: TcxButton;
    btnOk: TcxButton;
    dxLayoutAutoCreatedGroup1: TdxLayoutAutoCreatedGroup;
    dxLayoutGroup1: TdxLayoutGroup;
    dxLayoutItem3: TdxLayoutItem;
    dxLayoutItem4: TdxLayoutItem;
    dxLayoutItem5: TdxLayoutItem;
    liGroups: TdxLayoutItem;
    ListUserGroups: TcxCheckListBox;
    ListUsers: TcxCheckListBox;
    liUsers: TdxLayoutItem;
    btnMoreUsers: TButton;
    dxLayoutItem1: TdxLayoutItem;
    procedure btnApplyClick(Sender: TObject);
    procedure btnMoreUsersClick(Sender: TObject);
  private
    function GetController: TdxRangeEditingPermissionsFormController; inline;
  protected
    procedure ApplyLocalization; override;
    function CreateController(AControllerParameters: TdxFormControllerParameters): TdxFormController; override;

    procedure ApplyCheckedUserGroups;
    procedure ApplyCheckedUsers;
    procedure PopulateUserGroups;
    procedure PopulateUsers;
    procedure UpdateFormCore; override;
  public
    property Controller: TdxRangeEditingPermissionsFormController read GetController;
  end;

implementation

uses
  dxCore,
  dxRichEdit.Dialogs.Strs,
  dxRichEdit.Dialogs;

{$R *.dfm}

{ TRangeEditingPermissionsForm }

function TdxRangeEditingPermissionsForm.GetController: TdxRangeEditingPermissionsFormController;
begin
  Result := TdxRangeEditingPermissionsFormController(inherited Controller);
end;

procedure TdxRangeEditingPermissionsForm.ApplyCheckedUserGroups;
var
  ACount, I: Integer;
  AItem: TcxCheckListBoxItem;
begin
  Controller.CheckedUserGroups.Clear;
  ACount := ListUserGroups.Items.Count;
  for I := 0 to ACount - 1 do
  begin
    AItem := ListUserGroups.Items[I];
    if AItem.Checked then
      Controller.CheckedUserGroups.Add(AItem.Text);
  end;
end;

procedure TdxRangeEditingPermissionsForm.ApplyCheckedUsers;
var
  ACount, I: Integer;
  AItem: TcxCheckListBoxItem;
begin
  Controller.CheckedUsers.Clear;

  ACount := ListUsers.Items.Count;
  for I := 0 to ACount - 1 do
  begin
    AItem := ListUsers.Items[I];
    if AItem.Checked then
      Controller.CheckedUsers.Add(AItem.Text);
  end;
end;

procedure TdxRangeEditingPermissionsForm.ApplyLocalization;
begin
  Caption := cxGetResourceString(@sdxRangeEditingPermissionsForm);
  btnOk.Caption := cxGetResourceString(@sdxRichEditDialogButtonOK);
  btnCancel.Caption := cxGetResourceString(@sdxRichEditDialogButtonCancel);
  btnApply.Caption := cxGetResourceString(@sdxRangeEditingPermissionsButtonApply);
  btnMoreUsers.Caption := cxGetResourceString(@sdxRangeEditingPermissionsMoreUsers);
  liGroups.CaptionOptions.Text := cxGetResourceString(@sdxRangeEditingPermissionsGroups);
  liUsers.CaptionOptions.Text := cxGetResourceString(@sdxRangeEditingPermissionsUsers);
end;

procedure TdxRangeEditingPermissionsForm.btnApplyClick(Sender: TObject);
begin
  ApplyCheckedUsers;
  ApplyCheckedUserGroups;
  Controller.ApplyChanges;
end;

procedure TdxRangeEditingPermissionsForm.btnMoreUsersClick(Sender: TObject);
var
  ANewUser: string;
  AText: string;
begin
  ANewUser := Trim(TdxRichEditDialogs.InputBox(cxGetResourceString(@sdxRangeEditingPermissionsAddUsers),
    cxGetResourceString(@sdxRangeEditingPermissionsEnterUserNames), ''));
  if ANewUser = '' then
    Exit;
  if not Controller.ValidNserName(ANewUser) then
  begin
    AText := cxGetResourceString(@sdxRangeEditingPermissionsInvalidUserNames);
    Application.MessageBox(PChar(AText), PChar(Application.Title), MB_OK + MB_ICONWARNING);
  end
  else
    if Controller.AddUser(ANewUser) then
      PopulateUsers;
end;

function TdxRangeEditingPermissionsForm.CreateController(
  AControllerParameters: TdxFormControllerParameters): TdxFormController;
begin
  Result := TdxRangeEditingPermissionsFormController.Create(AControllerParameters as
    TdxRangeEditingPermissionsFormControllerParameters);
end;

procedure TdxRangeEditingPermissionsForm.PopulateUserGroups;
var
  ACount, I: Integer;
  AItem: TcxCheckListBoxItem;
begin
  ACount := Controller.AvailableUserGroups.Count;
  for I := 0 to ACount - 1 do
  begin
    AItem := ListUserGroups.Items.Add;
    AItem.Text := Controller.AvailableUserGroups[I];
    AItem.Checked := Controller.CheckedUserGroups.IndexOf(AItem.Text) >= 0;
  end;
end;

procedure TdxRangeEditingPermissionsForm.PopulateUsers;
var
  ACount, I: Integer;
  AItem: TcxCheckListBoxItem;
begin
  ListUsers.Items.Clear;
  if Controller.AvailableUsers is TStringList then
    TStringList(Controller.AvailableUsers).Sort;
  ACount := Controller.AvailableUsers.Count;
  for I := 0 to ACount - 1 do
  begin
    AItem := ListUsers.Items.Add;
    AItem.Text := Controller.AvailableUsers[I];
    AItem.Checked := Controller.CheckedUsers.IndexOf(AItem.Text) >= 0;
  end;
end;

procedure TdxRangeEditingPermissionsForm.UpdateFormCore;
begin
  PopulateUsers;
  PopulateUserGroups;
  btnMoreUsers.Visible := not Controller.HasUserListService;
end;

end.
