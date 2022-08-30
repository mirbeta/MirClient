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

unit dxSpreadSheetProtectSheetDialog;

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
  dxLayoutContainer, dxLayoutControl, dxLayoutLookAndFeels, dxSpreadSheetCore, dxSpreadSheetProtection, dxForms;

type

  { TdxSpreadSheetProtectSheetDialogForm }

  TdxSpreadSheetProtectSheetDialogFormClass = class of TdxSpreadSheetProtectSheetDialogForm;
  TdxSpreadSheetProtectSheetDialogForm = class(TdxForm)
    btnCancel: TcxButton;
    btnOK: TcxButton;
    cbProtected: TcxCheckBox;
    clbPermissions: TcxCheckListBox;
    dxLayoutAutoCreatedGroup1: TdxLayoutAutoCreatedGroup;
    dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel;
    dxLayoutItem1: TdxLayoutItem;
    dxLayoutItem2: TdxLayoutItem;
    dxLayoutItem3: TdxLayoutItem;
    dxLayoutLookAndFeelList: TdxLayoutLookAndFeelList;
    edPassword: TcxTextEdit;
    lcMain: TdxLayoutControl;
    lcMainGroup_Root: TdxLayoutGroup;
    liPassword: TdxLayoutItem;
    liPermissions: TdxLayoutItem;

    procedure btnOKClick(Sender: TObject);
    procedure cbProtectedClick(Sender: TObject);
    procedure clbPermissionsClickCheck(Sender: TObject; AIndex: Integer; APrevState, ANewState: TcxCheckBoxState);
  protected
    FOptions: TdxSpreadSheetSheetProtectionOptions;
    FPermissions: TList<TPair<string, Pointer>>;
    FSheet: TdxSpreadSheetTableView;

    procedure ApplyLocalizations; virtual;
    procedure Initialize(ASheet: TdxSpreadSheetTableView); virtual;
    procedure PopulatePermissions;

    procedure Load(AOptions: TdxSpreadSheetSheetProtectionOptions); virtual;
    procedure LoadState;
    procedure Save(AOptions: TdxSpreadSheetSheetProtectionOptions); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    //
    property Sheet: TdxSpreadSheetTableView read FSheet;
  end;

var
  dxSpreadSheetProtectSheetDialogClass: TdxSpreadSheetProtectSheetDialogFormClass = TdxSpreadSheetProtectSheetDialogForm;

procedure ShowProtectSheetDialog(ASheet: TdxSpreadSheetTableView);
implementation

uses
  dxSpreadSheetDialogStrs, dxSpreadSheetPasswordDialog;

{$R *.dfm}

procedure ShowProtectSheetDialog(ASheet: TdxSpreadSheetTableView);
var
  ADialog: TdxSpreadSheetProtectSheetDialogForm;
begin
  ADialog := dxSpreadSheetProtectSheetDialogClass.Create(GetParentForm(ASheet.SpreadSheet));
  try
    ADialog.Initialize(ASheet);
    ADialog.Load(ASheet.OptionsProtection);
    if ADialog.ShowModal = mrOk then
      ADialog.Save(ASheet.OptionsProtection);
  finally
    ADialog.Free;
  end;
end;

{ TdxSpreadSheetProtectSheetDialogForm }

constructor TdxSpreadSheetProtectSheetDialogForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOptions := TdxSpreadSheetSheetProtectionOptions.Create;
  FPermissions := TList<TPair<string, Pointer>>.Create;
  PopulatePermissions;
end;

destructor TdxSpreadSheetProtectSheetDialogForm.Destroy;
begin
  FreeAndNil(FPermissions);
  FreeAndNil(FOptions);
  inherited Destroy;
end;

procedure TdxSpreadSheetProtectSheetDialogForm.ApplyLocalizations;
var
  I: Integer;
begin
  Caption := cxGetResourceString(@sdxProtectSheetDialogCaption);

  btnOK.Caption := cxGetResourceString(@sdxProtectSheetDialogButtonOK);
  btnCancel.Caption := cxGetResourceString(@sdxProtectSheetDialogButtonCancel);
  liPassword.Caption := cxGetResourceString(@sdxProtectSheetDialogPassword);
  liPermissions.Caption := cxGetResourceString(@sdxProtectSheetDialogPermissions);
  cbProtected.Caption := cxGetResourceString(@sdxProtectSheetDialogProtect);

  clbPermissions.Items.BeginUpdate;
  try
    for I := 0 to clbPermissions.Items.Count - 1 do
      clbPermissions.Items[I].Text := cxGetResourceString(FPermissions[I].Value);
  finally
    clbPermissions.Items.EndUpdate;
  end;
end;

procedure TdxSpreadSheetProtectSheetDialogForm.Initialize(ASheet: TdxSpreadSheetTableView);
var
  I: Integer;
begin
  FSheet := ASheet;
  SetControlLookAndFeel(Self, Sheet.SpreadSheet.DialogsLookAndFeel);
  liPassword.Visible := dxSpreadSheetDefaultProtectionProvider <> nil;

  clbPermissions.Items.BeginUpdate;
  try
    clbPermissions.Items.Clear;
    for I := 0 to FPermissions.Count - 1 do
      clbPermissions.Items.Add;
  finally
    clbPermissions.Items.EndUpdate;
  end;

  ApplyLocalizations;
end;

procedure TdxSpreadSheetProtectSheetDialogForm.PopulatePermissions;
begin
  FPermissions.Add(TPair<string, Pointer>.Create('AllowSelectLockedCells', @sdxProtectSheetDialogAllowSelectLockedCells));
  FPermissions.Add(TPair<string, Pointer>.Create('AllowSelectUnlockedCells', @sdxProtectSheetDialogAllowSelectUnlockedCells));
  FPermissions.Add(TPair<string, Pointer>.Create('AllowFormatCells', @sdxProtectSheetDialogAllowFormatCells));
  FPermissions.Add(TPair<string, Pointer>.Create('AllowResizeColumns', @sdxProtectSheetDialogAllowResizeColumns));
  FPermissions.Add(TPair<string, Pointer>.Create('AllowResizeRows', @sdxProtectSheetDialogAllowResizeRows));
  FPermissions.Add(TPair<string, Pointer>.Create('AllowEditContainers', @sdxProtectSheetDialogAllowEditContainers));
  FPermissions.Add(TPair<string, Pointer>.Create('AllowEditHyperlinks', @sdxProtectSheetDialogAllowEditHyperlinks));
  FPermissions.Add(TPair<string, Pointer>.Create('AllowInsertColumns', @sdxProtectSheetDialogAllowInsertColumns));
  FPermissions.Add(TPair<string, Pointer>.Create('AllowInsertRows', @sdxProtectSheetDialogAllowInsertRows));
  FPermissions.Add(TPair<string, Pointer>.Create('AllowDeleteColumns', @sdxProtectSheetDialogAllowDeleteColumns));
  FPermissions.Add(TPair<string, Pointer>.Create('AllowDeleteRows', @sdxProtectSheetDialogAllowDeleteRows));
  FPermissions.Add(TPair<string, Pointer>.Create('AllowSort', @sdxProtectSheetDialogAllowSort));
end;

procedure TdxSpreadSheetProtectSheetDialogForm.Load(AOptions: TdxSpreadSheetSheetProtectionOptions);
begin
  FOptions.Assign(AOptions);
  LoadState;
end;

procedure TdxSpreadSheetProtectSheetDialogForm.LoadState;
var
  I: Integer;
begin
  clbPermissions.Items.BeginUpdate;
  try
    for I := 0 to clbPermissions.Items.Count - 1 do
      clbPermissions.Items[I].Checked := GetOrdProp(FOptions, FPermissions[I].Key) <> 0;
  finally
    clbPermissions.Items.EndUpdate;
  end;
end;

procedure TdxSpreadSheetProtectSheetDialogForm.Save(AOptions: TdxSpreadSheetSheetProtectionOptions);
begin
  AOptions.Assign(FOptions);
  AOptions.Protected := cbProtected.Checked;
  if edPassword.Text <> '' then
    AOptions.ProtectionInfo := dxSpreadSheetDefaultProtectionProvider.Create(edPassword.Text)
  else
    AOptions.ProtectionInfo := nil;
end;

procedure TdxSpreadSheetProtectSheetDialogForm.btnOKClick(Sender: TObject);
begin
  if (edPassword.Text = '') or TdxSpreadSheetPasswordDialogForm.ExecuteConfirmation(Self, Sheet.SpreadSheet, edPassword.Text) then
    ModalResult := mrOk;
end;

procedure TdxSpreadSheetProtectSheetDialogForm.cbProtectedClick(Sender: TObject);
begin
  btnOK.Enabled := cbProtected.Checked;
end;

procedure TdxSpreadSheetProtectSheetDialogForm.clbPermissionsClickCheck(
  Sender: TObject; AIndex: Integer; APrevState, ANewState: TcxCheckBoxState);
begin
  SetOrdProp(FOptions, FPermissions[AIndex].Key, Ord(ANewState = cbsChecked));
  LoadState;
end;

end.
