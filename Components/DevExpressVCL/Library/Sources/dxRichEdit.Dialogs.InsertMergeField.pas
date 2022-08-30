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

unit dxRichEdit.Dialogs.InsertMergeField;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
{$IFDEF DELPHIXE2}
  System.UITypes,
{$ENDIF}
  Generics.Defaults, Generics.Collections,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Menus,
  Controls, Forms, Dialogs, dxCore, cxGraphics, cxControls, cxLookAndFeels,
  cxLookAndFeelPainters, dxSkinsCore, dxLayoutLookAndFeels, cxClasses, dxLayoutContainer,
  dxLayoutControl, dxLayoutControlAdapters, StdCtrls, cxButtons, cxContainer, cxEdit, cxListBox, cxRadioGroup,
  dxRichEdit.Dialogs.Core,
  dxRichEdit.Dialogs.CustomDialog,
  dxRichEdit.DocumentModel.UnitConverter,
  dxRichEdit.DocumentModel.MailMerge,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.Types;

type

  { TdxRichEditInsertMergeFieldForm }

  TdxRichEditInsertMergeFieldForm = class(TdxRichEditCustomDialogForm)
    btnCancel: TcxButton;
    btnInsert: TcxButton;
    dxLayoutItem1: TdxLayoutItem;
    dxLayoutItem2: TdxLayoutItem;
    dxLayoutItem3: TdxLayoutItem;
    lblFields: TdxLayoutLabeledItem;
    lbMergeFields: TcxListBox;
    dxLayoutGroup1: TdxLayoutGroup;
    procedure btnInsertClick(Sender: TObject);
    procedure lbMergeFieldsDblClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
  private
    FDatabaseFieldsNames: TArray<TdxMergeFieldName>;
    function GetDocumentModel: TdxDocumentModel; inline;
  protected
    procedure CreateParams(var Params: TCreateParams); override;

    procedure ApplyLocalization; override;
    procedure InitializeForm; override;
    function CreateController(AControllerParameters: TdxFormControllerParameters): TdxFormController; override;

    function GetFieldNames: TArray<IdxRichEditMergeFieldName>;
    procedure ClearFieldsNames;
    procedure InsertMergeField;
    procedure UpdateMergeFieldList;

    property DocumentModel: TdxDocumentModel read GetDocumentModel;
    property DatabaseFieldsNames: TArray<TdxMergeFieldName> read FDatabaseFieldsNames;
  public
    destructor Destroy; override;
  end;

implementation

uses
  dxRichEdit.View.Core,
  dxRichEdit.Dialogs.Strs,
  dxRichEdit.Utils.Types,
  dxRichEdit.Commands.Fields,
  dxRichEdit.Commands.IDs,
  dxRichEdit.Control, dxCoreClasses;

{$R *.dfm}

type
  TdxCustomRichEditControlAccess = class(TdxCustomRichEditControl);

{ TdxRichEditInsertMergeFieldForm }

procedure TdxRichEditInsertMergeFieldForm.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  if (Control <> nil) and Control.Control.HandleAllocated then
    Params.WndParent := Control.Control.Handle;
end;

procedure TdxRichEditInsertMergeFieldForm.ApplyLocalization;
begin
  Caption := cxGetResourceString(@sdxRichEditInsertMergeFieldForm);
  btnInsert.Caption := cxGetResourceString(@sdxRichEditInsertMergeFieldButtonInsert);
  btnCancel.Caption := cxGetResourceString(@sdxRichEditInsertMergeFieldButtonCancel);
  lblFields.CaptionOptions.Text := cxGetResourceString(@sdxRichEditInsertMergeFieldFields);
end;

procedure TdxRichEditInsertMergeFieldForm.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TdxRichEditInsertMergeFieldForm.btnInsertClick(Sender: TObject);
begin
  InsertMergeField;
end;

function TdxRichEditInsertMergeFieldForm.GetDocumentModel: TdxDocumentModel;
begin
  Result := Control.InnerControl.DocumentModel;
end;

procedure TdxRichEditInsertMergeFieldForm.ClearFieldsNames;
var
  AFieldName: TdxMergeFieldName;
begin
  for AFieldName in FDatabaseFieldsNames do
    AFieldName.Free;
  Finalize(FDatabaseFieldsNames);
end;

function TdxRichEditInsertMergeFieldForm.CreateController(
  AControllerParameters: TdxFormControllerParameters): TdxFormController;
begin
  Result := nil;
end;

destructor TdxRichEditInsertMergeFieldForm.Destroy;
begin
  ClearFieldsNames;
  inherited Destroy;
end;

function TdxRichEditInsertMergeFieldForm.GetFieldNames: TArray<IdxRichEditMergeFieldName>;
var
  I, ACount: Integer;
  AMergeFieldsNames: TArray<IdxRichEditMergeFieldName>;
  AArgs: TdxCustomizeMergeFieldsEventArgs;
  AControl: TdxCustomRichEditControl;
begin
  ClearFieldsNames;
  FDatabaseFieldsNames := DocumentModel.GetDatabaseFieldNames;
  ACount := Length(FDatabaseFieldsNames);
  if ACount = 0 then
    Exit(nil);
  SetLength(AMergeFieldsNames, ACount);
  for I := 0 to ACount - 1 do
    AMergeFieldsNames[I] := FDatabaseFieldsNames[I];
  AArgs := TdxCustomizeMergeFieldsEventArgs.Create(AMergeFieldsNames);
  try
    AControl := Control.GetControl as TdxCustomRichEditControl;
    TdxCustomRichEditControlAccess(AControl).DoCustomizeMergeFields(AControl, AArgs);
    Result := AArgs.MergeFieldsNames;
  finally
    AArgs.Free;
  end;
end;

procedure TdxRichEditInsertMergeFieldForm.InitializeForm;
begin
  UpdateMergeFieldList;
  RecreateWnd;
end;

procedure TdxRichEditInsertMergeFieldForm.InsertMergeField;
var
  AArgument: TdxMergeFieldName;
  ACommand: TdxInsertMergeFieldCommand;
begin
  AArgument := TdxMergeFieldName(lbMergeFields.ItemObject);
  ACommand := Safe<TdxInsertMergeFieldCommand>.Cast(Control.InnerControl.CreateCommand(TdxRichEditCommandId.InsertMailMergeField));
  if ACommand <> nil then
  try
    ACommand.FieldArgument := AArgument.Name;
    ACommand.Execute;
  finally
    ACommand.Free;
  end;
end;

procedure TdxRichEditInsertMergeFieldForm.lbMergeFieldsDblClick(Sender: TObject);
begin
  InsertMergeField;
end;

procedure TdxRichEditInsertMergeFieldForm.UpdateMergeFieldList;
var
  AFieldNames: TArray<IdxRichEditMergeFieldName>;
  AFieldName: IdxRichEditMergeFieldName;
begin
  lbMergeFields.Items.BeginUpdate;
  try
    lbMergeFields.Items.Clear;
    AFieldNames := GetFieldNames;
    if (AFieldNames = nil) or (Length(AFieldNames) = 0) then
      btnInsert.Enabled := False
    else
    begin
      for AFieldName in AFieldNames do
        lbMergeFields.Items.AddObject(AFieldName.DisplayName, AFieldName as TdxMergeFieldName);
      lbMergeFields.ItemIndex := 0;
    end;
  finally
    lbMergeFields.Items.EndUpdate;
  end;
end;

end.
