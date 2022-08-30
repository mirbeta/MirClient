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

unit dxRichEdit.Dialogs.MergeOptions;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
{$IFDEF DELPHIXE2}
  System.UITypes,
{$ENDIF}
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, dxCore, cxGraphics, cxControls, cxLookAndFeels,
  cxLookAndFeelPainters, dxLayoutLookAndFeels, cxClasses, dxLayoutContainer, dxLayoutControl, dxLayoutControlAdapters,
  StdCtrls, cxRadioGroup, Menus, cxButtons,
  dxRichEdit.Dialogs.Core,
  dxRichEdit.Dialogs.CustomDialog,
  dxRichEdit.Dialogs.MergeOptionsController,
  dxRichEdit.DocumentModel.MailMerge,
  dxRichEdit.Utils.Properties;

type

  TdxRichEditMergeOptionsDialogForm = class(TdxRichEditCustomDialogForm)
    btnCancel: TcxButton;
    btnOk: TcxButton;
    dxLayoutAutoCreatedGroup2: TdxLayoutAutoCreatedGroup;
    dxLayoutGroup1: TdxLayoutGroup;
    dxLayoutGroup2: TdxLayoutGroup;
    dxLayoutItem1: TdxLayoutItem;
    dxLayoutItem2: TdxLayoutItem;
    dxLayoutItem3: TdxLayoutItem;
    dxLayoutItem4: TdxLayoutItem;
    dxLayoutItem5: TdxLayoutItem;
    dxLayoutItem6: TdxLayoutItem;
    liMergeRecords: TdxLayoutSeparatorItem;
    liMergeTo: TdxLayoutSeparatorItem;
    rbMergeAllRecords: TcxRadioButton;
    rbMergeSelectedRecords: TcxRadioButton;
    rbMergeToFile: TcxRadioButton;
    rbMergeToWindow: TcxRadioButton;
    procedure rbMergeRecordsClick(Sender: TObject);
    procedure rbMergeToClick(Sender: TObject);
  private
    function GetController: TdxMergeOptionsFormController; inline;
  protected
    procedure ApplyLocalization; override;
    function GetMergeDestination: TdxMergeDestination; virtual;
    function GetMergeRecords: TdxMergeRecords; virtual;
    procedure SetMergeDestination(const Value: TdxMergeDestination); virtual;
    procedure SetMergeRecords(const Value: TdxMergeRecords); virtual;

    function CreateController(AControllerParameters: TdxFormControllerParameters): TdxFormController; override;
    procedure UpdateFormCore; override;

    property MergeDestination: TdxMergeDestination read GetMergeDestination write SetMergeDestination;
    property MergeRecords: TdxMergeRecords read GetMergeRecords write SetMergeRecords;
  public
    property Controller: TdxMergeOptionsFormController read GetController;
  end;

implementation

uses
  dxRichEdit.Dialogs.Strs;

{$R *.dfm}

{ TTdxRichEditMergeOptionsDialogForm }

function TdxRichEditMergeOptionsDialogForm.GetController: TdxMergeOptionsFormController;
begin
  Result := TdxMergeOptionsFormController(inherited Controller);
end;

procedure TdxRichEditMergeOptionsDialogForm.ApplyLocalization;
begin
  Caption := cxGetResourceString(@sdxRichEditMergeOptionsDialogForm);
  btnOk.Caption := cxGetResourceString(@sdxRichEditDialogButtonOk);
  btnCancel.Caption := cxGetResourceString(@sdxRichEditDialogButtonCancel);
  rbMergeAllRecords.Caption := cxGetResourceString(@sdxRichEditMergeOptionsDialogMergeAllRecords);
  rbMergeSelectedRecords.Caption := cxGetResourceString(@sdxRichEditMergeOptionsDialogMergeSelectedRecords);
  rbMergeToWindow.Caption := cxGetResourceString(@sdxRichEditMergeOptionsDialogMergeToWindow);
  rbMergeToFile.Caption := cxGetResourceString(@sdxRichEditMergeOptionsDialogMergeToFile);
  liMergeRecords.CaptionOptions.Text := cxGetResourceString(@sdxRichEditMergeOptionsDialogMergeRecords);
  liMergeTo.CaptionOptions.Text := cxGetResourceString(@sdxRichEditMergeOptionsDialogMergeTo);
end;

function TdxRichEditMergeOptionsDialogForm.CreateController(
  AControllerParameters: TdxFormControllerParameters): TdxFormController;
begin
  Result := TdxMergeOptionsFormController.Create(AControllerParameters as TdxMergeOptionsFormControllerParameters);
end;

function TdxRichEditMergeOptionsDialogForm.GetMergeDestination: TdxMergeDestination;
begin
  if rbMergeToWindow.Checked then
    Result := TdxMergeDestination.NewTab
  else
    Result := TdxMergeDestination.&File;
end;

function TdxRichEditMergeOptionsDialogForm.GetMergeRecords: TdxMergeRecords;
begin
  if rbMergeAllRecords.Checked then
    Result := TdxMergeRecords.All
  else
    Result := TdxMergeRecords.Selected;
end;

procedure TdxRichEditMergeOptionsDialogForm.rbMergeRecordsClick(Sender: TObject);
begin
  Controller.MergeRecords := MergeRecords;
end;

procedure TdxRichEditMergeOptionsDialogForm.rbMergeToClick(Sender: TObject);
begin
  Controller.MergeDestination := MergeDestination;
end;

procedure TdxRichEditMergeOptionsDialogForm.SetMergeDestination(const Value: TdxMergeDestination);
begin
  if Value = TdxMergeDestination.&File then
    rbMergeToFile.Checked := True
  else
    rbMergeToWindow.Checked := True;
end;

procedure TdxRichEditMergeOptionsDialogForm.SetMergeRecords(const Value: TdxMergeRecords);
begin
  if Value = TdxMergeRecords.Selected then
    rbMergeSelectedRecords.Checked := True
  else
    rbMergeAllRecords.Checked := True;
end;

procedure TdxRichEditMergeOptionsDialogForm.UpdateFormCore;
begin
  MergeRecords := Controller.MergeRecords;
  MergeDestination := Controller.MergeDestination;
end;

end.
