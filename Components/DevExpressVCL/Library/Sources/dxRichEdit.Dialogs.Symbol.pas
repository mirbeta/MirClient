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

unit dxRichEdit.Dialogs.Symbol;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, StdCtrls, Menus,
  Controls, Forms, Dialogs, dxCore, dxRichEdit.Dialogs.CustomDialog, cxGraphics, cxControls,
  cxLookAndFeels, cxLookAndFeelPainters, ComCtrls, cxClasses, cxTextEdit, cxMaskEdit,
  dxSymbolListBox, cxListBox, cxContainer, cxEdit, dxLayoutcxEditAdapters,
  dxLayoutContainer, dxLayoutControlAdapters, cxButtons, cxDropDownEdit,
  cxFontNameComboBox, dxRichEditFontNameComboBox, dxLayoutControl,
  dxRichEdit.Dialogs.Core, dxRichEdit.Dialogs.SymbolFormController, dxLayoutLookAndFeels;

type
  TdxRichEditSymbolDialogForm = class(TdxRichEditCustomDialogForm)
    btnCancel: TcxButton;
    btnOk: TcxButton;
    cmbFont: TdxRichEditFontNameComboBox;
    dxLayoutControl1Group1: TdxLayoutAutoCreatedGroup;
    dxLayoutControl1Item2: TdxLayoutItem;
    dxLayoutControl1Item3: TdxLayoutItem;
    dxLayoutControl1Item4: TdxLayoutItem;
    edtCharacterCode: TcxTextEdit;
    lbSymbolList: TdxSymbolListBox;
    lciCharacterCode: TdxLayoutItem;
    lciFont: TdxLayoutItem;
    procedure cmbFontPropertiesChange(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure edtCharacterCodePropertiesChange(Sender: TObject);
    procedure edtCharacterCodeKeyPress(Sender: TObject; var Key: Char);
  private
    function GetController: TdxRichEditInsertSymbolController;
  protected
    procedure ApplyChanges;
    procedure ApplyLocalization; override;
    function CreateController(AControllerParameters: TdxFormControllerParameters): TdxFormController; override;
    procedure ChangeCharacterCode(AValue: Word);
    procedure InitializeForm; override;
    procedure SelectedIndexSymbolChanged(Sender: TObject);
    procedure SubscribeControlsEvents; override;
    procedure UnsubscribeControlsEvents; override;
    procedure UpdateFormCore; override;
  public
    property Controller: TdxRichEditInsertSymbolController read GetController;
  end;

implementation

uses
  dxRichEdit.Dialogs.Strs;

{$R *.dfm}

procedure TdxRichEditSymbolDialogForm.cmbFontPropertiesChange(Sender: TObject);
var
  AFontName: string;
begin
  AFontName := cmbFont.Text;
  lbSymbolList.FontName := AFontName;
  Controller.FontName := AFontName;
  Controller.UnicodeChar := lbSymbolList.SelectedChar;
end;

procedure TdxRichEditSymbolDialogForm.btnOkClick(Sender: TObject);
begin
  ApplyChanges;
end;

procedure TdxRichEditSymbolDialogForm.ChangeCharacterCode(AValue: Word);
begin
  edtCharacterCode.Text := IntToStr(AValue);
end;

function TdxRichEditSymbolDialogForm.GetController: TdxRichEditInsertSymbolController;
begin
  Result := TdxRichEditInsertSymbolController(inherited Controller);
end;

function TdxRichEditSymbolDialogForm.CreateController(
  AControllerParameters: TdxFormControllerParameters): TdxFormController;
begin
  Result := TdxRichEditInsertSymbolController.Create(AControllerParameters as TdxRichEditInsertSymbolControllerParameters);
end;

procedure TdxRichEditSymbolDialogForm.edtCharacterCodeKeyPress(Sender: TObject; var Key: Char);
begin
  if not CharInSet(Key, [#8, '0'..'9']) then
    Key := #0;
end;

procedure TdxRichEditSymbolDialogForm.edtCharacterCodePropertiesChange(Sender: TObject);
var
  ANewValue: Integer;
begin
  ANewValue := StrToIntDef(edtCharacterCode.Text, -1);
  if (ANewValue > 0) and lbSymbolList.IsCodeChar(ANewValue) then
  begin
    UnsubscribeControlsEvents;
    try
      lbSymbolList.SelectedCharCode := ANewValue;
    finally
      SubscribeControlsEvents;
    end;
  end;
end;

procedure TdxRichEditSymbolDialogForm.ApplyChanges;
begin
  Controller.ApplyChanges;
  ModalResult := mrOk;
end;

procedure TdxRichEditSymbolDialogForm.ApplyLocalization;
begin
  Caption := cxGetResourceString(@sdxRichEditSymbolDialogForm);
  btnOk.Caption := cxGetResourceString(@sdxRichEditDialogButtonOk);
  btnCancel.Caption := cxGetResourceString(@sdxRichEditDialogButtonCancel);
  lciFont.CaptionOptions.Text := cxGetResourceString(@sdxRichEditSymbolDialogFont);
  lciCharacterCode.CaptionOptions.Text := cxGetResourceString(@sdxRichEditSymbolDialogCharacterCode);
end;

procedure TdxRichEditSymbolDialogForm.InitializeForm;
begin
end;

procedure TdxRichEditSymbolDialogForm.SelectedIndexSymbolChanged(Sender: TObject);
begin
  UnsubscribeControlsEvents;
  try
    Controller.UnicodeChar := lbSymbolList.SelectedChar;
    ChangeCharacterCode(lbSymbolList.SelectedCharCode);
  finally
    SubscribeControlsEvents;
  end;
end;

procedure TdxRichEditSymbolDialogForm.SubscribeControlsEvents;
begin
  cmbFont.Properties.OnChange := cmbFontPropertiesChange;
  lbSymbolList.OnDblClick := btnOkClick;
  lbSymbolList.OnSelectedIndexSymbolChanged := SelectedIndexSymbolChanged;
  edtCharacterCode.Properties.OnChange := edtCharacterCodePropertiesChange;
end;

procedure TdxRichEditSymbolDialogForm.UnsubscribeControlsEvents;
begin
  cmbFont.Properties.OnChange := nil;
  lbSymbolList.OnDblClick := nil;
  lbSymbolList.OnSelectedIndexSymbolChanged := nil;
  edtCharacterCode.Properties.OnChange := nil;
end;

procedure TdxRichEditSymbolDialogForm.UpdateFormCore;
begin
  cmbFont.Text := Controller.FontName;
  lbSymbolList.FontName := Controller.FontName;
  lbSymbolList.SelectedChar := Controller.UnicodeChar;
  ChangeCharacterCode(lbSymbolList.SelectedCharCode);
end;

end.
