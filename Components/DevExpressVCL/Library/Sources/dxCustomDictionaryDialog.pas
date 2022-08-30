{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressSpellChecker                                      }
{                                                                    }
{           Copyright (c) 1998-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSSPELLCHECKER AND ALL           }
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

unit dxCustomDictionaryDialog;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, Menus, StdCtrls, dxCore,
  cxLookAndFeelPainters, cxButtons, cxControls, cxContainer, cxEdit, cxTextEdit, cxMemo, dxSpellChecker,
  dxSpellCheckerBaseForm, cxGraphics, cxLookAndFeels, dxLayoutcxEditAdapters, dxLayoutControlAdapters,
  dxLayoutContainer, cxClasses, dxLayoutControl, dxLayoutLookAndFeels;

type

  { TdxCustomDictionaryForm }

  TdxCustomDictionaryFormClass = class of TdxCustomDictionaryForm;
  TdxCustomDictionaryForm = class(TfmSpellCheckerBaseForm)
  private
    FDictionary: TdxUserSpellCheckerDictionary;
  protected
    procedure ApplyChanges; virtual;
    procedure Localize; override;
  protected
    property Dictionary: TdxUserSpellCheckerDictionary read FDictionary;
  public
    constructor CreateEx(ADictionary: TdxUserSpellCheckerDictionary); virtual;
    function ShowModal: Integer; override;
  end;

  { TfmCustomDictionaryForm }

  TfmCustomDictionaryForm = class(TdxCustomDictionaryForm)
    btnCancel: TcxButton;
    btnOk: TcxButton;
    dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel;
    dxLayoutGroup1: TdxLayoutGroup;
    dxLayoutItem1: TdxLayoutItem;
    dxLayoutItem2: TdxLayoutItem;
    dxLayoutItem3: TdxLayoutItem;
    dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList;
    lcMain: TdxLayoutControl;
    lcMainGroup_Root: TdxLayoutGroup;
    meDictionary: TcxMemo;

    procedure meDictionaryPropertiesChange(Sender: TObject);
  protected
    procedure ApplyChanges; override;
    procedure Localize; override;
  public
    constructor CreateEx(ADictionary: TdxUserSpellCheckerDictionary); override;
  end;

implementation

uses
  dxSpellCheckerStrs;

{$R *.dfm}

{ TdxCustomDictionaryForm }

constructor TdxCustomDictionaryForm.CreateEx(ADictionary: TdxUserSpellCheckerDictionary);
begin
  inherited Create(Application);
  FDictionary := ADictionary;
  Localize;
end;

procedure TdxCustomDictionaryForm.ApplyChanges;
begin
end;

procedure TdxCustomDictionaryForm.Localize;
begin
  Caption := cxGetResourceString(@sdxSpellCheckerCustomDictionaryFormCaption);
end;

function TdxCustomDictionaryForm.ShowModal: Integer;
begin
  Result := inherited ShowModal;
  if Result = mrOk then
    ApplyChanges;
end;

{ TfmCustomDictionaryForm }

constructor TfmCustomDictionaryForm.CreateEx(
  ADictionary: TdxUserSpellCheckerDictionary);
begin
  inherited CreateEx(ADictionary);
  Dictionary.SaveToStrings(meDictionary.Lines);
end;

procedure TfmCustomDictionaryForm.ApplyChanges;
begin
  Dictionary.LoadFromStrings(meDictionary.Lines);
end;

procedure TfmCustomDictionaryForm.Localize;
begin
  inherited Localize;
  btnOk.Caption := cxGetResourceString(@sdxSpellCheckerOkButton);
  btnCancel.Caption := cxGetResourceString(@sdxSpellCheckerCancelButton);
end;

procedure TfmCustomDictionaryForm.meDictionaryPropertiesChange(Sender: TObject);
begin
  btnOk.Enabled := True;
end;

end.
