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

unit dxSpellCheckerDialogs;

{$I cxVer.inc}

interface

uses
  cxLookAndFeels, dxSpellChecker, dxCustomDictionaryDialog, dxSpellCheckerOutlookDialog,
  dxSpellCheckerWordDialog, dxSpellCheckerSpellingOptionsDialog,
  dxSpellCheckerAutoCorrectOptionsDialog, dxSpellCheckerAutoCorrectExceptionDialog;

const
  dxCustomDictionaryDialogClass: TdxCustomDictionaryFormClass = TfmCustomDictionaryForm;
  dxSpellCheckerAutoCorrectExceptionsDialogClass: TdxCustomSpellCheckerAutoCorrectFormClass = TfmSpellCheckerAutoCorrectExceptionsForm;
  dxSpellCheckerAutoCorrectOptionsDialogClass: TdxCustomSpellCheckerAutoCorrectFormClass = TfmSpellCheckerAutoCorrectOptionsForm;
  dxSpellCheckerOutlookDialogClass: TdxCustomSpellCheckerFormClass = TfmSpellCheckerOutlookForm;
  dxSpellCheckerWordDialogClass: TdxCustomSpellCheckerFormClass = TfmSpellCheckerWordForm;
  dxSpellCheckerSpellingOptionsDialogClass: TfmSpellCheckerSpellingOptionsFormClass = TfmSpellCheckerSpellingOptionsForm;


function dxShowCustomDictionaryDialog(ADictionary: TdxUserSpellCheckerDictionary;
  ALookAndFeel: TcxLookAndFeel = nil): Integer;
function dxShowAutoCorrectExceptionsDialog(ASpellChecker: TdxCustomSpellChecker;
  ALookAndFeel: TcxLookAndFeel = nil): Integer;
function dxShowAutoCorrectOptionsDialog(ASpellChecker: TdxCustomSpellChecker;
  ALookAndFeel: TcxLookAndFeel = nil): Integer;
function dxShowOutlookSpellingDialog(ASpellChecker: TdxCustomSpellChecker;
  ALookAndFeel: TcxLookAndFeel = nil): Integer;
function dxShowSpellingOptionsDialog(ASpellChecker: TdxCustomSpellChecker;
  ALookAndFeel: TcxLookAndFeel = nil): Integer;
function dxShowWordSpellingDialog(ASpellChecker: TdxCustomSpellChecker;
  ALookAndFeel: TcxLookAndFeel = nil): Integer;

implementation

function dxShowCustomDictionaryDialog(
  ADictionary: TdxUserSpellCheckerDictionary;
  ALookAndFeel: TcxLookAndFeel = nil): Integer;
var
  ADialog: TdxCustomDictionaryForm;
begin
  ADialog := dxCustomDictionaryDialogClass.CreateEx(ADictionary);
  try
    if ALookAndFeel = nil then
      ALookAndFeel := ADictionary.SpellChecker.DialogLookAndFeel;
    SetControlLookAndFeel(ADialog, ALookAndFeel);
    Result := ADialog.ShowModal;
  finally
    ADialog.Free;
  end;
end;

function dxShowAutoCorrectExceptionsDialog(ASpellChecker: TdxCustomSpellChecker;
  ALookAndFeel: TcxLookAndFeel = nil): Integer;
var
  ADialog: TdxCustomSpellCheckerAutoCorrectForm;
begin
  ADialog := dxSpellCheckerAutoCorrectExceptionsDialogClass.CreateEx(ASpellChecker.AutoCorrectOptions);
  try
    if ALookAndFeel = nil then
      ALookAndFeel := ASpellChecker.DialogLookAndFeel;
    SetControlLookAndFeel(ADialog, ALookAndFeel);
    Result := ADialog.ShowModal;
  finally
    ADialog.Free;
  end;
end;

function dxShowAutoCorrectOptionsDialog(ASpellChecker: TdxCustomSpellChecker;
  ALookAndFeel: TcxLookAndFeel = nil): Integer;
var
  ADialog: TdxCustomSpellCheckerAutoCorrectForm;
begin
  ADialog := dxSpellCheckerAutoCorrectOptionsDialogClass.CreateEx(ASpellChecker.AutoCorrectOptions);
  try
    if ALookAndFeel = nil then
      ALookAndFeel := ASpellChecker.DialogLookAndFeel;
    SetControlLookAndFeel(ADialog, ALookAndFeel);
    Result := ADialog.ShowModal;
  finally
    ADialog.Free;
  end;
end;

function dxShowOutlookSpellingDialog(ASpellChecker: TdxCustomSpellChecker;
  ALookAndFeel: TcxLookAndFeel = nil): Integer;
var
  ADialog: TdxCustomSpellCheckerForm;
begin
  ADialog := dxSpellCheckerOutlookDialogClass.CreateEx(ASpellChecker);
  try
    if ALookAndFeel = nil then
      ALookAndFeel := ASpellChecker.DialogLookAndFeel;
    SetControlLookAndFeel(ADialog, ALookAndFeel);
    Result := ADialog.ShowModal;
  finally
    ADialog.Free;
  end;
end;

function dxShowSpellingOptionsDialog(ASpellChecker: TdxCustomSpellChecker;
  ALookAndFeel: TcxLookAndFeel = nil): Integer;
var
  ADialog: TfmSpellCheckerSpellingOptionsForm;
begin
  ADialog := dxSpellCheckerSpellingOptionsDialogClass.CreateEx(ASpellChecker);
  try
    if ALookAndFeel = nil then
      ALookAndFeel := ASpellChecker.DialogLookAndFeel;
    SetControlLookAndFeel(ADialog, ALookAndFeel);
    Result := ADialog.ShowModal;
  finally
    ADialog.Free;
  end;
end;

function dxShowWordSpellingDialog(ASpellChecker: TdxCustomSpellChecker;
  ALookAndFeel: TcxLookAndFeel = nil): Integer;
var
  ADialog: TdxCustomSpellCheckerForm;
begin
  ADialog := dxSpellCheckerWordDialogClass.CreateEx(ASpellChecker);
  try
    if ALookAndFeel = nil then
      ALookAndFeel := ASpellChecker.DialogLookAndFeel;
    SetControlLookAndFeel(ADialog, ALookAndFeel);
    Result := ADialog.ShowModal;
  finally
    ADialog.Free;
  end;
end;

end.
