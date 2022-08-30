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

unit dxSpellCheckerAutoCorrectExceptionDialog;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, Menus, StdCtrls, cxClasses,
  dxSpellCheckerBaseForm, dxSpellChecker, cxLookAndFeelPainters, cxControls, cxContainer, cxEdit, cxGroupBox, cxButtons,
  dxSpellCheckerAutoCorrectOptionsDialog, cxGraphics, cxLookAndFeels, dxLayoutControlAdapters, dxLayoutContainer,
  dxLayoutControl;

type
  { TfmSpellCheckerAutoCorrectExceptionsForm }

  TfmSpellCheckerAutoCorrectExceptionsForm = class(TdxCustomSpellCheckerAutoCorrectForm)
    btnClose: TcxButton;
    btnOk: TcxButton;
    lcMain: TdxLayoutControl;
    lcMainGroup_Root: TdxLayoutGroup;
    dxLayoutGroup1: TdxLayoutGroup;
    lgFirstLetter: TdxLayoutGroup;
    lgInitialCaps: TdxLayoutGroup;
    dxLayoutGroup4: TdxLayoutGroup;
    dxLayoutItem1: TdxLayoutItem;
    dxLayoutItem2: TdxLayoutItem;
  strict private
    FFirstLetterExceptionsFrame: TCustomFrame;
    FInitialCapsExceptionsFrame: TCustomFrame;

    function IsValidCandidate(const S: string; AMinLength: Integer = 1; const APossibleLastChar: WideChar = #0): Boolean;
  protected
    procedure ApplyChanges; override;
    procedure FirstLetterExceptionsExistHandler(var S: string; var AResult: Boolean);
    procedure FirstLetterExceptionsValidateHandler(var S: string; var AResult: Boolean);
    procedure InitialCapsExceptionsValidateHandler(var S: string; var AResult: Boolean);
    procedure Initialize; virtual;
    procedure Localize; override;
  public
    constructor CreateEx(AOptions: TdxSpellCheckerAutoCorrectOptions); override;
    destructor Destroy; override;
  end;

implementation

uses
  dxSpellCheckerStrs, dxSpellCheckerExceptionsFrame, dxSpellCheckerUtils, dxCore;

{$R *.dfm}

{ TfmSpellCheckerAutoCorrectExceptionsForm }

constructor TfmSpellCheckerAutoCorrectExceptionsForm.CreateEx(AOptions: TdxSpellCheckerAutoCorrectOptions);
begin
  inherited CreateEx(AOptions);
  Initialize;
end;

destructor TfmSpellCheckerAutoCorrectExceptionsForm.Destroy;
begin
  FreeAndNil(FFirstLetterExceptionsFrame);
  FreeAndNil(FInitialCapsExceptionsFrame);
  inherited Destroy;
end;

procedure TfmSpellCheckerAutoCorrectExceptionsForm.ApplyChanges;
begin
  Options.FirstLetterExceptions.AutoInclude := TfrmSpellCheckerExceptions(FFirstLetterExceptionsFrame).cbAutoInclude.Checked;
  Options.InitialCapsExceptions.AutoInclude := TfrmSpellCheckerExceptions(FInitialCapsExceptionsFrame).cbAutoInclude.Checked;
end;

procedure TfmSpellCheckerAutoCorrectExceptionsForm.FirstLetterExceptionsExistHandler(var S: string; var AResult: Boolean);
var
  ALen: Integer;
begin
  FirstLetterExceptionsValidateHandler(S, AResult);
  if AResult then
  begin
    ALen := Length(S);
    if S[ALen] <> '.' then
      S := S + '.';
    AResult := Options.FirstLetterExceptions.Find(S) <> -1;
  end;
end;

procedure TfmSpellCheckerAutoCorrectExceptionsForm.FirstLetterExceptionsValidateHandler(var S: string; var AResult: Boolean);
begin
  AResult := IsValidCandidate(S, 1, '.');
end;

procedure TfmSpellCheckerAutoCorrectExceptionsForm.InitialCapsExceptionsValidateHandler(var S: string; var AResult: Boolean);
begin
  AResult := IsValidCandidate(S, 3);
end;

procedure TfmSpellCheckerAutoCorrectExceptionsForm.Initialize;

  procedure AddToGroup(AGroup: TdxLayoutGroup; AControl: TControl);
  var
    AItem: TdxLayoutItem;
  begin
    AItem := TdxLayoutItem.Create(AGroup);
    AItem.Parent := AGroup;
    AItem.Control :=  AControl;
    AItem.AlignHorz := ahClient;
    AItem.AlignVert := avClient;
    AItem.CaptionOptions.Visible := False;
  end;

begin
  lcMain.BeginUpdate;
  try
    FFirstLetterExceptionsFrame := TfrmSpellCheckerExceptions.CreateEx(nil, Options.FirstLetterExceptions, 0);
    TfrmSpellCheckerExceptions(FFirstLetterExceptionsFrame).OnCandidateExist := FirstLetterExceptionsExistHandler;
    TfrmSpellCheckerExceptions(FFirstLetterExceptionsFrame).OnCandidateValidate := FirstLetterExceptionsValidateHandler;
    AddToGroup(lgFirstLetter, FFirstLetterExceptionsFrame);

    FInitialCapsExceptionsFrame := TfrmSpellCheckerExceptions.CreateEx(nil, Options.InitialCapsExceptions, 1);
    TfrmSpellCheckerExceptions(FInitialCapsExceptionsFrame).OnCandidateValidate := InitialCapsExceptionsValidateHandler;
    AddToGroup(lgInitialCaps, FInitialCapsExceptionsFrame);
  finally
    lcMain.EndUpdate(False);
  end;
end;

procedure TfmSpellCheckerAutoCorrectExceptionsForm.Localize;
begin
  Caption := cxGetResourceString(@sdxSpellCheckerAutoCorrectExceptionsFormCaption);
  btnOk.Caption := cxGetResourceString(@sdxSpellCheckerOkButton);
  btnClose.Caption := cxGetResourceString(@sdxSpellCheckerCloseButton);
  lgFirstLetter.Caption := cxGetResourceString(@sdxSpellCheckerFirstLetterExceptions);
  lgInitialCaps.Caption := cxGetResourceString(@sdxSpellCheckerInitialCapsExceptions);
end;

function TfmSpellCheckerAutoCorrectExceptionsForm.IsValidCandidate(
  const S: string; AMinLength: Integer = 1; const APossibleLastChar: WideChar = #0): Boolean;
var
  ALen, I: Integer;
begin
  Result := False;
  ALen := Length(S);
  if ALen >= AMinLength then
  begin
    Result := True;
    for I := 1 to ALen do
    begin
      Result := dxWideIsAlpha(S[I]);
      Result := Result or ((ALen > 1) and
        (I = ALen) and (APossibleLastChar <> #0) and (S[I] = APossibleLastChar));
      if not Result then
        Break;
    end;
  end;
end;

end.
