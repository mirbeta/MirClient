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

unit dxSpellCheckerExceptionsFrame;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  dxCore, cxEdit, cxTextEdit, cxControls, cxContainer, cxListBox, Menus,
  cxLookAndFeelPainters, StdCtrls, cxButtons, dxSpellChecker, cxCheckBox,
  cxGraphics, cxLookAndFeels, dxSpellCheckerCore, dxLayoutcxEditAdapters, dxLayoutContainer, dxLayoutControlAdapters,
  cxClasses, dxLayoutControl, dxLayoutLookAndFeels;

type

  TfrmSpellCheckerCandidateEvent = procedure(var S: string; var AResult: Boolean) of object;

  TfrmSpellCheckerExceptions = class(TFrame)
    lbxList: TcxListBox;
    teCandidate: TcxTextEdit;
    btnAdd: TcxButton;
    btnDelete: TcxButton;
    cbAutoInclude: TcxCheckBox;
    lcMainGroup_Root: TdxLayoutGroup;
    lcMain: TdxLayoutControl;
    dxLayoutGroup1: TdxLayoutGroup;
    dxLayoutItem1: TdxLayoutItem;
    dxLayoutItem2: TdxLayoutItem;
    dxLayoutItem3: TdxLayoutItem;
    dxLayoutGroup2: TdxLayoutGroup;
    dxLayoutItem4: TdxLayoutItem;
    dxLayoutItem5: TdxLayoutItem;
    dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList;
    dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel;

    procedure btnAddClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure lbxListClick(Sender: TObject);
    procedure teCandidateKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure teCandidatePropertiesEditValueChanged(Sender: TObject);
  private
    FExceptions: TdxSpellCheckerAutoCorrectExceptions;

    FOnCandidateExist: TfrmSpellCheckerCandidateEvent;
    FOnCandidateValidate: TfrmSpellCheckerCandidateEvent;

    function DoCandidateExist(var S: string): Boolean;
    function DoCandidateValidate(var S: string): Boolean;
    function GetCandidateIndex: Integer;
    procedure Initialize;
    procedure Localize(ALocalizeIndex: Integer);
    procedure PopulateList;
    procedure SelectItem;
    procedure UpdateButtonsState;
  public
    constructor CreateEx(AOwner: TComponent; AExceptions: TdxSpellCheckerAutoCorrectExceptions;
      ALocalizeIndex: Integer); virtual;

    property Exceptions: TdxSpellCheckerAutoCorrectExceptions read FExceptions;
    property OnCandidateExist: TfrmSpellCheckerCandidateEvent read FOnCandidateExist write FOnCandidateExist;
    property OnCandidateValidate: TfrmSpellCheckerCandidateEvent read FOnCandidateValidate write FOnCandidateValidate;
  end;

implementation

uses
  dxSpellCheckerUtils, dxSpellCheckerStrs, StrUtils;

{$R *.dfm}

{ TfrmSpellCheckerExceptions }

constructor TfrmSpellCheckerExceptions.CreateEx(AOwner: TComponent; AExceptions: TdxSpellCheckerAutoCorrectExceptions;
  ALocalizeIndex: Integer);
begin
  inherited Create(AOwner);
  FExceptions := AExceptions;
  Initialize;
  Localize(ALocalizeIndex);
end;

function TfrmSpellCheckerExceptions.DoCandidateExist(var S: string): Boolean;
begin
  Result := GetCandidateIndex <> -1;
  if not Result and Assigned(FOnCandidateExist) then
    FOnCandidateExist(S, Result);
end;

function TfrmSpellCheckerExceptions.DoCandidateValidate(var S: string): Boolean;
begin
  Result := True;
  if Assigned(FOnCandidateValidate) then
    FOnCandidateValidate(S, Result);
end;

function TfrmSpellCheckerExceptions.GetCandidateIndex: Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to lbxList.Items.Count - 1 do
    if WideCompareText(teCandidate.Text, lbxList.Items[I]) = 0 then
    begin
      Result := I;
      Break;
    end;
end;

procedure TfrmSpellCheckerExceptions.Initialize;
begin
  PopulateList;
  teCandidate.Text := '';
  UpdateButtonsState;
  cbAutoInclude.Checked := FExceptions.AutoInclude;
end;

procedure TfrmSpellCheckerExceptions.Localize;
begin
  if ALocalizeIndex = 0 then
  begin
    btnAdd.Caption := cxGetResourceString(@sdxSpellCheckerAddButton);
    btnDelete.Caption := cxGetResourceString(@sdxSpellCheckerDeleteButton1);
    cbAutoInclude.Caption := cxGetResourceString(@sdxSpellCheckerAutoInclude);
  end
  else
  begin
    btnAdd.Caption := cxGetResourceString(@sdxSpellCheckerAddButton1);
    btnDelete.Caption := cxGetResourceString(@sdxSpellCheckerDeleteButton2);
    cbAutoInclude.Caption := cxGetResourceString(@sdxSpellCheckerAutoInclude1);
  end;
end;

procedure TfrmSpellCheckerExceptions.PopulateList;

  procedure Sort;
  var
    S: string;
    I, J: Integer;
  begin
    for I := 0 to lbxList.Count - 1 do
      for J := 0 to lbxList.Count - 2 - I do
        if WideCompareStr(lbxList.Items[J], lbxList.Items[J + 1]) > 0 then
        begin
          S := lbxList.Items[J];
          lbxList.Items[J] := lbxList.Items[J + 1];
          lbxList.Items[J + 1] := S;
        end;
  end;

begin
  lbxList.Items.BeginUpdate;
  try
    lbxList.Items.Clear;
    Exceptions.PopulateExceptions(lbxList.Items);
    Sort;
  finally
    lbxList.Items.EndUpdate;
  end;
  SelectItem;
end;

procedure TfrmSpellCheckerExceptions.SelectItem;
var
  AIndex: Integer;
begin
  AIndex := GetCandidateIndex;
  if AIndex <> -1 then
    lbxList.ItemIndex := AIndex
  else
    lbxList.ItemIndex := -1;
end;

procedure TfrmSpellCheckerExceptions.UpdateButtonsState;
var
  AAddButtonEnabled, ADeleteButtonEnabled: Boolean;
  ACandidateExist: Boolean;
  S: string;
begin
  S := teCandidate.Text;
  AAddButtonEnabled := (Length(teCandidate.Text) > 0) and DoCandidateValidate(S);
  ADeleteButtonEnabled := AAddButtonEnabled;
  ACandidateExist := DoCandidateExist(S);
  AAddButtonEnabled := AAddButtonEnabled and not ACandidateExist;
  ADeleteButtonEnabled := ADeleteButtonEnabled and ACandidateExist;
  btnAdd.Enabled := AAddButtonEnabled;
  btnDelete.Enabled := ADeleteButtonEnabled;
end;

procedure TfrmSpellCheckerExceptions.teCandidatePropertiesEditValueChanged(
  Sender: TObject);
begin
  UpdateButtonsState;
  SelectItem;
end;

procedure TfrmSpellCheckerExceptions.btnAddClick(Sender: TObject);
begin
  Exceptions.Add(teCandidate.Text);
  PopulateList;
  UpdateButtonsState;
end;

procedure TfrmSpellCheckerExceptions.btnDeleteClick(Sender: TObject);
var
  S: string;
begin
  S := teCandidate.Text;
  if DoCandidateExist(S) then
  begin
    Exceptions.Delete(S);
    PopulateList;
    UpdateButtonsState;
  end;
end;

procedure TfrmSpellCheckerExceptions.lbxListClick(Sender: TObject);
begin
  if lbxList.ItemIndex >= 0 then
    teCandidate.Text := lbxList.Items[lbxList.ItemIndex];
  UpdateButtonsState;
end;

procedure TfrmSpellCheckerExceptions.teCandidateKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
  begin
    if btnAdd.Enabled then
      btnAddClick(nil)
    else
      if btnDelete.Enabled then
        btnDeleteClick(nil);
  end;
end;

end.
