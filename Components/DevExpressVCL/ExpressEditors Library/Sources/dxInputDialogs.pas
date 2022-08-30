{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressEditors                                           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSEDITORS AND ALL                }
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

unit dxInputDialogs;

{$I cxVer.inc}

interface

uses
  Types, Windows, Classes, Controls, Dialogs, Forms, Variants, cxEditConsts;

type
  TdxInputQueryValidationProc = reference to procedure (ValueIndex: Integer; const Value: string; var IsValid: Boolean);

function dxInputBox(const ACaption, APrompt, ADefaultValue: string): string; overload;
function dxInputQuery(const ACaption, APrompt: string; var AValue: string): Boolean; overload;
function dxInputQuery(const ACaption, APrompt: string; var AValue: string; AValidationProc: TdxInputQueryValidationProc): Boolean; overload;
function dxInputQuery(const ACaption: string; const APrompts: array of string; var AValues: array of string): Boolean; overload;
function dxInputQuery(const ACaption: string; const APrompts: array of string;
  var AValues: array of string; AValidationProc: TdxInputQueryValidationProc): Boolean; overload;
function dxSelectQuery(const ACaption: string; const APrompt: string; AValues: TStrings; var AValue: string;
  AAllowCustomValues: Boolean = False; AValidationProc: TdxInputQueryValidationProc = nil): Boolean;
implementation

uses
  SysUtils, Generics.Collections, Generics.Defaults, dxForms,
  cxTextEdit, cxButtons, cxLabel, cxGeometry, dxCoreClasses, cxClasses, dxCore, cxGraphics, cxDropDownEdit, Math;

type

  { TdxCustomInputDialog }

  TdxCustomInputDialog = class(TdxForm)
  strict private
    FCancelButton: TcxButton;
    FOkButton: TcxButton;
  protected
    FValidationProc: TdxInputQueryValidationProc;

    function AreAllValuesValid: Boolean; virtual; abstract;
    procedure DoShow; override;
    procedure DoValidate; virtual;
    procedure PlaceButtons(var R: TRect);
    //
    procedure HandlerEditChanged(Sender: TObject);
  public
    procedure AfterConstruction; override;
  end;

  { TdxInputDialog }

  TdxInputDialog = class(TdxCustomInputDialog)
  strict private
    FListEditors: TObjectList<TcxTextEdit>;
    FListLabels: TObjectList<TcxLabel>;
  protected
    function AreAllValuesValid: Boolean; override;
    procedure DoShow; override;
  public
    destructor Destroy; override;
    procedure Initialize(AValueCount: Integer);
    procedure InitializeEditor(AIndex: Integer; const ACaption, AValue: string);
    function GetValue(AIndex: Integer): string;
    //
    property ListEditors: TObjectList<TcxTextEdit> read FListEditors;
    property ListLabels: TObjectList<TcxLabel> read FListLabels;
  end;

  { TdxSelectDialog }

  TdxSelectDialog = class(TdxCustomInputDialog)
  strict private
    FEditor: TcxComboBox;
    FPrompt: TcxLabel;
  protected
    function AreAllValuesValid: Boolean; override;
    procedure DoShow; override;
  public
    procedure Initialize(AValues: TStrings; const AValue: string; AAllowCustomValues: Boolean);
    //
    property Editor: TcxComboBox read FEditor;
    property Prompt: TcxLabel read FPrompt;
  end;

function CreateControl(AClass: TControlClass; AParent: TWinControl;
  const R: TRect; AAlign: TAlign = alNone; AAnchors: TAnchors = [akLeft, akTop]): TControl; overload;
begin
  Result := AClass.Create(AParent);
  Result.Parent := AParent;
  Result.BoundsRect := R;
  Result.Align := AAlign;
  Result.Anchors := AAnchors;
end;

procedure CreateControl(var Obj; AClass: TControlClass; AParent: TWinControl;
  const R: TRect; AAlign: TAlign = alNone; AAnchors: TAnchors = [akLeft, akTop]); overload;
begin
  TControl(Obj) := CreateControl(AClass, AParent, R, AAlign, AAnchors);
end;

function dxInputBox(const ACaption, APrompt, ADefaultValue: string): string;
begin
  Result := ADefaultValue;
  dxInputQuery(ACaption, APrompt, Result);
end;

function dxInputQuery(const ACaption, APrompt: string; var AValue: string): Boolean;
begin
  Result := dxInputQuery(ACaption, APrompt, AValue, nil);
end;

function dxInputQuery(const ACaption, APrompt: string;
  var AValue: string; AValidationProc: TdxInputQueryValidationProc): Boolean;
var
  AValues: array[0..0] of string;
begin
  AValues[0] := AValue;
  Result := dxInputQuery(ACaption, [APrompt], AValues, AvalidationProc);
  if Result then
    AValue := AValues[0];
end;

function dxInputQuery(const ACaption: string; const APrompts: array of string; var AValues: array of string): Boolean;
begin
  Result := dxInputQuery(ACaption, APrompts, AValues, nil);
end;

function dxInputQuery(const ACaption: string; const APrompts: array of string;
  var AValues: array of string; AValidationProc: TdxInputQueryValidationProc): Boolean;
var
  ADialog: TdxInputDialog;
  I: Integer;
begin
  ADialog := TdxInputDialog.CreateNew(nil);
  try
    ADialog.Caption := ACaption;
    ADialog.Initialize(Length(APrompts));
    for I := 0 to Length(APrompts) - 1 do
      ADialog.InitializeEditor(I, APrompts[I], AValues[I]);
    ADialog.FValidationProc := AValidationProc;

    Result := ADialog.ShowModal = mrOk;
    if Result then
    begin
      for I := 0 to Length(APrompts) - 1 do
        AValues[I] := ADialog.GetValue(I);
    end;
  finally
    ADialog.Free;
  end;
end;

function dxSelectQuery(const ACaption: string; const APrompt: string; AValues: TStrings; var AValue: string;
  AAllowCustomValues: Boolean = False; AValidationProc: TdxInputQueryValidationProc = nil): Boolean;
var
  ADialog: TdxSelectDialog;
begin
  ADialog := TdxSelectDialog.CreateNew(nil);
  try
    ADialog.Caption := ACaption;
    ADialog.Initialize(AValues, AValue, AAllowCustomValues);
    ADialog.Prompt.Caption := APrompt;
    ADialog.FValidationProc := AValidationProc;
    Result := ADialog.ShowModal = mrOk;
    if Result then
      AValue := ADialog.Editor.Text;
  finally
    ADialog.Free;
  end;
end;

{ TdxCustomInputDialog }

procedure TdxCustomInputDialog.AfterConstruction;
begin
  inherited;

  Position := poOwnerFormCenter;
  BorderStyle := bsDialog;
  DoubleBuffered := True;
  ClientWidth := 335;

  Padding.Left := 7;
  Padding.Top := 7;
  Padding.Right := 7;
  Padding.Bottom := 7;

  CreateControl(FOKButton, TcxButton, Self, cxNullRect);
  FOkButton.Caption := cxGetResourceString(@cxSEditButtonOK);
  FOkButton.ModalResult := mrOk;
  FOkButton.Default := True;

  CreateControl(FCancelButton, TcxButton, Self, cxNullRect);
  FCancelButton.Caption := cxGetResourceString(@cxSEditButtonCancel);
  FCancelButton.ModalResult := mrCancel;
  FCancelButton.Cancel := True;
end;

procedure TdxCustomInputDialog.DoShow;
begin
  DoValidate;
  inherited;
end;

procedure TdxCustomInputDialog.DoValidate;
begin
  FOkButton.Enabled := AreAllValuesValid;
end;

procedure TdxCustomInputDialog.PlaceButtons(var R: TRect);
const
  ButtonHeight = 25;
  ButtonWidth = 75;
begin
  R.Top := R.Bottom + ScaleFactor.Apply(8);
  R.Bottom := R.Top + ScaleFactor.Apply(ButtonHeight);

  FCancelButton.BoundsRect := cxRectSetRight(R, R.Right, ScaleFactor.Apply(ButtonWidth));
  R := FCancelButton.BoundsRect;
  FOkButton.BoundsRect := cxRectSetRight(R, R.Left - ScaleFactor.Apply(6), ScaleFactor.Apply(ButtonWidth));

  R.Top := R.Bottom + FOkButton.Margins.Bottom;
end;

procedure TdxCustomInputDialog.HandlerEditChanged(Sender: TObject);
begin
  DoValidate;
end;

{ TdxInputDialog }

destructor TdxInputDialog.Destroy;
begin
  FreeAndNil(FListEditors);
  FreeAndNil(FListLabels);
  inherited Destroy;
end;

procedure TdxInputDialog.Initialize(AValueCount: Integer);
var
  AEdit: TcxTextEdit;
  ALabel: TcxLabel;
  I: Integer;
begin
  FListEditors := TObjectList<TcxTextEdit>.Create;
  FListLabels := TObjectList<TcxLabel>.Create;

  for I := 0 to AValueCount - 1 do
  begin
    CreateControl(ALabel, TcxLabel, Self, Rect(0, MaxWord, 0, 0), alTop);
    ALabel.AutoSize := True;
    ALabel.AlignWithMargins := True;
    ALabel.Style.TransparentBorder := False;
    ALabel.Properties.ShowEndEllipsis := True;
    ALabel.Properties.WordWrap := True;
    ALabel.Transparent := True;
    ALabel.Margins.Bottom := 0;
    FListLabels.Add(ALabel);

    CreateControl(AEdit, TcxTextEdit, Self, Rect(0, MaxWord, 0, 0), alTop);
    AEdit.Style.TransparentBorder := False;
    AEdit.AlignWithMargins := True;
    AEdit.Tag := I;
    AEdit.Properties.OnChange := HandlerEditChanged;
    FListEditors.Add(AEdit);
  end;

  ActiveControl := FListEditors[0];
end;

procedure TdxInputDialog.InitializeEditor(AIndex: Integer; const ACaption, AValue: string);
begin
  ListEditors[AIndex].EditValue := AValue;
  ListLabels[AIndex].Caption := ACaption;
end;

function TdxInputDialog.GetValue(AIndex: Integer): string;
begin
  Result := ListEditors[AIndex].EditValue;
end;

procedure TdxInputDialog.DoShow;
var
  R: TRect;
begin
  inherited DoShow;

  R := ListEditors.Last.BoundsRect;
  PlaceButtons(R);
  ClientHeight := R.Top + Padding.Bottom;
end;

function TdxInputDialog.AreAllValuesValid: Boolean;
var
  I: Integer;
begin
  Result := True;
  if Assigned(FValidationProc) then
  begin
    for I := 0 to FListEditors.Count - 1 do
    begin
      FValidationProc(I, ListEditors[I].EditingValue, Result);
      if not Result then
        Break;
    end;
  end;
end;

{ TdxSelectDialog }

procedure TdxSelectDialog.Initialize(AValues: TStrings; const AValue: string; AAllowCustomValues: Boolean);
const
  Map: array[Boolean] of TcxEditDropDownListStyle = (lsFixedList, lsEditList);
begin
  CreateControl(FPrompt, TcxLabel, Self, Rect(0, MaxWord, 0, 0), alTop);
  Prompt.AutoSize := True;
  Prompt.AlignWithMargins := True;
  Prompt.Style.TransparentBorder := False;
  Prompt.Properties.ShowEndEllipsis := True;
  Prompt.Properties.WordWrap := True;
  Prompt.Transparent := True;
  Prompt.Margins.Bottom := 0;

  CreateControl(FEditor, TcxComboBox, Self, Rect(0, MaxWord, 0, 0), alTop);
  Editor.Style.TransparentBorder := False;
  Editor.AlignWithMargins := True;
  Editor.Properties.Items.Assign(AValues);
  Editor.Properties.DropDownListStyle := Map[AAllowCustomValues];
  Editor.ItemIndex := Max(AValues.IndexOf(AValue), IfThen(AAllowCustomValues, -1));
  Editor.Properties.OnChange := HandlerEditChanged;
end;

function TdxSelectDialog.AreAllValuesValid: Boolean;
begin
  if Editor.Properties.DropDownListStyle = lsFixedList then
    Result := Editor.ItemIndex >= 0
  else
  begin
    Result := True;
    if Assigned(FValidationProc) then
      FValidationProc(0, Editor.Text, Result);
  end;
end;

procedure TdxSelectDialog.DoShow;
var
  R: TRect;
begin
  inherited DoShow;

  R := Editor.BoundsRect;
  PlaceButtons(R);
  ClientHeight := R.Top + Padding.Bottom;
end;

end.
