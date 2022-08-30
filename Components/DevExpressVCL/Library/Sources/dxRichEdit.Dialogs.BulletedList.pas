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

unit dxRichEdit.Dialogs.BulletedList;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
{$IFDEF DELPHIXE2}
  System.UITypes,
{$ENDIF}
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, cxGraphics, cxControls, cxLookAndFeels, Generics.Defaults, Generics.Collections,
  cxLookAndFeelPainters, cxContainer, cxEdit, dxLayoutcxEditAdapters, dxLayoutControlAdapters, dxLayoutContainer,
  Menus, cxListBox, StdCtrls, cxButtons, cxLabel, dxLayoutControl, cxTextEdit, cxMaskEdit, cxSpinEdit, dxMeasurementUnitEdit,
  dxRichEdit.Dialogs.CustomDialog, dxRichEdit.Dialogs.CustomNumberingListForm, dxRichEdit.Dialogs.NumberingHelper,
  dxRichEdit.Dialogs.NumberingFormController, dxRichEdit.Utils.Properties, dxSymbolListBox, dxLayoutLookAndFeels,
  cxClasses;

type
  TdxBulletCharacterControlHelper = class;

  TdxRichEditBulletedListDialogForm = class(TdxRichEditCustomNumberingListForm, IdxRichEditBulletedListForm)
    btnCharacter: TcxButton;
    dxLayoutControl1Group1: TdxLayoutAutoCreatedGroup;
    dxLayoutControl1Group2: TdxLayoutAutoCreatedGroup;
    dxLayoutControl1Group3: TdxLayoutGroup;
    dxLayoutControl1Item1: TdxLayoutItem;
    dxLayoutControl1Item10: TdxLayoutItem;
    dxLayoutControl1Item11: TdxLayoutItem;
    dxLayoutControl1Item2: TdxLayoutItem;
    dxLayoutControl1Item4: TdxLayoutItem;
    dxLayoutControl1Item5: TdxLayoutItem;
    dxLayoutControl1Item6: TdxLayoutItem;
    dxLayoutControl1Item7: TdxLayoutItem;
    dxLayoutControl1Item8: TdxLayoutItem;
    dxLayoutControl1Item9: TdxLayoutItem;
    lblBulletCharacter: TdxLayoutSeparatorItem;
    lblBulletPosition: TdxLayoutSeparatorItem;
    lblTextPosition: TdxLayoutSeparatorItem;
    lbSimpleSymbol1: TdxSimpleSymbolListBox;
    lbSimpleSymbol2: TdxSimpleSymbolListBox;
    lbSimpleSymbol3: TdxSimpleSymbolListBox;
    lbSimpleSymbol4: TdxSimpleSymbolListBox;
    lbSimpleSymbol5: TdxSimpleSymbolListBox;
    lbSimpleSymbol6: TdxSimpleSymbolListBox;
    lciAlignedAt: TdxLayoutItem;
    lciIndentAt: TdxLayoutItem;
    lcMainGroup_Root: TdxLayoutGroup;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure btnCharacterClick(Sender: TObject);
  private
    FBulletedListFormHelper: TdxBulletedListFormHelper;
    procedure BulletCharacterControlSelectedIndexChanged(Sender: TObject);
    procedure MouseDoubleClick(Sender: TObject);
    function GetActiveSymbolProperties: TdxSymbolProperties;
    function GetSelectedIndex: Integer;
    procedure SetActiveSymbolProperties(const Value: TdxSymbolProperties);
    procedure SetSelectedIndex(const Value: Integer);
  protected
    FBulletCharacterControlHelper: TdxBulletCharacterControlHelper;
    procedure ApplyLocalization; override;
    procedure ApplySymbolProperties(const ASymbolProperties: TdxSymbolProperties; AData: TObject);
    procedure CreateListBoxes;
    procedure InitializeForm; override;
    procedure InitializeComponents(const ASymbolsProperties: array of TdxSymbolProperties);
    procedure SubscribeControlsEvents; override;
    procedure UnsubscribeControlsEvents; override;

    property SelectedIndex: Integer read GetSelectedIndex write SetSelectedIndex;
    property ActiveSymbolProperties: TdxSymbolProperties read GetActiveSymbolProperties write SetActiveSymbolProperties;
  public
    destructor Destroy; override;
  end;

  { TdxBulletCharacterControlHelper }

  TdxBulletCharacterControlHelper = class
  private
    FOwner: TdxRichEditBulletedListDialogForm;
    FSymbolListBoxes: TList<TdxSimpleSymbolListBox>;
    FSelectedIndex: Integer;
    FOnMouseDoubleClick: TNotifyEvent;
    FOnSelectedIndexChanged: TNotifyEvent;
    function GetActiveSymbolListBox: TdxSimpleSymbolListBox;
    function GetActiveSymbolProperties: TdxSymbolProperties;
    function GetFocusedSymbolListBox: TdxSimpleSymbolListBox;
    procedure SetActiveSymbolProperties(const Value: TdxSymbolProperties);
    procedure SetSelectedIndex(const Value: Integer);
    procedure SetOnMouseDoubleClick(const Value: TNotifyEvent);
  protected
    procedure CreateListBoxes(const ASimpleSymbolListBox: array of TdxSimpleSymbolListBox);
    procedure DoSelectedIndexChanged;
    function GetNextActiveListBoxIndex(AKeyDown: Word; AActiveListBoxIndex: Integer): Integer;
    procedure InitializeComponents(const ASymbolsProperties: array of TdxSymbolProperties);
    procedure SimpleSymbolListBoxKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure SimpleSymbolListBoxMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure SimpleSymbolListBoxMouseDoubleClick(Sender: TObject);
  public
    constructor Create(AOwner: TdxRichEditBulletedListDialogForm);
    destructor Destroy; override;
    procedure ExchangeSelectedListBox(ANewListBox: TdxSimpleSymbolListBox);
    procedure Focus;
    procedure SubscribeControlsEvents;

    property SelectedIndex: Integer read FSelectedIndex write SetSelectedIndex;
    property ActiveSymbolProperties: TdxSymbolProperties read GetActiveSymbolProperties write SetActiveSymbolProperties;
    property OnSelectedIndexChanged: TNotifyEvent read FOnSelectedIndexChanged write FOnSelectedIndexChanged;
    property OnMouseDoubleClick: TNotifyEvent read FOnMouseDoubleClick write SetOnMouseDoubleClick;
  end;

implementation

uses
  dxCore, dxRichEdit.Dialogs.Strs,
  dxRichEdit.Options.Core,
  dxRichEdit.DocumentModel.CharacterFormatting,
  dxRichEdit.DocumentModel.NumberingFormatting,
  dxRichEdit.DocumentModel.Numbering;

{$R *.dfm}

{ TdxRichEditBulletedListDialogForm }

destructor TdxRichEditBulletedListDialogForm.Destroy;
begin
  FBulletedListFormHelper.Free;
  FBulletCharacterControlHelper.Free;
  inherited Destroy;
end;

procedure TdxRichEditBulletedListDialogForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

  function GetNumber(AKey: Word): Integer;
  const
    VK_1 = Ord('1');
    VK_2 = Ord('2');
    VK_3 = Ord('3');
    VK_4 = Ord('4');
    VK_5 = Ord('5');
    VK_6 = Ord('6');
  begin
    case AKey of
      VK_1, VK_NUMPAD1:
        Result := 1;
      VK_2, VK_NUMPAD2:
        Result := 2;
      VK_3, VK_NUMPAD3:
        Result := 3;
      VK_4, VK_NUMPAD4:
        Result := 4;
      VK_5, VK_NUMPAD5:
        Result := 5;
      VK_6, VK_NUMPAD6:
        Result := 6;
      else
        Result := 0;
    end;
  end;

begin
  if (ActiveControl is TCustomEdit) or (ActiveControl is TcxCustomEdit) then
    Exit;
  SelectedIndex := GetNumber(Key) - 1;
end;

procedure TdxRichEditBulletedListDialogForm.btnCharacterClick(Sender: TObject);
var
  ASymbolProperties: TdxSymbolProperties;
begin
  ASymbolProperties := FBulletCharacterControlHelper.GetActiveSymbolProperties;
  Control.ShowSymbolForm(ASymbolProperties, ApplySymbolProperties, nil);
end;

procedure TdxRichEditBulletedListDialogForm.BulletCharacterControlSelectedIndexChanged(Sender: TObject);
var
  ASymbolProperties: TdxSymbolProperties;
begin
  ASymbolProperties := FBulletCharacterControlHelper.ActiveSymbolProperties;
  Controller.DisplayFormat := ASymbolProperties.UnicodeChar;
  Controller.CharacterProperties.FontName := ASymbolProperties.FontName;
end;

procedure TdxRichEditBulletedListDialogForm.MouseDoubleClick(Sender: TObject);
begin
  ApplyChanges;
end;

function TdxRichEditBulletedListDialogForm.GetActiveSymbolProperties: TdxSymbolProperties;
begin
  Result := FBulletCharacterControlHelper.ActiveSymbolProperties
end;

function TdxRichEditBulletedListDialogForm.GetSelectedIndex: Integer;
begin
  Result := FBulletCharacterControlHelper.SelectedIndex;
end;

procedure TdxRichEditBulletedListDialogForm.SetActiveSymbolProperties(const Value: TdxSymbolProperties);
begin
  FBulletCharacterControlHelper.ActiveSymbolProperties := Value;
end;

procedure TdxRichEditBulletedListDialogForm.SetSelectedIndex(const Value: Integer);
begin
  FBulletCharacterControlHelper.SelectedIndex := Value;
end;

procedure TdxRichEditBulletedListDialogForm.ApplyLocalization;
begin
  inherited ApplyLocalization;
  Caption := cxGetResourceString(@sdxRichEditBulletedListDialogForm);
  lblBulletCharacter.Caption := cxGetResourceString(@sdxRichEditBulletedListDialogBulletCharacter);
  btnCharacter.Caption := cxGetResourceString(@sdxRichEditBulletedListDialogButtonCharacter);
  lblBulletPosition.Caption := cxGetResourceString(@sdxRichEditBulletedListDialogBulletPosition);
  lblTextPosition.Caption := cxGetResourceString(@sdxRichEditBulletedListDialogTextPosition);
  lciAlignedAt.CaptionOptions.Text := cxGetResourceString(@sdxRichEditBulletedListDialogAlignedAt);
  lciIndentAt.CaptionOptions.Text := cxGetResourceString(@sdxRichEditBulletedListDialogIndentAt);
end;

procedure TdxRichEditBulletedListDialogForm.ApplySymbolProperties(const ASymbolProperties: TdxSymbolProperties;
  AData: TObject);
var
  AIndex: Integer;
begin
  AIndex := FBulletCharacterControlHelper.SelectedIndex;
  TdxBulletedListFormHelper.SymbolsProperties[AIndex] := ASymbolProperties;
  Controller.DisplayFormat := ASymbolProperties.UnicodeChar;
  Controller.CharacterProperties.FontName := ASymbolProperties.FontName;
  FBulletCharacterControlHelper.SetActiveSymbolProperties(ASymbolProperties);
  FBulletCharacterControlHelper.Focus;
end;

procedure TdxRichEditBulletedListDialogForm.CreateListBoxes;
begin
  FBulletCharacterControlHelper.CreateListBoxes([lbSimpleSymbol1, lbSimpleSymbol2, lbSimpleSymbol3, lbSimpleSymbol4,
    lbSimpleSymbol5, lbSimpleSymbol6]);
end;

procedure TdxRichEditBulletedListDialogForm.InitializeForm;
begin
  inherited InitializeForm;
  FBulletCharacterControlHelper := TdxBulletCharacterControlHelper.Create(Self);
  CreateListBoxes;
  FBulletCharacterControlHelper.SubscribeControlsEvents;
  FBulletedListFormHelper := TdxBulletedListFormHelper.Create(Self);
  FBulletedListFormHelper.CreateBulletCharacters;
end;

procedure TdxRichEditBulletedListDialogForm.InitializeComponents(
  const ASymbolsProperties: array of TdxSymbolProperties);
begin
  FBulletCharacterControlHelper.InitializeComponents(ASymbolsProperties);
end;

procedure TdxRichEditBulletedListDialogForm.SubscribeControlsEvents;
begin
  inherited SubscribeControlsEvents;
  FBulletCharacterControlHelper.OnSelectedIndexChanged := BulletCharacterControlSelectedIndexChanged;
  FBulletCharacterControlHelper.OnMouseDoubleClick := MouseDoubleClick;
end;

procedure TdxRichEditBulletedListDialogForm.UnsubscribeControlsEvents;
begin
  inherited;
  FBulletCharacterControlHelper.OnSelectedIndexChanged := nil;
  FBulletCharacterControlHelper.OnMouseDoubleClick := nil;
end;

{ TdxBulletCharacterControlHelper }

function TdxBulletCharacterControlHelper.GetActiveSymbolListBox: TdxSimpleSymbolListBox;
begin
  for Result in FSymbolListBoxes do
    if Result.IsActive then
      Exit;
  Result := FSymbolListBoxes[0];
end;

function TdxBulletCharacterControlHelper.GetActiveSymbolProperties: TdxSymbolProperties;
var
  AActiveListBox: TdxSimpleSymbolListBox;
begin
  AActiveListBox := GetActiveSymbolListBox();
  Result := TdxSymbolProperties.Create(AActiveListBox.UnicodeChars[0], AActiveListBox.FontName);
end;

function TdxBulletCharacterControlHelper.GetFocusedSymbolListBox: TdxSimpleSymbolListBox;
begin
  for Result in FSymbolListBoxes do
    if Result.Focused then
      Exit;
  Result := FSymbolListBoxes[0];
end;

function TdxBulletCharacterControlHelper.GetNextActiveListBoxIndex(AKeyDown: Word;
  AActiveListBoxIndex: Integer): Integer;
begin
  if (AKeyDown = VK_LEFT) or (AKeyDown = VK_UP) then
    if (AActiveListBoxIndex = 0) then
      Result := FSymbolListBoxes.Count - 1
    else
      Result := AActiveListBoxIndex - 1
  else
    if (AKeyDown = VK_RIGHT) or (AKeyDown = VK_DOWN) then
      if AActiveListBoxIndex = FSymbolListBoxes.Count - 1 then
        Result := 0
      else
        Result := AActiveListBoxIndex + 1
  else
    Result := -1;
end;

procedure TdxBulletCharacterControlHelper.InitializeComponents(
  const ASymbolsProperties: array of TdxSymbolProperties);
var
  I: Integer;
begin
  for I := Low(ASymbolsProperties) to High(ASymbolsProperties) do
  begin
    FSymbolListBoxes[I].FontName := ASymbolsProperties[I].FontName;
    FSymbolListBoxes[I].BeginUpdate;
    FSymbolListBoxes[I].ClearChars;
    FSymbolListBoxes[I].AddChar(ASymbolsProperties[I].UnicodeChar);
    FSymbolListBoxes[I].EndUpdate;
  end;
end;

constructor TdxBulletCharacterControlHelper.Create(AOwner: TdxRichEditBulletedListDialogForm);
begin
  inherited Create;
  FOwner := AOwner;
  FSymbolListBoxes := TList<TdxSimpleSymbolListBox>.Create;
end;

procedure TdxBulletCharacterControlHelper.CreateListBoxes(
  const ASimpleSymbolListBox: array of TdxSimpleSymbolListBox);
var
  I: Integer;
begin
  if Length(ASimpleSymbolListBox) = 0 then
    Exit;
  ASimpleSymbolListBox[0].IsActive := True;
  for I := Low(ASimpleSymbolListBox) to High(ASimpleSymbolListBox) do
  begin
    FSymbolListBoxes.Add(ASimpleSymbolListBox[I]);
    if I > 0 then
      ASimpleSymbolListBox[I].TabStop := False;
  end;
end;

destructor TdxBulletCharacterControlHelper.Destroy;
begin
  FSymbolListBoxes.Free;
  inherited Destroy;
end;

procedure TdxBulletCharacterControlHelper.DoSelectedIndexChanged;
begin
  if Assigned(FOnSelectedIndexChanged) then
    OnSelectedIndexChanged(Self);
end;

procedure TdxBulletCharacterControlHelper.ExchangeSelectedListBox(
  ANewListBox: TdxSimpleSymbolListBox);
var
  AActiveListBox: TdxSimpleSymbolListBox;
begin
  AActiveListBox := GetActiveSymbolListBox;
  AActiveListBox.TabStop := False;
  AActiveListBox.IsActive := False;
  AActiveListBox.Refresh;
  ANewListBox.TabStop := True;
  ANewListBox.IsActive := True;
  if FOwner.Visible then
    ANewListBox.SetFocus
  else
    FOwner.ActiveControl := ANewListBox;
  ANewListBox.Refresh;
  FSelectedIndex := FSymbolListBoxes.IndexOf(ANewListBox);
  DoSelectedIndexChanged;
end;

procedure TdxBulletCharacterControlHelper.Focus;
var
  AControl: TWinControl;
begin
  AControl := GetActiveSymbolListBox;
  if Assigned(AControl) then
    AControl.SetFocus;
end;

procedure TdxBulletCharacterControlHelper.SetActiveSymbolProperties(
  const Value: TdxSymbolProperties);
var
  AActiveListBox: TdxSimpleSymbolListBox;
begin
  AActiveListBox := GetActiveSymbolListBox;
  AActiveListBox.BeginUpdate;
  AActiveListBox.ClearChars;
  AActiveListBox.AddChar(Value.UnicodeChar);
  AActiveListBox.EndUpdate;
  AActiveListBox.FontName := Value.FontName;
end;

procedure TdxBulletCharacterControlHelper.SetOnMouseDoubleClick(
  const Value: TNotifyEvent);
var
  ASimpleSymbol: TdxSimpleSymbolListBox;
begin
  FOnMouseDoubleClick := Value;
  for ASimpleSymbol in FSymbolListBoxes do
    ASimpleSymbol.OnDblClick := Value;
end;

procedure TdxBulletCharacterControlHelper.SetSelectedIndex(const Value: Integer);
begin
  if (Value < 0) or (Value >= FSymbolListBoxes.Count) then
    Exit;
  FSelectedIndex := Value;
  ExchangeSelectedListBox(FSymbolListBoxes[FSelectedIndex]);
end;

procedure TdxBulletCharacterControlHelper.SimpleSymbolListBoxKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
var
  AActiveListBox: TdxSimpleSymbolListBox;
  AActiveListBoxIndex: Integer;
  ANewListBoxIndex: Integer;
  ANewListBox: TdxSimpleSymbolListBox;
begin
  if not (Key in [VK_LEFT, VK_RIGHT, VK_UP, VK_DOWN]) then
    Exit;
  AActiveListBox := GetActiveSymbolListBox;
  AActiveListBoxIndex := FSymbolListBoxes.IndexOf(AActiveListBox);
  ANewListBoxIndex := GetNextActiveListBoxIndex(Key, AActiveListBoxIndex);
  ANewListBox := FSymbolListBoxes[ANewListBoxIndex];
  ExchangeSelectedListBox(ANewListBox);
  Key := 0;
end;

procedure TdxBulletCharacterControlHelper.SimpleSymbolListBoxMouseDoubleClick(
  Sender: TObject);
begin
  if Assigned(FOnMouseDoubleClick) then
    FOnMouseDoubleClick(Sender);
end;

procedure TdxBulletCharacterControlHelper.SimpleSymbolListBoxMouseDown(
  Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ANewListBox: TdxSimpleSymbolListBox;
begin
  ANewListBox := GetFocusedSymbolListBox;
  ExchangeSelectedListBox(ANewListBox);
end;

procedure TdxBulletCharacterControlHelper.SubscribeControlsEvents;
var
  AListBox: TdxSimpleSymbolListBox;
begin
  for AListBox in FSymbolListBoxes do
  begin
    AListBox.OnKeyDown := SimpleSymbolListBoxKeyDown;
    AListBox.OnMouseDown := SimpleSymbolListBoxMouseDown;
    AListBox.OnDblClick := SimpleSymbolListBoxMouseDoubleClick;
  end;
end;

end.
