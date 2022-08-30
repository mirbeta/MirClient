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

unit dxRichEdit.Dialogs.CustomNumberingList;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
{$IFDEF DELPHIXE2}
  System.UITypes,
{$ENDIF}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, Menus,
  Generics.Defaults, Generics.Collections, dxCore, dxRichEdit.Dialogs.CustomDialog, cxGraphics, cxControls, cxLookAndFeels,
  cxLookAndFeelPainters, dxLayoutControlAdapters, dxLayoutContainer, StdCtrls, cxButtons, dxLayoutControl,
  dxLayoutcxEditAdapters, cxContainer, cxEdit, cxLabel,
  dxLayoutLookAndFeels, cxClasses, cxTextEdit, cxMaskEdit, cxDropDownEdit, cxSpinEdit, dxMeasurementUnitEdit,
  dxRichEditDialogsSimpleControl,
  dxRichEdit.Control,
  dxGenerics,
  dxRichEdit.Utils.Types,
  dxRichEdit.Dialogs.CustomNumberingListForm,
  dxRichEdit.DocumentModel.Section,
  dxRichEdit.DocumentModel.Numbering,
  dxRichEdit.DocumentModel.NumberingFormatting,
  dxRichEdit.Dialogs.NumberingHelper,
  dxRichEdit.Dialogs.NumberingFormController,
  dxRichEdit.Platform.Win.Control,
  dxRichEdit.Control.Core;

type
  TdxRichEditCustomSimpleNumberingListForm = class(TdxRichEditCustomNumberingListForm)
    cmbDisplayFormat: TcxComboBox;
    cmbNumberingAlignment: TcxComboBox;
    dxLayoutControl1Group2: TdxLayoutGroup;
    dxLayoutControl1Group3: TdxLayoutGroup;
    dxLayoutControl1Group4: TdxLayoutGroup;
    dxLayoutControl1Group5: TdxLayoutAutoCreatedGroup;
    dxLayoutControl1Group7: TdxLayoutAutoCreatedGroup;
    dxLayoutControl1Item1: TdxLayoutItem;
    dxLayoutControl1Item10: TdxLayoutItem;
    dxLayoutControl1Item2: TdxLayoutItem;
    lciDisplayFormat: TdxLayoutItem;
    dxLayoutControl1Item7: TdxLayoutItem;
    dxLayoutControl1Item8: TdxLayoutItem;
    dxLayoutControl1Item9: TdxLayoutItem;
    edtNumberFormat: TdxSimpleRichEditControl;
    edStart: TcxSpinEdit;
    lblNumberStyle: TcxLabel;
    lciAlignedAt: TdxLayoutItem;
    lciIndentAt: TdxLayoutItem;
    lciStartAt: TdxLayoutItem;
    lcMainGroup_Root: TdxLayoutGroup;
    lblNumberFormat: TdxLayoutSeparatorItem;
    lblNumberPosition: TdxLayoutSeparatorItem;
    lblTextPosition: TdxLayoutSeparatorItem;
  private
    FIsCloseForm: Boolean;
    FDisplayFormatHelper: TdxDisplayFormatHelper;
    function GetController: TdxSimpleNumberingListController;
    function GetDisplayFormat: TdxNumberingFormat; inline;
    function GetDisplayFormatString(AFormat: TdxNumberingFormat): string;
    function GetNumberingAlignment: TdxListNumberAlignment; inline;
    procedure SetNumberingAlignment(const Value: TdxListNumberAlignment); inline;
    procedure SetDisplayFormat(const Value: TdxNumberingFormat); inline;

    procedure PopulateAlignment;
    procedure PopulateDisplayFormatItems;
    procedure UpdateMinAndMaxValueStart(AMinValue, AMaxValue: Int64);
  protected
    procedure ApplyLocalization; override;

    procedure AlignmentValueChanged(Sender: TObject);
    procedure ChangeFocus(AForward: Boolean = True); virtual;
    procedure DisplayFormatChanged(Sender: TObject);
    procedure FormatValueChanged(Sender: TObject);
    function GetOrdinalNumberConverter(AFormat: TdxNumberingFormat): TdxOrdinalBasedNumberConverter; virtual;
    procedure InitializeForm; override;
    procedure NumberFormatKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure StartTextChanged(Sender: TObject);
    procedure StartValueChanged(Sender: TObject);
    procedure SubscribeControlsEvents; override;
    procedure UnsubscribeControlsEvents; override;
    procedure UpdateFormCore; override;
    procedure UpdateNumberFormat;

    property DisplayFormat: TdxNumberingFormat read GetDisplayFormat write SetDisplayFormat;
    property NumberingAlignment: TdxListNumberAlignment read GetNumberingAlignment write SetNumberingAlignment;
  public
    destructor Destroy; override;
    procedure ApplyChanges; override;
    property Controller: TdxSimpleNumberingListController read GetController;
  end;

implementation

uses
  Math,
  dxRichEdit.NativeApi,
  dxRichEdit.Options.Core,
  dxRichEdit.Options,
  dxRichEdit.Types,
  dxRichEdit.Dialogs.Strs,
  dxRichEdit.Dialogs.Utils;

{$R *.dfm}

{ TdxRichEditCustomNumberingList }

destructor TdxRichEditCustomSimpleNumberingListForm.Destroy;
begin
  FDisplayFormatHelper.Free;
  inherited Destroy;
end;

procedure TdxRichEditCustomSimpleNumberingListForm.ApplyChanges;
begin
  if FIsCloseForm then
    inherited ApplyChanges;
end;

function TdxRichEditCustomSimpleNumberingListForm.GetController: TdxSimpleNumberingListController;
begin
  Result := TdxSimpleNumberingListController(inherited Controller);
end;

function TdxRichEditCustomSimpleNumberingListForm.GetDisplayFormat: TdxNumberingFormat;
var
  AValue: Integer;
begin
  if TryGetItemValue(cmbDisplayFormat, AValue) then
    Result := TdxNumberingFormat(AValue)
  else
    Result := TdxNumberingFormat.Decimal;
end;

function TdxRichEditCustomSimpleNumberingListForm.GetDisplayFormatString(AFormat: TdxNumberingFormat): string;
var
  AConverter: TdxOrdinalBasedNumberConverter;
begin
  AConverter := TdxOrdinalBasedNumberConverter.CreateConverter(AFormat, TdxLanguageId.English);
  try
    Result := Format('%s,%s,%s...', [AConverter.ConvertNumber(1), AConverter.ConvertNumber(2), AConverter.ConvertNumber(3)]);
  finally
    AConverter.Free;
  end;
end;

function TdxRichEditCustomSimpleNumberingListForm.GetNumberingAlignment: TdxListNumberAlignment;
var
  AValue: Integer;
begin
  if TryGetItemValue(cmbNumberingAlignment, AValue) then
    Result := TdxListNumberAlignment(AValue)
  else
    Result := TdxListNumberAlignment.Left;
end;

procedure TdxRichEditCustomSimpleNumberingListForm.SetNumberingAlignment(const Value: TdxListNumberAlignment);
begin
  UpdateSelectedIndex(cmbNumberingAlignment, Ord(Value));
end;

procedure TdxRichEditCustomSimpleNumberingListForm.SetDisplayFormat(const Value: TdxNumberingFormat);
begin
  UpdateSelectedIndex(cmbDisplayFormat, Ord(Value));
end;

procedure TdxRichEditCustomSimpleNumberingListForm.PopulateAlignment;
begin
  Populate(cmbNumberingAlignment, procedure(ACombobox: TcxCustomComboBox)
    var
      AAlignment: TdxListNumberAlignment;
    begin
      for AAlignment := Low(dxListNumberAlignmentNames) to High(dxListNumberAlignmentNames) do
        AddItemValue(ACombobox.Properties.Items, cxGetResourceString(dxListNumberAlignmentNames[AAlignment]),
          Ord(AAlignment));
    end);
end;

procedure TdxRichEditCustomSimpleNumberingListForm.PopulateDisplayFormatItems;
begin
  Populate(cmbDisplayFormat, procedure(ACombobox: TcxCustomComboBox)
    var
      AValues: TdxOrdinalList<TdxNumberingFormat>;
      AFormat: TdxNumberingFormat;
      I: Integer;
    begin
      AValues := TdxOrdinalBasedNumberConverter.GetSupportNumberingFormat;
      try
        for I := 0 to AValues.Count - 1 do
        begin
          AFormat := AValues[I];
          AddItemValue(ACombobox.Properties.Items, GetDisplayFormatString(AFormat), Ord(AFormat));
        end;
      finally
        AValues.Free;
      end;
    end);
end;

procedure TdxRichEditCustomSimpleNumberingListForm.UpdateMinAndMaxValueStart(AMinValue, AMaxValue: Int64);
begin
  edStart.Properties.MinValue := AMinValue;
  edStart.Properties.MaxValue := AMaxValue;
end;

procedure TdxRichEditCustomSimpleNumberingListForm.ApplyLocalization;
begin
  inherited ApplyLocalization;
  lblNumberStyle.Caption := cxGetResourceString(@sdxRichEditCustomNumberingListNumberStyle);
  lblNumberPosition.Caption := cxGetResourceString(@sdxRichEditCustomNumberingListNumberPosition);
  lblTextPosition.Caption := cxGetResourceString(@sdxRichEditCustomNumberingListTextPosition);
  lciStartAt.CaptionOptions.Text := cxGetResourceString(@sdxRichEditCustomNumberingListStartAt);
  lciAlignedAt.CaptionOptions.Text := cxGetResourceString(@sdxRichEditCustomNumberingListAlignedAt);
  lciIndentAt.CaptionOptions.Text := cxGetResourceString(@sdxRichEditCustomNumberingListIndentAt);

  PopulateAlignment;
  PopulateDisplayFormatItems;
end;

procedure TdxRichEditCustomSimpleNumberingListForm.AlignmentValueChanged(Sender: TObject);
begin
  Controller.Alignment := NumberingAlignment;
end;

procedure TdxRichEditCustomSimpleNumberingListForm.ChangeFocus(AForward: Boolean = True);
begin
  if AForward then
    btnFont.SetFocus
  else
    btnCancel.SetFocus
end;

procedure TdxRichEditCustomSimpleNumberingListForm.DisplayFormatChanged(Sender: TObject);
begin
  Controller.DisplayFormat := FDisplayFormatHelper.GetDisplayFormatString;
end;

procedure TdxRichEditCustomSimpleNumberingListForm.FormatValueChanged(Sender: TObject);
var
  AConverter: TdxOrdinalBasedNumberConverter;
  AMinValue, AMaxValue: Int64;
  AStart: Integer;
begin
  Controller.Format := DisplayFormat;
  AConverter := GetOrdinalNumberConverter(Controller.Format);
  try
    AMinValue := Max(MinInt, AConverter.MinValue);
    AMaxValue := Min(MaxInt, AConverter.MaxValue);
    AStart := edStart.Value;
    UpdateMinAndMaxValueStart(AMinValue, AMaxValue);
    edStart.Value := AStart;
    FIsCloseForm := edStart.ValidateEdit(False);
    UpdateNumberFormat;
  finally
    AConverter.Free;
  end;
end;

function TdxRichEditCustomSimpleNumberingListForm.GetOrdinalNumberConverter(
  AFormat: TdxNumberingFormat): TdxOrdinalBasedNumberConverter;
begin
  Result := TdxOrdinalBasedNumberConverter.CreateConverter(AFormat, TdxLanguageId.English);
end;

procedure TdxRichEditCustomSimpleNumberingListForm.InitializeForm;
begin
  inherited InitializeForm;
  FIsCloseForm := True;
  FDisplayFormatHelper := TdxDisplayFormatHelper.Create(edtNumberFormat, Controller.EditedLevels, Controller.EditedLevelIndex);
  edtNumberFormat.Options.Behavior.Drag := TdxDocumentCapability.Disabled;
  edtNumberFormat.Options.Behavior.Drop := TdxDocumentCapability.Disabled;
  edtNumberFormat.Options.Behavior.ShowPopupMenu := TdxDocumentCapability.Disabled;
  edtNumberFormat.Views.Simple.Padding.Value := Rect(3, 1, 0, 0);
end;

procedure TdxRichEditCustomSimpleNumberingListForm.NumberFormatKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_TAB:
      if Shift = [] then
        ChangeFocus
      else
      if Shift = [ssShift] then
        ChangeFocus(False)
      else
        Exit;
    VK_RETURN:
      ApplyChanges;
    VK_ESCAPE:
      Close;
    else
      Exit;
  end;
  Key := 0;
end;

procedure TdxRichEditCustomSimpleNumberingListForm.StartTextChanged(Sender: TObject);
begin
  FIsCloseForm := edStart.ValidateEdit(False);
end;

procedure TdxRichEditCustomSimpleNumberingListForm.StartValueChanged(Sender: TObject);
begin
  if not edStart.ValidateEdit(False) then
    Exit;
  Controller.Start := edStart.Value;
  UpdateNumberFormat;
end;

procedure TdxRichEditCustomSimpleNumberingListForm.SubscribeControlsEvents;
begin
  inherited SubscribeControlsEvents;
  cmbDisplayFormat.Properties.OnChange := FormatValueChanged;
  cmbNumberingAlignment.Properties.OnChange := AlignmentValueChanged;
  edStart.Properties.OnChange := StartValueChanged;
  edStart.Properties.OnEditValueChanged := StartTextChanged;
  edtNumberFormat.OnKeyDown := NumberFormatKeyDown;
  edtNumberFormat.OnContentChanged := DisplayFormatChanged;
end;

procedure TdxRichEditCustomSimpleNumberingListForm.UnsubscribeControlsEvents;
begin
  inherited UnsubscribeControlsEvents;
  cmbDisplayFormat.Properties.OnChange := nil;
  cmbNumberingAlignment.Properties.OnChange := nil;
  edStart.Properties.OnChange := nil;
  edStart.Properties.OnEditValueChanged := nil;
  edtNumberFormat.OnKeyDown := nil;
  edtNumberFormat.OnContentChanged := nil;
end;

procedure TdxRichEditCustomSimpleNumberingListForm.UpdateFormCore;
begin
  inherited UpdateFormCore;
  DisplayFormat := Controller.Format;
  NumberingAlignment := Controller.Alignment;
  edStart.Value := Controller.Start;
  UpdateNumberFormat;
  FormatValueChanged(edStart);
end;

procedure TdxRichEditCustomSimpleNumberingListForm.UpdateNumberFormat;
begin
  if Assigned(FDisplayFormatHelper) then
    FDisplayFormatHelper.SetDisplayFormat(Controller.DisplayFormat);
end;

end.
