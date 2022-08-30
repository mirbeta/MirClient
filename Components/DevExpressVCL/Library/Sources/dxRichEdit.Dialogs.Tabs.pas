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

unit dxRichEdit.Dialogs.Tabs;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, Menus,
  dxCore, dxCoreClasses, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, dxLayoutLookAndFeels, cxClasses,
  dxLayoutContainer, dxLayoutControl, dxLayoutControlAdapters, dxLayoutcxEditAdapters, cxContainer, cxEdit,
  cxLabel, cxMaskEdit, cxSpinEdit, cxListBox, cxTextEdit, StdCtrls, cxButtons, cxGroupBox, cxRadioGroup,
  dxMeasurementUnitEdit, Generics.Defaults, Generics.Collections,
  dxRichEdit.DocumentModel.TabFormatting,
  dxRichEdit.DocumentModel.UnitConverter,
  dxRichEdit.Dialogs.CustomDialog,
  dxRichEdit.Dialogs.Core,
  dxRichEdit.Dialogs.TabsFormController,
  dxRichEdit.LayoutEngine.Formatter,
  dxRichEdit.Utils.Types,
  dxGenerics;

type
  TdxRichEditTabsDialogForm = class(TdxRichEditCustomDialogForm)
    btnCancel: TcxButton;
    btnClear: TcxButton;
    btnClearAll: TcxButton;
    btnOk: TcxButton;
    btnSet: TcxButton;
    dxLayoutControl1Group1: TdxLayoutAutoCreatedGroup;
    dxLayoutControl1Group10: TdxLayoutAutoCreatedGroup;
    dxLayoutControl1Group2: TdxLayoutGroup;
    dxLayoutControl1Group3: TdxLayoutGroup;
    dxLayoutControl1Group4: TdxLayoutGroup;
    dxLayoutControl1Group5: TdxLayoutAutoCreatedGroup;
    dxLayoutControl1Group6: TdxLayoutGroup;
    dxLayoutControl1Group7: TdxLayoutAutoCreatedGroup;
    dxLayoutControl1Group8: TdxLayoutAutoCreatedGroup;
    dxLayoutControl1Group9: TdxLayoutGroup;
    dxLayoutControl1Item1: TdxLayoutItem;
    dxLayoutControl1Item12: TdxLayoutItem;
    dxLayoutControl1Item13: TdxLayoutItem;
    dxLayoutControl1Item14: TdxLayoutItem;
    dxLayoutControl1Item15: TdxLayoutItem;
    dxLayoutControl1Item16: TdxLayoutItem;
    dxLayoutControl1Item17: TdxLayoutItem;
    dxLayoutControl1Item18: TdxLayoutItem;
    dxLayoutControl1Item19: TdxLayoutItem;
    dxLayoutControl1Item2: TdxLayoutItem;
    dxLayoutControl1Item20: TdxLayoutItem;
    dxLayoutControl1Item21: TdxLayoutItem;
    dxLayoutControl1Item22: TdxLayoutItem;
    dxLayoutControl1Item3: TdxLayoutItem;
    dxLayoutControl1Item4: TdxLayoutItem;
    dxLayoutControl1Item5: TdxLayoutItem;
    dxLayoutControl1Item6: TdxLayoutItem;
    dxLayoutControl1Item7: TdxLayoutItem;
    dxLayoutControl1Item9: TdxLayoutItem;
    dxLayoutCxLookAndFeel2: TdxLayoutCxLookAndFeel;
    lblAlignment: TdxLayoutSeparatorItem;
    lblLeader: TdxLayoutSeparatorItem;
    lblRemoveTabStops: TcxLabel;
    lblTabStopsToBeCleared: TcxLabel;
    lbTabStopPosition: TcxListBox;
    lciDefaultTabStops: TdxLayoutItem;
    lciTabStopPosition: TdxLayoutItem;
    lcMainGroup_Root: TdxLayoutGroup;
    rbCenter: TcxRadioButton;
    rbDecimal: TcxRadioButton;
    rbDots: TcxRadioButton;
    rbEqualSign: TcxRadioButton;
    rbHyphens: TcxRadioButton;
    rbLeft: TcxRadioButton;
    rbMiddleDots: TcxRadioButton;
    rbNone: TcxRadioButton;
    rbRight: TcxRadioButton;
    rbThickLine: TcxRadioButton;
    rbUnderline: TcxRadioButton;
    seDefaultTabStops: TdxMeasurementUnitEdit;
    teTabStopPosition: TcxTextEdit;
    procedure AlignmentOrLeaderSelected(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure btnClearAllClick(Sender: TObject);
    procedure btnSetClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormShow(Sender: TObject);
    procedure seDefaultTabStopsPropertiesChange(Sender: TObject);
    procedure TabStopPositions(Sender: TObject);
    procedure teTabStopPositionPropertiesChange(Sender: TObject);
    procedure teTabStopPositionPropertiesValidate(Sender: TObject; var DisplayValue: Variant; var ErrorText: TCaption;
      var Error: Boolean);
  private
    FRemovedTabStops: TdxIntegerList;
    FTabsFormOwner: IdxFormOwner;
    FTabFormattingInfo: TdxTabFormattingInfo;
    FTabStopPositionMeasurementUnit: TdxMeasurementUnitEditHelper;
    function FindTabStopWithClosePosition(ATabFormattinInfo: TdxTabFormattingInfo; ANewTabStopPosition: Integer): TdxNullableValue<TdxTabInfo>;
    function GetController: TdxTabsFormController; inline;
    function GetIsSetAndClearButtonDisabled: Boolean;
    function GetTabAlignmentType: TdxTabAlignmentType;
    function GetTabFormattingInfo: TdxTabFormattingInfo; inline;
    function GetTabLeaderType: TdxTabLeaderType;
    function GetTabStopPosition: TdxNullableInteger;
    function GetTabStopPositionIndex: Integer;
    function PositionToText(APosition: integer): string;
    procedure RemoveTabStop(AItemIndex: Integer; ATabFormattingInfo: TdxTabFormattingInfo);
    procedure SetTabAlignmentType(const Value: TdxTabAlignmentType);
    procedure SetTabFormattingInfo(const Value: TdxTabFormattingInfo);
    procedure SetTabStopPosition(const Value: TdxNullableInteger);
    procedure SetTabLeaderType(const Value: TdxTabLeaderType);
  protected const
    DefaultFirstLineIndent = 1440 div 2;
    MaxIndentByDefault = 22 * 1440;
    MinIndentByDefault = -22 * 1440;
    MaxSpacingByDefault = 22 * 1440;
    MaxLineSpacingByDefault = MaxSpacingByDefault;
    MinLineSpacingByDefault = 20;
    MinTabStopPositionByDefault = -22 * 1440;
    MaxTabStopPositionByDefault = 22 * 1440;
    MinDefaultTabStopPosition = 20;

    MinimumAllowableTabStopPostionDistance = 4;
  protected
    FIsClearAllHappend: Boolean;
    procedure ApplyLocalization; override;
    function CreateController(AControllerParameters: TdxFormControllerParameters): TdxFormController; override;
    function GetEditedTabInfo: TdxNullableValue<TdxTabInfo>;
    function GetStringRepresentationsOfUnit(AUnitInTwips: TdxNullableInteger): string;
    procedure InitializeForm; override;
    procedure SubscribeControlsEvents; override;
    procedure UnsubscribeControlsEvents; override;

    function DoValidateStopPosition: Boolean;
    procedure UpdateFormCore; override;
    procedure UpdateLblRemoteTabs;
    procedure UpdateSetAndClearButtons;
    procedure UpdatePosition;
    procedure UpdateRgAlignment;
    procedure UpdateRgLeader;
    procedure UpdateTabInfoAlignmentAndLeader(const ATabInfo: TdxTabInfo);
    procedure UpdateTabStopPositions;

    property IsSetAndClearButtonDisabled: Boolean read GetIsSetAndClearButtonDisabled;
    property TabAlignmentType: TdxTabAlignmentType read GetTabAlignmentType write SetTabAlignmentType;
    property TabLeaderType: TdxTabLeaderType read GetTabLeaderType write SetTabLeaderType;
    property TabsFormOwner: IdxFormOwner read FTabsFormOwner;
    property TabFormattingInfo: TdxTabFormattingInfo read GetTabFormattingInfo write SetTabFormattingInfo;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Controller: TdxTabsFormController read GetController;
    property TabStopPosition: TdxNullableInteger read GetTabStopPosition write SetTabStopPosition;
    property TabStopPositionIndex: Integer read GetTabStopPositionIndex;
  end;

implementation

uses
  StrUtils,
  dxRichEdit.Dialogs.Strs,
  dxRichEdit.Utils.Exceptions.Strs;

{$R *.dfm}

{ TdxRichEditTabsDialogForm }

constructor TdxRichEditTabsDialogForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRemovedTabStops := TdxIntegerList.Create;
  FTabFormattingInfo := TdxTabFormattingInfo.Create;
end;

destructor TdxRichEditTabsDialogForm.Destroy;
begin
  FRemovedTabStops.Free;
  FTabStopPositionMeasurementUnit.Free;
  FTabFormattingInfo.Free;
  inherited;
end;

procedure TdxRichEditTabsDialogForm.AlignmentOrLeaderSelected(Sender: TObject);
var
  ATabInfo: TdxNullableValue<TdxTabInfo>;
begin
  ATabInfo := GetEditedTabInfo;
  if not ATabInfo.IsNull then
		UpdateTabInfoAlignmentAndLeader(ATabInfo.Value);
end;

procedure TdxRichEditTabsDialogForm.btnClearAllClick(Sender: TObject);
begin
	Controller.TabFormattingInfo.Clear;
  FIsClearAllHappend := True;
	UpdateForm;
end;

procedure TdxRichEditTabsDialogForm.btnClearClick(Sender: TObject);
var
  ACurrentItemIndex: Integer;
  ALastTabIndex: Integer;
  ATabFormattingInfo: TdxTabFormattingInfo;
begin
	ACurrentItemIndex := lbTabStopPosition.ItemIndex;
	if ACurrentItemIndex < 0 then
    Exit;
  ATabFormattingInfo := Controller.TabFormattingInfo;
  RemoveTabStop(ACurrentItemIndex, ATabFormattingInfo);
	ALastTabIndex := ATabFormattingInfo.Count - 1;
  if ACurrentItemIndex > ALastTabIndex then
    ACurrentItemIndex := ALastTabIndex;
  UpdateForm;
  UnsubscribeControlsEvents;
  try
    if ALastTabIndex > -1 then
      TabStopPosition := ATabFormattingInfo[ACurrentItemIndex].Position;
  finally
    SubscribeControlsEvents;
  end;
end;

procedure TdxRichEditTabsDialogForm.btnSetClick(Sender: TObject);
var
  ATabStopPosition: TdxNullableInteger;
  ACloseTabInfo: TdxNullableValue<TdxTabInfo>;
  AAlignment: TdxTabAlignmentType;
  ALeader: TdxTabLeaderType;
  AItem: TdxTabInfo;
begin
  if not DoValidateStopPosition then
    Exit;
  ATabStopPosition := TabStopPosition;
  if ATabStopPosition.IsNull then
    Exit;

  ACloseTabInfo := FindTabStopWithClosePosition(Controller.TabFormattingInfo, ATabStopPosition.Value);
  if not ACloseTabInfo.IsNull then
  begin
    TabStopPosition := ACloseTabInfo.Value.Position;
    Exit;
  end;

  AAlignment := GetTabAlignmentType;
  ALeader := GetTabLeaderType;
  AItem := TdxTabInfo.Create(ATabStopPosition.Value, AAlignment, ALeader, False, False);
  Controller.TabFormattingInfo.Add(AItem);
  UpdateForm;
end;

procedure TdxRichEditTabsDialogForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if ModalResult <> mrOk then
    Exit;
  btnSetClick(Sender);
  if not DoValidateStopPosition then
    CanClose := False
  else
    Controller.ApplyChanges;
end;

procedure TdxRichEditTabsDialogForm.FormShow(Sender: TObject);
begin
  if Assigned(TabsFormOwner) then
    TabsFormOwner.Hide;
end;

procedure TdxRichEditTabsDialogForm.seDefaultTabStopsPropertiesChange(Sender: TObject);
var
  AValue: Variant;
begin
  AValue := GetValueFromEditor(seDefaultTabStops);
  if not VarIsNull(AValue) then
  	Controller.DefaultTabWidth := AValue;
end;

procedure TdxRichEditTabsDialogForm.TabStopPositions(Sender: TObject);
var
  ACurrentPositionIndex: Integer;
begin
  ACurrentPositionIndex := lbTabStopPosition.ItemIndex;
  if ACurrentPositionIndex < 0 then
    Exit;

  TabStopPosition := TabFormattingInfo[ACurrentPositionIndex].Position;
  UpdateForm;
end;

procedure TdxRichEditTabsDialogForm.teTabStopPositionPropertiesChange(Sender: TObject);
begin
  UpdateSetAndClearButtons;
end;

procedure TdxRichEditTabsDialogForm.teTabStopPositionPropertiesValidate(Sender: TObject; var DisplayValue: Variant;
  var ErrorText: TCaption; var Error: Boolean);
var
  AValue: Variant;
  S, AMinValue, AMaxValue: string;
begin
  S := VarToStr(DisplayValue);
  AValue := FTabStopPositionMeasurementUnit.GetValueFromText(S, False);
  if VarIsNull(AValue) then
  begin
    ErrorText := cxGetResourceString(@sdxRichEditExceptionInvalidTabStop);
    Error := True;
  end
  else
    if FTabStopPositionMeasurementUnit.CorrectRange(AValue) <> AValue then
    begin
      AMinValue := PositionToText(MinTabStopPositionByDefault);
      AMaxValue := PositionToText(MaxTabStopPositionByDefault);
      ErrorText := Format(cxGetResourceString(@sdxRichEditExceptionInvalidValueRange), [AMinValue, AMaxValue]);
      Error := True;
    end;
end;

function TdxRichEditTabsDialogForm.FindTabStopWithClosePosition(ATabFormattinInfo: TdxTabFormattingInfo; ANewTabStopPosition: Integer): TdxNullableValue<TdxTabInfo>;
var
  I, ACount: Integer;
  ADelta: Integer;
  ATabInfo: TdxTabInfo;
begin
  ACount := ATabFormattinInfo.Count;
  for I := 0 to ACount - 1 do
  begin
    ATabInfo := ATabFormattinInfo[I];
    ADelta := Abs(ATabInfo.Position - ANewTabStopPosition);
		if ADelta <= MinimumAllowableTabStopPostionDistance then
      Exit(ATabInfo);
  end;
  Result := TdxNullableValue<TdxTabInfo>.Null;
end;

function TdxRichEditTabsDialogForm.GetController: TdxTabsFormController;
begin
  Result := TdxTabsFormController(inherited Controller);
end;

function TdxRichEditTabsDialogForm.GetIsSetAndClearButtonDisabled: Boolean;
begin
  Result := Trim(teTabStopPosition.EditingText) = '';
end;

function TdxRichEditTabsDialogForm.GetTabAlignmentType: TdxTabAlignmentType;
begin
  if rbLeft.Checked then
    Result := TdxTabAlignmentType.Left
  else
    if rbCenter.Checked then
      Result := TdxTabAlignmentType.Center
    else
      if rbRight.Checked then
        Result := TdxTabAlignmentType.Right
      else
        if rbDecimal.Checked then
          Result := TdxTabAlignmentType.Decimal
        else
          Result := TdxTabAlignmentType.Left;
end;

function TdxRichEditTabsDialogForm.GetTabFormattingInfo: TdxTabFormattingInfo;
begin
  Result := FTabFormattingInfo;
end;

function TdxRichEditTabsDialogForm.GetTabLeaderType: TdxTabLeaderType;
begin
  if rbNone.Checked then
    Result := TdxTabLeaderType.None
  else
    if rbDots.Checked then
      Result := TdxTabLeaderType.Dots
    else
      if rbMiddleDots.Checked then
        Result := TdxTabLeaderType.MiddleDots
      else
        if rbHyphens.Checked then
          Result := TdxTabLeaderType.Hyphens
        else
          if rbUnderline.Checked then
            Result := TdxTabLeaderType.Underline
          else
            if rbThickLine.Checked then
              Result := TdxTabLeaderType.ThickLine
            else
              if rbEqualSign.Checked then
                Result := TdxTabLeaderType.EqualSign
              else
                Result := TdxTabLeaderType.None;
end;

function TdxRichEditTabsDialogForm.GetTabStopPosition: TdxNullableInteger;
var
  S: string;
  AValue: Variant;
  AUnit: TdxUIUnit;
begin
  S := VarToStr(teTabStopPosition.EditingValue);
  if Trim(S) = '' then
    Exit(TdxNullableInteger.Null);
  AValue := FTabStopPositionMeasurementUnit.GetValueFromText(S, False);
  AUnit := TdxUIUnit.Create(AValue, UnitType);
  Result := UnitConverter.ToTwipsUnit(AUnit);
end;

function TdxRichEditTabsDialogForm.GetTabStopPositionIndex: Integer;
var
  S: string;
begin
  if TabStopPosition.IsNull then
    Result := -1
  else
  begin
    S := PositionToText(TabStopPosition.Value);
    Result := lbTabStopPosition.Items.IndexOf(S);
  end;
end;

function TdxRichEditTabsDialogForm.PositionToText(APosition: integer): string;
var
  AUnit: TdxUIUnit;
begin
  AUnit := UnitConverter.ToUIUnit(APosition, UnitType);
  Result := FTabStopPositionMeasurementUnit.GetTextFromValue(AUnit.Value);
end;

procedure TdxRichEditTabsDialogForm.RemoveTabStop(AItemIndex: Integer; ATabFormattingInfo: TdxTabFormattingInfo);
var
  ATabInfo: TdxTabInfo;
  APosition: Integer;
begin
  ATabInfo := ATabFormattingInfo[AItemIndex];
  ATabFormattingInfo.Remove(ATabInfo);
  APosition := ATabInfo.Position;
  if not FRemovedTabStops.Contains(APosition) then
  begin
    FRemovedTabStops.Add(APosition);
    FRemovedTabStops.Sort;
  end;
end;

procedure TdxRichEditTabsDialogForm.SetTabAlignmentType(const Value: TdxTabAlignmentType);
begin
  case Value of
    TdxTabAlignmentType.Left:
      rbLeft.Checked := True;
    TdxTabAlignmentType.Center:
      rbCenter.Checked := True;
    TdxTabAlignmentType.Right:
      rbRight.Checked := True;
    TdxTabAlignmentType.Decimal:
      rbDecimal.Checked := True;
  end;
end;

procedure TdxRichEditTabsDialogForm.SetTabFormattingInfo(const Value: TdxTabFormattingInfo);
begin
  FTabFormattingInfo.Clear;
  FTabFormattingInfo.AddRange(Value);
end;

procedure TdxRichEditTabsDialogForm.SetTabStopPosition(const Value: TdxNullableInteger);
var
  AUnitInTwips: TdxNullableInteger;
  ATabStopPosition: TdxNullableInteger;
  AEditValueStr: string;
  ANewValueStr: string;
begin
  if Value.IsNull then
    teTabStopPosition.EditValue := ''
  else
  begin
    AUnitInTwips := UnitConverter.ModelUnitsToTwips(Value.Value);
    ANewValueStr := GetStringRepresentationsOfUnit(AUnitInTwips);
    AEditValueStr := teTabStopPosition.EditValue;
    ATabStopPosition := TabStopPosition;
    if not ATabStopPosition.IsNull and (ATabStopPosition = Value) and (ANewValueStr = AEditValueStr) then
      Exit;
    teTabStopPosition.EditValue := ANewValueStr;
  end;
end;

procedure TdxRichEditTabsDialogForm.SetTabLeaderType(const Value: TdxTabLeaderType);
begin
  case Value of
    TdxTabLeaderType.None:
      rbNone.Checked := True;
    TdxTabLeaderType.Dots:
      rbDots.Checked := True;
    TdxTabLeaderType.MiddleDots:
      rbMiddleDots.Checked := True;
    TdxTabLeaderType.Hyphens:
      rbHyphens.Checked := True;
    TdxTabLeaderType.Underline:
      rbUnderline.Checked := True;
    TdxTabLeaderType.ThickLine:
      rbThickLine.Checked := True;
    TdxTabLeaderType.EqualSign:
      rbEqualSign.Checked := True;
  end;
end;

procedure TdxRichEditTabsDialogForm.ApplyLocalization;
begin
  Caption := cxGetResourceString(@sdxRichEditTabsDialogForm);
  btnOk.Caption := cxGetResourceString(@sdxRichEditDialogButtonOk);
  btnCancel.Caption := cxGetResourceString(@sdxRichEditDialogButtonCancel);
  lblTabStopsToBeCleared.Caption := cxGetResourceString(@sdxRichEditTabsDialogTabStopsToBeCleared);
  lblAlignment.Caption := cxGetResourceString(@sdxRichEditTabsDialogAlignment);
  lblLeader.Caption := cxGetResourceString(@sdxRichEditTabsDialogLeader);
  btnSet.Caption := cxGetResourceString(@sdxRichEditTabsDialogButtonSet);
  btnClear.Caption := cxGetResourceString(@sdxRichEditTabsDialogButtonClear);
  btnClearAll.Caption := cxGetResourceString(@sdxRichEditTabsDialogButtonClearAll);
  rbLeft.Caption := cxGetResourceString(@sdxRichEditTabsDialogLeft);
  rbDecimal.Caption := cxGetResourceString(@sdxRichEditTabsDialogDecimal);
  rbCenter.Caption := cxGetResourceString(@sdxRichEditTabsDialogCenter);
  rbHyphens.Caption := cxGetResourceString(@sdxRichEditTabsDialogHyphens);
  rbRight.Caption := cxGetResourceString(@sdxRichEditTabsDialogRight);
  rbNone.Caption := cxGetResourceString(@sdxRichEditTabsDialogNone);
  rbDots.Caption := cxGetResourceString(@sdxRichEditTabsDialogDots);
  rbMiddleDots.Caption := cxGetResourceString(@sdxRichEditTabsDialogMiddleDots);
  rbUnderline.Caption := cxGetResourceString(@sdxRichEditTabsDialogUnderline);
  rbThickLine.Caption := cxGetResourceString(@sdxRichEditTabsDialogThickLine);
  rbEqualSign.Caption := cxGetResourceString(@sdxRichEditTabsDialogEqualSign);
  lciTabStopPosition.CaptionOptions.Text := cxGetResourceString(@sdxRichEditTabsDialogTabStopPosition);
  lciDefaultTabStops.CaptionOptions.Text := cxGetResourceString(@sdxRichEditTabsDialogDefaultTabStops);
end;

function TdxRichEditTabsDialogForm.CreateController(
  AControllerParameters: TdxFormControllerParameters): TdxFormController;
begin
  Result := TdxTabsFormController.Create(AControllerParameters as TdxTabsFormControllerParameters);
end;

function TdxRichEditTabsDialogForm.GetEditedTabInfo: TdxNullableValue<TdxTabInfo>;
var
  AIndex: Integer;
begin
  if Controller.TabFormattingInfo.Count < 1 then
    Exit(TdxNullableValue<TdxTabInfo>.Null);
  AIndex := TabStopPositionIndex;
  if AIndex < 0 then
    Result := TdxNullableValue<TdxTabInfo>.Null
  else
    Result := Controller.TabFormattingInfo[AIndex];
end;

function TdxRichEditTabsDialogForm.GetStringRepresentationsOfUnit(AUnitInTwips: TdxNullableInteger): string;
begin
  if AUnitInTwips.IsNull then
    Result := ''
  else
    Result := PositionToText(AUnitInTwips.Value);
end;

procedure TdxRichEditTabsDialogForm.InitializeForm;
begin
  FTabsFormOwner := Controller.FormOwner;

  InitializeMeasurementUnitEdit(seDefaultTabStops, ToMeasurementType(UnitType),
    TdxMeasurementUnitEditHelper.Create(UnitTypeDescription, 0.1, 2,
      ModelUnitsToUIUnit(MinDefaultTabStopPosition), ModelUnitsToUIUnit(MaxTabStopPositionByDefault)));
  FTabStopPositionMeasurementUnit := TdxMeasurementUnitEditHelper.Create(UnitTypeDescription, 0.1, 2,
    ModelUnitsToUIUnit(MinTabStopPositionByDefault), ModelUnitsToUIUnit(MaxTabStopPositionByDefault));

  TabAlignmentType := TdxTabAlignmentType.Left;
  TabLeaderType := TdxTabLeaderType.None;
end;

procedure TdxRichEditTabsDialogForm.SubscribeControlsEvents;
begin
  seDefaultTabStops.Properties.OnChange := seDefaultTabStopsPropertiesChange;
  teTabStopPosition.Properties.OnChange := teTabStopPositionPropertiesChange;
  rbCenter.OnClick := AlignmentOrLeaderSelected;
  rbDecimal.OnClick := AlignmentOrLeaderSelected;
  rbDots.OnClick := AlignmentOrLeaderSelected;
  rbEqualSign.OnClick := AlignmentOrLeaderSelected;
  rbHyphens.OnClick := AlignmentOrLeaderSelected;
  rbLeft.OnClick := AlignmentOrLeaderSelected;
  rbMiddleDots.OnClick := AlignmentOrLeaderSelected;
  rbNone.OnClick := AlignmentOrLeaderSelected;
  rbRight.OnClick := AlignmentOrLeaderSelected;
  rbThickLine.OnClick := AlignmentOrLeaderSelected;
  rbUnderline.OnClick := AlignmentOrLeaderSelected;
end;

procedure TdxRichEditTabsDialogForm.UnsubscribeControlsEvents;
begin
  seDefaultTabStops.Properties.OnChange := nil;
  teTabStopPosition.Properties.OnChange := nil;
  rbCenter.OnClick := nil;
  rbDecimal.OnClick := nil;
  rbDots.OnClick := nil;
  rbEqualSign.OnClick := nil;
  rbHyphens.OnClick := nil;
  rbLeft.OnClick := nil;
  rbMiddleDots.OnClick := nil;
  rbNone.OnClick := nil;
  rbRight.OnClick := nil;
  rbThickLine.OnClick := nil;
  rbUnderline.OnClick := nil;
end;

function TdxRichEditTabsDialogForm.DoValidateStopPosition: Boolean;
var
  AValue: Variant;
  S: string;
begin
  S := Trim(teTabStopPosition.Text);
  if S = '' then
    Exit(True);
  AValue := FTabStopPositionMeasurementUnit.GetValueFromText(S);
  Result := not VarIsNull(AValue);
end;

procedure TdxRichEditTabsDialogForm.UpdateFormCore;
begin
  TabFormattingInfo := Controller.TabFormattingInfo;
  UpdateTabStopPositions;
  UpdateSetAndClearButtons;
  UpdateRgAlignment;
  UpdateRgLeader;
  UpdateLblRemoteTabs;
  SetValueToEditor(seDefaultTabStops, Controller.DefaultTabWidth);
end;

procedure TdxRichEditTabsDialogForm.UpdateLblRemoteTabs;
var
  AStringBuilder: TStringBuilder;
  AItem: Integer;
  AListSeparator: Char;
  I: Integer;
begin
  if FIsClearAllHappend then
    lblRemoveTabStops.Caption := cxGetResourceString(@sdxRichEditTabForm_All)
  else
    begin
      AStringBuilder := TStringBuilder.Create;
      try
        AListSeparator := ListSeparator;
        for I := 0 to FRemovedTabStops.Count - 1 do
        begin
          AItem := FRemovedTabStops[I];
          if AStringBuilder.Length > 0 then
            AStringBuilder.Append(AListSeparator).Append(' ');
          AStringBuilder.Append(PositionToText(AItem));
        end;
        lblRemoveTabStops.Caption := AStringBuilder.ToString;
      finally
        AStringBuilder.Free;
      end;
    end;
end;

procedure TdxRichEditTabsDialogForm.UpdateSetAndClearButtons;
var
  IsEnabled: Boolean;
begin
  IsEnabled := not IsSetAndClearButtonDisabled;
  btnSet.Enabled := IsEnabled;
  btnClear.Enabled := IsEnabled;
end;

procedure TdxRichEditTabsDialogForm.UpdatePosition;
var
  ATabInfo: TdxNullableValue<TdxTabInfo>;
begin
  ATabInfo := GetEditedTabInfo;
  if ATabInfo.IsNull then
    teTabStopPosition.EditValue := ''
  else
    TabStopPosition := ATabInfo.Value.Position;
end;

procedure TdxRichEditTabsDialogForm.UpdateRgAlignment;
var
  ATabInfo: TdxNullableValue<TdxTabInfo>;
begin
  ATabInfo := GetEditedTabInfo;
  if ATabInfo.IsNull then
    TabAlignmentType := TdxTabAlignmentType.Left
  else
    TabAlignmentType := ATabInfo.Value.Alignment;
end;

procedure TdxRichEditTabsDialogForm.UpdateRgLeader;
var
  ATabInfo: TdxNullableValue<TdxTabInfo>;
begin
  ATabInfo := GetEditedTabInfo;
  if ATabInfo.IsNull then
    TabLeaderType := TdxTabLeaderType.None
  else
    TabLeaderType := ATabInfo.Value.Leader;
end;

procedure TdxRichEditTabsDialogForm.UpdateTabInfoAlignmentAndLeader(const ATabInfo: TdxTabInfo);
var
  ATabFormattingInfo: TdxTabFormattingInfo;
  ANewTabInfo: TdxTabInfo;
  AAlignment: TdxTabAlignmentType;
  ALeader: TdxTabLeaderType;
begin
  ATabFormattingInfo := Controller.TabFormattingInfo;
	ATabFormattingInfo.Remove(ATabInfo);
  AAlignment := TabAlignmentType;
  ALeader := TabLeaderType;
  ANewTabInfo := TdxTabInfo.Create(ATabInfo.Position, AAlignment, ALeader, False, False);
  ATabFormattingInfo.Add(ANewTabInfo);
end;

procedure TdxRichEditTabsDialogForm.UpdateTabStopPositions;
var
  I, ACount: Integer;
  ATabInfo: TdxTabInfo;
  APositionInTwips: Integer;
  ATabStopPositionString: string;
begin
  lbTabStopPosition.Items.Clear;
  if TabFormattingInfo = nil then
    Exit;
  ACount := TabFormattingInfo.Count;
  if ACount < 1 then
  begin
    teTabStopPosition.EditValue := '';
    Exit;
  end;

  lbTabStopPosition.Items.BeginUpdate;
  try
    for I := 0 to ACount - 1 do
    begin
      ATabInfo := TabFormattingInfo[I];
      APositionInTwips := UnitConverter.ModelUnitsToTwips(ATabInfo.Position);
      lbTabStopPosition.Items.Add(GetStringRepresentationsOfUnit(APositionInTwips));
    end;
    ATabStopPositionString := GetStringRepresentationsOfUnit(TabStopPosition);
    ATabStopPositionString := IfThen(ATabStopPositionString = '', lbTabStopPosition.Items[0], ATabStopPositionString);
    teTabStopPosition.EditValue := ATabStopPositionString;
    lbTabStopPosition.ItemIndex := lbTabStopPosition.Items.IndexOf(ATabStopPositionString);
  finally
    lbTabStopPosition.Items.EndUpdate;
  end;
end;

end.
