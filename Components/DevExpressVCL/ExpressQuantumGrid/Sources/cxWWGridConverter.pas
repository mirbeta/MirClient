{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressQuantumGrid                                       }
{                                                                    }
{           Copyright (c) 2001-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSQUANTUMGRID AND ALL            }
{   ACCOMPANYING VCL AND CLX CONTROLS AS PART OF AN EXECUTABLE       }
{   PROGRAM ONLY.                                                    }
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
unit cxWWGridConverter;

interface

{$I cxVer.inc}

uses
  Classes, SysUtils, cxConverterFactory, cxGridConverter, cxGridDBBandedTableView, cxGridCustomView,
  DB, cxGraphics, cxCheckBox, Graphics, cxHyperLinkEdit, cxTextEdit, cxImage, cxSpinEdit,
  StdCtrls, cxImageComboBox, cxButtonEdit, cxEdit, cxDBLookupComboBox, cxGrid, cxLookupDBGrid,
  cxDropDownEdit, cxMRUEdit, cxMemo, cxCalendar, Controls, cxGridCustomTableView, cxStyles,
  cxConverterUtils, cxCustomConverter;

type
  { TcxWWGridConverter }
  TcxWWGridConverter = class(TcxCustomGridConverter)
  private
    FColor: Integer;
    FFont: TFont;
    FTitleColor: Integer;
    FTitleFont: TFont;
    function ExtractData(const AData: string; var AResultData: string;
      AStartIndex: Integer; ASeparator: Char): Integer;
    function GetCharCase(const ACharCase: string): TEditCharCase;
    function GetcxGridView: TcxGridDBBandedTableView;
    procedure GetFieldNames(AList: TStringList);
    function GetFontWidth(AFont: TFont): Integer;
    procedure ImportColumnCheckBox(AcxColumn: TcxGridDBBandedColumn; const AColumnData: string;
      const AReadOnly: Boolean);
    procedure ImportColumnCustomEdit(AcxColumn: TcxGridDBBandedColumn; const AColumnData: string);
    procedure ImportColumnImage(AcxColumn: TcxGridDBBandedColumn; const AColumnData: string;
      const AReadOnly: Boolean);
    procedure ImportColumnRichEdit(AcxColumn: TcxGridDBBandedColumn; const AColumnData: string);
    procedure ImportColumnText(AcxColumn: TcxGridDBBandedColumn; const AReadOnly: Boolean);
    procedure ImportColumnUrlLink(AcxColumn: TcxGridDBBandedColumn; const AReadOnly: Boolean);
    procedure ImportBands;
    procedure ImportColumns;
    procedure ImportGrid;
    procedure ImportGridStyles;
    procedure ImportWWCheckBox(AcxColumn: TcxGridDBBandedColumn; AComponent: TComponent);
    procedure ImportWWComboBox(AcxColumn: TcxGridDBBandedColumn; AComponent: TComponent);
    procedure ImportWWComboDlg(AcxColumn: TcxGridDBBandedColumn; AComponent: TComponent);
    procedure ImportWWDateTimePicker(AcxColumn: TcxGridDBBandedColumn; AComponent: TComponent);
    procedure ImportWWExpandButton(AcxColumn: TcxGridDBBandedColumn; AComponent: TComponent);
    procedure ImportWWLookupCombo(AcxColumn: TcxGridDBBandedColumn; AComponent: TComponent);
    procedure ImportWWLookupComboDlg(AcxColumn: TcxGridDBBandedColumn; AComponent: TComponent);
    procedure ImportWWMonthCalendar(AcxColumn: TcxGridDBBandedColumn; AComponent: TComponent);
    procedure ImportWWOptions;
    procedure ImportWWRadioGroup(AcxColumn: TcxGridDBBandedColumn; AComponent: TComponent);
    procedure ImportWWSpinEdit(AcxColumn:  TcxGridDBBandedColumn; AComponent: TComponent);
    procedure ImportWWTextEdit(AcxColumn:  TcxGridDBBandedColumn; AComponent: TComponent);
    function IsColumnDefault(const AFieldName: string): string;
    function Pass(const AData: string; AStartIndex: Integer; AFieldCount: Integer;
      ASeparator: Char): Integer;
  protected
    procedure DoRealImport; override;
    procedure DoImportStyles; override;
    function GetGridViewClass: TcxCustomGridViewClass; override;
    property cxGridView: TcxGridDBBandedTableView read GetcxGridView;
  public
    class function GetSourceClassName: string; override;
  end;

implementation

uses cxGridTableView;

{ TcxWWGridConverter }

class function TcxWWGridConverter.GetSourceClassName: string;
begin
  Result := 'TwwDBGrid';
end;

procedure TcxWWGridConverter.DoRealImport;
begin
  cxGridView.DataController.DataSource := GetClassProperty(nil, 'DataSource') as TDataSource;
  ImportBands;
  ImportColumns;
  ImportGrid;
end;

procedure TcxWWGridConverter.DoImportStyles;
begin
  ImportGridStyles;
end;

function TcxWWGridConverter.GetGridViewClass: TcxCustomGridViewClass;
begin
  Result := TcxGridDBBandedTableView;
end;

function TcxWWGridConverter.ExtractData(const AData: string; var AResultData: string;
  AStartIndex: Integer; ASeparator: Char): Integer;
begin
  Result := AStartIndex;
  AResultData := '';
  while Result <= Length(AData) do
  begin
    if AData[Result] = ASeparator then
    begin
      Inc(Result);
      Break;
    end;
    AResultData := AResultData + AData[Result];
    Inc(Result);
  end;
end;

function TcxWWGridConverter.GetCharCase(const ACharCase: string): TEditCharCase;
begin
  if ACharCase = 'ecNormal' then
    Result := ecNormal
  else if ACharCase = 'ecUpperCase' then
    Result := ecUpperCase
  else if ACharCase = 'ecLowerCase' then
    Result := ecLowerCase
  else
    Result := ecNormal;
end;

function TcxWWGridConverter.GetcxGridView: TcxGridDBBandedTableView;
begin
  Result := inherited cxGridView as TcxGridDBBandedTableView;
end;

procedure TcxWWGridConverter.GetFieldNames(AList: TStringList);
var
  ADataSource: TDataSource;
  I: Integer;
begin
  ADataSource := GetClassProperty(nil, 'DataSource') as TDataSource;
  if ADataSource <> nil then
    if ADataSource.DataSet <> nil then
    with ADataSource.DataSet do
      for I := 0 to Fields.Count - 1 do
        AList.Add(Fields[I].FieldName + ';' + IntToStr(Fields[I].DisplayWidth));
end;

function TcxWWGridConverter.GetFontWidth(AFont: TFont): Integer;
var
  APrevFont: TFont;
begin
  Result := 0;
  if AFont = nil then
    Exit;
  with Destination.Canvas do
  begin
    APrevFont := TFont.Create;
    try
      APrevFont.Assign(Font);
      Canvas.Font := AFont;
      Result := Canvas.TextWidth('X');
      Canvas.Font := APrevFont;
    finally
      APrevFont.Free;
    end;
  end;
end;

procedure TcxWWGridConverter.ImportColumnCheckBox(AcxColumn: TcxGridDBBandedColumn;
  const AColumnData: string; const AReadOnly: Boolean);
var
  AValueChecked: string;
  AValueUnchecked: string;
  AIndex: Integer;
begin
  AcxColumn.PropertiesClass := TcxCheckBoxProperties;
  with TcxCheckBoxProperties(AcxColumn.Properties) do
  begin
    ReadOnly := AReadOnly;
    AIndex := Pass(AColumnData, 1, 2, ';');
    AIndex := ExtractData(AColumnData, AValueChecked, AIndex, ';');
    ExtractData(AColumnData, AValueUnchecked, AIndex, ';');
    ValueChecked := AValueChecked;
    ValueUnchecked := AValueUnchecked;
    NullStyle := nssUnchecked;
  end;
end;

procedure TcxWWGridConverter.ImportColumnCustomEdit(AcxColumn: TcxGridDBBandedColumn; const AColumnData: string);
var
  AComponentName: string;
  AComponent: TComponent;
  AIndex: Integer;
begin
  AIndex := Pass(AColumnData, 1, 2, ';');
  ExtractData(AColumnData, AComponentName, AIndex, ';');
  /////////////////////////////////////////////////////////////////////
  //AComponent := Source.Owner.FindComponent(AComponentName);
  /////////////////////////////////////////////////////////////////////
  AComponent := Designer_.GetComponent(AComponentName);
  if AComponent <> nil then
  begin
    if AComponent.ClassName = 'TwwDBSpinEdit' then
      ImportWWSpinEdit(AcxColumn, AComponent)
    else if AComponent.ClassName = 'TwwDBEdit' then
      ImportWWTextEdit(AcxColumn, AComponent)
    else if AComponent.ClassName = 'TwwDBComboBox' then
      ImportWWComboBox(AcxColumn, AComponent)
    else if AComponent.ClassName = 'TwwDBComboDlg' then
      ImportWWComboDlg(AcxColumn, AComponent)
    else if AComponent.ClassName = 'TwwDBLookupCombo' then
      ImportWWLookupCombo(AcxColumn, AComponent)
    else if AComponent.ClassName = 'TwwDBLookupComboDlg' then
      ImportWWLookupComboDlg(AcxColumn, AComponent)
    else if AComponent.ClassName = 'TwwDBMonthCalendar' then
      ImportWWMonthCalendar(AcxColumn, AComponent)
    else if AComponent.ClassName = 'TwwDBDateTimePicker' then
      ImportWWDateTimePicker(AcxColumn, AComponent)
    else if AComponent.ClassName = 'TwwCheckBox' then
      ImportWWCheckBox(AcxColumn, AComponent)
    else if AComponent.ClassName = 'TwwExpandButton' then
      ImportWWExpandButton(AcxColumn, AComponent)
    else if AComponent.ClassName = 'TwwRadioGroup' then
      ImportWWRadioGroup(AcxColumn, AComponent);
  end;
end;

procedure TcxWWGridConverter.ImportColumnImage(AcxColumn: TcxGridDBBandedColumn;
  const AColumnData: string; const AReadOnly: Boolean);
var
  AIndex: Integer;
  AValue1: string;
begin
  AcxColumn.PropertiesClass := TcxImageProperties;
  with TcxImageProperties(AcxColumn.Properties) do
  begin
    ReadOnly := AReadOnly;
    AIndex := Pass(AColumnData, 1, 2, ';');
    ExtractData(AColumnData, AValue1, AIndex, ';');
    Stretch := AValue1 = 'Stretch To Fit';
  end;
end;

procedure TcxWWGridConverter.ImportColumnRichEdit(AcxColumn: TcxGridDBBandedColumn;
  const AColumnData: string);
var
  AComponentName: string;
  AComponent: TComponent;
  AIndex: Integer;
begin
  AIndex := Pass(AColumnData, 1, 2, ';');
  ExtractData(AColumnData, AComponentName, AIndex, ';');
  /////////////////////////////////////////////////////////////////////
  //AComponent := Source.Owner.FindComponent(AComponentName);
  /////////////////////////////////////////////////////////////////////
  AComponent := Designer_.GetComponent(AComponentName);
  if AComponent <> nil then
  begin
    AcxColumn.PropertiesClass := TcxMemoProperties;
    with TcxMemoProperties(AcxColumn.Properties) do
    begin
      Alignment := ConvertAlignment(GetEnumProperty(AComponent, 'Alignment'));
      ReadOnly := GetBooleanProperty(AComponent, 'ReadOnly', ReadOnly);
      AutoSelect := GetBooleanProperty(AComponent, 'AutoSelect', AutoSelect);
      HideSelection := GetBooleanProperty(AComponent, 'HideSelection', HideSelection);
      MaxLength := GetIntegerProperty(AComponent, 'MaxLength', MaxLength);
      ScrollBars := ConvertScrollStyle(GetEnumProperty(AComponent, 'ScrollBars'));
      WantReturns := GetBooleanProperty(AComponent, 'WantReturns', WantReturns);
      WantTabs := GetBooleanProperty(AComponent, 'WantTabs', WantTabs);
      WordWrap := GetBooleanProperty(AComponent, 'WordWrap', WordWrap);
    end;
  end;
end;

procedure TcxWWGridConverter.ImportColumnText(AcxColumn: TcxGridDBBandedColumn;
  const AReadOnly: Boolean);
begin
  AcxColumn.PropertiesClass := TcxTextEditProperties;
  with TcxTextEditProperties(AcxColumn.Properties) do
    ReadOnly := AReadOnly;
end;

procedure TcxWWGridConverter.ImportColumnUrlLink(AcxColumn: TcxGridDBBandedColumn;
  const AReadOnly: Boolean);
begin
  AcxColumn.PropertiesClass := TcxHyperLinkEditProperties;
  with TcxHyperLinkEditProperties(AcxColumn.Properties) do
    ReadOnly := AReadOnly;
end;

procedure TcxWWGridConverter.ImportBands;
begin
  cxGridView.OptionsView.BandHeaders := False;
  cxGridView.Bands.Add;
end;

procedure TcxWWGridConverter.ImportColumns;
var
  AColumns: TStringList;
  AColumnsNeedFree: Boolean;
  AcxColumn: TcxGridDBBandedColumn;
  I: Integer;
  AFieldName: string;
  AWidth: string;
  AHeader: string;
  AReadOnlyString: string;
  AReadOnly: Boolean;
  AColumnData: string;
  AControlType: string;
  AIndex: Integer;
  AFontWidth: Integer;
  AFieldColumns: Boolean;
  ASelected: TStringList;
begin
  AColumnsNeedFree := False;
  ASelected := GetClassProperty(nil, 'Selected') as TStringList;
  if ASelected <> nil then
  begin
    if ASelected.Count = 0 then
    begin
      AColumns := TStringList.Create;
      AColumnsNeedFree := True;
      GetFieldNames(AColumns);
      AFieldColumns := True;
    end
    else
    begin
      AFieldColumns := False;
      if cxGridView.DataController.DataSource <> nil then
        AColumns := ASelected
      else
      begin
        AColumns := TStringList.Create;
        AColumnsNeedFree := True;
      end;
    end;
  end
  else
  begin
    AColumns := TStringList.Create;
    AColumnsNeedFree := True;
    GetFieldNames(AColumns);
    AFieldColumns := True;
  end;
  try
    AFontWidth := GetFontWidth(GetClassProperty(nil, 'Font') as TFont);
    for I := 0 to AColumns.Count - 1 do
    begin
      if AFieldColumns then
      begin
        AIndex := ExtractData(AColumns[I], AFieldName, 1, ';');
        ExtractData(AColumns[I], AWidth, AIndex, ';');
        AHeader := AFieldName;
        AReadOnly := False;
      end
      else
      begin
        AIndex := ExtractData(AColumns[I], AFieldName, 1, #9);
        AIndex := ExtractData(AColumns[I], AWidth, AIndex, #9);
        AIndex := ExtractData(AColumns[I], AHeader, AIndex, #9);
        ExtractData(AColumns[I], AReadOnlyString, AIndex, #9);
        AReadOnly := AReadOnlyString = 'T';
      end;
      AcxColumn := cxGridView.CreateColumn;
      AcxColumn.DataBinding.FieldName := AFieldName;
      AcxColumn.Name := UniqueColumnName(AcxColumn, AcxColumn.DataBinding.FieldName);
      AcxColumn.Width := StrToInt(AWidth) * AFontWidth;
      AcxColumn.Caption := AHeader;
      AcxColumn.HeaderAlignmentHorz := ConvertAlignment(GetEnumProperty(nil, 'TitleAlignment'));
      AColumnData := IsColumnDefault(AFieldName);
      if AColumnData <> '' then
      begin
        AIndex := Pass(AColumnData, 1, 1, ';');
        ExtractData(AColumnData, AControlType, AIndex, ';');
        if AControlType = 'Bitmap' then
          ImportColumnImage(AcxColumn, AColumnData, AReadOnly)
        else if AControlType = 'CheckBox' then
          ImportColumnCheckBox(AcxColumn, AColumnData, AReadOnly)
        else if AControlType = 'CustomEdit' then
        begin
          if Designer_ <> nil then
            ImportColumnCustomEdit(AcxColumn, AColumnData);
        end
        else if AControlType = 'ImageIndex' then
        else if AControlType = 'URL-Link' then
          ImportColumnUrlLink(AcxColumn, AReadOnly)
        else if AControlType = 'RichEdit' then
        begin
          if Designer_ <> nil then
            ImportColumnRichEdit(AcxColumn, AColumnData);
        end
      end
      else
        if AReadOnly then
          ImportColumnText(AcxColumn, AReadOnly);
      AcxColumn.Position.BandIndex := 0;
    end;
  finally
    if AColumnsNeedFree then
      AColumns.Free;
  end;
end;

procedure TcxWWGridConverter.ImportGrid;
begin
  ImportWWOptions;
end;

procedure TcxWWGridConverter.ImportGridStyles;
var
  AStyle: TcxCustomStyle;
  AObject: TObject;
  AHighlightColor: Integer;
  AShadowColor: Integer;
  AAlternatingRowColor: Integer;
  AActiveRecordColor: Integer;
begin
  FColor := GetIntegerProperty(nil, 'Color');
  FFont := GetClassProperty(nil, 'Font') as TFont;
  if (FColor <> clWindow) or not DefaultFont(FFont) then
  begin
    AStyle := CreateStyleItem;
    (AStyle as TcxStyle).Color := FColor;
    (AStyle as TcxStyle).Font.Assign(FFont);
    (AStyle as TcxStyle).TextColor := (AStyle as TcxStyle).Font.Color;
    cxGridView.Styles.Content := AStyle as TcxStyle;
  end;
  FTitleColor := GetIntegerProperty(nil, 'TitleColor');
  FTitleFont := GetClassProperty(nil, 'TitleFont') as TFont;
  if (FTitleColor <> clBtnFace) or not DefaultFont(FTitleFont) then
  begin
    AStyle := CreateStyleItem;
    (AStyle as TcxStyle).Color := FTitleColor;
    (AStyle as TcxStyle).Font.Assign(FTitleFont);
    (AStyle as TcxStyle).TextColor := (AStyle as TcxStyle).Font.Color;
    cxGridView.Styles.Header := AStyle as TcxStyle;
    cxGridView.Styles.Footer := AStyle as TcxStyle;
    cxGridView.Styles.Background := AStyle as TcxStyle;
    cxGridView.Styles.Indicator := AStyle as TcxStyle;
  end;
  AObject := GetClassProperty(nil, 'LineColors');
  if AObject <> nil then
  begin
    AHighlightColor := GetIntegerProperty(AObject, 'HighlightColor');
    if AHighlightColor <> clBtnHighlight then
      cxGridView.OptionsView.GridLineColor := AHighlightColor
    else
    begin
      AShadowColor := GetIntegerProperty(AObject, 'ShadowColor');
      if AShadowColor <> clBtnShadow then
        cxGridView.OptionsView.GridLineColor := AShadowColor;
    end;
  end;
  AObject := GetClassProperty(nil, 'PaintOptions');
  if AObject <> nil then
  begin
    AAlternatingRowColor := GetIntegerProperty(AObject, 'AlternatingRowColor');
    if AAlternatingRowColor <> clNone then
    begin
      AStyle := CreateStyleItem;
      (AStyle as TcxStyle).Color := AAlternatingRowColor;
      (AStyle as TcxStyle).Font.Assign(FFont);
      (AStyle as TcxStyle).TextColor := (AStyle as TcxStyle).Font.Color;
      cxGridView.Styles.ContentOdd := AStyle as TcxStyle;
    end;
    AActiveRecordColor := GetIntegerProperty(AObject, 'ActiveRecordColor');
    if AActiveRecordColor <> clNone then
    begin
      AStyle := CreateStyleItem;
      (AStyle as TcxStyle).Color := AActiveRecordColor;
      (AStyle as TcxStyle).Font.Assign(FFont);
      (AStyle as TcxStyle).TextColor := (AStyle as TcxStyle).Font.Color;
      cxGridView.Styles.Selection := AStyle as TcxStyle;
    end;
  end;
end;

procedure TcxWWGridConverter.ImportWWCheckBox(AcxColumn: TcxGridDBBandedColumn; AComponent: TComponent);
  function GetNullStyle(const AStyle: string): TcxCheckBoxNullValueShowingStyle;
  begin
    if AStyle = 'cbGrayed' then
      Result := nssGrayedChecked
    else if AStyle = 'cbUnchecked' then
      Result := nssUnchecked
    else
      Result := nssInactive;
  end;
begin
  AcxColumn.PropertiesClass := TcxCheckBoxProperties;
  with TcxCheckBoxProperties(AcxColumn.Properties) do
  begin
    Alignment := ConvertAlignment(GetEnumProperty(AComponent, 'Alignment'));
    AllowGrayed := GetBooleanProperty(AComponent, 'AllowGrayed', AllowGrayed);
//    Caption := GetStringProperty(AComponent, 'Caption', Caption);
    DisplayChecked := GetStringProperty(AComponent, 'DisplayValueChecked', DisplayChecked);
    DisplayUnchecked := GetStringProperty(AComponent, 'DisplayValueUnchecked', DisplayUnchecked);
    FullFocusRect := GetBooleanProperty(AComponent, 'ShowFocusRect', FullFocusRect);
    NullStyle := GetNullStyle(GetEnumProperty(AComponent, 'NullAndBlankState'));
    ReadOnly := GetBooleanProperty(AComponent, 'ReadOnly', ReadOnly);
    ValueChecked := GetStringProperty(AComponent, 'ValueChecked', ValueChecked);
    ValueUnchecked := GetStringProperty(AComponent, 'ValueUnchecked', ValueUnchecked);
  end;
end;

procedure TcxWWGridConverter.ImportWWComboBox(AcxColumn: TcxGridDBBandedColumn; AComponent: TComponent);
var
  AwwItems: TStringList;
  AValue: string;
  AIndex: Integer;

  procedure ImportToImageComboBox;
  var
    I: Integer;
    AItem: TcxImageComboBoxItem;
    ADescription: string;
  begin
    AcxColumn.PropertiesClass := TcxImageComboBoxProperties;
    with TcxImageComboBoxProperties(AcxColumn.Properties) do
    begin
      ReadOnly := GetBooleanProperty(AComponent, 'ReadOnly', ReadOnly);
      AwwItems := GetClassProperty(AComponent, 'Items') as TStringList;
      if AwwItems <> nil then
      begin
        for I := 0 to AwwItems.Count - 1 do
        begin
          AItem := Items.Add as TcxImageComboBoxItem;
          AIndex := ExtractData(AwwItems[I], ADescription, 1, #9);
          ExtractData(AwwItems[I], AValue, AIndex, #9);
          AItem.Description := ADescription;
          AItem.Value := AValue;
        end;
      end;
      if GetEnumProperty(AComponent, 'ButtonStyle') = 'cbsCustom' then
        ButtonGlyph.Assign(GetClassProperty(AComponent, 'ButtonGlyph') as TBitmap);
      ImmediateDropDown := GetBooleanProperty(AComponent, 'AutoDropDown', ImmediateDropDown);
      DropDownRows := GetIntegerProperty(AComponent, 'DropDownCount', DropDownRows);
    end;
  end;

  procedure ImportToMRUEdit;
  var
    I: Integer;
  begin
    AcxColumn.PropertiesClass := TcxMRUEditProperties;
    with TcxMRUEditProperties(AcxColumn.Properties) do
    begin
      ReadOnly := GetBooleanProperty(AComponent, 'ReadOnly', ReadOnly);
      AwwItems := GetClassProperty(AComponent, 'Items') as TStringList;
      if AwwItems <> nil then
      begin
        for I := 0 to AwwItems.Count - 1 do
        begin
          AIndex := Pass(AwwItems[I], 1, 1, #9);
          ExtractData(AwwItems[I], AValue, AIndex, #9);
          LookupItems.Add(AValue);
        end;
      end;
      if GetEnumProperty(AComponent, 'ButtonStyle') = 'cbsCustom' then
        ButtonGlyph.Assign(GetClassProperty(AComponent, 'ButtonGlyph') as TBitmap);
      ImmediateDropDown := GetBooleanProperty(AComponent, 'AutoDropDown', ImmediateDropDown);
      DropDownRows := GetIntegerProperty(AComponent, 'DropDownCount', DropDownRows);
      ShowEllipsis := False;
    end;
  end;
var
  AWWHistory: TObject;
begin
  AWWHistory := GetClassProperty(AComponent, 'HistoryList');
  if AWWHistory <> nil then
  begin
    if GetBooleanProperty(AWWHistory, 'Enabled', False) then
      ImportToMRUEdit
    else
      ImportToImageComboBox;
  end
  else
    ImportToImageComboBox;
end;

procedure TcxWWGridConverter.ImportWWComboDlg(AcxColumn: TcxGridDBBandedColumn; AComponent: TComponent);
var
  AButton: TcxEditButton;
  AButtonStyle: string;
begin
  AcxColumn.PropertiesClass := TcxButtonEditProperties;
  with TcxButtonEditProperties(AcxColumn.Properties) do
  begin
    Buttons.Clear;
    AButton := Buttons.Add;
    AButton.Glyph.Assign(GetClassProperty(AComponent, 'ButtonGlyph') as TBitmap);
    AButtonStyle := GetEnumProperty(AComponent, 'ButtonStyle');
    if AButtonStyle = 'cbsCustom' then
      AButton.Kind := bkGlyph
    else if AButtonStyle = 'cbsEllipsis' then
      AButton.Kind := bkEllipsis
    else if AButtonStyle = 'cbsDownArrow' then
      AButton.Kind := bkDown;
    AButton.Width := GetIntegerProperty(AComponent, 'ButtonWidth', AButton.Width);
    CharCase := GetCharCase(GetEnumProperty(AComponent, 'CharCase'));
    MaxLength := GetIntegerProperty(AComponent, 'MaxLength', MaxLength);
    ReadOnly := GetBooleanProperty(AComponent, 'ReadOnly', ReadOnly);
    AButton.Visible := GetBooleanProperty(AComponent, 'ShowButton', AButton.Visible);
  end;
end;

procedure TcxWWGridConverter.ImportWWDateTimePicker(AcxColumn: TcxGridDBBandedColumn; AComponent: TComponent);
  function GetCalendarAlignment(const AValue: string): TAlignment;
  begin
    if AValue = 'wwdtaCenter' then
      Result := taCenter
    else if AValue = 'wwdtaLeft' then
      Result := taLeftJustify
    else if AValue = 'wwdtaRight' then
      Result := taRightJustify
    else
      Result := taCenter;
  end;
var
  AWWCalendarAttributes: TObject;
  ADateButtons: TDateButtons;
  AWWOptions: TStringList;
  I: Integer;
begin
  AcxColumn.PropertiesClass := TcxDateEditProperties;
  with TcxDateEditProperties(AcxColumn.Properties) do
  begin
    AWWCalendarAttributes := GetClassProperty(AComponent, 'CalendarAttributes');
    if AWWCalendarAttributes <> nil then
      Alignment.Horz := GetCalendarAlignment(GetEnumProperty(AWWCalendarAttributes, 'Alignment'));
    ButtonGlyph.Assign(GetClassProperty(AComponent, 'ButtonGlyph') as TBitmap);
    ADateButtons := [btnClear];
    if AWWCalendarAttributes <> nil then
    begin
      AWWOptions := TStringList.Create;
      try
        EnablePropertyException;
        try
          GetSetProperty(AWWCalendarAttributes, 'Options', AWWOptions);
          if not AWWOptions.Find('mdoNoToday', I) then
            Include(ADateButtons, btnToday);
          DateButtons := ADateButtons;
        except
          on EcxUnknownProperty do;
        end;
      finally
        AWWOptions.Free;
        DisablePropertyException;
      end;
    end;
  end;
end;

procedure TcxWWGridConverter.ImportWWExpandButton(AcxColumn: TcxGridDBBandedColumn; AComponent: TComponent);
begin
end;

procedure TcxWWGridConverter.ImportWWLookupCombo(AcxColumn: TcxGridDBBandedColumn;
  AComponent: TComponent);
var
  AListDataSource: TDataSource;
  AListTable: TObject;
  AIndex: Integer;
  ASelected: TStringList;
  I: Integer;
  AListFieldName: string;
  AListFieldWidth: string;
  AListFieldCaption: string;
  AcxLookupColumn: TcxLookupDBGridColumn;
  AWWOptions: TStringList;
  AString: string;
  AWWSeqSearchOptions: TStringList;
begin
  AcxColumn.PropertiesClass := TcxLookupComboBoxProperties;
  with TcxLookupComboBoxProperties(AcxColumn.Properties) do
  begin
    AListTable := GetClassProperty(AComponent, 'LookupTable');
    if AListTable <> nil then
    begin
      AListDataSource := TDataSource.Create(cxGridView);
      AListDataSource.DataSet := AListTable as TDataSet;
      ListSource := AListDataSource;
      KeyFieldNames := GetStringProperty(AComponent, 'LookupField', '');
      ASelected := GetClassProperty(AComponent, 'Selected') as TStringList;
      if ASelected <> nil then
      begin
        for I := 0 to ASelected.Count - 1 do
        begin
          AIndex := ExtractData(ASelected[I], AListFieldName, 1, #9);
          AIndex := ExtractData(ASelected[I], AListFieldWidth, AIndex, #9);
          ExtractData(ASelected[I], AListFieldCaption, AIndex, #9);
          AcxLookupColumn := ListColumns.Add;
          AcxLookupColumn.FieldName := AListFieldName;
          AcxLookupColumn.Caption := AListFieldCaption;
          AcxLookupColumn.Width := StrToInt(AListFieldWidth) *
            GetFontWidth(GetClassProperty(nil, 'Font') as TFont);
        end;
      end;
      DropDownWidth := GetIntegerProperty(AComponent, 'DropDownWidth', DropDownWidth);
      if DropDownWidth = 0 then
      begin
        for I := 0 to ListColumns.Count - 1 do
          DropDownWidth := DropDownWidth + ListColumns[I].Width;
      end;
      DropDownRows := GetIntegerProperty(AComponent, 'DropDownCount', DropDownRows);
      if GetEnumProperty(AComponent, 'ButtonStyle') = 'cbsCustom' then
        ButtonGlyph.Assign(GetClassProperty(AComponent, 'ButtonGlyph') as TBitmap);
      ReadOnly := GetBooleanProperty(AComponent, 'ReadOnly', ReadOnly);
      PopupAlignment := ConvertAlignment(GetEnumProperty(AComponent, 'DropDownAlignment'));
      AString := GetEnumProperty(AComponent, 'Style');
      if AString = 'csDropDown' then
        DropDownListStyle := lsEditList
      else
        DropDownListStyle := lsFixedList;
      ImmediateDropDown := GetBooleanProperty(AComponent, 'AutoDropDown', ImmediateDropDown);
      AWWSeqSearchOptions := TStringList.Create;
      try
        EnablePropertyException;
        try
          GetSetProperty(AComponent, 'SeqSearchOptions', AWWSeqSearchOptions);
          AWWSeqSearchOptions.Sort;
          IncrementalFiltering := AWWSeqSearchOptions.Find('ssoEnabled', I);
          ListOptions.CaseInsensitive := not AWWSeqSearchOptions.Find('ssoCaseSensitive', I);
        except
          on EcxUnknownProperty do;
        end;
      finally
        AWWSeqSearchOptions.Free;
        DisablePropertyException;
      end;
      AWWOptions := TStringList.Create;
      try
        EnablePropertyException;
        try
          GetSetProperty(AComponent, 'Options', AWWOptions);
          AWWOptions.Sort;
          if AWWOptions.Find('loColLines', I) then
          begin
            if AWWOptions.Find('loRowLines', I) then
              ListOptions.GridLines := glBoth
            else
              ListOptions.GridLines := glVertical;
          end
          else
          begin
            if AWWOptions.Find('loRowLines', I) then
              ListOptions.GridLines := glHorizontal
            else
              ListOptions.GridLines := glNone;
          end;
          ListOptions.ShowHeader := AWWOptions.Find('loTitles', I);
        except
          on EcxUnknownProperty do;
        end;
      finally
        AWWOptions.Free;
        DisablePropertyException;
      end;
    end;
  end;
end;

procedure TcxWWGridConverter.ImportWWLookupComboDlg(AcxColumn: TcxGridDBBandedColumn;
  AComponent: TComponent);
var
  AListDataSource: TDataSource;
  AListTable: TObject;
  AIndex: Integer;
  ASelected: TStringList;
  I: Integer;
  AListFieldName: string;
  AListFieldWidth: string;
  AListFieldCaption: string;
  AcxLookupColumn: TcxLookupDBGridColumn;
  AString: string;
  AWWSeqSearchOptions: TStringList;
  AWWGridOptions: TStringList;
begin
  AcxColumn.PropertiesClass := TcxLookupComboBoxProperties;
  with TcxLookupComboBoxProperties(AcxColumn.Properties) do
  begin
    AListTable := GetClassProperty(AComponent, 'LookupTable');
    if AListTable <> nil then
    begin
      AListDataSource := TDataSource.Create(cxGridView);
      AListDataSource.DataSet := AListTable as TDataSet;
      ListSource := AListDataSource;
      KeyFieldNames := GetStringProperty(AComponent, 'LookupField', '');
      ASelected := GetClassProperty(AComponent, 'Selected') as TStringList;
      if ASelected <> nil then
      begin
        for I := 0 to ASelected.Count - 1 do
        begin
          AIndex := ExtractData(ASelected[I], AListFieldName, 1, #9);
          AIndex := ExtractData(ASelected[I], AListFieldWidth, AIndex, #9);
          ExtractData(ASelected[I], AListFieldCaption, AIndex, #9);
          AcxLookupColumn := ListColumns.Add;
          AcxLookupColumn.FieldName := AListFieldName;
          AcxLookupColumn.Caption := AListFieldCaption;
          AcxLookupColumn.Width := StrToInt(AListFieldWidth) *
            GetFontWidth(GetClassProperty(nil, 'Font') as TFont);
        end;
      end;
      DropDownWidth := 0;
      for I := 0 to ListColumns.Count - 1 do
        DropDownWidth := DropDownWidth + ListColumns[I].Width;
      if GetEnumProperty(AComponent, 'ButtonStyle') = 'cbsCustom' then
        ButtonGlyph.Assign(GetClassProperty(AComponent, 'ButtonGlyph') as TBitmap);
      ReadOnly := GetBooleanProperty(AComponent, 'ReadOnly', ReadOnly);
      AString := GetEnumProperty(AComponent, 'Style');
      if AString = 'csDropDown' then
        DropDownListStyle := lsEditList
      else
        DropDownListStyle := lsFixedList;
      ImmediateDropDown := GetBooleanProperty(AComponent, 'AutoDropDown', ImmediateDropDown);
      AWWSeqSearchOptions := TStringList.Create;
      try
        EnablePropertyException;
        try
          GetSetProperty(AComponent, 'SeqSearchOptions', AWWSeqSearchOptions);
          AWWSeqSearchOptions.Sort;
          IncrementalFiltering := AWWSeqSearchOptions.Find('ssoEnabled', I);
          ListOptions.CaseInsensitive := not AWWSeqSearchOptions.Find('ssoCaseSensitive', I);
        except
          on EcxUnknownProperty do;
        end;
      finally
        AWWSeqSearchOptions.Free;
        DisablePropertyException;
      end;
      AWWGridOptions := TStringList.Create;
      try
        EnablePropertyException;
        try
          GetSetProperty(AComponent, 'GridOptions', AWWGridOptions);
          AWWGridOptions.Sort;
          if AWWGridOptions.Find('dgColLines', I) then
          begin
            if AWWGridOptions.Find('dgRowLines', I) then
              ListOptions.GridLines := glBoth
            else
              ListOptions.GridLines := glVertical;
          end
          else
          begin
            if AWWGridOptions.Find('dgRowLines', I) then
              ListOptions.GridLines := glHorizontal
            else
              ListOptions.GridLines := glNone;
          end;
          ListOptions.ShowHeader := AWWGridOptions.Find('dgTitles', I);
        except
          on EcxUnknownProperty do;
        end;
      finally
        AWWGridOptions.Free;
        DisablePropertyException;
      end;
    end;
  end;
end;

procedure TcxWWGridConverter.ImportWWMonthCalendar(AcxColumn: TcxGridDBBandedColumn; AComponent: TComponent);
var
  AWWOptions: TStringList;
  ADateButtons: TDateButtons;
  I: Integer;
begin
  AcxColumn.PropertiesClass := TcxDateEditProperties;
  with TcxDateEditProperties(AcxColumn.Properties) do
  begin
    AWWOptions := TStringList.Create;
    try
      EnablePropertyException;
      try
        GetSetProperty(AComponent, 'Options', AWWOptions);
        ADateButtons := [btnClear];
        if not AWWOptions.Find('mdoNoToday', I) then
          Include(ADateButtons, btnToday);
        DateButtons := ADateButtons;
      except
        on EcxUnknownProperty do;
      end;
    finally
      AWWOptions.Free;
      DisablePropertyException;
    end;
    DateOnError := deToday;
  end;
end;

procedure TcxWWGridConverter.ImportWWOptions;
var
  AOptions: TStringList;
  I: Integer;
begin
  AOptions := TStringList.Create;
  try
    EnablePropertyException;
    try
      GetSetProperty(nil, 'Options', AOptions);
      AOptions.Sort;
      with cxGridView do
      begin
        OptionsBehavior.AlwaysShowEditor := AOptions.Find('dgAlwaysShowEditor', I);
        OptionsData.Editing := AOptions.Find('dgEditing', I);
        OptionsView.Header := AOptions.Find('dgTitles', I);
        OptionsView.Indicator := AOptions.Find('dgIndicator', I);
        OptionsCustomize.ColumnHorzSizing := AOptions.Find('dgColumnResize', I);
        if AOptions.Find('dgColLines', I) then
        begin
          if AOptions.Find('dgRowLines', I) then
            OptionsView.GridLines := glBoth
          else
            OptionsView.GridLines := glVertical;
        end
        else
        begin
          if AOptions.Find('dgRowLines', I) then
            OptionsView.GridLines := glHorizontal
          else
            OptionsView.GridLines := glNone;
        end;
        OptionsBehavior.FocusCellOnTab := AOptions.Find('dgTabs', I);
        OptionsSelection.CellSelect := not AOptions.Find('dgRowSelect', I);
        OptionsSelection.HideSelection := not AOptions.Find('dgAlwaysShowSelection', I);
        OptionsData.DeletingConfirmation := AOptions.Find('dgConfirmDelete', I);
        OptionsData.CancelOnExit := AOptions.Find('dgCancelOnExit', I);
        OptionsSelection.MultiSelect := AOptions.Find('dgMultiSelect', I);
        OptionsView.Footer := AOptions.Find('dgShowFooter', I);
        OptionsBehavior.FocusCellOnCycle := not AOptions.Find('dgTabExitsOnLastCol', I);
      end;
      AOptions.Clear;
      GetSetProperty(nil, 'KeyOptions', AOptions);
      AOptions.Sort;
      with cxGridView do
      begin
        OptionsBehavior.GoToNextCellOnEnter := AOptions.Find('dgEnterToTab', I);
        OptionsData.Deleting := AOptions.Find('dgAllowDelete', I);
        OptionsData.Inserting := AOptions.Find('dgAllowInsert', I);
      end;
    except
      on EcxUnknownProperty do;
    end;
  finally
    AOptions.Free;
    DisablePropertyException;
  end;
end;

procedure TcxWWGridConverter.ImportWWRadioGroup(AcxColumn: TcxGridDBBandedColumn; AComponent: TComponent);
var
  AWWItems: TStringList;
  AWWValues: TStringList;
  AcxItem: TcxImageComboBoxItem;
  I: Integer;
begin
  AcxColumn.PropertiesClass := TcxImageComboBoxProperties;
  with TcxImageComboBoxProperties(AcxColumn.Properties) do
  begin
    AWWItems := GetClassProperty(AComponent, 'Items') as TStringList;
    AWWValues := GetClassProperty(AComponent, 'Values') as TStringList;
    if (AWWItems <> nil) and (AWWValues <> nil) then
    begin
      for I := 0 to AWWItems.Count - 1 do
      begin
        AcxItem := Items.Add as TcxImageComboBoxItem;
        AcxItem.Description := AWWItems[I];
        if I < AWWValues.Count then
          AcxItem.Value := AWWValues[I];
      end;
    end;
    ReadOnly := GetBooleanProperty(AComponent, 'ReadOnly', ReadOnly);
    Images := GetClassProperty(AComponent, 'GlyphImages') as TImageList;
    ShowDescriptions := GetBooleanProperty(AComponent, 'ShowText', ShowDescriptions);
  end;
  AcxColumn.Options.ShowEditButtons := isebAlways;
end;

procedure TcxWWGridConverter.ImportWWSpinEdit(AcxColumn: TcxGridDBBandedColumn; AComponent: TComponent);
begin
  AcxColumn.PropertiesClass := TcxSpinEditProperties;
  with TcxSpinEditProperties(AcxColumn.Properties) do
  begin
    AutoSelect := GetBooleanProperty(AComponent, 'AutoSelect', AutoSelect);
    ReadOnly := GetBooleanProperty(AComponent, 'ReadOnly', ReadOnly);
    MaxValue := GetFloatProperty(AComponent, 'MaxValue', MaxValue);
    MinValue := GetFloatProperty(AComponent, 'MinValue', MinValue);
    Increment := GetFloatProperty(AComponent, 'Increment', Increment);
  end;
end;

procedure TcxWWGridConverter.ImportWWTextEdit(AcxColumn: TcxGridDBBandedColumn; AComponent: TComponent);
begin
  AcxColumn.PropertiesClass := TcxTextEditProperties;
  with TcxTextEditProperties(AcxColumn.Properties) do
  begin
    AutoSelect := GetBooleanProperty(AComponent, 'AutoSelect', AutoSelect);
    ReadOnly := GetBooleanProperty(AComponent, 'ReadOnly', ReadOnly);
    CharCase := GetCharCase(GetEnumProperty(AComponent, 'CharCase'));
    MaxLength := GetIntegerProperty(AComponent, 'MaxLength', MaxLength);
  end;
end;

function TcxWWGridConverter.IsColumnDefault(const AFieldName: string): string;
var
  AControlType: TStringList;
  ADataFieldName: string;
  I: Integer;
begin
  Result := '';
  AControlType := GetClassProperty(nil, 'ControlType') as TStringList;
  if AControlType <> nil then
  begin
    for I := 0 to AControlType.Count - 1 do
    begin
      ExtractData(AControlType[I], ADataFieldName, 1, ';');
      if ADataFieldName = AFieldName then
      begin
        Result := AControlType[I];
        Break;
      end;
    end;
  end;
end;

function TcxWWGridConverter.Pass(const AData: string; AStartIndex: Integer;
  AFieldCount: Integer; ASeparator: Char): Integer;
var
  ACount: Integer;
begin
  Result := AStartIndex;
  ACount := 0;
  while Result <= Length(AData) do
  begin
    if AData[Result] = ASeparator then
    begin
      Inc(ACount);
      if ACount = AFieldCount then
      begin
        Inc(Result);
        Break;
      end;
    end;
    Inc(Result);
  end;
end;

initialization
  ConverterFactory(cxGridGroupConverterName).RegisterConverter('WW InfoPower Grid Converter', TcxWWGridConverter);

end.
