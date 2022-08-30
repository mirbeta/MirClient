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
unit cxDBGridConverter;

interface

{$I cxVer.inc}

uses
  Classes, SysUtils, cxConverterFactory, cxGridConverter, cxConverterUtils,
  cxGridDBTableView, cxGridCustomView, DB, cxEdit, cxGraphics, cxButtonEdit,
  cxDropDownEdit, cxTextEdit, cxDBLookupComboBox, cxStyles, Graphics, cxCustomConverter;

type

  { TcxDBGridConverter }
  TcxDBGridConverter = class(TcxCustomGridConverter)
  private
    FColor: Integer;
    FFixedColor: Integer;
    FFont: TFont;
    FTitleFont: TFont;
    function FindcxColumn(const AFieldName: string): TcxGridDBColumn;
    function GetcxGridView: TcxGridDBTableView;
    function GetFieldName(AColumn: TCollectionItem): string;
    procedure ImportColumns;
    procedure ImportColumnsStyles;
    procedure ImportGrid;
    procedure ImportGridStyles;
  protected
    procedure DoRealImport; override;
    procedure DoImportStyles; override;
    function GetGridViewClass: TcxCustomGridViewClass; override;
    property cxGridView: TcxGridDBTableView read GetcxGridView;
  public
    class function GetSourceClassName: string; override;
  end;

implementation

uses cxGridTableView, Windows;

type
  TcxCustomEditPropertiesAccessor = class(TcxCustomEditProperties);

{ TcxDBGridConverter }

class function TcxDBGridConverter.GetSourceClassName: string;
begin
  Result := 'TDBGrid';
end;

procedure TcxDBGridConverter.DoRealImport;
begin
  cxGridView.DataController.DataSource := GetClassProperty(nil, 'DataSource') as TDataSource;
  ImportColumns;
  ImportGrid;
end;

procedure TcxDBGridConverter.DoImportStyles;
begin
  ImportGridStyles;
  ImportColumnsStyles;
end;

function TcxDBGridConverter.GetGridViewClass: TcxCustomGridViewClass;
begin
  Result := TcxGridDBTableView;
end;

function TcxDBGridConverter.FindcxColumn(const AFieldName: string): TcxGridDBColumn;
var
  I: Integer;
begin
  Result := nil;
  with cxGridView do
  begin
    for I := 0 to ColumnCount do
      if Columns[I].DataBinding.FieldName = AFieldName then
      begin
        Result := Columns[I];
        Exit;
      end;
  end;
end;

function TcxDBGridConverter.GetcxGridView: TcxGridDBTableView;
begin
  Result := inherited cxGridView as TcxGridDBTableView;
end;

function TcxDBGridConverter.GetFieldName(AColumn: TCollectionItem): string;
begin
  Result := GetWideStringProperty(AColumn, 'FieldName', 'Field' + IntToStr(AColumn.Index));
end;

{function TcxDBGridConverter.GetDefaultColumnName: string;
begin
  Result := 'DBColumn';
end;}

procedure TcxDBGridConverter.ImportColumns;
var
  AColumns: TObject;
  AObject: TObject;
  ATitle: TObject;
  AcxColumn: TcxGridDBColumn;
  AButtonStyle: string;
  AReadOnly: Boolean;
  AAlignment: TAlignment;
  AFieldName: string;
  I, J: Integer;
  ADataSource: TDataSource;
  AField: TField;
  AButtonEditProperties: TcxButtonEditProperties;
  AComboBoxProperties: TcxComboBoxProperties;
  ALookupComboBoxProperties: TcxLookupComboBoxProperties;
begin
  AColumns := GetClassProperty(nil, 'Columns');
  if AColumns <> nil then
  begin
    if AColumns is TCollection then
    with TCollection(AColumns) do
    begin
      for I := 0 to Count - 1 do
      begin
        AcxColumn := cxGridView.CreateColumn;
        AFieldName := GetFieldName(Items[I]);
        AButtonStyle := GetEnumProperty(Items[I], 'ButtonStyle');
        if AButtonStyle = 'cbsAuto' then
        begin
          ADataSource := GetClassProperty(nil, 'DataSource') as TDataSource;
          AField := nil;
          if ADataSource <> nil then
          begin
            if ADataSource.DataSet <> nil then
            with ADataSource.DataSet.Fields do
            begin
              for J := 0 to Count - 1 do
                if Fields[J].FieldName = AFieldName then
                begin
                  AField := Fields[J];
                  Break;
                end;
            end;
          end;
          if (AField <> nil) and (AField.FieldKind = fkLookup) then
          begin
            AcxColumn.PropertiesClass := TcxLookupComboBoxProperties;
            ALookupComboBoxProperties := TcxLookupComboBoxProperties(AcxColumn.Properties);
            ALookupComboBoxProperties.DropDownRows := GetIntegerProperty(Items[I],
              'DropDownRows', ALookupComboBoxProperties.DropDownRows);
            ALookupComboBoxProperties.ReadOnly := GetBooleanProperty(Items[I],
              'ReadOnly', ALookupComboBoxProperties.ReadOnly);
          end
          else
          begin
            AObject := GetClassProperty(Items[I], 'PickList');
            if AObject <> nil then
            begin
              if (AObject as TStrings).Count > 0 then
              begin
                AcxColumn.PropertiesClass := TcxComboBoxProperties;
                AComboBoxProperties := TcxComboBoxProperties(AcxColumn.Properties);
                for J := 0 to (AObject as TStrings).Count - 1 do
                  AComboBoxProperties.Items.Add((AObject as TStrings)[J]);
                AComboBoxProperties.ReadOnly := GetBooleanProperty(
                  TCollection(AColumns).Items[I], 'ReadOnly', AComboBoxProperties.ReadOnly);
                AComboBoxProperties.Alignment.Horz :=
                  ConvertAlignment(GetEnumProperty(TCollection(AColumns).Items[I], 'Alignment'));
                AComboBoxProperties.DropDownRows :=
                  GetIntegerProperty(TCollection(AColumns).Items[I], 'DropDownRows', AComboBoxProperties.DropDownRows);
              end
              else
              begin
                AReadOnly := GetBooleanProperty(Items[I], 'ReadOnly', False);
                AAlignment := ConvertAlignment(GetEnumProperty(Items[I], 'Alignment'));
                if AReadOnly or (AAlignment <> taLeftJustify) then
                begin
                  AcxColumn.PropertiesClass := TcxTextEditProperties;
                  TcxTextEditProperties(AcxColumn.Properties).ReadOnly := AReadOnly;
                  TcxTextEditProperties(AcxColumn.Properties).Alignment.Horz := AAlignment;
                end;
              end;
            end;
          end;
        end
        else if AButtonStyle = 'cbsEllipsis' then
        begin
          AcxColumn.PropertiesClass := TcxButtonEditProperties;
          AButtonEditProperties := TcxButtonEditProperties(AcxColumn.Properties);
          AButtonEditProperties.ReadOnly := GetBooleanProperty(Items[I],
            'ReadOnly', AButtonEditProperties.ReadOnly);
          AButtonEditProperties.Alignment.Horz :=
            ConvertAlignment(GetEnumProperty(Items[I], 'Alignment'));
        end
        else
        begin
          AReadOnly := GetBooleanProperty(Items[I], 'ReadOnly', False);
          AAlignment := ConvertAlignment(GetEnumProperty(Items[I], 'Alignment'));
          if AReadOnly or (AAlignment <> taLeftJustify) then
          begin
            AcxColumn.PropertiesClass := TcxTextEditProperties;
            TcxTextEditProperties(AcxColumn.Properties).ReadOnly := AReadOnly;
            TcxTextEditProperties(AcxColumn.Properties).Alignment.Horz := AAlignment;
          end;
        end;
        AcxColumn.DataBinding.FieldName := AFieldName;
        AcxColumn.Name := UniqueColumnName(AcxColumn, AcxColumn.DataBinding.FieldName);
        ATitle := GetClassProperty(Items[I], 'Title');
        if ATitle <> nil then
        begin
          AcxColumn.Caption := GetStringProperty(ATitle, 'Caption', AcxColumn.Caption);
          AcxColumn.HeaderAlignmentHorz := ConvertAlignment(GetEnumProperty(ATitle, 'Alignment'));
        end;
        AcxColumn.Visible := GetBooleanProperty(Items[I], 'Visible', AcxColumn.Visible);
        AcxColumn.Width := GetIntegerProperty(Items[I], 'Width', AcxColumn.Width);
      end;
    end;
  end;
end;

procedure TcxDBGridConverter.ImportColumnsStyles;
var
  AColumns: TObject;
  AColumnTitle: TObject;
  AcxColumn: TcxGridDBColumn;
  I: Integer;
  AStyle: TcxCustomStyle;
  AColor: Integer;
  AFont: TFont;
begin
  AColumns := GetClassProperty(nil, 'Columns');
  if AColumns <> nil then
  begin
    if AColumns is TCollection then
    with TCollection(AColumns) do
    begin
      for I := 0 to Count - 1 do
      begin
        AcxColumn := FindcxColumn(GetFieldName(Items[I]));
        if AcxColumn <> nil then
        begin
          AColor := GetIntegerProperty(Items[I], 'Color');
          AFont := GetClassProperty(Items[I], 'Font') as TFont;
          if ((AColor <> clWindow) and (AColor <> FColor)) or
            (not DefaultFont(AFont) and not CompareFonts(AFont, FFont)) then
          begin
            AStyle := CreateStyleItem;
            (AStyle as TcxStyle).Color := AColor;
            (AStyle as TcxStyle).Font.Assign(AFont);
            (AStyle as TcxStyle).TextColor := (AStyle as TcxStyle).Font.Color;
            AcxColumn.Styles.Content := AStyle as TcxStyle;
          end;
          AColumnTitle := GetClassProperty(Items[I], 'Title');
          if AColumnTitle <> nil then
          begin
            AColor := GetIntegerProperty(AColumnTitle, 'Color');
            AFont := GetClassProperty(AColumnTitle, 'Font') as TFont;
            if ((AColor <> clBtnFace) and (AColor <> FFixedColor)) or
              (not DefaultFont(AFont) and not CompareFonts(AFont, FTitleFont)) then
            begin
              AStyle := CreateStyleItem;
              (AStyle as TcxStyle).Color := AColor;
              (AStyle as TcxStyle).Font.Assign(AFont);
              (AStyle as TcxStyle).TextColor := (AStyle as TcxStyle).Font.Color;
              AcxColumn.Styles.Header := AStyle as TcxStyle;
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TcxDBGridConverter.ImportGrid;
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
        OptionsData.Editing := AOptions.Find('dgEditing', I);
        OptionsBehavior.AlwaysShowEditor := AOptions.Find('dgAlwaysShowEditor', I);
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
        OptionsData.DeletingConfirmation := AOptions.Find('dgConfirmDelete', I);
        OptionsData.CancelOnExit := AOptions.Find('dgCancelOnExit', I);
        OptionsSelection.MultiSelect := AOptions.Find('dgMultiSelect', I);
        OptionsSelection.CellSelect := not AOptions.Find('dgRowSelect', I);
        OptionsView.Header := AOptions.Find('dgTitles', I);
        OptionsBehavior.FocusCellOnTab := AOptions.Find('dgTabs', I);
        OptionsSelection.HideSelection :=  not AOptions.Find('dgAlwaysShowSelection', I);
      end;
    except
      on EcxUnknownProperty do;
    end;
  finally
    AOptions.Free;
    DisablePropertyException;
  end;
end;

procedure TcxDBGridConverter.ImportGridStyles;
var
  AStyle: TcxCustomStyle;
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
  FFixedColor := GetIntegerProperty(nil, 'FixedColor');
  FTitleFont := GetClassProperty(nil, 'TitleFont') as TFont;
  if (FFixedColor <> clBtnFace) or not DefaultFont(FTitleFont) then
  begin
    AStyle := CreateStyleItem;
    (AStyle as TcxStyle).Color := FFixedColor;
    (AStyle as TcxStyle).Font.Assign(FTitleFont);
    (AStyle as TcxStyle).TextColor := (AStyle as TcxStyle).Font.Color;
    cxGridView.Styles.Header := AStyle as TcxStyle;
    cxGridView.Styles.Indicator := AStyle as TcxStyle;
  end;
end;

initialization
  ConverterFactory(cxGridGroupConverterName).RegisterConverter('DB Grid Converter', TcxDBGridConverter);

end.
