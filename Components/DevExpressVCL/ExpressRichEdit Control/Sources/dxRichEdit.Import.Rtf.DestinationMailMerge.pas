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

unit dxRichEdit.Import.Rtf.DestinationMailMerge;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, SysUtils, Classes, Graphics, Generics.Defaults, Generics.Collections,
  dxCoreClasses, dxCoreGraphics,

  dxRichEdit.DocumentModel.MailMerge,
  dxRichEdit.Import.Rtf;

type
  { TdxMailMergeDestination }

  TdxMailMergeDestination = class(TdxRichEditRtfDestinationBase)
  strict private
    class var
      FKeywordHT: TdxKeywordTranslatorTable;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateKeywordTable: TdxKeywordTranslatorTable; static;
  strict private
    class procedure ConnectionStringKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure QueryKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure DataSourceKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure FieldMapDataKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
  protected
    class function GetControlCharHT: TdxControlCharTranslatorTable; override;
    class function GetKeywordHT: TdxKeywordTranslatorTable; override;
    function CreateClone: TdxRichEditRtfDestinationBase; override;
  end;

  { TdxStringPropertyBaseDestination }

  TdxStringPropertyBaseDestination = class(TdxRichEditRtfDestinationBase)
  strict private
    type
      TPropertyModifier = reference to procedure(const Value: string);
    class var
      FControlCharHT: TdxControlCharTranslatorTable;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateControlCharTable: TdxControlCharTranslatorTable; static;
  strict private
    FValue: TStringBuilder;
    FModifier: TPropertyModifier;
  protected
    class function GetControlCharHT: TdxControlCharTranslatorTable; override;
    class function GetKeywordHT: TdxKeywordTranslatorTable; override;
    procedure ProcessCharCore(ACh: Char); override;
    function CreateClone: TdxRichEditRtfDestinationBase; override;
  public
    constructor Create(AImporter: TdxRtfImporter; const AModifier: TPropertyModifier); reintroduce;
    destructor Destroy; override;

    procedure AfterPopRtfState; override;
  end;

  { TdxFieldMapDataDestination }

  TdxFieldMapDataDestination = class(TdxRichEditRtfDestinationBase)
  strict private
    class var
      FKeywordHT: TdxKeywordTranslatorTable;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateKeywordTable: TdxKeywordTranslatorTable; static;
  strict private
    class function GetFieldMapDataForEdit(AImporter: TdxRtfImporter): TdxFieldMapData; static;
    class procedure NullFieldTypeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure ColumnFieldTypeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure AddressFieldTypeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure SalutationFieldTypeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure MappedFieldTypeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure BarcodeFieldTypeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure ColumnNameKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure MappedNameKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure ColumnIndexKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure DynamicAddressKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure LanguageIdKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
  protected
    class function GetControlCharHT: TdxControlCharTranslatorTable; override;
    class function GetKeywordHT: TdxKeywordTranslatorTable; override;
    function CreateClone: TdxRichEditRtfDestinationBase; override;
  end;

implementation

uses
  Contnrs, dxGenerics,
  dxStringHelper;

{ TdxMailMergeDestination }

class constructor TdxMailMergeDestination.Initialize;
begin
  FKeywordHT := CreateKeywordTable;
end;

class destructor TdxMailMergeDestination.Finalize;
begin
  FreeAndNil(FKeywordHT);
end;

class function TdxMailMergeDestination.CreateKeywordTable: TdxKeywordTranslatorTable;
begin
  Result := TdxKeywordTranslatorTable.Create;
  Result.Add('mmconnectstr', ConnectionStringKeywordHandler);
  Result.Add('mmquery', QueryKeywordHandler);
  Result.Add('mmdatasource', DataSourceKeywordHandler);
  Result.Add('mmodsofldmpdata', FieldMapDataKeywordHandler);
end;

class procedure TdxMailMergeDestination.ConnectionStringKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
var
  AProperties: TdxMailMergeProperties;
begin
  AProperties := AImporter.DocumentModel.MailMergeProperties;
  AImporter.Destination := TdxStringPropertyBaseDestination.Create(AImporter,
    procedure (const AValue: string)
    begin
      AProperties.ConnectionString := AValue;
    end);
end;

class procedure TdxMailMergeDestination.QueryKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
var
  AProperties: TdxMailMergeProperties;
begin
  AProperties := AImporter.DocumentModel.MailMergeProperties;
  AImporter.Destination := TdxStringPropertyBaseDestination.Create(AImporter,
    procedure (const AValue: string)
    begin
      AProperties.Query := AValue;
    end);
end;

class procedure TdxMailMergeDestination.DataSourceKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
var
  AProperties: TdxMailMergeProperties;
begin
  AProperties := AImporter.DocumentModel.MailMergeProperties;
  AImporter.Destination := TdxStringPropertyBaseDestination.Create(AImporter,
    procedure (const AValue: string)
    begin
      AProperties.DataSource := AValue;
    end);
end;

class procedure TdxMailMergeDestination.FieldMapDataKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Destination := TdxFieldMapDataDestination.Create(AImporter);
end;

class function TdxMailMergeDestination.GetControlCharHT: TdxControlCharTranslatorTable;
begin
  Result := nil;
end;

class function TdxMailMergeDestination.GetKeywordHT: TdxKeywordTranslatorTable;
begin
  Result := FKeywordHT;
end;

function TdxMailMergeDestination.CreateClone: TdxRichEditRtfDestinationBase;
begin
  if Self = nil then
    Exit(nil);
  Result := TdxMailMergeDestination.Create(Importer);
end;

{ TdxStringPropertyBaseDestination }

class constructor TdxStringPropertyBaseDestination.Initialize;
begin
  FControlCharHT := CreateControlCharTable;
end;

class destructor TdxStringPropertyBaseDestination.Finalize;
begin
  FreeAndNil(FControlCharHT);
end;

constructor TdxStringPropertyBaseDestination.Create(AImporter: TdxRtfImporter; const AModifier: TPropertyModifier);
begin
  inherited Create(AImporter);
  Assert(Assigned(AModifier));
  FModifier := AModifier;
  FValue := TStringBuilder.Create;
end;

destructor TdxStringPropertyBaseDestination.Destroy;
begin
  FreeAndNil(FValue);
  inherited Destroy;
end;

class function TdxStringPropertyBaseDestination.CreateControlCharTable: TdxControlCharTranslatorTable;
begin
  Result := TdxControlCharTranslatorTable.Create;
  Result.Add('\', EscapedCharHandler);
end;

class function TdxStringPropertyBaseDestination.GetControlCharHT: TdxControlCharTranslatorTable;
begin
  Result := FControlCharHT;
end;

class function TdxStringPropertyBaseDestination.GetKeywordHT: TdxKeywordTranslatorTable;
begin
  Result := nil;
end;

procedure TdxStringPropertyBaseDestination.ProcessCharCore(ACh: Char);
begin
  FValue.Append(ACh);
end;

procedure TdxStringPropertyBaseDestination.AfterPopRtfState;
var
  AValue: string;
begin
  AValue := TdxStringHelper.Trim(FValue.ToString, ['"', ' ']);
  FModifier(AValue);
end;

function TdxStringPropertyBaseDestination.CreateClone: TdxRichEditRtfDestinationBase;
begin
  if Self = nil then
    Exit(nil);
  Result := TdxStringPropertyBaseDestination.Create(Importer, FModifier);
end;

{ TdxFieldMapDataDestination }

class constructor TdxFieldMapDataDestination.Initialize;
begin
  FKeywordHT := CreateKeywordTable;
end;

class destructor TdxFieldMapDataDestination.Finalize;
begin
  FreeAndNil(FKeywordHT);
end;

class function TdxFieldMapDataDestination.CreateKeywordTable: TdxKeywordTranslatorTable;
begin
  Result := TdxKeywordTranslatorTable.Create;
  Result.Add('mmfttypenull', NullFieldTypeKeywordHandler);
  Result.Add('mmfttypedbcolumn', ColumnFieldTypeKeywordHandler);
  Result.Add('mmfttypeaddress', AddressFieldTypeKeywordHandler);
  Result.Add('mmfttypesalutation', SalutationFieldTypeKeywordHandler);
  Result.Add('mmfttypemapped', MappedFieldTypeKeywordHandler);
  Result.Add('mmfttypebarcode', BarcodeFieldTypeKeywordHandler);
  Result.Add('mmodsoname', ColumnNameKeywordHandler);
  Result.Add('mmodsomappedname', MappedNameKeywordHandler);
  Result.Add('mmodsofmcolumn', ColumnIndexKeywordHandler);
  Result.Add('mmodsodynaddr', DynamicAddressKeywordHandler);
  Result.Add('mmodsolid', LanguageIdKeywordHandler);
end;

class function TdxFieldMapDataDestination.GetFieldMapDataForEdit(AImporter: TdxRtfImporter): TdxFieldMapData;
var
  AFieldsMapData: TdxNotificationCollection<TdxFieldMapData>;
begin
  AFieldsMapData := AImporter.DocumentModel.MailMergeProperties.DataSourceObjectProperties.FieldsMapData;
  if AFieldsMapData.Count = 0 then
    AFieldsMapData.Add(TdxFieldMapData.Create);
  Result := AFieldsMapData[AFieldsMapData.Count - 1];
end;

class procedure TdxFieldMapDataDestination.NullFieldTypeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
var
  AFieldMapData: TdxFieldMapData;
begin
  AFieldMapData := GetFieldMapDataForEdit(AImporter);
  AFieldMapData.FieldType := TdxMailMergeFieldType.Null;
end;

class procedure TdxFieldMapDataDestination.ColumnFieldTypeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
var
  AFieldMapData: TdxFieldMapData;
begin
  AFieldMapData := GetFieldMapDataForEdit(AImporter);
  AFieldMapData.FieldType := TdxMailMergeFieldType.DbColumn;
end;

class procedure TdxFieldMapDataDestination.AddressFieldTypeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
end;

class procedure TdxFieldMapDataDestination.SalutationFieldTypeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
end;

class procedure TdxFieldMapDataDestination.MappedFieldTypeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
end;

class procedure TdxFieldMapDataDestination.BarcodeFieldTypeKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
end;

class procedure TdxFieldMapDataDestination.ColumnNameKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
var
  AFieldMapData: TdxFieldMapData;
begin
  AFieldMapData := GetFieldMapDataForEdit(AImporter);
  AImporter.Destination := TdxStringPropertyBaseDestination.Create(AImporter,
    procedure (const AValue: string)
    begin
      AFieldMapData.ColumnName := AValue;
    end);
end;

class procedure TdxFieldMapDataDestination.MappedNameKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
var
  AFieldMapData: TdxFieldMapData;
begin
  AFieldMapData := GetFieldMapDataForEdit(AImporter);
  AImporter.Destination := TdxStringPropertyBaseDestination.Create(AImporter,
    procedure (const AValue: string)
    begin
      AFieldMapData.MappedName := AValue;
    end);
end;

class procedure TdxFieldMapDataDestination.ColumnIndexKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
var
  AFieldMapData: TdxFieldMapData;
begin
  AFieldMapData := GetFieldMapDataForEdit(AImporter);
  if AHasParameter then
    AFieldMapData.ColumnIndex := AParameterValue
  else
    AFieldMapData.ColumnIndex := -1;
end;

class procedure TdxFieldMapDataDestination.DynamicAddressKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
var
  AFieldMapData: TdxFieldMapData;
begin
  if not AHasParameter then
    AParameterValue := 1;
  AFieldMapData := GetFieldMapDataForEdit(AImporter);
  if AParameterValue = 0 then
    AFieldMapData.DynamicAddress := False
  else
    AFieldMapData.DynamicAddress := True;
end;

class procedure TdxFieldMapDataDestination.LanguageIdKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
var
  AFieldMapData: TdxFieldMapData;
begin
  if (not AHasParameter) or (AParameterValue < 0) then
    AParameterValue := 0;
  AFieldMapData := GetFieldMapDataForEdit(AImporter);
  AFieldMapData.MergeFieldNameLanguageId := AParameterValue;
end;

class function TdxFieldMapDataDestination.GetControlCharHT: TdxControlCharTranslatorTable;
begin
  Result := nil;
end;

class function TdxFieldMapDataDestination.GetKeywordHT: TdxKeywordTranslatorTable;
begin
  Result := FKeywordHT;
end;

function TdxFieldMapDataDestination.CreateClone: TdxRichEditRtfDestinationBase;
begin
  if Self = nil then
    Exit(nil);
  Result := TdxFieldMapDataDestination.Create(Importer);
end;

end.
