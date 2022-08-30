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

unit dxRichEdit.DocumentModel.Fields.MergefieldField;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, SysUtils, Generics.Defaults, Generics.Collections,
  dxCoreClasses,
  dxRichEdit.Options,
  dxGenerics,
  dxRichEdit.Utils.Types,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Fields.Core,
  dxRichEdit.DocumentModel.Fields,
  dxRichEdit.DocumentModel.MailMerge;

type
  IdxDataFieldNameOwner = interface
    function GetDataFieldName: string;
    property DataFieldName: string read GetDataFieldName;
  end;


  { TdxMergefieldField }

  TdxMergefieldField = class(TdxCalculatedFieldBase, IdxDataFieldNameOwner)
  public const
    FieldType = 'MERGEFIELD';
  strict private
    class var
      FSwitchesWithArgument: TdxStringBooleanDictionary;
    class constructor Initialize;
    class destructor Finalize;
  strict private
    FDataFieldName: string;
    FTextBeforeIfFieldNotBlank: string;
    FTextAfterIfFieldNotBlank: string;
    FMappedField: Boolean;
    FEnableConversionForVerticalFormatting: Boolean;
  protected
    function GetSwitchesWithArguments: TdxStringBooleanDictionary; override;
    function GetFieldTypeName: string; override;
    function MailMergeType: TdxFieldMailMergeType; override;
    function GetNullValue: TdxCalculatedFieldValue; virtual;
    //IdxDataFieldNameOwner
    function GetDataFieldName: string;

    class property SwitchesWithArgument: TdxStringBooleanDictionary read FSwitchesWithArgument;
  public
    class function CreateField: TdxCalculatedFieldBase; override;
    procedure Initialize(APieceTable: TdxCustomPieceTable; AInstructions: TdxInstructionCollection); override;
    function GetCalculatedValueCore(ASourcePieceTable: TdxCustomPieceTable; AMailMergeDataMode: TdxMailMergeDataMode; ADocumentField: TdxField): TdxCalculatedFieldValue; override;

    property DataFieldName: string read GetDataFieldName;
    property TextBeforeIfFieldNotBlank: string read FTextBeforeIfFieldNotBlank;
    property TextAfterIfFieldNotBlank: string read FTextAfterIfFieldNotBlank;
    property MappedField: Boolean read FMappedField;
    property EnableConversionForVerticalFormatting: Boolean read FEnableConversionForVerticalFormatting;
  end;

implementation

uses
  Rtti,
  dxRichEdit.DocumentModel.PieceTable,
  dxStringHelper;

{ TdxMergefieldField }

class constructor TdxMergefieldField.Initialize;
begin
  FSwitchesWithArgument := CreateSwitchesWithArgument(TArray<string>.Create('b', 'f'));
end;

class destructor TdxMergefieldField.Finalize;
begin
  FreeAndNil(FSwitchesWithArgument);
end;

class function TdxMergefieldField.CreateField: TdxCalculatedFieldBase;
begin
  Result := TdxMergefieldField.Create;
end;

function TdxMergefieldField.GetSwitchesWithArguments: TdxStringBooleanDictionary;
begin
  Result := FSwitchesWithArgument;
end;

function TdxMergefieldField.GetFieldTypeName: string;
begin
  Result := FieldType;
end;

procedure TdxMergefieldField.Initialize(APieceTable: TdxCustomPieceTable; AInstructions: TdxInstructionCollection);
begin
  inherited Initialize(APieceTable, AInstructions);
  FDataFieldName := AInstructions.GetArgumentAsString(0);
  FTextBeforeIfFieldNotBlank := AInstructions.GetString('b').Value;
  FTextAfterIfFieldNotBlank := AInstructions.GetString('f').Value;
  FMappedField := AInstructions.GetBool('m');
  FEnableConversionForVerticalFormatting := AInstructions.GetBool('v');
end;

function TdxMergefieldField.MailMergeType: TdxFieldMailMergeType;
begin
  Result := TdxFieldMailMergeType.MailMerge;
end;

function TdxMergefieldField.GetCalculatedValueCore(ASourcePieceTable: TdxCustomPieceTable; AMailMergeDataMode: TdxMailMergeDataMode; ADocumentField: TdxField): TdxCalculatedFieldValue;
var
  AFieldDataService: IdxFieldDataService;
  AValue: TValue;
  ATable: TdxPieceTable absolute ASourcePieceTable;
begin
  AFieldDataService := ATable.DocumentModel.GetService<IdxFieldDataService>;
  Assert(AFieldDataService <> nil);
  AValue := AFieldDataService.GetFieldValue(ATable.DocumentModel.MailMergeProperties, FDataFieldName, FMappedField, AMailMergeDataMode, ATable, ADocumentField);
  if AValue.IsEmpty then
    Exit(GetNullValue);
  if (TextBeforeIfFieldNotBlank = '') and (TextAfterIfFieldNotBlank = '') then
    Exit(TdxCalculatedFieldValue.Create(AValue));
  Result := TdxCalculatedFieldValue.Create(FTextBeforeIfFieldNotBlank + AValue.ToString + FTextAfterIfFieldNotBlank);
end;

function TdxMergefieldField.GetNullValue: TdxCalculatedFieldValue;
begin
  Result := TdxCalculatedFieldValue.Create('');
end;

function TdxMergefieldField.GetDataFieldName: string;
begin
  Result := FDataFieldName;
end;

end.
