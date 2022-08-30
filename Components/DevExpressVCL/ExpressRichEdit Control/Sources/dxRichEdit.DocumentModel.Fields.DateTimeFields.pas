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

unit dxRichEdit.DocumentModel.Fields.DateTimeFields;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, SysUtils, Generics.Defaults, Generics.Collections,
  dxCoreClasses,
  dxRichEdit.Options.Simple,
  dxGenerics,
  dxRichEdit.Utils.Types,
  dxRichEdit.LayoutEngine.Formatter,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Section,
  dxRichEdit.DocumentModel.Fields.Core,
  dxRichEdit.DocumentModel.Fields,
  dxRichEdit.DocumentModel.DateTimeFieldFormatter,
  dxRichEdit.DocumentModel.MailMerge;

type
  { TdxDateField }

  TdxDateField = class(TdxCalculatedFieldBase)
  public const
    FieldType = 'DATE';
  strict private
    class var
      FSwitchesWithArgument: TdxStringBooleanDictionary;
    class constructor Initialize;
    class destructor Finalize;
  strict private
    FUseLunarCalendar: Boolean;
    FUseSakaEraCalendar: Boolean;
    FUseLastFormat: Boolean;
  protected
    function GetSwitchesWithArguments: TdxStringBooleanDictionary; override;
    function GetFieldTypeName: string; override;
  public
    class function CreateField: TdxCalculatedFieldBase; override;
    procedure Initialize(APieceTable: TdxCustomPieceTable; ASwitches: TdxInstructionCollection); override;
    function GetCalculatedValueCore(ASourcePieceTable: TdxCustomPieceTable; AMailMergeDataMode: TdxMailMergeDataMode; ADocumentField: TdxField): TdxCalculatedFieldValue; override;

    // for internal use
    function GetAllowedUpdateFieldTypes(const AOptions: TdxFieldUpdateOnLoadOptions): TdxUpdateFieldOperationTypes; override;

    property UseLunarCalendar: Boolean read FUseLunarCalendar;
    property UseSakaEraCalendar: Boolean read FUseSakaEraCalendar;
    property UseLastFormat: Boolean read FUseLastFormat;
  end;

  { TdxTimeFieldFormatter }

  TdxTimeFieldFormatter = class(TdxDateTimeFieldFormatter)
  strict private const
    DefaultFormat = 'h:mm am/pm';
  protected
    function FormatByDefault(const AValue: TDateTime): string; override;
  end;

  { TdxTimeField }

  TdxTimeField = class(TdxCalculatedFieldBase)
  public const
    FieldType = 'TIME';
  strict private
    class var
      FSwitchesWithArgument: TdxStringBooleanDictionary;
    class constructor Initialize;
    class destructor Finalize;
  protected
    function GetSwitchesWithArguments: TdxStringBooleanDictionary; override;
    function GetFieldTypeName: string; override;
    function CreateDateTimeFieldFormatter: TdxDateTimeFieldFormatter; override;
  public
    class function CreateField: TdxCalculatedFieldBase; override;
    function GetCalculatedValueCore(ASourcePieceTable: TdxCustomPieceTable; AMailMergeDataMode: TdxMailMergeDataMode; ADocumentField: TdxField): TdxCalculatedFieldValue; override;

    // for internal use
    function GetAllowedUpdateFieldTypes(const AOptions: TdxFieldUpdateOnLoadOptions): TdxUpdateFieldOperationTypes; override;
  end;

  { TdxCreateDateField }

  TdxCreateDateField = class(TdxCalculatedFieldBase)
  public const
    FieldType = 'CREATEDATE';
  strict private
    class var
      FSwitchesWithArgument: TdxStringBooleanDictionary;
    class constructor Initialize;
    class destructor Finalize;
  strict private
    FUseLunarCalendar: Boolean;
    FUseSakaEraCalendar: Boolean;
  protected
    function GetSwitchesWithArguments: TdxStringBooleanDictionary; override;
    function GetFieldTypeName: string; override;
    function MailMergeType: TdxFieldMailMergeType; override;
  public
    class function CreateField: TdxCalculatedFieldBase; override;
    procedure Initialize(APieceTable: TdxCustomPieceTable; AInstructions: TdxInstructionCollection); override;
    function GetCalculatedValueCore(ASourcePieceTable: TdxCustomPieceTable;
      AMailMergeDataMode: TdxMailMergeDataMode; ADocumentField: TdxField): TdxCalculatedFieldValue; override;

    property UseLunarCalendar: Boolean read FUseLunarCalendar;
    property UseSakaEraCalendar: Boolean read FUseSakaEraCalendar;
  end;

implementation

uses
  Rtti,
  dxRichEdit.DocumentModel.PieceTable;

{ TdxDateField }

class constructor TdxDateField.Initialize;
begin
  FSwitchesWithArgument := TdxStringBooleanDictionary.Create;
end;

class destructor TdxDateField.Finalize;
begin
  FreeAndNil(FSwitchesWithArgument);
end;

class function TdxDateField.CreateField: TdxCalculatedFieldBase;
begin
  Result := TdxDateField.Create;
end;

function TdxDateField.GetSwitchesWithArguments: TdxStringBooleanDictionary;
begin
  Result := FSwitchesWithArgument;
end;

function TdxDateField.GetFieldTypeName: string;
begin
  Result := FieldType;
end;

procedure TdxDateField.Initialize(APieceTable: TdxCustomPieceTable; ASwitches: TdxInstructionCollection);
begin
  inherited Initialize(APieceTable, ASwitches);
  FUseLunarCalendar := ASwitches.GetBool('h');
  FUseSakaEraCalendar := ASwitches.GetBool('s');
  FUseLastFormat := ASwitches.GetBool('l');
end;

function TdxDateField.GetCalculatedValueCore(ASourcePieceTable: TdxCustomPieceTable; AMailMergeDataMode: TdxMailMergeDataMode; ADocumentField: TdxField): TdxCalculatedFieldValue;
var
  ADateTimeService: IdxDateTimeService;
  AResult: TDateTime;
begin
  ADateTimeService := TdxPieceTable(ASourcePieceTable).DocumentModel.GetService<IdxDateTimeService>;
  if ADateTimeService <> nil then
    AResult := ADateTimeService.GetCurrentDateTime
  else
    AResult := Now;
  Result := TdxCalculatedFieldValue.Create(TValue.From<TDateTime>(AResult));
end;

function TdxDateField.GetAllowedUpdateFieldTypes(const AOptions: TdxFieldUpdateOnLoadOptions): TdxUpdateFieldOperationTypes;
begin
  Result := inherited GetAllowedUpdateFieldTypes(AOptions);
  if (AOptions <> nil) and AOptions.UpdateDateField then
    Result := Result + [TdxUpdateFieldOperationType.Load];
end;

{ TdxTimeFieldFormatter }

function TdxTimeFieldFormatter.FormatByDefault(const AValue: TDateTime): string;
begin
  if not UseCurrentCultureDateTimeFormat then
    Exit(InternalFormat(AValue, DefaultFormat));
  Result := TimeToStr(AValue);
end;

{ TdxTimeField }

class constructor TdxTimeField.Initialize;
begin
  FSwitchesWithArgument := TdxStringBooleanDictionary.Create;
end;

class destructor TdxTimeField.Finalize;
begin
  FreeAndNil(FSwitchesWithArgument);
end;

class function TdxTimeField.CreateField: TdxCalculatedFieldBase;
begin
  Result := TdxTimeField.Create;
end;

function TdxTimeField.GetSwitchesWithArguments: TdxStringBooleanDictionary;
begin
  Result := FSwitchesWithArgument;
end;

function TdxTimeField.GetFieldTypeName: string;
begin
  Result := FieldType;
end;

function TdxTimeField.GetCalculatedValueCore(ASourcePieceTable: TdxCustomPieceTable; AMailMergeDataMode: TdxMailMergeDataMode; ADocumentField: TdxField): TdxCalculatedFieldValue;
begin
  Result := TdxCalculatedFieldValue.Create(TValue.From<TDateTime>(Now));
end;

function TdxTimeField.CreateDateTimeFieldFormatter: TdxDateTimeFieldFormatter;
begin
  Result := TdxTimeFieldFormatter.Create;
  Result.UseCurrentCultureDateTimeFormat := UseCurrentCultureDateTimeFormat;
end;

function TdxTimeField.GetAllowedUpdateFieldTypes(const AOptions: TdxFieldUpdateOnLoadOptions): TdxUpdateFieldOperationTypes;
begin
  Result := inherited GetAllowedUpdateFieldTypes(AOptions);
  if (AOptions <> nil) and AOptions.UpdateTimeField then
    Result := Result + [TdxUpdateFieldOperationType.Load];
end;

{ TdxCreateDateField }

class constructor TdxCreateDateField.Initialize;
begin
  FSwitchesWithArgument := CreateSwitchesWithArgument;
end;

class destructor TdxCreateDateField.Finalize;
begin
  FreeAndNil(FSwitchesWithArgument);
end;

class function TdxCreateDateField.CreateField: TdxCalculatedFieldBase;
begin
  Result := TdxCreateDateField.Create;
end;

function TdxCreateDateField.GetSwitchesWithArguments: TdxStringBooleanDictionary;
begin
  Result := FSwitchesWithArgument;
end;

function TdxCreateDateField.GetFieldTypeName: string;
begin
  Result := FieldType;
end;

procedure TdxCreateDateField.Initialize(APieceTable: TdxCustomPieceTable; AInstructions: TdxInstructionCollection);
begin
  inherited Initialize(APieceTable, AInstructions);
  FUseLunarCalendar := AInstructions.GetBool('h');
  FUseSakaEraCalendar := AInstructions.GetBool('s');
end;

function TdxCreateDateField.MailMergeType: TdxFieldMailMergeType;
begin
  Result := TdxFieldMailMergeType.Mixed;
end;

function TdxCreateDateField.GetCalculatedValueCore(ASourcePieceTable: TdxCustomPieceTable;
  AMailMergeDataMode: TdxMailMergeDataMode; ADocumentField: TdxField): TdxCalculatedFieldValue;
begin
  Result := TdxCalculatedFieldValue.Create(TValue.From<TDateTime>(Now));
end;

end.

