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

unit dxRichEdit.DocumentModel.Fields.SymbolField;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, SysUtils, Generics.Defaults, Generics.Collections,
  dxCoreClasses,
  dxRichEdit.Options.Simple,
  dxRichEdit.Options,
  dxGenerics,
  dxRichEdit.Utils.Types,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.MailMerge,
  dxRichEdit.DocumentModel.Fields.Core,
  dxRichEdit.DocumentModel.Fields;

type
  { TdxSymbolField }

  TdxSymbolField = class(TdxCalculatedFieldBase)
  public const
    FieldType = 'SYMBOL';
  strict private
    class var
      FSwitchesWithArgument: TdxStringBooleanDictionary;
    class constructor Initialize;
    class destructor Finalize;
  strict private
    FCharacterCode: Integer;
    FIsAnsiCharacter: Boolean;
    FIsUnicodeCharacter: Boolean;
    FFontName: string;
    FDoubleFontSize: Integer;
  protected
    function GetSwitchesWithArguments: TdxStringBooleanDictionary; override;
    function GetFieldTypeName: string; override;
    function GetCharacterFormatFlag: TdxFieldResultOptions; override;
  public
    class function CreateField: TdxCalculatedFieldBase; override;
    procedure Initialize(APieceTable: TdxCustomPieceTable; ASwitches: TdxInstructionCollection); override;
    function GetAllowedUpdateFieldTypes(const AOptions: TdxFieldUpdateOnLoadOptions): TdxUpdateFieldOperationTypes; override;
    function GetCalculatedValueCore(ASourcePieceTable: TdxCustomPieceTable;
      AMailMergeDataMode: TdxMailMergeDataMode; ADocumentField: TdxField): TdxCalculatedFieldValue; override;
    function GetCharacterCode(const AArgument: string): Integer;
    function GetUnicodeCharacterCode: Integer;

    property CharacterCode: Integer read FCharacterCode;
    property IsAnsiCharacter: Boolean read FIsAnsiCharacter;
    property IsUnicodeCharacter: Boolean read FIsUnicodeCharacter;
    property FontName: string read FFontName;
    property DoubleFontSize: Integer read FDoubleFontSize;
  end;

implementation

uses
  Rtti,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.TextRange,
  dxEncoding,
  dxStringHelper;

{ TdxSymbolField }

class constructor TdxSymbolField.Initialize;
begin
  FSwitchesWithArgument := CreateSwitchesWithArgument(TArray<string>.Create('f', 's'));
end;

class destructor TdxSymbolField.Finalize;
begin
  FreeAndNil(FSwitchesWithArgument);
end;

class function TdxSymbolField.CreateField: TdxCalculatedFieldBase;
begin
  Result := TdxSymbolField.Create;
end;

function TdxSymbolField.GetSwitchesWithArguments: TdxStringBooleanDictionary;
begin
  Result := FSwitchesWithArgument;
end;

function TdxSymbolField.GetFieldTypeName: string;
begin
  Result := FieldType;
end;

procedure TdxSymbolField.Initialize(APieceTable: TdxCustomPieceTable; ASwitches: TdxInstructionCollection);
var
  AArgument: string;
begin
  inherited Initialize(APieceTable, ASwitches);

  AArgument := ASwitches.GetArgumentAsString(0);
  FCharacterCode := GetCharacterCode(AArgument);
  FFontName := ASwitches.GetString('f').Value;
  FDoubleFontSize := ASwitches.GetInt('s') * 2;
  FIsAnsiCharacter := ASwitches.GetBool('a');
  FIsUnicodeCharacter := ASwitches.GetBool('u');
end;

function TdxSymbolField.GetCharacterCode(const AArgument: string): Integer;
begin
  if TdxStringHelper.StartsWith(AArgument, '0x', False) then
  begin
    if TryStrToInt(Format('$%s', [Copy(AArgument, 3, Length(AArgument))]), Result) then
      Exit;
  end
  else
  begin
    if TryStrToInt(AArgument, Result) then
      Exit;
  end;
  Result := -1;
end;

function TdxSymbolField.GetCalculatedValueCore(ASourcePieceTable: TdxCustomPieceTable;
  AMailMergeDataMode: TdxMailMergeDataMode; ADocumentField: TdxField): TdxCalculatedFieldValue;
var
  ATable: TdxPieceTable absolute ASourcePieceTable;
  AUnicodeCharacterCode: Integer;
  ACharacter: Char;
  ATargetModel: TdxDocumentModel;
  ARun: TdxTextRun;
begin
  AUnicodeCharacterCode := GetUnicodeCharacterCode;
  if AUnicodeCharacterCode < 0 then
    Exit(TdxCalculatedFieldValue.NullValue);
  ACharacter := Char(AUnicodeCharacterCode);
  ATargetModel := ATable.DocumentModel.GetFieldResultModel;
  ATargetModel.MainPieceTable.InsertFieldSymbolResult(0, ACharacter);
  ARun := TdxTextRun(ATargetModel.MainPieceTable.LastInsertedRunInfo.Run);
  if FontName <> '' then
    ARun.FontName := FontName;
  if DoubleFontSize > 0 then
    ARun.DoubleFontSize := DoubleFontSize;
  Result := TdxCalculatedFieldValue.Create(TValue.From<TdxDocumentModel>(ATargetModel));
end;

function TdxSymbolField.GetUnicodeCharacterCode: Integer;
var
  ABytes: TArray<Byte>;
  AChars: TArray<Char>;
begin
  if IsUnicodeCharacter then
  begin
    if (CharacterCode < 0) or (CharacterCode > $FFFF) then
      Result := -1
    else
      Result := CharacterCode;
  end
  else
  begin
    if (CharacterCode < 0) or (CharacterCode > $FF) then
      Exit(-1);
    ABytes := TEncoding.Convert(TdxEncoding.GetEncoding(1252), TEncoding.UTF8, TArray<Byte>.Create(Byte(CharacterCode)));
    AChars := TEncoding.UTF8.GetChars(ABytes);
    if Length(AChars) <> 1 then
      Result := -1
    else
      Result := Ord(AChars[0]);
  end;
end;

function TdxSymbolField.GetAllowedUpdateFieldTypes(const AOptions: TdxFieldUpdateOnLoadOptions): TdxUpdateFieldOperationTypes;
begin
  Result := inherited GetAllowedUpdateFieldTypes(AOptions) +
    [TdxUpdateFieldOperationType.Load, TdxUpdateFieldOperationType.Copy, TdxUpdateFieldOperationType.CreateModelForExport];
end;

function TdxSymbolField.GetCharacterFormatFlag: TdxFieldResultOptions;
begin
  Result := inherited GetCharacterFormatFlag +
    [TdxFieldResultOption.DoNotApplyFieldCodeFormatting];
end;

end.
