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

unit dxRichEdit.DocumentModel.Fields.DocVariableField;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, SysUtils, Generics.Defaults, Generics.Collections, Rtti,
  dxCore, dxCoreClasses,
  dxRichEdit.Utils.Types,
  dxRichEdit.NativeApi,
  dxRichEdit.Types,
  dxRichEdit.Options.Simple,
  dxGenerics,
  dxRichEdit.Utils.Token,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.MailMerge,
  dxRichEdit.DocumentModel.Fields.Core,
  dxRichEdit.DocumentModel.Fields;

type
  { TdxDocVariableField }

  TdxDocVariableField = class(TdxCalculatedFieldBase)
  public const
    FieldType = 'DOCVARIABLE';
  strict private
    class var
      FSwitchesWithArgument: TdxStringBooleanDictionary;
    class constructor Initialize;
    class destructor Finalize;
  strict private
    FArguments: TdxArgumentCollection;
    FIsUpdateFieldOperationTypeLoad: Boolean;
    FVariableName: string;
  protected
    function GetSwitchesWithArguments: TdxStringBooleanDictionary; override;
    function GetFieldTypeName: string; override;
    function MailMergeType: TdxFieldMailMergeType; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    class function CreateField: TdxCalculatedFieldBase; override;
    procedure Initialize(APieceTable: TdxCustomPieceTable; AInstructions: TdxInstructionCollection); override;
    function GetAllowedUpdateFieldTypes(const AOptions: TdxFieldUpdateOnLoadOptions): TdxUpdateFieldOperationTypes; override;
    function GetCalculatedValueCore(ASourcePieceTable: TdxCustomPieceTable;
      AMailMergeDataMode: TdxMailMergeDataMode; ADocumentField: TdxField): TdxCalculatedFieldValue; override;
    function TryToGetValueDocumentModel(const AValue: TValue): TdxCustomDocumentModel;
  end;

implementation

uses
  dxRichEdit.Api.NativeDocumentBase,
  dxRichEdit.InnerControl,
  dxRichEdit.InternalRichEditDocumentServer,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.Commands;

{ TdxDocVariableField }

class constructor TdxDocVariableField.Initialize;
begin
  FSwitchesWithArgument := CreateSwitchesWithArgument;
end;

class destructor TdxDocVariableField.Finalize;
begin
  FreeAndNil(FSwitchesWithArgument);
end;

constructor TdxDocVariableField.Create;
begin
  inherited Create;
  FIsUpdateFieldOperationTypeLoad := True;
  FArguments := TdxArgumentCollection.Create;
end;

destructor TdxDocVariableField.Destroy;
begin
  FreeAndNil(FArguments);
  inherited Destroy;
end;

class function TdxDocVariableField.CreateField: TdxCalculatedFieldBase;
begin
  Result := TdxDocVariableField.Create;
end;

function TdxDocVariableField.GetSwitchesWithArguments: TdxStringBooleanDictionary;
begin
  Result := FSwitchesWithArgument;
end;

function TdxDocVariableField.GetFieldTypeName: string;
begin
  Result := FieldType;
end;

procedure TdxDocVariableField.Initialize(APieceTable: TdxCustomPieceTable; AInstructions: TdxInstructionCollection);
var
  ATokens: TdxTokenList;
  ACount, I: Integer;
begin
  inherited Initialize(APieceTable, AInstructions);

  FVariableName := AInstructions.GetArgumentAsString(0);
  FArguments.Clear;
  ATokens := AInstructions.Arguments;
  ACount := ATokens.Count;
  for I := 1 to ACount - 1 do
    FArguments.Add(TdxArgument.Create(ATokens[I]));
end;

function TdxDocVariableField.MailMergeType: TdxFieldMailMergeType;
begin
  Result := TdxFieldMailMergeType.Mixed;
end;

function TdxDocVariableField.GetAllowedUpdateFieldTypes(const AOptions: TdxFieldUpdateOnLoadOptions): TdxUpdateFieldOperationTypes;
begin
  Result := [TdxUpdateFieldOperationType.Copy, TdxUpdateFieldOperationType.Normal,
    TdxUpdateFieldOperationType.CreateModelForExport];
  if FIsUpdateFieldOperationTypeLoad then
    Include(Result, TdxUpdateFieldOperationType.Load);
end;

function TdxDocVariableField.GetCalculatedValueCore(ASourcePieceTable: TdxCustomPieceTable;
  AMailMergeDataMode: TdxMailMergeDataMode; ADocumentField: TdxField): TdxCalculatedFieldValue;
var
  ATable: TdxPieceTable absolute ASourcePieceTable;
  AValue: TValue;
  AValueDocumentModel, ATargetModel: TdxDocumentModel;
  ACommand: TdxPieceTableInsertContentConvertedToDocumentModelCommand;
begin
  AValue := ATable.DocumentModel.Variables.GetVariableValue(FVariableName, FArguments);
  AValueDocumentModel := TdxDocumentModel(TryToGetValueDocumentModel(AValue));
  if AValueDocumentModel = nil then
  begin
    if AValue.IsEmpty then
      FIsUpdateFieldOperationTypeLoad := False;
    Exit(TdxCalculatedFieldValue.Create(AValue));
  end;
  ATargetModel := ATable.DocumentModel.GetFieldResultModel;
  ACommand := TdxPieceTableInsertContentConvertedToDocumentModelCommand.Create(ATargetModel.MainPieceTable,
    AValueDocumentModel, 0, False);
  try
    ACommand.CopyBetweenInternalModels := True;
    ACommand.Execute;
  finally
    ACommand.Free;
  end;

  Result := TdxCalculatedFieldValue.Create(TValue.From<TdxDocumentModel>(ATargetModel),
    [TdxFieldResultOption.DoNotApplyFieldCodeFormatting, TdxFieldResultOption.SuppressMergeUseFirstParagraphStyle]);
end;

function TdxDocVariableField.TryToGetValueDocumentModel(const AValue: TValue): TdxCustomDocumentModel;
var
  AServer: TdxInternalRichEditDocumentServer;
  ADocument: TdxNativeSubDocumentBase;
  AObject: TObject;
begin
  Result := nil;
  AServer := TdxInternalRichEditDocumentServer.TryConvertInternalRichEditDocumentServer(AValue);
  if AServer <> nil then
    Exit(AServer.InnerServer.DocumentModel);

  if not (AValue.IsObject or AValue.IsInterface) then
    Exit;

  if AValue.IsObject then
    AObject := AValue.AsObject
  else
    AObject := TObject(AValue.AsInterface);

  ADocument := Safe<TdxNativeSubDocumentBase>.Cast(AObject);
  if ADocument <> nil then
    Exit(ADocument.DocumentModel);
  Result := Safe<TdxCustomDocumentModel>.Cast(AObject);
end;

end.
