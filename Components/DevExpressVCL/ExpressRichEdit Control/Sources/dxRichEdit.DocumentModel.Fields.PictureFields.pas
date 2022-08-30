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

unit dxRichEdit.DocumentModel.Fields.PictureFields;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, SysUtils, Generics.Defaults, Generics.Collections,
  dxCore, dxCoreClasses,
  dxGenerics,
  dxRichEdit.Utils.Types,
  dxRichEdit.Utils.OfficeImage,
  dxRichEdit.Options.Simple,
  dxRichEdit.Options,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Fields.Core,
  dxRichEdit.DocumentModel.Fields,
  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.DocumentModel.MailMerge;

type
  { TdxIncludePictureField }

  TdxIncludePictureField = class(TdxCalculatedFieldBase)
  public const
    FieldType = 'INCLUDEPICTURE';
  strict private
    class var
      FSwitchesWithArgument: TdxStringBooleanDictionary;
    class constructor Initialize;
    class destructor Finalize;
  strict private
    FDocumentName: string;
    FGraphicsFilter: string;
    FSuppressStoreGraphicsDataWithDocument: Boolean;
  protected
    function GetSwitchesWithArguments: TdxStringBooleanDictionary; override;
    function GetFieldTypeName: string; override;
    function MailMergeType: TdxFieldMailMergeType; override;
    procedure InsertInlinePictureInTargetModel(AImage: TdxOfficeImageReference;
      ATargetModel: TdxSimpleDocumentModel; ASuppressStore: Boolean);
  public
    class function CreateField: TdxCalculatedFieldBase; override;
    procedure Initialize(APieceTable: TdxCustomPieceTable; AInstructions: TdxInstructionCollection); override;
    function GetAllowedUpdateFieldTypes(const AOptions: TdxFieldUpdateOnLoadOptions): TdxUpdateFieldOperationTypes; override;
    function GetCalculatedValueCore(ASourcePieceTable: TdxCustomPieceTable;
      AMailMergeDataMode: TdxMailMergeDataMode; ADocumentField: TdxField): TdxCalculatedFieldValue; override;

    property DocumentName: string read FDocumentName;
    property GraphicsFilter: string read FGraphicsFilter;
    property SuppressStoreGraphicsDataWithDocument: Boolean read FSuppressStoreGraphicsDataWithDocument;
  end;

  { TdxShapeField }

  TdxShapeField = class(TdxCalculatedFieldBase)
  public const
    FieldType = 'SHAPE';
  strict private
    class var
      FSwitchesWithArgument: TdxStringBooleanDictionary;
    class constructor Initialize;
    class destructor Finalize;
  protected
    function GetSwitchesWithArguments: TdxStringBooleanDictionary; override;
    function GetFieldTypeName: string; override;
  public
    class function CreateField: TdxCalculatedFieldBase; override;
    function GetAllowedUpdateFieldTypes(const AOptions: TdxFieldUpdateOnLoadOptions): TdxUpdateFieldOperationTypes; override;
    function GetCalculatedValueCore(ASourcePieceTable: TdxCustomPieceTable;
      AMailMergeDataMode: TdxMailMergeDataMode; ADocumentField: TdxField): TdxCalculatedFieldValue; override;
    function Update(ASourcePieceTable: TdxCustomPieceTable; AMailMergeDataMode: TdxMailMergeDataMode;
      ADocumentField: TdxField): TdxCalculatedFieldValue; override;
  end;

implementation

uses
  IOUtils, Rtti, Forms,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.FloatingObjectRange,
  dxRichEdit.DocumentModel.InlineObjectRange;

{ TdxIncludePictureField }

class constructor TdxIncludePictureField.Initialize;
begin
  FSwitchesWithArgument := CreateSwitchesWithArgument(TArray<string>.Create('c'));
end;

class destructor TdxIncludePictureField.Finalize;
begin
  FreeAndNil(FSwitchesWithArgument);
end;

class function TdxIncludePictureField.CreateField: TdxCalculatedFieldBase;
begin
  Result := TdxIncludePictureField.Create;
end;

function TdxIncludePictureField.GetSwitchesWithArguments: TdxStringBooleanDictionary;
begin
  Result := FSwitchesWithArgument;
end;

function TdxIncludePictureField.GetFieldTypeName: string;
begin
  Result := FieldType;
end;

procedure TdxIncludePictureField.Initialize(APieceTable: TdxCustomPieceTable; AInstructions: TdxInstructionCollection);
begin
  inherited Initialize(APieceTable, AInstructions);
  FDocumentName := AInstructions.GetArgumentAsString(0);
  FGraphicsFilter := AInstructions.GetString('c').Value;
  FSuppressStoreGraphicsDataWithDocument := AInstructions.GetBool('d');
end;

function TdxIncludePictureField.MailMergeType: TdxFieldMailMergeType;
begin
  Result := TdxFieldMailMergeType.Mixed;
end;

function TdxIncludePictureField.GetAllowedUpdateFieldTypes(const AOptions: TdxFieldUpdateOnLoadOptions): TdxUpdateFieldOperationTypes;
begin
  Result := inherited GetAllowedUpdateFieldTypes(AOptions);
  if FSuppressStoreGraphicsDataWithDocument then
    Result := Result + [TdxUpdateFieldOperationType.Copy, TdxUpdateFieldOperationType.Load,
      TdxUpdateFieldOperationType.PasteFromIE, TdxUpdateFieldOperationType.CreateModelForExport];
end;

function TdxIncludePictureField.GetCalculatedValueCore(ASourcePieceTable: TdxCustomPieceTable;
  AMailMergeDataMode: TdxMailMergeDataMode; ADocumentField: TdxField): TdxCalculatedFieldValue;
var
  ADocumentModel: TdxDocumentModel;
  AUriList: TdxStringList;
  ACurrentFileName: string;
  AImage: TdxUriOfficeImage;
  ATargetModel: TdxDocumentModel;
  ADocumentName: string;
begin
  ADocumentModel := TdxPieceTable(ASourcePieceTable).DocumentModel;
  AUriList := TdxStringList.Create;
  try
    AUriList.Add(FDocumentName);
    ADocumentName := StringReplace(FDocumentName, '\\', '\', [rfReplaceAll]);
    AUriList.Add(ADocumentName);
    ACurrentFileName := ADocumentModel.DocumentSaveOptions.CurrentFileName;
    try
      if (ACurrentFileName = '') and (Application <> nil) then
        ACurrentFileName := Application.ExeName;
      if (ACurrentFileName <> '') and TPath.HasValidPathChars(FDocumentName, True) then
      begin
        AUriList.Add(TPath.Combine(TPath.GetDirectoryName(ACurrentFileName), FDocumentName));
        AUriList.Add(TPath.Combine(TPath.GetDirectoryName(ACurrentFileName), ADocumentName));
      end;
    except
    end;
    AImage := TdxUriOfficeImage.Create(ADocumentModel.ImageCache, ADocumentModel,
      AUriList.ToArray, 0, 0, True);
    try
      AImage.SuppressStorePlaceholder := True;
      ATargetModel := ADocumentModel.GetFieldResultModel;
      InsertInlinePictureInTargetModel(AImage, ATargetModel, FSuppressStoreGraphicsDataWithDocument and
        (AMailMergeDataMode <> TdxMailMergeDataMode.FinalMerging));
      Result := TdxCalculatedFieldValue.Create(TValue.From<TdxDocumentModel>(ATargetModel));
    finally
      AImage.Free;
    end;
  finally
    AUriList.Free;
  end;
end;

procedure TdxIncludePictureField.InsertInlinePictureInTargetModel(AImage: TdxOfficeImageReference;
  ATargetModel: TdxSimpleDocumentModel; ASuppressStore: Boolean);
begin
  AImage.SuppressStore := ASuppressStore;
  ATargetModel.MainPieceTable.InsertInlinePicture(0, AImage);
end;

{ TdxShapeField }

class constructor TdxShapeField.Initialize;
begin
  FSwitchesWithArgument := TdxStringBooleanDictionary.Create;
end;

class destructor TdxShapeField.Finalize;
begin
  FreeAndNil(FSwitchesWithArgument);
end;

class function TdxShapeField.CreateField: TdxCalculatedFieldBase;
begin
  Result := TdxShapeField.Create;
end;

function TdxShapeField.GetSwitchesWithArguments: TdxStringBooleanDictionary;
begin
  Result := FSwitchesWithArgument;
end;

function TdxShapeField.GetFieldTypeName: string;
begin
  Result := FieldType;
end;

function TdxShapeField.GetCalculatedValueCore(ASourcePieceTable: TdxCustomPieceTable;
  AMailMergeDataMode: TdxMailMergeDataMode; ADocumentField: TdxField): TdxCalculatedFieldValue;
begin
  Result := TdxCalculatedFieldValue.NullValue;
end;

function TdxShapeField.Update(ASourcePieceTable: TdxCustomPieceTable;
  AMailMergeDataMode: TdxMailMergeDataMode; ADocumentField: TdxField): TdxCalculatedFieldValue;
var
  ATable: TdxPieceTable absolute ASourcePieceTable;
  AStartRunIndex, AEndRunIndex: TdxRunIndex;
  ARuns: TdxTextRunCollection;
  AAnchorRun: TdxFloatingObjectAnchorRun;
  AInlinePicture: TdxInlinePictureRun;
  ADocumentModel: TdxDocumentModel;
begin
  Result := TdxCalculatedFieldValue.Create(nil, [TdxFieldResultOption.KeepOldResult]);
  AStartRunIndex := ADocumentField.Result.Start;
  AEndRunIndex := ADocumentField.Result.&End - 1;
  ARuns := ATable.Runs;
  if (AEndRunIndex <= AStartRunIndex) or ((AEndRunIndex - AStartRunIndex) > 1) then
    Exit;
  AAnchorRun := Safe<TdxFloatingObjectAnchorRun>.Cast(ARuns[AStartRunIndex]);
  AInlinePicture := Safe<TdxInlinePictureRun>.Cast(ARuns[AEndRunIndex]);
  if (AAnchorRun = nil) or (AInlinePicture = nil) or
      not AAnchorRun.FloatingObjectProperties.PseudoInline or
      not AInlinePicture.Properties.PseudoInline then
    Exit;
  ADocumentModel := ATable.DocumentModel;
  ADocumentModel.BeginUpdate;
  try
    ADocumentModel.UnsafeEditor.DeleteRuns(ATable, AEndRunIndex, 1);
  finally
    ADocumentModel.EndUpdate;
  end;
end;

function TdxShapeField.GetAllowedUpdateFieldTypes(const AOptions: TdxFieldUpdateOnLoadOptions): TdxUpdateFieldOperationTypes;
begin
  Result := [TdxUpdateFieldOperationType.Load];
end;

end.
