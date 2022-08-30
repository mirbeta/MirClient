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

unit dxRichEdit.DocumentModel.Fields.NumPagesField;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, SysUtils, Generics.Defaults, Generics.Collections,
  dxCoreClasses,
  dxGenerics,
  dxRichEdit.Options,
  dxRichEdit.Options.Simple,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Fields.Core,
  dxRichEdit.DocumentModel.Fields,
  dxRichEdit.DocumentModel.MailMerge;

type
  { TdxNumPagesFieldResultFormatting }

  TdxNumPagesFieldResultFormatting = class(TdxFieldResultFormatting)
  protected
    function GetRecalculateOnSecondaryFormatting: Boolean; override;
    function GetValueCore(AFormatter: TObject; ADocumentModel: TdxCustomDocumentModel): Integer; override;
  end;

  { TdxNumPagesField }

  TdxNumPagesField = class(TdxCalculatedFieldBase)
  public const
    FieldType = 'NUMPAGES';
  strict private
    class var
      FSwitchesWithArgument: TdxStringBooleanDictionary;
  strict private
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
    function ShouldInsertLayoutDependentRun(ASourcePieceTable: TdxCustomPieceTable): Boolean;
    function GetTopLevelPieceTable(ASourcePieceTable: TdxCustomPieceTable): TdxCustomPieceTable;
    procedure ApplyFieldFormatting(AValue: TdxCalculatedFieldValue; ACustomSeparators: TdxMailMergeCustomSeparators); override;
  end;

implementation

uses
  Rtti,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.FloatingObjectRange,
  dxRichEdit.DocumentModel.Section;

{ TdxNumPagesFieldResultFormatting }

function TdxNumPagesFieldResultFormatting.GetRecalculateOnSecondaryFormatting: Boolean;
begin
  Result := True;
end;

function TdxNumPagesFieldResultFormatting.GetValueCore(AFormatter: TObject; ADocumentModel: TdxCustomDocumentModel): Integer;
begin
  Result := TdxDocumentModel(ADocumentModel).ExtendedDocumentProperties.Pages;
end;

{ TdxNumPagesField }

class constructor TdxNumPagesField.Initialize;
begin
  FSwitchesWithArgument := CreateSwitchesWithArgument;
end;

class destructor TdxNumPagesField.Finalize;
begin
  FreeAndNil(FSwitchesWithArgument);
end;

class function TdxNumPagesField.CreateField: TdxCalculatedFieldBase;
begin
  Result := TdxNumPagesField.Create;
end;

function TdxNumPagesField.GetSwitchesWithArguments: TdxStringBooleanDictionary;
begin
  Result := FSwitchesWithArgument;
end;

function TdxNumPagesField.GetFieldTypeName: string;
begin
  Result := FieldType;
end;

function TdxNumPagesField.GetCalculatedValueCore(ASourcePieceTable: TdxCustomPieceTable;
  AMailMergeDataMode: TdxMailMergeDataMode; ADocumentField: TdxField): TdxCalculatedFieldValue;
var
  ATargetModel: TdxDocumentModel;
begin
  if ShouldInsertLayoutDependentRun(TdxPieceTable(ASourcePieceTable)) then
  begin
    ATargetModel := TdxPieceTable(ASourcePieceTable).DocumentModel.GetFieldResultModel;
    ATargetModel.BeginUpdate;
    try
      ATargetModel.MainPieceTable.InsertLayoutDependentTextRun(0, 0,
        TdxNumPagesFieldResultFormatting.Create(NumericFormatting, GeneralFormatting));
    finally
      ATargetModel.EndUpdate;
    end;
    Result := TdxCalculatedFieldValue.Create(TValue.From<TdxDocumentModel>(ATargetModel));
  end
  else
    Result := TdxCalculatedFieldValue.Create(TdxPieceTable(ASourcePieceTable).DocumentModel.ExtendedDocumentProperties.Pages);
end;

function TdxNumPagesField.ShouldInsertLayoutDependentRun(ASourcePieceTable: TdxCustomPieceTable): Boolean;
var
  ATopLevelPieceTable: TdxCustomPieceTable;
begin
  ATopLevelPieceTable := GetTopLevelPieceTable(ASourcePieceTable);
  Result := ATopLevelPieceTable.ContentType is TdxSectionHeaderFooterBase;
end;

function TdxNumPagesField.GetTopLevelPieceTable(ASourcePieceTable: TdxCustomPieceTable): TdxCustomPieceTable;
var
  ATextBox: TdxTextBoxContentType;
begin
  if ASourcePieceTable.ContentType is TdxTextBoxContentType then
  begin
    ATextBox := TdxTextBoxContentType(ASourcePieceTable.ContentType);
    Result := TdxPieceTable(ATextBox.AnchorRun.PieceTable);
  end
  else
    Result := ASourcePieceTable;
end;

procedure TdxNumPagesField.ApplyFieldFormatting(AValue: TdxCalculatedFieldValue; ACustomSeparators: TdxMailMergeCustomSeparators);
begin
  if AValue.IsDocumentModelValue then
    Exit;
  inherited ApplyFieldFormatting(AValue, ACustomSeparators);
end;

function TdxNumPagesField.GetAllowedUpdateFieldTypes(const AOptions: TdxFieldUpdateOnLoadOptions): TdxUpdateFieldOperationTypes;
begin
  Result := [TdxUpdateFieldOperationType.Copy, TdxUpdateFieldOperationType.Load,
    TdxUpdateFieldOperationType.Normal, TdxUpdateFieldOperationType.PasteFromIE,
    TdxUpdateFieldOperationType.CreateModelForExport];
end;

end.
