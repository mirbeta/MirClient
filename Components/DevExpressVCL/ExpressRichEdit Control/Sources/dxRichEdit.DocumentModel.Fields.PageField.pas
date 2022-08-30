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

unit dxRichEdit.DocumentModel.Fields.PageField;

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
  dxRichEdit.LayoutEngine.Formatter,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.SectionFormatting,
  dxRichEdit.DocumentModel.Fields.Core,
  dxRichEdit.DocumentModel.Fields,
  dxRichEdit.DocumentModel.Boxes,
  dxRichEdit.DocumentModel.MailMerge;

type
  { TdxPageFieldResultFormatting }

  TdxPageFieldResultFormatting = class(TdxFieldResultFormatting)
  strict private
    class var
      FEmptyGenericFormatting: TArray<string>;
    class constructor Initialize;
  protected
    class function GetPageNumbering(AFormatter: TdxParagraphBoxFormatter): TdxSectionPageNumbering; static;
    function GetGeneralFormatting: TArray<string>; override;
    function GetValueCore(AFormatter: TObject{TdxParagraphBoxFormatter}; ADocumentModel: TdxCustomDocumentModel): Integer; override;
    function GetPageOrdinal(AFormatter: TdxParagraphBoxFormatter): Integer; virtual;
    function GetRecalculateOnSecondaryFormatting: Boolean; override;
  public
    function ApplyImplicitFormatting(AFormatter: TObject; const AValue: string;
      AIntValue: Integer): string; override;
  end;

  { TdxPageField }

  TdxPageField = class(TdxCalculatedFieldBase)
  public const
    FieldType = 'PAGE';
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
    procedure ApplyFieldFormatting(AValue: TdxCalculatedFieldValue;
      ACustomSeparators: TdxMailMergeCustomSeparators); override;
    function GetAllowedUpdateFieldTypes(const AOptions: TdxFieldUpdateOnLoadOptions): TdxUpdateFieldOperationTypes; override;
    function GetCalculatedValueCore(ASourcePieceTable: TdxCustomPieceTable; AMailMergeDataMode: TdxMailMergeDataMode;
      ADocumentField: TdxField): TdxCalculatedFieldValue; override;
    function GetPageIndex(APieceTable: TdxCustomPieceTable; ALogPosition: TdxDocumentLogPosition): Integer; overload;
    function GetPageIndex(APieceTable: TdxCustomPieceTable; ADocumentLayout: TdxDocumentLayout;
      ALogPosition: TdxDocumentLogPosition): Integer; overload;
  end;

implementation

uses
  Rtti,
  cxClasses,
  dxRichEdit.InternalRichEditDocumentServer,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentLayout,
  dxRichEdit.DocumentLayout.Position,
  dxRichEdit.DocumentModel.NumberingFormatting;

{ TdxPageFieldResultFormatting }

class constructor TdxPageFieldResultFormatting.Initialize;
begin
  FEmptyGenericFormatting := TArray<string>.Create();
end;

function TdxPageFieldResultFormatting.GetRecalculateOnSecondaryFormatting: Boolean;
begin
  Result := False;
end;

function TdxPageFieldResultFormatting.GetGeneralFormatting: TArray<string>;
begin
  Result := inherited GetGeneralFormatting;
  if Result = nil then
    Result := FEmptyGenericFormatting;
end;

function TdxPageFieldResultFormatting.GetValueCore(AFormatter: TObject;
  ADocumentModel: TdxCustomDocumentModel): Integer;
begin
  Result := GetPageOrdinal(TdxParagraphBoxFormatter(AFormatter));
end;

class function TdxPageFieldResultFormatting.GetPageNumbering(AFormatter: TdxParagraphBoxFormatter): TdxSectionPageNumbering;
begin
  Result := AFormatter.RowsController.ColumnController.PageAreaController.PageController.CurrentSection.PageNumbering;
end;

function TdxPageFieldResultFormatting.ApplyImplicitFormatting(AFormatter: TObject; const AValue: string; AIntValue: Integer): string;
var
  APageNumbering: TdxSectionPageNumbering;
  AConverter: TdxOrdinalBasedNumberConverter;
begin
  APageNumbering := GetPageNumbering(TdxParagraphBoxFormatter(AFormatter));
  AConverter := TdxOrdinalBasedNumberConverter.CreateConverter(APageNumbering.NumberingFormat, TdxLanguageId.English);
  try
    Result := AConverter.ConvertNumber(AIntValue);
  finally
    AConverter.Free;
  end;
end;

function TdxPageFieldResultFormatting.GetPageOrdinal(AFormatter: TdxParagraphBoxFormatter): Integer;
begin
  if AFormatter.PageNumberSource <> nil then
    Exit(AFormatter.PageNumberSource.PageOrdinal)
  else
    Exit(AFormatter.RowsController.ColumnController.PageAreaController.PageController.Pages.Count);
end;

{ TdxPageField }

class constructor TdxPageField.Initialize;
begin
  FSwitchesWithArgument := CreateSwitchesWithArgument;
end;

class destructor TdxPageField.Finalize;
begin
  FreeAndNil(FSwitchesWithArgument);
end;

class function TdxPageField.CreateField: TdxCalculatedFieldBase;
begin
  Result := TdxPageField.Create;
end;

function TdxPageField.GetSwitchesWithArguments: TdxStringBooleanDictionary;
begin
  Result := FSwitchesWithArgument;
end;

function TdxPageField.GetFieldTypeName: string;
begin
  Result := FieldType;
end;

function TdxPageField.GetCalculatedValueCore(ASourcePieceTable: TdxCustomPieceTable;
  AMailMergeDataMode: TdxMailMergeDataMode; ADocumentField: TdxField): TdxCalculatedFieldValue;
var
  ALogPosition: TdxDocumentLogPosition;
  APageIndex: Integer;
  ATargetModel: TdxDocumentModel;
  AFormatting: TdxPageFieldResultFormatting;
begin
  ALogPosition := TdxDocumentModelPosition.FromRunStart(TdxPieceTable(ASourcePieceTable),
    ADocumentField.FirstRunIndex).LogPosition;
  APageIndex := GetPageIndex(TdxPieceTable(ASourcePieceTable), ALogPosition);
  if APageIndex >= 0 then
    Exit(TdxCalculatedFieldValue.Create(IntToStr(APageIndex)));
  ATargetModel := TdxPieceTable(ASourcePieceTable).DocumentModel.GetFieldResultModel;
  ATargetModel.BeginUpdate;
  try
    AFormatting := TdxPageFieldResultFormatting.Create(NumericFormatting, GeneralFormatting);
    ATargetModel.MainPieceTable.InsertLayoutDependentTextRun(0, 0, AFormatting);
  finally
    ATargetModel.EndUpdate;
  end;
  Result := TdxCalculatedFieldValue.Create(TValue.From<TdxDocumentModel>(ATargetModel));
end;

function TdxPageField.GetPageIndex(APieceTable: TdxCustomPieceTable; ALogPosition: TdxDocumentLogPosition): Integer;
var
  ADocumentLayoutService: IdxDocumentLayoutService;
  ADocumentLayout: TdxDocumentLayout;
begin
  ADocumentLayoutService := APieceTable.DocumentModel.GetService<IdxDocumentLayoutService>;
  if APieceTable.IsMain and (ADocumentLayoutService <> nil) then
  begin
    ADocumentLayout := ADocumentLayoutService.CalculateDocumentLayout;
    Result := GetPageIndex(APieceTable, ADocumentLayout, ALogPosition);
  end
  else
    Result := -1;
end;

function TdxPageField.GetPageIndex(APieceTable: TdxCustomPieceTable; ADocumentLayout: TdxDocumentLayout;
  ALogPosition: TdxDocumentLogPosition): Integer;
var
  ALayoutPosition: TdxDocumentLayoutPosition;
begin
  ALayoutPosition := TdxDocumentLayoutPosition(ADocumentLayout.CreateLayoutPosition(APieceTable, ALogPosition, -1));
  ALayoutPosition.Update(ADocumentLayout.Pages, TdxDocumentLayoutDetailsLevel.Page);
  if ALayoutPosition.IsValid(TdxDocumentLayoutDetailsLevel.Page) then
    Result := ALayoutPosition.Page.PageOrdinal
  else
    Result := -1;
end;

function TdxPageField.GetAllowedUpdateFieldTypes(const AOptions: TdxFieldUpdateOnLoadOptions): TdxUpdateFieldOperationTypes;
begin
  Result := [TdxUpdateFieldOperationType.Copy, TdxUpdateFieldOperationType.Load,
    TdxUpdateFieldOperationType.Normal, TdxUpdateFieldOperationType.PasteFromIE,
    TdxUpdateFieldOperationType.CreateModelForExport];
end;

procedure TdxPageField.ApplyFieldFormatting(AValue: TdxCalculatedFieldValue; ACustomSeparators: TdxMailMergeCustomSeparators);
begin
end;

end.
