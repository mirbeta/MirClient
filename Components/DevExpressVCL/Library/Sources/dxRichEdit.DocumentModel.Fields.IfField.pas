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

unit dxRichEdit.DocumentModel.Fields.IfField;

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
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.MailMerge,
  dxRichEdit.DocumentModel.Fields.Core,
  dxRichEdit.DocumentModel.Fields;

type
  { TdxIfField }

  TdxIfField = class(TdxCalculatedFieldBase)
  public const
    FieldType = 'IF';
  private const
    EqualOperator = '=';
    NotEqualOperator = '<>';
    LessOperator = '<';
    LessOrEqualOperator = '<=';
    GreaterOperator = '>';
    GreaterOrEqualOperator = '>=';
  strict private
    class var
      FSwitchesWithArgument: TdxStringBooleanDictionary;
    class constructor Initialize;
    class destructor Finalize;
  strict private
    FExpression1: string;
    FCmpOperator: string;
    FExpression2: string;
    FTrueResult: TdxDocumentLogInterval;
    FFalseResult: TdxDocumentLogInterval;
  protected
    function GetSwitchesWithArguments: TdxStringBooleanDictionary; override;
    function GetFieldTypeName: string; override;
    function MailMergeType: TdxFieldMailMergeType; override;
    function GetCharacterFormatFlag: TdxFieldResultOptions; override;
    procedure InsertResult(ASourcePieceTable: TdxPieceTable; ATargetModel: TdxDocumentModel; const ASourceInterval: TdxDocumentLogInterval);
    function Compare: Boolean; virtual;
    function GetNumericValue(const AExpression: string): Single;
    function CompareText: Boolean;
    function CompareTextUsingRegExp: Boolean;
    function CompareNumericExpression(AValue1: Single; AValue2: Single): Boolean;
    function CompareExpressionCore(ACompareResult: Single): Boolean;
  public
    class function CreateField: TdxCalculatedFieldBase; override;
    procedure Initialize(APieceTable: TdxCustomPieceTable; AInstructions: TdxInstructionCollection); override;
    function GetCalculatedValueCore(ASourcePieceTable: TdxCustomPieceTable;
      AMailMergeDataMode: TdxMailMergeDataMode; ADocumentField: TdxField): TdxCalculatedFieldValue; override;

    property Expression1: string read FExpression1;
    property CmpOperator: string read FCmpOperator;
    property Expression2: string read FExpression2;
    property TrueResult: TdxDocumentLogInterval read FTrueResult;
    property FalseResult: TdxDocumentLogInterval read FFalseResult;
  end;

implementation

uses
  Math, Rtti,
  dxStringHelper,
  dxRichEdit.DocumentModel.Commands;

{ TdxIfField }

class constructor TdxIfField.Initialize;
begin
  FSwitchesWithArgument := CreateSwitchesWithArgument;
end;

class destructor TdxIfField.Finalize;
begin
  FreeAndNil(FSwitchesWithArgument);
end;

class function TdxIfField.CreateField: TdxCalculatedFieldBase;
begin
  Result := TdxIfField.Create;
end;

function TdxIfField.GetSwitchesWithArguments: TdxStringBooleanDictionary;
begin
  Result := FSwitchesWithArgument;
end;

function TdxIfField.GetFieldTypeName: string;
begin
  Result := FieldType;
end;

procedure TdxIfField.Initialize(APieceTable: TdxCustomPieceTable; AInstructions: TdxInstructionCollection);
begin
  inherited Initialize(APieceTable, AInstructions);
  FExpression1 := AInstructions.GetArgumentAsString(0);
  FCmpOperator := AInstructions.GetArgumentAsString(1);
  FExpression2 := AInstructions.GetArgumentAsString(2);
  FTrueResult := AInstructions.GetArgumentAsDocumentInterval(3);
  if AInstructions.Arguments.Count >= 5 then
    FFalseResult := AInstructions.GetArgumentAsDocumentInterval(4)
  else
    FFalseResult := TdxDocumentLogInterval.Null;
end;

function TdxIfField.MailMergeType: TdxFieldMailMergeType;
begin
  Result := TdxFieldMailMergeType.Mixed;
end;

function TdxIfField.GetCalculatedValueCore(ASourcePieceTable: TdxCustomPieceTable; AMailMergeDataMode: TdxMailMergeDataMode; ADocumentField: TdxField): TdxCalculatedFieldValue;
var
  ATargetModel: TdxDocumentModel;
begin
  ATargetModel := TdxPieceTable(ASourcePieceTable).DocumentModel.GetFieldResultModel;
  if Compare then
    InsertResult(TdxPieceTable(ASourcePieceTable), ATargetModel, FTrueResult)
  else
    if FFalseResult.IsValid then
      InsertResult(TdxPieceTable(ASourcePieceTable), ATargetModel, FFalseResult);
  Result := TdxCalculatedFieldValue.Create(TValue.From<TdxDocumentModel>(ATargetModel));
end;

function TdxIfField.GetCharacterFormatFlag: TdxFieldResultOptions;
begin
  Result := inherited GetCharacterFormatFlag;
  if Result = [] then
    Result := [TdxFieldResultOption.DoNotApplyFieldCodeFormatting];
end;

procedure TdxIfField.InsertResult(ASourcePieceTable: TdxPieceTable; ATargetModel: TdxDocumentModel;
  const ASourceInterval: TdxDocumentLogInterval);
var
  AOptions: TdxDocumentModelCopyOptions;
  ACopyCommand: TdxDocumentModelCopyCommand;
begin
  AOptions := TdxDocumentModelCopyOptions.Create(ASourceInterval.Start, ASourceInterval.Length);
  try
    AOptions.CopyDocumentVariables := True;
    ACopyCommand := TdxDocumentModelCopyCommand(ASourcePieceTable.DocumentModel.CreateDocumentModelCopyCommand(ASourcePieceTable, ATargetModel, AOptions));
    try
      ACopyCommand.Execute;
    finally
      ACopyCommand.Free;
    end;
  finally
    AOptions.Free;
  end;
end;

function TdxIfField.Compare: Boolean;
var
  ANumericValue1, ANumericValue2: Single;
begin
  ANumericValue1 := GetNumericValue(FExpression1);
  if IsNaN(ANumericValue1) then
    Exit(CompareText);

  ANumericValue2 := GetNumericValue(FExpression2);
  if IsNaN(ANumericValue2) then
    Exit(CompareText);

  Result := CompareNumericExpression(ANumericValue1, ANumericValue2);
end;

function TdxIfField.GetNumericValue(const AExpression: string): Single;
begin
  if not TryStrToFloat(AExpression, Result) then
    Result := NaN;
end;

function TdxIfField.CompareText: Boolean;
begin
  if (TdxStringHelper.IndexOfAny(FExpression2, ['*', '?']) >= 0) and
      ((CmpOperator = EqualOperator) or (CmpOperator = NotEqualOperator)) then
    Result := CompareTextUsingRegExp
  else
    Result := CompareExpressionCore(CompareStr(FExpression1, FExpression2));
end;

function TdxIfField.CompareTextUsingRegExp: Boolean;
begin
  Result := FExpression1 = FExpression2;
end;

function TdxIfField.CompareNumericExpression(AValue1: Single; AValue2: Single): Boolean;
begin
  Result := CompareExpressionCore(AValue1 - AValue2);
end;

function TdxIfField.CompareExpressionCore(ACompareResult: Single): Boolean;
begin
  if CmpOperator = EqualOperator then
    Result := ACompareResult = 0
  else if CmpOperator = NotEqualOperator then
    Result := ACompareResult <> 0
  else if CmpOperator = GreaterOrEqualOperator then
    Result := ACompareResult >= 0
  else if CmpOperator = GreaterOperator then
    Result := ACompareResult > 0
  else if CmpOperator = LessOperator then
    Result := ACompareResult < 0
  else if CmpOperator = LessOrEqualOperator then
    Result := ACompareResult <= 0
  else
    Result := False;
end;

end.
