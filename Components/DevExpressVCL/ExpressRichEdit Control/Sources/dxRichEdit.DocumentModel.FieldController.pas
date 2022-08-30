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

unit dxRichEdit.DocumentModel.FieldController;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  dxCore,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.DocumentModel.Selections.Core,
  dxRichEdit.DocumentModel.Fields.Core,
  dxRichEdit.Control.HitTest;

type
  { TdxFieldController }

  TdxFieldController = class
  public
    class function FindFieldByHitTestResult(AHitTestResult: TdxRichEditHitTestResult): TdxField;
    class function FindParentFieldByType<T: class>(APieceTable: TdxSimplePieceTable; AField: TdxField; out AParsedInfo: T): TdxField;
    class function FindFieldBySelection<T: class>(ASelection: TdxSimpleSelection; out AParsedInfo: T): TdxField; overload;
    class function FindFieldBySelection(ASelection: TdxSimpleSelection): TdxField; overload;
    class function FindTopmostFieldBySelection(ASelection: TdxSimpleSelection): TdxField;
  end;

implementation

uses
  Math,
  dxRichEdit.DocumentModel.FieldCalculatorService,
  dxRichEdit.DocumentModel.Fields;

{ TdxFieldController }

class function TdxFieldController.FindFieldByHitTestResult(AHitTestResult: TdxRichEditHitTestResult): TdxField;
begin
  Assert(AHitTestResult <> nil, 'hitTestResult');
  if AHitTestResult.DetailsLevel < TdxDocumentLayoutDetailsLevel.Box then
    Exit(nil);
  Result := TdxSimplePieceTable(AHitTestResult.PieceTable).FindFieldByRunIndex(AHitTestResult.Box.StartPos.RunIndex);
end;

class function TdxFieldController.FindFieldBySelection<T>(ASelection: TdxSimpleSelection; out AParsedInfo: T): TdxField;
var
  AField: TdxField;
begin
  AParsedInfo := nil;
  AField := FindFieldBySelection(ASelection);
  if AField = nil then
    Result := nil
  else
    Result := FindParentFieldByType<T>(TdxSimplePieceTable(ASelection.PieceTable), AField, AParsedInfo);
end;

class function TdxFieldController.FindParentFieldByType<T>(APieceTable: TdxSimplePieceTable; AField: TdxField; out AParsedInfo: T): TdxField;
var
  AParseField: TdxCalculatedFieldBase;
  AParser: TdxFieldCalculatorService;
begin
  AParsedInfo := nil;
  AParser := TdxFieldCalculatorService.Create;
  try
    while AField <> nil do
    begin
      AParseField := AParser.ParseField(APieceTable, AField);
      AParsedInfo := Safe<T>.Cast(AParseField);
      if AParsedInfo <> nil then
        Exit(AField);
      AParseField.Free;
      AField := AField.Parent;
    end;
  finally
    AParser.Free;
  end;
  Result := nil;
end;

class function TdxFieldController.FindFieldBySelection(ASelection: TdxSimpleSelection): TdxField;
var
  APieceTable: TdxSimplePieceTable;
  AStart, AEnd: TdxDocumentModelPosition;
  AStartField, AEndField: TdxField;
begin
  APieceTable := TdxSimplePieceTable(ASelection.PieceTable);
  AStart := TdxPositionConverter.ToDocumentModelPosition(APieceTable, Min(ASelection.NormalizedStart, APieceTable.DocumentEndLogPosition));
  AStartField := APieceTable.FindFieldByRunIndex(AStart.RunIndex);
  if AStartField = nil then
    Exit(nil);
  AEnd := TdxPositionConverter.ToDocumentModelPosition(APieceTable, Min(ASelection.NormalizedEnd, APieceTable.DocumentEndLogPosition));
  if (AEnd.RunOffset = 0) and (AEnd.RunIndex = AStartField.LastRunIndex + 1) then
    Exit(AStartField);
  AEndField := APieceTable.FindFieldByRunIndex(AEnd.RunIndex);
  if AStartField = AEndField then
    Result := AStartField
  else
    Result := nil;
end;

class function TdxFieldController.FindTopmostFieldBySelection(ASelection: TdxSimpleSelection): TdxField;
var
  APieceTable: TdxSimplePieceTable;
  AStart, AEnd: TdxDocumentModelPosition;
  AStartField, AEndField: TdxField;
begin
  APieceTable := TdxSimplePieceTable(ASelection.PieceTable);
  AStart := TdxPositionConverter.ToDocumentModelPosition(APieceTable, Min(ASelection.NormalizedStart, APieceTable.DocumentEndLogPosition));
  AStartField := APieceTable.FindFieldByRunIndex(AStart.RunIndex);
  if AStartField = nil then
    Exit(nil);
  AEnd := TdxPositionConverter.ToDocumentModelPosition(APieceTable, Min(ASelection.NormalizedEnd, APieceTable.DocumentEndLogPosition));
  if (AEnd.RunOffset = 0) and (AEnd.RunIndex = AStartField.LastRunIndex + 1) then
    Exit(AStartField);
  AEndField := APieceTable.FindFieldByRunIndex(AEnd.RunIndex);
  if AEndField = nil then
    Exit(nil);
  if AStartField.ContainsField(AEndField) then
    Exit(AStartField);
  if AEndField.ContainsField(AStartField) then
    Exit(AEndField);
  if AStartField = AEndField then
    Result := AStartField
  else
    Result := nil;
end;

end.
