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

unit dxRichEdit.DocumentModel.FieldRange;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, SysUtils,
  dxRichEdit.Utils.ChunkedStringBuilder,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Boxes.Core,
  dxRichEdit.DocumentModel.TextRange,
  dxRichEdit.DocumentModel.CopyManager.Core,
  dxRichEdit.DocumentModel.Simple;

type

  { TdxFieldResultEndRun }

  TdxFieldResultEndRun = class(TdxTextRun)
  protected
    function GetLength: Integer; override;
    function GetRawTextFast(ABuffer: TdxChunkedStringBuilder): string; override;
    function GetText: string; override;
    procedure SetLength(const Value: Integer); override;
    function IsFieldRun: Boolean; override;
  public
    function CanPlaceCaretBefore: Boolean; override;
    function CanJoinWith(ARun: TdxTextRunBase): Boolean; override;
    function Copy(ACopyManager: TdxCustomDocumentModelCopyManager): TdxTextRunBase; override;
    procedure Export(const AExporter: IdxSimpleDocumentModelExporter); override;
    function GetPlainText(ABuffer: TdxChunkedStringBuilder): string; overload; override;
    function GetPlainText(ABuffer: TdxChunkedStringBuilder; AFrom, ATo: Integer): string; overload; override;
    function GetTextFast(ABuffer: TdxChunkedStringBuilder): string; override;
    procedure Measure(ABoxInfo: TdxBoxInfo; const AMeasurer: IdxObjectMeasurer); override;
    function TryAdjustEndPositionToFit(ABoxInfo: TdxBoxInfo; AMaxWidth: Integer; const AMeasurer: IdxObjectMeasurer): Boolean; override;
  end;

  { TdxFieldSymbolRun }

  TdxFieldSymbolRun = class(TdxTextRun)
  public
    function Copy(ACopyManager: TdxCustomDocumentModelCopyManager): TdxTextRunBase; override;
    function IsFieldSymbolRun: Boolean; override;
  end;

  { TdxFieldCodeRunBase }

  TdxFieldCodeRunBase = class abstract(TdxSpecialTextRun)
  protected
    function GetLength: Integer; override;
    function GetRawTextFast(ABuffer: TdxChunkedStringBuilder): string; override;
    function GetText: string; override;
    procedure SetLength(const Value: Integer); override;
    procedure InsertMarkRange(APieceTable: TdxSimplePieceTable; const APos: TdxDocumentModelPosition); virtual; abstract;
    function IsFieldRun: Boolean; override;
  public
    function CanPlaceCaretBefore: Boolean; override;
    function CanJoinWith(ARun: TdxTextRunBase): Boolean; override;
    function Copy(ACopyManager: TdxCustomDocumentModelCopyManager): TdxTextRunBase; override;
    procedure Export(const AExporter: IdxSimpleDocumentModelExporter); override;
    function GetPlainText(ABuffer: TdxChunkedStringBuilder; AFrom: Integer; ATo: Integer): string; override;
    function GetTextFast(ABuffer: TdxChunkedStringBuilder): string; override;
  end;

  { TdxFieldCodeStartRun }

  TdxFieldCodeStartRun = class(TdxFieldCodeRunBase)
  protected
    function GetRawTextFast(ABuffer: TdxChunkedStringBuilder): string; override;
    procedure InsertMarkRange(APieceTable: TdxSimplePieceTable; const APos: TdxDocumentModelPosition); override;
  public
    function GetPlainText(ABuffer: TdxChunkedStringBuilder): string; override;
  end;

  { TdxFieldCodeEndRun }

  TdxFieldCodeEndRun = class(TdxFieldCodeRunBase)
  protected
    procedure InsertMarkRange(APieceTable: TdxSimplePieceTable; const APos: TdxDocumentModelPosition); override;
    function GetRawTextFast(ABuffer: TdxChunkedStringBuilder): string; override;
  public
    function GetPlainText(ABuffer: TdxChunkedStringBuilder): string; override;
  end;

implementation

uses
  dxRichEdit.Utils.Exceptions;

{ TdxFieldResultEndRun }

function TdxFieldResultEndRun.CanJoinWith(ARun: TdxTextRunBase): Boolean;
begin
  Result := False;
end;

function TdxFieldResultEndRun.CanPlaceCaretBefore: Boolean;
begin
  Result := True;
end;

function TdxFieldResultEndRun.Copy(
  ACopyManager: TdxCustomDocumentModelCopyManager): TdxTextRunBase;
var
  APieceTable: TdxSimplePieceTable;
  ATargetPosition: TdxDocumentModelPosition;
begin
  APieceTable := TdxSimplePieceTable(ACopyManager.TargetPieceTable);
  ATargetPosition := ACopyManager.TargetPosition;
  APieceTable.InsertFieldResultEndRunCore(ATargetPosition.ParagraphIndex, ATargetPosition.LogPosition);
  Result := APieceTable.Runs[ATargetPosition.RunIndex];
  CopyCore(TdxSimpleDocumentModel(ACopyManager.TargetModel), Result);
end;

procedure TdxFieldResultEndRun.Export(
  const AExporter: IdxSimpleDocumentModelExporter);
begin
  AExporter.Export(Self);
end;

function TdxFieldResultEndRun.GetLength: Integer;
begin
  Result := 1;
end;

function TdxFieldResultEndRun.GetPlainText(ABuffer: TdxChunkedStringBuilder;
  AFrom, ATo: Integer): string;
begin
  Result := '';
end;

function TdxFieldResultEndRun.GetPlainText(
  ABuffer: TdxChunkedStringBuilder): string;
begin
  Result := '';
end;

function TdxFieldResultEndRun.GetRawTextFast(
  ABuffer: TdxChunkedStringBuilder): string;
begin
  Result := ' ';
end;

function TdxFieldResultEndRun.GetText: string;
begin
  Result := '';
end;

function TdxFieldResultEndRun.GetTextFast(
  ABuffer: TdxChunkedStringBuilder): string;
begin
  Result := '';
end;

procedure TdxFieldResultEndRun.Measure(ABoxInfo: TdxBoxInfo;
  const AMeasurer: IdxObjectMeasurer);
begin
  TdxRichEditExceptions.ThrowInternalException;
end;

procedure TdxFieldResultEndRun.SetLength(const Value: Integer);
begin
// do nothing
end;

function TdxFieldResultEndRun.IsFieldRun: Boolean;
begin
  Result := True;
end;

function TdxFieldResultEndRun.TryAdjustEndPositionToFit(ABoxInfo: TdxBoxInfo; AMaxWidth: Integer; const AMeasurer: IdxObjectMeasurer): Boolean;
begin
  Result := False;
end;

{ TdxFieldSymbolRun }

function TdxFieldSymbolRun.Copy(ACopyManager: TdxCustomDocumentModelCopyManager): TdxTextRunBase;
var
  APieceTable: TdxSimplePieceTable;
  ATargetPosition: TdxDocumentModelPosition;
begin
  APieceTable := TdxSimplePieceTable(ACopyManager.TargetPieceTable);
  ATargetPosition := ACopyManager.TargetPosition;
  APieceTable.InsertFieldSymbolResult(ATargetPosition.LogPosition, GetText[1]);
  Result := APieceTable.Runs[ATargetPosition.RunIndex];
  Result.CopyCharacterPropertiesFrom(ACopyManager, Self);
end;

function TdxFieldSymbolRun.IsFieldSymbolRun: Boolean;
begin
  Result := True;
end;

{ TdxFieldCodeRunBase }

function TdxFieldCodeRunBase.CanJoinWith(ARun: TdxTextRunBase): Boolean;
begin
  Result := False;
end;

function TdxFieldCodeRunBase.CanPlaceCaretBefore: Boolean;
begin
  Result := True;
end;

function TdxFieldCodeRunBase.Copy(
  ACopyManager: TdxCustomDocumentModelCopyManager): TdxTextRunBase;
var
  APieceTable: TdxSimplePieceTable;
  ATargetPosition: TdxDocumentModelPosition;
begin
  APieceTable := TdxSimplePieceTable(ACopyManager.TargetPieceTable);
  ATargetPosition := ACopyManager.TargetPosition;
  InsertMarkRange(APieceTable, ATargetPosition);
  Result := APieceTable.Runs[ATargetPosition.RunIndex];
  Result.CopyCharacterPropertiesFrom(ACopyManager, Self);
end;

procedure TdxFieldCodeRunBase.Export(const AExporter: IdxSimpleDocumentModelExporter);
begin
  AExporter.Export(Self);
end;

function TdxFieldCodeRunBase.GetLength: Integer;
begin
  Result := 1;
end;

function TdxFieldCodeRunBase.GetPlainText(ABuffer: TdxChunkedStringBuilder;
  AFrom, ATo: Integer): string;
begin
  Result := GetPlainText(ABuffer);
end;

function TdxFieldCodeRunBase.GetRawTextFast(
  ABuffer: TdxChunkedStringBuilder): string;
begin
  Result := '';
end;

function TdxFieldCodeRunBase.GetText: string;
begin
  Result := '';
end;

function TdxFieldCodeRunBase.GetTextFast(
  ABuffer: TdxChunkedStringBuilder): string;
begin
  Result := '';
end;

procedure TdxFieldCodeRunBase.SetLength(const Value: Integer);
begin
// do nothing
end;

function TdxFieldCodeRunBase.IsFieldRun: Boolean;
begin
  Result := True;
end;

{ TdxFieldCodeStartRun }

function TdxFieldCodeStartRun.GetPlainText(
  ABuffer: TdxChunkedStringBuilder): string;
begin
  Result := #$0013;
end;

function TdxFieldCodeStartRun.GetRawTextFast(
  ABuffer: TdxChunkedStringBuilder): string;
begin
  Result := '{';
end;

procedure TdxFieldCodeStartRun.InsertMarkRange(APieceTable: TdxSimplePieceTable;
  const APos: TdxDocumentModelPosition);
begin
  APieceTable.InsertFieldCodeStartRunCore(APos.ParagraphIndex, APos.LogPosition);
end;

{ TdxFieldCodeEndRun }

function TdxFieldCodeEndRun.GetPlainText(
  ABuffer: TdxChunkedStringBuilder): string;
begin
  Result := #$0015;
end;

function TdxFieldCodeEndRun.GetRawTextFast(
  ABuffer: TdxChunkedStringBuilder): string;
begin
  Result := '}';
end;

procedure TdxFieldCodeEndRun.InsertMarkRange(APieceTable: TdxSimplePieceTable;
  const APos: TdxDocumentModelPosition);
begin
  APieceTable.InsertFieldCodeEndRunCore(APos.ParagraphIndex, APos.LogPosition);
end;

end.
