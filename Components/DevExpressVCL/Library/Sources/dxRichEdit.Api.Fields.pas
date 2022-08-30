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

unit dxRichEdit.Api.Fields;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types,
  dxCore, dxCoreClasses, dxCoreGraphics,

  dxRichEdit.NativeApi,
  dxRichEdit.Api.NativeDocument,

  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.Fields.Core,
  dxRichEdit.DocumentModel.Fields;

type
  { TdxNativeField }

  TdxNativeField = class(TInterfacedObject, IdxRichEditField)
  strict private
    FDocument: TdxNativeSubDocument;
    FField: TdxField;
    FIsValid: Boolean;
    procedure CheckValid;
    function GetCodeRange: IdxRichEditDocumentRange;
    function GetLocked: Boolean;
    function GetParent: IdxRichEditField;
    function GetRange: IdxRichEditDocumentRange;
    function GetResultRange: IdxRichEditDocumentRange;
    function GetShowCodes: Boolean;
    procedure SetLocked(const AValue: Boolean);
    procedure SetShowCodes(const AValue: Boolean);
  public
    constructor Create(const ADocument: TdxNativeSubDocument;
      AField: TdxField);
    procedure Update;

    property Field: TdxField read FField;
    property IsValid: Boolean read FIsValid write FIsValid;

    property Range: IdxRichEditDocumentRange read GetRange;
    property CodeRange: IdxRichEditDocumentRange read GetCodeRange;
    property ResultRange: IdxRichEditDocumentRange read GetResultRange;
    property ShowCodes: Boolean read GetShowCodes write SetShowCodes;
    property Locked: Boolean read GetLocked write SetLocked;
    property Parent: IdxRichEditField read GetParent;
  end;

implementation

uses
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.Utils.Exceptions,
  dxRichEdit.Utils.Exceptions.Strs;

{ TdxNativeField }

constructor TdxNativeField.Create(const ADocument: TdxNativeSubDocument;
  AField: TdxField);
begin
  inherited Create;
  FDocument := ADocument;
  FField := AField;
  FIsValid := True;
end;

function TdxNativeField.GetShowCodes: Boolean;
begin
  CheckValid;
  Result := FField.IsCodeView;
end;

procedure TdxNativeField.SetShowCodes(const AValue: Boolean);
begin
  CheckValid;
  if ShowCodes <> AValue then
    FDocument.PieceTable.ToggleFieldCodesFromCommandOrApi(FField);
end;

function TdxNativeField.GetLocked: Boolean;
begin
  CheckValid;
  Result := FField.Locked;
end;

procedure TdxNativeField.SetLocked(const AValue: Boolean);
begin
  CheckValid;
  if Locked <> AValue then
    FDocument.PieceTable.ToggleFieldLocked(FField);
end;

procedure TdxNativeField.Update;
begin
  CheckValid;
  with TdxFieldUpdater.Create(FDocument.PieceTable) do
  try
    UpdateFieldAndNestedFields(FField);
  finally
    Free;
  end;
end;

function TdxNativeField.GetParent: IdxRichEditField;
begin
  if FField.Parent <> nil then
    Result := TdxNativeField.Create(FDocument, FField.Parent)
  else
    Result := nil;
end;

function TdxNativeField.GetRange: IdxRichEditDocumentRange;
var
  APieceTable: TdxPieceTable;
  AStart, AEnd: TdxDocumentModelPosition;
begin
  CheckValid;
  APieceTable := FDocument.PieceTable;
  AStart := TdxDocumentModelPosition.FromRunStart(APieceTable, FField.FirstRunIndex);
  AEnd := TdxDocumentModelPosition.FromRunStart(APieceTable, FField.LastRunIndex + 1);
  Result := TdxNativeDocumentRange.Create(FDocument, AStart, AEnd);
end;

function TdxNativeField.GetCodeRange: IdxRichEditDocumentRange;
var
  APieceTable: TdxPieceTable;
  AStart, AEnd: TdxDocumentModelPosition;
begin
  CheckValid;
  APieceTable := FDocument.PieceTable;
  AStart := TdxDocumentModelPosition.FromRunStart(APieceTable, FField.Code.Start + 1);
  AEnd := TdxDocumentModelPosition.FromRunStart(APieceTable, FField.Code.&End);
  Result := TdxNativeDocumentRange.Create(FDocument, AStart, AEnd);
end;

function TdxNativeField.GetResultRange: IdxRichEditDocumentRange;
var
  APieceTable: TdxPieceTable;
  AStart, AEnd: TdxDocumentModelPosition;
begin
  CheckValid;
  APieceTable := FDocument.PieceTable;
  AStart := TdxDocumentModelPosition.FromRunStart(APieceTable, FField.Result.Start);
  AEnd := TdxDocumentModelPosition.FromRunStart(APieceTable, FField.Result.&End);
  Result := TdxNativeDocumentRange.Create(FDocument, AStart, AEnd);
end;

procedure TdxNativeField.CheckValid;
begin
  if not FIsValid then
    TdxRichEditExceptions.ThrowInvalidOperationException(cxGetResourceString(@sdxRichEditExceptionUseDeletedFieldError));
end;

end.
