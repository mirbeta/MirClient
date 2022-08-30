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

unit dxRichEdit.DocumentModel.ParagraphRange;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, Classes, SysUtils, Graphics,
  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.DocumentModel.Boxes.Core,
  dxRichEdit.DocumentModel.CopyManager.Core,
  dxRichEdit.Utils.ChunkedStringBuilder;

type
  { TdxParagraphRun }

  TdxParagraphRun = class(TdxTextRunBase)
  protected
    function CanSetLength(const Value: Integer): Boolean; override;
    function GetRawTextFast(ABuffer: TdxChunkedStringBuilder): string; override;
    function GetText: string; override;
  public
    function CanJoinWith(ARun: TdxTextRunBase): Boolean; override;
    function CanPlaceCaretBefore: Boolean; override;
    function Copy(ACopyManager: TdxCustomDocumentModelCopyManager): TdxTextRunBase; override;
    procedure Export(const AExporter: IdxSimpleDocumentModelExporter); override;
    function GetPlainText(ABuffer: TdxChunkedStringBuilder): string; override;
    function GetPlainText(ABuffer: TdxChunkedStringBuilder; AFrom, ATo: Integer): string; override;
    function GetTextFast(ABuffer: TdxChunkedStringBuilder): string; override;
    procedure Measure(ABoxInfo: TdxBoxInfo; const AMeasurer: IdxObjectMeasurer); override;
    function TryAdjustEndPositionToFit(ABoxInfo: TdxBoxInfo; AMaxWidth: Integer; const AMeasurer: IdxObjectMeasurer): Boolean; override;
  end;

implementation

uses
  dxRichEdit.Utils.Exceptions;

{ TdxParagraphRun }

function TdxParagraphRun.CanJoinWith(ARun: TdxTextRunBase): Boolean;
begin
  Assert(ARun<>nil, 'Run = nil');
  Result := False;
end;

function TdxParagraphRun.CanPlaceCaretBefore: Boolean;
begin
  Result := True;
end;

function TdxParagraphRun.Copy(ACopyManager: TdxCustomDocumentModelCopyManager): TdxTextRunBase;
begin
  TdxRichEditExceptions.ThrowInternalException;
  Result := nil;
end;

procedure TdxParagraphRun.Export(const AExporter: IdxSimpleDocumentModelExporter);
begin
  AExporter.Export(Self);
end;

function TdxParagraphRun.GetPlainText(ABuffer: TdxChunkedStringBuilder): string;
begin
  Result := #13#10;
end;

function TdxParagraphRun.CanSetLength(const Value: Integer): Boolean;
begin
  Result := Value = 1;
end;

function TdxParagraphRun.GetPlainText(ABuffer: TdxChunkedStringBuilder; AFrom, ATo: Integer): string;
begin
  Result := GetPlainText(ABuffer);
end;

function TdxParagraphRun.GetRawTextFast(ABuffer: TdxChunkedStringBuilder): string;
begin
  Result := #13;
end;

function TdxParagraphRun.GetText: string;
begin
  Result := '';
end;

function TdxParagraphRun.GetTextFast(ABuffer: TdxChunkedStringBuilder): string;
begin
  Result := '';
end;

procedure TdxParagraphRun.Measure(ABoxInfo: TdxBoxInfo; const AMeasurer: IdxObjectMeasurer);
begin
  TdxRichEditExceptions.ThrowInternalException;
end;

function TdxParagraphRun.TryAdjustEndPositionToFit(ABoxInfo: TdxBoxInfo; AMaxWidth: Integer; const AMeasurer: IdxObjectMeasurer): Boolean;
begin
  Result := False;
end;

end.
