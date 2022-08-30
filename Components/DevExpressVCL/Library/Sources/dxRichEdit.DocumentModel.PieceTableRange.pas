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

unit dxRichEdit.DocumentModel.PieceTableRange;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  dxRichEdit.Utils.Types,
  dxRichEdit.DocumentModel.Boxes.Core,
  dxRichEdit.DocumentModel.CharacterFormatting,
  dxRichEdit.DocumentModel.CopyManager.Core,
  dxRichEdit.DocumentModel.Simple;

type
  { TdxLineNumberCommonRun }

  TdxLineNumberCommonRun = class(TdxTextRunBase)
  public
    function CanPlaceCaretBefore: Boolean; override;
    function CanJoinWith(ANextRun: TdxTextRunBase): Boolean; override;
    function Copy(ACopyManager: TdxCustomDocumentModelCopyManager): TdxTextRunBase; override;
    procedure Export(const AExporter: IdxSimpleDocumentModelExporter); override;
    function GetParentMergedCharacterProperties: TdxMergedCharacterProperties; override;
    procedure Measure(ABoxInfo: TdxBoxInfo; const AMeasurer: IdxObjectMeasurer); override;
    function TryAdjustEndPositionToFit(ABoxInfo: TdxBoxInfo; AMaxWidth: Integer; const AMeasurer: IdxObjectMeasurer): Boolean; override;
  end;

implementation

uses
  dxRichEdit.Utils.Exceptions;

{ TdxLineNumberCommonRun }

function TdxLineNumberCommonRun.CanJoinWith(ANextRun: TdxTextRunBase): Boolean;
begin
  Result := False;
end;

function TdxLineNumberCommonRun.CanPlaceCaretBefore: Boolean;
begin
  Result := False;
end;

function TdxLineNumberCommonRun.Copy(ACopyManager: TdxCustomDocumentModelCopyManager): TdxTextRunBase;
begin
  raise TdxNotImplementedException.Create;
end;

procedure TdxLineNumberCommonRun.Export(const AExporter: IdxSimpleDocumentModelExporter);
begin
end;

function TdxLineNumberCommonRun.GetParentMergedCharacterProperties: TdxMergedCharacterProperties;
begin
  Result := CharacterStyle.GetMergedCharacterProperties;
  Result.Merge(DocumentModel.DefaultCharacterProperties);
end;

procedure TdxLineNumberCommonRun.Measure(ABoxInfo: TdxBoxInfo; const AMeasurer: IdxObjectMeasurer);
begin
end;

function TdxLineNumberCommonRun.TryAdjustEndPositionToFit(ABoxInfo: TdxBoxInfo; AMaxWidth: Integer; const AMeasurer: IdxObjectMeasurer): Boolean;
begin
  Result := False;
end;

end.
