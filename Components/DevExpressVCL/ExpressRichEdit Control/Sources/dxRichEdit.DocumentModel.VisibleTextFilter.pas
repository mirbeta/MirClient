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

unit dxRichEdit.DocumentModel.VisibleTextFilter;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  dxRichEdit.Utils.Types,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.VisibleTextFilter.Core,
  dxRichEdit.DocumentModel.VisibleTextFilter.Simple;

type
  { TdxEmptyTextFilterSkipFloatingObjects }

  TdxEmptyTextFilterSkipFloatingObjects = class(TdxEmptyTextFilter)
  public
    function Clone(APieceTable: TdxCustomPieceTable): IdxVisibleTextFilter; override;
    function IsRunVisible(const ARunIndex: TdxRunIndex): Boolean; override;
  end;

  { TdxVisibleTextFilterSkipFloatingObjects }

  TdxVisibleTextFilterSkipFloatingObjects = class(TdxVisibleTextFilter)
  public
    function Clone(APieceTable: TdxCustomPieceTable): IdxVisibleTextFilter; override;
    function IsRunVisible(const ARunIndex: TdxRunIndex): Boolean; override;
  end;

implementation

uses
  dxRichEdit.DocumentModel.FloatingObjectRange;

{ TdxEmptyTextFilterSkipFloatingObjects }

function TdxEmptyTextFilterSkipFloatingObjects.Clone(
  APieceTable: TdxCustomPieceTable): IdxVisibleTextFilter;
begin
  if Self = nil then
    Exit(nil);
  Result := TdxEmptyTextFilterSkipFloatingObjects.Create(APieceTable);
end;

function TdxEmptyTextFilterSkipFloatingObjects.IsRunVisible(const ARunIndex: TdxRunIndex): Boolean;
begin
  Result := (inherited IsRunVisible(ARunIndex)) and not (PieceTable.Runs[ARunIndex] is TdxFloatingObjectAnchorRun);
end;

{ TdxVisibleTextFilterSkipFloatingObjects }

function TdxVisibleTextFilterSkipFloatingObjects.Clone(
  APieceTable: TdxCustomPieceTable): IdxVisibleTextFilter;
begin
  if Self = nil then
    Exit(nil);
  Result := TdxVisibleTextFilterSkipFloatingObjects.Create(APieceTable);
end;

function TdxVisibleTextFilterSkipFloatingObjects.IsRunVisible(const ARunIndex: TdxRunIndex): Boolean;
begin
  Result := (inherited IsRunVisible(ARunIndex)) and
    not (PieceTable.Runs[ARunIndex] is TdxFloatingObjectAnchorRun);
end;

end.
