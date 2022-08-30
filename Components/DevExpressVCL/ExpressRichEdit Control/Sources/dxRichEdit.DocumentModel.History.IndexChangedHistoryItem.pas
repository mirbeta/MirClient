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

unit dxRichEdit.DocumentModel.History.IndexChangedHistoryItem;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, dxRichEdit.DocumentModel.Core;

type
  { TdxIndexChangedHistoryItemCore }

  TdxIndexChangedHistoryItemCore = class(TdxHistoryItem)
  private
    FOldIndex: Integer;
    FNewIndex: Integer;
    FChangeActions: TdxDocumentModelChangeActions;
  protected
    procedure UndoCore; override;
    procedure RedoCore; override;
  public
    function GetObject: TdxIndexBasedObject; virtual; abstract;
    function NeedStoreSelection: Boolean; override;

    property OldIndex: Integer read FOldIndex write FOldIndex;
    property NewIndex: Integer read FNewIndex write FNewIndex;
    property ChangeActions: TdxDocumentModelChangeActions read FChangeActions write FChangeActions;
  end;

  { TdxIndexChangedHistoryItem }

  TdxIndexChangedHistoryItem = class(TdxIndexChangedHistoryItemCore)
  private
    FObj: TdxIndexBasedObject;
  public
    constructor Create(const ADocumentModelPart: TdxCustomPieceTable; const AObj: TdxIndexBasedObject); reintroduce;
    function GetObject: TdxIndexBasedObject; override;

    property &Object: TdxIndexBasedObject read FObj;
  end;

implementation

{ TdxIndexChangedHistoryItemCore<TActions> }

procedure TdxIndexChangedHistoryItemCore.RedoCore;
var
  AObj: TdxIndexBasedObject;
begin
  AObj := GetObject;
  AObj.SetIndex(NewIndex, ChangeActions);
end;

function TdxIndexChangedHistoryItemCore.NeedStoreSelection: Boolean;
begin
  Result := False;
end;

procedure TdxIndexChangedHistoryItemCore.UndoCore;
var
  AObj: TdxIndexBasedObject;
begin
  AObj := GetObject;
  AObj.SetIndex(OldIndex, ChangeActions);
end;

{ TdxIndexChangedHistoryItem<TActions> }

constructor TdxIndexChangedHistoryItem.Create(
  const ADocumentModelPart: TdxCustomPieceTable;
  const AObj: TdxIndexBasedObject);
begin
  inherited Create(ADocumentModelPart);
  FObj := AObj;
end;

function TdxIndexChangedHistoryItem.GetObject: TdxIndexBasedObject;
begin
  Result := FObj;
end;

end.
