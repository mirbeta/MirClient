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

unit dxRichEdit.DocumentModel.PieceTableModifiers.Core;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  dxRichEdit.Utils.Types,
  dxRichEdit.DocumentModel.Core;

type
  { TdxObjectInserter }

  TdxObjectInserter = class abstract
  strict private
    FDocumentModel: TdxCustomDocumentModel;
    FPieceTable: TdxCustomPieceTable;
    FTextLength: Integer;
  protected
    property DocumentModel: TdxCustomDocumentModel read FDocumentModel;
    property PieceTable: TdxCustomPieceTable read FPieceTable;
  public
    constructor Create(APieceTable: TdxCustomPieceTable); virtual;

    function CanMerge(ALogPosition: TdxDocumentLogPosition): Boolean; virtual; abstract;
    procedure Merge(ALogPosition: TdxDocumentLogPosition; AParagraphIndex: TdxParagraphIndex); virtual; abstract;
    procedure PerformInsert(AParagraph: TdxParagraphBase; ARunIndex: TdxRunIndex; ALogPosition: TdxDocumentLogPosition;
      AForceVisible: Boolean); virtual; abstract;

    property TextLength: Integer read FTextLength write FTextLength;
  end;

implementation

{ TdxObjectInserter }

constructor TdxObjectInserter.Create(APieceTable: TdxCustomPieceTable);
begin
  inherited Create;
  FPieceTable := APieceTable;
  FDocumentModel := FPieceTable.DocumentModel;
end;

end.
