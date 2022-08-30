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

unit dxRichEdit.DocumentModel.History.Style;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Generics.Defaults, Generics.Collections,
  dxRichEdit.Utils.Types,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.DocumentModel.Styles;

type
  { TdxChangeParagraphStyleIndexHistoryItem }

  TdxChangeParagraphStyleIndexHistoryItem = class(TdxRichEditHistoryItem)
  private
    FOldIndex: Integer;
    FNewIndex: Integer;
    FParagraphIndex: TdxParagraphIndex;
  protected
    procedure UndoCore; override;
    procedure RedoCore; override;
  public
    constructor Create(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex;
      AOldStyleIndex, ANewStyleIndex: Integer); reintroduce;

    property OldIndex: Integer read FOldIndex;
    property NewIndex: Integer read FNewIndex;
    property ParagraphIndex: TdxParagraphIndex read FParagraphIndex;
  end;

  { TdxParagraphStyleChangeAutoUpdatePropertyHistoryItem }

  TdxParagraphStyleChangeAutoUpdatePropertyHistoryItem = class(TdxRichEditHistoryItem)
  private
    FStyle: TdxParagraphStyle;
  protected
    procedure UndoCore; override;
    procedure RedoCore; override;
  public
    constructor Create(AStyle: TdxParagraphStyle); reintroduce;

    property Style: TdxParagraphStyle read FStyle;
  end;

  { TdxParagraphStyleChangeNextParagraphStylePropertyHistoryItem }

  TdxParagraphStyleChangeNextParagraphStylePropertyHistoryItem = class(TdxRichEditHistoryItem)
  strict private
    FStyle: TdxParagraphStyle;
    FOldValue: TdxParagraphStyle;
    FNewValue: TdxParagraphStyle;
  protected
    procedure UndoCore; override;
    procedure RedoCore; override;
  public
    constructor Create(AStyle: TdxParagraphStyle; AOldValue: TdxParagraphStyle; ANewValue: TdxParagraphStyle); reintroduce;

    property Style: TdxParagraphStyle read FStyle;
  end;

  { TdxChangeCharacterStyleIndexHistoryItem }

  TdxChangeCharacterStyleIndexHistoryItem = class(TdxRichEditHistoryItem)
  strict private
    FOldIndex: Integer;
    FNewIndex: Integer;
    FRunIndex: TdxRunIndex;
  protected
    procedure UndoCore; override;
    procedure RedoCore; override;
  public
    constructor Create(APieceTable: TdxCustomPieceTable; ARunIndex: TdxRunIndex;
      AOldStyleIndex, ANewStyleIndex: Integer); reintroduce;
    property OldIndex: Integer read FOldIndex;
    property NewIndex: Integer read FNewIndex;
    property RunIndex: TdxRunIndex read FRunIndex;
  end;

implementation

uses
  SysUtils;

type
  TdxParagraphStyleAccess = class(TdxParagraphStyle);

{ TdxChangeParagraphStyleIndexHistoryItem }

constructor TdxChangeParagraphStyleIndexHistoryItem.Create(APieceTable: TdxCustomPieceTable; AParagraphIndex: TdxParagraphIndex;
  AOldStyleIndex, ANewStyleIndex: Integer);
begin
  inherited Create(APieceTable);
  FOldIndex := AOldStyleIndex;
  FNewIndex := ANewStyleIndex;
  FParagraphIndex := AParagraphIndex;
end;

procedure TdxChangeParagraphStyleIndexHistoryItem.UndoCore;
begin
  PieceTable.Paragraphs[ParagraphIndex].SetParagraphStyleIndexCore(OldIndex);
end;

procedure TdxChangeParagraphStyleIndexHistoryItem.RedoCore;
begin
  PieceTable.Paragraphs[ParagraphIndex].SetParagraphStyleIndexCore(NewIndex);
end;

{ TdxParagraphStyleChangeAutoUpdatePropertyHistoryItem }

constructor TdxParagraphStyleChangeAutoUpdatePropertyHistoryItem.Create(AStyle: TdxParagraphStyle);
begin
  inherited Create(AStyle.DocumentModel.MainPart);
  FStyle := AStyle;
end;

procedure TdxParagraphStyleChangeAutoUpdatePropertyHistoryItem.RedoCore;
begin
  TdxParagraphStyleAccess(Style).SetAutoUpdateCore(not Style.AutoUpdate);
end;

procedure TdxParagraphStyleChangeAutoUpdatePropertyHistoryItem.UndoCore;
begin
  TdxParagraphStyleAccess(Style).SetAutoUpdateCore(not Style.AutoUpdate);
end;

{ TdxParagraphStyleChangeNextParagraphStylePropertyHistoryItem }

constructor TdxParagraphStyleChangeNextParagraphStylePropertyHistoryItem.Create(AStyle: TdxParagraphStyle; AOldValue: TdxParagraphStyle; ANewValue: TdxParagraphStyle);
begin
  inherited Create(AStyle.DocumentModel.MainPart);
  FStyle := AStyle;
  FOldValue := AOldValue;
  FNewValue := ANewValue;
end;

procedure TdxParagraphStyleChangeNextParagraphStylePropertyHistoryItem.UndoCore;
begin
  Style.SetNextParagraphStyleCore(FOldValue);
end;

procedure TdxParagraphStyleChangeNextParagraphStylePropertyHistoryItem.RedoCore;
begin
  Style.SetNextParagraphStyleCore(FNewValue);
end;

{ TdxChangeCharacterStyleIndexHistoryItem }

constructor TdxChangeCharacterStyleIndexHistoryItem.Create(
  APieceTable: TdxCustomPieceTable; ARunIndex: TdxRunIndex; AOldStyleIndex,
  ANewStyleIndex: Integer);
begin
  inherited Create(APieceTable);
  FRunIndex := ARunIndex;
  FOldIndex := AOldStyleIndex;
  FNewIndex := ANewStyleIndex;
end;

procedure TdxChangeCharacterStyleIndexHistoryItem.RedoCore;
begin
  PieceTable.Runs[RunIndex].SetCharacterStyleIndexCore(NewIndex);
end;

procedure TdxChangeCharacterStyleIndexHistoryItem.UndoCore;
begin
  PieceTable.Runs[RunIndex].SetCharacterStyleIndexCore(OldIndex);
end;

end.
