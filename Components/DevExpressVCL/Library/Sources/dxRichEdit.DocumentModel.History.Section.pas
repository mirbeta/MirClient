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

unit dxRichEdit.DocumentModel.History.Section;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Classes, Generics.Defaults, Generics.Collections, Contnrs,
  dxGenerics,
  dxRichEdit.Utils.Types,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.History.Simple,
  dxRichEdit.DocumentModel.SectionRange;

type
  { TdxSectionHistoryItem }

  TdxSectionHistoryItem = class abstract(TdxParagraphBaseHistoryItem)
  public
    constructor Create(ADocumentModel: TdxCustomDocumentModel); reintroduce; virtual;
  end;

  { TdxSectionInsertedHistoryItem }

  TdxSectionInsertedHistoryItem = class(TdxSectionHistoryItem)
  strict private
    FNewSection: TdxCustomSection;
  protected
    procedure UndoCore; override;
    procedure RedoCore; override;
  public
    destructor Destroy; override;
    procedure Execute; override;
  end;

  { TdxSectionsDeletedHistoryItem }

  TdxSectionsDeletedHistoryItem = class(TdxSectionHistoryItem)
  strict private
    FDeletedSectionsCount: Integer;
    FDeletedSections: TdxCustomSectionCollection;
  protected
    procedure UndoCore; override;
    procedure RedoCore; override;
  public
    constructor Create(ADocumentModel: TdxCustomDocumentModel); override;
    destructor Destroy; override;

    property DeletedSectionsCount: Integer read FDeletedSectionsCount write FDeletedSectionsCount;
  end;

implementation

uses
  SysUtils;

{ TdxSectionHistoryItem }

constructor TdxSectionHistoryItem.Create(ADocumentModel: TdxCustomDocumentModel);
begin
  inherited Create(ADocumentModel.MainPart);
end;

{ TdxSectionInsertedHistoryItem }

destructor TdxSectionInsertedHistoryItem.Destroy;
begin
  FreeAndNil(FNewSection);
  inherited Destroy;
end;

procedure TdxSectionInsertedHistoryItem.Execute;
var
  ASectionIndex: TdxSectionIndex;
  ASection: TdxCustomSection;
begin
  ASectionIndex := DocumentModel.MainPieceTable.LookupSectionIndexByParagraphIndex(ParagraphIndex);
  Assert(ASectionIndex >= 0);
  ASection := DocumentModel.Sections[ASectionIndex];
  FNewSection := DocumentModel.CreateSection;
  FNewSection.CopyFrom(ASection);

  inherited Execute;
end;

procedure TdxSectionInsertedHistoryItem.UndoCore;
var
  ASectionIndex: TdxSectionIndex;
  ASection: TdxCustomSection;
begin
  ASectionIndex := DocumentModel.MainPart.LookupSectionIndexByParagraphIndex(ParagraphIndex);
  Assert(ASectionIndex >= 0);
  ASection := DocumentModel.Sections[ASectionIndex];

  FNewSection := DocumentModel.Sections[ASectionIndex + 1];
  ASection.LastParagraphIndex := FNewSection.LastParagraphIndex;
  DocumentModel.OnSectionRemoved(ASectionIndex + 1);
  DocumentModel.Sections[ASectionIndex + 1].UnsubscribeHeadersFootersEvents;
  DocumentModel.Sections.Extract(FNewSection);
end;

procedure TdxSectionInsertedHistoryItem.RedoCore;
var
  ASectionIndex: TdxSectionIndex;
  ASection: TdxCustomSection;
begin
  ASectionIndex := DocumentModel.MainPart.LookupSectionIndexByParagraphIndex(ParagraphIndex);
  Assert(ASectionIndex >= 0);
  ASection := DocumentModel.Sections[ASectionIndex];

  FNewSection.FirstParagraphIndex := ParagraphIndex + 1;
  FNewSection.LastParagraphIndex := ASection.LastParagraphIndex;
  DocumentModel.Sections.Insert(ASectionIndex + 1, FNewSection);
  FNewSection.SubscribeInnerObjectsEvents;

  ASection.LastParagraphIndex := ParagraphIndex;

  DocumentModel.OnSectionInserted(ASectionIndex + 1);
  FNewSection := nil;
end;

{ TdxSectionsDeletedHistoryItem }

constructor TdxSectionsDeletedHistoryItem.Create(ADocumentModel: TdxCustomDocumentModel);
begin
  inherited Create(ADocumentModel);
  FDeletedSectionsCount := -1;
end;

destructor TdxSectionsDeletedHistoryItem.Destroy;
begin
  FDeletedSections.Free;
  inherited Destroy;
end;

procedure TdxSectionsDeletedHistoryItem.UndoCore;
var
  ASections: TdxCustomSectionCollection;
  ACount, I: TdxSectionIndex;
  ANewSection: TdxCustomSection;
begin
  ASections := DocumentModel.Sections;
  ACount := SectionIndex + DeletedSectionsCount;
  for I := SectionIndex to ACount - 1 do
  begin
    ANewSection := FDeletedSections[I - SectionIndex];
    ASections[I].FirstParagraphIndex := ANewSection.LastParagraphIndex + 1;
    ASections.Insert(SectionIndex, ANewSection);
    ANewSection.SubscribeHeadersFootersEvents;
    DocumentModel.OnSectionInserted(SectionIndex);
  end;
  FDeletedSections.OwnsObjects := False;
  FreeAndNil(FDeletedSections);
end;

procedure TdxSectionsDeletedHistoryItem.RedoCore;
var
  ASections: TdxCustomSectionCollection;
  ASection: TdxCustomSection;
  ACount, I: TdxSectionIndex;
  AFirstParagraphIndex: TdxParagraphIndex;
begin
  FDeletedSections := TdxCustomSectionCollection.Create;
  ASections := DocumentModel.Sections;
  ACount := SectionIndex + DeletedSectionsCount;
  for I := ACount - 1 downto SectionIndex do
  begin
    ASection := ASections[I];
    FDeletedSections.Add(ASection);

    AFirstParagraphIndex := ASection.FirstParagraphIndex;
    DocumentModel.OnSectionRemoved(I);
    ASection.UnsubscribeHeadersFootersEvents;
    ASections.Extract(ASection);
    ASections[I].FirstParagraphIndex := AFirstParagraphIndex;
  end;
end;

end.
