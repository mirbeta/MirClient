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

unit dxRichEdit.DocumentModel.Notes;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, Classes, Generics.Defaults, Generics.Collections,

  dxCoreClasses, dxGenerics,
  dxRichEdit.Utils.Types,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.Types;

type
  { TdxFootNoteBase }

  TdxFootNoteBase = class(TdxContentTypeBase)
  strict private
    FReferenceRun: TObject;
  protected
    function GetIsMain: Boolean; override;
    function GetIsHeaderFooter: Boolean; override;
    function GetIsFooter: Boolean; override;
    function GetIsHeader: Boolean; override;
    function GetIsNote: Boolean; override;
    function GetIsReferenced: Boolean; override;
  public
    function LookupSectionIndexByParagraphIndex(AParagraphIndex: TdxParagraphIndex): TdxSectionIndex; override;
    procedure FixLastParagraphOfLastSection(AOriginalParagraphCount: Integer); override;

    property ReferenceRun: TObject read FReferenceRun write FReferenceRun;
  end;

  { TdxFootNoteBaseCollection }

  TdxFootNoteBaseCollection = class(TdxObjectList<TdxFootNoteBase>)
  public
    procedure GetPieceTables(AResult: TdxFastList; AIncludeUnreferenced: Boolean);
  end;

  { TdxFootNote }

  TdxFootNote = class(TdxFootNoteBase)
  public const
    FootNoteCounterId = '_counter_footnotes';
  protected
    function GetIsFootNote: Boolean; override;
    function GetIsEndNote: Boolean; override;
  end;

  { TdxFootNoteCollection }

  TdxFootNoteCollection = class(TdxFootNoteBaseCollection)
  private
    function GetItem(Index: Integer): TdxFootNote;
  public
    property Items[Index: Integer]: TdxFootNote read GetItem; default;
  end;

  { TdxEndNote }

  TdxEndNote = class(TdxFootNoteBase)
  public const
    EndNoteCounterId = '_counter_endnotes';
  protected
    function GetIsFootNote: Boolean; override;
    function GetIsEndNote: Boolean; override;
  end;

  { TdxEndNoteCollection }

  TdxEndNoteCollection = class(TdxFootNoteBaseCollection)
  private
    function GetItem(Index: Integer): TdxEndNote;
  public
    property Items[Index: Integer]: TdxEndNote read GetItem; default;
  end;

implementation

{ TdxFootNoteBase }

function TdxFootNoteBase.GetIsMain: Boolean;
begin
  Result := False;
end;

function TdxFootNoteBase.GetIsHeaderFooter: Boolean;
begin
  Result := False;
end;

function TdxFootNoteBase.GetIsFooter: Boolean;
begin
  Result := False;
end;

function TdxFootNoteBase.GetIsHeader: Boolean;
begin
  Result := False;
end;

function TdxFootNoteBase.GetIsNote: Boolean;
begin
  Result := True;
end;

function TdxFootNoteBase.GetIsReferenced: Boolean;
begin
  Result := ReferenceRun <> nil;
end;

function TdxFootNoteBase.LookupSectionIndexByParagraphIndex(AParagraphIndex: TdxParagraphIndex): TdxSectionIndex;
begin
  Result := dxSectionIndexDontCare;
end;

procedure TdxFootNoteBase.FixLastParagraphOfLastSection(AOriginalParagraphCount: Integer);
begin
end;

{ TdxFootNoteBaseCollection<T> }

procedure TdxFootNoteBaseCollection.GetPieceTables(AResult: TdxFastList;
  AIncludeUnreferenced: Boolean);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].PieceTable.AddPieceTables(AResult, AIncludeUnreferenced);
end;

{ TdxFootNote }

function TdxFootNote.GetIsFootNote: Boolean;
begin
  Result := True;
end;

function TdxFootNote.GetIsEndNote: Boolean;
begin
  Result := False;
end;

{ TdxEndNote }

function TdxEndNote.GetIsFootNote: Boolean;
begin
  Result := False;
end;

function TdxEndNote.GetIsEndNote: Boolean;
begin
  Result := True;
end;

{ TdxFootNoteCollection }

function TdxFootNoteCollection.GetItem(Index: Integer): TdxFootNote;
begin
  Result := TdxFootNote(inherited Items[Index]);
end;

{ TdxEndNoteCollection }

function TdxEndNoteCollection.GetItem(Index: Integer): TdxEndNote;
begin
  Result := TdxEndNote(inherited Items[Index]);
end;

end.
