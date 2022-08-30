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

unit dxRichEdit.Import.OpenXML.DestinationEndNote;

{$I cxVer.inc}
{$I dxRichEditControl.inc}


interface

uses
  Types, SysUtils, Classes, Generics.Defaults, Generics.Collections,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.Section,
  dxRichEdit.DocumentModel.NumberingFormatting,
  dxRichEdit.DocumentModel.NotesRange,
  dxRichEdit.DocumentModel.Notes,
  dxRichEdit.DocumentModel.TextRange,
  dxRichEdit.Import,
  dxRichEdit.Import.OpenXML.DestinationBase,
  dxRichEdit.Import.OpenXML.DestinationFootNote,
  dxRichEdit.Import.OpenXML.WordProcessingMLBaseImporter,
  dxRichEdit.Import.OpenXML,
  dxGenerics,
  dxXMLReader;

type

  { TdxDocumentLevelEndNotePropertiesDestination }

  TdxDocumentLevelEndNotePropertiesDestination = class(TdxFootNotePropertiesDestinationBase)
  protected
    function CreatePlacementDestination(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxDestination; override;
    function CreateNumberingFormatDestination(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxDestination; override;
  end;

  { TdxSectionLevelEndNotePropertiesDestination }

  TdxSectionLevelEndNotePropertiesDestination = class(TdxFootNotePropertiesDestinationBase)
  protected
    function CreatePlacementDestination(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxDestination; override;
    function CreateNumberingFormatDestination(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxDestination; override;
  end;

  { TdxEndNotesDestination }

  TdxEndNotesDestination = class(TdxElementDestination)
  strict private
    class var
      FHandlerTable: TdxElementHandlerTable;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateElementHandlerTable: TdxElementHandlerTable; static;
  protected
    function GetElementHandlerTable: TdxElementHandlerTable; override;
    class function OnFootNote(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
  end;

  { TdxEndNoteDestination }

  TdxEndNoteDestination = class(TdxFootNoteDestinationBase)
  protected
    function CreateTargetFootNoteBase: TdxFootNoteBase; override;
    function RegisterNote(ANote: TdxFootNoteBase; const AId: string): Integer; override;
  public
    function OnFootNoteReference(AReader: TdxXmlReader): TdxDestination; override;
    function OnEndNoteReference(AReader: TdxXmlReader): TdxDestination; override;
  end;

  { TdxEndNoteReferenceDestination }

  TdxEndNoteReferenceDestination = class(TdxLeafElementDestination)
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxEndNoteSelfReferenceDestination }

  TdxEndNoteSelfReferenceDestination = class(TdxLeafElementDestination)
  strict private
    FSelfReferenceRuns: TdxFootNoteRunBaseList;
  public
    constructor Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; ASelfReferenceRuns: TdxFootNoteRunBaseList);
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

implementation

uses
  Contnrs,
  dxRichEdit.Utils.Types;

{ TdxDocumentLevelEndNotePropertiesDestination }

function TdxDocumentLevelEndNotePropertiesDestination.CreatePlacementDestination(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxDestination;
begin
  Result := TdxFootNotePlacementDestination.Create(AImporter, FootNote, TdxFootNotePosition.EndOfDocument);
end;

function TdxDocumentLevelEndNotePropertiesDestination.CreateNumberingFormatDestination(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxDestination;
begin
  Result := TdxFootNoteNumberingFormatDestination.Create(AImporter, FootNote, TdxNumberingFormat.LowerRoman);
end;

{ TdxSectionLevelEndNotePropertiesDestination }

function TdxSectionLevelEndNotePropertiesDestination.CreatePlacementDestination(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxDestination;
begin
  Result := TdxFootNotePlacementDestination.Create(AImporter, FootNote, TdxFootNotePosition.EndOfDocument);
end;

function TdxSectionLevelEndNotePropertiesDestination.CreateNumberingFormatDestination(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxDestination;
begin
  Result := TdxFootNoteNumberingFormatDestination.Create(AImporter, FootNote, TdxNumberingFormat.LowerRoman);
end;

{ TdxEndNotesDestination }

class constructor TdxEndNotesDestination.Initialize;
begin
  FHandlerTable := CreateElementHandlerTable;
end;

class destructor TdxEndNotesDestination.Finalize;
begin
  FHandlerTable.Free;
end;

class function TdxEndNotesDestination.CreateElementHandlerTable: TdxElementHandlerTable;
begin
  Result := TdxElementHandlerTable.Create;
  Result.Add('endnote', OnFootNote);
end;

function TdxEndNotesDestination.GetElementHandlerTable: TdxElementHandlerTable;
begin
  Result := FHandlerTable;
end;

class function TdxEndNotesDestination.OnFootNote(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxEndNoteDestination.Create(AImporter);
end;

{ TdxEndNoteDestination }

function TdxEndNoteDestination.CreateTargetFootNoteBase: TdxFootNoteBase;
begin
  Result := TdxEndNote.Create(DocumentModel);
end;

function TdxEndNoteDestination.RegisterNote(ANote: TdxFootNoteBase; const AId: string): Integer;
begin
  Result := TdxWordProcessingMLBaseImporter(Importer).RegisterEndNote(TdxEndNote(ANote), AId);
end;

function TdxEndNoteDestination.OnFootNoteReference(AReader: TdxXmlReader): TdxDestination;
begin
  Result := nil;
end;

function TdxEndNoteDestination.OnEndNoteReference(AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxEndNoteSelfReferenceDestination.Create(Importer, SelfReferenceRuns);
end;

{ TdxEndNoteReferenceDestination }

procedure TdxEndNoteReferenceDestination.ProcessElementOpen(AReader: TdxXmlReader);
var
  AImporter: TdxOpenXmlImporter;
  ANoteId: string;
  ANote: TdxEndNote;
  ANoteIndex: Integer;
begin
  AImporter := TdxOpenXmlImporter(Importer);
  ANoteId := AReader.GetAttribute('id', Importer.WordProcessingNamespaceConst);
  if not AImporter.EndNotes.TryGetValue(ANoteId, ANote) then
    Exit;

  ANoteIndex := DocumentModel.EndNotes.IndexOf(ANote);
  Assert(ANoteIndex >= 0);
  PieceTable.InsertEndNoteRun(AImporter.Position, ANoteIndex);
end;

{ TdxEndNoteSelfReferenceDestination }

constructor TdxEndNoteSelfReferenceDestination.Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  ASelfReferenceRuns: TdxFootNoteRunBaseList);
begin
  inherited Create(AImporter);
  Assert(ASelfReferenceRuns <> nil, 'selfReferenceRuns');
  FSelfReferenceRuns := ASelfReferenceRuns;
end;

procedure TdxEndNoteSelfReferenceDestination.ProcessElementOpen(AReader: TdxXmlReader);
var
  ARun: TdxEndNoteRun;
begin
  ARun := TdxEndNoteRun(PieceTable.InsertEndNoteRun(Importer.Position, -1));
  FSelfReferenceRuns.Add(ARun);
end;

end.
