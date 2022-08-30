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

unit dxRichEdit.Import.Rtf.DestinationFootNote;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, SysUtils, Classes, Contnrs, Generics.Defaults, Generics.Collections, dxCoreClasses,

  dxGenerics,
  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.Notes,
  dxRichEdit.DocumentModel.NotesRange,
  dxRichEdit.DocumentModel.TextRange,
  dxRichEdit.Import.Rtf,
  dxRichEdit.Import.Rtf.DestinationPieceTable;

type
  { TdxFootNoteDestinationBase }

  TdxFootNoteDestinationBase = class abstract(TdxDestinationPieceTable)
  strict private
    class var
      FKeywordHT: TdxKeywordTranslatorTable;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateKeywordTable: TdxKeywordTranslatorTable; static;
  strict private
    FSelfReferenceRuns: TdxFootNoteRunBaseList;
    FIsSelfReferenceRunsOwned: Boolean;
    function GetNote: TdxFootNoteBase;
    class procedure ConvertToEndNoteHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure FootNoteSelfReferenceHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
  protected
    class function GetKeywordHT: TdxKeywordTranslatorTable; override;
    procedure OnSelfReference; virtual; abstract;
    function GetNoteCollection: TdxFootNoteBaseCollection; virtual; abstract;

    property SelfReferenceRuns: TdxFootNoteRunBaseList read FSelfReferenceRuns;
    property Note: TdxFootNoteBase read GetNote;
  public
    constructor Create(AImporter: TdxRtfImporter; ATargetPieceTable: TdxFootNoteBase); reintroduce; overload;
    constructor Create(AImporter: TdxRtfImporter; AFootNoteContentType: TdxFootNoteBase;
      ASelfReferenceRuns: TdxFootNoteRunBaseList); reintroduce; overload;
    destructor Destroy; override;
    procedure FinalizePieceTableCreation; override;
  end;

  { TdxFootNoteDestination }

  TdxFootNoteDestination = class(TdxFootNoteDestinationBase)
  protected
    procedure OnSelfReference; override;
    function GetNoteCollection: TdxFootNoteBaseCollection; override;
  public
    function CreateClone: TdxRichEditRtfDestinationBase; override;
  end;

  { TdxEndNoteDestination }

  TdxEndNoteDestination = class(TdxFootNoteDestinationBase)
  protected
    procedure OnSelfReference; override;
    function GetNoteCollection: TdxFootNoteBaseCollection; override;
  public
    function CreateClone: TdxRichEditRtfDestinationBase; override;
  end;

implementation

{ TdxFootNoteDestinationBase }

class constructor TdxFootNoteDestinationBase.Initialize;
begin
  FKeywordHT := CreateKeywordTable;
end;

class destructor TdxFootNoteDestinationBase.Finalize;
begin
  FreeAndNil(FKeywordHT);
end;

constructor TdxFootNoteDestinationBase.Create(AImporter: TdxRtfImporter; ATargetPieceTable: TdxFootNoteBase);
begin
  Create(AImporter, ATargetPieceTable, TdxFootNoteRunBaseList.Create);
  FIsSelfReferenceRunsOwned := True;
end;

constructor TdxFootNoteDestinationBase.Create(AImporter: TdxRtfImporter;
  AFootNoteContentType: TdxFootNoteBase; ASelfReferenceRuns: TdxFootNoteRunBaseList);
begin
  inherited Create(AImporter, TdxPieceTable(AFootNoteContentType.PieceTable));
  FIsSelfReferenceRunsOwned := False;
  FSelfReferenceRuns := ASelfReferenceRuns;
end;

destructor TdxFootNoteDestinationBase.Destroy;
begin
  if FIsSelfReferenceRunsOwned then
    FreeAndNil(FSelfReferenceRuns);
  inherited Destroy;
end;

class function TdxFootNoteDestinationBase.CreateKeywordTable: TdxKeywordTranslatorTable;
begin
  Result := TdxKeywordTranslatorTable.Create;
  AddCommonCharacterKeywords(Result);
  AddCommonParagraphKeywords(Result);
  AddCommonSymbolsAndObjectsKeywords(Result);
  AddCommonTabKeywords(Result);
  AddCommonNumberingListsKeywords(Result);
  AppendTableKeywords(Result);
  Result.Add('chftn', FootNoteSelfReferenceHandler);
  Result.Add('ftnalt', ConvertToEndNoteHandler);
end;

class function TdxFootNoteDestinationBase.GetKeywordHT: TdxKeywordTranslatorTable;
begin
  Result := FKeywordHT;
end;

function TdxFootNoteDestinationBase.GetNote: TdxFootNoteBase;
begin
  Result := TdxFootNoteBase(PieceTable.ContentType);
end;

procedure TdxFootNoteDestinationBase.FinalizePieceTableCreation;
var
  ANotes: TdxFootNoteBaseCollection;
  AIndex, ACount, I: Integer;
begin
  inherited FinalizePieceTableCreation;

  ANotes := GetNoteCollection;
  AIndex := ANotes.Count;
  ANotes.Add(Note);

  ACount := FSelfReferenceRuns.Count;
  for I := 0 to ACount - 1 do
    FSelfReferenceRuns[I].NoteIndex := AIndex;
end;

class procedure TdxFootNoteDestinationBase.ConvertToEndNoteHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
var
  ANote: TdxEndNote;
begin
  if AImporter.Destination is TdxEndNoteDestination then
    Exit;

  ANote := TdxEndNote.Create(AImporter.DocumentModel);
  AImporter.DocumentModel.UnsafeEditor.InsertFirstParagraph(TdxPieceTable(ANote.PieceTable));
  AImporter.Destination := TdxEndNoteDestination.Create(AImporter, ANote);
end;

class procedure TdxFootNoteDestinationBase.FootNoteSelfReferenceHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
var
  AThisDestination: TdxFootNoteDestinationBase;
begin
  AThisDestination := TdxFootNoteDestinationBase(AImporter.Destination);
  AThisDestination.OnSelfReference;
end;

{ TdxFootNoteDestination }

function TdxFootNoteDestination.CreateClone: TdxRichEditRtfDestinationBase;
begin
  Result := TdxFootNoteDestination.Create(Importer, Note, SelfReferenceRuns);
end;

procedure TdxFootNoteDestination.OnSelfReference;
var
  ARun: TdxFootNoteRun;
begin
  ARun := TdxFootNoteRun(TdxPieceTable(Note.PieceTable).InsertFootNoteRun(Importer.Position, -1));
  SelfReferenceRuns.Add(ARun);
end;

function TdxFootNoteDestination.GetNoteCollection: TdxFootNoteBaseCollection;
begin
  Result := DocumentModel.FootNotes;
end;

{ TdxEndNoteDestination }

function TdxEndNoteDestination.CreateClone: TdxRichEditRtfDestinationBase;
begin
  Result := TdxEndNoteDestination.Create(Importer, Note, SelfReferenceRuns);
end;

procedure TdxEndNoteDestination.OnSelfReference;
var
  ARun: TdxEndNoteRun;
begin
  ARun := TdxEndNoteRun(TdxPieceTable(Note.PieceTable).InsertEndNoteRun(Importer.Position, -1));
  SelfReferenceRuns.Add(ARun);
end;

function TdxEndNoteDestination.GetNoteCollection: TdxFootNoteBaseCollection;
begin
  Result := DocumentModel.EndNotes;
end;

end.
