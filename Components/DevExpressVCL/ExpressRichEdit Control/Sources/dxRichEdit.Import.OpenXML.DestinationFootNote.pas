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

unit dxRichEdit.Import.OpenXML.DestinationFootNote;

{$I cxVer.inc}
{$I dxRichEditControl.inc}


interface

uses
  Types, SysUtils, Classes, Generics.Defaults, Generics.Collections,

  dxGenerics,
  dxRichEdit.Utils.Types,
  dxXMLReader,
  dxRichEdit.Import,
  dxRichEdit.DocumentModel.Section,
  dxRichEdit.DocumentModel.SectionFormatting,
  dxRichEdit.DocumentModel.NumberingFormatting,
  dxRichEdit.DocumentModel.NotesRange,
  dxRichEdit.DocumentModel.Notes,
  dxRichEdit.DocumentModel.TextRange,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.Import.OpenXML,
  dxRichEdit.Import.OpenXML.DestinationBase,
  dxRichEdit.Import.OpenXML.DestinationBody,
  dxRichEdit.Import.OpenXML.WordProcessingMLBaseImporter;

type

  { TdxFootNotePropertiesDestinationBase }

  TdxFootNotePropertiesDestinationBase = class abstract(TdxElementDestination)
  strict private
    class var
      FHandlerTable: TdxElementHandlerTable;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateElementHandlerTable: TdxElementHandlerTable; static;
  strict private
    FFootNote: TdxSectionFootNote;
  protected
    function GetElementHandlerTable: TdxElementHandlerTable; override;
    class function GetThis(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxFootNotePropertiesDestinationBase; static;
    class function OnPlacement(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnNumberingStart(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnNumberingFormat(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnNumberingRestart(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    function CreatePlacementDestination(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxDestination; virtual; abstract;
    function CreateNumberingFormatDestination(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxDestination; virtual; abstract;

    property FootNote: TdxSectionFootNote read FFootNote;
  public
    constructor Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AFootNote: TdxSectionFootNote);
  end;

  { TdxDocumentLevelFootNotePropertiesDestination }

  TdxDocumentLevelFootNotePropertiesDestination = class(TdxFootNotePropertiesDestinationBase)
  protected
    function CreatePlacementDestination(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxDestination; override;
    function CreateNumberingFormatDestination(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxDestination; override;
  end;

  { TdxSectionLevelFootNotePropertiesDestination }

  TdxSectionLevelFootNotePropertiesDestination = class(TdxFootNotePropertiesDestinationBase)
  protected
    function CreatePlacementDestination(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxDestination; override;
    function CreateNumberingFormatDestination(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxDestination; override;
  end;

  { TdxFootNotePropertiesLeafElementDestination }

  TdxFootNotePropertiesLeafElementDestination = class abstract(TdxLeafElementDestination)
  strict private
    FFootNote: TdxSectionFootNote;
  public
    constructor Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AFootNote: TdxSectionFootNote);

    property FootNote: TdxSectionFootNote read FFootNote;
  end;

  { TdxFootNotePlacementDestination }

  TdxFootNotePlacementDestination = class(TdxFootNotePropertiesLeafElementDestination)
  strict private
    FDefaultPosition: TdxFootNotePosition;
  public
    constructor Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AFootNote: TdxSectionFootNote; ADefaultPosition: TdxFootNotePosition);
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxFootNoteNumberingStartDestination }

  TdxFootNoteNumberingStartDestination = class(TdxFootNotePropertiesLeafElementDestination)
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxFootNoteNumberingFormatDestination }

  TdxFootNoteNumberingFormatDestination = class(TdxFootNotePropertiesLeafElementDestination)
  strict private
    FDefaultNumberingFormat: TdxNumberingFormat;
  public
    constructor Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AFootNote: TdxSectionFootNote; ADefaultNumberingFormat: TdxNumberingFormat);
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxFootNoteNumberingRestartTypeDestination }

  TdxFootNoteNumberingRestartTypeDestination = class(TdxFootNotePropertiesLeafElementDestination)
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxFootNotesDestination }

  TdxFootNotesDestination = class(TdxElementDestination)
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

  { TdxFootNoteDestinationBase }

  TdxFootNoteDestinationBase = class abstract(TdxBodyDestinationBase)
  strict private
    class var
      FHandlerTable: TdxElementHandlerTable;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateElementHandlerTable: TdxElementHandlerTable; static;
  strict private
    FId: string;
    FNewFootNote: TdxFootNoteBase;
    FSelfReferenceRuns: TdxFootNoteRunBaseList;
  protected
    function GetElementHandlerTable: TdxElementHandlerTable; override;
    class function GetThis(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxFootNoteDestinationBase; static;
    function CreateTargetFootNoteBase: TdxFootNoteBase; virtual; abstract;
    function RegisterNote(ANote: TdxFootNoteBase; const AId: string): Integer; virtual; abstract;
    function OnFootNoteReference(AReader: TdxXmlReader): TdxDestination; overload; virtual; abstract;
    function OnEndNoteReference(AReader: TdxXmlReader): TdxDestination; overload; virtual; abstract;

    property SelfReferenceRuns: TdxFootNoteRunBaseList read FSelfReferenceRuns;
  public
    constructor Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter);
    destructor Destroy; override;
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
    procedure ProcessElementClose(AReader: TdxXmlReader); override;
    class function OnTable(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnParagraph(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; static;
    class function OnFootNoteReference(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; overload; static;
    class function OnEndNoteReference(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination; overload; static;
  end;

  { TdxFootNoteDestination }

  TdxFootNoteDestination = class(TdxFootNoteDestinationBase)
  protected
    function CreateTargetFootNoteBase: TdxFootNoteBase; override;
    function RegisterNote(ANote: TdxFootNoteBase; const AId: string): Integer; override;
  public
    function OnFootNoteReference(AReader: TdxXmlReader): TdxDestination; override;
    function OnEndNoteReference(AReader: TdxXmlReader): TdxDestination; override;
  end;

  { TdxFootNoteReferenceDestination }

  TdxFootNoteReferenceDestination = class(TdxLeafElementDestination)
  public
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

  { TdxFootNoteSelfReferenceDestination }

  TdxFootNoteSelfReferenceDestination = class(TdxLeafElementDestination)
  strict private
    FSelfReferenceRuns: TdxFootNoteRunBaseList;
  public
    constructor Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; ASelfReferenceRuns: TdxFootNoteRunBaseList);
    procedure ProcessElementOpen(AReader: TdxXmlReader); override;
  end;

implementation

uses
  Contnrs,
  dxRichEdit.Import.OpenXML.DestinationTable,
  dxRichEdit.Export.OpenXML;

{ TdxFootNotePropertiesDestinationBase }

constructor TdxFootNotePropertiesDestinationBase.Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AFootNote: TdxSectionFootNote);
begin
  inherited Create(AImporter);
  Assert(AFootNote <> nil, 'footNote');
  FFootNote := AFootNote;
end;

class constructor TdxFootNotePropertiesDestinationBase.Initialize;
begin
  FHandlerTable := CreateElementHandlerTable;
end;

class destructor TdxFootNotePropertiesDestinationBase.Finalize;
begin
  FHandlerTable.Free;
end;

class function TdxFootNotePropertiesDestinationBase.CreateElementHandlerTable: TdxElementHandlerTable;
begin
  Result := TdxElementHandlerTable.Create;
  Result.Add('pos', OnPlacement);
  Result.Add('numStart', OnNumberingStart);
  Result.Add('numFmt', OnNumberingFormat);
  Result.Add('numRestart', OnNumberingRestart);
end;

function TdxFootNotePropertiesDestinationBase.GetElementHandlerTable: TdxElementHandlerTable;
begin
  Result := FHandlerTable;
end;

class function TdxFootNotePropertiesDestinationBase.GetThis(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxFootNotePropertiesDestinationBase;
begin
  Result := TdxFootNotePropertiesDestinationBase(AImporter.PeekDestination);
end;

class function TdxFootNotePropertiesDestinationBase.OnPlacement(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := GetThis(AImporter).CreatePlacementDestination(AImporter);
end;

class function TdxFootNotePropertiesDestinationBase.OnNumberingStart(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxFootNoteNumberingStartDestination.Create(AImporter, GetThis(AImporter).FootNote);
end;

class function TdxFootNotePropertiesDestinationBase.OnNumberingFormat(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := GetThis(AImporter).CreateNumberingFormatDestination(AImporter);
end;

class function TdxFootNotePropertiesDestinationBase.OnNumberingRestart(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxFootNoteNumberingRestartTypeDestination.Create(AImporter, GetThis(AImporter).FootNote);
end;

{ TdxDocumentLevelFootNotePropertiesDestination }

function TdxDocumentLevelFootNotePropertiesDestination.CreatePlacementDestination(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxDestination;
begin
  Result := TdxFootNotePlacementDestination.Create(AImporter, FootNote, TdxFootNotePosition.BottomOfPage);
end;

function TdxDocumentLevelFootNotePropertiesDestination.CreateNumberingFormatDestination(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxDestination;
begin
  Result := TdxFootNoteNumberingFormatDestination.Create(AImporter, FootNote, TdxNumberingFormat.Decimal);
end;

{ TdxSectionLevelFootNotePropertiesDestination }

function TdxSectionLevelFootNotePropertiesDestination.CreatePlacementDestination(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxDestination;
begin
  Result := TdxFootNotePlacementDestination.Create(AImporter, FootNote, TdxFootNotePosition.BottomOfPage);
end;

function TdxSectionLevelFootNotePropertiesDestination.CreateNumberingFormatDestination(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxDestination;
begin
  Result := TdxFootNoteNumberingFormatDestination.Create(AImporter, FootNote, TdxNumberingFormat.Decimal);
end;

{ TdxFootNotePropertiesLeafElementDestination }

constructor TdxFootNotePropertiesLeafElementDestination.Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AFootNote: TdxSectionFootNote);
begin
  inherited Create(AImporter);
  Assert(AFootNote <> nil, 'footNote');
  FFootNote := AFootNote;
end;

{ TdxFootNotePlacementDestination }

constructor TdxFootNotePlacementDestination.Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  AFootNote: TdxSectionFootNote; ADefaultPosition: TdxFootNotePosition);
begin
  inherited Create(AImporter, AFootNote);
  FDefaultPosition := ADefaultPosition;
end;

procedure TdxFootNotePlacementDestination.ProcessElementOpen(AReader: TdxXmlReader);
begin
  FootNote.Position := TdxWordProcessingMLBaseImporter(Importer).GetWpEnumValue<TdxFootNotePosition>(AReader, 'val',
    TdxOpenXmlExporter.FootNotePlacementTable, FDefaultPosition);
end;

{ TdxFootNoteNumberingStartDestination }

procedure TdxFootNoteNumberingStartDestination.ProcessElementOpen(AReader: TdxXmlReader);
begin
  FootNote.StartingNumber := Importer.GetWpSTIntegerValue(AReader, 'val', 1);
end;

{ TdxFootNoteNumberingFormatDestination }

constructor TdxFootNoteNumberingFormatDestination.Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AFootNote: TdxSectionFootNote; ADefaultNumberingFormat: TdxNumberingFormat);
begin
  inherited Create(AImporter, AFootNote);
  FDefaultNumberingFormat := ADefaultNumberingFormat;
end;

procedure TdxFootNoteNumberingFormatDestination.ProcessElementOpen(AReader: TdxXmlReader);
begin
  FootNote.NumberingFormat := TdxWordProcessingMLBaseImporter(Importer).GetWpEnumValue<TdxNumberingFormat>(AReader, 'val',
    TdxOpenXmlExporter.PageNumberingFormatTable, FDefaultNumberingFormat);
end;

{ TdxFootNoteNumberingRestartTypeDestination }

procedure TdxFootNoteNumberingRestartTypeDestination.ProcessElementOpen(AReader: TdxXmlReader);
begin
  FootNote.NumberingRestartType := TdxWordProcessingMLBaseImporter(Importer).GetWpEnumValue<TdxLineNumberingRestart>(AReader, 'val',
    TdxOpenXmlExporter.LineNumberingRestartTable, TdxLineNumberingRestart.Continuous);
end;

{ TdxFootNotesDestination }

class constructor TdxFootNotesDestination.Initialize;
begin
  FHandlerTable := CreateElementHandlerTable;
end;

class destructor TdxFootNotesDestination.Finalize;
begin
  FHandlerTable.Free;
end;

class function TdxFootNotesDestination.CreateElementHandlerTable: TdxElementHandlerTable;
begin
  Result := TdxElementHandlerTable.Create;
  Result.Add('footnote', OnFootNote);
end;

function TdxFootNotesDestination.GetElementHandlerTable: TdxElementHandlerTable;
begin
  Result := FHandlerTable;
end;

class function TdxFootNotesDestination.OnFootNote(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxFootNoteDestination.Create(AImporter);
end;

{ TdxFootNoteDestinationBase }

constructor TdxFootNoteDestinationBase.Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter);
begin
  inherited Create(AImporter);
  FSelfReferenceRuns := TdxFootNoteRunBaseList.Create;
end;

destructor TdxFootNoteDestinationBase.Destroy;
begin
  FSelfReferenceRuns.Free;
  inherited Destroy;
end;

class constructor TdxFootNoteDestinationBase.Initialize;
begin
  FHandlerTable := CreateElementHandlerTable;
end;

class destructor TdxFootNoteDestinationBase.Finalize;
begin
  FHandlerTable.Free;
end;

class function TdxFootNoteDestinationBase.CreateElementHandlerTable: TdxElementHandlerTable;
begin
  Result := TdxElementHandlerTable.Create;
  Result.Add('p', OnParagraph);
  Result.Add('tbl', OnTable);
  Result.Add('bookmarkStart', OnBookmarkStart);
  Result.Add('bookmarkEnd', OnBookmarkEnd);
  Result.Add('permStart', OnRangePermissionStart);
  Result.Add('permEnd', OnRangePermissionEnd);
  Result.Add('sdt', OnStructuredDocument);
  Result.Add('customXml', OnCustomXml);
end;

function TdxFootNoteDestinationBase.GetElementHandlerTable: TdxElementHandlerTable;
begin
  Result := FHandlerTable;
end;

class function TdxFootNoteDestinationBase.GetThis(AImporter: TdxRichEditDestinationAndXmlBasedImporter): TdxFootNoteDestinationBase;
begin
  Result := TdxFootNoteDestinationBase(AImporter.PeekDestination);
end;

procedure TdxFootNoteDestinationBase.ProcessElementOpen(AReader: TdxXmlReader);
var
  APieceTable: TdxPieceTable;
begin
  FNewFootNote := CreateTargetFootNoteBase;
  APieceTable := TdxPieceTable(FNewFootNote.PieceTable);
  DocumentModel.UnsafeEditor.InsertFirstParagraph(APieceTable);
  Importer.PushCurrentPieceTable(APieceTable);
  FId := AReader.GetAttribute('id', Importer.WordProcessingNamespaceConst);
end;

procedure TdxFootNoteDestinationBase.ProcessElementClose(AReader: TdxXmlReader);
var
  ANoteIndex, ACount, I: Integer;
begin
  PieceTable.FixLastParagraph;
  Importer.InsertBookmarks;
  Importer.InsertRangePermissions;
  PieceTable.FixTables;

  Importer.PopCurrentPieceTable;

  ANoteIndex := RegisterNote(FNewFootNote, FId);
  ACount := FSelfReferenceRuns.Count;
  for I := 0 to ACount - 1 do
    FSelfReferenceRuns[I].NoteIndex := ANoteIndex;
end;

class function TdxFootNoteDestinationBase.OnTable(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  if AImporter.DocumentModel.DocumentCapabilities.TablesAllowed then
    Result := TdxTableDestination.Create(AImporter)
  else
    Result := TdxTableDisabledDestination.Create(AImporter);
end;

class function TdxFootNoteDestinationBase.OnParagraph(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxWordProcessingMLBaseImporter(AImporter).CreateParagraphDestination;
end;

class function TdxFootNoteDestinationBase.OnFootNoteReference(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := GetThis(AImporter).OnFootNoteReference(AReader);
end;

class function TdxFootNoteDestinationBase.OnEndNoteReference(AImporter: TdxRichEditDestinationAndXmlBasedImporter; AReader: TdxXmlReader): TdxDestination;
begin
  Result := GetThis(AImporter).OnEndNoteReference(AReader);
end;

{ TdxFootNoteDestination }

function TdxFootNoteDestination.CreateTargetFootNoteBase: TdxFootNoteBase;
begin
  Result := TdxFootNote.Create(DocumentModel);
end;

function TdxFootNoteDestination.RegisterNote(ANote: TdxFootNoteBase; const AId: string): Integer;
begin
  Result := TdxWordProcessingMLBaseImporter(Importer).RegisterFootNote(TdxFootNote(ANote), AId);
end;

function TdxFootNoteDestination.OnFootNoteReference(AReader: TdxXmlReader): TdxDestination;
begin
  Result := TdxFootNoteSelfReferenceDestination.Create(Importer, SelfReferenceRuns);
end;

function TdxFootNoteDestination.OnEndNoteReference(AReader: TdxXmlReader): TdxDestination;
begin
  Result := nil;
end;

{ TdxFootNoteReferenceDestination }

procedure TdxFootNoteReferenceDestination.ProcessElementOpen(AReader: TdxXmlReader);
var
  AImporter: TdxOpenXmlImporter;
  ANoteId: string;
  ANote: TdxFootNote;
  ANoteIndex: Integer;
begin
  AImporter := TdxOpenXmlImporter(Importer);
  ANoteId := AReader.GetAttribute('id', Importer.WordProcessingNamespaceConst);
  if not AImporter.FootNotes.TryGetValue(ANoteId, ANote) then
    Exit;

  ANoteIndex := DocumentModel.FootNotes.IndexOf(ANote);
  Assert(ANoteIndex >= 0);
  PieceTable.InsertFootNoteRun(AImporter.Position, ANoteIndex);
end;

{ TdxFootNoteSelfReferenceDestination }

constructor TdxFootNoteSelfReferenceDestination.Create(AImporter: TdxRichEditDestinationAndXmlBasedImporter;
  ASelfReferenceRuns: TdxFootNoteRunBaseList);
begin
  inherited Create(AImporter);
  Assert(ASelfReferenceRuns <> nil, 'selfReferenceRuns');
  FSelfReferenceRuns := ASelfReferenceRuns;
end;

procedure TdxFootNoteSelfReferenceDestination.ProcessElementOpen(AReader: TdxXmlReader);
var
  ARun: TdxFootNoteRun;
begin
  ARun := TdxFootNoteRun(PieceTable.InsertFootNoteRun(Importer.Position, -1));
  FSelfReferenceRuns.Add(ARun);
end;

end.
