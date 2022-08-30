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
unit dxRichEdit.Import.Doc.DocContentIterator;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, Classes, Generics.Defaults, Generics.Collections,
  dxCore, dxGenerics, dxCoreGraphics,
  dxRichEdit.Utils.Types,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.FloatingObjectFormatting,
  dxRichEdit.Doc.Utils,
  dxRichEdit.Import.Doc.FileInformationBlock,
  dxRichEdit.Import.Doc.FileShapeAddress,
  dxRichEdit.Import.Doc.BreakDescriptor,
  dxRichEdit.Import.Doc.BlipContainer,
  dxRichEdit.Import.Doc.OfficeArtContent,
  dxRichEdit.Import.Doc.DocObjectCollection,
  dxRichEdit.Import.Doc.DocFieldsImportHelper,
  dxRichEdit.Import.Doc.DCO,
  dxRichEdit.Import.Doc.DocBookmark,
  dxRichEdit.Import.Doc.DocHeadersFooters,
  dxRichEdit.Import.Doc.DocRangeEditPermission,
  dxRichEdit.Import.Doc.DocComment;

type
  TdxDocContentIterator = class;

  { TdxDocFieldTable }

  TdxDocFieldTable = class
  public const
    FieldDescriptorSize = 2;
  strict private
    FCharacterPositions: TdxIntegerList;
    FFieldDescriptors: TList<IdxDocFieldDescriptor>;
  protected
    procedure Read(AReader: TBinaryReader; AOffset: Integer; ASize: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    class function FromStream(AReader: TBinaryReader; AOffset: Integer; ASize: Integer): TdxDocFieldTable; static;
    procedure Write(AWriter: TBinaryWriter);
    function GetFieldDescriptorByIndex(AIndex: Integer): IdxDocFieldDescriptor;
    procedure AddEntry(ACharacterPosition: Integer; const AFieldDescriptor: IdxDocFieldDescriptor);
    procedure Finish(ALastCharacterPosition: Integer);
  end;

  { TdxDocFieldsIterator }

  TdxDocFieldsIterator = class
  strict private
    class var
      FUnsupportedFieldCodes: TdxStringList;
    class constructor Initialize;
    class destructor Finalize;
  strict private
    FShapeFieldName: string;
    FCurrentFieldIndex: Integer;
    FFieldBeginDescriptors: TStack<TdxDocFieldBeginDescriptor>;
    FFieldTable: TdxDocFieldTable;
    FShapeField: Boolean;
  public
    constructor Create(AReader: TBinaryReader; AOffset: Integer; ASize: Integer);
    destructor Destroy; override;
    function MoveNext(APropertyContainer: TdxDocPropertyContainer): Boolean;
    function GetCurrentFieldType: TdxFieldType;
    procedure CheckFieldCompatibility(ADestination: TdxDocObjectCollection);
    procedure Reset;
    function GetFieldObjects(ADocObjects: TdxDocObjectCollection): TStack<IdxDocObject>;
    function GetFieldCode(AFieldObjects: TStack<IdxDocObject>): string;
    procedure SetShapeField(ADestination: TdxDocObjectCollection);
    procedure ResetShapeField;

    property ShapeField: Boolean read FShapeField;
  end;

  { TdxDocIteratorState }

  TdxDocIteratorState = class abstract
  strict private
    FOwnsDestination: Boolean;
    FThreshold: Integer;
    FDestination: TdxDocObjectCollection;
    FFieldIterator: TdxDocFieldsIterator;
    procedure SetDestination(const AValue: TdxDocObjectCollection);
    procedure SetFieldIterator(const AValue: TdxDocFieldsIterator);
  protected
    procedure CreateNewDestination;
    function GetCurrentState: TdxDocContentState; virtual; abstract;
    function GetNextState: TdxDocContentState; virtual; abstract;
    function GetReadingComplete: Boolean; virtual;
    procedure AdvanceNext(AOffset: Integer); virtual;
    procedure UpdateDestination; virtual;
    procedure FixLastParagraph;
    procedure NormalizeFields; virtual;
    function ShouldNormalizeFields(ADocObjects: TdxDocObjectCollection): Boolean;
    procedure NormalizeFieldsCore(ADocObjects: TdxDocObjectCollection); virtual;

    property OwnsDestination: Boolean read FOwnsDestination write FOwnsDestination;
  public
    constructor Create(AThreshold: Integer; ADestination: TdxDocObjectCollection); overload; virtual;
    constructor Create(AThreshold: Integer); overload; virtual;
    destructor Destroy; override;
    function ShouldChangeState(ACharacterPosition: Integer): Boolean; virtual;
    procedure FinishState; virtual;

    property CurrentState: TdxDocContentState read GetCurrentState;
    property NextState: TdxDocContentState read GetNextState;
    property ReadingComplete: Boolean read GetReadingComplete;
    property Threshold: Integer read FThreshold;
    property Destination: TdxDocObjectCollection read FDestination write SetDestination;
    property FieldIterator: TdxDocFieldsIterator read FFieldIterator write SetFieldIterator;
  end;

  { TdxMainDocumentIteratorState }

  TdxMainDocumentIteratorState = class(TdxDocIteratorState)
  protected
    function GetCurrentState: TdxDocContentState; override;
    function GetNextState: TdxDocContentState; override;
  end;

  { TdxHeaderFooterEndIteratorState }

  TdxHeaderFooterEndIteratorState = class(TdxDocIteratorState)
  protected
    function GetCurrentState: TdxDocContentState; override;
    function GetNextState: TdxDocContentState; override;
  end;

  { TdxHeaderFooterIteratorStoryState }

  TdxHeaderFooterIteratorStoryState = class(TdxDocIteratorState)
  strict private
    FPositions: TdxDocHeadersFootersPositions;
  protected
    function GetCurrentState: TdxDocContentState; override;
    function GetNextState: TdxDocContentState; override;
  public
    constructor Create(AThreshold: Integer; ADestination: TdxDocObjectCollection; APositions: TdxDocHeadersFootersPositions);
    procedure FinishState; override;
  end;

  { TdxMacroIteratorState }

  TdxMacroIteratorState = class(TdxDocIteratorState)
  protected
    function GetCurrentState: TdxDocContentState; override;
    function GetNextState: TdxDocContentState; override;
  end;

  { TdxCompositeObjectIteratorState }

  TdxCompositeObjectIteratorState = class abstract(TdxDocIteratorState)
  strict private
    FCurrentCharacterPosition: Integer;
    FShouldNormalizeFields: Boolean;
    FPositions: TdxIntegerList;
    FReferences: TdxIntegerList;
    FCompositeObjects: TdxDocObjectCollectionDictionary;
  protected
    FCurrentItemIndex: Integer;
    function GetPositions(AReader: TBinaryReader; AFib: TdxFileInformationBlock): TdxIntegerList; virtual; abstract;
    function GetPositionsCore(AReader: TBinaryReader; AOffset: Integer; ASize: Integer): TdxIntegerList;
    procedure AdvanceNext(AOffset: Integer); override;
    procedure UpdateDestination; override;
    procedure UpdateDestinationCore; virtual;
    function ShouldChangeItem: Boolean;
    procedure NormalizeFields; override;

    property CurrentCharacterPosition: Integer read FCurrentCharacterPosition;
    property CurrentItemIndex: Integer read FCurrentItemIndex write FCurrentItemIndex;
    property Positions: TdxIntegerList read FPositions;
    property References: TdxIntegerList read FReferences;
  public
    constructor Create(AThreshold: Integer; ACompositeObjects: TdxDocObjectCollectionDictionary;
      AReferences: TdxIntegerList; AReader: TBinaryReader; AFib: TdxFileInformationBlock);
    destructor Destroy; override;

    property CompositeObjects: TdxDocObjectCollectionDictionary read FCompositeObjects;
  end;

  { TdxFootNotesIteratorState }

  TdxFootNotesIteratorState = class(TdxCompositeObjectIteratorState)
  strict private
    FHasHeaders: Boolean;
  protected
    function GetCurrentState: TdxDocContentState; override;
    function GetNextState: TdxDocContentState; override;
    function GetPositions(AReader: TBinaryReader; AFib: TdxFileInformationBlock): TdxIntegerList; override;

    property HasHeaders: Boolean read FHasHeaders write FHasHeaders;
  public
    procedure FinishState; override;
  end;

  { TdxCommentsIteratorState }

  TdxCommentsIteratorState = class(TdxCompositeObjectIteratorState)
  protected
    function GetCurrentState: TdxDocContentState; override;
    function GetNextState: TdxDocContentState; override;
    function GetPositions(AReader: TBinaryReader; AFib: TdxFileInformationBlock): TdxIntegerList; override;
  public
    procedure FinishState; override;
  end;

  { TdxEndnotesIteratorState }

  TdxEndnotesIteratorState = class(TdxCompositeObjectIteratorState)
  protected
    function GetCurrentState: TdxDocContentState; override;
    function GetNextState: TdxDocContentState; override;
    function GetPositions(AReader: TBinaryReader; AFib: TdxFileInformationBlock): TdxIntegerList; override;
  public
    procedure FinishState; override;
  end;

  { TdxTextBoxesIteratorStateBase }

  TdxTextBoxesIteratorStateBase = class abstract(TdxCompositeObjectIteratorState)
  protected
    procedure UpdateDestinationCore; override;
  end;

  { TdxTextBoxesIteratorState }

  TdxTextBoxesIteratorState = class(TdxTextBoxesIteratorStateBase)
  protected
    function GetCurrentState: TdxDocContentState; override;
    function GetNextState: TdxDocContentState; override;
    function GetPositions(AReader: TBinaryReader; AFib: TdxFileInformationBlock): TdxIntegerList; override;
  end;

  { TdxHeaderTextBoxesIteratorState }

  TdxHeaderTextBoxesIteratorState = class(TdxTextBoxesIteratorStateBase)
  protected
    function GetCurrentState: TdxDocContentState; override;
    function GetNextState: TdxDocContentState; override;
    function GetPositions(AReader: TBinaryReader; AFib: TdxFileInformationBlock): TdxIntegerList; override;
  end;

  { TdxFinalIteratorState }

  TdxFinalIteratorState = class(TdxDocIteratorState)
  protected
    function GetCurrentState: TdxDocContentState; override;
    function GetNextState: TdxDocContentState; override;
    function GetReadingComplete: Boolean; override;
  public
    function ShouldChangeState(ACharacterPosition: Integer): Boolean; override;
  end;

  { TdxDocFloatingObjectsIterator }

  TdxDocFloatingObjectsIterator = class
  public const
    UnusedTextBoxInfoStart = 14;
    UnusedTextBoxInfoEnd   = 4;
    TextBoxInfoSize        = 22;
  strict private
    FState: TdxDocContentState;
    FFormatting: TdxFloatingObjectFormatting;
    FArtContent: TdxOfficeArtContent;
    FMainShapeAddresses: TdxFileShapeAddressTable;
    FHeaderShapeAddresses: TdxFileShapeAddressTable;
    FHeaderDocumentTextBoxReferences: TdxIntegerList;
    FMainDocumentTextBoxReferences: TdxIntegerList;
    FHeaderDocumentTextBoxes: TdxDocObjectCollectionDictionary;
    FMainDocumentTextBoxes: TdxDocObjectCollectionDictionary;
    FMainDocumentBreakDescriptors: TdxBreakDescriptorTable;
    FHeaderDocumentBreakDescriptors: TdxBreakDescriptorTable;
    function GetShapeAddresses: TdxFileShapeAddressTable;
    function GetBlipsWithProperties: TdxBlipsWithProperties;
    function GetTextBoxReferences: TdxIntegerList;
  protected
    procedure ReadFileShapeAddresses(AReader: TBinaryReader; AFib: TdxFileInformationBlock);
    procedure ReadTextBoxReferences(AReader: TBinaryReader; AFib: TdxFileInformationBlock);
    procedure ReadTextBoxBreakDescriptors(AReader: TBinaryReader; AFib: TdxFileInformationBlock);
    function ReadTextBoxReferencesCore(AReader: TBinaryReader; AOffset: Integer; ASize: Integer): TdxIntegerList;

    property Formatting: TdxFloatingObjectFormatting read FFormatting;
    property State: TdxDocContentState read FState write FState;
    property ShapeAddresses: TdxFileShapeAddressTable read GetShapeAddresses;
    property BlipsWithProperties: TdxBlipsWithProperties read GetBlipsWithProperties;
    property TextBoxReferences: TdxIntegerList read GetTextBoxReferences;
    property HeaderDocumentTextBoxReferences: TdxIntegerList read FHeaderDocumentTextBoxReferences;
    property MainDocumentTextBoxReferences: TdxIntegerList read FMainDocumentTextBoxReferences;
    property HeaderDocumentTextBoxes: TdxDocObjectCollectionDictionary read FHeaderDocumentTextBoxes;
    property MainDocumentTextBoxes: TdxDocObjectCollectionDictionary read FMainDocumentTextBoxes;
    property HeaderDocumentTextBoxesBreakTable: TdxBreakDescriptorTable read FHeaderDocumentBreakDescriptors;
  public
    constructor Create(AReader: TBinaryReader; AFib: TdxFileInformationBlock; AArtContent: TdxOfficeArtContent; AFormatting: TdxFloatingObjectFormatting);
    destructor Destroy; override;
    function GetFloatingObject(ACharacterPosition: Integer; var AObjectInfo: TdxDocObjectInfo; APropertyContainer: TdxDocPropertyContainer): TdxDocFloatingObjectBase;
    function GetBlip(APosition: Integer): TdxBlipBase;
    function GetFloatingObjectCore(AAddress: TdxFileShapeAddress; var AObjectInfo: TdxDocObjectInfo; APropertyContainer: TdxDocPropertyContainer): TdxDocFloatingObjectBase;
    function GetMainTextBoxObjects(AShapeId: Integer): TdxDocObjectCollection;
    function GetHeaderTextBoxObjects(AShapeId: Integer): TdxDocObjectCollection;

    property ArtContent: TdxOfficeArtContent read FArtContent;
    property MainTextBoxesBreakTable: TdxBreakDescriptorTable read FMainDocumentBreakDescriptors;
  end;

  { TdxDocIteratorStateFactory }

  TdxCreateStateDelegate = function(): TdxDocIteratorState of object;

  TdxDocIteratorStateFactory = class
  strict private
    FCreators: TDictionary<TdxDocContentState, TdxCreateStateDelegate>;
    FFib: TdxFileInformationBlock;
    FMainStreamReader: TBinaryReader;
    FTableStreamReader: TBinaryReader;
    FContentIterator: TdxDocContentIterator;
    FHeadersFootersPositions: TdxDocHeadersFootersPositions;
  protected
    procedure InitializeStateCreators;

    property FileInfo: TdxFileInformationBlock read FFib;
    property MainStreamReader: TBinaryReader read FMainStreamReader;
    property TableStreamReader: TBinaryReader read FTableStreamReader;
    property ContentIterator: TdxDocContentIterator read FContentIterator;
    property HeadersFootersPositions: TdxDocHeadersFootersPositions read FHeadersFootersPositions;
  public
    constructor Create(AFib: TdxFileInformationBlock; AMainStreamReader: TBinaryReader; ATableStreamReader: TBinaryReader; AIterator: TdxDocContentIterator);
    destructor Destroy; override;
    function CreateState(AStateType: TdxDocContentState): TdxDocIteratorState;
    function CreateMainDocumentState: TdxDocIteratorState;
    function CreateFootnotesState: TdxDocIteratorState;
    function CreateHeadersFootersStoryState: TdxDocIteratorState;
    function CreateHeadersFootersEndState: TdxDocIteratorState;
    function CreateMacroState: TdxDocIteratorState;
    function CreateCommentsState: TdxDocIteratorState;
    function CreateEndnotesState: TdxDocIteratorState;
    function CreateTextBoxesState: TdxDocIteratorState;
    function CreateHeaderTextBoxesState: TdxDocIteratorState;
    function CreateFinalState: TdxDocIteratorState;
    function CreateFieldIterator(AStateType: TdxDocContentState): TdxDocFieldsIterator;
  end;

  { TdxDocNotesIterator }

  TdxDocNotesIterator = class
  public const
    FlagSize = Integer(2);
  strict private
    FFootNotes: TdxDocObjectCollectionDictionary;
    FEndNotes: TdxDocObjectCollectionDictionary;
    FFootNotesReferences: TdxIntegerList;
    FEndNoteReferences: TdxIntegerList;
  protected
    procedure GetReferences(AReader: TBinaryReader; AFib: TdxFileInformationBlock);
    function GetNoteReferencesCore(AReader: TBinaryReader; AOffset: Integer; ASize: Integer): TdxIntegerList;

    property FootNoteReferences: TdxIntegerList read FFootNotesReferences;
    property EndNoteReferences: TdxIntegerList read FEndNoteReferences;
  public
    constructor Create(AFib: TdxFileInformationBlock; AReader: TBinaryReader);
    destructor Destroy; override;

    function GetNoteObject(AInfo: TdxDocObjectInfo; APropertyContainer: TdxDocPropertyContainer): IdxDocObject;
    function GetFootNoteObjects(ACharacterPosition: Integer): TdxDocObjectCollection;
    function GetEndNoteObjects(ACharacterPosition: Integer): TdxDocObjectCollection;

    property FootNotes: TdxDocObjectCollectionDictionary read FFootNotes;
    property EndNotes: TdxDocObjectCollectionDictionary read FEndNotes;
  end;

  { TdxDocContentIterator }

  TdxDocContentIterator = class
  strict private
    FBuilder: TObject{TdxDocContentBuilder};
    FArtContent: TdxOfficeArtContent;
    FCurrentCharacterPosition: Integer;
    FMainDocumentLength: Integer;
    FFootNotesTextLength: Integer;
    FHeadersFootersTextLength: Integer;
    FCommentsTextLength: Integer;
    FEndNotesTextLength: Integer;
    FTextBoxesTextLength: Integer;
    FHeaderTextBoxesTextLength: Integer;
    FBookmarkIterator: TdxDocBookmarkIterator;
    FCommentsIterator: TdxDocCommentsIterator;
    FNotesIterator: TdxDocNotesIterator;
    FFloatingObjectsIterator: TdxDocFloatingObjectsIterator;
    FPermissionsIterator: TdxDocRangeEditPermissionIterator;
    FState: TdxDocIteratorState;
    FFactory: TdxDocIteratorStateFactory;
    FMainTextDocObjects: TdxDocObjectCollection;
    FMacroObjects: TdxDocObjectCollection;
    FDocumentHeadersFooters: TdxDocHeadersFooters;
    function GetHeadersDocumentOrigin: Integer;
    function GetCommentsDocumentOrigin: Integer;
    function GetEndNotesDocumentOrigin: Integer;
    function GetTextBoxesDocumentOrigin: Integer;
    function GetHeaderTextBoxesDocumentOrigin: Integer;
    function GetDestination: TdxDocObjectCollection;
  protected
    procedure InitializePageColor(AModel: TdxDocumentModel); virtual;
    procedure SwitchToNextState;
    procedure ChangeState;
    function GetRelativeCharacterPosition(AInfo: TdxDocObjectInfo): Integer;

    property CurrentCharacterPosition: Integer read FCurrentCharacterPosition write FCurrentCharacterPosition;
    property MainDocumentLength: Integer read FMainDocumentLength write FMainDocumentLength;
    property FootNotesTextLength: Integer read FFootNotesTextLength write FFootNotesTextLength;
    property HeadersFootersTextLength: Integer read FHeadersFootersTextLength write FHeadersFootersTextLength;
    property CommentsTextLength: Integer read FCommentsTextLength write FCommentsTextLength;
    property EndNotesTextLength: Integer read FEndNotesTextLength write FEndNotesTextLength;
    property TextBoxesTextLength: Integer read FTextBoxesTextLength write FTextBoxesTextLength;
    property HeaderTextBoxesTextLength: Integer read FHeaderTextBoxesTextLength write FHeaderTextBoxesTextLength;
    property HeadersDocumentOrigin: Integer read GetHeadersDocumentOrigin;
    property CommentsDocumentOrigin: Integer read GetCommentsDocumentOrigin;
    property EndNotesDocumentOrigin: Integer read GetEndNotesDocumentOrigin;
    property TextBoxesDocumentOrigin: Integer read GetTextBoxesDocumentOrigin;
    property HeaderTextBoxesDocumentOrigin: Integer read GetHeaderTextBoxesDocumentOrigin;
  public
    constructor Create(ABuilder: TObject{TdxDocContentBuilder}; AFib: TdxFileInformationBlock; AMainStreamReader: TBinaryReader; ATableStreamReader: TBinaryReader; AModel: TdxDocumentModel);
    destructor Destroy; override;
    procedure AddToGC(AObject: TObject);
    procedure SetTextStreamBorders(AFib: TdxFileInformationBlock);
    procedure InitializeIterators(AFib: TdxFileInformationBlock; AMainStreamReader: TBinaryReader; ATableStreamReader: TBinaryReader; AModel: TdxDocumentModel);
    function ShouldProcessTextRun: Boolean;
    function AdvanceNext(AOffset: Integer): Integer;
    procedure UpdateState;
    function AdvanceField(APropertyContainer: TdxDocPropertyContainer): Boolean;
    function ShapeField: Boolean;
    procedure CheckFieldCompatibility(ADestination: TdxDocObjectCollection);
    function GetFloatingObject(var AObjectInfo: TdxDocObjectInfo; APropertyContainer: TdxDocPropertyContainer): TdxDocFloatingObjectBase;
    function GetNoteObject(var AObjectInfo: TdxDocObjectInfo; APropertyContainer: TdxDocPropertyContainer): IdxDocObject;
    function GetCurrentFieldType: TdxFieldType;
    procedure AdvancePosition(ALogPosition: TdxDocumentLogPosition; AOriginalPosition: Integer; ALength: Integer);
    procedure InsertBookmarks(APieceTable: TdxPieceTable);
    procedure BeginEmbeddedContent(AContent: TdxDocObjectCollection);
    procedure EndEmbeddedContent;

    property MainTextDocObjects: TdxDocObjectCollection read FMainTextDocObjects;
    property MacroObjects: TdxDocObjectCollection read FMacroObjects;
    property HeadersFooters: TdxDocHeadersFooters read FDocumentHeadersFooters;
    property Destination: TdxDocObjectCollection read GetDestination;
    property BookmarkIterator: TdxDocBookmarkIterator read FBookmarkIterator;
    property CommentsIterator: TdxDocCommentsIterator read FCommentsIterator;
    property FloatingObjectsIterator: TdxDocFloatingObjectsIterator read FFloatingObjectsIterator;
    property NotesIterator: TdxDocNotesIterator read FNotesIterator;
    property PermissionsIterator: TdxDocRangeEditPermissionIterator read FPermissionsIterator;
    property State: TdxDocIteratorState read FState;
  end;

implementation

uses
  Contnrs,
  dxRichEdit.Options,
  dxRichEdit.Import.Doc.DocContentBuilder,
  dxStringHelper;

{ TdxDocFieldTable }

constructor TdxDocFieldTable.Create;
begin
  FCharacterPositions := TdxIntegerList.Create;
  FFieldDescriptors := TList<IdxDocFieldDescriptor>.Create;
end;

destructor TdxDocFieldTable.Destroy;
begin
  FCharacterPositions.Free;
  FFieldDescriptors.Free;
  inherited Destroy;
end;

class function TdxDocFieldTable.FromStream(AReader: TBinaryReader; AOffset: Integer; ASize: Integer): TdxDocFieldTable;
begin
  Result := TdxDocFieldTable.Create;
  Result.Read(AReader, AOffset, ASize);
end;

procedure TdxDocFieldTable.Read(AReader: TBinaryReader; AOffset: Integer; ASize: Integer);
var
  ACount, I: Integer;
begin
  AReader.BaseStream.Seek(AOffset, TSeekOrigin.soBeginning);
  ACount := (ASize - TdxDocConstants.CharacterPositionSize) div (TdxDocConstants.CharacterPositionSize + FieldDescriptorSize);
  for I := 0 to ACount + 1 - 1 do
    FCharacterPositions.Add(AReader.ReadInt32);
  for I := 0 to ACount - 1 do
    FFieldDescriptors.Add(TdxDocFieldDescriptorFactory.CreateInstance(AReader));
end;

procedure TdxDocFieldTable.Write(AWriter: TBinaryWriter);
var
  ACount, I: Integer;
begin
  ACount := FCharacterPositions.Count;
  if ACount = 0 then
    Exit;
  for I := 0 to ACount - 1 do
    AWriter.Write(FCharacterPositions[I]);
  for I := 0 to ACount - 1 - 1 do
    FFieldDescriptors[I].Write(AWriter);
end;

function TdxDocFieldTable.GetFieldDescriptorByIndex(AIndex: Integer): IdxDocFieldDescriptor;
begin
  if FFieldDescriptors.Count > AIndex then
    Result := FFieldDescriptors[AIndex]
  else
    Result := nil;
end;

procedure TdxDocFieldTable.AddEntry(ACharacterPosition: Integer; const AFieldDescriptor: IdxDocFieldDescriptor);
begin
  FCharacterPositions.Add(ACharacterPosition);
  FFieldDescriptors.Add(AFieldDescriptor);
end;

procedure TdxDocFieldTable.Finish(ALastCharacterPosition: Integer);
begin
  FCharacterPositions.Add(ALastCharacterPosition);
end;

{ TdxDocFieldsIterator }

constructor TdxDocFieldsIterator.Create(AReader: TBinaryReader; AOffset: Integer; ASize: Integer);
begin
  FShapeFieldName := 'SHAPE';
  FFieldTable := TdxDocFieldTable.FromStream(AReader, AOffset, ASize);
  FFieldBeginDescriptors := TStack<TdxDocFieldBeginDescriptor>.Create;
end;

destructor TdxDocFieldsIterator.Destroy;
begin
  FFieldTable.Free;
  FFieldBeginDescriptors.Free;
  inherited Destroy;
end;

class constructor TdxDocFieldsIterator.Initialize;
begin
  FUnsupportedFieldCodes := TdxStringList.Create;
  FUnsupportedFieldCodes.Add(' SHAPE  \* MERGEFORMAT ');
  FUnsupportedFieldCodes.Add(' FORMCHECKBOX ');
end;

class destructor TdxDocFieldsIterator.Finalize;
begin
  FUnsupportedFieldCodes.Free;
end;

function TdxDocFieldsIterator.MoveNext(APropertyContainer: TdxDocPropertyContainer): Boolean;
var
  AFieldDescriptor: IdxDocFieldDescriptor;
  AFieldBeginDescriptor: TdxDocFieldBeginDescriptor;
  AFieldEndDescriptor: TdxDocFieldEndDescriptor;
begin
  AFieldDescriptor := FFieldTable.GetFieldDescriptorByIndex(FCurrentFieldIndex);
  Inc(FCurrentFieldIndex);
  AFieldBeginDescriptor := TdxDocFieldBeginDescriptor(AFieldDescriptor);
  if AFieldBeginDescriptor <> nil then
    FFieldBeginDescriptors.Push(AFieldBeginDescriptor);
  AFieldEndDescriptor := TdxDocFieldEndDescriptor(AFieldDescriptor);
  if AFieldEndDescriptor <> nil then
    if FFieldBeginDescriptors.Count = 0 then
      Exit(False)
    else
    begin
      if APropertyContainer <> nil then
        APropertyContainer.FieldProperties := AFieldEndDescriptor.Properties;
      FFieldBeginDescriptors.Pop;
    end;
  Result := True;
end;

function TdxDocFieldsIterator.GetCurrentFieldType: TdxFieldType;
var
  ACurrentFieldBeginDescriptor: TdxDocFieldBeginDescriptor;
begin
  if FFieldBeginDescriptors.Count = 0 then
    Exit(TdxFieldType.None);
  ACurrentFieldBeginDescriptor := FFieldBeginDescriptors.Peek;
  Result := ACurrentFieldBeginDescriptor.CalcFieldType;
end;

procedure TdxDocFieldsIterator.CheckFieldCompatibility(ADestination: TdxDocObjectCollection);
var
  AFieldObjects: TStack<IdxDocObject>;
  AFieldLength, ARemoveFrom, AFieldStartPosition, AFieldEndPosition, ACount, I: Integer;
  AFieldCode: string;
  ALastDocObject, ADocObject: IdxDocObject;
  AUnsupportedObject: TdxUnsupportedObject;
begin
  AFieldObjects := GetFieldObjects(ADestination);
  try
    AFieldLength := AFieldObjects.Count;
    AFieldCode := GetFieldCode(AFieldObjects);
    if FUnsupportedFieldCodes.Contains(AFieldCode) then
    begin
      ARemoveFrom := ADestination.Count - AFieldLength;
      AFieldStartPosition := ADestination[ADestination.Count - AFieldLength].Position;
      ALastDocObject := ADestination[ADestination.Count - 1];
      AFieldEndPosition := ALastDocObject.Position + ALastDocObject.Length;
      ADestination.DeleteRange(ARemoveFrom, AFieldLength);
      ACount := AFieldObjects.Count;
      for I := 0 to ACount - 1 - 1 do
      begin
        ADocObject := AFieldObjects.Pop;
        ADestination.Add(ADocObject);
        if ADocObject.DocObjectType = TdxDocObjectType.UnsupportedObject then
        begin
          AUnsupportedObject := TdxUnsupportedObject(ADocObject);
          if AUnsupportedObject <> nil then
          begin
            AUnsupportedObject.Position := AFieldStartPosition;
            AUnsupportedObject.Length := AFieldEndPosition - AFieldStartPosition;
          end;
        end;
      end;
    end;
  finally
    AFieldObjects.Free;
  end;
end;

procedure TdxDocFieldsIterator.Reset;
begin
  FCurrentFieldIndex := 0;
  FFieldBeginDescriptors.Clear;
end;

function TdxDocFieldsIterator.GetFieldObjects(ADocObjects: TdxDocObjectCollection): TStack<IdxDocObject>;
var
  ACount, I: Integer;
  AObject: TObject;
  ADocObject: IdxDocObject;
begin
  Assert(ADocObjects.Count > 0);
  Result := TStack<IdxDocObject>.Create;
  ACount := ADocObjects.Count - 1;
  for I := ACount downto 0 do
  begin
    ADocObject := ADocObjects[I];
    AObject := ADocObject.GetObject;
    Result.Push(ADocObject);
    if AObject.ClassType = TdxDocFieldBegin then
      Break;
  end;
end;

function TdxDocFieldsIterator.GetFieldCode(AFieldObjects: TStack<IdxDocObject>): string;
var
  AFieldCode: TStringBuilder;
  ACount, I: Integer;
  ADocObject: IdxDocObject;
  ATextRun: TdxDocTextRun;
  AObject: TObject;
begin
  Assert(AFieldObjects.Count > 0);
  AFieldObjects.Pop;
  AFieldCode := TStringBuilder.Create;
  try
    ACount := AFieldObjects.Count;
    for I := 0 to ACount - 1 do
    begin
      ADocObject := AFieldObjects.Pop;
      AObject := ADocObject.GetObject;
      ATextRun := Safe<TdxDocTextRun>.Cast(AObject);
      if ATextRun <> nil then
        AFieldCode.Append(ATextRun.Text);
      if AObject.ClassType = TdxDocFieldSeparator then
        Break;
    end;
    Result := AFieldCode.ToString;
  finally
    AFieldCode.Free;
  end;
end;

procedure TdxDocFieldsIterator.SetShapeField(ADestination: TdxDocObjectCollection);
var
  AFieldObjects: TStack<IdxDocObject>;
  AFieldCode: string;
begin
  AFieldObjects := GetFieldObjects(ADestination);
  try
    AFieldCode := GetFieldCode(AFieldObjects);
    FShapeField := (AFieldCode <> '') and TdxStringHelper.StartsWith(Trim(AFieldCode), FShapeFieldName);
  finally
    AFieldObjects.Free;
  end;
end;

procedure TdxDocFieldsIterator.ResetShapeField;
begin
  if FShapeField then
    FShapeField := False;
end;

{ TdxDocIteratorState }

constructor TdxDocIteratorState.Create(AThreshold: Integer);
begin
  inherited Create;
  FThreshold := AThreshold;
  CreateNewDestination;
end;

constructor TdxDocIteratorState.Create(AThreshold: Integer; ADestination: TdxDocObjectCollection);
begin
  inherited Create;
  FThreshold := AThreshold;
  FDestination := ADestination;
end;

destructor TdxDocIteratorState.Destroy;
begin
  FFieldIterator.Free;
  if FOwnsDestination then
    FDestination.Free;
  inherited Destroy;
end;

procedure TdxDocIteratorState.CreateNewDestination;
begin
  if FOwnsDestination then
    FDestination.Free;
  FDestination := TdxDocObjectCollection.Create;
  FOwnsDestination := True;
end;

function TdxDocIteratorState.GetReadingComplete: Boolean;
begin
  Result := False;
end;

procedure TdxDocIteratorState.SetDestination(const AValue: TdxDocObjectCollection);
begin
  if FOwnsDestination then
    FDestination.Free;
  FDestination := AValue;
  FOwnsDestination := False;
end;

procedure TdxDocIteratorState.SetFieldIterator(const AValue: TdxDocFieldsIterator);
begin
  Assert(FFieldIterator = nil);
  FFieldIterator := AValue;
end;

function TdxDocIteratorState.ShouldChangeState(ACharacterPosition: Integer): Boolean;
begin
  Result := ACharacterPosition = Threshold;
end;

procedure TdxDocIteratorState.FinishState;
begin
  NormalizeFields;
end;

procedure TdxDocIteratorState.AdvanceNext(AOffset: Integer);
begin
end;

procedure TdxDocIteratorState.UpdateDestination;
begin
end;

procedure TdxDocIteratorState.FixLastParagraph;
begin
  if Destination.Count <> 0 then
    Destination.Delete(Destination.Count - 1);
end;

procedure TdxDocIteratorState.NormalizeFields;
begin
  if ShouldNormalizeFields(Destination) then
  begin
    FieldIterator.Reset;
    NormalizeFieldsCore(Destination);
  end;
end;

function TdxDocIteratorState.ShouldNormalizeFields(ADocObjects: TdxDocObjectCollection): Boolean;
var
  AFieldBalance, ACount, I: Integer;
begin
  if FieldIterator = nil then
    Exit(False);
  AFieldBalance := 0;
  ACount := Destination.Count;
  for I := 0 to ACount - 1 do
  begin
    if ADocObjects[I].DocObjectType = TdxDocObjectType.FieldBegin then
      Inc(AFieldBalance);
    if ADocObjects[I].DocObjectType = TdxDocObjectType.FieldEnd then
      Dec(AFieldBalance);
    if AFieldBalance < 0 then
      Exit(True);
  end;
  Result := AFieldBalance <> 0;
end;

procedure TdxDocIteratorState.NormalizeFieldsCore(ADocObjects: TdxDocObjectCollection);
var
  ACount, I: Integer;
  ADocObject: IdxDocObject;
  AType: TdxDocObjectType;
begin
  ACount := ADocObjects.Count;
  for I := 0 to ACount - 1 do
  begin
    ADocObject := ADocObjects[I];
    AType := ADocObject.DocObjectType;
    if AType in [TdxDocObjectType.FieldBegin, TdxDocObjectType.FieldSeparator, TdxDocObjectType.FieldEnd] then
      FieldIterator.MoveNext(nil);
    if AType in [TdxDocObjectType.ExpectedFieldBegin, TdxDocObjectType.ExpectedFieldSeparator, TdxDocObjectType.ExpectedFieldEnd] then
      if FieldIterator.MoveNext(nil) then
        TdxExpectedDocObject(ADocObject).SetActualType;
  end;
end;

{ TdxMainDocumentIteratorState }

function TdxMainDocumentIteratorState.GetCurrentState: TdxDocContentState;
begin
  Result := TdxDocContentState.MainDocument;
end;

function TdxMainDocumentIteratorState.GetNextState: TdxDocContentState;
begin
  Result := TdxDocContentState.Footnotes;
end;

{ TdxHeaderFooterEndIteratorState }

function TdxHeaderFooterEndIteratorState.GetCurrentState: TdxDocContentState;
begin
  Result := TdxDocContentState.HeadersFootersEnd;
end;

function TdxHeaderFooterEndIteratorState.GetNextState: TdxDocContentState;
begin
  Result := TdxDocContentState.Macro;
end;

{ TdxHeaderFooterIteratorStoryState }

constructor TdxHeaderFooterIteratorStoryState.Create(AThreshold: Integer; ADestination: TdxDocObjectCollection; APositions: TdxDocHeadersFootersPositions);
begin
  inherited Create(AThreshold, ADestination);
  FPositions := APositions;
end;

function TdxHeaderFooterIteratorStoryState.GetCurrentState: TdxDocContentState;
begin
  Result := TdxDocContentState.HeadersFootersStory;
end;

function TdxHeaderFooterIteratorStoryState.GetNextState: TdxDocContentState;
begin
  if not FPositions.IsLastHeaderFooter then
    Result := TdxDocContentState.HeadersFootersStory
  else
    Result := TdxDocContentState.HeadersFootersEnd;
end;

procedure TdxHeaderFooterIteratorStoryState.FinishState;
begin
  inherited FinishState;
  FPositions.AdvanceNext;
  FixLastParagraph;
end;

{ TdxMacroIteratorState }

function TdxMacroIteratorState.GetCurrentState: TdxDocContentState;
begin
  Result := TdxDocContentState.Macro;
end;

function TdxMacroIteratorState.GetNextState: TdxDocContentState;
begin
  Result := TdxDocContentState.Comments;
end;

{ TdxCompositeObjectIteratorState }

constructor TdxCompositeObjectIteratorState.Create(AThreshold: Integer;
  ACompositeObjects: TdxDocObjectCollectionDictionary;
  AReferences: TdxIntegerList; AReader: TBinaryReader; AFib: TdxFileInformationBlock);
begin
  inherited Create(AThreshold);
  FCompositeObjects := ACompositeObjects;
  FReferences := AReferences;
  FPositions := GetPositions(AReader, AFib);
end;

destructor TdxCompositeObjectIteratorState.Destroy;
begin
  FPositions.Free;
  inherited Destroy;
end;

function TdxCompositeObjectIteratorState.GetPositionsCore(AReader: TBinaryReader; AOffset: Integer; ASize: Integer): TdxIntegerList;
var
  ACount, I: Integer;
begin
  Result := TdxIntegerList.Create;
  ACount := ASize div TdxDocConstants.CharacterPositionSize;
  AReader.BaseStream.Seek(AOffset, TSeekOrigin.soBeginning);
  for I := 0 to ACount - 1 do
    Result.Add(AReader.ReadInt32);
end;

procedure TdxCompositeObjectIteratorState.AdvanceNext(AOffset: Integer);
begin
  FCurrentCharacterPosition := FCurrentCharacterPosition + AOffset;
end;

procedure TdxCompositeObjectIteratorState.UpdateDestination;
begin
  if not ShouldChangeItem then
    Exit;
  FShouldNormalizeFields := FShouldNormalizeFields or ShouldNormalizeFields(Destination);
  UpdateDestinationCore;
end;

procedure TdxCompositeObjectIteratorState.UpdateDestinationCore;
begin
  CompositeObjects.Add(References[CurrentItemIndex], Destination);
  OwnsDestination := False;
  CreateNewDestination;
  Inc(FCurrentItemIndex);
end;

function TdxCompositeObjectIteratorState.ShouldChangeItem: Boolean;
var
  ANextItemIndex: Integer;
begin
  ANextItemIndex := CurrentItemIndex + 1;
  if (ANextItemIndex >= Positions.Count) or (CurrentItemIndex >= References.Count) then
    Exit(False);
  Result := CurrentCharacterPosition = Positions[ANextItemIndex];
end;

procedure TdxCompositeObjectIteratorState.NormalizeFields;
var
  I: Integer;
  ADestination: TdxDocObjectCollection;
begin
  if FShouldNormalizeFields then
  begin
    FieldIterator.Reset;
    for I := 0 to References.Count - 1 do
    begin
      if CompositeObjects.TryGetValue(References[I], ADestination) then
        NormalizeFieldsCore(ADestination);
    end;
  end;
end;

{ TdxFootNotesIteratorState }

function TdxFootNotesIteratorState.GetCurrentState: TdxDocContentState;
begin
  Result := TdxDocContentState.Footnotes;
end;

function TdxFootNotesIteratorState.GetNextState: TdxDocContentState;
begin
  if FHasHeaders then
    Result := TdxDocContentState.HeadersFootersStory
  else
    Result := TdxDocContentState.Macro;
end;

function TdxFootNotesIteratorState.GetPositions(AReader: TBinaryReader; AFib: TdxFileInformationBlock): TdxIntegerList;
begin
  Result := GetPositionsCore(AReader, AFib.FootNotesTextOffset, AFib.FootNotesTextSize);
end;

procedure TdxFootNotesIteratorState.FinishState;
begin
  inherited FinishState;
  FixLastParagraph;
end;

{ TdxCommentsIteratorState }

function TdxCommentsIteratorState.GetCurrentState: TdxDocContentState;
begin
  Result := TdxDocContentState.Comments;
end;

function TdxCommentsIteratorState.GetNextState: TdxDocContentState;
begin
  Result := TdxDocContentState.Endnotes;
end;

procedure TdxCommentsIteratorState.FinishState;
begin
  inherited FinishState;
  FixLastParagraph;
end;

function TdxCommentsIteratorState.GetPositions(AReader: TBinaryReader; AFib: TdxFileInformationBlock): TdxIntegerList;
begin
  Result := GetPositionsCore(AReader, AFib.CommentsTextOffset, AFib.CommentsTextSize);
end;

{ TdxEndnotesIteratorState }

function TdxEndnotesIteratorState.GetCurrentState: TdxDocContentState;
begin
  Result := TdxDocContentState.Endnotes;
end;

function TdxEndnotesIteratorState.GetNextState: TdxDocContentState;
begin
  Result := TdxDocContentState.TextBoxes;
end;

procedure TdxEndnotesIteratorState.FinishState;
begin
  inherited FinishState;
  FixLastParagraph;
end;

function TdxEndnotesIteratorState.GetPositions(AReader: TBinaryReader; AFib: TdxFileInformationBlock): TdxIntegerList;
begin
  Result := GetPositionsCore(AReader, AFib.EndnotesTextOffset, AFib.EndnotesTextSize);
end;

{ TdxTextBoxesIteratorStateBase }

procedure TdxTextBoxesIteratorStateBase.UpdateDestinationCore;
var
  ACurrentReference: Integer;
begin
  ACurrentReference := References[CurrentItemIndex];

  if Destination.Count > 0 then
    Destination.Delete(Destination.Count - 1);
  if ACurrentReference > 0 then
  begin
    CompositeObjects.Add(ACurrentReference, Destination);
    OwnsDestination := False;
  end;
  Destination := TdxDocObjectCollection.Create;
  OwnsDestination := True;
  Inc(FCurrentItemIndex);
end;

{ TdxTextBoxesIteratorState }

function TdxTextBoxesIteratorState.GetCurrentState: TdxDocContentState;
begin
  Result := TdxDocContentState.TextBoxes;
end;

function TdxTextBoxesIteratorState.GetNextState: TdxDocContentState;
begin
  Result := TdxDocContentState.HeaderTextBoxes;
end;

function TdxTextBoxesIteratorState.GetPositions(AReader: TBinaryReader; AFib: TdxFileInformationBlock): TdxIntegerList;
var
  ASize, ACount, I: Integer;
begin
  Result := TdxIntegerList.Create;
  AReader.BaseStream.Seek(AFib.MainDocumentTextBoxesTextOffset, TSeekOrigin.soBeginning);
  ASize := AFib.MainDocumentTextBoxesTextSize;
  if ASize = 0 then
    Exit;
  ACount := (ASize - TdxDocConstants.CharacterPositionSize) div (TdxDocConstants.CharacterPositionSize + TdxDocFloatingObjectsIterator.TextBoxInfoSize) + 1;
  for I := 0 to ACount - 1 do
    Result.Add(AReader.ReadInt32);
end;

{ TdxHeaderTextBoxesIteratorState }

function TdxHeaderTextBoxesIteratorState.GetCurrentState: TdxDocContentState;
begin
  Result := TdxDocContentState.HeaderTextBoxes;
end;

function TdxHeaderTextBoxesIteratorState.GetNextState: TdxDocContentState;
begin
  Result := TdxDocContentState.Final;
end;

function TdxHeaderTextBoxesIteratorState.GetPositions(AReader: TBinaryReader; AFib: TdxFileInformationBlock): TdxIntegerList;
var
  ASize, ACount, I: Integer;
begin
  Result := TdxIntegerList.Create;
  AReader.BaseStream.Seek(AFib.HeaderTextBoxesTextOffset, TSeekOrigin.soBeginning);
  ASize := AFib.HeaderTextBoxesTextSize;
  if ASize = 0 then
    Exit;
  ACount := (ASize - TdxDocConstants.CharacterPositionSize) div (TdxDocConstants.CharacterPositionSize + TdxDocFloatingObjectsIterator.TextBoxInfoSize) + 1;
  for I := 0 to ACount - 1 do
    Result.Add(AReader.ReadInt32);
end;

{ TdxFinalIteratorState }

function TdxFinalIteratorState.GetCurrentState: TdxDocContentState;
begin
  Result := TdxDocContentState.Final;
end;

function TdxFinalIteratorState.GetNextState: TdxDocContentState;
begin
  Result := TdxDocContentState.Final;
end;

function TdxFinalIteratorState.GetReadingComplete: Boolean;
begin
  Result := True;
end;

function TdxFinalIteratorState.ShouldChangeState(ACharacterPosition: Integer): Boolean;
begin
  Result := False;
end;

{ TdxDocFloatingObjectsIterator }

constructor TdxDocFloatingObjectsIterator.Create(AReader: TBinaryReader; AFib: TdxFileInformationBlock; AArtContent: TdxOfficeArtContent; AFormatting: TdxFloatingObjectFormatting);
begin
  inherited Create;
  FArtContent := AArtContent;
  FFormatting := AFormatting;
  FHeaderDocumentTextBoxes := TdxDocObjectCollectionDictionary.Create([doOwnsValues]);
  FMainDocumentTextBoxes := TdxDocObjectCollectionDictionary.Create([doOwnsValues]);
  ReadFileShapeAddresses(AReader, AFib);
  ReadTextBoxReferences(AReader, AFib);
  ReadTextBoxBreakDescriptors(AReader, AFib);
end;

destructor TdxDocFloatingObjectsIterator.Destroy;
begin
  FHeaderShapeAddresses.Free;
  FMainShapeAddresses.Free;
  FMainDocumentTextBoxReferences.Free;
  FHeaderDocumentTextBoxReferences.Free;
  FMainDocumentBreakDescriptors.Free;
  FHeaderDocumentBreakDescriptors.Free;

  FHeaderDocumentTextBoxes.Free;
  FMainDocumentTextBoxes.Free;
  FFormatting.Free;
  inherited Destroy;
end;

function TdxDocFloatingObjectsIterator.GetShapeAddresses: TdxFileShapeAddressTable;
begin
  if State = TdxDocContentState.MainDocument then
    Result := FMainShapeAddresses
  else
    Result := FHeaderShapeAddresses;
end;

function TdxDocFloatingObjectsIterator.GetBlipsWithProperties: TdxBlipsWithProperties;
begin
  if FArtContent = nil then
    Exit(nil);
  if State = TdxDocContentState.MainDocument then
    Result := FArtContent.MainDocumentBlips
  else
    Result := FArtContent.HeadersBlips;
end;

function TdxDocFloatingObjectsIterator.GetTextBoxReferences: TdxIntegerList;
begin
  if State = TdxDocContentState.MainDocument then
    Result := FMainDocumentTextBoxReferences
  else
    Result := FHeaderDocumentTextBoxReferences;
end;

procedure TdxDocFloatingObjectsIterator.ReadFileShapeAddresses(AReader: TBinaryReader; AFib: TdxFileInformationBlock);
begin
  FHeaderShapeAddresses := TdxFileShapeAddressTable.FromStream(AReader, AFib.HeadersFootersFileShapeTableOffset, AFib.HeadersFootersFileShapeTableSize);
  FMainShapeAddresses := TdxFileShapeAddressTable.FromStream(AReader, AFib.MainDocumentFileShapeTableOffset, AFib.MainDocumentFileShapeTableSize);
end;

procedure TdxDocFloatingObjectsIterator.ReadTextBoxReferences(AReader: TBinaryReader; AFib: TdxFileInformationBlock);
begin
  FMainDocumentTextBoxReferences := ReadTextBoxReferencesCore(AReader, AFib.MainDocumentTextBoxesTextOffset, AFib.MainDocumentTextBoxesTextSize);
  FHeaderDocumentTextBoxReferences := ReadTextBoxReferencesCore(AReader, AFib.HeaderTextBoxesTextOffset, AFib.HeaderTextBoxesTextSize);
end;

procedure TdxDocFloatingObjectsIterator.ReadTextBoxBreakDescriptors(AReader: TBinaryReader; AFib: TdxFileInformationBlock);
begin
  FMainDocumentBreakDescriptors := TdxBreakDescriptorTable.FromStream(AReader, AFib.MainTextBoxBreakTableOffset, AFib.MainTextBoxBreakTableSize);
  FHeaderDocumentBreakDescriptors := TdxBreakDescriptorTable.FromStream(AReader, AFib.HeadersFootersFieldTableOffset, AFib.HeadersFootersFieldTableSize);
end;

function TdxDocFloatingObjectsIterator.ReadTextBoxReferencesCore(AReader: TBinaryReader; AOffset: Integer; ASize: Integer): TdxIntegerList;
var
  ACount, I: Integer;
begin
  Result := TdxIntegerList.Create;
  if ASize = 0 then
    Exit(Result);
  ACount := (ASize - TdxDocConstants.CharacterPositionSize) div (TdxDocConstants.CharacterPositionSize + TextBoxInfoSize);
  AReader.BaseStream.Seek(AOffset + (ACount + 1) * TdxDocConstants.CharacterPositionSize, TSeekOrigin.soBeginning);
  for I := 0 to ACount - 1 do
  begin
    AReader.BaseStream.Seek(UnusedTextBoxInfoStart, TSeekOrigin.soCurrent);
    Result.Add(AReader.ReadInt32);
    AReader.BaseStream.Seek(UnusedTextBoxInfoEnd, TSeekOrigin.soCurrent);
  end;
end;

function TdxDocFloatingObjectsIterator.GetFloatingObject(ACharacterPosition: Integer; var AObjectInfo: TdxDocObjectInfo; APropertyContainer: TdxDocPropertyContainer): TdxDocFloatingObjectBase;
var
  AAddress: TdxFileShapeAddress;
begin
  if not ShapeAddresses.TranslationTable.TryGetValue(ACharacterPosition, AAddress) then
    Exit(nil);

  Result := GetFloatingObjectCore(AAddress, AObjectInfo, APropertyContainer);
end;

function TdxDocFloatingObjectsIterator.GetBlip(APosition: Integer): TdxBlipBase;
var
  AAddress: TdxFileShapeAddress;
begin
  if not ShapeAddresses.TranslationTable.TryGetValue(APosition, AAddress) then
    Exit(nil);
  if (BlipsWithProperties = nil) or not BlipsWithProperties.Blips.TryGetValue(AAddress.ShapeIdentifier, Result) then
    Exit(nil);
end;

function TdxDocFloatingObjectsIterator.GetFloatingObjectCore(AAddress: TdxFileShapeAddress;
  var AObjectInfo: TdxDocObjectInfo; APropertyContainer: TdxDocPropertyContainer): TdxDocFloatingObjectBase;
var
  AShapeIdentifier: Integer;
  ABlip: TdxBlipBase;
  AIsOlePicture: Boolean;
  AShapeRecord: TdxOfficeArtShapeRecord;
  AProperties: TdxOfficeArtProperties;
  ATertiaryProperties: TdxOfficeArtTertiaryProperties;
begin
  Result := nil;
  AShapeIdentifier := AAddress.ShapeIdentifier;

  if (BlipsWithProperties <> nil) and BlipsWithProperties.Blips.TryGetValue(AShapeIdentifier, ABlip) then
    Result := TdxDocPictureFloatingObject.Create(AObjectInfo, APropertyContainer, ABlip);

  AIsOlePicture := False;
  if BlipsWithProperties.ShapeRecords.TryGetValue(AShapeIdentifier, AShapeRecord) then
    AIsOlePicture := AShapeRecord.IsOleShape and (Result <> nil);

  if TextBoxReferences.Contains(AShapeIdentifier) and not AIsOlePicture then
  begin
    Result.Free;
    Result := TdxDocTextBoxFloatingObject.Create(AObjectInfo, APropertyContainer, AShapeIdentifier);
  end;
  if Result = nil then
    Exit;

  Result.Formatting := TdxFloatingObjectFormatting(Formatting.Clone);
  Result.ApplyFileShapeAddress(AAddress);
  if BlipsWithProperties.ShapeArtProperties.TryGetValue(AShapeIdentifier, AProperties) then
    Result.SetOfficeArtProperties(AProperties);
  if BlipsWithProperties.ShapeArtTertiaryProperties.TryGetValue(AShapeIdentifier, ATertiaryProperties) then
    Result.SetOfficeArtTertiaryProperties(ATertiaryProperties);
end;

function TdxDocFloatingObjectsIterator.GetMainTextBoxObjects(AShapeId: Integer): TdxDocObjectCollection;
begin
  if not MainDocumentTextBoxes.TryGetValue(AShapeId, Result) then
    Result := TdxDocObjectCollection.Create;
end;

function TdxDocFloatingObjectsIterator.GetHeaderTextBoxObjects(AShapeId: Integer): TdxDocObjectCollection;
begin
  if not HeaderDocumentTextBoxes.TryGetValue(AShapeId, Result) then
    Result := TdxDocObjectCollection.Create;
end;

{ TdxDocIteratorStateFactory }

constructor TdxDocIteratorStateFactory.Create(AFib: TdxFileInformationBlock; AMainStreamReader: TBinaryReader;
   ATableStreamReader: TBinaryReader; AIterator: TdxDocContentIterator);
begin
  FFib := AFib;
  FMainStreamReader := AMainStreamReader;
  FTableStreamReader := ATableStreamReader;
  FContentIterator := AIterator;
  FHeadersFootersPositions := TdxDocHeadersFootersPositions.FromStream(ATableStreamReader, AFib.HeadersFootersPositionsOffset, AFib.HeadersFootersPositionsSize);
  InitializeStateCreators;
end;

destructor TdxDocIteratorStateFactory.Destroy;
begin
  FCreators.Free;
  FHeadersFootersPositions.Free;
  inherited Destroy;
end;

procedure TdxDocIteratorStateFactory.InitializeStateCreators;
begin
  FCreators := TDictionary<TdxDocContentState, TdxCreateStateDelegate>.Create;
  FCreators.Add(TdxDocContentState.MainDocument, CreateMainDocumentState);
  FCreators.Add(TdxDocContentState.Footnotes, CreateFootnotesState);
  FCreators.Add(TdxDocContentState.HeadersFootersStory, CreateHeadersFootersStoryState);
  FCreators.Add(TdxDocContentState.HeadersFootersEnd, CreateHeadersFootersEndState);
  FCreators.Add(TdxDocContentState.Macro, CreateMacroState);
  FCreators.Add(TdxDocContentState.Comments, CreateCommentsState);
  FCreators.Add(TdxDocContentState.Endnotes, CreateEndnotesState);
  FCreators.Add(TdxDocContentState.TextBoxes, CreateTextBoxesState);
  FCreators.Add(TdxDocContentState.HeaderTextBoxes, CreateHeaderTextBoxesState);
  FCreators.Add(TdxDocContentState.Final, CreateFinalState);
end;

function TdxDocIteratorStateFactory.CreateState(AStateType: TdxDocContentState): TdxDocIteratorState;
var
  ACreator: TdxCreateStateDelegate;
begin
  if not FCreators.TryGetValue(AStateType, ACreator) then
    ACreator := CreateFinalState;
  Result := ACreator;
end;

function TdxDocIteratorStateFactory.CreateMainDocumentState: TdxDocIteratorState;
var
  AResult: TdxMainDocumentIteratorState;
begin
  AResult := TdxMainDocumentIteratorState.Create(FileInfo.MainDocumentLength, ContentIterator.MainTextDocObjects);
  AResult.FieldIterator := CreateFieldIterator(TdxDocContentState.MainDocument);
  Result := AResult;
end;

function TdxDocIteratorStateFactory.CreateFootnotesState: TdxDocIteratorState;
var
  AThreshold: Integer;
  ANotesIterator: TdxDocNotesIterator;
  AResult: TdxFootNotesIteratorState;
begin
  AThreshold := FileInfo.FootNotesStart + FileInfo.FootNotesLength;
  ANotesIterator := ContentIterator.NotesIterator;
  AResult := TdxFootNotesIteratorState.Create(AThreshold, ANotesIterator.FootNotes, ANotesIterator.FootNoteReferences, TableStreamReader, FileInfo);
  AResult.HasHeaders := not HeadersFootersPositions.IsEmpty;
  AResult.FieldIterator := CreateFieldIterator(TdxDocContentState.Footnotes);
  Result := AResult;
end;

function TdxDocIteratorStateFactory.CreateHeadersFootersStoryState: TdxDocIteratorState;
var
  AThreshold: Integer;
  AHeadersFooters: TdxDocHeadersFooters;
  AResult: TdxHeaderFooterIteratorStoryState;
begin
  AThreshold := FileInfo.HeadersFootersStart + HeadersFootersPositions.GetNextStoryPosition;
  AHeadersFooters := ContentIterator.HeadersFooters;
  AHeadersFooters.HeadersFooters.Add(TdxDocObjectCollection.Create);
  AResult := TdxHeaderFooterIteratorStoryState.Create(AThreshold, AHeadersFooters.ActiveCollection, HeadersFootersPositions);
  AResult.FieldIterator := CreateFieldIterator(TdxDocContentState.HeadersFootersStory);
  Result := AResult;
end;

function TdxDocIteratorStateFactory.CreateHeadersFootersEndState: TdxDocIteratorState;
var
  AThreshold: Integer;
  AResult: TdxHeaderFooterEndIteratorState;
begin
  AThreshold := FileInfo.HeadersFootersStart + FileInfo.HeadersFootersLength;
  AResult := TdxHeaderFooterEndIteratorState.Create(AThreshold, TdxDocObjectCollection.Create);
  AResult.OwnsDestination := True;
  Result := AResult;
end;

function TdxDocIteratorStateFactory.CreateMacroState: TdxDocIteratorState;
var
  AThreshold: Integer;
  AResult: TdxMacroIteratorState;
begin
  AThreshold := FileInfo.MacroStart + FileInfo.MacroLength;
  AResult := TdxMacroIteratorState.Create(AThreshold, ContentIterator.MacroObjects);
  Result := AResult;
end;

function TdxDocIteratorStateFactory.CreateCommentsState: TdxDocIteratorState;
var
  AThreshold: Integer;
  ACommentsIterator: TdxDocCommentsIterator;
begin
  AThreshold := FileInfo.CommentsStart + FileInfo.CommentsLength;
  ACommentsIterator := ContentIterator.CommentsIterator;
  Result := TdxCommentsIteratorState.Create(AThreshold, ACommentsIterator.CommentsContent, ACommentsIterator.CommentsReferences, TableStreamReader, FileInfo);
  Result.FieldIterator := CreateFieldIterator(TdxDocContentState.Comments);
end;

function TdxDocIteratorStateFactory.CreateEndnotesState: TdxDocIteratorState;
var
  AThreshold: Integer;
  ANotesIterator: TdxDocNotesIterator;
  AResult: TdxEndnotesIteratorState;
begin
  AThreshold := FileInfo.EndnotesStart + FileInfo.EndNotesLength;
  ANotesIterator := ContentIterator.NotesIterator;
  AResult := TdxEndnotesIteratorState.Create(AThreshold, ANotesIterator.EndNotes, ANotesIterator.EndNoteReferences, TableStreamReader, FileInfo);
  AResult.FieldIterator := CreateFieldIterator(TdxDocContentState.Endnotes);
  Result := AResult;
end;

function TdxDocIteratorStateFactory.CreateTextBoxesState: TdxDocIteratorState;
var
  AThreshold: Integer;
  ATextBoxes: TdxDocObjectCollectionDictionary;
  AReferences: TdxIntegerList;
  AResult: TdxTextBoxesIteratorState;
begin
  AThreshold := FileInfo.MainDocumentTextBoxesStart + FileInfo.MainDocumentTextBoxesLength;
  ATextBoxes := ContentIterator.FloatingObjectsIterator.MainDocumentTextBoxes;
  AReferences := ContentIterator.FloatingObjectsIterator.MainDocumentTextBoxReferences;
  AResult := TdxTextBoxesIteratorState.Create(AThreshold, ATextBoxes, AReferences, TableStreamReader, FileInfo);
  AResult.FieldIterator := CreateFieldIterator(TdxDocContentState.TextBoxes);
  Result := AResult;
end;

function TdxDocIteratorStateFactory.CreateHeaderTextBoxesState: TdxDocIteratorState;
var
  AThreshold: Integer;
  ATextBoxes: TdxDocObjectCollectionDictionary;
  AReferences: TdxIntegerList;
  AResult: TdxHeaderTextBoxesIteratorState;
begin
  AThreshold := FileInfo.HeaderTextBoxesStart + FileInfo.HeaderTextBoxesLength;
  ATextBoxes := ContentIterator.FloatingObjectsIterator.HeaderDocumentTextBoxes;
  AReferences := ContentIterator.FloatingObjectsIterator.HeaderDocumentTextBoxReferences;
  AResult := TdxHeaderTextBoxesIteratorState.Create(AThreshold, ATextBoxes, AReferences, TableStreamReader, FileInfo);
  AResult.FieldIterator := CreateFieldIterator(TdxDocContentState.HeaderTextBoxes);
  Result := AResult;
end;

function TdxDocIteratorStateFactory.CreateFinalState: TdxDocIteratorState;
begin
  Result := TdxFinalIteratorState.Create(MaxInt, nil);
end;

function TdxDocIteratorStateFactory.CreateFieldIterator(AStateType: TdxDocContentState): TdxDocFieldsIterator;
begin
  case AStateType of
    TdxDocContentState.MainDocument:
      Result := TdxDocFieldsIterator.Create(TableStreamReader, FileInfo.MainDocumentFieldTableOffset, FileInfo.MainDocumentFieldTableSize);
    TdxDocContentState.Footnotes:
      Result := TdxDocFieldsIterator.Create(TableStreamReader, FileInfo.FootNotesFieldTableOffset, FileInfo.FootNotesFieldTableSize);
    TdxDocContentState.HeadersFootersStory:
      Result := TdxDocFieldsIterator.Create(TableStreamReader, FileInfo.HeadersFootersFieldTableOffset, FileInfo.HeadersFootersFieldTableSize);
    TdxDocContentState.Comments:
      Result := TdxDocFieldsIterator.Create(TableStreamReader, FileInfo.CommentsFieldTableOffset, FileInfo.CommentsFieldTableSize);
    TdxDocContentState.Endnotes:
      Result := TdxDocFieldsIterator.Create(TableStreamReader, FileInfo.EndNotesFieldTableOffset, FileInfo.EndNotesFieldTableSize);
    TdxDocContentState.TextBoxes:
      Result := TdxDocFieldsIterator.Create(TableStreamReader, FileInfo.MainDocumentTextBoxesFieldTableOffset, FileInfo.MainDocumentTextBoxesFieldTableSize);
    TdxDocContentState.HeaderTextBoxes:
      Result := TdxDocFieldsIterator.Create(TableStreamReader, FileInfo.HeaderTextBoxesFieldTableOffset, FileInfo.HeaderTextBoxesFieldTableSize);
    else
      Result := nil;
  end;
end;

{ TdxDocNotesIterator }

constructor TdxDocNotesIterator.Create(AFib: TdxFileInformationBlock; AReader: TBinaryReader);
begin
  FFootNotes := TdxDocObjectCollectionDictionary.Create([doOwnsValues]);
  FEndNotes := TdxDocObjectCollectionDictionary.Create([doOwnsValues]);
  GetReferences(AReader, AFib);
end;

destructor TdxDocNotesIterator.Destroy;
begin
  FFootNotes.Free;
  FEndNotes.Free;
  FFootNotesReferences.Free;
  FEndNoteReferences.Free;
  inherited Destroy;
end;

procedure TdxDocNotesIterator.GetReferences(AReader: TBinaryReader; AFib: TdxFileInformationBlock);
begin
  FFootNotesReferences := GetNoteReferencesCore(AReader, AFib.FootNotesReferenceOffset, AFib.FootNotesReferenceSize);
  FEndNoteReferences := GetNoteReferencesCore(AReader, AFib.EndNotesReferenceOffset, AFib.EndNotesReferenceSize);
end;

function TdxDocNotesIterator.GetNoteReferencesCore(AReader: TBinaryReader; AOffset: Integer; ASize: Integer): TdxIntegerList;
var
  ACount, I: Integer;
begin
  Result := TdxIntegerList.Create;
  ACount := (ASize - TdxDocConstants.CharacterPositionSize) div (TdxDocConstants.CharacterPositionSize + FlagSize);
  AReader.BaseStream.Seek(AOffset, TSeekOrigin.soBeginning);
  for I := 0 to ACount - 1 do
    Result.Add(AReader.ReadInt32);
end;

function TdxDocNotesIterator.GetNoteObject(AInfo: TdxDocObjectInfo; APropertyContainer: TdxDocPropertyContainer): IdxDocObject;
var
  ACharacterPosition: Integer;
begin
  ACharacterPosition := AInfo.Position;
  if FootNoteReferences.Contains(ACharacterPosition) then
    Exit(TdxDocObjectFactory.Instance.CreateDocObject(TdxDocObjectType.AutoNumberedFootnoteReference, AInfo, APropertyContainer));
  if EndNoteReferences.Contains(ACharacterPosition) then
    Exit(TdxDocObjectFactory.Instance.CreateDocObject(TdxDocObjectType.EndnoteReference, AInfo, APropertyContainer));
  Result := TdxDocObjectFactory.Instance.CreateDocObject(TdxDocObjectType.NoteNumber, AInfo, APropertyContainer);
end;

function TdxDocNotesIterator.GetFootNoteObjects(ACharacterPosition: Integer): TdxDocObjectCollection;
begin
  if not FootNotes.TryGetValue(ACharacterPosition, Result) then
    Result := TdxDocObjectCollection.Create;
end;

function TdxDocNotesIterator.GetEndNoteObjects(ACharacterPosition: Integer): TdxDocObjectCollection;
begin
  if not EndNotes.TryGetValue(ACharacterPosition, Result) then
    Result := TdxDocObjectCollection.Create;
end;

{ TdxDocContentIterator }

constructor TdxDocContentIterator.Create(ABuilder: TObject{TdxDocContentBuilder}; AFib: TdxFileInformationBlock;
  AMainStreamReader: TBinaryReader; ATableStreamReader: TBinaryReader; AModel: TdxDocumentModel);
begin
  FBuilder := ABuilder;
  FMainTextDocObjects := TdxDocObjectCollection.Create;
  FMacroObjects := TdxDocObjectCollection.Create;
  FDocumentHeadersFooters := TdxDocHeadersFooters.Create;
  SetTextStreamBorders(AFib);
  InitializeIterators(AFib, AMainStreamReader, ATableStreamReader, AModel);
  FFactory := TdxDocIteratorStateFactory.Create(AFib, AMainStreamReader, ATableStreamReader, Self);
  FState := FFactory.CreateState(TdxDocContentState.MainDocument);
  SwitchToNextState;
end;

destructor TdxDocContentIterator.Destroy;
begin
  FState.Free;
  FBookmarkIterator.Free;
  FCommentsIterator.Free;
  FNotesIterator.Free;
  FFloatingObjectsIterator.Free;
  FPermissionsIterator.Free;
  FMainTextDocObjects.Free;
  FMacroObjects.Free;
  FDocumentHeadersFooters.Free;
  FFactory.Free;
  FArtContent.Free;
  inherited Destroy;
end;

function TdxDocContentIterator.GetHeadersDocumentOrigin: Integer;
begin
  Result := FMainDocumentLength + FFootNotesTextLength;
end;

function TdxDocContentIterator.GetCommentsDocumentOrigin: Integer;
begin
  Result := HeadersDocumentOrigin + HeadersFootersTextLength;
end;

function TdxDocContentIterator.GetEndNotesDocumentOrigin: Integer;
begin
  Result := CommentsDocumentOrigin + CommentsTextLength;
end;

function TdxDocContentIterator.GetTextBoxesDocumentOrigin: Integer;
begin
  Result := EndNotesDocumentOrigin + EndNotesTextLength;
end;

function TdxDocContentIterator.GetHeaderTextBoxesDocumentOrigin: Integer;
begin
  Result := TextBoxesDocumentOrigin + TextBoxesTextLength;
end;

function TdxDocContentIterator.GetDestination: TdxDocObjectCollection;
begin
  Result := State.Destination;
end;

procedure TdxDocContentIterator.SetTextStreamBorders(AFib: TdxFileInformationBlock);
begin
  MainDocumentLength := AFib.MainDocumentLength;
  FootNotesTextLength := AFib.FootNotesLength;
  HeadersFootersTextLength := AFib.HeadersFootersLength;
  CommentsTextLength := AFib.CommentsLength;
  EndNotesTextLength := AFib.EndNotesLength;
  TextBoxesTextLength := AFib.MainDocumentTextBoxesLength;
  HeaderTextBoxesTextLength := AFib.HeaderTextBoxesLength;
end;

procedure TdxDocContentIterator.InitializeIterators(AFib: TdxFileInformationBlock; AMainStreamReader: TBinaryReader; ATableStreamReader: TBinaryReader; AModel: TdxDocumentModel);
var
  AOptions: TdxDocDocumentImporterOptions;
  AFormatting: TdxFloatingObjectFormatting;
begin
  AOptions := AModel.DocumentImportOptions.Doc;
  FBookmarkIterator := TdxDocBookmarkIterator.Create(AFib, ATableStreamReader, AOptions.KeepBookmarksForRemovedRanges);
  FCommentsIterator := TdxDocCommentsIterator.Create(AFib, ATableStreamReader, False{AOptions.KeepCommentsForRemovedRanges});
  FNotesIterator := TdxDocNotesIterator.Create(AFib, ATableStreamReader);
  if AFib.DrawingObjectTableSize <> 0 then
    FArtContent := TdxOfficeArtContent.FromStream(ATableStreamReader, AMainStreamReader, AFib.DrawingObjectTableOffset, AFib.DrawingObjectTableSize, TdxDocContentBuilder(FBuilder));
  AFormatting := TdxFloatingObjectFormatting(AModel.Cache.FloatingObjectFormattingCache.DefaultItem.Clone);
  FFloatingObjectsIterator := TdxDocFloatingObjectsIterator.Create(ATableStreamReader, AFib, FArtContent, AFormatting);
  FPermissionsIterator := TdxDocRangeEditPermissionIterator.Create(AFib, ATableStreamReader, AOptions.KeepPermissionsForRemovedRanges);
  InitializePageColor(AModel);
end;

procedure TdxDocContentIterator.InitializePageColor(AModel: TdxDocumentModel);
var
  ACount, I: Integer;
  ABackgroundShape: TdxOfficeArtShapeContainer;
  APageColor: TdxAlphaColor;
begin
  if FArtContent = nil then
    Exit;

  ACount := FArtContent.Drawings.Count;
  for I := 0 to ACount - 1 do
  begin
    ABackgroundShape := FArtContent.Drawings[I].DrawingObjectsContainer.BackgroundShape;
    if (ABackgroundShape = nil) or (ABackgroundShape.ShapeRecord.ShapeIdentifier <> TdxOfficeArtConstants.DefaultMainDocumentShapeIdentifier) then
      Continue;

    APageColor := ABackgroundShape.ArtProperties.FillColor;
    if not TdxAlphaColors.IsEmpty(APageColor) then
      AModel.DocumentProperties.PageBackColor := APageColor;
    Break;
  end;
end;

function TdxDocContentIterator.ShouldProcessTextRun: Boolean;
begin
  Result := not State.ReadingComplete;
end;

function TdxDocContentIterator.AdvanceNext(AOffset: Integer): Integer;
begin
  Result := CurrentCharacterPosition;
  CurrentCharacterPosition := CurrentCharacterPosition + AOffset;
  State.AdvanceNext(AOffset);
end;

procedure TdxDocContentIterator.UpdateState;
begin
  State.UpdateDestination;
  SwitchToNextState;
end;

procedure TdxDocContentIterator.AddToGC(AObject: TObject);
begin
  TdxDocContentBuilder(FBuilder).AddToGC(AObject);
end;

function TdxDocContentIterator.AdvanceField(APropertyContainer: TdxDocPropertyContainer): Boolean;
begin
  if State.FieldIterator = nil then
    Exit(False);
  Result := State.FieldIterator.MoveNext(APropertyContainer);
end;

function TdxDocContentIterator.ShapeField: Boolean;
begin
  if State.FieldIterator = nil then
    Exit(False);
  Result := State.FieldIterator.ShapeField;
end;

procedure TdxDocContentIterator.CheckFieldCompatibility(ADestination: TdxDocObjectCollection);
begin
  State.FieldIterator.CheckFieldCompatibility(ADestination);
end;

function TdxDocContentIterator.GetFloatingObject(var AObjectInfo: TdxDocObjectInfo; APropertyContainer: TdxDocPropertyContainer): TdxDocFloatingObjectBase;
begin
  Result := FloatingObjectsIterator.GetFloatingObject(GetRelativeCharacterPosition(AObjectInfo), AObjectInfo, APropertyContainer);
end;

function TdxDocContentIterator.GetNoteObject(var AObjectInfo: TdxDocObjectInfo; APropertyContainer: TdxDocPropertyContainer): IdxDocObject;
begin
  Result := NotesIterator.GetNoteObject(AObjectInfo, APropertyContainer);
end;

function TdxDocContentIterator.GetCurrentFieldType: TdxFieldType;
begin
  Result := State.FieldIterator.GetCurrentFieldType;
end;

procedure TdxDocContentIterator.AdvancePosition(ALogPosition: TdxDocumentLogPosition; AOriginalPosition: Integer; ALength: Integer);
begin
  BookmarkIterator.AdvanceNext(ALogPosition, AOriginalPosition, ALength);
  CommentsIterator.AdvanceNext(ALogPosition, AOriginalPosition, ALength);
  PermissionsIterator.AdvanceNext(ALogPosition, AOriginalPosition, ALength);
end;

procedure TdxDocContentIterator.InsertBookmarks(APieceTable: TdxPieceTable);
begin
  BookmarkIterator.InsertBookmarks(APieceTable);
  PermissionsIterator.InsertRangeEditPermissions(APieceTable);
end;

procedure TdxDocContentIterator.SwitchToNextState;
begin
  while State.ShouldChangeState(CurrentCharacterPosition) do
    ChangeState;
end;

procedure TdxDocContentIterator.ChangeState;
var
  ANextState: TdxDocContentState;
begin
  FState.FinishState;
  ANextState := State.NextState;
  FState.Free;

  FState := FFactory.CreateState(ANextState);
  FloatingObjectsIterator.State := State.CurrentState;
end;

function TdxDocContentIterator.GetRelativeCharacterPosition(AInfo: TdxDocObjectInfo): Integer;
var
  APosition: Integer;
begin
  APosition := AInfo.Position;
  case State.CurrentState of
    TdxDocContentState.MainDocument:
      Result := APosition;
    TdxDocContentState.Footnotes:
      Result := APosition - MainDocumentLength;
    TdxDocContentState.HeadersFootersStory:
      Result := APosition - HeadersDocumentOrigin;
    TdxDocContentState.Comments:
      Result := APosition - CommentsDocumentOrigin;
    TdxDocContentState.Endnotes:
      Result := APosition - EndNotesDocumentOrigin;
    TdxDocContentState.TextBoxes:
      Result := APosition - TextBoxesDocumentOrigin;
    TdxDocContentState.HeaderTextBoxes:
      Result := APosition - HeaderTextBoxesDocumentOrigin;
    else
      Result := APosition;
  end;
end;

procedure TdxDocContentIterator.BeginEmbeddedContent(AContent: TdxDocObjectCollection);
var
  AStart, AEnd: Integer;
  ALastItem: IdxDocObject;
begin
  AStart := AContent[0].Position;
  ALastItem := AContent[AContent.Count - 1];
  AEnd := ALastItem.Position + ALastItem.Length - 1;
  BookmarkIterator.BeginEmbeddedContent(AStart, AEnd);
  PermissionsIterator.BeginEmbeddedContent(AStart, AEnd);
  CommentsIterator.BeginEmbeddedContent(AStart, AEnd);
end;

procedure TdxDocContentIterator.EndEmbeddedContent;
begin
  BookmarkIterator.EndEmbeddedContent;
  PermissionsIterator.EndEmbeddedContent;
  CommentsIterator.EndEmbeddedContent;
end;

end.
