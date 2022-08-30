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

unit dxRichEdit.DocumentModel.Fields.TocField;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, Classes, Generics.Defaults, Generics.Collections,
  dxCoreClasses,

  dxRichEdit.Utils.Types,
  dxGenerics,
  dxRichEdit.InternalRichEditDocumentServer,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.InlineObjectRange,
  dxRichEdit.DocumentModel.TextRange,
  dxRichEdit.DocumentModel.Bookmarks,
  dxRichEdit.DocumentModel.Section,
  dxRichEdit.DocumentModel.TabFormatting,
  dxRichEdit.DocumentModel.ParagraphRange,
  dxRichEdit.DocumentModel.Exporter,
  dxRichEdit.DocumentModel.Fields.Core,
  dxRichEdit.DocumentModel.Fields,
  dxRichEdit.DocumentModel.Fields.TocEntryField,
  dxRichEdit.DocumentModel.Fields.SequenceField,
  dxRichEdit.DocumentModel.FieldRange,
  dxRichEdit.DocumentModel.FieldCalculatorService,
  dxRichEdit.DocumentModel.MailMerge,
  dxRichEdit.DocumentLayout;

type
  { TdxStyleOutlineLevelInfo }

  TdxStyleOutlineLevelInfo = record
  strict private
    FStyleName: string;
    FOutlineLevel: Integer;
  public
    constructor Create(const AStyleName: string; AOutlineLevel: Integer);
    property StyleName: string read FStyleName;
    property OutlineLevel: Integer read FOutlineLevel;
  end;

  { TdxTocGeneratorOptions }

  TdxTocGeneratorOptions = class
  strict private
    FAdditionalStyles: TList<TdxStyleOutlineLevelInfo>;
    FBookmarkName: TdxNullableString;
    FCreateHyperlinks: Boolean;
    FDelimiter: TdxNullableString;
    FEntriesGroupId: TdxNullableString;
    FEntriesLevels: TdxIntegerList;
    FHeaderLevels: TdxIntegerList;
    FNoPageNumberLevels: TdxIntegerList;
    FPageNumberSeparator: TdxNullableString;
    FPrefixedSequenceId: TdxNullableString;
    FSequenceId: TdxNullableString;
    FUseParagraphOutlineLevel: Boolean;
    procedure SetAdditionalStyles(const Value: TList<TdxStyleOutlineLevelInfo>);
    procedure SetEntriesLevels(const Value: TdxIntegerList);
    procedure SetHeaderLevels(const Value: TdxIntegerList);
    procedure SetNoPageNumberLevels(const Value: TdxIntegerList);
  public
    constructor Create;
    destructor Destroy; override;

    property AdditionalStyles: TList<TdxStyleOutlineLevelInfo> read FAdditionalStyles write SetAdditionalStyles;
    property BookmarkName: TdxNullableString read FBookmarkName write FBookmarkName;
    property CreateHyperlinks: Boolean read FCreateHyperlinks write FCreateHyperlinks;
    property Delimiter: TdxNullableString read FDelimiter write FDelimiter;
    property EntriesGroupId: TdxNullableString read FEntriesGroupId write FEntriesGroupId;
    property EntriesLevels: TdxIntegerList read FEntriesLevels write SetEntriesLevels;
    property HeaderLevels: TdxIntegerList read FHeaderLevels write SetHeaderLevels;
    property NoPageNumberLevels: TdxIntegerList read FNoPageNumberLevels write SetNoPageNumberLevels;
    property PageNumberSeparator: TdxNullableString read FPageNumberSeparator write FPageNumberSeparator;
    property PrefixedSequenceId: TdxNullableString read FPrefixedSequenceId write FPrefixedSequenceId;
    property SequenceId: TdxNullableString read FSequenceId write FSequenceId;
    property UseParagraphOutlineLevel: Boolean read FUseParagraphOutlineLevel write FUseParagraphOutlineLevel;
  end;

  { TdxTocField }

  TdxTocField = class(TdxCalculatedFieldBase)
  public const
    FieldType = 'TOC';
  strict private
    class var
      FSwitchesWithArgument: TdxStringBooleanDictionary;
    class constructor Initialize;
    class destructor Finalize;
  strict private
    FOptions: TdxTocGeneratorOptions;
    FTargetModel: TdxDocumentModel;
    FDocumentLayoutService: IdxDocumentLayoutService;
    FFieldsToUpdate: TdxFieldList;
    function GetFieldSection(APieceTable: TdxPieceTable; ADocumentField: TdxField): TdxSection;
    function GetLevelsRange(const AValue: TdxNullableString;
      const ADefaultValue: string): TdxIntegerList;
    function GetLevelsRangeCore(const AValue: TdxNullableString): TdxIntegerList;
    function GetAdditionalStylesInfo(const AValue: TdxNullableString): TList<TdxStyleOutlineLevelInfo>;
  protected
    function GetSwitchesWithArguments: TdxStringBooleanDictionary; override;
    function GetFieldTypeName: string; override;
    function MailMergeType: TdxFieldMailMergeType; override;
    function GetCanPrepare: Boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    class function CreateField: TdxCalculatedFieldBase; override;

    function CanUseSwitchWithoutArgument(const AFieldSpecificSwitch: string): Boolean; override;
    function GetCalculatedValueCore(ASourcePieceTable: TdxCustomPieceTable;
      AMailMergeDataMode: TdxMailMergeDataMode; ADocumentField: TdxField): TdxCalculatedFieldValue; override;
    procedure Initialize(APieceTable: TdxCustomPieceTable; AInstructions: TdxInstructionCollection); override;
    procedure BeforeCalculateFields(ASourcePieceTable: TdxCustomPieceTable; ADocumentField: TdxField); override;

    property Options: TdxTocGeneratorOptions read FOptions;
  end;

  { TdxTocGenerator }

  TdxTocGenerator = class(TdxDocumentModelExporter)
  strict private
    FTargetDocumentModel: TdxDocumentModel;
    FParagraphText: TStringBuilder;
    FOptions: TdxTocGeneratorOptions;
    FTocFields: TdxFieldList;
    FRightTabPosition: Integer;
    FAnalyzeFields: Boolean;
    FStart: TdxDocumentModelPosition;
    FEnd: TdxDocumentModelPosition;
    function GetTargetPieceTable: TdxPieceTable;
  protected
    FCurrentRunIndex: TdxRunIndex;
    FCurrentParagraphOutlineLevel: Integer;
    FPageNumberPrefix: string;
    FNumberingListIndent: Integer;
    function ShouldExportHiddenText: Boolean; override;
    procedure ExportDocument; override;
    procedure SetTocSearchRange; virtual;
    procedure SetDefaultTocSearchRange; virtual;
    procedure ExportSectionHeadersFooters(ASection: TdxSection); override;
    function ShouldSplitRuns: Boolean; override;
    procedure PushVisitableDocumentIntervalBoundaryIterator; override;
    procedure PopVisitableDocumentIntervalBoundaryIterator; override;
    procedure TryToExportBookmarks(ARunIndex: TdxRunIndex; ARunOffset: Integer); override;
    procedure ExportRun(I: TdxRunIndex); override;
    procedure ExportFieldCodeStartRun(ARun: TdxFieldCodeStartRun); override;
    procedure ProcessField(AField: TdxField); virtual;
    procedure ProcessCalculatedField(AField: TdxCalculatedFieldBase;
      AScanner: TdxFieldScanner; ADocumentField: TdxField); virtual;
    procedure InitializeField(AField: TdxCalculatedFieldBase; AScanner: TdxFieldScanner); virtual;
    procedure ProcessTocEntryField(AEntry: TdxTocEntryField); virtual;
    procedure ProcessSequenceField(ASequence: TdxSequenceField; ADocumentField: TdxField); virtual;
    function GetSequenceFieldResultText(ASequence: TdxSequenceField; ADocumentField: TdxField): string;
    procedure ExportSection(const ASection: TdxSection); override;
    procedure ExportParagraphs(AFrom, ATo: TdxParagraphIndex); override;
    function ExportParagraph(AParagraph: TdxParagraph): TdxParagraphIndex; override;
    procedure AppendNumberingToParagraph(AParagraph: TdxParagraph);
    procedure ExportTextRun(ARun: TdxTextRun); override;
    procedure ExportInlinePictureRun(ARun: TdxInlinePictureRun); override;
    procedure ExportParagraphRun(ARun: TdxParagraphRun); override;
    procedure AppendTocEntry(const AText: string; ABookmark: TdxBookmark; ALevel: Integer);
    procedure ApplyTabsToParagraph(ATocParagraph: TdxParagraph);
    function ApplyPageNumber(const AText: string; ATarget: TdxParagraph): string;
    function ObtainTocBookmarkForHeaderOrOutline(AParagraph: TdxParagraph): TdxBookmark;
    function ObtainTocBookmarkForTocEntry(AEntry: TdxTocEntryField; ATocEntryFieldCodeFirstRunIndex: TdxRunIndex): TdxBookmark;
    function GenerateTocBookmarkName(APieceTable: TdxPieceTable): string; virtual;
    function LookupEntireParagraphBookmark(AParagraph: TdxParagraph; AActualParagraphPosition: TdxDocumentLogPosition): TdxBookmark; virtual;
    function LookupZeroLengthBookmarkAtLogPosition(APos: TdxDocumentLogPosition): TdxBookmark; virtual;
    function LookupExactBookmark(APos: TdxDocumentLogPosition; ALen: Integer): TdxBookmark; virtual;
    procedure ApplyTocStyleToParagraph(ALevel: Integer; ATocParagraph: TdxParagraph);
    function CalculateTocParagraphStyleIndex(ALevel: Integer): Integer;
    function ShouldWritePageNumberIntoToc(ALevel: Integer): Boolean;
    function GetActualOutlineLevel(AParagraph: TdxParagraph): Integer;
    function GetOutlineLevelByStyleName(const AStyleName: string): Integer;
    function CalculateActualParagraphPosition(AParagraph: TdxParagraph): TdxDocumentLogPosition;
    function IsWhiteSpace(ACh: Char): Boolean;
    function CanIncludeParagraphIntoToc(AParagraph: TdxParagraph): Boolean;
    function IsParagraphInsideField(AParagraph: TdxParagraph; AField: TdxField): Boolean;
  public
    constructor Create(ADocumentModel, ATargetDocumentModel: TdxDocumentModel;
      AOptions: TdxTocGeneratorOptions; ARightTabPosition: Integer); reintroduce;
    destructor Destroy; override;

    procedure Export(AOutputStream: TStream); override;

    property TargetDocumentModel: TdxDocumentModel read FTargetDocumentModel;
    property TargetPieceTable: TdxPieceTable read GetTargetPieceTable;
    property Options: TdxTocGeneratorOptions read FOptions;
  end;

implementation

uses
  Math, Windows, Character, Rtti,
  dxCore,
  dxRichEdit.Strs,
  dxRichEdit.ServiceManager,
  dxRichEdit.DocumentModel.Numbering,
  dxRichEdit.DocumentModel.ParagraphFormatting,
  dxRichEdit.DocumentModel.Hyperlink,
  dxRichEdit.DocumentModel.Fields.PageRefField,
  dxRichEdit.DocumentModel.VisibleTextFilter.Simple,
  dxRichEdit.LayoutEngine.Formatter,
  dxCharacters,
  dxRichEdit.Utils.Exceptions.Strs,
  dxRichEdit.Utils.Token,
  dxStringHelper;


{ TdxStyleOutlineLevelInfo }

constructor TdxStyleOutlineLevelInfo.Create(const AStyleName: string; AOutlineLevel: Integer);
begin
  FStyleName := AStyleName;
  FOutlineLevel := AOutlineLevel;
end;

{ TdxTocGeneratorOptions }

constructor TdxTocGeneratorOptions.Create;
begin
  inherited Create;
  FNoPageNumberLevels := TdxIntegerList.Create;
  FHeaderLevels := TdxIntegerList.Create;
  FEntriesLevels := TdxIntegerList.Create;
  FAdditionalStyles := TList<TdxStyleOutlineLevelInfo>.Create;
  FPageNumberSeparator := '';
end;

destructor TdxTocGeneratorOptions.Destroy;
begin
  FreeAndNil(FNoPageNumberLevels);
  FreeAndNil(FHeaderLevels);
  FreeAndNil(FEntriesLevels);
  FreeAndNil(FAdditionalStyles);
  inherited Destroy;
end;

procedure TdxTocGeneratorOptions.SetAdditionalStyles(
  const Value: TList<TdxStyleOutlineLevelInfo>);
begin
  if FAdditionalStyles = Value then
    Exit;
  FAdditionalStyles.Free;
  FAdditionalStyles := Value;
end;

procedure TdxTocGeneratorOptions.SetEntriesLevels(const Value: TdxIntegerList);
begin
  if FEntriesLevels = Value then
    Exit;
  FEntriesLevels.Free;
  FEntriesLevels := Value;
end;

procedure TdxTocGeneratorOptions.SetHeaderLevels(const Value: TdxIntegerList);
begin
  if FHeaderLevels = Value then
    Exit;
  FHeaderLevels.Free;
  FHeaderLevels := Value;
end;

procedure TdxTocGeneratorOptions.SetNoPageNumberLevels(
  const Value: TdxIntegerList);
begin
  if FNoPageNumberLevels = Value then
    Exit;
  FNoPageNumberLevels.Free;
  FNoPageNumberLevels := Value;
end;

{ TdxTocField }

class constructor TdxTocField.Initialize;
begin
  FSwitchesWithArgument := CreateSwitchesWithArgument(TArray<string>.Create('o', 'n', 't', 'l', 'f', 'a', 'b', 'c', 'd', 'p', 's'));
end;

class destructor TdxTocField.Finalize;
begin
  FreeAndNil(FSwitchesWithArgument);
end;

constructor TdxTocField.Create;
begin
  inherited Create;
  FOptions := TdxTocGeneratorOptions.Create;
end;

destructor TdxTocField.Destroy;
begin
  FreeAndNil(FOptions);
  inherited Destroy;
end;

class function TdxTocField.CreateField: TdxCalculatedFieldBase;
begin
  Result := TdxTocField.Create;
end;

function TdxTocField.GetSwitchesWithArguments: TdxStringBooleanDictionary;
begin
  Result := FSwitchesWithArgument;
end;

function TdxTocField.GetFieldTypeName: string;
begin
  Result := FieldType;
end;

procedure TdxTocField.Initialize(APieceTable: TdxCustomPieceTable; AInstructions: TdxInstructionCollection);
var
  ANoPageNumberLevels: TdxNullableString;
begin
  inherited Initialize(APieceTable, AInstructions);
  FOptions.PageNumberSeparator := AInstructions.GetString('p');
  FOptions.UseParagraphOutlineLevel := AInstructions.GetBool('u');
  FOptions.CreateHyperlinks := AInstructions.GetBool('h');
  FOptions.HeaderLevels := GetLevelsRange(AInstructions.GetString('o'), '1-9');
  FOptions.BookmarkName := AInstructions.GetString('b');
  ANoPageNumberLevels := AInstructions.GetString('n');
  if not ANoPageNumberLevels.IsNull then
    FOptions.NoPageNumberLevels := GetLevelsRange(ANoPageNumberLevels, '1-9')
  else
    if AInstructions.GetBool('n') then
      FOptions.NoPageNumberLevels := GetLevelsRange('1-9', '1-9');

  FOptions.AdditionalStyles := GetAdditionalStylesInfo(AInstructions.GetString('t'));

  FOptions.EntriesLevels := GetLevelsRange(AInstructions.GetString('l'), '');
  if FOptions.EntriesLevels.Count > 0 then
  begin
    FOptions.HeaderLevels.Clear;
    FOptions.AdditionalStyles.Clear;
  end;

  FOptions.EntriesGroupId := AInstructions.GetString('f');
  if not FOptions.EntriesGroupId.IsNullOrEmpty then
  begin
    FOptions.HeaderLevels.Clear;
    FOptions.AdditionalStyles.Clear;
  end;

  FOptions.SequenceId := AInstructions.GetString('c');
  if not FOptions.SequenceId.IsNullOrEmpty then
  begin
    FOptions.HeaderLevels.Clear;
    FOptions.AdditionalStyles.Clear;
    FOptions.HeaderLevels.Clear;
    FOptions.EntriesGroupId := '';
  end;
  FOptions.PrefixedSequenceId := AInstructions.GetString('s');
  FOptions.Delimiter := AInstructions.GetString('d');
  if FOptions.Delimiter.IsNullOrEmpty then
    FOptions.Delimiter := '-';
end;

function TdxTocField.MailMergeType: TdxFieldMailMergeType;
begin
  Result := TdxFieldMailMergeType.NonMailMerge;
end;

function TdxTocField.GetCanPrepare: Boolean;
begin
  Result := True;
end;

procedure TdxTocField.BeforeCalculateFields(ASourcePieceTable: TdxCustomPieceTable; ADocumentField: TdxField);
var
  ASection: TdxSection;
  ARightTabPosition: Integer;
  AGenerator: TdxTocGenerator;
  AUpdater: TdxFieldUpdater;
  APieceTable: TdxPieceTable absolute ASourcePieceTable;
begin
  inherited BeforeCalculateFields(ASourcePieceTable, ADocumentField);
  FTargetModel := APieceTable.DocumentModel.GetFieldResultModel;
  ASection := GetFieldSection(APieceTable, ADocumentField);
  ARightTabPosition := ASection.Page.Width - ASection.Margins.Left - ASection.Margins.Right;

  AGenerator := TdxTocGenerator.Create(APieceTable.DocumentModel, FTargetModel, FOptions, ARightTabPosition);
  try
    AGenerator.Export;
    FDocumentLayoutService := APieceTable.DocumentModel.GetService<IdxDocumentLayoutService>;
    if FDocumentLayoutService = nil then
      Exit;
    FTargetModel.ReplaceService<IdxBookmarkResolutionService>(TdxBookmarkResolutionService.Create(APieceTable));
    FTargetModel.ReplaceService<IdxDocumentLayoutService>(FDocumentLayoutService);
    FTargetModel.BeginUpdate;
    AUpdater := FTargetModel.MainPieceTable.FieldUpdater;
    FFieldsToUpdate := AUpdater.GetFieldsToUpdate(FTargetModel.MainPieceTable.Fields, nil);
    AUpdater.PrepareFieldsCore(FFieldsToUpdate, TdxUpdateFieldOperationType.Normal);
  finally
    AGenerator.Free;
  end;
end;

function TdxTocField.GetFieldSection(APieceTable: TdxPieceTable; ADocumentField: TdxField): TdxSection;
var
  ADocumentModel: TdxDocumentModel;
  APosition: TdxDocumentModelPosition;
  ASectionIndex: TdxSectionIndex;
begin
  ADocumentModel := APieceTable.DocumentModel;
  if APieceTable.ContentType.IsMain then
  begin
    APosition := TdxDocumentModelPosition.FromRunStart(APieceTable, ADocumentField.FirstRunIndex);
    ASectionIndex := ADocumentModel.FindSectionIndex(APosition.LogPosition);
    Exit(ADocumentModel.Sections[ASectionIndex]);
  end
  else
  begin
    Exit(ADocumentModel.Sections[0]);
  end;
end;

function TdxTocField.GetCalculatedValueCore(ASourcePieceTable: TdxCustomPieceTable;
  AMailMergeDataMode: TdxMailMergeDataMode; ADocumentField: TdxField): TdxCalculatedFieldValue;
var
  AServices: IdxServiceContainer;
begin
  try
    if (FDocumentLayoutService = nil) or (FTargetModel = nil) then
      Exit(TdxCalculatedFieldValue.Create(TValue.From<TdxDocumentModel>(FTargetModel)));
    try
      FTargetModel.MainPieceTable.FieldUpdater.UpdateFieldsCore(FFieldsToUpdate, TdxUpdateFieldOperationType.Normal);
    finally
      AServices := FTargetModel as IdxServiceContainer;
      AServices.RemoveService(IdxBookmarkResolutionService);
      AServices.RemoveService(IdxDocumentLayoutService);
    end;
    Result := TdxCalculatedFieldValue.Create(TValue.From<TdxDocumentModel>(FTargetModel));
  finally
    if FTargetModel <> nil then
      FTargetModel.EndUpdate;
    FTargetModel := nil;
    FDocumentLayoutService := nil;
    FreeAndNil(FFieldsToUpdate);
  end;
end;

function TdxTocField.GetLevelsRange(const AValue: TdxNullableString;
  const ADefaultValue: string): TdxIntegerList;
begin
  Result := GetLevelsRangeCore(AValue);
  if Result = nil then
    Result := GetLevelsRangeCore(ADefaultValue);
  if Result = nil then
    Result := TdxIntegerList.Create;
end;

function TdxTocField.GetLevelsRangeCore(const AValue: TdxNullableString): TdxIntegerList;
var
  AParts: TArray<string>;
  AStart, AEnd, I: Integer;
begin
  if AValue.IsNullOrEmpty then
    Exit(nil);
  AParts := TdxStringHelper.Split(AValue.Value, ['-']);
  if (AParts = nil) or (Length(AParts) <> 2) then
    Exit(nil);

  if not TryStrToInt(AParts[0], AStart) then
    Exit(nil);
  if not TryStrToInt(AParts[1], AEnd) then
    Exit(nil);

  if AEnd < AStart then
    Exit(nil);

  Result := TdxIntegerList.Create;
  for I := AStart to AEnd do
    Result.Add(I);
end;

function TdxTocField.GetAdditionalStylesInfo(const AValue: TdxNullableString): TList<TdxStyleOutlineLevelInfo>;
var
  AParts: TArray<string>;
  ACount, I, AOutlineLevel: Integer;
  AStyleName, AOutlineLevelString: string;
begin
  Result := TList<TdxStyleOutlineLevelInfo>.Create;
  if AValue.IsNullOrEmpty then
    Exit;

  AParts := TdxStringHelper.Split(AValue.Value, [',']);
  ACount := Length(AParts) div 2;
  for I := 0 to ACount - 1 do
  begin
    AStyleName := Trim(AParts[I * 2]);
    if AStyleName = '' then
      Continue;

    AOutlineLevelString := Trim(AParts[I * 2 + 1]);
    if TryStrToInt(AOutlineLevelString, AOutlineLevel) and (AOutlineLevel >= 0) and (AOutlineLevel <= 9) then
      Result.Add(TdxStyleOutlineLevelInfo.Create(AStyleName, AOutlineLevel));
  end;
end;

function TdxTocField.CanUseSwitchWithoutArgument(const AFieldSpecificSwitch: string): Boolean;
begin
  Result := AFieldSpecificSwitch = '\n';
end;

{ TdxTocGenerator }

constructor TdxTocGenerator.Create(ADocumentModel, ATargetDocumentModel: TdxDocumentModel;
  AOptions: TdxTocGeneratorOptions; ARightTabPosition: Integer);
begin
  Assert(ATargetDocumentModel <> nil, 'ATargetDocumentModel');
  Assert(AOptions <> nil, 'AOptions');
  inherited Create(ADocumentModel, nil);
  FTargetDocumentModel := ATargetDocumentModel;
  FOptions := AOptions;
  FRightTabPosition := ARightTabPosition;
  FCurrentRunIndex := -1;
  FParagraphText := TStringBuilder.Create;
end;

destructor TdxTocGenerator.Destroy;
begin
  FreeAndNil(FParagraphText);
  inherited Destroy;
end;

procedure TdxTocGenerator.Export(AOutputStream: TStream);
begin
end;

function TdxTocGenerator.GetTargetPieceTable: TdxPieceTable;
begin
  Result := FTargetDocumentModel.MainPieceTable;
end;

function TdxTocGenerator.ShouldExportHiddenText: Boolean;
begin
  Result := False;
end;

procedure TdxTocGenerator.ExportDocument;
begin
  DocumentModel.BeginUpdate;
  try
    SetTocSearchRange;
    FTocFields := DocumentModel.MainPieceTable.GetTocFields;
    try
      FAnalyzeFields := FOptions.EntriesGroupId.IsNullOrEmpty or
        (FOptions.EntriesLevels.Count >= 0) or
        FOptions.SequenceId.IsNullOrEmpty;
      inherited ExportDocument;
      if TargetDocumentModel.IsEmpty then
        TargetDocumentModel.MainPieceTable.InsertText(0, cxGetResourceString(@sdxRichEditExceptionNoTocEntriesFound));
    finally
      FreeAndNil(FTocFields);
    end;
  finally
    DocumentModel.EndUpdate;
  end;
end;

procedure TdxTocGenerator.SetTocSearchRange;
var
  ABookmark: TdxBookmark;
begin
  if FOptions.BookmarkName.IsNullOrEmpty then
  begin
    SetDefaultTocSearchRange;
    Exit;
  end;

  ABookmark := PieceTable.Bookmarks.FindByName(FOptions.BookmarkName.Value);
  if ABookmark = nil then
  begin
    SetDefaultTocSearchRange;
    Exit;
  end;

  FStart := TdxDocumentModelPosition.Create(PieceTable);
  FStart.LogPosition := ABookmark.NormalizedStart;
  FStart.Update;

  FEnd := TdxDocumentModelPosition.Create(PieceTable);
  FEnd.LogPosition := ABookmark.NormalizedEnd;
  FEnd.Update;
end;

procedure TdxTocGenerator.SetDefaultTocSearchRange;
begin
  FStart := TdxDocumentModelPosition.FromParagraphStart(PieceTable, 0);
  FEnd := TdxDocumentModelPosition.FromParagraphEnd(PieceTable, PieceTable.Paragraphs.Count - 1);
end;

procedure TdxTocGenerator.ExportSectionHeadersFooters(ASection: TdxSection);
begin
end;

function TdxTocGenerator.ShouldSplitRuns: Boolean;
begin
  Result := False;
end;

procedure TdxTocGenerator.PushVisitableDocumentIntervalBoundaryIterator;
begin
end;

procedure TdxTocGenerator.PopVisitableDocumentIntervalBoundaryIterator;
begin
end;

procedure TdxTocGenerator.TryToExportBookmarks(ARunIndex: TdxRunIndex; ARunOffset: Integer);
begin
end;

procedure TdxTocGenerator.ExportRun(I: TdxRunIndex);
begin
  FCurrentRunIndex := I;
  inherited ExportRun(I);
end;

procedure TdxTocGenerator.ExportFieldCodeStartRun(ARun: TdxFieldCodeStartRun);
var
  AField: TdxField;
begin
  inherited ExportFieldCodeStartRun(ARun);
  if (FAnalyzeFields and (FCurrentRunIndex >= FStart.RunIndex)) and (FCurrentRunIndex <= FEnd.RunIndex) then
  begin
    AField := PieceTable.FindFieldByRunIndex(FCurrentRunIndex);
    if AField <> nil then
      ProcessField(AField);
  end;
end;

procedure TdxTocGenerator.ProcessField(AField: TdxField);
var
  AIterator: TdxDocumentFieldIterator;
  AScanner: TdxFieldScanner;
  AToken: IdxToken;
  ACalculatedField: TdxCalculatedFieldBase;
begin
  AIterator := TdxDocumentFieldIterator.Create(PieceTable, AField);
  try
    AScanner := TdxFieldScanner.Create(AIterator, PieceTable.DocumentModel.MaxFieldSwitchLength,
      PieceTable.DocumentModel.EnableFieldNames, PieceTable.SupportFieldCommonStringFormat);
    try
      AToken := AScanner.Scan;
      if not (AToken.ActualKind in [TdxTokenKind.OpEQ, TdxTokenKind.Eq]) then
      begin
        ACalculatedField := TdxFieldCalculatorService.CreateField(AToken.Value);
        try
          if ACalculatedField <> nil then
            ProcessCalculatedField(ACalculatedField, AScanner, AField);
        finally
          ACalculatedField.Free;
        end;
      end;
    finally
      AScanner.Free;
    end;
  finally
    AIterator.Free;
  end;
end;

procedure TdxTocGenerator.ProcessCalculatedField(AField: TdxCalculatedFieldBase; AScanner: TdxFieldScanner; ADocumentField: TdxField);
begin
  if AField is TdxTocEntryField then
  begin
    InitializeField(AField, AScanner);
    ProcessTocEntryField(TdxTocEntryField(AField));
  end
  else
    if AField is TdxSequenceField then
    begin
      InitializeField(AField, AScanner);
      ProcessSequenceField(TdxSequenceField(AField), ADocumentField);
    end;
end;

procedure TdxTocGenerator.InitializeField(AField: TdxCalculatedFieldBase; AScanner: TdxFieldScanner);
var
  AInstructions: TdxInstructionCollection;
begin
  AInstructions := TdxFieldCalculatorService.ParseInstructions(AScanner, AField);
  AField.Initialize(PieceTable, AInstructions);
end;

procedure TdxTocGenerator.ProcessTocEntryField(AEntry: TdxTocEntryField);
var
  AText: string;
  ABookmark: TdxBookmark;
begin
  if FOptions.EntriesLevels.Contains(AEntry.Level) or
    (not FOptions.EntriesGroupId.IsNullOrEmpty and
    (FOptions.EntriesGroupId = AEntry.Id)) then
  begin
    AText := Trim(AEntry.Text);
    if AText <> '' then
    begin
      ABookmark := ObtainTocBookmarkForTocEntry(AEntry, FCurrentRunIndex + 1);
      AppendTocEntry(AText, ABookmark, AEntry.Level);
    end;
  end;
end;

procedure TdxTocGenerator.ProcessSequenceField(ASequence: TdxSequenceField; ADocumentField: TdxField);
var
  AText: string;
begin
  if not FOptions.SequenceId.IsNullOrEmpty and
    (FOptions.SequenceId.Value = ASequence.Id) and not ASequence.HideResult then
  begin
    FCurrentParagraphOutlineLevel := 1;
    if FOptions.PrefixedSequenceId = ASequence.Id then
    begin
      AText := GetSequenceFieldResultText(ASequence, ADocumentField);
      if AText <> '' then
        FPageNumberPrefix := AText + FOptions.Delimiter.Value;
    end;
  end;
end;

function TdxTocGenerator.GetSequenceFieldResultText(ASequence: TdxSequenceField; ADocumentField: TdxField): string;
var
  AResult: TdxDocumentModel;
begin
  AResult := PieceTable.DocumentModel.CreateNew;
  try
    Result := AResult.InternalAPI.GetDocumentPlainTextContent;
  finally
    AResult.Free;
  end;
end;

procedure TdxTocGenerator.ExportSection(const ASection: TdxSection);
begin
  inherited ExportSection(ASection);
end;

procedure TdxTocGenerator.ExportParagraphs(AFrom, ATo: TdxParagraphIndex);
begin
  AFrom := Max(AFrom, FStart.ParagraphIndex);
  ATo := Min(ATo, FEnd.ParagraphIndex);
  inherited ExportParagraphs(AFrom, ATo);
end;

function TdxTocGenerator.ExportParagraph(AParagraph: TdxParagraph): TdxParagraphIndex;
var
  AOutlineLevel: Integer;
begin
  FPageNumberPrefix := '';
  FParagraphText.Length := 0;
  FNumberingListIndent := -1;
  AOutlineLevel := GetActualOutlineLevel(AParagraph);
  if Options.HeaderLevels.Contains(AOutlineLevel) and CanIncludeParagraphIntoToc(AParagraph) then
  begin
    FCurrentParagraphOutlineLevel := AOutlineLevel;
    if AParagraph.IsInList then
      AppendNumberingToParagraph(AParagraph);
  end
  else
    FCurrentParagraphOutlineLevel := -1;
  Result := inherited ExportParagraph(AParagraph);
end;

procedure TdxTocGenerator.AppendNumberingToParagraph(AParagraph: TdxParagraph);
var
  ASeparator: string;
  AListLevelIndex: Integer;
  AListLevel: IdxParagraphProperties;
begin
  ASeparator := AParagraph.GetListLevelSeparator;
  if ASeparator = #9 then
  begin
    AListLevelIndex := AParagraph.GetListLevelIndex;
    AListLevel := AParagraph.DocumentModel.NumberingLists[AParagraph.GetNumberingListIndex].Levels[AListLevelIndex] as IdxParagraphProperties;
    FNumberingListIndent := AListLevel.FirstLineIndent;
  end;
  FParagraphText.Append(AParagraph.GetNumberingListText + ASeparator);
end;

procedure TdxTocGenerator.ExportTextRun(ARun: TdxTextRun);
var
  AText: string;
begin
  AText := ARun.GetPlainText(PieceTable.TextBuffer);
  AText := TdxStringHelper.Replace(AText, TdxCharacters.LineBreak, ' ');
  AText := TdxStringHelper.Replace(AText, TdxCharacters.PageBreak, '');
  FParagraphText.Append(AText);
end;

procedure TdxTocGenerator.ExportInlinePictureRun(ARun: TdxInlinePictureRun);
var
  AText: string;
begin
  if FCurrentParagraphOutlineLevel < 0 then
    Exit;

  AText := TdxStringHelper.TrimStart(FParagraphText.ToString, [' ']);
  if AText <> '' then
  begin
    TargetPieceTable.InsertText(TargetPieceTable.DocumentEndLogPosition, AText);
    FParagraphText.Length := 0;
  end;
  TargetPieceTable.InsertInlinePicture(TargetPieceTable.DocumentEndLogPosition,
    ARun.Image.Clone(TargetPieceTable.DocumentModel.ImageCache));
end;

procedure TdxTocGenerator.ExportParagraphRun(ARun: TdxParagraphRun);
var
  AText: string;
  ATocParagraph: TdxParagraph;
  ABookmark: TdxBookmark;
begin
  if FCurrentParagraphOutlineLevel < 0 then
    Exit;

  AText := Trim(FParagraphText.ToString);
  ATocParagraph := TargetPieceTable.Paragraphs.Last;
  if (AText <> '') or (ATocParagraph.Length > 1) then
  begin
    ABookmark := ObtainTocBookmarkForHeaderOrOutline(TdxParagraph(ARun.Paragraph));
    AppendTocEntry(AText, ABookmark, FCurrentParagraphOutlineLevel);
  end;
end;

procedure TdxTocGenerator.AppendTocEntry(const AText: string; ABookmark: TdxBookmark; ALevel: Integer);
var
  ATocParagraph: TdxParagraph;
  ATextStartPosition, APageRefPosition: TdxDocumentLogPosition;
  AFieldCode: string;
  AHyperlinkInfo: TdxHyperlinkInfo;
  S: string;
begin
  S := AText;
  ATocParagraph := TargetPieceTable.Paragraphs.Last;
  if ShouldWritePageNumberIntoToc(ALevel) then
    S := ApplyPageNumber(S, ATocParagraph);

  ATextStartPosition := TargetPieceTable.DocumentEndLogPosition;
  TargetPieceTable.InsertText(ATextStartPosition, S);

  if ShouldWritePageNumberIntoToc(ALevel) then
  begin
    APageRefPosition := TargetPieceTable.DocumentEndLogPosition;
    AFieldCode := Format('PAGEREF %s', [ABookmark.Name]);
    TargetPieceTable.InsertText(APageRefPosition, AFieldCode);
    TargetPieceTable.CreateField(APageRefPosition, Length(AFieldCode));
  end;

  if Options.CreateHyperlinks then
  begin
    AHyperlinkInfo := TdxHyperlinkInfo.Create;
    AHyperlinkInfo.Anchor := ABookmark.Name;
    TargetPieceTable.CreateHyperlink(ATextStartPosition, Length(S), AHyperlinkInfo);
  end;
  ApplyTocStyleToParagraph(ALevel, ATocParagraph);
  ApplyTabsToParagraph(ATocParagraph);
  TargetPieceTable.InsertParagraph(TargetPieceTable.DocumentEndLogPosition);
end;

procedure TdxTocGenerator.ApplyTabsToParagraph(ATocParagraph: TdxParagraph);
var
  AOwnTabs, ANewTabs, ATocStyleTabs: TdxTabFormattingInfo;
  ACount, I: Integer;
begin
  AOwnTabs := ATocParagraph.GetOwnTabs;
  try
    ANewTabs := TdxTabFormattingInfo.Create;
    try
      if FNumberingListIndent >= 0 then
        AOwnTabs.Add(TdxTabInfo.Create(FNumberingListIndent + ATocParagraph.LeftIndent, TdxTabAlignmentType.Left));
      ATocStyleTabs := ATocParagraph.ParagraphStyle.GetTabs;
      try
        ACount := Math.Max(0, AOwnTabs.Count - ATocStyleTabs.Count);
        for I := 0 to ACount - 1 do
          ANewTabs.Add(AOwnTabs[I]);
        ATocParagraph.SetOwnTabs(ANewTabs);
      finally
        ATocStyleTabs.Free;
      end;
    finally
      ANewTabs.Free;
    end;
  finally
    AOwnTabs.Free;
  end;
end;

function TdxTocGenerator.ApplyPageNumber(const AText: string; ATarget: TdxParagraph): string;
var
  ASeparator: string;
  AOwnTabs: TdxTabFormattingInfo;
begin
  if Options.PageNumberSeparator.IsNullOrEmpty then
    ASeparator := #9
  else
    ASeparator := Options.PageNumberSeparator.Value;
  if ASeparator = #9 then
  begin
    AOwnTabs := ATarget.GetOwnTabs;
    try
      AOwnTabs.Add(TdxTabInfo.Create(FRightTabPosition, TdxTabAlignmentType.Right, TdxTabLeaderType.Dots, False, False));
      ATarget.SetOwnTabs(AOwnTabs);
    finally
      AOwnTabs.Free;
    end;
  end;
  Result := AText + ASeparator + FPageNumberPrefix;
end;

function TdxTocGenerator.ObtainTocBookmarkForHeaderOrOutline(AParagraph: TdxParagraph): TdxBookmark;
var
  AActualParagraphPosition: TdxDocumentLogPosition;
  ABookmark: TdxBookmark;
  APieceTable: TdxPieceTable;
  AName: string;
begin
  AActualParagraphPosition := CalculateActualParagraphPosition(AParagraph);
  ABookmark := LookupEntireParagraphBookmark(AParagraph, AActualParagraphPosition);
  if ABookmark <> nil then
    Exit(ABookmark);
  APieceTable := AParagraph.PieceTable;
  AName := GenerateTocBookmarkName(APieceTable);
  APieceTable.CreateBookmark(AActualParagraphPosition, AParagraph.LogPosition + AParagraph.Length - AActualParagraphPosition - 1, AName);
  Result := APieceTable.Bookmarks.FindByName(AName);
end;

function TdxTocGenerator.ObtainTocBookmarkForTocEntry(AEntry: TdxTocEntryField; ATocEntryFieldCodeFirstRunIndex: TdxRunIndex): TdxBookmark;
var
  AParagraph: TdxParagraphBase;
  APos: TdxDocumentLogPosition;
  I: TdxRunIndex;
  ALen: Integer;
  ABookmark: TdxBookmark;
  AName: string;
begin
  AParagraph := PieceTable.Runs[ATocEntryFieldCodeFirstRunIndex].Paragraph;
  APos := AParagraph.LogPosition;
  for I := AParagraph.FirstRunIndex to ATocEntryFieldCodeFirstRunIndex - 1 do
    Inc(APos, PieceTable.Runs[I].Length);
  Inc(APos, 4);
  ALen := Length(AEntry.Text);
  ABookmark := LookupExactBookmark(APos, ALen);
  if ABookmark <> nil then
    Exit(ABookmark);

  AName := GenerateTocBookmarkName(PieceTable);
  PieceTable.CreateBookmark(APos, ALen, AName);
  Result := PieceTable.Bookmarks.FindByName(AName);
end;

function TdxTocGenerator.GenerateTocBookmarkName(APieceTable: TdxPieceTable): string;
var
  AName: string;
begin
  Randomize;
  while True do
  begin
    AName := Format('_Toc%d', [Random(GetTickCount)]);
    if APieceTable.Bookmarks.FindByName(AName) = nil then
      Exit(AName);
  end;
end;

function TdxTocGenerator.LookupEntireParagraphBookmark(AParagraph: TdxParagraph; AActualParagraphPosition: TdxDocumentLogPosition): TdxBookmark;
var
  APos: TdxDocumentLogPosition;
  ABookmarks: TdxBookmarkCollection;
  ACount, I: Integer;
  ABookmark: TdxBookmark;
begin
  APos := AActualParagraphPosition;
  ABookmarks := AParagraph.PieceTable.Bookmarks;
  ACount := ABookmarks.Count;
  for I := 0 to ACount - 1 do
  begin
    ABookmark := ABookmarks[I];
    if (ABookmark.NormalizedStart = APos) and
        (ABookmark.Start + ABookmark.Length = AParagraph.LogPosition + AParagraph.Length - 1) then
      Exit(ABookmark);
  end;
  Result := nil;
end;

function TdxTocGenerator.LookupZeroLengthBookmarkAtLogPosition(APos: TdxDocumentLogPosition): TdxBookmark;
begin
  Result := LookupExactBookmark(APos, 0);
end;

function TdxTocGenerator.LookupExactBookmark(APos: TdxDocumentLogPosition; ALen: Integer): TdxBookmark;
var
  ABookmarks: TdxBookmarkCollection;
  ACount, I: Integer;
  ABookmark: TdxBookmark;
begin
  ABookmarks := PieceTable.Bookmarks;
  ACount := ABookmarks.Count;
  for I := 0 to ACount - 1 do
  begin
    ABookmark := ABookmarks[I];
    if (ABookmark.NormalizedStart = APos) and (ABookmark.Length = ALen) then
      Exit(ABookmark);
  end;
  Result := nil;
end;

procedure TdxTocGenerator.ApplyTocStyleToParagraph(ALevel: Integer; ATocParagraph: TdxParagraph);
var
  AStyleIndex: Integer;
begin
  AStyleIndex := CalculateTocParagraphStyleIndex(ALevel);
  if AStyleIndex >= 0 then
    ATocParagraph.ParagraphStyleIndex := AStyleIndex;
end;

function TdxTocGenerator.CalculateTocParagraphStyleIndex(ALevel: Integer): Integer;
var
  AIndex, ASourceIndex: Integer;
begin
  AIndex := TargetDocumentModel.ParagraphStyles.GetTocStyle(ALevel);
  if AIndex >= 0 then
    Exit(AIndex);
  ASourceIndex := DocumentModel.ParagraphStyles.GetTocStyle(ALevel);
  if ASourceIndex >= 0 then
    Exit(DocumentModel.ParagraphStyles[ASourceIndex].Copy(TargetDocumentModel));
  Result := TargetDocumentModel.ParagraphStyles.CreateTocStyle(ALevel);
end;

function TdxTocGenerator.ShouldWritePageNumberIntoToc(ALevel: Integer): Boolean;
begin
  Result := not Options.NoPageNumberLevels.Contains(ALevel);
end;

function TdxTocGenerator.GetActualOutlineLevel(AParagraph: TdxParagraph): Integer;
var
  AStyleOutlineLevel: Integer;
begin
  AStyleOutlineLevel := GetOutlineLevelByStyleName(AParagraph.ParagraphStyle.StyleName);
  if AStyleOutlineLevel >= 0 then
    Exit(AStyleOutlineLevel);

  if Options.UseParagraphOutlineLevel then
  begin
    AStyleOutlineLevel := AParagraph.ParagraphStyle.ParagraphProperties.OutlineLevel;
    if AStyleOutlineLevel > 0 then
      Exit(AStyleOutlineLevel);
    Exit(AParagraph.ParagraphProperties.OutlineLevel);
  end;

  Result := AParagraph.ParagraphStyle.ParagraphProperties.OutlineLevel;
end;

function TdxTocGenerator.GetOutlineLevelByStyleName(const AStyleName: string): Integer;
var
  AStyleLevelInfo: TList<TdxStyleOutlineLevelInfo>;
  ACount, I: Integer;
begin
  AStyleLevelInfo := Options.AdditionalStyles;
  ACount := AStyleLevelInfo.Count;
  for I := 0 to ACount - 1 do
  begin
    if CompareStr(AStyleLevelInfo[I].StyleName, AStyleName) = 0 then
      Exit(AStyleLevelInfo[I].OutlineLevel);
  end;
  Result := -1;
end;

function TdxTocGenerator.CalculateActualParagraphPosition(AParagraph: TdxParagraph): TdxDocumentLogPosition;
var
  APieceTable: TdxPieceTable;
  ACharacterIterator: TdxParagraphCharacterIterator;
  AFilter: TdxVisibleOnlyTextFilter;
begin
  APieceTable := TdxPieceTable(AParagraph.PieceTable);
  AFilter := TdxVisibleOnlyTextFilter.Create(APieceTable);
  try
    ACharacterIterator := TdxParagraphCharacterIterator.Create(AParagraph, APieceTable, AFilter);
    try
      Result := AParagraph.LogPosition;
      while IsWhiteSpace(ACharacterIterator.CurrentChar) and not ACharacterIterator.IsEnd do
      begin
        Inc(Result);
        ACharacterIterator.Next;
      end;
    finally
      ACharacterIterator.Free;
    end;
  finally
    AFilter.Free;
  end;
end;

function TdxTocGenerator.IsWhiteSpace(ACh: Char): Boolean;
begin
  Result := {$IFDEF DELPHIXE4}ACh.IsWhiteSpace or{$ELSE}TCharacter.IsWhiteSpace(ACh) or{$ENDIF}
    CharInSet(ACh, [#$A0,
      TdxCharacters.PageBreak, TdxCharacters.ColumnBreak, TdxCharacters.LineBreak]);
end;

function TdxTocGenerator.CanIncludeParagraphIntoToc(AParagraph: TdxParagraph): Boolean;
var
  ACount, I: Integer;
begin
  ACount := FTocFields.Count;
  for I := 0 to ACount - 1 do
  begin
    if IsParagraphInsideField(AParagraph, FTocFields[I]) then
      Exit(False);
  end;
  Result := True;
end;

function TdxTocGenerator.IsParagraphInsideField(AParagraph: TdxParagraph; AField: TdxField): Boolean;
begin
  Result := (AParagraph.FirstRunIndex >= AField.Code.Start) and
    (AParagraph.LastRunIndex <= AField.Result.&End);
end;

end.
