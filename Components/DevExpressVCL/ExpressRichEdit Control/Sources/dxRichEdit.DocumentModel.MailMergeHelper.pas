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

unit dxRichEdit.DocumentModel.MailMergeHelper;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, Classes, Generics.Defaults, Generics.Collections,
  dxRichEdit.Types,
  dxRichEdit.Utils.Types,
  dxRichEdit.Utils.UriStreamService,
  dxRichEdit.Utils.ProgressIndication,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.Section,
  dxRichEdit.DocumentModel.SectionFormatting,
  dxRichEdit.DocumentModel.Fields.Core,
  dxRichEdit.DocumentModel.Fields,
  dxRichEdit.DocumentModel.MailMerge,
  dxRichEdit.DocumentModel.FieldDataService;

type
  { TdxMailMergeHelperBase }

  TdxMailMergeHelperBase = class abstract
  strict private
    FDocumentServerOwner: IdxInnerRichEditDocumentServerOwner;
    FOptions: IdxMailMergeOptions;
    FSourceModel: TdxDocumentModel;
    FProgressIndication: TdxProgressIndication;
  protected
    function GetOperationDescription: string; virtual; abstract;
    function GetSourceModel: TdxDocumentModel; virtual;
    function GetOptions: IdxMailMergeOptions; virtual;
    procedure ExecuteMailMergeCore(ATargetModel: TdxDocumentModel; const AFieldDataService: IdxMailMergeDataService); virtual;
    procedure PrepareTargetModel(ATargetModel: TdxDocumentModel);
    function MailMergeRecord(ATargetModel: TdxDocumentModel; const AFieldDataService: IdxMailMergeDataService; const AFieldCalculatorService: IdxFieldCalculatorService; const AUriStreamService: IdxUriStreamService; AFirstRecord: Boolean): Boolean; virtual;
    function MailMergeRecordCore(ATargetModel: TdxDocumentModel; AIntermediateResult: TdxDocumentModel; const AFieldDataService: IdxMailMergeDataService; AFirstRecord: Boolean): Boolean; virtual;
    procedure AppendRecordResult(ATargetModel: TdxDocumentModel; AIntermediateResult: TdxDocumentModel; const AFieldDataService: IdxMailMergeDataService; AFirstRecord: Boolean); virtual;
    function PrepareRecordResult(ATargetModel: TdxDocumentModel; AIntermediateResult: TdxDocumentModel; const AFieldDataService: IdxMailMergeDataService): Boolean;
    procedure PrepareRecordData(ARecordModel: TdxDocumentModel); virtual;
    procedure PrepareRecordResultCore(AIntermediateResult: TdxDocumentModel; const AFieldDataService: IdxMailMergeDataService); virtual;
    procedure BeforeRecordInserted(ATargetModel: TdxDocumentModel; AFirstRecord: Boolean); virtual;
    procedure AfterRecordInserted(AIntermediateResult: TdxDocumentModel; ATargetModel: TdxDocumentModel); virtual;
    function NewSectionInserted: Boolean; virtual;
    procedure CopyFromIntermediateResult(AIntermediateResult: TdxPieceTable; ATarget: TdxPieceTable);
    function CalculateTargetPosition(ATarget: TdxPieceTable): TdxDocumentModelPosition; virtual;
    procedure ApplyLastSectionSize(ATargetModel: TdxDocumentModel; ASourceModel: TdxDocumentModel);
    function RaiseMailMergeStarted(const AFieldDataService: IdxMailMergeDataService; ATargetModel: TdxDocumentModel): Boolean; virtual; abstract;
    function RaiseMailMergeRecordStarted(const AFieldDataService: IdxMailMergeDataService; ATargetModel: TdxDocumentModel; ARecordModel: TdxDocumentModel): Boolean; virtual;
    function RaiseMailMergeRecordFinished(const AFieldDataService: IdxMailMergeDataService; ATargetModel: TdxDocumentModel; ARecordModel: TdxDocumentModel): Boolean; virtual;
    procedure RaiseMailMergeFinished(const AFieldDataService: IdxMailMergeDataService; ATargetModel: TdxDocumentModel); virtual;
    procedure CopyTemplateStyles(ATargetModel: TdxDocumentModel; ASourceModel: TdxDocumentModel); virtual;
    procedure ReplaceServices(ADocumentModel: TdxDocumentModel; const AMailMergeDataService: IdxMailMergeDataService; const AFieldCalculatorService: IdxFieldCalculatorService; const AUriStreamService: IdxUriStreamService); virtual;
    procedure UpdateFields(ADocumentModel: TdxDocumentModel); virtual;
    procedure UpdateFieldsCore(ADocumentModel: TdxDocumentModel); overload; virtual;
    procedure UpdateFieldsCore(APieceTable: TdxPieceTable); overload; virtual;
    function AllowFixLastParagraph(APieceTable: TdxPieceTable): Boolean; virtual;
    procedure UpdateSectionHeadersFooters(AIntermediate: TdxSection; ATarget: TdxSection);
    procedure UpdateSectionHeaderFooter(ASource: TdxSectionHeaderFooterBase; ATarget: TdxSection; AOwnerList: TdxSectionHeadersFootersBase; ATargetProvider: TdxFunc<TdxSection, TdxPieceTable>);

    property DocumentServerOwner: IdxInnerRichEditDocumentServerOwner read FDocumentServerOwner;
    property SourceModel: TdxDocumentModel read GetSourceModel;
    property ProgressIndication: TdxProgressIndication read FProgressIndication;
    property Options: IdxMailMergeOptions read GetOptions;
    property OperationDescription: string read GetOperationDescription;
  public
    constructor Create(const ADocumentServerOwner: IdxInnerRichEditDocumentServerOwner;
      ASourceModel: TdxDocumentModel; const AOptions: IdxMailMergeOptions; const AProgressIndication: IdxProgressIndicationService = nil);
    destructor Destroy; override;
    procedure ExecuteMailMerge(ATargetModel: TdxDocumentModel); virtual;
  end;

  { TdxMailMergeHelper }

  TdxMailMergeHelper = class(TdxMailMergeHelperBase)
  strict private
    FStartedArgs: TdxMailMergeStartedEventArgs;
    function GetMergeMode: TdxMergeMode;
  protected
    function GetOperationDescription: string; override;
    function NewSectionInserted: Boolean; override;
    procedure BeforeRecordInserted(ATargetModel: TdxDocumentModel; AFirstRecord: Boolean); override;
    function AllowFixLastParagraph(APieceTable: TdxPieceTable): Boolean; override;
    function RaiseMailMergeStarted(const AFieldDataService: IdxMailMergeDataService; ATargetModel: TdxDocumentModel): Boolean; override;

    property MergeMode: TdxMergeMode read GetMergeMode;
  public
    destructor Destroy; override;
  end;

implementation

uses
  RTLConsts,
  dxCoreClasses,
  dxRichEdit.Utils.Exceptions,
  dxRichEdit.DocumentModel.CopyManager.Simple,
  dxRichEdit.DocumentModel.CopyManager,
  dxRichEdit.DocumentModel.Styles,
  dxRichEdit.DocumentModel.TableStyles,
  dxRichEdit.DocumentModel.Commands,
  dxRichEdit.Options;

{ TdxMailMergeHelperBase }

constructor TdxMailMergeHelperBase.Create(const ADocumentServerOwner: IdxInnerRichEditDocumentServerOwner;
  ASourceModel: TdxDocumentModel; const AOptions: IdxMailMergeOptions;
  const AProgressIndication: IdxProgressIndicationService = nil);
begin
  Assert(ADocumentServerOwner <> nil);
  Assert(ASourceModel <> nil);
  Assert(AOptions <> nil);
  inherited Create;
  FSourceModel := ASourceModel;
  FOptions := AOptions;
  FDocumentServerOwner := ADocumentServerOwner;

  FProgressIndication := TdxProgressIndication.Create(ASourceModel);
end;

destructor TdxMailMergeHelperBase.Destroy;
begin
  FProgressIndication.Free;
  inherited Destroy;
end;

function TdxMailMergeHelperBase.GetSourceModel: TdxDocumentModel;
begin
  Result := FSourceModel;
end;

function TdxMailMergeHelperBase.GetOptions: IdxMailMergeOptions;
begin
  Result := FOptions;
end;

procedure TdxMailMergeHelperBase.ExecuteMailMerge(ATargetModel: TdxDocumentModel);
var
  ASourceFieldDataService, AFieldDataService: IdxMailMergeDataService;
begin
  ASourceFieldDataService := SourceModel.GetService<IdxMailMergeDataService>;
  if not ASourceFieldDataService.BoundMode and (Options.DataSource = nil) then
    Exit;
  AFieldDataService := ASourceFieldDataService.StartMailMerge(Options);
  if AFieldDataService = nil then
    Exit;
  try
    ExecuteMailMergeCore(ATargetModel, AFieldDataService);
  finally
    AFieldDataService.EndMailMerge(AFieldDataService);
  end;
end;

procedure TdxMailMergeHelperBase.ExecuteMailMergeCore(ATargetModel: TdxDocumentModel; const AFieldDataService: IdxMailMergeDataService);
var
  ALastRecordIndex: Integer;
  AFieldCalculatorService: IdxFieldCalculatorService;
  AUriStreamService: IdxUriStreamService;
  AFirstRecord: Boolean;
  ASelection: TdxSelection;
begin
  PrepareTargetModel(ATargetModel);

  if not RaiseMailMergeStarted(AFieldDataService, ATargetModel) then
    Exit;

  ALastRecordIndex := AFieldDataService.GetRecordCount;
  ProgressIndication.&Begin(OperationDescription, 0, ALastRecordIndex, 0);

  AFieldCalculatorService := SourceModel.GetService<IdxFieldCalculatorService>;
  AUriStreamService := SourceModel.GetService<IdxUriStreamService>;

  AFirstRecord := True;
  while True do
  begin
    if not MailMergeRecord(ATargetModel, AFieldDataService, AFieldCalculatorService, AUriStreamService, AFirstRecord) then
      Break;
    AFirstRecord := False;

    if not AFieldDataService.MoveNextRecord then
      Break;
    ProgressIndication.SetProgress(AFieldDataService.GetActiveRecord);
  end;
  ATargetModel.MainPieceTable.FixLastParagraph;

  ASelection := ATargetModel.Selection;
  ASelection.Start := 0;
  ASelection.&End := 0;

  ProgressIndication.&End;
  RaiseMailMergeFinished(AFieldDataService, ATargetModel);
end;

procedure TdxMailMergeHelperBase.PrepareTargetModel(ATargetModel: TdxDocumentModel);
begin
  ATargetModel.DocumentProperties.CopyFrom(SourceModel.DocumentProperties.Info);
  TdxDocumentModelCopyCommand.ReplaceDefaultProperties(ATargetModel, SourceModel);
  TdxDocumentModelCopyCommand.ReplaceDefaultStyles(ATargetModel, SourceModel);
  if Options.CopyTemplateStyles then
    CopyTemplateStyles(ATargetModel, SourceModel);
end;

function TdxMailMergeHelperBase.MailMergeRecord(ATargetModel: TdxDocumentModel; const AFieldDataService: IdxMailMergeDataService; const AFieldCalculatorService: IdxFieldCalculatorService; const AUriStreamService: IdxUriStreamService; AFirstRecord: Boolean): Boolean;
var
  AIntermediateResult: TdxDocumentModel;
  AEventRouter: TdxCalculateDocumentVariableEventRouter;
  AMainPieceTable: TdxPieceTable;
  AStartLogPosition, AEndLogPosition: TdxDocumentLogPosition;
  AOptions: TdxDocumentModelCopyOptions;
  ACopyCommand: TdxDocumentModelCopyCommand;
begin
  AIntermediateResult := ATargetModel.CreateNew;
  try
    AIntermediateResult.ModelForExport := True;
    AIntermediateResult.IntermediateModel := True;
    AIntermediateResult.FieldOptions.CopyFrom(SourceModel.FieldOptions);
    AEventRouter := TdxCalculateDocumentVariableEventRouter.Create(ATargetModel);
    try
      AIntermediateResult.CalculateDocumentVariable.Add(AEventRouter.OnCalculateDocumentVariable);
      AIntermediateResult.BeginSetContent;
      try
        AMainPieceTable := SourceModel.MainPieceTable;
        AStartLogPosition := AMainPieceTable.DocumentStartLogPosition;
        AEndLogPosition := AMainPieceTable.DocumentEndLogPosition;
        AOptions := TdxDocumentModelCopyOptions.Create(AStartLogPosition, AEndLogPosition - AStartLogPosition + 1);
        try
          AOptions.CopyDocumentVariables := True;
          ACopyCommand := SourceModel.CreateDocumentModelCopyCommand(AMainPieceTable, AIntermediateResult, AOptions) as TdxDocumentModelCopyCommand;
          try
            ACopyCommand.FixLastParagraph := AllowFixLastParagraph(AMainPieceTable);
            ACopyCommand.SuppressFieldsUpdate := True;
            ACopyCommand.Execute;
          finally
            ACopyCommand.Free;
          end;
        finally
          AOptions.Free;
        end;
        if not AFieldDataService.BoundMode then
          TdxRichEditExceptions.ThrowInternalException;
        AIntermediateResult.DocumentSaveOptions.CurrentFileName := SourceModel.DocumentSaveOptions.CurrentFileName;
        ReplaceServices(AIntermediateResult, AFieldDataService, AFieldCalculatorService, AUriStreamService);
      finally
        AIntermediateResult.EndSetContent(TdxDocumentModelChangeType.LoadNewDocument, False, nil);
        Result := MailMergeRecordCore(ATargetModel, AIntermediateResult, AFieldDataService, AFirstRecord);
        AIntermediateResult.CalculateDocumentVariable.Remove(AEventRouter.OnCalculateDocumentVariable);
      end;
    finally
      AEventRouter.Free;
    end;
  finally
    AIntermediateResult.Free;
  end;
end;

function TdxMailMergeHelperBase.MailMergeRecordCore(ATargetModel: TdxDocumentModel; AIntermediateResult: TdxDocumentModel; const AFieldDataService: IdxMailMergeDataService; AFirstRecord: Boolean): Boolean;
var
  AResult: Boolean;
begin
  AResult := PrepareRecordResult(ATargetModel, AIntermediateResult, AFieldDataService);
  if AResult then
    AppendRecordResult(ATargetModel, AIntermediateResult, AFieldDataService, AFirstRecord);
  Result := AResult;
end;

procedure TdxMailMergeHelperBase.AppendRecordResult(ATargetModel: TdxDocumentModel; AIntermediateResult: TdxDocumentModel; const AFieldDataService: IdxMailMergeDataService; AFirstRecord: Boolean);
begin
  BeforeRecordInserted(ATargetModel, AFirstRecord);
  CopyFromIntermediateResult(AIntermediateResult.MainPieceTable, ATargetModel.MainPieceTable);
  AfterRecordInserted(AIntermediateResult, ATargetModel);
end;

function TdxMailMergeHelperBase.PrepareRecordResult(ATargetModel: TdxDocumentModel; AIntermediateResult: TdxDocumentModel; const AFieldDataService: IdxMailMergeDataService): Boolean;
begin
  PrepareRecordData(AIntermediateResult);
  if not RaiseMailMergeRecordStarted(AFieldDataService, ATargetModel, AIntermediateResult) then
    Exit(False);

  AIntermediateResult.MailMergeOptions.CustomSeparators.Assign(SourceModel.MailMergeOptions.CustomSeparators);

  PrepareRecordResultCore(AIntermediateResult, AFieldDataService);
  if not RaiseMailMergeRecordFinished(AFieldDataService, ATargetModel, AIntermediateResult) then
    Exit(False);
  Result := True;
end;

procedure TdxMailMergeHelperBase.PrepareRecordData(ARecordModel: TdxDocumentModel);
begin
end;

procedure TdxMailMergeHelperBase.PrepareRecordResultCore(AIntermediateResult: TdxDocumentModel; const AFieldDataService: IdxMailMergeDataService);
begin
  UpdateFields(AIntermediateResult);
end;

procedure TdxMailMergeHelperBase.BeforeRecordInserted(ATargetModel: TdxDocumentModel; AFirstRecord: Boolean);
begin
end;

procedure TdxMailMergeHelperBase.AfterRecordInserted(AIntermediateResult: TdxDocumentModel; ATargetModel: TdxDocumentModel);
begin
  if NewSectionInserted and not Options.HeaderFooterLinkToPrevious then
    UpdateSectionHeadersFooters(AIntermediateResult.Sections.Last, ATargetModel.Sections.Last);
end;

function TdxMailMergeHelperBase.NewSectionInserted: Boolean;
begin
  Result := False;
end;

procedure TdxMailMergeHelperBase.CopyFromIntermediateResult(AIntermediateResult: TdxPieceTable; ATarget: TdxPieceTable);
var
  ACopyManager: TdxDocumentModelCopyManager;
  AOperation: TdxCopySectionOperation;
begin
  ACopyManager := TdxDocumentModelCopyManager.Create(AIntermediateResult, ATarget, TdxParagraphNumerationCopyOptions.CopyAlways);
  try
    ACopyManager.TargetPosition.CopyFrom(CalculateTargetPosition(ATarget));
    AOperation := AIntermediateResult.DocumentModel.CreateCopySectionOperation(ACopyManager) as TdxCopySectionOperation;
    try
      AOperation.FixLastParagraph := True;
      AOperation.Execute(AIntermediateResult.DocumentStartLogPosition,
        AIntermediateResult.DocumentEndLogPosition - AIntermediateResult.DocumentStartLogPosition + 1, False);
      ApplyLastSectionSize(ATarget.DocumentModel, AIntermediateResult.DocumentModel);
    finally
      AOperation.Free;
    end;
  finally
    ACopyManager.Free;
  end;
end;

function TdxMailMergeHelperBase.CalculateTargetPosition(ATarget: TdxPieceTable): TdxDocumentModelPosition;
begin
  Result := TdxDocumentModelPosition.FromDocumentEnd(ATarget);
end;

procedure TdxMailMergeHelperBase.ApplyLastSectionSize(ATargetModel: TdxDocumentModel; ASourceModel: TdxDocumentModel);
begin
  ATargetModel.Sections.Last.CopyFromCore(ASourceModel.Sections.Last);
end;

function TdxMailMergeHelperBase.RaiseMailMergeRecordStarted(const AFieldDataService: IdxMailMergeDataService; ATargetModel: TdxDocumentModel; ARecordModel: TdxDocumentModel): Boolean;
var
  ARecordStartedArgs: TdxMailMergeRecordStartedEventArgs;
begin
  ARecordStartedArgs := TdxMailMergeRecordStartedEventArgs.Create(DocumentServerOwner, ATargetModel, ARecordModel);
  try
    Result := SourceModel.RaiseMailMergeRecordStarted(ARecordStartedArgs);
  finally
    ARecordStartedArgs.Free;
  end;
end;

function TdxMailMergeHelperBase.RaiseMailMergeRecordFinished(const AFieldDataService: IdxMailMergeDataService; ATargetModel: TdxDocumentModel; ARecordModel: TdxDocumentModel): Boolean;
var
  ARecordFinishedArgs: TdxMailMergeRecordFinishedEventArgs;
begin
  ARecordFinishedArgs := TdxMailMergeRecordFinishedEventArgs.Create(DocumentServerOwner, ATargetModel, ARecordModel);
  try
    Result := SourceModel.RaiseMailMergeRecordFinished(ARecordFinishedArgs);
  finally
    ARecordFinishedArgs.Free;
  end;
end;

procedure TdxMailMergeHelperBase.RaiseMailMergeFinished(const AFieldDataService: IdxMailMergeDataService; ATargetModel: TdxDocumentModel);
var
  AFinishedArgs: TdxMailMergeFinishedEventArgs;
begin
  AFinishedArgs := TdxMailMergeFinishedEventArgs.Create(DocumentServerOwner, ATargetModel);
  try
    SourceModel.RaiseMailMergeFinished(AFinishedArgs);
  finally
    AFinishedArgs.Free;
  end;
end;

procedure TdxMailMergeHelperBase.CopyTemplateStyles(ATargetModel: TdxDocumentModel; ASourceModel: TdxDocumentModel);
var
  ACharacterStyles: TdxCharacterStyleCollection;
  ACount, I: Integer;
  AParagraphStyles: TdxParagraphStyleCollection;
  ATableStyles: TdxTableStyleCollection;
begin
  if ATargetModel.DocumentCapabilities.CharacterStyleAllowed then
  begin
    ACharacterStyles := ASourceModel.CharacterStyles;
    ACount := ACharacterStyles.Count;
    for I := 0 to ACount - 1 do
      ACharacterStyles[I].Copy(ATargetModel);
  end;
  if ATargetModel.DocumentCapabilities.ParagraphStyleAllowed then
  begin
    AParagraphStyles := ASourceModel.ParagraphStyles;
    ACount := AParagraphStyles.Count;
    for I := 0 to ACount - 1 do
      AParagraphStyles[I].Copy(ATargetModel);
  end;
  if ATargetModel.DocumentCapabilities.TableStyleAllowed then
  begin
    ATableStyles := ASourceModel.TableStyles;
    ACount := ATableStyles.Count;
    for I := 0 to ACount - 1 do
      ATableStyles[I].Copy(ATargetModel);
  end;
end;

procedure TdxMailMergeHelperBase.ReplaceServices(ADocumentModel: TdxDocumentModel;
  const AMailMergeDataService: IdxMailMergeDataService;
  const AFieldCalculatorService: IdxFieldCalculatorService;
  const AUriStreamService: IdxUriStreamService);
begin
  ADocumentModel.ReplaceService<IdxFieldDataService>(AMailMergeDataService);
  ADocumentModel.ReplaceService<IdxMailMergeDataService>(AMailMergeDataService);
  ADocumentModel.ReplaceService(AFieldCalculatorService);
  ADocumentModel.ReplaceService(AUriStreamService);
end;

procedure TdxMailMergeHelperBase.UpdateFields(ADocumentModel: TdxDocumentModel);
begin
  ADocumentModel.BeginUpdate;
  try
    UpdateFieldsCore(ADocumentModel);
  finally
    ADocumentModel.EndUpdate;
  end;
end;

procedure TdxMailMergeHelperBase.UpdateFieldsCore(ADocumentModel: TdxDocumentModel);
var
  APieceTables: TdxFastList;
  ACount, I: Integer;
begin
  APieceTables := ADocumentModel.GetPieceTables(False);
  try
    ACount := APieceTables.Count;
    for I := 0 to ACount - 1 do
      UpdateFieldsCore(TdxPieceTable(APieceTables[I]));
  finally
    APieceTables.Free;
  end;
end;

procedure TdxMailMergeHelperBase.UpdateFieldsCore(APieceTable: TdxPieceTable);
var
  AFields: TdxFieldCollectionBase;
  AUpdater: TdxMailMergeFieldUpdater;
  ACount, I: Integer;
begin
  AFields := APieceTable.Fields;
  AUpdater := TdxMailMergeFieldUpdater.Create(APieceTable);
  try
    ACount := AFields.Count;
    for I := 0 to ACount - 1 do
      AUpdater.PrepareFieldUpdate(AFields[I], TdxUpdateFieldOperationType.Normal);
    AUpdater.UpdateFields(TdxUpdateFieldOperationType.Normal);
  finally
    AUpdater.Free;
  end;
end;

function TdxMailMergeHelperBase.AllowFixLastParagraph(APieceTable: TdxPieceTable): Boolean;
begin
  Result := True;
end;

procedure TdxMailMergeHelperBase.UpdateSectionHeadersFooters(AIntermediate: TdxSection; ATarget: TdxSection);
begin
  UpdateSectionHeaderFooter(AIntermediate.InnerOddPageHeader, ATarget, ATarget.Headers,
    function(const ASection: TdxSection): TdxPieceTable
    begin
      Result := TdxPieceTable(ASection.InnerOddPageHeader.PieceTable);
    end);
  UpdateSectionHeaderFooter(AIntermediate.InnerEvenPageHeader, ATarget, ATarget.Headers,
    function(const ASection: TdxSection): TdxPieceTable
    begin
      Result := TdxPieceTable(ASection.InnerEvenPageHeader.PieceTable);
    end);

  UpdateSectionHeaderFooter(AIntermediate.InnerFirstPageHeader, ATarget, ATarget.Headers,
    function(const ASection: TdxSection): TdxPieceTable
    begin
      Result := TdxPieceTable(ASection.InnerFirstPageHeader.PieceTable);
    end);

  UpdateSectionHeaderFooter(AIntermediate.InnerOddPageFooter, ATarget, ATarget.Footers,
    function(const ASection: TdxSection): TdxPieceTable
    begin
      Result := TdxPieceTable(ASection.InnerOddPageFooter.PieceTable);
    end);

  UpdateSectionHeaderFooter(AIntermediate.InnerEvenPageFooter, ATarget, ATarget.Footers,
    function(const ASection: TdxSection): TdxPieceTable
    begin
      Result := TdxPieceTable(ASection.InnerEvenPageFooter.PieceTable);
    end);

  UpdateSectionHeaderFooter(AIntermediate.InnerFirstPageFooter, ATarget, ATarget.Footers,
    function(const ASection: TdxSection): TdxPieceTable
    begin
      Result := TdxPieceTable(ASection.InnerFirstPageFooter.PieceTable);
    end);
end;

procedure TdxMailMergeHelperBase.UpdateSectionHeaderFooter(ASource: TdxSectionHeaderFooterBase;
  ATarget: TdxSection;
  AOwnerList: TdxSectionHeadersFootersBase;
  ATargetProvider: TdxFunc<TdxSection, TdxPieceTable>);
begin
  if ASource = nil then
    Exit;
  AOwnerList.Add(ASource.&Type);
  CopyFromIntermediateResult(TdxPieceTable(ASource.PieceTable), ATargetProvider(ATarget));
end;

{ TdxMailMergeHelper }

destructor TdxMailMergeHelper.Destroy;
begin
  FreeAndNil(FStartedArgs);
  inherited Destroy;
end;

function TdxMailMergeHelper.GetMergeMode: TdxMergeMode;
begin
  Result := Options.MergeMode;
end;

function TdxMailMergeHelper.GetOperationDescription: string;
begin
  Result := FStartedArgs.OperationDescription;
end;

function TdxMailMergeHelper.NewSectionInserted: Boolean;
begin
  Result := MergeMode = TdxMergeMode.NewSection;
end;

procedure TdxMailMergeHelper.BeforeRecordInserted(ATargetModel: TdxDocumentModel; AFirstRecord: Boolean);
begin
  if (MergeMode = TdxMergeMode.NewSection) and not AFirstRecord then
  begin
    ATargetModel.Sections.Last.GeneralSettings.StartType := TdxSectionStartType.NextPage;
    ATargetModel.InsertSection(ATargetModel.MainPieceTable.DocumentEndLogPosition, True);
  end;
end;

function TdxMailMergeHelper.AllowFixLastParagraph(APieceTable: TdxPieceTable): Boolean;
var
  AParagraphs: TdxParagraphCollection;
  ALastParagraph: TdxParagraph;
  AIsTableBeforeParagraph: Boolean;
begin
  AParagraphs := APieceTable.Paragraphs;
  if (MergeMode <> TdxMergeMode.JoinTables) or (AParagraphs.Count = 1) then
    Exit(False);

  ALastParagraph := AParagraphs.Last;
  AIsTableBeforeParagraph := AParagraphs[ALastParagraph.Index - 1].GetCell <> nil;
  Result := ALastParagraph.IsEmpty and AIsTableBeforeParagraph;
end;

function TdxMailMergeHelper.RaiseMailMergeStarted(const AFieldDataService: IdxMailMergeDataService; ATargetModel: TdxDocumentModel): Boolean;
begin
  FStartedArgs.Free;
  FStartedArgs := TdxMailMergeStartedEventArgs.Create(DocumentServerOwner, ATargetModel);
  Result := SourceModel.RaiseMailMergeStarted(FStartedArgs);
end;

end.
