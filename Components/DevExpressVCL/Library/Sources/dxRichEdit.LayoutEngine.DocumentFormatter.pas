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

unit dxRichEdit.LayoutEngine.DocumentFormatter;

interface

{$I cxVer.inc}
{$I dxRichEditControl.inc}

uses
  Types, SysUtils, Classes, Generics.Defaults, Generics.Collections, dxCore, dxCoreClasses,
  dxRichEdit.Types,
  dxRichEdit.Utils.Types,
  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.LayoutEngine.BoxMeasurer,
  dxRichEdit.LayoutEngine.Formatter,
  dxRichEdit.DocumentLayout,
  dxRichEdit.Platform.Font,
  dxRichEdit.DocumentModel.UnitToLayoutUnitConverter,
  dxRichEdit.DocumentModel.Section,
  dxRichEdit.Control.HitTest,
  dxRichEdit.DocumentModel.Boxes.Simple,
  dxRichEdit.DocumentModel.Boxes,
  dxRichEdit.DocumentModel.Core;

type
  TdxDocumentFormatterState = class;
  TdxContinueFromParagraph = class;
  TdxBeginParagraphFormattingFromTheMiddleOfParagraphAndStartOfRow = class;

  TdxDocumentFormatterStateType = (
    BeginParagraphFormatting,
    BeginParagraphFormattingFromTheMiddleOfParagraphAndStartOfRow,
    ContinueParagraphFormatting,
    EndParagraphFormatting,
    EndHiddenParagraphFormatting,
    ContinueFromParagraph,
    Final);

  { TdxDocumentFormattingController }

  TdxDocumentFormattingController = class
  private
    FDocumentLayout: TdxDocumentLayout;
    FPieceTable: TdxPieceTable;
    FPageController: TdxPageController;
    FRowsController: TdxRowsController;
    FPageAreaController: TdxPageAreaController;
    FColumnController: TdxColumnController;
    FFloatingObjectsLayout: TdxFloatingObjectsLayout;
    FParagraphFramesLayout: TdxParagraphFramesLayout;
    FPageCountSinceLastResetSecondaryFormatting: Integer;
    FOnPageCountChanged: TdxEventHandler;
    FOnResetSecondaryFormattingForPage: TdxResetSecondaryFormattingForPageEventHandler;
    FOnPageFormattingStarted: TdxPageFormattingCompleteEventHandler;
    FOnPageFormattingComplete: TdxPageFormattingCompleteEventHandler;
    function GetDocumentModel: TdxDocumentModel;
    function GetPageCount: Integer;
    procedure SetColumnController(const Value: TdxColumnController);
  protected
    procedure ClearPages; virtual;
    procedure ClearPagesFrom(APageIndex: Integer); virtual;
    function CreatePageController: TdxPageController; virtual; abstract;
    function CreatePageAreaController: TdxPageAreaController; virtual;
    function CreateColumnController: TdxColumnController; virtual; abstract;
    function CreateRowController: TdxRowsController; virtual; abstract;
  public
    constructor Create(ADocumentLayout: TdxDocumentLayout; APieceTable: TdxPieceTable;
      AFloatingObjectsLayout: TdxFloatingObjectsLayout; AParagraphFramesLayout: TdxParagraphFramesLayout);
    destructor Destroy; override;

    property PageFormattingStarted: TdxPageFormattingCompleteEventHandler read FOnPageFormattingStarted;
    procedure RaisePageFormattingStarted(APage: TdxPage); virtual;

    property PageFormattingComplete: TdxPageFormattingCompleteEventHandler read FOnPageFormattingComplete;

    procedure RaisePageFormattingComplete(APage: TdxPage; ADocumentFormattingComplete: Boolean); virtual;
    procedure RaisePageCountChanged; virtual;
    procedure RaiseResetSecondaryFormattingForPage(APage: TdxPage; APageIndex: Integer); virtual;

    property PageCountChanged: TdxEventHandler read FOnPageCountChanged;
    property ResetSecondaryFormattingForPage: TdxResetSecondaryFormattingForPageEventHandler read FOnResetSecondaryFormattingForPage;

    procedure Reset(AKeepFloatingObjects: Boolean); virtual;
    function ResetFrom(var AFrom: TdxDocumentModelPosition; AKeepFloatingObjects: Boolean): TdxDocumentModelPosition; virtual;
    procedure ResetFromTheStartOfRowAtCurrentPage(const AFrom: TdxDocumentModelPosition; AKeepFloatingObjects, AForceRestart: Boolean); virtual;
    procedure RestartFormattingFromTheStartOfRowAtCurrentPage(AClearContentResult: TdxClearInvalidatedContentResult;
      const AFrom: TdxDocumentModelPosition); virtual;
    function EnsurePositionVisible(var APos: TdxDocumentModelPosition): TdxDocumentModelPosition; virtual;
    function EnsureParagraphVisible(var APos: TdxDocumentModelPosition): TdxDocumentModelPosition; virtual;
    function EnsureRunVisible(var APos: TdxDocumentModelPosition): TdxDocumentModelPosition; virtual;
    function PositionSectionBreakAfterParagraphBreak(AParagraph: TdxParagraph; ARunIndex: TdxRunIndex): Boolean;
    function CreateFormatterPosition(const AFrom: TdxDocumentModelPosition; ARoundToParagraphBoundary: Boolean): TdxFormatterPosition;
    function ClearInvalidatedContent(const AFrom: TdxDocumentModelPosition;
      ARoundToParagraphBoundary, AKeepFloatingObjects: Boolean): TdxClearInvalidatedContentResult; virtual;
    function ClearInvalidatedContentFromTheStartOfRowAtCurrentPage(const AFrom: TdxDocumentModelPosition;
      ARoundToParagraphBoundary, AKeepFloatingObjects: Boolean): TdxClearInvalidatedContentResult; virtual;
    function CleanupEmptyBoxesForCurrentPage(const AFrom: TdxDocumentModelPosition): TdxDocumentModelPosition; virtual;
    function CleanupEmptyBoxes(const AFrom: TdxDocumentModelPosition): TdxDocumentModelPosition; virtual;
    procedure SetPageCount(APageCount: Integer; APrimaryFormattingFinished: Boolean);
    procedure OnBeginNextSectionFormatting(ASender: TObject; E: TdxEventArgs); virtual;
    procedure BeginNextSectionFormatting(ASectionIndex: TdxSectionIndex); virtual;
    procedure RestartFormattingFromTheStartOfSection; virtual;
    procedure RestartFormattingFromTheMiddleOfSection; virtual;
    procedure OnPageFormattingStarted(ASender: TObject; E: TdxPageFormattingCompleteEventArgs); virtual;
    procedure OnPageFormattingComplete(ASender: TObject; E: TdxPageFormattingCompleteEventArgs);
    procedure OnPageCountChanged(ASender: TObject; E: TdxEventArgs); virtual;
    procedure NotifyDocumentFormattingComplete; virtual;
    procedure OnParagraphFramesLayoutChanged(ASender: TObject; E: TdxEventArgs);
    procedure OnParagraphFormattingComplete(AParagraphIndex: TdxParagraphIndex); virtual;
    procedure OnFloatingObjectsLayoutChanged(ASender: TObject; E: TdxEventArgs);

    property DocumentLayout: TdxDocumentLayout read FDocumentLayout;
    property DocumentModel: TdxDocumentModel read GetDocumentModel;
    property PieceTable: TdxPieceTable read FPieceTable;
    property PageController: TdxPageController read FPageController;
    property PageAreaController: TdxPageAreaController read FPageAreaController;
    property ColumnController: TdxColumnController read FColumnController write SetColumnController;
    property RowsController: TdxRowsController read FRowsController;
    property PageCount: Integer read GetPageCount;
    property FloatingObjectsLayout: TdxFloatingObjectsLayout read FFloatingObjectsLayout;
    property ParagraphFramesLayout: TdxParagraphFramesLayout read FParagraphFramesLayout;
  end;

  { TdxDocumentFormatter }

  TdxDocumentFormatter = class
  private
    FParagraphIndex: TdxParagraphIndex;
    FController: TdxDocumentFormattingController;
    FParagraphFormatter: TdxParagraphBoxFormatter;
    FState: TdxDocumentFormatterState;
    FStates: array[TdxDocumentFormatterStateType] of TdxDocumentFormatterState;
    function GetMeasurer: TdxBoxMeasurer;
    procedure SetState(Value: TdxDocumentFormatterState);
  protected
    procedure CreateStates; virtual;
    procedure DestroyStates; virtual;
    procedure Init;
    function CreateParagraphBoxFormatter(AController: TdxDocumentFormattingController): TdxParagraphBoxFormatter; virtual;

    procedure OnParagraphFormattingComplete;
    procedure ResetFromTheStartOfRowAtCurrentPage(const AFrom: TdxDocumentModelPosition; AKeepFloatingObjects, AForceRestart: Boolean);

    property Measurer: TdxBoxMeasurer read GetMeasurer;
    property State: TdxDocumentFormatterState read FState write SetState;
  public
    constructor Create(AController: TdxDocumentFormattingController);
    destructor Destroy; override;
    procedure ChangeState(AStateType: TdxDocumentFormatterStateType);
    function GetDocumentModel: TdxDocumentModel;
    function GetPieceTable: TdxPieceTable;
    property DocumentModel: TdxDocumentModel read GetDocumentModel;
    property PieceTable: TdxPieceTable read GetPieceTable;
    property ParagraphFormatter: TdxParagraphBoxFormatter read FParagraphFormatter;
    property ParagraphIndex: TdxParagraphIndex read FParagraphIndex write FParagraphIndex;
    procedure Restart(const AFrom: TdxDocumentModelPosition);
    function FormatNextRow: TdxFormattingProcessResult;
    procedure ChangeStateContinueFromParagraph(AParagraphIndex: TdxParagraphIndex);
    procedure OnNewMeasurementAndDrawingStrategyChanged;

    property Controller: TdxDocumentFormattingController read FController;
  end;

  { TdxDocumentFormatterState }

  TdxDocumentFormatterState = class
  private
    FFormatter: TdxDocumentFormatter;
  protected
    procedure Reset; virtual;

    property Formatter: TdxDocumentFormatter read FFormatter;
  public
    constructor Create(AFormatter: TdxDocumentFormatter); virtual;

    function FormatNextRow: TdxFormattingProcess; virtual; abstract;
    function GetType: TdxDocumentFormatterStateType; virtual; abstract;
  end;
  TdxDocumentFormatterStateClass = class of TdxDocumentFormatterState;

  { TdxContinueFromParagraph }

  TdxContinueFromParagraph = class(TdxDocumentFormatterState)
  private
    FNextParagraphIndex: TdxParagraphIndex;
  public
    function FormatNextRow: TdxFormattingProcess; override;
    function GetType: TdxDocumentFormatterStateType; override;

    property NextParagraphIndex: TdxParagraphIndex read FNextParagraphIndex write FNextParagraphIndex;
  end;

  { TdxBeginParagraphFormatting }

  TdxBeginParagraphFormatting = class(TdxDocumentFormatterState)
  private
    FIsHeaderFooter: Boolean;
  protected
    function CreateIterator: TdxParagraphBoxIterator; virtual;
    function EnsureParagraphBoxes: Boolean; virtual;
    procedure Reset; override;
    function GetBeginFromParagraphStart: Boolean; virtual;
    procedure SetBeginFromParagraphStart(Value: Boolean); virtual;
  public
    function GetType: TdxDocumentFormatterStateType; override;
    function FormatNextRow: TdxFormattingProcess; override;

    property BeginFromParagraphStart: Boolean read GetBeginFromParagraphStart write SetBeginFromParagraphStart;
  end;

  { TdxBeginParagraphFormattingFromTheMiddleOfParagraphAndStartOfRow }

  TdxBeginParagraphFormattingFromTheMiddleOfParagraphAndStartOfRow = class(TdxBeginParagraphFormatting)
  private
    FStartModelPosition: TdxDocumentModelPosition;
    FBeginFromParagraphStart: Boolean;
  protected
    function CreateIterator: TdxParagraphBoxIterator; override;
    function GetBeginFromParagraphStart: Boolean; override;
    procedure SetBeginFromParagraphStart(Value: Boolean); override;
    function EnsureParagraphBoxes: Boolean; override;
  public
    function GetType: TdxDocumentFormatterStateType; override;

    property StartModelPosition: TdxDocumentModelPosition read FStartModelPosition write FStartModelPosition;
  end;

  { TdxContinueParagraphFormatting }

  TdxContinueParagraphFormatting = class(TdxDocumentFormatterState)
  public
    function FormatNextRow: TdxFormattingProcess; override;
    function GetType: TdxDocumentFormatterStateType; override;
  end;

  { TdxEndHiddenParagraphFormatting }

  TdxEndHiddenParagraphFormatting = class(TdxDocumentFormatterState)
  public
    function FormatNextRow: TdxFormattingProcess; override;
    function GetType: TdxDocumentFormatterStateType; override;
  end;

  { TdxEndParagraphFormatting }

  TdxEndParagraphFormatting = class(TdxDocumentFormatterState)
  private
    function IsParagraphInInvisibleCell(AParagraph: TdxParagraph): Boolean;
  public
    function FormatNextRow: TdxFormattingProcess; override;
    function GetType: TdxDocumentFormatterStateType; override;
  end;

  { TdxDocumentFormattingFinished }

  TdxDocumentFormattingFinished = class(TdxDocumentFormatterState)
  public
    function FormatNextRow: TdxFormattingProcess; override;
    function GetType: TdxDocumentFormatterStateType; override;
  end;

implementation

uses
  Contnrs, cxGeometry, dxTypeHelpers, Math,
  dxRichEdit.InnerControl,
  dxRichEdit.DocumentModel.Tables,
  dxRichEdit.DocumentModel.VisibleTextFilter.Core,
  dxRichEdit.DocumentModel.FrameFormatting,
  dxRichEdit.DocumentModel.SectionRange,
  dxRichEdit.DocumentModel.TableFormatting,
  dxRichEdit.DocumentModel.ParagraphFormatting;

{ TdxDocumentFormatter }

constructor TdxDocumentFormatter.Create(AController: TdxDocumentFormattingController);
begin
  inherited Create;
  Assert(AController <> nil);
  FController := AController;
  Init;
end;

destructor TdxDocumentFormatter.Destroy;
begin
  FParagraphFormatter.Free;
  FParagraphFormatter := nil;
  DestroyStates;
  inherited Destroy;
end;

function TdxDocumentFormatter.GetDocumentModel: TdxDocumentModel;
begin
  Result := Controller.DocumentModel;
end;

function TdxDocumentFormatter.GetPieceTable: TdxPieceTable;
begin
  Result := FController.PieceTable;
end;

function TdxDocumentFormatter.GetMeasurer: TdxBoxMeasurer;
begin
  Result := FController.DocumentLayout.Measurer;
end;

procedure TdxDocumentFormatter.SetState(Value: TdxDocumentFormatterState);
begin
  FState := Value;
  FState.Reset;
end;

procedure TdxDocumentFormatter.Init;
begin
  CreateStates;
  FParagraphIndex := -1;
  FParagraphFormatter := CreateParagraphBoxFormatter(FController);
  ChangeState(TdxDocumentFormatterStateType.BeginParagraphFormatting);
end;

function TdxDocumentFormatter.CreateParagraphBoxFormatter(AController: TdxDocumentFormattingController): TdxParagraphBoxFormatter;
begin
  Result := TdxParagraphBoxFormatter.Create(PieceTable, Measurer, AController.RowsController);
end;

procedure TdxDocumentFormatter.CreateStates;
const
  StateClasses: array[TdxDocumentFormatterStateType] of TdxDocumentFormatterStateClass = (
    TdxBeginParagraphFormatting,
    TdxBeginParagraphFormattingFromTheMiddleOfParagraphAndStartOfRow,
    TdxContinueParagraphFormatting,
    TdxEndParagraphFormatting,
    TdxEndHiddenParagraphFormatting,
    TdxContinueFromParagraph,
    TdxDocumentFormattingFinished);
var
  I: TdxDocumentFormatterStateType;
begin
  for I := Low(TdxDocumentFormatterStateType) to High(TdxDocumentFormatterStateType) do
    FStates[I] := StateClasses[I].Create(Self);
end;

procedure TdxDocumentFormatter.DestroyStates;
var
  I: TdxDocumentFormatterStateType;
begin
  for I := Low(TdxDocumentFormatterStateType) to High(TdxDocumentFormatterStateType) do
    FreeAndNil(FStates[I]);
end;

procedure TdxDocumentFormatter.OnNewMeasurementAndDrawingStrategyChanged;
begin
  FParagraphFormatter.OnNewMeasurementAndDrawingStrategyChanged(Measurer);
end;

function TdxDocumentFormatter.FormatNextRow: TdxFormattingProcessResult;
begin
  repeat
  until State.FormatNextRow = TdxFormattingProcess.Finish;
  case State.GetType of
    TdxDocumentFormatterStateType.Final:
      begin
        FController.NotifyDocumentFormattingComplete;
        Result.Init(TdxFormattingProcess.Finish);
      end;
    TdxDocumentFormatterStateType.ContinueFromParagraph:
      Result.Init(TdxContinueFromParagraph(State).NextParagraphIndex);
    else
      Result.Init(TdxFormattingProcess.Continue);
  end;
end;

procedure TdxDocumentFormatter.ChangeStateContinueFromParagraph(AParagraphIndex: TdxParagraphIndex);
begin
  State := FStates[TdxDocumentFormatterStateType.ContinueFromParagraph];
  TdxContinueFromParagraph(State).NextParagraphIndex := AParagraphIndex;
end;

procedure TdxDocumentFormatter.ChangeState(AStateType: TdxDocumentFormatterStateType);
begin
  State := FStates[AStateType];
  case AStateType of
    TdxDocumentFormatterStateType.BeginParagraphFormatting,
    TdxDocumentFormatterStateType.BeginParagraphFormattingFromTheMiddleOfParagraphAndStartOfRow:
      Inc(FParagraphIndex);
  end;
end;

procedure TdxDocumentFormatter.OnParagraphFormattingComplete;
begin
  FController.OnParagraphFormattingComplete(ParagraphIndex);
end;

procedure TdxDocumentFormatter.Restart(const AFrom: TdxDocumentModelPosition);
begin
  FParagraphIndex := AFrom.ParagraphIndex - 1;
end;

procedure TdxDocumentFormatter.ResetFromTheStartOfRowAtCurrentPage(const AFrom: TdxDocumentModelPosition;
  AKeepFloatingObjects, AForceRestart: Boolean);
var
  ANewState: TdxBeginParagraphFormattingFromTheMiddleOfParagraphAndStartOfRow;
begin
  FController.ResetFromTheStartOfRowAtCurrentPage(AFrom, AKeepFloatingObjects, AForceRestart);
  Restart(AFrom);
  ChangeState(TdxDocumentFormatterStateType.BeginParagraphFormattingFromTheMiddleOfParagraphAndStartOfRow);
  ANewState := (TdxBeginParagraphFormattingFromTheMiddleOfParagraphAndStartOfRow(State));
  ANewState.BeginFromParagraphStart := (AFrom.LogPosition = PieceTable.Paragraphs[AFrom.ParagraphIndex].LogPosition);
  ANewState.StartModelPosition := AFrom;
end;

{ TdxDocumentFormatterState }

constructor TdxDocumentFormatterState.Create(AFormatter: TdxDocumentFormatter);
begin
  inherited Create;
  Assert(AFormatter <> nil, 'formatter = nil');
  FFormatter := AFormatter;
end;

procedure TdxDocumentFormatterState.Reset;
begin
end;

{ TdxContinueFromParagraph }

function TdxContinueFromParagraph.GetType: TdxDocumentFormatterStateType;
begin
  Result := TdxDocumentFormatterStateType.ContinueFromParagraph;
end;

function TdxContinueFromParagraph.FormatNextRow: TdxFormattingProcess;
begin
  Formatter.ParagraphIndex := FNextParagraphIndex - 1;
  Formatter.ChangeState(TdxDocumentFormatterStateType.BeginParagraphFormatting);
  Result := TdxFormattingProcess.Continue;
end;

{ TdxBeginParagraphFormatting }

function TdxBeginParagraphFormatting.GetType: TdxDocumentFormatterStateType;
begin
  Result := TdxDocumentFormatterStateType.BeginParagraphFormatting;
end;

procedure TdxBeginParagraphFormatting.Reset;
begin
  FIsHeaderFooter := Formatter.PieceTable <> Formatter.DocumentModel.MainPieceTable;
end;

function TdxBeginParagraphFormatting.GetBeginFromParagraphStart: Boolean;
begin
  Result := True;
end;

procedure TdxBeginParagraphFormatting.SetBeginFromParagraphStart(Value: Boolean);
begin
end;

function TdxBeginParagraphFormatting.FormatNextRow: TdxFormattingProcess;
var
  AHasVisibleBoxes: Boolean;
  ARowsController: TdxRowsController;
  ACurrentParagraph: TdxParagraph;
  AParagraphFormatter: TdxParagraphBoxFormatter;
  AState: TdxStateParagraphFrame;
  AStateResult:  TdxStateContinueFormatResult;
  AIterator: TdxParagraphBoxIterator;
  AParagraphFramesLayout: TdxParagraphFramesLayout;
  AItems: TdxParagraphFrameBoxList;
begin
  AHasVisibleBoxes := EnsureParagraphBoxes;
  ARowsController := Formatter.Controller.RowsController;
  ACurrentParagraph := Formatter.PieceTable.Paragraphs[Formatter.ParagraphIndex];
  AParagraphFormatter := Formatter.ParagraphFormatter;
  if not AHasVisibleBoxes then
  begin
    if AParagraphFormatter.Iterator = nil then
      AParagraphFormatter.Iterator := CreateIterator;
    Formatter.ChangeState(TdxDocumentFormatterStateType.EndHiddenParagraphFormatting);
    Exit(TdxFormattingProcess.Continue);
  end;

  AParagraphFramesLayout := ARowsController.ParagraphFramesLayout;
  if (ARowsController.FrameParagraphIndex = -1) and
    AParagraphFormatter.HasActualParagraphFrameProperties(ACurrentParagraph) and
    not AParagraphFramesLayout.ContainsParagraph(ACurrentParagraph) then
  begin
    AParagraphFormatter.BeginParagraphFormatting(CreateIterator, BeginFromParagraphStart, True);
    AState := TdxStateParagraphFrame.Create(AParagraphFormatter, AParagraphFormatter.State);
    try
      AStateResult := AState.ContinueFormat;
    finally
      AState.Free;
    end;

    if ARowsController.FrameParagraphIndex <> -1 then
    begin
      Formatter.ParagraphIndex := ARowsController.FrameParagraphIndex;
      AItems := AParagraphFramesLayout.Items;
      AItems[AItems.Count - 1].LastParagraphIndex := ARowsController.FrameParagraphIndex;
      ARowsController.FrameParagraphIndex := -1;
    end;

    if AStateResult = TdxStateContinueFormatResult.RestartDueFloatingObject then
    begin
      Formatter.ResetFromTheStartOfRowAtCurrentPage(ARowsController.RestartModelPosition, True, False);
      Exit(TdxFormattingProcess.Continue);
    end;

    if Formatter.ParagraphIndex < Formatter.PieceTable.Paragraphs.Last.Index then
      Formatter.ParagraphIndex := Formatter.ParagraphIndex + 1
    else
      Formatter.ChangeState(TdxDocumentFormatterStateType.Final);
    Exit(TdxFormattingProcess.Finish);
  end;

  if AParagraphFramesLayout.ContainsParagraph(ACurrentParagraph) and (ARowsController.FrameParagraphIndex = -1) then
  begin
    if Formatter.ParagraphIndex < Formatter.PieceTable.Paragraphs.Last.Index then
      Formatter.ParagraphIndex := Formatter.ParagraphIndex + 1
    else
      Formatter.ChangeState(TdxDocumentFormatterStateType.ContinueParagraphFormatting);
    Exit(TdxFormattingProcess.Continue);
  end;

  AIterator := CreateIterator;
  AParagraphFormatter.BeginParagraphFormatting(AIterator, BeginFromParagraphStart, False);
  Formatter.ChangeState(TdxDocumentFormatterStateType.ContinueParagraphFormatting);
  Result := TdxFormattingProcess.Continue;
end;

function TdxBeginParagraphFormatting.CreateIterator: TdxParagraphBoxIterator;
var
  AVisibleTextFilter: TdxVisibleTextFilterBase;
  APieceTable: TdxPieceTable;
begin
  APieceTable := Formatter.PieceTable;
  AVisibleTextFilter := APieceTable.VisibleTextFilter;
  Result := TdxParagraphBoxIterator.Create(APieceTable.Paragraphs[Formatter.ParagraphIndex], APieceTable, AVisibleTextFilter);
end;

function TdxBeginParagraphFormatting.EnsureParagraphBoxes: Boolean;
var
  APreFormatter: TdxParagraphCharacterFormatter;
  ACharacterIterator: TdxParagraphCharacterIterator;
  AVisibleTextFilter: TdxVisibleTextFilterBase;
  AHasVisibleBox: Boolean;
  AParagraph: TdxParagraph;
  APieceTable: TdxPieceTable;
begin
  APieceTable := Formatter.PieceTable;
  AParagraph := APieceTable.Paragraphs[Self.Formatter.ParagraphIndex];
  if not AParagraph.BoxCollection.IsValid or FIsHeaderFooter then
  begin
    AVisibleTextFilter := APieceTable.VisibleTextFilter;
    ACharacterIterator := TdxParagraphCharacterIterator.Create(AParagraph, APieceTable, AVisibleTextFilter);
    try
      if (ACharacterIterator.RunIndex <= AParagraph.LastRunIndex) then
      begin
        APreFormatter := TdxParagraphCharacterFormatter.Create(APieceTable, Formatter.Measurer);
        try
          APreFormatter.Format(ACharacterIterator);
          ACharacterIterator := nil;
          AHasVisibleBox := True;
        finally
          FreeAndNil(APreFormatter);
        end;
      end
      else
      begin
        AHasVisibleBox := False;
        AParagraph.BoxCollection.Clear;
      end;
    finally
      FreeAndNil(ACharacterIterator);
    end;
  end
  else
    AHasVisibleBox := AParagraph.BoxCollection.Count > 0;
  AParagraph.BoxCollection.ParagraphStartRunIndex := AParagraph.FirstRunIndex;
  Result := AHasVisibleBox;
end;

{ TdxBeginParagraphFormattingFromTheMiddleOfParagraphAndStartOfRow }

function TdxBeginParagraphFormattingFromTheMiddleOfParagraphAndStartOfRow.GetBeginFromParagraphStart: Boolean;
begin
  Result := FBeginFromParagraphStart;
end;

procedure TdxBeginParagraphFormattingFromTheMiddleOfParagraphAndStartOfRow.SetBeginFromParagraphStart(Value: Boolean);
begin
  FBeginFromParagraphStart := Value;
end;

function TdxBeginParagraphFormattingFromTheMiddleOfParagraphAndStartOfRow.GetType:
  TdxDocumentFormatterStateType;
begin
  Result := TdxDocumentFormatterStateType.BeginParagraphFormattingFromTheMiddleOfParagraphAndStartOfRow;
end;

function TdxBeginParagraphFormattingFromTheMiddleOfParagraphAndStartOfRow.EnsureParagraphBoxes: Boolean;
begin
  Result := True;
end;

function TdxBeginParagraphFormattingFromTheMiddleOfParagraphAndStartOfRow.CreateIterator: TdxParagraphBoxIterator;
var
  ABoxIndex: Integer;
  AParagraph: TdxParagraph;
  APieceTable: TdxPieceTable;
  ARunIndex: TdxRunIndex;
  AFormatterPosition: TdxFormatterPosition;
  AIterator: TdxParagraphBoxIterator;
begin
  AIterator := inherited CreateIterator;
  if not AIterator.VisibleTextFilter.IsRunVisible(StartModelPosition.RunIndex) then
  begin
    ARunIndex := AIterator.VisibleTextFilter.GetNextVisibleRunIndex(StartModelPosition.RunIndex);
    AFormatterPosition.RunIndex := ARunIndex;
    AFormatterPosition.Offset := 0;
  end
  else
  begin
    AFormatterPosition.RunIndex := StartModelPosition.RunIndex;
    AFormatterPosition.Offset := StartModelPosition.RunOffset;
  end;
  APieceTable := Formatter.PieceTable;
  AParagraph := APieceTable.Paragraphs[Formatter.ParagraphIndex];
  ABoxIndex := AParagraph.BoxCollection.InnerCollection.BinarySearchBoxIndex(AFormatterPosition);
  Assert(ABoxIndex >= 0);
  AFormatterPosition.BoxIndex := ABoxIndex;
  AIterator.SetPosition(AFormatterPosition);
  Result := AIterator;
end;

{ TdxContinueParagraphFormatting }

function TdxContinueParagraphFormatting.GetType: TdxDocumentFormatterStateType;
begin
  Result := TdxDocumentFormatterStateType.ContinueParagraphFormatting;
end;

function TdxContinueParagraphFormatting.FormatNextRow: TdxFormattingProcess;
var
  AFormattingProcessResult: TdxFormattingProcessResult;
begin
  AFormattingProcessResult := Formatter.ParagraphFormatter.FormatNextRow;
  if AFormattingProcessResult.FormattingProcess = TdxFormattingProcess.Finish then
  begin
    Formatter.ChangeState(TdxDocumentFormatterStateType.EndParagraphFormatting);
    Result := TdxFormattingProcess.Continue;
  end
  else
    if AFormattingProcessResult.FormattingProcess = TdxFormattingProcess.ContinueFromParagraph then
    begin
      Formatter.ChangeStateContinueFromParagraph(AFormattingProcessResult.ParagraphIndex);
      Result := TdxFormattingProcess.Finish;
    end
    else
      if AFormattingProcessResult.FormattingProcess = TdxFormattingProcess.RestartFromTheStartOfRow then
      begin
        Formatter.ResetFromTheStartOfRowAtCurrentPage(AFormattingProcessResult.RestartPosition, True,
          AFormattingProcessResult.ForceRestart);
        Result := TdxFormattingProcess.Continue;
      end
      else
        Result := TdxFormattingProcess.Finish;
end;

{ TdxEndHiddenParagraphFormatting }

function TdxEndHiddenParagraphFormatting.GetType: TdxDocumentFormatterStateType;
begin
  Result := TdxDocumentFormatterStateType.EndHiddenParagraphFormatting;
end;

function TdxEndHiddenParagraphFormatting.FormatNextRow: TdxFormattingProcess;
var
  ALastPosition: TdxFormatterPosition;
  ACount: TdxParagraphIndex;
  AParagraphs: TdxParagraphCollection;
begin
  AParagraphs := Formatter.PieceTable.Paragraphs;
  ACount := AParagraphs.Count;
  if Formatter.ParagraphFormatter.Iterator <> nil then
  begin
    ALastPosition := Formatter.ParagraphFormatter.Iterator.CreatePosition;
    ALastPosition.RunIndex := AParagraphs[Formatter.ParagraphIndex].LastRunIndex + 1;
    Formatter.ParagraphFormatter.Iterator.SetPositionCore(ALastPosition);
  end;
  if (Formatter.ParagraphIndex + 1) >= ACount then
    Formatter.ChangeState(TdxDocumentFormatterStateType.Final)
  else
    Formatter.ChangeState(TdxDocumentFormatterStateType.BeginParagraphFormatting);
  Result := TdxFormattingProcess.Finish;
end;

{ TdxEndParagraphFormatting }

function TdxEndParagraphFormatting.GetType: TdxDocumentFormatterStateType;
begin
  Result := TdxDocumentFormatterStateType.EndParagraphFormatting;
end;

function TdxEndParagraphFormatting.FormatNextRow: TdxFormattingProcess;
var
  AVisibleTextFilter: IdxVisibleTextFilter;
  AParagraphIndex: TdxParagraphIndex;
  ACount: TdxParagraphIndex;
  AParagraphs: TdxParagraphCollection;
  ACurrentParagraphFrameProperties, ANextParagraphFrameProperties: TdxMergedFrameProperties;
begin
  Formatter.ParagraphFormatter.EndParagraphFormatting;
  Formatter.OnParagraphFormattingComplete;
  AParagraphs := Formatter.PieceTable.Paragraphs;
  ACount := AParagraphs.Count;
  AParagraphIndex := Formatter.ParagraphIndex;
  AVisibleTextFilter := Formatter.PieceTable.VisibleTextFilter;
  while (AParagraphIndex < ACount) and not AVisibleTextFilter.IsRunVisible(AParagraphs[AParagraphIndex].LastRunIndex) do
    Inc(AParagraphIndex);
  while (AParagraphIndex + 1 < ACount) and IsParagraphInInvisibleCell(AParagraphs[AParagraphIndex+ 1]) do
    Inc(AParagraphIndex);
  Formatter.ParagraphIndex := AParagraphIndex;

  ACurrentParagraphFrameProperties := Formatter.ParagraphFormatter.GetActualParagraphFrameProperties(Formatter.PieceTable.Paragraphs[Formatter.ParagraphIndex]);
  try
    if Formatter.ParagraphIndex + 1 < ACount then
    begin
      ANextParagraphFrameProperties := Formatter.PieceTable.Paragraphs[Formatter.ParagraphIndex + 1].GetMergedFrameProperties;
      try
        if ACurrentParagraphFrameProperties <> nil then
        begin
          if (ANextParagraphFrameProperties <> nil) and ACurrentParagraphFrameProperties.CanMerge(ANextParagraphFrameProperties) then
          begin
            Formatter.ChangeState(TdxDocumentFormatterStateType.BeginParagraphFormatting);
            Exit(TdxFormattingProcess.Finish);
          end
          else
          begin
            if Formatter.PieceTable.Paragraphs[Formatter.ParagraphIndex].IsInCell then
            begin
              Formatter.ParagraphFormatter.RowsController.TablesController.LeaveCurrentTableCore;
              Formatter.ChangeState(TdxDocumentFormatterStateType.Final);
              Exit(TdxFormattingProcess.Finish);
            end;
          end;
        end;
      finally
        ANextParagraphFrameProperties.Free;
      end;
    end;

    if ((Formatter.ParagraphIndex + 1) >= ACount) or (ACurrentParagraphFrameProperties <> nil) then
      Formatter.ChangeState(TdxDocumentFormatterStateType.Final)
    else
      Formatter.ChangeState(TdxDocumentFormatterStateType.BeginParagraphFormatting);
    Result := TdxFormattingProcess.Finish;
  finally
    ACurrentParagraphFrameProperties.Free;
  end;
end;

function TdxEndParagraphFormatting.IsParagraphInInvisibleCell(AParagraph: TdxParagraph): Boolean;
var
  ACell: TdxTableCell;
begin
  ACell := AParagraph.GetCell;
  Result := (ACell <> nil) and (ACell.VerticalMerging = TdxMergingState.Continue);
end;

{ TdxDocumentFormattingFinished }

function TdxDocumentFormattingFinished.GetType: TdxDocumentFormatterStateType;
begin
  Result := TdxDocumentFormatterStateType.Final;
end;

function TdxDocumentFormattingFinished.FormatNextRow: TdxFormattingProcess;
begin
  Result := TdxFormattingProcess.Finish;
end;

{ TdxDocumentFormattingController }

constructor TdxDocumentFormattingController.Create(ADocumentLayout: TdxDocumentLayout; APieceTable: TdxPieceTable;
  AFloatingObjectsLayout: TdxFloatingObjectsLayout; AParagraphFramesLayout: TdxParagraphFramesLayout);
begin
  inherited Create;
  Assert(ADocumentLayout <> nil);
  Assert(APieceTable <> nil);
  FDocumentLayout := ADocumentLayout;
  FPieceTable := APieceTable;
  FFloatingObjectsLayout := AFloatingObjectsLayout;
  FParagraphFramesLayout := AParagraphFramesLayout;
  FPageController := CreatePageController;
  PageController.FloatingObjectsLayoutChanged.Add(OnFloatingObjectsLayoutChanged);
  PageController.ParagraphFramesLayoutChanged.Add(OnParagraphFramesLayoutChanged);
  FPageAreaController := CreatePageAreaController;
  ColumnController := CreateColumnController;
  FRowsController := CreateRowController;
  Reset(False);
  PageController.PageFormattingStarted.Add(OnPageFormattingStarted);
  PageController.PageFormattingComplete.Add(OnPageFormattingComplete);
  PageController.PageCountChanged.Add(OnPageCountChanged);
  FRowsController.BeginNextSectionFormatting.Add(OnBeginNextSectionFormatting);
end;

destructor TdxDocumentFormattingController.Destroy;
begin
  FreeAndNil(FRowsController);
  FreeAndNil(FPageAreaController);
  FreeAndNil(FPageController);
  ColumnController := nil;
  inherited Destroy;
end;

function TdxDocumentFormattingController.CleanupEmptyBoxes(const AFrom: TdxDocumentModelPosition): TdxDocumentModelPosition;
var
  APage, ALastPage: TdxPage;
  ALastPageArea: TdxPageArea;
  ALastColumn: TdxColumn;
begin
  ALastPage := DocumentLayout.Pages.Last;
  ALastPage.ClearInvalidatedContent(AFrom.RunIndex, AFrom.PieceTable);
  ALastPageArea := ALastPage.Areas.Last;
  ALastColumn := ALastPageArea.Columns.Last;
  if ALastColumn.Rows.Count <= 0 then
  begin
    ALastPageArea.Columns.Delete(ALastPageArea.Columns.Count - 1);
    ColumnController.CleanupEmptyBoxes(ALastColumn);
  end;
  if ALastPageArea.Columns.Count > 0 then
    Exit(AFrom);
  if ALastPageArea.Columns.Count <= 0 then
    ALastPage.Areas.Delete(ALastPage.Areas.Count - 1);
  if ALastPage.Areas.Count > 0 then
    Result := AFrom;
  DocumentLayout.Pages.Delete(DocumentLayout.Pages.Count - 1);
  APage := DocumentLayout.Pages.Last;
  if APage = nil then
    Result := TdxDocumentModelPosition.Create(PieceTable)
  else
    Result := APage.GetLastPosition(PieceTable);
end;

function TdxDocumentFormattingController.CleanupEmptyBoxesForCurrentPage(
  const AFrom: TdxDocumentModelPosition): TdxDocumentModelPosition;
var
  ALastPage: TdxPage;
  ALastColumn: TdxColumn;
  ALastPageArea: TdxPageArea;
begin
  ALastPage := DocumentLayout.Pages.Last;
  ALastPage.ClearFloatingObjects;
  ALastPageArea := ALastPage.Areas.Last;
  ALastColumn := ALastPageArea.Columns.Last;
  if ALastColumn.Rows.Count <= 0 then
    Exit(AFrom);
  if ALastPageArea.Columns.Count > 0 then
    Exit(AFrom)
  else
    ALastPage.Areas.Delete(ALastPage.Areas.Count - 1);
  Result := AFrom;
end;

function TdxDocumentFormattingController.ClearInvalidatedContent(const AFrom: TdxDocumentModelPosition;
  ARoundToParagraphBoundary, AKeepFloatingObjects: Boolean): TdxClearInvalidatedContentResult;
var
  AEmptyCurrentRow: Boolean;
  APos: TdxFormatterPosition;
  AClearContentResult: TdxClearInvalidatedContentResult;
begin
  APos := CreateFormatterPosition(AFrom, ARoundToParagraphBoundary);
  AEmptyCurrentRow := RowsController.CurrentRow.Boxes.Count = 0;
  AClearContentResult := PageController.ClearInvalidatedContent(APos, AFrom.ParagraphIndex, AKeepFloatingObjects,
    AEmptyCurrentRow);
  if AClearContentResult = TdxClearInvalidatedContentResult.NoRestart then
    Exit(AClearContentResult);
  PageAreaController.ClearInvalidatedContent(APos);
  ColumnController.ClearInvalidatedContent(APos);
  RowsController.ClearInvalidatedContent(DocumentLayout.Pages.Last.Areas.Last.Columns.Last, APos, AFrom.ParagraphIndex);
  Result := AClearContentResult;
end;

function TdxDocumentFormattingController.ClearInvalidatedContentFromTheStartOfRowAtCurrentPage(
  const AFrom: TdxDocumentModelPosition; ARoundToParagraphBoundary,
  AKeepFloatingObjects: Boolean): TdxClearInvalidatedContentResult;
var
  APos: TdxFormatterPosition;
  ALeaveTable, AEmptyCurrentRow: Boolean;
  AClearContentResult: TdxClearInvalidatedContentResult;
begin
  APos := CreateFormatterPosition(AFrom, ARoundToParagraphBoundary);
  AEmptyCurrentRow := RowsController.CurrentRow.Boxes.Count = 0;
  AClearContentResult := PageController.ClearInvalidatedContentFromTheStartOfRowAtCurrentPage(APos, AFrom.ParagraphIndex,
    AKeepFloatingObjects, RowsController.TablesController, AEmptyCurrentRow, ALeaveTable);
  if AClearContentResult = TdxClearInvalidatedContentResult.NoRestart then
    Exit(AClearContentResult);
  if AClearContentResult = TdxClearInvalidatedContentResult.ClearOnlyTableCellRows then
  begin
    RowsController.ClearInvalidatedContentFromTheStartOfRowAtCurrentPage(DocumentLayout.Pages.Last.Areas.Last.Columns.Last, APos, AFrom.ParagraphIndex);
    Result := TdxClearInvalidatedContentResult.Restart;
  end
  else
  begin
    if ALeaveTable then
        RowsController.CurrentColumn := RowsController.CurrentColumn.TopLevelColumn;
    PageAreaController.ClearInvalidatedContent(APos);
    ColumnController.ClearInvalidatedContent(APos);
    RowsController.ClearInvalidatedContent(DocumentLayout.Pages.Last.Areas.Last.Columns.Last, APos, AFrom.ParagraphIndex);
    Result := AClearContentResult;
  end;
end;

function TdxDocumentFormattingController.EnsureParagraphVisible(
  var APos: TdxDocumentModelPosition): TdxDocumentModelPosition;
var
  AParagraphs: TdxParagraphCollection;
  AParagraphIndex: TdxParagraphIndex;
  AInitialCell: TdxTableCell;
  ACellStartParagraphIndex: TdxParagraphIndex;
  AParagraph, ANextParagraph: TdxParagraphBase;
begin
  AParagraphs := TdxParagraphCollection(APos.PieceTable.Paragraphs);
  AParagraphIndex := APos.ParagraphIndex;
  if AParagraphIndex > 0 then
  begin
    while AParagraphIndex > 0 do
    begin
      AParagraph := AParagraphs[AParagraphIndex];
      if APos.PieceTable.Runs[AParagraph.LastRunIndex] is TdxSectionRun then
      begin
        Dec(AParagraphIndex);
        Continue;
      end;
      if AParagraph.IsEmpty and (AParagraphIndex + 1 < AParagraphs.Count) then
      begin
        ANextParagraph := AParagraphs[AParagraphIndex + 1];
        if ANextParagraph.IsEmpty and (APos.PieceTable.Runs[ANextParagraph.FirstRunIndex] is TdxSectionRun) then
        begin
          Dec(AParagraphIndex);
          Continue;
        end;
      end;
      Break;
    end;
  end;
  AInitialCell := AParagraphs[AParagraphIndex].GetCell;
  if AInitialCell <> nil then
    ACellStartParagraphIndex := AInitialCell.StartParagraphIndex
  else
    ACellStartParagraphIndex := 0;
  while (AParagraphIndex > ACellStartParagraphIndex) and
    not PieceTable.VisibleTextFilter.IsRunVisible(AParagraphs[AParagraphIndex - 1].LastRunIndex) do
  begin
    if (AInitialCell <> nil) or not TdxParagraph(AParagraphs[AParagraphIndex - 1]).IsInCell then
      Dec(AParagraphIndex)
    else
      Break;
  end;
  AParagraph := AParagraphs[AParagraphIndex];
  APos.RunIndex := AParagraph.FirstRunIndex;
  APos.ParagraphIndex := AParagraphIndex;
  APos.LogPosition := AParagraph.LogPosition;
  APos.RunStartLogPosition := AParagraph.LogPosition;
  Result := APos;
end;

function TdxDocumentFormattingController.EnsurePositionVisible(
  var APos: TdxDocumentModelPosition): TdxDocumentModelPosition;
var
  AIndex: TdxParagraphIndex;
begin
  AIndex := APos.ParagraphIndex;

  APos := EnsureRunVisible(APos);
  APos := EnsureParagraphVisible(APos);
  if APos.ParagraphIndex <> AIndex then
    Result := TdxDocumentChangesHandler.EnsureTopLevelParagraph(APos)
  else
    Result := APos;
end;

function TdxDocumentFormattingController.EnsureRunVisible(var APos: TdxDocumentModelPosition): TdxDocumentModelPosition;
var
  ARunIndex: TdxRunIndex;
  AParagraphIndex, ACellStartParagraphIndex: TdxParagraphIndex;
  AParagraphs: TdxParagraphCollection;
  AParagraph: TdxParagraph;
  AInitialCell: TdxTableCell;
begin
  if PieceTable.VisibleTextFilter.IsRunVisible(APos.RunIndex) then
    Exit(APos);
  ARunIndex := APos.RunIndex;
  AParagraphIndex := APos.ParagraphIndex;
  AParagraphs := TdxParagraphCollection(APos.PieceTable.Paragraphs);
  AParagraph := AParagraphs[AParagraphIndex];
  AInitialCell := AParagraph.GetCell;
  if AInitialCell <> nil then
    ACellStartParagraphIndex := AInitialCell.StartParagraphIndex
  else
    ACellStartParagraphIndex := 0;
  while not PieceTable.VisibleTextFilter.IsRunVisible(ARunIndex) or
    PositionSectionBreakAfterParagraphBreak(AParagraph, ARunIndex) do
  begin
    Dec(ARunIndex);
    if ARunIndex < 0 then
      Break;
    if ARunIndex >= AParagraph.FirstRunIndex then
      Continue;
    Dec(AParagraphIndex);
    AParagraph := AParagraphs[AParagraphIndex];
    if AInitialCell <> nil then
    begin
      if AParagraphIndex < ACellStartParagraphIndex then
      begin
        Inc(AParagraphIndex);
        Inc(ARunIndex);
        Break;
      end;
    end
    else
    begin
      if AParagraph.IsInCell then
      begin
        Inc(AParagraphIndex);
        Inc(ARunIndex);
        Break;
      end;
    end;
  end;
  APos.RunIndex := ARunIndex;
  APos.ParagraphIndex := AParagraphIndex;
  Result := APos;
end;

procedure TdxDocumentFormattingController.BeginNextSectionFormatting(ASectionIndex: TdxSectionIndex);
begin
  PageController.BeginNextSectionFormatting(ASectionIndex);
  PageAreaController.BeginSectionFormatting(PageController.CurrentSection, ColumnController.TopLevelColumnsCount);
  ColumnController.BeginSectionFormatting(PageController.CurrentSection);
  RowsController.BeginSectionFormatting(PageController.CurrentSection);
end;

function TdxDocumentFormattingController.CreateFormatterPosition(const AFrom: TdxDocumentModelPosition;
  ARoundToParagraphBoundary: Boolean): TdxFormatterPosition;
var
  AParagraph: TdxParagraph;
begin
  if ARoundToParagraphBoundary then
  begin
    AParagraph := PieceTable.Paragraphs[AFrom.ParagraphIndex];
    Result := TdxFormatterPosition.Create(AParagraph.FirstRunIndex, 0, 0);
  end
  else
    Result := TdxFormatterPosition.Create(AFrom.RunIndex, AFrom.RunOffset, 0);
end;

function TdxDocumentFormattingController.CreatePageAreaController: TdxPageAreaController;
begin
  Result := TdxPageAreaController.Create(PageController);
end;

procedure TdxDocumentFormattingController.ClearPages;
begin
  DocumentLayout.Pages.Clear;
end;

procedure TdxDocumentFormattingController.ClearPagesFrom(APageIndex: Integer);
var
  APages: TdxPageCollection;
begin
  APages := DocumentLayout.Pages;
  APages.DeleteRange(APageIndex, APages.Count - APageIndex);
end;

function TdxDocumentFormattingController.GetDocumentModel: TdxDocumentModel;
begin
  Result := TdxDocumentModel(DocumentLayout.DocumentModel);
end;

function TdxDocumentFormattingController.GetPageCount: Integer;
begin
  Result := PageController.PageCount;
end;

procedure TdxDocumentFormattingController.NotifyDocumentFormattingComplete;
var
  APage: TdxPage;
  ANewPageCount: Integer;
begin
  APage := PageController.Pages.Last;
  PageController.FinalizePagePrimaryFormatting(APage, True);
  RowsController.OnPageFormattingComplete(FPageController.CurrentSection, APage);
  APage.PrimaryFormattingComplete := True;
  ANewPageCount := PageController.PageCount + APage.NumSkippedPages;
  if ANewPageCount <> DocumentModel.ExtendedDocumentProperties.Pages then
    SetPageCount(ANewPageCount, True);
  RaisePageFormattingComplete(APage, True);
end;

procedure TdxDocumentFormattingController.OnParagraphFramesLayoutChanged(ASender: TObject; E: TdxEventArgs);
begin
  RowsController.OnParagraphFramesLayoutChanged;
end;

procedure TdxDocumentFormattingController.OnBeginNextSectionFormatting(ASender: TObject; E: TdxEventArgs);
begin
  Assert(E is TdxBeginNextSectionFormattingEventArgs);
  BeginNextSectionFormatting(TdxBeginNextSectionFormattingEventArgs(E).SectionIndex);
end;

procedure TdxDocumentFormattingController.OnPageCountChanged(ASender: TObject; E: TdxEventArgs);
begin
  RaisePageCountChanged;
end;

procedure TdxDocumentFormattingController.OnPageFormattingComplete(ASender: TObject;
  E: TdxPageFormattingCompleteEventArgs);
var
  AOldPageCount, ANewPageCount: Integer;
begin
  PageController.FinalizePagePrimaryFormatting(E.Page, False);
  RowsController.OnPageFormattingComplete(FPageController.CurrentSection, E.Page);
  E.Page.PrimaryFormattingComplete := True;
  AOldPageCount := DocumentModel.ExtendedDocumentProperties.Pages;
  ANewPageCount := Math.Max(AOldPageCount, PageController.PageCount + E.Page.NumSkippedPages);
  if ANewPageCount <> AOldPageCount then
    SetPageCount(ANewPageCount, False);
  RaisePageFormattingComplete(E.Page, False);
end;

procedure TdxDocumentFormattingController.OnPageFormattingStarted(ASender: TObject;
  E: TdxPageFormattingCompleteEventArgs);
begin
  RaisePageFormattingStarted(E.Page);
end;

procedure TdxDocumentFormattingController.OnParagraphFormattingComplete(AParagraphIndex: TdxParagraphIndex);
begin
end;

procedure TdxDocumentFormattingController.OnFloatingObjectsLayoutChanged(ASender: TObject; E: TdxEventArgs);
begin
  RowsController.OnFloatingObjectsLayoutChanged;
end;

function TdxDocumentFormattingController.PositionSectionBreakAfterParagraphBreak(AParagraph: TdxParagraph;
  ARunIndex: TdxRunIndex): Boolean;
begin
  Result := (ARunIndex > 0) and (AParagraph.Length = 1) and (AParagraph.PieceTable.Runs[ARunIndex] is TdxSectionRun);
end;

procedure TdxDocumentFormattingController.RaisePageCountChanged;
begin
  if not FOnPageCountChanged.Empty then
    FOnPageCountChanged.Invoke(Self, TdxEventArgs.Empty);
end;

procedure TdxDocumentFormattingController.RaisePageFormattingComplete(APage: TdxPage;
  ADocumentFormattingComplete: Boolean);
var
  AArgs: TdxPageFormattingCompleteEventArgs;
begin
  if FOnPageFormattingComplete.Empty then
    Exit;
  AArgs := TdxPageFormattingCompleteEventArgs.Create(APage, ADocumentFormattingComplete);
  try
    FOnPageFormattingComplete.Invoke(Self, AArgs);
  finally
    AArgs.Free;
  end;
end;

procedure TdxDocumentFormattingController.RaisePageFormattingStarted(APage: TdxPage);
var
  AArgs: TdxPageFormattingCompleteEventArgs;
begin
  if FOnPageFormattingStarted.Empty then
    Exit;
  AArgs := TdxPageFormattingCompleteEventArgs.Create(APage, False);
  try
    FOnPageFormattingStarted.Invoke(Self, AArgs);
  finally
    AArgs.Free;
  end;
end;

procedure TdxDocumentFormattingController.RaiseResetSecondaryFormattingForPage(APage: TdxPage; APageIndex: Integer);
var
  AArgs: TdxResetSecondaryFormattingForPageArgs;
begin
  if FOnResetSecondaryFormattingForPage.Empty then
    Exit;
  AArgs := TdxResetSecondaryFormattingForPageArgs.Create(APage, APageIndex);
  try
    FOnResetSecondaryFormattingForPage.Invoke(Self, AArgs);
  finally
    AArgs.Free;
  end;
end;

procedure TdxDocumentFormattingController.Reset(AKeepFloatingObjects: Boolean);
begin
  DocumentLayout.Counters.Reset;
  PageController.Reset(AKeepFloatingObjects);
  PageAreaController.Reset(PageController.CurrentSection);
  ColumnController.Reset(PageController.CurrentSection);
  RowsController.Reset(PageController.CurrentSection, AKeepFloatingObjects);
end;

function TdxDocumentFormattingController.ResetFrom(var AFrom: TdxDocumentModelPosition;
  AKeepFloatingObjects: Boolean): TdxDocumentModelPosition;
var
  AClearContentResult: TdxClearInvalidatedContentResult;
  APageCountBefore: Integer;
  ALastPage: TdxPage;
  AFirstPosition: TdxDocumentModelPosition;
begin
  AFrom := EnsurePositionVisible(AFrom);
  if AFrom.ParagraphIndex = 0 then
  begin
    Reset(AKeepFloatingObjects);
    Exit(AFrom);
  end;

  RowsController.SaveCurrentInfo;

  AClearContentResult := ClearInvalidatedContent(AFrom, True, AKeepFloatingObjects);
  if AClearContentResult = TdxClearInvalidatedContentResult.NoRestart then
  begin
    DocumentLayout.Counters.ResetFrom(AFrom.LogPosition);
    Exit(AFrom);
  end;
  APageCountBefore := DocumentLayout.Pages.Count;
  Result := CleanupEmptyBoxes(AFrom);
  if DocumentLayout.Pages.Count < APageCountBefore then
    PageController.ClearFloatingObjectsLayout;
  ALastPage := DocumentLayout.Pages.Last;
  if ALastPage <> nil then
  begin
    ALastPage.PrimaryFormattingComplete := False;
    ALastPage.SecondaryFormattingComplete := False;
    ALastPage.CheckSpellingComplete := False;
    AFirstPosition := ALastPage.GetFirstPosition(PieceTable);
    if Result.LogPosition >= AFirstPosition.LogPosition then
      Result := AFirstPosition;
    DocumentLayout.Counters.ResetFrom(Result.LogPosition);
  end
  else
  begin
    Reset(AKeepFloatingObjects);
    AFrom := TdxDocumentModelPosition.FromParagraphStart(PieceTable, 0);
    Exit(AFrom);
  end;
  if AClearContentResult = TdxClearInvalidatedContentResult.RestartFromTheStartOfSection then
    RestartFormattingFromTheStartOfSection
  else
    RestartFormattingFromTheMiddleOfSection;
end;

procedure TdxDocumentFormattingController.ResetFromTheStartOfRowAtCurrentPage(const AFrom: TdxDocumentModelPosition;
  AKeepFloatingObjects, AForceRestart: Boolean);
var
  ALastPage: TdxPage;
  AResult: TdxDocumentModelPosition;
  AClearContentResult: TdxClearInvalidatedContentResult;
begin
  AClearContentResult := ClearInvalidatedContentFromTheStartOfRowAtCurrentPage(AFrom, False, AKeepFloatingObjects);
  if (AClearContentResult = TdxClearInvalidatedContentResult.NoRestart) and not AForceRestart then
    Exit;
  AResult := CleanupEmptyBoxesForCurrentPage(AFrom);
  ALastPage := DocumentLayout.Pages.Last;
  Assert(ALastPage <> nil);
  ALastPage.PrimaryFormattingComplete := False;
  ALastPage.SecondaryFormattingComplete := False;
  ALastPage.CheckSpellingComplete := False;
  if AResult.LogPosition >= ALastPage.GetFirstPosition(PieceTable).LogPosition then
    AResult := ALastPage.GetFirstPosition(PieceTable);
  DocumentLayout.Counters.ResetFrom(AResult.LogPosition);
  RestartFormattingFromTheStartOfRowAtCurrentPage(AClearContentResult, AResult);
end;

procedure TdxDocumentFormattingController.RestartFormattingFromTheMiddleOfSection;
var
  AIndex: Integer;
  ALastPage: TdxPage;
  ASection: TdxSection;
  ALastColumn: TdxColumn;
  ALastPageArea: TdxPageArea;
begin
  ALastPage := DocumentLayout.Pages.Last;
  ALastPageArea := ALastPage.Areas.Last;
  ALastColumn := ALastPageArea.Columns.Last;
  ASection := TdxDocumentModel(DocumentLayout.DocumentModel).Sections[PageController.CurrentSectionIndex];
  PageController.RestartFormattingFromTheMiddleOfSection(ASection);
  PageAreaController.RestartFormattingFromTheMiddleOfSection(PageController.CurrentSection, ALastPage.Areas.Count - 1);
  AIndex := ALastPageArea.Columns.IndexOf(ALastColumn);
  ColumnController.RestartFormattingFromTheMiddleOfSection(PageController.CurrentSection, AIndex);
  if AIndex < ColumnController.ColumnsBounds.Count then
    ALastColumn.Bounds := ColumnController.ColumnsBounds[AIndex];
  RowsController.RestartFormattingFromTheMiddleOfSection(PageController.CurrentSection, ALastColumn);
end;

procedure TdxDocumentFormattingController.RestartFormattingFromTheStartOfRowAtCurrentPage(
  AClearContentResult: TdxClearInvalidatedContentResult; const AFrom: TdxDocumentModelPosition);
var
  ALastPage: TdxPage;
  ASection: TdxSection;
  ALastColumn: TdxColumn;
  ALastPageArea: TdxPageArea;
begin
  ALastPage := DocumentLayout.Pages.Last;
  ALastPageArea := ALastPage.Areas.Last;
  if ALastPageArea <> nil then
    ALastColumn := ALastPageArea.Columns.Last
  else
    ALastColumn := nil;
  if AClearContentResult = TdxClearInvalidatedContentResult.RestartFromTheStartOfSection then
    PageController.RestartFormattingFromTheStartOfSection
  else
    PageController.RestartFormattingFromTheStartOfRowAtCurrentPage;
  ASection := TdxDocumentModel(DocumentLayout.DocumentModel).Sections[PageController.CurrentSectionIndex];
  PageAreaController.RestartFormattingFromTheStartOfRowAtCurrentPage;
  ColumnController.RestartFormattingFromTheStartOfRowAtCurrentPage;
  RowsController.RestartFormattingFromTheStartOfRowAtCurrentPage(ASection, ALastColumn);
end;

procedure TdxDocumentFormattingController.RestartFormattingFromTheStartOfSection;
var
  AIndex: Integer;
  ALastPage: TdxPage;
  ALastColumn: TdxColumn;
  ALastPageArea: TdxPageArea;
begin
  ALastPage := DocumentLayout.Pages.Last;
  ALastPageArea := ALastPage.Areas.Last;
  ALastColumn := ALastPageArea.Columns.Last;
  PageController.RestartFormattingFromTheStartOfSection;
  PageAreaController.RestartFormattingFromTheStartOfSection(PageController.CurrentSection, ALastPage.Areas.Count - 1);
  AIndex := ALastPageArea.Columns.IndexOf(ALastColumn);
  ColumnController.RestartFormattingFromTheStartOfSection(PageController.CurrentSection, AIndex);
  if AIndex < ColumnController.ColumnsBounds.Count then
    ALastColumn.Bounds := ColumnController.ColumnsBounds[AIndex];
  RowsController.RestartFormattingFromTheStartOfSection(PageController.CurrentSection, ALastColumn);
end;

procedure TdxDocumentFormattingController.SetColumnController(const Value: TdxColumnController);
begin
  if FColumnController <> Value then
  begin
    TdxColumnController.Release(FColumnController);
    FColumnController := Value;
    TdxColumnController.AddReference(FColumnController);
  end;
end;

procedure TdxDocumentFormattingController.SetPageCount(APageCount: Integer; APrimaryFormattingFinished: Boolean);
var
  APages: TdxPageCollection;
  APage: TdxPage;
  I: Integer;
begin
  PieceTable.ContentType.SetPageCount(APageCount);

  if not APrimaryFormattingFinished and (FPageCountSinceLastResetSecondaryFormatting < 100) then
  begin
    Inc(FPageCountSinceLastResetSecondaryFormatting);
    Exit;
  end;
  FPageCountSinceLastResetSecondaryFormatting := 0;
  APages := PageController.Pages;
  for I := 0 to APages.Count - 1 do
  begin
    APage := APages[I];
    if APage.NumPages <> APageCount then
    begin
      APage.NumPages := APageCount;
      if APage.SecondaryFormattingComplete then
      begin
        APage.SecondaryFormattingComplete := False;
        APage.CheckSpellingComplete := False;
        RaiseResetSecondaryFormattingForPage(APage, I);
      end;
    end;
  end;
end;

end.
