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

unit dxRichEdit.Dialogs.TableOfContentsController;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, SysUtils, Classes, Variants,
  Generics.Defaults, Generics.Collections,
  dxCoreClasses,
  dxRichEdit.View.Core,
  dxRichEdit.Utils.Types,
  dxGenerics,
  dxRichEdit.Dialogs.Core,
  dxRichEdit.DocumentModel.TextRange,
  dxRichEdit.DocumentModel.Fields.Core,
  dxRichEdit.DocumentModel.Fields,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.Fields.TocField,
  dxRichEdit.DocumentModel.Simple;

type
  { TdxTOCFormControllerParameters }

  TdxTOCFormControllerParameters = class(TdxFormControllerParameters)
  strict private
    FTocField: TdxField;
  protected
    property TocField: TdxField read FTocField;
  public
    constructor Create(const AControl: IdxRichEditControl; ATocField: TdxField);
  end;

  { TdxTableOfContentsFormController }

  TdxTableOfContentsFormController = class(TdxFormController)
  strict private
    FPieceTable: TdxPieceTable;
    FField: TdxField;
    FParsedInfo: TdxTocField;
    FShowPageNumbers: Boolean;
    FRightAlignPageNumbers: Boolean;
    FUseHyperlinks: Boolean;
    FShowLevels: Integer;
  public
    constructor Create(AParameters: TdxTOCFormControllerParameters);
    destructor Destroy; override;
    procedure ApplyChanges; override;

    property ShowPageNumbers: Boolean read FShowPageNumbers write FShowPageNumbers;
    property RightAlignPageNumbers: Boolean read FRightAlignPageNumbers write FRightAlignPageNumbers;
    property UseHyperlinks: Boolean read FUseHyperlinks write FUseHyperlinks;
    property ShowLevels: Integer read FShowLevels write FShowLevels;
  end;

implementation

uses
  Contnrs, Math,
  dxRichEdit.Utils.CopyHelper,
  dxRichEdit.DocumentModel.FieldCalculatorService,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.Utils.Exceptions,
  dxRichEdit.Utils.Token,
  dxStringHelper;

type
  { TdxUpdateFieldInfo }

  TdxUpdateFieldInfo = class abstract
  protected
    function GetInterval: TdxDocumentLogInterval; virtual; abstract;
  public
    procedure Update(APieceTable: TdxPieceTable); virtual; abstract;
    property Interval: TdxDocumentLogInterval read GetInterval;
  end;

  { TdxUpdateFieldInfoBase }

  TdxUpdateFieldInfoBase = class abstract(TdxUpdateFieldInfo)
  strict private
    FInterval: TdxDocumentLogInterval;
  protected
    function GetInterval: TdxDocumentLogInterval; override;
    procedure TrimEndSpace(APieceTable: TdxPieceTable);
    procedure UpdateCore(APieceTable: TdxPieceTable); virtual; abstract;
  public
    constructor Create(const AInterval: TdxDocumentLogInterval); overload;
    procedure Update(APieceTable: TdxPieceTable); override;
  end;

  { TdxUpdateStringFieldInfo }

  TdxUpdateStringFieldInfo = class(TdxUpdateFieldInfoBase)
  strict private
    FNewContent: string;
  protected
    procedure UpdateCore(APieceTable: TdxPieceTable); override;

    property NewContent: string read FNewContent;
  public
    constructor Create(const AInterval: TdxDocumentLogInterval; const ANewContent: string); overload;
  end;

  { TdxUpdateDocumentModelFieldInfo }

  TdxUpdateDocumentModelFieldInfo = class abstract(TdxUpdateFieldInfoBase)
  strict private
    FModel: TdxDocumentModel;
  protected
    property DocumentModel: TdxDocumentModel read FModel;
  public
    constructor Create(const AInterval: TdxDocumentLogInterval; AModel: TdxDocumentModel);
  end;

  { TdxUpdateDocumentModelAsField }

  TdxUpdateDocumentModelAsField = class(TdxUpdateDocumentModelFieldInfo)
  protected
    procedure UpdateCore(APieceTable: TdxPieceTable); override;
  end;

  { TdxUpdateDocumentModel }

  TdxUpdateDocumentModel = class(TdxUpdateDocumentModelFieldInfo)
  protected
    procedure UpdateCore(APieceTable: TdxPieceTable); override;
  end;

  { TdxUpdateFieldInfoComparer }

  TdxUpdateFieldInfoComparer = class(TComparer<TdxUpdateFieldInfo>)
  public
    function Compare(const Left, Right: TdxUpdateFieldInfo): Integer; override;
  end;

  { IdxInsertFieldInfo }

  TdxInsertFieldInfo = class abstract
    procedure Insert(APieceTable: TdxPieceTable; APos: TdxDocumentLogPosition); virtual; abstract;
  end;

  { TdxInsertStringFieldInfo }

  TdxInsertStringFieldInfo = class(TdxInsertFieldInfo)
  strict private
    FContent: string;
  public
    constructor Create(const AContent: string);
    procedure Insert(APieceTable: TdxPieceTable; APos: TdxDocumentLogPosition); override;
  end;

  { TdxInsertDocumentModelFieldInfo }

  TdxInsertDocumentModelFieldInfo = class(TdxInsertFieldInfo)
  strict private
    FModel: TdxDocumentModel;
  public
    constructor Create(AModel: TdxDocumentModel);
    procedure Insert(APieceTable: TdxPieceTable; APos: TdxDocumentLogPosition); override;
  end;

  { TdxInsertStringWithDocumentModelFieldInfoBase }

  TdxInsertStringWithDocumentModelFieldInfoBase = class abstract(TdxInsertFieldInfo)
  strict private
    FFieldSwitch: string;
    FArgument: TdxDocumentModel;
  protected
    procedure InsertArgument(APieceTable: TdxPieceTable; APos: TdxDocumentLogPosition); virtual; abstract;

    property Argument: TdxDocumentModel read FArgument;
  public
    constructor Create(const AFieldSwitch: string; AArgument: TdxDocumentModel);
    procedure Insert(APieceTable: TdxPieceTable; APos: TdxDocumentLogPosition); override;
  end;

  { TdxInsertStringWithDocumentModelAsField }

  TdxInsertStringWithDocumentModelAsField = class(TdxInsertStringWithDocumentModelFieldInfoBase)
  protected
    procedure InsertArgument(APieceTable: TdxPieceTable; APos: TdxDocumentLogPosition); override;
  end;

  { TdxInsertStringWithDocumentModel }

  TdxInsertStringWithDocumentModel = class(TdxInsertStringWithDocumentModelFieldInfoBase)
  protected
    procedure InsertArgument(APieceTable: TdxPieceTable; APos: TdxDocumentLogPosition); override;
  end;

  { TdxDeferredChanges }

  TdxDeferredChanges = class
  strict private
    FCanUpdateFieldType: Boolean;
    FInsertingSwitches: TdxNamedOrdinalDictionary<TdxInsertFieldInfo>;
    FUpdatingSwitches: TdxNamedOrdinalDictionary<TdxUpdateFieldInfo>;
    FArgumentPositions: TdxIntegerList;
    FDeferredInsertions: TdxObjectList<TdxInsertFieldInfo>;
    FDeferredUpdates: TdxObjectList<TdxUpdateFieldInfo>;
  public
    constructor Create;
    destructor Destroy; override;

    procedure UpdateFieldType(const AInfo: TdxUpdateFieldInfo);
    procedure UpdateSwitch(const AFieldSwitch: string; const AInfo: TdxUpdateFieldInfo);
    procedure UpdateArgument(AIndex: Integer; const AInfo: TdxUpdateFieldInfo);
    procedure AddSwitch(const AFieldSwitch, AArgument: string); overload;
    procedure AddSwitch(const AFieldSwitch: string; AArgument: TdxDocumentModel); overload;
    procedure AddSwitch(const AFieldSwitch: string; AArgument: TdxDocumentModel; AWrapToField: Boolean); overload;
    procedure AddArgument(AIndex: Integer; const AArgument: string); overload;
    procedure AddArgument(AIndex: Integer; AArgument: TdxDocumentModel); overload;
    function ContainsSwitch(const AFieldSwitch: string): Boolean;
    procedure CheckSwitch(const AFieldSwitch: string; const AInsertion: TdxInsertFieldInfo); overload;
    procedure CheckSwitch(const AFieldSwitch: string; const AUpdate: TdxUpdateFieldInfo); overload;
    procedure UndoSwitchModifications(const AFieldSwitch: string);
    procedure CheckArgument(AIndex: Integer);

    property DeferredInsertions: TdxObjectList<TdxInsertFieldInfo> read FDeferredInsertions;
    property DeferredUpdates: TdxObjectList<TdxUpdateFieldInfo> read FDeferredUpdates;
  end;

  { TdxInstructionController }

  TdxInstructionController = class
  strict private
    const
      CharsToEscape: array [0..2] of Char = (' ', '\', '"');
      Brackets: array [0..3] of Char = ('(', ')', '[', ']');
  strict private
    FInstructions: TdxInstructionCollection;
    FPieceTable: TdxPieceTable;
    FField: TdxField;
    FFieldCodeEnd: TdxDocumentLogPosition;
    FDeferredChanges: TdxDeferredChanges;
    FParsedInfo: TdxCalculatedFieldBase;
    FRemovingSwitches: TdxHashSet<string>;
    FSuppressFieldsUpdateAfterUpdateInstruction: Boolean;
    procedure UpdateFieldCodeEnd;
    function GetDocumentModel: TdxDocumentModel;
  protected
    FBeforeUpdateFields: TdxEventHandler;
    procedure UpdateSwitch(const AFieldSwitch: string; const AArgument: string); overload;
    procedure UpdateSwitch(const AFieldSwitch: string; AArgument: TdxDocumentModel; AWrapToField: Boolean = True); overload;

    property PieceTable: TdxPieceTable read FPieceTable;
    property DocumentModel: TdxDocumentModel read GetDocumentModel;
    property FieldCodeEnd: TdxDocumentLogPosition read FFieldCodeEnd;
    property DeferredChanges: TdxDeferredChanges read FDeferredChanges;
  public
    constructor Create(APieceTable: TdxPieceTable; AParsedInfo: TdxCalculatedFieldBase; AField: TdxField);
    destructor Destroy; override;
    class function GetEscapedFieldName(const AFieldName: string): string; static;
    class function GetEscapedArgument(const AArgument: string): string; overload; static;
    class function GetEscapedArgument(const AArgument: string; ATrimArgument: Boolean): string; overload; static;
    function GetArgumentAsString(AIndex: Integer): string;
    function GetFieldType: string;
    procedure UpdateFieldType(const AFieldType: string);
    procedure RemoveSwitch(const AFieldSwitch: string);
    procedure SetSwitch(const AFieldSwitch: string); overload;
    procedure SetSwitch(const AFieldSwitch: string; const AArgument: string); overload;
    procedure SetSwitch(const AFieldSwitch: string; const AArgument: string; ATrimArgument: Boolean); overload;
    procedure SetSwitch(const AFieldSwitch: string; AArgument: TdxDocumentModel); overload;
    procedure SetSwitch(const AFieldSwitch: string; AArgument: TdxDocumentModel; AWrapToField: Boolean); overload;
    function ContainsSwitch(const AFieldSwitch: string): Boolean;
    function IsSwitchRemoving(const AFieldSwitch: string): Boolean;
    function WillContainSwitch(const AFieldSwitch: string): Boolean;
    procedure SetArgument(AIndex: Integer; const AArgument: string); overload;
    procedure SetArgument(AIndex: Integer; AArgument: TdxDocumentModel); overload;
    procedure UpdateArgument(AIndex: Integer; const AArgument: string); overload;
    procedure UpdateArgument(AIndex: Integer; AArgument: TdxDocumentModel); overload;
    function GetArgumentAsLogInterval(AIndex: Integer): TdxDocumentLogInterval;
    procedure ApplyDeferredActions;
    procedure TrimEndSpace;
    function ApplyDeferredInsertions: Boolean;
    function ApplyDeferredChanges: Boolean;

    property Instructions: TdxInstructionCollection read FInstructions;
    property ParsedInfo: TdxCalculatedFieldBase read FParsedInfo;
    property Field: TdxField read FField;
    property SuppressFieldsUpdateAfterUpdateInstruction: Boolean read FSuppressFieldsUpdateAfterUpdateInstruction
      write FSuppressFieldsUpdateAfterUpdateInstruction;
  end;

{ TdxTOCFormControllerParameters }

constructor TdxTOCFormControllerParameters.Create(const AControl: IdxRichEditControl; ATocField: TdxField);
begin
  inherited Create(AControl);
  Assert(ATocField <> nil, 'tocField');
  FTocField := ATocField;
end;

{ TdxTableOfContentsFormController }

constructor TdxTableOfContentsFormController.Create(AParameters: TdxTOCFormControllerParameters);
var
  AParser: TdxFieldCalculatorService;
begin
  inherited Create;
  FPieceTable := TdxPieceTable(AParameters.TocField.PieceTable);
  AParser := TdxFieldCalculatorService.Create;
  try
    FParsedInfo := TdxTocField(AParser.ParseField(FPieceTable, AParameters.TocField));
  finally
    AParser.Free;
  end;
  FField := AParameters.TocField;
  UseHyperlinks := FParsedInfo.Options.CreateHyperlinks;
  ShowPageNumbers := FParsedInfo.Options.NoPageNumberLevels.Count = 0;
  RightAlignPageNumbers := FParsedInfo.Options.PageNumberSeparator.IsNull or (FParsedInfo.Options.PageNumberSeparator.Value = '');
  ShowLevels := FParsedInfo.Options.HeaderLevels.Count;
end;

destructor TdxTableOfContentsFormController.Destroy;
begin
  FParsedInfo.Free;
  inherited Destroy;
end;

procedure TdxTableOfContentsFormController.ApplyChanges;
var
  AController: TdxInstructionController;
begin
  AController := TdxInstructionController.Create(FPieceTable, FParsedInfo, FField);
  try
    if ShowPageNumbers then
      AController.RemoveSwitch('n')
    else
      AController.SetSwitch('n');
    if RightAlignPageNumbers then
      AController.RemoveSwitch('p')
    else
      AController.SetSwitch('p', ' ', False);
    if UseHyperlinks then
      AController.SetSwitch('h')
    else
      AController.RemoveSwitch('h');

    if ShowLevels <> 9 then
      AController.SetSwitch('o', Format('1-%d', [ShowLevels]));
    AController.ApplyDeferredActions;
  finally
    AController.Free;
  end;
end;

{ TdxUpdateFieldInfoBase }

constructor TdxUpdateFieldInfoBase.Create(const AInterval: TdxDocumentLogInterval);
begin
  inherited Create;
  FInterval := AInterval;
end;

procedure TdxUpdateFieldInfoBase.Update(APieceTable: TdxPieceTable);
begin
  APieceTable.DeleteContent(FInterval.Start, FInterval.Length, False, True, True, True);
  UpdateCore(APieceTable);
end;

function TdxUpdateFieldInfoBase.GetInterval: TdxDocumentLogInterval;
begin
  Result := FInterval;
end;

procedure TdxUpdateFieldInfoBase.TrimEndSpace(APieceTable: TdxPieceTable);
var
  AInfo: TdxRunInfo;
  AStart: TdxDocumentModelPosition;
begin
  AInfo := APieceTable.FindRunInfo(Interval.Start, 1);
  try
    if AInfo = nil then
      Exit;
    AStart := AInfo.Start;
    if APieceTable.Runs[AStart.RunIndex].ClassType <> TdxTextRun then
      Exit;
    if APieceTable.GetRunPlainText(AStart.RunIndex)[AStart.RunOffset] = ' ' then
      APieceTable.DeleteContent(AStart.RunStartLogPosition + AStart.RunOffset, 1, False, True, True);
  finally
    AInfo.Free;
  end;
end;

{ TdxUpdateStringFieldInfo }

constructor TdxUpdateStringFieldInfo.Create(const AInterval: TdxDocumentLogInterval; const ANewContent: string);
begin
  inherited Create(AInterval);
  FNewContent := ANewContent;
end;

procedure TdxUpdateStringFieldInfo.UpdateCore(APieceTable: TdxPieceTable);
begin
  if NewContent <> '' then
    APieceTable.InsertText(Interval.Start, NewContent)
  else
    TrimEndSpace(APieceTable);
end;

{ TdxUpdateDocumentModelFieldInfo }

constructor TdxUpdateDocumentModelFieldInfo.Create(const AInterval: TdxDocumentLogInterval; AModel: TdxDocumentModel);
begin
  inherited Create(AInterval);
  Assert(AModel <> nil, 'model');
  FModel := AModel;
end;

{ TdxUpdateDocumentModelAsField }

procedure TdxUpdateDocumentModelAsField.UpdateCore(APieceTable: TdxPieceTable);
var
  AField: TdxField;
begin
  AField := TdxCopyHelper.CopyAndWrapToField(DocumentModel.MainPieceTable, APieceTable,
    TdxDocumentLogInterval.Create(0, DocumentModel.MainPieceTable.DocumentEndLogPosition - DocumentModel.MainPieceTable.DocumentStartLogPosition),
    Interval.Start);
  AField.DisableUpdate := True;
end;

{ TdxUpdateDocumentModel }

procedure TdxUpdateDocumentModel.UpdateCore(APieceTable: TdxPieceTable);
begin
  TdxCopyHelper.CopyCore(DocumentModel.MainPieceTable, APieceTable,
    TdxDocumentLogInterval.Create(0, DocumentModel.MainPieceTable.DocumentEndLogPosition - DocumentModel.MainPieceTable.DocumentStartLogPosition),
    Interval.Start);
end;

{ TdxUpdateFieldInfoComparer }

function TdxUpdateFieldInfoComparer.Compare(const Left, Right: TdxUpdateFieldInfo): Integer;
var
  AXStart, AYStart: Integer;
begin
  AXStart := Left.Interval.Start;
  AYStart := Right.Interval.Start;
  Result := CompareValue(AYStart, AXStart);
end;

{ TdxInsertStringFieldInfo }

constructor TdxInsertStringFieldInfo.Create(const AContent: string);
begin
  inherited Create;
  FContent := AContent;
end;

procedure TdxInsertStringFieldInfo.Insert(APieceTable: TdxPieceTable; APos: TdxDocumentLogPosition);
begin
  if FContent = '' then
    Exit;
  APieceTable.InsertText(APos, FContent);
end;

{ TdxInsertDocumentModelFieldInfo }

constructor TdxInsertDocumentModelFieldInfo.Create(AModel: TdxDocumentModel);
begin
  inherited Create;
  Assert(AModel <> nil, 'model');
  FModel := AModel;
end;

procedure TdxInsertDocumentModelFieldInfo.Insert(APieceTable: TdxPieceTable; APos: TdxDocumentLogPosition);
var
  AField: TdxField;
begin
  AField := TdxCopyHelper.CopyAndWrapToField(FModel.MainPieceTable, APieceTable,
    TdxDocumentLogInterval.Create(0, FModel.MainPieceTable.DocumentEndLogPosition - FModel.MainPieceTable.DocumentStartLogPosition),
    APos);
  AField.DisableUpdate := True;
end;

{ TdxInsertStringWithDocumentModelFieldInfoBase }

constructor TdxInsertStringWithDocumentModelFieldInfoBase.Create(const AFieldSwitch: string; AArgument: TdxDocumentModel);
begin
  inherited Create;
  Assert(AFieldSwitch <> '', 'fieldSwitch');
  Assert(AArgument <> nil, 'argument');
  FFieldSwitch := Format('\%s ', [AFieldSwitch]);
  FArgument := AArgument;
end;

procedure TdxInsertStringWithDocumentModelFieldInfoBase.Insert(APieceTable: TdxPieceTable; APos: TdxDocumentLogPosition);
begin
  APieceTable.InsertText(APos, FFieldSwitch);
  Inc(APos, Length(FFieldSwitch));
  InsertArgument(APieceTable, APos);
end;

{ TdxInsertStringWithDocumentModelAsField }

procedure TdxInsertStringWithDocumentModelAsField.InsertArgument(APieceTable: TdxPieceTable; APos: TdxDocumentLogPosition);
var
  AArgumentPieceTable: TdxPieceTable;
  AField: TdxField;
begin
  AArgumentPieceTable := Argument.MainPieceTable;
  AField := TdxCopyHelper.CopyAndWrapToField(AArgumentPieceTable, APieceTable,
    TdxDocumentLogInterval.Create(0, AArgumentPieceTable.DocumentEndLogPosition - AArgumentPieceTable.DocumentStartLogPosition),
    APos);
  AField.DisableUpdate := True;
end;

{ TdxInsertStringWithDocumentModel }

procedure TdxInsertStringWithDocumentModel.InsertArgument(APieceTable: TdxPieceTable; APos: TdxDocumentLogPosition);
var
  AArgumentPieceTable: TdxPieceTable;
begin
  AArgumentPieceTable := Argument.MainPieceTable;
  TdxCopyHelper.CopyCore(AArgumentPieceTable, APieceTable,
    TdxDocumentLogInterval.Create(0, AArgumentPieceTable.DocumentEndLogPosition - AArgumentPieceTable.DocumentStartLogPosition),
    APos);
end;

{ TdxDeferredChanges }

constructor TdxDeferredChanges.Create;
begin
  inherited Create;
  FCanUpdateFieldType := True;
  FDeferredInsertions := TdxObjectList<TdxInsertFieldInfo>.Create;
  FDeferredUpdates := TdxObjectList<TdxUpdateFieldInfo>.Create;

  FInsertingSwitches := TdxNamedOrdinalDictionary<TdxInsertFieldInfo>.Create;
  FUpdatingSwitches := TdxNamedOrdinalDictionary<TdxUpdateFieldInfo>.Create;
  FArgumentPositions := TdxIntegerList.Create;
end;

destructor TdxDeferredChanges.Destroy;
begin
  FDeferredInsertions.Free;
  FDeferredUpdates.Free;
  FInsertingSwitches.Free;
  FUpdatingSwitches.Free;
  FArgumentPositions.Free;
  inherited Destroy;
end;

procedure TdxDeferredChanges.UpdateFieldType(const AInfo: TdxUpdateFieldInfo);
begin
  if not FCanUpdateFieldType then
    TdxRichEditExceptions.ThrowInternalException;
  DeferredUpdates.Add(AInfo);
  FCanUpdateFieldType := False;
end;

procedure TdxDeferredChanges.UpdateSwitch(const AFieldSwitch: string; const AInfo: TdxUpdateFieldInfo);
begin
  CheckSwitch(AFieldSwitch, AInfo);
  DeferredUpdates.Add(AInfo);
end;

procedure TdxDeferredChanges.UpdateArgument(AIndex: Integer; const AInfo: TdxUpdateFieldInfo);
begin
  CheckArgument(AIndex);
  DeferredUpdates.Add(AInfo);
end;

procedure TdxDeferredChanges.AddSwitch(const AFieldSwitch, AArgument: string);
var
  ASwitchWithArgument: string;
  AInsertStringFieldInfo: TdxInsertStringFieldInfo;
begin
  ASwitchWithArgument := Format('\%s %s', [AFieldSwitch, AArgument]);
  AInsertStringFieldInfo := TdxInsertStringFieldInfo.Create(ASwitchWithArgument);
  CheckSwitch(AFieldSwitch, AInsertStringFieldInfo);
  DeferredInsertions.Add(AInsertStringFieldInfo);
end;

procedure TdxDeferredChanges.AddSwitch(const AFieldSwitch: string; AArgument: TdxDocumentModel);
begin
  AddSwitch(AFieldSwitch, AArgument, True);
end;

procedure TdxDeferredChanges.AddSwitch(const AFieldSwitch: string; AArgument: TdxDocumentModel; AWrapToField: Boolean);
var
  AFieldInfo: TdxInsertFieldInfo;
begin
  if AWrapToField then
    AFieldInfo := TdxInsertStringWithDocumentModelAsField.Create(AFieldSwitch, AArgument)
  else
    AFieldInfo := TdxInsertStringWithDocumentModel.Create(AFieldSwitch, AArgument);
  CheckSwitch(AFieldSwitch, AFieldInfo);
  DeferredInsertions.Add(AFieldInfo);
end;

procedure TdxDeferredChanges.AddArgument(AIndex: Integer; const AArgument: string);
begin
  CheckArgument(AIndex);
  DeferredInsertions.Add(TdxInsertStringFieldInfo.Create(AArgument));
end;

procedure TdxDeferredChanges.AddArgument(AIndex: Integer; AArgument: TdxDocumentModel);
begin
  CheckArgument(AIndex);
  DeferredInsertions.Add(TdxInsertDocumentModelFieldInfo.Create(AArgument));
end;

function TdxDeferredChanges.ContainsSwitch(const AFieldSwitch: string): Boolean;
begin
  Result := (FInsertingSwitches.ContainsKey(AFieldSwitch)) or (FUpdatingSwitches.ContainsKey(AFieldSwitch));
end;

procedure TdxDeferredChanges.CheckSwitch(const AFieldSwitch: string; const AInsertion: TdxInsertFieldInfo);
begin
  UndoSwitchModifications(AFieldSwitch);
  FInsertingSwitches.Add(AFieldSwitch, AInsertion);
end;

procedure TdxDeferredChanges.CheckSwitch(const AFieldSwitch: string; const AUpdate: TdxUpdateFieldInfo);
begin
  UndoSwitchModifications(AFieldSwitch);
  FUpdatingSwitches.Add(AFieldSwitch, AUpdate);
end;

procedure TdxDeferredChanges.UndoSwitchModifications(const AFieldSwitch: string);
begin
  if FInsertingSwitches.ContainsKey(AFieldSwitch) then
  begin
    FDeferredInsertions.Remove(FInsertingSwitches[AFieldSwitch]);
    FInsertingSwitches.Remove(AFieldSwitch);
  end;
  if FUpdatingSwitches.ContainsKey(AFieldSwitch) then
  begin
    FDeferredUpdates.Remove(FUpdatingSwitches[AFieldSwitch]);
    FUpdatingSwitches.Remove(AFieldSwitch);
  end;
end;

procedure TdxDeferredChanges.CheckArgument(AIndex: Integer);
begin
  if FArgumentPositions.Contains(AIndex) then
    TdxRichEditExceptions.ThrowInternalException;
  FArgumentPositions.Add(AIndex);
end;

{ TdxInstructionController }

constructor TdxInstructionController.Create(APieceTable: TdxPieceTable; AParsedInfo: TdxCalculatedFieldBase; AField: TdxField);
begin
  inherited Create;
  Assert(APieceTable <> nil, 'pieceTable');
  Assert(AParsedInfo <> nil, 'parsedInfo');
  Assert(AField <> nil, 'field');
  FPieceTable := APieceTable;
  FParsedInfo := AParsedInfo;
  FInstructions := AParsedInfo.Switches;
  FField := AField;
  UpdateFieldCodeEnd;
  FDeferredChanges := TdxDeferredChanges.Create;
  FRemovingSwitches := TdxHashSet<string>.Create;
end;

destructor TdxInstructionController.Destroy;
begin
  FDeferredChanges.Free;
  FRemovingSwitches.Free;
  inherited Destroy;
end;

procedure TdxInstructionController.UpdateFieldCodeEnd;
begin
  FFieldCodeEnd := FPieceTable.GetRunLogPosition(FField.Code.&End);
end;

function TdxInstructionController.GetDocumentModel: TdxDocumentModel;
begin
  Result := PieceTable.DocumentModel;
end;

function TdxInstructionController.GetArgumentAsString(AIndex: Integer): string;
begin
  Result := Instructions.GetArgumentAsString(AIndex);
end;

function TdxInstructionController.GetFieldType: string;
var
  AIterator: TdxDocumentFieldIterator;
  AScanner: TdxFieldScanner;
  AToken: IdxToken;
begin
  AIterator := TdxDocumentFieldIterator.Create(PieceTable, Field);
  try
    AScanner := TdxFieldScanner.Create(AIterator, PieceTable);
    try
      AToken := AScanner.Scan;
      Result := AToken.Value;
    finally
      AScanner.Free;
    end;
  finally
    AIterator.Free;
  end;
end;

procedure TdxInstructionController.UpdateFieldType(const AFieldType: string);
var
  AIterator: TdxDocumentFieldIterator;
  AScanner: TdxFieldScanner;
  AToken: IdxToken;
  AInterval: TdxDocumentLogInterval;
  ANativeSwithces, AInvariableSwitches: TArray<string>;
  ACount, I, J: Integer;
  AShouldRemoveSwitch: Boolean;
begin
  AIterator := TdxDocumentFieldIterator.Create(PieceTable, Field);
  AScanner := TdxFieldScanner.Create(AIterator, PieceTable);
  try
    AToken := AScanner.Scan;
  finally
    AScanner.Free;
  end;
  AInterval := TdxDocumentLogInterval.Create(AToken.Position, AToken.Length);
  DeferredChanges.UpdateFieldType(TdxUpdateStringFieldInfo.Create(AInterval, AFieldType));
  ANativeSwithces := ParsedInfo.GetNativeSwithes;
  AInvariableSwitches := ParsedInfo.GetInvariableSwitches;
  ACount := Length(ANativeSwithces);
  for I := 0 to ACount - 1 do
  begin
    AShouldRemoveSwitch := True;
    for J := 0 to Length(AInvariableSwitches) - 1 do
      if AInvariableSwitches[J] = ANativeSwithces[I] then
      begin
        AShouldRemoveSwitch := False;
        Break;
      end;
    if AShouldRemoveSwitch then
      RemoveSwitch(ANativeSwithces[I]);
  end;
end;

procedure TdxInstructionController.RemoveSwitch(const AFieldSwitch: string);
var
  ASwitchInterval, AArgumentInterval, ASwitchWithArgument: TdxDocumentLogInterval;
begin
  DeferredChanges.UndoSwitchModifications(AFieldSwitch);
  if not Instructions.GetBool(AFieldSwitch) then
    Exit;
  FRemovingSwitches.Include(AFieldSwitch);
  ASwitchInterval := Instructions.GetSwitchDocumentInterval(AFieldSwitch);
  AArgumentInterval := Instructions.GetSwitchArgumentDocumentInterval(AFieldSwitch, True);
  if AArgumentInterval.IsNull then
    ASwitchWithArgument := ASwitchInterval
  else
    ASwitchWithArgument := TdxDocumentLogInterval.Create(ASwitchInterval.Start,
      AArgumentInterval.Start + AArgumentInterval.Length - ASwitchInterval.Start);

  DeferredChanges.UpdateSwitch(AFieldSwitch, TdxUpdateStringFieldInfo.Create(ASwitchWithArgument));
end;

procedure TdxInstructionController.SetSwitch(const AFieldSwitch: string);
begin
  SetSwitch(AFieldSwitch, '');
end;

procedure TdxInstructionController.SetSwitch(const AFieldSwitch: string; const AArgument: string);
begin
  SetSwitch(AFieldSwitch, AArgument, True);
end;

procedure TdxInstructionController.SetSwitch(const AFieldSwitch: string; const AArgument: string; ATrimArgument: Boolean);
begin
  FRemovingSwitches.Exclude(AFieldSwitch);
  if not Instructions.GetBool(AFieldSwitch) then
    DeferredChanges.AddSwitch(AFieldSwitch, GetEscapedArgument(AArgument, ATrimArgument))
  else
    UpdateSwitch(AFieldSwitch, AArgument);
end;

procedure TdxInstructionController.SetSwitch(const AFieldSwitch: string; AArgument: TdxDocumentModel);
begin
  SetSwitch(AFieldSwitch, AArgument, True);
end;

procedure TdxInstructionController.SetSwitch(const AFieldSwitch: string; AArgument: TdxDocumentModel; AWrapToField: Boolean);
begin
  FRemovingSwitches.Exclude(AFieldSwitch);
  if not Instructions.GetBool(AFieldSwitch) then
    DeferredChanges.AddSwitch(AFieldSwitch, AArgument, AWrapToField)
  else
    UpdateSwitch(AFieldSwitch, AArgument, AWrapToField);
end;

function TdxInstructionController.ContainsSwitch(const AFieldSwitch: string): Boolean;
begin
  Result := (Instructions.GetBool(AFieldSwitch)) or (DeferredChanges.ContainsSwitch(AFieldSwitch));
end;

function TdxInstructionController.IsSwitchRemoving(const AFieldSwitch: string): Boolean;
begin
  Result := FRemovingSwitches.Contains(AFieldSwitch);
end;

function TdxInstructionController.WillContainSwitch(const AFieldSwitch: string): Boolean;
begin
  Result := ContainsSwitch(AFieldSwitch) and not FRemovingSwitches.Contains(AFieldSwitch);
end;

procedure TdxInstructionController.UpdateSwitch(const AFieldSwitch: string; const AArgument: string);
var
  AArgumentInterval: TdxDocumentLogInterval;
begin
  AArgumentInterval := Instructions.GetSwitchArgumentDocumentInterval(AFieldSwitch, True);
  if AArgumentInterval.IsNull then
    Exit;
  DeferredChanges.UpdateSwitch(AFieldSwitch, TdxUpdateStringFieldInfo.Create(AArgumentInterval, GetEscapedArgument(AArgument)));
end;

procedure TdxInstructionController.UpdateSwitch(const AFieldSwitch: string; AArgument: TdxDocumentModel; AWrapToField: Boolean);
var
  AArgumentInterval: TdxDocumentLogInterval;
  AFieldInfo: TdxUpdateFieldInfo;
begin
  AArgumentInterval := Instructions.GetSwitchArgumentDocumentInterval(AFieldSwitch, True);
  if AArgumentInterval.IsNull then
    Exit;
  if AWrapToField then
    AFieldInfo := TdxUpdateDocumentModelAsField.Create(AArgumentInterval, AArgument)
  else
    AFieldInfo := TdxUpdateDocumentModel.Create(AArgumentInterval, AArgument);
  DeferredChanges.UpdateSwitch(AFieldSwitch, AFieldInfo);
end;

procedure TdxInstructionController.SetArgument(AIndex: Integer; const AArgument: string);
begin
  if AIndex >= Instructions.Arguments.Count then
    DeferredChanges.AddArgument(AIndex, GetEscapedArgument(AArgument))
  else
    UpdateArgument(AIndex, AArgument);
end;

procedure TdxInstructionController.SetArgument(AIndex: Integer; AArgument: TdxDocumentModel);
begin
  if AIndex >= Instructions.Arguments.Count then
    DeferredChanges.AddArgument(AIndex, AArgument)
  else
    UpdateArgument(AIndex, AArgument);
end;

procedure TdxInstructionController.UpdateArgument(AIndex: Integer; const AArgument: string);
var
  AInterval: TdxDocumentLogInterval;
begin
  AInterval := GetArgumentAsLogInterval(AIndex);

  DeferredChanges.UpdateArgument(AIndex, TdxUpdateStringFieldInfo.Create(AInterval, GetEscapedArgument(AArgument)));
end;

procedure TdxInstructionController.UpdateArgument(AIndex: Integer; AArgument: TdxDocumentModel);
var
  AInterval: TdxDocumentLogInterval;
begin
  AInterval := GetArgumentAsLogInterval(AIndex);

  DeferredChanges.UpdateArgument(AIndex, TdxUpdateDocumentModelAsField.Create(AInterval, AArgument));
end;

function TdxInstructionController.GetArgumentAsLogInterval(AIndex: Integer): TdxDocumentLogInterval;
var
  AToken: IdxToken;
begin
  AToken := Instructions.GetArgument(AIndex);
  case AToken.ActualKind of
    TdxTokenKind.QuotedText:
      Result := TdxDocumentLogInterval.Create(AToken.Position - 1, AToken.Length + 2);
    TdxTokenKind.Template:
      Result := TdxDocumentLogInterval.Create(AToken.Position - 1, AToken.Length + 3);
    else
      Result := TdxDocumentLogInterval.Create(AToken.Position, AToken.Length);
  end;
end;

class function TdxInstructionController.GetEscapedFieldName(const AFieldName: string): string;
var
  ANewFieldName: TStringBuilder;
begin
  if TdxStringHelper.IndexOfAny(AFieldName, Brackets) >= 0 then
  begin
    ANewFieldName := TStringBuilder.Create;
    try
      ANewFieldName.Append(AFieldName);
      ANewFieldName.Replace('(', '\(').Replace(')', '\)').Replace('[', '\[').Replace(']', '\]');
      Result := GetEscapedArgument(ANewFieldName.ToString);
    finally
      ANewFieldName.Free;
    end;
  end
  else
    Result := GetEscapedArgument(AFieldName);
end;

class function TdxInstructionController.GetEscapedArgument(const AArgument: string): string;
begin
  Result := GetEscapedArgument(AArgument, True);
end;

class function TdxInstructionController.GetEscapedArgument(const AArgument: string; ATrimArgument: Boolean): string;
begin
  if AArgument = '' then
    Exit(AArgument);
  if ATrimArgument then
    Result := Trim(AArgument)
  else
    Result := AArgument;
  if TdxStringHelper.IndexOfAny(Result, CharsToEscape) >= 0 then
  begin
    Result := StringReplace(StringReplace(Result, '\', '\\', [rfReplaceAll]), '"', '\"', [rfReplaceAll]);
    Result := Format('"%s"', [Result]);
  end;
end;

procedure TdxInstructionController.ApplyDeferredActions;
var
  AHasDeferredInsertion, AHasDeferredChanges: Boolean;
begin
  DocumentModel.BeginUpdate;
  try
    AHasDeferredInsertion := ApplyDeferredInsertions;
    AHasDeferredChanges := ApplyDeferredChanges;
    FRemovingSwitches.Clear;
    if not AHasDeferredChanges and not AHasDeferredInsertion then
      Exit;
    TrimEndSpace;
    if not SuppressFieldsUpdateAfterUpdateInstruction then
      PieceTable.FieldUpdater.UpdateFieldAndNestedFields(Field);
  finally
    DocumentModel.EndUpdate;
  end;
end;

procedure TdxInstructionController.TrimEndSpace;
var
  AIndex: TdxRunIndex;
  APlainText: string;
  AOffset: Integer;
begin
  AIndex := Field.Code.&End - 1;
  if FPieceTable.Runs[AIndex].ClassType <> TdxTextRun then
    Exit;
  APlainText := FPieceTable.GetRunPlainText(AIndex);
  AOffset := Length(APlainText) - 1;
  if APlainText[AOffset + 1] = ' ' then
    FPieceTable.DeleteContent(FPieceTable.GetRunLogPosition(AIndex) + AOffset, 1, False, True, True);
end;

function TdxInstructionController.ApplyDeferredInsertions: Boolean;
var
  ACount, I: Integer;
begin
  ACount := DeferredChanges.DeferredInsertions.Count;
  if ACount = 0 then
    Exit(False);
  for I := 0 to ACount - 1 do
  begin
    DeferredChanges.DeferredInsertions[I].Insert(PieceTable, FieldCodeEnd);
    PieceTable.InsertText(FieldCodeEnd, ' ');
    UpdateFieldCodeEnd;
  end;
  DeferredChanges.DeferredInsertions.Clear;
  Result := True;
end;

function TdxInstructionController.ApplyDeferredChanges: Boolean;
var
  ACount, I: Integer;
  AComparer: IComparer<TdxUpdateFieldInfo>;
begin
  ACount := DeferredChanges.DeferredUpdates.Count;
  if ACount = 0 then
    Exit(False);
  AComparer := TdxUpdateFieldInfoComparer.Create;
  DeferredChanges.DeferredUpdates.Sort(AComparer);
  for I := 0 to ACount - 1 do
    DeferredChanges.DeferredUpdates[I].Update(PieceTable);
  DeferredChanges.DeferredUpdates.Clear;
  Result := True;
end;

end.
