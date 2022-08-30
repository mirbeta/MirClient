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

unit dxRichEdit.Dialogs.NumberingFormController;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils,
  dxCore,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.Commands.Numbering,
  dxRichEdit.DocumentModel.Numbering,
  dxRichEdit.DocumentModel.NumberingFormatting,
  dxRichEdit.DocumentModel.ParagraphFormatting,
  dxRichEdit.View.Core,
  dxRichEdit.DocumentModel.CharacterFormatting,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.Dialogs.Core,
  dxRichEdit.Dialogs.CustomDialog,
  dxRichEdit.DocumentModel.Section,
  dxRichEdit.Utils.Types;

type

  TdxNumberingListApplyScope = (
    RestartNumbering,
    ModifyCurrentList,
    ContinuePreviousList,
    ToSelectedText
  );

  TdxMultiLevelNumberingListApplyScope = (
    WholeList,
    WholeDocument,
    ToSelectedText
  );

  { TdxBaseNumberingListFormControllerParameters }

  TdxBaseNumberingListFormControllerParameters = class(TdxFormControllerParameters)
  private
    FLevelIndex: Integer;
    FLevels: TdxListLevelCollection;
  public
    constructor Create(const AControl: IdxRichEditControl; ALevels: TdxListLevelCollection; ALevelIndex: Integer);
    property Levels: TdxListLevelCollection read FLevels;
    property LevelIndex: Integer read FLevelIndex;
  end;

  { TdxNumberingListFormControllerParameters }

  TdxNumberingListFormControllerParameters = class(TdxFormControllerParameters)
  strict private
    FParagraphs: TdxParagraphList;
  protected
    property Paragraphs: TdxParagraphList read FParagraphs;
  public
    constructor Create(const AControl: IdxRichEditControl; const AParagraphs: TdxParagraphList);
  end;

  { TdxBulletedListFormControllerParameters }

  TdxBulletedListFormControllerParameters = TdxBaseNumberingListFormControllerParameters;

  { TdxBulletedListFormControllerParameters }

  TdxSimpleNumberingListFormControllerParameters = class(TdxBaseNumberingListFormControllerParameters)
  private
    FStartNumber: Integer;
  public
    constructor Create(const AControl: IdxRichEditControl; ALevels: TdxListLevelCollection; ALevelIndex, AStartNumber: Integer);
    property StartNumber: Integer read FStartNumber;
  end;

  { TdxBulletedListFormController }

  TdxBulletedListFormController = class(TdxFormController)
  private
    FSourceLevels: TdxListLevelCollection;
    FEditedLevels: TdxListLevelCollection;
    FEditedLevelIndex: Integer;
    function GetEditedLevel: TdxListLevel; inline;
    function GetLeftIndent: Integer; inline;
    procedure SetLeftIndent(const Value: Integer); inline;
    function GetFirstLineIndent: Integer; inline;
    procedure SetFirstLineIndent(const Value: Integer); inline;
    function GetCharacterProperties: TdxCharacterProperties; inline;
    function GetDisplayFormat: string; inline;
    procedure SetDisplayFormat(const Value: string); inline;
    function GetFirstLineIndentType: TdxParagraphFirstLineIndent;
    procedure SetFirstLineIndentType(const Value: TdxParagraphFirstLineIndent);
  protected
    function CreateEditedLevels: TdxListLevelCollection;
    procedure InitializeController; virtual;
  public
    constructor Create(const AControllerParameters: TdxBaseNumberingListFormControllerParameters);
    destructor Destroy; override;
    procedure AssignControllerIndentValues(AAlign, AIndent: Variant);
    procedure ApplyChanges; override;
    property EditedLevel: TdxListLevel read GetEditedLevel;
    property EditedLevels: TdxListLevelCollection read FEditedLevels;
    property EditedLevelIndex: Integer read FEditedLevelIndex write FEditedLevelIndex;
    property DisplayFormat: string read GetDisplayFormat write SetDisplayFormat;
    property FirstLineIndent: Integer read GetFirstLineIndent write SetFirstLineIndent;
    property LeftIndent: Integer read GetLeftIndent write SetLeftIndent;
    property CharacterProperties: TdxCharacterProperties read GetCharacterProperties;
    property FirstLineIndentType: TdxParagraphFirstLineIndent read GetFirstLineIndentType write SetFirstLineIndentType;
  end;

  { TdxSimpleNumberingListController }

  TdxSimpleNumberingListController = class(TdxBulletedListFormController)
  private
    function GetAlignment: TdxListNumberAlignment; inline;
    function GetFormat: TdxNumberingFormat; inline;
    function GetStart: Integer; inline;
    procedure SetAlignment(const Value: TdxListNumberAlignment);
    procedure SetFormat(const Value: TdxNumberingFormat); inline;
    procedure SetStart(const Value: Integer);
  public
    constructor Create(const AControllerParameters: TdxSimpleNumberingListFormControllerParameters);
    property Alignment: TdxListNumberAlignment read GetAlignment write SetAlignment;
    property Format: TdxNumberingFormat read GetFormat write SetFormat;
    property Start: Integer read GetStart write SetStart;
  end;

  { TdxMultiLevelNumberingListFormController }

  TdxMultiLevelNumberingListFormController = class(TdxSimpleNumberingListController)
  private
    function GetSeparator: Char;
    procedure SetSeparator(const Value: Char);
  public
    property Separator: Char read GetSeparator write SetSeparator;
  end;

  { TdxNumberingListFormController }

  TdxNumberingListFormController = class(TdxFormController)
  strict private
    FFakeDocumentModel: TdxDocumentModel;
    FNoneList: TdxAbstractNumberingList;
  strict private
    FSelectedParagraphsListIndex: TdxNumberingListIndex;
    FNewListIndex: TdxNumberingListIndex;
    FLevelIndex: Integer;
    FControl: IdxRichEditControl;
    FParagraphs: TdxParagraphList;
    FNewAbstractList: TdxAbstractNumberingList;
    FApplyScope: TdxNumberingListApplyScope;
    FNewListType: TdxNumberingType;
    function GetDocumentModel: TdxDocumentModel;
    function GetNoneList: TdxAbstractNumberingList;
  protected
    procedure InitializeController;
    function GetSelectedParagraphsListIndex: TdxNumberingListIndex;
    procedure ApplyChangesCore;
    procedure ApplyChangesRestartNumbering;
    procedure ApplyChangesModifyCurrentList;
    procedure ApplyChangesContinuePreviousList;
    function AreThereParagraphsInList: Boolean;
    function AreThereParagraphsInDifferentLists: Boolean;
    procedure ApplyChangesToSelectedText; virtual;
    function GetFirstNumberingListIndex: TdxNumberingListIndex;
    procedure ModifyParagraph(AParagraph: TdxParagraph; AListIndex: TdxNumberingListIndex);
    procedure ModifyNumberingListLevel(ATargetList: TdxNumberingList; ALevelIndex: Integer);
    function CalculateListInsertionMode: TdxInsertListMode;
    procedure RemoveNumberingList;
    procedure UpdateParagraphsDown(ANumberingListIndex: TdxNumberingListIndex; ANewListIndex: TdxNumberingListIndex);
  public
    constructor Create(const AControllerParameters: TdxNumberingListFormControllerParameters);
    destructor Destroy; override;

    function CreateAbstractNumberingListCopy(ATargetModel: TdxDocumentModel;
      ASourceList: TdxAbstractNumberingList): TdxAbstractNumberingList;
    function GetSelectedAbstractNumberingList: TdxAbstractNumberingList;
    procedure ApplyChanges; override;

    property NoneList: TdxAbstractNumberingList read GetNoneList;
    property LevelIndex: Integer read FLevelIndex write FLevelIndex;
    property SelectedParagraphsListIndex: TdxNumberingListIndex read FSelectedParagraphsListIndex write FSelectedParagraphsListIndex;
    property NewAbstractList: TdxAbstractNumberingList read FNewAbstractList write FNewAbstractList;
    property NewListIndex: TdxNumberingListIndex read FNewListIndex write FNewListIndex;
    property Paragraphs: TdxParagraphList read FParagraphs;
    property DocumentModel: TdxDocumentModel read GetDocumentModel;
    property ApplyScope: TdxNumberingListApplyScope read FApplyScope write FApplyScope;
    property NewListType: TdxNumberingType read FNewListType write FNewListType;
  end;

implementation

uses
  Classes, RTLConsts,
  dxCoreClasses, cxVariants,
  dxRichEdit.Utils.BatchUpdateHelper;

{ TdxBaseNumberingListFormControllerParameters }

constructor TdxBaseNumberingListFormControllerParameters.Create(const AControl: IdxRichEditControl;
  ALevels: TdxListLevelCollection; ALevelIndex: Integer);
begin
  inherited Create(AControl);
  FLevels := ALevels;
  FLevelIndex := ALevelIndex;
end;

{ TdxNumberingListFormControllerParameters }

constructor TdxNumberingListFormControllerParameters.Create(const AControl: IdxRichEditControl;
  const AParagraphs: TdxParagraphList);
begin
  inherited Create(AControl);
  FParagraphs := AParagraphs;
end;

{ TdxSimpleNumberingListFormControllerParameters }

constructor TdxSimpleNumberingListFormControllerParameters.Create(const AControl: IdxRichEditControl;
  ALevels: TdxListLevelCollection; ALevelIndex, AStartNumber: Integer);
begin
  inherited Create(AControl, ALevels, ALevelIndex);
  FStartNumber := AStartNumber;
end;

{ TdxBulletedListFormController }

procedure TdxBulletedListFormController.ApplyChanges;
var
  I: Integer;
begin
  for I := 0 to FEditedLevels.Count - 1 do
    TdxListLevel(FSourceLevels[I]).CopyFrom(TdxListLevel(FEditedLevels[I]));
end;

procedure TdxBulletedListFormController.AssignControllerIndentValues(AAlign, AIndent: Variant);
var
  AAlignValue: Integer;
  AIndentValue: Integer;
  AProbableFirstLineIndent: Integer;
begin
  if VarIsSoftNull(AAlign) or VarIsSoftNull(AIndent) then
    Exit;
  AAlignValue := AAlign;
  AIndentValue := AIndent;

  LeftIndent := AIndentValue;
  AProbableFirstLineIndent := AIndentValue - AAlignValue;
  if AProbableFirstLineIndent > 0 then
  begin
    FirstLineIndentType := TdxParagraphFirstLineIndent.Hanging;
    FirstLineIndent := AProbableFirstLineIndent;
  end
  else
    if AProbableFirstLineIndent < 0 then
    begin
      FirstLineIndentType := TdxParagraphFirstLineIndent.Indented;
      FirstLineIndent := -AProbableFirstLineIndent;
    end
    else
    begin
      FirstLineIndentType := TdxParagraphFirstLineIndent.None;
      FirstLineIndent := 0;
    end;
end;

constructor TdxBulletedListFormController.Create(
  const AControllerParameters: TdxBaseNumberingListFormControllerParameters);
begin
  inherited Create;
  FSourceLevels := AControllerParameters.Levels;
  FEditedLevelIndex := AControllerParameters.FLevelIndex;
  InitializeController;
end;

function TdxBulletedListFormController.CreateEditedLevels: TdxListLevelCollection;
var
  I: Integer;
  ALevel: TdxListLevel;
begin
  Result := TdxListLevelCollection.Create;
  for I := 0 to FSourceLevels.Count - 1 do
  begin
    ALevel := TdxListLevel.Create(FSourceLevels[I].DocumentModel);
    ALevel.CopyFrom(TdxListLevel(FSourceLevels[i]));
    Result.Add(ALevel);
  end;
end;

destructor TdxBulletedListFormController.Destroy;
begin
  FEditedLevels.Free;
  inherited Destroy;
end;

function TdxBulletedListFormController.GetCharacterProperties: TdxCharacterProperties;
begin
  Result := EditedLevel.CharacterProperties;
end;

function TdxBulletedListFormController.GetDisplayFormat: string;
begin
  Result := EditedLevel.ListLevelProperties.DisplayFormatString;
end;

function TdxBulletedListFormController.GetEditedLevel: TdxListLevel;
begin
  Result := TdxListLevel(FEditedLevels[FEditedLevelIndex]);
end;

function TdxBulletedListFormController.GetFirstLineIndent: Integer;
begin
  Result := EditedLevel.FirstLineIndent;
end;

function TdxBulletedListFormController.GetFirstLineIndentType: TdxParagraphFirstLineIndent;
begin
  Result := EditedLevel.FirstLineIndentType;
end;

function TdxBulletedListFormController.GetLeftIndent: Integer;
begin
  Result := EditedLevel.LeftIndent;
end;

procedure TdxBulletedListFormController.InitializeController;
begin
  FEditedLevels := CreateEditedLevels;
end;

procedure TdxBulletedListFormController.SetDisplayFormat(const Value: string);
begin
  EditedLevel.ListLevelProperties.DisplayFormatString := Value;
end;

procedure TdxBulletedListFormController.SetFirstLineIndent(const Value: Integer);
begin
  EditedLevel.FirstLineIndent := Value;
end;

procedure TdxBulletedListFormController.SetFirstLineIndentType(const Value: TdxParagraphFirstLineIndent);
begin
  EditedLevel.FirstLineIndentType := Value;
end;

procedure TdxBulletedListFormController.SetLeftIndent(const Value: Integer);
begin
  EditedLevel.LeftIndent := Value;
end;

{ TdxSimpleNumberingListController }

constructor TdxSimpleNumberingListController.Create(
  const AControllerParameters: TdxSimpleNumberingListFormControllerParameters);
begin
  inherited Create(AControllerParameters);
  Start := AControllerParameters.StartNumber;
  EditedLevelIndex := AControllerParameters.LevelIndex;
end;

function TdxSimpleNumberingListController.GetAlignment: TdxListNumberAlignment;
begin
  Result := EditedLevel.ListLevelProperties.Alignment;
end;

function TdxSimpleNumberingListController.GetFormat: TdxNumberingFormat;
begin
  Result := EditedLevel.ListLevelProperties.Format;
end;

function TdxSimpleNumberingListController.GetStart: Integer;
begin
  Result := EditedLevel.ListLevelProperties.Start;
end;

procedure TdxSimpleNumberingListController.SetAlignment(const Value: TdxListNumberAlignment);
begin
  EditedLevel.ListLevelProperties.Alignment := Value;
end;

procedure TdxSimpleNumberingListController.SetFormat(const Value: TdxNumberingFormat);
begin
  EditedLevel.ListLevelProperties.Format := Value;
end;

procedure TdxSimpleNumberingListController.SetStart(const Value: Integer);
begin
  EditedLevel.ListLevelProperties.Start := Value;
end;

{ TdxMultiLevelNumberingListFormController }

function TdxMultiLevelNumberingListFormController.GetSeparator: Char;
begin
  Result := EditedLevel.ListLevelProperties.Separator;
end;

procedure TdxMultiLevelNumberingListFormController.SetSeparator(const Value: Char);
begin
  EditedLevel.ListLevelProperties.Separator := Value;
end;

{ TdxNumberingListFormController }

constructor TdxNumberingListFormController.Create(const AControllerParameters: TdxNumberingListFormControllerParameters);
begin
  inherited Create;
  FControl := AControllerParameters.Control;
  FParagraphs := AControllerParameters.Paragraphs;
  FApplyScope := TdxNumberingListApplyScope.ToSelectedText;
  FNewListType := TdxNumberingType.Bullet;
  InitializeController;
end;

destructor TdxNumberingListFormController.Destroy;
begin
  FreeAndNil(FNoneList);
  FreeAndNil(FFakeDocumentModel);
  inherited Destroy;
end;

function TdxNumberingListFormController.GetDocumentModel: TdxDocumentModel;
begin
  Result := FControl.InnerControl.DocumentModel;
end;

procedure TdxNumberingListFormController.InitializeController;
begin
  FSelectedParagraphsListIndex := GetSelectedParagraphsListIndex;
  if FSelectedParagraphsListIndex >= 0 then
    FLevelIndex := Paragraphs.First.GetListLevelIndex;
end;

function TdxNumberingListFormController.GetSelectedParagraphsListIndex: TdxNumberingListIndex;
begin
  if not AreThereParagraphsInList or AreThereParagraphsInDifferentLists then
    Result := NumberingListIndexListIndexNotSetted
  else
    Result := GetFirstNumberingListIndex;
end;

function TdxNumberingListFormController.GetSelectedAbstractNumberingList: TdxAbstractNumberingList;
var
  AListIndex: TdxNumberingListIndex;
  AList: TdxNumberingList;
begin
  if AreThereParagraphsInDifferentLists then
    Exit(nil);
  if not AreThereParagraphsInList then
    Exit(NoneList);
  AListIndex := GetFirstNumberingListIndex;
  AList := DocumentModel.NumberingLists[AListIndex];
  Result := AList.AbstractNumberingList;
end;

function TdxNumberingListFormController.CreateAbstractNumberingListCopy(ATargetModel: TdxDocumentModel; ASourceList: TdxAbstractNumberingList): TdxAbstractNumberingList;
begin
  Result := TdxAbstractNumberingList.Create(ATargetModel);
  Result.CopyFrom(ASourceList);
  ATargetModel.AddAbstractNumberingListUsingHistory(Result);
end;

procedure TdxNumberingListFormController.ApplyChanges;
begin
  FControl.BeginUpdate;
  DocumentModel.BeginUpdate;
  try
    ApplyChangesCore;
  finally
    DocumentModel.EndUpdate;
    FControl.EndUpdate;
  end;
end;

procedure TdxNumberingListFormController.ApplyChangesCore;
begin
  if NewAbstractList = NoneList then
    RemoveNumberingList
  else
    case ApplyScope of
      TdxNumberingListApplyScope.ToSelectedText:
        ApplyChangesToSelectedText;
      TdxNumberingListApplyScope.RestartNumbering:
        ApplyChangesRestartNumbering;
      TdxNumberingListApplyScope.ModifyCurrentList:
        ApplyChangesModifyCurrentList;
      TdxNumberingListApplyScope.ContinuePreviousList:
        ApplyChangesContinuePreviousList;
    end;
end;

procedure TdxNumberingListFormController.ApplyChangesRestartNumbering;
var
  ACalculator: TdxNumberingListIndexCalculator;
  ANewNumberingListIndex, AFirstSelectedParagraphListIndex: TdxNumberingListIndex;
  ANewNumberingList: TdxNumberingList;
  ALevelCount, I: Integer;
begin
  if not AreThereParagraphsInList or AreThereParagraphsInDifferentLists then
    Exit;
  ACalculator := DocumentModel.CommandsCreationStrategy.CreateNumberingListIndexCalculator(DocumentModel, NewListType);
  try
    ANewNumberingListIndex := ACalculator.CreateNewList(NewAbstractList);
    ANewNumberingList := DocumentModel.NumberingLists[ANewNumberingListIndex];
    AFirstSelectedParagraphListIndex := GetFirstNumberingListIndex;
    if NewListType = TdxNumberingType.MultiLevel then
      ALevelCount := NewAbstractList.Levels.Count
    else
      ALevelCount := 1;
    for I := 0 to ALevelCount - 1 do
      ModifyNumberingListLevel(ANewNumberingList, I);
    ANewNumberingList.Levels[LevelIndex].ListLevelProperties.Start := NewAbstractList.Levels[LevelIndex].ListLevelProperties.Start;
    UpdateParagraphsDown(AFirstSelectedParagraphListIndex, ANewNumberingListIndex);
  finally
    ACalculator.Free;
  end;
end;

procedure TdxNumberingListFormController.ApplyChangesModifyCurrentList;
var
  ANumberingListIndex: Integer;
  AList: TdxNumberingList;
  ALevelCount, I: Integer;
begin
  if not AreThereParagraphsInList or AreThereParagraphsInDifferentLists then
    Exit;
  ANumberingListIndex := GetFirstNumberingListIndex;
  AList := DocumentModel.NumberingLists[ANumberingListIndex];
  ALevelCount := NewAbstractList.Levels.Count;
  for I := 0 to ALevelCount - 1 do
    ModifyNumberingListLevel(AList, I);
end;

procedure TdxNumberingListFormController.ApplyChangesContinuePreviousList;
begin
  if not AreThereParagraphsInList or AreThereParagraphsInDifferentLists then
    Exit;
  UpdateParagraphsDown(GetFirstNumberingListIndex, NewListIndex);
end;

function TdxNumberingListFormController.AreThereParagraphsInList: Boolean;
var
  AParagraph: TdxParagraph;
  I: Integer;
begin
  Result := False;
  for I := 0 to Paragraphs.Count - 1 do
  begin
    AParagraph := Paragraphs[I];
    if AParagraph.IsInList then
      Exit(True);
  end;
end;

function TdxNumberingListFormController.AreThereParagraphsInDifferentLists: Boolean;
var
  APrevNumbListIndex: TdxNumberingListIndex;
  AParagraph: TdxParagraph;
  I: Integer;
begin
  APrevNumbListIndex := NumberingListIndexListIndexNotSetted;
  for I := 0 to Paragraphs.Count - 1 do
  begin
    AParagraph := Paragraphs[I];
    if not AParagraph.IsInList then
      Continue;
    if APrevNumbListIndex = NumberingListIndexListIndexNotSetted then
    begin
      APrevNumbListIndex := AParagraph.NumberingListIndex;
      Continue;
    end;
    if APrevNumbListIndex <> AParagraph.NumberingListIndex then
      Exit(True);
  end;
  Result := False;
end;

function TdxNumberingListFormController.GetFirstNumberingListIndex: TdxNumberingListIndex;
var
  AParagraph: TdxParagraph;
  I: Integer;
begin
  for I := 0 to Paragraphs.Count - 1 do
  begin
    AParagraph := Paragraphs[I];
    if AParagraph.IsInList then
      Exit(AParagraph.GetNumberingListIndex);
  end;
  Result := NumberingListIndexListIndexNotSetted;
end;

function TdxNumberingListFormController.GetNoneList: TdxAbstractNumberingList;
begin
  if FNoneList = nil then
  begin
    FFakeDocumentModel := TdxDocumentModel.Create;
    FNoneList := TdxAbstractNumberingList.Create(FFakeDocumentModel);
  end;
  Result := FNoneList;
end;

procedure TdxNumberingListFormController.UpdateParagraphsDown(ANumberingListIndex: TdxNumberingListIndex; ANewListIndex: TdxNumberingListIndex);
var
  AParagraph: TdxParagraph;
  AFirstSelectedParagraph: TdxParagraph;
  AParagraphs: TdxParagraphCollection;
  AFirstSelectedParagraphIndex, I: Integer;
begin
  AFirstSelectedParagraph := Paragraphs.First;
  AParagraphs := AFirstSelectedParagraph.PieceTable.Paragraphs;
  AFirstSelectedParagraphIndex := AFirstSelectedParagraph.Index;
  for I := AFirstSelectedParagraphIndex to AParagraphs.Last.Index do
  begin
    AParagraph := AParagraphs[I];
    if not AParagraph.IsInList or (AParagraph.NumberingListIndex <> ANumberingListIndex) then
      Continue;
    if (NewListType = TdxNumberingType.Simple) and (AParagraph.GetOwnListLevelIndex <> LevelIndex) then
      Continue;
    ModifyParagraph(AParagraph, ANewListIndex);
  end;
end;

procedure TdxNumberingListFormController.RemoveNumberingList;
var
  ACommand: TdxDeleteNumerationFromParagraphCommand;
begin
  ACommand := TdxDeleteNumerationFromParagraphCommand.Create(FControl);
  try
    ACommand.Execute;
  finally
    ACommand.Free;
  end;
end;

procedure TdxNumberingListFormController.ModifyParagraph(AParagraph: TdxParagraph; AListIndex: TdxNumberingListIndex);
var
  ALevelIndex: Integer;
begin
  if (AListIndex < 0) or (AParagraph.NumberingListIndex < 0) then
    Exit;
  if NewListType = TdxNumberingType.MultiLevel then
    ALevelIndex := AParagraph.GetOwnListLevelIndex
  else
    ALevelIndex := LevelIndex;
  AParagraph.PieceTable.RemoveParagraphFromList(AParagraph.Index);
  AParagraph.PieceTable.AddParagraphToList(AParagraph.Index, AListIndex, ALevelIndex);
end;

procedure TdxNumberingListFormController.ModifyNumberingListLevel(ATargetList: TdxNumberingList;
  ALevelIndex: Integer);
var
  ACharacterFormattingInfo: TdxCharacterFormattingInfo;
  ACharacterFormattingOptions: TdxCharacterFormattingOptions;
  ACharacterMergedProperties: TdxMergedCharacterProperties;
  AParagraphFormattingInfo: TdxParagraphFormattingInfo;
  AParagraphFormattingOptions: TdxParagraphFormattingOptions;
  AParagraphMergedProperties: TdxMergedParagraphProperties;
begin
  ACharacterFormattingInfo := NewAbstractList.Levels[ALevelIndex].CharacterProperties.Info.Info;
  ACharacterFormattingOptions := NewAbstractList.Levels[ALevelIndex].CharacterProperties.Info.Options;
  ACharacterMergedProperties := TdxMergedCharacterProperties.Create(ACharacterFormattingInfo, ACharacterFormattingOptions);
  try
    ATargetList.Levels[ALevelIndex].CharacterProperties.CopyFrom(ACharacterMergedProperties);
  finally
    ACharacterMergedProperties.Free;
  end;
  AParagraphFormattingInfo := NewAbstractList.Levels[ALevelIndex].ParagraphProperties.Info.Info;
  AParagraphFormattingOptions := NewAbstractList.Levels[ALevelIndex].ParagraphProperties.Info.Options;
  AParagraphMergedProperties := TdxMergedParagraphProperties.Create(AParagraphFormattingInfo, AParagraphFormattingOptions);
  try
    ATargetList.Levels[ALevelIndex].ParagraphProperties.CopyFrom(AParagraphMergedProperties);
  finally
    AParagraphMergedProperties.Free;
  end;
  ATargetList.Levels[ALevelIndex].ListLevelProperties.Alignment := NewAbstractList.Levels[ALevelIndex].ListLevelProperties.Alignment;
  ATargetList.Levels[ALevelIndex].ListLevelProperties.Format := NewAbstractList.Levels[ALevelIndex].ListLevelProperties.Format;
end;

procedure TdxNumberingListFormController.ApplyChangesToSelectedText;
var
  AMode: TdxInsertListMode;
  ACommand: TdxNumberingListCommandBase;
  ANewAbstractList: TdxAbstractNumberingList;
begin
  AMode := CalculateListInsertionMode;
  if NewAbstractList = NoneList then
    ACommand := TdxDeleteNumerationFromParagraphCommand.Create(FControl)
  else
  begin
    if DocumentModel <> NewAbstractList.DocumentModel then
      ANewAbstractList := CreateAbstractNumberingListCopy(DocumentModel, NewAbstractList)
    else
      ANewAbstractList := NewAbstractList;
    ACommand := TdxInsertListFormCommand.Create(FControl, ANewAbstractList, LevelIndex, AMode);
  end;
  try
    ACommand.Execute;
  finally
    ACommand.Free;
  end;
end;

function TdxNumberingListFormController.CalculateListInsertionMode: TdxInsertListMode;
var
  ASelectedList: TdxAbstractNumberingList;
  ASelectedListType, ANewListType: TdxNumberingType;
begin
  ASelectedList := GetSelectedAbstractNumberingList;
  if (ASelectedList = nil) or (ASelectedList = NoneList) then
  begin
    if TdxNumberingListHelper.GetListType(NewAbstractList) = TdxNumberingType.MultiLevel then
      Result := TdxInsertListMode.CalculateLevelIndexByIndent
    else
      Result := TdxInsertListMode.ChangeLevelIndex;
  end
  else
  begin
    ASelectedListType := TdxNumberingListHelper.GetListType(ASelectedList);
    ANewListType := TdxNumberingListHelper.GetListType(NewAbstractList);
    if (ASelectedListType = ANewListType) or (ANewListType = TdxNumberingType.Bullet) then
      Result := TdxInsertListMode.KeepLevelIndex
    else
      if ANewListType = TdxNumberingType.Simple then
        Result := TdxInsertListMode.ChangeLevelIndex
      else
        Result := TdxInsertListMode.KeepLevelIndex;
  end;
end;

end.
