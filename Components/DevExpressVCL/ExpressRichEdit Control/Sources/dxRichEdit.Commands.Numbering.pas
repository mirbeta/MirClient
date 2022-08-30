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

unit dxRichEdit.Commands.Numbering;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, Generics.Defaults, Generics.Collections,

  dxGenerics,
  dxRichEdit.Commands.IDs,
  dxRichEdit.Commands,
  dxRichEdit.Commands.ChangeProperties,
  dxRichEdit.Commands.Insert,
  dxRichEdit.View.Core,
  dxRichEdit.Utils.Types,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.Boxes.Core,
  dxRichEdit.DocumentModel.Boxes,
  dxRichEdit.DocumentModel.Boxes.Simple,
  dxRichEdit.DocumentModel.ParagraphFormatting,
  dxRichEdit.DocumentModel.Numbering,
  dxRichEdit.DocumentModel.NumberingFormatting,
  dxRichEdit.DocumentModel.PieceTableModifiers,
  dxRichEdit.DocumentModel.Section,
  dxRichEdit.DocumentModel.SectionFormatting;

type
  { TdxParagraphLayoutPosition }

  TdxParagraphLayoutPosition = class
  private
    FPage: TdxPage;
    FPageArea: TdxPageArea;
    FColumn: TdxColumn;
    FRow: TdxRow;
    FBoxIndex: Integer;
    FControl: IdxRichEditControl;
  protected
    procedure GetParagraphLayoutPosition(AParagraph: TdxParagraph);
    function LookupPage(APages: TdxPageCollection; ARunIndex: TdxRunIndex): TdxPage; virtual;
    function LookupPageArea(APage: TdxPage; AParagraph: TdxParagraph; ARunIndex: TdxRunIndex): TdxPageArea; virtual;
    function LookupColumn(AColumns: TdxColumnCollection; ARunIndex: TdxRunIndex): TdxColumn; virtual;
    function LookupRow(ARows: TdxRowCollection; ARunIndex: TdxRunIndex): TdxRow; virtual;
    function LookupBoxIndex(ABoxes: TdxBoxCollection; ARunIndex: TdxRunIndex): Integer; virtual;
    function LookupBoxCore(ABoxes: TdxBoxListBase; AComparer: TdxBoxComparable): TdxBoxBase; virtual;
    function LookupBoxIndexCore(ABoxes: TdxBoxListBase; AComparer: TdxBoxComparable): Integer; virtual;

    property Page: TdxPage read FPage;
    property PageArea: TdxPageArea read FPageArea;
    property Column: TdxColumn read FColumn;
    property Row: TdxRow read FRow;
    property BoxIndex: Integer read FBoxIndex;
  public
    constructor Create(const AControl: IdxRichEditControl);
  end;

  { TdxParagraphInterval }

  TdxParagraphInterval = class(TInterfacedObject)
  private
    FStart: TdxParagraphIndex;
    FEnd: TdxParagraphIndex;
  public
    constructor Create(AStart, AEnd: TdxParagraphIndex);
    function CompareTo(const AOther: TdxParagraphInterval): Integer;

    property Start: TdxParagraphIndex read FStart;
    property &End: TdxParagraphIndex read FEnd;
  end;
  TdxParagraphIntervalList = class(TdxList<TdxParagraphInterval>);

  { TdxNumberingListCommandBase }

  TdxNumberingListCommandBase = class abstract(TdxInsertObjectCommandBase)
  private
    FListIndex: TdxNumberingListIndex;
    FParagraphsLevelIndex: TdxIntegersDictionary;
    FParagraphLayoutPosition: TObjectDictionary<TdxParagraphIndex, TdxParagraphLayoutPosition>;
    FContinueList: Boolean;
    function GetNumberingListsTemplate: TdxAbstractNumberingListCollection;
    procedure ClearCore;
  protected
    procedure ModifyModel; override;
    procedure ModifyParagraphs(AParagraphIntervals: TdxParagraphIntervalList); virtual;

    procedure ModifyParagraphsCore(AStartParagraphIndex, AEndParagraphIndex: TdxParagraphIndex); virtual; abstract;

    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
    function GetLevelType(AParagraph: TdxParagraph): TdxNumberingType;
    function GetBox(ABoxIndex: Integer; ABoxes: TdxBoxCollection): TdxBox; virtual;
    function EqualLeftIndent: Boolean;
    function GetRowParentIndent(ARow: TdxRow; AParagraphLayoutPosition: TdxParagraphLayoutPosition): Integer; virtual;
    function GetRowIndent(ARow: TdxRow; AParagraphLayoutPosition: TdxParagraphLayoutPosition): Integer; virtual;
    function CreateAndUpdateParagraphLayoutPosition(AParagraph: TdxParagraph): TdxParagraphLayoutPosition;
    function CreateParagraphLayoutPositionCore(AParagraph: TdxParagraph): TdxParagraphLayoutPosition;
    function GetUpdateCaretPositionBeforeChangeSelectionDetailsLevel: TdxDocumentLayoutDetailsLevel; override;

    property ParagraphsLevelIndex: TdxIntegersDictionary read FParagraphsLevelIndex write FParagraphsLevelIndex;
    property ParagraphLayoutPositionIndex: TObjectDictionary<TdxParagraphIndex, TdxParagraphLayoutPosition> read FParagraphLayoutPosition write FParagraphLayoutPosition;
    property ListIndex: TdxNumberingListIndex read FListIndex write FListIndex;
    property EqualIndent: Boolean read EqualLeftIndent;
    property ContinueList: Boolean read FContinueList write FContinueList;
    property NumberingListsTemplate: TdxAbstractNumberingListCollection read GetNumberingListsTemplate;
  public
    constructor Create(const AControl: IdxRichEditControl); override;
    destructor Destroy; override;
  end;

  { TdxInsertMultiLevelListCommand }

  TdxInsertMultiLevelListCommand = class(TdxNumberingListCommandBase)
  private
    FNestingLevel: Integer;
    FTemplateListIndex: TdxAbstractNumberingListIndex;
    function GetActualNumberingPosition(const AListLevel: IdxOverrideListLevel): Integer;
  protected
    function GetNumberingListType: TdxNumberingType; virtual;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
    procedure ModifyParagraphs(AParagraphIntervals: TdxParagraphIntervalList); override;
    procedure ModifyParagraphsCore(AStartParagraphIndex, AEndParagraphIndex: TdxParagraphIndex); override;
    function ShouldAddNumberingListToParagraphs(AParagraphIntervals: TdxParagraphIntervalList): Boolean; virtual;
    function GetNumberingListIndex: TdxAbstractNumberingListIndex; virtual;
    function GetListIndex(AParagraphIntervals: TdxParagraphIntervalList): TdxNumberingListIndex; virtual;
    function AreAllParagraphsHasValidNumberingListType: Boolean; virtual;
    function GetEndSelectedParagraphIndex: TdxParagraphIndex; virtual;
    function GetEqualsTemplateListIndex(ANumberingList: TdxNumberingList): TdxAbstractNumberingListIndex;
    procedure FillParagraphsLevelIndex(AParagraphIntervals: TdxParagraphIntervalList); virtual;
    function CalculateParagraphListLevel(AParagraph: TdxParagraphBase; ALeftIndent: Integer): Integer; virtual;
    function HaveCurrentNumberingListType: Boolean;
    function IsStartWhiteSpaceParagraph(AStartParagraphIndex, AEndParagraphIndex: TdxParagraphIndex; AParagraph: TdxParagraphBase): Boolean;
    procedure InsertNumberingRangeForParagraph(AParagraphIntervals: TdxParagraphIntervalList);
    procedure AddNumberingListToParagraph(AParagraph: TdxParagraph; AListIndex: TdxNumberingListIndex; AListLevelIndex: Integer); virtual;
    procedure AddParagraphLayoutPositionIndex(AParagraphIntervals: TdxParagraphIntervalList); virtual;
    procedure AddParagraphLayoutPositionIndexCore(AParagraph: TdxParagraph); virtual;
    procedure DeleteWhiteSpaceBox(ABoxIndex: Integer; ABoxes: TdxBoxCollection); virtual;
    function GetLevelByCurrentIndent(ALeftIndent: Integer; AListIndex: TdxNumberingListIndex): Integer; virtual;
    procedure DeleteOldNumberingListRange(AIndex: TdxParagraphIndex); virtual;
    procedure AssignLevelsIndents(AIndex: TdxParagraphIndex);
    procedure AssignLevelsIndentsCore(ALevels: TdxListLevelCollection; AParagraph: TdxParagraph);
    procedure StoreOriginalLevelLeftIndent(const ALevel: IdxListLevel); virtual;

    property NumberingListType: TdxNumberingType read GetNumberingListType;
    property NestingLevel: Integer read FNestingLevel write FNestingLevel;
  public
    constructor Create(const AControl: IdxRichEditControl;
      ATemplateListIndex: TdxAbstractNumberingListIndex = AbstractNumberingListIndexInvalidValue); reintroduce;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
    procedure ChangeSelection(ASelection: TdxSelection); override;
  end;

  { TdxInsertSimpleListCommand }

  TdxInsertSimpleListCommand = class(TdxInsertMultiLevelListCommand)
  protected
    function GetNumberingListType: TdxNumberingType; override;
    procedure FillParagraphsLevelIndex(AParagraphIntervals: TdxParagraphIntervalList); override;
    procedure StoreOriginalLevelLeftIndent(const ALevel: IdxListLevel); override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
  public
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxInsertBulletListCommand }

  TdxInsertBulletListCommand = class(TdxInsertSimpleListCommand)
  protected
    function GetNumberingListType: TdxNumberingType; override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
  public
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxDeleteNumerationFromParagraphCommand }

  TdxDeleteNumerationFromParagraphCommand = class(TdxNumberingListCommandBase)
  protected
    procedure ModifyParagraphsCore(AStartParagraphIndex, AEndParagraphIndex: TdxParagraphIndex); override;
    procedure DeleteNumerationFromParagraph(AParagraph: TdxParagraph); virtual;
  public
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
    procedure ChangeSelection(ASelection: TdxSelection); override;
  end;

  { TdxToggleListCommandBase }

  TdxToggleListCommandBase = class abstract(TdxRichEditMenuItemSimpleCommand)
  private
    FDeleteNumerationCommand: TdxDeleteNumerationFromParagraphCommand;
  protected
    function GetInsertNumerationCommand: TdxNumberingListCommandBase; virtual; abstract;
    procedure ExecuteCore; override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;

    property InsertNumerationCommand: TdxNumberingListCommandBase read GetInsertNumerationCommand;
  public
    constructor Create(const AControl: IdxRichEditControl); override;
    destructor Destroy; override;

    procedure UpdateUIState(const AState: IdxCommandUIState); override;
    procedure ForceExecute(const AState: IdxCommandUIState); override;

    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxToggleBulletedListCommand }

  TdxToggleBulletedListCommand = class(TdxToggleListCommandBase)
  private
    FInsertNumerationCommand: TdxInsertBulletListCommand;
  protected
    function GetInsertNumerationCommand: TdxNumberingListCommandBase; override;
  public
    constructor Create(const AControl: IdxRichEditControl); override;
    destructor Destroy; override;

    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxToggleSimpleNumberingListCommand }

  TdxToggleSimpleNumberingListCommand = class(TdxToggleListCommandBase)
  private
    FInsertNumerationCommand: TdxInsertSimpleListCommand;
  protected
    function GetInsertNumerationCommand: TdxNumberingListCommandBase; override;
  public
    constructor Create(const AControl: IdxRichEditControl); override;
    destructor Destroy; override;

    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxToggleMultiLevelListCommand }

  TdxToggleMultiLevelListCommand = class(TdxToggleListCommandBase)
  private
    FInsertNumerationCommand: TdxInsertMultiLevelListCommand;
  protected
    function GetInsertNumerationCommand: TdxNumberingListCommandBase; override;
  public
    constructor Create(const AControl: IdxRichEditControl); override;
    destructor Destroy; override;

    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxIncrementNumerationFromParagraphCommand }

  TdxIncrementNumerationFromParagraphCommand = class(TdxNumberingListCommandBase)
  protected
    procedure ModifyParagraphsCore(AStartParagraphIndex, AEndParagraphIndex: TdxParagraphIndex); override;
    procedure IncrementNumerationFromParagraph(AParagraph: TdxParagraph; AListLevelIndex: Integer;
      ANumberingListIndex: TdxNumberingListIndex); virtual;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
    procedure ChangeSelection(ASelection: TdxSelection); override;
  end;

  { TdxNumerationParagraphIndentCommandBase }

  TdxNumerationParagraphIndentCommandBase = class abstract(TdxChangeParagraphIndentCommandBase<Integer>)
  protected
    procedure ModifyDocumentModelCore(const AState: IdxCommandUIState); override;
    procedure AssignNewIndentCore(AAbstractNumberingList: TdxAbstractNumberingList;
      ACurrentNumberingOrBulletPosition: Integer); virtual; abstract;
    function CalculateCurrentBulletOrNumberingPosition(ALeftIndent, AFirstLineIndent: Integer;
      AFirstLineIndentType: TdxParagraphFirstLineIndent): Integer; virtual;
    function CalculateLeftIndentDelta(ATaregetNumerationOrBulletPosition, ACurrentLeftIndent, AFirstLineIndent: Integer;
      AFirstLineIndentType: TdxParagraphFirstLineIndent): Integer; virtual;
    procedure AssignNumberingListLeftIndentModifier(AAbstractNumberingList: TdxAbstractNumberingList;
      ATargetNumberingOrBulletPosition: Integer); virtual;
  end;

  { TdxIncrementNumerationParagraphIndentCommand }

  TdxIncrementNumerationParagraphIndentCommand = class(TdxNumerationParagraphIndentCommandBase)
  protected
    procedure AssignNewIndentCore(AAbstractNumberingList: TdxAbstractNumberingList; ACurrentNumberingOrBulletPosition: Integer); override;
    function CreateModifier(const AState: IdxCommandUIState): TdxParagraphPropertyModifier<Integer>; override;
    procedure FillTabsList; override;
  public
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxDecrementNumerationFromParagraphCommand }

  TdxDecrementNumerationFromParagraphCommand = class(TdxNumberingListCommandBase)
  protected
    procedure ModifyParagraphsCore(AStartParagraphIndex, AEndParagraphIndex: TdxParagraphIndex); override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
    procedure ChangeSelection(ASelection: TdxSelection); override;
  end;

  { TdxDecrementNumerationParagraphIndentCommand }

  TdxDecrementNumerationParagraphIndentCommand = class(TdxNumerationParagraphIndentCommandBase)
  protected
    procedure AssignNewIndentCore(AAbstractNumberingList: TdxAbstractNumberingList;
      ACurrentNumberingOrBulletPosition: Integer); override;
    function CreateModifier(const AState: IdxCommandUIState): TdxParagraphPropertyModifier<Integer>; override;
  public
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxPageRunComparer }

  TdxPageRunComparer = class(TdxBoxComparable)
  private
    FIndex: TdxRunIndex;
    function IsRunBeforePage(APage: TdxPage): Boolean;
    function IsRunAfterPage(APage: TdxPage): Boolean;
  public
    constructor Create(AIndex: TdxRunIndex);
    function CompareTo(const Page: TdxBoxBase): Integer; override;
  end;

  { TdxColumnRunComparer }

  TdxColumnRunComparer = class(TdxBoxComparable)
  private
    FIndex: TdxRunIndex;
    function IsRunBeforeColumn(AColumn: TdxColumn): Boolean;
    function IsRunAfterColumn(AColumn: TdxColumn): Boolean;
  public
    constructor Create(AIndex: TdxRunIndex);
    function CompareTo(const Column: TdxBoxBase): Integer; override;
  end;

  { TdxPageAreaRunComparer }

  TdxPageAreaRunComparer = class(TdxBoxComparable)
  private
    FIndex: TdxRunIndex;
    function IsRunBeforePageArea(APageArea: TdxPageArea): Boolean;
    function IsRunAfterPageArea(APageArea: TdxPageArea): Boolean;
  public
    constructor Create(AIndex: TdxRunIndex);
    function CompareTo(const PageArea: TdxBoxBase): Integer; override;
  end;

  { TdxRowRunComparer }

  TdxRowRunComparer = class(TdxBoxComparable)
  private
    FIndex: TdxRunIndex;
    function IsRunBeforeRow(ARow: TdxRow): Boolean;
    function IsRunAfterRow(ARow: TdxRow): Boolean;
  public
    constructor Create(AIndex: TdxRunIndex);
    function CompareTo(const Row: TdxBoxBase): Integer; override;
  end;

  { TdxInsertListFormCommand }

  TdxInsertListMode = (
    KeepLevelIndex,
    CalculateLevelIndexByIndent,
    ChangeLevelIndex);

  TdxInsertListFormCommand = class(TdxInsertMultiLevelListCommand)
  strict private
    FList: TdxAbstractNumberingList;
    FLevelIndex: Integer;
    FMode: TdxInsertListMode;
  protected
    function ShouldAddNumberingListToParagraphs(AParagraphIntervals: TdxParagraphIntervalList): Boolean; override;
    function GetListIndex(AParagraphIntervals: TdxParagraphIntervalList): TdxNumberingListIndex; override;
    procedure AddNumberingListToParagraph(AParagraph: TdxParagraph; AListIndex: TdxNumberingListIndex;
      AListLevelIndex: Integer); override;
    function CalculateParagraphListLevel(AParagraph: TdxParagraphBase; ALeftIndent: Integer): Integer; override;
  public
    constructor Create(const AControl: IdxRichEditControl; AList: TdxAbstractNumberingList;
      ALevelIndex: Integer; AMode: TdxInsertListMode); reintroduce;
  end;

  { TdxHeaderFooterParagraphLayoutPosition }

  TdxHeaderFooterParagraphLayoutPosition = class(TdxParagraphLayoutPosition)
  strict private
    FPage: TdxPage;
    FArea: TdxPageArea;
  protected
    function LookupPage(APages: TdxPageCollection; ARunIndex: TdxRunIndex): TdxPage; override;
    function LookupPageArea(APage: TdxPage; AParagraph: TdxParagraph; ARunIndex: TdxRunIndex): TdxPageArea; override;
  public
    constructor Create(const AControl: IdxRichEditControl; APage: TdxPage; AArea: TdxPageArea);
  end;

  { TdxChangeSectionLineNumberingCommand }

  TdxChangeSectionLineNumberingCommand = class(TdxRichEditMenuItemSimpleCommand)
  protected
    procedure ExecuteCore; override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxSetPredefinedSectionLineNumberingCommand }

  TdxSetPredefinedSectionLineNumberingCommand = class abstract(TdxToggleChangeSectionFormattingCommandBase<TdxLineNumberingRestart>)
  protected
    function GetLineNumberingRestart: TdxLineNumberingRestart; virtual; abstract;
    function CreateModifier(const AState: IdxCommandUIState): TdxSectionPropertyModifier<TdxLineNumberingRestart>; override;
    function IsCheckedValue(AValue: TdxLineNumberingRestart): Boolean; override;

    property LineNumberingRestart: TdxLineNumberingRestart read GetLineNumberingRestart;
  end;

  { TdxSetSectionLineNumberingNoneCommand }

  TdxSetSectionLineNumberingNoneCommand = class(TdxSetPredefinedSectionLineNumberingCommand)
  protected
    function GetLineNumberingRestart: TdxLineNumberingRestart; override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxSetSectionLineNumberingContinuousCommand }

  TdxSetSectionLineNumberingContinuousCommand = class(TdxSetPredefinedSectionLineNumberingCommand)
  protected
    function GetLineNumberingRestart: TdxLineNumberingRestart; override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxSetSectionLineNumberingRestartNewPageCommand }

  TdxSetSectionLineNumberingRestartNewPageCommand = class(TdxSetPredefinedSectionLineNumberingCommand)
  protected
    function GetLineNumberingRestart: TdxLineNumberingRestart; override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxSetSectionLineNumberingRestartNewSectionCommand }

  TdxSetSectionLineNumberingRestartNewSectionCommand = class(TdxSetPredefinedSectionLineNumberingCommand)
  protected
    function GetLineNumberingRestart: TdxLineNumberingRestart; override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

implementation

uses
  Classes, Contnrs, Math, dxCore,

  dxRichEdit.Commands.Images,
  dxRichEdit.Utils.Exceptions,
  dxRichEdit.Commands.Strs,
  dxRichEdit.DocumentModel.Tables,
  dxRichEdit.DocumentModel.TableFormatting;

type
  { TdxBoxRunComparer }

  TdxBoxRunComparer = class(TdxBoxComparable)
  private
    FIndex: TdxRunIndex;
    function IsRunBeforeBox(ABox: TdxBoxBase): Boolean;
    function IsRunAfterBox(ABox: TdxBoxBase): Boolean;
  public
    constructor Create(AIndex: TdxRunIndex);

    function CompareTo(const ABox: TdxBoxBase): Integer; override;
  end;

{ TdxBoxRunComparer }

function TdxBoxRunComparer.CompareTo(const ABox: TdxBoxBase): Integer;
begin
  if IsRunBeforeBox(ABox) then
    Result := 1
  else
    if IsRunAfterBox(ABox) then
      Result := -1
    else
      Result := 0;
end;

constructor TdxBoxRunComparer.Create(AIndex: TdxRunIndex);
begin
  inherited Create;
  FIndex := AIndex;
end;

function TdxBoxRunComparer.IsRunAfterBox(ABox: TdxBoxBase): Boolean;
begin
  Result := FIndex > ABox.StartPos.RunIndex;
end;

function TdxBoxRunComparer.IsRunBeforeBox(ABox: TdxBoxBase): Boolean;
begin
  Result := (FIndex < ABox.StartPos.RunIndex) or ((FIndex = ABox.StartPos.RunIndex) and (ABox.StartPos.Offset > 0));
end;

{ TdxParagraphLayoutPosition }

constructor TdxParagraphLayoutPosition.Create(const AControl: IdxRichEditControl);
begin
  inherited Create;
  Assert(AControl <> nil);
  FControl := AControl;
end;

procedure TdxParagraphLayoutPosition.GetParagraphLayoutPosition(AParagraph: TdxParagraph);
var
  ARunIndex: TdxRunIndex;
begin
  Assert(AParagraph <> nil);

  ARunIndex := AParagraph.FirstRunIndex;
  FPage := LookupPage(FControl.InnerControl.ActiveView.DocumentLayout.Pages, ARunIndex);
  FPageArea := LookupPageArea(FPage, AParagraph, ARunIndex);
  FColumn := LookupColumn(FPageArea.Columns, ARunIndex);
  FRow := LookupRow(FColumn.Rows, ARunIndex);
  FBoxIndex := LookupBoxIndex(FRow.Boxes, ARunIndex);
end;

function TdxParagraphLayoutPosition.LookupBoxCore(ABoxes: TdxBoxListBase; AComparer: TdxBoxComparable): TdxBoxBase;
var
  ABoxIndex: Integer;
begin
  ABoxIndex := LookupBoxIndexCore(ABoxes, AComparer);
  Result := ABoxes[ABoxIndex];
end;

function TdxParagraphLayoutPosition.LookupBoxIndex(ABoxes: TdxBoxCollection; ARunIndex: TdxRunIndex): Integer;
var
  AComparer: TdxBoxRunComparer;
begin
  AComparer := TdxBoxRunComparer.Create(ARunIndex);
  try
    Result := LookupBoxIndexCore(ABoxes, AComparer);
  finally
    AComparer.Free;
  end;
end;

function TdxParagraphLayoutPosition.LookupBoxIndexCore(ABoxes: TdxBoxListBase; AComparer: TdxBoxComparable): Integer;
begin
  TdxAlgorithms1<TdxBoxBase>.BinarySearch(ABoxes, AComparer, Result);
  Assert(Result < ABoxes.Count);
end;

function TdxParagraphLayoutPosition.LookupColumn(AColumns: TdxColumnCollection; ARunIndex: TdxRunIndex): TdxColumn;
var
  AComparer: TdxColumnRunComparer;
begin
  AComparer := TdxColumnRunComparer.Create(ARunIndex);
  try
    Result := TdxColumn(LookupBoxCore(AColumns, AComparer));
  finally
    AComparer.Free;
  end;
end;

function TdxParagraphLayoutPosition.LookupPage(APages: TdxPageCollection; ARunIndex: TdxRunIndex): TdxPage;
var
  AComparer: TdxPageRunComparer;
begin
  AComparer := TdxPageRunComparer.Create(ARunIndex);
  try
    Result := TdxPage(LookupBoxCore(APages, AComparer));
  finally
    AComparer.Free;
  end;
end;

function TdxParagraphLayoutPosition.LookupPageArea(APage: TdxPage; AParagraph: TdxParagraph;
  ARunIndex: TdxRunIndex): TdxPageArea;
var
  AComparer: TdxPageAreaRunComparer;
begin
  if (APage.Header <> nil) and (APage.Header.PieceTable = AParagraph.PieceTable) then
    Exit(APage.Header);
  if (APage.Footer <> nil) and (APage.Footer.PieceTable = AParagraph.PieceTable) then
    Exit(APage.Footer);
  AComparer := TdxPageAreaRunComparer.Create(ARunIndex);
  try
    Result := TdxPageArea(LookupBoxCore(Page.Areas, AComparer));
  finally
    AComparer.Free;
  end;
end;

function TdxParagraphLayoutPosition.LookupRow(ARows: TdxRowCollection; ARunIndex: TdxRunIndex): TdxRow;
var
  AComparer: TdxRowRunComparer;
begin
  AComparer := TdxRowRunComparer.Create(ARunIndex);
  try
    Result := TdxRow(LookupBoxCore(ARows, AComparer));
  finally
    AComparer.Free;
  end;
end;

{ TdxParagraphInterval }

constructor TdxParagraphInterval.Create(AStart, AEnd: TdxParagraphIndex);
begin
  inherited Create;
  FStart := AStart;
  FEnd := AEnd;
end;

function TdxParagraphInterval.CompareTo(const AOther: TdxParagraphInterval): Integer;
begin
  Result := FStart - AOther.Start;
end;

{ TdxNumberingListCommandBase }

constructor TdxNumberingListCommandBase.Create(const AControl: IdxRichEditControl);
begin
  inherited Create(AControl);
  FParagraphsLevelIndex := TdxIntegersDictionary.Create;
  FParagraphLayoutPosition := TObjectDictionary<TdxParagraphIndex, TdxParagraphLayoutPosition>.Create([doOwnsValues]);
end;

destructor TdxNumberingListCommandBase.Destroy;
begin
  FParagraphLayoutPosition.Free;
  FParagraphsLevelIndex.Free;
  inherited Destroy;
end;

function TdxNumberingListCommandBase.CreateAndUpdateParagraphLayoutPosition(
  AParagraph: TdxParagraph): TdxParagraphLayoutPosition;
var
  AParagraphPosition: TdxParagraphLayoutPosition;
begin
  AParagraphPosition := CreateParagraphLayoutPositionCore(AParagraph);
  AParagraphPosition.GetParagraphLayoutPosition(AParagraph);
  Result := AParagraphPosition;
end;

function TdxNumberingListCommandBase.CreateParagraphLayoutPositionCore(
  AParagraph: TdxParagraph): TdxParagraphLayoutPosition;
begin
  if AParagraph.PieceTable.IsMain then
    Result := TdxParagraphLayoutPosition.Create(RichEditControl)
  else
  begin
    Assert(CaretPosition.LayoutPosition.DetailsLevel >= UpdateCaretPositionBeforeChangeSelectionDetailsLevel);
    Result := TdxHeaderFooterParagraphLayoutPosition.Create(RichEditControl, CaretPosition.LayoutPosition.Page, CaretPosition.LayoutPosition.PageArea);
  end;
end;

function TdxNumberingListCommandBase.EqualLeftIndent: Boolean;
var
  ABox: TdxBox;
  AList: TdxNumberingList;
  APosition: TdxParagraphLayoutPosition;
  AMinLeftIndent, AMaxLeftIndent, ABoxLeft, ALeftIndent: Integer;
begin
  if ParagraphLayoutPositionIndex.Count = 1 then
    Exit(True);
  AMinLeftIndent := MaxInt;
  AMaxLeftIndent := MinInt;
  for APosition in ParagraphLayoutPositionIndex.Values do
  begin
    ABox := GetBox(APosition.BoxIndex, APosition.Row.Boxes);
    ABoxLeft := ABox.Bounds.Left - GetRowParentIndent(APosition.Row, APosition);
    AMinLeftIndent := Math.Min(AMinLeftIndent, ABoxLeft);
    AMaxLeftIndent := Math.Max(AMaxLeftIndent, ABoxLeft);
  end;
  AList := DocumentModel.NumberingLists[ListIndex];
  ALeftIndent := (AList.Levels[1] as IdxParagraphProperties).LeftIndent - (AList.Levels[0] as IdxParagraphProperties).LeftIndent;
  Result := AMaxLeftIndent - AMinLeftIndent < ALeftIndent;
end;

function TdxNumberingListCommandBase.GetBox(ABoxIndex: Integer; ABoxes: TdxBoxCollection): TdxBox;
var
  I: Integer;
begin
  for I := ABoxIndex to ABoxes.Count - 1 do
    if ABoxes[I].IsNotWhiteSpaceBox then
      Exit(ABoxes[I]);
  Result := ABoxes[ABoxIndex];
end;

function TdxNumberingListCommandBase.GetLevelType(AParagraph: TdxParagraph): TdxNumberingType;
var
  ANumberingList: TdxNumberingList;
begin
  ANumberingList := DocumentModel.NumberingLists[AParagraph.GetNumberingListIndex];
  Result := TdxNumberingListHelper.GetLevelType(ANumberingList, AParagraph.GetListLevelIndex);
end;

function TdxNumberingListCommandBase.GetNumberingListsTemplate: TdxAbstractNumberingListCollection;
begin
  Result := RichEditControl.InnerControl.DocumentModelTemplate.AbstractNumberingLists;
end;

procedure TdxNumberingListCommandBase.ClearCore;
begin
  ParagraphLayoutPositionIndex.Clear;
  ParagraphsLevelIndex.Clear;
end;

function TdxNumberingListCommandBase.GetRowIndent(ARow: TdxRow;
  AParagraphLayoutPosition: TdxParagraphLayoutPosition): Integer;
begin
  Result := ARow.Bounds.Left - GetRowParentIndent(ARow, AParagraphLayoutPosition);
end;

function TdxNumberingListCommandBase.GetRowParentIndent(ARow: TdxRow;
  AParagraphLayoutPosition: TdxParagraphLayoutPosition): Integer;
begin
  if ARow is TdxTableCellRow then
    Result := TdxTableCellRow(ARow).CellViewInfo.TextLeft + AParagraphLayoutPosition.Column.Bounds.Left
  else
    Result := AParagraphLayoutPosition.Column.Bounds.Left;
end;

function TdxNumberingListCommandBase.GetUpdateCaretPositionBeforeChangeSelectionDetailsLevel: TdxDocumentLayoutDetailsLevel;
begin
  Result := TdxDocumentLayoutDetailsLevel.PageArea;
end;

procedure TdxNumberingListCommandBase.ModifyModel;
var
  I: Integer;
  ASelectionItem: TdxSelectionItem;
  AIntervals: TdxParagraphIntervalList;
  ASelectionItems: TdxSelectionItemList;
  AStartParagraphIndex, AEndParagraphIndex: TdxParagraphIndex;
begin
  ASelectionItems := DocumentModel.Selection.Items;
  AIntervals := TdxParagraphIntervalList.Create(True);
  try
    for I := 0 to ASelectionItems.Count - 1 do
    begin
      ASelectionItem := ASelectionItems[I];
      AStartParagraphIndex := ASelectionItem.GetStartParagraphIndex;
      AEndParagraphIndex := ASelectionItem.GetEndParagraphIndex;
      AIntervals.Add(TdxParagraphInterval.Create(AStartParagraphIndex, AEndParagraphIndex));
    end;
    AIntervals.Sort;
    ModifyParagraphs(AIntervals);
  finally
    AIntervals.Free;
  end;
  ClearCore;
end;

procedure TdxNumberingListCommandBase.ModifyParagraphs(AParagraphIntervals: TdxParagraphIntervalList);
var
  I: Integer;
  AParagraphInterval: TdxParagraphInterval;
begin
  for I := 0 to AParagraphIntervals.Count - 1 do
  begin
    AParagraphInterval := AParagraphIntervals[I];
    ModifyParagraphsCore(AParagraphInterval.Start, AParagraphInterval.&End);
  end;
end;

procedure TdxNumberingListCommandBase.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  AState.Enabled := IsContentEditable;
  AState.Visible := True;
  ApplyDocumentProtectionToSelectedParagraphs(AState);
end;

{ TdxInsertMultiLevelListCommand }

procedure TdxInsertMultiLevelListCommand.AddNumberingListToParagraph(AParagraph: TdxParagraph;
  AListIndex: TdxNumberingListIndex; AListLevelIndex: Integer);
var
  ACell: TdxTableCell;
begin
  ACell := AParagraph.GetCell;
  if (ACell <> nil) and (ACell.VerticalMerging = TdxMergingState.Continue) then
    Exit;
  ActivePieceTable.AddNumberingListToParagraph(AParagraph, AListIndex, AListLevelIndex);
  AParagraph.ParagraphProperties.ResetUse([TdxUsedParagraphFormattingOption.UseFirstLineIndent,
    TdxUsedParagraphFormattingOption.UseLeftIndent]);
end;

procedure TdxInsertMultiLevelListCommand.AddParagraphLayoutPositionIndex(
  AParagraphIntervals: TdxParagraphIntervalList);
var
  I: Integer;
  AParagraphInterval: TdxParagraphInterval;
  J, AStartParagraphIndex, AEndParagraphIndex: TdxParagraphIndex;
begin
  for I := 0 to AParagraphIntervals.Count - 1 do
  begin
    AParagraphInterval := AParagraphIntervals[I];
    AStartParagraphIndex := AParagraphInterval.Start;
    AEndParagraphIndex := AParagraphInterval.&End;
    for J := AStartParagraphIndex to AEndParagraphIndex do
      AddParagraphLayoutPositionIndexCore(ActivePieceTable.Paragraphs[J]);
  end;
end;

procedure TdxInsertMultiLevelListCommand.AddParagraphLayoutPositionIndexCore(AParagraph: TdxParagraph);
var
  AParagraphPosition: TdxParagraphLayoutPosition;
begin
  AParagraphPosition := CreateAndUpdateParagraphLayoutPosition(AParagraph);
  ParagraphLayoutPositionIndex.Add(AParagraph.Index, AParagraphPosition);
end;

function TdxInsertMultiLevelListCommand.AreAllParagraphsHasValidNumberingListType: Boolean;
var
  AParagraph: TdxParagraph;
  I, AStartParagraphIndex, AEndParagraphIndex: TdxParagraphIndex;
begin
  AStartParagraphIndex := DocumentModel.Selection.Interval.NormalizedStart.ParagraphIndex;
  AEndParagraphIndex := GetEndSelectedParagraphIndex;

  for I := AStartParagraphIndex to AEndParagraphIndex do
  begin
    AParagraph := ActivePieceTable.Paragraphs[I];
    if not AParagraph.IsInList or (GetLevelType(AParagraph) <> NumberingListType) then
      Exit(False);
  end;
  Result := True;
end;

procedure TdxInsertMultiLevelListCommand.AssignLevelsIndents(AIndex: TdxParagraphIndex);
var
  AParagraph: TdxParagraph;
  AFirstLevelLeftIndent: Integer;
  ALevels: TdxListLevelCollection;
begin
  AParagraph := ActivePieceTable.Paragraphs[AIndex];
  ALevels := DocumentModel.NumberingLists[ListIndex].Levels;
  AFirstLevelLeftIndent := (ALevels[0] as IdxParagraphProperties).LeftIndent;
  if AParagraph.LeftIndent <> AFirstLevelLeftIndent then
    AssignLevelsIndentsCore(ALevels, AParagraph);
end;

procedure TdxInsertMultiLevelListCommand.AssignLevelsIndentsCore(ALevels: TdxListLevelCollection;
  AParagraph: TdxParagraph);
var
  I: Integer;
  ALevel: IdxListLevel;
  ALevelParagraphProperties: TdxParagraphProperties;
begin
  for I := 0 to ALevels.Count - 1 do
  begin
    ALevel := ALevels[I];
    ALevelParagraphProperties := ALevel.ParagraphProperties;
    StoreOriginalLevelLeftIndent(ALevel);
    ALevel.ListLevelProperties.OriginalLeftIndent := ALevelParagraphProperties.LeftIndent;
    ALevelParagraphProperties.LeftIndent := ALevelParagraphProperties.LeftIndent + AParagraph.LeftIndent;
  end;
end;

function TdxInsertMultiLevelListCommand.CalculateParagraphListLevel(AParagraph: TdxParagraphBase;
  ALeftIndent: Integer): Integer;
begin
  if NestingLevel <> 0 then
    Result := NestingLevel
  else
    Result := GetLevelByCurrentIndent(DocumentModel.ToDocumentLayoutUnitConverter.ToModelUnits(ALeftIndent), ListIndex);
end;

procedure TdxInsertMultiLevelListCommand.ChangeSelection(ASelection: TdxSelection);
begin
end;

constructor TdxInsertMultiLevelListCommand.Create(const AControl: IdxRichEditControl;
  ATemplateListIndex: TdxAbstractNumberingListIndex);
begin
  inherited Create(AControl);
  FTemplateListIndex := ATemplateListIndex;
end;

procedure TdxInsertMultiLevelListCommand.DeleteOldNumberingListRange(AIndex: TdxParagraphIndex);
begin
  if ActivePieceTable.Paragraphs[AIndex].IsInList then
    ActivePieceTable.RemoveNumberingFromParagraph(ActivePieceTable.Paragraphs[AIndex]);
end;

procedure TdxInsertMultiLevelListCommand.DeleteWhiteSpaceBox(ABoxIndex: Integer; ABoxes: TdxBoxCollection);
var
  I, ALength: Integer;
  ALogPosition: TdxDocumentLogPosition;
begin
  ALength := 0;
  I := ABoxIndex;
  while (I < ABoxes.Count) and not ABoxes[I].IsNotWhiteSpaceBox do
  begin
    Inc(ALength, ABoxes[I].EndPos.Offset - ABoxes[I].StartPos.Offset + 1);
    Inc(I);
  end;
  ALogPosition := ActivePieceTable.Runs[ABoxes[ABoxIndex].StartPos.RunIndex].Paragraph.LogPosition;
  ActivePieceTable.DeleteContent(ALogPosition, ALength, False);
end;

procedure TdxInsertMultiLevelListCommand.FillParagraphsLevelIndex(AParagraphIntervals: TdxParagraphIntervalList);
var
  ARow: TdxRow;
  ABox: TdxBox;
  AParagraph: TdxParagraphBase;
  AParagraphInterval: TdxParagraphInterval;
  I, AStartParagraphIndex, AEndParagraphIndex: TdxParagraphIndex;
  AParagraphIntervalIndex, ABoxIndex, ARowIndent, AWhiteSpaceBoxLength, ALeftIndent: Integer;
begin
  for AParagraphIntervalIndex := 0 to AParagraphIntervals.Count - 1 do
  begin
    AParagraphInterval := AParagraphIntervals[AParagraphIntervalIndex];
    AStartParagraphIndex := AParagraphInterval.Start;
    AEndParagraphIndex := AParagraphInterval.&End;
    for I := AStartParagraphIndex to AEndParagraphIndex do
    begin
      ARow := ParagraphLayoutPositionIndex[I].Row;
      ABoxIndex := ParagraphLayoutPositionIndex[I].BoxIndex;
      ABox := GetBox(ABoxIndex, ARow.Boxes);
      AParagraph := ActivePieceTable.Runs[ABox.StartPos.RunIndex].Paragraph;
      if (ABox is TdxParagraphMarkBox) and not IsStartWhiteSpaceParagraph(AStartParagraphIndex, AEndParagraphIndex, AParagraph) then
        Continue;
      ARowIndent := GetRowIndent(ARow, ParagraphLayoutPositionIndex[I]);
      AWhiteSpaceBoxLength := ABox.Bounds.Left - ARow.Boxes[ABoxIndex].Bounds.Left;
      ALeftIndent := ARowIndent + AWhiteSpaceBoxLength;
      ParagraphsLevelIndex.Add(I, CalculateParagraphListLevel(AParagraph, ALeftIndent));
    end;
  end;
end;

function TdxInsertMultiLevelListCommand.GetActualNumberingPosition(const AListLevel: IdxOverrideListLevel): Integer;
var
  AParagraphProperties: IdxParagraphProperties;
begin
  AParagraphProperties := AListLevel as IdxParagraphProperties;
  if AParagraphProperties.FirstLineIndentType = TdxParagraphFirstLineIndent.Hanging then
    Result := AParagraphProperties.LeftIndent - AParagraphProperties.FirstLineIndent
  else
    Result := AParagraphProperties.LeftIndent;
end;

class function TdxInsertMultiLevelListCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertMultilevelListDescription);
end;

function TdxInsertMultiLevelListCommand.GetEndSelectedParagraphIndex: TdxParagraphIndex;
var
  AParagraph: TdxParagraph;
  APieceTable: TdxPieceTable;
  ASelectionEnd: TdxDocumentModelPosition;
  ALastSelectedParagraphIndex, ALastParagraphIndex: TdxParagraphIndex;
begin
  ASelectionEnd := DocumentModel.Selection.Interval.NormalizedEnd^;
  ALastSelectedParagraphIndex := ASelectionEnd.ParagraphIndex;
  APieceTable := TdxPieceTable(ASelectionEnd.PieceTable);
  ALastParagraphIndex := APieceTable.Paragraphs.Count - 1;
  if DocumentModel.Selection.Length = 0 then
  begin
    if ALastSelectedParagraphIndex > ALastParagraphIndex then
      Exit(ALastParagraphIndex)
    else
      Exit(ALastSelectedParagraphIndex);
  end;
  if ALastSelectedParagraphIndex > ALastParagraphIndex then
    Exit(ALastParagraphIndex);
  AParagraph := APieceTable.Paragraphs[ALastSelectedParagraphIndex];
  if AParagraph.LogPosition < ASelectionEnd.LogPosition then
    Result := ALastSelectedParagraphIndex
  else
    Result := ALastSelectedParagraphIndex - 1;
end;

function TdxInsertMultiLevelListCommand.GetEqualsTemplateListIndex(
  ANumberingList: TdxNumberingList): TdxAbstractNumberingListIndex;
var
  I, ACount: TdxAbstractNumberingListIndex;
  AAbstractNumberingList: TdxAbstractNumberingList;
  ANumberingListTemplate: TdxAbstractNumberingList;
begin
  ACount := NumberingListsTemplate.Count;
  AAbstractNumberingList := ANumberingList.AbstractNumberingList;
  for I := 0 to ACount - 1 do
  begin
    ANumberingListTemplate := NumberingListsTemplate[I];
    if AAbstractNumberingList.IsEqual(ANumberingListTemplate) then
      Exit(I);
  end;
  Result := -1;
end;

function TdxInsertMultiLevelListCommand.GetLevelByCurrentIndent(ALeftIndent: Integer;
  AListIndex: TdxNumberingListIndex): Integer;
var
  ALevels: TdxListLevelCollection;
  I, AActualNumberingPosition: Integer;
begin
  ALevels := DocumentModel.NumberingLists[AListIndex].Levels;
  for I := 0 to ALevels.Count - 1 do
  begin
    AActualNumberingPosition := GetActualNumberingPosition(ALevels[I] as IdxOverrideListLevel);
    if ALeftIndent <= AActualNumberingPosition then
      Exit(I);
  end;
  Result := ALevels.Count - 1;
end;

function TdxInsertMultiLevelListCommand.GetListIndex(
  AParagraphIntervals: TdxParagraphIntervalList): TdxNumberingListIndex;
var
  ACalculator: TdxNumberingListIndexCalculator;
  AFirstInterval, ALastInterval: TdxParagraphInterval;
  AStartParagraphIndex, AEndParagraphIndex: TdxParagraphIndex;
begin
  AFirstInterval := AParagraphIntervals[0];
  AStartParagraphIndex := AFirstInterval.Start;
  ALastInterval := AParagraphIntervals[AParagraphIntervals.Count - 1];
  AEndParagraphIndex := ALastInterval.&End;
  ACalculator := DocumentModel.CommandsCreationStrategy.CreateNumberingListIndexCalculator(DocumentModel, NumberingListType);
  try
    Result := ACalculator.GetListIndex(AStartParagraphIndex, AEndParagraphIndex);
    if Result >= 0 then
    begin
      ContinueList := ACalculator.ContinueList;
      NestingLevel := ACalculator.NestingLevel;
      Exit;
    end;
    Result := ACalculator.CreateNewList(NumberingListsTemplate[FTemplateListIndex]);
  finally
    ACalculator.Free;
  end;
end;

class function TdxInsertMultiLevelListCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertMultilevelListMenuCaption);
end;

function TdxInsertMultiLevelListCommand.GetNumberingListIndex: TdxAbstractNumberingListIndex;
begin
  Result := TdxNumberingListHelper.GetAbstractListIndexByType(NumberingListsTemplate, NumberingListType);
end;

function TdxInsertMultiLevelListCommand.GetNumberingListType: TdxNumberingType;
begin
  Result := TdxNumberingType.MultiLevel;
end;

function TdxInsertMultiLevelListCommand.HaveCurrentNumberingListType: Boolean;
begin
  Result := DocumentModel.AbstractNumberingLists.HasListOfType(NumberingListType);
end;

procedure TdxInsertMultiLevelListCommand.InsertNumberingRangeForParagraph(
  AParagraphIntervals: TdxParagraphIntervalList);
var
  AParagraph: TdxParagraph;
  AParagraphIntervalIndex: Integer;
  AParagraphInterval: TdxParagraphInterval;
  I, AKey, AStartParagraphIndex, AEndParagraphIndex: TdxParagraphIndex;
begin
  for AParagraphIntervalIndex := AParagraphIntervals.Count - 1 downto 0 do
  begin
    AParagraphInterval := AParagraphIntervals[AParagraphIntervalIndex];
    AStartParagraphIndex := AParagraphInterval.Start;
    AEndParagraphIndex := AParagraphInterval.&End;
    for I := AEndParagraphIndex downto AStartParagraphIndex do
    begin
      DeleteOldNumberingListRange(I);
      DeleteWhiteSpaceBox(ParagraphLayoutPositionIndex[I].BoxIndex, ParagraphLayoutPositionIndex[I].Row.Boxes);
    end;
  end;
  for AKey in ParagraphsLevelIndex.Keys do
  begin
    AParagraph := ActivePieceTable.Paragraphs[AKey];
    AddNumberingListToParagraph(AParagraph, ListIndex, ParagraphsLevelIndex[AKey]);
  end;
end;

function TdxInsertMultiLevelListCommand.IsStartWhiteSpaceParagraph(AStartParagraphIndex,
  AEndParagraphIndex: TdxParagraphIndex; AParagraph: TdxParagraphBase): Boolean;
begin
  if (AStartParagraphIndex <> AEndParagraphIndex) and (AParagraph.Index <> AStartParagraphIndex) then
    Result := False
  else
    if (AParagraph.Length <= 1) and (AStartParagraphIndex <> AEndParagraphIndex) then
      Result := False
    else
      Result := True;
end;

procedure TdxInsertMultiLevelListCommand.ModifyParagraphs(AParagraphIntervals: TdxParagraphIntervalList);
begin
  if FTemplateListIndex = AbstractNumberingListIndexInvalidValue then
    FTemplateListIndex := GetNumberingListIndex;
  ActiveView.EnsureFormattingCompleteForSelection;
  if ShouldAddNumberingListToParagraphs(AParagraphIntervals) then
  begin
    ListIndex := GetListIndex(AParagraphIntervals);

    AddParagraphLayoutPositionIndex(AParagraphIntervals);
    FillParagraphsLevelIndex(AParagraphIntervals);
    InsertNumberingRangeForParagraph(AParagraphIntervals);
  end;
end;

procedure TdxInsertMultiLevelListCommand.ModifyParagraphsCore(AStartParagraphIndex,
  AEndParagraphIndex: TdxParagraphIndex);
begin
end;

function TdxInsertMultiLevelListCommand.ShouldAddNumberingListToParagraphs(
  AParagraphIntervals: TdxParagraphIntervalList): Boolean;
var
  AParagraph: TdxParagraph;
  ANumberingList: TdxNumberingList;
  ANumberingListIndex: TdxNumberingListIndex;
begin
  AParagraph := ActivePieceTable.Paragraphs[AParagraphIntervals[0].Start];
  if not AParagraph.IsInList then
    Exit(True);
  ANumberingListIndex := AParagraph.GetNumberingListIndex;
  ANumberingList := DocumentModel.NumberingLists[ANumberingListIndex];
  Result := FTemplateListIndex <> GetEqualsTemplateListIndex(ANumberingList);
end;

procedure TdxInsertMultiLevelListCommand.StoreOriginalLevelLeftIndent(const ALevel: IdxListLevel);
begin
  ALevel.ListLevelProperties.OriginalLeftIndent := 0;
end;

procedure TdxInsertMultiLevelListCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  inherited UpdateUIStateCore(AState);
  ApplyCommandRestrictionOnEditableControl(AState, DocumentModel.DocumentCapabilities.Numbering.MultiLevel, AState.Enabled);
  ApplyDocumentProtectionToSelectedParagraphs(AState);
  AState.Checked := AreAllParagraphsHasValidNumberingListType;
end;

{ TdxInsertSimpleListCommand }

procedure TdxInsertSimpleListCommand.FillParagraphsLevelIndex(AParagraphIntervals: TdxParagraphIntervalList);
var
  AParagraphIntervalIndex: Integer;
  I, AStartParagraphIndex, AEndParagraphIndex: TdxParagraphIndex;
  AParagraphInterval: TdxParagraphInterval;
begin
  if EqualIndent then
  begin
    for AParagraphIntervalIndex := 0 to AParagraphIntervals.Count - 1 do
    begin
      AParagraphInterval := AParagraphIntervals[AParagraphIntervalIndex];
      AStartParagraphIndex := AParagraphInterval.Start;
      AEndParagraphIndex := AParagraphInterval.&End;

      for I := AStartParagraphIndex to AEndParagraphIndex do
        ParagraphsLevelIndex.Add(I, 0);
      if not ContinueList then
        AssignLevelsIndents(AStartParagraphIndex);
    end;
  end
  else
    inherited FillParagraphsLevelIndex(AParagraphIntervals);
end;

class function TdxInsertSimpleListCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertSimpleListDescription);
end;

class function TdxInsertSimpleListCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertSimpleListMenuCaption);
end;

function TdxInsertSimpleListCommand.GetNumberingListType: TdxNumberingType;
begin
  Result := TdxNumberingType.Simple;
end;

procedure TdxInsertSimpleListCommand.StoreOriginalLevelLeftIndent(const ALevel: IdxListLevel);
begin
  ALevel.ListLevelProperties.OriginalLeftIndent := ALevel.ParagraphProperties.LeftIndent;
end;

procedure TdxInsertSimpleListCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  inherited UpdateUIStateCore(AState);
  ApplyCommandRestrictionOnEditableControl(AState, DocumentModel.DocumentCapabilities.Numbering.Simple);
  ApplyDocumentProtectionToSelectedParagraphs(AState);
end;

{ TdxInsertBulletListCommand }

class function TdxInsertBulletListCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertBulletListDescription);
end;

class function TdxInsertBulletListCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertBulletListMenuCaption);
end;

function TdxInsertBulletListCommand.GetNumberingListType: TdxNumberingType;
begin
  Result := TdxNumberingType.Bullet;
end;

procedure TdxInsertBulletListCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  inherited UpdateUIStateCore(AState);
  ApplyCommandRestrictionOnEditableControl(AState, DocumentModel.DocumentCapabilities.Numbering.Bulleted);
  ApplyDocumentProtectionToSelectedParagraphs(AState);
end;

{ TdxDeleteNumerationFromParagraphCommand }

procedure TdxDeleteNumerationFromParagraphCommand.ChangeSelection(ASelection: TdxSelection);
begin
end;

procedure TdxDeleteNumerationFromParagraphCommand.DeleteNumerationFromParagraph(AParagraph: TdxParagraph);
begin
  ActivePieceTable.DeleteNumerationFromParagraph(AParagraph);
end;

class function TdxDeleteNumerationFromParagraphCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandDeleteNumerationFromParagraphDescription);
end;

class function TdxDeleteNumerationFromParagraphCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandDeleteNumerationFromParagraphMenuCaption);
end;

procedure TdxDeleteNumerationFromParagraphCommand.ModifyParagraphsCore(AStartParagraphIndex,
  AEndParagraphIndex: TdxParagraphIndex);
var
  I: TdxParagraphIndex;
  AParagraph: TdxParagraph;
begin
  for I := AStartParagraphIndex to AEndParagraphIndex do
  begin
    AParagraph := ActivePieceTable.Paragraphs[I];
    if AParagraph.IsInList then
      DeleteNumerationFromParagraph(AParagraph);
  end;
end;

{ TdxToggleListCommandBase }

constructor TdxToggleListCommandBase.Create(const AControl: IdxRichEditControl);
begin
  inherited Create(AControl);
  FDeleteNumerationCommand := TdxDeleteNumerationFromParagraphCommand.Create(AControl);
end;

destructor TdxToggleListCommandBase.Destroy;
begin
  FreeAndNil(FDeleteNumerationCommand);
  inherited Destroy;
end;

procedure TdxToggleListCommandBase.ExecuteCore;
begin
end;

procedure TdxToggleListCommandBase.ForceExecute(const AState: IdxCommandUIState);
begin
  NotifyBeginCommandExecution(AState);
  try
    if AState.Checked then
      FDeleteNumerationCommand.ForceExecute(AState)
    else
      InsertNumerationCommand.ForceExecute(AState);
  finally
    NotifyEndCommandExecution(AState);
  end;
end;

class function TdxToggleListCommandBase.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertSimpleListDescription);
end;

class function TdxToggleListCommandBase.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertSimpleListMenuCaption);
end;

procedure TdxToggleListCommandBase.UpdateUIState(const AState: IdxCommandUIState);
begin
  InsertNumerationCommand.UpdateUIState(AState);
end;

procedure TdxToggleListCommandBase.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
end;

{ TdxToggleBulletedListCommand }

constructor TdxToggleBulletedListCommand.Create(const AControl: IdxRichEditControl);
begin
  inherited Create(AControl);
  FInsertNumerationCommand := TdxInsertBulletListCommand.Create(AControl);
end;

destructor TdxToggleBulletedListCommand.Destroy;
begin
  FreeAndNil(FInsertNumerationCommand);
  inherited Destroy;
end;

class function TdxToggleBulletedListCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertBulletListDescription);
end;

function TdxToggleBulletedListCommand.GetInsertNumerationCommand: TdxNumberingListCommandBase;
begin
  Result := FInsertNumerationCommand;
end;

class function TdxToggleBulletedListCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertBulletListMenuCaption);
end;

class function TdxToggleBulletedListCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ToggleBulletedListItem;
end;

class function TdxToggleBulletedListCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.ToggleBulletedList;
end;

{ TdxToggleSimpleNumberingListCommand }

constructor TdxToggleSimpleNumberingListCommand.Create(const AControl: IdxRichEditControl);
begin
  inherited Create(AControl);
  FInsertNumerationCommand := TdxInsertSimpleListCommand.Create(AControl);
end;

destructor TdxToggleSimpleNumberingListCommand.Destroy;
begin
  FreeAndNil(FInsertNumerationCommand);
  inherited Destroy;
end;

class function TdxToggleSimpleNumberingListCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertSimpleListDescription);
end;

function TdxToggleSimpleNumberingListCommand.GetInsertNumerationCommand: TdxNumberingListCommandBase;
begin
  Result := FInsertNumerationCommand;
end;

class function TdxToggleSimpleNumberingListCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertSimpleListMenuCaption);
end;

class function TdxToggleSimpleNumberingListCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ToggleNumberingListItem;
end;

class function TdxToggleSimpleNumberingListCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.ToggleSimpleNumberingList;
end;

{ TdxToggleMultiLevelListCommand }

constructor TdxToggleMultiLevelListCommand.Create(const AControl: IdxRichEditControl);
begin
  inherited Create(AControl);
  FInsertNumerationCommand := TdxInsertMultiLevelListCommand.Create(AControl);
end;

destructor TdxToggleMultiLevelListCommand.Destroy;
begin
  FInsertNumerationCommand.Free;
  inherited Destroy;
end;

class function TdxToggleMultiLevelListCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertMultilevelListDescription);
end;

function TdxToggleMultiLevelListCommand.GetInsertNumerationCommand: TdxNumberingListCommandBase;
begin
  Result := FInsertNumerationCommand;
end;

class function TdxToggleMultiLevelListCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertMultilevelListMenuCaption);
end;

class function TdxToggleMultiLevelListCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ToggleMultilevelListItem;
end;

class function TdxToggleMultiLevelListCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.ToggleMultiLevelList;
end;

{ TdxIncrementNumerationFromParagraphCommand }

procedure TdxIncrementNumerationFromParagraphCommand.ChangeSelection(ASelection: TdxSelection);
begin
end;

class function TdxIncrementNumerationFromParagraphCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandIncrementNumerationFromParagraphDescription);
end;

class function TdxIncrementNumerationFromParagraphCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandIncrementNumerationFromParagraphMenuCaption);
end;

class function TdxIncrementNumerationFromParagraphCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.IncrementNumerationFromParagraph;
end;

procedure TdxIncrementNumerationFromParagraphCommand.IncrementNumerationFromParagraph(AParagraph: TdxParagraph;
  AListLevelIndex: Integer; ANumberingListIndex: TdxNumberingListIndex);
begin
  AListLevelIndex := AListLevelIndex + 1;
  AParagraph.ParagraphProperties.ResetUse(
    [TdxUsedParagraphFormattingOption.UseFirstLineIndent, TdxUsedParagraphFormattingOption.UseLeftIndent]);
  ActivePieceTable.AddNumberingListToParagraph(AParagraph, ANumberingListIndex, AListLevelIndex);
end;

procedure TdxIncrementNumerationFromParagraphCommand.ModifyParagraphsCore(AStartParagraphIndex,
  AEndParagraphIndex: TdxParagraphIndex);
var
  AParagraph: TdxParagraph;
  I, AListLevelIndex: Integer;
  ANumberingListIndex: TdxNumberingListIndex;
begin
  for I := AStartParagraphIndex to AEndParagraphIndex do
  begin
    AParagraph := ActivePieceTable.Paragraphs[I];
    AListLevelIndex := AParagraph.GetListLevelIndex;
    if AParagraph.IsInList and (AListLevelIndex < 8) then
    begin
      ANumberingListIndex := AParagraph.GetNumberingListIndex;
      ActivePieceTable.RemoveNumberingFromParagraph(AParagraph);
      IncrementNumerationFromParagraph(AParagraph, AListLevelIndex, ANumberingListIndex);
    end;
  end;
end;

{ TdxNumerationParagraphIndentCommandBase }

procedure TdxNumerationParagraphIndentCommandBase.AssignNumberingListLeftIndentModifier(
  AAbstractNumberingList: TdxAbstractNumberingList; ATargetNumberingOrBulletPosition: Integer);
var
  ALevels: TdxListLevelCollection;
  ALevel, AFirstLevel: TdxListLevel;
  I, ADelta, ANewLeftIndent, AFirstLineLeftIndent: Integer;
begin
  ALevels := AAbstractNumberingList.Levels;
  AFirstLevel := TdxListLevel(ALevels[0]);
  ADelta := CalculateLeftIndentDelta(ATargetNumberingOrBulletPosition, AFirstLevel.LeftIndent,
    AFirstLevel.FirstLineIndent, AFirstLevel.FirstLineIndentType);
  for I := 0 to ALevels.Count - 1 do
  begin
    ALevel := TdxListLevel(ALevels[I]);
    ANewLeftIndent := ALevel.LeftIndent + ADelta;
    if ANewLeftIndent >= 0 then
    begin
      if ALevel.FirstLineIndentType = TdxParagraphFirstLineIndent.Hanging then
      begin
        AFirstLineLeftIndent := ANewLeftIndent - ALevel.FirstLineIndent;
        if AFirstLineLeftIndent < 0 then
          ANewLeftIndent := ANewLeftIndent - AFirstLineLeftIndent;
      end;
      if (I = 0) and (ALevel.LeftIndent = ANewLeftIndent) then
        Break;
      ALevel.LeftIndent := ANewLeftIndent;
    end
    else
      if I = 0 then
        Break;
  end;
end;

function TdxNumerationParagraphIndentCommandBase.CalculateCurrentBulletOrNumberingPosition(ALeftIndent,
  AFirstLineIndent: Integer; AFirstLineIndentType: TdxParagraphFirstLineIndent): Integer;
begin
  if AFirstLineIndentType = TdxParagraphFirstLineIndent.Hanging then
    Result := ALeftIndent - AFirstLineIndent
  else
    Result := ALeftIndent;
end;

function TdxNumerationParagraphIndentCommandBase.CalculateLeftIndentDelta(ATaregetNumerationOrBulletPosition,
  ACurrentLeftIndent, AFirstLineIndent: Integer; AFirstLineIndentType: TdxParagraphFirstLineIndent): Integer;
begin
  Result := ATaregetNumerationOrBulletPosition - CalculateCurrentBulletOrNumberingPosition(ACurrentLeftIndent,
    AFirstLineIndent, AFirstLineIndentType);
end;

procedure TdxNumerationParagraphIndentCommandBase.ModifyDocumentModelCore(const AState: IdxCommandUIState);
var
  AParagraph: TdxParagraph;
  AFirstLevel: TdxListLevel;
  ACurrentBulletOrNumberingPosition: Integer;
  ANumberingListIndex: TdxNumberingListIndex;
  AAbstractNumberingList: TdxAbstractNumberingList;
begin
  FillTabsList;
  AParagraph := ActivePieceTable.Paragraphs[StartParagraphIndex];
  ANumberingListIndex := AParagraph.GetNumberingListIndex;
  if ANumberingListIndex < 0 then
    Exit;
  AAbstractNumberingList := DocumentModel.NumberingLists[ANumberingListIndex].AbstractNumberingList;
  AFirstLevel := TdxListLevel(AAbstractNumberingList.Levels[0]);
  ACurrentBulletOrNumberingPosition := CalculateCurrentBulletOrNumberingPosition(AFirstLevel.LeftIndent,
    AFirstLevel.FirstLineIndent, AFirstLevel.FirstLineIndentType);
  AssignNewIndentCore(AAbstractNumberingList, ACurrentBulletOrNumberingPosition);
end;

{ TdxIncrementNumerationParagraphIndentCommand }

procedure TdxIncrementNumerationParagraphIndentCommand.AssignNewIndentCore(
  AAbstractNumberingList: TdxAbstractNumberingList; ACurrentNumberingOrBulletPosition: Integer);
var
  ANearestRightDefaultTab,ANearestRightTab: Integer;
begin
  ANearestRightDefaultTab := GetNearRightDefaultTab(ACurrentNumberingOrBulletPosition);
  ANearestRightTab := GetNearRightTab(ACurrentNumberingOrBulletPosition);
  if (ANearestRightDefaultTab < ANearestRightTab) or (ANearestRightTab = ACurrentNumberingOrBulletPosition) then
    AssignNumberingListLeftIndentModifier(AAbstractNumberingList, ANearestRightDefaultTab)
  else
    AssignNumberingListLeftIndentModifier(AAbstractNumberingList, ANearestRightTab);
end;

function TdxIncrementNumerationParagraphIndentCommand.CreateModifier(
  const AState: IdxCommandUIState): TdxParagraphPropertyModifier<Integer>;
begin
  TdxRichEditExceptions.ThrowInternalException;
  Result := nil;
end;

procedure TdxIncrementNumerationParagraphIndentCommand.FillTabsList;
var
  AParagraph: TdxParagraph;
begin
  AParagraph := ActivePieceTable.Paragraphs[StartParagraphIndex];
  if AParagraph.FirstLineIndentType = TdxParagraphFirstLineIndent.Hanging then
    TabsList.Add(AParagraph.LeftIndent + AParagraph.FirstLineIndent);
  inherited FillTabsList;
end;

class function TdxIncrementNumerationParagraphIndentCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandIncrementParagraphLeftIndentDescription);
end;

class function TdxIncrementNumerationParagraphIndentCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandIncrementParagraphLeftIndentMenuCaption);
end;

{ TdxDecrementNumerationFromParagraphCommand }

procedure TdxDecrementNumerationFromParagraphCommand.ChangeSelection(ASelection: TdxSelection);
begin
end;

class function TdxDecrementNumerationFromParagraphCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandDecrementNumerationFromParagraphDescription);
end;

class function TdxDecrementNumerationFromParagraphCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandDecrementNumerationFromParagraphMenuCaption);
end;

class function TdxDecrementNumerationFromParagraphCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.DecrementNumerationFromParagraph;
end;

procedure TdxDecrementNumerationFromParagraphCommand.ModifyParagraphsCore(AStartParagraphIndex,
  AEndParagraphIndex: TdxParagraphIndex);
var
  I: TdxParagraphIndex;
  AListLevelIndex: Integer;
  AParagraph: TdxParagraph;
  ANumberingListIndex: TdxNumberingListIndex;
begin
  for I := AStartParagraphIndex to AEndParagraphIndex do
  begin
    AParagraph := ActivePieceTable.Paragraphs[I];
    AListLevelIndex := AParagraph.GetListLevelIndex;
    if AParagraph.IsInList and (AListLevelIndex > 0) then
    begin
      ANumberingListIndex := AParagraph.GetNumberingListIndex;
      ActivePieceTable.RemoveNumberingFromParagraph(AParagraph);
      AListLevelIndex := AListLevelIndex - 1;
      AParagraph.ParagraphProperties.ResetUse(
        [TdxUsedParagraphFormattingOption.UseFirstLineIndent, TdxUsedParagraphFormattingOption.UseLeftIndent]);
      ActivePieceTable.AddNumberingListToParagraph(AParagraph, ANumberingListIndex, AListLevelIndex);
    end;
  end;
end;

{ TdxDecrementNumerationParagraphIndentCommand }

procedure TdxDecrementNumerationParagraphIndentCommand.AssignNewIndentCore(
  AAbstractNumberingList: TdxAbstractNumberingList; ACurrentNumberingOrBulletPosition: Integer);
var
  ANearLeftDefaultTab, ANearLeftTab: Integer;
begin
  ANearLeftDefaultTab := GetNearLeftDefaultTab(ACurrentNumberingOrBulletPosition);
  ANearLeftTab := GetNearLeftTab(ACurrentNumberingOrBulletPosition);

  if (ANearLeftDefaultTab > ANearLeftTab) or (ANearLeftTab = ACurrentNumberingOrBulletPosition) then
    AssignNumberingListLeftIndentModifier(AAbstractNumberingList, ANearLeftDefaultTab)
  else
    AssignNumberingListLeftIndentModifier(AAbstractNumberingList, ANearLeftTab);
end;

function TdxDecrementNumerationParagraphIndentCommand.CreateModifier(
  const AState: IdxCommandUIState): TdxParagraphPropertyModifier<Integer>;
begin
  raise TdxInternalException.Create;
end;

class function TdxDecrementNumerationParagraphIndentCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandDecrementParagraphLeftIndentDescription);
end;

class function TdxDecrementNumerationParagraphIndentCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandDecrementParagraphLeftIndentMenuCaption);
end;

{ TdxPageRunComparer }

constructor TdxPageRunComparer.Create(AIndex: TdxRunIndex);
begin
  inherited Create;
  FIndex := AIndex;
end;

function TdxPageRunComparer.CompareTo(const Page: TdxBoxBase): Integer;
var
  APage: TdxPage absolute Page;
begin
  if IsRunBeforePage(APage) then
    Result := 1
  else
    if IsRunAfterPage(APage) then
      Result := -1
    else
      Result := 0;
end;

function TdxPageRunComparer.IsRunAfterPage(APage: TdxPage): Boolean;
var
  AEndPos: TdxFormatterPosition;
begin
  AEndPos := APage.Areas.Last.Columns.Last.Rows.Last.Boxes.Last.StartPos;
  Result := FIndex > AEndPos.RunIndex;
end;

function TdxPageRunComparer.IsRunBeforePage(APage: TdxPage): Boolean;
var
  AStartPos: TdxFormatterPosition;
begin
  AStartPos := APage.Areas.First.Columns.First.Rows.First.Boxes.First.StartPos;
  Result := (FIndex < AStartPos.RunIndex) or ((FIndex = AStartPos.RunIndex) and (AStartPos.Offset > 0));
end;

{ TdxColumnRunComparer }

constructor TdxColumnRunComparer.Create(AIndex: TdxRunIndex);
begin
  inherited Create;
  FIndex := AIndex;
end;

function TdxColumnRunComparer.CompareTo(const Column: TdxBoxBase): Integer;
var
  AColumn: TdxColumn absolute Column;
begin
  if IsRunBeforeColumn(AColumn) then
    Result := 1
  else
    if IsRunAfterColumn(AColumn) then
      Result := -1
    else
      Result := 0;
end;

function TdxColumnRunComparer.IsRunAfterColumn(AColumn: TdxColumn): Boolean;
var
  AEndPos: TdxFormatterPosition;
begin
  AEndPos := AColumn.Rows.Last.Boxes.Last.StartPos;
  Result := FIndex > AEndPos.RunIndex;
end;

function TdxColumnRunComparer.IsRunBeforeColumn(AColumn: TdxColumn): Boolean;
var
  AStartPos: TdxFormatterPosition;
begin
  AStartPos := AColumn.Rows.First.Boxes.First.StartPos;
  Result := (FIndex < AStartPos.RunIndex) or ((FIndex = AStartPos.RunIndex) and (AStartPos.Offset > 0));
end;

{ TdxPageAreaRunComparer }

constructor TdxPageAreaRunComparer.Create(AIndex: TdxRunIndex);
begin
  inherited Create;
  FIndex := AIndex;
end;

function TdxPageAreaRunComparer.CompareTo(const PageArea: TdxBoxBase): Integer;
var
  APageArea: TdxPageArea absolute PageArea;
begin
  if IsRunBeforePageArea(APageArea) then
    Result := 1
  else
    if IsRunAfterPageArea(APageArea) then
      Result := -1
    else
      Result := 0;
end;

function TdxPageAreaRunComparer.IsRunAfterPageArea(APageArea: TdxPageArea): Boolean;
var
  AEndPos: TdxFormatterPosition;
begin
  AEndPos := APageArea.Columns.Last.Rows.Last.Boxes.Last.StartPos;
  Result := FIndex > AEndPos.RunIndex;
end;

function TdxPageAreaRunComparer.IsRunBeforePageArea(APageArea: TdxPageArea): Boolean;
var
  AStartPos: TdxFormatterPosition;
begin
  AStartPos := APageArea.Columns.First.Rows.First.Boxes.First.StartPos;
  Result := (FIndex < AStartPos.RunIndex) or ((FIndex = AStartPos.RunIndex) and (AStartPos.Offset > 0));
end;

{ TdxRowRunComparer }

constructor TdxRowRunComparer.Create(AIndex: TdxRunIndex);
begin
  inherited Create;
  FIndex := AIndex;
end;

function TdxRowRunComparer.CompareTo(const Row: TdxBoxBase): Integer;
var
  ARow: TdxRow absolute Row;
begin
  if IsRunBeforeRow(ARow) then
    Result := 1
  else
    if IsRunAfterRow(ARow) then
      Result := -1
    else
      Result := 0;
end;

function TdxRowRunComparer.IsRunAfterRow(ARow: TdxRow): Boolean;
var
  AEndPos: TdxFormatterPosition;
begin
  AEndPos := ARow.Boxes.Last.StartPos;
  Result := FIndex > AEndPos.RunIndex;
end;

function TdxRowRunComparer.IsRunBeforeRow(ARow: TdxRow): Boolean;
var
  AStartPos: TdxFormatterPosition;
begin
  AStartPos := ARow.Boxes.First.StartPos;
  Result := (FIndex < AStartPos.RunIndex) or ((FIndex = AStartPos.RunIndex) and (AStartPos.Offset > 0));
end;

{ TdxInsertListFormCommand }

constructor TdxInsertListFormCommand.Create(const AControl: IdxRichEditControl; AList: TdxAbstractNumberingList;
  ALevelIndex: Integer; AMode: TdxInsertListMode);
begin
  inherited Create(AControl);
  FList := AList;
  FLevelIndex := ALevelIndex;
  FMode := AMode;
end;

function TdxInsertListFormCommand.ShouldAddNumberingListToParagraphs(AParagraphIntervals: TdxParagraphIntervalList): Boolean;
begin
  Result := True;
end;

function TdxInsertListFormCommand.GetListIndex(AParagraphIntervals: TdxParagraphIntervalList): TdxNumberingListIndex;
var
  ACalculator: TdxNumberingListIndexCalculator;
begin
  ACalculator := DocumentModel.CommandsCreationStrategy.CreateNumberingListIndexCalculator(DocumentModel,
    NumberingListType);
  try
    Result := ACalculator.CreateNewList(FList);
  finally
    ACalculator.Free;
  end;
end;

procedure TdxInsertListFormCommand.AddNumberingListToParagraph(AParagraph: TdxParagraph; AListIndex: TdxNumberingListIndex; AListLevelIndex: Integer);
begin
  inherited AddNumberingListToParagraph(AParagraph, AListIndex, AListLevelIndex);
  AParagraph.ParagraphProperties.ResetUse([TdxUsedParagraphFormattingOption.UseFirstLineIndent, TdxUsedParagraphFormattingOption.UseLeftIndent]);
end;

function TdxInsertListFormCommand.CalculateParagraphListLevel(AParagraph: TdxParagraphBase; ALeftIndent: Integer): Integer;
begin
  case FMode of
    TdxInsertListMode.KeepLevelIndex:
      if AParagraph.IsInList then
        Result := AParagraph.GetListLevelIndex
      else
        Result := inherited CalculateParagraphListLevel(AParagraph, ALeftIndent);
    TdxInsertListMode.ChangeLevelIndex:
      Result := FLevelIndex;
  else
    Result := inherited CalculateParagraphListLevel(AParagraph, ALeftIndent);
  end;
end;

{ TdxHeaderFooterParagraphLayoutPosition }

constructor TdxHeaderFooterParagraphLayoutPosition.Create(const AControl: IdxRichEditControl; APage: TdxPage; AArea: TdxPageArea);
begin
  inherited Create(AControl);
  Assert(APage <> nil);
  Assert(AArea <> nil);
  FPage := APage;
  FArea := AArea;
end;

function TdxHeaderFooterParagraphLayoutPosition.LookupPage(APages: TdxPageCollection; ARunIndex: TdxRunIndex): TdxPage;
begin
  Result := FPage;
end;

function TdxHeaderFooterParagraphLayoutPosition.LookupPageArea(APage: TdxPage; AParagraph: TdxParagraph; ARunIndex: TdxRunIndex): TdxPageArea;
begin
  Result := FArea;
end;

{ TdxChangeSectionLineNumberingCommand }

procedure TdxChangeSectionLineNumberingCommand.ExecuteCore;
begin
end;

class function TdxChangeSectionLineNumberingCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandChangeSectionLineNumberingDescription);
end;

class function TdxChangeSectionLineNumberingCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandChangeSectionLineNumberingMenuCaption);
end;

class function TdxChangeSectionLineNumberingCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ChangeSectionLineNumbering;
end;

class function TdxChangeSectionLineNumberingCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.LineNumbers;
end;

procedure TdxChangeSectionLineNumberingCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  AState.Enabled := IsContentEditable;
  AState.Visible := True;
  ApplyCommandRestrictionOnEditableControl(AState, DocumentModel.DocumentCapabilities.Sections, AState.Enabled);
  ApplyDocumentProtectionToSelectedSections(AState);
  if not RichEditControl.InnerControl.DocumentModel.ActivePieceTable.CanContainCompositeContent then
    AState.Enabled := False;
end;

{ TdxSetPredefinedSectionLineNumberingCommand }

function TdxSetPredefinedSectionLineNumberingCommand.CreateModifier(const AState: IdxCommandUIState): TdxSectionPropertyModifier<TdxLineNumberingRestart>;
begin
  Result := TdxSectionLineNumberingStepAndRestartModifier.Create(LineNumberingRestart);
end;

function TdxSetPredefinedSectionLineNumberingCommand.IsCheckedValue(AValue: TdxLineNumberingRestart): Boolean;
begin
  Result := AValue = LineNumberingRestart;
end;

{ TdxSetSectionLineNumberingNoneCommand }

class function TdxSetSectionLineNumberingNoneCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandSetSectionLineNumberingNoneDescription);
end;

function TdxSetSectionLineNumberingNoneCommand.GetLineNumberingRestart: TdxLineNumberingRestart;
begin
  Result := TdxLineNumberingRestart(-1);
end;

class function TdxSetSectionLineNumberingNoneCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandSetSectionLineNumberingNoneMenuCaption);
end;

class function TdxSetSectionLineNumberingNoneCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.SetSectionLineNumberingNone;
end;

{ TdxSetSectionLineNumberingContinuousCommand }

class function TdxSetSectionLineNumberingContinuousCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandSetSectionLineNumberingContinuousDescription);
end;

function TdxSetSectionLineNumberingContinuousCommand.GetLineNumberingRestart: TdxLineNumberingRestart;
begin
  Result := TdxLineNumberingRestart.Continuous;
end;

class function TdxSetSectionLineNumberingContinuousCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandSetSectionLineNumberingContinuousMenuCaption);
end;

class function TdxSetSectionLineNumberingContinuousCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.SetSectionLineNumberingContinuous;
end;

{ TdxSetSectionLineNumberingRestartNewPageCommand }

class function TdxSetSectionLineNumberingRestartNewPageCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandSetSectionLineNumberingRestartNewPageDescription);
end;

function TdxSetSectionLineNumberingRestartNewPageCommand.GetLineNumberingRestart: TdxLineNumberingRestart;
begin
  Result := TdxLineNumberingRestart.NewPage;
end;

class function TdxSetSectionLineNumberingRestartNewPageCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandSetSectionLineNumberingRestartNewPageMenuCaption);
end;

class function TdxSetSectionLineNumberingRestartNewPageCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.SetSectionLineNumberingRestartNewPage;
end;

{ TdxSetSectionLineNumberingRestartNewSectionCommand }

class function TdxSetSectionLineNumberingRestartNewSectionCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandSetSectionLineNumberingRestartNewSectionDescription);
end;

function TdxSetSectionLineNumberingRestartNewSectionCommand.GetLineNumberingRestart: TdxLineNumberingRestart;
begin
  Result := TdxLineNumberingRestart.NewSection;
end;

class function TdxSetSectionLineNumberingRestartNewSectionCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandSetSectionLineNumberingRestartNewSectionMenuCaption);
end;

class function TdxSetSectionLineNumberingRestartNewSectionCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.SetSectionLineNumberingRestartNewSection;
end;

end.
