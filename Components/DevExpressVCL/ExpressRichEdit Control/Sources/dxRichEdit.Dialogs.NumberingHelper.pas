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

unit dxRichEdit.Dialogs.NumberingHelper;

interface

{$I cxVer.inc}
{$I dxRichEditControl.inc}

uses
  Types, SysUtils, Classes, Generics.Defaults, Generics.Collections, DB, Graphics, Controls,
  dxContainerListBox,
{$IFNDEF DELPHI22}
  DBPlatform,
{$ENDIF}
  dxRichEdit.Types,
  dxRichEdit.Utils.Types,
  dxRichEdit.DocumentModel.Numbering,
  dxRichEdit.DocumentModel.NumberingFormatting,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.Control,
  dxRichEdit.DocumentModel.ParagraphFormatting,
  dxRichEdit.View.Core,
  dxRichEditDialogsSimpleControl,
  dxRichEdit.Actions,
  dxRichEdit.Dialogs.NumberingFormController,
  dxRichEdit.Dialogs.CustomDialog,
  dxRichEdit.Utils.Properties, dxCoreGraphics;

type
  TdxNumberingListBoxHelper = class;

  IdxRichEditNumberingListForm = interface
    ['{111F1B1D-3227-4D2D-A3E9-320827FA8EBF}']
    function GetActiveGallery: TdxNumberingListBoxHelper;
    function GetController: TdxNumberingListFormController;
    function GetContinuePreviousList: Boolean;
    function GetRestartNumbering: Boolean;
    procedure SetContinuePreviousList(const Value: Boolean);
    procedure SetRestartNumbering(const Value: Boolean);
    function CreateNumberingListFormCore(AListLevels: TdxListLevelCollection; AFirstParagraphStartIndex: Integer;
      AMultiLevels: Boolean): TdxRichEditCustomDialogForm;
    function IsBulletedTabSelected: Boolean;
    function IsNumberedTabSelected: Boolean;
    function IsOutlineNumberedTabSelected: Boolean;
    property ActiveGallery: TdxNumberingListBoxHelper read GetActiveGallery;
    property Controller: TdxNumberingListFormController read GetController;
    property ContinuePreviousList: Boolean read GetContinuePreviousList write SetContinuePreviousList;
    property RestartNumbering: Boolean read GetRestartNumbering write SetRestartNumbering;
  end;

  IdxRichEditBulletedListForm = interface
    ['{EEE4C0DE-A743-4D92-8D52-04B736D6C449}']
    function GetController: TdxBulletedListFormController;
    function GetSelectedIndex: Integer;
    procedure SetSelectedIndex(const Value: Integer);
    procedure InitializeComponents(const ASymbolsProperties: array of TdxSymbolProperties);
    property Controller: TdxBulletedListFormController read GetController;
    property SelectedIndex: Integer read GetSelectedIndex write SetSelectedIndex;
  end;

  TdxNumberingListBoxHelper = class
  public const
    ColumnCount = 4;
    RowCount = 2;
    DpiX = 96;
    DpiY = 96;
  private
    FListBox: TdxCustomContainerListBox;
    FControl: IdxRichEditControl;
    FController: TdxNumberingListFormController;
    FLevelIndex: Integer;
    procedure AddNoneItem;
    procedure AddMultiLevelNumberingListsToParagraph(APieceTable: TdxPieceTable);
    procedure AddNumberingListsToParagraph(APieceTable: TdxPieceTable);
    procedure AddNumberingListLevelToParagraph(APieceTable: TdxPieceTable; AParagraph: TdxParagraph;
      ANumberingListIndex: TdxNumberingListIndex; ALevelIndex: Integer);
    procedure AdjustLevel(const ALevel: IdxListLevel; ALevelIndex: Integer);
    function CalculateItemHeight: Integer;
    function CalculateLevelIndentAdjustement(const ALevel: IdxListLevel): Integer;
    function ContainsListWithSameProperties(AAbstractList: TdxAbstractNumberingList): Boolean;
    function CreateSimpleControl: TdxSimpleRichEditControl;
    function CreateSimpleControlNoneText: TdxSimpleRichEditControl;
    function CreateNumberingListCopy(ATargetDocumentModel: TdxDocumentModel; ASourceList: TdxAbstractNumberingList): TdxNumberingList;
    procedure CreatePreviewContent(APieceTable: TdxPieceTable; AParagraphCount: Integer);
    procedure FormatPreviewContent(APieceTable: TdxPieceTable);
    function GetCaptionNone: string; inline;
    function GetHeight: Integer; inline;
    function GetWidth: Integer; inline;
    function GetItemHeight: Integer; inline;
    procedure InitializeControlCore(ANumberingType: TdxNumberingType; ADocumentModel: TdxDocumentModel);
    procedure PrepareListPreviewContent(ADocumentModel: TdxDocumentModel; APreviewParagraphCount: integer;
      ASourceList: TdxAbstractNumberingList);
    function GetSelectedAbstractList: TdxAbstractNumberingList;
    procedure SetSelectedAbstractList(const Value: TdxAbstractNumberingList);
  protected
    function GetSelectedListIndex(AValue: TdxAbstractNumberingList): Integer;

    property Controller: TdxNumberingListFormController read FController;
  public
    constructor Create(const AControl: IdxRichEditControl; AController: TdxNumberingListFormController);
    procedure InitializeControl(AListBox: TdxCustomContainerListBox; ANumberingType: TdxNumberingType; ALevelIndex: Integer);
    property SelectedAbstractList: TdxAbstractNumberingList read GetSelectedAbstractList write SetSelectedAbstractList;
    property CaptionNone: string read GetCaptionNone;
    property Control: IdxRichEditControl read FControl;
    property Height: Integer read GetHeight;
    property Width: Integer read GetWidth;
    property ItemHeight: Integer read GetItemHeight;
    property ListBox: TdxCustomContainerListBox read FListBox;
  end;

  TdxNumberingListFormHelper = class
  public const
    ParagraphIndexMinValue = 0;
    ParagraphIndexMaxValue = MaxInt;
  private
    FOwner: IdxRichEditNumberingListForm;
    FController: TdxNumberingListFormController;
    function GetParagraphs: TdxParagraphList; inline;
  protected
    property Owner: IdxRichEditNumberingListForm read FOwner;
  public
    constructor Create(const AOwner: IdxRichEditNumberingListForm);
    function AreThereParagraphsInList: Boolean;
    procedure ApplyChanges(ANewList: TdxAbstractNumberingList); overload;
    procedure ApplyChanges(ANewList: TdxAbstractNumberingList; AApplyScope: TdxNumberingListApplyScope;
      ANewListType: TdxNumberingType; ANewListIndex: TdxNumberingListIndex); overload;
    procedure ApplyChangesWithoutCustomization;
    function DoSelectedParagraphsHaveDifferentNumberingLists: Boolean;
    function GetIndexOfParagraphInListBeforeCurrent: Integer;
    function GetFirstParagraphInList: TdxParagraph;
    function IsParagraphInDifferentListFromCurrent(AIndex: Integer): Boolean;
    function IsSelectedParagraphsBulletedLists: Boolean;
    procedure ShowNumberingListFormCore(ASource: TdxAbstractNumberingList; AListLevels: TdxListLevelCollection; AMultiLevels: Boolean);
    property Controller: TdxNumberingListFormController read FController write FController;
    property Paragraphs: TdxParagraphList read GetParagraphs;
  end;

  { TdxBulletedListFormHelper }

  TdxBulletedListFormHelper = class
  public const
    SymbolsProperties: array[0..5] of TdxSymbolProperties = (
      (UnicodeChar: #$00B7; FontName: 'Symbol'),
      (UnicodeChar: #$00FC; FontName: 'Wingdings'),
      (UnicodeChar: #$00D8; FontName: 'Wingdings'),
      (UnicodeChar: #118; FontName: 'Wingdings'),
      (UnicodeChar: #111; FontName: 'Courier New'),
      (UnicodeChar: #$00A7; FontName: 'Wingdings'));
  private
    FOwner: IdxRichEditBulletedListForm;
    function GetController: TdxBulletedListFormController; inline;
  protected
    property Owner: IdxRichEditBulletedListForm read FOwner;
  public
    class function GetActiveSymbolIndex(const AActiveProperties: TdxSymbolProperties): Integer;
    constructor Create(const AOwner: IdxRichEditBulletedListForm);
    procedure CreateBulletCharacters;
    property Controller: TdxBulletedListFormController read GetController;
  end;

  { TdxDisplayFormatHelper }

  TdxDisplayFormatHelper = class
  public const
    FieldNames: array[0..8] of string = ('F0', 'F1', 'F2', 'F3', 'F4', 'F5', 'F6', 'F7', 'F8');
    FieldCodeTemplate = 'MERGEFIELD %s';
  public type
    TdxState = procedure (AChar: Char) of object;
  private
    FControl: TdxSimpleRichEditControl;
    FLevels: TdxListLevelCollection;
    FSource: TDataSet;
    FStringBuilder: TStringBuilder;
    FCurrentState: TdxState;
    FDataSource: TDataSource;
  protected
    procedure FlushAsFormat;
    procedure FlushAsText;
    procedure TextState(AChar: Char);
    procedure CloseBracketInTextState(AChar: Char);
    procedure OpenBracketState(AChar: Char);
    procedure FormatState(AChar: Char);
    procedure CloseBracketState(AChar: Char);

    procedure ProcessChar(AChar: Char);
    procedure ProcessEndOfFormat;
  public
    constructor Create(const AControl: TdxSimpleRichEditControl; const ALevels: TdxListLevelCollection; ALevelIndex: Integer);
    destructor Destroy; override;
    function AppendFormatFromField(const AStringBuilder: TStringBuilder; AStartIndex: TdxRunIndex): TdxRunIndex;
    function GetDisplayFormatString: string;
    procedure SetDisplayFormat(const AFormat: string);
    class function IndexOf(const AArray: array of string; const AValue: string): Integer;
    procedure Update;
    property Levels: TdxListLevelCollection read FLevels;
  end;

  { TdxNumberingListDataSet }

  TdxNumberingListDataSet = class(TDataSet)
  public type
    TdxNumberingListTextSource = record
    private
      FOwner: TdxDisplayFormatHelper;
      function GetLevels: TdxListLevelCollection; inline;
      function FormatLevelText(ALevelIndex: Integer): string;
      function GetListCounters: TIntegerDynArray;
    public
      constructor Create(const AOwner: TdxDisplayFormatHelper);
      property Levels: TdxListLevelCollection read GetLevels;
      property F0: string index 0 read FormatLevelText;
      property F1: string index 1 read FormatLevelText;
      property F2: string index 2 read FormatLevelText;
      property F3: string index 3 read FormatLevelText;
      property F4: string index 4 read FormatLevelText;
      property F5: string index 5 read FormatLevelText;
      property F6: string index 6 read FormatLevelText;
      property F7: string index 7 read FormatLevelText;
      property F8: string index 8 read FormatLevelText;
    end;
  private const
    MaxFieldSize = 256;
    RecBufSize = MaxFieldSize * 9;
  private
    FActive: Boolean;
    FNumberingListTextSource: TdxNumberingListTextSource;
  protected
    function AllocRecordBuffer: TRecordBuffer; override;
    procedure FreeRecordBuffer(var Buffer: TRecordBuffer); override;
    function GetRecord(Buffer: PByte; GetMode: TGetMode; DoCheck: Boolean): TGetResult; override;
    procedure InternalInitRecord(Buffer: TRecordBuffer); override;
    function GetRecordSize: Word; override;

    procedure InternalClose; override;
    procedure InternalHandleException; override;
    procedure InternalInitFieldDefs; override;
    procedure InternalOpen; override;
    function IsCursorOpen: Boolean; override;
    function GetCanModify: Boolean; override;
  public
    constructor Create(AOwner: TdxDisplayFormatHelper); reintroduce;
    function GetFieldData(Field: TField; {$IFDEF DELPHI18}var{$ENDIF} Buffer: TValueBuffer): Boolean; override;
  end;

function Format(const AFormat: string; const Args: array of string): string; overload;

implementation

uses
{$IFDEF DELPHIXE2}
  System.UITypes,
{$ENDIF}
  RTLConsts, Contnrs, Math, StrUtils, Character,
  cxControls,
  dxCore, dxCoreClasses, dxTypeHelpers,

  dxRichEdit.Utils.Exceptions,
  dxRichEdit.Dialogs.Strs,
  dxRichEdit.Platform.Font,
  dxRichEdit.InnerControl,
  dxRichEdit.DocumentModel.FieldRange,
  dxRichEdit.Utils.ChunkedStringBuilder,
  dxCharacters,
  dxRichEdit.Utils.BatchUpdateHelper,
  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.DocumentModel.TextRange,
  dxRichEdit.DocumentModel.PieceTableModifiers,
  dxRichEdit.DocumentModel.CharacterFormatting,
  dxRichEdit.DocumentModel.TabFormatting,
  dxRichEdit.DocumentModel.Fields.Core,
  dxRichEdit.DocumentModel.Fields,
  dxRichEdit.View.Simple,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.MailMerge;

type
  TdxRichEditControlAccess = class(TdxCustomRichEditControl);
  TdxSimpleViewAccess = class(TdxSimpleView);
  TdxTextRunBaseAccess = class(TdxTextRunBase);

function Format(const AFormat: string; const Args: array of string): string;
var
  I: Integer;
  AConstArray: array of TVarRec;
begin
  SetLength(AConstArray, Length(Args));
  for I := Low(Args) to High(Args) do
  begin
    AConstArray[I].VUnicodeString := Pointer(UnicodeString(Args[I]));
    AConstArray[I].VType := vtUnicodeString;
  end;
  Result := SysUtils.Format(AFormat, AConstArray);
end;

{ TdxNumberingListBoxHelper }

constructor TdxNumberingListBoxHelper.Create(const AControl: IdxRichEditControl;
  AController: TdxNumberingListFormController);
begin
  inherited Create;
  FControl := AControl;
  FController := AController;
end;

procedure TdxNumberingListBoxHelper.AddMultiLevelNumberingListsToParagraph(APieceTable: TdxPieceTable);
var
  ADocumentModel: TdxDocumentModel;
  ANumberingListIndex: TdxNumberingListIndex;
  AList: TdxNumberingList;
  AParagraphs: TdxParagraphCollection;
  ACount: TdxParagraphIndex;
  ALevelIndex: TdxParagraphIndex;
begin
  ADocumentModel := APieceTable.DocumentModel;
  ANumberingListIndex := TdxNumberingListIndex(ADocumentModel.NumberingLists.Count - 1);
  AList := ADocumentModel.NumberingLists[ANumberingListIndex];

  AParagraphs := APieceTable.Paragraphs;
  ACount := TdxParagraphIndex(AParagraphs.Count - 1);
  for ALevelIndex := TdxParagraphIndex(0) to ACount - 1 do
  begin
    AdjustLevel(AList.Levels[ALevelIndex], ALevelIndex);
    AddNumberingListLevelToParagraph(APieceTable, AParagraphs[ALevelIndex], ANumberingListIndex, ALevelIndex);
  end;
  APieceTable.AddNumberingListToParagraph(AParagraphs[ACount], ANumberingListIndex, 0);
end;

procedure TdxNumberingListBoxHelper.AddNoneItem;
begin
  FListBox.AddControlItem(CreateSimpleControlNoneText);
end;

procedure TdxNumberingListBoxHelper.AddNumberingListLevelToParagraph(APieceTable: TdxPieceTable; AParagraph: TdxParagraph;
  ANumberingListIndex: TdxNumberingListIndex; ALevelIndex: Integer);
var
  ADocumentModel: TdxDocumentModel;
  ALevel: IdxListLevel;
  ATabs: TdxTabFormattingInfo;
begin
  ADocumentModel := APieceTable.DocumentModel;
  ALevel := ADocumentModel.NumberingLists[ANumberingListIndex].Levels[ALevelIndex];
  ATabs := AParagraph.GetOwnTabs;
  try
    ATabs.Add(TdxTabInfo.Create(ALevel.ParagraphProperties.LeftIndent));
    ATabs.Add(TdxTabInfo.Create(ALevel.ParagraphProperties.LeftIndent +
      ADocumentModel.UnitConverter.PixelsToModelUnits(ItemHeight, dpiX)));
    AParagraph.SetOwnTabs(ATabs);

    APieceTable.AddNumberingListToParagraph(AParagraph, ANumberingListIndex, ALevelIndex);
  finally
    ATabs.Free;
  end;
end;

procedure TdxNumberingListBoxHelper.AddNumberingListsToParagraph(APieceTable: TdxPieceTable);
var
  ADocumentModel: TdxDocumentModel;
  ANumberingListIndex: TdxNumberingListIndex;
  ALevel: IdxListLevel;
  ACount: TdxParagraphIndex;
  AParagraphs: TdxParagraphCollection;
  I: TdxParagraphIndex;
begin
  ADocumentModel := APieceTable.DocumentModel;
  ANumberingListIndex := TdxNumberingListIndex(ADocumentModel.NumberingLists.Count - 1);
  ALevel := ADocumentModel.NumberingLists[ANumberingListIndex].Levels[FLevelIndex];
  AdjustLevel(ALevel, 0);
  AParagraphs := APieceTable.Paragraphs;
  ACount := TdxParagraphIndex(AParagraphs.Count);
  for I := TdxParagraphIndex(0) to ACount - 1 do
    AddNumberingListLevelToParagraph(APieceTable, AParagraphs[I], ANumberingListIndex, FLevelIndex);
end;

procedure TdxNumberingListBoxHelper.AdjustLevel(const ALevel: IdxListLevel; ALevelIndex: Integer);
var
  ALevelOffset: Integer;
  AAdjustment: Integer;
begin
  ALevelOffset := ALevel.DocumentModel.UnitConverter.PixelsToModelUnits(20, dpiX);
  AAdjustment := CalculateLevelIndentAdjustement(ALevel);
  ALevel.ParagraphProperties.LeftIndent := ALevel.ParagraphProperties.LeftIndent - AAdjustment;
  ALevel.ParagraphProperties.LeftIndent := ALevel.ParagraphProperties.LeftIndent + ALevelIndex * ALevelOffset;
  ALevel.ListLevelProperties.Alignment := TdxListNumberAlignment.Left;
end;

function TdxNumberingListBoxHelper.CalculateItemHeight: Integer;
begin
  Result := Height div RowCount - FListBox.IndentFromEdges;
end;

function TdxNumberingListBoxHelper.CalculateLevelIndentAdjustement(const ALevel: IdxListLevel): Integer;
begin
  case ALevel.ParagraphProperties.FirstLineIndentType of
    TdxParagraphFirstLineIndent.Hanging:
      Result := ALevel.ParagraphProperties.LeftIndent - ALevel.ParagraphProperties.FirstLineIndent;
    TdxParagraphFirstLineIndent.Indented:
      Result := ALevel.ParagraphProperties.LeftIndent;
  else
    Result := 0;
  end;
end;

function TdxNumberingListBoxHelper.ContainsListWithSameProperties(AAbstractList: TdxAbstractNumberingList): Boolean;
var
  I: Integer;
  AExistingList: TdxAbstractNumberingList;
begin
  Result := False;
  for I := 0 to ListBox.ControlItems.Count - 1 do
  begin
    AExistingList := (ListBox.ControlItems[I] as TdxSimpleRichEditControl).AbstractList;
    if AExistingList = Controller.NoneList then
      Continue;
    if AExistingList.IsEqual(AAbstractList) then
    begin
      Result := True;
      Break;
    end;
  end;
end;

function TdxNumberingListBoxHelper.CreateNumberingListCopy(ATargetDocumentModel: TdxDocumentModel;
  ASourceList: TdxAbstractNumberingList): TdxNumberingList;
var
  AAbstractNumberingList: TdxAbstractNumberingList;
begin
  AAbstractNumberingList := TdxAbstractNumberingList.Create(ATargetDocumentModel);
  AAbstractNumberingList.CopyFrom(ASourceList);
  ATargetDocumentModel.AddAbstractNumberingListUsingHistory(AAbstractNumberingList);
  Result := TdxNumberingList.Create(ATargetDocumentModel, 0);
  ATargetDocumentModel.AddNumberingListUsingHistory(Result);
end;

procedure TdxNumberingListBoxHelper.CreatePreviewContent(APieceTable: TdxPieceTable; AParagraphCount: Integer);
const
  ListParagraphContent = TdxCharacters.TabMark + TdxCharacters.LineBreak + TdxCharacters.TabMark;
var
  I: Integer;
begin
  APieceTable.InsertPlainText(TdxDocumentLogPosition(0), ListParagraphContent);
  for I := 1 to AParagraphCount - 1 do
  begin
    APieceTable.InsertParagraph(TdxDocumentLogPosition(0));
    APieceTable.InsertPlainText(TdxDocumentLogPosition(0), ListParagraphContent);
  end;
end;

function TdxNumberingListBoxHelper.CreateSimpleControl: TdxSimpleRichEditControl;
var
  ABackColor: TdxAlphaColor;
begin
  Result := TdxSimpleRichEditControl.Create(FListBox);
  Result.SetBounds(0, 0, FListBox.ColumnWidth - 2 * FListBox.ItemPadding, FListBox.ItemHeight - 2 * FListBox.ItemPadding);
  Result.Font.Size := 10;
  Result.Font.Name := 'Arial';
  ABackColor := Result.DocumentModel.DocumentProperties.PageBackColor;
  if TdxAlphaColors.IsTransparentOrEmpty(ABackColor) then
    ABackColor := Result.InnerControl.ActiveView.ActualBackColor;
  Result.Color := TdxAlphaColors.ToColor(ABackColor);
end;

function TdxNumberingListBoxHelper.CreateSimpleControlNoneText: TdxSimpleRichEditControl;
var
  ACache: TdxFontCache;
  APieceTable: TdxPieceTable;
  AFontInfo: TdxFontInfo;
  ATextHeight: Integer;
  APaddingTop: Integer;
  AParagraph: TdxParagraph;
begin
  Result := CreateSimpleControl;
  Result.AbstractList := Controller.NoneList;

  ACache := Result.DocumentModel.FontCache;
  APieceTable := Result.DocumentModel.MainPieceTable;
  AFontInfo := ACache[APieceTable.Runs.First.FontCacheIndex];
  ATextHeight := Result.DocumentModel.LayoutUnitConverter.LayoutUnitsToPixels(
    ACache.Measurer.MeasureString(CaptionNone, AFontInfo).Height, dpiY);
  APaddingTop := (Result.Height - ATextHeight) div 2;
  APaddingTop := Result.DocumentModel.UnitConverter.PixelsToModelUnits(APaddingTop);
  AParagraph := APieceTable.Paragraphs[TdxParagraphIndex(0)];
  AParagraph.FirstLineIndent := 0;
  AParagraph.LeftIndent := 0;
  AParagraph.SpacingBefore := APaddingTop;
  AParagraph.Alignment := TdxParagraphAlignment.Center;
  APieceTable.InsertPlainText(TdxDocumentLogPosition(0), CaptionNone);
end;

procedure TdxNumberingListBoxHelper.FormatPreviewContent(APieceTable: TdxPieceTable);
var
  AFrom: TdxDocumentLogPosition;
  ALength: Integer;
  AUnderlineModifier: TdxRunFontUnderlineTypeModifier;
  AUnderlineColorModifier: TdxRunUnderlineColorModifier;
begin
  AFrom := APieceTable.DocumentStartLogPosition;
  ALength := APieceTable.DocumentEndLogPosition - APieceTable.DocumentStartLogPosition;
  AUnderlineModifier := TdxRunFontUnderlineTypeModifier.Create(TdxUnderlineType.ThickSingle);
  try
    APieceTable.ApplyCharacterFormatting(AFrom, ALength, AUnderlineModifier);
    AUnderlineColorModifier := TdxRunUnderlineColorModifier.Create(TdxAlphaColors.BtnShadow);
    try
      APieceTable.ApplyCharacterFormatting(AFrom, ALength, AUnderlineColorModifier);
    finally
      AUnderlineColorModifier.Free;
    end;
  finally
    AUnderlineModifier.Free;
  end;
end;

function TdxNumberingListBoxHelper.GetCaptionNone: string;
begin
  Result := cxGetResourceString(@sdxRichEditNumberingListBoxNone);
end;

function TdxNumberingListBoxHelper.GetHeight: Integer;
begin
  Result := FListBox.ClientHeight;
end;

function TdxNumberingListBoxHelper.GetItemHeight: Integer;
begin
  Result := FListBox.ItemHeight;
end;

function TdxNumberingListBoxHelper.GetSelectedAbstractList: TdxAbstractNumberingList;
begin
  if FListBox.SelectedIndex < FListBox.ControlItems.Count then
    Result := TdxSimpleRichEditControl(FListBox.ControlItems[FListBox.SelectedIndex]).AbstractList
  else
    Result := nil;
end;

function TdxNumberingListBoxHelper.GetSelectedListIndex(AValue: TdxAbstractNumberingList): Integer;
begin
  if not Assigned(AValue) then
  begin
    Result := -1;
    Exit
  end;
  for Result := 0 to FListBox.ControlItems.Count - 1 do
    if TdxSimpleRichEditControl(FListBox.ControlItems[Result]).AbstractList.IsEqual(AValue) then
      Exit;
  Assert(false);
  Result := -1;
end;

function TdxNumberingListBoxHelper.GetWidth: Integer;
begin
  Result := FListBox.ClientWidth;
end;

procedure TdxNumberingListBoxHelper.InitializeControl(AListBox: TdxCustomContainerListBox; ANumberingType: TdxNumberingType;
  ALevelIndex: Integer);
begin
  if ALevelIndex = -1 then
    FLevelIndex := 0
  else
    FLevelIndex := ALevelIndex;
  FListBox := AListBox;
  FListBox.ColumnCount := ColumnCount;
  FListBox.ItemHeight := CalculateItemHeight;
  FListBox.ControlItems.Clear;
  AddNoneItem;
  InitializeControlCore(ANumberingType, TdxCustomRichEditControl(Control).DocumentModel);
  InitializeControlCore(ANumberingType, TdxCustomRichEditControl(Control).InnerControl.DocumentModelTemplate);
  FListBox.Refresh;
end;

procedure TdxNumberingListBoxHelper.InitializeControlCore(ANumberingType: TdxNumberingType;
  ADocumentModel: TdxDocumentModel);
var
  ASimpleControl: TdxSimpleRichEditControl;
  ASourceLists: TdxAbstractNumberingListCollection;
  AAbstractList: TdxAbstractNumberingList;
  AListType: TdxNumberingType;
  I: TdxAbstractNumberingListIndex;
  ACount: TdxAbstractNumberingListIndex;
begin
  ASourceLists := ADocumentModel.AbstractNumberingLists;
  ACount := ASourceLists.Count;
  for I := 0 to ACount - 1 do
  begin
    AAbstractList := ASourceLists[i];
    AListType := TdxNumberingListHelper.GetListType(AAbstractList);
    if ANumberingType <> AListType then
      Continue;
    if ContainsListWithSameProperties(AAbstractList) then
      Continue;
    ASimpleControl := CreateSimpleControl;
    ASimpleControl.AbstractList := AAbstractList;
    PrepareListPreviewContent(ASimpleControl.DocumentModel, 4, AAbstractList);
    FListBox.AddControlItem(ASimpleControl);
  end;
end;

procedure TdxNumberingListBoxHelper.PrepareListPreviewContent(ADocumentModel: TdxDocumentModel;
  APreviewParagraphCount: integer; ASourceList: TdxAbstractNumberingList);
var
  APieceTable: TdxPieceTable;
begin
  ADocumentModel.BeginUpdate;
  try
    CreateNumberingListCopy(ADocumentModel, ASourceList);
    APieceTable := ADocumentModel.MainPieceTable;
    CreatePreviewContent(APieceTable, APreviewParagraphCount);
    FormatPreviewContent(APieceTable);
    ADocumentModel.Selection.Start := TdxDocumentLogPosition(0);
    ADocumentModel.Selection.&End := TdxDocumentLogPosition(0);
    if TdxNumberingListHelper.GetListType(ASourceList) = TdxNumberingType.MultiLevel then
      AddMultiLevelNumberingListsToParagraph(APieceTable)
    else
      AddNumberingListsToParagraph(APieceTable);
  finally
    ADocumentModel.EndUpdate;
  end;
end;

procedure TdxNumberingListBoxHelper.SetSelectedAbstractList(const Value: TdxAbstractNumberingList);
begin
  FListBox.SelectedIndex := GetSelectedListIndex(Value);
end;

{ TdxNumberingListFormHelper }

procedure TdxNumberingListFormHelper.ApplyChanges(ANewList: TdxAbstractNumberingList;
  AApplyScope: TdxNumberingListApplyScope; ANewListType: TdxNumberingType; ANewListIndex: TdxNumberingListIndex);
begin
  Controller.ApplyScope := AApplyScope;
  Controller.NewListType := ANewListType;
  Controller.NewListIndex := ANewListIndex;
  Controller.NewAbstractList := ANewList;
  Controller.ApplyChanges;
end;

procedure TdxNumberingListFormHelper.ApplyChanges(ANewList: TdxAbstractNumberingList);
begin
  ApplyChanges(ANewList, TdxNumberingListApplyScope.ToSelectedText, TdxNumberingType.Bullet, NumberingListIndexListIndexNotSetted);
end;

procedure TdxNumberingListFormHelper.ApplyChangesWithoutCustomization;
var
  AFirstSelectedParagraph: TdxParagraph;
  AParagraphs: TdxParagraphCollection;
  AIsOutlineNumbered: Boolean;
  AApplyScope: TdxNumberingListApplyScope;
  ANewListType: TdxNumberingType;
  AIndexOfParagraphInListBeforeCurrent: Integer;
  AIsNumbListOfParagraphDifferent: Boolean;
  ANewListIndex: Integer;
begin
  if ((Owner.IsNumberedTabSelected or Owner.IsOutlineNumberedTabSelected)
    and not DoSelectedParagraphsHaveDifferentNumberingLists and AreThereParagraphsInList) then
  begin
    AFirstSelectedParagraph := GetFirstParagraphInList;
    AParagraphs := AFirstSelectedParagraph.PieceTable.Paragraphs;
    AIsOutlineNumbered := Owner.IsOutlineNumberedTabSelected;

    AApplyScope := TdxNumberingListApplyScope.ToSelectedText;
    if AIsOutlineNumbered then
      ANewListType := TdxNumberingType.MultiLevel
    else
      ANewListType := TdxNumberingType.Simple;

    AIndexOfParagraphInListBeforeCurrent := GetIndexOfParagraphInListBeforeCurrent;
    AIsNumbListOfParagraphDifferent := IsParagraphInDifferentListFromCurrent(AIndexOfParagraphInListBeforeCurrent);

    if Owner.RestartNumbering then
      AApplyScope := TdxNumberingListApplyScope.RestartNumbering
    else
    if Owner.ContinuePreviousList then
      if AIsNumbListOfParagraphDifferent then
        AApplyScope := TdxNumberingListApplyScope.ContinuePreviousList
      else
        if not IsSelectedParagraphsBulletedLists then
          AApplyScope := TdxNumberingListApplyScope.ModifyCurrentList;

    if AIndexOfParagraphInListBeforeCurrent >= ParagraphIndexMinValue then
      ANewListIndex := AParagraphs[AIndexOfParagraphInListBeforeCurrent].NumberingListIndex
    else
      ANewListIndex := NumberingListIndexListIndexNotSetted;
    ApplyChanges(Owner.ActiveGallery.SelectedAbstractList, AApplyScope, ANewListType, ANewListIndex);
  end
  else
    ApplyChanges(Owner.ActiveGallery.SelectedAbstractList);
end;

function TdxNumberingListFormHelper.AreThereParagraphsInList: Boolean;
begin
  Result := GetFirstParagraphInList <> nil;
end;

constructor TdxNumberingListFormHelper.Create(const AOwner: IdxRichEditNumberingListForm);
begin
  inherited Create;
  FOwner := AOwner;
  FController := AOwner.Controller;
end;

function TdxNumberingListFormHelper.DoSelectedParagraphsHaveDifferentNumberingLists: Boolean;
var
  I: Integer;
  AFirstSelectedParagraphListIndex: TdxNumberingListIndex;
  AParagraph: TdxParagraph;
begin
  AFirstSelectedParagraphListIndex := Controller.Paragraphs.First.NumberingListIndex;
  Result := False;
  for I := 0 to Paragraphs.Count - 1 do
  begin
    AParagraph := Paragraphs[I];
    if not AParagraph.IsInList then
      Continue;
    if(AFirstSelectedParagraphListIndex < NumberingListIndexMinValue) then
    begin
      AFirstSelectedParagraphListIndex := AParagraph.NumberingListIndex;
      Continue;
    end;
    if AParagraph.NumberingListIndex <> AFirstSelectedParagraphListIndex then
    begin
      Result := True;
      Break;
    end;
  end;
end;

function TdxNumberingListFormHelper.GetFirstParagraphInList: TdxParagraph;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Paragraphs.Count - 1 do
    if Paragraphs[I].IsInList then
    begin
      Result := Paragraphs[I];
      Break;
    end;
end;

function TdxNumberingListFormHelper.GetIndexOfParagraphInListBeforeCurrent: Integer;
const
  WrongParagraphIndex = -1;
var
  I: Integer;
  AFirstSelectedParagraph: TdxParagraph;
  AParagraphs: TdxParagraphCollection;
begin
  if not AreThereParagraphsInList then
    Exit(WrongParagraphIndex);
  AFirstSelectedParagraph := Paragraphs.First as TdxParagraph;
  AParagraphs := AFirstSelectedParagraph.PieceTable.Paragraphs;

  for I := AFirstSelectedParagraph.Index - 1 downto AParagraphs.First.Index do
    if AParagraphs[I].IsInList then
      Exit(I);
  Result := WrongParagraphIndex;
end;

function TdxNumberingListFormHelper.GetParagraphs: TdxParagraphList;
begin
  Result := Controller.Paragraphs;
end;

function TdxNumberingListFormHelper.IsParagraphInDifferentListFromCurrent(AIndex: Integer): Boolean;
var
  AFirstSelectedParagraph: TdxParagraph;
  AParagraphs: TdxParagraphCollection;
  AAreNumberingListsIndexesDifferent: Boolean;
begin
  if (AIndex < ParagraphIndexMinValue) or not AreThereParagraphsInList then
    Exit(False);
  AFirstSelectedParagraph := GetFirstParagraphInList;
  AParagraphs := AFirstSelectedParagraph.PieceTable.Paragraphs;
  AAreNumberingListsIndexesDifferent := AParagraphs[AIndex].NumberingListIndex <> AFirstSelectedParagraph.NumberingListIndex;
  Result := AAreNumberingListsIndexesDifferent;
end;

function TdxNumberingListFormHelper.IsSelectedParagraphsBulletedLists: Boolean;
var
  AAbstractNumberingList: TdxAbstractNumberingList;
begin
  AAbstractNumberingList := Controller.GetSelectedAbstractNumberingList;
  Result := (AAbstractNumberingList <> nil) and (AAbstractNumberingList <> Controller.NoneList) and
    (TdxNumberingListHelper.GetListType(AAbstractNumberingList) = TdxNumberingType.Bullet);
end;

procedure TdxNumberingListFormHelper.ShowNumberingListFormCore(ASource: TdxAbstractNumberingList;
  AListLevels: TdxListLevelCollection; AMultiLevels: Boolean);
var
  AForm: TdxRichEditCustomDialogForm;
  AAreThereParagraphsInList: Boolean;
  AFirstSelectedParagraph: TdxParagraph;
  AFirstSelectedParagraphStartIndexes: TIntegerDynArray;
  AIndexOfParagraphInListBeforeCurrent: TdxParagraphIndex;
  AIsPrevParagraphInDifferentList: Boolean;
  ASelectedParagraphsHaveDifferentNumberingLists: Boolean;
  ALevelIndex: Integer;
  AFirstParagraphStartIndex: Integer;
  AParag: TdxParagraph;
  APrevParagraphInDifferentListStartIndexes: TIntegerDynArray;
  AApplyScope: TdxNumberingListApplyScope;
  ANewListType: TdxNumberingType;
  AIsListLevelStartIndexCorrect: Boolean;
  ANewNumbListIndex: TdxNumberingListIndex;
begin
  AFirstSelectedParagraph := GetFirstParagraphInList;
  AAreThereParagraphsInList := Assigned(AFirstSelectedParagraph);
  if AAreThereParagraphsInList then
    AFirstSelectedParagraphStartIndexes := AFirstSelectedParagraph.PieceTable.GetRangeListCounters(AFirstSelectedParagraph)
  else
    AFirstSelectedParagraphStartIndexes := TIntegerDynArray.Create(1);
  AIndexOfParagraphInListBeforeCurrent := GetIndexOfParagraphInListBeforeCurrent;
  AIsPrevParagraphInDifferentList := IsParagraphInDifferentListFromCurrent(AIndexOfParagraphInListBeforeCurrent);

  ASelectedParagraphsHaveDifferentNumberingLists := DoSelectedParagraphsHaveDifferentNumberingLists;

  ALevelIndex := IfThen(AAreThereParagraphsInList, Controller.LevelIndex);
  AFirstParagraphStartIndex := AFirstSelectedParagraphStartIndexes[ALevelIndex];
  if (FOwner.RestartNumbering or ASelectedParagraphsHaveDifferentNumberingLists or not AAreThereParagraphsInList) then
    AFirstParagraphStartIndex := 1
  else
    if AIsPrevParagraphInDifferentList then
    begin
      AParag := AFirstSelectedParagraph.PieceTable.Paragraphs[AIndexOfParagraphInListBeforeCurrent];
      APrevParagraphInDifferentListStartIndexes := AFirstSelectedParagraph.PieceTable.GetRangeListCounters(AParag);
      if Length(APrevParagraphInDifferentListStartIndexes) = Length(AFirstSelectedParagraphStartIndexes) then
        AFirstParagraphStartIndex := APrevParagraphInDifferentListStartIndexes[Controller.LevelIndex] + 1;
    end;

  AForm := FOwner.CreateNumberingListFormCore(AListLevels, AFirstParagraphStartIndex, AMultiLevels);
  try
    if AForm.ShowModal = mrOk then
    begin
      AApplyScope := TdxNumberingListApplyScope.ToSelectedText;
      if AMultiLevels then
        ANewListType := TdxNumberingType.MultiLevel
      else
        ANewListType := TdxNumberingType.Simple;
      if( not ASelectedParagraphsHaveDifferentNumberingLists and AAreThereParagraphsInList) then
      begin
        AIsListLevelStartIndexCorrect := AFirstSelectedParagraphStartIndexes[Controller.LevelIndex] =
          ASource.Levels[Controller.LevelIndex].ListLevelProperties.Start;
        if FOwner.ContinuePreviousList and AIsPrevParagraphInDifferentList then
          AIsListLevelStartIndexCorrect := AFirstParagraphStartIndex = ASource.Levels[Controller.LevelIndex].ListLevelProperties.Start;
        if FOwner.ContinuePreviousList and AIsListLevelStartIndexCorrect then
        begin
          if AIsPrevParagraphInDifferentList then
            AApplyScope := TdxNumberingListApplyScope.ContinuePreviousList
          else
            AApplyScope := TdxNumberingListApplyScope.ModifyCurrentList;
        end
        else
          AApplyScope := TdxNumberingListApplyScope.RestartNumbering;
      end;
      if AIndexOfParagraphInListBeforeCurrent >= ParagraphIndexMinValue then
        ANewNumbListIndex := AFirstSelectedParagraph.PieceTable.Paragraphs[AIndexOfParagraphInListBeforeCurrent].NumberingListIndex
      else
        ANewNumbListIndex := NumberingListIndexListIndexNotSetted;
      ApplyChanges(ASource, AApplyScope, ANewListType, ANewNumbListIndex);
    end;
  finally
    AForm.Free;
  end;
end;

{ TdxBulletedListFormHelper }

constructor TdxBulletedListFormHelper.Create(const AOwner: IdxRichEditBulletedListForm);
begin
  inherited Create;
  FOwner := AOwner;
end;

procedure TdxBulletedListFormHelper.CreateBulletCharacters;
var
  AFontName: string;
  AActiveProperties: TdxSymbolProperties;
  ASymbolIndex: Integer;
begin
  AFontName := Controller.CharacterProperties.Info.FontName;
  AActiveProperties := TdxSymbolProperties.Create(Controller.DisplayFormat[1], AFontName);
  ASymbolIndex := TdxBulletedListFormHelper.GetActiveSymbolIndex(AActiveProperties);
  if ASymbolIndex = -1 then
    begin
      TdxBulletedListFormHelper.SymbolsProperties[0] := AActiveProperties;
      ASymbolIndex := 0;
    end;
  Owner.InitializeComponents(SymbolsProperties);
  Owner.SelectedIndex := ASymbolIndex;
end;

class function TdxBulletedListFormHelper.GetActiveSymbolIndex(const AActiveProperties: TdxSymbolProperties): Integer;
begin
  Result := Length(SymbolsProperties) - 1;
  while Result >= 0 do
    if AActiveProperties = SymbolsProperties[Result] then
      Break
    else
      Dec(Result);
end;

function TdxBulletedListFormHelper.GetController: TdxBulletedListFormController;
begin
  Result := FOwner.Controller;
end;

{ TdxDisplayFormatHelper }

function TdxDisplayFormatHelper.AppendFormatFromField(const AStringBuilder: TStringBuilder;
  AStartIndex: TdxRunIndex): TdxRunIndex;
var
  S: string;
  AIndex: TdxRunIndex;
  APieceTable: TdxPieceTable;
  ATextBuffer: TdxChunkedStringBuilder;
  ARuns: TdxTextRunCollection;
  ACode: TStringBuilder;
  ARun: TdxTextRunBase;
{$IFDEF DELPHIXE4}
  ACodes: TArray<string>;
{$ELSE}
  ACodes: TStringDynArray;
{$ENDIF}
  AFieldCode: string;
  ALevelIndex: Integer;
begin
  AIndex := AStartIndex;
  APieceTable := FControl.DocumentModel.MainPieceTable;
  ATextBuffer := APieceTable.TextBuffer;
  ARuns := APieceTable.Runs;
  Assert(ARuns[AIndex] is TdxFieldCodeStartRun);
  Inc(AIndex);
  ACode := TStringBuilder.Create;
  try
    while not (ARuns[AIndex] is TdxFieldCodeEndRun) do
    begin
      ARun := ARuns[AIndex];
      if ARun is TdxTextRun then
      begin
        S := TdxTextRunBaseAccess(ARun).GetTextFast(ATextBuffer);
        ACode.Append(S);
      end;
      Inc(AIndex);
    end;
    while not (ARuns[AIndex] is TdxFieldResultEndRun) do
      Inc(AIndex);
  {$IFDEF DELPHIXE4}
    ACodes := ACode.ToString.Trim.ToUpperInvariant.Split([' ']);
  {$ELSE}
    S := TCharacter.ToUpper(Trim(ACode.ToString));
    ACodes := SplitString(S, ' ');
  {$ENDIF}
    if (Length(ACodes) >= 2) then
    begin
      AFieldCode := Trim(ACodes[0]);
      ALevelIndex := IndexOf(FieldNames, Trim(ACodes[1]));
      if (AFieldCode = 'MERGEFIELD') and (ALevelIndex >= 0) then
        AStringBuilder.AppendFormat('%%%d:s', [ALevelIndex]);
    end;
  finally
    Result := AIndex;
    ACode.Free;
  end;
end;

procedure TdxDisplayFormatHelper.CloseBracketInTextState(AChar: Char);
begin
  if AChar <> '}' then
    FCurrentState := nil
  else
  begin
    FStringBuilder.Append('}');
    FCurrentState := TextState;
  end;
end;

procedure TdxDisplayFormatHelper.CloseBracketState(AChar: Char);
begin
  case AChar of
   '}':
     FCurrentState := nil;
   '{':
     FCurrentState := OpenBracketState;
   else
   begin
     FStringBuilder.Append(AChar);
     FCurrentState := TextState;
   end;
  end;
end;

constructor TdxDisplayFormatHelper.Create(const AControl: TdxSimpleRichEditControl;
  const ALevels: TdxListLevelCollection; ALevelIndex: Integer);
begin
  inherited Create;
  FControl := AControl;
  FLevels := ALevels;
  FDataSource := TDataSource.Create(nil);
  FSource := TdxNumberingListDataSet.Create(Self);
  FDataSource.DataSet := FSource;
  FSource.Open;
  FControl.BeginUpdate;
  try
    FControl.Options.MailMerge.DataSource := FDataSource;
    FControl.Options.MailMerge.ViewMergedData := True;
    FControl.Options.Fields.HighlightMode := TdxFieldsHighlightMode.Always;
  finally
    FControl.EndUpdate;
  end;
  FStringBuilder := TStringBuilder.Create;
end;

destructor TdxDisplayFormatHelper.Destroy;
begin
  FStringBuilder.Free;
  FDataSource.Free;
  FSource.Free;
  inherited;
end;

procedure TdxDisplayFormatHelper.FlushAsFormat;
var
  AFieldName: string;
  AFieldCode: string;
  AInsertPosition: TdxDocumentLogPosition;
  AIndex: Integer;
begin
  AIndex := StrToInt(FStringBuilder.ToString);
  AFieldName := FieldNames[AIndex];

  AFieldCode := Format(FieldCodeTemplate, AFieldName);
  AInsertPosition := FControl.DocumentModel.MainPieceTable.DocumentEndLogPosition;
  FControl.DocumentModel.MainPieceTable.InsertText(AInsertPosition, AFieldCode);
  FControl.DocumentModel.MainPieceTable.CreateField(AInsertPosition, Length(AFieldCode));
  FStringBuilder.Clear;
end;

procedure TdxDisplayFormatHelper.FlushAsText;
begin
  if FStringBuilder.Length > 0 then
    FControl.DocumentModel.MainPieceTable.InsertText(FControl.DocumentModel.MainPieceTable.DocumentEndLogPosition,
      FStringBuilder.ToString);
  FStringBuilder.Clear;
end;

procedure TdxDisplayFormatHelper.FormatState(AChar: Char);
begin
   case AChar of
    '}': begin
      FlushAsFormat;
      FCurrentState := CloseBracketState;
    end;
    '{':
      FCurrentState := nil;
    else
      begin
        FStringBuilder.Append(AChar);
        FCurrentState := FormatState;
      end;
   end;
end;

function TdxDisplayFormatHelper.GetDisplayFormatString: string;
var
  S: string;
  APieceTable: TdxPieceTable;
  ATextBuffer: TdxChunkedStringBuilder;
  ARuns: TdxTextRunCollection;
  AIndex: TdxRunIndex;
  AMaxIndex: TdxRunIndex;
  AStringBuilder: TStringBuilder;
  ARun: TdxTextRunBase;
begin
  APieceTable := FControl.DocumentModel.MainPieceTable;
  ATextBuffer := APieceTable.TextBuffer;
  ARuns := APieceTable.Runs;
  AIndex := 0;
  AMaxIndex := ARuns.Count - 1;
  AStringBuilder := TStringBuilder.Create;
  try
    while (AIndex <= AMaxIndex) do
    begin
      ARun := ARuns[AIndex];
      if (ARun is TdxFieldCodeStartRun) then
        AIndex := AppendFormatFromField(AStringBuilder, AIndex)
      else
        if (ARun is TdxTextRun) then
        begin
          S := TdxTextRunBaseAccess(ARun).GetTextFast(ATextBuffer);
          S := StringReplace(S, '%', '%%', [rfReplaceAll]);
          AStringBuilder.Append(S);
        end;
      Inc(AIndex);
    end;
    Result := AStringBuilder.ToString;
  finally
    AStringBuilder.Free;
  end;
end;

class function TdxDisplayFormatHelper.IndexOf(const AArray: array of string; const AValue: string): Integer;
var
  I: Integer;
begin
  I := Length(AArray) - 1;
  while (I >= 0) and not SameText(AArray[I], AValue) do
    Dec(I);
  Result := I;
end;

procedure TdxDisplayFormatHelper.OpenBracketState(AChar: Char);
begin
  case AChar of
    '{': begin
      FStringBuilder.Append('{');
      FCurrentState := TextState;
    end;
    '}':
      FCurrentState := nil;
    else
    begin
      FlushAsText;
      FormatState(AChar);
      FCurrentState := FormatState;
    end;
  end;
end;

procedure TdxDisplayFormatHelper.ProcessChar(AChar: Char);
begin
  FCurrentState(AChar);
  if not Assigned(FCurrentState) then
    TdxRichEditExceptions.ThrowInternalException;
end;

function Compare(const A, B: TdxDisplayFormatHelper.TdxState): Boolean; inline;
begin
  Result := @A = @B;
end;

procedure TdxDisplayFormatHelper.ProcessEndOfFormat;
begin
  if Compare(FCurrentState, CloseBracketState) then
    FCurrentState := TextState;
  if not Compare(FCurrentState, TextState) then
    TdxRichEditExceptions.ThrowInternalException;
  FlushAsText;
end;

procedure TdxDisplayFormatHelper.SetDisplayFormat(const AFormat: string);
var
  C: Char;
  APrepareFormat: string;
begin
  APrepareFormat := Format(AFormat, ['{0}','{1}','{2}','{3}','{4}','{5}','{6}','{7}','{8}']);

  FCurrentState := TextState;
  FStringBuilder.Clear;
  FControl.BeginUpdate;
  try
    FControl.CreateNewDocument;
    FControl.DocumentModel.BeginUpdate;
    try
      for C in APrepareFormat do
        ProcessChar(C);
      ProcessEndOfFormat;
      Update;
    finally
      FControl.DocumentModel.EndUpdate;
    end;
  finally
    FControl.EndUpdate;
  end;
end;

procedure TdxDisplayFormatHelper.TextState(AChar: Char);
begin
  case AChar of
    '{': FCurrentState := OpenBracketState;
    '}': FCurrentState := CloseBracketInTextState;
  else
    begin
      FStringBuilder.Append(AChar);
      FCurrentState := TextState;
    end;
  end;
end;

procedure TdxDisplayFormatHelper.Update;
var
  APieceTable: TdxPieceTable;
  AFields: TdxFieldCollection;
  I, ACount: Integer;
  AUpdater: TdxFieldUpdater;
  AUpdateResult: TdxUpdateFieldOperationResult;
begin
  APieceTable := FControl.DocumentModel.MainPieceTable;
  AFields := APieceTable.Fields;
  ACount := AFields.Count;
  AUpdater := APieceTable.FieldUpdater;
  for I := 0 to ACount - 1 do
  begin
    AUpdateResult := AUpdater.UpdateField(AFields[I], TdxMailMergeDataMode.ViewMergedData);
    AUpdateResult.Free;
  end;
end;

{ TdxNumberingListDataSet }

function TdxNumberingListDataSet.AllocRecordBuffer: TRecordBuffer;
begin
  Result := AllocMem(RecBufSize);
end;

constructor TdxNumberingListDataSet.Create(AOwner: TdxDisplayFormatHelper);
begin
  inherited Create(nil);
  FNumberingListTextSource := TdxNumberingListTextSource.Create(AOwner);
end;

procedure TdxNumberingListDataSet.FreeRecordBuffer(var Buffer: TRecordBuffer);
begin
  FreeMem(Buffer);
  Buffer := nil;
end;

function TdxNumberingListDataSet.GetCanModify: Boolean;
begin
  Result := True;
end;

function TdxNumberingListDataSet.GetFieldData(Field: TField; {$IFDEF DELPHI18}var{$ENDIF} Buffer: TValueBuffer): Boolean;
var
  AValue: string;
{$IFNDEF DELPHIXE3}
  ATempBuff: UnicodeString;
{$ELSE}
  ATempBuff: TValueBuffer;
{$ENDIF}
begin
  Result := True;
  Assert(Field.DataType = ftWideString);
  AValue := FNumberingListTextSource.FormatLevelText(Field.FieldNo - 1);
{$IFNDEF DELPHIXE3}
  ATempBuff := UnicodeString(AValue);
  FillChar(Buffer^, Field.DataSize, 0);
  Move(PChar(ATempBuff)^, Buffer^, Length(ATempBuff) * SizeOf(Char));
{$ELSE}
  ATempBuff := TEncoding.Unicode.GetBytes(AValue);
  Move(ATempBuff[0], Buffer[0], Length(ATempBuff));
{$ENDIF}
end;

function TdxNumberingListDataSet.GetRecord(Buffer: PByte; GetMode: TGetMode; DoCheck: Boolean): TGetResult;
begin
  case GetMode of
    gmNext:
      Result := grEof;
    gmPrior:
      Result := grBof;
    else
      Result := grOK;
  end;
end;

function TdxNumberingListDataSet.GetRecordSize: Word;
begin
  Result := RecBufSize;
end;

procedure TdxNumberingListDataSet.InternalClose;
begin
  BindFields(False);
  DestroyFields;
  FActive := False;
end;

procedure TdxNumberingListDataSet.InternalHandleException;
begin
  TdxRichEditExceptions.ThrowInternalException;
end;

procedure TdxNumberingListDataSet.InternalInitFieldDefs;
var
  AFieldName: string;
  AFieldDef: TFieldDef;
begin
  FieldDefs.BeginUpdate;
  try
    FieldDefs.Clear;
    for AFieldName in TdxDisplayFormatHelper.FieldNames do
    begin
      AFieldDef := FieldDefs.AddFieldDef;
      AFieldDef.Name := AFieldName;
      AFieldDef.DataType := ftWideString;
      AFieldDef.Size := MaxFieldSize;
      AFieldDef.Required := False;
    end;
  finally
    FieldDefs.EndUpdate;
  end;
end;

procedure TdxNumberingListDataSet.InternalInitRecord(Buffer: TRecordBuffer);
begin
  //do nothing
end;

procedure TdxNumberingListDataSet.InternalOpen;
begin
  InternalInitFieldDefs;
  CreateFields;
  BindFields(True);
  FActive := True;
end;

function TdxNumberingListDataSet.IsCursorOpen: Boolean;
begin
  Result := FActive;
end;

{ TdxNumberingListDataSet.TdxNumberingListTextSource }

constructor TdxNumberingListDataSet.TdxNumberingListTextSource.Create(const AOwner: TdxDisplayFormatHelper);
begin
  FOwner := AOwner;
end;

function TdxNumberingListDataSet.TdxNumberingListTextSource.FormatLevelText(ALevelIndex: Integer): string;
var
  AFormatString: string;
begin
  AFormatString := Format('%%%d:s', [ALevelIndex]);
  Result := TdxParagraph.Format(AFormatString, GetListCounters, Levels);
end;

function TdxNumberingListDataSet.TdxNumberingListTextSource.GetLevels: TdxListLevelCollection;
begin
  Result := FOwner.Levels;
end;

function TdxNumberingListDataSet.TdxNumberingListTextSource.GetListCounters: TIntegerDynArray;
var
  I: Integer;
  ACount: Integer;
begin
  ACount := Levels.Count;
  SetLength(Result, ACount);
  for I := 0 to ACount - 1 do
    Result[I] := Levels[i].ListLevelProperties.Start;
end;

end.
