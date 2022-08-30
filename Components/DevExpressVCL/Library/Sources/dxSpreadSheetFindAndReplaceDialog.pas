{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressSpreadSheet                                       }
{                                                                    }
{           Copyright (c) 2001-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSSPREADSHEET CONTROL AND ALL    }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY. }
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

unit dxSpreadSheetFindAndReplaceDialog;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, Menus, StdCtrls, ExtCtrls,
  Generics.Defaults, Generics.Collections,
  cxClasses, dxSpreadSheetCore, dxSpreadSheetStrs, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters,
  cxButtons, dxLayoutLookAndFeels, dxLayoutContainer, dxLayoutControl, cxContainer, cxEdit, dxLayoutControlAdapters,
  cxListBox, dxLayoutcxEditAdapters, cxLabel, cxTextEdit, cxDropDownEdit, cxCheckBox, dxSpreadSheetGraphics, dxCore,
  cxCheckListBox, dxSpreadSheetClasses, dxSpreadSheetTypes, cxMaskEdit, cxMCListBox, dxForms, dxMessages,
  cxCustomListBox;

type
  TdxSpreadSheetFindAndReplaceDialogLookMode = (fdlmInFormulas, fdlmInValues);

  TdxSpreadSheetFindAndReplaceDialogMatchMode = (fdmmCase, fdmmEntireCell);
  TdxSpreadSheetFindAndReplaceDialogMatchModes = set of TdxSpreadSheetFindAndReplaceDialogMatchMode;

  TdxSpreadSheetFindAndReplaceDialogSearchMode = (fdsmByRows, fdsmByColumns);
  TdxSpreadSheetFindAndReplaceDialogSearchRange = (fdsrSheet, fdsrAllSheets);

  { TdxSpreadSheetFindAndReplaceDialogForm }

  TdxSpreadSheetFindAndReplaceDialogFormClass = class of TdxSpreadSheetFindAndReplaceDialogForm;
  TdxSpreadSheetFindAndReplaceDialogForm = class(TdxForm,
    IdxSpreadSheetListener,
    IcxLookAndFeelNotificationListener,
    IcxLookAndFeelNotificationListener2
  )
    btnClose: TcxButton;
    btnFindAll: TcxButton;
    btnFindNext: TcxButton;
    btnOptionsFind: TcxButton;
    btnOptionsReplace: TcxButton;
    btnReplace: TcxButton;
    btnReplaceAll: TcxButton;
    chkEntireCellFind: TcxCheckBox;
    chkEntireCellReplace: TcxCheckBox;
    chkMatchCaseFind: TcxCheckBox;
    chkMatchCaseReplace: TcxCheckBox;
    cmbLookInFind: TcxComboBox;
    cmbLookInReplace: TcxComboBox;
    cmbSearchModeFind: TcxComboBox;
    cmbSearchModeReplace: TcxComboBox;
    cmbWhatFind: TcxComboBox;
    cmbWhatReplace: TcxComboBox;
    cmbWithinRangeFind: TcxComboBox;
    cmbWithinRangeReplace: TcxComboBox;
    cmbWithReplace: TcxComboBox;
    dxLayoutAutoCreatedGroup2: TdxLayoutAutoCreatedGroup;
    dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel;
    dxLayoutGroup1: TdxLayoutGroup;
    dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList;
    lbFoundCellsInfo: TcxLabel;
    lcFindHorzAlignSpaceItem: TdxLayoutEmptySpaceItem;
    lcFindSpaceItem: TdxLayoutEmptySpaceItem;
    lcgGroupOptionsFind: TdxLayoutGroup;
    lcgGroupOptionsReplace: TdxLayoutGroup;
    lcgReplaceButtons: TdxLayoutGroup;
    lcgTabFind: TdxLayoutGroup;
    lcgTabReplace: TdxLayoutGroup;
    lciEntireCellFind: TdxLayoutItem;
    lciEntireCellReplace: TdxLayoutItem;
    lciLookInFind: TdxLayoutItem;
    lciLookInReplace: TdxLayoutItem;
    lciMatchCaseFind: TdxLayoutItem;
    lciMatchCaseReplace: TdxLayoutItem;
    lciSearchModeFind: TdxLayoutItem;
    lciSearchModeReplace: TdxLayoutItem;
    lciWhatFind: TdxLayoutItem;
    lciWhatReplace: TdxLayoutItem;
    lciWithinRangeFind: TdxLayoutItem;
    lciWithinRangeReplace: TdxLayoutItem;
    lciWithReplace: TdxLayoutItem;
    lcMain: TdxLayoutControl;
    lcMainContentGroup: TdxLayoutGroup;
    lcMainGroup_Root: TdxLayoutGroup;
    lcMainGroup11: TdxLayoutAutoCreatedGroup;
    lcMainGroup14: TdxLayoutGroup;
    lcMainGroup2: TdxLayoutAutoCreatedGroup;
    lcMainGroup3: TdxLayoutAutoCreatedGroup;
    lcMainGroup4: TdxLayoutAutoCreatedGroup;
    lcMainGroup5: TdxLayoutAutoCreatedGroup;
    lcMainGroup8: TdxLayoutAutoCreatedGroup;
    lcMainItem1: TdxLayoutItem;
    lcMainItem2: TdxLayoutItem;
    lcMainItem3: TdxLayoutItem;
    lcMainItem4: TdxLayoutItem;
    lcMainItem5: TdxLayoutItem;
    lcMainItem6: TdxLayoutItem;
    lcMainItem7: TdxLayoutItem;
    lcMainSpaceItem1: TdxLayoutEmptySpaceItem;
    lcMainSpaceItem2: TdxLayoutEmptySpaceItem;
    lgOptionsHeightAdjusting: TdxLayoutGroup;
    liOptionsHeightAdjustingItem1: TdxLayoutEmptySpaceItem;
    liOptionsHeightAdjustingItem2: TdxLayoutEmptySpaceItem;
    liOptionsHeightAdjustingItem3: TdxLayoutEmptySpaceItem;
    mcFindAllResults: TcxMCListBox;

    procedure btnCloseClick(Sender: TObject);
    procedure btnFindAllClick(Sender: TObject);
    procedure btnFindNextClick(Sender: TObject);
    procedure btnOptionsFindClick(Sender: TObject);
    procedure btnReplaceAllClick(Sender: TObject);
    procedure btnReplaceClick(Sender: TObject);
    procedure chkFindEntireCellPropertiesChange(Sender: TObject);
    procedure chkFindMatchCasePropertiesChange(Sender: TObject);
    procedure cmbFindSearchModePropertiesChange(Sender: TObject);
    procedure cmbFindWhatPropertiesChange(Sender: TObject);
    procedure cmbFindWithinRangePropertiesChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lcMainContentGroupTabChanged(Sender: TObject);
    procedure mcFindAllResultsClick(Sender: TObject);
  strict private
    FIsFormCreateProcessing: Boolean;
    FIsFormSizing: Boolean;
    FPriorClientHeight: Integer;
    FPriorClientWidth: Integer;
    FPriorLayoutHeight: Integer;
    FPriorLayoutWidth: Integer;
    FReplacementCount: Integer;

    FFoundReferences: TcxObjectList;
    FLessOptionsCaption: string;
    FMoreOptionsCaption: string;
    FMasterLookAndFeel: TcxLookAndFeel;
    FSpreadSheet: TdxCustomSpreadSheet;
    FSpreadSheets: TList;

    procedure AddSearchResult(ACell: TdxSpreadSheetCell);
    procedure AdjustOptionsHeight;
    procedure CheckFirstRowAndColumn(var AFirstRow: TdxSpreadSheetTableRow; var AFirstColumn: TdxSpreadSheetTableColumn;
      const AStartRowIndex, AStartColumnIndex: Integer);
    procedure CheckReplaceButtonsVisibility;
    procedure ClearSearchResults;
    function CreateCellName(const ARowIndex, AColumnIndex: Integer): string;
    function CreateSearchResultString(ASheet: TdxSpreadSheetTableView; const ARowIndex, AColumnIndex: Integer): string;
    procedure DoFindAll(ASheet: TdxSpreadSheetTableView);
    function DoFindNextCell(ASheet: TdxSpreadSheetTableView; const AStartRowIndex, AStartColumnIndex: Integer): TdxSpreadSheetCell;
    procedure DoReplaceAll(ASheet: TdxSpreadSheetTableView);
    procedure DoReplacement(ACell: TdxSpreadSheetCell);
    function FoundNextCell(AStartSheet: TdxSpreadSheetTableView): TdxSpreadSheetCell;
    function GetDefinedName(ASheet: TdxSpreadSheetTableView; const ARowIndex, AColumnIndex: Integer): string;
    function GetFormulaText(ACell: TdxSpreadSheetCell): string;
    function GetMinClientHeight: Integer;
    procedure GetNextRowAndColumn(var ARow: TdxSpreadSheetTableRow; var AColumn: TdxSpreadSheetTableColumn;
      AFirstRow: TdxSpreadSheetTableRow; AFirstColumn: TdxSpreadSheetTableColumn);
    function GetReplacementPos(const AWhatReplace, AWhereReplace: string; const AOffset: Integer): Integer;
    function GetSearchArea(const AStartRowIndex, AStartColumnIndex: Integer): TRect;
    procedure GetStartIndexesForFindAll(var ARowIndex, AColumnIndex: Integer);
    function IsRemoved(ASheet: TdxSpreadSheetTableView): Boolean;
    procedure SaveComboHistory(ACombo: TcxComboBox);
    procedure SetMasterLookAndFeel(ALookAndFeel: TcxLookAndFeel);
    procedure ShowInfoMessage(AMessage: string; AType: Cardinal);
    procedure ShowSearchResults;
    procedure SynchronizeLookMode;

    function GetLookMode: TdxSpreadSheetFindAndReplaceDialogLookMode;
    function GetMatchModes: TdxSpreadSheetFindAndReplaceDialogMatchModes;
    function GetSearchMode: TdxSpreadSheetFindAndReplaceDialogSearchMode;
    function GetSearchRange: TdxSpreadSheetFindAndReplaceDialogSearchRange;
  protected
    procedure AdjustLayout;
    procedure ApplyLocalization; virtual;
    function CheckCondition(ACell: TdxSpreadSheetCell): Boolean;
    procedure CheckFoundCellsInfo;
    procedure CheckReplacementPossibility(AView: TdxSpreadSheetTableView; const AIsReplaceAll: Boolean);
    procedure CleanSearchResults(ASpreadSheet: TdxCustomSpreadSheet);
    procedure DoShowCell(AIndex: Integer);
    procedure Initialize(ASpreadSheet: TdxCustomSpreadSheet; AActivePage: Integer);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure ScaleFactorChanged(M, D: Integer); override;
    procedure WndProc(var Message: TMessage); override;

    class procedure Register(ASpreadSheet: TdxCustomSpreadSheet);
    class procedure Unregister(ASpreadSheet: TdxCustomSpreadSheet);

    // IdxSpreadSheetListener
    procedure DataChanged(Sender: TdxCustomSpreadSheet);

    // IcxLookAndFeelNotificationListener
    function GetObject: TObject;
    procedure MasterLookAndFeelChanged(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues);
    procedure MasterLookAndFeelDestroying(Sender: TcxLookAndFeel);

    // IcxLookAndFeelNotificationListener2
    procedure MasterLookAndFeelBeginChange;
    procedure MasterLookAndFeelEndChange;

    property LookMode: TdxSpreadSheetFindAndReplaceDialogLookMode read GetLookMode;
    property MasterLookAndFeel: TcxLookAndFeel read FMasterLookAndFeel write SetMasterLookAndFeel;
    property MatchModes: TdxSpreadSheetFindAndReplaceDialogMatchModes read GetMatchModes;
    property ReplacementCount: Integer read FReplacementCount;
    property SearchMode: TdxSpreadSheetFindAndReplaceDialogSearchMode read GetSearchMode;
    property SearchRange: TdxSpreadSheetFindAndReplaceDialogSearchRange read GetSearchRange;
    property SpreadSheet: TdxCustomSpreadSheet read FSpreadSheet;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  dxSpreadSheetFindAndReplaceDialogClass: TdxSpreadSheetFindAndReplaceDialogFormClass = TdxSpreadSheetFindAndReplaceDialogForm;

procedure ShowFindAndReplaceDialog(ASpreadSheet: TdxCustomSpreadSheet; const AActivePage: Integer = 0);

implementation

uses
  Math, dxTypeHelpers, dxHashUtils, cxDrawTextUtils, dxCoreGraphics, cxGeometry, dxSpreadSheetUtils,
  dxSpreadSheetDialogStrs, dxSpreadSheetNumberFormat, StrUtils, cxFormats, dxSpreadSheetCoreStrs;

{$R *.dfm}

type
  TdxTableViewAccess = class(TdxSpreadSheetTableView);
  TdxTableViewControllerAccess = class(TdxSpreadSheetTableViewController);
  TdxSpreadSheetAccess = class(TdxCustomSpreadSheet);

  { TdxFindAndReplaceDialogFoundReference }

  TdxFindAndReplaceDialogFoundReference = class
  public
    Sheet: TdxSpreadSheetTableView;
    RowIndex, ColumnIndex: Integer;

    constructor Create(ASheet: TdxSpreadSheetTableView; const ARowIndex, AColumnIndex: Integer);
  end;

var
  FFindAndReplaceDialog: TdxSpreadSheetFindAndReplaceDialogForm;

{ TdxFindAndReplaceDialogFoundReference }

constructor TdxFindAndReplaceDialogFoundReference.Create(ASheet: TdxSpreadSheetTableView; const ARowIndex, AColumnIndex: Integer);
begin
  inherited Create;
  Sheet := ASheet;
  RowIndex := ARowIndex;
  ColumnIndex := AColumnIndex;
end;

{ TdxSpreadSheetFindAndReplaceDialogForm }

constructor TdxSpreadSheetFindAndReplaceDialogForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFoundReferences := TcxObjectList.Create;
  FSpreadSheets := TList.Create;
  ApplyLocalization;
end;

destructor TdxSpreadSheetFindAndReplaceDialogForm.Destroy;
begin
  FreeAndNil(FFoundReferences);
  FreeAndNil(FSpreadSheets);
  inherited Destroy;
end;

procedure TdxSpreadSheetFindAndReplaceDialogForm.AddSearchResult(ACell: TdxSpreadSheetCell);
var
  AReference: TdxFindAndReplaceDialogFoundReference;
begin
  AReference := TdxFindAndReplaceDialogFoundReference.Create(ACell.View, ACell.RowIndex, ACell.ColumnIndex);
  mcFindAllResults.AddItem(CreateSearchResultString(ACell.View, ACell.RowIndex, ACell.ColumnIndex), AReference);
  FFoundReferences.Add(AReference);
end;

procedure TdxSpreadSheetFindAndReplaceDialogForm.AdjustOptionsHeight;
begin
  lcFindSpaceItem.SizeOptions.Height := cmbWithReplace.Height;
  liOptionsHeightAdjustingItem1.SizeOptions.Height := lcFindSpaceItem.SizeOptions.Height;
  liOptionsHeightAdjustingItem2.SizeOptions.Height := lcFindSpaceItem.SizeOptions.Height;
  liOptionsHeightAdjustingItem3.SizeOptions.Height := lcFindSpaceItem.SizeOptions.Height;
end;

function TdxSpreadSheetFindAndReplaceDialogForm.CreateCellName(const ARowIndex, AColumnIndex: Integer): string;
begin
  Result := dxReferenceToString(ARowIndex, AColumnIndex,
    SpreadSheet.OptionsView.R1C1Reference, [croAbsoluteColumn, croAbsoluteRow]);
end;

function TdxSpreadSheetFindAndReplaceDialogForm.CreateSearchResultString(ASheet: TdxSpreadSheetTableView; const ARowIndex, AColumnIndex: Integer): string;
var
  ADelimiter: Char;
  AName, ACellName, AFormula: string;
  AValue: Variant;
  ACell: TdxSpreadSheetCell;
begin
  AName := GetDefinedName(ASheet, ARowIndex, AColumnIndex);
  AValue := '';
  AFormula := '';
  ACell := ASheet.Cells[ARowIndex, AColumnIndex];
  if ACell <> nil then
  begin
    AValue := ACell.DisplayValue.Text;
    if ACell.IsFormula then
      AFormula := ACell.AsFormula.AsText;
  end;
  ACellName := CreateCellName(ARowIndex, AColumnIndex);
  ADelimiter := mcFindAllResults.Delimiter;
  Result := ASheet.Caption + ADelimiter + AName + ADelimiter + ACellName + ADelimiter +
    VarToStr(AValue) + ADelimiter + AFormula + ADelimiter;
end;

function TdxSpreadSheetFindAndReplaceDialogForm.GetDefinedName(ASheet: TdxSpreadSheetTableView; const ARowIndex, AColumnIndex: Integer): string;
var
  I, P: Integer;
  AName: TdxSpreadSheetDefinedName;
  AReference, ARefSheet: string;
begin
  Result := '';
  for I := 0 to SpreadSheet.DefinedNames.Count - 1 do
  begin
    AName := SpreadSheet.DefinedNames[I];
    AReference := AName.Reference;
    if Pos(dxAreaSeparator, AReference) > 0 then
      Continue;
    P := Pos(dxRefSeparator, AReference);
    if P > 0 then
    begin
      ARefSheet := Copy(AReference, 3, P - 4);
      if SameText(ARefSheet, ASheet.Caption) and
         SameText(Copy(AReference, P + 1, Length(AReference) - P + 1), CreateCellName(ARowIndex, AColumnIndex)) then
        Result := AName.Caption;
    end;
  end;
end;

function TdxSpreadSheetFindAndReplaceDialogForm.GetFormulaText(ACell: TdxSpreadSheetCell): string;
begin
  if ACell.IsFormula then
    Result := ACell.AsFormula.AsText
  else
    Result := VarToStr(ACell.AsVariant);
end;

function TdxSpreadSheetFindAndReplaceDialogForm.GetMinClientHeight: Integer;
begin
  Result := mcFindAllResults.Top - 1;
end;

procedure TdxSpreadSheetFindAndReplaceDialogForm.AdjustLayout;
var
  dH, dW: Integer;
begin
  dH := Max(FPriorClientHeight - ClientHeight + lcMain.OccupiedClientHeight - FPriorLayoutHeight, 0);
  dW := Max(FPriorClientWidth - ClientWidth + lcMain.OccupiedClientWidth - FPriorLayoutWidth, 0);

  Constraints.MinHeight := 0;
  Constraints.MinWidth := 0;

  Height := Height + dH;
  Width := Width + dW;

  FPriorLayoutHeight := Max(lcMain.Height, lcMain.OccupiedClientHeight);
  FPriorLayoutWidth := Max(lcMain.Width, lcMain.OccupiedClientWidth);
  FPriorClientHeight := ClientHeight;
  FPriorClientWidth := ClientWidth;

  Constraints.MinHeight := lcMain.OccupiedClientHeight + Height - ClientHeight;
  Constraints.MinWidth := lcMain.OccupiedClientWidth + Width - ClientWidth;
end;

procedure TdxSpreadSheetFindAndReplaceDialogForm.ApplyLocalization;
begin
  // Common
  Caption := cxGetResourceString(@sdxFindAndReplaceDialogCaption);

  FLessOptionsCaption := cxGetResourceString(@sdxFindAndReplaceDialogLessOptions);
  FMoreOptionsCaption := cxGetResourceString(@sdxFindAndReplaceDialogMoreOptions);

  btnReplaceAll.Caption := cxGetResourceString(@sdxFindAndReplaceDialogButtonReplaceAll);
  btnReplace.Caption := cxGetResourceString(@sdxFindAndReplaceDialogButtonReplace);
  btnFindAll.Caption := cxGetResourceString(@sdxFindAndReplaceDialogButtonFindAll);
  btnFindNext.Caption := cxGetResourceString(@sdxFindAndReplaceDialogButtonFindNext);
  btnClose.Caption := cxGetResourceString(@sdxFindAndReplaceDialogButtonClose);

  // FindAll results
  mcFindAllResults.HeaderSections[0].Text := cxGetResourceString(@sdxFindAndReplaceDialogColumnSheetCaption);
  mcFindAllResults.HeaderSections[1].Text := cxGetResourceString(@sdxFindAndReplaceDialogColumnNameCaption);
  mcFindAllResults.HeaderSections[2].Text := cxGetResourceString(@sdxFindAndReplaceDialogColumnCellCaption);
  mcFindAllResults.HeaderSections[3].Text := cxGetResourceString(@sdxFindAndReplaceDialogColumnValueCaption);
  mcFindAllResults.HeaderSections[4].Text := cxGetResourceString(@sdxFindAndReplaceDialogColumnFormulaCaption);

  // Find page
  lcgTabFind.Caption := cxGetResourceString(@sdxFindAndReplaceDialogFindTabCaption);

  lciWhatFind.Caption := cxGetResourceString(@sdxFindAndReplaceDialogFindWhat);

  btnOptionsFind.Caption := FMoreOptionsCaption;

  lciWithinRangeFind.Caption := cxGetResourceString(@sdxFindAndReplaceDialogWithinRange);
  cmbWithinRangeFind.Properties.Items.Clear;
  cmbWithinRangeFind.Properties.Items.Add(cxGetResourceString(@sdxFindAndReplaceDialogWithinRangeSheet));
  cmbWithinRangeFind.Properties.Items.Add(cxGetResourceString(@sdxFindAndReplaceDialogWithinRangeSpreadsheet));
  cmbWithinRangeFind.ItemIndex := 0;

  lciSearchModeFind.Caption  := cxGetResourceString(@sdxFindAndReplaceDialogSearchMode);
  cmbSearchModeFind.Properties.Items.Clear;
  cmbSearchModeFind.Properties.Items.Add(cxGetResourceString(@sdxFindAndReplaceDialogSearchModeByRows));
  cmbSearchModeFind.Properties.Items.Add(cxGetResourceString(@sdxFindAndReplaceDialogSearchModeByColumns));
  cmbSearchModeFind.ItemIndex := 0;

  lciLookInFind.Caption  := cxGetResourceString(@sdxFindAndReplaceDialogLookIn);
  cmbLookInFind.Properties.Items.Clear;
  cmbLookInFind.Properties.Items.Add(cxGetResourceString(@sdxFindAndReplaceDialogLookInFormulas));
  cmbLookInFind.Properties.Items.Add(cxGetResourceString(@sdxFindAndReplaceDialogLookInValues));
  cmbLookInFind.ItemIndex := 0;

  chkMatchCaseFind.Caption := cxGetResourceString(@sdxFindAndReplaceDialogMatchCase);
  chkEntireCellFind.Caption := cxGetResourceString(@sdxFindAndReplaceDialogMatchEntireCell);

  // Replace page
  lcgTabReplace.Caption := cxGetResourceString(@sdxFindAndReplaceDialogReplaceTabCaption);

  lciWhatReplace.Caption := lciWhatFind.Caption;
  lciWithReplace.Caption := cxGetResourceString(@sdxFindAndReplaceDialogReplaceWith);

  btnOptionsReplace.Caption := btnOptionsFind.Caption;

  lciWithinRangeReplace.Caption := lciWithinRangeFind.Caption;
  cmbWithinRangeReplace.Properties.Items.Clear;
  cmbWithinRangeReplace.Properties.Items.Add(cxGetResourceString(@sdxFindAndReplaceDialogWithinRangeSheet));
  cmbWithinRangeReplace.Properties.Items.Add(cxGetResourceString(@sdxFindAndReplaceDialogWithinRangeSpreadsheet));
  cmbWithinRangeReplace.ItemIndex := 0;

  lciSearchModeReplace.Caption  := cxGetResourceString(@sdxFindAndReplaceDialogSearchMode);
  cmbSearchModeReplace.Properties.Items.Clear;
  cmbSearchModeReplace.Properties.Items.Add(cxGetResourceString(@sdxFindAndReplaceDialogSearchModeByRows));
  cmbSearchModeReplace.Properties.Items.Add(cxGetResourceString(@sdxFindAndReplaceDialogSearchModeByColumns));
  cmbSearchModeReplace.ItemIndex := 0;

  lciLookInReplace.Caption  := lciLookInFind.Caption;
  cmbLookInReplace.Properties.Items.Clear;
  cmbLookInReplace.Properties.Items.Add(cxGetResourceString(@sdxFindAndReplaceDialogLookInFormulas));
  cmbLookInReplace.ItemIndex := 0;

  chkMatchCaseReplace.Caption := chkMatchCaseFind.Caption;
  chkEntireCellReplace.Caption := chkEntireCellFind.Caption;
end;

procedure TdxSpreadSheetFindAndReplaceDialogForm.ClearSearchResults;
var
  I: Integer;
begin
  FFoundReferences.Clear;
  mcFindAllResults.Items.Clear;
  for I := 0 to mcFindAllResults.HeaderSections.Count - 1 do
    mcFindAllResults.HeaderSections[I].SortOrder := soNone;
  CheckFoundCellsInfo;
end;

function TdxSpreadSheetFindAndReplaceDialogForm.CheckCondition(ACell: TdxSpreadSheetCell): Boolean;
var
  AWhatFind, AWhereFind: string;
begin
  Result := ACell <> nil;
  if not Result then Exit;

  AWhatFind := cmbWhatFind.Text;
  if LookMode = fdlmInValues then
    AWhereFind := ACell.DisplayText
  else
    AWhereFind := GetFormulaText(ACell);

  if not (fdmmCase in MatchModes) then
  begin
    AWhatFind := AnsiUpperCase(AWhatFind);
    AWhereFind := AnsiUpperCase(AWhereFind);
  end;

  if fdmmEntireCell in MatchModes then
    Result := AWhatFind = AWhereFind
  else
    Result := Pos(AWhatFind, AWhereFind) > 0;
end;

procedure TdxSpreadSheetFindAndReplaceDialogForm.CheckFoundCellsInfo;
begin
  lbFoundCellsInfo.Caption := Format(cxGetResourceString(@sdxFindAndReplaceDialogStatusBarCellsFound), [mcFindAllResults.Count]);
end;

procedure TdxSpreadSheetFindAndReplaceDialogForm.CheckFirstRowAndColumn(var AFirstRow: TdxSpreadSheetTableRow;
  var AFirstColumn: TdxSpreadSheetTableColumn; const AStartRowIndex, AStartColumnIndex: Integer);
begin
  if SearchMode = fdsmByRows then
  begin
    if (AFirstRow <> nil) and (AFirstRow.Index = AStartRowIndex) then
      while (AFirstColumn <> nil) and (AFirstColumn.Index <= AStartColumnIndex) do
        AFirstColumn := TdxSpreadSheetTableColumn(AFirstColumn.Next);
  end
  else
    if (AFirstColumn <> nil) and (AFirstColumn.Index = AStartColumnIndex) then
      while (AFirstRow <> nil) and (AFirstRow.Index <= AStartRowIndex) do
        AFirstRow := TdxSpreadSheetTableRow(AFirstRow.Next);
end;

procedure TdxSpreadSheetFindAndReplaceDialogForm.CheckReplaceButtonsVisibility;
begin
  lcgReplaceButtons.Visible := lcMainContentGroup.ItemIndex = 1;
end;

procedure TdxSpreadSheetFindAndReplaceDialogForm.CheckReplacementPossibility(AView: TdxSpreadSheetTableView;
  const AIsReplaceAll: Boolean);
begin
  if not(AIsReplaceAll and (SearchRange = fdsrAllSheets)) and AView.Options.Protected then
    raise EdxSpreadSheetError.Create(cxGetResourceString(@sdxErrorCannotExecuteActionOnProtectedSheet));
end;

procedure TdxSpreadSheetFindAndReplaceDialogForm.CleanSearchResults(ASpreadSheet: TdxCustomSpreadSheet);
var
  I: Integer;
  AReference: TdxFindAndReplaceDialogFoundReference;
begin
  I := 0;
  mcFindAllResults.Items.BeginUpdate;
  try
    while I <= mcFindAllResults.Items.Count - 1 do
    begin
      AReference := mcFindAllResults.Items.Objects[I] as TdxFindAndReplaceDialogFoundReference;
      if AReference.Sheet.SpreadSheet = ASpreadSheet then
      begin
        FFoundReferences.Delete(FFoundReferences.IndexOf(AReference));
        mcFindAllResults.Items.Delete(I);
        AReference.Free;
      end
      else
        Inc(I);
    end;
  finally
    mcFindAllResults.Items.EndUpdate;
  end;
end;

// IdxSpreadSheetListener
procedure TdxSpreadSheetFindAndReplaceDialogForm.DataChanged(Sender: TdxCustomSpreadSheet);
var
  I: Integer;
  AReference: TdxFindAndReplaceDialogFoundReference;
  st: string;
begin
  mcFindAllResults.Items.BeginUpdate;
  try
    for I := mcFindAllResults.Items.Count - 1 downto 0 do
    begin
      AReference := mcFindAllResults.Items.Objects[I] as TdxFindAndReplaceDialogFoundReference;
      if IsRemoved(AReference.Sheet) then
        mcFindAllResults.Items.Delete(I)
      else
      begin
        st := CreateSearchResultString(AReference.Sheet, AReference.RowIndex, AReference.ColumnIndex);
        if not SameText(st, mcFindAllResults.Items[I]) then
          mcFindAllResults.Items[I] := st;
      end;
    end;
  finally
    mcFindAllResults.Items.EndUpdate;
  end;
end;

// IcxLookAndFeelNotificationListener
function TdxSpreadSheetFindAndReplaceDialogForm.GetObject: TObject;
begin
  Result := Self;
end;

procedure TdxSpreadSheetFindAndReplaceDialogForm.MasterLookAndFeelChanged(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues);
begin
  // do nothing
end;

procedure TdxSpreadSheetFindAndReplaceDialogForm.MasterLookAndFeelDestroying(Sender: TcxLookAndFeel);
begin
  MasterLookAndFeel := nil;
end;

// IcxLookAndFeelNotificationListener2
procedure TdxSpreadSheetFindAndReplaceDialogForm.MasterLookAndFeelBeginChange;
begin
  // do nothing
end;

procedure TdxSpreadSheetFindAndReplaceDialogForm.MasterLookAndFeelEndChange;
begin
  AdjustLayout;
end;

procedure TdxSpreadSheetFindAndReplaceDialogForm.DoFindAll(ASheet: TdxSpreadSheetTableView);
var
  ARowIndex, AColumnIndex: Integer;
  ACell: TdxSpreadSheetCell;
begin
  GetStartIndexesForFindAll(ARowIndex, AColumnIndex);
  repeat
    ACell := DoFindNextCell(ASheet, ARowIndex, AColumnIndex);
    if ACell <> nil then
    begin
      ARowIndex := ACell.RowIndex;
      AColumnIndex := ACell.ColumnIndex;
      AddSearchResult(ACell);
    end;
  until ACell = nil;
end;

function TdxSpreadSheetFindAndReplaceDialogForm.DoFindNextCell(ASheet: TdxSpreadSheetTableView;
  const AStartRowIndex, AStartColumnIndex: Integer): TdxSpreadSheetCell;
var
  AArea: TRect;
  ACell: TdxSpreadSheetCell;
  AController: TdxTableViewControllerAccess;
  ARow, AFirstRow: TdxSpreadSheetTableRow;
  AColumn, AFirstColumn: TdxSpreadSheetTableColumn;
begin
  Result := nil;
  AController := TdxTableViewControllerAccess(TdxTableViewAccess(ASheet).Controller);
  AArea := GetSearchArea(AStartRowIndex, AStartColumnIndex);
  AFirstRow := AController.GetSearchAreaFirstRow(AArea);
  AFirstColumn := AController.GetSearchAreaFirstColumn(AArea);

  ARow := AFirstRow;
  AColumn := AFirstColumn;

  CheckFirstRowAndColumn(ARow, AColumn, AStartRowIndex, AStartColumnIndex);
  if (ARow = nil) or (AColumn = nil) then
    GetNextRowAndColumn(ARow, AColumn, AFirstRow, AFirstColumn);

  while (Result = nil) and (ARow <> nil) and (AColumn <> nil) do
  begin
    ACell := ARow.Cells[AColumn.Index];
    if CheckCondition(ACell) then
      Result := ACell
    else
      GetNextRowAndColumn(ARow, AColumn, AFirstRow, AFirstColumn);
  end;
end;

procedure TdxSpreadSheetFindAndReplaceDialogForm.DoReplaceAll(ASheet: TdxSpreadSheetTableView);
var
  ARowIndex, AColumnIndex: Integer;
  ACell: TdxSpreadSheetCell;
begin
  TdxTableViewAccess(ASheet).History.BeginAction(TdxSpreadSheetHistoryReplaceAction);
  try
    GetStartIndexesForFindAll(ARowIndex, AColumnIndex);
    repeat
      ACell := DoFindNextCell(ASheet, ARowIndex, AColumnIndex);
      if ACell <> nil then
      begin
        DoReplacement(ACell);
        ARowIndex := ACell.RowIndex;
        AColumnIndex := ACell.ColumnIndex;
      end;
    until ACell = nil;
  finally
    TdxTableViewAccess(ASheet).History.EndAction;
  end;
end;

procedure TdxSpreadSheetFindAndReplaceDialogForm.DoReplacement(ACell: TdxSpreadSheetCell);
var
  AWhatReplace, AWhereReplace, AReplaceWith: string;
  P, P2: Integer;
begin
  AWhatReplace := cmbWhatReplace.Text;
  AWhereReplace := GetFormulaText(ACell);
  AReplaceWith := cmbWithReplace.Text;
  if fdmmEntireCell in MatchModes then
    AWhereReplace := AReplaceWith
  else
  begin
    P := GetReplacementPos(AWhatReplace, AWhereReplace, 1);
    while P > 0 do
    begin
      P2 := P + Length(AWhatReplace);
      AWhereReplace := Copy(AWhereReplace, 1, P - 1) + AReplaceWith + Copy(AWhereReplace, P2, Length(AWhereReplace) - P2 + 1);
      Inc(FReplacementCount);
      P := GetReplacementPos(AWhatReplace, AWhereReplace, P2 + Length(AReplaceWith));
    end;
  end;
  ACell.SetText(AWhereReplace, True);
end;

procedure TdxSpreadSheetFindAndReplaceDialogForm.DoShowCell(AIndex: Integer);
var
  AReference: TdxFindAndReplaceDialogFoundReference;
begin
  AReference := mcFindAllResults.Items.Objects[AIndex] as TdxFindAndReplaceDialogFoundReference;
  AReference.Sheet.Active := True;
  AReference.Sheet.Selection.SetFocused(AReference.RowIndex, AReference.ColumnIndex, []);
end;

function TdxSpreadSheetFindAndReplaceDialogForm.FoundNextCell(AStartSheet: TdxSpreadSheetTableView): TdxSpreadSheetCell;
var
  ASheet: TdxSpreadSheetTableView;
begin
  ASheet := AStartSheet;
  Result := DoFindNextCell(ASheet, ASheet.Selection.FocusedRow, ASheet.Selection.FocusedColumn);
  if Result = nil then
    if SearchRange = fdsrSheet then
      Result := DoFindNextCell(ASheet, 0, 0)
    else
    repeat
      ASheet := TdxTableViewAccess(ASheet).GetNextVisibleView(True);
      Result := DoFindNextCell(ASheet, 0, 0);
    until (Result <> nil) or (ASheet = AStartSheet);
  if Result <> nil then
  begin
    SpreadSheet.ActiveSheet := Result.View;
    SpreadSheet.ActiveSheetAsTable.Selection.SetFocused(Result.RowIndex, Result.ColumnIndex, []);
  end
end;

procedure TdxSpreadSheetFindAndReplaceDialogForm.GetNextRowAndColumn(var ARow: TdxSpreadSheetTableRow; var AColumn: TdxSpreadSheetTableColumn;
  AFirstRow: TdxSpreadSheetTableRow; AFirstColumn: TdxSpreadSheetTableColumn);
begin
  if SearchMode = fdsmByRows then
  begin
    if AColumn <> nil then
      AColumn := TdxSpreadSheetTableColumn(AColumn.Next);
    if AColumn = nil then
    begin
      if ARow <> nil then
        ARow := TdxSpreadSheetTableRow(ARow.Next);
      if ARow <> nil then
        AColumn := AFirstColumn;
    end;
  end
  else
  begin
    if ARow <> nil then
      ARow := TdxSpreadSheetTableRow(ARow.Next);
    if ARow = nil then
    begin
      if AColumn <> nil then
        AColumn := TdxSpreadSheetTableColumn(AColumn.Next);
      if AColumn <> nil then
        ARow := AFirstRow;
    end;
  end
end;

function TdxSpreadSheetFindAndReplaceDialogForm.GetReplacementPos(const AWhatReplace, AWhereReplace: string; const AOffset: Integer): Integer;
begin
  if not (fdmmCase in MatchModes) then
    Result := PosEx(AnsiUpperCase(AWhatReplace), AnsiUpperCase(AWhereReplace), AOffset)
  else
    Result := PosEx(AWhatReplace, AWhereReplace, AOffset);
end;

function TdxSpreadSheetFindAndReplaceDialogForm.GetSearchArea(const AStartRowIndex, AStartColumnIndex: Integer): TRect;
begin
  if SearchMode = fdsmByRows then
    Result := cxRect(0, AStartRowIndex, dxSpreadSheetMaxColumnIndex, dxSpreadSheetMaxRowIndex)
  else
    Result := cxRect(AStartColumnIndex, 0, dxSpreadSheetMaxColumnIndex, dxSpreadSheetMaxRowIndex)
end;

procedure TdxSpreadSheetFindAndReplaceDialogForm.GetStartIndexesForFindAll(var ARowIndex, AColumnIndex: Integer);
begin
  ARowIndex := 0;
  Dec(ARowIndex, Integer(SearchMode = fdsmByColumns));
  AColumnIndex := 0;
  Dec(AColumnIndex, Integer(SearchMode = fdsmByRows));
end;

function TdxSpreadSheetFindAndReplaceDialogForm.IsRemoved(ASheet: TdxSpreadSheetTableView): Boolean;
begin
  Result := (ASheet = nil) or (TdxSpreadSheetAccess(SpreadSheet).FSheets.IndexOf(ASheet) < 0);
end;

function TdxSpreadSheetFindAndReplaceDialogForm.GetLookMode: TdxSpreadSheetFindAndReplaceDialogLookMode;
begin
  Result := TdxSpreadSheetFindAndReplaceDialogLookMode(cmbLookInFind.ItemIndex);
end;

function TdxSpreadSheetFindAndReplaceDialogForm.GetMatchModes: TdxSpreadSheetFindAndReplaceDialogMatchModes;
begin
  Result := [];
  if chkMatchCaseFind.Checked then
    Result := Result + [fdmmCase];
  if chkEntireCellFind.Checked then
    Result := Result + [fdmmEntireCell];
end;

function TdxSpreadSheetFindAndReplaceDialogForm.GetSearchMode: TdxSpreadSheetFindAndReplaceDialogSearchMode;
begin
  Result := TdxSpreadSheetFindAndReplaceDialogSearchMode(cmbSearchModeFind.ItemIndex);
end;

function TdxSpreadSheetFindAndReplaceDialogForm.GetSearchRange: TdxSpreadSheetFindAndReplaceDialogSearchRange;
begin
  Result := TdxSpreadSheetFindAndReplaceDialogSearchRange(cmbWithinRangeFind.ItemIndex);
end;

procedure TdxSpreadSheetFindAndReplaceDialogForm.Initialize(ASpreadSheet: TdxCustomSpreadSheet; AActivePage: Integer);
begin
  FSpreadSheet := ASpreadSheet;
  FormStyle := fsStayOnTop;
  lcMainContentGroup.ItemIndex := AActivePage;
  MasterLookAndFeel := ASpreadSheet.DialogsLookAndFeel;
end;

procedure TdxSpreadSheetFindAndReplaceDialogForm.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent is TdxCustomSpreadSheet) and (Operation = opRemove) then
    TdxSpreadSheetFindAndReplaceDialogForm.Unregister(TdxCustomSpreadSheet(AComponent));
end;

procedure TdxSpreadSheetFindAndReplaceDialogForm.ScaleFactorChanged(M, D: Integer);
begin
  inherited;
  FPriorClientHeight := MulDiv(FPriorClientHeight, M, D);
  FPriorClientWidth := MulDiv(FPriorClientWidth, M, D);
  FPriorLayoutHeight := MulDiv(FPriorLayoutHeight, M, D);
  FPriorLayoutWidth := MulDiv(FPriorLayoutWidth, M, D);
  PostMessage(Handle, DXM_LAYOUT_LAYOUTCHANGED, 0, 0);
end;

procedure TdxSpreadSheetFindAndReplaceDialogForm.WndProc(var Message: TMessage);
begin
  inherited WndProc(Message);
  case Message.Msg of
    DXM_LAYOUT_LAYOUTCHANGED:
      AdjustLayout;
    WM_ENTERSIZEMOVE:
      FIsFormSizing := True;
    WM_EXITSIZEMOVE:
      FIsFormSizing := False;
  end;
end;

procedure TdxSpreadSheetFindAndReplaceDialogForm.SaveComboHistory(ACombo: TcxComboBox);
var
  AText, AItem: string;
  I: Integer;
  APresent: Boolean;
begin
  AText := ACombo.Text;
  APresent := False;
  for I := 0 to ACombo.Properties.Items.Count - 1 do
  begin
    AItem := ACombo.Properties.Items[I];
    APresent := AnsiSameText(AText, AItem);
    if APresent then
      Break;
  end;
  if not APresent then
  begin
    ACombo.Properties.Items.Insert(0, AText);
    if ACombo = cmbWhatFind then
      cmbWhatReplace.Properties.Items.Insert(0, AText);
  end;
end;

procedure TdxSpreadSheetFindAndReplaceDialogForm.SetMasterLookAndFeel(ALookAndFeel: TcxLookAndFeel);
begin
  if FMasterLookAndFeel <> ALookAndFeel then
  begin
    if FMasterLookAndFeel <> nil then
      FMasterLookAndFeel.RemoveChangeListener(Self);
    FMasterLookAndFeel := ALookAndFeel;
    if FMasterLookAndFeel <> nil then
    begin
      SetControlLookAndFeel(Self, ALookAndFeel);
      FMasterLookAndFeel.AddChangeListener(Self);
    end;
  end;
end;

procedure TdxSpreadSheetFindAndReplaceDialogForm.ShowInfoMessage(AMessage: string; AType: Cardinal);
begin
  MessageBox(Handle, PChar(AMessage), PChar(PChar(Application.Title)), AType or MB_OK);
end;

procedure TdxSpreadSheetFindAndReplaceDialogForm.ShowSearchResults;
begin
  if mcFindAllResults.Top >= ClientHeight then
    Height := Height + ClientHeight div 2;
  mcFindAllResults.HeaderSections[4].AutoSize := True;
  mcFindAllResults.HeaderSections[4].AutoSize := False;
  mcFindAllResults.ItemIndex := 0;
  mcFindAllResultsClick(mcFindAllResults);
end;

procedure TdxSpreadSheetFindAndReplaceDialogForm.SynchronizeLookMode;
begin
  if lcMainContentGroup.ItemIndex = 1 then
    cmbLookInFind.ItemIndex := cmbLookInReplace.ItemIndex;
end;

procedure TdxSpreadSheetFindAndReplaceDialogForm.FormCreate(Sender: TObject);
var
  L: Integer;
begin
  lcMain.HandleNeeded;
  AdjustOptionsHeight;
  ClientHeight := GetMinClientHeight;

  FIsFormCreateProcessing := True;
  try
    lcMainContentGroup.ItemIndex := 1;
    L := cmbWhatReplace.Left;
    lcMainContentGroup.ItemIndex := 0;
    lcFindHorzAlignSpaceItem.SizeOptions.Width := L - cmbWhatFind.Left + 2;
  finally
    FIsFormCreateProcessing := False;
  end;
end;

procedure TdxSpreadSheetFindAndReplaceDialogForm.FormActivate(Sender: TObject);
begin
  CheckReplaceButtonsVisibility;
end;

procedure TdxSpreadSheetFindAndReplaceDialogForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SpreadSheet.RemoveListener(Self);
  ClearSearchResults;
end;

procedure TdxSpreadSheetFindAndReplaceDialogForm.FormResize(Sender: TObject);
begin
  if FIsFormSizing then
  begin
    FPriorClientHeight := ClientHeight;
    FPriorClientWidth := ClientWidth;
  end;
  lbFoundCellsInfo.Visible := ClientHeight - lbFoundCellsInfo.Height > GetMinClientHeight;
  mcFindAllResults.Visible := lbFoundCellsInfo.Visible;
end;

procedure TdxSpreadSheetFindAndReplaceDialogForm.FormShow(Sender: TObject);
begin
  AdjustLayout;
end;

procedure TdxSpreadSheetFindAndReplaceDialogForm.btnFindAllClick(Sender: TObject);
var
  I: Integer;
begin
  SynchronizeLookMode;
  SaveComboHistory(cmbWhatFind);
  ClearSearchResults;

  mcFindAllResults.Items.BeginUpdate;
  ShowHourglassCursor;
  try
    if SearchRange = fdsrSheet then
      DoFindAll(SpreadSheet.ActiveSheetAsTable)
    else
    for I := 0 to SpreadSheet.SheetCount - 1 do
      DoFindAll(SpreadSheet.Sheets[I] as TdxSpreadSheetTableView);
  finally
    HideHourglassCursor;
    mcFindAllResults.Items.EndUpdate;
  end;
  CheckFoundCellsInfo;
  if mcFindAllResults.Count > 0 then
    ShowSearchResults
  else
    ShowInfoMessage(cxGetResourceString(@sdxFindAndReplaceDialogFindMatchNotFound), MB_ICONWARNING);
end;

procedure TdxSpreadSheetFindAndReplaceDialogForm.btnFindNextClick(Sender: TObject);
begin
  SynchronizeLookMode;
  SaveComboHistory(cmbWhatFind);
  if FoundNextCell(SpreadSheet.ActiveSheetAsTable) = nil then
    ShowInfoMessage(cxGetResourceString(@sdxFindAndReplaceDialogFindMatchNotFound), MB_ICONWARNING);
end;

procedure TdxSpreadSheetFindAndReplaceDialogForm.btnOptionsFindClick(Sender: TObject);
begin
  lcMain.BeginUpdate;
  try
    lcgGroupOptionsFind.Visible := not lcgGroupOptionsFind.Visible;
    lcgGroupOptionsReplace.Visible := lcgGroupOptionsFind.Visible;
    lgOptionsHeightAdjusting.Visible := not lcgGroupOptionsFind.Visible;
    if lcgGroupOptionsFind.Visible then
      btnOptionsFind.Caption := FLessOptionsCaption
    else
      btnOptionsFind.Caption := FMoreOptionsCaption;

    btnOptionsReplace.Caption := btnOptionsFind.Caption;
  finally
    lcMain.EndUpdate;
    AdjustLayout;
  end;
end;

procedure TdxSpreadSheetFindAndReplaceDialogForm.btnReplaceAllClick(Sender: TObject);
var
  I: Integer;
begin
  CheckReplacementPossibility(SpreadSheet.ActiveSheetAsTable, True);
  FReplacementCount := 0;
  SynchronizeLookMode;
  SaveComboHistory(cmbWithReplace);

  SpreadSheet.BeginUpdate;
  try
    if SearchRange = fdsrSheet then
      DoReplaceAll(SpreadSheet.ActiveSheetAsTable)
    else
      for I := 0 to SpreadSheet.SheetCount - 1 do
        if not (SpreadSheet.Sheets[I] as TdxSpreadSheetTableView).Options.&Protected then
          DoReplaceAll(SpreadSheet.Sheets[I] as TdxSpreadSheetTableView);
  finally
    SpreadSheet.EndUpdate;
  end;

  if ReplacementCount > 0 then
    ShowInfoMessage(Format(cxGetResourceString(@sdxFindAndReplaceDialogReplacementResult), [ReplacementCount]), MB_ICONINFORMATION)
  else
    ShowInfoMessage(cxGetResourceString(@sdxFindAndReplaceDialogReplaceMatchNotFound), MB_ICONWARNING);
end;

procedure TdxSpreadSheetFindAndReplaceDialogForm.btnReplaceClick(Sender: TObject);
var
  ASheet: TdxSpreadSheetTableView;
  ACell: TdxSpreadSheetCell;
begin
  CheckReplacementPossibility(SpreadSheet.ActiveSheetAsTable, False);
  FReplacementCount := 0;
  SynchronizeLookMode;
  SaveComboHistory(cmbWithReplace);
  ASheet := SpreadSheet.ActiveSheetAsTable;
  ACell := TdxTableViewAccess(ASheet).GetActiveCell(ASheet.Selection.FocusedRow, ASheet.Selection.FocusedColumn);
  if ACell = nil then
    ACell := ASheet.Selection.FocusedCell;
  if not CheckCondition(ACell)  then
    ShowInfoMessage(cxGetResourceString(@sdxFindAndReplaceDialogReplaceMatchNotFound), MB_ICONWARNING)
  else
  begin
    TdxTableViewAccess(ASheet).History.BeginAction(TdxSpreadSheetHistoryReplaceAction);
    try
      DoReplacement(ACell);
    finally
      TdxTableViewAccess(ASheet).History.EndAction;
    end;
    FoundNextCell(ACell.View);
  end;
end;

procedure TdxSpreadSheetFindAndReplaceDialogForm.cmbFindWhatPropertiesChange(Sender: TObject);
begin
  cmbWhatFind.Text := TcxComboBox(Sender).Text;
  cmbWhatReplace.Text := cmbWhatFind.Text;
end;

procedure TdxSpreadSheetFindAndReplaceDialogForm.cmbFindWithinRangePropertiesChange(Sender: TObject);
begin
  cmbWithinRangeFind.ItemIndex := TcxComboBox(Sender).ItemIndex;
  cmbWithinRangeReplace.ItemIndex := cmbWithinRangeFind.ItemIndex;
end;

procedure TdxSpreadSheetFindAndReplaceDialogForm.cmbFindSearchModePropertiesChange(Sender: TObject);
begin
  cmbSearchModeFind.ItemIndex := TcxComboBox(Sender).ItemIndex;
  cmbSearchModeReplace.ItemIndex := cmbSearchModeFind.ItemIndex;
end;

procedure TdxSpreadSheetFindAndReplaceDialogForm.chkFindMatchCasePropertiesChange(Sender: TObject);
begin
  chkMatchCaseFind.Checked := TcxCheckBox(Sender).Checked;
  chkMatchCaseReplace.Checked := chkMatchCaseFind.Checked;
end;

procedure TdxSpreadSheetFindAndReplaceDialogForm.chkFindEntireCellPropertiesChange(Sender: TObject);
begin
  chkEntireCellFind.Checked := TcxCheckBox(Sender).Checked;
  chkEntireCellReplace.Checked := chkEntireCellFind.Checked;
end;

procedure TdxSpreadSheetFindAndReplaceDialogForm.lcMainContentGroupTabChanged(Sender: TObject);
begin
  if FIsFormCreateProcessing then
    Exit;

  CheckReplaceButtonsVisibility;
  if lcMainContentGroup.ItemIndex < 1 then
  begin
    PostMessage(cmbWhatFind.Handle, WM_SETFOCUS, 0, 0);
    PostMessage(cmbWhatFind.Handle, WM_KEYDOWN, VK_END, 0);
  end
  else
  begin
    PostMessage(cmbWhatReplace.Handle, WM_KEYDOWN, VK_END, 0);
    if cmbWhatReplace.Text <> '' then
    begin
      PostMessage(cmbWithReplace.Handle, WM_SETFOCUS, 0, 0);
      PostMessage(cmbWithReplace.Handle, WM_KEYDOWN, VK_END, 0);
    end
    else
      PostMessage(cmbWhatReplace.Handle, WM_SETFOCUS, 0, 0);
  end;
end;

procedure TdxSpreadSheetFindAndReplaceDialogForm.mcFindAllResultsClick(Sender: TObject);
begin
  if mcFindAllResults.Items.Count > 0 then
    DoShowCell(mcFindAllResults.ItemIndex);
end;

procedure TdxSpreadSheetFindAndReplaceDialogForm.btnCloseClick(Sender: TObject);
begin
  Close;
end;

class procedure TdxSpreadSheetFindAndReplaceDialogForm.Register(ASpreadSheet: TdxCustomSpreadSheet);
begin
  if FFindAndReplaceDialog = nil then
    FFindAndReplaceDialog := dxSpreadSheetFindAndReplaceDialogClass.Create(nil);
  if FFindAndReplaceDialog.FSpreadSheets.IndexOf(ASpreadSheet) < 0 then
  begin
    FFindAndReplaceDialog.FSpreadSheets.Add(ASpreadSheet);
    ASpreadSheet.AddListener(FFindAndReplaceDialog);
    ASpreadSheet.FreeNotification(FFindAndReplaceDialog);
  end;
end;

class procedure TdxSpreadSheetFindAndReplaceDialogForm.Unregister(ASpreadSheet: TdxCustomSpreadSheet);
begin
  if FFindAndReplaceDialog <> nil then
  begin
    ASpreadSheet.RemoveListener(FFindAndReplaceDialog);
    FFindAndReplaceDialog.FSpreadSheets.Remove(ASpreadSheet);
    FFindAndReplaceDialog.CleanSearchResults(ASpreadSheet);
    if FFindAndReplaceDialog.FSpreadSheets.Count = 0 then
    begin
      FFindAndReplaceDialog.MasterLookAndFeel := nil;
      FreeAndNil(FFindAndReplaceDialog);
    end
    else
      if FFindAndReplaceDialog.SpreadSheet = ASpreadSheet then
      begin
        FFindAndReplaceDialog.MasterLookAndFeel := nil;
        FFindAndReplaceDialog.Close;
      end;
  end;
end;

procedure ShowFindAndReplaceDialog(ASpreadSheet: TdxCustomSpreadSheet; const AActivePage: Integer = 0);
begin
  TdxSpreadSheetFindAndReplaceDialogForm.Register(ASpreadSheet);
  FFindAndReplaceDialog.Initialize(ASpreadSheet, AActivePage);
  FFindAndReplaceDialog.Show;
end;

initialization
  FFindAndReplaceDialog := nil;
end.

