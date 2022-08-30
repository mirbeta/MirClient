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

unit dxSpreadSheetPopupMenu;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, Windows, Classes, Graphics, ImgList, Menus,
  dxCore, cxControls, cxGraphics,
  dxSpreadSheetCore, dxSpreadSheetUtils, dxSpreadSheetTypes, dxBuiltInPopupMenu, dxSpreadSheetHyperlinks,
  dxSpreadSheetProtection;

type

  { TdxSpreadSheetCustomPopupMenu }

  TdxSpreadSheetCustomPopupMenu = class(TComponent)
  strict private
    FAdapter: TdxCustomBuiltInPopupMenuAdapter;
    FImageList: TcxImageList;

    function GetSpreadSheet: TdxCustomSpreadSheet; inline;
    procedure MenuItemClick(Sender: TObject);
  protected
    function AddImage(const AResourceName: string): Integer;
    function AddMenuItem(ACaption: Pointer; ACommandID: Word;
      const AImageResName: string = ''; AEnabled: Boolean = True; AParent: TComponent = nil): TComponent; overload;
    function AddMenuItem(ACaption: Pointer; ACommandID, AParam: Word;
      const AImageResName: string = ''; AEnabled: Boolean = True; AParent: TComponent = nil): TComponent; overload;
    procedure AddSeparator;
    procedure PopulateMenuItems; virtual; abstract;
    procedure ProcessClick(ACommand, AParam: Word); virtual; abstract;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Initialize;
    function Popup(const P: TPoint): Boolean;

    property Adapter: TdxCustomBuiltInPopupMenuAdapter read FAdapter;
    property ImageList: TcxImageList read FImageList;
    property SpreadSheet: TdxCustomSpreadSheet read GetSpreadSheet;
  end;

  { TdxSpreadSheetBuiltInPageControlTabPopupMenu }

  TdxSpreadSheetBuiltInPageControlTabPopupMenu = class(TdxSpreadSheetCustomPopupMenu)
  strict private const
    ccDelete    = 1;
    ccHide      = 2;
    ccInsert    = 3;
    ccRename    = 4;
    ccUnhide    = 5;
    ccProtect   = 6;
    ccUnprotect = 7;
  protected
    function CanDelete: Boolean; virtual;
    function CanHide: Boolean; virtual;
    function CanInsert: Boolean; virtual;
    function CanRename: Boolean; virtual;
    function CanUnhide: Boolean; virtual;

    procedure Delete; virtual;
    procedure Hide; virtual;
    procedure Insert; virtual;
    procedure Rename; virtual;
    procedure Unhide; virtual;

    procedure PopulateMenuItems; override;
    procedure ProcessClick(ACommand, AParam: Word); override;
  end;

  { TdxSpreadSheetBuiltInTableViewPopupMenu }

  TdxSpreadSheetBuiltInTableViewPopupMenu = class(TdxSpreadSheetCustomPopupMenu)
  strict private const
    // Command ID
    ccBringToFront = 1;
    ccClearContent = 2;
    ccCopy = 3;
    ccCustomizeObject = 4;
    ccCut = 5;
    ccDelete = 6;
    ccDeleteComments = 7;
    ccEditComment = 8;
    ccFormatCells = 9;
    ccHide = 10;
    ccHyperlink = 11;
    ccInsert = 12;
    ccMerge = 13;
    ccOpenHyperlink = 14;
    ccPaste = 15;
    ccPasteSpecial = 16;
    ccPasteSpecialDialog = 17;
    ccRemoveHyperlink = 18;
    ccSendToBack = 19;
    ccShowHideComment = 20;
    ccSplit = 21;
    ccUnhide = 22;

    // CommandParams
    cpPasteSpecialColumnWidths = 1;
    cpPasteSpecialFormulas     = 2;
    cpPasteSpecialNumberFormat = 4;
    cpPasteSpecialStyles       = 8;

  strict private
    function GetFocusedCellComment: TdxSpreadSheetContainer;
    function GetFocusedContainer: TdxSpreadSheetContainer;
    function GetHyperlink: TdxSpreadSheetHyperlink;
    function GetView: TdxSpreadSheetTableView; inline;
  protected
    function GetFocusedCell(ACreateIfNotExists: Boolean): TdxSpreadSheetCell;
    procedure PopulateMenuItems; override;
    procedure PopulateMenuItemsForCells; virtual;
    procedure PopulateMenuItemsForCellsComments; virtual;
    procedure PopulateMenuItemsForContainer; virtual;
    procedure PopulateMenuItemsForHyperlink;
    procedure PopulatePasteSpecialMenuItems;
    procedure ProcessClick(ACommand, AParam: Word); override;

    // Commands
    procedure ChangeVisibility(AVisibility: Boolean); virtual;
    procedure Delete; virtual;
    procedure PasteSpecial(AParam: Word);
  public
    property FocusedCellComment: TdxSpreadSheetContainer read GetFocusedCellComment;
    property FocusedContainer: TdxSpreadSheetContainer read GetFocusedContainer;
    property Hyperlink: TdxSpreadSheetHyperlink read GetHyperlink;
    property View: TdxSpreadSheetTableView read GetView;
  end;

implementation

uses
  Math, SysUtils, Dialogs, dxGDIPlusClasses, cxClasses, dxSpreadSheetStrs, dxSpreadSheetFormatCellsDialog,
  dxHashUtils, dxSpreadSheetContainerCustomizationDialog, dxSpreadSheetUnhideSheetDialog, dxSpreadSheetContainers,
  dxSpreadSheetEditHyperlinkDialog, dxSpreadSheetPasteSpecialDialog, dxSpreadSheetProtectSheetDialog,
  dxSpreadSheetPasswordDialog, dxOLECryptoContainerStrs, dxInputDialogs, dxSpreadSheetCoreStrs;

{$R dxSpreadSheetPopupMenu.res}

type
  TdxSpreadSheetTableItemsAccess = class(TdxSpreadSheetTableItems);
  TdxSpreadSheetTableViewAccess = class(TdxSpreadSheetTableView);

{ TdxSpreadSheetCustomPopupMenu }

constructor TdxSpreadSheetCustomPopupMenu.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FImageList := TcxImageList.Create(Self);
end;

destructor TdxSpreadSheetCustomPopupMenu.Destroy;
begin
  FreeAndNil(FAdapter);
  FreeAndNil(FImageList);
  inherited Destroy;
end;

procedure TdxSpreadSheetCustomPopupMenu.Initialize;
begin
  if Adapter = nil then
    FAdapter := TdxBuiltInPopupMenuAdapterManager.GetActualAdapterClass.Create(Self);

  Adapter.Clear;
  Adapter.SetImages(nil);
  ImageList.Clear;
  Adapter.SetLookAndFeel(SpreadSheet.LookAndFeel);
  Adapter.SetImages(ImageList);
  PopulateMenuItems;
end;

function TdxSpreadSheetCustomPopupMenu.Popup(const P: TPoint): Boolean;
begin
  Initialize;
  Result := Adapter.Popup(SpreadSheet.ClientToScreen(P));
  if not TdxBuiltInPopupMenuAdapterManager.IsActualAdapterStandard then
    FreeAndNil(FAdapter);
end;

function TdxSpreadSheetCustomPopupMenu.AddImage(const AResourceName: string): Integer;
var
  APNGImage: TdxPNGImage;
begin
  Result := -1;
  if AResourceName <> '' then
  begin
    APNGImage := TdxPNGImage.Create;
    try
      APNGImage.LoadFromResource(HInstance, AResourceName, 'PNG');
      Result := ImageList.Add(APNGImage);
    finally
      APNGImage.Free;
    end;
  end;
end;

function TdxSpreadSheetCustomPopupMenu.AddMenuItem(ACaption: Pointer; ACommandID: Word;
  const AImageResName: string = ''; AEnabled: Boolean = True; AParent: TComponent = nil): TComponent;
begin
  Result := AddMenuItem(ACaption, ACommandID, 0, AImageResName, AEnabled, AParent);
end;

function TdxSpreadSheetCustomPopupMenu.AddMenuItem(ACaption: Pointer; ACommandID, AParam: Word;
  const AImageResName: string = ''; AEnabled: Boolean = True; AParent: TComponent = nil): TComponent;
begin
  Result := Adapter.Add(cxGetResourceString(ACaption), MenuItemClick,
    MakeLong(ACommandID, AParam), AddImage(AImageResName), AEnabled, 0, AParent);
end;

procedure TdxSpreadSheetCustomPopupMenu.AddSeparator;
begin
  Adapter.AddSeparator;
end;

function TdxSpreadSheetCustomPopupMenu.GetSpreadSheet: TdxCustomSpreadSheet;
begin
  Result := Owner as TdxCustomSpreadSheet;
end;

procedure TdxSpreadSheetCustomPopupMenu.MenuItemClick(Sender: TObject);
begin
  ProcessClick(Word(TComponent(Sender).Tag), HiWord(TComponent(Sender).Tag));
end;

{ TdxSpreadSheetBuiltInPageControlTabPopupMenu }

function TdxSpreadSheetBuiltInPageControlTabPopupMenu.CanDelete: Boolean;
begin
  Result := (SpreadSheet.VisibleSheetCount > 1) and SpreadSheet.OptionsProtection.ActualAllowChangeStructure;
end;

function TdxSpreadSheetBuiltInPageControlTabPopupMenu.CanHide: Boolean;
begin
  Result := (SpreadSheet.VisibleSheetCount > 1) and SpreadSheet.OptionsProtection.ActualAllowChangeStructure;
end;

function TdxSpreadSheetBuiltInPageControlTabPopupMenu.CanInsert: Boolean;
begin
  Result := SpreadSheet.OptionsProtection.ActualAllowChangeStructure;
end;

function TdxSpreadSheetBuiltInPageControlTabPopupMenu.CanRename: Boolean;
begin
  Result := SpreadSheet.OptionsProtection.ActualAllowChangeStructure;
end;

function TdxSpreadSheetBuiltInPageControlTabPopupMenu.CanUnhide: Boolean;
begin
  Result := (SpreadSheet.VisibleSheetCount <> SpreadSheet.SheetCount) and SpreadSheet.OptionsProtection.ActualAllowChangeStructure;
end;

procedure TdxSpreadSheetBuiltInPageControlTabPopupMenu.Delete;
begin
  SpreadSheet.ActiveSheet.Free;
end;

procedure TdxSpreadSheetBuiltInPageControlTabPopupMenu.Hide;
begin
  SpreadSheet.ActiveSheet.Visible := False;
end;

procedure TdxSpreadSheetBuiltInPageControlTabPopupMenu.Insert;
var
  ASheet: TdxSpreadSheetCustomView;
begin
  ASheet := SpreadSheet.AddSheet;
  ASheet.Index := SpreadSheet.ActiveSheetIndex;
  ASheet.Active := True;
end;

procedure TdxSpreadSheetBuiltInPageControlTabPopupMenu.Rename;
var
  AValue: string;
begin
  AValue := SpreadSheet.ActiveSheet.Caption;
  while True do
  try
    if dxInputQuery(cxGetResourceString(@sdxRenameDialogCaption), cxGetResourceString(@sdxRenameDialogSheetName), AValue) then
    begin
      AValue := Trim(AValue);
      if AValue <> '' then
        SpreadSheet.ActiveSheet.Caption := AValue;
    end;
    Break;
  except
    MessageDlg(cxGetResourceString(@sdxErrorCannotRenameSheet), mtWarning, [mbOK], 0);
  end;
end;

procedure TdxSpreadSheetBuiltInPageControlTabPopupMenu.Unhide;
begin
  ShowUnhideSheetDialog(SpreadSheet);
end;

procedure TdxSpreadSheetBuiltInPageControlTabPopupMenu.PopulateMenuItems;
begin
  AddMenuItem(@sdxBuiltInPopupMenuInsert, ccInsert, 'DXSPREADSHEET_POPUPMENU_GLYPH_INSERTSHEET', CanInsert);
  AddMenuItem(@sdxBuiltInPopupMenuDelete, ccDelete, 'DXSPREADSHEET_POPUPMENU_GLYPH_REMOVESHEET', CanDelete);
  AddMenuItem(@sdxBuiltInPopupMenuRename, ccRename, '', CanRename);

  if SpreadSheet.ActiveSheetAsTable.OptionsProtection.Protected then
    AddMenuItem(@sdxBuiltInPopupMenuUnprotectSheet, ccUnprotect, 'DXSPREADSHEET_POPUPMENU_GLYPH_PROTECTION')
  else
    AddMenuItem(@sdxBuiltInPopupMenuProtectSheet, ccProtect, 'DXSPREADSHEET_POPUPMENU_GLYPH_PROTECTION');

  AddSeparator;
  AddMenuItem(@sdxBuiltInPopupMenuHide, ccHide, '', CanHide);
  AddMenuItem(@sdxBuiltInPopupMenuUnhideDialog, ccUnhide, '', CanUnhide);
end;

procedure TdxSpreadSheetBuiltInPageControlTabPopupMenu.ProcessClick(ACommand, AParam: Word);
begin
  case ACommand of
    ccInsert:
      Insert;
    ccRename:
      Rename;
    ccDelete:
      Delete;
    ccHide:
      Hide;
    ccUnhide:
      Unhide;
    ccProtect:
      SpreadSheet.ActiveSheetAsTable.Protect;
    ccUnprotect:
      SpreadSheet.ActiveSheetAsTable.Unprotect;
  end;
end;

{ TdxSpreadSheetBuiltInTableViewPopupMenu }

function TdxSpreadSheetBuiltInTableViewPopupMenu.GetFocusedCell(ACreateIfNotExists: Boolean): TdxSpreadSheetCell;
begin
  Result := View.Selection.FocusedCell;
  if (Result = nil) and ACreateIfNotExists then
    Result := View.CreateCell(View.Selection.FocusedRow, View.Selection.FocusedColumn);
end;

function TdxSpreadSheetBuiltInTableViewPopupMenu.GetFocusedCellComment: TdxSpreadSheetContainer;
begin
  if not View.Containers.FindCommentContainer(GetFocusedCell(False), Result) then
    Result := nil;
end;

procedure TdxSpreadSheetBuiltInTableViewPopupMenu.PopulateMenuItems;
begin
  AddMenuItem(@sdxBuiltInPopupMenuCut, ccCut, 'DXSPREADSHEET_POPUPMENU_GLYPH_CUT', View.CanCutToClipboard);
  AddMenuItem(@sdxBuiltInPopupMenuCopy, ccCopy, 'DXSPREADSHEET_POPUPMENU_GLYPH_COPY', View.CanCopyToClipboard);
  AddMenuItem(@sdxBuiltInPopupMenuPaste, ccPaste, 'DXSPREADSHEET_POPUPMENU_GLYPH_PASTE', View.CanPasteFromClipboard);
  PopulatePasteSpecialMenuItems;
  AddSeparator;

  if FocusedContainer = nil then
    PopulateMenuItemsForCells
  else
    PopulateMenuItemsForContainer
end;

procedure TdxSpreadSheetBuiltInTableViewPopupMenu.PopulateMenuItemsForCells;
const
  DeleteCommandCaptions: array[Boolean] of Pointer = (@sdxBuiltInPopupMenuDelete, @sdxBuiltInPopupMenuDeleteDialog);
  InsertCommandCaptions: array[Boolean] of Pointer = (@sdxBuiltInPopupMenuInsert, @sdxBuiltInPopupMenuInsertDialog);
var
  ACanEditHeaders: Boolean;
  AEntireColumnIsSelected: Boolean;
  AEntireRowIsSelected: Boolean;
  AEntireRowOrColumnIsSelected: Boolean;
begin
  if SpreadSheet.OptionsBehavior.Editing then
  begin
    AEntireColumnIsSelected := (FocusedContainer = nil) and dxSpreadSheetIsEntireColumn(View.Selection.Area);
    AEntireRowIsSelected := (FocusedContainer = nil) and dxSpreadSheetIsEntireRow(View.Selection.Area);
    AEntireRowOrColumnIsSelected := AEntireColumnIsSelected or AEntireRowIsSelected;

    // Merging
    AddMenuItem(@sdxBuiltInPopupMenuMergeCells, ccMerge, 'DXSPREADSHEET_POPUPMENU_GLYPH_MERGE', View.CanMergeSelected);
    AddMenuItem(@sdxBuiltInPopupMenuSplitCells, ccSplit, 'DXSPREADSHEET_POPUPMENU_GLYPH_UNMERGE', View.CanSplitSelected);

    // Selection
    AddSeparator;
    if SpreadSheet.OptionsBehavior.Inserting then
      AddMenuItem(InsertCommandCaptions[not AEntireRowOrColumnIsSelected], ccInsert, '', View.CanInsert);
    if SpreadSheet.OptionsBehavior.Deleting then
      AddMenuItem(DeleteCommandCaptions[not AEntireRowOrColumnIsSelected], ccDelete, '', View.CanDelete);
    AddMenuItem(@sdxBuiltInPopupMenuClearContents, ccClearContent, '', View.CanClearCells);

    // Comments
    PopulateMenuItemsForCellsComments;

    // Formatting
    if SpreadSheet.OptionsBehavior.Formatting then
    begin
      AddSeparator;
      AddMenuItem(@sdxBuiltInPopupMenuFormatCells, ccFormatCells,
        'DXSPREADSHEET_POPUPMENU_GLYPH_FORMATCELLS', View.OptionsProtection.ActualAllowFormatCells);
    end;

    // Row / Columns
    if AEntireRowOrColumnIsSelected then
    begin
      ACanEditHeaders :=
        AEntireRowIsSelected and View.OptionsProtection.ActualAllowResizeRows or
        AEntireColumnIsSelected and View.OptionsProtection.ActualAllowResizeColumns;
      AddMenuItem(@sdxBuiltInPopupMenuHide, ccHide, '', ACanEditHeaders);
      AddMenuItem(@sdxBuiltInPopupMenuUnhide, ccUnhide, '', ACanEditHeaders);
    end
    else
      PopulateMenuItemsForHyperlink;
  end;
end;

procedure TdxSpreadSheetBuiltInTableViewPopupMenu.PopulateMenuItemsForCellsComments;
const
  ChangeCommentVisibilityCommandCaption: array[Boolean] of Pointer = (
    @sdxBuiltInPopupMenuShowComment, @sdxBuiltInPopupMenuHideComment
  );
  EditCommentCommandCaption: array[Boolean] of Pointer = (
    @sdxBuiltInPopupMenuInsertComment, @sdxBuiltInPopupMenuEditComment
  );
  EditCommentImages: array[Boolean] of string = (
    'DXSPREADSHEET_POPUPMENU_GLYPH_INSERTCOMMENT', 'DXSPREADSHEET_POPUPMENU_GLYPH_EDITCOMMENT'
  );
var
  AContainer: TdxSpreadSheetContainer;
begin
  AddSeparator;
  AContainer := FocusedCellComment;
  if View.CanEditContainers then
    AddMenuItem(EditCommentCommandCaption[AContainer <> nil], ccEditComment, EditCommentImages[AContainer <> nil], View.CanEditComment);
  if View.CanDeleteComments then
    AddMenuItem(@sdxBuiltInPopupMenuDeleteComment, ccDeleteComments, 'DXSPREADSHEET_POPUPMENU_GLYPH_DELETECOMMENT');
  if AContainer <> nil then
    AddMenuItem(ChangeCommentVisibilityCommandCaption[AContainer.Visible], ccShowHideComment);
end;

procedure TdxSpreadSheetBuiltInTableViewPopupMenu.PopulateMenuItemsForContainer;
begin
  AddMenuItem(@sdxBuiltInPopupMenuBringToFront, ccBringToFront, 'DXSPREADSHEET_POPUPMENU_GLYPH_BRINGTOFRONT', View.Containers.Count > 1);
  AddMenuItem(@sdxBuiltInPopupMenuSendToBack, ccSendToBack, 'DXSPREADSHEET_POPUPMENU_GLYPH_SENDTOBACK', View.Containers.Count > 1);
  AddSeparator;

  AddMenuItem(@sdxBuiltInPopupMenuDelete, ccDelete, '', View.CanDelete);
  AddSeparator;

  AddMenuItem(@sdxBuiltInPopupMenuCustomizeObject, ccCustomizeObject, '', View.CanEditContainers);

  if FocusedCellComment = nil then
  begin
    AddSeparator;
    PopulateMenuItemsForHyperlink;
  end;
end;

procedure TdxSpreadSheetBuiltInTableViewPopupMenu.PopulateMenuItemsForHyperlink;
const
  Map: array[Boolean] of Pointer = (@sdxBuiltInPopupMenuCreateHyperlink, @sdxBuiltInPopupMenuCreateHyperlink);
begin
  if View.OptionsProtection.ActualAllowEditHyperlinks then
    AddMenuItem(Map[Hyperlink <> nil], ccHyperlink, 'DXSPREADSHEET_POPUPMENU_GLYPH_HYPERLINK', View.CanEditHyperlinks);

  if Hyperlink <> nil then
  begin
    AddMenuItem(@sdxBuiltInPopupMenuOpenHyperlink, ccOpenHyperlink);
    if View.OptionsProtection.ActualAllowEditHyperlinks then
      AddMenuItem(@sdxBuiltInPopupMenuRemoveHyperlink, ccRemoveHyperlink, 'DXSPREADSHEET_POPUPMENU_GLYPH_DELETEHYPERLINK', View.CanDeleteHyperlink);
  end;
end;

procedure TdxSpreadSheetBuiltInTableViewPopupMenu.ProcessClick(ACommand, AParam: Word);
begin
  case ACommand of
    ccCut:
      View.CutToClipboard;
    ccCopy:
      View.CopyToClipboard;
    ccPaste:
      View.PasteFromClipboard;
    ccSplit:
      View.SplitSelected;
    ccMerge:
      View.MergeSelected;
    ccClearContent:
      View.ClearCellValues;
    ccDelete:
      Delete;
    ccInsert:
      View.InsertCells;
    ccFormatCells:
      ShowFormatCellsDialog(View);
    ccBringToFront:
      FocusedContainer.BringToFront;
    ccSendToBack:
      FocusedContainer.SendToBack;
    ccCustomizeObject:
      ShowContainerCustomizationDialog(FocusedContainer);
    ccHide, ccUnhide:
      ChangeVisibility(ACommand = ccUnhide);
    ccEditComment:
      View.EditComment;
    ccDeleteComments:
      View.DeleteComments;
    ccShowHideComment:
      FocusedCellComment.Visible := not FocusedCellComment.Visible;
    ccHyperlink:
      View.EditHyperlink;
    ccOpenHyperlink:
      Hyperlink.Execute;
    ccRemoveHyperlink:
      View.DeleteHyperlink;
    ccPasteSpecial:
      PasteSpecial(AParam);
    ccPasteSpecialDialog:
      ShowPasteSpecialDialog(SpreadSheet);
  end;
end;

procedure TdxSpreadSheetBuiltInTableViewPopupMenu.PopulatePasteSpecialMenuItems;
var
  ARootMenu: TComponent;
begin
  ARootMenu := nil;
  if View.CanPasteFromClipboard([]) then
  begin
    ARootMenu := Adapter.AddSubMenu(cxGetResourceString(@sdxBuiltInPopupMenuPasteSpecial), nil);

    AddMenuItem(@sdxBuiltInPopupMenuPasteSpecialAll, ccPaste, 'DXSPREADSHEET_POPUPMENU_GLYPH_PASTE', True, ARootMenu);

    AddSeparator;

    AddMenuItem(@sdxBuiltInPopupMenuPasteSpecialValues, ccPasteSpecial, 0, '', True, ARootMenu);
    AddMenuItem(@sdxBuiltInPopupMenuPasteSpecialValuesAndFormatting,
      ccPasteSpecial, cpPasteSpecialNumberFormat, '', True, ARootMenu);
    AddMenuItem(@sdxBuiltInPopupMenuPasteSpecialValuesAndStyles,
      ccPasteSpecial, cpPasteSpecialStyles, '', True, ARootMenu);

    AddSeparator;

    AddMenuItem(@sdxBuiltInPopupMenuPasteSpecialFormulas,
      ccPasteSpecial, cpPasteSpecialFormulas, '', True, ARootMenu);
    AddMenuItem(@sdxBuiltInPopupMenuPasteSpecialFormulasAndFormatting,
      ccPasteSpecial, cpPasteSpecialFormulas or cpPasteSpecialNumberFormat, '', True, ARootMenu);
    AddMenuItem(@sdxBuiltInPopupMenuPasteSpecialFormulasAndStyles,
      ccPasteSpecial, cpPasteSpecialFormulas or cpPasteSpecialStyles, '', True, ARootMenu);
    AddMenuItem(@sdxBuiltInPopupMenuPasteSpecialFormulasAndColumnWidths,
      ccPasteSpecial, cpPasteSpecialFormulas or cpPasteSpecialStyles or cpPasteSpecialColumnWidths, '', True, ARootMenu);

    AddSeparator;
  end;

  AddMenuItem(@sdxBuiltInPopupMenuPasteSpecialShowDialog, ccPasteSpecialDialog,
    '', (ARootMenu <> nil) or View.CanPasteFromClipboard, ARootMenu);
end;

procedure TdxSpreadSheetBuiltInTableViewPopupMenu.ChangeVisibility(AVisibility: Boolean);

  procedure DoChangeItemsVisibility(AItems: TdxSpreadSheetTableItems; AStartIndex, AFinishIndex: Integer);
  var
    I: Integer;
  begin
    if AVisibility then
    begin
      TdxSpreadSheetTableItemsAccess(AItems).ForEach(
        procedure (AItem: TdxDynamicListItem)
        begin
          TdxSpreadSheetTableItem(AItem).Visible := True;
        end,
        AStartIndex, AFinishIndex);
    end
    else
      for I := AStartIndex to AFinishIndex do
        AItems.CreateItem(I).Visible := AVisibility;
  end;

  procedure DoChangeVisibilityInArea(const AArea: TRect);
  begin
    if dxSpreadSheetIsEntireRow(AArea) then
      DoChangeItemsVisibility(View.Rows, AArea.Top, AArea.Bottom)
    else
      if dxSpreadSheetIsEntireColumn(AArea) then
        DoChangeItemsVisibility(View.Columns, AArea.Left, AArea.Right);
  end;

var
  AAreaIndex: Integer;
  ASelection: TdxSpreadSheetTableViewSelection;
begin
  View.BeginUpdate;
  try
    TdxSpreadSheetTableViewAccess(View).History.BeginAction(TdxSpreadSheetHistoryChangeRowColumnItemAction);
    try
      ASelection := View.Selection;
      for AAreaIndex := 0 to ASelection.Count - 1 do
        DoChangeVisibilityInArea(dxSpreadSheetGetRealArea(ASelection[AAreaIndex].Rect));
    finally
      TdxSpreadSheetTableViewAccess(View).History.EndAction;
    end;
  finally
    View.EndUpdate;
  end;
end;

procedure TdxSpreadSheetBuiltInTableViewPopupMenu.Delete;
begin
  if FocusedContainer <> nil then
    FocusedContainer.Free
  else
    View.DeleteCells;
end;

procedure TdxSpreadSheetBuiltInTableViewPopupMenu.PasteSpecial(AParam: Word);
var
  AOptions: TdxSpreadSheetClipboardPasteOptions;
begin
  AOptions := [cpoValues];
  if AParam and cpPasteSpecialColumnWidths <> 0 then
    Include(AOptions, cpoColumnWidths);
  if AParam and cpPasteSpecialFormulas <> 0 then
    Include(AOptions, cpoFormulas);
  if AParam and cpPasteSpecialNumberFormat <> 0 then
    Include(AOptions, cpoNumberFormatting);
  if AParam and cpPasteSpecialStyles <> 0 then
    AOptions := AOptions + [cpoStyles, cpoComments];
  View.PasteFromClipboard(AOptions);
end;

function TdxSpreadSheetBuiltInTableViewPopupMenu.GetFocusedContainer: TdxSpreadSheetContainer;
begin
  Result := TdxSpreadSheetTableViewAccess(View).Controller.FocusedContainer;
end;

function TdxSpreadSheetBuiltInTableViewPopupMenu.GetHyperlink: TdxSpreadSheetHyperlink;
begin
  Result := TdxSpreadSheetTableViewAccess(View).Controller.FocusedHyperlink;
end;

function TdxSpreadSheetBuiltInTableViewPopupMenu.GetView: TdxSpreadSheetTableView;
begin
  Result := SpreadSheet.ActiveSheetAsTable;
end;

end.
