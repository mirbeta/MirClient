{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressPrinting System                                   }
{                                                                    }
{           Copyright (C) 1998-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSPRINTING SYSTEM AND            }
{   ALL ACCOMPANYING VCL CONTROLS AS PART OF AN                      }
{   EXECUTABLE PROGRAM ONLY.                                         }
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

unit dxPSCPDsg;

interface

{$I cxVer.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, ImgList, Menus, IniFiles, dxCore, dxPSCore, dxPSForm,
  cxLookAndFeelPainters, cxButtons, cxControls, cxContainer, cxListView, cxPC, cxEdit,
  cxGroupBox, cxLabel, cxGraphics, cxLookAndFeels, dxLayoutContainer, dxLayoutControlAdapters,
  dxLayoutLookAndFeels, cxClasses, dxLayoutControl, cxImageList;

type
  TdxfmCPDesignerOption = (doShowDescription);
  TdxfmCPDesignerOptions = set of TdxfmCPDesignerOption;

  TdxfmCPDesigner = class(TCustomdxPSForm)
    pmItems: TPopupMenu;
    miDesign: TMenuItem;
    miLine2: TMenuItem;
    miAdd: TMenuItem;
    miDelete: TMenuItem;
    lvItems: TcxListView;
    miPageSetup: TMenuItem;
    miPrintPreview: TMenuItem;
    miPrint: TMenuItem;
    miLine1: TMenuItem;
    miRename: TMenuItem;
    miAddComposition: TMenuItem;
    btnClose: TcxButton;
    btnHelp: TcxButton;
    btnDesign: TcxButton;
    btnAdd: TcxButton;
    btnDelete: TcxButton;
    btnPageSetup: TcxButton;
    btnPrintPreview: TcxButton;
    btnPrint: TcxButton;
    btnAddComposition: TcxButton;
    ilItems: TcxImageList;
    lcMainGroup_Root: TdxLayoutGroup;
    lcMain: TdxLayoutControl;
    dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList;
    dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel;
    dxLayoutGroup1: TdxLayoutGroup;
    tbsItems: TdxLayoutGroup;
    dxLayoutItem1: TdxLayoutItem;
    dxLayoutGroup3: TdxLayoutGroup;
    dxLayoutItem2: TdxLayoutItem;
    dxLayoutItem3: TdxLayoutItem;
    dxLayoutItem4: TdxLayoutItem;
    dxLayoutItem5: TdxLayoutItem;
    dxLayoutItem6: TdxLayoutItem;
    dxLayoutItem7: TdxLayoutItem;
    dxLayoutItem8: TdxLayoutItem;
    dxLayoutGroup4: TdxLayoutGroup;
    dxLayoutItem9: TdxLayoutItem;
    libtnHelp: TdxLayoutItem;
    pnlNoItems: TcxLabel;
    procedure lvItemsChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure AddClick(Sender: TObject);
    procedure AddCompositionClick(Sender: TObject);
    procedure DeleteClick(Sender: TObject);
    procedure pmItemsPopup(Sender: TObject);
    procedure DesignClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure lvItemsEdited(Sender: TObject; Item: TListItem; var S: String);
    procedure lvItemsEditing(Sender: TObject; Item: TListItem; var AllowEdit: Boolean);
    procedure lvItemsDblClick(Sender: TObject);
    procedure PageSetupClick(Sender: TObject);
    procedure PrintPreviewClick(Sender: TObject);
    procedure PrintClick(Sender: TObject);
    procedure RenameClick(Sender: TObject);
    procedure lvItemsResize(Sender: TObject);
  private
    FComponentPrinter: TdxComponentPrinter;
    FIsRefreshing: Boolean;
    FListViewWndProc: TWndMethod;
    FOptions: TdxfmCPDesignerOptions;

    function GetCurrentLink: TBasedxReportLink;
    function GetItem(Index: Integer): TBasedxReportLink;
    function GetItemCount: Integer;
    function GetSelected(Index: Integer): Boolean;
    function GetSelectedCount: Integer;
    procedure SetCurrentLink(Value: TBasedxReportLink);
    procedure SetSelected(Index: Integer; Value: Boolean);

    function CanAdd: Boolean;
    function CanAddComposition: Boolean;
    function CanDelete: Boolean;
    function CanDesign: Boolean;
    function CanPageSetup: Boolean;
    function CanPrint: Boolean;
    function CanPrintPreview: Boolean;
    function CanRename: Boolean;

    procedure DeleteSelection;
    function GetSelectedItemsAsString: string;
    function HasOnlyBuiltInsAreInSelection: Boolean;
    procedure Initialize;
    procedure LoadStrings;
    procedure RefreshColumns;
    procedure RefreshList;
    procedure UpdateControlsState;

    procedure ListViewWndProc(var Message: TMessage);
    procedure SubClassListView;
    procedure UnsubClassListView;

    procedure CMDialogChar(var Msg: TCMDialogChar); message CM_DIALOGCHAR;
  protected
    procedure BeforeConstruction; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Execute;
    procedure LoadFromIniFile(AIniFile: TCustomIniFile; const ASectionName: string); override;
    procedure SaveToIniFile(AIniFile: TCustomIniFile; const ASectionName: string); override;

    property ComponentPrinter: TdxComponentPrinter read FComponentPrinter write FComponentPrinter;
    property CurrentLink: TBasedxReportLink read GetCurrentLink write SetCurrentLink;
    property ItemCount: Integer read GetItemCount;
    property Items[Index: Integer]: TBasedxReportLink read GetItem;
    property Selected[Index: Integer]: Boolean read GetSelected write SetSelected;
    property SelectedCount: Integer read GetSelectedCount;
  end;

  PdxCPDesignerDlgData = ^TdxCPDesignerDlgData;
  TdxCPDesignerDlgData = record
    ComponentPrinter: TdxComponentPrinter;
    Options: TdxfmCPDesignerOptions;
  end;

procedure dxShowCPDesignerDlg(const AData: TdxCPDesignerDlgData);

implementation

{$R *.DFM}

uses
  Math, CommCtrl, dxPSGlbl, dxPSUtl, dxPSRes, dxPSPopupMan, dxPSfmLnkAdd, dxPSCompsProvider, dxDPIAwareUtils, cxGeometry;

procedure dxShowCPDesignerDlg(const AData: TdxCPDesignerDlgData);
var
  Dialog: TdxfmCPDesigner;
begin
  if AData.ComponentPrinter = nil then Exit;

  Dialog := TdxfmCPDesigner.Create(nil);
  try
    Dialog.ComponentPrinter := AData.ComponentPrinter;
    Dialog.FOptions := AData.Options;
    Dialog.Execute;
  finally
    Dialog.Free;
  end;
end;

{ TdxfmCPDesigner }

procedure TdxfmCPDesigner.lvItemsChange(Sender: TObject; Item: TListItem; Change: TItemChange);
begin
  if (Change = ctState) and not FIsRefreshing then
    UpdateControlsState;
end;

procedure TdxfmCPDesigner.pmItemsPopup(Sender: TObject);
begin
  miAdd.Enabled := CanAdd;
  miAddComposition.Enabled := CanAddComposition;
  miDelete.Enabled := CanDelete;
  miRename.Enabled := CanRename;
  miDesign.Enabled := CanDesign;
  miPageSetup.Enabled := CanPageSetup;
  miPrint.Enabled := CanPrint;
  miPrintPreview.Enabled := CanPrintPreview;
end;

procedure TdxfmCPDesigner.AddClick(Sender: TObject);
var
  AData: TdxAddReportLinkDlgData;
  ALink: TBasedxReportLink;
  I: Integer;
begin
  AData.Initialize;
  try
    AData.ComponentPrinter := FComponentPrinter;
    AData.Options := [adoShowDesignButton, adoShowDescriptionColumn, adoAllowMultiSelect, adoShowCaptionEdit, adoAllowSelectLinkClass];
    AData.ReportLinkCaption := cxGetResourceString(@sdxNewReport);

    if dxShowAddComponentsDlg(AData) then
      for I := 0 to AData.Components.Count - 1 do
      begin
        ALink := ComponentPrinter.AddEmptyLink(TdxReportLinkClass(AData.ComponentsLinkClasses[I]));
        ALink.Component := TComponent(AData.Components[I]);
        ALink.Caption := AData.ReportLinkCaption;
        ALink.Description := AData.ReportLinkDescription;
        if AData.Components.Count = 1 then
          ALink.Name := AData.ReportLinkName;
        RefreshList;
        CurrentLink := ALink;
        if AData.DesignBtnPressed then
          ComponentPrinter.DesignReport(ALink);
      end;
  finally
    AData.Finalize;
  end;
end;

procedure TdxfmCPDesigner.AddCompositionClick(Sender: TObject);
var
  Link: TBasedxReportLink;
begin
  Link := ComponentPrinter.AddEmptyLink(TdxCompositionReportLink);
  Link.Caption := cxGetResourceString(@sdxNewCompositionCaption);
  RefreshList;
  CurrentLink := Link;
  RenameClick(nil);
end;

procedure TdxfmCPDesigner.DeleteClick(Sender: TObject);
begin
  if not lvItems.IsEditing and
    MessageQuestion(Format(cxGetResourceString(@sdxConfirmDeleteItem), [GetSelectedItemsAsString])) then
    DeleteSelection;
end;

procedure TdxfmCPDesigner.RenameClick(Sender: TObject);
begin
  if SelectedCount = 1 then
    lvItems.Selected.EditCaption;
end;

procedure TdxfmCPDesigner.DesignClick(Sender: TObject);
begin
  if CurrentLink.DesignReport then
    RefreshList
  else
    UpdateControlsState;
end;

procedure TdxfmCPDesigner.PageSetupClick(Sender: TObject);
begin
  CurrentLink.PageSetup;
end;

procedure TdxfmCPDesigner.PrintPreviewClick(Sender: TObject);
begin
  CurrentLink.Preview(True);
end;

procedure TdxfmCPDesigner.PrintClick(Sender: TObject);
begin
  CurrentLink.Print(True, nil);
end;

procedure TdxfmCPDesigner.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := not lvItems.IsEditing;
end;

procedure TdxfmCPDesigner.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_ESCAPE) or (Key = VK_RETURN) then Close;
end;

procedure TdxfmCPDesigner.lvItemsEditing(Sender: TObject; Item: TListItem;
  var AllowEdit: Boolean);
begin
  AllowEdit := not Items[Item.Index].BuiltIn;
  if AllowEdit then
    lvItems.PopupMenu := nil;
end;

procedure TdxfmCPDesigner.lvItemsResize(Sender: TObject);
var
  R: TRect;
begin
  R := lvItems.ClientRect;
  InflateRect(R, -ScaleFactor.Apply(3), -ScaleFactor.Apply(3));
  with R do
  begin
    Top := (Bottom - Top - pnlNoItems.Height) div 2;
    Bottom := Top + pnlNoItems.Height;
  end;
  pnlNoItems.BoundsRect := R;
end;

procedure TdxfmCPDesigner.lvItemsEdited(Sender: TObject; Item: TListItem;
  var S: string);
begin
  Items[Item.Index].Caption := S;
end;

procedure TdxfmCPDesigner.lvItemsDblClick(Sender: TObject);
begin
  if SelectedCount = 1 then btnDesign.Click;
end;

constructor TdxfmCPDesigner.Create(AOwner: TComponent);
begin
  inherited;
  HelpContext := dxPSGlbl.dxhcAddComponentsToComponentPrinterDlg;
  dxPSPopupMenuController.RegisterControl(lvItems);
  SubClassListView;
end;

destructor TdxfmCPDesigner.Destroy;
begin
  UnsubClassListView;
  dxPSPopupMenuController.UnregisterControl(lvItems);
  inherited;
end;

procedure TdxfmCPDesigner.Execute;
begin
  Initialize;
  ShowModal;
end;

procedure TdxfmCPDesigner.LoadFromIniFile(
  AIniFile: TCustomIniFile; const ASectionName: string);
begin
  inherited LoadFromIniFile(AIniFile, ASectionName);
  dxLoadListViewColumns(AIniFile, ASectionName, lvItems.InnerListView);
end;

procedure TdxfmCPDesigner.SaveToIniFile(
  AIniFile: TCustomIniFile; const ASectionName: string);
begin
  inherited SaveToIniFile(AIniFile, ASectionName);
  dxSaveListViewColumns(AIniFile, ASectionName, lvItems.InnerListView);
end;

procedure TdxfmCPDesigner.BeforeConstruction;
begin
  inherited BeforeConstruction;
  Options := Options + [foSizeableDialog];
end;

function TdxfmCPDesigner.CanDesign: Boolean;
begin
  Result := (SelectedCount = 1) and (CurrentLink <> nil) and CurrentLink.CheckToDesign;
end;

function TdxfmCPDesigner.CanPageSetup: Boolean;
begin
  Result := (SelectedCount = 1) and (CurrentLink <> nil);
end;

function TdxfmCPDesigner.CanPrint: Boolean;
begin
  Result := (SelectedCount = 1) and (CurrentLink <> nil) and CurrentLink.DataProviderPresent;
end;

function TdxfmCPDesigner.CanPrintPreview: Boolean;
begin
  Result := (SelectedCount = 1) and (CurrentLink <> nil) and CurrentLink.DataProviderPresent;
end;

function TdxfmCPDesigner.CanRename: Boolean;
begin
  Result := (SelectedCount = 1) and (CurrentLink <> nil) and not CurrentLink.BuiltIn;
end;

procedure TdxfmCPDesigner.DeleteSelection;
var
  I: Integer;
begin
  for I := 0 to ItemCount - 1 do
    if Selected[I] and not Items[I].BuiltIn then
      TObject(lvItems.Items[I].Data).Free;
  RefreshList;
end;

function TdxfmCPDesigner.GetCurrentLink: TBasedxReportLink;
begin
  if SelectedCount = 1 then
    Result := TBasedxReportLink(lvItems.Selected.Data)
  else
    Result := nil;
end;

function TdxfmCPDesigner.GetItem(Index: Integer): TBasedxReportLink;
begin
  Result := TBasedxReportLink(lvItems.Items[Index].Data);
end;

function TdxfmCPDesigner.GetItemCount: Integer;
begin
  Result := lvItems.Items.Count;
end;

function TdxfmCPDesigner.GetSelected(Index: Integer): Boolean;
begin
  Result := lvItems.Items[Index].Selected;
end;

function TdxfmCPDesigner.GetSelectedCount: Integer;
begin
  Result := lvItems.SelCount;
end;

procedure TdxfmCPDesigner.SetCurrentLink(Value: TBasedxReportLink);
var
  ListItem: TListItem;
  I: Integer;
begin
  ListItem := lvItems.FindData(0, Value, True, True);
  if ListItem <> nil then
  begin
    lvItems.Items.BeginUpdate;
    try
      for I := 0 To ItemCount - 1 do
        Selected[I] := False;
      lvItems.Selected := ListItem;
    finally
      lvItems.Items.EndUpdate;
    end;
  end;
end;

procedure TdxfmCPDesigner.SetSelected(Index: Integer; Value: Boolean);
begin
  lvItems.Items[Index].Selected := Value;
end;

function TdxfmCPDesigner.CanAdd: Boolean;
begin
  Result := True;
end;

function TdxfmCPDesigner.CanAddComposition: Boolean;
begin
  Result := True;
end;

function TdxfmCPDesigner.CanDelete: Boolean;
begin
  Result := (SelectedCount <> 0) and not HasOnlyBuiltInsAreInSelection;
end;

procedure TdxfmCPDesigner.LoadStrings;
begin
  Caption := cxGetResourceString(@sdxEditReports);

  tbsItems.Caption := cxGetResourceString(@sdxItems);
  pnlNoItems.Caption := cxGetResourceString(@sdxThereAreNowItemsForShow);

  btnAdd.Caption := AddEndEllipsis(cxGetResourceString(@sdxBtnAdd));
  btnAddComposition.Caption := cxGetResourceString(@sdxBtnAddComposition);
  btnDelete.Caption := cxGetResourceString(@sdxBtnDelete);
  btnDesign.Caption := cxGetResourceString(@sdxBtnDesign);
  btnPageSetup.Caption := cxGetResourceString(@sdxBtnPageSetup);
  btnPrint.Caption := cxGetResourceString(@sdxBtnPrint);
  btnPrintPreview.Caption := cxGetResourceString(@sdxBtnPreview);

  btnClose.Caption := cxGetResourceString(@sdxBtnClose);
  btnHelp.Caption := cxGetResourceString(@sdxBtnHelp);

  miAdd.Caption := AddEndEllipsis(cxGetResourceString(@sdxBtnAdd));
  miAddComposition.Caption := cxGetResourceString(@sdxBtnAddComposition);
  miDelete.Caption := cxGetResourceString(@sdxBtnDelete);
  miRename.Caption := cxGetResourceString(@sdxRename);
  miDesign.Caption := cxGetResourceString(@sdxBtnDesign);
  miPageSetup.Caption := cxGetResourceString(@sdxBtnPageSetup);
  miPrint.Caption := cxGetResourceString(@sdxBtnPrint);
  miPrintPreview.Caption := cxGetResourceString(@sdxBtnPreview);
end;

function TdxfmCPDesigner.GetSelectedItemsAsString: string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to ItemCount - 1 do
    if Selected[I] and not Items[I].BuiltIn then
    begin
      if Result <> '' then Result := Result + ', ';
      Result := Result + '"' + lvItems.Items[I].Caption + '"';
    end;
end;

function TdxfmCPDesigner.HasOnlyBuiltInsAreInSelection: Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to ItemCount - 1 do
    if Selected[I] and not Items[I].BuiltIn then
      Exit;
  Result := True;
end;

procedure TdxfmCPDesigner.Initialize;
begin
  CheckDialogFormHelpContext(Self, libtnHelp);
  LoadStrings;

  RefreshColumns;
  RefreshList;

  pnlNoItems.Parent := lvItems;
  lvItemsResize(nil);
end;

procedure TdxfmCPDesigner.RefreshColumns;
var
  AItem: TListColumn;
  ColumnWidths: array of Integer;
  I: Integer;
begin
  lvItems.Columns.BeginUpdate;
  try
    SetLength(ColumnWidths, lvItems.Columns.Count);
    for I := 0 to lvItems.Columns.Count - 1 do
      ColumnWidths[I] := ScaleFactor.Revert(lvItems.Columns[I].Width);

    lvItems.Columns.Clear;
    AItem := lvItems.Columns.Add;
    AItem.Width := 2 * (lvItems.Width - dxGetSystemMetrics(SM_CXHSCROLL, ScaleFactor) - 2) div 3 - ScaleFactor.Apply(3);
    AItem.Caption := cxGetResourceString(@sdxItemName);

    if doShowDescription in FOptions then
    begin
      AItem := lvItems.Columns.Add;
      AItem.Width := (lvItems.Width - dxGetSystemMetrics(SM_CXHSCROLL, ScaleFactor) - 2) div 3 - ScaleFactor.Apply(3);
      AItem.Caption := cxGetResourceString(@sdxItemDescription);
    end;

    for I := 0 to MinIntValue([lvItems.Columns.Count - 1, Length(ColumnWidths) - 1]) do
      lvItems.Columns[I].Width := ScaleFactor.Apply(ColumnWidths[I]);
  finally
    lvItems.Columns.EndUpdate;
  end;
end;

procedure TdxfmCPDesigner.RefreshList;
const
  ViewStyles: array[Boolean] of TViewStyle = (vsReport, vsList);
var
  Selection: TList;
  I: Integer;
  ReportLink: TBasedxReportLink;
begin
  lvItems.Items.BeginUpdate;
  FIsRefreshing := True;
  try
    Selection := TList.Create;
    try
      dxPSUtl.dxSaveListViewSelection(lvItems.InnerListView, Selection);

      lvItems.Items.Clear;
      for I := 0 to ComponentPrinter.LinkCount - 1 do
      begin
        ReportLink := ComponentPrinter[I];
        with lvItems.Items.Add do
        begin
          Caption := ReportLink.Caption;
          Data := ReportLink;
          SubItems.Add(ReportLink.Description);
        end;
      end;

      dxPSUtl.dxRestoreListViewSelection(lvItems.InnerListView, Selection);
     finally
       Selection.Free;
     end;

    lvItems.ViewStyle := ViewStyles[ItemCount = 0];
    lvItems.Enabled := ItemCount <> 0;
    lvItems.HideSelection := ItemCount = 0;
    if (lvItems.SelCount = 0) and (ItemCount <> 0) then
      lvItems.Selected := lvItems.Items[0];
  finally
    FIsRefreshing := False;
    lvItems.Items.EndUpdate;
  end;
  UpdateControlsState;
  lvItems.Refresh;
end;

procedure TdxfmCPDesigner.UpdateControlsState;
begin
  pnlNoItems.Visible := ItemCount = 0;
  btnAdd.Enabled := CanAdd;
  btnAddComposition.Enabled := CanAddComposition;
  btnDelete.Enabled := CanDelete;
  btnDesign.Enabled := CanDesign;
  btnPageSetup.Enabled := CanPageSetup;
  btnPrint.Enabled := CanPrint;
  btnPrintPreview.Enabled := CanPrintPreview;
end;

procedure TdxfmCPDesigner.ListViewWndProc(var Message: TMessage);
begin
  FListViewWndProc(Message);
  if Message.Msg = CN_NOTIFY then
    if TWMNotify(Message).NMHdr^.code = LVN_ENDLABELEDIT then
      lvItems.PopupMenu := pmItems;
end;

procedure TdxfmCPDesigner.SubClassListView;
begin
  lvItems.HandleNeeded;
  FListViewWndProc := lvItems.WindowProc;
  lvItems.WindowProc := ListViewWndProc;
end;

procedure TdxfmCPDesigner.UnsubClassListView;
begin
  lvItems.WindowProc := FListViewWndProc;
end;

procedure TdxfmCPDesigner.CMDialogChar(var Msg: TCMDialogChar);
begin
  inherited;
  if IsAccel(Msg.CharCode, tbsItems.Caption) then
  begin
    Msg.Result := 1;
    if lvItems.Enabled and lvItems.CanFocus then
      ActiveControl := lvItems;
  end;
end;

end.
