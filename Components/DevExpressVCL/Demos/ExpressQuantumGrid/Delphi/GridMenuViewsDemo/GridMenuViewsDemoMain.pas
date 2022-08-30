unit GridMenuViewsDemoMain;

{$I cxVer.inc}

interface

uses
  Variants, Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, cxGridLevel, cxGridCustomTableView, cxGridTableView,
  cxGridDBTableView, cxControls, cxGridCustomView, cxGrid, cxCustomData,
  ExtCtrls, ActnList, ImgList, Menus, ComCtrls, cxGridCustomPopupMenu,
  cxGridPopupMenu, ToolWin, cxStyles, cxGraphics,
  cxFilter, cxData, cxEdit, DB, cxDBData, cxClasses, cxDataStorage,
  cxDBLookupComboBox, cxCalendar, cxImageComboBox, cxCalc, cxBlobEdit,
  cxSpinEdit, cxLookAndFeels, cxLookAndFeelPainters, BaseForm, cxGridCardView, CarsDataForGrid, cxNavigator;

type
  TGridMenuViewsDemoMainForm = class(TfmBaseForm)
    miOptions: TMenuItem;
    Grid: TcxGrid;
    tvOrders: TcxGridDBTableView;
    tvOrdersCustomerID: TcxGridDBColumn;
    tvOrdersPurchaseDate: TcxGridDBColumn;
    tvOrdersPaymentType: TcxGridDBColumn;
    tvOrdersPaymentAmount: TcxGridDBColumn;
    tvOrdersDescription: TcxGridDBColumn;
    tvOrdersQuantity: TcxGridDBColumn;
    GridPopupMenu: TcxGridPopupMenu;
    lvOrders: TcxGridLevel;
    tvOrdersProductID: TcxGridDBColumn;
    tvOrdersPurchaseMonth: TcxGridDBColumn;
    PopupMenu: TPopupMenu;
    miDelete: TMenuItem;
    miInsert: TMenuItem;
    CustomizePopupmenus1: TMenuItem;
    miUseBuiltInPopupMenu: TMenuItem;
    miAddCopyToClipboard: TMenuItem;
    miUseCustomPopupMenu: TMenuItem;
    procedure FormShow(Sender: TObject);
    procedure miCopyToClipboardClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure GridMenuPopup(ASenderMenu: TComponent;
      AHitTest: TcxCustomGridHitTest; X, Y: Integer);
    procedure miDeleteClick(Sender: TObject);
    procedure miInsertClick(Sender: TObject);
    procedure miUseBuiltInPopupMenuClick(Sender: TObject);
    procedure miAddCopyToClipboardClick(Sender: TObject);
    procedure miUseCustomPopupMenuClick(Sender: TObject);
  protected
    procedure LookAndFeelChanged(Sender: TcxLookAndFeel;
      AChangedValues: TcxLookAndFeelValues); override;
  public
    FMenuItem: TMenuItem;
    procedure InsertMenuItem;
  end;

var
  GridMenuViewsDemoMainForm: TGridMenuViewsDemoMainForm;

implementation

{$R *.dfm}

uses
  GridMenuViewsDemoData, Clipbrd, AboutDemoForm;

procedure TGridMenuViewsDemoMainForm.LookAndFeelChanged(
  Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues);
begin
  inherited LookAndFeelChanged(Sender, AChangedValues);
  UpdateTableViewStyleSheet(tvOrders);
end;

procedure TGridMenuViewsDemoMainForm.FormShow(Sender: TObject);
begin
  if GridMenuViewsDemoDataDM.cdsOrders.Active then
  begin
    tvOrders.DataController.Groups.FullCollapse;
    tvOrders.DataController.Groups.ChangeExpanding(0, False, False);
    tvOrders.DataController.GotoFirst;
  end
end;

procedure TGridMenuViewsDemoMainForm.FormCreate(Sender: TObject);
begin
  InsertMenuItem;
end;

procedure TGridMenuViewsDemoMainForm.miDeleteClick(Sender: TObject);
begin
  tvOrders.Controller.DeleteSelection;
end;

procedure TGridMenuViewsDemoMainForm.miInsertClick(Sender: TObject);
begin
  tvOrders.DataController.Insert;
end;

procedure TGridMenuViewsDemoMainForm.miCopyToClipboardClick(Sender: TObject);

  function GetSummaryItemIndexByColumn(ASummaryItems: TcxDataSummaryItems;
    AColumn: TcxGridColumn): Integer;
  var
    I: Integer;
  begin
    Result := -1;
    for I := 0 to ASummaryItems.Count - 1 do
      if (TcxGridDBTableSummaryItem(ASummaryItems[I]).Column = AColumn)
        and (ASummaryItems[I].Position = spFooter) then
      begin
        Result :=  I;
        Break;
      end;
  end;

  function GetFooterSummaryValue(AHitTest: TcxGridFooterCellHitTest): Variant;
  var
    ASummary: TcxDataSummary;
  begin
    ASummary := tvOrders.DataController.Summary;
    Result := ASummary.FooterSummaryValues[
      GetSummaryItemIndexByColumn(ASummary.FooterSummaryItems, AHitTest.Column)];
  end;

  function GetGroupFooterSummaryValue(AHitTest: TcxGridGroupFooterCellHitTest): Variant;
  var
    ASummary: TcxDataSummary;
    ARowIndex, ADataGroupIndex: Integer;
    ADataControllerGroups: TcxDataControllerGroups;
  begin
    ASummary := tvOrders.DataController.Summary;
    ADataControllerGroups := tvOrders.DataController.Groups;
    ARowIndex := (AHitTest.ViewInfo as TcxGridRowFooterCellViewInfo).GridRecord.Index;
    ADataGroupIndex := ADataControllerGroups.DataGroupIndexByRowIndex[ARowIndex];
    Result := ASummary.GroupSummaryValues[ ADataGroupIndex,
      GetSummaryItemIndexByColumn(ASummary.DefaultGroupSummaryItems, AHitTest.Column)];
  end;

var
  AHitTest: TcxCustomGridHitTest;
  AHitType: TcxGridViewHitType;
  AValue: Variant;
begin
  AHitTest := GridPopupMenu.HitTest;
  if AHitTest.ViewInfo is TcxGridFooterCellViewInfo then
  begin
    AHitType := GetHitTypeByHitCode(AHitTest.HitTestCode);
    case AHitType of
      gvhtFooterCell:
        AValue := GetFooterSummaryValue(TcxGridFooterCellHitTest(AHitTest));
      gvhtGroupFooterCell:
        AValue := GetGroupFooterSummaryValue(TcxGridGroupFooterCellHitTest(AHitTest));
    end;
    Clipboard.AsText := VarToStr(AValue);
    MessageDlg('Clipboard: '+ VarToStr(AValue), mtInformation, [mbOK], 0);
  end;
end;

procedure TGridMenuViewsDemoMainForm.InsertMenuItem;
var
  I: Integer;
  AMenu: TComponent;
  ABuiltInMenus: TcxGridDefaultPopupMenu;
begin
  AMenu := nil;
  ABuiltInMenus := GridPopupMenu.BuiltInPopupMenus;
  for I := 0 to ABuiltInMenus.Count - 1 do
    if ([gvhtFooter, gvhtFooterCell, gvhtGroupFooter, gvhtGroupFooterCell] *
      ABuiltInMenus[I].HitTypes) <> [] then
    begin
      AMenu := ABuiltInMenus[I].PopupMenu;
    end;
  if Assigned(AMenu) and AMenu.InheritsFrom(TPopupMenu) then
  begin
    FMenuItem := TMenuItem.Create(Self);
    FMenuItem.Caption := '-';
    TPopupMenu(AMenu).Items.Add(FMenuItem);
    
    FMenuItem := TMenuItem.Create(Self);
    FMenuItem.Caption := 'Copy to clipboard';
    FMenuItem.Hint := 'Copy the contents to clipboard';
    FMenuItem.OnClick := miCopyToClipboardClick;
    TPopupMenu(AMenu).Items.Add(FMenuItem);
  end;
end;

procedure TGridMenuViewsDemoMainForm.GridMenuPopup(
  ASenderMenu: TComponent; AHitTest: TcxCustomGridHitTest; X, Y: Integer);
begin
  PopupMenu.Popup(X, Y);
end;

procedure TGridMenuViewsDemoMainForm.miUseBuiltInPopupMenuClick(Sender: TObject);
begin
  GridPopupMenu.UseBuiltInPopupMenus := GetMenuItemChecked(Sender);
  if GridPopupMenu.UseBuiltInPopupMenus then
  begin
    InsertMenuItem;
    MenuItemSetEnabled('miAddCopyToClipboard', True);
    MenuItemSetChecked('miAddCopyToClipboard', True);
  end
  else
    MenuItemSetEnabled('miAddCopyToClipboard', False);
end;

procedure TGridMenuViewsDemoMainForm.miAddCopyToClipboardClick(Sender: TObject);
begin
  if GridPopupMenu.UseBuiltInPopupMenus then
    FMenuItem.Visible := GetMenuItemChecked(Sender);
end;

procedure TGridMenuViewsDemoMainForm.miUseCustomPopupMenuClick(Sender: TObject);
begin
  if GetMenuItemChecked(Sender) then
  begin
    GridPopupMenu[0].OnPopup := GridMenuPopup;
    GridPopupMenu[0].PopupMenu := PopupMenu;
  end
  else
  begin
    GridPopupMenu[0].OnPopup := nil;
    GridPopupMenu[0].PopupMenu := nil;
  end;
end;

end.  
