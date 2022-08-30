unit ViewBandedDemoMain;

interface

uses
  Windows, Messages, Classes, Graphics, Controls, Forms,
  Dialogs, cxGridCustomTableView, cxGridTableView, cxGridBandedTableView,
  cxGridDBBandedTableView, cxControls, cxGridCustomView, StdCtrls, Menus,
  cxGridLevel, cxGrid, ActnList, ImgList, cxLookAndFeels,
  ComCtrls, cxStyles, cxCustomData, cxGraphics, cxFilter,
  cxData, cxEdit, cxClasses, cxListBox, cxDataStorage, DB, cxDBData,
  cxImageComboBox, cxDBLookupComboBox, cxCalendar, cxLookAndFeelPainters,
  BaseForm, cxNavigator, cxGridCardView;

type
  TViewBandedDemoMainForm = class(TfmBaseForm)
    glItems: TcxGridLevel;
    cxGrid: TcxGrid;
    btvItems: TcxGridDBBandedTableView;
    btvItemsNAME: TcxGridDBBandedColumn;
    btvItemsTYPE: TcxGridDBBandedColumn;
    btvItemsPROJECTID: TcxGridDBBandedColumn;
    btvItemsPRIORITY: TcxGridDBBandedColumn;
    btvItemsSTATUS: TcxGridDBBandedColumn;
    btvItemsCREATEDDATE: TcxGridDBBandedColumn;
    btvItemsFIXEDDATE: TcxGridDBBandedColumn;
    btvItemsDESCRIPTION: TcxGridDBBandedColumn;
    btvItemsOwnerName: TcxGridDBBandedColumn;
    btvItemsCreatorName: TcxGridDBBandedColumn;
    miOptions: TMenuItem;
    ilMain: TImageList;
    btvItemsCreatorPhone: TcxGridDBBandedColumn;
    btvItemsOwnerPhone: TcxGridDBBandedColumn;
    miShowPreviewRow: TMenuItem;
    miBandMoving: TMenuItem;
    miBandSizing: TMenuItem;
    miDeleteBand: TMenuItem;
    miCreateBand: TMenuItem;
    miColumnsCustomization: TMenuItem;
    N1: TMenuItem;
    miShowNavigator: TMenuItem;
    procedure FormShow(Sender: TObject);
    procedure miShowPreviewRowClick(Sender: TObject);
    procedure miBandMovingClick(Sender: TObject);
    procedure miBandSizingClick(Sender: TObject);
    procedure miColumnsCustomizationClick(Sender: TObject);
    procedure miCreateBandClick(Sender: TObject);
    procedure miDeleteBandClick(Sender: TObject);
    procedure miShowNavigatorClick(Sender: TObject);
  private
    procedure DeleteBand(ABandCaption: String);
    function GetBandByCaption(ABandCaption: String): TcxGridBand;
  protected
    procedure UpdateButtons;
  end;

var
  ViewBandedDemoMainForm: TViewBandedDemoMainForm;

implementation

{$R *.dfm}

uses
  ViewBandedDemoData, ViewBandeDemoBands, AboutDemoForm;

procedure TViewBandedDemoMainForm.FormShow(Sender: TObject);
begin
  TcxGridDBBandedTableView(cxGrid.FocusedView).DataController.Groups.FullExpand;
end;

procedure TViewBandedDemoMainForm.miShowPreviewRowClick(Sender: TObject);
begin
  if GetMenuItemChecked(Sender) then
  begin
    //btvItemsDESCRIPTION.Visible := False;
    btvItems.Preview.Column := btvItemsDESCRIPTION;
    btvItems.Preview.Visible := True;
  end
  else
  begin
    btvItems.Preview.Visible := False;
    btvItems.Preview.Column := nil;
  end;
end;

procedure TViewBandedDemoMainForm.miBandMovingClick(Sender: TObject);
begin
  btvItems.OptionsCustomize.BandMoving := GetMenuItemChecked(Sender);
end;

procedure TViewBandedDemoMainForm.miBandSizingClick(Sender: TObject);
begin
  btvItems.OptionsCustomize.BandSizing := GetMenuItemChecked(Sender);
end;

procedure TViewBandedDemoMainForm.DeleteBand(ABandCaption: String);
begin
  GetBandByCaption(ABandCaption).Free;
end;

function TViewBandedDemoMainForm.GetBandByCaption(ABandCaption: String): TcxGridBand;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to btvItems.Bands.Count - 1 do
    if btvItems.Bands[i].Caption = ABandCaption then
    begin
      Result := btvItems.Bands[i];
      Exit;
    end;
end;

procedure TViewBandedDemoMainForm.UpdateButtons;
begin
  MenuItemSetEnabled('miDeleteBand', btvItems.Bands.Count <> 0);
  MenuItemSetEnabled('miShowPreviewRow', btvItems.Bands.Count <> 0);
end;

procedure TViewBandedDemoMainForm.miColumnsCustomizationClick(Sender: TObject);
begin
  btvItems.Controller.Customization := True;
end;

procedure TViewBandedDemoMainForm.miCreateBandClick(Sender: TObject);
var
  BandCaption: String;
  ABand: TcxGridBand;
  ABandViewInfo: TcxGridbandViewInfo;
begin
  if InputQuery('Create band', 'Specify a caption of the band', BandCaption) then
    if GetBandByCaption(BandCaption) <> nil then
      MessageDlg('Band with this caption already exists', mtWarning, [mbOK], 0)
    else
    begin
      ABand := btvItems.Bands.Add;
      ABand.Caption := BandCaption;
      ABandViewInfo :=
        btvItems.ViewInfo.HeaderViewInfo.BandsViewInfo[ABand.VisibleIndex];
      btvItems.Controller.LeftPos := ABandViewInfo.Bounds.Right;
    end;
  UpdateButtons;
end;

procedure TViewBandedDemoMainForm.miDeleteBandClick(Sender: TObject);

  procedure HideCoulmns;
  var
    j: Integer;
  begin
    for j:=0 to btvItems.ColumnCount - 1 do
      if (btvItems.Columns[j].Position.Band = nil) and btvItems.Columns[j].Visible then
        btvItems.Columns[j].Visible := False;
  end;

  procedure AddBands(AStringList: TStrings);
  var
    i: Integer;
  begin
    for i:=0 to btvItems.Bands.Count - 1 do
      AStringList.Add(btvItems.Bands[i].Caption);
  end;

  procedure RemoveBands(AListBox: TcxListBox);
  var
    i: Integer;
  begin
    for i:=0 to AListBox.Items.Count - 1 do
      if AListBox.Selected[i] then
        DeleteBand(AListBox.Items[i]);
    HideCoulmns;
  end;

begin
  with TViewBandeDemoBandsForm.Create(nil) do
  try
    lbBands.Items.Clear;
    AddBands(lbBands.Items);
    if ShowModal = mrOK then
      RemoveBands(lbBands);
  finally
    Free;
  end;
  UpdateButtons;
end;

procedure TViewBandedDemoMainForm.miShowNavigatorClick(Sender: TObject);
begin
  btvItems.Navigator.Visible := GetMenuItemChecked(Sender);
end;

end.
