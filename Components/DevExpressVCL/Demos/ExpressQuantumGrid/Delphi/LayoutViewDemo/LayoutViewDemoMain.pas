unit LayoutViewDemoMain;

interface

{$I cxVer.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, cxStyles, cxCustomData, cxGraphics, cxFilter, cxData, cxDataStorage,
  cxEdit, DB, cxDBData, cxControls, cxGridCustomView, cxGridCustomTableView,
  cxClasses, cxGridLevel, cxGrid, StdCtrls, Menus, cxMemo, cxImage, cxCurrencyEdit,
  cxHyperLinkEdit, cxTextEdit, cxEditRepositoryItems, cxLookAndFeels, cxLookAndFeelPainters,
  dxLayoutContainer, cxGridLayoutView, cxGridDBLayoutView, cxGridCustomLayoutView,
  cxContainer, cxGroupBox, dxLayoutLookAndFeels, ExtCtrls, cxButtons, 
  dxmdaset, BaseForm, cxGridTableView, cxRadioGroup, cxCheckBox, cxGridCardView,
  ComCtrls, ImgList, cxLabel, cxMaskEdit, cxDropDownEdit, CarsDataForGrid, cxNavigator;

type
  TfrmMain = class(TfmBaseForm)
    Grid: TcxGrid;
    miView: TMenuItem;
    miCustomize: TMenuItem;
    GridLevel1: TcxGridLevel;
    LayoutViewGroup_Root: TdxLayoutGroup;
    LayoutView: TcxGridDBLayoutView;
    LayoutViewGroup1: TdxLayoutGroup;
    LayoutViewGroup2: TdxLayoutGroup;
    LayoutViewGroup3: TdxLayoutGroup;
    LayoutViewLayoutItem1: TcxGridLayoutItem;
    LayoutViewRecId: TcxGridDBLayoutViewItem;
    LayoutViewLayoutItem2: TcxGridLayoutItem;
    LayoutViewID: TcxGridDBLayoutViewItem;
    LayoutViewLayoutItem3: TcxGridLayoutItem;
    LayoutViewTrademark: TcxGridDBLayoutViewItem;
    LayoutViewLayoutItem4: TcxGridLayoutItem;
    LayoutViewModel: TcxGridDBLayoutViewItem;
    LayoutViewLayoutItem5: TcxGridLayoutItem;
    LayoutViewHP: TcxGridDBLayoutViewItem;
    LayoutViewLayoutItem7: TcxGridLayoutItem;
    LayoutViewCyl: TcxGridDBLayoutViewItem;
    LayoutViewLayoutItem8: TcxGridLayoutItem;
    LayoutViewTransmissSpeedCount: TcxGridDBLayoutViewItem;
    LayoutViewLayoutItem9: TcxGridLayoutItem;
    LayoutViewTransmissAutomatic: TcxGridDBLayoutViewItem;
    LayoutViewLayoutItem10: TcxGridLayoutItem;
    LayoutViewMPG_City: TcxGridDBLayoutViewItem;
    LayoutViewLayoutItem11: TcxGridLayoutItem;
    LayoutViewMPG_Highway: TcxGridDBLayoutViewItem;
    LayoutViewLayoutItem12: TcxGridLayoutItem;
    LayoutViewCategory: TcxGridDBLayoutViewItem;
    LayoutViewLayoutItem13: TcxGridLayoutItem;
    LayoutViewDescription: TcxGridDBLayoutViewItem;
    LayoutViewLayoutItem14: TcxGridLayoutItem;
    LayoutViewHyperlink: TcxGridDBLayoutViewItem;
    LayoutViewLayoutItem15: TcxGridLayoutItem;
    LayoutViewPicture: TcxGridDBLayoutViewItem;
    LayoutViewLayoutItem16: TcxGridLayoutItem;
    LayoutViewPrice: TcxGridDBLayoutViewItem;
    rgViewMode: TcxRadioGroup;
    btnCustomize: TcxButton;
    cbCenterRecords: TcxCheckBox;
    cbShowOnlyEntireRecords: TcxCheckBox;
    cbMultiSelectRecords: TcxCheckBox;
    cbRecordCaptions: TcxCheckBox;
    cbExpandableRecords: TcxCheckBox;
    LayoutViewGroup4: TdxLayoutGroup;
    LayoutViewGroup5: TdxLayoutGroup;
    LayoutViewGroup6: TdxLayoutGroup;
    LayoutViewGroup7: TdxLayoutGroup;
    LayoutViewGroup8: TdxLayoutGroup;
    LayoutViewSpaceItem1: TdxLayoutEmptySpaceItem;
    stValues: TcxStyle;
    stItems: TcxStyle;
    LayoutViewGroup10: TdxLayoutGroup;
    LayoutViewSeparatorItem1: TdxLayoutSeparatorItem;
    LayoutViewGroup9: TdxLayoutGroup;
    LayoutViewSpaceItem2: TdxLayoutEmptySpaceItem;
    LayoutViewSpaceItem3: TdxLayoutEmptySpaceItem;
    LayoutViewSpaceItem4: TdxLayoutEmptySpaceItem;
    LayoutViewSpaceItem5: TdxLayoutEmptySpaceItem;
    LayoutViewGroup11: TdxLayoutGroup;
    stHeader: TcxStyle;
    stRecordCaption: TcxStyle;
    GroupBox: TcxGroupBox;
    Images: TcxImageList;
    stRecordSelected: TcxStyle;
    cbStretch: TcxComboBox;
    lbStretch: TcxLabel;
    procedure miCustomizeClick(Sender: TObject);
    procedure rgViewModeClick(Sender: TObject);
    procedure cbCenterRecordsClick(Sender: TObject);
    procedure btnCustomizeClick(Sender: TObject);
    procedure cbShowOnlyEntireRecordsClick(Sender: TObject);
    procedure cbMultiSelectRecordsClick(Sender: TObject);
    procedure cbExpandableRecordsClick(Sender: TObject);
    procedure cbRecordCaptionsClick(Sender: TObject);
    procedure LayoutViewTrademarkStylesGetContentStyle(
      Sender: TcxCustomGridTableView; ARecord: TcxCustomGridRecord;
      AItem: TcxCustomGridTableItem; var AStyle: TcxStyle);
    procedure cbStretchPropertiesChange(Sender: TObject);
  public
  {$IFDEF EXPRESSBARS}
    procedure PlaceControls; override;
  {$ENDIF}
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.miCustomizeClick(Sender: TObject);
begin
  (Grid.ActiveView as TcxGridLayoutView).Controller.Customization := True;
end;

procedure TfrmMain.btnCustomizeClick(Sender: TObject);
begin
  LayoutView.Controller.Customization := True;
end;

procedure TfrmMain.cbCenterRecordsClick(Sender: TObject);
begin
  LayoutView.OptionsView.CenterRecords := cbCenterRecords.Checked;
end;

procedure TfrmMain.cbExpandableRecordsClick(Sender: TObject);
begin
  LayoutView.OptionsCustomize.RecordExpanding := cbExpandableRecords.Checked;
end;

procedure TfrmMain.cbMultiSelectRecordsClick(Sender: TObject);
begin
  LayoutView.OptionsSelection.MultiSelect := cbMultiSelectRecords.Checked;
end;

procedure TfrmMain.cbShowOnlyEntireRecordsClick(Sender: TObject);
begin
  LayoutView.OptionsView.ShowOnlyEntireRecords := cbShowOnlyEntireRecords.Checked;
end;

procedure TfrmMain.cbStretchPropertiesChange(Sender: TObject);
begin
  LayoutView.OptionsView.SingleRecordStretch := TcxGridLayoutViewSingleRecordStretch(cbStretch.ItemIndex);
end;

procedure TfrmMain.LayoutViewTrademarkStylesGetContentStyle(
  Sender: TcxCustomGridTableView; ARecord: TcxCustomGridRecord;
  AItem: TcxCustomGridTableItem; var AStyle: TcxStyle);
begin
  AStyle := stHeader;
end;

procedure TfrmMain.cbRecordCaptionsClick(Sender: TObject);
begin
  LayoutView.OptionsView.RecordCaption.Visible := cbRecordCaptions.Checked;
end;

procedure TfrmMain.rgViewModeClick(Sender: TObject);
begin
  LayoutView.OptionsView.ViewMode := TcxGridLayoutViewViewMode(rgViewMode.ItemIndex);
  cbStretch.Enabled := LayoutView.OptionsView.ViewMode = lvvmSingleRecord;
  lbStretch.Enabled := cbStretch.Enabled;
end;

{$IFDEF EXPRESSBARS}
procedure TfrmMain.PlaceControls;
begin
  DisableAlign;
  lbDescription.Align := alNone;
  GroupBox.Align := alNone;
  lbDescription.Top := 100;
  GroupBox.Top := 200;
  EnableAlign;
  lbDescription.Align := alTop;
  GroupBox.Align := alTop;
end;
{$ENDIF}

end.
