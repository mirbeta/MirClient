unit LayoutViewDemoGroupScrollingMain;

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
  ComCtrls, ImgList, cxLabel, cxMaskEdit, cxDropDownEdit, CarsDataForGrid, cxNavigator,
  cxGridViewLayoutContainer, cxSpinEdit;

type
  TfrmMain = class(TfmBaseForm)
    Grid: TcxGrid;
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
    LayoutViewGroup4: TdxLayoutGroup;
    LayoutViewGroup5: TdxLayoutGroup;
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
    Images: TcxImageList;
    stRecordSelected: TcxStyle;
    LayoutViewGroup12: TdxLayoutGroup;
    LayoutViewGroup13: TdxLayoutAutoCreatedGroup;
    cxGroupBox1: TcxGroupBox;
    seRecordWidth: TcxSpinEdit;
    lbRecordWidth: TcxLabel;
    lbrecordHeight: TcxLabel;
    seRecordHeight: TcxSpinEdit;
    procedure seRecordWidthPropertiesChange(Sender: TObject);
    procedure seRecordHeightPropertiesChange(Sender: TObject);
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.seRecordHeightPropertiesChange(Sender: TObject);
begin
  LayoutView.OptionsView.RecordSize.Height := seRecordHeight.Value;
end;

procedure TfrmMain.seRecordWidthPropertiesChange(Sender: TObject);
begin
  LayoutView.OptionsView.RecordSize.Width := seRecordWidth.Value;
end;

end.
