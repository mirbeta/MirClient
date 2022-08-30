unit LayoutViewCarouselModeDemoMain;

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
  ComCtrls, ImgList, cxLabel, cxMaskEdit, cxDropDownEdit, cxNavigator,
  dxLayoutControlAdapters, dxLayoutcxEditAdapters, dxLayoutControl,
  cxTrackBar, DBClient, cxExtEditRepositoryItems, dxToggleSwitch;

type
  TfrmMain = class(TfmBaseForm)
    dsHomes: TDataSource;
    miView: TMenuItem;
    EditRepository: TcxEditRepository;
    EditRepositoryImage: TcxEditRepositoryImageItem;
    EditRepositoryMemo: TcxEditRepositoryMemoItem;
    EditRepositoryPrice: TcxEditRepositoryCurrencyItem;
    miCustomize: TMenuItem;
    btnCustomize: TcxButton;
    cbMultiSelectRecords: TcxCheckBox;
    cbRecordCaptions: TcxCheckBox;
    cbExpandableRecords: TcxCheckBox;
    mdHomes: TdxMemData;
    lcMainGroup_Root: TdxLayoutGroup;
    lcMain: TdxLayoutControl;
    lcMainItem1: TdxLayoutItem;
    lcMainItem2: TdxLayoutItem;
    lcMainItem3: TdxLayoutItem;
    lcMainItem4: TdxLayoutItem;
    dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList;
    dxLayoutSkinLookAndFeel1: TdxLayoutSkinLookAndFeel;
    lcMainGroup1: TdxLayoutGroup;
    tbPitchAngle: TcxTrackBar;
    liPitchAngle: TdxLayoutItem;
    tbEndRecordScale: TcxTrackBar;
    tbBackgroundAlphaLevel: TcxTrackBar;
    lcMainItem6: TdxLayoutItem;
    tbStartRecordScale: TcxTrackBar;
    lcMainItem7: TdxLayoutItem;
    tbRollAngle: TcxTrackBar;
    lcMainItem8: TdxLayoutItem;
    lcMainGroup3: TdxLayoutGroup;
    lcMainItem9: TdxLayoutItem;
    lcMainItem10: TdxLayoutItem;
    tbRecordCount: TcxTrackBar;
    cbInterpolationMode: TcxComboBox;
    lcMainItem11: TdxLayoutItem;
    lcMainGroup7: TdxLayoutGroup;
    Grid: TcxGrid;
    LayoutView: TcxGridDBLayoutView;
    dxLayoutGroup1: TdxLayoutGroup;
    GridLevel1: TcxGridLevel;
    lcMainGroup5: TdxLayoutGroup;
    mdHomesAddress: TMemoField;
    mdHomesBeds: TSmallintField;
    mdHomesBaths: TSmallintField;
    mdHomesHouseSize: TFloatField;
    mdHomesPrice: TFloatField;
    mdHomesFeatures: TMemoField;
    mdHomesYearBuilt: TMemoField;
    mdHomesPhoto: TBlobField;
    LayoutViewLayoutItem1: TcxGridLayoutItem;
    LayoutViewRecId: TcxGridDBLayoutViewItem;
    LayoutViewLayoutItem3: TcxGridLayoutItem;
    LayoutViewAddress: TcxGridDBLayoutViewItem;
    LayoutViewLayoutItem4: TcxGridLayoutItem;
    LayoutViewBeds: TcxGridDBLayoutViewItem;
    LayoutViewLayoutItem5: TcxGridLayoutItem;
    LayoutViewBaths: TcxGridDBLayoutViewItem;
    LayoutViewLayoutItem6: TcxGridLayoutItem;
    LayoutViewHouseSize: TcxGridDBLayoutViewItem;
    LayoutViewLayoutItem8: TcxGridLayoutItem;
    LayoutViewPrice: TcxGridDBLayoutViewItem;
    LayoutViewLayoutItem9: TcxGridLayoutItem;
    LayoutViewFeatures: TcxGridDBLayoutViewItem;
    LayoutViewLayoutItem10: TcxGridLayoutItem;
    LayoutViewYearBuilt: TcxGridDBLayoutViewItem;
    LayoutViewLayoutItem13: TcxGridLayoutItem;
    LayoutViewPhoto: TcxGridDBLayoutViewItem;
    LayoutViewGroup4: TdxLayoutGroup;
    LayoutViewGroup13: TdxLayoutGroup;
    EditRepositorySpinItem: TcxEditRepositorySpinItem;
    LayoutViewGroup3: TdxLayoutGroup;
    tsAutoPitchAngle: TdxToggleSwitch;
    lcMainItem5: TdxLayoutItem;
    procedure miCustomizeClick(Sender: TObject);
    procedure btnCustomizeClick(Sender: TObject);
    procedure cbMultiSelectRecordsClick(Sender: TObject);
    procedure cbExpandableRecordsClick(Sender: TObject);
    procedure cbRecordCaptionsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CarouselModePropertiesChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FLockCount: Integer;
  public
  {$IFDEF EXPRESSBARS}
    procedure PlaceControls; override;
  {$ENDIF}
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

uses
  dxGDIPlusClasses;

procedure TfrmMain.miCustomizeClick(Sender: TObject);
begin
  LayoutView.Controller.Customization := True;
end;

procedure TfrmMain.btnCustomizeClick(Sender: TObject);
begin
  LayoutView.Controller.Customization := True;  
end;

procedure TfrmMain.cbExpandableRecordsClick(Sender: TObject);
begin
  LayoutView.OptionsCustomize.RecordExpanding := cbExpandableRecords.Checked;
end;

procedure TfrmMain.cbMultiSelectRecordsClick(Sender: TObject);
begin
  LayoutView.OptionsSelection.MultiSelect := cbMultiSelectRecords.Checked;
end;

procedure TfrmMain.cbRecordCaptionsClick(Sender: TObject);
begin
  LayoutView.OptionsView.RecordCaption.Visible := cbRecordCaptions.Checked;
end;

procedure TfrmMain.FormCreate(Sender: TObject);

  function GetDir(const AFileName: string; ALevelUp: Integer): string;
  var
    I: Integer;
  begin
    Result := AFileName;
    for I := 1 to ALevelUp do
      Result := ExtractFileDir(Result);
  end;

begin
  Inc(FLockCount);
  try
    tsAutoPitchAngle.Checked := LayoutView.OptionsView.CarouselMode.AutoPitchAngle;
    cbInterpolationMode.ItemIndex := Ord(LayoutView.OptionsView.CarouselMode.InterpolationMode);
    tbPitchAngle.Position := Trunc(LayoutView.OptionsView.CarouselMode.PitchAngle);
    tbRollAngle.Position := Trunc(LayoutView.OptionsView.CarouselMode.RollAngle);
    tbRecordCount.Position := LayoutView.OptionsView.CarouselMode.RecordCount;
    tbBackgroundAlphaLevel.Position := LayoutView.OptionsView.CarouselMode.BackgroundRecordAlphaLevel;
    tbStartRecordScale.Position := LayoutView.OptionsView.CarouselMode.BackgroundRecordStartScale;
    tbEndRecordScale.Position := LayoutView.OptionsView.CarouselMode.BackgroundRecordEndScale;
  finally
    Dec(FLockCount);
  end;
  mdHomes.LoadFromBinaryFile(GetDir(Application.ExeName, 3) + '\Data\Homes.dat');
end;

{$IFDEF EXPRESSBARS}
procedure TfrmMain.PlaceControls;
begin
  DisableAlign;
  lcMain.Align := alNone;
  lbDescription.Align := alNone;
  lbDescription.Top := 100;
  EnableAlign;
  lbDescription.Align := alTop;
  lcMain.Align := alTop;
end;
{$ENDIF}

procedure TfrmMain.CarouselModePropertiesChange(Sender: TObject);
begin
  if FLockCount > 0 then
    Exit;
  Inc(FLockCount);
  try
    LayoutView.OptionsView.CarouselMode.AutoPitchAngle := tsAutoPitchAngle.Checked;
    LayoutView.OptionsView.CarouselMode.InterpolationMode := TdxGPInterpolationMode(cbInterpolationMode.ItemIndex);
    LayoutView.OptionsView.CarouselMode.PitchAngle := tbPitchAngle.Position;
    LayoutView.OptionsView.CarouselMode.RollAngle := tbRollAngle.Position;
    LayoutView.OptionsView.CarouselMode.RecordCount := tbRecordCount.Position;
    LayoutView.OptionsView.CarouselMode.BackgroundRecordAlphaLevel := tbBackgroundAlphaLevel.Position;
    LayoutView.OptionsView.CarouselMode.BackgroundRecordStartScale := tbStartRecordScale.Position;
    LayoutView.OptionsView.CarouselMode.BackgroundRecordEndScale := tbEndRecordScale.Position;
  finally
    Dec(FLockCount);
  end;
  liPitchAngle.Enabled := not tsAutoPitchAngle.Checked;
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  inherited;
  LayoutView.DataController.RecNo := LayoutView.DataController.RecordCount div 2;
end;

end.
